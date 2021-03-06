
(* This is a combinator-style parser for the Hemlock grammar.
   The combinator library used is in helmock/cparsing/basic-sig.sml
                                 and hemlock/cparsing/utils-sig.sml.

   This structure contains parsers from token streams into expressions (etc.)
   See tokenize.sml for the tokenizer.

   The parsers here are straightforward except for a few things:
      Some parsers have contexts. I use the context to track, for instance,
      the fixity of tokens. Thus, "infix x" inserts something into the context,
      and the pattern and app phrases make use of the context. The Initfix
      structure gives the default context.

      The "import" keyword causes the target file to be read in, tokenized, and
      then prepended to the current input. 
*)

(* FIXME:

   Not parsing: PRecord, Record (must use tuples)
   ... what else?
*)

(* XXX:
   
   better error messages. This can be done with something like
   Prolog's cut operator (!), where after seeing the keyword "FUN" we
   can expect to successfully complete the declaration, or else give
   an error. cparsing/utils-sig.sml's "guard" works rather like this.
 *)

(* XXX: several phrases will parse, looking for a postfix operator
   (handle), and then parse again if that fails. Instead we should
   do an optional parse of the postfix phrase to avoid doubling of
   work. *)


structure Parse :> PARSE =
struct

  val include_dirs = 
      Params.paramacc ["/usr0/src/hemlock/stdlib/", "."]
                     (SOME ("-I",
                            "additional include directories", #",")) "include"

  open Tokens

  open Parsing

  open EL

  structure LU = ListUtil

  infixr 4 << >>
  infixr 3 &&
  infix  2 -- ##
  infix  2 wth suchthat return guard when
  infixr 1 ||

  exception Impossible 

  exception Parse of string

  val namedstring = HemlockUtil.newstr
  val itos = Int.toString

  fun dirplus s1 s2 =
      (* XXX use platform dependent directory separator   (how?) *)
      if CharVector.sub(s1, size s1 - 1) = #"/"
      then s1 ^ s2
      else s1 ^ "/" ^ s2

  (* look in every include path for this file *)
  fun tryopen f =
      let
          fun one s =
              (SOME (StreamUtil.ftostream (dirplus s f)))
              handle _ => NONE
      in
          case List.mapPartial one (!include_dirs) of
              nil => raise Parse (f ^ " not found in any include dir (" ^ 
                                  StringUtil.delimit "," (!include_dirs))
            | [h] => h
            | h::_ =>
                  let in
                      (* XXX move to a 'warn' function *)
                      print ("WARNING: include file " ^ f ^
                             " found in multiple directories; " ^
                             "choosing arbitrarily.\n");
                      h
                  end
      end

  fun `w = satisfy (fn x => eq (x, w))

  fun ifmany f [x] = x
    | ifmany f l = f l

  val id = satisfy (fn ID s => true | _ => false) 
                 wth (fn ID s => s | _ => raise Impossible)

  local 
      (* look for prodtypes separated by arrows.
         prodtypes are apptypes separated by *.
         apptypes are parenthesized, atomic, or applications. *)
      fun arrowtype () = separate ($prodtype) (`ARROW) wth LU.combiner TArrow

      and arrowtypes () = separate ($arrowtype) (`COMMA)

      and prodtype () = separate ($apptype) (`TIMES) wth ifmany
          (* create record 1:, 2:, ... *)
             (fn l => TRec (ListUtil.mapi 
                            (fn (t, n) => (itos (n+1), t)) l))

      and mostatomic () =
          alt [id wth TVar,
               `LPAREN >> $arrowtype << `RPAREN]

      and apptype () = 
          alt [`LPAREN && $arrowtype && `COMMA && 
                          $arrowtypes && `RPAREN && id 
                  wth (fn (_,(h,(_,(t,(_,i))))) => TApp(h::t,i)),
               $mostatomic && repeat1 id wth (fn (i, l) => 
                                              foldr (fn (a, b) => 
                                                     TApp ([b], a)) i l),
               $mostatomic]
  in
      val attype = $mostatomic
      val typ = $arrowtype
  end

  fun ttoc (STRLIT s) = SOME (CString s)
    | ttoc (INT i) = SOME (CInt i)
    | ttoc (CHAR c) = SOME (CChar c)
    | ttoc _ = NONE

  val constant = maybe ttoc

  fun isnonfix G s = not (LU.Alist.haskey op= G s)

  (* in expressions, we can use some tokens that have special
     meaning in types *)
  val expid = id || `TIMES return "*" || `ARROW return "->"

  (* nonfix identifiers, or any identifier prefixed with op *)
  fun fid G = expid suchthat (isnonfix G) || `OP >> expid

  (* crappy. *)
  fun call G parser = $(fn () => parser G)

  local
      (* pattern parsing is with respect to a fixity context. (string *
         (prec * status)) the fixity context is in sorted order by
         precedence (lower precedences near the head of the list).
         *)

      fun mapat G =
          alt [`UNDERSCORE return PWild,
               constant wth PConstant,
               fid G wth PVar,
               `LPAREN >> separate0 (call G pat) (`COMMA) << `RPAREN wth ifmany
                  (fn pl => PRecord(ListUtil.mapi 
                                    (fn (p, i) => 
                                     (Int.toString (i+1), p)) pl))]

      and appat G =
          alt [fid G && call G appat wth PApp,
               call G mapat]

      and infixpat G =
          let
              val par =
                  alt [expid when (LU.Alist.get op= G) 
                         wth (fn (s,(prec, ass)) => 
                              Opr(Infix(ass, prec, 
                                        (fn (x,y) => 
                                         PApp(s, PRecord[("1", x),
                                                         ("2", y)]))))),
                       call G appat wth Atm]
          in
              parsefixity par
          end

      and aspat G =
          alt [fid G && `AS && call G aspat wth (fn (i,(_,p)) => PAs(i, p)),
               call G infixpat]

      and pat G = 
          call G aspat && opt (`COLON >> typ)
               wth (fn (p, SOME t) => PConstrain(p, t)
                     | (p, NONE) => p)

  in
      val pat = fn G => call G pat
      val atpat = fn G => call G mapat
  end

  (* a pattern is irrefutable if it contains no constants or applications *)
  (* XXX actually, we should expand this notion to include applications
     of one-constructor datatypes. Then this needs to be moved into the
     elaborator, unless we go crazy enough to track bindings of datatype
     constructors, etc. (That might not be so bad. For one thing we need
     to rewrite non-carrying ctors to carry unit.) *)

  fun irrefutable PWild = true
    | irrefutable (PVar _) = true
    | irrefutable (PAs(_, p)) = irrefutable p
    | irrefutable (PRecord pl) = List.all (fn (_, p) => irrefutable p) pl
    | irrefutable (PConstrain (p, t)) = irrefutable p
    | irrefutable _ = false

  val tyvars = alt [id wth LU.list,
                    `LPAREN >> separate id (`COMMA) << `RPAREN]

  val datatypes = separate 
      (id && `EQUALS &&
       (separate0 (expid && opt (`OF >> typ)) (`BAR)) wth
       (fn (b, (_, c)) => (b, c))) (`AND)

  local
      fun fixcomp ((_,(p,_)), (_,(q,_))) = Int.compare (p, q)

      fun ifmanymark f [(x,_)] = x
        | ifmanymark f l = f l

      fun combinermark f [x] = x
        | combinermark f ((h,l)::t) = (f ((h,l), combinermark f t), l)
        | combinermark f nil = raise Impossible

      fun foldrmark f un nil = un
        | foldrmark f un ((h,l)::t) = (f ((h,l), foldrmark f un t), l)

      fun atomexp G =
          alt [fid G wth Var,
               constant wth Constant,
               `LET >> call G decs -- 
                    (fn (G,ds) => 
                     `IN >> separate (call G exp) (`SEMICOLON) << `END
                     wth (fn es => #1 (foldrmark Let 
                                       (combinermark Seq es) ds))),

               (`LSQUARE && `BAR) >> 
                     separate0 (call G exp) (`COMMA) << 
               (`BAR && `RSQUARE) wth Vector,

               (* parens can be a parenthesized expression,
                  a tuple expression (= record),
                  or a sequenced expression. Putting them
                  all together makes for an efficient parser! *)
               (`LPAREN && ` RPAREN) return (Record nil),

               `LPAREN >> (call G exp) && 
                           opt (alt [`SEMICOLON >> 
                                        separate (call G exp) (`SEMICOLON)
                                        wth Util.A,
                                     `COMMA >> separate (call G exp) (`COMMA)
                                        wth Util.B]) << `RPAREN
                    wth (fn (e, NONE) => #1 e
                          | (e, SOME(Util.A el)) => 
                                   #1 (combinermark Seq (e::el))
                          | (e, SOME(Util.B el)) => 
                                Record(ListUtil.mapi 
                                        (fn (e, i) => 
                                         (Int.toString (i + 1), e)) (e::el)))]

      and appexp G =
          let
              fun mkinfix (s, x as (_,l), y) = 
                  (App((Var s,l), (Record[("1",x),("2",y)],l)),l)
              fun mark ass prec f = 
                  Opr(Infix(ass, prec, (fn (x as (_,l), y) => (f(x,y),l))))

              val par =
                  alt [expid when (LU.Alist.get op= G)
                         wth (fn (s,(prec, ass)) =>
                              Opr(Infix(ass, prec, (fn (x as (_,l),y) => 
                                                    mkinfix (s, x, y))))),
                       `EQUALS return mark Non 1 (#1 o (fn (e1, e2) =>
                                                  mkinfix ("=", e1, e2))),
                       `ANDALSO return mark Right ~100 Andalso,
                       `ORELSE return mark Right ~200 Orelse,
                       `ANDTHEN return mark Right ~300 Andthen,
                       `OTHERWISE return mark Right ~300 Otherwise,
                       !!(call G atomexp) wth Atm]
          in
              parsefixityadj par Left (fn (a,b as (_,l)) =>
                                       (App (a, b),l)) wth #1
          end

      and handlexp G =
          !! (call G appexp) && opt (`HANDLE && call G matching)
                     wth (fn (a,SOME (_,m)) => Handle (a, m)
                           | ((a,_), NONE) => a)

      and matching G = separate0 (call G pat && `DARROW && call G exp wth 
                                  (fn (a,(_,c)) => (a,c))) (`BAR) 

      (* XXX use repeat to allow e : t : t : t *)
      and constrainexp G =
          !! (call G handlexp) && opt (`COLON && typ) 
                     wth (fn (a,SOME(_,c)) => Constrain (a, c)
                           | ((a,_),NONE) => a)

      and exp G = 
          (* can only write cases with one object, though the
             ast allows multiple *)
          !!( alt [`CASE >> call G exp && `OF && call G matching
                      wth (fn (obj,(_,pel)) => Case([obj], 
                                                    map (fn (p,e) =>
                                                         ([p], e))
                                                    pel)),
                      (* generalize these *)
                   `RAISE >> call G exp wth Raise,
                   `SUBMIT >> call G exp wth Submit,
                   `SPAWN >> call G exp wth Spawn,
                   `SYNCALL >> call G exp wth Syncall,

                   (* FIXME use 'label' instead of 'id' for #1 *)
                   (* XXX could rewrite this to a function in place *)
                   (* XXX why does this need to be followed by an expression?
                      val f = #1/5 
                      answer: so canonical form of arrow type is fn.
                      *)

                   `HASH >> id && `DIVIDE && attype && call G exp 
                      wth (fn (i, (_, (t, e))) => Proj(i, t, e)),
                   `IF >> call G exp && `THEN && call G exp && `ELSE && 
                      call G exp 
                      wth (fn (e as (_,l),(_,(t,(_,f)))) => If (e,t,f)),
                   !!(`FN) && (separate0 (repeat1 (call G atpat) && 
                                          (`DARROW return NONE) && 
                                          call G exp) (`BAR))
                      wth (fn ((_, l), s) => 
                           let val v = namedstring "anonfn"
                           in Let ((Fun [(nil, v, map flat3 s)], l), 
                                   (Var v, l))
                           end),
                   call G constrainexp])

      and funclause G =
          repeat1 (call G atpat) && opt (`COLON >> typ) && `EQUALS && 
           call G exp
           wth (fn (pats, (to, (_, e))) => (pats, to, e))

      and onefun' G =
          expid -- (fn f => separate (call G funclause) 
                    (`BAR >> expid suchthat (fn x => x = f)) wth 
                    (fn a => (f, a)))

      and onefun G =
          alt [(`LPAREN >> separate id (`COMMA) << `RPAREN) && call G onefun' 
                   wth (fn (tv, (f, clauses)) => (tv, f, clauses)),
               call G onefun' wth (fn (f, clauses) => (nil, f, clauses))]

      and funs G =
          separate (call G onefun) (`AND)

      and dec G = call G regulardec wth Util.A
               || call G infixdec wth Util.B

      (* after processing a dec, either use the new fixity context or
         place the dec on the accumulator *)
      and postdecs G (Util.A d) = call G decs -- 
                                    (fn (G,ds) => succeed (G, d :: ds))
        | postdecs _ (Util.B G) = call G decs

      and decs G = alt [`IMPORT >> (any when (fn STRLIT s => SOME s
                                              | _ => NONE))
                                    -- (fn s =>
                                         let
                                             (* XXX error messages *)
                                             fun tokenize f = 
                                                 Parsing.transform 
                                                   Tokenize.token 
                                                    (Pos.markstream 
                                                      (tryopen f))
                                         in
                                             push (tokenize s)
                                              (call G dec -- postdecs G)
                                         end),
                        call G dec -- postdecs G,
                        succeed (G,nil)]

      and infixdec G =
          alt [(`INFIX || `INFIXR) && opt 
               (any when (fn INT i => 
                          if i >= 0 andalso i <= 9
                          then SOME i 
                          else NONE | _ => NONE)) && expid 
                 wth (fn (fx, (preco, i)) => 
                      LU.Sorted.insert fixcomp G (i, (Option.getOpt 
                                                      (preco,4), 
                                                      case fx of 
                                                          INFIX => Left 
                                                        | _ => Right))),
               `NONFIX >> expid wth (fn i => List.filter 
                                     (fn (j,_) => i <> j) G)
                 ]

      and regulardec G =
         !!(alt [`VAL >> (call G pat suchthat irrefutable) && `EQUALS && 
                   call G exp
                   wth (fn (pat, (_, e)) => Val(nil, pat, e)),
                 `VAL >> tyvars && (call G pat suchthat irrefutable) && 
                   `EQUALS && call G exp
                   wth (fn (tv, (pat, (_, e))) => Val(tv, pat, e)),
                 `DO >> call G exp wth Do,
                 `TYPE >> id && `EQUALS && typ wth (fn (i,(_,t)) => 
                                                    Type (nil,i,t)),
                 `TYPE >> tyvars && id && `EQUALS && typ 
                   wth (fn (tv,(i,(_,t))) => Type(tv,i,t)),
                 `TAGTYPE >> id wth Tagtype,
                 `NEWTAG >> expid && opt (`OF >> typ) && `IN && id
                   wth (fn (i,(to,(_,ty))) => Newtag (i, to, ty)),
                 `EXCEPTION >> expid && opt (`OF >> typ) wth Exception,
                 `DATATYPE >> 
                   alt [tyvars && datatypes wth Datatype,
                        datatypes wth (fn d => Datatype(nil, d))],
                 `FUN >> call G funs wth Fun])

  in
      val exp = fn G => call G exp
      val dec = fn G => call G dec
  end

end
