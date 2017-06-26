
structure ChordDB =
struct

  datatype fingering =
    (* -1, -2, =, 1, 2, ... *)
    F of int
  | O     (* open *)
  | X     (* muted *)

  type chord = fingering list

  datatype exp =
    Rep of exp list
  | Chord of chord
  | Tune of chord * exp list
    (* new base, 
       chord (typically OOBBBO where B is the same as the base),
       expressiosn *)
  | PC of int * chord * exp list

  type song = string * exp list
  type db = song list
    
  exception ChordDB of string

  fun perror s _ = raise ChordDB s

  fun readdb f =
    let
      datatype token =
        STRING of string
      | ID of string
      | LBRACK
      | RBRACK
      | LPAREN
      | RPAREN
      | SONG
      | END
      | DEF
      | TUNE
      | PC_

      val toks = [("[", LBRACK),
                  ("]", RBRACK),
                  ("(", LPAREN),
                  (")", RPAREN),
                  ("SONG", SONG),
                  ("DEF", DEF),
                  ("END", END),
                  ("PC", PC_),
                  ("TUNE", TUNE)]
        
      local
        open SimpleTok

        val tk = settokens (empty()) toks
        val tk = setother tk ID
        val tk = setstring tk STRING [SSStandard]
        val tk = setcomment tk [CSLine ";",
                                CSBracketed ("(*", "*)")]
        val tk = setsep tk (Char.contains "[](),")

      in
        val tokenizer = parser tk
      end

      val toks = Parsing.transform tokenizer 
                    (Pos.markstream (StreamUtil.ftostream f))

      

      local 
        open Parsing
        infixr 4 << >>
        infixr 3 &&
        infix  2 -- ##
        infix  2 wth suchthat return guard when
        infixr 1 ||

        val ` = literal

        exception Impossible


        val id = satisfy (fn ID s => true | _ => false) 
                 wth (fn ID s => s | _ => raise Impossible)
          
        val strlit = satisfy (fn STRING s => true | _ => false)
                     wth (fn STRING s => s | _ => raise Impossible)

        fun call G parser = $(fn () => parser G)

        fun parsechord s =
          let 
            exception Bad

            fun getnum c =
              case StringUtil.find (Char.toString c)
                "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ" of
                NONE => raise Bad (* non-fingering character *)
              | SOME i => i
              
            fun go nil 0 = nil
              | go nil _ = raise Bad (* not enough chars *)
              | go _   0 = raise Bad (* too many chars *)
              | go (#"-"::c::t) n = F (~ (getnum c)) :: go t (n - 1)
              | go (#"="::t) n = F 0 :: go t (n - 1)
              | go (#"0"::t) n = O :: go t (n - 1)
              | go (#"x"::t) n = X :: go t (n - 1)
              | go (c::t) n = F (getnum c) :: go t (n - 1)
          in
            SOME (go (explode s) 6) handle Bad => NONE
          end

        fun chord G : (chord, token) parser =
          id -- (fn s =>
                 case parsechord s of
                   SOME c => succeed c
                 | NONE => 
                     case ListUtil.Alist.find op= G s of
                       SOME c => succeed c
                     | NONE =>
                         let in
                           print ("Not a chord: " ^ s ^ "\n");
                           fail
                         end)

        fun exp G : (exp, token) parser = 
          alt [`LBRACK >> call G exps << `RBRACK wth Rep,
               `TUNE >> call G chord && call G exps << `END wth Tune,
               `PC_ >> id && id && call G exps << `END
               wth (fn (i, (ch, es)) =>
                    if size ch = 6
                    then PC
                      let val base = (case Int.fromString i of
                         SOME ii => ii
                       | NONE => raise ChordDB
                           ("pc base must be natural number: " ^ i))
                      in
                      (base,
                       map (fn #"-" => O
                            | #"=" => F base
                            | _ => raise 
                            ChordDB ("pc key must be - and = only: " ^ ch))
                            (explode ch),
                       es)
                      end
                    else raise ChordDB ("pc key must be 6 characters: " ^ ch)),
               call G chord wth Chord]

        and exps G : (exp list, token) parser =
          repeat (call G exp)

        fun dbline G : ((string * chord, song) Util.sum option, 
                        token) parser =
          opt 
          (alt [`DEF >> (id && call G chord wth Util.A
                         guard perror "expected DEF id chord"),
                `SONG >> (strlit && call G exps << `END wth Util.B
                          guard perror "expected SONG string exps END")])

        and db G : (song list, token) parser =
          let
            fun postline (SOME (Util.A def)) = call (def::G) db
              | postline (SOME (Util.B song)) = 
                   call G db -- (fn ss => succeed (song :: ss))
              | postline NONE = succeed nil
          in
            call G dbline -- postline
          end

      in
        val db = db nil
      end

    in
      (* XXX tokenizer should probably return positions... *)
      case Stream.tolist (Parsing.transform db (Pos.markany toks)) of
        [alldb] => alldb
      | _ => raise ChordDB "parse error: not a single expression"
    end handle (e as ChordDB s) =>
      let in
        print s;
        print "\n";
        raise e
      end
   

end

