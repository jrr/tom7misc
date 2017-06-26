
(* Obsolete parser. see parse.sml instead *)

structure NCBI =
struct
  
  open Parsing

  structure LU = ListUtil

  infixr 4 << >>
  infixr 3 &&
  infix  2 -- ##
  infix  2 wth suchthat return guard when
  infixr 1 ||

  exception Impossible 

  datatype section =
    Any of string * string list
  | Origin of CharVector.vector
  (* XXX Feature, etc *)

  exception Parse of string

  local
    val size = ref 1
    val last = ref 0
  in
    fun setsize n = 
      let in
        last := 0;
        size := n
      end

    fun progress p =
      let 
        val (_, c) = Pos.getabs p
      in
        (* have we made more than 1% progress? *)
        if real (c - !last) / real (!size)
           > 0.01
        then
          let in
            last := c;
            print (Int.toString (Real.trunc (100.0 * (real (!last) /  real (!size))))
                   ^ "%...\n")
          end
        else ()
      end
  end

  fun warn s =
    let in
      TextIO.output (TextIO.stdErr, ("Warning: " ^ s ^ "\n"));
    end

  fun error s =
    let in
      TextIO.output (TextIO.stdErr, ("Error: " ^ s ^ "\n"));
      raise Parse s
    end

  (* match a string, case insensitive *)
  fun ? s =
    let
      val ss = size s
      fun next n =
        if n >= ss 
        then succeed ()
        else satisfy (fn c => 
                      Char.toLower c = 
                      Char.toLower (CharVector.sub(s, n))) -- 
                     (fn _ => next (n + 1))
    in
      next 0
    end

  (* match <= n repetitions of c. returns the number matched *)
  fun upto 0 c = succeed 0
    | upto n c =
    (literal c -- (fn _ => upto (n - 1) c wth (fn x => x + 1))) ||
    succeed 0

  fun count c =
    (literal c -- (fn _ => count c wth (fn x => x + 1))) ||
    succeed 0

  fun newline c = c = #"\n" orelse c = #"\r"
  val letter = satisfy (StringUtil.charspec "-A-Za-z0-9'_*")
  val digit = satisfy (StringUtil.charspec "0-9")
  val word = repeat1 letter wth implode
  val number = repeat1 digit wth implode

  fun restofline () =
    let
      val a = GCharArray.init 80
      fun rd () =
        let in
          (satisfy (not o newline) -- (fn x =>
                                       (GCharArray.append a x;
                                        rd ())))
          || satisfy newline
        end
    in
      $rd wth (fn _ =>
               (CharVector.tabulate (GCharArray.length a,
                                     (fn z =>
                                      GCharArray.sub a z))))
    end

  val restofline = $restofline

  (* start matching a section. if there is a section name, it
     must start before column n! *)
  fun newsection n () =
    upto n #" " -- 
    (fn leading =>
     word --
     (fn w =>
      (* now match as many spaces as we can. *)
       count #" " --
       (fn trailing =>
        restofline wth 
          (fn rol => (String.map Char.toLower w, 
                      leading + trailing + size w, rol))
          )))

  fun morelines c =
    repeat (repeatn c (literal #" ") >> restofline)

  fun dosection (("origin", _, _), pos) =
    let 
      (* start with a large array *)
      val a = GCharArray.init 4000000

      fun eatws () =
        (literal #" " >> $eatws)
        || succeed ()

      fun rol () = 
          (satisfy (not o newline) -- 
           (fn #" " => rol ()
             | x => 
            let in
              GCharArray.append a x;
              rol ()
            end))
          || satisfy newline

      fun readlines () =
        (($eatws >> (!!number wth (fn (_, p) => progress p)) >> $rol)
         >> $readlines) || succeed ()
    in
      progress pos;
      $readlines wth (fn _ =>
                      Origin 
                      (CharVector.tabulate (GCharArray.length a,
                                            (fn z =>
                                             GCharArray.sub a z))))      
    end
    | dosection ((name, column, firstline), pos) =
     let in
       progress pos;
       morelines column
         wth (fn ls => Any (name, firstline :: ls))
     end

  fun seqfile () = !!($(newsection 9999)) -- dosection

  fun parsefile f =
    let 
      val n = Position.toInt (Posix.FileSys.ST.size (Posix.FileSys.stat f))
      val _ = print (f ^ " is " ^ Int.toString n ^ " bytes\n")
      val _ = setsize n

      val s = StreamUtil.ftostream f
      val s = Pos.markstreamex f s
      val s = Parsing.transform ($seqfile) s
    in
      (*
      case Stream.toList s of
        [one] => one
      | (h::_) =>
          let in
            warn "garbage at end of file?";
            h
          end
      | _ => error "unable to find any records"
          *)
      s
    end

  fun ppout s =
    let
      fun pentry (Any(name, lines)) =
        let in
          print (name ^ ":\n");
          app (fn l => print ("..." ^ l ^ "\n")) lines
        end
        | pentry (Origin s) =
        let in
          print ("ORIGIN (... "  ^ Int.toString (size s) ^ " bytes ...)\n");
          print (s ^ "\n")
        end
    in
      Stream.app pentry s
    end

end