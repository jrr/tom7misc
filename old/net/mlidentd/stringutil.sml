
(* String utilities by Tom 7. 
   See stringutil-sig for documentation. *)

structure StringUtil :> STRINGUTIL =
struct

  (* workaround for andb bug in MLton 20010706 *)
  fun mkbyte w = Word.mod (w, 0w256)

  exception StringUtil of string

  fun K x y = x
  
  fun unformatted_table sll =
    (foldl (fn (sl, rest) =>
            rest ^ "\n" ^
            (foldl (fn (s, b) => b ^ " " ^ s) "" sl)) "" sll) ^ "\n"

  fun ischar c d = c = d
  fun isn'tchar c d = c <> d
      
  (* XXX probably more efficient to use String.concat.
     I use this a LOT, so good to check... *)
  fun delimit s nil = ""
    | delimit s (h::t) =
        foldl (fn (a, b) => b ^ s ^ a) h t

  (* if s is longer than n chars, split it as best as possible
     to a list of strings less than or equal to n chars in length. 
     
     when encountering a \n character, also use that to break the
     line.
     *)
  fun wrapto (n : int) (s : string) : string list = 
      let
	  fun wrapline ss =
	      if size ss <= n then [ss]
	      else
		  let
		      fun grab nil nil _ = nil
			| grab nil l _ = [delimit " " (rev l)]
			| grab (h::t) l sof = 
			  if size h + sof <= n 
			  then grab t (h::l) (sof + (size h + 1))
			  else if null l andalso size h > n 
			       then String.substring (h, 0, n) :: 
				      grab (String.substring (h, n, size h - n)::t) nil 0
			       else (delimit " " (rev l)) :: grab (h::t) nil 0
		  in
		      grab (String.fields (ischar #" ") ss) nil 0
		  end
      in
	  List.concat (map wrapline (String.fields (ischar #"\n") s))
      end


  (* XXX old, worse wrapto *)
  fun hardwrapto n s =
    if size s <= n then [s] else
	String.substring (s, 0, n) :: wrapto n (substring (s, n, size s - n))


  fun pad' n s =
    if (size s >= n) then (s, "")
    else (s, (CharVector.tabulate (n - size s, K #" ")))

  fun pad n s = 
    if n < 0 then
        let val (a, b) = pad' (~ n) s
        in b ^ a
        end
    else
        let val (a, b) = pad' n s
        in a ^ b
        end

  (* this one takes a hard width for each column (as il) *)
  fun hardtable il sll =
    let 
      fun f nil = ""
        | f (sl::rest) =
        let 
          (* split up each string at its corresponding length *)
          val psl : string list list =
                let fun g (nil, nil) = nil
                      | g (s::r, n::t) = (wrapto (abs n) s) :: g(r, t)
                      | g _ = raise StringUtil "inconsistent hardtables(1)"
                in
                  g (sl, il)
                end
 
          fun maybetl nil = nil
            | maybetl (_::t) = t
        
          fun j (sll : string list list) =
                if (List.all List.null sll) then ""
                else let
                       fun k (nil : string list list, nil : int list) = "\n"
                         | k (nil::r, n::t) = (pad n "") ^ " " ^ k (r,t)
                         | k ((s::_)::r, n::t) = (pad n s) ^ " " ^ k (r,t)
                         | k _ = raise StringUtil "inconsistent hardtables(2)"
                     in
                         k (sll, il) ^ j (map maybetl sll)
                     end

          val block = j psl

        in
          block ^ (f rest)
        end
    in
      f sll
    end


  fun max (a, b) = if a < b then b else a

  fun make_square (x : string list list) : string list list * int =
    let
        val cols = foldl (fn (l, b) => max(length l, b)) 0 x
    in
        (foldr (fn (l, b) =>
                let val n = length l
                in
                if n < cols then
                   l @ (List.tabulate (cols - n, K ""))
                else l
                end :: b) nil x, cols)
    end


  (* XXX: this method sucks when lines are long 
     (and especially when they don't appear near the end.) *)
  fun table n sll =
    let
        val (sll, cols) = make_square sll
        
        val sizes = 
            foldl (ListPair.map (fn (a,b) => max (size a, b))) 
                     (List.tabulate (cols, K 0)) sll

        fun fixsizes (h::t, l, surplus) = 
              if h < surplus then fixsizes(t, h::l, surplus - h)
              else fixsizes(t, max(surplus, 4) :: l, 0)
          | fixsizes (nil, l, surplus) = (rev l, surplus)
    in
        hardtable sizes sll
    end

  fun ucase s =
      let fun uc nil = nil
            | uc (h::t) =
          (if h >= #"a" andalso h <= #"z" then chr(ord h - 32)
           else h) :: uc t
      in
          implode (uc (explode s))
      end

  fun lcase s =
      let fun lc nil = nil
            | lc (h::t) =
          (if h >= #"A" andalso h <= #"Z" then chr(ord h + 32)
           else h) :: lc t
      in
          implode (lc (explode s))
      end

  (* XXX this could be a lot more efficient. *)
  fun filter f = implode o (List.filter f) o explode

  fun readfile f = 
    let
      val l = TextIO.openIn f
      val s = TextIO.inputAll l
    in
      TextIO.closeIn l; s
    end

  fun truncate l s = 
      if size s > l then String.substring(s, 0, l)
      else s

  val digits = "0123456789ABCDEF"

  fun hexdig i = implode [CharVector.sub (digits, i div 16),
                          CharVector.sub (digits, i mod 16)]

  fun inlist nil c = false
    | inlist (h::t) (c : char) = c = h orelse inlist t c

  fun harden f esc l s =
      let
          (* will need at most this many chars, but don't bother
             translating any more... *)
          val ss = truncate l s
            
          fun ff c = if (c <> esc andalso f c)
                     orelse Char.isAlphaNum c then str c
                     else str esc ^ hexdig (ord c)
      in
          truncate l (String.translate ff ss)
      end

  fun wordtohex_be w = 
    let
      val a = mkbyte (Word.>> (w, 0w24))
      val b = mkbyte (Word.>> (w, 0w16))
      val c = mkbyte (Word.>> (w, 0w8))
      val d = mkbyte w
    in
      hexdig (Word.toInt a) ^
      hexdig (Word.toInt b) ^
      hexdig (Word.toInt c) ^
      hexdig (Word.toInt d)
    end

  fun wordtohex_le w =
    let
      val a = mkbyte(Word.>> (w, 0w24))
      val b = mkbyte(Word.>> (w, 0w16))
      val c = mkbyte(Word.>> (w, 0w8))
      val d = mkbyte w
    in
      hexdig (Word.toInt d) ^
      hexdig (Word.toInt c) ^
      hexdig (Word.toInt b) ^
      hexdig (Word.toInt a)
    end

  fun word16tohex w =
    let
      val c = mkbyte (Word.>> (w, 0w8))
      val d = mkbyte w
    in
      hexdig (Word.toInt c) ^
      hexdig (Word.toInt d)
    end

  fun nybbletohex i = hexdig (i div 256) ^ hexdig (i mod 256)
  fun bytetohex i = hexdig (i mod 256)

  fun all f s =
      let
          fun ff ~1 = true
            | ff n = f (CharVector.sub(s, n)) andalso ff (n - 1)
      in
          ff (size s - 1)
      end

  fun charspec s =
      let
          fun none _ = false
          fun r (f, nil) = f
            | r (f, (#"\\" :: c :: t)) = r ((fn d => d = c orelse f d), t)
            | r (f, (c :: #"-" :: d :: t)) = 
              let val (c, d) = if c > d then (c, d) else (d, c)
              in r ((fn e => (e <= c andalso e >= d) orelse f e), t)
              end
            | r (f, (c :: t)) = r ((fn d => d = c orelse f d), t)
      in
          case explode s of
              #"^" :: rest => let val f = r (none, rest)
                              in fn c => not (f c)
                              end
            | chars => r (none, chars)
      end

  (* XXX stage? *)
  fun escape c what s =
      let
	  fun range lo hi = Substring.extract(s, lo, SOME ((hi - lo) + 1))

	  val sp = charspec (what ^ implode[c])
	  fun f st n =
	      if n >= size s 
	      then [range st (n - 1)]
	      else 
		  let val ch = (String.sub (s, n))
		  in
		      if sp ch
		      then range st (n - 1) :: Substring.all (implode[c,ch]) :: f (n + 1) (n + 1)
		      else f st (n + 1)
		  end
      in
	  Substring.concat (f 0 0)
      end

  (* last element in a non-empty list *)
  fun llast nil = raise List.Empty
    | llast (h::t) =
      let fun f a nil = a
            | f _ (h::t) = f h t
      in f h t
      end

  (* XXX: matchat is speed-critical in findat; it should really be
     rewritten to compare character-wise without allocating a
     substring.
   *)
  fun matchat n small big =
      (String.substring (big, n, size small) = small)
      handle _ => false (* might raise Subscript *)

  fun matchtail small big =
      matchat (size big - size small) small big

  val matchhead = matchat 0 

  (* XXX: kmp is more appropriate for really big 'big'. (but properly
     staged, we wouldn't know what 'big' is until too late (perhaps
     use thunks?)). Right now, we do m*n time search. 
   *)
  fun findat n small big =
      if (size big - n) < size small then NONE
      else if matchat n small big then SOME n
           else findat (n + 1) small big

  val find = findat 0

  fun rfindat n small big =
      if n < 0 then NONE
      else if matchat n small big then SOME n
          else rfindat (n - 1) small big

  fun rfind small big = rfindat (size big - size small) small big

  fun wcmatch w s =
      let
          (* act specially on endpoints.
             We could probably pull some trick of
             inserting sentinels if there were some
             characters we knew wouldn't be used,
             but this is easy enough...
             *)
          val sfront = CharVector.sub(w, 0) = #"*"
          val sback = CharVector.sub(w, size w - 1) = #"*"

           (* allinorder n sl s
              true if all strings in sl appear in s,
              in order without overlap, beginning
              at character n *)
          fun allinorder n nil s = true
            | allinorder n (h::t) s =
              case findat n h s of
                  NONE => false
                | SOME nn => allinorder (nn+size h) t s
      in
          case String.tokens (ischar #"*") w of
              nil => true (* *, **, etc. matches anything. *)
            | parts => 
                  let val first = hd parts
                      val last = llast parts
                  in
                  (sfront orelse matchhead first s) andalso
                  (sback orelse matchtail last s) andalso
                  allinorder 0 parts s
                  end
      end

  (* to remove dependency on Util *)
  fun for lo hi f =
      if lo > hi then ()
      else (f lo; for (lo + 1) hi f)

  (* engrish? ;) *)
  fun fordr lo hi init f =
      if lo > hi then init
      else (f (lo, fordr (lo + 1) hi init f))

  (* XXX maybe allow them to change this... *)
  val printable = charspec "-A-Za-z0-9 ,./;':<>?[]{}\\|=_+^~`!@#$%^&*()"

  fun hexvalue ch =  SysWord.toInt (SysWord.orb(SysWord.fromInt(ord ch), SysWord.fromInt 4400)) mod 55

  fun hexdump s =
      let
          (* one chunk of <= 16 bytes *)
          fun line startat s =
                  if startat > size s then "" else
                  (fordr startat (startat + 15) ""
                   (fn (j, rest) =>
                    (* the byte *)
                    (if j >= size s then "  "
                     else (bytetohex (ord (CharVector.sub(s,j))))) ^
                    (if j mod 2 = 1 then " "
                     else "") ^
                    (if j mod 8 = 7 then " "
                     else "") ^
                    rest
                    ) ^
                   fordr startat (startat + 15) ""
                   (fn (j, rest) =>
                    (if j >= size s then " "
                     else (if printable (CharVector.sub(s,j)) then
                              implode[CharVector.sub(s,j)] else ".")) ^
                     (if j mod 8 = 7 then " "
                     else "") ^
                     rest
                    ) ^
                   "\n" ^
                   line (startat + 16) s)
      in
          line 0 s
      end

  fun tabulate n c = CharVector.tabulate (n, fn _ => c)

end
