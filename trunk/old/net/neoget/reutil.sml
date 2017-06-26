
(* 


 Note: This code does not work any more.
 Use the one in sml-lib/util/re*


*)

structure REUtil : REUTIL =
struct

  exception Error

  (* StringCvt.scanString (R.find re) .. *)

  structure R = RegExpFn(structure P=AwkSyntax
                         structure E=BackTrackEngine);

  (* FIXME this is screwed up because StringCvt.cs is
     now abstract, and the only way to use it is deep
     within the argument to StringCvt, which I don't
     understand. 

Another had the same problem, solved by Matthew Fluet:

> fun test n = let val matches = StringCvt.scanString (BT.find cregex)
> line
>                  val tree = Option.valOf matches
>              in
>                  case MatchTree.nth(tree,n) of
>                      NONE => "0"
>                    | SOME {len,pos} => Int.toString (pos) (* or
> String.substring (line,pos,len)*)
>              end
>
> end

You have your scan inverted.  Try the following:

fun test n =
   let
      val matches = (BT.find cregex) Substring.getc (Substring.full line)
      val (tree,rest) = Option.valOf matches
   in
      case MatchTree.nth(tree,n) of
         NONE => "0"
       | SOME {len,pos} => Substring.string (Substring.slice(pos,0,SOME len))
   end

end

I don't know the SML/NJ RegExp library, but from the description of
REGEXP.find, it appears that pos will hold the substring where the regexp
match starts, and len will hold the length of the match.  The code above
prints out the match. *)

  fun find x =
    let
      val re = R.compileString x
      val ss = StringCvt.scanString (R.find re)
      fun f s =
        case ss s of
          NONE => NONE
        | SOME mt => 
            let
              fun g i =
                case MatchTree.nth (mt, i) of
                  NONE => raise Error
                | SOME {pos,len} => 
                    String.substring (s, pos, len)
            in
              SOME g
            end
    in
      f
    end


  fun findall x =
    let
      val re = R.compileString x
      val ss = StringCvt.scanString (R.find re)
      fun f s =
        case ss s of
          NONE => nil
        | SOME mt => 
            let
              fun g i =
                case MatchTree.nth (mt, i) of
                  NONE => raise Error
                | SOME {pos,len} => 
                    String.substring (s, pos, len)
            in
                case MatchTree.nth (mt, 0) of
                    SOME {pos,len} => g :: f (String.substring (s, pos + len, size s - (pos + len)))
                  | NONE => raise Error
            end
    in
      f
    end

  fun ismatch re s =
    case find re s of
      NONE => false
    | _ => true


end