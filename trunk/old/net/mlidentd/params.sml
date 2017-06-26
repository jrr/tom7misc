
(* mutable collection of flags and string constants.
   see params-sig.sml.
*)

structure Params :> PARAMS =
struct

  exception BadOption of string

  val flags = (ref nil) 
              : (bool ref * bool * (string * string * unit) option * 
                 string) list ref

  val params = (ref nil)
               : (string ref * string * (string * string * unit) option * 
                  string) list ref

  val paramlists = (ref nil)
               : (string list ref * string list * (string * string * char) option *
		  string) list ref

  fun I x = x

  fun get lr name =
    let
      fun f nil = NONE
        | f ((r, _, _, n)::t) = if name = n 
                               then SOME r
                             else f t 
    in
      f (!lr)
    end

  fun argget lr name =
    let
      fun f nil = NONE
        | f ((r, _, SOME(n, _, _), _)::t) = if name = n 
                                           then SOME r
                                         else f t 
        | f (_::t) = f t
    in
      f (!lr)
    end

  fun make (f  : 'spec option -> (string * string * 'extra) option) 
           (lr : ('output ref * 'output * (string * string * 'extra) option * string) list ref)
	   (default : 'output)
	   (cmd : 'spec option)
	   (name : string)
	   : 'output ref =
    case get lr name of
      NONE => 
        let 
          val h = ref default
        in
          lr := ((h,default,f cmd,name) :: !lr);
          h
        end
    | SOME r => r

  fun twotothree NONE = NONE
    | twotothree (SOME (a,b)) = SOME(a,b,())

  val getflag = get flags
  val flag = make twotothree flags

  val getparam = get params
  val param = make twotothree params

  val getparamlist = get paramlists
  val paramlist = make I paramlists

  fun asint def (ref a) = getOpt (Int.fromString a, def)

  val table = StringUtil.table 75

  fun usage () =
    let
	fun smp ((cl, doc, _)) s = [cl, s, doc]
	fun pl ((cl, doc, c)) s = [cl, str c, s, doc]
	fun f ml s ts l = rev (foldr (fn ((_, d, SOME xx, _), b) =>
                                   ml xx (ts d) :: b 
                                   | (_, b) => b) [s] l)
        fun bts d = (if d then "(true)" else "(false)")
    in
      (case !flags of
         nil => ""
       | l => "The following flags are supported (specify to toggle):\n" ^
              (table (f smp ["flag", "default", "description"] bts l))) ^
      (case !params of
         nil => ""
       | l => "\nThe following parameters are supported (specify,\n" ^
              "followed by a string, to change them):\n" ^
              (table (f smp ["param", "default", "description"] I l))) ^
      (case !paramlists of
	 nil => ""
       | l => "\nThe following parameters take a list of strings.\n" ^
	      "Specify them followed by a list of arguments separated\n" ^
	      "by the given separator character.\n" ^
	      (table (f pl ["param", "sep", "default", "description"] (StringUtil.delimit ", ") l)))
    end

  fun docommandline () = 
    let
      fun f nil l = rev l
        | f (h::t) l =
        case argget flags h of
          NONE =>
            (case argget params h of
               NONE => f t (h::l)
             | SOME sr => 
                 (case t of 
                    nil => raise BadOption 
                          (h ^ " must be followed by a value.\n")
                  | v::rest => 
                          let in
                            sr := v;
                            f rest l
                          end))
        | SOME br => let in
                       br := (not (!br));
                       f t l
                     end
    in
      f (CommandLine.arguments()) nil
    end

end
