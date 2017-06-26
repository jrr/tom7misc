
(* memoizing streams using the standard trick *)
structure Stream :> STREAM =
struct

  datatype 'a front = Nil | Cons of 'a * 'a stream
  withtype 'a stream = unit -> 'a front

  fun force f = f ()
  fun delay (s : unit -> 'a front) : 'a stream =
    let
      val r = ref (fn () => raise Match)
    in
      r := (fn () => 
            let val ss = s ()
            in 
              r := (fn () => ss);
              ss
            end);
      (fn () => (!r) ())
    end

  fun map (f : 'a -> 'b) (s : 'a stream) : 'b stream = delay (map' f s)
  and map' f s () : 'b front = 
    case force s of
      Nil => Nil
    | Cons(a, rest) => Cons(f a, map f rest)

  fun app f s =
    case force s of
      Nil => ()
    | Cons(a, rest) =>
        let in
          f a;
          app f rest
        end

  fun filter f s = delay (filt' f s)
  and filt' f s () =
      case force s of
	  Nil => Nil
	| Cons(a, rest) => 
	      if f a then Cons(a, filter f rest)
	      else filt' f rest ()
	       
		  
  fun tolist s =
      case force s of
	  Nil => nil
	| Cons(a, rest) => a :: tolist rest

end