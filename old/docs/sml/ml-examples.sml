
fun curry2 f x y = f (x, y)

fun subsets nil = [nil]
  | subsets (h::t) = 
    let val res = subsets t
    in  (map (curry2 op :: h) res) @ res
    end

(* like filter, but pass in the rest of the list *)
fun filterrest f nil = nil
  | filterrest f (h :: t) = if (f (h, t)) then h :: (filterrest f t)
                            else filterrest f t

(* SLOW implementation of unique *)
fun unique l = filterrest (fn (x,r) => not (List.exists (fn u => u = x) r)) l

(* neat! *)

type @ = unit -> unit
exception E of @ -> @
val roll = fn x => fn y => raise E x
val unroll = fn x => (x (); fn y => y) handle E z => z;

val _ = let val w = roll (fn x => unroll x x)
        in unroll w w
        end

(* or *)
type @ = unit -> unit
exception E of @ -> @
fun roll x y = raise E x
fun unroll x = (x (); fn y => y) handle E z => z;

val _ = let val w = roll (fn x => unroll x x)
        in unroll w w
        end

(*
So, 
w = (fn y => raise E (fn x => unroll x x))

unroll w w 
 =>
(((fn y => raise E (fn x => unroll x x)) (); fn y => y) handle E z => z) w;
 =>
((raise E (fn x => unroll x x); fn y => y) handle E z => z) w;
 =>
(fn x => unroll x x) w;
 =>
unroll w w
 =>
      ...
*)

(* minimal version: *)

let type@ ={}->{}
exception$of@ -> @fun!x=(x();fn y=>y)handle$z=>z
val` =fn y=>raise$(fn z=> !z z)in! ` `end
