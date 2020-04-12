
// The interface I would want is like (ML style)
//
// ((A -> B) -> bool) -> (A -> B)
//
// where the argument is the "test" and the output is
// the function passing the test.
//
// Runtime version,
// fun gen test =
//   letcc ret
//   in
//      fn a =>
//        let b = ... generate b ...
//        in if test (fn a => b)   (* ?? *)
//           then b
//           else longjmp ret
//         end
//   end

(* Tautology version. This doesn't make sense... *)
fun gen test =
   let cc ret
       fun f a =
         if test f
         then ... generate b ...
         else longjmp ret
   in
     f
   end

(* There is a boring first-order version of this, where you just have
   a table of test cases. *)
fun gen (table : (A * B) list) =
  case table of
    (* This meets spec because you don't have any conditions to
       satisfy! *)
    nil => loop()
  | (_, example_b) =>
      (fn a =>
       case List.find (table, op=, a) of
         (* Found a test that tells us the answer *)
         SOME (_, b) => b
       (* any B is consistent with tests *)
       | NONE => example_b)

(* So I think the more interesting version of this is one
   where we are generating actual programs