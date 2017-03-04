(* Really simple program that creates the test harness for DOS,
   so I don't have to try to do this with makefile scripting. *)
structure MakeTest =
struct

  fun main exes =
    app (fn exe => print (exe ^ "\n")) exes

end

val () = Params.main
  "Usage:\n  maketest test1.exe test2.exe ... > dos/test.bat"
  MakeTest.main
