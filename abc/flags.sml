structure Flags =
struct
  val verbose =
    Params.flag false (SOME ("-v", "Enable verbose info.")) "verbose"
end