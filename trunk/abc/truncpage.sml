structure TruncPage =
struct
  infixr 9 `
  fun a ` b = a b

  exception TruncPage of string

  fun go filename =
    let
      val contents = StringUtil.readfile filename
      val contents = String.substring(contents, 0, 128 * 160)
    in
      StringUtil.writefile filename contents;
      print ("Wrote " ^ filename ^ "\n")
    end

end

val () = Params.main1 "truncpage.exe file_in_place" TruncPage.go
       handle e as TruncPage.TruncPage s =>
         let in
           TextIO.output(TextIO.stdErr, s ^ "\n");
           raise e
         end
