(* For the ABC paper, this takes a text file (maybe containing some commands?)
   and generates a dense two-column textual blob that is then inserted into
   EXE files by ABC (in exe.sml). *)
structure TwoColumn =
struct

  infixr 9 `
  fun a ` b = a b

  exception TwoColumn of string

  val WIDTH = 160
  val HEIGHT = 128

  (* Number of characters of whitespace between columns. *)
  val GUTTER = 6

  val LRBORDER = 2
  val TBBORDER = 2

  (* A single column is thus

         2                         6                        2
      DOT SPACE   LEFT-COLUMN    GUTTER   RIGHT-COLUMN   SPACE DOT *)
  val LC_WIDTH = (WIDTH - LRBORDER - LRBORDER - GUTTER) div 2
  val RC_WIDTH = WIDTH - LRBORDER - LRBORDER - GUTTER - LC_WIDTH

  val LC_START = LRBORDER
  val RC_START = LRBORDER + LC_WIDTH + GUTTER

  val debug =
    Params.flag false (SOME ("-debug", "Enable debug output.")) "debug"

  val totalpages =
    Params.param "1"
    (SOME ("-pages",
           "Total pages to generate.")) "pages"

  val startcol =
    Params.param "0"
    (SOME ("-startcol",
           "The starting column on the first line of the first page. " ^
           "Many text chunks start with some unavoidable junk (like " ^
           "the header struct for the paper's beginning) on the first " ^
           "line; this makes twocolumn aware of it.")) "startcol"

  val startline =
    Params.param "0"
    (SOME ("-startline",
           "The starting line on the first page. This is used for " ^
           "pagination and drawing of borders.")) "startline"

  val endcol =
    Params.param (Int.toString WIDTH)
    (SOME ("-endcol",
           "Column in which junk begins on the last line on the last page. " ^
           Int.toString WIDTH ^ " means the whole line is free; 0 means " ^
           "none of it is (but then why not have one fewer line?)")) "endcol"

  val endlines =
    Params.param "0"
    (SOME ("-endlines",
           "Number of full lines on the last page that can't be used. Does " ^
           "not include a partial line (-endcol).")) "endlines"

  val botclearance =
    Params.param "1"
    (SOME ("-botclearance",
           "When approaching the junk on the bottom, the number of blank " ^
           "lines to leave between the text and it.")) "botclearance"

  val outfile =
    Params.param ""
    (SOME ("-o",
           "Required: The output file to write.")) "outfile"

  fun debugpage filename p =
    let
      val f = TextIO.openOut filename
    in
      Array.app (fn b => TextIO.output1 (f, b)) p;
      TextIO.closeOut f;
      print ("Wrote debug page to " ^ filename ^ "\n")
    end

  fun dopage (pagenum : int) (text : string list) : string list =
    let
      val COLWIDTH = LC_WIDTH

      val totalpages = Params.asint 0 totalpages

      val startcol =
        if pagenum = 0
        then Params.asint 0 startcol
        else 0
      val startline =
        if pagenum = 0
        then Params.asint 0 startline
        else 0

      val endcol =
        if pagenum = totalpages - 1
        then Params.asint 0 endcol
        else WIDTH
      val endlines =
        if pagenum = totalpages - 1
        then Params.asint 0 endlines
        else 0

      val botclearance = Params.asint 1 botclearance

      val firstwriteline =
        Int.max (TBBORDER, startline)
      val lastwriteline =
        Int.min (HEIGHT - endlines - (botclearance + 2) -
                 (if endcol < RC_START then 1 else 0),
                 HEIGHT - TBBORDER - 1)


      (* Number of pages we'll generate. We work page-by-page so that
         we can add borders. *)
      (* The page is always the physical page, though we may only
         be working within part of it because some of it contains
         junk. *)
      val page = Array.array (WIDTH * HEIGHT, #" ")

      (* Current row on the page. *)
      val row = ref (if startcol = 0
                     then firstwriteline
                     else startline + 1)

      (* If true, we're writing to the second of the two columns. *)
      val right = ref false

      fun set (x, y) c =
        if x < 0 orelse x >= WIDTH orelse
           y < 0 orelse y >= HEIGHT
        then raise TwoColumn ("x/y out of range in set: " ^ Int.toString x ^ "/" ^
                              Int.toString y)
        else Array.update (page, y * WIDTH + x, c)

      val () =
        (* Mark off lines we shouldn't touch. *)
        Util.for 0 (startline - 1)
        (fn y =>
         Util.for 0 (WIDTH - 1)
         (fn x =>
          set (x, y) #"="));

      val () =
        (* Mark off lines we shouldn't touch. *)
        Util.for (HEIGHT - endlines) (HEIGHT - 1)
        (fn y =>
         Util.for 0 (WIDTH - 1)
         (fn x =>
          set (x, y) #"&"));

      val () =
        (* Mark off any junk in the first line that we have access to. *)
        Util.for 0 (startcol - 1)
        (fn x =>
         set (x, startline) #"%");

      val () =
        (* Any junk in the last line *)
        Util.for endcol (WIDTH - 1)
        (fn x =>
         set (x, HEIGHT - endlines - 1) #"/")

      (* Draw a border around the usable region. *)

      (* Top. *)
      val () =
        if startline = 0
        then
          let in
            Util.for startcol (WIDTH - 1)
            (fn x =>
             if x = 0 orelse x = WIDTH - 1
             then set (x, 0) #"+"
             else set (x, 0) #".")
          end
        else ()

      (* Left and right. *)
      val () =
        Util.for startline (HEIGHT - 1 - endlines)
        (fn y =>
         (* If we're on the first row, it was already
            dealt with by the top border. *)
         if y = 0
         then ()
         else
           let in
             (* XXX if startcol is the whole line? *)
             if y > startline orelse startcol = 0
             then set (0, y) #"."
             else ();
             if y < (HEIGHT - endlines - 1) orelse endcol = WIDTH
             then set (WIDTH - 1, y) #"."
             else ()
           end)

      (* Bottom. *)
      val () =
        if endlines = 0
        then
          Util.for 0 (endcol - 1)
          (fn x =>
           if x = 0 orelse x = WIDTH - 1
           then set (x, HEIGHT - 1) #"+"
           else set (x, HEIGHT - 1) #".")
        else ()

      fun writeline nil = nil
        | writeline (line :: rest) =
        if !row > lastwriteline
        then
          if !right
          then (line :: rest)
          else
            let in
              right := true;
              (* XXX not if we have startcol? *)
              row := firstwriteline;
              writeline (line :: rest)
            end
        else
          let
            val line = StringUtil.losespecr StringUtil.whitespec line
            val () = if size line > COLWIDTH
                     then raise TwoColumn
                       ("Line is longer than " ^
                        Int.toString COLWIDTH ^
                        " chars:\n" ^
                        CharVector.tabulate (COLWIDTH, fn _ => #"-") ^
                        "\n" ^ line)
                     else ()
          in
            Util.for 0 (size line - 1)
            (fn i =>
             set (if !right
                  then RC_START + i
                  else LC_START + i, !row) ` String.sub (line, i));
            row := !row + 1;
            writeline rest
          end

      (* Now, write line by line until we reach the end of the page. *)
      val text = writeline text

      (* Truncate the page data so that we only output the region
         that's ready to be concatenated between junk. *)
      fun truncate () =
        let
          val start = startline * WIDTH + startcol
          val rstart = endlines * WIDTH + (WIDTH - endcol)
          val len = (WIDTH * HEIGHT - start - rstart)
        in
          CharVector.tabulate (len,
                               fn i => Array.sub (page, start + i))
        end

      fun all_empty nil = true
        | all_empty ("" :: rest) = all_empty rest
        | all_empty _ = false
    in
      if !debug
      then debugpage ("debug" ^ Int.toString pagenum ^ ".page") page
      else ();

      truncate () ::
      (if pagenum + 1 = totalpages
       then if all_empty text
            then nil (* Perfect! *)
            else raise TwoColumn ("Text exceeded total page count! Looking at:\n" ^
                                  StringUtil.delimit "\n"
                                  (ListUtil.takeupto 5 text))
       else dopage (pagenum + 1) text)
    end


  fun go infile =
    let
      val () = if LC_WIDTH <> RC_WIDTH
               then raise TwoColumn
                    ("left and right columns don't have the same width. " ^
                    "Try increasing or decreasing the gutter by 1.")
               else ()

      val outfile =
        case !outfile of
          "" => raise TwoColumn "You must specify an output file with -o."
        | f => f

      fun linesfromfile f =
        String.fields (StringUtil.ischar #"\n") (StringUtil.readfile f)
      val text = linesfromfile infile

      fun process nil = nil
        | process (h :: rest) =
        (case StringUtil.removehead "%insert " h of
           (* NOT processed recursively. *)
           SOME file => linesfromfile (StringUtil.losespecsides
                                       StringUtil.whitespec file) @ process rest
         | NONE => h :: process rest)
      val text = process text

      val output = String.concat (dopage 0 text)
    in
      StringUtil.writefile outfile output
    end
  handle TwoColumn s =>
    let in
      TextIO.output (TextIO.stdErr, "Error: " ^ s ^ "\n");
      OS.Process.exit OS.Process.failure
    end

end

val () =
  Params.main1
  "Pass the input file on the command line."
  TwoColumn.go