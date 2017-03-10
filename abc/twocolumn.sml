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

  (* A single column is thus

         2                         6                        2
      DOT SPACE   LEFT-COLUMN    GUTTER   RIGHT-COLUMN   SPACE DOT *)
  val LC_WIDTH = (WIDTH - LRBORDER - LRBORDER - GUTTER) div 2
  val RC_WIDTH = WIDTH - LRBORDER - LRBORDER - GUTTER - LC_WIDTH

  val LC_START = LRBORDER
  val RC_START = LRBORDER + LC_WIDTH + GUTTER

  val startcol =
    Params.param "0"
    (SOME ("-startcol",
           "The starting column on the first line. Many text chunks start " ^
           "with some unavoidable junk (like the header struct for the " ^
           "paper's beginning) on the first line; this makes twocolumn " ^
           "aware of it.")) "startcol"

  val startline =
    Params.param "0"
    (SOME ("-startline",
           "The starting line on the page. This is used for pagination " ^
           "and drawing of borders.")) "startline"

  val numlines =
    Params.param (Int.toString HEIGHT)
    (SOME ("-numlines",
           "Number of lines, including a possibly incomplete first and " ^
           "last one, to render into. Gives an error if the input doesn't " ^
           "fit. This can be more than a single page.")) "numlines"

  val endcol =
    Params.param (Int.toString WIDTH)
    (SOME ("-endcol",
           "Column in which junk begins on the last line. " ^
           Int.toString WIDTH ^ " means the whole line is free; 0 means " ^
           "none of it is (but then why not have one fewer line?)")) "endcol"

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

  fun dopage (startline, startcol) text =
    let
      val COLWIDTH = LC_WIDTH

      (* Number of pages we'll generate. We work page-by-page so that
         we can add borders. *)
      (* val pages = (numlines + startline) mod HEIGHT *)
      (* The page is always the physical page, though we may only
         be working within part of it because some of it contains
         junk. *)
      val page = Array.array (WIDTH * HEIGHT, #" ")

      (* Current row on the page. *)
      val row = ref (if startcol = 0 then startline else startline + 1)

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
        (* Mark off any junk in the first line that we have access to. *)
        Util.for 0 (startcol - 1)
        (fn x =>
         set (x, startline) #"%");

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
        Util.for startline (HEIGHT - 1)
        (fn y =>
         (* If we're on the first row, it was already
            dealt with by the top border. *)
         if y = 0 (* XXX also test lastline... *)
         then ()
         else
           let in
             set (0, y) #".";
             set (WIDTH - 1, y) #"."
           end)

      (* XXX Bottom border... *)

      fun writeline nil = ()
        | writeline (line :: rest) =
        if !row >= HEIGHT
        then
          if !right
          then
            raise TwoColumn "page filled. unimplemented..."
          else
            let in
              right := true;
              row := startline; (* XXX *)
              writeline (line :: rest)
            end
        else
          let
            val line = StringUtil.losespecr StringUtil.whitespec line
            val () = if size line > COLWIDTH
                     then raise TwoColumn ("Line is longer than " ^
                                           Int.toString COLWIDTH ^
                                           "chars:\n" ^ line)
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
    in
      (* Now, write line by line until we reach the end of the page. *)
      writeline text;

      debugpage "debug.page" page
    end


  fun go infile =
    let
      val () = if LC_WIDTH <> RC_WIDTH
               then raise TwoColumn
                    ("left and right columns don't have the same width. " ^
                    "Try increasing or decreasing the gutter by 1.")
               else ()

      val text = String.fields (StringUtil.ischar #"\n") (StringUtil.readfile infile)
      val startcol = Params.asint 0 startcol
      val startline = Params.asint 0 startline
      val numlines = Params.asint 0 numlines
      val endcol = Params.asint 0 endcol
    in
      dopage (startline, startcol) text
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