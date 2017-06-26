
(* This is a small application for analyzing the Wikipedia (wikipedia.org)
   database offline to find articles with problems to fix. In its current
   form, it attempts to find paragraphs that lack final punctuation. *)

val stylesheet =
  "<STYLE TYPE=\"text/css\">\n" ^
  "     H1 { font: bold 16pt Verdana,Arial,Helvetica }\n" ^
  "     P { font: 10pt Verdana,Arial,Helvetica }\n" ^
  "     TH { font: bold 10pt Verdana,Arial,Helvetica ; \n" ^
  "          text-align: center}\n" ^
  "     TD { font: 10pt Verdana,Arial,Helvetica }\n" ^
  "     A:link { color: #4444DD }\n" ^
  "     A:visited { color: #9999FF }\n" ^
  "     A:active { color: #DDDD44 }\n" ^
  "</STYLE>\n"

(* print something for every 50 articles we look at *)
val progscale = 500

local
  val pr = ref 0
in
  fun progress () =
    let in
      pr := (!pr + 1) mod progscale;
      if !pr = 0
      then print "."
      else ()
    end
end

(* maximum number of articles to return from one query *)
val qnum = 1000

(* return a query, and new 'low' range *)
fun qrange lowest max =
     "select cur_id, cur_title, cur_text " ^
     (* don't get redirects, or non-article namespace *)
     "from wikipedia.cur where cur_is_redirect=0 " ^
     (* much faster without this, explain select says that
        it then "uses filesort". we can filter these out with
        mwdumper, so do that instead at import time. *)
     (* "and cur_namespace=0" ^ *)
     " and cur_id >= " ^ Int.toString lowest ^ 
     " and cur_id <= " ^ Int.toString max ^
     " order by cur_id " ^
     " limit " ^ Int.toString qnum

(* separate into paragraphs *)
fun paragraphs s = StringUtil.sfields "\n\n" s

(* change this to search for different problems. 
   here we search for paragraphs that don't end in punctuation *)
fun isbad (s, nexts) =
  let 
    (* filter leading newlines, since we treat exactly two
       newlines as a paragraph separator *)
    val s = StringUtil.losespecl (StringUtil.charspec "\r\n") s
    val nexts = StringUtil.losespecl (StringUtil.charspec "\r\n") nexts

    fun startsfunny s =
      size s > 0 andalso
      (case String.sub (s, 0) of
         #";" => true
       | #" " => true
       | #":" => true
       | #"*" => true
       | #"#" => true
       | #"|" => true
       | #"-" => true
       | #"=" => true
       | #"<" => true
       | #"{" => true
       | _ => 
         StringUtil.matchhead "[[Category:" s orelse 
         StringUtil.matchhead "[[Image:" s orelse
         StringUtil.matchhead "[[category:" s orelse 
         StringUtil.matchhead "[[image:" s orelse

         StringUtil.matchhead "''See also" s orelse
         StringUtil.matchhead "''see also" s orelse

         StringUtil.matchhead "''For a" s orelse
         StringUtil.matchhead "''For further" s orelse

         StringUtil.matchhead "''This page" s orelse
         StringUtil.matchhead "''This article" s orelse

         (* language link [[de:Verlag]] *)
         (StringUtil.matchhead "[[" s 
          andalso size s > 4 andalso 
          String.sub(s, 4) = #":")
         )
      
    (* the majority of the text should be outside wiki markup 
       for this to be considered a paragraph. *)
    fun isn't_text s =
      let
        fun count_out inn out n =
          if n < size s
          then (case String.sub(s, n) of
                  #"[" => count_inn inn out (n + 1) 1
                  (* if ] then bad markup *)
                | _ => count_out inn (out + 1) (n + 1))
          else (inn, out)
        and count_inn inn out n depth =
          if n < size s
          then
            (case String.sub(s, n) of
               #"[" => count_inn inn out (n + 1) (depth + 1)
             | #"]" => 
               (case (depth - 1) of
                  0 => count_out inn out (n + 1)
                | d => count_inn inn out (n + 1) d)
             | _ => count_inn (inn + 1) out (n + 1) depth)

          (* this is bad markup: unclosed []s! *)
          else (inn, out)

        val (inn, out) = count_out 0 0 0
      in
        inn >= out
      end

    (* check if the next paragraph is a list, or math.
       if so, it makes sense for this paragraph to end
       without punctuation *)
    fun islistetc s =
      StringUtil.matchhead "<math>" s
      orelse StringUtil.matchhead ":" s
      orelse StringUtil.matchhead "*" s
      orelse StringUtil.matchhead "#" s
      orelse StringUtil.matchhead " " s

  in
    if size s = 0
       orelse islistetc nexts
       orelse List.exists startsfunny 
              (String.fields (StringUtil.ischar #"\n") s)
       orelse isn't_text s
    then NONE
    else
         (* now filter some other things that aren't
            really paragraphs *)
         case StringUtil.token (StringUtil.ischar #" ") s of
           (_, "") => NONE (* one word *)
         | _ =>
             (* now check if it ends in punctuation *)
             let 
               
               (* removes trailing spaces, matched parens *)
               fun rm_trail s =
                 let
                   val s = 
                     StringUtil.losespecr (StringUtil.charspec 
                                           (" \r\n\t'\"" ^ (* " *)
                                            (* fancy quote *)
                                           implode [chr 148])) s
                                      
                 in
                   if size s = 0
                   then ""
                   else if (String.sub(s, size s - 1)) = #")"
                        then (case StringUtil.rfind "(" s of
                                NONE => s (* ??? *)
                              | SOME i => rm_trail (String.substring
                                                    (s, 0, i)))
                        else s
                 end

               val s' = rm_trail s

                 (* expensive, since it needs to search the whole string
                    (many times) *)
                 fun finalfilter "" = NONE
                   | finalfilter ss =
                   (* does it end in an image or URL? *)
                   let
                     val (prevtext, lastword) = 
                       let 
                         val ws = StringUtil.charspec " \r\n\t"
                         val z = 
                           StringUtil.losespecr ws ss

                         fun getidx 0 _ = 0
                           | getidx i depth = 
                           let 
                             val c = Vector.sub(z, i)
                           in
                             (* end at whitespace, or period, since
                                a commonly-seen non-mistake is:

                                century.[[image:king_atomocles.jpg|thumb|right|King 
                                Atomocles, as rendered by Jasper the Elder.]] *)

                             if ws c orelse c = #"."
                             then (if depth = 0
                                   then i
                                   else (getidx (i - 1) depth))
                             else
                               case c of
                                 #"]" => getidx (i - 1) (depth + 1)
                               | #"[" => getidx (i - 1) (depth - 1)
                               | _ => getidx (i - 1) depth
                           end
                         
                         val ii = getidx (size z - 1) 0
                       in
                         (String.substring(z, 0, ii),
                          String.substring(z, ii + 1, size z - (ii + 1)))
                       end

                     (* same as above, but don't treat period as a
                        word separator, and don't worry about matching
                        [[]]s. (this is important because urls often
                        have periods in them) *)
                     val (_, lastword_easy) =
                       StringUtil.rtoken (StringUtil.charspec " \r\n") ss

                       (*
                     val _ = 
                       if Option.isSome (StringUtil.find "Image:" ss)
                       then print ("lastword: " ^ lastword ^ "\n")
                       else ()
                       *)
                   in
                     if StringUtil.matchhead "[[Image:" lastword
                  orelse StringUtil.matchhead "[[image:" lastword
                  (* this almost always contains a period not in brackets, so
                     lastword is useless *)
                  orelse StringUtil.matchhead "http:" lastword_easy
                  orelse StringUtil.matchhead "ftp:" lastword_easy
                  orelse StringUtil.matchhead "[http:" lastword
                  orelse StringUtil.matchhead "[[http:" lastword
                  orelse StringUtil.matchhead "''Main article:" ss
                  orelse StringUtil.matchhead "''main article:" ss
                  (* suspicious manual formatting *)
                  orelse (Option.isSome (StringUtil.find "br>" ss))
                  orelse (Option.isSome (StringUtil.find "See:" ss))
                  orelse (Option.isSome (StringUtil.find "see:" ss))
                  orelse (Option.isSome (StringUtil.find "''See" ss))
                  orelse (Option.isSome (StringUtil.find "''see" ss))
                  orelse (Option.isSome (StringUtil.find "see also:" ss))
                  orelse (Option.isSome (StringUtil.find "See also:" ss))
                  orelse (Option.isSome (StringUtil.find "ISBN" ss))
                  orelse (Option.isSome (StringUtil.find "INFOBOX" ss))
                  (* see [[other article]] *)
                  orelse StringUtil.matchtail "ee also" prevtext
                  orelse StringUtil.matchtail "see" prevtext
                  orelse StringUtil.matchtail "See" prevtext
                  orelse StringUtil.matchtail "compare" prevtext
                  orelse StringUtil.matchhead "see" ss
                  orelse StringUtil.matchhead "See" ss
                  orelse StringUtil.matchtail "<!--" ss
                  orelse StringUtil.matchtail "including" ss
                  orelse StringUtil.matchtail "such as" ss
                  orelse StringUtil.matchtail "include" ss
                  orelse StringUtil.matchtail "includes" ss
                  orelse StringUtil.matchtail "the formula" ss
                  orelse StringUtil.matchtail "the equation" ss
                  orelse StringUtil.matchtail "as follows" ss
                  orelse StringUtil.matchtail "defined as" ss
                  orelse StringUtil.matchtail "stated as" ss
                  orelse StringUtil.matchtail "written as" ss
                  orelse StringUtil.matchtail "definition is" ss
                  orelse StringUtil.matchtail "given by" ss
                  orelse StringUtil.matchtail "equation is" ss
                  orelse StringUtil.matchtail "formula is" ss
                  orelse StringUtil.matchtail "such that" ss
                  orelse StringUtil.matchtail "then" ss
                  orelse StringUtil.matchtail "approximately" ss
                  orelse StringUtil.matchtail "we have" ss
                  (* various specials like __TOC__ *)
                  orelse StringUtil.matchtail "__" ss
                  orelse StringUtil.matchtail "D.C.]]" ss
                  orelse StringUtil.matchtail "Jr.]]" ss
                  orelse StringUtil.matchtail "Sr.]]" ss
                  orelse StringUtil.matchtail "U.S.]]" ss
                  orelse StringUtil.matchtail "U.S.A.]]" ss
                  orelse (StringUtil.matchhead "''" ss andalso
                          StringUtil.matchtail "''" ss)
                  then NONE
                  else SOME ss
                    end

                 fun endspunct s' =
                   if size s' = 0 
                   then true (* well, we want to filter it *)
                   else
                    case String.sub (s', size s' - 1) of
                      #"." => true
                    | #"!" => true
                    | #"?" => true
                    (* assume this ends an html entity *)
                    | #";" => true
                    (* a bizarre but common idiom *)
                    | #"-" => size s' > 1 andalso String.sub(s', size s' - 2) = #":"
                    (* assume these are before some math markup, or whatever *)
                    | #":" => true
                    | #"," => true
                    (* filter out some funny business, too *)
                    | #"}" => true
                    | #">" => true
                      
                    | #"]" => 
                      (* is web link? *)
                      String.sub(s', size s' - 2) <> #"]"
                    | _ => false
             in

               (* s has parens at end, s' does not *)
               if (StringUtil.matchhead "'''" s'
                   andalso (Option.isSome (StringUtil.find ":" s')))
                  orelse endspunct s'
                  orelse endspunct (StringUtil.losespecr
                                    (StringUtil.charspec 
                                     (" ()\r\n\t'\"" ^ (*"*)
                                     (* fancy quote *)
                                     implode [chr 148]))
                                    s)
               then NONE
               else finalfilter s
             end
  end

(* Don't even look at articles that need to be cleaned up or
   wikified, since these are likely to have things like unbulleted
   lists. *)
fun exclude s =
  (Option.isSome (StringUtil.find "#REDIRECT" s)) orelse
  (Option.isSome (StringUtil.find "{{cleanup" s)) orelse
  (Option.isSome (StringUtil.find "{{wikify" s))

fun process file id title text =
  if exclude text
  then false
  else
  let
    (* like mappartial, but each call to the function gets
       the next element. pass in a sentinel element x for the
       last call *)
    fun mapPartialNext _ _ nil = nil
      | mapPartialNext f x [h] =
      (case f(h, x) of
         NONE => nil
       | SOME e => [e])
      | mapPartialNext f x (a::b::l) =
         (case f(a, b) of
            NONE => mapPartialNext f x (b::l)
          | SOME e => e :: mapPartialNext f x (b::l))

    val bads = mapPartialNext isbad "" (paragraphs text)

    (* protect against runaway html markup *)
    fun nohtml s =
      StringUtil.replace "<" "&lt;" (StringUtil.replace ">" "&gt;" s)

    fun underspace s = StringUtil.replace "_" " " s
  in
    case bads of
      nil => false
    | _ => 
        let
          fun line s = TextIO.output (file, "<p>" ^ nohtml s ^ "\n")
        in
          TextIO.output 
          (file, 
           "<p><div style=\"width : 95% ; margin : 2px ; padding: 2px ; " ^
           "border : 1px solid black ; " ^
           "font: 12pt Verdana,Arial,Helvetica ; " ^
           "background : #BBBBFF\"><b>" ^
           Int.toString id ^ ": " ^
           "<a href=\"http://en.wikipedia.org/wiki/" ^ 
           nohtml title ^ "\">"
           ^ underspace (nohtml title) ^ "</a></b> [" ^
           "<a href=\"http://en.wikipedia.org/w/index.php?title=" ^
           nohtml title ^ "&action=edit\">edit</a>]" ^
           "</div>\n");
          app line bads;
          true
        end
  end

exception Oops of string

fun test file start per max =
  let
    val m = MySQL.connect "root" NONE

    fun dosome f lowest remaining =
      ( (* print ("dosome f " ^ Int.toString lowest ^ " " ^
              Int.toString remaining ^ "\n"); *)
      if remaining > 0
      then 
        let val q = qrange lowest max
        in
          (* print ("Query: " ^ q ^ "\n"); *)
          case MySQL.query m q of
            NONE => (print "no result!!\n";
                     raise Oops "no result!!")
          | SOME r =>
              let
                (* keep track of the number of
                   errors we've found, and the latest
                   article ID we saw *)
                fun read num nextlow =
                  case MySQL.readone m r of
                    NONE => (num, nextlow)
                  | SOME [SOME (MySQL.Int id), 
                          SOME (MySQL.String title), 
                          SOME (MySQL.String text)] =>
                      let in
                        progress ();
                        if process f id title text
                        then read (num + 1) id
                        else read num id
                      end
                  | _ => (print "Bad row\n";
                          raise Oops "bad row from server")
                    
              val (found, nextlow) = read 0 lowest
            in
              (* print ("nextlow: " ^ Int.toString nextlow ^ "\n"); *)
              MySQL.free r;
              if nextlow > lowest
              then
                dosome f nextlow (remaining - found)
                (* XXX this is normal end condition *)
              else (print "No progress??\n";
                    raise Oops "no progress??")
            end
        end
      else 
        lowest (* done with this file *)
        )

    fun header f n st per =
      let in
        TextIO.output
        (f,
         "<html><head><title>Periodbot output starting #" ^ 
         Int.toString st ^ 
         "</title>\n" ^
         stylesheet ^
         "</head><body bgcolor=\"#FFFFFF\">\n" ^
         "<h1>Periodbot output (file #" ^ Int.toString n ^ ") " ^
         "starting at article #" ^ Int.toString st ^ 
         " (" ^ Int.toString per ^ " errors)" ^
         " (<a href=\"http://en.wikipedia.org/wiki/Wikipedia:" ^
         "WikiProject_Punctuation\">Project Punctuation</a>)</h1>\n" ^
         "<br/>Generated " ^ 
           Date.toString (Date.fromTimeUniv (Time.now ())) ^ 
           " (UTC)\n" ^
         "<p>\n")
      end
      
    fun footer f = TextIO.output (f, "</body></html>\n")

    (* start at st and do 'per' errors into a new file *)
    fun chunk st n =
      let

        val fs = FSUtil.dirplus file (Int.toString n ^ ".html")
        val f = TextIO.openOut fs
          
        (* percent done *)
        val pct = ((st - start) * 100) div (max - start)
          
        val () = header f n st per
        val () = print ("(" ^ Int.toString pct ^ "%) file " ^ 
                        Int.toString n ^ ", id " ^
                        Int.toString st ^ ": ")

        val newlow = dosome f st per
      in
        footer f;
        TextIO.closeOut f;
        print "\n";
        if newlow < max
        then chunk newlow (n + 1)
        else ()
      end

  in
    chunk start 1;
    MySQL.close m
  end

fun protect f =
  (ignore (f ())) handle e => 
    let in
      print (exnName e ^ ": " ^ exnMessage e ^ "\n");
      app (fn s => print (s ^ "\n")) (MLton.Exn.history e)
    end

exception Usage

val _ = 
  (case CommandLine.arguments () of
     file :: ints =>
       (case map Int.fromString ints of
          [SOME start, SOME per, SOME max] => 
            protect (fn () => test file start per max)
        | _ => raise Usage)
   | _ => raise Usage)
     handle Usage =>
       print ("Usage: ./periodbot fileprefix start# countperfile# maximum#\n" ^
              "  fileprefix is like /home/tom/www/periodbot/\n" ^
              "  min# is usually 0, unless interrupted\n" ^
              "  countperfile# is the number of errors to\n" ^
              "    put in each file\n" ^
              "  maximum# is the maximum article number to look at\n\n")
