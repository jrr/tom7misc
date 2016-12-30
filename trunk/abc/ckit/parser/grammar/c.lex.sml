functor CLexFun(structure Tokens : C_TOKENS
                         structure TokTable : TOKENTABLE
                         sharing TokTable.Tokens = Tokens)  = struct

    structure yyInput : sig

        type stream
	val mkStream : (int -> string) -> stream
	val fromStream : TextIO.StreamIO.instream -> stream
	val getc : stream -> (Char.char * stream) option
	val getpos : stream -> int
	val getlineNo : stream -> int
	val subtract : stream * stream -> string
	val eof : stream -> bool
	val lastWasNL : stream -> bool

      end = struct

        structure TIO = TextIO
        structure TSIO = TIO.StreamIO
	structure TPIO = TextPrimIO

        datatype stream = Stream of {
            strm : TSIO.instream,
	    id : int,  (* track which streams originated 
			* from the same stream *)
	    pos : int,
	    lineNo : int,
	    lastWasNL : bool
          }

	local
	  val next = ref 0
	in
	fun nextId() = !next before (next := !next + 1)
	end

	val initPos = 2 (* ml-lex bug compatibility *)

	fun mkStream inputN = let
              val strm = TSIO.mkInstream 
			   (TPIO.RD {
			        name = "lexgen",
				chunkSize = 4096,
				readVec = SOME inputN,
				readArr = NONE,
				readVecNB = NONE,
				readArrNB = NONE,
				block = NONE,
				canInput = NONE,
				avail = (fn () => NONE),
				getPos = NONE,
				setPos = NONE,
				endPos = NONE,
				verifyPos = NONE,
				close = (fn () => ()),
				ioDesc = NONE
			      }, "")
	      in 
		Stream {strm = strm, id = nextId(), pos = initPos, lineNo = 1,
			lastWasNL = true}
	      end

	fun fromStream strm = Stream {
		strm = strm, id = nextId(), pos = initPos, lineNo = 1, lastWasNL = true
	      }

	fun getc (Stream {strm, pos, id, lineNo, ...}) = (case TSIO.input1 strm
              of NONE => NONE
	       | SOME (c, strm') => 
		   SOME (c, Stream {
			        strm = strm', 
				pos = pos+1, 
				id = id,
				lineNo = lineNo + 
					 (if c = #"\n" then 1 else 0),
				lastWasNL = (c = #"\n")
			      })
	     (* end case*))

	fun getpos (Stream {pos, ...}) = pos

	fun getlineNo (Stream {lineNo, ...}) = lineNo

	fun subtract (new, old) = let
	      val Stream {strm = strm, pos = oldPos, id = oldId, ...} = old
	      val Stream {pos = newPos, id = newId, ...} = new
              val (diff, _) = if newId = oldId andalso newPos >= oldPos
			      then TSIO.inputN (strm, newPos - oldPos)
			      else raise Fail 
				"BUG: yyInput: attempted to subtract incompatible streams"
	      in 
		diff 
	      end

	fun eof s = not (isSome (getc s))

	fun lastWasNL (Stream {lastWasNL, ...}) = lastWasNL

      end

    datatype yystart_state = 
C | L | S | INITIAL
    structure UserDeclarations = 
      struct

(* Copyright (c) 1998 by Lucent Technologies *)

(*
 * The following replacement for c.lex should give correct (ANSI)
 * In particular, we do not allow
 *
 * char *t = "abd
 * lkj";
 *
 * GCC accepts this, but SGI cc does not. This program is not ANSI
 * compliant.
 *
 *)

type svalue = Tokens.svalue
type pos = int
type lexresult = (svalue,pos) Tokens.token
type errWarn = {err: pos*pos*string->unit, warn: pos*pos*string->unit}
type lexarg =  {comLevel : int ref,
                sourceMap : SourceMap.sourcemap,
                charlist : string list ref,
                stringstart : int ref,  (* start of current string or comment*)
                errWarn: errWarn}

type arg = lexarg
type ('a,'b) token = ('a,'b) Tokens.token

fun ordof (s, i) = Char.ord (String.sub (s, i))
fun dec (iRef : int ref) = iRef := (!iRef) - 1
fun inc (iRef : int ref) = iRef := (!iRef) + 1
fun chr i = String.str(Char.chr i)
fun ord s = Char.ord(String.sub(s, 0))
fun explode s = CharVector.foldr (fn (c, l) => str c :: l) [] s
fun implode strList = String.concat strList
fun hd [] = (print "c.lex: hd of empty\n";
             raise Empty)
  | hd (h :: l) = h

val eof = fn ({comLevel,errWarn,sourceMap,stringstart,charlist}:lexarg) =>
           let val pos = Int.max(!stringstart+2, SourceMap.currPos sourceMap)
            in if !comLevel>0 then (#err errWarn) (!stringstart,pos, "unclosed comment" )
                              else ();
               Tokens.EOF(pos,pos)
           end
fun addString (charlist,s:string) = charlist := s :: (!charlist)
fun makeString charlist = (implode(rev(!charlist)) before charlist := nil)

fun mkHexInt (s,a,b,errWarn:errWarn)=((case (StringCvt.scanString (LargeInt.scan StringCvt.HEX) s) of
                  SOME i => i
                | _ => ((#err errWarn)(a,b,"trouble in parsing int");Int.toLarge(0)))
                        handle OverFlow => ((#err errWarn)(a,b,"large int const");Int.toLarge(0)))

fun mkHexChar (args as (s, a, b, errWarn:errWarn)) : int (* returns a character sized integer *) =
        let val i = mkHexInt args
        in
          if (i>255) then
            ((#warn errWarn) (a,b,"overflow in hexadecimal escape sequence");
            IntInf.toInt(i mod 256))
          else
            IntInf.toInt i
        end

fun mkOctInt (s,a,b,errWarn:errWarn)
        = ((case (StringCvt.scanString (LargeInt.scan StringCvt.OCT) s) of
                  SOME i => i
                | _ => ((#err errWarn)(a,b,"trouble in parsing int");Int.toLarge(0)))
                        handle OverFlow => ((#err errWarn)(a,b,"large int const");Int.toLarge(0)))


fun mkOctChar (args as (s, a, b, errWarn:errWarn)) (* returns a character sized integer *) =
        let val i = mkOctInt args
        in
          if (i>255) then
            ((#warn errWarn) (a,b,"overflow in octal escape sequence");
            IntInf.toInt(i mod 256))
          else
            IntInf.toInt i
        end

fun mkInt (s,a,b,errWarn:errWarn) = ((case (StringCvt.scanString (LargeInt.scan StringCvt.DEC) s) of
                  SOME i => i
                | _ => ((#err errWarn)(a,b,"trouble in parsing int");Int.toLarge(0)))
                        handle OverFlow => ((#err errWarn)(a,b,"large int const");Int.toLarge(0)))

fun mkRealNum (s,a,b,errWarn:errWarn) = ((case (StringCvt.scanString Real.scan s) of
                   SOME r => r
                 | _ => ((#err errWarn)(a,b,"trouble in parsing real");0.0))
                        handle OverFlow => ((#err errWarn)(a,b,"large real const"); 0.0))

val backslasha = 7

fun special_char(c,fst,last,errWarn:errWarn) =
                (case c of
                        "\\a" => 7
                      | "\\b" => 8
                      | "\\f" => 12
                      | "\\n" => 10
                      | "\\r" => 13
                      | "\\t" => 9
                      | "\\v" => 11
                      | _ => ordof(c,1)
                              (* strictly speaking, should only handle
                                \?, \\, \", \', but it is common
                                to simply ignore slash, and just use next char *)
                )


(* Notes on lexer states:
   INITIAL -- predefined start state and the default token state
   S -- inside a string (entered from INTITAL with double-quote)
   C -- inside a comment (entered from INITIAL with open comment sequence)
   L -- inside a line-comment (entered from INITIAL with //)
 *)




      end

    datatype yymatch 
      = yyNO_MATCH
      | yyMATCH of yyInput.stream * action * yymatch
    withtype action = yyInput.stream * yymatch -> UserDeclarations.lexresult

    local

    val yytable = 
Vector.fromList []
    fun mk yyins = let
        (* current start state *)
        val yyss = ref INITIAL
	fun YYBEGIN ss = (yyss := ss)
	(* current input stream *)
        val yystrm = ref yyins
	(* get one char of input *)
	val yygetc = yyInput.getc
	(* create yytext *)
	fun yymktext(strm) = yyInput.subtract (strm, !yystrm)
        open UserDeclarations
        fun lex 
(yyarg as ({comLevel,errWarn,sourceMap,charlist,stringstart})) () = let 
     fun continue() = let
            val yylastwasn = yyInput.lastWasNL (!yystrm)
            fun yystuck (yyNO_MATCH) = raise Fail "stuck state"
	      | yystuck (yyMATCH (strm, action, old)) = 
		  action (strm, old)
	    val yypos = yyInput.getpos (!yystrm)
	    val yygetlineNo = yyInput.getlineNo
	    fun yyactsToMatches (strm, [],	  oldMatches) = oldMatches
	      | yyactsToMatches (strm, act::acts, oldMatches) = 
		  yyMATCH (strm, act, yyactsToMatches (strm, acts, oldMatches))
	    fun yygo actTable = 
		(fn (~1, _, oldMatches) => yystuck oldMatches
		  | (curState, strm, oldMatches) => let
		      val (transitions, finals') = Vector.sub (yytable, curState)
		      val finals = map (fn i => Vector.sub (actTable, i)) finals'
		      fun tryfinal() = 
		            yystuck (yyactsToMatches (strm, finals, oldMatches))
		      fun find (c, []) = NONE
			| find (c, (c1, c2, s)::ts) = 
		            if c1 <= c andalso c <= c2 then SOME s
			    else find (c, ts)
		      in case yygetc strm
			  of SOME(c, strm') => 
			       (case find (c, transitions)
				 of NONE => tryfinal()
				  | SOME n => 
				      yygo actTable
					(n, strm', 
					 yyactsToMatches (strm, finals, oldMatches)))
			   | NONE => tryfinal()
		      end)
	    in 
let
fun yyAction0 (strm, lastMatch : yymatch) = let
      val oldStrm = !(yystrm)
      fun REJECT () = (yystrm := oldStrm; yystuck(lastMatch))
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        if not yylastwasn then REJECT() else ((SourceMap.parseDirective sourceMap
                                      (yypos,yytext); continue()))
      end
fun yyAction1 (strm, lastMatch : yymatch) = (yystrm := strm;
      (SourceMap.newline sourceMap yypos; continue()))
fun yyAction2 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN C; continue()))
fun yyAction3 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN INITIAL; continue()))
fun yyAction4 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction5 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN L; print "enter\n"; continue()))
fun yyAction6 (strm, lastMatch : yymatch) = (yystrm := strm;
      (SourceMap.newline sourceMap yypos;
                    YYBEGIN INITIAL; continue()))
fun yyAction7 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction8 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction9 (strm, lastMatch : yymatch) = (yystrm := strm;
      (charlist := [""]; stringstart := yypos; YYBEGIN S; continue()))
fun yyAction10 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN INITIAL;Tokens.STRING(makeString charlist,!stringstart,yypos+1)))
fun yyAction11 (strm, lastMatch : yymatch) = (yystrm := strm;
      ((#err errWarn) (!stringstart,yypos,"unclosed string");
                    SourceMap.newline sourceMap yypos;
                    YYBEGIN INITIAL; Tokens.STRING(makeString charlist,!stringstart,yypos)))
fun yyAction12 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (addString(charlist,yytext); continue())
      end
fun yyAction13 (strm, lastMatch : yymatch) = (yystrm := strm;
      (SourceMap.newline sourceMap yypos; continue()))
fun yyAction14 (strm, lastMatch : yymatch) = (yystrm := strm;
      (addString(charlist,chr 0);continue()))
fun yyAction15 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (addString(charlist, chr(mkOctChar(substring(yytext, 1, size(yytext)-1), yypos, yypos+size(yytext), errWarn))); continue())
      end
fun yyAction16 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (addString(charlist, chr(mkHexChar(substring(yytext, 2, size(yytext)-2), yypos, yypos+size(yytext), errWarn))); continue())
      end
fun yyAction17 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (addString(charlist,chr(ordof(yytext,2)-ord("@"))); continue())
      end
fun yyAction18 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (addString(charlist, chr(special_char(yytext, yypos, yypos+size(yytext), errWarn))); continue())
      end
fun yyAction19 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.COLON(yypos,yypos+1)))
fun yyAction20 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.SEMICOLON(yypos,yypos+1)))
fun yyAction21 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LPAREN(yypos,yypos+1)))
fun yyAction22 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RPAREN(yypos,yypos+1)))
fun yyAction23 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LBRACE(yypos,yypos+1)))
fun yyAction24 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RBRACE(yypos,yypos+1)))
fun yyAction25 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LCURLY(yypos,yypos+1)))
fun yyAction26 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RCURLY(yypos,yypos+1)))
fun yyAction27 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.DOT(yypos,yypos+1)))
fun yyAction28 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.ELIPSIS(yypos,yypos+3)))
fun yyAction29 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.COMMA(yypos,yypos+1)))
fun yyAction30 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.TIMES(yypos,yypos+1)))
fun yyAction31 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.BANG(yypos,yypos+1)))
fun yyAction32 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.HAT(yypos,yypos+1)))
fun yyAction33 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.PLUS(yypos,yypos+1)))
fun yyAction34 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.MINUS(yypos,yypos+1)))
fun yyAction35 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.INC(yypos,yypos+2)))
fun yyAction36 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.DEC(yypos,yypos+2)))
fun yyAction37 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.ARROW(yypos,yypos+1)))
fun yyAction38 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.DIVIDE(yypos,yypos+1)))
fun yyAction39 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.TILDE(yypos,yypos+1)))
fun yyAction40 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.QUESTION(yypos,yypos+1)))
fun yyAction41 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.BAR(yypos,yypos+1)))
fun yyAction42 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.AMP(yypos,yypos+1)))
fun yyAction43 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.PERCENT(yypos,yypos+1)))
fun yyAction44 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LTE(yypos,yypos+2)))
fun yyAction45 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.GTE(yypos,yypos+2)))
fun yyAction46 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.EQ(yypos,yypos+2)))
fun yyAction47 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.EQUALS(yypos,yypos+1)))
fun yyAction48 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.PLUSEQUALS(yypos,yypos+2)))
fun yyAction49 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.MINUSEQUALS(yypos,yypos+2)))
fun yyAction50 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.XOREQUALS(yypos,yypos+2)))
fun yyAction51 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.MODEQUALS(yypos,yypos+2)))
fun yyAction52 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.TIMESEQUALS(yypos,yypos+2)))
fun yyAction53 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.DIVEQUALS(yypos,yypos+2)))
fun yyAction54 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.OREQUALS(yypos,yypos+2)))
fun yyAction55 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.ANDEQUALS(yypos,yypos+2)))
fun yyAction56 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LSHIFTEQUALS(yypos,yypos+3)))
fun yyAction57 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RSHIFTEQUALS(yypos,yypos+3)))
fun yyAction58 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LT(yypos,yypos+1)))
fun yyAction59 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.GT(yypos,yypos+1)))
fun yyAction60 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.NEQ(yypos,yypos+2)))
fun yyAction61 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.OR(yypos,yypos+2)))
fun yyAction62 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.AND(yypos,yypos+2)))
fun yyAction63 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LSHIFT(yypos,yypos+2)))
fun yyAction64 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RSHIFT(yypos,yypos+2)))
fun yyAction65 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (Tokens.DECNUM(mkOctInt(yytext,yypos,yypos+size(yytext),errWarn),yypos, yypos+size(yytext)))
      end
fun yyAction66 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (Tokens.DECNUM(mkHexInt(yytext,yypos,yypos+size(yytext),errWarn),yypos, yypos+size(yytext)))
      end
fun yyAction67 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (Tokens.DECNUM(mkInt (yytext,yypos,yypos+size(yytext),errWarn), yypos,yypos+size(yytext)))
      end
fun yyAction68 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (Tokens.REALNUM(mkRealNum(yytext,yypos,yypos+size(yytext),errWarn), yypos, yypos
+ size(yytext)))
      end
fun yyAction69 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (let val s = substring(yytext, 2, size(yytext)-3)
                                     in Tokens.CCONST(IntInf.fromInt (mkOctChar(s,yypos,yypos+size(yytext),errWarn)),
                                                      yypos,
                                              yypos+size(yytext))
                                     end)
      end
fun yyAction70 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (let val s = substring(yytext, 3, size(yytext)-4)
                                     in Tokens.CCONST(IntInf.fromInt (mkHexChar(s,yypos,yypos+size(yytext),errWarn)),
                                                      yypos,
                                                      yypos+size(yytext))
                                     end)
      end
fun yyAction71 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (let val cval = ordof(yytext,1)
                                    in Tokens.CCONST(Int.toLarge cval,yypos,yypos+size(yytext))
                                    end)
      end
fun yyAction72 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (Tokens.CCONST(IntInf.fromInt(special_char(substring(yytext,1,size(yytext)-2),yypos,yypos+size(yytext),errWarn)), yypos, yypos+size(yytext)))
      end
fun yyAction73 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (TokTable.checkToken(yytext,yypos))
      end
fun yyAction74 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyQ62 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction39(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction39(strm, yyNO_MATCH)
      (* end case *))
fun yyQ61 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ64 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction61(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction61(strm, yyNO_MATCH)
      (* end case *))
fun yyQ63 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction54(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction54(strm, yyNO_MATCH)
      (* end case *))
fun yyQ60 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction41(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #">"
              then yyAction41(strm, yyNO_MATCH)
            else if inp < #">"
              then if inp = #"="
                  then yyQ63(strm', yyMATCH(strm, yyAction41, yyNO_MATCH))
                  else yyAction41(strm, yyNO_MATCH)
            else if inp = #"|"
              then yyQ64(strm', yyMATCH(strm, yyAction41, yyNO_MATCH))
              else yyAction41(strm, yyNO_MATCH)
      (* end case *))
fun yyQ59 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction25(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction25(strm, yyNO_MATCH)
      (* end case *))
fun yyQ65 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ58 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction32(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"="
              then yyQ65(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
              else yyAction32(strm, yyNO_MATCH)
      (* end case *))
fun yyQ57 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction24(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction24(strm, yyNO_MATCH)
      (* end case *))
fun yyQ56 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ66 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction73(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction73(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction73(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction73(strm, yyNO_MATCH)
                      else yyQ66(strm', yyMATCH(strm, yyAction73, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction73(strm, yyNO_MATCH)
                  else yyQ66(strm', yyMATCH(strm, yyAction73, yyNO_MATCH))
            else if inp = #"`"
              then yyAction73(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ66(strm', yyMATCH(strm, yyAction73, yyNO_MATCH))
                  else yyAction73(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ66(strm', yyMATCH(strm, yyAction73, yyNO_MATCH))
              else yyAction73(strm, yyNO_MATCH)
      (* end case *))
fun yyQ55 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction73(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction73(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction73(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction73(strm, yyNO_MATCH)
                      else yyQ66(strm', yyMATCH(strm, yyAction73, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction73(strm, yyNO_MATCH)
                  else yyQ66(strm', yyMATCH(strm, yyAction73, yyNO_MATCH))
            else if inp = #"`"
              then yyAction73(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ66(strm', yyMATCH(strm, yyAction73, yyNO_MATCH))
                  else yyAction73(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ66(strm', yyMATCH(strm, yyAction73, yyNO_MATCH))
              else yyAction73(strm, yyNO_MATCH)
      (* end case *))
fun yyQ54 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction40(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction40(strm, yyNO_MATCH)
      (* end case *))
fun yyQ69 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction57(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction57(strm, yyNO_MATCH)
      (* end case *))
fun yyQ68 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction64(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"="
              then yyQ69(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
              else yyAction64(strm, yyNO_MATCH)
      (* end case *))
fun yyQ67 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ53 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction59(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #">"
              then yyQ68(strm', yyMATCH(strm, yyAction59, yyNO_MATCH))
            else if inp < #">"
              then if inp = #"="
                  then yyQ67(strm', yyMATCH(strm, yyAction59, yyNO_MATCH))
                  else yyAction59(strm, yyNO_MATCH)
              else yyAction59(strm, yyNO_MATCH)
      (* end case *))
fun yyQ70 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction46(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction46(strm, yyNO_MATCH)
      (* end case *))
fun yyQ52 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"="
              then yyQ70(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ72 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction44(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction44(strm, yyNO_MATCH)
      (* end case *))
fun yyQ73 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction56(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction56(strm, yyNO_MATCH)
      (* end case *))
fun yyQ71 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction63(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"="
              then yyQ73(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
              else yyAction63(strm, yyNO_MATCH)
      (* end case *))
fun yyQ51 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction58(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"="
              then yyQ72(strm', yyMATCH(strm, yyAction58, yyNO_MATCH))
            else if inp < #"="
              then if inp = #"<"
                  then yyQ71(strm', yyMATCH(strm, yyAction58, yyNO_MATCH))
                  else yyAction58(strm, yyNO_MATCH)
              else yyAction58(strm, yyNO_MATCH)
      (* end case *))
fun yyQ50 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction20(strm, yyNO_MATCH)
      (* end case *))
fun yyQ49 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction19(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction19(strm, yyNO_MATCH)
      (* end case *))
fun yyQ79 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction67(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction67(strm, yyNO_MATCH)
      (* end case *))
fun yyQ78 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction67(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"V"
              then yyAction67(strm, yyNO_MATCH)
            else if inp < #"V"
              then if inp = #"M"
                  then yyAction67(strm, yyNO_MATCH)
                else if inp < #"M"
                  then if inp = #"L"
                      then yyQ79(strm', yyMATCH(strm, yyAction67, yyNO_MATCH))
                      else yyAction67(strm, yyNO_MATCH)
                else if inp = #"U"
                  then yyQ79(strm', yyMATCH(strm, yyAction67, yyNO_MATCH))
                  else yyAction67(strm, yyNO_MATCH)
            else if inp = #"m"
              then yyAction67(strm, yyNO_MATCH)
            else if inp < #"m"
              then if inp = #"l"
                  then yyQ79(strm', yyMATCH(strm, yyAction67, yyNO_MATCH))
                  else yyAction67(strm, yyNO_MATCH)
            else if inp = #"u"
              then yyQ79(strm', yyMATCH(strm, yyAction67, yyNO_MATCH))
              else yyAction67(strm, yyNO_MATCH)
      (* end case *))
fun yyQ77 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction67(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"V"
              then yyAction67(strm, yyNO_MATCH)
            else if inp < #"V"
              then if inp = #"M"
                  then yyAction67(strm, yyNO_MATCH)
                else if inp < #"M"
                  then if inp = #"L"
                      then yyQ79(strm', yyMATCH(strm, yyAction67, yyNO_MATCH))
                      else yyAction67(strm, yyNO_MATCH)
                else if inp = #"U"
                  then yyQ79(strm', yyMATCH(strm, yyAction67, yyNO_MATCH))
                  else yyAction67(strm, yyNO_MATCH)
            else if inp = #"m"
              then yyAction67(strm, yyNO_MATCH)
            else if inp < #"m"
              then if inp = #"l"
                  then yyQ79(strm', yyMATCH(strm, yyAction67, yyNO_MATCH))
                  else yyAction67(strm, yyNO_MATCH)
            else if inp = #"u"
              then yyQ79(strm', yyMATCH(strm, yyAction67, yyNO_MATCH))
              else yyAction67(strm, yyNO_MATCH)
      (* end case *))
fun yyQ82 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction68(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction68(strm, yyNO_MATCH)
      (* end case *))
fun yyQ81 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction68(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"L"
              then yyQ82(strm', yyMATCH(strm, yyAction68, yyNO_MATCH))
            else if inp < #"L"
              then if inp = #"0"
                  then yyQ81(strm', yyMATCH(strm, yyAction68, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction68(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ81(strm', yyMATCH(strm, yyAction68, yyNO_MATCH))
                  else yyAction68(strm, yyNO_MATCH)
            else if inp = #"l"
              then yyQ82(strm', yyMATCH(strm, yyAction68, yyNO_MATCH))
              else yyAction68(strm, yyNO_MATCH)
      (* end case *))
fun yyQ80 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ81(strm', lastMatch)
            else if inp < #"0"
              then yystuck(lastMatch)
            else if inp <= #"9"
              then yyQ81(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ76 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"-"
              then yyQ80(strm', lastMatch)
            else if inp < #"-"
              then if inp = #"+"
                  then yyQ80(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"0"
              then yyQ81(strm', lastMatch)
            else if inp < #"0"
              then yystuck(lastMatch)
            else if inp <= #"9"
              then yyQ81(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ83 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction68(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"L"
              then yyQ82(strm', yyMATCH(strm, yyAction68, yyNO_MATCH))
            else if inp < #"L"
              then if inp = #":"
                  then yyAction68(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction68(strm, yyNO_MATCH)
                      else yyQ83(strm', yyMATCH(strm, yyAction68, yyNO_MATCH))
                else if inp = #"E"
                  then yyQ76(strm', yyMATCH(strm, yyAction68, yyNO_MATCH))
                  else yyAction68(strm, yyNO_MATCH)
            else if inp = #"f"
              then yyAction68(strm, yyNO_MATCH)
            else if inp < #"f"
              then if inp = #"e"
                  then yyQ76(strm', yyMATCH(strm, yyAction68, yyNO_MATCH))
                  else yyAction68(strm, yyNO_MATCH)
            else if inp = #"l"
              then yyQ82(strm', yyMATCH(strm, yyAction68, yyNO_MATCH))
              else yyAction68(strm, yyNO_MATCH)
      (* end case *))
fun yyQ74 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ83(strm', lastMatch)
            else if inp < #"0"
              then yystuck(lastMatch)
            else if inp <= #"9"
              then yyQ83(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ75 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction67(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"M"
              then yyAction67(strm, yyNO_MATCH)
            else if inp < #"M"
              then if inp = #":"
                  then yyAction67(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp = #"/"
                      then yyAction67(strm, yyNO_MATCH)
                    else if inp < #"/"
                      then if inp = #"."
                          then yyQ74(strm', yyMATCH(strm, yyAction67, yyNO_MATCH))
                          else yyAction67(strm, yyNO_MATCH)
                      else yyQ75(strm', yyMATCH(strm, yyAction67, yyNO_MATCH))
                else if inp = #"F"
                  then yyAction67(strm, yyNO_MATCH)
                else if inp < #"F"
                  then if inp = #"E"
                      then yyQ76(strm', yyMATCH(strm, yyAction67, yyNO_MATCH))
                      else yyAction67(strm, yyNO_MATCH)
                else if inp = #"L"
                  then yyQ77(strm', yyMATCH(strm, yyAction67, yyNO_MATCH))
                  else yyAction67(strm, yyNO_MATCH)
            else if inp = #"f"
              then yyAction67(strm, yyNO_MATCH)
            else if inp < #"f"
              then if inp = #"V"
                  then yyAction67(strm, yyNO_MATCH)
                else if inp < #"V"
                  then if inp = #"U"
                      then yyQ78(strm', yyMATCH(strm, yyAction67, yyNO_MATCH))
                      else yyAction67(strm, yyNO_MATCH)
                else if inp = #"e"
                  then yyQ76(strm', yyMATCH(strm, yyAction67, yyNO_MATCH))
                  else yyAction67(strm, yyNO_MATCH)
            else if inp = #"m"
              then yyAction67(strm, yyNO_MATCH)
            else if inp < #"m"
              then if inp = #"l"
                  then yyQ77(strm', yyMATCH(strm, yyAction67, yyNO_MATCH))
                  else yyAction67(strm, yyNO_MATCH)
            else if inp = #"u"
              then yyQ78(strm', yyMATCH(strm, yyAction67, yyNO_MATCH))
              else yyAction67(strm, yyNO_MATCH)
      (* end case *))
fun yyQ48 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction67(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"M"
              then yyAction67(strm, yyNO_MATCH)
            else if inp < #"M"
              then if inp = #":"
                  then yyAction67(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp = #"/"
                      then yyAction67(strm, yyNO_MATCH)
                    else if inp < #"/"
                      then if inp = #"."
                          then yyQ74(strm', yyMATCH(strm, yyAction67, yyNO_MATCH))
                          else yyAction67(strm, yyNO_MATCH)
                      else yyQ75(strm', yyMATCH(strm, yyAction67, yyNO_MATCH))
                else if inp = #"F"
                  then yyAction67(strm, yyNO_MATCH)
                else if inp < #"F"
                  then if inp = #"E"
                      then yyQ76(strm', yyMATCH(strm, yyAction67, yyNO_MATCH))
                      else yyAction67(strm, yyNO_MATCH)
                else if inp = #"L"
                  then yyQ77(strm', yyMATCH(strm, yyAction67, yyNO_MATCH))
                  else yyAction67(strm, yyNO_MATCH)
            else if inp = #"f"
              then yyAction67(strm, yyNO_MATCH)
            else if inp < #"f"
              then if inp = #"V"
                  then yyAction67(strm, yyNO_MATCH)
                else if inp < #"V"
                  then if inp = #"U"
                      then yyQ78(strm', yyMATCH(strm, yyAction67, yyNO_MATCH))
                      else yyAction67(strm, yyNO_MATCH)
                else if inp = #"e"
                  then yyQ76(strm', yyMATCH(strm, yyAction67, yyNO_MATCH))
                  else yyAction67(strm, yyNO_MATCH)
            else if inp = #"m"
              then yyAction67(strm, yyNO_MATCH)
            else if inp < #"m"
              then if inp = #"l"
                  then yyQ77(strm', yyMATCH(strm, yyAction67, yyNO_MATCH))
                  else yyAction67(strm, yyNO_MATCH)
            else if inp = #"u"
              then yyQ78(strm', yyMATCH(strm, yyAction67, yyNO_MATCH))
              else yyAction67(strm, yyNO_MATCH)
      (* end case *))
fun yyQ89 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction66(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction66(strm, yyNO_MATCH)
      (* end case *))
fun yyQ88 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction66(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"V"
              then yyAction66(strm, yyNO_MATCH)
            else if inp < #"V"
              then if inp = #"M"
                  then yyAction66(strm, yyNO_MATCH)
                else if inp < #"M"
                  then if inp = #"L"
                      then yyQ89(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
                      else yyAction66(strm, yyNO_MATCH)
                else if inp = #"U"
                  then yyQ89(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
                  else yyAction66(strm, yyNO_MATCH)
            else if inp = #"m"
              then yyAction66(strm, yyNO_MATCH)
            else if inp < #"m"
              then if inp = #"l"
                  then yyQ89(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
                  else yyAction66(strm, yyNO_MATCH)
            else if inp = #"u"
              then yyQ89(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
              else yyAction66(strm, yyNO_MATCH)
      (* end case *))
fun yyQ87 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction66(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"U"
              then yyQ88(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
            else if inp < #"U"
              then if inp = #"A"
                  then yyQ87(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
                else if inp < #"A"
                  then if inp = #"0"
                      then yyQ87(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
                    else if inp < #"0"
                      then yyAction66(strm, yyNO_MATCH)
                    else if inp <= #"9"
                      then yyQ87(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
                      else yyAction66(strm, yyNO_MATCH)
                else if inp = #"L"
                  then yyQ88(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
                else if inp < #"L"
                  then if inp <= #"F"
                      then yyQ87(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
                      else yyAction66(strm, yyNO_MATCH)
                  else yyAction66(strm, yyNO_MATCH)
            else if inp = #"l"
              then yyQ88(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
            else if inp < #"l"
              then if inp = #"a"
                  then yyQ87(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
                else if inp < #"a"
                  then yyAction66(strm, yyNO_MATCH)
                else if inp <= #"f"
                  then yyQ87(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
                  else yyAction66(strm, yyNO_MATCH)
            else if inp = #"u"
              then yyQ88(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
              else yyAction66(strm, yyNO_MATCH)
      (* end case *))
fun yyQ86 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ87(strm', lastMatch)
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ87(strm', lastMatch)
                else if inp < #"0"
                  then yystuck(lastMatch)
                else if inp <= #"9"
                  then yyQ87(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"a"
              then yyQ87(strm', lastMatch)
            else if inp < #"a"
              then if inp <= #"F"
                  then yyQ87(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= #"f"
              then yyQ87(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ85 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction68(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"F"
              then yyAction68(strm, yyNO_MATCH)
            else if inp < #"F"
              then if inp = #"0"
                  then yyQ85(strm', yyMATCH(strm, yyAction68, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"."
                      then yyQ74(strm', yyMATCH(strm, yyAction68, yyNO_MATCH))
                      else yyAction68(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction68(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ85(strm', yyMATCH(strm, yyAction68, yyNO_MATCH))
                else if inp = #"E"
                  then yyQ76(strm', yyMATCH(strm, yyAction68, yyNO_MATCH))
                  else yyAction68(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ76(strm', yyMATCH(strm, yyAction68, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"L"
                  then yyQ82(strm', yyMATCH(strm, yyAction68, yyNO_MATCH))
                  else yyAction68(strm, yyNO_MATCH)
            else if inp = #"l"
              then yyQ82(strm', yyMATCH(strm, yyAction68, yyNO_MATCH))
              else yyAction68(strm, yyNO_MATCH)
      (* end case *))
fun yyQ92 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction65(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction65(strm, yyNO_MATCH)
      (* end case *))
fun yyQ91 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction65(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"V"
              then yyAction65(strm, yyNO_MATCH)
            else if inp < #"V"
              then if inp = #"M"
                  then yyAction65(strm, yyNO_MATCH)
                else if inp < #"M"
                  then if inp = #"L"
                      then yyQ92(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
                      else yyAction65(strm, yyNO_MATCH)
                else if inp = #"U"
                  then yyQ92(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
                  else yyAction65(strm, yyNO_MATCH)
            else if inp = #"m"
              then yyAction65(strm, yyNO_MATCH)
            else if inp < #"m"
              then if inp = #"l"
                  then yyQ92(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
                  else yyAction65(strm, yyNO_MATCH)
            else if inp = #"u"
              then yyQ92(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
              else yyAction65(strm, yyNO_MATCH)
      (* end case *))
fun yyQ90 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction65(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"V"
              then yyAction65(strm, yyNO_MATCH)
            else if inp < #"V"
              then if inp = #"M"
                  then yyAction65(strm, yyNO_MATCH)
                else if inp < #"M"
                  then if inp = #"L"
                      then yyQ92(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
                      else yyAction65(strm, yyNO_MATCH)
                else if inp = #"U"
                  then yyQ92(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
                  else yyAction65(strm, yyNO_MATCH)
            else if inp = #"m"
              then yyAction65(strm, yyNO_MATCH)
            else if inp < #"m"
              then if inp = #"l"
                  then yyQ92(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
                  else yyAction65(strm, yyNO_MATCH)
            else if inp = #"u"
              then yyQ92(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
              else yyAction65(strm, yyNO_MATCH)
      (* end case *))
fun yyQ84 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction65(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"M"
              then yyAction65(strm, yyNO_MATCH)
            else if inp < #"M"
              then if inp = #"8"
                  then yyQ85(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
                else if inp < #"8"
                  then if inp = #"/"
                      then yyAction65(strm, yyNO_MATCH)
                    else if inp < #"/"
                      then if inp = #"."
                          then yyQ74(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
                          else yyAction65(strm, yyNO_MATCH)
                      else yyQ84(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
                else if inp = #"E"
                  then yyQ76(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
                else if inp < #"E"
                  then if inp <= #"9"
                      then yyQ85(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
                      else yyAction65(strm, yyNO_MATCH)
                else if inp = #"L"
                  then yyQ90(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
                  else yyAction65(strm, yyNO_MATCH)
            else if inp = #"f"
              then yyAction65(strm, yyNO_MATCH)
            else if inp < #"f"
              then if inp = #"V"
                  then yyAction65(strm, yyNO_MATCH)
                else if inp < #"V"
                  then if inp = #"U"
                      then yyQ91(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
                      else yyAction65(strm, yyNO_MATCH)
                else if inp = #"e"
                  then yyQ76(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
                  else yyAction65(strm, yyNO_MATCH)
            else if inp = #"m"
              then yyAction65(strm, yyNO_MATCH)
            else if inp < #"m"
              then if inp = #"l"
                  then yyQ90(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
                  else yyAction65(strm, yyNO_MATCH)
            else if inp = #"u"
              then yyQ91(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
              else yyAction65(strm, yyNO_MATCH)
      (* end case *))
fun yyQ47 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction67(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"V"
              then yyAction67(strm, yyNO_MATCH)
            else if inp < #"V"
              then if inp = #":"
                  then yyAction67(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp = #"/"
                      then yyAction67(strm, yyNO_MATCH)
                    else if inp < #"/"
                      then if inp = #"."
                          then yyQ74(strm', yyMATCH(strm, yyAction67, yyNO_MATCH))
                          else yyAction67(strm, yyNO_MATCH)
                    else if inp <= #"7"
                      then yyQ84(strm', yyMATCH(strm, yyAction67, yyNO_MATCH))
                      else yyQ85(strm', yyMATCH(strm, yyAction67, yyNO_MATCH))
                else if inp = #"L"
                  then yyQ77(strm', yyMATCH(strm, yyAction67, yyNO_MATCH))
                else if inp < #"L"
                  then if inp = #"E"
                      then yyQ76(strm', yyMATCH(strm, yyAction67, yyNO_MATCH))
                      else yyAction67(strm, yyNO_MATCH)
                else if inp = #"U"
                  then yyQ78(strm', yyMATCH(strm, yyAction67, yyNO_MATCH))
                  else yyAction67(strm, yyNO_MATCH)
            else if inp = #"l"
              then yyQ77(strm', yyMATCH(strm, yyAction67, yyNO_MATCH))
            else if inp < #"l"
              then if inp = #"Y"
                  then yyAction67(strm, yyNO_MATCH)
                else if inp < #"Y"
                  then if inp = #"X"
                      then yyQ86(strm', yyMATCH(strm, yyAction67, yyNO_MATCH))
                      else yyAction67(strm, yyNO_MATCH)
                else if inp = #"e"
                  then yyQ76(strm', yyMATCH(strm, yyAction67, yyNO_MATCH))
                  else yyAction67(strm, yyNO_MATCH)
            else if inp = #"v"
              then yyAction67(strm, yyNO_MATCH)
            else if inp < #"v"
              then if inp = #"u"
                  then yyQ78(strm', yyMATCH(strm, yyAction67, yyNO_MATCH))
                  else yyAction67(strm, yyNO_MATCH)
            else if inp = #"x"
              then yyQ86(strm', yyMATCH(strm, yyAction67, yyNO_MATCH))
              else yyAction67(strm, yyNO_MATCH)
      (* end case *))
fun yyQ95 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ94 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction5(strm, yyNO_MATCH)
      (* end case *))
fun yyQ93 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ46 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction38(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"/"
              then yyQ94(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
            else if inp < #"/"
              then if inp = #"*"
                  then yyQ93(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                  else yyAction38(strm, yyNO_MATCH)
            else if inp = #"="
              then yyQ95(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
              else yyAction38(strm, yyNO_MATCH)
      (* end case *))
fun yyQ97 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ96 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"."
              then yyQ97(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ45 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction27(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"/"
              then yyAction27(strm, yyNO_MATCH)
            else if inp < #"/"
              then if inp = #"."
                  then yyQ96(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                  else yyAction27(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ83(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
              else yyAction27(strm, yyNO_MATCH)
      (* end case *))
fun yyQ100 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ99 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction49(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction49(strm, yyNO_MATCH)
      (* end case *))
fun yyQ98 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction36(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction36(strm, yyNO_MATCH)
      (* end case *))
fun yyQ44 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction34(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"="
              then yyQ99(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
            else if inp < #"="
              then if inp = #"-"
                  then yyQ98(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
                  else yyAction34(strm, yyNO_MATCH)
            else if inp = #">"
              then yyQ100(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
              else yyAction34(strm, yyNO_MATCH)
      (* end case *))
fun yyQ43 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ102 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction48(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction48(strm, yyNO_MATCH)
      (* end case *))
fun yyQ101 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction35(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction35(strm, yyNO_MATCH)
      (* end case *))
fun yyQ42 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction33(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #","
              then yyAction33(strm, yyNO_MATCH)
            else if inp < #","
              then if inp = #"+"
                  then yyQ101(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                  else yyAction33(strm, yyNO_MATCH)
            else if inp = #"="
              then yyQ102(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
              else yyAction33(strm, yyNO_MATCH)
      (* end case *))
fun yyQ103 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction52(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction52(strm, yyNO_MATCH)
      (* end case *))
fun yyQ41 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction30(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"="
              then yyQ103(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
              else yyAction30(strm, yyNO_MATCH)
      (* end case *))
fun yyQ40 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction22(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction22(strm, yyNO_MATCH)
      (* end case *))
fun yyQ39 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction21(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction21(strm, yyNO_MATCH)
      (* end case *))
fun yyQ111 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction70(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction70(strm, yyNO_MATCH)
      (* end case *))
fun yyQ110 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #":"
              then yystuck(lastMatch)
            else if inp < #":"
              then if inp = #"("
                  then yystuck(lastMatch)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ111(strm', lastMatch)
                      else yystuck(lastMatch)
                else if inp <= #"/"
                  then yystuck(lastMatch)
                  else yyQ110(strm', lastMatch)
            else if inp = #"G"
              then yystuck(lastMatch)
            else if inp < #"G"
              then if inp <= #"@"
                  then yystuck(lastMatch)
                  else yyQ110(strm', lastMatch)
            else if inp = #"a"
              then yyQ110(strm', lastMatch)
            else if inp < #"a"
              then yystuck(lastMatch)
            else if inp <= #"f"
              then yyQ110(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ109 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction72(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction72(strm, yyNO_MATCH)
      (* end case *))
fun yyQ108 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #":"
              then yystuck(lastMatch)
            else if inp < #":"
              then if inp = #"("
                  then yystuck(lastMatch)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ109(strm', lastMatch)
                      else yystuck(lastMatch)
                else if inp <= #"/"
                  then yystuck(lastMatch)
                  else yyQ110(strm', lastMatch)
            else if inp = #"G"
              then yystuck(lastMatch)
            else if inp < #"G"
              then if inp <= #"@"
                  then yystuck(lastMatch)
                  else yyQ110(strm', lastMatch)
            else if inp = #"a"
              then yyQ110(strm', lastMatch)
            else if inp < #"a"
              then yystuck(lastMatch)
            else if inp <= #"f"
              then yyQ110(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ114 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction69(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction69(strm, yyNO_MATCH)
      (* end case *))
fun yyQ115 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"'"
              then yyQ114(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ113 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"("
              then yystuck(lastMatch)
            else if inp < #"("
              then if inp = #"'"
                  then yyQ114(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"0"
              then yyQ115(strm', lastMatch)
            else if inp < #"0"
              then yystuck(lastMatch)
            else if inp <= #"7"
              then yyQ115(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ112 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction69(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction69(strm, yyNO_MATCH)
      (* end case *))
fun yyQ107 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"("
              then yystuck(lastMatch)
            else if inp < #"("
              then if inp = #"'"
                  then yyQ112(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"0"
              then yyQ113(strm', lastMatch)
            else if inp < #"0"
              then yystuck(lastMatch)
            else if inp <= #"7"
              then yyQ113(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ106 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"'"
              then yyQ109(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ105 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ107(strm', lastMatch)
            else if inp < #"0"
              then if inp = #"\n"
                  then yystuck(lastMatch)
                  else yyQ106(strm', lastMatch)
            else if inp = #"x"
              then yyQ108(strm', lastMatch)
            else if inp < #"x"
              then if inp <= #"7"
                  then yyQ107(strm', lastMatch)
                  else yyQ106(strm', lastMatch)
              else yyQ106(strm', lastMatch)
      (* end case *))
fun yyQ116 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction71(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction71(strm, yyNO_MATCH)
      (* end case *))
fun yyQ104 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"'"
              then yyQ116(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ38 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction74(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\v"
              then yyQ104(strm', yyMATCH(strm, yyAction74, yyNO_MATCH))
            else if inp < #"\v"
              then if inp = #"\n"
                  then yyAction74(strm, yyNO_MATCH)
                  else yyQ104(strm', yyMATCH(strm, yyAction74, yyNO_MATCH))
            else if inp = #"\\"
              then yyQ105(strm', yyMATCH(strm, yyAction74, yyNO_MATCH))
              else yyQ104(strm', yyMATCH(strm, yyAction74, yyNO_MATCH))
      (* end case *))
fun yyQ118 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction55(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction55(strm, yyNO_MATCH)
      (* end case *))
fun yyQ117 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction62(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction62(strm, yyNO_MATCH)
      (* end case *))
fun yyQ37 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction42(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"'"
              then yyAction42(strm, yyNO_MATCH)
            else if inp < #"'"
              then if inp = #"&"
                  then yyQ117(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                  else yyAction42(strm, yyNO_MATCH)
            else if inp = #"="
              then yyQ118(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
              else yyAction42(strm, yyNO_MATCH)
      (* end case *))
fun yyQ119 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction51(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction51(strm, yyNO_MATCH)
      (* end case *))
fun yyQ36 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction43(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"="
              then yyQ119(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
              else yyAction43(strm, yyNO_MATCH)
      (* end case *))
fun yyQ11 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, lastMatch)
        | SOME(inp, strm') => yyAction0(strm, lastMatch)
      (* end case *))
fun yyQ10 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyQ11(strm', lastMatch)
              else yyQ10(strm', lastMatch)
      (* end case *))
fun yyQ35 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction74(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyQ11(strm', yyMATCH(strm, yyAction74, yyNO_MATCH))
              else yyQ10(strm', yyMATCH(strm, yyAction74, yyNO_MATCH))
      (* end case *))
fun yyQ34 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction9(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction9(strm, yyNO_MATCH)
      (* end case *))
fun yyQ120 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction60(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction60(strm, yyNO_MATCH)
      (* end case *))
fun yyQ33 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"="
              then yyQ120(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ6 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ12 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\r"
              then yyAction8(strm, yyNO_MATCH)
            else if inp < #"\r"
              then if inp = #"\n"
                  then yyAction8(strm, yyNO_MATCH)
                else if inp < #"\n"
                  then if inp = #"\t"
                      then yyQ12(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                      else yyAction8(strm, yyNO_MATCH)
                else if inp = #"\f"
                  then yyQ12(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                  else yyAction8(strm, yyNO_MATCH)
            else if inp = #"!"
              then yyAction8(strm, yyNO_MATCH)
            else if inp < #"!"
              then if inp = #" "
                  then yyQ12(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                  else yyAction8(strm, yyNO_MATCH)
            else if inp = #"#"
              then yyQ10(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ32 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\r"
              then yyAction8(strm, yyNO_MATCH)
            else if inp < #"\r"
              then if inp = #"\n"
                  then yyAction8(strm, yyNO_MATCH)
                else if inp < #"\n"
                  then if inp = #"\t"
                      then yyQ12(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                      else yyAction8(strm, yyNO_MATCH)
                else if inp = #"\f"
                  then yyQ12(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                  else yyAction8(strm, yyNO_MATCH)
            else if inp = #"!"
              then yyAction8(strm, yyNO_MATCH)
            else if inp < #"!"
              then if inp = #" "
                  then yyQ12(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                  else yyAction8(strm, yyNO_MATCH)
            else if inp = #"#"
              then yyQ10(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ31 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction74(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction74(strm, yyNO_MATCH)
      (* end case *))
fun yyQ3 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ47(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp < #"0"
              then if inp = #"%"
                  then yyQ36(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp < #"%"
                  then if inp = #"\r"
                      then yyQ31(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                    else if inp < #"\r"
                      then if inp = #"\n"
                          then yyQ6(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                        else if inp < #"\n"
                          then if inp = #"\t"
                              then yyQ32(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                              else yyQ31(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                        else if inp = #"\v"
                          then yyQ31(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                          else yyQ32(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                    else if inp = #"\""
                      then yyQ34(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                    else if inp < #"\""
                      then if inp = #" "
                          then yyQ32(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                        else if inp = #"!"
                          then yyQ33(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                          else yyQ31(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                    else if inp = #"#"
                      then yyQ35(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                      else yyQ31(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp = #"+"
                  then yyQ42(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp < #"+"
                  then if inp = #"("
                      then yyQ39(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                    else if inp < #"("
                      then if inp = #"&"
                          then yyQ37(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                          else yyQ38(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                    else if inp = #")"
                      then yyQ40(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                      else yyQ41(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp = #"."
                  then yyQ45(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp < #"."
                  then if inp = #","
                      then yyQ43(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                      else yyQ44(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                  else yyQ46(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp = #"\\"
              then yyQ31(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp < #"\\"
              then if inp = #">"
                  then yyQ53(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp < #">"
                  then if inp = #";"
                      then yyQ50(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                    else if inp < #";"
                      then if inp = #":"
                          then yyQ49(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                          else yyQ48(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                    else if inp = #"<"
                      then yyQ51(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                      else yyQ52(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ55(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp < #"A"
                  then if inp = #"?"
                      then yyQ54(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                      else yyQ31(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp = #"["
                  then yyQ56(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                  else yyQ55(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp = #"{"
              then yyQ59(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp < #"{"
              then if inp = #"_"
                  then yyQ55(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp < #"_"
                  then if inp = #"]"
                      then yyQ57(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                      else yyQ58(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp = #"`"
                  then yyQ31(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                  else yyQ55(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp = #"~"
              then yyQ62(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp < #"~"
              then if inp = #"|"
                  then yyQ60(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                  else yyQ61(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyQ31(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
      (* end case *))
fun yyQ27 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction16(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ27(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ27(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction16(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ27(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
                  else yyAction16(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ27(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"F"
                  then yyQ27(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
                  else yyAction16(strm, yyNO_MATCH)
            else if inp <= #"f"
              then yyQ27(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
              else yyAction16(strm, yyNO_MATCH)
      (* end case *))
fun yyQ26 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction18(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ27(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ27(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction18(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ27(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
                  else yyAction18(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ27(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"F"
                  then yyQ27(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
                  else yyAction18(strm, yyNO_MATCH)
            else if inp <= #"f"
              then yyQ27(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
              else yyAction18(strm, yyNO_MATCH)
      (* end case *))
fun yyQ28 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction17(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction17(strm, yyNO_MATCH)
      (* end case *))
fun yyQ25 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction18(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"@"
              then yyQ28(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
            else if inp < #"@"
              then yyAction18(strm, yyNO_MATCH)
            else if inp <= #"_"
              then yyQ28(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
              else yyAction18(strm, yyNO_MATCH)
      (* end case *))
fun yyQ30 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction15(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction15(strm, yyNO_MATCH)
      (* end case *))
fun yyQ29 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ30(strm', lastMatch)
            else if inp < #"0"
              then yystuck(lastMatch)
            else if inp <= #"7"
              then yyQ30(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ24 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction18(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ29(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
            else if inp < #"0"
              then yyAction18(strm, yyNO_MATCH)
            else if inp <= #"7"
              then yyQ29(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
              else yyAction18(strm, yyNO_MATCH)
      (* end case *))
fun yyQ23 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction14(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ29(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
            else if inp < #"0"
              then yyAction14(strm, yyNO_MATCH)
            else if inp <= #"7"
              then yyQ29(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
              else yyAction14(strm, yyNO_MATCH)
      (* end case *))
fun yyQ22 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction13(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction13(strm, yyNO_MATCH)
      (* end case *))
fun yyQ21 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction18(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction18(strm, yyNO_MATCH)
      (* end case *))
fun yyQ20 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"8"
              then yyQ21(strm', lastMatch)
            else if inp < #"8"
              then if inp = #"\v"
                  then yyQ21(strm', lastMatch)
                else if inp < #"\v"
                  then if inp = #"\n"
                      then yyQ22(strm', lastMatch)
                      else yyQ21(strm', lastMatch)
                else if inp = #"0"
                  then yyQ23(strm', lastMatch)
                else if inp <= #"/"
                  then yyQ21(strm', lastMatch)
                  else yyQ24(strm', lastMatch)
            else if inp = #"_"
              then yyQ21(strm', lastMatch)
            else if inp < #"_"
              then if inp = #"^"
                  then yyQ25(strm', lastMatch)
                  else yyQ21(strm', lastMatch)
            else if inp = #"x"
              then yyQ26(strm', lastMatch)
              else yyQ21(strm', lastMatch)
      (* end case *))
fun yyQ19 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction10(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction10(strm, yyNO_MATCH)
      (* end case *))
fun yyQ18 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction11(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction11(strm, yyNO_MATCH)
      (* end case *))
fun yyQ17 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction12(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\""
              then yyAction12(strm, yyNO_MATCH)
            else if inp < #"\""
              then if inp = #"\n"
                  then yyAction12(strm, yyNO_MATCH)
                  else yyQ17(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
            else if inp = #"\\"
              then yyAction12(strm, yyNO_MATCH)
              else yyQ17(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
      (* end case *))
fun yyQ2 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yyAction12(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\""
              then yyQ19(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
            else if inp < #"\""
              then if inp = #"\n"
                  then yyQ18(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
                  else yyQ17(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
            else if inp = #"\\"
              then yyQ20(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
              else yyQ17(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
      (* end case *))
fun yyQ16 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyQ11(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
              else yyQ10(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
      (* end case *))
fun yyQ15 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction6(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction6(strm, yyNO_MATCH)
      (* end case *))
fun yyQ14 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\r"
              then yyAction7(strm, yyNO_MATCH)
            else if inp < #"\r"
              then if inp = #"\n"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp < #"\n"
                  then if inp = #"\t"
                      then yyQ12(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                      else yyAction7(strm, yyNO_MATCH)
                else if inp = #"\f"
                  then yyQ12(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                  else yyAction7(strm, yyNO_MATCH)
            else if inp = #"!"
              then yyAction7(strm, yyNO_MATCH)
            else if inp < #"!"
              then if inp = #" "
                  then yyQ12(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                  else yyAction7(strm, yyNO_MATCH)
            else if inp = #"#"
              then yyQ10(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
              else yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ13 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ1 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\r"
              then yyQ13(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp < #"\r"
              then if inp = #"\n"
                  then yyQ15(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp < #"\n"
                  then if inp = #"\t"
                      then yyQ14(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                      else yyQ13(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp = #"\v"
                  then yyQ13(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                  else yyQ14(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp = #"!"
              then yyQ13(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp < #"!"
              then if inp = #" "
                  then yyQ14(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                  else yyQ13(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp = #"#"
              then yyQ16(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyQ13(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
      (* end case *))
fun yyQ9 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction3(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction3(strm, yyNO_MATCH)
      (* end case *))
fun yyQ8 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"/"
              then yyQ9(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
              else yyAction4(strm, yyNO_MATCH)
      (* end case *))
fun yyQ7 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyQ11(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
              else yyQ10(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
      (* end case *))
fun yyQ5 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\r"
              then yyAction4(strm, yyNO_MATCH)
            else if inp < #"\r"
              then if inp = #"\n"
                  then yyAction4(strm, yyNO_MATCH)
                else if inp < #"\n"
                  then if inp = #"\t"
                      then yyQ12(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                      else yyAction4(strm, yyNO_MATCH)
                else if inp = #"\f"
                  then yyQ12(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                  else yyAction4(strm, yyNO_MATCH)
            else if inp = #"!"
              then yyAction4(strm, yyNO_MATCH)
            else if inp < #"!"
              then if inp = #" "
                  then yyQ12(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                  else yyAction4(strm, yyNO_MATCH)
            else if inp = #"#"
              then yyQ10(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
              else yyAction4(strm, yyNO_MATCH)
      (* end case *))
fun yyQ4 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction4(strm, yyNO_MATCH)
      (* end case *))
fun yyQ0 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #" "
              then yyQ5(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp < #" "
              then if inp = #"\v"
                  then yyQ4(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp < #"\v"
                  then if inp = #"\t"
                      then yyQ5(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                    else if inp = #"\n"
                      then yyQ6(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                      else yyQ4(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp = #"\f"
                  then yyQ5(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                  else yyQ4(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp = #"$"
              then yyQ4(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp < #"$"
              then if inp = #"#"
                  then yyQ7(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                  else yyQ4(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp = #"*"
              then yyQ8(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyQ4(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
      (* end case *))
in
  (case (!(yyss))
   of C => yyQ0(!(yystrm), yyNO_MATCH)
    | L => yyQ1(!(yystrm), yyNO_MATCH)
    | S => yyQ2(!(yystrm), yyNO_MATCH)
    | INITIAL => yyQ3(!(yystrm), yyNO_MATCH)
  (* end case *))
end
            end
	  in 
            continue() 	  
	    handle IO.Io{cause, ...} => raise cause
          end
        in 
          lex 
        end
    in
    fun makeLexer yyinputN = mk (yyInput.mkStream yyinputN)
    fun makeLexer' ins = mk (yyInput.mkStream ins)
    end

  end
