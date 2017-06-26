DEFINT A-Z
' converts Escape (or any) file into mailable data.

CONST version$ = "1.22"

DECLARE FUNCTION strip$ (in$)
DECLARE FUNCTION ncword$ ()
COMMON SHARED xmand$

DIM cmword$(20)
CLOSE

ON ERROR GOTO fatalerr
PRINT
FOR m = 1 TO 5
COLOR 8: PRINT "Ä";
COLOR 7: PRINT "Ä";
COLOR 15: PRINT "-";
NEXT m
COLOR 15: PRINT " Escape Level Encoder ";
FOR m = 1 TO 5
COLOR 15: PRINT "Ä";
COLOR 7: PRINT "Ä";
COLOR 8: PRINT "-";
NEXT m

PRINT
COLOR 7: PRINT "                     Version "; version$

xmand$ = LCASE$(COMMAND$)
oxmand$ = xmand$
IF xmand$ = "" THEN GOTO nocommand

FOR zm = 1 TO 20'(max 19 input files)
a$ = ncword$
IF a$ = "" THEN EXIT FOR
cmword$(zm) = a$
NEXT
zm = zm - 1
IF zm = 1 THEN b$ = cmword$(1): firste = 1
IF zm > 1 THEN b$ = cmword$(1): firste = 2

IF firste = 1 THEN IF INSTR(b$, ".") THEN COLOR 4: PRINT : PRINT "No extension allowed if encoding only one file.": GOTO endit
crlf$ = CHR$(13) + CHR$(10)
thise = firste
outt$ = b$
IF INSTR(b$, ".") THEN  ELSE outt$ = b$ + ".txt"
COLOR 7: PRINT "Outputting to "; : COLOR 11: PRINT outt$: COLOR 7
OPEN outt$ FOR BINARY AS #2

in$ = " "
DO UNTIL EOF(2) ' advance to the end
GET 2, , in$
LOOP
PUT 2, , crlf$

doit:
a$ = cmword$(thise)
thise = thise + 1
IF a$ = "" THEN GOTO noinput

inn$ = a$
IF INSTR(a$, ".") THEN  ELSE inn$ = a$ + ".esc"


PRINT : PRINT "File: "; inn$; " ";
pctx = POS(0)
pcty = CSRLIN

OPEN inn$ FOR BINARY AS 1
IF LOF(1) = 0 THEN
        CLOSE 1
        KILL inn$
        COLOR 4: PRINT : PRINT "The file "; : COLOR 11: PRINT inn$; : COLOR 4: PRINT " did not exist or was empty.": PRINT : COLOR 7
        GOTO doit
END IF
' Put the starting and ending information:

a$ = "--[XnC " + version$ + "]-----Begin Escape Level-----" + crlf$
PUT 2, , a$
a$ = "Filename= " + inn$ + crlf$ + "*" + crlf$
PUT 2, , a$

csum1 = 0
csum2 = 0
csum3 = 0

'LOCATE 2, 1: PRINT LOF(1);
'LOCATE 5, 1

amount = LOF(1)
x = 0
DO UNTIL EOF(1)
GET 1, , in$
out$ = ":" + strip$(STR$(ASC(in$)))
x = x + 1
IF x / 200 = INT(x / 200) THEN
        cpct = (x / amount) * 100
        LOCATE pcty, pctx
        PRINT , STR$(cpct); "% ";
END IF
' checksums
csum1 = csum1 + ASC(in$)
IF csum1 > 255 THEN csum1 = csum1 - 255
csum2 = csum2 + (csum1 XOR ASC(in$))
IF csum2 > 255 THEN csum2 = csum2 - 255
csum3 = csum3 + (ASC(in$) AND csum2)
IF csum3 > 255 THEN csum3 = csum3 - 255


' cheesy RLE...
IF oin$ = in$ THEN reps = reps + 1
IF oin$ <> in$ THEN
        IF reps > 3 THEN
                out$ = "]" + strip$(STR$(reps)) + "}" + strip$(STR$(ASC(oin$))) + ":" + strip$(STR$(ASC(in$)))
        END IF
        IF reps <= 3 THEN
                out$ = ""
                FOR xxx = 1 TO reps
                out$ = out$ + ":" + strip$(STR$(ASC(oin$)))
                NEXT xxx
                out$ = out$ + ":" + strip$(STR$(ASC(in$)))
        END IF
        reps = 0
        IF widt > 55 THEN widt = 0: out$ = out$ + crlf$
        widt = widt + LEN(out$)
        PUT 2, , out$
END IF
oin$ = in$
LOOP

' put the checksums...
'PRINT "Checksums:"; csum1; csum2; csum3
a$ = crlf$ + "?" + strip$(STR$(csum1)) + "?" + strip$(STR$(csum2)) + "?" + strip$(STR$(csum3))
PUT 2, , a$
a$ = crlf$ + "------End Escape Level------" + crlf$
PUT 2, , a$
        LOCATE pcty, pctx
        PRINT , "100%"
encoded = encoded + 1
CLOSE 1
GOTO doit

END

nocommand:
COLOR 7
PRINT : PRINT "Usage:": COLOR 9: PRINT "XENCODE input"
COLOR 7: PRINT "or"
COLOR 9: PRINT "XENCODE output input [input [input [...]]]"
COLOR 7
COLOR 14: PRINT : PRINT "You must enter the input file on the command line. Example:"
COLOR 9: PRINT "XENCODE level"
COLOR 7: PRINT "(.esc is assumed)"
PRINT "or"
COLOR 9: PRINT "XENCODE outmail.txt border square.xcp endgame.xrc"
COLOR 7
PRINT : PRINT "Thank you."
PRINT
GOTO endit

endit:
COLOR 7
PRINT "Encoded"; : COLOR 15: PRINT encoded; : COLOR 7: PRINT "files."
PRINT "Escape level encoder v"; version$; " is Copyright ("; : COLOR 11: PRINT "c"; : COLOR 7: PRINT ") 1996 Tom Murphy."
PRINT "All rights reserved."
CLOSE
END

noinput:
COLOR 15: PRINT "No more input files."
GOTO endit

fatalerr:
COLOR 15, 4
PRINT "FATAL ERROR!";
COLOR 7, 0
PRINT : PRINT "A fatal error occured for an unknown reason. If you can repeat this"
PRINT "error, please contact Tom Murphy (ImightbeTM@aol.com). Thank you!"
PRINT : PRINT "Program aborted."
CLOSE
END

DEFSNG A-Z
FUNCTION ncword$
' uses 'n modifies xmand$
vvv = LEN(xmand$)
FOR i = 1 TO vvv
g$ = MID$(xmand$, i, 1)
IF g$ = " " THEN ncword$ = oo$: xmand$ = RIGHT$(xmand$, vvv - i): EXIT FUNCTION ELSE oo$ = oo$ + g$
NEXT i
xmand$ = ""
ncword$ = oo$

END FUNCTION

FUNCTION strip$ (ix$)
strip$ = RIGHT$(ix$, LEN(ix$) - 1)
END FUNCTION

