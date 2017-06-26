DEFINT A-Z
' Escape Campaign (.XCP) file compiler...

' This needs some SERIOUS freaking work.


CONST version$ = "1.1"

DECLARE SUB getlevel (file$)
DECLARE FUNCTION ncword$ ()
DECLARE FUNCTION strip$ (in$)
DECLARE FUNCTION idstring$ ()
DECLARE FUNCTION padspace$ (inp$, lent)
DECLARE FUNCTION fixfile$ (infile$)
DECLARE SUB putlevel (level)
DECLARE SUB pputlevel (levelname$, position)
DECLARE SUB clbottom ()
DECLARE SUB showthing (coller)

COMMON SHARED guystx, guysty, tile(), xtag(), ytag(), title$
COMMON SHARED xmand$, levels, ofile$

DIM tile(1 TO 18, 1 TO 10)
DIM xtag(1 TO 18, 1 TO 10)
DIM ytag(1 TO 18, 1 TO 10)
DIM commands$(1 TO 20)

RANDOMIZE TIMER 'dohhhhhh

xmand$ = COMMAND$
oxmand$ = xmand$

FOR cds = 1 TO 20
commands$(cds) = ncword$
NEXT cds

' decide what we're doing...

'SELECT CASE commands$(1)
' Nevermind; screw this.
COLOR 15
PRINT : PRINT : PRINT : PRINT
FOR xe = 1 TO 6
COLOR 11: PRINT "þ"; : COLOR 9: PRINT ">";
NEXT
COLOR 14, 0
PRINT " Escape Campaign file compiler ";
FOR xe = 1 TO 6
COLOR 9: PRINT "<"; : COLOR 11: PRINT "þ";
NEXT
COLOR 7, 0
PRINT : PRINT SPACE$(21); "Version: "; version$
PRINT : PRINT "This is for registered users only. Please respect"
PRINT "this and do not distribute it."
PRINT
PRINT : COLOR 7: PRINT "Campaign output file? "; : COLOR 11: PRINT "[.XCP]"; : COLOR 15: INPUT ""; ofile$
IF ofile$ = "" THEN COLOR 4: PRINT "Fine."; : COLOR 7: END
IF INSTR(ofile$, ".") THEN  ELSE ofile$ = ofile$ + ".XCP"
ofile$ = UCASE$(ofile$)
COLOR 7: PRINT "Output to:"; : COLOR 11: PRINT ofile$
startx$ = "ESCTOESX" + CHR$(0)
COLOR 7: PRINT "Campaign title? (30)"; : COLOR 15: INPUT ""; cname$
ocname$ = padspace$(cname$, 30) + CHR$(0)
PRINT ocname$; "x"
ids$ = idstring$ + CHR$(42) + "xxx" ' "xxx" is to be filled with csum info
COLOR 7: PRINT "ID string: "; : COLOR 15: PRINT ids$
OPEN ofile$ FOR BINARY AS #2
PUT 2, 1, startx$
PUT 2, 10, ocname$
' Number of levels at 41
PUT 2, 42, ids$
' begin the levels...
levels = 0
CLS
'showthing 15
DO
redoit:
showthing 15
DO
cmd$ = UCASE$(INKEY$): IF cmd$ <> "" THEN EXIT DO

LOOP
showthing 8
clbottom
LOCATE 10, 1


SELECT CASE cmd$
                        CASE CHR$(13)
COLOR 7: PRINT "Level? "; : COLOR 11: PRINT "[.ESC]"; : COLOR 7: PRINT " (Blank line to end)"; : COLOR 15: INPUT ""; lev$
COLOR 7
IF lev$ = "" THEN GOTO redoit
levels = levels + 1
pputlevel lev$, levels
                CASE "I"
        ' insert a level...
                CASE CHR$(27)
                EXIT DO
        CASE "R"
COLOR 7: PRINT "Level position?"; : COLOR 15: INPUT "", levp
IF levp > levels THEN
        COLOR 4
        PRINT "What??? There isn't a level there yet..."
        GOTO redoit
END IF
COLOR 7: PRINT "Level? "; : COLOR 11: PRINT "[.ESC]"; : COLOR 7: PRINT " (Blank line to end)"; : COLOR 15: INPUT ""; lev$
COLOR 7
IF lev$ = "" THEN GOTO redoit
pputlevel lev$, levp
END SELECT

LOOP
x$ = CHR$(255) + CHR$(0) + CHR$(255) + CHR$(0) + CHR$(255) + CHR$(0)
PUT 2, , x$
x$ = CHR$(levels)
PUT 2, 41, x$
' now write header csum
' @ 43-45
insum$ = " "
'COLOR 7
FOR zzz = 1 TO 42
GET 2, zzz, insum$
isx = ASC(insum$)
csum1 = csum1 + isx
IF csum1 > 255 THEN csum1 = csum1 - 255
csum2 = csum2 + (csum1 XOR isx)
IF csum2 > 255 THEN csum2 = csum2 - 255
csum3 = csum3 + (isx AND csum2)
IF csum3 > 255 THEN csum3 = csum3 - 255
'PRINT csum1, csum2, csum3
NEXT zzz
COLOR 15
PRINT "Checksums: "; : COLOR 7: PRINT csum1, csum2, csum3
wax$ = CHR$(csum1)
PUT 2, 53, wax$
wax$ = CHR$(csum2)
PUT 2, 54, wax$
wax$ = CHR$(csum3)
PUT 2, 55, wax$
PRINT "Done writing checksums..."
COLOR 7: PRINT "Campaign file "; : COLOR 11: PRINT ofile$; : COLOR 7: PRINT " written."
CLOSE
END

SUB clbottom

clspace$ = SPACE$(76)
FOR xx = 10 TO 24
LOCATE xx, 1: PRINT clspace$;
NEXT




END SUB

FUNCTION fixfile$ (in$)
IF INSTR(in$, ".") THEN  ELSE in$ = in$ + ".ESC"
fixfile$ = UCASE$(in$)
END FUNCTION

SUB getlevel (file$)

OPEN file$ FOR BINARY AS #1
wa$ = " "
FOR y = 1 TO 10
FOR x = 1 TO 18
GET 1, , wa$
tile(x, y) = ASC(wa$)
NEXT: NEXT
FOR y = 1 TO 10
FOR x = 1 TO 18
GET 1, , wa$
xtag(x, y) = ASC(wa$)
NEXT: NEXT
FOR y = 1 TO 10
FOR x = 1 TO 18
GET 1, , wa$
ytag(x, y) = ASC(wa$)
NEXT: NEXT

GET 1, , wa$
guystx = ASC(wa$)
GET 1, , wa$
guysty = ASC(wa$)
title$ = SPACE$(30)
GET 1, , title$

COLOR 7: PRINT : PRINT "Read level "; : COLOR 11: PRINT strip$(title$); : COLOR 7: PRINT "..."

CLOSE 1

END SUB

FUNCTION idstring$
' random 10 digit alphanumeric string
ok$ = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
' 26 + 26 + 10 =  62
u$ = ""
FOR xxx = 1 TO 10
u$ = u$ + MID$(ok$, INT(RND * 61 + 1), 1)
NEXT xxx
idstring$ = u$


END FUNCTION

FUNCTION ncword$

vvv = LEN(xmand$)
FOR i = 1 TO vvv
g$ = MID$(xmand$, i, 1)
IF g$ = " " THEN ncword$ = oo$: xmand$ = RIGHT$(xmand$, vvv - i): EXIT FUNCTION ELSE oo$ = oo$ + g$
NEXT i
xmand$ = ""
ncword$ = oo$

END FUNCTION

FUNCTION padspace$ (instring$, lent)
instring$ = LEFT$(instring$, lent)
a$ = SPACE$(lent)
FOR m = 1 TO LEN(instring$)
MID$(a$, m, 1) = MID$(instring$, m, 1)
NEXT
padspace$ = a$

END FUNCTION

SUB pputlevel (levl$, levnum)

rlev$ = fixfile$(levl$)
getlevel levl$
IF tile(1, 1) = 0 THEN COLOR 4: PRINT "ERROR reading "; levl$; "..."; : exitcode = 42: EXIT SUB
putlevel levnum
COLOR 7: PRINT "Successfully (ha!) wrote "; : COLOR 11: PRINT levl$; : COLOR 15: PRINT " -> "; : COLOR 11: PRINT ofile$; "."
COLOR 7: PRINT "Total levels: "; : COLOR 14: PRINT STR$(levels)

END SUB

SUB putlevel (levnum)

start = ((levnum * 567) + 56) - 567
' start is position to start from
a$ = " "
GET 2, start - 1, a$
FOR y = 1 TO 10
FOR x = 1 TO 18
wa$ = CHR$(tile(x, y))
PUT 2, , wa$
NEXT: NEXT
FOR y = 1 TO 10
FOR x = 1 TO 18
wa$ = CHR$(xtag(x, y))
PUT 2, , wa$
NEXT: NEXT
FOR y = 1 TO 10
FOR x = 1 TO 18
wa$ = CHR$(ytag(x, y))
PUT 2, , wa$
NEXT: NEXT
wa$ = CHR$(guystx)
PUT 2, , wa$
wa$ = CHR$(guysty)
PUT 2, , wa$
title$ = padspace$(title$, 25)
PUT 2, , title$
END SUB

SUB showthing (clr)
COLOR clr
LOCATE 3, 1
PRINT "[Enter] Append level"
PRINT "I Insert level"
PRINT "R Replace level"
PRINT "T Change title"
PRINT "N New ID string"
PRINT "[Esc] Quit (and save)"
PRINT : PRINT


END SUB

FUNCTION strip$ (in$)
' strip off white space...
FOR u = LEN(in$) TO 1 STEP -1
IF MID$(in$, u, 1) <> " " AND MID$(in$, u, 1) <> CHR$(0) THEN EXIT FOR
NEXT
strip$ = LEFT$(in$, u)
END FUNCTION

