' converts Escape (or any) file from mailable data.
DEFINT A-Z

CONST version$ = "1.231"

DECLARE FUNCTION nextcharfrom1$ (ln%)
DECLARE FUNCTION strip$ (in$)
DECLARE FUNCTION whiteout$ (in$)
DECLARE SUB pbuff (ins$)

COMMON SHARED filebuff$, zzx

ON ERROR GOTO fatalerr
CLOSE


FOR mx = 1 TO 6
COLOR 11: PRINT "Ä"; : COLOR 9: PRINT "Í";
NEXT mx
COLOR 15: PRINT " Escape Level Decoder ";
FOR mx = 1 TO 6
COLOR 9: PRINT "Í"; : COLOR 11: PRINT "Ä";
NEXT mx
PRINT
COLOR 7
PRINT "                  Version "; version$

IF COMMAND$ = "" THEN GOTO nocommand
A$ = COMMAND$

DO
IF INSTR(A$, "/") THEN
        ' there's a switch
        m = INSTR(A$, "/") + 1
        uu$ = ""
        endcm = 1
        FOR mm = m TO LEN(A$)
        u$ = MID$(A$, mm, 1)
        IF u$ = " " THEN endcm = 0: EXIT FOR
        uu$ = uu$ + u$
        NEXT mm
        SELECT CASE uu$
        CASE "DEBUG"
        COLOR 7
        PRINT "Debug Mode on."
        debugmode = 1
        CASE ELSE
        COLOR 7
        PRINT "Unknown switch, "; : COLOR 11: PRINT uu$
        COLOR 7
        END SELECT
        IF endcm = 0 THEN
        newa$ = LEFT$(A$, m - 2) + RIGHT$(A$, LEN(A$) - mm)
        ELSE
        newa$ = LEFT$(A$, m - 2)
        END IF
        A$ = newa$
        ELSE
        EXIT DO
 END IF
LOOP

IF debugmode = 1 THEN PRINT "What's left:"; A$

filebuff$ = ""
crlf$ = CHR$(13) + CHR$(10)

'INPUT "Input file? (.txt)"; a$
IF INSTR(A$, ".") THEN  ELSE A$ = A$ + ".txt"

'xyz# = 1
OPEN A$ FOR BINARY AS #1
IF LOF(1) = 0 THEN
        CLOSE
        KILL A$
        PRINT : COLOR 4: PRINT "Input file was empty.": PRINT : COLOR 7
        GOTO endit
        END IF
lookforanother:
xyz# = SEEK(1)
IF xyz# = 0 THEN xyz# = 1
' Get rid of the crap...
ax$ = "-----Begin Escape Level-----"
bx$ = ax$
DO
IF EOF(1) THEN GOTO outoffiles
GET 1, xyz#, bx$
IF bx$ = ax$ THEN EXIT DO
xyz# = xyz# + 1
LOOP
xyz# = xyz# + LEN(ax$) ' move counter
ax$ = "Filename= "
bx$ = ax$
DO
GET 1, xyz#, bx$
IF bx$ = ax$ THEN EXIT DO
xyz# = xyz# + 1
LOOP
xyz# = xyz# + LEN(ax$)
uu$ = " "
outname$ = ""
DO
GET 1, , uu$
IF uu$ = "*" OR uu$ = CHR$(13) OR uu$ = CHR$(10) THEN EXIT DO
outname$ = outname$ + uu$
LOOP
' got filename, stuff.
b$ = outname$
IF INSTR(outname$, ".") THEN  ELSE b$ = outname$ + ".esc"
PRINT : COLOR 7: PRINT "Decoding "; : COLOR 15: PRINT b$; : COLOR 7: PRINT "...";
pctx = POS(0)
pcty = CSRLIN
zzx = 0
ozzx = 0
CLOSE 2
OPEN b$ FOR BINARY AS #2
CLOSE 2
KILL b$
OPEN b$ FOR BINARY AS #2

in$ = " "
xin$ = " "
in2$ = "  "
DO UNTIL EOF(1)
nexto:
GET 1, , xin$
in$ = whiteout$(xin$)
'PRINT in$;
IF in$ = "" THEN GOTO nexto
'END
zzx = LEN(filebuff$) - ozzx
IF zzx > 128 THEN
        LOCATE pcty, pctx
        ozzx = ozzx + zzx
        PRINT , ozzx; "  ";
END IF

checkcolon:
IF in$ = "-" THEN
        ' hit eof
        GOTO csumerr
END IF
IF in$ = ":" THEN
        u$ = ""
       
        DO  ' so get the number into u$
        GET 1, , in$
        IF ASC(in$) >= 48 AND ASC(in$) <= 57 THEN  ELSE EXIT DO
        u$ = u$ + in$
        LOOP

        num = VAL(u$)
        IF num > 255 OR num < 0 THEN x = 1: GOTO fileerr
        A$ = CHR$(num)
        pbuff A$ 'PUT 2, , a$
        GOTO checkcolon
END IF
IF in$ = "]" THEN
       
        u$ = ""
        DO  ' so get the number into u$
        GET 1, , in$
        IF ASC(in$) >= 48 AND ASC(in$) <= 57 THEN  ELSE EXIT DO
        u$ = u$ + in$
        LOOP
        
        'u$ = number of reps.
        reps = VAL(u$)
        f$ = in$
        IF f$ <> "}" THEN x = 2: GOTO fileerr

        u$ = ""
        DO  ' so get the number into u$
        GET 1, , in$
        IF ASC(in$) >= 48 AND ASC(in$) <= 57 THEN  ELSE EXIT DO
        u$ = u$ + in$
        LOOP

        num = VAL(u$)
        aa$ = CHR$(num)
        out$ = ""
        FOR xxx = 1 TO reps
        out$ = out$ + aa$
        NEXT xxx
        pbuff out$ 'PUT 2, , out$
        GOTO checkcolon
END IF
IF in$ = "?" THEN
        ' get the checksums, exit (eof)...
       
        u$ = ""
        DO  ' so get the number into u$
        GET 1, , in$
        IF ASC(in$) >= 48 AND ASC(in$) <= 57 THEN  ELSE EXIT DO
        u$ = u$ + in$
        LOOP
        xsum1 = VAL(u$)
        u$ = ""
        DO  ' so get the number into u$
        GET 1, , in$
        IF ASC(in$) >= 48 AND ASC(in$) <= 57 THEN  ELSE EXIT DO
        u$ = u$ + in$
        LOOP
        xsum2 = VAL(u$)
        u$ = ""
        DO  ' so get the number into u$
        GET 1, , in$
        IF ASC(in$) >= 48 AND ASC(in$) <= 57 THEN  ELSE EXIT DO
        u$ = u$ + in$
        LOOP
        xsum3 = VAL(u$)
        EXIT DO
END IF
LOOP
' write it
' check the checksums!!
       
        LOCATE pcty, pctx
        PRINT , LEN(filebuff$) - 1; "  ";

csum1 = 0
csum2 = 0
csum3 = 0
FOR mxx = 1 TO LEN(filebuff$)
ii = ASC(MID$(filebuff$, mxx, 1))
csum1 = csum1 + ii
IF csum1 > 255 THEN csum1 = csum1 - 255
csum2 = csum2 + (csum1 XOR ii)
IF csum2 > 255 THEN csum2 = csum2 - 255
csum3 = csum3 + (csum2 AND ii)
IF csum3 > 255 THEN csum3 = csum3 - 255
NEXT mxx
IF debugmode = 1 THEN PRINT : PRINT "Expected Checksums:", xsum1, xsum2, xsum3
IF debugmode = 1 THEN PRINT "Checksums:", , csum1, csum2, csum3
IF csum1 <> xsum1 OR csum2 <> xsum2 OR csum3 <> xsum3 THEN GOTO csumerr
COLOR 10: PRINT "Checksum OK!"
filebuff$ = LEFT$(filebuff$, LEN(filebuff$) - 1)
PUT 2, , filebuff$
nextfile:
CLOSE 2
filebuff$ = ""
decoded = decoded + 1
GOTO lookforanother
CLOSE
END

csumerr:
COLOR 4: PRINT : PRINT "Checksum error on file "; : COLOR 14: PRINT b$; : COLOR 4: PRINT "!"
CLOSE 2
KILL b$
decoded = decoded - 1
GOTO nextfile
END

fileerr:
COLOR 4: PRINT : PRINT "Error in file "; : COLOR 14: PRINT b$; : COLOR 4: PRINT ":"
COLOR 7
SELECT CASE x
CASE 1
PRINT "Numeric overflow; this does not seem to be a proper "; : COLOR 11: PRINT "XENCODE"; : COLOR 7: PRINT "d file."
CASE 2
PRINT "Syntax error: no }"
CASE ELSE
PRINT x
PRINT "Unknown problem."
PRINT in$
END SELECT
decoded = decoded - 1
CLOSE 2
KILL b$
GOTO nextfile
CLOSE
END

outoffiles:
COLOR 15: PRINT "No more Escape files found."
COLOR 7: PRINT : PRINT "Total files decoded: "; : COLOR 14: PRINT decoded
COLOR 7
endit:
PRINT "Escape level decoder v"; version$; " is Copyright ("; : COLOR 11: PRINT "c"; : COLOR 7: PRINT ") 1996 Tom Murphy."
PRINT "All rights reserved."
CLOSE
END

fatalerr:
COLOR 15, 4
PRINT "FATAL ERROR!";
COLOR 7, 0
SELECT CASE ERR
CASE 14
PRINT "Out of string space! This is usually because the file to be decoded"
PRINT "is WAY TOO BIG. No escape level should do this."
CASE ELSE
PRINT : PRINT "A fatal error occured for an unknown reason. If you can repeat this"
PRINT "error, please contact Tom Murphy (ImightbeTM@aol.com). Thank you!"
END SELECT
PRINT : PRINT "Program aborted."
CLOSE
END

nocommand:
COLOR 7
PRINT : PRINT
PRINT "You must enter the input file on the command line. Example:"
COLOR 9: PRINT "XDECODE mail"
COLOR 7: PRINT "(.txt is assumed)"
PRINT "or"
COLOR 9: PRINT "XDECODE message1.doc"
COLOR 7
PRINT : PRINT "Thank you."
PRINT
GOTO endit

FUNCTION nextcharfrom1$ (ln)
END FUNCTION

SUB pbuff (ins$)
' this puts ins$ into the next spot in filebuff$.
' to save writing speed and to fix an annoying bug.

filebuff$ = filebuff$ + ins$

END SUB

DEFSNG A-Z
FUNCTION strip$ (ix$)
strip$ = RIGHT$(ix$, LEN(ix$) - 1)
END FUNCTION

DEFINT A-Z
FUNCTION whiteout$ (in$)

' removes " ", chr$(13), chr$(10)

FOR temp = 1 TO LEN(in$)
A$ = MID$(in$, temp, 1)
SELECT CASE A$
CASE " ", CHR$(13), CHR$(10)
CASE ELSE
out$ = out$ + A$
END SELECT
NEXT
whiteout$ = out$

END FUNCTION

