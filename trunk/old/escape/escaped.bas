DEFINT A-Z
' ESCAPED - The ESCAPE map editor.
CONST version$ = "1.93á"
DECLARE FUNCTION MouseInit% ()
DECLARE FUNCTION padspace$ (instring$, length)
DECLARE FUNCTION getcpu2% ()
DECLARE FUNCTION addext$ (infile$, ext$)
DECLARE FUNCTION tround% (in%)
DECLARE FUNCTION getstr$ (length, default$)
DECLARE FUNCTION stripend$ (in$)
DECLARE FUNCTION linstr (strinng$, init$)
DECLARE FUNCTION validf (fil$)
DECLARE FUNCTION stripext$ (filee$)
DECLARE SUB randomspot (rtype)
DECLARE SUB loadagif (aloady$, adjust%)
DECLARE SUB MouseDriver (ax%, bx%, cx%, dx%)
DECLARE SUB mousestatus (lb%, rb%, xmouse%, ymouse%)
DECLARE SUB mousehide ()
DECLARE SUB mouseshow ()
DECLARE SUB mouseput (x%, y%)
DECLARE SUB tileput (x%, y%, tile%)
DECLARE SUB waitrelease ()
DECLARE SUB drawscreen ()
DECLARE SUB mouserange (x%, y%, x2%, y2%)
DECLARE SUB putguy (x, y)
DECLARE SUB toptiles (which)
DECLARE SUB save (file$)
DECLARE SUB ptitle (stuff$)
DECLARE SUB statmsg (text$, seconds)
DECLARE SUB cPRINT (text$)
DECLARE SUB rstatmsg ()
DECLARE SUB textmap ()

COMMON SHARED x, y, lb, rb, xmouse, ymouse, eraseold
COMMON SHARED floor1(), redblock(), blublock(), grablock(), greblock(), exitdoor()
COMMON SHARED doorup(), gldblock(), shooter(), steptile(), arrowright(), arrowleft()
COMMON SHARED arrowup(), arrowdown(), hole(), colors(), savefile$
COMMON SHARED rough(), electric(), elecon(), elecoff(), break()
COMMON SHARED transp(), horiz(), vert(), but0(), but1()
COMMON SHARED dude(), lasttile, ctiles, tile, savef$, fatalerr
COMMON SHARED facing, guystx, guysty, absguyx, absguyy, title$
COMMON SHARED tile(), xtag(), ytag(), object(), sttimeup!, message
COMMON SHARED wireud(), wireur(), wireul(), wiredr(), wiredl(), wirelr()
COMMON SHARED wireon(), blulite(), redlite(), grelite()
COMMON SHARED bluehi(), bluelo(), redhi(), redlo(), grehi(), grelo()

'$INCLUDE: 'loadgif.bi'

DIM SHARED asmr$: asmr$ = SPACE$(94) ' declare string

RANDOMIZE TIMER


DIM tile(1 TO 18, 1 TO 10)
DIM object(1 TO 18, 1 TO 10)
DIM xtag(1 TO 18, 1 TO 10)
DIM ytag(1 TO 18, 1 TO 10)
DIM colors(1 TO 100)

' Get command line...
file$ = COMMAND$
IF file$ <> "" THEN loadfirst = 1


'DIM x, y, lb, rb, xmouse, ymouse AS INTEGER
FOR zuu = 1 TO 8
COLOR 15: PRINT "ú"; : COLOR 9: PRINT "ù";
NEXT zuu
COLOR 14
PRINT " Escaped ";
FOR zuu = 1 TO 8
COLOR 9: PRINT "ù"; : COLOR 15: PRINT "ú";
NEXT zuu
PRINT
COLOR 7
PRINT "CPU is an "; : COLOR 14: PRINT "80"; RIGHT$(STR$(getcpu2%), 3)
COLOR 7: PRINT "Looking for mouse..."
GOSUB loadmouse
GOSUB loadsave
' -------------
mtime! = .07
sstime! = 180 ' *************** FIX THIS.
aitype = 2
eraseold = 1
' -------------

SCREEN 13
loadagif "escape.gif", 0
mouseshow

    'Array           '# 'Tag 'Sp
DIM floor1(129)      '1
DIM redblock(129)    '2       X
DIM blublock(129)    '3
DIM grablock(129)    '4       X
DIM greblock(129)    '5       X
DIM exitdoor(129)    '6       X
DIM hole(129)        '7      
DIM gldblock(129)    '8       X
DIM shooter(129)     '9       X
DIM steptile(129)    '10  S   X
DIM doorup(129)      '11      X
DIM arrowright(129)  '12
DIM arrowleft(129)   '13
DIM arrowup(129)     '14
DIM arrowdown(129)   '15
DIM rough(129)       '16
DIM electric(129)    '17      X
DIM elecon(129)      '18      X
DIM elecoff(129)     '19   
DIM transp(129)      '20  S   X
DIM break(129)       '21      X
DIM horiz(129)       '22
DIM vert(129)        '23
DIM but0(129)        '24
DIM but1(129)        '25

DIM wireud(129)      '26
DIM wireur(129)      '27
DIM wireul(129)      '28
DIM wiredr(129)      '29
DIM wiredl(129)      '30
DIM wirelr(129)      '31
DIM wireon(129)      '32
DIM blulite(129)     '33
DIM redlite(129)     '34
DIM grelite(129)     '35
DIM bluehi(129)      '36
DIM bluelo(129)      '37
DIM redhi(129)       '38
DIM redlo(129)       '39
DIM grehi(129)       '40
DIM grelo(129)       '41

DIM dude(512) 'o1

' -0-0-0-0-0-0-0-0-0-0-0-0-
lasttile = 41
ctiles = 1
mctiles = INT(lasttile / 17) + 1
tile = 3
' -0-0-0-0-0-0-0-0-0-0-0-0-

' dimensions are 15x15
GET (8, 24)-(23, 39), floor1
GET (24, 24)-(39, 39), redblock
GET (40, 24)-(55, 39), blublock
GET (56, 24)-(71, 39), grablock
GET (72, 24)-(87, 39), greblock
GET (88, 24)-(103, 39), exitdoor
GET (120, 24)-(135, 39), hole
GET (136, 24)-(151, 39), gldblock
GET (152, 24)-(167, 39), shooter
GET (168, 24)-(183, 39), steptile
GET (184, 24)-(199, 39), doorup
GET (200, 24)-(215, 39), arrowright
GET (216, 24)-(231, 39), arrowleft
GET (232, 24)-(247, 39), arrowup
GET (248, 24)-(263, 39), arrowdown
GET (264, 24)-(279, 39), rough
GET (8, 40)-(23, 55), electric
GET (24, 40)-(39, 55), elecon
GET (40, 40)-(55, 55), elecoff
GET (56, 40)-(71, 55), transp
GET (72, 40)-(87, 55), break
GET (88, 40)-(103, 55), horiz
GET (104, 40)-(119, 55), vert
GET (120, 40)-(135, 55), but0
GET (136, 40)-(151, 55), but1
GET (8, 56)-(23, 71), wiredr
GET (24, 56)-(39, 71), wiredl
GET (40, 56)-(55, 71), wirelr
GET (56, 56)-(71, 71), wireon
GET (72, 56)-(87, 71), redlo
GET (88, 56)-(103, 71), bluelo
GET (104, 56)-(119, 71), grelo
GET (8, 72)-(23, 87), wireur
GET (24, 72)-(39, 87), wireul
GET (40, 72)-(55, 87), wireud

GET (72, 72)-(87, 87), redhi
GET (88, 72)-(103, 87), bluehi
GET (104, 72)-(119, 87), grehi

GET (72, 88)-(87, 103), redlite
GET (88, 88)-(103, 103), blulite
GET (104, 88)-(119, 103), grelite




'get objects...
idx = 0
FOR v = 24 TO 39
FOR h = 104 TO (238 / 2)
c = POINT(h, v)
idx = idx + 1
dude(idx) = c
NEXT h
idx = idx + 1
dude(idx) = 255
NEXT v

' Init...
FOR x = 1 TO 18
tile(x, 1) = 3
tile(x, 10) = 3
NEXT x
FOR y = 2 TO 9
tile(1, y) = 3
tile(18, y) = 3
FOR x = 2 TO 17
tile(x, y) = 1
NEXT x
NEXT y
mouserange 16, 8, 588, 183

IF guystx = 0 OR guysty = 0 THEN
guystx = 2
guysty = 9
absguyx = guystx * 16 - 8
absguyy = guysty * 16 + 8
END IF

changed = 0
drawscreen
toptiles ctiles
sstimeup! = TIMER + sstime!

statmsg SPACE$(7) + "^14[^01E^09s^01c^09a^01p^09e^01d^14] ^11Version ^48" + version$ + "^15", 3
rtimeup! = TIMER + INT(RND * 35) + 45
DO
a$ = UCASE$(INKEY$)
IF a$ = CHR$(27) AND noescape = 0 THEN EXIT DO
IF a$ = "Q" THEN EXIT DO

IF a$ = "M" THEN rmes = rmes XOR 1

' CDplayer...                  Might want to remove this.
IF a$ = CHR$(0) + "2" THEN
        ' pressed alt-m
        mousehide
        CLS
        SCREEN 0
        WIDTH 80
        SHELL "cdplay"
        SCREEN 13
        loadagif "epal.gif", 0
        drawscreen
        toptiles ctiles
        statmsg "^15[^11þ ^09No Escape^11 þ^15]", 2
        noescape = 1
        mouseshow
        sstimeup! = TIMER + sstime!
END IF

IF message = 1 AND (TIMER > sttimeup! OR a$ = "M") THEN
        ' erase the message at the bottom!
        mousehide
        LOCATE 24, 1: PRINT SPACE$(40);
        mouseshow
        message = 0
        noescape = 0
        newone = 0
        fillyes = 0
END IF

IF (rtimeup! < TIMER OR a$ = ".") AND rmes = 0 THEN
        IF message = 0 THEN rstatmsg
        rtimeup! = TIMER + INT(RND * 35) + 45 'random time for next one...
END IF

IF (sstimeup! < TIMER OR a$ = CHR$(0) + "X") AND (1 = 2) THEN   'DISABLED! =(
        'screensaver timeout
        mousehide
        DEF SEG = VARSEG(asmr$)
        thing% = SADD(asmr$)
        CALL Absolute(ax%, bx%, cx%, dx%, thing%)
        WIDTH 80
        DEF SEG
        SCREEN 13
        CLS
        loadagif "epal.gif", 0
        drawscreen
        toptiles ctiles
        mouseshow
        statmsg "^15[^11þ ^09No Escape^11 þ^15]", 2
        noescape = 1
        sstimeup! = TIMER + sstime!
END IF

IF a$ = CHR$(13) THEN
        drawscreen
        toptiles ctiles
        mouseshow
        statmsg "         ^15[^11þ ^09Screen Restored^11 þ^15]", 1
        sstimeup! = TIMER + sstime!
END IF



'LOCATE 1, 1: PRINT a$;
mousestatus lb, rb, xmouse, ymouse

IF tround(ooxmouse / 2) = tround(xmouse / 2) AND tround(ooymouse) = tround(ymouse) THEN
        ' still...
        IF ymouse < 24 THEN GOTO noway
        IF toremove = 0 THEN
                xtile = INT((xmouse - 16) / 32) + 1
                ytile = INT((ymouse - 8) / 16)
                SELECT CASE tile(xtile, ytile)
                CASE 10, 20
                ' a source tile.
                X1 = (xtag(xtile, ytile) - 1) * 16 + 8
                Y1 = ytag(xtile, ytile) * 16 + 8
                mousehide
                LINE (X1, Y1)-(X1 + 15, Y1 + 15), 15, B
                mouseshow
                txtile = X1
                tytile = Y1
                toremove = 1
                END SELECT
        END IF
noway:
ELSE
'IF tround(ooxmouse) <> tround(xmouse) OR tround(ooymouse) <> tround(ymouse) THEN
        IF toremove = 1 THEN
        xtile = INT((txtile - 8) / 16) + 1
        ytile = INT((tytile - 8) / 16)
        mousehide
        IF xtile > 0 AND ytile > 0 THEN tileput txtile, tytile, tile(xtile, ytile) ELSE toptiles ctiles
        mouseshow
        toremove = 0
END IF
END IF
IF ooxmouse <> xmouse OR ooymouse <> ymouse THEN
        ooxmouse = xmouse
        ooymouse = ymouse
        sstimeup! = TIMER + sstime!
        xdest = INT((xmouse - 16) / 32) + 1
        ydest = INT((ymouse - 8) / 16)
IF oxdest <> xdest OR oydest <> ydest THEN
        LOCATE 1, 10: PRINT xdest; " "; ydest;
        oxdest = xdest
        oydest = ydest
END IF
END IF


IF a$ <> "" THEN
        sstimeup! = TIMER + sstime!
END IF

IF a$ = "N" THEN
        IF newone = 0 THEN
            newone = 1
            statmsg "^15= ^57Press '^14N^57' again for a new level ^15=", 2
        ELSE
            FOR x = 1 TO 18
            tile(x, 1) = 3
            tile(x, 10) = 3
            NEXT x
            FOR y = 2 TO 9
            tile(1, y) = 3
            tile(18, y) = 3
            FOR x = 2 TO 17
            tile(x, y) = 1
            NEXT x
            NEXT y
            drawscreen
            savefile$ = ""
            title$ = ""
            changed = 0
            newone = 0
            sttimeup! = 0
        END IF
END IF

IF a$ = "E" THEN
        eraseold = ABS(eraseold - 1)
        SELECT CASE eraseold
        CASE 1
        statmsg "^15:^11Now erasing old during random^15:    ", 2
        CASE 0
        statmsg "^15:^11Now not erasing old during random^15:", 2
        END SELECT
END IF

IF a$ = "F" THEN
        IF fillyes = 0 THEN
        fillyes = 1
        statmsg "^15-^26-^55Press '^14F^55' again to fill with tile^26-^15-", 2
        ELSE
        SELECT CASE tile
        CASE 10, 20
        statmsg "^15þ ^55Not with panel or transporter!^15 þ", 2
        fillyes = 0
        CASE ELSE
        FOR y = 1 TO 10
        FOR x = 1 TO 18
        tile(x, y) = tile
        NEXT x
        NEXT y
        drawscreen
        changed = 1
        sttimeup! = 0
        END SELECT
        
END IF
END IF
IF a$ = "R" THEN
        IF newone = 0 THEN
            newone = 1
            statmsg "^15= ^57Press '^14R^57' again for random ^15= ^11(^90Type^15" + STR$(aitype) + "^11)", 2
            ELSE
            IF eraseold = 1 THEN
            FOR x = 1 TO 18
            tile(x, 1) = 3
            tile(x, 10) = 3
            NEXT x
           
            FOR y = 2 TO 9
            tile(1, y) = 3
            tile(18, y) = 3
            FOR x = 2 TO 17
            tile(x, y) = 1
            NEXT x
            NEXT y
            END IF
            randomspot aitype
           
            drawscreen
            savefile$ = ""
            changed = 1
                sttimeup! = TIMER + 3
                message = 1
        END IF
END IF

IF a$ <> "" THEN
ascc = ASC(a$)
IF ascc >= 48 AND ascc <= 57 THEN
        aitype = ascc - 48
        statmsg "         ^15[^68AI Type set to^11" + STR$(aitype) + "^68.^15]      ", 3
        END IF
END IF

IF a$ = "P" THEN
        'shell escape to try out the level!
        IF changed = 1 OR savefile$ = "" THEN
                osave$ = savefile$
                savefile$ = "~TEMP~"
                save savefile$
                savedtemp = 1
        ELSE
        savedtemp = 0
        END IF
        ' ok to run it, it's been saved.
        SCREEN 0
        shcmd$ = "escape /play " + savefile$
        WIDTH 80
        SHELL shcmd$
        WIDTH 80
        SCREEN 13
        CLS
        loadagif "epal.gif", 0
        drawscreen
        toptiles ctiles
        mouseshow
        sstimeup! = TIMER + sstime!
        IF savedtemp = 0 THEN
        statmsg "^15[^11þ ^09No Escape^11 þ^15]", 2
        ELSE
        statmsg "^15[^11þ ^09No Escape^11 þ^15]    ^09*^15Not Saved^09*", 2
        KILL "~TEMP~.ESC"
        savefile$ = osave$
        END IF
        noescape = 1
END IF


IF a$ = " " THEN
        IF ymouse < 25 THEN SOUND 200, .5: GOTO evermind
        putguy xmouse / 2, ymouse
        drawscreen
        changed = 1
        END IF
evermind:
IF a$ = "S" THEN
        'Save...
savee:      
        a$ = ""
        IF savefile$ = "" THEN GOTO saveas
        savefile$ = stripext$(savefile$)
        IF validf(savefile$) THEN
                save savefile$
                drawscreen
                toptiles ctiles
                changed = 0
        ELSE
        savefile$ = ""
        statmsg "^04Invalid filename.", 3
        LOCATE 1, 1: PRINT SPACE$(39);
        toptiles ctiles
        END IF
        END IF
IF a$ = "A" THEN
saveas:
        'Save As...
LOCATE 1, 1: PRINT SPACE$(39);
COLOR 3
LOCATE 1, 3: PRINT "Filename?"; : COLOR 11
savefile$ = getstr$(12, savefile$)
LOCATE 1, 1: PRINT SPACE$(39);
LOCATE 1, 1: COLOR 7
IF savefile$ <> "" THEN
        GOTO savee
ELSE
drawscreen
toptiles ctiles
END IF
        END IF

IF a$ = "T" THEN
        'title
        mousehide
        LOCATE 2, 1
        PRINT SPACE$(30)
        LOCATE 2, 1
        
        PRINT "Title:";
        title$ = getstr$(24, stripend$(title$))
        LOCATE 2, 1: PRINT SPACE$(30)
        title$ = padspace$(title$, 24)
        COLOR 11
        ptitle title$
        COLOR 15
        drawscreen
        toptiles ctiles
        mouseshow
        changed = 1
        sstimeup! = TIMER + sstime!
END IF

IF loadfirst = 1 THEN GOTO yeahb
IF a$ = "L" THEN
IF loadfirst = 1 THEN GOTO yeahb
        LOCATE 1, 1: PRINT SPACE$(39);
        COLOR 3
        LOCATE 1, 3: PRINT "Filename?"; : COLOR 11
        file$ = getstr$(12, "")
        LOCATE 1, 3: PRINT SPACE$(39);
        LOCATE 1, 3: COLOR 7
yeahb:
        file$ = addext$(file$, ".esc")
        loadfirst = 0
        PRINT "Reading "; file$; "...";
        ' **********************************
        ' strip off extension, truncate.....
        ' **********************************
ON ERROR GOTO loaderror
OPEN file$ FOR BINARY AS #1
ON ERROR GOTO 0
        IF LOF(1) = 0 THEN
                LOCATE 1, 1: PRINT SPACE$(39);
                statmsg "   ^04File was empty!", 3
                CLOSE
                ON ERROR GOTO loaderror
                KILL file$
                ON ERROR GOTO 0
                GOTO outahere
                END IF
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
COLOR 11: ptitle title$
COLOR 15

IF guystx = 0 OR guysty = 0 THEN
guystx = 2
guysty = 9
changed = 0
sstimeup! = TIMER + sstime!
END IF


'locate 1,3: PRINT guystx, guysty
absguyx = guystx * 16 - 8
absguyy = guysty * 16 + 8


        LOCATE 1, 1: PRINT SPACE$(39);
        statmsg "             ^11Done.", 1
        CLOSE
        savefile$ = file$
outahere:
        toptiles ctiles
        drawscreen
        COLOR 11: ptitle title$
END IF

IF lb = -1 OR (rb = -1 AND ymouse > 24) THEN
        changed = 1
        IF (rb = -1 AND ymouse > 24) THEN erasing = 1 ELSE erasing = 0
        IF ymouse < 24 THEN
dopicktile:
                DO WHILE lb = -1
                IF oxmouse <> xmouse OR oymouse <> ymouse THEN
                        mousestatus lb, rb, xmouse, ymouse
                        tile = ((ctiles * 17) - 16) + INT((xmouse + 16) / 32) - 2
                        SELECT CASE tile
                        CASE IS > lasttile
                        tile = lasttile
                        mousehide
                        tileput 8, 8, tile
                        mouseshow
                        CASE IS < 1
                        tile = 1
                        mousehide
                        tileput 8, 8, tile
                        mouseshow
                        CASE ELSE
                        tileput 8, 8, tile
                        END SELECT
                END IF
                GOTO nevermind
                LOOP
        END IF
       
        DO
        IF oxmouse <> xmouse OR oymouse <> ymouse THEN
                IF ymouse < 24 THEN GOTO dopicktile
                mousehide
                xtile = INT((xmouse - 16) / 32) + 1
                ytile = INT((ymouse - 8) / 16)
                IF ytile = 0 THEN ytile = 1
                IF ytile > 10 THEN ytile = 10
               
                IF erasing = 1 THEN pptile = 1 ELSE pptile = tile
                tileput (xmouse / 2), ymouse, pptile
                
                tile(xtile, ytile) = pptile
                SELECT CASE pptile
                CASE 10, 20
                        waitrelease
                        LOCATE 24, 1: PRINT SPACE$(40);
                        LOCATE 24, 1: COLOR 9: PRINT "Set destination.";
                        ' Set bounds...
                        mouserange 16, 24, 588, 183
                        COLOR 15
                        mouseshow
                        DO UNTIL lb = -1
                        mousestatus lb, rb, xmouse, ymouse
                        xdest = INT((xmouse - 16) / 32) + 1
                        ydest = INT((ymouse - 8) / 16)
                        LOCATE 1, 10: PRINT xdest; " "; ydest;
                        IF lb = -1 THEN EXIT DO
                        LOOP
                        LOCATE 24, 1: PRINT "                ";
                        mouserange 16, 8, 588, 183
                        ' pressed spot.         
                        COLOR 9
                        xtag(xtile, ytile) = xdest
                        ytag(xtile, ytile) = ydest
                CASE ELSE
                        xtag(xtile, ytile) = 0
                        ytag(xtile, ytile) = 0
                END SELECT
                oxmouse = xmouse
                oymouse = ymouse
                mouseshow
        END IF
        IF mtimeup! < TIMER THEN
                mousestatus lb, rb, xmouse, ymouse
                mtimeup! = TIMER + mtime!
        END IF
        LOOP WHILE (lb = -1 OR (rb = -1 AND ymouse > 24))
        'drawscreen
        mouseshow
        sstimeup! = TIMER + sstime!
        
END IF
IF (rb = -1 AND ymouse < 24) OR a$ = CHR$(0) + "P" THEN
        ctiles = ctiles + 1
        IF ctiles > mctiles THEN ctiles = 1
        toptiles ctiles
        waitrelease
        sstimeup! = TIMER + sstime!
END IF
IF a$ = CHR$(0) + "H" THEN
        ctiles = ctiles - 1
        IF ctiles < 1 THEN ctiles = mctiles
        toptiles ctiles
        sstimeup! = TIMER + sstime!
END IF

nevermind:

LOOP

CLOSE
mousehide
CLS
IF changed = 0 THEN GOTO ENDo
GOSUB loadcolors
textmap
ENDo:
SCREEN 0
WIDTH 80
CLS
PRINT "Thank you for using Escaped!"
PRINT : PRINT "Escaped "; version$; " is Copyright (c) 1996 Tom Murphy."
PRINT "All rights reserved."
COLOR 14: PRINT : PRINT "Have a nice day."
COLOR 7
END

fatalerror:
SCREEN 0
WIDTH 80
COLOR 7, 0
CLS
COLOR 15, 4
PRINT "          FATAL ERROR!!!       "; : COLOR 7, 0
PRINT : PRINT
PRINT "(invalid filename?)"
savef$ = "CRASH" + RIGHT$(STR$(INT(RND * 899) + 100), 3) + ".ESC"
PRINT : PRINT "Your work was saved in the file "; : COLOR 11
PRINT savef$; : COLOR 7: PRINT "."
fatalerr = 1
RESUME
END

loaderror:
SCREEN 0
WIDTH 80
COLOR 7, 0
CLS
COLOR 15, 4
PRINT "          FATAL ERROR!!!       "; : COLOR 7, 0
PRINT : PRINT
PRINT "(invalid filename?)"
PRINT : PRINT
END


loadmouse:
'DIM cpt(8)
 DEF SEG = &HA000:           
DIM SHARED mouse$: mouse$ = SPACE$(57)
FOR i% = 1 TO 57:  READ a$:  h$ = CHR$(VAL("&H" + a$))
MID$(mouse$, i%, 1) = h$: NEXT i%
'RESTORE
ms% = MouseInit%
IF NOT ms% THEN
  PRINT "Mouse not found.. and you need one."
  END
END IF
PRINT "Mouse found. Good."
RETURN

' Mouse data
DATA 55,89,E5,8B,5E,0C,8B,07,50,8B,5E,0A,8B,07,50,8B
DATA 5E,08,8B,0F,8B,5E,06,8B,17,5B,58,1E,07,CD,33,53
DATA 8B,5E,0C,89,07,58,8B,5E,0A,89,07,8B,5E,08,89,0F
DATA 8B,5E,06,89,17,5D,CA,08,00         


loadsave:
FOR i = 1 TO 94 ' load sub into string
READ a$
h$ = CHR$(VAL("&H" + a$))
MID$(asmr$, i, 1) = h$
NEXT i
RETURN
DATA 055,0B8,0D,00,0CD,010,0B8,00,0A0,08E,0C0,033,0FF,0B9,0D0
DATA 07,0A1,058,01,08B,01E,05A,01,08B,02E,05C,01,05,037,0A1
DATA 0D1,0C0,0D1,0C0,0A3,058,01,03,0D8,0D1,0CB,089,01E,05A,01
DATA 02B,0EB,033,0C5,089,02E,05C,01,0AB,08B,0C3,0AB,049,075,0D4
DATA 033,0C0,08E,0C0,026,0A1,01A,04,026,03B,06,01C,04,074,0BB
DATA 033,0C0,0CD,016,0B8,03,00,0CD,010,05D,0CA,08,00,08B,034
DATA 034,07F,0BF,032

'Number of bytes: 94

loadcolors:
FOR xxz = 1 TO lasttile
READ colors(xxz)
IF colors(xxz) = 0 THEN EXIT FOR
NEXT xxz
RETURN
'1-25
DATA 8,4,1,7,10,6,4,4,8,8,4,7,7,7,7,7,14,4,4,15,8,7,7,10,10
'26-41              x x xx
DATA 8,8,8,8,8,8,15,9,4,10,9,9,4,4,10,10,0

'721 lines, 22 subs 5.17.96
'742 lines, 27 subs 6.11.96

FUNCTION addext$ (inf$, ext$)
IF INSTR(inf$, ".") THEN  ELSE inf$ = inf$ + ext$
addext$ = inf$
END FUNCTION

SUB cPRINT (text$)

' prints with color codes denoted by ^XX where XX is a 2 digit color code
' ^^^ will yeild a '^'.
m = LEN(text$)

FOR mo = 1 TO m
cha$ = MID$(text$, mo, 1)
IF cha$ = "^" THEN
        ' escape code.
        cho$ = MID$(text$, mo + 1, 2)
        mo = mo + 2
        IF cho$ = "^^" THEN PRINT "^"; : GOTO noize
        c = VAL(cho$)
        IF c >= 0 AND c <= 99 THEN COLOR c
ELSE
PRINT cha$;
END IF
noize:
NEXT

END SUB

SUB drawscreen
mousehide
FOR y = 1 TO 10
FOR x = 1 TO 18
tileput (x * 16), (y * 16 + 8), tile(x, y)
NEXT x
NEXT y
putguy absguyx, absguyy
mouseshow
mouserange 16, 8, 588, 183
END SUB

FUNCTION getstr$ (maxlen, deflt$)

px1 = POS(0)
py1 = CSRLIN

LOCATE py1, px1 + 1: PRINT "Û";
gtmp2$ = ""
gtmp$ = ""
gtmp = 0
IF deflt$ <> "" THEN gtmp2$ = deflt$: gtmp = LEN(deflt$): LOCATE py1, px1 + 1: PRINT gtmp2$; "Û";


DO
DO
gtmp$ = INKEY$: IF gtmp$ <> "" THEN EXIT DO
LOOP
IF gtmp$ = CHR$(27) THEN getstr$ = deflt$: EXIT FUNCTION
IF gtmp$ = CHR$(13) THEN EXIT DO
IF LEFT$(gtmp$, 1) <> CHR$(0) THEN
IF gtmp$ = CHR$(8) THEN
        IF gtmp >= 1 THEN
                gtmp = gtmp - 1
                LOCATE py1, px1 + gtmp + 1
                PRINT "Û ";
                gtmp2$ = MID$(gtmp2$, 1, gtmp)
        ELSE
                SOUND 400, .1
        END IF
ELSE
        IF gtmp < maxlen THEN
                gtmp = gtmp + 1
                LOCATE py1, px1 + gtmp
                PRINT gtmp$; : IF gtmp <> maxlen THEN PRINT "Û";
                gtmp2$ = gtmp2$ + gtmp$
        ELSE
                SOUND 400, .1
        END IF
END IF
END IF
LOOP

getstr$ = gtmp2$

END FUNCTION

FUNCTION linstr (in$, init$)
FOR idx = 1 TO LEN(init$)
IF INSTR(in$, MID$(init$, idx, 1)) THEN
        linstr = 1
        EXIT FUNCTION
        END IF
NEXT
linstr = 0

END FUNCTION

DEFSNG Z
SUB loadagif (aloady$, adjust)
' this function displays a screen 13 gif.

aloady$ = aloady$ + CHR$(0)       'Must make "ASCIZ" filename.

'SCREEN 13               '320x200x256, 6 bits per primary.
ScrType = 1             'Select mode 13 pixel plotter.
ScrOffset = 0           'Screen offset is 0.
ScrWidth = 320          '320 bytes per scanline.
XRes = 319              'Screen size is (0,0)-(319,199)
YRes = 199
AdapterType = 1         'Set VGA palette.

''SCREEN 12               '640x480x16, 6 bits per primary.
'ScrType = 0             'Select 16 color mode pixel plotter.
'ScrOffset = 0           'Screen offset is 0.
'ScrWidth = 80           '80 bytes per scanline.
'XRes = 639              'Screen size is (0,0)-(639,479)
'YRes = 479
'AdapterType = 1         'Set VGA palette.



X0 = 0                  'View window covers entire screen.
Y0 = 0                  'Any points outside of this window will not
X1 = XRes               'be plotted.
Y1 = YRes
XOrg = 0                'Upper left hand corner of GIF goes at (0,0).
YOrg = adjust
PalIgnore = 0           'Don't ignore palette.

'Allocate the memory required by the LoadGIF function.
DIM GIFMem(LoadGIFMem * 8) AS INTEGER   '8 integers = 16 bytes = 1 page.

'For QuickBASIC 4.5.
Arror = LoadGIF(VARSEG(GIFMem(0)), VARSEG(aloady$), SADD(aloady$), ScrType, ScrOffset, ScrWidth, XRes, YRes, X0, Y0, X1, Y1, XOrg, YOrg, AdapterType, PalIgnore, 0, 0, PalColors)

'Deallocate memory required by the LoadGIF function.
ERASE GIFMem

IF Arror THEN              'check error variable
 SCREEN 0: WIDTH 80
 COLOR 4
 SELECT CASE ErrorReport
  CASE 0 TO 99: PRINT "Critical Error #:"; ErrorReport
  CASE IS >= 100: PRINT "DOS Error #:"; ErrorReport
  CASE -1: PRINT "End of file before GIF terminator."
  CASE -2: PRINT "LZW data stream is corrupted."
  CASE -3: PRINT "Not a GIF file."
  CASE -4: PRINT "Too many colors in GIF file for screen mode."
  CASE -5: PRINT "Bad image descriptor or GIF too big to be processed."
  CASE -6: PRINT "ScrType parameter is bad. Must be 0, 1 or 2."
  CASE -7: PRINT "AdapterType parameter is bad or EGA palette specified for a 256 color mode."
 END SELECT
COLOR 7: PRINT "(There has been some sort of error in the GIF loader)."
END
END IF

END SUB

DEFLNG A-Z
SUB MouseDriver (ax%, bx%, cx%, dx%)
  DEF SEG = VARSEG(mouse$)
  mouse% = SADD(mouse$)
  CALL Absolute(ax%, bx%, cx%, dx%, mouse%)
END SUB

SUB mousehide
 ax% = 2
 MouseDriver ax%, 0, 0, 0
END SUB

FUNCTION MouseInit%
  ax% = 0
  MouseDriver ax%, 0, 0, 0
  MouseInit% = ax%
END FUNCTION

DEFINT A-Z
SUB mouseput (x%, y%)
  ax% = 4
  cx% = x%
  dx% = y%
  MouseDriver ax%, 0, cx%, dx%
END SUB

SUB mouserange (X1%, Y1%, x2%, y2%)
  ax% = 7
  cx% = X1%
  dx% = x2%
  MouseDriver ax%, 0, cx%, dx%
  ax% = 8
  cx% = Y1%
  dx% = y2%
  MouseDriver ax%, 0, cx%, dx%
END SUB

DEFLNG A-Z
SUB mouseshow
  ax% = 1
  MouseDriver ax%, 0, 0, 0
END SUB

SUB mousestatus (lb%, rb%, xmouse%, ymouse%)
  ax% = 3
  MouseDriver ax%, bx%, cx%, dx%
  lb% = ((bx% AND 1) <> 0)
  rb% = ((bx% AND 2) <> 0)
  xmouse% = cx%
  ymouse% = dx%
END SUB

DEFINT A-Z
FUNCTION padspace$ (instring$, lent)
instring$ = LEFT$(instring$, lent)
a$ = SPACE$(lent)
FOR m = 1 TO LEN(instring$)
MID$(a$, m, 1) = MID$(instring$, m, 1)
NEXT
padspace$ = a$
END FUNCTION

SUB ptitle (stuff$)
stuff$ = LEFT$(stuff$, 25)
FOR mmm = 1 TO LEN(stuff$)
LOCATE mmm, 1
PRINT MID$(stuff$, mmm, 1);
NEXT mmm
END SUB

SUB putguy (startx, starty)
startx = INT((startx + 8) / 16) * 16 - 8
starty = INT((starty + 8) / 16) * 16 - 8
absguyx = startx
absguyy = starty
guystx = INT((startx - 8) / 16) + 1
guysty = INT((starty - 10) / 16) + 1
'LOCATE 5, 5: PRINT guystx, guysty
x = startx
y = starty
mousehide
FOR m = 1 TO 512
IF dude(m) = 255 THEN x = startx: y = y + 1 ELSE x = x + 1: IF dude(m) > 0 THEN PSET (x, y), dude(m)
NEXT m
mouseshow
END SUB

SUB randomspot (rtype)
' randomizes a level.
SELECT CASE rtype
CASE 1
' Type 1 - Almost all tiles. Totally random.
' 100% likelihood...
FOR y = 2 TO 9
FOR x = 2 TO 17
tilee = INT(RND * (lasttile - 1)) + 1
SELECT CASE tilee
CASE 20, 10
tilee = 1
END SELECT
tile(x, y) = tilee
' 35%
NEXT x
NEXT y
FOR y = 2 TO 9
FOR x = 2 TO 17
IF RND < .35 THEN
        tilee = INT(RND * (15)) + 1
        SELECT CASE tilee
        CASE 10, 6, 9
        tilee = 1
        CASE 12 TO 15
        tilee = 4
        END SELECT
        tile(x, y) = tilee
END IF
NEXT x
NEXT y
' 25%
FOR y = 2 TO 9
FOR x = 2 TO 17
IF RND < .35 THEN tile(x, y) = 1
NEXT x
NEXT y

CASE 2, 3, 4
' A maze of Blue. (Type 1)
' first fill it all with blue.

IF eraseold = 1 THEN
FOR y = 2 TO 9
FOR x = 2 TO 17
tile(x, y) = 3
NEXT x
NEXT y
END IF
' Now wander.
depth = 120
x = guystx
y = guysty
IF rtype = 4 THEN hi = 9 ELSE hi = 4
IF rtype = 3 THEN hi = 7
FOR u = 1 TO depth
m = INT(RND * hi) + 1
IF m > 4 THEN m = m - 4
IF m = 5 THEN IF INT(RND * 5) <> 3 THEN m = 3
'LOCATE 5, 5: PRINT m; : SLEEP
SELECT CASE m
CASE 1
dx = -1
dy = 0
CASE 2
dx = 0
dy = -1
CASE 3
dx = 1
dy = 0
CASE 4
dx = 0
dy = 1
CASE 5
rx = INT(RND * 14) + 2
ry = INT(RND * 7) + 2
dx = 0
dy = 0
tile(x, y) = 20
xtag(x, y) = rx
ytag(x, y) = ry
x = rx
y = ry
END SELECT
x = x + dx
y = y + dy
IF y > 9 THEN y = 9
IF y < 2 THEN y = 2
IF x > 17 THEN x = 17
IF x < 2 THEN x = 2
IF rtype > 2 AND INT(RND * 10) = 3 THEN tile(x, y) = 4 ELSE tile(x, y) = 1
NEXT u
END SELECT


tile(guystx, guysty) = 1

END SUB

SUB rstatmsg
a = INT(RND * 16) + 1
'a = a + 1: IF a > 10 THEN a = 1
SELECT CASE a
CASE 1
statmsg "^11Hint: ^23Don't forget to save! Press '^14S^23'.", 3
CASE 2
statmsg "^11Hint: ^23'^14T^23' titles the current level.", 3
CASE 3
statmsg "^11Hint: ^23'^14P^23' will test the current level!", 3
CASE 4
statmsg "^11Hint: ^23'^14Alt-1^23' loads the screen saver.", 3
CASE 5
statmsg "^11Hint: ^23You can load a level with ^23'^14L^23'.", 3
CASE 6
statmsg "^14[^01E^09s^01c^09a^01p^09e^01d^14] ^07Copyright (^11c^07) 1996 ^42Tom Murphy^07.", 3
CASE 7
statmsg "^11Hint: ^14SPACEBAR^23 places the starting spot.", 3
CASE 8
statmsg "^11Hint: ^23'^14M^23' shuts these messages off!", 3
CASE 9
statmsg "^11Hint: ^23'^14Q^23' or [^14ESC^23] quits!", 3
CASE 10
statmsg "^11Hint: ^140-9^23 change the random AI type!", 3
CASE 11
statmsg "^11Hint: ^23'^14R^23' makes a random level!", 3
CASE 11
statmsg "^11Hint: ^23'^14N^23' clears the level!", 3
CASE 12
statmsg "^11Hint: ^23'^14E^23' changes write-over mode!", 3
CASE 13
statmsg "^11Hint: ^23'^14F^23' fills with the current tile!", 3
CASE 14
statmsg "^11Hint: ^23[^14ENTER^23] redraws the screen!", 3
CASE ELSE
statmsg SPACE$(7) + "^14[^01E^09s^01c^09a^01p^09e^01d^14] ^11Version ^48" + version$ + "^15", 3
END SELECT
COLOR 15
END SUB

SUB save (file$)
IF file$ = "" THEN EXIT SUB

' **********************************
' strip off extension, truncate.....
' **********************************
file$ = addext$(file$, ".esc")
LOCATE 1, 3: PRINT "Writing "; file$; "...";
LOCATE 1, 3: PRINT SPACE$(35);
ON ERROR GOTO fatalerror
savef$ = file$
OPEN savef$ FOR BINARY AS #1
'ON ERROR GOTO 0
LOCATE 24, 1: PRINT "Writing tiles...  ";
FOR y = 1 TO 10
FOR x = 1 TO 18
wa$ = CHR$(tile(x, y))
PUT 1, , wa$
NEXT: NEXT
LOCATE 24, 1: PRINT "Writing X tags...    ";
FOR y = 1 TO 10
FOR x = 1 TO 18
wa$ = CHR$(xtag(x, y))
PUT 1, , wa$
NEXT: NEXT
LOCATE 24, 1: PRINT "Writing Y tags...";
FOR y = 1 TO 10
FOR x = 1 TO 18
wa$ = CHR$(ytag(x, y))
PUT 1, , wa$
NEXT: NEXT
LOCATE 24, 1: PRINT "Writing Start positions...";
wa$ = CHR$(guystx)
PUT 1, , wa$
wa$ = CHR$(guysty)
PUT 1, , wa$
LOCATE 24, 1: PRINT "Writing title...          ";
title$ = padspace$(title$, 25)
PUT 1, , title$
'LOCATE 24, 1: PRINT "                   ";
LOCATE 24, 1: PRINT SPACE$(39);
LOCATE 24, 1: COLOR 9
PRINT "Done.";
CLOSE
IF fatalerr = 1 THEN END
ON ERROR GOTO 0
END SUB

SUB statmsg (text$, secs)
LOCATE 24, 1
mousehide
PRINT SPACE$(39);
LOCATE 24, 1
cPRINT text$
mouseshow
sttimeup! = TIMER + secs
message = 1
END SUB

FUNCTION stripend$ (in$)
FOR index = LEN(in$) TO 1 STEP -1
IF MID$(in$, index, 1) <> " " THEN EXIT FOR
NEXT
stripend$ = LEFT$(in$, index)
END FUNCTION

FUNCTION stripext$ (in$)
FOR index = 1 TO LEN(in$)
IF MID$(in$, index, 1) = "." THEN EXIT FOR
NEXT
stripext$ = LEFT$(in$, index - 1)
END FUNCTION

SUB textmap

COLOR 11
LOCATE 3, 10: PRINT title$

charz$ = "°þþþþX þÎoS" + CHR$(26) + CHR$(27) + CHR$(24) + CHR$(25)
charz$ = charz$ + "±°øùè²01³ÀÙÚ¿Äíééé²²²°°°"

FOR y = 1 TO 10
FOR x = 1 TO 18
t = tile(x, y)

COLOR colors(t)

u$ = MID$(charz$, t, 1)

LOCATE y + 5, x + 10: PRINT u$;
NEXT x
NEXT y
LOCATE 23, 1: COLOR 11: PRINT "Write this level? (Y/n)";
a$ = UCASE$(INPUT$(1))
IF a$ = "N" OR a$ = CHR$(27) THEN
EXIT SUB
ELSE
LOCATE 23, 1: PRINT SPACE$(39);
COLOR 3

PRINT "Filename?"; : COLOR 11: file$ = getstr$(12, savefile$)
LOCATE 23, 1: PRINT SPACE$(39);
LOCATE 23, 1: COLOR 7
save file$
END IF
END SUB

SUB tileput (xt%, yt%, tilep%)
'round x,y:

xt% = INT((xt% + 8) / 16) * 16 - 8
yt% = INT((yt% + 8) / 16) * 16 - 8



SELECT CASE tilep%
CASE 1
PUT (xt%, yt%), floor1, PSET
CASE 2
PUT (xt%, yt%), redblock, PSET
CASE 3
PUT (xt%, yt%), blublock, PSET
CASE 4
PUT (xt%, yt%), grablock, PSET
CASE 5
PUT (xt%, yt%), greblock, PSET
CASE 6
PUT (xt%, yt%), exitdoor, PSET
CASE 7
PUT (xt%, yt%), hole, PSET
CASE 8
PUT (xt%, yt%), gldblock, PSET
CASE 9
PUT (xt%, yt%), shooter, PSET
CASE 10
PUT (xt%, yt%), steptile, PSET
CASE 11
PUT (xt%, yt%), doorup, PSET
CASE 12
PUT (xt%, yt%), arrowright, PSET
CASE 13
PUT (xt%, yt%), arrowleft, PSET
CASE 14
PUT (xt%, yt%), arrowup, PSET
CASE 15
PUT (xt%, yt%), arrowdown, PSET
CASE 16
PUT (xt%, yt%), rough, PSET
CASE 17
PUT (xt%, yt%), electric, PSET
CASE 18
PUT (xt%, yt%), elecon, PSET
CASE 19
PUT (xt%, yt%), elecoff, PSET
CASE 20
PUT (xt%, yt%), transp, PSET
CASE 21
PUT (xt%, yt%), break, PSET
CASE 22
PUT (xt%, yt%), horiz, PSET
CASE 23
PUT (xt%, yt%), vert, PSET
CASE 24
PUT (xt%, yt%), but0, PSET
CASE 25
PUT (xt%, yt%), but1, PSET
CASE 26
PUT (xt%, yt%), wireud, PSET
CASE 27
PUT (xt%, yt%), wireur, PSET
CASE 28
PUT (xt%, yt%), wireul, PSET
CASE 29
PUT (xt%, yt%), wiredr, PSET
CASE 30
PUT (xt%, yt%), wiredl, PSET
CASE 31
PUT (xt%, yt%), wirelr, PSET
CASE 32
PUT (xt%, yt%), wireon, PSET
CASE 33
PUT (xt%, yt%), blulite, PSET
CASE 34
PUT (xt%, yt%), redlite, PSET
CASE 35
PUT (xt%, yt%), grelite, PSET
CASE 36
PUT (xt%, yt%), bluehi, PSET
CASE 37
PUT (xt%, yt%), bluelo, PSET
CASE 38
PUT (xt%, yt%), redhi, PSET
CASE 39
PUT (xt%, yt%), redlo, PSET
CASE 40
PUT (xt%, yt%), grehi, PSET
CASE 41
PUT (xt%, yt%), grelo, PSET
CASE ELSE

LOCATE 23, 1: PRINT "Undefined Tile Error! #"; tilep%
END SELECT
END SUB

SUB toptiles (ct)
mousehide
tileput 8, 8, tile
starttile = (ct * 17) - 16
endtile = (ct * 17)
IF endtile > lasttile THEN endtile = lasttile
        FOR t = starttile TO endtile
        u = u + 1
        tileput u * 16 + 16, 15, t
        NEXT t
' black out the rest...
zxt% = (u + 1) * 16 + 16
zyt% = 15
zxt% = INT((zxt% + 8) / 16) * 16 - 8
zyt% = INT((zyt% + 8) / 16) * 16 - 8
LINE (zxt%, zyt%)-(296, 23), 0, BF
mouseshow

END SUB

FUNCTION tround% (in%)
tround% = INT((in% + 8) / 16) * 16 - 8
END FUNCTION

FUNCTION validf (fil$)
IF fil$ = "" THEN EXIT FUNCTION
z$ = UCASE$(fil$)
FOR u = 1 TO LEN(z$)
IF INSTR("ABCDEFGHIJKLMNOPQRSTUVWXYZ-1234567890()~", MID$(z$, u, 1)) THEN  ELSE EXIT FUNCTION
NEXT
SELECT CASE z$
CASE "CON", "PRN", "COM1", "COM2", "COM3", "COM4", "LPT1", "LPT2", "NUL"
validf = 0: EXIT FUNCTION
END SELECT
validf = 1
END FUNCTION

SUB waitrelease
mousestatus lb, rb, xmouse, ymouse
mx = xmouse
my = ymouse
DO WHILE (lb + rb) <> 0
mousestatus lb, rb, xmouse, ymouse
LOOP
mouseput mx, my

END SUB

