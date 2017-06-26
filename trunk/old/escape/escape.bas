DEFINT A-Z
'                           Tom Murphy's
'          ±±±±°°   ±±±°°   ±±±±°°   ±±±±±±°  ±±±±±±°   ±±±±°°
'          ±°       ±°      ±°       ±°   ±°  ±°   ±°   ±°
'          ±±°°     ±±±±°   ±°       ±±±±±±°  ±±±±±±°   ±±°°
'          ±°          ±°   ±°       ±°   ±°  ±°        ±°
'          ±°          ±°   ±°       ±°   ±°  ±°        ±°
'          ±±±±°°   ±±±±°   ±±±±°°   ±°   ±°  ±°        ±±±±°°
                       CONST version$ = "1.94á"

CONST left = 1
CONST right = 2
CONST up = 3
CONST down = 4
CONST startover = 5
CONST win = 6
CONST cancel = 7

CONST hiplayer = 5

CONST campaign = 1
CONST level = 2

DECLARE FUNCTION MouseInit% ()
DECLARE FUNCTION checkmove (direction)
DECLARE FUNCTION getcpu2% ()
DECLARE FUNCTION twoc$ (in$)
DECLARE FUNCTION validf (filename$)
DECLARE FUNCTION getstr$ (llen, default$)
DECLARE SUB bkill (file$)
DECLARE SUB oSound (freq%, dura!)
DECLARE SUB loadagif (aloady$, adjust%)
DECLARE SUB MouseDriver (ax%, bx%, cx%, dx%)
DECLARE SUB mousestatus (lb%, rb%, xmouse%, ymouse%)
DECLARE SUB mousehide ()
DECLARE SUB mouseshow ()
DECLARE SUB tileput (x%, y%, tile%)
DECLARE SUB waitrelease ()
DECLARE SUB drawscreen ()
DECLARE SUB mouserange (x%, y%, x2%, y2%)
DECLARE SUB putguy (x, y)
DECLARE SUB moveguy (direction)
DECLARE SUB drawvlaser (X1, Y1, x2, y2)
DECLARE SUB drawhlaser (X1, Y1, x2, y2)
DECLARE SUB died ()
DECLARE SUB lasernoise ()
DECLARE SUB load (where$, what, which)
DECLARE SUB entprompt (colorv)
DECLARE SUB writemove (what)
DECLARE SUB loadinfo (idstring$)
DECLARE SUB savebeat (ids$, cdoing)
DECLARE SUB swaptiles (source, dest)
DECLARE SUB cPRINT (inpu$)            ' <----- Not being used, currently

COMMON SHARED x, y, lb, rb, xmouse, ymouse
COMMON SHARED floor1(), redblock(), blublock(), grablock(), greblock(), exitdoor()
COMMON SHARED shooter(), doorup(), steptile(), gldblock(), arrowup(), arrowdown()
COMMON SHARED arrowright(), arrowleft(), lasttile, trnsport
COMMON SHARED rough(), electric(), elecon(), elecoff(), break()
COMMON SHARED transp(), hole(), dude(), horiz(), vert(), but0(), but1()
COMMON SHARED facing, guyx, guyy, guystx, guysty, tries()
COMMON SHARED tile(), xtag(), ytag(), ztag(), otag()
COMMON SHARED beatlevel, guydied, exitcode, title$, recfile$, recording
COMMON SHARED macro, testdie, movehim, oface, player, levsbeat, beat()
COMMON SHARED wireud(), wireur(), wireul(), wiredr(), wiredl(), wirelr()
COMMON SHARED wireon(), blulite(), redlite(), grelite(), soundon
COMMON SHARED bluehi(), bluelo(), redhi(), redlo(), grehi(), grelo()


'$INCLUDE: 'loadgif.bi'
DIM tile(0 TO 19, 0 TO 11)
'DIM object(0 TO 19, 0 TO 11)
DIM xtag(1 TO 18, 1 TO 11)
DIM ytag(1 TO 18, 1 TO 11)
DIM ztag(1 TO 18, 1 TO 11) ' # of tile that used to be there.
DIM otag(1 TO 18, 1 TO 11) ' Other tag... steptile under a block?
DIM beat(1 TO 50)
DIM tries(1 TO 50)
DIM titlez$(1 TO 50)
DIM usersf$(1 TO 10)

' Set up initial values
facing = 2
player = 1
soundon = 1

' Init the edges
FOR temp = 0 TO 19
  tile(temp, 0) = -1
  tile(temp, 11) = -1
    IF temp <= 11 THEN
        tile(0, temp) = -1
        tile(19, temp) = -1
    END IF
NEXT temp

CLOSE
COLOR 15
PRINT

' Command line splicer...
' check if we're doing a escaped shell

MM$ = COMMAND$

file$ = MM$
IF file$ <> "" THEN comline = 1
IF UCASE$(LEFT$(MM$, 6)) = "/PLAY " THEN
        escapedon = 1
        file$ = RIGHT$(MM$, LEN(MM$) - 6)
END IF


IF escapedon = 0 THEN
FOR x = 1 TO 10
COLOR 11: PRINT "þ"; : COLOR 9: PRINT CHR$(4);
NEXT x
COLOR 14: PRINT " Escape "; : COLOR 15
FOR x = 1 TO 10
COLOR 9: PRINT CHR$(4); : COLOR 11: PRINT "þ";
NEXT x
COLOR 7
cpu = getcpu2%
PRINT : PRINT "CPU is an"; : COLOR 14: PRINT " 80"; RIGHT$(STR$(cpu), 3); : COLOR 7: PRINT " or higher."
END IF

'DIM x, y, lb, rb, xmouse, ymouse AS INTEGER

' check if the user file exits. if not, make a new one!

OPEN "users.xdt" FOR BINARY AS #1
a$ = CHR$(42) + CHR$(255) + "USERS" + CHR$(0) + "Nobody" + CHR$(0)
PUT 1, , a$
OPEN "nobody.xsr" FOR BINARY AS #8
a$ = CHR$(0) + CHR$(42) + CHR$(255) + "USERX" + CHR$(0) + CHR$(1) + CHR$(0)
PUT 8, , a$
CLOSE

' User selection...

OPEN "users.xdt" FOR BINARY AS #1
seluse:
cp = 0
CLS
IF escapedon = 0 THEN
LOCATE 1, 13
FOR x = 1 TO 10
COLOR 11: PRINT "þ"; : COLOR 9: PRINT CHR$(4);
NEXT x
COLOR 14: PRINT " Escape "; : COLOR 15
FOR x = 1 TO 10
COLOR 9: PRINT CHR$(4); : COLOR 11: PRINT "þ";
NEXT x
COLOR 8
LOCATE 2, 30
PRINT "version "; version$
END IF
' get users...
test1$ = CHR$(42) + CHR$(255) + "USERS" + CHR$(0)
test2$ = test1$
' valid?
GET 1, 1, test2$
IF test2$ <> test1$ THEN
        COLOR 4
        PRINT "There has been an error."
        COLOR 7
        PRINT : PRINT "The "; : COLOR 11: PRINT "USERS.XDT"; : COLOR 7: PRINT " file is invalid."
        END
END IF
' ok, it's ok.
uu$ = ""
in$ = " "

DO
GET 1, , in$
IF in$ = CHR$(0) THEN
       ' end the current string
       IF uu$ = "" THEN EXIT DO
       cp = cp + 1
       usersf$(cp) = uu$
       uu$ = ""
       IF cp = 10 THEN EXIT DO
ELSE
       ' more on current string
       uu$ = uu$ + in$
END IF
LOOP

hicp = cp


IF file$ <> "" THEN userx = 1: GOTO gotuser

LOCATE 4, 15: COLOR 15: PRINT "Please select a user:";
'COLOR 8: LOCATE 21, 1: PRINT "(The following two just barely work:)";
COLOR 7
IF hicp < 10 THEN LOCATE 22, 1: PRINT "To make a new user, press "; : COLOR 15: PRINT "N"; : COLOR 7: PRINT ".";
IF hicp > 1 THEN LOCATE 23, 1: PRINT "To delete a user, select them and press "; : COLOR 15: PRINT "D"; : COLOR 7: PRINT ".";
FOR xu = 1 TO hicp
LOCATE xu + 5, 7
COLOR 14: PRINT STR$(xu); : COLOR 7: PRINT ": "; usersf$(xu)
NEXT xu

nolasc = hicp + 48
oxxw = 2

xxw = 1

COLOR 7, 0
DO
nono:
a$ = UCASE$(INKEY$)

IF a$ = CHR$(0) + "H" THEN 'up
        xxw = xxw - 1
        IF xxw < 1 THEN xxw = 1
END IF
IF a$ = CHR$(0) + "P" THEN 'down
        xxw = xxw + 1
        IF xxw > hicp THEN xxw = hicp
END IF
IF oxxw <> xxw THEN
        LOCATE oxxw + 5, 11
        COLOR 7, 0
        PRINT usersf$(oxxw);
        LOCATE xxw + 5, 11
        COLOR 15, 1
        PRINT usersf$(xxw);
        oxxw = xxw
END IF
IF a$ = CHR$(13) THEN
        userx = xxw
        EXIT DO
END IF
IF a$ <> "" THEN
        IF ASC(a$) > 48 AND ASC(a$) <= nolasc THEN
        userx = ASC(a$) - 48
        EXIT DO
END IF
END IF
IF a$ = "D" AND hicp > 1 THEN
        COLOR 7
        COLOR 7, 0
        ' delete one
        userd = xxw
        IF userd = 1 THEN LOCATE 12, 1: PRINT "Can't delete 'Nobody'.": GOTO nono
GET 1, 8, in$
start$ = ""
cp = 0
DO
GET 1, , in$
start$ = start$ + in$
IF in$ = CHR$(0) THEN
       IF cp = userd - 2 THEN EXIT DO
       cp = cp + 1
END IF
LOOP
' advanced to beginning of string we wanted.
dels$ = ""
DO
GET 1, , in$
IF in$ = CHR$(0) THEN EXIT DO
dels$ = dels$ + in$
LOOP
tempo$ = dels$ + ".xsr"
' passed the one to delete.
'PRINT "located."
source$ = ""
DO
GET 1, , in$
IF EOF(1) THEN EXIT DO ELSE source$ = source$ + in$
LOOP
CLOSE 1

bkill "users.xdt"
OPEN "users.xdt" FOR BINARY AS #1

' now put the new file.

toput$ = CHR$(42) + CHR$(255) + "USERS" + CHR$(0) + start$ + source$
PUT 1, 1, toput$
bkill dels$ + ".xsr"
GOTO seluse
END IF

IF a$ = "N" THEN
        ' new user.
        IF hicp = 10 THEN GOTO nono
        COLOR 7, 0
        CLS
        ' ********************************
reenternu:
        COLOR 7
        LOCATE 4, 1
        PRINT "New user's filename? (8 characters max)?";
        COLOR 11: newf$ = getstr$(8, "")
        IF LEN(newf$) > 8 THEN newf$ = LEFT$(newf$, 8)
        IF newf$ = "" THEN GOTO seluse
        ' bwahaha!!
        ' add 'im (I ought to check if the filename's valid, or it'll die every time. ;)
        ' HEY TOM!!! LOOK HERE!!!!!!!!!!
        IF validf(newf$) = 0 THEN GOTO reenternu
        filezz$ = newf$ + ".XSR"
        OPEN filezz$ FOR BINARY AS #8
        a$ = CHR$(0) + CHR$(42) + CHR$(255) + "USERX" + CHR$(0) + CHR$(1) + CHR$(0)
        PUT 8, , a$
        CLOSE 8
        newf$ = newf$ + CHR$(0)
        PUT 1, LOF(1) + 1, newf$
        GOTO seluse
END IF


IF a$ = CHR$(27) THEN GOTO endx
LOOP

gotuser:

' otay...
COLOR 7, 0
CLOSE
' so now open it...
ufile$ = usersf$(userx) + ".XSR"
ON ERROR GOTO userfatal
OPEN ufile$ FOR BINARY AS #33
ON ERROR GOTO 0
' is it right?
test1$ = CHR$(0) + "*" + CHR$(255) + "USERX" + CHR$(0)
test2$ = test1$
GET 33, 1, test2$
IF test2$ <> test1$ THEN
        COLOR 4
        PRINT : PRINT "There has been an error."
        COLOR 7
        PRINT : PRINT "The "; : COLOR 11: PRINT ufile$; : COLOR 7: PRINT " file was invalid."
        END
END IF

GET 33, 10, in$
uchar = ASC(in$)
GET 33, 11, in$
usercamps = ASC(in$)

' -------------
mtime! = .1
' -------------


SCREEN 13
loadagif "escape.gif", 0
'mouseshow

tile = 1
    'Array           '# 'Tag 'Sp 'Transp
DIM floor1(129)      '1             X
DIM redblock(129)    '2       X  
DIM blublock(129)    '3
DIM grablock(129)    '4       X
DIM greblock(129)    '5       X
DIM exitdoor(129)    '6       X
DIM hole(129)        '7       X     X-
DIM gldblock(129)    '8       X
DIM shooter(129)     '9   D   X
DIM steptile(129)    '10  S   X     X
DIM doorup(129)      '11  D   X
DIM arrowright(129)  '12
DIM arrowleft(129)   '13
DIM arrowup(129)     '14
DIM arrowdown(129)   '15
DIM rough(129)       '16            X
DIM electric(129)    '17      ?     X-
DIM elecon(129)      '18      X
DIM elecoff(129)     '19   
DIM transp(129)      '20  S   X     X-
DIM break(129)       '21      X
DIM horiz(129)       '22      X
DIM vert(129)        '23      X
DIM but0(129)        '24      X
DIM but1(129)        '25      X

DIM wireud(129)      '26      X
DIM wireur(129)      '27      X
DIM wireul(129)      '28      X
DIM wiredr(129)      '29      X
DIM wiredl(129)      '30      X
DIM wirelr(129)      '31      X
DIM wireon(129)      '32      X
DIM blulite(129)     '33
DIM redlite(129)     '34
DIM grelite(129)     '35
DIM bluehi(129)      '36
DIM bluelo(129)      '37            X
DIM redhi(129)       '38
DIM redlo(129)       '39            X
DIM grehi(129)       '40
DIM grelo(129)       '41            X

lasttile = 41

DIM dude(512, 1 TO 4, 1 TO hiplayer)  'o1

DIM earon(211)      ' Sound ON
DIM earoff(211)     ' Sound OFF
DIM earswap(211)    ' Space for swap

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

GET (120, 56)-(140, 75), earon
GET (120, 76)-(140, 95), earoff

'get objects...

FOR pers = 1 TO hiplayer
FOR face = 1 TO 4
idx = 0
FOR v = (40 + (pers - 1) * 16) TO (55 + (pers - 1) * 16)
FOR h = (152 + (face - 1) * 16) TO (167 + (face - 1) * 16)
c = POINT(h, v)
idx = idx + 1
dude(idx, face, pers) = c
NEXT h
idx = idx + 1
dude(idx, face, pers) = 255
NEXT v
NEXT face
NEXT pers

IF uchar < 1 THEN uchar = 1
IF uchar > hiplayer THEN uchar = hiplayer

player = uchar

IF file$ <> "" THEN GOTO newgame

menu:
CLS
COLOR 11
loadagif "esctitle.gif", 0

' 214, 97
putguy 13, 9
oface = facing
DO
a$ = UCASE$(INKEY$)
IF a$ = "1" THEN file$ = "": GOTO campaign
IF a$ = "2" THEN file$ = "": GOTO newgame
IF a$ = "3" THEN
        player = player + 1
        IF player > hiplayer THEN player = 1
        LINE (200, 152)-(215, 168), 0, BF
        putguy 13, 9
        END IF
IF a$ = CHR$(0) + "H" THEN facing = 1
IF a$ = CHR$(0) + "P" THEN facing = 2
IF a$ = CHR$(0) + "K" THEN facing = 3
IF a$ = CHR$(0) + "M" THEN facing = 4
IF oface <> facing THEN
        LINE (200, 152)-(215, 168), 0, BF
        putguy 13, 9
        oface = facing
END IF
IF a$ = CHR$(27) THEN GOTO endo
LOOP

campaign:
' load a campaign.
DO
COLOR 7
LOCATE 1, 1: PRINT "Campaign filename?"; : COLOR 15
campf$ = getstr$(8, "")
IF campf$ = "" THEN GOTO menu
LOOP WHILE validf(campf$) = 0
IF INSTR(campf$, ".") THEN  ELSE campf$ = campf$ + ".XCP"
CLOSE 2
OPEN campf$ FOR BINARY AS #2
IF LOF(2) = 0 THEN
        CLS
        CLOSE 2
        bkill campf$
        LOCATE 3, 3: COLOR 4: PRINT "That file does not exist."
        GOTO campaign
END IF
checkx$ = "ESCTOESX" + CHR$(0)
checkxx$ = checkx$
GET 2, 1, checkxx$
IF checkxx$ <> checkx$ THEN
        CLS
        PRINT "Invalid campaign file."
        END
END IF
resetcamp:
'reset the campaign info
FOR wx = 1 TO 50
beat(wx) = 0
NEXT wx
cpage = 0
'oxxw = 0
xxw = 1
camp:
oxxw = 0
CLS
' get title...
ctitle$ = SPACE$(30)
GET 2, 10, ctitle$
LOCATE 1, 3: COLOR 11: PRINT ctitle$
in1$ = " "
ids$ = SPACE$(10)
GET 2, 41, in1$
nolevs = ASC(in1$)
'PRINT nolevs
GET 2, 42, ids$
'PRINT ids$

'load user's info for this level.
loadinfo ids$

pages = INT(nolevs / 10)

LOCATE 24, 1
COLOR 7: PRINT "Use "; : COLOR 15: PRINT ""; : COLOR 7: PRINT ", "; : COLOR 15: PRINT "";
COLOR 7: PRINT ", ";
LOCATE 24, 21: COLOR 7: PRINT " to select.";

clear$ = SPACE$(30)
showpage:
LOCATE 24, 11
COLOR 8 + (ABS((cpage > 0) * 7)): PRINT "PgUp"; : COLOR 7: PRINT ", ";
COLOR 8 + (ABS((cpage <> pages) * 7)): PRINT "PgDn";

FOR uu = 3 TO 13
LOCATE uu, 2: PRINT clear$;
NEXT uu

hilevs = (cpage * 10) + 10
IF hilevs > nolevs THEN hilevs = nolevs

levsbeat = 0
FOR xzs = (cpage * 10) + 1 TO hilevs
IF beat(xzs) = 1 THEN
        levsbeat = levsbeat + 1
        LOCATE xzs - (cpage * 10) + 3, 2
        COLOR 9
        PRINT "*";
END IF

LOCATE (xzs - (cpage * 10)) + 3, 3
COLOR 14: PRINT twoc$(STR$(xzs)); : COLOR 8: PRINT ". ";
title$ = SPACE$(22)

'LOCATE 10, 10: PRINT xzs: SLEEP
target! = (xzs * 567) + 31
GET 2, target!, title$
titlez$(xzs) = title$
PRINT title$
NEXT xzs
'IF levsbeat = nolevs THEN LOCATE 1, 1: COLOR 4: PRINT "*";
COLOR 8
' which one would they like?
protime! = .5
nolasc = nolevs + 48
DO

a$ = INKEY$
IF a$ = CHR$(0) + "Q" THEN
        ' PgDn
        IF cpage < pages THEN
                cpage = cpage + 1
                oxxw = 0
                IF xxw > nolevs - (cpage * 10) THEN xxw = nolevs - (cpage * 10)
                GOTO showpage
        END IF
        xxw = hilevs - (INT(hilevs / 10) * 10)
END IF
IF a$ = CHR$(0) + "I" THEN
        ' PgUp
        IF cpage > 0 THEN
                cpage = cpage - 1
                oxxw = 0
                GOTO showpage
        END IF
        xxw = 1
END IF
IF a$ = CHR$(0) + "H" THEN 'up
        xxw = xxw - 1
        IF xxw < 1 THEN
                IF cpage > 0 THEN
                cpage = cpage - 1
                xxw = 10
                oxxw = 0
                GOTO showpage
                END IF
                xxw = 1
        END IF
        LOCATE 20, 7: PRINT "";
END IF
IF a$ = CHR$(0) + "P" THEN 'down
        xxw = xxw + 1
        IF (xxw + cpage * 10) > hilevs THEN
                IF pages > cpage THEN
                        cpage = cpage + 1
                        xxw = 1
                        oxxw = 0
                        GOTO showpage
                        END IF
                xxw = hilevs - (INT(hilevs / 10) * 10)
        END IF
        LOCATE 20, 7: PRINT "";
END IF
IF oxxw <> xxw THEN
        LOCATE oxxw + 3, 8
        COLOR 8
        IF oxxw > 0 THEN PRINT titlez$(cpage * 10 + oxxw);
        LOCATE xxw + 3, 8
        COLOR 15
        PRINT titlez$(cpage * 10 + xxw);
        oxxw = xxw
END IF
IF a$ = CHR$(13) THEN
        cdoing = (cpage * 10) + xxw
        GOTO rstcmp
END IF

IF a$ <> "" THEN IF ASC(a$) > 48 AND ASC(a$) <= nolasc THEN EXIT DO
IF a$ = CHR$(27) THEN GOTO menu
IF promptime! < TIMER THEN
        promptog = ABS(promptog - 1)
        LOCATE 20, 6
        SELECT CASE promptog
        CASE 1
        PRINT "þ þ"
        CASE 0
        PRINT " "
        END SELECT
        promptime! = TIMER + protime!
END IF
LOOP

' otay...

cdoing = ASC(a$) - 48
rstcmp:
LOCATE 24, 1: PRINT SPACE$(38);
IF cdoing > hilevs THEN GOTO showpage
load campf$, campaign, cdoing
IF exitcode = 42 THEN GOTO endo
camping = 1
beatlevel = 0
guydied = 0
GOTO showlev


END

newgame:
CLS
camping = 0
newgame2:
LOCATE 1, 1: PRINT SPACE$(39);
COLOR 3
IF file$ = "" THEN
DO
COLOR 7
LOCATE 1, 1: PRINT "Filename?"; : COLOR 15
file$ = getstr$(8, "")
IF file$ = "" THEN GOTO menu
LOOP WHILE validf(file$) = 0
comline = 0
END IF

'IF file$ = "" THEN LOCATE 1, 1: PRINT "Filename?";
'file$ = getstr$(8, file$)
IF INSTR(file$, ".") THEN  ELSE file$ = file$ + ".esc"
LOCATE 2, 1: PRINT "                 ";
LOCATE 1, 1: PRINT SPACE$(39);
LOCATE 1, 1: COLOR 7
PRINT "Reading "; file$; "...";
' **********************************
' strip off extension, truncate.....
' **********************************
filex$ = file$
IF filex$ = ".esc" THEN GOTO endo
restart:
beatlevel = 0
guydied = 0
exitcode = 0
load filex$, level, 1
IF exitcode = 42 THEN
IF comline = 1 THEN GOTO endo ELSE file$ = "": GOTO newgame2
END IF
comline = 0
showlev:
LOCATE 2, 1: COLOR 11: PRINT title$
COLOR 15
guyx = guystx
guyy = guysty
'absguyx = guystx * 16 - 8
'absguyy = guysty * 16 + 8

' null out the tile arrays...
FOR y = 1 TO 10
FOR x = 1 TO 18
otag(x, y) = 0
ztag(x, y) = 1
NEXT x
NEXT y

LOCATE 1, 1: PRINT SPACE$(39);

'mouserange 16, 24, 588, 183

drawscreen

' temporary:
'object(2, 2) = 1' guy
'guyx = 2
'guyy = 9
'IF guyx = 0 THEN guyx = 2: guyy = 9
putguy guyx, guyy

DO
rekey:
a$ = UCASE$(INKEY$)

IF macro = 1 THEN
IF guydied = 1 THEN
        writemove startover
        macro = 0
        LOCATE 24, 27
        PRINT "   ";
        CLOSE 42
        entprompt 200
        IF camping = 1 THEN GOTO rstcmp ELSE GOTO restart
END IF
      
        ' running a macro.
        IF a$ <> "" AND stepr = 0 THEN
                ' cancel macro
                macro = 0
                LOCATE 24, 27
                PRINT "   ";
                CLOSE 42
                GOTO rekey
        END IF
        IF a$ = "" AND stepr = 0 THEN
        'otherwise, continue...
        ack$ = " "
        GET 42, , ack$
        mv = ASC(ack$)
        IF mv = 0 THEN
                macro = 0
                LOCATE 24, 27
                PRINT "   ";
                CLOSE 42
                GOTO rekey
        END IF
        movehim = checkmove(mv)
        facing = mv
        moveguy mv
        GOTO rekey
        ELSE
        ' pressed a key to step through?
        'LOCATE 10, 10: PRINT xpx: xpx = xpx + 1: IF xpx > 100 THEN xpx = 0
        IF a$ = "" THEN GOTO rekey
        IF a$ = CHR$(0) + CHR$(31) THEN stepr = 0: LOCATE 24, 26: PRINT " ";
        'otherwise, continue...
        ack$ = " "
        GET 42, , ack$
        mv = ASC(ack$)
        IF mv = 0 THEN macro = 0: GOTO nomacro
        movehim = checkmove(mv)
        facing = mv
        moveguy mv
        GOTO rekey
        END IF
END IF
nomacro:

IF a$ = CHR$(27) THEN
        writemove cancel
        IF escapedon = 1 THEN CLOSE : END
        IF camping = 1 THEN GOTO camp
        GOTO menu
END IF

'IF LEFT$(a$, 1) = CHR$(0) THEN
'        LOCATE 1, 1: PRINT ASC(RIGHT$(a$, 1));
'        END IF

IF a$ = CHR$(0) + CHR$(31) THEN
        ' toggle step playback
        stepr = ABS(stepr - 1)
        LOCATE 24, 26
        IF stepr = 1 THEN COLOR 4: PRINT "";  ELSE PRINT " ";
        COLOR 7
END IF

IF a$ = CHR$(0) + CHR$(25) AND macro = 0 THEN
        ' playback.
        LOCATE 1, 3
        PRINT "Playback file?";
        pfile$ = getstr$(8, "")
        IF validf(pfile$) = 1 THEN
                IF INSTR(pfile$, ".") THEN  ELSE pfile$ = pfile$ + ".XRC"
                LOCATE 1, 3: PRINT SPACE$(27);
                LOCATE 24, 27
                COLOR 15: PRINT "["; : COLOR 11: PRINT ""; : COLOR 15: PRINT "]";
                CLOSE 42
                OPEN pfile$ FOR BINARY AS #42
                macro = 1
        ELSE
        LOCATE 1, 1: PRINT SPACE$(39);
        END IF
END IF

IF a$ = CHR$(0) + CHR$(19) THEN
        ' recording.
        SELECT CASE recording
        CASE 0
        ' turn recording on.
        LOCATE 1, 3
        COLOR 15: PRINT "Record file?"; : COLOR 11
        recfile$ = getstr$(8, "")
        IF validf(recfile$) = 1 THEN
        recording = 1
        IF INSTR(recfile$, ".") THEN  ELSE recfile$ = recfile$ + ".XRC"
        LOCATE 1, 3: PRINT SPACE$(27);
        LOCATE 24, 27
        COLOR 15: PRINT "["; : COLOR 4: PRINT "þ"; : COLOR 15: PRINT "]";
        CLOSE 27
        bkill recfile$
        OPEN recfile$ FOR BINARY AS #27
        'IF camping THEN GOTO rstcmp ELSE GOTO restart
        ELSE
        LOCATE 1, 1: PRINT SPACE$(39);
        END IF
        CASE 1
        recording = 0
        LOCATE 24, 27
        PRINT "   ";
        writemove cancel
        END SELECT
        
END IF
IF a$ = CHR$(13) THEN
        writemove startover
        IF camping THEN GOTO rstcmp ELSE GOTO restart
END IF
IF a$ = "L" AND escapedon = 0 THEN file$ = "": GOTO newgame
IF a$ = "C" AND escapedon = 0 THEN GOTO campaign

IF a$ = "S" THEN
        ' Sound toggle
        soundon = ABS(soundon - 1)
        SELECT CASE soundon
        CASE 1
        IF earson = 0 THEN GET (290, 180)-(310, 199), earswap
        PUT (290, 180), earon, PSET
        CASE 0
        IF earson = 0 THEN GET (290, 180)-(310, 199), earswap
        PUT (290, 180), earoff, PSET
        END SELECT
        eartimeup! = TIMER + .8
        earson = 1
        a$ = ""
END IF

IF earson = 1 AND (TIMER > eartimeup! OR a$ <> "") THEN
        PUT (290, 180), earswap, PSET
        earson = 0
END IF


' **************************************************** CHEATS *******
IF a$ = CHR$(0) + "," THEN
        tile(guyx, guyy + 1) = 1
        tile(guyx, guyy - 1) = 1
        tile(guyx + 1, guyy) = 1
        tile(guyx - 1, guyy) = 1
        tileput guyx, guyy + 1, 1
        tileput guyx, guyy - 1, 1
        tileput guyx + 1, guyy, 1
        tileput guyx - 1, guyy, 1
END IF

IF a$ = CHR$(0) + "-" THEN
        LOCATE 1, 1: INPUT "X", newx: LOCATE 1, 1: INPUT "Y", newy
        LOCATE 1, 1: PRINT "       ";
        IF newx > 0 AND newy > 0 AND newx < 19 AND newy < 11 THEN
        tileput guyx, guyy, tile(guyx, guyy)
        guyx = newx
        guyy = newy
        putguy guyx, guyy
        END IF
END IF

IF a$ = CHR$(0) + "H" THEN
        writemove up
        movehim = checkmove(up)
        facing = 1
        moveguy up
        END IF
IF a$ = CHR$(0) + "P" THEN
        writemove down
        movehim = checkmove(down)
        facing = 2
        moveguy down
        END IF
IF a$ = CHR$(0) + "K" THEN
        writemove left
        movehim = checkmove(left)
        facing = 3
        moveguy left
        END IF
IF a$ = CHR$(0) + "M" THEN
        writemove right
        movehim = checkmove(right)
        facing = 4
        moveguy right
        END IF

IF beatlevel = 1 THEN
        writemove win
        entprompt 135
        IF escapedon = 1 THEN GOTO restart
        file$ = ""
        IF camping = 1 THEN
               
                savebeat ids$, cdoing
               
                beat(cdoing) = 1
                GOTO camp
                END IF
        GOTO menu
END IF
IF guydied = 1 THEN
        writemove startover
        entprompt 200
        IF camping = 1 THEN GOTO rstcmp ELSE GOTO restart
END IF

LOOP
endo:
' write some changes to #33
uu$ = CHR$(player)
PUT 33, 10, uu$
CLOSE
endx:
SCREEN 0
WIDTH 80
COLOR 7, 0
CLS
PRINT "Thank you for using Escape!"
PRINT : PRINT "Escape "; version$; " is Copyright (c) 1996 Tom Murphy."
PRINT "All rights reserved."
COLOR 14: PRINT : PRINT "Have a nice day."
COLOR 7
END

userfatal:
CLS
SCREEN 0
COLOR 14, 4: PRINT "     þ"; : COLOR 15: PRINT "FATAL ERROR"; : COLOR 14: PRINT "þ      "; : COLOR 7, 0
PRINT : PRINT "Error opening the specified user file. Did you call it 'con' or 'prn' or"
PRINT "some other device? Bad idea."
END
killerror:
CLS
SCREEN 0
COLOR 14, 4: PRINT "     þ"; : COLOR 15: PRINT "FATAL ERROR"; : COLOR 14: PRINT "þ      "; : COLOR 7, 0
PRINT : PRINT "Error erasing a file. This usually comes from trying to delete a device or"
PRINT "a read only or shared file."
END

SUB bkill (file$)
' safe-kills a file.
IF file$ = "" THEN EXIT SUB
m = FREEFILE
OPEN file$ FOR BINARY AS m
CLOSE m
ON ERROR GOTO killerror
KILL file$
ON ERROR GOTO 0
END SUB

FUNCTION checkmove (dir)

' This sub has to...
' (x) check for hitting walls
' (x) check edge of map
' (x) check pushable blocks (and other stuff)
' ( ) check objects! (what objects?)

testdie = 0

trnsport = 0

SELECT CASE dir
CASE up
targetspot = tile(guyx, guyy - 1)
CASE down
targetspot = tile(guyx, guyy + 1)
CASE left
targetspot = tile(guyx - 1, guyy)
CASE right
targetspot = tile(guyx + 1, guyy)
END SELECT

        dx = 0
        dy = 0
        SELECT CASE dir
        CASE up
        dy = -1
        CASE down
        dy = 1
        CASE left
        dx = -1
        CASE right
        dx = 1
        END SELECT


SELECT CASE targetspot
CASE 1, 16, 37, 39, 41
checkmove = 1
EXIT FUNCTION
CASE 3, 7, 11, 9, 12 TO 15, 17, 19, 33 TO 36, 38, 40, -1
checkmove = 0
EXIT FUNCTION
CASE 24, 25
FOR uu = 400 TO 600 STEP 10
oSound uu, .05
NEXT uu

SELECT CASE tile(guyx + dx, guyy + dy)
        CASE 24
        tile(guyx + dx, guyy + dy) = 25
        tileput guyx + dx, guyy + dy, 25
        CASE 25
        tile(guyx + dx, guyy + dy) = 24
        tileput guyx + dx, guyy + dy, 24
END SELECT

swaptiles 22, 23

checkmove = 0
CASE 22, 23
' horz/vert movers
SELECT CASE targetspot
CASE 22
IF dx = 0 THEN GOTO nomove
CASE 23
IF dy = 0 THEN GOTO nomove
END SELECT
        redtarget = tile(guyx + (2 * dx), guyy + (2 * dy))
        SELECT CASE redtarget
        CASE 1
        oSound 300, .08
        ' ok. [move old]
        tile(guyx + (2 * dx), guyy + (2 * dy)) = targetspot
        tile(guyx + dx, guyy + dy) = 1
        tileput guyx + dx, guyy + dy, 1
        tileput guyx + (2 * dx), guyy + (2 * dy), targetspot
        checkmove = 1
        EXIT FUNCTION
        END SELECT
nomove:
checkmove = 0

CASE 32
' WireOn... One serious pain in the butt.
rctilex = guyx + dx
rctiley = guyy + dy

FOR dir1 = -1 TO 1 STEP 2  ' to do all 4 directions
FOR dir2 = 0 TO 1 STEP 1
mddx = 0: mddy = 0
IF dir2 = 0 THEN mddx = dir1 ELSE mddy = dir1
rrctilex = rctilex
rrctiley = rctiley
ddx = mddx
ddy = mddy
DO
rrctilex = rrctilex + ddx
rrctiley = rrctiley + ddy
'tileput rrctilex, rrctiley, 25
'SLEEP
SELECT CASE tile(rrctilex, rrctiley)
CASE 26
IF ddx <> 0 THEN EXIT DO
CASE 27
IF ddx = 1 OR ddy = -1 THEN EXIT DO
IF ddx = -1 THEN ddy = -1: ddx = 0 ELSE ddy = 0: ddx = 1
CASE 28
IF ddx = -1 OR ddy = -1 THEN EXIT DO
IF ddx = 1 THEN ddy = -1: ddx = 0 ELSE ddy = 0: ddx = -1
CASE 29
IF ddx = 1 OR ddy = 1 THEN EXIT DO
IF ddx = -1 THEN ddy = 1: ddx = 0 ELSE ddy = 0: ddx = 1
CASE 30
IF ddx = -1 OR ddy = 1 THEN EXIT DO
IF ddx = 1 THEN ddy = 1: ddx = 0 ELSE ddy = 0: ddx = -1
CASE 31
IF ddy <> 0 THEN EXIT DO
CASE 33
oSound 800, .1
oSound 1200, .1
swaptiles 36, 37: EXIT DO
CASE 34
oSound 800, .1
oSound 1200, .1
swaptiles 38, 39: EXIT DO
CASE 35
oSound 800, .1
oSound 1200, .1
swaptiles 40, 41: EXIT DO
CASE ELSE
EXIT DO
END SELECT
LOOP
NEXT dir2
NEXT dir1
checkmove = 0
testdie = 1
EXIT FUNCTION

CASE 21
' grey breaker...
FOR u = 1 TO 15
oSound INT(RND * 300) + 200, .05
NEXT u
tile(guyx + dx, guyy + dy) = 1
tileput guyx + dx, guyy + dy, 1
checkmove = 0
testdie = 1
CASE 18
FOR u = 500 TO 200 STEP -20
oSound u, .1
NEXT u
FOR y = 1 TO 10
FOR x = 1 TO 18
IF tile(x, y) = 17 THEN
        tile(x, y) = 1
        tileput x, y, 1
END IF
NEXT x
NEXT y
tileput guyx + dx, guyy + dy, 19
tile(guyx + dx, guyy + dy) = 19
checkmove = 0
EXIT FUNCTION
CASE 5
        gretarget = tile(guyx + (2 * dx), guyy + (2 * dy))
        SELECT CASE gretarget
        CASE 1
        oSound 300, .08
        ' ok. [move old]
        tile(guyx + (2 * dx), guyy + (2 * dy)) = 3
        tile(guyx + dx, guyy + dy) = 1
        tileput guyx + dx, guyy + dy, 1
        tileput guyx + (2 * dx), guyy + (2 * dy), 3
        checkmove = 1
        END SELECT

CASE 2, 26 TO 31  ' red blocks & wires act the same
        redtarget = tile(guyx + (2 * dx), guyy + (2 * dy))
        SELECT CASE redtarget
        CASE 1
        oSound 300, .08
        ' ok. [move old]
        tile(guyx + (2 * dx), guyy + (2 * dy)) = targetspot
                SELECT CASE otag(guyx + dx, guyy + dy)
                CASE 1
                tile(guyx + dx, guyy + dy) = 10
                tileput guyx + dx, guyy + dy, 10
                CASE ELSE
                tile(guyx + dx, guyy + dy) = 1
                tileput guyx + dx, guyy + dy, 1
                END SELECT
        tileput guyx + (2 * dx), guyy + (2 * dy), targetspot
        checkmove = 1
        CASE 10
        oSound 300, .08
        ' a steptile... eeew.
        ' this is really gross.
        otag(guyx + (2 * dx), guyy + (2 * dy)) = 1 ' set the button-under tag
        tile(guyx + (2 * dx), guyy + (2 * dy)) = targetspot ' put it
      
                SELECT CASE otag(guyx + dx, guyy + dy)
                CASE 1
                tile(guyx + dx, guyy + dy) = 10
                tileput guyx + dx, guyy + dy, 10
                CASE ELSE
                tile(guyx + dx, guyy + dy) = 1
                tileput guyx + dx, guyy + dy, 1
                END SELECT
        tileput guyx + (2 * dx), guyy + (2 * dy), targetspot

        thex = guyx + (dx * 2)
        they = guyy + (dy * 2)
        destx = xtag(thex, they)
        desty = ytag(thex, they)

        
        t = ztag(destx, desty)
        IF t = 0 THEN t = 1
        ztag(destx, desty) = tile(destx, desty)
        tile(destx, desty) = t
        tileput destx, desty, t
        checkmove = 1
        CASE 17
        FOR u = 300 TO 400 STEP 15
        oSound u, .1
        NEXT u
        checkmove = 1
        ' kill it.
                SELECT CASE otag(guyx + dx, guyy + dy)
                CASE 1
                tile(guyx + dx, guyy + dy) = 10
                tileput guyx + dx, guyy + dy, 10
                CASE ELSE
                tile(guyx + dx, guyy + dy) = 1
                tileput guyx + dx, guyy + dy, 1
                END SELECT
      
        END SELECT
CASE 4
' grey blocks... paaain...

        greytarget = tile(guyx + (2 * dx), guyy + (2 * dy))
        SELECT CASE greytarget
        CASE 1
        oSound 300, .08
        ' ok. [move old]
        tile(guyx + (2 * dx), guyy + (2 * dy)) = 4
                SELECT CASE otag(guyx + dx, guyy + dy)
                CASE 1
                tile(guyx + dx, guyy + dy) = 10
                tileput guyx + dx, guyy + dy, 10
                CASE ELSE
                tile(guyx + dx, guyy + dy) = 1
                tileput guyx + dx, guyy + dy, 1
                END SELECT
        tileput guyx + (2 * dx), guyy + (2 * dy), 4
        checkmove = 1
        CASE 7
        ' a hole.
        tile(guyx + (2 * dx), guyy + (2 * dy)) = 1
                SELECT CASE otag(guyx + dx, guyy + dy)
                CASE 1
                tile(guyx + dx, guyy + dy) = 10
                tileput guyx + dx, guyy + dy, 10
                CASE ELSE
                tile(guyx + dx, guyy + dy) = 1
                tileput guyx + dx, guyy + dy, 1
                END SELECT
        tileput guyx + (2 * dx), guyy + (2 * dy), 1
        FOR MM = 750 TO 350 STEP -25
        oSound MM, .1
        NEXT MM
        checkmove = 1
        CASE 10
        oSound 300, .08
        ' a steptile... eeew.
        ' this is really gross.
        otag(guyx + (2 * dx), guyy + (2 * dy)) = 1 ' set the button-under tag
        tile(guyx + (2 * dx), guyy + (2 * dy)) = 4 ' put it
      
                SELECT CASE otag(guyx + dx, guyy + dy)
                CASE 1
                tile(guyx + dx, guyy + dy) = 10
                tileput guyx + dx, guyy + dy, 10
                CASE ELSE
                tile(guyx + dx, guyy + dy) = 1
                tileput guyx + dx, guyy + dy, 1
                END SELECT
        tileput guyx + (2 * dx), guyy + (2 * dy), 4

        thex = guyx + (dx * 2)
        they = guyy + (dy * 2)
        destx = xtag(thex, they)
        desty = ytag(thex, they)

        t = ztag(destx, desty)
        IF t = 0 THEN t = 1
        ztag(destx, desty) = tile(destx, desty)
        tile(destx, desty) = t
        tileput destx, desty, t
       
        checkmove = 1
        CASE 17
        FOR u = 300 TO 400 STEP 15
        oSound u, .1
        NEXT u
        checkmove = 1
        ' kill it.
                SELECT CASE otag(guyx + dx, guyy + dy)
                CASE 1
                tile(guyx + dx, guyy + dy) = 10
                tileput guyx + dx, guyy + dy, 10
                CASE ELSE
                tile(guyx + dx, guyy + dy) = 1
                tileput guyx + dx, guyy + dy, 1
                END SELECT
      
       
        CASE ELSE
        ' forget it!
        END SELECT
       
CASE 6
' beat the level!
LOCATE 2, 1: PRINT SPACE$(29);
LOCATE 2, 1: COLOR 11: PRINT "You win!";
beatlevel = 1
EXIT FUNCTION
CASE 10
' step tiles... eeeaaaauuugh.
'LOCATE 1, 1: PRINT xtag(guyx + dx, guyy + dy), ytag(guyx + dx, guyy + dy); : SLEEP
oSound 500, .05
thex = guyx + dx
they = guyy + dy
destx = xtag(thex, they)
desty = ytag(thex, they)

        t = ztag(destx, desty)
        IF t = 0 THEN t = 1
        ztag(destx, desty) = tile(destx, desty)
        tile(destx, desty) = t
        tileput destx, desty, t
checkmove = 1
CASE 8
' gold block... another gross one.

goldx = guyx + dx
goldy = guyy + dy
sgoldx = goldx
sgoldy = goldy
DO
        u = tile(goldx + dx, goldy + dy)
        SELECT CASE u
        CASE 1
        ' regular floor.
        tile(goldx + dx, goldy + dy) = 8
        'tile(goldx, goldy) = 1 ' er...
        'tileput goldx, goldy, 1
                SELECT CASE otag(goldx, goldy)
                CASE 1
                tile(goldx, goldy) = 10
                tileput goldx, goldy, 10
                CASE ELSE
                tile(goldx, goldy) = 1
                tileput goldx, goldy, 1
                END SELECT
        tileput goldx + dx, goldy + dy, 8
        CASE 10
        ' step tile!
        otag(goldx + dx, goldy + dy) = 1
        tile(goldx + dx, goldy + dy) = 8
        'tile(goldx, goldy) = 1 ' er...
        'tileput goldx, goldy, 1
                SELECT CASE otag(goldx, goldy)
                CASE 1
                tile(goldx, goldy) = 10
                tileput goldx, goldy, 10
                CASE ELSE
                tile(goldx, goldy) = 1
                tileput goldx, goldy, 1
                END SELECT
        tileput goldx + dx, goldy + dy, 8
        CASE 17
        FOR u = 300 TO 400 STEP 15
        oSound u, .1
        NEXT u
        SELECT CASE otag(goldx, goldy)
                CASE 1
                tile(goldx, goldy) = 10
                tileput goldx, goldy, 10
                CASE ELSE
                tile(goldx, goldy) = 1
                tileput goldx, goldy, 1
                END SELECT
                EXIT DO
        CASE ELSE
        EXIT DO
        END SELECT
        goldx = goldx + dx
        goldy = goldy + dy
LOOP
testdie = 1
IF sgoldx = goldx AND sgoldy = goldy THEN EXIT FUNCTION ' didn't move!
oSound 300, .08
' check if it ultimately landed on a steptile.
IF otag(goldx, goldy) = 1 THEN
        ' it did.
        destx = xtag(goldx, goldy)
        desty = ytag(goldx, goldy)

        t = ztag(destx, desty)
        IF t = 0 THEN t = 1
        ztag(destx, desty) = tile(destx, desty)
        tile(destx, desty) = t
        tileput destx, desty, t

END IF
' check if it started on a steptile, to be unset.
IF otag(sgoldx, sgoldy) = 1 THEN
destx = xtag(sgoldx, sgoldy)
desty = ytag(sgoldx, sgoldy)
IF ztag(destx, desty) > 0 THEN
        t = ztag(destx, desty)
        IF t = 0 THEN t = 1
        ztag(destx, desty) = tile(destx, desty)
        tile(destx, desty) = t
        tileput destx, desty, t
END IF
END IF


checkmove = 0
CASE 20
' A transporter. Pretty easy.
' handled in moveguy.
checkmove = 1
trnsport = 1

CASE ELSE
LOCATE 23, 1: COLOR 15: PRINT "["; : COLOR 14: PRINT "ERROR"; : COLOR 15: PRINT "]"
COLOR 11: PRINT "I can't handle tile type "; targetspot; ".";
SLEEP
END
END SELECT

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

SUB died
LOCATE 1, 1
COLOR 15: PRINT "["; : COLOR 4: PRINT "DEAD!"; : COLOR 15: PRINT "]"; SPACE$(15)
guydied = 1
END SUB

SUB drawhlaser (X1, Y1, x2, y2)

xx1 = X1 * 16
yy1 = Y1 * 16 + 16
xx2 = x2 * 16 - 8
yy2 = y2 * 16 + 16

LINE (xx1, yy1 - 1)-(xx2, yy2 - 1), 4
LINE (xx1, yy1)-(xx2, yy2), 15
LINE (xx1, yy1 + 1)-(xx2, yy2 + 1), 4
lasernoise

END SUB

SUB drawscreen
FOR y = 1 TO 10
FOR x = 1 TO 18
tileput x, y, tile(x, y)
NEXT x
NEXT y
END SUB

SUB drawvlaser (X1, Y1, x2, y2)

xx1 = X1 * 16
yy1 = Y1 * 16 + 16
xx2 = x2 * 16
yy2 = y2 * 16 + 8

LINE (xx1 - 1, yy1)-(xx2 - 1, yy2), 4
LINE (xx1, yy1)-(xx2, yy2), 15
LINE (xx1 + 1, yy1)-(xx2 + 1, yy2), 4

lasernoise

END SUB

SUB entprompt (cv)
       
        LOCATE 24, 1: PRINT "Press enter to continue.";
        DO WHILE INKEY$ <> CHR$(13)
        COLOR INT(RND * 16) + cv
        LOCATE 24, 1: PRINT "Press enter to continue.";
        LOOP
        LOCATE 24, 1: PRINT SPACE$(26);

END SUB

FUNCTION getstr$ (maxlen, deflt$)

px1 = POS(0)
py1 = CSRLIN
LOCATE py1, px1 + 1: PRINT SPACE$(maxlen);
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
                oSound 400, .1
        END IF
ELSE
        IF gtmp < maxlen THEN
                gtmp = gtmp + 1
                LOCATE py1, px1 + gtmp
                PRINT gtmp$; : IF gtmp <> maxlen THEN PRINT "Û";
                gtmp2$ = gtmp2$ + gtmp$
        ELSE
                oSound 400, .1
        END IF
END IF
END IF
LOOP
LOCATE py1, px1 + gtmp + 1: PRINT " ";
getstr$ = gtmp2$

END FUNCTION

SUB lasernoise

FOR MM = 1 TO 3
FOR xx = 300 + (m * 100) TO 800 + m * 100 STEP 25
oSound xx, .1
NEXT xx
NEXT MM

END SUB

SUB load (filex$, what, which)

IF what = campaign THEN starto = ((which * 567) + 56) - 567 ELSE starto = 1


OPEN filex$ FOR BINARY AS #1
IF LOF(1) = 0 THEN
        LOCATE 8, 8
        COLOR 4: PRINT "FILE DOES NOT EXIST.";
        COLOR 15
        CLOSE 1
        bkill filex$
        exitcode = 42 ' abort...
        EXIT SUB
        END IF
wa$ = " "
FOR y = 1 TO 10
FOR x = 1 TO 18
GET 1, starto, wa$
starto = starto + 1
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
title$ = SPACE$(24)
GET 1, , title$

CLOSE 1

END SUB

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

DEFINT Z
SUB loadinfo (id$)

'loads information from file #33 about level id$. If none is found, it
'creates a new heading for it, and fills it with defaults.
in$ = " "
idin$ = SPACE$(10)
GET 33, 11, in$
campaigns = ASC(in$)
FOR xo = 0 TO (campaigns * 110) STEP 110
GET 33, 12 + xo, idin$
IF idin$ = id$ THEN
        ' found it!
        ' So read the info.
        FOR uu = 0 TO 98 STEP 2
        GET 33, 22 + xo + uu, in$
        beat(uu / 2 + 1) = ASC(in$)
        GET 33, , in$
        tries(uu / 2 + 1) = ASC(in$)
        NEXT uu
        EXIT SUB
END IF
NEXT xo

' Didn't find it. =(

' move to the end of the file (last header) and put the new id string:
PUT 33, campaigns * 110 + 12, id$
' now put 100 zeros:
zeros$ = STRING$(100, CHR$(0))
PUT 33, , zeros$

'now add 1 campaign to the count...
zeros$ = CHR$(campaigns + 1)
PUT 33, 11, zeros$

' done. =)

END SUB

SUB moveguy (dir)


IF movehim = 0 THEN
        IF facing <> oface THEN
        tileput guyx, guyy, tile(guyx, guyy)
        putguy guyx, guyy
        END IF
        IF testdie = 1 THEN GOTO die
        EXIT SUB
END IF
IF testdie = 1 THEN GOTO die
moveagain:
dx = 0
dy = 0
SELECT CASE dir
CASE up
dy = -1
CASE down
dy = 1
CASE left
dx = -1
CASE right
dx = 1
END SELECT

'stepping off a button?
IF tile(guyx, guyy) = 10 THEN
oSound 300, .05
destx = xtag(guyx, guyy)
desty = ytag(guyx, guyy)
IF ztag(destx, desty) > 0 THEN
        t = ztag(destx, desty)
        IF t = 0 THEN t = 1
        ztag(destx, desty) = tile(destx, desty)
        tile(destx, desty) = t
        tileput destx, desty, t
END IF
END IF



' get rid of old guy:
tileput guyx, guyy, tile(guyx, guyy)

' move him (see, structured!!)
guyx = guyx + dx
guyy = guyy + dy

' put the new guy

putguy guyx, guyy

' now check if he's standing on a transporter
IF trnsport = 1 THEN
        trnsport = 0
        ' noise:
        FOR X1 = 1 TO 3
        FOR x2 = 400 + X1 * 100 TO 800 STEP 25
        oSound x2, .1
        NEXT: NEXT
       
        ' get rid of the old guy:
        tileput guyx, guyy, tile(guyx, guyy)
        gyx = xtag(guyx, guyy)
        guyy = ytag(guyx, guyy)
        guyx = gyx
        'putguy destx, desty
        dir = 42
        GOTO moveagain
END IF

die:

' check if he's been killed.
FOR x = 1 TO 18
FOR y = 1 TO 11
IF tile(x, y) = 9 THEN
        IF guyx = x THEN
                IF guyy < y THEN
                        FOR my = y - 1 TO guyy + 1 STEP -1
                        u = tile(guyx, my)
                        SELECT CASE u
                        CASE 2 TO 6, 8, 9, 11 TO 15, 18 TO 25, 26 TO 36, 38, 40
                        GOTO nodice
                        END SELECT
                        NEXT my
                        drawvlaser guyx, guyy, x, y
                        died
                        EXIT FOR
                END IF
                IF guyy > y THEN
                        FOR my = y + 1 TO guyy - 1
                        u = tile(guyx, my)
                        SELECT CASE u
                        CASE 2 TO 6, 8, 9, 11 TO 15, 18 TO 25, 26 TO 36, 38, 40
                        GOTO nodice
                        END SELECT
                        NEXT my
                        drawvlaser guyx, guyy, x, y + 1
                        died
                        EXIT FOR
                END IF
      
        END IF
        IF guyy = y THEN
                IF guyx < x THEN
                        FOR mx = x - 1 TO guyx + 1 STEP -1
                        u = tile(mx, guyy)
                        SELECT CASE u
                        CASE 2 TO 6, 8, 9, 11 TO 15, 18 TO 25, 26 TO 36, 38, 40
                        GOTO nodice
                        END SELECT
                        NEXT mx
                        drawhlaser guyx, guyy, x, y
                        died
                        EXIT FOR
                END IF
                IF guyx > x THEN
                        FOR mx = x + 1 TO guyx - 1
                        u = tile(mx, guyy)
                        SELECT CASE u
                        CASE 2 TO 6, 8, 9, 11 TO 15, 18 TO 25, 26 TO 36, 38, 40
                        GOTO nodice
                        END SELECT
                        NEXT mx
                        drawhlaser guyx, guyy, x + 1, y
                        died
                        EXIT FOR
                END IF
        END IF
        END IF
nodice:
NEXT y
IF guydied = 1 THEN EXIT FOR
NEXT x

END SUB

SUB oSound (freq, dura!)
IF soundon = 1 THEN SOUND freq, dura!
END SUB

SUB putguy (xtt, ytt)
'startx = INT((startx + 8) / 16) * 16 - 8
'starty = INT((starty + 8) / 16) * 16 - 8
startx = xtt * 16 - 8
starty = ytt * 16 + 8
x = startx
y = starty
FOR m = 1 TO 512
uu = dude(m, facing, player)
IF uu = 255 THEN x = startx: y = y + 1 ELSE x = x + 1: IF uu > 0 THEN PSET (x, y), uu
NEXT m
oface = facing
END SUB

SUB savebeat (id$, ctoput)
one$ = CHR$(1)
in$ = " "
idin$ = SPACE$(10)
GET 33, 11, in$
campaigns = ASC(in$)
FOR xo = 0 TO (campaigns * 110) STEP 110
GET 33, 12 + xo, idin$
IF idin$ = id$ THEN
        ' found it!
        ' So read the info.
        PUT 33, 20 + xo + (ctoput * 2), one$
        EXIT SUB
END IF
NEXT xo
' didn't find it!!?!?

LOCATE 4, 4
COLOR 4
PRINT "FATAL ERROR! REALLY FATAL!!!"
END
END SUB

SUB swaptiles (source, dest)

' swaps all source, dest, and writes them to the screen

FOR y = 1 TO 10
FOR x = 1 TO 18
IF tile(x, y) = source THEN tile(x, y) = -2
NEXT x
NEXT y
FOR y = 1 TO 10
FOR x = 1 TO 18
IF tile(x, y) = dest THEN
        tile(x, y) = source
        tileput x, y, source
        END IF
NEXT x
NEXT y
FOR y = 1 TO 10
FOR x = 1 TO 18
IF tile(x, y) = -2 THEN
        tile(x, y) = dest
        tileput x, y, dest
END IF
NEXT x
NEXT y

END SUB

SUB tileput (oxt%, oyt%, tilep%)
'round x,y:


xt% = oxt% * 16 - 8
yt% = oyt% * 16 + 8

'xt% = INT((xt% + 8) / 16) * 16 - 8
'yt% = INT((yt% + 8) / 16) * 16 - 8


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
LOCATE 24, 1: PRINT "Undefined Tile Error! #"; tilep%;
END SELECT
END SUB

FUNCTION twoc$ (in$)
IF LEN(in$) = 2 THEN twoc$ = " " + in$ ELSE twoc$ = in$
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

SUB writemove (what)
IF recording = 0 OR macro = 1 THEN EXIT SUB
SELECT CASE what
CASE up
aa$ = CHR$(up)
PUT 27, , aa$
CASE down
aa$ = CHR$(down)
PUT 27, , aa$
CASE left
aa$ = CHR$(left)
PUT 27, , aa$
CASE right
aa$ = CHR$(right)
PUT 27, , aa$
CASE startover
CLOSE 27
bkill recfile$
OPEN recfile$ FOR BINARY AS #27
CASE cancel
CLOSE 27
recording = 0
macro = 0
CASE win
CLOSE 27
recording = 0
macro = 0
END SELECT
END SUB

