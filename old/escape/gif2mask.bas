DEFDBL A-Z
' GIF2MASK - for screen D - turns screen 13 screen into screen D mask format
DECLARE FUNCTION MouseInit% ()
DECLARE SUB loadagif (aloady$, adjust%)
DECLARE SUB MouseDriver (ax%, bx%, cx%, dx%)
DECLARE SUB mousestatus (lb%, rb%, xMouse%, yMouse%)
DECLARE SUB MouseHide ()
DECLARE SUB MouseShow ()
COMMON SHARED x, y, lb, rb, xMouse, yMouse
'$INCLUDE: 'loadgif.bi'
DIM cpt(8)
 DEF SEG = &HA000:             
DIM SHARED mouse$: mouse$ = SPACE$(57)
FOR i% = 1 TO 57:  READ a$:  H$ = CHR$(VAL("&H" + a$))
MID$(mouse$, i%, 1) = H$: NEXT i%
DATA 55,89,E5,8B,5E,0C,8B,07,50,8B,5E,0A,8B,07,50,8B
DATA 5E,08,8B,0F,8B,5E,06,8B,17,5B,58,1E,07,CD,33,53
DATA 8B,5E,0C,89,07,58,8B,5E,0A,89,07,8B,5E,08,89,0F
DATA 8B,5E,06,89,17,5D,CA,08,00             
RESTORE
ms% = MouseInit%
IF NOT ms% THEN
  PRINT "Mouse not found.. and you need one."
  END
END IF

INPUT "GIF file to convert (valid DOS files only please):", infile$
INPUT "File to output to:", outfile$
INPUT "X width?", xww
INPUT "Y width?", yww
SCREEN 13
loadagif infile$, 0

'save across for this version
crlf$ = CHR$(13) + CHR$(10)
db$ = "DB "
z = 1
OPEN outfile$ FOR BINARY AS #1
PUT 1, z, db$
z = z + LEN(db$)
FOR ry = 1 TO yww
FOR rx = 1 TO xww STEP 8
FOR i = 0 TO 7
cpt(i + 1) = 1
m = POINT(rx + i, ry)' get value
IF m = 0 THEN cpt(i + 1) = 0' set bit
PSET (rx + i, ry), 10
NEXT i
'convert to binary
m = (128 * cpt(1)) + (64 * cpt(2)) + (32 * cpt(3)) + (16 * cpt(4)) + (8 * cpt(5)) + (4 * cpt(6)) + (2 * cpt(7)) + cpt(8)
c$ = STR$(m) + ","
PUT 1, z, c$
z = z + LEN(c$)
rep = rep + 1: IF rep = 19 THEN rep = 0: z = z - 1: PUT 1, z, crlf$: z = z + LEN(crlf$): PUT 1, z, db$: z = z + LEN(db$)
NEXT rx
NEXT ry
z = z - 1
PUT 1, z, crlf$
z = z + LEN(clrf$)
' all should be OK
CLOSE

DEFINT A-Y
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

SUB MouseHide
 ax% = 2
 MouseDriver ax%, 0, 0, 0
END SUB

FUNCTION MouseInit%
  ax% = 0
  MouseDriver ax%, 0, 0, 0
  MouseInit% = ax%
END FUNCTION

SUB MouseShow
  ax% = 1
  MouseDriver ax%, 0, 0, 0
END SUB

SUB mousestatus (lb%, rb%, xMouse%, yMouse%)
  ax% = 3
  MouseDriver ax%, bx%, cx%, dx%
  lb% = ((bx% AND 1) <> 0)
  rb% = ((bx% AND 2) <> 0)
  xMouse% = cx%
  yMouse% = dx%
END SUB

