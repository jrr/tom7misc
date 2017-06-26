'Simple test program for LoadGIF routine:
'Specify a GIF filename on the command line and
'it will be shown in 320x200x256 color mode.

DEFINT A-Z
'$INCLUDE: 'LOADGIF.BI'

A$ = COMMAND$           '...Or specify filename directly.
IF LEN(A$) = 0 THEN END
A$ = A$ + CHR$(0)       'Must make "ASCIZ" filename.

SCREEN 13               '320x200x256, 6 bits per primary.
ScrType = 1             'Select mode 13 pixel plotter.
ScrOffset = 0           'Screen offset is 0.
ScrWidth = 320          '320 bytes per scanline.
XRes = 319              'Screen size is (0,0)-(319,199)
YRes = 199
AdapterType = 1         'Set VGA palette.

'SCREEN 12               '640x480x16, 6 bits per primary.
'ScrType = 0             'Select 16 color mode pixel plotter.
'ScrOffset = 0           'Screen offset is 0.
'ScrWidth = 80           '80 bytes per scanline.
'XRes = 639              'Screen size is (0,0)-(639,479)
'YRes = 479
'AdapterType = 1         'Set VGA palette.

'SCREEN 9                '640x350x16, 2 bits per primary.
'ScrType = 0             'Select 16 color mode pixel plotter.
'ScrOffset = 0           'Screen offset is 0.
'ScrWidth = 80           '80 bytes per scanline.
'XRes = 639              'Screen size is (0,0)-(639,479)
'YRes = 349
'AdapterType = 0         'Set EGA palette.

X0 = 0                  'View window covers entire screen.
Y0 = 0                  'Any points outside of this window will not
X1 = XRes               'be plotted.
Y1 = YRes
XOrg = 0                'Upper left hand corner of GIF goes at (0,0).
YOrg = 0
PalIgnore = 0           'Don't ignore palette.

'Allocate the memory required by the LoadGIF function.
DIM GIFMem(LoadGIFMem * 8) AS INTEGER   '8 integers = 16 bytes = 1 page.

'For QuickBASIC 4.5.
A = LoadGIF(VARSEG(GIFMem(0)), VARSEG(A$), SADD(A$), ScrType, ScrOffset, ScrWidth, XRes, YRes, X0, Y0, X1, Y1, XOrg, YOrg, AdapterType, PalIgnore, 0, 0, PalColors)
'For PDS and VB-DOS.
'A = LoadGIF(VARSEG(GIFMem(0)), SSEG(A$), SADD(A$), ScrType, ScrOffset, ScrWidth, XRes, YRes, X0, Y0, X1, Y1, XOrg, YOrg, AdapterType, PalIgnore, 0, 0, PalColors)

'Deallocate memory required by the LoadGIF function.
ERASE GIFMem

IF A THEN              'check error variable
 SCREEN 0: WIDTH 80
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
END
END IF

A$ = INPUT$(1)
SCREEN 0: WIDTH 80

