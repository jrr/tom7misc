Attribute VB_Name = "Escaped"
'Option Explicit

Declare Function BitBlt Lib "GDI" (ByVal hDestDC As Integer, ByVal X As Integer, ByVal Y As Integer, ByVal nWidth As Integer, ByVal nHeight As Integer, ByVal hSrcDC As Integer, ByVal XSrc As Integer, ByVal YSrc As Integer, ByVal dwRop As Long) As Integer
Declare Function SetWindowWord Lib "User" (ByVal hwnd As Integer, ByVal index As Integer, ByVal wNewWord As Integer) As Integer
Global Const SRCCOPY = &HCC0020 ' (DWORD) dest = source
Global Const SRCAND = &H8800C6
Global Const SRCPAINT = &HEE0086
Global Const NOTSRCCOPY = &H330008
Global Const NoRefresh = 1
Global Const lasttile = 41
Global puttingGuy As Integer
Global FloatLoaded As Integer
Global Const GWW_HWNDPARENT = (-8)
Global drawtile As Integer
Public tile(18, 10)
Public xtag(18, 1 To 10)
Public ytag(18, 10)

Sub bootup()

newlevel
drawtile = 3
End Sub
Sub drawtiles()

For Y = 1 To 10
For X = 1 To 18
puttile X, Y, tile(X, Y), NoRefresh
Next: Next
WinEscape.picture1.Refresh

End Sub

Sub loadfile(filename As String)
Dim wa As String
Close 1
Open filename For Binary As #1
        If LOF(1) = 0 Then
                ' file empty!
                Close 1
                Exit Sub
                End If
        wa = " "
        For Y = 1 To 10
        For X = 1 To 18
        Get 1, , wa
        tile(X, Y) = Asc(wa)
        Next: Next
        For Y = 1 To 10
        For X = 1 To 18
        Get 1, , wa
        xtag(X, Y) = Asc(wa)
        Next: Next
        For Y = 1 To 10
        For X = 1 To 18
        Get 1, , wa
        ytag(X, Y) = Asc(wa)
        Next: Next
Get 1, , wa
guystx = Asc(wa)
Get 1, , wa
guysty = Asc(wa)
Dim title As String
title = Space(30)
Get 1, , title
WinEscape.levelname.Text = title
Close 1
drawtiles

End Sub

Sub newlevel()
For X = 1 To 18
tile(X, 1) = 3
tile(X, 10) = 3
Next X
For Y = 2 To 9
tile(1, Y) = 3
tile(18, Y) = 3
For X = 2 To 17
tile(X, Y) = 1
Next X
Next Y

drawtiles

WinEscape.levelname.Text = "Untitled Level"
savename = ""
End Sub

Sub puttile(ByVal X As Integer, ByVal Y As Integer, ByVal tilex As Integer, NoRefreshy)
m = NoRefreshy
' puts tile # tilex at x,y (grid coords)

' calc input x,y

srcy = Int((tilex - 1) / 16) + 1
srcx = tilex Mod 16
If srcx = 0 Then srcx = 16

blockput srcx, srcy, X, Y
If m <> 1 Then WinEscape.picture1.Refresh
'Debug.Print srcx, srcy

End Sub

Sub blockput(srcx, srcy, destx, desty)
With WinEscape
  Dim bitmap_x%, bitmap_y%, z%
  Dim srcxx As Integer, srcyy As Integer
  .picture1.AutoRedraw = True
  .PicClip.AutoRedraw = True
'  picture1 = LoadPicture("")
  srcxx = (32 * (srcx - 1))
  srcyy = (32 * (srcy - 1))
  z% = BitBlt(.picture1.hDC, (destx - 1) * 32, (desty - 1) * 32, 32, 32, .PicClip.hDC, srcxx, srcyy, SRCCOPY)
  'picture1.Refresh
  'picture1.AutoRedraw = False
  'PicClip.AutoRedraw = False

End With
End Sub

Sub savefile(filename As String)

savef = filename
Open savef For Binary As #1
statusPrint "Writing tiles...  "
Dim wa As String
For Y = 1 To 10
For X = 1 To 18
wa = Chr(tile(X, Y))
Put 1, , wa
Next: Next
statusPrint "Writing X tags...    "
For Y = 1 To 10
For X = 1 To 18
wa = Chr(xtag(X, Y))
Put 1, , wa
Next: Next
statusPrint "Writing Y tags..."
For Y = 1 To 10
For X = 1 To 18
wa = Chr(ytag(X, Y))
Put 1, , wa
Next: Next
statusPrint "Writing Start positions..."
wa = Chr(guystx)
Put 1, , wa
wa = Chr(guysty)
Put 1, , wa
statusPrint "Writing title...          "
Dim title As String
title = padspace(WinEscape.levelname.Text, 25)
Put 1, , title
Close 1
statusPrint "Done Saving."

End Sub

Function padspace(instring As String, lent As Integer) As String
instring = Left(instring, lent)
a = Space(lent)
For m = 1 To Len(instring)
Mid(a, m, 1) = Mid(instring, m, 1)
Next
padspace = a
End Function

Sub statusPrint(message As String)

WinEscape.StatBox.Caption = message
StatusTime = 3

End Sub


