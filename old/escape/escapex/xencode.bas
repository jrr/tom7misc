Attribute VB_Name = "Module1"
' converts Escape (or any) file into mailable data.

' Ported from QBASIC in November 1996

Const version = "1.0x"
Public xmand
Public cmword(20)
Sub encode(arguments)
'Dim cmword(20) As String
' Arguments is a string:

' output input input input input...
'Dim oxmand As String
Close

'Dim oxmand As String

'On Error GoTo fatalerr
Dim a As String

xmand = arguments
oxmand = xmand
'If xmand = "" Then GoTo nocommand

For zm = 1 To 20 '(max 19 input files)
a = ncword
If a = "" Then Exit For
cmword(zm) = a
Next
zm = zm - 1
If zm = 1 Then b = cmword(1): firste = 1
If zm > 1 Then b = cmword(1): firste = 2

If firste = 1 Then If InStr(b, ".") Then Debug.Print: Debug.Print "No extension allowed if encoding only one file.": GoTo endit
Dim crlf As String
crlf = Chr(13) + Chr(10)
thise = firste
outt = b
If InStr(b, ".") Then Else outt = b + ".txt"
Debug.Print "Outputting to "; outt
Open outt For Binary As #2

Dim inxs As String
Dim out As String
inxs = " "

Do Until EOF(2) ' advance to the end
Get 2, , inxs
Loop
Put 2, , crlf

doit:
a = cmword(thise)
thise = thise + 1
If a = "" Then GoTo noinput

inn = a
If InStr(a, ".") Then Else inn = a + ".esc"


Debug.Print: Debug.Print "File: "; inn; " ";
'pctx = POS(0)
'pcty = CSRLIN

Open inn For Binary As 1
If LOF(1) = 0 Then
        Close 1
        Kill inn
        Debug.Print "The file "; inn; " did not exist or was empty."
        GoTo doit
End If
' Put the starting and ending information:
a = "--[XnC " + version + "]-----Begin Escape Level-----" + crlf
Put 2, , a
a = "Filename= " + strippath(inn) + crlf + "*" + crlf
Put 2, , a

csum1 = 0
csum2 = 0
csum3 = 0

'LOCATE 2, 1: PRINT LOF(1);
'LOCATE 5, 1

amount = LOF(1)
x = 0
Do Until EOF(1)
Get 1, , inxs
out = ":" + strip(Str(Asc(inxs)))
x = x + 1
'If x / 200 = Int(x / 200) Then
'        cpct = (x / amount) * 100
'        LOCATE pcty, pctx
'        Print , Str(cpct); "% ";
'End If
' checksums
csum1 = csum1 + Asc(inxs)
If csum1 > 255 Then csum1 = csum1 - 255
csum2 = csum2 + (csum1 Xor Asc(inxs))
If csum2 > 255 Then csum2 = csum2 - 255
csum3 = csum3 + (Asc(inxs) And csum2)
If csum3 > 255 Then csum3 = csum3 - 255


' cheesy RLE...
If oin = inxs Then reps = reps + 1
If oin <> inxs Then
        If reps > 3 Then
                out = "]" + strip(Str(reps)) + "}" + strip(Str(Asc(oin))) + ":" + strip(Str(Asc(inxs)))
        End If
        If reps <= 3 Then
                out = ""
                For xxx = 1 To reps
                out = out + ":" + strip(Str(Asc(oin)))
                Next xxx
                out = out + ":" + strip(Str(Asc(inxs)))
        End If
        reps = 0
        If widt > 55 Then widt = 0: out = out + crlf
        widt = widt + Len(out)
        Put 2, , out
End If
oin = inxs
Loop

' put the checksums...
'PRINT "Checksums:"; csum1; csum2; csum3
a = crlf + "?" + strip(Str(csum1)) + "?" + strip(Str(csum2)) + "?" + strip(Str(csum3))
Put 2, , a
a = crlf + "------End Escape Level------" + crlf
Put 2, , a
'        LOCATE pcty, pctx
'        Print , "100%"
encoded = encoded + 1
Close 1
GoTo doit

End

nocommand:
MsgBox "ERROR! No arguments to decode()!", vbCritical
Exit Sub

noinput:
endit:
'Color 7
MsgBox "Done! Encoded" + Str(encoded) + " files.", vbInformation
Close
Exit Sub

fatalerr:
MsgBox "FATAL ERROR!", vbCritical
Close
End

End Sub


Function ncword()
' uses 'n modifies xmand
vvv = Len(xmand)
For i = 1 To vvv
g = Mid(xmand, i, 1)
If g = " " Then ncword = oo: xmand = Right(xmand, vvv - i): Exit Function Else oo = oo + g
Next i
xmand = ""
ncword = oo

End Function

Function strip(ix)
strip = Right(ix, Len(ix) - 1)
End Function


Function strippath(pathin) As String

' Strips the c:\directory\subdir\ off a filepath

u = InStr(pathin, "\")
If u = 0 Then strippath = pathin: Exit Function

Do While u > 0   ' find last occurance...
ou = u
u = InStr(ou + 1, pathin, "\")
Loop

strippath = Right(pathin, Len(pathin) - ou)

End Function


