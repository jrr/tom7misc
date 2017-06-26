VERSION 4.00
Begin VB.Form Xdecode 
   Caption         =   "XENCODE"
   ClientHeight    =   3210
   ClientLeft      =   1140
   ClientTop       =   1515
   ClientWidth     =   6720
   Height          =   3615
   Left            =   1080
   LinkTopic       =   "Form1"
   ScaleHeight     =   3210
   ScaleWidth      =   6720
   Top             =   1170
   Width           =   6840
   Begin VB.ListBox inFilesList 
      Height          =   2010
      Left            =   120
      MultiSelect     =   2  'Extended
      TabIndex        =   6
      Top             =   1080
      Width           =   6495
   End
   Begin VB.Frame Frame1 
      Caption         =   "Save to"
      Height          =   615
      Left            =   3600
      TabIndex        =   3
      Top             =   360
      Width           =   1815
      Begin VB.OptionButton ToClip 
         Caption         =   "Clipboard"
         Height          =   255
         Left            =   720
         TabIndex        =   5
         Top             =   240
         Width           =   975
      End
      Begin VB.OptionButton ToFile 
         Caption         =   "File"
         Height          =   255
         Left            =   120
         TabIndex        =   4
         Top             =   240
         Value           =   -1  'True
         Width           =   735
      End
   End
   Begin VB.CommandButton Command2 
      Caption         =   "Add file"
      Height          =   375
      Left            =   1080
      TabIndex        =   2
      Top             =   600
      Width           =   1095
   End
   Begin VB.CommandButton Command1 
      Caption         =   "Save"
      Height          =   375
      Left            =   2280
      TabIndex        =   1
      Top             =   600
      Width           =   1215
   End
   Begin MSComDlg.CommonDialog CDG1 
      Left            =   1320
      Top             =   120
      _version        =   65536
      _extentx        =   847
      _extenty        =   847
      _stockprops     =   0
      dialogtitle     =   "Add level"
      filename        =   "*.esc"
      filter          =   "*.esc"
   End
   Begin VB.Label Label1 
      Alignment       =   2  'Center
      Caption         =   "X E N C O D E"
      BeginProperty Font 
         name            =   "MS Sans Serif"
         charset         =   1
         weight          =   400
         size            =   18
         underline       =   0   'False
         italic          =   0   'False
         strikethrough   =   0   'False
      EndProperty
      Height          =   375
      Left            =   1320
      TabIndex        =   0
      Top             =   0
      Width           =   3855
   End
End
Attribute VB_Name = "Xdecode"
Attribute VB_Creatable = False
Attribute VB_Exposed = False

Private Sub Command1_Click()

' Decode which kind?

If ToClip.Value = True Then
Open "cliptmp.txt" For Binary As #1
Close
Kill "CLIPTMP.TXT"
        
        For m = 0 To inFilesList.ListCount
            outfilez = outfilez + " " + inFilesList.List(m)
        Next m
        Debug.Print outfilez
encode "CLIPTMP.TXT" + outfilez
Open "CLIPTMP.TXT" For Binary As #1
Dim inpoo As String
inpoo = " "
For m = 1 To LOF(1)
Get 1, , inpoo
Clipstring = Clipstring + inpoo
Next
Close 1
Kill "CLIPTMP.TXT"
Clipboard.SetText Clipstring

Else
    CDG1.Filter = "*.*"
    CDG1.DialogTitle = "Save As"
    CDG1.filename = "*.txt"
    CDG1.ShowSave
    If CDG1.filename <> "*.txt" Then
        ' encode it!
        Debug.Print CDG1.filename
        For m = 0 To inFilesList.ListCount
            outfilez = outfilez + " " + inFilesList.List(m)
        Next m
        encode CDG1.filename + outfilez
'        encode outfile + " " + infiles
    End If

End If

End Sub


Private Sub Dir1_Click()
File1.Pattern = Dir1.List(ListIndex) + "\*.esc"
End Sub


Private Sub Command2_Click()
CDG1.filename = "*.esc"
CDG1.Filter = "*.esc"
CDG1.DialogTitle = "Add file"
CDG1.ShowOpen
'Debug.Print CDG1.filename
If CDG1.filename <> "*.esc" Then inFilesList.AddItem CDG1.filename
End Sub


Private Sub Form_Resize()
If Xdecode.Width < 5835 Then Xdecode.Width = 5835
If Xdecode.Height < 3835 Then Xdecode.Height = 3835

inFilesList.Height = Xdecode.Height - 1630
inFilesList.Width = Xdecode.Width - 345
End Sub


Private Sub inFilesList_KeyDown(KeyCode As Integer, Shift As Integer)
If KeyCode = 46 Then ' delete
    Do
        u = u + 1
        If inFilesList.Selected(u - 1) Then inFilesList.RemoveItem (u - 1)
    Loop While u < inFilesList.ListCount
End If
End Sub

