; CLW file contains information for the MFC ClassWizard

[General Info]
Version=1
LastClass=mapdlg
LastTemplate=CDialog
NewFileInclude1=#include "stdafx.h"
NewFileInclude2=#include "Tabsynth.h"
LastPage=0

ClassCount=7
Class1=CTabsynthApp
Class2=CTabsynthDoc
Class3=CTabsynthView
Class4=CMainFrame

ResourceCount=4
Resource1=IDD_ABOUTBOX
Resource2=IDR_MAINFRAME
Class5=CAboutDlg
Class6=combinedlg
Resource3=IDD_COMBINEDLG
Class7=mapdlg
Resource4=TOM7_MAPDLG

[CLS:CTabsynthApp]
Type=0
HeaderFile=Tabsynth.h
ImplementationFile=Tabsynth.cpp
Filter=N
BaseClass=CWinApp
VirtualFilter=AC

[CLS:CTabsynthDoc]
Type=0
HeaderFile=TabsynthDoc.h
ImplementationFile=TabsynthDoc.cpp
Filter=N
BaseClass=CDocument
VirtualFilter=DC

[CLS:CTabsynthView]
Type=0
HeaderFile=TabsynthView.h
ImplementationFile=TabsynthView.cpp
Filter=C
BaseClass=CView
VirtualFilter=VWC
LastObject=COMBINETREE

[CLS:CMainFrame]
Type=0
HeaderFile=MainFrm.h
ImplementationFile=MainFrm.cpp
Filter=T



[CLS:CAboutDlg]
Type=0
HeaderFile=Tabsynth.cpp
ImplementationFile=Tabsynth.cpp
Filter=D

[DLG:IDD_ABOUTBOX]
Type=1
Class=CAboutDlg
ControlCount=4
Control1=IDC_STATIC,static,1342308480
Control2=IDC_STATIC,static,1342308352
Control3=IDOK,button,1342373889
Control4=IDC_STATIC,static,1342177283

[MNU:IDR_MAINFRAME]
Type=1
Class=CMainFrame
Command1=ID_FILE_NEW
Command2=ID_FILE_OPEN
Command3=ID_FILE_SAVE
Command4=ID_FILE_SAVE_AS
Command5=ID_FILE_MRU_FILE1
Command6=ID_APP_EXIT
Command7=ID_MAPPINGS_EDIT
Command8=TOM7_TEST_TONE
Command9=TOM7_OTONE
Command10=TEST_MAPDLG
Command11=ID_APP_ABOUT
CommandCount=11

[ACL:IDR_MAINFRAME]
Type=1
Class=CMainFrame
Command1=ID_FILE_NEW
Command2=ID_FILE_OPEN
Command3=ID_FILE_SAVE
Command4=ID_EDIT_UNDO
Command5=ID_EDIT_CUT
Command6=ID_EDIT_COPY
Command7=ID_EDIT_PASTE
Command8=ID_EDIT_UNDO
Command9=ID_EDIT_CUT
Command10=ID_EDIT_COPY
Command11=ID_EDIT_PASTE
Command12=ID_NEXT_PANE
Command13=ID_PREV_PANE
CommandCount=13

[TB:IDR_MAINFRAME]
Type=1
Class=?
Command1=ID_FILE_NEW
Command2=ID_FILE_OPEN
Command3=ID_FILE_SAVE
Command4=ID_EDIT_CUT
Command5=ID_EDIT_COPY
Command6=ID_EDIT_PASTE
Command7=ID_FILE_PRINT
Command8=ID_APP_ABOUT
CommandCount=8

[CLS:combinedlg]
Type=0
HeaderFile=combinedlg.h
ImplementationFile=combinedlg.cpp
BaseClass=CDialog
Filter=D
LastObject=combinedlg

[DLG:IDD_COMBINEDLG]
Type=1
Class=combinedlg
ControlCount=2
Control1=IDOK,button,1342242817
Control2=COMBINETREE,SysTreeView32,1350631427

[DLG:TOM7_MAPDLG]
Type=1
Class=mapdlg
ControlCount=3
Control1=IDOK,button,1342242817
Control2=IDCANCEL,button,1208025088
Control3=TOM7_MAPDISPLAY,static,1342181390

[CLS:mapdlg]
Type=0
HeaderFile=mapdlg.h
ImplementationFile=mapdlg.cpp
BaseClass=CDialog
Filter=W
VirtualFilter=dWC
LastObject=mapdlg

