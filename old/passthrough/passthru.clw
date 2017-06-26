; CLW file contains information for the MFC ClassWizard

[General Info]
Version=1
LastClass=Passdlg
LastTemplate=CDialog
NewFileInclude1=#include "stdafx.h"
NewFileInclude2=#include "passthru.h"

ClassCount=3
Class1=Passthrough
Class2=Passdlg
Class3=CAboutDlg

ResourceCount=3
Resource1=IDD_PASSTHRU_DIALOG
Resource2=IDR_MAINFRAME
Resource3=IDD_ABOUTBOX

[CLS:Passthrough]
Type=0
HeaderFile=passthru.h
ImplementationFile=passthru.cpp
Filter=N

[CLS:Passdlg]
Type=0
HeaderFile=passdlg.h
ImplementationFile=passdlg.cpp
Filter=D
BaseClass=CDialog
VirtualFilter=dWC
LastObject=Passdlg

[CLS:CAboutDlg]
Type=0
HeaderFile=passdlg.h
ImplementationFile=passdlg.cpp
Filter=D

[DLG:IDD_ABOUTBOX]
Type=1
Class=CAboutDlg
ControlCount=11
Control1=IDC_STATIC,static,1342177283
Control2=IDC_STATIC,static,1342308480
Control3=IDC_STATIC,static,1342308352
Control4=IDOK,button,1342373889
Control5=IDC_STATIC,static,1342308352
Control6=IDC_STATIC,static,1342308352
Control7=IDC_STATIC,button,1342177287
Control8=IDC_STATIC,static,1342308352
Control9=IDC_STATIC,static,1342308352
Control10=IDC_STATIC,static,1342308352
Control11=IDC_STATIC,static,1342308352

[DLG:IDD_PASSTHRU_DIALOG]
Type=1
Class=Passdlg
ControlCount=6
Control1=IDOK,button,1342242817
Control2=IDC_STATIC,static,1342308352
Control3=IDC_STATIC,static,1342308352
Control4=IDC_STATIC,static,1342308352
Control5=IDC_LISTFROM,listbox,1352728577
Control6=IDC_LISTTO,listbox,1352728577

