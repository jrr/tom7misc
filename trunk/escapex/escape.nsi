;; This file configures the installer for windows.
;; I'm using the NSIS installer system (nsis.sourceforge.net).
;; Very little happens in here!

;; to-do:
;; run escape when done

;; from example
!macro BIMAGE IMAGE
	Push $0
	GetTempFileName $0
	File /oname=$0 "${IMAGE}"
	SetBrandingImage $0
	Delete $0
	Pop $0
!macroend


;--------------------------------

; The name of the installer
Name "Escape"

; The filename for the installer
OutFile "escapesetup.exe"

BrandingText "Escape Setup"

; The default installation directory
;; Not using the traditional $PROGRAMFILES because
;; Escape wants to write to its directory (including
;; overwriting its own binary). $APPDATA is a reasonable
;; choice, but this directory is hidden from users, which
;; makes it harder to do things like manually recover one's
;; player file or levels. 
InstallDir c:\escape

  ;; less confusing
  ShowInstDetails "nevershow"
  ShowUninstDetails "nevershow"

; best compression
SetCompressor lzma

CompletedText "Completed. Press close to quit the installer."

Icon "escape-install.ico"

;--------------------------------

; Modern stuff

  ; use modern skin
;  !include "MUI.nsh"

  ; show finish page
;  !define MUI_FINISHPAGE  

SetFont Verdana 8

AddBrandingImage left 100

; Pages

Function dirImage
	!insertmacro BIMAGE "graphics\installbrand.bmp"
FunctionEnd

Function instImage
	!insertmacro BIMAGE "graphics\installbrand.bmp"
FunctionEnd


Page directory dirImage
Page instfiles instImage

;--------------------------------

; The stuff to install
Section "" ; No components page, so name is not important

  ; Set output path to the installation directory.
  SetOutPath $INSTDIR
  
  ; XXX get this from some master source (currently
  ; RELEASEFILES in the makefile)
  File escape.exe
  File replace.exe
  File font.png
  File animation.png
  File fontsmall.png
  File tiles.png
  File tileutil.png
  File title.png
  File splash.png
  File icon.png
  File escape.txt
  File COPYING
  File changelog

; would be nice to abstract this following
; repeated operation into an nsis function
  SetOutPath $INSTDIR\triage
  File triage\*.esx
  File triage\*.esi

  SetOutPath $INSTDIR\triage\veryfunny
  File triage\veryfunny\*.esx
  File triage\veryfunny\*.esi

  SetOutPath $INSTDIR\official
;; currently none
;  File official\*.esx
  File official\*.esi

  SetOutPath $INSTDIR\official\tutorial
  File official\tutorial\*.esx
  File official\tutorial\*.esi

; but make an empty skeleton for mylevels
  SetOutPath $INSTDIR\mylevels
  File mylevels\index.esi

; desktop and start menu links. (no prompting)
  CreateShortCut "$DESKTOP\Escape.lnk" "$INSTDIR\escape.exe" ""
  CreateShortCut "$SMPROGRAMS\Escape.lnk" "$INSTDIR\escape.exe" ""

SectionEnd ; end the section
