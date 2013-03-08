; gdl-editor-installer.nsi
;
; This script is based on example1.nsi, but it remember the directory, 
; has uninstall support and (optionally) installs start menu shortcuts.
;
; It will install gdl-editor-installer.nsi into a directory that the user selects,

;--------------------------------

; The name of the installer
Name "GDL editor"

; The file to write
OutFile "../../../../dist/gdl-editor/gdl-editor-installer.exe"

; The default installation directory
InstallDir $PROGRAMFILES\GDLEditor

; Registry key to check for directory (so if you install again, it will 
; overwrite the old one automatically)
InstallDirRegKey HKLM "Software\NSIS_GDL_editor" "Install_Dir"

; Request application privileges for Windows Vista
RequestExecutionLevel admin


!macro WriteToFile NewLine File String
  !if `${NewLine}` == true
  Push `${String}$\r$\n`
  !else
  Push `${String}`
  !endif
  Push `${File}`
  Call WriteToFile
!macroend
!define WriteToFile `!insertmacro WriteToFile false`
!define WriteLineToFile `!insertmacro WriteToFile true`

;--------------------------------

; Pages

Page components
Page directory
Page instfiles

UninstPage uninstConfirm
UninstPage instfiles

;--------------------------------

; The stuff to install
Section "GDL editor (required)"
  
  SectionIn RO
  
  ; Set output path to the installation directory.
  SetOutPath $INSTDIR
  
  ; Get compiled files
  File /r "..\..\..\target\gdl-editor\*"
  
  ; Write the installation path into the registry
  WriteRegStr HKLM SOFTWARE\NSIS_GDL_editor "Install_Dir" "$INSTDIR"
  
  ;Write user config
  Delete "$INSTDIR\UserConfig.properties"
  ${WriteLineToFile} "$INSTDIR\UserConfig.properties" "ArchetypesFolder=$INSTDIR\Archetypes"
  ${WriteLineToFile} "$INSTDIR\UserConfig.properties" "TemplatesFolder=$INSTDIR\Templates"
  ${WriteLineToFile} "$INSTDIR\UserConfig.properties" "TerminologiesFolder=$INSTDIR\Terminologies"
  ${WriteLineToFile} "$INSTDIR\UserConfig.properties" "OntologiesFolder=$INSTDIR\Ontologies"
  
  ; Write the uninstall keys for Windows
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\GDLEditor" "DisplayName" "NSIS GDLEditor"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\GDLEditor" "UninstallString" '"$INSTDIR\uninstall.exe"'
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\GDLEditor" "NoModify" 1
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\GDLEditor" "NoRepair" 1
  WriteUninstaller "uninstall.exe"
  
SectionEnd

; Optional section (can be disabled by the user)
Section "Start Menu Shortcuts"
  CreateDirectory "$SMPROGRAMS\GDLEditor"
  CreateShortCut "$SMPROGRAMS\GDLEditor\Uninstall.lnk" "$INSTDIR\uninstall.exe" "" "$INSTDIR\uninstall.exe" 0
  CreateShortCut "$SMPROGRAMS\GDLEditor\GDLEditor.lnk" "$INSTDIR\gdl-editor.exe" "" "$INSTDIR\gdl-editor.exe" 0
SectionEnd

; Register file extension GDL
Section "Register GDL File extension"
	WriteRegStr HKCR ".gdl" "" "GDLEditor.File"
	WriteRegStr HKCR "GDLEditor.File" "" "GDLEditor File"
	WriteRegStr HKCR "GDLEditor.File\DefaultIcon" "" "$INSTDIR\gdl.ico"
	WriteRegStr HKCR "GDLEditor.File\shell\open\command" "" '"$INSTDIR\gdl-editor.exe" "%1"'
	WriteRegStr HKCR "GDLEditor.File\shell\print\command" "" '"$INSTDIR\gdl-editor.exe" /p "%1"'
SectionEnd

;--------------------------------

; Uninstaller

Section "Uninstall"
  
  ; Remove registry keys
  DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\GDLEditor"
  DeleteRegKey HKLM SOFTWARE\NSIS_GDLEditor

  ; Remove files and uninstaller
  RMDir /r $INSTDIR

  ; Remove shortcuts, if any
  Delete "$SMPROGRAMS\GDLEditor\*.*"

  ; Remove directories used
  RMDir "$SMPROGRAMS\GDLEditor"
  RMDir /r "$INSTDIR"

SectionEnd

Function WriteToFile
Exch $0 ;file to write to
Exch
Exch $1 ;text to write
 
  FileOpen $0 $0 a #open file
  FileSeek $0 0 END #go to end
  FileWrite $0 $1 #write to file
  FileClose $0
 
Pop $1
Pop $0
FunctionEnd