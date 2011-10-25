; NSIS Config (http://nsis.sf.net)
;
; Run it with
;
;    .../makensis [-DWITH_SOURCES] [-DWITH_SQLITE_DLLS] this-file.nsi
;
; to create the installer sqliteodbc.exe
;
; If -DWITH_SOURCES is specified, source code is included.
; If -DWITH_SQLITE_DLLS is specified, separate SQLite DLLs
; are packaged which allows to exchange these independently
; of the ODBC drivers in the Win32 system folder.

; -------------------------------
; Start

BrandingText " "
Name "SQLite ODBC Driver"

!define PROD_NAME  "SQLite ODBC Driver"
!define PROD_NAME0 "SQLite ODBC Driver"
CRCCheck On
!include "MUI.nsh"
!include "Sections.nsh"
 
;--------------------------------
; General
 
OutFile "sqliteodbc.exe"
 
;--------------------------------
; Folder selection page
 
InstallDir "$PROGRAMFILES\${PROD_NAME0}"
 
;--------------------------------
; Modern UI Configuration

!define MUI_ICON "sqliteodbc.ico"
!define MUI_UNICON "sqliteodbc.ico" 
!define MUI_WELCOMEPAGE_TITLE "SQLite ODBC Installation"
!define MUI_WELCOMEPAGE_TEXT "This program will guide you through the \
installation of SQLite ODBC Driver.\r\n\r\n$_CLICK"
!insertmacro MUI_PAGE_WELCOME
!insertmacro MUI_PAGE_LICENSE "license.txt"
!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_COMPONENTS
!insertmacro MUI_PAGE_INSTFILES

!define MUI_FINISHPAGE_TITLE "SQLite ODBC Installation"  
!define MUI_FINISHPAGE_TEXT "The installation of SQLite ODBC Driver is complete.\
\r\n\r\n$_CLICK"

!insertmacro MUI_PAGE_FINISH

!insertmacro MUI_UNPAGE_CONFIRM
!insertmacro MUI_UNPAGE_INSTFILES
 
;--------------------------------
; Language
 
!insertmacro MUI_LANGUAGE "English"
 
;--------------------------------
; Installer Sections

Section "-Main (required)" InstallationInfo
 
; Add files
 SetOutPath "$INSTDIR"
 File "sqliteodbc.dll"
 File "sqliteodbcu.dll"
 File "sqlite3odbc.dll"
; unsupported non-WCHAR driver for SQLite3
 File "sqlite3odbcnw.dll"
 File "sqlite.exe"
 File "sqliteu.exe"
 File "sqlite3.exe"
 File "inst.exe"
 File "instq.exe"
 File "uninst.exe"
 File "uninstq.exe"
 File "adddsn.exe"
 File "remdsn.exe"
 File "addsysdsn.exe"
 File "remsysdsn.exe"
 File "SQLiteODBCInstaller.exe"
; SQLite 3.4.*
; File "sqlite3_mod_fts1.dll"
; File "sqlite3_mod_fts2.dll"
; SQLite 3.5.*
 File "sqlite3_mod_fts3.dll"
 File "sqlite3_mod_blobtoxy.dll"
 File "sqlite3_mod_impexp.dll"
; SQLite 3.6.*
 File "sqlite3_mod_rtree.dll"
 File "sqlite3_mod_extfunc.dll"
 File "license.terms"
 File "license.txt"
 File "README"
 File "readme.txt"
!ifdef WITH_SQLITE_DLLS
 File "sqlite.dll"
 File "sqliteu.dll"
 File "sqlite3.dll"
!endif

; Shortcuts
 SetOutPath "$SMPROGRAMS\${PROD_NAME0}"
 CreateShortCut "$SMPROGRAMS\${PROD_NAME0}\Re-install ODBC Drivers.lnk" \
   "$INSTDIR\inst.exe"
 CreateShortCut "$SMPROGRAMS\${PROD_NAME0}\Remove ODBC Drivers.lnk" \
   "$INSTDIR\uninst.exe"
 CreateShortCut "$SMPROGRAMS\${PROD_NAME0}\Uninstall.lnk" \
   "$INSTDIR\uninstall.exe"
 CreateShortCut "$SMPROGRAMS\${PROD_NAME0}\View README.lnk" \
   "$INSTDIR\readme.txt"
 SetOutPath "$SMPROGRAMS\${PROD_NAME0}\Shells"
 CreateShortCut "$SMPROGRAMS\${PROD_NAME0}\Shells\SQLite 3.lnk" \
   "$INSTDIR\sqlite3.exe"
 CreateShortCut "$SMPROGRAMS\${PROD_NAME0}\Shells\SQLite 2.lnk" \
   "$INSTDIR\sqlite.exe"
 CreateShortCut "$SMPROGRAMS\${PROD_NAME0}\Shells\SQLite 2 (UTF-8).lnk" \
   "$INSTDIR\sqliteu.exe"
 
; Write uninstall information to the registry
 WriteRegStr HKLM \
  "Software\Microsoft\Windows\CurrentVersion\Uninstall\${PROD_NAME0}" \
  "DisplayName" "${PROD_NAME} (remove only)"
 WriteRegStr HKLM \
  "Software\Microsoft\Windows\CurrentVersion\Uninstall\${PROD_NAME0}" \
  "UninstallString" "$INSTDIR\Uninstall.exe"

 SetOutPath "$INSTDIR"
 WriteUninstaller "$INSTDIR\Uninstall.exe"

 ExecWait '"$INSTDIR\instq.exe"'

SectionEnd

!ifdef WITH_SOURCES
Section /o "Source Code" SourceInstall
 SetOutPath "$INSTDIR\source"
 File "source\README"
 File "source\VERSION"
 File "source\ChangeLog"
 File "source\license.terms"
 File "source\header.html"
 File "source\footer.html"
 File "source\stylesheet.css"
 File "source\doxygen.conf"
 File "source\adddsn.c"
 File "source\inst.c"
 File "source\fixup.c"
 File "source\mkopc.c"
 File "source\mkopc3.c"
 File "source\sqliteodbc.c"
 File "source\sqliteodbc.h"
 File "source\sqliteodbc.rc"
 File "source\sqliteodbc.mak"
 File "source\sqliteodbc.def"
 File "source\sqliteodbcu.def"
 File "source\sqliteodbc.spec"
 File "source\sqliteodbc.spec.in"
 File "source\sqlite3odbc.c"
 File "source\sqlite3odbc.h"
 File "source\sqlite3odbc.rc"
 File "source\sqlite3odbc.mak"
 File "source\sqlite3odbc.def"
 File "source\sqlite.mak"
 File "source\sqlite3.mak"
 File "source\resource.h.in"
 File "source\install-sh"
 File "source\Makefile.in"
 File "source\configure.in"
 File "source\config.guess"
 File "source\config.sub"
 File "source\ltmain.sh"
 File "source\libtool"
 File "source\aclocal.m4"
 File "source\configure"
 File "source\sqliteodbcos2.rc"
 File "source\sqliteodbcos2.def"
 File "source\makefile.os2"
 File "source\resourceos2.h"
 File "source\README.OS2"
 File "source\README.ic"
 File "source\drvdsninst.sh"
 File "source\drvdsnuninst.sh"
 File "source\Makefile.mingw-cross"
 File "source\mf-sqlite.mingw-cross"
 File "source\mf-sqlite3.mingw-cross"
 File "source\mf-sqlite3fts.mingw-cross"
 File "source\mf-sqlite3rtree.mingw-cross"
 File "source\mingw-cross-build.sh"
 File "source\sqliteodbc.nsi"
 File "source\SQLiteODBCInstaller.c"
 File "source\blobtoxy.c"
 File "source\blobtoxy.rc"
 File "source\impexp.c"
 File "source\sqlite.ico"
 File "source\sqliteodbc.ico"
 File "source\tcc-0.9.23.patch"
 File "source\tcc-0.9.24.patch"
 File "source\sqlite+tcc.c"
 File "source\strict_typing.sql"
 File "source\README.sqlite+tcc"
 SetOutPath "$INSTDIR\source\missing"
 File "source\missing/ini.h"
 File "source\missing/log.h"
 SetOutPath "$INSTDIR\source\tccex"
 File "source\tccex\README.bench"
 File "source\tccex\obench.c"
 File "source\tccex\sbench.c"
 File "source\tccex\sqlite.c"
 File "source\tccex\sqlite3.c"
 File "source\tccex\samplext.c"
 SetOutPath "$INSTDIR\source\tccex\a10n"
 File "source\tccex\a10n\README.txt"
SectionEnd

!insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
 !insertmacro MUI_DESCRIPTION_TEXT ${SourceInstall} \
   "Source code"
!insertmacro MUI_FUNCTION_DESCRIPTION_END
!endif

Section /o "SQLite+TCC" TccInstall
 SetOutPath "$INSTDIR\TCC"
 File "README.sqlite+tcc"
 File "sqlite+tcc.dll"
 File "strict_typing.sql"
 File "TCC\tcc.exe"
 File "TCC\tiny_impdef.exe"
 File /r "TCC\doc"
 File /r "TCC\include"
 File /r "TCC\lib"
 SetOutPath "$INSTDIR\TCC\samples"
 File "tccex/samplext.c"
 File "tccex/sqlite.c"
 File "tccex/sqlite3.c"
 File "tccex/obench.c"
 File "tccex/sbench.c"
 File "tccex/README.bench"
 SetOutPath "$INSTDIR\TCC\samples\a10n"
 File "sqlite3/sqlite3.c"
; deprecated as of 3.5.1
; File "sqlite3/sqlite3internal.h"
 File "tccex/a10n/README.txt"
SectionEnd

!insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
 !insertmacro MUI_DESCRIPTION_TEXT ${TccInstall} \
   "Experimental combination of SQLite and TinyCC"
!insertmacro MUI_FUNCTION_DESCRIPTION_END

;--------------------------------
; Uninstaller Section

Section "Uninstall"

ExecWait '"$INSTDIR\uninstq.exe"'
 
; Delete Files 
RMDir /r "$INSTDIR\*" 
RMDir /r "$INSTDIR\*.*" 
 
; Remove the installation directory
RMDir /r "$INSTDIR"

; Remove start menu/program files subdirectory

RMDir /r "$SMPROGRAMS\${PROD_NAME0}"
  
; Delete Uninstaller And Unistall Registry Entries
DeleteRegKey HKEY_LOCAL_MACHINE "SOFTWARE\${PROD_NAME0}"
DeleteRegKey HKEY_LOCAL_MACHINE \
    "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\${PROD_NAME0}"
  
SectionEnd
 
;--------------------------------
; EOF
