@echo off

SETLOCAL

SET DIR=%CD%
SET BORLAND=C:\Borland
SET CODEGEAR=C:\CodeGear
SET EMBT=C:\Program Files (x86)\Embarcadero
SET OLDPATH=%PATH%
SET ISCC="C:\Program Files (x86)\Inno Setup 5\ISCC.exe"
SET ZIP="C:\Program Files\7-Zip\7z.exe"

if exist "%EMBT%\RAD Studio\8.0\Bin\brcc32.exe"   "%EMBT%\RAD Studio\8.0\Bin\brcc32.exe" Version.rc -foVersion.res

if not exist "%DIR%\bin" md "%DIR%\bin"
if not exist "%DIR%\lib" md "%DIR%\lib"

::goto Setup
echo === Delphi 11 Alexandria =======================

SET PATH=%EMBT%\Studio\22.0\Bin;%OLDPATH%
call rsvars.bat
cd source
msbuild /t:Clean DfmCheckD11.dproj
msbuild /p:Configuration=Release DfmCheckD11.dproj

echo === Delphi 10.4 Sydney =======================

SET PATH=%EMBT%\Studio\21.0\Bin;%OLDPATH%
call rsvars.bat
cd source
msbuild /t:Clean DfmCheckD104.dproj
msbuild /p:Configuration=Release DfmCheckD104.dproj


echo === Delphi 10.3 Rio =======================

SET PATH=%EMBT%\Studio\20.0\Bin;%OLDPATH%
call rsvars.bat
cd source
msbuild /t:Clean DfmCheckD103.dproj
msbuild /p:Configuration=Release DfmCheckD103.dproj


echo === Delphi 10.2 Tokyo =======================

SET PATH=%EMBT%\Studio\19.0\Bin;%OLDPATH%
call rsvars.bat
cd source
msbuild /t:Clean DfmCheckD102.dproj
msbuild /p:Configuration=Release DfmCheckD102.dproj


echo === Delphi 10.1 Berlin =======================

SET PATH=%EMBT%\Studio\18.0\Bin;%OLDPATH%
call rsvars.bat
cd source
msbuild /t:Clean DfmCheckD101.dproj
msbuild /p:Configuration=Release DfmCheckD101.dproj


echo === Delphi 10 Seattle =======================

SET PATH=%EMBT%\Studio\17.0\Bin;%OLDPATH%
call rsvars.bat
cd source
msbuild /t:Clean DfmCheckD10.dproj
msbuild /p:Configuration=Release DfmCheckD10.dproj


echo === Delphi XE8 ==============================

SET PATH=%EMBT%\Studio\16.0\Bin;%OLDPATH%
call rsvars.bat
cd source
msbuild /t:Clean DfmCheckXE8.dproj
msbuild /p:Configuration=Release DfmCheckXE8.dproj


echo === Delphi XE7 ==============================

SET PATH=%EMBT%\Studio\15.0\Bin;%OLDPATH%
call rsvars.bat
cd source
msbuild /t:Clean DfmCheckXE7.dproj
msbuild /p:Configuration=Release DfmCheckXE7.dproj


echo === Delphi XE6 ==============================

SET PATH=%EMBT%\Studio\14.0\Bin;%OLDPATH%
call rsvars.bat
cd source
msbuild /t:Clean DfmCheckXE6.dproj
msbuild /p:Configuration=Release DfmCheckXE6.dproj


echo === Delphi XE5 ==============================

SET PATH=%EMBT%\RAD Studio\12.0\Bin;%OLDPATH%
call rsvars.bat
cd source
msbuild /t:Clean DfmCheckXE5.dproj
msbuild /p:Configuration=Release DfmCheckXE5.dproj


echo === Delphi XE4 ==============================

SET PATH=%EMBT%\RAD Studio\11.0\Bin;%OLDPATH%
call rsvars.bat
cd source
msbuild /t:Clean DfmCheckXE4.dproj
msbuild /p:Configuration=Release DfmCheckXE4.dproj


echo === Delphi XE3 ==============================

SET PATH=%EMBT%\RAD Studio\10.0\Bin;%OLDPATH%
call rsvars.bat
cd source
msbuild /t:Clean DfmCheckXE3.dproj
msbuild /p:Configuration=Release DfmCheckXE3.dproj


echo === Delphi XE2 ==============================

SET PATH=%EMBT%\RAD Studio\9.0\Bin;%OLDPATH%
call rsvars.bat
cd source
msbuild /t:Clean DfmCheckXE2.dproj
msbuild /p:Configuration=Release DfmCheckXE2.dproj


echo === Delphi XE ==============================

SET PATH=%EMBT%\RAD Studio\8.0\Bin;%OLDPATH%
call rsvars.bat
cd source
msbuild /t:Clean DfmCheckXE.dproj
msbuild /p:Configuration=Release DfmCheckXE.dproj

cd console
msbuild /t:Clean DfmCheck.dproj
msbuild /p:Configuration=Release /p:LANGDIR=EN DfmCheck.dproj
if ERRORLEVEL 1 goto Error1
cd ..
cd ..

echo === Delphi 2010 ==============================

SET PATH=%EMBT%\RAD Studio\7.0\Bin;%OLDPATH%
call rsvars.bat
cd source
msbuild /t:Clean DfmCheck2010.dproj
msbuild /p:Configuration=Release DfmCheck2010.dproj
if ERRORLEVEL 1 goto Error1
cd ..

echo === Delphi 2009 ==============================

SET PATH=%CodeGear%\RAD Studio\6.0\Bin;%OLDPATH%
call rsvars.bat
cd source
msbuild /t:Clean DfmCheck2009.dproj
msbuild /p:Configuration=Release DfmCheck2009.dproj
if ERRORLEVEL 1 goto Error1
cd ..

echo === Delphi 11 ==============================

SET PATH=%CodeGear%\RAD Studio\5.0\Bin;%OLDPATH%
cd source
dcc32 -B -Q DFMCheck.dpk
if ERRORLEVEL 1 goto Error1
cd ..

echo === Delphi 10 ==============================

SET PATH=%Borland%\BDS\4.0\Bin;%OLDPATH%
cd source
dcc32 -B -Q DFMCheck.dpk
if ERRORLEVEL 1 goto Error1
cd ..

echo === Delphi 9 ==============================

SET PATH=%Borland%\BDS\3.0\Bin;%OLDPATH%
cd source
dcc32 -B -Q DFMCheck.dpk
if ERRORLEVEL 1 goto Error1
cd ..

echo === Delphi 7 ==============================

SET PATH=%Borland%\Delphi7\Bin;%OLDPATH%
cd source
dcc32 -B -Q -U"%Borland%\Delphi7\Lib" DFMCheck.dpk
if ERRORLEVEL 1 goto Error1
cd ..

echo === Delphi 6 ==============================

SET PATH=%Borland%\Delphi6\Bin
cd source
dcc32 -B -Q DFMCheck.dpk
if ERRORLEVEL 1 goto Error1
cd ..

echo === Delphi 5 ==============================

SET PATH=%Borland%\Delphi5\Bin
cd source
dcc32 -B -Q DFMCheck5.dpk
if ERRORLEVEL 1 goto Error1
cd ..

echo === Installer =============================
:Setup
if not exist "Installer" goto Leave
cd Installer
%ISCC% DfmCheck.iss
if ERRORLEVEL 1 goto Error1
cd ..

:: ===========================================
goto Leave
:Error2
cd ..
:Error1
cd ..
:Error0
pause


:Leave
cd /d "%DIR%"

SET PATH=%OLDPATH%
SET OLDPATH=
SET BORLAND=
SET CODEGEAR=
SET EMBT=
SET DIR=

ENDLOCAL
