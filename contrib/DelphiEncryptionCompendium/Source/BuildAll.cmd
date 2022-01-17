@echo off
cls
setlocal enableextensions
setlocal enabledelayedexpansion

echo.
echo Compiles for all Delphis in %ProgramFiles(x86)%\Embarcadero
echo TODO : switch to paths read out of registry
echo HKCU\Software\Embarcadero\BDS\*.0 : RootDir
echo.
echo Compiles as well for Lazarus/FPC in C:\lazarus
echo.
echo creates one directory per DelphiVersion+ProjectConfig with the DCUs 
echo ..\Compiled\DCU_IDE$(ProductVersion)_$(Platform)_$(Config)
echo.

title CLEAR
echo.
echo ##### CLEAR #####
echo. 
echo delete "..\Compiled"
echo.
del "%~dpn0.log"
rd /s /q "%~dp0..\Compiled"

cd /d "%ProgramFiles(x86)%\Embarcadero"
for /r %%X in (rsvars*.bat) do call :run_compiler "%%X"

echo.
title %comspec%
type "%~dpn0.log"
echo.
find /c "FAIL " "%~dpn0.log" >nul
if not errorlevel 1 pause
exit /b


:run_compiler
title COMPILE
echo.
echo ##### %~dp1 #####
echo.

setlocal
call "%~1"
set IDEVER=unknown
for /f "delims=" %%E in ("%BDS%") do set IDEVER=%%~nxE
echo. >> "%~dpn0.log" 
echo ### Delphi %IDEVER% ### >> "%~dpn0.log" 
echo. >> "%~dpn0.log" 

::::: Lazarus-DCUs :::::
title COMPILE Lazarus x86_64 win64 : Source\DEC60Lazarus.lpk
echo ### Lazarus x86_64 win64 # Source\DEC60Lazarus.lpk
C:\lazarus\lazbuild.exe --build-all --cpu=x86_64 --build-mode=Default "%~dp0\DEC60Lazarus.lpk"
if errorlevel 1 (
  echo FAIL   Source\DEC60Lazarus.lpk   : x86_64 win64 >> "%~dpn0.log"
  rundll32 user32.dll,MessageBeep
  timeout 11
) else (
  echo OK     Source\DEC60Lazarus.lpk   : x86_64 win64 >> "%~dpn0.log" 
)
echo.

title COMPILE Lazarus i386 win32 : Source\DEC60Lazarus.lpk
echo ### Lazarus i386 win32 # Source\DEC60Lazarus.lpk
C:\lazarus\lazbuild.exe --build-all --cpu=i386 --build-mode=Default "%~dp0\DEC60Lazarus.lpk"
if errorlevel 1 (
  echo FAIL   Source\DEC60Lazarus.lpk   : i386 win32 >> "%~dpn0.log"
  rundll32 user32.dll,MessageBeep
  timeout 11
) else (
  echo OK     Source\DEC60Lazarus.lpk   : i386 win32 >> "%~dpn0.log" 
)
echo.

::::: Delphi-DCUs :::::
for %%P in (Win32,Win64,Linux64,Android,Android64,iOSDevice64,iOSSimulator,OSX32,OSX64) do (
  for %%C in (Debug,Release) do (
    call :do_compile "Source\DEC60.dproj" %%P %%C
  )
)

::::: TestApps :::::
echo. >> "%~dpn0.log" 
for %%P in (Win32) do (
  for %%C in (Debug,Console) do (
    call :do_compile "Unit Tests\DECDUnitTestSuite.dproj" %%P %%C
  )
  for %%C in (Debug,GUI,MobileGUI,TestInsight) do (
    call :do_compile "Unit Tests\DECDUnitXTestSuite.dproj" %%P %%C
  )
)

::::: DemoApps :::::
echo. >> "%~dpn0.log" 
call :do_compile "Demos\Cipher_Console\Cipher_Console.dproj"
call :do_compile "Demos\Cipher_FMX\Cipher_FMX.dproj"
call :do_compile "Demos\CryptoWorkbench_VCL\CryptoWorkbench_VCL.dproj"
call :do_compile "Demos\Format_Console\Format_Console.dproj"
call :do_compile "Demos\Hash_Console\Hash_Console.dproj"
call :do_compile "Demos\Hash_FMX\Hash_FMX.dproj"
call :do_compile "Demos\Progress_VCL\Progress_VCL.dproj"
call :do_compile "Demos\Random_Console\Random_Console.dproj"

echo. >> "%~dpn0.log" 
title RUN Tests
echo ##### RUN Tests #####
echo.
REM for %%C in (Debug,Console,GUI) do (
REM   for %%P in (Win32) do (
REM     call :do_execute DECDUnitTestSuite.exe %%P %%C
REM     call :do_execute DECDUnitXTestSuite.exe %%P %%C
REM   )
REM )
call :do_execute DECDUnitTestSuite.exe Win32 Console
call :do_execute DECDUnitTestSuite.exe Win32 Debug
call :do_execute DECDUnitXTestSuite.exe Win32 Debug
call :do_execute DECDUnitXTestSuite.exe Win32 GUI

endlocal
exit /b


:do_compile
title COMPILE %IDEVER% %2 %3 : %~1
echo ### %IDEVER% %2 %3 # %~1
set params=
if not "%2" == "" set params=/p:Platform=%2 /p:Config=%3
REM msbuild "%~dp0..\%~1" /t:Rebuild %params%     :: $(ProductVersion) is missing in msbuild, but is present in InlineCompiler of the IDE
msbuild "%~dp0..\%~1" /t:Rebuild %params% /p:ProductVersion=%IDEVER%
if errorlevel 1 (
  echo FAIL   %~1   : %2 %3 >> "%~dpn0.log"
  rundll32 user32.dll,MessageBeep
  timeout 11
) else (
  echo OK     %~1   : %2 %3 >> "%~dpn0.log" 
)
:: remove dir if empty
if not "%2" == "" (
  rd /q "%~dp0..\Compiled\BIN_IDE%IDEVER%_%2_%3" >nul
  rd /q "%~dp0..\Compiled\DCP_IDE%IDEVER%_%2_%3" >nul
  rd /q "%~dp0..\Compiled\DCU_IDE%IDEVER%_%2_%3" >nul
)
echo.
exit /b


:do_execute
title EXECUTE %IDEVER% %2 %3 : %~1
echo ### %IDEVER% %2 %3 # %~1
"%~dp0..\Compiled\BIN_IDE%IDEVER%_%2_%3\%~1"
set "ERR=%ERRORLEVEL%     "
echo RUN:%ERR:~0,6%  %~1   : %2 %3 >> "%~dpn0.log"
echo EXITCODE:%ERR%
echo.
exit /b