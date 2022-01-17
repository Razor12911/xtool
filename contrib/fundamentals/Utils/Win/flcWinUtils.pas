{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals 5.00                                        }
{   File name:        flcWinUtils.pas                                          }
{   File version:     5.14                                                     }
{   Description:      MS Windows utility functions                             }
{                                                                              }
{   Copyright:        Copyright (c) 2000-2019, David J Butler                  }
{                     All rights reserved.                                     }
{                     Redistribution and use in source and binary forms, with  }
{                     or without modification, are permitted provided that     }
{                     the following conditions are met:                        }
{                     Redistributions of source code must retain the above     }
{                     copyright notice, this list of conditions and the        }
{                     following disclaimer.                                    }
{                     THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND   }
{                     CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED          }
{                     WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED   }
{                     WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A          }
{                     PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL     }
{                     THE REGENTS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,    }
{                     INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR             }
{                     CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,    }
{                     PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF     }
{                     USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)         }
{                     HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER   }
{                     IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING        }
{                     NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE   }
{                     USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE             }
{                     POSSIBILITY OF SUCH DAMAGE.                              }
{                                                                              }
{   Github:           https://github.com/fundamentalslib                       }
{   E-mail:           fundamentals.library at gmail.com                        }
{                                                                              }
{ Revision history:                                                            }
{                                                                              }
{   2000/10/01  1.01  Initial version created from cUtils.                     }
{   2002/03/15  2.02  Added GetWinOSType.                                      }
{   2002/06/26  3.03  Refactored for Fundamentals 3.                           }
{   2002/09/22  3.04  Refactored registry functions.                           }
{   2002/12/08  3.05  Improvements to registry functions.                      }
{   2003/01/04  3.06  Added Reboot function.                                   }
{   2003/10/01  3.07  Updated GetWindowsVersion function.                      }
{   2005/08/26  4.08  Split unit into cWinUtils and cWinClasses.               }
{   2005/09/29  4.09  Revised for Fundamentals 4.                              }
{   2009/03/27  4.10  Updates for Delphi 2009.                                 }
{   2016/01/21  4.11  String changes.                                          }
{   2016/01/22  5.12  Revised for Fundamentals 5.                              }
{   2018/08/12  5.13  String type changes.                                     }
{   2019/03/22  5.14  Changes for FreePascal 3.0.4.                            }
{                                                                              }
{ Supported compilers:                                                         }
{                                                                              }
{   Delphi XE7 Win32                    5.12  2016/01/21                       }
{   Delphi XE7 Win64                    5.12  2016/01/21                       }
{                                                                              }
{******************************************************************************}

{$INCLUDE ..\..\flcInclude.inc}

{$IFDEF FREEPASCAL}
  {$WARNINGS OFF}
  {$HINTS OFF}
{$ENDIF}

unit flcWinUtils;

interface

uses
  { System }
  Windows,
  SysUtils,

  { Fundamentals }
  flcStdTypes,
  flcUtils;



{                                                                              }
{ Errors                                                                       }
{                                                                              }
{$IFDEF DELPHI5}
type
  EOSError = class(EWin32Error);
{$ENDIF}

function  GetLastWinError: LongWord;

function  WinErrorMessageA(const ErrorCode: LongWord): AnsiString;
function  WinErrorMessageU(const ErrorCode: LongWord): UnicodeString;
function  WinErrorMessage(const ErrorCode: LongWord): String;

function  GetLastWinErrorMessageA: AnsiString;
function  GetLastWinErrorMessageU: UnicodeString;
function  GetLastWinErrorMessage: String;

procedure RaiseWinError(const ErrorCode: LongWord); {$IFDEF UseInline}inline;{$ENDIF}
procedure RaiseLastWinError; {$IFDEF UseInline}inline;{$ENDIF}



{                                                                              }
{ Environment variables                                                        }
{                                                                              }
function  GetEnvironmentVariableA(const Name: AnsiString): AnsiString;
function  GetEnvironmentVariableU(const Name: UnicodeString): UnicodeString;
function  GetEnvironmentVariable(const Name: String): String; {$IFDEF UseInline}inline;{$ENDIF}

function  GetEnvironmentStringsA: AnsiStringArray;
function  GetEnvironmentStringsU: UnicodeStringArray;
function  GetEnvironmentStrings: StringArray; {$IFDEF UseInline}inline;{$ENDIF}



{                                                                              }
{ Registry                                                                     }
{                                                                              }

{ SplitRegName                                                                 }
procedure SplitRegNameA(const Name: AnsiString; var Key, ValueName: AnsiString);
procedure SplitRegNameU(const Name: UnicodeString; var Key, ValueName: UnicodeString);
procedure SplitRegName(const Name: String; var Key, ValueName: String); {$IFDEF UseInline}inline;{$ENDIF}

{ Exists                                                                       }
function  RegKeyExistsA(const RootKey: HKEY; const Key: AnsiString): Boolean;
function  RegKeyExistsU(const RootKey: HKEY; const Key: UnicodeString): Boolean;
function  RegKeyExists(const RootKey: HKEY; const Key: String): Boolean; {$IFDEF UseInline}inline;{$ENDIF}

function  RegValueExistsA(const RootKey: HKEY; const Key, Name: AnsiString): Boolean;
function  RegValueExistsU(const RootKey: HKEY; const Key, Name: UnicodeString): Boolean;
function  RegValueExists(const RootKey: HKEY; const Key, Name: String): Boolean; {$IFDEF UseInline}inline;{$ENDIF}

{ Set                                                                          }
function  RegSetValueA(const RootKey: HKEY; const Key, Name: AnsiString;
          const ValueType: Cardinal; const Value: Pointer;
          const ValueSize: Integer): Boolean; overload;
function  RegSetValueU(const RootKey: HKEY; const Key, Name: UnicodeString;
          const ValueType: Cardinal; const Value: Pointer;
          const ValueSize: Integer): Boolean; overload;
function  RegSetValue(const RootKey: HKEY; const Key, Name: String;
          const ValueType: Cardinal; const Value: Pointer;
          const ValueSize: Integer): Boolean; overload; {$IFDEF UseInline}inline;{$ENDIF}

function  RegSetValueA(const RootKey: HKEY; const Name: AnsiString;
          const ValueType: Cardinal; const Value: Pointer;
          const ValueSize: Integer): Boolean; overload;
function  RegSetValueU(const RootKey: HKEY; const Name: UnicodeString;
          const ValueType: Cardinal; const Value: Pointer;
          const ValueSize: Integer): Boolean; overload;
function  RegSetValue(const RootKey: HKEY; const Name: String;
          const ValueType: Cardinal; const Value: Pointer;
          const ValueSize: Integer): Boolean; overload; {$IFDEF UseInline}inline;{$ENDIF}

function  SetRegistryStringA(const RootKey: HKEY; const Key: AnsiString;
          const Name: AnsiString; const Value: AnsiString): Boolean; overload;
function  SetRegistryStringU(const RootKey: HKEY; const Key: UnicodeString;
          const Name: UnicodeString; const Value: UnicodeString): Boolean; overload;
function  SetRegistryString(const RootKey: HKEY; const Key: String;
          const Name: String; const Value: String): Boolean; overload; {$IFDEF UseInline}inline;{$ENDIF}

function  SetRegistryStringA(const RootKey: HKEY; const Name: AnsiString;
          const Value: AnsiString): Boolean; overload;
function  SetRegistryStringU(const RootKey: HKEY; const Name: UnicodeString;
          const Value: UnicodeString): Boolean; overload;
function  SetRegistryString(const RootKey: HKEY; const Name: String;
          const Value: String): Boolean; overload; {$IFDEF UseInline}inline;{$ENDIF}

function  SetRegistryDWordA(const RootKey: HKEY; const Name: AnsiString;
          const Value: LongWord): Boolean;
function  SetRegistryDWordU(const RootKey: HKEY; const Name: UnicodeString;
          const Value: LongWord): Boolean;
function  SetRegistryDWord(const RootKey: HKEY; const Name: String;
          const Value: LongWord): Boolean; {$IFDEF UseInline}inline;{$ENDIF}

function  SetRegistryBinaryA(const RootKey: HKEY; const Name: AnsiString;
          const Value; const ValueSize: Integer): Boolean;

{ Get                                                                          }
function  RegGetValueA(
          const RootKey: HKEY; const Key, Name: AnsiString;
          const ValueType: Cardinal;
          var RegValueType: Cardinal;
          var ValueBuf: Pointer; var ValueSize: Integer): Boolean; overload;
function  RegGetValueU(
          const RootKey: HKEY; const Key, Name: UnicodeString;
          const ValueType: Cardinal;
          var RegValueType: Cardinal;
          var ValueBuf: Pointer; var ValueSize: Integer): Boolean; overload;
function  RegGetValue(
          const RootKey: HKEY; const Key, Name: String;
          const ValueType: Cardinal;
          var RegValueType: Cardinal;
          var ValueBuf: Pointer; var ValueSize: Integer): Boolean; overload; {$IFDEF UseInline}inline;{$ENDIF}

function  RegGetValueA(
          const RootKey: HKEY; const Name: AnsiString;
          const ValueType: Cardinal; var RegValueType: Cardinal;
          var ValueBuf: Pointer; var ValueSize: Integer): Boolean; overload;

function  GetRegistryStringA(const RootKey: HKEY; const Key, Name: AnsiString): AnsiString; overload;
function  GetRegistryStringU(const RootKey: HKEY; const Key, Name: UnicodeString): UnicodeString; overload;
function  GetRegistryString(const RootKey: HKEY; const Key, Name: String): String; overload; {$IFDEF UseInline}inline;{$ENDIF}

function  GetRegistryStringA(const RootKey: HKEY; const Name: AnsiString): AnsiString; overload;
function  GetRegistryStringU(const RootKey: HKEY; const Name: UnicodeString): UnicodeString; overload;
function  GetRegistryString(const RootKey: HKEY; const Name: String): String; overload; {$IFDEF UseInline}inline;{$ENDIF}

function  GetRegistryDWordA(const RootKey: HKEY; const Key, Name: AnsiString): LongWord;
function  GetRegistryDWordU(const RootKey: HKEY; const Key, Name: UnicodeString): LongWord;
function  GetRegistryDWord(const RootKey: HKEY; const Key, Name: String): LongWord; {$IFDEF UseInline}inline;{$ENDIF}

{ Delete                                                                       }
function  DeleteRegistryValueA(const RootKey: HKEY; const Key, Name: AnsiString): Boolean;
function  DeleteRegistryValueU(const RootKey: HKEY; const Key, Name: UnicodeString): Boolean;
function  DeleteRegistryValue(const RootKey: HKEY; const Key, Name: String): Boolean; {$IFDEF UseInline}inline;{$ENDIF}

function  DeleteRegistryKeyA(const RootKey: HKEY; const Key: AnsiString): Boolean;
function  DeleteRegistryKeyU(const RootKey: HKEY; const Key: UnicodeString): Boolean;
function  DeleteRegistryKey(const RootKey: HKEY; const Key: String): Boolean; {$IFDEF UseInline}inline;{$ENDIF}

{ Remote Registries                                                            }
function  ConnectRegistryA(const MachineName: AnsiString; const RootKey: HKEY;
          var RemoteKey: HKEY): Boolean;
function  ConnectRegistryU(const MachineName: UnicodeString; const RootKey: HKEY;
          var RemoteKey: HKEY): Boolean;

function  DisconnectRegistry(const RemoteKey: HKEY): Boolean;

{ Enumerate                                                                    }
function  EnumRegistryValuesA(const RootKey: HKEY; const Name: AnsiString;
          var ValueList: AnsiStringArray): Boolean;
function  EnumRegistryValuesU(const RootKey: HKEY; const Name: UnicodeString;
          var ValueList: UnicodeStringArray): Boolean;
function  EnumRegistryValues(const RootKey: HKEY; const Name: String;
          var ValueList: StringArray): Boolean; {$IFDEF UseInline}inline;{$ENDIF}

function  EnumRegistryKeysA(const RootKey: HKEY; const Name: AnsiString;
          var KeyList: AnsiStringArray): Boolean;
function  EnumRegistryKeysU(const RootKey: HKEY; const Name: UnicodeString;
          var KeyList: UnicodeStringArray): Boolean;
function  EnumRegistryKeys(const RootKey: HKEY; const Name: String;
          var KeyList: StringArray): Boolean; {$IFDEF UseInline}inline;{$ENDIF}



{                                                                              }
{ Windows Version                                                              }
{                                                                              }
type
  TWindowsVersion = (
      // 16-bit Windows
      Win16_31,
      // 32-bit Windows
      Win32_95, Win32_95R2, Win32_98, Win32_98SE, Win32_ME, Win32_Future,
      // Windows NT (Pre 3)
      WinNT_Pre3,
      // Windows NT 3
      WinNT_31, WinNT_35, WinNT_351,
      // Windows NT 4
      WinNT_40,
      // Windows NT 5 - 2000/XP/2003
      Win_2000, Win_XP, Win_2003, WinNT5_Future,
      // Windows NT 6 - Vista/7/8/8.1
      Win_Vista, Win_7, Win_8, Win_81, WinNT6_Future,
      // Windows NT 10 - 10
      Win_10, WinNT10_Future,
      // Windows NT 11+
      WinNT_Future,
      // Windows Post-NT
      Win_Future);
  TWindowsVersions = set of TWindowsVersion;

function  GetWindowsVersion: TWindowsVersion;

function  GetWindowsVersionString: String;

function  IsWinPlatform95: Boolean;
function  IsWinPlatformNT: Boolean;

function  GetWindowsProductIDA: AnsiString;
function  GetWindowsProductIDU: UnicodeString;
function  GetWindowsProductID: String; {$IFDEF UseInline}inline;{$ENDIF}

function  GetWindowsProductNameA: AnsiString;
function  GetWindowsProductNameU: UnicodeString;
function  GetWindowsProductName: String; {$IFDEF UseInline}inline;{$ENDIF}



{                                                                              }
{ Windows Paths                                                                }
{                                                                              }
procedure EnsurePathSuffixA(var Path: AnsiString);
procedure EnsurePathSuffixU(var Path: UnicodeString);
procedure EnsurePathSuffix(var Path: String);

function  GetWindowsTemporaryPathA: AnsiString;
function  GetWindowsTemporaryPathU: UnicodeString;
function  GetWindowsTemporaryPath: String; {$IFDEF UseInline}inline;{$ENDIF}

function  GetWindowsPathA: AnsiString;
function  GetWindowsPathU: UnicodeString;
function  GetWindowsPath: String; {$IFDEF UseInline}inline;{$ENDIF}

function  GetWindowsSystemPathA: AnsiString;
function  GetWindowsSystemPathU: UnicodeString;
function  GetWindowsSystemPath: String; {$IFDEF UseInline}inline;{$ENDIF}

function  GetProgramFilesPathA: AnsiString;
function  GetProgramFilesPathU: UnicodeString;
function  GetProgramFilesPath: String;

function  GetCommonFilesPathA: AnsiString;
function  GetCommonFilesPathU: UnicodeString;
function  GetCommonFilesPath: String;

function  GetApplicationFileNameA: AnsiString;
function  GetApplicationFileNameU: UnicodeString;
function  GetApplicationFileName: String;

function  GetApplicationPath: String;

function  GetHomePathA: AnsiString;
function  GetHomePathU: UnicodeString;
function  GetHomePath: String; {$IFDEF UseInline}inline;{$ENDIF}

function  GetLocalAppDataPathA: AnsiString;
function  GetLocalAppDataPathU: UnicodeString;
function  GetLocalAppDataPath: String; {$IFDEF UseInline}inline;{$ENDIF}



{                                                                              }
{ Identification                                                               }
{                                                                              }
function  GetUserNameA: AnsiString;
function  GetUserNameU: UnicodeString;
function  GetUserName: String; {$IFDEF UseInline}inline;{$ENDIF}

function  GetLocalComputerNameA: AnsiString;
function  GetLocalComputerNameU: UnicodeString;
function  GetLocalComputerName: String; {$IFDEF UseInline}inline;{$ENDIF}



{                                                                              }
{ Application Version Info                                                     }
{                                                                              }
type
  TVersionInfo = (viFileVersion, viFileDescription, viLegalCopyright,
                  viComments, viCompanyName, viInternalName,
                  viLegalTrademarks, viOriginalFilename, viProductName,
                  viProductVersion);

function  GetAppVersionInfoA(const VersionInfo: TVersionInfo): AnsiString;
function  GetAppVersionInfoU(const VersionInfo: TVersionInfo): UnicodeString;
function  GetAppVersionInfo(const VersionInfo: TVersionInfo): String; {$IFDEF UseInline}inline;{$ENDIF}



{                                                                              }
{ Windows Processes                                                            }
{                                                                              }
function  WinExecuteA(const ExeName, Params: AnsiString;
          const ShowWin: Word = SW_SHOWNORMAL;
          const WaitTime: Integer = -1;
          const DefaultPath: AnsiString = ''): LongWord;
function  WinExecuteU(const ExeName, Params: UnicodeString;
          const ShowWin: Word = SW_SHOWNORMAL;
          const WaitTime: Integer = -1;
          const DefaultPath: UnicodeString = ''): LongWord;
function  WinExecute(const ExeName, Params: String;
          const ShowWin: Word = SW_SHOWNORMAL;
          const WaitTime: Integer = -1;
          const DefaultPath: String = ''): LongWord; {$IFDEF UseInline}inline;{$ENDIF}



{                                                                              }
{ Dynamic library                                                              }
{                                                                              }
type
  TLibraryHandle = Cardinal;

function  LoadLibraryA(const LibraryName: AnsiString): TLibraryHandle; overload;
function  LoadLibraryA(const LibraryName: array of AnsiString): TLibraryHandle; overload;
function  LoadLibraryU(const LibraryName: UnicodeString): TLibraryHandle;
function  LoadLibrary(const LibraryName: String): TLibraryHandle; {$IFDEF UseInline}inline;{$ENDIF}

function  GetProcAddressA(const Handle: TLibraryHandle; const ProcName: AnsiString): Pointer;
{$IFNDEF FREEPASCAL}
function  GetProcAddressU(const Handle: TLibraryHandle; const ProcName: UnicodeString): Pointer;
{$ENDIF}
function  GetProcAddress(const Handle: TLibraryHandle; const ProcName: String): Pointer; {$IFDEF UseInline}inline;{$ENDIF}

procedure FreeLibrary(const Handle: TLibraryHandle);

type
  TDynamicLibrary = class
  protected
    FHandle : TLibraryHandle;

    function  GetProcAddressA(const ProcName: AnsiString): Pointer;
    function  GetProcAddressU(const ProcName: UnicodeString): Pointer;

  public
    constructor CreateA(const LibraryName: AnsiString); overload;
    constructor CreateA(const LibraryName: array of AnsiString); overload;

    constructor CreateU(const LibraryName: UnicodeString);

    destructor  Destroy; override;

    property  Handle: TLibraryHandle read FHandle;

    property  ProcAddressA[const ProcName: AnsiString]: Pointer read GetProcAddressA; default;
    property  ProcAddressU[const ProcName: UnicodeString]: Pointer read GetProcAddressU;
  end;



{                                                                              }
{ Exit Windows                                                                 }
{                                                                              }
type
  TExitWindowsType = (exitLogOff, exitPowerOff, exitReboot, exitShutDown);

function  ExitWindows(const ExitType: TExitWindowsType;
          const Force: Boolean = False): Boolean;

function  LogOff(const Force: Boolean = False): Boolean;
function  PowerOff(const Force: Boolean = False): Boolean;
function  Reboot(const Force: Boolean = False): Boolean;
function  ShutDown(const Force: Boolean = False): Boolean;



{                                                                              }
{ Locale information                                                           }
{                                                                              }
function  GetCountryCode1A: AnsiString;
function  GetCountryCode1U: UnicodeString;
function  GetCountryCode1: String; {$IFDEF UseInline}inline;{$ENDIF}

function  GetCountryCode2A: AnsiString;
function  GetCountryCode2U: UnicodeString;
function  GetCountryCode2: String; {$IFDEF UseInline}inline;{$ENDIF}

function  GetCountryNameA: AnsiString;
function  GetCountryNameU: UnicodeString;
function  GetCountryName: String; {$IFDEF UseInline}inline;{$ENDIF}



{                                                                              }
{ Miscelleaneous Windows API                                                   }
{                                                                              }
function  ContentTypeFromExtentionA(const Extention: AnsiString): AnsiString;
function  ContentTypeFromExtentionU(const Extention: UnicodeString): UnicodeString;
function  ContentTypeFromExtention(const Extention: String): String; {$IFDEF UseInline}inline;{$ENDIF}

function  FileClassFromExtentionA(const Extention: AnsiString): AnsiString;

function  GetFileClassA(const FileName: AnsiString): AnsiString;

function  GetFileAssociationA(const FileName: AnsiString): AnsiString;

function  IsApplicationAutoRunA(const Name: AnsiString): Boolean;

procedure SetApplicationAutoRunA(const Name: AnsiString; const AutoRun: Boolean);

function  GetKeyPressed(const VKeyCode: Integer): Boolean;

function  GetHardDiskSerialNumberA(const DriveLetter: AnsiChar): AnsiString;
function  GetHardDiskSerialNumberU(const DriveLetter: WideChar): UnicodeString;
function  GetHardDiskSerialNumber(const DriveLetter: Char): String;



{                                                                              }
{ Windows Fibers API                                                           }
{                                                                              }
type
  TFNFiberStartRoutine = TFarProc;

function  ConvertThreadToFiber(lpParameter: Pointer): Pointer; stdcall;
function  CreateFiber(dwStackSize: DWORD; lpStartAddress: TFNFiberStartRoutine;
          lpParameter: Pointer): Pointer; stdcall;



{                                                                              }
{ Windows Shell API                                                            }
{                                                                              }
function  ShellExecuteA(hWnd: HWND; Operation, FileName, Parameters,
          Directory: PAnsiChar; ShowCmd: Integer): HINST; stdcall;

procedure ShellLaunch(const S: AnsiString);



{                                                                              }
{ WinSpool API                                                                 }
{                                                                              }
function  EnumPortsA(pName: PAnsiChar; Level: DWORD; pPorts: Pointer; cbBuf: DWORD;
          var pcbNeeded, pcReturned: DWORD): BOOL; stdcall;

function  GetWinPortNamesA: AnsiStringArray;



{                                                                              }
{ Timers                                                                       }
{                                                                              }
function  GetMsCount: LongWord;
function  GetUsCount: Int64;



{                                                                              }
{ Tests                                                                        }
{                                                                              }
{$IFDEF DEBUG}
{$IFDEF TEST}
procedure Test;
{$ENDIF}
{$ENDIF}



implementation

uses
  { Fundamentals }
  flcDynArrays,
  flcStrings,
  flcZeroTermStrings,
  flcFileUtils;



resourcestring
  SInvalidParameter = 'Invalid parameter';
  SProcessTimedOut  = 'Process timed out';



{                                                                              }
{ Errors                                                                       }
{                                                                              }
function GetLastWinError: LongWord;
begin
  Result := Windows.GetLastError;
end;

const
  MAX_ERRORMESSAGE_LENGTH = 512;

function WinErrorMessageA(const ErrorCode: LongWord): AnsiString;
var Buf: array[0..MAX_ERRORMESSAGE_LENGTH + 1] of AnsiChar;
    Len: LongWord;
begin
  FillChar(Buf, Sizeof(Buf), 0);
  Len := Windows.FormatMessageA(FORMAT_MESSAGE_FROM_SYSTEM, nil, ErrorCode, 0,
      @Buf, MAX_ERRORMESSAGE_LENGTH, nil);
  if Len = 0 then
    Result := 'WindowsError#' + IntToStringA(ErrorCode)
  else
    Result := StrZPasA(PAnsiChar(@Buf));
end;

function WinErrorMessageU(const ErrorCode: LongWord): UnicodeString;
var Buf: array[0..MAX_ERRORMESSAGE_LENGTH + 1] of WideChar;
    Len: LongWord;
begin
  FillChar(Buf, Sizeof(Buf), 0);
  Len := Windows.FormatMessageW(FORMAT_MESSAGE_FROM_SYSTEM, nil, ErrorCode, 0,
      @Buf, MAX_ERRORMESSAGE_LENGTH, nil);
  if Len = 0 then
    Result := 'WindowsError#' + IntToStringU(ErrorCode)
  else
    Result := StrZPasU(PWideChar(@Buf));
end;

function WinErrorMessage(const ErrorCode: LongWord): String;
var Buf: array[0..MAX_ERRORMESSAGE_LENGTH + 1] of Char;
    Len: LongWord;
begin
  FillChar(Buf, Sizeof(Buf), 0);
  {$IFDEF StringIsUnicode}
  Len := Windows.FormatMessageW(FORMAT_MESSAGE_FROM_SYSTEM, nil, ErrorCode, 0,
      @Buf, MAX_ERRORMESSAGE_LENGTH, nil);
  {$ELSE}
  Len := Windows.FormatMessageA(FORMAT_MESSAGE_FROM_SYSTEM, nil, ErrorCode, 0,
      @Buf, MAX_ERRORMESSAGE_LENGTH, nil);
  {$ENDIF}
  if Len = 0 then
    Result := 'WindowsError#' + IntToStr(ErrorCode)
  else
    Result := StrPas(PChar(@Buf));
end;

function GetLastWinErrorMessageA: AnsiString;
begin
  Result := WinErrorMessageA(GetLastWinError);
end;

function GetLastWinErrorMessageU: UnicodeString;
begin
  Result := WinErrorMessageU(GetLastWinError);
end;

function GetLastWinErrorMessage: String;
begin
  Result := WinErrorMessage(GetLastWinError);
end;

procedure RaiseWinError(const ErrorCode: LongWord);
begin
  raise EOSError.Create(WinErrorMessage(ErrorCode));
end;

procedure RaiseLastWinError;
begin
  raise EOSError.Create(GetLastWinErrorMessage);
end;



{                                                                              }
{ Environment variables                                                        }
{                                                                              }
const
  MAX_ENVIRONMENTVARIABLE_LEN = 16384;

function GetEnvironmentVariableA(const Name: AnsiString): AnsiString;
var Buf: array[0..MAX_ENVIRONMENTVARIABLE_LEN] of AnsiChar;
begin
  FillChar(Buf, Sizeof(Buf), 0);
  Windows.GetEnvironmentVariableA(PAnsiChar(Name), @Buf[0], MAX_ENVIRONMENTVARIABLE_LEN);
  Result := StrZPasA(@Buf[0]);
end;

function GetEnvironmentVariableU(const Name: UnicodeString): UnicodeString;
var Buf: array[0..MAX_ENVIRONMENTVARIABLE_LEN] of WideChar;
begin
  FillChar(Buf, Sizeof(Buf), 0);
  Windows.GetEnvironmentVariableW(PWideChar(Name), @Buf[0], MAX_ENVIRONMENTVARIABLE_LEN);
  Result := StrZPasU(@Buf[0]);
end;

function GetEnvironmentVariable(const Name: String): String;
begin
  {$IFDEF StringIsUnicode}
  Result := GetEnvironmentVariableU(Name);
  {$ELSE}
  Result := GetEnvironmentVariableA(Name);
  {$ENDIF}
end;

function GetEnvironmentStringsA: AnsiStringArray;
var P, Q, H : PAnsiChar;
    I : Integer;
    S : AnsiString;
begin
  H := PAnsiChar(Windows.GetEnvironmentStringsA);
  try
    P := H;
    if P^ <> #0 then
      repeat
        Q := P;
        I := 0;
        while Q^ <> #0 do
          begin
            Inc(Q);
            Inc(I);
          end;
        SetLength(S, I);
        if I > 0 then
          MoveMem(P^, PAnsiChar(S)^, I);
        DynArrayAppendA(Result, S);
        P := Q;
        Inc(P);
      until P^ = #0;
  finally
    FreeEnvironmentStringsA(H);
  end;
end;

function GetEnvironmentStringsU: UnicodeStringArray;
var P, Q, H : PWideChar;
    I : Integer;
    S : UnicodeString;
begin
  H := PWideChar(Windows.GetEnvironmentStringsW);
  try
    P := H;
    if P^ <> #0 then
      repeat
        Q := P;
        I := 0;
        while Q^ <> #0 do
          begin
            Inc(Q);
            Inc(I);
          end;
        SetLength(S, I);
        if I > 0 then
          MoveMem(P^, PWideChar(S)^, I * SizeOf(WideChar));
        DynArrayAppendU(Result, S);
        P := Q;
        Inc(P);
      until P^ = #0;
  finally
    FreeEnvironmentStringsW(H);
  end;
end;

function GetEnvironmentStrings: StringArray;
begin
  {$IFDEF StringIsUnicode}
  Result := StringArray(GetEnvironmentStringsU);
  {$ELSE}
  Result := StringArray(GetEnvironmentStringsA);
  {$ENDIF}
end;



{                                                                              }
{ Registry                                                                     }
{                                                                              }
procedure SplitRegNameA(const Name: AnsiString; var Key, ValueName: AnsiString);
var S : AnsiString;
    I : Integer;
begin
  S := StrExclSuffixA(StrExclPrefixA(Name, '\'), '\');
  I := PosCharA('\', S);
  if I <= 0 then
    begin
      Key := S;
      ValueName := '';
      exit;
    end;
  Key := CopyLeftA(S, I - 1);
  ValueName := CopyFromA(S, I + 1);
end;

procedure SplitRegNameU(const Name: UnicodeString; var Key, ValueName: UnicodeString);
var S : UnicodeString;
    I : Integer;
begin
  S := StrExclSuffixU(StrExclPrefixU(Name, '\'), '\');
  I := PosCharU('\', S);
  if I <= 0 then
    begin
      Key := S;
      ValueName := '';
      exit;
    end;
  Key := CopyLeftU(S, I - 1);
  ValueName := CopyFromU(S, I + 1);
end;

procedure SplitRegName(const Name: String; var Key, ValueName: String);
begin
  {$IFDEF StringIsUnicode}
  SplitRegNameU(Name, Key, ValueName);
  {$ELSE}
  SplitRegNameA(Name, Key, ValueName);
  {$ENDIF}
end;

{ Exists                                                                       }
function RegKeyExistsA(const RootKey: HKEY; const Key: AnsiString): Boolean;
var Handle : HKEY;
begin
  if RegOpenKeyExA(RootKey, PAnsiChar(Key), 0, KEY_READ, Handle) = ERROR_SUCCESS then
    begin
      RegCloseKey(Handle);
      Result := True;
    end
  else
    Result := False;
end;

function RegKeyExistsU(const RootKey: HKEY; const Key: UnicodeString): Boolean;
var Handle : HKEY;
begin
  if RegOpenKeyExW(RootKey, PWideChar(Key), 0, KEY_READ, Handle) = ERROR_SUCCESS then
    begin
      RegCloseKey(Handle);
      Result := True;
    end
  else
    Result := False;
end;

function RegKeyExists(const RootKey: HKEY; const Key: String): Boolean;
begin
  {$IFDEF StringIsUnicode}
  Result := RegKeyExistsU(RootKey, Key);
  {$ELSE}
  Result := RegKeyExistsA(RootKey, Key);
  {$ENDIF}
end;

function RegValueExistsA(const RootKey: HKEY; const Key, Name: AnsiString): Boolean;
var Handle : HKEY;
begin
  if Windows.RegOpenKeyExA(RootKey, PAnsiChar(Key), 0, KEY_READ, Handle) = ERROR_SUCCESS then
    begin
      Result := Windows.RegQueryValueExA(Handle, PAnsiChar(Name), nil, nil, nil, nil) = ERROR_SUCCESS;
      RegCloseKey(Handle);
    end
  else
    Result := False;
end;

function RegValueExistsU(const RootKey: HKEY; const Key, Name: UnicodeString): Boolean;
var Handle : HKEY;
begin
  if Windows.RegOpenKeyExW(RootKey, PWideChar(Key), 0, KEY_READ, Handle) = ERROR_SUCCESS then
    begin
      Result := Windows.RegQueryValueExW(Handle, PWideChar(Name), nil, nil, nil, nil) = ERROR_SUCCESS;
      RegCloseKey(Handle);
    end
  else
    Result := False;
end;

function RegValueExists(const RootKey: HKEY; const Key, Name: String): Boolean;
begin
  {$IFDEF StringIsUnicode}
  Result := RegValueExistsU(RootKey, Key, Name);
  {$ELSE}
  Result := RegValueExistsA(RootKey, Key, Name);
  {$ENDIF}
end;

{ Set                                                                          }
function RegSetValueA(const RootKey: HKEY; const Key, Name: AnsiString;
         const ValueType: Cardinal; const Value: Pointer;
         const ValueSize: Integer): Boolean;
var D : DWORD;
    Handle : HKEY;
begin
  Result := False;
  if ValueSize < 0 then
    exit;
  if RegCreateKeyExA(RootKey, PAnsiChar(Key), 0, nil, REG_OPTION_NON_VOLATILE,
      KEY_WRITE, nil, Handle, @D) <> ERROR_SUCCESS then
    exit;
  Result := RegSetValueExA(Handle, PAnsiChar(Name), 0, ValueType, Value, ValueSize) = ERROR_SUCCESS;
  RegCloseKey(Handle);
end;

function RegSetValueU(const RootKey: HKEY; const Key, Name: UnicodeString;
         const ValueType: Cardinal; const Value: Pointer;
         const ValueSize: Integer): Boolean;
var D : DWORD;
    Handle : HKEY;
begin
  Result := False;
  if ValueSize < 0 then
    exit;
  if RegCreateKeyExW(RootKey, PWideChar(Key), 0, nil, REG_OPTION_NON_VOLATILE,
      KEY_WRITE, nil, Handle, @D) <> ERROR_SUCCESS then
    exit;
  Result := RegSetValueExW(Handle, PWideChar(Name), 0, ValueType, Value, ValueSize) = ERROR_SUCCESS;
  RegCloseKey(Handle);
end;

function  RegSetValue(const RootKey: HKEY; const Key, Name: String;
          const ValueType: Cardinal; const Value: Pointer;
          const ValueSize: Integer): Boolean;
begin
  {$IFDEF StringIsUnicode}
  Result := RegSetValueU(RootKey, Key, Name, ValueType, Value, ValueSize);
  {$ELSE}
  Result := RegSetValueA(RootKey, Key, Name, ValueType, Value, ValueSize);
  {$ENDIF}
end;

function RegSetValueA(const RootKey: HKEY; const Name: AnsiString;
         const ValueType: Cardinal; const Value: Pointer;
         const ValueSize: Integer): Boolean;
var K, N : AnsiString;
begin
  SplitRegNameA(Name, K, N);
  Result := RegSetValueA(RootKey, K, N, ValueType, Value, ValueSize);
end;

function RegSetValueU(const RootKey: HKEY; const Name: UnicodeString;
         const ValueType: Cardinal; const Value: Pointer;
         const ValueSize: Integer): Boolean;
var K, N : UnicodeString;
begin
  SplitRegNameU(Name, K, N);
  Result := RegSetValueU(RootKey, K, N, ValueType, Value, ValueSize);
end;

function RegSetValue(const RootKey: HKEY; const Name: String;
         const ValueType: Cardinal; const Value: Pointer;
         const ValueSize: Integer): Boolean;
begin
  {$IFDEF StringIsUnicode}
  Result := RegSetValueU(RootKey, Name, ValueType, Value, ValueSize);
  {$ELSE}
  Result := RegSetValueA(RootKey, Name, ValueType, Value, ValueSize);
  {$ENDIF}
end;

function SetRegistryStringA(const RootKey: HKEY; const Key: AnsiString;
    const Name: AnsiString; const Value: AnsiString): Boolean;
begin
  Result := RegSetValueA(RootKey, Key, Name, REG_SZ, PAnsiChar(Value), Length(Value) + 1);
end;

function SetRegistryStringU(const RootKey: HKEY; const Key: UnicodeString;
    const Name: UnicodeString; const Value: UnicodeString): Boolean;
begin
  Result := RegSetValueU(RootKey, Key, Name, REG_SZ, PWideChar(Value), (Length(Value) + 1) * SizeOf(WideChar));
end;

function SetRegistryString(const RootKey: HKEY; const Key: String;
         const Name: String; const Value: String): Boolean;
begin
  {$IFDEF StringIsUnicode}
  Result := SetRegistryStringU(RootKey, Key, Name, Value);
  {$ELSE}
  Result := SetRegistryStringA(RootKey, Key, Name, Value);
  {$ENDIF}
end;

function SetRegistryStringA(const RootKey: HKEY; const Name: AnsiString;
    const Value: AnsiString): Boolean;
begin
  Result := RegSetValueA(RootKey, Name, REG_SZ, PAnsiChar(Value), Length(Value) + 1);
end;

function  SetRegistryStringU(const RootKey: HKEY; const Name: UnicodeString;
          const Value: UnicodeString): Boolean;
begin
  Result := RegSetValueU(RootKey, Name, REG_SZ, PWideChar(Value), (Length(Value) + 1) * SizeOf(WideChar));
end;

function  SetRegistryString(const RootKey: HKEY; const Name: String;
          const Value: String): Boolean;
begin
  {$IFDEF StringIsUnicode}
  Result := SetRegistryStringU(RootKey, Name, Value);
  {$ELSE}
  Result := SetRegistryStringA(RootKey, Name, Value);
  {$ENDIF}
end;

function SetRegistryDWordA(const RootKey: HKEY; const Name: AnsiString;
    const Value: LongWord): Boolean;
begin
  Result := RegSetValueA(RootKey, Name, REG_DWORD, @Value, Sizeof(LongWord));
end;

function SetRegistryDWordU(const RootKey: HKEY; const Name: UnicodeString;
    const Value: LongWord): Boolean;
begin
  Result := RegSetValueU(RootKey, Name, REG_DWORD, @Value, Sizeof(LongWord));
end;

function SetRegistryDWord(const RootKey: HKEY; const Name: String;
    const Value: LongWord): Boolean;
begin
  {$IFDEF StringIsUnicode}
  Result := SetRegistryDWordU(RootKey, Name, Value);
  {$ELSE}
  Result := SetRegistryDWordA(RootKey, Name, Value);
  {$ENDIF}
end;

function SetRegistryBinaryA(const RootKey: HKEY; const Name: AnsiString; const Value;
    const ValueSize: Integer): Boolean;
begin
  Result := RegSetValueA(RootKey, Name, REG_BINARY, @Value, ValueSize);
end;

{ Get                                                                          }
function RegGetValueA(const RootKey: HKEY; const Key, Name: AnsiString;
         const ValueType: Cardinal; var RegValueType: Cardinal;
         var ValueBuf: Pointer; var ValueSize: Integer): Boolean;
var Handle  : HKEY;
    Buf     : Pointer;
    BufSize : Cardinal;
begin
  Result := False;
  ValueSize := 0;
  ValueBuf := nil;
  if RegOpenKeyExA(RootKey, PAnsiChar(Key), 0, KEY_READ, Handle) <> ERROR_SUCCESS then
    exit;
  try
    BufSize := 0;
    RegQueryValueExA(Handle, PAnsiChar(Name), nil, @RegValueType, nil, @BufSize);
    if BufSize <= 0 then
      exit;
    GetMem(Buf, BufSize);
    if RegQueryValueExA(Handle, PAnsiChar(Name), nil, @RegValueType, Buf, @BufSize) = ERROR_SUCCESS then
      begin
        ValueBuf := Buf;
        ValueSize := Integer(BufSize);
        Result := True;
      end;
    if not Result then
      FreeMem(Buf);
  finally
    RegCloseKey(Handle);
  end;
end;

function RegGetValueU(const RootKey: HKEY; const Key, Name: UnicodeString;
         const ValueType: Cardinal; var RegValueType: Cardinal;
         var ValueBuf: Pointer; var ValueSize: Integer): Boolean;
var Handle  : HKEY;
    Buf     : Pointer;
    BufSize : Cardinal;
begin
  Result := False;
  ValueSize := 0;
  ValueBuf := nil;
  if RegOpenKeyExW(RootKey, PWideChar(Key), 0, KEY_READ, Handle) <> ERROR_SUCCESS then
    exit;
  try
    BufSize := 0;
    RegQueryValueExW(Handle, PWideChar(Name), nil, @RegValueType, nil, @BufSize);
    if BufSize <= 0 then
      exit;
    GetMem(Buf, BufSize);
    if RegQueryValueExW(Handle, PWideChar(Name), nil, @RegValueType, Buf, @BufSize) = ERROR_SUCCESS then
      begin
        ValueBuf := Buf;
        ValueSize := Integer(BufSize);
        Result := True;
      end;
    if not Result then
      FreeMem(Buf);
  finally
    RegCloseKey(Handle);
  end;
end;

function RegGetValue(
         const RootKey: HKEY; const Key, Name: String;
         const ValueType: Cardinal;
         var RegValueType: Cardinal;
         var ValueBuf: Pointer; var ValueSize: Integer): Boolean;
begin
  {$IFDEF StringIsUnicode}
  Result := RegGetValueU(RootKey, Key, Name, ValueType, RegValueType, ValueBuf, ValueSize);
  {$ELSE}
  Result := RegGetValueA(RootKey, Key, Name, ValueType, RegValueType, ValueBuf, ValueSize);
  {$ENDIF}
end;

function RegGetValueA(const RootKey: HKEY; const Name: AnsiString;
         const ValueType: Cardinal; var RegValueType: Cardinal;
         var ValueBuf: Pointer; var ValueSize: Integer): Boolean;
var K, N : AnsiString;
begin
  SplitRegNameA(Name, K, N);
  Result := RegGetValueA(RootKey, K, N, ValueType, RegValueType, ValueBuf, ValueSize);
end;

function GetRegistryStringA(const RootKey: HKEY; const Key, Name: AnsiString): AnsiString;
var Buf   : Pointer;
    Size  : Integer;
    VType : Cardinal;
begin
  Result := '';
  if not RegGetValueA(RootKey, Key, Name, REG_SZ, VType, Buf, Size) then
    exit;
  if (VType = REG_DWORD) and (Size >= Sizeof(LongWord)) then
    Result := IntToStringA(PLongWord(Buf)^) else
  if Size > 0 then
    begin
      SetLength(Result, Size - 1);
      MoveMem(Buf^, PAnsiChar(Result)^, Size - 1);
    end;
  FreeMem(Buf);
end;

function GetRegistryStringU(const RootKey: HKEY; const Key, Name: UnicodeString): UnicodeString;
var Buf   : Pointer;
    Size  : Integer;
    VType : Cardinal;
begin
  Result := '';
  if not RegGetValueU(RootKey, Key, Name, REG_SZ, VType, Buf, Size) then
    exit;
  if (VType = REG_DWORD) and (Size >= Sizeof(LongWord)) then
    Result := IntToStringU(PLongWord(Buf)^) else
  if Size > 0 then
    begin
      SetLength(Result, (Size div SizeOf(WideChar)) - 1);
      MoveMem(Buf^, PWideChar(Result)^, Size - 1);
    end;
  FreeMem(Buf);
end;

function GetRegistryString(const RootKey: HKEY; const Key, Name: String): String;
begin
  {$IFDEF StringIsUnicode}
  Result := GetRegistryStringU(RootKey, Key, Name);
  {$ELSE}
  Result := GetRegistryStringA(RootKey, Key, Name);
  {$ENDIF}
end;

function GetRegistryStringA(const RootKey: HKEY; const Name: AnsiString): AnsiString;
var K, N : AnsiString;
begin
  SplitRegNameA(Name, K, N);
  Result := GetRegistryStringA(RootKey, K, N);
end;

function GetRegistryStringU(const RootKey: HKEY; const Name: UnicodeString): UnicodeString;
var K, N : UnicodeString;
begin
  SplitRegNameU(Name, K, N);
  Result := GetRegistryStringU(RootKey, K, N);
end;

function GetRegistryString(const RootKey: HKEY; const Name: String): String;
begin
  {$IFDEF StringIsUnicode}
  Result := GetRegistryStringU(RootKey, Name);
  {$ELSE}
  Result := GetRegistryStringA(RootKey, Name);
  {$ENDIF}
end;

function GetRegistryDWordA(const RootKey: HKEY; const Key, Name: AnsiString): LongWord;
var Buf   : Pointer;
    Size  : Integer;
    VType : Cardinal;
begin
  Result := 0;
  if not RegGetValueA(RootKey, Key, Name, REG_DWORD, VType, Buf, Size) then
    exit;
  if (VType = REG_DWORD) and (Size >= Sizeof(LongWord)) then
    Result := PLongWord(Buf)^;
  FreeMem(Buf);
end;

function GetRegistryDWordU(const RootKey: HKEY; const Key, Name: UnicodeString): LongWord;
var Buf   : Pointer;
    Size  : Integer;
    VType : Cardinal;
begin
  Result := 0;
  if not RegGetValueU(RootKey, Key, Name, REG_DWORD, VType, Buf, Size) then
    exit;
  if (VType = REG_DWORD) and (Size >= Sizeof(LongWord)) then
    Result := PLongWord(Buf)^;
  FreeMem(Buf);
end;

function GetRegistryDWord(const RootKey: HKEY; const Key, Name: String): LongWord;
begin
  {$IFDEF StringIsUnicode}
  Result := GetRegistryDWordU(RootKey, Key, Name);
  {$ELSE}
  Result := GetRegistryDWordA(RootKey, Key, Name);
  {$ENDIF}
end;

{ Delete                                                                       }
function DeleteRegistryValueA(const RootKey: HKEY; const Key, Name: AnsiString): Boolean;
var Handle : HKEY;
begin
  if RegOpenKeyExA(RootKey, PAnsiChar(Key), 0, KEY_WRITE, Handle) = ERROR_SUCCESS then
    begin
      Result := RegDeleteValueA(Handle, PAnsiChar(Name)) = ERROR_SUCCESS;
      RegCloseKey(Handle);
    end
  else
    Result := False;
end;

function DeleteRegistryValueU(const RootKey: HKEY; const Key, Name: UnicodeString): Boolean;
var Handle : HKEY;
begin
  if RegOpenKeyExW(RootKey, PWideChar(Key), 0, KEY_WRITE, Handle) = ERROR_SUCCESS then
    begin
      Result := RegDeleteValueW(Handle, PWideChar(Name)) = ERROR_SUCCESS;
      RegCloseKey(Handle);
    end
  else
    Result := False;
end;

function DeleteRegistryValue(const RootKey: HKEY; const Key, Name: String): Boolean;
begin
  {$IFDEF StringIsUnicode}
  Result := DeleteRegistryValueU(RootKey, Key, Name);
  {$ELSE}
  Result := DeleteRegistryValueA(RootKey, Key, Name);
  {$ENDIF}
end;

function DeleteRegistryKeyA(const RootKey: HKEY; const Key: AnsiString): Boolean;
var Handle : HKEY;
    K, N   : AnsiString;
begin
  SplitRegNameA(Key, K, N);
  if RegOpenKeyExA(RootKey, PAnsiChar(K), 0, KEY_WRITE, Handle) = ERROR_SUCCESS then
    begin
      Result := RegDeleteKeyA(Handle, PAnsiChar(N)) = ERROR_SUCCESS;
      RegCloseKey(Handle);
    end
  else
    Result := False;
end;

function DeleteRegistryKeyU(const RootKey: HKEY; const Key: UnicodeString): Boolean;
var Handle : HKEY;
    K, N   : UnicodeString;
begin
  SplitRegNameU(Key, K, N);
  if RegOpenKeyExW(RootKey, PWideChar(K), 0, KEY_WRITE, Handle) = ERROR_SUCCESS then
    begin
      Result := RegDeleteKeyW(Handle, PWideChar(N)) = ERROR_SUCCESS;
      RegCloseKey(Handle);
    end
  else
    Result := False;
end;

function DeleteRegistryKey(const RootKey: HKEY; const Key: String): Boolean;
begin
  {$IFDEF StringIsUnicode}
  Result := DeleteRegistryKeyU(RootKey, Key);
  {$ELSE}
  Result := DeleteRegistryKeyA(RootKey, Key);
  {$ENDIF}
end;

{ Remote Registries                                                            }
function ConnectRegistryA(const MachineName: AnsiString; const RootKey: HKEY;
         var RemoteKey: HKEY): Boolean;
begin
  Result := RegConnectRegistryA(PAnsiChar(MachineName), RootKey, RemoteKey) = ERROR_SUCCESS;
end;

function ConnectRegistryU(const MachineName: UnicodeString; const RootKey: HKEY;
         var RemoteKey: HKEY): Boolean;
begin
  Result := RegConnectRegistryW(PWideChar(MachineName), RootKey, RemoteKey) = ERROR_SUCCESS;
end;

function DisconnectRegistry(const RemoteKey: HKEY): Boolean;
begin
  Result := RegCloseKey(RemoteKey) = ERROR_SUCCESS;
end;

{ Enumerate                                                                    }
function RegEnumA(const RootKey: HKEY; const Name: AnsiString;
         var ResultList: AnsiStringArray; const DoKeys: Boolean): Boolean;
const
  BufCharCount = 2048;
var Buf    : array[0..BufCharCount] of AnsiChar;
    BufLen : LongWord;
    I      : Integer;
    Res    : Integer;
    S      : AnsiString;
    Handle : HKEY;
begin
  ResultList := nil;
  Result := RegOpenKeyExA(RootKey, PAnsiChar(Name), 0, KEY_READ, Handle) = ERROR_SUCCESS;
  if not Result then
    exit;
  I := 0;
  repeat
    BufLen := BufCharCount;
    if DoKeys then
      Res := RegEnumKeyExA(Handle, I, @Buf[0], BufLen, nil, nil, nil, nil)
    else
      Res := RegEnumValueA(Handle, I, @Buf[0], BufLen, nil, nil, nil, nil);
    if Res = ERROR_SUCCESS then
      begin
        SetLength(S, BufLen);
        if BufLen > 0 then
          MoveMem(Buf[0], PAnsiChar(S)^, BufLen);
        DynArrayAppendA(ResultList, S);
        Inc(I);
      end;
  until Res <> ERROR_SUCCESS;
  RegCloseKey(Handle);
end;

function RegEnumU(const RootKey: HKEY; const Name: UnicodeString;
         var ResultList: UnicodeStringArray; const DoKeys: Boolean): Boolean;
const
  BufCharCount = 2048;
var Buf    : array[0..BufCharCount] of WideChar;
    BufLen : LongWord;
    I      : Integer;
    Res    : Integer;
    S      : UnicodeString;
    Handle : HKEY;
begin
  ResultList := nil;
  Result := RegOpenKeyExW(RootKey, PWideChar(Name), 0, KEY_READ, Handle) = ERROR_SUCCESS;
  if not Result then
    exit;
  I := 0;
  repeat
    BufLen := BufCharCount;
    if DoKeys then
      Res := RegEnumKeyExW(Handle, I, @Buf[0], BufLen, nil, nil, nil, nil)
    else
      Res := RegEnumValueW(Handle, I, @Buf[0], BufLen, nil, nil, nil, nil);
    if Res = ERROR_SUCCESS then
      begin
        SetLength(S, BufLen);
        if BufLen > 0 then
          MoveMem(Buf[0], PWideChar(S)^, BufLen * SizeOf(WideChar));
        DynArrayAppendU(ResultList, S);
        Inc(I);
      end;
  until Res <> ERROR_SUCCESS;
  RegCloseKey(Handle);
end;

function EnumRegistryValuesA(const RootKey: HKEY; const Name: AnsiString;
    var ValueList: AnsiStringArray): Boolean;
begin
  Result := RegEnumA(RootKey, Name, ValueList, False);
end;

function EnumRegistryValuesU(const RootKey: HKEY; const Name: UnicodeString;
         var ValueList: UnicodeStringArray): Boolean;
begin
  Result := RegEnumU(RootKey, Name, ValueList, False);
end;

function EnumRegistryValues(const RootKey: HKEY; const Name: String;
         var ValueList: StringArray): Boolean;
begin
  {$IFDEF StringIsUnicode}
  Result := EnumRegistryValuesU(RootKey, Name, UnicodeStringArray(ValueList));
  {$ELSE}
  Result := EnumRegistryValuesA(RootKey, Name, AnsiStringArray(ValueList));
  {$ENDIF}
end;

function EnumRegistryKeysA(const RootKey: HKEY; const Name: AnsiString;
    var KeyList: AnsiStringArray): Boolean;
begin
  Result := RegEnumA(RootKey, Name, KeyList, True);
end;

function EnumRegistryKeysU(const RootKey: HKEY; const Name: UnicodeString;
    var KeyList: UnicodeStringArray): Boolean;
begin
  Result := RegEnumU(RootKey, Name, KeyList, True);
end;

function EnumRegistryKeys(const RootKey: HKEY; const Name: String;
    var KeyList: StringArray): Boolean;
begin
  {$IFDEF StringIsUnicode}
  Result := EnumRegistryKeysU(RootKey, Name, UnicodeStringArray(KeyList));
  {$ELSE}
  Result := EnumRegistryKeysA(RootKey, Name, AnsiStringArray(KeyList));
  {$ENDIF}
end;



{                                                                              }
{ Windows Version Info                                                         }
{                                                                              }
var
  Win32PlatformInit : Boolean = False;
  Win32Platform     : Integer;
  Win32MajorVersion : Integer;
  Win32MinorVersion : Integer;
  Win32CSDVersion   : AnsiString;

procedure InitPlatformId;
var OSVersionInfo : TOSVersionInfoA;
begin
  OSVersionInfo.dwOSVersionInfoSize := SizeOf(OSVersionInfo);
  if Windows.GetVersionExA(OSVersionInfo) then
    with OSVersionInfo do
      begin
        Win32Platform := dwPlatformId;
        Win32MajorVersion := dwMajorVersion;
        Win32MinorVersion := dwMinorVersion;
        Win32CSDVersion := szCSDVersion;
      end;
  Win32PlatformInit := True;
end;

function GetWindowsVersion: TWindowsVersion;
begin
  if not Win32PlatformInit then
    InitPlatformId;
  case Win32Platform of
    VER_PLATFORM_WIN32s :
      Result := Win16_31;
    VER_PLATFORM_WIN32_WINDOWS :
      if Win32MajorVersion <= 4 then
        case Win32MinorVersion of
          0..9   : if StrTrimA(Win32CSDVersion, csWhiteSpace) = 'B' then
                     Result := Win32_95R2
                   else
                     Result := Win32_95;
          10..89 : if StrTrimA(Win32CSDVersion, csWhiteSpace) = 'A' then
                     Result := Win32_98SE
                   else
                     Result := Win32_98;
          90..99 : Result := Win32_ME;
        else
          Result := Win32_Future;
        end
      else
        Result := Win32_Future;
    VER_PLATFORM_WIN32_NT :
      case Win32MajorVersion of
        0..2 : Result := WinNT_Pre3;
        3 : case Win32MinorVersion of
              1, 10..19 : Result := WinNT_31;
              5, 50     : Result := WinNT_35;
              51..99    : Result := WinNT_351;
            else
              Result := WinNT_31;
            end;
        4 : Result := WinNT_40;
        5 : case Win32MinorVersion of
              0 : Result := Win_2000;
              1 : Result := Win_XP;
              2 : Result := Win_2003;
            else
              Result := WinNT5_Future;
            end;
        6 : case Win32MinorVersion of
              0 : Result := Win_Vista;
              1 : Result := Win_7;
              2 : Result := Win_8;
              3 : Result := Win_81;
            else
              Result := WinNT6_Future;
            end;
        10 : case Win32MinorVersion of
               0 : Result := Win_10;
             else
               Result := WinNT10_Future;
             end;
      else
        Result := WinNT_Future;
      end;
  else
    Result := Win_Future;
  end;
end;

function GetWindowsVersionString: String;
begin
  case GetWindowsVersion of
    Win16_31       : Result := '3.1 16-bit';
    Win32_95       : Result := '95';
    Win32_95R2     : Result := '95 R2';
    Win32_98       : Result := '98';
    Win32_98SE     : Result := '98 SE';
    Win32_ME       : Result := 'ME';
    Win32_Future   : Result := '32-bit';
    WinNT_Pre3     : Result := 'NT <3';
    WinNT_31       : Result := 'NT 3.1';
    WinNT_35       : Result := 'NT 3.5';
    WinNT_351      : Result := 'NT 3.51';
    WinNT_40       : Result := 'NT 4';
    Win_2000       : Result := '2000';
    Win_XP         : Result := 'XP';
    Win_2003       : Result := '2003';
    WinNT5_Future  : Result := '2003+';
    Win_Vista      : Result := 'Vista/2008';
    Win_7          : Result := '7/2008R2';
    Win_8          : Result := '8/2012';
    Win_81         : Result := '8.1/2012R2';
    WinNT6_Future  : Result := '8.1+/2012R2+';
    Win_10         : Result := '10/2016';
    WinNT10_Future : Result := '10+/2016+';
    WinNT_Future   : Result := '(future)';
    Win_Future     : Result := '(future)';
  else
    Result := '';
  end;
end;

function IsWinPlatform95: Boolean;
begin
  Result := Win32Platform = VER_PLATFORM_WIN32_WINDOWS;
end;

function IsWinPlatformNT: Boolean;
begin
  Result := Win32Platform = VER_PLATFORM_WIN32_NT;
end;

const
  CurrentVersionKey1 = 'Software\Microsoft\Windows NT\CurrentVersion';
  CurrentVersionKey2 = 'Software\Microsoft\Windows\CurrentVersion';

function GetWindowsProductIDA: AnsiString;
begin
  Result := GetRegistryStringA(HKEY_LOCAL_MACHINE, CurrentVersionKey1, 'ProductId');
  if Result = '' then
    Result := GetRegistryStringA(HKEY_LOCAL_MACHINE, CurrentVersionKey2, 'ProductId');
end;

function GetWindowsProductIDU: UnicodeString;
begin
  Result := GetRegistryStringU(HKEY_LOCAL_MACHINE, CurrentVersionKey1, 'ProductId');
  if Result = '' then
    Result := GetRegistryStringU(HKEY_LOCAL_MACHINE, CurrentVersionKey2, 'ProductId');
end;

function GetWindowsProductID: String;
begin
  {$IFDEF StringIsUnicode}
  Result := GetWindowsProductIDU;
  {$ELSE}
  Result := GetWindowsProductIDA;
  {$ENDIF}
end;

function GetWindowsProductNameA: AnsiString;
begin
  Result := GetRegistryStringA(HKEY_LOCAL_MACHINE, CurrentVersionKey1, 'ProductName');
  if Result = '' then
    Result := GetRegistryStringA(HKEY_LOCAL_MACHINE, CurrentVersionKey2, 'ProductName');
end;

function GetWindowsProductNameU: UnicodeString;
begin
  Result := GetRegistryStringU(HKEY_LOCAL_MACHINE, CurrentVersionKey1, 'ProductName');
  if Result = '' then
    Result := GetRegistryStringU(HKEY_LOCAL_MACHINE, CurrentVersionKey2, 'ProductName');
end;

function GetWindowsProductName: String;
begin
  {$IFDEF StringIsUnicode}
  Result := GetWindowsProductNameU;
  {$ELSE}
  Result := GetWindowsProductNameA;
  {$ENDIF}
end;


{                                                                              }
{ Windows Paths                                                                }
{                                                                              }
procedure EnsurePathSuffixA(var Path: AnsiString);
var L : Integer;
begin
  L := Length(Path);
  if (L > 0) and (Path[L] <> '\') then
    begin
      SetLength(Path, L + 1);
      Path[L + 1] := '\';
    end;
end;

procedure EnsurePathSuffixU(var Path: UnicodeString);
var L : Integer;
begin
  L := Length(Path);
  if (L > 0) and (Path[L] <> '\') then
    begin
      SetLength(Path, L + 1);
      Path[L + 1] := '\';
    end;
end;

procedure EnsurePathSuffix(var Path: String);
var L : Integer;
begin
  L := Length(Path);
  if (L > 0) and (Path[L] <> '\') then
    begin
      SetLength(Path, L + 1);
      Path[L + 1] := '\';
    end;
end;

const
  MaxTempPathLen = MAX_PATH + 1;

function GetWindowsTemporaryPathA: AnsiString;
var I : LongWord;
begin
  SetLength(Result, MaxTempPathLen);
  I := GetTempPathA(MaxTempPathLen, PAnsiChar(Result));
  if I > 0 then
    SetLength(Result, I)
  else
    Result := GetEnvironmentVariableA('TEMP');
  EnsurePathSuffixA(Result);
end;

function GetWindowsTemporaryPathU: UnicodeString;
var I : LongWord;
begin
  SetLength(Result, MaxTempPathLen);
  I := GetTempPathW(MaxTempPathLen, PWideChar(Result));
  if I > 0 then
    SetLength(Result, I)
  else
    Result := GetEnvironmentVariableU('TEMP');
  EnsurePathSuffixU(Result);
end;

function GetWindowsTemporaryPath: String;
begin
  {$IFDEF StringIsUnicode}
  Result := GetWindowsTemporaryPathU;
  {$ELSE}
  Result := GetWindowsTemporaryPathA;
  {$ENDIF}
end;

const
  MaxWinPathLen = MAX_PATH + 1;

function GetWindowsPathA: AnsiString;
var I : LongWord;
begin
  SetLength(Result, MaxWinPathLen);
  I := Windows.GetWindowsDirectoryA(PAnsiChar(Result), MaxWinPathLen);
  if I > 0 then
    SetLength(Result, I)
  else
    Result := GetEnvironmentVariableA('SystemRoot');
  EnsurePathSuffixA(Result);
end;

function GetWindowsPathU: UnicodeString;
var I : LongWord;
begin
  SetLength(Result, MaxWinPathLen);
  I := Windows.GetWindowsDirectoryW(PWideChar(Result), MaxWinPathLen);
  if I > 0 then
    SetLength(Result, I)
  else
    Result := GetEnvironmentVariableU('SystemRoot');
  EnsurePathSuffixU(Result);
end;

function GetWindowsPath: String;
begin
  {$IFDEF StringIsUnicode}
  Result := GetWindowsPathU;
  {$ELSE}
  Result := GetWindowsPathA;
  {$ENDIF}
end;

const
  MaxWinSysPathLen = MAX_PATH + 1;

function GetWindowsSystemPathA: AnsiString;
var I : LongWord;
begin
  SetLength(Result, MaxWinSysPathLen);
  I := Windows.GetSystemDirectoryA(PAnsiChar(Result), MaxWinSysPathLen);
  if I > 0 then
    SetLength(Result, I)
  else
    Result := '';
  EnsurePathSuffixA(Result);
end;

function GetWindowsSystemPathU: UnicodeString;
var I : LongWord;
begin
  SetLength(Result, MaxWinSysPathLen);
  I := Windows.GetSystemDirectoryW(PWideChar(Result), MaxWinSysPathLen);
  if I > 0 then
    SetLength(Result, I)
  else
    Result := '';
  EnsurePathSuffixU(Result);
end;

function GetWindowsSystemPath: String;
begin
  {$IFDEF StringIsUnicode}
  Result := GetWindowsSystemPathU;
  {$ELSE}
  Result := GetWindowsSystemPathA;
  {$ENDIF}
end;

const
  CurrentVersionRegistryKey = 'SOFTWARE\Microsoft\Windows\CurrentVersion';

function GetProgramFilesPathA: AnsiString;
begin
  Result := GetRegistryStringA(HKEY_LOCAL_MACHINE, CurrentVersionRegistryKey,
      'ProgramFilesDir');
  EnsurePathSuffixA(Result);
end;

function GetProgramFilesPathU: UnicodeString;
begin
  Result := GetRegistryStringU(HKEY_LOCAL_MACHINE, CurrentVersionRegistryKey,
      'ProgramFilesDir');
  EnsurePathSuffixU(Result);
end;

function GetProgramFilesPath: String;
begin
  Result := GetRegistryString(HKEY_LOCAL_MACHINE, CurrentVersionRegistryKey,
      'ProgramFilesDir');
  EnsurePathSuffix(Result);
end;

function GetCommonFilesPathA: AnsiString;
begin
  Result := GetRegistryStringA(HKEY_LOCAL_MACHINE, CurrentVersionRegistryKey,
      'CommonFilesDir');
  EnsurePathSuffixA(Result);
end;

function GetCommonFilesPathU: UnicodeString;
begin
  Result := GetRegistryStringU(HKEY_LOCAL_MACHINE, CurrentVersionRegistryKey,
      'CommonFilesDir');
  EnsurePathSuffixU(Result);
end;

function GetCommonFilesPath: String;
begin
  Result := GetRegistryString(HKEY_LOCAL_MACHINE, CurrentVersionRegistryKey,
      'CommonFilesDir');
  EnsurePathSuffix(Result);
end;

function GetApplicationFileNameA: AnsiString;
begin
  Result := ToAnsiString(ParamStr(0));
end;

function GetApplicationFileNameU: UnicodeString;
begin
  Result := ToUnicodeString(ParamStr(0));
end;

function GetApplicationFileName: String;
begin
  Result := ParamStr(0);
end;

function GetApplicationPath: String;
begin
  Result := ExtractFilePath(GetApplicationFileName);
  EnsurePathSuffix(Result);
end;

function GetHomePathA: AnsiString;
begin
  Result :=
      GetEnvironmentVariableA('HOMEDRIVE') +
      GetEnvironmentVariableA('HOMEPATH');
  if Result = '' then
    Result := GetEnvironmentVariableA('USERPROFILE');
  EnsurePathSuffixA(Result);
end;

function GetHomePathU: UnicodeString;
begin
  Result :=
      GetEnvironmentVariableU('HOMEDRIVE') +
      GetEnvironmentVariableU('HOMEPATH');
  if Result = '' then
    Result := GetEnvironmentVariableU('USERPROFILE');
  EnsurePathSuffixU(Result);
end;

function GetHomePath: String;
begin
  {$IFDEF StringIsUnicode}
  Result := GetHomePathU;
  {$ELSE}
  Result := GetHomePathA;
  {$ENDIF}
end;

function GetLocalAppDataPathA: AnsiString;
begin
  Result := GetEnvironmentVariableA('LOCALAPPDATA');
  EnsurePathSuffixA(Result);
end;

function GetLocalAppDataPathU: UnicodeString;
begin
  Result := GetEnvironmentVariableU('LOCALAPPDATA');
  EnsurePathSuffixU(Result);
end;

function GetLocalAppDataPath: String;
begin
  {$IFDEF StringIsUnicode}
  Result := GetLocalAppDataPathU;
  {$ELSE}
  Result := GetLocalAppDataPathA;
  {$ENDIF}
end;

{                                                                              }
{ Identification                                                               }
{                                                                              }
const
  MAX_USERNAME_LENGTH = 256;

function GetUserNameA: AnsiString;
var L : LongWord;
begin
  L := MAX_USERNAME_LENGTH;
  SetLength(Result, L + 1);
  if Windows.GetUserNameA(PAnsiChar(Result), L) and (L > 0) then
    SetLength(Result, StrZLenA(PAnsiChar(Result)))
  else
    Result := GetEnvironmentVariableA('USERNAME');
end;

function GetUserNameU: UnicodeString;
var L : LongWord;
begin
  L := MAX_USERNAME_LENGTH;
  SetLength(Result, L + 1);
  if Windows.GetUserNameW(PWideChar(Result), L) and (L > 0) then
    SetLength(Result, StrZLenW(PWideChar(Result)))
  else
    Result := GetEnvironmentVariableU('USERNAME');
end;

function GetUserName: String;
begin
  {$IFDEF StringIsUnicode}
  Result := GetUserNameU;
  {$ELSE}
  Result := GetUserNameA;
  {$ENDIF}
end;

function GetLocalComputerNameA: AnsiString;
var L : LongWord;
begin
  L := MAX_COMPUTERNAME_LENGTH + 2;
  SetLength(Result, L);
  if Windows.GetComputerNameA(PAnsiChar(Result), L) and (L > 0) then
    SetLength(Result, StrZLenA(PAnsiChar(Result)))
  else
    Result := GetEnvironmentVariableA('COMPUTERNAME');
end;

function GetLocalComputerNameU: UnicodeString;
var L : LongWord;
begin
  L := MAX_COMPUTERNAME_LENGTH + 2;
  SetLength(Result, L);
  if Windows.GetComputerNameW(PWideChar(Result), L) and (L > 0) then
    SetLength(Result, StrZLenW(PWideChar(Result)))
  else
    Result := GetEnvironmentVariableU('COMPUTERNAME');
end;

function GetLocalComputerName: String;
begin
  {$IFDEF StringIsUnicode}
  Result := GetLocalComputerNameU;
  {$ELSE}
  Result := GetLocalComputerNameA;
  {$ENDIF}
end;


{                                                                              }
{ Application Version Info                                                     }
{                                                                              }
var
  VersionInfoBufA : Pointer = nil;
  VerTransStrA    : AnsiString;

// Returns True if VersionInfo is available
function LoadAppVersionInfoA: Boolean;
type TTransBuffer = array[1..4] of SmallInt;
     PTransBuffer = ^TTransBuffer;
var InfoSize : Integer;
    Size, H  : LongWord;
    EXEName  : AnsiString;
    Trans    : PTransBuffer;
begin
  Result := Assigned(VersionInfoBufA);
  if Result then
    exit;
  EXEName := GetApplicationFileNameA;
  InfoSize := GetFileVersionInfoSizeA(PAnsiChar(EXEName), H);
  if InfoSize = 0 then
    exit;
  GetMem(VersionInfoBufA, InfoSize);
  if not GetFileVersionInfoA(PAnsiChar(EXEName), H, InfoSize, VersionInfoBufA) then
    begin
      FreeMem(VersionInfoBufA);
      VersionInfoBufA := nil;
      exit;
    end;
  VerQueryValueA(VersionInfoBufA, PAnsiChar('\VarFileInfo\Translation'),
                 Pointer(Trans), Size);
  VerTransStrA := Word32ToHexA(Trans^[1], 4, True) + Word32ToHexA(Trans^[2], 4, True);
  Result := True;
end;

var
  VersionInfoBufW : Pointer = nil;
  VerTransStrU    : UnicodeString;

// Returns True if VersionInfo is available
function LoadAppVersionInfoW: Boolean;
type
  TTransBuffer = array[1..4] of SmallInt;
  PTransBuffer = ^TTransBuffer;
var InfoSize : Integer;
    Size, H  : LongWord;
    EXEName  : UnicodeString;
    Trans    : PTransBuffer;
begin
  Result := Assigned(VersionInfoBufW);
  if Result then
    exit;
  EXEName := GetApplicationFileNameU;
  InfoSize := GetFileVersionInfoSizeW(PWideChar(EXEName), H);
  if InfoSize = 0 then
    exit;
  GetMem(VersionInfoBufW, InfoSize);
  if not GetFileVersionInfoW(PWideChar(EXEName), H, InfoSize, VersionInfoBufW) then
    begin
      FreeMem(VersionInfoBufW);
      VersionInfoBufW := nil;
      exit;
    end;
  VerQueryValueW(VersionInfoBufW, PWideChar('\VarFileInfo\Translation'),
                 Pointer(Trans), Size);
  VerTransStrU := Word32ToHexU(Trans^[1], 4, True) + Word32ToHexU(Trans^[2], 4, True);
  Result := True;
end;

const
  VersionInfoStrA: Array [TVersionInfo] of AnsiString = (
    'FileVersion',
    'FileDescription',
    'LegalCopyright',
    'Comments',
    'CompanyName',
    'InternalName',
    'LegalTrademarks',
    'OriginalFilename',
    'ProductName',
    'ProductVersion'
    );

function GetAppVersionInfoA(const VersionInfo: TVersionInfo): AnsiString;
var S     : AnsiString;
    Size  : LongWord;
    Value : PAnsiChar;
begin
  Result := '';
  if not LoadAppVersionInfoA then
    exit;
  S := 'StringFileInfo\' + VerTransStrA + '\' + VersionInfoStrA[VersionInfo];
  if VerQueryValueA(VersionInfoBufA, PAnsiChar(S), Pointer(Value), Size) then
    Result := Value;
end;

const
  VersionInfoStrW: array [TVersionInfo] of UnicodeString = (
    'FileVersion',
    'FileDescription',
    'LegalCopyright',
    'Comments',
    'CompanyName',
    'InternalName',
    'LegalTrademarks',
    'OriginalFilename',
    'ProductName',
    'ProductVersion'
    );

function GetAppVersionInfoU(const VersionInfo: TVersionInfo): UnicodeString;
var S     : UnicodeString;
    Size  : LongWord;
    Value : PWideChar;
begin
  Result := '';
  if not LoadAppVersionInfoW then
    exit;
  S := 'StringFileInfo\' + VerTransStrU + '\' + VersionInfoStrW[VersionInfo];
  if VerQueryValueW(VersionInfoBufW, PWideChar(S), Pointer(Value), Size) then
    Result := Value;
end;

function GetAppVersionInfo(const VersionInfo: TVersionInfo): String;
begin
  {$IFDEF StringIsUnicode}
  Result := GetAppVersionInfoU(VersionInfo);
  {$ELSE}
  Result := GetAppVersionInfoA(VersionInfo);
  {$ENDIF}
end;


{                                                                              }
{ Windows Processes                                                            }
{                                                                              }
{$IFDEF DELPHI7_DOWN}
type
  TStartupInfoA = _STARTUPINFOA;
{$ENDIF}

const
  WINEXECUTE_MAXCMDBUFLEN = 1024;

function WinExecuteA(const ExeName, Params: AnsiString; const ShowWin: Word;
    const WaitTime: Integer; const DefaultPath: AnsiString): LongWord;
var LStartUpInfo : TStartupInfoA;
    LProcessInfo : TProcessInformation;
    Cmd          : AnsiString;
    CmdBuf       : array[0..WINEXECUTE_MAXCMDBUFLEN + 2] of AnsiChar;
    DefDir       : PAnsiChar;
    TimeOut      : LongWord;
begin
  if ExeName = '' then
    raise EOSError.Create(SInvalidParameter);
  if Params = '' then
    Cmd := ExeName
  else
    Cmd := ExeName + ' ' + Params;
  if PosStrA('%', Cmd) > 0 then
    begin
      FillChar(CmdBuf, Sizeof(CmdBuf), 0);
      if ExpandEnvironmentStringsA(PAnsiChar(Cmd), @CmdBuf, WINEXECUTE_MAXCMDBUFLEN) > 0 then
        Cmd := StrZPasA(PAnsiChar(@CmdBuf));
    end;
  FillChar(LStartUpInfo, SizeOf(LStartUpInfo), 0);
  LStartUpInfo.cb := SizeOf(LStartUpInfo);
  LStartUpInfo.dwFlags := STARTF_USESHOWWINDOW;
  LStartUpInfo.wShowWindow := ShowWin;
  if DefaultPath = '' then
    DefDir := nil
  else
    DefDir := PAnsiChar(DefaultPath);
  FillChar(LProcessInfo, Sizeof(LProcessInfo), 0);
  if not CreateProcessA(
        nil, PAnsiChar(Cmd), nil, nil, False,
        CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS, nil, DefDir,
        LStartUpInfo, LProcessInfo) then
    RaiseLastWinError;
  if LProcessInfo.hThread <> 0 then
    CloseHandle(LProcessInfo.hThread);
  if WaitTime < 0 then
    TimeOut := INFINITE
  else
    TimeOut := WaitTime;
  if WaitTime = 0 then
    Result := 0
  else
    if Windows.WaitForSingleObject(LProcessInfo.hProcess, TimeOut) = WAIT_TIMEOUT then
      begin
        TerminateProcess(LProcessInfo.hProcess, 1);
        CloseHandle(LProcessInfo.hProcess);
        raise EOSError.Create(SProcessTimedOut)
      end
    else
      begin
        GetExitCodeProcess(LProcessInfo.hProcess, Result);
        CloseHandle(LProcessInfo.hProcess);
      end;
end;

function WinExecuteU(const ExeName, Params: UnicodeString; const ShowWin: Word;
    const WaitTime: Integer; const DefaultPath: UnicodeString): LongWord;
var StartUpInfo : TStartupInfoW;
    ProcessInfo	: TProcessInformation;
    Cmd         : UnicodeString;
    CmdBuf      : array[0..WINEXECUTE_MAXCMDBUFLEN + 2] of WideChar;
    DefDir      : PWideChar;
    TimeOut     : LongWord;
begin
  if ExeName = '' then
    raise EOSError.Create(SInvalidParameter);
  if Params = '' then
    Cmd := ExeName
  else
    Cmd := ExeName + ' ' + Params;
  if PosStrU('%', Cmd) > 0 then
    begin
      FillChar(CmdBuf, Sizeof(CmdBuf), 0);
      if ExpandEnvironmentStringsW(PWideChar(Cmd), @CmdBuf, WINEXECUTE_MAXCMDBUFLEN) > 0 then
        Cmd := StrZPasU(PWideChar(@CmdBuf));
    end;
  FillChar(StartUpInfo, SizeOf(StartUpInfo), 0);
  StartUpInfo.cb := SizeOf(StartUpInfo);
  StartUpInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartUpInfo.wShowWindow := ShowWin;
  if DefaultPath = '' then
    DefDir := nil
  else
    DefDir := PWideChar(DefaultPath);
  FillChar(ProcessInfo, Sizeof(ProcessInfo), 0);
  if not CreateProcessW(
        nil, PWideChar(Cmd), nil, nil, False,
        CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS, nil, DefDir,
        StartUpInfo, ProcessInfo) then
    RaiseLastWinError;
  if ProcessInfo.hThread <> 0 then
    CloseHandle(ProcessInfo.hThread);
  if WaitTime < 0 then
    TimeOut := INFINITE
  else
    TimeOut := WaitTime;
  if WaitTime = 0 then
    Result := 0
  else
    if Windows.WaitForSingleObject(ProcessInfo.hProcess, TimeOut) = WAIT_TIMEOUT then
      begin
        TerminateProcess(ProcessInfo.hProcess, 1);
        CloseHandle(ProcessInfo.hProcess);
        raise EOSError.Create(SProcessTimedOut)
      end
    else
      begin
        GetExitCodeProcess(ProcessInfo.hProcess, Result);
        CloseHandle(ProcessInfo.hProcess);
      end;
end;

function WinExecute(const ExeName, Params: String; const ShowWin: Word;
    const WaitTime: Integer; const DefaultPath: String): LongWord;
begin
  {$IFDEF StringIsUnicode}
  Result := WinExecuteU(ExeName, Params, ShowWin, WaitTime, DefaultPath);
  {$ELSE}
  Result := WinExecuteA(ExeName, Params, ShowWin, WaitTime, DefaultPath);
  {$ENDIF}
end;



{                                                                              }
{ Dynamic library                                                              }
{                                                                              }
function LoadLibraryA(const LibraryName: AnsiString): TLibraryHandle;
begin
  Result := TLibraryHandle(Windows.LoadLibraryA(PAnsiChar(LibraryName)));
  if Result <= HINSTANCE_ERROR then
    raise EOSError.Create('Failed to load library (' +
        ToStringB(PathExtractFileNameB(LibraryName)) + '): ' + GetLastWinErrorMessage);
end;

function LoadLibraryU(const LibraryName: UnicodeString): TLibraryHandle;
begin
  Result := TLibraryHandle(Windows.LoadLibraryW(PWideChar(LibraryName)));
  if Result <= HINSTANCE_ERROR then
    raise EOSError.Create('Failed to load library (' +
        PathExtractFileName(LibraryName) + '): ' + GetLastWinErrorMessage);
end;

function LoadLibraryA(const LibraryName: array of AnsiString): TLibraryHandle;
var I, L : Integer;
begin
  L := Length(LibraryName);
  if L = 0 then
    begin
      raise EOSError.Create('Failed to load library');
      exit;
    end;
  for I := 0 to L - 1 do
    begin
      Result := TLibraryHandle(Windows.LoadLibraryA(PAnsiChar(LibraryName[I])));
      if Result > HINSTANCE_ERROR then
        exit;
    end;
  raise EOSError.Create('Failed to load library: ' + GetLastWinErrorMessage);
end;

function LoadLibrary(const LibraryName: String): TLibraryHandle;
begin
  {$IFDEF StringIsUnicode}
  Result := LoadLibraryU(LibraryName);
  {$ELSE}
  Result := LoadLibraryA(LibraryName);
  {$ENDIF}
end;

function GetProcAddressA(const Handle: TLibraryHandle; const ProcName: AnsiString): Pointer;
begin
  Result := Windows.GetProcAddress(Cardinal(Handle), LPCSTR(PAnsiChar(ProcName)));
end;

{$IFNDEF FREEPASCAL}
function GetProcAddressU(const Handle: TLibraryHandle; const ProcName: UnicodeString): Pointer;
begin
  Result := Windows.GetProcAddress(Cardinal(Handle), LPCWSTR(PWideChar(ProcName)));
end;
{$ENDIF}

function GetProcAddress(const Handle: TLibraryHandle; const ProcName: String): Pointer;
begin
  {$IFDEF FREEPASCAL}
  Result := GetProcAddressA(Handle, ProcName);
  {$ELSE}
  {$IFDEF StringIsUnicode}
  Result := GetProcAddressU(Handle, ProcName);
  {$ELSE}
  Result := GetProcAddressA(Handle, ProcName);
  {$ENDIF}
  {$ENDIF}
end;

procedure FreeLibrary(const Handle: TLibraryHandle);
begin
  Windows.FreeLibrary(Cardinal(Handle));
end;

{ TDynamicLibrary                                                              }
constructor TDynamicLibrary.CreateA(const LibraryName: AnsiString);
begin
  inherited Create;
  FHandle := LoadLibraryA(LibraryName);
  Assert(FHandle <> 0);
end;

constructor TDynamicLibrary.CreateA(const LibraryName: array of AnsiString);
begin
  inherited Create;
  FHandle := LoadLibraryA(LibraryName);
  Assert(FHandle <> 0);
end;

constructor TDynamicLibrary.CreateU(const LibraryName: UnicodeString);
begin
  inherited Create;
  FHandle := LoadLibraryU(LibraryName);
  Assert(FHandle <> 0);
end;

destructor TDynamicLibrary.Destroy;
begin
  if FHandle <> 0 then
    begin
      FreeLibrary(FHandle);
      FHandle := 0;
    end;
  inherited Destroy;
end;

function TDynamicLibrary.GetProcAddressA(const ProcName: AnsiString): Pointer;
begin
  Assert(FHandle <> 0);
  Result := flcWinUtils.GetProcAddressA(FHandle, ProcName);
end;

function TDynamicLibrary.GetProcAddressU(const ProcName: UnicodeString): Pointer;
begin
  Assert(FHandle <> 0);
  {$IFDEF FREEPASCAL}
  Result := flcWinUtils.GetProcAddressA(FHandle, ProcName);
  {$ELSE}
  Result := flcWinUtils.GetProcAddressU(FHandle, ProcName);
  {$ENDIF}
end;



{                                                                              }
{ Exit Windows                                                                 }
{                                                                              }
function ExitWindows(const ExitType: TExitWindowsType; const Force: Boolean): Boolean;
const SE_SHUTDOWN_NAME = 'SeShutDownPrivilege';
      ExitTypeFlags : array[TExitWindowsType] of Cardinal =
          (EWX_LOGOFF, EWX_POWEROFF, EWX_REBOOT, EWX_SHUTDOWN);
var hToken : THandle;
    tkp    : TTokenPrivileges;
    retval : Cardinal;
    uFlags : Cardinal;
begin
  if IsWinPlatformNT then
    if OpenProcessToken(GetCurrentProcess(), TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY, hToken) then
      begin
        LookupPrivilegeValue(nil, SE_SHUTDOWN_NAME, tkp.Privileges[0].Luid);
        tkp.PrivilegeCount := 1;
        tkp.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
        AdjustTokenPrivileges(hToken, false, tkp, 0, tkp, retval);
      end;
  uFlags := ExitTypeFlags[ExitType];
  if Force then
    uFlags := uFlags or EWX_FORCE;
  Result := Windows.ExitWindowsEx(uFlags, 0);
end;

function LogOff(const Force: Boolean = False): Boolean;
begin
  Result := ExitWindows(exitLogOff, Force);
end;

function PowerOff(const Force: Boolean = False): Boolean;
begin
  Result := ExitWindows(exitPowerOff, Force);
end;

function Reboot(const Force: Boolean): Boolean;
begin
  Result := ExitWindows(exitReboot, Force);
end;

function ShutDown(const Force: Boolean = False): Boolean;
begin
  Result := ExitWindows(exitShutDown, Force);
end;



{                                                                              }
{ Locale information                                                           }
{                                                                              }
const
  LOCALE_MAXSIZE = 1024;

function GetLocaleStringA(const LocaleType: LongWord): AnsiString;
var Buf : array[0..LOCALE_MAXSIZE] of AnsiChar;
begin
  FillChar(Buf[0], SizeOf(Buf), 0);
  if GetLocaleInfoA(LOCALE_USER_DEFAULT, LocaleType, @Buf[0], LOCALE_MAXSIZE) <> 0 then
    Result := StrZPasA(PAnsiChar(@Buf[0]))
  else
    Result := '';
end;

function GetLocaleStringU(const LocaleType: LongWord): UnicodeString;
var Buf : array[0..LOCALE_MAXSIZE] of WideChar;
begin
  FillChar(Buf[0], SizeOf(Buf), 0);
  if GetLocaleInfoW(LOCALE_USER_DEFAULT, LocaleType, @Buf[0], LOCALE_MAXSIZE) <> 0 then
    Result := StrZPasU(PWideChar(@Buf[0]))
  else
    Result := '';
end;

function GetCountryCode1A: AnsiString;
begin
  Result := GetLocaleStringA(LOCALE_ICOUNTRY);
end;

function GetCountryCode1U: UnicodeString;
begin
  Result := GetLocaleStringU(LOCALE_ICOUNTRY);
end;

function GetCountryCode1: String;
begin
  {$IFDEF StringIsUnicode}
  Result := GetCountryCode1U;
  {$ELSE}
  Result := GetCountryCode1A;
  {$ENDIF}
end;

function GetCountryCode2A: AnsiString;
begin
  Result := GetLocaleStringA(LOCALE_SISO3166CTRYNAME);
end;

function GetCountryCode2U: UnicodeString;
begin
  Result := GetLocaleStringU(LOCALE_SISO3166CTRYNAME);
end;

function GetCountryCode2: String;
begin
  {$IFDEF StringIsUnicode}
  Result := GetCountryCode2U;
  {$ELSE}
  Result := GetCountryCode2A;
  {$ENDIF}
end;

function GetCountryNameA: AnsiString;
begin
  Result := GetLocaleStringA(LOCALE_SENGCOUNTRY);
end;

function GetCountryNameU: UnicodeString;
begin
  Result := GetLocaleStringU(LOCALE_SENGCOUNTRY);
end;

function GetCountryName: String;
begin
  {$IFDEF StringIsUnicode}
  Result := GetCountryNameU;
  {$ELSE}
  Result := GetCountryNameA;
  {$ENDIF}
end;



{                                                                              }
{ Miscelleaneous Windows API                                                   }
{                                                                              }
function ContentTypeFromExtentionA(const Extention: AnsiString): AnsiString;
begin
  Result := GetRegistryStringA(HKEY_CLASSES_ROOT, Extention, 'Content Type');
end;

function ContentTypeFromExtentionU(const Extention: UnicodeString): UnicodeString;
begin
  Result := GetRegistryStringU(HKEY_CLASSES_ROOT, Extention, 'Content Type');
end;

function ContentTypeFromExtention(const Extention: String): String;
begin
  {$IFDEF StringIsUnicode}
  Result := ContentTypeFromExtentionU(Extention);
  {$ELSE}
  Result := ContentTypeFromExtentionA(Extention);
  {$ENDIF}
end;

function FileClassFromExtentionA(const Extention: AnsiString): AnsiString;
begin
  Result := GetRegistryStringA(HKEY_CLASSES_ROOT, Extention, '');
end;

function GetFileClassA(const FileName: AnsiString): AnsiString;
begin
  Result := FileClassFromExtentionA(PathExtractFileExtB(FileName));
end;

function GetFileAssociationA(const FileName: AnsiString): AnsiString;
var S : AnsiString;
begin
  S := FileClassFromExtentionA(PathExtractFileExtB(FileName));
  if S = '' then
    Result := ''
  else
    Result := GetRegistryStringA(HKEY_CLASSES_ROOT, S + '\Shell\Open\Command', '');
end;

const
  AutoRunRegistryKey = 'SOFTWARE\Microsoft\Windows\CurrentVersion\Run';

function IsApplicationAutoRunA(const Name: AnsiString): Boolean;
var S : AnsiString;
begin
  S := GetApplicationFileNameA;
  Result := (S <> '') and (Name <> '') and
      StrEqualNoAsciiCaseA(GetRegistryStringA(HKEY_LOCAL_MACHINE, AutoRunRegistryKey, Name), S);
end;

procedure SetApplicationAutoRunA(const Name: AnsiString; const AutoRun: Boolean);
begin
  if Name = '' then
    exit;
  if AutoRun then
    SetRegistryStringA(HKEY_LOCAL_MACHINE, AnsiString(AutoRunRegistryKey), Name, GetApplicationFileNameA)
  else
    DeleteRegistryValueA(HKEY_LOCAL_MACHINE, AutoRunRegistryKey, Name);
end;

function GetKeyPressed(const VKeyCode: Integer): Boolean;
begin
  Result := GetKeyState(VKeyCode) and $80 <> 0;
end;

function GetHardDiskSerialNumberA(const DriveLetter: AnsiChar): AnsiString;
var N, F, S : DWORD;
    T : AnsiString;
begin
  S := 0;
  T := DriveLetter + AnsiString(':\');
  GetVolumeInformationA(PAnsiChar(T), nil, MAX_PATH + 1, @S,
      N, F, nil, 0);
  Result := Word32ToHexA(S, 8, False);
end;

function GetHardDiskSerialNumberU(const DriveLetter: WideChar): UnicodeString;
var N, F, S : DWORD;
    T : UnicodeString;
begin
  S := 0;
  T := DriveLetter + UnicodeString(':\');
  GetVolumeInformationW(PWideChar(T), nil, MAX_PATH + 1, @S,
      N, F, nil, 0);
  Result := Word32ToHexU(S, 8, False);
end;

function GetHardDiskSerialNumber(const DriveLetter: Char): String;
begin
  {$IFDEF StringIsUnicode}
  Result := GetHardDiskSerialNumberU(DriveLetter);
  {$ELSE}
  Result := GetHardDiskSerialNumberA(DriveLetter);
  {$ENDIF}
end;


{                                                                              }
{ Windows Fibers                                                               }
{                                                                              }
function ConvertThreadToFiber; external kernel32 name 'ConvertThreadToFiber';
function CreateFiber(dwStackSize: DWORD; lpStartAddress: TFNFiberStartRoutine;
         lpParameter: Pointer): Pointer; external kernel32 name 'CreateFiber';



{                                                                              }
{ Windows Shell                                                                }
{                                                                              }
const
  shell32 = 'shell32.dll';

function ShellExecuteA; external shell32 name 'ShellExecuteA';

procedure ShellLaunch(const S: AnsiString);
begin
  ShellExecuteA(0, 'open', PAnsiChar(S), '', '', SW_SHOWNORMAL);
end;



{                                                                              }
{ WinSpool API                                                                 }
{                                                                              }
const
   winspl = 'winspool.drv';

function EnumPortsA; external winspl name 'EnumPortsA';

type
  PPortInfo1A = ^TPortInfo1A;
  PPortInfo1 = PPortInfo1A;
  _PORT_INFO_1A = record
    pName: PAnsiChar;
  end;
  TPortInfo1A = _PORT_INFO_1A;
  TPortInfo1 = TPortInfo1A;

function GetWinPortNamesA: AnsiStringArray;
var BytesNeeded, N, I : LongWord;
    Buf : Pointer;
    InfoPtr : PPortInfo1;
    TempStr : AnsiString;
begin
  Result := nil;
  if EnumPortsA(nil, 1, nil, 0, BytesNeeded, N) then
    exit;
  if GetLastWinError <> ERROR_INSUFFICIENT_BUFFER then
    RaiseLastWinError;
  GetMem(Buf, BytesNeeded);
  try
    if not EnumPortsA(nil, 1, Buf, BytesNeeded, BytesNeeded, N) then
      RaiseLastWinError;
    For I := 0 to N - 1 do
      begin
        InfoPtr := PPortInfo1(LongWord(Buf) + I * SizeOf(TPortInfo1));
        TempStr := InfoPtr^.pName;
        DynArrayAppendA(Result, TempStr);
      end;
  finally
    FreeMem(Buf);
  end;
end;



{                                                                              }
{ Timers                                                                       }
{                                                                              }
function GetMsCount: LongWord;
begin
  Result := GetTickCount;
end;

function GetUsCount: Int64;
var F : Int64;
begin
  QueryPerformanceCounter(Result);
  if not QueryPerformanceFrequency(F) then
    Result := 0
  else
    Result := Result div (F div 1000000);
end;



{                                                                              }
{ Tests                                                                        }
{                                                                              }
{$IFDEF DEBUG}
{$IFDEF TEST}
{$ASSERTIONS ON}
{$WARNINGS OFF}

{$DEFINE TEST_APPVERSIONINFO}
{$DEFINE TEST_DRIVEC_VALID}

procedure Test;
var
  A : AnsiStringArray;
  B : UnicodeStringArray;
  I : Integer;
begin
  Assert(Length(WinErrorMessageA(2)) > 5);
  Assert(Length(WinErrorMessageU(2)) > 5);
  Assert(Length(WinErrorMessage(2)) > 5);
  Assert(WinErrorMessageU(2) = WinErrorMessageA(2));

  Assert(GetEnvironmentVariableA('PATH') <> '', 'GetEnvironmentVariable');
  Assert(GetEnvironmentVariableU('PATH') <> '', 'GetEnvironmentVariable');
  Assert(GetEnvironmentVariable('PATH') <> '',  'GetEnvironmentVariable');
  Assert(GetEnvironmentVariableU('PATH') = GetEnvironmentVariableA('PATH'));

  A := GetEnvironmentStringsA;
  B := GetEnvironmentStringsU;
  Assert(Length(A) > 0);
  Assert(Length(B) > 0);
  Assert(Length(A) = Length(B));
  for I := 0 to Length(A) - 1 do
    Assert(A[I] = B[I]);

  Assert(GetWindowsVersionString <> '', 'GetWindowsVersionString');

  Assert(GetWindowsProductNameA <> '', 'GetWindowsProductName');
  Assert(GetWindowsProductNameU <> '', 'GetWindowsProductName');
  Assert(GetWindowsProductNameA = GetWindowsProductNameU);

  {$IFNDEF WIN32}
  // Win32 returns empty string
  Assert(GetWindowsProductIDA <> '', 'GetWindowsProductID');
  Assert(GetWindowsProductIDU <> '', 'GetWindowsProductID');
  Assert(GetWindowsProductIDA = GetWindowsProductIDU);
  {$ENDIF}

  Assert(GetUserNameA <> '',          'GetUserName');
  Assert(GetUserNameU <> '',          'GetUserName');
  Assert(GetUserName <> '',           'GetUserName');
  Assert(GetUserNameA = GetUserNameU, 'GetUserName');

  Assert(GetWindowsTemporaryPathA <> '', 'GetWindowsTemporaryPath');
  Assert(GetWindowsTemporaryPathU <> '', 'GetWindowsTemporaryPath');
  Assert(GetWindowsTemporaryPathU = GetWindowsTemporaryPathA);

  Assert(GetWindowsPathA <> '',             'GetWindowsPath');
  Assert(GetWindowsPathU <> '',             'GetWindowsPath');
  Assert(GetWindowsPathA = GetWindowsPathU, 'GetWindowsPath');

  Assert(GetWindowsSystemPathA <> '', 'GetWindowsSystemPath');
  Assert(GetWindowsSystemPathU <> '', 'GetWindowsSystemPath');
  Assert(GetWindowsSystemPathA = GetWindowsSystemPathU);

  Assert(GetProgramFilesPathA <> '', 'GetProgramFilesPath');
  Assert(GetProgramFilesPathU <> '', 'GetProgramFilesPath');
  Assert(GetProgramFilesPathU = GetProgramFilesPathA);

  Assert(GetCommonFilesPathA <> '', 'GetCommonFilesPath');
  Assert(GetCommonFilesPathU <> '', 'GetCommonFilesPath');
  Assert(GetCommonFilesPathU = GetCommonFilesPathA);

  Assert(GetApplicationPath <> '', 'GetApplicationPath');

  Assert(GetHomePathA <> '',          'GetHomePath');
  Assert(GetHomePathU <> '',          'GetHomePath');
  Assert(GetHomePathA = GetHomePathU, 'GetHomePath');

  Assert(GetCountryCode1A <> '',    'GetCountryCode1');
  Assert(GetCountryCode1U <> '',    'GetCountryCode1');
  Assert(GetCountryCode1A = GetCountryCode1U);

  Assert(GetCountryCode2A <> '',    'GetCountryCode2');
  Assert(GetCountryCode2U <> '',    'GetCountryCode2');

  Assert(GetCountryNameA <> '',     'GetCountryName');
  Assert(GetCountryNameU <> '',     'GetCountryName');

  {$IFDEF TEST_APPVERSIONINFO}
  Assert(GetAppVersionInfoA(viFileVersion) <> '');
  Assert(GetAppVersionInfoU(viFileVersion) <> '');
  Assert(GetAppVersionInfoA(viFileVersion) = GetAppVersionInfoU(viFileVersion));
  {$ENDIF}

  {$IFDEF TEST_DRIVEC_VALID}
  Assert(GetHardDiskSerialNumberA('C') <> '');
  Assert(GetHardDiskSerialNumberU('C') = GetHardDiskSerialNumberA('C'));
  {$ENDIF}

  Assert(ContentTypeFromExtentionA('.html') <> '');
  Assert(ContentTypeFromExtentionU('.html') <> '');
  Assert(ContentTypeFromExtentionA('.html') = ContentTypeFromExtentionU('.html'));
end;
{$ENDIF}
{$ENDIF}


initialization
finalization
  if Assigned(VersionInfoBufA) then
    FreeMem(VersionInfoBufA);
  if Assigned(VersionInfoBufW) then
    FreeMem(VersionInfoBufW);
end.

