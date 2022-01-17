{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals 5.00                                        }
{   File name:        flcFileUtils.pas                                         }
{   File version:     5.18                                                     }
{   Description:      File name and file system functions                      }
{                                                                              }
{   Copyright:        Copyright (c) 2002-2020, David J Butler                  }
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
{   2002/06/01  3.01  Created cFileUtils from cSysUtils.                       }
{   2002/12/12  3.02  Revision.                                                }
{   2005/07/22  4.03  Compilable with FreePascal 2 Win32 i386.                 }
{   2005/08/21  4.04  Compilable with FreePascal 2 Linux i386.                 }
{   2005/09/20  4.05  Improved error handling.                                 }
{   2005/09/21  4.06  Revised for Fundamentals 4.                              }
{   2008/12/30  4.07  Revision.                                                }
{   2009/06/05  4.08  File access functions.                                   }
{   2009/07/30  4.09  File access functions.                                   }
{   2010/06/27  4.10  Compilable with FreePascal 2.4.0 OSX x86-64              }
{   2012/03/26  4.11  Unicode update.                                          }
{   2013/11/15  4.12  Unicode update.                                          }
{   2015/03/13  4.13  RawByteString functions.                                 }
{   2016/01/09  5.14  Revised for Fundamentals 5.                              }
{   2016/02/01  5.15  Unicode update.                                          }
{   2016/04/10  5.16  Change to FileOpenWait.                                  }
{   2018/08/13  5.17  String type changes.                                     }
{   2019/07/29  5.18  DriveSizeA function.                                     }
{                                                                              }
{ Supported compilers:                                                         }
{                                                                              }
{   Delphi 7 Win32                      5.17  2019/02/24                       }
{   Delphi XE2 Win32                    5.17  2019/03/02                       }
{   Delphi XE2 Win64                    5.17  2019/03/02                       }
{   Delphi XE3 Win32                    5.17  2019/03/02                       }
{   Delphi XE3 Win64                    5.17  2019/03/02                       }
{   Delphi XE6 Win32                    5.17  2019/03/02                       }
{   Delphi XE6 Win64                    5.17  2019/03/02                       }
{   Delphi XE7 Win32                    5.17  2019/03/02                       }
{   Delphi XE7 Win64                    5.17  2019/03/02                       }
{   Delphi 10 Win32                     5.14  2016/01/09                       }
{   Delphi 10 Win64                     5.14  2016/01/09                       }
{   Delphi 10.2 Linux64                 5.17  2019/04/02                       }
{   FreePascal 3.0.4 Win32              5.17  2019/02/24                       }
{   FreePascal 3.0.4 Linux              5.18  2019/07/29                       }
{                                                                              }
{ Todo:                                                                        }
{  - IFDef win certain functions                                               }
{******************************************************************************}

{$INCLUDE ..\flcInclude.inc}

{$IFDEF FREEPASCAL}
  {$WARNINGS OFF}{$HINTS OFF}
{$ENDIF}
{$IFDEF DELPHI6_UP}
  {$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}

unit flcFileUtils;

interface

uses
  { System }
  {$IFDEF MSWIN}
  Windows,
  {$ENDIF}
  SysUtils,

  { Fundamentals }
  flcStdTypes;



{                                                                              }
{ Path functions                                                               }
{                                                                              }
const
  PosixPathSeparator = '/';
  WinPathSeparator   = '\';
  PathSeparator = {$IFDEF POSIX} PosixPathSeparator {$ENDIF}
                  {$IFDEF MSWIN} WinPathSeparator   {$ENDIF};

function  WinPathHasDriveLetterB(const Path: RawByteString): Boolean;
function  WinPathHasDriveLetterU(const Path: UnicodeString): Boolean;
function  WinPathHasDriveLetter(const Path: String): Boolean;

function  WinPathIsDriveLetterB(const Path: RawByteString): Boolean;
function  WinPathIsDriveLetterU(const Path: UnicodeString): Boolean;
function  WinPathIsDriveLetter(const Path: String): Boolean;

function  WinPathIsDriveRootB(const Path: RawByteString): Boolean;
function  WinPathIsDriveRootU(const Path: UnicodeString): Boolean;
function  WinPathIsDriveRoot(const Path: String): Boolean;

function  PathIsRootB(const Path: RawByteString): Boolean;
function  PathIsRootU(const Path: UnicodeString): Boolean;
function  PathIsRoot(const Path: String): Boolean;

function  PathIsUNCPathB(const Path: RawByteString): Boolean;
function  PathIsUNCPathU(const Path: UnicodeString): Boolean;
function  PathIsUNCPath(const Path: String): Boolean;

function  PathIsAbsoluteB(const Path: RawByteString): Boolean;
function  PathIsAbsoluteU(const Path: UnicodeString): Boolean;
function  PathIsAbsolute(const Path: String): Boolean;

function  PathIsDirectoryB(const Path: RawByteString): Boolean;
function  PathIsDirectoryU(const Path: UnicodeString): Boolean;
function  PathIsDirectory(const Path: String): Boolean;

function  PathInclSuffixB(const Path: RawByteString;
          const PathSep: ByteChar = PathSeparator): RawByteString;
function  PathInclSuffixU(const Path: UnicodeString;
          const PathSep: WideChar = PathSeparator): UnicodeString;
function  PathInclSuffix(const Path: String;
          const PathSep: Char = PathSeparator): String;

function  PathExclSuffixB(const Path: RawByteString;
          const PathSep: ByteChar = PathSeparator): RawByteString;
function  PathExclSuffixU(const Path: UnicodeString;
          const PathSep: WideChar = PathSeparator): UnicodeString;
function  PathExclSuffix(const Path: String;
          const PathSep: Char = PathSeparator): String;

procedure PathEnsureSuffixB(var Path: RawByteString;
          const PathSep: ByteChar = PathSeparator);
procedure PathEnsureSuffixU(var Path: UnicodeString;
          const PathSep: WideChar = PathSeparator);
procedure PathEnsureSuffix(var Path: String;
          const PathSep: Char = PathSeparator);

procedure PathEnsureNoSuffixB(var Path: RawByteString;
          const PathSep: ByteChar = PathSeparator);
procedure PathEnsureNoSuffixU(var Path: UnicodeString;
          const PathSep: WideChar = PathSeparator);
procedure PathEnsureNoSuffix(var Path: String;
          const PathSep: Char = PathSeparator);

function  PathCanonicalB(const Path: RawByteString;
          const PathSep: ByteChar = PathSeparator): RawByteString;
function  PathCanonicalU(const Path: UnicodeString;
          const PathSep: WideChar = PathSeparator): UnicodeString;
function  PathCanonical(const Path: String;
          const PathSep: Char = PathSeparator): String;

function  PathExpandB(const Path: RawByteString; const BasePath: RawByteString = '';
          const PathSep: ByteChar = PathSeparator): RawByteString;
function  PathExpandU(const Path: UnicodeString; const BasePath: UnicodeString = '';
          const PathSep: WideChar = PathSeparator): UnicodeString;
function  PathExpand(const Path: String; const BasePath: String = '';
          const PathSep: Char = PathSeparator): String;

function  PathLeftElementB(const Path: RawByteString;
          const PathSep: ByteChar = PathSeparator): RawByteString;
function  PathLeftElementU(const Path: UnicodeString;
          const PathSep: WideChar = PathSeparator): UnicodeString;
function  PathLeftElement(const Path: String;
          const PathSep: Char = PathSeparator): String;

procedure PathSplitLeftElementB(const Path: RawByteString;
          var LeftElement, RightPath: RawByteString;
          const PathSep: ByteChar = PathSeparator);
procedure PathSplitLeftElementU(const Path: UnicodeString;
          var LeftElement, RightPath: UnicodeString;
          const PathSep: WideChar = PathSeparator);
procedure PathSplitLeftElement(const Path: String;
          var LeftElement, RightPath: String;
          const PathSep: Char = PathSeparator);

procedure DecodeFilePathB(const FilePath: RawByteString;
          var Path, FileName: RawByteString;
          const PathSep: ByteChar = PathSeparator);
procedure DecodeFilePathU(const FilePath: UnicodeString;
          var Path, FileName: UnicodeString;
          const PathSep: WideChar = PathSeparator);
procedure DecodeFilePath(const FilePath: String;
          var Path, FileName: String;
          const PathSep: Char = PathSeparator);

function  PathExtractFilePathB(const FilePath: RawByteString;
          const PathSep: ByteChar = PathSeparator): RawByteString;
function  PathExtractFilePathU(const FilePath: UnicodeString;
          const PathSep: WideChar = PathSeparator): UnicodeString;
function  PathExtractFilePath(const FilePath: String;
          const PathSep: Char = PathSeparator): String;

function  PathExtractFileNameB(const FilePath: RawByteString;
          const PathSep: ByteChar = PathSeparator): RawByteString;
function  PathExtractFileNameU(const FilePath: UnicodeString;
          const PathSep: WideChar = PathSeparator): UnicodeString;
function  PathExtractFileName(const FilePath: String;
          const PathSep: Char = PathSeparator): String;

function  PathExtractFileExtB(const FilePath: RawByteString;
          const PathSep: ByteChar = PathSeparator): RawByteString;
function  PathExtractFileExtU(const FilePath: UnicodeString;
          const PathSep: WideChar = PathSeparator): UnicodeString;
function  PathExtractFileExt(const FilePath: String;
          const PathSep: Char = PathSeparator): String;

function  FileNameCleanB(const FileName: RawByteString): RawByteString;
function  FileNameCleanU(const FileName: UnicodeString): UnicodeString;
function  FileNameClean(const FileName: String): String;

function  FilePathB(const FileName, Path: RawByteString; const BasePath: RawByteString = '';
          const PathSep: ByteChar = PathSeparator): RawByteString;
function  FilePathU(const FileName, Path: UnicodeString; const BasePath: UnicodeString = '';
          const PathSep: WideChar = PathSeparator): UnicodeString;
function  FilePath(const FileName, Path: String; const BasePath: String = '';
          const PathSep: Char = PathSeparator): String;

function  DirectoryExpandB(const Path: RawByteString; const BasePath: RawByteString = '';
          const PathSep: ByteChar = PathSeparator): RawByteString;
function  DirectoryExpandU(const Path: UnicodeString; const BasePath: UnicodeString = '';
          const PathSep: WideChar = PathSeparator): UnicodeString;
function  DirectoryExpand(const Path: String; const BasePath: String = '';
          const PathSep: Char = PathSeparator): String;

function  UnixPathToSafeWinPathB(const Path: RawByteString): RawByteString;

function  WinPathToSafeUnixPathB(const Path: RawByteString): RawByteString;



{                                                                              }
{ File errors                                                                  }
{                                                                              }
type
  TFileError = (
    feNone                  {$IFDEF SupportEnumValue} = $00 {$ENDIF},
    feInvalidParameter      {$IFDEF SupportEnumValue} = $01 {$ENDIF},

    feFileError             {$IFDEF SupportEnumValue} = $10 {$ENDIF},
    feFileOpenError         {$IFDEF SupportEnumValue} = $11 {$ENDIF},
    feFileCreateError       {$IFDEF SupportEnumValue} = $12 {$ENDIF},
    feFileSharingError      {$IFDEF SupportEnumValue} = $13 {$ENDIF},
    feFileSeekError         {$IFDEF SupportEnumValue} = $14 {$ENDIF},
    feFileReadError         {$IFDEF SupportEnumValue} = $15 {$ENDIF},
    feFileWriteError        {$IFDEF SupportEnumValue} = $16 {$ENDIF},
    feFileSizeError         {$IFDEF SupportEnumValue} = $17 {$ENDIF},
    feFileExists            {$IFDEF SupportEnumValue} = $18 {$ENDIF},
    feFileDoesNotExist      {$IFDEF SupportEnumValue} = $19 {$ENDIF},
    feFileMoveError         {$IFDEF SupportEnumValue} = $1A {$ENDIF},
    feFileDeleteError       {$IFDEF SupportEnumValue} = $1B {$ENDIF},
    feDirectoryCreateError  {$IFDEF SupportEnumValue} = $1C {$ENDIF},

    feOutOfSpace            {$IFDEF SupportEnumValue} = $20 {$ENDIF},
    feOutOfResources        {$IFDEF SupportEnumValue} = $21 {$ENDIF},
    feInvalidFilePath       {$IFDEF SupportEnumValue} = $22 {$ENDIF},
    feInvalidFileName       {$IFDEF SupportEnumValue} = $23 {$ENDIF},
    feAccessDenied          {$IFDEF SupportEnumValue} = $24 {$ENDIF},
    feDeviceFailure         {$IFDEF SupportEnumValue} = $25 {$ENDIF}
  );

  EFileError = class(Exception)
  private
    FFileError : TFileError;

  public
    constructor Create(const FileError: TFileError; const Msg: string);
    constructor CreateFmt(const FileError: TFileError; const Msg: string; const Args: array of const);

    property FileError: TFileError read FFileError;
  end;



{                                                                              }
{ File operations                                                              }
{                                                                              }
type
  TFileHandle = Integer;

  TFileAccess = (
    faRead,
    faWrite,
    faReadWrite);

  TFileSharing = (
    fsDenyNone,
    fsDenyRead,
    fsDenyWrite,
    fsDenyReadWrite,
    fsExclusive);

  TFileOpenFlags = set of (
    foDeleteOnClose,
    foNoBuffering,
    foWriteThrough,
    foRandomAccessHint,
    foSequentialScanHint,
    foSeekToEndOfFile);

  TFileCreationMode = (
    fcCreateNew,
    fcCreateAlways,
    fcOpenExisting,
    fcOpenAlways,
    fcTruncateExisting);

  TFileSeekPosition = (
    fpOffsetFromStart,
    fpOffsetFromCurrent,
    fpOffsetFromEnd);

  PFileOpenWait = ^TFileOpenWait;
  TFileOpenWaitProcedure = procedure (const FileOpenWait: PFileOpenWait);
  TFileOpenWait = packed record
    Wait           : Boolean;
    UserData       : LongWord;
    Timeout        : Integer;    // Total time to wait (ms) for operation to succeed (including retries)
    RetryInterval  : Integer;    // Interval to wait (ms) before a retry
    RetryRandomise : Boolean;    // Randomize RetryInterval
    Callback       : TFileOpenWaitProcedure;
    Aborted        : Boolean;
    {$IFDEF MSWIN}
    Signal         : THandle;
    {$ENDIF}
  end;

var
  FileOpenWaitNone : TFileOpenWait = (
    Wait           : False;
    UserData       : 0;
    Timeout        : 0;
    RetryInterval  : 0;
    RetryRandomise : False;
    Callback       : nil;
    Aborted        : False;
    {$IFDEF MSWIN}
    Signal         : 0;
    {$ENDIF}
    );

  FileOpenWaitFewSec : TFileOpenWait = (
    Wait           : True;
    UserData       : 0;
    Timeout        : 2500;
    RetryInterval  : 250;
    RetryRandomise : True;
    Callback       : nil;
    Aborted        : False;
    {$IFDEF MSWIN}
    Signal         : 0;
    {$ENDIF}
    );

function  FileOpenExB(
          const FileName: RawByteString;
          const FileAccess: TFileAccess = faRead;
          const FileSharing: TFileSharing = fsDenyNone;
          const FileOpenFlags: TFileOpenFlags = [];
          const FileCreationMode: TFileCreationMode = fcOpenExisting;
          const FileOpenWait: PFileOpenWait = nil): TFileHandle;
function  FileOpenExU(
          const FileName: UnicodeString;
          const FileAccess: TFileAccess;
          const FileSharing: TFileSharing;
          const FileOpenFlags: TFileOpenFlags;
          const FileCreationMode: TFileCreationMode;
          const FileOpenWait: PFileOpenWait): TFileHandle;
function  FileOpenEx(
          const FileName: String;
          const FileAccess: TFileAccess;
          const FileSharing: TFileSharing;
          const FileOpenFlags: TFileOpenFlags = [];
          const FileCreationMode: TFileCreationMode = fcOpenExisting;
          const FileOpenWait: PFileOpenWait = nil): TFileHandle;

function  FileSeekEx(
          const FileHandle: TFileHandle;
          const FileOffset: Int64;
          const FilePosition: TFileSeekPosition = fpOffsetFromStart): Int64;

function  FileReadEx(
          const FileHandle: TFileHandle;
          var Buf; const BufSize: Integer): Integer;

function  FileWriteEx(
          const FileHandle: TFileHandle;
          const Buf; const BufSize: Integer): Integer;

procedure FileCloseEx(
          const FileHandle: TFileHandle);

function  FindFirstB(const Path: RawByteString; Attr: Integer; var F: TSearchRec): Integer;
function  FindFirstU(const Path: UnicodeString; Attr: Integer; var F: TSearchRec): Integer;

function  FileExistsB(const FileName: RawByteString): Boolean;
function  FileExistsU(const FileName: UnicodeString): Boolean;
function  FileExists(const FileName: String): Boolean;

function  FileGetSizeB(const FileName: RawByteString): Int64;
function  FileGetSizeU(const FileName: UnicodeString): Int64;
function  FileGetSize(const FileName: String): Int64;

function  FileGetDateTimeB(const FileName: RawByteString): TDateTime;
function  FileGetDateTime(const FileName: String): TDateTime;

function  FileGetDateTime2(const FileName: String): TDateTime;

function  FileIsReadOnly(const FileName: String): Boolean;

procedure FileDeleteEx(const FileName: String);

procedure FileRenameEx(const OldFileName, NewFileName: String);

function  ReadFileBufB(
          const FileName: RawByteString;
          var Buf; const BufSize: Integer;
          const FileSharing: TFileSharing = fsDenyNone;
          const FileCreationMode: TFileCreationMode = fcOpenExisting;
          const FileOpenWait: PFileOpenWait = nil): Integer;
function  ReadFileBufU(
          const FileName: UnicodeString;
          var Buf; const BufSize: Integer;
          const FileSharing: TFileSharing = fsDenyNone;
          const FileCreationMode: TFileCreationMode = fcOpenExisting;
          const FileOpenWait: PFileOpenWait = nil): Integer;
function  ReadFileBuf(
          const FileName: String;
          var Buf; const BufSize: Integer;
          const FileSharing: TFileSharing = fsDenyNone;
          const FileCreationMode: TFileCreationMode = fcOpenExisting;
          const FileOpenWait: PFileOpenWait = nil): Integer;

function  ReadFileRawStrB(
          const FileName: RawByteString;
          const FileSharing: TFileSharing = fsDenyNone;
          const FileCreationMode: TFileCreationMode = fcOpenExisting;
          const FileOpenWait: PFileOpenWait = nil): RawByteString;
function  ReadFileRawStrU(
          const FileName: UnicodeString;
          const FileSharing: TFileSharing = fsDenyNone;
          const FileCreationMode: TFileCreationMode = fcOpenExisting;
          const FileOpenWait: PFileOpenWait = nil): RawByteString;
function  ReadFileRawStr(
          const FileName: String;
          const FileSharing: TFileSharing = fsDenyNone;
          const FileCreationMode: TFileCreationMode = fcOpenExisting;
          const FileOpenWait: PFileOpenWait = nil): RawByteString;

function  ReadFileBytesB(
          const FileName: RawByteString;
          const FileSharing: TFileSharing = fsDenyNone;
          const FileCreationMode: TFileCreationMode = fcOpenExisting;
          const FileOpenWait: PFileOpenWait = nil): TBytes;
function  ReadFileBytesU(
          const FileName: UnicodeString;
          const FileSharing: TFileSharing = fsDenyNone;
          const FileCreationMode: TFileCreationMode = fcOpenExisting;
          const FileOpenWait: PFileOpenWait = nil): TBytes;
function  ReadFileBytes(
          const FileName: String;
          const FileSharing: TFileSharing = fsDenyNone;
          const FileCreationMode: TFileCreationMode = fcOpenExisting;
          const FileOpenWait: PFileOpenWait = nil): TBytes;

procedure WriteFileBufB(
          const FileName: RawByteString;
          const Buf; const BufSize: Integer;
          const FileSharing: TFileSharing = fsDenyReadWrite;
          const FileCreationMode: TFileCreationMode = fcCreateAlways;
          const FileOpenWait: PFileOpenWait = nil);
procedure WriteFileBufU(
          const FileName: UnicodeString;
          const Buf; const BufSize: Integer;
          const FileSharing: TFileSharing = fsDenyReadWrite;
          const FileCreationMode: TFileCreationMode = fcCreateAlways;
          const FileOpenWait: PFileOpenWait = nil);
procedure WriteFileBuf(
          const FileName: String;
          const Buf; const BufSize: Integer;
          const FileSharing: TFileSharing = fsDenyReadWrite;
          const FileCreationMode: TFileCreationMode = fcCreateAlways;
          const FileOpenWait: PFileOpenWait = nil);

procedure WriteFileRawStrB(
          const FileName: RawByteString;
          const Buf: RawByteString;
          const FileSharing: TFileSharing = fsDenyReadWrite;
          const FileCreationMode: TFileCreationMode = fcCreateAlways;
          const FileOpenWait: PFileOpenWait = nil);
procedure WriteFileRawStrU(
          const FileName: UnicodeString;
          const Buf: RawByteString;
          const FileSharing: TFileSharing = fsDenyReadWrite;
          const FileCreationMode: TFileCreationMode = fcCreateAlways;
          const FileOpenWait: PFileOpenWait = nil);
procedure WriteFileRawStr(
          const FileName: String;
          const Buf: RawByteString;
          const FileSharing: TFileSharing = fsDenyReadWrite;
          const FileCreationMode: TFileCreationMode = fcCreateAlways;
          const FileOpenWait: PFileOpenWait = nil);

procedure WriteFileBytesB(
          const FileName: RawByteString;
          const Buf: TBytes;
          const FileSharing: TFileSharing = fsDenyReadWrite;
          const FileCreationMode: TFileCreationMode = fcCreateAlways;
          const FileOpenWait: PFileOpenWait = nil);
procedure WriteFileBytesU(
          const FileName: UnicodeString;
          const Buf: TBytes;
          const FileSharing: TFileSharing = fsDenyReadWrite;
          const FileCreationMode: TFileCreationMode = fcCreateAlways;
          const FileOpenWait: PFileOpenWait = nil);
procedure WriteFileBytes(
          const FileName: String;
          const Buf: TBytes;
          const FileSharing: TFileSharing = fsDenyReadWrite;
          const FileCreationMode: TFileCreationMode = fcCreateAlways;
          const FileOpenWait: PFileOpenWait = nil);

procedure AppendFileB(
          const FileName: RawByteString;
          const Buf; const BufSize: Integer;
          const FileSharing: TFileSharing = fsDenyWrite;
          const FileCreationMode: TFileCreationMode = fcOpenExisting;
          const FileOpenWait: PFileOpenWait = nil);
procedure AppendFileU(
          const FileName: UnicodeString;
          const Buf; const BufSize: Integer;
          const FileSharing: TFileSharing = fsDenyWrite;
          const FileCreationMode: TFileCreationMode = fcOpenAlways;
          const FileOpenWait: PFileOpenWait = nil);
procedure AppendFile(
          const FileName: String;
          const Buf; const BufSize: Integer;
          const FileSharing: TFileSharing = fsDenyWrite;
          const FileCreationMode: TFileCreationMode = fcOpenAlways;
          const FileOpenWait: PFileOpenWait = nil);

procedure AppendFileRawStrB(
          const FileName: RawByteString;
          const Buf: RawByteString;
          const FileSharing: TFileSharing = fsDenyWrite;
          const FileCreationMode: TFileCreationMode = fcOpenAlways;
          const FileOpenWait: PFileOpenWait = nil);
procedure AppendFileRawStrU(
          const FileName: UnicodeString;
          const Buf: RawByteString;
          const FileSharing: TFileSharing = fsDenyWrite;
          const FileCreationMode: TFileCreationMode = fcOpenAlways;
          const FileOpenWait: PFileOpenWait = nil);
procedure AppendFileRawStr(
          const FileName: String;
          const Buf: RawByteString;
          const FileSharing: TFileSharing = fsDenyWrite;
          const FileCreationMode: TFileCreationMode = fcOpenAlways;
          const FileOpenWait: PFileOpenWait = nil);



{                                                                              }
{ Directory entries                                                            }
{                                                                              }
function  DirectoryEntryExistsB(const Name: RawByteString): Boolean;
function  DirectoryEntryExistsU(const Name: UnicodeString): Boolean;
function  DirectoryEntryExists(const Name: String): Boolean;

function  DirectoryEntrySizeB(const Name: RawByteString): Int64;
function  DirectoryEntrySizeU(const Name: UnicodeString): Int64;
function  DirectoryEntrySize(const Name: String): Int64;

function  DirectoryExistsB(const DirectoryName: RawByteString): Boolean;
function  DirectoryExistsU(const DirectoryName: UnicodeString): Boolean;
function  DirectoryExists(const DirectoryName: String): Boolean;

function  DirectoryGetDateTimeB(const DirectoryName: RawByteString): TDateTime;
function  DirectoryGetDateTimeU(const DirectoryName: UnicodeString): TDateTime;
function  DirectoryGetDateTime(const DirectoryName: String): TDateTime;

function  GetFirstFileNameMatchingB(const FileMask: RawByteString): RawByteString;
function  GetFirstFileNameMatchingU(const FileMask: UnicodeString): UnicodeString;
function  GetFirstFileNameMatching(const FileMask: String): String;

function  DirEntryGetAttrB(const FileName: RawByteString): Integer;
function  DirEntryGetAttrU(const FileName: UnicodeString): Integer;
function  DirEntryGetAttr(const FileName: String): Integer;

function  DirEntryIsDirectoryB(const FileName: RawByteString): Boolean;
function  DirEntryIsDirectoryU(const FileName: UnicodeString): Boolean;
function  DirEntryIsDirectory(const FileName: String): Boolean;

function  FileHasAttr(const FileName: String; const Attr: Word): Boolean;
function  FileHasAttrB(const FileName: RawByteString; const Attr: Word): Boolean;
function  FileHasAttrU(const FileName: UnicodeString; const Attr: Word): Boolean;

procedure DirectoryCreateB(const DirectoryName: RawByteString);
procedure DirectoryCreateU(const DirectoryName: UnicodeString);
procedure DirectoryCreate(const DirectoryName: String);



{                                                                              }
{ File operations                                                              }
{   MoveFile first attempts a rename, then a copy and delete.                  }
{                                                                              }
procedure CopyFile(const FileName, DestName: String);
procedure MoveFile(const FileName, DestName: String);
function  DeleteFiles(const FileMask: String): Boolean;



{$IFDEF MSWINDOWS}
{                                                                              }
{ Logical Drive functions                                                      }
{                                                                              }
type
  TLogicalDriveType = (
      DriveRemovable,
      DriveFixed,
      DriveRemote,
      DriveCDRom,
      DriveRamDisk,
      DriveTypeUnknown);

function  DriveIsValidA(const Drive: ByteChar): Boolean;
function  DriveIsValidW(const Drive: WideChar): Boolean;

function  DriveGetTypeA(const Path: AnsiString): TLogicalDriveType;

function  DriveFreeSpaceA(const Path: AnsiString): Int64;
function  DriveSizeA(const Path: AnsiString): Int64;
{$ENDIF}



{                                                                              }
{ Test cases                                                                   }
{                                                                              }
{$IFDEF DEBUG}
{$IFDEF TEST}
procedure Test;
{$ENDIF}
{$ENDIF}



implementation

uses
  { System }
  {$IFDEF DELPHI}{$IFDEF POSIX}
  Posix.Unistd,
  Posix.Stdio,
  {$ENDIF}{$ENDIF}
  {$IFDEF FREEPASCAL}{$IFDEF UNIX}
  BaseUnix,
  Unix,
  {$ENDIF}{$ENDIF}

  { Fundamentals }
  flcDynArrays,
  flcUtils,
  flcStrings,
  flcSysUtils;



{$IFDEF DELPHI6_UP}
  {$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}


resourcestring
  SCannotOpenFile          = 'Cannot open file: %s: %s';
  SCannotCreateFile        = 'Cannot create file: %s: %s';
  SCannotMoveFile          = 'Cannot move file: %s: %s';
  SFileSizeError           = 'File size error: %s';
  SFileReadError           = 'File read error: %s';
  SFileWriteError          = 'File write error: %s: %s';
  SInvalidFileCreationMode = 'Invalid file creation mode';
  SFileExists              = 'File exists: %s';
  SFileDoesNotExist        = 'File does not exist: %s';
  SInvalidFileHandle       = 'Invalid file handle';
  SInvalidFilePosition     = 'Invalid file position';
  SFileSeekError           = 'File seek error: %s';
  SInvalidFileName         = 'Invalid file name';
  SInvalidPath             = 'Invalid path';
  SFileDeleteError         = 'File delete error: %s';
  SInvalidFileAccess       = 'Invalid file access';
  SInvalidFileSharing      = 'Invalid file sharing';
  SCannotCreateDirectory   = 'Cannot create directory: %s: %s';



{                                                                              }
{ Path functions                                                               }
{                                                                              }
function WinPathHasDriveLetterB(const Path: RawByteString): Boolean;
begin
  Result := False;
  if Length(Path) < 2 then
    exit;
  case Path[1] of
    'A'..'Z', 'a'..'z' : ;
  else
    exit;
  end;
  if Path[2] <> ':' then
    exit;
  Result := True;
end;

function WinPathHasDriveLetterU(const Path: UnicodeString): Boolean;
begin
  Result := False;
  if Length(Path) < 2 then
    exit;
  case Path[1] of
    'A'..'Z', 'a'..'z' : ;
  else
    exit;
  end;
  if Path[2] <> ':' then
    exit;
  Result := True;
end;

function WinPathHasDriveLetter(const Path: String): Boolean;
begin
  Result := False;
  if Length(Path) < 2 then
    exit;
  case Path[1] of
    'A'..'Z', 'a'..'z' : ;
  else
    exit;
  end;
  if Path[2] <> ':' then
    exit;
  Result := True;
end;

function WinPathIsDriveLetterB(const Path: RawByteString): Boolean;
begin
  Result := (Length(Path) = 2) and WinPathHasDriveLetterB(Path);
end;

function WinPathIsDriveLetterU(const Path: UnicodeString): Boolean;
begin
  Result := (Length(Path) = 2) and WinPathHasDriveLetterU(Path);
end;

function WinPathIsDriveLetter(const Path: String): Boolean;
begin
  Result := (Length(Path) = 2) and WinPathHasDriveLetter(Path);
end;

function WinPathIsDriveRootB(const Path: RawByteString): Boolean;
begin
  Result := (Length(Path) = 3) and WinPathHasDriveLetterB(Path) and
            (Path[3] = '\');
end;

function WinPathIsDriveRootU(const Path: UnicodeString): Boolean;
begin
  Result := (Length(Path) = 3) and WinPathHasDriveLetterU(Path) and
            (Path[3] = '\');
end;

function WinPathIsDriveRoot(const Path: String): Boolean;
begin
  Result := (Length(Path) = 3) and WinPathHasDriveLetter(Path) and
            (Path[3] = '\');
end;

function PathIsRootB(const Path: RawByteString): Boolean;
begin
  Result := ((Length(Path) = 1) and (Path[1] in csSlash)) or
            WinPathIsDriveRootB(Path);
end;

function PathIsRootU(const Path: UnicodeString): Boolean;
begin
  Result := WinPathIsDriveRootU(Path);
  if Result then
    exit;
  if Length(Path) = 1 then
    case Path[1] of
      '/', '\' : Result := True;
    end;
end;

function PathIsRoot(const Path: String): Boolean;
begin
  Result := WinPathIsDriveRoot(Path);
  if Result then
    exit;
  if Length(Path) = 1 then
    case Path[1] of
      '/', '\' : Result := True;
    end;
end;

function PathIsUNCPathB(const Path: RawByteString): Boolean;
var P: PByteChar;
begin
  Result := False;
  if Length(Path) < 2 then
    exit;
  P := Pointer(Path);
  if P^ <> '\' then
    exit;
  Inc(P);
  if P^ <> '\' then
    exit;
  Result := True;
end;

function PathIsUNCPathU(const Path: UnicodeString): Boolean;
begin
  Result := False;
  if Length(Path) < 2 then
    exit;
  if Path[1] <> '\' then
    exit;
  if Path[2] <> '\' then
    exit;
  Result := True;
end;

function PathIsUNCPath(const Path: String): Boolean;
begin
  Result := False;
  if Length(Path) < 2 then
    exit;
  if Path[1] <> '\' then
    exit;
  if Path[2] <> '\' then
    exit;
  Result := True;
end;

function PathIsAbsoluteB(const Path: RawByteString): Boolean;
begin
  if Path = '' then
    Result := False else
  if WinPathHasDriveLetterB(Path) then
    Result := True else
  if PByteChar(Pointer(Path))^ in ['\', '/'] then
    Result := True
  else
    Result := False;
end;

function PathIsAbsoluteU(const Path: UnicodeString): Boolean;
begin
  if Path = '' then
    Result := False else
  if WinPathHasDriveLetterU(Path) then
    Result := True
  else
    case Path[1] of
      '\', '/' : Result := True;
    else
      Result := False;
    end;
end;

function PathIsAbsolute(const Path: String): Boolean;
begin
  if Path = '' then
    Result := False else
  if WinPathHasDriveLetter(Path) then
    Result := True
  else
    case Path[1] of
      '\', '/' : Result := True;
    else
      Result := False;
    end;
end;

function PathIsDirectoryB(const Path: RawByteString): Boolean;
var L: Integer;
    P: PByteChar;
begin
  L := Length(Path);
  if L = 0 then
    Result := False else
  if (L = 2) and WinPathHasDriveLetterB(Path) then
    Result := True else
    begin
      P := Pointer(Path);
      Inc(P, L - 1);
      Result := P^ in csSlash;
    end;
end;

function PathIsDirectoryU(const Path: UnicodeString): Boolean;
var L: Integer;
begin
  L := Length(Path);
  if L = 0 then
    Result := False else
  if (L = 2) and WinPathHasDriveLetterU(Path) then
    Result := True
  else
    case Path[L] of
      '/', '\' : Result := True;
    else
      Result := False;
    end;
end;

function PathIsDirectory(const Path: String): Boolean;
var L: Integer;
begin
  L := Length(Path);
  if L = 0 then
    Result := False else
  if (L = 2) and WinPathHasDriveLetter(Path) then
    Result := True
  else
    case Path[L] of
      '/', '\' : Result := True;
    else
      Result := False;
    end;
end;

function PathInclSuffixB(const Path: RawByteString; const PathSep: ByteChar): RawByteString;
var L: Integer;
begin
  L := Length(Path);
  if L = 0 then
    Result := ''
  else
    begin
      if Path[L] = PathSep then
        Result := Path
      else
        Result := Path + PathSep;
    end;
end;

function PathInclSuffixU(const Path: UnicodeString; const PathSep: WideChar): UnicodeString;
var L: Integer;
begin
  L := Length(Path);
  if L = 0 then
    Result := ''
  else
    begin
      if Path[L] = PathSep then
        Result := Path
      else
        Result := Path + PathSep;
    end;
end;

function PathInclSuffix(const Path: String; const PathSep: Char): String;
var L: Integer;
begin
  L := Length(Path);
  if L = 0 then
    Result := ''
  else
    begin
      if Path[L] = PathSep then
        Result := Path
      else
        Result := Path + PathSep;
    end;
end;

procedure PathEnsureSuffixB(var Path: RawByteString; const PathSep: ByteChar);
begin
  Path := PathInclSuffixB(Path, PathSep);
end;

procedure PathEnsureSuffixU(var Path: UnicodeString; const PathSep: WideChar);
begin
  Path := PathInclSuffixU(Path, PathSep);
end;

procedure PathEnsureSuffix(var Path: String; const PathSep: Char);
begin
  Path := PathInclSuffix(Path, PathSep);
end;

procedure PathEnsureNoSuffixB(var Path: RawByteString; const PathSep: ByteChar);
begin
  Path := PathExclSuffixB(Path, PathSep);
end;

procedure PathEnsureNoSuffixU(var Path: UnicodeString; const PathSep: WideChar);
begin
  Path := PathExclSuffixU(Path, PathSep);
end;

procedure PathEnsureNoSuffix(var Path: String; const PathSep: Char);
begin
  Path := PathExclSuffix(Path, PathSep);
end;

function PathExclSuffixB(const Path: RawByteString; const PathSep: ByteChar): RawByteString;
var L: Integer;
begin
  L := Length(Path);
  if L = 0 then
    Result := '' else
    begin
      if Path[L] = PathSep then
        Result := Copy(Path, 1, L - 1)
      else
        Result := Path;
    end;
end;

function PathExclSuffixU(const Path: UnicodeString; const PathSep: WideChar): UnicodeString;
var L: Integer;
begin
  L := Length(Path);
  if L = 0 then
    Result := '' else
    begin
      if Path[L] = PathSep then
        Result := Copy(Path, 1, L - 1)
      else
        Result := Path;
    end;
end;

function PathExclSuffix(const Path: String; const PathSep: Char): String;
var L: Integer;
begin
  L := Length(Path);
  if L = 0 then
    Result := '' else
    begin
      if Path[L] = PathSep then
        Result := Copy(Path, 1, L - 1)
      else
        Result := Path;
    end;
end;

function PathCanonicalB(const Path: RawByteString; const PathSep: ByteChar): RawByteString;
var L, M : Integer;
    I, J : Integer;
    P    : RawByteStringArray;
    Q    : PByteChar;
begin
  Result := Path;
  // \.\ references
  M := Length(Result);
  repeat
    L := M;
    if L = 0 then
      exit;
    Result := StrReplaceB('\.\', '\', Result);
    Result := StrReplaceB('/./', '/', Result);
    M := Length(Result);
  until L = M;
  // .\ prefix
  StrEnsureNoPrefixB(Result, '.\');
  StrEnsureNoPrefixB(Result, './');
  // \. suffix
  StrEnsureNoSuffixB(Result, '\.');
  StrEnsureNoSuffixB(Result, '/.');
  // ..
  if PosStrB('..', Result) > 0 then
    begin
      P := StrSplitCharB(Result, PathSep);
      repeat
        J := -1;
        For I := Length(P) - 1 downto 0 do
          if P[I] = '..' then
            begin
              J := I;
              break;
            end;
        if J = -1 then
          break;
        M := -1;
        For I := J - 1 downto 0 do
          if (P[I] = '') or ((I = 0) and WinPathHasDriveLetterB(P[I])) then
            break else
          if P[I] <> '..' then
            begin
              M := I;
              break;
            end;
        if M = -1 then
          break;
        DynArrayRemoveB(P, J, 1);
        DynArrayRemoveB(P, M, 1);
      until False;
      Result := StrJoinCharB(P, PathSep);
    end;
  // \..\ prefix
  while StrMatchLeftB(Result, '\..\') do
    Delete(Result, 1, 3);
  while StrMatchLeftB(Result, '/../') do
    Delete(Result, 1, 3);
  if (Result = '\..') or (Result = '/..') then
    Result := '';
  L := Length(Result);
  if L = 0 then
    exit;
  // X:\..\ prefix
  Q := Pointer(Result);
  if Q^ in ['A'..'Z', 'a'..'z'] then
    begin
      if StrMatchB(Result, ':\..\', 2) then
        Delete(Result, 4, 3) else
      if (L = 5) and StrMatchB(Result, ':\..', 2) then
        begin
          SetLength(Result, 2);
          exit;
        end;
      L := Length(Result);
    end;
  // single dot
  Q := Pointer(Result);
  if L = 1 then
    begin
      if Q^ = '.' then
        Result := '';
      exit;
    end;
  // final dot
  Inc(Q, L - 2);
  if not (Q^ in ['.', '\', '/', ':']) then
    begin
      Inc(Q);
      if Q^ = '.' then
        Delete(Result, L, 1);
    end;
end;

function PathCanonicalU(const Path: UnicodeString; const PathSep: WideChar): UnicodeString;
var L, M : Integer;
    I, J : Integer;
    P    : UnicodeStringArray;
    Q    : PWideChar;
    C    : WideChar;
begin
  Result := Path;
  // \.\ references
  M := Length(Result);
  repeat
    L := M;
    if L = 0 then
      exit;
    Result := StrReplaceU('\.\', '\', Result);
    Result := StrReplaceU('/./', '/', Result);
    M := Length(Result);
  until L = M;
  // .\ prefix
  StrEnsureNoPrefixU(Result, '.\');
  StrEnsureNoPrefixU(Result, './');
  // \. suffix
  StrEnsureNoSuffixU(Result, '\.');
  StrEnsureNoSuffixU(Result, '/.');
  // ..
  if PosStrU('..', Result) > 0 then
    begin
      P := StrSplitCharU(Result, PathSep);
      repeat
        J := -1;
        For I := Length(P) - 1 downto 0 do
          if P[I] = '..' then
            begin
              J := I;
              break;
            end;
        if J = -1 then
          break;
        M := -1;
        For I := J - 1 downto 0 do
          if (P[I] = '') or ((I = 0) and WinPathHasDriveLetterU(P[I])) then
            break else
          if P[I] <> '..' then
            begin
              M := I;
              break;
            end;
        if M = -1 then
          break;
        DynArrayRemoveU(P, J, 1);
        DynArrayRemoveU(P, M, 1);
      until False;
      Result := StrJoinCharU(P, PathSep);
    end;
  // \..\ prefix
  while StrMatchLeftU(Result, '\..\') do
    Delete(Result, 1, 3);
  while StrMatchLeftU(Result, '/../') do
    Delete(Result, 1, 3);
  if (Result = '\..') or (Result = '/..') then
    Result := '';
  L := Length(Result);
  if L = 0 then
    exit;
  // X:\..\ prefix
  Q := Pointer(Result);
  case Q^ of
    'A'..'Z',
    'a'..'z' :
      begin
        if StrMatchU(Result, ':\..\', 2) then
          Delete(Result, 4, 3) else
        if (L = 5) and StrMatchU(Result, ':\..', 2) then
          begin
            SetLength(Result, 2);
            exit;
          end;
        L := Length(Result);
      end;
  end;
  // single dot
  Q := Pointer(Result);
  if L = 1 then
    begin
      if Q^ = '.' then
        Result := '';
      exit;
    end;
  // final dot
  Inc(Q, L - 2);
  C := Q^;
  if not ((C = '.') or (C = '\') or (C = '/') or (C = ':')) then
    begin
      Inc(Q);
      if Q^ = '.' then
        Delete(Result, L, 1);
    end;
end;

function PathCanonical(const Path: String; const PathSep: Char): String;
var L, M : Integer;
    I, J : Integer;
    P    : StringArray;
    Q    : PChar;
    C    : Char;
begin
  Result := Path;
  // \.\ references
  M := Length(Result);
  repeat
    L := M;
    if L = 0 then
      exit;
    Result := StrReplace('\.\', '\', Result);
    Result := StrReplace('/./', '/', Result);
    M := Length(Result);
  until L = M;
  // .\ prefix
  StrEnsureNoPrefix(Result, '.\');
  StrEnsureNoPrefix(Result, './');
  // \. suffix
  StrEnsureNoSuffix(Result, '\.');
  StrEnsureNoSuffix(Result, '/.');
  // ..
  if PosStr('..', Result) > 0 then
    begin
      P := StrSplitChar(Result, PathSep);
      repeat
        J := -1;
        For I := Length(P) - 1 downto 0 do
          if P[I] = '..' then
            begin
              J := I;
              break;
            end;
        if J = -1 then
          break;
        M := -1;
        For I := J - 1 downto 0 do
          if (P[I] = '') or ((I = 0) and WinPathHasDriveLetter(P[I])) then
            break else
          if P[I] <> '..' then
            begin
              M := I;
              break;
            end;
        if M = -1 then
          break;
        DynArrayRemove(P, J, 1);
        DynArrayRemove(P, M, 1);
      until False;
      Result := StrJoinChar(P, PathSep);
    end;
  // \..\ prefix
  while StrMatchLeft(Result, '\..\') do
    Delete(Result, 1, 3);
  while StrMatchLeft(Result, '/../') do
    Delete(Result, 1, 3);
  if (Result = '\..') or (Result = '/..') then
    Result := '';
  L := Length(Result);
  if L = 0 then
    exit;
  // X:\..\ prefix
  Q := Pointer(Result);
  C := Q^;
  if ((C >= 'A') and (C <= 'Z')) or
     ((C >= 'a') and (C <= 'z')) then
    begin
      if StrMatch(Result, ':\..\', 2) then
        Delete(Result, 4, 3) else
      if (L = 5) and StrMatch(Result, ':\..', 2) then
        begin
          SetLength(Result, 2);
          exit;
        end;
      L := Length(Result);
    end;
  // single dot
  Q := Pointer(Result);
  if L = 1 then
    begin
      if Q^ = '.' then
        Result := '';
      exit;
    end;
  // final dot
  Inc(Q, L - 2);
  C := Q^;
  if not ((C = '.') or (C = '\') or (C = '/') or (C = ':')) then
    begin
      Inc(Q);
      if Q^ = '.' then
        Delete(Result, L, 1);
    end;
end;

function PathExpandB(const Path: RawByteString; const BasePath: RawByteString;
    const PathSep: ByteChar): RawByteString;
begin
  if Path = '' then
    Result := BasePath else
  if PathIsAbsoluteB(Path) then
    Result := Path else
    Result := PathInclSuffixB(BasePath, PathSep) + Path;
  Result := PathCanonicalB(Result, PathSep);
end;

function PathExpandU(const Path: UnicodeString; const BasePath: UnicodeString;
    const PathSep: WideChar): UnicodeString;
begin
  if Path = '' then
    Result := BasePath else
  if PathIsAbsoluteU(Path) then
    Result := Path else
    Result := PathInclSuffixU(BasePath, PathSep) + Path;
  Result := PathCanonicalU(Result, PathSep);
end;

function PathExpand(const Path: String; const BasePath: String;
    const PathSep: Char): String;
begin
  if Path = '' then
    Result := BasePath else
  if PathIsAbsolute(Path) then
    Result := Path else
    Result := PathInclSuffix(BasePath, PathSep) + Path;
  Result := PathCanonical(Result, PathSep);
end;

function PathLeftElementB(const Path: RawByteString; const PathSep: ByteChar): RawByteString;
var I: Integer;
begin
  I := PosCharB(PathSep, Path);
  if I <= 0 then
    Result := Path
  else
    Result := Copy(Path, 1, I - 1);
end;

function PathLeftElementU(const Path: UnicodeString; const PathSep: WideChar): UnicodeString;
var I: Integer;
begin
  I := PosCharU(PathSep, Path);
  if I <= 0 then
    Result := Path
  else
    Result := Copy(Path, 1, I - 1);
end;

function PathLeftElement(const Path: String; const PathSep: Char): String;
var I: Integer;
begin
  I := PosChar(PathSep, Path);
  if I <= 0 then
    Result := Path
  else
    Result := Copy(Path, 1, I - 1);
end;

procedure PathSplitLeftElementB(const Path: RawByteString;
    var LeftElement, RightPath: RawByteString; const PathSep: ByteChar);
var I: Integer;
begin
  I := PosCharB(PathSep, Path);
  if I <= 0 then
    begin
      LeftElement := Path;
      RightPath := '';
    end
  else
    begin
      LeftElement := Copy(Path, 1, I - 1);
      RightPath := CopyFromB(Path, I + 1);
    end;
end;

procedure PathSplitLeftElementU(const Path: UnicodeString;
          var LeftElement, RightPath: UnicodeString; const PathSep: WideChar);
var I: Integer;
begin
  I := PosCharU(PathSep, Path);
  if I <= 0 then
    begin
      LeftElement := Path;
      RightPath := '';
    end
  else
    begin
      LeftElement := Copy(Path, 1, I - 1);
      RightPath := CopyFromU(Path, I + 1);
    end;
end;

procedure PathSplitLeftElement(const Path: String;
    var LeftElement, RightPath: String; const PathSep: Char);
var I: Integer;
begin
  I := PosChar(PathSep, Path);
  if I <= 0 then
    begin
      LeftElement := Path;
      RightPath := '';
    end
  else
    begin
      LeftElement := Copy(Path, 1, I - 1);
      RightPath := CopyFrom(Path, I + 1);
    end;
end;

procedure DecodeFilePathB(const FilePath: RawByteString; var Path, FileName: RawByteString;
    const PathSep: ByteChar);
var I: Integer;
begin
  I := PosCharRevB(PathSep, FilePath);
  if I <= 0 then
    begin
      Path := '';
      FileName := FilePath;
    end
  else
    begin
      Path := Copy(FilePath, 1, I);
      FileName := CopyFromB(FilePath, I + 1);
    end;
end;

procedure DecodeFilePathU(const FilePath: UnicodeString; var Path, FileName: UnicodeString;
    const PathSep: WideChar);
var I: Integer;
begin
  I := PosCharRevU(PathSep, FilePath);
  if I <= 0 then
    begin
      Path := '';
      FileName := FilePath;
    end
  else
    begin
      Path := Copy(FilePath, 1, I);
      FileName := CopyFromU(FilePath, I + 1);
    end;
end;

procedure DecodeFilePath(const FilePath: String; var Path, FileName: String;
    const PathSep: Char);
var I: Integer;
begin
  I := PosCharRev(PathSep, FilePath);
  if I <= 0 then
    begin
      Path := '';
      FileName := FilePath;
    end
  else
    begin
      Path := Copy(FilePath, 1, I);
      FileName := CopyFrom(FilePath, I + 1);
    end;
end;

function PathExtractFilePathB(const FilePath: RawByteString;
         const PathSep: ByteChar): RawByteString;
var FileName : RawByteString;
begin
  DecodeFilePathB(FilePath, Result, FileName, PathSep);
end;

function PathExtractFilePathU(const FilePath: UnicodeString;
         const PathSep: WideChar): UnicodeString;
var FileName : UnicodeString;
begin
  DecodeFilePathU(FilePath, Result, FileName, PathSep);
end;

function PathExtractFilePath(const FilePath: String;
         const PathSep: Char): String;
var FileName : String;
begin
  DecodeFilePath(FilePath, Result, FileName, PathSep);
end;

function PathExtractFileNameB(const FilePath: RawByteString;
         const PathSep: ByteChar): RawByteString;
var Path : RawByteString;
begin
  DecodeFilePathB(FilePath, Path, Result, PathSep);
end;

function PathExtractFileNameU(const FilePath: UnicodeString;
         const PathSep: WideChar): UnicodeString;
var Path : UnicodeString;
begin
  DecodeFilePathU(FilePath, Path, Result, PathSep);
end;

function PathExtractFileName(const FilePath: String;
         const PathSep: Char): String;
var Path : String;
begin
  DecodeFilePath(FilePath, Path, Result, PathSep);
end;

function PathExtractFileExtB(const FilePath: RawByteString;
         const PathSep: ByteChar): RawByteString;
var FileName : RawByteString;
    I : Integer;
begin
  FileName := PathExtractFileNameB(FilePath, PathSep);
  I := PosCharRevB('.', FileName);
  if I <= 0 then
    begin
      Result := '';
      exit;
    end;
  Result := CopyFromB(FileName, I);
end;

function PathExtractFileExtU(const FilePath: UnicodeString;
         const PathSep: WideChar): UnicodeString;
var FileName : UnicodeString;
    I : Integer;
begin
  FileName := PathExtractFileNameU(FilePath, PathSep);
  I := PosCharRevU('.', FileName);
  if I <= 0 then
    begin
      Result := '';
      exit;
    end;
  Result := CopyFromU(FileName, I);
end;

function PathExtractFileExt(const FilePath: String;
         const PathSep: Char): String;
var FileName : String;
    I : Integer;
begin
  FileName := PathExtractFileName(FilePath, PathSep);
  I := PosCharRev('.', FileName);
  if I <= 0 then
    begin
      Result := '';
      exit;
    end;
  Result := CopyFrom(FileName, I);
end;

function FileNameCleanB(const FileName: RawByteString): RawByteString;
begin
  Result := StrReplaceCharB(['\', '/', ':', '>', '<', '*', '?'], '_', FileName);
  if Result = '.' then
    Result := '_' else
  if Result = '..' then
    Result := '__';
end;

function FileNameCleanU(const FileName: UnicodeString): UnicodeString;
begin
  Result := StrReplaceCharU(['\', '/', ':', '>', '<', '*', '?'], WideChar('_'), FileName);
  if Result = '.' then
    Result := '_' else
  if Result = '..' then
    Result := '__';
end;

function FileNameClean(const FileName: String): String;
begin
  Result := StrReplaceChar(['\', '/', ':', '>', '<', '*', '?'], '_', FileName);
  if Result = '.' then
    Result := '_' else
  if Result = '..' then
    Result := '__';
end;

function FilePathB(const FileName, Path: RawByteString; const BasePath: RawByteString;
    const PathSep: ByteChar): RawByteString;
var P, F: RawByteString;
begin
  F := FileNameCleanB(FileName);
  if F = '' then
    begin
      Result := '';
      exit;
    end;
  P := PathExpandB(Path, BasePath, PathSep);
  if P = '' then
    Result := F
  else
    Result := PathInclSuffixB(P, PathSep) + F;
end;

function FilePathU(const FileName, Path: UnicodeString; const BasePath: UnicodeString;
    const PathSep: WideChar): UnicodeString;
var P, F: UnicodeString;
begin
  F := FileNameCleanU(FileName);
  if F = '' then
    begin
      Result := '';
      exit;
    end;
  P := PathExpandU(Path, BasePath, PathSep);
  if P = '' then
    Result := F
  else
    Result := PathInclSuffixU(P, PathSep) + F;
end;

function FilePath(const FileName, Path: String; const BasePath: String;
    const PathSep: Char): String;
var P, F: String;
begin
  F := FileNameClean(FileName);
  if F = '' then
    begin
      Result := '';
      exit;
    end;
  P := PathExpand(Path, BasePath, PathSep);
  if P = '' then
    Result := F
  else
    Result := PathInclSuffix(P, PathSep) + F;
end;

function DirectoryExpandB(const Path: RawByteString; const BasePath: RawByteString;
    const PathSep: ByteChar): RawByteString;
begin
  Result := PathExpandB(PathInclSuffixB(Path, PathSep),
      PathInclSuffixB(BasePath, PathSep), PathSep);
end;

function DirectoryExpandU(const Path: UnicodeString; const BasePath: UnicodeString;
    const PathSep: WideChar): UnicodeString;
begin
  Result := PathExpandU(PathInclSuffixU(Path, PathSep),
      PathInclSuffixU(BasePath, PathSep), PathSep);
end;

function DirectoryExpand(const Path: String; const BasePath: String;
    const PathSep: Char): String;
begin
  Result := PathExpand(PathInclSuffix(Path, PathSep),
      PathInclSuffix(BasePath, PathSep), PathSep);
end;

function UnixPathToSafeWinPathB(const Path: RawByteString): RawByteString;
begin
  Result := StrReplaceCharB('/', '\',
              StrReplaceCharB(['\', ':', '<', '>', '|', '?', '*'], '_', Path));
end;

function WinPathToSafeUnixPathB(const Path: RawByteString): RawByteString;
begin
  Result := Path;
  if WinPathHasDriveLetterB(Path) then
    begin
      // X: -> \X
      Result[2] := Result[1];
      Result[1] := '\';
    end
  else
  if StrMatchLeftB(Path, '\\.\') then
    // \\.\ -> \
    Delete(Result, 1, 3)
  else
  if PathIsUNCPathB(Path) then
    // \\ -> \
    Delete(Result, 1, 1);
  Result := StrReplaceCharB('\', '/',
              StrReplaceCharB(['/', ':', '<', '>', '|', '?', '*'], '_', Result));
end;



{                                                                              }
{ System helper functions                                                      }
{                                                                              }
{$IFDEF WindowsPlatform}
function GetTick: LongWord;
begin
  Result := GetTickCount;
end;
{$ELSE}{$IFDEF UNIX}
function GetTick: LongWord;
begin
  Result := LongWord(DateTimeToTimeStamp(Now).Time);
end;
{$ENDIF}{$ENDIF}



{                                                                              }
{ File errors                                                                  }
{                                                                              }
constructor EFileError.Create(const FileError: TFileError; const Msg: string);
begin
  FFileError := FileError;
  inherited Create(Msg);
end;

constructor EFileError.CreateFmt(const FileError: TFileError; const Msg: string; const Args: array of const);
begin
  FFileError := FileError;
  inherited CreateFmt(Msg, Args);
end;

{$IFDEF MSWINDOWS}
function WinErrorCodeToFileError(const ErrorCode: LongWord): TFileError;
begin
  case ErrorCode of
    0                             : Result := feNone;
    ERROR_INVALID_HANDLE          : Result := feInvalidParameter;
    ERROR_FILE_NOT_FOUND,
    ERROR_PATH_NOT_FOUND          : Result := feFileDoesNotExist;
    ERROR_ALREADY_EXISTS,
    ERROR_FILE_EXISTS             : Result := feFileExists;
    ERROR_WRITE_PROTECT,
    ERROR_OPEN_FAILED             : Result := feFileOpenError;
    ERROR_CANNOT_MAKE             : Result := feFileCreateError;
    ERROR_NEGATIVE_SEEK           : Result := feFileSeekError;
    ERROR_ACCESS_DENIED,
    ERROR_NETWORK_ACCESS_DENIED   : Result := feAccessDenied;
    ERROR_SHARING_VIOLATION,
    ERROR_LOCK_VIOLATION,
    ERROR_SHARING_PAUSED,
    ERROR_LOCK_FAILED             : Result := feFileSharingError;
    ERROR_HANDLE_DISK_FULL,
    ERROR_DISK_FULL               : Result := feOutOfSpace;
    ERROR_BAD_NETPATH,
    ERROR_DIRECTORY,
    ERROR_INVALID_DRIVE           : Result := feInvalidFilePath;
    ERROR_INVALID_NAME,
    ERROR_FILENAME_EXCED_RANGE,
    ERROR_BAD_NET_NAME,
    ERROR_BUFFER_OVERFLOW         : Result := feInvalidFileName;
    ERROR_OUTOFMEMORY,
    ERROR_NOT_ENOUGH_MEMORY,
    ERROR_TOO_MANY_OPEN_FILES,
    ERROR_SHARING_BUFFER_EXCEEDED : Result := feOutOfResources;
    ERROR_SEEK,
    ERROR_READ_FAULT,
    ERROR_WRITE_FAULT,
    ERROR_GEN_FAILURE,
    ERROR_CRC,
    ERROR_NETWORK_BUSY,
    ERROR_NET_WRITE_FAULT,
    ERROR_REM_NOT_LIST,
    ERROR_DEV_NOT_EXIST,
    ERROR_NETNAME_DELETED         : Result := feDeviceFailure;
  else
    Result := feFileError;
  end;
end;
{$ENDIF}



{                                                                              }
{ File operations                                                              }
{                                                                              }

{$IFDEF MSWINDOWS}
function FileOpenFlagsToWinFileFlags(const FileOpenFlags: TFileOpenFlags): LongWord;
var
  FileFlags : LongWord;
begin
  FileFlags := 0;
  if foDeleteOnClose in FileOpenFlags then
    FileFlags := FileFlags or FILE_FLAG_DELETE_ON_CLOSE;
  if foNoBuffering in FileOpenFlags then
    FileFlags := FileFlags or FILE_FLAG_NO_BUFFERING;
  if foWriteThrough in FileOpenFlags then
    FileFlags := FileFlags or FILE_FLAG_WRITE_THROUGH;
  if foRandomAccessHint in FileOpenFlags then
    FileFlags := FileFlags or FILE_FLAG_RANDOM_ACCESS;
  if foSequentialScanHint in FileOpenFlags then
    FileFlags := FileFlags or FILE_FLAG_SEQUENTIAL_SCAN;
  Result := FileFlags;
end;

function FileCreationModeToWinFileCreateDisp(const FileCreationMode: TFileCreationMode): LongWord;
var
  FileCreateDisp : LongWord;
begin
  case FileCreationMode of
    fcCreateNew        : FileCreateDisp := CREATE_NEW;
    fcCreateAlways     : FileCreateDisp := CREATE_ALWAYS;
    fcOpenExisting     : FileCreateDisp := OPEN_EXISTING;
    fcOpenAlways       : FileCreateDisp := OPEN_ALWAYS;
    fcTruncateExisting : FileCreateDisp := TRUNCATE_EXISTING;
  else
    raise EFileError.Create(feInvalidParameter, SInvalidFileCreationMode);
  end;
  Result := FileCreateDisp;
end;

function FileSharingToWinFileShareMode(const FileSharing: TFileSharing): LongWord;
var
  FileShareMode : LongWord;
begin
  case FileSharing of
    fsDenyNone      : FileShareMode := FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE;
    fsDenyRead      : FileShareMode := FILE_SHARE_WRITE;
    fsDenyWrite     : FileShareMode := FILE_SHARE_READ;
    fsDenyReadWrite : FileShareMode := 0;
    fsExclusive     : FileShareMode := 0;
  else
    raise EFileError.Create(feInvalidParameter, SInvalidFileSharing);
  end;
  Result := FileShareMode;
end;

function FileAccessToWinFileOpenAccess(const FileAccess: TFileAccess): LongWord;
var
  FileOpenAccess : LongWord;
begin
  case FileAccess of
    faRead      : FileOpenAccess := GENERIC_READ;
    faWrite     : FileOpenAccess := GENERIC_WRITE;
    faReadWrite : FileOpenAccess := GENERIC_READ or GENERIC_WRITE;
  else
    raise EFileError.Create(feInvalidParameter, SInvalidFileAccess);
  end;
  Result := FileOpenAccess;
end;
{$ELSE}
function FileOpenShareMode(
         const FileAccess: TFileAccess;
         const FileSharing: TFileSharing): LongWord;
var FileShareMode : LongWord;
begin
  case FileAccess of
    faRead      : FileShareMode := fmOpenRead;
    faWrite     : FileShareMode := fmOpenWrite;
    faReadWrite : FileShareMode := fmOpenReadWrite;
  else
    raise EFileError.Create(feInvalidParameter, SInvalidFileAccess);
  end;
  case FileSharing of
    fsDenyNone      : FileShareMode := FileShareMode or fmShareDenyNone;
    fsDenyRead      : FileShareMode := FileShareMode{$IFDEF MSWINDOWS} or fmShareDenyRead{$ENDIF};
    fsDenyWrite     : FileShareMode := FileShareMode or fmShareDenyWrite;
    fsDenyReadWrite : FileShareMode := FileShareMode{$IFDEF MSWINDOWS} or fmShareDenyRead{$ENDIF} or fmShareDenyWrite;
    fsExclusive     : FileShareMode := FileShareMode or fmShareExclusive;
  else
    raise EFileError.Create(feInvalidParameter, SInvalidFileSharing);
  end;
  Result := FileShareMode;
end;

function FileCreateWithShareMode(
         const FileName: String;
         const FileShareMode: LongWord): Integer;
var FileHandle : Integer;
begin
  FileHandle := FileCreate(FileName);
  if FileHandle < 0 then
    begin
      Result := -1;
      exit;
    end;
  FileClose(FileHandle);
  FileHandle := FileOpen(FileName, FileShareMode);
  Result := FileHandle;
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
procedure DoFileOpenWait(const FileOpenWait: PFileOpenWait; const WaitStart: LongWord; var Retry: Boolean);

  function GetRandomRetryWait: Integer;
  var Seed : Integer;
      ExtraWaitTime : Integer;
  begin
    if not FileOpenWait^.RetryRandomise then
      ExtraWaitTime := 0
    else
    if FileOpenWait^.RetryInterval < 0 then
      ExtraWaitTime := 0
    else
      begin
        ExtraWaitTime := FileOpenWait^.RetryInterval div 8;
        Seed := Integer(GetTick) xor Integer(FileOpenWait);
        ExtraWaitTime := Seed mod ExtraWaitTime;
      end;
    Result := ExtraWaitTime;
  end;

var
  WaitTime : LongWord;
  WaitResult : LongInt;
begin
  Assert(Assigned(FileOpenWait));

  Retry := False;
  if FileOpenWait^.Signal <> 0 then
    begin
      if FileOpenWait^.RetryInterval < 0 then
        WaitTime := INFINITE
      else
        WaitTime := FileOpenWait^.RetryInterval + GetRandomRetryWait;
      WaitResult := WaitForSingleObject(FileOpenWait^.Signal, WaitTime);
      if WaitResult = WAIT_TIMEOUT then
        Retry := True;
    end
  else
  if Assigned(FileOpenWait^.Callback) then
    begin
      FileOpenWait^.Aborted := False;
      FileOpenWait^.Callback(FileOpenWait);
      if not FileOpenWait^.Aborted then
        Retry := True;
    end
  else
    begin
      Sleep(MaxInt(0, FileOpenWait^.RetryInterval) + GetRandomRetryWait);
      Retry := True;
    end;
  if Retry then
    if LongInt(Int64(GetTick) - Int64(WaitStart)) >= FileOpenWait^.Timeout then
      Retry := False;
end;
{$ENDIF}

function FileOpenExB(
         const FileName: RawByteString;
         const FileAccess: TFileAccess;
         const FileSharing: TFileSharing;
         const FileOpenFlags: TFileOpenFlags;
         const FileCreationMode: TFileCreationMode;
         const FileOpenWait: PFileOpenWait): TFileHandle;
var FileHandle     : Integer;
    FileShareMode  : LongWord;
    {$IFDEF MSWINDOWS}
    FileOpenAccess : LongWord;
    FileFlags      : LongWord;
    FileCreateDisp : LongWord;
    ErrorCode      : LongWord;
    ErrorSharing   : Boolean;
    Retry          : Boolean;
    WaitStart      : LongWord;
    WaitOpen       : Boolean;
    {$ELSE}
    FileNameStr    : String;
    {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  FileFlags := FileOpenFlagsToWinFileFlags(FileOpenFlags);
  FileCreateDisp := FileCreationModeToWinFileCreateDisp(FileCreationMode);
  FileShareMode := FileSharingToWinFileShareMode(FileSharing);
  FileOpenAccess := FileAccessToWinFileOpenAccess(FileAccess);
  WaitOpen := False;
  WaitStart := 0;
  if Assigned(FileOpenWait) then
    if FileOpenWait^.Wait and (FileOpenWait^.Timeout > 0) then
      begin
        WaitOpen := True;
        WaitStart := GetTick;
        if Assigned(FileOpenWait^.Callback) then
          FileOpenWait^.Aborted := False;
      end;
  Retry := False;
  repeat
    FileHandle := Integer(Windows.CreateFileA(
        PAnsiChar(FileName),
        FileOpenAccess,
        FileShareMode,
        nil,
        FileCreateDisp,
        FileFlags,
        0));
    if FileHandle < 0 then
      begin
        ErrorCode := GetLastError;
        ErrorSharing :=
            (ErrorCode = ERROR_SHARING_VIOLATION) or
            (ErrorCode = ERROR_LOCK_VIOLATION);
      end
    else
      begin
        ErrorCode := 0;
        ErrorSharing := False;
      end;
    if WaitOpen and ErrorSharing then
      DoFileOpenWait(FileOpenWait, WaitStart, Retry);
  until not Retry;
  if FileHandle < 0 then
    raise EFileError.CreateFmt(WinErrorCodeToFileError(ErrorCode), SCannotOpenFile,
        [GetLastOSErrorMessage, FileName]);
  {$ELSE}
  FileNameStr := ToStringB(FileName);
  FileShareMode := FileOpenShareMode(FileAccess, FileSharing);
  case FileCreationMode of
    fcCreateNew :
      if FileExists(FileNameStr) then
        raise EFileError.CreateFmt(feFileExists, SFileExists, [FileNameStr])
      else
        FileHandle := FileCreateWithShareMode(FileNameStr, FileShareMode);
    fcCreateAlways :
      FileHandle := FileCreateWithShareMode(FileNameStr, FileShareMode);
    fcOpenExisting :
      FileHandle := FileOpen(FileNameStr, FileShareMode);
    fcOpenAlways :
      if not FileExists(FileNameStr) then
        FileHandle := FileCreateWithShareMode(FileNameStr, FileShareMode)
      else
        FileHandle := FileOpen(FileNameStr, FileShareMode);
    fcTruncateExisting :
      if not FileExists(FileNameStr) then
        raise EFileError.CreateFmt(feFileDoesNotExist, SFileDoesNotExist, [FileNameStr])
      else
        FileHandle := FileCreateWithShareMode(FileNameStr, FileShareMode)
  else
    raise EFileError.CreateFmt(feInvalidParameter, SInvalidFileCreationMode, []);
  end;
  if FileHandle < 0 then
    raise EFileError.CreateFmt(feFileOpenError, SCannotOpenFile, [GetLastOSErrorMessage, FileNameStr]);
  {$ENDIF}
  if foSeekToEndOfFile in FileOpenFlags then
    FileSeekEx(FileHandle, 0, fpOffsetFromEnd);
  Result := FileHandle;
end;

function FileOpenExU(
         const FileName: UnicodeString;
         const FileAccess: TFileAccess;
         const FileSharing: TFileSharing;
         const FileOpenFlags: TFileOpenFlags;
         const FileCreationMode: TFileCreationMode;
         const FileOpenWait: PFileOpenWait): TFileHandle;
var FileHandle     : Integer;
    FileShareMode  : LongWord;
    {$IFDEF MSWINDOWS}
    FileOpenAccess : LongWord;
    FileFlags      : LongWord;
    FileCreateDisp : LongWord;
    ErrorCode      : LongWord;
    ErrorSharing   : Boolean;
    Retry          : Boolean;
    WaitStart      : LongWord;
    WaitOpen       : Boolean;
    {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  FileFlags := FileOpenFlagsToWinFileFlags(FileOpenFlags);
  FileCreateDisp := FileCreationModeToWinFileCreateDisp(FileCreationMode);
  FileShareMode := FileSharingToWinFileShareMode(FileSharing);
  FileOpenAccess := FileAccessToWinFileOpenAccess(FileAccess);
  WaitOpen := False;
  WaitStart := 0;
  if Assigned(FileOpenWait) then
    if FileOpenWait^.Wait and (FileOpenWait^.Timeout > 0) then
      begin
        WaitOpen := True;
        WaitStart := GetTick;
        if Assigned(FileOpenWait^.Callback) then
          FileOpenWait^.Aborted := False;
      end;
  Retry := False;
  repeat
    FileHandle := Integer(Windows.CreateFileW(
        PWideChar(FileName),
        FileOpenAccess,
        FileShareMode,
        nil,
        FileCreateDisp,
        FileFlags,
        0));
    if FileHandle < 0 then
      begin
        ErrorCode := GetLastError;
        ErrorSharing :=
            (ErrorCode = ERROR_SHARING_VIOLATION) or
            (ErrorCode = ERROR_LOCK_VIOLATION);
      end
    else
      begin
        ErrorCode := 0;
        ErrorSharing := False;
      end;
    if WaitOpen and ErrorSharing then
      DoFileOpenWait(FileOpenWait, WaitStart, Retry);
  until not Retry;
  if FileHandle < 0 then
    raise EFileError.CreateFmt(WinErrorCodeToFileError(ErrorCode), SCannotOpenFile,
        [GetLastOSErrorMessage, FileName]);
  {$ELSE}
  FileShareMode := FileOpenShareMode(FileAccess, FileSharing);
  case FileCreationMode of
    fcCreateNew :
      if FileExists(FileName) then
        raise EFileError.CreateFmt(feFileExists, SFileExists, [FileName])
      else
        FileHandle := FileCreateWithShareMode(FileName, FileShareMode);
    fcCreateAlways :
      FileHandle := FileCreateWithShareMode(FileName, FileShareMode);
    fcOpenExisting :
      FileHandle := FileOpen(FileName, FileShareMode);
    fcOpenAlways :
      if not FileExists(FileName) then
        FileHandle := FileCreateWithShareMode(FileName, FileShareMode)
      else
        FileHandle := FileOpen(FileName, FileShareMode);
    fcTruncateExisting :
      if not FileExists(FileName) then
        raise EFileError.CreateFmt(feFileDoesNotExist, SFileDoesNotExist, [FileName])
      else
        FileHandle := FileCreateWithShareMode(FileName, FileShareMode)
  else
    raise EFileError.CreateFmt(feInvalidParameter, SInvalidFileCreationMode, []);
  end;
  if FileHandle < 0 then
    raise EFileError.CreateFmt(feFileOpenError, SCannotOpenFile, [GetLastOSErrorMessage, FileName]);
  {$ENDIF}
  if foSeekToEndOfFile in FileOpenFlags then
    FileSeekEx(FileHandle, 0, fpOffsetFromEnd);
  Result := FileHandle;
end;

function FileOpenEx(
         const FileName: String;
         const FileAccess: TFileAccess;
         const FileSharing: TFileSharing;
         const FileOpenFlags: TFileOpenFlags;
         const FileCreationMode: TFileCreationMode;
         const FileOpenWait: PFileOpenWait): TFileHandle;
var FileHandle     : Integer;
    FileShareMode  : LongWord;
    {$IFDEF MSWINDOWS}
    FileOpenAccess : LongWord;
    FileFlags      : LongWord;
    FileCreateDisp : LongWord;
    ErrorCode      : LongWord;
    ErrorSharing   : Boolean;
    Retry          : Boolean;
    WaitStart      : LongWord;
    WaitOpen       : Boolean;
    {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  FileFlags := FileOpenFlagsToWinFileFlags(FileOpenFlags);
  FileCreateDisp := FileCreationModeToWinFileCreateDisp(FileCreationMode);
  FileShareMode := FileSharingToWinFileShareMode(FileSharing);
  FileOpenAccess := FileAccessToWinFileOpenAccess(FileAccess);
  WaitOpen := False;
  WaitStart := 0;
  if Assigned(FileOpenWait) then
    if FileOpenWait^.Wait and (FileOpenWait^.Timeout > 0) then
      begin
        WaitOpen := True;
        WaitStart := GetTick;
        if Assigned(FileOpenWait^.Callback) then
          FileOpenWait^.Aborted := False;
      end;
  Retry := False;
  repeat
    {$IFDEF CharIsWide}
    FileHandle := Integer(Windows.CreateFileW(
        PWideChar(FileName),
        FileOpenAccess,
        FileShareMode,
        nil,
        FileCreateDisp,
        FileFlags,
        0));
    {$ELSE}
    FileHandle := Integer(Windows.CreateFileA(
        PAnsiChar(FileName),
        FileOpenAccess,
        FileShareMode,
        nil,
        FileCreateDisp,
        FileFlags,
        0));
    {$ENDIF}
    if FileHandle < 0 then
      begin
        ErrorCode := GetLastError;
        ErrorSharing :=
            (ErrorCode = ERROR_SHARING_VIOLATION) or
            (ErrorCode = ERROR_LOCK_VIOLATION);
      end
    else
      begin
        ErrorCode := 0;
        ErrorSharing := False;
      end;
    if WaitOpen and ErrorSharing then
      DoFileOpenWait(FileOpenWait, WaitStart, Retry);
  until not Retry;
  if FileHandle < 0 then
    raise EFileError.CreateFmt(WinErrorCodeToFileError(ErrorCode), SCannotOpenFile,
        [GetLastOSErrorMessage, FileName]);
  {$ELSE}
  FileShareMode := FileOpenShareMode(FileAccess, FileSharing);
  case FileCreationMode of
    fcCreateNew :
      if FileExists(FileName) then
        raise EFileError.CreateFmt(feFileExists, SFileExists, [FileName])
      else
        FileHandle := FileCreateWithShareMode(FileName, FileShareMode);
    fcCreateAlways :
      FileHandle := FileCreateWithShareMode(FileName, FileShareMode);
    fcOpenExisting :
      FileHandle := FileOpen(FileName, FileShareMode);
    fcOpenAlways :
      if not FileExists(FileName) then
        FileHandle := FileCreateWithShareMode(FileName, FileShareMode)
      else
        FileHandle := FileOpen(FileName, FileShareMode);
    fcTruncateExisting :
      if not FileExists(FileName) then
        raise EFileError.CreateFmt(feFileDoesNotExist, SFileDoesNotExist, [FileName])
      else
        FileHandle := FileCreateWithShareMode(FileName, FileShareMode)
  else
    raise EFileError.CreateFmt(feInvalidParameter, SInvalidFileCreationMode, []);
  end;
  if FileHandle < 0 then
    raise EFileError.CreateFmt(feFileOpenError, SCannotOpenFile, [GetLastOSErrorMessage, FileName]);
  {$ENDIF}
  if foSeekToEndOfFile in FileOpenFlags then
    FileSeekEx(FileHandle, 0, fpOffsetFromEnd);
  Result := FileHandle;
end;

function FileSeekEx(
         const FileHandle: TFileHandle;
         const FileOffset: Int64;
         const FilePosition: TFileSeekPosition): Int64;
begin
  if FileHandle = 0 then
    raise EFileError.CreateFmt(feInvalidParameter, SInvalidFileHandle, []);
  case FilePosition of
    fpOffsetFromStart   : Result := FileSeek(FileHandle, FileOffset, 0);
    fpOffsetFromCurrent : Result := FileSeek(FileHandle, FileOffset, 1);
    fpOffsetFromEnd     : Result := FileSeek(FileHandle, FileOffset, 2);
  else
    raise EFileError.CreateFmt(feInvalidParameter, SInvalidFilePosition, []);
  end;
  if Result < 0 then
    raise EFileError.CreateFmt(feFileSeekError, SFileSeekError, [GetLastOSErrorMessage]);
end;

function FileReadEx(
         const FileHandle: TFileHandle;
         var Buf; const BufSize: Integer): Integer;
begin
  {$IFDEF MSWINDOWS}
  if not ReadFile(FileHandle, Buf, BufSize, LongWord(Result), nil) then
    raise EFileError.CreateFmt(feFileReadError, SFileReadError, [GetLastOSErrorMessage]);
  {$ELSE}
  Result := FileRead(FileHandle, Buf, BufSize);
  if Result < 0 then
    raise EFileError.Create(feFileReadError, SFileReadError);
  {$ENDIF}
end;

function FileWriteEx(
         const FileHandle: TFileHandle;
         const Buf; const BufSize: Integer): Integer;
begin
  {$IFDEF MSWINDOWS}
  if not WriteFile(FileHandle, Buf, BufSize, LongWord(Result), nil) then
    raise EFileError.CreateFmt(feFileWriteError, SFileWriteError, [GetLastOSErrorMessage, IntToStr(Ord(FileHandle))]);
  {$ELSE}
  Result := FileWrite(FileHandle, Buf, BufSize);
  if Result < 0 then
    raise EFileError.CreateFmt(feFileWriteError, SFileWriteError, [GetLastOSErrorMessage, IntToStr(Ord(FileHandle))]);
  {$ENDIF}
end;

procedure FileCloseEx(const FileHandle: TFileHandle);
begin
  FileClose(FileHandle);
end;

function FindFirstB(const Path: RawByteString; Attr: Integer; var F: TSearchRec): Integer;
begin
  Result := FindFirst(ToStringB(Path), Attr, F);
end;

function FindFirstU(const Path: UnicodeString; Attr: Integer; var F: TSearchRec): Integer;
begin
  Result := FindFirst(ToStringU(Path), Attr, F);
end;

function FileExistsB(const FileName: RawByteString): Boolean;
{$IFDEF MSWINDOWS}
var Attr : LongWord;
{$ELSE}
var SRec : TSearchRec;
{$ENDIF}
begin
  if FileName = '' then
    raise EFileError.Create(feInvalidParameter, SInvalidFileName);
  {$IFDEF MSWINDOWS}
  Attr := GetFileAttributesA(PAnsiChar(FileName));
  if Attr = $FFFFFFFF then
    Result := False
  else
    Result := Attr and FILE_ATTRIBUTE_DIRECTORY = 0;
  {$ELSE}
  if FindFirstB(FileName, faAnyFile, SRec) <> 0 then
    Result := False
  else
    begin
      Result := SRec.Attr and faDirectory = 0;
      FindClose(SRec);
    end;
  {$ENDIF}
end;

function FileExistsU(const FileName: UnicodeString): Boolean;
{$IFDEF MSWINDOWS}
var Attr : LongWord;
{$ELSE}
var SRec : TSearchRec;
{$ENDIF}
begin
  if FileName = '' then
    raise EFileError.Create(feInvalidParameter, SInvalidFileName);
  {$IFDEF MSWINDOWS}
  Attr := GetFileAttributesW(PWideChar(FileName));
  if Attr = $FFFFFFFF then
    Result := False
  else
    Result := Attr and FILE_ATTRIBUTE_DIRECTORY = 0;
  {$ELSE}
  if FindFirstU(FileName, faAnyFile, SRec) <> 0 then
    Result := False
  else
    begin
      Result := SRec.Attr and faDirectory = 0;
      FindClose(SRec);
    end;
  {$ENDIF}
end;

function FileExists(const FileName: String): Boolean;
{$IFDEF MSWINDOWS}
var Attr : LongWord;
{$ELSE}
var SRec : TSearchRec;
{$ENDIF}
begin
  if FileName = '' then
    raise EFileError.Create(feInvalidParameter, SInvalidFileName);
  {$IFDEF MSWINDOWS}
  {$IFDEF StringIsUnicode}
  Attr := GetFileAttributesW(PWideChar(FileName));
  {$ELSE}
  Attr := GetFileAttributesA(PAnsiChar(FileName));
  {$ENDIF}
  if Attr = $FFFFFFFF then
    Result := False
  else
    Result := Attr and FILE_ATTRIBUTE_DIRECTORY = 0;
  {$ELSE}
  if FindFirst(FileName, faAnyFile, SRec) <> 0 then
    Result := False
  else
    begin
      Result := SRec.Attr and faDirectory = 0;
      FindClose(SRec);
    end;
  {$ENDIF}
end;

function FileGetSizeB(const FileName: RawByteString): Int64;
var SRec : TSearchRec;
begin
  if FileName = '' then
    raise EFileError.Create(feInvalidParameter, SInvalidFileName);
  if FindFirstB(FileName, faAnyFile, SRec) <> 0 then
    Result := -1
  else
  begin
    if SRec.Attr and faDirectory <> 0 then
      Result := -1
    else
      begin
        {$IFDEF MSWINDOWS}
        Int64Rec(Result).Lo := SRec.FindData.nFileSizeLow;
        Int64Rec(Result).Hi := SRec.FindData.nFileSizeHigh;
        {$ELSE}
        Result := SRec.Size;
        {$ENDIF}
      end;
    FindClose(SRec);
  end;
end;

function FileGetSizeU(const FileName: UnicodeString): Int64;
var SRec : TSearchRec;
begin
  if FileName = '' then
    raise EFileError.Create(feInvalidParameter, SInvalidFileName);
  if FindFirstU(FileName, faAnyFile, SRec) <> 0 then
    Result := -1
  else
  begin
    if SRec.Attr and faDirectory <> 0 then
      Result := -1
    else
      begin
        {$IFDEF MSWINDOWS}
        Int64Rec(Result).Lo := SRec.FindData.nFileSizeLow;
        Int64Rec(Result).Hi := SRec.FindData.nFileSizeHigh;
        {$ELSE}
        Result := SRec.Size;
        {$ENDIF}
      end;
    FindClose(SRec);
  end;
end;

function FileGetSize(const FileName: String): Int64;
var SRec : TSearchRec;
begin
  if FileName = '' then
    raise EFileError.Create(feInvalidParameter, SInvalidFileName);
  if FindFirst(FileName, faAnyFile, SRec) <> 0 then
    Result := -1
  else
  begin
    if SRec.Attr and faDirectory <> 0 then
      Result := -1
    else
      begin
        {$IFDEF MSWINDOWS}
        Int64Rec(Result).Lo := SRec.FindData.nFileSizeLow;
        Int64Rec(Result).Hi := SRec.FindData.nFileSizeHigh;
        {$ELSE}
        Result := SRec.Size;
        {$ENDIF}
      end;
    FindClose(SRec);
  end;
end;

function FileGetDateTimeB(const FileName: RawByteString): TDateTime;
var SRec : TSearchRec;
begin
  if FileName = '' then
    raise EFileError.Create(feInvalidParameter, SInvalidFileName);
  if FindFirstB(FileName, faAnyFile, SRec) <> 0 then
    Result := 0.0
  else
    begin
      if SRec.Attr and faDirectory <> 0 then
        Result := 0.0
      else
        Result := FileDateToDateTime(SRec.Time);
      FindClose(SRec);
    end;
end;

function FileGetDateTime(const FileName: String): TDateTime;
var SRec : TSearchRec;
begin
  if FileName = '' then
    raise EFileError.Create(feInvalidParameter, SInvalidFileName);
  if FindFirst(FileName, faAnyFile, SRec) <> 0 then
    Result := 0.0
  else
    begin
      if SRec.Attr and faDirectory <> 0 then
        Result := 0.0
      else
        Result := FileDateToDateTime(SRec.Time);
      FindClose(SRec);
    end;
end;

function FileGetDateTime2(const FileName: String): TDateTime;
var Age : LongInt;
begin
  Age := FileAge(FileName);
  if Age = -1 then
    Result := 0.0
  else
    Result := FileDateToDateTime(Age);
end;

function FileIsReadOnly(const FileName: String): Boolean;
var SRec : TSearchRec;
begin
  if FileName = '' then
    raise EFileError.Create(feInvalidParameter, SInvalidFileName);
  if FindFirst(FileName, faAnyFile, SRec) <> 0 then
    Result := False
  else
    begin
      Result := SRec.Attr and (faReadOnly or faDirectory) = faReadOnly;
      FindClose(SRec);
    end;
end;

procedure FileDeleteEx(const FileName: String);
begin
  if FileName = '' then
    raise EFileError.Create(feInvalidParameter, SInvalidFileName);
  if not DeleteFile(FileName) then
    raise EFileError.CreateFmt(feFileDeleteError, SFileDeleteError, [GetLastOSErrorMessage]);
end;

procedure FileRenameEx(const OldFileName, NewFileName: String);
begin
  RenameFile(OldFileName, NewFileName);
end;

{ ReadFileBuf }

function ReadFileBufB(
         const FileName: RawByteString;
         var Buf; const BufSize: Integer;
         const FileSharing: TFileSharing;
         const FileCreationMode: TFileCreationMode;
         const FileOpenWait: PFileOpenWait): Integer;
var FileHandle : Integer;
    FileSize   : Int64;
begin
  Result := 0;
  FileHandle := FileOpenExB(FileName, faRead, FileSharing,
      [foSequentialScanHint], FileCreationMode, FileOpenWait);
  try
    FileSize := FileGetSize(ToStringB(FileName));
    if FileSize = 0 then
      exit;
    if FileSize < 0 then
      raise EFileError.CreateFmt(feFileSizeError, SFileSizeError, [FileName]);
    if FileSize > MaxInteger then
      raise EFileError.CreateFmt(feFileSizeError, SFileSizeError, [FileName]);
    if FileSize > BufSize then
      raise EFileError.CreateFmt(feFileSizeError, SFileSizeError, [FileName]);
    Result := FileReadEx(FileHandle, Buf, FileSize);
  finally
    FileClose(FileHandle);
  end;
end;

function ReadFileBufU(
         const FileName: UnicodeString;
         var Buf; const BufSize: Integer;
         const FileSharing: TFileSharing = fsDenyNone;
         const FileCreationMode: TFileCreationMode = fcOpenExisting;
         const FileOpenWait: PFileOpenWait = nil): Integer;
var FileHandle : Integer;
    FileSize   : Int64;
begin
  Result := 0;
  FileHandle := FileOpenExU(FileName, faRead, FileSharing,
      [foSequentialScanHint], FileCreationMode, FileOpenWait);
  try
    FileSize := FileGetSizeU(FileName);
    if FileSize = 0 then
      exit;
    if FileSize < 0 then
      raise EFileError.CreateFmt(feFileSizeError, SFileSizeError, [FileName]);
    if FileSize > MaxInteger then
      raise EFileError.CreateFmt(feFileSizeError, SFileSizeError, [FileName]);
    if FileSize > BufSize then
      raise EFileError.CreateFmt(feFileSizeError, SFileSizeError, [FileName]);
    Result := FileReadEx(FileHandle, Buf, FileSize);
  finally
    FileClose(FileHandle);
  end;
end;

function ReadFileBuf(
         const FileName: String;
         var Buf; const BufSize: Integer;
         const FileSharing: TFileSharing = fsDenyNone;
         const FileCreationMode: TFileCreationMode = fcOpenExisting;
         const FileOpenWait: PFileOpenWait = nil): Integer;
var FileHandle : Integer;
    FileSize   : Int64;
begin
  Result := 0;
  FileHandle := FileOpenEx(FileName, faRead, FileSharing,
      [foSequentialScanHint], FileCreationMode, FileOpenWait);
  try
    FileSize := FileGetSize(FileName);
    if FileSize = 0 then
      exit;
    if FileSize < 0 then
      raise EFileError.CreateFmt(feFileSizeError, SFileSizeError, [FileName]);
    if FileSize > MaxInteger then
      raise EFileError.CreateFmt(feFileSizeError, SFileSizeError, [FileName]);
    if FileSize > BufSize then
      raise EFileError.CreateFmt(feFileSizeError, SFileSizeError, [FileName]);
    Result := FileReadEx(FileHandle, Buf, FileSize);
  finally
    FileClose(FileHandle);
  end;
end;

{ ReadFileStr }

function ReadFileRawStrB(
         const FileName: RawByteString;
         const FileSharing: TFileSharing;
         const FileCreationMode: TFileCreationMode;
         const FileOpenWait: PFileOpenWait): RawByteString;
var FileHandle : Integer;
    FileSize   : Int64;
    ReadBytes  : Integer;
begin
  FileHandle := FileOpenExB(FileName, faRead, FileSharing,
      [foSequentialScanHint], FileCreationMode, FileOpenWait);
  try
    FileSize := FileGetSize(ToStringB(FileName));
    if FileSize < 0 then
      raise EFileError.CreateFmt(feFileSizeError, SFileSizeError, [FileName]);
    if FileSize > MaxInteger then
      raise EFileError.CreateFmt(feFileSizeError, SFileSizeError, [FileName]);
    SetLength(Result, FileSize);
    if FileSize = 0 then
      exit;
    ReadBytes := FileReadEx(FileHandle, Result[1], FileSize);
    if ReadBytes < FileSize then
      SetLength(Result, ReadBytes);
  finally
    FileClose(FileHandle);
  end;
end;

function ReadFileRawStrU(
         const FileName: UnicodeString;
         const FileSharing: TFileSharing;
         const FileCreationMode: TFileCreationMode;
         const FileOpenWait: PFileOpenWait): RawByteString;
var FileHandle : Integer;
    FileSize   : Int64;
    ReadBytes  : Integer;
begin
  FileHandle := FileOpenExU(FileName, faRead, FileSharing,
      [foSequentialScanHint], FileCreationMode, FileOpenWait);
  try
    FileSize := FileGetSizeU(FileName);
    if FileSize < 0 then
      raise EFileError.CreateFmt(feFileSizeError, SFileSizeError, [FileName]);
    if FileSize > MaxInteger then
      raise EFileError.CreateFmt(feFileSizeError, SFileSizeError, [FileName]);
    SetLength(Result, FileSize);
    if FileSize = 0 then
      exit;
    ReadBytes := FileReadEx(FileHandle, Result[1], FileSize);
    if ReadBytes < FileSize then
      SetLength(Result, ReadBytes);
  finally
    FileClose(FileHandle);
  end;
end;

function ReadFileRawStr(
         const FileName: String;
         const FileSharing: TFileSharing;
         const FileCreationMode: TFileCreationMode;
         const FileOpenWait: PFileOpenWait): RawByteString;
var FileHandle : Integer;
    FileSize   : Int64;
    ReadBytes  : Integer;
begin
  FileHandle := FileOpenEx(FileName, faRead, FileSharing,
      [foSequentialScanHint], FileCreationMode, FileOpenWait);
  try
    FileSize := FileGetSize(FileName);
    if FileSize < 0 then
      raise EFileError.CreateFmt(feFileSizeError, SFileSizeError, [FileName]);
    if FileSize > MaxInteger then
      raise EFileError.CreateFmt(feFileSizeError, SFileSizeError, [FileName]);
    SetLength(Result, FileSize);
    if FileSize = 0 then
      exit;
    ReadBytes := FileReadEx(FileHandle, Result[1], FileSize);
    if ReadBytes < FileSize then
      SetLength(Result, ReadBytes);
  finally
    FileClose(FileHandle);
  end;
end;

{ ReadFileBytes }

function ReadFileBytesB(
         const FileName: RawByteString;
         const FileSharing: TFileSharing;
         const FileCreationMode: TFileCreationMode;
         const FileOpenWait: PFileOpenWait): TBytes;
var FileHandle : Integer;
    FileSize   : Int64;
    ReadBytes  : Integer;
begin
  FileHandle := FileOpenExB(FileName, faRead, FileSharing,
      [foSequentialScanHint], FileCreationMode, FileOpenWait);
  try
    FileSize := FileGetSize(ToStringB(FileName));
    if FileSize < 0 then
      raise EFileError.CreateFmt(feFileSizeError, SFileSizeError, [FileName]);
    if FileSize > MaxInteger then
      raise EFileError.CreateFmt(feFileSizeError, SFileSizeError, [FileName]);
    SetLength(Result, FileSize);
    if FileSize = 0 then
      exit;
    ReadBytes := FileReadEx(FileHandle, Result[0], FileSize);
    if ReadBytes < FileSize then
      SetLength(Result, ReadBytes);
  finally
    FileClose(FileHandle);
  end;
end;

function ReadFileBytesU(
         const FileName: UnicodeString;
         const FileSharing: TFileSharing;
         const FileCreationMode: TFileCreationMode;
         const FileOpenWait: PFileOpenWait): TBytes;
var FileHandle : Integer;
    FileSize   : Int64;
    ReadBytes  : Integer;
begin
  FileHandle := FileOpenExU(FileName, faRead, FileSharing,
      [foSequentialScanHint], FileCreationMode, FileOpenWait);
  try
    FileSize := FileGetSizeU(FileName);
    if FileSize < 0 then
      raise EFileError.CreateFmt(feFileSizeError, SFileSizeError, [FileName]);
    if FileSize > MaxInteger then
      raise EFileError.CreateFmt(feFileSizeError, SFileSizeError, [FileName]);
    SetLength(Result, FileSize);
    if FileSize = 0 then
      exit;
    ReadBytes := FileReadEx(FileHandle, Result[0], FileSize);
    if ReadBytes < FileSize then
      SetLength(Result, ReadBytes);
  finally
    FileClose(FileHandle);
  end;
end;

function ReadFileBytes(
         const FileName: String;
         const FileSharing: TFileSharing;
         const FileCreationMode: TFileCreationMode;
         const FileOpenWait: PFileOpenWait): TBytes;
var FileHandle : Integer;
    FileSize   : Int64;
    ReadBytes  : Integer;
begin
  FileHandle := FileOpenEx(FileName, faRead, FileSharing,
      [foSequentialScanHint], FileCreationMode, FileOpenWait);
  try
    FileSize := FileGetSize(FileName);
    if FileSize < 0 then
      raise EFileError.CreateFmt(feFileSizeError, SFileSizeError, [FileName]);
    if FileSize > MaxInteger then
      raise EFileError.CreateFmt(feFileSizeError, SFileSizeError, [FileName]);
    SetLength(Result, FileSize);
    if FileSize = 0 then
      exit;
    ReadBytes := FileReadEx(FileHandle, Result[0], FileSize);
    if ReadBytes < FileSize then
      SetLength(Result, ReadBytes);
  finally
    FileClose(FileHandle);
  end;
end;

{ WtiteFileBuf }

procedure WriteFileBufB(
          const FileName: RawByteString;
          const Buf; const BufSize: Integer;
          const FileSharing: TFileSharing;
          const FileCreationMode: TFileCreationMode;
          const FileOpenWait: PFileOpenWait);
var FileHandle : Integer;
begin
  if BufSize <= 0 then
    exit;
  FileHandle := FileOpenExB(FileName, faWrite, FileSharing, [],
      FileCreationMode, FileOpenWait);
  try
    if FileWriteEx(FileHandle, Buf, BufSize) <> BufSize then
      raise EFileError.CreateFmt(feFileWriteError, SFileWriteError, [GetLastOSErrorMessage, FileName]);
  finally
    FileClose(FileHandle);
  end;
end;

procedure WriteFileBufU(
          const FileName: UnicodeString;
          const Buf; const BufSize: Integer;
          const FileSharing: TFileSharing;
          const FileCreationMode: TFileCreationMode;
          const FileOpenWait: PFileOpenWait);
var FileHandle : Integer;
begin
  if BufSize <= 0 then
    exit;
  FileHandle := FileOpenExU(FileName, faWrite, FileSharing, [],
      FileCreationMode, FileOpenWait);
  try
    if FileWriteEx(FileHandle, Buf, BufSize) <> BufSize then
      raise EFileError.CreateFmt(feFileWriteError, SFileWriteError, [GetLastOSErrorMessage, FileName]);
  finally
    FileClose(FileHandle);
  end;
end;

procedure WriteFileBuf(
          const FileName: String;
          const Buf; const BufSize: Integer;
          const FileSharing: TFileSharing;
          const FileCreationMode: TFileCreationMode;
          const FileOpenWait: PFileOpenWait);
var FileHandle : Integer;
begin
  if BufSize <= 0 then
    exit;
  FileHandle := FileOpenEx(FileName, faWrite, FileSharing, [],
      FileCreationMode, FileOpenWait);
  try
    if FileWriteEx(FileHandle, Buf, BufSize) <> BufSize then
      raise EFileError.CreateFmt(feFileWriteError, SFileWriteError, [GetLastOSErrorMessage, FileName]);
  finally
    FileClose(FileHandle);
  end;
end;

{ WriteFileStr }

procedure WriteFileRawStrB(
          const FileName: RawByteString;
          const Buf: RawByteString;
          const FileSharing: TFileSharing;
          const FileCreationMode: TFileCreationMode;
          const FileOpenWait: PFileOpenWait);
var BufSize    : Integer;
begin
  BufSize := Length(Buf);
  if BufSize <= 0 then
    exit;
  WriteFileBufB(FileName, Buf[1], BufSize, FileSharing, FileCreationMode, FileOpenWait);
end;

procedure WriteFileRawStrU(
          const FileName: UnicodeString;
          const Buf: RawByteString;
          const FileSharing: TFileSharing;
          const FileCreationMode: TFileCreationMode;
          const FileOpenWait: PFileOpenWait);
var BufSize    : Integer;
begin
  BufSize := Length(Buf);
  if BufSize <= 0 then
    exit;
  WriteFileBufU(FileName, Buf[1], BufSize, FileSharing, FileCreationMode, FileOpenWait);
end;

procedure WriteFileRawStr(
          const FileName: String;
          const Buf: RawByteString;
          const FileSharing: TFileSharing;
          const FileCreationMode: TFileCreationMode;
          const FileOpenWait: PFileOpenWait);
var BufSize    : Integer;
begin
  BufSize := Length(Buf);
  if BufSize <= 0 then
    exit;
  WriteFileBuf(FileName, Buf[1], BufSize, FileSharing, FileCreationMode, FileOpenWait);
end;

{ WriteFileBytes }

procedure WriteFileBytesB(
          const FileName: RawByteString;
          const Buf: TBytes;
          const FileSharing: TFileSharing;
          const FileCreationMode: TFileCreationMode;
          const FileOpenWait: PFileOpenWait);
var BufSize    : Integer;
begin
  BufSize := Length(Buf);
  if BufSize <= 0 then
    exit;
  WriteFileBufB(FileName, Buf[0], BufSize, FileSharing, FileCreationMode, FileOpenWait);
end;

procedure WriteFileBytesU(
          const FileName: UnicodeString;
          const Buf: TBytes;
          const FileSharing: TFileSharing;
          const FileCreationMode: TFileCreationMode;
          const FileOpenWait: PFileOpenWait);
var BufSize    : Integer;
begin
  BufSize := Length(Buf);
  if BufSize <= 0 then
    exit;
  WriteFileBufU(FileName, Buf[0], BufSize, FileSharing, FileCreationMode, FileOpenWait);
end;

procedure WriteFileBytes(
          const FileName: String;
          const Buf: TBytes;
          const FileSharing: TFileSharing;
          const FileCreationMode: TFileCreationMode;
          const FileOpenWait: PFileOpenWait);
var BufSize    : Integer;
begin
  BufSize := Length(Buf);
  if BufSize <= 0 then
    exit;
  WriteFileBuf(FileName, Buf[0], BufSize, FileSharing, FileCreationMode, FileOpenWait);
end;

{ AppendFile }

procedure AppendFileB(
          const FileName: RawByteString;
          const Buf; const BufSize: Integer;
          const FileSharing: TFileSharing;
          const FileCreationMode: TFileCreationMode;
          const FileOpenWait: PFileOpenWait);
var FileHandle : Integer;
begin
  if BufSize <= 0 then
    exit;
  FileHandle := FileOpenExB(FileName, faWrite, FileSharing, [foSeekToEndOfFile],
      FileCreationMode, FileOpenWait);
  try
    if FileWriteEx(FileHandle, Buf, BufSize) <> BufSize then
      raise EFileError.CreateFmt(feFileWriteError, SFileWriteError, [GetLastOSErrorMessage, FileName]);
  finally
    FileClose(FileHandle);
  end;
end;

procedure AppendFileU(
          const FileName: UnicodeString;
          const Buf; const BufSize: Integer;
          const FileSharing: TFileSharing;
          const FileCreationMode: TFileCreationMode;
          const FileOpenWait: PFileOpenWait);
var FileHandle : Integer;
begin
  if BufSize <= 0 then
    exit;
  FileHandle := FileOpenExU(FileName, faWrite, FileSharing, [foSeekToEndOfFile],
      FileCreationMode, FileOpenWait);
  try
    if FileWriteEx(FileHandle, Buf, BufSize) <> BufSize then
      raise EFileError.CreateFmt(feFileWriteError, SFileWriteError, [GetLastOSErrorMessage, FileName]);
  finally
    FileClose(FileHandle);
  end;
end;

procedure AppendFile(
          const FileName: String;
          const Buf; const BufSize: Integer;
          const FileSharing: TFileSharing;
          const FileCreationMode: TFileCreationMode;
          const FileOpenWait: PFileOpenWait);
var FileHandle : Integer;
begin
  if BufSize <= 0 then
    exit;
  FileHandle := FileOpenEx(FileName, faWrite, FileSharing, [foSeekToEndOfFile],
      FileCreationMode, FileOpenWait);
  try
    if FileWriteEx(FileHandle, Buf, BufSize) <> BufSize then
      raise EFileError.CreateFmt(feFileWriteError, SFileWriteError, [GetLastOSErrorMessage, FileName]);
  finally
    FileClose(FileHandle);
  end;
end;

{ AppendFileStr }

procedure AppendFileRawStrB(
          const FileName: RawByteString;
          const Buf: RawByteString;
          const FileSharing: TFileSharing;
          const FileCreationMode: TFileCreationMode;
          const FileOpenWait: PFileOpenWait);
var BufSize    : Integer;
begin
  BufSize := Length(Buf);
  if BufSize <= 0 then
    exit;
  AppendFileB(FileName, Buf[1], BufSize, FileSharing, FileCreationMode, FileOpenWait);
end;

procedure AppendFileRawStrU(
          const FileName: UnicodeString;
          const Buf: RawByteString;
          const FileSharing: TFileSharing;
          const FileCreationMode: TFileCreationMode;
          const FileOpenWait: PFileOpenWait);
var BufSize    : Integer;
begin
  BufSize := Length(Buf);
  if BufSize <= 0 then
    exit;
  AppendFileU(FileName, Buf[1], BufSize, FileSharing, FileCreationMode, FileOpenWait);
end;

procedure AppendFileRawStr(
          const FileName: String;
          const Buf: RawByteString;
          const FileSharing: TFileSharing;
          const FileCreationMode: TFileCreationMode;
          const FileOpenWait: PFileOpenWait);
var BufSize    : Integer;
begin
  BufSize := Length(Buf);
  if BufSize <= 0 then
    exit;
  AppendFile(FileName, Buf[1], BufSize, FileSharing, FileCreationMode, FileOpenWait);
end;

function DirectoryEntryExistsB(const Name: RawByteString): Boolean;
var SRec : TSearchRec;
begin
  if FindFirstB(Name, faAnyFile, SRec) <> 0 then
    Result := False
  else
    begin
      Result := True;
      FindClose(SRec);
    end;
end;

function DirectoryEntryExistsU(const Name: UnicodeString): Boolean;
var SRec : TSearchRec;
begin
  if FindFirstU(Name, faAnyFile, SRec) <> 0 then
    Result := False
  else
    begin
      Result := True;
      FindClose(SRec);
    end;
end;

function DirectoryEntryExists(const Name: String): Boolean;
var SRec : TSearchRec;
begin
  if FindFirst(Name, faAnyFile, SRec) <> 0 then
    Result := False
  else
    begin
      Result := True;
      FindClose(SRec);
    end;
end;

function DirectoryEntrySizeB(const Name: RawByteString): Int64;
var SRec : TSearchRec;
begin
  if FindFirstB(Name, faAnyFile, SRec) <> 0 then
    Result := -1
  else
    begin
      if SRec.Attr and faDirectory <> 0 then
        Result := 0
      else
        begin
          {$IFDEF MSWINDOWS}
          {$WARNINGS OFF}
          Int64Rec(Result).Lo := SRec.FindData.nFileSizeLow;
          Int64Rec(Result).Hi := SRec.FindData.nFileSizeHigh;
          {$IFDEF DEBUG}{$IFNDEF FREEPASCAL}{$WARNINGS ON}{$ENDIF}{$ENDIF}
          {$ELSE}
          Result := SRec.Size;
          {$ENDIF}
        end;
      FindClose(SRec);
    end;
end;

function DirectoryEntrySizeU(const Name: UnicodeString): Int64;
var SRec : TSearchRec;
begin
  if FindFirstU(Name, faAnyFile, SRec) <> 0 then
    Result := -1
  else
    begin
      if SRec.Attr and faDirectory <> 0 then
        Result := 0
      else
        begin
          {$IFDEF MSWINDOWS}
          {$WARNINGS OFF}
          Int64Rec(Result).Lo := SRec.FindData.nFileSizeLow;
          Int64Rec(Result).Hi := SRec.FindData.nFileSizeHigh;
          {$IFDEF DEBUG}{$IFNDEF FREEPASCAL}{$WARNINGS ON}{$ENDIF}{$ENDIF}
          {$ELSE}
          Result := SRec.Size;
          {$ENDIF}
        end;
      FindClose(SRec);
    end;
end;

function DirectoryEntrySize(const Name: String): Int64;
var SRec : TSearchRec;
begin
  if FindFirst(Name, faAnyFile, SRec) <> 0 then
    Result := -1
  else
    begin
      if SRec.Attr and faDirectory <> 0 then
        Result := 0
      else
        begin
          {$IFDEF MSWINDOWS}
          {$WARNINGS OFF}
          Int64Rec(Result).Lo := SRec.FindData.nFileSizeLow;
          Int64Rec(Result).Hi := SRec.FindData.nFileSizeHigh;
          {$IFDEF DEBUG}{$IFNDEF FREEPASCAL}{$WARNINGS ON}{$ENDIF}{$ENDIF}
          {$ELSE}
          Result := SRec.Size;
          {$ENDIF}
        end;
      FindClose(SRec);
    end;
end;

function DirectoryExistsB(const DirectoryName: RawByteString): Boolean;
{$IFDEF MSWINDOWS}
var Attr : LongWord;
{$ELSE}
var SRec : TSearchRec;
{$ENDIF}
begin
  if DirectoryName = '' then
    raise EFileError.Create(feInvalidParameter, SInvalidPath);
  {$IFDEF MSWINDOWS}
  Attr := GetFileAttributesA(PAnsiChar(DirectoryName));
  if Attr = $FFFFFFFF then
    Result := False
  else
    Result := Attr and FILE_ATTRIBUTE_DIRECTORY <> 0;
  {$ELSE}
  if FindFirstB(DirectoryName, faAnyFile, SRec) <> 0 then
    Result := False
  else
    begin
      Result := SRec.Attr and faDirectory <> 0;
      FindClose(SRec);
    end;
  {$ENDIF}
end;

function DirectoryExistsU(const DirectoryName: UnicodeString): Boolean;
{$IFDEF MSWINDOWS}
var Attr : LongWord;
{$ELSE}
var SRec : TSearchRec;
{$ENDIF}
begin
  if DirectoryName = '' then
    raise EFileError.Create(feInvalidParameter, SInvalidPath);
  {$IFDEF MSWINDOWS}
  Attr := GetFileAttributesW(PWideChar(DirectoryName));
  if Attr = $FFFFFFFF then
    Result := False
  else
    Result := Attr and FILE_ATTRIBUTE_DIRECTORY <> 0;
  {$ELSE}
  if FindFirstU(DirectoryName, faAnyFile, SRec) <> 0 then
    Result := False
  else
    begin
      Result := SRec.Attr and faDirectory <> 0;
      FindClose(SRec);
    end;
  {$ENDIF}
end;

function DirectoryExists(const DirectoryName: String): Boolean;
{$IFDEF MSWINDOWS}
var Attr : LongWord;
{$ELSE}
var SRec : TSearchRec;
{$ENDIF}
begin
  if DirectoryName = '' then
    raise EFileError.Create(feInvalidParameter, SInvalidPath);
  {$IFDEF MSWINDOWS}
  {$IFDEF StringIsUnicode}
  Attr := GetFileAttributesW(PWideChar(DirectoryName));
  {$ELSE}
  Attr := GetFileAttributesA(PAnsiChar(DirectoryName));
  {$ENDIF}
  if Attr = $FFFFFFFF then
    Result := False
  else
    Result := Attr and FILE_ATTRIBUTE_DIRECTORY <> 0;
  {$ELSE}
  if FindFirst(DirectoryName, faAnyFile, SRec) <> 0 then
    Result := False
  else
    begin
      Result := SRec.Attr and faDirectory <> 0;
      FindClose(SRec);
    end;
  {$ENDIF}
end;

function DirectoryGetDateTimeB(const DirectoryName: RawByteString): TDateTime;
var SRec : TSearchRec;
begin
  if DirectoryName = '' then
    raise EFileError.Create(feInvalidParameter, SInvalidPath);
  if FindFirstB(DirectoryName, faAnyFile, SRec) <> 0 then
    Result := 0.0
  else
    begin
      if SRec.Attr and faDirectory = 0 then
        Result := 0.0
      else
        Result := FileDateToDateTime(SRec.Time);
      FindClose(SRec);
    end;
end;

function DirectoryGetDateTimeU(const DirectoryName: UnicodeString): TDateTime;
var SRec : TSearchRec;
begin
  if DirectoryName = '' then
    raise EFileError.Create(feInvalidParameter, SInvalidPath);
  if FindFirstU(DirectoryName, faAnyFile, SRec) <> 0 then
    Result := 0.0
  else
    begin
      if SRec.Attr and faDirectory = 0 then
        Result := 0.0
      else
        Result := FileDateToDateTime(SRec.Time);
      FindClose(SRec);
    end;
end;

function DirectoryGetDateTime(const DirectoryName: String): TDateTime;
var SRec : TSearchRec;
begin
  if DirectoryName = '' then
    raise EFileError.Create(feInvalidParameter, SInvalidPath);
  if FindFirst(DirectoryName, faAnyFile, SRec) <> 0 then
    Result := 0.0
  else
    begin
      if SRec.Attr and faDirectory = 0 then
        Result := 0.0
      else
        Result := FileDateToDateTime(SRec.Time);
      FindClose(SRec);
    end;
end;



{                                                                              }
{ File operations                                                              }
{                                                                              }
function GetFirstFileNameMatchingB(const FileMask: RawByteString): RawByteString;
var SRec : TSearchRec;
begin
  Result := '';
  if FindFirstB(FileMask, faAnyFile, SRec) = 0 then
    try
      repeat
        if SRec.Attr and faDirectory = 0 then
          begin
            Result := PathExtractFilePathB(FileMask) + ToRawByteString(SRec.Name);
            exit;
          end;
      until FindNext(SRec) <> 0;
    finally
      FindClose(SRec);
    end;
end;

function GetFirstFileNameMatchingU(const FileMask: UnicodeString): UnicodeString;
var SRec : TSearchRec;
begin
  Result := '';
  if FindFirstU(FileMask, faAnyFile, SRec) = 0 then
    try
      repeat
        if SRec.Attr and faDirectory = 0 then
          begin
            Result := PathExtractFilePathU(FileMask) + SRec.Name;
            exit;
          end;
      until FindNext(SRec) <> 0;
    finally
      FindClose(SRec);
    end;
end;

function GetFirstFileNameMatching(const FileMask: String): String;
var SRec : TSearchRec;
begin
  Result := '';
  if FindFirst(FileMask, faAnyFile, SRec) = 0 then
    try
      repeat
        if SRec.Attr and faDirectory = 0 then
          begin
            Result := PathExtractFilePath(FileMask) + SRec.Name;
            exit;
          end;
      until FindNext(SRec) <> 0;
    finally
      FindClose(SRec);
    end;
end;

function DirEntryGetAttrB(const FileName: RawByteString): Integer;
var SRec : TSearchRec;
begin
  if (FileName = '') or WinPathIsDriveLetterB(FileName) then
    Result := -1
  else
  if PathIsRootB(FileName) then
    Result := faDirectory
  else
  if FindFirstB(PathExclSuffixB(FileName), faAnyFile, SRec) = 0 then
    begin
      Result := SRec.Attr;
      FindClose(SRec);
    end
  else
    Result := -1;
end;

function DirEntryGetAttrU(const FileName: UnicodeString): Integer;
var SRec : TSearchRec;
begin
  if (FileName = '') or WinPathIsDriveLetterU(FileName) then
    Result := -1
  else
  if PathIsRootU(FileName) then
    Result := faDirectory
  else
  if FindFirstU(PathExclSuffixU(FileName), faAnyFile, SRec) = 0 then
    begin
      Result := SRec.Attr;
      FindClose(SRec);
    end
  else
    Result := -1;
end;

function DirEntryGetAttr(const FileName: String): Integer;
var SRec : TSearchRec;
begin
  if (FileName = '') or WinPathIsDriveLetter(FileName) then
    Result := -1
  else
  if PathIsRoot(FileName) then
    Result := faDirectory
  else
  if FindFirst(PathExclSuffix(FileName), faAnyFile, SRec) = 0 then
    begin
      Result := SRec.Attr;
      FindClose(SRec);
    end
  else
    Result := -1;
end;

function DirEntryIsDirectoryB(const FileName: RawByteString): Boolean;
var SRec : TSearchRec;
begin
  if (FileName = '') or WinPathIsDriveLetterB(FileName) then
    Result := False
  else
  if PathIsRootB(FileName) then
    Result := True
  else
  if FindFirstB(PathExclSuffixB(FileName), faDirectory, SRec) = 0 then
    begin
      Result := SRec.Attr and faDirectory <> 0;
      FindClose(SRec);
    end
  else
    Result := False;
end;

function DirEntryIsDirectoryU(const FileName: UnicodeString): Boolean;
var SRec : TSearchRec;
begin
  if (FileName = '') or WinPathIsDriveLetterU(FileName) then
    Result := False
  else
  if PathIsRootU(FileName) then
    Result := True
  else
  if FindFirstU(PathExclSuffixU(FileName), faDirectory, SRec) = 0 then
    begin
      Result := SRec.Attr and faDirectory <> 0;
      FindClose(SRec);
    end
  else
    Result := False;
end;

function DirEntryIsDirectory(const FileName: String): Boolean;
var SRec : TSearchRec;
begin
  if (FileName = '') or WinPathIsDriveLetter(FileName) then
    Result := False
  else
  if PathIsRoot(FileName) then
    Result := True
  else
  if FindFirst(PathExclSuffix(FileName), faDirectory, SRec) = 0 then
    begin
      Result := SRec.Attr and faDirectory <> 0;
      FindClose(SRec);
    end
  else
    Result := False;
end;

{$IFDEF DELPHI6_UP}{$WARN SYMBOL_PLATFORM OFF}{$ENDIF}
function FileHasAttr(const FileName: String; const Attr: Word): Boolean;
var A : Integer;
begin
  A := FileGetAttr(FileName);
  Result := (A >= 0) and (A and Attr <> 0);
end;

function FileHasAttrB(const FileName: RawByteString; const Attr: Word): Boolean;
var A : Integer;
begin
  A := FileGetAttr(ToStringB(FileName));
  Result := (A >= 0) and (A and Attr <> 0);
end;

function FileHasAttrU(const FileName: UnicodeString; const Attr: Word): Boolean;
var A : Integer;
begin
  A := FileGetAttr(ToStringU(FileName));
  Result := (A >= 0) and (A and Attr <> 0);
end;

procedure DirectoryCreateB(const DirectoryName: RawByteString);
begin
  if DirectoryName = '' then
    raise EFileError.Create(feInvalidParameter, SInvalidPath);
  if not CreateDir(ToStringB(DirectoryName)) then
    raise EFileError.CreateFmt(feDirectoryCreateError, SCannotCreateDirectory,
        [GetLastOSErrorMessage, DirectoryName]);
end;

procedure DirectoryCreateU(const DirectoryName: UnicodeString);
begin
  if DirectoryName = '' then
    raise EFileError.Create(feInvalidParameter, SInvalidPath);
  if not CreateDir(ToStringU(DirectoryName)) then
    raise EFileError.CreateFmt(feDirectoryCreateError, SCannotCreateDirectory,
        [GetLastOSErrorMessage, DirectoryName]);
end;

procedure DirectoryCreate(const DirectoryName: String);
begin
  if DirectoryName = '' then
    raise EFileError.Create(feInvalidParameter, SInvalidPath);
  if not CreateDir(DirectoryName) then
    raise EFileError.CreateFmt(feDirectoryCreateError, SCannotCreateDirectory,
        [GetLastOSErrorMessage, DirectoryName]);
end;



{                                                                              }
{ File operations                                                              }
{                                                                              }
procedure CopyFile(const FileName, DestName: String);
const
  BufferSize = 65536;
var DestFileName : String;
    SourceHandle : Integer;
    DestHandle   : Integer;
    Buffer       : array[0..BufferSize - 1] of Byte;
    BufferUsed   : Integer;
begin
  DestFileName := ExpandFileName(DestName);
  if FileHasAttr(DestFileName, faDirectory) then // if destination is a directory, append file name
    DestFileName := DestFileName + '\' + ExtractFileName(FileName);
  SourceHandle := FileOpen(FileName, fmShareDenyWrite);
  if SourceHandle < 0 then
    raise EFileError.CreateFmt(feFileOpenError, SCannotOpenFile, [GetLastOSErrorMessage,
        FileName]);
  try
    DestHandle := FileCreate(DestFileName);
    if DestHandle < 0 then
      raise EFileError.CreateFmt(feFileCreateError, SCannotCreateFile, [GetLastOSErrorMessage,
          DestFileName]);
    try
      repeat
        BufferUsed := FileRead(SourceHandle, Buffer[0], BufferSize);
        if BufferUsed > 0 then
          FileWrite(DestHandle, Buffer[0], BufferUsed);
      until BufferUsed < BufferSize;
    finally
      FileClose(DestHandle);
    end;
  finally
    FileClose(SourceHandle);
  end;
end;

procedure MoveFile(const FileName, DestName: String);
var Destination : String;
    Attr        : Integer;
begin
  Destination := ExpandFileName(DestName);
  if not RenameFile(FileName, Destination) then
    begin
      Attr := FileGetAttr(FileName);
      if (Attr < 0) or (Attr and faReadOnly <> 0) then
        raise EFileError.CreateFmt(feFileMoveError, SCannotMoveFile,
            [GetLastOSErrorMessage, FileName]);
      CopyFile(FileName, Destination);
      DeleteFile(FileName);
    end;
end;

function DeleteFiles(const FileMask: String): Boolean;
var SRec : TSearchRec;
    Path : String;
begin
  Result := FindFirst(FileMask, faAnyFile, SRec) = 0;
  if not Result then
    exit;
  try
    Path := ExtractFilePath(FileMask);
    repeat
      if (SRec.Name <> '') and (SRec.Name  <> '.') and (SRec.Name <> '..') and
         (SRec.Attr and (faVolumeID + faDirectory) = 0) then
        begin
          Result := DeleteFile(Path + SRec.Name);
          if not Result then
            break;
        end;
    until FindNext(SRec) <> 0;
  finally
    FindClose(SRec);
  end;
end;
{$IFDEF DELPHI6_UP}{$WARN SYMBOL_PLATFORM ON}{$ENDIF}



{$IFDEF MSWINDOWS}
{                                                                              }
{ Logical Drive functions                                                      }
{                                                                              }
function Word32IsBitSet(const A: Word32; const B: Integer): Boolean;
begin
  if (B < 0) or (B > 31) then
    Result := False
  else
    Result := (A and (1 shl B) <> 0);
end;

function DriveIsValidA(const Drive: ByteChar): Boolean;
var D : ByteChar;
begin
  D := UpCase(Drive);
  Result := D in ['A'..'Z'];
  if not Result then
    exit;
  Result := Word32IsBitSet(GetLogicalDrives, Ord(D) - Ord('A'));
end;

function DriveIsValidW(const Drive: WideChar): Boolean;
begin
  if (Ord(Drive) < Ord('A')) or
     (Ord(Drive) > Ord('z')) then
    Result := False
  else
    Result := DriveIsValidA(ByteChar(Drive));
end;

function DriveGetTypeA(const Path: AnsiString): TLogicalDriveType;
begin
  case GetDriveTypeA(PAnsiChar(Path)) of
    DRIVE_REMOVABLE : Result := DriveRemovable;
    DRIVE_FIXED     : Result := DriveFixed;
    DRIVE_REMOTE    : Result := DriveRemote;
    DRIVE_CDROM     : Result := DriveCDRom;
    DRIVE_RAMDISK   : Result := DriveRamDisk;
  else
    Result := DriveTypeUnknown;
  end;
end;

function DriveFreeSpaceA(const Path: AnsiString): Int64;
var D: Byte;
begin
  if WinPathHasDriveLetterB(Path) then
    D := Ord(UpCase(PAnsiChar(Path)^)) - Ord('A') + 1
  else
  if PathIsUNCPathB(Path) then
    begin
      Result := -1;
      exit;
    end
  else
    D := 0;
  Result := DiskFree(D);
end;

function DriveSizeA(const Path: AnsiString): Int64;
var D: Byte;
begin
  if WinPathHasDriveLetterB(Path) then
    D := Ord(UpCase(PAnsiChar(Path)^)) - Ord('A') + 1
  else
  if PathIsUNCPathB(Path) then
    begin
      Result := -1;
      exit;
    end
  else
    D := 0;
  Result := DiskSize(D);
end;
{$ENDIF}



{                                                                              }
{ Test cases                                                                   }
{                                                                              }
{$IFDEF DEBUG}
{$IFDEF TEST}
{$ASSERTIONS ON}
procedure Test;
{$IFDEF MSWINDOWS}
const
  TempPath = 'c:\temp';
  TempFilename = 'c:\temp\cFileUtilsTest.txt';
{$ENDIF}
{$IFDEF POSIX}
const
  TempPath = './temp';
  TempFilename = './temp/cFileUtilsTest.txt';
{$ENDIF}
begin
  // PathHasDriveLetter
  Assert(WinPathHasDriveLetterB('A:'), 'PathHasDriveLetter');
  Assert(WinPathHasDriveLetterB('a:'), 'PathHasDriveLetter');
  Assert(WinPathHasDriveLetterB('A:\'), 'PathHasDriveLetter');
  Assert(not WinPathHasDriveLetterB('a\'), 'PathHasDriveLetter');
  Assert(not WinPathHasDriveLetterB('\a\'), 'PathHasDriveLetter');
  Assert(not WinPathHasDriveLetterB('::'), 'PathHasDriveLetter');

  Assert(WinPathHasDriveLetter('A:'), 'PathHasDriveLetter');
  Assert(WinPathHasDriveLetter('a:'), 'PathHasDriveLetter');
  Assert(WinPathHasDriveLetter('A:\'), 'PathHasDriveLetter');
  Assert(not WinPathHasDriveLetter('a\'), 'PathHasDriveLetter');
  Assert(not WinPathHasDriveLetter('\a\'), 'PathHasDriveLetter');
  Assert(not WinPathHasDriveLetter('::'), 'PathHasDriveLetter');

  // PathIsDriveLetter
  Assert(WinPathIsDriveLetterB('B:'), 'PathIsDriveLetter');
  Assert(not WinPathIsDriveLetterB('B:\'), 'PathIsDriveLetter');

  Assert(WinPathIsDriveLetter('B:'), 'PathIsDriveLetter');
  Assert(not WinPathIsDriveLetter('B:\'), 'PathIsDriveLetter');

  // PathIsDriveRoot
  Assert(WinPathIsDriveRootB('C:\'), 'PathIsDriveRoot');
  Assert(not WinPathIsDriveRootB('C:'), 'PathIsDriveRoot');
  Assert(not WinPathIsDriveRootB('C:\A'), 'PathIsDriveRoot');

  Assert(WinPathIsDriveRoot('C:\'), 'PathIsDriveRoot');
  Assert(not WinPathIsDriveRoot('C:'), 'PathIsDriveRoot');
  Assert(not WinPathIsDriveRoot('C:\A'), 'PathIsDriveRoot');

  // PathIsAbsolute
  Assert(PathIsAbsoluteB('\'), 'PathIsAbsolute');
  Assert(PathIsAbsoluteB('\C'), 'PathIsAbsolute');
  Assert(PathIsAbsoluteB('\C\'), 'PathIsAbsolute');
  Assert(PathIsAbsoluteB('C:\'), 'PathIsAbsolute');
  Assert(PathIsAbsoluteB('C:'), 'PathIsAbsolute');
  Assert(PathIsAbsoluteB('\C\..\'), 'PathIsAbsolute');
  Assert(not PathIsAbsoluteB(''), 'PathIsAbsolute');
  Assert(not PathIsAbsoluteB('C'), 'PathIsAbsolute');
  Assert(not PathIsAbsoluteB('C\'), 'PathIsAbsolute');
  Assert(not PathIsAbsoluteB('C\D'), 'PathIsAbsolute');
  Assert(not PathIsAbsoluteB('C\D\'), 'PathIsAbsolute');
  Assert(not PathIsAbsoluteB('..\'), 'PathIsAbsolute');

  Assert(PathIsAbsolute('\'), 'PathIsAbsolute');
  Assert(PathIsAbsolute('\C'), 'PathIsAbsolute');
  Assert(PathIsAbsolute('\C\'), 'PathIsAbsolute');
  Assert(PathIsAbsolute('C:\'), 'PathIsAbsolute');
  Assert(PathIsAbsolute('C:'), 'PathIsAbsolute');
  Assert(PathIsAbsolute('\C\..\'), 'PathIsAbsolute');
  Assert(not PathIsAbsolute(''), 'PathIsAbsolute');
  Assert(not PathIsAbsolute('C'), 'PathIsAbsolute');
  Assert(not PathIsAbsolute('C\'), 'PathIsAbsolute');
  Assert(not PathIsAbsolute('C\D'), 'PathIsAbsolute');
  Assert(not PathIsAbsolute('C\D\'), 'PathIsAbsolute');
  Assert(not PathIsAbsolute('..\'), 'PathIsAbsolute');

  // PathIsDirectory
  Assert(PathIsDirectoryB('\'), 'PathIsDirectory');
  Assert(PathIsDirectoryB('\C\'), 'PathIsDirectory');
  Assert(PathIsDirectoryB('C:'), 'PathIsDirectory');
  Assert(PathIsDirectoryB('C:\'), 'PathIsDirectory');
  Assert(PathIsDirectoryB('C:\D\'), 'PathIsDirectory');
  Assert(not PathIsDirectoryB(''), 'PathIsDirectory');
  Assert(not PathIsDirectoryB('D'), 'PathIsDirectory');
  Assert(not PathIsDirectoryB('C\D'), 'PathIsDirectory');

  Assert(PathIsDirectory('\'), 'PathIsDirectory');
  Assert(PathIsDirectory('\C\'), 'PathIsDirectory');
  Assert(PathIsDirectory('C:'), 'PathIsDirectory');
  Assert(PathIsDirectory('C:\'), 'PathIsDirectory');
  Assert(PathIsDirectory('C:\D\'), 'PathIsDirectory');
  Assert(not PathIsDirectory(''), 'PathIsDirectory');
  Assert(not PathIsDirectory('D'), 'PathIsDirectory');
  Assert(not PathIsDirectory('C\D'), 'PathIsDirectory');

  // PathInclSuffix
  Assert(PathInclSuffixB('', '\') = '', 'PathInclSuffix');
  Assert(PathInclSuffixB('C', '\') = 'C\', 'PathInclSuffix');
  Assert(PathInclSuffixB('C\', '\') = 'C\', 'PathInclSuffix');
  Assert(PathInclSuffixB('C\D', '\') = 'C\D\', 'PathInclSuffix');
  Assert(PathInclSuffixB('C\D\', '\') = 'C\D\', 'PathInclSuffix');
  Assert(PathInclSuffixB('C:', '\') = 'C:\', 'PathInclSuffix');
  Assert(PathInclSuffixB('C:\', '\') = 'C:\', 'PathInclSuffix');

  Assert(PathInclSuffix('', '\') = '', 'PathInclSuffix');
  Assert(PathInclSuffix('C', '\') = 'C\', 'PathInclSuffix');
  Assert(PathInclSuffix('C\', '\') = 'C\', 'PathInclSuffix');
  Assert(PathInclSuffix('C\D', '\') = 'C\D\', 'PathInclSuffix');
  Assert(PathInclSuffix('C\D\', '\') = 'C\D\', 'PathInclSuffix');
  Assert(PathInclSuffix('C:', '\') = 'C:\', 'PathInclSuffix');
  Assert(PathInclSuffix('C:\', '\') = 'C:\', 'PathInclSuffix');

  // PathExclSuffix
  Assert(PathExclSuffixB('', '\') = '', 'PathExclSuffix');
  Assert(PathExclSuffixB('C', '\') = 'C', 'PathExclSuffix');
  Assert(PathExclSuffixB('C\', '\') = 'C', 'PathExclSuffix');
  Assert(PathExclSuffixB('C\D', '\') = 'C\D', 'PathExclSuffix');
  Assert(PathExclSuffixB('C\D\', '\') = 'C\D', 'PathExclSuffix');
  Assert(PathExclSuffixB('C:', '\') = 'C:', 'PathExclSuffix');
  Assert(PathExclSuffixB('C:\', '\') = 'C:', 'PathExclSuffix');

  Assert(PathExclSuffix('', '\') = '', 'PathExclSuffix');
  Assert(PathExclSuffix('C', '\') = 'C', 'PathExclSuffix');
  Assert(PathExclSuffix('C\', '\') = 'C', 'PathExclSuffix');
  Assert(PathExclSuffix('C\D', '\') = 'C\D', 'PathExclSuffix');
  Assert(PathExclSuffix('C\D\', '\') = 'C\D', 'PathExclSuffix');
  Assert(PathExclSuffix('C:', '\') = 'C:', 'PathExclSuffix');
  Assert(PathExclSuffix('C:\', '\') = 'C:', 'PathExclSuffix');

  // PathCanonical
  Assert(PathCanonicalB('', '\') = '', 'PathCanonical');
  Assert(PathCanonicalB('.', '\') = '', 'PathCanonical');
  Assert(PathCanonicalB('.\', '\') = '', 'PathCanonical');
  Assert(PathCanonicalB('..\', '\') = '..\', 'PathCanonical');
  Assert(PathCanonicalB('\..\', '\') = '\', 'PathCanonical');
  Assert(PathCanonicalB('\X\..\..\', '\') = '\', 'PathCanonical');
  Assert(PathCanonicalB('\..', '\') = '', 'PathCanonical');
  Assert(PathCanonicalB('X', '\') = 'X', 'PathCanonical');
  Assert(PathCanonicalB('\X', '\') = '\X', 'PathCanonical');
  Assert(PathCanonicalB('X.', '\') = 'X', 'PathCanonical');
  Assert(PathCanonicalB('.', '\') = '', 'PathCanonical');
  Assert(PathCanonicalB('\X.', '\') = '\X', 'PathCanonical');
  Assert(PathCanonicalB('\X.Y', '\') = '\X.Y', 'PathCanonical');
  Assert(PathCanonicalB('\X.Y\', '\') = '\X.Y\', 'PathCanonical');
  Assert(PathCanonicalB('\A\X..Y\', '\') = '\A\X..Y\', 'PathCanonical');
  Assert(PathCanonicalB('\A\.Y\', '\') = '\A\.Y\', 'PathCanonical');
  Assert(PathCanonicalB('\A\..Y\', '\') = '\A\..Y\', 'PathCanonical');
  Assert(PathCanonicalB('\A\Y..\', '\') = '\A\Y..\', 'PathCanonical');
  Assert(PathCanonicalB('\A\Y..', '\') = '\A\Y..', 'PathCanonical');
  Assert(PathCanonicalB('X', '\') = 'X', 'PathCanonical');
  Assert(PathCanonicalB('X\', '\') = 'X\', 'PathCanonical');
  Assert(PathCanonicalB('X\Y\..', '\') = 'X', 'PathCanonical');
  Assert(PathCanonicalB('X\Y\..\', '\') = 'X\', 'PathCanonical');
  Assert(PathCanonicalB('\X\Y\..', '\') = '\X', 'PathCanonical');
  Assert(PathCanonicalB('\X\Y\..\', '\') = '\X\', 'PathCanonical');
  Assert(PathCanonicalB('\X\Y\..\..', '\') = '', 'PathCanonical');
  Assert(PathCanonicalB('\X\Y\..\..\', '\') = '\', 'PathCanonical');
  Assert(PathCanonicalB('\A\.\.\X\.\Y\..\.\..\.\', '\') = '\A\', 'PathCanonical');
  Assert(PathCanonicalB('C:', '\') = 'C:', 'PathCanonical');
  Assert(PathCanonicalB('C:\', '\') = 'C:\', 'PathCanonical');
  Assert(PathCanonicalB('C:\A\..', '\') = 'C:', 'PathCanonical');
  Assert(PathCanonicalB('C:\A\..\', '\') = 'C:\', 'PathCanonical');
  Assert(PathCanonicalB('C:\..\', '\') = 'C:\', 'PathCanonical');
  Assert(PathCanonicalB('C:\..', '\') = 'C:', 'PathCanonical');
  Assert(PathCanonicalB('C:\A\..\..', '\') = 'C:', 'PathCanonical');
  Assert(PathCanonicalB('C:\A\..\..\', '\') = 'C:\', 'PathCanonical');
  Assert(PathCanonicalB('\A\B\..\C\D\..\', '\') = '\A\C\', 'PathCanonical');
  Assert(PathCanonicalB('\A\B\..\C\D\..\..\', '\') = '\A\', 'PathCanonical');
  Assert(PathCanonicalB('\A\B\..\C\D\..\..\..\', '\') = '\', 'PathCanonical');
  Assert(PathCanonicalB('\A\B\..\C\D\..\..\..\..\', '\') = '\', 'PathCanonical');

  Assert(PathExpandB('', '', '\') = '', 'PathExpand');
  Assert(PathExpandB('', '\', '\') = '\', 'PathExpand');
  Assert(PathExpandB('', '\C', '\') = '\C', 'PathExpand');
  Assert(PathExpandB('', '\C\', '\') = '\C\', 'PathExpand');
  Assert(PathExpandB('..\', '\C\', '\') = '\', 'PathExpand');
  Assert(PathExpandB('..', '\C\', '\') = '', 'PathExpand');
  Assert(PathExpandB('\..', '\C\', '\') = '', 'PathExpand');
  Assert(PathExpandB('\..\', '\C\', '\') = '\', 'PathExpand');
  Assert(PathExpandB('A', '..\', '\') = '..\A', 'PathExpand');
  Assert(PathExpandB('..\', '..\', '\') = '..\..\', 'PathExpand');
  Assert(PathExpandB('\', '', '\') = '\', 'PathExpand');
  Assert(PathExpandB('\', '\C', '\') = '\', 'PathExpand');
  Assert(PathExpandB('\A', '\C\', '\') = '\A', 'PathExpand');
  Assert(PathExpandB('\A\', '\C\', '\') = '\A\', 'PathExpand');
  Assert(PathExpandB('\A\B', '\C', '\') = '\A\B', 'PathExpand');
  Assert(PathExpandB('A\B', '\C', '\') = '\C\A\B', 'PathExpand');
  Assert(PathExpandB('A\B', '\C', '\') = '\C\A\B', 'PathExpand');
  Assert(PathExpandB('A\B', '\C\', '\') = '\C\A\B', 'PathExpand');
  Assert(PathExpandB('A\B', '\C\', '\') = '\C\A\B', 'PathExpand');
  Assert(PathExpandB('A\B', 'C\D', '\') = 'C\D\A\B', 'PathExpand');
  Assert(PathExpandB('..\A\B', 'C\D', '\') = 'C\A\B', 'PathExpand');
  Assert(PathExpandB('..\A\B', '\C\D', '\') = '\C\A\B', 'PathExpand');
  Assert(PathExpandB('..\..\A\B', 'C\D', '\') = 'A\B', 'PathExpand');
  Assert(PathExpandB('..\..\A\B', '\C\D', '\') = '\A\B', 'PathExpand');
  Assert(PathExpandB('..\..\..\A\B', '\C\D', '\') = '\A\B', 'PathExpand');
  Assert(PathExpandB('\..\A\B', '\C\D', '\') = '\A\B', 'PathExpand');
  Assert(PathExpandB('..\A\B', '\..\C\D', '\') = '\C\A\B', 'PathExpand');
  Assert(PathExpandB('..\A\B', '..\C\D', '\') = '..\C\A\B', 'PathExpand');
  Assert(PathExpandB('..\A\B', 'C:\C\D', '\') = 'C:\C\A\B', 'PathExpand');
  Assert(PathExpandB('..\A\B\', 'C:\C\D', '\') = 'C:\C\A\B\', 'PathExpand');

  Assert(FilePathB('C', '..\X\Y', 'A\B', '\') = 'A\X\Y\C', 'FilePath');
  Assert(FilePathB('C', '\X\Y', 'A\B', '\') = '\X\Y\C', 'FilePath');
  Assert(FilePathB('C', '', 'A\B', '\') = 'A\B\C', 'FilePath');
  Assert(FilePathB('', '\X\Y', 'A\B', '\') = '', 'FilePath');
  Assert(FilePathB('C', 'X\Y', 'A\B', '\') = 'A\B\X\Y\C', 'FilePath');
  Assert(FilePathB('C', 'X\Y', '', '\') = 'X\Y\C', 'FilePath');

  Assert(FilePath('C', '..\X\Y', 'A\B', '\') = 'A\X\Y\C', 'FilePath');
  Assert(FilePath('C', '\X\Y', 'A\B', '\') = '\X\Y\C', 'FilePath');
  Assert(FilePath('C', '', 'A\B', '\') = 'A\B\C', 'FilePath');
  Assert(FilePath('', '\X\Y', 'A\B', '\') = '', 'FilePath');
  Assert(FilePath('C', 'X\Y', 'A\B', '\') = 'A\B\X\Y\C', 'FilePath');
  Assert(FilePath('C', 'X\Y', '', '\') = 'X\Y\C', 'FilePath');
  Assert(DirectoryExpandB('', '', '\') = '', 'DirectoryExpand');
  Assert(DirectoryExpandB('', '\X', '\') = '\X\', 'DirectoryExpand');
  Assert(DirectoryExpandB('\', '\X', '\') = '\', 'DirectoryExpand');
  Assert(DirectoryExpandB('\A', '\X', '\') = '\A\', 'DirectoryExpand');
  Assert(DirectoryExpandB('\A\', '\X', '\') = '\A\', 'DirectoryExpand');
  Assert(DirectoryExpandB('\A\B', '\X', '\') = '\A\B\', 'DirectoryExpand');
  Assert(DirectoryExpandB('A', '\X', '\') = '\X\A\', 'DirectoryExpand');
  Assert(DirectoryExpandB('A\', '\X', '\') = '\X\A\', 'DirectoryExpand');
  Assert(DirectoryExpandB('C:', '\X', '\') = 'C:\', 'DirectoryExpand');
  Assert(DirectoryExpandB('C:\', '\X', '\') = 'C:\', 'DirectoryExpand');

  Assert(UnixPathToSafeWinPathB('/c/d.f') = '\c\d.f', 'UnixPathToWinPath');
  Assert(WinPathToSafeUnixPathB('\c\d.f') = '/c/d.f', 'WinPathToUnixPath');

  {$IFDEF MSWINDOWS}
  Assert(PathExtractFileNameB('c:\test\abc\file.txt') = 'file.txt');
  Assert(PathExtractFileNameU('c:\test\abc\file.txt') = 'file.txt');
  {$ENDIF}

  if not DirectoryExists(TempPath) then
    DirectoryCreate(TempPath);
  Assert(DirectoryExists(TempPath));

  WriteFileRawStr(TempFilename, 'FileUtilsTest', fsExclusive, fcCreateAlways, nil);
  Assert(FileExists(TempFilename));
  Assert(FileGetSize(TempFilename) = 13);
  Assert(ReadFileRawStr(TempFilename, fsDenyNone, fcOpenExisting, nil) = 'FileUtilsTest');

  AppendFileRawStr(TempFilename, '123', fsExclusive, fcOpenExisting, nil);
  Assert(FileGetSize(TempFilename) = 16);
  Assert(ReadFileRawStr(TempFilename, fsDenyNone, fcOpenExisting, nil) = 'FileUtilsTest123');

  FileDeleteEx(TempFilename);
  Assert(not FileExists(TempFilename));
end;
{$ENDIF}
{$ENDIF}



end.

