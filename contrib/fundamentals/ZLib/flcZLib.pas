{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals 5.00                                        }
{   File name:        flcZLib.pas                                              }
{   File version:     5.05                                                     }
{   Description:      ZLib compression                                         }
{                                                                              }
{   Copyright:        Copyright (c) 2008-2018, David J Butler                  }
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
{                                                                              }
{   ZLib copyright information:                                                }
{                                                                              }
{   Copyright (C) 1995-1998 Jean-loup Gailly and Mark Adler                    }
{                                                                              }
{   Permission is granted to anyone to use this software for any purpose,      }
{   including commercial applications, and to alter it and redistribute it     }
{   freely, subject to the following restrictions:                             }
{                                                                              }
{   1. The origin of this software must not be misrepresented; you must not    }
{      claim that you wrote the original software. If you use this software    }
{      in a product, an acknowledgment in the product documentation would be   }
{      appreciated but is not required.                                        }
{   2. Altered source versions must be plainly marked as such, and must not be }
{      misrepresented as being the original software.                          }
{   3. This notice may not be removed or altered from any source distribution. }
{                                                                              }
{   ZLib web page: http://www.zlib.net/                                        }
{                                                                              }
{                                                                              }
{ Revision history:                                                            }
{                                                                              }
{   2008/12/12  0.01  Initial version using zlib 1.2.3 object files.           }
{   2015/04/04  4.02  Portable version using zlibpas.                          }
{   2015/04/04  4.03  Stream implementation.                                   }
{   2016/01/09  5.04  Revised for Fundamentals 5.                              }
{   2018/08/13  5.05  Portability changes.                                     }
{                                                                              }
{ Supported compilers:                                                         }
{                                                                              }
{   Delphi 7 Win32                      5.04  2016/01/09                       }
{   Delphi 7 Win32 ZLIB_PORTABLE        5.04  2016/01/09                       }
{   Delphi XE7 Win32                    5.04  2016/01/09                       }
{   Delphi XE7 Win64                    5.04  2016/01/09                       }
{   Delphi XE7 Win32 ZLIB_PORTABLE      5.04  2016/01/09                       }
{   Delphi XE7 Win64 ZLIB_PORTABLE      5.04  2016/01/09                       }
{   FreePascal 2.6.2 Linux x64          4.03  2015/04/04                       }
{                                                                              }
{******************************************************************************}

{$INCLUDE ..\flcInclude.inc}

{.DEFINE ZLIB_PORTABLE}

{$IFDEF ZLIB_PORTABLE}
  {$DEFINE ZLIBPAS}
{$ELSE}
  {$IFDEF DELPHI}
    {$IFDEF OS_WIN32}
      {$DEFINE ZLIB123}
      {$DEFINE ZLIBOBJ}
    {$ELSE}
    {$IFDEF OS_WIN64}
      {$DEFINE ZLIB127}
      {$DEFINE ZLIBOBJ}
    {$ELSE}
      {$DEFINE ZLIBPAS}
    {$ENDIF}
    {$ENDIF}
  {$ELSE}
    {$DEFINE ZLIBPAS}
  {$ENDIF}
{$ENDIF}

{$IFDEF DEBUG}
  {$DEFINE ZLIB_DEBUG}
  {$IFDEF TEST}
    {$DEFINE ZLIB_TEST}
  {$ENDIF}
  {$IFDEF PROFILE}
    {$DEFINE ZLIB_PROFILE}
  {$ENDIF}
{$ENDIF}

unit flcZLib;

interface

uses
  SysUtils,
  Classes
  {$IFDEF ZLIBPAS},
  ZUtil,
  gZlib,
  zDeflate,
  zInflate
  {$ENDIF};



{ RawByteString }

{$IFNDEF ZLIBPAS}
{$IFNDEF SupportRawByteString}
type
  RawByteString = AnsiString;
  PRawByteString = ^RawByteString;
{$ENDIF}
{$ENDIF}



{ ZLib constants }

const
  {$IFDEF ZLIB123}
  ZLIB_VERSION : PAnsiChar = '1.2.3';
  ZLIB_VERNUM          = $1230;
  ZLIB_VER_MAJOR       = 1;
  ZLIB_VER_MINOR       = 2;
  ZLIB_VER_REVISION    = 3;
  ZLIB_VER_SUBREVISION = 0;
  {$ENDIF}
  {$IFDEF ZLIB127}
  ZLIB_VERSION : PAnsiChar = '1.2.7';
  ZLIB_VERNUM          = $1270;
  ZLIB_VER_MAJOR       = 1;
  ZLIB_VER_MINOR       = 2;
  ZLIB_VER_REVISION    = 7;
  ZLIB_VER_SUBREVISION = 0;
  {$ENDIF}
  {$IFDEF ZLIBPAS}
  ZLIB_VERSION : string = '1.1.2';
  ZLIB_VERNUM          = $1120;
  ZLIB_VER_MAJOR       = 1;
  ZLIB_VER_MINOR       = 1;
  ZLIB_VER_REVISION    = 2;
  ZLIB_VER_SUBREVISION = 0;
  {$ENDIF}

  // flush constants
  Z_NO_FLUSH      = 0;
  Z_PARTIAL_FLUSH = 1;
  Z_SYNC_FLUSH    = 2;
  Z_FULL_FLUSH    = 3;
  Z_FINISH        = 4;
  {$IFDEF ZLIB127}
  Z_BLOCK         = 5;
  Z_TREES         = 6;
  {$ENDIF}

  // return codes
  Z_OK            =  0;
  Z_STREAM_END    =  1;
  Z_NEED_DICT     =  2;
  Z_ERRNO         = -1;
  Z_STREAM_ERROR  = -2;
  Z_DATA_ERROR    = -3;
  Z_MEM_ERROR     = -4;
  Z_BUF_ERROR     = -5;
  Z_VERSION_ERROR = -6;

  // compression levels
  Z_NO_COMPRESSION       =  0;
  Z_BEST_SPEED           =  1;
  Z_BEST_COMPRESSION     =  9;
  Z_DEFAULT_COMPRESSION  = -1;

  // compression strategies
  Z_FILTERED         = 1;
  Z_HUFFMAN_ONLY     = 2;
  Z_DEFAULT_STRATEGY = 0;
  {$IFDEF ZLIB127}
  Z_RLE              = 3;
  Z_FIXED            = 4;
  {$ENDIF}

  // data types
  Z_BINARY   = 0;
  Z_TEXT     = 1;
  Z_ASCII    = Z_TEXT;
  Z_UNKNOWN  = 2;

  // compression methods
  Z_DEFLATED = 8;



{ ZLib declarations }

{$IFDEF ZLIBPAS}
type
  TZStreamRec = z_stream;
{$ENDIF}

{$IFDEF ZLIBOBJ}
type
  TZAlloc = function (const opaque: Pointer; const items, size: Integer): Pointer;
  TZFree  = procedure (const opaque, block: Pointer);

  { TZStreamRec }

  TZStreamRec = packed record
    next_in   : PAnsiChar; // next input byte
    avail_in  : LongInt;   // number of bytes available at next_in
    total_in  : LongInt;   // total nb of input bytes read so far

    next_out  : PAnsiChar; // next output byte should be put here
    avail_out : LongInt;   // remaining free space at next_out
    total_out : LongInt;   // total nb of bytes output so far

    msg       : PAnsiChar; // last error message, NULL if no error
    state     : Pointer;   // not visible by applications

    zalloc    : TZAlloc;   // used to allocate the internal state
    zfree     : TZFree;    // used to free the internal state
    opaque    : Pointer;   // private data object passed to zalloc and zfree

    data_type : Integer;   // best guess about the data type: ascii or binary
    adler     : LongInt;   // adler32 value of the uncompressed data
    reserved  : LongInt;   // reserved for future use
  end;

{ ZLib export routines }

function  adler32(const Adler: LongInt; const Buf: PAnsiChar; const Len: LongInt): LongInt;
function  deflateInit_(var Strm: TZStreamRec; const Level: LongInt; const Version: PAnsiChar; const RecSize: LongInt): LongInt;
function  deflate(var Strm: TZStreamRec; Flush: LongInt): LongInt;
function  deflateEnd(var Strm: TZStreamRec): LongInt;
function  inflateInit_(var Strm: TZStreamRec; const Version: PAnsiChar; const RecSize: LongInt): LongInt;
function  inflate(var Strm: TZStreamRec; const Flush: LongInt): LongInt;
function  inflateEnd(var Strm: TZStreamRec): LongInt;
function  inflateReset(var Strm: TZStreamRec): LongInt;
{$ENDIF}



{ ZLib helpers }

type
  TZLibCompressionLevel = (
    zclNone,
    zclBestSpeed,
    zclBestCompression,
    zclDefault
  );

type
  EZLibError = class(Exception);
  EZCompressionError = class(EZLibError);
  EZDecompressionError = class(EZLibError);

procedure ZLibCompressBuf(
          const InBuffer: Pointer; const InSize: Integer;
          out OutBuffer: Pointer; out OutSize: Integer;
          const Level: TZLibCompressionLevel = zclDefault);
function  ZLibCompressStr(
          const S: RawByteString;
          const Level: TZLibCompressionLevel = zclDefault): RawByteString;

procedure ZLibDecompressBuf(
          const InBuffer: Pointer; const InSize: Integer;
          out OutBuffer: Pointer; out OutSize: Integer);
function  ZLibDecompressStr(const S: RawByteString): RawByteString;



{ ZLib stream class }

type
  TZLibStreamBase = class(TStream)
  private
    FStream    : TStream;
    FStreamRec : TZStreamRec;

  public
    constructor Create(const AStream: TStream);
    destructor Destroy; override;
  end;

  TZLibCompressionStream = class(TZLibStreamBase)
  private
    FLevel       : TZLibCompressionLevel;
    FOutBuffer   : Pointer;
    FDeflateInit : Boolean;

    procedure DoDeflate(const Flush: Integer);

  protected
    function GetSize: Int64; override;

  public
    constructor Create(const AStream: TStream; const Level: TZLibCompressionLevel = zclDefault);
    destructor Destroy; override;

    function  Seek(Offset: Longint; Origin: Word): Longint; override;
    function  Read(var Buffer; Count: Longint): Longint; override;
    function  Write(const Buffer; Count: Longint): Longint; override;
    procedure Flush;
  end;

  TZLibDecompressionStream = class(TZLibStreamBase)
  private
    FInBuffer     : Pointer;
    FOutBuffer    : Pointer;
    FOutAvailable : Integer;
    FInflateInit  : Boolean;
    FDoInRead     : Boolean;

  protected
    function GetSize: Int64; override;

  public
    constructor Create(const AStream: TStream);
    destructor Destroy; override;

    function Seek(Offset: Longint; Origin: Word): Longint; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
  end;



{                                                                              }
{ Test cases                                                                   }
{                                                                              }
{$IFDEF ZLIB_TEST}
procedure Test;
{$ENDIF}
{$IFDEF ZLIB_PROFILE}
procedure Profile;
{$ENDIF}



implementation



{                                                                              }
{ zlib link libaries                                                           }
{                                                                              }
{ note: do not reorder these -- doing so will result in external functions     }
{ being undefined                                                              }
{                                                                              }
{ obj files taken as built by zlibex 1.2.3                                     }
{ bcc32 -c -6 -O2 -Ve -X -pr -a8 -b -d -k- -vi -tWM -r -RT- -ff *.c            }
{                                                                              }
{$IFDEF ZLIBOBJ}

  {$IFDEF OS_MSWIN}
    {$IFDEF COMMAND_LINE}
      {$L adler32.obj}
      {$L deflate.obj}
      {$L infback.obj}
      {$L inffast.obj}
      {$L inflate.obj}
      {$L inftrees.obj}
      {$L trees.obj}
      {$L compress.obj}
      {$L crc32.obj}
    {$ENDIF}
  {$ENDIF}

  {$IFDEF OS_WIN32}
    {$L zlib123/adler32.obj}
    {$L zlib123/deflate.obj}
    {$L zlib123/infback.obj}
    {$L zlib123/inffast.obj}
    {$L zlib123/inflate.obj}
    {$L zlib123/inftrees.obj}
    {$L zlib123/trees.obj}
    {$L zlib123/compress.obj}
    {$L zlib123/crc32.obj}
  {$ENDIF}

  {$IFDEF OS_WIN64}
    {$L zlib127/win64/deflate.obj}
    {$L zlib127/win64/inflate.obj}
    {$L zlib127/win64/inftrees.obj}
    {$L zlib127/win64/infback.obj}
    {$L zlib127/win64/inffast.obj}
    {$L zlib127/win64/trees.obj}
    {$L zlib127/win64/compress.obj}
    {$L zlib127/win64/adler32.obj}
    {$L zlib127/win64/crc32.obj}
  {$ENDIF}

{ zlib external utility routines }

function adler32(const Adler: LongInt; const Buf: PAnsiChar; const Len: LongInt): LongInt; external;

{ zlib external deflate routines }

function deflateInit_(var Strm: TZStreamRec; const Level: LongInt; const Version: PAnsiChar;
         const RecSize: LongInt): LongInt; external;
function deflate(var Strm: TZStreamRec; Flush: LongInt): LongInt; external;
function deflateEnd(var Strm: TZStreamRec): LongInt; external;

{ zlib external inflate routines }

function inflateInit_(var Strm: TZStreamRec; const Version: PAnsiChar;
         const RecSize: LongInt): LongInt; external;
function inflate(var Strm: TZStreamRec; const Flush: LongInt): LongInt; external;
function inflateEnd(var Strm: TZStreamRec): LongInt; external;
function inflateReset(var Strm: TZStreamRec): LongInt; external;

{ zlib external function implementations }

function zcalloc(const Opaque: Pointer; const Items, Size: LongInt): Pointer;
begin
  GetMem(Result, Items * Size);
end;

procedure zcfree(const Opaque, Block: Pointer);
begin
  FreeMem(Block);
end;

{$ENDIF}

{ zlib external symbol implementations }

const
  {$IFDEF ZLIB123}
  _z_errmsg: array[0..9] of PAnsiChar = (
  {$ENDIF}
  {$IFDEF ZLIB127}
  z_errmsg: array[0..9] of PAnsiChar = (
  {$ENDIF}
  {$IFDEF ZLIBPAS}
  z_errmsg: array[0..9] of String = (
  {$ENDIF}
    'Need dictionary',      // Z_NEED_DICT      (2)
    'Stream end',           // Z_STREAM_END     (1)
    '',                     // Z_OK             (0)
    'File error',           // Z_ERRNO          (-1)
    'Stream error',         // Z_STREAM_ERROR   (-2)
    'Data error',           // Z_DATA_ERROR     (-3)
    'Insufficient memory',  // Z_MEM_ERROR      (-4)
    'Buffer error',         // Z_BUF_ERROR      (-5)
    'Incompatible version', // Z_VERSION_ERROR  (-6)
    ''
    );

{ c external function implementations }

{$IFDEF ZLIB127}
function memset(const P: Pointer; const B: Byte; const Count: LongInt): Pointer; cdecl;
begin
  FillChar(P^, Count, B);
  Result := P;
end;
{$ENDIF}
{$IFDEF ZLIB123}
procedure _memset(const P: Pointer; const B: Byte; const Count: LongInt); cdecl;
begin
  FillChar(P^, Count, B);
end;
{$ENDIF}

{$IFDEF ZLIB127}
procedure memcpy(const Dest, Source: Pointer; const Count: LongInt); cdecl;
begin
  Move(Source^, Dest^, Count);
end;
{$ENDIF}
{$IFDEF ZLIB123}
procedure _memcpy(const Dest, Source: Pointer; const Count: LongInt); cdecl;
begin
  Move(Source^, Dest^, Count);
end;
{$ENDIF}



{ ZLib helpers }

function ZLibErrorMessage(const Code: LongInt): String;
begin
  case Code of
    {$IFDEF ZLIB123}
    -6..2 : Result := String(_z_errmsg[2 - Code]);
    {$ENDIF}
    {$IFDEF ZLIB127}
    -6..2 : Result := String(z_errmsg[2 - Code]);
    {$ENDIF}
    {$IFDEF ZLIBPAS}
    -6..2 : Result := z_errmsg[2 - Code];
    {$ENDIF}
  else
    Result := 'error ' + IntToStr(Code);
  end;
end;

function CheckZLibError(const Code: LongInt): LongInt; {$IFDEF UseInline}inline;{$ENDIF}
begin
  if Code < 0 then
    raise EZLibError.Create(ZLibErrorMessage(Code));
  Result := Code;
end;

procedure StreamRecInit(
          var StreamRec: TZStreamRec;
          const InBuffer: Pointer; const InSize: Integer;
          const OutBuffer: Pointer; const OutSize: Integer);
begin
  FillChar(StreamRec, SizeOf(TZStreamRec), 0);
  StreamRec.next_in := InBuffer;
  StreamRec.avail_in := InSize;
  StreamRec.next_out := OutBuffer;
  StreamRec.avail_out := OutSize;
end;

procedure StreamRecOutBufResize(
          var StreamRec: TZStreamRec;
          var OutBuffer: Pointer; const OutSize: Integer;
          const OutDone: Integer);
var
  P : PByte;
begin
  Assert(Assigned(OutBuffer));
  Assert(OutSize > 0);

  ReallocMem(OutBuffer, OutSize);
  P := OutBuffer;
  Inc(P, OutDone);
  {$IFDEF ZLIBPAS}
  StreamRec.next_out := pBytef(P);
  {$ELSE}
  StreamRec.next_out := PAnsiChar(P);
  {$ENDIF}
  StreamRec.avail_out := OutSize - OutDone;
end;

const
  ZCompressionLevelMapIn: array[TZLibCompressionLevel] of LongInt = (
    Z_NO_COMPRESSION,
    Z_BEST_SPEED,
    Z_BEST_COMPRESSION,
    Z_DEFAULT_COMPRESSION
  );

function DeflateInit(var StreamRec: TZStreamRec; const Level: TZLibCompressionLevel): Integer;
begin
  {$IFDEF ZLIBPAS}
  Result := DeflateInit_(@StreamRec, ZCompressionLevelMapIn[Level], ZLIB_VERSION,
      SizeOf(TZStreamRec));
  {$ELSE}
  Result := DeflateInit_(StreamRec, ZCompressionLevelMapIn[Level], ZLIB_VERSION,
      SizeOf(TZStreamRec));
  {$ENDIF}
end;

function InflateInit(var StreamRec: TZStreamRec): Integer;
begin
  {$IFDEF ZLIBPAS}
  Result := InflateInit_(@StreamRec, ZLIB_VERSION, SizeOf(TZStreamRec));
  {$ELSE}
  Result := InflateInit_(StreamRec, ZLIB_VERSION, SizeOf(TZStreamRec));
  {$ENDIF}
end;

{ ZLib Compress }

function ZLibCompressBufSizeEstimate(const InSize: Integer): Integer;
begin
  if InSize <= $F8 then
    Result := $100
  else
    Result := (InSize + $108) and not $FF;
end;

function ZLibCompressBufSizeReEstimate(const OutSize: Integer): Integer;
begin
  Assert(OutSize >= $100);
  Result := OutSize + (OutSize div 2);
end;

procedure ZLibCompressBuf(
          const InBuffer: Pointer; const InSize: Integer;
          out OutBuffer: Pointer; out OutSize: Integer;
          const Level: TZLibCompressionLevel);
var
  StreamRec : TZStreamRec;
  OutDone : Integer;

  procedure DoDeflate(const Flush: Integer);
  var
    PrevAvailOut, Ret : Integer;
    Fin : Boolean;
  begin
    Fin := False;
    repeat
      PrevAvailOut := StreamRec.avail_out;
      Ret := deflate(StreamRec, Flush);
      Inc(OutDone, PrevAvailOut - Integer(StreamRec.avail_out));
      if StreamRec.avail_out = 0 then
        begin
          OutSize := ZLibCompressBufSizeReEstimate(OutSize);
          StreamRecOutBufResize(StreamRec, OutBuffer, OutSize, OutDone);
        end
      else
        if (Ret = Z_OK) or (Ret = Z_STREAM_END) then
          Fin := True
        else
          raise EZLibError.Create(ZLibErrorMessage(Ret));
    until Fin;
  end;

var
  Ret : Integer;

begin
  OutSize := ZLibCompressBufSizeEstimate(InSize);
  GetMem(OutBuffer, OutSize);
  try
    StreamRecInit(StreamRec, InBuffer, InSize, OutBuffer, OutSize);
    Ret := DeflateInit(StreamRec, Level);
    CheckZLibError(Ret);
    try
      OutDone := 0;
      DoDeflate(Z_NO_FLUSH);
      DoDeflate(Z_FINISH);
    finally
      Ret := deflateEnd(StreamRec);
      CheckZLibError(Ret);
    end;
    if OutSize > OutDone then
      begin
        OutSize := OutDone;
        ReallocMem(OutBuffer, OutSize);
      end;
  except
    FreeMem(OutBuffer);
    raise;
  end;
end;

function ZLibCompressStr(const S: RawByteString; const Level: TZLibCompressionLevel): RawByteString;
var
  OutBuffer : Pointer;
  OutSize   : Integer;
begin
  ZLibCompressBuf(Pointer(S), Length(S), OutBuffer, OutSize, Level);
  try
    SetLength(Result, OutSize);
    Move(OutBuffer^, Pointer(Result)^, OutSize);
  finally
    FreeMem(OutBuffer);
  end;
end;

{ ZLib Decompress }

function ZLibDecompressBufSizeEstimate(const InSize: Integer): Integer;
begin
  if InSize <= $80 then
    Result := $100
  else
    Result := (InSize * 2 + $108) and not $FF;
end;

function ZLibDecompressBufSizeReEstimate(const OutSize: Integer): Integer;
begin
  Assert(OutSize >= $100);
  Result := OutSize + (OutSize div 2);
end;

procedure ZLibDecompressBuf(
          const InBuffer: Pointer; const InSize: Integer;
          out OutBuffer: Pointer; out OutSize: Integer);
var
  StreamRec : TZStreamRec;
  Ret : LongInt;
  Fin : Boolean;
  OutDone, PrevAvailOut : Integer;
begin
  OutSize := ZLibDecompressBufSizeEstimate(InSize);
  GetMem(OutBuffer, OutSize);
  try
    StreamRecInit(StreamRec, InBuffer, InSize, OutBuffer, OutSize);
    Ret := InflateInit(StreamRec);
    CheckZLibError(Ret);
    try
      OutDone := 0;
      Fin := False;
      repeat
        PrevAvailOut := StreamRec.avail_out;
        Ret := inflate(StreamRec, Z_NO_FLUSH);
        Inc(OutDone, PrevAvailOut - Integer(StreamRec.avail_out));
        if Ret = Z_STREAM_END then
          Fin := True
        else
        if StreamRec.avail_out > 0 then
          if Ret < 0 then
            raise EZLibError.Create(ZLibErrorMessage(Ret))
          else
            Fin := True
        else
          begin
            OutSize := ZLibDecompressBufSizeReEstimate(OutSize);
            StreamRecOutBufResize(StreamRec, OutBuffer, OutSize, OutDone);
          end;
      until Fin;
    finally
      CheckZLibError(inflateEnd(StreamRec));
    end;
    if OutSize > OutDone then
      begin
        OutSize := OutDone;
        ReallocMem(OutBuffer, OutSize);
      end;
  except
    FreeMem(OutBuffer);
    raise;
  end;
end;

function ZLibDecompressStr(const S: RawByteString): RawByteString;
var
  Buffer : Pointer;
  Size   : Integer;
begin
  ZLibDecompressBuf(Pointer(S), Length(S), Buffer, Size);
  try
    SetLength(Result, Size);
    if Size > 0 then
      Move(Buffer^, Pointer(Result)^, Size);
  finally
    FreeMem(Buffer);
  end;
end;



{                                                                              }
{ TZLibStreamBase                                                              }
{                                                                              }
const
  ZLib_StreamBufferSize = 16384;

constructor TZLibStreamBase.Create(const AStream: TStream);
begin
  Assert(Assigned(AStream));
  inherited Create;
  FStream := AStream;
end;

destructor TZLibStreamBase.Destroy;
begin
  inherited Destroy;
end;



{                                                                              }
{ TZLibCompressionStream                                                       }
{                                                                              }
constructor TZLibCompressionStream.Create(const AStream: TStream; const Level: TZLibCompressionLevel);
begin
  inherited Create(AStream);
  FLevel := Level;
  GetMem(FOutBuffer, ZLib_StreamBufferSize);
  FStreamRec.next_out := FOutBuffer;
  FStreamRec.avail_out := ZLib_StreamBufferSize;
  CheckZLibError(DeflateInit(FStreamRec, Level));
  FDeflateInit := True;
end;

destructor TZLibCompressionStream.Destroy;
begin
  if FDeflateInit then
    try
      Flush;
    finally
      deflateEnd(FStreamRec);
    end;
  if Assigned(FOutBuffer) then
    FreeMem(FOutBuffer);
  inherited Destroy;
end;

procedure TZLibCompressionStream.DoDeflate(const Flush: Integer);
var
  Fin : Boolean;
  Ret : Integer;
  OutDone : Integer;
begin
  Fin := False;
  repeat
    FStreamRec.next_out := FOutBuffer;
    FStreamRec.avail_out := ZLib_StreamBufferSize;
    Ret := deflate(FStreamRec, Flush);
    OutDone := ZLib_StreamBufferSize - FStreamRec.avail_out;
    if OutDone > 0 then
      FStream.Write(FOutBuffer^, OutDone)
    else
      if FStreamRec.avail_in = 0 then
        Fin := True
      else
      if (Ret = Z_OK) or (Ret = Z_STREAM_END) then
        Fin := True
      else
        raise EZLibError.Create(ZLibErrorMessage(Ret));
  until Fin;
end;

procedure TZLibCompressionStream.Flush;
begin
  DoDeflate(Z_FINISH);
end;

function TZLibCompressionStream.GetSize: Int64;
begin
  raise EZCompressionError.Create('Invalid method');
end;

function TZLibCompressionStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  raise EZCompressionError.Create('Invalid method');
end;

function TZLibCompressionStream.Read(var Buffer; Count: Longint): Longint;
begin
  raise EZCompressionError.Create('Invalid method');
end;

function TZLibCompressionStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := 0;
  if Count <= 0 then
    exit;
  FStreamRec.next_in := @Buffer;
  FStreamRec.avail_in := Count;
  DoDeflate(Z_NO_FLUSH);
  Result := Count;
end;



{                                                                              }
{ TZLibDecompressionStream                                                     }
{                                                                              }
constructor TZLibDecompressionStream.Create(const AStream: TStream);
begin
  inherited Create(AStream);
  GetMem(FInBuffer, ZLib_StreamBufferSize);
  GetMem(FOutBuffer, ZLib_StreamBufferSize);
  FStreamRec.next_in := FInBuffer;
  FStreamRec.avail_in := 0;
  FStreamRec.next_out := FOutBuffer;
  FStreamRec.avail_out := ZLib_StreamBufferSize;
  CheckZLibError(InflateInit(FStreamRec));
  FInflateInit := True;
  FDoInRead := True;
end;

destructor TZLibDecompressionStream.Destroy;
begin
  if FInflateInit then
    inflateEnd(FStreamRec);
  if Assigned(FOutBuffer) then
    FreeMem(FOutBuffer);
  if Assigned(FInBuffer) then
    FreeMem(FInBuffer);
  inherited Destroy;
end;

function TZLibDecompressionStream.GetSize: Int64;
begin
  raise EZDecompressionError.Create('Invalid method');
end;

function TZLibDecompressionStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  raise EZDecompressionError.Create('Invalid method');
end;

function TZLibDecompressionStream.Read(var Buffer; Count: Longint): Longint;
var
  P, Q : PByte;
  L, InBufUsed : Integer;
  Ret : Integer;
  Fin : Boolean;
  PrevAvailOut, OutDone : Integer;
begin
  Result := 0;
  if Count <= 0 then
    exit;
  P := @Buffer;
  Fin := False;
  repeat
    // read from buffer
    if FOutAvailable > 0 then
      begin
        L := FOutAvailable;
        if L > Count then
          L := Count;
        Move(FOutBuffer^, P^, L);
        if L < FOutAvailable then
          begin
            Q := FOutBuffer;
            Inc(Q, L);
            Move(Q^, FOutBuffer^, FOutAvailable - L);
          end;
        Inc(P, L);
        Dec(FOutAvailable, L);
        Inc(Result, L);
        if Result >= Count then
          exit; // required data read
      end;
    // fill buffer
    if FDoInRead then
      begin
        InBufUsed := FStream.Read(FInBuffer^, ZLib_StreamBufferSize);
        if InBufUsed = 0 then
          exit; // no more to read
        FStreamRec.next_in := FInBuffer;
        FStreamRec.avail_in := InBufUsed;
        FDoInRead := False;
      end;
    Q := FOutBuffer;
    Inc(Q, FOutAvailable);
    FStreamRec.next_out := Pointer(Q);
    FStreamRec.avail_out := ZLib_StreamBufferSize - FOutAvailable;
    PrevAvailOut := FStreamRec.avail_out;
    Ret := inflate(FStreamRec, Z_NO_FLUSH);
    OutDone := PrevAvailOut - Integer(FStreamRec.avail_out);
    Inc(FOutAvailable, OutDone);
    if OutDone = 0 then
      begin
        if Ret = Z_STREAM_END then
          Fin := True
        else
        if Ret = Z_BUF_ERROR then
          FDoInRead := True
        else
        if Ret < 0 then
          raise EZLibError.Create(ZLibErrorMessage(Ret));
      end
    else
      if (Ret < 0) and (Ret <> Z_BUF_ERROR) then
        raise EZLibError.Create(ZLibErrorMessage(Ret));
  until Fin;
end;

function TZLibDecompressionStream.Write(const Buffer; Count: Longint): Longint;
begin
  raise EZDecompressionError.Create('Invalid method');
end;



{                                                                              }
{ Self testing code                                                            }
{                                                                              }
{$IFDEF ZLIB_TEST}
{$ASSERTIONS ON}
procedure Test_TestCases;
var
  S : RawByteString;
begin
  S := ZLibCompressStr(#0, zclNone);
  Assert(Length(S) = 12);
  Assert(ZLibDecompressStr(S) = #0);

  S := ZLibCompressStr(#0, zclDefault);
  Assert(Length(S) = 9);
  Assert(ZLibDecompressStr(S) = #0);

  S := ZLibCompressStr('Fundamentals', zclDefault);
  Assert(Length(S) = 20);
  S := ZLibDecompressStr(S);
  Assert(S = 'Fundamentals');

  S := ZLibCompressStr('aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa', zclDefault);
  Assert(Length(S) = 12);
  Assert(ZLibDecompressStr(S) = 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa');

  Assert(ZLibDecompressStr(RawByteString(#$78#$DA#$01#$00#$00#$FF#$FF#$00#$00#$00#$01)) = ''); // compress none, ZLibObj (ZLib 1.2)
  Assert(ZLibDecompressStr(RawByteString(#$78#$01#$01#$00#$00#$FF#$FF#$00#$00#$00#$01)) = ''); // compress none, ZLibPas (ZLib 1.1)
end;

const
  TestStrCount = 8;
  TestStr : array[1..TestStrCount] of RawByteString = (
      '',
      #0,
      'Fundamentals',
      'ZLIB 1.2.3',
      'Test string with string repetition and string repetition',
      '...........................................................',
      RawByteString(#$FF#$00#$01#$02#$FE#$F0#$80#$7F),
      RawByteString(#$78#$9C#$03#$00#$00#$00#$00#$01)
      );

procedure Test_TestStrs;
var
  I : Integer;
  L : TZLibCompressionLevel;
begin
  for L := Low(TZLibCompressionLevel) to High(TZLibCompressionLevel) do
    for I := 1 to TestStrCount do
      Assert(ZLibDecompressStr(ZLibCompressStr(TestStr[I], L)) = TestStr[I]);
end;

procedure Test_LongStr;
var
  I : Integer;
  L : TZLibCompressionLevel;
  S : RawByteString;
  T : RawByteString;
begin
  S := 'Long string';
  for I := 1 to 10000 do
    S := S + ' testing ' + RawByteString(IntToStr(I));
  for L := Low(TZLibCompressionLevel) to High(TZLibCompressionLevel) do
    begin
      T := ZLibCompressStr(S, L);
      Assert( (L = zclNone) or
              ((S <> T) and (Length(T) < Length(S)))
            );
      Assert(ZLibDecompressStr(T) = S);
    end;
end;

procedure Test_Encoding_EmptyStr;
const
  EmptyTestStrCompressed : array[TZLibCompressionLevel] of RawByteString = (
      RawByteString(#$78#$DA#$01#$00#$00#$FF#$FF#$00#$00#$00#$01), // none
      RawByteString(#$78#$01#$03#$00#$00#$00#$00#$01),             // best speed
      RawByteString(#$78#$DA#$03#$00#$00#$00#$00#$01),             // best compression
      RawByteString(#$78#$9C#$03#$00#$00#$00#$00#$01)              // default
      );
var
  L : TZLibCompressionLevel;
  S : RawByteString;
begin
  for L := Low(TZLibCompressionLevel) to High(TZLibCompressionLevel) do
    Assert(ZLibDecompressStr(EmptyTestStrCompressed[L]) = '');
  for L := Low(TZLibCompressionLevel) to High(TZLibCompressionLevel) do
    begin
      S := ZLibCompressStr('', L);
      Assert(Length(S) = Length(EmptyTestStrCompressed[L]));
      Assert(ZLibDecompressStr(S) = '');
    end;
end;

procedure Test_CompressStream;

  procedure Test(const TestStr: RawByteString);
  var
    S : TZLibCompressionStream;
    T : TStringStream;
    F : RawByteString;
    G : RawByteString;
  begin
    T := TStringStream.Create('');
    S := TZLibCompressionStream.Create(T, zclDefault);
    F := TestStr;
    if F <> '' then
      S.Write(F[1], Length(F));
    S.Free;
    G := RawByteString(T.DataString);
    T.Free;
    Assert(ZLibDecompressStr(G) = TestStr);
  end;

var
  I : Integer;
  S : RawByteString;
begin
  for I := 1 to TestStrCount do
    Test(TestStr[I]);
  S := '';
  for I := 1 to 100000 do
    S := S + 'test';
  Test(S);
  S := '';
  for I := 1 to 100000 do
    S := S + 'test' + RawByteString(IntToStr(I));
  Test(S);
end;

procedure Test_DecompressStream;

  procedure Test(const TestStr: RawByteString; const BufSize: Integer);
  var
    S : TZLibDecompressionStream;
    T : TStringStream;
    F : RawByteString;
    L : Integer;
  begin
    T := TStringStream.Create(ZLibCompressStr(TestStr));
    S := TZLibDecompressionStream.Create(T);
    SetLength(F, BufSize);
    L := S.Read(F[1], Length(F));
    S.Free;
    T.Free;
    SetLength(F, L);
    Assert(F = TestStr);
  end;

var
  I : Integer;
  S : RawByteString;
begin
  for I := 1 to TestStrCount do
    Test(TestStr[I], 1000);
  S := '';
  for I := 1 to 100000 do
    S := S + 'test';
  Test(S, 500000);
  S := '';
  for I := 1 to 100000 do
    S := S + 'test' + RawByteString(IntToStr(I));
  Test(S, 1000000);
end;

procedure Test;
begin
  Test_TestCases;
  Test_Encoding_EmptyStr;
  Test_TestStrs;
  Test_LongStr;
  Test_CompressStream;
  Test_DecompressStream;
end;
{$ENDIF}

{$IFDEF ZLIB_PROFILE}
procedure Profile;
begin
end;
{$ENDIF}



end.

