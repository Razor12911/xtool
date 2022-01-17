{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals 5.00                                        }
{   File name:        flcUnicodeReader.pas                                     }
{   File version:     5.09                                                     }
{   Description:      Unicode reader class                                     }
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
{   2002/04/19  0.01  Initial version.                                         }
{   2002/10/28  3.02  Refactored for Fundamentals 3.                           }
{   2002/10/29  3.03  Bug fixes and improvements.                              }
{   2002/11/05  3.04  Improved buffer handling.                                }
{   2004/01/02  3.05  Changed reader's block size to 64K as suggested by Eb.   }
{   2005/08/27  4.06  Revised for Fundamentals 4.                              }
{   2011/10/16  4.07  Changes for Unicode Delphi.                              }
{   2015/04/01  4.08  Revision.                                                }
{   2016/01/17  5.09  Revised for Fundamentals 5.                              }
{                                                                              }
{ Supported compilers:                                                         }
{                                                                              }
{   Delphi 2010-10.4 Win32/Win64        5.09  2020/06/02                       }
{   Delphi 10.2-10.4 Linux64            5.09  2020/06/02                       }
{   FreePascal 3.0.4 Win64              5.09  2020/06/02                       }
{                                                                              }
{******************************************************************************}

{$INCLUDE ..\flcInclude.inc}

unit flcUnicodeReader;

interface

uses
  { System }
  SysUtils,

  { Fundamentals }
  flcStdTypes,
  flcStreams,
  flcUnicodeCodecs;



{                                                                              }
{ TUnicodeReader                                                               }
{                                                                              }
type
  TUnicodeReader = class
  protected
    FReader         : AReader;
    FReaderOwner    : Boolean;
    FReaderStartPos : Int64;
    FCodec          : TCustomUnicodeCodec;
    FCodecOwner     : Boolean;
    FBuffer         : UnicodeString;
    FBufPos         : Integer;
    FBufLen         : Integer;
    FRawBuf         : Pointer;
    FRawSize        : Integer;

    procedure ReadError;
    function  BufferChars(const Count: Integer): Integer;
    function  GetBuffer(const Count: Integer): Boolean;

  public
    constructor Create(const Reader: AReader;
                const ReaderOwner: Boolean = True;
                const Codec: TCustomUnicodeCodec = nil;
                const CodecOwner: Boolean = True);
    destructor Destroy; override;

    property  Codec: TCustomUnicodeCodec read FCodec;
    property  CodecOwner: Boolean read FCodecOwner write FCodecOwner;

    procedure Reset;
    function  EOF: Boolean;

    function  ReadChar: WideChar;
    function  ReadWide(const Buf: PWideChar; const Len: Integer): Integer;
    function  ReadUnicodeStr(const Len: Integer): UnicodeString;
    function  ReadUTF8Str(const Len: Integer): RawByteString;

    procedure Skip(const Count: Integer);
    function  SkipAll(const CharMatchFunc: TWideCharMatchFunction): Integer;

    function  MatchChar(const CharMatchFunc: TWideCharMatchFunction;
              const Skip: Boolean): Boolean;
    function  MatchWideChar(const Ch: WideChar; const Skip: Boolean): Boolean;

    function  MatchRawByteStr(const S: RawByteString; const CaseSensitive: Boolean;
              const Skip: Boolean): Boolean;
    function  MatchRawByteStrDelimited(const S: RawByteString;
              const CaseSensitive: Boolean;
              const Delimiter: TWideCharMatchFunction;
              const Skip: Boolean): Boolean;

    function  MatchChars(const CharMatchFunc: TWideCharMatchFunction): Integer;
    function  MatchRawByteChars(const C: ByteCharSet): Integer;

    function  LocateRawByteChar(const C: ByteCharSet;
              const Optional: Boolean = False): Integer;
    function  LocateRawByteStr(const S: RawByteString; const CaseSensitive: Boolean;
              const Optional: Boolean = False): Integer;

    function  PeekChar: WideChar;
    function  SkipAndPeek(out Ch: WideChar): Boolean;
    function  GetPeekBuffer(const Len: Integer; var Buffer: PWideChar): Integer;

    function  ReadChars(const CharMatchFunc: TWideCharMatchFunction): UnicodeString;
    function  ReadRawByteChars(const C: ByteCharSet): RawByteString;

    function  SkipToRawByteChar(const C: ByteCharSet;
              const SkipDelimiter: Boolean): Integer;
    function  ReadToRawByteChar(const C: ByteCharSet;
              const SkipDelimiter: Boolean = False): UnicodeString;
    function  ReadUTF8StrToRawByteChar(const C: ByteCharSet;
              const SkipDelimiter: Boolean = False): RawByteString;

    function  ReadToRawByteStr(const S: RawByteString;
              const CaseSensitive: Boolean = True;
              const SkipDelimiter: Boolean = False): UnicodeString;
    function  ReadUTF8StrToRawByteStr(const S: RawByteString;
              const CaseSensitive: Boolean = True;
              const SkipDelimiter: Boolean = False): RawByteString;
  end;
  EUnicodeReader = class(Exception);
  EUnicodeReaderReadError = class(EUnicodeReader);



{                                                                              }
{ TUnicodeMemoryReader                                                         }
{                                                                              }
type
  TUnicodeMemoryReader = class(TUnicodeReader)
  public
    constructor Create(const Data: Pointer; const Size: Integer;
                const Codec: TCustomUnicodeCodec = nil;
                const CodecOwner: Boolean = True);
  end;



{$IFDEF SupportRawByteString}
{                                                                              }
{ TUnicodeLongStringReader                                                     }
{                                                                              }
type
  TUnicodeLongStringReader = class(TUnicodeReader)
  public
    constructor Create(const DataStr: RawByteString;
                const Codec: TCustomUnicodeCodec = nil;
                const CodecOwner: Boolean = True);
  end;
{$ENDIF}



{                                                                              }
{ TUnicodeFileReader                                                           }
{                                                                              }
type
  TUnicodeFileReader = class(TUnicodeReader)
  public
    constructor Create(const FileName: String;
                const Codec: TCustomUnicodeCodec = nil;
                const CodecOwner: Boolean = True);
  end;



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
  { Fundamentals }
  flcUtils,
  flcUTF,
  flcZeroTermStrings;



resourcestring
  SReadError = 'Read error';



{                                                                              }
{ TUnicodeReader                                                               }
{                                                                              }
const
  ReaderBlockSize = 65536; // 64K

constructor TUnicodeReader.Create(const Reader: AReader;
    const ReaderOwner: Boolean;
    const Codec: TCustomUnicodeCodec;
    const CodecOwner: Boolean);
begin
  inherited Create;
  Assert(Assigned(Reader));
  FReader := Reader;
  FReaderOwner := ReaderOwner;
  FReaderStartPos := Reader.Position;
  FCodec := Codec;
  FCodecOwner := CodecOwner;
  GetMem(FRawBuf, ReaderBlockSize);
end;

destructor TUnicodeReader.Destroy;
begin
  if Assigned(FRawBuf) then
    FreeMem(FRawBuf);
  if FReaderOwner then
    FreeAndNil(FReader);
  if FCodecOwner then
    FreeAndNil(FCodec);
  inherited Destroy;
end;

procedure TUnicodeReader.ReadError;
begin
  raise EUnicodeReaderReadError.Create(SReadError);
end;

procedure TUnicodeReader.Reset;
begin
  FReader.Position := FReaderStartPos;
  FBufPos := 0;
  FBufLen := 0;
  // Free excessively large buffer, keep part of it for re-use
  if Length(FBuffer) > 4 * ReaderBlockSize then
    SetLength(FBuffer, 4 * ReaderBlockSize);
end;

function TUnicodeReader.EOF: Boolean;
begin
  if FBufPos < FBufLen then
    Result := False
  else
    Result := FReader.EOF;
end;

// Attempts to decode at least Count characters
// Returns number of decoded characters available in buffer
function TUnicodeReader.BufferChars(const Count: Integer): Integer;
var I, J, L, M, N: Integer;
    P: PByte;
    Q: PWideChar;
begin
  // Check available characters
  Result := FBufLen - FBufPos;
  if Result >= Count then
    exit;
  L := Length(FBuffer);
  if L > 0 then
    begin
      // Reorganise buffer
      if Result <= 0 then // buffer empty
        begin
          // move pointer to front
          FBufPos := 0;
          FBufLen := 0;
        end
      else
      if (Result <= ReaderBlockSize div 16) or // buffer is nearly empty; or
         (FBufPos >= 4 * ReaderBlockSize) then // buffer has too much unused space at front
        begin
          // Move data to front
          Q := Pointer(FBuffer);
          Inc(Q, FBufPos);
          Move(Q^, Pointer(FBuffer)^, Result * Sizeof(WideChar));
          FBufPos := 0;
          FBufLen := Result;
        end;
    end;
  // Fill unicode buffer
  repeat
    // Fill raw character buffer
    P := FRawBuf;
    Inc(P, FRawSize);
    J := FReader.Read(P^, ReaderBlockSize - FRawSize);
    if J <= 0 then // eof
      exit;
    Inc(FRawSize, J);
    // Decode to unicode buffer
    if Assigned(FCodec) then
      begin
        // Decode raw buffer using codec
        P := FRawBuf;
        J := FRawSize;
        L := Length(FBuffer) - FBufLen;
        repeat
          if L < ReaderBlockSize then
            begin
              // grow unicode buffer to fit at least one raw buffer
              L := ReaderBlockSize;
              SetLength(FBuffer, FBufLen + L);
            end;
          Q := Pointer(FBuffer);
          Inc(Q, FBufLen);
          FCodec.Decode(P, J, Q, L * Sizeof(WideChar), M, N);
          Inc(P, M);
          Dec(J, M);
          Inc(FBufLen, N);
          Dec(L, N);
        until (J <= 0) or (L > 0);
        I := FRawSize - J;
      end
    else
      begin
        // read raw 16-bit unicode
        I := FRawSize div Sizeof(WideChar);
        L := Length(FBuffer) - FBufLen;
        if L < I then
          begin
            L := I;
            SetLength(FBuffer, FBufLen + L);
          end;
        Q := Pointer(FBuffer);
        Inc(Q, FBufLen);
        Inc(FBufLen, I);
        I := I * Sizeof(WideChar);
        Move(FRawBuf^, Q^, I);
      end;
    // Move undecoded raw data to front of buffer
    if I < FRawSize then
      begin
        Move(P^, FRawBuf^, FRawSize - I);
        Dec(FRawSize, I);
      end
    else
      FRawSize := 0;
    // Check if enough characters have been buffered
    Result := FBufLen - FBufPos;
  until Result >= Count;
end;

function TUnicodeReader.GetBuffer(const Count: Integer): Boolean;
begin
  Result := FBufLen - FBufPos >= Count;
  if Result then
    exit;
  Result := BufferChars(Count) >= Count;
end;

function TUnicodeReader.ReadWide(const Buf: PWideChar;
    const Len: Integer): Integer;
var P: PWideChar;
begin
  if Len <= 0 then
    begin
      Result := 0;
      exit;
    end;
  // buffer
  Result := FBufLen - FBufPos;
  if Result < Len then
    Result := BufferChars(Len);
  if Result > Len then
    Result := Len;
  // read
  P := Pointer(FBuffer);
  Inc(P, FBufPos);
  Move(P^, Buf^, Sizeof(WideChar) * Result);
  Inc(FBufPos, Result);
end;

function TUnicodeReader.ReadUnicodeStr(const Len: Integer): UnicodeString;
var L: Integer;
    P: PWideChar;
begin
  if Len <= 0 then
    begin
      Result := '';
      exit;
    end;
  // buffer
  L := FBufLen - FBufPos;
  if L < Len then
    L := BufferChars(Len);
  if L > Len then
    L := Len;
  // read
  P := Pointer(FBuffer);
  Inc(P, FBufPos);
  SetLength(Result, L);
  Move(P^, Pointer(Result)^, Sizeof(WideChar) * L);
  Inc(FBufPos, L);
end;

function TUnicodeReader.ReadUTF8Str(const Len: Integer): RawByteString;
var L: Integer;
    P: PWideChar;
begin
  if Len <= 0 then
    begin
      SetLength(Result, 0);
      exit;
    end;
  // buffer
  L := FBufLen - FBufPos;
  if L < Len then
    L := BufferChars(Len);
  if L > Len then
    L := Len;
  // read
  P := Pointer(FBuffer);
  Inc(P, FBufPos);
  Result := WideBufToUTF8String(P, L);
  Inc(FBufPos, L);
end;

procedure TUnicodeReader.Skip(const Count: Integer);
begin
  // buffer
  if Count <= 0 then
    exit;
  if FBufLen - FBufPos < Count then
    if not GetBuffer(Count) then
      ReadError;
  // skip
  Inc(FBufPos, Count);
end;

function TUnicodeReader.SkipAll(const CharMatchFunc: TWideCharMatchFunction): Integer;
var P: PWideChar;
    N, I: Integer;
begin
  Result := 0;
  // buffer
  N := FBufLen - FBufPos;
  if N <= 0 then
    N := BufferChars(1);
  repeat
    if N <= 0 then // eof
      exit;
    // skip
    P := Pointer(FBuffer);
    Inc(P, FBufPos);
    for I := 1 to N do
      if not CharMatchFunc(P^) then
        exit
      else
        begin
          Inc(Result);
          Inc(FBufPos);
          Inc(P);
        end;
    // buffer more
    N := BufferChars(1);
  until False;
end;

function TUnicodeReader.MatchChar(const CharMatchFunc: TWideCharMatchFunction;
    const Skip: Boolean): Boolean;
var P: PWideChar;
begin
  // buffer
  if FBufPos >= FBufLen then
    if BufferChars(1) <= 0 then // eof
      begin
        Result := False;
        exit;
      end;
  // match
  P := Pointer(FBuffer);
  Inc(P, FBufPos);
  Result := CharMatchFunc(P^);
  // skip
  if Skip and Result then
    Inc(FBufPos);
end;

function TUnicodeReader.MatchWideChar(const Ch: WideChar;
    const Skip: Boolean): Boolean;
var P: PWideChar;
begin
  // buffer
  if FBufPos >= FBufLen then
    if BufferChars(1) <= 0 then // eof
      begin
        Result := False;
        exit;
      end;
  // match
  P := Pointer(FBuffer);
  Inc(P, FBufPos);
  Result := P^ = Ch;
  // skip
  if Skip and Result then
    Inc(FBufPos);
end;

function TUnicodeReader.MatchRawByteStr(const S: RawByteString;
    const CaseSensitive: Boolean; const Skip: Boolean): Boolean;
var L: Integer;
    P: PWideChar;
begin
  L := Length(S);
  if L = 0 then
    begin
      Result := False;
      exit;
    end;
  // buffer
  if FBufLen - FBufPos < L then
    if BufferChars(L) < L then // eof
      begin
        Result := False;
        exit;
      end;
  // match
  P := Pointer(FBuffer);
  Inc(P, FBufPos);
  Result := StrZMatchStrAsciiBW(P, S, CaseSensitive);
  // skip
  if Skip and Result then
    Inc(FBufPos, L);
end;

function TUnicodeReader.MatchRawByteStrDelimited(const S: RawByteString;
    const CaseSensitive: Boolean; const Delimiter: TWideCharMatchFunction;
    const Skip: Boolean): Boolean;
var L: Integer;
    P: PWideChar;
begin
  L := Length(S);
  // buffer
  if FBufLen - FBufPos < L + 1 then
    if BufferChars(L + 1) < L + 1 then // eof
      begin
        Result := False;
        exit;
      end;
  // match
  P := Pointer(FBuffer);
  Inc(P, FBufPos);
  Result := StrZMatchStrAsciiBW(P, S, CaseSensitive);
  if not Result then
    exit;
  Inc(P, L);
  Result := Delimiter(P^);
  // skip
  if Skip and Result then
    Inc(FBufPos, L);
end;

function TUnicodeReader.MatchChars(const CharMatchFunc: TWideCharMatchFunction): Integer;
var P: PWideChar;
    N, I: Integer;
begin
  Result := 0;
  // buffer
  N := FBufLen - FBufPos;
  if N <= 0 then
    N := BufferChars(1);
  repeat
    if N < Result + 1 then // eof
      exit;
    // match
    P := Pointer(FBuffer);
    Inc(P, FBufPos + Result);
    for I := Result + 1 to N do
      if not CharMatchFunc(P^) then
        exit
      else
        begin
          Inc(Result);
          Inc(P);
        end;
    // buffer more
    N := BufferChars(Result + 1);
  until False;
end;

function TUnicodeReader.MatchRawByteChars(const C: ByteCharSet): Integer;
var P: PWideChar;
    N, I: Integer;
begin
  Result := 0;
  // buffer
  N := FBufLen - FBufPos;
  if N <= 0 then
    N := BufferChars(1);
  repeat
    if N < Result + 1 then // eof
      exit;
    // match
    P := Pointer(FBuffer);
    Inc(P, FBufPos + Result);
    for I := Result + 1 to N do
      if (Ord(P^) > $FF) or not (ByteChar(Byte(P^)) in C) then
        exit
      else
        begin
          Inc(Result);
          Inc(P);
        end;
    // buffer more
    N := BufferChars(Result + 1);
  until False;
end;

function TUnicodeReader.LocateRawByteChar(const C: ByteCharSet;
    const Optional: Boolean): Integer;
var P: PWideChar;
    N, I: Integer;
    V: Word;
begin
  Result := 0;
  // buffer
  N := FBufLen - FBufPos;
  if N <= 0 then
    N := BufferChars(1);
  repeat
    if N < Result + 1 then
      begin
        // eof
        if Optional then
          Result := N
        else
          Result := -1;
        exit;
      end;
    // locate
    P := Pointer(FBuffer);
    Inc(P, FBufPos + Result);
    for I := Result + 1 to N do
      begin
        V := Ord(P^);
        if (V <= $FF) and (ByteChar(Byte(V)) in C) then
          // found
          exit;
        Inc(Result);
        Inc(P);
      end;
    // buffer more
    N := BufferChars(Result + 1);
  until False;
end;

function TUnicodeReader.LocateRawByteStr(const S: RawByteString;
    const CaseSensitive: Boolean;
    const Optional: Boolean): Integer;
var P: PWideChar;
    M, N, I: Integer;
begin
  Result := 0;
  M := Length(S);
  if M = 0 then
    exit;
  // buffer
  N := FBufLen - FBufPos;
  if N < M then
    N := BufferChars(M);
  repeat
    if N < Result + M then
      begin
        // eof
        if Optional then
          Result := N
        else
          Result := -1;
        exit;
      end;
    P := Pointer(FBuffer);
    Inc(P, FBufPos + Result);
    for I := Result + 1 to N - M + 1 do
      if StrZMatchStrAsciiBW(P, S, CaseSensitive) then
        // found
        exit
      else
        begin
          Inc(Result);
          Inc(P);
        end;
    // buffer more characters
    N := BufferChars(Result + M);
  until False;
end;

function TUnicodeReader.PeekChar: WideChar;
var P: PWideChar;
begin
  // buffer
  if FBufPos >= FBufLen then
    if not GetBuffer(1) then
      ReadError;
  // peek
  P := Pointer(FBuffer);
  Inc(P, FBufPos);
  Result := P^;
end;

function TUnicodeReader.GetPeekBuffer(const Len: Integer;
    var Buffer: PWideChar): Integer;
var P: PWideChar;
begin
  // Result returns the number of wide characters in Buffer.
  // Buffer points to the actual data. The buffer is only valid until the next
  // call to the reader.
  Result := BufferChars(Len);
  if Result = 0 then
    Buffer := nil
  else
    begin
      P := Pointer(FBuffer);
      Inc(P, FBufPos);
      Buffer := P;
    end;
end;

function TUnicodeReader.ReadChar: WideChar;
var P: PWideChar;
    O: Integer;
begin
  // buffer
  O := FBufPos;
  if O >= FBufLen then
    if GetBuffer(1) then
      O := FBufPos
    else
      ReadError;
  // read
  P := Pointer(FBuffer);
  Inc(P, O);
  Result := P^;
  Inc(FBufPos);
end;

function TUnicodeReader.SkipAndPeek(out Ch: WideChar): Boolean;
var P: PWideChar;
    C: Integer;
begin
  // Skip
  C := FBufLen - FBufPos;
  if C >= 2 then
    begin
      Inc(FBufPos);
      Result := True;
    end
  else
    begin
      Result := GetBuffer(2);
      if FBufPos < FBufLen then
        Inc(FBufPos);
    end;
  if Result then
    begin
      // Peek
      P := Pointer(FBuffer);
      Inc(P, FBufPos);
      Ch := P^;
    end
  else
    Ch := WideChar(#0);
end;

function TUnicodeReader.ReadChars(const CharMatchFunc: TWideCharMatchFunction): UnicodeString;
var P: PWideChar;
    L: Integer;
begin
  // calculate length
  L := MatchChars(CharMatchFunc);
  if L = 0 then
    Result := ''
  else
    begin
      // read
      SetLength(Result, L);
      P := Pointer(FBuffer);
      Inc(P, FBufPos);
      Move(P^, Pointer(Result)^, Sizeof(WideChar) * L);
      Inc(FBufPos, L);
    end;
end;

function TUnicodeReader.ReadRawByteChars(const C: ByteCharSet): RawByteString;
var P : PWideChar;
    L : Integer;
begin
  // calculate length
  L := MatchRawByteChars(C);
  if L = 0 then
    SetLength(Result, 0)
  else
    begin
      // read
      SetLength(Result, L);
      P := Pointer(FBuffer);
      Inc(P, FBufPos);
      Result := WideBufToRawByteString(P, L);
      Inc(FBufPos, L);
    end;
end;

function TUnicodeReader.SkipToRawByteChar(const C: ByteCharSet;
    const SkipDelimiter: Boolean): Integer;
var L: Integer;
begin
  // locate
  L := LocateRawByteChar(C, False);
  if L = 0 then
    Result := 0
  else
    begin
      // skip characters
      if L < 0 then
        Result := FBufLen - FBufPos
      else
        Result := L;
      Inc(FBufPos, Result);
    end;
  // skip delimiter
  if (L >= 0) and SkipDelimiter then
    Inc(FBufPos);
end;

function TUnicodeReader.ReadToRawByteChar(const C: ByteCharSet;
    const SkipDelimiter: Boolean): UnicodeString;
var L, M: Integer;
begin
  // locate
  L := LocateRawByteChar(C, False);
  if L = 0 then
    Result := ''
  else
    begin
      // read
      if L < 0 then
        M := FBufLen - FBufPos
      else
        M := L;
      Result := ReadUnicodeStr(M);
    end;
  // skip delimiter
  if (L >= 0) and SkipDelimiter then
    Inc(FBufPos);
end;

function TUnicodeReader.ReadUTF8StrToRawByteChar(const C: ByteCharSet;
    const SkipDelimiter: Boolean): RawByteString;
var L, M: Integer;
begin
  // locate
  L := LocateRawByteChar(C, False);
  if L = 0 then
    SetLength(Result, 0)
  else
    begin
      // read
      if L < 0 then
        M := FBufLen - FBufPos
      else
        M := L;
      Result := ReadUTF8Str(M);
    end;
  // skip delimiter
  if (L >= 0) and SkipDelimiter then
    Inc(FBufPos);
end;

function TUnicodeReader.ReadToRawByteStr(const S: RawByteString;
    const CaseSensitive: Boolean; const SkipDelimiter: Boolean): UnicodeString;
var L, M: Integer;
begin
  // locate
  L := LocateRawByteStr(S, CaseSensitive, False);
  if L = 0 then
    Result := ''
  else
    begin
      // read
      if L < 0 then
        M := FBufLen - FBufPos
      else
        M := L;
      Result := ReadUnicodeStr(M);
    end;
  // skip delimiter
  if (L >= 0) and SkipDelimiter then
    Inc(FBufPos, Length(S));
end;

function TUnicodeReader.ReadUTF8StrToRawByteStr(const S: RawByteString;
    const CaseSensitive: Boolean; const SkipDelimiter: Boolean): RawByteString;
var L, M: Integer;
begin
  // locate
  L := LocateRawByteStr(S, CaseSensitive, False);
  if L = 0 then
    SetLength(Result, 0)
  else
    begin
      // read
      if L < 0 then
        M := FBufLen - FBufPos
      else
        M := L;
      Result := ReadUTF8Str(M);
    end;
  // skip delimiter
  if (L >= 0) and SkipDelimiter then
    Inc(FBufPos, Length(S));
end;



{                                                                              }
{ TUnicodeMemoryReader                                                         }
{                                                                              }
constructor TUnicodeMemoryReader.Create(const Data: Pointer; const Size: Integer;
    const Codec: TCustomUnicodeCodec; const CodecOwner: Boolean);
begin
  inherited Create(TMemoryReader.Create(Data, Size), True, Codec, CodecOwner);
end;



{$IFDEF SupportRawByteString}
{                                                                              }
{ TUnicodeLongStringReader                                                     }
{                                                                              }
constructor TUnicodeLongStringReader.Create(const DataStr: RawByteString;
    const Codec: TCustomUnicodeCodec; const CodecOwner: Boolean);
begin
  inherited Create(TRawByteStringReader.Create(DataStr), True, Codec, CodecOwner);
end;
{$ENDIF}



{                                                                              }
{ TUnicodeFileReader                                                           }
{                                                                              }
constructor TUnicodeFileReader.Create(const FileName: String;
    const Codec: TCustomUnicodeCodec; const CodecOwner: Boolean);
begin
  inherited Create(TFileReader.Create(FileName), True, Codec, CodecOwner);
end;



{                                                                              }
{ Test cases                                                                   }
{                                                                              }
{$IFDEF DEBUG}
{$IFDEF TEST}
{$ASSERTIONS ON}
procedure Test;
{$IFDEF SupportRawByteString}
var A : TUnicodeReader;
{$ENDIF}
begin
  {$IFDEF SupportRawByteString}
  A := TUnicodeReader.Create(
      TRawByteStringReader.Create(RawByteString(#$41#$E2#$89#$A2#$CE#$91#$2E)),
      True,
      TUTF8Codec.Create,
      True);
  Assert(not A.EOF, 'UnicodeReader.EOF');
  Assert(A.PeekChar = #$0041, 'UnicodeReader.PeekChar');
  Assert(A.ReadChar = #$0041, 'UnicodeReader.ReadChar');
  Assert(A.ReadChar = #$2262, 'UnicodeReader.ReadChar');
  Assert(A.ReadChar = #$0391, 'UnicodeReader.ReadChar');
  Assert(A.ReadChar = #$002E, 'UnicodeReader.ReadChar');
  Assert(A.EOF, 'UnicodeReader.EOF');
  A.Free;
  {$ENDIF}
end;
{$ENDIF}{$ENDIF}



end.

