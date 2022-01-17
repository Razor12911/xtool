{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals 5.00                                        }
{   File name:        flcASCII.pas                                             }
{   File version:     5.04                                                     }
{   Description:      Ascii utility functions.                                 }
{                                                                              }
{   Copyright:        Copyright (c) 2000-2020, David J Butler                  }
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
{   2000-2017   1.01  Initial versions.                                        }
{   2018/07/11  5.02  Move to flcASCII unit from flcUtils.                     }
{   2018/08/12  5.03  String type changes.                                     }
{   2020/03/21  5.04  NativeInt changes.                                       }
{                                                                              }
{ Supported compilers:                                                         }
{                                                                              }
{   Delphi 2010-10.4 Win32/Win64        5.04  2020/06/02                       }
{   Delphi 10.2-10.4 Linux64            5.04  2020/06/02                       }
{   FreePascal 3.0.4 Win64              5.04  2020/06/02                       }
{                                                                              }
{******************************************************************************}

{$INCLUDE ..\flcInclude.inc}

{$IFDEF FREEPASCAL}
  {$WARNINGS OFF}
  {$HINTS OFF}
{$ENDIF}

{$IFDEF DEBUG}
{$IFDEF TEST}
  {$DEFINE ASCII_TEST}
{$ENDIF}
{$ENDIF}

unit flcASCII;

interface

uses
  { Fundamentals }
  flcStdTypes;



{                                                                              }
{ ASCII constants                                                              }
{                                                                              }
const
  AsciiNULL = ByteChar(#0);
  AsciiSOH  = ByteChar(#1);
  AsciiSTX  = ByteChar(#2);
  AsciiETX  = ByteChar(#3);
  AsciiEOT  = ByteChar(#4);
  AsciiENQ  = ByteChar(#5);
  AsciiACK  = ByteChar(#6);
  AsciiBEL  = ByteChar(#7);
  AsciiBS   = ByteChar(#8);
  AsciiHT   = ByteChar(#9);
  AsciiLF   = ByteChar(#10);
  AsciiVT   = ByteChar(#11);
  AsciiFF   = ByteChar(#12);
  AsciiCR   = ByteChar(#13);
  AsciiSO   = ByteChar(#14);
  AsciiSI   = ByteChar(#15);
  AsciiDLE  = ByteChar(#16);
  AsciiDC1  = ByteChar(#17);
  AsciiDC2  = ByteChar(#18);
  AsciiDC3  = ByteChar(#19);
  AsciiDC4  = ByteChar(#20);
  AsciiNAK  = ByteChar(#21);
  AsciiSYN  = ByteChar(#22);
  AsciiETB  = ByteChar(#23);
  AsciiCAN  = ByteChar(#24);
  AsciiEM   = ByteChar(#25);
  AsciiEOF  = ByteChar(#26);
  AsciiESC  = ByteChar(#27);
  AsciiFS   = ByteChar(#28);
  AsciiGS   = ByteChar(#29);
  AsciiRS   = ByteChar(#30);
  AsciiUS   = ByteChar(#31);
  AsciiSP   = ByteChar(#32);
  AsciiDEL  = ByteChar(#127);
  AsciiXON  = AsciiDC1;
  AsciiXOFF = AsciiDC3;

  AsciiCRLF = AsciiCR + AsciiLF;

  AsciiDecimalPoint = ByteChar(#46);
  AsciiComma        = ByteChar(#44);
  AsciiBackSlash    = ByteChar(#92);
  AsciiForwardSlash = ByteChar(#47);
  AsciiPercent      = ByteChar(#37);
  AsciiAmpersand    = ByteChar(#38);
  AsciiPlus         = ByteChar(#43);
  AsciiMinus        = ByteChar(#45);
  AsciiEqualSign    = ByteChar(#61);
  AsciiSingleQuote  = ByteChar(#39);
  AsciiDoubleQuote  = ByteChar(#34);

  AsciiDigit0 = ByteChar(#48);
  AsciiDigit9 = ByteChar(#57);
  AsciiUpperA = ByteChar(#65);
  AsciiUpperZ = ByteChar(#90);
  AsciiLowerA = ByteChar(#97);
  AsciiLowerZ = ByteChar(#122);



{                                                                              }
{ WideChar constants                                                           }
{                                                                              }
const
  WideNULL = WideChar(#0);
  WideSOH  = WideChar(#1);
  WideSTX  = WideChar(#2);
  WideETX  = WideChar(#3);
  WideEOT  = WideChar(#4);
  WideENQ  = WideChar(#5);
  WideACK  = WideChar(#6);
  WideBEL  = WideChar(#7);
  WideBS   = WideChar(#8);
  WideHT   = WideChar(#9);
  WideLF   = WideChar(#10);
  WideVT   = WideChar(#11);
  WideFF   = WideChar(#12);
  WideCR   = WideChar(#13);
  WideSO   = WideChar(#14);
  WideSI   = WideChar(#15);
  WideDLE  = WideChar(#16);
  WideDC1  = WideChar(#17);
  WideDC2  = WideChar(#18);
  WideDC3  = WideChar(#19);
  WideDC4  = WideChar(#20);
  WideNAK  = WideChar(#21);
  WideSYN  = WideChar(#22);
  WideETB  = WideChar(#23);
  WideCAN  = WideChar(#24);
  WideEM   = WideChar(#25);
  WideEOF  = WideChar(#26);
  WideESC  = WideChar(#27);
  WideFS   = WideChar(#28);
  WideGS   = WideChar(#29);
  WideRS   = WideChar(#30);
  WideUS   = WideChar(#31);
  WideSP   = WideChar(#32);
  WideDEL  = WideChar(#127);
  WideXON  = WideDC1;
  WideXOFF = WideDC3;

  {$IFDEF DELPHI5}
  WideCRLF = WideString(#13#10);
  {$ELSE}
  {$IFDEF DELPHI7_DOWN}
  WideCRLF = WideString(WideCR) + WideString(WideLF);
  {$ELSE}
  WideCRLF = WideCR + WideLF;
  {$ENDIF}
  {$ENDIF}



{                                                                              }
{ ASCII low case lookup                                                        }
{                                                                              }
const
  AsciiLowCaseLookup: array[Byte] of Byte = (
    $00, $01, $02, $03, $04, $05, $06, $07,
    $08, $09, $0A, $0B, $0C, $0D, $0E, $0F,
    $10, $11, $12, $13, $14, $15, $16, $17,
    $18, $19, $1A, $1B, $1C, $1D, $1E, $1F,
    $20, $21, $22, $23, $24, $25, $26, $27,
    $28, $29, $2A, $2B, $2C, $2D, $2E, $2F,
    $30, $31, $32, $33, $34, $35, $36, $37,
    $38, $39, $3A, $3B, $3C, $3D, $3E, $3F,
    $40, $61, $62, $63, $64, $65, $66, $67,
    $68, $69, $6A, $6B, $6C, $6D, $6E, $6F,
    $70, $71, $72, $73, $74, $75, $76, $77,
    $78, $79, $7A, $5B, $5C, $5D, $5E, $5F,
    $60, $61, $62, $63, $64, $65, $66, $67,
    $68, $69, $6A, $6B, $6C, $6D, $6E, $6F,
    $70, $71, $72, $73, $74, $75, $76, $77,
    $78, $79, $7A, $7B, $7C, $7D, $7E, $7F,
    $80, $81, $82, $83, $84, $85, $86, $87,
    $88, $89, $8A, $8B, $8C, $8D, $8E, $8F,
    $90, $91, $92, $93, $94, $95, $96, $97,
    $98, $99, $9A, $9B, $9C, $9D, $9E, $9F,
    $A0, $A1, $A2, $A3, $A4, $A5, $A6, $A7,
    $A8, $A9, $AA, $AB, $AC, $AD, $AE, $AF,
    $B0, $B1, $B2, $B3, $B4, $B5, $B6, $B7,
    $B8, $B9, $BA, $BB, $BC, $BD, $BE, $BF,
    $C0, $C1, $C2, $C3, $C4, $C5, $C6, $C7,
    $C8, $C9, $CA, $CB, $CC, $CD, $CE, $CF,
    $D0, $D1, $D2, $D3, $D4, $D5, $D6, $D7,
    $D8, $D9, $DA, $DB, $DC, $DD, $DE, $DF,
    $E0, $E1, $E2, $E3, $E4, $E5, $E6, $E7,
    $E8, $E9, $EA, $EB, $EC, $ED, $EE, $EF,
    $F0, $F1, $F2, $F3, $F4, $F5, $F6, $F7,
    $F8, $F9, $FA, $FB, $FC, $FD, $FE, $FF);



{                                                                              }
{ ASCII functions                                                              }
{                                                                              }
function  IsAsciiCharB(const C: ByteChar): Boolean; {$IFDEF UseInline}inline;{$ENDIF}
function  IsAsciiCharW(const C: WideChar): Boolean; {$IFDEF UseInline}inline;{$ENDIF}
function  IsAsciiChar(const C: Char): Boolean;      {$IFDEF UseInline}inline;{$ENDIF}

function  IsAsciiBufB(const Buf: Pointer; const Len: NativeInt): Boolean; {$IFDEF UseInline}inline;{$ENDIF}
function  IsAsciiBufW(const Buf: Pointer; const Len: NativeInt): Boolean; {$IFDEF UseInline}inline;{$ENDIF}

function  IsAsciiStringB(const S: RawByteString): Boolean; {$IFDEF UseInline}inline;{$ENDIF}
function  IsAsciiStringU(const S: UnicodeString): Boolean; {$IFDEF UseInline}inline;{$ENDIF}
function  IsAsciiString(const S: String): Boolean;         {$IFDEF UseInline}inline;{$ENDIF}

function  AsciiHexCharValueB(const C: ByteChar): Integer;
function  AsciiHexCharValueW(const C: WideChar): Integer;

function  AsciiIsHexCharB(const C: ByteChar): Boolean;
function  AsciiIsHexCharW(const C: WideChar): Boolean;

function  AsciiDecimalCharValueB(const C: ByteChar): Integer;
function  AsciiDecimalCharValueW(const C: WideChar): Integer;

function  AsciiIsDecimalCharB(const C: ByteChar): Boolean;
function  AsciiIsDecimalCharW(const C: WideChar): Boolean;

function  AsciiOctalCharValueB(const C: ByteChar): Integer;
function  AsciiOctalCharValueW(const C: WideChar): Integer;

function  AsciiIsOctalCharB(const C: ByteChar): Boolean;
function  AsciiIsOctalCharW(const C: WideChar): Boolean;



{                                                                              }
{ String compare functions                                                     }
{                                                                              }
{   Returns  -1  if A < B                                                      }
{             0  if A = B                                                      }
{             1  if A > B                                                      }
{                                                                              }
function  CharCompareNoAsciiCaseB(const A, B: ByteChar): Integer; {$IFDEF UseInline}inline;{$ENDIF}
function  CharCompareNoAsciiCaseW(const A, B: WideChar): Integer; {$IFDEF UseInline}inline;{$ENDIF}
function  CharCompareNoAsciiCase(const A, B: Char): Integer;      {$IFDEF UseInline}inline;{$ENDIF}

function  CharEqualNoAsciiCaseB(const A, B: ByteChar): Boolean;  {$IFDEF UseInline}inline;{$ENDIF}
function  CharEqualNoAsciiCaseW(const A, B: WideChar): Boolean;  {$IFDEF UseInline}inline;{$ENDIF}
function  CharEqualNoAsciiCase(const A, B: Char): Boolean;       {$IFDEF UseInline}inline;{$ENDIF}

function  StrPCompareNoAsciiCaseB(const A, B: PByteChar; const Len: NativeInt): Integer;
function  StrPCompareNoAsciiCaseW(const A, B: PWideChar; const Len: NativeInt): Integer;
function  StrPCompareNoAsciiCase(const A, B: PChar; const Len: NativeInt): Integer;

{$IFDEF SupportAnsiString}
function  StrCompareNoAsciiCaseA(const A, B: AnsiString): Integer;
{$ENDIF}
function  StrCompareNoAsciiCaseB(const A, B: RawByteString): Integer;
function  StrCompareNoAsciiCaseU(const A, B: UnicodeString): Integer;
function  StrCompareNoAsciiCase(const A, B: String): Integer;



{                                                                              }
{ ASCII case conversion                                                        }
{                                                                              }
function  AsciiLowCaseB(const C: ByteChar): ByteChar;
function  AsciiLowCaseW(const C: WideChar): WideChar; {$IFDEF UseInline}inline;{$ENDIF}
function  AsciiLowCase(const C: Char): Char;          {$IFDEF UseInline}inline;{$ENDIF}

function  AsciiUpCaseB(const C: ByteChar): ByteChar;
function  AsciiUpCaseW(const C: WideChar): WideChar; {$IFDEF UseInline}inline;{$ENDIF}
function  AsciiUpCase(const C: Char): Char; {$IFDEF UseInline}inline;{$ENDIF}

procedure AsciiConvertUpperB(var S: RawByteString);
procedure AsciiConvertUpperU(var S: UnicodeString);
procedure AsciiConvertUpper(var S: String);

procedure AsciiConvertLowerB(var S: RawByteString);
procedure AsciiConvertLowerU(var S: UnicodeString);
procedure AsciiConvertLower(var S: String);

function  AsciiUpperCaseB(const A: RawByteString): RawByteString;
function  AsciiUpperCaseU(const A: UnicodeString): UnicodeString;
function  AsciiUpperCase(const A: String): String;

function  AsciiLowerCaseB(const A: RawByteString): RawByteString;
function  AsciiLowerCaseU(const A: UnicodeString): UnicodeString;
function  AsciiLowerCase(const A: String): String;

procedure AsciiConvertFirstUpB(var S: RawByteString);
procedure AsciiConvertFirstUpU(var S: UnicodeString);
procedure AsciiConvertFirstUp(var S: String);

function  AsciiFirstUpB(const S: RawByteString): RawByteString;
function  AsciiFirstUpU(const S: UnicodeString): UnicodeString;
function  AsciiFirstUp(const S: String): String;

procedure AsciiConvertArrayUpper(var S: RawByteStringArray);
procedure AsciiConvertArrayLower(var S: RawByteStringArray);



{                                                                              }
{ Tests                                                                        }
{                                                                              }
{$IFDEF ASCII_TEST}
procedure Test;
{$ENDIF}



implementation




{                                                                              }
{ US-ASCII String functions                                                    }
{                                                                              }
function IsAsciiCharB(const C: ByteChar): Boolean;
begin
  Result := C in [ByteChar(0)..ByteChar(127)];
end;

function IsAsciiCharW(const C: WideChar): Boolean;
begin
  Result := Ord(C) <= 127;
end;

function IsAsciiChar(const C: Char): Boolean;
begin
  Result := Ord(C) <= 127;
end;

function IsAsciiBufB(const Buf: Pointer; const Len: NativeInt): Boolean;
var
  I : NativeInt;
  P : PByte;
begin
  P := Buf;
  for I := 1 to Len do
    if P^ >= $80 then
      begin
        Result := False;
        exit;
      end
    else
      Inc(P);
  Result := True;
end;

function IsAsciiBufW(const Buf: Pointer; const Len: NativeInt): Boolean;
var
  I : NativeInt;
  P : PWord16;
begin
  P := Buf;
  for I := 1 to Len do
    if P^ >= $80 then
      begin
        Result := False;
        exit;
      end
    else
      Inc(P);
  Result := True;
end;

function IsAsciiStringB(const S: RawByteString): Boolean;
begin
  Result := IsAsciiBufB(Pointer(S), Length(S));
end;

function IsAsciiStringU(const S: UnicodeString): Boolean;
begin
  Result := IsAsciiBufW(PWideChar(S), Length(S));
end;

function IsAsciiString(const S: String): Boolean;
begin
  {$IFDEF StringIsUnicode}
  Result := IsAsciiStringU(S);
  {$ELSE}
  Result := IsAsciiStringB(S);
  {$ENDIF}
end;



{                                                                              }
{ ASCII functions                                                              }
{                                                                              }
function AsciiHexCharValueB(const C: ByteChar): Integer;
begin
  case Ord(C) of
    Ord('0')..Ord('9') : Result := Ord(C) - Ord('0');
    Ord('A')..Ord('F') : Result := Ord(C) - Ord('A') + 10;
    Ord('a')..Ord('f') : Result := Ord(C) - Ord('a') + 10;
  else
    Result := -1;
  end;
end;

function AsciiHexCharValueW(const C: WideChar): Integer;
begin
  if Ord(C) >= $80 then
    Result := -1
  else
    Result := AsciiHexCharValueB(ByteChar(Ord(C)));
end;

function AsciiIsHexCharB(const C: ByteChar): Boolean;
begin
  Result := AsciiHexCharValueB(C) >= 0;
end;

function AsciiIsHexCharW(const C: WideChar): Boolean;
begin
  Result := AsciiHexCharValueW(C) >= 0;
end;

function AsciiDecimalCharValueB(const C: ByteChar): Integer;
begin
  case Ord(C) of
    Ord('0')..Ord('9') : Result := Ord(C) - Ord('0');
  else
    Result := -1;
  end;
end;

function AsciiDecimalCharValueW(const C: WideChar): Integer;
begin
  if Ord(C) >= $80 then
    Result := -1
  else
    Result := AsciiDecimalCharValueB(ByteChar(Ord(C)));
end;

function AsciiIsDecimalCharB(const C: ByteChar): Boolean;
begin
  Result := AsciiDecimalCharValueB(C) >= 0;
end;

function AsciiIsDecimalCharW(const C: WideChar): Boolean;
begin
  Result := AsciiDecimalCharValueW(C) >= 0;
end;

function AsciiOctalCharValueB(const C: ByteChar): Integer;
begin
  case Ord(C) of
    Ord('0')..Ord('7') : Result := Ord(C) - Ord('0');
  else
    Result := -1;
  end;
end;

function AsciiOctalCharValueW(const C: WideChar): Integer;
begin
  if Ord(C) >= $80 then
    Result := -1
  else
    Result := AsciiOctalCharValueB(ByteChar(Ord(C)));
end;

function AsciiIsOctalCharB(const C: ByteChar): Boolean;
begin
  Result := AsciiOctalCharValueB(C) >= 0;
end;

function AsciiIsOctalCharW(const C: WideChar): Boolean;
begin
  Result := AsciiOctalCharValueW(C) >= 0;
end;



{                                                                              }
{ Compare                                                                      }
{                                                                              }
function CharCompareNoAsciiCaseB(const A, B: ByteChar): Integer;
var C, D : ByteChar;
begin
  C := ByteChar(AsciiLowCaseLookup[Ord(A)]);
  D := ByteChar(AsciiLowCaseLookup[Ord(B)]);
  if C < D then
    Result := -1 else
    if C > D then
      Result := 1
    else
      Result := 0;
end;

function CharCompareNoAsciiCaseW(const A, B: WideChar): Integer;
var C, D : WideChar;
begin
  C := AsciiUpCaseW(A);
  D := AsciiUpCaseW(B);
  if Ord(C) < Ord(D) then
    Result := -1 else
    if Ord(C) > Ord(D) then
      Result := 1
    else
      Result := 0;
end;

function CharCompareNoAsciiCase(const A, B: Char): Integer;
var C, D : Char;
begin
  C := AsciiUpCase(A);
  D := AsciiUpCase(B);
  if Ord(C) < Ord(D) then
    Result := -1 else
    if Ord(C) > Ord(D) then
      Result := 1
    else
      Result := 0;
end;

function CharEqualNoAsciiCaseB(const A, B: ByteChar): Boolean;
begin
  Result := AsciiUpCaseB(A) = AsciiUpCaseB(B);
end;

function CharEqualNoAsciiCaseW(const A, B: WideChar): Boolean;
begin
  Result := AsciiUpCaseW(A) = AsciiUpCaseW(B);
end;

function CharEqualNoAsciiCase(const A, B: Char): Boolean;
begin
  Result := AsciiUpCase(A) = AsciiUpCase(B);
end;

function StrPCompareNoAsciiCaseB(const A, B: PByteChar; const Len: NativeInt): Integer;
var
  P, Q : PByte;
  C, D : Byte;
  I    : NativeInt;
begin
  P := Pointer(A);
  Q := Pointer(B);
  if P <> Q then
    for I := 1 to Len do
      begin
        C := AsciiLowCaseLookup[P^];
        D := AsciiLowCaseLookup[Q^];
        if C = D then
          begin
            Inc(P);
            Inc(Q);
          end
        else
          begin
            if C < D then
              Result := -1
            else
              Result := 1;
            exit;
          end;
      end;
  Result := 0;
end;

function StrPCompareNoAsciiCaseW(const A, B: PWideChar; const Len: NativeInt): Integer;
var
  P, Q : PWideChar;
  C, D : Word16;
  I    : NativeInt;
begin
  P := A;
  Q := B;
  if P <> Q then
    for I := 1 to Len do
      begin
        C := Ord(P^);
        D := Ord(Q^);
        if C <= $7F then
          C := AsciiLowCaseLookup[Byte(C)];
        if D <= $7F then
          D := AsciiLowCaseLookup[Byte(D)];
        if C = D then
          begin
            Inc(P);
            Inc(Q);
          end
        else
          begin
            if C < D then
              Result := -1
            else
              Result := 1;
            exit;
          end;
      end;
  Result := 0;
end;

function StrPCompareNoAsciiCase(const A, B: PChar; const Len: NativeInt): Integer;
var
  P, Q : PChar;
  C, D : Integer;
  I    : NativeInt;
begin
  P := A;
  Q := B;
  if P <> Q then
    for I := 1 to Len do
      begin
        C := Ord(P^);
        D := Ord(Q^);
        if C <= $7F then
          C := Integer(AsciiLowCaseLookup[Byte(C)]);
        if D <= $7F then
          D := Integer(AsciiLowCaseLookup[Byte(D)]);
        if C = D then
          begin
            Inc(P);
            Inc(Q);
          end
        else
          begin
            if C < D then
              Result := -1
            else
              Result := 1;
            exit;
          end;
      end;
  Result := 0;
end;

{$IFDEF SupportAnsiString}
function StrCompareNoAsciiCaseA(const A, B: AnsiString): Integer;
var
  L, M, I: NativeInt;
begin
  L := Length(A);
  M := Length(B);
  if L < M then
    I := L
  else
    I := M;
  Result := StrPCompareNoAsciiCaseB(PByteChar(A), PByteChar(B), I);
  if Result <> 0 then
    exit;
  if L = M then
    Result := 0 else
  if L < M then
    Result := -1
  else
    Result := 1;
end;
{$ENDIF}

function StrCompareNoAsciiCaseB(const A, B: RawByteString): Integer;
var
  L, M, I: NativeInt;
begin
  L := Length(A);
  M := Length(B);
  if L < M then
    I := L
  else
    I := M;
  Result := StrPCompareNoAsciiCaseB(PByteChar(A), PByteChar(B), I);
  if Result <> 0 then
    exit;
  if L = M then
    Result := 0 else
  if L < M then
    Result := -1
  else
    Result := 1;
end;

function StrCompareNoAsciiCaseU(const A, B: UnicodeString): Integer;
var
  L, M, I: NativeInt;
begin
  L := Length(A);
  M := Length(B);
  if L < M then
    I := L
  else
    I := M;
  Result := StrPCompareNoAsciiCaseW(Pointer(A), Pointer(B), I);
  if Result <> 0 then
    exit;
  if L = M then
    Result := 0 else
  if L < M then
    Result := -1
  else
    Result := 1;
end;

function StrCompareNoAsciiCase(const A, B: String): Integer;
var
  L, M, I: Integer;
begin
  L := Length(A);
  M := Length(B);
  if L < M then
    I := L
  else
    I := M;
  Result := StrPCompareNoAsciiCase(Pointer(A), Pointer(B), I);
  if Result <> 0 then
    exit;
  if L = M then
    Result := 0 else
  if L < M then
    Result := -1
  else
    Result := 1;
end;



{                                                                              }
{ ASCII case conversion                                                        }
{                                                                              }
const
  AsciiCaseDiff = Byte(AsciiLowerA) - Byte(AsciiUpperA);

{$IFDEF ASM386_DELPHI}
function AsciiLowCaseB(const C: ByteChar): ByteChar; register; assembler;
asm
      CMP     AL, AsciiUpperA
      JB      @@exit
      CMP     AL, AsciiUpperZ
      JA      @@exit
      ADD     AL, AsciiCaseDiff
@@exit:
end;
{$ELSE}
function AsciiLowCaseB(const C: ByteChar): ByteChar; {$IFDEF UseInline}inline;{$ENDIF}
begin
  if C in [AsciiUpperA..AsciiUpperZ] then
    Result := ByteChar(Byte(C) + AsciiCaseDiff)
  else
    Result := C;
end;
{$ENDIF}

function AsciiLowCaseW(const C: WideChar): WideChar;
begin
  case Ord(C) of
    Ord(AsciiUpperA)..Ord(AsciiUpperZ) : Result := WideChar(Ord(C) + AsciiCaseDiff)
  else
    Result := C;
  end;
end;

function AsciiLowCase(const C: Char): Char;
begin
  case Ord(C) of
    Ord(AsciiUpperA)..Ord(AsciiUpperZ) : Result := Char(Ord(C) + AsciiCaseDiff)
  else
    Result := C;
  end;
end;

{$IFDEF ASM386_DELPHI}
function AsciiUpCaseB(const C: ByteChar): ByteChar; register; assembler;
asm
      CMP     AL, AsciiLowerA
      JB      @@exit
      CMP     AL, AsciiLowerZ
      JA      @@exit
      SUB     AL, AsciiLowerA - AsciiUpperA
@@exit:
end;
{$ELSE}
function AsciiUpCaseB(const C: ByteChar): ByteChar; {$IFDEF UseInline}inline;{$ENDIF}
begin
  if C in [AsciiLowerA..AsciiLowerZ] then
    Result := ByteChar(Byte(C) - AsciiCaseDiff)
  else
    Result := C;
end;
{$ENDIF}

function AsciiUpCaseW(const C: WideChar): WideChar;
begin
  case Ord(C) of
    Ord(AsciiLowerA)..Ord(AsciiLowerZ) : Result := WideChar(Ord(C) - AsciiCaseDiff)
  else
    Result := C;
  end;
end;

function AsciiUpCase(const C: Char): Char;
begin
  case Ord(C) of
    Ord(AsciiLowerA)..Ord(AsciiLowerZ) : Result := Char(Ord(C) - AsciiCaseDiff)
  else
    Result := C;
  end;
end;

procedure AsciiConvertUpperB(var S: RawByteString);
var F : Integer;
    B : Byte;
begin
  for F := 1 to Length(S) do
    begin
      B := Ord(S[F]);
      if (B >= Ord(AsciiLowerA)) and (B <= Ord(AsciiLowerZ)) then
        S[F] := RawByteChar(B - AsciiCaseDiff);
    end;
end;

procedure AsciiConvertUpperU(var S: UnicodeString);
var
  F : NativeInt;
  C : WideChar;
begin
  for F := 1 to Length(S) do
    begin
      C := S[F];
      if Ord(C) <= $FF then
        if ByteChar(Ord(C)) in [AsciiLowerA..AsciiLowerZ] then
          S[F] := WideChar(Ord(C) - AsciiCaseDiff);
    end;
end;

procedure AsciiConvertUpper(var S: String);
var
  F : NativeInt;
  C : Char;
begin
  for F := 1 to Length(S) do
    begin
      C := S[F];
      {$IFDEF StringIsUnicode}
      if Ord(C) <= $FF then
      {$ENDIF}
        if ByteChar(Ord(C)) in [AsciiLowerA..AsciiLowerZ] then
          S[F] := Char(Ord(C) - AsciiCaseDiff);
    end;
end;

procedure AsciiConvertLowerB(var S: RawByteString);
var
  F : NativeInt;
begin
  for F := 1 to Length(S) do
    if S[F] in [AsciiUpperA..AsciiUpperZ] then
      S[F] := ByteChar(Ord(S[F]) + AsciiCaseDiff);
end;

procedure AsciiConvertLowerU(var S: UnicodeString);
var
  F : NativeInt;
  C : WideChar;
begin
  for F := 1 to Length(S) do
    begin
      C := S[F];
      if Ord(C) <= $FF then
        if ByteChar(Ord(C)) in [AsciiUpperA..AsciiUpperZ] then
          S[F] := WideChar(Ord(C) + AsciiCaseDiff);
    end;
end;

procedure AsciiConvertLower(var S: String);
var
  F : NativeInt;
  C : Char;
begin
  for F := 1 to Length(S) do
    begin
      C := S[F];
      {$IFDEF StringIsUnicode}
      if Ord(C) <= $FF then
      {$ENDIF}
        if ByteChar(Ord(C)) in [AsciiUpperA..AsciiUpperZ] then
          S[F] := Char(Ord(C) + AsciiCaseDiff);
    end;
end;

function AsciiUpperCaseB(const A: RawByteString): RawByteString;
begin
  Result := A;
  AsciiConvertUpperB(Result);
end;

function AsciiUpperCaseU(const A: UnicodeString): UnicodeString;
begin
  Result := A;
  AsciiConvertUpperU(Result);
end;

function AsciiUpperCase(const A: String): String;
begin
  Result := A;
  AsciiConvertUpper(Result);
end;

function AsciiLowerCaseB(const A: RawByteString): RawByteString;
begin
  Result := A;
  AsciiConvertLowerB(Result);
end;

function AsciiLowerCaseU(const A: UnicodeString): UnicodeString;
begin
  Result := A;
  AsciiConvertLowerU(Result);
end;

function AsciiLowerCase(const A: String): String;
begin
  Result := A;
  AsciiConvertLower(Result);
end;

procedure AsciiConvertFirstUpB(var S: RawByteString);
var
  C : ByteChar;
begin
  if S <> '' then
    begin
      C := S[1];
      if C in [AsciiLowerA..AsciiLowerZ] then
        S[1] := AsciiUpCaseB(C);
    end;
end;

procedure AsciiConvertFirstUpU(var S: UnicodeString);
var
  C : WideChar;
begin
  if S <> '' then
    begin
      C := S[1];
      if (Ord(C) >= Ord(AsciiLowerA)) and (Ord(C) <= Ord(AsciiLowerZ)) then
        S[1] := AsciiUpCaseW(C);
    end;
end;

procedure AsciiConvertFirstUp(var S: String);
var
  C : Char;
begin
  if S <> '' then
    begin
      C := S[1];
      if (Ord(C) >= Ord(AsciiLowerA)) and (Ord(C) <= Ord(AsciiLowerZ)) then
        S[1] := AsciiUpCase(C);
    end;
end;

function AsciiFirstUpB(const S: RawByteString): RawByteString;
begin
  Result := S;
  AsciiConvertFirstUpB(Result);
end;

function AsciiFirstUpU(const S: UnicodeString): UnicodeString;
begin
  Result := S;
  AsciiConvertFirstUpU(Result);
end;

function AsciiFirstUp(const S: String): String;
begin
  Result := S;
  AsciiConvertFirstUp(Result);
end;

procedure AsciiConvertArrayUpper(var S: RawByteStringArray);
var I : Integer;
begin
  for I := 0 to Length(S) - 1 do
    AsciiConvertUpperB(S[I]);
end;

procedure AsciiConvertArrayLower(var S: RawByteStringArray);
var I : Integer;
begin
  for I := 0 to Length(S) - 1 do
    AsciiConvertLowerB(S[I]);
end;



{$IFDEF ASCII_TEST}
{$ASSERTIONS ON}
procedure Test;
var
  W : WideChar;
  S : RawByteString;
  D : UnicodeString;
  Y : String;
  C : ByteChar;
const
  TestStrA1 : RawByteString = '012XYZabc{}_ ';
  TestStrA2 : RawByteString = #$80;
  TestStrA3 : RawByteString = '';
const
  TestStrU1 : UnicodeString = '012XYZabc{}_ ';
begin
  Assert(IsAsciiStringB(TestStrA1), 'IsAsciiString');
  Assert(not IsAsciiStringB(TestStrA2), 'IsAsciiString');
  Assert(IsAsciiStringB(TestStrA3), 'IsAsciiString');

  Assert(IsAsciiStringU(TestStrU1), 'IsAsciiString');
  W := WideChar(#$0080);
  Assert(not IsAsciiStringU(W), 'IsAsciiString');
  W := WideChar($2262);
  Assert(not IsAsciiStringU(W), 'IsAsciiString');
  Assert(IsAsciiStringU(''), 'IsAsciiString');

  Assert(IsAsciiCharB(ByteChar(32)));
  Assert(IsAsciiCharW(#32));
  Assert(IsAsciiChar(#32));

  Assert(not IsAsciiCharB(ByteChar(128)));
  Assert(not IsAsciiCharW(#128));
  Assert(not IsAsciiChar(#128));

  {$IFDEF SupportAnsiChar}
  Assert(AsciiLowCaseB('A') = 'a');
  {$ENDIF}
  Assert(AsciiLowCaseW('A') = 'a');
  Assert(AsciiLowCase('A') = 'a');

  {$IFDEF SupportAnsiChar}
  Assert(AsciiLowCaseB('a') = 'a');
  {$ENDIF}
  Assert(AsciiLowCaseW('a') = 'a');
  Assert(AsciiLowCase('a') = 'a');

  {$IFDEF SupportAnsiChar}
  Assert(AsciiUpCaseB('A') = 'A');
  {$ENDIF}
  Assert(AsciiUpCaseW('A') = 'A');
  Assert(AsciiUpCase('A') = 'A');

  {$IFDEF SupportAnsiChar}
  Assert(AsciiUpCaseB('a') = 'A');
  {$ENDIF}
  Assert(AsciiUpCaseW('a') = 'A');
  Assert(AsciiUpCase('a') = 'A');

  {$IFDEF SupportAnsiChar}
  S := '012AbcdEF';
  Assert(AsciiUpperCaseB(S) = '012ABCDEF');
  AsciiConvertUpperB(S);
  Assert(S = '012ABCDEF');
  {$ENDIF}

  D := '012AbcdEF';
  Assert(AsciiUpperCaseU(D) = '012ABCDEF');
  AsciiConvertUpperU(D);
  Assert(D = '012ABCDEF');

  Y := '012AbcdEF';
  Assert(AsciiUpperCase(Y) = '012ABCDEF');
  AsciiConvertUpper(Y);
  Assert(Y = '012ABCDEF');

  {$IFDEF SupportAnsiChar}
  S := '012AbcdEF';
  Assert(AsciiLowerCaseB(S) ='012abcdef');
  AsciiConvertLowerB(S);
  Assert(S = '012abcdef');
  {$ENDIF}

  D := '012AbcdEF';
  Assert(AsciiLowerCaseU(D) ='012abcdef');
  AsciiConvertLowerU(D);
  Assert(D = '012abcdef');

  Y := '012AbcdEF';
  Assert(AsciiLowerCase(Y) ='012abcdef');
  AsciiConvertLower(Y);
  Assert(Y = '012abcdef');

  {$IFDEF SupportAnsiChar}
  Assert(AsciiLowCaseB('A') = 'a', 'LowCase');
  {$ENDIF}
  Assert(UpCase('a') = 'A', 'UpCase');
  {$IFDEF SupportAnsiChar}
  Assert(AsciiLowCaseB('-') = '-', 'LowCase');
  {$ENDIF}

  Assert(UpCase('}') = '}', 'UpCase');
  {$IFDEF SupportAnsiChar}
  Assert(AsciiFirstUpB('abra') = 'Abra', 'FirstUp');
  Assert(AsciiFirstUpB('') = '', 'FirstUp');
  {$ENDIF}

  {$IFDEF DELPHI2007_DOWN}
  for C := #0 to #255 do
    Assert(AsciiLowCase(C) = LowerCase(C), 'LowCase = LowerCase');
  {$ENDIF}

  for C := AnsiChar('A') to AnsiChar('Z') do
    Assert(AsciiLowCaseB(C) <> C, 'LowCase');
  for C := AnsiChar('a') to AnsiChar('z') do
    Assert(AsciiLowCaseB(C) = C, 'LowCase');

  Assert(CharEqualNoAsciiCaseB(AnsiChar('a'), AnsiChar('a')));
  Assert(CharEqualNoAsciiCaseB(AnsiChar('B'), AnsiChar('b')));
  Assert(not CharEqualNoAsciiCaseB(AnsiChar('C'), AnsiChar('D')));

  {$IFDEF SupportAnsiChar}
  S := 'aBcDEfg-123';
  AsciiConvertUpperB(S);
  Assert(S = 'ABCDEFG-123', 'ConvertUpper');

  S := 'aBcDEfg-123';
  AsciiConvertLowerB(S);
  Assert(S = 'abcdefg-123', 'ConvertLower');

  S := '';
  AsciiConvertLowerB(S);
  Assert(S = '', 'ConvertLower');

  S := 'abc';
  AsciiConvertLowerB(S);
  Assert(S = 'abc', 'ConvertLower');
  {$ENDIF}

  {$IFDEF SupportAnsiChar}
  Assert(StrCompareNoAsciiCaseA('a', 'a') = 0, 'StrCompareNoCase');
  Assert(StrCompareNoAsciiCaseA('a', 'b') = -1, 'StrCompareNoCase');
  Assert(StrCompareNoAsciiCaseA('b', 'a') = 1, 'StrCompareNoCase');
  Assert(StrCompareNoAsciiCaseA('A', 'a') = 0, 'StrCompareNoCase');
  Assert(StrCompareNoAsciiCaseA('A', 'b') = -1, 'StrCompareNoCase');
  Assert(StrCompareNoAsciiCaseA('b', 'A') = 1, 'StrCompareNoCase');
  Assert(StrCompareNoAsciiCaseA('aa', 'a') = 1, 'StrCompareNoCase');
  Assert(StrCompareNoAsciiCaseA('a', 'aa') = -1, 'StrCompareNoCase');
  Assert(StrCompareNoAsciiCaseA('AA', 'b') = -1, 'StrCompareNoCase');
  Assert(StrCompareNoAsciiCaseA('B', 'aa') = 1, 'StrCompareNoCase');
  Assert(StrCompareNoAsciiCaseA('aa', 'Aa') = 0, 'StrCompareNoCase');

  Assert(StrCompareNoAsciiCaseB('a', 'a') = 0, 'StrCompareNoCase');
  Assert(StrCompareNoAsciiCaseB('a', 'b') = -1, 'StrCompareNoCase');
  Assert(StrCompareNoAsciiCaseB('b', 'a') = 1, 'StrCompareNoCase');
  Assert(StrCompareNoAsciiCaseB('A', 'a') = 0, 'StrCompareNoCase');
  Assert(StrCompareNoAsciiCaseB('A', 'b') = -1, 'StrCompareNoCase');
  Assert(StrCompareNoAsciiCaseB('b', 'A') = 1, 'StrCompareNoCase');
  Assert(StrCompareNoAsciiCaseB('aa', 'a') = 1, 'StrCompareNoCase');
  Assert(StrCompareNoAsciiCaseB('a', 'aa') = -1, 'StrCompareNoCase');
  Assert(StrCompareNoAsciiCaseB('AA', 'b') = -1, 'StrCompareNoCase');
  Assert(StrCompareNoAsciiCaseB('B', 'aa') = 1, 'StrCompareNoCase');
  Assert(StrCompareNoAsciiCaseB('aa', 'Aa') = 0, 'StrCompareNoCase');
  {$ENDIF}

  Assert(StrCompareNoAsciiCaseU('a', 'a') = 0, 'StrCompareNoCase');
  Assert(StrCompareNoAsciiCaseU('a', 'b') = -1, 'StrCompareNoCase');
  Assert(StrCompareNoAsciiCaseU('b', 'a') = 1, 'StrCompareNoCase');
  Assert(StrCompareNoAsciiCaseU('A', 'a') = 0, 'StrCompareNoCase');
  Assert(StrCompareNoAsciiCaseU('A', 'b') = -1, 'StrCompareNoCase');
  Assert(StrCompareNoAsciiCaseU('b', 'A') = 1, 'StrCompareNoCase');
  Assert(StrCompareNoAsciiCaseU('aa', 'a') = 1, 'StrCompareNoCase');
  Assert(StrCompareNoAsciiCaseU('a', 'aa') = -1, 'StrCompareNoCase');
  Assert(StrCompareNoAsciiCaseU('AA', 'b') = -1, 'StrCompareNoCase');
  Assert(StrCompareNoAsciiCaseU('B', 'aa') = 1, 'StrCompareNoCase');
  Assert(StrCompareNoAsciiCaseU('aa', 'Aa') = 0, 'StrCompareNoCase');

  Assert(StrCompareNoAsciiCase('a', 'a') = 0, 'StrCompareNoCase');
  Assert(StrCompareNoAsciiCase('a', 'b') = -1, 'StrCompareNoCase');
  Assert(StrCompareNoAsciiCase('b', 'a') = 1, 'StrCompareNoCase');
  Assert(StrCompareNoAsciiCase('A', 'a') = 0, 'StrCompareNoCase');
  Assert(StrCompareNoAsciiCase('A', 'b') = -1, 'StrCompareNoCase');
  Assert(StrCompareNoAsciiCase('b', 'A') = 1, 'StrCompareNoCase');
  Assert(StrCompareNoAsciiCase('aa', 'a') = 1, 'StrCompareNoCase');
  Assert(StrCompareNoAsciiCase('a', 'aa') = -1, 'StrCompareNoCase');
  Assert(StrCompareNoAsciiCase('AA', 'b') = -1, 'StrCompareNoCase');
  Assert(StrCompareNoAsciiCase('B', 'aa') = 1, 'StrCompareNoCase');
  Assert(StrCompareNoAsciiCase('aa', 'Aa') = 0, 'StrCompareNoCase');
end;
{$ENDIF}



end.

