{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals 5.00                                        }
{   File name:        flcCharSet.pas                                           }
{   File version:     5.04                                                     }
{   Description:      Character/Byte set functions.                            }
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
{   2000/02/02  0.01  Initial version.                                         }
{   2017/10/07  5.02  Moved functions from unit flcUtils.                      }
{   2018/08/11  5.03  Moved functions from unit flcStrings.                    }
{   2018/08/14  5.04  ByteChar changes.                                        }
{                                                                              }
{ Supported compilers:                                                         }
{                                                                              }
{   Delphi 10 Win32                     5.02  2016/01/09                       }
{                                                                              }
{******************************************************************************}

{$INCLUDE ..\flcInclude.inc}

{$IFDEF FREEPASCAL}
  {$WARNINGS OFF}
  {$HINTS OFF}
{$ENDIF}

{$IFDEF DEBUG}
{$IFDEF TEST}
  {$DEFINE CHARSET_TEST}
{$ENDIF}
{$ENDIF}

unit flcCharSet;

interface

uses
  { Fundamentals }
  flcStdTypes;



{                                                                              }
{ Sets                                                                         }
{   Operations on byte and character sets.                                     }
{                                                                              }
const
  CompleteByteCharSet = [ByteChar(#0)..ByteChar(#255)];
  CompleteByteSet = [0..255];

function  AsAnsiCharSet(const C: array of ByteChar): ByteCharSet;
function  AsByteSet(const C: array of Byte): ByteSet;
function  AsByteCharSet(const C: array of ByteChar): ByteCharSet;
procedure ComplementChar(var C: ByteCharSet; const Ch: ByteChar);
procedure ClearCharSet(var C: ByteCharSet);
procedure FillCharSet(var C: ByteCharSet);
procedure ComplementCharSet(var C: ByteCharSet);
procedure AssignCharSet(var DestSet: ByteCharSet; const SourceSet: ByteCharSet); overload;
procedure Union(var DestSet: ByteCharSet; const SourceSet: ByteCharSet); overload;
procedure Difference(var DestSet: ByteCharSet; const SourceSet: ByteCharSet); overload;
procedure Intersection(var DestSet: ByteCharSet; const SourceSet: ByteCharSet); overload;
procedure XORCharSet(var DestSet: ByteCharSet; const SourceSet: ByteCharSet);
function  IsSubSet(const A, B: ByteCharSet): Boolean;
function  IsEqual(const A, B: ByteCharSet): Boolean; overload;
function  IsEmpty(const C: ByteCharSet): Boolean;
function  IsComplete(const C: ByteCharSet): Boolean;
function  CharCount(const C: ByteCharSet): Integer; overload;
procedure ConvertCaseInsensitive(var C: ByteCharSet);
function  CaseInsensitiveCharSet(const C: ByteCharSet): ByteCharSet;
function  CharSetToStrB(const C: ByteCharSet): RawByteString;
function  StrToCharSetB(const S: RawByteString): ByteCharSet;



{                                                                              }
{ Character class strings                                                      }
{                                                                              }
{   Perl-like character class string representation of character sets, eg      }
{   the set ['0', 'A'..'Z'] is presented as [0A-Z]. Negated classes are also   }
{   supported, eg '[^A-Za-z]' is all non-alpha characters. The empty and       }
{   complete sets have special representations; '[]' and '.' respectively.     }
{                                                                              }
{$IFDEF SupportAnsiString}
function  CharSetToCharClassStr(const C: ByteCharSet): AnsiString;
{$ENDIF}
// function  CharClassStrToCharSet(const S: AnsiString): CharSet;



{                                                                              }
{ Tests                                                                        }
{                                                                              }
{$IFDEF CHARSET_TEST}
procedure Test;
{$ENDIF}



implementation

uses
  { Fundamentals }
  flcASCII,
  flcUtils;



{                                                                              }
{ Sets                                                                         }
{                                                                              }
function AsAnsiCharSet(const C: array of ByteChar): ByteCharSet;
var I: Integer;
begin
  Result := [];
  for I := 0 to High(C) do
    Include(Result, C[I]);
end;

function AsByteSet(const C: array of Byte): ByteSet;
var I: Integer;
begin
  Result := [];
  for I := 0 to High(C) do
    Include(Result, C[I]);
end;

function AsByteCharSet(const C: array of ByteChar): ByteCharSet;
var I: Integer;
begin
  Result := [];
  for I := 0 to High(C) do
    Include(Result, C[I]);
end;

{$IFDEF ASM386_DELPHI}
procedure ComplementChar(var C: ByteCharSet; const Ch: ByteChar);
asm
      MOVZX   ECX, DL
      BTC     [EAX], ECX
end;
{$ELSE}
procedure ComplementChar(var C: ByteCharSet; const Ch: ByteChar);
begin
  if Ch in C then
    Exclude(C, Ch)
  else
    Include(C, Ch);
end;
{$ENDIF}

{$IFDEF ASM386_DELPHI}
procedure ClearCharSet(var C: ByteCharSet);
asm
      XOR     EDX, EDX
      MOV     [EAX], EDX
      MOV     [EAX + 4], EDX
      MOV     [EAX + 8], EDX
      MOV     [EAX + 12], EDX
      MOV     [EAX + 16], EDX
      MOV     [EAX + 20], EDX
      MOV     [EAX + 24], EDX
      MOV     [EAX + 28], EDX
end;
{$ELSE}
procedure ClearCharSet(var C: ByteCharSet);
begin
  C := [];
end;
{$ENDIF}

{$IFDEF ASM386_DELPHI}
procedure FillCharSet(var C: ByteCharSet);
asm
      MOV     EDX, $FFFFFFFF
      MOV     [EAX], EDX
      MOV     [EAX + 4], EDX
      MOV     [EAX + 8], EDX
      MOV     [EAX + 12], EDX
      MOV     [EAX + 16], EDX
      MOV     [EAX + 20], EDX
      MOV     [EAX + 24], EDX
      MOV     [EAX + 28], EDX
end;
{$ELSE}
procedure FillCharSet(var C: ByteCharSet);
begin
  C := [ByteChar(0)..ByteChar(255)];
end;
{$ENDIF}

{$IFDEF ASM386_DELPHI}
procedure ComplementCharSet(var C: ByteCharSet);
asm
      NOT     DWORD PTR [EAX]
      NOT     DWORD PTR [EAX + 4]
      NOT     DWORD PTR [EAX + 8]
      NOT     DWORD PTR [EAX + 12]
      NOT     DWORD PTR [EAX + 16]
      NOT     DWORD PTR [EAX + 20]
      NOT     DWORD PTR [EAX + 24]
      NOT     DWORD PTR [EAX + 28]
end;
{$ELSE}
procedure ComplementCharSet(var C: ByteCharSet);
begin
  C := [ByteChar(0)..ByteChar(255)] - C;
end;
{$ENDIF}

{$IFDEF ASM386_DELPHI}
procedure AssignCharSet(var DestSet: ByteCharSet; const SourceSet: ByteCharSet);
asm
      MOV     ECX, [EDX]
      MOV     [EAX], ECX
      MOV     ECX, [EDX + 4]
      MOV     [EAX + 4], ECX
      MOV     ECX, [EDX + 8]
      MOV     [EAX + 8], ECX
      MOV     ECX, [EDX + 12]
      MOV     [EAX + 12], ECX
      MOV     ECX, [EDX + 16]
      MOV     [EAX + 16], ECX
      MOV     ECX, [EDX + 20]
      MOV     [EAX + 20], ECX
      MOV     ECX, [EDX + 24]
      MOV     [EAX + 24], ECX
      MOV     ECX, [EDX + 28]
      MOV     [EAX + 28], ECX
end;
{$ELSE}
procedure AssignCharSet(var DestSet: ByteCharSet; const SourceSet: ByteCharSet);
begin
  DestSet := SourceSet;
end;
{$ENDIF}

{$IFDEF ASM386_DELPHI}
procedure Union(var DestSet: ByteCharSet; const SourceSet: ByteCharSet);
asm
      MOV     ECX, [EDX]
      OR      [EAX], ECX
      MOV     ECX, [EDX + 4]
      OR      [EAX + 4], ECX
      MOV     ECX, [EDX + 8]
      OR      [EAX + 8], ECX
      MOV     ECX, [EDX + 12]
      OR      [EAX + 12], ECX
      MOV     ECX, [EDX + 16]
      OR      [EAX + 16], ECX
      MOV     ECX, [EDX + 20]
      OR      [EAX + 20], ECX
      MOV     ECX, [EDX + 24]
      OR      [EAX + 24], ECX
      MOV     ECX, [EDX + 28]
      OR      [EAX + 28], ECX
end;
{$ELSE}
procedure Union(var DestSet: ByteCharSet; const SourceSet: ByteCharSet);
begin
  DestSet := DestSet + SourceSet;
end;
{$ENDIF}

{$IFDEF ASM386_DELPHI}
procedure Difference(var DestSet: ByteCharSet; const SourceSet: ByteCharSet);
asm
      MOV     ECX, [EDX]
      NOT     ECX
      AND     [EAX], ECX
      MOV     ECX, [EDX + 4]
      NOT     ECX
      AND     [EAX + 4], ECX
      MOV     ECX, [EDX + 8]
      NOT     ECX
      AND     [EAX + 8],ECX
      MOV     ECX, [EDX + 12]
      NOT     ECX
      AND     [EAX + 12], ECX
      MOV     ECX, [EDX + 16]
      NOT     ECX
      AND     [EAX + 16], ECX
      MOV     ECX, [EDX + 20]
      NOT     ECX
      AND     [EAX + 20], ECX
      MOV     ECX, [EDX + 24]
      NOT     ECX
      AND     [EAX + 24], ECX
      MOV     ECX, [EDX + 28]
      NOT     ECX
      AND     [EAX + 28], ECX
end;
{$ELSE}
procedure Difference(var DestSet: ByteCharSet; const SourceSet: ByteCharSet);
begin
  DestSet := DestSet - SourceSet;
end;
{$ENDIF}

{$IFDEF ASM386_DELPHI}
procedure Intersection(var DestSet: ByteCharSet; const SourceSet: ByteCharSet);
asm
      MOV     ECX, [EDX]
      AND     [EAX], ECX
      MOV     ECX, [EDX + 4]
      AND     [EAX + 4], ECX
      MOV     ECX, [EDX + 8]
      AND     [EAX + 8], ECX
      MOV     ECX, [EDX + 12]
      AND     [EAX + 12], ECX
      MOV     ECX, [EDX + 16]
      AND     [EAX + 16], ECX
      MOV     ECX, [EDX + 20]
      AND     [EAX + 20], ECX
      MOV     ECX, [EDX + 24]
      AND     [EAX + 24], ECX
      MOV     ECX, [EDX + 28]
      AND     [EAX + 28], ECX
end;
{$ELSE}
procedure Intersection(var DestSet: ByteCharSet; const SourceSet: ByteCharSet);
begin
  DestSet := DestSet * SourceSet;
end;
{$ENDIF}

{$IFDEF ASM386_DELPHI}
procedure XORCharSet(var DestSet: ByteCharSet; const SourceSet: ByteCharSet);
asm
      MOV     ECX, [EDX]
      XOR     [EAX], ECX
      MOV     ECX, [EDX + 4]
      XOR     [EAX + 4], ECX
      MOV     ECX, [EDX + 8]
      XOR     [EAX + 8], ECX
      MOV     ECX, [EDX + 12]
      XOR     [EAX + 12], ECX
      MOV     ECX, [EDX + 16]
      XOR     [EAX + 16], ECX
      MOV     ECX, [EDX + 20]
      XOR     [EAX + 20], ECX
      MOV     ECX, [EDX + 24]
      XOR     [EAX + 24], ECX
      MOV     ECX, [EDX + 28]
      XOR     [EAX + 28], ECX
end;
{$ELSE}
procedure XORCharSet(var DestSet: ByteCharSet; const SourceSet: ByteCharSet);
var Ch: ByteChar;
begin
  for Ch := ByteChar(0) to ByteChar(255) do
    if Ch in DestSet then
      begin
        if Ch in SourceSet then
          Exclude(DestSet, Ch);
      end else
      if Ch in SourceSet then
        Include(DestSet, Ch);
end;
{$ENDIF}

{$IFDEF ASM386_DELPHI}
function IsSubSet(const A, B: ByteCharSet): Boolean;
asm
      MOV     ECX, [EDX]
      NOT     ECX
      AND     ECX, [EAX]
      JNE     @Fin0
      MOV     ECX, [EDX + 4]
      NOT     ECX
      AND     ECX, [EAX + 4]
      JNE     @Fin0
      MOV     ECX, [EDX + 8]
      NOT     ECX
      AND     ECX, [EAX + 8]
      JNE     @Fin0
      MOV     ECX, [EDX + 12]
      NOT     ECX
      AND     ECX, [EAX + 12]
      JNE     @Fin0
      MOV     ECX, [EDX + 16]
      NOT     ECX
      AND     ECX, [EAX + 16]
      JNE     @Fin0
      MOV     ECX, [EDX + 20]
      NOT     ECX
      AND     ECX, [EAX + 20]
      JNE     @Fin0
      MOV     ECX, [EDX + 24]
      NOT     ECX
      AND     ECX, [EAX + 24]
      JNE     @Fin0
      MOV     ECX, [EDX + 28]
      NOT     ECX
      AND     ECX, [EAX + 28]
      JNE     @Fin0
      MOV     EAX, 1
      RET
@Fin0:
      XOR     EAX, EAX
end;
{$ELSE}
function IsSubSet(const A, B: ByteCharSet): Boolean;
begin
  Result := A <= B;
end;
{$ENDIF}

{$IFDEF ASM386_DELPHI}
function IsEqual(const A, B: ByteCharSet): Boolean;
asm
      MOV     ECX, [EDX]
      XOR     ECX, [EAX]
      JNE     @Fin0
      MOV     ECX, [EDX + 4]
      XOR     ECX, [EAX + 4]
      JNE     @Fin0
      MOV     ECX, [EDX + 8]
      XOR     ECX, [EAX + 8]
      JNE     @Fin0
      MOV     ECX, [EDX + 12]
      XOR     ECX, [EAX + 12]
      JNE     @Fin0
      MOV     ECX, [EDX + 16]
      XOR     ECX, [EAX + 16]
      JNE     @Fin0
      MOV     ECX, [EDX + 20]
      XOR     ECX, [EAX + 20]
      JNE     @Fin0
      MOV     ECX, [EDX + 24]
      XOR     ECX, [EAX + 24]
      JNE     @Fin0
      MOV     ECX, [EDX + 28]
      XOR     ECX, [EAX + 28]
      JNE     @Fin0
      MOV     EAX, 1
      RET
@Fin0:
      XOR     EAX, EAX
end;
{$ELSE}
function IsEqual(const A, B: ByteCharSet): Boolean;
begin
  Result := A = B;
end;
{$ENDIF}

{$IFDEF ASM386_DELPHI}
function IsEmpty(const C: ByteCharSet): Boolean;
asm
      MOV     EDX, [EAX]
      OR      EDX, [EAX + 4]
      OR      EDX, [EAX + 8]
      OR      EDX, [EAX + 12]
      OR      EDX, [EAX + 16]
      OR      EDX, [EAX + 20]
      OR      EDX, [EAX + 24]
      OR      EDX, [EAX + 28]
      JNE     @Fin0
      MOV     EAX, 1
      RET
@Fin0:
      XOR     EAX,EAX
end;
{$ELSE}
function IsEmpty(const C: ByteCharSet): Boolean;
begin
  Result := C = [];
end;
{$ENDIF}

{$IFDEF ASM386_DELPHI}
function IsComplete(const C: ByteCharSet): Boolean;
asm
      MOV     EDX, [EAX]
      AND     EDX, [EAX + 4]
      AND     EDX, [EAX + 8]
      AND     EDX, [EAX + 12]
      AND     EDX, [EAX + 16]
      AND     EDX, [EAX + 20]
      AND     EDX, [EAX + 24]
      AND     EDX, [EAX + 28]
      CMP     EDX, $FFFFFFFF
      JNE     @Fin0
      MOV     EAX, 1
      RET
@Fin0:
      XOR     EAX, EAX
end;
{$ELSE}
function IsComplete(const C: ByteCharSet): Boolean;
begin
  Result := C = CompleteByteCharSet;
end;
{$ENDIF}

{$IFDEF __ASM386_DELPHI}
function CharCount(const C: ByteCharSet): Integer;
asm
      PUSH    EBX
      PUSH    ESI
      MOV     EBX, EAX
      XOR     ESI, ESI
      MOV     EAX, [EBX]
      CALL    BitCount32
      ADD     ESI, EAX
      MOV     EAX, [EBX + 4]
      CALL    BitCount32
      ADD     ESI, EAX
      MOV     EAX, [EBX + 8]
      CALL    BitCount32
      ADD     ESI, EAX
      MOV     EAX, [EBX + 12]
      CALL    BitCount32
      ADD     ESI, EAX
      MOV     EAX, [EBX + 16]
      CALL    BitCount32
      ADD     ESI, EAX
      MOV     EAX, [EBX + 20]
      CALL    BitCount32
      ADD     ESI, EAX
      MOV     EAX, [EBX + 24]
      CALL    BitCount32
      ADD     ESI, EAX
      MOV     EAX, [EBX + 28]
      CALL    BitCount32
      ADD     EAX, ESI
      POP     ESI
      POP     EBX
end;
{$ELSE}
function CharCount(const C: ByteCharSet): Integer;
var I : ByteChar;
begin
  Result := 0;
  for I := ByteChar(0) to ByteChar(255) do
    if I in C then
      Inc(Result);
end;
{$ENDIF}

{$IFDEF ASM386_DELPHI}
procedure ConvertCaseInsensitive(var C: ByteCharSet);
asm
      MOV     ECX, [EAX + 12]
      AND     ECX, $3FFFFFF
      OR      [EAX + 8], ECX
      MOV     ECX, [EAX + 8]
      AND     ECX, $3FFFFFF
      OR      [EAX + 12], ECX
end;
{$ELSE}
procedure ConvertCaseInsensitive(var C: ByteCharSet);
var Ch : ByteChar;
begin
  for Ch := ByteChar(Ord('A')) to ByteChar(Ord('Z')) do
    if Ch in C then
      Include(C, ByteChar(Ord(Ch) + 32));
  for Ch := ByteChar(Ord('a')) to ByteChar(Ord('z')) do
    if Ch in C then
      Include(C, ByteChar(Ord(Ch) - 32));
end;
{$ENDIF}

function CaseInsensitiveCharSet(const C: ByteCharSet): ByteCharSet;
begin
  AssignCharSet(Result, C);
  ConvertCaseInsensitive(Result);
end;

{$IFDEF ASM386_DELPHI}
function CharSetToStrB(const C: ByteCharSet): RawByteString; // Andrew N. Driazgov
asm
      PUSH    EBX
      MOV     ECX, $100
      MOV     EBX, EAX
      PUSH    ESI
      MOV     EAX, EDX
      SUB     ESP, ECX
      XOR     ESI, ESI
      XOR     EDX, EDX
@@lp: BT      [EBX], EDX
      JC      @@mm
@@nx: INC     EDX
      DEC     ECX
      JNE     @@lp
      MOV     ECX, ESI
      MOV     EDX, ESP
      CALL    System.@LStrFromPCharLen
      ADD     ESP, $100
      POP     ESI
      POP     EBX
      RET
@@mm: MOV     [ESP + ESI], DL
      INC     ESI
      JMP     @@nx
end;
{$ELSE}
function CharSetToStrB(const C: ByteCharSet): RawByteString;
// Implemented recursively to avoid multiple memory allocations
  procedure CharMatch(const Start: ByteChar; const Count: Integer);
  var Ch : ByteChar;
  begin
    for Ch := Start to ByteChar(255) do
      if Ch in C then
        begin
          if Ch = ByteChar(255) then
            SetLength(Result, Count + 1)
          else
            CharMatch(ByteChar(Byte(Ch) + 1), Count + 1);
          Result[Count + 1] := Ch;
          exit;
        end;
    SetLength(Result, Count);
  end;
begin
  CharMatch(ByteChar(0), 0);
end;
{$ENDIF}

{$IFDEF ASM386_DELPHI}
function StrToCharSetB(const S: RawByteString): ByteCharSet; // Andrew N. Driazgov
asm
      XOR     ECX, ECX
      MOV     [EDX], ECX
      MOV     [EDX + 4], ECX
      MOV     [EDX + 8], ECX
      MOV     [EDX + 12], ECX
      MOV     [EDX + 16], ECX
      MOV     [EDX + 20], ECX
      MOV     [EDX + 24], ECX
      MOV     [EDX + 28], ECX
      TEST    EAX, EAX
      JE      @@qt
      MOV     ECX, [EAX - 4]
      PUSH    EBX
      SUB     ECX, 8
      JS      @@nx
@@lp: MOVZX   EBX, BYTE PTR [EAX]
      BTS     [EDX], EBX
      MOVZX   EBX, BYTE PTR [EAX + 1]
      BTS     [EDX], EBX
      MOVZX   EBX, BYTE PTR [EAX + 2]
      BTS     [EDX], EBX
      MOVZX   EBX, BYTE PTR [EAX + 3]
      BTS     [EDX], EBX
      MOVZX   EBX, BYTE PTR [EAX + 4]
      BTS     [EDX], EBX
      MOVZX   EBX, BYTE PTR [EAX + 5]
      BTS     [EDX], EBX
      MOVZX   EBX, BYTE PTR [EAX + 6]
      BTS     [EDX], EBX
      MOVZX   EBX, BYTE PTR [EAX + 7]
      BTS     [EDX], EBX
      ADD     EAX, 8
      SUB     ECX, 8
      JNS     @@lp
@@nx: JMP     DWORD PTR @@tV[ECX * 4 + 32]
@@tV: DD      @@ex, @@t1, @@t2, @@t3
      DD      @@t4, @@t5, @@t6, @@t7
@@t7: MOVZX   EBX, BYTE PTR [EAX + 6]
      BTS     [EDX], EBX
@@t6: MOVZX   EBX, BYTE PTR [EAX + 5]
      BTS     [EDX], EBX
@@t5: MOVZX   EBX, BYTE PTR [EAX + 4]
      BTS     [EDX], EBX
@@t4: MOVZX   EBX, BYTE PTR [EAX + 3]
      BTS     [EDX], EBX
@@t3: MOVZX   EBX, BYTE PTR [EAX + 2]
      BTS     [EDX], EBX
@@t2: MOVZX   EBX, BYTE PTR [EAX + 1]
      BTS     [EDX], EBX
@@t1: MOVZX   EBX, BYTE PTR [EAX]
      BTS     [EDX], EBX
@@ex: POP     EBX
@@qt:
end;
{$ELSE}
function StrToCharSetB(const S: RawByteString): ByteCharSet;
var I : Integer;
begin
  ClearCharSet(Result);
  for I := 1 to Length(S) do
    Include(Result, S[I]);
end;
{$ENDIF}



{                                                                              }
{ Character class strings                                                      }
{                                                                              }
{$IFDEF SupportAnsiString}
function CharSetToCharClassStr(const C: ByteCharSet): AnsiString;

  function ChStr(const Ch: ByteChar): AnsiString;
  begin
    case Ch of
      '\'       : Result := '\\';
      ']'       : Result := '\]';
      AsciiBEL  : Result := '\a';
      AsciiBS   : Result := '\b';
      AsciiESC  : Result := '\e';
      AsciiFF   : Result := '\f';
      AsciiLF   : Result := '\n';
      AsciiCR   : Result := '\r';
      AsciiHT   : Result := '\t';
      AsciiVT   : Result := '\v';
      else if (Ch < #32) or (Ch > #127) then // non-printable
        Result := '\x' + Word32ToHexA(Ord(Ch), 1) else
        Result := Ch;
    end;
  end;

  function SeqStr(const SeqStart, SeqEnd: ByteChar): AnsiString;
  begin
    Result := ChStr(SeqStart);
    if Ord(SeqEnd) = Ord(SeqStart) + 1 then
      Result := Result + ChStr(SeqEnd) else // consequetive chars
      if SeqEnd > SeqStart then // range
        Result := Result + '-' + ChStr(SeqEnd);
  end;

var CS       : ByteCharSet;
    F        : ByteChar;
    SeqStart : ByteChar;
    Seq      : Boolean;

begin
  if IsComplete(C) then
    Result := '.' else
  if IsEmpty(C) then
    Result := '[]' else
    begin
      Result := '[';
      CS := C;
      if (ByteChar(#0) in C) and (ByteChar(#255) in C) then
        begin
          ComplementCharSet(CS);
          Result := Result + '^';
        end;
      Seq := False;
      SeqStart := #0;
      for F := #0 to #255 do
        if F in CS then
          begin
            if not Seq then
              begin
                SeqStart := F;
                Seq := True;
              end;
          end else
          if Seq then
            begin
              Result := Result + SeqStr(SeqStart, ByteChar(Ord(F) - 1));
              Seq := False;
            end;
      if Seq then
        Result := Result + SeqStr(SeqStart, #255);
      Result := Result + ']';
    end;
end;
{$ENDIF}

(*
function CharClassStrToCharSet(const S: AnsiString): CharSet;
var I, L : Integer;

  function DecodeChar: ByteChar;
  var J : Integer;
  begin
    if S[I] = '\' then
      if I + 1 = L then
        begin
          Inc(I);
          Result := '\';
        end else
        if not MatchQuantSeqB(J, [['x'], csHexDigit, csHexDigit],
            [mqOnce, mqOnce, mqOptional], S, [moDeterministic], I + 1) then
          begin
            case S[I + 1] of
              '0' : Result := AsciiNULL;
              'a' : Result := AsciiBEL;
              'b' : Result := AsciiBS;
              'e' : Result := AsciiESC;
              'f' : Result := AsciiFF;
              'n' : Result := AsciiLF;
              'r' : Result := AsciiCR;
              't' : Result := AsciiHT;
              'v' : Result := AsciiVT;
              else Result := S[I + 1];
            end;
            Inc(I, 2);
          end else
          begin
            if J = I + 2 then
              Result := ByteChar(HexByteCharToInt(S[J])) else
              Result := ByteChar(HexByteCharToInt(S[J - 1]) * 16 + HexByteCharToInt(S[J]));
            I := J + 1;
          end
    else
      begin
        Result := S[I];
        Inc(I);
      end;
  end;

var Neg  : Boolean;
    A, B : ByteChar;
begin
  L := Length(S);
  if (L = 0) or (S = '[]') then
    Result := [] else
  if L = 1 then
    if S[1] in ['.', '*', '?'] then
      Result := CompleteCharSet else
      Result := [S[1]] else
  if (S[1] <> '[') or (S[L] <> ']') then
    raise EConvertError.Create('Invalid character class string')
  else
    begin
      Neg := S[2] = '^';
      I := iif(Neg, 3, 2);
      Result := [];
      while I < L do
        begin
          A := DecodeChar;
          if (I + 1 < L) and (S[I] = '-') then
            begin
              Inc(I);
              B := DecodeChar;
              Result := Result + [A..B];
            end else
            Include(Result, A);
       end;
      if Neg then
        ComplementCharSet(Result);
    end;
end;
*)



{$IFDEF CHARSET_TEST}
procedure Test;
begin
  // ByteCharSet
  Assert(CharCount([]) = 0, 'CharCount');
  Assert(CharCount([ByteChar(Ord('a'))..ByteChar(Ord('z'))]) = 26, 'CharCount');
  Assert(CharCount([ByteChar(0), ByteChar(255)]) = 2, 'CharCount');

  // CharClassStr
  {$IFDEF SupportAnsiString}
  Assert(CharSetToCharClassStr(['a'..'z']) = '[a-z]', 'CharClassStr');
  Assert(CharSetToCharClassStr(CompleteByteCharSet) = '.', 'CharClassStr');
  Assert(CharSetToCharClassStr([#0..#31]) = '[\x0-\x1F]', 'CharClassStr');
  Assert(CharSetToCharClassStr([#0..#32]) = '[\x0- ]', 'CharClassStr');
  Assert(CharSetToCharClassStr(CompleteByteCharSet - ['a']) = '[^a]', 'CharClassStr');
  Assert(CharSetToCharClassStr(CompleteByteCharSet - ['a'..'z']) = '[^a-z]', 'CharClassStr');
  Assert(CharSetToCharClassStr(['a'..'b']) = '[ab]', 'CharClassStr');
  Assert(CharSetToCharClassStr([]) = '[]', 'CharClassStr');
  {$ENDIF}
  (*
  Assert(CharClassStrToCharSet('[a]') = ['a'], 'CharClassStr');
  Assert(CharClassStrToCharSet('[]') = [], 'CharClassStr');
  Assert(CharClassStrToCharSet('.') = CompleteCharSet, 'CharClassStr');
  Assert(CharClassStrToCharSet('') = [], 'CharClassStr');
  Assert(CharClassStrToCharSet('[a-z]') = ['a'..'z'], 'CharClassStr');
  Assert(CharClassStrToCharSet('[^a-z]') = CompleteCharSet - ['a'..'z'], 'CharClassStr');
  Assert(CharClassStrToCharSet('[-]') = ['-'], 'CharClassStr');
  Assert(CharClassStrToCharSet('[a-]') = ['a', '-'], 'CharClassStr');
  Assert(CharClassStrToCharSet('[\x5]') = [#$5], 'CharClassStr');
  Assert(CharClassStrToCharSet('[\x1f]') = [#$1f], 'CharClassStr');
  Assert(CharClassStrToCharSet('[\x10-]') = [#$10, '-'], 'CharClassStr');
  Assert(CharClassStrToCharSet('[\x10-\x1f]') = [#$10..#$1f], 'CharClassStr');
  Assert(CharClassStrToCharSet('[\x10-\xf]') = [], 'CharClassStr');
  *)
end;
{$ENDIF}



end.

