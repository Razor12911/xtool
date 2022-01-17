{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals 5.00                                        }
{   File name:        flcBit32.pas                                             }
{   File version:     5.03                                                     }
{   Description:      Bit function: 32 bit.                                    }
{                                                                              }
{   Copyright:        Copyright (c) 2001-2020, David J Butler                  }
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
{   2001/05/03  1.01  Improved bit functions. Added Pascal versions of         }
{                     assembly routines.                                       }
{   2015/06/07  4.02  Moved bit functions from cUtils to cBits32.              }
{   2016/01/09  5.03  Revised for Fundamentals 5.                              }
{                                                                              }
{ Supported compilers:                                                         }
{                                                                              }
{   Delphi XE7 Win32                    5.03  2016/01/09                       }
{   Delphi XE7 Win64                    5.03  2016/01/09                       }
{   Delphi 10 Win32                     5.03  2016/01/09                       }
{   Delphi 10 Win64                     5.03  2016/01/09                       }
{                                                                              }
{******************************************************************************}

{$INCLUDE ..\flcInclude.inc}

unit flcBits32;

interface

uses
  { Fundamentals }
  flcStdTypes;



{                                                                              }
{ Bit functions                                                                }
{                                                                              }
function  ClearBit32(const Value, BitIndex: Word32): Word32;
function  SetBit32(const Value, BitIndex: Word32): Word32;
function  IsBitSet32(const Value, BitIndex: Word32): Boolean;
function  ToggleBit32(const Value, BitIndex: Word32): Word32;
function  IsHighBitSet32(const Value: Word32): Boolean;

function  SetBitScanForward32(const Value: Word32): Integer; overload;
function  SetBitScanForward32(const Value, BitIndex: Word32): Integer; overload;
function  SetBitScanReverse32(const Value: Word32): Integer; overload;
function  SetBitScanReverse32(const Value, BitIndex: Word32): Integer; overload;
function  ClearBitScanForward32(const Value: Word32): Integer; overload;
function  ClearBitScanForward32(const Value, BitIndex: Word32): Integer; overload;
function  ClearBitScanReverse32(const Value: Word32): Integer; overload;
function  ClearBitScanReverse32(const Value, BitIndex: Word32): Integer; overload;

function  ReverseBits32(const Value: Word32): Word32; overload;
function  ReverseBits32(const Value: Word32; const BitCount: Integer): Word32; overload;
function  SwapEndian32(const Value: Word32): Word32;
procedure SwapEndianBuf32(var Buf; const Count: Integer);
function  TwosComplement32(const Value: Word32): Word32;

function  RotateLeftBits16(const Value: Word; const Bits: Byte): Word;
function  RotateLeftBits32(const Value: Word32; const Bits: Byte): Word32;
function  RotateRightBits16(const Value: Word; const Bits: Byte): Word;
function  RotateRightBits32(const Value: Word32; const Bits: Byte): Word32;

function  BitCount32(const Value: Word32): Word32;
function  IsPowerOfTwo32(const Value: Word32): Boolean;

function  LowBitMask32(const HighBitIndex: Word32): Word32;
function  HighBitMask32(const LowBitIndex: Word32): Word32;
function  RangeBitMask32(const LowBitIndex, HighBitIndex: Word32): Word32;

function  SetBitRange32(const Value: Word32;
          const LowBitIndex, HighBitIndex: Word32): Word32;
function  ClearBitRange32(const Value: Word32;
          const LowBitIndex, HighBitIndex: Word32): Word32;
function  ToggleBitRange32(const Value: Word32;
          const LowBitIndex, HighBitIndex: Word32): Word32;
function  IsBitRangeSet32(const Value: Word32;
          const LowBitIndex, HighBitIndex: Word32): Boolean;
function  IsBitRangeClear32(const Value: Word32;
          const LowBitIndex, HighBitIndex: Word32): Boolean;

const
  BitMaskTable32: array[0..31] of Word32 =
    ($00000001, $00000002, $00000004, $00000008,
     $00000010, $00000020, $00000040, $00000080,
     $00000100, $00000200, $00000400, $00000800,
     $00001000, $00002000, $00004000, $00008000,
     $00010000, $00020000, $00040000, $00080000,
     $00100000, $00200000, $00400000, $00800000,
     $01000000, $02000000, $04000000, $08000000,
     $10000000, $20000000, $40000000, $80000000);



{                                                                              }
{ Test cases                                                                   }
{                                                                              }
{$IFDEF DEBUG}
{$IFDEF TEST}
procedure Test;
{$ENDIF}
{$ENDIF}



implementation



{                                                                              }
{ Bit functions                                                                }
{                                                                              }
{$IFDEF ASM386_DELPHI}
function ReverseBits32(const Value: Word32): Word32; register; assembler;
asm
      BSWAP   EAX
      MOV     EDX, EAX
      AND     EAX, 0AAAAAAAAh
      SHR     EAX, 1
      AND     EDX, 055555555h
      SHL     EDX, 1
      OR      EAX, EDX
      MOV     EDX, EAX
      AND     EAX, 0CCCCCCCCh
      SHR     EAX, 2
      AND     EDX, 033333333h
      SHL     EDX, 2
      OR      EAX, EDX
      MOV     EDX, EAX
      AND     EAX, 0F0F0F0F0h
      SHR     EAX, 4
      AND     EDX, 00F0F0F0Fh
      SHL     EDX, 4
      OR      EAX, EDX
end;
{$ELSE}
function ReverseBits32(const Value: Word32): Word32;
var I : Byte;
begin
  Result := 0;
  for I := 0 to 31 do
    if Value and BitMaskTable32[I] <> 0 then
      Result := Result or BitMaskTable32[31 - I];
end;
{$ENDIF}

function ReverseBits32(const Value: Word32; const BitCount: Integer): Word32;
var I, C : Integer;
    V : Word32;
begin
  V := Value;
  Result := 0;
  C := BitCount;
  if C > 32 then
    C := 32;
  for I := 0 to C - 1 do
    begin
      Result := (Result shl 1) or (V and 1);
      V := V shr 1;
    end;
end;

{$IFDEF ASM386_DELPHI}
function SwapEndian32(const Value: Word32): Word32; register; assembler;
asm
      XCHG    AH, AL
      ROL     EAX, 16
      XCHG    AH, AL
end;
{$ELSE}
function SwapEndian32(const Value: Word32): Word32;
begin
  Result := ((Value and $000000FF) shl 24)  or
            ((Value and $0000FF00) shl 8)   or
            ((Value and $00FF0000) shr 8)   or
            ((Value and $FF000000) shr 24);
end;
{$ENDIF}

procedure SwapEndianBuf32(var Buf; const Count: Integer);
var P : PWord32;
    I : Integer;
begin
  P := @Buf;
  for I := 1 to Count do
    begin
      P^ := SwapEndian32(P^);
      Inc(P);
    end;
end;

{$IFDEF ASM386_DELPHI}
function TwosComplement32(const Value: Word32): Word32; register; assembler;
asm
      NEG     EAX
end;
{$ELSE}
function TwosComplement32(const Value: Word32): Word32;
begin
  Result := Word32(not Value + 1);
end;
{$ENDIF}

{$IFDEF ASM386_DELPHI}
function RotateLeftBits16(const Value: Word; const Bits: Byte): Word;
asm
      MOV     CL, DL
      ROL     AX, CL
end;
{$ELSE}
function RotateLeftBits16(const Value: Word; const Bits: Byte): Word;
var I, B : Integer;
    R : Word;
begin
  R := Value;
  if Bits >= 16 then
    B := Bits mod 16
  else
    B := Bits;
  for I := 1 to B do
    if R and $8000 = 0 then
      R := Word(R shl 1)
    else
      R := Word(R shl 1) or 1;
  Result := R;
end;
{$ENDIF}

{$IFDEF ASM386_DELPHI}
function RotateLeftBits32(const Value: Word32; const Bits: Byte): Word32;
asm
      MOV     CL, DL
      ROL     EAX, CL
end;
{$ELSE}
function RotateLeftBits32(const Value: Word32; const Bits: Byte): Word32;
var I, B : Integer;
    R : Word32;
begin
  R := Value;
  if Bits >= 32 then
    B := Bits mod 32
  else
    B := Bits;
  for I := 1 to B do
    if R and $80000000 = 0 then
      R := Word32(R shl 1)
    else
      R := Word32(R shl 1) or 1;
  Result := R;
end;
{$ENDIF}

{$IFDEF ASM386_DELPHI}
function RotateRightBits16(const Value: Word; const Bits: Byte): Word;
asm
      MOV     CL, DL
      ROR     AX, CL
end;
{$ELSE}
function RotateRightBits16(const Value: Word; const Bits: Byte): Word;
var I, B : Integer;
    R : Word;
begin
  R := Value;
  if Bits >= 16 then
    B := Bits mod 16
  else
    B := Bits;
  for I := 1 to B do
    if R and 1 = 0 then
      R := Word(R shr 1)
    else
      R := Word(R shr 1) or $8000;
  Result := R;
end;
{$ENDIF}

{$IFDEF ASM386_DELPHI}
function RotateRightBits32(const Value: Word32; const Bits: Byte): Word32;
asm
      MOV     CL, DL
      ROR     EAX, CL
end;
{$ELSE}
function RotateRightBits32(const Value: Word32; const Bits: Byte): Word32;
var I, B : Integer;
    R : Word32;
begin
  R := Value;
  if Bits >= 32 then
    B := Bits mod 32
  else
    B := Bits;
  for I := 1 to B do
    if R and 1 = 0 then
      R := Word32(R shr 1)
    else
      R := Word32(R shr 1) or $80000000;
  Result := R;
end;
{$ENDIF}

{$IFDEF ___ASM386_DELPHI}
function SetBit32(const Value, BitIndex: Word32): Word32;
asm
      {$IFOPT R+}
      CMP     BitIndex, 32
      JB      @RangeOk
      JMP     RaiseRangeCheckError
  @RangeOk:
      {$ENDIF}
      OR      EAX, DWORD PTR [BitIndex * 4 + BitMaskTable32]
end;
{$ELSE}
function SetBit32(const Value, BitIndex: Word32): Word32;
begin
  Result := Value or BitMaskTable32[BitIndex];
end;
{$ENDIF}

{$IFDEF ___ASM386_DELPHI}
function ClearBit32(const Value, BitIndex: Word32): Word32;
asm
      {$IFOPT R+}
      CMP     BitIndex, 32
      JB      @RangeOk
      JMP     RaiseRangeCheckError
  @RangeOk:
      {$ENDIF}
      MOV     ECX, DWORD PTR [BitIndex * 4 + BitMaskTable32]
      NOT     ECX
      AND     EAX, ECX
  @Fin:
end;
{$ELSE}
function ClearBit32(const Value, BitIndex: Word32): Word32;
begin
  Result := Value and not BitMaskTable32[BitIndex];
end;
{$ENDIF}

{$IFDEF ___ASM386_DELPHI}
function ToggleBit32(const Value, BitIndex: Word32): Word32;
asm
      {$IFOPT R+}
      CMP     BitIndex, 32
      JB      @RangeOk
      JMP     RaiseRangeCheckError
  @RangeOk:
      {$ENDIF}
      XOR     EAX, DWORD PTR [BitIndex * 4 + BitMaskTable32]
end;
{$ELSE}
function ToggleBit32(const Value, BitIndex: Word32): Word32;
begin
  Result := Value xor BitMaskTable32[BitIndex];
end;
{$ENDIF}

{$IFDEF ASM386_DELPHI}
function IsHighBitSet32(const Value: Word32): Boolean; register; assembler;
asm
      TEST    Value, $80000000
      SETNZ   AL
end;
{$ELSE}
function IsHighBitSet32(const Value: Word32): Boolean;
begin
  Result := Value and $80000000 <> 0;
end;
{$ENDIF}

{$IFDEF ___ASM386_DELPHI}
function IsBitSet32(const Value, BitIndex: Word32): Boolean;
asm
      {$IFOPT R+}
      CMP     BitIndex, 32
      JB      @RangeOk
      JMP     RaiseRangeCheckError
  @RangeOk:
      {$ENDIF}
      MOV     ECX, DWORD PTR BitMaskTable32 [BitIndex * 4]
      TEST    Value, ECX
      SETNZ   AL
end;
{$ELSE}
function IsBitSet32(const Value, BitIndex: Word32): Boolean;
begin
  Result := Value and BitMaskTable32[BitIndex] <> 0;
end;
{$ENDIF}

{$IFDEF ASM386_DELPHI}
function SetBitScanForward32(const Value: Word32): Integer;
asm
      OR      EAX, EAX
      JZ      @NoBits
      BSF     EAX, EAX
      RET
  @NoBits:
      MOV     EAX, -1
end;

function SetBitScanForward32(const Value, BitIndex: Word32): Integer;
asm
      CMP     BitIndex, 32
      JAE     @NotFound
      MOV     ECX, BitIndex
      MOV     EDX, $FFFFFFFF
      SHL     EDX, CL
      AND     EDX, EAX
      JE      @NotFound
      BSF     EAX, EDX
      RET
  @NotFound:
      MOV     EAX, -1
end;
{$ELSE}
function SetBitScanForward32(const Value, BitIndex: Word32): Integer;
var I : Integer;
begin
  if BitIndex < 32 then
    for I := Integer(BitIndex) to 31 do
      if Value and BitMaskTable32[I] <> 0 then
        begin
          Result := I;
          exit;
        end;
  Result := -1;
end;

function SetBitScanForward32(const Value: Word32): Integer;
begin
  Result := SetBitScanForward32(Value, 0);
end;
{$ENDIF}

{$IFDEF ASM386_DELPHI}
function SetBitScanReverse32(const Value: Word32): Integer;
asm
      OR      EAX, EAX
      JZ      @NoBits
      BSR     EAX, EAX
      RET
  @NoBits:
      MOV     EAX, -1
end;

function SetBitScanReverse32(const Value, BitIndex: Word32): Integer;
asm
      CMP     EDX, 32
      JAE     @NotFound
      LEA     ECX, [EDX - 31]
      MOV     EDX, $FFFFFFFF
      NEG     ECX
      SHR     EDX, CL
      AND     EDX, EAX
      JE      @NotFound
      BSR     EAX, EDX
      RET
  @NotFound:
      MOV     EAX, -1
end;
{$ELSE}
function SetBitScanReverse32(const Value, BitIndex: Word32): Integer;
var I : Integer;
begin
  if BitIndex < 32 then
    for I := Integer(BitIndex) downto 0 do
      if Value and BitMaskTable32[I] <> 0 then
        begin
          Result := I;
          exit;
        end;
  Result := -1;
end;

function SetBitScanReverse32(const Value: Word32): Integer;
begin
  Result := SetBitScanReverse32(Value, 31);
end;
{$ENDIF}

{$IFDEF ASM386_DELPHI}
function ClearBitScanForward32(const Value: Word32): Integer;
asm
      NOT     EAX
      OR      EAX, EAX
      JZ      @NoBits
      BSF     EAX, EAX
      RET
  @NoBits:
      MOV     EAX, -1
end;

function ClearBitScanForward32(const Value, BitIndex: Word32): Integer;
asm
      CMP     EDX, 32
      JAE     @NotFound
      MOV     ECX, EDX
      MOV     EDX, $FFFFFFFF
      NOT     EAX
      SHL     EDX, CL
      AND     EDX, EAX
      JE      @NotFound
      BSF     EAX, EDX
      RET
  @NotFound:
      MOV     EAX, -1
end;
{$ELSE}
function ClearBitScanForward32(const Value, BitIndex: Word32): Integer;
var I : Integer;
begin
  if BitIndex < 32 then
    for I := Integer(BitIndex) to 31 do
      if Value and BitMaskTable32[I] = 0 then
        begin
          Result := I;
          exit;
        end;
  Result := -1;
end;

function ClearBitScanForward32(const Value: Word32): Integer;
begin
  Result := ClearBitScanForward32(Value, 0);
end;
{$ENDIF}

{$IFDEF ASM386_DELPHI}
function ClearBitScanReverse32(const Value: Word32): Integer;
asm
      NOT     EAX
      OR      EAX, EAX
      JZ      @NoBits
      BSR     EAX, EAX
      RET
  @NoBits:
      MOV     EAX, -1
end;

function ClearBitScanReverse32(const Value, BitIndex: Word32): Integer;
asm
      CMP     EDX, 32
      JAE     @NotFound
      LEA     ECX, [EDX - 31]
      MOV     EDX, $FFFFFFFF
      NEG     ECX
      NOT     EAX
      SHR     EDX, CL
      AND     EDX, EAX
      JE      @NotFound
      BSR     EAX, EDX
      RET
  @NotFound:
      MOV     EAX, -1
end;
{$ELSE}
function ClearBitScanReverse32(const Value, BitIndex: Word32): Integer;
var I : Integer;
begin
  if BitIndex < 32 then
    for I := Integer(BitIndex) downto 0 do
      if Value and BitMaskTable32[I] = 0 then
        begin
          Result := I;
          exit;
        end;
  Result := -1;
end;

function ClearBitScanReverse32(const Value: Word32): Integer;
begin
  Result := ClearBitScanReverse32(Value, 31);
end;
{$ENDIF}

const
  BitCountTable32 : array[Byte] of Byte =
    (0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4,
     1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
     1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
     2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
     1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
     2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
     2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
     3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
     1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
     2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
     2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
     3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
     2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
     3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
     3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
     4, 5, 5, 6, 5, 6, 6, 7, 5, 6, 6, 7, 6, 7, 7, 8);

{$IFDEF ASM386_DELPHI}
function BitCount32(const Value: Word32): Word32; register; assembler;
asm
      MOVZX   EDX, AL
      MOVZX   EDX, BYTE PTR [EDX + BitCountTable32]
      MOVZX   ECX, AH
      ADD     DL, BYTE PTR [ECX + BitCountTable32]
      SHR     EAX, 16
      MOVZX   ECX, AH
      ADD     DL, BYTE PTR [ECX + BitCountTable32]
      AND     EAX, $FF
      ADD     DL, BYTE PTR [EAX + BitCountTable32]
      MOV     AL, DL
end;
{$ELSE}
function BitCount32(const Value: Word32): Word32;
begin
  Result := BitCountTable32[(Value and $000000FF)       ] +
            BitCountTable32[(Value and $0000FF00) shr 8 ] +
            BitCountTable32[(Value and $00FF0000) shr 16] +
            BitCountTable32[(Value and $FF000000) shr 24];
end;
{$ENDIF}

function IsPowerOfTwo32(const Value: Word32): Boolean;
begin
  Result := BitCount32(Value) = 1;
end;

function LowBitMask32(const HighBitIndex: Word32): Word32;
begin
  if HighBitIndex >= 32 then
    Result := 0
  else
    Result := BitMaskTable32[HighBitIndex] - 1;
end;

function HighBitMask32(const LowBitIndex: Word32): Word32;
begin
  if LowBitIndex >= 32 then
    Result := 0
  else
    Result := not BitMaskTable32[LowBitIndex] + 1;
end;

function RangeBitMask32(const LowBitIndex, HighBitIndex: Word32): Word32;
begin
  if (LowBitIndex >= 32) and (HighBitIndex >= 32) then
    begin
      Result := 0;
      exit;
    end;
  Result := $FFFFFFFF;
  if LowBitIndex > 0 then
    Result := Result xor (BitMaskTable32[LowBitIndex] - 1);
  if HighBitIndex < 31 then
    Result := Result xor (not BitMaskTable32[HighBitIndex + 1] + 1);
end;

function SetBitRange32(const Value: Word32; const LowBitIndex, HighBitIndex: Word32): Word32;
begin
  Result := Value or RangeBitMask32(LowBitIndex, HighBitIndex);
end;

function ClearBitRange32(const Value: Word32; const LowBitIndex, HighBitIndex: Word32): Word32;
begin
  Result := Value and not RangeBitMask32(LowBitIndex, HighBitIndex);
end;

function ToggleBitRange32(const Value: Word32; const LowBitIndex, HighBitIndex: Word32): Word32;
begin
  Result := Value xor RangeBitMask32(LowBitIndex, HighBitIndex);
end;

function IsBitRangeSet32(const Value: Word32; const LowBitIndex, HighBitIndex: Word32): Boolean;
var M: Word32;
begin
  M := RangeBitMask32(LowBitIndex, HighBitIndex);
  Result := Value and M = M;
end;

function IsBitRangeClear32(const Value: Word32; const LowBitIndex, HighBitIndex: Word32): Boolean;
begin
  Result := Value and RangeBitMask32(LowBitIndex, HighBitIndex) = 0;
end;



{                                                                              }
{ Test cases                                                                   }
{                                                                              }
{$IFDEF DEBUG}
{$IFDEF TEST}
{$ASSERTIONS ON}
procedure Test;
begin
  Assert(SetBit32($100F, 5) = $102F,            'SetBit');
  Assert(ClearBit32($102F, 5) = $100F,          'ClearBit');
  Assert(ToggleBit32($102F, 5) = $100F,         'ToggleBit');
  Assert(ToggleBit32($100F, 5) = $102F,         'ToggleBit');
  Assert(IsBitSet32($102F, 5),                  'IsBitSet');
  Assert(not IsBitSet32($100F, 5),              'IsBitSet');
  Assert(IsHighBitSet32($80000000),             'IsHighBitSet');
  Assert(not IsHighBitSet32($00000001),         'IsHighBitSet');
  Assert(not IsHighBitSet32($7FFFFFFF),         'IsHighBitSet');

  Assert(SetBitScanForward32(0) = -1,           'SetBitScanForward');
  Assert(SetBitScanForward32($1020) = 5,        'SetBitScanForward');
  Assert(SetBitScanReverse32($1020) = 12,       'SetBitScanForward');
  Assert(SetBitScanForward32($1020, 6) = 12,    'SetBitScanForward');
  Assert(SetBitScanReverse32($1020, 11) = 5,    'SetBitScanForward');
  Assert(ClearBitScanForward32($FFFFFFFF) = -1, 'ClearBitScanForward');
  Assert(ClearBitScanForward32($1020) = 0,      'ClearBitScanForward');
  Assert(ClearBitScanReverse32($1020) = 31,     'ClearBitScanForward');
  Assert(ClearBitScanForward32($1020, 5) = 6,   'ClearBitScanForward');
  Assert(ClearBitScanReverse32($1020, 12) = 11, 'ClearBitScanForward');

  Assert(ReverseBits32($12345678) = $1E6A2C48,  'ReverseBits');
  Assert(ReverseBits32($1) = $80000000,         'ReverseBits');
  Assert(ReverseBits32($80000000) = $1,         'ReverseBits');
  Assert(SwapEndian32($12345678) = $78563412,   'SwapEndian');

  Assert(BitCount32($12341234) = 10,            'BitCount');
  Assert(IsPowerOfTwo32(1),                     'IsPowerOfTwo');
  Assert(IsPowerOfTwo32(2),                     'IsPowerOfTwo');
  Assert(not IsPowerOfTwo32(3),                 'IsPowerOfTwo');

  Assert(RotateLeftBits32(0, 1) = 0,          'RotateLeftBits32');
  Assert(RotateLeftBits32(1, 0) = 1,          'RotateLeftBits32');
  Assert(RotateLeftBits32(1, 1) = 2,          'RotateLeftBits32');
  Assert(RotateLeftBits32($80000000, 1) = 1,  'RotateLeftBits32');
  Assert(RotateLeftBits32($80000001, 1) = 3,  'RotateLeftBits32');
  Assert(RotateLeftBits32(1, 2) = 4,          'RotateLeftBits32');
  Assert(RotateLeftBits32(1, 31) = $80000000, 'RotateLeftBits32');
  Assert(RotateLeftBits32(5, 2) = 20,         'RotateLeftBits32');
  Assert(RotateRightBits32(0, 1) = 0,         'RotateRightBits32');
  Assert(RotateRightBits32(1, 0) = 1,         'RotateRightBits32');
  Assert(RotateRightBits32(1, 1) = $80000000, 'RotateRightBits32');
  Assert(RotateRightBits32(2, 1) = 1,         'RotateRightBits32');
  Assert(RotateRightBits32(4, 2) = 1,         'RotateRightBits32');

  Assert(LowBitMask32(10) = $3FF,               'LowBitMask');
  Assert(HighBitMask32(28) = $F0000000,         'HighBitMask');
  Assert(RangeBitMask32(2, 6) = $7C,            'RangeBitMask');

  Assert(SetBitRange32($101, 2, 6) = $17D,      'SetBitRange');
  Assert(ClearBitRange32($17D, 2, 6) = $101,    'ClearBitRange');
  Assert(ToggleBitRange32($17D, 2, 6) = $101,   'ToggleBitRange');
  Assert(IsBitRangeSet32($17D, 2, 6),           'IsBitRangeSet');
  Assert(not IsBitRangeSet32($101, 2, 6),       'IsBitRangeSet');
  Assert(not IsBitRangeClear32($17D, 2, 6),     'IsBitRangeClear');
  Assert(IsBitRangeClear32($101, 2, 6),         'IsBitRangeClear');
end;
{$ENDIF}
{$ENDIF}


end.

