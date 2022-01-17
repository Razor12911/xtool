{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals 5.00                                        }
{   File name:        flcCipherRC2.pas                                         }
{   File version:     5.01                                                     }
{   Description:      RC2 cipher routines                                      }
{                                                                              }
{   Copyright:        Copyright (c) 2007-2020, David J Butler                  }
{                     All rights reserved.                                     }
{                     This file is licensed under the BSD License.             }
{                     See http://www.opensource.org/licenses/bsd-license.php   }
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
{   2007/01/05  4.01  Initial version.                                         }
{                                                                              }
{******************************************************************************}

{$INCLUDE flcCipher.inc}

unit flcCipherRC2;

interface



{                                                                              }
{ RC2                                                                          }
{   RC2 was developed by Ron Rivest in 1987.                                   }
{                                                                              }
type
  TRC2CipherKey = packed record
    case Integer of
      0 : (Bytes: array[0..127] of Byte);
      1 : (Words: array[0..63] of Word);
  end;
  PRC2CipherKey = ^TRC2CipherKey;

  TRC2Block = packed record
    case Integer of
      0 : (Bytes: array[0..7] of Byte);
      1 : (Words: array[0..3] of Word);
      2 : (A, B, C, D: Word);
  end;
  PRC2Block = ^TRC2Block;

const
  RC2BlockSize = 8;

procedure RC2Init(const Key; const KeySize: Integer; const KeyBits: Integer;
          var CipherKey: TRC2CipherKey);
procedure RC2EncryptBlock(const CipherKey: TRC2CipherKey; var Block: TRC2Block);
procedure RC2DecryptBlock(const CipherKey: TRC2CipherKey; var Block: TRC2Block);



implementation

uses
  { Cipher }
  flcCipherUtils;



{                                                                              }
{ RC2                                                                          }
{ See RFC 2268 for details on the algorithm.                                   }
{                                                                              }

// RC2 PITABLE - "Random" values based on value of Pi
const
  RC2Table: array[Byte] of Byte = (
    $D9, $78, $F9, $C4, $19, $DD, $B5, $ED, $28, $E9, $FD, $79, $4A, $A0, $D8, $9D,
    $C6, $7E, $37, $83, $2B, $76, $53, $8E, $62, $4C, $64, $88, $44, $8B, $FB, $A2,
    $17, $9A, $59, $F5, $87, $B3, $4F, $13, $61, $45, $6D, $8D, $09, $81, $7D, $32,
    $BD, $8F, $40, $EB, $86, $B7, $7B, $0B, $F0, $95, $21, $22, $5C, $6B, $4E, $82,
    $54, $D6, $65, $93, $CE, $60, $B2, $1C, $73, $56, $C0, $14, $A7, $8C, $F1, $DC,
    $12, $75, $CA, $1F, $3B, $BE, $E4, $D1, $42, $3D, $D4, $30, $A3, $3C, $B6, $26,
    $6F, $BF, $0E, $DA, $46, $69, $07, $57, $27, $F2, $1D, $9B, $BC, $94, $43, $03,
    $F8, $11, $C7, $F6, $90, $EF, $3E, $E7, $06, $C3, $D5, $2F, $C8, $66, $1E, $D7,
    $08, $E8, $EA, $DE, $80, $52, $EE, $F7, $84, $AA, $72, $AC, $35, $4D, $6A, $2A,
    $96, $1A, $D2, $71, $5A, $15, $49, $74, $4B, $9F, $D0, $5E, $04, $18, $A4, $EC,
    $C2, $E0, $41, $6E, $0F, $51, $CB, $CC, $24, $91, $AF, $50, $A1, $F4, $70, $39,
    $99, $7C, $3A, $85, $23, $B8, $B4, $7A, $FC, $02, $36, $5B, $25, $55, $97, $31,
    $2D, $5D, $FA, $98, $E3, $8A, $92, $AE, $05, $DF, $29, $10, $67, $6C, $BA, $C9,
    $D3, $00, $E6, $CF, $E1, $9E, $A8, $2C, $63, $16, $01, $3F, $58, $E2, $89, $A9,
    $0D, $38, $34, $1B, $AB, $33, $FF, $B0, $BB, $48, $0C, $5F, $B9, $B1, $CD, $2E,
    $C5, $F3, $DB, $47, $E5, $A5, $9C, $77, $0A, $A6, $20, $68, $FE, $7F, $C1, $AD);

// RC2 ROR helper function (16 bit rotate right)
{$IFDEF ASM386_DELPHI}
function RC2ROR(const Value: Word; const Bits: Byte): Word;
asm
      MOV     CL, DL
      ROR     AX, CL
end;
{$ELSE}
function RC2ROR(const Value: Word; const Bits: Byte): Word;
var I : Integer;
begin
  Result := Value;
  for I := 1 to Bits do
    if Result and 1 = 0 then
      Result := Result shr 1
    else
      Result := (Result shr 1) or $8000;
end;
{$ENDIF}

// RC2 ROL helper function (16 bit rotate left)
{$IFDEF ASM386_DELPHI}
function RC2ROL(const Value: Word; const Bits: Byte): Word;
asm
      MOV     CL, DL
      ROL     AX, CL
end;
{$ELSE}
function RC2ROL(const Value: Word; const Bits: Byte): Word;
var I : Integer;
begin
  Result := Value;
  for I := 1 to Bits do
    if Result and $8000 = 0 then
      Result := Result shl 1
    else
      Result := Word(Result shl 1) or 1;
end;
{$ENDIF}

// RC2 initialization
procedure RC2Init(
    const Key; const KeySize: Integer;
    const KeyBits: Integer;
    var CipherKey: TRC2CipherKey);
var I  : Integer;
    T8 : Byte;
    TM : Byte;
begin
  // Validate parameters
  if KeySize > 128 then
    raise ECipher.Create(CipherError_InvalidKeySize, 'Maximum RC2 key length is 128');
  if KeySize < 1 then
    raise ECipher.Create(CipherError_InvalidKeySize, 'Minimum RC2 key length is 1');
  if (KeyBits <= 0) or (KeyBits > 1024) then
    raise ECipher.Create(CipherError_InvalidKeyBits, 'Invalid number of key bits');
  // RC2 key expansion
  Move(Key, CipherKey, KeySize);
  with CipherKey do
    begin
      for I := KeySize to 127 do
        Bytes[I] := RC2Table[Byte(Bytes[I - 1] + Bytes[I - KeySize])];
      T8 := (KeyBits + 7) div 8;
      TM := 255 mod (1 shl (8 + KeyBits - 8 * T8));
      Bytes[128 - T8] := RC2Table[Bytes[128 - T8] and TM];
      for I := 127 - T8 downto 0 do
        Bytes[I] := RC2Table[Bytes[I + 1] xor Bytes[I + T8]];
    end;
end;

// RC2 Encrypt
// RC2 Mix:  R[i] = R[i] + K[j] + (R[i-1] & R[i-2]) + ((~R[i-1]) & R[i-3])
//           R[i] = R[i] rol s[i]
// RC2 Mash: R[i] = R[i] + K[R[i-1] & 63]
procedure RC2EncryptBlock(const CipherKey: TRC2CipherKey; var Block: TRC2Block);
var J : PWord;
    I : Integer;
begin
  J := @CipherKey.Words[0];
  with Block do
    begin
      for I := 1 to 5 do
        begin
          A := RC2ROL(Word(A + J^ + (D and C) + (not D and B)), 1); Inc(J);
          B := RC2ROL(Word(B + J^ + (A and D) + (not A and C)), 2); Inc(J);
          C := RC2ROL(Word(C + J^ + (B and A) + (not B and D)), 3); Inc(J);
          D := RC2ROL(Word(D + J^ + (C and B) + (not C and A)), 5); Inc(J);
        end;
      A := Word(A + CipherKey.Words[D and $3F]);
      B := Word(B + CipherKey.Words[A and $3F]);
      C := Word(C + CipherKey.Words[B and $3F]);
      D := Word(D + CipherKey.Words[C and $3F]);
      for I := 1 to 6 do
        begin
          A := RC2ROL(Word(A + J^ + (D and C) + (not D and B)), 1); Inc(J);
          B := RC2ROL(Word(B + J^ + (A and D) + (not A and C)), 2); Inc(J);
          C := RC2ROL(Word(C + J^ + (B and A) + (not B and D)), 3); Inc(J);
          D := RC2ROL(Word(D + J^ + (C and B) + (not C and A)), 5); Inc(J);
        end;
      A := Word(A + CipherKey.Words[D and $3F]);
      B := Word(B + CipherKey.Words[A and $3F]);
      C := Word(C + CipherKey.Words[B and $3F]);
      D := Word(D + CipherKey.Words[C and $3F]);
      for I := 1 to 5 do
        begin
          A := RC2ROL(Word(A + J^ + (D and C) + (not D and B)), 1); Inc(J);
          B := RC2ROL(Word(B + J^ + (A and D) + (not A and C)), 2); Inc(J);
          C := RC2ROL(Word(C + J^ + (B and A) + (not B and D)), 3); Inc(J);
          D := RC2ROL(Word(D + J^ + (C and B) + (not C and A)), 5); Inc(J);
        end;
    end;
end;

// RC2 Decrypt
// RC2 r-Mix:  R[i] = R[i] ror s[i]
//             R[i] = R[i] - K[j] - (R[i-1] & R[i-2]) - ((~R[i-1]) & R[i-3])
// RC2 r-Mash: R[i] = R[i] - K[R[i-1] & 63]
procedure RC2DecryptBlock(const CipherKey: TRC2CipherKey; var Block: TRC2Block);
var J : PWord;
    I : Integer;
begin
  J := @CipherKey.Words[63];
  with Block do
    begin
      for I := 1 to 5 do
        begin
          D := Word(RC2ROR(D, 5) - J^ - (C and B) - (not C and A)); Dec(J);
          C := Word(RC2ROR(C, 3) - J^ - (B and A) - (not B and D)); Dec(J);
          B := Word(RC2ROR(B, 2) - J^ - (A and D) - (not A and C)); Dec(J);
          A := Word(RC2ROR(A, 1) - J^ - (D and C) - (not D and B)); Dec(J);
        end;
      D := Word(D - CipherKey.Words[C and $3F]);
      C := Word(C - CipherKey.Words[B and $3F]);
      B := Word(B - CipherKey.Words[A and $3F]);
      A := Word(A - CipherKey.Words[D and $3F]);
      for I := 1 to 6 do
        begin
          D := Word(RC2ROR(D, 5) - J^ - (C and B) - (not C and A)); Dec(J);
          C := Word(RC2ROR(C, 3) - J^ - (B and A) - (not B and D)); Dec(J);
          B := Word(RC2ROR(B, 2) - J^ - (A and D) - (not A and C)); Dec(J);
          A := Word(RC2ROR(A, 1) - J^ - (D and C) - (not D and B)); Dec(J);
        end;
      D := Word(D - CipherKey.Words[C and $3F]);
      C := Word(C - CipherKey.Words[B and $3F]);
      B := Word(B - CipherKey.Words[A and $3F]);
      A := Word(A - CipherKey.Words[D and $3F]);
      for I := 1 to 5 do
        begin
          D := Word(RC2ROR(D, 5) - J^ - (C and B) - (not C and A)); Dec(J);
          C := Word(RC2ROR(C, 3) - J^ - (B and A) - (not B and D)); Dec(J);
          B := Word(RC2ROR(B, 2) - J^ - (A and D) - (not A and C)); Dec(J);
          A := Word(RC2ROR(A, 1) - J^ - (D and C) - (not D and B)); Dec(J);
        end;
    end;
end;



end.

