{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals 5.00                                        }
{   File name:        flcCipherDES.pas                                         }
{   File version:     5.04                                                     }
{   Description:      DES cipher routines                                      }
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
{ References:                                                                  }
{                                                                              }
{   DES 40 - http://www.watersprings.org/pub/id/draft-hoffman-des40-00.txt     }
{                                                                              }
{ Revision history:                                                            }
{                                                                              }
{   2007/01/06  4.01  DES and Triple-DES                                       }
{   2010/12/16  4.02  DES-40                                                   }
{   2019/06/09  5.03  Triple-DES3 buffer functions for Encrypt and Decrypt.    }
{   2019/09/22  5.04  Optimisations.                                           }
{                                                                              }
{******************************************************************************}

{$INCLUDE flcCipher.inc}

{$IFDEF FREEPASCAL}
{$R-}
{$ENDIF}

unit flcCipherDES;

interface

uses
  { Fundamentals }
  flcStdTypes;



{                                                                              }
{ DES                                                                          }
{   Data Encryption Standard.                                                  }
{   FIPS-46 became a US NBS standard in 1977.                                  }
{                                                                              }
type
  TDESKey = array[0..7] of Byte;
  PDESKey = ^TDESKey;

  TDESWord32Pair = array[0..1] of Word32;
  PDESWord32Pair = ^TDESWord32Pair;

  TDESContext = array[1..16] of TDESWord32Pair;
  PDESContext = ^TDESContext;

  TDESBlock = array[0..7] of Byte;
  PDESBlock = ^TDESBlock;

const
  DESBlockSize = 8;
  DESContextSize = SizeOf(TDESContext);

procedure DESInit(const Encrypt: Boolean; const Key: TDESKey; var Context: TDESContext);
procedure DESBuffer(const Context: TDESContext; var Block: TDESBlock);



{                                                                              }
{ DES-40                                                                       }
{                                                                              }
procedure DESKeyConvertToDES40(var Key: TDESKey);



{                                                                              }
{ Triple-DES-EDE                                                               }
{   Also known as 2-key Triple-DES                                             }
{                                                                              }
type
  TTripleDESKey = array[0..1] of TDESKey;
  PTripleDESKey = ^TTripleDESKey;
  TTripleDESContext = array[0..1] of TDESContext;
  PTripleDESContext = ^TTripleDESContext;

const
  TripleDESContextSize = SizeOf(TTripleDESContext);

procedure TripleDESInit(const Encrypt: Boolean; const Key: TTripleDESKey;
          var Context: TTripleDESContext);
procedure TripleDESBuffer(const Context: TTripleDESContext; var Block: TDESBlock);



{                                                                              }
{ Triple-DES-3 (EDE and EEE modes)                                             }
{   Also known as 3-key Triple-DES                                             }
{                                                                              }
type
  TTripleDES3Key = array[0..2] of TDESKey;
  PTripleDES3Key = ^TTripleDES3Key;
  TTripleDES3Context = array[0..2] of TDESContext;
  PTripleDES3Context = ^TTripleDES3Context;

const
  TripleDES3ContextSize = SizeOf(TTripleDES3Context);

procedure TripleDES3Init(const Encrypt: Boolean; const Key: TTripleDES3Key;
          const EEEMode: Boolean; var Context: TTripleDES3Context);
procedure TripleDES3BufferEncrypt(const Context: TTripleDES3Context; var Block: TDESBlock);
procedure TripleDES3BufferDecrypt(const Context: TTripleDES3Context; var Block: TDESBlock);



implementation

uses
  { Cipher }
  flcCipherUtils;



{                                                                              }
{ DES                                                                          }
{                                                                              }
const
  // DES PC-1 (key permutation 1) table
  DESPC1Table : array[0..55] of Byte = (
      57,  49,  41,  33,  25,  17,   9,
       1,  58,  50,  42,  34,  26,  18,
      10,   2,  59,  51,  43,  35,  27,
      19,  11,   3,  60,  52,  44,  36,
      63,  55,  47,  39,  31,  23,  15,
       7,  62,  54,  46,  38,  30,  22,
      14,   6,  61,  53,  45,  37,  29,
      21,  13,   5,  28,  20,  12,   4);

  // DES key shift table
  DESKeyShiftTable : array[1..16] of Byte = (
      1, 1, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 1);

  // DES PC-2 (key permuation 2) table
  DESPC2Table : array[0..47] of Byte = (
      14,  17,  11,  24,   1,   5,
       3,  28,  15,   6,  21,  10,
      23,  19,  12,   4,  26,   8,
      16,   7,  27,  20,  13,   2,
      41,  52,  31,  37,  47,  55,
      30,  40,  51,  45,  33,  48,
      44,  49,  39,  56,  34,  53,
      46,  42,  50,  36,  29,  32);

  // DES IP (initial permutation) table
  DESIPTable : array[0..63] of Byte = (
      58,  50,  42,  34,  26,  18,  10,   2,
      60,  52,  44,  36,  28,  20,  12,   4,
      62,  54,  46,  38,  30,  22,  14,   6,
      64,  56,  48,  40,  32,  24,  16,   8,
      57,  49,  41,  33,  25,  17,   9,   1,
      59,  51,  43,  35,  27,  19,  11,   3,
      61,  53,  45,  37,  29,  21,  13,   5,
      63,  55,  47,  39,  31,  23,  15,   7);

  // DES E bit-selection table
  DESETable : array[0..47] of Byte = (
      32,   1,   2,   3,   4,   5,
       4,   5,   6,   7,   8,   9,
       8,   9,  10,  11,  12,  13,
      12,  13,  14,  15,  16,  17,
      16,  17,  18,  19,  20,  21,
      20,  21,  22,  23,  24,  25,
      24,  25,  26,  27,  28,  29,
      28,  29,  30,  31,  32,   1);

  // DES S boxes (S1..S8)
  DESSTable : array[0..7, 0..63] of Byte = (
      (14,   4,  13,   1,   2,  15,  11,   8,   3,  10,   6,  12,   5,   9,   0,   7,
        0,  15,   7,   4,  14,   2,  13,   1,  10,   6,  12,  11,   9,   5,   3,   8,
        4,   1,  14,   8,  13,   6,   2,  11,  15,  12,   9,   7,   3,  10,   5,   0,
       15,  12,   8,   2,   4,   9,   1,   7,   5,  11,   3,  14,  10,   0,   6,  13),
      (15,   1,   8,  14,   6,  11,   3,   4,   9,   7,   2,  13,  12,   0,   5,  10,
        3,  13,   4,   7,  15,   2,   8,  14,  12,   0,   1,  10,   6,   9,  11,   5,
        0,  14,   7,  11,  10,   4,  13,   1,   5,   8,  12,   6,   9,   3,   2,  15,
       13,   8,  10,   1,   3,  15,   4,   2,  11,   6,   7,  12,   0,   5,  14,   9),
      (10,   0,   9,  14,   6,   3,  15,   5,   1,  13,  12,   7,  11,   4,   2,   8,
       13,   7,   0,   9,   3,   4,   6,  10,   2,   8,   5,  14,  12,  11,  15,   1,
       13,   6,   4,   9,   8,  15,   3,   0,  11,   1,   2,  12,   5,  10,  14,   7,
        1,  10,  13,   0,   6,   9,   8,   7,   4,  15,  14,   3,  11,   5,   2,  12),
      ( 7,  13,  14,   3,   0,   6,   9,  10,   1,   2,   8,   5,  11,  12,   4,  15,
       13,   8,  11,   5,   6,  15,   0,   3,   4,   7,   2,  12,   1,  10,  14,   9,
       10,   6,   9,   0,  12,  11,   7,  13,  15,   1,   3,  14,   5,   2,   8,   4,
        3,  15,   0,   6,  10,   1,  13,   8,   9,   4,   5,  11,  12,   7,   2,  14),
      ( 2,  12,   4,   1,   7,  10,  11,   6,   8,   5,   3,  15,  13,   0,  14,   9,
       14,  11,   2,  12,   4,   7,  13,   1,   5,   0,  15,  10,   3,   9,   8,   6,
        4,   2,   1,  11,  10,  13,   7,   8,  15,   9,  12,   5,   6,   3,   0,  14,
       11,   8,  12,   7,   1,  14,   2,  13,   6,  15,   0,   9,  10,   4,   5,   3),
      (12,   1,  10,  15,   9,   2,   6,   8,   0,  13,   3,   4,  14,   7,   5,  11,
       10,  15,   4,   2,   7,  12,   9,   5,   6,   1,  13,  14,   0,  11,   3,   8,
        9,  14,  15,   5,   2,   8,  12,   3,   7,   0,   4,  10,   1,  13,  11,   6,
        4,   3,   2,  12,   9,   5,  15,  10,  11,  14,   1,   7,   6,   0,   8,  13),
      ( 4,  11,   2,  14,  15,   0,   8,  13,   3,  12,   9,   7,   5,  10,   6,   1,
       13,   0,  11,   7,   4,   9,   1,  10,  14,   3,   5,  12,   2,  15,   8,   6,
        1,   4,  11,  13,  12,   3,   7,  14,  10,  15,   6,   8,   0,   5,   9,   2,
        6,  11,  13,   8,   1,   4,  10,   7,   9,   5,   0,  15,  14,   2,   3,  12),
      (13,   2,   8,   4,   6,  15,  11,   1,  10,   9,   3,  14,   5,   0,  12,   7,
        1,  15,  13,   8,  10,   3,   7,   4,  12,   5,   6,  11,   0,  14,   9,   2,
        7,  11,   4,   1,   9,  12,  14,   2,   0,   6,  10,  13,  15,   3,   5,   8,
        2,   1,  14,   7,   4,  10,   8,  13,  15,  12,   9,   0,   3,   5,   6,  11));

  // DES P (permutation) table
  DESPTable : array[0..31] of Byte = (
      16,   7,  20,  21,  29,  12,  28,  17,
       1,  15,  23,  26,   5,  18,  31,  10,
       2,   8,  24,  14,  32,  27,   3,   9,
      19,  13,  30,   6,  22,  11,   4,  25);

  // DES IP-1 table
  DESIP1Table : array[0..63] of Byte = (
      40,   8,  48,  16,  56,  24,  64,  32,
      39,   7,  47,  15,  55,  23,  63,  31,
      38,   6,  46,  14,  54,  22,  62,  30,
      37,   5,  45,  13,  53,  21,  61,  29,
      36,   4,  44,  12,  52,  20,  60,  28,
      35,   3,  43,  11,  51,  19,  59,  27,
      34,   2,  42,  10,  50,  18,  58,  26,
      33,   1,  41,   9,  49,  17,  57,  25);

// 28 bit ROL operation
function DESROL28(const Value: Word32; const Bits: Byte): Word32;
var I : Integer;
begin
  Result := Value;
  for I := 1 to Bits do
    if Result and $08000000 = 0 then
      Result := Result shl 1
    else
      Result := ((Result and $07FFFFFF) shl 1) or 1;
end;

// 32 bit swap endian
{$IFDEF ASM386}
function DESSwapEndian32(const Value: Word32): Word32;
asm
      XCHG    AH, AL
      ROL     EAX, 16
      XCHG    AH, AL
end;
{$ELSE}
function DESSwapEndian32(const Value: Word32): Word32;
begin
  Result := ((Value and $000000FF) shl 24)  or
            ((Value and $0000FF00) shl 8)   or
            ((Value and $00FF0000) shr 8)   or
            ((Value and $FF000000) shr 24);
end;
{$ENDIF}

// PC-1 permutation from K (64 bits) to K+ (56 bits)
function DESPC1Permutation(const K: TDESWord32Pair): TDESWord32Pair;
var I : Integer;
    N : Byte;
begin
  Result[0] := 0;
  Result[1] := 0;
  for I := 0 to 55 do
    begin
      N := DESPC1Table[I] - 1;
      if K[N div 32] and (1 shl (31 - (N mod 32))) <> 0 then
        Result[I div 32] := Result[I div 32] or (1 shl (31 - (I mod 32)));
    end;
end;

// PC-2 permutation from CnDn (56 bits) to Kn (48 bits)
function DESPC2Permutation(const K: TDESWord32Pair): TDESWord32Pair;
var I : Integer;
    N : Byte;
begin
  Result[0] := 0;
  Result[1] := 0;
  for I := 0 to 47 do
    begin
      N := DESPC2Table[I] - 1;
      if K[N div 32] and (1 shl (31 - (N mod 32))) <> 0 then
        Result[I div 32] := Result[I div 32] or (1 shl (31 - (I mod 32)));
    end;
end;

// DES initialization
procedure DESInit(const Encrypt: Boolean; const Key: TDESKey; var Context: TDESContext);
var K    : TDESWord32Pair;
    C, D : array[0..16] of Word32;
    I, J : Integer;
    N    : Byte;
    E    : TDESWord32Pair;
begin
  // Move key into K and swap endian
  Move(Key, K, Sizeof(K));
  K[0] := DESSwapEndian32(K[0]);
  K[1] := DESSwapEndian32(K[1]);
  // PC-1 permutation from KD (64 bits) into KP (56 bits)
  K := DESPC1Permutation(K);
  // Split KP into C0 and D0 (28 bits each)
  C[0] := (K[0] and $FFFFFFF0) shr 4;
  D[0] := ((K[0] and $0000000F) shl 24) or ((K[1] and $FFFFFF00) shr 8);
  SecureClear(K, Sizeof(K));
  // Generate C1..C16 and D1..D16 using shift table
  for I := 1 to 16 do
    begin
      N := DESKeyShiftTable[I];
      C[I] := DESROL28(C[I - 1], N);
      D[I] := DESROL28(D[I - 1], N);
    end;
  // PC-2 permutation from C1..16 (28 bits) and D1..16 (28 bits) into K1..K16 (48 bits)
  for I := 1 to 16 do
    begin
      E[0] := (C[I] shl 4) or (D[I] shr 24);
      E[1] := D[I] shl 8;
      if Encrypt then
        J := I
      else
        J := 17 - I;
      Context[J] := DESPC2Permutation(E);
    end;
  SecureClear(C, Sizeof(C));
  SecureClear(D, Sizeof(D));
  SecureClear(E, Sizeof(E));
end;

{$R-,Q-}

// DES E bit-selection table (modified for bit operation optimisation)
const
  DESETable_Mod : array[0..47] of Word32 = (
      1 shl (32 - 32),   Word32(1) shl (32 - 1),   1 shl (32 - 2),   1 shl (32 - 3),   1 shl (32 - 4),   1 shl (32 - 5),
       1 shl (32 - 4),   1 shl (32 - 5),   1 shl (32 - 6),   1 shl (32 - 7),   1 shl (32 - 8),   1 shl (32 - 9),
       1 shl (32 - 8),   1 shl (32 - 9),  1 shl (32 - 10),  1 shl (32 - 11),  1 shl (32 - 12),  1 shl (32 - 13),
      1 shl (32 - 12),  1 shl (32 - 13),  1 shl (32 - 14),  1 shl (32 - 15),  1 shl (32 - 16),  1 shl (32 - 17),
      1 shl (32 - 16),  1 shl (32 - 17),  1 shl (32 - 18),  1 shl (32 - 19),  1 shl (32 - 20),  1 shl (32 - 21),
      1 shl (32 - 20),  1 shl (32 - 21),  1 shl (32 - 22),  1 shl (32 - 23),  1 shl (32 - 24),  1 shl (32 - 25),
      1 shl (32 - 24),  1 shl (32 - 25),  1 shl (32 - 26),  1 shl (32 - 27),  1 shl (32 - 28),  1 shl (32 - 29),
      1 shl (32 - 28),  1 shl (32 - 29),  1 shl (32 - 30),  1 shl (32 - 31),  1 shl (32 - 32),   Word32(1) shl (32 - 1));

  // DES P (permutation) table (modified for bit operation optimisation)
  DESPTable_Mod : array[0..31] of Word32 = (
      1 shl (32 - 16),   1 shl (32 - 7),  1 shl (32 - 20),  1 shl (32 - 21),  1 shl (32 - 29),  1 shl (32 - 12),  1 shl (32 - 28),  1 shl (32 - 17),
       Word32(1) shl (32 - 1),  1 shl (32 - 15),  1 shl (32 - 23),  1 shl (32 - 26),   1 shl (32 - 5),  1 shl (32 - 18),  1 shl (32 - 31),  1 shl (32 - 10),
       1 shl (32 - 2),   1 shl (32 - 8),  1 shl (32 - 24),  1 shl (32 - 14),  1 shl (32 - 32),  1 shl (32 - 27),   1 shl (32 - 3),   1 shl (32 - 9),
      1 shl (32 - 19),  1 shl (32 - 13),  1 shl (32 - 30),   1 shl (32 - 6),  1 shl (32 - 22),  1 shl (32 - 11),   1 shl (32 - 4),  1 shl (32 - 25));

// DES E-function
function DESEFunc(const R: Word32; const K: TDESWord32Pair): Word32;
var E0, E1 : Word32;
    B      : Word32;
    I      : Integer;
    BI, SI : Byte;
    X, Y   : Byte;
    S      : Word32;
begin
  // Expand R (32 bits) to E (48 bits) using E bit-selection table
  E0 := 0;
  E1 := 0;
  B := Word32(1) shl 31;
  for I := 0 to 31 do
    begin
      if R and DESETable_Mod[I] <> 0 then
        E0 := E0 or B;
      B := B shr 1;
    end;
  B := Word32(1) shl 31;
  for I := 32 to 47 do
    begin
      if R and DESETable_Mod[I] <> 0 then
        E1 := E1 or B;
      B := B shr 1;
    end;
  // Apply key (48 bits) to E
  E0 := E0 xor K[0];
  E1 := E1 xor K[1];
  // Apply S boxes on 8 groups of 6 bits
  S := 0;
  for I := 0 to 7 do
    begin
      case I of
        0..4 : BI :=  (E0 shr (26 - (I * 6))) and $3F;
        5    : BI := ((E0 and $00000003) shl 4) or
                     ((E1 and $F0000000) shr 28);
          else BI :=  (E1 shr (22 + 36 - (I * 6))) and $3F;
      end;                                     // BI = Bi (6 bits) from E
      Y := (BI and 1) or ((BI and $20) shr 4); // Y  = Row (first and last bits of Bi)
      X := (BI and $1E) shr 1;                 // X  = Column (middle 4 bits of Bi)
      SI := DESSTable[I, Y * 16 + X];          // SI = Si (4 bits)
      S := S or (SI shl (28 - (I * 4)));       // S  = S0..S7 (32 bits)
    end;

  // Apply permutation table to S
  Result := 0;
  B := Word32(1) shl 31;
  for I := 0 to 31 do
    begin
      if S and DESPTable_Mod[I] <> 0 then
        Result := Result or B;
      B := B shr 1;
    end;
end;

// DES block function
procedure DESBuffer(const Context: TDESContext; var Block: TDESBlock);
var IP   : TDESBlock;
    IPP  : PByte;
    I    : Integer;
    IH   : Byte;
    IL   : Byte;
    N    : Byte;
    L, R : array[0..16] of Word32;
    P    : array[0..1] of Word32;
begin
  // Apply initial permuation on M (64 bits) to get IP (64 bits)
  FillChar(IP, Sizeof(TDESBlock), 0);
  IH := 0;
  IPP := @IP[IH];
  for I := 0 to 63 do
    begin
      IL := I and 7;
      if IL = 0 then
        begin
          IH := I shr 3;
          IPP := @IP[IH];
        end;
      N := DESIPTable[I] - 1;
      if Block[N shr 3] and (1 shl (7 - (N and 7))) <> 0 then
        IPP^ := IPP^ or (1 shl (7 - IL));
    end;
  // Split IP into L0 and R0 (32 bits each)
  L[0] := IP[3] or (IP[2] shl 8) or (IP[1] shl 16) or (IP[0] shl 24);
  R[0] := IP[7] or (IP[6] shl 8) or (IP[5] shl 16) or (IP[4] shl 24);
  SecureClear(IP, Sizeof(IP));
  // Generate L1..L16 and R1..R16
  for I := 1 to 16 do
    begin
      L[I] := R[I - 1];
      R[I] := L[I - 1] xor DESEFunc(R[I - 1], Context[I]);
    end;
  // Combine R16L16 (64 bits)
  P[0] := R[16];
  P[1] := L[16];
  SecureClear(L, Sizeof(L));
  SecureClear(R, Sizeof(R));
  // Apply IP-1 permutation to R16L16 to get result (64 bits)
  FillChar(Block, Sizeof(TDESBlock), 0);
  IH := 0;
  IPP := @Block[IH];
  for I := 0 to 63 do
    begin
      IL := I and 7;
      if IL = 0 then
        begin
          IH := I shr 3;
          IPP := @Block[IH];
        end;
      N := DESIP1Table[I] - 1;
      if P[N shr 5] and (1 shl (31 - (N and $1F))) <> 0 then
        IPP^ := IPP^ or (1 shl (7 - IL));
    end;
  SecureClear(P, Sizeof(P));
end;



{                                                                              }
{ DES-40                                                                       }
{                                                                              }
const
  BitCountTable : array[Byte] of Byte =
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

procedure DESSetOddParity(var A: Byte);
begin
  if not Odd(BitCountTable[A]) then
    A := A xor $80;
end;

procedure DESKeyConvertToDES40(var Key: TDESKey);
var A, B, C, D : Byte;
begin
  // clear lower 4 bits of every second byte starting at first byte
  A := Key[0] and $F0;
  B := Key[2] and $F0;
  C := Key[4] and $F0;
  D := Key[6] and $F0;
  // make parity odd on modified bytes by toggling high bit
  DESSetOddParity(A);
  DESSetOddParity(B);
  DESSetOddParity(C);
  DESSetOddParity(D);
  // modify key
  Key[0] := A;
  Key[2] := B;
  Key[4] := C;
  Key[6] := D;
end;



{                                                                              }
{ Triple-DES-EDE                                                               }
{                                                                              }
procedure TripleDESInit(const Encrypt: Boolean; const Key: TTripleDESKey;
    var Context: TTripleDESContext);
begin
  DESInit(Encrypt, Key[0], Context[0]);
  DESInit(not Encrypt, Key[1], Context[1]);
end;

procedure TripleDESBuffer(const Context: TTripleDESContext; var Block: TDESBlock);
begin
  DESBuffer(Context[0], Block);
  DESBuffer(Context[1], Block);
  DESBuffer(Context[0], Block);
end;



{                                                                              }
{ Triple-DES-EDE-3                                                             }
{                                                                              }
procedure TripleDES3Init(const Encrypt: Boolean; const Key: TTripleDES3Key;
          const EEEMode: Boolean; var Context: TTripleDES3Context);
begin
  DESInit(Encrypt, Key[0], Context[0]);
  DESInit(not Encrypt xor EEEMode, Key[1], Context[1]);
  DESInit(Encrypt, Key[2], Context[2]);
end;

procedure TripleDES3BufferEncrypt(const Context: TTripleDES3Context; var Block: TDESBlock);
begin
  DESBuffer(Context[0], Block);
  DESBuffer(Context[1], Block);
  DESBuffer(Context[2], Block);
end;

procedure TripleDES3BufferDecrypt(const Context: TTripleDES3Context; var Block: TDESBlock);
begin
  DESBuffer(Context[2], Block);
  DESBuffer(Context[1], Block);
  DESBuffer(Context[0], Block);
end;



end.

