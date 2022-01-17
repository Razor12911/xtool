{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals 5.00                                        }
{   File name:        flcCipherAES.pas                                         }
{   File version:     5.02                                                     }
{   Description:      AES cipher routines                                      }
{                                                                              }
{   Copyright:        Copyright (c) 2010-2020, David J Butler                  }
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
{   http://www.comms.scitech.sussex.ac.uk/fft/crypto/aesspec.pdf               }
{   http://people.eku.edu/styere/Encrypt/JS-AES.html                           }
{                                                                              }
{ Revision history:                                                            }
{                                                                              }
{   2010/12/15  4.01  Initial version.                                         }
{   2019/09/23  5.02  Optimisation.                                            }
{                                                                              }
{******************************************************************************}

{$INCLUDE flcCipher.inc}

unit flcCipherAES;

interface

uses
  { Fundamentals }
  flcStdTypes;



{                                                                              }
{ AES                                                                          }
{                                                                              }
const
  AESBlockSize = 16; // 128 bits
  AES_Nb = 4;        // Input bits (128) divided by 32. Fixed number for AES-128, AES-192 and AES-256.
  AES_Nr_Max = 14;   // Maximum number of rounds
  AES_KeyScheduleSize = AES_Nb * (AES_Nr_Max + 1);

type
  TAESState = array[0..3, 0..AES_Nb - 1] of Byte;
  TAESKeySchedule = array[0..AES_KeyScheduleSize - 1] of Word32;
  TAESContext = record
    Nk     : Byte;              // Nk = length of cipher key in multiples of 32 bits (4, 6 or 8)
    Nr     : Byte;              // Nr = number of rounds
    State  : TAESState;
    W      : TAESKeySchedule;
  end;
  PAESContext = ^TAESContext;

const
  AESContextSize = SizeOf(TAESContext);

procedure AESContextInit(var Context: TAESContext; const KeySize: Integer);
procedure AESContextFinalise(var Context: TAESContext);

procedure AESInit(var Context: TAESContext;
          const KeySize: Integer;
          const KeyBuf; const KeyBufSize: Integer);
procedure AESEncryptBlock(
          var Context: TAESContext;
          const DataBuf; const DataBufSize: Integer;
          var CipherBuf; const CipherBufSize: Integer);
procedure AESDecryptBlock(
          var Context: TAESContext;
          const DataBuf; const DataBufSize: Integer;
          var CipherBuf; const CipherBufSize: Integer);
procedure AESFinalise(var Context: TAESContext);

procedure AESEncrypt(
          const KeySize: Integer;
          const KeyBuf; const KeyBufSize: Integer;
          const DataBuf; const DataBufSize: Integer;
          var CipherBuf; const CipherBufSize: Integer);
procedure AESDecrypt(
          const KeySize: Integer;
          const KeyBuf; const KeyBufSize: Integer;
          const DataBuf; const DataBufSize: Integer;
          var CipherBuf; const CipherBufSize: Integer);

          

{                                                                              }
{ Test cases                                                                   }
{                                                                              }
{$IFDEF CIPHER_TEST}
procedure Test;
{$ENDIF}



implementation

uses
  { Cipher }
  flcCipherUtils;



{ AES S-Box and inverse S-Box }

const
  AES_S : array[Byte] of Byte = (
    $63, $7c, $77, $7b, $f2, $6b, $6f, $c5, $30, $01, $67, $2b, $fe, $d7, $ab, $76,
    $ca, $82, $c9, $7d, $fa, $59, $47, $f0, $ad, $d4, $a2, $af, $9c, $a4, $72, $c0,
    $b7, $fd, $93, $26, $36, $3f, $f7, $cc, $34, $a5, $e5, $f1, $71, $d8, $31, $15,
    $04, $c7, $23, $c3, $18, $96, $05, $9a, $07, $12, $80, $e2, $eb, $27, $b2, $75,
    $09, $83, $2c, $1a, $1b, $6e, $5a, $a0, $52, $3b, $d6, $b3, $29, $e3, $2f, $84,
    $53, $d1, $00, $ed, $20, $fc, $b1, $5b, $6a, $cb, $be, $39, $4a, $4c, $58, $cf,
    $d0, $ef, $aa, $fb, $43, $4d, $33, $85, $45, $f9, $02, $7f, $50, $3c, $9f, $a8,
    $51, $a3, $40, $8f, $92, $9d, $38, $f5, $bc, $b6, $da, $21, $10, $ff, $f3, $d2,
    $cd, $0c, $13, $ec, $5f, $97, $44, $17, $c4, $a7, $7e, $3d, $64, $5d, $19, $73,
    $60, $81, $4f, $dc, $22, $2a, $90, $88, $46, $ee, $b8, $14, $de, $5e, $0b, $db,
    $e0, $32, $3a, $0a, $49, $06, $24, $5c, $c2, $d3, $ac, $62, $91, $95, $e4, $79,
    $e7, $c8, $37, $6d, $8d, $d5, $4e, $a9, $6c, $56, $f4, $ea, $65, $7a, $ae, $08,
    $ba, $78, $25, $2e, $1c, $a6, $b4, $c6, $e8, $dd, $74, $1f, $4b, $bd, $8b, $8a,
    $70, $3e, $b5, $66, $48, $03, $f6, $0e, $61, $35, $57, $b9, $86, $c1, $1d, $9e,
    $e1, $f8, $98, $11, $69, $d9, $8e, $94, $9b, $1e, $87, $e9, $ce, $55, $28, $df,
    $8c, $a1, $89, $0d, $bf, $e6, $42, $68, $41, $99, $2d, $0f, $b0, $54, $bb, $16
    );

  AES_InvS : array[Byte] of Byte = (
    $52, $09, $6a, $d5, $30, $36, $a5, $38, $bf, $40, $a3, $9e, $81, $f3, $d7, $fb,
    $7c, $e3, $39, $82, $9b, $2f, $ff, $87, $34, $8e, $43, $44, $c4, $de, $e9, $cb,
    $54, $7b, $94, $32, $a6, $c2, $23, $3d, $ee, $4c, $95, $0b, $42, $fa, $c3, $4e,
    $08, $2e, $a1, $66, $28, $d9, $24, $b2, $76, $5b, $a2, $49, $6d, $8b, $d1, $25,
    $72, $f8, $f6, $64, $86, $68, $98, $16, $d4, $a4, $5c, $cc, $5d, $65, $b6, $92,
    $6c, $70, $48, $50, $fd, $ed, $b9, $da, $5e, $15, $46, $57, $a7, $8d, $9d, $84,
    $90, $d8, $ab, $00, $8c, $bc, $d3, $0a, $f7, $e4, $58, $05, $b8, $b3, $45, $06,
    $d0, $2c, $1e, $8f, $ca, $3f, $0f, $02, $c1, $af, $bd, $03, $01, $13, $8a, $6b,
    $3a, $91, $11, $41, $4f, $67, $dc, $ea, $97, $f2, $cf, $ce, $f0, $b4, $e6, $73,
    $96, $ac, $74, $22, $e7, $ad, $35, $85, $e2, $f9, $37, $e8, $1c, $75, $df, $6e,
    $47, $f1, $1a, $71, $1d, $29, $c5, $89, $6f, $b7, $62, $0e, $aa, $18, $be, $1b,
    $fc, $56, $3e, $4b, $c6, $d2, $79, $20, $9a, $db, $c0, $fe, $78, $cd, $5a, $f4,
    $1f, $dd, $a8, $33, $88, $07, $c7, $31, $b1, $12, $10, $59, $27, $80, $ec, $5f,
    $60, $51, $7f, $a9, $19, $b5, $4a, $0d, $2d, $e5, $7a, $9f, $93, $c9, $9c, $ef,
    $a0, $e0, $3b, $4d, $ae, $2a, $f5, $b0, $c8, $eb, $bb, $3c, $83, $53, $99, $61,
    $17, $2b, $04, $7e, $ba, $77, $d6, $26, $e1, $69, $14, $63, $55, $21, $0c, $7d
    );



{ AES parameters }

function AES_Nr(const KeySize: Integer): Integer; // Number of rounds
begin
  case KeySize of
    128 : Result := 10;
    192 : Result := 12;
    256 : Result := 14;
  else
    raise ECipher.Create(CipherError_InvalidKeySize, 'Invalid AES key length');
  end;
end;



{ AES context }

procedure AESContextInit(var Context: TAESContext; const KeySize: Integer);
begin
  if (KeySize <> 128) and
     (KeySize <> 192) and
     (KeySize <> 256) then
    raise ECipher.Create(CipherError_InvalidKeySize, 'Invalid AES key length');
  FillChar(Context, SizeOf(Context), 0);
  Context.Nk := KeySize div 32;
  Context.Nr := AES_Nr(KeySize);
end;

procedure AESContextFinalise(var Context: TAESContext);
begin
  SecureClear(Context, SizeOf(Context));
end;



{ AES transformations }

{$R-,Q-}

procedure SubBytes(var State: TAESState);
var R : Byte;
    P : PByte;
begin
  // assumes AES_Nb = 4
  for R := 0 to 3 do
    begin
      P := @State[R, 0];
      P^ := AES_S[P^];
      Inc(P);
      P^ := AES_S[P^];
      Inc(P);
      P^ := AES_S[P^];
      Inc(P);
      P^ := AES_S[P^];
    end;
end;

procedure InvSubBytes(var State: TAESState);
var R : Byte;
    P : PByte;
begin
  // assumes AES_Nb = 4
  for R := 0 to 3 do
    begin
      P := @State[R, 0];
      P^ := AES_InvS[P^];
      Inc(P);
      P^ := AES_InvS[P^];
      Inc(P);
      P^ := AES_InvS[P^];
      Inc(P);
      P^ := AES_InvS[P^];
    end;
end;

procedure ShiftRows(var State: TAESState);
// assumes AES_Nb = 4
var
  // T : Word32;
  T0, T1, T2, T3 : Byte;
begin
{
  T := PWord32(@State[1])^;
  T := (T shr 8) or Word32(T shl 24);
  PWord32(@State[1])^ := T;
}
  T0 := State[1, 1];
  T1 := State[1, 2];
  T2 := State[1, 3];
  T3 := State[1, 0];
  State[1, 0] := T0;
  State[1, 1] := T1;
  State[1, 2] := T2;
  State[1, 3] := T3;

{
  T := PWord32(@State[2])^;
  T := (T shr 16) or Word32(T shl 16);
  PWord32(@State[2])^ := T;
}
  T0 := State[2, 2];
  T1 := State[2, 3];
  T2 := State[2, 0];
  T3 := State[2, 1];
  State[2, 0] := T0;
  State[2, 1] := T1;
  State[2, 2] := T2;
  State[2, 3] := T3;

{
  T := PWord32(@State[3])^;
  T := (T shr 24) or Word32(T shl 8);
  PWord32(@State[3])^ := T;
}
  T0 := State[3, 3];
  T1 := State[3, 0];
  T2 := State[3, 1];
  T3 := State[3, 2];
  State[3, 0] := T0;
  State[3, 1] := T1;
  State[3, 2] := T2;
  State[3, 3] := T3;
end;

procedure InvShiftRows(var State: TAESState);
// assumes AES_Nb = 4
var
  T0, T1, T2, T3 : Byte;
begin
  T1 := State[1, 0];
  T2 := State[1, 1];
  T3 := State[1, 2];
  T0 := State[1, 3];
  State[1, 0] := T0;
  State[1, 1] := T1;
  State[1, 2] := T2;
  State[1, 3] := T3;

  T2 := State[2, 0];
  T3 := State[2, 1];
  T0 := State[2, 2];
  T1 := State[2, 3];
  State[2, 0] := T0;
  State[2, 1] := T1;
  State[2, 2] := T2;
  State[2, 3] := T3;

  T3 := State[3, 0];
  T0 := State[3, 1];
  T1 := State[3, 2];
  T2 := State[3, 3];
  State[3, 0] := T0;
  State[3, 1] := T1;
  State[3, 2] := T2;
  State[3, 3] := T3;
end;

const
  FFLog : array[Byte] of Byte = (
    $00, $00, $19, $01, $32, $02, $1a, $c6, $4b, $c7, $1b, $68, $33, $ee, $df, $03,
    $64, $04, $e0, $0e, $34, $8d, $81, $ef, $4c, $71, $08, $c8, $f8, $69, $1c, $c1,
    $7d, $c2, $1d, $b5, $f9, $b9, $27, $6a, $4d, $e4, $a6, $72, $9a, $c9, $09, $78,
    $65, $2f, $8a, $05, $21, $0f, $e1, $24, $12, $f0, $82, $45, $35, $93, $da, $8e,
    $96, $8f, $db, $bd, $36, $d0, $ce, $94, $13, $5c, $d2, $f1, $40, $46, $83, $38,
    $66, $dd, $fd, $30, $bf, $06, $8b, $62, $b3, $25, $e2, $98, $22, $88, $91, $10,
    $7e, $6e, $48, $c3, $a3, $b6, $1e, $42, $3a, $6b, $28, $54, $fa, $85, $3d, $ba,
    $2b, $79, $0a, $15, $9b, $9f, $5e, $ca, $4e, $d4, $ac, $e5, $f3, $73, $a7, $57,
    $af, $58, $a8, $50, $f4, $ea, $d6, $74, $4f, $ae, $e9, $d5, $e7, $e6, $ad, $e8,
    $2c, $d7, $75, $7a, $eb, $16, $0b, $f5, $59, $cb, $5f, $b0, $9c, $a9, $51, $a0,
    $7f, $0c, $f6, $6f, $17, $c4, $49, $ec, $d8, $43, $1f, $2d, $a4, $76, $7b, $b7,
    $cc, $bb, $3e, $5a, $fb, $60, $b1, $86, $3b, $52, $a1, $6c, $aa, $55, $29, $9d,
    $97, $b2, $87, $90, $61, $be, $dc, $fc, $bc, $95, $cf, $cd, $37, $3f, $5b, $d1,
    $53, $39, $84, $3c, $41, $a2, $6d, $47, $14, $2a, $9e, $5d, $56, $f2, $d3, $ab,
    $44, $11, $92, $d9, $23, $20, $2e, $89, $b4, $7c, $b8, $26, $77, $99, $e3, $a5,
    $67, $4a, $ed, $de, $c5, $31, $fe, $18, $0d, $63, $8c, $80, $c0, $f7, $70, $07);
    
  FFPow : array[Byte] of Byte = (
    $01, $03, $05, $0f, $11, $33, $55, $ff, $1a, $2e, $72, $96, $a1, $f8, $13, $35,
    $5f, $e1, $38, $48, $d8, $73, $95, $a4, $f7, $02, $06, $0a, $1e, $22, $66, $aa,
    $e5, $34, $5c, $e4, $37, $59, $eb, $26, $6a, $be, $d9, $70, $90, $ab, $e6, $31,
    $53, $f5, $04, $0c, $14, $3c, $44, $cc, $4f, $d1, $68, $b8, $d3, $6e, $b2, $cd,
    $4c, $d4, $67, $a9, $e0, $3b, $4d, $d7, $62, $a6, $f1, $08, $18, $28, $78, $88,
    $83, $9e, $b9, $d0, $6b, $bd, $dc, $7f, $81, $98, $b3, $ce, $49, $db, $76, $9a,
    $b5, $c4, $57, $f9, $10, $30, $50, $f0, $0b, $1d, $27, $69, $bb, $d6, $61, $a3,
    $fe, $19, $2b, $7d, $87, $92, $ad, $ec, $2f, $71, $93, $ae, $e9, $20, $60, $a0,
    $fb, $16, $3a, $4e, $d2, $6d, $b7, $c2, $5d, $e7, $32, $56, $fa, $15, $3f, $41,
    $c3, $5e, $e2, $3d, $47, $c9, $40, $c0, $5b, $ed, $2c, $74, $9c, $bf, $da, $75,
    $9f, $ba, $d5, $64, $ac, $ef, $2a, $7e, $82, $9d, $bc, $df, $7a, $8e, $89, $80,
    $9b, $b6, $c1, $58, $e8, $23, $65, $af, $ea, $25, $6f, $b1, $c8, $43, $c5, $54,
    $fc, $1f, $21, $63, $a5, $f4, $07, $09, $1b, $2d, $77, $99, $b0, $cb, $46, $ca,
    $45, $cf, $4a, $de, $79, $8b, $86, $91, $a8, $e3, $3e, $42, $c6, $51, $f3, $0e,
    $12, $36, $5a, $ee, $29, $7b, $8d, $8c, $8f, $8a, $85, $94, $a7, $f2, $0d, $17,
    $39, $4b, $dd, $7c, $84, $97, $a2, $fd, $1c, $24, $6c, $b4, $c7, $52, $f6, $01);

function FFmul(const A, B: Byte): Byte; {$IFDEF UseInline}inline;{$ENDIF}
var
  T : Word;
begin
  if (A <> 0) and (B <> 0) then
    begin
      T := FFLog[A] + FFLog[B];
      if T >= 255 then
        Dec(T, 255);
      Result := FFPow[T];
    end
  else
    Result := 0;
end;

function FFmul2(const B: Byte): Byte; {$IFDEF UseInline}inline;{$ENDIF}
var
  T : Word;
begin
  if B <> 0 then
    begin
      T := $19 + FFLog[B];
      if T >= 255 then
        Dec(T, 255);
      Result := FFPow[T];
    end
  else
    Result := 0;
end;

function FFmul3(const B: Byte): Byte; {$IFDEF UseInline}inline;{$ENDIF}
var
  T : Word;
begin
  if B <> 0 then
    begin
      T := $01 + FFLog[B];
      if T >= 255 then
        Dec(T, 255);
      Result := FFPow[T];
    end
  else
    Result := 0;
end;

function FFmulE(const B: Byte): Byte; {$IFDEF UseInline}inline;{$ENDIF}
var
  T : Word;
begin
  if B <> 0 then
    begin
      T := $df + FFLog[B];
      if T >= 255 then
        Dec(T, 255);
      Result := FFPow[T];
    end
  else
    Result := 0;
end;

function FFmulB(const B: Byte): Byte; {$IFDEF UseInline}inline;{$ENDIF}
var
  T : Word;
begin
  if B <> 0 then
    begin
      T := $68 + FFLog[B];
      if T >= 255 then
        Dec(T, 255);
      Result := FFPow[T];
    end
  else
    Result := 0;
end;

function FFmulD(const B: Byte): Byte; {$IFDEF UseInline}inline;{$ENDIF}
var
  T : Word;
begin
  if B <> 0 then
    begin
      T := $ee + FFLog[B];
      if T >= 255 then
        Dec(T, 255);
      Result := FFPow[T];
    end
  else
    Result := 0;
end;

function FFmul9(const B: Byte): Byte; {$IFDEF UseInline}inline;{$ENDIF}
var
  T : Word;
begin
  if B <> 0 then
    begin
      T := $c7 + FFLog[B];
      if T >= 255 then
        Dec(T, 255);
      Result := FFPow[T];
    end
  else
    Result := 0;
end;

procedure MixColumns(var State: TAESState);
var
  T0, T1, T2, T3 : Byte;
  C : Byte;
begin
  for C := 0 to AES_Nb - 1 do
    begin
      T0 := State[0, C];
      T1 := State[1, C];
      T2 := State[2, C];
      T3 := State[3, C];
      State[0, C] := FFmul2(T0) xor FFmul3(T1) xor T2 xor T3;
      State[1, C] := FFmul2(T1) xor FFmul3(T2) xor T3 xor T0;
      State[2, C] := FFmul2(T2) xor FFmul3(T3) xor T0 xor T1;
      State[3, C] := FFmul2(T3) xor FFmul3(T0) xor T1 xor T2;
    end;
end;

procedure InvMixColumns(var State: TAESState);
var
  T0, T1, T2, T3 : Byte;
  C : Byte;
begin
  for C := 0 to AES_Nb - 1 do
    begin
      T0 := State[0, C];
      T1 := State[1, C];
      T2 := State[2, C];
      T3 := State[3, C];
      State[0, C] := FFmulE(T0) xor FFmulB(T1) xor FFmulD(T2) xor FFmul9(T3);
      State[1, C] := FFmulE(T1) xor FFmulB(T2) xor FFmulD(T3) xor FFmul9(T0);
      State[2, C] := FFmulE(T2) xor FFmulB(T3) xor FFmulD(T0) xor FFmul9(T1);
      State[3, C] := FFmulE(T3) xor FFmulB(T0) xor FFmulD(T1) xor FFmul9(T2);
    end;
end;

function xbyte(const R: Byte; const K: Word32): Byte; {$IFDEF UseInline}inline;{$ENDIF}
begin
  Result := Byte((K shr (R * 8)) and $FF);
end;

procedure XorRoundKey(var State: TAESState; const Rk: TAESKeySchedule; const RkIdx: Integer);
var C : Byte;
    P : PByte;
    RkP : PWord32;
    RkC : Word32;
begin
  RkP := @Rk[RkIdx];
  for C := 0 to AES_Nb - 1 do
    begin
      RkC := RkP^;
      P := @State[0, C];
      P^ := P^ xor xbyte(0, RkC);
      P := @State[1, C];
      P^ := P^ xor xbyte(1, RkC);
      P := @State[2, C];
      P^ := P^ xor xbyte(2, RkC);
      P := @State[3, C];
      P^ := P^ xor xbyte(3, RkC);
      Inc(RkP);
    end;
end;



{ AES key expansion }

function SubWord(const A: Word32): Word32; {$IFDEF UseInline}inline;{$ENDIF}
begin
  Result :=
       AES_S[Byte (A and $000000FF)] or
      (AES_S[Byte((A and $0000FF00) shr 8)]  shl 8)  or
      (AES_S[Byte((A and $00FF0000) shr 16)] shl 16) or
      (AES_S[Byte((A and $FF000000) shr 24)] shl 24);
end;

function RotWord(const A: Word32): Word32; {$IFDEF UseInline}inline;{$ENDIF}
begin
  Result :=
      ((A and $FFFFFF00) shr 8) or
      ((A and $000000FF) shl 24);
end;

const
  AES_Rcon : array[Byte] of Byte = (
    $8d, $01, $02, $04, $08, $10, $20, $40, $80, $1b, $36, $6c, $d8, $ab, $4d, $9a,
    $2f, $5e, $bc, $63, $c6, $97, $35, $6a, $d4, $b3, $7d, $fa, $ef, $c5, $91, $39,
    $72, $e4, $d3, $bd, $61, $c2, $9f, $25, $4a, $94, $33, $66, $cc, $83, $1d, $3a,
    $74, $e8, $cb, $8d, $01, $02, $04, $08, $10, $20, $40, $80, $1b, $36, $6c, $d8,
    $ab, $4d, $9a, $2f, $5e, $bc, $63, $c6, $97, $35, $6a, $d4, $b3, $7d, $fa, $ef,
    $c5, $91, $39, $72, $e4, $d3, $bd, $61, $c2, $9f, $25, $4a, $94, $33, $66, $cc,
    $83, $1d, $3a, $74, $e8, $cb, $8d, $01, $02, $04, $08, $10, $20, $40, $80, $1b,
    $36, $6c, $d8, $ab, $4d, $9a, $2f, $5e, $bc, $63, $c6, $97, $35, $6a, $d4, $b3,
    $7d, $fa, $ef, $c5, $91, $39, $72, $e4, $d3, $bd, $61, $c2, $9f, $25, $4a, $94,
    $33, $66, $cc, $83, $1d, $3a, $74, $e8, $cb, $8d, $01, $02, $04, $08, $10, $20,
    $40, $80, $1b, $36, $6c, $d8, $ab, $4d, $9a, $2f, $5e, $bc, $63, $c6, $97, $35,
    $6a, $d4, $b3, $7d, $fa, $ef, $c5, $91, $39, $72, $e4, $d3, $bd, $61, $c2, $9f,
    $25, $4a, $94, $33, $66, $cc, $83, $1d, $3a, $74, $e8, $cb, $8d, $01, $02, $04,
    $08, $10, $20, $40, $80, $1b, $36, $6c, $d8, $ab, $4d, $9a, $2f, $5e, $bc, $63,
    $c6, $97, $35, $6a, $d4, $b3, $7d, $fa, $ef, $c5, $91, $39, $72, $e4, $d3, $bd,
    $61, $c2, $9f, $25, $4a, $94, $33, $66, $cc, $83, $1d, $3a, $74, $e8, $cb, $00);

procedure AESKeyExpansion(var Context: TAESContext; const KeyBuf; const KeyBufSize: Integer);
var Nk : Byte;
    I : Integer;
    T : Word32;
    P : PWord32;
begin
  Nk := Context.Nk;
  if KeyBufSize <> Nk * 4 then
    raise ECipher.Create(CipherError_InvalidBufferSize, 'Invalid key size');
  P := @KeyBuf;
  for I := 0 to Nk - 1 do
    begin
      Context.W[I] := P^;
      Inc(P);
    end;
  for I := Nk to AES_Nb * (Context.Nr + 1) - 1 do
    begin
      T := Context.W[I - 1];
      if I mod Nk = 0 then
        begin
          T := RotWord(T);
          T := SubWord(T);
          T := T xor AES_Rcon[I div Nk];
        end else
      if (Nk = 8) and (I mod Nk = 4) then
        T := SubWord(T);
      Context.W[I] := Context.W[I - Nk] xor T;
    end;
end;



{ AES cipher and inverse cipher }

procedure InBufToState(var Context: TAESContext; const InBuf; const InBufSize: Integer);
var R, C : Byte;
    P : PByte;
begin
  if InBufSize <> AES_Nb * 4 then
    raise ECipher.Create(CipherError_InvalidBufferSize, 'Invalid buffer size');
  for R := 0 to 3 do
    for C := 0 to AES_Nb - 1 do
      begin
        P := @InBuf;
        Inc(P, R + 4 * C);
        Context.State[R, C] := P^;
      end;
end;

procedure StateToOutBuf(var Context: TAESContext; var OutBuf; const OutBufSize: Integer);
var R, C : Byte;
    P : PByte;
begin
  if OutBufSize < AES_Nb * 4 then
    raise ECipher.Create(CipherError_InvalidBufferSize, 'Invalid buffer size');
  for C := 0 to AES_Nb - 1 do
    for R := 0 to 3 do
      begin
        P := @OutBuf;
        Inc(P, R + 4 * C);
        P^ := Context.State[R, C];
      end;
end;

procedure AESCipher(var Context: TAESContext;
          const InBuf; const InBufSize: Integer;
          var OutBuf; const OutBufSize: Integer);
var R : Integer;
begin
  InBufToState(Context, InBuf, InBufSize);
  XorRoundKey(Context.State, Context.W, 0);
  for R := 1 to Context.Nr - 1 do
    begin
      SubBytes(Context.State);
      ShiftRows(Context.State);
      MixColumns(Context.State);
      XorRoundKey(Context.State, Context.W, R * AES_Nb);
    end;
  SubBytes(Context.State);
  ShiftRows(Context.State);
  XorRoundKey(Context.State, Context.W, Context.Nr * AES_Nb);
  StateToOutBuf(Context, OutBuf, OutBufSize);
end;

procedure AESInvCipher(var Context: TAESContext;
          const InBuf; const InBufSize: Integer;
          var OutBuf; const OutBufSize: Integer);
var R : Integer;
begin
  InBufToState(Context, InBuf, InBufSize);
  XorRoundKey(Context.State, Context.W, Context.Nr * AES_Nb);
  for R := Context.Nr - 1 downto 1 do
    begin
      InvShiftRows(Context.State);
      InvSubBytes(Context.State);
      XorRoundKey(Context.State, Context.W, R * AES_Nb);
      InvMixColumns(Context.State);
    end;
  InvShiftRows(Context.State);
  InvSubBytes(Context.State);
  XorRoundKey(Context.State, Context.W, 0);
  StateToOutBuf(Context, OutBuf, OutBufSize);
end;



{ AES encrypt and decrypt }

procedure AESInit(var Context: TAESContext;
          const KeySize: Integer;
          const KeyBuf; const KeyBufSize: Integer);
begin
  AESContextInit(Context, KeySize);
  AESKeyExpansion(Context, KeyBuf, KeyBufSize);
end;

procedure AESEncryptBlock(
          var Context: TAESContext;
          const DataBuf; const DataBufSize: Integer;
          var CipherBuf; const CipherBufSize: Integer);
begin
  AESCipher(Context, DataBuf, DataBufSize, CipherBuf, CipherBufSize)
end;

procedure AESDecryptBlock(
          var Context: TAESContext;
          const DataBuf; const DataBufSize: Integer;
          var CipherBuf; const CipherBufSize: Integer);
begin
  AESInvCipher(Context, DataBuf, DataBufSize, CipherBuf, CipherBufSize)
end;

procedure AESFinalise(var Context: TAESContext);
begin
  AESContextFinalise(Context);
end;

procedure AESEncrypt(
          const KeySize: Integer;
          const KeyBuf; const KeyBufSize: Integer;
          const DataBuf; const DataBufSize: Integer;
          var CipherBuf; const CipherBufSize: Integer);
var Context: TAESContext;
begin
  AESContextInit(Context, KeySize);
  AESKeyExpansion(Context, KeyBuf, KeyBufSize);
  AESCipher(Context, DataBuf, DataBufSize, CipherBuf, CipherBufSize);
  AESContextFinalise(Context);
end;

procedure AESDecrypt(
          const KeySize: Integer;
          const KeyBuf; const KeyBufSize: Integer;
          const DataBuf; const DataBufSize: Integer;
          var CipherBuf; const CipherBufSize: Integer);
var Context: TAESContext;
begin
  AESContextInit(Context, KeySize);
  AESKeyExpansion(Context, KeyBuf, KeyBufSize);
  AESInvCipher(Context, DataBuf, DataBufSize, CipherBuf, CipherBufSize);
  AESContextFinalise(Context);
end;



{                                                                              }
{ Test cases                                                                   }
{                                                                              }
{$IFDEF CIPHER_TEST}
{$ASSERTIONS ON}
procedure Test;
var K, T, B : RawByteString;
begin
  K := RawByteString(#$0f#$15#$71#$c9#$47#$d9#$e8#$59#$0c#$b7#$ad#$d6#$af#$7f#$67#$98);
  T := '1234567890123456';
  SetLength(B, 16);
  AESEncrypt(128, K[1], Length(K), T[1], Length(T), B[1], Length(B));
  Assert(B = #$2f#$7d#$76#$42#$5e#$bb#$85#$e4#$f2#$e7#$b0#$08#$68#$bf#$0f#$ce);
  T := '00000000000000000';
  AESDecrypt(128, K[1], Length(K), B[1], Length(B), T[1], Length(T));
  Assert(T = '12345678901234560');
end;
{$ENDIF}



end.

