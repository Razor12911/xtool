{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals 5.00                                        }
{   File name:        flcCipherRandom.pas                                      }
{   File version:     5.06                                                     }
{   Description:      Cipher random                                            }
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
{ Revision history:                                                            }
{                                                                              }
{   2010/12/17  4.01  Initial version.                                         }
{   2013/09/25  4.02  UnicodeString version.                                   }
{   2015/05/05  4.03  Multiple PRNGs and PRSS and SHA512 hash in random block  }
{                     generator.                                               }
{   2016/01/09  5.04  Revised for Fundamentals 5.                              }
{   2019/06/06  5.05  SecureRandomBytes function.                              }
{   2020/02/13  5.06  Remove MD5 from generator.                               }
{                     Use 8 random bits in generator for each secure           }
{                     random bit.                                              }
{                                                                              }
{******************************************************************************}

{$INCLUDE flcCipher.inc}

unit flcCipherRandom;

interface

uses
  { System }
  SysUtils,

  { Fundamentals }
  flcStdTypes;



procedure SecureRandomBuf(var Buf; const Size: Integer);

function  SecureRandomBytes(const Size: Integer): TBytes;

function  SecureRandomStrA(const Size: Integer): RawByteString;

function  SecureRandomHexStr(const Digits: Integer; const UpperCase: Boolean = True): String;
function  SecureRandomHexStrB(const Digits: Integer; const UpperCase: Boolean = True): RawByteString;
function  SecureRandomHexStrU(const Digits: Integer; const UpperCase: Boolean = True): UnicodeString;

function  SecureRandomWord32: Word32;



implementation

uses
  { Fundamentals }
  flcUtils,
  flcRandom,
  flcHash;



const
  SecureRandomBlockBits = 128;
  SecureRandomBlockSize = SecureRandomBlockBits div 8; // 16 bytes

type
  TSecureRandomBlock = array[0..SecureRandomBlockSize - 1] of Byte;
  PSecureRandomBlock = ^TSecureRandomBlock;

// produces a block of SecureRandomBlockSize bytes of secure random material
procedure SecureRandomBlockGenerator(var Block: TSecureRandomBlock);
const
  RandomDataBits = SecureRandomBlockBits * 8; // 1024 bits (128 bytes)
  RandomDataLen  = RandomDataBits div 32;     // 32 * Word32
var
  I     : Integer;
  RData : array[0..RandomDataLen - 1] of Word32;
  S32   : Word32;
  H512  : T512BitDigest;
  H256  : T256BitDigest;
begin
  try
    // initialise 1024 bits with multiple Pseudo Random Numbers Generators (PRNG)
    // and Pseudo Random System State (PRSS)
    FillChar(RData, SizeOf(RData), $FF);
    S32 := RandomSeed32;
    RData[0] := RData[0] xor S32;
    for I := 0 to RandomDataLen - 1 do
      RData[I] := RData[I] xor RandomUniform32;
    for I := 0 to RandomDataLen - 1 do
      RData[I] := RData[I] xor urnRandom32;
    S32 := RandomSeed32;
    RData[RandomDataLen - 1] := RData[RandomDataLen - 1] xor S32;
    // hash 1024 bits using SHA512 into 512 bits
    H512 := CalcSHA512(RData, SizeOf(RData));
    // hash 512 bits using SHA256 into 256 bits
    H256 := CalcSHA256(H512, SizeOf(T512BitDigest));
    // move 128 bits to secure random block
    Assert(SizeOf(H256) >= SecureRandomBlockSize);
    Move(H256, Block, SecureRandomBlockSize);
  finally
    SecureClear(H256, SizeOf(T256BitDigest));
    SecureClear(H512, SizeOf(T512BitDigest));
    SecureClear(RData, SizeOf(RData));
  end;
end;

procedure SecureRandomBuf(var Buf; const Size: Integer);
var
  P : PSecureRandomBlock;
  L : Integer;
  B : TSecureRandomBlock;
begin
  P := @Buf;
  L := Size;
  while L >= SecureRandomBlockSize do
    begin
      SecureRandomBlockGenerator(P^);
      Inc(P);
      Dec(L, SecureRandomBlockSize);
    end;
  if L > 0 then
    begin
      SecureRandomBlockGenerator(B);
      Move(B, P^, L);
      SecureClear(B, SecureRandomBlockSize);
    end;
end;

function SecureRandomBytes(const Size: Integer): TBytes;
begin
  SetLength(Result, Size);
  if Size <= 0 then
    exit;
  SecureRandomBuf(Pointer(Result)^, Size);
end;

function SecureRandomStrA(const Size: Integer): RawByteString;
begin
  SetLength(Result, Size);
  if Size <= 0 then
    exit;
  SecureRandomBuf(Result[1], Size);
end;

function SecureRandomHexStr(const Digits: Integer; const UpperCase: Boolean = True): String;
var
  B    : TSecureRandomBlock;
  S, T : String;
  L, N : Integer;
  P    : PWord32;
  Q    : PChar;
begin
  if Digits <= 0 then
    begin
      Result := '';
      exit;
    end;
  SetLength(S, Digits);
  Q := PChar(S);
  L := Digits;
  while L >= 8 do
    begin
      SecureRandomBlockGenerator(B);
      P := @B;
      N := SecureRandomBlockSize div 4;
      while (L >= 8) and (N > 0) do
        begin
          T := Word32ToHex(P^, 8, UpperCase);
          Move(PChar(T)^, Q^, 8 * SizeOf(Char));
          SecureClearStr(T);
          Inc(Q, 8);
          Dec(N);
          Inc(P);
          Dec(L, 8);
        end;
    end;
  if L > 0 then
    begin
      SecureRandomBlockGenerator(B);
      P := @B;
      T := Word32ToHex(P^, L, UpperCase);
      Move(PChar(T)^, Q^, L * SizeOf(Char));
      SecureClearStr(T);
    end;
  SecureClear(B, SecureRandomBlockSize);
  Result := S;
end;

function SecureRandomHexStrB(const Digits: Integer; const UpperCase: Boolean): RawByteString;
var
  B    : TSecureRandomBlock;
  S, T : RawByteString;
  L, N : Integer;
  P    : PWord32;
  Q    : PByte;
begin
  if Digits <= 0 then
    begin
      Result := '';
      exit;
    end;
  SetLength(S, Digits);
  Q := PByte(S);
  L := Digits;
  while L >= 8 do
    begin
      SecureRandomBlockGenerator(B);
      P := @B;
      N := SecureRandomBlockSize div 4;
      while (L >= 8) and (N > 0) do
        begin
          T := Word32ToHexB(P^, 8, UpperCase);
          Move(PByte(T)^, Q^, 8);
          SecureClearStrB(T);
          Inc(Q, 8);
          Dec(N);
          Inc(P);
          Dec(L, 8);
        end;
    end;
  if L > 0 then
    begin
      SecureRandomBlockGenerator(B);
      P := @B;
      T := Word32ToHexB(P^, L, UpperCase);
      Move(PByte(T)^, Q^, L);
      SecureClearStrB(T);
    end;
  SecureClear(B, SecureRandomBlockSize);
  Result := S;
end;

function SecureRandomHexStrU(const Digits: Integer; const UpperCase: Boolean): UnicodeString;
var
  B    : TSecureRandomBlock;
  S, T : UnicodeString;
  L, N : Integer;
  P    : PWord32;
  Q    : PWideChar;
begin
  if Digits <= 0 then
    begin
      Result := '';
      exit;
    end;
  SetLength(S, Digits);
  Q := PWideChar(S);
  L := Digits;
  while L >= 8 do
    begin
      SecureRandomBlockGenerator(B);
      P := @B;
      N := SecureRandomBlockSize div 4;
      while (L >= 8) and (N > 0) do
        begin
          T := Word32ToHexU(P^, 8, UpperCase);
          Move(PWideChar(T)^, Q^, 8 * SizeOf(WideChar));
          SecureClear(T[1], 8 * SizeOf(WideChar));
          Inc(Q, 8);
          Dec(N);
          Inc(P);
          Dec(L, 8);
        end;
    end;
  if L > 0 then
    begin
      SecureRandomBlockGenerator(B);
      P := @B;
      T := Word32ToHexU(P^, L, UpperCase);
      Move(PWideChar(T)^, Q^, L * SizeOf(WideChar));
      SecureClear(T[1], 8 * SizeOf(WideChar));
    end;
  SecureClear(B, SecureRandomBlockSize);
  Result := S;
end;

function SecureRandomWord32: Word32;
var
  L : Word32;
begin
  SecureRandomBuf(L, SizeOf(Word32));
  Result := L;
  SecureClear(L, SizeOf(Word32));
end;



end.

