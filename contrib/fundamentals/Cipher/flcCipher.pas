{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals 5.00                                        }
{   File name:        flcCipher.pas                                            }
{   File version:     5.11                                                     }
{   Description:      Cipher library                                           }
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
{   2007/01/05  0.01  Initial version                                          }
{   2007/01/07  0.02  ECB and padding support                                  }
{   2008/06/15  0.03  CBC mode                                                 }
{   2008/06/17  0.04  CFB and OFB modes                                        }
{   2010/11/18  4.05  Revision                                                 }
{   2010/12/16  4.06  AES cipher                                               }
{   2011/08/09  4.07  Revision                                                 }
{   2015/03/21  4.08  Revision                                                 }
{   2016/01/09  5.09  Revised for Fundamentals 5.                              }
{   2018/06/17  5.10  Type changes.                                            }
{   2019/06/09  5.11  Tests for Triple-DES-EDE-3.                              }
{                                                                              }
{ Supported compilers:                                                         }
{                                                                              }
{   Delphi 5 Win32                      4.08  2015/03/31                       }
{   Delphi 7 Win32                      5.10  2019/02/24                       }
{   Delphi XE7 Win32                    5.09  2016/01/09                       }
{   Delphi XE7 Win64                    5.09  2016/01/09                       }
{   Delphi 10 Win32                     5.09  2016/01/09                       }
{   Delphi 10 Win64                     5.09  2016/01/09                       }
{                                                                              }
{ Todo:                                                                        }
{ - Split out tests                                                            }
{******************************************************************************}

{.DEFINE Cipher_SupportRSA}

{$INCLUDE flcCipher.inc}

unit flcCipher;

interface

uses
  { System }
  SysUtils,
  { Fundamentals }
  flcStdTypes,
  flcUtils,
  { Cipher }
  flcCipherUtils,
  flcCipherRC2,
  flcCipherRC4,
  flcCipherDES,
  flcCipherAES
  {$IFDEF Cipher_SupportRSA},
  flcCipherRSA
  {$ENDIF};



{                                                                              }
{ Cipher information                                                           }
{                                                                              }
type
  TCipherType = (
    ctNone,
    ctRC2,            // RC2
    ctRC4,            // RC4
    ctDES,            // DES
    ctDES40,          // DES-40
    ctTripleDESEDE,   // Triple-DES-EDE 2 keys
    ctTripleDES3EDE,  // Triple-DES-EDE 3 keys
    ctTripleDES3EEE,  // Triple-DES-EEE 3 keys
    ctAES,            // AES-128, AES-192, AES-256
    ctRSAOAEP1024     // RSA-OAEP-1024
   );

  TCipherOperation = (
    coEncrypt,
    coDecrypt);

  TCipherInitFunc =
    procedure (const Operation: TCipherOperation;
               const KeyBuffer: Pointer; const KeyBufferSize: Integer;
               const KeyBits: Integer;
               const ContextBuffer: Pointer);

  TCipherBufferFunc =
    procedure (const Context, Data: Pointer; const DataSize: Integer);

  TCipherFinaliseFunc =
    procedure (const Context: Pointer);

  TCipherBufferMode = (
    cbmStream,
    cbmMultipleBlocks,
    cbmSingleBlock);

  TCipherInfo = record
    ValidCipher  : Boolean;
    ShortName    : RawByteString;
    BufferMode   : TCipherBufferMode;
    InBlockSize  : Integer;
    OutBlockSize : Integer;
    KeyBitsMin   : Integer;
    KeyBitsMax   : Integer;
    ContextSize  : Integer;
    InitFunc     : TCipherInitFunc;
    EncryptFunc  : TCipherBufferFunc;
    DecryptFunc  : TCipherBufferFunc;
    FinaliseFunc : TCipherFinaliseFunc;
  end;
  PCipherInfo = ^TCipherInfo;

function GetCipherInfo(const Cipher: TCipherType): PCipherInfo;



{                                                                              }
{ Cipher                                                                       }
{                                                                              }
type
  TCipherMode = (
    cmECB,            // Electronic Codebook
    cmCBC,            // Cipher Block Chaining
    cmCFB,            // Cipher Feedback
    cmOFB             // Output Feedback
    );

  TCipherPadding = (
    cpNone,           // Padded with zeros (size of decrypted text may be longer
                      // than original text if original text size is not a
                      // multiple of cipher's block size; used with test cases)
    cpPadSizeByte,    // Padded with zeros and pad size byte
    cpPadPKCS5);      // Padded with pad size byte repeated

const
  MaxCipherContextSize = 384;

type
  TCipherState = record
    Operation  : TCipherOperation;
    Cipher     : TCipherType;
    Mode       : TCipherMode;
    Padding    : TCipherPadding;
    KeyBits    : Integer;
    CipherInfo : PCipherInfo;
    Context    : array[0..MaxCipherContextSize - 1] of Byte;
  end;

const
  CipherStateSize = SizeOf(TCipherState);



{                                                                              }
{ Encrypt                                                                      }
{                                                                              }
procedure EncryptInit(
          var State: TCipherState;
          const Cipher: TCipherType; const Mode: TCipherMode;
          const Padding: TCipherPadding;
          const KeyBits: Integer;
          const KeyBuffer: Pointer; const KeyBufferSize: Integer);

function  EncryptBuf(
          var State: TCipherState;
          const InputBuffer: Pointer; const InputBufferSize: Integer;
          const OutputBuffer: Pointer; const OutputBufferSize: Integer;
          const InitVectorBuffer: Pointer; const InitVectorBufferSize: Integer): Integer;

procedure EncryptFinalise(var State: TCipherState);

function  EncryptedSize(
          const Cipher: TCipherType; const Padding: TCipherPadding;
          const BufferSize: Integer): Integer;

function  Encrypt(
          const Cipher: TCipherType; const Mode: TCipherMode;
          const Padding: TCipherPadding;
          const KeyBits: Integer;
          const KeyBuffer: Pointer; const KeyBufferSize: Integer;
          const InputBuffer: Pointer; const InputBufferSize: Integer;
          const OutputBuffer: Pointer; const OutputBufferSize: Integer;
          const InitVectorBuffer: Pointer; const InitVectorBufferSize: Integer): Integer; overload;

function  Encrypt(
          const Cipher: TCipherType; const Mode: TCipherMode;
          const Padding: TCipherPadding;
          const KeyBits: Integer;
          const Key, Data, InitVector: RawByteString): RawByteString; overload;



{                                                                              }
{ Decrypt                                                                      }
{                                                                              }
procedure DecryptInit(
          var State: TCipherState;
          const Cipher: TCipherType; const Mode: TCipherMode;
          const Padding: TCipherPadding;
          const KeyBits: Integer;
          const KeyBuffer: Pointer; const KeyBufferSize: Integer);

function  DecryptBuf(
          var State: TCipherState;
          const Buffer: Pointer; const BufferSize: Integer;
          const InitVectorBuffer: Pointer; const InitVectorBufferSize: Integer): Integer;

procedure DecryptFinalise(var State: TCipherState);

function  Decrypt(
          const Cipher: TCipherType; const Mode: TCipherMode;
          const Padding: TCipherPadding;
          const KeyBits: Integer;
          const KeyBuffer: Pointer; const KeyBufferSize: Integer;
          const Buffer: Pointer; const BufferSize: Integer;
          const InitVectorBuffer: Pointer; const InitVectorBufferSize: Integer): Integer; overload;

function  Decrypt(
          const Cipher: TCipherType; const Mode: TCipherMode;
          const Padding: TCipherPadding;
          const KeyBits: Integer;
          const Key, Data, InitVector: RawByteString): RawByteString; overload;



{                                                                              }
{ Test cases                                                                   }
{                                                                              }
{$IFDEF CIPHER_TEST}
procedure Test;
{$ENDIF}



implementation

uses
  { Fundamentals }
  flcHugeInt,

  { Cipher }
  flcCipherRandom;



{                                                                              }
{ RC2 cipher                                                                   }
{                                                                              }
const
  RC2ContextSize = Sizeof(TRC2CipherKey);

procedure CipherRC2Init(const Operation: TCipherOperation;
    const KeyBuffer: Pointer; const KeyBufferSize: Integer;
    const KeyBits: Integer; const ContextBuffer: Pointer);
begin
  RC2Init(KeyBuffer^, KeyBufferSize, KeyBits, PRC2CipherKey(ContextBuffer)^);
end;

procedure CipherRC2EncryptBuffer(const Context, Data: Pointer; const DataSize: Integer);
begin
  Assert(DataSize = RC2BlockSize);
  RC2EncryptBlock(PRC2CipherKey(Context)^, PRC2Block(Data)^);
end;

procedure CipherRC2DecryptBuffer(const Context, Data: Pointer; const DataSize: Integer);
begin
  Assert(DataSize = RC2BlockSize);
  RC2DecryptBlock(PRC2CipherKey(Context)^, PRC2Block(Data)^);
end;



{                                                                              }
{ RC4 cipher                                                                   }
{                                                                              }
const
  RC4ContextSize = Sizeof(TRC4Context);

procedure CipherRC4Init(const Operation: TCipherOperation;
    const KeyBuffer: Pointer; const KeyBufferSize: Integer;
    const KeyBits: Integer; const ContextBuffer: Pointer);
begin
  RC4Init(KeyBuffer^, KeyBufferSize, PRC4Context(ContextBuffer)^);
end;

procedure CipherRC4Buffer(const Context, Data: Pointer; const DataSize: Integer);
begin
  RC4Buffer(PRC4Context(Context)^, Data^, DataSize);
end;



{                                                                              }
{ DES cipher                                                                   }
{                                                                              }
const
  DESContextSize = Sizeof(TDESContext);
  DESKeySize     = Sizeof(TDESKey);

procedure CipherDESInit(const Operation: TCipherOperation;
    const KeyBuffer: Pointer; const KeyBufferSize: Integer;
    const KeyBits: Integer; const ContextBuffer: Pointer);
begin
  if KeyBufferSize <> DESKeySize then
    raise ECipher.Create(CipherError_InvalidKeySize, 'Invalid DES key length');
  DESInit(Operation = coEncrypt, PDESKey(KeyBuffer)^, PDESContext(ContextBuffer)^);
end;

procedure CipherDESBuffer(const Context, Data: Pointer; const DataSize: Integer);
begin
  Assert(DataSize = DESBlockSize);
  DESBuffer(PDESContext(Context)^, PDESBlock(Data)^);
end;



{                                                                              }
{ DES-40 cipher                                                                }
{                                                                              }
const
  DES40ContextSize = DESContextSize;
  DES40KeySize     = DESKeySize;

procedure CipherDES40Init(const Operation: TCipherOperation;
    const KeyBuffer: Pointer; const KeyBufferSize: Integer;
    const KeyBits: Integer; const ContextBuffer: Pointer);
var DES40Key : TDESKey;
begin
  if KeyBufferSize <> DES40KeySize then
    raise ECipher.Create(CipherError_InvalidKeySize, 'Invalid DES key length');
  Move(KeyBuffer, DES40Key, DES40KeySize);
  DESKeyConvertToDES40(DES40Key);
  DESInit(Operation = coEncrypt, DES40Key, PDESContext(ContextBuffer)^);
end;

procedure CipherDES40Buffer(const Context, Data: Pointer; const DataSize: Integer);
begin
  Assert(DataSize = DESBlockSize);
  DESBuffer(PDESContext(Context)^, PDESBlock(Data)^);
end;



{                                                                              }
{ Triple-DES cipher                                                            }
{                                                                              }
const
  TripleDESContextSize = Sizeof(TTripleDESContext);

procedure CipherTripleDESInit(const Operation: TCipherOperation;
    const KeyBuffer: Pointer; const KeyBufferSize: Integer;
    const KeyBits: Integer; const ContextBuffer: Pointer);
begin
  if KeyBufferSize <> SizeOf(TTripleDESKey) then
    raise ECipher.Create(CipherError_InvalidKeySize, 'Invalid DES key length');
  TripleDESInit(Operation = coEncrypt, PTripleDESKey(KeyBuffer)^,
      PTripleDESContext(ContextBuffer)^);
end;

procedure CipherTripleDESBuffer(const Context, Data: Pointer; const DataSize: Integer);
begin
  Assert(DataSize = DESBlockSize);
  TripleDESBuffer(PTripleDESContext(Context)^, PDESBlock(Data)^);
end;



{                                                                              }
{ Triple-DES-3 cipher                                                          }
{                                                                              }
const
  TripleDES3ContextSize = Sizeof(TTripleDES3Context);

procedure CipherTripleDES3EDEInit(const Operation: TCipherOperation;
    const KeyBuffer: Pointer; const KeyBufferSize: Integer;
    const KeyBits: Integer; const ContextBuffer: Pointer);
begin
  if KeyBufferSize <> SizeOf(TTripleDES3Key) then
    raise ECipher.Create(CipherError_InvalidKeySize, 'Invalid DES key length');
  TripleDES3Init(Operation = coEncrypt, PTripleDES3Key(KeyBuffer)^, False,
      PTripleDES3Context(ContextBuffer)^);
end;

procedure CipherTripleDES3EEEInit(const Operation: TCipherOperation;
    const KeyBuffer: Pointer; const KeyBufferSize: Integer;
    const KeyBits: Integer; const ContextBuffer: Pointer);
begin
  if KeyBufferSize <> SizeOf(TTripleDES3Key) then
    raise ECipher.Create(CipherError_InvalidKeySize, 'Invalid DES key length');
  TripleDES3Init(Operation = coEncrypt, PTripleDES3Key(KeyBuffer)^, True,
      PTripleDES3Context(ContextBuffer)^);
end;

procedure CipherTripleDES3BufferEncrypt(const Context, Data: Pointer; const DataSize: Integer);
begin
  Assert(DataSize = DESBlockSize);
  TripleDES3BufferEncrypt(PTripleDES3Context(Context)^, PDESBlock(Data)^);
end;

procedure CipherTripleDES3BufferDecrypt(const Context, Data: Pointer; const DataSize: Integer);
begin
  Assert(DataSize = DESBlockSize);
  TripleDES3BufferDecrypt(PTripleDES3Context(Context)^, PDESBlock(Data)^);
end;



{                                                                              }
{ AES cipher                                                                   }
{                                                                              }
const
  AESContextSize = Sizeof(TAESContext);

procedure CipherAESInit(const Operation: TCipherOperation;
    const KeyBuffer: Pointer; const KeyBufferSize: Integer;
    const KeyBits: Integer; const ContextBuffer: Pointer);
begin
  AESInit(PAESContext(ContextBuffer)^, KeyBits, KeyBuffer^, KeyBufferSize);
end;

procedure CipherAESEncryptBuffer(const Context, Data: Pointer; const DataSize: Integer);
begin
  Assert(DataSize = AESBlockSize);
  AESEncryptBlock(PAESContext(Context)^, Data^, DataSize, Data^, DataSize);
end;

procedure CipherAESDecryptBuffer(const Context, Data: Pointer; const DataSize: Integer);
begin
  Assert(DataSize = AESBlockSize);
  AESDecryptBlock(PAESContext(Context)^, Data^, DataSize, Data^, DataSize);
end;

procedure CipherAESFinalise(const Context: Pointer);
begin
  AESFinalise(PAESContext(Context)^);
end;



{                                                                              }
{ RSA cipher                                                                   }
{ Note: Stub under development                                                 } 
{                                                                              }
{$IFDEF Cipher_SupportRSA}
type
  TRSAOAEP1024Context = packed record
    Encrypt    : Boolean;
    PublicKey  : TRSAPublicKey;
    PrivateKey : TRSAPrivateKey;
  end;
  PRSAOAEP1024Context = ^TRSAOAEP1024Context;

const
  RSAOAEP1024ContextSize = SizeOf(TRSAOAEP1024Context);

procedure CipherRSAOAEP1024Init(const Operation: TCipherOperation;
    const KeyBuffer: Pointer; const KeyBufferSize: Integer;
    const KeyBits: Integer; const ContextBuffer: Pointer);
const
  KeyPartBits = 1024;
  KeyPartSize = 1024 div 8;
var C : PRSAOAEP1024Context;
    P, Q : PByte;
begin
  C := ContextBuffer;
  C^.Encrypt := Operation = coEncrypt;
  RSAPublicKeyInit(C^.PublicKey);
  RSAPrivateKeyInit(C^.PrivateKey);
  if (KeyBufferSize <> KeyPartSize * 2) or (KeyBits <> KeyPartBits) then
    raise ECipher.Create(CipherError_InvalidKeySize, 'Invalid RSA key length');
  P := @KeyBuffer;
  Q := P;
  Inc(Q, KeyPartSize);
  if Operation = coEncrypt then
    RSAPublicKeyAssignBuf(C^.PublicKey, KeyPartBits, P^, KeyPartSize, Q^, KeyPartSize, False)
  else
    RSAPrivateKeyAssignBuf(C^.PrivateKey, KeyPartBits, P^, KeyPartSize, Q^, KeyPartSize, False);
end;

procedure CipherRSAOAEP1024EncryptBuffer(const Context, Data: Pointer; const DataSize: Integer);
var C : PRSAOAEP1024Context;
    B : array[0..127] of Byte;
begin
  C := Context;
  RSAEncrypt(rsaetOAEP, C^.PublicKey, Data, DataSize, B[0], SizeOf(B));
  Move(B[0], Data^, 128);
end;

procedure CipherRSAOAEP1024DecryptBuffer(const Context, Data: Pointer; const DataSize: Integer);
var C : PRSAOAEP1024Context;
    B : array[0..127] of Byte;
begin
  C := Context;
  RSADecrypt(rsaetOAEP, C^.PrivateKey, Data, DataSize, B[0], SizeOf(B));
  Move(B[0], Data^, 128);
end;

procedure CipherRSAOAEP1024Finalise(const Context: Pointer);
var C : PRSAOAEP1024Context;
begin
  C := Context;
  RSAPrivateKeyFinalise(C^.PrivateKey);
  RSAPublicKeyFinalise(C^.PublicKey);
end;
{$ENDIF}



{                                                                              }
{ Cipher mode buffer functions                                                 }
{                                                                              }
procedure XORBuffer(const Buffer1, Buffer2: Pointer; const BufferSize: Integer);
var I    : Integer;
    P, Q : PByte;
begin
  Assert(Assigned(Buffer1));
  Assert(Assigned(Buffer2));
  P := Buffer1;
  Q := Buffer2;
  for I := 0 to BufferSize - 1 do
    begin
      P^ := P^ xor Q^;
      Inc(P);
      Inc(Q);
    end;
end;

{ Cipher buffer                                                                }
procedure CipherBuffer(
    const Buffer: Pointer; const BufferSize: Integer;
    const CipherContext: Pointer;
    const CipherFunc: TCipherBufferFunc; const CipherBlockSize: Integer);
begin
  CipherFunc(CipherContext, @Buffer, BufferSize);
end;

{ ECB - Electronic codebook                                                    }
procedure ECBBuffer(
    const Buffer: Pointer; const BufferSize: Integer;
    const CipherContext: Pointer;
    const CipherFunc: TCipherBufferFunc; const CipherBlockSize: Integer);
var P    : PByte;
    L, M : Integer;
begin
  P := Buffer;
  L := BufferSize;
  while L > 0 do
    begin
      if L >= CipherBlockSize then
        M := CipherBlockSize
      else
        M := L;
      CipherFunc(CipherContext, P, M);
      Inc(P, M);
      Dec(L, M);
    end;
end;

{ CBC - Cipher-block chaining                                                  }
{ See http://en.wikipedia.org/wiki/Cipher_block_chaining                       }
const
  MaxCipherBlockSize = 4096;

procedure CBCEncode(
    const Buffer: Pointer; const BufferSize: Integer;
    const CipherContext: Pointer;
    const CipherFunc: TCipherBufferFunc; const CipherBlockSize: Integer;
    const InitVector: Pointer; const InitVectorBufferSize: Integer);
var P, F : PByte;
    L    : Integer;
    B    : array[0..MaxCipherBlockSize - 1] of Byte;
begin
  Assert(BufferSize > 0);
  Assert(CipherBlockSize <= MaxCipherBlockSize);
  Assert(Assigned(InitVector));
  Assert(InitVectorBufferSize = CipherBlockSize);
  P := Buffer;
  L := BufferSize;
  F := InitVector;
  while L >= CipherBlockSize do
    begin
      XORBuffer(P, F, CipherBlockSize);
      CipherFunc(CipherContext, P, CipherBlockSize);
      F := P;
      Dec(L, CipherBlockSize);
      Inc(P, CipherBlockSize);
    end;
  if L > 0 then
    begin
      Move(P^, B[0], L);
      FillChar(B[L], CipherBlockSize - L, 0);
      XORBuffer(@B[0], F, CipherBlockSize);
      CipherFunc(CipherContext, @B[0], L);
      Move(B[0], P^, L);
      SecureClear(B[0], CipherBlockSize);
    end;
end;

procedure CBCDecode(
    const Buffer: Pointer; const BufferSize: Integer;
    const CipherState: Pointer;
    const CipherFunc: TCipherBufferFunc; const CipherBlockSize: Integer;
    const InitVector: Pointer; const InitVectorBufferSize: Integer);
var P : PByte;
    L : Integer;
    B : array[0..MaxCipherBlockSize - 1] of Byte;
    C : array[0..MaxCipherBlockSize - 1] of Byte;
begin
  Assert(BufferSize > 0);
  Assert(CipherBlockSize <= MaxCipherBlockSize);
  Assert(Assigned(InitVector));
  Assert(InitVectorBufferSize = CipherBlockSize);
  P := Buffer;
  L := BufferSize;
  Move(InitVector^, B[0], CipherBlockSize);
  while L >= CipherBlockSize do
    begin
      Move(P^, C[0], CipherBlockSize);
      CipherFunc(CipherState, P, CipherBlockSize);
      XORBuffer(P, @B[0], CipherBlockSize);
      Move(C[0], B[0], CipherBlockSize);
      Dec(L, CipherBlockSize);
      Inc(P, CipherBlockSize);
    end;
  if L > 0 then
    begin
      Move(P^, C[0], L);
      FillChar(C[L], CipherBlockSize - L, 0);
      CipherFunc(CipherState, @C[0], L);
      XORBuffer(@C[0], @B[0], CipherBlockSize);
      Move(C[0], P^, L);
    end;
  SecureClear(B[0], CipherBlockSize);
  SecureClear(C[0], CipherBlockSize);
end;

{ CFB - Cipher FeedBack                                                        }
procedure CFBEncode(
    const Buffer: Pointer; const BufferSize: Integer;
    const CipherContext: Pointer;
    const CipherFunc: TCipherBufferFunc; const CipherBlockSize: Integer;
    const InitVector: Pointer; const InitVectorBufferSize: Integer);
var P    : PByte;
    L    : Integer;
    B, F : array[0..MaxCipherBlockSize - 1] of Byte;
begin
  Assert(BufferSize > 0);
  Assert(CipherBlockSize <= MaxCipherBlockSize);
  Assert(Assigned(InitVector));
  Assert(InitVectorBufferSize = CipherBlockSize);
  P := Buffer;
  L := BufferSize;
  Move(InitVector^, F[0], CipherBlockSize);
  while L >= 0 do
    begin
      Move(F[0], B[0], CipherBlockSize);
      CipherFunc(CipherContext, @B[0], CipherBlockSize);
      P^ := P^ xor B[0];
      Move(F[1], F[0], CipherBlockSize - 1);
      F[CipherBlockSize - 1] := P^;
      Dec(L);
      Inc(P);
    end;
  SecureClear(B[0], CipherBlockSize);
end;

procedure CFBDecode(
    const Buffer: Pointer; const BufferSize: Integer;
    const CipherContext: Pointer;
    const CipherFunc: TCipherBufferFunc; const CipherBlockSize: Integer;
    const InitVector: Pointer; const InitVectorBufferSize: Integer);
var P    : PByte;
    C    : Byte;
    L    : Integer;
    B, F : array[0..MaxCipherBlockSize - 1] of Byte;
begin
  Assert(BufferSize > 0);
  Assert(CipherBlockSize <= MaxCipherBlockSize);
  Assert(Assigned(InitVector));
  Assert(InitVectorBufferSize = CipherBlockSize);
  P := Buffer;
  L := BufferSize;
  Move(InitVector^, F[0], CipherBlockSize);
  while L >= 0 do
    begin
      Move(F[0], B[0], CipherBlockSize);
      CipherFunc(CipherContext, @B[0], CipherBlockSize);
      Move(F[1], F[0], CipherBlockSize - 1);
      C := P^;
      F[CipherBlockSize - 1] := C;
      P^ := C xor B[0];
      Dec(L);
      Inc(P);
    end;
  SecureClear(B[0], CipherBlockSize);
end;

{ OFB - Output FeedBack                                                        }
procedure OFBEncode(
    const Buffer: Pointer; const BufferSize: Integer;
    const CipherContext: Pointer;
    const CipherFunc: TCipherBufferFunc; const CipherBlockSize: Integer;
    const InitVector: Pointer; const InitVectorBufferSize: Integer);
var P    : PByte;
    L    : Integer;
    B, F : array[0..MaxCipherBlockSize - 1] of Byte;
begin
  Assert(BufferSize > 0);
  Assert(CipherBlockSize <= MaxCipherBlockSize);
  Assert(Assigned(InitVector));
  Assert(InitVectorBufferSize = CipherBlockSize);
  P := Buffer;
  L := BufferSize;
  Move(InitVector^, F[0], CipherBlockSize);
  while L >= 0 do
    begin
      Move(F[0], B[0], CipherBlockSize);
      CipherFunc(CipherContext, @B[0], CipherBlockSize);
      P^ := P^ xor B[0];
      Move(F[1], F[0], CipherBlockSize - 1);
      F[CipherBlockSize - 1] := B[0];
      Dec(L);
      Inc(P);
    end;
  SecureClear(B[0], CipherBlockSize);
end;

procedure OFBDecode(
    const Buffer: Pointer; const BufferSize: Integer;
    const CipherContext: Pointer;
    const CipherFunc: TCipherBufferFunc; const CipherBlockSize: Integer;
    const InitVector: Pointer; const InitVectorBufferSize: Integer);
var P    : PByte;
    L    : Integer;
    B, F : array[0..MaxCipherBlockSize - 1] of Byte;
begin
  Assert(BufferSize > 0);
  Assert(CipherBlockSize <= MaxCipherBlockSize);
  Assert(Assigned(InitVector));
  Assert(InitVectorBufferSize = CipherBlockSize);
  P := Buffer;
  L := BufferSize;
  Move(InitVector^, F[0], CipherBlockSize);
  while L >= 0 do
    begin
      Move(F[0], B[0], CipherBlockSize);
      CipherFunc(CipherContext, @B[0], CipherBlockSize);
      Move(F[1], F[0], CipherBlockSize - 1);
      F[CipherBlockSize - 1] := B[0];
      P^ := P^ xor B[0];
      Dec(L);
      Inc(P);
    end;
  SecureClear(B[0], CipherBlockSize);
end;



{                                                                              }
{ Cipher information                                                           }
{                                                                              }
const
  CipherInfo : array[TCipherType] of TCipherInfo = (
    (
     ValidCipher:  False;
     ShortName:    '';
     BufferMode:   cbmStream;
     InBlockSize:  0;
     OutBlockSize: 0;
     KeyBitsMin:   0;
     KeyBitsMax:   0;
     ContextSize:  0;
     InitFunc:     nil;
     EncryptFunc:  nil;
     DecryptFunc:  nil;
     FinaliseFunc: nil;
    ),

    (
     ValidCipher:  True;
     ShortName:    'RC2';
     BufferMode:   cbmMultipleBlocks;
     InBlockSize:  RC2BlockSize;
     OutBlockSize: RC2BlockSize;
     KeyBitsMin:   1;
     KeyBitsMax:   1024;
     ContextSize:  RC2ContextSize;
     InitFunc:     CipherRC2Init;
     EncryptFunc:  CipherRC2EncryptBuffer;
     DecryptFunc:  CipherRC2DecryptBuffer;
     FinaliseFunc: nil;
    ),

    (
     ValidCipher:  True;
     ShortName:    'RC4';
     BufferMode:   cbmStream;
     InBlockSize:  1024;
     OutBlockSize: 1024;
     KeyBitsMin:   1;
     KeyBitsMax:   2048;
     ContextSize:  RC4ContextSize;
     InitFunc:     CipherRC4Init;
     EncryptFunc:  CipherRC4Buffer;
     DecryptFunc:  CipherRC4Buffer;
     FinaliseFunc: nil;
    ),

    (
     ValidCipher:  True;
     ShortName:    'DES';
     BufferMode:   cbmMultipleBlocks;
     InBlockSize:  DESBlockSize;
     OutBlockSize: DESBlockSize;
     KeyBitsMin:   64;
     KeyBitsMax:   64;
     ContextSize:  DESContextSize;
     InitFunc:     CipherDESInit;
     EncryptFunc:  CipherDESBuffer;
     DecryptFunc:  CipherDESBuffer;
     FinaliseFunc: nil;
    ),

    (
     ValidCipher:  True;
     ShortName:    'DES-40';
     BufferMode:   cbmMultipleBlocks;
     InBlockSize:  DESBlockSize;
     OutBlockSize: DESBlockSize;
     KeyBitsMin:   64;
     KeyBitsMax:   64;
     ContextSize:  DES40ContextSize;
     InitFunc:     CipherDES40Init;
     EncryptFunc:  CipherDES40Buffer;
     DecryptFunc:  CipherDES40Buffer;
     FinaliseFunc: nil;
    ),

    (
     ValidCipher:  True;
     ShortName:    'TripleDES';
     BufferMode:   cbmMultipleBlocks;
     InBlockSize:  DESBlockSize;
     OutBlockSize: DESBlockSize;
     KeyBitsMin:   128;
     KeyBitsMax:   128;
     ContextSize:  TripleDESContextSize;
     InitFunc:     CipherTripleDESInit;
     EncryptFunc:  CipherTripleDESBuffer;
     DecryptFunc:  CipherTripleDESBuffer;
     FinaliseFunc: nil;
    ),

    (
     ValidCipher:  True;
     ShortName:    'TripleDES3EDE';
     BufferMode:   cbmMultipleBlocks;
     InBlockSize:  DESBlockSize;
     OutBlockSize: DESBlockSize;
     KeyBitsMin:   192;
     KeyBitsMax:   192;
     ContextSize:  TripleDES3ContextSize;
     InitFunc:     CipherTripleDES3EDEInit;
     EncryptFunc:  CipherTripleDES3BufferEncrypt;
     DecryptFunc:  CipherTripleDES3BufferDecrypt;
     FinaliseFunc: nil;
    ),

    (
     ValidCipher:  True;
     ShortName:    'TipleDES3EEE';
     BufferMode:   cbmMultipleBlocks;
     InBlockSize:  DESBlockSize;
     OutBlockSize: DESBlockSize;
     KeyBitsMin:   192;
     KeyBitsMax:   192;
     ContextSize:  TripleDES3ContextSize;
     InitFunc:     CipherTripleDES3EEEInit;
     EncryptFunc:  CipherTripleDES3BufferEncrypt;
     DecryptFunc:  CipherTripleDES3BufferDecrypt;
     FinaliseFunc: nil;
    ),

    (
     ValidCipher:  True;
     ShortName:    'AES';
     BufferMode:   cbmMultipleBlocks;
     InBlockSize:  AESBlockSize;
     OutBlockSize: AESBlockSize;
     KeyBitsMin:   128;
     KeyBitsMax:   256;
     ContextSize:  AESContextSize;
     InitFunc:     CipherAESInit;
     EncryptFunc:  CipherAESEncryptBuffer;
     DecryptFunc:  CipherAESDecryptBuffer;
     FinaliseFunc: CipherAESFinalise;
    ),

    {$IFDEF Cipher_SupportRSA}
    (
     ValidCipher:  True;
     ShortName:    'RSA-OAEP-1024';
     BufferMode:   cbmSingleBlock;
     InBlockSize:  0;
     OutBlockSize: 128;
     KeyBitsMin:   2048;
     KeyBitsMax:   2048;
     ContextSize:  RSAOAEP1024ContextSize;
     InitFunc:     CipherRSAOAEP1024Init;
     EncryptFunc:  CipherRSAOAEP1024EncryptBuffer;
     DecryptFunc:  CipherRSAOAEP1024DecryptBuffer;
     FinaliseFunc: CipherRSAOAEP1024Finalise;
    )
    {$ELSE}
    (
     ValidCipher:  False;
     ShortName:    'RSA-OAEP-1024';
     BufferMode:   cbmSingleBlock;
     InBlockSize:  0;
     OutBlockSize: 128;
     KeyBitsMin:   1024;
     KeyBitsMax:   1024;
     ContextSize:  0;
     InitFunc:     nil;
     EncryptFunc:  nil;
     DecryptFunc:  nil;
     FinaliseFunc: nil;
    )
    {$ENDIF}
  );

function GetCipherInfo(const Cipher: TCipherType): PCipherInfo;
begin
  Result := @CipherInfo[Cipher];
  if not Result^.ValidCipher then
    Result := nil;
end;



{                                                                              }
{ Cipher                                                                       }
{                                                                              }
function InitCipherState(
         var State: TCipherState;
         const Operation: TCipherOperation;
         const Cipher: TCipherType; const Mode: TCipherMode;
         const Padding: TCipherPadding;
         const KeyBits: Integer): PCipherInfo;
var C : PCipherInfo;
begin
  C := GetCipherInfo(Cipher);
  if not Assigned(C) then
    raise ECipher.Create(CipherError_InvalidCipher, 'Invalid cipher');
  FillChar(State, SizeOf(State), 0);
  State.Operation  := Operation;
  State.Cipher     := Cipher;
  State.Mode       := Mode;
  State.Padding    := Padding;
  State.KeyBits    := KeyBits;
  State.CipherInfo := C;
  Result := C;
end;



{                                                                              }
{ Encrypt                                                                      }
{                                                                              }
procedure EncryptInit(
          var State: TCipherState;
          const Cipher: TCipherType; const Mode: TCipherMode;
          const Padding: TCipherPadding;
          const KeyBits: Integer;
          const KeyBuffer: Pointer; const KeyBufferSize: Integer);
var C : PCipherInfo;
begin
  C := InitCipherState(State, coEncrypt, Cipher, Mode, Padding, KeyBits);
  if Assigned(C^.InitFunc) then
    C^.InitFunc(coEncrypt, KeyBuffer, KeyBufferSize, KeyBits, @State.Context);
end;

procedure EncryptPadBuffer(
          const Padding: TCipherPadding;
          const Buffer: Pointer;
          const InputBufferSize, OutputBufferSize: Integer);
var P : PByte;
    I : Integer;
begin
  P := Buffer;
  Inc(P, InputBufferSize);
  I := OutputBufferSize - InputBufferSize;
  case Padding of
    cpNone        : FillChar(P^, I, 0);
    cpPadSizeByte :
      begin
        FillChar(P^, I - 1, 0);
        Assert(I <= $FF);
        Inc(P, I - 1);
        P^ := Byte(I);
      end;
    cpPadPKCS5    :
      begin
        Assert(I <= $FF);
        FillChar(P^, I, Char(I));
      end;
  end;
end;

function EncryptedSize(
         const Cipher: TCipherType; const Padding: TCipherPadding;
         const BufferSize: Integer): Integer;
var C : PCipherInfo;
    B : Integer;
begin
  if BufferSize < 0 then
    begin
      Result := 0;
      exit;
    end;
  C := GetCipherInfo(Cipher);
  case C^.BufferMode of
    cbmMultipleBlocks :
      begin
        Result := BufferSize;
        B := C^.OutBlockSize;
        Assert((Padding = cpNone) or (B <= $FF));
        case Padding of
          cpNone        : ;
          cpPadSizeByte : Inc(Result);
          cpPadPKCS5    :
            if Result mod B = 0 then
              Inc(Result, B);
        end;
        Result := ((Result + (B - 1)) div B) * B;
      end;
    cbmSingleBlock : Result := C^.OutBlockSize;
    cbmStream      : Result := BufferSize;
  else
    Result := BufferSize;
  end;
end;

function EncryptBuf(
         var State: TCipherState;
         const InputBuffer: Pointer; const InputBufferSize: Integer;
         const OutputBuffer: Pointer; const OutputBufferSize: Integer;
         const InitVectorBuffer: Pointer; const InitVectorBufferSize: Integer): Integer;
var C : PCipherInfo;
    P : Pointer;
begin
  Assert(State.Operation = coEncrypt);
  Assert(Assigned(State.CipherInfo));
  
  C := State.CipherInfo;
  // validate parameters
  Result := EncryptedSize(State.Cipher, State.Padding, InputBufferSize);
  if Result = 0 then
    exit;
  if Result > OutputBufferSize then
    raise ECipher.Create(CipherError_InvalidBufferSize, 'Output buffer too small');
  if not Assigned(InputBuffer) or not Assigned(OutputBuffer) then
    raise ECipher.Create(CipherError_InvalidBuffer, 'Invalid buffer');
  // encrypt
  if InputBuffer <> OutputBuffer then
    Move(InputBuffer^, OutputBuffer^, InputBufferSize);
  // pad buffer
  if C^.BufferMode = cbmMultipleBlocks then
    if InputBufferSize < Result then
      EncryptPadBuffer(State.Padding, OutputBuffer, InputBufferSize, Result);
  // encrypt buffer
  P := @State.Context;
  if C^.BufferMode = cbmSingleBlock then
    CipherBuffer(OutputBuffer, Result, P, C^.EncryptFunc, C^.InBlockSize)
  else
    case State.Mode of
      cmECB : ECBBuffer(OutputBuffer, Result, P, C^.EncryptFunc, C^.InBlockSize);
      cmCBC : CBCEncode(OutputBuffer, Result, P, C^.EncryptFunc, C^.InBlockSize, InitVectorBuffer, InitVectorBufferSize);
      cmCFB : CFBEncode(OutputBuffer, Result, P, C^.EncryptFunc, C^.InBlockSize, InitVectorBuffer, InitVectorBufferSize);
      cmOFB : OFBEncode(OutputBuffer, Result, P, C^.EncryptFunc, C^.InBlockSize, InitVectorBuffer, InitVectorBufferSize);
    else
      raise ECipher.Create(CipherError_InvalidCipherMode, 'Invalid cipher mode');
    end;
end;

procedure EncryptFinalise(var State: TCipherState);
var C : PCipherInfo;
begin
  C := State.CipherInfo;
  if Assigned(C) then
    if Assigned(C^.FinaliseFunc) then
      C^.FinaliseFunc(@State.Context);
  SecureClear(State, SizeOf(State));
end;

function Encrypt(const Cipher: TCipherType; const Mode: TCipherMode;
         const Padding: TCipherPadding;
         const KeyBits: Integer;
         const KeyBuffer: Pointer; const KeyBufferSize: Integer;
         const InputBuffer: Pointer; const InputBufferSize: Integer;
         const OutputBuffer: Pointer; const OutputBufferSize: Integer;
         const InitVectorBuffer: Pointer; const InitVectorBufferSize: Integer): Integer;
var State : TCipherState;
begin
  EncryptInit(
      State, Cipher, Mode, Padding,
      KeyBits, KeyBuffer, KeyBufferSize);
  Result := EncryptBuf(
      State,
      InputBuffer, InputBufferSize, OutputBuffer, OutputBufferSize,
      InitVectorBuffer, InitVectorBufferSize);
  EncryptFinalise(State);
end;

function Encrypt(const Cipher: TCipherType; const Mode: TCipherMode;
         const Padding: TCipherPadding; const KeyBits: Integer;
         const Key, Data, InitVector: RawByteString): RawByteString;
var L, M : Integer;
begin
  M := Length(Data);
  L := EncryptedSize(Cipher, Padding, M);
  SetLength(Result, L);
  if L = 0 then
    exit;
  Encrypt(Cipher, Mode, Padding, KeyBits, PByteChar(Key), Length(Key),
      PByteChar(Data), M, PByteChar(Result), L,
      PByteChar(InitVector), Length(InitVector));
end;



{                                                                              }
{ Decrypt                                                                      }
{                                                                              }
procedure DecryptInit(
          var State: TCipherState;
          const Cipher: TCipherType; const Mode: TCipherMode;
          const Padding: TCipherPadding;
          const KeyBits: Integer;
          const KeyBuffer: Pointer; const KeyBufferSize: Integer);
var C : PCipherInfo;
begin
  C := InitCipherState(State, coDecrypt, Cipher, Mode, Padding, KeyBits);
  if Assigned(C^.InitFunc) then
    if Mode in [cmCFB, cmOFB] then
      C^.InitFunc(coEncrypt, KeyBuffer, KeyBufferSize, KeyBits, @State.Context)
    else
      C^.InitFunc(coDecrypt, KeyBuffer, KeyBufferSize, KeyBits, @State.Context)
end;

function DecryptBuf(
         var State: TCipherState;
         const Buffer: Pointer; const BufferSize: Integer;
         const InitVectorBuffer: Pointer; const InitVectorBufferSize: Integer): Integer;
var C : PCipherInfo;
    P : PByte;
    D : Pointer;
    I : Integer;
    L : Byte;
    N : Integer;
begin
  Assert(State.Operation = coDecrypt);
  Assert(Assigned(State.CipherInfo));

  C := State.CipherInfo;
  // decrypt buffer
  D := @State.Context;
  if C^.BufferMode = cbmSingleBlock then
    CipherBuffer(Buffer, BufferSize, D, C^.DecryptFunc, C^.InBlockSize)
  else
    case State.Mode of
      cmECB : ECBBuffer(Buffer, BufferSize, D, C^.DecryptFunc, C^.InBlockSize);
      cmCBC : CBCDecode(Buffer, BufferSize, D, C^.DecryptFunc, C^.InBlockSize, InitVectorBuffer, InitVectorBufferSize);
      cmCFB : CFBDecode(Buffer, BufferSize, D, C^.EncryptFunc, C^.InBlockSize, InitVectorBuffer, InitVectorBufferSize);
      cmOFB : OFBDecode(Buffer, BufferSize, D, C^.EncryptFunc, C^.InBlockSize, InitVectorBuffer, InitVectorBufferSize);
    else
      raise ECipher.Create(CipherError_InvalidCipherMode, 'Invalid cipher mode');
    end;
  N := BufferSize;
  // remove padding
  if C^.BufferMode = cbmMultipleBlocks then
    if State.Padding in [cpPadSizeByte, cpPadPKCS5] then
      begin
        // get pad size byte (last byte)
        P := Buffer;
        Inc(P, BufferSize - 1);
        L := P^;
        // validate padding
        if (L > N) or (L > C^.InBlockSize) then
          raise ECipher.Create(CipherError_InvalidData, 'Invalid data');
        for I := 1 to L - 1 do
          begin
            Dec(P);
            if ((State.Padding = cpPadSizeByte) and (P^ <> 0)) or
               ((State.Padding = cpPadPKCS5)    and (P^ <> L)) then
              raise ECipher.Create(CipherError_InvalidData, 'Invalid data');
          end;
        // decrease buffer size with padding size
        Dec(N, L);
      end;
  // return number of decoded bytes
  Result := N;
end;

procedure DecryptFinalise(var State: TCipherState);
var C : PCipherInfo;
begin
  C := State.CipherInfo;
  if Assigned(C) then
    if Assigned(C^.FinaliseFunc) then
      C^.FinaliseFunc(@State.Context);
  SecureClear(State, SizeOf(State));
end;

function Decrypt(const Cipher: TCipherType; const Mode: TCipherMode;
         const Padding: TCipherPadding;
         const KeyBits: Integer;
         const KeyBuffer: Pointer; const KeyBufferSize: Integer;
         const Buffer: Pointer; const BufferSize: Integer;
         const InitVectorBuffer: Pointer; const InitVectorBufferSize: Integer): Integer; overload;
var State : TCipherState;
begin
  DecryptInit(State, Cipher, Mode, Padding, KeyBits, KeyBuffer, KeyBufferSize);
  Result := DecryptBuf(State, Buffer, BufferSize, InitVectorBuffer, InitVectorBufferSize);
  DecryptFinalise(State);
end;

function Decrypt(const Cipher: TCipherType; const Mode: TCipherMode;
         const Padding: TCipherPadding; const KeyBits: Integer;
         const Key, Data, InitVector: RawByteString): RawByteString;
var L, M : Integer;
begin
  L := Length(Data);
  SetLength(Result, L);
  if L = 0 then
    exit;
  Move(Pointer(Data)^, Pointer(Result)^, L);
  M := Decrypt(Cipher, Mode, Padding, KeyBits, PByteChar(Key), Length(Key),
      PByteChar(Result), L,
      PByteChar(InitVector), Length(InitVector));
  if M < L then
    SetLength(Result, M);
end;



{                                                                              }
{ Test                                                                         }
{                                                                              }
{$IFDEF CIPHER_TEST}
{$ASSERTIONS ON}
procedure Test;
begin
  Assert(RC2ContextSize = 128);
  Assert(RC2ContextSize <= MaxCipherContextSize);
  Assert(RC4ContextSize <= MaxCipherContextSize);
  Assert(DESContextSize <= MaxCipherContextSize);
  Assert(TripleDESContextSize <= MaxCipherContextSize);
  Assert(TripleDES3ContextSize <= MaxCipherContextSize);
end;
{$ENDIF}



end.

