{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals TLS                                         }
{   File name:        cTLSCipher.pas                                           }
{   File version:     5.04                                                     }
{   Description:      TLS cipher                                               }
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
{ Revision history:                                                            }
{                                                                              }
{   2008/01/18  0.01  Initial version.                                         }
{   2010/11/30  0.02  Revision.                                                }
{   2010/12/16  0.03  AES support.                                             }
{   2018/07/17  5.04  Revised for Fundamentals 5.                              }
{                                                                              }
{******************************************************************************}

{$INCLUDE flcTLS.inc}

unit flcTLSCipher;

interface

uses
  { Cipher }

  flcCipher,

  { TLS }

  flcTLSCipherSuite;



{                                                                              }
{ Cipher info                                                                  }
{                                                                              }
type
  TTLSCipherSuiteCipherCipherInfo = record
    CipherType : TCipherType;
    CipherMode : TCipherMode;
    Padding    : TCipherPadding;
  end;
  PTLSCipherSuiteCipherCipherInfo = ^TTLSCipherSuiteCipherCipherInfo;

const
  TLSCipherSuiteCipherCipherInfo : array[TTLSCipherSuiteCipher] of TTLSCipherSuiteCipherCipherInfo = (
    ( // None
     CipherType : ctNone;
     CipherMode : cmECB;
     Padding    : cpNone),
    ( // NULL
     CipherType : ctNone;
     CipherMode : cmECB;
     Padding    : cpNone),
    ( // RC2_CBC_40
     CipherType : ctRC2;
     CipherMode : cmCBC;
     Padding    : cpNone),
    ( // RC4_40
     CipherType : ctRC4;
     CipherMode : cmECB;
     Padding    : cpNone),
    ( // RC4_56
     CipherType : ctRC4;
     CipherMode : cmECB;
     Padding    : cpNone),
    ( // RC4_128
     CipherType : ctRC4;
     CipherMode : cmECB;
     Padding    : cpNone),
    ( // IDEA_CBC
     CipherType : ctNone;
     CipherMode : cmCBC;
     Padding    : cpNone),
    ( // DES40_CBC
     CipherType : ctDES;
     CipherMode : cmCBC;
     Padding    : cpNone),
    ( // DES_CBC
     CipherType : ctDES;
     CipherMode : cmCBC;
     Padding    : cpNone),
    ( // 3DES_EDE_CBC
     CipherType : ctTripleDES3EDE; //// TripleDESEDE or TripleDES3EDE ?
     CipherMode : cmCBC;
     Padding    : cpNone),
    ( // AES_128_CBC
     CipherType : ctAES;
     CipherMode : cmCBC;
     Padding    : cpNone),
    ( // AES_256_CBC
     CipherType : ctAES;
     CipherMode : cmCBC;
     Padding    : cpNone)
  );



{                                                                              }
{ Cipher state                                                                 }
{                                                                              }
type
  TTLSCipherOperation = (
    tlscoNone,
    tlscoEncrypt,
    tlscoDecrypt);

  TTLSCipherState = record
    Operation     : TTLSCipherOperation;
    TLSCipher     : TTLSCipherSuiteCipher;
    TLSCipherInfo : TTLSCipherSuiteCipherInfo;
    CipherInfo    : TTLSCipherSuiteCipherCipherInfo;
    CipherState   : TCipherState;
  end;



{                                                                              }
{ Cipher                                                                       }
{                                                                              }
procedure TLSCipherInitNone(
          var CipherState: TTLSCipherState;
          const Operation: TTLSCipherOperation);
procedure TLSCipherInitNULL(
          var CipherState: TTLSCipherState;
          const Operation: TTLSCipherOperation);
procedure TLSCipherInit(
          var CipherState: TTLSCipherState;
          const Operation: TTLSCipherOperation;
          const TLSCipher: TTLSCipherSuiteCipher;
          const KeyBuf;
          const KeySize: Integer);

procedure TLSCipherBuf(
          var CipherState: TTLSCipherState;
          const MessageBuf;
          const MessageSize: Integer;
          var   CipherBuf;
          const CipherBufSize: Integer;
          out   CipherSize: Integer;
          const IVBufPtr: Pointer;
          const IVBufSize: Integer);

procedure TLSCipherFinalise(var CipherState: TTLSCipherState);



{                                                                              }
{ Test cases                                                                   }
{                                                                              }
{$IFDEF TLS_TEST}
procedure Test;
{$ENDIF}



implementation

uses
  { Utils }

  flcStdTypes,

  { TLS }

  flcTLSErrors;



{                                                                              }
{ Cipher                                                                       }
{                                                                              }
procedure TLSCipherInitNone(
          var CipherState: TTLSCipherState;
          const Operation: TTLSCipherOperation);
begin
  FillChar(CipherState, SizeOf(CipherState), 0);
  CipherState.Operation     := Operation;
  CipherState.TLSCipher     := tlscscNone;
  CipherState.TLSCipherInfo := TLSCipherSuiteCipherInfo[tlscscNone];
  CipherState.CipherInfo    := TLSCipherSuiteCipherCipherInfo[tlscscNone];
end;

procedure TLSCipherInitNULL(
          var CipherState: TTLSCipherState;
          const Operation: TTLSCipherOperation);
begin
  FillChar(CipherState, SizeOf(CipherState), 0);
  CipherState.Operation     := Operation;
  CipherState.TLSCipher     := tlscscNULL;
  CipherState.TLSCipherInfo := TLSCipherSuiteCipherInfo[tlscscNULL];
  CipherState.CipherInfo    := TLSCipherSuiteCipherCipherInfo[tlscscNULL];
end;

procedure TLSCipherInit(
          var CipherState: TTLSCipherState;
          const Operation: TTLSCipherOperation;
          const TLSCipher: TTLSCipherSuiteCipher;
          const KeyBuf;
          const KeySize: Integer);
begin
  FillChar(CipherState, SizeOf(CipherState), 0);
  CipherState.Operation     := Operation;
  CipherState.TLSCipher     := TLSCipher;
  CipherState.TLSCipherInfo := TLSCipherSuiteCipherInfo[TLSCipher];
  CipherState.CipherInfo    := TLSCipherSuiteCipherCipherInfo[TLSCipher];
  case Operation of
    tlscoEncrypt :
      EncryptInit(
          CipherState.CipherState,
          CipherState.CipherInfo.CipherType,
          CipherState.CipherInfo.CipherMode,
          CipherState.CipherInfo.Padding,
          CipherState.TLSCipherInfo.KeyBits, @KeyBuf, KeySize);
    tlscoDecrypt :
      DecryptInit(
          CipherState.CipherState,
          CipherState.CipherInfo.CipherType,
          CipherState.CipherInfo.CipherMode,
          CipherState.CipherInfo.Padding,
          CipherState.TLSCipherInfo.KeyBits, @KeyBuf, KeySize);
  end;
end;

procedure TLSCipherBuf(
          var CipherState: TTLSCipherState;
          const MessageBuf;
          const MessageSize: Integer;
          var CipherBuf;
          const CipherBufSize: Integer;
          out CipherSize: Integer;
          const IVBufPtr: Pointer;
          const IVBufSize: Integer);
begin
  if CipherState.Operation = tlscoNone then
    raise ETLSError.Create(TLSError_InvalidParameter); // cipher not initialised
  if MessageSize < 0 then
    raise ETLSError.Create(TLSError_InvalidBuffer);
  if CipherBufSize < MessageSize then
    raise ETLSError.Create(TLSError_InvalidBuffer);
  if CipherState.TLSCipher in [tlscscNone, tlscscNULL] then
    begin
      Move(MessageBuf, CipherBuf, MessageSize);
      CipherSize := MessageSize;
    end
  else
  case CipherState.Operation of
    tlscoEncrypt :
      CipherSize := EncryptBuf(CipherState.CipherState,
          @MessageBuf, MessageSize,
          @CipherBuf, CipherBufSize,
          IVBufPtr, IVBufSize);
    tlscoDecrypt :
      begin
        Move(MessageBuf, CipherBuf, MessageSize);
        CipherSize := DecryptBuf(CipherState.CipherState,
            @CipherBuf, MessageSize,
            IVBufPtr, IVBufSize);
      end;
  end;
end;

procedure TLSCipherFinalise(var CipherState: TTLSCipherState);
begin
  case CipherState.Operation of
    tlscoEncrypt : EncryptFinalise(CipherState.CipherState);
    tlscoDecrypt : DecryptFinalise(CipherState.CipherState);
  end;
end;



{                                                                              }
{ Test cases                                                                   }
{                                                                              }
{$IFDEF TLS_TEST}
procedure Test;
const
  ClientWriteKey = RawByteString(#$10#$D0#$D6#$C2#$D9#$B7#$62#$CB#$2C#$74#$BF#$5F#$85#$3C#$6F#$E7);
var
  S, T : RawByteString;
  C : TTLSCipherState;
  B : RawByteString;
  L : Integer;
begin
  //                                                                                              //
  // Example from http://download.oracle.com/javase/1.5.0/docs/guide/security/jsse/ReadDebug.html //
  //                                                                                              //
  S := ClientWriteKey;
  TLSCipherInit(C, tlscoEncrypt, tlscscRC4_128, S[1], Length(S));
  T := RawByteString(
      #$14#$00#$00#$0C#$F2#$62#$42#$AA#$7C#$7C#$CC#$E7#$49#$0F#$ED#$AC +
      #$FA#$06#$3C#$9F#$8C#$41#$1D#$ED#$2B#$06#$D0#$5A#$ED#$31#$F2#$80);
  SetLength(B, 1024);
  L := 1024;
  TLSCipherBuf(C, T[1], Length(T), B[1], L, L, nil, 0);
  SetLength(B, L);
  Assert(B = RawByteString(
      #$15#$8C#$25#$BA#$4E#$73#$F5#$27#$79#$49#$B1 +
      #$E9#$F5#$7E#$C8#$48#$A7#$D3#$A6#$9B#$BD#$6F#$8E#$A5#$8E#$2B#$B7 +
      #$EE#$DC#$BD#$F4#$D7));
end;
{$ENDIF}



end.

