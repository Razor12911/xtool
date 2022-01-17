{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals TLS                                         }
{   File name:        flcTLSRecord.pas                                         }
{   File version:     5.08                                                     }
{   Description:      TLS records                                              }
{                                                                              }
{   Copyright:        Copyright (c) 2008-2020, David J Butler                  }
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
{   2008/01/18  0.01  Initial development.                                     }
{   2010/11/30  0.02  Stream cipher.                                           }
{   2010/12/01  0.03  Block cipher for TLS 1.0.                                }
{   2010/12/02  0.04  Block cipher for TLS 1.1 and TLS 1.2.                    }
{   2010/12/17  0.05  Fixes for TLS 1.1 and TLS 1.2 block ciphers.             }
{   2011/10/11  0.06  Fixes for TLS 1.1 block cipher encoding.                 }
{   2011/10/12  0.07  MAC validation on decoding.                              }
{   2018/07/17  5.08  Revised for Fundamentals 5.                              }
{                                                                              }
{******************************************************************************}

{$INCLUDE flcTLS.inc}

unit flcTLSRecord;

interface

uses
  { TLS }

  flcTLSProtocolVersion,
  flcTLSAlgorithmTypes,
  flcTLSCipherSuite,
  flcTLSCipher;



{                                                                              }
{ ContentType                                                                  }
{                                                                              }
type
  TTLSContentType = (
    tlsctInvalid            = 0,
    tlsctChange_cipher_spec = 20,
    tlsctAlert              = 21,
    tlsctHandshake          = 22,
    tlsctApplication_data   = 23,
    tlsctHeartbeat          = 24,   // RFC 6520
    tlsctMax                = 255
  );
  PTLSContentType = ^TTLSContentType;

const
  TLSContentTypeSize = Sizeof(TTLSContentType);

function TLSContentTypeToStr(const A: TTLSContentType): String;
function IsKnownTLSContentType(const A: TTLSContentType): Boolean;



{                                                                              }
{ TLS Record Header                                                            }
{                                                                              }
type
  TTLSRecordHeader = packed record
    _type   : TTLSContentType;
    version : TTLSProtocolVersion;
    length  : Word;
  end;
  PTLSRecordHeader = ^TTLSRecordHeader;

const
  TLSRecordHeaderSize = Sizeof(TTLSRecordHeader);

procedure InitTLSRecordHeader(var RecordHeader: TTLSRecordHeader;
          const ContentType: TTLSContentType;
          const Version: TTLSProtocolVersion;
          const Length: Word);
procedure DecodeTLSRecordHeader(const RecordHeader: TTLSRecordHeader;
          var ContentType: TTLSContentType;
          var Version: TTLSProtocolVersion;
          var Length: Word);



{                                                                              }
{ Record payload MAC                                                           }
{                                                                              }
function  PrepareTLSRecordPayloadMACBuffer(
          var Buffer; const Size: Integer;
          const ProtocolVersion: TTLSProtocolVersion;
          const MACSize: Integer;
          const SequenceNumber: Int64;
          const TLSCompressedHdr;
          const TLSCompressedBuf; const TLSCompressedBufSize: Integer): Integer;

function  GenerateTLSRecordPayloadMAC(
          const MACAlgorithm: TTLSMACAlgorithm;
          const Key; const KeySize: Integer;
          const Buf; const BufSize: Integer;
          var Digest; const DigestSize: Integer): Integer;

function  GenerateRecordPayloadMAC(
          const ProtocolVersion: TTLSProtocolVersion;
          const CipherSuiteDetails: TTLSCipherSuiteDetails;
          const Key; const KeySize: Integer;
          const SequenceNumber: Int64;
          const TLSCompressedHdr: PTLSRecordHeader;
          const TLSCompressedBuf; const TLSCompressedBufSize: Integer;
          var Digest; const DigestSize: Integer): Integer;

          

{                                                                              }
{ Record                                                                       }
{                                                                              }
function  EncodeTLSRecord(
          var Buffer; const Size: Integer;
          const ProtocolVersion: TTLSProtocolVersion;
          const ContentType: TTLSContentType;
          const ContentBuffer; const ContentSize: Integer;
          const CompressionMethod: TTLSCompressionMethod;
          const CipherSuiteDetails: TTLSCipherSuiteDetails;
          const SequenceNumber: Int64;
          const MACKey; const MACKeySize: Integer;
          var CipherState: TTLSCipherState;
          const IVBufPtr: Pointer; const IVBufSize: Integer): Integer;

procedure DecodeTLSRecord(
          const RecHeader: PTLSRecordHeader;
          const Buffer; const Size: Integer;
          const ProtocolVersion: TTLSProtocolVersion;
          const CompressionMethod: TTLSCompressionMethod;
          const CipherSuiteDetails: TTLSCipherSuiteDetails;
          const SequenceNumber: Int64;
          const MACKey; const MACKeySize: Integer;
          var CipherState: TTLSCipherState;
          const IVBufPtr: Pointer; const IVBufSize: Integer;
          var ContentBuffer; const ContentBufferSize: Integer;
          out ContentSize: Integer);



{                                                                              }
{ Test cases                                                                   }
{                                                                              }
{$IFDEF TLS_TEST}
procedure Test;
{$ENDIF}



implementation

uses
  { System }

  SysUtils,

  { Utils }

  flcStdTypes,
  flcHash,

  { Cipher }

  flcCipherRandom,

  { TLS }

  flcTLSConsts,
  flcTLSErrors,
  flcTLSCompress;



{                                                                              }
{ ContentType                                                                  }
{                                                                              }
function TLSContentTypeToStr(const A: TTLSContentType): String;
begin
  case A of
    tlsctInvalid            : Result := 'Invalid';
    tlsctChange_cipher_spec : Result := 'Change_cipher_spec';
    tlsctAlert              : Result := 'Alert';
    tlsctHandshake          : Result := 'Handshake';
    tlsctApplication_data   : Result := 'Application_data';
    tlsctHeartbeat          : Result := 'Heartbeat';
  else
    Result := '[TLSContentType#' + IntToStr(Ord(A)) + ']';
  end;
end;

function IsKnownTLSContentType(const A: TTLSContentType): Boolean;
begin
  Result := A in [
      tlsctChange_cipher_spec,
      tlsctAlert,
      tlsctHandshake,
      tlsctApplication_data,
      tlsctHeartbeat];
end;



{                                                                              }
{ TLS Record Header                                                            }
{                                                                              }
{ SSL3 / TLS 1.0 / TLS 1.1 / TLS 1.2:                                          }
{   ContentType     type                                                       }
{   ProtocolVersion version                                                    }
{   uint16          length                                                     }
{                                                                              }
procedure InitTLSRecordHeader(var RecordHeader: TTLSRecordHeader;
          const ContentType: TTLSContentType;
          const Version: TTLSProtocolVersion;
          const Length: Word);
begin
  RecordHeader._type   := ContentType;
  RecordHeader.version := Version;
  RecordHeader.length  :=
      ((Length and $FF) shl 8) or
       (Length shr 8);
end;

procedure DecodeTLSRecordHeader(const RecordHeader: TTLSRecordHeader;
          var ContentType: TTLSContentType;
          var Version: TTLSProtocolVersion;
          var Length: Word);
begin
  ContentType := RecordHeader._type;
  Version     := RecordHeader.version ;
  Length      :=
      ((RecordHeader.length and $FF) shl 8) or
       (RecordHeader.length shr 8);
end;



{                                                                              }
{ Record payload MAC                                                           }
{                                                                              }
{ SSL 3:                                                                       }
{   hash(MAC_write_secret + pad_2 +                                            }
{        hash(MAC_write_secret + pad_1 +                                       }
{             seq_num +                                                        }
{             SSLCompressed.type +                                             }
{             SSLCompressed.length +                                           }
{             SSLCompressed.fragment));                                        }
{   pad_1 = The character 0x36 repeated 48 times for MD5 or 40 times for SHA.  }
{   pad_2 = The character 0x5c repeated 48 times for MD5 or 40 times for SHA.  }
{                                                                              }
{ TLS 1.0 / TLS 1.1 / TLS 1.2:                                                 }
{   HMAC_hash(MAC_write_key, seq_num +                                         }
{                      TLSCompressed.type +                                    }
{                      TLSCompressed.version +                                 }
{                      TLSCompressed.length +                                  }
{                      TLSCompressed.fragment);                                }
{                                                                              }
procedure EncodeSequenceNumber(
          const SequenceNumber: Int64;
          const Buf; const BufSize: Integer);
var P : PByte;
    B : array[0..7] of Byte;
    I : Integer;
begin
  if BufSize < 8 then
    raise ETLSError.Create(TLSError_InvalidBuffer);
  P := @Buf;
  Move(SequenceNumber, B, 8);
  for I := 0 to 7 do
    begin
      P^ := B[7 - I];
      Inc(P);
    end;
end;

function CalculateSSLHash(
         const Hash: TTLSCipherSuiteHash;
         const Buf; const BufSize: Integer;
         var Digest; const DigestSize: Integer): Integer;
var L : Integer;
begin
  L := TLSCipherSuiteHashInfo[Hash].HashSize div 8;
  if DigestSize < L then
    raise ETLSError.Create(TLSError_InvalidBuffer);
  case Hash of
    tlscshMD5 : P128BitDigest(@Digest)^ := CalcMD5(Buf, BufSize);
    tlscshSHA : P160BitDigest(@Digest)^ := CalcSHA1(Buf, BufSize);
  else
    raise ETLSError.Create(TLSError_InvalidParameter);
  end;
  Result := L;
end;

const
  SSL_MACBufSize = (TLS_PLAINTEXT_FRAGMENT_MAXSIZE + 1024) * 2;

function GenerateSSLRecordPayloadMAC(
         const Hash: TTLSCipherSuiteHash;
         const Key; const KeySize: Integer;
         const SequenceNumber: Int64;
         const TLSCompressedHdr: PTLSRecordHeader;
         const TLSCompressedBuf; const TLSCompressedBufSize: Integer;
         var Digest; const DigestSize: Integer): Integer;
var PadN : Integer;
    Buf1 : array[0..SSL_MACBufSize - 1] of Byte;
    Len1 : Integer;
    Buf2 : array[0..SSL_MACBufSize - 1] of Byte;
    Len2 : Integer;
    Hash1 : array[0..TLS_MAC_MAXDIGESTSIZE - 1] of Byte;
    Hash1Len : Integer;
    Hash2 : array[0..TLS_MAC_MAXDIGESTSIZE - 1] of Byte;
    Hash2Len : Integer;
    P : PByte;
begin
  case Hash of
    tlscshMD5 : PadN := 48;
    tlscshSHA : PadN := 40;
  else
    raise ETLSError.Create(TLSError_InvalidParameter);
  end;
  // hash(MAC_write_secret + pad_1 + seq_num + SSLCompressed.type + SSLCompressed.length + SSLCompressed.fragment)
  P := @Buf1;
  Len1 := 0;
  Move(Key, P^, KeySize);
  Inc(P, KeySize);
  Inc(Len1, KeySize);
  FillChar(P^, PadN, #$36);
  Inc(P, PadN);
  Inc(Len1, PadN);
  EncodeSequenceNumber(SequenceNumber, P^, SSL_MACBufSize - Len1);
  Inc(P, 8);
  Inc(Len1, 8);
  Move(TLSCompressedHdr^._type, P^, 1);
  Inc(P);
  Inc(Len1);
  Move(TLSCompressedHdr^.length, P^, 2);
  Inc(P, 2);
  Inc(Len1, 2);
  Move(TLSCompressedBuf, P^, TLSCompressedBufSize);
  Inc(Len1, TLSCompressedBufSize);
  Hash1Len := CalculateSSLHash(Hash, Buf1, Len1, Hash1, SizeOf(Hash1));
  // hash(MAC_write_secret + pad_2 + hash1)
  P := @Buf2;
  Len2 := 0;
  Move(Key, P^, KeySize);
  Inc(P, KeySize);
  Inc(Len2, KeySize);
  FillChar(P^, PadN, #$5C);
  Inc(P, PadN);
  Inc(Len2, PadN);
  Move(Hash1, P^, Hash1Len);
  Inc(Len2, Hash1Len);
  Hash2Len := CalculateSSLHash(Hash, Buf2, Len2, Hash2, SizeOf(Hash2));
  // result
  if DigestSize < Hash2Len then
    raise ETLSError.Create(TLSError_InvalidBuffer);
  Move(Hash2, Digest, Hash2Len);
  Result := Hash2Len;
end;

function PrepareTLSRecordPayloadMACBuffer(
         var Buffer; const Size: Integer;
         const ProtocolVersion: TTLSProtocolVersion;
         const MACSize: Integer;
         const SequenceNumber: Int64;
         const TLSCompressedHdr;
         const TLSCompressedBuf; const TLSCompressedBufSize: Integer): Integer;
var P : PByte;
    N : Integer;
begin
  if not IsTLS10OrLater(ProtocolVersion) then
    raise ETLSError.Create(TLSError_InvalidParameter);
  N := Size;
  Dec(N, 8);
  Dec(N, TLSRecordHeaderSize);
  Dec(N, TLSCompressedBufSize);
  if N < 0 then
    raise ETLSError.Create(TLSError_InvalidBuffer);
  P := @Buffer;
  EncodeSequenceNumber(SequenceNumber, P^, Size);
  Inc(P, 8);
  Move(TLSCompressedHdr, P^, TLSRecordHeaderSize);
  Inc(P, TLSRecordHeaderSize);
  Move(TLSCompressedBuf, P^, TLSCompressedBufSize);
  Result := Size - N;
end;

function GenerateTLSRecordPayloadMAC(
         const MACAlgorithm: TTLSMACAlgorithm;
         const Key; const KeySize: Integer;
         const Buf; const BufSize: Integer;
         var Digest; const DigestSize: Integer): Integer;
var L : Integer;
begin
  L := TLSMACAlgorithmInfo[MACAlgorithm].DigestSize;
  if DigestSize < L then
    raise ETLSError.Create(TLSError_InvalidBuffer);
  case MACAlgorithm of
    tlsmaHMAC_MD5    : P128BitDigest(@Digest)^ := CalcHMAC_MD5(@Key, KeySize, Buf, BufSize);
    tlsmaHMAC_SHA1   : P160BitDigest(@Digest)^ := CalcHMAC_SHA1(@Key, KeySize, Buf, BufSize);
    tlsmaHMAC_SHA256 : P256BitDigest(@Digest)^ := CalcHMAC_SHA256(@Key, KeySize, Buf, BufSize);
    tlsmaHMAC_SHA512 : P512BitDigest(@Digest)^ := CalcHMAC_SHA512(@Key, KeySize, Buf, BufSize);
  else
    raise ETLSError.Create(TLSError_InvalidParameter, 'Invalid MAC algorithm[' + IntToStr(Ord(MACAlgorithm)) + ']');
  end;
  Result := L;
end;

const
  // size of temporary buffers used in record encoding and decoding
  // this should be large enough to hold any plain, compressed or encrypted record
  TLS_RecordBufSize = (TLS_PLAINTEXT_FRAGMENT_MAXSIZE + 1024) * 2;

function GenerateRecordPayloadMAC(
         const ProtocolVersion: TTLSProtocolVersion;
         const CipherSuiteDetails: TTLSCipherSuiteDetails;
         const Key; const KeySize: Integer;
         const SequenceNumber: Int64;
         const TLSCompressedHdr: PTLSRecordHeader;
         const TLSCompressedBuf; const TLSCompressedBufSize: Integer;
         var Digest; const DigestSize: Integer): Integer;
var MACSize    : Integer;
    MACAlgo    : TTLSMACAlgorithm;
    BufMAC     : array[0..TLS_RecordBufSize - 1] of Byte;
    BufMACSize : Integer;
begin
  if IsSSL3(ProtocolVersion) then
    begin
      Result := GenerateSSLRecordPayloadMAC(
          CipherSuiteDetails.CipherSuiteInfo^.Hash,
          Key, KeySize,
          SequenceNumber,
          TLSCompressedHdr,
          TLSCompressedBuf, TLSCompressedBufSize,
          Digest, DigestSize);
    end
  else
  if IsTLS10OrLater(ProtocolVersion) then
    begin
      MACSize := CipherSuiteDetails.HashInfo^.HashSize div 8;
      MACAlgo := TLSCipherSuiteHashInfo[CipherSuiteDetails.CipherSuiteInfo^.Hash].MACAlgorithm;
      BufMACSize := PrepareTLSRecordPayloadMACBuffer(
          BufMAC, SizeOf(BufMAC),
          ProtocolVersion,
          MACSize,
          SequenceNumber,
          TLSCompressedHdr^,
          TLSCompressedBuf, TLSCompressedBufSize);
      Result := GenerateTLSRecordPayloadMAC(
          MACAlgo,
          Key, KeySize,
          BufMAC, BufMACSize,
          Digest, DigestSize);
      SecureClear(BufMAC, BufMACSize);
    end
  else
    raise ETLSError.Create(TLSError_InvalidParameter);
end;



{                                                                              }
{ Record                                                                       }
{                                                                              }
{ SSL 3:                                                                       }
{ GenericStreamCipher                                                          }
{      stream-ciphered struct                                                  }
{          opaque content[SSLCompressed.length];                               }
{          opaque MAC[CipherSpec.hash_size];                                   }
{                                                                              }
{ TLS 1.0 / 1.1 / 1.2:                                                         }
{ GenericStreamCipher                                                          }
{      stream-ciphered struct                                                  }
{          opaque content[TLSCompressed.length];                               }
{          opaque MAC[SecurityParameters.mac_length];                          }
{                                                                              }
{ SSL 3:                                                                       }
{ GenericBlockCipher                                                           }
{      block-ciphered struct                                                   }
{          opaque content[SSLCompressed.length];                               }
{          opaque MAC[CipherSpec.hash_size];                                   }
{          uint8 padding[GenericBlockCipher.padding_length];                   }
{          uint8 padding_length;                                               }
{                                                                              }
{ TLS 1.0:                                                                     }
{ GenericBlockCipher                                                           }
{      block-ciphered struct                                                   }
{          opaque content[TLSCompressed.length];                               }
{          opaque MAC[CipherSpec.hash_size];                                   }
{          uint8 padding[GenericBlockCipher.padding_length];                   }
{          uint8 padding_length;                                               }
{                                                                              }
{ TLS 1.1:                                                                     }
{ GenericBlockCipher                                                           }
{      block-ciphered struct                                                   }
{          opaque IV[CipherSpec.block_length];                                 }
{          opaque content[TLSCompressed.length];                               }
{          opaque MAC[CipherSpec.hash_size];                                   }
{          uint8 padding[GenericBlockCipher.padding_length];                   }
{          uint8 padding_length;                                               }
{                                                                              }
{ TLS 1.2:                                                                     }
{ GenericBlockCipher                                                           }
{      opaque IV[SecurityParameters.record_iv_length];                         }
{      block-ciphered struct                                                   }
{          opaque content[TLSCompressed.length];                               }
{          opaque MAC[SecurityParameters.mac_length];                          }
{          uint8 padding[GenericBlockCipher.padding_length];                   }
{          uint8 padding_length;                                               }
{                                                                              }

// Returns number of padding bytes encoded
function EncodeTLSGenericBlockCipherPadding(
         var Buffer; const BufferSize: Integer;
         const GenericBlockSize: Integer;
         const CipherBlockSize: Integer): Integer;
var L, I, N : Integer;
    P : PByte;
begin
  if (CipherBlockSize <= 0) or (CipherBlockSize > 256) then
    raise ETLSError.Create(TLSError_InvalidParameter);

  L := (GenericBlockSize + 1) mod CipherBlockSize;
  if L > 0 then
    L := CipherBlockSize - L;
  Assert(L <= $FF);
  Assert((GenericBlockSize + L + 1) mod CipherBlockSize = 0);
  
  N := L + 1;
  if BufferSize < N then
    raise ETLSError.Create(TLSError_InvalidBuffer);
  P := @Buffer;
  for I := 1 to L do
    begin
      P^ := L;
      Inc(P);
    end;
  P^ := L;
  Result := N;
end;

// Returns number of padding bytes at end of padded buffer
// Validates padding bytes
function DecodeTLSGenericBlockCipherPadding(const Buffer; const Size: Integer): Integer;
var P : PByte;
    C : Byte;
    L, I : Integer;
begin
  P := @Buffer;
  Inc(P, Size - 1);
  C := P^;
  L := C + 1;
  if Size < L then
    raise ETLSError.Create(TLSError_InvalidBuffer);
  for I := 0 to C - 1 do
    begin
      Dec(P);
      if P^ <> C then
        raise ETLSError.Create(TLSError_DecodeError);
    end;
  Result := C + 1;
end;

// TLS 1.0: get IV from last cipher block
procedure tls10UpdateIV(
          const Buffer; const Size: Integer;
          var IVBuffer; const IVBufferSize: Integer);
var P : PByte;
begin
  if (IVBufferSize <= 0) or (Size < IVBufferSize) then
    raise ETLSError.Create(TLSError_InvalidBuffer);
  P := @Buffer;
  Inc(P, Size - IVBufferSize);
  Move(P^, IVBuffer, IVBufferSize);
end;

// TLS 1.1 1.2: generate random IV
procedure tls11UpdateIV(var IVBuffer; const IVBufferSize: Integer);
begin
  if IVBufferSize <= 0 then
    raise ETLSError.Create(TLSError_InvalidBuffer);
  SecureRandomBuf(IVBuffer, IVBufferSize);
end;

// Encode a TLS record and update IV
// Returns size of encoded TLS record (including header)
function EncodeTLSRecord(
         var Buffer; const Size: Integer;
         const ProtocolVersion: TTLSProtocolVersion;
         const ContentType: TTLSContentType;
         const ContentBuffer; const ContentSize: Integer;
         const CompressionMethod: TTLSCompressionMethod;
         const CipherSuiteDetails: TTLSCipherSuiteDetails;
         const SequenceNumber: Int64;
         const MACKey; const MACKeySize: Integer;
         var CipherState: TTLSCipherState;
         const IVBufPtr: Pointer; const IVBufSize: Integer): Integer;
var BufP          : PByte;
    BufLeft       : Integer;
    HasCipher     : Boolean;
    IsBlockCipher : Boolean;
    UseIV         : Boolean;
    IVSize        : Integer;
    RecHeader     : PTLSRecordHeader;
    RecContent    : Pointer;
    RecMAC        : Pointer;
    RecCipher     : Pointer;
    ComprSize     : Integer;
    MACSize       : Integer;
    CipherSize    : Integer;
    BlockSize     : Integer;
    BufCipher     : array[0..TLS_RecordBufSize - 1] of Byte;
    CipheredSize  : Integer;
    FinalSize     : Integer;
begin
  if ContentSize > TLS_PLAINTEXT_FRAGMENT_MAXSIZE then
    raise ETLSError.Create(TLSError_InvalidParameter);

  // initialise parameters
  HasCipher := CipherSuiteDetails.CipherSuite <> tlscsNone;
  IsBlockCipher :=
      HasCipher and
      (CipherSuiteDetails.CipherInfo^.CipherType = tlscsctBlock);
  UseIV :=
      IsBlockCipher and
      (CipherSuiteDetails.CipherInfo^.IVSize > 0);
  if UseIV then
    IVSize := CipherSuiteDetails.CipherInfo^.IVSize
  else
    IVSize := 0;
  if UseIV and (IVBufSize < IVSize) then
    raise ETLSError.Create(TLSError_InvalidBuffer);

  // encode to Buffer
  BufP := @Buffer;
  BufLeft := Size;

  // header
  Dec(BufLeft, TLSRecordHeaderSize);
  if BufLeft < 0 then
    raise ETLSError.Create(TLSError_InvalidBuffer);
  RecHeader := Pointer(BufP);
  Inc(BufP, TLSRecordHeaderSize);

  // TLS 1.1/1.2: generate random IV
  if UseIV then
    if IsTLS11OrLater(ProtocolVersion) then
      tls11UpdateIV(IVBufPtr^, IVBufSize);

  // TLS 1.1/1.2: encode IV field
  if UseIV then
    if IsTLS11OrLater(ProtocolVersion) then
      begin
        Dec(BufLeft, IVSize);
        if BufLeft < 0 then
          raise ETLSError.Create(TLSError_InvalidBuffer);
        Move(IVBufPtr^, BufP^, IVSize);
        Inc(BufP, IVSize);
      end;

  // compress content
  RecContent := BufP;
  TLSCompressFragment(CompressionMethod,
      ContentBuffer, ContentSize,
      BufP^, BufLeft,
      ComprSize);
  Inc(BufP, ComprSize);
  Dec(BufLeft, ComprSize);

  // update header with compressed details
  InitTLSRecordHeader(RecHeader^, ContentType, ProtocolVersion, ComprSize);

  if HasCipher then
    begin
      // calculate MAC
      RecMAC := BufP;
      MACSize := CipherSuiteDetails.HashInfo^.HashSize div 8;
      GenerateRecordPayloadMAC(
          ProtocolVersion,
          CipherSuiteDetails,
          MACKey, MACKeySize,
          SequenceNumber,
          RecHeader,
          RecContent^, ComprSize,
          RecMAC^, BufLeft);
      Inc(BufP, MACSize);
      Dec(BufLeft, MACSize);

      // cipher size
      CipherSize := ComprSize + MACSize;

      // block cipher padding
      if IsBlockCipher then
        begin
          BlockSize := CipherSuiteDetails.CipherInfo^.BlockSize;
          Inc(CipherSize,
              EncodeTLSGenericBlockCipherPadding(BufP^, BufLeft, CipherSize, BlockSize));
        end;

      // Encrypts content (excluding IV)
      RecCipher := RecContent;
      TLSCipherBuf(CipherState,
          RecCipher^, CipherSize,
          BufCipher, SizeOf(BufCipher),
          CipheredSize,
          IVBufPtr, IVSize);
      Move(BufCipher, RecCipher^, CipheredSize);

      // update IV
      if UseIV then
        if IsTLS10(ProtocolVersion) or IsSSL3(ProtocolVersion) then
          tls10UpdateIV(RecContent^, CipheredSize, IVBufPtr^, IVBufSize);

      // final size
      FinalSize := CipheredSize;
      if UseIV then
        if IsTLS11OrLater(ProtocolVersion) then
          Inc(FinalSize, IVSize);

      // update header with final encrypted details
      InitTLSRecordHeader(RecHeader^, ContentType, ProtocolVersion, FinalSize);
    end
  else
    FinalSize := ComprSize;

  Result := TLSRecordHeaderSize + FinalSize;
end;

// Decode a TLS record into ContentBuffer and returns size of decoded content in ContentSize
// Updates IV for next record
// Buffer points to first byte afer record header
procedure DecodeTLSRecord(
          const RecHeader: PTLSRecordHeader;
          const Buffer; const Size: Integer;
          const ProtocolVersion: TTLSProtocolVersion;
          const CompressionMethod: TTLSCompressionMethod;
          const CipherSuiteDetails: TTLSCipherSuiteDetails;
          const SequenceNumber: Int64;
          const MACKey; const MACKeySize: Integer;
          var CipherState: TTLSCipherState;
          const IVBufPtr: Pointer; const IVBufSize: Integer;
          var ContentBuffer; const ContentBufferSize: Integer;
          out ContentSize: Integer);
var BufP          : PByte;
    BufPLeft      : Integer;
    BufQ          : PByte;
    BufQLeft      : Integer;
    HasCipher     : Boolean;
    IsBlockCipher : Boolean;
    UseIV         : Boolean;
    IVSize        : Integer;
    BufCipher     : array[0..TLS_RecordBufSize - 1] of Byte;
    CipherSize    : Integer;
    RecContent    : Pointer;
    ComprSize     : Integer;
    BufPlain      : array[0..TLS_RecordBufSize - 1] of Byte;
    PlainSize     : Integer;
    NextIV        : array[0..TLS_CIPHERSUITE_MaxIVSize - 1] of Byte;
    MACSize       : Integer;
    PadSize       : Integer;
    CalcMAC       : array[0..TLS_MAC_MAXDIGESTSIZE - 1] of Byte;
    RecMAC        : Pointer;
    MACOk         : Boolean;
    MACIdx        : Integer;
    ComprHdr      : TTLSRecordHeader;
    P, Q          : PByte;
begin
  // initialise parameters
  HasCipher := CipherSuiteDetails.CipherSuite <> tlscsNone;
  IsBlockCipher :=
      HasCipher and
      (CipherSuiteDetails.CipherInfo^.CipherType = tlscsctBlock);
  UseIV :=
      IsBlockCipher and
      (CipherSuiteDetails.CipherInfo^.IVSize > 0);
  if UseIV then
    IVSize := CipherSuiteDetails.CipherInfo^.IVSize
  else
    IVSize := 0;
  Assert(IVSize <= TLS_CIPHERSUITE_MaxIVSize);
  if UseIV and (IVBufSize < IVSize) then
    raise ETLSError.Create(TLSError_InvalidBuffer);

  if HasCipher then
    begin
      // decode Buffer
      BufP := @Buffer;
      BufPLeft := Size;

      // TLS 1.2: get IV field from unencrypted buffer
      if UseIV then
        if IsTLS12OrLater(ProtocolVersion) then
          begin
            Move(BufP^, IVBufPtr^, IVSize);
            Inc(BufP, IVSize);
            Dec(BufPLeft, IVSize);
          end;

      // TLS 1.0: get IV from encrypted block
      if UseIV then
        if IsTLS10(ProtocolVersion) then
          tls10UpdateIV(BufP^, BufPLeft, NextIV, IVSize);

      // TLS 1.1: IV is first encrypted block

      // decrypt from Buffer to BufCipher
      TLSCipherBuf(CipherState,
          BufP^, BufPLeft,
          BufCipher, SizeOf(BufCipher),
          CipherSize,
          IVBufPtr, IVSize);

      // decode decrypted Buffer
      BufQ := @BufCipher;
      BufQLeft := CipherSize;

      // TLS 1.1: skip over IV field
      if UseIV then
        if IsTLS11(ProtocolVersion) then
          begin
            Move(BufQ^, IVBufPtr^, IVSize);
            Inc(BufQ, IVSize);
            Dec(BufQLeft, IVSize);
          end;

      // TLS 1.0: update IV
      if UseIV then
        if IsTLS10(ProtocolVersion) then
          Move(NextIV, IVBufPtr^, IVSize);

      // decode padding
      if IsBlockCipher then
        begin
          PadSize := DecodeTLSGenericBlockCipherPadding(BufQ^, BufQLeft);
          Dec(BufQLeft, PadSize);
        end;

      // update size for MAC
      MACSize := CipherSuiteDetails.HashInfo^.HashSize div 8;
      Dec(BufQLeft, MACSize);

      // compressed content
      RecContent := BufQ;
      ComprSize := BufQLeft;

      // Calculate MAC
      InitTLSRecordHeader(ComprHdr, RecHeader^._type, RecHeader^.version, ComprSize);
      GenerateRecordPayloadMAC(
          ProtocolVersion,
          CipherSuiteDetails,
          MACKey, MACKeySize,
          SequenceNumber,
          @ComprHdr,
          RecContent^, ComprSize,
          CalcMAC, MACSize);

      // get MAC
      Inc(BufQ, ComprSize);
      RecMAC := BufQ;

      // validate MAC
      P := @CalcMAC;
      Q := RecMAC;
      MACOk := True;
      for MACIdx := 0 to MACSize - 1 do
        if P^ <> Q^ then
          begin
            MACOk := False;
            break;
          end
        else
          begin
            Inc(P);
            Inc(Q);
          end;
      if not MACOk then
        raise ETLSError.Create(TLSError_DecodeError);
    end
  else
    begin
      Move(Buffer, BufCipher, Size);
      RecContent := @BufCipher;
      ComprSize := Size;
    end;

  // decompress from BufCipher to BufPlain
  TLSDecompressFragment(
      CompressionMethod,
      RecContent^, ComprSize,
      BufPlain, SizeOf(BufPlain),
      PlainSize);

  // plain text
  Move(BufPlain, ContentBuffer, PlainSize);
  ContentSize := PlainSize;
end;



{                                                                              }
{ Test cases                                                                   }
{                                                                              }
//// TBytes
{$IFDEF TLS_TEST}
{$ASSERTIONS ON}
procedure SelfTestPayloadMAC;
const
  MACWriteKey = RawByteString(#$85#$F0#$56#$F8#$07#$1D#$B1#$89#$89#$D0#$E1#$33#$3C#$CA#$63#$F9);
var S, T, D : RawByteString;
    L : Integer;
begin
  //                                                                                              //
  // Example from http://download.oracle.com/javase/1.5.0/docs/guide/security/jsse/ReadDebug.html //
  //                                                                                              //
  T := MACWriteKey;
  D := RawByteString(
      #$00#$00#$00#$00#$00#$00#$00#$00 +                                  // seq_num
      #$16#$03#$01#$00#$10 +                                              // compressed hdr
      #$14#$00#$00#$0C#$F2#$62#$42#$AA#$7C#$7C#$CC#$E7#$49#$0F#$ED#$AC);  // handshake msg
  SetLength(S, 256);
  L := GenerateTLSRecordPayloadMAC(tlsmaHMAC_MD5, T[1], Length(T), D[1], Length(D), S[1], Length(S));
  Assert(L = 16);
  SetLength(S, L);
  Assert(S = #$FA#$06#$3C#$9F#$8C#$41#$1D#$ED#$2B#$06#$D0#$5A#$ED#$31#$F2#$80);
end;

procedure Test;
begin
  Assert(TLSContentTypeSize = 1);
  Assert(TLSRecordHeaderSize = 5);
  SelfTestPayloadMAC;
end;
{$ENDIF}



end.



