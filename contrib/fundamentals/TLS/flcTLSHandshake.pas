{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals TLS                                         }
{   File name:        flcTLSHandshake.pas                                      }
{   File version:     5.04                                                     }
{   Description:      TLS handshake protocol                                   }
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
{   2008/01/18  0.01  Initial development: Headers                             }
{   2010/11/26  0.02  Initial development: Messages                            }
{   2016/01/08  0.03  String changes                                           }
{   2018/07/17  5.04  Revised for Fundamentals 5.                              }
{                                                                              }
{******************************************************************************}

{$INCLUDE flcTLS.inc}

unit flcTLSHandshake;

interface

uses
  { Utils }

  flcStdTypes,

  { Cipher }

  flcCipherRSA,

  { TLS }

  flcTLSProtocolVersion,
  flcTLSAlgorithmTypes,
  flcTLSRandom,
  flcTLSCipherSuite,
  flcTLSSessionID,
  flcTLSKeyExchangeParams,
  flcTLSCertificate,
  flcTLSHandshakeExtension;



{                                                                              }
{ HandshakeHeader                                                              }
{                                                                              }
type
  TTLSHandshakeType = (
    tlshtHello_request        = 0,    // Not used in TLS 1.3
    tlshtClient_hello         = 1,
    tlshtServer_hello         = 2,
    tlshtNew_session_ticket   = 4,    // TLS 1.3
    tlshtEnd_of_early_data    = 5,    // TLS 1.3
    tlshtEncrypted_extensions = 8,    // TLS 1.3
    tlshtCertificate          = 11,
    tlshtServer_key_exchange  = 12,   // Not used in TLS 1.3
    tlshtCertificate_request  = 13,
    tlshtServer_hello_done    = 14,   // Not used in TLS 1.3
    tlshtCertificate_verify   = 15,
    tlshtClient_key_exchange  = 16,   // Not used in TLS 1.3
    tlshtFinished             = 20,
    tlshtCertificate_url      = 21,   // RFC 6066  // Not used in TLS 1.3
    tlshtCertificate_status   = 22,   // RFC 6066  // Not used in TLS 1.3
    tlshtKey_update           = 24,   // TLS 1.3
    tlshtMessage_hash         = 254,  // TLS 1.3
    tlshtMax                  = 255
    );

  TTLSHandshakeHeader = packed record
    msg_type : TTLSHandshakeType;    // handshake type
    length   : array[0..2] of Byte;  // bytes in message
  end;
  PTLSHandshakeHeader = ^TTLSHandshakeHeader;

const
  TLSHandshakeHeaderSize = Sizeof(TTLSHandshakeHeader);

function  TLSHandshakeTypeToStr(const A: TTLSHandshakeType): String;

procedure EncodeTLSHandshakeHeader(
          var Handshake: TTLSHandshakeHeader;
          const MsgType: TTLSHandshakeType;
          const Length: Integer);
procedure DecodeTLSHandshakeHeader(
          const Handshake: TTLSHandshakeHeader;
          var MsgType: TTLSHandshakeType;
          var Length: Integer);

function  TLSHandshakeDataPtr(
          const Buffer; const Size: Integer;
          var DataSize: Integer): Pointer;
function  EncodeTLSHandshake(
          var Buffer; const Size: Integer;
          const MsgType: TTLSHandshakeType; const MsgSize: Integer): Integer;



{                                                                              }
{ HelloRequest                                                                 }
{                                                                              }
function  EncodeTLSHandshakeHelloRequest(var Buffer; const Size: Integer): Integer;



{                                                                              }
{ ClientHello                                                                  }
{                                                                              }
type
  TTLSClientHello = record
    ProtocolVersion    : TTLSProtocolVersion;
    SessionID          : TTLSSessionID;
    CipherSuites       : TTLSCipherSuites;
    CompressionMethods : TTLSCompressionMethods;
    Random             : TTLSRandom;
    ExtensionsPresent  : Boolean;
    SignAndHashAlgos   : TTLSSignatureAndHashAlgorithmArray;
  end;

procedure InitTLSClientHello(
          var ClientHello: TTLSClientHello;
          const ProtocolVersion: TTLSProtocolVersion;
          const SessionID: RawByteString);

function  EncodeTLSClientHello(
          var Buffer; const Size: Integer;
          var ClientHello: TTLSClientHello): Integer;

function  DecodeTLSClientHello(
          const Buffer; const Size: Integer;
          var ClientHello: TTLSClientHello): Integer;

function  EncodeTLSHandshakeClientHello(
          var Buffer; const Size: Integer;
          var ClientHello: TTLSClientHello): Integer;



{                                                                              }
{ ServerHello                                                                  }
{                                                                              }
type
  TTLSServerHello = record
    ProtocolVersion   : TTLSProtocolVersion;
    SessionID         : TTLSSessionID;
    CipherSuite       : TTLSCipherSuiteRec;
    CompressionMethod : TTLSCompressionMethod;
    Random            : TTLSRandom;
  end;

procedure InitTLSServerHello(
          var ServerHello: TTLSServerHello;
          const ProtocolVersion: TTLSProtocolVersion;
          const SessionID: RawByteString;
          const CipherSuite: TTLSCipherSuiteRec;
          const CompressionMethod: TTLSCompressionMethod);

function  EncodeTLSServerHello(
          var Buffer; const Size: Integer;
          const ServerHello: TTLSServerHello): Integer;

function  DecodeTLSServerHello(
          const Buffer; const Size: Integer;
          var ServerHello: TTLSServerHello): Integer;

function  EncodeTLSHandshakeServerHello(
          var Buffer; const Size: Integer;
          const ServerHello: TTLSServerHello): Integer;



{                                                                              }
{ Certificate                                                                  }
{                                                                              }
function  EncodeTLSHandshakeCertificate(
          var Buffer; const Size: Integer;
          const CertificateList: TTLSCertificateList): Integer;



{                                                                              }
{ ServerKeyExchange                                                            }
{                                                                              }
function  EncodeTLSHandshakeServerKeyExchange(
          var Buffer; const Size: Integer;
          const KeyExchangeAlgorithm: TTLSKeyExchangeAlgorithm;
          const ServerKeyExchange: TTLSServerKeyExchange): Integer;



{                                                                              }
{ CertificateRequest                                                           }
{                                                                              }
type
  TTLSClientCertificateTypeArray = array of TTLSClientCertificateType;

  TTLSCertificateRequest = record
    CertificateTypes             : TTLSClientCertificateTypeArray;
    SupportedSignatureAlgorithms : TTLSSignatureAndHashAlgorithmArray;
    DistinguishedName            : RawByteString;
  end;

function  EncodeTLSCertificateRequest(
          var Buffer; const Size: Integer;
          const CertificateRequest: TTLSCertificateRequest): Integer;

function  DecodeTLSCertificateRequest(
          const Buffer; const Size: Integer;
          var CertificateRequest: TTLSCertificateRequest): Integer;

function  EncodeTLSHandshakeCertificateRequest(
          var Buffer; const Size: Integer;
          const CertificateRequest: TTLSCertificateRequest): Integer;



{                                                                              }
{ ServerHelloDone                                                              }
{                                                                              }
function  EncodeTLSHandshakeServerHelloDone(var Buffer; const Size: Integer): Integer;



{                                                                              }
{ CertificateVerify                                                            }
{                                                                              }
function  EncodeTLSHandshakeCertificateVerify(var Buffer; const Size: Integer): Integer;



{                                                                              }
{ PreMasterSecret                                                              }
{                                                                              }
type
  TTLSPreMasterSecret = packed record
    client_version : TTLSProtocolVersion;
    random         : array[0..45] of Byte;
  end;

const
  TLSPreMasterSecretSize = SizeOf(TTLSPreMasterSecret);

procedure InitTLSPreMasterSecret_Random(
          var PreMasterSecret: TTLSPreMasterSecret;
          const ClientVersion: TTLSProtocolVersion);

function  TLSPreMasterSecretToStr(const PreMasterSecret: TTLSPreMasterSecret): RawByteString;

procedure InitTLSEncryptedPreMasterSecret_RSA(
          var EncryptedPreMasterSecret: RawByteString;
          const PreMasterSecret: TTLSPreMasterSecret;
          const RSAPublicKey: TRSAPublicKey);



{                                                                              }
{ ClientKeyExchange                                                            }
{                                                                              }
function  EncodeTLSHandshakeClientKeyExchange(
          var Buffer; const Size: Integer;
          const KeyExchangeAlgorithm: TTLSKeyExchangeAlgorithm;
          const ClientKeyExchange: TTLSClientKeyExchange): Integer;



{                                                                              }
{ Finished                                                                     }
{                                                                              }
function  EncodeTLSHandshakeFinished(var Buffer; const Size: Integer;
          const MasterSecret: RawByteString;
          const ProtocolVersion: TTLSProtocolVersion;
          const HandshakeData: RawByteString;
          const SenderIsClient: Boolean): Integer;



{                                                                              }
{ ChangeCipherSpec Protocol                                                    }
{                                                                              }
type
  TTLSChangeCipherSpecType = (
    spectype_change_cipher_spec = 1);
  TTLSChangeCipherSpec = packed record
    _type : TTLSChangeCipherSpecType;
  end;
  PTLSChangeCipherSpec = ^TTLSChangeCipherSpec;

const
  TLSChangeCipherSpecSize = SizeOf(TTLSChangeCipherSpec);

procedure InitTLSChangeCipherSpec(var ChangeCipherSpec: TTLSChangeCipherSpec);



{                                                                              }
{ Test                                                                         }
{                                                                              }
{$IFDEF TLS_TEST}
procedure Test;
{$ENDIF}



implementation

uses
  { System }

  SysUtils,

  { Utils }

  flcStrings,
  flcHash,

  { Cipher }

  flcCipherRandom,

  { TLS }

  flcTLSAlert,
  flcTLSErrors,
  flcTLSPRF,
  flcTLSOpaqueEncoding;



{                                                                              }
{ Handshake Header                                                             }
{                                                                              }
function TLSHandshakeTypeToStr(const A: TTLSHandshakeType): String;
begin
  case A of
    tlshtHello_request       : Result := 'Hello request';
    tlshtClient_hello        : Result := 'Client hello';
    tlshtServer_hello        : Result := 'Server hello';
    tlshtCertificate         : Result := 'Certificate';
    tlshtServer_key_exchange : Result := 'Server key exchange';
    tlshtCertificate_request : Result := 'Certficiate request';
    tlshtServer_hello_done   : Result := 'Server hello done';
    tlshtCertificate_verify  : Result := 'Certificate verify';
    tlshtClient_key_exchange : Result := 'Client key exchange';
    tlshtFinished            : Result := 'Finished';
  else
    Result := '[Handshake#' + IntToStr(Ord(A)) + ']';
  end;
end;

procedure EncodeTLSHandshakeHeader(
          var Handshake: TTLSHandshakeHeader;
          const MsgType: TTLSHandshakeType;
          const Length: Integer);
var I : Byte;
    L : Integer;
begin
  Assert(Length >= 0);
  Assert(Length <= $FFFFFF);
  Handshake.msg_type := MsgType;
  L := Length;
  I := L div $10000;
  Handshake.length[0] := I;
  L := L - I * $10000;
  I := L div $100;
  Handshake.length[1] := I;
  L := L - I * $100;
  I := Byte(L);
  Handshake.length[2] := I;
end;

procedure DecodeTLSHandshakeHeader(
          const Handshake: TTLSHandshakeHeader;
          var MsgType: TTLSHandshakeType;
          var Length: Integer);
begin
  MsgType := Handshake.msg_type;
  Length :=
      Handshake.length[0] * $10000 +
      Handshake.length[1] * $100 +
      Handshake.length[2];
end;

function TLSHandshakeDataPtr(
         const Buffer; const Size: Integer;
         var DataSize: Integer): Pointer;
var P : PByte;
begin
  P := @Buffer;
  if not Assigned(P) or (Size < TLSHandshakeHeaderSize) then
    raise ETLSError.Create(TLSError_InvalidBuffer);
  DataSize := Size - TLSHandshakeHeaderSize;
  Inc(P, TLSHandshakeHeaderSize);
  Result := P;
end;

function EncodeTLSHandshake(
         var Buffer; const Size: Integer;
         const MsgType: TTLSHandshakeType; const MsgSize: Integer): Integer;
var L : Integer;
    P : PByte;
begin
  Assert(MsgSize >= 0);
  P := @Buffer;
  L := TLSHandshakeHeaderSize + MsgSize;
  if not Assigned(P) or (Size < L) then
    raise ETLSError.CreateAlertBufferEncode;
  EncodeTLSHandshakeHeader(PTLSHandshakeHeader(P)^, MsgType, MsgSize);
  Result := L;
end;



{                                                                              }
{ Hello Request                                                                }
{                                                                              }
function EncodeTLSHandshakeHelloRequest(var Buffer; const Size: Integer): Integer;
begin
  Result := EncodeTLSHandshake(Buffer, Size, tlshtHello_request, 0);
end;



{                                                                              }
{ ClientHello                                                                  }
{   client_version                : TTLSProtocolVersion;                       }
{   random                        : TTLSRandom;                                }
{   session_id                    : TLSSessionID;                              }
{   cipher_suites_count           : Word;                                      }
{   cipher_suites                 : <2..2^16-1> TTLSCipherSuite;               }
{   compression_methods_count     : Byte;                                      }
{   compression_methods           : <1..2^8-1> TTLSCompressionMethod;          }
{   select (extensions_present)                                                }
{       case false: ;                                                          }
{       case true:  Extension extensions<0..2^16-1>;                           }
{                                                                              }
procedure InitTLSClientHello(
          var ClientHello: TTLSClientHello;
          const ProtocolVersion: TTLSProtocolVersion;
          const SessionID: RawByteString);
begin
  ClientHello.ProtocolVersion := ProtocolVersion;
  InitTLSSessionID(ClientHello.SessionID, SessionID);
  InitTLSRandom(ClientHello.Random);
end;

function EncodeTLSClientHello(
         var Buffer; const Size: Integer;
         var ClientHello: TTLSClientHello): Integer;
var P       : PByte;
    L, N    : Integer;
    C       : TTLSCipherSuite;
    D       : TTLSCompressionMethod;
    Suite   : TTLSCipherSuiteRec;
    SignRSA : Boolean;
begin
  Assert(Assigned(@Buffer));

  N := Size;
  P := @Buffer;

  // client_version
  Dec(N, TLSProtocolVersionSize);
  if N < 0 then
    raise ETLSError.CreateAlertBufferEncode;
  PTLSProtocolVersion(P)^ := ClientHello.ProtocolVersion;
  Inc(P, TLSProtocolVersionSize);
  // random
  Dec(N, TLSRandomSize);
  if N < 0 then
    raise ETLSError.CreateAlertBufferEncode;
  PTLSRandom(P)^ := ClientHello.Random;
  Inc(P, TLSRandomSize);
  // session_id
  L := EncodeTLSSessionID(P^, N, ClientHello.SessionID);
  Dec(N, L);
  Inc(P, L);
  // cipher_suites
  L := 0;
  for C := Low(TTLSCipherSuite) to High(TTLSCipherSuite) do
    if C in ClientHello.CipherSuites then
      Inc(L);
  Assert(L <= $FFFF);
  EncodeTLSLen16(P^, N, L * TLSCipherSuiteRecSize);
  Inc(P, 2);
  Dec(N, 2 + L * TLSCipherSuiteRecSize);
  if N < 0 then
    raise ETLSError.CreateAlertBufferEncode;
  SignRSA := False;
  for C := Low(TTLSCipherSuite) to High(TTLSCipherSuite) do
    if C in ClientHello.CipherSuites then
      begin
        Suite.B1 := TLSCipherSuiteInfo[C].Rec.B1;
        Suite.B2 := TLSCipherSuiteInfo[C].Rec.B2;
        PTLSCipherSuiteRec(P)^ := Suite;
        Inc(P, TLSCipherSuiteRecSize);
        if TLSCipherSuiteInfo[C].Authentication = tlscsaRSA then
          SignRSA := True;
      end;
  // compression_methods
  L := 0;
  for D := Low(TTLSCompressionMethod) to High(TTLSCompressionMethod) do
    if D in ClientHello.CompressionMethods then
      Inc(L);
  Assert(L <= $FF);
  Dec(N, 1 + L);
  if N < 0 then
    raise ETLSError.CreateAlertBufferEncode;
  P^ := L;
  Inc(P);
  for D := Low(TTLSCompressionMethod) to High(TTLSCompressionMethod) do
    if D in ClientHello.CompressionMethods then
      begin
        P^ := Ord(D);
        Inc(P);
      end;
  // extensions
  {
  if IsTLS12(ClientHello.ProtocolVersion) then
    if SignRSA then
      begin
        SetLength(ClientHello.SignAndHashAlgos, 5);
        ClientHello.SignAndHashAlgos[0].Hash := tlshaSHA1;
        ClientHello.SignAndHashAlgos[0].Signature := tlssaRSA;
        ClientHello.SignAndHashAlgos[1].Hash := tlshaSHA224;
        ClientHello.SignAndHashAlgos[1].Signature := tlssaRSA;
        ClientHello.SignAndHashAlgos[2].Hash := tlshaSHA256;
        ClientHello.SignAndHashAlgos[2].Signature := tlssaRSA;
        ClientHello.SignAndHashAlgos[3].Hash := tlshaSHA384;
        ClientHello.SignAndHashAlgos[3].Signature := tlssaRSA;
        ClientHello.SignAndHashAlgos[4].Hash := tlshaSHA512;
        ClientHello.SignAndHashAlgos[4].Signature := tlssaRSA;
        ClientHello.ExtensionsPresent := True;
        L := EncodeTLSClientHelloExtension_SignatureAlgorithms(P^, N, ClientHello);
        Dec(N, L);
      end;
    }
  Result := Size - N;
end;

function DecodeTLSClientHello(
         const Buffer; const Size: Integer;
         var ClientHello: TTLSClientHello): Integer;
var P : PByte;
    L, N, C, ExtType, ExtLen : Integer;
    I : Word;
    E : TTLSCipherSuite;
    F : TTLSCipherSuiteRec;
    D : TTLSCompressionMethod;
begin
  Assert(Assigned(@Buffer));
  Assert(Size >= 0);

  N := Size;
  P := @Buffer;

  // client_version
  Dec(N, TLSProtocolVersionSize);
  if N < 0 then
    raise ETLSError.CreateAlertBufferDecode;
  ClientHello.ProtocolVersion := PTLSProtocolVersion(P)^;
  Inc(P, TLSProtocolVersionSize);
  // random
  Dec(N, TLSRandomSize);
  if N < 0 then
    raise ETLSError.CreateAlertBufferDecode;
  ClientHello.Random := PTLSRandom(P)^;
  Inc(P, TLSRandomSize);
  // session_id
  L := DecodeTLSSessionID(P^, N, ClientHello.SessionID);
  Dec(N, L);
  Inc(P, L);
  // cipher_suites
  ClientHello.CipherSuites := [];
  L := DecodeTLSLen16(P^, N, C);
  Dec(N, L);
  Inc(P, L);
  Dec(N, C);
  if N < 0 then
    raise ETLSError.CreateAlertBufferDecode;
  C := C div TLSCipherSuiteRecSize;
  for I := 0 to C - 1 do
    begin
      F := PTLSCipherSuiteRec(P)^;
      E := GetCipherSuiteByRec(F.B1, F.B2);
      Include(ClientHello.CipherSuites, E);
      Inc(P, TLSCipherSuiteRecSize);
    end;
  // compression_methods
  Dec(N);
  if N < 0 then
    raise ETLSError.CreateAlertBufferDecode;
  ClientHello.CompressionMethods := [];
  C := P^;
  Inc(P);
  Dec(N, C);
  if N < 0 then
    raise ETLSError.CreateAlertBufferDecode;
  for I := 0 to C - 1 do
    begin
      D := PTLSCompressionMethod(P)^;
      Include(ClientHello.CompressionMethods, D);
      Inc(P, TLSCompressionMethodSize);
    end;
  // extensions
  ClientHello.ExtensionsPresent := N > 0;
  while N > 0 do
    begin
      if N < 4 then
        raise ETLSError.CreateAlertBufferDecode;
      DecodeTLSWord16(P^, N, ExtType);
      Inc(P, 2);
      Dec(N, 2);
      DecodeTLSLen16(P^, N, ExtLen);
      Inc(P, 2);
      Dec(N, 2);
      Dec(N, ExtLen);
      if N < 0 then
        raise ETLSError.CreateAlertBufferDecode;
      Inc(P, ExtLen);
    end;

  Result := Size - N;
end;

function EncodeTLSHandshakeClientHello(
         var Buffer; const Size: Integer;
         var ClientHello: TTLSClientHello): Integer;
var P : Pointer;
    N, L : Integer;
begin
  P := TLSHandshakeDataPtr(Buffer, Size, N);
  L := EncodeTLSClientHello(P^, N, ClientHello);
  Result := EncodeTLSHandshake(Buffer, Size, tlshtClient_hello, L);
end;



{                                                                              }
{ ServerHello                                                                  }
{   server_version     : TTLSProtocolVersion;                                  }
{   random             : TTLSRandom;                                           }
{   session_id         : TLSSessionID;                                         }
{   cipher_suite       : TTLSCipherSuite;                                      }
{   compression_method : TTLSCompressionMethod;                                }
{   select (extensions_present)                                                }
{       case false: ;                                                          }
{       case true:  Extension extensions<0..2^16-1>;                           }
{                                                                              }
procedure InitTLSServerHello(
          var ServerHello: TTLSServerHello;
          const ProtocolVersion: TTLSProtocolVersion;
          const SessionID: RawByteString;
          const CipherSuite: TTLSCipherSuiteRec;
          const CompressionMethod: TTLSCompressionMethod);
begin
  ServerHello.ProtocolVersion := ProtocolVersion;
  InitTLSSessionID(ServerHello.SessionID, SessionID);
  ServerHello.CipherSuite := CipherSuite;
  ServerHello.CompressionMethod := CompressionMethod;
  InitTLSRandom(ServerHello.Random);
end;

function EncodeTLSServerHello(
         var Buffer; const Size: Integer;
         const ServerHello: TTLSServerHello): Integer;
var P : PByte;
    L, N : Integer;
begin
  Assert(Assigned(@Buffer));
  Assert(Size >= 0);

  N := Size;
  P := @Buffer;
  // server_version
  Dec(N, TLSProtocolVersionSize);
  if N < 0 then
    raise ETLSError.CreateAlertBufferEncode;
  PTLSProtocolVersion(P)^ := ServerHello.ProtocolVersion;
  Inc(P, TLSProtocolVersionSize);
  // random
  Dec(N, TLSRandomSize);
  if N < 0 then
    raise ETLSError.CreateAlertBufferEncode;
  PTLSRandom(P)^ := ServerHello.Random;
  Inc(P, TLSRandomSize);
  // session_id
  L := EncodeTLSSessionID(P^, N, ServerHello.SessionID);
  Dec(N, L);
  Inc(P, L);
  // cipher_suite
  Dec(N, TLSCipherSuiteRecSize);
  if N < 0 then
    raise ETLSError.CreateAlertBufferEncode;
  PTLSCipherSuiteRec(P)^ := ServerHello.CipherSuite;
  Inc(P, TLSCipherSuiteRecSize);
  // compression_method
  Dec(N, TLSCompressionMethodSize);
  if N < 0 then
    raise ETLSError.CreateAlertBufferEncode;
  PTLSCompressionMethod(P)^ := ServerHello.CompressionMethod;

  Result := Size - N;
end;

function DecodeTLSServerHello(
         const Buffer; const Size: Integer;
         var ServerHello: TTLSServerHello): Integer;
var P : PByte;
    L, N : Integer;
begin
  Assert(Assigned(@Buffer));
  Assert(Size >= 0);

  N := Size;
  P := @Buffer;
  // server_version
  Dec(N, TLSProtocolVersionSize);
  if N < 0 then
    raise ETLSError.CreateAlertBufferDecode;
  ServerHello.ProtocolVersion := PTLSProtocolVersion(P)^;
  Inc(P, TLSProtocolVersionSize);
  // random
  Dec(N, TLSRandomSize);
  if N < 0 then
    raise ETLSError.CreateAlertBufferDecode;
  ServerHello.Random := PTLSRandom(P)^;
  Inc(P, TLSRandomSize);
  // session_id
  L := DecodeTLSSessionID(P^, N, ServerHello.SessionID);
  Dec(N, L);
  Inc(P, L);
  // cipher_suite
  Dec(N, TLSCipherSuiteRecSize);
  if N < 0 then
    raise ETLSError.CreateAlertBufferDecode;
  ServerHello.CipherSuite := PTLSCipherSuiteRec(P)^;
  Inc(P, TLSCipherSuiteRecSize);
  // compression_method
  Dec(N, TLSCompressionMethodSize);
  if N < 0 then
    raise ETLSError.CreateAlertBufferDecode;
  ServerHello.CompressionMethod := PTLSCompressionMethod(P)^;

  Result := Size - N;
end;

function EncodeTLSHandshakeServerHello(
         var Buffer; const Size: Integer;
         const ServerHello: TTLSServerHello): Integer;
var P : Pointer;
    N, L : Integer;
begin
  P := TLSHandshakeDataPtr(Buffer, Size, N);
  L := EncodeTLSServerHello(P^, N, ServerHello);
  Result := EncodeTLSHandshake(Buffer, Size, tlshtServer_hello, L);
end;



{                                                                              }
{ Certificate                                                                  }
{   certificate_list  : <0..2^24-1> ASN.1Cert;                                 }
{                                                                              }
{ ASN.1Cert = <1..2^24-1> opaque;                                              }
{                                                                              }
function EncodeTLSHandshakeCertificate(
         var Buffer; const Size: Integer;
         const CertificateList: TTLSCertificateList): Integer;
var P : Pointer;
    N, L : Integer;
begin
  P := TLSHandshakeDataPtr(Buffer, Size, N);
  L := EncodeTLSCertificate(P^, N, CertificateList);
  Result := EncodeTLSHandshake(Buffer, Size, tlshtCertificate, L);
end;




{                                                                              }
{ ServerKeyExchange                                                            }
{                                                                              }
function EncodeTLSHandshakeServerKeyExchange(
         var Buffer; const Size: Integer;
         const KeyExchangeAlgorithm: TTLSKeyExchangeAlgorithm;
         const ServerKeyExchange: TTLSServerKeyExchange): Integer;
var P : Pointer;
    N, L : Integer;
begin
  P := TLSHandshakeDataPtr(Buffer, Size, N);
  L := EncodeTLSServerKeyExchange(P^, N, KeyExchangeAlgorithm, ServerKeyExchange);
  Result := EncodeTLSHandshake(Buffer, Size, tlshtServer_key_exchange, L);
end;



{                                                                              }
{ Certificate Request                                                          }
{     certificate_types              : ClientCertificateType <1..2^8-1>;       }
{     supported_signature_algorithms : SignatureAndHashAlgorithm <2^16-1>;     }
{     certificate_authorities        : DistinguishedName <0..2^16-1>;          }
{ DistinguishedName = opaque <1..2^16-1>                                       }
{                                                                              }
function EncodeTLSCertificateRequest(
         var Buffer; const Size: Integer;
         const CertificateRequest: TTLSCertificateRequest): Integer;
var P : PByte;
    N, L, I : Integer;
begin
  Assert(Assigned(@Buffer));
  Assert(Size >= 0);

  N := Size;
  P := @Buffer;
  // certificate_types
  L := Length(CertificateRequest.CertificateTypes);
  if (L <= 0) or (L > 255) then
    raise ETLSError.Create(TLSError_InvalidParameter);
  Dec(N, 1 + L);
  if N < 0 then
    raise ETLSError.Create(TLSError_InvalidBuffer);
  P^ := L;
  Inc(P);
  for I := 0 to L -  1 do
    begin
      P^ := Ord(CertificateRequest.CertificateTypes[I]);
      Inc(P);
    end;
  // supported_signature_algorithms
  L := Length(CertificateRequest.SupportedSignatureAlgorithms);
  EncodeTLSLen16(P^, N, L * TLSSignatureAndHashAlgorithmSize);
  Inc(P, 2);
  Dec(N, 2 + L * TLSSignatureAndHashAlgorithmSize);
  if N < 0 then
    raise ETLSError.Create(TLSError_InvalidBuffer);
  for I := 0 to L - 1 do
    begin
      PTLSSignatureAndHashAlgorithm(P)^ := CertificateRequest.SupportedSignatureAlgorithms[I];
      Inc(P, TLSSignatureAndHashAlgorithmSize);
    end;
  // certificate_authorities
  L := EncodeTLSOpaque16(P^, N, CertificateRequest.DistinguishedName);
  Dec(N, L);
  Result := Size - N;
end;

function DecodeTLSCertificateRequest(
         const Buffer; const Size: Integer;
         var CertificateRequest: TTLSCertificateRequest): Integer;
var P : PByte;
    N, L, I : Integer;
begin
  Assert(Assigned(@Buffer));
  Assert(Size >= 0);

  N := Size;
  P := @Buffer;
  // certificate_types
  Dec(N);
  if N < 0 then
    raise ETLSError.CreateAlertBufferDecode;
  L := P^;
  Inc(P);
  Dec(N, L);
  if N < 0 then
    raise ETLSError.CreateAlertBufferDecode;
  SetLength(CertificateRequest.CertificateTypes, L);
  for I := 0 to L - 1 do
    begin
      CertificateRequest.CertificateTypes[I] := TTLSClientCertificateType(P^);
      Inc(P);
    end;
  // supported_signature_algorithms
  DecodeTLSLen16(P^, N, L);
  Assert(L mod TLSSignatureAndHashAlgorithmSize = 0);
  L := L div TLSSignatureAndHashAlgorithmSize;
  Inc(P, 2);
  Dec(N, 2 + L * TLSSignatureAndHashAlgorithmSize);
  if N < 0 then
    raise ETLSError.CreateAlertBufferDecode;
  SetLength(CertificateRequest.SupportedSignatureAlgorithms, L);
  for I := 0 to L - 1 do
    begin
      CertificateRequest.SupportedSignatureAlgorithms[I] := PTLSSignatureAndHashAlgorithm(P)^;
      Inc(P, TLSSignatureAndHashAlgorithmSize);
    end;
  // certificate_authorities
  L := DecodeTLSOpaque16(P^, N, CertificateRequest.DistinguishedName);
  Dec(N, L);
  Result := Size - N;
end;

function EncodeTLSHandshakeCertificateRequest(
         var Buffer; const Size: Integer;
         const CertificateRequest: TTLSCertificateRequest): Integer;
var P : Pointer;
    N, L : Integer;
    C : TTLSCertificateRequest;
begin
  P := TLSHandshakeDataPtr(Buffer, Size, N);
  L := EncodeTLSCertificateRequest(P^, N, C);
  Result := EncodeTLSHandshake(Buffer, Size, tlshtCertificate_request, L);
end;



{                                                                              }
{ Server Hello Done                                                            }
{                                                                              }
function EncodeTLSHandshakeServerHelloDone(var Buffer; const Size: Integer): Integer;
begin
  Result := EncodeTLSHandshake(Buffer, Size, tlshtServer_hello_done, 0);
end;



{                                                                              }
{ Certificate Verify                                                           }
{                                                                              }
function EncodeTLSHandshakeCertificateVerify(var Buffer; const Size: Integer): Integer;
begin
  Result := EncodeTLSHandshake(Buffer, Size, tlshtCertificate_verify, 0);
end;



{                                                                              }
{ PreMasterSecret                                                              }
{   client_version : ProtocolVersion;                                          }
{   random         : opaque[46];                                               }
{                                                                              }
{ EncryptedPreMasterSecret = public-key-encrypted PreMasterSecret              }
{ Public-key-encrypted data is represented as an opaque vector <0..2^16-1>.    }
{ Thus, the RSA-encrypted PreMasterSecret in a ClientKeyExchange is preceded   }
{ by two length bytes.                                                         }
{                                                                              }
procedure InitTLSPreMasterSecret_Random(var PreMasterSecret: TTLSPreMasterSecret;
          const ClientVersion: TTLSProtocolVersion);
begin
  PreMasterSecret.client_version := ClientVersion;
  SecureRandomBuf(PreMasterSecret.random, SizeOf(PreMasterSecret.random));
end;

function TLSPreMasterSecretToStr(const PreMasterSecret: TTLSPreMasterSecret): RawByteString;
begin
  SetLength(Result, TLSPreMasterSecretSize);
  Move(PreMasterSecret, Result[1], TLSPreMasterSecretSize);
end;

procedure InitTLSEncryptedPreMasterSecret_RSA(
          var EncryptedPreMasterSecret: RawByteString;
          const PreMasterSecret: TTLSPreMasterSecret;
          const RSAPublicKey: TRSAPublicKey);
var B : array[0..1023] of Byte;
    L : Integer;
begin
  L := RSAEncrypt(rsaetRSAES_PKCS1, RSAPublicKey, PreMasterSecret, SizeOf(PreMasterSecret),
      B, SizeOf(B));
  SetLength(EncryptedPreMasterSecret, L + 2);
  EncodeTLSLen16(EncryptedPremasterSecret[1], L + 2, L);
  Move(B, EncryptedPreMasterSecret[3], L);
end;



{                                                                              }
{ ClientKeyExchange                                                            }
{                                                                              }
{   select (KeyExchangeAlgorithm)                                              }
{       case rsa     : EncryptedPreMasterSecret;                               }
{       case dhe_dss :                                                         }
{       case dhe_rsa :                                                         }
{       case dh_dss  :                                                         }
{       case dh_rsa  :                                                         }
{       case dh_anon : ClientDiffieHellmanPublic;                              }
{                                                                              }
function EncodeTLSHandshakeClientKeyExchange(
         var Buffer; const Size: Integer;
         const KeyExchangeAlgorithm: TTLSKeyExchangeAlgorithm;
         const ClientKeyExchange: TTLSClientKeyExchange): Integer;
var N, L : Integer;
    P : PByte;
begin
  P := TLSHandshakeDataPtr(Buffer, Size, N);
  L := EncodeTLSClientKeyExchange(P^, N, KeyExchangeAlgorithm, ClientKeyExchange);
  Result := EncodeTLSHandshake(Buffer, Size, tlshtClient_key_exchange, L);
end;



{                                                                              }
{ Finished                                                                     }
{                                                                              }
{ SSL 3:                                                                       }
{   Finished                                                                   }
{       opaque md5_hash[16];                                                   }
{       opaque sha_hash[20];                                                   }
{                                                                              }
{ TLS 1.0 1.1 1.2:                                                             }
{   Finished                                                                   }
{       opaque verify_data[12];                                                }
{                                                                              }
type
  TSSLFinishedMD5Hash = array[0..15] of Byte;
  TSSLFinishedSHAHash = array[0..19] of Byte;
  TSSLFinished = packed record
    md5_hash : TSSLFinishedMD5Hash;
    sha_hash : TSSLFinishedSHAHash;
  end;
  PSSLFinished = ^TSSLFinished;

  TTLSFinished = packed record
    verify_data : array[0..11] of Byte;
  end;
  PTLSFinished = ^TTLSFinished;

const
  SSLFinishedSize = SizeOf(TSSLFinished);
  TLSFinishedSize = Sizeof(TTLSFinished);



{                                                                              }
{ SSL 3.0:                                                                     }
{ md5_hash = MD5(master_secret + pad2 +                                        }
{     MD5(handshake_messages + Sender + master_secret + pad1));                }
{ sha_hash = SHA(master_secret + pad2 +                                        }
{     SHA(handshake_messages + Sender + master_secret + pad1));                }
{ pad_1 = The character 0x36 repeated 48 times for MD5 or 40 times for SHA.    }
{ pad_2 = The character 0x5c repeated 48 times for MD5 or 40 times for SHA.    }
{ Sender = enum ( client(0x434C4E54), server(0x53525652) )                     }
{                                                                              }
{ TLS 1.0: verify_data =                                                       }
{     PRF(master_secret, finished_label, MD5(handshake_messages) +             }
{          SHA-1(handshake_messages)) [0..11];                                 }
{                                                                              }
{ TLS 1.2: verify_data =                                                       }
{     PRF(master_secret, finished_label, Hash(handshake_messages))             }
{            [0..verify_data_length-1];                                        }
{                                                                              }
{ For the PRF defined in Section 5, the Hash MUST be the Hash used as the      }
{ basis for the PRF.  Any cipher suite which defines a different PRF MUST      }
{ also define the Hash to use in the Finished computation.                     }
{ In previous versions of TLS, the verify_data was always 12 octets            }
{ long.  In the current version of TLS, it depends on the cipher suite.        }
{ Any cipher suite which does not explicitly specify verify_data_length has a  }
{ verify_data_length equal to 12.  This includes all existing cipher suites.   }
{ All of the data from all messages in this handshake (not                     }
{ including any HelloRequest messages) up to, but not including,               }
{ this message.  This is only data visible at the handshake layer              }
{ and does not include record layer headers.  This is the                      }
{ concatenation of all the Handshake structures as defined in                  }
{ Section 7.4, exchanged thus far.                                             }
{                                                                              }
procedure CalcSSLVerifyData(
          const MasterSecret: RawByteString;
          const HandshakeData: RawByteString;
          const SenderIsClient: Boolean;
          var md5_hash: TSSLFinishedMD5Hash;
          var sha_hash: TSSLFinishedSHAHash);
var
  M, S, T : RawByteString;
begin
  if SenderIsClient then
    T := #$43#$4C#$4E#$54
  else
    T := #$53#$52#$56#$52;
  M := MD5DigestToStrA(
           CalcMD5(MasterSecret + DupCharB(#$5C, 48) +
               MD5DigestToStrA(
                   CalcMD5(HandshakeData + T + MasterSecret + DupCharB(#$36, 48)))));
  S := SHA1DigestToStrA(
           CalcSHA1(MasterSecret + DupCharB(#$5C, 40) +
               SHA1DigestToStrA(
                   CalcSHA1(HandshakeData + T + MasterSecret + DupCharB(#$36, 40)))));
  Move(M[1], md5_hash, 16);
  Move(S[1], sha_hash, 20);
end;

const
  client_finished_label = 'client finished';
  server_finished_label = 'server finished';

function CalcTLSVerifyData(
         const ProtocolVersion: TTLSProtocolVersion;
         const MasterSecret: RawByteString;
         const HandshakeData: RawByteString): RawByteString;
var VDL : Integer;
    D1 : T128BitDigest;
    D2 : T160BitDigest;
    D3 : T256BitDigest;
    Seed : RawByteString;
    PRF : RawByteString;
begin
  if IsTLS12OrLater(ProtocolVersion) then
    begin
      VDL := 12;
      D3 := CalcSHA256(HandshakeData[1], Length(HandshakeData));
      SetLength(Seed, SizeOf(D3));
      Move(D3, Seed[1], SizeOf(D3));
      PRF := tls12PRF_SHA256(MasterSecret, client_finished_label, Seed, VDL);
    end else
  if IsTLS10OrLater(ProtocolVersion) then
    begin
      VDL := 12;
      D1 := CalcMD5(HandshakeData);
      D2 := CalcSHA1(HandshakeData);
      SetLength(Seed, SizeOf(D1) + SizeOf(D2));
      Move(D1, Seed[1], SizeOf(D1));
      Move(D2, Seed[SizeOf(D1) + 1], SizeOf(D2));
      PRF := tls10PRF(MasterSecret, client_finished_label, Seed, VDL);
    end
  else
    raise ETLSError.Create(TLSError_InvalidParameter);
  Result := PRF;
end;

function EncodeTLSHandshakeFinished(var Buffer; const Size: Integer;
         const MasterSecret: RawByteString;
         const ProtocolVersion: TTLSProtocolVersion;
         const HandshakeData: RawByteString;
         const SenderIsClient: Boolean): Integer;
var N : Integer;
    P : PByte;
    V : RawByteString;
    F : PSSLFinished;
begin
  P := TLSHandshakeDataPtr(Buffer, Size, N);
  if IsTLS10OrLater(ProtocolVersion) then
    begin
      V := CalcTLSVerifyData(ProtocolVersion, MasterSecret, HandshakeData);
      Assert(Length(V) >= 12);
      Move(V[1], PTLSFinished(P)^.verify_data[0], 12);
      Result := EncodeTLSHandshake(Buffer, Size, tlshtFinished, TLSFinishedSize);
    end else
  if IsSSL3(ProtocolVersion) then
    begin
      F := PSSLFinished(P);
      CalcSSLVerifyData(MasterSecret, HandshakeData, SenderIsClient, F^.md5_hash, F^.sha_hash);
      Result := EncodeTLSHandshake(Buffer, Size, tlshtFinished, SSLFinishedSize);
    end
  else
    raise ETLSError.Create(TLSError_InvalidParameter);
end;



{                                                                              }
{ ChangeCipherSpec Protocol                                                    }
{                                                                              }
procedure InitTLSChangeCipherSpec(var ChangeCipherSpec: TTLSChangeCipherSpec);
begin
  ChangeCipherSpec._type := spectype_change_cipher_spec;
end;



{                                                                              }
{ Unit test                                                                    }
{                                                                              }
{$IFDEF TLS_TEST}
{$ASSERTIONS ON}
procedure Test;
begin
  Assert(TLSHandshakeHeaderSize = 4);
end;
{$ENDIF}



end.

