{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals TLS                                         }
{   File name:        flcTLSKeyExchangeParams.pas                              }
{   File version:     5.04                                                     }
{   Description:      TLS Key Exchange Parameters                              }
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
{   References:                                                                }
{                                                                              }
{     RFC 4492 - Elliptic Curve Cryptography (ECC) Cipher Suites for           }
{     Transport Layer Security (TLS)                                           }
{     https://tools.ietf.org/html/rfc4492                                      }
{                                                                              }
{     RFC 8422 - Elliptic Curve Cryptography (ECC) Cipher Suites for           }
{     Transport Layer Security (TLS) Versions 1.2 and Earlier                  }
{     https://tools.ietf.org/html/rfc8422                                      }
{                                                                              }
{     https://ldapwiki.com/wiki/Key-Exchange                                   }
{     https://crypto.stackexchange.com/questions/26354/whats-the-structure-of-server-key-exchange-message-during-tls-handshake }
{     https://security.stackexchange.com/questions/8343/what-key-exchange-mechanism-should-be-used-in-tls }
{                                                                              }
{ Revision history:                                                            }
{                                                                              }
{   2008/01/18  0.01  Initial development.                                     }
{   2020/05/09  5.02  Create flcTLSKeyExchangeParams unit from                 }
{                     flcTLSUtils unit.                                        }
{   2020/05/11  5.03  TLSDigitallySigned, SignTLSServerKeyExchangeDH_RSA.      }
{   2020/05/19  5.04  Sign/Verify RSA authentication signature for DHE_RSA.    }
{                                                                              }
{******************************************************************************}

{$INCLUDE flcTLS.inc}

unit flcTLSKeyExchangeParams;

interface

uses
  { System }

  SysUtils,

  { Utils }

  flcStdTypes,

  { Cipher }

  flcCipherRSA,

  { TLS }

  flcTLSAlgorithmTypes;



{                                                                              }
{ ServerDHParams                                                               }
{ Ephemeral DH parameters                                                      }
{                                                                              }
type
  TTLSServerDHParams = record
    dh_p  : RawByteString;
    dh_g  : RawByteString;
    dh_Ys : RawByteString;
  end;

procedure AssignTLSServerDHParams(var ServerDHParams: TTLSServerDHParams;
          const p, g, Ys: RawByteString);

function  EncodeTLSServerDHParams(
          var Buffer; const Size: Integer;
          const ServerDHParams: TTLSServerDHParams): Integer;

function  DecodeTLSServerDHParams(
          const Buffer; const Size: Integer;
          var ServerDHParams: TTLSServerDHParams): Integer;



{                                                                              }
{ ServerRSAParams                                                              }
{                                                                              }
type
  TTLSServerRSAParams = record
    rsa_modulus  : RawByteString;
    rsa_exponent : RawByteString;
  end;

procedure AssignTLSServerRSAParams(var ServerRSAParams: TTLSServerRSAParams;
          const modulus, exponent: RawByteString);

function  EncodeTLSServerRSAParams(
          var Buffer; const Size: Integer;
          const ServerRSAParams: TTLSServerRSAParams): Integer;

function  DecodeTLSServerRSAParams(
          const Buffer; const Size: Integer;
          var ServerRSAParams: TTLSServerRSAParams): Integer;


{                                                                              }
{ ServerECDHParams                                                             }
{                                                                              }




{                                                                              }
{ ServerParamsHashBuf                                                          }
{                                                                              }
type
  TTLSClientServerRandom = array[0..31] of Byte;
  PTLSClientServerRandom = ^TTLSClientServerRandom;

function EncodeTLSServerDHParamsHashBuf(
         var Buffer; const Size: Integer;
         const client_random: TTLSClientServerRandom;
         const server_random: TTLSClientServerRandom;
         const Params: TTLSServerDHParams): Integer;



{                                                                              }
{ SignedStruct                                                                 }
{                                                                              }
type
  TTLSRSASignedStruct = packed record
    md5_hash : array[0..15] of Byte;
    sha_hash : array[0..19] of Byte;
  end;

  TTLSDSASignedStruct = packed record
    sha_hash : array[0..19] of Byte;
  end;



{                                                                              }
{ DigitallySigned                                                              }
{                                                                              }
{     SignatureAndHashAlgorithm algorithm;                                     }
{     opaque signature<0..2^16-1>;                                             }
{                                                                              }
type
  TTLSDigitallySigned = record
    Algorithm : TTLSSignatureAndHashAlgorithm;
    Signature : RawByteString;
  end;

function  EncodeTLSDigitallySigned(
          var Buffer; const Size: Integer;
          const Signed: TTLSDigitallySigned): Integer;

function  DecodeTLSDigitallySigned(
          const Buffer; const Size: Integer;
          var Signed: TTLSDigitallySigned): Integer;



{                                                                              }
{ ServerKeyExchange                                                            }
{                                                                              }
type
  TTLSServerKeyExchange = record
    DHParams     : TTLSServerDHParams;
    RSAParams    : TTLSServerRSAParams;
    SignedParams : TTLSDigitallySigned;
  end;

procedure InitTLSServerKeyExchange(var ServerKeyExchange: TTLSServerKeyExchange);

procedure AssignTLSServerKeyExchangeDHParams(
          var ServerKeyExchange: TTLSServerKeyExchange;
          const p, g, Ys: RawByteString);

function  EncodeTLSServerKeyExchange(
          var Buffer; const Size: Integer;
          const KeyExchangeAlgorithm: TTLSKeyExchangeAlgorithm;
          const ServerKeyExchange: TTLSServerKeyExchange): Integer;

function  DecodeTLSServerKeyExchange(
          const Buffer; const Size: Integer;
          const KeyExchangeAlgorithm: TTLSKeyExchangeAlgorithm;
          var ServerKeyExchange: TTLSServerKeyExchange): Integer;

procedure SignTLSServerKeyExchangeDH_RSA(
          var ServerKeyExchange: TTLSServerKeyExchange;
          const client_random : TTLSClientServerRandom;
          const server_random : TTLSClientServerRandom;
          const RSAPrivateKey: TRSAPrivateKey);

function  VerifyTLSServerKeyExchangeDH_RSA(
          const ServerKeyExchange: TTLSServerKeyExchange;
          const client_random : TTLSClientServerRandom;
          const server_random : TTLSClientServerRandom;
          const RSAPublicKey: TRSAPublicKey): Boolean;



{                                                                              }
{ ClientDiffieHellmanPublic                                                    }
{                                                                              }
type
  TTLSClientDiffieHellmanPublic = record
    PublicValueEncodingExplicit : Boolean;
    dh_Yc : RawByteString;
  end;

function EncodeTLSClientDiffieHellmanPublic(
         var Buffer; const Size: Integer;
         const ClientDiffieHellmanPublic: TTLSClientDiffieHellmanPublic): Integer;

function DecodeTLSClientDiffieHellmanPublic(
         const Buffer; const Size: Integer;
         const PublicValueEncodingExplicit: Boolean;
         var ClientDiffieHellmanPublic: TTLSClientDiffieHellmanPublic): Integer;



{                                                                              }
{ ClientKeyExchange                                                            }
{                                                                              }
type
  TTLSClientKeyExchange = record
    EncryptedPreMasterSecret  : RawByteString;
    ClientDiffieHellmanPublic : TTLSClientDiffieHellmanPublic;
  end;

function  EncodeTLSClientKeyExchange(
          var Buffer; const Size: Integer;
          const KeyExchangeAlgorithm: TTLSKeyExchangeAlgorithm;
          const ClientKeyExchange: TTLSClientKeyExchange): Integer;

function  DecodeTLSClientKeyExchange(
          const Buffer; const Size: Integer;
          const KeyExchangeAlgorithm: TTLSKeyExchangeAlgorithm;
          const PublicValueEncodingExplicit: Boolean;
          var ClientKeyExchange: TTLSClientKeyExchange): Integer;



implementation

uses
  { Utils }

  flcHash,
  flcHugeInt,

  { TLS }

  flcTLSAlert,
  flcTLSErrors,
  flcTLSOpaqueEncoding;



{                                                                              }
{ ServerDHParams                                                               }
{ Ephemeral DH parameters                                                      }
{                                                                              }
{   struct                                                                     }
{     dh_p  : opaque <1..2^16-1>;                                              }
{     dh_g  : opaque <1..2^16-1>;                                              }
{     dh_Ys : opaque <1..2^16-1>;                                              }
{                                                                              }
procedure AssignTLSServerDHParams(var ServerDHParams: TTLSServerDHParams;
          const p, g, Ys: RawByteString);
begin
  ServerDHParams.dh_p := p;
  ServerDHParams.dh_g := g;
  ServerDHParams.dh_Ys := Ys;
end;

function EncodeTLSServerDHParams(
         var Buffer; const Size: Integer;
         const ServerDHParams: TTLSServerDHParams): Integer;
var P : PByte;
    N, L : Integer;
begin
  Assert(Size >= 0);

  if (ServerDHParams.dh_p = '') or
     (ServerDHParams.dh_g = '') or
     (ServerDHParams.dh_Ys = '') then
    raise ETLSError.Create(TLSError_InvalidParameter);

  N := Size;
  P := @Buffer;
  // dh_p
  L := EncodeTLSOpaque16(P^, N, ServerDHParams.dh_p);
  Dec(N, L);
  Inc(P, L);
  // dh_g
  L := EncodeTLSOpaque16(P^, N, ServerDHParams.dh_g);
  Dec(N, L);
  Inc(P, L);
  // dh_Ys
  L := EncodeTLSOpaque16(P^, N, ServerDHParams.dh_Ys);
  Dec(N, L);
  Result := Size - N;
end;

function DecodeTLSServerDHParams(
         const Buffer; const Size: Integer;
         var ServerDHParams: TTLSServerDHParams): Integer;
var P : PByte;
    N, L : Integer;
begin
  Assert(Size >= 0);
  N := Size;
  P := @Buffer;
  // dh_p
  L := DecodeTLSOpaque16(P^, N, ServerDHParams.dh_p);
  Dec(N, L);
  Inc(P, L);
  // dh_g
  L := DecodeTLSOpaque16(P^, N, ServerDHParams.dh_g);
  Dec(N, L);
  Inc(P, L);
  // dh_Ys
  L := DecodeTLSOpaque16(P^, N, ServerDHParams.dh_Ys);
  Dec(N, L);
  Result := Size - N;
end;



{                                                                              }
{ ServerRSAParams                                                              }
{                                                                              }
{   struct                                                                     }
{     rsa_modulus  : opaque <1..2^16-1>;                                       }
{     rsa_exponent : opaque <1..2^16-1>;                                       }
{                                                                              }
procedure AssignTLSServerRSAParams(var ServerRSAParams: TTLSServerRSAParams;
          const modulus, exponent: RawByteString);
begin
  ServerRSAParams.rsa_modulus := modulus;
  ServerRSAParams.rsa_exponent := exponent;
end;

function EncodeTLSServerRSAParams(
         var Buffer; const Size: Integer;
         const ServerRSAParams: TTLSServerRSAParams): Integer;
var P : PByte;
    N, L : Integer;
begin
  Assert(Size >= 0);
  if (ServerRSAParams.rsa_modulus = '') or
     (ServerRSAParams.rsa_exponent = '') then
    raise ETLSError.Create(TLSError_InvalidParameter);
  N := Size;
  P := @Buffer;
  // rsa_modulus
  L := EncodeTLSOpaque16(P^, N, ServerRSAParams.rsa_modulus);
  Dec(N, L);
  Inc(P, L);
  // rsa_exponent
  L := EncodeTLSOpaque16(P^, N, ServerRSAParams.rsa_exponent);
  Dec(N, L);
  Result := Size - N;
end;

function DecodeTLSServerRSAParams(
         const Buffer; const Size: Integer;
         var ServerRSAParams: TTLSServerRSAParams): Integer;
var P : PByte;
    N, L : Integer;
begin
  Assert(Size >= 0);
  N := Size;
  P := @Buffer;
  // rsa_modulus
  L := DecodeTLSOpaque16(P^, N, ServerRSAParams.rsa_modulus);
  Dec(N, L);
  Inc(P, L);
  // rsa_exponent
  L := DecodeTLSOpaque16(P^, N, ServerRSAParams.rsa_exponent);
  Dec(N, L);
  Result := Size - N;
end;




{                                                                              }
{ ECParameters                                                                 }
{                                                                              }
type
  TTLSECParameters = record
    CurveType  : TTLSECCurveType;
    NamedCurve : TTLSNamedCurve;
  end;


(*
            ECCurveType    curve_type;
            select (curve_type) {
                case explicit_prime:
                    opaque      prime_p <1..2^8-1>;
                    ECCurve     curve;
                    ECPoint     base;
                    opaque      order <1..2^8-1>;
                    opaque      cofactor <1..2^8-1>;
                case explicit_char2:
                    uint16      m;
                    ECBasisType basis;
                    select (basis) {
                        case ec_trinomial:
                            opaque  k <1..2^8-1>;
                        case ec_pentanomial:
                            opaque  k1 <1..2^8-1>;
                            opaque  k2 <1..2^8-1>;
                            opaque  k3 <1..2^8-1>;
                    };
                    ECCurve     curve;
                    ECPoint     base;
                    opaque      order <1..2^8-1>;
                    opaque      cofactor <1..2^8-1>;
                case named_curve:
                    NamedCurve namedcurve;
            };
        }
*)


{                                                                              }
{ ECPoint                                                                      }
{                                                                              }
{               opaque point <1..2^8-1>;                                       }
{                                                                              }
type
  TTLSECPoint = array of Byte;



{                                                                              }
{ ServerECDHParams                                                             }
{                                                                              }
{            ECParameters    curve_params;                                     }
{            ECPoint         public;                                           }
{                                                                              }
type
  TTLSServerECDHParams = record
    CurveParams : TTLSECParameters;
    PublicKey   : TTLSECPoint;
  end;




{                                                                              }
{ ServerParamsHashBuf                                                          }
{                                                                              }
function EncodeTLSServerDHParamsHashBuf(
         var Buffer; const Size: Integer;
         const client_random: TTLSClientServerRandom;
         const server_random: TTLSClientServerRandom;
         const Params: TTLSServerDHParams): Integer;
var P : PByte;
    N, L : Integer;
begin
  N := Size;
  P := @Buffer;
  Dec(N, 64);
  if N < 0 then
    raise ETLSError.Create(TLSError_InvalidBuffer);
  Move(client_random, P^, 32);
  Inc(P, 32);
  Move(server_random, P^, 32);
  Inc(P, 32);
  L := EncodeTLSServerDHParams(P^, N, Params);
  Dec(N, L);
  Result := Size - N;
end;



{                                                                              }
{ DigitallySigned                                                              }
{                                                                              }
(*   struct {                                                                 *)
(*       SignatureAndHashAlgorithm algorithm;                                 *)
(*       opaque signature<0..2^16-1>;                                         *)
(*    } DigitallySigned;                                                      *)
{                                                                              }
function EncodeTLSDigitallySigned(
         var Buffer; const Size: Integer;
         const Signed: TTLSDigitallySigned): Integer;
var P : PByte;
    N, L : Integer;
begin
  Assert(Signed.Algorithm.Hash <> tlshaNone);
  Assert(Signed.Algorithm.Signature <> tlssaAnonymous);
  Assert(Length(Signed.Signature) > 0);

  N := Size;
  P := @Buffer;
  Dec(N, 2);
  if N < 0 then
    raise ETLSError.CreateAlertBufferEncode;
  P^ := Ord(Signed.Algorithm.Hash);
  Inc(P);
  P^ := Ord(Signed.Algorithm.Signature);
  Inc(P);
  L := EncodeTLSOpaque16(P^, N, Signed.Signature);
  Dec(N, L);
  Result := Size - N;
end;

function DecodeTLSDigitallySigned(
         const Buffer; const Size: Integer;
         var Signed: TTLSDigitallySigned): Integer;
var P : PByte;
    N, L : Integer;
begin
  N := Size;
  P := @Buffer;
  Dec(N, 2);
  if N < 0 then
    raise ETLSError.CreateAlertBufferDecode;
  Signed.Algorithm.Hash := TTLSHashAlgorithm(P^);
  Inc(P);
  Signed.Algorithm.Signature := TTLSSignatureAlgorithm(P^);
  Inc(P);
  L := DecodeTLSOpaque16(P^, N, Signed.Signature);
  Dec(N, L);
  Result := Size - N;
end;



{                                                                              }
{ Server Key Exchange                                                          }
{                                                                              }
{   select (KeyExchangeAlgorithm)                                              }
{     case dh_anon: params : ServerDHParams;                                   }
{     case dhe_dss:                                                            }
{     case dhe_rsa: params : ServerDHParams;                                   }
{                   signed_params : digitally-signed struct (                  }
{                     client_random : opaque [32];                             }
{                     server_random : opaque [32];                             }
{                     params : ServerDHParams ;                                }
{                     );                                                       }
{     case rsa:                                                                }
{     case dh_dss:                                                             }
{     case dh_rsa: struct ();                                                  }
{     case ec_diffie_hellman:                                                  }
{              ServerECDHParams    params;                                     }
{              Signature           signed_params;                              }
{                                                                              }
procedure InitTLSServerKeyExchange(var ServerKeyExchange: TTLSServerKeyExchange);
begin
  FillChar(ServerKeyExchange, SizeOf(ServerKeyExchange), 0);
end;

procedure AssignTLSServerKeyExchangeDHParams(
          var ServerKeyExchange: TTLSServerKeyExchange;
          const p, g, Ys: RawByteString);
begin
  AssignTLSServerDHParams(ServerKeyExchange.DHParams, p, g, Ys);
end;

function EncodeTLSServerKeyExchange(
         var Buffer; const Size: Integer;
         const KeyExchangeAlgorithm: TTLSKeyExchangeAlgorithm;
         const ServerKeyExchange: TTLSServerKeyExchange): Integer;
var P : PByte;
    N, L : Integer;
begin
  Assert(KeyExchangeAlgorithm <> tlskeaNone);

  N := Size;
  P := @Buffer;
  case KeyExchangeAlgorithm of
    tlskeaDH_Anon :
      begin
        L := EncodeTLSServerDHParams(P^, N, ServerKeyExchange.DHParams);
        Dec(N, L);
      end;
    tlskeaDHE_DSS,
    tlskeaDHE_RSA :
      begin
        L := EncodeTLSServerDHParams(P^, N, ServerKeyExchange.DHParams);
        Dec(N, L);
        Inc(P, L);
        L := EncodeTLSDigitallySigned(P^, N, ServerKeyExchange.SignedParams);
        Dec(N, L);
      end;
    tlskeaECDHE_ECDSA,
    tlskeaECDHE_RSA : ;
    tlskeaRSA,
    tlskeaDH_DSS,
    tlskeaDH_RSA : ;
  end;
  Result := Size - N;
end;

function DecodeTLSServerKeyExchange(
         const Buffer; const Size: Integer;
         const KeyExchangeAlgorithm: TTLSKeyExchangeAlgorithm;
         var ServerKeyExchange: TTLSServerKeyExchange): Integer;
var P : PByte;
    N, L : Integer;
begin
  N := Size;
  P := @Buffer;
  case KeyExchangeAlgorithm of
    tlskeaDH_Anon :
      begin
        L := DecodeTLSServerDHParams(P^, N, ServerKeyExchange.DHParams);
        Dec(N, L);
      end;
    tlskeaDHE_DSS,
    tlskeaDHE_RSA :
      begin
        L := DecodeTLSServerDHParams(P^, N, ServerKeyExchange.DHParams);
        Dec(N, L);
        Inc(P, L);
        L := DecodeTLSDigitallySigned(P^, N, ServerKeyExchange.SignedParams);
        Dec(N, L);
      end;
    tlskeaRSA,
    tlskeaDH_DSS,
    tlskeaDH_RSA : ;
  end;
  Result := Size - N;
end;

procedure SignTLSServerKeyExchangeDH_RSA(
          var ServerKeyExchange: TTLSServerKeyExchange;
          const client_random : TTLSClientServerRandom;
          const server_random : TTLSClientServerRandom;
          const RSAPrivateKey: TRSAPrivateKey);
const
  MaxHashBufSize = 32768;
var
  HashBuf : array[0..MaxHashBufSize - 1] of Byte;
  L : Integer;
  //HashMd5 : T128BitDigest;
  //HashSha : T160BitDigest;
  //SignedStruct : TTLSRSASignedStruct;
  //SignHash : T256BitDigest;
  SignatureBuf : RawByteString;
begin
  Writeln('Sign:');

  Assert(Length(ServerKeyExchange.DHParams.dh_p) > 0);
  Assert(Length(ServerKeyExchange.DHParams.dh_g) > 0);
  Assert(Length(ServerKeyExchange.DHParams.dh_Ys) > 0);
  Assert(RSAPrivateKey.KeyBits > 0);

  L := EncodeTLSServerDHParamsHashBuf(
      HashBuf, SizeOf(HashBuf),
      client_random,
      server_random,
      ServerKeyExchange.DHParams);

      {
  HashMd5 := CalcMD5(HashBuf, L);
  HashSha := CalcSHA1(HashBuf, L);
  Move(HashMd5, SignedStruct.md5_hash, SizeOf(SignedStruct.md5_hash));
  Move(HashSha, SignedStruct.sha_hash, SizeOf(SignedStruct.sha_hash));
  SignHash := CalcSHA256(SignedStruct, SizeOf(SignedStruct));
  Writeln('SignHash:', DigestToHexU(SignHash, Sizeof(SignHash)));
  Writeln('PrivateKey:', HugeWordToHex(RSAPrivateKey.Exponent), '  ', HugeWordToHex(RSAPrivateKey.Modulus));
}

  SetLength(SignatureBuf, RSAPrivateKey.KeyBits div 8);

  RSASignMessage(rsastEMSA_PKCS1, rsahfSHA256, RSAPrivateKey,
      HashBuf, L,
      Pointer(SignatureBuf)^, Length(SignatureBuf));

  ServerKeyExchange.SignedParams.Algorithm.Hash := tlshaSHA256;
  ServerKeyExchange.SignedParams.Algorithm.Signature := tlssaRSA;
  ServerKeyExchange.SignedParams.Signature := SignatureBuf;
end;

function VerifyTLSServerKeyExchangeDH_RSA(
         const ServerKeyExchange: TTLSServerKeyExchange;
         const client_random : TTLSClientServerRandom;
         const server_random : TTLSClientServerRandom;
         const RSAPublicKey: TRSAPublicKey): Boolean;
const
  MaxHashBufSize = 32768;
var
  HashBuf : array[0..MaxHashBufSize - 1] of Byte;
  L : Integer;
//  HashMd5 : T128BitDigest;
//  HashSha : T160BitDigest;
//  SignedStruct : TTLSRSASignedStruct;
//  SignHash : T256BitDigest;
  SignatureBuf : RawByteString;
begin
  Assert(Length(ServerKeyExchange.DHParams.dh_p) > 0);
  Assert(Length(ServerKeyExchange.DHParams.dh_g) > 0);
  Assert(Length(ServerKeyExchange.DHParams.dh_Ys) > 0);
  Assert(RSAPublicKey.KeyBits > 0);

  //// After validated, encode hash buf using received Params buf, it might
  //// have params in a different order.
  L := EncodeTLSServerDHParamsHashBuf(HashBuf, SizeOf(HashBuf),
      client_random,
      server_random,
      ServerKeyExchange.DHParams);

  SignatureBuf := ServerKeyExchange.SignedParams.Signature;

  (*
  HashMd5 := CalcMD5(HashBuf, L);
  HashSha := CalcSHA1(HashBuf, L);
  Move(HashMd5, SignedStruct.md5_hash, SizeOf(SignedStruct.md5_hash));
  Move(HashSha, SignedStruct.sha_hash, SizeOf(SignedStruct.sha_hash));
  SignHash := CalcSHA256(SignedStruct, SizeOf(SignedStruct));
  Writeln('SignHash:', DigestToHexU(SignHash, Sizeof(SignHash)));
  Writeln('Signature:', DigestToHexU(Pointer(SignatureBuf)^, Length(SignatureBuf)));
  Writeln('PublicKey:', HugeWordToHex(RSAPublicKey.Exponent), '  ', HugeWordToHex(RSAPublicKey.Modulus));
  *)

  Result :=
      RSACheckSignature(rsastEMSA_PKCS1, RSAPublicKey,
          HashBuf, L,
          Pointer(SignatureBuf)^, Length(SignatureBuf));
end;



{                                                                              }
{ ClientDiffieHellmanPublic                                                    }
{   select (PublicValueEncoding)                                               }
{       case implicit: struct ();                                              }
{       case explicit: opaque dh_Yc<1..2^16-1>;                                }
{                                                                              }
function EncodeTLSClientDiffieHellmanPublic(
         var Buffer; const Size: Integer;
         const ClientDiffieHellmanPublic: TTLSClientDiffieHellmanPublic): Integer;
var P : PByte;
    N, L : Integer;
begin
  N := Size;
  P := @Buffer;
  if ClientDiffieHellmanPublic.PublicValueEncodingExplicit then
    begin
      if ClientDiffieHellmanPublic.dh_Yc = '' then
        raise ETLSError.Create(TLSError_InvalidParameter);
      L := EncodeTLSOpaque16(P^, N, ClientDiffieHellmanPublic.dh_Yc);
      Dec(N, L);
    end;
  Result := Size - N;
end;

function DecodeTLSClientDiffieHellmanPublic(
         const Buffer; const Size: Integer;
         const PublicValueEncodingExplicit: Boolean;
         var ClientDiffieHellmanPublic: TTLSClientDiffieHellmanPublic): Integer;
var P : PByte;
    N, L : Integer;
begin
  N := Size;
  P := @Buffer;
  if PublicValueEncodingExplicit then
    begin
      L := DecodeTLSOpaque16(P^, N, ClientDiffieHellmanPublic.dh_Yc);
      Dec(N, L);
    end;
  Result := Size - N;
end;

(*
    struct {
               ECPoint ecdh_Yc;
           } ClientECDiffieHellmanPublic;
*)


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
function EncodeTLSClientKeyExchange(
         var Buffer; const Size: Integer;
         const KeyExchangeAlgorithm: TTLSKeyExchangeAlgorithm;
         const ClientKeyExchange: TTLSClientKeyExchange): Integer;
var P : PByte;
    N, L : Integer;
begin
  N := Size;
  P := @Buffer;
  case KeyExchangeAlgorithm of
    tlskeaRSA :
      begin
        L := Length(ClientKeyExchange.EncryptedPreMasterSecret);
        if L = 0 then
          raise ETLSError.Create(TLSError_InvalidParameter);
        Move(ClientKeyExchange.EncryptedPreMasterSecret[1], P^, L);
        Dec(N, L);
      end;
    tlskeaDHE_DSS,
    tlskeaDHE_RSA,
    tlskeaDH_DSS,
    tlskeaDH_RSA,
    tlskeaDH_Anon :
      begin
        L := EncodeTLSClientDiffieHellmanPublic(P^, N, ClientKeyExchange.ClientDiffieHellmanPublic);
        Dec(N, L);
      end;
  end;
  Result := Size - N;
end;

function DecodeTLSClientKeyExchange(
         const Buffer; const Size: Integer;
         const KeyExchangeAlgorithm: TTLSKeyExchangeAlgorithm;
         const PublicValueEncodingExplicit: Boolean;
         var ClientKeyExchange: TTLSClientKeyExchange): Integer;
var P : PByte;
    N, L, C : Integer;
begin
  N := Size;
  P := @Buffer;
  case KeyExchangeAlgorithm of
    tlskeaRSA :
      begin
        L := DecodeTLSLen16(P^, N, C);
        Dec(N, L);
        Inc(P, L);
        Assert(N = C);
        SetLength(ClientKeyExchange.EncryptedPreMasterSecret, C);
        Move(P^, ClientKeyExchange.EncryptedPreMasterSecret[1], C);
        Dec(N, C);
      end;
    tlskeaDHE_DSS,
    tlskeaDHE_RSA,
    tlskeaDH_DSS,
    tlskeaDH_RSA,
    tlskeaDH_Anon :
      begin
        L := DecodeTLSClientDiffieHellmanPublic(P^, N, PublicValueEncodingExplicit, ClientKeyExchange.ClientDiffieHellmanPublic);
        Dec(N, L);
      end;
  end;
  Result := Size - N;
end;



end.

