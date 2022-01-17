{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals TLS                                         }
{   File name:        flcTLSHandshakeExtension.pas                             }
{   File version:     5.03                                                     }
{   Description:      TLS handshake extension                                  }
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
{   2020/05/11  5.02  ExtensionType.                                           }
{                     SignatureAlgorithms ClientHello extension.               }
{   2020/05/19  5.03  Create flcTLSHandshakeExtension unit from                }
{                     flcTLSHandshake unit.                                    }
{                                                                              }
{******************************************************************************}

{$INCLUDE flcTLS.inc}

unit flcTLSHandshakeExtension;

interface

uses
  { TLS }

  flcTLSAlgorithmTypes;



{                                                                              }
{ ExtensionType                                                                }
{                                                                              }
type
  TTLSExtensionType = (
    tlsetServer_name                            = 0,     // RFC 6066
    tlsetMax_fragment_length                    = 1,     // RFC 6066
    tlsetStatus_request                         = 5,     // RFC 6066
    tlsetSupported_groups                       = 10,    // RFC 8422, 7919
    tlsetSignature_algorithms                   = 13,    // RFC 8446 - TLS 1.2
    tlsetUse_srtp                               = 14,    // RFC 5764
    tlsetHeartbeat                              = 15,    // RFC 6520
    tlsetApplication_layer_protocol_negotiation = 16,    // RFC 7301
    tlsetSigned_certificate_timestamp           = 18,    // RFC 6962
    tlsetClient_certificate_type                = 19,    // RFC 7250
    tlsetServer_certificate_type                = 20,    // RFC 7250
    tlsetPadding                                = 21,    // RFC 7685
    tlsetPre_shared_key                         = 41,    // RFC 8446
    tlsetEarly_data                             = 42,    // RFC 8446
    tlsetSupported_versions                     = 43,    // RFC 8446
    tlsetCookie                                 = 44,    // RFC 8446
    tlsetPsk_key_exchange_modes                 = 45,    // RFC 8446
    tlsetCertificate_authorities                = 47,    // RFC 8446
    tlsetOid_filters                            = 48,    // RFC 8446
    tlsetPost_handshake_auth                    = 49,    // RFC 8446
    tlsetSignature_algorithms_cert              = 50,    // RFC 8446
    tlsetKey_share                              = 51,    // RFC 8446
    tlsetMax                                    = 65535
    );



{                                                                              }
{ SignatureAlgorithms                                                          }
{                                                                              }
function EncodeTLSExtension_SignatureAlgorithms(
         var Buffer; const Size: Integer;
         const SignAndHashAlgos: TTLSSignatureAndHashAlgorithmArray): Integer;



implementation

uses
  { TLS }

  flcTLSErrors,
  flcTLSOpaqueEncoding;



{                                                                              }
{ SignatureAlgorithms                                                          }
{                                                                              }
function EncodeTLSExtension_SignatureAlgorithms(
         var Buffer; const Size: Integer;
         const SignAndHashAlgos: TTLSSignatureAndHashAlgorithmArray): Integer;
var P     : PByte;
    L, N  : Integer;
    C, I  : Integer;
begin
  N := Size;
  P := @Buffer;

  Dec(N, 2);
  if N < 0 then
    raise ETLSError.Create(TLSError_InvalidBuffer);
  EncodeTLSWord16(P^, N, Ord(TTLSExtensionType.tlsetSignature_algorithms));
  Inc(P, 2);

  C := Length(SignAndHashAlgos);
  Assert(C > 0);
  L := C * TLSSignatureAndHashAlgorithmSize;
  EncodeTLSLen16(P^, N, L);
  Inc(P, 2);
  Dec(N, 2);

  Dec(N, L);
  if N < 0 then
    raise ETLSError.Create(TLSError_InvalidBuffer);

  for I := 0 to C - 1 do
    begin
      P^ := Ord(SignAndHashAlgos[I].Hash);
      Inc(P);
      P^ := Ord(SignAndHashAlgos[I].Signature);
      Inc(P);
    end;

  Result := Size - N;
end;



end.
