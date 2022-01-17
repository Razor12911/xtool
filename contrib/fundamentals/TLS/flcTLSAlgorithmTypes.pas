{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals TLS                                         }
{   File name:        flcTLSAlgorithmTypes.pas                                 }
{   File version:     5.03                                                     }
{   Description:      TLS Algorithm Types                                      }
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
{   2020/05/09  5.02  Create flcTLSAlgorithmTypes unit from flcTLSUtils unit.  }
{   2020/05/11  5.03  NamedCurve and ECPointFormat enumerations.               }
{                                                                              }
{******************************************************************************}

{$INCLUDE flcTLS.inc}

unit flcTLSAlgorithmTypes;

interface



{                                                                              }
{ CompressionMethod                                                            }
{                                                                              }
type
  TTLSCompressionMethod = (
    tlscmNull    = 0,    // Enumerations from handshake
    tlscmDeflate = 1,
    tlscmMax     = 255
    );
  PTLSCompressionMethod = ^TTLSCompressionMethod;

  TTLSCompressionMethods = set of TTLSCompressionMethod;

const
  TLSCompressionMethodSize = Sizeof(TTLSCompressionMethod);



{                                                                              }
{ HashAlgorithm                                                                }
{                                                                              }
type
  TTLSHashAlgorithm = (
    tlshaNone   = 0,    // Enumerations from handshake
    tlshaMD5    = 1,
    tlshaSHA1   = 2,
    tlshaSHA224 = 3,
    tlshaSHA256 = 4,
    tlshaSHA384 = 5,
    tlshaSHA512 = 6,
    tlshaMax    = 255
    );
  TTLSHashAlgorithms = set of TTLSHashAlgorithm;

function HashAlgorithmToStr(const A: TTLSHashAlgorithm): String;



{                                                                              }
{ SignatureAlgorithm                                                           }
{                                                                              }
type
  TTLSSignatureAlgorithm = (
    tlssaAnonymous = 0,    // Enumerations from handshake
    tlssaRSA       = 1,
    tlssaDSA       = 2,
    tlssaECDSA     = 3,
    tlssaMax       = 255
    );
  TTLSSignatureAlgorithms = set of TTLSSignatureAlgorithm;



{                                                                              }
{ SignatureAndHashAlgorithm                                                    }
{                                                                              }
type
  TTLSSignatureAndHashAlgorithm = packed record
    Hash      : TTLSHashAlgorithm;
    Signature : TTLSSignatureAlgorithm;
  end;
  PTLSSignatureAndHashAlgorithm = ^TTLSSignatureAndHashAlgorithm;
  TTLSSignatureAndHashAlgorithmArray = array of TTLSSignatureAndHashAlgorithm;

const
  TLSSignatureAndHashAlgorithmSize = SizeOf(TTLSSignatureAndHashAlgorithm);



{                                                                              }
{ MACAlgorithm                                                                 }
{ Used in TLS record.                                                          }
{                                                                              }
type
  TTLSMACAlgorithm = (
    tlsmaNone,
    tlsmaNULL,
    tlsmaHMAC_MD5,
    tlsmaHMAC_SHA1,
    tlsmaHMAC_SHA256,
    tlsmaHMAC_SHA384,
    tlsmaHMAC_SHA512
    );

  TTLSMacAlgorithmInfo = record
    Name       : RawByteString;
    DigestSize : Integer;
    Supported  : Boolean; // Not used
  end;
  PTLSMacAlgorithmInfo = ^TTLSMacAlgorithmInfo;

const
  TLSMACAlgorithmInfo : array[TTLSMACAlgorithm] of TTLSMacAlgorithmInfo = (
    ( // None
     Name       : '';
     DigestSize : 0;
     Supported  : False;
    ),
    ( // NULL
     Name       : 'NULL';
     DigestSize : 0;
     Supported  : True;
    ),
    ( // HMAC_MD5
     Name       : 'HMAC-MD5';
     DigestSize : 16;
     Supported  : True;
    ),
    ( // HMAC_SHA1
     Name       : 'HMAC-SHA1';
     DigestSize : 20;
     Supported  : True;
    ),
    ( // HMAC_SHA256
     Name       : 'HMAC-SHA256';
     DigestSize : 32;
     Supported  : True;
    ),
    ( // HMAC_SHA384
     Name       : 'HMAC-SHA384';
     DigestSize : 48;
     Supported  : False;
    ),
    ( // HMAC_SHA512
     Name       : 'HMAC-SHA512';
     DigestSize : 64;
     Supported  : True;
    )
    );

const
  TLS_MAC_MAXDIGESTSIZE = 64;



{                                                                              }
{ KeyExchangeAlgorithm                                                         }
{                                                                              }
type
  TTLSKeyExchangeAlgorithm = (
    tlskeaNone,
    tlskeaNULL,
    tlskeaDHE_DSS,
    tlskeaDHE_RSA,
    tlskeaDH_Anon,
    tlskeaRSA,
    tlskeaDH_DSS,
    tlskeaDH_RSA,
    tlskeaECDHE_ECDSA,
    tlskeaECDH_ECDSA,
    tlskeaECDHE_RSA,
    tlskeaECDH_RSA
    );

  TTLSKeyExchangeMethod = (
    tlskemNone,
    tlskemNULL,
    tlskemDHE,
    tlskemDH_Anon,
    tlskemRSA,
    tlskemDH,
    tlskemPSK,
    tlskemECDH,
    tlskemECDHE
    );

  TTLSKeyExchangeAuthenticationType = (
    tlskeatNone,
    tlskeatAnon,
    tlskeatDSS,
    tlskeatRSA,
    tlskeatPSK,
    tlskeatECDSA
    );

  TTLSKeyExchangeAlgorithmInfo = record
    Name      : RawByteString;
    Method    : TTLSKeyExchangeMethod;
    KeyType   : TTLSKeyExchangeAuthenticationType;
    Supported : Boolean; // Not used
  end;
  PTLSKeyExchangeAlgorithmInfo = ^TTLSKeyExchangeAlgorithmInfo;

const
  TLSKeyExchangeAlgorithmInfo: array[TTLSKeyExchangeAlgorithm] of TTLSKeyExchangeAlgorithmInfo = (
    ( // None
     Name      : '';
     Method    : tlskemNone;
     KeyType   : tlskeatNone;
     Supported : False;
    ),
    ( // NULL
     Name      : 'NULL';
     Method    : tlskemNULL;
     KeyType   : tlskeatNone;
     Supported : True;
    ),
    ( // DHE_DSS
     Name      : 'DHE_DSS';
     Method    : tlskemDHE;
     KeyType   : tlskeatDSS;
     Supported : False;
    ),
    ( // DHE_RSA
     Name      : 'DHE_RSA';
     Method    : tlskemDHE;
     KeyType   : tlskeatRSA;
     Supported : False;
    ),
    ( // DH_Anon
     Name      : 'DH_Anon';
     Method    : tlskemDH_Anon;
     KeyType   : tlskeatNone;
     Supported : False;
    ),
    ( // RSA
     Name      : 'RSA';
     Method    : tlskemRSA;
     KeyType   : tlskeatRSA;
     Supported : True;
    ),
    ( // DH_DSS
     Name      : 'DH_DSS';
     Method    : tlskemDH;
     KeyType   : tlskeatDSS;
     Supported : False;
    ),
    ( // DH_RSA
     Name      : 'DH_RSA';
     Method    : tlskemDH;
     KeyType   : tlskeatRSA;
     Supported : False;
    ),
    ( // ECDHE_ECDSA
     Name      : 'ECDHE_ECDSA';
     Method    : tlskemECDHE;
     KeyType   : tlskeatECDSA;
     Supported : False;
    ),
    ( // ECDH_ECDSA
     Name      : 'ECDH_ECDSA';
     Method    : tlskemECDH;
     KeyType   : tlskeatECDSA;
     Supported : False;
    ),
    ( // ECDHE_RSA
     Name      : 'ECDHE_RSA';
     Method    : tlskemECDHE;
     KeyType   : tlskeatRSA;
     Supported : False;
    ),
    ( // ECDH_RSA
     Name      : 'ECDH_RSA';
     Method    : tlskemECDH;
     KeyType   : tlskeatRSA;
     Supported : False;
    )
    );



{                                                                              }
{ ClientCertificateType                                                        }
{                                                                              }
type
  TTLSClientCertificateType = (
      tlscctRsa_sign                  = 1,
      tlscctDss_sign                  = 2,
      tlscctRsa_fixed_dh              = 3,
      tlscctDss_fixed_dh              = 4,
      tlscctRsa_ephemeral_dh_RESERVED = 5,
      tlscctDss_ephemeral_dh_RESERVED = 6,
      tlscctFortezza_dms_RESERVED     = 20,
      tlscctEcdsa_sign                = 64,     // RFC 8422
      tlscctRsa_fixed_ecdh            = 65,     // RFC 4492, deprecated in RFC 8422
      tlscctEcdsa_fixed_ecdh          = 66,     // RFC 4492, deprecated in RFC 8422
      tlscctMax                       = 255
  );



{                                                                              }
{ ECCurveType                                                                  }
{                                                                              }
type
  TTLSECCurveType = (
    tlsectExplicit_prime = 1,     // RFC 4492, deprecated in RFC 8422
    tlsectExplicit_char2 = 2,     // RFC 4492, deprecated in RFC 8422
    tlsectNamed_curve    = 3,     // RFC 4492
    tlsectMax            = 255
    );



{                                                                              }
{ NamedCurve                                                                   }
{                                                                              }
type
  TTLSNamedCurve = (
    tlsncSect163k1 = 1,     // deprecated in RFC 8422
    tlsncSect163r1 = 2,     // deprecated in RFC 8422
    tlsncSect163r2 = 3,     // deprecated in RFC 8422
    tlsncSect193r1 = 4,     // deprecated in RFC 8422
    tlsncSect193r2 = 5,     // deprecated in RFC 8422
    tlsncSect233k1 = 6,     // deprecated in RFC 8422
    tlsncSect233r1 = 7,     // deprecated in RFC 8422
    tlsncSect239k1 = 8,     // deprecated in RFC 8422
    tlsncSect283k1 = 9,     // deprecated in RFC 8422
    tlsncSect283r1 = 10,    // deprecated in RFC 8422
    tlsncSect409k1 = 11,    // deprecated in RFC 8422
    tlsncSect409r1 = 12,    // deprecated in RFC 8422
    tlsncSect571k1 = 13,    // deprecated in RFC 8422
    tlsncSect571r1 = 14,    // deprecated in RFC 8422
    tlsncSecp160k1 = 15,    // deprecated in RFC 8422
    tlsncSecp160r1 = 16,    // deprecated in RFC 8422
    tlsncSecp160r2 = 17,    // deprecated in RFC 8422
    tlsncSecp192k1 = 18,    // deprecated in RFC 8422
    tlsncSecp192r1 = 19,    // deprecated in RFC 8422
    tlsncSecp224k1 = 20,    // deprecated in RFC 8422
    tlsncSecp224r1 = 21,    // deprecated in RFC 8422
    tlsncSecp256k1 = 22,    // deprecated in RFC 8422
    tlsncSecp256r1 = 23,
    tlsncSecp384r1 = 24,
    tlsncSecp521r1 = 25,
    tlsncX25519    = 29,    // RFC 8422
    tlsncX448      = 30,    // RFC 8422
    tlsncArbitrary_explicit_prime_curves = $FF01,    // deprecated in RFC 8422
    tlsncArbitrary_explicit_char2_curves = $FF02,    // deprecated in RFC 8422
    tlsncMax = $FFFF
    );



{                                                                              }
{ ECPointFormat                                                                }
{                                                                              }
type
  TTLSECPointFormat = (
    tlsepfUncompressed              = 0,
    tlsepfAnsiX962_compressed_prime = 1,
    tlsepfAnsiX962_compressed_char2 = 2,
    tlsepfMax                       = 255
    );



{                                                                              }
{ Test                                                                         }
{                                                                              }
{$IFDEF TLS_TEST}
procedure Test;
{$ENDIF}



implementation

uses
  { System }

  SysUtils;



{                                                                              }
{ HashAlgorithm                                                                }
{                                                                              }
function HashAlgorithmToStr(const A: TTLSHashAlgorithm): String;
begin
  case A of
    tlshaNone   : Result := 'None';
    tlshaMD5    : Result := 'MD5';
    tlshaSHA1   : Result := 'SHA1';
    tlshaSHA224 : Result := 'SHA224';
    tlshaSHA256 : Result := 'SHA256';
    tlshaSHA384 : Result := 'SHA384';
    tlshaSHA512 : Result := 'SHA512';
  else
    Result := 'Hash#' + IntToStr(Ord(A));
  end;
end;



{                                                                              }
{ Test                                                                         }
{                                                                              }
{$IFDEF TLS_TEST}
{$ASSERTIONS ON}
procedure Test;
begin
  Assert(TLSCompressionMethodSize = 1);
end;
{$ENDIF}



end.
