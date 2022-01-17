{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals TLS                                         }
{   File name:        flcTLSCipherSuite.pas                                    }
{   File version:     5.04                                                     }
{   Description:      TLS cipher suite                                         }
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
{ References:                                                                  }
{                                                                              }
{   http://www.iana.org/assignments/tls-parameters/tls-parameters.xhtml        }
{   https://ldapwiki.com/wiki/Known%20Cipher%20Suites                          }
{                                                                              }
{ Revision history:                                                            }
{                                                                              }
{   2008/01/18  0.01  Initial development.                                     }
{   2018/07/17  5.02  Revised for Fundamentals 5.                              }
{   2020/05/09  5.03  Populate CipherSuiteInfo.Auhtentication.                 }
{   2020/05/19  5.04  Enable support for DHE_RSA_WITH_AES suites.              }
{                                                                              }
{******************************************************************************}

{$INCLUDE flcTLS.inc}

unit flcTLSCipherSuite;

interface

uses
  { Utils }

  flcStdTypes,

  { TLS }

  flcTLSProtocolVersion,
  flcTLSAlgorithmTypes;



{                                                                              }
{ Cipher Suite                                                                 }
{                                                                              }
type
  TTLSCipherSuite = (
    tlscsNone,
    tlscsNULL_WITH_NULL_NULL,
    tlscsRSA_WITH_NULL_MD5,
    tlscsRSA_WITH_NULL_SHA,
    tlscsDH_Anon_EXPORT_WITH_RC4_40_MD5,
    tlscsDH_Anon_WITH_RC4_128_MD5,
    tlscsDH_Anon_EXPORT_WITH_DES40_CBC_SHA,
    tlscsDH_Anon_WITH_DES_CBC_SHA,
    tlscsDH_Anon_WITH_3DES_EDE_CBC_SHA,
    tlscsDH_Anon_WITH_AES_128_CBC_SHA256,
    tlscsDH_Anon_WITH_AES_256_CBC_SHA256,
    tlscsDH_DSS_EXPORT_WITH_DES40_CBC_SHA,
    tlscsDH_DSS_WITH_DES_CBC_SHA,
    tlscsDH_DSS_WITH_3DES_EDE_CBC_SHA,
    tlscsDH_RSA_EXPORT_WITH_DES40_CBC_SHA,
    tlscsDH_RSA_WITH_DES_CBC_SHA,
    tlscsDH_RSA_WITH_3DES_EDE_CBC_SHA,
    tlscsDH_DSS_WITH_AES_128_CBC_SHA,
    tlscsDH_DSS_WITH_AES_256_CBC_SHA,
    tlscsDH_DSS_WITH_AES_128_CBC_SHA256,
    tlscsDH_DSS_WITH_AES_256_CBC_SHA256,
    tlscsDH_RSA_WITH_AES_128_CBC_SHA,
    tlscsDH_RSA_WITH_AES_256_CBC_SHA,
    tlscsDH_RSA_WITH_AES_128_CBC_SHA256,
    tlscsDH_RSA_WITH_AES_256_CBC_SHA256,
    tlscsRSA_EXPORT_WITH_RC4_40_MD5,
    tlscsRSA_WITH_RC4_128_MD5,
    tlscsRSA_WITH_RC4_128_SHA,
    tlscsRSA_EXPORT_WITH_RC2_CBC_40_MD5,
    tlscsRSA_WITH_IDEA_CBC_SHA,
    tlscsRSA_EXPORT_WITH_DES40_CBC_SHA,
    tlscsRSA_WITH_DES_CBC_SHA,
    tlscsRSA_WITH_3DES_EDE_CBC_SHA,
    tlscsRSA_EXPORT1024_WITH_DES_CBC_SHA,            // draft-ietf-tls-56-bit-ciphersuites-01
    tlscsRSA_EXPORT1024_WITH_RC4_56_SHA,             // draft-ietf-tls-56-bit-ciphersuites-01
    tlscsRSA_WITH_NULL_SHA256,
    tlscsRSA_WITH_AES_128_CBC_SHA,                   // mandatory for tls 1.2 (rfc 5246)
    tlscsRSA_WITH_AES_256_CBC_SHA,
    tlscsRSA_WITH_AES_128_CBC_SHA256,
    tlscsRSA_WITH_AES_256_CBC_SHA256,
    tlscsDHE_DSS_EXPORT_WITH_DES40_CBC_SHA,
    tlscsDHE_DSS_WITH_DES_CBC_SHA,
    tlscsDHE_DSS_WITH_3DES_EDE_CBC_SHA,
    tlscsDHE_DSS_WITH_RC4_128_SHA,                   // draft-ietf-tls-56-bit-ciphersuites-01
    tlscsDHE_RSA_EXPORT_WITH_DES40_CBC_SHA,
    tlscsDHE_RSA_WITH_DES_CBC_SHA,
    tlscsDHE_RSA_WITH_3DES_EDE_CBC_SHA,
    tlscsDHE_DSS_WITH_AES_128_CBC_SHA,
    tlscsDHE_DSS_WITH_AES_256_CBC_SHA,
    tlscsDHE_DSS_WITH_AES_128_CBC_SHA256,
    tlscsDHE_DSS_WITH_AES_256_CBC_SHA256,
    tlscsDHE_RSA_WITH_AES_128_CBC_SHA,
    tlscsDHE_RSA_WITH_AES_256_CBC_SHA,
    tlscsDHE_RSA_WITH_AES_128_CBC_SHA256,
    tlscsDHE_RSA_WITH_AES_256_CBC_SHA256,
    tlscsECDHE_ECDSA_WITH_AES_128_CBC_SHA256,        // rfc 5289
    tlscsECDHE_ECDSA_WITH_AES_256_CBC_SHA384,        // rfc 5289
    tlscsECDH_ECDSA_WITH_AES_128_CBC_SHA256,         // rfc 5289
    tlscsECDH_ECDSA_WITH_AES_256_CBC_SHA384,         // rfc 5289
    tlscsECDHE_RSA_WITH_AES_128_CBC_SHA256,          // rfc 5289
    tlscsECDHE_RSA_WITH_AES_256_CBC_SHA384,          // rfc 5289
    tlscsECDH_RSA_WITH_AES_128_CBC_SHA256,           // rfc 5289
    tlscsECDH_RSA_WITH_AES_256_CBC_SHA384            // rfc 5289
  );
  TTLSCipherSuites = set of TTLSCipherSuite;

  TTLSCipherSuiteKeyExchange = (
    tlscskeNone,
    tlscskeNULL,
    tlscskeRSA,
    tlscskeRSA_EXPORT,
    tlscskeRSA_EXPORT1024,
    tlscskeDH_DSS_EXPORT,
    tlscskeDH_DSS,
    tlscskeDH_RSA_EXPORT,
    tlscskeDH_RSA,
    tlscskeDHE_DSS_EXPORT,
    tlscskeDHE_DSS,
    tlscskeDHE_RSA_EXPORT,
    tlscskeDHE_RSA,
    tlscskeDH_anon_EXPORT,
    tlscskeDH_anon,
    tlscskeECDHE_ECDSA,
    tlscskeECDH_ECDSA,
    tlscskeECDHE_RSA,
    tlscskeECDH_RSA
  );

  TTLSCipherSuiteAuthentication = (
    tlscsaNone,
    tlscsaAnon,
    tlscsaRSA,
    tlscsaDSS,
    tlscsaPSK,
    tlscsaECDSA
    );

  TTLSCipherSuiteCipher = (
    tlscscNone,
    tlscscNULL,
    tlscscRC2_CBC_40,
    tlscscRC4_40,
    tlscscRC4_56,
    tlscscRC4_128,
    tlscscIDEA_CBC,
    tlscscDES40_CBC,
    tlscscDES_CBC,
    tlscsc3DES_EDE_CBC,
    tlscscAES_128_CBC,
    tlscscAES_256_CBC
  );

  TTLSCipherSuiteHash = (
    tlscshNone,
    tlscshNULL,
    tlscshSHA,
    tlscshSHA256,
    tlscshSHA384,
    tlscshMD5
  );

type
  TTLSCipherSuiteRec = packed record
    B1, B2 : Byte;
  end;
  PTLSCipherSuiteRec = ^TTLSCipherSuiteRec;

const
  TLSCipherSuiteRecSize = Sizeof(TTLSCipherSuiteRec);

type
  TTLSCipherSuiteInfo = record
    Name           : RawByteString;
    KeyExchange    : TTLSCipherSuiteKeyExchange;
    Authentication : TTLSCipherSuiteAuthentication;
    Cipher         : TTLSCipherSuiteCipher;
    Hash           : TTLSCipherSuiteHash;
    Rec            : TTLSCipherSuiteRec;
    ServerSupport  : Boolean;
    ClientSupport  : Boolean;
    MinVersion     : TTLSProtocolVersion;
  end;
  PTLSCipherSuiteInfo = ^TTLSCipherSuiteInfo;

const
  TLSCipherSuiteInfo : array[TTLSCipherSuite] of TTLSCipherSuiteInfo = (
    ( // None
    Name           : '';
    KeyExchange    : tlscskeNone;
    Authentication : tlscsaNone;
    Cipher         : tlscscNone;
    Hash           : tlscshNone;
    Rec            : (B1: $FF; B2: $FF);
    ServerSupport  : False;
    ClientSupport  : False;
    MinVersion     : (major: 0; minor: 0);
    ),
    ( // NULL_WITH_NULL_NULL
    Name           : 'NULL_WITH_NULL_NULL';
    KeyExchange    : tlscskeNULL;
    Authentication : tlscsaNone;
    Cipher         : tlscscNULL;
    Hash           : tlscshNULL;
    Rec            : (B1: $00; B2: $00);
    ServerSupport  : False;
    ClientSupport  : False;
    MinVersion     : (major: 0; minor: 0);
    ),
    ( // RSA_WITH_NULL_MD5
    Name           : 'RSA_WITH_NULL_MD5';
    KeyExchange    : tlscskeRSA;
    Authentication : tlscsaAnon;
    Cipher         : tlscscNULL;
    Hash           : tlscshMD5;
    Rec            : (B1: $00; B2: $01);
    ServerSupport  : False;
    ClientSupport  : False;
    ),
    ( // RSA_WITH_NULL_SHA
    Name           : 'RSA_WITH_NULL_SHA';
    KeyExchange    : tlscskeRSA;
    Authentication : tlscsaAnon;
    Cipher         : tlscscNULL;
    Hash           : tlscshSHA;
    Rec            : (B1: $00; B2: $02);
    ServerSupport  : False;
    ClientSupport  : False;
    ),
    ( // DH_anon_EXPORT_WITH_RC4_40_MD5
    Name           : 'DH_anon_EXPORT_WITH_RC4_40_MD5';
    KeyExchange    : tlscskeDH_anon_EXPORT;
    Authentication : tlscsaAnon;
    Cipher         : tlscscRC4_40;
    Hash           : tlscshMD5;
    Rec            : (B1: $00; B2: $17);
    ServerSupport  : False;
    ClientSupport  : False;
    ),
    ( // DH_anon_WITH_RC4_128_MD5
    Name           : 'DH_anon_WITH_RC4_128_MD5';
    KeyExchange    : tlscskeDH_anon;
    Authentication : tlscsaAnon;
    Cipher         : tlscscRC4_128;
    Hash           : tlscshMD5;
    Rec            : (B1: $00; B2: $18);
    ServerSupport  : False;
    ClientSupport  : False;
    ),
    ( // DH_anon_EXPORT_WITH_DES40_CBC_SHA
    Name           : 'DH_anon_EXPORT_WITH_DES40_CBC_SHA';
    KeyExchange    : tlscskeDH_anon_EXPORT;
    Authentication : tlscsaAnon;
    Cipher         : tlscscDES40_CBC;
    Hash           : tlscshSHA;
    Rec            : (B1: $00; B2: $19);
    ServerSupport  : False;
    ClientSupport  : False;
    ),
    ( // DH_anon_WITH_DES_CBC_SHA
    Name           : 'DH_anon_WITH_DES_CBC_SHA';
    KeyExchange    : tlscskeDH_anon;
    Authentication : tlscsaAnon;
    Cipher         : tlscscDES_CBC;
    Hash           : tlscshSHA;
    Rec            : (B1: $00; B2: $1A);
    ServerSupport  : False;
    ClientSupport  : False;
    ),
    ( // DH_anon_WITH_3DES_EDE_CBC_SHA
    Name           : 'DH_anon_WITH_3DES_EDE_CBC_SHA';
    KeyExchange    : tlscskeDH_anon;
    Authentication : tlscsaAnon;
    Cipher         : tlscsc3DES_EDE_CBC;
    Hash           : tlscshSHA;
    Rec            : (B1: $00; B2: $1B);
    ServerSupport  : False;
    ClientSupport  : False;
    ),
    ( // DH_anon_WITH_AES_128_CBC_SHA256
    Name           : 'DH_anon_WITH_AES_128_CBC_SHA256';
    KeyExchange    : tlscskeDH_anon;
    Authentication : tlscsaAnon;
    Cipher         : tlscscAES_128_CBC;
    Hash           : tlscshSHA256;
    Rec            : (B1: $00; B2: $6C);
    ServerSupport  : False;
    ClientSupport  : False;
    ),
    ( // DH_anon_WITH_AES_256_CBC_SHA256
    Name           : 'DH_anon_WITH_AES_256_CBC_SHA256';
    KeyExchange    : tlscskeDH_anon;
    Authentication : tlscsaAnon;
    Cipher         : tlscscAES_256_CBC;
    Hash           : tlscshSHA256;
    Rec            : (B1: $00; B2: $6D);
    ServerSupport  : False;
    ClientSupport  : False;
    ),
    ( // DH_DSS_EXPORT_WITH_DES40_CBC_SHA
    Name           : 'DH_DSS_EXPORT_WITH_DES40_CBC_SHA';
    KeyExchange    : tlscskeDH_DSS_EXPORT;
    Authentication : tlscsaDSS;
    Cipher         : tlscscDES40_CBC;
    Hash           : tlscshSHA;
    Rec            : (B1: $00; B2: $0B);
    ServerSupport  : False;
    ClientSupport  : False;
    ),
    ( // DH_DSS_WITH_DES_CBC_SHA
    Name           : 'DH_DSS_WITH_DES_CBC_SHA';
    KeyExchange    : tlscskeDH_DSS;
    Authentication : tlscsaDSS;
    Cipher         : tlscscDES_CBC;
    Hash           : tlscshSHA;
    Rec            : (B1: $00; B2: $0C);
    ServerSupport  : False;
    ClientSupport  : False;
    ),
    ( // DH_DSS_WITH_3DES_EDE_CBC_SHA
    Name           : 'DH_DSS_WITH_3DES_EDE_CBC_SHA';
    KeyExchange    : tlscskeDH_DSS;
    Authentication : tlscsaDSS;
    Cipher         : tlscsc3DES_EDE_CBC;
    Hash           : tlscshSHA;
    Rec            : (B1: $00; B2: $0D);
    ServerSupport  : False;
    ClientSupport  : False;
    ),
    ( // DH_RSA_EXPORT_WITH_DES40_CBC_SHA
    Name           : 'DH_RSA_EXPORT_WITH_DES40_CBC_SHA';
    KeyExchange    : tlscskeDH_RSA_EXPORT;
    Authentication : tlscsaAnon;
    Cipher         : tlscscDES40_CBC;
    Hash           : tlscshSHA;
    Rec            : (B1: $00; B2: $0E);
    ServerSupport  : False;
    ClientSupport  : False;
    ),
    ( // DH_RSA_WITH_DES_CBC_SHA
    Name           : 'DH_RSA_WITH_DES_CBC_SHA';
    KeyExchange    : tlscskeDH_RSA;
    Authentication : tlscsaRSA;
    Cipher         : tlscscDES_CBC;
    Hash           : tlscshSHA;
    Rec            : (B1: $00; B2: $0F);
    ServerSupport  : False;
    ClientSupport  : False;
    ),
    ( // DH_RSA_WITH_3DES_EDE_CBC_SHA
    Name           : 'DH_RSA_WITH_3DES_EDE_CBC_SHA';
    KeyExchange    : tlscskeDH_RSA;
    Authentication : tlscsaRSA;
    Cipher         : tlscsc3DES_EDE_CBC;
    Hash           : tlscshSHA;
    Rec            : (B1: $00; B2: $10);
    ServerSupport  : False;
    ClientSupport  : False;
    ),
    ( // DH_DSS_WITH_AES_128_CBC_SHA
    Name           : 'DH_DSS_WITH_AES_128_CBC_SHA';
    KeyExchange    : tlscskeDH_DSS;
    Authentication : tlscsaDSS;
    Cipher         : tlscscAES_128_CBC;
    Hash           : tlscshSHA;
    Rec            : (B1: $00; B2: $30);
    ServerSupport  : False;
    ClientSupport  : False;
    ),
    ( // DH_DSS_WITH_AES_256_CBC_SHA
    Name           : 'DH_DSS_WITH_AES_256_CBC_SHA';
    KeyExchange    : tlscskeDH_DSS;
    Authentication : tlscsaDSS;
    Cipher         : tlscscAES_256_CBC;
    Hash           : tlscshSHA;
    Rec            : (B1: $00; B2: $36);
    ServerSupport  : False;
    ClientSupport  : False;
    ),
    ( // DH_DSS_WITH_AES_128_CBC_SHA256
    Name           : 'DH_DSS_WITH_AES_128_CBC_SHA256';
    KeyExchange    : tlscskeDH_DSS;
    Authentication : tlscsaDSS;
    Cipher         : tlscscAES_128_CBC;
    Hash           : tlscshSHA256;
    Rec            : (B1: $00; B2: $3E);
    ServerSupport  : False;
    ClientSupport  : False;
    ),
    ( // DH_DSS_WITH_AES_256_CBC_SHA256
    Name           : 'DH_DSS_WITH_AES_256_CBC_SHA256';
    KeyExchange    : tlscskeDH_DSS;
    Authentication : tlscsaDSS;
    Cipher         : tlscscAES_256_CBC;
    Hash           : tlscshSHA256;
    Rec            : (B1: $00; B2: $68);
    ServerSupport  : False;
    ClientSupport  : False;
    ),
    ( // DH_RSA_WITH_AES_128_CBC_SHA
    Name           : 'DH_RSA_WITH_AES_128_CBC_SHA';
    KeyExchange    : tlscskeDH_RSA;
    Authentication : tlscsaRSA;
    Cipher         : tlscscAES_128_CBC;
    Hash           : tlscshSHA;
    Rec            : (B1: $00; B2: $31);
    ServerSupport  : False;
    ClientSupport  : False;
    ),
    ( // DH_RSA_WITH_AES_256_CBC_SHA
    Name           : 'DH_RSA_WITH_AES_256_CBC_SHA';
    KeyExchange    : tlscskeDH_RSA;
    Authentication : tlscsaRSA;
    Cipher         : tlscscAES_256_CBC;
    Hash           : tlscshSHA;
    Rec            : (B1: $00; B2: $37);
    ServerSupport  : False;
    ClientSupport  : False;
    ),
    ( // DH_RSA_WITH_AES_128_CBC_SHA256
    Name           : 'DH_RSA_WITH_AES_128_CBC_SHA256';
    KeyExchange    : tlscskeDH_RSA;
    Authentication : tlscsaRSA;
    Cipher         : tlscscAES_128_CBC;
    Hash           : tlscshSHA256;
    Rec            : (B1: $00; B2: $3F);
    ServerSupport  : False;
    ClientSupport  : False;
    ),
    ( // DH_RSA_WITH_AES_256_CBC_SHA256
    Name           : 'DH_RSA_WITH_AES_256_CBC_SHA256';
    KeyExchange    : tlscskeDH_RSA;
    Authentication : tlscsaRSA;
    Cipher         : tlscscAES_256_CBC;
    Hash           : tlscshSHA256;
    Rec            : (B1: $00; B2: $69);
    ServerSupport  : False;
    ClientSupport  : False;
    ),
    ( // RSA_EXPORT_WITH_RC4_40_MD5
    Name           : 'RSA_EXPORT_WITH_RC4_40_MD5';
    KeyExchange    : tlscskeRSA_EXPORT;
    Authentication : tlscsaAnon;
    Cipher         : tlscscRC4_40;
    Hash           : tlscshMD5;
    Rec            : (B1: $00; B2: $03);
    ServerSupport  : False;
    ClientSupport  : False;
    MinVersion     : (major: 3; minor: 0); // SSL 3
    ),
    ( // RSA_WITH_RC4_128_MD5
    Name           : 'RSA_WITH_RC4_128_MD5';
    KeyExchange    : tlscskeRSA;
    Authentication : tlscsaAnon;
    Cipher         : tlscscRC4_128;
    Hash           : tlscshMD5;
    Rec            : (B1: $00; B2: $04);
    ServerSupport  : True;
    ClientSupport  : True;
    MinVersion     : (major: 3; minor: 0); // SSL 3
    ),
    ( // RSA_WITH_RC4_128_SHA
    Name           : 'RSA_WITH_RC4_128_SHA';
    KeyExchange    : tlscskeRSA;
    Authentication : tlscsaAnon;
    Cipher         : tlscscRC4_128;
    Hash           : tlscshSHA;
    Rec            : (B1: $00; B2: $05);
    ServerSupport  : True;
    ClientSupport  : True;
    MinVersion     : (major: 3; minor: 0); // SSL 3
    ),
    ( // RSA_EXPORT_WITH_RC2_CBC_40_MD5
    Name           : 'RSA_EXPORT_WITH_RC2_CBC_40_MD5';
    KeyExchange    : tlscskeRSA_EXPORT;
    Authentication : tlscsaAnon;
    Cipher         : tlscscRC2_CBC_40;
    Hash           : tlscshMD5;
    Rec            : (B1: $00; B2: $06);
    ServerSupport  : False;
    ClientSupport  : False;
    MinVersion     : (major: 3; minor: 0); // SSL 3
    ),
    ( // RSA_WITH_IDEA_CBC_SHA
    Name           : 'RSA_WITH_IDEA_CBC_SHA';
    KeyExchange    : tlscskeRSA;
    Authentication : tlscsaAnon;
    Cipher         : tlscscIDEA_CBC;
    Hash           : tlscshSHA;
    Rec            : (B1: $00; B2: $07);
    ServerSupport  : False;
    ClientSupport  : False;
    ),
    ( // RSA_EXPORT_WITH_DES40_CBC_SHA
    Name           : 'RSA_EXPORT_WITH_DES40_CBC_SHA';
    KeyExchange    : tlscskeRSA_EXPORT;
    Authentication : tlscsaAnon;
    Cipher         : tlscscDES40_CBC;
    Hash           : tlscshSHA;
    Rec            : (B1: $00; B2: $08);
    ServerSupport  : False;
    ClientSupport  : False;
    ),
    ( // RSA_WITH_DES_CBC_SHA
    Name           : 'RSA_WITH_DES_CBC_SHA';
    KeyExchange    : tlscskeRSA;
    Authentication : tlscsaAnon;
    Cipher         : tlscscDES_CBC;
    Hash           : tlscshSHA;
    Rec            : (B1: $00; B2: $09);
    ServerSupport  : True;
    ClientSupport  : True;
    MinVersion     : (major: 3; minor: 0); // SSL 3
    ),
    ( // RSA_WITH_3DES_EDE_CBC_SHA
    Name           : 'RSA_WITH_3DES_EDE_CBC_SHA';
    KeyExchange    : tlscskeRSA;
    Authentication : tlscsaAnon;
    Cipher         : tlscsc3DES_EDE_CBC;
    Hash           : tlscshSHA;
    Rec            : (B1: $00; B2: $0A);
    ServerSupport  : False;
    ClientSupport  : False;
    MinVersion     : (major: 3; minor: 0); // SSL 3
    ),
    ( // RSA_EXPORT1024_WITH_DES_CBC_SHA
    Name           : 'RSA_EXPORT1024_WITH_DES_CBC_SHA';
    KeyExchange    : tlscskeRSA_EXPORT1024;
    Authentication : tlscsaAnon;
    Cipher         : tlscscDES_CBC;
    Hash           : tlscshSHA;
    Rec            : (B1: $00; B2: $62);
    ServerSupport  : False;
    ClientSupport  : False;
    MinVersion     : (major: 3; minor: 0); // SSL 3
    ),
    ( // RSA_EXPORT1024_WITH_RC4_56_SHA
    Name           : 'RSA_EXPORT1024_WITH_RC4_56_SHA';
    KeyExchange    : tlscskeRSA_EXPORT1024;
    Authentication : tlscsaAnon;
    Cipher         : tlscscRC4_56;
    Hash           : tlscshSHA;
    Rec            : (B1: $00; B2: $64);
    ServerSupport  : False;
    ClientSupport  : False;
    MinVersion     : (major: 3; minor: 0); // SSL 3
    ),
    ( // RSA_WITH_NULL_SHA256
    Name           : 'RSA_WITH_NULL_SHA256';
    KeyExchange    : tlscskeRSA;
    Authentication : tlscsaAnon;
    Cipher         : tlscscNULL;
    Hash           : tlscshSHA256;
    Rec            : (B1: $00; B2: $3B);
    ServerSupport  : False;
    ClientSupport  : False;
    ),
    ( // RSA_WITH_AES_128_CBC_SHA
    Name           : 'RSA_WITH_AES_128_CBC_SHA';
    KeyExchange    : tlscskeRSA;
    Authentication : tlscsaAnon;
    Cipher         : tlscscAES_128_CBC;
    Hash           : tlscshSHA;
    Rec            : (B1: $00; B2: $2F);
    ServerSupport  : True;
    ClientSupport  : True;
    ),
    ( // RSA_WITH_AES_256_CBC_SHA
    Name           : 'RSA_WITH_AES_256_CBC_SHA';
    KeyExchange    : tlscskeRSA;
    Authentication : tlscsaAnon;
    Cipher         : tlscscAES_256_CBC;
    Hash           : tlscshSHA;
    Rec            : (B1: $00; B2: $35);
    ServerSupport  : True;
    ClientSupport  : True;
    ),
    ( // RSA_WITH_AES_128_CBC_SHA256
    Name           : 'RSA_WITH_AES_128_CBC_SHA256';
    KeyExchange    : tlscskeRSA;
    Authentication : tlscsaAnon;
    Cipher         : tlscscAES_128_CBC;
    Hash           : tlscshSHA256;
    Rec            : (B1: $00; B2: $3C);
    ServerSupport  : True;
    ClientSupport  : True;
    MinVersion     : (major: 3; minor: 3); // TLS 1.2
    ),
    ( // RSA_WITH_AES_256_CBC_SHA256
    Name           : 'RSA_WITH_AES_256_CBC_SHA256';
    KeyExchange    : tlscskeRSA;
    Authentication : tlscsaAnon;
    Cipher         : tlscscAES_256_CBC;
    Hash           : tlscshSHA256;
    Rec            : (B1: $00; B2: $3D);
    ServerSupport  : True;
    ClientSupport  : True;
    MinVersion     : (major: 3; minor: 3); // TLS 1.2
    ),
    ( // DHE_DSS_EXPORT_WITH_DES40_CBC_SHA
    Name           : 'DHE_DSS_EXPORT_WITH_DES40_CBC_SHA';
    KeyExchange    : tlscskeDHE_DSS_EXPORT;
    Authentication : tlscsaDSS;
    Cipher         : tlscscDES40_CBC;
    Hash           : tlscshSHA;
    Rec            : (B1: $00; B2: $11);
    ServerSupport  : False;
    ClientSupport  : False;
    ),
    ( // DHE_DSS_WITH_DES_CBC_SHA
    Name           : 'DHE_DSS_WITH_DES_CBC_SHA';
    KeyExchange    : tlscskeDHE_DSS;
    Authentication : tlscsaDSS;
    Cipher         : tlscscDES_CBC;
    Hash           : tlscshSHA;
    Rec            : (B1: $00; B2: $12);
    ServerSupport  : False;
    ClientSupport  : False;
    ),
    ( // DHE_DSS_WITH_3DES_EDE_CBC_SHA
    Name           : 'DHE_DSS_WITH_3DES_EDE_CBC_SHA';
    KeyExchange    : tlscskeDHE_DSS;
    Authentication : tlscsaDSS;
    Cipher         : tlscsc3DES_EDE_CBC;
    Hash           : tlscshSHA;
    Rec            : (B1: $00; B2: $13);
    ServerSupport  : False;
    ClientSupport  : False;
    ),
    ( // DHE_DSS_WITH_RC4_128_SHA
    Name           : 'DHE_DSS_WITH_RC4_128_SHA';
    KeyExchange    : tlscskeDHE_DSS;
    Authentication : tlscsaDSS;
    Cipher         : tlscscRC4_128;
    Hash           : tlscshSHA;
    Rec            : (B1: $00; B2: $66);
    ServerSupport  : False;
    ClientSupport  : False;
    ),
    ( // DHE_RSA_EXPORT_WITH_DES40_CBC_SHA
    Name           : 'DHE_RSA_EXPORT_WITH_DES40_CBC_SHA';
    KeyExchange    : tlscskeDHE_RSA_EXPORT;
    Authentication : tlscsaRSA;
    Cipher         : tlscscDES40_CBC;
    Hash           : tlscshSHA;
    Rec            : (B1: $00; B2: $14);
    ServerSupport  : False;
    ClientSupport  : False;
    ),
    ( // DHE_RSA_WITH_DES_CBC_SHA
    Name           : 'DHE_RSA_WITH_DES_CBC_SHA';
    KeyExchange    : tlscskeDHE_RSA;
    Authentication : tlscsaRSA;
    Cipher         : tlscscDES_CBC;
    Hash           : tlscshSHA;
    Rec            : (B1: $00; B2: $15);
    ServerSupport  : True;
    ClientSupport  : True;
    ),
    ( // DHE_RSA_WITH_3DES_EDE_CBC_SHA
    Name           : 'DHE_RSA_WITH_3DES_EDE_CBC_SHA';
    KeyExchange    : tlscskeDHE_RSA;
    Authentication : tlscsaRSA;
    Cipher         : tlscsc3DES_EDE_CBC;
    Hash           : tlscshSHA;
    Rec            : (B1: $00; B2: $16);
    ServerSupport  : False;
    ClientSupport  : False;
    ),
    ( // DHE_DSS_WITH_AES_128_CBC_SHA
    Name           : 'DHE_DSS_WITH_AES_128_CBC_SHA  ';
    KeyExchange    : tlscskeDHE_DSS;
    Authentication : tlscsaDSS;
    Cipher         : tlscscAES_128_CBC;
    Hash           : tlscshSHA;
    Rec            : (B1: $00; B2: $32);
    ServerSupport  : False;
    ClientSupport  : False;
    ),
    ( // DHE_DSS_WITH_AES_256_CBC_SHA
    Name           : 'DHE_DSS_WITH_AES_256_CBC_SHA';
    KeyExchange    : tlscskeDHE_DSS;
    Authentication : tlscsaDSS;
    Cipher         : tlscscAES_256_CBC;
    Hash           : tlscshSHA;
    Rec            : (B1: $00; B2: $38);
    ServerSupport  : False;
    ClientSupport  : False;
    ),
    ( // DHE_DSS_WITH_AES_128_CBC_SHA256
    Name           : 'DHE_DSS_WITH_AES_128_CBC_SHA256';
    KeyExchange    : tlscskeDHE_DSS;
    Authentication : tlscsaDSS;
    Cipher         : tlscscAES_128_CBC;
    Hash           : tlscshSHA256;
    Rec            : (B1: $00; B2: $40);
    ServerSupport  : False;
    ClientSupport  : False;
    ),
    ( // DHE_DSS_WITH_AES_256_CBC_SHA256
    Name           : 'DHE_DSS_WITH_AES_256_CBC_SHA256';
    KeyExchange    : tlscskeDHE_DSS;
    Authentication : tlscsaDSS;
    Cipher         : tlscscAES_256_CBC;
    Hash           : tlscshSHA256;
    Rec            : (B1: $00; B2: $6A);
    ServerSupport  : False;
    ClientSupport  : False;
    ),
    ( // DHE_RSA_WITH_AES_128_CBC_SHA
    Name           : 'DHE_RSA_WITH_AES_128_CBC_SHA';
    KeyExchange    : tlscskeDHE_RSA;
    Authentication : tlscsaRSA;
    Cipher         : tlscscAES_128_CBC;
    Hash           : tlscshSHA;
    Rec            : (B1: $00; B2: $33);
    ServerSupport  : True;
    ClientSupport  : True;
    ),
    ( // DHE_RSA_WITH_AES_256_CBC_SHA
    Name           : 'DHE_RSA_WITH_AES_256_CBC_SHA';
    KeyExchange    : tlscskeDHE_RSA;
    Authentication : tlscsaRSA;
    Cipher         : tlscscAES_256_CBC;
    Hash           : tlscshSHA;
    Rec            : (B1: $00; B2: $39);
    ServerSupport  : True;
    ClientSupport  : True;
    ),
    ( // DHE_RSA_WITH_AES_128_CBC_SHA256
    Name           : 'DHE_RSA_WITH_AES_128_CBC_SHA256';
    KeyExchange    : tlscskeDHE_RSA;
    Authentication : tlscsaRSA;
    Cipher         : tlscscAES_128_CBC;
    Hash           : tlscshSHA256;
    Rec            : (B1: $00; B2: $67);
    ServerSupport  : True;
    ClientSupport  : True;
    ),
    ( // DHE_RSA_WITH_AES_256_CBC_SHA256
    Name           : 'DHE_RSA_WITH_AES_256_CBC_SHA256';
    KeyExchange    : tlscskeDHE_RSA;
    Authentication : tlscsaRSA;
    Cipher         : tlscscAES_256_CBC;
    Hash           : tlscshSHA256;
    Rec            : (B1: $00; B2: $6B);
    ServerSupport  : True;
    ClientSupport  : True;
    ),
    ( // ECDHE_ECDSA_WITH_AES_128_CBC_SHA256
    Name           : 'ECDHE_ECDSA_WITH_AES_128_CBC_SHA256';
    KeyExchange    : tlscskeECDHE_ECDSA;
    Authentication : tlscsaECDSA;
    Cipher         : tlscscAES_128_CBC;
    Hash           : tlscshSHA256;
    Rec            : (B1: $C0; B2: $23);
    ServerSupport  : False;
    ClientSupport  : False;
    ),
    ( // ECDHE_ECDSA_WITH_AES_256_CBC_SHA384
    Name           : 'ECDHE_ECDSA_WITH_AES_256_CBC_SHA384';
    KeyExchange    : tlscskeECDHE_ECDSA;
    Authentication : tlscsaECDSA;
    Cipher         : tlscscAES_256_CBC;
    Hash           : tlscshSHA384;
    Rec            : (B1: $C0; B2: $24);
    ServerSupport  : False;
    ClientSupport  : False;
    ),
    ( // ECDH_ECDSA_WITH_AES_128_CBC_SHA256
    Name           : 'ECDH_ECDSA_WITH_AES_128_CBC_SHA256';
    KeyExchange    : tlscskeECDH_ECDSA;
    Authentication : tlscsaECDSA;
    Cipher         : tlscscAES_128_CBC;
    Hash           : tlscshSHA256;
    Rec            : (B1: $C0; B2: $25);
    ServerSupport  : False;
    ClientSupport  : False;
    ),
    ( // ECDH_ECDSA_WITH_AES_256_CBC_SHA384
    Name           : 'ECDH_ECDSA_WITH_AES_256_CBC_SHA384';
    KeyExchange    : tlscskeECDH_ECDSA;
    Authentication : tlscsaECDSA;
    Cipher         : tlscscAES_256_CBC;
    Hash           : tlscshSHA384;
    Rec            : (B1: $C0; B2: $26);
    ServerSupport  : False;
    ClientSupport  : False;
    ),
    ( // ECDHE_RSA_WITH_AES_128_CBC_SHA256
    Name           : 'ECDHE_RSA_WITH_AES_128_CBC_SHA256';
    KeyExchange    : tlscskeECDHE_RSA;
    Authentication : tlscsaRSA;
    Cipher         : tlscscAES_128_CBC;
    Hash           : tlscshSHA256;
    Rec            : (B1: $C0; B2: $27);
    ServerSupport  : False;
    ClientSupport  : False;
    ),
    ( // ECDHE_RSA_WITH_AES_256_CBC_SHA384
    Name           : 'ECDHE_RSA_WITH_AES_256_CBC_SHA384';
    KeyExchange    : tlscskeECDHE_RSA;
    Authentication : tlscsaRSA;
    Cipher         : tlscscAES_256_CBC;
    Hash           : tlscshSHA384;
    Rec            : (B1: $C0; B2: $28);
    ServerSupport  : False;
    ClientSupport  : False;
    ),
    ( // ECDH_RSA_WITH_AES_128_CBC_SHA256
    Name           : 'ECDH_RSA_WITH_AES_128_CBC_SHA256';
    KeyExchange    : tlscskeECDH_RSA;
    Authentication : tlscsaRSA;
    Cipher         : tlscscAES_128_CBC;
    Hash           : tlscshSHA256;
    Rec            : (B1: $C0; B2: $29);
    ServerSupport  : False;
    ClientSupport  : False;
    ),
    ( // ECDH_RSA_WITH_AES_256_CBC_SHA384
    Name           : 'ECDH_RSA_WITH_AES_256_CBC_SHA384';
    KeyExchange    : tlscskeECDH_RSA;
    Authentication : tlscsaRSA;
    Cipher         : tlscscAES_256_CBC;
    Hash           : tlscshSHA384;
    Rec            : (B1: $C0; B2: $2A);
    ServerSupport  : False;
    ClientSupport  : False;
    )
    );

function GetCipherSuiteByRec(const B1, B2: Byte): TTLSCipherSuite;
function GetCipherSuiteByName(const Name: RawByteString): TTLSCipherSuite;

type
  TTLSCipherSuiteKeyExchangeInfo = record
    Name        : RawByteString;
    Algorithm   : TTLSKeyExchangeAlgorithm;
    Exportable  : Boolean;
  end;
  PTLSCipherSuiteKeyExchangeInfo = ^TTLSCipherSuiteKeyExchangeInfo;

const
  TLSCipherSuiteKeyExchangeInfo : array[TTLSCipherSuiteKeyExchange] of TTLSCipherSuiteKeyExchangeInfo = (
    ( // None
      Name        : '';
      Algorithm   : tlskeaNone;
      Exportable  : True;
    ),
    ( // NULL
      Name        : 'NULL';
      Algorithm   : tlskeaNULL;
      Exportable  : True;
    ),
    ( // RSA
      Name        : 'RSA';
      Algorithm   : tlskeaRSA;
      Exportable  : False;
    ),
    ( // RSA_EXPORT
      Name        : 'RSA_EXPORT';
      Algorithm   : tlskeaRSA;
      Exportable  : True;
    ),
    ( // RSA_EXPORT1024
      Name        : 'RSA_EXPORT1024';
      Algorithm   : tlskeaRSA;
      Exportable  : True;
    ),
    ( // DH_DSS_EXPORT
      Name        : 'DH_DSS_EXPORT';
      Algorithm   : tlskeaDH_DSS;
      Exportable  : True;
    ),
    ( // DH_DSS
      Name        : 'DH_DSS';
      Algorithm   : tlskeaDH_DSS;
      Exportable  : False;
    ),
    ( // DH_RSA_EXPORT
      Name        : 'DH_RSA_EXPORT';
      Algorithm   : tlskeaDH_RSA;
      Exportable  : True;
    ),
    ( // DH_RSA
      Name        : 'DH_RSA';
      Algorithm   : tlskeaDH_RSA;
      Exportable  : False;
    ),
    ( // DHE_DSS_EXPORT
      Name        : 'DHE_DSS_EXPORT';
      Algorithm   : tlskeaDHE_DSS;
      Exportable  : True;
    ),
    (  // DHE_DSS
      Name        : 'DHE_DSS';
      Algorithm   : tlskeaDHE_DSS;
      Exportable  : False;
    ),
    ( // DHE_RSA_EXPORT
      Name        : 'DHE_RSA_EXPORT';
      Algorithm   : tlskeaDHE_RSA;
      Exportable  : True;
    ),
    ( // DHE_RSA
      Name        : 'DHE_RSA';
      Algorithm   : tlskeaDHE_RSA;
      Exportable  : False;
    ),
    ( // DH_anon_EXPORT
      Name        : 'DH_anon_EXPORT';
      Algorithm   : tlskeaDH_Anon;
      Exportable  : True;
    ),
    ( // DH_anon
      Name        : 'DH_anon';
      Algorithm   : tlskeaDH_Anon;
      Exportable  : False;
    ),
    ( // ECDHE_ECDSA
      Name        : 'ECDHE_ECDSA';
      Algorithm   : tlskeaECDHE_ECDSA;
      Exportable  : False;
    ),
    ( // ECDH_ECDSA
      Name        : 'ECDH_ECDSA';
      Algorithm   : tlskeaECDH_ECDSA;
      Exportable  : False;
    ),
    ( // ECDHE_RSA
      Name        : 'ECDHE_RSA';
      Algorithm   : tlskeaECDHE_RSA;
      Exportable  : False;
    ),
    ( // ECDH_RSA
      Name        : 'ECDH_RSA';
      Algorithm   : tlskeaECDH_RSA;
      Exportable  : False;
    )
    );



{                                                                              }
{ Cipher Suite Cipher                                                          }
{                                                                              }
type
  TTLSCipherSuiteCipherType = (
    tlscsctNone,
    tlscsctNULL,
    tlscsctStream,
    tlscsctBlock
    );

  TTLSCipherSuiteBulkCipher = (
    tlscsbcNone,
    tlscsbcNULL,
    tlscsbcRC4,
    tlscsbcRC2,
    tlscsbcDES,
    tlscsbc3DES,
    tlscsbcIDEA,
    tlscsbcDES40,
    tlscsbcAES
    );

  TTLSCipherSuiteCipherInfo = record
    Name        : RawByteString;
    CipherType  : TTLSCipherSuiteCipherType;
    BulkCipher  : TTLSCipherSuiteBulkCipher;
    KeyBits     : Integer;
    KeyMaterial : Integer;
    ExpKeyMat   : Integer; // expanded key material
    IVSize      : Integer;
    BlockSize   : Integer;
    Exportable  : Boolean;
    Supported   : Boolean; // Not used
  end;
  PTLSCipherSuiteCipherInfo = ^TTLSCipherSuiteCipherInfo;

const
  TLSCipherSuiteCipherInfo : array[TTLSCipherSuiteCipher] of TTLSCipherSuiteCipherInfo = (
    ( // None
    Name        : '';
    CipherType  : tlscsctNone;
    BulkCipher  : tlscsbcNone;
    KeyBits     : 0;
    KeyMaterial : 0;
    ExpKeyMat   : 0;
    IVSize      : 0;
    BlockSize   : 0;
    Exportable  : True;
    Supported   : False;
    ),
    ( // NULL
    Name        : 'NULL';
    CipherType  : tlscsctNULL;
    BulkCipher  : tlscsbcNULL;
    KeyBits     : 0;
    KeyMaterial : 0;
    ExpKeyMat   : 0;
    IVSize      : 0;
    BlockSize   : 0;
    Exportable  : True;
    Supported   : True;
    ),
    ( // RC2_CBC_40
    Name        : 'RC2_CBC_40';
    CipherType  : tlscsctBlock;
    BulkCipher  : tlscsbcRC2;
    KeyBits     : 40;
    KeyMaterial : 5;
    ExpKeyMat   : 16;
    IVSize      : 8;
    BlockSize   : 8;
    Exportable  : True;
    Supported   : False;
    ),
    ( // RC4_40
    Name        : 'RC4_40';
    CipherType  : tlscsctStream;
    BulkCipher  : tlscsbcRC4;
    KeyBits     : 40;
    KeyMaterial : 5;
    ExpKeyMat   : 16;
    IVSize      : 0;
    BlockSize   : 0;
    Exportable  : True;
    Supported   : False;
    ),
    ( // RC4_56
    Name        : 'RC4_56';
    CipherType  : tlscsctStream;
    BulkCipher  : tlscsbcRC4;
    KeyBits     : 56;
    KeyMaterial : 7;
    ExpKeyMat   : 16;
    IVSize      : 0;
    BlockSize   : 0;
    Exportable  : True;
    Supported   : False;
    ),
    ( // RC4_128
    Name        : 'RC4_128';
    CipherType  : tlscsctStream;
    BulkCipher  : tlscsbcRC4;
    KeyBits     : 128;
    KeyMaterial : 16;
    ExpKeyMat   : 16;
    IVSize      : 0;
    BlockSize   : 0;
    Exportable  : False;
    Supported   : True;
    ),
    ( // IDEA_CBC
    Name        : 'IDEA_CBC';
    CipherType  : tlscsctBlock;
    BulkCipher  : tlscsbcIDEA;
    KeyBits     : 128;
    KeyMaterial : 16;
    ExpKeyMat   : 16;
    IVSize      : 8;
    BlockSize   : 8;
    Exportable  : False;
    Supported   : False;
    ),
    ( // DES40_CBC
    Name        : 'DES40_CBC';
    CipherType  : tlscsctBlock;
    BulkCipher  : tlscsbcDES40;
    KeyBits     : 40;
    KeyMaterial : 5;
    ExpKeyMat   : 8;
    IVSize      : 8;
    BlockSize   : 8;
    Exportable  : True;
    Supported   : False;
    ),
    ( // DES_CBC
    Name        : 'DES_CBC';
    CipherType  : tlscsctBlock;
    BulkCipher  : tlscsbcDES;
    KeyBits     : 64;
    KeyMaterial : 8;
    ExpKeyMat   : 8;
    IVSize      : 8;
    BlockSize   : 8;
    Exportable  : False;
    Supported   : True;
    ),
    ( // 3DES_EDE_CBC
    Name        : '3DES_EDE_CBC';
    CipherType  : tlscsctBlock;
    BulkCipher  : tlscsbc3DES;
    KeyBits     : 128;
    KeyMaterial : 24;
    ExpKeyMat   : 24;
    IVSize      : 8;
    BlockSize   : 8;
    Exportable  : False;
    Supported   : False;
    ),
    ( // AES_128_CBC
    Name        : 'AES_128_CBC';
    CipherType  : tlscsctBlock;
    BulkCipher  : tlscsbcAES;
    KeyBits     : 128;
    KeyMaterial : 16;
    ExpKeyMat   : 16;
    IVSize      : 16;
    BlockSize   : 16;
    Exportable  : False;
    Supported   : True;
    ),
    ( // AES_256_CBC
    Name        : 'AES_256_CBC';
    CipherType  : tlscsctBlock;
    BulkCipher  : tlscsbcAES;
    KeyBits     : 256;
    KeyMaterial : 32;
    ExpKeyMat   : 32;
    IVSize      : 16;
    BlockSize   : 16;
    Exportable  : False;
    Supported   : True;
    )
    );

  TLS_CIPHERSUITE_MaxBlockSize = 16;
  TLS_CIPHERSUITE_MaxIVSize    = 16;



{                                                                              }
{ Cipher Suite Hash                                                            }
{                                                                              }
type
  TTLSCipherSuiteHashInfo = record
    Name         : RawByteString;
    HashSize     : Integer;
    KeyLength    : Integer;
    MACAlgorithm : TTLSMACAlgorithm;
    Supported    : Boolean; // Not used
  end;
  PTLSCipherSuiteHashInfo = ^TTLSCipherSuiteHashInfo;

const
  TLSCipherSuiteHashInfo : array[TTLSCipherSuiteHash] of TTLSCipherSuiteHashInfo = (
    ( // None
    Name         : '';
    HashSize     : 0;
    KeyLength    : 0;
    MACAlgorithm : tlsmaNone;
    Supported    : False;
    ),
    ( // NULL
    Name         : 'NULL';
    HashSize     : 0;
    KeyLength    : 0;
    MACAlgorithm : tlsmaNULL;
    Supported    : True;
    ),
    ( // SHA
    Name         : 'SHA';
    HashSize     : 160;
    KeyLength    : 160;
    MACAlgorithm : tlsmaHMAC_SHA1;
    Supported    : True;
    ),
    ( // SHA256
    Name         : 'SHA256';
    HashSize     : 256;
    KeyLength    : 256;
    MACAlgorithm : tlsmaHMAC_SHA256;
    Supported    : True;
    ),
    ( // SHA384
    Name         : 'SHA384';
    HashSize     : 384;
    KeyLength    : 384;
    MACAlgorithm : tlsmaHMAC_SHA384;
    Supported    : True;
    ),
    ( // MD5
    Name         : 'MD5';
    HashSize     : 128;
    KeyLength    : 128;
    MACAlgorithm : tlsmaHMAC_MD5;
    Supported    : True;
    )
    );



{                                                                              }
{ Cipher Suite Details                                                         }
{                                                                              }
type
  TTLSCipherSuiteDetails = record
    CipherSuite     : TTLSCipherSuite;
    CipherSuiteInfo : PTLSCipherSuiteInfo;
    CipherInfo      : PTLSCipherSuiteCipherInfo;
    HashInfo        : PTLSCipherSuiteHashInfo;
    KeyExchangeInfo : PTLSCipherSuiteKeyExchangeInfo;
  end;

procedure InitTLSCipherSuiteDetails(var A: TTLSCipherSuiteDetails; const CipherSuite: TTLSCipherSuite);
procedure InitTLSCipherSuiteDetailsNULL(var A: TTLSCipherSuiteDetails);



implementation



{                                                                              }
{ Cipher Suite                                                                 }
{                                                                              }
function GetCipherSuiteByRec(const B1, B2: Byte): TTLSCipherSuite;
var C : TTLSCipherSuite;
    D : PTLSCipherSuiteInfo;
begin
  for C := Low(TTLSCipherSuite) to High(TTLSCipherSuite) do
    begin
      D := @TLSCipherSuiteInfo[C];
      if (D^.Rec.B1 = B1) and (D^.Rec.B2 = B2) then
        begin
          Result := C;
          exit;
        end;
    end;
  Result := tlscsNone;
end;

function GetCipherSuiteByName(const Name: RawByteString): TTLSCipherSuite;
var C : TTLSCipherSuite;
    D : PTLSCipherSuiteInfo;
begin
  for C := Low(TTLSCipherSuite) to High(TTLSCipherSuite) do
    begin
      D := @TLSCipherSuiteInfo[C];
      if D^.Name = Name then
        begin
          Result := C;
          exit;
        end;
    end;
  Result := tlscsNone;
end;



{                                                                              }
{ Cipher Suite Details                                                         }
{                                                                              }
procedure InitTLSCipherSuiteDetails(var A: TTLSCipherSuiteDetails; const CipherSuite: TTLSCipherSuite);
var C : PTLSCipherSuiteInfo;
begin
  C := @TLSCipherSuiteInfo[CipherSuite];
  A.CipherSuite     := CipherSuite;
  A.CipherSuiteInfo := C;
  A.CipherInfo      := @TLSCipherSuiteCipherInfo[C^.Cipher];
  A.HashInfo        := @TLSCipherSuiteHashInfo[C^.Hash];
  A.KeyExchangeInfo := @TLSCipherSuiteKeyExchangeInfo[C^.KeyExchange];
end;

procedure InitTLSCipherSuiteDetailsNULL(var A: TTLSCipherSuiteDetails);
begin
  InitTLSCipherSuiteDetails(A, tlscsNULL_WITH_NULL_NULL);
end;



end.

