{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals TLS                                         }
{   File name:        flcTLSTransportTypes.pas                                 }
{   File version:     5.01                                                     }
{   Description:      TLS Transport Types                                      }
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
{   2020/05/01  5.01  Initial version: Options.                                }
{                                                                              }
{******************************************************************************}

{$INCLUDE flcTLS.inc}

unit flcTLSTransportTypes;

interface



type
  TTLSLogType = (
    tlsltDebug,
    tlsltParameter,
    tlsltInfo,
    tlsltWarning,
    tlsltError
    );



type
  TTLSVersionOption = (
    tlsvoSSL3,
    tlsvoTLS10,
    tlsvoTLS11,
    tlsvoTLS12
    );

  TTLSVersionOptions = set of TTLSVersionOption;

const
  AllTLSVersionOptions = [
    tlsvoSSL3,
    tlsvoTLS10,
    tlsvoTLS11,
    tlsvoTLS12
    ];



type
  TTLSKeyExchangeOption = (
    tlskeoRSA,
    tlskeoDH_Anon,
    tlskeoDH_RSA,
    tlskeoDHE_RSA,
    tlskeoECDH_RSA,
    tlskeoECDHE_RSA
    );

  TTLSKeyExchangeOptions = set of TTLSKeyExchangeOption;

const
  AllTLSKeyExchangeOptions = [
    tlskeoRSA,
    tlskeoDH_Anon,
    tlskeoDH_RSA,
    tlskeoDHE_RSA,
    tlskeoECDH_RSA,
    tlskeoECDHE_RSA
    ];



type
  TTLSCipherOption = (
    tlscoRC4,
    tlscoDES,
    tlsco3DES,
    tlscoAES128,
    tlscoAES256
    );

  TTLSCipherOptions = set of TTLSCipherOption;

const
  AllTLSCipherOptions = [
    tlscoRC4,
    tlscoDES,
    tlsco3DES,
    tlscoAES128,
    tlscoAES256
    ];



type
  TTLSHashOption = (
    tlshoMD5,
    tlshoSHA1,
    tlshoSHA256,
    tlshoSHA384
    );

  TTLSHashOptions = set of TTLSHashOption;

const
  AllTLSHashOptions = [
    tlshoMD5,
    tlshoSHA1,
    tlshoSHA256,
    tlshoSHA384
    ];



implementation



end.

