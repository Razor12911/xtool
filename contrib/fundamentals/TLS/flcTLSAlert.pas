{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals TLS                                         }
{   File name:        flcTLSAlert.pas                                          }
{   File version:     5.04                                                     }
{   Description:      TLS alert protocol                                       }
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
{   2010/11/30  0.02  Additional alerts from RFC 4366.                         }
{   2018/07/17  5.03  Revised for Fundamentals 5.                              }
{   2020/05/09  5.04  TLS 1.3 alerts.                                          }
{                                                                              }
{******************************************************************************}

{$INCLUDE flcTLS.inc}

unit flcTLSAlert;

interface



{                                                                              }
{ Alert Protocol                                                               }
{                                                                              }
type
  TTLSAlertLevel = (
    tlsalWarning        = 1,
    tlsalFatal          = 2,
    tlsalAlertLevelMax  = 255
  );

  TTLSAlertDescription = (
    tlsadClose_notify                    = 0,     // SSL 3
    tlsadUnexpected_message              = 10,    // SSL 3
    tlsadBad_record_mac                  = 20,    // SLL 3
    tlsadDecryption_failed               = 21,    // TLS 1.0 / TLS 1.2 reserved
    tlsadRecord_overflow                 = 22,    // TLS 1.0
    tlsadDecompression_failure           = 30,    // SLL 3
    tlsadHandshake_failure               = 40,    // SLL 3
    tlsadNo_certificate                  = 41,    // SLL 3 / TLS 1.1 reserved / TLS 1.2 reserved
    tlsadBad_certificate                 = 42,    // SLL 3
    tlsadUnsupported_certificate         = 43,    // SLL 3
    tlsadCertificate_revoked             = 44,    // SLL 3
    tlsadCertificate_expired             = 45,    // SLL 3
    tlsadCertificate_unknown             = 46,    // SLL 3
    tlsadIllegal_parameter               = 47,    // SLL 3
    tlsadUnknown_ca                      = 48,    // TLS 1.0
    tlsadAccess_denied                   = 49,    // TLS 1.0
    tlsadDecode_error                    = 50,    // TLS 1.0
    tlsadDecrypt_error                   = 51,    // TLS 1.0
    tlsadExport_restriction              = 60,    // TLS 1.0 / TLS 1.1 reserved / TLS 1.2 reserved
    tlsadProtocol_version                = 70,    // TLS 1.0
    tlsadInsufficient_security           = 71,    // TLS 1.0
    tlsadInternal_error                  = 80,    // TLS 1.0
    tlsadUser_canceled                   = 90,    // TLS 1.0
    tlsadNo_renegotiation                = 100,   // TLS 1.0
    tlsadMissing_extension               = 109,   // TLS 1.3
    tlsadUnsupported_extention           = 110,   // TLS 1.2
    tlsadCertificate_unobtainable        = 111,   // RFC 4366
    tlsadUnrecognized_name               = 112,   // RFC 4366 / TLS 1.3
    tlsadBad_certificate_status_response = 113,   // RFC 4366 / TLS 1.3
    tlsadBad_certificate_hash_value      = 114,   // RFC 4366
    tlsadUnknown_psk_identity            = 115,   // TLS 1.3
    tlsadCertificate_required            = 116,   // TLS 1.3
    tlsadNo_application_protocol         = 120,   { TLS 1.3 }    tlsadMax                             = 255  );

function  TLSAlertLevelToStr(const Level: TTLSAlertLevel): String;
function  TLSAlertDescriptionToStr(const Description: TTLSAlertDescription): String;

type
  TTLSAlert = packed record
    level       : TTLSAlertLevel;
    description : TTLSAlertDescription;
  end;
  PTLSAlert = ^TTLSAlert;

const
  TLSAlertSize = Sizeof(TTLSAlert);

procedure InitTLSAlert(var Alert: TTLSAlert;
          const Level: TTLSAlertLevel; const Description: TTLSAlertDescription);



{                                                                              }
{ Test cases                                                                   }
{                                                                              }
{$IFDEF TLS_TEST}
procedure Test;
{$ENDIF}



implementation

uses
  { System }
  SysUtils;



{                                                                              }
{ Alert Protocol                                                               }
{                                                                              }
function TLSAlertLevelToStr(const Level: TTLSAlertLevel): String;
begin
  case Level of
    tlsalWarning : Result := 'Warning';
    tlsalFatal   : Result := 'Fatal';
  else
    Result := '[Level#' + IntToStr(Ord(Level)) + ']';
  end;
end;

function TLSAlertDescriptionToStr(const Description: TTLSAlertDescription): String;
begin
  case Description of
    tlsadClose_notify                    : Result := 'Close notify';
    tlsadUnexpected_message              : Result := 'Unexpected message';
    tlsadBad_record_mac                  : Result := 'Bad record MAC';
    tlsadDecryption_failed               : Result := 'Decryption failed';
    tlsadRecord_overflow                 : Result := 'Record overflow';
    tlsadDecompression_failure           : Result := 'Decompression failure';
    tlsadHandshake_failure               : Result := 'Handshake failure';
    tlsadNo_certificate                  : Result := 'No certificate';
    tlsadBad_certificate                 : Result := 'Bad certificate';
    tlsadUnsupported_certificate         : Result := 'Unsupported certificate';
    tlsadCertificate_revoked             : Result := 'Certificate revoked';
    tlsadCertificate_expired             : Result := 'Certificate expired';
    tlsadCertificate_unknown             : Result := 'Certficiate unknown';
    tlsadIllegal_parameter               : Result := 'Illegal parameter';
    tlsadUnknown_ca                      : Result := 'Unknown CA';
    tlsadAccess_denied                   : Result := 'Access denied';
    tlsadDecode_error                    : Result := 'Decode error';
    tlsadDecrypt_error                   : Result := 'Decrypt error';
    tlsadExport_restriction              : Result := 'Export restriction';
    tlsadProtocol_version                : Result := 'Protocol version';
    tlsadInsufficient_security           : Result := 'Insufficient security';
    tlsadInternal_error                  : Result := 'Internal error';
    tlsadUser_canceled                   : Result := 'User cancelled';
    tlsadNo_renegotiation                : Result := 'No renegotiation';
    tlsadMissing_extension               : Result := 'Missing extention';
    tlsadUnsupported_extention           : Result := 'Unsuported extention';
    tlsadCertificate_unobtainable        : Result := 'Certificate unobtainable';
    tlsadUnrecognized_name               : Result := 'Unrecognised name';
    tlsadBad_certificate_status_response : Result := 'Bad certificate status response';
    tlsadBad_certificate_hash_value      : Result := 'Bad certificate hash value';
    tlsadUnknown_psk_identity            : Result := 'Unknown PSK identitiy';
    tlsadCertificate_required            : Result := 'Certificate required';
    tlsadNo_application_protocol         : Result := 'No application protocol';  else
    Result := '[Alert#' + IntToStr(Ord(Description)) + ']';
  end;
end;

procedure InitTLSAlert(
          var Alert: TTLSAlert;
          const Level: TTLSAlertLevel;
          const Description: TTLSAlertDescription);
begin
  Alert.level := Level;
  Alert.description := Description;
end;



{                                                                              }
{ Test                                                                         }
{                                                                              }
{$IFDEF TLS_TEST}
{$ASSERTIONS ON}
procedure Test;
begin
  Assert(TLSAlertSize = 2);
end;
{$ENDIF}



end.

