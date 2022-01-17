{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals TLS                                         }
{   File name:        flcTLSErrors.pas                                         }
{   File version:     5.03                                                     }
{   Description:      TLS Errors                                               }
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
{   2020/05/09  5.02  Create flcTLSErrors unit from flcTLSUtils unit.          }
{   2020/05/11  5.03  ETLSError CreateAlert constructor.                       }
{                                                                              }
{******************************************************************************}

{$INCLUDE flcTLS.inc}

unit flcTLSErrors;

interface

uses
  { System }

  SysUtils,

  { TLS }

  flcTLSAlert;



{                                                                              }
{ Errors                                                                       }
{                                                                              }
const
  TLSError_None               = 0;
  TLSError_InvalidBuffer      = 1;
  TLSError_InvalidParameter   = 2;
  TLSError_InvalidCertificate = 3;
  TLSError_InvalidState       = 4;
  TLSError_DecodeError        = 5;
  TLSError_BadProtocol        = 6;

function TLSErrorMessage(const TLSError: Integer): String;

type
  ETLSError = class(Exception)
  private
    FTLSError         : Integer;
    FAlertDescription : TTLSAlertDescription;

    function  InitAlert(
              const ATLSError: Integer;
              const AAlertDescription: TTLSAlertDescription;
              const AMsg: String = ''): String;

  public
    constructor Create(
                const ATLSError: Integer;
                const AMsg: String = '');

    constructor CreateAlert(
                const ATLSError: Integer;
                const AAlertDescription: TTLSAlertDescription;
                const AMsg: String = '');

    constructor CreateAlertBufferDecode;
    constructor CreateAlertBufferEncode;

    constructor CreateAlertUnexpectedMessage;

    constructor CreateAlertBadProtocolVersion;

    property TLSError: Integer read FTLSError;
    property AlertDescription: TTLSAlertDescription read FAlertDescription;
  end;



implementation



{                                                                              }
{ Errors                                                                       }
{                                                                              }
const
  SErr_InvalidBuffer      = 'Invalid buffer';
  SErr_InvalidCertificate = 'Invalid certificate';
  SErr_InvalidParameter   = 'Invalid parameter';
  SErr_InvalidState       = 'Invalid state';
  SErr_DecodeError        = 'Decode error';
  SErr_BadProtocol        = 'Bad protocol';

function TLSErrorMessage(const TLSError: Integer): String;
begin
  case TLSError of
    TLSError_None               : Result := '';
    TLSError_InvalidBuffer      : Result := SErr_InvalidBuffer;
    TLSError_InvalidParameter   : Result := SErr_InvalidParameter;
    TLSError_InvalidCertificate : Result := SErr_InvalidCertificate;
    TLSError_InvalidState       : Result := SErr_InvalidState;
    TLSError_DecodeError        : Result := SErr_DecodeError;
    TLSError_BadProtocol        : Result := SErr_BadProtocol;
  else
    Result := '[TLSError#' + IntToStr(TLSError) + ']';
  end;
end;

constructor ETLSError.Create(
            const ATLSError: Integer;
            const AMsg: String);
var S : String;
begin
  FTLSError := ATLSError;
  if AMsg = '' then
    S := TLSErrorMessage(ATLSError)
  else
    S := AMsg;
  FAlertDescription := tlsadMax;
  inherited Create(S);
end;

constructor ETLSError.CreateAlert(
            const ATLSError: Integer;
            const AAlertDescription: TTLSAlertDescription;
            const AMsg: String);
begin
  inherited Create(InitAlert(ATLSError, AAlertDescription, AMsg));
end;

function ETLSError.InitAlert(
         const ATLSError: Integer;
         const AAlertDescription: TTLSAlertDescription;
         const AMsg: String): String;
var
  S : String;
begin
  FTLSError := ATLSError;
  FAlertDescription := AAlertDescription;

  if AMsg = '' then
    S := TLSErrorMessage(ATLSError) + ':' + TLSAlertDescriptionToStr(AAlertDescription)
  else
    S := AMsg;

  Result := S;
end;

constructor ETLSError.CreateAlertBufferDecode;
begin
  inherited Create(InitAlert(TLSError_InvalidBuffer, tlsadDecode_error));
end;

constructor ETLSError.CreateAlertBufferEncode;
begin
  inherited Create(InitAlert(TLSError_InvalidBuffer, tlsadInternal_error));
end;

constructor ETLSError.CreateAlertUnexpectedMessage;
begin
  inherited Create(InitAlert(TLSError_BadProtocol, tlsadUnexpected_message));
end;

constructor ETLSError.CreateAlertBadProtocolVersion;
begin
  inherited Create(InitAlert(TLSError_BadProtocol, tlsadProtocol_version));
end;



end.

