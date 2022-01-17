{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals TLS                                         }
{   File name:        flcTLSSessionID.pas                                      }
{   File version:     5.02                                                     }
{   Description:      TLS Session ID                                           }
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
{   2020/05/09  5.02  Create flcTLSSessionID unit from flcTLSUtils unit.       }
{                                                                              }
{******************************************************************************}

{$INCLUDE flcTLS.inc}

unit flcTLSSessionID;

interface



{                                                                              }
{ SessionID                                                                    }
{                                                                              }
const
  TLSSessionIDMaxLen = 32;

type
  TTLSSessionID = record
    Len : Byte;
    Data : array[0..TLSSessionIDMaxLen - 1] of Byte;
  end;

procedure InitTLSSessionID(var SessionID: TTLSSessionID; const A: RawByteString);
function  EncodeTLSSessionID(var Buffer; const Size: Integer; const SessionID: TTLSSessionID): Integer;
function  DecodeTLSSessionID(const Buffer; const Size: Integer; var SessionID: TTLSSessionID): Integer;



implementation

uses
  { TLS }

  flcTLSErrors;



{                                                                              }
{ SessionID                                                                    }
{   length    : Byte;                                                          }
{   SessionID : <0..32>;                                                       }
{                                                                              }
procedure InitTLSSessionID(var SessionID: TTLSSessionID; const A: RawByteString);
var
  L : Integer;
begin
  L := Length(A);
  if L > TLSSessionIDMaxLen then
    raise ETLSError.Create(TLSError_InvalidParameter, 'Invalid SessionID length');
  SessionID.Len := Byte(L);
  FillChar(SessionID.Data[0], TLSSessionIDMaxLen, 0);
  if L > 0 then
    Move(A[1], SessionID.Data[0], L);
end;

function EncodeTLSSessionID(var Buffer; const Size: Integer; const SessionID: TTLSSessionID): Integer;
var L : Byte;
    N : Integer;
    P : PByte;
begin
  L := SessionID.Len;
  N := L + 1;
  if Size < N then
    raise ETLSError.CreateAlertBufferEncode;
  P := @Buffer;
  P^ := L;
  Inc(P);
  if L > 0 then
    Move(SessionID.Data[0], P^, L);
  Result := N;
end;

function DecodeTLSSessionID(const Buffer; const Size: Integer; var SessionID: TTLSSessionID): Integer;
var L : Byte;
    P : PByte;
begin
  if Size < 1 then
    raise ETLSError.CreateAlertBufferDecode;
  P := @Buffer;
  L := P^;
  if L = 0 then
    begin
      SessionID.Len := 0;
      Result := 1;
    end
  else
    begin
      if Size < 1 + L then
        raise ETLSError.CreateAlertBufferDecode;
      if L > TLSSessionIDMaxLen then
        raise ETLSError.CreateAlertBufferDecode; // invalid length
      SessionID.Len := L;
      Inc(P);
      Move(P^, SessionID.Data[0], L);
      Result := 1 + L;
    end;
end;



end.
