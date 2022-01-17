{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals TLS                                         }
{   File name:        flcTLSProtocolVersion.pas                                }
{   File version:     5.02                                                     }
{   Description:      TLS Protocol Version                                     }
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
{   2020/05/09  5.02  Create flcTLSProtocolVersion unit from flcTLSUtils unit. }
{                                                                              }
{******************************************************************************}

{$INCLUDE flcTLS.inc}

unit flcTLSProtocolVersion;

interface



{                                                                              }
{ ProtocolVersion                                                              }
{                                                                              }
type
  TTLSProtocolVersion = packed record
    major, minor : Byte;
  end;
  PTLSProtocolVersion = ^TTLSProtocolVersion;

const
  TLSProtocolVersionSize = Sizeof(TTLSProtocolVersion);

  SSLProtocolVersion20 : TTLSProtocolVersion = (major: 0; minor: 2);
  SSLProtocolVersion30 : TTLSProtocolVersion = (major: 3; minor: 0);
  TLSProtocolVersion10 : TTLSProtocolVersion = (major: 3; minor: 1);
  TLSProtocolVersion11 : TTLSProtocolVersion = (major: 3; minor: 2);
  TLSProtocolVersion12 : TTLSProtocolVersion = (major: 3; minor: 3);
  TLSProtocolVersion13 : TTLSProtocolVersion = (major: 3; minor: 4);

procedure InitSSLProtocolVersion30(var A: TTLSProtocolVersion);
procedure InitTLSProtocolVersion10(var A: TTLSProtocolVersion);
procedure InitTLSProtocolVersion11(var A: TTLSProtocolVersion);
procedure InitTLSProtocolVersion12(var A: TTLSProtocolVersion);
function  IsTLSProtocolVersion(const A, B: TTLSProtocolVersion): Boolean;
function  IsSSL2(const A: TTLSProtocolVersion): Boolean;
function  IsSSL3(const A: TTLSProtocolVersion): Boolean;
function  IsTLS10(const A: TTLSProtocolVersion): Boolean;
function  IsTLS11(const A: TTLSProtocolVersion): Boolean;
function  IsTLS12(const A: TTLSProtocolVersion): Boolean;
function  IsTLS13(const A: TTLSProtocolVersion): Boolean;
function  IsTLS10OrLater(const A: TTLSProtocolVersion): Boolean;
function  IsTLS11OrLater(const A: TTLSProtocolVersion): Boolean;
function  IsTLS12OrLater(const A: TTLSProtocolVersion): Boolean;
function  IsPostTLS12(const A: TTLSProtocolVersion): Boolean;
function  IsKnownTLSVersion(const A: TTLSProtocolVersion): Boolean;   ////
function  TLSProtocolVersionToStr(const A: TTLSProtocolVersion): String;
function  TLSProtocolVersionName(const A: TTLSProtocolVersion): String;



{                                                                              }
{ Tests                                                                        }
{                                                                              }
{$IFDEF TLS_TEST}
procedure Test;
{$ENDIF}



implementation

uses
  { System }

  SysUtils;



{                                                                              }
{ ProtocolVersion                                                              }
{                                                                              }
procedure InitSSLProtocolVersion30(var A: TTLSProtocolVersion);
begin
  A := SSLProtocolVersion30;
end;

procedure InitTLSProtocolVersion10(var A: TTLSProtocolVersion);
begin
  A := TLSProtocolVersion10;
end;

procedure InitTLSProtocolVersion11(var A: TTLSProtocolVersion);
begin
  A := TLSProtocolVersion11;
end;

procedure InitTLSProtocolVersion12(var A: TTLSProtocolVersion);
begin
  A := TLSProtocolVersion12;
end;

function IsTLSProtocolVersion(const A, B: TTLSProtocolVersion): Boolean;
begin
  Result :=
      (A.major = B.major) and
      (A.minor = B.minor);
end;

function IsSSL2(const A: TTLSProtocolVersion): Boolean;
begin
  Result := IsTLSProtocolVersion(A, SSLProtocolVersion20);
end;

function IsSSL3(const A: TTLSProtocolVersion): Boolean;
begin
  Result := IsTLSProtocolVersion(A, SSLProtocolVersion30);
end;

function IsTLS10(const A: TTLSProtocolVersion): Boolean;
begin
  Result := IsTLSProtocolVersion(A, TLSProtocolVersion10);
end;

function IsTLS11(const A: TTLSProtocolVersion): Boolean;
begin
  Result := IsTLSProtocolVersion(A, TLSProtocolVersion11);
end;

function IsTLS12(const A: TTLSProtocolVersion): Boolean;
begin
  Result := IsTLSProtocolVersion(A, TLSProtocolVersion12);
end;

function IsTLS13(const A: TTLSProtocolVersion): Boolean;
begin
  Result := IsTLSProtocolVersion(A, TLSProtocolVersion13);
end;

function IsTLS10OrLater(const A: TTLSProtocolVersion): Boolean;
begin
  Result :=
      ((A.major =  TLSProtocolVersion10.major) and
       (A.minor >= TLSProtocolVersion10.minor))
      or
       (A.major >  TLSProtocolVersion10.major);
end;

function IsTLS11OrLater(const A: TTLSProtocolVersion): Boolean;
begin
  Result :=
      ((A.major =  TLSProtocolVersion11.major) and
       (A.minor >= TLSProtocolVersion11.minor))
      or
       (A.major >  TLSProtocolVersion11.major);
end;

function IsTLS12OrLater(const A: TTLSProtocolVersion): Boolean;
begin
  Result :=
      ((A.major =  TLSProtocolVersion12.major) and
       (A.minor >= TLSProtocolVersion12.minor))
      or
       (A.major >  TLSProtocolVersion12.major);
end;

function IsPostTLS12(const A: TTLSProtocolVersion): Boolean;
begin
  Result :=
      ((A.major = TLSProtocolVersion12.major) and
       (A.minor > TLSProtocolVersion12.minor))
      or
       (A.major > TLSProtocolVersion12.major);
end;

function IsKnownTLSVersion(const A: TTLSProtocolVersion): Boolean;
begin
  Result := IsTLS12(A) or IsTLS11(A) or IsTLS10(A) or IsSSL3(A);
end;

function TLSProtocolVersionToStr(const A: TTLSProtocolVersion): String;
begin
  Result := IntToStr(A.major) + '.' + IntToStr(A.minor);
end;

function TLSProtocolVersionName(const A: TTLSProtocolVersion): String;
begin
  if IsSSL2(A) then
    Result := 'SSL2' else
  if IsSSL3(A) then
    Result := 'SSL3' else
  if IsTLS10(A) then
    Result := 'TLS1.0' else
  if IsTLS11(A) then
    Result := 'TLS1.1' else
  if IsTLS12(A) then
    Result := 'TLS1.2'
  else
  if IsTLS13(A) then
    Result := 'TLS1.3'
  else
    Result := '[TLS' + TLSProtocolVersionToStr(A) + ']';
end;



{                                                                              }
{ Tests                                                                        }
{                                                                              }
{$IFDEF TLS_TEST}
{$ASSERTIONS ON}
procedure Test;
begin
  Assert(TLSProtocolVersionSize = 2);

  Assert(IsTLS12OrLater(TLSProtocolVersion12));
  Assert(not IsTLS12OrLater(TLSProtocolVersion10));

  Assert(TLSProtocolVersionToStr(TLSProtocolVersion12) = '3.3');

  Assert(TLSProtocolVersionName(SSLProtocolVersion20) = 'SSL2');
  Assert(TLSProtocolVersionName(SSLProtocolVersion30) = 'SSL3');
  Assert(TLSProtocolVersionName(TLSProtocolVersion10) = 'TLS1.0');
  Assert(TLSProtocolVersionName(TLSProtocolVersion11) = 'TLS1.1');
  Assert(TLSProtocolVersionName(TLSProtocolVersion12) = 'TLS1.2');
end;
{$ENDIF}



end.
