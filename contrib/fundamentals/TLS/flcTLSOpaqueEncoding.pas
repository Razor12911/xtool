{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals TLS                                         }
{   File name:        flcTLSOpaqueEncoding.pas                                 }
{   File version:     5.02                                                     }
{   Description:      TLS Opaque Encoding                                      }
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
{   2020/05/09  5.02  Create flcTLSOpaqueEncoding unit from flcTLSUtils unit.  }
{                                                                              }
{******************************************************************************}

{$INCLUDE flcTLS.inc}

unit flcTLSOpaqueEncoding;

interface



function EncodeTLSWord16(var Buffer; const Size: Integer; const AVal: Integer): Integer;
function DecodeTLSWord16(const Buffer; const Size: Integer; var AVal: Integer): Integer;

function EncodeTLSLen16(var Buffer; const Size: Integer; const ALen: Integer): Integer;
function EncodeTLSLen24(var Buffer; const Size: Integer; const ALen: Integer): Integer;

function DecodeTLSLen16(const Buffer; const Size: Integer; var ALen: Integer): Integer;
function DecodeTLSLen24(const Buffer; const Size: Integer; var ALen: Integer): Integer;

function EncodeTLSOpaque16(var Buffer; const Size: Integer; const A: RawByteString): Integer;
function EncodeTLSOpaque24(var Buffer; const Size: Integer; const A: RawByteString): Integer;

function DecodeTLSOpaque16(const Buffer; const Size: Integer; var A: RawByteString): Integer;
function DecodeTLSOpaque24(const Buffer; const Size: Integer; var A: RawByteString): Integer;



implementation


uses
  { TLS }

  flcTLSAlert,
  flcTLSErrors;



function EncodeTLSWord16(var Buffer; const Size: Integer; const AVal: Integer): Integer;
var
  P : PByte;
begin
  Assert(AVal >= 0);
  Assert(AVal <= $FFFF);

  if Size < 2 then
    raise ETLSError.CreateAlertBufferEncode;
  P := @Buffer;
  P^ := (AVal and $FF00) shr 8;
  Inc(P);
  P^ := (AVal and $00FF);
  Result := 2;
end;

function DecodeTLSWord16(const Buffer; const Size: Integer; var AVal: Integer): Integer;
var
  P : PByte;
begin
  if Size < 2 then
    raise ETLSError.CreateAlertBufferDecode;
  P := @Buffer;
  AVal := P^ shl 8;
  Inc(P);
  Inc(AVal, P^);
  Result := 2;
end;

function EncodeTLSLen16(var Buffer; const Size: Integer; const ALen: Integer): Integer;
begin
  Result := EncodeTLSWord16(Buffer, Size, ALen);
end;

function EncodeTLSLen24(var Buffer; const Size: Integer; const ALen: Integer): Integer;
var
  P : PByte;
begin
  Assert(ALen >= 0);
  Assert(ALen <= $FFFFFF);

  if Size < 3 then
    raise ETLSError.CreateAlertBufferEncode;
  P := @Buffer;
  P^ := (ALen and $FF0000) shr 16;
  Inc(P);
  P^ := (ALen and $00FF00) shr 8;
  Inc(P);
  P^ := (ALen and $0000FF);
  Result := 3;
end;

function DecodeTLSLen16(const Buffer; const Size: Integer; var ALen: Integer): Integer;
begin
  Result := DecodeTLSWord16(Buffer, Size, ALen);
end;

function DecodeTLSLen24(const Buffer; const Size: Integer; var ALen: Integer): Integer;
var
  P : PByte;
begin
  if Size < 3 then
    raise ETLSError.CreateAlertBufferDecode;
  P := @Buffer;
  ALen := P^ shl 16;
  Inc(P);
  Inc(ALen, P^ shl 8);
  Inc(P);
  Inc(ALen, P^);
  Result := 3;
end;

function EncodeTLSOpaque16(var Buffer; const Size: Integer; const A: RawByteString): Integer;
var
  P : PByte;
  N, L : Integer;
begin
  N := Size;
  P := @Buffer;
  L := Length(A);
  EncodeTLSLen16(P^, N, L);
  Inc(P, 2);
  Dec(N, 2);
  Dec(N, L);
  if N < 0 then
    raise ETLSError.CreateAlertBufferEncode;
  if L > 0 then
    Move(Pointer(A)^, P^, L);
  Result := Size - N;
end;

function EncodeTLSOpaque24(var Buffer; const Size: Integer; const A: RawByteString): Integer;
var
  P : PByte;
  N, L : Integer;
begin
  N := Size;
  P := @Buffer;
  L := Length(A);
  EncodeTLSLen24(P^, N, L);
  Inc(P, 3);
  Dec(N, 3);
  Dec(N, L);
  if N < 0 then
    raise ETLSError.CreateAlertBufferEncode;
  if L > 0 then
    Move(Pointer(A)^, P^, L);
  Result := Size - N;
end;

function DecodeTLSOpaque16(const Buffer; const Size: Integer; var A: RawByteString): Integer;
var
  P : PByte;
  N, L : Integer;
begin
  N := Size;
  P := @Buffer;
  DecodeTLSLen16(P^, N, L);
  Inc(P, 2);
  Dec(N, 2);
  Dec(N, L);
  if N < 0 then
    raise ETLSError.CreateAlertBufferDecode;
  SetLength(A, L);
  if L > 0 then
    Move(P^, Pointer(A)^, L);
  Result := Size - N;
end;

function DecodeTLSOpaque24(const Buffer; const Size: Integer; var A: RawByteString): Integer;
var
  P : PByte;
  N, L : Integer;
begin
  N := Size;
  P := @Buffer;
  DecodeTLSLen24(P^, N, L);
  Inc(P, 3);
  Dec(N, 3);
  Dec(N, L);
  if N < 0 then
    raise ETLSError.CreateAlertBufferDecode;
  SetLength(A, L);
  if L > 0 then
    Move(P^, Pointer(A)^, L);
  Result := Size - N;
end;



end.

