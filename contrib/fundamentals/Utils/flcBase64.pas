{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals 5.00                                        }
{   File name:        flcBase64.pas                                            }
{   File version:     5.03                                                     }
{   Description:      Base64 encoding/decoding                                 }
{                                                                              }
{   Copyright:        Copyright (c) 2002-2020, David J Butler                  }
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
{   2002/01/03  2.01  Added EncodeBase64, DecodeBase64 from cMaths and         }
{                     optimized.                                               }
{   2002/03/30  2.02  Fixed bug in DecodeBase64.                               }
{   2017/10/07  5.03  Moved from flcUtils.                                     }
{                                                                              }
{ Supported compilers:                                                         }
{                                                                              }
{   Delphi 2010-10.4 Win32/Win64        5.03  2020/06/02                       }
{   Delphi 10.2-10.4 Linux64            5.03  2020/06/02                       }
{   FreePascal 3.0.4 Win64              5.03  2020/06/02                       }
{                                                                              }
{******************************************************************************}

{$INCLUDE ..\flcInclude.inc}

{$IFDEF FREEPASCAL}
  {$WARNINGS OFF}
  {$HINTS OFF}
{$ENDIF}

{$IFDEF DEBUG}
{$IFDEF TEST}
  {$DEFINE BASE64_TEST}
{$ENDIF}
{$ENDIF}

unit flcBase64;

interface

uses
  { Fundamentals }
  flcStdTypes;



{                                                                              }
{ Base64                                                                       }
{                                                                              }
{   EncodeBase64 converts a binary string (S) to a base 64 string using        }
{   Alphabet. if Pad is True, the result will be padded with PadChar to be a   }
{   multiple of PadMultiple.                                                   }
{                                                                              }
{   DecodeBase64 converts a base 64 string using Alphabet (64 characters for   }
{   values 0-63) to a binary string.                                           }
{                                                                              }
function  EncodeBase64(const S, Alphabet: RawByteString;
          const Pad: Boolean = False;
          const PadMultiple: Integer = 4;
          const PadChar: AnsiChar = AnsiChar(Ord('='))): RawByteString;

function  DecodeBase64(const S, Alphabet: RawByteString;
          const PadSet: ByteCharSet = []): RawByteString;

const
  b64_MIMEBase64 : RawByteString = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
  b64_UUEncode   : RawByteString = ' !"#$%&''()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_';
  b64_XXEncode   : RawByteString = '+-0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz';

function  MIMEBase64Decode(const S: RawByteString): RawByteString;
function  MIMEBase64Encode(const S: RawByteString): RawByteString;
function  UUDecode(const S: RawByteString): RawByteString;
function  XXDecode(const S: RawByteString): RawByteString;



implementation



{                                                                              }
{ Base64                                                                       }
{                                                                              }
function EncodeBase64(const S, Alphabet: RawByteString; const Pad: Boolean;
    const PadMultiple: Integer; const PadChar: AnsiChar): RawByteString;
var
  R, C : Byte;
  F, L, M, N, U : Integer;
  P : PByteChar;
  T : Boolean;
begin
  Assert(Length(Alphabet) = 64);
  {$IFOPT R+}
  if Length(Alphabet) <> 64 then
    begin
      Result := '';
      exit;
    end;
  {$ENDIF}
  L := Length(S);
  if L = 0 then
    begin
      Result := '';
      exit;
    end;
  M := L mod 3;
  N := (L div 3) * 4 + M;
  if M > 0 then
    Inc(N);
  T := Pad and (PadMultiple > 1);
  if T then
    begin
      U := N mod PadMultiple;
      if U > 0 then
        begin
          U := PadMultiple - U;
          Inc(N, U);
        end;
    end else
    U := 0;
  SetLength(Result, N);
  P := Pointer(Result);
  R := 0;
  for F := 0 to L - 1 do
    begin
      C := Byte(S [F + 1]);
      case F mod 3 of
        0 : begin
              P^ := Alphabet[C shr 2 + 1];
              Inc(P);
              R := (C and 3) shl 4;
            end;
        1 : begin
              P^ := Alphabet[C shr 4 + R + 1];
              Inc(P);
              R := (C and $0F) shl 2;
            end;
        2 : begin
              P^ := Alphabet[C shr 6 + R + 1];
              Inc(P);
              P^ := Alphabet[C and $3F + 1];
              Inc(P);
            end;
      end;
    end;
  if M > 0 then
    begin
      P^ := Alphabet[R + 1];
      Inc(P);
    end;
  for F := 1 to U do
    begin
      P^ := PadChar;
      Inc(P);
    end;
end;

function DecodeBase64(const S, Alphabet: RawByteString; const PadSet: ByteCharSet): RawByteString;
var
  F, L, M, P : Integer;
  B, OutPos  : Byte;
  OutB       : array[1..3] of Byte;
  Lookup     : array[AnsiChar] of Byte;
  R          : PByteChar;
begin
  Assert(Length(Alphabet) = 64);
  {$IFOPT R+}
  if Length(Alphabet) <> 64 then
    begin
      Result := '';
      exit;
    end;
  {$ENDIF}
  L := Length(S);
  P := 0;
  if PadSet <> [] then
    while (L - P > 0) and (S[L - P] in PadSet) do
      Inc(P);
  M := L - P;
  if M = 0 then
    begin
      Result := '';
      exit;
    end;
  SetLength(Result, (M * 3) div 4);
  FillChar(Lookup, Sizeof(Lookup), #0);
  for F := 0 to 63 do
    Lookup[Alphabet[F + 1]] := Byte(F);
  R := Pointer(Result);
  OutPos := 0;
  for F := 1 to L - P do
    begin
      B := Lookup[S[F]];
      case OutPos of
          0 : OutB[1] := B shl 2;
          1 : begin
                OutB[1] := OutB[1] or (B shr 4);
                R^ := AnsiChar(OutB[1]);
                Inc(R);
                OutB[2] := (B shl 4) and $FF;
              end;
          2 : begin
                OutB[2] := OutB[2] or (B shr 2);
                R^ := AnsiChar(OutB[2]);
                Inc(R);
                OutB[3] := (B shl 6) and $FF;
              end;
          3 : begin
                OutB[3] := OutB[3] or B;
                R^ := AnsiChar(OutB[3]);
                Inc(R);
              end;
        end;
      OutPos := (OutPos + 1) mod 4;
    end;
  if (OutPos > 0) and (P = 0) then // incomplete encoding, add the partial byte if not 0
    if OutB[OutPos] <> 0 then
      Result := Result + ByteChar(OutB[OutPos]);
end;

function MIMEBase64Encode(const S: RawByteString): RawByteString;
begin
  Result := EncodeBase64(S, b64_MIMEBase64, True, 4, AnsiChar(Ord('=')));
end;

function UUDecode(const S: RawByteString): RawByteString;
begin
  // Line without size indicator (first byte = length + 32)
  Result := DecodeBase64(S, b64_UUEncode, [AnsiChar(Ord('`'))]);
end;

function MIMEBase64Decode(const S: RawByteString): RawByteString;
begin
  Result := DecodeBase64(S, b64_MIMEBase64, [AnsiChar(Ord('='))]);
end;

function XXDecode(const S: RawByteString): RawByteString;
begin
  Result := DecodeBase64(S, b64_XXEncode, []);
end;



end.

