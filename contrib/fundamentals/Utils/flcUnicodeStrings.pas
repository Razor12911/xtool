{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals 5.00                                        }
{   File name:        flcUnicodeStrings.pas                                    }
{   File version:     5.02                                                     }
{   Description:      Unicode string utility functions                         }
{                                                                              }
{   Copyright:        Copyright (c) 1999-2020, David J Butler                  }
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
{   2018/08/11  5.01  Split from flcString unit.                               }
{   2018/08/12  5.02  String type changes.                                     }
{                                                                              }
{ Supported compilers:                                                         }
{                                                                              }
{   Delphi 2010-10.4 Win32/Win64        5.02  2020/06/02                       }
{   Delphi 10.2-10.4 Linux64            5.02  2020/06/02                       }
{   FreePascal 3.0.4 Win64              5.02  2020/06/02                       }
{                                                                              }
{******************************************************************************}

{$INCLUDE ..\flcInclude.inc}

unit flcUnicodeStrings;

interface

uses
  flcStdTypes;



function  CharCompareNoUnicodeCaseW(const A, B: WideChar): Integer;
function  StrPCompareNoUnicodeCaseW(const A, B: PWideChar; const Len: Integer): Integer;

function  StrPMatchNoUnicodeCaseW(const A, B: PWideChar; const Len: Integer): Boolean;

function  StrCompareNoUnicodeCaseU(const A, B: UnicodeString): Integer;

function  StrMatchNoUnicodeCaseU(const S, M: UnicodeString; const Index: Integer = 1): Boolean;

function  StrZMatchStrNoUnicodeCaseU(const P: PWideChar; const M: UnicodeString): Boolean;
function  StrZMatchStrUnicodeCaseU(const P: PWideChar; const M: UnicodeString;
          const UnicodeCaseSensitive: Boolean): Boolean;

function  StrEqualNoUnicodeCaseU(const A, B: UnicodeString): Boolean;

function  UnicodeUpperCaseU(const S: UnicodeString): UnicodeString;
function  UnicodeLowerCaseU(const S: UnicodeString): UnicodeString;



implementation

uses
  { Fundamentals }
  flcUnicodeChar;



function CharCompareNoUnicodeCaseW(const A, B: WideChar): Integer;
var C, D : WideChar;
begin
  C := UnicodeUpCase(A);
  D := UnicodeUpCase(B);
  if Ord(C) < Ord(D) then
    Result := -1 else
    if Ord(C) > Ord(D) then
      Result := 1
    else
      Result := 0;
end;

function StrPCompareNoUnicodeCaseW(const A, B: PWideChar; const Len: Integer): Integer;
var P, Q : PWideChar;
    C, D : WideChar;
    I    : Integer;
begin
  P := A;
  Q := B;
  if P <> Q then
    for I := 1 to Len do
      begin
        C := UnicodeUpCase(P^);
        D := UnicodeUpCase(Q^);
        if C = D then
          begin
            Inc(P);
            Inc(Q);
          end
        else
          begin
            if C < D then
              Result := -1
            else
              Result := 1;
            exit;
          end;
      end;
  Result := 0;
end;

function StrPMatchNoUnicodeCaseW(const A, B: PWideChar; const Len: Integer): Boolean;
var P, Q : PWideChar;
    I    : Integer;
begin
  P := A;
  Q := B;
  if P <> Q then
    for I := 1 to Len do
      begin
        if UnicodeCharIsEqualNoCase(P^, Q^) then
          begin
            Inc(P);
            Inc(Q);
          end
        else
          begin
            Result := False;
            exit;
          end;
      end;
  Result := True;
end;

function StrCompareNoUnicodeCaseU(const A, B: UnicodeString): Integer;
var L, M, I: Integer;
begin
  L := Length(A);
  M := Length(B);
  if L < M then
    I := L
  else
    I := M;
  Result := StrPCompareNoUnicodeCaseW(Pointer(A), Pointer(B), I);
  if Result <> 0 then
    exit;
  if L = M then
    Result := 0 else
  if L < M then
    Result := -1
  else
    Result := 1;
end;

function StrMatchNoUnicodeCaseU(const S, M: UnicodeString; const Index: Integer): Boolean;
var N, T : Integer;
    Q    : PWideChar;
begin
  N := Length(M);
  T := Length(S);
  if (N = 0) or (T = 0) or (Index < 1) or (Index + N - 1 > T) then
    begin
      Result := False;
      exit;
    end;
  Q := Pointer(S);
  Inc(Q, Index - 1);
  Result := StrPMatchNoUnicodeCaseW(Pointer(M), Q, N);
end;

function StrZMatchStrNoUnicodeCaseU(const P: PWideChar; const M: UnicodeString): Boolean;
var T, Q : PWideChar;
    I, L : Integer;
    C, D : WideChar;
begin
  L := Length(M);
  if L = 0 then
    begin
      Result := False;
      exit;
    end;
  T := P;
  Q := Pointer(M);
  for I := 1 to L do
    begin
      C := T^;
      if C = #0 then
        begin
          Result := False;
          exit;
        end;
      D := Q^;
      if not UnicodeCharIsEqualNoCase(C, D) then
        begin
          Result := False;
          exit;
        end;
      Inc(T);
      Inc(Q);
    end;
  Result := True;
end;

function StrZMatchStrU(const P: PWideChar; const M: UnicodeString): Boolean;
var T, Q : PWideChar;
    I, L : Integer;
    C    : WideChar;
begin
  L := Length(M);
  if L = 0 then
    begin
      Result := False;
      exit;
    end;
  T := P;
  Q := Pointer(M);
  for I := 1 to L do
    begin
      C := T^;
      if (C = #0) or (C <> Q^) then
        begin
          Result := False;
          exit;
        end else
        begin
          Inc(T);
          Inc(Q);
        end;
    end;
  Result := True;
end;

function StrZMatchStrUnicodeCaseU(const P: PWideChar; const M: UnicodeString; const UnicodeCaseSensitive: Boolean): Boolean;
begin
  if UnicodeCaseSensitive then
    Result := StrZMatchStrU(P, M)
  else
    Result := StrZMatchStrNoUnicodeCaseU(P, M);
end;

function StrEqualNoUnicodeCaseU(const A, B: UnicodeString): Boolean;
var L, M : Integer;
begin
  L := Length(A);
  M := Length(B);
  Result := L = M;
  if not Result or (L = 0) then
    exit;
  Result := StrPMatchNoUnicodeCaseW(Pointer(A), Pointer(B), L);
end;

function UnicodeUpperCaseU(const S: UnicodeString): UnicodeString;
var L, I : Integer;
begin
  L := Length(S);
  SetLength(Result, L);
  for I := 1 to L do
    Result[I] := UnicodeUpCase(S[I]);
end;

function UnicodeLowerCaseU(const S: UnicodeString): UnicodeString;
var L, I : Integer;
begin
  L := Length(S);
  SetLength(Result, L);
  for I := 1 to L do
    Result[I] := UnicodeLowCase(S[I]);
end;



end.

