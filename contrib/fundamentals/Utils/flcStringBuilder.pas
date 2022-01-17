{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals 5.00                                        }
{   File name:        flcStringBuilder.pas                                     }
{   File version:     5.04                                                     }
{   Description:      String builder classes                                   }
{                                                                              }
{   Copyright:        Copyright (c) 2005-2020, David J Butler                  }
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
{   2005/09/20  4.01  Added TStringBuilder class.                              }
{   2014/08/26  4.02  StringBuilder unit tests.                                }
{   2017/10/07  5.03  Split from flcStrings unit.                              }
{   2019/03/15  5.04  Removed dependency on unit flcAscii.                     }
{                                                                              }
{ Supported compilers:                                                         }
{                                                                              }
{   Delphi 2010-10.4 Win32/Win64        5.04  2020/06/02                       }
{   Delphi 10.2-10.4 Linux64            5.04  2020/06/02                       }
{   FreePascal 3.0.4 Win64              5.04  2020/06/02                       }
{                                                                              }
{******************************************************************************}

{$INCLUDE ..\flcInclude.inc}

{$IFDEF FREEPASCAL}
  {$WARNINGS OFF}
  {$HINTS OFF}
{$ENDIF}

{$IFDEF DEBUG}
{$IFDEF TEST}
  {$DEFINE STRINGBUILDER_TEST}
{$ENDIF}
{$ENDIF}

unit flcStringBuilder;

interface

uses
  { Fundamentals }
  flcStdTypes;



{                                                                              }
{ String Builder                                                               }
{                                                                              }
{   Class to help construct a string.                                          }
{   The String Builder class is used to efficiently construct a long string    }
{   from multiple shorter strings.                                             }
{                                                                              }
type
  {$IFDEF SupportAnsiString}
  TAnsiStringBuilder = class
  protected
    FString : AnsiString;
    FLength : Integer;

    procedure EnsureCapacity(const L: Integer);

    function  GetAsAnsiString: AnsiString;
    procedure SetAsAnsiString(const S: AnsiString);
    function  GetAsString: String;

  public
    constructor Create(const S: AnsiString = ''); overload;
    constructor Create(const Capacity: Integer); overload;

    property  Length: Integer read FLength;
    property  AsAnsiString: AnsiString read GetAsAnsiString write SetAsAnsiString;
    property  AsString: String read GetAsString;

    procedure Clear;
    procedure Assign(const S: TAnsiStringBuilder);

    procedure Append(const S: AnsiString); overload;
    procedure AppendCRLF;
    procedure AppendLn(const S: AnsiString = '');
    procedure Append(const S: AnsiString; const Count: Integer); overload;
    procedure AppendCh(const C: AnsiChar); overload;
    procedure AppendCh(const C: AnsiChar; const Count: Integer); overload;
    procedure Append(const BufPtr: Pointer; const Size: Integer); overload;
    procedure Append(const S: TAnsiStringBuilder); overload;

    procedure Pack;
  end;
  {$ENDIF}

  TRawByteStringBuilder = class
  protected
    FString : RawByteString;
    FLength : Integer;

    procedure EnsureCapacity(const L: Integer);

    function  GetAsRawByteString: RawByteString;
    procedure SetAsRawByteString(const S: RawByteString);
    function  GetAsString: String;

  public
    constructor Create(const S: RawByteString = ''); overload;
    constructor Create(const Capacity: Integer); overload;

    property  Length: Integer read FLength;
    property  AsRawByteString: RawByteString read GetAsRawByteString write SetAsRawByteString;
    property  AsString: String read GetAsString;

    procedure Clear;
    procedure Assign(const S: TRawByteStringBuilder);

    procedure Append(const S: RawByteString); overload;
    procedure AppendCRLF;
    procedure AppendLn(const S: RawByteString = '');
    procedure Append(const S: RawByteString; const Count: Integer); overload;
    procedure AppendCh(const C: AnsiChar); overload;
    procedure AppendCh(const C: AnsiChar; const Count: Integer); overload;
    procedure Append(const BufPtr: Pointer; const Size: Integer); overload;
    procedure Append(const S: TRawByteStringBuilder); overload;

    procedure Pack;
  end;

  {$IFDEF SupportUnicodeString}
  TUnicodeStringBuilder = class
  protected
    FString : UnicodeString;
    FLength : Integer;

    procedure EnsureCapacity(const L: Integer);
    function  GetAsUnicodeString: UnicodeString;
    procedure SetAsUnicodeString(const S: UnicodeString);

  public
    constructor Create(const S: UnicodeString = ''); overload;
    constructor Create(const Capacity: Integer); overload;

    property  Length: Integer read FLength;
    property  AsUnicodeString: UnicodeString read GetAsUnicodeString write SetAsUnicodeString;

    procedure Clear;
    procedure Assign(const S: TUnicodeStringBuilder);

    procedure Append(const S: UnicodeString); overload;
    procedure AppendCRLF;
    procedure AppendLn(const S: UnicodeString = '');
    procedure Append(const S: UnicodeString; const Count: Integer); overload;
    procedure AppendCh(const C: WideChar); overload;
    procedure AppendCh(const C: WideChar; const Count: Integer); overload;
    procedure Append(const S: TUnicodeStringBuilder); overload;

    procedure Pack;
  end;
  {$ENDIF}

  TStringBuilder = class
  protected
    FString : String;
    FLength : Integer;

    procedure EnsureCapacity(const L: Integer);
    function  GetAsString: String;
    procedure SetAsString(const S: String);

  public
    constructor Create(const S: String = ''); overload;
    constructor Create(const Capacity: Integer); overload;

    property  Length: Integer read FLength;
    property  AsString: String read GetAsString write SetAsString;

    procedure Clear;
    procedure Assign(const S: TStringBuilder);

    procedure Append(const S: String); overload;
    procedure AppendCRLF;
    procedure AppendLn(const S: String = '');
    procedure Append(const S: String; const Count: Integer); overload;
    procedure AppendCh(const C: Char); overload;
    procedure AppendCh(const C: Char; const Count: Integer); overload;
    procedure Append(const S: TStringBuilder); overload;

    procedure Pack;
  end;



{                                                                              }
{ Tests                                                                        }
{                                                                              }
procedure Test;



implementation



{                                                                              }
{ String Builder                                                               }
{                                                                              }
function StringBuilderNewCapacity(const L, N: Integer): Integer; {$IFDEF UseInline}inline;{$ENDIF}
begin
  // memory allocation strategy
  if N = 0 then        // first allocation is exactly as requested
    Result := L else
  if L < 16 then       // if grow to < 16 then allocate 16
    Result := 16
  else                 // if grow to >= 16 then pre-allocate 1/4
    Result := L + (L shr 2);
end;



{                                                                              }
{ TAnsiStringBuilder                                                           }
{                                                                              }
{$IFDEF SupportAnsiString}
constructor TAnsiStringBuilder.Create(const S: AnsiString);
begin
  inherited Create;
  SetAsAnsiString(S);
end;

constructor TAnsiStringBuilder.Create(const Capacity: Integer);
begin
  inherited Create;
  EnsureCapacity(Capacity);
end;

procedure TAnsiStringBuilder.EnsureCapacity(const L: Integer);
var N : Integer;
begin
  N := System.Length(FString);
  if L > N then
    begin
      N := StringBuilderNewCapacity(L, N);
      SetLength(FString, N);
    end;
end;

function TAnsiStringBuilder.GetAsAnsiString: AnsiString;
begin
  if FLength = System.Length(FString) then
    Result := FString // return reference instead of copy
  else
    Result := Copy(FString, 1, FLength);
end;

procedure TAnsiStringBuilder.SetAsAnsiString(const S: AnsiString);
begin
  FString := S;
  FLength := System.Length(S);
end;

function TAnsiStringBuilder.GetAsString: String;
begin
  {$IFDEF StringIsUnicode}
  Result := String(GetAsAnsiString);
  {$ELSE}
  Result := GetAsAnsiString;
  {$ENDIF}
end;

procedure TAnsiStringBuilder.Clear;
begin
  FString := '';
  FLength := 0;
end;

procedure TAnsiStringBuilder.Assign(const S: TAnsiStringBuilder);
var L : Integer;
begin
  L := S.FLength;
  FString := Copy(S.FString, 1, L);
  FLength := L;
end;

procedure TAnsiStringBuilder.Append(const S: AnsiString);
var M, L, N : Integer;
    P       : PAnsiChar;
begin
  M := System.Length(S);
  if M = 0 then
    exit;
  N := FLength;
  L := N + M;
  if L > System.Length(FString) then
    EnsureCapacity(L);
  P := Pointer(FString);
  Inc(P, N);
  Move(Pointer(S)^, P^, M);
  FLength := L;
end;

const
  AnsiStringCRLF = AnsiString(#13#10);

procedure TAnsiStringBuilder.AppendCRLF;
begin
  Append(AnsiStringCRLF);
end;

procedure TAnsiStringBuilder.AppendLn(const S: AnsiString);
begin
  Append(S);
  AppendCRLF;
end;

procedure TAnsiStringBuilder.Append(const S: AnsiString; const Count: Integer);
var M, L, N, I : Integer;
    P          : PAnsiChar;
begin
  if Count <= 0 then
    exit;
  M := System.Length(S);
  if M = 0 then
    exit;
  N := FLength;
  L := N + (M * Count);
  if L > System.Length(FString) then
    EnsureCapacity(L);
  P := Pointer(FString);
  Inc(P, N);
  for I := 1 to Count do
    begin
      Move(Pointer(S)^, P^, M);
      Inc(P, M);
    end;
  FLength := L;
end;

procedure TAnsiStringBuilder.AppendCh(const C: AnsiChar);
var L, N : Integer;
    P    : PAnsiChar;
begin
  N := FLength;
  L := N + 1;
  if L > System.Length(FString) then
    EnsureCapacity(L);
  P := Pointer(FString);
  Inc(P, N);
  P^ := C;
  FLength := L;
end;

procedure TAnsiStringBuilder.AppendCh(const C: AnsiChar; const Count: Integer);
var L, N : Integer;
    P    : PAnsiChar;
begin
  if Count <= 0 then
    exit;
  N := FLength;
  L := N + Count;
  if L > System.Length(FString) then
    EnsureCapacity(L);
  P := Pointer(FString);
  Inc(P, N);
  FillChar(P^, Count, Ord(C));
  FLength := L;
end;

procedure TAnsiStringBuilder.Append(const BufPtr: Pointer; const Size: Integer);
var L, N : Integer;
    P    : PAnsiChar;
begin
  if Size <= 0 then
    exit;
  N := FLength;
  L := N + Size;
  if L > System.Length(FString) then
    EnsureCapacity(L);
  P := Pointer(FString);
  Inc(P, N);
  Move(BufPtr^, P^, Size);
  FLength := L;
end;

procedure TAnsiStringBuilder.Append(const S: TAnsiStringBuilder);
var M, L, N : Integer;
    P       : PAnsiChar;
begin
  M := S.FLength;
  if M = 0 then
    exit;
  N := FLength;
  L := N + M;
  if L > System.Length(FString) then
    EnsureCapacity(L);
  P := Pointer(FString);
  Inc(P, N);
  Move(Pointer(S.FString)^, P^, M);
  FLength := L;
end;

procedure TAnsiStringBuilder.Pack;
var L : Integer;
begin
  L := FLength;
  if L = System.Length(FString) then
    exit;
  SetLength(FString, L);
end;
{$ENDIF}



{                                                                              }
{ TRawByteStringBuilder                                                        }
{                                                                              }
constructor TRawByteStringBuilder.Create(const S: RawByteString);
begin
  inherited Create;
  SetAsRawByteString(S);
end;

constructor TRawByteStringBuilder.Create(const Capacity: Integer);
begin
  inherited Create;
  EnsureCapacity(Capacity);
end;

procedure TRawByteStringBuilder.EnsureCapacity(const L: Integer);
var N : Integer;
begin
  N := System.Length(FString);
  if L > N then
    begin
      N := StringBuilderNewCapacity(L, N);
      SetLength(FString, N);
    end;
end;

function TRawByteStringBuilder.GetAsRawByteString: RawByteString;
begin
  if FLength = System.Length(FString) then
    Result := FString // return reference instead of copy
  else
    Result := Copy(FString, 1, FLength);
end;

procedure TRawByteStringBuilder.SetAsRawByteString(const S: RawByteString);
begin
  FString := S;
  FLength := System.Length(S);
end;

function TRawByteStringBuilder.GetAsString: String;
begin
  {$IFDEF StringIsUnicode}
  Result := String(GetAsRawByteString);
  {$ELSE}
  Result := GetAsRawByteString;
  {$ENDIF}
end;

procedure TRawByteStringBuilder.Clear;
begin
  FString := '';
  FLength := 0;
end;

procedure TRawByteStringBuilder.Assign(const S: TRawByteStringBuilder);
var L : Integer;
begin
  L := S.FLength;
  FString := Copy(S.FString, 1, L);
  FLength := L;
end;

procedure TRawByteStringBuilder.Append(const S: RawByteString);
var M, L, N : Integer;
    P       : PByte;
begin
  M := System.Length(S);
  if M = 0 then
    exit;
  N := FLength;
  L := N + M;
  if L > System.Length(FString) then
    EnsureCapacity(L);
  P := Pointer(FString);
  Inc(P, N);
  Move(Pointer(S)^, P^, M);
  FLength := L;
end;

const
  RawByteStringCRLF = RawByteString(#13#10);

procedure TRawByteStringBuilder.AppendCRLF;
begin
  Append(RawByteStringCRLF);
end;

procedure TRawByteStringBuilder.AppendLn(const S: RawByteString);
begin
  Append(S);
  AppendCRLF;
end;

procedure TRawByteStringBuilder.Append(const S: RawByteString; const Count: Integer);
var M, L, N, I : Integer;
    P          : PByte;
begin
  if Count <= 0 then
    exit;
  M := System.Length(S);
  if M = 0 then
    exit;
  N := FLength;
  L := N + (M * Count);
  if L > System.Length(FString) then
    EnsureCapacity(L);
  P := Pointer(FString);
  Inc(P, N);
  for I := 1 to Count do
    begin
      Move(Pointer(S)^, P^, M);
      Inc(P, M);
    end;
  FLength := L;
end;

procedure TRawByteStringBuilder.AppendCh(const C: AnsiChar);
var L, N : Integer;
    P    : PByte;
begin
  N := FLength;
  L := N + 1;
  if L > System.Length(FString) then
    EnsureCapacity(L);
  P := Pointer(FString);
  Inc(P, N);
  P^ := Ord(C);
  FLength := L;
end;

procedure TRawByteStringBuilder.AppendCh(const C: AnsiChar; const Count: Integer);
var L, N : Integer;
    P    : PByte;
begin
  if Count <= 0 then
    exit;
  N := FLength;
  L := N + Count;
  if L > System.Length(FString) then
    EnsureCapacity(L);
  P := Pointer(FString);
  Inc(P, N);
  FillChar(P^, Count, Ord(C));
  FLength := L;
end;

procedure TRawByteStringBuilder.Append(const BufPtr: Pointer; const Size: Integer);
var L, N : Integer;
    P    : PByte;
begin
  if Size <= 0 then
    exit;
  N := FLength;
  L := N + Size;
  if L > System.Length(FString) then
    EnsureCapacity(L);
  P := Pointer(FString);
  Inc(P, N);
  Move(BufPtr^, P^, Size);
  FLength := L;
end;

procedure TRawByteStringBuilder.Append(const S: TRawByteStringBuilder);
var M, L, N : Integer;
    P       : PByte;
begin
  M := S.FLength;
  if M = 0 then
    exit;
  N := FLength;
  L := N + M;
  if L > System.Length(FString) then
    EnsureCapacity(L);
  P := Pointer(FString);
  Inc(P, N);
  Move(Pointer(S.FString)^, P^, M);
  FLength := L;
end;

procedure TRawByteStringBuilder.Pack;
var L : Integer;
begin
  L := FLength;
  if L = System.Length(FString) then
    exit;
  SetLength(FString, L);
end;



{$IFDEF SupportUnicodeString}
{                                                                              }
{ TUnicodeStringBuilder                                                        }
{                                                                              }
constructor TUnicodeStringBuilder.Create(const S: UnicodeString);
begin
  inherited Create;
  SetAsUnicodeString(S);
end;

constructor TUnicodeStringBuilder.Create(const Capacity: Integer);
begin
  inherited Create;
  EnsureCapacity(Capacity);
end;

procedure TUnicodeStringBuilder.EnsureCapacity(const L: Integer);
var N : Integer;
begin
  N := System.Length(FString);
  if L > N then
    begin
      N := StringBuilderNewCapacity(L, N);
      SetLength(FString, N);
    end;
end;

function TUnicodeStringBuilder.GetAsUnicodeString: UnicodeString;
begin
  if FLength = System.Length(FString) then
    Result := FString
  else
    Result := Copy(FString, 1, FLength);
end;

procedure TUnicodeStringBuilder.SetAsUnicodeString(const S: UnicodeString);
begin
  FString := S;
  FLength := System.Length(S);
end;

procedure TUnicodeStringBuilder.Clear;
begin
  FString := '';
  FLength := 0;
end;

procedure TUnicodeStringBuilder.Assign(const S: TUnicodeStringBuilder);
var L : Integer;
begin
  L := S.FLength;
  FString := Copy(S.FString, 1, L);
  FLength := L;
end;

procedure TUnicodeStringBuilder.Append(const S: UnicodeString);
var M, L, N : Integer;
    P       : PWideChar;
begin
  M := System.Length(S);
  if M = 0 then
    exit;
  N := FLength;
  L := N + M;
  if L > System.Length(FString) then
    EnsureCapacity(L);
  P := Pointer(FString);
  Inc(P, N);
  Move(Pointer(S)^, P^, M * SizeOf(WideChar));
  FLength := L;
end;

const
  UnicodeStringCRLF = UnicodeString(WideChar(#13) + WideChar(#10));

procedure TUnicodeStringBuilder.AppendCRLF;
begin
  Append(WideChar(#13) + WideChar(#10));
end;

procedure TUnicodeStringBuilder.AppendLn(const S: UnicodeString);
begin
  Append(S);
  AppendCRLF;
end;

procedure TUnicodeStringBuilder.Append(const S: UnicodeString; const Count: Integer);
var M, L, N, I : Integer;
    P          : PWideChar;
begin
  if Count <= 0 then
    exit;
  M := System.Length(S);
  if M = 0 then
    exit;
  N := FLength;
  L := N + (M * Count);
  if L > System.Length(FString) then
    EnsureCapacity(L);
  P := Pointer(FString);
  Inc(P, N);
  for I := 1 to Count do
    begin
      Move(Pointer(S)^, P^, M * SizeOf(WideChar));
      Inc(P, M);
    end;
  FLength := L;
end;

procedure TUnicodeStringBuilder.AppendCh(const C: WideChar);
var L, N : Integer;
    P    : PWideChar;
begin
  N := FLength;
  L := N + 1;
  if L > System.Length(FString) then
    EnsureCapacity(L);
  P := Pointer(FString);
  Inc(P, N);
  P^ := C;
  FLength := L;
end;

procedure TUnicodeStringBuilder.AppendCh(const C: WideChar; const Count: Integer);
var L, N, I : Integer;
    P       : PWideChar;
begin
  if Count <= 0 then
    exit;
  N := FLength;
  L := N + Count;
  if L > System.Length(FString) then
    EnsureCapacity(L);
  P := Pointer(FString);
  Inc(P, N);
  for I := 0 to Count - 1 do
    begin
      P^ := C;
      Inc(P);
    end;
  FLength := L;
end;

procedure TUnicodeStringBuilder.Append(const S: TUnicodeStringBuilder);
var M, L, N : Integer;
    P       : PWideChar;
begin
  M := S.FLength;
  if M = 0 then
    exit;
  N := FLength;
  L := N + M;
  if L > System.Length(FString) then
    EnsureCapacity(L);
  P := Pointer(FString);
  Inc(P, N);
  Move(Pointer(S.FString)^, P^, M * SizeOf(WideChar));
  FLength := L;
end;

procedure TUnicodeStringBuilder.Pack;
var L : Integer;
begin
  L := FLength;
  if L = System.Length(FString) then
    exit;
  SetLength(FString, L);
end;
{$ENDIF}



{                                                                              }
{ TStringBuilder                                                               }
{                                                                              }
constructor TStringBuilder.Create(const S: String);
begin
  inherited Create;
  SetAsString(S);
end;

constructor TStringBuilder.Create(const Capacity: Integer);
begin
  inherited Create;
  EnsureCapacity(Capacity);
end;

procedure TStringBuilder.EnsureCapacity(const L: Integer);
var N : Integer;
begin
  N := System.Length(FString);
  if L > N then
    begin
      N := StringBuilderNewCapacity(L, N);
      SetLength(FString, N);
    end;
end;

function TStringBuilder.GetAsString: String;
begin
  if FLength = System.Length(FString) then
    Result := FString // return reference instead of copy
  else
    Result := Copy(FString, 1, FLength);
end;

procedure TStringBuilder.SetAsString(const S: String);
begin
  FString := S;
  FLength := System.Length(S);
end;

procedure TStringBuilder.Clear;
begin
  FString := '';
  FLength := 0;
end;

procedure TStringBuilder.Assign(const S: TStringBuilder);
var L : Integer;
begin
  L := S.FLength;
  FString := Copy(S.FString, 1, L);
  FLength := L;
end;

procedure TStringBuilder.Append(const S: String);
var M, L, N : Integer;
    P       : PChar;
begin
  M := System.Length(S);
  if M = 0 then
    exit;
  N := FLength;
  L := N + M;
  if L > System.Length(FString) then
    EnsureCapacity(L);
  P := Pointer(FString);
  Inc(P, N);
  Move(Pointer(S)^, P^, M * SizeOf(Char));
  FLength := L;
end;

const
  StringCRLF = String(#13#10);

procedure TStringBuilder.AppendCRLF;
begin
  Append(StringCRLF);
end;

procedure TStringBuilder.AppendLn(const S: String);
begin
  Append(S);
  AppendCRLF;
end;

procedure TStringBuilder.Append(const S: String; const Count: Integer);
var M, L, N, I : Integer;
    P          : PChar;
begin
  if Count <= 0 then
    exit;
  M := System.Length(S);
  if M = 0 then
    exit;
  N := FLength;
  L := N + (M * Count);
  if L > System.Length(FString) then
    EnsureCapacity(L);
  P := Pointer(FString);
  Inc(P, N);
  for I := 1 to Count do
    begin
      Move(Pointer(S)^, P^, M * SizeOf(Char));
      Inc(P, M);
    end;
  FLength := L;
end;

procedure TStringBuilder.AppendCh(const C: Char);
var L, N : Integer;
    P    : PChar;
begin
  N := FLength;
  L := N + 1;
  if L > System.Length(FString) then
    EnsureCapacity(L);
  P := Pointer(FString);
  Inc(P, N);
  P^ := C;
  FLength := L;
end;

procedure TStringBuilder.AppendCh(const C: Char; const Count: Integer);
var L, N, I : Integer;
    P       : PChar;
begin
  if Count <= 0 then
    exit;
  N := FLength;
  L := N + Count;
  if L > System.Length(FString) then
    EnsureCapacity(L);
  P := Pointer(FString);
  Inc(P, N);
  for I := 1 to Count do
    begin
      P^ := C;
      Inc(P);
    end;
  FLength := L;
end;

procedure TStringBuilder.Append(const S: TStringBuilder);
var M, L, N : Integer;
    P       : PChar;
begin
  M := S.FLength;
  if M = 0 then
    exit;
  N := FLength;
  L := N + M;
  if L > System.Length(FString) then
    EnsureCapacity(L);
  P := Pointer(FString);
  Inc(P, N);
  Move(Pointer(S.FString)^, P^, M * SizeOf(Char));
  FLength := L;
end;

procedure TStringBuilder.Pack;
var L : Integer;
begin
  L := FLength;
  if L = System.Length(FString) then
    exit;
  SetLength(FString, L);
end;



{                                                                              }
{ Tests                                                                        }
{                                                                              }
{$IFDEF SupportAnsiString}
procedure Test_AnsiStringBuilder;
var A : TAnsiStringBuilder;
begin
  A := TAnsiStringBuilder.Create;
  try
    Assert(A.Length = 0);
    A.Append('X');
    Assert(A.Length = 1);
    A.Append('ABC');
    Assert(A.Length = 4);
    Assert(A.GetAsAnsiString = 'XABC');
    A.AppendCRLF;
    Assert(A.Length = 6);
    A.AppendCh('D');
    Assert(A.Length = 7);
    A.AppendCh('E', 3);
    Assert(A.Length = 10);
    Assert(A.GetAsAnsiString = 'XABC'#13#10'DEEE');
    A.Pack;
    Assert(A.Length = 10);
    A.Clear;
    Assert(A.Length = 0);
  finally
    A.Free;
  end;
end;
{$ENDIF}

procedure Test_RawByteStringBuilder;
var A : TRawByteStringBuilder;
begin
  A := TRawByteStringBuilder.Create;
  try
    Assert(A.Length = 0);
    A.Append('X');
    Assert(A.Length = 1);
    A.Append('ABC');
    Assert(A.Length = 4);
    Assert(A.GetAsRawByteString = 'XABC');
    A.AppendCRLF;
    Assert(A.Length = 6);
    A.AppendCh('D');
    Assert(A.Length = 7);
    A.AppendCh('E', 3);
    Assert(A.Length = 10);
    Assert(A.GetAsRawByteString = 'XABC'#13#10'DEEE');
    A.Pack;
    Assert(A.Length = 10);
    A.Clear;
    Assert(A.Length = 0);
  finally
    A.Free;
  end;
end;

{$IFDEF SupportUnicodeString}
procedure Test_UnicodeStringBuilder;
var A : TUnicodeStringBuilder;
begin
  A := TUnicodeStringBuilder.Create;
  try
    Assert(A.Length = 0);
    A.Append('X');
    Assert(A.Length = 1);
    A.Append('ABC');
    Assert(A.Length = 4);
    Assert(A.GetAsUnicodeString = 'XABC');
    A.AppendCRLF;
    Assert(A.Length = 6);
    A.AppendCh(WideChar('D'));
    Assert(A.Length = 7);
    A.AppendCh(WideChar('E'), 3);
    Assert(A.Length = 10);
    Assert(A.GetAsUnicodeString = 'XABC'#13#10'DEEE');
    A.Pack;
    Assert(A.Length = 10);
    A.Clear;
    Assert(A.Length = 0);
  finally
    A.Free;
  end;
end;
{$ENDIF}

procedure Test_StringBuilder;
var A : TStringBuilder;
begin
  A := TStringBuilder.Create;
  try
    Assert(A.Length = 0);
    A.Append('X');
    Assert(A.Length = 1);
    A.Append('ABC');
    Assert(A.Length = 4);
    Assert(A.GetAsString = 'XABC');
    A.AppendCRLF;
    Assert(A.Length = 6);
    A.AppendCh('D');
    Assert(A.Length = 7);
    A.AppendCh('E', 3);
    Assert(A.Length = 10);
    Assert(A.GetAsString = 'XABC'#13#10'DEEE');
    A.Pack;
    Assert(A.Length = 10);
    A.Clear;
    Assert(A.Length = 0);
  finally
    A.Free;
  end;
end;



{                                                                              }
{ Tests                                                                        }
{                                                                              }
procedure Test;
begin
  {$IFDEF SupportAnsiString}
  Test_AnsiStringBuilder;
  {$ENDIF}
  Test_RawByteStringBuilder;
  {$IFDEF SupportUnicodeString}
  Test_UnicodeStringBuilder;
  {$ENDIF}
  Test_StringBuilder;
end;



end.

