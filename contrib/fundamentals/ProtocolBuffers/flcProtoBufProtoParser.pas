{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals 5.00                                        }
{   File name:        flcProtoBufParser.pas                                    }
{   File version:     5.05                                                     }
{   Description:      Protocol Buffer proto file parser.                       }
{                                                                              }
{   Copyright:        Copyright (c) 2012-2016, David J Butler                  }
{                     All rights reserved.                                     }
{                     This file is licensed under the BSD License.             }
{                     See http://www.opensource.org/licenses/bsd-license.php   }
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
{   2012/04/15  0.01  Lexer and basic parsing.                                 }
{   2012/04/17  0.02  Line comments, complex types, recursive declarations.    }
{   2012/04/25  0.03  Allow enums definitions at root level.                   }
{   2012/04/26  0.03  Parse and use imported packages.                         }
{                     Parser keeps track of line number.                       }
{   2016/01/13  0.04  RawByteString changes.                                   }
{   2016/01/14  5.05  Revised for Fundamentals 5.                              }
{                                                                              }
{ Supported compilers:                                                         }
{                                                                              }
{   Delphi XE7 Win32                    5.05  2016/01/14                       }
{   Delphi XE7 Win64                    5.05  2016/01/14                       }
{                                                                              }
{******************************************************************************}

{$INCLUDE flcProtoBuf.inc}

unit flcProtoBufProtoParser;

interface

uses
  { System }
  SysUtils,

  { Fundamentals }
  flcStdTypes,
  flcProtoBufProtoNodes;



type
  TpbProtoParserToken = (
    pptNone,
    pptEndOfText,

    pptLineComment,

    pptSemiColon,
    pptOpenCurly,
    pptCloseCurly,
    pptEqualSign,
    pptOpenSquare,
    pptCloseSquare,
    pptOpenParenthesis,
    pptCloseParenthesis,
    pptComma,

    pptLiteralInteger,
    pptLiteralFloat,
    pptLiteralString,

    pptIdentifier,

    pptMessage,

    pptRequired,
    pptOptional,
    pptRepeated,

    pptDouble,
    pptFloat,
    pptInt32,
    pptInt64,
    pptUInt32,
    pptUInt64,
    pptSInt32,
    pptSInt64,
    pptFixed32,
    pptFixed64,
    pptSFixed32,
    pptSFixed64,
    pptBool,
    pptString,
    pptBytes,

    pptDefault,
    pptEnum,
    pptImport,
    pptPackage,
    pptOption,
    pptExtend,
    pptService,
    pptPacked,
    pptExtensions,
    pptTrue,
    pptFalse,
    pptTo,
    pptMax
  );

const
  pptKeywordFirst = pptMessage;
  pptKeywordLast  = pptExtensions;

type
  EpbProtoParser = class(Exception);

  TpbProtoParserStateFlag = (
    ppsfPackageIdStatement);

  TpbProtoParserStateFlags = set of TpbProtoParserStateFlag;

  TpbProtoParser = class
  protected
    FProtoPath    : String;
    FFileName     : String;
    FFileNameUsed : String;
    FFileNameName : String;

    FBufPtr    : PByteChar;
    FBufSize   : Integer;
    FBufPos    : Integer;
    FBufStrRef : RawByteString;
    FLineNr    : Integer;

    FNodeFactory : TpbProtoNodeFactory;

    FToken      : TpbProtoParserToken;
    FTokenStr   : RawByteString;
    FTokenInt   : Int64;
    FTokenFloat : Extended;

    FStateFlags : TpbProtoParserStateFlags;

    procedure ResetParser;

    function  EndOfText: Boolean;

    function  SkipChar: Boolean;
    function  SkipCh(const C: ByteCharSet): Boolean;
    function  ExtractChar(var C: AnsiChar): Boolean;
    function  SkipAllCh(const C: ByteCharSet): Boolean;
    function  SkipWhiteSpace: Boolean;
    function  SkipToStr(const S: RawByteString; const CaseSensitive: Boolean): Boolean;

    function  ExtractAllCh(const C: ByteCharSet): RawByteString;
    function  ExtractTo(const C: ByteCharSet; var S: RawByteString; const SkipDelim: Boolean): AnsiChar;

    function  GetNextToken_IdentifierOrKeword: TpbProtoParserToken;
    function  GetNextToken_Number: TpbProtoParserToken;
    function  GetNextToken_String: TpbProtoParserToken;
    function  GetNextToken_LineComment: TpbProtoParserToken;
    function  GetNextToken: TpbProtoParserToken;

    function  SkipToken(const Token: TpbProtoParserToken): Boolean;
    procedure ExpectToken(const Token: TpbProtoParserToken; const TokenExpected: String);
    procedure ExpectDelimiter;
    procedure ExpectEqualSign;

    function  ExpectIdentifier: RawByteString;
    function  ExpectLiteralInteger: LongInt;
    function  ExpectLiteralFloat: Extended;
    function  ExpectLiteralString: RawByteString;
    function  ExpectLiteralBoolean: Boolean;

    function  ParseFieldCardinality: TpbProtoFieldCardinality;
    function  ExpectFieldCardinality: TpbProtoFieldCardinality;
    function  ParseFieldBaseType: TpbProtoFieldBaseType;
    function  ExpectFieldType(const AField: TpbProtoField): TpbProtoFieldType;

    procedure ParseEnumValue(const P: TpbProtoPackage; const AParentNode: TpbProtoNode; const E: TpbProtoEnum);
    function  ParseEnum(const P: TpbProtoPackage; const AParentNode: TpbProtoNode): TpbProtoEnum;

    function  ExpectLiteral(const P: TpbProtoPackage; const AParentNode: TpbProtoNode): TpbProtoLiteral;

    procedure ParseFieldOptions(const P: TpbProtoPackage; const M: TpbProtoMessage; const F: TpbProtoField);
    procedure ParseField(const P: TpbProtoPackage; const M: TpbProtoMessage);

    procedure ParseMessageExtensions(const P: TpbProtoPackage; const M: TpbProtoMessage);
    procedure ParseMessageEntry(const P: TpbProtoPackage; const M: TpbProtoMessage);
    function  ParseMessageDeclaration(const P: TpbProtoPackage; const AParent: TpbProtoNode): TpbProtoMessage;

    procedure ParsePackageOption(const APackage: TpbProtoPackage);
    procedure ParseImportStatement(const APackage: TpbProtoPackage);
    procedure ParsePackageIdStatement(const APackage: TpbProtoPackage);

    procedure InitPackage(const P: TpbProtoPackage);
    function  FindProtoFile(const AFileName: String): String;
    procedure ProcessImport(const APackage: TpbProtoPackage; const AFileName: RawByteString);

  public
    property  ProtoPath: String read FProtoPath write FProtoPath;

    procedure SetTextBuf(const Buf; const BufSize: Integer);
    procedure SetTextStr(const S: RawByteString);
    procedure SetFileName(const AFileName: String);

    function  Parse(const ANodeFactory: TpbProtoNodeFactory): TpbProtoPackage;
  end;



implementation

uses
  { System }
  Classes,

  { Fundamentals }
  flcUtils,
  flcStrings,
  flcFloats;



{ TpbProtoParser }

type
  TKeywordMap = record
    Keyword : RawByteString;
    Token   : TpbProtoParserToken;
  end;

const
  KeywordMapEntries = 32;
  KeywordMap : array[0..KeywordMapEntries - 1] of TKeywordMap = (
      (
      Keyword : 'message';
      Token   : pptMessage;
      ),
      (
      Keyword : 'required';
      Token   : pptRequired;
      ),
      (
      Keyword : 'optional';
      Token   : pptOptional;
      ),
      (
      Keyword : 'repeated';
      Token   : pptRepeated;
      ),
      (
      Keyword : 'double';
      Token   : pptDouble;
      ),
      (
      Keyword : 'float';
      Token   : pptFloat;
      ),
      (
      Keyword : 'int32';
      Token   : pptInt32;
      ),
      (
      Keyword : 'int64';
      Token   : pptInt64;
      ),
      (
      Keyword : 'uint32';
      Token   : pptUInt32;
      ),
      (
      Keyword : 'uint64';
      Token   : pptUInt64;
      ),
      (
      Keyword : 'sint32';
      Token   : pptSInt32;
      ),
      (
      Keyword : 'sint64';
      Token   : pptSInt64;
      ),
      (
      Keyword : 'fixed32';
      Token   : pptFixed32;
      ),
      (
      Keyword : 'fixed64';
      Token   : pptFixed64;
      ),
      (
      Keyword : 'sfixed32';
      Token   : pptSFixed32;
      ),
      (
      Keyword : 'sfixed64';
      Token   : pptSFixed64;
      ),
      (
      Keyword : 'bool';
      Token   : pptBool;
      ),
      (
      Keyword : 'string';
      Token   : pptString;
      ),
      (
      Keyword : 'bytes';
      Token   : pptBytes;
      ),
      (
      Keyword : 'default';
      Token   : pptDefault;
      ),
      (
      Keyword : 'enum';
      Token   : pptEnum;
      ),
      (
      Keyword : 'import';
      Token   : pptImport;
      ),
      (
      Keyword : 'package';
      Token   : pptPackage;
      ),
      (
      Keyword : 'option';
      Token   : pptOption;
      ),
      (
      Keyword : 'extend';
      Token   : pptExtend;
      ),
      (
      Keyword : 'service';
      Token   : pptService;
      ),
      (
      Keyword : 'packed';
      Token   : pptPacked;
      ),
      (
      Keyword : 'extensions';
      Token   : pptExtensions;
      ),
      (
      Keyword : 'true';
      Token   : pptTrue;
      ),
      (
      Keyword : 'false';
      Token   : pptFalse;
      ),
      (
      Keyword : 'to';
      Token   : pptTo;
      ),
      (
      Keyword : 'max';
      Token   : pptMax;
      )
  );

procedure TpbProtoParser.SetTextBuf(const Buf; const BufSize: Integer);
begin
  FFileName := '';
  if BufSize < 0 then
    raise EpbProtoParser.Create('Invalid parameter');
  FBufPtr := @Buf;
  FBufSize := BufSize;
  FBufPos := 0;
  FBufStrRef := '';
end;

procedure TpbProtoParser.SetTextStr(const S: RawByteString);
begin
  FFileName := '';
  FBufStrRef := S;
  FBufSize := Length(S);
  FBufPtr := Pointer(FBufStrRef);
  FBufPos := 0;
end;

procedure TpbProtoParser.SetFileName(const AFileName: String);
var F : TFileStream;
    L : Integer;
    S : RawByteString;
    E : TSearchRec;
begin
  FFileName := AFileName;
  FFileNameUsed := FindProtoFile(FFileName);
  if FindFirst(FFileNameUsed, faAnyFile, E) = 0 then
    begin
      // get file name from file system to preserve case
      FFileNameName := E.Name;
      FindClose(E);
    end
  else
    FFileNameName := ExtractFileName(FFileNameUsed);
  F := TFileStream.Create(FFileNameUsed, fmOpenRead);
  try
    L := F.Size;
    SetLength(S, L);
    if L > 0 then
      F.ReadBuffer(S[1], L);
  finally
    F.Free;
  end;
  FBufStrRef := S;
  FBufSize := Length(S);
  FBufPtr := Pointer(FBufStrRef);
  FBufPos := 0;
end;

procedure TpbProtoParser.ResetParser;
begin
  FBufPos := 0;
  FToken := pptNone;
  FStateFlags := [];
  FLineNr := 1;
end;

function TpbProtoParser.EndOfText: Boolean;
begin
  Result := FBufPos >= FBufSize;
end;

function TpbProtoParser.SkipChar: Boolean;
begin
  if EndOfText then
    begin
      Result := False;
      exit;
    end;
  Inc(FBufPos);
  Result := True;
end;

function TpbProtoParser.SkipCh(const C: ByteCharSet): Boolean;
var N, F : Integer;
    P : PByteChar;
begin
  F := FBufPos;
  N := FBufSize - F;
  if N <= 0 then
    begin
      Result := False;
      exit;
    end;
  P := FBufPtr;
  Inc(P, F);
  if P^ in C then
    begin
      Inc(FBufPos);
      Result := True;
    end
  else
    Result := False;
end;

function TpbProtoParser.ExtractChar(var C: AnsiChar): Boolean;
var N, F : Integer;
    P : PByteChar;
begin
  F := FBufPos;
  N := FBufSize - F;
  if N <= 0 then
    begin
      C := #0;
      Result := False;
      exit;
    end;
  P := FBufPtr;
  Inc(P, F);
  C := P^;
  Inc(FBufPos);
  Result := True;
end;

function TpbProtoParser.SkipAllCh(const C: ByteCharSet): Boolean;
var N, L, F : Integer;
    P : PByteChar;
begin
  L := 0;
  F := FBufPos;
  N := FBufSize - F;
  P := FBufPtr;
  Inc(P, F);
  while N > 0 do
    if P^ in C then
      begin
        Inc(P);
        Dec(N);
        Inc(L);
      end
    else
      break;
  if L > 0 then
    begin
      Inc(FBufPos, L);
      Result := True;
    end
  else
    Result := False;
end;

function TpbProtoParser.SkipWhiteSpace: Boolean;
var R : Boolean;
begin
  Result := False;
  repeat
    R := False;
    if SkipAllCh([#1..#32] - [#13]) then
      R := True;
    if SkipCh([#13]) then
      begin
        Inc(FLineNr);
        R := True;
      end;
    if R then
      Result := True;
  until not R;
end;

function TpbProtoParser.SkipToStr(const S: RawByteString; const CaseSensitive: Boolean): Boolean;
var N, L, F, C : Integer;
    P : PByteChar;
    R, T : Boolean;
begin
  L := Length(S);
  F := FBufPos;
  N := FBufSize - F;
  P := FBufPtr;
  Inc(P, F);
  R := False;
  C := 0;
  while N >= L do
    begin
      if CaseSensitive then
        T := SysUtils.CompareMem(Pointer(S), P, L)
      else
        T := StrPMatchNoAsciiCaseB(Pointer(S), Pointer(P), L);
      if T then
        break;
      Dec(N);
      Inc(P);
      Inc(C);
      R := True;
    end;
  Inc(FBufPos, C);
  Result := R;
end;

function TpbProtoParser.ExtractAllCh(const C: ByteCharSet): RawByteString;
var N, L : Integer;
    P, Q : PByteChar;
    D : AnsiChar;
    R : Boolean;
    S : RawByteString;
begin
  P := FBufPtr;
  Inc(P, FBufPos);
  Q := P;
  N := FBufSize - FBufPos;
  L := 0;
  while N > 0 do
    begin
      D := P^;
      R := D in C;
      if not R then
        break
      else
        Inc(L);
      Inc(P);
      Dec(N);
    end;
  SetLength(S, L);
  if L > 0 then
    Move(Q^, S[1], L);
  Inc(FBufPos, L);
  Result := S;
end;

function TpbProtoParser.ExtractTo(const C: ByteCharSet; var S: RawByteString; const SkipDelim: Boolean): AnsiChar;
var N, L : Integer;
    P, Q : PByteChar;
    D : AnsiChar;
    R : Boolean;
begin
  P := FBufPtr;
  Inc(P, FBufPos);
  Q := P;
  N := FBufSize - FBufPos;
  L := 0;
  R := False;
  D := #0;
  while N > 0 do
    begin
      D := P^;
      R := D in C;
      if R then
        break
      else
        Inc(L);
      Inc(P);
      Dec(N);
    end;
  SetLength(S, L);
  if L > 0 then
    Move(Q^, S[1], L);
  Inc(FBufPos, L);
  if R and SkipDelim then
    Inc(FBufPos);
  Result := D;
end;

function TpbProtoParser.GetNextToken_IdentifierOrKeword: TpbProtoParserToken;
var
  S : RawByteString;
  I : Integer;
begin
  S := ExtractAllCh(['A'..'Z', 'a'..'z', '_', '0'..'9', '.']);
  FTokenStr := S;
  for I := 0 to KeywordMapEntries - 1 do
    if KeywordMap[I].Keyword = S then
      begin
        Result := KeywordMap[I].Token;
        exit;
      end;
  Result := pptIdentifier;
end;

function TpbProtoParser.GetNextToken_Number: TpbProtoParserToken;
var
  S : RawByteString;
  I : Int64;
  F : Extended;
begin
  S := ExtractAllCh(['-', '0'..'9', '.', 'e', 'E']);
  if TryStringToInt64B(S, I) then
    begin
      Result := pptLiteralInteger;
      FTokenInt := I;
      exit;
    end;
  if TryStringToFloatB(S, F) then
    begin
      Result := pptLiteralFloat;
      FTokenFloat := F;
      exit;
    end;
  // TODO: hex values e.g. 0xFFFFFFFF, -0x7FFFFFFF
  raise EpbProtoParser.CreateFmt('Invalid numeric value (%s)', [S]);
end;

const
  SErr_StringNotTerminated = 'String literal not terminated';

function TpbProtoParser.GetNextToken_String: TpbProtoParserToken;
var
  S, T : RawByteString;
  F : Boolean;
  C, D : AnsiChar;
begin
  SkipChar;
  S := '';
  F := False;
  repeat
    C := ExtractTo(['"', '\'], T, True);
    S := S + T;
    case C of
      '"' :
        begin
          if SkipCh(['"']) then // escaped quote
            S := S + '"'
          else
            F := True;
        end;
      '\' : // escape character
        begin
          if not ExtractChar(C) then
            raise EpbProtoParser.Create(SErr_StringNotTerminated);
          case C of
            '0' : D := #0;
            'r' : D := #13;
            'n' : D := #10;
            // TODO: hex format e.g. \xfe
            //       unicode escaping in string, e.g. "\u1234"
            //       utf8 escaping in string, e.g. "\341\210\264"
          else
            D := C;
          end;
          S := S + D;

        end;
    else
      raise EpbProtoParser.Create(SErr_StringNotTerminated);
    end;
  until F;
  // literal string decoded
  FTokenStr := S;
  Result := pptLiteralString;
end;

function TpbProtoParser.GetNextToken_LineComment: TpbProtoParserToken;
begin
  SkipChar;
  if not SkipCh(['/']) then
    raise EpbProtoParser.Create('Unexpected token (/)');
  ExtractTo([#13, #10], FTokenStr, True);
  SkipCh([#13, #10]);
  Result := pptLineComment;
end;

function TpbProtoParser.GetNextToken: TpbProtoParserToken;
var C : AnsiChar;
    P : PByteChar;
    T : TpbProtoParserToken;
begin
  repeat
    FTokenStr := '';
    SkipWhiteSpace;
    if EndOfText then
      begin
        FToken := pptEndOfText;
        Result := pptEndOfText;
        exit;
      end;
    // single character tokens
    P := FBufPtr;
    Inc(P, FBufPos);
    C := P^;
    case C of
      ';' : T := pptSemiColon;
      '{' : T := pptOpenCurly;
      '}' : T := pptCloseCurly;
      '=' : T := pptEqualSign;
      '[' : T := pptOpenSquare;
      ']' : T := pptCloseSquare;
      '(' : T := pptOpenParenthesis;
      ')' : T := pptCloseParenthesis;
      ',' : T := pptComma;
    else
      T := pptNone;
    end;
    if T <> pptNone then
      begin
        SkipChar;
        FToken := T;
        Result := T;
        exit;
      end;
    // other tokens
    case C of
      'A'..'Z',
      'a'..'z',
      '_'       : T := GetNextToken_IdentifierOrKeword;
      '+', '-',
      '0'..'9'  : T := GetNextToken_Number;
      '"'       : T := GetNextToken_String;
      '/'       : T := GetNextToken_LineComment;
    else
      raise EpbProtoParser.CreateFmt('Unexpected input character (%d)', [Ord(C)]);
    end;
  until T <> pptLineComment;
  FToken := T;
  Result := T;
end;

function TpbProtoParser.SkipToken(const Token: TpbProtoParserToken): Boolean;
begin
  Result := FToken = Token;
  if Result then
    GetNextToken;
end;

procedure TpbProtoParser.ExpectToken(const Token: TpbProtoParserToken; const TokenExpected: String);
begin
  if FToken <> Token then
    raise EpbProtoParser.CreateFmt('%s expected', [TokenExpected]);
  GetNextToken;
end;

procedure TpbProtoParser.ExpectDelimiter;
begin
  ExpectToken(pptSemiColon, ';');
end;

procedure TpbProtoParser.ExpectEqualSign;
begin
  ExpectToken(pptEqualSign, '=');
end;

function TpbProtoParser.ExpectIdentifier: RawByteString;
begin
  if not (
     (FToken = pptIdentifier) or
     ( (FToken >= pptKeywordFirst) and (FToken <= pptKeywordLast) )
     ) then
    raise EpbProtoParser.Create('Identifier expected');
  Result := FTokenStr;
  GetNextToken;
end;

function TpbProtoParser.ExpectLiteralInteger: LongInt;
begin
  if FToken <> pptLiteralInteger then
    raise EpbProtoParser.Create('Integer literal expected');
  Result := FTokenInt;
  GetNextToken;
end;

function TpbProtoParser.ExpectLiteralFloat: Extended;
begin
  if FToken <> pptLiteralFloat then
    raise EpbProtoParser.Create('Float literal expected');
  Result := FTokenFloat;
  GetNextToken;
end;

function TpbProtoParser.ExpectLiteralString: RawByteString;
begin
  if FToken <> pptLiteralString then
    raise EpbProtoParser.Create('String literal expected');
  Result := FTokenStr;
  GetNextToken;
end;

function TpbProtoParser.ExpectLiteralBoolean: Boolean;
begin
  if not (FToken in [pptTrue, pptFalse]) then
    raise EpbProtoParser.Create('Boolean literal expected');
  Result := FToken = pptTrue;
  GetNextToken;
end;

function TpbProtoParser.ParseFieldCardinality: TpbProtoFieldCardinality;
begin
  case FToken of
    pptRequired : Result := pfcRequired;
    pptOptional : Result := pfcOptional;
    pptRepeated : Result := pfcRepeated;
  else
    Result := pfcNone;
  end;
  if Result <> pfcNone then
    GetNextToken;
end;

function TpbProtoParser.ExpectFieldCardinality: TpbProtoFieldCardinality;
begin
  Result := ParseFieldCardinality;
  if Result = pfcNone then
    raise EpbProtoParser.Create('Field cardinality expected');
end;

function TpbProtoParser.ParseFieldBaseType: TpbProtoFieldBaseType;
begin
  case FToken of
    pptDouble   : Result := pftDouble;
    pptFloat    : Result := pftFloat;
    pptInt32    : Result := pftInt32;
    pptInt64    : Result := pftInt64;
    pptUInt32   : Result := pftUInt32;
    pptUInt64   : Result := pftUInt64;
    pptSInt32   : Result := pftSInt32;
    pptSInt64   : Result := pftSInt64;
    pptFixed32  : Result := pftFixed32;
    pptFixed64  : Result := pftFixed64;
    pptSFixed32 : Result := pftSFixed32;
    pptSFixed64 : Result := pftSFixed64;
    pptBool     : Result := pftBool;
    pptString   : Result := pftString;
    pptBytes    : Result := pftBytes;
  else
    Result := pftNone;
  end;
  if Result <> pftNone then
    GetNextToken;
end;

function TpbProtoParser.ExpectFieldType(const AField: TpbProtoField): TpbProtoFieldType;
var A : TpbProtoFieldType;
    B : TpbProtoFieldBaseType;
begin
  A := FNodeFactory.CreateFieldType(AField);
  try
    B := ParseFieldBaseType;
    if B <> pftNone then
      A.BaseType := B
    else
    if FToken = pptIdentifier then
      begin
        A.BaseType := pftIdentifier;
        A.IdenStr := FTokenStr;
        GetNextToken;
      end
    else
      raise EpbProtoParser.Create('Field type expected');
  except
    A.Free;
    raise;
  end;
  Result := A;
end;

procedure TpbProtoParser.ParseEnumValue(const P: TpbProtoPackage; const AParentNode: TpbProtoNode;
          const E: TpbProtoEnum);
var V : TpbProtoEnumValue;
begin
  V := FNodeFactory.CreateEnumValue(E);
  try
    V.Name := ExpectIdentifier;
    ExpectEqualSign;
    V.Value := ExpectLiteralInteger;
    ExpectDelimiter;
  except
    V.Free;
    raise;
  end;
  E.Add(V);
end;

function TpbProtoParser.ParseEnum(const P: TpbProtoPackage; const AParentNode: TpbProtoNode): TpbProtoEnum;
var E : TpbProtoEnum;
begin
  Assert(FToken = pptEnum);
  GetNextToken;

  E := FNodeFactory.CreateEnum(AParentNode);
  try
    E.Name := ExpectIdentifier;
    ExpectToken(pptOpenCurly, '{');
    while not (FToken in [pptCloseCurly, pptEndOfText]) do
      ParseEnumValue(P, AParentNode, E);
    ExpectToken(pptCloseCurly, '}');
  except
    E.Free;
    raise;
  end;

  Result := E;
end;

function TpbProtoParser.ExpectLiteral(const P: TpbProtoPackage; const AParentNode: TpbProtoNode): TpbProtoLiteral;
var L : TpbProtoLiteral;
begin
  L := FNodeFactory.CreateLiteral(AParentNode);
  try
    case FToken of
      pptLiteralInteger :
        begin
          L.LiteralType := pltInteger;
          L.LiteralInt := ExpectLiteralInteger;
        end;
      pptLiteralFloat :
        begin
          L.LiteralType := pltFloat;
          L.LiteralFloat := ExpectLiteralFloat;
        end;
      pptLiteralString :
        begin
          L.LiteralType := pltString;
          L.LiteralStr := ExpectLiteralString;
        end;
      pptTrue, pptFalse :
        begin
          L.LiteralType := pltBoolean;
          L.LiteralBool := FToken = pptTrue;
          GetNextToken;
        end;
      pptIdentifier :
        begin
          L.LiteralType := pltIdentifier;
          L.LiteralIden := ExpectIdentifier;
        end;
    else
      raise EpbProtoParser.Create('Invalid literal value');
    end;
  except
    L.Free;
    raise;
  end;
  Result := L;
end;

procedure TpbProtoParser.ParseFieldOptions(const P: TpbProtoPackage; const M: TpbProtoMessage; const F: TpbProtoField);
var A : TpbProtoOption;
begin
  Assert(FToken = pptOpenSquare);
  GetNextToken;
  repeat
    repeat
      if SkipToken(pptPacked) then
        begin
          ExpectEqualSign;
          F.OptionPacked := ExpectLiteralBoolean;
        end else
      if SkipToken(pptDefault) then
        begin
          ExpectEqualSign;
          F.DefaultValue := ExpectLiteral(P, F);
        end
      else
        begin
          // unknown option
          A := TpbProtoOption.Create(FNodeFactory);
          try
            A.Custom := SkipToken(pptOpenParenthesis);
            A.Name := ExpectIdentifier;
            if A.Custom then
              ExpectToken(pptCloseParenthesis, ')');
            ExpectEqualSign;
            A.Value := ExpectLiteral(P, A);
          except
            A.Free;
            raise;
          end;
          F.AddOption(A);
        end;
    until not SkipToken(pptComma);
    ExpectToken(pptCloseSquare, ']');
  until not SkipToken(pptOpenSquare);
end;

const
  pbMaxTagID = 536870911;

procedure TpbProtoParser.ParseField(const P: TpbProtoPackage; const M: TpbProtoMessage);
var F : TpbProtoField;
begin
  F := FNodeFactory.CreateField(M);
  try
    F.Cardinality := ExpectFieldCardinality;
    F.FieldType := ExpectFieldType(F);
    F.Name := ExpectIdentifier;
    ExpectEqualSign;
    F.TagID := ExpectLiteralInteger;
    if (F.TagID <= 0) or (F.TagID > pbMaxTagID) then
      raise EpbProtoParser.CreateFmt('TagID out of range (%d)', [F.TagID]);
    if (F.TagID >= 19000) and (F.TagID <= 19999) then
      raise EpbProtoParser.CreateFmt('TagID reserved (%d)', [F.TagID]);
    if FToken = pptOpenSquare then
      ParseFieldOptions(P, M, F);
    ExpectDelimiter;
    if Assigned(M.GetFieldByTagID(F.TagID)) then
      raise EpbProtoParser.CreateFmt('Duplicate TagID (%d)', [F.TagID]);
  except
    F.Free;
    raise;
  end;
  M.AddField(F);
end;

procedure TpbProtoParser.ParseMessageExtensions(const P: TpbProtoPackage; const M: TpbProtoMessage);
begin
  Assert(FToken = pptExtensions);
  GetNextToken;
  M.ExtensionsMin := ExpectLiteralInteger;
  ExpectToken(pptTo, 'to');
  if SkipToken(pptMax) then
    M.ExtensionsMax := pbMaxTagID
  else
    M.ExtensionsMax := ExpectLiteralInteger;
end;

procedure TpbProtoParser.ParseMessageEntry(const P: TpbProtoPackage; const M: TpbProtoMessage);
begin
  case FToken of
    pptEnum       : M.AddEnum(ParseEnum(P, M));
    pptMessage    : M.AddMessage(ParseMessageDeclaration(P, M));
    pptExtensions : ParseMessageExtensions(P, M);
  else
    ParseField(P, M);
  end;
end;

{ example:                                                                     }
(*  message Open { <fields> }                                                 *)
function TpbProtoParser.ParseMessageDeclaration(const P: TpbProtoPackage; const AParent: TpbProtoNode): TpbProtoMessage;
var M : TpbProtoMessage;
begin
  Assert(FToken = pptMessage);
  GetNextToken;

  M := FNodeFactory.CreateMessage(AParent);
  try
    M.Name := ExpectIdentifier;
    ExpectToken(pptOpenCurly, '{');
    while not (FToken in [pptCloseCurly, pptEndOfText]) do
      ParseMessageEntry(P, M);
    ExpectToken(pptCloseCurly, '}');
  except
    M.Free;
    raise;
  end;

  Result := M;
end;

{ example:                                                                     }
{   option optimize_for = SPEED;                                               }
procedure TpbProtoParser.ParsePackageOption(const APackage: TpbProtoPackage);
var A : TpbProtoOption;
begin
  Assert(FToken = pptOption);
  GetNextToken;

  A := TpbProtoOption.Create(FNodeFactory);
  try
    A.Custom := SkipToken(pptOpenParenthesis);
    A.Name := ExpectIdentifier;
    if A.Custom then
      ExpectToken(pptCloseParenthesis, ')');
    ExpectEqualSign;
    A.Value := ExpectLiteral(APackage, A);
    ExpectDelimiter;
  except
    A.Free;
    raise;
  end;

  APackage.AddOption(A);
end;

{ example:                                                                     }
{   import "myproject/other_protos.proto";                                     }
procedure TpbProtoParser.ParseImportStatement(const APackage: TpbProtoPackage);
var F : RawByteString;
begin
  Assert(FToken = pptImport);
  GetNextToken;

  F := ExpectLiteralString;
  APackage.AddImport(F);
  ExpectDelimiter;

  ProcessImport(APackage, F);
end;

{ example:                                                                     }
{   package foo.bar;                                                           }
procedure TpbProtoParser.ParsePackageIdStatement(const APackage: TpbProtoPackage);
begin
  Assert(FToken = pptPackage);
  GetNextToken;

  if ppsfPackageIdStatement in FStateFlags then
    raise EpbProtoParser.Create('Duplicate package declaration');
  Include(FStateFlags, ppsfPackageIdStatement);

  APackage.Name := ExpectIdentifier;
  ExpectDelimiter;
end;

function TpbProtoParser.Parse(const ANodeFactory: TpbProtoNodeFactory): TpbProtoPackage;
var P : TpbProtoPackage;
begin
  if not Assigned(ANodeFactory) then
    raise EpbProtoParser.Create('Node factory required');
  FNodeFactory := ANodeFactory;
  ResetParser;
  try
    GetNextToken;
    P := FNodeFactory.CreatePackage;
    try
      InitPackage(P);
      while FToken <> pptEndOfText do
        case FToken of
          pptPackage   : ParsePackageIdStatement(P);
          pptImport    : ParseImportStatement(P);
          pptOption    : ParsePackageOption(P);
          pptMessage   : P.AddMessage(ParseMessageDeclaration(P, P));
          pptSemiColon : GetNextToken;
          pptEnum      : P.AddEnum(ParseEnum(P, P));
        else
          raise EpbProtoParser.Create('Unexpected token');
        end;
    except
      P.Free;
      raise;
    end;
  except
    on E: Exception do
      raise EpbProtoParser.CreateFmt('%s(%d): %s', [FFileNameName, FLineNr, E.Message]);
  end;
  Result := P;
end;

procedure TpbProtoParser.InitPackage(const P: TpbProtoPackage);
var S : String;
begin
  if FFileName <> '' then
    begin
      // set file name
      S := FFileNameName;
      {$IFDEF StringIsUnicode}
      P.FileName := UTF8Encode(S);
      {$ELSE}
      P.FileName := S;
      {$ENDIF}
      // derive default package name from file name
      if ExtractFileExt(S) = '.proto' then
        S := ChangeFileExt(S, '');
      {$IFDEF StringIsUnicode}
      P.Name := UTF8Encode(S);
      {$ELSE}
      P.Name := S;
      {$ENDIF}
    end
  else
    P.Name := '';
end;

function TpbProtoParser.FindProtoFile(const AFileName: String): String;
var F, S : String;
begin
  if AFileName = '' then
    raise EpbProtoParser.Create('Filename not specified');
  F := '';
  if FileExists(AFileName) then
    F := AFileName;
  if (F = '') and (FProtoPath <> '') then
    begin
      S := IncludeTrailingPathDelimiter(FProtoPath) + AFileName;
      if FileExists(S) then
        F := S;
    end;
  if F = '' then
    F := AFileName;
  Result := F;
end;

procedure TpbProtoParser.ProcessImport(const APackage: TpbProtoPackage; const AFileName: RawByteString);
var
  F : String;
  P : TpbProtoParser;
  A : TpbProtoPackage;
begin
  F := FindProtoFile(ToStringB(AFileName));
  P := TpbProtoParser.Create;
  try
    P.ProtoPath := FProtoPath;
    P.SetFileName(F);
    A := P.Parse(FNodeFactory);
    APackage.AddImportedPackage(A);
  finally
    P.Free;
  end;
end;



end.

