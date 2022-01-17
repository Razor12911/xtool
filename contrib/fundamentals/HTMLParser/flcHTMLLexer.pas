{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals 5.00 - HTML Parser                          }
{   File name:        flcHTMLLexer.pas                                         }
{   File version:     5.08                                                     }
{   Description:      HTML Lexer                                               }
{                                                                              }
{   Copyright:        Copyright (c) 2000-2020, David J Butler                  }
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
{   2002/10/21  1.00  Initial version.                                         }
{   2002/10/29  1.01  Unicode support.                                         }
{   2002/11/02  1.02  Optimization for known names.                            }
{   2002/11/04  1.03  Add TelHTMLLexicalParser component.                      }
{   2002/12/16  1.04  Optimizations.                                           }
{                     Add LineBreak token.                                     }
{   2004/06/02  1.05  Treat text inside SCRIPT tags as raw text.               }
{   2004/11/06  1.06  References with optional trailing semi-colon.            }
{   2015/04/11  1.07  UnicodeString changes.                                   }
{   2019/02/22  5.08  Revise for Fundamentals 5.                               }
{                                                                              }
{******************************************************************************}

{$INCLUDE flcHTML.inc}

unit flcHTMLLexer;

interface

uses
  { System }
  Classes,

  { Fundamentals }
  flcStdTypes,
  flcUnicodeReader,
  flcHTMLElements;



{                                                                              }
{ ThtmlLexer                                                                   }
{                                                                              }
type
  ThtmlTokenType = (
      htNone,
      htEOF,
      htText,
      htLineBreak,
      htCharRef,
      htCharRefHex,
      htEntityRef,
      htRefTrailer,
      htStartTag,
      htEndTag,
      htTagAttrName,
      htTagAttrValueStart,
      htTagAttrValueEnd,
      htEmptyTag,
      htComment,
      htCommentEnd,
      htEmptyComment,
      htPITarget,
      htPI,
      htDeclaration,
      htDeclarationText,
      htMarkedSection,
      htCDATA
      );

function  htmlTokenTypeDescription(const TokenType: ThtmlTokenType): String;
function  htmlTokenTypeIDStr(const TokenType: ThtmlTokenType): String;

const
  htmlTokensCharRef           = [htCharRef, htCharRefHex];
  htmlTokensRef               = htmlTokensCharRef + [htEntityRef];
  htmlTokensText              = htmlTokensRef + [htRefTrailer, htText];
  htmlTokensTextInclLineBreak = htmlTokensText + [htLineBreak];
  htmlTokensComment           = [htComment, htEmptyComment];

type
  ThtmlLexerContext = (
      lcTop,
      lcPI,
      lcDeclaration,
      lcComment,
      lcTagAttrName,
      lcTagAttrValue,
      lcTagAttrValueText,
      lcScriptText
      );

  ThtmlLexer = class
  protected
    FReader           : TUnicodeReader;
    FReaderOwner      : Boolean;
    FNoLineBreakToken : Boolean;
    FContext          : ThtmlLexerContext;
    FTokenType        : ThtmlTokenType;
    FTokenStr         : String;
    FTokenStrResolved : Boolean;
    FTokenStrBuf      : PWideChar;
    FTokenStrLen      : Integer;
    FTagToken         : ThtmlTokenType;
    FTagID            : ThtmlTagID;
    FAttrID           : ThtmlAttrID;
    FAttrQuote        : AnsiChar;
    FAttrDelim        : ByteCharSet;
    FTokenCount       : Integer;

    function  GetTokenTypeDescription: String;
    function  GetTokenTypeIDStr: String;
    function  GetTokenStr: String;

    function  SkipSpace: Integer;
    procedure ExtractTextRef(const StrBuf: PWideChar; const StrLen: Integer);
    function  ExtractNumber: String;
    function  ExtractHexDigits: String;
    procedure ExtractNameRef(const C: WideChar);
    procedure ExtractToRef(const DelimStr: RawByteString; const SkipDelim: Boolean;
              const CaseSensitive: Boolean);

    procedure SetTokenText(const Text: String);
    procedure SetTokenTextRef(const StrBuf: PWideChar; const StrLen: Integer);

    procedure ParseText(const InitialText: String);
    procedure ParseCommentStart;
    procedure ParseCommentText;
    procedure ParseComment;
    procedure ParseQTag;
    procedure ParsePI;
    procedure ParseDeclaration;
    procedure ParseTagName;
    procedure ParseStartTag;
    procedure ParseAttrName;
    procedure ParseTagAttrName(const C: WideChar);
    procedure ParseTagAttrValue;
    procedure ParseTagAttrValueText;
    procedure ParseScriptText;
    procedure ParseEndTag;
    procedure ParseETag;
    procedure ParseTag;
    procedure ParseEntity;
    procedure ParseTop;
    procedure ParseToken;

  public
    constructor Create(const Reader: TUnicodeReader; const ReaderOwner: Boolean);
    destructor Destroy; override;

    property  Reader: TUnicodeReader read FReader;
    property  ReaderOwner: Boolean read FReaderOwner write FReaderOwner;

    property  NoLineBreakToken: Boolean read FNoLineBreakToken write FNoLineBreakToken;

    procedure Reset;
    function  GetNextToken: ThtmlTokenType;

    property  Context: ThtmlLexerContext read FContext;
    property  TokenCount: Integer read FTokenCount;
    property  TokenType: ThtmlTokenType read FTokenType;
    property  TokenTypeDescription: String read GetTokenTypeDescription;
    property  TokenTypeIDStr: String read GetTokenTypeIDStr;
    property  TokenStr: String read GetTokenStr;

    property  TagID: ThtmlTagID read FTagID;
    property  AttrID: ThtmlAttrID read FAttrID;

    function  CharRefValue: LongWord;
    function  ResolveCharRef(var Text: String): Boolean;
    function  ResolveEntityRef(var Text: String): Boolean;
    function  ResolveReference(var Text: String): Boolean;
  end;



{                                                                              }
{ TfclHTMLLexicalParser                                                        }
{                                                                              }
type
  ThtmlLexicalParser = class;

  TLexicalParseTokenEvent = procedure(Parser: ThtmlLexicalParser;
    TokenType: ThtmlTokenType) of object;

  TLexicalParseTokenStrEvent = procedure(Parser: ThtmlLexicalParser;
    TokenType: ThtmlTokenType; TokenStr: String) of object;

  TLexicalParserTagEvent = procedure(Parser: ThtmlLexicalParser;
    TagID: ThtmlTagID) of object;
  TLexicalParserTagStrEvent = procedure(Parser: ThtmlLexicalParser;
    TagID: ThtmlTagID; const TagName: String) of object;

  TLexicalParserTextEvent = procedure(Parser: ThtmlLexicalParser;
    Text: String) of object;

  TLexicalParserTagAttrEvent = procedure(Parser: ThtmlLexicalParser;
    TagID: ThtmlTagID; AttrID: ThtmlAttrID) of object;
  TLexicalParserTagAttrStrEvent = procedure(Parser: ThtmlLexicalParser;
    TagID: ThtmlTagID; AttrID: ThtmlAttrID; AttrName: String) of object;
  TLexicalParserTagAttrValueEvent = procedure(Parser: ThtmlLexicalParser;
    TagID: ThtmlTagID; AttrID: ThtmlAttrID; AttrValue: String) of object;

  ThtmlLexicalParserOptions = set of (
      loDisableNotifications,
      loResolveReferences,
      loNoLineBreakToken
      );

  ThtmlLexicalParser = class(TComponent)
  protected
    FOptions            : ThtmlLexicalParserOptions;
    FText               : RawByteString;
    FFileName           : String;
    FEncoding           : RawByteString;
    FOnToken            : TLexicalParseTokenEvent;
    FOnTokenStr         : TLexicalParseTokenStrEvent;
    FOnText             : TLexicalParserTextEvent;
    FOnContentText      : TLexicalParserTextEvent;
    FOnStartTag         : TLexicalParserTagEvent;
    FOnStartTagStr      : TLexicalParserTagStrEvent;
    FOnEndTag           : TLexicalParserTagEvent;
    FOnEndTagStr        : TLexicalParserTagStrEvent;
    FOnTagAttr          : TLexicalParserTagAttrEvent;
    FOnTagAttrStr       : TLexicalParserTagAttrStrEvent;
    FOnTagAttrValue     : TLexicalParserTagAttrValueEvent;
    FOnComment          : TLexicalParserTextEvent;
    FLexer              : ThtmlLexer;
    FAborted            : Boolean;
    FTokenType          : ThtmlTokenType;
    FTokenStr           : String;
    FHasTokenStr        : Boolean;
    FTagID              : ThtmlTagID;
    FAttrID             : ThtmlAttrID;
    FInAttributeValue   : Boolean;
    FGetAttributeValue  : Boolean;
    FAttributeValue     : String;

    procedure SetOptions(const Options: ThtmlLexicalParserOptions);

    function  GetTokenCount: Integer;
    function  GetTokenTypeDescription: String;
    function  GetTokenTypeIDStr: String;
    function  GetTokenStr: String;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Reset;
    procedure Abort;
    procedure GetNextToken;
    procedure Parse;

    property  Aborted: Boolean read FAborted;
    property  TokenCount: Integer read GetTokenCount;
    property  TokenType: ThtmlTokenType read FTokenType;
    property  TokenTypeDescription: String read GetTokenTypeDescription;
    property  TokenTypeIDStr: String read GetTokenTypeIDStr;
    property  TokenStr: String read GetTokenStr;

    property  TagID: ThtmlTagID read FTagID;
    property  AttrID: ThtmlAttrID read FAttrID;

    property  Options: ThtmlLexicalParserOptions read FOptions write SetOptions default [];
    property  Text: RawByteString read FText write FText;
    property  FileName: String read FFileName write FFileName;
    property  Encoding: RawByteString read FEncoding write FEncoding;

    property  OnToken: TLexicalParseTokenEvent read FOnToken write FOnToken;
    property  OnTokenStr: TLexicalParseTokenStrEvent read FOnTokenStr write FOnTokenStr;

    property  OnText: TLexicalParserTextEvent read FOnText write FOnText;
    property  OnContentTextU: TLexicalParserTextEvent read FOnContentText write FOnContentText;

    property  OnStartTag: TLexicalParserTagEvent read FOnStartTag write FOnStartTag;
    property  OnStartTagStr: TLexicalParserTagStrEvent read FOnStartTagStr write FOnStartTagStr;
    property  OnEndTag: TLexicalParserTagEvent read FOnEndTag write FOnEndTag;
    property  OnEndTagStr: TLexicalParserTagStrEvent read FOnEndTagStr write FOnEndTagStr;

    property  OnTagAttr: TLexicalParserTagAttrEvent read FOnTagAttr write FOnTagAttr;
    property  OnTagAttrStr: TLexicalParserTagAttrStrEvent read FOnTagAttrStr write FOnTagAttrStr;
    property  OnTagAttrValue: TLexicalParserTagAttrValueEvent read FOnTagAttrValue write FOnTagAttrValue;

    property  OnComment: TLexicalParserTextEvent read FOnComment write FOnComment;
  end;

  TfclHtmlLexicalParser = class(ThtmlLexicalParser)
  published
    property  Options;
    property  Text;
    property  FileName;
    property  Encoding;

    property  OnToken;
    property  OnTokenStr;

    property  OnText;
    property  OnContentTextU;

    property  OnStartTag;
    property  OnStartTagStr;
    property  OnEndTag;
    property  OnEndTagStr;

    property  OnTagAttr;
    property  OnTagAttrStr;
    property  OnTagAttrValue;

    property  OnComment;
  end;



{                                                                              }
{ Tests                                                                        }
{                                                                              }
{$IFDEF HTML_TEST}
procedure Test;
{$ENDIF}



implementation

uses
  { System }
  SysUtils,

  { Fundamentals }
  flcASCII,
  flcUtils,
  flcUTF,
  flcStrings,
  flcUnicodeChar,
  flcUnicodeCodecs,

  { HTML }
  flcHTMLReader,
  flcHTMLCharEntity;



{ String helpers functions }

const
  htmlWhiteSpace = [#0..#32];
  htmlNumberChar = ['0'..'9'];
  htmlHexDigits  = htmlNumberChar + ['A'..'F', 'a'..'f'];

function htmlIsNumberChar(const C: WideChar): Boolean;
begin
  Result := WideCharInCharSet(C, htmlNumberChar);
end;

function htmlIsHexDigit(const C: WideChar): Boolean;
begin
  Result := WideCharInCharSet(C, htmlHexDigits);
end;

function htmlIsWhiteSpace(const Ch: WideChar): Boolean;
begin
  Result := UnicodeIsWhiteSpace(Ch);
end;

function htmlIsLineBreakChar(const Ch: WideChar): Boolean;
begin
  case Ord(Ch) of
    10, 13 : Result := True;
  else
    Result := False;
  end;
end;

function htmlIsNameStartChar(const Ch: WideChar): Boolean;
begin
  case Ch of
    'A'..'Z', 'a'..'z', '_', ':' :
      Result := True;
  else
    Result := False;
  end;
end;

function htmlIsNameChar(const Ch: WideChar): Boolean;
begin
  Result := htmlIsNameStartChar(Ch);
  if Result then
    exit;
  case Ch of
    '0'..'9', '.', '-' :
      Result := True;
  else
    Result := False;
  end;
end;



{                                                                              }
{ Lexer functions                                                              }
{                                                                              }
const
  htmlTokenTypeDescStr: array[ThtmlTokenType] of String = (
      '',
      'EOF',
      'text',
      'line break',
      'character entity reference',
      'hex character entity reference',
      'entity reference',
      'reference trailer',
      'start tag',
      'end tag',
      'tag attribute name',
      'tag attribute value',
      'tag attribute value end',
      'empty tag',
      'comment',
      'comment end',
      'empty comment',
      'processing instruction target',
      'processing instruction',
      'declaration',
      'declaration text',
      'marked section',
      'CDATA section'
    );

function htmlTokenTypeDescription(const TokenType: ThtmlTokenType): String;
begin
  Result := htmlTokenTypeDescStr[TokenType];
end;

const
  htmlTokenTypeID: array[ThtmlTokenType] of String = (
      '',
      'EOF',
      'TEXT',
      'LINE_BREAK',
      'CHAR_REF',
      'CHAR_REF_HEX',
      'ENTITY_REF',
      'REF_TRAILER',
      'TAG_START',
      'TAG_END',
      'TAG_ATTR_NAME',
      'TAG_ATTR_VALUE',
      'TAG_ATTR_VALUE_END',
      'TAG_EMPTY',
      'COMMENT',
      'COMMENT_END',
      'COMMENT_EMPTY',
      'PI_TARGET',
      'PI',
      'DECL',
      'DECL_TEXT',
      'MARKED_SECTION',
      'CDATA'
    );

function htmlTokenTypeIDStr(const TokenType: ThtmlTokenType): String;
begin
  Result := htmlTokenTypeID[TokenType];
end;



{                                                                              }
{ ThtmlLexer                                                                   }
{                                                                              }
constructor ThtmlLexer.Create(const Reader: TUnicodeReader; const ReaderOwner: Boolean);
begin
  inherited Create;
  Assert(Assigned(Reader));
  FReader := Reader;
  FReaderOwner := ReaderOwner;
  FNoLineBreakToken := False;
end;

destructor ThtmlLexer.Destroy;
begin
  if FReaderOwner then
    FreeAndNil(FReader);
  inherited Destroy;
end;

procedure ThtmlLexer.Reset;
begin
  // reset
  Assert(Assigned(FReader));
  FReader.Reset;
  // initialize state
  FTokenType := htNone;
  FTokenStr := '';
  FTokenStrResolved := True;
  FTokenStrBuf := nil;
  FTokenStrLen := 0;
  FTagID := HTML_TAG_None;
  FAttrID := HTML_ATTR_None;
  FContext := lcTop;
  FTokenCount := 0;
end;

function ThtmlLexer.GetTokenTypeDescription: String;
begin
  Result := htmlTokenTypeDescription(FTokenType);
end;

function ThtmlLexer.GetTokenTypeIDStr: String;
begin
  Result := htmlTokenTypeIDStr(FTokenType);
end;

function ThtmlLexer.GetTokenStr: String;
begin
  if not FTokenStrResolved then
    begin
      case FTokenType of
        htStartTag, htEndTag:
          if FTagID <> HTML_TAG_NONE then
            FTokenStr := htmlGetTagName(FTagID);
        htTagAttrName:
          if FAttrID <> HTML_ATTR_NONE then
            FTokenStr := htmlGetAttrName(FAttrID);
        htText,
        htEntityRef,
        htDeclaration,
        htPI,
        htPITarget,
        htComment,
        htCommentEnd,
        htMarkedSection,
        htCDATA:
          FTokenStr := StrPToStrU(FTokenStrBuf, FTokenStrLen);
      else
        FTokenStr := '';
      end;
      FTokenStrResolved := True;
    end;
  Result := FTokenStr;
end;

function ThtmlLexer.SkipSpace: Integer;
begin
  Result := FReader.SkipAll(htmlIsWhiteSpace);
end;

procedure ThtmlLexer.ExtractTextRef(const StrBuf: PWideChar;
    const StrLen: Integer);
begin
  Assert(StrLen > 0);
  FTokenStr := '';
  FTokenStrBuf := StrBuf;
  FTokenStrLen := StrLen;
  FTokenStrResolved := False;
  FReader.Skip(StrLen);
end;

function ThtmlLexer.ExtractNumber: String;
begin
  Result := FReader.ReadChars(htmlIsNumberChar);
end;

function ThtmlLexer.ExtractHexDigits: String;
begin
  Result := FReader.ReadChars(htmlIsHexDigit);
end;

procedure ThtmlLexer.ExtractNameRef(const C: WideChar);
var
  L : Integer;
  U : PWideChar;
begin
  if not htmlIsNameStartChar(C) then
    exit;
  L := FReader.MatchChars(htmlIsNameChar);
  Assert(L > 0);
  FReader.GetPeekBuffer(L, U);
  ExtractTextRef(U, L);
end;

procedure ThtmlLexer.ExtractToRef(const DelimStr: RawByteString; const SkipDelim: Boolean;
    const CaseSensitive: Boolean);
var
  L : Integer;
  U : PWideChar;
begin
  L := FReader.LocateRawByteStr(DelimStr, CaseSensitive, True);
  if L > 0 then
    begin
      FReader.GetPeekBuffer(L, U);
      ExtractTextRef(U, L);
    end;
  if SkipDelim then
    for L := 1 to Length(DelimStr) do
      if not FReader.EOF then
        FReader.Skip(1)
      else
        break;
end;

procedure ThtmlLexer.SetTokenText(const Text: String);
begin
  FTokenType := htText;
  FTokenStr := Text;
  FTokenStrBuf := nil;
  FTokenStrLen := 0;
  FTokenStrResolved := True;
end;

procedure ThtmlLexer.SetTokenTextRef(const StrBuf: PWideChar; const StrLen: Integer);
begin
  FTokenType := htText;
  ExtractTextRef(StrBuf, StrLen);
end;

procedure ThtmlLexer.ParseText(const InitialText: String);
const
  TextDelim     : ByteCharSet = ['<', '&'];
  TextLineDelim : ByteCharSet = ['<', '&', #10];
var P    : PByteCharSet;
    L    : Integer;
    U, Q : PWideChar;
    B    : Boolean;
begin
  // get text delimiter
  if FContext = lcTagAttrValueText then
    begin
      P := @FAttrDelim;
      B := False;
    end
  else
    begin
      B := not FNoLineBreakToken;
      if B then
        P := @TextLineDelim
      else
        P := @TextDelim;
    end;
  // locate text delimiter
  L := FReader.LocateRawByteChar(P^, True);
  // check line break
  // #10, #13#10 and #10#13 are returned as a line break;
  // all other #13 are handled like whitespace in text
  if B and (L > 0) then
    begin
      FReader.GetPeekBuffer(L, U);
      Q := U;
      Inc(Q, L - 1);
      if Q^ = WideCR then
        Dec(L);
    end;
  if L = 0 then
    SetTokenText(InitialText)
  else
    begin
      FReader.GetPeekBuffer(L, U);
      if InitialText = '' then
        SetTokenTextRef(U, L)
      else
        begin
          SetTokenText(InitialText + StrPToStrU(U, L));
          FReader.Skip(L);
        end;
    end;
end;

procedure ThtmlLexer.ParseEntity;
var C : WideChar;
    S : String;
begin
  if not FReader.SkipAndPeek(C) then
    begin
      SetTokenText('&');
      exit;
    end;
  if C = '#' then
    begin
      // Character reference
      if not FReader.SkipAndPeek(C) then
        begin
          SetTokenText('&#');
          exit;
        end;
      if C = 'x' then // hex
        begin
          S := ExtractHexDigits;
          if S = '' then
            begin
              ParseText('&#x');
              exit;
            end;
          FTokenType := htCharRefHex;
          FTokenStr := S;
        end
      else // decimal
        begin
          S := ExtractNumber;
          if S = '' then
            begin
              ParseText('&#');
              exit;
            end;
          FTokenType := htCharRef;
          FTokenStr := S;
        end;
    end
  else
  if htmlIsNameStartChar(C) then
    begin
      // Entity reference
      FTokenType := htEntityRef;
      ExtractNameRef(C);
    end
  else
    begin
      ParseText('&');
      exit;
    end;
end;

procedure ThtmlLexer.ParseCommentStart;
begin
  FReader.Skip(1);
  FContext := lcComment;
  ParseCommentText;
end;

procedure ThtmlLexer.ParseCommentText;
begin
  FTokenType := htComment;
  ExtractToRef('--', True, True);
  SkipSpace;
end;

procedure ThtmlLexer.ParseComment;
var C: WideChar;
begin
  FTokenStr := '';
  C := FReader.PeekChar;
  if C = '>' then
    FReader.Skip(1)
  else
  if C <> '-' then
    // invalid comment
    ExtractToRef('>', True, True)
  else
  if FReader.SkipAndPeek(C) then
    if C <> '-' then
      // invalid comment
      ExtractToRef('>', True, True)
    else
      begin
        // valid comment
        ParseCommentText;
        exit;
      end;
  FTokenType := htCommentEnd;
  FTokenStr := '';
  FTokenStrResolved := True;
  FContext := lcTop;
end;

procedure ThtmlLexer.ParseETag;
var C : WideChar;
begin
  if not FReader.SkipAndPeek(C) then
    begin
      SetTokenText('<!');
      exit;
    end;
  if C = '-' then // comment
    begin
      if not FReader.SkipAndPeek(C) then
        begin
          SetTokenText('<!-');
          exit;
        end;
      if C = '-' then
        ParseCommentStart
      else
        ParseText('<!-');
    end
  else
  if C = '>' then // empty comment <!>
    begin
      FReader.Skip(1);
      FTokenType := htEmptyComment;
      FTokenStr := '';
    end
  else
  if C = '[' then // marked section declaration (not allowed in HTML)
    begin
      FReader.Skip(1);
      SkipSpace;
      if FReader.MatchRawByteStr('CDATA[', False, True) then // CDATA section
        begin
          FTokenType := htCDATA;
          ExtractToRef(']]>', True, True);
        end
      else
        begin
          FTokenType := htMarkedSection;
          ExtractToRef(']>', True, True);
        end;
    end
  else
  if htmlIsNameStartChar(C) then // Markup declaration
    begin
      FTokenType := htDeclaration;
      ExtractNameRef(C);
      FContext := lcDeclaration;
    end
  else
    ParseText('<!');
end;

procedure ThtmlLexer.ParseDeclaration;
begin
  SkipSpace;
  FTokenType := htDeclarationText;
  FTokenStr := FReader.ReadToRawByteChar(['>'], True);
  FContext := lcTop;
end;

procedure ThtmlLexer.ParseQTag;
var C: WideChar;
begin
  if not FReader.SkipAndPeek(C) then
    begin
      SetTokenText('<?');
      exit;
    end;
  // Processing Instructions
  FTokenType := htPITarget;
  ExtractNameRef(C);
  FContext := lcPI;
end;

procedure ThtmlLexer.ParsePI;
begin
  SkipSpace;
  FTokenType := htPI;
  ExtractToRef('>', True, True);
  FContext := lcTop;
end;

procedure ThtmlLexer.ParseTagName;
const
  TagNameDelim = htmlWhiteSpace + ['/', '>', '='];
var
  I : Integer;
  P : PWideChar;
begin
  I := FReader.LocateRawByteChar(TagNameDelim, True);
  if I = 0 then // no name
    begin
      FTagID := HTML_TAG_None;
      FTokenStr := '';
      FTokenStrResolved := True;
    end
  else
    begin
      FReader.GetPeekBuffer(I, P);
      FTagID := htmlGetTagIDPtrW(P, I);
      if FTagID = HTML_TAG_None then // not a known HTML tag name
        begin
          FTokenStr := FReader.ReadUnicodeStr(I);
          FTokenStrResolved := True;
        end
      else
        begin // known HTML tag name
          FTokenStr := '';
          FTokenStrResolved := False;
          FReader.Skip(I);
        end;
    end;
end;

procedure ThtmlLexer.ParseStartTag;
begin
  FTokenType := htStartTag;
  FTagToken := htStartTag;
  ParseTagName;
  FContext := lcTagAttrName;
  SkipSpace;
end;

procedure ThtmlLexer.ParseEndTag;
var
  C : WideChar;
begin
  if not FReader.SkipAndPeek(C) then
    begin
      SetTokenText('</');
      exit;
    end;
  if not htmlIsNameStartChar(C) then // not an end tag
    begin
      ParseText('</');
      exit;
    end;
  FTokenType := htEndTag;
  FTagToken := htEndTag;
  ParseTagName;
  FContext := lcTagAttrName;
  SkipSpace;
end;

procedure ThtmlLexer.ParseAttrName;
const
  AttrNameDelim = htmlWhiteSpace + ['/', '>', '='];
var
  I : Integer;
  P : PWideChar;
begin
  I := FReader.LocateRawByteChar(AttrNameDelim, True);
  if I = 0 then // no name
    begin
      FAttrID := HTML_ATTR_None;
      FTokenStr := '';
      FTokenStrResolved := True;
    end
  else
    begin
      FReader.GetPeekBuffer(I, P);
      FAttrID := htmlGetAttrIDPtrW(P, I);
      if FAttrID = HTML_ATTR_None then // not a known HTML attribute name
        begin
          FTokenStr := FReader.ReadUnicodeStr(I);
          FTokenStrResolved := True;
        end
      else
        begin // known HTML attribute name
          FTokenStr := '';
          FTokenStrResolved := False;
          FReader.Skip(I);
        end;
    end;
end;

procedure ThtmlLexer.ParseTagAttrName(const C: WideChar);
begin
  if C = '>' then
    begin
      // end of start/end tag
      FReader.Skip(1);
      if (FTagToken = htStartTag) and (FTagID = HTML_TAG_SCRIPT) then
        FContext := lcScriptText
      else
        FContext := lcTop;
      FTokenStrResolved := True;
      ParseToken;
    end
  else
  if (C = '/') and FReader.MatchRawByteStr('/>', True, True) then // TODO: handle "/" without "/>" in tag attr name
    begin
      // end of empty tag
      FTokenType := htEmptyTag;
      FTokenStr := '';
      FTokenStrResolved := True;
      FContext := lcTop;
    end
  else
    begin
      // tag attribute name
      FTokenType := htTagAttrName;
      ParseAttrName;
      FContext := lcTagAttrValue;
      SkipSpace;
    end;
end;

procedure ThtmlLexer.ParseTagAttrValue;
const
  UnqoutedAttrDelim = htmlWhiteSpace + ['&', '>'];
  QuotedAttrDelim = ['&'];
var
  C : WideChar;
begin
  C := FReader.PeekChar;
  if C <> '=' then
    ParseTagAttrName(C)
  else
    begin
      FReader.Skip(1);
      FTokenType := htTagAttrValueStart;
      FTokenStr := '';
      SkipSpace;
      if FReader.EOF then
        exit;
      C := FReader.PeekChar;
      if (C = '''') or (C = '"') then
        begin
          FReader.Skip(1);
          FAttrQuote := AnsiChar(C);
          FAttrDelim := QuotedAttrDelim;
          Include(FAttrDelim, AnsiChar(C));
        end
      else
        begin
          FAttrQuote := #0;
          FAttrDelim := UnqoutedAttrDelim;
        end;
      FContext := lcTagAttrValueText;
    end;
end;

procedure ThtmlLexer.ParseTagAttrValueText;
var C : WideChar;
begin
  C := FReader.PeekChar;
  if (C = ';') and (FTokenType in htmlTokensRef) then
    begin
      FTokenType := htRefTrailer;
      FTokenStrResolved := True;
      FTokenStr := ';';
      FReader.Skip(1);
    end
  else
  if (Ord(C) > $FF) or not (AnsiChar(Ord(C)) in FAttrDelim) then
    ParseText('')
  else
  if C = '&' then
    ParseEntity
  else
    begin
      if (FAttrQuote <> #0) and (C = WideChar(FAttrQuote)) then
        FReader.Skip(1);
      FTokenType := htTagAttrValueEnd;
      FTokenStr := '';
      FContext := lcTagAttrName;
      SkipSpace;
    end;
end;

procedure ThtmlLexer.ParseScriptText;
begin
  ExtractToRef('</SCRIPT', False, False);
  FTokenType := htText;
  FContext := lcTop;
end;

procedure ThtmlLexer.ParseTag;
var C : WideChar;
begin
  if not FReader.SkipAndPeek(C) then
    begin
      SetTokenText('<');
      exit;
    end;
  case C of
    '!' : ParseETag;
    '?' : ParseQTag;
    '/' : ParseEndTag;
  else
    if htmlIsNameStartChar(C) then
      ParseStartTag
    else
      ParseText('<'); // not a tag
  end;
end;

procedure ThtmlLexer.ParseTop;
var C : WideChar;
begin
  C := FReader.PeekChar;
  case C of
    '&' : ParseEntity;
    '<' : ParseTag;
    ';' :
      if FTokenType in htmlTokensRef then
        begin
          FTokenType := htRefTrailer;
          FTokenStrResolved := True;
          FTokenStr := ';';
          FReader.Skip(1);
        end
      else
        ParseText('');
    #10 :
      if FNoLineBreakToken then
        ParseText('')
      else
        begin
          FTokenType := htLineBreak;
          FTokenStrResolved := True;
          if not FReader.SkipAndPeek(C) then
            FTokenStr := #10
          else
            if C <> #13 then
              FTokenStr := #10
            else
              begin
                FReader.Skip(1);
                FTokenStr := #10#13;
              end;
        end;
    #13 :
      if FNoLineBreakToken then
        ParseText('')
      else
        if not FReader.SkipAndPeek(C) then
          SetTokenText(#13)
        else
          if C <> #10 then
            ParseText(#13)
          else
            begin
              FReader.Skip(1);
              FTokenType := htLineBreak;
              FTokenStr := #13#10;
              FTokenStrResolved := True;
            end;
  else
    ParseText('');
  end;
end;

procedure ThtmlLexer.ParseToken;
begin
  if FReader.EOF then
    begin
      FTokenType := htEOF;
      FTokenStr := '';
    end
  else
    case FContext of
      lcTop              : ParseTop;
      lcTagAttrName      : ParseTagAttrName(FReader.PeekChar);
      lcTagAttrValue     : ParseTagAttrValue;
      lcTagAttrValueText : ParseTagAttrValueText;
      lcComment          : ParseComment;
      lcPI               : ParsePI;
      lcDeclaration      : ParseDeclaration;
      lcScriptText       : ParseScriptText;
    end;
end;

function ThtmlLexer.GetNextToken: ThtmlTokenType;
begin
  ParseToken;
  Result := FTokenType;
  Inc(FTokenCount);
end;

function ThtmlLexer.CharRefValue: LongWord;
begin
  Assert(FTokenType in htmlTokensCharRef);
  case FTokenType of
    htCharRefHex : Result := HexToWord32U(FTokenStr);
    htCharRef    : Result := StringToWord32U(FTokenStr);
  else
    Result := 0;
  end;
end;

function ThtmlLexer.ResolveCharRef(var Text: String): Boolean;
var CharVal : LongWord;
begin
  Assert(FTokenType in htmlTokensCharRef);
  CharVal := CharRefValue;
  Result := CharVal <= $1FFFFF; // Unicode character
  if Result then
    Text := UTF8StringToUnicodeString(UCS4CharToUTF8String(UCS4Char(CharVal)))
  else
    Text := '';
end;

function ThtmlLexer.ResolveEntityRef(var Text: String): Boolean;
var CharVal : Word;
begin
  Assert(FTokenType = htEntityRef);
  CharVal := htmlDecodeCharEntity(GetTokenStr);
  Result := CharVal <> 0;
  if Result then
    Text := UTF8StringToUnicodeString(UCS4CharToUTF8String(UCS4Char(CharVal)))
  else
    Text := '';
end;

function ThtmlLexer.ResolveReference(var Text: String): Boolean;
begin
  Assert(FTokenType in htmlTokensRef);
  Text := '';
  case FTokenType of
    htEntityRef  : Result := ResolveEntityRef(Text);
    htCharRef,
    htCharRefHex : Result := ResolveCharRef(Text);
  else
    Result := False;
  end;
end;



{                                                                              }
{ TfndHtmlLexicalParser                                                        }
{                                                                              }
constructor ThtmlLexicalParser.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOptions := [];
end;

destructor ThtmlLexicalParser.Destroy;
begin
  FreeAndNil(FLexer);
  inherited Destroy;
end;

procedure ThtmlLexicalParser.SetOptions(const Options: ThtmlLexicalParserOptions);
begin
  if Options = FOptions then
    exit;
  FOptions := Options;
  if Assigned(FLexer) then
    FLexer.NoLineBreakToken := loNoLineBreakToken in FOptions;
end;

function ThtmlLexicalParser.GetTokenCount: Integer;
begin
  if Assigned(FLexer) then
    Result := FLexer.TokenCount
  else
    Result := 0;
end;

function ThtmlLexicalParser.GetTokenTypeDescription: String;
begin
  Result := htmlTokenTypeDescription(FTokenType);
end;

function ThtmlLexicalParser.GetTokenTypeIDStr: String;
begin
  Result := htmlTokenTypeIDStr(FTokenType);
end;

function ThtmlLexicalParser.GetTokenStr: String;
begin
  if not FHasTokenStr then
    begin
      if Assigned(FLexer) then
        FTokenStr := FLexer.TokenStr
      else
        FTokenStr := '';
      FHasTokenStr := True;
    end;
  Result := FTokenStr;
end;

procedure ThtmlLexicalParser.Reset;
var Reader: TUnicodeReader;
begin
  // Create new lexer
  FreeAndNil(FLexer);
  if FFileName <> '' then
    Reader := htmlGetDocumentReaderForFile(FFileName, FEncoding)
  else
    Reader := htmlGetDocumentReaderForRawString(FText, FEncoding);
  FLexer := ThtmlLexer.Create(Reader, True);
  FLexer.NoLineBreakToken := loNoLineBreakToken in FOptions;
  // Initialize state
  FAborted := False;
  FTokenType := htNone;
  FTokenStr := '';
  FHasTokenStr := True;
  FTagID := HTML_TAG_None;
  FAttrID := HTML_ATTR_None;
end;

procedure ThtmlLexicalParser.Abort;
begin
  FAborted := True;
end;

procedure ThtmlLexicalParser.GetNextToken;
var S      : String;
    Notify : Boolean;
begin
  if not Assigned(FLexer) then
    Reset;
  Notify := not (loDisableNotifications in FOptions);
  // Get token
  FTokenType := FLexer.GetNextToken;
  FHasTokenStr := False;
  // Process specific tokens
  case FTokenType of
    htStartTag, htEndTag:
      FTagID := FLexer.TagID;
    htTagAttrName:
      FAttrID := FLexer.AttrID;
    htCharRef,
    htCharRefHex,
    htEntityRef:
      // Resolve reference
      if loResolveReferences in FOptions then
        if FLexer.ResolveReference(S) then
          begin
            // resolved: return token htText instead of the reference token
            FTokenType := htText;
            FTokenStr := S;
            FHasTokenStr := True;
          end;
    htTagAttrValueStart:
      begin
        FInAttributeValue := True;
        FGetAttributeValue := Notify and Assigned(FOnTagAttrValue);
        FAttributeValue := '';
      end;
    htTagAttrValueEnd:
      FInAttributeValue := False;
  end;
  // Collect attribute value
  if FInAttributeValue and FGetAttributeValue then
    case FTokenType of
      htText       : FAttributeValue := FAttributeValue + TokenStr;
      htCharRef    : FAttributeValue := FAttributeValue + '&#' + TokenStr + ';';
      htCharRefHex : FAttributeValue := FAttributeValue + '&#x' + TokenStr + ';';
      htEntityRef  : FAttributeValue := FAttributeValue + '&' + TokenStr + ';'
    end;
  // Do notifications
  if Notify then
    begin
      // Notify token
      if Assigned(FOnToken) then
        FOnToken(self, FTokenType);
      if FAborted then
        exit;
      // Notify token string
      if Assigned(FOnTokenStr) then
        FOnTokenStr(self, FTokenType, TokenStr);
      if FAborted then
        exit;
      // Notify specific tokens
      case FTokenType of
        htStartTag:
          begin
            if Assigned(FOnStartTag) then
              FOnStartTag(self, FTagID);
            if Assigned(FOnStartTagStr) then
              FOnStartTagStr(self, FTagID, TokenStr);
          end;
        htEndTag:
          begin
            if Assigned(FOnEndTag) then
              FOnEndTag(self, FTagID);
            if Assigned(FOnEndTagStr) then
              FOnEndTagStr(self, FTagID, TokenStr);
          end;
        htText:
          begin
            if Assigned(FOnText) then
              FOnText(self, TokenStr);
            if not FInAttributeValue then
              begin
                if Assigned(FOnContentText) then
                  FOnContentText(self, TokenStr);
              end;
          end;
        htTagAttrName:
          begin
            if Assigned(FOnTagAttr) then
              FOnTagAttr(self, FTagID, FAttrID);
            if Assigned(FOnTagAttrStr) then
              FOnTagAttrStr(self, FTagID, FAttrID, TokenStr);
          end;
        htTagAttrValueEnd:
          begin
            if Assigned(FOnTagAttrValue) then
              FOnTagAttrValue(self, FTagID, FAttrID, FAttributeValue);
          end;
        htComment:
          begin
            if Assigned(FOnComment) then
              FOnComment(self, TokenStr);
          end;
      end;
    end;
end;

procedure ThtmlLexicalParser.Parse;
begin
  Reset;
  repeat
    GetNextToken;
  until FAborted or (FTokenType = htEOF);
end;



{                                                                              }
{ Tests                                                                        }
{                                                                              }
{$IFDEF HTML_TEST}
{$ASSERTIONS ON}
procedure Test;
var
  S : RawByteString;
  R : TUnicodeLongStringReader;
  L : ThtmlLexer;
begin
  S := '<HTML><BODY>' +
       'Test&amp;&lt&gt' +
       '<script>n;&amp;<test></script>' +
       '<?exec abc&amp; abs<abc>' +
       '<b style="s1&lt"></b>' +
       '<br/>' +
       '<!--Comment;&amp;1<abc>-->' +
       '</BODY></HTML>';
  R := TUnicodeLongStringReader.Create(S, TUTF8Codec.Create, True);
  L := ThtmlLexer.Create(R, True);
  Assert(L.GetNextToken = htStartTag);
  Assert(L.TokenStr = 'HTML');
  Assert(L.GetNextToken = htStartTag);
  Assert(L.TokenStr = 'BODY');
  Assert(L.GetNextToken = htText);
  Assert(L.TokenStr = 'Test');
  Assert(L.GetNextToken = htEntityRef);
  Assert(L.TokenStr = 'amp');
  Assert(L.GetNextToken = htRefTrailer);
  Assert(L.TokenStr = ';');
  Assert(L.GetNextToken = htEntityRef);
  Assert(L.TokenStr = 'lt');
  Assert(L.GetNextToken = htEntityRef);
  Assert(L.TokenStr = 'gt');
  Assert(L.GetNextToken = htStartTag);
  Assert(L.TokenStr = 'SCRIPT');
  Assert(L.GetNextToken = htText);
  Assert(L.TokenStr = 'n;&amp;<test>');
  Assert(L.GetNextToken = htEndTag);
  Assert(L.TokenStr = 'SCRIPT');
  Assert(L.GetNextToken = htPITarget);
  Assert(L.TokenStr = 'exec');
  Assert(L.GetNextToken = htPI);
  Assert(L.TokenStr = 'abc&amp; abs<abc');
  Assert(L.GetNextToken = htStartTag);
  Assert(L.TokenStr = 'B');
  Assert(L.GetNextToken = htTagAttrName);
  Assert(L.TokenStr = 'STYLE');
  Assert(L.GetNextToken = htTagAttrValueStart);
  Assert(L.TokenStr = '');
  Assert(L.GetNextToken = htText);
  Assert(L.TokenStr = 's1');
  Assert(L.GetNextToken = htEntityRef);
  Assert(L.TokenStr = 'lt');
  Assert(L.GetNextToken = htTagAttrValueEnd);
  Assert(L.TokenStr = '');
  Assert(L.GetNextToken = htEndTag);
  Assert(L.TokenStr = 'B');
  Assert(L.GetNextToken = htStartTag);
  Assert(L.TokenStr = 'BR');
  Assert(L.GetNextToken = htEmptyTag);
  Assert(L.TokenStr = '');
  Assert(L.GetNextToken = htComment);
  Assert(L.TokenStr = 'Comment;&amp;1<abc>');
  Assert(L.GetNextToken = htCommentEnd);
  Assert(L.TokenStr = '');
  Assert(L.GetNextToken = htEndTag);
  Assert(L.TokenStr = 'BODY');
  Assert(L.GetNextToken = htEndTag);
  Assert(L.TokenStr = 'HTML');
  Assert(L.GetNextToken = htEOF);
  L.Free;
end;
{$ENDIF}



end.

