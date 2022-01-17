{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals 5.00                                        }
{   File name:        flcXMLFunctions.pas                                      }
{   File version:     5.10                                                     }
{   Description:      XML functions                                            }
{                                                                              }
{   Copyright:        Copyright (c) 2000-2019, David J Butler                  }
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
{   2000/05/11  1.01  Created cXML from cInternetStandards.                    }
{   2002/02/14  2.02  Fixed bug in xmlValidEncName. Thanks to Thomas Jensen    }
{                     <tjensen@xs4all.nl> for finding it.                      }
{   2002/04/17  2.03  Created cXMLFunctions from cXML.                         }
{   2002/04/26  2.04  Unicode support.                                         }
{   2003/09/07  3.05  Revised for Fundamentals 3.                              }
{   2004/02/21  3.06  Added xmlResolveEntityReference function.                }
{   2004/04/01  3.07  Compilable with FreePascal 1.9.2 Win32 i386.             }
{   2005/12/07  4.08  Compilable with FreePascal 2.0.1 Linux i386.             }
{   2019/04/28  5.09  String type changes.                                     }
{   2019/04/28  5.10  Revised for Fundamentals 5.                              }
{                                                                              }
{******************************************************************************}

{$INCLUDE flcXML.inc}

unit flcXMLFunctions;

interface

uses
  { System }
  SysUtils,

  { Fundamentals }
  flcStdTypes,
  flcUnicodeCodecs,
  flcZeroTermStrings;



{                                                                              }
{ Constants                                                                    }
{                                                                              }
const
  xmlVersion = '1.0';



{                                                                              }
{ Exception                                                                    }
{                                                                              }
type
  Exml = class(Exception);



{                                                                              }
{ Decoding                                                                     }
{                                                                              }
function  xmlValidChar(const Ch: AnsiChar): Boolean; overload;
function  xmlValidChar(const Ch: UCS4Char): Boolean; overload;
function  xmlValidChar(const Ch: WideChar): Boolean; overload;

function  xmlIsSpaceChar(const Ch: WideChar): Boolean;
function  xmlIsLetter(const Ch: WideChar): Boolean;
function  xmlIsDigit(const Ch: WideChar): Boolean;

function  xmlIsNameStartChar(const Ch: WideChar): Boolean;
function  xmlIsNameChar(const Ch: WideChar): Boolean;
function  xmlIsPubidChar(const Ch: WideChar): Boolean;
function  xmlValidName(const Text: UnicodeString): Boolean;

const
  xmlSpace = [#$20, #$9, #$D, #$A];

function  xmlSkipSpace(var P: PWideChar): Boolean;
function  xmlSkipEq(var P: PWideChar): Boolean;
function  xmlExtractQuotedText(var P: PWideChar; var S: UnicodeString): Boolean;

{ xmlGetEntityEncoding                                                         }
{   Buf is a pointer to the first part of an xml entity. It must include       }
{   the xml declaration. HeaderSize returns the number of bytes offset in      }
{   Buf to the actual xml.                                                     }
function  xmlGetEntityEncoding(const Buf: Pointer; const BufSize: Integer;
          out HeaderSize: Integer): TUnicodeCodecClass;

function  xmlResolveEntityReference(const RefName: UnicodeString): WideChar;



{                                                                              }
{ Encoding                                                                     }
{                                                                              }
function  xmlTag(const Tag: UnicodeString): UnicodeString;
function  xmlEndTag(const Tag: UnicodeString): UnicodeString;
function  xmlAttrTag(const Tag: UnicodeString;
          const Attr: UnicodeString = ''): UnicodeString;
function  xmlEmptyTag(const Tag, Attr: UnicodeString): UnicodeString;
procedure xmlSafeTextInPlace(var Txt: UnicodeString);
function  xmlSafeText(const Txt: UnicodeString): UnicodeString;
function  xmlSpaceIndent(const IndentLength: Integer;
          const IndentLevel: Integer): UnicodeString;
function  xmlTabIndent(const IndentLevel: Integer): UnicodeString;
function  xmlComment(const Comment: UnicodeString): UnicodeString;



{                                                                              }
{ Test cases                                                                   }
{                                                                              }
{$IFDEF XML_TEST}
procedure Test;
{$ENDIF}



implementation

uses
  { Fundamentals }
  flcUtils,
  flcStrings,
  flcUnicodeChar;



{                                                                              }
{ Decoding                                                                     }
{                                                                              }

{   [2]   Char ::=  #x9 | #xA | #xD | [#x20-#xD7FF] | [#xE000-#xFFFD] |        }
{                   [#x10000-#x10FFFF]                                         }
function xmlValidChar(const Ch: AnsiChar): Boolean;
begin
  Result := Ch in [#$9, #$A, #$D, #$20..#$FF];
end;

function xmlValidChar(const Ch: UCS4Char): Boolean;
begin
  if Ch <= $D7FF then
    if Ch >= $20 then
      Result := True
    else
      Result := Ch in [$9, $A, $D]
  else
    if Ch > $10FFFF then
      Result := False
    else
      case Ch of
        $D800..$DFFF,
        $FFFE..$FFFF : Result := False;
      else
        Result := True;
      end;
end;

function xmlValidChar(const Ch: WideChar): Boolean;
begin
  if Ord(Ch) <= $D7FF then
    if Ord(Ch) >= $20 then
      Result := True
    else
      Result := Byte(Ord(Ch)) in [$9, $A, $D]
  else
    case Ch of
      #$D800..#$DFFF,
      #$FFFE..#$FFFF : Result := False;
    else
      Result := True;
    end;
end;

{   [3]   S ::=  (#x20 | #x9 | #xD | #xA)+                                     }
function xmlIsSpaceChar(const Ch: WideChar): Boolean;
begin
  if Ord(Ch) > $20 then
    Result := False
  else
    Result := Byte(Ch) in [$20, $09, $0D, $0A];
end;

{   [84]  Letter ::=  BaseChar | Ideographic                                   }
function xmlIsLetter(const Ch: WideChar): Boolean;
begin
  Result := UnicodeIsLetter(Ch);
end;

{   [88]  Digit ::=  [#x0030-#x0039] | [#x0660-#x0669] | [#x06F0-#x06F9] |     }
{                    [#x0966-#x096F] | [#x09E6-#x09EF] | [#x0A66-#x0A6F] |     }
{                    [#x0AE6-#x0AEF] | [#x0B66-#x0B6F] | [#x0BE7-#x0BEF] |     }
{                    [#x0C66-#x0C6F] | [#x0CE6-#x0CEF] | [#x0D66-#x0D6F] |     }
{                    [#x0E50-#x0E59] | [#x0ED0-#x0ED9] | [#x0F20-#x0F29]       }
function xmlIsDigit(const Ch: WideChar): Boolean;
begin
  Result := UnicodeIsDecimalDigit(Ch);
end;


{   [4]   NameChar ::=  Letter | Digit | '.' | '-' | '_' | ':' |               }
{                 CombiningChar | Extender                                     }
function xmlIsNameChar(const Ch: WideChar): Boolean;
begin
  Result := xmlIsLetter(Ch) or xmlIsDigit(Ch);
  if Result then
    exit;
  case Ch of
    '.', '-', '_', ':' : Result := True;
  end;
end;

{   [5]   Name ::=  (Letter | '_' | ':') (NameChar)*                           }
function xmlIsNameStartChar(const Ch: WideChar): Boolean;
begin
  Result := xmlIsLetter(Ch) or (Ch = '_') or (Ch = ':');
end;

{   [13]  PubidChar ::=  #x20 | #xD | #xA | [a-zA-Z0-9] |                      }
{                        [-'()+,./:=?;!*#@$_%]                                 }
function xmlIsPubidChar(const Ch: WideChar): Boolean;
begin
  case Ch of
    ' ', #$D, #$A, 'A'..'Z', 'a'..'z', '0'..'9',
    '-', '''', '(', ')', '+', ',', '.', '/', ':',
    '=', '?', ';', '!', '*', '#', '@', '$', '_', '%' :
      Result := True;
  else
    Result := False;
  end;
end;

function xmlValidName(const Text: UnicodeString): Boolean;
var P : PWideChar;
    L : Integer;
begin
  Result := False;
  P := Pointer(Text);
  L := Length(Text);
  if not Assigned(P) or (L = 0) then
    exit;
  if not xmlIsNameStartChar(P^) then
    exit;
  Inc(P);
  Dec(L);
  Result := StrPMatchCharW(P, L, xmlIsNameChar);
end;

{   [3]   S ::=  (#x20 | #x9 | #xD | #xA)+                                     }
function xmlSkipSpace(var P: PWideChar): Boolean;
begin
  Assert(Assigned(P));
  Result := StrZSkipAllW(P, xmlIsSpaceChar) > 0;
end;

{   [25]  Eq ::=  S? '=' S?                                                    }
function xmlSkipEq(var P: PWideChar): Boolean;
var Q : PWideChar;
begin
  Q := P;
  xmlSkipSpace(Q);
  Result := Ord(Q^) = Ord('=');
  if not Result then
    exit;
  Inc(Q);
  xmlSkipSpace(Q);
  P := Q;
end;

{   [15]  Comment ::=  '<!--' ((Char - '-') | ('-' (Char - '-')))* '-->'       }
function xmlSkipComment(var P: PWideChar): Boolean;
var I : Integer;
begin
  Result := StrZMatchStrAsciiBW(P, '<!--', True);
  if not Result then
    exit;
  I := StrZPosBW('-->', P);
  Result := I >= 0;
  if not Result then
    exit;
  Inc(P, I + 3);
end;

{   [..]  QuotedText  ::=  ' Text ' | " Text "                                 }
function xmlExtractQuotedText(var P: PWideChar; var S: UnicodeString): Boolean;
begin
  Assert(Assigned(P));
  Result := StrZExtractQuotedU(P, S, ['''']);
  if Result then
    exit;
  Result := StrZExtractQuotedU(P, S, ['"']);
end;

{ Entity encoding                                                              }
{   [23]  XMLDecl ::=  '<?xml' VersionInfo EncodingDecl? SDDecl? S? '?>'       }
{   [24]  VersionInfo ::=  S 'version' Eq (' VersionNum ' | " VersionNum ")    }
{   [26]  VersionNum ::=  ([a-zA-Z0-9_.:] | '-')+                              }
{   [80]  EncodingDecl ::=  S 'encoding' Eq ('"' EncName '"' |                 }
{         "'" EncName "'" )                                                    }
function xmlGetEntityEncoding(const Buf: Pointer; const BufSize: Integer;
    out HeaderSize: Integer): TUnicodeCodecClass;
var S    : UnicodeString;
    R    : PByteChar;
    P, Q : PWideChar;
    L    : Integer;
begin
  R := Buf;
  L := BufSize;
  // Detect Unicode markings
  Result := DetectUTFEncoding(R, L, HeaderSize);
  if Assigned(Result) then
    exit;
  HeaderSize := 0;
  Result := TUTF8Codec;
  // Check if first character is a likely XML UTF-16 character
  if L >= 2 then
    begin
      P := Pointer(R);
      case Ord(P^) of
        $0009, $000A, $000D, $0020, $003C : Result := TUTF16LECodec;
        $0900, $0A00, $0D00, $2000, $3C00 : Result := TUTF16BECodec;
      end;
    end;
  if L < 16 then
    exit;
  // Decode
  S := EncodingToUTF16U(Result, R, MinInt(L, 4096));
  // Locate XML declaration
  P := Pointer(S);
  while xmlSkipSpace(P) or xmlSkipComment(P) do ;
  if not StrZSkipStrBW(P, '<?xml', False) then
    exit;
  if not xmlSkipSpace(P) then
    exit;
  // Locate encoding attribute
  while (Ord(P^) <> 0) and (P^ <> '>') do
    begin
      if xmlIsSpaceChar(P^) then
        begin
          Q := P;
          Inc(Q);
          if StrZSkipStrBW(Q, 'encoding', False) then
            if xmlSkipEq(Q) then
              begin
                // Extract encoding
                if not xmlExtractQuotedText(Q, S) then
                  S := StrZExtractToU(Q, ['>', #0] + xmlSpace);
                StrTrimInPlaceU(S, UnicodeIsWhiteSpace);
                if S <> '' then
                  // Get codec type from encoding name
                  begin
                    Result := GetCodecClassByAlias(S);
                  end;
                // Found encoding attribute
                exit;
              end;
        end;
      // Next character
      Inc(P);
    end;
end;

function xmlResolveEntityReference(const RefName: UnicodeString): WideChar;
begin
  if StrEqualU(RefName, 'amp', True) then
    Result := '&'
  else
  if StrEqualU(RefName, 'lt', True) then
    Result := '<'
  else
  if StrEqualU(RefName, 'gt', True) then
    Result := '>'
  else
  if StrEqualU(RefName, 'quot', True) then
    Result := '"'
  else
  if StrEqualU(RefName, 'apos', True) then
    Result := ''''
  else
    Result := WideChar(#0);
end;



{                                                                              }
{ Encoding                                                                     }
{                                                                              }
function xmlTag(const Tag: UnicodeString): UnicodeString;
begin
  Result := '<' + Tag + '>';
end;

function xmlAttrTag(const Tag: UnicodeString; const Attr: UnicodeString): UnicodeString;
begin
  if Attr = '' then
    Result := xmlTag(Tag)
  else
    Result := '<' + Tag + ' ' + Attr + '>';
end;

function xmlEndTag(const Tag: UnicodeString): UnicodeString;
begin
  Result := '</' + Tag + '>';
end;

function xmlEmptyTag(const Tag, Attr: UnicodeString): UnicodeString;
begin
  if Attr = '' then
    Result := '<' + Tag + '/>'
  else
    Result := '<' + Tag + ' ' + Attr + '/>';
end;

procedure xmlSafeTextInPlace(var Txt: UnicodeString);
begin
  if PosCharSetU([#0, '&', '''', '"', '>', '<'], Txt) <= 0 then
    exit;
  Txt := StrReplaceCharStrU(#0, '', Txt);
  Txt := StrReplaceCharStrU('&', '&amp;', Txt);
  Txt := StrReplaceCharStrU('''', '&apos;', Txt);
  Txt := StrReplaceCharStrU('"', '&quot;', Txt);
  Txt := StrReplaceCharStrU('>', '&gt;', Txt);
  Txt := StrReplaceCharStrU('<', '&lt;', Txt);
end;

function xmlSafeText(const Txt: UnicodeString): UnicodeString;
begin
  Result := Txt;
  xmlSafeTextInPlace(Result);
end;

function xmlSpaceIndent(const IndentLength: Integer; const IndentLevel: Integer): UnicodeString;
begin
  Result := DupCharU(#32, IndentLevel * IndentLength);
end;

function xmlTabIndent(const IndentLevel: Integer): UnicodeString;
begin
  Result := DupCharU(#9, IndentLevel);
end;

{   [15]  Comment ::=  '<!--' ((Char - '-') | ('-' (Char - '-')))* '-->'       }
function xmlComment(const Comment: UnicodeString): UnicodeString;
begin
  Assert(PosStrU('--', Comment) = 0, 'Invalid sequence in comment');
  Result := '<!--' + Comment + '-->';
end;



{                                                                              }
{ Test cases                                                                   }
{                                                                              }
{$IFDEF XML_TEST}
{$ASSERTIONS ON}
procedure Test;
var S : RawByteString;
    T : UnicodeString;
    I : Integer;
begin
  Assert(xmlValidChar(WideChar(#32)), 'xmlValidChar');
  Assert(xmlValidChar(WideChar(#13)), 'xmlValidChar');
  Assert(not xmlValidChar(WideChar(#0)), 'xmlValidChar');
  Assert(not xmlValidChar(WideChar(#11)), 'xmlValidChar');
  Assert(xmlValidName('_Test_0'), 'xmlValidName');
  Assert(not xmlValidName('X '), 'xmlValidName');
  Assert(not xmlValidName('X$'), 'xmlValidName');
  Assert(not xmlValidName('5X'), 'xmlValidName');
  Assert(xmlTag('Test') = '<Test>', 'xmlTag');
  Assert(xmlComment('Test') = '<!--Test-->', 'xmlComment');
  Assert(xmlSafeText('(abc) [123]') = '(abc) [123]', 'xmlSafeText');
  Assert(xmlSafeText('a&<b>') = 'a&amp;&lt;b&gt;', 'xmlSafeText');
  Assert(xmlIsSpaceChar(#32), 'xmlIsSpaceChar');
  Assert(xmlIsSpaceChar(#13), 'xmlIsSpaceChar');
  Assert(not xmlIsSpaceChar('_'), 'xmlIsSpaceChar');
  S := '<?xml version="1.0" encoding="ascii">';
  Assert(xmlGetEntityEncoding(Pointer(S), Length(S), I) = TUSASCIICodec, 'xmlGetEntityEncoding');
  S := '  <?xml  attr  encoding=ISO-8859-10 >  ';
  Assert(xmlGetEntityEncoding(Pointer(S), Length(S), I) = TISO8859_10Codec, 'xmlGetEntityEncoding');
  T := '<?xml version="1.0">';
  Assert(xmlGetEntityEncoding(Pointer(T), Length(T) * SizeOf(WideChar), I) = TUTF16LECodec, 'xmlGetEntityEncoding');
end;
{$ENDIF}



end.

