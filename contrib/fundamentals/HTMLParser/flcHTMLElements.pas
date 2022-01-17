{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals 5.00 - HTML Parser                          }
{   File name:        flcHTMLElements.pas                                      }
{   File version:     5.08                                                     }
{   Description:      HTML elements                                            }
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
{   2002/11/03  1.00  Part of cHTMLUtils                                       }
{                     ThtmlTagID and ThtmlAttrID.                              }
{   2002/11/04  1.01  ThtmlcssPropertyID.                                      }
{   2002/12/08  1.02  Created cHTMLConsts unit.                                }
{   2012/12/16  1.03  HTML 5.1 tags.                                           }
{   2015/04/04  1.04  RawByteString changes.                                   }
{   2015/04/11  1.05  UnicodeString changes.                                   }
{   2019/02/21  5.06  Revised for Fundamentals 5.                              }
{   2019/02/22  5.07  Part of flcHTMLElements.                                 }
{   2019/10/03  5.08  Fix AnsiChar lookups.                                    }
{                                                                              }
{******************************************************************************}

{$INCLUDE flcHTML.inc}

unit flcHTMLElements;

interface

uses
  { Fundamentals }
  flcStdTypes;



{                                                                              }
{ HTML Tags                                                                    }
{                                                                              }
type
  ThtmlTagID = (
      // Special tag IDs
      HTML_TAG_None,
      HTML_TAG_Document,
      // Element tags
      // From HTML 4.01
      HTML_TAG_A,
      HTML_TAG_ABBR,
      HTML_TAG_ACRONYM,
      HTML_TAG_ADDRESS,
      HTML_TAG_APPLET,
      HTML_TAG_AREA,
      HTML_TAG_B,
      HTML_TAG_BASE,
      HTML_TAG_BASEFONT,
      HTML_TAG_BDO,
      HTML_TAG_BIG,
      HTML_TAG_BLOCKQUOTE,
      HTML_TAG_BODY,
      HTML_TAG_BR,
      HTML_TAG_BUTTON,
      HTML_TAG_CAPTION,
      HTML_TAG_CENTER,
      HTML_TAG_CITE,
      HTML_TAG_CODE,
      HTML_TAG_COL,
      HTML_TAG_COLGROUP,
      HTML_TAG_DD,
      HTML_TAG_DEL,
      HTML_TAG_DFN,
      HTML_TAG_DIR,
      HTML_TAG_DIV,
      HTML_TAG_DL,
      HTML_TAG_DT,
      HTML_TAG_EM,
      HTML_TAG_FIELDSET,
      HTML_TAG_FONT,
      HTML_TAG_FORM,
      HTML_TAG_FRAME,
      HTML_TAG_FRAMESET,
      HTML_TAG_H1,
      HTML_TAG_H2,
      HTML_TAG_H3,
      HTML_TAG_H4,
      HTML_TAG_H5,
      HTML_TAG_H6,
      HTML_TAG_HEAD,
      HTML_TAG_HR,
      HTML_TAG_HTML,
      HTML_TAG_I,
      HTML_TAG_IFRAME,
      HTML_TAG_IMG,
      HTML_TAG_INPUT,
      HTML_TAG_INS,
      HTML_TAG_ISINDEX,
      HTML_TAG_KBD,
      HTML_TAG_LABEL,
      HTML_TAG_LEGEND,
      HTML_TAG_LI,
      HTML_TAG_LINK,
      HTML_TAG_MAP,
      HTML_TAG_MENU,
      HTML_TAG_META,
      HTML_TAG_NOFRAMES,
      HTML_TAG_NOSCRIPT,
      HTML_TAG_OBJECT,
      HTML_TAG_OL,
      HTML_TAG_OPTGROUP,
      HTML_TAG_OPTION,
      HTML_TAG_P,
      HTML_TAG_PARAM,
      HTML_TAG_PRE,
      HTML_TAG_Q,
      HTML_TAG_S,
      HTML_TAG_SAMP,
      HTML_TAG_SCRIPT,
      HTML_TAG_SELECT,
      HTML_TAG_SMALL,
      HTML_TAG_SPAN,
      HTML_TAG_STRIKE,
      HTML_TAG_STRONG,
      HTML_TAG_STYLE,
      HTML_TAG_SUB,
      HTML_TAG_SUP,
      HTML_TAG_TABLE,
      HTML_TAG_TBODY,
      HTML_TAG_TD,
      HTML_TAG_TEXTAREA,
      HTML_TAG_TFOOT,
      HTML_TAG_TH,
      HTML_TAG_THEAD,
      HTML_TAG_TITLE,
      HTML_TAG_TR,
      HTML_TAG_TT,
      HTML_TAG_U,
      HTML_TAG_UL,
      HTML_TAG_VAR,
      // From HTML 5.1
      HTML_TAG_ARTICLE,
      HTML_TAG_ASIDE,
      HTML_TAG_AUDIO,
      HTML_TAG_BDI,
      HTML_TAG_CANVAS,
      HTML_TAG_COMMAND,
      HTML_TAG_DATALIST,
      HTML_TAG_DETAILS,
      HTML_TAG_DIALOG,
      HTML_TAG_FIGCAPTION,
      HTML_TAG_FIGURE,
      HTML_TAG_FOOTER,
      HTML_TAG_HEADER,
      HTML_TAG_HGROUP,
      HTML_TAG_KEYGEN,
      HTML_TAG_MARK,
      HTML_TAG_METER,
      HTML_TAG_NAV,
      HTML_TAG_OUTPUT,
      HTML_TAG_PROGRESS,
      HTML_TAG_SECTION,
      HTML_TAG_SOURCE,
      HTML_TAG_SUMMARY,
      HTML_TAG_TIME,
      HTML_TAG_TRACK,
      HTML_TAG_VIDEO,
      HTML_TAG_WBR
    );

const
  HTML_TAG_FirstID  = HTML_TAG_A;
  HTML_TAG_LastID   = High(ThtmlTagID);
  HTML_TAG_MaxIndex = Ord(High(ThtmlTagID));

function  htmlGetTagName(const TagID: ThtmlTagID): String; overload;
function  htmlGetTagName(const TagID: ThtmlTagID; const Name: String): String; overload;
function  htmlGetTagIDPtrA(const Name: PAnsiChar; const NameLen: Integer): ThtmlTagID;
function  htmlGetTagIDPtrW(const Name: PWideChar; const NameLen: Integer): ThtmlTagID;
function  htmlGetTagIDStrB(const Name: RawByteString): ThtmlTagID;
function  htmlGetTagIDStr(const Name: String): ThtmlTagID;
function  htmlIsSameTag(const TagID1: ThtmlTagID; const Name1: String;
          const TagID2: ThtmlTagID; const Name2: String): Boolean;



{                                                                              }
{ HTML Element Attributes                                                      }
{                                                                              }
type
  ThtmlAttrID = (
      // Special attribute IDs
      HTML_ATTR_None,
      // Attribute name IDs
      HTML_ATTR_ABBR,
      HTML_ATTR_ACCEPT_CHARSET,
      HTML_ATTR_ACCEPT,
      HTML_ATTR_ACCESSKEY,
      HTML_ATTR_ACTION,
      HTML_ATTR_ALIGN,
      HTML_ATTR_ALINK,
      HTML_ATTR_ALT,
      HTML_ATTR_ARCHIVE,
      HTML_ATTR_AXIS,
      HTML_ATTR_BACKGROUND,
      HTML_ATTR_BGCOLOR,
      HTML_ATTR_BORDER,
      HTML_ATTR_CELLPADDING,
      HTML_ATTR_CELLSPACING,
      HTML_ATTR_CHAR,
      HTML_ATTR_CHAROFF,
      HTML_ATTR_CHARSET,
      HTML_ATTR_CHECKED,
      HTML_ATTR_CITE,
      HTML_ATTR_CLASS,
      HTML_ATTR_CLASSID,
      HTML_ATTR_CLEAR,
      HTML_ATTR_CODE,
      HTML_ATTR_CODEBASE,
      HTML_ATTR_CODETYPE,
      HTML_ATTR_COLOR,
      HTML_ATTR_COLS,
      HTML_ATTR_COLSPAN,
      HTML_ATTR_COMPACT,
      HTML_ATTR_CONTENT,
      HTML_ATTR_COORDS,
      HTML_ATTR_DATA,
      HTML_ATTR_DATETIME,
      HTML_ATTR_DECLARE,
      HTML_ATTR_DEFER,
      HTML_ATTR_DIR,
      HTML_ATTR_DISABLED,
      HTML_ATTR_ENCTYPE,
      HTML_ATTR_FACE,
      HTML_ATTR_FOR,
      HTML_ATTR_FRAME,
      HTML_ATTR_FRAMEBORDER,
      HTML_ATTR_HEADERS,
      HTML_ATTR_HEIGHT,
      HTML_ATTR_HREF,
      HTML_ATTR_HREFLANG,
      HTML_ATTR_HSPACE,
      HTML_ATTR_HTTP_EQUIV,
      HTML_ATTR_ID,
      HTML_ATTR_ISMAP,
      HTML_ATTR_LABEL,
      HTML_ATTR_LANG,
      HTML_ATTR_LANGUAGE,
      HTML_ATTR_LINK,
      HTML_ATTR_LONGDESC,
      HTML_ATTR_MARGINHEIGHT,
      HTML_ATTR_MARGINWIDTH,
      HTML_ATTR_MAXLENGTH,
      HTML_ATTR_MEDIA,
      HTML_ATTR_METHOD,
      HTML_ATTR_MULTIPLE,
      HTML_ATTR_NAME,
      HTML_ATTR_NOHREF,
      HTML_ATTR_NORESIZE,
      HTML_ATTR_NOSHADE,
      HTML_ATTR_NOWRAP,
      HTML_ATTR_OBJECT,
      HTML_ATTR_ONBLUR,
      HTML_ATTR_ONCHANGE,
      HTML_ATTR_ONCLICK,
      HTML_ATTR_ONDBLCLICK,
      HTML_ATTR_ONFOCUS,
      HTML_ATTR_ONKEYDOWN,
      HTML_ATTR_ONKEYPRESS,
      HTML_ATTR_ONKEYUP,
      HTML_ATTR_ONLOAD,
      HTML_ATTR_ONMOUSEDOWN,
      HTML_ATTR_ONMOUSEMOVE,
      HTML_ATTR_ONMOUSEOUT,
      HTML_ATTR_ONMOUSEOVER,
      HTML_ATTR_ONMOUSEUP,
      HTML_ATTR_ONRESET,
      HTML_ATTR_ONSELECT,
      HTML_ATTR_ONSUBMIT,
      HTML_ATTR_ONUNLOAD,
      HTML_ATTR_PROFILE,
      HTML_ATTR_PROMPT,
      HTML_ATTR_READONLY,
      HTML_ATTR_REL,
      HTML_ATTR_REV,
      HTML_ATTR_ROWS,
      HTML_ATTR_ROWSPAN,
      HTML_ATTR_RULES,
      HTML_ATTR_SCHEME,
      HTML_ATTR_SCOPE,
      HTML_ATTR_SCROLLING,
      HTML_ATTR_SELECTED,
      HTML_ATTR_SHAPE,
      HTML_ATTR_SIZE,
      HTML_ATTR_SPAN,
      HTML_ATTR_SRC,
      HTML_ATTR_STANDBY,
      HTML_ATTR_START,
      HTML_ATTR_STYLE,
      HTML_ATTR_SUMMARY,
      HTML_ATTR_TABINDEX,
      HTML_ATTR_TARGET,
      HTML_ATTR_TEXT,
      HTML_ATTR_TITLE,
      HTML_ATTR_TYPE,
      HTML_ATTR_USEMAP,
      HTML_ATTR_VALIGN,
      HTML_ATTR_VALUE,
      HTML_ATTR_VALUETYPE,
      HTML_ATTR_VERSION,
      HTML_ATTR_VLINK,
      HTML_ATTR_VSPACE,
      HTML_ATTR_WIDTH
    );

const
  HTML_ATTR_FirstID  = HTML_ATTR_ABBR;
  HTML_ATTR_LastID   = High(ThtmlAttrID);
  HTML_ATTR_MaxIndex = Ord(High(ThtmlAttrID));

function  htmlGetAttrName(const AttrID: ThtmlAttrID): String;
function  htmlGetAttrIDPtrA(const Name: PAnsiChar; const NameLen: Integer): ThtmlAttrID;
function  htmlGetAttrIDPtrW(const Name: PWideChar; const NameLen: Integer): ThtmlAttrID;
function  htmlGetAttrIDStrA(const Name: RawByteString): ThtmlAttrID;
function  htmlGetAttrIDStr(const Name: String): ThtmlAttrID;



{                                                                              }
{ HTML Element Information                                                     }
{                                                                              }
type
  ThtmlElementFlags = set of (
      elStartTagOptional,
      elEmpty,
      elEndTagForbidden,
      elEndTagOptional,
      elDeprecated,
      elFrameDTD,
      elLooseDTD,
      elFontStyle,
      elPhraseElement,
      elFormControl,
      elSpecialElement,
      elInline,
      elBlock,
      elList,
      elPreformatted,
      elTableElement,
      elHeadElement);
  ThtmlElementInformation = record
    Name  : String;
    Flags : ThtmlElementFlags;
  end;
  PhtmlElementInformation = ^ThtmlElementInformation;

function  htmlGetElementInformation(const Name: String): PhtmlElementInformation;
function  htmlIsEmptyElement(const TagID: ThtmlTagID): Boolean; overload;
function  htmlIsEmptyElement(const Name: String): Boolean; overload;
function  htmlIsElementEndTagOptional(const TagID: ThtmlTagID): Boolean; overload;
function  htmlIsElementEndTagOptional(const Name: String): Boolean; overload;
function  htmlIsElementFormControl(const TagID: ThtmlTagID): Boolean; overload;
function  htmlIsElementFormControl(const Name: String): Boolean; overload;
function  htmlIsElementList(const TagID: ThtmlTagID): Boolean; overload;
function  htmlIsElementList(const Name: String): Boolean; overload;
function  htmlIsTableElement(const TagID: ThtmlTagID): Boolean; overload;
function  htmlIsTableElement(const Name: String): Boolean; overload;
function  htmlIsHeadElement(const TagID: ThtmlTagID): Boolean; overload;
function  htmlIsHeadElement(const Name: String): Boolean; overload;



{                                                                              }
{ Overlapping tag functions                                                    }
{                                                                              }
function  htmlDoesCloseTagCloseOutside(const CloseTagID, TagID: ThtmlTagID): Boolean;
function  htmlDoesCloseTagCloseOpenTag(const CloseTagID, TagID: ThtmlTagID): Boolean;
function  htmlDoesOpenTagAutoCloseOpenTag(const OpenTagID, TagID: ThtmlTagID): Boolean;
function  htmlDoesOpenTagAutoCloseOutside(const OpenTagID, TagID: ThtmlTagID): Boolean;
function  htmlAutoOpenTag(const OpenTagID, TagID: ThtmlTagID): ThtmlTagID;



{                                                                              }
{ Tests                                                                        }
{                                                                              }
procedure Test;



implementation

uses
  { Fundamentals }
  flcASCII,
  flcUtils,
  flcStrings;



{                                                                              }
{ HTML Elements                                                                }
{                                                                              }
const
  htmlElementTable: array[ThtmlTagID] of ThtmlElementInformation = (
      // Special tag IDs
      (Name: '';             Flags:[]),
      (Name: '';             Flags:[]),
      // Element IDs
      // From HTML 4.01
      (Name: 'A';            Flags:[elInline, elSpecialElement]),
      (Name: 'ABBR';         Flags:[elInline, elPhraseElement]),
      (Name: 'ACRONYM';      Flags:[elInline, elPhraseElement]),
      (Name: 'ADDRESS';      Flags:[elBlock]),
      (Name: 'APPLET';       Flags:[elInline, elSpecialElement, elDeprecated, elLooseDTD]),
      (Name: 'AREA';         Flags:[elEmpty, elEndTagForbidden]),
      (Name: 'B';            Flags:[elInline, elFontStyle]),
      (Name: 'BASE';         Flags:[elEmpty, elEndTagForbidden, elHeadElement]),
      (Name: 'BASEFONT';     Flags:[elInline, elSpecialElement, elEmpty, elEndTagForbidden, elDeprecated, elLooseDTD]),
      (Name: 'BDO';          Flags:[elInline, elSpecialElement]),
      (Name: 'BIG';          Flags:[elInline, elFontStyle]),
      (Name: 'BLOCKQUOTE';   Flags:[elBlock]),
      (Name: 'BODY';         Flags:[elStartTagOptional, elEndTagOptional]),
      (Name: 'BR';           Flags:[elInline, elSpecialElement, elEmpty, elEndTagForbidden]),
      (Name: 'BUTTON';       Flags:[elInline, elFormControl]),
      (Name: 'CAPTION';      Flags:[]),
      (Name: 'CENTER';       Flags:[elBlock, elDeprecated, elLooseDTD]),
      (Name: 'CITE';         Flags:[elInline, elPhraseElement]),
      (Name: 'CODE';         Flags:[elInline, elPhraseElement]),
      (Name: 'COL';          Flags:[elTableElement, elEmpty, elEndTagForbidden]),
      (Name: 'COLGROUP';     Flags:[elTableElement, elEndTagOptional]),
      (Name: 'DD';           Flags:[elEndTagOptional]),
      (Name: 'DEL';          Flags:[elInline]),
      (Name: 'DFN';          Flags:[elInline, elPhraseElement]),
      (Name: 'DIR';          Flags:[elList, elBlock, elDeprecated, elLooseDTD]),
      (Name: 'DIV';          Flags:[elBlock]),
      (Name: 'DL';           Flags:[elBlock]),
      (Name: 'DT';           Flags:[elEndTagOptional]),
      (Name: 'EM';           Flags:[elInline, elPhraseElement]),
      (Name: 'FIELDSET';     Flags:[elBlock]),
      (Name: 'FONT';         Flags:[elInline, elSpecialElement, elDeprecated, elLooseDTD]),
      (Name: 'FORM';         Flags:[elBlock]),
      (Name: 'FRAME';        Flags:[elEmpty, elEndTagForbidden, elFrameDTD]),
      (Name: 'FRAMESET';     Flags:[elFrameDTD]),
      (Name: 'H1';           Flags:[elBlock]),
      (Name: 'H2';           Flags:[elBlock]),
      (Name: 'H3';           Flags:[elBlock]),
      (Name: 'H4';           Flags:[elBlock]),
      (Name: 'H5';           Flags:[elBlock]),
      (Name: 'H6';           Flags:[elBlock]),
      (Name: 'HEAD';         Flags:[elStartTagOptional, elEndTagOptional]),
      (Name: 'HR';           Flags:[elBlock, elEmpty, elEndTagForbidden]),
      (Name: 'HTML';         Flags:[elStartTagOptional, elEndTagOptional]),
      (Name: 'I';            Flags:[elInline, elFontStyle]),
      (Name: 'IFRAME';       Flags:[elInline, elSpecialElement, elLooseDTD]),
      (Name: 'IMG';          Flags:[elInline, elSpecialElement, elEmpty, elEndTagForbidden]),
      (Name: 'INPUT';        Flags:[elInline, elFormControl, elEmpty, elEndTagForbidden]),
      (Name: 'INS';          Flags:[elInline]),
      (Name: 'ISINDEX';      Flags:[elBlock, elEmpty, elEndTagForbidden, elDeprecated, elLooseDTD, elHeadElement]),
      (Name: 'KBD';          Flags:[elInline, elPhraseElement]),
      (Name: 'LABEL';        Flags:[elInline, elFormControl]),
      (Name: 'LEGEND';       Flags:[elInline]),
      (Name: 'LI';           Flags:[elEndTagOptional]),
      (Name: 'LINK';         Flags:[elEmpty, elEndTagForbidden, elHeadElement]),
      (Name: 'MAP';          Flags:[elInline, elSpecialElement]),
      (Name: 'MENU';         Flags:[elList, elBlock, elDeprecated, elLooseDTD]),
      (Name: 'META';         Flags:[elEmpty, elEndTagForbidden, elHeadElement]),
      (Name: 'NOFRAMES';     Flags:[elBlock, elFrameDTD]),
      (Name: 'NOSCRIPT';     Flags:[elBlock]),
      (Name: 'OBJECT';       Flags:[elInline, elSpecialElement, elHeadElement]),
      (Name: 'OL';           Flags:[elList, elBlock]),
      (Name: 'OPTGROUP';     Flags:[]),
      (Name: 'OPTION';       Flags:[elEndTagOptional]),
      (Name: 'P';            Flags:[elBlock, elEndTagOptional]),
      (Name: 'PARAM';        Flags:[elEmpty, elEndTagForbidden]),
      (Name: 'PRE';          Flags:[elPreformatted, elBlock]),
      (Name: 'Q';            Flags:[elInline, elSpecialElement]),
      (Name: 'S';            Flags:[elInline, elFontStyle, elDeprecated, elLooseDTD]),
      (Name: 'SAMP';         Flags:[elInline, elPhraseElement]),
      (Name: 'SCRIPT';       Flags:[elInline, elSpecialElement, elHeadElement]),
      (Name: 'SELECT';       Flags:[elInline, elFormControl]),
      (Name: 'SMALL';        Flags:[elInline, elFontStyle]),
      (Name: 'SPAN';         Flags:[elInline, elSpecialElement]),
      (Name: 'STRIKE';       Flags:[elInline, elFontStyle, elLooseDTD]),
      (Name: 'STRONG';       Flags:[elInline, elPhraseElement]),
      (Name: 'STYLE';        Flags:[elHeadElement]),
      (Name: 'SUB';          Flags:[elInline, elSpecialElement]),
      (Name: 'SUP';          Flags:[elInline, elSpecialElement]),
      (Name: 'TABLE';        Flags:[elTableElement, elBlock]),
      (Name: 'TBODY';        Flags:[elTableElement, elStartTagOptional, elEndTagOptional]),
      (Name: 'TD';           Flags:[elTableElement, elEndTagOptional]),
      (Name: 'TEXTAREA';     Flags:[elInline, elFormControl]),
      (Name: 'TFOOT';        Flags:[elTableElement, elEndTagOptional]),
      (Name: 'TH';           Flags:[elTableElement, elEndTagOptional]),
      (Name: 'THEAD';        Flags:[elTableElement, elEndTagOptional]),
      (Name: 'TITLE';        Flags:[elHeadElement]),
      (Name: 'TR';           Flags:[elTableElement, elEndTagOptional]),
      (Name: 'TT';           Flags:[elInline, elFontStyle]),
      (Name: 'U';            Flags:[elInline, elFontStyle, elDeprecated, elLooseDTD]),
      (Name: 'UL';           Flags:[elBlock, elList]),
      (Name: 'VAR';          Flags:[elInline, elPhraseElement]),
      // From HTML 5.1
      (Name: 'ARTICLE';      Flags:[elBlock]),
      (Name: 'ASIDE';        Flags:[elBlock]),
      (Name: 'AUDIO';        Flags:[elInline]),
      (Name: 'BDI';          Flags:[elInline]),
      (Name: 'CANVAS';       Flags:[elInline]),
      (Name: 'COMMAND';      Flags:[elInline]),
      (Name: 'DATALIST';     Flags:[elInline]),
      (Name: 'DETAILS';      Flags:[elBlock]),
      (Name: 'DIALOG';       Flags:[elBlock]),
      (Name: 'FIGCAPTION';   Flags:[elBlock]),
      (Name: 'FIGURE';       Flags:[elBlock]),
      (Name: 'FOOTER';       Flags:[elBlock]),
      (Name: 'HEADER';       Flags:[elBlock]),
      (Name: 'HGROUP';       Flags:[elBlock]),
      (Name: 'KEYGEN';       Flags:[elInline]),
      (Name: 'MARK';         Flags:[elInline]),
      (Name: 'METER';        Flags:[elInline]),
      (Name: 'NAV';          Flags:[elBlock]),
      (Name: 'OUTPUT';       Flags:[elInline]),
      (Name: 'PROGRESS';     Flags:[elInline]),
      (Name: 'SECTION';      Flags:[elBlock]),
      (Name: 'SOURCE';       Flags:[elInline]),
      (Name: 'SUMMARY';      Flags:[elInline]),
      (Name: 'TIME';         Flags:[elInline]),
      (Name: 'TRACK';        Flags:[elInline]),
      (Name: 'VIDEO';        Flags:[elInline]),
      (Name: 'WBR';          Flags:[elInline])
    );

var
  ElementHashIndex : array['A'..'Z'] of ThtmlTagID;
  ElementHashCount : array['A'..'Z'] of Integer;
  ElementHashInit  : Boolean = False;

procedure InitElementHash;
var I: ThtmlTagID;
    C: AnsiChar;
begin
  for C := 'A' to 'Z' do
    ElementHashIndex[C] := HTML_TAG_None;
  FillChar(ElementHashCount, Sizeof(ElementHashCount), #0);
  for I := HTML_TAG_FirstID to HTML_TAG_LastID do
    begin
      Assert(htmlElementTable[I].Name <> '', 'Invalid name');
      C := AsciiUpCaseB(AnsiChar(htmlElementTable[I].Name[1]));
      Assert(C in ['A'..'Z'], 'Invalid name');
      if ElementHashIndex[C] = HTML_TAG_None then
        ElementHashIndex[C] := I;
      Inc(ElementHashCount[C]);
    end;
  ElementHashInit := True;
end;

var
  TagNameRef: array[ThtmlTagID] of String;

function htmlGetTagName(const TagID: ThtmlTagID): String;
begin
  if (TagID < HTML_TAG_FirstID) or (TagID > HTML_TAG_LastID) then
    Result := ''
  else
    begin
      Result := TagNameRef[TagID]; // check if a reference exists
      if Result <> '' then
        exit;
      Result := htmlElementTable[TagID].Name; // copy
      TagNameRef[TagID] := Result; // store reference
    end;
end;

function htmlGetTagName(const TagID: ThtmlTagID; const Name: String): String;
begin
  if (TagID < HTML_TAG_FirstID) or (TagID > HTML_TAG_LastID) then
    Result := Name
  else
    Result := htmlGetTagName(TagID);
end;

function htmlGetTagIDPtrA(const Name: PAnsiChar; const NameLen: Integer): ThtmlTagID;
var I: Integer;
    P: PhtmlElementInformation;
    C: AnsiChar;
begin
  if NameLen > 0 then
    begin
      C := UpCase(Name^);
      if C in ['A'..'Z'] then
        begin
          if not ElementHashInit then
            InitElementHash;
          Result := ElementHashIndex[C];
          if Result <> HTML_TAG_None then
            begin
              P := @htmlElementTable[Result];
              for I := 1 to ElementHashCount[C] do
                if (Length(P^.Name) = NameLen) and
                   StrPMatchNoAsciiCaseBW(Pointer(P^.Name), Pointer(Name), NameLen) then
                  // Found
                  exit
                else
                  begin
                    {$R-}
                    Inc(Result);
                    Inc(P);
                  end;
            end;
        end;
    end;
  Result := HTML_TAG_None;
end;

function htmlGetTagIDPtrW(const Name: PWideChar; const NameLen: Integer): ThtmlTagID;
var I: Integer;
    P: PhtmlElementInformation;
    D: WideChar;
    C: AnsiChar;
begin
  if NameLen > 0 then
    begin
      D := Name^;
      if (Ord(D) <= $FF) and (AnsiChar(Ord(D)) in ['A'..'Z', 'a'..'z']) then
        begin
          C := UpCase(AnsiChar(Ord(D)));
          if not ElementHashInit then
            InitElementHash;
          Result := ElementHashIndex[C];
          if Result <> HTML_TAG_None then
            begin
              P := @htmlElementTable[Result];
              for I := 1 to ElementHashCount[C] do
                if (Length(P^.Name) = NameLen) and
                    StrPMatchNoAsciiCase(Name, Pointer(P^.Name), NameLen) then
                  // Found
                  exit
                else
                  begin
                    {$R-}
                    Inc(Result);
                    Inc(P);
                  end;
            end;
        end;
    end;
  Result := HTML_TAG_None;
end;

function htmlGetTagIDStrB(const Name: RawByteString): ThtmlTagID;
begin
  Result := htmlGetTagIDPtrA(Pointer(Name), Length(Name));
end;

function htmlGetTagIDStr(const Name: String): ThtmlTagID;
begin
  Result := htmlGetTagIDPtrW(Pointer(Name), Length(Name));
end;

function htmlIsSameTag(const TagID1: ThtmlTagID; const Name1: String;
    const TagID2: ThtmlTagID; const Name2: String): Boolean;
begin
  Result := TagID1 = TagID2;
  if not Result then
    exit;
  if (TagID1 = HTML_TAG_None) and (TagID2 = HTML_TAG_None) then
    Result := StrEqualNoAsciiCase(Name1, Name2);
end;

const
  htmlAttributeTable: array[ThtmlAttrID] of String = ('',
      'ABBR',             'ACCEPT-CHARSET',   'ACCEPT',
      'ACCESSKEY',        'ACTION',           'ALIGN',
      'ALINK',            'ALT',              'ARCHIVE',
      'AXIS',             'BACKGROUND',       'BGCOLOR',
      'BORDER',           'CELLPADDING',      'CELLSPACING',
      'CHAR',             'CHAROFF',          'CHARSET',
      'CHECKED',          'CITE',             'CLASS',
      'CLASSID',          'CLEAR',            'CODE',
      'CODEBASE',         'CODETYPE',         'COLOR',
      'COLS',             'COLSPAN',          'COMPACT',
      'CONTENT',          'COORDS',           'DATA',
      'DATETIME',         'DECLARE',          'DEFER',
      'DIR',              'DISABLED',         'ENCTYPE',
      'FACE',             'FOR',              'FRAME',
      'FRAMEBORDER',      'HEADERS',          'HEIGHT',
      'HREF',             'HREFLANG',         'HSPACE',
      'HTTP-EQUIV',       'ID',               'ISMAP',
      'LABEL',            'LANG',             'LANGUAGE',
      'LINK',             'LONGDESC',         'MARGINHEIGHT',
      'MARGINWIDTH',      'MAXLENGTH',        'MEDIA',
      'METHOD',           'MULTIPLE',         'NAME',
      'NOHREF',           'NORESIZE',         'NOSHADE',
      'NOWRAP',           'OBJECT',           'ONBLUR',
      'ONCHANGE',         'ONCLICK',          'ONDBLCLICK',
      'ONFOCUS',          'ONKEYDOWN',        'ONKEYPRESS',
      'ONKEYUP',          'ONLOAD',           'ONMOUSEDOWN',
      'ONMOUSEMOVE',      'ONMOUSEOUT',       'ONMOUSEOVER',
      'ONMOUSEUP',        'ONRESET',          'ONSELECT',
      'ONSUBMIT',         'ONUNLOAD',         'PROFILE',
      'PROMPT',           'READONLY',         'REL',
      'REV',              'ROWS',             'ROWSPAN',
      'RULES',            'SCHEME',           'SCOPE',
      'SCROLLING',        'SELECTED',         'SHAPE',
      'SIZE',             'SPAN',             'SRC',
      'STANDBY',          'START',            'STYLE',
      'SUMMARY',          'TABINDEX',         'TARGET',
      'TEXT',             'TITLE',            'TYPE',
      'USEMAP',           'VALIGN',           'VALUE',
      'VALUETYPE',        'VERSION',          'VLINK',
      'VSPACE',           'WIDTH');

var
  AttributeHashIndex : array['A'..'Z'] of ThtmlAttrID;
  AttributeHashCount : array['A'..'Z'] of Integer;
  AttributeHashInit  : Boolean = False;

procedure InitAttributeHash;
var I: ThtmlAttrID;
    C: AnsiChar;
begin
  for C := 'A' to 'Z' do
    AttributeHashIndex[C] := HTML_ATTR_None;
  FillChar(AttributeHashCount, Sizeof(AttributeHashCount), #0);
  for I := HTML_ATTR_FirstID to HTML_ATTR_LastID do
    begin
      Assert(htmlAttributeTable[I] <> '', 'Invalid name');
      C := AsciiUpCaseB(AnsiChar(htmlAttributeTable[I][1]));
      Assert(C in ['A'..'Z'], 'Invalid name');
      if AttributeHashIndex[C] = HTML_ATTR_None then
        AttributeHashIndex[C] := I;
      Inc(AttributeHashCount[C]);
    end;
  AttributeHashInit := True;
end;

var
  AttrNameRef: array[ThtmlAttrID] of String;

function htmlGetAttrName(const AttrID: ThtmlAttrID): String;
begin
  if (AttrID < HTML_ATTR_FirstID) or (AttrID > HTML_ATTR_LastID) then
    Result := ''
  else
    begin
      Result := AttrNameRef[AttrID]; // reference
      if Result <> '' then
        exit;
      Result := htmlAttributeTable[AttrID]; // copy
      AttrNameRef[AttrID] := Result; // reference
    end;
end;

function htmlGetAttrIDPtrA(const Name: PAnsiChar; const NameLen: Integer): ThtmlAttrID;
var I: Integer;
    C: AnsiChar;
begin
  if NameLen > 0 then
    begin
      C := UpCase(Name^);
      if C in ['A'..'Z'] then
        begin
          if not AttributeHashInit then
            InitAttributeHash;
          Result := AttributeHashIndex[C];
          if Result <> HTML_ATTR_None then
            for I := 1 to AttributeHashCount[C] do
              if (Length(htmlAttributeTable[Result]) = NameLen) and
                 StrPMatchNoAsciiCaseBW(PWideChar(htmlAttributeTable[Result]), Pointer(Name), NameLen) then
                // Found
                exit
              else
                {$R-}
                Inc(Result);
        end;
    end;
  Result := HTML_ATTR_None;
end;

function htmlGetAttrIDPtrW(const Name: PWideChar; const NameLen: Integer): ThtmlAttrID;
var I: Integer;
    D: WideChar;
    C: AnsiChar;
begin
  if NameLen > 0 then
    begin
      D := Name^;
      if (Ord(D) <= $FF) and (AnsiChar(Ord(D)) in ['A'..'Z', 'a'..'z']) then
        begin
          C := UpCase(AnsiChar(Ord(D)));
          if not AttributeHashInit then
            InitAttributeHash;
          Result := AttributeHashIndex[C];
          if Result <> HTML_ATTR_None then
            for I := 1 to AttributeHashCount[C] do
              if (Length(htmlAttributeTable[Result]) = NameLen) and
                 StrPMatchNoAsciiCase(Pointer(Name), Pointer(htmlAttributeTable[Result]), NameLen) then
                // Found
                exit
              else
                {$R-}
                Inc(Result);
        end;
    end;
  Result := HTML_ATTR_None;
end;

function htmlGetAttrIDStrA(const Name: RawByteString): ThtmlAttrID;
begin
  Result := htmlGetAttrIDPtrA(Pointer(Name), Length(Name));
end;

function htmlGetAttrIDStr(const Name: String): ThtmlAttrID;
begin
  Result := htmlGetAttrIDPtrW(Pointer(Name), Length(Name));
end;

function htmlGetElementInformation(const Name: String): PhtmlElementInformation;
var I: ThtmlTagID;
begin
  I := htmlGetTagIDStr(Name);
  if I <> HTML_TAG_None then
    Result := @htmlElementTable[I]
  else
    Result := nil;
end;

function htmlIsEmptyElement(const TagID: ThtmlTagID): Boolean;
begin
  Result := elEmpty in htmlElementTable[TagID].Flags;
end;

function htmlIsEmptyElement(const Name: String): Boolean;
var P: PhtmlElementInformation;
begin
  P := htmlGetElementInformation(Name);
  Result := Assigned(P) and (elEmpty in P^.Flags);
end;

function htmlIsElementEndTagOptional(const TagID: ThtmlTagID): Boolean;
begin
  Result := elEndTagOptional in htmlElementTable[TagID].Flags;
end;

function htmlIsElementEndTagOptional(const Name: String): Boolean;
var P: PhtmlElementInformation;
begin
  P := htmlGetElementInformation(Name);
  Result := Assigned(P) and (elEndTagOptional in P^.Flags);
end;

function htmlIsElementFormControl(const TagID: ThtmlTagID): Boolean;
begin
  Result := elFormControl in htmlElementTable[TagID].Flags;
end;

function htmlIsElementFormControl(const Name: String): Boolean;
var P: PhtmlElementInformation;
begin
  P := htmlGetElementInformation(Name);
  Result := Assigned(P) and (elFormControl in P^.Flags);
end;

function htmlIsElementList(const TagID: ThtmlTagID): Boolean;
begin
  Result := elList in htmlElementTable[TagID].Flags;
end;

function htmlIsElementList(const Name: String): Boolean;
var P: PhtmlElementInformation;
begin
  P := htmlGetElementInformation(Name);
  Result := Assigned(P) and (elList in P^.Flags);
end;

function htmlIsTableElement(const TagID: ThtmlTagID): Boolean;
begin
  Result := elTableElement in htmlElementTable[TagID].Flags;
end;

function htmlIsTableElement(const Name: String): Boolean;
var P: PhtmlElementInformation;
begin
  P := htmlGetElementInformation(Name);
  Result := Assigned(P) and (elTableElement in P^.Flags);
end;

function htmlIsHeadElement(const TagID: ThtmlTagID): Boolean;
begin
  Result := elHeadElement in htmlElementTable[TagID].Flags;
end;

function htmlIsHeadElement(const Name: String): Boolean;
var P: PhtmlElementInformation;
begin
  P := htmlGetElementInformation(Name);
  Result := Assigned(P) and (elHeadElement in P^.Flags);
end;



{                                                                              }
{ Overlapping tag functions                                                    }
{                                                                              }

{   "Overlapping tags" are not allowed by the HTML specification but are       }
{   interpreted by Internet Explorer (IE).                                     }

// Returns True if </CloseTag> propagates through open overlapping <Tag>
function htmlDoesCloseTagCloseOutside(const CloseTagID, TagID: ThtmlTagID): Boolean;
begin
  if (TagID = HTML_TAG_TD) or (TagID = HTML_TAG_TH) then
    begin
      // Only a TABLE close propagates outside a cell
      if CloseTagID = HTML_TAG_TABLE then
        Result := True else
        Result := False;
    end else
  if TagID = HTML_TAG_FORM then
    begin
      // Form controls close is local to form
      if htmlIsElementFormControl(CloseTagID) then
        Result := False else
        Result := True;
    end else
  if CloseTagID = HTML_TAG_LI then
    begin
      // LI close is local to list
      if htmlIsElementList(TagID) then
        Result := False else
        Result := True;
    end
  else
    Result := True; // default is for tag to close
end;

// Returns True if </CloseTag> closes overlapping open <Tag>
function htmlDoesCloseTagCloseOpenTag(const CloseTagID, TagID: ThtmlTagID): Boolean;
begin
  if (CloseTagID = HTML_TAG_H4) and (TagID = HTML_TAG_P) then
    Result := True else
  if CloseTagID = HTML_TAG_None then
    Result := False else // Unknown tags do not close overlapping tags
  if CloseTagID = TagID then
    Result := True else // Matching open tag
  if htmlIsElementList(CloseTagID) then // list close
    begin
      // List close closes LI and other lists
      if TagID = HTML_TAG_LI then
        Result := True else
      if htmlIsElementList(TagID) then
        Result := True else
        Result := False;
    end else
  if htmlIsTableElement(CloseTagID) then
    Result := True else // table elements close all content
  if CloseTagID = HTML_TAG_FORM then
    // Form close closes form controls
    if htmlIsElementFormControl(TagID) then
      Result := True else
      Result := False
  else
    Result := False; // leave overlapping tag open by default
end;

// Returns True if <OpenTag> closes ancestral open <Tag>
function htmlDoesOpenTagAutoCloseOpenTag(const OpenTagID, TagID: ThtmlTagID): Boolean;
begin
  // BODY open auto-closes HEAD
  if OpenTagID = HTML_TAG_BODY then
    if TagID = HTML_TAG_HEAD then
      begin
        Result := True;
        exit;
      end;
  // COL is auto-closed by any table element open
  if TagID = HTML_TAG_COL then
    if htmlIsTableElement(OpenTagID) then
      begin
        Result := True;
        exit;
      end;
  // COLGROUP is auto-closed by any table element open except COL
  if TagID = HTML_TAG_COLGROUP then
    if (OpenTagID <> HTML_TAG_COL) and htmlIsTableElement(OpenTagID) then
      begin
        Result := True;
        exit;
      end;
  // THEAD/TBODY/TFOOT/TR/TD/TH is auto-closed by THEAD/TBODY/TFOOT open
  if (TagID = HTML_TAG_THEAD) or (TagID = HTML_TAG_TFOOT) or
     (TagID = HTML_TAG_TBODY) or (TagID = HTML_TAG_TR) or
     (TagID = HTML_TAG_TD) or (TagID = HTML_TAG_TH) then
    if (OpenTagID = HTML_TAG_TBODY) or (OpenTagID = HTML_TAG_THEAD) or
       (OpenTagID = HTML_TAG_TFOOT) then
      begin
        Result := True;
        exit;
      end;
  // default is for tag open to not auto-close other open tags
  Result := False;
end;

// Returns True if <OpenTag> propagates closes through open overlapping <Tag>
function htmlDoesOpenTagAutoCloseOutside(const OpenTagID, TagID: ThtmlTagID): Boolean;
begin
  if OpenTagID = HTML_TAG_HTML then
    Result := False else
  if (OpenTagID = HTML_TAG_TD) or (OpenTagID = HTML_TAG_TH) then
    begin
      // TD/TH does not close outside TR+
      if (TagID = HTML_TAG_TR) or (TagID = HTML_TAG_TBODY) or
         (TagID = HTML_TAG_TFOOT) or (TagID = HTML_TAG_THEAD) or
         (TagID = HTML_TAG_TABLE) then
        Result := False else
        Result := True;
    end else
  if OpenTagID = HTML_TAG_TR then
    begin
      // TR does not close outside TBODY/TFOOT/THEAD+
      if (TagID = HTML_TAG_TBODY) or (TagID = HTML_TAG_TFOOT) or
         (TagID = HTML_TAG_THEAD) or (TagID = HTML_TAG_TABLE) then
        Result := False else
        Result := True;
    end else
  if OpenTagID = HTML_TAG_COL then
    begin
      // COL does not close outside COLGROUP/TABLE
      if (TagID = HTML_TAG_COLGROUP) or (TagID = HTML_TAG_TABLE) then
        Result := False else
        Result := True;
    end else
  if htmlIsTableElement(OpenTagID) then
    begin
      // rest of table element does not close beyond table
      if TagID = HTML_TAG_TABLE then
        Result := False else
        Result := True;
    end else
  if TagID = HTML_TAG_FORM then
    begin
      // Form controls close is local to form
      if htmlIsElementFormControl(OpenTagID) then
        Result := False else
        Result := True;
    end else
  if htmlIsElementList(OpenTagID) then
    begin
      // List open do not close any elements
      Result := False;
    end else
  if htmlIsElementList(TagID) then
    begin
      // LI close is local to list
      if OpenTagID = HTML_TAG_LI then
        Result := False else
        Result := True;
    end
  else
    Result := True; // Default is to close through
end;

// Returns tag to be inserted after parent <Tag> and before child <OpenTag>
// Returns <Tag> if tag is direct container for <OpenTag>
// Returns HTML_TAG_None if <Tag> is not a container for <OpenTag>
function htmlAutoOpenTag(const OpenTagID, TagID: ThtmlTagID): ThtmlTagID;
begin
  if (OpenTagID = HTML_TAG_META) then
    // Don't auto open for META (TODO: Handle badly structured HTML)
    Result := HTML_TAG_None else
  if (OpenTagID = HTML_TAG_None) or (TagID = HTML_TAG_None) then
    Result := HTML_TAG_None else
  if TagID = HTML_TAG_Document then
    begin
      // Document is container for HTML
      if OpenTagID = HTML_TAG_HTML then
        Result := TagID else
        // HTML needed between Document and content
        Result := HTML_TAG_HTML;
    end else
  if TagID = HTML_TAG_HTML then
    begin
      // HTML is container for HEAD/BODY/FRAMESET
      if (OpenTagID = HTML_TAG_HEAD) or (OpenTagID = HTML_TAG_BODY) or
         (OpenTagID = HTML_TAG_FRAMESET) then
        Result := TagID else
      // FRAMESET required between HTML and FRAME
      if OpenTagID = HTML_TAG_FRAME then
        Result := HTML_TAG_FRAMESET else
      // HEAD required between HTML and head elements
      if htmlIsHeadElement(OpenTagID) then
        Result := HTML_TAG_HEAD else
        // BODY assumed between HTML and content
        Result := HTML_TAG_BODY;
    end else
  if TagID = HTML_TAG_HEAD then
    begin
      // HEAD is container for head elements
      if htmlIsHeadElement(OpenTagID) then
        Result := TagID else
        Result := HTML_TAG_None;
    end else
  if (TagID = HTML_TAG_BODY) or (TagID = HTML_TAG_FRAMESET) then
    begin
      // UL list required for LI
      if OpenTagID = HTML_TAG_LI then
        Result := HTML_TAG_UL else
      // FORM required for form controls
      if htmlIsElementFormControl(OpenTagID) then
        Result := HTML_TAG_FORM else
      // TABLE required for table elements
      if (OpenTagID <> HTML_TAG_TABLE) and htmlIsTableElement(OpenTagID) then
        Result := HTML_TAG_TABLE else
        // Final container
        Result := TagID;
    end else
  if TagID = HTML_TAG_FORM then
    begin
      // FORM is container for form controls
      if htmlIsElementFormControl(OpenTagID) then
        Result := TagID else
        Result := HTML_TAG_None;
    end else
  if TagID = HTML_TAG_TABLE then
    begin
      // TBODY required between TABLE and TD/TH/TR
      if (OpenTagID = HTML_TAG_TD) or (OpenTagID = HTML_TAG_TH) or
         (OpenTagID = HTML_TAG_TR) then
        Result := HTML_TAG_TBODY else
      // COLGROUP required between TABLE and COL
      if OpenTagID = HTML_TAG_COL then
        Result := HTML_TAG_COLGROUP else
      // TABLE is container for TBODY/THEAD/TFOOT/COLGROUP/COL/CAPTION
      if (OpenTagID = HTML_TAG_TBODY) or (OpenTagID = HTML_TAG_THEAD) or
         (OpenTagID = HTML_TAG_TFOOT) or (OpenTagID = HTML_TAG_COLGROUP) or
         (OpenTagID = HTML_TAG_COL) or (OpenTagID = HTML_TAG_CAPTION) then
        Result := TagID else
        Result := HTML_TAG_None;
    end else
  if TagID = HTML_TAG_COLGROUP then
    begin
      // COLGROUP is container for COL
      if OpenTagID = HTML_TAG_COL then
        Result := TagID else
        Result := HTML_TAG_None;
    end else
  if (TagID = HTML_TAG_TBODY) or (TagID = HTML_TAG_THEAD) or
     (TagID = HTML_TAG_TFOOT) then
    begin
      // TR required between TBODY and TD/TH
      if (OpenTagID = HTML_TAG_TD) or (OpenTagID = HTML_TAG_TH) then
        Result := HTML_TAG_TR else
      // TBODY/THEAD/TFOOT is container for TR
      if OpenTagID = HTML_TAG_TR then
        Result := TagID else
        Result := HTML_TAG_None;
    end else
  if TagID = HTML_TAG_TR then
    begin
      // TR is container for TD/TH
      if (OpenTagID = HTML_TAG_TD) or (OpenTagID = HTML_TAG_TH) then
        Result := TagID else
        Result := HTML_TAG_None;
    end else
  if htmlIsElementList(TagID) then
    begin
      // List is container for LI
      if OpenTagID = HTML_TAG_LI then
        Result := TagID else
        Result := HTML_TAG_None;
    end else
    Result := HTML_TAG_None;
end;



{                                                                              }
{ Tests                                                                        }
{                                                                              }
{$IFDEF HTML_TEST}
{$ASSERTIONS ON}
procedure Test;
begin
  Assert(htmlGetTagName(HTML_TAG_VAR) = 'VAR', 'htmlGetTagName');
  Assert(htmlGetTagIDStrB('VAR') = HTML_TAG_VAR, 'htmlGetTagIDStr');
  Assert(htmlGetTagIDStrB('html') = HTML_TAG_HTML, 'htmlGetTagIDStr');
  Assert(htmlGetTagIDStrB('META') = HTML_TAG_META, 'htmlGetTagIDStr');

  Assert(htmlGetAttrName(HTML_ATTR_WIDTH) = 'WIDTH', 'htmlGetAttrName');
  Assert(htmlGetAttrIDStrA('WIDTH') = HTML_ATTR_WIDTH, 'htmlGetAttrIDStr');
  Assert(htmlGetAttrIDStrA('height') = HTML_ATTR_HEIGHT, 'htmlGetAttrIDStr');

  Assert(htmlIsEmptyElement('br'), 'htmlIsEmptyElement');
  Assert(htmlIsEmptyElement('IMG'), 'htmlIsEmptyElement');
  Assert(htmlIsEmptyElement('META'), 'htmlIsEmptyElement');
  Assert(not htmlIsEmptyElement('A'), 'htmlIsEmptyElement');
  Assert(not htmlIsEmptyElement('XYZ'), 'htmlIsEmptyElement');
end;
{$ENDIF}



end.

