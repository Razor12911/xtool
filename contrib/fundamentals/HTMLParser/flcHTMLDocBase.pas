{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals 5.00 - HTML Parser                          }
{   File name:        flcHTMLDocBase.pas                                       }
{   File version:     5.10                                                     }
{   Description:      HTML DOM base classes                                    }
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
{   2002/10/27  1.01  Classes for all HTML 4 element objects.                  }
{   2002/10/28  1.02  Unicode support.                                         }
{   2002/11/03  1.03  Optimizations.                                           }
{   2002/11/22  1.04  Style sheet support.                                     }
{   2002/12/03  1.05  Style sheet changes.                                     }
{   2002/12/07  1.06  Small additions.                                         }
{   2002/12/08  1.07  Split-up unit cHTMLObjects.                              }
{   2002/12/16  1.08  Improved white-space and line-break handling.            }
{   2015/04/11  1.09  UnicodeString changes.                                   }
{   2019/02/22  5.10  Revise for Fundamentals 5.                               }
{                                                                              }
{******************************************************************************}

{$INCLUDE flcHTML.inc}

unit flcHTMLDocBase;

interface

uses
  { System }
  SysUtils,

  { Fundamentals }
  flcStdTypes,
  flcUnicodeCodecs,

  { HTML }
  flcHTMLElements,
  flcHTMLStyleProperties,
  flcHTMLStyleSheet;



{                                                                              }
{ AhtmlObject                                                                  }
{   Base class for HTML document objects.                                      }
{                                                                              }
type
  TRefactorOperation = (
      reopRefactorForLayout
  );
  TRefactorOperations = set of TRefactorOperation;
  AhtmlObject = class
  protected
    FUserTag : NativeUInt;
    FUserObj : TObject;
    FName    : String;
    FParent  : AhtmlObject;
    FPrev    : AhtmlObject;
    FNext    : AhtmlObject;
    FStyle   : ThtmlcssStyleProperties;

    procedure Init; virtual;

    function  GetName: String; virtual;
    function  GetNameUTF8: RawByteString;

    function  GetContentText: String; virtual;
    procedure SetContentText(const ContentText: String); virtual;

    function  GetContentTextUTF8: RawByteString;
    procedure SetContentTextUTF8(const ContentText: RawByteString);

    function  GetHTMLText: String; virtual;
    function  GetHTMLTextUTF8: RawByteString;

    function  GetEncodedHTMLText(const Codec: TCustomUnicodeCodec): RawByteString; virtual;

    procedure InitStyleElementInfo(const StyleSheet: ThtmlCSS;
              const ParentInfo: PhtmlcssElementInfo); virtual;
    procedure InitStyleInfo(const StyleSheet: ThtmlCSS;
              const StyleInfo: ThtmlcssStyleProperties); virtual;

    function  Refactor(const Operations: TRefactorOperations): AhtmlObject; virtual;

  public
    constructor Create; overload; virtual;

    property  UserTag: NativeUInt read FUserTag write FUserTag;
    property  UserObj: TObject read FUserObj write FUserObj;

    property  Name: String read GetName;
    property  NameUTF8: RawByteString read GetNameUTF8;

    property  Parent: AhtmlObject read FParent;
    property  PrevSibling: AhtmlObject read FPrev;
    property  NextSibling: AhtmlObject read FNext;

    function  DuplicateObject: AhtmlObject; virtual;

    function  GetStructureStr(const Depth: Integer = 0;
              const Level: Integer = 0): String; virtual;

    property  ContentText: String read GetContentText write SetContentText;
    property  ContentTextUTF8: RawByteString read GetContentTextUTF8 write SetContentTextUTF8;

    property  HTMLTextUTF8: RawByteString read GetHTMLTextUTF8;
    property  HTMLText: String read GetHTMLText;

    function  EncodedHTMLText(const Codec: TCustomUnicodeCodec): RawByteString;

    property  Style: ThtmlcssStyleProperties read FStyle;
  end;
  AhtmlObjectClass = class of AhtmlObject;
  EhtmlObject = class(Exception);



{                                                                              }
{ AhtmlContainerObject                                                         }
{   Base class for HTML objects that can contain other HTML objects.           }
{                                                                              }
type
  AhtmlContainerObject = class(AhtmlObject)
  protected
    FFirst : AhtmlObject;
    FLast  : AhtmlObject;

    procedure SetContentText(const ContentText: String); override;
    function  GetContentText: String; override;

    // function  GetHTMLTextUTF8: RawByteString; override;
    function  GetHTMLText: String; override;
    function  GetEncodedHTMLText(const Codec: TCustomUnicodeCodec): RawByteString; override;

    function  GetItem(const Name: String): AhtmlObject; virtual;
    procedure SetItem(const Name: String; const Item: AhtmlObject); virtual;

    function  GetItemText(const Name: String): String;
    procedure SetItemText(const Name: String; const Text: String);

    procedure DeleteChild(const Obj: AhtmlObject);
    procedure ReplaceChild(const Obj, NewObj: AhtmlObject);

    procedure InitStyleElementInfo(const StyleSheet: ThtmlCSS;
              const ParentInfo: PhtmlcssElementInfo); override;
    procedure InitStyleInfo(const StyleSheet: ThtmlCSS;
              const StyleInfo: ThtmlcssStyleProperties); override;

    function  Refactor(const Operations: TRefactorOperations): AhtmlObject; override;

  public
    destructor Destroy; override;

    property  FirstChild: AhtmlObject read FFirst;
    property  LastChild: AhtmlObject read FLast;

    procedure ClearItems;
    function  DuplicateObject: AhtmlObject; override;

    function  GetStructureStr(const Depth: Integer = 0;
              const Level: Integer = 0): String; override;

    function  CreateItem(const ID: Integer; const Name: String): AhtmlObject; virtual;
    procedure AddItem(const Obj: AhtmlObject); virtual;

    function  FindNext(const Previous: AhtmlObject;
              const ItemClass: AhtmlObjectClass): AhtmlObject;
    function  FindNextName(const Previous: AhtmlObject;
              const Name: String): AhtmlObject;

    function  GetItemByClass(const ItemClass: AhtmlObjectClass): AhtmlObject;
    function  RequireItemByClass(const ItemClass: AhtmlObjectClass): AhtmlObject;
    function  GetItemTextByClass(const ItemClass: AhtmlObjectClass): RawByteString;

    property  Item[const Name: String]: AhtmlObject read GetItem write SetItem;
    function  RequireItem(const Name: String;
              const ItemClass: AhtmlObjectClass = nil): AhtmlObject;
    property  ItemText[const Name: String]: String
              read GetItemText write SetItemText;
  end;
  EhtmlContainerObject = class(EhtmlObject);



{                                                                              }
{ AhtmlTextContentObject                                                       }
{   Base class for text content objects.                                       }
{                                                                              }
type
  AhtmlTextContentObject = class(AhtmlObject)
  end;



{                                                                              }
{ AhtmlTextStringContentObject                                                 }
{   Base class for text content objects that store content as a UnicodeString. }
{                                                                              }
type
  AhtmlTextStringContentObject = class(AhtmlTextContentObject)
  protected
    FText : String;

    function  GetContentText: String; override;
    procedure SetContentText(const ContentText: String); override;

  public
    constructor Create(const Text: String); overload;
    constructor CreateWide(const Text: WideString);

    function  DuplicateObject: AhtmlObject; override;

    property  Text: String read FText write FText;
  end;



{                                                                              }
{ ThtmlText                                                                    }
{   Text content.                                                              }
{                                                                              }
type
  ThtmlText = class(AhtmlTextStringContentObject)
  protected
    function  GetHTMLText: String; override;
    function  GetEncodedHTMLText(const Codec: TCustomUnicodeCodec): RawByteString; override;

    function  IsStartWhiteSpace: Boolean;
    function  IsEndWhiteSpace: Boolean;
    function  Refactor(const Operations: TRefactorOperations): AhtmlObject; override;

  public
    function  GetStructureStr(const Depth: Integer = 0;
              const Level: Integer = 0): String; override;
  end;



{                                                                              }
{ ThtmlCDATA                                                                   }
{   Raw content.                                                               }
{                                                                              }
type
  ThtmlCDATA = class(AhtmlTextStringContentObject)
  protected
    function  GetHTMLText: String; override;
    function  GetEncodedHTMLText(const Codec: TCustomUnicodeCodec): RawByteString; override;
  end;



{                                                                              }
{ ThtmlLineBreak                                                               }
{                                                                              }
type
  ThtmlLineBreak = class(AhtmlTextContentObject)
  protected
    function  GetContentText: String; override;
    function  GetHTMLText: String; override;

    function  Refactor(const Operations: TRefactorOperations): AhtmlObject; override;
  end;



{                                                                              }
{ ThtmlCharRef                                                                 }
{   A character reference.                                                     }
{                                                                              }
type
  ThtmlCharRef = class(AhtmlTextContentObject)
  protected
    FCharVal    : LongWord;
    FHasTrailer : Boolean;

    function  GetContentText: String; override;
    function  GetHTMLText: String; override;
    function  GetEncodedHTMLText(const Codec: TCustomUnicodeCodec): RawByteString; override;

    function  GetAsWideChar: WideChar;

    function  Refactor(const Operations: TRefactorOperations): AhtmlObject; override;

  public
    constructor Create(const CharVal: LongWord; const HasTrailer: Boolean); overload;

    function  DuplicateObject: AhtmlObject; override;

    property  CharVal: LongWord read FCharVal write FCharVal;
    property  AsWideChar: WideChar read GetAsWideChar;
  end;



{                                                                              }
{ ThtmlEntityRef                                                               }
{   Entity reference.                                                          }
{                                                                              }
type
  ThtmlEntityRef = class(AhtmlTextContentObject)
  protected
    FHasTrailer : Boolean;

    function  GetContentText: String; override;
    function  GetHTMLText: String; override;
    function  GetEncodedHTMLText(const Codec: TCustomUnicodeCodec): RawByteString; override;

    function  Refactor(const Operations: TRefactorOperations): AhtmlObject; override;

  public
    constructor Create(const EntityName: String; const HasTrailer: Boolean); overload;

    property  EntityName: String read FName write FName;
  end;



{                                                                              }
{ ThtmlComment                                                                 }
{   A comment tag.                                                             }
{                                                                              }
type
  ThtmlComment = class(AhtmlObject)
  protected
    FComments: StringArray;

    function  GetHTMLText: String; override;

  public
    constructor Create(const Comments: StringArray); overload;

    function  DuplicateObject: AhtmlObject; override;
    property  Comments: StringArray read FComments write FComments;
  end;

  ThtmlEmptyComment = class(AhtmlObject)
  protected
    function  GetHTMLText: String; override;
  end;



{                                                                              }
{ ThtmlPI                                                                      }
{   A "Processing Information" tag.                                            }
{                                                                              }
type
  ThtmlPI = class(AhtmlObject)
  protected
    FTarget, FPI: String;

    function  GetHTMLText: String; override;

  public
    constructor Create(const Target, PI: String); overload;

    function  DuplicateObject: AhtmlObject; override;
    property  Target: String read FTarget write FTarget;
    property  PI: String read FPI write FPI;
  end;



{                                                                              }
{ ThtmlRawTag                                                                  }
{   A raw un-interpreted tag.                                                  }
{                                                                              }
type
  ThtmlRawTag = class(AhtmlObject)
  protected
    FTag: String;

    function  GetHTMLText: String; override;

  public
    constructor Create(const Tag: String); overload;

    function  DuplicateObject: AhtmlObject; override;
    property  Tag: String read FTag write FTag;
  end;



{                                                                              }
{ ThtmlElementAttribute                                                        }
{   Element attribute.                                                         }
{   The object's content is the attribute value.                               }
{                                                                              }
type
  ThtmlElementAttribute = class(AhtmlContainerObject)
  protected
    FAttrID: ThtmlAttrID;

    function  GetName: String; override;
    function  GetHTMLText: String; override;
    function  GetEncodedHTMLText(const Codec: TCustomUnicodeCodec): RawByteString; override;

  public
    constructor Create(const AttrID: ThtmlAttrID; const Name: String); overload;

    property  AttrID: ThtmlAttrID read FAttrID;

    function  DuplicateObject: AhtmlObject; override;
  end;



{                                                                              }
{ ThtmlElementAttributes                                                       }
{   Element attribute collection.                                              }
{                                                                              }
type
  ThtmlElementAttributes = class(AhtmlContainerObject)
  protected
    function  GetContentText: String; override;
    procedure SetContentText(const ContentText: String); override;

    function  GetHTMLText: String; override;
    function  GetEncodedHTMLText(const Codec: TCustomUnicodeCodec): RawByteString; override;

    function  GetAttributeText(const Name: String): String;
    procedure SetAttributeText(const Name: String; const Value: String);
    function  GetAttributeFlag(const Name: String): Boolean;
    procedure SetAttributeFlag(const Name: String; const Value: Boolean);

  public
    function  CreateItem(const ID: Integer; const Name: String): AhtmlObject; override;

    function  HasAttribute(const Name: String): Boolean;
    property  AttributeText[const Name: String]: String
              read GetAttributeText write SetAttributeText;
    property  AttributeFlag[const Name: String]: Boolean
              read GetAttributeFlag write SetAttributeFlag;
    procedure DeleteAttribute(const Name: String);
  end;



{                                                                              }
{ AhtmlElement                                                                 }
{   Base class for element implementations.                                    }
{                                                                              }
type
  ThtmlElementTagType = (ttContentTags, ttEmptyTag, ttStartTag);
  AhtmlElement = class(AhtmlContainerObject)
  protected
    FTagID      : ThtmlTagID;
    FTagType    : ThtmlElementTagType;
    FAttributes : ThtmlElementAttributes;
    FStyleAttr  : ThtmlcssDeclarations;
    FStyleInfo  : ThtmlcssElementInfo;

    function  GetName: String; override;
    procedure SetContentText(const ContentText: String); override;
    function  GetHTMLText: String; override;
    function  GetEncodedHTMLText(const Codec: TCustomUnicodeCodec): RawByteString; override;

    procedure AssignElementInfo(const Source: AhtmlElement);
    function  GetAttributes: ThtmlElementAttributes;
    function  GetAttributeText(const Name: String): String;
    procedure SetAttributeText(const Name: String; const Text: String);
    function  GetAttributeFlag(const Name: String): Boolean;
    procedure SetAttributeFlag(const Name: String; const Value: Boolean);

    procedure InitStyleElementInfo(const StyleSheet: ThtmlCSS;
              const ParentInfo: PhtmlcssElementInfo); override;

    procedure InitElementStyleInfo(const StyleSheet: ThtmlCSS;
              var Info: ThtmlcssStyleProperties; const StyleInfo: ThtmlcssStyleProperties);
    procedure InitChildrenStyleInfo(const StyleSheet: ThtmlCSS;
              const StyleInfo: ThtmlcssStyleProperties);
    procedure InitStyleInfo(const StyleSheet: ThtmlCSS;
              const StyleInfo: ThtmlcssStyleProperties); override;
              
    procedure ApplyHTMLStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
              const ParentStyle: ThtmlcssStyleProperties); virtual;

  public
    constructor Create(const TagID: ThtmlTagID; const Name: String;
                const TagType: ThtmlElementTagType); overload;
    destructor Destroy; override;

    property  TagID: ThtmlTagID read FTagID write FTagID;
    property  TagType: ThtmlElementTagType read FTagType write FTagType;

    function  DuplicateObject: AhtmlObject; override;
    function  DuplicateElement: AhtmlElement; virtual;

    function  HasAttribute(const Name: String): Boolean;
    property  Attributes: ThtmlElementAttributes read GetAttributes;
    property  AttributeText[const Name: String]: String
              read GetAttributeText write SetAttributeText;
    property  AttributeFlag[const Name: String]: Boolean
              read GetAttributeFlag write SetAttributeFlag;
  end;
  ThtmlElement = class(AhtmlElement);
  ThtmlElementClass = class of AhtmlElement;



{                                                                              }
{ AhtmlKnownElement                                                            }
{   Base class for HTML elements that can be identified with a TagID.          }
{                                                                              }
type
  AhtmlKnownElement = class(AhtmlElement)
  public
    constructor Create(const TagID: ThtmlTagID;
                const TagType: ThtmlElementTagType); overload;
  end;



implementation

uses
  { Fundamentals }
  flcASCII,
  flcUtils,
  flcStrings,
  flcUTF,

  { HTML }
  flcHTMLCharEntity,
  flcHTMLProperties,
  flcHTMLDocElements;



{                                                                              }
{ EncodeHTMLText                                                               }
{   Helper function that encodes HTML text using a specific Unicode Codec.     }
{                                                                              }
function EncodeHTMLText(const WideHTMLText: PWideChar; const Len: Integer;
    const Codec: TCustomUnicodeCodec; const CanUseRefs: Boolean;
    const ReplaceText: WideString): RawByteString;
var I, L : Integer;
    P : PWideChar;
    T : RawByteString;
    U : String;
begin
  Assert(Assigned(Codec), 'Assigned(Codec)');
  Assert(Codec.ErrorAction = eaStop, 'Codec.ErrorAction = eaStop');
  L := Len;
  if L <= 0 then
    begin
      Result := '';
      exit;
    end;
  // Encode using Codec, optionally replace unencodable characters with
  // their character references
  P := WideHTMLText;
  Result := '';
  while L > 0 do
    begin
      T := Codec.Encode(P, L, I);
      if I > 0 then // bytes encoded
        begin
          Inc(P, I);
          Dec(L, I);
          Result := Result + T;
        end;
      if L > 0 then // unencodable character
        begin
          if CanUseRefs then
            begin
              // replace with encoded character reference
              U := htmlCharRef(Ord(P^), False);
              Result := Result + EncodeHTMLText(Pointer(U), Length(U),
                  Codec, False, '');
            end else
            if ReplaceText <> '' then
              // replace with encoded ReplaceText
              Result := Result + EncodeHTMLText(Pointer(ReplaceText), Length(ReplaceText),
                  Codec, False, '');
          // skip character
          Inc(P);
          Dec(L);
        end;
    end;
end;



{                                                                              }
{ AhtmlObject                                                                  }
{                                                                              }
constructor AhtmlObject.Create;
begin
  inherited Create;
  Init;
end;

procedure AhtmlObject.Init;
begin
end;

function AhtmlObject.DuplicateObject: AhtmlObject;
begin
  Result := AhtmlObject(ClassType.Create);
  Result.FName := FName;
  Result.FStyle := FStyle;
end;

function AhtmlObject.GetName: String;
begin
  Result := FName;
end;

function AhtmlObject.GetNameUTF8: RawByteString;
begin
  Result := StringToUTF8String(GetName);
end;

function AhtmlObject.GetStructureStr(const Depth: Integer;
    const Level: Integer): String;
begin
  Result := GetName;
  if Result = '' then
    Result := '{' + StrExclPrefixU(StrExclPrefixU(ClassName, 'T'), 'html', False) + '}';
end;

function AhtmlObject.GetContentText: String;
begin
  raise EhtmlObject.Create('Cannot get content');
end;

procedure AhtmlObject.SetContentText(const ContentText: String);
begin
  raise EhtmlObject.Create('Cannot set content');
end;

function AhtmlObject.GetContentTextUTF8: RawByteString;
begin
  Result := StringToUTF8String(GetContentText);
end;

procedure AhtmlObject.SetContentTextUTF8(const ContentText: RawByteString);
begin
  SetContentText(UTF8StringToString(ContentText));
end;

function AhtmlObject.GetHTMLText: String;
begin
  raise EhtmlObject.Create('Cannot get HTML text');
end;

function AhtmlObject.GetHTMLTextUTF8: RawByteString;
begin
  Result := StringToUTF8String(GetHTMLText);
end;

function AhtmlObject.GetEncodedHTMLText(const Codec: TCustomUnicodeCodec): RawByteString;
var S: String;
begin
  S := GetHTMLText;
  Result := EncodeHTMLText(Pointer(S), Length(S), Codec, True, '');
end;

function AhtmlObject.EncodedHTMLText(const Codec: TCustomUnicodeCodec): RawByteString;
begin
  if not Assigned(Codec) then
    raise EhtmlObject.Create('No codec');
  Codec.ErrorAction := eaStop;
  Result := GetEncodedHTMLText(Codec);
end;

procedure AhtmlObject.InitStyleElementInfo(const StyleSheet: ThtmlCSS;
    const ParentInfo: PhtmlcssElementInfo);
begin
end;

procedure AhtmlObject.InitStyleInfo(const StyleSheet: ThtmlCSS;
    const StyleInfo: ThtmlcssStyleProperties);
begin
  FStyle := StyleInfo;
end;

function AhtmlObject.Refactor(const Operations: TRefactorOperations): AhtmlObject;
begin
  Result := self;
end;



{                                                                              }
{ AhtmlContainerObject                                                         }
{                                                                              }
destructor AhtmlContainerObject.Destroy;
begin
  ClearItems;
  inherited Destroy;
end;

procedure AhtmlContainerObject.ClearItems;
var I, N: AhtmlObject;
begin
  I := FFirst;
  while Assigned(I) do
    begin
      N := I.FNext;
      I.Free;
      if I = FLast then
        break;
      I := N;
    end;
  FFirst := nil;
  FLast := nil;
end;

function AhtmlContainerObject.DuplicateObject: AhtmlObject;
var I: AhtmlObject;
begin
  Result := inherited DuplicateObject;
  I := FFirst;
  while Assigned(I) do
    begin
      AhtmlContainerObject(Result).AddItem(I.DuplicateObject);
      I := I.FNext;
    end;
end;

function AhtmlContainerObject.GetStructureStr(const Depth: Integer;
    const Level: Integer): String;
var I: AhtmlObject;
begin
  if (Depth > 0) and (Level > Depth) then
    begin
      Result := '';
      exit;
    end;
  Result := GetName;
  if (Depth <= 0) or (Level < Depth) then
    begin
      I := FFirst;
      if Assigned(I) then
        begin
          Result := WideCRLF + DupStrU('  ', Level) + Result + '[';
          while Assigned(I) do
            begin
              if I <> FFirst then
                Result := Result + ',';
              Result := Result + I.GetStructureStr(Depth, Level + 1);
              I := I.FNext;
            end;
          Result := Result + WideCRLF + DupStrU('  ', Level) + ']';
        end else
        Result := Result + '[]';
    end;
end;

function AhtmlContainerObject.GetContentText: String;
var I: AhtmlObject;
begin
  Result := '';
  I := FFirst;
  while Assigned(I) do
    begin
      Result := Result + I.GetContentText;
      I := I.FNext;
    end;
end;

procedure AhtmlContainerObject.SetContentText(const ContentText: String);
var T: Boolean;
begin
  T := Assigned(FFirst) and (FFirst = FLast);
  if T then
    begin
      T := FFirst is ThtmlText;
      if T then
        ThtmlText(FFirst).ContentText := ContentText;
    end;
  if not T then
    begin
      ClearItems;
      AddItem(ThtmlText.Create(ContentText));
    end;
end;

{
function AhtmlContainerObject.GetHTMLTextUTF8: RawByteString;
var I: AhtmlObject;
begin
  Result := '';
  I := FFirst;
  while Assigned(I) do
    begin
      Result := Result + I.GetHTMLTextUTF8;
      I := I.FNext;
    end;
end;
}

function AhtmlContainerObject.GetHTMLText: String;
var I: AhtmlObject;
begin
  Result := '';
  I := FFirst;
  while Assigned(I) do
    begin
      Result := Result + I.GetHTMLText;
      I := I.FNext;
    end;
end;

function AhtmlContainerObject.GetEncodedHTMLText(const Codec: TCustomUnicodeCodec): RawByteString;
var I: AhtmlObject;
begin
  Result := '';
  I := FFirst;
  while Assigned(I) do
    begin
      Result := Result + I.GetEncodedHTMLText(Codec);
      I := I.FNext;
    end;
end;

procedure AhtmlContainerObject.AddItem(const Obj: AhtmlObject);
begin
  Obj.FParent := self;
  Obj.FNext := nil;
  Obj.FPrev := FLast;
  if Assigned(FLast) then
    FLast.FNext := Obj else
    FFirst := Obj;
  FLast := Obj;
end;

procedure AhtmlContainerObject.DeleteChild(const Obj: AhtmlObject);
begin
  Obj.FParent := nil;
  if Assigned(Obj.FPrev) then
    Obj.FPrev.FNext := Obj.FNext;
  if Assigned(Obj.FNext) then
    Obj.FNext.FPrev := Obj.FPrev;
  if FFirst = Obj then
    FFirst := Obj.FNext;
  if FLast = Obj then
    FLast := Obj.FPrev;
end;

procedure AhtmlContainerObject.ReplaceChild(const Obj, NewObj: AhtmlObject);
begin
  NewObj.FParent := self;
  NewObj.FNext := Obj.FNext;
  NewObj.FPrev := Obj.FPrev;
  if Assigned(Obj.FNext) then
    Obj.FNext.FPrev := NewObj;
  if Assigned(Obj.FPrev) then
    Obj.FPrev.FNext := NewObj;
  if FFirst = Obj then
    FFirst := NewObj;
  if FLast = Obj then
    FLast := NewObj;
end;

function AhtmlContainerObject.FindNext(const Previous: AhtmlObject;
    const ItemClass: AhtmlObjectClass): AhtmlObject;
begin
  if Assigned(Previous) then
    Result := Previous.FNext else
    Result := FFirst;
  while Assigned(Result) do
    if Result.InheritsFrom(ItemClass) then
      exit
    else
      Result := Result.FNext;
end;

function AhtmlContainerObject.FindNextName(const Previous: AhtmlObject;
    const Name: String): AhtmlObject;
begin
  if Assigned(Previous) then
    Result := Previous.FNext
  else
    Result := FFirst;
  while Assigned(Result) do
    if StrEqualNoAsciiCaseU(Name, Result.Name) then
      exit
    else
      Result := Result.FNext;
end;

function AhtmlContainerObject.GetItemByClass(const ItemClass: AhtmlObjectClass): AhtmlObject;
begin
  Result := FindNext(nil, ItemClass);
end;

function AhtmlContainerObject.RequireItemByClass(const ItemClass: AhtmlObjectClass): AhtmlObject;
begin
  Result := GetItemByClass(ItemClass);
  if Assigned(Result) then
    exit;
  Result := ItemClass.Create;
  AddItem(Result);
end;

function AhtmlContainerObject.GetItemTextByClass(const ItemClass: AhtmlObjectClass): RawByteString;
var V: AhtmlObject;
begin
  V := GetItemByClass(ItemClass);
  if Assigned(V) then
    Result := V.ContentTextUTF8
  else
    Result := '';
end;

function AhtmlContainerObject.CreateItem(const ID: Integer; const Name: String): AhtmlObject;
begin
  if FParent is AhtmlContainerObject then
    Result := AhtmlContainerObject(FParent).CreateItem(ID, Name)
  else
    Result := htmlCreateElement(ThtmlTagID(ID), Name);
end;

function AhtmlContainerObject.GetItem(const Name: String): AhtmlObject;
begin
  Result := FindNextName(nil, Name);
end;

procedure AhtmlContainerObject.SetItem(const Name: String; const Item: AhtmlObject);
var V: AhtmlObject;
begin
  V := FindNextName(nil, Name);
  if not Assigned(V) then
    AddItem(Item)
  else
    begin
      Item.FParent := self;
      Item.FNext := V.FNext;
      Item.FPrev := V.FPrev;
      if Assigned(Item.FPrev) then
        Item.FPrev.FNext := Item;
      if Assigned(Item.FNext) then
        Item.FNext.FPrev := Item;
      if FFirst = V then
        FFirst := Item;
      if FLast = V then
        FLast := Item;
      V.Free;
    end;
end;

function AhtmlContainerObject.RequireItem(const Name: String;
    const ItemClass: AhtmlObjectClass): AhtmlObject;
begin
  Result := GetItem(Name);
  if Assigned(Result) then
    exit;
  if not Assigned(ItemClass) then
    begin // class not specified
      Result := CreateItem(-1, Name); // request item from container
      if not Assigned(Result) then
        raise EhtmlContainerObject.Create('Can not create item');
    end
  else
    Result := ItemClass.Create; // create specified item class
  AddItem(Result);
end;

function AhtmlContainerObject.GetItemText(const Name: String): String;
var T: AhtmlObject;
begin
  T := GetItem(Name);
  if not Assigned(T) then
    Result := ''
  else
    Result := T.ContentText;
end;

procedure AhtmlContainerObject.SetItemText(const Name: String; const Text: String);
begin
  RequireItem(Name, nil).ContentText := Text;
end;

procedure AhtmlContainerObject.InitStyleElementInfo(const StyleSheet: ThtmlCSS;
    const ParentInfo: PhtmlcssElementInfo);
var I: AhtmlObject;
begin
  inherited InitStyleElementInfo(StyleSheet, ParentInfo);
  // Init children's element info
  I := FFirst;
  while Assigned(I) do
    begin
      Assert(I.FParent = self, 'I.FParent = self');
      I.InitStyleElementInfo(StyleSheet, ParentInfo);
      if I = FLast then
        break;
      I := I.FNext;
    end;
end;

procedure AhtmlContainerObject.InitStyleInfo(const StyleSheet: ThtmlCSS;
    const StyleInfo: ThtmlcssStyleProperties);
var I: AhtmlObject;
begin
  inherited;
  // Init children's style info
  I := FFirst;
  while Assigned(I) do
    begin
      Assert(I.FParent = self, 'I.FParent = self');
      I.InitStyleInfo(StyleSheet, StyleInfo);
      if I = FLast then
        break;
      I := I.FNext;
    end;
end;

function AhtmlContainerObject.Refactor(const Operations: TRefactorOperations): AhtmlObject;
var I, J, N : AhtmlObject;
    R       : Boolean;
begin
  Result := self;
  // Refactor children
  I := FFirst;
  while Assigned(I) do
    begin
      Assert(I.FParent = self, 'I.FParent = self');
      J := I.Refactor(Operations);
      R := I = FLast;
      N := I.FNext;
      if not Assigned(J) then // Delete child
        begin
          DeleteChild(I);
          I.Free;
        end else
        if J <> I then // Replace child
          begin
            ReplaceChild(I, J);
            I.Free;
          end;
      if R then
        break;
      I := N;
    end;
end;



{                                                                              }
{ AhtmlTextStringContentObject                                                 }
{                                                                              }
constructor AhtmlTextStringContentObject.Create(const Text: String);
begin
  inherited Create;
  FText := Text;
end;

constructor AhtmlTextStringContentObject.CreateWide(const Text: WideString);
begin
  inherited Create;
  SetContentText(Text);
end;

function AhtmlTextStringContentObject.DuplicateObject: AhtmlObject;
begin
  Result := inherited DuplicateObject;
  AhtmlTextStringContentObject(Result).FText := FText;
end;

function AhtmlTextStringContentObject.GetContentText: String;
begin
  Result := FText;
end;

procedure AhtmlTextStringContentObject.SetContentText(const ContentText: String);
begin
  FText := ContentText;
end;



{                                                                              }
{ ThtmlText                                                                    }
{                                                                              }
procedure htmlSafeText(var S: String);
begin
  S := StrReplaceCharStrU('&', '&amp;',  S);
  S := StrReplaceCharStrU('<', '&lt;',   S);
  S := StrReplaceCharStrU('>', '&gt;',   S);
  S := StrReplaceCharStrU('"', '&quot;', S);
  S := StrReplaceCharStrU(#39, '&apos;', S);
end;

function ThtmlText.GetHTMLText: String;
begin
  Result := FText;
  htmlSafeText(Result);
end;

function ThtmlText.GetEncodedHTMLText(const Codec: TCustomUnicodeCodec): RawByteString;
var S: WideString;
begin
  S := GetHTMLText;
  Result := EncodeHTMLText(Pointer(S), Length(S), Codec, True, '');
end;

function ThtmlText.GetStructureStr(const Depth: Integer;
    const Level: Integer): String;
begin
  Result := '"' +
      StrReplaceCharU([#0..#31], WideChar('#'),
          CopyLeftEllipsedU(GetContentText, 15)) + '"';
end;

function ThtmlText.IsStartWhiteSpace: Boolean;
var P: PAnsiChar;
begin
  Result := False;
  if FText = '' then
    exit;
  P := Pointer(FText);
  Result := P^ in [#0..#32];
end;

function ThtmlText.IsEndWhiteSpace: Boolean;
var P: PAnsiChar;
begin
  Result := False;
  if FText = '' then
    exit;
  P := Pointer(FText);
  Inc(P, Length(FText) - 1);
  Result := P^ in [#0..#32];
end;

function ThtmlText.Refactor(const Operations: TRefactorOperations): AhtmlObject;
begin
  Result := self;
  if reopRefactorForLayout in Operations then
    begin
      // prepare whitespace
      FText := StrReplaceCharU([#0..#31], WideChar(#32), FText);
      if FStyle.WhiteSpace = wsPre then
        exit;
      // Not pre-formatted style
      // Remove consecutive whitespaces
      FText := StrRemoveDupU(FText, #32);
      // Trim preceding white space if previous object ends with a white space
      if IsStartWhiteSpace and Assigned(FPrev) and
         ((FPrev is ThtmlLineBreak) or
          ((FPrev is ThtmlText) and ThtmlText(FPrev).IsEndWhiteSpace)) then
        StrTrimLeftInPlaceU(FText, [#0..#32]);
    end;
end;



{                                                                              }
{ ThtmlLineBreak                                                               }
{                                                                              }
function ThtmlLineBreak.GetContentText: String;
begin
  Result := WideCRLF;
end;

function ThtmlLineBreak.GetHTMLText: String;
begin
  Result := WideCRLF;
end;

function ThtmlLineBreak.Refactor(const Operations: TRefactorOperations): AhtmlObject;
begin
  Result := self;
  if reopRefactorForLayout in Operations then
    begin
      if FStyle.WhiteSpace = wsPre then
        exit;
      // Not pre-formatted style
      // Delete line-break if its preceded by white-space
      if Assigned(FPrev) and
         ((FPrev is ThtmlLineBreak) or
          ((FPrev is ThtmlText) and ThtmlText(FPrev).IsEndWhiteSpace)) then
        begin
          Result := nil;
          exit;
        end;
    end;
end;



{                                                                              }
{ ThtmlCDATA                                                                   }
{                                                                              }
function ThtmlCDATA.GetHTMLText: String;
begin
  Result := '<![CDATA[' + FText + ']]>';
end;

function ThtmlCDATA.GetEncodedHTMLText(const Codec: TCustomUnicodeCodec): RawByteString;
var S: String;
begin
  S := GetHTMLText;
  Result := EncodeHTMLText(Pointer(S), Length(S), Codec, False, '');
end;



{                                                                              }
{ ThtmlCharRef                                                                 }
{                                                                              }
constructor ThtmlCharRef.Create(const CharVal: LongWord; const HasTrailer: Boolean);
begin
  inherited Create;
  FCharVal := CharVal;
  FHasTrailer := HasTrailer;
end;

function ThtmlCharRef.GetAsWideChar: WideChar;
begin
  if FCharVal <= $FFFF then
    Result := WideChar(FCharVal) else
    Result := WideChar(#$FFFD); // Unicode replacement character
end;

function ThtmlCharRef.DuplicateObject: AhtmlObject;
begin
  Result := inherited DuplicateObject;
  ThtmlCharRef(Result).FCharVal := FCharVal;
end;

function ThtmlCharRef.GetContentText: String;
begin
  SetLength(Result, 1);
  PWideChar(Pointer(Result))^ := GetAsWideChar;
end;

function ThtmlCharRef.GetHTMLText: String;
begin
  Result := '&#' + Word32ToStrU(FCharVal) + ';';
end;

function ThtmlCharRef.GetEncodedHTMLText(const Codec: TCustomUnicodeCodec): RawByteString;
var S: WideString;
begin
  S := GetHTMLText;
  Result := EncodeHTMLText(Pointer(S), Length(S), Codec, False, '');
end;

function ThtmlCharRef.Refactor(const Operations: TRefactorOperations): AhtmlObject;
begin
  if reopRefactorForLayout in Operations then
    // Replace with resolved text object
    Result := ThtmlText.Create(GetContentText)
  else
    Result := self;
end;



{                                                                              }
{ ThtmlEntityRef                                                               }
{                                                                              }
constructor ThtmlEntityRef.Create(const EntityName: String; const HasTrailer: Boolean);
begin
  inherited Create;
  FName := EntityName;
  FHasTrailer := HasTrailer;
end;

function ThtmlEntityRef.GetContentText: String;
var C: Word;
begin
  C := htmlDecodeCharEntity(FName);
  if C > 0 then
    begin
      SetLength(Result, 1);
      PWideChar(Pointer(Result))^ := WideChar(C);
    end else
    begin // unresolved
      Result := '&' + GetName;
      if FHasTrailer then
        Result := Result + ';';
    end;
end;

function ThtmlEntityRef.GetHTMLText: String;
begin
  Result := '&' + FName + ';';
end;

function ThtmlEntityRef.GetEncodedHTMLText(const Codec: TCustomUnicodeCodec): RawByteString;
var S: WideString;
begin
  S := GetHTMLText;
  Result := EncodeHTMLText(Pointer(S), Length(S), Codec, False, '');
end;

function ThtmlEntityRef.Refactor(const Operations: TRefactorOperations): AhtmlObject;
var S: String;
begin
  Result := self;
  if reopRefactorForLayout in Operations then
    begin
      // Replace with text object
      S := GetContentText;
      if S = '' then
        Result := ThtmlText.Create(GetHTMLText)
      else
        Result := ThtmlText.Create(S);
    end;
end;



{                                                                              }
{ ThtmlComment                                                                 }
{                                                                              }
constructor ThtmlComment.Create(const Comments: StringArray);
begin
  inherited Create;
  FComments := Comments;
end;

function ThtmlComment.DuplicateObject: AhtmlObject;
begin
  Result := inherited DuplicateObject;
  ThtmlComment(Result).FComments := Comments;
end;

function ThtmlComment.GetHTMLText: String;
var I: Integer;
begin
  Result := '<!--';
  for I := 0 to Length(FComments) - 1 do
    Result := Result + iifU(I > 0, ' ', '') + FComments[I];
  Result := Result + '-->';
end;



{                                                                              }
{ ThtmlEmptyComment                                                            }
{                                                                              }
function ThtmlEmptyComment.GetHTMLText: String;
begin
  Result := '<!>';
end;



{                                                                              }
{ ThtmlPI                                                                      }
{                                                                              }
constructor ThtmlPI.Create(const Target, PI: String);
begin
  inherited Create;
  FTarget := Target;
  FPI := PI;
end;

function ThtmlPI.DuplicateObject: AhtmlObject;
begin
  Result := inherited DuplicateObject;
  ThtmlPI(Result).FTarget := FTarget;
  ThtmlPI(Result).FPI := FPI;
end;

function ThtmlPI.GetHTMLText: String;
begin
  Result := '<?' + FTarget + ' ' + FPI + '>';
end;



{                                                                              }
{ ThtmlRawTag                                                                  }
{                                                                              }
constructor ThtmlRawTag.Create(const Tag: String);
begin
  inherited Create;
  FTag := Tag;
end;

function ThtmlRawTag.DuplicateObject: AhtmlObject;
begin
  Result := inherited DuplicateObject;
  ThtmlRawTag(Result).FTag := FTag;
end;

function ThtmlRawTag.GetHTMLText: String;
begin
  Result := '<' + FTag + '>';
end;



{                                                                              }
{ ThtmlElementAttribute                                                        }
{                                                                              }
constructor ThtmlElementAttribute.Create(const AttrID: ThtmlAttrID; const Name: String);
begin
  inherited Create;
  FAttrID := AttrID;
  FName := Name;
end;

function ThtmlElementAttribute.DuplicateObject: AhtmlObject;
begin
  Result := inherited DuplicateObject;
  ThtmlElementAttribute(Result).FAttrID := FAttrID;
end;

function ThtmlElementAttribute.GetName: String;
begin
  if (FName = '') and (FAttrID <> HTML_ATTR_None) then
    FName := htmlGetAttrName(FAttrID);
  Result := FName;
end;

{
function ThtmlElementAttribute.GetHTMLTextUTF8: RawByteString;
begin
  Result := NameUTF8;
  if Assigned(FFirst) then
    Result := Result + '="' + inherited GetHTMLTextUTF8 + '"';
end;
}

function ThtmlElementAttribute.GetHTMLText: String;
begin
  Result := Name;
  if Assigned(FFirst) then
    Result := Result + '="' + inherited GetHTMLText + '"';
end;

function ThtmlElementAttribute.GetEncodedHTMLText(const Codec: TCustomUnicodeCodec): RawByteString;
var S: String;
begin
  S := Name;
  Result := EncodeHTMLText(Pointer(S), Length(S), Codec, False, '');
  if Assigned(FFirst) then
    Result := Result + Codec.EncodeStr('="') +
                       inherited GetEncodedHTMLText(Codec) +
                       Codec.EncodeStr('"');
end;



{                                                                              }
{ ThtmlElementAttributes                                                       }
{                                                                              }
function ThtmlElementAttributes.GetContentText: String;
begin
  raise EhtmlContainerObject.Create('No content');
end;

procedure ThtmlElementAttributes.SetContentText(const ContentText: String);
begin
  raise EhtmlContainerObject.Create('Can not set content');
end;

function ThtmlElementAttributes.CreateItem(const ID: Integer; const Name: String): AhtmlObject;
begin
  Result := ThtmlElementAttribute.Create(ThtmlAttrID(ID), Name);
end;

{
function ThtmlElementAttributes.GetHTMLTextUTF8: RawByteString;
var I: AhtmlObject;
begin
  Result := '';
  I := FFirst;
  while Assigned(I) do
    begin
      Result := Result + ' ' + I.GetHTMLTextUTF8;
      I := I.FNext;
    end;
end;
}

function ThtmlElementAttributes.GetHTMLText: String;
var I: AhtmlObject;
begin
  Result := '';
  I := FFirst;
  while Assigned(I) do
    begin
      Result := Result + ' ' + I.GetHTMLText;
      I := I.FNext;
    end;
end;

function ThtmlElementAttributes.GetEncodedHTMLText(const Codec: TCustomUnicodeCodec): RawByteString;
var I: AhtmlObject;
begin
  Result := '';
  I := FFirst;
  while Assigned(I) do
    begin
      Result := Result + Codec.EncodeStr(' ') + I.GetEncodedHTMLText(Codec);
      I := I.FNext;
    end;
end;

function ThtmlElementAttributes.HasAttribute(const Name: String): Boolean;
begin
  Result := Assigned(Item[Name]);
end;

function ThtmlElementAttributes.GetAttributeText(const Name: String): String;
begin
  Result := ItemText[Name];
end;

procedure ThtmlElementAttributes.SetAttributeText(const Name: String; const Value: String);
begin
  ItemText[Name] := Value;
end;

function ThtmlElementAttributes.GetAttributeFlag(const Name: String): Boolean;
begin
  Result := HasAttribute(Name);
end;

procedure ThtmlElementAttributes.SetAttributeFlag(const Name: String; const Value: Boolean);
begin
  if GetAttributeFlag(Name) = Value then
    exit;
  if Value then
    AddItem(CreateItem(-1, Name)) else
    DeleteAttribute(Name);
end;

procedure ThtmlElementAttributes.DeleteAttribute(const Name: String);
var I: AhtmlObject;
begin
  I := FindNextName(nil, Name);
  if Assigned(I) then
    DeleteChild(I);
end;



{                                                                              }
{ AhtmlElement                                                                 }
{                                                                              }
constructor AhtmlElement.Create(const TagID: ThtmlTagID; const Name: String;
    const TagType: ThtmlElementTagType);
begin
  inherited Create;
  FName := Name;
  FTagID := TagID;
  FTagType := TagType;
end;

destructor AhtmlElement.Destroy;
begin
  FreeAndNil(FAttributes);
  FreeAndNil(FStyleAttr);
  inherited Destroy;
end;

function AhtmlElement.GetName: String;
begin
  Result := FName;
  if (Result = '') and (FTagID <> HTML_TAG_None) then
    begin
      FName := htmlGetTagName(FTagID);
      Result := FName;
    end;
end;

procedure AhtmlElement.AssignElementInfo(const Source: AhtmlElement);
begin
  FName := Source.FName;
  FTagType := Source.FTagType;
  FTagID := Source.FTagID;
  if Assigned(Source.FAttributes) then
    FAttributes := ThtmlElementAttributes(Source.FAttributes.DuplicateObject);
end;

function AhtmlElement.DuplicateObject: AhtmlObject;
begin
  Result := inherited DuplicateObject;
  AhtmlElement(Result).AssignElementInfo(self);
end;

function AhtmlElement.DuplicateElement: AhtmlElement;
begin
  Result := AhtmlElement(ClassType.Create);
  Result.AssignElementInfo(self);
end;

function AhtmlElement.GetAttributes: ThtmlElementAttributes;
begin
  if not Assigned(FAttributes) then
    FAttributes := ThtmlElementAttributes.Create;
  Result := FAttributes;
end;

procedure AhtmlElement.SetContentText(const ContentText: String);
begin
  if FTagType <> ttContentTags then
    raise EhtmlContainerObject.Create('Tag(' + Name + ') cannot have content');
  inherited;
end;

{
function AhtmlElement.GetHTMLTextUTF8: RawByteString;
begin
  Result := '<' + NameUTF8;
  if Assigned(FAttributes) then
    Result := Result + FAttributes.GetHTMLTextUTF8;
  case FTagType of
    ttEmptyTag : Result := Result + '/>';
    ttStartTag : Result := Result + '>';
  else
    Result := Result + '>' + inherited GetHTMLTextUTF8 + '</' + NameUTF8 + '>';
  end;
end;
}

function AhtmlElement.GetHTMLText: String;
var N: String;
begin
  N := Name;
  Result := '<' + N;
  if Assigned(FAttributes) then
    Result := Result + FAttributes.GetHTMLText;
  case FTagType of
    ttEmptyTag : Result := Result + '/>';
    ttStartTag : Result := Result + '>';
  else
    Result := Result + '>' + inherited GetHTMLText + '</' + N + '>';
  end;
end;

function AhtmlElement.GetEncodedHTMLText(const Codec: TCustomUnicodeCodec): RawByteString;
var N, S: RawByteString;
begin
  N := Codec.EncodeStr(UTF8StringToString(NameUTF8));
  Result := Codec.EncodeStr('<') + N;
  if Assigned(FAttributes) then
    Result := Result + FAttributes.GetEncodedHTMLText(Codec);
  case FTagType of
    ttEmptyTag : Result := Result + Codec.EncodeStr('/>');
    ttStartTag : Result := Result + Codec.EncodeStr('>');
  else
    begin
      S := Codec.EncodeStr('>');
      Result := Result + S + inherited GetEncodedHTMLText(Codec) +
                         Codec.EncodeStr('</') + N + S;
    end;
  end;
end;

function AhtmlElement.HasAttribute(const Name: String): Boolean;
begin
  if Assigned(FAttributes) then
    Result := FAttributes.HasAttribute(Name)
  else
    Result := False;
end;

function AhtmlElement.GetAttributeText(const Name: String): String;
begin
  if Assigned(FAttributes) then
    Result := FAttributes.AttributeText[Name]
  else
    Result := '';
end;

procedure AhtmlElement.SetAttributeText(const Name: String; const Text: String);
begin
  Attributes.AttributeText[Name] := Text;
end;

function AhtmlElement.GetAttributeFlag(const Name: String): Boolean;
begin
  if not Assigned(FAttributes) then
    Result := False
  else
    Result := FAttributes.AttributeFlag[Name];
end;

procedure AhtmlElement.SetAttributeFlag(const Name: String; const Value: Boolean);
begin
  if GetAttributeFlag(Name) = Value then
    exit;
  Attributes.AttributeFlag[Name] := Value;
end;

procedure AhtmlElement.InitStyleElementInfo(const StyleSheet: ThtmlCSS;
    const ParentInfo: PhtmlcssElementInfo);
var S: String;
    T: StringArray;
    I, L: Integer;
    V: AhtmlObject;
begin
  T := nil;
  // Initialize style parent
  FStyleInfo.ParentInfo := ParentInfo;
  // Initialize ElementID
  S := AttributeText['ID'];
  if S = '' then
    FStyleInfo.ElementID := -1
  else
    FStyleInfo.ElementID := StyleSheet.RequireElementID(S);
  // Initialize ClassIDs
  S := AttributeText['CLASS'];
  if S = '' then
    FStyleInfo.ClassIDs := nil
  else
    begin
      T := StrSplitChar(S, ',');
      L := Length(T);
      SetLength(FStyleInfo.ClassIDs, L);
      for I := 0 to L - 1 do
        FStyleInfo.ClassIDs[I] := StyleSheet.RequireClassID(T[I]);
    end;
  // Initialize TagID
  FStyleInfo.TagID := FTagID;
  // Initialize Style attribute
  S := AttributeText['STYLE'];
  if S = '' then
    FStyleAttr := nil else
    FStyleAttr := StyleSheet.ParseDeclarations(S);
  // Initialize children
  V := FFirst;
  while Assigned(V) do
    begin
      V.InitStyleElementInfo(StyleSheet, @FStyleInfo);
      if V = FLast then
        break;
      V := V.FNext;
    end;
end;

procedure AhtmlElement.ApplyHTMLStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
    const ParentStyle: ThtmlcssStyleProperties);
begin
end;

procedure AhtmlElement.InitElementStyleInfo(const StyleSheet: ThtmlCSS;
    var Info: ThtmlcssStyleProperties; const StyleInfo: ThtmlcssStyleProperties);
begin
  // Apply default style values for element
  flcHTMLStyleSheet.InitElementStyleProperties(Info, StyleInfo, FStyleInfo);
  // Apply style values from stylistic HTML attributes
  ApplyHTMLStyleInfo(Info, StyleInfo);
  // Apply style values from style sheet
  if Assigned(StyleSheet) then
    StyleSheet.ApplyStyleInfo(Info, StyleInfo, FStyleInfo);
  // Apply style attribute values
  if Assigned(FStyleAttr) then
    FStyleAttr.ApplyStyleInfo(Info, StyleInfo);
end;

procedure AhtmlElement.InitChildrenStyleInfo(const StyleSheet: ThtmlCSS;
    const StyleInfo: ThtmlcssStyleProperties);
var I: AhtmlObject;
begin
  I := FFirst;
  while Assigned(I) do
    begin
      I.InitStyleInfo(StyleSheet, StyleInfo);
      if I = FLast then
        break;
      I := I.FNext;
    end;
end;

procedure AhtmlElement.InitStyleInfo(const StyleSheet: ThtmlCSS;
    const StyleInfo: ThtmlcssStyleProperties);
begin
  FStyle := StyleInfo;
  InitElementStyleInfo(StyleSheet, FStyle, StyleInfo);
  InitChildrenStyleInfo(StyleSheet, FStyle);
end;



{                                                                              }
{ AhtmlKnownElement                                                            }
{                                                                              }
constructor AhtmlKnownElement.Create(const TagID: ThtmlTagID;
            const TagType: ThtmlElementTagType);
begin
  Assert(TagID <> HTML_TAG_None, 'TagID <> HTML_TAG_None');
  inherited Create(TagID, '', TagType);
end;



end.

