{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals 5.00 - HTML Parser                          }
{   File name:        flcHTMLParser.pas                                        }
{   File version:     5.07                                                     }
{   Description:      HTML Parser                                              }
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
{   2002/10/30  1.02  Support for overlapping tags.                            }
{   2002/11/02  1.03  Refactored.                                              }
{   2002/11/04  1.04  Optimizations.                                           }
{   2002/12/16  1.05  Improved white-space and line-break handling.            }
{   2015/04/11  1.06  UnicodeString changes.                                   }
{   2019/02/22  5.07  Revise for Fundamentals 5.                               }
{                                                                              }
{******************************************************************************}

{$INCLUDE flcHTML.inc}

unit flcHTMLParser;

interface

uses
  { System }
  SysUtils,
  Classes,

  { Fundamentals }
  flcStdTypes,
  flcDataStructs,

  { HTML }
  flcHTMLElements,
  flcHTMLLexer,
  flcHTMLDocBase,
  flcHTMLDoc;



{                                                                              }
{ ThtmlParser                                                                  }
{                                                                              }
type
  ThtmlParser = class; // forward

  { ThtmlOpenElementInfo                                                       }
  {   Structure to store a nested open element                                 }
  ThtmlOpenElementInfo = class(TDoublyLinkedItem)
  protected
    FTagID   : ThtmlTagID;
    FName    : String;
    FElement : AhtmlElement;

  public
    property  TagID: ThtmlTagID read FTagID write FTagID;
    property  Name: String read FName write FName;
    property  Element: AhtmlElement read FElement write FElement;
  end;

  { ThtmlParserState                                                           }
  {   Class that represents the state of the HTML parser while parsing.        }
  ThtmlParserState = class
  protected
    FParser       : ThtmlParser;
    FDocument     : ThtmlDocument;
    FElementList  : TDoublyLinkedList;
    FFreeList     : TDoublyLinkedList;
    FReopenList   : TDoublyLinkedList;

    function  GetOpenCount: Integer;
    function  GetInnerItem: ThtmlOpenElementInfo;
    function  GetInnerTagID: ThtmlTagID;
    function  GetInnerContainer: AhtmlContainerObject;
    function  GetOuterItem: ThtmlOpenElementInfo;

    function  AddToContainer(const Container: AhtmlContainerObject;
              const Obj: AhtmlObject): Boolean;
    function  AddToInnerContainer(const Obj: AhtmlObject): Boolean;

    procedure OpenElement(const Item: ThtmlOpenElementInfo); overload;
    procedure OpenElement(const TagID: ThtmlTagID; const Name: String;
              const Element: AhtmlElement); overload;
    procedure OpenAutoStartElements(const TagID: ThtmlTagID);

    function  ReleaseElement: ThtmlOpenElementInfo;
    procedure CloseElementToFree;
    procedure CloseElementToReopen;
    procedure ReOpenElements;

    procedure CloseOverlapping(const OpenElement: ThtmlOpenElementInfo);
    procedure CloseElement(const OpenElement: ThtmlOpenElementInfo);
    function  GetOpenElementForEndTag(const TagID: ThtmlTagID;
              const Name: String): ThtmlOpenElementInfo;
    function  GetAutoCloseElementForStartTag(
              const TagID: ThtmlTagID): ThtmlOpenElementInfo;

  public
    constructor Create(const Parser: ThtmlParser);
    destructor Destroy; override;

    procedure Reset;

    property  Parser: ThtmlParser read FParser;
    property  Document: ThtmlDocument read FDocument;

    property  OpenCount: Integer read GetOpenCount;
    property  InnerItem: ThtmlOpenElementInfo read GetInnerItem;
    property  InnerTagID: ThtmlTagID read GetInnerTagID;
    property  InnerContainer: AhtmlContainerObject read GetInnerContainer;
    property  OuterItem: ThtmlOpenElementInfo read GetOuterItem;

    function  LocateTagID(const TagID: ThtmlTagID): ThtmlOpenElementInfo;
    function  LocateName(const Name: String): ThtmlOpenElementInfo;
    function  IsTagIDOpen(const TagID: ThtmlTagID): Boolean;
    function  IsNameOpen(const Name: String): Boolean;
    function  IsBodyOpen: Boolean;
  end;

  { ThtmlParser event types                                                    }
  ThtmlParserEvent = procedure (Sender: ThtmlParser) of object;

  ThtmlParserMessageType = (
      hmWarning,
      hmError);
  ThtmlParserMessageEvent = procedure (Sender: ThtmlParser;
      MsgType: ThtmlParserMessageType; Msg: String) of object;

  ThtmlParserTokenEvent = procedure (Sender: ThtmlParser;
      TokenType: ThtmlTokenType) of object;
  ThtmlParserTokenStrEvent = procedure (Sender: ThtmlParser;
      TokenType: ThtmlTokenType; TokenStr: String) of object;

  ThtmlParserTextContext = (tcContent, tcAttributeValue);
  ThtmlParserTextEvent = procedure (Sender: ThtmlParser;
      TextContext: ThtmlParserTextContext; var Text: String) of object;
  ThtmlParserResolveEntityRefEvent = procedure (Sender: ThtmlParser;
      EntityRef: String;
      var Text: String; var Resolved: Boolean) of object;

  ThtmlParserStringEvent = procedure (Sender: ThtmlParser;
      Text: String) of object;
  ThtmlParserCommentEvent = procedure (Sender: ThtmlParser;
      Comments: array of String) of object;
  ThtmlParserDeclarationEvent = procedure (Sender: ThtmlParser;
      Name, Text: String) of object;
  ThtmlParserProcessingInstrEvent = procedure (Sender: ThtmlParser;
      Target, PI: String) of object;

  ThtmlParserStartTagEvent = procedure (Sender: ThtmlParser;
      TagID: ThtmlTagID; Name: String) of object;
  ThtmlParserStartTagAttributesEvent = procedure (Sender: ThtmlParser;
      TagID: ThtmlTagID; Name: String;
      AttributeNames, AttributeValues: array of String;
      Element: AhtmlElement) of object;
  ThtmlParserEndTagEvent = procedure (Sender: ThtmlParser;
      TagID: ThtmlTagID; Name: String) of object;

  ThtmlParserElementEvent = procedure (Sender: ThtmlParser;
      TagID: ThtmlTagID; Name: String; Element: AhtmlElement) of object;

  ThtmlParserDocumentObjectEvent = procedure (Sender: ThtmlParser;
      const Obj: AhtmlObject) of object;

  { ThtmlParser types                                                          }
  ThtmlParserOptions = set of (
    poDisableNotifications,
    poStopOnError,
    poDontProduceDocument,
    poDontResolveReferences,
    poDontAllowOverlappedTags
    );

  { ThtmlParser                                                                }
  ThtmlParser = class(TComponent)
  protected
    FOptions              : ThtmlParserOptions;
    FRawText              : RawByteString;
    FFileName             : String;
    FEncoding             : RawByteString;
    FOnMessage            : ThtmlParserMessageEvent;
    FOnToken              : ThtmlParserTokenEvent;
    FOnTokenStr           : ThtmlParserTokenStrEvent;
    FOnResolveEntityRef   : ThtmlParserResolveEntityRefEvent;
    FOnText               : ThtmlParserTextEvent;
    FOnUnresolvedText     : ThtmlParserTextEvent;
    FOnLineBreak          : ThtmlParserEvent;
    FOnComment            : ThtmlParserCommentEvent;
    FOnDeclaration        : ThtmlParserDeclarationEvent;
    FOnMarkedSection      : ThtmlParserStringEvent;
    FOnCDATA              : ThtmlParserStringEvent;
    FOnProcessingInstr    : ThtmlParserProcessingInstrEvent;
    FOnStartTag           : ThtmlParserStartTagEvent;
    FOnStartTagAttributes : ThtmlParserStartTagAttributesEvent;
    FOnEndTag             : ThtmlParserEndTagEvent;
    FOnElementOpen        : ThtmlParserElementEvent;
    FOnElementClose       : ThtmlParserElementEvent;
    FOnDocumentObject     : ThtmlParserDocumentObjectEvent;

    FLexer                : ThtmlLexer;
    FState                : ThtmlParserState;
    FTokenType            : ThtmlTokenType;

    procedure LogMessage(const MsgType: ThtmlParserMessageType;
              const Msg: String);
    procedure LogUnexpectedToken;
    procedure LogUnmatchedEndTag(const TagID: ThtmlTagID; const Name: String);

    procedure TriggerElementOpen(const Item: ThtmlOpenElementInfo); virtual;
    procedure TriggerElementClose(const Item: ThtmlOpenElementInfo); virtual;
    procedure TriggerDocumentObject(const Obj: AhtmlObject); virtual;

    function  CreateText(const Context: ThtmlParserTextContext;
              var Text: String): AhtmlObject; virtual;
    function  CreateCharRef(const CharVal: LongWord;
              const HasTrailer: Boolean): AhtmlObject; virtual;
    function  CreateEntityRef(const EntityName: String;
              const HasTrailer: Boolean): AhtmlObject; virtual;
    function  CreateLineBreak: AhtmlObject; virtual;
    function  CreateEmptyComment: AhtmlObject; virtual;
    function  CreateComment(const Comments: StringArray): AhtmlObject; virtual;
    function  CreateDeclaration(const Name, Text: String): AhtmlObject; virtual;
    function  CreateMarkedSection(const Text: String): AhtmlObject; virtual;
    function  CreateCDATA(const CDATA: String): AhtmlObject; virtual;
    function  CreatePI(const Target, PI: String): AhtmlObject; virtual;
    function  CreateElement(const TagID: ThtmlTagID;
              const Name: String): AhtmlElement; virtual;

    function  GetTokenStr: String;
    function  GetNextToken: ThtmlTokenType;
    function  ResolveRef(var Text: String): Boolean;

    function  ParsePlainText: String;
    function  ParseText(const TextContext: ThtmlParserTextContext;
              var Text: String): AhtmlObject;
    function  ParseContentText: AhtmlObject;
    function  ParseLineBreak: AhtmlObject;
    function  ParseComment: AhtmlObject;
    function  ParseDeclaration: AhtmlObject;
    function  ParseMarkedSection: AhtmlObject;
    function  ParseCDATA: AhtmlObject;
    function  ParsePI: AhtmlObject;
    function  ParseTagAttributeValue(
              const Attribute: ThtmlElementAttribute): String;
    function  ParseTagAttribute(const TagID: ThtmlTagID; const TagName: String;
              const Element: AhtmlElement;
              var AttrID: ThtmlAttrID;
              var Name, Value: String): ThtmlElementAttribute;
    procedure ParseTagAttributes(const TagID: ThtmlTagID; const Name: String;
              const Element: AhtmlElement);
    function  ParseStartTag: AhtmlObject;
    function  ParseEndTag: AhtmlObject;

    function  ParseToken: AhtmlObject;

    function  GetDocument: ThtmlDocument;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property  Options: ThtmlParserOptions read FOptions write FOptions;
    property  RawText: RawByteString read FRawText write FRawText;
    property  FileName: String read FFileName write FFileName;
    property  Encoding: RawByteString read FEncoding write FEncoding;

    property  OnMessage: ThtmlParserMessageEvent read FOnMessage write FOnMessage;

    property  OnToken: ThtmlParserTokenEvent read FOnToken write FOnToken;
    property  OnTokenStr: ThtmlParserTokenStrEvent read FOnTokenStr write FOnTokenStr;

    property  OnResolveEntityRef: ThtmlParserResolveEntityRefEvent read FOnResolveEntityRef write FOnResolveEntityRef;
    property  OnText: ThtmlParserTextEvent read FOnText write FOnText;
    property  OnUnresolvedText: ThtmlParserTextEvent read FOnUnresolvedText write FOnUnresolvedText;
    property  OnLineBreak: ThtmlParserEvent read FOnLineBreak write FOnLineBreak;

    property  OnComment: ThtmlParserCommentEvent read FOnComment write FOnComment;
    property  OnDeclaration: ThtmlParserDeclarationEvent read FOnDeclaration write FOnDeclaration;
    property  OnMarkedSection: ThtmlParserStringEvent read FOnMarkedSection write FOnMarkedSection;
    property  OnCDATA: ThtmlParserStringEvent read FOnCDATA write FOnCDATA;
    property  OnProcessingInstr: ThtmlParserProcessingInstrEvent read FOnProcessingInstr write FOnProcessingInstr;

    property  OnStartTag: ThtmlParserStartTagEvent read FOnStartTag write FOnStartTag;
    property  OnStartTagAttributes: ThtmlParserStartTagAttributesEvent read FOnStartTagAttributes write FOnStartTagAttributes;
    property  OnEndTag: ThtmlParserEndTagEvent read FOnEndTag write FOnEndTag;

    property  OnElementOpen: ThtmlParserElementEvent read FOnElementOpen write FOnElementOpen;
    property  OnElementClose: ThtmlParserElementEvent read FOnElementClose write FOnElementClose;

    property  OnDocumentObject: ThtmlParserDocumentObjectEvent read FOnDocumentObject write FOnDocumentObject;

    function  ParseDocument: ThtmlDocument;
    property  State: ThtmlParserState read FState;

    property  Document: ThtmlDocument read GetDocument;
  end;
  EhtmlParser = class(Exception);



{                                                                              }
{ TfclHTMLParser                                                               }
{                                                                              }
type
  TfclHTMLParser = class (ThtmlParser)
  published
    property Options;
    property RawText;
    property FileName;
    property Encoding;

    property OnMessage;
    property OnToken;
    property OnTokenStr;
    property OnResolveEntityRef;
    property OnText;
    property OnUnresolvedText;
    property OnLineBreak;
    property OnComment;
    property OnDeclaration;
    property OnMarkedSection;
    property OnCDATA;
    property OnProcessingInstr;
    property OnStartTag;
    property OnStartTagAttributes;
    property OnEndTag;
    property OnElementOpen;
    property OnElementClose;
    property OnDocumentObject;
  end;



{                                                                              }
{ Test cases                                                                   }
{                                                                              }
{$IFDEF HTML_TEST}
procedure Test;
{$ENDIF}



implementation

uses
  { Fundamentals }
  flcUtils,
  flcDynArrays,
  flcStrings,
  flcUnicodeReader,

  { HTML }
  flcHTMLCharEntity,
  flcHTMLDocElements,
  flcHTMLReader;



{                                                                              }
{ ThtmlParserState                                                             }
{                                                                              }
constructor ThtmlParserState.Create(const Parser: ThtmlParser);
begin
  inherited Create;
  Assert(Assigned(Parser));
  FParser := Parser;
  FElementList := TDoublyLinkedList.Create;
  FFreeList := TDoublyLinkedList.Create;
  FReopenList := TDoublyLinkedList.Create;
end;

destructor ThtmlParserState.Destroy;
begin
  FreeAndNil(FDocument);
  FreeAndNil(FReopenList);
  FreeAndNil(FFreeList);
  FreeAndNil(FElementList);
  inherited Destroy;
end;

procedure ThtmlParserState.Reset;
begin
  FreeAndNil(FDocument);
  FReopenList.DeleteList;
  FElementList.DeleteList;
  if not (poDontProduceDocument in FParser.FOptions) then
    FDocument := ThtmlDocument.Create;
  OpenElement(HTML_TAG_Document, '<!DOC!>', nil);
end;

function ThtmlParserState.GetOpenCount: Integer;
begin
  Result := FElementList.Count;
end;

function ThtmlParserState.GetInnerItem: ThtmlOpenElementInfo;
begin
  Result := ThtmlOpenElementInfo(FElementList.Last);
end;

function ThtmlParserState.GetInnerTagID: ThtmlTagID;
begin
  if not Assigned(FElementList.Last) then
    Result := HTML_TAG_None
  else
    Result := ThtmlOpenElementInfo(FElementList.Last).TagID;
end;

function ThtmlParserState.GetInnerContainer: AhtmlContainerObject;
var I: ThtmlOpenElementInfo;
begin
  I := GetInnerItem;
  if Assigned(I) then
    if I.TagID = HTML_TAG_Document then
      Result := FDocument else
      Result := I.Element
  else
    Result := FDocument;
end;

function ThtmlParserState.GetOuterItem: ThtmlOpenElementInfo;
begin
  Result := ThtmlOpenElementInfo(FElementList.First);
end;

function ThtmlParserState.AddToContainer(const Container: AhtmlContainerObject;
    const Obj: AhtmlObject): Boolean;
begin
  Result := False;
  if not Assigned(Obj) then
    exit;
  if Assigned(Container) then
    begin
      Container.AddItem(Obj);
      Result := True;
    end;
  FParser.TriggerDocumentObject(Obj);
  if not Assigned(Container) then
    Obj.Free;
end;

function ThtmlParserState.AddToInnerContainer(const Obj: AhtmlObject): Boolean;
begin
  Result := AddToContainer(GetInnerContainer, Obj);
end;

function ThtmlParserState.LocateTagID(const TagID: ThtmlTagID): ThtmlOpenElementInfo;
begin
  Result := ThtmlOpenElementInfo(FElementList.Last);
  while Assigned(Result) do
    if TagID = Result.TagID then
      exit
    else
      Result := ThtmlOpenElementInfo(Result.Prev);
end;

function ThtmlParserState.LocateName(const Name: String): ThtmlOpenElementInfo;
begin
  Result := ThtmlOpenElementInfo(FElementList.Last);
  while Assigned(Result) do
    if StrEqualNoAsciiCaseU(Name, Result.Name) then
      exit
    else
      Result := ThtmlOpenElementInfo(Result.Prev);
end;

function ThtmlParserState.IsTagIDOpen(const TagID: ThtmlTagID): Boolean;
begin
  Result := Assigned(LocateTagID(TagID));;
end;

function ThtmlParserState.IsNameOpen(const Name: String): Boolean;
begin
  Result := Assigned(LocateName(Name));
end;

function ThtmlParserState.IsBodyOpen: Boolean;
var I: ThtmlOpenElementInfo;

begin
  I := ThtmlOpenElementInfo(FElementList.First);
  Result := False;
  while Assigned(I) do
    if I.TagID = HTML_TAG_HTML then
      begin
        Result := True;
        break;
      end else
      I := ThtmlOpenElementInfo(I.Next);
  if not Result then
    exit;
  Result := False;
  I := ThtmlOpenElementInfo(I.Next);
  while Assigned(I) do
    if I.TagID = HTML_TAG_BODY then
      begin
        Result := True;
        break;
      end
    else
      I := ThtmlOpenElementInfo(I.Next);
end;

procedure ThtmlParserState.OpenElement(const Item: ThtmlOpenElementInfo);
begin
  Assert(Assigned(Item), 'Assigned(Item)');
  FElementList.Append(Item);
  FParser.TriggerElementOpen(Item);
end;

procedure ThtmlParserState.OpenElement(const TagID: ThtmlTagID;
    const Name: String; const Element: AhtmlElement);
var Item: ThtmlOpenElementInfo;
begin
  Item := ThtmlOpenElementInfo(FFreeList.RemoveFirst);
  if not Assigned(Item) then
    Item := ThtmlOpenElementInfo.Create;
  Item.TagID := TagID;
  Item.Name := Name;
  Item.Element := Element;
  OpenElement(Item);
end;

procedure ThtmlParserState.OpenAutoStartElements(const TagID: ThtmlTagID);
var Item        : ThtmlOpenElementInfo;
    NewTag, Tag : ThtmlTagID;
    Element     : AhtmlElement;
    Container   : AhtmlContainerObject;
begin
  Item := InnerItem;
  while Assigned(Item) do
    begin
      if TagID = Item.TagID then
        break;
      NewTag := htmlAutoOpenTag(TagID, Item.TagID);
      if NewTag <> HTML_TAG_None then
        begin
          if NewTag = Item.TagID then
            break; // no auto-start element
          if Item.TagID = HTML_TAG_Document then
            Container := FDocument else
            Container := Item.Element;
          repeat
            // create auto-start element
            Element := FParser.CreateElement(NewTag, '');
            if Assigned(Element) then
              if not AddToContainer(Container, Element) then
                Element := nil;
            OpenElement(NewTag, '', Element);
            // next
            Tag := htmlAutoOpenTag(TagID, NewTag);
            if (Tag = HTML_TAG_None) or (Tag = NewTag) then
              break;
            Container := Element;
            NewTag := Tag;
          until False;
          break;
        end
      else
        Item := ThtmlOpenElementInfo(Item.Prev);
    end;
end;

function ThtmlParserState.ReleaseElement: ThtmlOpenElementInfo;
begin
  Result := ThtmlOpenElementInfo(FElementList.Last);
  if not Assigned(Result) then
    exit;
  FParser.TriggerElementClose(Result);
  FElementList.RemoveLast;
end;

procedure ThtmlParserState.CloseElementToFree;
var Item: ThtmlOpenElementInfo;
begin
  Item := ReleaseElement;
  if not Assigned(Item) then
    exit;
  FFreeList.Append(Item);
end;

procedure ThtmlParserState.CloseElementToReOpen;
var Item: ThtmlOpenElementInfo;
begin
  Item := ReleaseElement;
  if not Assigned(Item) then
    exit;
  FReopenList.Append(Item);
end;

procedure ThtmlParserState.ReOpenElements;
var Container : AhtmlContainerObject;
    Item      : ThtmlOpenElementInfo;
begin
  if FReopenList.IsEmpty then
    exit;
  Container := InnerContainer;
  repeat
    Item := ThtmlOpenElementInfo(FReopenList.RemoveLast);
    if not Assigned(Item) then
      break;
    if Assigned(Item.Element) then
      begin
        Item.Element := Item.Element.DuplicateElement;
        if not AddToContainer(Container, Item.Element) then
          Item.Element := nil;
        Container := Item.Element;
      end
    else
      Container := nil;
    OpenElement(Item);
  until False;
end;

procedure ThtmlParserState.CloseOverlapping
    (const OpenElement: ThtmlOpenElementInfo);
var Item: ThtmlOpenElementInfo;
begin
  // Close open overlapping tags up to, but excluding, OpenElement
  if poDontAllowOverlappedTags in FParser.FOptions then
    // Strict: Close all open overlapped elements
    repeat
      Item := InnerItem;
      if OpenElement = Item then
        break;
      CloseElementToFree;
    until False
  else
    // Overlaps allowed: Close certian overlapping elements
    repeat
      Item := InnerItem;
      if OpenElement = Item then
        break;
      if not htmlDoesCloseTagCloseOpenTag(OpenElement.TagID, Item.TagID) then
        CloseElementToReopen
      else
        CloseElementToFree;
    until False;
end;

procedure ThtmlParserState.CloseElement(const OpenElement: ThtmlOpenElementInfo);
begin
  CloseOverlapping(OpenElement);
  CloseElementToFree;
  ReOpenElements;
end;

function ThtmlParserState.GetOpenElementForEndTag(const TagID: ThtmlTagID;
    const Name: String): ThtmlOpenElementInfo;
begin
  Result := InnerItem;
  while Assigned(Result) do
    begin
      if htmlIsSameTag(TagID, Name, Result.TagID, Result.Name) then
        // found matching open tag
        exit else
      if poDontAllowOverlappedTags in FParser.FOptions then
        begin
          if not htmlIsElementEndTagOptional(Result.TagID) then
            begin
              Result := nil; // strict: overlapping closes only allowed
              exit;          // through elements with optional end tags
            end;
        end else
      if not htmlDoesCloseTagCloseOutside(TagID, Result.TagID) then
        begin
          Result := nil; // tag does not close through this level
          exit;
        end;
      Result := ThtmlOpenElementInfo(Result.Prev);
    end;
  Result := nil; // no matching open tag found
end;

function ThtmlParserState.GetAutoCloseElementForStartTag(
    const TagID: ThtmlTagID): ThtmlOpenElementInfo;
begin
  Result := InnerItem;
  while Assigned(Result) do
    begin
      // 1.  check if auto-close element
      if TagID = Result.TagID then
        begin
          if htmlIsElementEndTagOptional(TagID) then
            // close matching open tag with optional end tag
            exit;
          // no further searching
          Result := nil;
          exit;
        end else
      if htmlDoesOpenTagAutoCloseOpenTag(TagID, Result.TagID) then
        // found
        exit else
      // 2.  check if iteration should continue outside
      if poDontAllowOverlappedTags in FParser.FOptions then
        begin
          if not htmlIsElementEndTagOptional(Result.TagID) then
            begin
              Result := nil; // strict: overlapping closes only allowed
              exit;          // through elements with optional end tags
            end;
        end else
      if not htmlDoesOpenTagAutoCloseOutside(TagID, Result.TagID) then
        begin
          Result := nil; // tag does not close through this level
          exit;
        end;
      // 3.  next open level
      Result := ThtmlOpenElementInfo(Result.Prev);
    end;
  Result := nil; // no matching open tag found
end;



{                                                                              }
{ ThtmlParser                                                                  }
{                                                                              }
constructor ThtmlParser.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOptions := [];
  FState := ThtmlParserState.Create(self);
end;

destructor ThtmlParser.Destroy;
begin
  FreeAndNil(FLexer);
  FreeAndNil(FState);
  inherited Destroy;
end;

{ Messages                                                                     }
procedure ThtmlParser.LogMessage(const MsgType: ThtmlParserMessageType;
    const Msg: String);
begin
  if not (poDisableNotifications in FOptions) and Assigned(FOnMessage) then
    FOnMessage(self, MsgType, Msg);
  if (MsgType = hmError) and (poStopOnError in FOptions) then
    raise EhtmlParser.Create('Parse error: ' + Msg);
end;

procedure ThtmlParser.LogUnexpectedToken;
begin
  LogMessage(hmError, 'Unexpected token (' + FLexer.TokenTypeDescription + ')');
end;

procedure ThtmlParser.LogUnmatchedEndTag(const TagID: ThtmlTagID; const Name: String);
var S: String;
begin
  S := Name;
  if (S = '') and (TagID <> HTML_TAG_None) then
    S := htmlGetTagName(TagID);
  LogMessage(hmError, 'Unmatched end tag (' + ToStringU(S) + ')');
end;

{ Triggers                                                                     }
procedure ThtmlParser.TriggerElementOpen(const Item: ThtmlOpenElementInfo);
begin
  if not (poDisableNotifications in FOptions) and
     Assigned(FOnElementOpen) then
    with Item do
      FOnElementOpen(self, TagID, Name, Element);
end;

procedure ThtmlParser.TriggerElementClose(const Item: ThtmlOpenElementInfo);
begin
  if not (poDisableNotifications in FOptions) and
     Assigned(FOnElementClose) then
    with Item do
      FOnElementClose(self, TagID, Name, Element);
end;

procedure ThtmlParser.TriggerDocumentObject(const Obj: AhtmlObject);
begin
  if not (poDisableNotifications in FOptions) then
    begin
      if Assigned(FOnDocumentObject) then
        FOnDocumentObject(self, Obj);
    end;
end;

{ Create                                                                       }
function ThtmlParser.CreateText(const Context: ThtmlParserTextContext;
    var Text: String): AhtmlObject;
begin
  if not (poDisableNotifications in FOptions) and
      Assigned(FOnText) then
    FOnText(self, Context, Text);
  if poDontProduceDocument in FOptions then
    Result := nil
  else
    Result := ThtmlText.Create(Text);
end;

function ThtmlParser.CreateCharRef(const CharVal: LongWord; const HasTrailer: Boolean): AhtmlObject;
begin
  if poDontProduceDocument in FOptions then
    Result := nil
  else
    Result := ThtmlCharRef.Create(CharVal, HasTrailer);
end;

function ThtmlParser.CreateEntityRef(const EntityName: String; const HasTrailer: Boolean): AhtmlObject;
begin
  if poDontProduceDocument in FOptions then
    Result := nil
  else
    Result := ThtmlEntityRef.Create(EntityName, HasTrailer);
end;

function ThtmlParser.CreateLineBreak: AhtmlObject;
begin
  if not (poDisableNotifications in FOptions) and
      Assigned(FOnLineBreak) then
    FOnLineBreak(self);
  if poDontProduceDocument in FOptions then
    Result := nil
  else
    Result := ThtmlLineBreak.Create;
end;

function ThtmlParser.CreateEmptyComment: AhtmlObject;
begin
  if poDontProduceDocument in FOptions then
    Result := nil
  else
    Result := ThtmlEmptyComment.Create;
end;

function ThtmlParser.CreateComment(const Comments: StringArray): AhtmlObject;
begin
  if not (poDisableNotifications in FOptions) and
     Assigned(FOnComment) then
    FOnComment(self, Comments);
  if poDontProduceDocument in FOptions then
    Result := nil
  else
    Result := ThtmlComment.Create(Comments);
end;

function ThtmlParser.CreateDeclaration(const Name, Text: String): AhtmlObject;
begin
  if not (poDisableNotifications in FOptions) and
     Assigned(FOnDeclaration) then
    FOnDeclaration(self, Name, Text);
  if poDontProduceDocument in FOptions then
    Result := nil
  else
    Result := ThtmlRawTag.Create('!' + Name + iifU(Text <> '', ' ', '') + Text);
end;

function ThtmlParser.CreateMarkedSection(const Text: String): AhtmlObject;
begin
  if not (poDisableNotifications in FOptions) and
     Assigned(FOnMarkedSection) then
    FOnMarkedSection(self, Text);
  if poDontProduceDocument in FOptions then
    Result := nil
  else
    Result := ThtmlRawTag.Create('![' + Text + ']');
end;

function ThtmlParser.CreateCDATA(const CDATA: String): AhtmlObject;
begin
  if not (poDisableNotifications in FOptions) and
     Assigned(FOnCDATA) then
    FOnCDATA(self, CDATA);
  if poDontProduceDocument in FOptions then
    Result := nil
  else
    Result := ThtmlCDATA.Create(CDATA);
end;

function ThtmlParser.CreatePI(const Target, PI: String): AhtmlObject;
begin
  if not (poDisableNotifications in FOptions) and
     Assigned(FOnProcessingInstr) then
    FOnProcessingInstr(self, Target, PI);
  if poDontProduceDocument in FOptions then
    Result := nil
  else
    Result := ThtmlPI.Create(Target, PI);
end;

function ThtmlParser.CreateElement(const TagID: ThtmlTagID;
    const Name: String): AhtmlElement;
var C: AhtmlContainerObject;
begin
  if poDontProduceDocument in FOptions then
    Result := nil
  else
    begin
      C := FState.GetInnerContainer;
      if Assigned(C) then
        Result := C.CreateItem(Ord(TagID), Name) as AhtmlElement
      else
        Result := htmlCreateElement(TagID, Name);
    end;
end;

{ Token                                                                        }
function ThtmlParser.GetTokenStr: String;
begin
  Result := FLexer.TokenStr;
end;

function ThtmlParser.GetNextToken: ThtmlTokenType;
begin
  if not (poDisableNotifications in FOptions) and (FTokenType <> htNone) then
    begin
      // note: notify token After it has been examined
      if Assigned(FOnToken) then
        FOnToken(self, FTokenType);
      if Assigned(FOnTokenStr) then
        FOnTokenStr(self, FTokenType, FLexer.TokenStr);
    end;
  // get next token from lexer
  FTokenType := FLexer.GetNextToken;
  // return token type
  Result := FTokenType;
end;

function ThtmlParser.ResolveRef(var Text: String): Boolean;
begin
  Assert(FTokenType in htmlTokensRef, 'FTokenType in htmlTokensRef');
  Text := '';
  if poDontResolveReferences in FOptions then
    Result := False
  else
    case FTokenType of
      htEntityRef:
        begin
          Result := FLexer.ResolveEntityRef(Text);
          if not (poDisableNotifications in FOptions) and
              Assigned(FOnResolveEntityRef) then
            FOnResolveEntityRef(self, GetTokenStr, Text, Result);
        end;
      htCharRef,
      htCharRefHex:
        Result := FLexer.ResolveCharRef(Text);
    else
      Result := False;
    end;
end;

{ Parse                                                                        }
function ThtmlParser.ParsePlainText: String;
var T: String;
begin
  Assert(FTokenType in htmlTokensText, 'FTokenType in htmlTokensText');
  Result := '';
  repeat
    case FTokenType of
      htText:
        Result := Result + GetTokenStr;
      htCharRef,
      htCharRefHex,
      htEntityRef:
        if ResolveRef(T) then
          Result := Result + T
        else
          break;
    else
      break;
    end;
    GetNextToken;
  until False;
end;

function ThtmlParser.ParseText(const TextContext: ThtmlParserTextContext;
    var Text: String): AhtmlObject;
var CharVal : LongWord;
    S       : String;
    R       : Boolean;
begin
  // extract resolved text
  Assert(FTokenType in htmlTokensText, 'FTokenType in htmlTokensText');
  Text := ParsePlainText;
  if Text <> '' then
    Result := CreateText(TextContext, Text)
  else
    // extract unresolved entity reference
    case FTokenType of
      htCharRefHex,
      htCharRef:
        begin
          CharVal := FLexer.CharRefValue;
          Text := htmlCharRef(CharVal, FTokenType = htCharRefHex);
          GetNextToken;
          R := (FTokenType = htRefTrailer);
          if R then
            GetNextToken;
          Result := CreateCharRef(CharVal, R);
          if not (poDisableNotifications in FOptions) and
              Assigned(FOnUnresolvedText) then
            FOnUnresolvedText(self, TextContext, Text);
        end;
      htEntityRef:
        begin
          S := GetTokenStr;
          Text := '&' + S + ';';
          GetNextToken;
          R := (FTokenType = htRefTrailer);
          if R then
            GetNextToken;
          Result := CreateEntityRef(S, R);
          if not (poDisableNotifications in FOptions) and
              Assigned(FOnUnresolvedText) then
            FOnUnresolvedText(self, TextContext, Text);
        end;
    else
      Result := nil;
    end;
end;

function ThtmlParser.ParseContentText: AhtmlObject;
var T: String;
begin
  Result := ParseText(tcContent, T);
end;

function ThtmlParser.ParseLineBreak: AhtmlObject;
begin
  Assert(FTokenType = htLineBreak, 'FTokenType = htLineBreak');
  Result := CreateLineBreak;
  GetNextToken;
end;

function ThtmlParser.ParseComment: AhtmlObject;
var S: StringArray;
begin
  Assert(FTokenType in htmlTokensComment, 'FTokenType in htmlTokensComment');
  case FTokenType of
    htEmptyComment:
      begin
        GetNextToken;
        Result := CreateEmptyComment;
      end;
    htComment:
      begin
        S := nil;
        repeat
          DynArrayAppend(S, GetTokenStr);
        until GetNextToken <> htComment;
        if FTokenType = htCommentEnd then
          GetNextToken
        else
          LogUnexpectedToken;
        Result := CreateComment(S);
      end;
  else
    Result := nil;
  end;
end;

function ThtmlParser.ParseDeclaration: AhtmlObject;
var Name, Text: String;
begin
  Assert(FTokenType = htDeclaration, 'FTokenType = htDeclaration');
  Name := GetTokenStr;
  if GetNextToken = htDeclarationText then
    begin
      Text := GetTokenStr;
      GetNextToken;
    end else
    Text := '';
  Result := CreateDeclaration(Name, Text);
end;

function ThtmlParser.ParseMarkedSection: AhtmlObject;
var S: String;
begin
  Assert(FTokenType = htMarkedSection, 'FTokenType = htMarkedSection');
  S := GetTokenStr;
  GetNextToken;
  Result := CreateMarkedSection(S);
end;

function ThtmlParser.ParseCDATA: AhtmlObject;
var S: String;
begin
  Assert(FTokenType = htCDATA, 'FTokenType = htCDATA');
  S := GetTokenStr;
  GetNextToken;
  Result := CreateCDATA(S);
end;

function ThtmlParser.ParsePI: AhtmlObject;
var Target, PI: String;
begin
  Assert(FTokenType = htPITarget, 'FTokenType = htPITarget');
  Target := GetTokenStr;
  if GetNextToken = htPI then
    begin
      PI := GetTokenStr;
      GetNextToken;
      Result := CreatePI(Target, PI);
    end else
    Result := CreatePI(Target, '');
end;

function ThtmlParser.ParseTagAttributeValue(
    const Attribute: ThtmlElementAttribute): String;
var Obj  : AhtmlObject;
    Text : String;
begin
  Assert(FTokenType = htTagAttrValueStart, 'FTokenType = htTagAttrValueStart');
  GetNextToken;
  Result := '';
  while FTokenType in htmlTokensTextInclLineBreak do
    begin
      Obj := ParseText(tcAttributeValue, Text);
      Result := Result + Text;
      if Assigned(Attribute) and Assigned(Obj) then
        Attribute.AddItem(Obj);
    end;
  if FTokenType = htTagAttrValueEnd then
    GetNextToken
  else
    LogUnexpectedToken;
end;

function ThtmlParser.ParseTagAttribute(const TagID: ThtmlTagID;
    const TagName: String; const Element: AhtmlElement;
    var AttrID: ThtmlAttrID; var Name, Value: String): ThtmlElementAttribute;
begin
  Assert(FTokenType = htTagAttrName, 'FTokenType = htTagAttrName');
  AttrID := FLexer.AttrID;
  if AttrID = HTML_ATTR_None then
    Name := GetTokenStr
  else
    Name := '';
  GetNextToken;
  if poDontProduceDocument in FOptions then
    Result := nil
  else
    Result := ThtmlElementAttribute.Create(AttrID, Name);
  // Parse value
  if FTokenType = htTagAttrValueStart then
    Value := ParseTagAttributeValue(Result)
  else
    Value := '';
end;

procedure ThtmlParser.ParseTagAttributes(const TagID: ThtmlTagID;
    const Name: String; const Element: AhtmlElement);
var Attr     : ThtmlElementAttribute;
    AttrID   : ThtmlAttrID;
    AttrName : String;
    Value    : String;
    Names    : StringArray;
    Values   : StringArray;
    Notify   : Boolean;
begin
  Notify := not (poDisableNotifications in FOptions) and
            Assigned(FOnStartTagAttributes);
  repeat
    case FTokenType of
      htTagAttrName:
        begin
          Attr := ParseTagAttribute(TagID, Name, Element,
              AttrID, AttrName, Value);
          if Assigned(Element) and Assigned(Attr) then
            Element.Attributes.AddItem(Attr);
          if Notify then
            begin
              if (AttrName = '') and (AttrID <> HTML_ATTR_None) then
                AttrName := htmlGetAttrName(AttrID);
              DynArrayAppend(Names, AttrName);
              DynArrayAppend(Values, Value);
            end;
        end;
      htTagAttrValueStart:
        repeat
          LogUnexpectedToken;
          GetNextToken;
        until not (FTokenType in htmlTokensText);
      htTagAttrValueEnd:
        begin
          LogUnexpectedToken;
          GetNextToken;
        end;
    else
      break;
    end;
  until False;
  if Notify then
    FOnStartTagAttributes(self, TagID, Name, Names, Values, Element);
end;

function ThtmlParser.ParseStartTag: AhtmlObject;
var TagID      : ThtmlTagID;
    Name       : String;
    Element    : AhtmlElement;
    Item       : ThtmlOpenElementInfo;
    ContentTag : Boolean;
begin
  Assert(FTokenType = htStartTag, 'FTokenType = htStartTag');
  // get tag name
  TagID := FLexer.TagID;
  Name := GetTokenStr;
  GetNextToken;
  if not (poDisableNotifications in FOptions) and Assigned(FOnStartTag) then
    FOnStartTag(self, TagID, Name);
  // auto-close nesting levels
  Item := FState.GetAutoCloseElementForStartTag(TagID);
  if Assigned(Item) then
    FState.CloseElement(Item); // auto-close
  // auto-start nesting levels
  FState.OpenAutoStartElements(TagID);
  // create element
  Element := CreateElement(TagID, Name);
  // parse rest of tag
  ParseTagAttributes(TagID, Name, Element);
  // set tag type
  if FTokenType = htEmptyTag then // empty tag
    begin
      if Assigned(Element) then
        Element.TagType := ttEmptyTag;
      ContentTag := False;
      GetNextToken;
    end else
  if htmlIsEmptyElement(TagID) then // required to be empty
    begin
      if Assigned(Element) then
        Element.TagType := ttStartTag;
      ContentTag := False;
    end else
    begin // content tag
      if Assigned(Element) then
        Element.TagType := ttContentTags;
      ContentTag := True;
    end;
  // update nesting levels
  if not FState.AddToInnerContainer(Element) then
    Element := nil;
  if ContentTag then
    // new nest level
    FState.OpenElement(TagID, Name, Element);
  // object added to container
  Result := nil;
end;

function ThtmlParser.ParseEndTag: AhtmlObject;
var TagID : ThtmlTagID;
    Item  : ThtmlOpenElementInfo;
    Name  : String;
begin
  // parse tag
  TagID := FLexer.TagID;
  if TagID = HTML_TAG_None then
    Name := GetTokenStr
  else
    Name := '';
  GetNextToken;
  // notify
  if not (poDisableNotifications in FOptions) and Assigned(FOnEndTag) then
    FOnEndTag(self, TagID, Name);
  // close element
  Item := FState.GetOpenElementForEndTag(TagID, Name);
  if Assigned(Item) then
    FState.CloseElement(Item)
  else
    LogUnmatchedEndTag(TagID, Name);
  // object in container
  Result := nil;
end;

function ThtmlParser.ParseToken: AhtmlObject;
begin
  case FTokenType of
    htText,
    htEntityRef,
    htCharRef,
    htCharRefHex      : Result := ParseContentText;
    htStartTag        : Result := ParseStartTag;
    htEndTag          : Result := ParseEndTag;
    htLineBreak       : Result := ParseLineBreak;
    htEmptyComment,
    htComment         : Result := ParseComment;
    htMarkedSection   : Result := ParseMarkedSection;
    htCDATA           : Result := ParseCDATA;
    htPITarget        : Result := ParsePI;
    htDeclaration     : Result := ParseDeclaration;
    htEOF             : Result := nil;
  else
    begin
      LogUnexpectedToken;
      GetNextToken;
      Result := nil;
    end;
  end;
end;

function ThtmlParser.ParseDocument: ThtmlDocument;
var Reader: TUnicodeReader;
begin
  // Initialize
  FreeAndNil(FLexer);
  if FFileName <> '' then
    Reader := htmlGetDocumentReaderForFile(FFileName, FEncoding)
  else
    Reader := htmlGetDocumentReaderForRawString(FRawText, FEncoding);
  FLexer := ThtmlLexer.Create(Reader, True);
  FLexer.NoLineBreakToken := False;
  FState.Reset;
  // Parse
  FTokenType := htNone;
  GetNextToken;
  while FTokenType <> htEOF do
    FState.AddToInnerContainer(ParseToken);
  // Success
  FreeAndNil(FLexer);
  Result := FState.Document;
  if Assigned(Result) then
    Result.PrepareStructure;
end;

function ThtmlParser.GetDocument: ThtmlDocument;
begin
  Assert(Assigned(FState));
  Result := FState.Document;
end;




{                                                                              }
{ Test cases                                                                   }
{                                                                              }
{$IFDEF HTML_TEST}
{$ASSERTIONS ON}
procedure Test;
var P : ThtmlParser;

  procedure TestHTML(const RawHTML: RawByteString);
  var D : ThtmlDocument;
  begin
    P.RawText := RawHTML;
    D := P.ParseDocument;
    Assert(D.HTMLText = UTF8ToString(RawHTML));
  end;

begin
  P := ThtmlParser.Create(nil);
  try
    TestHTML('<!DOCTYPE HTML><HTML><HEAD></HEAD><BODY>Test</BODY></HTML>');
    TestHTML('<!DOCTYPE HTML><HTML><HEAD><_Test1></_Test1></HEAD><BODY>Test<_Test2></_Test2></BODY></HTML>');
    TestHTML('<!DOCTYPE HTML><HTML><HEAD><_Test1/></HEAD><BODY>Test<_Test2/></BODY></HTML>');
  finally
    P.Free;
  end;
end;
{$ENDIF}



end.

