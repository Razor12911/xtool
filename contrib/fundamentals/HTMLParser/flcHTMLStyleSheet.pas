{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals 5.00 - HTML Parser                          }
{   File name:        flcHTMLStyleSheet.pas                                    }
{   File version:     5.05                                                     }
{   Description:      HTML Style Sheet                                         }
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
{   2002/11/04  1.00  Initial version.                                         }
{   2002/11/22  1.01  Refactored.                                              }
{   2002/12/03  1.02  Redesigned style resolution.                             }
{   2002/12/08  1.03  Created seperate unit for ThtmlcssProperty classes.      }
{   2015/04/11  1.04  UnicodeString changes.                                   }
{   2019/02/22  5.05  Revise for Fundamentals 5.                               }
{                                                                              }
{******************************************************************************}

{$INCLUDE flcHTML.inc}

unit flcHTMLStyleSheet;

interface

uses
  { System }
  {$IFDEF DELPHI6_UP}
  Types,
  {$ELSE}
  Windows,
  {$ENDIF}
  SysUtils,
  System.UITypes,

  { Fundamentals }
  flcStdTypes,
  flcDataStructs,

  { HTML }
  flcHTMLElements,
  flcHTMLProperties,
  flcHTMLStyleTypes,
  flcHTMLStyleProperties;



{                                                                              }
{ EhtmlCSS                                                                     }
{                                                                              }
type
  EhtmlCSS = class(Exception);



{                                                                              }
{ ThtmlcssElementInfo                                                          }
{   HTML element information required by ThtmlcssSelector.                     }
{                                                                              }
type
  PhtmlcssElementInfo = ^ThtmlcssElementInfo;
  ThtmlcssElementInfo = record
    ElementID  : Integer;
    ClassIDs   : IntegerArray;
    TagID      : ThtmlTagID;
    PseudoIDs  : ThtmlcssPseudoPropertyIDSet;
    ParentInfo : PhtmlcssElementInfo;
  end;



{                                                                              }
{ ThtmlcssStyleProperties                                                      }
{                                                                              }
procedure InitDefaultStyleProperties(var StyleInfo: ThtmlcssStyleProperties);
procedure InitElementStyleProperties(var StyleInfo: ThtmlcssStyleProperties;
          const ParentStyle: ThtmlcssStyleProperties;
          const ElementInfo: ThtmlcssElementInfo);
function  StyleResolveSystemFontName(const StyleInfo: ThtmlcssStyleProperties): String;
function  StyleResolveSystemFontSize(const StyleInfo: ThtmlcssStyleProperties): Integer;
procedure StyleResolveSystemFont(const StyleInfo: ThtmlcssStyleProperties;
          var Name: String; var Styles: TFontStyles; var Size: Integer);
function  StyleResolveSystemColor(const Color: ThtmlColor): TColor;



{                                                                              }
{ ThtmlcssSelector                                                             }
{                                                                              }
type
  ThtmlcssSelector = class
  protected
    FNext             : ThtmlcssSelector;
    FTagID            : ThtmlTagID;
    FClassID          : Integer;
    FElementID        : Integer;
    FPseudoPropIDs    : ThtmlcssPseudoPropertyIDSet;
    FAncestorSelector : ThtmlcssSelector;
    FSpecificity      : Integer;

  public
    constructor Create(const TagID: ThtmlTagID; const ClassID: Integer;
        const ElementID: Integer; const PseudoPropIDs: ThtmlcssPseudoPropertyIDSet;
        const AncestorSelector: ThtmlcssSelector);
    destructor Destroy; override;

    property  Specificity: Integer read FSpecificity;
    function  Match(const ElementInfo: ThtmlcssElementInfo): Boolean;
  end;



{                                                                              }
{ ThtmlcssSelectors                                                            }
{                                                                              }
type
  ThtmlcssSelectors = class
  protected
    FFirst : ThtmlcssSelector;
    FLast  : ThtmlcssSelector;

    procedure AddSelector(const Selector: ThtmlcssSelector);

  public
    destructor Destroy; override;

    function  MatchingSelector(const ElementInfo: ThtmlcssElementInfo): ThtmlcssSelector;
  end;



{                                                                              }
{ ThtmlcssDeclarations                                                         }
{                                                                              }
type
  ThtmlcssDeclarations = class
  protected
    FFirst : ThtmlcssProperty;
    FLast  : ThtmlcssProperty;

    procedure AddDeclaration(const Declaration: ThtmlcssProperty);

  public
    destructor Destroy; override;

    procedure ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
              const ParentStyle: ThtmlcssStyleProperties);
  end;



{                                                                              }
{ ThtmlcssMatchingDeclarations                                                 }
{                                                                              }
type
  ThtmlcssMatchingDeclarationsItem = record
    Specificity  : Integer;
    Order        : Integer;
    Declarations : ThtmlcssDeclarations;
  end;
  PhtmlcssMatchingDeclarationsItem = ^ThtmlcssMatchingDeclarationsItem;
  ThtmlcssMatchingDeclarations = class
  protected
    FMatches : Array of ThtmlcssMatchingDeclarationsItem;
    FCount   : Integer;

  public
    property  Count: Integer read FCount;
    procedure Clear;
    procedure AddMatch(const Specificity: Integer;
              const Declarations: ThtmlcssDeclarations);
    procedure Sort;
    procedure ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
              const ParentStyle: ThtmlcssStyleProperties);
  end;



{                                                                              }
{ ThtmlcssRule                                                                 }
{                                                                              }
type
  ThtmlcssRule = class
  protected
    FNext         : ThtmlcssRule;
    FSelectors    : ThtmlcssSelectors;
    FDeclarations : ThtmlcssDeclarations;

  public
    constructor Create(const Selectors: ThtmlcssSelectors;
        const Declarations: ThtmlcssDeclarations);
    destructor Destroy; override;

    procedure AddMatch(const ElementInfo: ThtmlcssElementInfo;
              const Matches: ThtmlcssMatchingDeclarations);
  end;



{                                                                              }
{ ThtmlcssRuleSet                                                              }
{                                                                              }
type
  ThtmlCSS = class;
  ThtmlcssStyleSheetState = (ssNotImported, ssReady, ssBusyImporting,
      ssImportFailed);
  ThtmlcssRuleSetState = (rsLocal, rsNotImported, rsBusyImporting, rsImported,
      rsImportFailed);
  ThtmlcssRuleSet = class
  protected
    FCSS     : ThtmlCSS;
    FSource  : String;
    FState   : ThtmlcssRuleSetState;
    FFirst   : ThtmlcssRule;
    FLast    : ThtmlcssRule;
    FImports : array of ThtmlcssRuleSet;

  public
    constructor Create(const CSS: ThtmlCSS; const State: ThtmlcssRuleSetState;
                const Source: String);
    destructor Destroy; override;

    property  CSS: ThtmlCSS read FCSS;
    property  State: ThtmlcssRuleSetState read FState write FState;

    procedure AddRule(const Rule: ThtmlcssRule);
    procedure AddImport(const Source: String);

    procedure GetRequiredImports(var Sources: StringArray);
    procedure SetImportedStyleState(const Source: String;
              const State: ThtmlcssRuleSetState;
              const StyleText: String);
    function  GetStyleSheetState: ThtmlcssStyleSheetState;
    procedure GetImportProgress(var StateVal, TotVal: Integer);

    procedure GetMatches(const ElementInfo: ThtmlcssElementInfo;
              const Matches: ThtmlcssMatchingDeclarations);
  end;



{                                                                              }
{ ThtmlCSS                                                                     }
{                                                                              }
  ThtmlcssParser = class;
  ThtmlCSS = class
  protected
    FParser     : ThtmlcssParser;
    FRuleSet    : ThtmlcssRuleSet;
    FClassIDs   : THashedUnicodeStringArray;
    FElementIDs : THashedUnicodeStringArray;
    FMatches    : ThtmlcssMatchingDeclarations;

    function  AddClassID(const ClassID: String): Integer;
    function  AddElementID(const ElementID: String): Integer;

  public
    constructor Create;
    destructor Destroy; override;

    property  Parser: ThtmlcssParser read FParser;
    property  RuleSet: ThtmlcssRuleSet read FRuleSet;

    function  GetClassID(const ClassIDBuf: PWideChar; const ClassIDLen: Integer): Integer; overload;
    function  GetClassID(const ClassID: String): Integer; overload;
    function  RequireClassID(const ClassIDBuf: PWideChar; const ClassIDLen: Integer): Integer; overload;
    function  RequireClassID(const ClassID: String): Integer; overload;
    function  GetElementID(const ElementID: String): Integer;
    function  RequireElementID(const ElementID: String): Integer;

    // Initialize document style
    procedure InitStyle(const ReaderStyleText, DocumentStyleText: String;
              DocumentStyleRef: StringArray);
    function  GetRequiredImports: StringArray;
    procedure SetImportedStyleState(const Source: String;
              const State: ThtmlcssRuleSetState; const StyleText: String);
    function  GetStyleSheetState: ThtmlcssStyleSheetState;
    function  GetImportProgress: Integer;

    // Initialize element style
    function  ParseDeclarations(const Text: String): ThtmlcssDeclarations;
    procedure ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
              const ParentStyle: ThtmlcssStyleProperties;
              const ElementInfo: ThtmlcssElementInfo);
  end;



{                                                                              }
{ ThtmlcssParser                                                               }
{                                                                              }
  ThtmlcssParser = class
  protected
    FData     : Pointer;
    FDataSize : Integer;
    FDataPos  : Integer;
    FText     : String;

    procedure Init;
    procedure SetText(const Text: String);

    function  GetBuf(var Buf: PWideChar; var Size: Integer): Boolean;
    function  SkipSpace(var P: PWideChar): Integer;

    function  ParseDeclaration: ThtmlcssProperty;
    function  ParseSelector(const RuleSet: ThtmlcssRuleSet): ThtmlcssSelector;
    function  ParseSelectors(const RuleSet: ThtmlcssRuleSet): ThtmlcssSelectors;
    function  ParseRule(const RuleSet: ThtmlcssRuleSet): ThtmlcssRule;

  public
    procedure InitBuf(const Buf: Pointer; const Size: Integer);
    procedure InitText(const Text: String);

    property  Data: Pointer read FData;
    property  DataSize: Integer read FDataSize;
    property  Text: String read FText write SetText;

    function  ParseDeclarations: ThtmlcssDeclarations;
    procedure ParseRules(const RuleSet: ThtmlcssRuleSet);
  end;



implementation

uses
  { System }
  {$IFDEF DELPHI6_UP}
  Windows,
  {$ENDIF}

  { Fundamentals }
  flcUtils,
  flcDynArrays,
  flcStrings,
  flcZeroTermStrings;



{                                                                              }
{ ThtmlcssStyleProperties                                                      }
{                                                                              }
procedure InitDefaultStyleProperties(var StyleInfo: ThtmlcssStyleProperties);
begin
  FillChar(StyleInfo, Sizeof(StyleInfo), #0);
  with StyleInfo do
    begin
      BackColor.ColorType := colorNamed;
      BackColor.NamedColor := 'white';
      SetLength(FontFamily, 1);
      FontFamily[0].FamilyType := ffamGenericFamily;
      FontFamily[0].GenericFamily := gffamSerif;
      FontStyle := fstyleNormal;
      FontVariant := fvariantNormal;
      FontWeight := fweightNormal;
      FontSize.SizeType := fsizeHTMLAbsolute;
      FontSize.Value := 3.0;
      FontColor.ColorType := colorNamed;
      FontColor.NamedColor := 'black';
      TextDecoration := tdecoNone;
      VerticalAlign.AlignType := valignBaseline;
      TextAlign := talignLeft;
      LineHeight.LengthType := lenDefault;
      WhiteSpace := wsNormal;
    end;
end;

procedure InitElementStyleProperties(var StyleInfo: ThtmlcssStyleProperties;
    const ParentStyle: ThtmlcssStyleProperties; const ElementInfo: ThtmlcssElementInfo);
var FontWeight: ThtmlFontWeight;
begin
  case ElementInfo.TagID of
    HTML_TAG_BODY   :
      begin
        SetLength(StyleInfo.FontFamily, 1);
        StyleInfo.FontFamily[0].FamilyType := ffamGenericFamily;
        StyleInfo.FontFamily[0].GenericFamily := gffamSerif;
        StyleInfo.FontColor.ColorType := colorNamed;
        StyleInfo.FontColor.NamedColor := 'black';
        StyleInfo.BackColor.ColorType := colorNamed;
        StyleInfo.BackColor.NamedColor := 'white';
        StyleInfo.FontSize.SizeType := fsizeHTMLAbsolute;
        StyleInfo.FontSize.Value := 3.0;
      end;
    HTML_TAG_A      :
      begin
        if HTML_CSS_PP_Link in ElementInfo.PseudoIDs then
          begin
            StyleInfo.FontColor.ColorType := colorNamed;
            StyleInfo.FontColor.NamedColor := 'blue';
            StyleInfo.TextDecoration := tdecoUnderline;
          end else
        if HTML_CSS_PP_Visited in ElementInfo.PseudoIDs then
          begin
            StyleInfo.FontColor.ColorType := colorNamed;
            StyleInfo.FontColor.NamedColor := 'red';
            StyleInfo.TextDecoration := tdecoUnderline;
          end;
      end;
    HTML_TAG_PRE,
    HTML_TAG_TT,
    HTML_TAG_CODE,
    HTML_TAG_KBD,
    HTML_TAG_SAMP   :
      begin
        SetLength(StyleInfo.FontFamily, 1);
        StyleInfo.FontFamily[0].FamilyType := ffamGenericFamily;
        StyleInfo.FontFamily[0].GenericFamily := gffamMonospace;
        if ElementInfo.TagID = HTML_TAG_PRE then
          StyleInfo.WhiteSpace := wsPre;
      end;
    HTML_TAG_H1     :
      begin
        StyleInfo.FontWeight := fweightBold;
        StyleInfo.FontSize.SizeType := fsizeHTMLAbsolute;
        StyleInfo.FontSize.Value := 7.0;
        {    HTML_CSS_PROP_Text_Align: Value := 'center'; }
      end;
    HTML_TAG_H2     :
      begin
        StyleInfo.FontWeight := fweightBold;
        StyleInfo.FontSize.SizeType := fsizeHTMLAbsolute;
        StyleInfo.FontSize.Value := 6.0;
      end;
    HTML_TAG_H3     :
      begin
        StyleInfo.FontStyle := fstyleItalic;
        StyleInfo.FontSize.SizeType := fsizeHTMLAbsolute;
        StyleInfo.FontSize.Value := 5.0;
      end;
    HTML_TAG_H4     :
      begin
        StyleInfo.FontWeight := fweightBold;
      end;
    HTML_TAG_H5     :
      begin
        StyleInfo.FontStyle := fstyleItalic;
      end;
    HTML_TAG_H6     :
      begin
        StyleInfo.FontWeight := fweightBold;
      end;
    HTML_TAG_B,
    HTML_TAG_STRONG :
      begin
        FontWeight := fweightBolder;
        htmlResolveRelFontWeight(FontWeight, ParentStyle.FontWeight);
        StyleInfo.FontWeight := FontWeight;
      end;
    HTML_TAG_I,
    HTML_TAG_CITE,
    HTML_TAG_EM,
    HTML_TAG_VAR,
    HTML_TAG_ADDRESS,
    HTML_TAG_BLOCKQUOTE :
      begin
        StyleInfo.FontStyle := fstyleItalic;
      end;
  end;
  // non-inherited properties reset to default for every element
  htmlcssStyleResetNonInherited(StyleInfo);
end;

function StyleResolveSystemFontName(const StyleInfo: ThtmlcssStyleProperties): String;
var I: Integer;
begin
  Result := '';
  for I := 0 to Length(StyleInfo.FontFamily) - 1 do
    with StyleInfo.FontFamily[I] do
      begin
        case FamilyType of
          ffamFamilyName :
            Result := FamilyName;
          ffamGenericFamily :
            case GenericFamily of
              gffamSerif     : Result := 'Times New Roman';
              gffamSansSerif : Result := 'MS Arial';
              gffamCursive   : Result := 'Script';
              gffamFantasy   : Result := 'Comic Sans MS';
              gffamMonospace : Result := 'Courier New';
            end;
        end;
        if Result <> '' then
          exit;
      end;
end;

function StyleResolveSystemFontSize(const StyleInfo: ThtmlcssStyleProperties): Integer;
begin
  Result := htmlResolveFontSizePoints(StyleInfo.FontSize);
  if Result = 0 then
    Result := -htmlResolveFontSizePixels(StyleInfo.FontSize);
end;

procedure StyleResolveSystemFont(const StyleInfo: ThtmlcssStyleProperties;
          var Name: String; var Styles: TFontStyles; var Size: Integer);
begin
  Name := StyleResolveSystemFontName(StyleInfo);
  // Styles
  Styles := [];
  if htmlIsFontWeightBold(StyleInfo.FontWeight) then
    Include(Styles, TFontStyle.fsBold);
  case StyleInfo.TextDecoration of
    tdecoUnderline   : Include(Styles, TFontStyle.fsUnderline);
    tdecoLineThrough : Include(Styles, TFontStyle.fsStrikeOut);
  end;
  Size := StyleResolveSystemFontSize(StyleInfo);
end;

function StyleResolveSystemColor(const Color: ThtmlColor): TColor;
begin
  Result := htmlColorValue(Color).Color;
end;



{                                                                              }
{ ThtmlcssSelector                                                             }
{                                                                              }
constructor ThtmlcssSelector.Create(const TagID: ThtmlTagID;
    const ClassID: Integer; const ElementID: Integer;
    const PseudoPropIDs: ThtmlcssPseudoPropertyIDSet;
    const AncestorSelector: ThtmlcssSelector);
var I: ThtmlcssPseudoPropertyID;
begin
  inherited Create;
  FTagID := TagID;
  FClassID := ClassID;
  FElementID := ElementID;
  FPseudoPropIDs := PseudoPropIDs;
  FAncestorSelector := AncestorSelector;
  // calculate specificity
  FSpecificity := 0;
  if FElementID >= 0 then
    Inc(FSpecificity, $10000);
  if FClassID >= 0 then
    Inc(FSpecificity, $100);
  if FTagID <> HTML_TAG_None then
    Inc(FSpecificity, $1);
  for I := HTML_CSS_PP_FirstID to HTML_CSS_PP_LastID do
    if I in FPseudoPropIDs then
      if I in HTML_CSS_PP_PseudoClasses then
        Inc(FSpecificity, $100) else // pseudo-class has same weight as class
        Inc(FSpecificity, $1);       // pseudo-element has same weight as element
  if Assigned(FAncestorSelector) then
    Inc(FSpecificity, FAncestorSelector.Specificity);
  Assert(FSpecificity > 0, 'FSpecificity > 0');
end;

destructor ThtmlcssSelector.Destroy;
begin
  FreeAndNil(FAncestorSelector);
  inherited Destroy;
end;

function ThtmlcssSelector.Match(const ElementInfo: ThtmlcssElementInfo): Boolean;
var I: Integer;
    P: PhtmlcssElementInfo;
    ClassIDMatch: Boolean;
begin
  Result := False;
  // check tag match
  if (FTagID <> HTML_TAG_None) and (ElementInfo.TagID <> FTagID) then
    exit;
  // check class ID match
  if FClassID >= 0 then
    begin
      ClassIDMatch := False;
      for I := 0 to Length(ElementInfo.ClassIDs) - 1 do
        if ElementInfo.ClassIDs[I] = FClassID then
          begin
            ClassIDMatch := True;
            break;
          end;
      if not ClassIDMatch then
        exit;
    end;
  // check ID match
  if (FElementID >= 0) and (ElementInfo.ElementID <> FElementID) then
    exit;
  // check pseudo ID match
  if (FPseudoPropIDs <> []) and (ElementInfo.PseudoIDs * FPseudoPropIDs <> FPseudoPropIDs) then
    exit;
  // check context selector pattern match (nfa)
  if Assigned(FAncestorSelector) then
    begin
      P := ElementInfo.ParentInfo;
      while Assigned(P) do
        begin
          if FAncestorSelector.Match(P^) then
            // match
            break;
          P := P.ParentInfo;
        end;
      if not Assigned(P) then
        // no match
        exit;
    end;
  // selector matches
  Result := True;
end;



{                                                                              }
{ ThtmlcssSelectors                                                            }
{                                                                              }
destructor ThtmlcssSelectors.Destroy;
var I, N: ThtmlcssSelector;
begin
  I := FFirst;
  while Assigned(I) do
    begin
      N := I.FNext;
      I.Free;
      I := N;
    end;
  inherited;
end;

procedure ThtmlcssSelectors.AddSelector(const Selector: ThtmlcssSelector);
begin
  if not Assigned(FFirst) then
    FFirst := Selector;
  if Assigned(FLast) then
    FLast.FNext := Selector;
  FLast := Selector;
  Selector.FNext := nil;
end;

function ThtmlcssSelectors.MatchingSelector(const ElementInfo: ThtmlcssElementInfo): ThtmlcssSelector;
var Pri, MaxPri: Integer;
    Selector: ThtmlcssSelector;
begin
  // locate matching selector with highest priority
  Result := nil;
  MaxPri := 0;
  Selector := FFirst;
  while Assigned(Selector) do
    begin
      if Selector.Match(ElementInfo) then
        begin
          Pri := Selector.Specificity;
          if Pri > MaxPri then
            begin
              MaxPri := Pri;
              Result := Selector;
            end;
        end;
      Selector := Selector.FNext;
    end;
end;



{                                                                              }
{ ThtmlcssDeclarations                                                         }
{                                                                              }
destructor ThtmlcssDeclarations.Destroy;
var I, N: ThtmlcssProperty;
begin
  I := FFirst;
  while Assigned(I) do
    begin
      N := I.Next;
      I.Free;
      I := N;
    end;
  inherited Destroy;
end;

procedure ThtmlcssDeclarations.AddDeclaration(const Declaration: ThtmlcssProperty);
begin
  if not Assigned(FFirst) then
    FFirst := Declaration;
  if Assigned(FLast) then
    FLast.Next := Declaration;
  FLast := Declaration;
  Declaration.Next := nil;
end;

procedure ThtmlcssDeclarations.ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
    const ParentStyle: ThtmlcssStyleProperties);
var I: ThtmlcssProperty;
begin
  // apply declaration's style info
  I := FFirst;
  while Assigned(I) do
    begin
      I.ApplyStyleInfo(StyleInfo, ParentStyle);
      I := I.Next;
    end;
end;



{                                                                              }
{ ThtmlcssMatchingDeclarations                                                 }
{                                                                              }
procedure ThtmlcssMatchingDeclarations.Clear;
begin
  FCount := 0;
end;

procedure ThtmlcssMatchingDeclarations.AddMatch(const Specificity: Integer;
    const Declarations: ThtmlcssDeclarations);
var L: Integer;
begin
  L := FCount;
  Inc(FCount);
  if L >= Length(FMatches) then
    SetLength(FMatches, L + 1);
  FMatches[L].Specificity := Specificity;
  FMatches[L].Declarations := Declarations;
  FMatches[L].Order := L;
end;

procedure ThtmlcssMatchingDeclarations.Sort;

  procedure QuickSort(L, R: Integer);
  var I, J, M    : Integer;
      IV, JV, MV : PhtmlcssMatchingDeclarationsItem;
      Tmp        : ThtmlcssMatchingDeclarationsItem;
  begin
    repeat
      I := L;
      J := R;
      M := (L + R) shr 1;
      repeat
        MV := @FMatches[M];
        repeat
          IV := @FMatches[I];
          if (IV^.Specificity < MV^.Specificity) or
            ((IV^.Specificity = MV^.Specificity) and (IV^.Order < MV^.Order)) then
            Inc(I) else
            break;
        until False;
        repeat
          JV := @FMatches[J];
          if (JV^.Specificity > MV^.Specificity) or
            ((JV^.Specificity = MV^.Specificity) and (JV^.Order > MV^.Order)) then
            Dec(J) else
            break;
        until False;
        if I <= J then
          begin
            Tmp := IV^;
            IV^ := JV^;
            JV^ := Tmp;
            if M = I then
              M := J else
              if M = J then
                M := I;
            Inc(I);
            Dec(J);
          end;
      until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    until I >= R;
  end;

var L: Integer;
begin
  // Sort matches by specificity and order
  L := FCount;
  if L > 0 then
    QuickSort(0, L - 1);
end;

procedure ThtmlcssMatchingDeclarations.ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
    const ParentStyle: ThtmlcssStyleProperties);
var I: Integer;
begin
  for I := 0 to FCount - 1 do
    FMatches[I].Declarations.ApplyStyleInfo(StyleInfo, ParentStyle);
end;



{                                                                              }
{ ThtmlcssRule                                                                 }
{                                                                              }
constructor ThtmlcssRule.Create(const Selectors: ThtmlcssSelectors;
    const Declarations: ThtmlcssDeclarations);
begin
  inherited Create;
  FSelectors := Selectors;
  FDeclarations := Declarations;
end;

destructor ThtmlcssRule.Destroy;
begin
  FreeAndNil(FDeclarations);
  FreeAndNil(FSelectors);
  inherited Destroy;
end;

procedure ThtmlcssRule.AddMatch(const ElementInfo: ThtmlcssElementInfo;
    const Matches: ThtmlcssMatchingDeclarations);
var Selector: ThtmlcssSelector;
begin
  Selector := FSelectors.MatchingSelector(ElementInfo);
  if not Assigned(Selector) then
    // rule selector does not match
    exit;
  // add match
  Matches.AddMatch(Selector.Specificity, FDeclarations);
end;



{                                                                              }
{ ThtmlcssRuleSet                                                              }
{                                                                              }
constructor ThtmlcssRuleSet.Create(const CSS: ThtmlCSS;
    const State: ThtmlcssRuleSetState; const Source: String);
begin
  inherited Create;
  FCSS := CSS;
  FState := State;
  FSource := Source;
end;

destructor ThtmlcssRuleSet.Destroy;
var I, N: ThtmlcssRule;
begin
  I := FFirst;
  while Assigned(I) do
    begin
      N := I.FNext;
      I.Free;
      I := N;
    end;
  inherited Destroy;
end;

procedure ThtmlcssRuleSet.AddRule(const Rule: ThtmlcssRule);
begin
  if not Assigned(FFirst) then
    FFirst := Rule;
  if Assigned(FLast) then
    FLast.FNext := Rule;
  FLast := Rule;
  Rule.FNext := nil;
end;

procedure ThtmlcssRuleSet.AddImport(const Source: String);
begin
  DynArrayAppend(ObjectArray(FImports), ThtmlcssRuleSet.Create(FCSS, rsNotImported, Source));
end;

procedure ThtmlcssRuleSet.GetRequiredImports(var Sources: StringArray);
var S: String;
    I: Integer;
begin
  if FState = rsNotImported then
    begin
      S := FSource;
      StrTrimInPlaceU(S);
      if (S <> '') and (PosNextNoCaseU(S, Sources, -1, False) < 0) then
        DynArrayAppend(Sources, S);
    end;
  for I := 0 to Length(FImports) - 1 do
    FImports[I].GetRequiredImports(Sources);
end;

procedure ThtmlcssRuleSet.SetImportedStyleState(const Source: String;
    const State: ThtmlcssRuleSetState; const StyleText: String);
var I: Integer;
begin
  if (FState in [rsNotImported, rsBusyImporting]) and
     StrEqualNoAsciiCaseU(Source, FSource) then
    begin
      FState := State;
      if StyleText <> '' then
        begin
          FCSS.Parser.SetText(StyleText);
          FCSS.Parser.ParseRules(self);
        end;
    end;
  for I := 0 to Length(FImports) - 1 do
    FImports[I].SetImportedStyleState(Source, State, StyleText);
end;

function ThtmlcssRuleSet.GetStyleSheetState: ThtmlcssStyleSheetState;
var I: Integer;
begin
  case FState of
    rsLocal,
    rsImported      : Result := ssReady;
    rsBusyImporting : Result := ssBusyImporting;
    rsImportFailed  :
      begin
        Result := ssImportFailed;
        exit;
      end;
  else
    Result := ssNotImported;
  end;
  for I := 0 to Length(FImports) - 1 do
    case FImports[I].GetStyleSheetState of
      ssNotImported :
        if Result = ssReady then
          Result := ssNotImported;
      ssReady : ;
      ssBusyImporting :
        if Result in [ssReady, ssNotImported] then
          Result := ssBusyImporting;
      ssImportFailed :
        begin
          Result := ssImportFailed;
          exit;
        end;
    end;
end;

procedure ThtmlcssRuleSet.GetImportProgress(var StateVal, TotVal: Integer);
var I: Integer;
begin
  case FState of
    rsImported,
    rsImportFailed :
      begin
        Inc(StateVal, 10);
        Inc(TotVal, 10);
      end;
    rsBusyImporting :
      begin
        Inc(StateVal);
        Inc(TotVal, 10);
      end;
  end;
  for I := 0 to Length(FImports) - 1 do
    FImports[I].GetImportProgress(StateVal, TotVal);
end;

procedure ThtmlcssRuleSet.GetMatches(const ElementInfo: ThtmlcssElementInfo;
    const Matches: ThtmlcssMatchingDeclarations);
var Rule: ThtmlcssRule;
    I: Integer;
begin
  if FState in [rsNotImported, rsBusyImporting] then
    exit;
  // check imported rules (imported rules have lower cascade order)
  for I := 0 to Length(FImports) - 1 do
    FImports[I].GetMatches(ElementInfo, Matches);
  // check rules
  Rule := FFirst;
  while Assigned(Rule) do
    begin
      Rule.AddMatch(ElementInfo, Matches);
      Rule := Rule.FNext;
    end;
end;



{                                                                              }
{ ThtmlCSS                                                                     }
{                                                                              }
constructor ThtmlCSS.Create;
begin
  inherited Create;
  FClassIDs := THashedUnicodeStringArray.Create(False);
  FElementIDs := THashedUnicodeStringArray.Create(True);
  FRuleSet := ThtmlcssRuleSet.Create(self, rsLocal, '');
  FMatches := ThtmlcssMatchingDeclarations.Create;
  FParser := ThtmlcssParser.Create;
end;

destructor ThtmlCSS.Destroy;
begin
  FreeAndNil(FParser);
  FreeAndNil(FMatches);
  FreeAndNil(FRuleSet);
  FreeAndNil(FElementIDs);
  FreeAndNil(FClassIDs);
  inherited Destroy;
end;

function ThtmlCSS.AddClassID(const ClassID: String): Integer;
begin
  Result := FClassIDs.AppendItem(ClassID);
end;

function ThtmlCSS.GetClassID(const ClassIDBuf: PWideChar; const ClassIDLen: Integer): Integer;
begin
  if ClassIDLen <= 0 then
    Result := -1
  else
    Result := FClassIDs.PosNext(ClassIDBuf); // ?
end;

function ThtmlCSS.GetClassID(const ClassID: String): Integer;
begin
  Result := GetClassID(Pointer(ClassID), Length(ClassID));
end;

function ThtmlCSS.RequireClassID(const ClassIDBuf: PWideChar; const ClassIDLen: Integer): Integer;
var S: String;
begin
  if ClassIDLen <= 0 then
    begin
      Result := -1;
      exit;
    end;
  Result := GetClassID(ClassIDBuf, ClassIDLen);
  if Result >= 0 then
    exit;
  SetLength(S, ClassIDLen);
  Move(ClassIDBuf^, Pointer(S)^, ClassIDLen * SizeOf(WideChar));
  Result := AddClassID(S);
end;

function ThtmlCSS.RequireClassID(const ClassID: String): Integer;
begin
  Result := RequireClassID(Pointer(ClassID), Length(ClassID));
end;

function ThtmlCSS.GetElementID(const ElementID: String): Integer;
begin
  if ElementID = '' then
    Result := -1
  else
    Result := FElementIDs.PosNext(ElementID);
end;

function ThtmlCSS.AddElementID(const ElementID: String): Integer;
begin
  Result := FElementIDs.AppendItem(ElementID);
end;

function ThtmlCSS.RequireElementID(const ElementID: String): Integer;
begin
  if ElementID = '' then
    begin
      Result := -1;
      exit;
    end;
  Result := GetElementID(ElementID);
  if Result >= 0 then
    exit;
  Result := AddElementID(ElementID);
end;

procedure ThtmlCSS.InitStyle(const ReaderStyleText, DocumentStyleText: String;
    DocumentStyleRef: StringArray);
var I : Integer;
begin
  // Init Reader style (lower cascading order than Document style)
  if ReaderStyleText <> '' then
    begin
      FParser.SetText(ReaderStyleText);
      FParser.ParseRules(FRuleSet);
    end;
  // Init Document style
  for I := 0 to Length(DocumentStyleRef) - 1 do
    FRuleSet.AddImport(DocumentStyleRef[I]);
  if DocumentStyleText <> '' then
    begin
      FParser.SetText(DocumentStyleText);
      FParser.ParseRules(FRuleSet);
    end;
end;

function ThtmlCSS.GetRequiredImports: StringArray;
begin
  Result := nil;
  FRuleSet.GetRequiredImports(Result);
end;

procedure ThtmlCSS.SetImportedStyleState(const Source: String;
    const State: ThtmlcssRuleSetState; const StyleText: String);
begin
  FRuleSet.SetImportedStyleState(Source, State, StyleText);
end;

function ThtmlCSS.GetStyleSheetState: ThtmlcssStyleSheetState;
begin
  Result := FRuleSet.GetStyleSheetState;
end;

function ThtmlCSS.GetImportProgress: Integer;
var I, J: Integer;
begin
  I := 0;
  J := 0;
  FRuleSet.GetImportProgress(I, J);
  if J = 0 then
    Result := 100
  else
    Result := Round(I / J * 100.0);
end;

function ThtmlCSS.ParseDeclarations(const Text: String): ThtmlcssDeclarations;
begin
  FParser.SetText(Text);
  Result := FParser.ParseDeclarations;
end;

procedure ThtmlCSS.ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
    const ParentStyle: ThtmlcssStyleProperties; const ElementInfo: ThtmlcssElementInfo);
begin
  FMatches.Clear;
  FRuleSet.GetMatches(ElementInfo, FMatches);
  if FMatches.Count > 0 then
    begin
      FMatches.Sort;
      FMatches.ApplyStyleInfo(StyleInfo, ParentStyle);
    end;
end;



{                                                                              }
{ ThtmlcssParser                                                               }
{                                                                              }
const
  htmlcssWhiteSpace = [#0..#32];
  htmlcssNameChar   = ['a'..'z', 'A'..'Z', '-', '0'..'9', '_'];

procedure ThtmlcssParser.Init;
begin
  FDataPos := 0;
end;

procedure ThtmlcssParser.InitBuf(const Buf: Pointer; const Size: Integer);
begin
  FData := Buf;
  FDataSize := Size;
  FText := '';
  Init;
end;

procedure ThtmlcssParser.InitText(const Text: String);
begin
  FText := Text;
  FData := Pointer(FText);
  FDataSize := Length(FText);
  Init;
end;

procedure ThtmlcssParser.SetText(const Text: String);
begin
  InitText(Text);
end;

function ThtmlcssParser.GetBuf(var Buf: PWideChar; var Size: Integer): Boolean;
var I, L: Integer;
begin
  I := FDataPos;
  L := FDataSize;
  if I < L then
    begin
      // Data available
      Buf := FData;
      Inc(Buf, I);
      Size := L - I;
      Result := True;
    end
  else
    begin
      // EOF
      Buf := nil;
      Size := 0;
      Result := False;
    end;
end;

function ThtmlcssParser.SkipSpace(var P: PWideChar): Integer;
begin
  Result := 0;
  Inc(Result, StrZSkipAllW(P, htmlcssWhiteSpace));
  while StrPMatchStrU(P, 2, '/*') do // comment
    begin
      Inc(P, 2);
      Inc(Result, 2);
      Inc(Result, StrZSkipToStrU(P, '*/', True));
      Inc(P, 2);
      Inc(Result, 2);
      Inc(Result, StrZSkipAllW(P, htmlcssWhiteSpace));
    end;
end;

function ThtmlcssParser.ParseDeclaration: ThtmlcssProperty;
var P, Q: PWideChar;
    L, M, N: Integer;
    PropID: ThtmlcssPropertyID;
    Value: String;
    Imp: Boolean;
begin
  Result := nil;
  if not GetBuf(P, M) then
    exit;
  L := M;
  try
    // Name
    Dec(L, SkipSpace(P));
    if L <= 0 then
      exit;
    Q := P;
    N := StrZSkipAllW(P, htmlcssNameChar);
    if N = 0 then
      exit;
    PropID := htmlcssGetPropertyIDPtrW(Q, N);
    Dec(L, N);
    if L <= 0 then
      exit;
    // Value
    Dec(L, SkipSpace(P));
    if L <= 0 then
      exit;
    if P^ <> ':' then
      exit;
    Inc(P);
    Dec(L);
    Dec(L, SkipSpace(P));
    if L <= 0 then
      exit;
    Value := StrZExtractToU(P, [';', '!', '}']);
    Dec(L, Length(Value));
    if L < 0 then
      exit;
    StrTrimInPlaceU(Value, htmlcssWhiteSpace);
    Imp := False;
    if P^ = '!' then
      begin
        Inc(P);
        Dec(L);
        while (L > 0) and not WideCharInCharSet(P^, [';', '}']) do
          begin
            Dec(L, SkipSpace(P));
            if L <= 0 then
              break;
            Q := P;
            N := StrZSkipAllW(P, htmlcssNameChar);
            if (N = 9) and StrPMatchNoAsciiCaseW(Q, 'important', 9) then
              Imp := True;
            Dec(L, N);
          end;
      end;
    Result := htmlcssGetPropertyClass(PropID).Create(PropID, Value, Imp);
  finally
    Inc(FDataPos, M - L);
  end;
end;

function ThtmlcssParser.ParseDeclarations: ThtmlcssDeclarations;
var Decl : ThtmlcssProperty;
    P    : PWideChar;
    L    : Integer;
begin
  Result := nil;
  repeat
    // parse declaration
    Decl := ParseDeclaration;
    if Assigned(Decl) then
      begin
        if not Assigned(Result) then
          Result := ThtmlcssDeclarations.Create;
        Result.AddDeclaration(Decl);
      end;
    // skip to start of next declaration
    if not GetBuf(P, L) then
      exit;
    Inc(FDataPos, StrZSkipToCharW(P, [';', '}']));
    if P^ = ';' then
      Inc(FDataPos)
    else
      break;
  until False;
end;

const
  SelectorStartChar = htmlcssNameChar + ['#', ':', '.'];

function ThtmlcssParser.ParseSelector(const RuleSet: ThtmlcssRuleSet): ThtmlcssSelector;
var P, Q : PWideChar;
    L, M, N : Integer;
    TagID: ThtmlTagID;
    IDStr: String;
    PPIDs: ThtmlcssPseudoPropertyIDSet;
    PPID: ThtmlcssPseudoPropertyID;
    ClassID: Integer;
    OuterSel: ThtmlcssSelector;
begin
  Result := nil;
  if not GetBuf(P, M) then
    exit;
  L := M;
  try
    Dec(L, SkipSpace(P));
    if L <= 0 then
      exit;
    OuterSel := nil;
    repeat
      // tag name
      Q := P;
      N := StrZSkipAllW(P, htmlcssNameChar);
      if N > 0 then
        begin
          Dec(L, N);
          TagID := htmlGetTagIDPtrW(Q, N);
        end
      else
        TagID := HTML_TAG_None;
      IDStr := '';
      PPIDs := [];
      ClassID := -1;
      repeat
        case P^ of
          '#' :
            begin
              Inc(P);
              Dec(L);
              Dec(L, SkipSpace(P)); // space actually not allowed here
              // parse id
              IDStr := StrZExtractAllU(P, htmlcssNameChar);
              Dec(L, Length(IDStr));
            end;
          ':' :
            begin
              Inc(P);
              Dec(L);
              Dec(L, SkipSpace(P)); // space actually not allowed here
              // parse pseudo-class id
              Q := P;
              N := StrZSkipAllW(P, htmlcssNameChar);
              if N > 0 then
                begin
                  Dec(L, N);
                  PPID := htmlcssGetPseudoPropIDPtrW(Q, N);
                  if PPID <> HTML_CSS_PP_None then
                    Include(PPIDs, PPID);
                end;
            end;
          '.' :
            begin
              Inc(P);
              Dec(L);
              Dec(L, SkipSpace(P)); // space actually not allowed here
              // parse class id
              Q := P;
              N := StrZSkipAllW(P, htmlcssNameChar);
              if N > 0 then
                begin
                  Dec(L, N);
                  ClassID := RuleSet.CSS.RequireClassID(Q, N);
                end;
            end;
        else
          break;
        end;
      until (L <= 0) or WideCharInCharSet(P^, htmlcssWhiteSpace);
      if (TagID = HTML_Tag_None) and (IDStr = '') and (PPIDs = []) and
         (ClassID = -1) then
        begin
          // no more selectors
          Result := OuterSel;
          break;
        end
      else
        begin
          if (TagID = HTML_TAG_None) and (PPIDs * HTML_CSS_PP_Properties_Anchor <> []) then
            TagID := HTML_TAG_A;
          Result := ThtmlcssSelector.Create(TagID, ClassID,
              RuleSet.CSS.RequireElementID(IDStr), PPIDs, OuterSel);
          OuterSel := Result;
        end;
      Dec(L, SkipSpace(P));
    until (L <= 0) or not WideCharInCharSet(P^, SelectorStartChar);
  finally
    Inc(FDataPos, M - L);
  end;
end;

function ThtmlcssParser.ParseSelectors(const RuleSet: ThtmlcssRuleSet): ThtmlcssSelectors;
var Sel: ThtmlcssSelector;
    P: PWideChar;
    L: Integer;
begin
  Result := nil;
  repeat
    // parse selector
    Sel := ParseSelector(RuleSet);
    if not Assigned(Sel) then
      exit;
    if not Assigned(Result) then
      Result := ThtmlcssSelectors.Create;
    Result.AddSelector(Sel);
    // parse selector ',' seperator
    if not GetBuf(P, L) then
      exit;
    Inc(FDataPos, SkipSpace(P));
    if P^ <> ',' then
      exit;
    Inc(FDataPos);
  until False;
end;

function ThtmlcssParser.ParseRule(const RuleSet: ThtmlcssRuleSet): ThtmlcssRule;
var Sel: ThtmlcssSelectors;
    Decl: ThtmlcssDeclarations;
    P, Q: PWideChar;
    L: Integer;
    S: String;
begin
  Result := nil;
  if not GetBuf(P, L) then
    exit;
  // @ rules
  while P^ = '@' do
    begin
      Inc(P);
      Inc(FDataPos);
      Q := P;
      L := StrZSkipToCharW(P, [';']);
      Inc(FDataPos, L);
      if (L > 0) and StrPMatchNoAsciiCaseW(Q, 'import ', 7) then
        begin
          // import rule
          Inc(Q, 7);
          SetLength(S, L - 7);
          if L > 7 then
            Move(Q^, Pointer(S)^, (L - 7) * SizeOf(WideChar));
          RuleSet.AddImport(S);
        end;
      Inc(P);
      Inc(FDataPos);
      Inc(FDataPos, SkipSpace(P));
    end;
  // parse selectors
  Sel := ParseSelectors(RuleSet);
  if GetBuf(P, L) then
    begin
      // skip to start of declaration (skip unknown characters in selector)
      Inc(FDataPos, StrZSkipToCharW(P, ['{']));
      Inc(FDataPos);
      // parse declarations
      Decl := ParseDeclarations;
    end else
    Decl := nil;
  if GetBuf(P, L) then
    begin
      // skip to end of rule (skip unknown characters in declarations)
      Inc(FDataPos, StrZSkipToCharW(P, ['}']));
      Inc(FDataPos);
    end;
  // create rule
  if not Assigned(Sel) or not Assigned(Decl) then
    begin
      Sel.Free;
      Decl.Free;
      Result := nil;
      exit;
    end;
  Result := ThtmlcssRule.Create(Sel, Decl);
end;

procedure ThtmlcssParser.ParseRules(const RuleSet: ThtmlcssRuleSet);
var Rule : ThtmlcssRule;
    P    : PWideChar;
    L    : Integer;
begin
  repeat
    // parse rule
    Rule := ParseRule(RuleSet);
    if Assigned(Rule) then
      RuleSet.AddRule(Rule);
    // skip to start of next rule
    if not GetBuf(P, L) then
      exit;
    Inc(FDataPos, SkipSpace(P));
  until FDataPos >= FDataSize;
end;



end.

