{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals 5.00 - HTML Parser                          }
{   File name:        flcHTMLDoc.pas                                           }
{   File version:     5.03                                                     }
{   Description:      HTML document                                            }
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
{   2002/10/21  1.00  Initial version in cHTMLObjects                          }
{   2002/12/08  1.01  Created unit cHTMLDoc.                                   }
{   2015/04/11  1.02  UnicodeString changes.                                   }
{   2019/02/22  5.03  Revise for Fundamentals 5.                               }
{                                                                              }
{******************************************************************************}

{$INCLUDE flcHTML.inc}

unit flcHTMLDoc;

interface

uses
  { Fundamentals }
  flcStdTypes,

  { HTML }
  flcHTMLStyleSheet,
  flcHTMLDocBase,
  flcHTMLDocElements;



{                                                                              }
{ ThtmlDocument                                                                }
{                                                                              }
type
  ThtmlDocumentState = (
      htdocInit,
      htdocStructurePrepared,
      htdocPreparingStyle,
      htdocStylePrepared,
      htdocRefactoredForLayout);

  ThtmlDocument = class(AhtmlContainerObject)
  protected
    FStyleSheet : ThtmlCSS;
    FState      : ThtmlDocumentState;

    function  GetHTML: ThtmlHTML;

  public
    constructor Create; override;
    destructor Destroy; override;

    { Document objects                                                         }
    function  CreateItem(const ID: Integer; const Name: String): AhtmlObject; override;
    property  HTML: ThtmlHTML read GetHTML;
    property  StyleSheet: ThtmlCSS read FStyleSheet;

    { Document state                                                           }
    property  State: ThtmlDocumentState read FState;

    { Initial preparation of the document structure                            }
    procedure PrepareStructure;

    { Parse and apply style sheet information                                  }
    procedure InitDocStyle(const ReaderStyle: String;
              var ExternalStyles: StringArray);
    procedure SetExternalStyleState(const Source: String;
              const State: ThtmlcssRuleSetState;
              const StyleText: String);
    procedure SetContentStyle;

    { Layout preparation                                                       }
    function  ReadyForLayout: Boolean;
    procedure RefactorForLayout;
  end;



implementation

uses
  { System }
  SysUtils,

  { Fundamentals }
  flcDynArrays,

  { HTML }
  flcHTMLElements,
  flcHTMLStyleProperties;



{                                                                              }
{ ThtmlDocument                                                                }
{                                                                              }
constructor ThtmlDocument.Create;
begin
  inherited Create;
  FStyleSheet := ThtmlCSS.Create;
  FState := htdocInit;
end;

destructor ThtmlDocument.Destroy;
begin
  FreeAndNil(FStyleSheet);
  inherited Destroy;
end;

function ThtmlDocument.CreateItem(const ID: Integer; const Name: String): AhtmlObject;
begin
  if ThtmlTagID(ID) = HTML_TAG_HTML then
    Result := ThtmlHTML.Create
  else
    Result := inherited CreateItem(ID, Name);
end;

function ThtmlDocument.GetHTML: ThtmlHTML;
begin
  Result := ThtmlHTML(RequireItemByClass(ThtmlHTML));
end;

procedure ThtmlDocument.PrepareStructure;
begin
  Assert(FState in [htdocInit, htdocStructurePrepared],
        'FState in [htdocInit, htdocStructurePrepared]');
  HTML.PrepareStructure;
  FState := htdocStructurePrepared;
end;

procedure ThtmlDocument.InitDocStyle(
          const ReaderStyle: String;
          var ExternalStyles: StringArray);
var
  Refs1, Refs2: StringArray;
begin
  Assert(FState in [htdocStructurePrepared, htdocPreparingStyle],
        'FState in [htdocStructurePrepared, htdocPreparingStyle]');
  FState := htdocPreparingStyle;
  // init main style
  Refs1 := HTML.Head.StyleRefs;
  Refs2 := HTML.Body.StyleRefs;
  DynArrayAppendStringArray(Refs1, Refs2);
  FStyleSheet.InitStyle(ReaderStyle, HTML.Head.StyleText, Refs1);
  // get external style sources
  ExternalStyles := FStyleSheet.GetRequiredImports;
end;

procedure ThtmlDocument.SetExternalStyleState(const Source: String;
    const State: ThtmlcssRuleSetState; const StyleText: String);
begin
  Assert(FState = htdocPreparingStyle, 'FState = htdocPreparingStyle');
  FStyleSheet.SetImportedStyleState(Source, State, StyleText);
end;

procedure ThtmlDocument.SetContentStyle;
var StyleInfo : ThtmlcssStyleProperties;
begin
  Assert(FState in [htdocStructurePrepared, htdocPreparingStyle, htdocStylePrepared, htdocRefactoredForLayout],
        'FState in [htdocStructurePrepared, htdocPreparingStyle, htdocStylePrepared, htdocRefactoredForLayout]');
  // init information required by style sheet selectors
  InitStyleElementInfo(FStyleSheet, nil);
  // set default style information for document
  InitDefaultStyleProperties(StyleInfo);
  // init actual style information for child objects
  InitStyleInfo(FStyleSheet, StyleInfo);
  // update document state
  FState := htdocStylePrepared;
end;

function ThtmlDocument.ReadyForLayout: Boolean;
begin
  Result := FState in [htdocStructurePrepared, htdocPreparingStyle,
                       htdocStylePrepared, htdocRefactoredForLayout];
end;

procedure ThtmlDocument.RefactorForLayout;
begin
  Assert(FState in [htdocStructurePrepared, htdocPreparingStyle, htdocStylePrepared],
        'FState in [htdocStructurePrepared, htdocPreparingStyle, htdocStylePrepared]');
  // refactor children
  Refactor([reopRefactorForLayout]);
  // update document state
  FState := htdocRefactoredForLayout;
end;



end.

