{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals 5.00 - HTML Parser                          }
{   File name:        flcHTMLDocElements.pas                                   }
{   File version:     5.07                                                     }
{   Description:      HTML DOM elements                                        }
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
{   2002/10/27  1.01  Classes for all HTML 4 element objects.                  }
{   2002/11/22  1.02  Style sheet support.                                     }
{   2002/12/03  1.03  Style sheet changes.                                     }
{   2002/12/07  1.04  Small additions.                                         }
{   2002/12/08  1.05  Created unit cHTMLDocElements.                           }
{   2015/04/11  1.06  UnicodeString changes.                                   }
{   2019/02/22  5.07  Revise for Fundamentals 5.                               }
{                                                                              }
{******************************************************************************}

{$INCLUDE flcHTML.inc}

unit flcHTMLDocElements;

interface

uses
  { System }
  UITypes,

  { Fundamentals }
  flcStdTypes,
  
  { HTML }
  flcHTMLElements,
  flcHTMLProperties,
  flcHTMLStyleTypes,
  flcHTMLStyleProperties,
  flcHTMLStyleSheet,
  flcHTMLDocBase;



{                                                                              }
{ Global element factory                                                       }
{                                                                              }
function  htmlElementClass(const TagID: ThtmlTagID): ThtmlElementClass; overload;
function  htmlElementClass(const Name: String): ThtmlElementClass; overload;
function  htmlCreateElement(const TagID: ThtmlTagID; const Name: String): AhtmlElement;



{ <!ENTITY % coreattrs                                                         }
{  "id          ID             #IMPLIED  -- document-wide unique id --         }
{   class       CDATA          #IMPLIED  -- space-separated list of classes -- }
{   style       %StyleSheet;   #IMPLIED  -- associated style info --           }
{   title       %Text;         #IMPLIED  -- advisory title --"                 }
type
  AhtmlElementInclCoreAttrs = class(AhtmlKnownElement)
  private
    function  GetID: String;
    procedure SetID(const Value: String);
    function  GetClasses: String;
    procedure SetClasses(const Value: String);
    function  GetStyleAttrStr: String;
    procedure SetStyleAttrStr(const Value: String);
    function  GetTitle: String;
    procedure SetTitle(const Value: String);

  public
    property  ID: String read GetID write SetID;
    property  Classes: String read GetClasses write SetClasses;
    property  StyleAttrStr: String read GetStyleAttrStr write SetStyleAttrStr;
    property  Title: String read GetTitle write SetTitle;
  end;

{ <!ENTITY % i18n                                                                   }
{  "lang        %LanguageCode; #IMPLIED  -- language code --                        }
{   dir         (ltr|rtl)      #IMPLIED  -- direction for weak/neutral text --"     }
type
  AhtmlElementIncli18n = class(AhtmlKnownElement)
  private
    function  GetLang: String;
    procedure SetLang(const Value: String);
    function  GetDir: ThtmlTextDir;
    procedure SetDir(const Value: ThtmlTextDir);

  public
    property  Lang: String read GetLang write SetLang;
    property  Dir: ThtmlTextDir read GetDir write SetDir;
  end;

  AhtmlElementInclCoreAttrsAndi18n = class(AhtmlElementInclCoreAttrs)
  private
    function  GetLang: String;
    procedure SetLang(const Value: String);
    function  GetDir: ThtmlTextDir;
    procedure SetDir(const Value: ThtmlTextDir);

  public
    property  Lang: String read GetLang write SetLang;
    property  Dir: ThtmlTextDir read GetDir write SetDir;
  end;

{ <!ENTITY % attrs "%coreattrs; %i18n; %events;">                                   }
{ <!ENTITY % events                                                                 }
{  "onclick     %Script;       #IMPLIED  -- a pointer button was clicked --         }
{   ondblclick  %Script;       #IMPLIED  -- a pointer button was double clicked--   }
{   onmousedown %Script;       #IMPLIED  -- a pointer button was pressed down --    }
{   onmouseup   %Script;       #IMPLIED  -- a pointer button was released --        }
{   onmouseover %Script;       #IMPLIED  -- a pointer was moved onto --             }
{   onmousemove %Script;       #IMPLIED  -- a pointer was moved within --           }
{   onmouseout  %Script;       #IMPLIED  -- a pointer was moved away --             }
{   onkeypress  %Script;       #IMPLIED  -- a key was pressed and released --       }
{   onkeydown   %Script;       #IMPLIED  -- a key was pressed down --               }
{   onkeyup     %Script;       #IMPLIED  -- a key was released --"                  }
type
  AhtmlElementInclAttrs = class(AhtmlElementInclCoreAttrsAndi18n)
  private
    function  GetOnClick: String;
    procedure SetOnClick(const Value: String);
    function  GetOnDblClick: String;
    procedure SetOnDblClick(const Value: String);
    function  GetOnMouseDown: String;
    procedure SetOnMouseDown(const Value: String);
    function  GetOnMouseUp: String;
    procedure SetOnMouseUp(const Value: String);
    function  GetOnMouseOver: String;
    procedure SetOnMouseOver(const Value: String);
    function  GetOnMouseMove: String;
    procedure SetOnMouseMove(const Value: String);
    function  GetOnMouseOut: String;
    procedure SetOnMouseOut(const Value: String);
    function  GetOnKeyPress: String;
    procedure SetOnKeyPress(const Value: String);
    function  GetOnKeyDown: String;
    procedure SetOnKeyDown(const Value: String);
    function  GetOnKeyUp: String;
    procedure SetOnKeyUp(const Value: String);

  public
    property  OnClick: String read GetOnClick write SetOnClick;
    property  OnDblClick: String read GetOnDblClick write SetOnDblClick;
    property  OnMouseDown: String read GetOnMouseDown write SetOnMouseDown;
    property  OnMouseUp: String read GetOnMouseUp write SetOnMouseUp;
    property  OnMouseOver: String read GetOnMouseOver write SetOnMouseOver;
    property  OnMouseMove: String read GetOnMouseMove write SetOnMouseMove;
    property  OnMouseOut: String read GetOnMouseOut write SetOnMouseOut;
    property  OnKeyPress: String read GetOnKeyPress write SetOnKeyPress;
    property  OnKeyDown: String read GetOnKeyDown write SetOnKeyDown;
    property  OnKeyUp: String read GetOnKeyUp write SetOnKeyUp;
  end;



{ ============================================================================ }
{ TEXT MARKUP                                                                  }
{ ============================================================================ }

{ <!ENTITY % fontstyle                                                             }
{  "TT | I | B | U | S | STRIKE | BIG | SMALL">                                    }
{ <!ELEMENT (%fontstyle;|%phrase;) - - (%inline;)*>                                }
{ <!ATTLIST (%fontstyle;|%phrase;)                                                 }
{   %attrs;                              -- %coreattrs, %i18n, %events --          }
type
  ThtmlFontStyle = (hfsTeleType, hfsItalic, hfsBold, hfsUnderline, hfsStrikeThrough,
      hfsBig, hfsSmall);
  ThtmlFontStyles = Set of ThtmlFontStyle;
  AhtmlFontStyle = class(AhtmlElementInclAttrs)
  protected
    FFontStyles: ThtmlFontStyles;

  public
    constructor Create(const TagID: ThtmlTagID; const FontStyles: ThtmlFontStyles); overload;

    property  FontStyles: ThtmlFontStyles read FFontStyles;
  end;

  ThtmlTT = class(AhtmlFontStyle)
  public
    constructor Create; override;
  end;

  ThtmlI = class(AhtmlFontStyle)
  public
    constructor Create; override;
  end;

  ThtmlB = class(AhtmlFontStyle)
  public
    constructor Create; override;
  end;

  ThtmlU = class(AhtmlFontStyle)
  public
    constructor Create; override;
  end;

  ThtmlS = class(AhtmlFontStyle)
  public
    constructor Create; override;
  end;

  ThtmlSTRIKE = class(AhtmlFontStyle)
  public
    constructor Create; override;
  end;

  ThtmlBIG = class(AhtmlFontStyle)
  public
    constructor Create; override;
  end;

  ThtmlSMALL = class(AhtmlFontStyle)
  public
    constructor Create; override;
  end;

{ <!ENTITY % phrase "EM | STRONG | DFN | CODE |                                    }
{                    SAMP | KBD | VAR | CITE | ABBR | ACRONYM" >                   }
type
  ThtmlPhraseElement = (peEmphasise, peStrong, peDefiningInstance, peCode,
      peSample, peKeyboard, peVariable, peCitation, peAbbreviation,
      peAcronym);
  AhtmlPhraseElement = class(AhtmlElementInclAttrs)
  protected
    FPhraseElement: ThtmlPhraseElement;

  public
    constructor Create(const TagID: ThtmlTagID;
                const PhraseElement: ThtmlPhraseElement); overload;

    property  PhraseElement: ThtmlPhraseElement read FPhraseElement;
  end;

  ThtmlEM = class(AhtmlPhraseElement)
  public
    constructor Create; override;
  end;

  ThtmlSTRONG = class(AhtmlPhraseElement)
  public
    constructor Create; override;
  end;

  ThtmlDFN = class(AhtmlPhraseElement)
  public
    constructor Create; override;
  end;

  ThtmlCODE = class(AhtmlPhraseElement)
  public
    constructor Create; override;
  end;

  ThtmlSAMP = class(AhtmlPhraseElement)
  public
    constructor Create; override;
  end;

  ThtmlKBD = class(AhtmlPhraseElement)
  public
    constructor Create; override;
  end;

  ThtmlVAR = class(AhtmlPhraseElement)
  public
    constructor Create; override;
  end;

  ThtmlCITE = class(AhtmlPhraseElement)
  public
    constructor Create; override;
  end;

  ThtmlABBR = class(AhtmlPhraseElement)
  public
    constructor Create; override;
  end;

  ThtmlACRONYM = class(AhtmlPhraseElement)
  public
    constructor Create; override;
  end;

{ <!ELEMENT (SUB|SUP) - - (%inline;)*    -- subscript, superscript -->             }
{ <!ATTLIST (SUB|SUP)                                                              }
{   %attrs;                              -- %coreattrs, %i18n, %events --          }
type
  ThtmlSUB = class(AhtmlElementInclAttrs)
  public
    constructor Create; override;
  end;

  ThtmlSUP = class(AhtmlElementInclAttrs)
  public
    constructor Create; override;
  end;

{ <!ELEMENT SPAN - - (%inline;)*         -- generic language/style container -->   }
{ <!ATTLIST SPAN                                                                   }
{   %attrs;                              -- %coreattrs, %i18n, %events --          }
{   %reserved;                   -- reserved for possible future use --            }
type
  ThtmlSPAN = class(AhtmlElementInclAttrs)
  public
    constructor Create; override;
  end;

{ <!ELEMENT BDO - - (%inline;)*          -- I18N BiDi over-ride -->                }
{ <!ATTLIST BDO                                                                    }
{   %coreattrs;                          -- id, class, style, title --             }
{   lang        %LanguageCode; #IMPLIED  -- language code --                       }
{   dir         (ltr|rtl)      #REQUIRED -- directionality --                      }
type
  ThtmlBDO = class(AhtmlElementInclCoreAttrsAndi18n)
  public
    constructor Create; override;
  end;

{ <!ATTLIST BASEFONT                                                                }
{   id          ID             #IMPLIED  -- document-wide unique id --              }
{   size        CDATA          #REQUIRED -- base font size for FONT elements --     }
{   color       %Color;        #IMPLIED  -- text color --                           }
{   face        CDATA          #IMPLIED  -- comma-separated list of font names --   }
type
  ThtmlBASEFONT = class(AhtmlKnownElement)
  private
    function  GetID: String;
    procedure SetID(const Value: String);
    function  GetSize: String;
    procedure SetSize(const Value: String);
    function  GetColor: String;
    procedure SetColor(const Value: String);
    function  GetColorDelphi: TColor;
    procedure SetColorDelphi(const Value: TColor);
    function  GetFace: String;
    procedure SetFace(const Value: String);

  public
    constructor Create; override;

    property  ID: String read GetID write SetID;
    property  Size: String read GetSize write SetSize;
    property  Color: String read GetColor write SetColor;
    property  ColorDelphi: TColor read GetColorDelphi write SetColorDelphi;
    property  Face: String read GetFace write SetFace;
  end;

{ <!ELEMENT FONT - - (%inline;)*         -- local change to font -->              }
{ <!ATTLIST FONT                                                                  }
{   %coreattrs;                          -- id, class, style, title --            }
{   %i18n;                               -- lang, dir --                          }
{   size        CDATA          #IMPLIED  -- [+|-]nn e.g. size="+1", size="4" --   }
{   color       %Color;        #IMPLIED  -- text color --                         }
{   face        CDATA          #IMPLIED  -- comma-separated list of font names -- }
type
  ThtmlFONT = class(AhtmlElementInclCoreAttrsAndi18n)
  private
    function  GetSize: String;
    procedure SetSize(const Value: String);
    function  GetSizeValue: ThtmlFontSize;
    function  GetColor: String;
    procedure SetColor(const Value: String);
    function  GetColorRGB: LongWord;
    procedure SetColorRGB(const Value: LongWord);
    function  GetColorDelphi: TColor;
    procedure SetColorDelphi(const Value: TColor);
    function  GetFaceStr: String;
    procedure SetFaceStr(const Value: String);

  protected
    procedure ApplyHTMLStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
              const ParentStyle: ThtmlcssStyleProperties); override;

  public
    constructor Create; override;

    property  Size: String read GetSize write SetSize;
    property  SizeValue: ThtmlFontSize read GetSizeValue;
    property  Color: String read GetColor write SetColor;
    property  ColorRGB: LongWord read GetColorRGB write SetColorRGB;
    property  ColorDelphi: TColor read GetColorDelphi write SetColorDelphi;
    property  FaceStr: String read GetFaceStr write SetFaceStr;
  end;

{ <!ELEMENT BR - O EMPTY                   -- forced line break -->            }
{ <!ATTLIST BR                                                                 }
{   %coreattrs;                            -- id, class, style, title --       }
{   clear       (left|all|right|none) none -- control of text flow --          }
type
  ThtmlBR = class(AhtmlElementInclCoreAttrs)
  private
    function  GetClear: ThtmlClearType;
    procedure SetClear(const Value: ThtmlClearType);

  protected
    function  GetHTMLText: String; override;

  public
    constructor Create; override;

    property  Clear: ThtmlClearType read GetClear write SetClear;
  end;



{ ============================================================================ }
{ DOCUMENT BODY                                                                }
{ ============================================================================ }

{ <!ENTITY % bodycolors "                                                          }
{   bgcolor     %Color;        #IMPLIED  -- document background color --           }
{   text        %Color;        #IMPLIED  -- document text color --                 }
{   link        %Color;        #IMPLIED  -- color of links --                      }
{   vlink       %Color;        #IMPLIED  -- color of visited links --              }
{   alink       %Color;        #IMPLIED  -- color of selected links --             }
{ <!ELEMENT BODY O O (%flow;)* +(INS|DEL) -- document body -->                     }
{   <!ATTLIST BODY                                                                 }
{    %attrs;                              -- %coreattrs, %i18n, %events --         }
{    onload          %Script;   #IMPLIED  -- the document has been loaded --       }
{    onunload        %Script;   #IMPLIED  -- the document has been removed --      }
{    background      %URI;      #IMPLIED  -- texture tile for document             }
{                                            background --                         }
{    %bodycolors;                         -- bgcolor, text, link, vlink, alink --  }
type
  ThtmlBODY = class(AhtmlElementInclAttrs)
  private
    function  GetOnLoad: String;
    procedure SetOnLoad(const Value: String);
    function  GetOnUnload: String;
    procedure SetOnUnload(const Value: String);
    function  GetBackground: String;
    procedure SetBackground(const Value: String);
    function  GetBackColor: String;
    procedure SetBackColor(const Value: String);
    function  GetBackColorDelphi: TColor;
    function  GetTextColor: String;
    procedure SetTextColor(const Value: String);
    function  GetLinkColor: String;
    procedure SetLinkColor(const Value: String);
    function  GetVisitedLinkColor: String;
    procedure SetVisitedLinkColor(const Value: String);
    function  GetSelectedLinkColor: String;
    procedure SetSelectedLinkColor(const Value: String);

  protected
    procedure ApplyHTMLStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
              const ParentStyle: ThtmlcssStyleProperties); override;

  public
    constructor Create; override;

    property  OnLoad: String read GetOnLoad write SetOnLoad;
    property  OnUnload: String read GetOnUnload write SetOnUnload;
    property  Background: String read GetBackground write SetBackground;
    property  BackColor: String read GetBackColor write SetBackColor;
    property  BackColorDelphi: TColor read GetBackColorDelphi;
    property  TextColor: String read GetTextColor write SetTextColor;
    property  LinkColor: String read GetLinkColor write SetLinkColor;
    property  VisitedLinkColor: String read GetVisitedLinkColor write SetVisitedLinkColor;
    property  SelectedLinkColor: String read GetSelectedLinkColor write SetSelectedLinkColor;

    procedure PrepareStructure;
    function  StyleRefs: StringArray;
  end;

{ <!ELEMENT ADDRESS - - ((%inline;)|P)*  -- information on author -->              }
{ <!ATTLIST ADDRESS                                                                }
{   %attrs;                              -- %coreattrs, %i18n, %events --          }
type
  ThtmlADDRESS = class(AhtmlElementInclAttrs)
  public
    constructor Create; override;
  end;

{ <!ENTITY % align "align (left|center|right|justify)  #IMPLIED"                   }
{                    -- default is left for ltr paragraphs, right for rtl --       }
type
  AhtmlElementInclAttrsAndAlign = class(AhtmlElementInclAttrs)
  private
    function  GetAlign: ThtmlTextAlignType;
    procedure SetAlign(const Value: ThtmlTextAlignType);

  public
    property  Align: ThtmlTextAlignType read GetAlign write SetAlign;
  end;

{ <!ELEMENT DIV - - (%flow;)*            -- generic language/style container -->    }
{ <!ATTLIST DIV                                                                     }
{   %attrs;                              -- %coreattrs, %i18n, %events --           }
{   %align;                              -- align, text alignment --                }
{   %reserved;                           -- reserved for possible future use --     }
type
  ThtmlDIV = class(AhtmlElementInclAttrsAndAlign)
  public
    constructor Create; override;
  end;

{ <!ELEMENT CENTER - - (%flow;)*         -- shorthand for DIV align=center -->      }
{ <!ATTLIST CENTER                                                                  }
{   %attrs;                              -- %coreattrs, %i18n, %events --           }
type
  ThtmlCENTER = class(AhtmlElementInclAttrs)
  public
    constructor Create; override;
  end;



{ ============================================================================ }
{ ANCHOR ELEMENT                                                               }
{ ============================================================================ }

{ <!ELEMENT A - - (%inline;)* -(A)       -- anchor -->                             }
{ <!ATTLIST A                                                                      }
{   %attrs;                              -- %coreattrs, %i18n, %events --          }
{   charset     %Charset;      #IMPLIED  -- char encoding of linked resource --    }
{   type        %ContentType;  #IMPLIED  -- advisory content type --               }
{   name        CDATA          #IMPLIED  -- named link end --                      }
{   href        %URI;          #IMPLIED  -- URI for linked resource --             }
{   hreflang    %LanguageCode; #IMPLIED  -- language code --                       }
{   target      %FrameTarget;  #IMPLIED  -- render in this frame --                }
{   rel         %LinkTypes;    #IMPLIED  -- forward link types --                  }
{   rev         %LinkTypes;    #IMPLIED  -- reverse link types --                  }
{   accesskey   %Character;    #IMPLIED  -- accessibility key character --         }
{   shape       %Shape;        rect      -- for use with client-side image maps -- }
{   coords      %Coords;       #IMPLIED  -- for use with client-side image maps -- }
{   tabindex    NUMBER         #IMPLIED  -- position in tabbing order --           }
{   onfocus     %Script;       #IMPLIED  -- the element got the focus --           }
{   onblur      %Script;       #IMPLIED  -- the element lost the focus --          }
type
  ThtmlA = class(AhtmlElementInclAttrs)
  private
    function  GetContentType: String;
    procedure SetContentType(const Value: String);
    function  GetNameAttr: String;
    procedure SetNameAttr(const Value: String);
    function  GetHRef: String;
    procedure SetHRef(const Value: String);
    function  GetTarget: String;
    procedure SetTarget(const Value: String);
    function  GetRel: String;
    procedure SetRel(const Value: String);
    function  GetRev: String;
    procedure SetRev(const Value: String);
    function  GetTabIndex: String;
    procedure SetTabIndex(const Value: String);
    function  GetOnFocus: String;
    procedure SetOnFocus(const Value: String);
    function  GetOnBlur: String;
    procedure SetOnBlur(const Value: String);

  protected
    FIsLink      : Boolean;
    FStateStyles : Array[ThtmlcssAnchorPseudoPropertyState] of ThtmlcssStyleProperties;

    procedure InitStyleElementInfo(const StyleSheet: ThtmlCSS;
              const ParentInfo: PhtmlcssElementInfo = nil); override;
    procedure InitStyleInfo(const StyleSheet: ThtmlCSS;
              const StyleInfo: ThtmlcssStyleProperties); override;

  public
    constructor Create; override;

    property  ContentType: String read GetContentType write SetContentType;
    property  Name: String read GetNameAttr write SetNameAttr;
    property  HRef: String read GetHRef write SetHRef;
    property  Target: String read GetTarget write SetTarget;
    property  Rel: String read GetRel write SetRel;
    property  Rev: String read GetRev write SetRev;
    property  TabIndex: String read GetTabIndex write SetTabIndex;
    property  OnFocus: String read GetOnFocus write SetOnFocus;
    property  OnBlur: String read GetOnBlur write SetOnBlur;
  end;



{ ============================================================================ }
{ CLIENT-SIDE IMAGE MAPS                                                       }
{ ============================================================================ }

{ <!ELEMENT MAP - - ((%block;) | AREA)+ -- client-side image map -->               }
{ <!ATTLIST MAP                                                                    }
{   %attrs;                              -- %coreattrs, %i18n, %events --          }
{   name        CDATA          #REQUIRED -- for reference by usemap --             }
type
  ThtmlMAP = class(AhtmlElementInclAttrs)
  private
    function  GetNameAttr: String;
    procedure SetNameAttr(const Value: String);

  public
    constructor Create; override;

    property  Name: String read GetNameAttr write SetNameAttr;
  end;

{ <!ELEMENT AREA - O EMPTY               -- client-side image map area -->         }
{ <!ATTLIST AREA                                                                   }
{   %attrs;                              -- %coreattrs, %i18n, %events --          }
{   shape       %Shape;        rect      -- controls interpretation of coords --   }
{   coords      %Coords;       #IMPLIED  -- comma-separated list of lengths --     }
{   href        %URI;          #IMPLIED  -- URI for linked resource --             }
{   target      %FrameTarget;  #IMPLIED  -- render in this frame --                }
{   nohref      (nohref)       #IMPLIED  -- this region has no action --           }
{   alt         %Text;         #REQUIRED -- short description --                   }
{   tabindex    NUMBER         #IMPLIED  -- position in tabbing order --           }
{   accesskey   %Character;    #IMPLIED  -- accessibility key character --         }
{   onfocus     %Script;       #IMPLIED  -- the element got the focus --           }
{   onblur      %Script;       #IMPLIED  -- the element lost the focus --          }
type
  ThtmlAREA = class(AhtmlElementInclAttrs)
  private
    function  GetShape: String;
    procedure SetShape(const Value: String);
    function  GetCoords: String;
    procedure SetCoords(const Value: String);
    function  GetHRef: String;
    procedure SetHRef(const Value: String);
    function  GetTarget: String;
    procedure SetTarget(const Value: String);
    function  GetNoHRef: Boolean;
    procedure SetNoHRef(const Value: Boolean);
    function  GetAlt: String;
    procedure SetAlt(const Value: String);
    function  GetTabIndex: String;
    procedure SetTabIndex(const Value: String);
    function  GetOnFocus: String;
    procedure SetOnFocus(const Value: String);
    function  GetOnBlur: String;
    procedure SetOnBlur(const Value: String);

  public
    constructor Create; override;

    property  Shape: String read GetShape write SetShape;
    property  Coords: String read GetCoords write SetCoords;
    property  HRef: String read GetHRef write SetHRef;
    property  Target: String read GetTarget write SetTarget;
    property  NoHRef: Boolean read GetNoHRef write SetNoHRef;
    property  Alt: String read GetAlt write SetAlt;
    property  TabIndex: String read GetTabIndex write SetTabIndex;
    property  OnFocus: String read GetOnFocus write SetOnFocus;
    property  OnBlur: String read GetOnBlur write SetOnBlur;
  end;



{ ============================================================================ }
{ LINK ELEMENT                                                                 }
{ ============================================================================ }

{ <!ELEMENT LINK - O EMPTY               -- a media-independent link -->           }
{ <!ATTLIST LINK                                                                   }
{   %attrs;                              -- %coreattrs, %i18n, %events --          }
{   charset     %Charset;      #IMPLIED  -- char encoding of linked resource --    }
{   href        %URI;          #IMPLIED  -- URI for linked resource --             }
{   hreflang    %LanguageCode; #IMPLIED  -- language code --                       }
{   type        %ContentType;  #IMPLIED  -- advisory content type --               }
{   rel         %LinkTypes;    #IMPLIED  -- forward link types --                  }
{   rev         %LinkTypes;    #IMPLIED  -- reverse link types --                  }
{   media       %MediaDesc;    #IMPLIED  -- for rendering on these media --        }
{   target      %FrameTarget;  #IMPLIED  -- render in this frame --                }
type
  ThtmlLINK = class(AhtmlElementInclAttrs)
  private
    function  GetHRef: String;
    procedure SetHRef(const Value: String);
    function  GetContentType: String;
    procedure SetContentType(const Value: String);
    function  GetRel: String;
    procedure SetRel(const Value: String);
    function  GetRev: String;
    procedure SetRev(const Value: String);
    function  GetMedia: String;
    procedure SetMedia(const Value: String);
    function  GetTarget: String;
    procedure SetTarget(const Value: String);

  public
    constructor Create; override;

    property  HRef: String read GetHRef write SetHRef;
    property  ContentType: String read GetContentType write SetContentType;
    property  Rel: String read GetRel write SetRel;
    property  Rev: String read GetRev write SetRev;
    property  Media: String read GetMedia write SetMedia;
    property  Target: String read GetTarget write SetTarget;
  end;



{ ============================================================================ }
{ IMAGES                                                                       }
{ ============================================================================ }

{   %attrs;                              -- %coreattrs, %i18n, %events --          }
{   name        CDATA          #IMPLIED  -- name of image for scripting --         }
{   align       %IAlign;       #IMPLIED  -- vertical or horizontal alignment --    }
{   width       %Length;       #IMPLIED  -- override width --                      }
{   height      %Length;       #IMPLIED  -- override height --                     }
{   hspace      %Pixels;       #IMPLIED  -- horizontal gutter --                   }
{   vspace      %Pixels;       #IMPLIED  -- vertical gutter --                     }
{   usemap      %URI;          #IMPLIED  -- use client-side image map --           }
type
  AhtmlImageElement = class(AhtmlElementInclAttrs)
  protected
    function  GetNameAttr: String;
    procedure SetNameAttr(const Value: String);
    function  GetAlign: ThtmlIAlignType;
    procedure SetAlign(const Value: ThtmlIAlignType);
    function  GetWidthStr: String;
    procedure SetWidthStr(const Value: String);
    function  GetHeightStr: String;
    procedure SetHeightStr(const Value: String);
    function  GetWidth: ThtmlLength;
    function  GetHeight: ThtmlLength;
    function  GetHSpace: Integer;
    procedure SetHSpace(const Value: Integer);
    function  GetVSpace: Integer;
    procedure SetVSpace(const Value: Integer);
    function  GetUseMap: String;
    procedure SetUseMap(const Value: String);

    procedure ApplyHTMLStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
              const ParentStyle: ThtmlcssStyleProperties); override;

  public
    property  Name: String read GetNameAttr write SetNameAttr;
    property  Align: ThtmlIAlignType read GetAlign write SetAlign;
    property  WidthStr: String read GetWidthStr write SetWidthStr;
    property  HeightStr: String read GetHeightStr write SetHeightStr;
    property  Width: ThtmlLength read GetWidth;
    property  Height: ThtmlLength read GetHeight;
    property  HSpace: Integer read GetHSpace write SetHSpace;
    property  VSpace: Integer read GetVSpace write SetVSpace;
    property  UseMap: String read GetUseMap write SetUseMap;
  end;

{ <!ELEMENT IMG - O EMPTY                -- Embedded image -->                     }
{ <!ATTLIST IMG                                                                    }
{   %attrs;                              -- %coreattrs, %i18n, %events --          }
{   src         %URI;          #REQUIRED -- URI of image to embed --               }
{   alt         %Text;         #REQUIRED -- short description --                   }
{   longdesc    %URI;          #IMPLIED  -- link to long description               }
{                                           (complements alt) --                   }
{   name        CDATA          #IMPLIED  -- name of image for scripting --         }
{   height      %Length;       #IMPLIED  -- override height --                     }
{   width       %Length;       #IMPLIED  -- override width --                      }
{   usemap      %URI;          #IMPLIED  -- use client-side image map --           }
{   ismap       (ismap)        #IMPLIED  -- use server-side image map --           }
{   align       %IAlign;       #IMPLIED  -- vertical or horizontal alignment --    }
{   border      %Pixels;       #IMPLIED  -- link border width --                   }
{   hspace      %Pixels;       #IMPLIED  -- horizontal gutter --                   }
{   vspace      %Pixels;       #IMPLIED  -- vertical gutter --                     }
type
  ThtmlIMG = class(AhtmlImageElement)
  private
    function  GetSrc: String;
    procedure SetSrc(const Value: String);
    function  GetAlt: String;
    procedure SetAlt(const Value: String);
    function  GetLongDesc: String;
    procedure SetLongDesc(const Value: String);
    function  GetBorder: String;
    procedure SetBorder(const Value: String);
    function  GetIsMap: Boolean;
    procedure SetIsMap(const Value: Boolean);

  public
    constructor Create; override;

    property  Src: String read GetSrc write SetSrc;
    property  Alt: String read GetAlt write SetAlt;
    property  LongDesc: String read GetLongDesc write SetLongDesc;
    property  IsMap: Boolean read GetIsMap write SetIsMap;
    property  Border: String read GetBorder write SetBorder;
  end;



{ ============================================================================ }
{ EMBEDDED OBJECT                                                              }
{ ============================================================================ }

{ <!ELEMENT OBJECT - - (PARAM | %flow;)*                                           }
{  -- generic embedded object -->                                                  }
{ <!ATTLIST OBJECT                                                                 }
{   %attrs;                              -- %coreattrs, %i18n, %events --          }
{   declare     (declare)      #IMPLIED  -- declare but don't instantiate flag --  }
{   classid     %URI;          #IMPLIED  -- identifies an implementation --        }
{   codebase    %URI;          #IMPLIED  -- base URI for classid, data, archive--  }
{   data        %URI;          #IMPLIED  -- reference to object's data --          }
{   type        %ContentType;  #IMPLIED  -- content type for data --               }
{   codetype    %ContentType;  #IMPLIED  -- content type for code --               }
{   archive     CDATA          #IMPLIED  -- space-separated list of URIs --        }
{   standby     %Text;         #IMPLIED  -- message to show while loading --       }
{   height      %Length;       #IMPLIED  -- override height --                     }
{   width       %Length;       #IMPLIED  -- override width --                      }
{   usemap      %URI;          #IMPLIED  -- use client-side image map --           }
{   name        CDATA          #IMPLIED  -- submit as part of form --              }
{   tabindex    NUMBER         #IMPLIED  -- position in tabbing order --           }
{   align       %IAlign;       #IMPLIED  -- vertical or horizontal alignment --    }
{   border      %Pixels;       #IMPLIED  -- link border width --                   }
{   hspace      %Pixels;       #IMPLIED  -- horizontal gutter --                   }
{   vspace      %Pixels;       #IMPLIED  -- vertical gutter --                     }
{   %reserved;                           -- reserved for possible future use --    }
type
  ThtmlEmbeddedOBJECT = class(AhtmlImageElement)
  private
    function  GetDeclare: Boolean;
    procedure SetDeclare(const Value: Boolean);
    function  GetClassID: String;
    procedure SetClassID(const Value: String);
    function  GetData: String;
    procedure SetData(const Value: String);
    function  GetCodeBase: String;
    procedure SetCodeBase(const Value: String);
    function  GetContentType: String;
    procedure SetContentType(const Value: String);
    function  GetCodeType: String;
    procedure SetCodeType(const Value: String);
    function  GetStandBy: String;
    procedure SetStandBy(const Value: String);
    function  GetBorder: Integer;
    procedure SetBorder(const Value: Integer);
    function  GetTabIndex: String;
    procedure SetTabIndex(const Value: String);

  public
    constructor Create; override;

    function  CreateItem(const ID: Integer; const Name: String): AhtmlObject; override;
    property  Declare: Boolean read GetDeclare write SetDeclare;
    property  ClassID: String read GetClassID write SetClassID;
    property  CodeBase: String read GetCodeBase write SetCodeBase;
    property  Data: String read GetData write SetData;
    property  ContentType: String read GetContentType write SetContentType;
    property  CodeType: String read GetCodeType write SetCodeType;
    property  StandBy: String read GetStandBy write SetStandBy;
    property  TabIndex: String read GetTabIndex write SetTabIndex;
    property  Border: Integer read GetBorder write SetBorder;
  end;

{ <!ELEMENT PARAM - O EMPTY              -- named property value -->               }
{ <!ATTLIST PARAM                                                                  }
{   id          ID             #IMPLIED  -- document-wide unique id --             }
{   name        CDATA          #REQUIRED -- property name --                       }
{   value       CDATA          #IMPLIED  -- property value --                      }
{   valuetype   (DATA|REF|OBJECT) DATA   -- How to interpret value --              }
{   type        %ContentType;  #IMPLIED  -- content type for value                 }
{                                           when valuetype=ref --                  }
type
  ThtmlEmbeddedObjectPARAM = class(AhtmlKnownElement)
  private
    function  GetID: String;
    procedure SetID(const Value: String);
    function  GetNameAttr: String;
    procedure SetNameAttr(const Value: String);
    function  GetValue: String;
    procedure SetValue(const Value: String);
    function  GetValueType: String;
    procedure SetValueType(const Value: String);
    function  GetContentType: String;
    procedure SetContentType(const Value: String);

  public
    constructor Create; override;

    property  ID: String read GetID write SetID;
    property  Name: String read GetNameAttr write SetNameAttr;
    property  Value: String read GetValue write SetValue;
    property  ValueType: String read GetValueType write SetValueType;
    property  ContentType: String read GetContentType write SetContentType;
  end;



{ ============================================================================ }
{ APPLET                                                                       }
{ ============================================================================ }

{ <!ELEMENT APPLET - - (PARAM | %flow;)* -- Java applet -->                        }
{ <!ATTLIST APPLET                                                                 }
{   %coreattrs;                          -- id, class, style, title --             }
{   codebase    %URI;          #IMPLIED  -- optional base URI for applet --        }
{   archive     CDATA          #IMPLIED  -- comma-separated archive list --        }
{   code        CDATA          #IMPLIED  -- applet class file --                   }
{   object      CDATA          #IMPLIED  -- serialized applet file --              }
{   alt         %Text;         #IMPLIED  -- short description --                   }
{   name        CDATA          #IMPLIED  -- allows applets to find each other --   }
{   width       %Length;       #REQUIRED -- initial width --                       }
{   height      %Length;       #REQUIRED -- initial height --                      }
{   align       %IAlign;       #IMPLIED  -- vertical or horizontal alignment --    }
{   hspace      %Pixels;       #IMPLIED  -- horizontal gutter --                   }
{   vspace      %Pixels;       #IMPLIED  -- vertical gutter --                     }
type
  ThtmlAPPLET = class(AhtmlElementInclCoreAttrs)
  private
    function  GetCodeBase: String;
    procedure SetCodeBase(const Value: String);
    function  GetCode: String;
    procedure SetCode(const Value: String);
    function  GetSerObject: String;
    procedure SetSerObject(const Value: String);
    function  GetAlt: String;
    procedure SetAlt(const Value: String);
    function  GetNameAttr: String;
    procedure SetNameAttr(const Value: String);
    function  GetWidth: String;
    procedure SetWidth(const Value: String);
    function  GetHeight: String;
    procedure SetHeight(const Value: String);
    function  GetAlign: ThtmlIAlignType;
    procedure SetAlign(const Value: ThtmlIAlignType);
    function  GetHSpace: Integer;
    procedure SetHSpace(const Value: Integer);
    function  GetVSpace: Integer;
    procedure SetVSpace(const Value: Integer);

  public
    constructor Create; override;

    property  CodeBase: String read GetCodeBase write SetCodeBase;
    property  Code: String read GetCode write SetCode;
    property  SerObject: String read GetSerObject write SetSerObject;
    property  Alt: String read GetAlt write SetAlt;
    property  Name: String read GetNameAttr write SetNameAttr;
    property  Width: String read GetWidth write SetWidth;
    property  Height: String read GetHeight write SetHeight;
    property  Align: ThtmlIAlignType read GetAlign write SetAlign;
    property  HSpace: Integer read GetHSpace write SetHSpace;
    property  VSpace: Integer read GetVSpace write SetVSpace;
  end;



{ ============================================================================ }
{ HORISONTAL RULE                                                              }
{ ============================================================================ }

{ <!ELEMENT HR - O EMPTY -- horizontal rule -->                                    }
{ <!ATTLIST HR                                                                     }
{   %attrs;                              -- %coreattrs, %i18n, %events --          }
{   align       (left|center|right) #IMPLIED                                       }
{   noshade     (noshade)      #IMPLIED                                            }
{   size        %Pixels;       #IMPLIED                                            }
{   width       %Length;       #IMPLIED                                            }
type
  ThtmlHR = class(AhtmlElementInclAttrs)
  private
    function  GetAlign: String;
    procedure SetAlign(const Value: String);
    function  GetNoShade: Boolean;
    procedure SetNoShade(const Value: Boolean);
    function  GetSize: Integer;
    procedure SetSize(const Value: Integer);
    function  GetWidth: String;
    procedure SetWidth(const Value: String);

  public
    constructor Create; override;

    property  Align: String read GetAlign write SetAlign;
    property  NoShade: Boolean read GetNoShade write SetNoShade;
    property  Size: Integer read GetSize write SetSize;
    property  Width: String read GetWidth write SetWidth;
  end;



{ ============================================================================ }
{ PARAGRAPHS                                                                   }
{ ============================================================================ }

{ <!ELEMENT P - O (%inline;)*            -- paragraph -->                          }
{ <!ATTLIST P                                                                      }
{   %attrs;                              -- %coreattrs, %i18n, %events --          }
{   %align;                              -- align, text alignment --               }
type
  ThtmlP = class(AhtmlElementInclAttrsAndAlign)
  public
    constructor Create; override;
  end;



{ ============================================================================ }
{ HEADINGS                                                                     }
{ ============================================================================ }

{ <!ELEMENT (%heading;)  - - (%inline;)* -- heading -->                            }
{ <!ATTLIST (%heading;)                                                            }
{   %attrs;                              -- %coreattrs, %i18n, %events --          }
{   %align;                              -- align, text alignment --               }
type
  AhtmlHeadingTag = class(AhtmlElementInclAttrsAndAlign)
  public
    constructor Create(const TagID: ThtmlTagID); overload;
  end;

  ThtmlH1 = class(AhtmlHeadingTag)
  public
    constructor Create; override;
  end;

  ThtmlH2 = class(AhtmlHeadingTag)
  public
    constructor Create; override;
  end;

  ThtmlH3 = class(AhtmlHeadingTag)
  public
    constructor Create; override;
  end;

  ThtmlH4 = class(AhtmlHeadingTag)
  public
    constructor Create; override;
  end;

  ThtmlH5 = class(AhtmlHeadingTag)
  public
    constructor Create; override;
  end;

  ThtmlH6 = class(AhtmlHeadingTag)
  public
    constructor Create; override;
  end;



{ ============================================================================ }
{ PREFORMATTED TEXT                                                            }
{ ============================================================================ }

{ <!ENTITY % pre.exclusion "IMG|OBJECT|APPLET|BIG|SMALL|SUB|SUP|FONT|BASEFONT">    }
{ <!ELEMENT PRE - - (%inline;)* -(%pre.exclusion;) -- preformatted text -->        }
{ <!ATTLIST PRE                                                                    }
{   %attrs;                              -- %coreattrs, %i18n, %events --          }
{   width       NUMBER         #IMPLIED                                            }
type
  ThtmlPRE = class(AhtmlElementInclAttrs)
  protected
    function  GetWidth: Integer;
    procedure SetWidth(const Value: Integer);

    procedure ApplyHTMLStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
              const ParentStyle: ThtmlcssStyleProperties); override;

  public
    constructor Create; override;

    property  Width: Integer read GetWidth write SetWidth;
  end;



{ ============================================================================ }
{ QUOTES                                                                       }
{ ============================================================================ }

{ <!ELEMENT Q - - (%inline;)*            -- short inline quotation -->             }
{ <!ATTLIST Q                                                                      }
{   %attrs;                              -- %coreattrs, %i18n, %events --          }
{   cite        %URI;          #IMPLIED  -- URI for source document or msg --      }
type
  ThtmlQ = class(AhtmlElementInclAttrs)
  private
    function  GetCite: String;
    procedure SetCite(const Value: String);

  public
    constructor Create; override;

    property  Cite: String read GetCite write SetCite;
  end;

{ <!ELEMENT BLOCKQUOTE - - (%flow;)*     -- long quotation -->                     }
{ <!ATTLIST BLOCKQUOTE                                                             }
{   %attrs;                              -- %coreattrs, %i18n, %events --          }
{   cite        %URI;          #IMPLIED  -- URI for source document or msg --      }
type
  ThtmlBLOCKQUOTE = class(AhtmlElementInclAttrs)
  private
    function  GetCite: String;
    procedure SetCite(const Value: String);

  public
    constructor Create; override;

    property  Cite: String read GetCite write SetCite;
  end;



{ ============================================================================ }
{ INSERTED/DELETED TEXT                                                        }
{ ============================================================================ }

{ <!ELEMENT (INS|DEL) - - (%flow;)*      -- inserted text, deleted text -->        }
{ <!ATTLIST (INS|DEL)                                                              }
{   %attrs;                              -- %coreattrs, %i18n, %events --          }
{   cite        %URI;          #IMPLIED  -- info on reason for change --           }
{   datetime    %Datetime;     #IMPLIED  -- date and time of change --             }
type
  AhtmlVersionElement = class(AhtmlElementInclAttrs)
  private
    function  GetCite: String;
    procedure SetCite(const Value: String);
    function  GetDateTime: String;
    procedure SetDateTime(const Value: String);

  public
    property  Cite: String read GetCite write SetCite;
    property  DateTime: String read GetDateTime write SetDateTime;
  end;

  ThtmlINS = class(AhtmlVersionElement)
  public
    constructor Create; override;
  end;

  ThtmlDEL = class(AhtmlVersionElement)
  public
    constructor Create; override;
  end;



{ ============================================================================ }
{ LISTS                                                                        }
{ ============================================================================ }

{ <!ELEMENT DL - - (DT|DD)+              -- definition list -->                    }
{ <!ATTLIST DL                                                                     }
{   %attrs;                              -- %coreattrs, %i18n, %events --          }
{   compact     (compact)      #IMPLIED  -- reduced interitem spacing --           }
{ <!ELEMENT DT - O (%inline;)*           -- definition term -->                    }
{ <!ELEMENT DD - O (%flow;)*             -- definition description -->             }
{ <!ATTLIST (DT|DD)                                                                }
{   %attrs;                              -- %coreattrs, %i18n, %events --          }
type
  ThtmlDL = class(AhtmlElementInclAttrs)
  private
    function  GetCompact: Boolean;
    procedure SetCompact(const Value: Boolean);

  public
    constructor Create; override;

    function  CreateItem(const ID: Integer; const Name: String): AhtmlObject; override;
    property  Compact: Boolean read GetCompact write SetCompact;
  end;

  ThtmlDT = class(AhtmlElementInclAttrs)
  public
    constructor Create; override;
  end;

  ThtmlDD = class(AhtmlElementInclAttrs)
  public
    constructor Create; override;
  end;

{ AhtmlListItemContainer                                                           }
{   %attrs;                              -- %coreattrs, %i18n, %events --          }
{   compact     (compact)      #IMPLIED  -- reduced interitem spacing --           }
type
  AhtmlListItemContainer = class(AhtmlElementInclAttrs)
  private
    function  GetCompact: Boolean;
    procedure SetCompact(const Value: Boolean);

  public
    constructor Create(const TagID: ThtmlTagID); overload;

    function  CreateItem(const ID: Integer; const Name: String): AhtmlObject; override;
    property  Compact: Boolean read GetCompact write SetCompact;
  end;

{ <!ENTITY % OLStyle "CDATA"      -- constrained to: "(1|a|A|i|I)" -->             }
{ <!ELEMENT OL - - (LI)+                 -- ordered list -->                       }
{ <!ATTLIST OL                                                                     }
{   %attrs;                              -- %coreattrs, %i18n, %events --          }
{   type        %OLStyle;      #IMPLIED  -- numbering style --                     }
{   compact     (compact)      #IMPLIED  -- reduced interitem spacing --           }
{   start       NUMBER         #IMPLIED  -- starting sequence number --            }
type
  ThtmlOL = class(AhtmlListItemContainer)
  private
    function  GetListType: String;
    procedure SetListType(const Value: String);
    function  GetStart: Integer;
    procedure SetStart(const Value: Integer);

  public
    constructor Create; override;

    property  ListType: String read GetListType write SetListType;
    property  Start: Integer read GetStart write SetStart;
  end;

{ <!ENTITY % ULStyle "(disc|square|circle)">                                       }
{ <!ELEMENT UL - - (LI)+                 -- unordered list -->                     }
{ <!ATTLIST UL                                                                     }
{   %attrs;                              -- %coreattrs, %i18n, %events --          }
{   type        %ULStyle;      #IMPLIED  -- bullet style --                        }
{   compact     (compact)      #IMPLIED  -- reduced interitem spacing --           }
type
  ThtmlUL = class(AhtmlListItemContainer)
  private
    function  GetListType: String;
    procedure SetListType(const Value: String);

  public
    constructor Create; override;

    property  ListType: String read GetListType write SetListType;
  end;

{ <!ELEMENT (DIR|MENU) - - (LI)+ -(%block;) -- directory list, menu list -->       }
{ <!ATTLIST DIR                                                                    }
{   %attrs;                              -- %coreattrs, %i18n, %events --          }
{   compact     (compact)      #IMPLIED -- reduced interitem spacing --            }
{ <!ATTLIST MENU                                                                   }
{   %attrs;                              -- %coreattrs, %i18n, %events --          }
{   compact     (compact)      #IMPLIED -- reduced interitem spacing --            }
type
  ThtmlDIR = class(AhtmlListItemContainer)
  public
    constructor Create; override;
  end;

  ThtmlMENU = class(AhtmlListItemContainer)
  public
    constructor Create; override;
  end;

{ <!ELEMENT LI - O (%flow;)*             -- list item -->                          }
{ <!ATTLIST LI                                                                     }
{   %attrs;                              -- %coreattrs, %i18n, %events --          }
{   type        %LIStyle;      #IMPLIED  -- list item style --                     }
{   value       NUMBER         #IMPLIED  -- reset sequence number --               }
type
  ThtmlLI = class(AhtmlElementInclAttrs)
  private
    function  GetItemType: String;
    procedure SetItemType(const Value: String);
    function  GetValue: Integer;
    procedure SetValue(const Value: Integer);

  public
    constructor Create; override;

    property  ItemType: String read GetItemType write SetItemType;
    property  Value: Integer read GetValue write SetValue;
  end;



{ ============================================================================ }
{ FORMS                                                                        }
{ ============================================================================ }

{ <!ELEMENT FORM - - (%flow;)* -(FORM)   -- interactive form -->                   }
{ <!ATTLIST FORM                                                                   }
{   %attrs;                              -- %coreattrs, %i18n, %events --          }
{   action      %URI;          #REQUIRED -- server-side form handler --            }
{   method      (GET|POST)     GET       -- HTTP method used to submit the form--  }
{   enctype     %ContentType;  "application/x-www-form-urlencoded"                 }
{   accept      %ContentTypes; #IMPLIED  -- list of MIME types for file upload --  }
{   name        CDATA          #IMPLIED  -- name of form for scripting --          }
{   onsubmit    %Script;       #IMPLIED  -- the form was submitted --              }
{   onreset     %Script;       #IMPLIED  -- the form was reset --                  }
{   target      %FrameTarget;  #IMPLIED  -- render in this frame --                }
{   accept-charset %Charsets;  #IMPLIED  -- list of supported charsets --          }
type
  ThtmlFORM = class(AhtmlElementInclAttrs)
  private
    function  GetAction: String;
    procedure SetAction(const Value: String);
    function  GetMethod: String;
    procedure SetMethod(const Value: String);
    function  GetNameAttr: String;
    procedure SetNameAttr(const Value: String);
    function  GetOnSubmit: String;
    procedure SetOnSubmit(const Value: String);
    function  GetOnReset: String;
    procedure SetOnReset(const Value: String);
    function  GetTarget: String;
    procedure SetTarget(const Value: String);

  public
    constructor Create; override;

    function  CreateItem(const ID: Integer; const Name: String): AhtmlObject; override;
    property  Action: String read GetAction write SetAction;
    property  Method: String read GetMethod write SetMethod;
    property  Name: String read GetNameAttr write SetNameAttr;
    property  OnSubmit: String read GetOnSubmit write SetOnSubmit;
    property  OnReset: String read GetOnReset write SetOnReset;
    property  Target: String read GetTarget write SetTarget;
  end;

{ <!ELEMENT LABEL - - (%inline;)* -(LABEL) -- form field label text -->            }
{ <!ATTLIST LABEL                                                                  }
{   %attrs;                              -- %coreattrs, %i18n, %events --          }
{   for         IDREF          #IMPLIED  -- matches field ID value --              }
{   accesskey   %Character;    #IMPLIED  -- accessibility key character --         }
{   onfocus     %Script;       #IMPLIED  -- the element got the focus --           }
{   onblur      %Script;       #IMPLIED  -- the element lost the focus --          }
type
  ThtmlLABEL = class(AhtmlElementInclAttrs)
  private
    function  GetForID: String;
    procedure SetForID(const Value: String);
    function  GetOnFocus: String;
    procedure SetOnFocus(const Value: String);
    function  GetOnBlur: String;
    procedure SetOnBlur(const Value: String);

  public
    constructor Create; override;

    property  ForID: String read GetForID write SetForID;
    property  OnFocus: String read GetOnFocus write SetOnFocus;
    property  OnBlur: String read GetOnBlur write SetOnBlur;
  end;

{ AhtmlTextInputElement                                                                  }
{   %attrs;                              -- %coreattrs, %i18n, %events --                }
{   name        CDATA          #IMPLIED  -- submit as part of form --                    }
{   disabled    (disabled)     #IMPLIED  -- unavailable in this context --               }
{   readonly    (readonly)     #IMPLIED  -- for text and passwd --                       }
{   tabindex    NUMBER         #IMPLIED  -- position in tabbing order --                 }
{   onfocus     %Script;       #IMPLIED  -- the element got the focus --                 }
{   onblur      %Script;       #IMPLIED  -- the element lost the focus --                }
{   onselect    %Script;       #IMPLIED  -- some text was selected --                    }
{   onchange    %Script;       #IMPLIED  -- the element value was changed --             }
type
  AhtmlTextInputElement = class(AhtmlElementInclAttrs)
  private
    function  GetNameAttr: String;
    procedure SetNameAttr(const Value: String);
    function  GetDisabled: Boolean;
    procedure SetDisabled(const Value: Boolean);
    function  GetReadOnly: Boolean;
    procedure SetReadOnly(const Value: Boolean);
    function  GetTabIndex: String;
    procedure SetTabIndex(const Value: String);
    function  GetOnFocus: String;
    procedure SetOnFocus(const Value: String);
    function  GetOnBlur: String;
    procedure SetOnBlur(const Value: String);
    function  GetOnSelect: String;
    procedure SetOnSelect(const Value: String);
    function  GetOnChange: String;
    procedure SetOnChange(const Value: String);

  public
    property  Name: String read GetNameAttr write SetNameAttr;
    property  Disabled: Boolean read GetDisabled write SetDisabled;
    property  ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property  TabIndex: String read GetTabIndex write SetTabIndex;
    property  OnFocus: String read GetOnFocus write SetOnFocus;
    property  OnBlur: String read GetOnBlur write SetOnBlur;
    property  OnSelect: String read GetOnSelect write SetOnSelect;
    property  OnChange: String read GetOnChange write SetOnChange;
  end;

{ <!ENTITY % InputType  "(TEXT | PASSWORD | CHECKBOX | RADIO | SUBMIT | RESET |          }
{                         FILE | HIDDEN | IMAGE | BUTTON)"                               }
{ <!ELEMENT INPUT - O EMPTY              -- form control -->                             }
{ <!ATTLIST INPUT                                                                        }
{   %attrs;                              -- %coreattrs, %i18n, %events --                }
{   type        %InputType;    TEXT      -- what kind of widget is needed --             }
{   name        CDATA          #IMPLIED  -- submit as part of form --                    }
{   value       CDATA          #IMPLIED  -- Specify for radio buttons and checkboxes --  }
{   checked     (checked)      #IMPLIED  -- for radio buttons and check boxes --         }
{   disabled    (disabled)     #IMPLIED  -- unavailable in this context --               }
{   readonly    (readonly)     #IMPLIED  -- for text and passwd --                       }
{   size        CDATA          #IMPLIED  -- specific to each type of field --            }
{   maxlength   NUMBER         #IMPLIED  -- max chars for text fields --                 }
{   src         %URI;          #IMPLIED  -- for fields with images --                    }
{   alt         CDATA          #IMPLIED  -- short description --                         }
{   usemap      %URI;          #IMPLIED  -- use client-side image map --                 }
{   ismap       (ismap)        #IMPLIED  -- use server-side image map --                 }
{   tabindex    NUMBER         #IMPLIED  -- position in tabbing order --                 }
{   accesskey   %Character;    #IMPLIED  -- accessibility key character --               }
{   onfocus     %Script;       #IMPLIED  -- the element got the focus --                 }
{   onblur      %Script;       #IMPLIED  -- the element lost the focus --                }
{   onselect    %Script;       #IMPLIED  -- some text was selected --                    }
{   onchange    %Script;       #IMPLIED  -- the element value was changed --             }
{   accept      %ContentTypes; #IMPLIED  -- list of MIME types for file upload --        }
{   align       %IAlign;       #IMPLIED  -- vertical or horizontal alignment --          }
{   %reserved;                           -- reserved for possible future use --          }
type
  ThtmlINPUT = class(AhtmlTextInputElement)
  private
    function  GetInputType: ThtmlInputType;
    procedure SetInputType(const Value: ThtmlInputType);
    function  GetValue: String;
    procedure SetValue(const Value: String);
    function  GetChecked: Boolean;
    procedure SetChecked(const Value: Boolean);
    function  GetSize: String;
    procedure SetSize(const Value: String);
    function  GetMaxLength: Integer;
    procedure SetMaxLength(const Value: Integer);
    function  GetSrc: String;
    procedure SetSrc(const Value: String);
    function  GetAlt: String;
    procedure SetAlt(const Value: String);
    function  GetAlign: ThtmlIAlignType;
    procedure SetAlign(const Value: ThtmlIAlignType);

  public
    constructor Create; override;

    property  InputType: ThtmlInputType read GetInputType write SetInputType;
    property  Value: String read GetValue write SetValue;
    property  Checked: Boolean read GetChecked write SetChecked;
    property  Size: String read GetSize write SetSize;
    property  MaxLength: Integer read GetMaxLength write SetMaxLength;
    property  Src: String read GetSrc write SetSrc;
    property  Alt: String read GetAlt write SetAlt;
    property  Align: ThtmlIAlignType read GetAlign write SetAlign;

    function  GetSizeInt: Integer;
  end;

{ <!ELEMENT SELECT - - (OPTGROUP|OPTION)+ -- option selector -->                   }
{ <!ATTLIST SELECT                                                                 }
{   %attrs;                              -- %coreattrs, %i18n, %events --          }
{   name        CDATA          #IMPLIED  -- field name --                          }
{   size        NUMBER         #IMPLIED  -- rows visible --                        }
{   multiple    (multiple)     #IMPLIED  -- default is single selection --         }
{   disabled    (disabled)     #IMPLIED  -- unavailable in this context --         }
{   tabindex    NUMBER         #IMPLIED  -- position in tabbing order --           }
{   onfocus     %Script;       #IMPLIED  -- the element got the focus --           }
{   onblur      %Script;       #IMPLIED  -- the element lost the focus --          }
{   onchange    %Script;       #IMPLIED  -- the element value was changed --       }
{   %reserved;                           -- reserved for possible future use --    }
type
  ThtmlSELECT = class(AhtmlElementInclAttrs)
  private
    function  GetNameAttr: String;
    procedure SetNameAttr(const Value: String);
    function  GetSize: Integer;
    procedure SetSize(const Value: Integer);
    function  GetMultiple: Boolean;
    procedure SetMultiple(const Value: Boolean);
    function  GetDisabled: Boolean;
    procedure SetDisabled(const Value: Boolean);
    function  GetTabIndex: String;
    procedure SetTabIndex(const Value: String);
    function  GetOnFocus: String;
    procedure SetOnFocus(const Value: String);
    function  GetOnBlur: String;
    procedure SetOnBlur(const Value: String);
    function  GetOnChange: String;
    procedure SetOnChange(const Value: String);

  public
    constructor Create; override;

    function  CreateItem(const ID: Integer; const Name: String): AhtmlObject; override;
    property  Name: String read GetNameAttr write SetNameAttr;
    property  Size: Integer read GetSize write SetSize;
    property  Multiple: Boolean read GetMultiple write SetMultiple;
    property  Disabled: Boolean read GetDisabled write SetDisabled;
    property  TabIndex: String read GetTabIndex write SetTabIndex;
    property  OnFocus: String read GetOnFocus write SetOnFocus;
    property  OnBlur: String read GetOnBlur write SetOnBlur;
    property  OnChange: String read GetOnChange write SetOnChange;
  end;

{ <!ELEMENT OPTGROUP - - (OPTION)+ -- option group -->                             }
{ <!ATTLIST OPTGROUP                                                               }
{   %attrs;                              -- %coreattrs, %i18n, %events --          }
{   disabled    (disabled)     #IMPLIED  -- unavailable in this context --         }
{   label       %Text;         #REQUIRED -- for use in hierarchical menus --       }
type
  ThtmlOPTGROUP = class(AhtmlElementInclAttrs)
  private
    function  GetDisabled: Boolean;
    procedure SetDisabled(const Value: Boolean);
    function  GetLabelText: String;
    procedure SetLabelText(const Value: String);

  public
    constructor Create; override;

    property  Disabled: Boolean read GetDisabled write SetDisabled;
    property  LabelText: String read GetLabelText write SetLabelText;
  end;

{ <!ELEMENT OPTION - O (#PCDATA)         -- selectable choice -->                  }
{ <!ATTLIST OPTION                                                                 }
{   %attrs;                              -- %coreattrs, %i18n, %events --          }
{   selected    (selected)     #IMPLIED                                            }
{   disabled    (disabled)     #IMPLIED  -- unavailable in this context --         }
{   label       %Text;         #IMPLIED  -- for use in hierarchical menus --       }
{   value       CDATA          #IMPLIED  -- defaults to element content --         }
type
  ThtmlOPTION = class(AhtmlElementInclAttrs)
  private
    function  GetDisabled: Boolean;
    procedure SetDisabled(const Value: Boolean);
    function  GetSelected: Boolean;
    procedure SetSelected(const Value: Boolean);
    function  GetLabelText: String;
    procedure SetLabelText(const Value: String);
    function  GetValue: String;
    procedure SetValue(const Value: String);

  public
    constructor Create; override;

    property  Selected: Boolean read GetSelected write SetSelected;
    property  Disabled: Boolean read GetDisabled write SetDisabled;
    property  LabelText: String read GetLabelText write SetLabelText;
    property  Value: String read GetValue write SetValue;
  end;

{ <!ELEMENT TEXTAREA - - (#PCDATA)       -- multi-line text field -->              }
{ <!ATTLIST TEXTAREA                                                               }
{   %attrs;                              -- %coreattrs, %i18n, %events --          }
{   name        CDATA          #IMPLIED                                            }
{   rows        NUMBER         #REQUIRED                                           }
{   cols        NUMBER         #REQUIRED                                           }
{   disabled    (disabled)     #IMPLIED  -- unavailable in this context --         }
{   readonly    (readonly)     #IMPLIED                                            }
{   tabindex    NUMBER         #IMPLIED  -- position in tabbing order --           }
{   accesskey   %Character;    #IMPLIED  -- accessibility key character --         }
{   onfocus     %Script;       #IMPLIED  -- the element got the focus --           }
{   onblur      %Script;       #IMPLIED  -- the element lost the focus --          }
{   onselect    %Script;       #IMPLIED  -- some text was selected --              }
{   onchange    %Script;       #IMPLIED  -- the element value was changed --       }
{   %reserved;                           -- reserved for possible future use --    }
type
  ThtmlTEXTAREA = class(AhtmlTextInputElement)
  private
    function  GetRows: Integer;
    procedure SetRows(const Value: Integer);
    function  GetCols: Integer;
    procedure SetCols(const Value: Integer);

  public
    constructor Create; override;

    property  Rows: Integer read GetRows write SetRows;
    property  Cols: Integer read GetCols write SetCols;
  end;

{ <!ELEMENT FIELDSET - - (#PCDATA,LEGEND,(%flow;)*) -- form control group -->      }
{ <!ATTLIST FIELDSET                                                               }
{   %attrs;                              -- %coreattrs, %i18n, %events --          }
type
  ThtmlFIELDSET = class(AhtmlElementInclAttrs)
  public
    constructor Create; override;
  end;

{ <!ELEMENT LEGEND - - (%inline;)*       -- fieldset legend -->                    }
{ <!ATTLIST LEGEND                                                                 }
{   %attrs;                              -- %coreattrs, %i18n, %events --          }
{   accesskey   %Character;    #IMPLIED  -- accessibility key character --         }
{   align       %LAlign;       #IMPLIED  -- relative to fieldset --                }
type
  ThtmlLEGEND = class(AhtmlElementInclAttrs)
  private
    function  GetAlign: String;
    procedure SetAlign(const Value: String);

  public
    constructor Create; override;

    property  Align: String read GetAlign write SetAlign;
  end;

{ <!ELEMENT BUTTON - - (%flow;)* -(A|%formctrl;|FORM|ISINDEX|FIELDSET|IFRAME)      }
{ <!ATTLIST BUTTON                                                                 }
{   %attrs;                              -- %coreattrs, %i18n, %events --          }
{   name        CDATA          #IMPLIED                                            }
{   value       CDATA          #IMPLIED  -- sent to server when submitted --       }
{   type        (button|submit|reset) submit -- for use as form button --          }
{   disabled    (disabled)     #IMPLIED  -- unavailable in this context --         }
{   tabindex    NUMBER         #IMPLIED  -- position in tabbing order --           }
{   accesskey   %Character;    #IMPLIED  -- accessibility key character --         }
{   onfocus     %Script;       #IMPLIED  -- the element got the focus --           }
{   onblur      %Script;       #IMPLIED  -- the element lost the focus --          }
{   %reserved;                           -- reserved for possible future use --    }
type
  ThtmlBUTTON = class(AhtmlElementInclAttrs)
  private
    function  GetNameAttr: String;
    procedure SetNameAttr(const Value: String);
    function  GetValue: String;
    procedure SetValue(const Value: String);
    function  GetButtonType: String;
    procedure SetButtonType(const Value: String);
    function  GetDisabled: Boolean;
    procedure SetDisabled(const Value: Boolean);
    function  GetTabIndex: String;
    procedure SetTabIndex(const Value: String);
    function  GetOnFocus: String;
    procedure SetOnFocus(const Value: String);
    function  GetOnBlur: String;
    procedure SetOnBlur(const Value: String);

  public
    constructor Create; override;

    property  Name: String read GetNameAttr write SetNameAttr;
    property  Value: String read GetValue write SetValue;
    property  ButtonType: String read GetButtonType write SetButtonType;
    property  Disabled: Boolean read GetDisabled write SetDisabled;
    property  TabIndex: String read GetTabIndex write SetTabIndex;
    property  OnFocus: String read GetOnFocus write SetOnFocus;
    property  OnBlur: String read GetOnBlur write SetOnBlur;
  end;

{ ============================================================================ }
{ TABLES                                                                       }
{ ============================================================================ }

{ <!ENTITY % TAlign "(left|center|right)">                                           }
{ <!ELEMENT TABLE - -                                                                }
{      (CAPTION?, (COL*|COLGROUP*), THEAD?, TFOOT?, TBODY+)>                         }
{ <!ELEMENT CAPTION  - - (%inline;)*     -- table caption -->                        }
{ <!ELEMENT THEAD    - O (TR)+           -- table header -->                         }
{ <!ELEMENT TFOOT    - O (TR)+           -- table footer -->                         }
{ <!ELEMENT TBODY    O O (TR)+           -- table body -->                           }
{ <!ELEMENT COLGROUP - O (COL)*          -- table column group -->                   }
{ <!ELEMENT COL      - O EMPTY           -- table column -->                         }
{ <!ELEMENT TR       - O (TH|TD)+        -- table row -->                            }
{ <!ELEMENT (TH|TD)  - O (%flow;)*       -- table header cell, table data cell-->    }
{ <!ATTLIST TABLE                        -- table element --                         }
{   %attrs;                              -- %coreattrs, %i18n, %events --            }
{   summary     %Text;         #IMPLIED  -- purpose/structure for speech output--    }
{   width       %Length;       #IMPLIED  -- table width --                           }
{   border      %Pixels;       #IMPLIED  -- controls frame width around table --     }
{   frame       %TFrame;       #IMPLIED  -- which parts of frame to render --        }
{   rules       %TRules;       #IMPLIED  -- rulings between rows and cols --         }
{   cellspacing %Length;       #IMPLIED  -- spacing between cells --                 }
{   cellpadding %Length;       #IMPLIED  -- spacing within cells --                  }
{   align       %TAlign;       #IMPLIED  -- table position relative to window --     }
{   bgcolor     %Color;        #IMPLIED  -- background color for cells --            }
{   %reserved;                           -- reserved for possible future use --      }
{   datapagesize CDATA         #IMPLIED  -- reserved for possible future use --      }
type
  ThtmlTABLE = class(AhtmlElementInclAttrs)
  protected
    function  GetSummary: String;
    procedure SetSummary(const Value: String);
    function  GetWidthStr: String;
    procedure SetWidthStr(const Value: String);
    function  GetWidth: ThtmlLength;
    function  GetBorder: Integer;
    procedure SetBorder(const Value: Integer);
    function  GetCellSpacing: String;
    procedure SetCellSpacing(const Value: String);
    function  GetCellSpacingInt: Integer;
    function  GetCellPadding: String;
    procedure SetCellPadding(const Value: String);
    function  GetCellPaddingInt: Integer;
    function  GetBgColor: String;
    procedure SetBgColor(const Value: String);
    function  GetBgColorRGB: LongWord;
    procedure SetBgColorRGB(const Value: LongWord);
    function  GetBgColorDelphi: TColor;
    function  GetAlign: ThtmlTAlignType;
    procedure SetAlign(const Value: ThtmlTAlignType);

    procedure ApplyHTMLStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
              const ParentStyle: ThtmlcssStyleProperties); override;

  public
    constructor Create; override;

    function  CreateItem(const ID: Integer; const Name: String): AhtmlObject; override;
    property  Summary: String read GetSummary write SetSummary;
    property  WidthStr: String read GetWidthStr write SetWidthStr;
    property  Width: ThtmlLength read GetWidth;
    property  Border: Integer read GetBorder write SetBorder;
    property  CellSpacing: String read GetCellSpacing write SetCellSpacing;
    property  CellSpacingInt: Integer read GetCellSpacingInt;
    property  CellPadding: String read GetCellPadding write SetCellPadding;
    property  CellPaddingInt: Integer read GetCellPaddingInt;
    property  Align: ThtmlTAlignType read GetAlign write SetAlign;
    property  BgColor: String read GetBgColor write SetBgColor;
    property  BgColorRGB: LongWord read GetBgColorRGB write SetBgColorRGB;
    property  BgColorDelphi: TColor read GetBgColorDelphi;

    function  GetColGroupsSpanResolved: Integer;
    function  GetColGroupsWidthsPixels(const StyleSheet: ThtmlCSS;
              const AbsWidth: Integer): IntegerArray;
    procedure GetColGroupsAlign(var HorAlign: ThtmlTextAlignArray;
              var VerAlign: ThtmlVerticalAlignArray);
  end;

{ <!ENTITY % CAlign "(top|bottom|left|right)">                                     }
{ <!ATTLIST CAPTION                                                                }
{   %attrs;                              -- %coreattrs, %i18n, %events --          }
{   align       %CAlign;       #IMPLIED  -- relative to table --                   }
type
  ThtmlCAPTION = class(AhtmlElementInclAttrs)
  private
    function  GetAlign: String;
    procedure SetAlign(const Value: String);

  public
    constructor Create; override;

    property  Align: String read GetAlign write SetAlign;
  end;

{   %attrs;                              -- %coreattrs, %i18n, %events --          }
{   %cellhalign;                         -- horizontal alignment in cells --       }
{   %cellvalign;                         -- vertical alignment in cells --         }
{ <!ENTITY % cellhalign                                                            }
{   "align      (left|center|right|justify|char) #IMPLIED                          }
{    char       %Character;    #IMPLIED  -- alignment char, e.g. char=':' --       }
{    charoff    %Length;       #IMPLIED  -- offset for alignment char --"          }
{ <!ENTITY % cellvalign                                                            }
{   "valign     (top|middle|bottom|baseline) #IMPLIED"                             }
type
  AhtmlElementInclCellAlign = class(AhtmlElementInclAttrs)
  private
    function  GetAlign: String;
    procedure SetAlign(const Value: String);
    function  GetAlignChar: String;
    procedure SetAlignChar(const Value: String);
    function  GetCharOff: String;
    procedure SetCharOff(const Value: String);
    function  GetVAlign: String;
    procedure SetVAlign(const Value: String);
    function  GetHorAlign: ThtmlTextAlignType;
    function  GetVerAlign: ThtmlVerticalAlign;

  public
    property  Align: String read GetAlign write SetAlign;
    property  AlignChar: String read GetAlignChar write SetAlignChar;
    property  CharOff: String read GetCharOff write SetCharOff;
    property  VAlign: String read GetVAlign write SetVAlign;
    property  HorAlign: ThtmlTextAlignType read GetHorAlign;
    property  VerAlign: ThtmlVerticalAlign read GetVerAlign;
  end;

{   %attrs;                              -- %coreattrs, %i18n, %events --          }
{   span        NUMBER         1         -- default number of columns in group --  }
{   width       %MultiLength;  #IMPLIED  -- default width for enclosed COLs --     }
{   %cellhalign;                         -- horizontal alignment in cells --       }
{   %cellvalign;                         -- vertical alignment in cells --         }
type
  AhtmlTableColumnDefinitionElement = class(AhtmlElementInclCellAlign)
  private
    function  GetSpan: Integer;
    procedure SetSpan(const Value: Integer);
    function  GetWidthStr: String;
    procedure SetWidthStr(const Value: String);
    function  GetWidth: ThtmlLength;

  protected
    procedure ApplyHTMLStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
              const ParentStyle: ThtmlcssStyleProperties); override;

  public
    property  Span: Integer read GetSpan write SetSpan;
    property  WidthStr: String read GetWidthStr write SetWidthStr;
    property  Width: ThtmlLength read GetWidth;
  end;

{ <!ATTLIST COLGROUP                                                               }
{   %attrs;                              -- %coreattrs, %i18n, %events --          }
{   span        NUMBER         1         -- default number of columns in group --  }
{   width       %MultiLength;  #IMPLIED  -- default width for enclosed COLs --     }
{   %cellhalign;                         -- horizontal alignment in cells --       }
{   %cellvalign;                         -- vertical alignment in cells --         }
type
  ThtmlCOLGROUP = class(AhtmlTableColumnDefinitionElement)
  public
    constructor Create; override;

    function  GetSpanResolved: Integer;
    function  GetColWidthsPixels(const StyleSheet: ThtmlCSS;
              const AbsWidth: Integer): IntegerArray;
    procedure GetGroupAlign(var HorAlign: ThtmlTextAlignArray;
              var VerAlign: ThtmlVerticalAlignArray);
  end;

{ <!ATTLIST COL                          -- column groups and properties --        }
{   %attrs;                              -- %coreattrs, %i18n, %events --          }
{   span        NUMBER         1         -- COL attributes affect N columns --     }
{   width       %MultiLength;  #IMPLIED  -- column width specification --          }
{   %cellhalign;                         -- horizontal alignment in cells --       }
{   %cellvalign;                         -- vertical alignment in cells --         }
type
  ThtmlCOL = class(AhtmlTableColumnDefinitionElement)
  public
    constructor Create; override;
  end;

{ <!ATTLIST (THEAD|TBODY|TFOOT)          -- table section --                       }
{   %attrs;                              -- %coreattrs, %i18n, %events --          }
{   %cellhalign;                         -- horizontal alignment in cells --       }
{   %cellvalign;                         -- vertical alignment in cells --         }
type
  ThtmlTHEAD = class(AhtmlElementInclCellAlign)
  public
    constructor Create; override;
  end;

  ThtmlTBODY = class(AhtmlElementInclCellAlign)
  public
    constructor Create; override;
  end;

  ThtmlTFOOT = class(AhtmlElementInclCellAlign)
  public
    constructor Create; override;
  end;

{ <!ATTLIST TR                           -- table row --                           }
{   %attrs;                              -- %coreattrs, %i18n, %events --          }
{   %cellhalign;                         -- horizontal alignment in cells --       }
{   %cellvalign;                         -- vertical alignment in cells --         }
{   bgcolor     %Color;        #IMPLIED  -- background color for row --            }
type
  ThtmlTR = class(AhtmlElementInclCellAlign)
  protected
    function  GetBgColor: String;
    procedure SetBgColor(const Value: String);
    function  GetBgColorDelphi: TColor;

    procedure ApplyHTMLStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
              const ParentStyle: ThtmlcssStyleProperties); override;

  public
    constructor Create; override;

    property  BgColor: String read GetBgColor write SetBgColor;
    property  BgColorDelphi: TColor read GetBgColorDelphi;
  end;

{ <!ATTLIST (TH|TD)                      -- header or data cell --                    }
{   %attrs;                              -- %coreattrs, %i18n, %events --             }
{   abbr        %Text;         #IMPLIED  -- abbreviation for header cell --           }
{   axis        CDATA          #IMPLIED  -- comma-separated list of related headers-- }
{   headers     IDREFS         #IMPLIED  -- list of id's for header cells --          }
{   scope       %Scope;        #IMPLIED  -- scope covered by header cells --          }
{   rowspan     NUMBER         1         -- number of rows spanned by cell --         }
{   colspan     NUMBER         1         -- number of cols spanned by cell --         }
{   %cellhalign;                         -- horizontal alignment in cells --          }
{   %cellvalign;                         -- vertical alignment in cells --            }
{   nowrap      (nowrap)       #IMPLIED  -- suppress word wrap --                     }
{   bgcolor     %Color;        #IMPLIED  -- cell background color --                  }
{   width       %Length;       #IMPLIED  -- width for cell --                         }
{   height      %Length;       #IMPLIED  -- height for cell --                        }
type
  AhtmlTableDataCell = class(AhtmlElementInclCellAlign)
  protected
    function  GetAbbr: String;
    procedure SetAbbr(const Value: String);
    function  GetRowSpan: Integer;
    procedure SetRowSpan(const Value: Integer);
    function  GetColSpan: Integer;
    procedure SetColSpan(const Value: Integer);
    function  GetNoWrap: Boolean;
    procedure SetNoWrap(const Value: Boolean);
    function  GetBgColor: String;
    procedure SetBgColor(const Value: String);
    function  GetBgColorDelphi: TColor;
    function  GetWidthStr: String;
    procedure SetWidthStr(const Value: String);
    function  GetHeightStr: String;
    procedure SetHeightStr(const Value: String);
    function  GetWidth: ThtmlLength;
    function  GetHeight: ThtmlLength;

    procedure ApplyHTMLStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
              const ParentStyle: ThtmlcssStyleProperties); override;

  public
    constructor Create(const TagID: ThtmlTagID); overload;

    property  Abbr: String read GetAbbr write SetAbbr;
    property  RowSpan: Integer read GetRowSpan write SetRowSpan;
    property  ColSpan: Integer read GetColSpan write SetColSpan;
    property  NoWrap: Boolean read GetNoWrap write SetNoWrap;
    property  BgColor: String read GetBgColor write SetBgColor;
    property  BgColorDelphi: TColor read GetBgColorDelphi;
    property  WidthStr: String read GetWidthStr write SetWidthStr;
    property  HeightStr: String read GetHeightStr write SetHeightStr;
    property  Width: ThtmlLength read GetWidth;
    property  Height: ThtmlLength read GetHeight;
  end;

  ThtmlTD = class(AhtmlTableDataCell)
  public
    constructor Create; override;
  end;

  ThtmlTH = class(AhtmlTableDataCell)
  public
    constructor Create; override;
  end;



{ ============================================================================ }
{ FRAMES                                                                       }
{ ============================================================================ }

{ <!ELEMENT FRAMESET - - ((FRAMESET|FRAME)+ & NOFRAMES?) -- window subdivision-->  }
{ <!ATTLIST FRAMESET                                                               }
{   %coreattrs;                          -- id, class, style, title --             }
{   rows        %MultiLengths; #IMPLIED  -- list of lengths,                       }
{                                           default: 100% (1 row) --               }
{   cols        %MultiLengths; #IMPLIED  -- list of lengths,                       }
{                                           default: 100% (1 col) --               }
{   onload      %Script;       #IMPLIED  -- all the frames have been loaded  --    }
{   onunload    %Script;       #IMPLIED  -- all the frames have been removed --    }
type
  ThtmlFRAMESET = class(AhtmlElementInclCoreAttrs)
  private
    function  GetRows: String;
    procedure SetRows(const Value: String);
    function  GetCols: String;
    procedure SetCols(const Value: String);
    function  GetOnLoad: String;
    procedure SetOnLoad(const Value: String);
    function  GetOnUnload: String;
    procedure SetOnUnload(const Value: String);

  public
    constructor Create; override;

    function  CreateItem(const ID: Integer; const Name: String): AhtmlObject; override;
    property  Rows: String read GetRows write SetRows;
    property  Cols: String read GetCols write SetCols;
    property  OnLoad: String read GetOnLoad write SetOnLoad;
    property  OnUnload: String read GetOnUnload write SetOnUnload;
  end;

{   %coreattrs;                          -- id, class, style, title --             }
{   longdesc    %URI;          #IMPLIED  -- link to long description               }
{                                           (complements title) --                 }
{   name        CDATA          #IMPLIED  -- name of frame for targetting --        }
{   src         %URI;          #IMPLIED  -- source of frame content --             }
{   frameborder (1|0)          1         -- request frame borders? --              }
{   marginwidth %Pixels;       #IMPLIED  -- margin widths in pixels --             }
{   marginheight %Pixels;      #IMPLIED  -- margin height in pixels --             }
{   scrolling   (yes|no|auto)  auto      -- scrollbar or none --                   }
type
  AhtmlFrameElement = class(AhtmlElementInclCoreAttrs)
  private
    function  GetLongDesc: String;
    procedure SetLongDesc(const Value: String);
    function  GetNameAttr: String;
    procedure SetNameAttr(const Value: String);
    function  GetSrc: String;
    procedure SetSrc(const Value: String);
    function  GetFrameBorder: Boolean;
    procedure SetFrameBorder(const Value: Boolean);
    function  GetMarginWidth: Integer;
    procedure SetMarginWidth(const Value: Integer);
    function  GetMarginHeight: Integer;
    procedure SetMarginHeight(const Value: Integer);
    function  GetScrolling: String;
    procedure SetScrolling(const Value: String);

  public
    property  LongDesc: String read GetLongDesc write SetLongDesc;
    property  Name: String read GetNameAttr write SetNameAttr;
    property  Src: String read GetSrc write SetSrc;
    property  FrameBorder: Boolean read GetFrameBorder write SetFrameBorder;
    property  MarginWidth: Integer read GetMarginWidth write SetMarginWidth;
    property  MarginHeight: Integer read GetMarginHeight write SetMarginHeight;
    property  Scrolling: String read GetScrolling write SetScrolling;
  end;

{ <!-- reserved frame names start with "_" otherwise starts with letter -->        }
{ <!ELEMENT FRAME - O EMPTY              -- subwindow -->                          }
{ <!ATTLIST FRAME                                                                  }
{   %coreattrs;                          -- id, class, style, title --             }
{   longdesc    %URI;          #IMPLIED  -- link to long description               }
{                                           (complements title) --                 }
{   name        CDATA          #IMPLIED  -- name of frame for targetting --        }
{   src         %URI;          #IMPLIED  -- source of frame content --             }
{   frameborder (1|0)          1         -- request frame borders? --              }
{   marginwidth %Pixels;       #IMPLIED  -- margin widths in pixels --             }
{   marginheight %Pixels;      #IMPLIED  -- margin height in pixels --             }
{   noresize    (noresize)     #IMPLIED  -- allow users to resize frames? --       }
{   scrolling   (yes|no|auto)  auto      -- scrollbar or none --                   }
type
  ThtmlFRAME = class(AhtmlFrameElement)
  private
    function  GetNoResize: Boolean;
    procedure SetNoResize(const Value: Boolean);

  public
    constructor Create; override;

    property  NoResize: Boolean read GetNoResize write SetNoResize;
  end;

{ <!ELEMENT IFRAME - - (%flow;)*         -- inline subwindow -->                   }
{ <!ATTLIST IFRAME                                                                 }
{   %coreattrs;                          -- id, class, style, title --             }
{   longdesc    %URI;          #IMPLIED  -- link to long description               }
{                                           (complements title) --                 }
{   name        CDATA          #IMPLIED  -- name of frame for targetting --        }
{   src         %URI;          #IMPLIED  -- source of frame content --             }
{   frameborder (1|0)          1         -- request frame borders? --              }
{   marginwidth %Pixels;       #IMPLIED  -- margin widths in pixels --             }
{   marginheight %Pixels;      #IMPLIED  -- margin height in pixels --             }
{   scrolling   (yes|no|auto)  auto      -- scrollbar or none --                   }
{   align       %IAlign;       #IMPLIED  -- vertical or horizontal alignment --    }
{   height      %Length;       #IMPLIED  -- frame height --                        }
{   width       %Length;       #IMPLIED  -- frame width --                         }
type
  ThtmlIFRAME = class(AhtmlFrameElement)
  private
    function  GetAlign: ThtmlIAlignType;
    procedure SetAlign(const Value: ThtmlIAlignType);
    function  GetHeight: String;
    procedure SetHeight(const Value: String);
    function  GetWidth: String;
    procedure SetWidth(const Value: String);

  public
    constructor Create; override;

    property  Align: ThtmlIAlignType read GetAlign write SetAlign;
    property  Height: String read GetHeight write SetHeight;
    property  Width: String read GetWidth write SetWidth;
  end;

{ <!ELEMENT NOFRAMES - - %noframes.content;                                        }
{ <!ATTLIST NOFRAMES                                                               }
{   %attrs;                              -- %coreattrs, %i18n, %events --          }
type
  ThtmlNOFRAMES = class(AhtmlElementInclAttrs)
  public
    constructor Create; override;
  end;



{ ============================================================================ }
{ DOCUMENT HEAD                                                                }
{ ============================================================================ }

{ <!ENTITY % head.misc "SCRIPT|STYLE|META|LINK|OBJECT" -- repeatable head elements --> }
{ <!ENTITY % head.content "TITLE & ISINDEX? & BASE?">                                  }
{ <!ELEMENT HEAD O O (%head.content;) +(%head.misc;) -- document head -->              }
{ <!ATTLIST HEAD                                                                       }
{   %i18n;                               -- lang, dir --                               }
{   profile     %URI;          #IMPLIED  -- named dictionary of meta info --           }
type
  ThtmlHEAD = class(AhtmlElementIncli18n)
  protected
    function  GetMetaHttpField(const Name: String): String;
    procedure SetMetaHttpField(const Name, Value: String);

    function  GetProfile: String;
    procedure SetProfile(const Value: String);
    function  GetTitle: String;
    procedure SetTitle(const Value: String);
    function  GetContentType: String;
    procedure SetContentType(const Value: String);
    function  GetDescription: String;
    procedure SetDescription(const Value: String);
    function  GetKeywords: String;
    procedure SetKeywords(const Value: String);
    function  GetAutoRefresh: Boolean;
    procedure SetAutoRefresh(const Value: Boolean);
    function  GetAutoRefreshInterval: Integer;
    procedure SetAutoRefreshInterval(const Value: Integer);
    function  GetAutoRefreshURL: String;
    procedure SetAutoRefreshURL(const Value: String);
    function  GetStyleText: String;
    function  GetStyleRefs: StringArray;

  public
    constructor Create; override;

    function  CreateItem(const ID: Integer; const Name: String): AhtmlObject; override;

    function  FindMetaHttpField(const Name: String): AhtmlElement;
    property  MetaHttpField[const Name: String]: String
              read GetMetaHttpField write SetMetaHttpField;

    property  Profile: String read GetProfile write SetProfile;
    property  Title: String read GetTitle write SetTitle;
    property  ContentType: String read GetContentType write SetContentType;
    property  Description: String read GetDescription write SetDescription;
    property  Keywords: String read GetKeywords write SetKeywords;
    property  AutoRefresh: Boolean read GetAutoRefresh write SetAutoRefresh;
    property  AutoRefreshInterval: Integer
              read GetAutoRefreshInterval write SetAutoRefreshInterval;
    property  AutoRefreshURL: String
              read GetAutoRefreshURL write SetAutoRefreshURL;
    property  StyleText: String read GetStyleText;
    property  StyleRefs: StringArray read GetStyleRefs;

    procedure PrepareStructure;
  end;

{ <!ELEMENT TITLE - - (#PCDATA) -(%head.misc;) -- document title -->               }
{ <!ATTLIST TITLE %i18n>                                                           }
type
  ThtmlTITLE = class(AhtmlElementIncli18n)
  public
    constructor Create; override;
  end;

{ <!ELEMENT ISINDEX - O EMPTY            -- single line prompt -->                 }
{ <!ATTLIST ISINDEX                                                                }
{   %coreattrs;                          -- id, class, style, title --             }
{   %i18n;                               -- lang, dir --                           }
{   prompt      %Text;         #IMPLIED  -- prompt message -->                     }
type
  ThtmlISINDEX = class(AhtmlElementInclCoreAttrs)
  private
    function  GetPrompt: String;
    procedure SetPrompt(const Value: String);

  public
    constructor Create; override;

    property  Prompt: String read GetPrompt write SetPrompt;
  end;

{ <!ELEMENT BASE - O EMPTY               -- document base URI -->                  }
{ <!ATTLIST BASE                                                                   }
{   href        %URI;          #IMPLIED  -- URI that acts as base URI --           }
{   target      %FrameTarget;  #IMPLIED  -- render in this frame --                }
type
  ThtmlBASE = class(AhtmlKnownElement)
  private
    function  GetHRef: String;
    procedure SetHRef(const Value: String);
    function  GetTarget: String;
    procedure SetTarget(const Value: String);
    
  public
    constructor Create; override;

    property  HRef: String read GetHRef write SetHRef;
    property  Target: String read GetTarget write SetTarget;
  end;

{ <!ELEMENT META - O EMPTY               -- generic metainformation -->            }
{ <!ATTLIST META                                                                   }
{   %i18n;                               -- lang, dir, for use with content --     }
{   http-equiv  NAME           #IMPLIED  -- HTTP response header name  --          }
{   name        NAME           #IMPLIED  -- metainformation name --                }
{   content     CDATA          #REQUIRED -- associated information --              }
{   scheme      CDATA          #IMPLIED  -- select form of content --              }
type
  ThtmlMETA = class(AhtmlElementIncli18n)
  private
    function  GetHttpEquiv: String;
    procedure SetHttpEquiv(const Value: String);
    function  GetFieldContent: String;
    procedure SetFieldContent(const Value: String);
    function  GetNameAttr: String;
    procedure SetNameAttr(const Value: String);
    function  GetScheme: String;
    procedure SetScheme(const Value: String);

  public
    constructor CreateField(const HttpEquiv, Value: String); overload;
    constructor Create; override;

    property  HttpEquiv: String read GetHttpEquiv write SetHttpEquiv;
    property  FieldContent: String read GetFieldContent write SetFieldContent;
    property  Name: String read GetNameAttr write SetNameAttr;
    property  Scheme: String read GetScheme write SetScheme;
  end;

{ <!ELEMENT STYLE - - %StyleSheet        -- style info -->                         }
{ <!ATTLIST STYLE                                                                  }
{   %i18n;                               -- lang, dir, for use with title --       }
{   type        %ContentType;  #REQUIRED -- content type of style language --      }
{   media       %MediaDesc;    #IMPLIED  -- designed for use with these media --   }
{   title       %Text;         #IMPLIED  -- advisory title --                      }
type
  ThtmlSTYLE = class(AhtmlElementIncli18n)
  protected
    function  GetContentType: String;
    procedure SetContentType(const Value: String);
    function  GetMedia: String;
    procedure SetMedia(const Value: String);
    function  GetTitle: String;
    procedure SetTitle(const Value: String);
    function  GetStyleText: String;

  public
    constructor Create; override;

    property  ContentType: String read GetContentType write SetContentType;
    property  Media: String read GetMedia write SetMedia;
    property  Title: String read GetTitle write SetTitle;
    property  StyleText: String read GetStyleText;
  end;

{ <!ELEMENT SCRIPT - - %Script;          -- script statements -->                     }
{ <!ATTLIST SCRIPT                                                                    }
{   charset     %Charset;      #IMPLIED  -- char encoding of linked resource --       }
{   type        %ContentType;  #REQUIRED -- content type of script language --        }
{   language    CDATA          #IMPLIED  -- predefined script language name --        }
{   src         %URI;          #IMPLIED  -- URI for an external script --             }
{   defer       (defer)        #IMPLIED  -- UA may defer execution of script --       }
{   event       CDATA          #IMPLIED  -- reserved for possible future use --       }
{   for         %URI;          #IMPLIED  -- reserved for possible future use --       }
type
  ThtmlSCRIPT = class(AhtmlKnownElement)
  private
    function  GetContentType: String;
    procedure SetContentType(const Value: String);
    function  GetLanguage: String;
    procedure SetLangauge(const Value: String);
    function  GetSrc: String;
    procedure SetSrc(const Value: String);
    function  GetDefer: Boolean;
    procedure SetDefer(const Value: Boolean);

  public
    constructor Create; override;

    property  ContentType: String read GetContentType write SetContentType;
    property  Language: String read GetLanguage write SetLangauge;
    property  Src: String read GetSrc write SetSrc;
    property  Defer: Boolean read GetDefer write SetDefer;
  end;

{ <!ELEMENT NOSCRIPT - - (%flow;)*                                                 }
{ <!ATTLIST NOSCRIPT                                                               }
{   %attrs;                              -- %coreattrs, %i18n, %events --          }
type
  ThtmlNOSCRIPT = class(AhtmlElementInclAttrs)
  public
    constructor Create; override;
  end;



{ ============================================================================ }
{ DOCUMENT STRUCTURE                                                           }
{ ============================================================================ }

{ <!ENTITY % HTML.Version "-//W3C//DTD HTML 4.01 Transitional//EN"             }
type
  ThtmlVersionTag = class(AhtmlObject)
  protected
    FVersion: String;

    function  GetHTMLText: String; override;

  public
    constructor Create(const Version: String); overload;

    function  DuplicateObject: AhtmlObject; override;

    property  Version: String read FVersion write FVersion;
  end;

{ <!ENTITY % version "version CDATA #FIXED '%HTML.Version;'">                  }
{ <![ %HTML.Frameset; [<!ENTITY % html.content "HEAD, FRAMESET">]]>            }
{ <!ENTITY % html.content "HEAD, BODY">                                        }
{ <!ELEMENT HTML O O (%html.content;)    -- document root element -->          }
{ <!ATTLIST HTML                                                               }
{   %i18n;                               -- lang, dir --                       }
{   %version;                                                                  }
type
  ThtmlHTML = class(AhtmlElementIncli18n)
  protected
    procedure SetContentText(const ContentText: String); override;

    function  GetVersion: String;
    procedure SetVersion(const Value: String);
    function  GetHead: ThtmlHEAD;
    function  GetBody: ThtmlBODY;
    function  GetFrameSet: ThtmlFRAMESET;

  public
    constructor Create; override;

    function  CreateItem(const ID: Integer; const Name: String): AhtmlObject; override;

    property  Version: String read GetVersion write SetVersion;
    property  Head: ThtmlHEAD read GetHead;
    property  Body: ThtmlBODY read GetBody;
    property  FrameSet: ThtmlFRAMESET read GetFrameSet;

    procedure PrepareStructure;
  end;



implementation

uses
  { System }
  SysUtils,

  { Fundamentals }
  flcUtils,
  flcDynArrays,
  flcStrings;



{ Global tag factory                                                           }
const
  ElementClass: Array[ThtmlTagID] of ThtmlElementClass = (
      // Special tags
      nil,
      nil,
      // Element tags
      // From HTML 4.01
      ThtmlA,
      ThtmlABBR,
      ThtmlACRONYM,
      ThtmlADDRESS,
      ThtmlAPPLET,
      ThtmlAREA,
      ThtmlB,
      ThtmlBASE,
      ThtmlBASEFONT,
      ThtmlBDO,
      ThtmlBIG,
      ThtmlBLOCKQUOTE,
      ThtmlBODY,
      ThtmlBR,
      ThtmlBUTTON,
      ThtmlCAPTION,
      ThtmlCENTER,
      ThtmlCITE,
      ThtmlCODE,
      ThtmlCOL,
      ThtmlCOLGROUP,
      ThtmlDD,
      ThtmlDEL,
      ThtmlDFN,
      ThtmlDIR,
      ThtmlDIV,
      ThtmlDL,
      ThtmlDT,
      ThtmlEM,
      ThtmlFIELDSET,
      ThtmlFONT,
      ThtmlFORM,
      ThtmlFRAME,
      ThtmlFRAMESET,
      ThtmlH1,
      ThtmlH2,
      ThtmlH3,
      ThtmlH4,
      ThtmlH5,
      ThtmlH6,
      ThtmlHEAD,
      ThtmlHR,
      ThtmlHTML,
      ThtmlI,
      ThtmlIFRAME,
      ThtmlIMG,
      ThtmlINPUT,
      ThtmlINS,
      ThtmlISINDEX,
      ThtmlKBD,
      ThtmlLABEL,
      ThtmlLEGEND,
      ThtmlLI,
      ThtmlLINK,
      ThtmlMAP,
      ThtmlMENU,
      ThtmlMETA,
      ThtmlNOFRAMES,
      ThtmlNOSCRIPT,
      ThtmlEmbeddedOBJECT,
      ThtmlOL,
      ThtmlOPTGROUP,
      ThtmlOPTION,
      ThtmlP,
      ThtmlEmbeddedObjectPARAM,
      ThtmlPRE,
      ThtmlQ,
      ThtmlS,
      ThtmlSAMP,
      ThtmlSCRIPT,
      ThtmlSELECT,
      ThtmlSMALL,
      ThtmlSPAN,
      ThtmlSTRIKE,
      ThtmlSTRONG,
      ThtmlSTYLE,
      ThtmlSUB,
      ThtmlSUP,
      ThtmlTABLE,
      ThtmlTBODY,
      ThtmlTD,
      ThtmlTEXTAREA,
      ThtmlTFOOT,
      ThtmlTH,
      ThtmlTHEAD,
      ThtmlTITLE,
      ThtmlTR,
      ThtmlTT,
      ThtmlU,
      ThtmlUL,
      ThtmlVAR,
      // From HTML 5.1
      nil, // HTML_TAG_ARTICLE,
      nil, // HTML_TAG_ASIDE,
      nil, // HTML_TAG_AUDIO,
      nil, // HTML_TAG_BDI,
      nil, // HTML_TAG_CANVAS,
      nil, // HTML_TAG_COMMAND,
      nil, // HTML_TAG_DATALIST,
      nil, // HTML_TAG_DETAILS,
      nil, // HTML_TAG_DIALOG,
      nil, // HTML_TAG_FIGCAPTION,
      nil, // HTML_TAG_FIGURE,
      nil, // HTML_TAG_FOOTER,
      nil, // HTML_TAG_HEADER,
      nil, // HTML_TAG_HGROUP,
      nil, // HTML_TAG_KEYGEN,
      nil, // HTML_TAG_MARK,
      nil, // HTML_TAG_METER,
      nil, // HTML_TAG_NAV,
      nil, // HTML_TAG_OUTPUT,
      nil, // HTML_TAG_PROGRESS,
      nil, // HTML_TAG_SECTION,
      nil, // HTML_TAG_SOURCE,
      nil, // HTML_TAG_SUMMARY,
      nil, // HTML_TAG_TIME,
      nil, // HTML_TAG_TRACK,
      nil, // HTML_TAG_VIDEO,
      nil  // HTML_TAG_WBR
    );

function htmlElementClass(const TagID: ThtmlTagID): ThtmlElementClass;
begin
  Result := ElementClass[TagID];
end;

function htmlElementClass(const Name: String): ThtmlElementClass;
begin
  Result := ElementClass[htmlGetTagIDStr(Name)];
end;

function htmlCreateElement(const TagID: ThtmlTagID; const Name: String): AhtmlElement;
var C: ThtmlElementClass;
begin
  if TagID = HTML_TAG_None then
    C := htmlElementClass(Name)
  else
    C := htmlElementClass(TagID);
  if Assigned(C) then
    Result := C.Create
  else
    Result := ThtmlElement.Create(TagID, Name, ttContentTags);
end;



{ AhtmlElementInclCoreAttrs                                                        }
function AhtmlElementInclCoreAttrs.GetID: String;
begin
  Result := AttributeText['ID'];
end;

procedure AhtmlElementInclCoreAttrs.SetID(const Value: String);
begin
  AttributeText['ID'] := Value;
end;

function AhtmlElementInclCoreAttrs.GetClasses: String;
begin
  Result := AttributeText['CLASS'];
end;

procedure AhtmlElementInclCoreAttrs.SetClasses(const Value: String);
begin
  AttributeText['CLASS'] := Value;
end;

function AhtmlElementInclCoreAttrs.GetStyleAttrStr: String;
begin
  Result := AttributeText['STYLE'];
end;

procedure AhtmlElementInclCoreAttrs.SetStyleAttrStr(const Value: String);
begin
  AttributeText['STYLE'] := Value;
end;

function AhtmlElementInclCoreAttrs.GetTitle: String;
begin
  Result := AttributeText['TITLE'];
end;

procedure AhtmlElementInclCoreAttrs.SetTitle(const Value: String);
begin
  AttributeText['TITLE'] := Value;
end;

{ AhtmlElementIncli18n                                                         }
function AhtmlElementIncli18n.GetLang: String;
begin
  Result := AttributeText['LANG'];
end;

procedure AhtmlElementIncli18n.SetLang(const Value: String);
begin
  AttributeText['LANG'] := Value;
end;

function AhtmlElementIncli18n.GetDir: ThtmlTextDir;
begin
  Result := htmlDecodeTextDir(AttributeText['DIR']);
end;

procedure AhtmlElementIncli18n.SetDir(const Value: ThtmlTextDir);
begin
  AttributeText['DIR'] := htmlEncodeTextDir(Value);
end;

{ AhtmlElementInclCoreAttrsAndi18n                                             }
function AhtmlElementInclCoreAttrsAndi18n.GetLang: String;
begin
  Result := AttributeText['LANG'];
end;

procedure AhtmlElementInclCoreAttrsAndi18n.SetLang(const Value: String);
begin
  AttributeText['LANG'] := Value;
end;

function AhtmlElementInclCoreAttrsAndi18n.GetDir: ThtmlTextDir;
begin
  Result := htmlDecodeTextDir(AttributeText['DIR']);
end;

procedure AhtmlElementInclCoreAttrsAndi18n.SetDir(const Value: ThtmlTextDir);
begin
  AttributeText['DIR'] := htmlEncodeTextDir(Value);
end;

{ AhtmlElementInclAttrs                                                            }
function AhtmlElementInclAttrs.GetOnClick: String;
begin
  Result := AttributeText['ONCLICK'];
end;

procedure AhtmlElementInclAttrs.SetOnClick(const Value: String);
begin
  AttributeText['ONCLICK'] := Value;
end;

function AhtmlElementInclAttrs.GetOnDblClick: String;
begin
  Result := AttributeText['ONDBLCLICK'];
end;

procedure AhtmlElementInclAttrs.SetOnDblClick(const Value: String);
begin
  AttributeText['ONDBLCLICK'] := Value;
end;

function AhtmlElementInclAttrs.GetOnKeyPress: String;
begin
  Result := AttributeText['ONKEYPRESS'];
end;

procedure AhtmlElementInclAttrs.SetOnKeyPress(const Value: String);
begin
  AttributeText['ONKEYPRESS'] := Value;
end;

function AhtmlElementInclAttrs.GetOnKeyDown: String;
begin
  Result := AttributeText['ONKEYDOWN'];
end;

procedure AhtmlElementInclAttrs.SetOnKeyDown(const Value: String);
begin
  AttributeText['ONKEYDOWN'] := Value;
end;

function AhtmlElementInclAttrs.GetOnKeyUp: String;
begin
  Result := AttributeText['ONKEYUP'];
end;

procedure AhtmlElementInclAttrs.SetOnKeyUp(const Value: String);
begin
  AttributeText['ONKEYUP'] := Value;
end;

function AhtmlElementInclAttrs.GetOnMouseDown: String;
begin
  Result := AttributeText['ONMOUSEDOWN'];
end;

procedure AhtmlElementInclAttrs.SetOnMouseDown(const Value: String);
begin
  AttributeText['ONMOUSEDOWN'] := Value;
end;

function AhtmlElementInclAttrs.GetOnMouseUp: String;
begin
  Result := AttributeText['ONMOUSEUP'];
end;

procedure AhtmlElementInclAttrs.SetOnMouseUp(const Value: String);
begin
  AttributeText['ONMOUSEUP'] := Value;
end;

function AhtmlElementInclAttrs.GetOnMouseMove: String;
begin
  Result := AttributeText['ONMOUSEMOVE'];
end;

procedure AhtmlElementInclAttrs.SetOnMouseMove(const Value: String);
begin
  AttributeText['ONMOUSEMOVE'] := Value;
end;

function AhtmlElementInclAttrs.GetOnMouseOver: String;
begin
  Result := AttributeText['ONMOUSEOVER'];
end;

procedure AhtmlElementInclAttrs.SetOnMouseOver(const Value: String);
begin
  AttributeText['ONMOUSEOVER'] := Value;
end;

function AhtmlElementInclAttrs.GetOnMouseOut: String;
begin
  Result := AttributeText['ONMOUSEOUT'];
end;

procedure AhtmlElementInclAttrs.SetOnMouseOut(const Value: String);
begin
  AttributeText['ONMOUSEOUT'] := Value;
end;




{ ============================================================================ }
{ TEXT MARKUP                                                                  }
{ ============================================================================ }

{ AhtmlFontStyle                                                               }
constructor AhtmlFontStyle.Create(const TagID: ThtmlTagID;
    const FontStyles: ThtmlFontStyles);
begin
  inherited Create(TagID, ttContentTags);
  FFontStyles := FontStyles;
end;

{ ThtmlTT                                                                      }
constructor ThtmlTT.Create;
begin
  inherited Create(HTML_TAG_TT, [hfsTeleType]);
end;

{ ThtmlB                                                                       }
constructor ThtmlB.Create;
begin
  inherited Create(HTML_TAG_B, [hfsBold]);
end;

{ ThtmlI                                                                       }
constructor ThtmlI.Create;
begin
  inherited Create(HTML_TAG_I, [hfsItalic]);
end;

{ ThtmlU                                                                       }
constructor ThtmlU.Create;
begin
  inherited Create(HTML_TAG_U, [hfsUnderline]);
end;

{ ThtmlS                                                                       }
constructor ThtmlS.Create;
begin
  inherited Create(HTML_TAG_S, [hfsStrikeThrough]);
end;

{ ThtmlSTRIKE                                                                  }
constructor ThtmlSTRIKE.Create;
begin
  inherited Create(HTML_TAG_STRIKE, [hfsStrikeThrough]);
end;

{ ThtmlBIG                                                                     }
constructor ThtmlBIG.Create;
begin
  inherited Create(HTML_TAG_BIG, [hfsBig]);
end;

{ ThtmlSMALL                                                                   }
constructor ThtmlSMALL.Create;
begin
  inherited Create(HTML_TAG_SMALL, [hfsSmall]);
end;

{ AhtmlPhraseElement                                                           }
constructor AhtmlPhraseElement.Create(const TagID: ThtmlTagID;
    const PhraseElement: ThtmlPhraseElement);
begin
  inherited Create(TagID, ttContentTags);
  FPhraseElement := PhraseElement;
end;

{ ThtmlEM                                                                      }
constructor ThtmlEM.Create;
begin
  inherited Create(HTML_TAG_EM, peEmphasise);
end;

{ ThtmlSTRONG                                                                  }
constructor ThtmlSTRONG.Create;
begin
  inherited Create(HTML_TAG_STRONG, peStrong);
end;

{ ThtmlDFN                                                                     }
constructor ThtmlDFN.Create;
begin
  inherited Create(HTML_TAG_DFN, peDefiningInstance);
end;

{ ThtmlCODE                                                                    }
constructor ThtmlCODE.Create;
begin
  inherited Create(HTML_TAG_CODE, peCode);
end;

{ ThtmlSAMP                                                                    }
constructor ThtmlSAMP.Create;
begin
  inherited Create(HTML_TAG_SAMP, peSample);
end;

{ ThtmlKBD                                                                     }
constructor ThtmlKBD.Create;
begin
  inherited Create(HTML_TAG_KBD, peKeyboard);
end;

{ ThtmlVAR                                                                     }
constructor ThtmlVAR.Create;
begin
  inherited Create(HTML_TAG_VAR, peVariable);
end;

{ ThtmlCITE                                                                    }
constructor ThtmlCITE.Create;
begin
  inherited Create(HTML_TAG_CITE, peCitation);
end;

{ ThtmlABBR                                                                    }
constructor ThtmlABBR.Create;
begin
  inherited Create(HTML_TAG_ABBR, peAbbreviation);
end;

{ ThtmlACRONYM                                                                 }
constructor ThtmlACRONYM.Create;
begin
  inherited Create(HTML_TAG_ACRONYM, peAcronym);
end;

{ ThtmlSUB                                                                     }
constructor ThtmlSUB.Create;
begin
  inherited Create(HTML_TAG_SUB, ttContentTags);
end;

{ ThtmlSUP                                                                     }
constructor ThtmlSUP.Create;
begin
  inherited Create(HTML_TAG_SUP, ttContentTags);
end;

{ ThtmlSPAN                                                                    }
constructor ThtmlSPAN.Create;
begin
  inherited Create(HTML_TAG_SPAN, ttContentTags);
end;

{ ThtmlBDO                                                                     }
constructor ThtmlBDO.Create;
begin
  inherited Create(HTML_TAG_BDO, ttContentTags);
end;

{ ThtmlBASEFONT                                                                }
constructor ThtmlBASEFONT.Create;
begin
  inherited Create(HTML_TAG_BASEFONT, ttContentTags);
end;

function ThtmlBASEFONT.GetID: String;
begin
  Result := AttributeText['ID'];
end;

procedure ThtmlBASEFONT.SetID(const Value: String);
begin
  AttributeText['ID'] := Value;
end;

function ThtmlBASEFONT.GetColor: String;
begin
  Result := AttributeText['COLOR'];
end;

procedure ThtmlBASEFONT.SetColor(const Value: String);
begin
  AttributeText['COLOR'] := Value;
end;

function ThtmlBASEFONT.GetColorDelphi: TColor;
begin
  Result := htmlResolveColor(GetColor).Color;
end;

procedure ThtmlBASEFONT.SetColorDelphi(const Value: TColor);
begin
  SetColor(htmlEncodeRGBColor(htmlRGBColor(Value)));
end;

function ThtmlBASEFONT.GetSize: String;
begin
  Result := AttributeText['SIZE'];
end;

procedure ThtmlBASEFONT.SetSize(const Value: String);
begin
  AttributeText['SIZE'] := Value;
end;

function ThtmlBASEFONT.GetFace: String;
begin
  Result := AttributeText['FACE'];
end;

procedure ThtmlBASEFONT.SetFace(const Value: String);
begin
  AttributeText['FACE'] := Value;
end;

{ ThtmlFONT                                                                    }
constructor ThtmlFONT.Create;
begin
  inherited Create(HTML_TAG_FONT, ttContentTags);
end;

function ThtmlFONT.GetColor: String;
begin
  Result := AttributeText['COLOR'];
end;

procedure ThtmlFONT.SetColor(const Value: String);
begin
  AttributeText['COLOR'] := Value;
end;

function ThtmlFONT.GetColorRGB: LongWord;
begin
  Result := htmlResolveColor(GetColor).RGB;
end;

procedure ThtmlFONT.SetColorRGB(const Value: LongWord);
begin
  SetColor(htmlEncodeRGBColor(htmlRGBColor(Value)));
end;

function ThtmlFONT.GetColorDelphi: TColor;
begin
  Result := htmlResolveColor(GetColor).Color;
end;

procedure ThtmlFONT.SetColorDelphi(const Value: TColor);
begin
  SetColor(htmlEncodeRGBColor(htmlRGBColor(Value)));
end;

function ThtmlFONT.GetSize: String;
begin
  Result := AttributeText['SIZE'];
end;

procedure ThtmlFONT.SetSize(const Value: String);
begin
  AttributeText['SIZE'] := Value;
end;

function ThtmlFONT.GetSizeValue: ThtmlFontSize;
begin
  Result := htmlDecodeFontSize(GetSize);
end;

function ThtmlFONT.GetFaceStr: String;
begin
  Result := AttributeText['FACE'];
end;

procedure ThtmlFONT.SetFaceStr(const Value: String);
begin
  AttributeText['FACE'] := Value;
end;

procedure ThtmlFONT.ApplyHTMLStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
    const ParentStyle: ThtmlcssStyleProperties);
var S: String;
    Size: ThtmlFontSize;
begin
  S := GetFaceStr;
  if S <> '' then
    StyleInfo.FontFamily := htmlDecodeFontFamilyList(S);
  Size := GetSizeValue;
  if Size.SizeType <> fsizeDefault then
    begin
      htmlResolveRelativeFontSize(Size, ParentStyle.FontSize);
      StyleInfo.FontSize := Size;
    end;
  S := GetColor;
  if S <> '' then
    StyleInfo.FontColor := htmlDecodeColor(S);
end;

{ ThtmlBR                                                                      }
constructor ThtmlBR.Create;
begin
  inherited Create(HTML_TAG_BR, ttStartTag);
end;

function ThtmlBR.GetClear: ThtmlClearType;
begin
  Result := htmlDecodeClearType(AttributeText['CLEAR']);
end;

procedure ThtmlBR.SetClear(const Value: ThtmlClearType);
begin
  AttributeText['CLEAR'] := htmlEncodeBRClearType(Value);
end;

function ThtmlBR.GetHTMLText: String;
begin
  Result := '<BR>';
end;

{ ============================================================================ }
{ DOCUMENT BODY                                                                }
{ ============================================================================ }

{ ThtmlBODY                                                                    }
constructor ThtmlBODY.Create;
begin
  inherited Create(HTML_TAG_BODY, ttContentTags);
end;

function ThtmlBODY.GetOnLoad: String;
begin
  Result := AttributeText['ONLOAD'];
end;

procedure ThtmlBODY.SetOnLoad(const Value: String);
begin
  AttributeText['ONLOAD'] := Value;
end;

function ThtmlBODY.GetOnUnload: String;
begin
  Result := AttributeText['ONUNLOAD'];
end;

procedure ThtmlBODY.SetOnUnload(const Value: String);
begin
  AttributeText['ONUNLOAD'] := Value;
end;

function ThtmlBODY.GetBackground: String;
begin
  Result := AttributeText['BACKGROUND'];
end;

procedure ThtmlBODY.SetBackground(const Value: String);
begin
  AttributeText['BACKGROUND'] := Value;
end;

function ThtmlBODY.GetBackColor: String;
begin
  Result := AttributeText['BGCOLOR'];
end;

procedure ThtmlBODY.SetBackColor(const Value: String);
begin
  AttributeText['BGCOLOR'] := Value;
end;

function ThtmlBODY.GetBackColorDelphi: TColor;
begin
  Result := htmlResolveColor(GetBackColor).Color;
end;

function ThtmlBODY.GetTextColor: String;
begin
  Result := AttributeText['TEXT'];
end;

procedure ThtmlBODY.SetTextColor(const Value: String);
begin
  AttributeText['TEXT'] := Value;
end;

function ThtmlBODY.GetLinkColor: String;
begin
  Result := AttributeText['LINK'];
end;

procedure ThtmlBODY.SetLinkColor(const Value: String);
begin
  AttributeText['LINK'] := Value;
end;

function ThtmlBODY.GetVisitedLinkColor: String;
begin
  Result := AttributeText['VLINK'];
end;

procedure ThtmlBODY.SetVisitedLinkColor(const Value: String);
begin
  AttributeText['VLINK'] := Value;
end;

function ThtmlBODY.GetSelectedLinkColor: String;
begin
  Result := AttributeText['ALINK'];
end;

procedure ThtmlBODY.SetSelectedLinkColor(const Value: String);
begin
  AttributeText['ALINK'] := Value;
end;

procedure ThtmlBODY.ApplyHTMLStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
          const ParentStyle: ThtmlcssStyleProperties);
var S: String;
begin
  inherited;
  S := GetBackColor;
  if S <> '' then
    StyleInfo.BackColor := htmlDecodeColor(S);
end;

procedure ThtmlBODY.PrepareStructure;
begin
end;

function ThtmlBODY.StyleRefs: StringArray;
var L : ThtmlLINK;
begin
  Result := nil;
  L := ThtmlLINK(FindNext(nil, ThtmlLINK));
  while Assigned(L) do
    begin
      if StrEqualNoAsciiCaseU(L.Rel, 'stylesheet') then
        DynArrayAppend(Result, L.GetHRef);
      L := ThtmlLINK(FindNext(L, ThtmlLINK));
    end;
end;

{ ThtmlADDRESS                                                                 }
constructor ThtmlADDRESS.Create;
begin
  inherited Create(HTML_TAG_ADDRESS, ttContentTags);
end;

{ AhtmlElementInclAttrsAndAlign                                                }
function AhtmlElementInclAttrsAndAlign.GetAlign: ThtmlTextAlignType;
begin
  Result := htmlDecodeTextAlignType(AttributeText['ALIGN']);
end;

procedure AhtmlElementInclAttrsAndAlign.SetAlign(const Value: ThtmlTextAlignType);
begin
  AttributeText['ALIGN'] := htmlEncodeTextAlignType(Value);
end;

{ ThtmlDIV                                                                     }
constructor ThtmlDIV.Create;
begin
  inherited Create(HTML_TAG_DIV, ttContentTags);
end;

{ ThtmlCENTER                                                                  }
constructor ThtmlCENTER.Create;
begin
  inherited Create(HTML_TAG_CENTER, ttContentTags);
end;

{ ============================================================================ }
{ ANCHOR ELEMENT                                                               }
{ ============================================================================ }

{ ThtmlA                                                                       }
constructor ThtmlA.Create;
begin
  inherited Create(HTML_TAG_A, ttContentTags);
end;

function ThtmlA.GetContentType: String;
begin
  Result := AttributeText['TYPE'];
end;

procedure ThtmlA.SetContentType(const Value: String);
begin
  AttributeText['TYPE'] := Value;
end;

function ThtmlA.GetNameAttr: String;
begin
  Result := AttributeText['NAME'];
end;

procedure ThtmlA.SetNameAttr(const Value: String);
begin
  AttributeText['NAME'] := Value;
end;

function ThtmlA.GetHRef: String;
begin
  Result := AttributeText['HREF'];
end;

procedure ThtmlA.SetHRef(const Value: String);
begin
  AttributeText['HREF'] := Value;
end;

function ThtmlA.GetTarget: String;
begin
  Result := AttributeText['TARGET'];
end;

procedure ThtmlA.SetTarget(const Value: String);
begin
  AttributeText['TARGET'] := Value;
end;

function ThtmlA.GetRel: String;
begin
  Result := AttributeText['REL'];
end;

procedure ThtmlA.SetRel(const Value: String);
begin
  AttributeText['REL'] := Value;
end;

function ThtmlA.GetRev: String;
begin
  Result := AttributeText['REV'];
end;

procedure ThtmlA.SetRev(const Value: String);
begin
  AttributeText['REV'] := Value;
end;

function ThtmlA.GetTabIndex: String;
begin
  Result := AttributeText['TABINDEX'];
end;

procedure ThtmlA.SetTabIndex(const Value: String);
begin
  AttributeText['TABINDEX'] := Value;
end;

function ThtmlA.GetOnFocus: String;
begin
  Result := AttributeText['ONFOCUS'];
end;

procedure ThtmlA.SetOnFocus(const Value: String);
begin
  AttributeText['ONFOCUS'] := Value;
end;

function ThtmlA.GetOnBlur: String;
begin
  Result := AttributeText['ONBLUR'];
end;

procedure ThtmlA.SetOnBlur(const Value: String);
begin
  AttributeText['ONBLUR'] := Value;
end;

procedure ThtmlA.InitStyleElementInfo(const StyleSheet: ThtmlCSS;
    const ParentInfo: PhtmlcssElementInfo);
begin
  FIsLink := GetHRef <> '';
  inherited;
end;

procedure ThtmlA.InitStyleInfo(const StyleSheet: ThtmlCSS;
    const StyleInfo: ThtmlcssStyleProperties);
var State: ThtmlcssAnchorPseudoPropertyState;
begin
  if not FIsLink then
    begin
      // init style with no pseudo properties
      FStyleInfo.PseudoIDs := [];
      inherited InitStyleInfo(StyleSheet, StyleInfo);
      exit;
    end;
  // init state styles
  for State := Low(State) to High(State) do
    begin
      FStyleInfo.PseudoIDs := HTML_CSS_PP_Properties_AnchorState[State];
      FStateStyles[State] := StyleInfo;
      InitElementStyleInfo(StyleSheet, FStateStyles[State], StyleInfo);
    end;
  // init style with Link state
  FStyleInfo.PseudoIDs := HTML_CSS_PP_Properties_AnchorState[anchorLink];
  FStyle := FStateStyles[anchorLink];
  InitChildrenStyleInfo(StyleSheet, FStyle);
end;

{ ============================================================================ }
{ CLIENT-SIDE IMAGE MAPS                                                       }
{ ============================================================================ }

{ ThtmlMAP                                                                     }
constructor ThtmlMAP.Create;
begin
  inherited Create(HTML_TAG_MAP, ttContentTags);
end;

function ThtmlMAP.GetNameAttr: String;
begin
  Result := AttributeText['NAME'];
end;

procedure ThtmlMAP.SetNameAttr(const Value: String);
begin
  AttributeText['NAME'] := Value;
end;

{ ThtmlAREA                                                                    }
constructor ThtmlAREA.Create;
begin
  inherited Create(HTML_TAG_AREA, ttStartTag);
end;

function ThtmlAREA.GetShape: String;
begin
  Result := AttributeText['SHAPE'];
end;

procedure ThtmlAREA.SetShape(const Value: String);
begin
  AttributeText['SHAPE'] := Value;
end;

function ThtmlAREA.GetCoords: String;
begin
  Result := AttributeText['COORDS'];
end;

procedure ThtmlAREA.SetCoords(const Value: String);
begin
  AttributeText['COORDS'] := Value;
end;

function ThtmlAREA.GetHRef: String;
begin
  Result := AttributeText['HREF'];
end;

procedure ThtmlAREA.SetHRef(const Value: String);
begin
  AttributeText['HREF'] := Value;
end;

function ThtmlAREA.GetTarget: String;
begin
  Result := AttributeText['TARGET'];
end;

procedure ThtmlAREA.SetTarget(const Value: String);
begin
  AttributeText['TARGET'] := Value;
end;

function ThtmlAREA.GetNoHRef: Boolean;
begin
  Result := AttributeFlag['NOHREF'];
end;

procedure ThtmlAREA.SetNoHRef(const Value: Boolean);
begin
  AttributeFlag['NOHREF'] := Value;
end;

function ThtmlAREA.GetAlt: String;
begin
  Result := AttributeText['ALT'];
end;

procedure ThtmlAREA.SetAlt(const Value: String);
begin
  AttributeText['ALT'] := Value;
end;

function ThtmlAREA.GetTabIndex: String;
begin
  Result := AttributeText['TABINDEX'];
end;

procedure ThtmlAREA.SetTabIndex(const Value: String);
begin
  AttributeText['TABINDEX'] := Value;
end;

function ThtmlAREA.GetOnFocus: String;
begin
  Result := AttributeText['ONFOCUS'];
end;

procedure ThtmlAREA.SetOnFocus(const Value: String);
begin
  AttributeText['ONFOCUS'] := Value;
end;

function ThtmlAREA.GetOnBlur: String;
begin
  Result := AttributeText['ONBLUR'];
end;

procedure ThtmlAREA.SetOnBlur(const Value: String);
begin
  AttributeText['ONBLUR'] := Value;
end;

{ ============================================================================ }
{ LINK ELEMENT                                                                 }
{ ============================================================================ }

{ ThtmlLINK                                                                    }
constructor ThtmlLINK.Create;
begin
  inherited Create(HTML_TAG_LINK, ttStartTag);
end;

function ThtmlLINK.GetHRef: String;
begin
  Result := AttributeText['HREF'];
end;

procedure ThtmlLINK.SetHRef(const Value: String);
begin
  AttributeText['HREF'] := Value;
end;

function ThtmlLINK.GetContentType: String;
begin
  Result := AttributeText['TYPE'];
end;

procedure ThtmlLINK.SetContentType(const Value: String);
begin
  AttributeText['TYPE'] := Value;
end;

function ThtmlLINK.GetRel: String;
begin
  Result := AttributeText['REL'];
end;

procedure ThtmlLINK.SetRel(const Value: String);
begin
  AttributeText['REL'] := Value;
end;

function ThtmlLINK.GetRev: String;
begin
  Result := AttributeText['REV'];
end;

procedure ThtmlLINK.SetRev(const Value: String);
begin
  AttributeText['REV'] := Value;
end;

function ThtmlLINK.GetMedia: String;
begin
  Result := AttributeText['MEDIA'];
end;

procedure ThtmlLINK.SetMedia(const Value: String);
begin
  AttributeText['MEDIA'] := Value;
end;

function ThtmlLINK.GetTarget: String;
begin
  Result := AttributeText['TARGET'];
end;

procedure ThtmlLINK.SetTarget(const Value: String);
begin
  AttributeText['TARGET'] := Value;
end;

{ ============================================================================ }
{ IMAGES                                                                       }
{ ============================================================================ }

{ AhtmlImageElement                                                            }
function AhtmlImageElement.GetNameAttr: String;
begin
  Result := AttributeText['NAME'];
end;

procedure AhtmlImageElement.SetNameAttr(const Value: String);
begin
  AttributeText['NAME'] := Value;
end;

function AhtmlImageElement.GetAlign: ThtmlIAlignType;
begin
  Result := htmlDecodeIAlignType(AttributeText['ALIGN']);
end;

procedure AhtmlImageElement.SetAlign(const Value: ThtmlIAlignType);
begin
  AttributeText['ALIGN'] := htmlEncodeIAlignType(Value);
end;

function AhtmlImageElement.GetWidthStr: String;
begin
  Result := AttributeText['WIDTH'];
end;

procedure AhtmlImageElement.SetWidthStr(const Value: String);
begin
  AttributeText['WIDTH'] := Value;
end;

function AhtmlImageElement.GetHeightStr: String;
begin
  Result := AttributeText['HEIGHT'];
end;

procedure AhtmlImageElement.SetHeightStr(const Value: String);
begin
  AttributeText['HEIGHT'] := Value;
end;

function AhtmlImageElement.GetWidth: ThtmlLength;
begin
  Result := htmlDecodeLength(GetWidthStr);
end;

function AhtmlImageElement.GetHeight: ThtmlLength;
begin
  Result := htmlDecodeLength(GetHeightStr);
end;

function AhtmlImageElement.GetHSpace: Integer;
begin
  Result := htmlDecodeInteger(AttributeText['HSPACE'], 0);
end;

procedure AhtmlImageElement.SetHSpace(const Value: Integer);
begin
  AttributeText['HSPACE'] := htmlEncodeInteger(Value);
end;

function AhtmlImageElement.GetVSpace: Integer;
begin
  Result := htmlDecodeInteger(AttributeText['VSPACE'], 0);
end;

procedure AhtmlImageElement.SetVSpace(const Value: Integer);
begin
  AttributeText['VSPACE'] := htmlEncodeInteger(Value);
end;

function AhtmlImageElement.GetUseMap: String;
begin
  Result := AttributeText['USEMAP'];
end;

procedure AhtmlImageElement.SetUseMap(const Value: String);
begin
  AttributeText['USEMAP'] := Value;
end;

procedure AhtmlImageElement.ApplyHTMLStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
    const ParentStyle: ThtmlcssStyleProperties);
var S: String;
begin
  S := GetWidthStr;
  if S <> '' then
    StyleInfo.Width := htmlDecodeLength(S);
  S := GetHeightStr;
  if S <> '' then
    StyleInfo.Height := htmlDecodeLength(S);
end;

{ ThtmlIMG                                                                     }
constructor ThtmlIMG.Create;
begin
  inherited Create(HTML_TAG_IMG, ttStartTag);
end;

function ThtmlIMG.GetSrc: String;
begin
  Result := AttributeText['SRC'];
end;

procedure ThtmlIMG.SetSrc(const Value: String);
begin
  AttributeText['SRC'] := Value;
end;

function ThtmlIMG.GetAlt: String;
begin
  Result := AttributeText['ALT'];
end;

procedure ThtmlIMG.SetAlt(const Value: String);
begin
  AttributeText['ALT'] := Value;
end;

function ThtmlIMG.GetLongDesc: String;
begin
  Result := AttributeText['LONGDESC'];
end;

procedure ThtmlIMG.SetLongDesc(const Value: String);
begin
  AttributeText['LONGDESC'] := Value;
end;

function ThtmlIMG.GetIsMap: Boolean;
begin
  Result := AttributeFlag['ISMAP'];
end;

procedure ThtmlIMG.SetIsMap(const Value: Boolean);
begin
  AttributeFlag['ISMAP'] := Value;
end;

function ThtmlIMG.GetBorder: String;
begin
  Result := AttributeText['BORDER'];
end;

procedure ThtmlIMG.SetBorder(const Value: String);
begin
  AttributeText['BORDER'] := Value;
end;

{ ============================================================================ }
{ EMBEDDED OBJECT                                                              }
{ ============================================================================ }

{ ThtmlEmbeddedOBJECT                                                          }
constructor ThtmlEmbeddedOBJECT.Create;
begin
  inherited Create(HTML_TAG_OBJECT, ttContentTags);
end;

function ThtmlEmbeddedOBJECT.CreateItem(const ID: Integer; const Name: String): AhtmlObject;
begin
  if ThtmlTagID(ID) = HTML_TAG_PARAM then
    Result := ThtmlEmbeddedObjectPARAM.Create
  else
    Result := inherited CreateItem(ID, Name);
end;

function ThtmlEmbeddedOBJECT.GetDeclare: Boolean;
begin
  Result := AttributeFlag['DECLARE'];
end;

procedure ThtmlEmbeddedOBJECT.SetDeclare(const Value: Boolean);
begin
  AttributeFlag['DECLARE'] := Value;
end;

function ThtmlEmbeddedOBJECT.GetClassID: String;
begin
  Result := AttributeText['CLASSID'];
end;

procedure ThtmlEmbeddedOBJECT.SetClassID(const Value: String);
begin
  AttributeText['CLASSID'] := Value;
end;

function ThtmlEmbeddedOBJECT.GetCodeBase: String;
begin
  Result := AttributeText['CODEBASE'];
end;

procedure ThtmlEmbeddedOBJECT.SetCodeBase(const Value: String);
begin
  AttributeText['CODEBASE'] := Value;
end;

function ThtmlEmbeddedOBJECT.GetData: String;
begin
  Result := AttributeText['DATA'];
end;

procedure ThtmlEmbeddedOBJECT.SetData(const Value: String);
begin
  AttributeText['DATA'] := Value;
end;

function ThtmlEmbeddedOBJECT.GetContentType: String;
begin
  Result := AttributeText['TYPE'];
end;

procedure ThtmlEmbeddedOBJECT.SetContentType(const Value: String);
begin
  AttributeText['TYPE'] := Value;
end;

function ThtmlEmbeddedOBJECT.GetCodeType: String;
begin
  Result := AttributeText['CODETYPE'];
end;

procedure ThtmlEmbeddedOBJECT.SetCodeType(const Value: String);
begin
  AttributeText['CODETYPE'] := Value;
end;

function ThtmlEmbeddedOBJECT.GetStandBy: String;
begin
  Result := AttributeText['STANDBY'];
end;

procedure ThtmlEmbeddedOBJECT.SetStandBy(const Value: String);
begin
  AttributeText['STANDBY'] := Value;
end;

function ThtmlEmbeddedOBJECT.GetTabIndex: String;
begin
  Result := AttributeText['TABINDEX'];
end;

procedure ThtmlEmbeddedOBJECT.SetTabIndex(const Value: String);
begin
  AttributeText['TABINDEX'] := Value;
end;

function ThtmlEmbeddedOBJECT.GetBorder: Integer;
begin
  Result := htmlDecodeInteger(AttributeText['BORDER'], 0);
end;

procedure ThtmlEmbeddedOBJECT.SetBorder(const Value: Integer);
begin
  AttributeText['BORDER'] := htmlEncodeInteger(Value);
end;

{ ThtmlEmbeddedObjectPARAM                                                     }
constructor ThtmlEmbeddedObjectPARAM.Create;
begin
  inherited Create(HTML_TAG_PARAM, ttStartTag);
end;

function ThtmlEmbeddedObjectPARAM.GetID: String;
begin
  Result := AttributeText['ID'];
end;

procedure ThtmlEmbeddedObjectPARAM.SetID(const Value: String);
begin
  AttributeText['ID'] := Value;
end;

function ThtmlEmbeddedObjectPARAM.GetNameAttr: String;
begin
  Result := AttributeText['NAME'];
end;

procedure ThtmlEmbeddedObjectPARAM.SetNameAttr(const Value: String);
begin
  AttributeText['NAME'] := Value;
end;

function ThtmlEmbeddedObjectPARAM.GetValue: String;
begin
  Result := AttributeText['VALUE'];
end;

procedure ThtmlEmbeddedObjectPARAM.SetValue(const Value: String);
begin
  AttributeText['VALUE'] := Value;
end;

function ThtmlEmbeddedObjectPARAM.GetValueType: String;
begin
  Result := AttributeText['VALUETYPE'];
end;

procedure ThtmlEmbeddedObjectPARAM.SetValueType(const Value: String);
begin
  AttributeText['VALUETYPE'] := Value;
end;

function ThtmlEmbeddedObjectPARAM.GetContentType: String;
begin
  Result := AttributeText['TYPE'];
end;

procedure ThtmlEmbeddedObjectPARAM.SetContentType(const Value: String);
begin
  AttributeText['TYPE'] := Value;
end;

{ ============================================================================ }
{ APPLET                                                                       }
{ ============================================================================ }

{ ThtmlAPPLET                                                                  }
constructor ThtmlAPPLET.Create;
begin
  inherited Create(HTML_TAG_APPLET, ttContentTags);
end;

function ThtmlAPPLET.GetAlt: String;
begin
  Result := AttributeText['ALT'];
end;

procedure ThtmlAPPLET.SetAlt(const Value: String);
begin
  AttributeText['ALT'] := Value;
end;

function ThtmlAPPLET.GetNameAttr: String;
begin
  Result := AttributeText['NAME'];
end;

procedure ThtmlAPPLET.SetNameAttr(const Value: String);
begin
  AttributeText['NAME'] := Value;
end;

function ThtmlAPPLET.GetCode: String;
begin
  Result := AttributeText['CODE'];
end;

procedure ThtmlAPPLET.SetCode(const Value: String);
begin
  AttributeText['CODE'] := Value;
end;

function ThtmlAPPLET.GetSerObject: String;
begin
  Result := AttributeText['OBJECT'];
end;

procedure ThtmlAPPLET.SetSerObject(const Value: String);
begin
  AttributeText['OBJECT'] := Value;
end;

function ThtmlAPPLET.GetCodeBase: String;
begin
  Result := AttributeText['CODEBASE'];
end;

procedure ThtmlAPPLET.SetCodeBase(const Value: String);
begin
  AttributeText['CODEBASE'] := Value;
end;

function ThtmlAPPLET.GetAlign: ThtmlIAlignType;
begin
  Result := htmlDecodeIAlignType(AttributeText['ALIGN']);
end;

procedure ThtmlAPPLET.SetAlign(const Value: ThtmlIAlignType);
begin
  AttributeText['ALIGN'] := htmlEncodeIAlignType(Value);
end;

function ThtmlAPPLET.GetWidth: String;
begin
  Result := AttributeText['WIDTH'];
end;

procedure ThtmlAPPLET.SetWidth(const Value: String);
begin
  AttributeText['WIDTH'] := Value;
end;

function ThtmlAPPLET.GetHeight: String;
begin
  Result := AttributeText['HEIGHT'];
end;

procedure ThtmlAPPLET.SetHeight(const Value: String);
begin
  AttributeText['HEIGHT'] := Value;
end;

function ThtmlAPPLET.GetHSpace: Integer;
begin
  Result := htmlDecodeInteger(AttributeText['HSPACE'], 0);
end;

procedure ThtmlAPPLET.SetHSpace(const Value: Integer);
begin
  AttributeText['HSPACE'] := htmlEncodeInteger(Value);
end;

function ThtmlAPPLET.GetVSpace: Integer;
begin
  Result := htmlDecodeInteger(AttributeText['VSPACE'], 0);
end;

procedure ThtmlAPPLET.SetVSpace(const Value: Integer);
begin
  AttributeText['VSPACE'] := htmlEncodeInteger(Value);
end;

{ ============================================================================ }
{ HORISONTAL RULE                                                              }
{ ============================================================================ }

{ ThtmlHR                                                                      }
constructor ThtmlHR.Create;
begin
  inherited Create(HTML_TAG_HR, ttStartTag);
end;

function ThtmlHR.GetAlign: String;
begin
  Result := AttributeText['ALIGN'];
end;

procedure ThtmlHR.SetAlign(const Value: String);
begin
  AttributeText['ALIGN'] := Value;
end;

function ThtmlHR.GetSize: Integer;
begin
  Result := htmlDecodeInteger(AttributeText['SIZE'], -1);
end;

procedure ThtmlHR.SetSize(const Value: Integer);
begin
  AttributeText['SIZE'] := htmlEncodeInteger(Value);
end;

function ThtmlHR.GetWidth: String;
begin
  Result := AttributeText['WIDTH'];
end;

procedure ThtmlHR.SetWidth(const Value: String);
begin
  AttributeText['WIDTH'] := Value;
end;

function ThtmlHR.GetNoShade: Boolean;
begin
  Result := AttributeFlag['NOSHADE'];
end;

procedure ThtmlHR.SetNoShade(const Value: Boolean);
begin
  AttributeFlag['NOSHADE'] := Value;
end;

{ ============================================================================ }
{ PARAGRAPHS                                                                   }
{ ============================================================================ }

{ ThtmlP                                                                       }
constructor ThtmlP.Create;
begin
  inherited Create(HTML_TAG_P, ttContentTags);
end;

{ ============================================================================ }
{ HEADINGS                                                                     }
{ ============================================================================ }

{ AhtmlHeadingTag                                                              }
constructor AhtmlHeadingTag.Create(const TagID: ThtmlTagID);
begin
  inherited Create(TagID, ttContentTags);
end;

{ ThtmlH1                                                                      }
constructor ThtmlH1.Create;
begin
  inherited Create(HTML_TAG_H1);
end;

{ ThtmlH2                                                                      }
constructor ThtmlH2.Create;
begin
  inherited Create(HTML_TAG_H2);
end;

{ ThtmlH3                                                                      }
constructor ThtmlH3.Create;
begin
  inherited Create(HTML_TAG_H3);
end;

{ ThtmlH4                                                                      }
constructor ThtmlH4.Create;
begin
  inherited Create(HTML_TAG_H4);
end;

{ ThtmlH5                                                                      }
constructor ThtmlH5.Create;
begin
  inherited Create(HTML_TAG_H5);
end;

{ ThtmlH6                                                                      }
constructor ThtmlH6.Create;
begin
  inherited Create(HTML_TAG_H6);
end;

{ ============================================================================ }
{ PREFORMATTED TEXT                                                            }
{ ============================================================================ }

{ ThtmlPRE                                                                     }
constructor ThtmlPRE.Create;
begin
  inherited Create(HTML_TAG_PRE, ttContentTags);
end;

function ThtmlPRE.GetWidth: Integer;
begin
  Result := htmlDecodeInteger(AttributeText['WIDTH'], -1);
end;

procedure ThtmlPRE.SetWidth(const Value: Integer);
begin
  AttributeText['WIDTH'] := htmlEncodeInteger(Value);
end;

procedure ThtmlPRE.ApplyHTMLStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
          const ParentStyle: ThtmlcssStyleProperties);
begin
  StyleInfo.WhiteSpace := wsPre;
end;

{ ============================================================================ }
{ QUOTES                                                                       }
{ ============================================================================ }

{ ThtmlQ                                                                       }
constructor ThtmlQ.Create;
begin
  inherited Create(HTML_TAG_Q, ttContentTags);
end;

function ThtmlQ.GetCite: String;
begin
  Result := AttributeText['CITE'];
end;

procedure ThtmlQ.SetCite(const Value: String);
begin
  AttributeText['CITE'] := Value;
end;

{ ThtmlBLOCKQUOTE                                                              }
constructor ThtmlBLOCKQUOTE.Create;
begin
  inherited Create(HTML_TAG_BLOCKQUOTE, ttContentTags);
end;

function ThtmlBLOCKQUOTE.GetCite: String;
begin
  Result := AttributeText['CITE'];
end;

procedure ThtmlBLOCKQUOTE.SetCite(const Value: String);
begin
  AttributeText['CITE'] := Value;
end;

{ ============================================================================ }
{ INSERTED/DELETED TEXT                                                        }
{ ============================================================================ }

{ AhtmlVersionElement                                                          }
function AhtmlVersionElement.GetCite: String;
begin
  Result := AttributeText['CITE'];
end;

procedure AhtmlVersionElement.SetCite(const Value: String);
begin
  AttributeText['CITE'] := Value;
end;

function AhtmlVersionElement.GetDateTime: String;
begin
  Result := AttributeText['DATETIME'];
end;

procedure AhtmlVersionElement.SetDateTime(const Value: String);
begin
  AttributeText['DATETIME'] := Value;
end;

{ ThtmlINS                                                                     }
constructor ThtmlINS.Create;
begin
  inherited Create(HTML_TAG_INS, ttContentTags);
end;

{ ThtmlDEL                                                                     }
constructor ThtmlDEL.Create;
begin
  inherited Create(HTML_TAG_DEL, ttContentTags);
end;

{ ============================================================================ }
{ LISTS                                                                        }
{ ============================================================================ }

{ ThtmlDL                                                                      }
constructor ThtmlDL.Create;
begin
  inherited Create(HTML_TAG_DL, ttContentTags);
end;

function ThtmlDL.CreateItem(const ID: Integer; const Name: String): AhtmlObject;
begin
  if ThtmlTagID(ID) = HTML_TAG_DT then
    Result := ThtmlDT.Create else
  if ThtmlTagID(ID) = HTML_TAG_DD then
    Result := ThtmlDD.Create
  else
    Result := inherited CreateItem(ID, Name);
end;

function ThtmlDL.GetCompact: Boolean;
begin
  Result := AttributeFlag['COMPACT'];
end;

procedure ThtmlDL.SetCompact(const Value: Boolean);
begin
  AttributeFlag['COMPACT'] := Value;
end;

{ ThtmlDT                                                                      }
constructor ThtmlDT.Create;
begin
  inherited Create(HTML_TAG_DT, ttContentTags);
end;

{ ThtmlDD                                                                      }
constructor ThtmlDD.Create;
begin
  inherited Create(HTML_TAG_DD, ttContentTags);
end;

{ AhtmlListItemContainer                                                       }
constructor AhtmlListItemContainer.Create(const TagID: ThtmlTagID);
begin
  inherited Create(TagID, ttContentTags);
end;

function AhtmlListItemContainer.CreateItem(const ID: Integer; const Name: String): AhtmlObject;
begin
  if ThtmlTagID(ID) = HTML_TAG_LI then
    Result := ThtmlLI.Create
  else
    Result := inherited CreateItem(ID, Name);
end;

function AhtmlListItemContainer.GetCompact: Boolean;
begin
  Result := AttributeFlag['COMPACT'];
end;

procedure AhtmlListItemContainer.SetCompact(const Value: Boolean);
begin
  AttributeFlag['COMPACT'] := Value;
end;

{ ThtmlOL                                                                      }
constructor ThtmlOL.Create;
begin
  inherited Create(HTML_TAG_OL);
end;

function ThtmlOL.GetListType: String;
begin
  Result := AttributeText['TYPE'];
end;

procedure ThtmlOL.SetListType(const Value: String);
begin
  AttributeText['TYPE'] := Value;
end;

function ThtmlOL.GetStart: Integer;
begin
  Result := htmlDecodeInteger(AttributeText['START'], -1);
end;

procedure ThtmlOL.SetStart(const Value: Integer);
begin
  AttributeText['START'] := htmlEncodeInteger(Value);
end;

{ ThtmlUL                                                                      }
constructor ThtmlUL.Create;
begin
  inherited Create(HTML_TAG_UL);
end;

function ThtmlUL.GetListType: String;
begin
  Result := AttributeText['TYPE'];
end;

procedure ThtmlUL.SetListType(const Value: String);
begin
  AttributeText['TYPE'] := Value;
end;

{ ThtmlDIR                                                                     }
constructor ThtmlDIR.Create;
begin
  inherited Create(HTML_TAG_DIR);
end;

{ ThtmlMENU                                                                    }
constructor ThtmlMENU.Create;
begin
  inherited Create(HTML_TAG_MENU);
end;

{ ThtmlLI                                                                      }
constructor ThtmlLI.Create;
begin
  inherited Create(HTML_TAG_LI, ttStartTag);
end;

function ThtmlLI.GetItemType: String;
begin
  Result := AttributeText['TYPE'];
end;

procedure ThtmlLI.SetItemType(const Value: String);
begin
  AttributeText['TYPE'] := Value;
end;

function ThtmlLI.GetValue: Integer;
begin
  Result := htmlDecodeInteger(AttributeText['VALUE'], -1);
end;

procedure ThtmlLI.SetValue(const Value: Integer);
begin
  AttributeText['VALUE'] := htmlEncodeInteger(Value);
end;

{ ============================================================================ }
{ FORMS                                                                        }
{ ============================================================================ }

{ ThtmlFORM                                                                    }
constructor ThtmlFORM.Create;
begin
  inherited Create(HTML_TAG_FORM, ttContentTags);
end;

function ThtmlFORM.CreateItem(const ID: Integer; const Name: String): AhtmlObject;
begin
  if ThtmlTagID(ID) = HTML_TAG_LABEL then
    Result := ThtmlLABEL.Create else
  if ThtmlTagID(ID) = HTML_TAG_INPUT then
    Result := ThtmlINPUT.Create else
  if ThtmlTagID(ID) = HTML_TAG_SELECT then
    Result := ThtmlSELECT.Create else
  if ThtmlTagID(ID) = HTML_TAG_TEXTAREA then
    Result := ThtmlTEXTAREA.Create else
  if ThtmlTagID(ID) = HTML_TAG_FIELDSET then
    Result := ThtmlFIELDSET.Create else
  if ThtmlTagID(ID) = HTML_TAG_LEGEND then
    Result := ThtmlLEGEND.Create else
  if ThtmlTagID(ID) = HTML_TAG_BUTTON then
    Result := ThtmlBUTTON.Create
  else
    Result := inherited CreateItem(ID, Name);
end;

function ThtmlFORM.GetAction: String;
begin
  Result := AttributeText['ACTION'];
end;

procedure ThtmlFORM.SetAction(const Value: String);
begin
  AttributeText['ACTION'] := Value;
end;

function ThtmlFORM.GetMethod: String;
begin
  Result := AttributeText['METHOD'];
end;

procedure ThtmlFORM.SetMethod(const Value: String);
begin
  AttributeText['METHOD'] := Value;
end;

function ThtmlFORM.GetNameAttr: String;
begin
  Result := AttributeText['NAME'];
end;

procedure ThtmlFORM.SetNameAttr(const Value: String);
begin
  AttributeText['NAME'] := Value;
end;

function ThtmlFORM.GetOnSubmit: String;
begin
  Result := AttributeText['ONSUBMIT'];
end;

procedure ThtmlFORM.SetOnSubmit(const Value: String);
begin
  AttributeText['ONSUBMIT'] := Value;
end;

function ThtmlFORM.GetOnReset: String;
begin
  Result := AttributeText['ONRESET'];
end;

procedure ThtmlFORM.SetOnReset(const Value: String);
begin
  AttributeText['ONRESET'] := Value;
end;

function ThtmlFORM.GetTarget: String;
begin
  Result := AttributeText['TARGET'];
end;

procedure ThtmlFORM.SetTarget(const Value: String);
begin
  AttributeText['TARGET'] := Value;
end;

{ ThtmlLABEL                                                                   }
constructor ThtmlLABEL.Create;
begin
  inherited Create(HTML_TAG_LABEL, ttContentTags);
end;

function ThtmlLABEL.GetForID: String;
begin
  Result := AttributeText['FOR'];
end;

procedure ThtmlLABEL.SetForID(const Value: String);
begin
  AttributeText['FOR'] := Value;
end;

function ThtmlLABEL.GetOnFocus: String;
begin
  Result := AttributeText['ONFOCUS'];
end;

procedure ThtmlLABEL.SetOnFocus(const Value: String);
begin
  AttributeText['ONFOCUS'] := Value;
end;

function ThtmlLABEL.GetOnBlur: String;
begin
  Result := AttributeText['ONBLUR'];
end;

procedure ThtmlLABEL.SetOnBlur(const Value: String);
begin
  AttributeText['ONBLUR'] := Value;
end;

{ AhtmlTextInputElement                                                        }
function AhtmlTextInputElement.GetNameAttr: String;
begin
  Result := AttributeText['NAME'];
end;

procedure AhtmlTextInputElement.SetNameAttr(const Value: String);
begin
  AttributeText['NAME'] := Value;
end;

function AhtmlTextInputElement.GetDisabled: Boolean;
begin
  Result := AttributeFlag['DISABLED'];
end;

procedure AhtmlTextInputElement.SetDisabled(const Value: Boolean);
begin
  AttributeFlag['DISABLED'] := Value;
end;

function AhtmlTextInputElement.GetReadOnly: Boolean;
begin
  Result := AttributeFlag['READONLY'];
end;

procedure AhtmlTextInputElement.SetReadOnly(const Value: Boolean);
begin
  AttributeFlag['READONLY'] := Value;
end;

function AhtmlTextInputElement.GetTabIndex: String;
begin
  Result := AttributeText['TABINDEX'];
end;

procedure AhtmlTextInputElement.SetTabIndex(const Value: String);
begin
  AttributeText['TABINDEX'] := Value;
end;

function AhtmlTextInputElement.GetOnFocus: String;
begin
  Result := AttributeText['ONFOCUS'];
end;

procedure AhtmlTextInputElement.SetOnFocus(const Value: String);
begin
  AttributeText['ONFOCUS'] := Value;
end;

function AhtmlTextInputElement.GetOnBlur: String;
begin
  Result := AttributeText['ONBLUR'];
end;

procedure AhtmlTextInputElement.SetOnBlur(const Value: String);
begin
  AttributeText['ONBLUR'] := Value;
end;

function AhtmlTextInputElement.GetOnSelect: String;
begin
  Result := AttributeText['ONSELECT'];
end;

procedure AhtmlTextInputElement.SetOnSelect(const Value: String);
begin
  AttributeText['ONSELECT'] := Value;
end;

function AhtmlTextInputElement.GetOnChange: String;
begin
  Result := AttributeText['ONCHANGE'];
end;

procedure AhtmlTextInputElement.SetOnChange(const Value: String);
begin
  AttributeText['ONCHANGE'] := Value;
end;

{ ThtmlINPUT                                                                   }
constructor ThtmlINPUT.Create;
begin
  inherited Create(HTML_TAG_INPUT, ttStartTag);
end;

function ThtmlINPUT.GetInputType: ThtmlInputType;
begin
  Result := htmlDecodeInputType(AttributeText['TYPE']);
end;

procedure ThtmlINPUT.SetInputType(const Value: ThtmlInputType);
begin
  AttributeText['TYPE'] := htmlEncodeInputType(Value);
end;

function ThtmlINPUT.GetValue: String;
begin
  Result := AttributeText['VALUE'];
end;

procedure ThtmlINPUT.SetValue(const Value: String);
begin
  AttributeText['VALUE'] := Value;
end;

function ThtmlINPUT.GetChecked: Boolean;
begin
  Result := AttributeFlag['CHECKED'];
end;

procedure ThtmlINPUT.SetChecked(const Value: Boolean);
begin
  AttributeFlag['CHECKED'] := Value;
end;

function ThtmlINPUT.GetSize: String;
begin
  Result := AttributeText['SIZE'];
end;

procedure ThtmlINPUT.SetSize(const Value: String);
begin
  AttributeText['SIZE'] := Value;
end;

function ThtmlINPUT.GetMaxLength: Integer;
begin
  Result := htmlDecodeInteger(AttributeText['MAXLENGTH'], -1);
end;

procedure ThtmlINPUT.SetMaxLength(const Value: Integer);
begin
  AttributeText['MAXLENGTH'] := htmlEncodeInteger(Value);
end;

function ThtmlINPUT.GetSrc: String;
begin
  Result := AttributeText['SRC'];
end;

procedure ThtmlINPUT.SetSrc(const Value: String);
begin
  AttributeText['SRC'] := Value;
end;

function ThtmlINPUT.GetAlt: String;
begin
  Result := AttributeText['ALT'];
end;

procedure ThtmlINPUT.SetAlt(const Value: String);
begin
  AttributeText['ALT'] := Value;
end;

function ThtmlINPUT.GetAlign: ThtmlIAlignType;
begin
  Result := htmlDecodeIAlignType(AttributeText['ALIGN']);
end;

procedure ThtmlINPUT.SetAlign(const Value: ThtmlIAlignType);
begin
  AttributeText['ALIGN'] := htmlEncodeIAlignType(Value);
end;

function ThtmlINPUT.GetSizeInt: Integer;
begin
  Result := StringToIntDefU(GetSize, 0);
end;

{ ThtmlSELECT                                                                  }
constructor ThtmlSELECT.Create;
begin
  inherited Create(HTML_TAG_SELECT, ttContentTags);
end;

function ThtmlSELECT.CreateItem(const ID: Integer; const Name: String): AhtmlObject;
begin
  if ThtmlTagID(ID) = HTML_TAG_OPTGROUP then
    Result := ThtmlOPTGROUP.Create else
  if ThtmlTagID(ID) = HTML_TAG_OPTION then
    Result := ThtmlOPTION.Create
  else
    Result := inherited CreateItem(ID, Name);
end;

function ThtmlSELECT.GetNameAttr: String;
begin
  Result := AttributeText['NAME'];
end;

procedure ThtmlSELECT.SetNameAttr(const Value: String);
begin
  AttributeText['NAME'] := Value;
end;

function ThtmlSELECT.GetSize: Integer;
begin
  Result := htmlDecodeInteger(AttributeText['SIZE'], -1);
end;

procedure ThtmlSELECT.SetSize(const Value: Integer);
begin
  AttributeText['SIZE'] := htmlEncodeInteger(Value);
end;

function ThtmlSELECT.GetMultiple: Boolean;
begin
  Result := AttributeFlag['MULTIPLE'];
end;

procedure ThtmlSELECT.SetMultiple(const Value: Boolean);
begin
  AttributeFlag['MULTIPLE'] := Value;
end;

function ThtmlSELECT.GetDisabled: Boolean;
begin
  Result := AttributeFlag['DISABLED'];
end;

procedure ThtmlSELECT.SetDisabled(const Value: Boolean);
begin
  AttributeFlag['DISABLED'] := Value;
end;

function ThtmlSELECT.GetTabIndex: String;
begin
  Result := AttributeText['TABINDEX'];
end;

procedure ThtmlSELECT.SetTabIndex(const Value: String);
begin
  AttributeText['TABINDEX'] := Value;
end;

function ThtmlSELECT.GetOnFocus: String;
begin
  Result := AttributeText['ONFOCUS'];
end;

procedure ThtmlSELECT.SetOnFocus(const Value: String);
begin
  AttributeText['ONFOCUS'] := Value;
end;

function ThtmlSELECT.GetOnBlur: String;
begin
  Result := AttributeText['ONBLUR'];
end;

procedure ThtmlSELECT.SetOnBlur(const Value: String);
begin
  AttributeText['ONBLUR'] := Value;
end;

function ThtmlSELECT.GetOnChange: String;
begin
  Result := AttributeText['ONCHANGE'];
end;

procedure ThtmlSELECT.SetOnChange(const Value: String);
begin
  AttributeText['ONCHANGE'] := Value;
end;

{ ThtmlOPTGROUP                                                                }
constructor ThtmlOPTGROUP.Create;
begin
  inherited Create(HTML_TAG_OPTGROUP, ttContentTags);
end;

function ThtmlOPTGROUP.GetDisabled: Boolean;
begin
  Result := AttributeFlag['DISABLED'];
end;

procedure ThtmlOPTGROUP.SetDisabled(const Value: Boolean);
begin
  AttributeFlag['DISABLED'] := Value;
end;

function ThtmlOPTGROUP.GetLabelText: String;
begin
  Result := AttributeText['LABEL'];
end;

procedure ThtmlOPTGROUP.SetLabelText(const Value: String);
begin
  AttributeText['LABEL'] := Value;
end;

{ ThtmlOPTION                                                                  }
constructor ThtmlOPTION.Create;
begin
  inherited Create(HTML_TAG_OPTION, ttContentTags);
end;

function ThtmlOPTION.GetDisabled: Boolean;
begin
  Result := AttributeFlag['DISABLED'];
end;

procedure ThtmlOPTION.SetDisabled(const Value: Boolean);
begin
  AttributeFlag['DISABLED'] := Value;
end;

function ThtmlOPTION.GetSelected: Boolean;
begin
  Result := AttributeFlag['SELECTED'];
end;

procedure ThtmlOPTION.SetSelected(const Value: Boolean);
begin
  AttributeFlag['SELECTED'] := Value;
end;

function ThtmlOPTION.GetLabelText: String;
begin
  Result := AttributeText['LABEL'];
end;

procedure ThtmlOPTION.SetLabelText(const Value: String);
begin
  AttributeText['LABEL'] := Value;
end;

function ThtmlOPTION.GetValue: String;
begin
  Result := AttributeText['VALUE'];
end;

procedure ThtmlOPTION.SetValue(const Value: String);
begin
  AttributeText['VALUE'] := Value;
end;

{ ThtmlTEXTAREA                                                                }
constructor ThtmlTEXTAREA.Create;
begin
  inherited Create(HTML_TAG_TEXTAREA, ttContentTags);
end;

function ThtmlTEXTAREA.GetRows: Integer;
begin
  Result := htmlDecodeInteger(AttributeText['ROWS'], 0);
end;

procedure ThtmlTEXTAREA.SetRows(const Value: Integer);
begin
  AttributeText['ROWS'] := htmlEncodeInteger(Value);
end;

function ThtmlTEXTAREA.GetCols: Integer;
begin
  Result := htmlDecodeInteger(AttributeText['COLS'], 0);
end;

procedure ThtmlTEXTAREA.SetCols(const Value: Integer);
begin
  AttributeText['COLS'] := htmlEncodeInteger(Value);
end;

{ ThtmlFIELDSET                                                                }
constructor ThtmlFIELDSET.Create;
begin
  inherited Create(HTML_TAG_FIELDSET, ttContentTags);
end;

{ ThtmlLEGEND                                                                  }
constructor ThtmlLEGEND.Create;
begin
  inherited Create(HTML_TAG_LEGEND, ttContentTags);
end;

function ThtmlLEGEND.GetAlign: String;
begin
  Result := AttributeText['ALIGN'];
end;

procedure ThtmlLEGEND.SetAlign(const Value: String);
begin
  AttributeText['ALIGN'] := Value;
end;

{ ThtmlBUTTON                                                                  }
constructor ThtmlBUTTON.Create;
begin
  inherited Create(HTML_TAG_BUTTON, ttContentTags);
end;

function ThtmlBUTTON.GetNameAttr: String;
begin
  Result := AttributeText['NAME'];
end;

procedure ThtmlBUTTON.SetNameAttr(const Value: String);
begin
  AttributeText['NAME'] := Value;
end;

function ThtmlBUTTON.GetValue: String;
begin
  Result := AttributeText['VALUE'];
end;

procedure ThtmlBUTTON.SetValue(const Value: String);
begin
  AttributeText['VALUE'] := Value;
end;

function ThtmlBUTTON.GetButtonType: String;
begin
  Result := AttributeText['TYPE'];
end;

procedure ThtmlBUTTON.SetButtonType(const Value: String);
begin
  AttributeText['TYPE'] := Value;
end;

function ThtmlBUTTON.GetDisabled: Boolean;
begin
  Result := AttributeFlag['DISABLED'];
end;

procedure ThtmlBUTTON.SetDisabled(const Value: Boolean);
begin
  AttributeFlag['DISABLED'] := Value;
end;

function ThtmlBUTTON.GetTabIndex: String;
begin
  Result := AttributeText['TABINDEX'];
end;

procedure ThtmlBUTTON.SetTabIndex(const Value: String);
begin
  AttributeText['TABINDEX'] := Value;
end;

function ThtmlBUTTON.GetOnFocus: String;
begin
  Result := AttributeText['ONFOCUS'];
end;

procedure ThtmlBUTTON.SetOnFocus(const Value: String);
begin
  AttributeText['ONFOCUS'] := Value;
end;

function ThtmlBUTTON.GetOnBlur: String;
begin
  Result := AttributeText['ONBLUR'];
end;

procedure ThtmlBUTTON.SetOnBlur(const Value: String);
begin
  AttributeText['ONBLUR'] := Value;
end;

{ ============================================================================ }
{ TABLES                                                                       }
{ ============================================================================ }

{ ThtmlTABLE                                                                   }
constructor ThtmlTABLE.Create;
begin
  inherited Create(HTML_TAG_TABLE, ttContentTags);
end;

function ThtmlTABLE.CreateItem(const ID: Integer; const Name: String): AhtmlObject;
begin
  if ThtmlTagID(ID) = HTML_TAG_TD then
    Result := ThtmlTD.Create else
  if ThtmlTagID(ID) = HTML_TAG_TR then
    Result := ThtmlTR.Create else
  if ThtmlTagID(ID) = HTML_TAG_TH then
    Result := ThtmlTH.Create else
  if ThtmlTagID(ID) = HTML_TAG_CAPTION then
    Result := ThtmlCAPTION.Create else
  if ThtmlTagID(ID) = HTML_TAG_COL then
    Result := ThtmlCOL.Create else
  if ThtmlTagID(ID) = HTML_TAG_COLGROUP then
    Result := ThtmlCOLGROUP.Create else
  if ThtmlTagID(ID) = HTML_TAG_THEAD then
    Result := ThtmlTHEAD.Create else
  if ThtmlTagID(ID) = HTML_TAG_TBODY then
    Result := ThtmlTBODY.Create else
  if ThtmlTagID(ID) = HTML_TAG_TFOOT then
    Result := ThtmlTFOOT.Create
  else
    Result := inherited CreateItem(ID, Name);
end;

function ThtmlTABLE.GetSummary: String;
begin
  Result := AttributeText['SUMMARY'];
end;

procedure ThtmlTABLE.SetSummary(const Value: String);
begin
  AttributeText['SUMMARY'] := Value;
end;

function ThtmlTABLE.GetWidthStr: String;
begin
  Result := AttributeText['WIDTH'];
end;

procedure ThtmlTABLE.SetWidthStr(const Value: String);
begin
  AttributeText['WIDTH'] := Value;
end;

function ThtmlTABLE.GetWidth: ThtmlLength;
begin
  Result := htmlDecodeLength(GetWidthStr);
end;

function ThtmlTABLE.GetBorder: Integer;
begin
  Result := htmlDecodeInteger(AttributeText['BORDER'], 0);
end;

procedure ThtmlTABLE.SetBorder(const Value: Integer);
begin
  AttributeText['BORDER'] := htmlEncodeInteger(Value);
end;

function ThtmlTABLE.GetCellSpacing: String;
begin
  Result := AttributeText['CELLSPACING'];
end;

procedure ThtmlTABLE.SetCellSpacing(const Value: String);
begin
  AttributeText['CELLSPACING'] := Value;
end;

function ThtmlTABLE.GetCellSpacingInt: Integer;
begin
  Result := StringToIntDefU(GetCellSpacing, 1);
end;

function ThtmlTABLE.GetCellPadding: String;
begin
  Result := AttributeText['CELLPADDING'];
end;

procedure ThtmlTABLE.SetCellPadding(const Value: String);
begin
  AttributeText['CELLPADDING'] := Value;
end;

function ThtmlTABLE.GetCellPaddingInt: Integer;
begin
  Result := StringToIntDefU(GetCellPadding, 1);
end;

function ThtmlTABLE.GetAlign: ThtmlTAlignType;
begin
  Result := htmlDecodeTAlignType(AttributeText['ALIGN']);
end;

procedure ThtmlTABLE.SetAlign(const Value: ThtmlTAlignType);
begin
  AttributeText['ALIGN'] := htmlEncodeTAlignType(Value);
end;

function ThtmlTABLE.GetBgColor: String;
begin
  Result := AttributeText['BGCOLOR'];
end;

procedure ThtmlTABLE.SetBgColor(const Value: String);
begin
  AttributeText['BGCOLOR'] := Value;
end;

function ThtmlTABLE.GetBgColorRGB: LongWord;
begin
  Result := htmlResolveColor(GetBgColor).RGB;
end;

procedure ThtmlTABLE.SetBgColorRGB(const Value: LongWord);
begin
  SetBgColor(htmlEncodeRGBColor(htmlRGBColor(Value)));
end;

function ThtmlTABLE.GetBgColorDelphi: TColor;
begin
  Result := htmlResolveColor(GetBgColor).Color;
end;

function ThtmlTABLE.GetColGroupsSpanResolved: Integer;
var ColGroup: ThtmlCOLGROUP;
begin
  Result := 0;
  ColGroup := nil;
  repeat
    ColGroup := ThtmlCOLGROUP(FindNext(ColGroup, ThtmlCOLGROUP));
    if not Assigned(ColGroup) then
      break;
    Inc(Result, ColGroup.GetSpanResolved);
  until False;
end;

function ThtmlTABLE.GetColGroupsWidthsPixels(const StyleSheet: ThtmlCSS;
    const AbsWidth: Integer): IntegerArray;
var ColGroup: ThtmlCOLGROUP;
    W: IntegerArray;
begin
  W := nil;
  Result := nil;
  ColGroup := nil;
  repeat
    ColGroup := ThtmlCOLGROUP(FindNext(ColGroup, ThtmlCOLGROUP));
    if not Assigned(ColGroup) then
      break;
    W := ColGroup.GetColWidthsPixels(StyleSheet, AbsWidth);
    DynArrayAppendIntegerArray(Result, W);
  until False;
end;

procedure ThtmlTABLE.GetColGroupsAlign(var HorAlign: ThtmlTextAlignArray;
          var VerAlign: ThtmlVerticalAlignArray);
var ColGroup: ThtmlCOLGROUP;
    H: ThtmlTextAlignArray;
    V: ThtmlVerticalAlignArray;
    I, L, M: Integer;
begin
  H := nil;
  V := nil;
  L := GetColGroupsSpanResolved;
  SetLength(HorAlign, L);
  SetLength(VerAlign, L);
  ColGroup := nil;
  L := 0;
  repeat
    ColGroup := ThtmlCOLGROUP(FindNext(ColGroup, ThtmlCOLGROUP));
    if not Assigned(ColGroup) then
      break;
    ColGroup.GetGroupAlign(H, V);
    M := Length(H);
    if M > 0 then
      begin
        Assert(Length(V) = Length(H), 'Length(V) = Length(H)');
        for I := 0 to M - 1 do
          begin
            HorAlign[L + I] := H[I];
            VerAlign[L + I] := V[I];
          end;
        Inc(L, M);
      end;
  until False;
end;

procedure ThtmlTABLE.ApplyHTMLStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
          const ParentStyle: ThtmlcssStyleProperties);
var S: String;
begin
  inherited;
  S := GetBgColor;
  if S <> '' then
    StyleInfo.BackColor := htmlDecodeColor(S);
  S := GetWidthStr;
  if S <> '' then
    StyleInfo.Width := htmlDecodeLength(S);
end;

{ ThtmlCAPTION                                                                 }
constructor ThtmlCAPTION.Create;
begin
  inherited Create(HTML_TAG_CAPTION, ttContentTags);
end;

function ThtmlCAPTION.GetAlign: String;
begin
  Result := AttributeText['ALIGN'];
end;

procedure ThtmlCAPTION.SetAlign(const Value: String);
begin
  AttributeText['ALIGN'] := Value;
end;

{ AhtmlElementInclCellAlign                                                    }
function AhtmlElementInclCellAlign.GetAlign: String;
begin
  Result := AttributeText['ALIGN'];
end;

procedure AhtmlElementInclCellAlign.SetAlign(const Value: String);
begin
  AttributeText['ALIGN'] := Value;
end;

function AhtmlElementInclCellAlign.GetAlignChar: String;
begin
  Result := AttributeText['CHAR'];
end;

procedure AhtmlElementInclCellAlign.SetAlignChar(const Value: String);
begin
  AttributeText['CHAR'] := Value;
end;

function AhtmlElementInclCellAlign.GetCharOff: String;
begin
  Result := AttributeText['CHAROFF'];
end;

procedure AhtmlElementInclCellAlign.SetCharOff(const Value: String);
begin
  AttributeText['CHAROFF'] := Value;
end;

function AhtmlElementInclCellAlign.GetVAlign: String;
begin
  Result := AttributeText['VALIGN'];
end;

procedure AhtmlElementInclCellAlign.SetVAlign(const Value: String);
begin
  AttributeText['VALIGN'] := Value;
end;

function AhtmlElementInclCellAlign.GetHorAlign: ThtmlTextAlignType;
begin
  Result := htmlDecodeTextAlignType(GetAlign);
end;

function AhtmlElementInclCellAlign.GetVerAlign: ThtmlVerticalAlign;
begin
  Result := htmlcssDecodeVerticalAlign(GetVAlign);
end;

{ AhtmlTableColumnDefinitionElement                                            }
function AhtmlTableColumnDefinitionElement.GetSpan: Integer;
begin
  Result := htmlDecodeInteger(AttributeText['SPAN'], 1);
end;

procedure AhtmlTableColumnDefinitionElement.SetSpan(const Value: Integer);
begin
  AttributeText['SPAN'] := htmlEncodeInteger(Value);
end;

function AhtmlTableColumnDefinitionElement.GetWidthStr: String;
begin
  Result := AttributeText['WIDTH'];
end;

procedure AhtmlTableColumnDefinitionElement.SetWidthStr(const Value: String);
begin
  AttributeText['WIDTH'] := Value;
end;

function AhtmlTableColumnDefinitionElement.GetWidth: ThtmlLength;
begin
  Result := htmlDecodeLength(GetWidthStr);
end;

procedure AhtmlTableColumnDefinitionElement.ApplyHTMLStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
    const ParentStyle: ThtmlcssStyleProperties);
var S: String;
begin
  inherited;
  S := GetWidthStr;
  if S <> '' then
    StyleInfo.Width := htmlDecodeLength(S);
end;

{ ThtmlCOLGROUP                                                                }
constructor ThtmlCOLGROUP.Create;
begin
  inherited Create(HTML_TAG_COLGROUP, ttContentTags);
end;

function ThtmlCOLGROUP.GetSpanResolved: Integer;
var Col: ThtmlCOL;
    ColCount, SpanTot: Integer;
begin
  // Count span from COL elements
  ColCount := 0;
  SpanTot := 0;
  Col := nil;
  repeat
    Col := ThtmlCOL(FindNext(Col, ThtmlCOL));
    if not Assigned(Col) then
      break;
    Inc(SpanTot, Col.Span);
    Inc(ColCount);
  until False;
  // Calculate span
  if ColCount = 0 then
    Result := GetSpan // use Span attribute
  else
    Result := SpanTot;
end;

function ThtmlCOLGROUP.GetColWidthsPixels(const StyleSheet: ThtmlCSS;
    const AbsWidth: Integer): IntegerArray;
var I, J, L, S, V, W: Integer;
    Col: ThtmlCOL;
begin
  // get span
  L := GetSpanResolved;
  SetLength(Result, L);
  if L = 0 then
    exit;
  // initialize result from COLGROUP width
  W := MaxInt(0, htmlResolveLengthPixels(FStyle.Width, AbsWidth));
  for I := 0 to L - 1 do
    Result[I] := W;
  // set result from COL widths
  I := 0;
  Col := nil;
  repeat
    Col := ThtmlCOL(FindNext(Col, ThtmlCOL));
    if not Assigned(Col) then
      break;
    V := htmlResolveLengthPixels(Col.FStyle.Width, AbsWidth);
    S := Col.Span;
    for J := I to I + S - 1 do
      Result[J] := V;
    Inc(I, S);
  until I >= L;
end;

procedure ThtmlCOLGROUP.GetGroupAlign(var HorAlign: ThtmlTextAlignArray;
          var VerAlign: ThtmlVerticalAlignArray);
var I, J, L, S: Integer;
    V: ThtmlVerticalAlign;
    H: ThtmlTextAlignType;
    Col: ThtmlCOL;
begin
  // get span
  L := GetSpanResolved;
  SetLength(HorAlign, L);
  SetLength(VerAlign, L);
  if L = 0 then
    exit;
  // initialize result
  for I := 0 to L - 1 do
    begin
      HorAlign[I] := talignDefault;
      VerAlign[I].AlignType := valignDefault;
    end;
  // set result from COLs
  I := 0;
  Col := nil;
  repeat
    Col := ThtmlCOL(FindNext(Col, ThtmlCOL));
    if not Assigned(Col) then
      break;
    V := Col.VerAlign;
    H := Col.HorAlign;
    S := Col.Span;
    for J := I to I + S - 1 do
      begin
        HorAlign[J] := H;
        VerAlign[J] := V;
      end;
    Inc(I, S);
  until I >= L;
end;

{ ThtmlCOL                                                                     }
constructor ThtmlCOL.Create;
begin
  inherited Create(HTML_TAG_COL, ttContentTags);
end;

{ ThtmlTHEAD                                                                   }
constructor ThtmlTHEAD.Create;
begin
  inherited Create(HTML_TAG_THEAD, ttContentTags);
end;

{ ThtmlTBODY                                                                   }
constructor ThtmlTBODY.Create;
begin
  inherited Create(HTML_TAG_TBODY, ttContentTags);
end;

{ ThtmlTFOOT                                                                   }
constructor ThtmlTFOOT.Create;
begin
  inherited Create(HTML_TAG_TFOOT, ttContentTags);
end;

{ ThtmlTR                                                                      }
constructor ThtmlTR.Create;
begin
  inherited Create(HTML_TAG_TR, ttContentTags);
end;

function ThtmlTR.GetBgColor: String;
begin
  Result := AttributeText['BGCOLOR'];
end;

procedure ThtmlTR.SetBgColor(const Value: String);
begin
  AttributeText['BGCOLOR'] := value;
end;

function ThtmlTR.GetBgColorDelphi: TColor;
begin
  Result := htmlResolveColor(GetBgColor).Color;
end;

procedure ThtmlTR.ApplyHTMLStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
          const ParentStyle: ThtmlcssStyleProperties);
var S: String;
begin
  inherited;
  S := GetBgColor;
  if S <> '' then
    StyleInfo.BackColor := htmlDecodeColor(S);
end;

{ AhtmlTableDataCell                                                           }
constructor AhtmlTableDataCell.Create(const TagID: ThtmlTagID);
begin
  inherited Create(TagID, ttContentTags);
end;

function AhtmlTableDataCell.GetAbbr: String;
begin
  Result := AttributeText['ABBR'];
end;

procedure AhtmlTableDataCell.SetAbbr(const Value: String);
begin
  AttributeText['ABBR'] := Value;
end;

function AhtmlTableDataCell.GetRowSpan: Integer;
begin
  Result := htmlDecodeInteger(AttributeText['ROWSPAN'], 1);
end;

procedure AhtmlTableDataCell.SetRowSpan(const Value: Integer);
begin
  AttributeText['ROWSPAN'] := htmlEncodeInteger(Value);
end;

function AhtmlTableDataCell.GetColSpan: Integer;
begin
  Result := htmlDecodeInteger(AttributeText['COLSPAN'], 1);
end;

procedure AhtmlTableDataCell.SetColSpan(const Value: Integer);
begin
  AttributeText['COLSPAN'] := htmlEncodeInteger(Value);
end;

function AhtmlTableDataCell.GetNoWrap: Boolean;
begin
  Result := AttributeFlag['NOWRAP'];
end;

procedure AhtmlTableDataCell.SetNoWrap(const Value: Boolean);
begin
  AttributeFlag['NOWRAP'] := Value;
end;

function AhtmlTableDataCell.GetBgColor: String;
begin
  Result := AttributeText['BGCOLOR'];
end;

procedure AhtmlTableDataCell.SetBgColor(const Value: String);
begin
  AttributeText['BGCOLOR'] := Value;
end;

function AhtmlTableDataCell.GetBgColorDelphi: TColor;
begin
  Result := htmlResolveColor(GetBgColor).Color;
end;

function AhtmlTableDataCell.GetWidthStr: String;
begin
  Result := AttributeText['WIDTH'];
end;

procedure AhtmlTableDataCell.SetWidthStr(const Value: String);
begin
  AttributeText['WIDTH'] := Value;
end;

function AhtmlTableDataCell.GetHeightStr: String;
begin
  Result := AttributeText['HEIGHT'];
end;

procedure AhtmlTableDataCell.SetHeightStr(const Value: String);
begin
  AttributeText['HEIGHT'] := Value;
end;

function AhtmlTableDataCell.GetWidth: ThtmlLength;
begin
  Result := htmlDecodeLength(GetWidthStr);
end;

function AhtmlTableDataCell.GetHeight: ThtmlLength;
begin
  Result := htmlDecodeLength(GetHeightStr);
end;

procedure AhtmlTableDataCell.ApplyHTMLStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
          const ParentStyle: ThtmlcssStyleProperties);
var S: String;
begin
  inherited;
  S := GetBgColor;
  if S <> '' then
    StyleInfo.BackColor := htmlDecodeColor(S);
  S := GetWidthStr;
  if S <> '' then
    StyleInfo.Width := htmlDecodeLength(S);
  S := GetHeightStr;
  if S <> '' then
    StyleInfo.Height := htmlDecodeLength(S);
end;

{ ThtmlTD                                                                      }
constructor ThtmlTD.Create;
begin
  inherited Create(HTML_TAG_TD);
  FTagType := ttContentTags;
end;

{ ThtmlTH                                                                      }
constructor ThtmlTH.Create;
begin
  inherited Create(HTML_TAG_TH);
  FTagType := ttContentTags;
end;

{ ============================================================================ }
{ FRAMES                                                                       }
{ ============================================================================ }

{ ThtmlFRAMESET                                                                }
constructor ThtmlFRAMESET.Create;
begin
  inherited Create(HTML_TAG_FRAMESET, ttContentTags);
end;

function ThtmlFRAMESET.CreateItem(const ID: Integer; const Name: String): AhtmlObject;
begin
  if ThtmlTagID(ID) = HTML_TAG_FRAME then
    Result := ThtmlFRAME.Create
  else
    Result := inherited CreateItem(ID, Name);
end;

function ThtmlFRAMESET.GetRows: String;
begin
  Result := AttributeText['ROWS'];
end;

procedure ThtmlFRAMESET.SetRows(const Value: String);
begin
  AttributeText['ROWS'] := Value;
end;

function ThtmlFRAMESET.GetCols: String;
begin
  Result := AttributeText['COLS'];
end;

procedure ThtmlFRAMESET.SetCols(const Value: String);
begin
  AttributeText['COLS'] := Value;
end;

function ThtmlFRAMESET.GetOnLoad: String;
begin
  Result := AttributeText['ONLOAD'];
end;

procedure ThtmlFRAMESET.SetOnLoad(const Value: String);
begin
  AttributeText['ONLOAD'] := Value;
end;

function ThtmlFRAMESET.GetOnUnload: String;
begin
  Result := AttributeText['ONUNLOAD'];
end;

procedure ThtmlFRAMESET.SetOnUnload(const Value: String);
begin
  AttributeText['ONUNLOAD'] := Value;
end;

{ AhtmlFrameElement                                                            }
function AhtmlFrameElement.GetLongDesc: String;
begin
  Result := AttributeText['LONGDESC'];
end;

procedure AhtmlFrameElement.SetLongDesc(const Value: String);
begin
  AttributeText['LONGDESC'] := Value;
end;

function AhtmlFrameElement.GetNameAttr: String;
begin
  Result := AttributeText['NAME'];
end;

procedure AhtmlFrameElement.SetNameAttr(const Value: String);
begin
  AttributeText['NAME'] := Value;
end;

function AhtmlFrameElement.GetSrc: String;
begin
  Result := AttributeText['SRC'];
end;

procedure AhtmlFrameElement.SetSrc(const Value: String);
begin
  AttributeText['SRC'] := Value;
end;

function AhtmlFrameElement.GetFrameBorder: Boolean;
begin
  Result := htmlDecodeInteger(AttributeText['FRAMEBORDER'], 1) <> 0;
end;

procedure AhtmlFrameElement.SetFrameBorder(const Value: Boolean);
begin
  AttributeText['FRAMEBORDER'] := iifU(Value, '1', '0');
end;

function AhtmlFrameElement.GetMarginWidth: Integer;
begin
  Result := htmlDecodeInteger(AttributeText['MARGINWIDTH'], 0);
end;

procedure AhtmlFrameElement.SetMarginWidth(const Value: Integer);
begin
  AttributeText['MARGINWIDTH'] := htmlEncodeInteger(Value);
end;

function AhtmlFrameElement.GetMarginHeight: Integer;
begin
  Result := htmlDecodeInteger(AttributeText['MARGINHEIGHT'], 0);
end;

procedure AhtmlFrameElement.SetMarginHeight(const Value: Integer);
begin
  AttributeText['MARGINHEIGHT'] := htmlEncodeInteger(Value);
end;

function AhtmlFrameElement.GetScrolling: String;
begin
  Result := AttributeText['SCROLLING'];
end;

procedure AhtmlFrameElement.SetScrolling(const Value: String);
begin
  AttributeText['SCROLLING'] := Value;
end;

{ ThtmlFRAME                                                                   }
constructor ThtmlFRAME.Create;
begin
  inherited Create(HTML_TAG_FRAME, ttStartTag);
end;

function ThtmlFRAME.GetNoResize: Boolean;
begin
  Result := AttributeFlag['NORESIZE'];
end;

procedure ThtmlFRAME.SetNoResize(const Value: Boolean);
begin
  AttributeFlag['NORESIZE'] := Value;
end;

{ ThtmlIFRAME                                                                  }
constructor ThtmlIFRAME.Create;
begin
  inherited Create(HTML_TAG_IFRAME, ttContentTags);
end;

function ThtmlIFRAME.GetAlign: ThtmlIAlignType;
begin
  Result := htmlDecodeIAlignType(AttributeText['ALIGN']);
end;

procedure ThtmlIFRAME.SetAlign(const Value: ThtmlIAlignType);
begin
  AttributeText['ALIGN'] := htmlEncodeIAlignType(Value);
end;

function ThtmlIFRAME.GetHeight: String;
begin
  Result := AttributeText['HEIGHT'];
end;

procedure ThtmlIFRAME.SetHeight(const Value: String);
begin
  AttributeText['HEIGHT'] := Value;
end;

function ThtmlIFRAME.GetWidth: String;
begin
  Result := AttributeText['WIDTH'];
end;

procedure ThtmlIFRAME.SetWidth(const Value: String);
begin
  AttributeText['WIDTH'] := Value;
end;

{ ThtmlNOFRAMES                                                                }
constructor ThtmlNOFRAMES.Create;
begin
  inherited Create(HTML_TAG_NOFRAMES, ttContentTags);
end;

{ ============================================================================ }
{ DOCUMENT HEAD                                                                }
{ ============================================================================ }

{ ThtmlHEAD                                                                    }
constructor ThtmlHEAD.Create;
begin
  inherited Create(HTML_TAG_HEAD, ttContentTags);
end;

function ThtmlHEAD.CreateItem(const ID: Integer; const Name: String): AhtmlObject;
begin
  if ThtmlTagID(ID) = HTML_TAG_TITLE then
    Result := ThtmlTITLE.Create else
  if ThtmlTagID(ID) = HTML_TAG_ISINDEX then
    Result := ThtmlISINDEX.Create else
  if ThtmlTagID(ID) = HTML_TAG_BASE then
    Result := ThtmlBASE.Create else
  if ThtmlTagID(ID) = HTML_TAG_SCRIPT then
    Result := ThtmlSCRIPT.Create else
  if ThtmlTagID(ID) = HTML_TAG_STYLE then
    Result := ThtmlSTYLE.Create else
  if ThtmlTagID(ID) = HTML_TAG_META then
    Result := ThtmlMETA.Create else
  if ThtmlTagID(ID) = HTML_TAG_LINK then
    Result := ThtmlLINK.Create else
  if ThtmlTagID(ID) = HTML_TAG_OBJECT then
    Result := ThtmlLINK.Create
  else
    Result := inherited CreateItem(ID, Name);
end;

function ThtmlHEAD.GetProfile: String;
begin
  Result := AttributeText['PROFILE'];
end;

procedure ThtmlHEAD.SetProfile(const Value: String);
begin
  AttributeText['PROFILE'] := Value;
end;

function ThtmlHEAD.FindMetaHttpField(const Name: String): AhtmlElement;
var T: AhtmlObject;
begin
  T := nil;
  repeat
    T := FindNextName(T, 'META');
    if not Assigned(T) then
      break;
    if (T is AhtmlElement) and
        StrEqualNoAsciiCaseU(Name, AhtmlElement(T).AttributeText['http-equiv']) then
      begin
        Result := AhtmlElement(T);
        exit;
      end;
  until False;
  Result := nil;
end;

function ThtmlHEAD.GetMetaHttpField(const Name: String): String;
var T: AhtmlElement;
begin
  T := FindMetaHttpField(Name);
  if Assigned(T) then
    Result := T.AttributeText['CONTENT'] else
    Result := '';
end;

procedure ThtmlHEAD.SetMetaHttpField(const Name, Value: String);
var T: AhtmlElement;
begin
  T := FindMetaHttpField(Name);
  if Assigned(T) then
    T.AttributeText['CONTENT'] := Value
  else
    AddItem(ThtmlMETA.CreateField(Name, Value));
end;

function ThtmlHEAD.GetTitle: String;
var T: AhtmlObject;
begin
  Result := ItemText['TITLE'];
  if Result = '' then
    Result := MetaHttpField['title'];
  if Result = '' then
    begin
      T := FindNextName(nil, 'STYLE');
      if Assigned(T) and (T is AhtmlElement) then
        Result := AhtmlElement(T).AttributeText['TITLE'];
    end;
end;

procedure ThtmlHEAD.SetTitle(const Value: String);
begin
  ItemText['TITLE'] := Value;
  MetaHttpField['title'] := Value;
end;

function ThtmlHEAD.GetContentType: String;
begin
  Result := MetaHttpField['content-type'];
end;

procedure ThtmlHEAD.SetContentType(const Value: String);
begin
  MetaHttpField['content-type'] := Value;
end;

function ThtmlHEAD.GetDescription: String;
begin
  Result := MetaHttpField['description'];
end;

procedure ThtmlHEAD.SetDescription(const Value: String);
begin
  MetaHttpField['description'] := Value;
end;

function ThtmlHEAD.GetKeywords: String;
begin
  Result := MetaHttpField['keywords'];
end;

procedure ThtmlHEAD.SetKeywords(const Value: String);
begin
  MetaHttpField['keywords'] := Value;
end;

function ThtmlHEAD.GetAutoRefresh: Boolean;
begin
  Result := Assigned(FindMetaHttpField('refresh'));
end;

procedure ThtmlHEAD.SetAutoRefresh(const Value: Boolean);
begin
  if Value = GetAutoRefresh then
    exit;
  if Value then
    MetaHttpField['refresh'] := '';
end;

function ThtmlHEAD.GetAutoRefreshInterval: Integer;
begin
  Result := StringToIntDefU(StrBeforeU(MetaHttpField['refresh'], ';', True), 0);
end;

procedure ThtmlHEAD.SetAutoRefreshInterval(const Value: Integer);
var S, T: String;
begin
  S := MetaHttpField['refresh'];
  T := StrAfterU(S, ';');
  MetaHttpField['refresh'] := IntToStringU(Value) + iifU(T <> '', ';', '') + T;
end;

function ThtmlHEAD.GetAutoRefreshURL: String;
begin
  Result := StrBetweenU(MetaHttpField['refresh'], ';URL=', [';'],
      False, True, False);
end;

procedure ThtmlHEAD.SetAutoRefreshURL(const Value: String);
var S, T: String;
begin
  S := MetaHttpField['refresh'];
  T := StrBeforeU(S, ';URL=', True, False);
  MetaHttpField['refresh'] := T + ';URL=' + Value;
end;

function ThtmlHEAD.GetStyleText: String;
var Style: ThtmlSTYLE;
begin
  Result := '';
  Style := nil;
  repeat
    Style := ThtmlSTYLE(FindNext(Style, ThtmlSTYLE));
    if not Assigned(Style) then
      exit;
    if Result <> '' then
      Result := Result + #13#10;
    Result := Result + Style.StyleText;
  until False;
end;

function ThtmlHEAD.GetStyleRefs: StringArray;
var Link : ThtmlLINK;
    S    : String;
begin
  Result := nil;
  Link := nil;
  repeat
    Link := ThtmlLINK(FindNext(Link, ThtmlLINK));
    if not Assigned(Link) then
      exit;
    if StrEqualNoAsciiCaseU(Link.Rel, 'STYLESHEET') and
       ((Link.Media = '') or (PosStrU('screen', Link.Media, 1, False) > 0)) then
      begin
        S := Link.HRef;
        StrTrimInPlaceU(S);
        if S <> '' then
          DynArrayAppend(Result, S);
      end;
  until False;
end;

procedure ThtmlHEAD.PrepareStructure;
begin
end;

{ ThtmlTITLE                                                                   }
constructor ThtmlTITLE.Create;
begin
  inherited Create(HTML_TAG_TITLE, ttContentTags);
end;

{ ThtmlISINDEX                                                                 }
constructor ThtmlISINDEX.Create;
begin
  inherited Create(HTML_TAG_ISINDEX, ttStartTag);
end;

function ThtmlISINDEX.GetPrompt: String;
begin
  Result := AttributeText['PROMPT'];
end;

procedure ThtmlISINDEX.SetPrompt(const Value: String);
begin
  AttributeText['PROMPT'] := Value;
end;

{ ThtmlBASE                                                                    }
constructor ThtmlBASE.Create;
begin
  inherited Create(HTML_TAG_BASE, ttStartTag);
end;

function ThtmlBASE.GetHRef: String;
begin
  Result := AttributeText['HREF'];
end;

procedure ThtmlBASE.SetHRef(const Value: String);
begin
  AttributeText['HREF'] := Value;
end;

function ThtmlBASE.GetTarget: String;
begin
  Result := AttributeText['TARGET'];
end;

procedure ThtmlBASE.SetTarget(const Value: String);
begin
  AttributeText['TARGET'] := Value;
end;

{ ThtmlMETA                                                                    }
constructor ThtmlMETA.Create;
begin
  inherited Create(HTML_TAG_META, ttStartTag);
end;

constructor ThtmlMETA.CreateField(const HttpEquiv, Value: String);
begin
  inherited Create;
  SetHttpEquiv(HttpEquiv);
  SetFieldContent(Value);
end;

function ThtmlMETA.GetHttpEquiv: String;
begin
  Result := AttributeText['HTTP-EQUIV'];
end;

procedure ThtmlMETA.SetHttpEquiv(const Value: String);
begin
  AttributeText['HTTP-EQUIV'] := Value;
end;

function ThtmlMETA.GetFieldContent: String;
begin
  Result := AttributeText['CONTENT'];
end;

procedure ThtmlMETA.SetFieldContent(const Value: String);
begin
  AttributeText['CONTENT'] := Value;
end;

function ThtmlMETA.GetNameAttr: String;
begin
  Result := AttributeText['NAME'];
end;

procedure ThtmlMETA.SetNameAttr(const Value: String);
begin
  AttributeText['NAME'] := Value;
end;

function ThtmlMETA.GetScheme: String;
begin
  Result := AttributeText['SCHEME'];
end;

procedure ThtmlMETA.SetScheme(const Value: String);
begin
  AttributeText['SCHEME'] := Value;
end;

{ ThtmlSTYLE                                                                   }
constructor ThtmlSTYLE.Create;
begin
  inherited Create(HTML_TAG_STYLE, ttContentTags);
end;

function ThtmlSTYLE.GetContentType: String;
begin
  Result := AttributeText['TYPE'];
end;

procedure ThtmlSTYLE.SetContentType(const Value: String);
begin
  AttributeText['TYPE'] := Value;
end;

function ThtmlSTYLE.GetMedia: String;
begin
  Result := AttributeText['MEDIA'];
end;

procedure ThtmlSTYLE.SetMedia(const Value: String);
begin
  AttributeText['MEDIA'] := Value;
end;

function ThtmlSTYLE.GetTitle: String;
begin
  Result := AttributeText['TITLE'];
end;

procedure ThtmlSTYLE.SetTitle(const Value: String);
begin
  AttributeText['TITLE'] := Value;
end;

function ThtmlSTYLE.GetStyleText: String;
var C: AhtmlObject;
    I: Integer;
begin
  Result := '';
  // Get style sheet text from content. Text may be in comments.
  C := nil;
  repeat
    C := FindNext(C, AhtmlObject);
    if not Assigned(C) then
      break;
    if C is ThtmlComment then
      for I := 0 to Length(ThtmlComment(C).Comments) - 1 do
        Result := Result + ThtmlComment(C).Comments[I] + ' '
    else
    if C is AhtmlTextContentObject then
      Result := Result + C.ContentText;
  until False;
end;

{ ThtmlSCRIPT                                                                  }
constructor ThtmlSCRIPT.Create;
begin
  inherited Create(HTML_TAG_SCRIPT, ttContentTags);
end;

function ThtmlSCRIPT.GetContentType: String;
begin
  Result := AttributeText['TYPE'];
end;

procedure ThtmlSCRIPT.SetContentType(const Value: String);
begin
  AttributeText['TYPE'] := Value;
end;

function ThtmlSCRIPT.GetLanguage: String;
begin
  Result := AttributeText['LANGUAGE'];
end;

procedure ThtmlSCRIPT.SetLangauge(const Value: String);
begin
  AttributeText['LANGUAGE'] := Value;
end;

function ThtmlSCRIPT.GetSrc: String;
begin
  Result := AttributeText['SRC'];
end;

procedure ThtmlSCRIPT.SetSrc(const Value: String);
begin
  AttributeText['SRC'] := Value;
end;

function ThtmlSCRIPT.GetDefer: Boolean;
begin
  Result := AttributeFlag['DEFER'];
end;

procedure ThtmlSCRIPT.SetDefer(const Value: Boolean);
begin
  AttributeFlag['DEFER'] := Value;
end;

{ ThtmlNOSCRIPT                                                                }
constructor ThtmlNOSCRIPT.Create;
begin
  inherited Create(HTML_TAG_NOSCRIPT, ttContentTags);
end;

{ ============================================================================ }
{ DOCUMENT STRUCTURE                                                           }
{ ============================================================================ }

{ ThtmlVersionTag                                                              }
constructor ThtmlVersionTag.Create(const Version: String);
begin
  inherited Create;
  FVersion := Version;
end;

function ThtmlVersionTag.DuplicateObject: AhtmlObject;
begin
  Result := inherited DuplicateObject;
  ThtmlVersionTag(Result).FVersion := FVersion;
end;

function ThtmlVersionTag.GetHTMLText: String;
begin
  Result := '<!DOCTYPE HTML PUBLIC "' + FVersion + '">';
end;

{ ThtmlHTML                                                                    }
constructor ThtmlHTML.Create;
begin
  inherited Create(HTML_TAG_HTML, ttContentTags);
end;

function ThtmlHTML.CreateItem(const ID: Integer; const Name: String): AhtmlObject;
begin
  if ThtmlTagID(ID) = HTML_TAG_HEAD then
    Result := ThtmlHEAD.Create else
  if ThtmlTagID(ID) = HTML_TAG_BODY then
    Result := ThtmlBODY.Create else
  if ThtmlTagID(ID) = HTML_TAG_FRAMESET then
    Result := ThtmlFRAMESET.Create else
  if ThtmlTagID(ID) = HTML_TAG_NOFRAMES then
    Result := ThtmlNOFRAMES.Create
  else
    Result := inherited CreateItem(ID, Name);
end;

function ThtmlHTML.GetVersion: String;
var T: AhtmlObject;
begin
  T := FindNext(nil, ThtmlVersionTag);
  if Assigned(T) then
    Result := ThtmlVersionTag(T).Version
  else
    Result := '';
end;

procedure ThtmlHTML.SetVersion(const Value: String);
var T: AhtmlObject;
begin
  T := FindNext(nil, ThtmlVersionTag);
  if Assigned(T) then
    ThtmlVersionTag(T).Version := Value
  else
    AddItem(ThtmlVersionTag.Create(Value));
end;

function ThtmlHTML.GetHead: ThtmlHEAD;
begin
  Result := ThtmlHEAD(RequireItemByClass(ThtmlHEAD));
end;

function ThtmlHTML.GetBody: ThtmlBODY;
begin
  Result := ThtmlBODY(RequireItemByClass(ThtmlBODY));
end;

function ThtmlHTML.GetFrameSet: ThtmlFRAMESET;
begin
  Result := ThtmlFRAMESET(RequireItemByClass(ThtmlFRAMESET));
end;

procedure ThtmlHTML.SetContentText(const ContentText: String);
begin
  Body.ContentText := ContentText;
end;

procedure ThtmlHTML.PrepareStructure;
begin
  Head.PrepareStructure;
  Body.PrepareStructure;
end;



end.

