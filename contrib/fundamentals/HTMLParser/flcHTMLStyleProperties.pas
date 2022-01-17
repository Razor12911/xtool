{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals 5.00 - HTML Parser                          }
{   File name:        flcHTMLStyleProperties.pas                               }
{   File version:     5.04                                                     }
{   Description:      HTML Style Sheet properties                              }
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
{   2002/11/04  1.00  Initial version part of cHTMLStyleSheet.                 }
{   2002/12/08  1.01  Created cHTMLStyleProperty unit.                         }
{                     Created specific property classes.                       }
{   2005/10/01  1.02  All CSS level 1 property classes implemented.            }
{   2015/04/11  1.03  UnicodeString changes.                                   }
{   2019/02/22  5.04  Revise for Fundamentals 5.                               }
{                                                                              }
{ Description:                                                                 }
{   This unit implements classes for CSS properties.                           }
{                                                                              }
{ Notes:                                                                       }
{   All CSS1 properties implemented.                                           }
{                                                                              }
{******************************************************************************}

{$INCLUDE flcHTML.inc}

unit flcHTMLStyleProperties;

interface

uses
  { System }
  {$IFDEF DELPHI6_UP}
  Types,
  {$ELSE}
  Windows,
  {$ENDIF}

  { Fundamentals }
  flcUtils,

  { HTML }
  flcHTMLElements,
  flcHTMLProperties,
  flcHTMLStyleTypes;



{                                                                              }
{ ThtmlcssStyleProperties                                                      }
{   Collection of CSS style property values.                                   }
{                                                                              }
type
  ThtmlcssStyleProperties = record
    // inherited values
    FontFamily         : ThtmlFontFamilyList;
    FontStyle          : ThtmlFontStyle;
    FontVariant        : ThtmlFontVariant;
    FontWeight         : ThtmlFontWeight;
    FontSize           : ThtmlFontSize;
    FontColor          : ThtmlColor;
    TextDecoration     : ThtmlTextDecoration;
    VerticalAlign      : ThtmlVerticalAlign;
    TextAlign          : ThtmlTextAlignType;
    LineHeight         : ThtmlLineHeight;
    WhiteSpace         : ThtmlWhiteSpaceType;
    WordSpacing        : ThtmlSpacing;
    LetterSpacing      : ThtmlSpacing;
    TextTransform      : ThtmlTextTransform;
    TextIndent         : ThtmlLength;
    ListStyleType      : ThtmlListStyleType;
    ListStyleImage     : String;
    ListStylePosition  : ThtmlListStylePositionType;

    // non-inherited values
    BackColor          : ThtmlColor;
    BackAttach         : ThtmlBackgroundAttachment;
    BackImage          : ThtmlBackgroundImage;
    BackRepeat         : ThtmlBackgroundRepeat;
    BorderCol          : ThtmlColor;
    BorderStyle        : ThtmlBorderStyle;
    BorderWidths       : ThtmlBorderWidths;
    Margins            : ThtmlMargins;
    Width              : ThtmlLength;
    Height             : ThtmlLength;
    Padding            : TRect;
    Clear              : ThtmlClearType;
    Display            : ThtmlDisplayType;
    ListStyle          : ThtmlListStyleType;
    Float              : ThtmlFloatType;
  end;

procedure htmlcssStyleResetNonInherited(var Style: ThtmlcssStyleProperties);



{                                                                              }
{ ThtmlcssProperty                                                             }
{   Base class for CSS properties.                                             }
{                                                                              }
type
  ThtmlcssProperty = class
  protected
    FNext      : ThtmlcssProperty;
    FPropID    : ThtmlcssPropertyID;
    FValueStr  : String;
    FImportant : Boolean;

  public
    constructor Create(const PropID: ThtmlcssPropertyID;
                const ValueStr: String; const Important: Boolean); virtual;

    property  Next: ThtmlcssProperty read FNext write FNext;
    property  PropID: ThtmlcssPropertyID read FPropID;
    property  ValueStr: String read FValueStr;
    property  Important: Boolean read FImportant;

    procedure ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
              const ParentStyle: ThtmlcssStyleProperties); virtual;
  end;
  ThtmlcssPropertyClass = Class of ThtmlcssProperty;



{                                                                              }
{ Font                                                                         }
{                                                                              }
type
  { font-family                                                                }
  ThtmlcssFontFamilyProperty = class(ThtmlcssProperty)
  protected
    FValue : ThtmlFontFamilyList;

  public
    constructor Create(const PropID: ThtmlcssPropertyID;
                const ValueStr: String; const Important: Boolean); override;

    procedure ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
              const ParentStyle: ThtmlcssStyleProperties); override;
  end;

  { font-style                                                                 }
  ThtmlcssFontStyleProperty = class(ThtmlcssProperty)
  protected
    FValue : ThtmlFontStyle;

  public
    constructor Create(const PropID: ThtmlcssPropertyID;
                const ValueStr: String; const Important: Boolean); override;

    procedure ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
              const ParentStyle: ThtmlcssStyleProperties); override;
  end;

  { font-variant                                                               }
  ThtmlcssFontVariantProperty = class(ThtmlcssProperty)
  protected
    FValue : ThtmlFontVariant;

  public
    constructor Create(const PropID: ThtmlcssPropertyID;
                const ValueStr: String; const Important: Boolean); override;

    procedure ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
              const ParentStyle: ThtmlcssStyleProperties); override;
  end;

  { font-weight                                                                }
  ThtmlcssFontWeightProperty = class(ThtmlcssProperty)
  protected
    FValue : ThtmlFontWeight;

  public
    constructor Create(const PropID: ThtmlcssPropertyID;
                const ValueStr: String; const Important: Boolean); override;

    procedure ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
              const ParentStyle: ThtmlcssStyleProperties); override;
  end;

  { font-size                                                                  }
  ThtmlcssFontSizeProperty = class(ThtmlcssProperty)
  protected
    FValue : ThtmlFontSize;

  public
    constructor Create(const PropID: ThtmlcssPropertyID;
                const ValueStr: String; const Important: Boolean); override;

    procedure ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
              const ParentStyle: ThtmlcssStyleProperties); override;
  end;

  { font                                                                       }
  ThtmlcssFontProperty = class(ThtmlcssProperty)
  protected
    FFontStyle   : ThtmlFontStyle;
    FFontVariant : ThtmlFontVariant;
    FFontWeight  : ThtmlFontWeight;
    FFontSize    : ThtmlFontSize;
    FLineHeight  : ThtmlFontSize;
    FFontFamily  : ThtmlFontFamilyList;

  public
    constructor Create(const PropID: ThtmlcssPropertyID;
                const ValueStr: String; const Important: Boolean); override;

    procedure ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
              const ParentStyle: ThtmlcssStyleProperties); override;
  end;



{                                                                              }
{ Color and Background                                                         }
{                                                                              }
type
  { color                                                                      }
  ThtmlcssColorProperty = class(ThtmlcssProperty)
  protected
    FValue : ThtmlColor;

  public
    constructor Create(const PropID: ThtmlcssPropertyID;
                const ValueStr: String; const Important: Boolean); override;

    procedure ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
              const ParentStyle: ThtmlcssStyleProperties); override;
  end;

  { background-color                                                           }
  ThtmlcssBackgroundColorProperty = class(ThtmlcssProperty)
  protected
    FValue : ThtmlColor;

  public
    constructor Create(const PropID: ThtmlcssPropertyID;
                const ValueStr: String; const Important: Boolean); override;

    procedure ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
              const ParentStyle: ThtmlcssStyleProperties); override;
  end;

  { background-image                                                           }
  ThtmlcssBackgroundImageProperty = class(ThtmlcssProperty)
  protected
    FValue : ThtmlBackgroundImage;

  public
    constructor Create(const PropID: ThtmlcssPropertyID;
                const ValueStr: String; const Important: Boolean); override;

    procedure ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
              const ParentStyle: ThtmlcssStyleProperties); override;
  end;

  { background-repeat                                                          }
  ThtmlcssBackgroundRepeatProperty = class(ThtmlcssProperty)
  protected
    FValue : ThtmlBackgroundRepeat;

  public
    constructor Create(const PropID: ThtmlcssPropertyID;
                const ValueStr: String; const Important: Boolean); override;

    procedure ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
              const ParentStyle: ThtmlcssStyleProperties); override;
  end;

  { background-attachment                                                      }
  ThtmlcssBackgroundAttachmentProperty = class(ThtmlcssProperty)
  protected
    FValue : ThtmlBackgroundAttachment;

  public
    constructor Create(const PropID: ThtmlcssPropertyID;
                const ValueStr: String; const Important: Boolean); override;

    procedure ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
              const ParentStyle: ThtmlcssStyleProperties); override;
  end;

  { background-position                                                        }
  ThtmlcssBackgroundPositionProperty = class(ThtmlcssProperty)
  protected
    FValue : ThtmlLength;

  public
    constructor Create(const PropID: ThtmlcssPropertyID;
                const ValueStr: String; const Important: Boolean); override;

    procedure ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
              const ParentStyle: ThtmlcssStyleProperties); override;
  end;

  { background                                                                 }
  ThtmlcssBackgroundProperty = class(ThtmlcssProperty)
  protected
    FBackColor  : ThtmlColor;
    FBackImage  : ThtmlBackgroundImage;
    FBackRepeat : ThtmlBackgroundRepeat;
    FBackAttach : ThtmlBackgroundAttachment;

  public
    constructor Create(const PropID: ThtmlcssPropertyID;
                const ValueStr: String; const Important: Boolean); override;

    procedure ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
              const ParentStyle: ThtmlcssStyleProperties); override;
  end;



{                                                                              }
{ Text                                                                         }
{                                                                              }
type
  { word-spacing                                                               }
  ThtmlcssWordSpacingProperty = class(ThtmlcssProperty)
  protected
    FValue : ThtmlSpacing;

  public
    constructor Create(const PropID: ThtmlcssPropertyID;
                const ValueStr: String; const Important: Boolean); override;

    procedure ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
              const ParentStyle: ThtmlcssStyleProperties); override;
  end;

  { letter-spacing                                                             }
  ThtmlcssLetterSpacingProperty = class(ThtmlcssProperty)
  protected
    FValue : ThtmlSpacing;

  public
    constructor Create(const PropID: ThtmlcssPropertyID;
                const ValueStr: String; const Important: Boolean); override;

    procedure ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
              const ParentStyle: ThtmlcssStyleProperties); override;
  end;

  { text-decoration                                                            }
  ThtmlcssTextDecorationProperty = class(ThtmlcssProperty)
  protected
    FValue : ThtmlTextDecoration;

  public
    constructor Create(const PropID: ThtmlcssPropertyID;
                const ValueStr: String; const Important: Boolean); override;

    procedure ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
              const ParentStyle: ThtmlcssStyleProperties); override;
  end;

  { vertical-align                                                             }
  ThtmlcssVerticalAlignProperty = class(ThtmlcssProperty)
  protected
    FValue : ThtmlVerticalAlign;

  public
    constructor Create(const PropID: ThtmlcssPropertyID;
                const ValueStr: String; const Important: Boolean); override;

    procedure ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
              const ParentStyle: ThtmlcssStyleProperties); override;
  end;

  { text-transform                                                             }
  ThtmlcssTextTransformProperty = class(ThtmlcssProperty)
  protected
    FValue : ThtmlTextTransform;

  public
    constructor Create(const PropID: ThtmlcssPropertyID;
                const ValueStr: String; const Important: Boolean); override;

    procedure ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
              const ParentStyle: ThtmlcssStyleProperties); override;
  end;

  { text-align                                                                 }
  ThtmlcssTextAlignProperty = class(ThtmlcssProperty)
  protected
    FValue : ThtmlTextAlignType;

  public
    constructor Create(const PropID: ThtmlcssPropertyID;
                const ValueStr: String; const Important: Boolean); override;

    procedure ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
              const ParentStyle: ThtmlcssStyleProperties); override;
  end;

  { text-indent                                                                }
  ThtmlcssTextIndentProperty = class(ThtmlcssProperty)
  protected
    FValue : ThtmlLength;

  public
    constructor Create(const PropID: ThtmlcssPropertyID;
                const ValueStr: String; const Important: Boolean); override;

    procedure ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
              const ParentStyle: ThtmlcssStyleProperties); override;
  end;

  { line-height                                                                }
  ThtmlcssLineHeightProperty = class(ThtmlcssProperty)
  protected
    FValue : ThtmlLength;

  public
    constructor Create(const PropID: ThtmlcssPropertyID;
                const ValueStr: String; const Important: Boolean); override;

    procedure ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
              const ParentStyle: ThtmlcssStyleProperties); override;
  end;



{                                                                              }
{ Box                                                                          }
{                                                                              }
type
  { margin-top                                                                 }
  ThtmlcssMarginTopProperty = class(ThtmlcssProperty)
  protected
    FValue : ThtmlMargin;

  public
    constructor Create(const PropID: ThtmlcssPropertyID;
                const ValueStr: String; const Important: Boolean); override;

    procedure ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
              const ParentStyle: ThtmlcssStyleProperties); override;
  end;

  { margin-right                                                               }
  ThtmlcssMarginRightProperty = class(ThtmlcssProperty)
  protected
    FValue : ThtmlMargin;

  public
    constructor Create(const PropID: ThtmlcssPropertyID;
                const ValueStr: String; const Important: Boolean); override;

    procedure ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
              const ParentStyle: ThtmlcssStyleProperties); override;
  end;

  { margin-bottom                                                              }
  ThtmlcssMarginBottomProperty = class(ThtmlcssProperty)
  protected
    FValue : ThtmlMargin;

  public
    constructor Create(const PropID: ThtmlcssPropertyID;
                const ValueStr: String; const Important: Boolean); override;

    procedure ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
              const ParentStyle: ThtmlcssStyleProperties); override;
  end;

  { margin-left                                                                }
  ThtmlcssMarginLeftProperty = class(ThtmlcssProperty)
  protected
    FValue : ThtmlMargin;

  public
    constructor Create(const PropID: ThtmlcssPropertyID;
                const ValueStr: String; const Important: Boolean); override;

    procedure ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
              const ParentStyle: ThtmlcssStyleProperties); override;
  end;

  { margin                                                                     }
  ThtmlcssMarginsProperty = class(ThtmlcssProperty)
  protected
    FValue : ThtmlMargins;

  public
    constructor Create(const PropID: ThtmlcssPropertyID;
                const ValueStr: String; const Important: Boolean); override;

    procedure ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
              const ParentStyle: ThtmlcssStyleProperties); override;
  end;

  { padding-top                                                                }
  ThtmlcssPaddingTopProperty = class(ThtmlcssProperty)
  protected
    FValue : ThtmlPadding;

  public
    constructor Create(const PropID: ThtmlcssPropertyID;
                const ValueStr: String; const Important: Boolean); override;

    procedure ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
              const ParentStyle: ThtmlcssStyleProperties); override;
  end;

  { padding-bottom                                                             }
  ThtmlcssPaddingBottomProperty = class(ThtmlcssProperty)
  protected
    FValue : ThtmlPadding;

  public
    constructor Create(const PropID: ThtmlcssPropertyID;
                const ValueStr: String; const Important: Boolean); override;

    procedure ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
              const ParentStyle: ThtmlcssStyleProperties); override;
  end;

  { padding-left                                                               }
  ThtmlcssPaddingLeftProperty = class(ThtmlcssProperty)
  protected
    FValue : ThtmlPadding;

  public
    constructor Create(const PropID: ThtmlcssPropertyID;
                const ValueStr: String; const Important: Boolean); override;

    procedure ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
              const ParentStyle: ThtmlcssStyleProperties); override;
  end;

  { padding-right                                                              }
  ThtmlcssPaddingRightProperty = class(ThtmlcssProperty)
  protected
    FValue : ThtmlPadding;

  public
    constructor Create(const PropID: ThtmlcssPropertyID;
                const ValueStr: String; const Important: Boolean); override;

    procedure ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
              const ParentStyle: ThtmlcssStyleProperties); override;
  end;

  { padding                                                                    }
  ThtmlcssPaddingProperty = class(ThtmlcssProperty)
  protected
    FValue : ThtmlPaddings;

  public
    constructor Create(const PropID: ThtmlcssPropertyID;
                const ValueStr: String; const Important: Boolean); override;

    procedure ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
              const ParentStyle: ThtmlcssStyleProperties); override;
  end;

  { border-top-width                                                           }
  ThtmlcssBorderTopWidthProperty = class(ThtmlcssProperty)
  protected
    FValue : ThtmlBorderWidth;

  public
    constructor Create(const PropID: ThtmlcssPropertyID;
                const ValueStr: String; const Important: Boolean); override;

    procedure ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
              const ParentStyle: ThtmlcssStyleProperties); override;
  end;

  { border-right-width                                                         }
  ThtmlcssBorderRightWidthProperty = class(ThtmlcssProperty)
  protected
    FValue : ThtmlBorderWidth;

  public
    constructor Create(const PropID: ThtmlcssPropertyID;
                const ValueStr: String; const Important: Boolean); override;

    procedure ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
              const ParentStyle: ThtmlcssStyleProperties); override;
  end;

  { border-bottom-width                                                        }
  ThtmlcssBorderBottomWidthProperty = class(ThtmlcssProperty)
  protected
    FValue : ThtmlBorderWidth;

  public
    constructor Create(const PropID: ThtmlcssPropertyID;
                const ValueStr: String; const Important: Boolean); override;

    procedure ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
              const ParentStyle: ThtmlcssStyleProperties); override;
  end;

  { border-left-width                                                          }
  ThtmlcssBorderLeftWidthProperty = class(ThtmlcssProperty)
  protected
    FValue : ThtmlBorderWidth;

  public
    constructor Create(const PropID: ThtmlcssPropertyID;
                const ValueStr: String; const Important: Boolean); override;

    procedure ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
              const ParentStyle: ThtmlcssStyleProperties); override;
  end;

  { border-width                                                               }
  ThtmlcssBorderWidthsProperty = class(ThtmlcssProperty)
  protected
    FValue : ThtmlBorderWidths;

  public
    constructor Create(const PropID: ThtmlcssPropertyID;
                const ValueStr: String; const Important: Boolean); override;

    procedure ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
              const ParentStyle: ThtmlcssStyleProperties); override;
  end;

  { border-colour                                                              }
  ThtmlcssBorderColProperty = class(ThtmlcssProperty)
  protected
    FValue : ThtmlColor;

  public
    constructor Create(const PropID: ThtmlcssPropertyID;
                const ValueStr: String; const Important: Boolean); override;

    procedure ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
              const ParentStyle: ThtmlcssStyleProperties); override;
  end;

  { border-style                                                               }
  ThtmlcssBorderStyleProperty = class(ThtmlcssProperty)
  protected
    FValue : ThtmlBorderStyle;

  public
    constructor Create(const PropID: ThtmlcssPropertyID;
                const ValueStr: String; const Important: Boolean); override;

    procedure ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
              const ParentStyle: ThtmlcssStyleProperties); override;
  end;

  { border-top                                                                 }
  ThtmlcssBorderTopProperty = class(ThtmlcssProperty)
  protected
    FWidth : ThtmlLength;
    FStyle : ThtmlBorderStyle;
    FColor : ThtmlColor;

  public
    constructor Create(const PropID: ThtmlcssPropertyID;
                const ValueStr: String; const Important: Boolean); override;

    procedure ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
              const ParentStyle: ThtmlcssStyleProperties); override;
  end;

  { border-right                                                               }
  ThtmlcssBorderRightProperty = class(ThtmlcssProperty)
  protected
    FWidth : ThtmlLength;
    FStyle : ThtmlBorderStyle;
    FColor : ThtmlColor;

  public
    constructor Create(const PropID: ThtmlcssPropertyID;
                const ValueStr: String; const Important: Boolean); override;

    procedure ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
              const ParentStyle: ThtmlcssStyleProperties); override;
  end;

  { border-bottom                                                              }
  ThtmlcssBorderBottomProperty = class(ThtmlcssProperty)
  protected
    FWidth : ThtmlLength;
    FStyle : ThtmlBorderStyle;
    FColor : ThtmlColor;

  public
    constructor Create(const PropID: ThtmlcssPropertyID;
                const ValueStr: String; const Important: Boolean); override;

    procedure ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
              const ParentStyle: ThtmlcssStyleProperties); override;
  end;

  { border-left                                                                }
  ThtmlcssBorderLeftProperty = class(ThtmlcssProperty)
  protected
    FWidth : ThtmlLength;
    FStyle : ThtmlBorderStyle;
    FColor : ThtmlColor;

  public
    constructor Create(const PropID: ThtmlcssPropertyID;
                const ValueStr: String; const Important: Boolean); override;

    procedure ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
              const ParentStyle: ThtmlcssStyleProperties); override;
  end;

  { border                                                                     }
  ThtmlcssBorderProperty = class(ThtmlcssProperty)
  protected
    FWidth : ThtmlLength;
    FStyle : ThtmlBorderStyle;
    FColor : ThtmlColor;

  public
    constructor Create(const PropID: ThtmlcssPropertyID;
                const ValueStr: String; const Important: Boolean); override;

    procedure ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
              const ParentStyle: ThtmlcssStyleProperties); override;
  end;

  { width                                                                      }
  ThtmlcssWidthProperty = class(ThtmlcssProperty)
  protected
    FValue : ThtmlLength;

  public
    constructor Create(const PropID: ThtmlcssPropertyID;
                const ValueStr: String; const Important: Boolean); override;

    procedure ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
              const ParentStyle: ThtmlcssStyleProperties); override;
  end;

  { height                                                                     }
  ThtmlcssHeightProperty = class(ThtmlcssProperty)
  protected
    FValue : ThtmlLength;

  public
    constructor Create(const PropID: ThtmlcssPropertyID;
                const ValueStr: String; const Important: Boolean); override;

    procedure ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
              const ParentStyle: ThtmlcssStyleProperties); override;
  end;

  { float                                                                      }
  ThtmlcssFloatProperty = class(ThtmlcssProperty)
  protected
    FValue : ThtmlFloatType;

  public
    constructor Create(const PropID: ThtmlcssPropertyID;
                const ValueStr: String; const Important: Boolean); override;

    procedure ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
              const ParentStyle: ThtmlcssStyleProperties); override;
  end;

  { clear                                                                      }
  ThtmlcssClearProperty = class(ThtmlcssProperty)
  protected
    FValue : ThtmlClearType;

  public
    constructor Create(const PropID: ThtmlcssPropertyID;
                const ValueStr: String; const Important: Boolean); override;

    procedure ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
              const ParentStyle: ThtmlcssStyleProperties); override;
  end;



{                                                                              }
{ Classification                                                               }
{                                                                              }
type
  { display                                                                    }
  ThtmlcssDisplayProperty = class(ThtmlcssProperty)
  protected
    FValue : ThtmlDisplayType;

  public
    constructor Create(const PropID: ThtmlcssPropertyID;
                const ValueStr: String; const Important: Boolean); override;

    procedure ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
              const ParentStyle: ThtmlcssStyleProperties); override;
  end;

  { white-space                                                                }
  ThtmlcssWhiteSpaceProperty = class(ThtmlcssProperty)
  protected
    FValue : ThtmlWhiteSpaceType;

  public
    constructor Create(const PropID: ThtmlcssPropertyID;
                const ValueStr: String; const Important: Boolean); override;

    procedure ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
              const ParentStyle: ThtmlcssStyleProperties); override;
  end;

  { list-style-type                                                            }
  ThtmlcssListStyleTypeProperty = class(ThtmlcssProperty)
  protected
    FValue : ThtmlListStyleType;

  public
    constructor Create(const PropID: ThtmlcssPropertyID;
                const ValueStr: String; const Important: Boolean); override;

    procedure ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
              const ParentStyle: ThtmlcssStyleProperties); override;
  end;

  { list-style-image                                                           }
  ThtmlcssListStyleImageProperty = class(ThtmlcssProperty)
  protected
    FValue : String;

  public
    constructor Create(const PropID: ThtmlcssPropertyID;
                const ValueStr: String; const Important: Boolean); override;

    procedure ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
              const ParentStyle: ThtmlcssStyleProperties); override;
  end;

  { list-style-position                                                        }
  ThtmlcssListStylePositionProperty = class(ThtmlcssProperty)
  protected
    FValue : ThtmlListStylePositionType;

  public
    constructor Create(const PropID: ThtmlcssPropertyID;
                const ValueStr: String; const Important: Boolean); override;

    procedure ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
              const ParentStyle: ThtmlcssStyleProperties); override;
  end;

  { list-style                                                                 }
  ThtmlcssListStyleProperty = class(ThtmlcssProperty)
  protected
    FValue : ThtmlListStyleType;

  public
    constructor Create(const PropID: ThtmlcssPropertyID;
                const ValueStr: String; const Important: Boolean); override;

    procedure ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
              const ParentStyle: ThtmlcssStyleProperties); override;
  end;



{ Utility functions                                                            }
function  htmlcssGetPropertyClass(const PropertyID: ThtmlcssPropertyID): ThtmlcssPropertyClass;



implementation

uses
  { System }
  {$IFDEF DELPHI6_UP}
  Windows,
  {$ENDIF}
  System.UITypes;



{                                                                              }
{ ThtmlcssStyleProperties                                                      }
{                                                                              }
procedure htmlcssStyleResetNonInherited(var Style: ThtmlcssStyleProperties);
begin
  with Style do
    begin
      BackColor.ColorType := colorDefault;
      BackAttach := battScroll;
      BackImage.ImageType := backimgDefault;
      BackRepeat := brepRepeat;
      BorderCol.ColorType := colorDefault;
      BorderWidths.Top.WidthType := borwidthDefault;
      BorderWidths.Left.WidthType := borwidthDefault;
      BorderWidths.Bottom.WidthType := borwidthDefault;
      BorderWidths.Right.WidthType := borwidthDefault;
      Margins.Top.LengthType := lenDefault;
      Margins.Left.LengthType := lenDefault;
      Margins.Bottom.LengthType := lenDefault;
      Margins.Right.LengthType := lenDefault;
      Width.LengthType := lenDefault;
      Height.LengthType := lenDefault;
      SetRect(Padding, -1, -1, -1, -1);
      Clear := clearDefault;
      Display := displayDefault;
      ListStyle := liststyleDefault;
      Float := floatNone;
    end;
end;



{                                                                              }
{ ThtmlcssProperty                                                             }
{                                                                              }
constructor ThtmlcssProperty.Create(const PropID: ThtmlcssPropertyID;
    const ValueStr: String; const Important: Boolean);
begin
  inherited Create;
  FPropID := PropID;
  FValueStr := ValueStr;
  FImportant := Important;
end;

procedure ThtmlcssProperty.ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
    const ParentStyle: ThtmlcssStyleProperties);
begin
end;



{                                                                              }
{ font-family                                                                  }
{                                                                              }
constructor ThtmlcssFontFamilyProperty.Create(const PropID: ThtmlcssPropertyID;
    const ValueStr: String; const Important: Boolean);
begin
  inherited Create(PropID, ValueStr, Important);
  FValue := htmlDecodeFontFamilyList(ValueStr);
end;

procedure ThtmlcssFontFamilyProperty.ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
    const ParentStyle: ThtmlcssStyleProperties);
begin
  StyleInfo.FontFamily := FValue;
end;



{                                                                              }
{ font-style                                                                   }
{                                                                              }
constructor ThtmlcssFontStyleProperty.Create(const PropID: ThtmlcssPropertyID;
    const ValueStr: String; const Important: Boolean);
begin
  inherited Create(PropID, ValueStr, Important);
  FValue := htmlcssDecodeFontStyle(ValueStr);
end;

procedure ThtmlcssFontStyleProperty.ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
    const ParentStyle: ThtmlcssStyleProperties);
begin
  StyleInfo.FontStyle := FValue;
end;



{                                                                              }
{ font-variant                                                                 }
{                                                                              }
constructor ThtmlcssFontVariantProperty.Create(const PropID: ThtmlcssPropertyID;
    const ValueStr: String; const Important: Boolean);
begin
  inherited Create(PropID, ValueStr, Important);
  FValue := htmlcssDecodeFontVariant(ValueStr);
end;

procedure ThtmlcssFontVariantProperty.ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
    const ParentStyle: ThtmlcssStyleProperties);
begin
  StyleInfo.FontVariant := FValue;
end;



{                                                                              }
{ font-weight                                                                  }
{                                                                              }
constructor ThtmlcssFontWeightProperty.Create(const PropID: ThtmlcssPropertyID;
    const ValueStr: String; const Important: Boolean);
begin
  inherited Create(PropID, ValueStr, Important);
  FValue := htmlcssDecodeFontWeight(ValueStr);
end;

procedure ThtmlcssFontWeightProperty.ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
    const ParentStyle: ThtmlcssStyleProperties);
begin
  StyleInfo.FontWeight := FValue;
end;



{                                                                              }
{ font-size                                                                    }
{                                                                              }
constructor ThtmlcssFontSizeProperty.Create(const PropID: ThtmlcssPropertyID;
    const ValueStr: String; const Important: Boolean);
begin
  inherited Create(PropID, ValueStr, Important);
  FValue := htmlcssDecodeFontSize(ValueStr);
end;

procedure ThtmlcssFontSizeProperty.ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
    const ParentStyle: ThtmlcssStyleProperties);
begin
  StyleInfo.FontSize := FValue;
  htmlResolveRelativeFontSize(StyleInfo.FontSize, ParentStyle.FontSize);
end;



{                                                                              }
{ font                                                                         }
{                                                                              }
constructor ThtmlcssFontProperty.Create(const PropID: ThtmlcssPropertyID;
    const ValueStr: String; const Important: Boolean);
begin
  inherited Create(PropID, ValueStr, Important);
  htmlcssDecodeFont(ValueStr, FFontStyle, FFontVariant, FFontWeight,
      FFontSize, FFontFamily);
end;

procedure ThtmlcssFontProperty.ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
    const ParentStyle: ThtmlcssStyleProperties);
begin
  if FFontStyle <> fstyleDefault then
    StyleInfo.FontStyle := FFontStyle;
  if FFontVariant <> fvariantDefault then
    StyleInfo.FontVariant := FFontVariant;
  if FFontWeight <> fweightDefault then
    StyleInfo.FontWeight := FFontWeight;
  if FFontSize.SizeType <> fsizeDefault then
    StyleInfo.FontSize := FFontSize;
  if FFontFamily <> nil then
    StyleInfo.FontFamily := FFontFamily;
end;



{                                                                              }
{ color                                                                        }
{                                                                              }
constructor ThtmlcssColorProperty.Create(const PropID: ThtmlcssPropertyID;
    const ValueStr: String; const Important: Boolean);
begin
  inherited Create(PropID, ValueStr, Important);
  FValue := htmlcssDecodeColor(ValueStr);
end;

procedure ThtmlcssColorProperty.ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
    const ParentStyle: ThtmlcssStyleProperties);
begin
  StyleInfo.FontColor := FValue;
end;



{                                                                              }
{ background-color                                                             }
{                                                                              }
constructor ThtmlcssBackgroundColorProperty.Create(const PropID: ThtmlcssPropertyID;
    const ValueStr: String; const Important: Boolean);
begin
  inherited Create(PropID, ValueStr, Important);
  FValue := htmlcssDecodeColor(ValueStr);
end;

procedure ThtmlcssBackgroundColorProperty.ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
    const ParentStyle: ThtmlcssStyleProperties);
begin
  StyleInfo.BackColor := FValue;
end;




{                                                                              }
{ background-image                                                             }
{                                                                              }
constructor ThtmlcssBackgroundImageProperty.Create(const PropID: ThtmlcssPropertyID;
    const ValueStr: String; const Important: Boolean);
begin
  inherited Create(PropID, ValueStr, Important);
  FValue := htmlcssDecodeBackgroundImage(ValueStr);
end;

procedure ThtmlcssBackgroundImageProperty.ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
    const ParentStyle: ThtmlcssStyleProperties);
begin
  StyleInfo.BackImage := FValue;
end;



{                                                                              }
{ background-repeat                                                            }
{                                                                              }
constructor ThtmlcssBackgroundRepeatProperty.Create(const PropID: ThtmlcssPropertyID;
    const ValueStr: String; const Important: Boolean);
begin
  inherited Create(PropID, ValueStr, Important);
  FValue := htmlcssDecodeBackgroundRepeat(ValueStr);
end;

procedure ThtmlcssBackgroundRepeatProperty.ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
    const ParentStyle: ThtmlcssStyleProperties);
begin
  StyleInfo.BackRepeat := FValue;
end;



{                                                                              }
{ background-attachment                                                        }
{                                                                              }
constructor ThtmlcssBackgroundAttachmentProperty.Create(const PropID: ThtmlcssPropertyID;
    const ValueStr: String; const Important: Boolean);
begin
  inherited Create(PropID, ValueStr, Important);
  FValue := htmlcssDecodeBackgroundAttachment(ValueStr);
end;

procedure ThtmlcssBackgroundAttachmentProperty.ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
    const ParentStyle: ThtmlcssStyleProperties);
begin
  StyleInfo.BackAttach := FValue;
end;



{                                                                              }
{ background-position                                                          }
{ ::= [<percentage> | <length>](1,2) | [top | center | bottom] ||              }
{     [left | center | right]                                                  }
{ TODO: Parse correctly                                                        }
{                                                                              }
constructor ThtmlcssBackgroundPositionProperty.Create(const PropID: ThtmlcssPropertyID;
    const ValueStr: String; const Important: Boolean);
begin
  inherited Create(PropID, ValueStr, Important);
  FValue := htmlcssDecodeLength(ValueStr);
end;

procedure ThtmlcssBackgroundPositionProperty.ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
    const ParentStyle: ThtmlcssStyleProperties);
begin
  // TODO: Implement
end;



{                                                                              }
{ background                                                                   }
{                                                                              }
constructor ThtmlcssBackgroundProperty.Create(const PropID: ThtmlcssPropertyID;
    const ValueStr: String; const Important: Boolean);
begin
  inherited Create(PropID, ValueStr, Important);
  htmlcssDecodeBackground(ValueStr, FBackColor, FBackImage, FBackRepeat,
      FBackAttach);
end;

procedure ThtmlcssBackgroundProperty.ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
    const ParentStyle: ThtmlcssStyleProperties);
begin
  if FBackColor.ColorType <> colorDefault then
    StyleInfo.BackColor := FBackColor;
  if FBackImage.ImageType <> backimgDefault then
    StyleInfo.BackImage := FBackImage;
  if FBackRepeat <> brepDefault then
    StyleInfo.BackRepeat := FBackRepeat;
  if FBackAttach <> battDefault then
    StyleInfo.BackAttach := FBackAttach;
end;



{                                                                              }
{ word-spacing                                                                 }
{                                                                              }
constructor ThtmlcssWordSpacingProperty.Create(const PropID: ThtmlcssPropertyID;
    const ValueStr: String; const Important: Boolean);
begin
  inherited Create(PropID, ValueStr, Important);
  FValue := htmlcssDecodeWordSpacing(ValueStr);
end;

procedure ThtmlcssWordSpacingProperty.ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
    const ParentStyle: ThtmlcssStyleProperties);
begin
  StyleInfo.WordSpacing := FValue;
end;



{                                                                              }
{ letter-spacing                                                               }
{                                                                              }
constructor ThtmlcssLetterSpacingProperty.Create(const PropID: ThtmlcssPropertyID;
    const ValueStr: String; const Important: Boolean);
begin
  inherited Create(PropID, ValueStr, Important);
  FValue := htmlcssDecodeLetterSpacing(ValueStr);
end;

procedure ThtmlcssLetterSpacingProperty.ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
    const ParentStyle: ThtmlcssStyleProperties);
begin
  StyleInfo.LetterSpacing := FValue;
end;



{                                                                              }
{ text-decoration                                                              }
{                                                                              }
constructor ThtmlcssTextDecorationProperty.Create(const PropID: ThtmlcssPropertyID;
    const ValueStr: String; const Important: Boolean);
begin
  inherited Create(PropID, ValueStr, Important);
  FValue := htmlcssDecodeTextDecoration(ValueStr);
end;

procedure ThtmlcssTextDecorationProperty.ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
    const ParentStyle: ThtmlcssStyleProperties);
begin
  StyleInfo.TextDecoration := FValue;
end;



{                                                                              }
{ vertical-align                                                               }
{                                                                              }
constructor ThtmlcssVerticalAlignProperty.Create(const PropID: ThtmlcssPropertyID;
    const ValueStr: String; const Important: Boolean);
begin
  inherited Create(PropID, ValueStr, Important);
  FValue := htmlcssDecodeVerticalAlign(ValueStr);
end;

procedure ThtmlcssVerticalAlignProperty.ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
    const ParentStyle: ThtmlcssStyleProperties);
begin
  StyleInfo.VerticalAlign := FValue;
end;



{                                                                              }
{ text-transform                                                               }
{                                                                              }
constructor ThtmlcssTextTransformProperty.Create(const PropID: ThtmlcssPropertyID;
    const ValueStr: String; const Important: Boolean);
begin
  inherited Create(PropID, ValueStr, Important);
  FValue := htmlcssDecodeTextTransform(ValueStr);
end;

procedure ThtmlcssTextTransformProperty.ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
    const ParentStyle: ThtmlcssStyleProperties);
begin
  StyleInfo.TextTransform := FValue;
end;



{                                                                              }
{ text-align                                                                   }
{                                                                              }
constructor ThtmlcssTextAlignProperty.Create(const PropID: ThtmlcssPropertyID;
    const ValueStr: String; const Important: Boolean);
begin
  inherited Create(PropID, ValueStr, Important);
  FValue := htmlDecodeTextAlignType(ValueStr);
end;

procedure ThtmlcssTextAlignProperty.ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
    const ParentStyle: ThtmlcssStyleProperties);
begin
  StyleInfo.TextAlign := FValue;
end;



{                                                                              }
{ text-indent                                                                  }
{                                                                              }
constructor ThtmlcssTextIndentProperty.Create(const PropID: ThtmlcssPropertyID;
    const ValueStr: String; const Important: Boolean);
begin
  inherited Create(PropID, ValueStr, Important);
  FValue := htmlcssDecodeTextIndent(ValueStr);
end;

procedure ThtmlcssTextIndentProperty.ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
    const ParentStyle: ThtmlcssStyleProperties);
begin
  StyleInfo.TextIndent := FValue;
end;



{                                                                              }
{ line-height                                                                  }
{                                                                              }
constructor ThtmlcssLineHeightProperty.Create(const PropID: ThtmlcssPropertyID;
    const ValueStr: String; const Important: Boolean);
begin
  inherited Create(PropID, ValueStr, Important);
  FValue := htmlcssDecodeLength(ValueStr);
end;

procedure ThtmlcssLineHeightProperty.ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
    const ParentStyle: ThtmlcssStyleProperties);
begin
  StyleInfo.LineHeight := FValue;
end;



{                                                                              }
{ margin-top                                                                   }
{                                                                              }
constructor ThtmlcssMarginTopProperty.Create(const PropID: ThtmlcssPropertyID;
    const ValueStr: String; const Important: Boolean);
begin
  inherited Create(PropID, ValueStr, Important);
  FValue := htmlcssDecodeMargin(ValueStr);
end;

procedure ThtmlcssMarginTopProperty.ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
    const ParentStyle: ThtmlcssStyleProperties);
begin
  StyleInfo.Margins.Top := FValue;
end;



{                                                                              }
{ margin-right                                                                 }
{                                                                              }
constructor ThtmlcssMarginRightProperty.Create(const PropID: ThtmlcssPropertyID;
    const ValueStr: String; const Important: Boolean);
begin
  inherited Create(PropID, ValueStr, Important);
  FValue := htmlcssDecodeMargin(ValueStr);
end;

procedure ThtmlcssMarginRightProperty.ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
    const ParentStyle: ThtmlcssStyleProperties);
begin
  StyleInfo.Margins.Right := FValue;
end;



{                                                                              }
{ margin-bottom                                                                }
{                                                                              }
constructor ThtmlcssMarginBottomProperty.Create(const PropID: ThtmlcssPropertyID;
    const ValueStr: String; const Important: Boolean);
begin
  inherited Create(PropID, ValueStr, Important);
  FValue := htmlcssDecodeMargin(ValueStr);
end;

procedure ThtmlcssMarginBottomProperty.ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
    const ParentStyle: ThtmlcssStyleProperties);
begin
  StyleInfo.Margins.Bottom := FValue;
end;



{                                                                              }
{ margin-left                                                                  }
{                                                                              }
constructor ThtmlcssMarginLeftProperty.Create(const PropID: ThtmlcssPropertyID;
    const ValueStr: String; const Important: Boolean);
begin
  inherited Create(PropID, ValueStr, Important);
  FValue := htmlcssDecodeMargin(ValueStr);
end;

procedure ThtmlcssMarginLeftProperty.ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
    const ParentStyle: ThtmlcssStyleProperties);
begin
  StyleInfo.Margins.Left := FValue;
end;



{                                                                              }
{ margin                                                                       }
{                                                                              }
constructor ThtmlcssMarginsProperty.Create(const PropID: ThtmlcssPropertyID;
    const ValueStr: String; const Important: Boolean);
begin
  inherited Create(PropID, ValueStr, Important);
  FValue := htmlcssDecodeMargins(ValueStr);
end;

procedure ThtmlcssMarginsProperty.ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
    const ParentStyle: ThtmlcssStyleProperties);
begin
  StyleInfo.Margins := FValue;
end;



{                                                                              }
{ padding-top                                                                  }
{                                                                              }
constructor ThtmlcssPaddingTopProperty.Create(const PropID: ThtmlcssPropertyID;
    const ValueStr: String; const Important: Boolean);
begin
  inherited Create(PropID, ValueStr, Important);
  FValue := htmlcssDecodePadding(ValueStr);
end;

procedure ThtmlcssPaddingTopProperty.ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
    const ParentStyle: ThtmlcssStyleProperties);
begin
  StyleInfo.Padding.Top := htmlResolveLengthPixels(FValue, 0);
end;



{                                                                              }
{ padding-left                                                                 }
{                                                                              }
constructor ThtmlcssPaddingLeftProperty.Create(const PropID: ThtmlcssPropertyID;
    const ValueStr: String; const Important: Boolean);
begin
  inherited Create(PropID, ValueStr, Important);
  FValue := htmlcssDecodePadding(ValueStr);
end;

procedure ThtmlcssPaddingLeftProperty.ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
    const ParentStyle: ThtmlcssStyleProperties);
begin
  StyleInfo.Padding.Left := htmlResolveLengthPixels(FValue, 0);
end;



{                                                                              }
{ padding-bottom                                                               }
{                                                                              }
constructor ThtmlcssPaddingBottomProperty.Create(const PropID: ThtmlcssPropertyID;
    const ValueStr: String; const Important: Boolean);
begin
  inherited Create(PropID, ValueStr, Important);
  FValue := htmlcssDecodePadding(ValueStr);
end;

procedure ThtmlcssPaddingBottomProperty.ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
    const ParentStyle: ThtmlcssStyleProperties);
begin
  StyleInfo.Padding.Bottom := htmlResolveLengthPixels(FValue, 0);
end;



{                                                                              }
{ padding-right                                                                }
{                                                                              }
constructor ThtmlcssPaddingRightProperty.Create(const PropID: ThtmlcssPropertyID;
    const ValueStr: String; const Important: Boolean);
begin
  inherited Create(PropID, ValueStr, Important);
  FValue := htmlcssDecodePadding(ValueStr);
end;

procedure ThtmlcssPaddingRightProperty.ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
    const ParentStyle: ThtmlcssStyleProperties);
begin
  StyleInfo.Padding.Right := htmlResolveLengthPixels(FValue, 0);
end;



{                                                                              }
{ padding                                                                      }
{                                                                              }
constructor ThtmlcssPaddingProperty.Create(const PropID: ThtmlcssPropertyID;
    const ValueStr: String; const Important: Boolean);
begin
  inherited Create(PropID, ValueStr, Important);
  FValue := htmlcssDecodePaddings(ValueStr);
end;

procedure ThtmlcssPaddingProperty.ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
    const ParentStyle: ThtmlcssStyleProperties);
begin
  htmlApplyResolvedPaddings(StyleInfo.Padding, htmlResolvePaddings(FValue));
end;



{                                                                              }
{ border-top-width                                                             }
{                                                                              }
constructor ThtmlcssBorderTopWidthProperty.Create(const PropID: ThtmlcssPropertyID;
    const ValueStr: String; const Important: Boolean);
begin
  inherited Create(PropID, ValueStr, Important);
  FValue := htmlcssDecodeBorderWidth(ValueStr);
end;

procedure ThtmlcssBorderTopWidthProperty.ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
    const ParentStyle: ThtmlcssStyleProperties);
begin
  StyleInfo.BorderWidths.Top := FValue;
end;



{                                                                              }
{ border-right-width                                                           }
{                                                                              }
constructor ThtmlcssBorderRightWidthProperty.Create(const PropID: ThtmlcssPropertyID;
    const ValueStr: String; const Important: Boolean);
begin
  inherited Create(PropID, ValueStr, Important);
  FValue := htmlcssDecodeBorderWidth(ValueStr);
end;

procedure ThtmlcssBorderRightWidthProperty.ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
    const ParentStyle: ThtmlcssStyleProperties);
begin
  StyleInfo.BorderWidths.Right := FValue;
end;



{                                                                              }
{ border-bottom-width                                                          }
{                                                                              }
constructor ThtmlcssBorderBottomWidthProperty.Create(const PropID: ThtmlcssPropertyID;
    const ValueStr: String; const Important: Boolean);
begin
  inherited Create(PropID, ValueStr, Important);
  FValue := htmlcssDecodeBorderWidth(ValueStr);
end;

procedure ThtmlcssBorderBottomWidthProperty.ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
    const ParentStyle: ThtmlcssStyleProperties);
begin
  StyleInfo.BorderWidths.Bottom := FValue;
end;



{                                                                              }
{ border-left-width                                                            }
{                                                                              }
constructor ThtmlcssBorderLeftWidthProperty.Create(const PropID: ThtmlcssPropertyID;
    const ValueStr: String; const Important: Boolean);
begin
  inherited Create(PropID, ValueStr, Important);
  FValue := htmlcssDecodeBorderWidth(ValueStr);
end;

procedure ThtmlcssBorderLeftWidthProperty.ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
    const ParentStyle: ThtmlcssStyleProperties);
begin
  StyleInfo.BorderWidths.Left := FValue;
end;



{                                                                              }
{ border-width                                                                 }
{                                                                              }
constructor ThtmlcssBorderWidthsProperty.Create(const PropID: ThtmlcssPropertyID;
    const ValueStr: String; const Important: Boolean);
begin
  inherited Create(PropID, ValueStr, Important);
  FValue := htmlcssDecodeBorderWidths(ValueStr);
end;

procedure ThtmlcssBorderWidthsProperty.ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
    const ParentStyle: ThtmlcssStyleProperties);
begin
  StyleInfo.BorderWidths := FValue;
end;



{                                                                              }
{ border-color                                                                 }
{                                                                              }
constructor ThtmlcssBorderColProperty.Create(const PropID: ThtmlcssPropertyID;
    const ValueStr: String; const Important: Boolean);
begin
  inherited Create(PropID, ValueStr, Important);
  FValue := htmlcssDecodeColor(ValueStr);
end;

procedure ThtmlcssBorderColProperty.ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
    const ParentStyle: ThtmlcssStyleProperties);
begin
  StyleInfo.BorderCol := FValue;
end;



{                                                                              }
{ border-style                                                                 }
{                                                                              }
constructor ThtmlcssBorderStyleProperty.Create(const PropID: ThtmlcssPropertyID;
    const ValueStr: String; const Important: Boolean);
begin
  inherited Create(PropID, ValueStr, Important);
  FValue := htmlcssDecodeBorderStyle(ValueStr);
end;

procedure ThtmlcssBorderStyleProperty.ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
    const ParentStyle: ThtmlcssStyleProperties);
begin
  StyleInfo.BorderStyle := FValue;
end;



{                                                                              }
{ border-top                                                                   }
{                                                                              }
constructor ThtmlcssBorderTopProperty.Create(const PropID: ThtmlcssPropertyID;
    const ValueStr: String; const Important: Boolean);
begin
  inherited Create(PropID, ValueStr, Important);
  FWidth := htmlcssDecodeLength(ValueStr);
  FStyle := htmlcssDecodeBorderStyle(ValueStr);
  FColor := htmlcssDecodeColor(ValueStr);
end;

procedure ThtmlcssBorderTopProperty.ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
    const ParentStyle: ThtmlcssStyleProperties);
begin
  if FWidth.LengthType <> lenDefault then
    begin
      StyleInfo.BorderWidths.Top.WidthType := borwidthLength;
      StyleInfo.BorderWidths.Top.Len := FWidth;
    end;
end;



{                                                                              }
{ border-right                                                                 }
{                                                                              }
constructor ThtmlcssBorderRightProperty.Create(const PropID: ThtmlcssPropertyID;
    const ValueStr: String; const Important: Boolean);
begin
  inherited Create(PropID, ValueStr, Important);
  FWidth := htmlcssDecodeLength(ValueStr);
  FStyle := htmlcssDecodeBorderStyle(ValueStr);
  FColor := htmlcssDecodeColor(ValueStr);
end;

procedure ThtmlcssBorderRightProperty.ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
    const ParentStyle: ThtmlcssStyleProperties);
begin
  if FWidth.LengthType <> lenDefault then
    begin
      StyleInfo.BorderWidths.Right.WidthType := borwidthLength;
      StyleInfo.BorderWidths.Right.Len := FWidth;
    end;
end;



{                                                                              }
{ border-bottom                                                                }
{                                                                              }
constructor ThtmlcssBorderBottomProperty.Create(const PropID: ThtmlcssPropertyID;
    const ValueStr: String; const Important: Boolean);
begin
  inherited Create(PropID, ValueStr, Important);
  FWidth := htmlcssDecodeLength(ValueStr);
  FStyle := htmlcssDecodeBorderStyle(ValueStr);
  FColor := htmlcssDecodeColor(ValueStr);
end;

procedure ThtmlcssBorderBottomProperty.ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
    const ParentStyle: ThtmlcssStyleProperties);
begin
  if FWidth.LengthType <> lenDefault then
    begin
      StyleInfo.BorderWidths.Bottom.WidthType := borwidthLength;
      StyleInfo.BorderWidths.Bottom.Len := FWidth;
    end;
end;



{                                                                              }
{ border-left                                                                  }
{                                                                              }
constructor ThtmlcssBorderLeftProperty.Create(const PropID: ThtmlcssPropertyID;
    const ValueStr: String; const Important: Boolean);
begin
  inherited Create(PropID, ValueStr, Important);
  FWidth := htmlcssDecodeLength(ValueStr);
  FStyle := htmlcssDecodeBorderStyle(ValueStr);
  FColor := htmlcssDecodeColor(ValueStr);
end;

procedure ThtmlcssBorderLeftProperty.ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
    const ParentStyle: ThtmlcssStyleProperties);
begin
  if FWidth.LengthType <> lenDefault then
    begin
      StyleInfo.BorderWidths.Left.WidthType := borwidthLength;
      StyleInfo.BorderWidths.Left.Len := FWidth;
    end;
end;



{                                                                              }
{ border                                                                       }
{                                                                              }
constructor ThtmlcssBorderProperty.Create(const PropID: ThtmlcssPropertyID;
    const ValueStr: String; const Important: Boolean);
begin
  inherited Create(PropID, ValueStr, Important);
  FWidth := htmlcssDecodeLength(ValueStr);
  FStyle := htmlcssDecodeBorderStyle(ValueStr);
  FColor := htmlcssDecodeColor(ValueStr);
end;

procedure ThtmlcssBorderProperty.ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
    const ParentStyle: ThtmlcssStyleProperties);
begin
end;



{                                                                              }
{ width                                                                        }
{                                                                              }
constructor ThtmlcssWidthProperty.Create(const PropID: ThtmlcssPropertyID;
    const ValueStr: String; const Important: Boolean);
begin
  inherited Create(PropID, ValueStr, Important);
  FValue := htmlcssDecodeLength(ValueStr);
end;

procedure ThtmlcssWidthProperty.ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
    const ParentStyle: ThtmlcssStyleProperties);
begin
  StyleInfo.Width := FValue;
end;



{                                                                              }
{ height                                                                       }
{                                                                              }
constructor ThtmlcssHeightProperty.Create(const PropID: ThtmlcssPropertyID;
    const ValueStr: String; const Important: Boolean);
begin
  inherited Create(PropID, ValueStr, Important);
  FValue := htmlcssDecodeLength(ValueStr);
end;

procedure ThtmlcssHeightProperty.ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
    const ParentStyle: ThtmlcssStyleProperties);
begin
  StyleInfo.Height := FValue;
end;



{                                                                              }
{ float                                                                        }
{                                                                              }
constructor ThtmlcssFloatProperty.Create(const PropID: ThtmlcssPropertyID;
    const ValueStr: String; const Important: Boolean);
begin
  inherited Create(PropID, ValueStr, Important);
  FValue := htmlcssDecodeFloatType(ValueStr);
end;

procedure ThtmlcssFloatProperty.ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
    const ParentStyle: ThtmlcssStyleProperties);
begin
  StyleInfo.Float := FValue;
end;



{                                                                              }
{ clear                                                                        }
{                                                                              }
constructor ThtmlcssClearProperty.Create(const PropID: ThtmlcssPropertyID;
    const ValueStr: String; const Important: Boolean);
begin
  inherited Create(PropID, ValueStr, Important);
  FValue := htmlDecodeClearType(ValueStr);
end;

procedure ThtmlcssClearProperty.ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
    const ParentStyle: ThtmlcssStyleProperties);
begin
  StyleInfo.Clear := FValue;
end;



{                                                                              }
{ display                                                                      }
{                                                                              }
constructor ThtmlcssDisplayProperty.Create(const PropID: ThtmlcssPropertyID;
    const ValueStr: String; const Important: Boolean);
begin
  inherited Create(PropID, ValueStr, Important);
  FValue := htmlcssDecodeDisplayType(ValueStr);
end;

procedure ThtmlcssDisplayProperty.ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
    const ParentStyle: ThtmlcssStyleProperties);
begin
  StyleInfo.Display := FValue;
end;



{                                                                              }
{ white-space                                                                  }
{                                                                              }
constructor ThtmlcssWhiteSpaceProperty.Create(const PropID: ThtmlcssPropertyID;
    const ValueStr: String; const Important: Boolean);
begin
  inherited Create(PropID, ValueStr, Important);
  FValue := htmlcssDecodeWhiteSpaceType(ValueStr);
end;

procedure ThtmlcssWhiteSpaceProperty.ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
    const ParentStyle: ThtmlcssStyleProperties);
begin
  StyleInfo.WhiteSpace := FValue;
end;



{                                                                              }
{ list-style-type                                                              }
{                                                                              }
constructor ThtmlcssListStyleTypeProperty.Create(const PropID: ThtmlcssPropertyID;
    const ValueStr: String; const Important: Boolean);
begin
  inherited Create(PropID, ValueStr, Important);
  FValue := htmlcssDecodeListStyleType(ValueStr);
end;

procedure ThtmlcssListStyleTypeProperty.ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
    const ParentStyle: ThtmlcssStyleProperties);
begin
  StyleInfo.ListStyleType := FValue;
end;



{                                                                              }
{ list-style-image                                                             }
{                                                                              }
constructor ThtmlcssListStyleImageProperty.Create(const PropID: ThtmlcssPropertyID;
    const ValueStr: String; const Important: Boolean);
begin
  inherited Create(PropID, ValueStr, Important);
  FValue := ValueStr;
end;

procedure ThtmlcssListStyleImageProperty.ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
    const ParentStyle: ThtmlcssStyleProperties);
begin
  StyleInfo.ListStyleImage := FValue;
end;



{                                                                              }
{ list-style-position                                                          }
{                                                                              }
constructor ThtmlcssListStylePositionProperty.Create(const PropID: ThtmlcssPropertyID;
    const ValueStr: String; const Important: Boolean);
begin
  inherited Create(PropID, ValueStr, Important);
  FValue := htmlcssDecodeListStylePositionType(ValueStr);
end;

procedure ThtmlcssListStylePositionProperty.ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
    const ParentStyle: ThtmlcssStyleProperties);
begin
  StyleInfo.ListStylePosition := FValue;
end;



{                                                                              }
{ list-style                                                                   }
{                                                                              }
constructor ThtmlcssListStyleProperty.Create(const PropID: ThtmlcssPropertyID;
    const ValueStr: String; const Important: Boolean);
begin
  inherited Create(PropID, ValueStr, Important);
  FValue := htmlcssDecodeListStyleType(ValueStr);
end;

procedure ThtmlcssListStyleProperty.ApplyStyleInfo(var StyleInfo: ThtmlcssStyleProperties;
    const ParentStyle: ThtmlcssStyleProperties);
begin
  StyleInfo.ListStyle := FValue;
end;



{                                                                              }
{ Property Class Mapping                                                       }
{                                                                              }
const
  HTMLCSSPropertyClasses: Array[ThtmlcssPropertyID] of ThtmlcssPropertyClass = (
      { Font properties }
      nil,
      ThtmlcssFontFamilyProperty,
      ThtmlcssFontStyleProperty,
      ThtmlcssFontVariantProperty,
      ThtmlcssFontWeightProperty,
      ThtmlcssFontSizeProperty,
      ThtmlcssFontProperty,
      { Color and background properties }
      ThtmlcssColorProperty,
      ThtmlcssBackgroundColorProperty,
      ThtmlcssBackgroundImageProperty,
      ThtmlcssBackgroundRepeatProperty,
      ThtmlcssBackgroundAttachmentProperty,
      ThtmlcssBackgroundPositionProperty,
      ThtmlcssBackgroundProperty,
      { Text properties }
      ThtmlcssWordSpacingProperty,
      ThtmlcssLetterSpacingProperty,
      ThtmlcssTextDecorationProperty,
      ThtmlcssVerticalAlignProperty,
      ThtmlcssTextTransformProperty,
      ThtmlcssTextAlignProperty,
      ThtmlcssTextIndentProperty,
      ThtmlcssLineHeightProperty,
      { Box properties }
      ThtmlcssMarginTopProperty,
      ThtmlcssMarginRightProperty,
      ThtmlcssMarginBottomProperty,
      ThtmlcssMarginLeftProperty,
      ThtmlcssMarginsProperty,
      ThtmlcssPaddingTopProperty,
      ThtmlcssPaddingRightProperty,
      ThtmlcssPaddingBottomProperty,
      ThtmlcssPaddingLeftProperty,
      ThtmlcssPaddingProperty,
      ThtmlcssBorderTopWidthProperty,
      ThtmlcssBorderRightWidthProperty,
      ThtmlcssBorderBottomWidthProperty,
      ThtmlcssBorderLeftWidthProperty,
      ThtmlcssBorderWidthsProperty,
      ThtmlcssBorderColProperty,
      ThtmlcssBorderStyleProperty,
      ThtmlcssBorderTopProperty,
      ThtmlcssBorderRightProperty,
      ThtmlcssBorderBottomProperty,
      ThtmlcssBorderLeftProperty,
      ThtmlcssBorderProperty,
      ThtmlcssWidthProperty,
      ThtmlcssHeightProperty,
      ThtmlcssFloatProperty,
      ThtmlcssClearProperty,
      { Classification properties }
      ThtmlcssDisplayProperty,
      ThtmlcssWhiteSpaceProperty,
      ThtmlcssListStyleTypeProperty,
      ThtmlcssListStyleImageProperty,
      ThtmlcssListStylePositionProperty,
      ThtmlcssListStyleProperty);

function htmlcssGetPropertyClass(const PropertyID: ThtmlcssPropertyID): ThtmlcssPropertyClass;
begin
  Result := HTMLCSSPropertyClasses[PropertyID];
  if not Assigned(Result) then
    Result := ThtmlcssProperty;
end;



end.

