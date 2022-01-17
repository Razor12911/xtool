{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals 5.00 - HTML Parser                          }
{   File name:        flcHTMLProperties.pas                                    }
{   File version:     5.10                                                     }
{   Description:      HTML properties                                          }
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
{   2000/04/12  1.00  Part of cInternetStandards unit.                         }
{   2001/04/13  1.01  Part of cHTML unit.                                      }
{   2002/02/10  1.02  Part of cHTMLEncode unit.                                }
{   2002/10/23  1.03  Created cHTMLUtils unit.                                 }
{                     Added tests.                                             }
{   2002/10/29  1.04  Attribute encoding/decoding functions.                   }
{   2002/11/04  1.05  Style sheet encoding/decoding functions.                 }
{   2002/12/03  1.06  Attribute encoding/decoding functions.                   }
{   2015/04/04  1.07  RawByteString changes.                                   }
{   2015/04/11  1.08  UnicodeString changes.                                   }
{   2019/02/21  1.09  Part of flcHTMLProperties.                               }
{   2019/02/22  5.10  Revise for Fundamentals 5.                               }
{                                                                              }
{******************************************************************************}

{$INCLUDE flcHTML.inc}

unit flcHTMLProperties;

interface

uses
  System.Types,
  System.UITypes;



{ Integer                                                                      }
function  htmlDecodeInteger(const S: String; const Default: Integer): Integer;
function  htmlDecodeValidInteger(const S: String; var Value: Integer): Boolean;
function  htmlEncodeInteger(const I: Integer): String;

{ HTML text direction                                                          }
type
  ThtmlTextDir = (dirDefault, dirLTR, dirRTL);

function  htmlDecodeTextDir(const S: String): ThtmlTextDir;
function  htmlEncodeTextDir(const Dir: ThtmlTextDir): String;

{ HTML Clear                                                                   }
type
  ThtmlClearType = (clearDefault, clearLeft, clearAll, clearRight, clearNone);

function  htmlDecodeClearType(const S: String): ThtmlClearType;
function  htmlEncodeBRClearType(const ClearType: ThtmlClearType): String;

{ HTML Input Type                                                              }
type
  ThtmlInputType = (inputDefault, inputText, inputPassword, inputCheckBox,
      inputRadio, inputSubmit, inputReset, inputFile, inputHidden, inputImage,
      inputButton);

function  htmlDecodeInputType(const S: String): ThtmlInputType;
function  htmlEncodeInputType(const InputType: ThtmlInputType): String;

{ HTML Align                                                                   }
type
  ThtmlIAlignType = (ialDefault, ialTop, ialMiddle, ialBottom, ialLeft, ialRight);

function  htmlDecodeIAlignType(const S: String): ThtmlIAlignType;
function  htmlEncodeIAlignType(const Align: ThtmlIAlignType): String;

type
  ThtmlTAlignType = (talDefault, talLeft, talCenter, talRight);

function  htmlDecodeTAlignType(const S: String): ThtmlTAlignType;
function  htmlEncodeTAlignType(const Align: ThtmlTAlignType): String;

{ 'Length' values                                                              }
type
  ThtmlLengthType = (
      lenDefault,
      lenAbsolute,
      lenPercent,
      lenPixels,
      lenInches,
      lenCentimeters,
      lenMillimeters,
      lenUnitsRelative,  // relative length units
      lenRelFontHeight,  // relative to element's font height
      lenRelFontxHeight, // relative to height of 'x' character
      lenFontPoints,     // font size in points
      lenFontPicas       // font size in picas(1 pica = 12 pt)
      );
  ThtmlLength = record
    LengthType : ThtmlLengthType;
    Value      : Double;
  end;

function  htmlDecodeLength(const S: String): ThtmlLength;
function  htmlcssDecodeLength(const S: String): ThtmlLength;
function  htmlResolveLengthPixels(const Len: ThtmlLength;
          const AbsLength: Integer): Integer;

{ font-family                                                                  }
type
  ThtmlFontFamilyType = (
      ffamDefault,
      ffamFamilyName,
      ffamGenericFamily);
  ThtmlGenericFontFamily = (
      gffamSerif,
      gffamSansSerif,
      gffamCursive,
      gffamFantasy,
      gffamMonospace);
  ThtmlFontFamily = record
    FamilyType    : ThtmlFontFamilyType;
    FamilyName    : String;
    GenericFamily : ThtmlGenericFontFamily;
  end;
  ThtmlFontFamilyList = Array of ThtmlFontFamily;

function  htmlDecodeFontFamily(const S: String): ThtmlFontFamily;
function  htmlDecodeFontFamilyList(const S: String): ThtmlFontFamilyList;

{ font-style                                                                   }
type
  ThtmlFontStyle = (
      fstyleDefault,
      fstyleNormal,
      fstyleItalic,
      fstyleOblique);

function  htmlcssDecodeFontStyle(const S: String): ThtmlFontStyle;

{ font-variant                                                                 }
type
  ThtmlFontVariant = (
      fvariantDefault,
      fvariantNormal,
      fvariantSmallCaps);

function  htmlcssDecodeFontVariant(const S: String): ThtmlFontVariant;

{ font-weight                                                                  }
type
  ThtmlFontWeight = (fweightDefault, fweightNormal, fweightBold, fweightBolder,
      fweightLighter, fweight100, fweight200, fweight300, fweight400,
      fweight500, fweight600, fweight700, fweight800, fweight900);

function  htmlcssDecodeFontWeight(const S: String): ThtmlFontWeight;
function  htmlIsFontWeightBold(const Weight: ThtmlFontWeight): Boolean;
procedure htmlResolveRelFontWeight(var RelFontWeight: ThtmlFontWeight;
          const AbsFontWeight: ThtmlFontWeight);

{ font-size                                                                    }
type
  ThtmlFontSizeType = (
      fsizeDefault,
      fsizeHTMLAbsolute,
      fsizeHTMLRelative,
      fsizeFontHeight,
      fsizeFontPoints,
      fsizeFontPicas,
      fsizePercent,
      fsizeRelFontHeight,
      fsizeRelFontxHeight);
  ThtmlFontSize = record
    SizeType : ThtmlFontSizeType;
    Value    : Double;
  end;

function  htmlDecodeFontSize(const S: String): ThtmlFontSize;
function  htmlcssDecodeFontSize(const S: String): ThtmlFontSize;
procedure htmlResolveRelativeFontSize(var RelSize: ThtmlFontSize;
          const AbsSize: ThtmlFontSize);
function  htmlResolveFontSizePoints(const FontSize: ThtmlFontSize): Integer;
function  htmlResolveFontSizePixels(const FontSize: ThtmlFontSize): Integer;

{ font                                                                         }
procedure htmlcssDecodeFont(const S: String;
          var FontStyle: ThtmlFontStyle;
          var FontVariant: ThtmlFontVariant;
          var FontWeight: ThtmlFontWeight;
          var Size: ThtmlFontSize;
          var FamilyList: ThtmlFontFamilyList);

{ 'spacing' types                                                              }
type
  ThtmlSpacingType = (
      spacingDefault,
      spacingNormal,
      spacingLength);
  ThtmlSpacing = record
    SpacingType : ThtmlSpacingType;
    Length      : ThtmlLength;
  end;

{ word-spacing                                                                 }
function  htmlcssDecodeWordSpacing(const S: String): ThtmlSpacing;

{ letter-spacing                                                               }
function  htmlcssDecodeLetterSpacing(const S: String): ThtmlSpacing;

{ text-decoration                                                              }
type
  ThtmlTextDecoration = (
      tdecoDefault,
      tdecoNone,
      tdecoUnderline,
      tdecoOverline,
      tdecoLineThrough,
      tdecoBlink);

function  htmlcssDecodeTextDecoration(const S: String): ThtmlTextDecoration;

{ text-transform                                                               }
type
  ThtmlTextTransform = (
      ttranDefault,
      ttranCapitalize,
      ttranUpperCase,
      ttranLowerCase,
      ttranNone);

function  htmlcssDecodeTextTransform(const S: String): ThtmlTextTransform;

{ text-indent                                                                  }
function  htmlcssDecodeTextIndent(const S: String): ThtmlLength;

{ color                                                                        }
type
  ThtmlRGBColor = record
    case Integer of
      0: (RGB: LongWord);
      1: (R, G, B: Byte);
      2: (Color: TColor);
  end;
  ThtmlColorType = (colorDefault, colorRGB, colorNamed);
  ThtmlColor = record
    ColorType  : ThtmlColorType;
    RGBColor   : ThtmlRGBColor;
    NamedColor : String;
  end;

function  htmlDecodeColor(const S: String): ThtmlColor;
function  htmlResolveColor(const S: String): ThtmlRGBColor;
function  htmlColorValue(const Color: ThtmlColor): ThtmlRGBColor;
function  htmlRGBColor(const RGB: LongWord): ThtmlRGBColor;
function  htmlEncodeRGBColor(const Color: ThtmlRGBColor): String;
function  htmlEncodeColor(const Color: ThtmlColor): String;
function  htmlcssDecodeColor(const S: String): ThtmlColor;
function  htmlcssResolveColor(const S: String): ThtmlRGBColor;

{ CSS 'string' values                                                          }
function  htmlcssDecodeString(const S: String): String;

{ CSS 'uri' values                                                             }
function  htmlcssDecodeURI(const S: String): String;

{ background-image                                                             }
type
  ThtmlBackgroundImageType = (
      backimgDefault,
      backimgURL,
      backimgNone);
  ThtmlBackgroundImage = record
    ImageType : ThtmlBackgroundImageType;
    URL       : String;
  end;

function  htmlcssDecodeBackgroundImage(const S: String): ThtmlBackgroundImage;

{ background-repeat                                                            }
type
  ThtmlBackgroundRepeat = (
      brepDefault,
      brepRepeat,
      brepRepeatX,
      brepRepeatY,
      brepNoRepeat);

function  htmlcssDecodeBackgroundRepeat(const S: String): ThtmlBackgroundRepeat;

{ background-attachment                                                        }
type
  ThtmlBackgroundAttachment = (
      battDefault,
      battScroll,
      battFixed);

function  htmlcssDecodeBackgroundAttachment(const S: String): ThtmlBackgroundAttachment;

{ background                                                                   }
procedure htmlcssDecodeBackground(const S: String;
          var BackColor: ThtmlColor;
          var BackImage: ThtmlBackgroundImage;
          var BackRepeat: ThtmlBackgroundRepeat;
          var BackAttach: ThtmlBackgroundAttachment);

{ vertical-align                                                               }
type
  ThtmlVerticalAlignType = (
      valignDefault,
      valignBaseline,
      valignSub,
      valignSuper,
      valignTop,
      valignTextTop,
      valignMiddle,
      valignBottom,
      valignTextBottom,
      valignPercent);
  ThtmlVerticalAlign = record
    AlignType : ThtmlVerticalAlignType;
    Value     : Integer;
  end;
  ThtmlVerticalAlignArray = Array of ThtmlVerticalAlign;

function  htmlcssDecodeVerticalAlign(const S: String): ThtmlVerticalAlign;

{ text-align                                                                   }
type
  ThtmlTextAlignType = (
      talignDefault,
      talignLeft,
      talignRight,
      talignCenter,
      talignJustify);
  ThtmlTextAlignArray = Array of ThtmlTextAlignType;

function  htmlDecodeTextAlignType(const S: String): ThtmlTextAlignType;
function  htmlEncodeTextAlignType(const Align: ThtmlTextAlignType): String;

{ line-height                                                                  }
type
  ThtmlLineHeight = ThtmlLength;

function  htmlcssDecodeLineHeight(const S: String): ThtmlLineHeight;

{ margin                                                                       }
type
  ThtmlMargin = ThtmlLength;
  ThtmlMargins = record
    Top    : ThtmlMargin;
    Bottom : ThtmlMargin;
    Left   : ThtmlMargin;
    Right  : ThtmlMargin;
  end;

function  htmlcssDecodeMargin(const S: String): ThtmlMargin;
function  htmlcssDecodeMargins(const S: String): ThtmlMargins;

{ padding                                                                      }
type
  ThtmlPadding = ThtmlLength;
  ThtmlPaddings = record
    Top    : ThtmlPadding;
    Bottom : ThtmlPadding;
    Left   : ThtmlPadding;
    Right  : ThtmlPadding;
  end;

function  htmlcssDecodePadding(const S: String): ThtmlPadding;
function  htmlcssDecodePaddings(const S: String): ThtmlPaddings;
function  htmlResolvePaddings(const Paddings: ThtmlPaddings): TRect;
procedure htmlApplyResolvedPaddings(var Dest: TRect; const Source: TRect);

{ border-width                                                                 }
type
  ThtmlBorderWidthType = (
      borwidthDefault,
      borwidthThin,
      borwidthMedium,
      borwidthThick,
      borwidthLength);
  ThtmlBorderWidth = record
    WidthType : ThtmlBorderWidthType;
    Len       : ThtmlLength;
  end;
  ThtmlBorderWidths = record
    Top    : ThtmlBorderWidth;
    Bottom : ThtmlBorderWidth;
    Left   : ThtmlBorderWidth;
    Right  : ThtmlBorderWidth;
  end;

function  htmlcssDecodeBorderWidth(const S: String): ThtmlBorderWidth;
function  htmlcssDecodeBorderWidths(const S: String): ThtmlBorderWidths;

{ border-style                                                                 }
type
  ThtmlBorderStyle = (
      borstyleDefault,
      borstyleNone,
      borstyleDotted,
      borstyleDashed,
      borstyleSolid,
      borstyleDouble,
      borstyleGroove,
      borstyleRidge,
      borstyleInset,
      borstyleOutset);

function  htmlcssDecodeBorderStyle(const S: String): ThtmlBorderStyle;

{ float                                                                        }
type
  ThtmlFloatType = (
      floatLeft,
      floatRight,
      floatNone);

function  htmlcssDecodeFloatType(const S: String): ThtmlFloatType;

{ white-space                                                                  }
type
  ThtmlWhiteSpaceType = (
      wsDefault,
      wsNormal,
      wsPre,
      wsNoWrap);

function  htmlcssDecodeWhiteSpaceType(const S: String): ThtmlWhiteSpaceType;

{ display                                                                      }
type
  ThtmlDisplayType = (
      displayDefault,
      displayBlock,
      displayInline,
      displayListItem,
      displayNone);

function  htmlcssDecodeDisplayType(const S: String): ThtmlDisplayType;

{ list-style-position                                                          }
type
  ThtmlListStylePositionType = (
      liststyleposDefault,
      liststyleposInside,
      liststyleposOutside);

function  htmlcssDecodeListStylePositionType(const S: String): ThtmlListStylePositionType;

{ list-style                                                                   }
type
  ThtmlListStyleType = (
      liststyleDefault,
      liststyleDisc,
      liststyleCircle,
      liststyleSquare,
      liststyleDecimal,
      liststyleLowerRoman,
      liststyleUpperRoman,
      liststyleLowerAlpha,
      liststyleUpperAlpha,
      liststyleNone,
      liststyleInside,
      liststyleOutside);

function  htmlcssDecodeListStyleType(const S: String): ThtmlListStyleType;



{ Tests                                                                        }
{$IFDEF HTML_TEST}
procedure Test;
{$ENDIF}



implementation

uses
  flcStdTypes,
  flcUtils,
  flcAscii,
  flcStrings,
  flcFloats;



function htmlDecodeInteger(const S: String; const Default: Integer): Integer;
begin
  Result := StringToIntDefU(S, Default);
end;

function htmlDecodeValidInteger(const S: String; var Value: Integer): Boolean;
begin
  Result := StrIsIntegerU(S);
  if Result then
    Value := StringToIntDefU(S, -1)
  else
    Value := 0;
end;

function htmlEncodeInteger(const I: Integer): String;
begin
  Result := IntToStringU(I);
end;

{ 'Length' values                                                              }
function htmlDecodeLength(const S: String): ThtmlLength;
var L: Integer;
begin
  L := Length(S);
  if L = 0 then
    begin
      Result.LengthType := lenDefault;
      Result.Value := -1.0;
    end else
    begin
      case S[L] of
        '%' : // percent
          begin
            Result.LengthType := lenPercent;
            Result.Value := htmlDecodeInteger(Copy(S, 1, L - 1), -1);
          end;
        '*' : // relative units
          begin
            Result.LengthType := lenUnitsRelative;
            if L = 1 then
              Result.Value := 1 else
              Result.Value := htmlDecodeInteger(Copy(S, 1, L - 1), -1);
          end;
      else // absolute value
        begin
          Result.LengthType := lenAbsolute;
          Result.Value := htmlDecodeInteger(S, -1);
        end;
      end;
      if Result.Value < 0 then // invalid value
        Result.LengthType := lenDefault;
    end;
end;

function htmlcssDecodeLength(const S: String): ThtmlLength;
var L: Integer;
    C1, C2: WideChar;
    T: String;
begin
  Result.LengthType := lenDefault;
  Result.Value := -1.0;
  L := Length(S);
  if L = 0 then
    exit;
  // check 2-character suffix for unit
  if L > 2 then
    begin
      C1 := AsciiLowCaseW(S[L - 1]);
      C2 := AsciiLowCaseW(S[L]);
      case C1 of
        'c' :
          case C2 of
            'm' : Result.LengthType := lenCentimeters;
          end;
        'e' :
          case C2 of
            'm' : Result.LengthType := lenRelFontHeight;
            'x' : Result.LengthType := lenRelFontxHeight;
          end;
        'i' :
          case C2 of
            'n' : Result.LengthType := lenInches;
          end;
        'm' :
          case C2 of
            'm' : Result.LengthType := lenMillimeters;
          end;
        'p' :
          case C2 of
            'c' : Result.LengthType := lenFontPicas;
            't' : Result.LengthType := lenFontPoints;
            'x' : Result.LengthType := lenPixels;
          end;
      end;
      if Result.LengthType <> lenDefault then
        begin
          T := CopyLeftU(S, L - 2);
          Result.Value := StringToFloatDefU(T, -1.0);
          if Result.Value < 0 then
            Result.LengthType := lenDefault;
          exit;
        end;
    end;
  // check HTML length
  Result := htmlDecodeLength(S);
end;

function htmlResolveLengthPixels(const Len: ThtmlLength;
    const AbsLength: Integer): Integer;
begin
  case Len.LengthType of
    lenDefault   : Result := -1;
    lenAbsolute,
    lenPixels    : Result := Round(Len.Value);
    lenPercent   : Result := Round(Len.Value / 100.0 * AbsLength);
  else
    Result := -1;
  end;
end;

function htmlDecodeTextDir(const S: String): ThtmlTextDir;
begin
  if StrEqualNoAsciiCase(S, 'ltr') then
    Result := dirLTR else
  if StrEqualNoAsciiCase(S, 'rtl') then
    Result := dirRTL else
    Result := dirDefault;
end;

function htmlEncodeTextDir(const Dir: ThtmlTextDir): String;
begin
  case Dir of
    dirLTR: Result := 'ltr';
    dirRTL: Result := 'rtl';
  else
    Result := '';
  end;
end;

function htmlDecodeClearType(const S: String): ThtmlClearType;
begin
  if StrEqualNoAsciiCase(S, 'left') then
    Result := clearLeft else
  if StrEqualNoAsciiCase(S, 'all') then
    Result := clearAll else
  if StrEqualNoAsciiCase(S, 'right') then
    Result := clearRight else
  if StrEqualNoAsciiCase(S, 'none') then
    Result := clearNone else
    Result := clearDefault;
end;

function htmlEncodeBRClearType(const ClearType: ThtmlClearType): String;
begin
  case ClearType of
    clearLeft  : Result := 'left';
    clearAll   : Result := 'all';
    clearRight : Result := 'right';
    clearNone  : Result := 'none';
  else
    Result := '';
  end;
end;

function htmlDecodeInputType(const S: String): ThtmlInputType;
begin
  if StrEqualNoAsciiCase(S, 'TEXT') then
    Result := inputText else
  if StrEqualNoAsciiCase(S, 'PASSWORD') then
    Result := inputPassword else
  if StrEqualNoAsciiCase(S, 'CHECKBOX') then
    Result := inputCheckBox else
  if StrEqualNoAsciiCase(S, 'RADIO') then
    Result := inputRadio else
  if StrEqualNoAsciiCase(S, 'SUBMIT') then
    Result := inputSubmit else
  if StrEqualNoAsciiCase(S, 'RESET') then
    Result := inputReset else
  if StrEqualNoAsciiCase(S, 'FILE') then
    Result := inputFile else
  if StrEqualNoAsciiCase(S, 'HIDDEN') then
    Result := inputHidden else
  if StrEqualNoAsciiCase(S, 'IMAGE') then
    Result := inputImage else
  if StrEqualNoAsciiCase(S, 'BUTTON') then
    Result := inputButton else
    Result := inputDefault;
end;

function htmlEncodeInputType(const InputType: ThtmlInputType): String;
begin
  case InputType of
    inputText     : Result := 'TEXT';
    inputPassword : Result := 'PASSWORD';
    inputCheckBox : Result := 'CHECKBOX';
    inputRadio    : Result := 'RADIO';
    inputSubmit   : Result := 'SUBMIT';
    inputReset    : Result := 'RESET';
    inputFile     : Result := 'FILE';
    inputHidden   : Result := 'HIDDEN';
    inputImage    : Result := 'IMAGE';
    inputButton   : Result := 'BUTTON';
  else
    Result := '';
  end;
end;

{ HTML Align                                                                   }
function htmlDecodeIAlignType(const S: String): ThtmlIAlignType;
begin
  if StrEqualNoAsciiCase(S, 'top') then
    Result := ialTop else
  if StrEqualNoAsciiCase(S, 'middle') then
    Result := ialMiddle else
  if StrEqualNoAsciiCase(S, 'bottom') then
    Result := ialBottom else
  if StrEqualNoAsciiCase(S, 'left') then
    Result := ialLeft else
  if StrEqualNoAsciiCase(S, 'right') then
    Result := ialRight else
    Result := ialDefault;
end;

function htmlEncodeIAlignType(const Align: ThtmlIAlignType): String;
begin
  case Align of
    ialTop    : Result := 'top';
    ialMiddle : Result := 'middle';
    ialBottom : Result := 'bottom';
    ialLeft   : Result := 'left';
    ialRight  : Result := 'right';
  else
    Result := '';
  end;
end;

function htmlDecodeTAlignType(const S: String): ThtmlTAlignType;
begin
  if StrEqualNoAsciiCase(S, 'left') then
    Result := talLeft else
  if StrEqualNoAsciiCase(S, 'center') then
    Result := talCenter else
  if StrEqualNoAsciiCase(S, 'right') then
    Result := talRight
  else
    Result := talDefault;
end;

function htmlEncodeTAlignType(const Align: ThtmlTAlignType): String;
begin
  case Align of
    talLeft   : Result := 'left';
    talCenter : Result := 'center';
    talRight  : Result := 'right';
  else
    Result := '';
  end;
end;

{ font-family                                                                  }
{ ::= [[<family-name> | <generic-family>],]*                                   }
{     [<family-name> | <generic-family>]                                       }
function htmlDecodeFontFamily(const S: String): ThtmlFontFamily;
begin
  Result.FamilyType := ffamDefault;
  if S = '' then
    exit;
  Result.FamilyType := ffamGenericFamily;
  if StrEqualNoAsciiCase(S, 'serif') then
    Result.GenericFamily := gffamSerif else
  if StrEqualNoAsciiCase(S, 'sans-serif') then
    Result.GenericFamily := gffamSansSerif else
  if StrEqualNoAsciiCase(S, 'cursive') then
    Result.GenericFamily := gffamCursive else
  if StrEqualNoAsciiCase(S, 'fantasy') then
    Result.GenericFamily := gffamFantasy else
  if StrEqualNoAsciiCase(S, 'monospace') then
    Result.GenericFamily := gffamMonospace
  else
    begin
      Result.FamilyType := ffamFamilyName;
      Result.FamilyName := S;
    end;
end;

function htmlDecodeFontFamilyList(const S: String): ThtmlFontFamilyList;
var T: StringArray;
    I, L, M: Integer;
begin
  T := nil;
  Result := nil;
  if S = '' then
    exit;
  T := StrSplitChar(S, ',');
  TrimStrings(T, [#0..#32]);
  M := 0;
  L := Length(T);
  SetLength(Result, L);
  for I := 0 to L - 1 do
    if T[I] <> '' then
      begin
        Result[M] := htmlDecodeFontFamily(T[I]);
        if Result[M].FamilyType <> ffamDefault then
          Inc(M);
      end;
  if M < L then
    SetLength(Result, M);
end;

{ font-style                                                                   }
{ ::= normal | italic | oblique                                                }
function htmlcssDecodeFontStyle(const S: String): ThtmlFontStyle;
begin
  if S = '' then
    Result := fstyleDefault else
  if StrEqualNoAsciiCase(S, 'normal') then
    Result := fstyleNormal else
  if StrEqualNoAsciiCase(S, 'italic') then
    Result := fstyleItalic else
  if StrEqualNoAsciiCase(S, 'oblique') then
    Result := fstyleOblique
  else
    Result := fstyleDefault;
end;

{ font-variant                                                                 }
{ ::= normal | small-caps                                                      }
function htmlcssDecodeFontVariant(const S: String): ThtmlFontVariant;
begin
  if S = '' then
    Result := fvariantDefault else
  if StrEqualNoAsciiCase(S, 'normal') then
    Result := fvariantNormal else
  if StrEqualNoAsciiCase(S, 'small-caps') then
    Result := fvariantSmallCaps
  else
    Result := fvariantDefault;
end;

{ font-weight                                                                  }
{ ::= normal | bold | bolder | lighter | 100 | 200 | 300 | 400 | 500 | 600 |   }
{     700 | 800 | 900                                                          }
function htmlcssDecodeFontWeight(const S: String): ThtmlFontWeight;
begin
  if S = '' then
    Result := fweightDefault else
  if StrEqualNoAsciiCase(S, 'normal') then
    Result := fweightNormal else
  if StrEqualNoAsciiCase(S, 'bold') then
    Result := fweightBold else
  if StrEqualNoAsciiCase(S, 'bolder') then
    Result := fweightBolder else
  if StrEqualNoAsciiCase(S, 'lighter') then
    Result := fweightLighter else
  if (Length(S) = 3) and WideCharInCharSet(S[2], ['0'..'9']) and WideCharInCharSet(S[3], ['0'..'9']) then
    case S[1] of
      '1' : Result := fweight100;
      '2' : Result := fweight200;
      '3' : Result := fweight300;
      '4' : Result := fweight400;
      '5' : Result := fweight500;
      '6' : Result := fweight600;
      '7' : Result := fweight700;
      '8' : Result := fweight800;
      '9' : Result := fweight900;
    else
      Result := fweightDefault;
    end
  else
    Result := fweightDefault;
end;

function htmlIsFontWeightBold(const Weight: ThtmlFontWeight): Boolean;
begin
  Result := Weight in [fweightBold, fweight700, fweight800, fweight900];
end;

procedure htmlResolveRelFontWeight(var RelFontWeight: ThtmlFontWeight;
          const AbsFontWeight: ThtmlFontWeight);
begin
  case RelFontWeight of
    fweightNormal  : RelFontWeight := fweight400;
    fweightBold    : RelFontWeight := fweight700;
    fweightBolder  :
      case AbsFontWeight of
        fweight100..fweight800 : RelFontWeight := ThtmlFontWeight(Ord(AbsFontWeight) + 1);
        fweightNormal          : RelFontWeight := fweight500;
        fweightBold            : RelFontWeight := fweight800;
      end;
    fweightLighter :
      case AbsFontWeight of
        fweight200..fweight900 : RelFontWeight := ThtmlFontWeight(Ord(AbsFontWeight) - 1);
        fweightNormal          : RelFontWeight := fweight300;
        fweightBold            : RelFontWeight := fweight600;
      end;
  end;
end;

{ font-size                                                                    }
{ ::= <absolute-size> | <relative-size> | <length> | <percentage>              }
function htmlDecodeFontSize(const S: String): ThtmlFontSize;
var V: Integer;
    Len: ThtmlLength;
begin
  Result.SizeType := fsizeDefault;
  Result.Value := 0.0;
  if S = '' then
    exit;
  if htmlDecodeValidInteger(S, V) then
    begin
      if WideCharInCharSet(S[1], ['+', '-']) then
        Result.SizeType := fsizeHTMLRelative
      else
        Result.SizeType := fsizeHTMLAbsolute;
      Result.Value := V;
    end else
    begin
      Len := htmlDecodeLength(S);
      case Len.LengthType of
        lenPercent :
          begin
            Result.SizeType := fsizePercent;
            Result.Value := Len.Value;
          end;
      end;
    end;
end;

const
  htmlcssAbsFontSizeKeywords: Array[1..7] of String = (
      'xx-small', 'x-small', 'small', 'medium', 'large', 'x-large', 'xx-large');

function htmlcssDecodeFontSize(const S: String): ThtmlFontSize;
var I: Integer;
    Len: ThtmlLength;
begin
  Result.SizeType := fsizeDefault;
  Result.Value := 0.0;
  if S = '' then
    exit;
  // decode length
  Len := htmlcssDecodeLength(S);
  if Len.LengthType <> lenDefault then
    begin
      Result.Value := Len.Value;
      case Len.LengthType of
        lenFontPoints,
        lenAbsolute       : Result.SizeType := fsizeFontPoints;
        lenFontPicas      : Result.SizeType := fsizeFontPicas;
        lenPercent        : Result.SizeType := fsizePercent;
        lenRelFontHeight  : Result.SizeType := fsizeRelFontHeight;
        lenRelFontxHeight : Result.SizeType := fsizeRelFontxHeight;
        lenPixels         : Result.SizeType := fsizeFontHeight;
      end;
      if Result.SizeType <> fsizeDefault then
        exit;
    end;
  // check absolute size keywords
  for I := 1 to 7 do
    if StrEqualNoAsciiCase(S, htmlcssAbsFontSizeKeywords[I]) then
      begin
        Result.SizeType := fsizeHTMLAbsolute;
        Result.Value := I;
        exit;
      end;
  // check relative size keywords
  if StrEqualNoAsciiCase(S, 'larger') then
    begin
      Result.SizeType := fsizeHTMLRelative;
      Result.Value := 1;
      exit;
    end else
  if StrEqualNoAsciiCase(S, 'smaller') then
    begin
      Result.SizeType := fsizeHTMLRelative;
      Result.Value := -1;
      exit;
    end;
  // not decoded
  Result.Value := 0.0;
end;

procedure htmlResolveRelativeFontSize(var RelSize: ThtmlFontSize;
  const AbsSize: ThtmlFontSize);
begin
  case RelSize.SizeType of
    fsizeHTMLRelative :
      case AbsSize.SizeType of
        fsizeHTMLAbsolute:
          begin
            RelSize.SizeType := fsizeHTMLAbsolute;
            RelSize.Value := RelSize.Value + AbsSize.Value;
          end;
      end;
    fsizePercent :
      case AbsSize.SizeType of
        fsizeHTMLAbsolute,
        fsizeFontHeight,
        fsizeFontPoints,
        fsizeFontPicas     :
          begin
            RelSize.SizeType := AbsSize.SizeType;
            RelSize.Value := RelSize.Value * AbsSize.Value / 100.0;
          end;
      end;
    fsizeRelFontHeight :
      case AbsSize.SizeType of
        fsizeFontHeight :
          begin
            RelSize.SizeType := fsizeFontHeight;
            RelSize.Value := RelSize.Value * AbsSize.Value;
          end;
      end;
  end;
end;

const
  htmlFontSizePtsSmallest : Array[0..7] of Integer = (4, 6, 8, 9, 10, 12, 16, 24);
  htmlFontSizePtsSmall    : Array[0..7] of Integer = (5, 7, 9, 10, 12, 16, 20, 30);
  htmlFontSizePtsNormal   : Array[0..7] of Integer = (6, 8, 10, 12, 14, 18, 24, 36);
  htmlFontSizePtsLarge    : Array[0..7] of Integer = (8, 10, 12, 14, 16, 20, 28, 42);
  htmlFontSizePtsLargest  : Array[0..7] of Integer = (10, 12, 14, 16, 18, 24, 32, 48);

function htmlResolveFontSizePoints(const FontSize: ThtmlFontSize): Integer;
begin
  case FontSize.SizeType of
    fsizeHTMLAbsolute :
      Result := htmlFontSizePtsNormal[MaxInt(0, MinInt(7, Integer(Round(FontSize.Value))))];
    fsizeFontPoints :
      Result := Round(FontSize.Value);
    fsizeFontPicas :
      Result := Round(FontSize.Value * 12.0);
  else
    Result := 0;
  end;
end;

function htmlResolveFontSizePixels(const FontSize: ThtmlFontSize): Integer;
begin
  case FontSize.SizeType of
    fsizeFontHeight :
      Result := Round(FontSize.Value);
  else
    Result := 0;
  end;
end;

{ font                                                                         }
{ ::= [ <font-style> || <font-variant> || <font-weight> ]?                     }
{     <font-size> [ / <line-height> ]? <font-family>                           }
{ TODO: parse <line-height>                                                    }
procedure htmlcssDecodeFont(const S: String;
    var FontStyle: ThtmlFontStyle;
    var FontVariant: ThtmlFontVariant;
    var FontWeight: ThtmlFontWeight;
    var Size: ThtmlFontSize;
    var FamilyList: ThtmlFontFamilyList);
var T: StringArray;
    I, L: Integer;
    F: ThtmlFontFamily;
    Y: ThtmlFontStyle;
    V, V1, V2: String;
    A: ThtmlFontVariant;
    W: ThtmlFontWeight;
    E: ThtmlFontSize;
begin
  // initialize
  T := nil;
  FamilyList := nil;
  FontStyle := fstyleNormal;
  FontVariant := fvariantNormal;
  FontWeight := fweightNormal;
  Size.SizeType := fsizeDefault;
  // decode
  if S = '' then
    exit;
  T := StrSplitChar(S, ' ');
  for I := 0 to Length(T) - 1 do
    begin
      V := T[I];
      StrTrimInPlaceU(V, [#0..#32]);
      if V <> '' then
        begin
          // check font-family
          F := htmlDecodeFontFamily(V);
          if F.FamilyType = ffamGenericFamily then
            begin
              L := Length(FamilyList);
              SetLength(FamilyList, L + 1);
              FamilyList[L] := F;
            end else
            begin
              // check font-style
              Y := htmlcssDecodeFontStyle(V);
              if Y <> fstyleDefault then
                FontStyle := Y else
                begin
                  // check font-variant
                  A := htmlcssDecodeFontVariant(V);
                  if A <> fvariantDefault then
                    FontVariant := A else
                    begin
                      // check font-weight
                      W := htmlcssDecodeFontWeight(V);
                      if W <> fweightDefault then
                        FOntWeight := W else
                        begin
                          // check font-size
                          StrSplitAtU(V, '/', V1, V2, False, True);
                          E := htmlcssDecodeFontSize(V1);
                          if E.SizeType <> fsizeDefault then
                            Size := E;
                        end;
                    end;
                end;
            end;
        end;
    end;
end;

{ 'spacing' types                                                                 }
function htmlcssDecodeSpacing(const S: String): ThtmlSpacing;
begin
  if S = '' then
    Result.SpacingType := spacingDefault else
  if StrEqualNoAsciiCase(S, 'normal') then
    Result.SpacingType := spacingNormal
  else
    begin
      Result.Length := htmlcssDecodeLength(S);
      if Result.Length.LengthType = lenDefault then
        Result.SpacingType := spacingDefault
      else
        Result.SpacingType := spacingLength;
    end;
end;

{ word-spacing                                                                 }
function htmlcssDecodeWordSpacing(const S: String): ThtmlSpacing;
begin
  Result := htmlcssDecodeSpacing(S);
end;

{ letter-spacing                                                               }
function htmlcssDecodeLetterSpacing(const S: String): ThtmlSpacing;
begin
  Result := htmlcssDecodeSpacing(S);
end;

{ text-decoration                                                              }
{ ::= none | [ underline || overline || line-through || blink ]                }
function htmlcssDecodeTextDecoration(const S: String): ThtmlTextDecoration;
begin
  if S = '' then
    Result := tdecoDefault else
  if StrEqualNoAsciiCase(S, 'none') then
    Result := tdecoNone else
  if StrEqualNoAsciiCase(S, 'underline') then
    Result := tdecoUnderline else
  if StrEqualNoAsciiCase(S, 'overline') then
    Result := tdecoOverline else
  if StrEqualNoAsciiCase(S, 'line-through') then
    Result := tdecoLineThrough else
  if StrEqualNoAsciiCase(S, 'blink') then
    Result := tdecoBlink
  else
    Result := tdecoDefault;
end;

{ text-transform                                                               }
{ ::= capitalize | uppercase | lowercase | none                                }
function htmlcssDecodeTextTransform(const S: String): ThtmlTextTransform;
begin
  if S = '' then
    Result := ttranDefault else
  if StrEqualNoAsciiCase(S, 'capitalize') then
    Result := ttranCapitalize else
  if StrEqualNoAsciiCase(S, 'uppercase') then
    Result := ttranUpperCase else
  if StrEqualNoAsciiCase(S, 'lowercase') then
    Result := ttranLowerCase else
  if StrEqualNoAsciiCase(S, 'none') then
    Result := ttranNone
  else
    Result := ttranDefault;
end;

{ text-indent                                                                  }
function htmlcssDecodeTextIndent(const S: String): ThtmlLength;
begin
  Result := htmlcssDecodeLength(S);
end;

{ Known colors                                                                 }
type
  TKnownHtmlColor = record
    Name  : String;
    Color : LongWord;
  end;

const
  KnownHtmlColors = 140;
  KnownHtmlColor: array[0..KnownHtmlColors - 1] of TKnownHtmlColor = (
      (Name:'AliceBlue';              Color:$FFF8F0),
      (Name:'AntiqueWhite';           Color:$D7EBFA),
      (Name:'Aqua';                   Color:$FFFF00),
      (Name:'Aquamarine';             Color:$D4FF7F),
      (Name:'Azure';                  Color:$FFFFF0),
      (Name:'Beige';                  Color:$DCF5F5),
      (Name:'Bisque';                 Color:$C4E4FF),
      (Name:'Black';                  Color:$000000),
      (Name:'BlanchedAlmond';         Color:$CDEBFF),
      (Name:'Blue';                   Color:$FF0000),
      (Name:'BlueViolet';             Color:$E22B8A),
      (Name:'Brown';                  Color:$2A2AA5),
      (Name:'BurlyWood';              Color:$87B8DE),
      (Name:'CadetBlue';              Color:$A09E5F),
      (Name:'Chartreuse';             Color:$00FF7F),
      (Name:'Chocolate';              Color:$1E6902),
      (Name:'Coral';                  Color:$507FFF),
      (Name:'CornflowerBlue';         Color:$ED9564),
      (Name:'CornSilk';               Color:$DCF8FF),
      (Name:'Crimson';                Color:$3C14DC),
      (Name:'Cyan';                   Color:$FFFF00),
      (Name:'DarkBlue';               Color:$8B0000),
      (Name:'DarkCyan';               Color:$8B8B00),
      (Name:'DarkGoldenrod';          Color:$0B86B8),
      (Name:'DarkGray';               Color:$A9A9A9),
      (Name:'DarkGreen';              Color:$006400),
      (Name:'DarkKhaki';              Color:$6BB7BD),
      (Name:'DarkMagenta';            Color:$8B008B),
      (Name:'DarkOliveGreen';         Color:$2F6B55),
      (Name:'DarkOrange';             Color:$008CFF),
      (Name:'DarkOrchid';             Color:$CC3299),
      (Name:'DarkRed';                Color:$000088),
      (Name:'DarkSalmon';             Color:$7A96E9),
      (Name:'DarkSeaGreen';           Color:$8FBC8F),
      (Name:'DarkSlateBlue';          Color:$8B3D48),
      (Name:'DarkSlateGray';          Color:$4F4F2F),
      (Name:'DarkTurquoise';          Color:$D1CE00),
      (Name:'DarkViolet';             Color:$030094),
      (Name:'DeepPink';               Color:$9314FF),
      (Name:'DeepSkyBlue';            Color:$FFBF00),
      (Name:'DimGray';                Color:$696969),
      (Name:'DodgerBlue';             Color:$FF901E),
      (Name:'FireBrick';              Color:$2222B2),
      (Name:'FloralWhite';            Color:$F0FAFF),
      (Name:'ForestGreen';            Color:$228B22),
      (Name:'Fuchsia';                Color:$FF00FF),
      (Name:'GhostWhite';             Color:$FFF8F8),
      (Name:'Gainsboro';              Color:$DCDCDC),
      (Name:'Gold';                   Color:$00D7FF),
      (Name:'Goldenrod';              Color:$20A5DA),
      (Name:'Gray';                   Color:$808080),
      (Name:'Green';                  Color:$008000),
      (Name:'GreenYellow';            Color:$2FFFAD),
      (Name:'HoneyDew';               Color:$F0FFF0),
      (Name:'HotPink';                Color:$B469FF),
      (Name:'IndianRed';              Color:$5C5CCD),
      (Name:'Indigo';                 Color:$82004B),
      (Name:'Ivory';                  Color:$F0FFFF),
      (Name:'Khaki';                  Color:$8CE6F0),
      (Name:'Lavender';               Color:$FAE6E6),
      (Name:'LavenderBlush';          Color:$F5F0FF),
      (Name:'LawnGreen';              Color:$00FC7C),
      (Name:'LemonChiffon';           Color:$CDFAFF),
      (Name:'LightBlue';              Color:$E6D8AD),
      (Name:'LightCoral';             Color:$8080F0),
      (Name:'LightCyan';              Color:$FFFFE0),
      (Name:'LightGoldenrodYellow';   Color:$D2FAFA),
      (Name:'LightGreen';             Color:$90EE90),
      (Name:'LightGrey';              Color:$D3D3D3),
      (Name:'LightPink';              Color:$C1B6FF),
      (Name:'LightSalmon';            Color:$7AA0FF),
      (Name:'LightSeaGreen';          Color:$AAB220),
      (Name:'LightSkyBlue';           Color:$FACE87),
      (Name:'LightSlateGray';         Color:$998877),
      (Name:'LightSteelBlue';         Color:$DEC4B0),
      (Name:'LightYellow';            Color:$E0FFFF),
      (Name:'Lime';                   Color:$00FF00),
      (Name:'LimeGreen';              Color:$32CD32),
      (Name:'Linen';                  Color:$E6F0FA),
      (Name:'Magenta';                Color:$FF00FF),
      (Name:'Maroon';                 Color:$000080),
      (Name:'MediumAquamarine';       Color:$AACD66),
      (Name:'MediumBlue';             Color:$CD0000),
      (Name:'MediumOrchid';           Color:$D355BA),
      (Name:'MediumPurple';           Color:$DB7093),
      (Name:'MediumSeaGreen';         Color:$71B33C),
      (Name:'MediumSlateBlue';        Color:$EE687B),
      (Name:'MediumSpringGreen';      Color:$9AFA00),
      (Name:'MediumTurquoise';        Color:$CCD148),
      (Name:'MediumVioletRed';        Color:$851507),
      (Name:'MidnightBlue';           Color:$701919),
      (Name:'MintCream';              Color:$FAFFF5),
      (Name:'MistyRose';              Color:$E1E4FF),
      (Name:'Moccasin';               Color:$B5E4FF),
      (Name:'NavajoWhite';            Color:$ADDEFF),
      (Name:'Navy';                   Color:$800000),
      (Name:'OldLace';                Color:$E6F5FD),
      (Name:'Olive';                  Color:$008080),
      (Name:'OliveDrab';              Color:$238E6B),
      (Name:'Orange';                 Color:$00A5FF),
      (Name:'Orangered';              Color:$0045FF),
      (Name:'Orchid';                 Color:$D670DA),
      (Name:'PaleGoldenrod';          Color:$AAE8EE),
      (Name:'PaleGreen';              Color:$98FB98),
      (Name:'PaleTurquoise';          Color:$EEEEAF),
      (Name:'PaleVioletRed';          Color:$9370DB),
      (Name:'PapayaWhip';             Color:$D5EFFF),
      (Name:'PeachPuff';              Color:$B9DAFF),
      (Name:'Peru';                   Color:$3F85CD),
      (Name:'Pink';                   Color:$CBC0FF),
      (Name:'Plum';                   Color:$DDA0DD),
      (Name:'PowderBlue';             Color:$E6E0B0),
      (Name:'Purple';                 Color:$800080),
      (Name:'Red';                    Color:$0000FF),
      (Name:'RosyBrown';              Color:$8F8FBC),
      (Name:'RoyalBlue';              Color:$E16941),
      (Name:'SaddleBrown';            Color:$13458B),
      (Name:'Salmon';                 Color:$7280FA),
      (Name:'SandyBrown';             Color:$60A4F4),
      (Name:'SeaGreen';               Color:$578B2E),
      (Name:'SeaShell';               Color:$EEF5FF),
      (Name:'Sienna';                 Color:$2D52A0),
      (Name:'Silver';                 Color:$C0C0C0),
      (Name:'SkyBlue';                Color:$EBCE87),
      (Name:'SlateBlue';              Color:$CD5A6A),
      (Name:'SlateGray';              Color:$908070),
      (Name:'Snow';                   Color:$FAFAFF),
      (Name:'SpringGreen';            Color:$7FFF00),
      (Name:'SteelBlue';              Color:$B48246),
      (Name:'Tan';                    Color:$8CB4D2),
      (Name:'Teal';                   Color:$808000),
      (Name:'Thistle';                Color:$D8BFD8),
      (Name:'Tomato';                 Color:$4763FF),
      (Name:'Turquoise';              Color:$D0E040),
      (Name:'Violet';                 Color:$EE82EE),
      (Name:'Wheat';                  Color:$B3DEF5),
      (Name:'White';                  Color:$FFFFFF),
      (Name:'WhiteSmoke';             Color:$F5F5F5),
      (Name:'Yellow';                 Color:$00FFFF),
      (Name:'YellowGreen';            Color:$32CD9A)
      );

var
  HTMLColorHashIndex : array['A'..'Z'] of Integer;
  HTMLColorHashCount : array['A'..'Z'] of Integer;
  HTMLColorHashInit  : Boolean = False;

procedure InitHTMLColorHash;
var I : Integer;
    C : Char;
    H : AnsiChar;
begin
  for H := 'A' to 'Z' do
    HTMLColorHashIndex[H] := -1;
  FillChar(HTMLColorHashCount, Sizeof(HTMLColorHashCount), #0);
  for I := 0 to KnownHTMLColors - 1 do
    begin
      C := AsciiUpCaseW(KnownHtmlColor[I].Name[1]);
      Assert(WideCharInCharSet(C, ['A'..'Z']), 'Invalid name');
      H := AnsiChar(C);
      if HTMLColorHashIndex[H] < 0 then
        HTMLColorHashIndex[H] := I;
      Inc(HTMLColorHashCount[H]);
    end;
  HTMLColorHashInit := True;
end;

function htmlGetKnownColorPtrW(const Name: PWideChar; const NameLen: Integer;
    var Color: LongWord): Boolean;
var I, J: Integer;
    C: WideChar;
begin
  if NameLen > 0 then
    begin
      C := AsciiUpCaseW(Name^);
      if WideCharInCharSet(C, ['A'..'Z']) then
        begin
          if not HTMLColorHashInit then
            InitHTMLColorHash;
          J := HTMLColorHashIndex[AnsiChar(C)];
          if J >= 0 then
            for I := J to J + HTMLColorHashCount[AnsiChar(C)] - 1 do
              if (Length(KnownHtmlColor[I].Name) = NameLen) and
                 StrPMatchNoAsciiCase(Pointer(Name), Pointer(KnownHtmlColor[I].Name), NameLen) then
                begin
                  // Found
                  Color := KnownHtmlColor[I].Color;
                  Result := True;
                  exit;
                end;
        end;
    end;
  Color := $FFFFFFFF;
  Result := False;
end;

function htmlGetKnownColor(const Name: String; var Color: LongWord): Boolean;
begin
  Result := htmlGetKnownColorPtrW(Pointer(Name), Length(Name), Color);
end;

{ color                                                                        }
function htmlResolveNamedColor(const Name: String; var Color: ThtmlRGBColor): Boolean;
begin
  Result := htmlGetKnownColor(Name, Color.RGB);
  if not Result then
    Color.Color := TColorRec.SysNone;
end;

function htmlDecodeColor(const S: String): ThtmlColor;
var J    : Integer;
    C, L : LongWord;
    V    : Byte;
begin
  if S = '' then // no value
    begin
      Result.ColorType := colorDefault;
      Result.RGBColor.Color := TColorRec.SysNone;
      Result.NamedColor := '';
    end else
  if S[1] = '#' then // hex #rrggbb value
    begin
      C := 0;
      L := 0;
      for J := Length(S) downto 2 do
        begin
          V := HexWideCharDigitToInt(S[J]);
          if V <= $F then
            begin
              C := C or (V shl L);
              Inc(L, 4);
            end;
        end;
      // swap RGB order
      C := ((C and $0000FF) shl 16) or
           ((C and $FF0000) shr 16) or
            (C and $00FF00);
      Result.ColorType := colorRGB;
      Result.RGBColor.RGB := C;
      Result.NamedColor := '';
    end else // named value
    begin
      Result.ColorType := colorNamed;
      Result.RGBColor.Color := TColorRec.SysNone;
      Result.NamedColor := S;
    end;
end;

function htmlcssDecodeColor(const S: String): ThtmlColor;
var J    : Integer;
    C, L : LongWord;
    V    : Byte;
    T    : String;
    U    : StringArray;
begin
  U := nil;
  if S = '' then // no value
    begin
      Result.ColorType := colorDefault;
      Result.RGBColor.Color := TColorRec.SysNone;
      Result.NamedColor := '';
      exit;
    end else
  if S[1] = '#' then // hex #rgb or #rrggbb value
    begin
      C := 0;
      L := 0;
      for J := Length(S) downto 2 do
        begin
          V := HexWideCharDigitToInt(S[J]);
          if V <= $F then
            begin
              C := C or (V shl L);
              Inc(L, 4);
            end;
        end;
      if L <= 12 then // #rgb - Convert to #r0g0b0
        C := ((C and $00F) shl 4)   or
             ((C and $0F0) shl 8)   or
             ((C and $F00) shl 12);
      // swap RGB order
      C := ((C and $0000FF) shl 16) or
           ((C and $FF0000) shr 16) or
            (C and $00FF00);
      Result.ColorType := colorRGB;
      Result.RGBColor.RGB := C;
      Result.NamedColor := '';
      exit;
    end else
  if StrMatchLeftU('rgb', S, False) then // decimal rgb(r,g,b)
    begin
      T := CopyFromU(S, 4);
      StrTrimInPlaceU(T, [#0..#32]);
      L := Length(T);
      if (L >= 2) and (T[1] = '(') and (T[L] = ')') then
        begin
          U := StrSplitChar(T, ',');
          if Length(U) = 3 then
            begin
              for J := 0 to 2 do
                StrTrimInPlaceU(U[J], [#0..#32]);
              Result.RGBColor.R := Byte(StringToIntDefU(U[0], 0));
              Result.RGBColor.G := Byte(StringToIntDefU(U[1], 0));
              Result.RGBColor.B := Byte(StringToIntDefU(U[2], 0));
              Result.ColorType := colorRGB;
              Result.NamedColor := '';
              exit;
            end;
        end;
    end;
  // named color
  Result.ColorType := colorNamed;
  Result.RGBColor.Color := TColorRec.SysNone;
  Result.NamedColor := S;
end;

function htmlColorValue(const Color: ThtmlColor): ThtmlRGBColor;
begin
  case Color.ColorType of
    colorRGB:
      Result.RGB := Color.RGBColor.RGB;
    colorNamed:
      if not htmlResolveNamedColor(Color.NamedColor, Result) then
        Result.Color := TColorRec.SysNone;
  else
    Result.Color := TColorRec.SysNone;
  end;
end;

function htmlResolveColor(const S: String): ThtmlRGBColor;
begin
  Result := htmlColorValue(htmlDecodeColor(S));
end;

function htmlcssResolveColor(const S: String): ThtmlRGBColor;
begin
  Result := htmlColorValue(htmlcssDecodeColor(S));
end;

function htmlRGBColor(const RGB: LongWord): ThtmlRGBColor;
begin
  Result.RGB := RGB;
end;

function htmlDelphiColor(const Color: TColor): ThtmlRGBColor;
begin
  Result.Color := TColor(TColorRec.ColorToRGB(Color));
end;

function htmlEncodeRGBColor(const Color: ThtmlRGBColor): String;
begin
  if (Color.Color = TColorRec.SysNone) or (Color.RGB > $FFFFFF) then
    Result := ''
  else
    Result := '#' + Word32toHexU(Color.RGB, 6);
end;

function htmlEncodeColor(const Color: ThtmlColor): String;
begin
  case Color.ColorType of
    colorRGB     : Result := htmlEncodeRGBColor(Color.RGBColor);
    colorNamed   : Result := Color.NamedColor;
  else
    Result := '';
  end;
end;

{ CSS 'string' values                                                          }
function htmlcssDecodeString(const S: String): String;
var L: Integer;
begin
  Result := S;
  L := Length(S);
  if L = 0 then
    exit;
  // remove quotes
  if (L >= 2) and WideCharInCharSet(S[1], ['"', '''']) and (S[L] = S[1]) then
    begin
      Result := Copy(S, 2, L - 2);
      StrTrimInPlaceU(Result, [#0..#32]);
    end;
  // unescape
  Result := StrReplaceU('\'#13#10, '', Result);
  Result := StrReplaceU('\'#10#13, '', Result);
  StrCharUnescapeU(Result, '\', ['(', ')', ',', ' ', '''', '"', 'A', #13, #10],
                            ['(', ')', ',', ' ', '''', '"', #10, '',  ''], False);
end;

{ CSS 'uri' values                                                             }
function htmlcssDecodeURI(const S: String): String;
var L: Integer;
begin
  Result := '';
  L := Length(S);
  if L = 0 then
    exit;
  if (L >= 5) and StrMatchLeftU(S, 'url', False) and WideCharInCharSet(S[4], [#0..#32, '(']) then
    begin
      // remove url- prefix
      Result := Copy(S, 4, L - 3);
      StrTrimInPlaceU(Result, [#0..#32]);
      L := Length(Result);
      // remove brackets
      if (L >= 2) and (Result[1] = '(') and (Result[L] = ')') then
        begin
          Result := Copy(Result, 2, L - 2);
          StrTrimInPlaceU(Result, [#0..#32]);
        end;
      // decode string
      Result := htmlcssDecodeString(Result);
    end
  else
    Result := S;
end;

{ background-image                                                             }
{ ::= <url> | none                                                             }
function htmlcssDecodeBackgroundImage(const S: String): ThtmlBackgroundImage;
begin
  if S = '' then
    Result.ImageType := backimgDefault else
  if StrEqualNoAsciiCase(S, 'none') then
    Result.ImageType := backimgNone
  else
    begin
      Result.ImageType := backimgURL;
      Result.URL := htmlcssDecodeURI(S);
    end;
end;

{ background-repeat                                                            }
function htmlcssDecodeBackgroundRepeat(const S: String): ThtmlBackgroundRepeat;
begin
  if S = '' then
    Result := brepDefault else
  if StrEqualNoAsciiCase(S, 'repeat') then
    Result := brepRepeat else
  if StrEqualNoAsciiCase(S, 'repeat-x') then
    Result := brepRepeatX else
  if StrEqualNoAsciiCase(S, 'repeat-y') then
    Result := brepRepeatY else
  if StrEqualNoAsciiCase(S, 'no-repeat') then
    Result := brepNoRepeat
  else
    Result := brepDefault;
end;

{ background-attachment                                                        }
function htmlcssDecodeBackgroundAttachment(const S: String): ThtmlBackgroundAttachment;
begin
  if S = '' then
    Result := battDefault else
  if StrEqualNoAsciiCase(S, 'scroll') then
    Result := battScroll else
  if StrEqualNoAsciiCase(S, 'fixed') then
    Result := battFixed
  else
    Result := battDefault;
end;

{ background                                                                   }
{ ::= <background-color> || <background-image> || <background-repeat> ||       }
{     <background-attachment> || <background-position>                         }
{ TODO: Parse <background-position>                                            }
procedure htmlcssDecodeBackground(const S: String;
    var BackColor: ThtmlColor; var BackImage: ThtmlBackgroundImage;
    var BackRepeat: ThtmlBackgroundRepeat; var BackAttach: ThtmlBackgroundAttachment);
begin
  BackColor := htmlcssDecodeColor(S);
  BackImage := htmlcssDecodeBackgroundImage(S);
  BackRepeat := htmlcssDecodeBackgroundRepeat(S);
  BackAttach := htmlcssDecodeBackgroundAttachment(S);
end;

{ vertical-align                                                               }
{ ::= baseline | sub | super | top | text-top | middle | bottom | text-bottom  }
{     | <percentage>                                                           }
function htmlcssDecodeVerticalAlign(const S: String): ThtmlVerticalAlign;
var L: Integer;
begin
  Result.AlignType := valignDefault;
  Result.Value := -1;
  L := Length(S);
  if L > 0 then
    if S[L] = '%' then
      begin
        Result.AlignType := valignPercent;
        Result.Value := htmlDecodeInteger(Copy(S, 1, L - 1), -1);
      end else
    if StrEqualNoAsciiCase(S, 'baseline') then
      Result.AlignType := valignBaseline else
    if StrEqualNoAsciiCase(S, 'sub') then
      Result.AlignType := valignSub else
    if StrEqualNoAsciiCase(S, 'super') then
      Result.AlignType := valignSuper else
    if StrEqualNoAsciiCase(S, 'top') then
      Result.AlignType := valignTop else
    if StrEqualNoAsciiCase(S, 'text-top') then
      Result.AlignType := valignTextTop else
    if StrEqualNoAsciiCase(S, 'middle') then
      Result.AlignType := valignMiddle else
    if StrEqualNoAsciiCase(S, 'bottom') then
      Result.AlignType := valignBottom else
    if StrEqualNoAsciiCase(S, 'text-bottom') then
      Result.AlignType := valignTextBottom;
end;

{ text-align                                                                   }
{ ::= left | right | center | justify                                          }
function htmlDecodeTextAlignType(const S: String): ThtmlTextAlignType;
begin
  if StrEqualNoAsciiCase(S, 'left') then
    Result := talignLeft else
  if StrEqualNoAsciiCase(S, 'right') then
    Result := talignRight else
  if StrEqualNoAsciiCase(S, 'center') then
    Result := talignCenter else
  if StrEqualNoAsciiCase(S, 'justify') then
    Result := talignJustify
  else
    Result := talignDefault;
end;

function htmlEncodeTextAlignType(const Align: ThtmlTextAlignType): String;
begin
  case Align of
    talignDefault : Result := '';
    talignLeft    : Result := 'left';
    talignRight   : Result := 'right';
    talignCenter  : Result := 'center';
    talignJustify : Result := 'justify';
  else
    Result := '';
  end;
end;

{ line-height                                                                  }
function htmlcssDecodeLineHeight(const S: String): ThtmlLineHeight;
begin
  Result := htmlcssDecodeLength(S);
  case Result.LengthType of
    lenAbsolute : Result.LengthType := lenRelFontHeight;
  end;
end;

{ margin                                                                       }
{ ::= <length> | <percentage> | auto                                           }
const
  BoxSidesMap: Array[1..4] of Array[0..3] of Integer =
    ((0, 0, 0, 0),
     (0, 1, 0, 1),
     (0, 1, 2, 1),
     (0, 1, 2, 3));

function htmlcssDecodeMargin(const S: String): ThtmlMargin;
begin
  if StrEqualNoAsciiCase(S, 'auto') then
    begin
      Result.LengthType := lenDefault;
      Result.Value := 0.0;
    end else
    Result := htmlcssDecodeLength(S);
end;

function htmlcssDecodeMargins(const S: String): ThtmlMargins;
var T: StringArray;
    M: Array[0..3] of ThtmlMargin;
    L, I: Integer;
begin
  T := nil;
  Result.Top.LengthType := lenDefault;
  Result.Bottom.LengthType := lenDefault;
  Result.Left.LengthType := lenDefault;
  Result.Right.LengthType := lenDefault;
  if S = '' then
    exit;
  T := StrSplitChar(S, ' ');
  TrimStrings(T, [#0..#32]);
  L := 0;
  for I := 0 to Length(T) - 1 do
    if T[I] <> '' then
      begin
        M[L] := htmlcssDecodeMargin(T[I]);
        Inc(L);
        if L = 4 then
          break;
      end;
  if L > 0 then
    begin
      Result.Top := M[BoxSidesMap[L][0]];
      Result.Right := M[BoxSidesMap[L][1]];
      Result.Bottom := M[BoxSidesMap[L][2]];
      Result.Left := M[BoxSidesMap[L][3]];
    end;
end;

{ padding                                                                      }
{ ::= <length> | <percentage>                                                  }
function htmlcssDecodePadding(const S: String): ThtmlPadding;
begin
  Result := htmlcssDecodeLength(S);
end;

function htmlcssDecodePaddings(const S: String): ThtmlPaddings;
var T: StringArray;
    M: Array[0..3] of ThtmlPadding;
    L, I: Integer;
begin
  T := nil;
  Result.Top.LengthType := lenDefault;
  Result.Bottom.LengthType := lenDefault;
  Result.Left.LengthType := lenDefault;
  Result.Right.LengthType := lenDefault;
  if S = '' then
    exit;
  T := StrSplitChar(S, ' ');
  TrimStrings(T, [#0..#32]);
  L := 0;
  for I := 0 to Length(T) - 1 do
    if T[I] <> '' then
      begin
        M[L] := htmlcssDecodePadding(T[I]);
        Inc(L);
        if L = 4 then
          break;
      end;
  if L > 0 then
    begin
      Result.Top := M[BoxSidesMap[L][0]];
      Result.Right := M[BoxSidesMap[L][1]];
      Result.Bottom := M[BoxSidesMap[L][2]];
      Result.Left := M[BoxSidesMap[L][3]];
    end;
end;

function htmlResolvePaddings(const Paddings: ThtmlPaddings): TRect;
begin
  Result.Top := htmlResolveLengthPixels(Paddings.Top, 0);
  Result.Left := htmlResolveLengthPixels(Paddings.Left, 0);
  Result.Bottom := htmlResolveLengthPixels(Paddings.Bottom, 0);
  Result.Right := htmlResolveLengthPixels(Paddings.Right, 0);
end;

procedure htmlApplyResolvedPaddings(var Dest: TRect; const Source: TRect);
begin
  if Source.Top >= 0 then
    Dest.Top := Source.Top;
  if Source.Left >= 0 then
    Dest.Left := Source.Left;
  if Source.Bottom >= 0 then
    Dest.Bottom := Source.Bottom;
  if Source.Right >= 0 then
    Dest.Right := Source.Right;
end;

{ border-width                                                                 }
function htmlcssDecodeBorderWidth(const S: String): ThtmlBorderWidth;
begin
  Result.WidthType := borwidthDefault;
  Result.Len.LengthType := lenDefault;
  Result.Len.Value := 0.0;
  if S = '' then
    exit;
  if StrEqualNoAsciiCase(S, 'thin') then
    Result.WidthType := borwidthThin else
  if StrEqualNoAsciiCase(S, 'medium') then
    Result.WidthType := borwidthMedium else
  if StrEqualNoAsciiCase(S, 'thick') then
    Result.WidthType := borwidthThick else
    begin
      Result.Len := htmlcssDecodeLength(S);
      if Result.Len.LengthType <> lenDefault then
        Result.WidthType := borwidthLength;
    end;
end;

function htmlcssDecodeBorderWidths(const S: String): ThtmlBorderWidths;
var T: StringArray;
    M: Array[0..3] of ThtmlBorderWidth;
    L, I: Integer;
begin
  T := nil;
  Result.Top.WidthType := borwidthMedium;
  Result.Bottom.WidthType := borwidthMedium;
  Result.Left.WidthType := borwidthMedium;
  Result.Right.WidthType := borwidthMedium;
  if S = '' then
    exit;
  T := StrSplitChar(S, ' ');
  TrimStrings(T, [#0..#32]);
  L := 0;
  for I := 0 to Length(T) - 1 do
    if T[I] <> '' then
      begin
        M[L] := htmlcssDecodeBorderWidth(T[I]);
        Inc(L);
        if L = 4 then
          break;
      end;
  if L > 0 then
    begin
      Result.Top := M[BoxSidesMap[L][0]];
      Result.Right := M[BoxSidesMap[L][1]];
      Result.Bottom := M[BoxSidesMap[L][2]];
      Result.Left := M[BoxSidesMap[L][3]];
    end;
end;

{ border-style                                                                 }
function htmlcssDecodeBorderStyle(const S: String): ThtmlBorderStyle;
begin
  Result := borstyleDefault;
  if S <> '' then
    if StrEqualNoAsciiCase(S, 'none') then
      Result := borstyleNone else
    if StrEqualNoAsciiCase(S, 'dotted') then
      Result := borstyleDotted else
    if StrEqualNoAsciiCase(S, 'dashed') then
      Result := borstyleDashed else
    if StrEqualNoAsciiCase(S, 'solid') then
      Result := borstyleSolid else
    if StrEqualNoAsciiCase(S, 'double') then
      Result := borstyleDouble else
    if StrEqualNoAsciiCase(S, 'groove') then
      Result := borstyleGroove else
    if StrEqualNoAsciiCase(S, 'ridge') then
      Result := borstyleRidge else
    if StrEqualNoAsciiCase(S, 'inset') then
      Result := borstyleInset else
    if StrEqualNoAsciiCase(S, 'outset') then
      Result := borstyleOutset;
end;

{ float                                                                        }
function htmlcssDecodeFloatType(const S: String): ThtmlFloatType;
begin
  Result := floatNone;
  if S <> '' then
    if StrEqualNoAsciiCase(S, 'left') then
      Result := floatLeft else
    if StrEqualNoAsciiCase(S, 'right') then
      Result := floatRight else
    if StrEqualNoAsciiCase(S, 'none') then
      Result := floatNone;
end;

{ white-space                                                                  }
function htmlcssDecodeWhiteSpaceType(const S: String): ThtmlWhiteSpaceType;
begin
  Result := wsDefault;
  if S <> '' then
    if StrEqualNoAsciiCase(S, 'normal') then
      Result := wsNormal else
    if StrEqualNoAsciiCase(S, 'pre') then
      Result := wsPre else
    if StrEqualNoAsciiCase(S, 'nowrap') then
      Result := wsNoWrap;
end;

{ display                                                                      }
function htmlcssDecodeDisplayType(const S: String): ThtmlDisplayType;
begin
  Result := displayDefault;
  if S <> '' then
    if StrEqualNoAsciiCase(S, 'block') then
      Result := displayBlock else
    if StrEqualNoAsciiCase(S, 'inline') then
      Result := displayInline else
    if StrEqualNoAsciiCase(S, 'list-item') then
      Result := displayListItem else
    if StrEqualNoAsciiCase(S, 'none') then
      Result := displayNone;
end;

{ list-style-position                                                          }
function htmlcssDecodeListStylePositionType(const S: String): ThtmlListStylePositionType;
begin
  if S = '' then
    Result := liststyleposDefault else
  if StrEqualNoAsciiCase(S, 'inside') then
    Result := liststyleposInside else
  if StrEqualNoAsciiCase(S, 'outside') then
    Result := liststyleposOutside
  else
    Result := liststyleposDefault;
end;

{ list-style                                                                   }
function htmlcssDecodeListStyleType(const S: String): ThtmlListStyleType;
begin
  Result := liststyleDefault;
  if S <> '' then
    if StrEqualNoAsciiCase(S, 'disc') then
      Result := liststyleDisc else
    if StrEqualNoAsciiCase(S, 'circle') then
      Result := liststyleCircle else
    if StrEqualNoAsciiCase(S, 'square') then
      Result := liststyleSquare else
    if StrEqualNoAsciiCase(S, 'decimal') then
      Result := liststyleDecimal else
    if StrEqualNoAsciiCase(S, 'lower-roman') then
      Result := liststyleLowerRoman else
    if StrEqualNoAsciiCase(S, 'upper-roman') then
      Result := liststyleUpperRoman else
    if StrEqualNoAsciiCase(S, 'lower-alpha') then
      Result := liststyleLowerAlpha else
    if StrEqualNoAsciiCase(S, 'upper-alpha') then
      Result := liststyleUpperAlpha else
    if StrEqualNoAsciiCase(S, 'none') then
      Result := liststyleNone else
    if StrEqualNoAsciiCase(S, 'inside') then
      Result := liststyleInside else
    if StrEqualNoAsciiCase(S, 'outside') then
      Result := liststyleOutside;
end;



{ Tests                                                                        }
{$IFDEF HTML_TEST}
{$ASSERTIONS ON}
procedure Test;
begin
  Assert(htmlResolveColor('Black').RGB = 0, 'htmlResolveColor');
  Assert(htmlResolveColor('White').RGB = $FFFFFF, 'htmlResolveColor');
  Assert(htmlResolveColor('Red').RGB = $0000FF, 'htmlResolveColor');
  Assert(htmlResolveColor('#123456').RGB = $563412, 'htmlResolveColor');
  Assert(htmlResolveColor('#12').RGB = $120000, 'htmlResolveColor');
  Assert(htmlResolveColor('#AbCdEf').R = $AB, 'htmlResolveColor');
  Assert(htmlResolveColor('Blue').B = $FF, 'htmlResolveColor');
  Assert(htmlResolveColor('BLUE').B = $FF, 'htmlResolveColor');
end;
{$ENDIF}



end.

