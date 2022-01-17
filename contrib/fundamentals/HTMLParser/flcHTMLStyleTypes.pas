{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals 5.00 - HTML Parser                          }
{   File name:        flcHTMLStyleTypes.pas                                    }
{   File version:     5.06                                                     }
{   Description:      HTML style types                                         }
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
{   2002/11/03  1.00  Part of cHTMLUtils.                                      }
{   2002/11/04  1.01  ThtmlcssPropertyID.                                      }
{   2002/12/08  1.02  Part of cHTMLConsts.                                     }
{   2012/12/16  1.03  HTML 5.1 tags.                                           }
{   2015/04/04  1.04  RawByteString changes.                                   }
{   2015/04/11  1.05  UnicodeString changes.                                   }
{   2019/02/22  5.06  Revised for Fundamentals 5.                              }
{                                                                              }
{******************************************************************************}

{$INCLUDE flcHTML.inc}

unit flcHTMLStyleTypes;

interface



{                                                                              }
{ HTML CSS Properties                                                          }
{                                                                              }
type
  ThtmlcssPropertyID = (
      HTML_CSS_PROP_None,
      { Font properties }
      HTML_CSS_PROP_font_family,
      HTML_CSS_PROP_font_style,
      HTML_CSS_PROP_font_variant,
      HTML_CSS_PROP_font_weight,
      HTML_CSS_PROP_font_size,
      HTML_CSS_PROP_font,
      { Color and background properties }
      HTML_CSS_PROP_color,
      HTML_CSS_PROP_background_color,
      HTML_CSS_PROP_background_image,
      HTML_CSS_PROP_background_repeat,
      HTML_CSS_PROP_background_attachment,
      HTML_CSS_PROP_background_position,
      HTML_CSS_PROP_background,
      { Text properties }
      HTML_CSS_PROP_word_spacing,
      HTML_CSS_PROP_letter_spacing,
      HTML_CSS_PROP_text_decoration,
      HTML_CSS_PROP_vertical_align,
      HTML_CSS_PROP_text_transform,
      HTML_CSS_PROP_text_align,
      HTML_CSS_PROP_text_indent,
      HTML_CSS_PROP_line_height,
      { Box properties }
      HTML_CSS_PROP_margin_top,
      HTML_CSS_PROP_margin_right,
      HTML_CSS_PROP_margin_bottom,
      HTML_CSS_PROP_margin_left,
      HTML_CSS_PROP_margin,
      HTML_CSS_PROP_padding_top,
      HTML_CSS_PROP_padding_right,
      HTML_CSS_PROP_padding_bottom,
      HTML_CSS_PROP_padding_left,
      HTML_CSS_PROP_padding,
      HTML_CSS_PROP_border_top_width,
      HTML_CSS_PROP_border_right_width,
      HTML_CSS_PROP_border_bottom_width,
      HTML_CSS_PROP_border_left_width,
      HTML_CSS_PROP_border_width,
      HTML_CSS_PROP_border_color,
      HTML_CSS_PROP_border_style,
      HTML_CSS_PROP_border_top,
      HTML_CSS_PROP_border_right,
      HTML_CSS_PROP_border_bottom,
      HTML_CSS_PROP_border_left,
      HTML_CSS_PROP_border,
      HTML_CSS_PROP_width,
      HTML_CSS_PROP_height,
      HTML_CSS_PROP_float,
      HTML_CSS_PROP_clear,
      { Classification properties }
      HTML_CSS_PROP_display,
      HTML_CSS_PROP_white_space,
      HTML_CSS_PROP_list_style_type,
      HTML_CSS_PROP_list_style_image,
      HTML_CSS_PROP_list_style_position,
      HTML_CSS_PROP_list_style
    );

const
  HTML_CSS_PROP_FirstID = HTML_CSS_PROP_font_family;
  HTML_CSS_PROP_LastID  = HTML_CSS_PROP_list_style;

function  htmlcssGetPropertyIDPtrW(const Name: PWideChar; const NameLen: Integer): ThtmlcssPropertyID;
function  htmlcssGetPropertyName(const PropID: ThtmlcssPropertyID): String;



{                                                                              }
{ HTML CSS Pseudo Properties                                                   }
{                                                                              }
type
  ThtmlcssPseudoPropertyID = (
      HTML_CSS_PP_None,
      { Anchor pseudo classes }
      HTML_CSS_PP_Link,
      HTML_CSS_PP_Visited,
      HTML_CSS_PP_Hover,
      HTML_CSS_PP_Active,
      HTML_CSS_PP_Focus,
      { Typographical pseudo-elements }
      HTML_CSS_PP_First_Line,
      HTML_CSS_PP_First_Letter
    );
  ThtmlcssPseudoPropertyIDSet = Set of ThtmlcssPseudoPropertyID;

  ThtmlcssAnchorPseudoPropertyState = (
      anchorLink,
      anchorLink_Hover,
      anchorLink_Active,
      anchorLink_Focus,
      anchorVisited,
      anchorVisited_Hover,
      anchorVisited_Active,
      anchorVisited_Focus
    );

const
  HTML_CSS_PP_FirstID = HTML_CSS_PP_Link;
  HTML_CSS_PP_LastID  = HTML_CSS_PP_First_Letter;

  HTML_CSS_PP_Properties_Anchor : ThtmlcssPseudoPropertyIDSet =
      [HTML_CSS_PP_Link..HTML_CSS_PP_Focus];
  HTML_CSS_PP_PseudoClasses     : ThtmlcssPseudoPropertyIDSet =
      [HTML_CSS_PP_Link, HTML_CSS_PP_Visited, HTML_CSS_PP_Hover,
       HTML_CSS_PP_Active, HTML_CSS_PP_Focus];
  HTML_CSS_PP_PseudoElements    : ThtmlcssPseudoPropertyIDSet =
      [HTML_CSS_PP_First_Line, HTML_CSS_PP_First_Letter];

  HTML_CSS_PP_Properties_AnchorState : Array[ThtmlcssAnchorPseudoPropertyState]
      of ThtmlcssPseudoPropertyIDSet = (
          [HTML_CSS_PP_Link],
          [HTML_CSS_PP_Link, HTML_CSS_PP_Hover],
          [HTML_CSS_PP_Link, HTML_CSS_PP_Active],
          [HTML_CSS_PP_Link, HTML_CSS_PP_Focus],
          [HTML_CSS_PP_Visited],
          [HTML_CSS_PP_Visited, HTML_CSS_PP_Hover],
          [HTML_CSS_PP_Visited, HTML_CSS_PP_Active],
          [HTML_CSS_PP_Visited, HTML_CSS_PP_Focus]);

function  htmlcssGetPseudoPropIDPtrW(const Name: PWideChar;
          const NameLen: Integer): ThtmlcssPseudoPropertyID;



implementation

uses
  flcStrings;



{                                                                              }
{ HTML CSS Properties                                                          }
{                                                                              }
const
  htmlcssPropertyTable: array[ThtmlcssPropertyID] of String = ('',
      'font-family',            'font-style',
      'font-variant',           'font-weight',
      'font-size',              'font',
      'color',                  'background-color',
      'background-image',       'background-repeat',
      'background-attachment',  'background-position',
      'background',             'word-spacing',
      'letter-spacing',         'text-decoration',
      'vertical-align',         'text-transform',
      'text-align',             'text-indent',
      'line-height',            'margin-top',
      'margin-right',           'margin-bottom',
      'margin-left',            'margin',
      'padding-top',            'padding-right',
      'padding-bottom',         'padding-left',
      'padding',                'border-top-width',
      'border-right-width',     'border-bottom-width',
      'border-left-width',      'border-width',
      'border-color',           'border-style',
      'border-top',             'border-right',
      'border-bottom',          'border-left',
      'border',                 'width',
      'height',                 'float',
      'clear',                  'display',
      'white-space',            'list-style-type',
      'list-style-image',       'list-style-position',
      'list-style');

function htmlcssGetPropertyIDPtrW(const Name: PWideChar; const NameLen: Integer): ThtmlcssPropertyID;
var I: ThtmlcssPropertyID;
begin
  if NameLen > 0 then
    for I := HTML_CSS_PROP_FirstID to HTML_CSS_PROP_LastID do
      if (NameLen = Length(htmlcssPropertyTable[I])) and
         StrPMatchNoAsciiCase(Name, Pointer(htmlcssPropertyTable[I]), NameLen) then
        begin
          Result := I;
          exit;
        end;
  Result := HTML_CSS_PROP_None;
end;

var
  PropNameRef: Array[ThtmlcssPropertyID] of String;

function htmlcssGetPropertyName(const PropID: ThtmlcssPropertyID): String;
begin
  if (PropID < HTML_CSS_PROP_FirstID) or (PropID > HTML_CSS_PROP_LastID) then
    Result := '' else
    begin
      Result := PropNameRef[PropID]; // reference
      if Result <> '' then
        exit;
      Result := htmlcssPropertyTable[PropID]; // copy
      PropNameRef[PropID] := Result; // reference
    end;
end;

const
  htmlcssPseudoClassTable: Array[ThtmlcssPseudoPropertyID] of String = ('',
      'Link', 'Visited', 'Hover', 'Active', 'Focus',
      'First-Line', 'First-Letter');

function htmlcssGetPseudoPropIDPtrW(const Name: PWideChar; const NameLen: Integer): ThtmlcssPseudoPropertyID;
var I: ThtmlcssPseudoPropertyID;
begin
  if NameLen > 0 then
    for I := HTML_CSS_PP_FirstID to HTML_CSS_PP_LastID do
      if (Length(htmlcssPseudoClassTable[I]) = NameLen) and
         (StrPMatchNoAsciiCase(Name, Pointer(htmlcssPseudoClassTable[I]), NameLen)) then
        begin
          Result := I;
          exit;
        end;
  Result := HTML_CSS_PP_None;
end;



end.
