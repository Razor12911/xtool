{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals 5.00 - HTML Parser                          }
{   File name:        flcHTMLCharEntity.pas                                    }
{   File version:     5.04                                                     }
{   Description:      HTML named character entities                            }
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
{   2002/12/08  1.01  Part of cHTMLConsts.                                     }
{   2015/04/04  1.02  RawByteString changes.                                   }
{   2015/04/11  1.03  UnicodeString changes.                                   }
{   2019/02/22  5.04  Revised for Fundamentals 5.                              }
{                                                                              }
{******************************************************************************}

{$INCLUDE flcHTML.inc}

unit flcHTMLCharEntity;

interface



{                                                                              }
{ HTML Named Character Entities                                                }
{                                                                              }
function  htmlDecodeCharEntity(const Entity: String): Word;



{                                                                              }
{ htmlCharRef                                                                  }
{                                                                              }
function htmlCharRef(const CharVal: LongWord; const UseHex: Boolean): String;



{                                                                              }
{ Tests                                                                        }
{                                                                              }
{$IFDEF HTML_TEST}
procedure Test;
{$ENDIF}



implementation

uses
  flcStdTypes,
  flcUtils,
  flcDynArrays;



{                                                                              }
{ HTML Named Character Entities                                                }
{                                                                              }
type
  THtmlEntity = record
    Name : String;
    Char : Word;
  end;

const
  // HTML 4 character entity references
  HtmlEntities = 253;
  HtmlEntity: array[0..HtmlEntities - 1] of ThtmlEntity = (
      (* Additional                                                           *)
      (Name:'apos';     Char:39),
      (* HTMLspecial.ent                                                      *)
      (Name:'quot';     Char:34),    (Name:'amp';      Char:38),
      (Name:'lt';       Char:60),    (Name:'gt';       Char:62),
      { Latin Extended-A }
      (Name:'OElig';    Char:338),   (Name:'oelig';    Char:339),
      (Name:'Scaron';   Char:352),   (Name:'scaron';   Char:353),
      (Name:'Yuml';     Char:376),
      { Spacing Modifier Letters }
      (Name:'circ';     Char:710),   (Name:'tilde';    Char:732),
      { General Punctuation }
      (Name:'ensp';     Char:8194),  (Name:'emsp';     Char:8195),
      (Name:'thinsp';   Char:8201),  (Name:'zwnj';     Char:8204),
      (Name:'zwj';      Char:8205),  (Name:'lrm';      Char:8206),
      (Name:'rlm';      Char:8207),  (Name:'ndash';    Char:8211),
      (Name:'mdash';    Char:8212),  (Name:'lsquo';    Char:8216),
      (Name:'rsquo';    Char:8217),  (Name:'sbquo';    Char:8218),
      (Name:'ldquo';    Char:8220),  (Name:'rdquo';    Char:8221),
      (Name:'bdquo';    Char:8222),  (Name:'dagger';   Char:8224),
      (Name:'Dagger';   Char:8225),  (Name:'permil';   Char:8240),
      (Name:'lsaquo';   Char:8249),  (Name:'rsaquo';   Char:8250),
      (Name:'euro';     Char:8364),
      (* HTMLsymbol.ent                                                       *)
      { Latin Extended-B }
      (Name:'fnof';     Char:402),
      { Greek }
      (Name:'Alpha';    Char:913),   (Name:'Beta';     Char:914),
      (Name:'Gamma';    Char:915),   (Name:'Delta';    Char:916),
      (Name:'Epsilon';  Char:917),   (Name:'Zeta';     Char:918),
      (Name:'Eta';      Char:919),   (Name:'Theta';    Char:920),
      (Name:'Iota';     Char:921),   (Name:'Kappa';    Char:922),
      (Name:'Lambda';   Char:923),   (Name:'Mu';       Char:924),
      (Name:'Nu';       Char:925),   (Name:'Xi';       Char:926),
      (Name:'Omicron';  Char:927),   (Name:'Pi';       Char:928),
      (Name:'Rho';      Char:929),   (Name:'Sigma';    Char:931),
      (Name:'Tau';      Char:932),   (Name:'Upsilon';  Char:933),
      (Name:'Phi';      Char:934),   (Name:'Chi';      Char:935),
      (Name:'Psi';      Char:936),   (Name:'Omega';    Char:937),
      (Name:'alpha';    Char:945),   (Name:'beta';     Char:946),
      (Name:'gamma';    Char:947),   (Name:'delta';    Char:948),
      (Name:'epsilon';  Char:949),   (Name:'zeta';     Char:950),
      (Name:'eta';      Char:951),   (Name:'theta';    Char:952),
      (Name:'iota';     Char:953),   (Name:'kappa';    Char:954),
      (Name:'lambda';   Char:955),   (Name:'mu';       Char:956),
      (Name:'nu';       Char:957),   (Name:'xi';       Char:958),
      (Name:'omicron';  Char:959),   (Name:'pi';       Char:960),
      (Name:'rho';      Char:961),   (Name:'sigmaf';   Char:962),
      (Name:'sigma';    Char:963),   (Name:'tau';      Char:964),
      (Name:'upsilon';  Char:965),   (Name:'phi';      Char:966),
      (Name:'chi';      Char:967),   (Name:'psi';      Char:968),
      (Name:'omega';    Char:969),   (Name:'thetasym'; Char:977),
      (Name:'upsih';    Char:978),   (Name:'piv';      Char:982),
      { General Punctuation }
      (Name:'bull';     Char:8226),  (Name:'hellip';   Char:8230),
      (Name:'prime';    Char:8242),  (Name:'Prime';    Char:8243),
      (Name:'oline';    Char:8254),  (Name:'frasl';    Char:8260),
      { Letterlike Symbols }
      (Name:'weierp';   Char:8472),  (Name:'image';    Char:8465),
      (Name:'real';     Char:8476),  (Name:'trade';    Char:8482),
      (Name:'alefsym';  Char:8501),
      { Arrows }
      (Name:'larr';     Char:8592),  (Name:'uarr';     Char:8593),
      (Name:'rarr';     Char:8594),  (Name:'darr';     Char:8595),
      (Name:'harr';     Char:8596),  (Name:'crarr';    Char:8629),
      (Name:'lArr';     Char:8656),  (Name:'uArr';     Char:8657),
      (Name:'rArr';     Char:8658),  (Name:'dArr';     Char:8659),
      (Name:'hArr';     Char:8660),
      { Mathematical Operators }
      (Name:'forall';   Char:8704),  (Name:'part';     Char:8706),
      (Name:'exist';    Char:8707),  (Name:'empty';    Char:8709),
      (Name:'nabla';    Char:8711),  (Name:'isin';     Char:8712),
      (Name:'notin';    Char:8713),  (Name:'ni';       Char:8715),
      (Name:'prod';     Char:8719),  (Name:'sum';      Char:8721),
      (Name:'minus';    Char:8722),  (Name:'lowast';   Char:8727),
      (Name:'radic';    Char:8730),  (Name:'prop';     Char:8733),
      (Name:'infin';    Char:8734),  (Name:'ang';      Char:8736),
      (Name:'and';      Char:8743),  (Name:'or';       Char:8744),
      (Name:'cap';      Char:8745),  (Name:'cup';      Char:8746),
      (Name:'int';      Char:8747),  (Name:'there4';   Char:8756),
      (Name:'sim';      Char:8764),  (Name:'cong';     Char:8773),
      (Name:'asymp';    Char:8776),  (Name:'ne';       Char:8800),
      (Name:'equiv';    Char:8801),  (Name:'le';       Char:8804),
      (Name:'ge';       Char:8805),  (Name:'sub';      Char:8834),
      (Name:'sup';      Char:8835),  (Name:'nsub';     Char:8836),
      (Name:'sube';     Char:8838),  (Name:'supe';     Char:8839),
      (Name:'oplus';    Char:8853),  (Name:'otimes';   Char:8855),
      (Name:'perp';     Char:8869),  (Name:'sdot';     Char:8901),
      { Miscellaneous Technical }
      (Name:'lceil';    Char:8968),  (Name:'rceil';    Char:8969),
      (Name:'lfloor';   Char:8970),  (Name:'rfloor';   Char:8971),
      (Name:'lang';     Char:9001),  (Name:'rang';     Char:9002),
      (Name:'loz';      Char:9674),
      { Miscellaneous Symbols }
      (Name:'spades';   Char:9824),  (Name:'clubs';    Char:9827),
      (Name:'hearts';   Char:9829),  (Name:'diams';    Char:9830),

      (* HTMLlat1.ent                                                         *)
      (Name:'nbsp';     Char:160),   (Name:'iexcl';    Char:161),
      (Name:'cent';     Char:162),   (Name:'pound';    Char:163),
      (Name:'curren';   Char:164),   (Name:'yen';      Char:165),
      (Name:'brvbar';   Char:166),   (Name:'sect';     Char:167),
      (Name:'uml';      Char:168),   (Name:'copy';     Char:169),
      (Name:'ordf';     Char:170),   (Name:'laquo';    Char:171),
      (Name:'not';      Char:172),   (Name:'shy';      Char:173),
      (Name:'reg';      Char:174),   (Name:'macr';     Char:175),
      (Name:'deg';      Char:176),   (Name:'plusmn';   Char:177),
      (Name:'sup2';     Char:178),   (Name:'sup3';     Char:179),
      (Name:'acute';    Char:180),   (Name:'micro';    Char:181),
      (Name:'para';     Char:182),   (Name:'middot';   Char:183),
      (Name:'cedil';    Char:184),   (Name:'sup1';     Char:185),
      (Name:'ordm';     Char:186),   (Name:'raquo';    Char:187),
      (Name:'frac14';   Char:188),   (Name:'frac12';   Char:189),
      (Name:'frac34';   Char:190),   (Name:'iquest';   Char:191),
      (Name:'Agrave';   Char:192),   (Name:'Aacute';   Char:193),
      (Name:'Acirc';    Char:194),   (Name:'Atilde';   Char:195),
      (Name:'Auml';     Char:196),   (Name:'Aring';    Char:197),
      (Name:'AElig';    Char:198),   (Name:'Ccedil';   Char:199),
      (Name:'Egrave';   Char:200),   (Name:'Eacute';   Char:201),
      (Name:'Ecirc';    Char:202),   (Name:'Euml';     Char:203),
      (Name:'Igrave';   Char:204),   (Name:'Iacute';   Char:205),
      (Name:'Icirc';    Char:206),   (Name:'Iuml';     Char:207),
      (Name:'ETH';      Char:208),   (Name:'Ntilde';   Char:209),
      (Name:'Ograve';   Char:210),   (Name:'Oacute';   Char:211),
      (Name:'Ocirc';    Char:212),   (Name:'Otilde';   Char:213),
      (Name:'Ouml';     Char:214),   (Name:'times';    Char:215),
      (Name:'Oslash';   Char:216),   (Name:'Ugrave';   Char:217),
      (Name:'Uacute';   Char:218),   (Name:'Ucirc';    Char:219),
      (Name:'Uuml';     Char:220),   (Name:'Yacute';   Char:221),
      (Name:'THORN';    Char:222),   (Name:'szlig';    Char:223),
      (Name:'agrave';   Char:224),   (Name:'aacute';   Char:225),
      (Name:'acirc';    Char:226),   (Name:'atilde';   Char:227),
      (Name:'auml';     Char:228),   (Name:'aring';    Char:229),
      (Name:'aelig';    Char:230),   (Name:'ccedil';   Char:231),
      (Name:'egrave';   Char:232),   (Name:'eacute';   Char:233),
      (Name:'ecirc';    Char:234),   (Name:'euml';     Char:235),
      (Name:'igrave';   Char:236),   (Name:'iacute';   Char:237),
      (Name:'icirc';    Char:238),   (Name:'iuml';     Char:239),
      (Name:'eth';      Char:240),   (Name:'ntilde';   Char:241),
      (Name:'ograve';   Char:242),   (Name:'oacute';   Char:243),
      (Name:'ocirc';    Char:244),   (Name:'otilde';   Char:245),
      (Name:'ouml';     Char:246),   (Name:'divide';   Char:247),
      (Name:'oslash';   Char:248),   (Name:'ugrave';   Char:249),
      (Name:'uacute';   Char:250),   (Name:'ucirc';    Char:251),
      (Name:'uuml';     Char:252),   (Name:'yacute';   Char:253),
      (Name:'thorn';    Char:254),   (Name:'yuml';     Char:255)
      );

const
  HtmlEntityHashSize = HtmlEntities;

var
  HtmlEntityHashIndex : array of LongIntArray;
  HtmlEntityHashInit  : Boolean = False;

procedure InitHTMLEntityHash;
var I: Integer;
begin
  HtmlEntityHashIndex := nil;
  SetLength(HtmlEntityHashIndex, HtmlEntityHashSize);
  for I := 0 to HtmlEntities - 1 do
    DynArrayAppend(HtmlEntityHashIndex[HashStr(HtmlEntity[I].Name, 1, -1, True, HtmlEntityHashSize)], I);
  HtmlEntityHashInit := True;
end;

function htmlDecodeCharEntity(const Entity: String): Word;
var I, J, H: Integer;
begin
  if not HtmlEntityHashInit then
    InitHTMLEntityHash;
  H := HashStr(Entity, 1, -1, True, HtmlEntityHashSize);
  for I := 0 to Length(HtmlEntityHashIndex[H]) - 1 do
    begin
      J := HtmlEntityHashIndex[H][I];
      if Entity = HtmlEntity[J].Name then // case-sensitive
        begin
          Result := HtmlEntity[J].Char;
          exit;
        end;
    end;
  Result := 0;
end;



{                                                                              }
{ htmlCharRef                                                                  }
{                                                                              }
function htmlCharRef(const CharVal: LongWord; const UseHex: Boolean): String;
begin
  if UseHex then
    if CharVal <= $FF then
      Result := '#x' + Word32toHex(CharVal, 2) + ';'
    else
    if CharVal <= $FFFF then
      Result := '#x' + Word32toHex(CharVal, 4) + ';'
    else
      Result := '#x' + Word32toHex(CharVal, 6) + ';'
  else
    Result := '#' + Word32ToStr(CharVal) + ';';
end;



{                                                                              }
{ Tests                                                                        }
{                                                                              }
{$IFDEF HTML_TEST}
{$ASSERTIONS ON}
procedure Test;
begin
  Assert(htmlDecodeCharEntity('quot') = 34, 'htmlDecodeCharEntity');
  Assert(htmlDecodeCharEntity('QUOT') = 0, 'htmlDecodeCharEntity');
  Assert(htmlDecodeCharEntity('pi') = 960, 'htmlDecodeCharEntity');
  Assert(htmlDecodeCharEntity('xyz') = 0, 'htmlDecodeCharEntity');
end;
{$ENDIF}


end.
