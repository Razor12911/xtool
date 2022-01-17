{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals 5.00                                        }
{   File name:        flcStrings.pas                                           }
{   File version:     5.71                                                     }
{   Description:      String utility functions                                 }
{                                                                              }
{   Copyright:        Copyright (c) 1999-2020, David J Butler                  }
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
{   1999/10/19  0.01  Split from Maths unit.                                   }
{   1999/10/26  0.02  Revision.                                                }
{   1999/10/30  0.03  Added Count, Reverse.                                    }
{   1999/10/31  0.04  Improved Match.                                          }
{                     Added Replace, Count, PadInside.                         }
{   1999/11/06  1.05  Added Remove, TrimEllipse.                               }
{   1999/11/09  1.06  Added Pack functions.                                    }
{   1999/11/17  1.07  Added PosN, Before, After, Between and Split.            }
{   1999/11/22  1.08  Added Join.                                              }
{   1999/11/23  1.09  Added Translate.                                         }
{   1999/12/02  1.10  Fixed bugs in Replace and Match reported by daiqingbo.   }
{   1999/12/27  1.11  Added SelfTest procedure and Bug fixes.                  }
{   2000/01/04  1.12  Added InsensitiveCharSet.                                }
{   2000/01/08  1.13  Added Append.                                            }
{   2000/05/08  1.14  Revision.                                                }
{   2000/07/20  1.15  Bug fixes.                                               }
{   2000/08/30  1.16  Bug fixes.                                               }
{   2000/09/04  1.17  Added MatchFileMask.                                     }
{   2000/09/31  1.18  Added HexEscapeText and HexUnescapeText.                 }
{   2000/12/04  1.19  Changes to CopyRange, CopyLeft to avoid memory           }
{                     allocation in specific cases.                            }
{   2001/04/22  1.20  Added CaseSensitive parameter to Match, PosNext, PosN    }
{   2001/04/25  1.21  Added CopyEx, MatchLeft and MatchRight.                  }
{   2001/04/28  1.22  Refactoring.                                             }
{                     Replaced PosNext and PosPrev with Pos.                   }
{   2001/04/29  1.23  Improvements.                                            }
{   2001/05/13  1.24  Added simple regular expression matching.                }
{                     Added CharClassStr conversion routines.                  }
{   2001/06/01  1.25  Added TQuickLexer                                        }
{   2001/07/07  1.26  Optimizations.                                           }
{   2001/07/30  1.27  Revision.                                                }
{   2001/08/22  1.28  Revision.                                                }
{   2001/11/11  2.29  Revision.                                                }
{   2002/02/14  2.30  Added MatchPattern.                                      }
{   2002/04/03  3.31  Added string functions from cUtils.                      }
{   2002/04/14  3.32  Moved TQuickLexer to unit cQuickLexer.                   }
{   2002/12/14  3.33  Major revision. Removed rarely used functions.           }
{   2003/07/28  3.34  Minor changes.                                           }
{   2003/08/04  3.35  Changed parameters of StrMatch functions to be           }
{                     consistent with other string functions.                  }
{                     Changed StrCompare functions to return integer result.   }
{   2003/09/06  3.36  Removed dependancy on Delphi's Math and Variant units.   }
{                     This saves about 25K when used in a DLL.                 }
{   2003/11/07  3.37  Compilable with FreePascal 1.90 Win32 i386.              }
{   2004/07/31  3.38  Improved StrReplace function to efficiently handle       }
{                     cases where millions of matches are found.               }
{   2004/08/01  3.39  Added ToLongWord conversion functions.                   }
{   2005/06/10  4.40  Compilable with FreePascal 2 Win32 i386.                 }
{   2005/09/20  4.41  Added TStringBuilder class.                              }
{   2005/09/21  4.42  Revised for Fundamentals 4.                              }
{   2007/06/08  4.43  Compilable with FreePascal 2.0.4 Win32 i386              }
{   2008/08/18  4.44  StrP functions; added Str prefix to some functions.      }
{   2009/01/04  4.45  Added AsciiChar and AsciiString.                         }
{                     Initial update for Delphi 2009.                          }
{   2009/10/09  4.46  Compilable with Delphi 2009 Win32/.NET.                  }
{   2010/06/27  4.47  Compilable with FreePascal 2.4.0 OSX x86-64              }
{   2011/03/17  4.48  Compilable with Delphi 5.                                }
{   2011/05/28  4.49  Fix in TWideStringBuilder.                               }
{   2011/06/14  4.50  Added Append(BufPtr) method to TStringBuilder.           }
{   2011/09/30  4.51  Improved UnicodeString support.                          }
{   2011/10/17  4.52  WideString functions.                                    }
{   2012/03/26  4.53  Add A and W string functions.                            }
{   2012/08/24  4.54  Improvements to StrZ functions.                          }
{   2012/08/25  4.55  Add U string functions.                                  }
{   2012/08/28  4.56  Add Unicode character functions from cUnicode.           }
{   2012/08/29  4.57  Improve pattern matcher functions.                       }
{   2013/05/12  4.58  Move string type definitions to Utils unit.              }
{   2014/08/26  4.59  StringBuilder unit tests.                                }
{   2015/03/13  4.60  RawByteString functions.                                 }
{   2015/04/11  4.61  UnicodeString functions.                                 }
{   2016/01/09  5.62  Revised for Fundamentals 5.                              }
{   2016/04/13  5.63  Change pattern match functions to RawByteString.         }
{   2016/04/16  5.64  Changes to compile with FreePascal 3.0.0.                }
{   2017/10/07  5.65  Created units flcStringPatternMatcher, flcStringBuilder  }
{   2017/10/24  5.66  Created units flcUnicodeChar, flcZeroTerminatedString    }
{   2018/07/17  5.67  Type changes.                                            }
{   2018/08/11  5.68  Created unit flcUnicodeString.                           }
{   2018/08/11  5.69  Removed dependency on flcCharSet.                        }
{   2018/08/12  5.70  Removed WideString functions and CLR code.               }
{   2018/08/14  5.71  ByteChar changes.                                        }
{                                                                              }
{ Supported compilers:                                                         }
{                                                                              }
{   Delphi 2010-10.4 Win32/Win64        5.71  2020/06/02                       }
{   Delphi 10.2-10.4 Linux64            5.71  2020/06/02                       }
{   FreePascal 3.0.4 Win64              5.71  2020/06/02                       }
{                                                                              }
{******************************************************************************}

{$INCLUDE ..\flcInclude.inc}

{$IFDEF FREEPASCAL}
  {$WARNINGS OFF}
  {$HINTS OFF}
{$ENDIF}

{$IFDEF DEBUG}
{$IFDEF TEST}
  {$DEFINE STRINGS_TEST}
{$ENDIF}
{$ENDIF}

unit flcStrings;

interface

uses
  { System }
  SysUtils,

  { Fundamentals }
  flcStdTypes;



{                                                                              }
{ Exceptions                                                                   }
{                                                                              }
type
  EStringError = class(Exception);
  EStrInvalidArgument = class(EStringError);



{                                                                              }
{ ByteChar constants                                                           }
{                                                                              }
const
  { Common characters }
  chTab          = ByteChar(#9);
  chSpace        = ByteChar(#32);
  chDecimalPoint = ByteChar('.');
  chComma        = ByteChar(',');
  chBackSlash    = ByteChar('\');
  chForwardSlash = ByteChar('/');
  chPercent      = ByteChar('%');
  chAmpersand    = ByteChar('&');
  chPlus         = ByteChar('+');
  chMinus        = ByteChar('-');
  chEqual        = ByteChar('=');
  chLessThan     = ByteChar('<');
  chGreaterThan  = ByteChar('>');
  chSingleQuote  = ByteChar('''');
  chDoubleQuote  = ByteChar('"');
  chExclamation  = ByteChar('!');
  chHash         = ByteChar('#');
  chDollar       = ByteChar('$');
  chCaret        = ByteChar('^');
  chAsterisk     = ByteChar('*');

  { Common sequences }
  CRLF        = ByteChar(#13) + ByteChar(#10);
  LFCR        = ByteChar(#10) + ByteChar(#13);
  DosNewLine  = CRLF;
  UnixNewLine = ByteChar(#10);

  { Character sets }
  csComplete        = [ByteChar(#0)..ByteChar(#255)];
  csAnsi            = [ByteChar(#0)..ByteChar(#255)];
  csAscii           = [ByteChar(#0)..ByteChar(#127)];
  csNotAscii        = csComplete - csAscii;
  csAsciiCtl        = [ByteChar(#0)..ByteChar(#31)];
  csAsciiText       = [ByteChar(#32)..ByteChar(#127)];
  csAlphaLow        = [ByteChar('a')..ByteChar('z')];
  csAlphaUp         = [ByteChar('A')..ByteChar('Z')];
  csAlpha           = csAlphaLow + csAlphaUp;
  csNotAlpha        = csComplete - csAlpha;
  csNumeric         = [ByteChar('0')..ByteChar('9')];
  csNotNumeric      = csComplete - csNumeric;
  csAlphaNumeric    = csNumeric + csAlpha;
  csNotAlphaNumeric = csComplete - csAlphaNumeric;
  csWhiteSpace      = csAsciiCtl + [ByteChar(#32)];
  csSign            = [chPlus, chMinus];
  csExponent        = [ByteChar('E'), ByteChar('e')];
  csBinaryDigit     = [ByteChar('0')..ByteChar('1')];
  csOctalDigit      = [ByteChar('0')..ByteChar('7')];
  csHexDigitLow     = csNumeric + [ByteChar('a')..ByteChar('f')];
  csHexDigitUp      = csNumeric + [ByteChar('A')..ByteChar('F')];
  csHexDigit        = csNumeric + [ByteChar('A')..ByteChar('F'), ByteChar('a')..ByteChar('f')];
  csQuotes          = [chSingleQuote, chDoubleQuote];
  csParentheses     = [ByteChar('('), ByteChar(')')];
  csCurlyBrackets   = [ByteChar('{'), ByteChar('}')];
  csBlockBrackets   = [ByteChar('['), ByteChar(']')];
  csPunctuation     = [ByteChar('.'), ByteChar(','), ByteChar(':'), ByteChar('/'),
                       ByteChar('?'), ByteChar('<'), ByteChar('>'), ByteChar(';'),
                       ByteChar('"'), ByteChar(''''), ByteChar('['), ByteChar(']'),
                       ByteChar('{'), ByteChar('}'), ByteChar('+'), ByteChar('='),
                       ByteChar('-'), ByteChar('\'), ByteChar('('), ByteChar(')'),
                       ByteChar('*'), ByteChar('&'), ByteChar('^'), ByteChar('%'),
                       ByteChar('$'), ByteChar('#'), ByteChar('@'), ByteChar('!'),
                       ByteChar('`'), ByteChar('~')];
  csSlash           = [chBackSlash, chForwardSlash];



{                                                                              }
{ UCS4 constants                                                               }
{                                                                              }
const
  Ucs4NULL             = UCS4Char(0);
  Ucs4HT               = UCS4Char(9);
  Ucs4LF               = UCS4Char(10);
  Ucs4CR               = UCS4Char(13);
  Ucs4StringTerminator = UCS4Char($9C);



{                                                                              }
{ String functions                                                             }
{                                                                              }
{$IFDEF SupportAnsiString}
procedure SetLengthAndZeroA(var S: AnsiString; const NewLength: Integer);
{$ENDIF}
procedure SetLengthAndZeroB(var S: RawByteString; const NewLength: Integer);
procedure SetLengthAndZeroU(var S: UnicodeString; const NewLength: Integer);

function  ToStringChB(const A: ByteChar): String; {$IFDEF UseInline}inline;{$ENDIF}
function  ToStringChW(const A: WideChar): String; {$IFDEF UseInline}inline;{$ENDIF}

{$IFDEF SupportAnsiString}
function  ToStringA(const A: AnsiString): String; {$IFDEF UseInline}inline;{$ENDIF}
{$ENDIF}
function  ToStringB(const A: RawByteString): String; {$IFDEF UseInline}inline;{$ENDIF}
function  ToStringU(const A: UnicodeString): String; {$IFDEF UseInline}inline;{$ENDIF}



{                                                                              }
{ Match                                                                        }
{                                                                              }
function  CharMatchNoAsciiCaseB(const A, B: ByteChar): Boolean;
function  CharMatchNoAsciiCaseW(const A, B: WideChar): Boolean;
function  CharMatchNoAsciiCaseBW(const A: ByteChar; const B: WideChar): Boolean;
function  CharMatchNoAsciiCaseBS(const A: ByteChar; const B: Char): Boolean;
function  CharMatchNoAsciiCase(const A, B: Char): Boolean;

function  CharMatchB(const A, B: ByteChar; const AsciiCaseSensitive: Boolean = True): Boolean;
function  CharMatchW(const A, B: WideChar; const AsciiCaseSensitive: Boolean = True): Boolean;
function  CharMatchBW(const A: ByteChar; const B: WideChar; const AsciiCaseSensitive: Boolean = True): Boolean;
function  CharMatchBS(const A: ByteChar; const B: Char; const AsciiCaseSensitive: Boolean = True): Boolean;
function  CharMatch(const A, B: Char; const AsciiCaseSensitive: Boolean = True): Boolean;

function  CharSetMatchCharB(const A: ByteCharSet; const B: ByteChar; const AsciiCaseSensitive: Boolean = True): Boolean;
function  CharSetMatchCharW(const A: ByteCharSet; const B: WideChar; const AsciiCaseSensitive: Boolean = True): Boolean;
function  CharSetMatchChar(const A: ByteCharSet; const B: Char; const AsciiCaseSensitive: Boolean = True): Boolean;

function  StrPMatchB(const A, B: PByteChar; const Len: Integer): Boolean; overload;
function  StrPMatchW(const A, B: PWideChar; const Len: Integer): Boolean; overload;
function  StrPMatchBW(const A: PWideChar; B: PByteChar; const Len: Integer): Boolean; overload;
function  StrPMatchBS(const A: PChar; B: PByteChar; const Len: Integer): Boolean; overload;
function  StrPMatch(const A, B: PChar; const Len: Integer): Boolean; overload;

function  StrPMatchB(const S, M: PByteChar; const LenS, LenM: Integer): Boolean; overload;
function  StrPMatchW(const S, M: PWideChar; const LenS, LenM: Integer): Boolean; overload;
function  StrPMatchBW(const S: PWideChar; const M: PByteChar; const LenS, LenM: Integer): Boolean; overload;
function  StrPMatchBS(const S: PChar; const M: PByteChar; const LenS, LenM: Integer): Boolean; overload;
function  StrPMatch(const S, M: PChar; const LenS, LenM: Integer): Boolean; overload;
function  StrPMatchStrPB(const S: PChar; const M: PByteChar; const LenS, LenM: Integer): Boolean;

{$IFDEF SupportAnsiString}
function  StrPMatchStrA(const S: PAnsiChar; const Len: Integer; const M: AnsiString): Boolean;
{$ENDIF}
function  StrPMatchStrB(const S: PByteChar; const Len: Integer; const M: RawByteString): Boolean;
{$IFDEF SupportAnsiString}
function  StrPMatchStrAW(const S: PWideChar; const Len: Integer; const M: AnsiString): Boolean;
function  StrPMatchStrAS(const S: PChar; const Len: Integer; const M: AnsiString): Boolean;
{$ENDIF}
function  StrPMatchStrU(const S: PWideChar; const Len: Integer; const M: UnicodeString): Boolean;
function  StrPMatchStr(const S: PChar; const Len: Integer; const M: String): Boolean;

function  StrPMatchNoAsciiCaseB(const A, B: PByteChar; const Len: Integer): Boolean;
function  StrPMatchNoAsciiCaseW(const A, B: PWideChar; const Len: Integer): Boolean;
function  StrPMatchNoAsciiCaseBW(const A: PWideChar; const B: PByteChar; const Len: Integer): Boolean;
function  StrPMatchNoAsciiCaseBS(const A: PChar; const B: PByteChar; const Len: Integer): Boolean;
function  StrPMatchNoAsciiCase(const A, B: PChar; const Len: Integer): Boolean;

function  StrPMatchLenB(const P: PByteChar; const Len: Integer; const M: ByteCharSet): Integer;
function  StrPMatchLenW(const P: PWideChar; const Len: Integer; const M: ByteCharSet): Integer; overload;
function  StrPMatchLenW(const P: PWideChar; const Len: Integer; const M: TWideCharMatchFunction): Integer; overload;
function  StrPMatchLen(const P: PChar; const Len: Integer; const M: ByteCharSet): Integer;

function  StrPMatchCountB(const P: PByteChar; const Len: Integer; const M: ByteCharSet): Integer;
function  StrPMatchCountW(const P: PWideChar; const Len: Integer; const M: ByteCharSet): Integer; overload;
function  StrPMatchCountW(const P: PWideChar; const Len: Integer; const M: TWideCharMatchFunction): Integer; overload;

function  StrPMatchCharB(const P: PByteChar; const Len: Integer; const M: ByteCharSet): Boolean;
function  StrPMatchCharW(const P: PWideChar; const Len: Integer; const M: ByteCharSet): Boolean; overload;
function  StrPMatchCharW(const P: PWideChar; const Len: Integer; const M: TWideCharMatchFunction): Boolean; overload;
function  StrPMatchChar(const P: PChar; const Len: Integer; const M: ByteCharSet): Boolean;

{$IFDEF SupportAnsiString}
function  StrMatchA(const S, M: AnsiString; const Index: Integer = 1): Boolean;
{$ENDIF}
function  StrMatchB(const S, M: RawByteString; const Index: Integer = 1): Boolean;
function  StrMatchU(const S, M: UnicodeString; const Index: Integer = 1): Boolean;
{$IFDEF SupportAnsiString}
function  StrMatchAU(const S: UnicodeString; const M: AnsiString; const Index: Integer = 1): Boolean;
{$ENDIF}
function  StrMatchBU(const S: UnicodeString; const M: RawByteString; const Index: Integer = 1): Boolean;
{$IFDEF SupportAnsiString}
function  StrMatchAS(const S: String; const M: AnsiString; const Index: Integer = 1): Boolean;
{$ENDIF}
function  StrMatch(const S, M: String; const Index: Integer = 1): Boolean;

{$IFDEF SupportAnsiString}
function  StrMatchNoAsciiCaseA(const S, M: AnsiString; const Index: Integer = 1): Boolean;
{$ENDIF}
function  StrMatchNoAsciiCaseB(const S, M: RawByteString; const Index: Integer = 1): Boolean;
function  StrMatchNoAsciiCaseU(const S, M: UnicodeString; const Index: Integer = 1): Boolean;
{$IFDEF SupportAnsiString}
function  StrMatchNoAsciiCaseAU(const S: UnicodeString; const M: AnsiString; const Index: Integer = 1): Boolean;
function  StrMatchNoAsciiCaseAS(const S: String; const M: AnsiString; const Index: Integer = 1): Boolean;
{$ENDIF}
function  StrMatchNoAsciiCaseBU(const S: UnicodeString; const M: RawByteString; const Index: Integer = 1): Boolean;
function  StrMatchNoAsciiCase(const S, M: String; const Index: Integer = 1): Boolean;

{$IFDEF SupportAnsiString}
function  StrMatchLeftA(const S, M: AnsiString; const AsciiCaseSensitive: Boolean = True): Boolean;
{$ENDIF}
function  StrMatchLeftB(const S, M: RawByteString; const AsciiCaseSensitive: Boolean = True): Boolean;
function  StrMatchLeftU(const S, M: UnicodeString; const AsciiCaseSensitive: Boolean = True): Boolean;
{$IFDEF SupportAnsiString}
function  StrMatchLeftAU(const S: UnicodeString; const M: AnsiString; const AsciiCaseSensitive: Boolean = True): Boolean;
{$ENDIF}
function  StrMatchLeftBU(const S: UnicodeString; const M: RawByteString; const AsciiCaseSensitive: Boolean = True): Boolean;
function  StrMatchLeft(const S, M: String; const AsciiCaseSensitive: Boolean = True): Boolean;

{$IFDEF SupportAnsiString}
function  StrMatchRightA(const S, M: AnsiString; const AsciiCaseSensitive: Boolean = True): Boolean;
{$ENDIF}
function  StrMatchRightB(const S, M: RawByteString; const AsciiCaseSensitive: Boolean = True): Boolean;
function  StrMatchRightU(const S, M: UnicodeString; const AsciiCaseSensitive: Boolean = True): Boolean;
{$IFDEF SupportAnsiString}
function  StrMatchRightAU(const S: UnicodeString; const M: AnsiString; const AsciiCaseSensitive: Boolean = True): Boolean;
{$ENDIF}
function  StrMatchRight(const S, M: String; const AsciiCaseSensitive: Boolean = True): Boolean;

{$IFDEF SupportAnsiString}
function  StrMatchLenA(const S: AnsiString; const M: ByteCharSet; const Index: Integer = 1): Integer;
{$ENDIF}
function  StrMatchLenB(const S: RawByteString; const M: ByteCharSet; const Index: Integer = 1): Integer;
function  StrMatchLenU(const S: UnicodeString; const M: ByteCharSet; const Index: Integer = 1): Integer; overload;
function  StrMatchLenU(const S: UnicodeString; const M: TWideCharMatchFunction; const Index: Integer = 1): Integer; overload;
function  StrMatchLen(const S: String; const M: ByteCharSet; const Index: Integer = 1): Integer;

{$IFDEF SupportAnsiString}
function  StrMatchCharA(const S: AnsiString; const M: ByteCharSet): Boolean;
{$ENDIF}
function  StrMatchCharB(const S: RawByteString; const M: ByteCharSet): Boolean;
function  StrMatchCharU(const S: UnicodeString; const M: ByteCharSet): Boolean; overload;
function  StrMatchCharU(const S: UnicodeString; const M: TWideCharMatchFunction): Boolean; overload;
function  StrMatchChar(const S: String; const M: ByteCharSet): Boolean;



{                                                                              }
{ Equal                                                                        }
{                                                                              }
function  StrPEqual(const P1, P2: PByteChar; const Len1, Len2: Integer; const AsciiCaseSensitive: Boolean = True): Boolean;
{$IFDEF SupportAnsiString}
function  StrPEqualStr(const P: PAnsiChar; const Len: Integer; const S: AnsiString; const AsciiCaseSensitive: Boolean = True): Boolean;
{$ENDIF}

{$IFDEF SupportAnsiString}
function  StrEqualA(const A, B: AnsiString; const AsciiCaseSensitive: Boolean = True): Boolean;
{$ENDIF}
function  StrEqualB(const A, B: RawByteString; const AsciiCaseSensitive: Boolean = True): Boolean;
function  StrEqualU(const A, B: UnicodeString; const AsciiCaseSensitive: Boolean = True): Boolean;
{$IFDEF SupportAnsiString}
function  StrEqualAU(const A: UnicodeString; const B: AnsiString; const AsciiCaseSensitive: Boolean = True): Boolean;
{$ENDIF}
function  StrEqualBU(const A: UnicodeString; const B: RawByteString; const AsciiCaseSensitive: Boolean = True): Boolean;
function  StrEqual(const A, B: String; const AsciiCaseSensitive: Boolean = True): Boolean;

{$IFDEF SupportAnsiString}
function  StrEqualNoAsciiCaseA(const A, B: AnsiString): Boolean;
{$ENDIF}
function  StrEqualNoAsciiCaseB(const A, B: RawByteString): Boolean;
function  StrEqualNoAsciiCaseU(const A, B: UnicodeString): Boolean;
function  StrEqualNoAsciiCaseBU(const A: UnicodeString; const B: RawByteString): Boolean;
function  StrEqualNoAsciiCase(const A, B: String): Boolean;



{                                                                              }
{ Validation                                                                   }
{                                                                              }
{$IFDEF SupportAnsiString}
function  StrIsNumericA(const S: AnsiString): Boolean;
{$ENDIF}
function  StrIsNumericB(const S: RawByteString): Boolean;
function  StrIsNumericU(const S: UnicodeString): Boolean;
function  StrIsNumeric(const S: String): Boolean;

{$IFDEF SupportAnsiString}
function  StrIsHexA(const S: AnsiString): Boolean;
{$ENDIF}
function  StrIsHexB(const S: RawByteString): Boolean;
function  StrIsHexU(const S: UnicodeString): Boolean;
function  StrIsHex(const S: String): Boolean;

{$IFDEF SupportAnsiString}
function  StrIsAlphaA(const S: AnsiString): Boolean;
{$ENDIF}
function  StrIsAlphaB(const S: RawByteString): Boolean;
function  StrIsAlphaU(const S: UnicodeString): Boolean;
function  StrIsAlpha(const S: String): Boolean;

{$IFDEF SupportAnsiString}
function  StrIsAlphaNumericA(const S: AnsiString): Boolean;
{$ENDIF}
function  StrIsAlphaNumericB(const S: RawByteString): Boolean;
function  StrIsAlphaNumericU(const S: UnicodeString): Boolean;
function  StrIsAlphaNumeric(const S: String): Boolean;

{$IFDEF SupportAnsiString}
function  StrIsIntegerA(const S: AnsiString): Boolean;
{$ENDIF}
function  StrIsIntegerB(const S: RawByteString): Boolean;
function  StrIsIntegerU(const S: UnicodeString): Boolean;
function  StrIsInteger(const S: String): Boolean;



{                                                                              }
{ Pos                                                                          }
{                                                                              }
function  StrPPosCharB(const F: ByteChar; const S: PByteChar; const Len: Integer): Integer;
function  StrPPosCharW(const F: WideChar; const S: PWideChar; const Len: Integer): Integer;

function  StrPPosCharSetB(const F: ByteCharSet; const S: PByteChar; const Len: Integer): Integer;
function  StrPPosCharSetW(const F: ByteCharSet; const S: PWideChar; const Len: Integer): Integer;

function  StrPPosB(const F, S: PByteChar; const LenF, LenS: Integer): Integer;
function  StrPPosW(const F, S: PWideChar; const LenF, LenS: Integer): Integer;

{$IFDEF SupportAnsiString}
function  StrPPosStrA(const F: AnsiString; const S: PAnsiChar; const Len: Integer): Integer;
{$ENDIF}
function  StrPPosStrB(const F: RawByteString; const S: PByteChar; const Len: Integer): Integer;

{$IFDEF SupportAnsiString}
function  PosCharA(const F: AnsiChar; const S: AnsiString; const Index: Integer = 1): Integer;
{$ENDIF}
function  PosCharB(const F: ByteChar; const S: RawByteString; const Index: Integer = 1): Integer;
function  PosCharU(const F: WideChar; const S: UnicodeString; const Index: Integer = 1): Integer;
function  PosChar(const F: Char; const S: String; const Index: Integer = 1): Integer;

{$IFDEF SupportAnsiString}
function  PosCharSetA(const F: ByteCharSet; const S: AnsiString; const Index: Integer = 1): Integer;
{$ENDIF}
function  PosCharSetB(const F: ByteCharSet; const S: RawByteString; const Index: Integer = 1): Integer;
function  PosCharSetU(const F: ByteCharSet; const S: UnicodeString; const Index: Integer = 1): Integer; overload;
function  PosCharSetU(const F: TWideCharMatchFunction; const S: UnicodeString; const Index: Integer = 1): Integer; overload;
function  PosCharSet(const F: ByteCharSet; const S: String; const Index: Integer = 1): Integer;

{$IFDEF SupportAnsiString}
function  PosNotCharA(const F: AnsiChar; const S: AnsiString; const Index: Integer = 1): Integer;
{$ENDIF}
function  PosNotCharB(const F: ByteChar; const S: RawByteString; const Index: Integer = 1): Integer;
function  PosNotCharU(const F: WideChar; const S: UnicodeString; const Index: Integer = 1): Integer;
function  PosNotChar(const F: Char; const S: String; const Index: Integer = 1): Integer;

{$IFDEF SupportAnsiString}
function  PosNotCharSetA(const F: ByteCharSet; const S: AnsiString; const Index: Integer = 1): Integer;
{$ENDIF}
function  PosNotCharSetB(const F: ByteCharSet; const S: RawByteString; const Index: Integer = 1): Integer;
function  PosNotCharSetU(const F: ByteCharSet; const S: UnicodeString; const Index: Integer = 1): Integer; overload;
function  PosNotCharSetU(const F: TWideCharMatchFunction; const S: UnicodeString; const Index: Integer = 1): Integer; overload;
function  PosNotCharSet(const F: ByteCharSet; const S: String; const Index: Integer = 1): Integer;

{$IFDEF SupportAnsiString}
function  PosCharRevA(const F: AnsiChar; const S: AnsiString; const Index: Integer = 1): Integer;
{$ENDIF}
function  PosCharRevB(const F: ByteChar; const S: RawByteString; const Index: Integer = 1): Integer;
function  PosCharRevU(const F: WideChar; const S: UnicodeString; const Index: Integer = 1): Integer;
function  PosCharRev(const F: Char; const S: String; const Index: Integer = 1): Integer;

{$IFDEF SupportAnsiString}
function  PosCharSetRevA(const F: ByteCharSet; const S: AnsiString; const Index: Integer = 1): Integer;
{$ENDIF}
function  PosCharSetRevB(const F: ByteCharSet; const S: RawByteString; const Index: Integer = 1): Integer;
function  PosCharSetRevU(const F: ByteCharSet; const S: UnicodeString; const Index: Integer = 1): Integer; overload;
function  PosCharSetRevU(const F: TWideCharMatchFunction; const S: UnicodeString; const Index: Integer = 1): Integer; overload;
function  PosCharSetRev(const F: ByteCharSet; const S: String; const Index: Integer = 1): Integer;

{$IFDEF SupportAnsiString}
function  PosStrA(const F, S: AnsiString; const Index: Integer = 1; const AsciiCaseSensitive: Boolean = True): Integer;
{$ENDIF}
function  PosStrB(const F, S: RawByteString; const Index: Integer = 1; const AsciiCaseSensitive: Boolean = True): Integer;
function  PosStrU(const F, S: UnicodeString; const Index: Integer = 1; const AsciiCaseSensitive: Boolean = True): Integer;
{$IFDEF SupportAnsiString}
function  PosStrAU(const F: AnsiString; const S: UnicodeString; const Index: Integer = 1; const AsciiCaseSensitive: Boolean = True): Integer;
{$ENDIF}
function  PosStr(const F, S: String; const Index: Integer = 1; const AsciiCaseSensitive: Boolean = True): Integer;

{$IFDEF SupportAnsiString}
function  PosStrRevA(const F, S: AnsiString; const Index: Integer = 1; const AsciiCaseSensitive: Boolean = True): Integer;
{$ENDIF}
function  PosStrRevB(const F, S: RawByteString; const Index: Integer = 1; const AsciiCaseSensitive: Boolean = True): Integer;
function  PosStrRevU(const F, S: UnicodeString; const Index: Integer = 1; const AsciiCaseSensitive: Boolean = True): Integer;
function  PosStrRev(const F, S: String; const Index: Integer = 1; const AsciiCaseSensitive: Boolean = True): Integer;

{$IFDEF SupportAnsiString}
function  PosStrRevIdxA(const F, S: AnsiString; const Index: Integer = 1; const AsciiCaseSensitive: Boolean = True): Integer;
{$ENDIF}
function  PosStrRevIdxB(const F, S: RawByteString; const Index: Integer = 1; const AsciiCaseSensitive: Boolean = True): Integer;
function  PosStrRevIdxU(const F, S: UnicodeString; const Index: Integer = 1; const AsciiCaseSensitive: Boolean = True): Integer;
function  PosStrRevIdx(const F, S: String; const Index: Integer = 1; const AsciiCaseSensitive: Boolean = True): Integer;

{$IFDEF SupportAnsiString}
function  PosNStrA(const F, S: AnsiString; const N: Integer; const Index: Integer = 1; const AsciiCaseSensitive: Boolean = True): Integer;
{$ENDIF}
function  PosNStrB(const F, S: RawByteString; const N: Integer; const Index: Integer = 1; const AsciiCaseSensitive: Boolean = True): Integer;
function  PosNStrU(const F, S: UnicodeString; const N: Integer; const Index: Integer = 1; const AsciiCaseSensitive: Boolean = True): Integer;
function  PosNStr(const F, S: String; const N: Integer; const Index: Integer = 1; const AsciiCaseSensitive: Boolean = True): Integer;



{                                                                              }
{ Copy                                                                         }
{                                                                              }
{   Out-of-range values of StartIndex, StopIndex and Count are clipped.        }
{   These variants return a reference to the existing string if possible.      }
{                                                                              }
{$IFDEF SupportAnsiString}
function  CopyRangeA(const S: AnsiString; const StartIndex, StopIndex: Integer): AnsiString;
{$ENDIF}
function  CopyRangeB(const S: RawByteString; const StartIndex, StopIndex: Integer): RawByteString;
function  CopyRangeU(const S: UnicodeString; const StartIndex, StopIndex: Integer): UnicodeString;
function  CopyRange(const S: String; const StartIndex, StopIndex: Integer): String;

{$IFDEF SupportAnsiString}
function  CopyFromA(const S: AnsiString; const Index: Integer): AnsiString;
{$ENDIF}
function  CopyFromB(const S: RawByteString; const Index: Integer): RawByteString;
function  CopyFromU(const S: UnicodeString; const Index: Integer): UnicodeString;
function  CopyFrom(const S: String; const Index: Integer): String;

{$IFDEF SupportAnsiString}
function  CopyLeftA(const S: AnsiString; const Count: Integer): AnsiString;
{$ENDIF}
function  CopyLeftB(const S: RawByteString; const Count: Integer): RawByteString;
function  CopyLeftU(const S: UnicodeString; const Count: Integer): UnicodeString;
function  CopyLeft(const S: String; const Count: Integer): String;

{$IFDEF SupportAnsiString}
function  CopyRightA(const S: AnsiString; const Count: Integer): AnsiString;
{$ENDIF}
function  CopyRightB(const S: RawByteString; const Count: Integer): RawByteString;
function  CopyRightU(const S: UnicodeString; const Count: Integer): UnicodeString;
function  CopyRight(const S: String; const Count: Integer): String;

{$IFDEF SupportAnsiString}
function  CopyLeftEllipsedA(const S: AnsiString; const Count: Integer): AnsiString;
{$ENDIF}
function  CopyLeftEllipsedU(const S: UnicodeString; const Count: Integer): UnicodeString;



{                                                                              }
{ CopyEx                                                                       }
{                                                                              }
{   CopyEx functions extend Copy so that negative Start/Stop values reference  }
{   indexes from the end of the string, eg. -2 will reference the second last  }
{   character in the string.                                                   }
{                                                                              }
{$IFDEF SupportAnsiString}
function  CopyExA(const S: AnsiString; const Start, Count: Integer): AnsiString;
{$ENDIF}
function  CopyExB(const S: RawByteString; const Start, Count: Integer): RawByteString;
function  CopyExW(const S: String; const Start, Count: Integer): String;
function  CopyExU(const S: UnicodeString; const Start, Count: Integer): UnicodeString;
function  CopyEx(const S: String; const Start, Count: Integer): String;

{$IFDEF SupportAnsiString}
function  CopyRangeExA(const S: AnsiString; const Start, Stop: Integer): AnsiString;
{$ENDIF}
function  CopyRangeExB(const S: RawByteString; const Start, Stop: Integer): RawByteString;
function  CopyRangeExU(const S: UnicodeString; const Start, Stop: Integer): UnicodeString;
function  CopyRangeEx(const S: String; const Start, Stop: Integer): String;

{$IFDEF SupportAnsiString}
function  CopyFromExA(const S: AnsiString; const Start: Integer): AnsiString;
function  CopyFromExB(const S: RawByteString; const Start: Integer): RawByteString;
{$ENDIF}
function  CopyFromExU(const S: UnicodeString; const Start: Integer): UnicodeString;
function  CopyFromEx(const S: String; const Start: Integer): String;



{                                                                              }
{ Trim                                                                         }
{                                                                              }
{$IFDEF SupportAnsiString}
function  StrTrimLeftA(const S: AnsiString;    const C: ByteCharSet = csWhiteSpace): AnsiString;
{$ENDIF}
function  StrTrimLeftB(const S: RawByteString; const C: ByteCharSet = csWhiteSpace): RawByteString;
function  StrTrimLeftU(const S: UnicodeString; const C: ByteCharSet = csWhiteSpace): UnicodeString; overload;
function  StrTrimLeftU(const S: UnicodeString; const C: TWideCharMatchFunction): UnicodeString; overload;
function  StrTrimLeft(const S: String;         const C: ByteCharSet = csWhiteSpace): String;

{$IFDEF SupportAnsiString}
procedure StrTrimLeftInPlaceA(var S: AnsiString;    const C: ByteCharSet = csWhiteSpace);
{$ENDIF}
procedure StrTrimLeftInPlaceB(var S: RawByteString; const C: ByteCharSet = csWhiteSpace);
procedure StrTrimLeftInPlaceU(var S: UnicodeString; const C: ByteCharSet = csWhiteSpace); overload;
procedure StrTrimLeftInPlaceU(var S: UnicodeString; const C: TWideCharMatchFunction); overload;
procedure StrTrimLeftInPlace(var S: String;         const C: ByteCharSet = csWhiteSpace);

{$IFDEF SupportAnsiString}
function  StrTrimLeftStrNoCaseA(const S: AnsiString; const TrimStr: AnsiString): AnsiString;
{$ENDIF}
function  StrTrimLeftStrNoCaseB(const S: RawByteString; const TrimStr: RawByteString): RawByteString;
function  StrTrimLeftStrNoCaseU(const S: UnicodeString; const TrimStr: UnicodeString): UnicodeString;
function  StrTrimLeftStrNoCase(const S: String; const TrimStr: String): String;

{$IFDEF SupportAnsiString}
function  StrTrimRightA(const S: AnsiString;    const C: ByteCharSet = csWhiteSpace): AnsiString;
{$ENDIF}
function  StrTrimRightB(const S: RawByteString; const C: ByteCharSet = csWhiteSpace): RawByteString;
function  StrTrimRightU(const S: UnicodeString; const C: ByteCharSet = csWhiteSpace): UnicodeString; overload;
function  StrTrimRightU(const S: UnicodeString; const C: TWideCharMatchFunction): UnicodeString; overload;
function  StrTrimRight(const S: String;         const C: ByteCharSet = csWhiteSpace): String;

{$IFDEF SupportAnsiString}
procedure StrTrimRightInPlaceA(var S: AnsiString;    const C: ByteCharSet = csWhiteSpace);
{$ENDIF}
procedure StrTrimRightInPlaceB(var S: RawByteString; const C: ByteCharSet = csWhiteSpace);
procedure StrTrimRightInPlaceU(var S: UnicodeString; const C: ByteCharSet = csWhiteSpace); overload;
procedure StrTrimRightInPlaceU(var S: UnicodeString; const C: TWideCharMatchFunction); overload;
procedure StrTrimRightInPlace(var S: String;         const C: ByteCharSet = csWhiteSpace);

{$IFDEF SupportAnsiString}
function  StrTrimRightStrNoCaseA(const S: AnsiString; const TrimStr: AnsiString): AnsiString;
{$ENDIF}
function  StrTrimRightStrNoCaseB(const S: RawByteString; const TrimStr: RawByteString): RawByteString;
function  StrTrimRightStrNoCaseU(const S: UnicodeString; const TrimStr: UnicodeString): UnicodeString;
function  StrTrimRightStrNoCase(const S: String; const TrimStr: String): String;

{$IFDEF SupportAnsiString}
function  StrTrimA(const S: AnsiString; const C: ByteCharSet = csWhiteSpace): AnsiString;
{$ENDIF}
function  StrTrimB(const S: RawByteString; const C: ByteCharSet = csWhiteSpace): RawByteString;
function  StrTrimU(const S: UnicodeString; const C: ByteCharSet = csWhiteSpace): UnicodeString; overload;
function  StrTrimU(const S: UnicodeString; const C: TWideCharMatchFunction): UnicodeString; overload;
function  StrTrim(const S: String; const C: ByteCharSet): String; overload;

{$IFDEF SupportAnsiString}
procedure StrTrimInPlaceA(var S: AnsiString;    const C: ByteCharSet = csWhiteSpace);
{$ENDIF}
procedure StrTrimInPlaceB(var S: RawByteString; const C: ByteCharSet = csWhiteSpace);
procedure StrTrimInPlaceU(var S: UnicodeString; const C: ByteCharSet = csWhiteSpace); overload;
procedure StrTrimInPlaceU(var S: UnicodeString; const C: TWideCharMatchFunction); overload;
procedure StrTrimInPlace(var S: String;         const C: ByteCharSet = csWhiteSpace);

{$IFDEF SupportAnsiString}
procedure TrimStringsA(var S: AnsiStringArray; const C: ByteCharSet = csWhiteSpace); overload;
{$ENDIF}
procedure TrimStringsB(var S: RawByteStringArray; const C: ByteCharSet = csWhiteSpace); overload;
procedure TrimStringsU(var S: UnicodeStringArray; const C: ByteCharSet = csWhiteSpace); overload;
procedure TrimStrings(var S: StringArray; const C: ByteCharSet = csWhiteSpace);



{                                                                              }
{ Duplicate                                                                    }
{                                                                              }
{$IFDEF SupportAnsiString}
function  BufToStrA(const Buf; const BufSize: Integer): AnsiString;
{$ENDIF}
function  BufToStrB(const Buf; const BufSize: Integer): RawByteString;
function  BufToStrU(const Buf; const BufSize: Integer): UnicodeString;
function  BufToStr(const Buf; const BufSize: Integer): String;

{$IFDEF SupportAnsiString}
function  DupBufA(const Buf; const BufSize: Integer; const Count: Integer): AnsiString;
{$ENDIF}
function  DupBufB(const Buf; const BufSize: Integer; const Count: Integer): RawByteString;
function  DupBufU(const Buf; const BufSize: Integer; const Count: Integer): UnicodeString;
function  DupBuf(const Buf; const BufSize: Integer; const Count: Integer): String;

{$IFDEF SupportAnsiString}
function  DupStrA(const S: AnsiString; const Count: Integer): AnsiString;
{$ENDIF}
function  DupStrB(const S: RawByteString; const Count: Integer): RawByteString;
function  DupStrU(const S: UnicodeString; const Count: Integer): UnicodeString;
function  DupStr(const S: String; const Count: Integer): String;

{$IFDEF SupportAnsiString}
function  DupCharA(const Ch: AnsiChar; const Count: Integer): AnsiString;
{$ENDIF}
function  DupCharB(const Ch: ByteChar; const Count: Integer): RawByteString;
function  DupCharU(const Ch: WideChar; const Count: Integer): UnicodeString;
function  DupChar(const Ch: Char; const Count: Integer): String;

{$IFDEF SupportAnsiString}
function  DupSpaceA(const Count: Integer): AnsiString;
{$ENDIF}
function  DupSpaceB(const Count: Integer): RawByteString;
function  DupSpaceU(const Count: Integer): UnicodeString;
function  DupSpace(const Count: Integer): String;



{                                                                              }
{ Pad                                                                          }
{                                                                              }
{$IFDEF SupportAnsiString}
function  StrPadA(const S: AnsiString; const PadChar: AnsiChar; const Len: Integer;
          const Cut: Boolean = False): AnsiString;
{$ENDIF}
function  StrPadB(const S: RawByteString; const PadChar: ByteChar; const Len: Integer;
          const Cut: Boolean = False): RawByteString;
function  StrPadU(const S: UnicodeString; const PadChar: WideChar; const Len: Integer;
          const Cut: Boolean = False): UnicodeString;
function  StrPad(const S: String; const PadChar: Char; const Len: Integer;
          const Cut: Boolean = False): String;

{$IFDEF SupportAnsiString}
function  StrPadLeftA(const S: AnsiString; const PadChar: AnsiChar;
          const Len: Integer; const Cut: Boolean = False): AnsiString;
{$ENDIF}
function  StrPadLeftB(const S: RawByteString; const PadChar: ByteChar;
          const Len: Integer; const Cut: Boolean = False): RawByteString;
function  StrPadLeftU(const S: UnicodeString; const PadChar: WideChar;
          const Len: Integer; const Cut: Boolean = False): UnicodeString;
function  StrPadLeft(const S: String; const PadChar: Char;
          const Len: Integer; const Cut: Boolean = False): String;

{$IFDEF SupportAnsiString}
function  StrPadRightA(const S: AnsiString; const PadChar: AnsiChar;
          const Len: Integer; const Cut: Boolean = False): AnsiString;
{$ENDIF}
function  StrPadRightB(const S: RawByteString; const PadChar: ByteChar;
          const Len: Integer; const Cut: Boolean = False): RawByteString;
function  StrPadRightU(const S: UnicodeString; const PadChar: WideChar;
          const Len: Integer; const Cut: Boolean = False): UnicodeString;
function  StrPadRight(const S: String; const PadChar: Char;
          const Len: Integer; const Cut: Boolean = False): String;



{                                                                              }
{ Delimited                                                                    }
{                                                                              }
{$IFDEF SupportAnsiString}
function  StrBetweenCharA(const S: AnsiString;
          const FirstDelim, SecondDelim: AnsiChar;
          const FirstOptional: Boolean = False;
          const SecondOptional: Boolean = False): AnsiString; overload;
function  StrBetweenCharA(const S: AnsiString;
          const FirstDelim, SecondDelim: ByteCharSet;
          const FirstOptional: Boolean = False;
          const SecondOptional: Boolean = False): AnsiString; overload;

function  StrBetweenCharB(const S: RawByteString;
          const FirstDelim, SecondDelim: ByteChar;
          const FirstOptional: Boolean = False;
          const SecondOptional: Boolean = False): RawByteString; overload;
function  StrBetweenCharB(const S: RawByteString;
          const FirstDelim, SecondDelim: ByteCharSet;
          const FirstOptional: Boolean = False;
          const SecondOptional: Boolean = False): RawByteString; overload;
{$ENDIF}

function  StrBetweenCharU(const S: UnicodeString;
          const FirstDelim, SecondDelim: WideChar;
          const FirstOptional: Boolean = False;
          const SecondOptional: Boolean = False): UnicodeString; overload;
function  StrBetweenCharU(const S: UnicodeString;
          const FirstDelim, SecondDelim: ByteCharSet;
          const FirstOptional: Boolean = False;
          const SecondOptional: Boolean = False): UnicodeString; overload;

function  StrBetweenChar(const S: String;
          const FirstDelim, SecondDelim: Char;
          const FirstOptional: Boolean = False;
          const SecondOptional: Boolean = False): String; overload;
function  StrBetweenChar(const S: String;
          const FirstDelim, SecondDelim: ByteCharSet;
          const FirstOptional: Boolean = False;
          const SecondOptional: Boolean = False): String; overload;

{$IFDEF SupportAnsiString}
function  StrBetweenA(const S: AnsiString;
          const FirstDelim: AnsiString; const SecondDelim: ByteCharSet;
          const FirstOptional: Boolean = False;
          const SecondOptional: Boolean = False;
          const FirstDelimAsciiCaseSensitive: Boolean = True): AnsiString; overload;
function  StrBetweenA(const S: AnsiString;
          const FirstDelim, SecondDelim: AnsiString;
          const FirstOptional: Boolean = False;
          const SecondOptional: Boolean = False;
          const FirstDelimAsciiCaseSensitive: Boolean = True;
          const SecondDelimAsciiCaseSensitive: Boolean = True): AnsiString; overload;

function  StrBetweenB(const S: RawByteString;
          const FirstDelim: RawByteString; const SecondDelim: ByteCharSet;
          const FirstOptional: Boolean = False;
          const SecondOptional: Boolean = False;
          const FirstDelimAsciiCaseSensitive: Boolean = True): RawByteString; overload;
function  StrBetweenB(const S: RawByteString;
          const FirstDelim, SecondDelim: RawByteString;
          const FirstOptional: Boolean = False;
          const SecondOptional: Boolean = False;
          const FirstDelimAsciiCaseSensitive: Boolean = True;
          const SecondDelimAsciiCaseSensitive: Boolean = True): RawByteString; overload;
{$ENDIF}

function  StrBetweenU(const S: UnicodeString;
          const FirstDelim: UnicodeString; const SecondDelim: ByteCharSet;
          const FirstOptional: Boolean = False;
          const SecondOptional: Boolean = False;
          const FirstDelimAsciiCaseSensitive: Boolean = True): UnicodeString; overload;
function  StrBetweenU(const S: UnicodeString;
          const FirstDelim, SecondDelim: UnicodeString;
          const FirstOptional: Boolean = False;
          const SecondOptional: Boolean = False;
          const FirstDelimAsciiCaseSensitive: Boolean = True;
          const SecondDelimAsciiCaseSensitive: Boolean = True): UnicodeString; overload;

function  StrBetween(const S: String;
          const FirstDelim: String; const SecondDelim: ByteCharSet;
          const FirstOptional: Boolean = False;
          const SecondOptional: Boolean = False;
          const FirstDelimAsciiCaseSensitive: Boolean = True): String; overload;
function  StrBetween(const S: String;
          const FirstDelim, SecondDelim: String;
          const FirstOptional: Boolean = False;
          const SecondOptional: Boolean = False;
          const FirstDelimAsciiCaseSensitive: Boolean = True;
          const SecondDelimAsciiCaseSensitive: Boolean = True): String; overload;

{$IFDEF SupportAnsiString}
function  StrBeforeA(const S, D: AnsiString;
          const Optional: Boolean = True;
          const AsciiCaseSensitive: Boolean = True): AnsiString;
function  StrBeforeRevA(const S, D: AnsiString;
          const Optional: Boolean = True;
          const AsciiCaseSensitive: Boolean = True): AnsiString;
{$ENDIF}

function  StrBeforeB(const S, D: RawByteString;
          const Optional: Boolean = True;
          const AsciiCaseSensitive: Boolean = True): RawByteString;
function  StrBeforeRevB(const S, D: RawByteString;
          const Optional: Boolean = True;
          const AsciiCaseSensitive: Boolean = True): RawByteString;

function  StrBeforeU(const S, D: UnicodeString;
          const Optional: Boolean = True;
          const AsciiCaseSensitive: Boolean = True): UnicodeString;
function  StrBeforeRevU(const S, D: UnicodeString;
          const Optional: Boolean = True;
          const AsciiCaseSensitive: Boolean = True): UnicodeString;

function  StrBefore(const S, D: String;
          const Optional: Boolean = True;
          const AsciiCaseSensitive: Boolean = True): String;
function  StrBeforeRev(const S, D: String;
          const Optional: Boolean = True;
          const AsciiCaseSensitive: Boolean = True): String;

{$IFDEF SupportAnsiString}
function  StrBeforeCharA(const S: AnsiString; const D: AnsiChar; const Optional: Boolean = True): AnsiString; overload;
function  StrBeforeCharA(const S: AnsiString; const D: ByteCharSet; const Optional: Boolean = True): AnsiString; overload;
function  StrBeforeCharRevA(const S: AnsiString; const D: ByteCharSet; const Optional: Boolean = True): AnsiString;
{$ENDIF}

function  StrBeforeCharB(const S: RawByteString; const D: ByteChar; const Optional: Boolean = True): RawByteString; overload;
function  StrBeforeCharB(const S: RawByteString; const D: ByteCharSet; const Optional: Boolean = True): RawByteString; overload;
function  StrBeforeCharRevB(const S: RawByteString; const D: ByteCharSet; const Optional: Boolean = True): RawByteString;

function  StrBeforeCharU(const S: UnicodeString; const D: WideChar; const Optional: Boolean = True): UnicodeString; overload;
function  StrBeforeCharU(const S: UnicodeString; const D: ByteCharSet; const Optional: Boolean = True): UnicodeString; overload;
function  StrBeforeCharRevU(const S: UnicodeString; const D: ByteCharSet; const Optional: Boolean = True): UnicodeString;

function  StrBeforeChar(const S: String; const D: Char; const Optional: Boolean = True): String; overload;
function  StrBeforeChar(const S: String; const D: ByteCharSet; const Optional: Boolean = True): String; overload;
function  StrBeforeCharRev(const S: String; const D: ByteCharSet; const Optional: Boolean = True): String;

{$IFDEF SupportAnsiString}
function  StrAfterA(const S, D: AnsiString; const Optional: Boolean = False): AnsiString;
function  StrAfterRevA(const S, D: AnsiString; const Optional: Boolean = False): AnsiString;
{$ENDIF}

function  StrAfterB(const S, D: RawByteString; const Optional: Boolean = False): RawByteString;
function  StrAfterRevB(const S, D: RawByteString; const Optional: Boolean = False): RawByteString;

function  StrAfterU(const S, D: UnicodeString; const Optional: Boolean = False): UnicodeString;
function  StrAfterRevU(const S, D: UnicodeString; const Optional: Boolean = False): UnicodeString;

function  StrAfter(const S, D: String; const Optional: Boolean = False): String;
function  StrAfterRev(const S, D: String; const Optional: Boolean = False): String;

{$IFDEF SupportAnsiString}
function  StrAfterCharA(const S: AnsiString; const D: ByteCharSet): AnsiString; overload;
function  StrAfterCharA(const S: AnsiString; const D: AnsiChar): AnsiString; overload;
{$ENDIF}

function  StrAfterCharB(const S: RawByteString; const D: ByteCharSet): RawByteString; overload;
function  StrAfterCharB(const S: RawByteString; const D: ByteChar): RawByteString; overload;

function  StrAfterCharU(const S: UnicodeString; const D: ByteCharSet): UnicodeString; overload;
function  StrAfterCharU(const S: UnicodeString; const D: WideChar): UnicodeString; overload;

function  StrAfterChar(const S: String; const D: ByteCharSet): String; overload;
function  StrAfterChar(const S: String; const D: Char): String; overload;

{$IFDEF SupportAnsiString}
function  StrCopyToCharA(const S: AnsiString; const D: ByteCharSet;
          const Optional: Boolean = True): AnsiString; overload;
function  StrCopyToCharA(const S: AnsiString; const D: AnsiChar;
          const Optional: Boolean = True): AnsiString; overload;
{$ENDIF}

function  StrCopyToCharB(const S: RawByteString; const D: ByteCharSet;
          const Optional: Boolean = True): RawByteString; overload;
function  StrCopyToCharB(const S: RawByteString; const D: ByteChar;
          const Optional: Boolean = True): RawByteString; overload;

function  StrCopyToCharU(const S: UnicodeString; const D: ByteCharSet;
          const Optional: Boolean = True): UnicodeString; overload;
function  StrCopyToCharU(const S: UnicodeString; const D: WideChar;
          const Optional: Boolean = True): UnicodeString; overload;

function  StrCopyToChar(const S: String; const D: ByteCharSet;
          const Optional: Boolean = True): String; overload;
function  StrCopyToChar(const S: String; const D: Char;
          const Optional: Boolean = True): String; overload;

{$IFDEF SupportAnsiString}
function  StrCopyFromCharA(const S: AnsiString; const D: ByteCharSet): AnsiString; overload;
function  StrCopyFromCharA(const S: AnsiString; const D: AnsiChar): AnsiString; overload;
{$ENDIF}

function  StrCopyFromCharB(const S: RawByteString; const D: ByteCharSet): RawByteString; overload;
function  StrCopyFromCharB(const S: RawByteString; const D: ByteChar): RawByteString; overload;

function  StrCopyFromCharU(const S: UnicodeString; const D: ByteCharSet): UnicodeString; overload;
function  StrCopyFromCharU(const S: UnicodeString; const D: WideChar): UnicodeString; overload;

function  StrCopyFromChar(const S: String; const D: ByteCharSet): String; overload;
function  StrCopyFromChar(const S: String; const D: Char): String; overload;

{$IFDEF SupportAnsiString}
function  StrRemoveCharDelimitedA(var S: AnsiString;
          const FirstDelim, SecondDelim: AnsiChar): AnsiString;
{$ENDIF}
function  StrRemoveCharDelimitedB(var S: RawByteString;
          const FirstDelim, SecondDelim: ByteChar): RawByteString;

function  StrRemoveCharDelimitedU(var S: UnicodeString;
          const FirstDelim, SecondDelim: WideChar): UnicodeString;
function  StrRemoveCharDelimited(var S: String;
          const FirstDelim, SecondDelim: Char): String;



{                                                                              }
{ Count                                                                        }
{                                                                              }
{$IFDEF SupportAnsiString}
function  StrCountCharA(const S: AnsiString; const C: AnsiChar): Integer; overload;
{$ENDIF}
function  StrCountCharB(const S: RawByteString; const C: ByteChar): Integer; overload;
function  StrCountCharU(const S: UnicodeString; const C: WideChar): Integer; overload;
function  StrCountChar(const S: String; const C: Char): Integer; overload;

{$IFDEF SupportAnsiString}
function  StrCountCharA(const S: AnsiString; const C: ByteCharSet): Integer; overload;
{$ENDIF}
function  StrCountCharB(const S: RawByteString; const C: ByteCharSet): Integer; overload;
function  StrCountCharU(const S: UnicodeString; const C: ByteCharSet): Integer; overload;
function  StrCountChar(const S: String; const C: ByteCharSet): Integer; overload;



{                                                                              }
{ Replace                                                                      }
{                                                                              }
{$IFDEF SupportAnsiString}
function  StrReplaceCharA(const Find, Replace: AnsiChar; const S: AnsiString): AnsiString; overload;
{$ENDIF}
function  StrReplaceCharB(const Find, Replace: ByteChar; const S: RawByteString): RawByteString; overload;
function  StrReplaceCharU(const Find, Replace: WideChar; const S: UnicodeString): UnicodeString; overload;
function  StrReplaceChar(const Find, Replace: Char; const S: String): String; overload;

{$IFDEF SupportAnsiString}
function  StrReplaceCharA(const Find: ByteCharSet; const Replace: AnsiChar; const S: AnsiString): AnsiString; overload;
{$ENDIF}
function  StrReplaceCharB(const Find: ByteCharSet; const Replace: ByteChar; const S: RawByteString): RawByteString; overload;
function  StrReplaceCharU(const Find: ByteCharSet; const Replace: WideChar; const S: UnicodeString): UnicodeString; overload;
function  StrReplaceChar(const Find: ByteCharSet; const Replace: Char; const S: String): String; overload;

{$IFDEF SupportAnsiString}
function  StrReplaceA(const Find, Replace, S: AnsiString; const AsciiCaseSensitive: Boolean = True): AnsiString; overload;
{$ENDIF}
function  StrReplaceB(const Find, Replace, S: RawByteString; const AsciiCaseSensitive: Boolean = True): RawByteString; overload;
function  StrReplaceU(const Find, Replace, S: UnicodeString; const AsciiCaseSensitive: Boolean = True): UnicodeString; overload;
function  StrReplace(const Find, Replace, S: String; const AsciiCaseSensitive: Boolean = True): String; overload;

{$IFDEF SupportAnsiString}
function  StrReplaceA(const Find: ByteCharSet; const Replace, S: AnsiString): AnsiString; overload;
{$ENDIF}
function  StrReplaceB(const Find: ByteCharSet; const Replace, S: RawByteString): RawByteString; overload;
function  StrReplaceU(const Find: ByteCharSet; const Replace, S: UnicodeString): UnicodeString; overload;
function  StrReplace(const Find: ByteCharSet; const Replace, S: String): String; overload;

{$IFDEF SupportAnsiString}
function  StrReplaceCharStrA(const Find: AnsiChar; const Replace, S: AnsiString): AnsiString;
{$ENDIF}
function  StrReplaceCharStrU(const Find: WideChar; const Replace, S: UnicodeString): UnicodeString;

{$IFDEF SupportAnsiString}
function  StrRemoveDupA(const S: AnsiString; const C: AnsiChar): AnsiString;
{$ENDIF}
function  StrRemoveDupU(const S: UnicodeString; const C: WideChar): UnicodeString;
function  StrRemoveDup(const S: String; const C: Char): String;

{$IFDEF SupportAnsiString}
function  StrRemoveCharA(const S: AnsiString; const C: AnsiChar): AnsiString;
{$ENDIF}
function  StrRemoveCharU(const S: UnicodeString; const C: WideChar): UnicodeString;
function  StrRemoveChar(const S: String; const C: Char): String;

{$IFDEF SupportAnsiString}
function  StrRemoveCharSetA(const S: AnsiString; const C: ByteCharSet): AnsiString;
{$ENDIF}
function  StrRemoveCharSetB(const S: RawByteString; const C: ByteCharSet): RawByteString;
function  StrRemoveCharSetU(const S: UnicodeString; const C: ByteCharSet): UnicodeString;
function  StrRemoveCharSet(const S: String; const C: ByteCharSet): String;



{                                                                              }
{ Split                                                                        }
{                                                                              }
{$IFDEF SupportAnsiString}
function  StrSplitAtA(const S: AnsiString; const C: AnsiString;
          var Left, Right: AnsiString;
          const AsciiCaseSensitive: Boolean = True;
          const Optional: Boolean = True): Boolean;
{$ENDIF}
function  StrSplitAtB(const S: RawByteString; const C: RawByteString;
          var Left, Right: RawByteString;
          const AsciiCaseSensitive: Boolean = True;
          const Optional: Boolean = True): Boolean;
function  StrSplitAtU(const S: UnicodeString; const C: UnicodeString;
          var Left, Right: UnicodeString;
          const AsciiCaseSensitive: Boolean = True;
          const Optional: Boolean = True): Boolean;
function  StrSplitAt(const S: String; const C: String;
          var Left, Right: String;
          const AsciiCaseSensitive: Boolean = True;
          const Optional: Boolean = True): Boolean;

{$IFDEF SupportAnsiString}
function  StrSplitAtCharA(const S: AnsiString; const C: AnsiChar;
          var Left, Right: AnsiString;
          const Optional: Boolean = True): Boolean;
{$ENDIF}
function  StrSplitAtCharB(const S: RawByteString; const C: ByteChar;
          var Left, Right: RawByteString;
          const Optional: Boolean = True): Boolean;
function  StrSplitAtCharU(const S: UnicodeString; const C: WideChar;
          var Left, Right: UnicodeString;
          const Optional: Boolean = True): Boolean;
function  StrSplitAtChar(const S: String; const C: Char;
          var Left, Right: String;
          const Optional: Boolean = True): Boolean;

{$IFDEF SupportAnsiString}
function  StrSplitAtCharSetA(const S: AnsiString; const C: ByteCharSet;
          var Left, Right: AnsiString;
          const Optional: Boolean = True): Boolean;
{$ENDIF}
function  StrSplitAtCharSetB(const S: RawByteString; const C: ByteCharSet;
          var Left, Right: RawByteString;
          const Optional: Boolean = True): Boolean;

{$IFDEF SupportAnsiString}
function  StrSplitA(const S, D: AnsiString): AnsiStringArray;
{$ENDIF}
function  StrSplitB(const S, D: RawByteString): RawByteStringArray;
function  StrSplitU(const S, D: UnicodeString): UnicodeStringArray;
function  StrSplit(const S, D: String): StringArray;

{$IFDEF SupportAnsiString}
function  StrSplitCharA(const S: AnsiString; const D: AnsiChar): AnsiStringArray;
{$ENDIF}
function  StrSplitCharB(const S: RawByteString; const D: ByteChar): RawByteStringArray;
function  StrSplitCharU(const S: UnicodeString; const D: WideChar): UnicodeStringArray;
function  StrSplitChar(const S: String; const D: Char): StringArray;

{$IFDEF SupportAnsiString}
function  StrSplitCharSetA(const S: AnsiString; const D: ByteCharSet): AnsiStringArray;
{$ENDIF}
function  StrSplitCharSetB(const S: RawByteString; const D: ByteCharSet): RawByteStringArray;
function  StrSplitCharSetU(const S: UnicodeString; const D: ByteCharSet): UnicodeStringArray;
function  StrSplitCharSet(const S: String; const D: ByteCharSet): StringArray;

{$IFDEF SupportAnsiString}
function  StrSplitWords(const S: AnsiString; const C: ByteCharSet): AnsiStringArray;
{$ENDIF}

{$IFDEF SupportAnsiString}
function  StrJoinA(const S: array of AnsiString; const D: AnsiString): AnsiString;
{$ENDIF}
function  StrJoinB(const S: array of RawByteString; const D: RawByteString): RawByteString;
function  StrJoinU(const S: array of UnicodeString; const D: UnicodeString): UnicodeString;
function  StrJoin(const S: array of String; const D: String): String;

{$IFDEF SupportAnsiString}
function  StrJoinCharA(const S: array of AnsiString; const D: AnsiChar): AnsiString;
{$ENDIF}
function  StrJoinCharB(const S: array of RawByteString; const D: ByteChar): RawByteString;
function  StrJoinCharU(const S: array of UnicodeString; const D: WideChar): UnicodeString;
function  StrJoinChar(const S: array of String; const D: Char): String;



{                                                                              }
{ Quoting                                                                      }
{                                                                              }
{   QuoteText, UnquoteText converts text where the string is enclosed in a     }
{   pair of the same quote characters, and two consequetive occurance of the   }
{   quote character inside the quotes indicate a quote character in the text.  }
{   Examples:                                                                  }
{                                                                              }
{     StrQuote ('abc', '"') = '"abc"'                                          }
{     StrQuote ('a"b"c', '"') = '"a""b""c"'                                    }
{     StrUnquote ('"a""b""c"') = 'a"b"c'                                       }
{                                                                              }
{$IFDEF SupportAnsiString}
function  StrHasSurroundingQuotesA(const S: AnsiString;
          const Quotes: ByteCharSet = csQuotes): Boolean;
function  StrHasSurroundingQuotesB(const S: RawByteString;
          const Quotes: ByteCharSet = csQuotes): Boolean;
{$ENDIF}
function  StrHasSurroundingQuotesU(const S: UnicodeString;
          const Quotes: ByteCharSet = csQuotes): Boolean;
function  StrHasSurroundingQuotes(const S: String;
          const Quotes: ByteCharSet = csQuotes): Boolean;

{$IFDEF SupportAnsiString}
function  StrRemoveSurroundingQuotesA(const S: AnsiString;
          const Quotes: ByteCharSet = csQuotes): AnsiString;
function  StrRemoveSurroundingQuotesB(const S: RawByteString;
          const Quotes: ByteCharSet = csQuotes): RawByteString;
{$ENDIF}
function  StrRemoveSurroundingQuotesU(const S: UnicodeString;
          const Quotes: ByteCharSet = csQuotes): UnicodeString;
function  StrRemoveSurroundingQuotes(const S: String;
          const Quotes: ByteCharSet = csQuotes): String;

{$IFDEF SupportAnsiString}
function  StrQuoteA(const S: AnsiString; const Quote: AnsiChar = AnsiChar('"')): AnsiString;
{$ENDIF}
function  StrQuoteB(const S: RawByteString; const Quote: ByteChar = ByteChar('"')): RawByteString;
function  StrQuoteU(const S: UnicodeString; const Quote: WideChar = WideChar('"')): UnicodeString;
function  StrQuote(const S: String; const Quote: Char = '"'): String;

{$IFDEF SupportAnsiString}
function  StrUnquoteA(const S: AnsiString): AnsiString;
{$ENDIF}
function  StrUnquoteB(const S: RawByteString): RawByteString;
function  StrUnquoteU(const S: UnicodeString): UnicodeString;
function  StrUnquote(const S: String): String;

{$IFDEF SupportAnsiString}
function  StrMatchQuotedStrA(const S: AnsiString;
          const ValidQuotes: ByteCharSet = csQuotes;
          const Index: Integer = 1): Integer;

function  StrIsQuotedStrA(const S: AnsiString;
          const ValidQuotes: ByteCharSet = csQuotes): Boolean;

function  StrFindClosingQuoteA(const S: AnsiString;
          const OpenQuotePos: Integer): Integer;
{$ENDIF}



{                                                                              }
{ Bracketing                                                                   }
{                                                                              }
function  StrFindClosingBracketB(const S: RawByteString;
          const OpenBracketPos: Integer; const CloseBracket: ByteChar): Integer;



{                                                                              }
{ Escaping                                                                     }
{                                                                              }
function  StrHexEscapeB(const S: RawByteString; const C: ByteCharSet;
          const EscPrefix: RawByteString = '\x';
          const EscSuffix: RawByteString = '';
          const UpperHex: Boolean = True;
          const TwoDigitHex: Boolean = True): RawByteString;

function  StrHexUnescapeB(const S: RawByteString;
          const EscPrefix: RawByteString = '\x';
          const AsciiCaseSensitive: Boolean = True): RawByteString;

{$IFDEF SupportAnsiString}
function  StrCharEscapeA(const S: AnsiString; const C: array of AnsiChar;
          const EscPrefix: AnsiString;
          const EscSeq: array of AnsiString): AnsiString;

function  StrCharUnescapeA(const S: AnsiString; const EscPrefix: AnsiString;
          const C: array of AnsiChar; const Replace: array of AnsiString;
          const PrefixAsciiCaseSensitive: Boolean = True;
          const AlwaysDropPrefix: Boolean = True): AnsiString;

function  StrCharUnescapeU(const S: UnicodeString; const EscPrefix: UnicodeString;
          const C: array of WideChar; const Replace: array of UnicodeString;
          const PrefixAsciiCaseSensitive: Boolean = True;
          const AlwaysDropPrefix: Boolean = True): UnicodeString;

function  StrCStyleEscapeA(const S: AnsiString): AnsiString;
function  StrCStyleUnescapeA(const S: AnsiString): AnsiString;
{$ENDIF}



{                                                                              }
{ Prefix and Suffix                                                            }
{                                                                              }
{$IFDEF SupportAnsiString}
function  StrInclPrefixA(const S: AnsiString; const Prefix: AnsiString; const AsciiCaseSensitive: Boolean = True): AnsiString;
{$ENDIF}
function  StrInclPrefixB(const S: RawByteString; const Prefix: RawByteString; const AsciiCaseSensitive: Boolean = True): RawByteString;
function  StrInclPrefixU(const S: UnicodeString; const Prefix: UnicodeString; const AsciiCaseSensitive: Boolean = True): UnicodeString;
function  StrInclPrefix(const S: String; const Prefix: String; const AsciiCaseSensitive: Boolean = True): String;

{$IFDEF SupportAnsiString}
function  StrInclSuffixA(const S: AnsiString; const Suffix: AnsiString; const AsciiCaseSensitive: Boolean = True): AnsiString;
{$ENDIF}
function  StrInclSuffixB(const S: RawByteString; const Suffix: RawByteString; const AsciiCaseSensitive: Boolean = True): RawByteString;
function  StrInclSuffixU(const S: UnicodeString; const Suffix: UnicodeString; const AsciiCaseSensitive: Boolean = True): UnicodeString;
function  StrInclSuffix(const S: String; const Suffix: String; const AsciiCaseSensitive: Boolean = True): String;

{$IFDEF SupportAnsiString}
function  StrExclPrefixA(const S: AnsiString; const Prefix: AnsiString; const AsciiCaseSensitive: Boolean = True): AnsiString;
{$ENDIF}
function  StrExclPrefixB(const S: RawByteString; const Prefix: RawByteString; const AsciiCaseSensitive: Boolean = True): RawByteString;
function  StrExclPrefixU(const S: UnicodeString; const Prefix: UnicodeString; const AsciiCaseSensitive: Boolean = True): UnicodeString;
function  StrExclPrefix(const S: String; const Prefix: String; const AsciiCaseSensitive: Boolean = True): String;

{$IFDEF SupportAnsiString}
function  StrExclSuffixA(const S: AnsiString; const Suffix: AnsiString; const AsciiCaseSensitive: Boolean = True): AnsiString;
{$ENDIF}
function  StrExclSuffixB(const S: RawByteString; const Suffix: RawByteString; const AsciiCaseSensitive: Boolean = True): RawByteString;
function  StrExclSuffixU(const S: UnicodeString; const Suffix: UnicodeString; const AsciiCaseSensitive: Boolean = True): UnicodeString;
function  StrExclSuffix(const S: String; const Suffix: String; const AsciiCaseSensitive: Boolean = True): String;

{$IFDEF SupportAnsiString}
procedure StrEnsurePrefixA(var S: AnsiString; const Prefix: AnsiString; const AsciiCaseSensitive: Boolean = True);
{$ENDIF}
procedure StrEnsurePrefixB(var S: RawByteString; const Prefix: RawByteString; const AsciiCaseSensitive: Boolean = True);
procedure StrEnsurePrefixU(var S: UnicodeString; const Prefix: UnicodeString; const AsciiCaseSensitive: Boolean = True);
procedure StrEnsurePrefix(var S: String; const Prefix: String; const AsciiCaseSensitive: Boolean = True);

{$IFDEF SupportAnsiString}
procedure StrEnsureSuffixA(var S: AnsiString; const Suffix: AnsiString; const AsciiCaseSensitive: Boolean = True);
{$ENDIF}
procedure StrEnsureSuffixB(var S: RawByteString; const Suffix: RawByteString; const AsciiCaseSensitive: Boolean = True);
procedure StrEnsureSuffixU(var S: UnicodeString; const Suffix: UnicodeString; const AsciiCaseSensitive: Boolean = True);
procedure StrEnsureSuffix(var S: String; const Suffix: String; const AsciiCaseSensitive: Boolean = True);

{$IFDEF SupportAnsiString}
procedure StrEnsureNoPrefixA(var S: AnsiString; const Prefix: AnsiString; const AsciiCaseSensitive: Boolean = True);
{$ENDIF}
procedure StrEnsureNoPrefixB(var S: RawByteString; const Prefix: RawByteString; const AsciiCaseSensitive: Boolean = True);
procedure StrEnsureNoPrefixU(var S: UnicodeString; const Prefix: UnicodeString; const AsciiCaseSensitive: Boolean = True);
procedure StrEnsureNoPrefix(var S: String; const Prefix: String; const AsciiCaseSensitive: Boolean = True);

{$IFDEF SupportAnsiString}
procedure StrEnsureNoSuffixA(var S: AnsiString; const Suffix: AnsiString; const AsciiCaseSensitive: Boolean = True);
{$ENDIF}
procedure StrEnsureNoSuffixB(var S: RawByteString; const Suffix: RawByteString; const AsciiCaseSensitive: Boolean = True);
procedure StrEnsureNoSuffixU(var S: UnicodeString; const Suffix: UnicodeString; const AsciiCaseSensitive: Boolean = True);
procedure StrEnsureNoSuffix(var S: String; const Suffix: String; const AsciiCaseSensitive: Boolean = True);



{                                                                              }
{ Reverse                                                                      }
{                                                                              }
{$IFDEF SupportAnsiString}
function  StrReverseA(const S: AnsiString): AnsiString;
{$ENDIF}
function  StrReverseB(const S: RawByteString): RawByteString;
function  StrReverseU(const S: UnicodeString): UnicodeString;
function  StrReverse(const S: String): String;



{                                                                              }
{ Type conversion                                                              }
{                                                                              }
function  StrToFloatDef(const S: String; const Default: Extended): Extended;

{$IFDEF SupportAnsiString}
function  BooleanToStrA(const B: Boolean): AnsiString;
{$ENDIF}
function  BooleanToStrB(const B: Boolean): RawByteString;
function  BooleanToStrU(const B: Boolean): UnicodeString;
function  BooleanToStr(const B: Boolean): String;

{$IFDEF SupportAnsiString}
function  StrToBooleanA(const S: AnsiString): Boolean;
{$ENDIF}
function  StrToBooleanB(const S: RawByteString): Boolean;
function  StrToBooleanU(const S: UnicodeString): Boolean;
function  StrToBoolean(const S: String): Boolean;



{                                                                              }
{ Dynamic array functions                                                      }
{                                                                              }
{$IFDEF SupportAnsiString}
function  StringsTotalLengthA(const S: array of AnsiString): Integer;
{$ENDIF}
function  StringsTotalLengthB(const S: array of RawByteString): Integer;
function  StringsTotalLengthU(const S: array of UnicodeString): Integer;
function  StringsTotalLength(const S: array of String): Integer;

{$IFDEF SupportAnsiString}
function  PosNextNoCaseA(const Find: AnsiString; const V: array of AnsiString;
          const PrevPos: Integer = -1;
          const IsSortedAscending: Boolean = False): Integer;
{$ENDIF}
function  PosNextNoCaseB(const Find: RawByteString; const V: array of RawByteString;
          const PrevPos: Integer = -1;
          const IsSortedAscending: Boolean = False): Integer;
function  PosNextNoCaseU(const Find: UnicodeString; const V: array of UnicodeString;
          const PrevPos: Integer = -1;
          const IsSortedAscending: Boolean = False): Integer;



{                                                                              }
{ Natural language                                                             }
{                                                                              }
function  StorageSize(const Bytes: Int64;
          const ShortFormat: Boolean = False): String;
function  TransferRate(const Bytes, MillisecondsElapsed: Int64;
          const ShortFormat: Boolean = False): String;



{                                                                              }
{ Tests                                                                        }
{                                                                              }
{$IFDEF STRINGS_TEST}
procedure Test;
{$ENDIF}



implementation

uses
  { Fundamentals }
  flcUtils,
  flcASCII;



{                                                                              }
{ String functions                                                             }
{                                                                              }
{$IFDEF SupportAnsiString}
procedure SetLengthAndZeroA(var S: AnsiString; const NewLength: Integer);
var L : Integer;
    P : PAnsiChar;
begin
  L := Length(S);
  if L = NewLength then
    exit;
  SetLength(S, NewLength);
  if L > NewLength then
    exit;
  P := Pointer(S);
  Inc(P, L);
  ZeroMem(P^, NewLength - L);
end;
{$ENDIF}

procedure SetLengthAndZeroB(var S: RawByteString; const NewLength: Integer);
var L : Integer;
    P : PByteChar;
begin
  L := Length(S);
  if L = NewLength then
    exit;
  SetLength(S, NewLength);
  if L > NewLength then
    exit;
  P := Pointer(S);
  Inc(P, L);
  ZeroMem(P^, NewLength - L);
end;

procedure SetLengthAndZeroU(var S: UnicodeString; const NewLength: Integer);
var L : Integer;
    P : PWideChar;
begin
  L := Length(S);
  if L = NewLength then
    exit;
  SetLength(S, NewLength);
  if L > NewLength then
    exit;
  P := Pointer(S);
  Inc(P, L);
  ZeroMem(P^, (NewLength - L) * SizeOf(WideChar));
end;

function ToStringChB(const A: ByteChar): String;
begin
  {$IFDEF StringIsUnicode}
  Result := WideChar(A);
  {$ELSE}
  Result := A;
  {$ENDIF}
end;

function ToStringChW(const A: WideChar): String;
begin
  {$IFDEF StringIsUnicode}
  Result := A;
  {$ELSE}
  if Ord(A) <= $FF then
    Result := AnsiChar(Ord(A))
  else
    raise EConvertError.Create('Invalid character');
  {$ENDIF}
end;

{$IFDEF SupportAnsiString}
function ToStringA(const A: AnsiString): String;
begin
  {$IFDEF StringIsUnicode}
  Result := String(A);
  {$ELSE}
  Result := A;
  {$ENDIF}
end;
{$ENDIF}

function ToStringB(const A: RawByteString): String;
begin
  {$IFDEF StringIsUnicode}
  Result := String(A);
  {$ELSE}
  Result := A;
  {$ENDIF}
end;

function ToStringU(const A: UnicodeString): String;
begin
  {$IFDEF StringIsUnicode}
  Result := String(A);
  {$ELSE}
  Result := AnsiString(A);
  {$ENDIF}
end;



{                                                                              }
{ Match                                                                        }
{                                                                              }
{$IFDEF ASM386_DELPHI}
function CharMatchNoAsciiCaseB(const A, B: ByteChar): Boolean;
asm
      AND     EAX, $000000FF
      AND     EDX, $000000FF
      MOV     AL, BYTE PTR [AsciiLowCaseLookup + EAX]
      CMP     AL, BYTE PTR [AsciiLowCaseLookup + EDX]
      SETZ    AL
end;
{$ELSE}
function CharMatchNoAsciiCaseB(const A, B: ByteChar): Boolean;
begin
  Result := AsciiLowCaseLookup[Ord(A)] = AsciiLowCaseLookup[Ord(B)];
end;
{$ENDIF}

function CharMatchNoAsciiCaseW(const A, B: WideChar): Boolean;
begin
  if (Ord(A) <= $7F) and (Ord(B) <= $7F) then
    Result := AsciiLowCaseLookup[Ord(A)] = AsciiLowCaseLookup[Ord(B)]
  else
    Result := Ord(A) = Ord(B);
end;

function CharMatchNoAsciiCaseBW(const A: ByteChar; const B: WideChar): Boolean;
begin
  if (Ord(A) <= $7F) and (Ord(B) <= $7F) then
    Result := AsciiLowCaseLookup[Ord(A)] = AsciiLowCaseLookup[Ord(B)]
  else
    Result := Ord(A) = Ord(B);
end;

function CharMatchNoAsciiCaseBS(const A: ByteChar; const B: Char): Boolean;
begin
  if (Ord(A) <= $7F) and (Ord(B) <= $7F) then
    Result := AsciiLowCaseLookup[Ord(A)] = AsciiLowCaseLookup[Ord(B)]
  else
    Result := Ord(A) = Ord(B);
end;

function CharMatchNoAsciiCase(const A, B: Char): Boolean;
begin
  if (Ord(A) <= $7F) and (Ord(B) <= $7F) then
    Result := AsciiLowCaseLookup[Ord(A)] = AsciiLowCaseLookup[Ord(B)]
  else
    Result := Ord(A) = Ord(B);
end;

{$IFDEF ASM386_DELPHI}
function CharMatchB(const A, B: ByteChar; const AsciiCaseSensitive: Boolean): Boolean;
asm
      OR      CL, CL
      JZ      CharMatchNoAsciiCaseB
      CMP     AL, DL
      SETZ    AL
end;
{$ELSE}
function CharMatchB(const A, B: ByteChar; const AsciiCaseSensitive: Boolean): Boolean;
begin
  if AsciiCaseSensitive then
    Result := A = B
  else
    Result := AsciiLowCaseLookup[Ord(A)] = AsciiLowCaseLookup[Ord(B)];
end;
{$ENDIF}

function CharMatchW(const A, B: WideChar; const AsciiCaseSensitive: Boolean = True): Boolean;
begin
  if AsciiCaseSensitive then
    Result := A = B
  else
    if (Ord(A) <= $7F) and (Ord(B) <= $7F) then
      Result := AsciiLowCaseLookup[Ord(A)] = AsciiLowCaseLookup[Ord(B)]
    else
      Result := A = B;
end;

function CharMatchBW(const A: ByteChar; const B: WideChar; const AsciiCaseSensitive: Boolean = True): Boolean;
begin
  if AsciiCaseSensitive then
    Result := Ord(A) = Ord(B)
  else
    if (Ord(A) <= $7F) and (Ord(B) <= $7F) then
      Result := AsciiLowCaseLookup[Ord(A)] = AsciiLowCaseLookup[Ord(B)]
    else
      Result := Ord(A) = Ord(B);
end;

function CharMatchBS(const A: ByteChar; const B: Char; const AsciiCaseSensitive: Boolean = True): Boolean;
begin
  if AsciiCaseSensitive then
    Result := Ord(A) = Ord(B)
  else
    if (Ord(A) <= $7F) and (Ord(B) <= $7F) then
      Result := AsciiLowCaseLookup[Ord(A)] = AsciiLowCaseLookup[Ord(B)]
    else
      Result := Ord(A) = Ord(B);
end;

function CharMatch(const A, B: Char; const AsciiCaseSensitive: Boolean = True): Boolean;
begin
  if AsciiCaseSensitive then
    Result := A = B
  else
    if (Ord(A) <= $7F) and (Ord(B) <= $7F) then
      Result := AsciiLowCaseLookup[Ord(A)] = AsciiLowCaseLookup[Ord(B)]
    else
      Result := A = B;
end;

function CharSetMatchCharB(const A: ByteCharSet; const B: ByteChar; const AsciiCaseSensitive: Boolean): Boolean;
begin
  if AsciiCaseSensitive then
    Result := B in A
  else
    Result := (AsciiUpCaseB(B) in A) or (AsciiLowCaseB(B) in A);
end;

function CharSetMatchCharW(const A: ByteCharSet; const B: WideChar; const AsciiCaseSensitive: Boolean): Boolean;
begin
  if Ord(B) > $FF then
    Result := False
  else
    if AsciiCaseSensitive then
      Result := ByteChar(Ord(B)) in A
    else
      Result := (AsciiUpCaseB(ByteChar(Ord(B))) in A) or
                (AsciiLowCaseB(ByteChar(Ord(B))) in A);
end;

function CharSetMatchChar(const A: ByteCharSet; const B: Char; const AsciiCaseSensitive: Boolean): Boolean;
begin
  {$IFDEF StringIsUnicode}
  if Ord(B) > $FF then
    Result := False
  else
  {$ENDIF}
    if AsciiCaseSensitive then
      Result := ByteChar(Ord(B)) in A
    else
      Result := (AsciiUpCaseB(ByteChar(Ord(B))) in A) or
                (AsciiLowCaseB(ByteChar(Ord(B))) in A);
end;

function StrPMatchB(const A, B: PByteChar; const Len: Integer): Boolean;
var P, Q : PByteChar;
    I    : Integer;
begin
  P := A;
  Q := B;
  if P <> Q then
    for I := 1 to Len do
      if P^ = Q^ then
        begin
          Inc(P);
          Inc(Q);
        end else
        begin
          Result := False;
          exit;
        end;
  Result := True;
end;

function StrPMatchW(const A, B: PWideChar; const Len: Integer): Boolean;
var P, Q : PWideChar;
    I    : Integer;
begin
  P := A;
  Q := B;
  if P <> Q then
    for I := 1 to Len do
      if P^ = Q^ then
        begin
          Inc(P);
          Inc(Q);
        end else
        begin
          Result := False;
          exit;
        end;
  Result := True;
end;

function StrPMatchBW(const A: PWideChar; B: PByteChar; const Len: Integer): Boolean;
var P : PWideChar;
    Q : PByteChar;
    I : Integer;
begin
  P := A;
  Q := B;
  for I := 1 to Len do
    if Ord(P^) = Ord(Q^) then
      begin
        Inc(P);
        Inc(Q);
      end else
      begin
        Result := False;
        exit;
      end;
  Result := True;
end;

function StrPMatchBS(const A: PChar; B: PByteChar; const Len: Integer): Boolean;
var P : PChar;
    Q : PByteChar;
    I : Integer;
begin
  P := A;
  Q := B;
  for I := 1 to Len do
    if Ord(P^) = Ord(Q^) then
      begin
        Inc(P);
        Inc(Q);
      end else
      begin
        Result := False;
        exit;
      end;
  Result := True;
end;

function StrPMatch(const A, B: PChar; const Len: Integer): Boolean;
var P, Q : PChar;
    I    : Integer;
begin
  P := A;
  Q := B;
  if P <> Q then
    for I := 1 to Len do
      if P^ = Q^ then
        begin
          Inc(P);
          Inc(Q);
        end else
        begin
          Result := False;
          exit;
        end;
  Result := True;
end;

function StrPMatchB(const S, M: PByteChar; const LenS, LenM: Integer): Boolean;
var P, Q : PByteChar;
    I    : Integer;
begin
  if LenM = 0 then
    begin
      Result := True;
      exit;
    end;
  if LenM > LenS then
    begin
      Result := False;
      exit;
    end;
  if Pointer(S) = Pointer(M) then
    begin
      Result := True;
      exit;
    end;
  P := S;
  Q := M;
  for I := 1 to LenM do
    if P^ = Q^ then
      begin
        Inc(P);
        Inc(Q);
      end else
      begin
        Result := False;
        exit;
      end;
  Result := True;
end;

function StrPMatchW(const S, M: PWideChar; const LenS, LenM: Integer): Boolean;
var P, Q : PWideChar;
    I    : Integer;
begin
  if LenM = 0 then
    begin
      Result := True;
      exit;
    end;
  if LenM > LenS then
    begin
      Result := False;
      exit;
    end;
  if Pointer(S) = Pointer(M) then
    begin
      Result := True;
      exit;
    end;
  P := S;
  Q := M;
  for I := 1 to LenM do
    if P^ = Q^ then
      begin
        Inc(P);
        Inc(Q);
      end else
      begin
        Result := False;
        exit;
      end;
  Result := True;
end;

function StrPMatchBW(const S: PWideChar; const M: PByteChar; const LenS, LenM: Integer): Boolean;
var P : PWideChar;
    Q : PByteChar;
    I : Integer;
begin
  if LenM = 0 then
    begin
      Result := True;
      exit;
    end;
  if LenM > LenS then
    begin
      Result := False;
      exit;
    end;
  P := S;
  Q := M;
  for I := 1 to LenM do
    if Ord(P^) = Ord(Q^) then
      begin
        Inc(P);
        Inc(Q);
      end else
      begin
        Result := False;
        exit;
      end;
  Result := True;
end;

function StrPMatchBS(const S: PChar; const M: PByteChar; const LenS, LenM: Integer): Boolean;
var P : PChar;
    Q : PByteChar;
    I : Integer;
begin
  if LenM = 0 then
    begin
      Result := True;
      exit;
    end;
  if LenM > LenS then
    begin
      Result := False;
      exit;
    end;
  P := S;
  Q := M;
  for I := 1 to LenM do
    if Ord(P^) = Ord(Q^) then
      begin
        Inc(P);
        Inc(Q);
      end else
      begin
        Result := False;
        exit;
      end;
  Result := True;
end;

function StrPMatch(const S, M: PChar; const LenS, LenM: Integer): Boolean;
var P, Q : PChar;
    I    : Integer;
begin
  if LenM = 0 then
    begin
      Result := True;
      exit;
    end;
  if LenM > LenS then
    begin
      Result := False;
      exit;
    end;
  if Pointer(S) = Pointer(M) then
    begin
      Result := True;
      exit;
    end;
  P := S;
  Q := M;
  for I := 1 to LenM do
    if P^ = Q^ then
      begin
        Inc(P);
        Inc(Q);
      end else
      begin
        Result := False;
        exit;
      end;
  Result := True;
end;

function StrPMatchStrPB(const S: PChar; const M: PByteChar; const LenS, LenM: Integer): Boolean;
var P : PChar;
    Q : PByteChar;
    I : Integer;
begin
  if LenM = 0 then
    begin
      Result := True;
      exit;
    end;
  if LenM > LenS then
    begin
      Result := False;
      exit;
    end;
  P := S;
  Q := M;
  for I := 1 to LenM do
    if Ord(P^) = Ord(Q^) then
      begin
        Inc(P);
        Inc(Q);
      end else
      begin
        Result := False;
        exit;
      end;
  Result := True;
end;

{$IFDEF SupportAnsiString}
function StrPMatchStrA(const S: PAnsiChar; const Len: Integer; const M: AnsiString): Boolean;
begin
  Result := StrPMatchB(Pointer(S), Pointer(M), Len, Length(M));
end;
{$ENDIF}

function StrPMatchStrB(const S: PByteChar; const Len: Integer; const M: RawByteString): Boolean;
begin
  Result := StrPMatchB(S, Pointer(M), Len, Length(M));
end;

{$IFDEF SupportAnsiString}
function StrPMatchStrAW(const S: PWideChar; const Len: Integer; const M: AnsiString): Boolean;
begin
  Result := StrPMatchBW(S, Pointer(M), Len, Length(M));
end;
{$ENDIF}

{$IFDEF SupportAnsiString}
function StrPMatchStrAS(const S: PChar; const Len: Integer; const M: AnsiString): Boolean;
begin
  Result := StrPMatchBS(S, Pointer(M), Len, Length(M));
end;
{$ENDIF}

function StrPMatchStrU(const S: PWideChar; const Len: Integer; const M: UnicodeString): Boolean;
begin
  Result := StrPMatchW(S, Pointer(M), Len, Length(M));
end;

function StrPMatchStr(const S: PChar; const Len: Integer; const M: String): Boolean;
begin
  Result := StrPMatch(S, Pointer(M), Len, Length(M));
end;

function StrPMatchNoAsciiCaseB(const A, B: PByteChar; const Len: Integer): Boolean;
var P, Q : PByte;
    C, D : Byte;
    I    : Integer;
begin
  P := Pointer(A);
  Q := Pointer(B);
  if P <> Q then
    for I := 1 to Len do
      begin
        C := AsciiLowCaseLookup[P^];
        D := AsciiLowCaseLookup[Q^];
        if C = D then
          begin
            Inc(P);
            Inc(Q);
          end
        else
          begin
            Result := False;
            exit;
          end;
      end;
  Result := True;
end;

function StrPMatchNoAsciiCaseW(const A, B: PWideChar; const Len: Integer): Boolean;
var P, Q : PWideChar;
    I    : Integer;
begin
  P := A;
  Q := B;
  if P <> Q then
    for I := 1 to Len do
      begin
        if CharMatchNoAsciiCaseW(P^, Q^) then
          begin
            Inc(P);
            Inc(Q);
          end
        else
          begin
            Result := False;
            exit;
          end;
      end;
  Result := True;
end;

function StrPMatchNoAsciiCaseBW(const A: PWideChar; const B: PByteChar; const Len: Integer): Boolean;
var P : PWideChar;
    Q : PByteChar;
    I : Integer;
begin
  P := A;
  Q := B;
  for I := 1 to Len do
    begin
      if CharMatchNoAsciiCaseBW(Q^, P^) then
        begin
          Inc(P);
          Inc(Q);
        end
      else
        begin
          Result := False;
          exit;
        end;
    end;
  Result := True;
end;

function StrPMatchNoAsciiCaseBS(const A: PChar; const B: PByteChar; const Len: Integer): Boolean;
var P : PChar;
    Q : PByteChar;
    I : Integer;
begin
  P := A;
  Q := B;
  for I := 1 to Len do
    begin
      if CharMatchNoAsciiCaseBS(Q^, P^) then
        begin
          Inc(P);
          Inc(Q);
        end
      else
        begin
          Result := False;
          exit;
        end;
    end;
  Result := True;
end;

function StrPMatchNoAsciiCase(const A, B: PChar; const Len: Integer): Boolean;
var P, Q : PChar;
    I    : Integer;
begin
  P := A;
  Q := B;
  if P <> Q then
    for I := 1 to Len do
      begin
        if CharMatchNoAsciiCase(P^, Q^) then
          begin
            Inc(P);
            Inc(Q);
          end else
          begin
            Result := False;
            exit;
          end;
      end;
  Result := True;
end;

function StrPMatchLenB(const P: PByteChar; const Len: Integer; const M: ByteCharSet): Integer;
var Q : PByteChar;
    L : Integer;
begin
  Q := P;
  L := Len;
  Result := 0;
  if not Assigned(Q) then
    exit;
  while L > 0 do
    if Q^ in M then
      begin
        Inc(Q);
        Dec(L);
        Inc(Result);
      end
    else
      exit;
end;

function StrPMatchLenW(const P: PWideChar; const Len: Integer; const M: ByteCharSet): Integer;
var Q : PWideChar;
    L : Integer;
begin
  Q := P;
  L := Len;
  Result := 0;
  if not Assigned(Q) then
    exit;
  while L > 0 do
    if WideCharInCharSet(Q^, M) then
      begin
        Inc(Q);
        Dec(L);
        Inc(Result);
      end
    else
      exit;
end;

function StrPMatchLenW(const P: PWideChar; const Len: Integer; const M: TWideCharMatchFunction): Integer;
var Q : PWideChar;
    L : Integer;
begin
  Q := P;
  L := Len;
  Result := 0;
  if not Assigned(Q) then
    exit;
  while L > 0 do
    if M(Q^) then
      begin
        Inc(Q);
        Dec(L);
        Inc(Result);
      end
    else
      exit;
end;

function StrPMatchLen(const P: PChar; const Len: Integer; const M: ByteCharSet): Integer;
var Q : PChar;
    C : Char;
    L : Integer;
begin
  Q := P;
  L := Len;
  Result := 0;
  if not Assigned(Q) then
    exit;
  while L > 0 do
    begin
      C := Q^;
      {$IFDEF StringIsUnicode}
      if WideCharInCharSet(C, M) then
      {$ELSE}
      if C in M then
      {$ENDIF}
        begin
          Inc(Q);
          Dec(L);
          Inc(Result);
        end
      else
        exit;
    end;
end;

function StrPMatchCountB(const P: PByteChar; const Len: Integer; const M: ByteCharSet): Integer;
var Q : PByteChar;
    L : Integer;
begin
  Q := P;
  L := Len;
  Result := 0;
  if not Assigned(Q) then
    exit;
  while L > 0 do
    begin
      if Q^ in M then
        Inc(Result);
      Inc(Q);
      Dec(L);
    end;
end;

function StrPMatchCountW(const P: PWideChar; const Len: Integer; const M: ByteCharSet): Integer;
var Q : PWideChar;
    L : Integer;
begin
  Q := P;
  L := Len;
  Result := 0;
  if not Assigned(Q) then
    exit;
  while L > 0 do
    begin
      if WideCharInCharSet(Q^, M) then
        Inc(Result);
      Inc(Q);
      Dec(L);
    end;
end;

function StrPMatchCountW(const P: PWideChar; const Len: Integer; const M: TWideCharMatchFunction): Integer;
var Q : PWideChar;
    L : Integer;
begin
  Q := P;
  L := Len;
  Result := 0;
  if not Assigned(Q) then
    exit;
  while L > 0 do
    begin
      if M(Q^) then
        Inc(Result);
      Inc(Q);
      Dec(L);
    end;
end;

function StrPMatchCharB(const P: PByteChar; const Len: Integer; const M: ByteCharSet): Boolean;
begin
  Result := StrPMatchLenB(P, Len, M) = Len;
end;

function StrPMatchCharW(const P: PWideChar; const Len: Integer; const M: ByteCharSet): Boolean;
begin
  Result := StrPMatchLenW(P, Len, M) = Len;
end;

function StrPMatchCharW(const P: PWideChar; const Len: Integer; const M: TWideCharMatchFunction): Boolean;
begin
  Result := StrPMatchLenW(P, Len, M) = Len;
end;

function StrPMatchChar(const P: PChar; const Len: Integer; const M: ByteCharSet): Boolean;
begin
  Result := StrPMatchLen(P, Len, M) = Len;
end;

{$IFDEF SupportAnsiString}
function StrMatchA(const S, M: AnsiString; const Index: Integer): Boolean;
var N, T : Integer;
    Q    : PByteChar;
begin
  N := Length(M);
  T := Length(S);
  if (N = 0) or (T = 0) or (Index < 1) or (Index + N - 1 > T) then
    begin
      Result := False;
      exit;
    end;
  Q := Pointer(S);
  Inc(Q, Index - 1);
  Result := StrPMatchB(Pointer(M), Q, N);
end;
{$ENDIF}

function StrMatchB(const S, M: RawByteString; const Index: Integer): Boolean;
var N, T : Integer;
    Q    : PByteChar;
begin
  N := Length(M);
  T := Length(S);
  if (N = 0) or (T = 0) or (Index < 1) or (Index + N - 1 > T) then
    begin
      Result := False;
      exit;
    end;
  Q := Pointer(S);
  Inc(Q, Index - 1);
  Result := StrPMatchB(Pointer(M), Q, N);
end;

function StrMatchU(const S, M: UnicodeString; const Index: Integer): Boolean;
var N, T, I : Integer;
begin
  N := Length(M);
  T := Length(S);
  if (N = 0) or (T = 0) or (Index < 1) or (Index + N - 1 > T) then
    begin
      Result := False;
      exit;
    end;
  for I := 1 to N do
    if M[I] <> S[I + Index - 1] then
      begin
        Result := False;
        exit;
      end;
  Result := True;
end;

{$IFDEF SupportAnsiString}
function StrMatchAU(const S: UnicodeString; const M: AnsiString; const Index: Integer): Boolean;
var N, T, I : Integer;
begin
  N := Length(M);
  T := Length(S);
  if (N = 0) or (T = 0) or (Index < 1) or (Index + N - 1 > T) then
    begin
      Result := False;
      exit;
    end;
  for I := 1 to N do
    if Ord(M[I]) <> Ord(S[I + Index - 1]) then
      begin
        Result := False;
        exit;
      end;
  Result := True;
end;
{$ENDIF}

function StrMatchBU(const S: UnicodeString; const M: RawByteString; const Index: Integer): Boolean;
var N, T, I : Integer;
begin
  N := Length(M);
  T := Length(S);
  if (N = 0) or (T = 0) or (Index < 1) or (Index + N - 1 > T) then
    begin
      Result := False;
      exit;
    end;
  for I := 1 to N do
    if Ord(M[I]) <> Ord(S[I + Index - 1]) then
      begin
        Result := False;
        exit;
      end;
  Result := True;
end;

{$IFDEF SupportAnsiString}
function StrMatchAS(const S: String; const M: AnsiString; const Index: Integer): Boolean;
var N, T, I : Integer;
begin
  N := Length(M);
  T := Length(S);
  if (N = 0) or (T = 0) or (Index < 1) or (Index + N - 1 > T) then
    begin
      Result := False;
      exit;
    end;
  for I := 1 to N do
    if Ord(M[I]) <> Ord(S[I + Index - 1]) then
      begin
        Result := False;
        exit;
      end;
  Result := True;
end;
{$ENDIF}

function StrMatch(const S, M: String; const Index: Integer): Boolean;
var N, T, I : Integer;
begin
  N := Length(M);
  T := Length(S);
  if (N = 0) or (T = 0) or (Index < 1) or (Index + N - 1 > T) then
    begin
      Result := False;
      exit;
    end;
  for I := 1 to N do
    if M[I] <> S[I + Index - 1] then
      begin
        Result := False;
        exit;
      end;
  Result := True;
end;

{$IFDEF SupportAnsiString}
function StrMatchNoAsciiCaseA(const S, M: AnsiString; const Index: Integer): Boolean;
var N, T : Integer;
    Q    : PAnsiChar;
begin
  N := Length(M);
  T := Length(S);
  if (N = 0) or (T = 0) or (Index < 1) or (Index + N - 1 > T) then
    begin
      Result := False;
      exit;
    end;
  Q := Pointer(S);
  Inc(Q, Index - 1);
  Result := StrPMatchNoAsciiCaseB(Pointer(M), Pointer(Q), N);
end;
{$ENDIF}

function StrMatchNoAsciiCaseB(const S, M: RawByteString; const Index: Integer): Boolean;
var N, T : Integer;
    Q    : PByteChar;
begin
  N := Length(M);
  T := Length(S);
  if (N = 0) or (T = 0) or (Index < 1) or (Index + N - 1 > T) then
    begin
      Result := False;
      exit;
    end;
  Q := Pointer(S);
  Inc(Q, Index - 1);
  Result := StrPMatchNoAsciiCaseB(Pointer(M), Q, N);
end;

function StrMatchNoAsciiCaseU(const S, M: UnicodeString; const Index: Integer): Boolean;
var N, T : Integer;
    Q    : PWideChar;
begin
  N := Length(M);
  T := Length(S);
  if (N = 0) or (T = 0) or (Index < 1) or (Index + N - 1 > T) then
    begin
      Result := False;
      exit;
    end;
  Q := Pointer(S);
  Inc(Q, Index - 1);
  Result := StrPMatchNoAsciiCaseW(Pointer(M), Q, N);
end;

{$IFDEF SupportAnsiString}
function StrMatchNoAsciiCaseAU(const S: UnicodeString; const M: AnsiString; const Index: Integer): Boolean;
var N, T : Integer;
    Q    : PWideChar;
begin
  N := Length(M);
  T := Length(S);
  if (N = 0) or (T = 0) or (Index < 1) or (Index + N - 1 > T) then
    begin
      Result := False;
      exit;
    end;
  Q := Pointer(S);
  Inc(Q, Index - 1);
  Result := StrPMatchNoAsciiCaseBW(Q, Pointer(M), N);
end;
{$ENDIF}

{$IFDEF SupportAnsiString}
function StrMatchNoAsciiCaseAS(const S: String; const M: AnsiString; const Index: Integer): Boolean;
var N, T : Integer;
    Q    : PChar;
begin
  N := Length(M);
  T := Length(S);
  if (N = 0) or (T = 0) or (Index < 1) or (Index + N - 1 > T) then
    begin
      Result := False;
      exit;
    end;
  Q := Pointer(S);
  Inc(Q, Index - 1);
  Result := StrPMatchNoAsciiCaseBS(Q, Pointer(M), N);
end;
{$ENDIF}

function StrMatchNoAsciiCaseBU(const S: UnicodeString; const M: RawByteString; const Index: Integer): Boolean;
var N, T : Integer;
    Q    : PWideChar;
begin
  N := Length(M);
  T := Length(S);
  if (N = 0) or (T = 0) or (Index < 1) or (Index + N - 1 > T) then
    begin
      Result := False;
      exit;
    end;
  Q := Pointer(S);
  Inc(Q, Index - 1);
  Result := StrPMatchNoAsciiCaseBW(Q, Pointer(M), N);
end;

function StrMatchNoAsciiCase(const S, M: String; const Index: Integer = 1): Boolean;
var N, T : Integer;
    Q    : PChar;
begin
  N := Length(M);
  T := Length(S);
  if (N = 0) or (T = 0) or (Index < 1) or (Index + N - 1 > T) then
    begin
      Result := False;
      exit;
    end;
  Q := Pointer(S);
  Inc(Q, Index - 1);
  Result := StrPMatchNoAsciiCase(Pointer(M), Q, N);
end;

{$IFDEF SupportAnsiString}
function StrMatchLeftA(const S, M: AnsiString; const AsciiCaseSensitive: Boolean): Boolean;
begin
  if AsciiCaseSensitive then
    Result := StrMatchA(S, M, 1)
  else
    Result := StrMatchNoAsciiCaseA(S, M, 1);
end;
{$ENDIF}

function StrMatchLeftB(const S, M: RawByteString; const AsciiCaseSensitive: Boolean): Boolean;
begin
  if AsciiCaseSensitive then
    Result := StrMatchB(S, M, 1)
  else
    Result := StrMatchNoAsciiCaseB(S, M, 1);
end;

function StrMatchLeftU(const S, M: UnicodeString; const AsciiCaseSensitive: Boolean): Boolean;
begin
  if AsciiCaseSensitive then
    Result := StrMatchU(S, M, 1)
  else
    Result := StrMatchNoAsciiCaseU(S, M, 1);
end;

{$IFDEF SupportAnsiString}
function StrMatchLeftAU(const S: UnicodeString; const M: AnsiString; const AsciiCaseSensitive: Boolean): Boolean;
begin
  if AsciiCaseSensitive then
    Result := StrMatchAU(S, M, 1)
  else
    Result := StrMatchNoAsciiCaseAU(S, M, 1);
end;
{$ENDIF}

function StrMatchLeftBU(const S: UnicodeString; const M: RawByteString; const AsciiCaseSensitive: Boolean): Boolean;
begin
  if AsciiCaseSensitive then
    Result := StrMatchBU(S, M, 1)
  else
    Result := StrMatchNoAsciiCaseBU(S, M, 1);
end;

function StrMatchLeft(const S, M: String; const AsciiCaseSensitive: Boolean): Boolean;
begin
  if AsciiCaseSensitive then
    Result := StrMatch(S, M, 1)
  else
    Result := StrMatchNoAsciiCase(S, M, 1);
end;

{$IFDEF SupportAnsiString}
function StrMatchRightA(const S, M: AnsiString; const AsciiCaseSensitive: Boolean): Boolean;
var I: Integer;
begin
  I := Length(S) - Length(M) + 1;
  if AsciiCaseSensitive then
    Result := StrMatchA(S, M, I)
  else
    Result := StrMatchNoAsciiCaseA(S, M, I);
end;
{$ENDIF}

function StrMatchRightB(const S, M: RawByteString; const AsciiCaseSensitive: Boolean): Boolean;
var I: Integer;
begin
  I := Length(S) - Length(M) + 1;
  if AsciiCaseSensitive then
    Result := StrMatchB(S, M, I)
  else
    Result := StrMatchNoAsciiCaseB(S, M, I);
end;

function StrMatchRightU(const S, M: UnicodeString; const AsciiCaseSensitive: Boolean): Boolean;
var I: Integer;
begin
  I := Length(S) - Length(M) + 1;
  if AsciiCaseSensitive then
    Result := StrMatchU(S, M, I)
  else
    Result := StrMatchNoAsciiCaseU(S, M, I);
end;

{$IFDEF SupportAnsiString}
function StrMatchRightAU(const S: UnicodeString; const M: AnsiString; const AsciiCaseSensitive: Boolean): Boolean;
var I: Integer;
begin
  I := Length(S) - Length(M) + 1;
  if AsciiCaseSensitive then
    Result := StrMatchAU(S, M, I)
  else
    Result := StrMatchNoAsciiCaseAU(S, M, I);
end;
{$ENDIF}

function StrMatchRight(const S, M: String; const AsciiCaseSensitive: Boolean): Boolean;
var I: Integer;
begin
  I := Length(S) - Length(M) + 1;
  if AsciiCaseSensitive then
    Result := StrMatch(S, M, I)
  else
    Result := StrMatchNoAsciiCase(S, M, I);
end;

{$IFDEF SupportAnsiString}
function StrMatchLenA(const S: AnsiString; const M: ByteCharSet; const Index: Integer): Integer;
var P    : PAnsiChar;
    L, I : Integer;
begin
  I := Index;
  if I <= 0 then
    I := 1;
  L := Length(S);
  if I > L then
    Result := 0
  else
    begin
      P := Pointer(S);
      Dec(I);
      Inc(P, I);
      Result := StrPMatchLenB(Pointer(P), L - I, M);
    end;
end;
{$ENDIF}

function StrMatchLenB(const S: RawByteString; const M: ByteCharSet; const Index: Integer): Integer;
var P    : PByteChar;
    L, I : Integer;
begin
  I := Index;
  if I <= 0 then
    I := 1;
  L := Length(S);
  if I > L then
    Result := 0
  else
    begin
      P := Pointer(S);
      Dec(I);
      Inc(P, I);
      Result := StrPMatchLenB(P, L - I, M);
    end;
end;

function StrMatchLenU(const S: UnicodeString; const M: ByteCharSet; const Index: Integer): Integer;
var P    : PWideChar;
    L, I : Integer;
begin
  I := Index;
  if I <= 0 then
    I := 1;
  L := Length(S);
  if I > L then
    Result := 0
  else
    begin
      P := Pointer(S);
      Dec(I);
      Inc(P, I);
      Result := StrPMatchLenW(P, L - I, M);
    end;
end;

function StrMatchLenU(const S: UnicodeString; const M: TWideCharMatchFunction; const Index: Integer = 1): Integer;
var P    : PWideChar;
    L, I : Integer;
begin
  I := Index;
  if I <= 0 then
    I := 1;
  L := Length(S);
  if I > L then
    Result := 0
  else
    begin
      P := Pointer(S);
      Dec(I);
      Inc(P, I);
      Result := StrPMatchLenW(P, L - I, M);
    end;
end;

function StrMatchLen(const S: String; const M: ByteCharSet; const Index: Integer): Integer;
var P    : PChar;
    L, I : Integer;
begin
  I := Index;
  if I <= 0 then
    I := 1;
  L := Length(S);
  if I > L then
    Result := 0
  else
    begin
      P := Pointer(S);
      Dec(I);
      Inc(P, I);
      Result := StrPMatchLen(P, L - I, M);
    end;
end;

{$IFDEF SupportAnsiString}
function StrMatchCharA(const S: AnsiString; const M: ByteCharSet): Boolean;
var L: Integer;
begin
  L := Length(S);
  Result := (L > 0) and (StrPMatchLenB(Pointer(S), L, M) = L);
end;
{$ENDIF}

function StrMatchCharB(const S: RawByteString; const M: ByteCharSet): Boolean;
var L: Integer;
begin
  L := Length(S);
  Result := (L > 0) and (StrPMatchLenB(Pointer(S), L, M) = L);
end;

function StrMatchCharU(const S: UnicodeString; const M: ByteCharSet): Boolean;
var L: Integer;
begin
  L := Length(S);
  Result := (L > 0) and (StrPMatchLenW(Pointer(S), L, M) = L);
end;

function StrMatchCharU(const S: UnicodeString; const M: TWideCharMatchFunction): Boolean;
var L: Integer;
begin
  L := Length(S);
  Result := (L > 0) and (StrPMatchLenW(Pointer(S), L, M) = L);
end;

function StrMatchChar(const S: String; const M: ByteCharSet): Boolean;
var L: Integer;
begin
  L := Length(S);
  Result := (L > 0) and (StrPMatchLen(Pointer(S), L, M) = L);
end;



{                                                                              }
{ Equal                                                                        }
{                                                                              }
function StrPEqual(const P1, P2: PByteChar; const Len1, Len2: Integer;
         const AsciiCaseSensitive: Boolean): Boolean;
begin
  Result := Len1 = Len2;
  if not Result or (Len1 = 0) then
    exit;
  if AsciiCaseSensitive then
    Result := StrPMatchB(P1, P2, Len1)
  else
    Result := StrPMatchNoAsciiCaseB(P1, P2, Len1);
end;

{$IFDEF SupportAnsiString}
function StrPEqualStr(const P: PAnsiChar; const Len: Integer; const S: AnsiString;
         const AsciiCaseSensitive: Boolean): Boolean;
begin
  Result := Len = Length(S);
  if not Result or (Len = 0) then
    exit;
  if AsciiCaseSensitive then
    Result := StrPMatchB(Pointer(P), Pointer(S), Len)
  else
    Result := StrPMatchNoAsciiCaseB(Pointer(P), Pointer(S), Len);
end;
{$ENDIF}

{$IFDEF SupportAnsiString}
function StrEqualA(const A, B: AnsiString; const AsciiCaseSensitive: Boolean): Boolean;
var L1, L2 : Integer;
begin
  L1 := Length(A);
  L2 := Length(B);
  Result := L1 = L2;
  if not Result or (L1 = 0) then
    exit;
  if AsciiCaseSensitive then
    Result := StrPMatchB(Pointer(A), Pointer(B), L1)
  else
    Result := StrPMatchNoAsciiCaseB(Pointer(A), Pointer(B), L1);
end;
{$ENDIF}

function StrEqualB(const A, B: RawByteString; const AsciiCaseSensitive: Boolean): Boolean;
var L1, L2 : Integer;
begin
  L1 := Length(A);
  L2 := Length(B);
  Result := L1 = L2;
  if not Result or (L1 = 0) then
    exit;
  if AsciiCaseSensitive then
    Result := StrPMatchB(Pointer(A), Pointer(B), L1)
  else
    Result := StrPMatchNoAsciiCaseB(Pointer(A), Pointer(B), L1);
end;

function StrEqualU(const A, B: UnicodeString; const AsciiCaseSensitive: Boolean): Boolean;
var L1, L2 : Integer;
begin
  L1 := Length(A);
  L2 := Length(B);
  Result := L1 = L2;
  if not Result or (L1 = 0) then
    exit;
  if AsciiCaseSensitive then
    Result := StrPMatchW(Pointer(A), Pointer(B), L1)
  else
    Result := StrPMatchNoAsciiCaseW(Pointer(A), Pointer(B), L1);
end;

{$IFDEF SupportAnsiString}
function StrEqualAU(const A: UnicodeString; const B: AnsiString; const AsciiCaseSensitive: Boolean): Boolean;
var L1, L2 : Integer;
begin
  L1 := Length(A);
  L2 := Length(B);
  Result := L1 = L2;
  if not Result or (L1 = 0) then
    exit;
  if AsciiCaseSensitive then
    Result := StrPMatchBW(Pointer(A), Pointer(B), L1, L1)
  else
    Result := StrPMatchNoAsciiCaseBW(Pointer(A), Pointer(B), L1);
end;
{$ENDIF}

function StrEqualBU(const A: UnicodeString; const B: RawByteString; const AsciiCaseSensitive: Boolean): Boolean;
var L1, L2 : Integer;
begin
  L1 := Length(A);
  L2 := Length(B);
  Result := L1 = L2;
  if not Result or (L1 = 0) then
    exit;
  if AsciiCaseSensitive then
    Result := StrPMatchBW(Pointer(A), Pointer(B), L1, L1)
  else
    Result := StrPMatchNoAsciiCaseBW(Pointer(A), Pointer(B), L1);
end;

function StrEqual(const A, B: String; const AsciiCaseSensitive: Boolean): Boolean;
var L1, L2 : Integer;
begin
  L1 := Length(A);
  L2 := Length(B);
  Result := L1 = L2;
  if not Result or (L1 = 0) then
    exit;
  if AsciiCaseSensitive then
    Result := StrPMatch(Pointer(A), Pointer(B), L1)
  else
    Result := StrPMatchNoAsciiCase(Pointer(A), Pointer(B), L1);
end;

{$IFDEF SupportAnsiString}
function StrEqualNoAsciiCaseA(const A, B: AnsiString): Boolean;
var L, M : Integer;
begin
  L := Length(A);
  M := Length(B);
  Result := L = M;
  if not Result or (L = 0) then
    exit;
  Result := StrPMatchNoAsciiCaseB(Pointer(A), Pointer(B), L);
end;
{$ENDIF}

function StrEqualNoAsciiCaseB(const A, B: RawByteString): Boolean;
var L, M : Integer;
begin
  L := Length(A);
  M := Length(B);
  Result := L = M;
  if not Result or (L = 0) then
    exit;
  Result := StrPMatchNoAsciiCaseB(Pointer(A), Pointer(B), L);
end;

function StrEqualNoAsciiCaseU(const A, B: UnicodeString): Boolean;
var L, M : Integer;
begin
  L := Length(A);
  M := Length(B);
  Result := L = M;
  if not Result or (L = 0) then
    exit;
  Result := StrPMatchNoAsciiCaseW(Pointer(A), Pointer(B), L);
end;

{$IFDEF SupportAnsiString}
function StrEqualNoAsciiCaseAU(const A: UnicodeString; const B: AnsiString): Boolean;
var L, M : Integer;
begin
  L := Length(A);
  M := Length(B);
  Result := L = M;
  if not Result or (L = 0) then
    exit;
  Result := StrPMatchNoAsciiCaseBW(Pointer(A), Pointer(B), L);
end;
{$ENDIF}

function StrEqualNoAsciiCaseBU(const A: UnicodeString; const B: RawByteString): Boolean;
var L, M : Integer;
begin
  L := Length(A);
  M := Length(B);
  Result := L = M;
  if not Result or (L = 0) then
    exit;
  Result := StrPMatchNoAsciiCaseBW(Pointer(A), Pointer(B), L);
end;

function StrEqualNoAsciiCase(const A, B: String): Boolean;
var L, M : Integer;
begin
  L := Length(A);
  M := Length(B);
  Result := L = M;
  if not Result or (L = 0) then
    exit;
  Result := StrPMatchNoAsciiCase(Pointer(A), Pointer(B), L);
end;



{                                                                              }
{ Validation                                                                   }
{                                                                              }
{$IFDEF SupportAnsiString}
function StrIsNumericA(const S: AnsiString): Boolean;
begin
  Result := StrMatchCharA(S, csNumeric);
end;
{$ENDIF}

function StrIsNumericB(const S: RawByteString): Boolean;
begin
  Result := StrMatchCharB(S, csNumeric);
end;

function StrIsNumericU(const S: UnicodeString): Boolean;
begin
  Result := StrMatchCharU(S, csNumeric);
end;

function StrIsNumeric(const S: String): Boolean;
begin
  Result := StrMatchChar(S, csNumeric);
end;

{$IFDEF SupportAnsiString}
function StrIsHexA(const S: AnsiString): Boolean;
begin
  Result := StrMatchCharA(S, csHexDigit);
end;
{$ENDIF}

function StrIsHexB(const S: RawByteString): Boolean;
begin
  Result := StrMatchCharB(S, csHexDigit);
end;

function StrIsHexU(const S: UnicodeString): Boolean;
begin
  Result := StrMatchCharU(S, csHexDigit);
end;

function StrIsHex(const S: String): Boolean;
begin
  Result := StrMatchChar(S, csHexDigit);
end;

{$IFDEF SupportAnsiString}
function StrIsAlphaA(const S: AnsiString): Boolean;
begin
  Result := StrMatchCharA(S, csAlpha);
end;
{$ENDIF}

function StrIsAlphaB(const S: RawByteString): Boolean;
begin
  Result := StrMatchCharB(S, csAlpha);
end;

function StrIsAlphaU(const S: UnicodeString): Boolean;
begin
  Result := StrMatchCharU(S, csAlpha);
end;

function StrIsAlpha(const S: String): Boolean;
begin
  Result := StrMatchChar(S, csAlpha);
end;

{$IFDEF SupportAnsiString}
function StrIsAlphaNumericA(const S: AnsiString): Boolean;
begin
  Result := StrMatchCharA(S, csAlphaNumeric);
end;
{$ENDIF}

function StrIsAlphaNumericB(const S: RawByteString): Boolean;
begin
  Result := StrMatchCharB(S, csAlphaNumeric);
end;

function StrIsAlphaNumericU(const S: UnicodeString): Boolean;
begin
  Result := StrMatchCharU(S, csAlphaNumeric);
end;

function StrIsAlphaNumeric(const S: String): Boolean;
begin
  Result := StrMatchChar(S, csAlphaNumeric);
end;

{$IFDEF SupportAnsiString}
function StrIsIntegerA(const S: AnsiString): Boolean;
var L: Integer;
    P: PAnsiChar;
begin
  L := Length(S);
  Result := L > 0;
  if not Result then
    exit;
  P := Pointer(S);
  if P^ in csSign then
    begin
      Inc(P);
      Dec(L);
    end;
  Result := (L > 0) and (StrPMatchLenB(Pointer(P), L, csNumeric) = L);
end;
{$ENDIF}

function StrIsIntegerB(const S: RawByteString): Boolean;
var L: Integer;
    P: PByteChar;
begin
  L := Length(S);
  Result := L > 0;
  if not Result then
    exit;
  P := Pointer(S);
  if P^ in csSign then
    begin
      Inc(P);
      Dec(L);
    end;
  Result := (L > 0) and (StrPMatchLenB(P, L, csNumeric) = L);
end;

function StrIsIntegerU(const S: UnicodeString): Boolean;
var L: Integer;
    P: PWideChar;
begin
  L := Length(S);
  Result := L > 0;
  if not Result then
    exit;
  P := Pointer(S);
  case P^ of
    '+', '-' :
      begin
        Inc(P);
        Dec(L);
      end;
  end;
  Result := (L > 0) and (StrPMatchLenW(P, L, csNumeric) = L);
end;

function StrIsInteger(const S: String): Boolean;
var L: Integer;
    P: PChar;
begin
  L := Length(S);
  Result := L > 0;
  if not Result then
    exit;
  P := Pointer(S);
  case P^ of
    '+', '-' :
      begin
        Inc(P);
        Dec(L);
      end;
  end;
  Result := (L > 0) and (StrPMatchLen(P, L, csNumeric) = L);
end;



{                                                                              }
{ Pos                                                                          }
{                                                                              }
function StrPPosCharB(const F: ByteChar; const S: PByteChar; const Len: Integer): Integer;
var I : Integer;
    P : PByteChar;
begin
  if Len <= 0 then
    begin
      Result := -1;
      exit;
    end;
  P := S;
  for I := 0 to Len - 1 do
    if P^ = F then
      begin
        Result := I;
        exit;
      end
    else
      Inc(P);
  Result := -1;
end;

function StrPPosCharW(const F: WideChar; const S: PWideChar; const Len: Integer): Integer;
var I : Integer;
    P : PWideChar;
begin
  if Len <= 0 then
    begin
      Result := -1;
      exit;
    end;
  P := S;
  for I := 0 to Len - 1 do
    if P^ = F then
      begin
        Result := I;
        exit;
      end
    else
      Inc(P);
  Result := -1;
end;

function StrPPosCharSetB(const F: ByteCharSet; const S: PByteChar; const Len: Integer): Integer;
var I : Integer;
    P : PByteChar;
begin
  if Len <= 0 then
    begin
      Result := -1;
      exit;
    end;
  P := S;
  for I := 0 to Len - 1 do
    if P^ in F then
      begin
        Result := I;
        exit;
      end
    else
      Inc(P);
  Result := -1;
end;

function StrPPosCharSetW(const F: ByteCharSet; const S: PWideChar; const Len: Integer): Integer;
var I : Integer;
    P : PWideChar;
begin
  if Len <= 0 then
    begin
      Result := -1;
      exit;
    end;
  P := S;
  for I := 0 to Len - 1 do
    if WideCharInCharSet(P^, F) then
      begin
        Result := I;
        exit;
      end
    else
      Inc(P);
  Result := -1;
end;

function StrPPosB(const F, S: PByteChar; const LenF, LenS: Integer): Integer;
var I : Integer;
    P : PByteChar;
begin
  if (LenF <= 0) or (LenS <= 0) or (LenF > LenS) then
    begin
      Result := -1;
      exit;
    end;
  P := S;
  for I := 0 to LenS - LenF do
    if StrPMatchB(P, F, LenF) then
      begin
        Result := I;
        exit;
      end
    else
      Inc(P);
  Result := -1;
end;

function StrPPosW(const F, S: PWideChar; const LenF, LenS: Integer): Integer;
var I : Integer;
    P : PWideChar;
begin
  if (LenF <= 0) or (LenS <= 0) or (LenF > LenS) then
    begin
      Result := -1;
      exit;
    end;
  P := S;
  for I := 0 to LenS - LenF do
    if StrPMatchW(P, F, LenF) then
      begin
        Result := I;
        exit;
      end
    else
      Inc(P);
  Result := -1;
end;

{$IFDEF SupportAnsiString}
function StrPPosStrA(const F: AnsiString; const S: PAnsiChar; const Len: Integer): Integer;
var LenF : Integer;
    I : Integer;
    P : PAnsiChar;
begin
  LenF := Length(F);
  if (LenF <= 0) or (Len <= 0) or (LenF > Len) then
    begin
      Result := -1;
      exit;
    end;
  P := S;
  for I := 0 to Len - LenF do
    if StrPMatchStrA(P, LenF, F) then
      begin
        Result := I;
        exit;
      end
    else
      Inc(P);
  Result := -1;
end;
{$ENDIF}

function StrPPosStrB(const F: RawByteString; const S: PByteChar; const Len: Integer): Integer;
var LenF : Integer;
    I : Integer;
    P : PByteChar;
begin
  LenF := Length(F);
  if (LenF <= 0) or (Len <= 0) or (LenF > Len) then
    begin
      Result := -1;
      exit;
    end;
  P := S;
  for I := 0 to Len - LenF do
    if StrPMatchStrB(P, LenF, F) then
      begin
        Result := I;
        exit;
      end
    else
      Inc(P);
  Result := -1;
end;

{$IFDEF SupportAnsiString}
function PosCharA(const F: AnsiChar; const S: AnsiString; const Index: Integer): Integer;
var P    : PAnsiChar;
    L, I : Integer;
begin
  L := Length(S);
  if (L = 0) or (Index > L) then
    begin
      Result := 0;
      exit;
    end;
  if Index < 1 then
    I := 1
  else
    I := Index;
  P := Pointer(S);
  Inc(P, I - 1);
  while I <= L do
    if P^ = F then
      begin
        Result := I;
        exit;
      end else
      begin
        Inc(P);
        Inc(I);
      end;
  Result := 0;
end;
{$ENDIF}

function PosCharB(const F: ByteChar; const S: RawByteString; const Index: Integer): Integer;
var P    : PByteChar;
    L, I : Integer;
begin
  L := Length(S);
  if (L = 0) or (Index > L) then
    begin
      Result := 0;
      exit;
    end;
  if Index < 1 then
    I := 1
  else
    I := Index;
  P := Pointer(S);
  Inc(P, I - 1);
  while I <= L do
    if P^ = F then
      begin
        Result := I;
        exit;
      end else
      begin
        Inc(P);
        Inc(I);
      end;
  Result := 0;
end;

function PosCharU(const F: WideChar; const S: UnicodeString; const Index: Integer): Integer;
var L, I : Integer;
begin
  L := Length(S);
  if (L = 0) or (Index > L) then
    begin
      Result := 0;
      exit;
    end;
  if Index < 1 then
    I := 1
  else
    I := Index;
  while I <= L do
    if S[I] = F then
      begin
        Result := I;
        exit;
      end
    else
      Inc(I);
  Result := 0;
end;

function PosChar(const F: Char; const S: String; const Index: Integer): Integer;
var L, I : Integer;
begin
  L := Length(S);
  if (L = 0) or (Index > L) then
    begin
      Result := 0;
      exit;
    end;
  if Index < 1 then
    I := 1
  else
    I := Index;
  while I <= L do
    if S[I] = F then
      begin
        Result := I;
        exit;
      end
    else
      Inc(I);
  Result := 0;
end;

{$IFDEF SupportAnsiString}
function PosCharSetA(const F: ByteCharSet; const S: AnsiString; const Index: Integer): Integer;
var P    : PAnsiChar;
    L, I : Integer;
begin
  L := Length(S);
  if (L = 0) or (Index > L) then
    begin
      Result := 0;
      exit;
    end;
  if Index < 1 then
    I := 1
  else
    I := Index;
  P := Pointer(S);
  Inc(P, I - 1);
  while I <= L do
    if P^ in F then
      begin
        Result := I;
        exit;
      end else
      begin
        Inc(P);
        Inc(I);
      end;
  Result := 0;
end;
{$ENDIF}

function PosCharSetB(const F: ByteCharSet; const S: RawByteString; const Index: Integer): Integer;
var P    : PByteChar;
    L, I : Integer;
begin
  L := Length(S);
  if (L = 0) or (Index > L) then
    begin
      Result := 0;
      exit;
    end;
  if Index < 1 then
    I := 1
  else
    I := Index;
  P := Pointer(S);
  Inc(P, I - 1);
  while I <= L do
    if P^ in F then
      begin
        Result := I;
        exit;
      end else
      begin
        Inc(P);
        Inc(I);
      end;
  Result := 0;
end;

function PosCharSetU(const F: ByteCharSet; const S: UnicodeString;
    const Index: Integer): Integer;
var P    : PWideChar;
    C    : WideChar;
    L, I : Integer;
begin
  L := Length(S);
  if (L = 0) or (Index > L) then
    begin
      Result := 0;
      exit;
    end;
  if Index < 1 then
    I := 1
  else
    I := Index;
  P := Pointer(S);
  Inc(P, I - 1);
  while I <= L do
    begin
      C := P^;
      if Ord(C) <= $FF then
        if ByteChar(Ord(C)) in F then
          begin
            Result := I;
            exit;
          end else
          begin
            Inc(P);
            Inc(I);
          end;
    end;
  Result := 0;
end;

function PosCharSetU(const F: TWideCharMatchFunction; const S: UnicodeString;
    const Index: Integer): Integer;
var P    : PWideChar;
    C    : WideChar;
    L, I : Integer;
begin
  L := Length(S);
  if (L = 0) or (Index > L) then
    begin
      Result := 0;
      exit;
    end;
  if Index < 1 then
    I := 1
  else
    I := Index;
  P := Pointer(S);
  Inc(P, I - 1);
  while I <= L do
    begin
      C := P^;
      if F(C) then
        begin
          Result := I;
          exit;
        end else
        begin
          Inc(P);
          Inc(I);
        end;
    end;
  Result := 0;
end;

function PosCharSet(const F: ByteCharSet; const S: String;
    const Index: Integer): Integer;
var P    : PChar;
    C    : Char;
    L, I : Integer;
begin
  L := Length(S);
  if (L = 0) or (Index > L) then
    begin
      Result := 0;
      exit;
    end;
  if Index < 1 then
    I := 1
  else
    I := Index;
  P := Pointer(S);
  Inc(P, I - 1);
  while I <= L do
    begin
      C := P^;
      {$IFDEF StringIsUnicode}
      if Ord(C) <= $FF then
      {$ENDIF}
        if ByteChar(Ord(C)) in F then
          begin
            Result := I;
            exit;
          end else
          begin
            Inc(P);
            Inc(I);
          end;
    end;
  Result := 0;
end;

{$IFDEF SupportAnsiString}
function PosNotCharA(const F: AnsiChar; const S: AnsiString;
    const Index: Integer): Integer;
var P    : PAnsiChar;
    L, I : Integer;
begin
  L := Length(S);
  if (L = 0) or (Index > L) then
    begin
      Result := 0;
      exit;
    end;
  if Index < 1 then
    I := 1
  else
    I := Index;
  P := Pointer(S);
  Inc(P, I - 1);
  while I <= L do
    if P^ <> F then
      begin
        Result := I;
        exit;
      end else
      begin
        Inc(P);
        Inc(I);
      end;
  Result := 0;
end;
{$ENDIF}

function PosNotCharB(const F: ByteChar; const S: RawByteString;
    const Index: Integer): Integer;
var P    : PByteChar;
    L, I : Integer;
begin
  L := Length(S);
  if (L = 0) or (Index > L) then
    begin
      Result := 0;
      exit;
    end;
  if Index < 1 then
    I := 1
  else
    I := Index;
  P := Pointer(S);
  Inc(P, I - 1);
  while I <= L do
    if P^ <> F then
      begin
        Result := I;
        exit;
      end else
      begin
        Inc(P);
        Inc(I);
      end;
  Result := 0;
end;

function PosNotCharU(const F: WideChar; const S: UnicodeString; const Index: Integer): Integer;
var L, I : Integer;
begin
  L := Length(S);
  if (L = 0) or (Index > L) then
    begin
      Result := 0;
      exit;
    end;
  if Index < 1 then
    I := 1
  else
    I := Index;
  while I <= L do
    if S[I] <> F then
      begin
        Result := I;
        exit;
      end
    else
      Inc(I);
  Result := 0;
end;

function PosNotChar(const F: Char; const S: String; const Index: Integer): Integer;
var L, I : Integer;
begin
  L := Length(S);
  if (L = 0) or (Index > L) then
    begin
      Result := 0;
      exit;
    end;
  if Index < 1 then
    I := 1
  else
    I := Index;
  while I <= L do
    if S[I] <> F then
      begin
        Result := I;
        exit;
      end
    else
      Inc(I);
  Result := 0;
end;

{$IFDEF SupportAnsiString}
function PosNotCharSetA(const F: ByteCharSet; const S: AnsiString;
    const Index: Integer): Integer;
var P    : PAnsiChar;
    L, I : Integer;
begin
  L := Length(S);
  if (L = 0) or (Index > L) then
    begin
      Result := 0;
      exit;
    end;
  if Index < 1 then
    I := 1
  else
    I := Index;
  P := Pointer(S);
  Inc(P, I - 1);
  while I <= L do
    if not (P^ in F) then
      begin
        Result := I;
        exit;
      end else
      begin
        Inc(P);
        Inc(I);
      end;
  Result := 0;
end;
{$ENDIF}

function PosNotCharSetB(const F: ByteCharSet; const S: RawByteString;
    const Index: Integer): Integer;
var P    : PByteChar;
    L, I : Integer;
begin
  L := Length(S);
  if (L = 0) or (Index > L) then
    begin
      Result := 0;
      exit;
    end;
  if Index < 1 then
    I := 1
  else
    I := Index;
  P := Pointer(S);
  Inc(P, I - 1);
  while I <= L do
    if not (P^ in F) then
      begin
        Result := I;
        exit;
      end else
      begin
        Inc(P);
        Inc(I);
      end;
  Result := 0;
end;

function PosNotCharSetU(const F: ByteCharSet; const S: UnicodeString;
    const Index: Integer): Integer;
var P    : PWideChar;
    C    : WideChar;
    L, I : Integer;
    R    : Boolean;
begin
  L := Length(S);
  if (L = 0) or (Index > L) then
    begin
      Result := 0;
      exit;
    end;
  if Index < 1 then
    I := 1
  else
    I := Index;
  P := Pointer(S);
  Inc(P, I - 1);
  while I <= L do
    begin
      C := P^;
      R := Ord(C) > $FF;
      if not R then
        R := not (ByteChar(Ord(C)) in F);
      if R then
        begin
          Result := I;
          exit;
        end else
        begin
          Inc(P);
          Inc(I);
        end;
    end;
  Result := 0;
end;

function PosNotCharSetU(const F: TWideCharMatchFunction; const S: UnicodeString;
    const Index: Integer): Integer;
var P    : PWideChar;
    C    : WideChar;
    L, I : Integer;
    R    : Boolean;
begin
  L := Length(S);
  if (L = 0) or (Index > L) then
    begin
      Result := 0;
      exit;
    end;
  if Index < 1 then
    I := 1
  else
    I := Index;
  P := Pointer(S);
  Inc(P, I - 1);
  while I <= L do
    begin
      C := P^;
      R := not F(C);
      if R then
        begin
          Result := I;
          exit;
        end else
        begin
          Inc(P);
          Inc(I);
        end;
    end;
  Result := 0;
end;

function PosNotCharSet(const F: ByteCharSet; const S: String;
    const Index: Integer): Integer;
var P    : PChar;
    C    : Char;
    L, I : Integer;
    R    : Boolean;
begin
  L := Length(S);
  if (L = 0) or (Index > L) then
    begin
      Result := 0;
      exit;
    end;
  if Index < 1 then
    I := 1
  else
    I := Index;
  P := Pointer(S);
  Inc(P, I - 1);
  while I <= L do
    begin
      C := P^;
      {$IFDEF StringIsUnicode}
      R := Ord(C) > $FF;
      if not R then
      {$ENDIF}
        R := not (ByteChar(Ord(C)) in F);
      if R then
        begin
          Result := I;
          exit;
        end else
        begin
          Inc(P);
          Inc(I);
        end;
    end;
  Result := 0;
end;

{$IFDEF SupportAnsiString}
function PosCharRevA(const F: AnsiChar; const S: AnsiString;
    const Index: Integer): Integer;
var P       : PAnsiChar;
    L, I, J : Integer;
begin
  L := Length(S);
  if (L = 0) or (Index > L) then
    begin
      Result := 0;
      exit;
    end;
  if Index < 1 then
    I := 1
  else
    I := Index;
  P := Pointer(S);
  J := L;
  Inc(P, J - 1);
  while J >= I do
    if P^ = F then
      begin
        Result := J;
        exit;
      end else
      begin
        Dec(P);
        Dec(J);
      end;
  Result := 0;
end;
{$ENDIF}

function PosCharRevB(const F: ByteChar; const S: RawByteString;
    const Index: Integer): Integer;
var P       : PByteChar;
    L, I, J : Integer;
begin
  L := Length(S);
  if (L = 0) or (Index > L) then
    begin
      Result := 0;
      exit;
    end;
  if Index < 1 then
    I := 1
  else
    I := Index;
  P := Pointer(S);
  J := L;
  Inc(P, J - 1);
  while J >= I do
    if P^ = F then
      begin
        Result := J;
        exit;
      end else
      begin
        Dec(P);
        Dec(J);
      end;
  Result := 0;
end;

function PosCharRevU(const F: WideChar; const S: UnicodeString; const Index: Integer): Integer;
var L, I, J : Integer;
begin
  L := Length(S);
  if (L = 0) or (Index > L) then
    begin
      Result := 0;
      exit;
    end;
  if Index < 1 then
    I := 1
  else
    I := Index;
  J := L;
  while J >= I do
    if S[J] = F then
      begin
        Result := J;
        exit;
      end
    else
      Dec(J);
  Result := 0;
end;

function PosCharRev(const F: Char; const S: String; const Index: Integer): Integer;
var L, I, J : Integer;
begin
  L := Length(S);
  if (L = 0) or (Index > L) then
    begin
      Result := 0;
      exit;
    end;
  if Index < 1 then
    I := 1
  else
    I := Index;
  J := L;
  while J >= I do
    if S[J] = F then
      begin
        Result := J;
        exit;
      end
    else
      Dec(J);
  Result := 0;
end;

{$IFDEF SupportAnsiString}
function PosCharSetRevA(const F: ByteCharSet; const S: AnsiString; const Index: Integer): Integer;
var P       : PAnsiChar;
    L, I, J : Integer;
begin
  L := Length(S);
  if (L = 0) or (Index > L) then
    begin
      Result := 0;
      exit;
    end;
  if Index < 1 then
    I := 1
  else
    I := Index;
  P := Pointer(S);
  J := L;
  Inc(P, J - 1);
  while J >= I do
    if P^ in F then
      begin
        Result := J;
        exit;
      end
    else
      begin
        Dec(P);
        Dec(J);
      end;
  Result := 0;
end;
{$ENDIF}

function PosCharSetRevB(const F: ByteCharSet; const S: RawByteString; const Index: Integer): Integer;
var P       : PByteChar;
    L, I, J : Integer;
begin
  L := Length(S);
  if (L = 0) or (Index > L) then
    begin
      Result := 0;
      exit;
    end;
  if Index < 1 then
    I := 1
  else
    I := Index;
  P := Pointer(S);
  J := L;
  Inc(P, J - 1);
  while J >= I do
    if P^ in F then
      begin
        Result := J;
        exit;
      end
    else
      begin
        Dec(P);
        Dec(J);
      end;
  Result := 0;
end;

function PosCharSetRevU(const F: ByteCharSet; const S: UnicodeString; const Index: Integer): Integer;
var P       : PWideChar;
    L, I, J : Integer;
    C       : WideChar;
begin
  L := Length(S);
  if (L = 0) or (Index > L) then
    begin
      Result := 0;
      exit;
    end;
  if Index < 1 then
    I := 1
  else
    I := Index;
  P := Pointer(S);
  J := L;
  Inc(P, J - 1);
  while J >= I do
    begin
      C := P^;
      if Ord(C) <= $FF then
        if ByteChar(Ord(C)) in F then
          begin
            Result := J;
            exit;
          end
        else
          begin
            Dec(P);
            Dec(J);
          end;
    end;
  Result := 0;
end;

function PosCharSetRevU(const F: TWideCharMatchFunction; const S: UnicodeString; const Index: Integer): Integer;
var P       : PWideChar;
    L, I, J : Integer;
    C       : WideChar;
begin
  L := Length(S);
  if (L = 0) or (Index > L) then
    begin
      Result := 0;
      exit;
    end;
  if Index < 1 then
    I := 1
  else
    I := Index;
  P := Pointer(S);
  J := L;
  Inc(P, J - 1);
  while J >= I do
    begin
      C := P^;
      if F(C) then
        begin
          Result := J;
          exit;
        end
      else
        begin
          Dec(P);
          Dec(J);
        end;
    end;
  Result := 0;
end;

function PosCharSetRev(const F: ByteCharSet; const S: String; const Index: Integer): Integer;
var P       : PChar;
    L, I, J : Integer;
    C       : Char;
begin
  L := Length(S);
  if (L = 0) or (Index > L) then
    begin
      Result := 0;
      exit;
    end;
  if Index < 1 then
    I := 1
  else
    I := Index;
  P := Pointer(S);
  J := L;
  Inc(P, J - 1);
  while J >= I do
    begin
      C := P^;
      {$IFDEF StringIsUnicode}
      if Ord(C) <= $FF then
      {$ENDIF}
        if ByteChar(Ord(C)) in F then
          begin
            Result := J;
            exit;
          end
        else
          begin
            Dec(P);
            Dec(J);
          end;
    end;
  Result := 0;
end;

{$IFDEF SupportAnsiString}
function PosStrA(const F, S: AnsiString; const Index: Integer;
    const AsciiCaseSensitive: Boolean): Integer;
var P, Q    : PAnsiChar;
    L, M, I : Integer;
begin
  L := Length(S);
  M := Length(F);
  if (L = 0) or (Index > L) or (M = 0) or (M > L) then
    begin
      Result := 0;
      exit;
    end;
  Q := Pointer(F);
  if Index < 1 then
    I := 1
  else
    I := Index;
  P := Pointer(S);
  Inc(P, I - 1);
  Dec(L, M - 1);
  if AsciiCaseSensitive then
    while I <= L do
      if StrPMatchB(Pointer(P), Pointer(Q), M) then
        begin
          Result := I;
          exit;
        end else
        begin
          Inc(P);
          Inc(I);
        end
  else
    while I <= L do
      if StrPMatchNoAsciiCaseB(Pointer(P), Pointer(Q), M) then
        begin
          Result := I;
          exit;
        end else
        begin
          Inc(P);
          Inc(I);
        end;
  Result := 0;
end;
{$ENDIF}

function PosStrB(const F, S: RawByteString; const Index: Integer;
    const AsciiCaseSensitive: Boolean): Integer;
var P, Q    : PByteChar;
    L, M, I : Integer;
begin
  L := Length(S);
  M := Length(F);
  if (L = 0) or (Index > L) or (M = 0) or (M > L) then
    begin
      Result := 0;
      exit;
    end;
  Q := Pointer(F);
  if Index < 1 then
    I := 1
  else
    I := Index;
  P := Pointer(S);
  Inc(P, I - 1);
  Dec(L, M - 1);
  if AsciiCaseSensitive then
    while I <= L do
      if StrPMatchB(P, Q, M) then
        begin
          Result := I;
          exit;
        end else
        begin
          Inc(P);
          Inc(I);
        end
  else
    while I <= L do
      if StrPMatchNoAsciiCaseB(P, Q, M) then
        begin
          Result := I;
          exit;
        end else
        begin
          Inc(P);
          Inc(I);
        end;
  Result := 0;
end;

function PosStrU(const F, S: UnicodeString; const Index: Integer;
    const AsciiCaseSensitive: Boolean): Integer;
var P, Q    : PWideChar;
    L, M, I : Integer;
begin
  L := Length(S);
  M := Length(F);
  if (L = 0) or (Index > L) or (M = 0) or (M > L) then
    begin
      Result := 0;
      exit;
    end;
  Q := Pointer(F);
  if Index < 1 then
    I := 1
  else
    I := Index;
  P := Pointer(S);
  Inc(P, I - 1);
  Dec(L, M - 1);
  if AsciiCaseSensitive then
    while I <= L do
      if StrPMatchW(P, Q, M) then
        begin
          Result := I;
          exit;
        end else
        begin
          Inc(P);
          Inc(I);
        end
  else
    while I <= L do
      if StrPMatchNoAsciiCaseW(P, Q, M) then
        begin
          Result := I;
          exit;
        end else
        begin
          Inc(P);
          Inc(I);
        end;
  Result := 0;
end;

{$IFDEF SupportAnsiString}
function PosStrAU(const F: AnsiString; const S: UnicodeString; const Index: Integer;
    const AsciiCaseSensitive: Boolean): Integer;
var P       : PWideChar;
    Q       : PAnsiChar;
    L, M, I : Integer;
begin
  L := Length(S);
  M := Length(F);
  if (L = 0) or (Index > L) or (M = 0) or (M > L) then
    begin
      Result := 0;
      exit;
    end;
  Q := Pointer(F);
  if Index < 1 then
    I := 1
  else
    I := Index;
  P := Pointer(S);
  Inc(P, I - 1);
  Dec(L, M - 1);
  if AsciiCaseSensitive then
    while I <= L do
      if StrPMatchBW(P, Pointer(Q), M) then
        begin
          Result := I;
          exit;
        end else
        begin
          Inc(P);
          Inc(I);
        end
  else
    while I <= L do
      if StrPMatchNoAsciiCaseBW(P, Pointer(Q), M) then
        begin
          Result := I;
          exit;
        end else
        begin
          Inc(P);
          Inc(I);
        end;
  Result := 0;
end;
{$ENDIF}

function PosStr(const F, S: String; const Index: Integer;
    const AsciiCaseSensitive: Boolean): Integer;
var P, Q    : PChar;
    L, M, I : Integer;
begin
  L := Length(S);
  M := Length(F);
  if (L = 0) or (Index > L) or (M = 0) or (M > L) then
    begin
      Result := 0;
      exit;
    end;
  Q := Pointer(F);
  if Index < 1 then
    I := 1
  else
    I := Index;
  P := Pointer(S);
  Inc(P, I - 1);
  Dec(L, M - 1);
  if AsciiCaseSensitive then
    while I <= L do
      if StrPMatch(P, Q, M) then
        begin
          Result := I;
          exit;
        end else
        begin
          Inc(P);
          Inc(I);
        end
  else
    while I <= L do
      if StrPMatchNoAsciiCase(P, Q, M) then
        begin
          Result := I;
          exit;
        end else
        begin
          Inc(P);
          Inc(I);
        end;
  Result := 0;
end;

{$IFDEF SupportAnsiString}
function PosStrRevA(const F, S: AnsiString; const Index: Integer;
    const AsciiCaseSensitive: Boolean): Integer;
var P, Q       : PAnsiChar;
    L, M, I, J : Integer;
begin
  L := Length(S);
  M := Length(F);
  if (L = 0) or (Index > L) or (M = 0) or (M > L) then
    begin
      Result := 0;
      exit;
    end;
  Q := Pointer(F);
  if Index < 1 then
    I := 1
  else
    I := Index;
  P := Pointer(S);
  Dec(L, M - 1);
  Inc(P, L - 1);
  J := L;
  if AsciiCaseSensitive then
    while J >= I do
      if StrPMatchB(Pointer(P), Pointer(Q), M) then
        begin
          Result := J;
          exit;
        end else
        begin
          Dec(P);
          Dec(J);
        end
  else
    while J >= I do
      if StrPMatchNoAsciiCaseB(Pointer(P), Pointer(Q), M) then
        begin
          Result := J;
          exit;
        end else
        begin
          Dec(P);
          Dec(J);
        end;
  Result := 0;
end;
{$ENDIF}

function PosStrRevB(const F, S: RawByteString; const Index: Integer;
    const AsciiCaseSensitive: Boolean): Integer;
var P, Q       : PByteChar;
    L, M, I, J : Integer;
begin
  L := Length(S);
  M := Length(F);
  if (L = 0) or (Index > L) or (M = 0) or (M > L) then
    begin
      Result := 0;
      exit;
    end;
  Q := Pointer(F);
  if Index < 1 then
    I := 1
  else
    I := Index;
  P := Pointer(S);
  Dec(L, M - 1);
  Inc(P, L - 1);
  J := L;
  if AsciiCaseSensitive then
    while J >= I do
      if StrPMatchB(P, Q, M) then
        begin
          Result := J;
          exit;
        end else
        begin
          Dec(P);
          Dec(J);
        end
  else
    while J >= I do
      if StrPMatchNoAsciiCaseB(P, Q, M) then
        begin
          Result := J;
          exit;
        end else
        begin
          Dec(P);
          Dec(J);
        end;
  Result := 0;
end;

function PosStrRevU(const F, S: UnicodeString; const Index: Integer;
    const AsciiCaseSensitive: Boolean): Integer;
var P, Q       : PWideChar;
    L, M, I, J : Integer;
begin
  L := Length(S);
  M := Length(F);
  if (L = 0) or (Index > L) or (M = 0) or (M > L) then
    begin
      Result := 0;
      exit;
    end;
  Q := Pointer(F);
  if Index < 1 then
    I := 1
  else
    I := Index;
  P := Pointer(S);
  Dec(L, M - 1);
  Inc(P, L - 1);
  J := L;
  if AsciiCaseSensitive then
    while J >= I do
      if StrPMatchW(P, Q, M) then
        begin
          Result := J;
          exit;
        end else
        begin
          Dec(P);
          Dec(J);
        end
  else
    while J >= I do
      if StrPMatchNoAsciiCaseW(P, Q, M) then
        begin
          Result := J;
          exit;
        end else
        begin
          Dec(P);
          Dec(J);
        end;
  Result := 0;
end;

function PosStrRev(const F, S: String; const Index: Integer;
    const AsciiCaseSensitive: Boolean): Integer;
var P, Q       : PChar;
    L, M, I, J : Integer;
begin
  L := Length(S);
  M := Length(F);
  if (L = 0) or (Index > L) or (M = 0) or (M > L) then
    begin
      Result := 0;
      exit;
    end;
  Q := Pointer(F);
  if Index < 1 then
    I := 1
  else
    I := Index;
  P := Pointer(S);
  Dec(L, M - 1);
  Inc(P, L - 1);
  J := L;
  if AsciiCaseSensitive then
    while J >= I do
      if StrPMatch(P, Q, M) then
        begin
          Result := J;
          exit;
        end else
        begin
          Dec(P);
          Dec(J);
        end
  else
    while J >= I do
      if StrPMatchNoAsciiCase(P, Q, M) then
        begin
          Result := J;
          exit;
        end else
        begin
          Dec(P);
          Dec(J);
        end;
  Result := 0;
end;

{$IFDEF SupportAnsiString}
function PosStrRevIdxA(const F, S: AnsiString; const Index: Integer;
    const AsciiCaseSensitive: Boolean): Integer;
var P, Q       : PAnsiChar;
    L, M, I, J : Integer;
begin
  L := Length(S);
  M := Length(F);
  if (L = 0) or (Index > L) or (M = 0) or (M > L) then
    begin
      Result := 0;
      exit;
    end;
  Q := Pointer(F);
  if Index < 1 then
    I := L
  else
    I := Index;
  P := Pointer(S);
  Inc(P, I - 1);
  J := I;
  if AsciiCaseSensitive then
    while J >= 1 do
      if StrPMatchB(Pointer(P), Pointer(Q), M) then
        begin
          Result := J;
          exit;
        end else
        begin
          Dec(P);
          Dec(J);
        end
  else
    while J >= 1 do
      if StrPMatchNoAsciiCaseB(Pointer(P), Pointer(Q), M) then
        begin
          Result := J;
          exit;
        end else
        begin
          Dec(P);
          Dec(J);
        end;
  Result := 0;
end;
{$ENDIF}

function PosStrRevIdxB(const F, S: RawByteString; const Index: Integer;
    const AsciiCaseSensitive: Boolean): Integer;
var P, Q       : PByteChar;
    L, M, I, J : Integer;
begin
  L := Length(S);
  M := Length(F);
  if (L = 0) or (Index > L) or (M = 0) or (M > L) then
    begin
      Result := 0;
      exit;
    end;
  Q := Pointer(F);
  if Index < 1 then
    I := L
  else
    I := Index;
  P := Pointer(S);
  Inc(P, I - 1);
  J := I;
  if AsciiCaseSensitive then
    while J >= 1 do
      if StrPMatchB(P, Q, M) then
        begin
          Result := J;
          exit;
        end else
        begin
          Dec(P);
          Dec(J);
        end
  else
    while J >= 1 do
      if StrPMatchNoAsciiCaseB(P, Q, M) then
        begin
          Result := J;
          exit;
        end else
        begin
          Dec(P);
          Dec(J);
        end;
  Result := 0;
end;

function PosStrRevIdxU(const F, S: UnicodeString; const Index: Integer;
    const AsciiCaseSensitive: Boolean): Integer;
var P, Q       : PWideChar;
    L, M, I, J : Integer;
begin
  L := Length(S);
  M := Length(F);
  if (L = 0) or (Index > L) or (M = 0) or (M > L) then
    begin
      Result := 0;
      exit;
    end;
  Q := Pointer(F);
  if Index < 1 then
    I := L
  else
    I := Index;
  P := Pointer(S);
  Inc(P, I - 1);
  J := I;
  if AsciiCaseSensitive then
    while J >= 1 do
      if StrPMatchW(P, Q, M) then
        begin
          Result := J;
          exit;
        end else
        begin
          Dec(P);
          Dec(J);
        end
  else
    while J >= 1 do
      if StrPMatchNoAsciiCaseW(P, Q, M) then
        begin
          Result := J;
          exit;
        end else
        begin
          Dec(P);
          Dec(J);
        end;
  Result := 0;
end;

function PosStrRevIdx(const F, S: String; const Index: Integer;
    const AsciiCaseSensitive: Boolean): Integer;
var P, Q       : PChar;
    L, M, I, J : Integer;
begin
  L := Length(S);
  M := Length(F);
  if (L = 0) or (Index > L) or (M = 0) or (M > L) then
    begin
      Result := 0;
      exit;
    end;
  Q := Pointer(F);
  if Index < 1 then
    I := L
  else
    I := Index;
  P := Pointer(S);
  Inc(P, I - 1);
  J := I;
  if AsciiCaseSensitive then
    while J >= 1 do
      if StrPMatch(P, Q, M) then
        begin
          Result := J;
          exit;
        end else
        begin
          Dec(P);
          Dec(J);
        end
  else
    while J >= 1 do
      if StrPMatchNoAsciiCase(P, Q, M) then
        begin
          Result := J;
          exit;
        end else
        begin
          Dec(P);
          Dec(J);
        end;
  Result := 0;
end;

{$IFDEF SupportAnsiString}
function PosNStrA(const F, S: AnsiString; const N: Integer;
    const Index: Integer; const AsciiCaseSensitive: Boolean): Integer;
var I, J, M: Integer;
begin
  Result := 0;
  if N <= 0 then
    exit;
  M := Length(F);
  if M = 0 then
    exit;
  J := Index;
  for I := 1 to N do
    begin
      Result := PosStrA(F, S, J, AsciiCaseSensitive);
      if Result = 0 then
        exit;
      J := Result + M;
    end;
end;
{$ENDIF}

function PosNStrB(const F, S: RawByteString; const N: Integer;
    const Index: Integer; const AsciiCaseSensitive: Boolean): Integer;
var I, J, M: Integer;
begin
  Result := 0;
  if N <= 0 then
    exit;
  M := Length(F);
  if M = 0 then
    exit;
  J := Index;
  for I := 1 to N do
    begin
      Result := PosStrB(F, S, J, AsciiCaseSensitive);
      if Result = 0 then
        exit;
      J := Result + M;
    end;
end;

function PosNStrU(const F, S: UnicodeString; const N: Integer;
    const Index: Integer; const AsciiCaseSensitive: Boolean): Integer;
var I, J, M: Integer;
begin
  Result := 0;
  if N <= 0 then
    exit;
  M := Length(F);
  if M = 0 then
    exit;
  J := Index;
  for I := 1 to N do
    begin
      Result := PosStrU(F, S, J, AsciiCaseSensitive);
      if Result = 0 then
        exit;
      J := Result + M;
    end;
end;

function PosNStr(const F, S: String; const N: Integer;
    const Index: Integer; const AsciiCaseSensitive: Boolean): Integer;
var I, J, M: Integer;
begin
  Result := 0;
  if N <= 0 then
    exit;
  M := Length(F);
  if M = 0 then
    exit;
  J := Index;
  for I := 1 to N do
    begin
      Result := PosStr(F, S, J, AsciiCaseSensitive);
      if Result = 0 then
        exit;
      J := Result + M;
    end;
end;



{                                                                              }
{ Copy variations                                                              }
{                                                                              }
{$IFDEF SupportAnsiString}
function CopyRangeA(const S: AnsiString; const StartIndex, StopIndex: Integer): AnsiString;
var L, I : Integer;
begin
  L := Length(S);
  if (StartIndex > StopIndex) or (StopIndex < 1) or (StartIndex > L) or (L = 0) then
    Result := ''
  else
    begin
      if StartIndex <= 1 then
        if StopIndex >= L then
          begin
            Result := S;
            exit;
          end
        else
          I := 1
      else
        I := StartIndex;
      Result := Copy(S, I, StopIndex - I + 1);
    end;
end;
{$ENDIF}

function CopyRangeB(const S: RawByteString; const StartIndex, StopIndex: Integer): RawByteString;
var L, I : Integer;
begin
  L := Length(S);
  if (StartIndex > StopIndex) or (StopIndex < 1) or (StartIndex > L) or (L = 0) then
    Result := ''
  else
    begin
      if StartIndex <= 1 then
        if StopIndex >= L then
          begin
            Result := S;
            exit;
          end
        else
          I := 1
      else
        I := StartIndex;
      Result := Copy(S, I, StopIndex - I + 1);
    end;
end;

function CopyRangeU(const S: UnicodeString; const StartIndex, StopIndex: Integer): UnicodeString;
var L, I : Integer;
begin
  L := Length(S);
  if (StartIndex > StopIndex) or (StopIndex < 1) or (StartIndex > L) or (L = 0) then
    Result := ''
  else
    begin
      if StartIndex <= 1 then
        if StopIndex >= L then
          begin
            Result := S;
            exit;
          end
        else
          I := 1
      else
        I := StartIndex;
      Result := Copy(S, I, StopIndex - I + 1);
    end;
end;

function CopyRange(const S: String; const StartIndex, StopIndex: Integer): String;
var L, I : Integer;
begin
  L := Length(S);
  if (StartIndex > StopIndex) or (StopIndex < 1) or (StartIndex > L) or (L = 0) then
    Result := ''
  else
    begin
      if StartIndex <= 1 then
        if StopIndex >= L then
          begin
            Result := S;
            exit;
          end
        else
          I := 1
      else
        I := StartIndex;
      Result := Copy(S, I, StopIndex - I + 1);
    end;
end;

{$IFDEF SupportAnsiString}
function CopyFromA(const S: AnsiString; const Index: Integer): AnsiString;
var L : Integer;
begin
  if Index <= 1 then
    Result := S
  else
    begin
      L := Length(S);
      if (L = 0) or (Index > L) then
        Result := ''
      else
        Result := Copy(S, Index, L - Index + 1);
    end;
end;
{$ENDIF}

function CopyFromB(const S: RawByteString; const Index: Integer): RawByteString;
var L : Integer;
begin
  if Index <= 1 then
    Result := S
  else
    begin
      L := Length(S);
      if (L = 0) or (Index > L) then
        Result := ''
      else
        Result := Copy(S, Index, L - Index + 1);
    end;
end;

function CopyFromU(const S: UnicodeString; const Index: Integer): UnicodeString;
var L : Integer;
begin
  if Index <= 1 then
    Result := S
  else
    begin
      L := Length(S);
      if (L = 0) or (Index > L) then
        Result := ''
      else
        Result := Copy(S, Index, L - Index + 1);
    end;
end;

function CopyFrom(const S: String; const Index: Integer): String;
var L : Integer;
begin
  if Index <= 1 then
    Result := S
  else
    begin
      L := Length(S);
      if (L = 0) or (Index > L) then
        Result := ''
      else
        Result := Copy(S, Index, L - Index + 1);
    end;
end;

{$IFDEF SupportAnsiString}
function CopyLeftA(const S: AnsiString; const Count: Integer): AnsiString;
var L : Integer;
begin
  L := Length(S);
  if (L = 0) or (Count <= 0) then
    Result := '' else
    if Count >= L then
      Result := S
    else
      Result := Copy(S, 1, Count);
end;
{$ENDIF}

function CopyLeftB(const S: RawByteString; const Count: Integer): RawByteString;
var L : Integer;
begin
  L := Length(S);
  if (L = 0) or (Count <= 0) then
    Result := '' else
    if Count >= L then
      Result := S
    else
      Result := Copy(S, 1, Count);
end;

function CopyLeftU(const S: UnicodeString; const Count: Integer): UnicodeString;
var L : Integer;
begin
  L := Length(S);
  if (L = 0) or (Count <= 0) then
    Result := '' else
    if Count >= L then
      Result := S
    else
      Result := Copy(S, 1, Count);
end;

function CopyLeft(const S: String; const Count: Integer): String;
var L : Integer;
begin
  L := Length(S);
  if (L = 0) or (Count <= 0) then
    Result := '' else
    if Count >= L then
      Result := S
    else
      Result := Copy(S, 1, Count);
end;

{$IFDEF SupportAnsiString}
function CopyRightA(const S: AnsiString; const Count: Integer): AnsiString;
var L : Integer;
begin
  L := Length(S);
  if (L = 0) or (Count <= 0) then
    Result := '' else
    if Count >= L then
      Result := S
    else
      Result := Copy(S, L - Count + 1, Count);
end;
{$ENDIF}

function CopyRightB(const S: RawByteString; const Count: Integer): RawByteString;
var L : Integer;
begin
  L := Length(S);
  if (L = 0) or (Count <= 0) then
    Result := '' else
    if Count >= L then
      Result := S
    else
      Result := Copy(S, L - Count + 1, Count);
end;

function CopyRightU(const S: UnicodeString; const Count: Integer): UnicodeString;
var L : Integer;
begin
  L := Length(S);
  if (L = 0) or (Count <= 0) then
    Result := '' else
    if Count >= L then
      Result := S
    else
      Result := Copy(S, L - Count + 1, Count);
end;

function CopyRight(const S: String; const Count: Integer): String;
var L : Integer;
begin
  L := Length(S);
  if (L = 0) or (Count <= 0) then
    Result := '' else
    if Count >= L then
      Result := S
    else
      Result := Copy(S, L - Count + 1, Count);
end;

{$IFDEF SupportAnsiString}
function CopyLeftEllipsedA(const S: AnsiString; const Count: Integer): AnsiString;
var L: Integer;
begin
  if Count < 0 then
    begin
      Result := S;
      exit;
    end;
  if Count = 0 then
    begin
      Result := '';
      exit;
    end;
  L := Length(S);
  if L <= Count then
    begin
      Result := S;
      exit;
    end;
  if Count <= 3 then
    begin
      Result := DupCharA(' ', Count);
      exit;
    end;
  Result := Copy(S, 1, Count - 3) + '...';
end;
{$ENDIF}

function CopyLeftEllipsedU(const S: UnicodeString; const Count: Integer): UnicodeString;
var L: Integer;
begin
  if Count < 0 then
    begin
      Result := S;
      exit;
    end;
  if Count = 0 then
    begin
      Result := '';
      exit;
    end;
  L := Length(S);
  if L <= Count then
    begin
      Result := S;
      exit;
    end;
  if Count <= 3 then
    begin
      Result := DupCharU(' ', Count);
      exit;
    end;
  Result := Copy(S, 1, Count - 3) + '...';
end;



{                                                                              }
{ CopyEx                                                                       }
{                                                                              }

{ TranslateStartStop translates Start, Stop parameters (negative values are    }
{ indexed from back of string) into StartIdx and StopIdx (relative to start).  }
{ Returns False if the Start, Stop does not specify a valid range.             }
function TranslateStart(const Len, Start: Integer; var StartIndex : Integer): Boolean;
begin
  if Len = 0 then
    Result := False
  else
    begin
      StartIndex := Start;
      if Start < 0 then
        Inc(StartIndex, Len + 1);
      if StartIndex > Len then
        Result := False
      else
        begin
          if StartIndex < 1 then
            StartIndex := 1;
          Result := True;
        end;
    end;
end;

function TranslateStartStop(const Len, Start, Stop: Integer; var StartIndex, StopIndex: Integer): Boolean;
begin
  if Len = 0 then
    Result := False
  else
    begin
      StartIndex := Start;
      if Start < 0 then
        Inc(StartIndex, Len + 1);
      StopIndex := Stop;
      if StopIndex < 0 then
        Inc(StopIndex, Len + 1);
      if (StopIndex < 1) or (StartIndex > Len) or (StopIndex < StartIndex) then
        Result := False
      else
        begin
          if StopIndex > Len then
            StopIndex:= Len;
          if StartIndex < 1 then
            StartIndex := 1;
          Result := True;
        end;
    end;
end;

{$IFDEF SupportAnsiString}
function CopyExA(const S: AnsiString; const Start, Count: Integer): AnsiString;
var I, L : Integer;
begin
  L := Length(S);
  if (Count < 0) or not TranslateStart(L, Start, I) then
    Result := '' else
    if (I = 1) and (Count >= L) then
      Result := S
    else
      Result := Copy(S, I, Count);
end;
{$ENDIF}

function CopyExB(const S: RawByteString; const Start, Count: Integer): RawByteString;
var I, L : Integer;
begin
  L := Length(S);
  if (Count < 0) or not TranslateStart(L, Start, I) then
    Result := '' else
    if (I = 1) and (Count >= L) then
      Result := S
    else
      Result := Copy(S, I, Count);
end;

function CopyExW(const S: String; const Start, Count: Integer): String;
var I, L : Integer;
begin
  L := Length(S);
  if (Count < 0) or not TranslateStart(L, Start, I) then
    Result := '' else
    if (I = 1) and (Count >= L) then
      Result := S
    else
      Result := Copy(S, I, Count);
end;

function CopyExU(const S: UnicodeString; const Start, Count: Integer): UnicodeString;
var I, L : Integer;
begin
  L := Length(S);
  if (Count < 0) or not TranslateStart(L, Start, I) then
    Result := '' else
    if (I = 1) and (Count >= L) then
      Result := S
    else
      Result := Copy(S, I, Count);
end;

function CopyEx(const S: String; const Start, Count: Integer): String;
var I, L : Integer;
begin
  L := Length(S);
  if (Count < 0) or not TranslateStart(L, Start, I) then
    Result := '' else
    if (I = 1) and (Count >= L) then
      Result := S
    else
      Result := Copy(S, I, Count);
end;

{$IFDEF SupportAnsiString}
function CopyRangeExA(const S: AnsiString; const Start, Stop: Integer): AnsiString;
var I, J, L : Integer;
begin
  L := Length(S);
  if not TranslateStartStop(L, Start, Stop, I, J) then
    Result := '' else
    if (I = 1) and (J = L) then
      Result := S
    else
      Result := Copy(S, I, J - I + 1);
end;
{$ENDIF}

function CopyRangeExB(const S: RawByteString; const Start, Stop: Integer): RawByteString;
var I, J, L : Integer;
begin
  L := Length(S);
  if not TranslateStartStop(L, Start, Stop, I, J) then
    Result := '' else
    if (I = 1) and (J = L) then
      Result := S
    else
      Result := Copy(S, I, J - I + 1);
end;

function CopyRangeExU(const S: UnicodeString; const Start, Stop: Integer): UnicodeString;
var I, J, L : Integer;
begin
  L := Length(S);
  if not TranslateStartStop(L, Start, Stop, I, J) then
    Result := '' else
    if (I = 1) and (J = L) then
      Result := S
    else
      Result := Copy(S, I, J - I + 1);
end;

function CopyRangeEx(const S: String; const Start, Stop: Integer): String;
var I, J, L : Integer;
begin
  L := Length(S);
  if not TranslateStartStop(L, Start, Stop, I, J) then
    Result := '' else
    if (I = 1) and (J = L) then
      Result := S
    else
      Result := Copy(S, I, J - I + 1);
end;

{$IFDEF SupportAnsiString}
function CopyFromExA(const S: AnsiString; const Start: Integer): AnsiString;
var I, L : Integer;
begin
  L := Length(S);
  if not TranslateStart(L, Start, I) then
    Result := '' else
    if I <= 1 then
      Result := S
    else
      Result := Copy(S, I, L - I + 1);
end;

function CopyFromExB(const S: RawByteString; const Start: Integer): RawByteString;
var I, L : Integer;
begin
  L := Length(S);
  if not TranslateStart(L, Start, I) then
    Result := '' else
    if I <= 1 then
      Result := S
    else
      Result := Copy(S, I, L - I + 1);
end;
{$ENDIF}

function CopyFromExU(const S: UnicodeString; const Start: Integer): UnicodeString;
var I, L : Integer;
begin
  L := Length(S);
  if not TranslateStart(L, Start, I) then
    Result := '' else
    if I <= 1 then
      Result := S
    else
      Result := Copy(S, I, L - I + 1);
end;

function CopyFromEx(const S: String; const Start: Integer): String;
var I, L : Integer;
begin
  L := Length(S);
  if not TranslateStart(L, Start, I) then
    Result := '' else
    if I <= 1 then
      Result := S
    else
      Result := Copy(S, I, L - I + 1);
end;



{                                                                              }
{ Trim                                                                         }
{                                                                              }
{$IFDEF SupportAnsiString}
function StrTrimLeftA(const S: AnsiString; const C: ByteCharSet): AnsiString;
var F, L : Integer;
begin
  L := Length(S);
  F := 1;
  while (F <= L) and (S[F] in C) do
    Inc(F);
  Result := CopyFromA(S, F);
end;
{$ENDIF}

function StrTrimLeftB(const S: RawByteString; const C: ByteCharSet): RawByteString;
var F, L : Integer;
begin
  L := Length(S);
  F := 1;
  while (F <= L) and (S[F] in C) do
    Inc(F);
  Result := CopyFromB(S, F);
end;

function StrTrimLeftU(const S: UnicodeString; const C: ByteCharSet): UnicodeString;
var F, L : Integer;
begin
  L := Length(S);
  F := 1;
  while (F <= L) and WideCharInCharSet(S[F], C) do
    Inc(F);
  Result := CopyFromU(S, F);
end;

function StrTrimLeftU(const S: UnicodeString; const C: TWideCharMatchFunction): UnicodeString;
var F, L : Integer;
begin
  L := Length(S);
  F := 1;
  while (F <= L) and C(S[F]) do
    Inc(F);
  Result := CopyFromU(S, F);
end;

function StrTrimLeft(const S: String; const C: ByteCharSet): String;
begin
  {$IFDEF StringIsUnicode}
  Result := StrTrimLeftU(S, C);
  {$ELSE}
  Result := StrTrimLeftA(S, C);
  {$ENDIF}
end;

{$IFDEF SupportAnsiString}
procedure StrTrimLeftInPlaceA(var S: AnsiString; const C: ByteCharSet);
var F, L : Integer;
    P    : PAnsiChar;
begin
  L := Length(S);
  F := 1;
  while (F <= L) and (S[F] in C) do
    Inc(F);
  if F > L then
    S := '' else
    if F > 1 then
      begin
        L := L - F + 1;
        if L > 0 then
          begin
            P := Pointer(S);
            Inc(P, F - 1);
            MoveMem(P^, Pointer(S)^, L);
          end;
        SetLength(S, L);
      end;
end;
{$ENDIF}

procedure StrTrimLeftInPlaceB(var S: RawByteString; const C: ByteCharSet);
var F, L : Integer;
    P    : PByteChar;
begin
  L := Length(S);
  F := 1;
  while (F <= L) and (S[F] in C) do
    Inc(F);
  if F > L then
    S := '' else
    if F > 1 then
      begin
        L := L - F + 1;
        if L > 0 then
          begin
            P := Pointer(S);
            Inc(P, F - 1);
            MoveMem(P^, Pointer(S)^, L);
          end;
        SetLength(S, L);
      end;
end;

procedure StrTrimLeftInPlaceU(var S: UnicodeString; const C: ByteCharSet);
var F, L : Integer;
    P    : PWideChar;
begin
  L := Length(S);
  F := 1;
  while (F <= L) and CharSetMatchCharW(C, S[F], True) do
    Inc(F);
  if F > L then
    S := '' else
    if F > 1 then
      begin
        L := L - F + 1;
        if L > 0 then
          begin
            P := Pointer(S);
            Inc(P, F - 1);
            MoveMem(P^, Pointer(S)^, L * SizeOf(WideChar));
          end;
        SetLength(S, L);
      end;
end;

procedure StrTrimLeftInPlaceU(var S: UnicodeString; const C: TWideCharMatchFunction);
var F, L : Integer;
    P    : PWideChar;
begin
  L := Length(S);
  F := 1;
  while (F <= L) and C(S[F]) do
    Inc(F);
  if F > L then
    S := '' else
    if F > 1 then
      begin
        L := L - F + 1;
        if L > 0 then
          begin
            P := Pointer(S);
            Inc(P, F - 1);
            MoveMem(P^, Pointer(S)^, L * SizeOf(WideChar));
          end;
        SetLength(S, L);
      end;
end;

procedure StrTrimLeftInPlace(var S: String; const C: ByteCharSet);
var F, L : Integer;
    P    : PChar;
begin
  L := Length(S);
  F := 1;
  while (F <= L) and CharSetMatchChar(C, S[F], True) do
    Inc(F);
  if F > L then
    S := '' else
    if F > 1 then
      begin
        L := L - F + 1;
        if L > 0 then
          begin
            P := Pointer(S);
            Inc(P, F - 1);
            MoveMem(P^, Pointer(S)^, L * SizeOf(Char));
          end;
        SetLength(S, L);
      end;
end;

{$IFDEF SupportAnsiString}
function StrTrimLeftStrNoCaseA(const S: AnsiString; const TrimStr: AnsiString): AnsiString;
var F, L, M : Integer;
begin
  L := Length(TrimStr);
  M := Length(S);
  F := 1;
  while (F <= M) and StrMatchNoAsciiCaseA(S, TrimStr, F) do
    Inc(F, L);
  Result := CopyFromA(S, F);
end;
{$ENDIF}

function StrTrimLeftStrNoCaseB(const S: RawByteString; const TrimStr: RawByteString): RawByteString;
var F, L, M : Integer;
begin
  L := Length(TrimStr);
  M := Length(S);
  F := 1;
  while (F <= M) and StrMatchNoAsciiCaseB(S, TrimStr, F) do
    Inc(F, L);
  Result := CopyFromB(S, F);
end;

function StrTrimLeftStrNoCaseU(const S: UnicodeString; const TrimStr: UnicodeString): UnicodeString;
var F, L, M : Integer;
begin
  L := Length(TrimStr);
  M := Length(S);
  F := 1;
  while (F <= M) and StrMatchNoAsciiCaseU(S, TrimStr, F) do
    Inc(F, L);
  Result := CopyFromU(S, F);
end;

function StrTrimLeftStrNoCase(const S: String; const TrimStr: String): String;
var F, L, M : Integer;
begin
  L := Length(TrimStr);
  M := Length(S);
  F := 1;
  while (F <= M) and StrMatchNoAsciiCase(S, TrimStr, F) do
    Inc(F, L);
  Result := CopyFrom(S, F);
end;

{$IFDEF SupportAnsiString}
function StrTrimRightA(const S: AnsiString; const C: ByteCharSet): AnsiString;
var F : Integer;
begin
  F := Length(S);
  while (F >= 1) and (S[F] in C) do
    Dec(F);
  Result := CopyLeftA(S, F);
end;
{$ENDIF}

function StrTrimRightB(const S: RawByteString; const C: ByteCharSet): RawByteString;
var F : Integer;
begin
  F := Length(S);
  while (F >= 1) and (S[F] in C) do
    Dec(F);
  Result := CopyLeftB(S, F);
end;

function StrTrimRightU(const S: UnicodeString; const C: ByteCharSet): UnicodeString;
var F : Integer;
begin
  F := Length(S);
  while (F >= 1) and WideCharInCharSet(S[F], C) do
    Dec(F);
  Result := CopyLeftU(S, F);
end;

function StrTrimRightU(const S: UnicodeString; const C: TWideCharMatchFunction): UnicodeString;
var F : Integer;
begin
  F := Length(S);
  while (F >= 1) and C(S[F]) do
    Dec(F);
  Result := CopyLeftU(S, F);
end;

function StrTrimRight(const S: String; const C: ByteCharSet): String;
begin
  {$IFDEF StringIsUnicode}
  Result := StrTrimRightU(S, C);
  {$ELSE}
  Result := StrTrimRightA(S, C);
  {$ENDIF}
end;

{$IFDEF SupportAnsiString}
procedure StrTrimRightInPlaceA(var S: AnsiString; const C: ByteCharSet);
var F : Integer;
begin
  F := Length(S);
  while (F >= 1) and (S[F] in C) do
    Dec(F);
  if F = 0 then
    S := ''
  else
    SetLength(S, F);
end;
{$ENDIF}

procedure StrTrimRightInPlaceB(var S: RawByteString; const C: ByteCharSet);
var F : Integer;
begin
  F := Length(S);
  while (F >= 1) and (S[F] in C) do
    Dec(F);
  if F = 0 then
    S := ''
  else
    SetLength(S, F);
end;

procedure StrTrimRightInPlaceU(var S: UnicodeString; const C: ByteCharSet);
var F : Integer;
begin
  F := Length(S);
  while (F >= 1) and CharSetMatchCharW(C, S[F], True) do
    Dec(F);
  if F = 0 then
    S := ''
  else
    SetLength(S, F);
end;

procedure StrTrimRightInPlaceU(var S: UnicodeString; const C: TWideCharMatchFunction);
var F : Integer;
begin
  F := Length(S);
  while (F >= 1) and C(S[F]) do
    Dec(F);
  if F = 0 then
    S := ''
  else
    SetLength(S, F);
end;

procedure StrTrimRightInPlace(var S: String; const C: ByteCharSet);
var F : Integer;
begin
  F := Length(S);
  while (F >= 1) and CharSetMatchChar(C, S[F], True) do
    Dec(F);
  if F = 0 then
    S := ''
  else
    SetLength(S, F);
end;

{$IFDEF SupportAnsiString}
function StrTrimRightStrNoCaseA(const S: AnsiString; const TrimStr: AnsiString): AnsiString;
var F, L : Integer;
begin
  L := Length(TrimStr);
  F := Length(S) - L  + 1;
  while (F >= 1) and StrMatchNoAsciiCaseA(S, TrimStr, F) do
    Dec(F, L);
  Result := CopyLeftA(S, F + L - 1);
end;
{$ENDIF}

function StrTrimRightStrNoCaseB(const S: RawByteString; const TrimStr: RawByteString): RawByteString;
var F, L : Integer;
begin
  L := Length(TrimStr);
  F := Length(S) - L  + 1;
  while (F >= 1) and StrMatchNoAsciiCaseB(S, TrimStr, F) do
    Dec(F, L);
  Result := CopyLeftB(S, F + L - 1);
end;

function StrTrimRightStrNoCaseU(const S: UnicodeString; const TrimStr: UnicodeString): UnicodeString;
var F, L : Integer;
begin
  L := Length(TrimStr);
  F := Length(S) - L  + 1;
  while (F >= 1) and StrMatchNoAsciiCaseU(S, TrimStr, F) do
    Dec(F, L);
  Result := CopyLeftU(S, F + L - 1);
end;

function StrTrimRightStrNoCase(const S: String; const TrimStr: String): String;
var F, L : Integer;
begin
  L := Length(TrimStr);
  F := Length(S) - L  + 1;
  while (F >= 1) and StrMatchNoAsciiCase(S, TrimStr, F) do
    Dec(F, L);
  Result := CopyLeft(S, F + L - 1);
end;

{$IFDEF SupportAnsiString}
function StrTrimA(const S: AnsiString; const C: ByteCharSet): AnsiString;
var F, G, L : Integer;
begin
  L := Length(S);
  F := 1;
  while (F <= L) and (S[F] in C) do
    Inc(F);
  G := L;
  while (G >= F) and (S[G] in C) do
    Dec(G);
  Result := CopyRangeA(S, F, G);
end;
{$ENDIF}

function StrTrimB(const S: RawByteString; const C: ByteCharSet): RawByteString;
var F, G, L : Integer;
begin
  L := Length(S);
  F := 1;
  while (F <= L) and (S[F] in C) do
    Inc(F);
  G := L;
  while (G >= F) and (S[G] in C) do
    Dec(G);
  Result := CopyRangeB(S, F, G);
end;

function StrTrimU(const S: UnicodeString; const C: ByteCharSet): UnicodeString;
var F, G, L : Integer;
begin
  L := Length(S);
  F := 1;
  while (F <= L) and WideCharInCharSet(S[F], C) do
    Inc(F);
  G := L;
  while (G >= F) and WideCharInCharSet(S[G], C) do
    Dec(G);
  Result := CopyRangeU(S, F, G);
end;

function StrTrimU(const S: UnicodeString; const C: TWideCharMatchFunction): UnicodeString;
var F, G, L : Integer;
begin
  L := Length(S);
  F := 1;
  while (F <= L) and C(S[F]) do
    Inc(F);
  G := L;
  while (G >= F) and C(S[G]) do
    Dec(G);
  Result := CopyRangeU(S, F, G);
end;

function StrTrim(const S: String; const C: ByteCharSet): String;
begin
  {$IFDEF StringIsUnicode}
  Result := StrTrimU(S, C);
  {$ELSE}
  Result := StrTrimA(S, C);
  {$ENDIF}
end;

{$IFDEF SupportAnsiString}
procedure StrTrimInPlaceA(var S: AnsiString; const C: ByteCharSet);
begin
  StrTrimLeftInPlaceA(S, C);
  StrTrimRightInPlaceA(S, C);
end;
{$ENDIF}

procedure StrTrimInPlaceB(var S: RawByteString; const C: ByteCharSet);
begin
  StrTrimLeftInPlaceB(S, C);
  StrTrimRightInPlaceB(S, C);
end;

procedure StrTrimInPlaceU(var S : UnicodeString; const C: ByteCharSet);
begin
  StrTrimLeftInPlaceU(S, C);
  StrTrimRightInPlaceU(S, C);
end;

procedure StrTrimInPlaceU(var S: UnicodeString; const C: TWideCharMatchFunction);
begin
  StrTrimLeftInPlaceU(S, C);
  StrTrimRightInPlaceU(S, C);
end;

procedure StrTrimInPlace(var S : String; const C: ByteCharSet);
begin
  StrTrimLeftInPlace(S, C);
  StrTrimRightInPlace(S, C);
end;

{$IFDEF SupportAnsiString}
procedure TrimStringsA(var S : AnsiStringArray; const C: ByteCharSet);
var I : Integer;
begin
  for I := 0 to Length(S) - 1 do
    StrTrimInPlaceA(S[I], C);
end;
{$ENDIF}

procedure TrimStringsB(var S : RawByteStringArray; const C: ByteCharSet);
var I : Integer;
begin
  for I := 0 to Length(S) - 1 do
    StrTrimInPlaceB(S[I], C);
end;

procedure TrimStringsU(var S : UnicodeStringArray; const C: ByteCharSet);
var I : Integer;
begin
  for I := 0 to Length(S) - 1 do
    StrTrimInPlaceU(S[I], C);
end;

procedure TrimStrings(var S : StringArray; const C: ByteCharSet);
var I : Integer;
begin
  for I := 0 to Length(S) - 1 do
    StrTrimInPlace(S[I], C);
end;



{                                                                              }
{ Dup                                                                          }
{                                                                              }
{$IFDEF SupportAnsiString}
function BufToStrA(const Buf; const BufSize: Integer): AnsiString;
begin
  if BufSize <= 0 then
    Result := ''
  else
    begin
      SetLength(Result, BufSize);
      MoveMem(Buf, Pointer(Result)^, BufSize);
    end;
end;
{$ENDIF}

function BufToStrB(const Buf; const BufSize: Integer): RawByteString;
begin
  if BufSize <= 0 then
    Result := ''
  else
    begin
      SetLength(Result, BufSize);
      MoveMem(Buf, Pointer(Result)^, BufSize);
    end;
end;

function BufToStrU(const Buf; const BufSize: Integer): UnicodeString;
var L : Integer;
begin
  if BufSize <= 0 then
    Result := ''
  else
    begin
      L := (BufSize + 1) div SizeOf(WideChar);
      SetLength(Result, L);
      MoveMem(Buf, Pointer(Result)^, BufSize);
    end;
end;

function BufToStr(const Buf; const BufSize: Integer): String;
var L : Integer;
begin
  if BufSize <= 0 then
    Result := ''
  else
    begin
      {$IFDEF StringIsUnicode}
      L := (BufSize + 1) div SizeOf(Char);
      {$ELSE}
      L := BufSize;
      {$ENDIF}
      SetLength(Result, L);
      MoveMem(Buf, Pointer(Result)^, BufSize);
    end;
end;

{$IFDEF SupportAnsiString}
function DupBufA(const Buf; const BufSize: Integer; const Count: Integer): AnsiString;
var P : PAnsiChar;
    I : Integer;
begin
  if (Count <= 0) or (BufSize <= 0) then
    Result := ''
  else
    begin
      SetLength(Result, Count * BufSize);
      P := Pointer(Result);
      for I := 1 to Count do
        begin
          MoveMem(Buf, P^, BufSize);
          Inc(P, BufSize);
        end;
    end;
end;
{$ENDIF}

function DupBufB(const Buf; const BufSize: Integer; const Count: Integer): RawByteString;
var P : PByteChar;
    I : Integer;
begin
  if (Count <= 0) or (BufSize <= 0) then
    Result := ''
  else
    begin
      SetLength(Result, Count * BufSize);
      P := Pointer(Result);
      for I := 1 to Count do
        begin
          MoveMem(Buf, P^, BufSize);
          Inc(P, BufSize);
        end;
    end;
end;

function DupBufU(const Buf; const BufSize: Integer; const Count: Integer): UnicodeString;
var P : PWideChar;
    I, L : Integer;
begin
  if (Count <= 0) or (BufSize <= 0) then
    Result := ''
  else
    begin
      Assert(BufSize mod SizeOf(WideChar) = 0);
      L := BufSize div SizeOf(WideChar);
      SetLength(Result, Count * L);
      P := Pointer(Result);
      for I := 1 to Count do
        begin
          MoveMem(Buf, P^, BufSize);
          Inc(P, L);
        end;
    end;
end;

function DupBuf(const Buf; const BufSize: Integer; const Count: Integer): String;
var P : PChar;
    I, L : Integer;
begin
  if (Count <= 0) or (BufSize <= 0) then
    Result := ''
  else
    begin
      {$IFDEF StringIsUnicode}
      Assert(BufSize mod SizeOf(Char) = 0);
      L := BufSize div SizeOf(WideChar);
      {$ELSE}
      L := BufSize;
      {$ENDIF}
      SetLength(Result, Count * L);
      P := Pointer(Result);
      for I := 1 to Count do
        begin
          MoveMem(Buf, P^, BufSize);
          Inc(P, L);
        end;
    end;
end;

{$IFDEF SupportAnsiString}
function DupStrA(const S: AnsiString; const Count: Integer): AnsiString;
var L : Integer;
begin
  L := Length(S);
  if L = 0 then
    Result := ''
  else
    Result := DupBufA(Pointer(S)^, L, Count);
end;
{$ENDIF}

function DupStrB(const S: RawByteString; const Count: Integer): RawByteString;
var L : Integer;
begin
  L := Length(S);
  if L = 0 then
    Result := ''
  else
    Result := DupBufB(Pointer(S)^, L, Count);
end;

function DupStrU(const S: UnicodeString; const Count: Integer): UnicodeString;
var L : Integer;
begin
  L := Length(S);
  if L = 0 then
    Result := ''
  else
    Result := DupBufU(Pointer(S)^, L * SizeOf(WideChar), Count);
end;

function DupStr(const S: String; const Count: Integer): String;
var L : Integer;
begin
  L := Length(S);
  if L = 0 then
    Result := ''
  else
    Result := DupBuf(Pointer(S)^, L * SizeOf(Char), Count);
end;

{$IFDEF SupportAnsiString}
function DupCharA(const Ch: AnsiChar; const Count: Integer): AnsiString;
begin
  if Count <= 0 then
    begin
      Result := '';
      exit;
    end;
  SetLength(Result, Count);
  FillMem(Pointer(Result)^, Count, Ord(Ch));
end;
{$ENDIF}

function DupCharB(const Ch: ByteChar; const Count: Integer): RawByteString;
begin
  if Count <= 0 then
    begin
      Result := '';
      exit;
    end;
  SetLength(Result, Count);
  FillMem(Pointer(Result)^, Count, Ord(Ch));
end;

function DupCharU(const Ch: WideChar; const Count: Integer): UnicodeString;
var I : Integer;
begin
  if Count <= 0 then
    begin
      Result := '';
      exit;
    end;
  SetLength(Result, Count);
  for I := 1 to Count do
    Result[I] := Ch;
end;

function DupChar(const Ch: Char; const Count: Integer): String;
var I : Integer;
begin
  if Count <= 0 then
    begin
      Result := '';
      exit;
    end;
  SetLength(Result, Count);
  for I := 1 to Count do
    Result[I] := Ch;
end;

{$IFDEF SupportAnsiString}
function DupSpaceA(const Count: Integer): AnsiString;
begin
  Result := DupCharA(AsciiSP, Count);
end;
{$ENDIF}

function DupSpaceB(const Count: Integer): RawByteString;
begin
  Result := DupCharB(AsciiSP, Count);
end;

function DupSpaceU(const Count: Integer): UnicodeString;
begin
  Result := DupCharU(WideSP, Count);
end;

function DupSpace(const Count: Integer): String;
begin
  Result := DupChar(' ', Count);
end;



{                                                                              }
{ Pad                                                                          }
{                                                                              }
{$IFDEF SupportAnsiString}
function StrPadLeftA(const S: AnsiString; const PadChar: AnsiChar;
    const Len: Integer; const Cut: Boolean): AnsiString;
var F, L, P, M : Integer;
    I, J       : PAnsiChar;
begin
  if Len = 0 then
    begin
      if Cut then
        Result := ''
      else
        Result := S;
      exit;
    end;
  M := Length(S);
  if Len = M then
    begin
      Result := S;
      exit;
    end;
  if Cut then
    L := Len
  else
    L := MaxInt(Len, M);
  P := L - M;
  if P < 0 then
    P := 0;
  SetLength(Result, L);
  if P > 0 then
    FillMem(Pointer(Result)^, P, Ord(PadChar));
  if L > P then
    begin
      I := Pointer(Result);
      J := Pointer(S);
      Inc(I, P);
      for F := 1 to L - P do
        begin
          I^ := J^;
          Inc(I);
          Inc(J);
        end;
    end;
end;
{$ENDIF}

function StrPadLeftB(const S: RawByteString; const PadChar: ByteChar;
    const Len: Integer; const Cut: Boolean): RawByteString;
var F, L, P, M : Integer;
    I, J       : PByteChar;
begin
  if Len = 0 then
    begin
      if Cut then
        Result := ''
      else
        Result := S;
      exit;
    end;
  M := Length(S);
  if Len = M then
    begin
      Result := S;
      exit;
    end;
  if Cut then
    L := Len
  else
    L := MaxInt(Len, M);
  P := L - M;
  if P < 0 then
    P := 0;
  SetLength(Result, L);
  if P > 0 then
    FillMem(Pointer(Result)^, P, Ord(PadChar));
  if L > P then
    begin
      I := Pointer(Result);
      J := Pointer(S);
      Inc(I, P);
      for F := 1 to L - P do
        begin
          I^ := J^;
          Inc(I);
          Inc(J);
        end;
    end;
end;

function StrPadLeftU(const S: UnicodeString; const PadChar: WideChar;
    const Len: Integer; const Cut: Boolean): UnicodeString;
var F, L, P, M : Integer;
    I, J       : PWideChar;
begin
  if Len = 0 then
    begin
      if Cut then
        Result := ''
      else
        Result := S;
      exit;
    end;
  M := Length(S);
  if Len = M then
    begin
      Result := S;
      exit;
    end;
  if Cut then
    L := Len
  else
    L := MaxInt(Len, M);
  P := L - M;
  if P < 0 then
    P := 0;
  SetLength(Result, L);
  for F := 1 to P do
    Result[F] := PadChar;
  if L > P then
    begin
      I := Pointer(Result);
      J := Pointer(S);
      Inc(I, P);
      for F := 1 to L - P do
        begin
          I^ := J^;
          Inc(I);
          Inc(J);
        end;
    end;
end;

function StrPadLeft(const S: String; const PadChar: Char;
    const Len: Integer; const Cut: Boolean): String;
var F, L, P, M : Integer;
    I, J       : PChar;
begin
  if Len = 0 then
    begin
      if Cut then
        Result := ''
      else
        Result := S;
      exit;
    end;
  M := Length(S);
  if Len = M then
    begin
      Result := S;
      exit;
    end;
  if Cut then
    L := Len
  else
    L := MaxInt(Len, M);
  P := L - M;
  if P < 0 then
    P := 0;
  SetLength(Result, L);
  for F := 1 to P do
    Result[F] := PadChar;
  if L > P then
    begin
      I := Pointer(Result);
      J := Pointer(S);
      Inc(I, P);
      for F := 1 to L - P do
        begin
          I^ := J^;
          Inc(I);
          Inc(J);
        end;
    end;
end;

{$IFDEF SupportAnsiString}
function StrPadRightA(const S: AnsiString; const PadChar: AnsiChar;
    const Len: Integer; const Cut: Boolean): AnsiString;
var F, L, P, M : Integer;
    I, J       : PAnsiChar;
begin
  if Len = 0 then
    begin
      if Cut then
        Result := ''
      else
        Result := S;
      exit;
    end;
  M := Length(S);
  if Len = M then
    begin
      Result := S;
      exit;
    end;
  if Cut then
    L := Len
  else
    L := MaxInt(Len, M);
  P := L - M;
  if P < 0 then
    P := 0;
  SetLength(Result, L);
  if L > P then
    begin
      I := Pointer(Result);
      J := Pointer(S);
      for F := 1 to L - P do
        begin
          I^ := J^;
          Inc(I);
          Inc(J);
        end;
    end;
  if P > 0 then
    FillMem(Result[L - P + 1], P, Ord(PadChar));
end;
{$ENDIF}

function StrPadRightB(const S: RawByteString; const PadChar: ByteChar;
    const Len: Integer; const Cut: Boolean): RawByteString;
var F, L, P, M : Integer;
    I, J       : PByteChar;
begin
  if Len = 0 then
    begin
      if Cut then
        Result := ''
      else
        Result := S;
      exit;
    end;
  M := Length(S);
  if Len = M then
    begin
      Result := S;
      exit;
    end;
  if Cut then
    L := Len
  else
    L := MaxInt(Len, M);
  P := L - M;
  if P < 0 then
    P := 0;
  SetLength(Result, L);
  if L > P then
    begin
      I := Pointer(Result);
      J := Pointer(S);
      for F := 1 to L - P do
        begin
          I^ := J^;
          Inc(I);
          Inc(J);
        end;
    end;
  if P > 0 then
    FillMem(Result[L - P + 1], P, Ord(PadChar));
end;

function StrPadRightU(const S: UnicodeString; const PadChar: WideChar;
    const Len: Integer; const Cut: Boolean): UnicodeString;
var F, L, P, M : Integer;
    I, J       : PWideChar;
begin
  if Len = 0 then
    begin
      if Cut then
        Result := ''
      else
        Result := S;
      exit;
    end;
  M := Length(S);
  if Len = M then
    begin
      Result := S;
      exit;
    end;
  if Cut then
    L := Len
  else
    L := MaxInt(Len, M);
  P := L - M;
  if P < 0 then
    P := 0;
  SetLength(Result, L);
  if L > P then
    begin
      I := Pointer(Result);
      J := Pointer(S);
      for F := 1 to L - P do
        begin
          I^ := J^;
          Inc(I);
          Inc(J);
        end;
    end;
  for F := L - P + 1 to L do
    Result[F] := PadChar;
end;

function StrPadRight(const S: String; const PadChar: Char;
    const Len: Integer; const Cut: Boolean): String;
var F, L, P, M : Integer;
    I, J       : PChar;
begin
  if Len = 0 then
    begin
      if Cut then
        Result := ''
      else
        Result := S;
      exit;
    end;
  M := Length(S);
  if Len = M then
    begin
      Result := S;
      exit;
    end;
  if Cut then
    L := Len
  else
    L := MaxInt(Len, M);
  P := L - M;
  if P < 0 then
    P := 0;
  SetLength(Result, L);
  if L > P then
    begin
      I := Pointer(Result);
      J := Pointer(S);
      for F := 1 to L - P do
        begin
          I^ := J^;
          Inc(I);
          Inc(J);
        end;
    end;
  for F := L - P + 1 to L do
    Result[F] := PadChar;
end;

{$IFDEF SupportAnsiString}
function StrPadA(const S: AnsiString; const PadChar: AnsiChar; const Len: Integer;
    const Cut: Boolean): AnsiString;
var I : Integer;
begin
  I := Len - Length(S);
  Result := DupCharA(PadChar, I div 2) + S + DupCharA(PadChar, (I + 1) div 2);
  if Cut then
    SetLength(Result, Len);
end;
{$ENDIF}

function StrPadB(const S: RawByteString; const PadChar: ByteChar; const Len: Integer;
    const Cut: Boolean): RawByteString;
var I : Integer;
begin
  I := Len - Length(S);
  Result := DupCharB(PadChar, I div 2) + S + DupCharB(PadChar, (I + 1) div 2);
  if Cut then
    SetLength(Result, Len);
end;

function StrPadU(const S: UnicodeString; const PadChar: WideChar; const Len: Integer;
    const Cut: Boolean): UnicodeString;
var I : Integer;
begin
  I := Len - Length(S);
  Result := DupCharU(PadChar, I div 2) + S + DupCharU(PadChar, (I + 1) div 2);
  if Cut then
    SetLength(Result, Len);
end;

function StrPad(const S: String; const PadChar: Char; const Len: Integer;
    const Cut: Boolean): String;
var I : Integer;
begin
  I := Len - Length(S);
  Result := DupChar(PadChar, I div 2) + S + DupChar(PadChar, (I + 1) div 2);
  if Cut then
    SetLength(Result, Len);
end;



{                                                                              }
{ Delimited                                                                    }
{                                                                              }
{$IFDEF SupportAnsiString}
function StrBetweenCharA(const S: AnsiString;
    const FirstDelim, SecondDelim: AnsiChar;
    const FirstOptional: Boolean; const SecondOptional: Boolean): AnsiString;
var I, J : Integer;
begin
  Result := '';
  I := PosCharA(FirstDelim, S);
  if (I = 0) and not FirstOptional then
    exit;
  J := PosCharA(SecondDelim, S, I + 1);
  if J = 0 then
    if not SecondOptional then
      exit
    else
      J := Length(S) + 1;
  Result := CopyRangeA(S, I + 1, J - 1);
end;

function StrBetweenCharA(const S: AnsiString;
    const FirstDelim, SecondDelim: ByteCharSet;
    const FirstOptional: Boolean; const SecondOptional: Boolean): AnsiString;
var I, J : Integer;
begin
  Result := '';
  I := PosCharSetA(FirstDelim, S);
  if (I = 0) and not FirstOptional then
    exit;
  J := PosCharSetA(SecondDelim, S, I + 1);
  if J = 0 then
    if not SecondOptional then
      exit
    else
      J := Length(S) + 1;
  Result := CopyRangeA(S, I + 1, J - 1);
end;

function StrBetweenCharB(const S: RawByteString;
    const FirstDelim, SecondDelim: ByteChar;
    const FirstOptional: Boolean; const SecondOptional: Boolean): RawByteString;
var I, J : Integer;
begin
  Result := '';
  I := PosCharB(FirstDelim, S);
  if (I = 0) and not FirstOptional then
    exit;
  J := PosCharB(SecondDelim, S, I + 1);
  if J = 0 then
    if not SecondOptional then
      exit
    else
      J := Length(S) + 1;
  Result := CopyRangeB(S, I + 1, J - 1);
end;

function StrBetweenCharB(const S: RawByteString;
    const FirstDelim, SecondDelim: ByteCharSet;
    const FirstOptional: Boolean; const SecondOptional: Boolean): RawByteString;
var I, J : Integer;
begin
  Result := '';
  I := PosCharSetB(FirstDelim, S);
  if (I = 0) and not FirstOptional then
    exit;
  J := PosCharSetB(SecondDelim, S, I + 1);
  if J = 0 then
    if not SecondOptional then
      exit
    else
      J := Length(S) + 1;
  Result := CopyRangeB(S, I + 1, J - 1);
end;
{$ENDIF}

function StrBetweenCharU(const S: UnicodeString;
    const FirstDelim, SecondDelim: WideChar;
    const FirstOptional: Boolean; const SecondOptional: Boolean): UnicodeString;
var I, J : Integer;
begin
  Result := '';
  I := PosCharU(FirstDelim, S);
  if (I = 0) and not FirstOptional then
    exit;
  J := PosCharU(SecondDelim, S, I + 1);
  if J = 0 then
    if not SecondOptional then
      exit
    else
      J := Length(S) + 1;
  Result := CopyRangeU(S, I + 1, J - 1);
end;

function StrBetweenCharU(const S: UnicodeString;
    const FirstDelim, SecondDelim: ByteCharSet;
    const FirstOptional: Boolean; const SecondOptional: Boolean): UnicodeString;
var I, J : Integer;
begin
  Result := '';
  I := PosCharSetU(FirstDelim, S);
  if (I = 0) and not FirstOptional then
    exit;
  J := PosCharSetU(SecondDelim, S, I + 1);
  if J = 0 then
    if not SecondOptional then
      exit
    else
      J := Length(S) + 1;
  Result := CopyRangeU(S, I + 1, J - 1);
end;

function StrBetweenChar(const S: String;
    const FirstDelim, SecondDelim: Char;
    const FirstOptional: Boolean; const SecondOptional: Boolean): String;
var I, J : Integer;
begin
  Result := '';
  I := PosChar(FirstDelim, S);
  if (I = 0) and not FirstOptional then
    exit;
  J := PosChar(SecondDelim, S, I + 1);
  if J = 0 then
    if not SecondOptional then
      exit
    else
      J := Length(S) + 1;
  Result := CopyRange(S, I + 1, J - 1);
end;

function StrBetweenChar(const S: String;
    const FirstDelim, SecondDelim: ByteCharSet;
    const FirstOptional: Boolean; const SecondOptional: Boolean): String;
var I, J : Integer;
begin
  Result := '';
  I := PosCharSet(FirstDelim, S);
  if (I = 0) and not FirstOptional then
    exit;
  J := PosCharSet(SecondDelim, S, I + 1);
  if J = 0 then
    if not SecondOptional then
      exit
    else
      J := Length(S) + 1;
  Result := CopyRange(S, I + 1, J - 1);
end;

{$IFDEF SupportAnsiString}
function StrBetweenA(const S: AnsiString; const FirstDelim: AnsiString;
    const SecondDelim: ByteCharSet; const FirstOptional: Boolean;
    const SecondOptional: Boolean;
    const FirstDelimAsciiCaseSensitive: Boolean): AnsiString;
var I, J : Integer;
begin
  Result := '';
  I := PosStrA(FirstDelim, S, 1, FirstDelimAsciiCaseSensitive);
  if (I = 0) and not FirstOptional then
    exit;
  Inc(I, Length(FirstDelim));
  J := PosCharSetA(SecondDelim, S, I);
  if J = 0 then
    if not SecondOptional then
      exit
    else
      J := Length(S) + 1;
  Result := CopyRangeA(S, I, J - 1);
end;

function StrBetweenA(const S: AnsiString;
    const FirstDelim, SecondDelim: AnsiString; const FirstOptional: Boolean;
    const SecondOptional: Boolean ; const FirstDelimAsciiCaseSensitive: Boolean;
    const SecondDelimAsciiCaseSensitive: Boolean): AnsiString;
var I, J : Integer;
begin
  Result := '';
  I := PosStrA(FirstDelim, S, 1, FirstDelimAsciiCaseSensitive);
  if (I = 0) and not FirstOptional then
    exit;
  Inc(I, Length(FirstDelim));
  J := PosStrA(SecondDelim, S, I, SecondDelimAsciiCaseSensitive);
  if J = 0 then
    if not SecondOptional then
      exit
    else
      J := Length(S) + 1;
  Result := CopyRangeA(S, I, J - 1);
end;

function StrBetweenB(const S: RawByteString; const FirstDelim: RawByteString;
    const SecondDelim: ByteCharSet; const FirstOptional: Boolean;
    const SecondOptional: Boolean;
    const FirstDelimAsciiCaseSensitive: Boolean): RawByteString;
var I, J : Integer;
begin
  Result := '';
  I := PosStrB(FirstDelim, S, 1, FirstDelimAsciiCaseSensitive);
  if (I = 0) and not FirstOptional then
    exit;
  Inc(I, Length(FirstDelim));
  J := PosCharSetB(SecondDelim, S, I);
  if J = 0 then
    if not SecondOptional then
      exit
    else
      J := Length(S) + 1;
  Result := CopyRangeB(S, I, J - 1);
end;

function StrBetweenB(const S: RawByteString;
    const FirstDelim, SecondDelim: RawByteString; const FirstOptional: Boolean;
    const SecondOptional: Boolean ; const FirstDelimAsciiCaseSensitive: Boolean;
    const SecondDelimAsciiCaseSensitive: Boolean): RawByteString;
var I, J : Integer;
begin
  Result := '';
  I := PosStrB(FirstDelim, S, 1, FirstDelimAsciiCaseSensitive);
  if (I = 0) and not FirstOptional then
    exit;
  Inc(I, Length(FirstDelim));
  J := PosStrB(SecondDelim, S, I, SecondDelimAsciiCaseSensitive);
  if J = 0 then
    if not SecondOptional then
      exit
    else
      J := Length(S) + 1;
  Result := CopyRangeB(S, I, J - 1);
end;
{$ENDIF}

function StrBetweenU(const S: UnicodeString; const FirstDelim: UnicodeString;
    const SecondDelim: ByteCharSet; const FirstOptional: Boolean;
    const SecondOptional: Boolean;
    const FirstDelimAsciiCaseSensitive: Boolean): UnicodeString;
var I, J : Integer;
begin
  Result := '';
  I := PosStrU(FirstDelim, S, 1, FirstDelimAsciiCaseSensitive);
  if (I = 0) and not FirstOptional then
    exit;
  Inc(I, Length(FirstDelim));
  J := PosCharSetU(SecondDelim, S, I);
  if J = 0 then
    if not SecondOptional then
      exit
    else
      J := Length(S) + 1;
  Result := CopyRangeU(S, I, J - 1);
end;

function StrBetweenU(const S: UnicodeString;
    const FirstDelim, SecondDelim: UnicodeString; const FirstOptional: Boolean;
    const SecondOptional: Boolean ; const FirstDelimAsciiCaseSensitive: Boolean;
    const SecondDelimAsciiCaseSensitive: Boolean): UnicodeString;
var I, J : Integer;
begin
  Result := '';
  I := PosStrU(FirstDelim, S, 1, FirstDelimAsciiCaseSensitive);
  if (I = 0) and not FirstOptional then
    exit;
  Inc(I, Length(FirstDelim));
  J := PosStrU(SecondDelim, S, I, SecondDelimAsciiCaseSensitive);
  if J = 0 then
    if not SecondOptional then
      exit
    else
      J := Length(S) + 1;
  Result := CopyRangeU(S, I, J - 1);
end;

function StrBetween(const S: String; const FirstDelim: String;
    const SecondDelim: ByteCharSet; const FirstOptional: Boolean;
    const SecondOptional: Boolean;
    const FirstDelimAsciiCaseSensitive: Boolean): String;
var I, J : Integer;
begin
  Result := '';
  I := PosStr(FirstDelim, S, 1, FirstDelimAsciiCaseSensitive);
  if (I = 0) and not FirstOptional then
    exit;
  Inc(I, Length(FirstDelim));
  J := PosCharSet(SecondDelim, S, I);
  if J = 0 then
    if not SecondOptional then
      exit
    else
      J := Length(S) + 1;
  Result := CopyRange(S, I, J - 1);
end;

function StrBetween(const S: String;
    const FirstDelim, SecondDelim: String; const FirstOptional: Boolean;
    const SecondOptional: Boolean ; const FirstDelimAsciiCaseSensitive: Boolean;
    const SecondDelimAsciiCaseSensitive: Boolean): String;
var I, J : Integer;
begin
  Result := '';
  I := PosStr(FirstDelim, S, 1, FirstDelimAsciiCaseSensitive);
  if (I = 0) and not FirstOptional then
    exit;
  Inc(I, Length(FirstDelim));
  J := PosStr(SecondDelim, S, I, SecondDelimAsciiCaseSensitive);
  if J = 0 then
    if not SecondOptional then
      exit
    else
      J := Length(S) + 1;
  Result := CopyRange(S, I, J - 1);
end;

{$IFDEF SupportAnsiString}
function StrBeforeA(const S, D: AnsiString; const Optional: Boolean;
    const AsciiCaseSensitive: Boolean): AnsiString;
var I : Integer;
begin
  I := PosStrA(D, S, 1, AsciiCaseSensitive);
  if I = 0 then
    if Optional then
      Result := S
    else
      Result := ''
  else
    Result := CopyLeftA(S, I - 1);
end;

function StrBeforeRevA(const S, D: AnsiString; const Optional: Boolean;
    const AsciiCaseSensitive: Boolean): AnsiString;
var I : Integer;
begin
  I := PosStrRevA(D, S, 1, AsciiCaseSensitive);
  if I = 0 then
    if Optional then
      Result := S
    else
      Result := ''
  else
    Result := CopyLeftA(S, I - 1);
end;
{$ENDIF}

function StrBeforeB(const S, D: RawByteString; const Optional: Boolean;
    const AsciiCaseSensitive: Boolean): RawByteString;
var I : Integer;
begin
  I := PosStrB(D, S, 1, AsciiCaseSensitive);
  if I = 0 then
    if Optional then
      Result := S
    else
      Result := ''
  else
    Result := CopyLeftB(S, I - 1);
end;

function StrBeforeRevB(const S, D: RawByteString; const Optional: Boolean;
    const AsciiCaseSensitive: Boolean): RawByteString;
var I : Integer;
begin
  I := PosStrRevB(D, S, 1, AsciiCaseSensitive);
  if I = 0 then
    if Optional then
      Result := S
    else
      Result := ''
  else
    Result := CopyLeftB(S, I - 1);
end;

function StrBeforeU(const S, D: UnicodeString; const Optional: Boolean;
    const AsciiCaseSensitive: Boolean): UnicodeString;
var I : Integer;
begin
  I := PosStrU(D, S, 1, AsciiCaseSensitive);
  if I = 0 then
    if Optional then
      Result := S
    else
      Result := ''
  else
    Result := CopyLeftU(S, I - 1);
end;

function StrBeforeRevU(const S, D: UnicodeString; const Optional: Boolean;
    const AsciiCaseSensitive: Boolean): UnicodeString;
var I : Integer;
begin
  I := PosStrRevU(D, S, 1, AsciiCaseSensitive);
  if I = 0 then
    if Optional then
      Result := S
    else
      Result := ''
  else
    Result := CopyLeftU(S, I - 1);
end;

function StrBefore(const S, D: String; const Optional: Boolean;
    const AsciiCaseSensitive: Boolean): String;
var I : Integer;
begin
  I := PosStr(D, S, 1, AsciiCaseSensitive);
  if I = 0 then
    if Optional then
      Result := S
    else
      Result := ''
  else
    Result := CopyLeft(S, I - 1);
end;

function StrBeforeRev(const S, D: String; const Optional: Boolean;
    const AsciiCaseSensitive: Boolean): String;
var I : Integer;
begin
  I := PosStrRev(D, S, 1, AsciiCaseSensitive);
  if I = 0 then
    if Optional then
      Result := S
    else
      Result := ''
  else
    Result := CopyLeft(S, I - 1);
end;

{$IFDEF SupportAnsiString}
function StrBeforeCharA(const S: AnsiString; const D: ByteCharSet;
    const Optional: Boolean): AnsiString;
var I : Integer;
begin
  I := PosCharSetA(D, S);
  if I = 0 then
    if Optional then
      Result := S
    else
      Result := ''
  else
    Result := CopyLeftA(S, I - 1);
end;

function StrBeforeCharA(const S: AnsiString; const D: AnsiChar;
    const Optional: Boolean): AnsiString;
var I : Integer;
begin
  I := PosCharA(D, S);
  if I = 0 then
    if Optional then
      Result := S
    else
      Result := ''
  else
    Result := CopyLeftA(S, I - 1);
end;

function StrBeforeCharRevA(const S: AnsiString; const D: ByteCharSet;
    const Optional: Boolean): AnsiString;
var I : Integer;
begin
  I := PosCharSetRevA(D, S);
  if I = 0 then
    if Optional then
      Result := S
    else
      Result := ''
  else
    Result := CopyLeftA(S, I - 1);
end;
{$ENDIF}

function StrBeforeCharB(const S: RawByteString; const D: ByteCharSet;
    const Optional: Boolean): RawByteString;
var I : Integer;
begin
  I := PosCharSetB(D, S);
  if I = 0 then
    if Optional then
      Result := S
    else
      Result := ''
  else
    Result := CopyLeftB(S, I - 1);
end;

function StrBeforeCharB(const S: RawByteString; const D: ByteChar;
    const Optional: Boolean): RawByteString;
var I : Integer;
begin
  I := PosCharB(D, S);
  if I = 0 then
    if Optional then
      Result := S
    else
      Result := ''
  else
    Result := CopyLeftB(S, I - 1);
end;

function StrBeforeCharRevB(const S: RawByteString; const D: ByteCharSet;
    const Optional: Boolean): RawByteString;
var I : Integer;
begin
  I := PosCharSetRevB(D, S);
  if I = 0 then
    if Optional then
      Result := S
    else
      Result := ''
  else
    Result := CopyLeftB(S, I - 1);
end;

function StrBeforeCharU(const S: UnicodeString; const D: ByteCharSet;
    const Optional: Boolean): UnicodeString;
var I : Integer;
begin
  I := PosCharSetU(D, S);
  if I = 0 then
    if Optional then
      Result := S
    else
      Result := ''
  else
    Result := CopyLeftU(S, I - 1);
end;

function StrBeforeCharU(const S: UnicodeString; const D: WideChar;
    const Optional: Boolean): UnicodeString;
var I : Integer;
begin
  I := PosCharU(D, S);
  if I = 0 then
    if Optional then
      Result := S
    else
      Result := ''
  else
    Result := CopyLeftU(S, I - 1);
end;

function StrBeforeCharRevU(const S: UnicodeString; const D: ByteCharSet;
    const Optional: Boolean): UnicodeString;
var I : Integer;
begin
  I := PosCharSetRevU(D, S);
  if I = 0 then
    if Optional then
      Result := S
    else
      Result := ''
  else
    Result := CopyLeftU(S, I - 1);
end;

function StrBeforeChar(const S: String; const D: ByteCharSet;
    const Optional: Boolean): String;
var I : Integer;
begin
  I := PosCharSet(D, S);
  if I = 0 then
    if Optional then
      Result := S
    else
      Result := ''
  else
    Result := CopyLeft(S, I - 1);
end;

function StrBeforeChar(const S: String; const D: Char;
    const Optional: Boolean): String;
var I : Integer;
begin
  I := PosChar(D, S);
  if I = 0 then
    if Optional then
      Result := S
    else
      Result := ''
  else
    Result := CopyLeft(S, I - 1);
end;

function StrBeforeCharRev(const S: String; const D: ByteCharSet;
    const Optional: Boolean): String;
var I : Integer;
begin
  I := PosCharSetRev(D, S);
  if I = 0 then
    if Optional then
      Result := S
    else
      Result := ''
  else
    Result := CopyLeft(S, I - 1);
end;

{$IFDEF SupportAnsiString}
function StrAfterA(const S, D: AnsiString; const Optional: Boolean): AnsiString;
var I : Integer;
begin
  I := PosStrA(D, S);
  if I = 0 then
    if Optional then
      Result := S
    else
      Result := ''
  else
    Result := CopyFromA(S, I + Length(D));
end;

function StrAfterRevA(const S, D: AnsiString; const Optional: Boolean): AnsiString;
var I : Integer;
begin
  I := PosStrRevA(D, S);
  if I = 0 then
    if Optional then
      Result := S
    else
      Result := ''
  else
    Result := CopyFromA(S, I + Length(D));
end;
{$ENDIF}

function StrAfterB(const S, D: RawByteString; const Optional: Boolean): RawByteString;
var I : Integer;
begin
  I := PosStrB(D, S);
  if I = 0 then
    if Optional then
      Result := S
    else
      Result := ''
  else
    Result := CopyFromB(S, I + Length(D));
end;

function StrAfterRevB(const S, D: RawByteString; const Optional: Boolean): RawByteString;
var I : Integer;
begin
  I := PosStrRevB(D, S);
  if I = 0 then
    if Optional then
      Result := S
    else
      Result := ''
  else
    Result := CopyFromB(S, I + Length(D));
end;

function StrAfterU(const S, D: UnicodeString; const Optional: Boolean): UnicodeString;
var I : Integer;
begin
  I := PosStrU(D, S);
  if I = 0 then
    if Optional then
      Result := S
    else
      Result := ''
  else
    Result := CopyFromU(S, I + Length(D));
end;

function StrAfterRevU(const S, D: UnicodeString; const Optional: Boolean): UnicodeString;
var I : Integer;
begin
  I := PosStrRevU(D, S);
  if I = 0 then
    if Optional then
      Result := S
    else
      Result := ''
  else
    Result := CopyFromU(S, I + Length(D));
end;

function StrAfter(const S, D: String; const Optional: Boolean): String;
var I : Integer;
begin
  I := PosStr(D, S);
  if I = 0 then
    if Optional then
      Result := S
    else
      Result := ''
  else
    Result := CopyFrom(S, I + Length(D));
end;

function StrAfterRev(const S, D: String; const Optional: Boolean): String;
var I : Integer;
begin
  I := PosStrRev(D, S);
  if I = 0 then
    if Optional then
      Result := S
    else
      Result := ''
  else
    Result := CopyFrom(S, I + Length(D));
end;

{$IFDEF SupportAnsiString}
function StrAfterCharA(const S: AnsiString; const D: ByteCharSet): AnsiString;
var I : Integer;
begin
  I := PosCharSetA(D, S);
  if I = 0 then
    Result := ''
  else
    Result := CopyFromA(S, I + 1);
end;

function StrAfterCharA(const S: AnsiString; const D: AnsiChar): AnsiString;
var I : Integer;
begin
  I := PosCharA(D, S);
  if I = 0 then
    Result := ''
  else
    Result := CopyFromA(S, I + 1);
end;
{$ENDIF}

function StrAfterCharB(const S: RawByteString; const D: ByteCharSet): RawByteString;
var I : Integer;
begin
  I := PosCharSetB(D, S);
  if I = 0 then
    Result := ''
  else
    Result := CopyFromB(S, I + 1);
end;

function StrAfterCharB(const S: RawByteString; const D: ByteChar): RawByteString;
var I : Integer;
begin
  I := PosCharB(D, S);
  if I = 0 then
    Result := ''
  else
    Result := CopyFromB(S, I + 1);
end;

function StrAfterCharU(const S: UnicodeString; const D: ByteCharSet): UnicodeString;
var I : Integer;
begin
  I := PosCharSetU(D, S);
  if I = 0 then
    Result := ''
  else
    Result := CopyFromU(S, I + 1);
end;

function StrAfterCharU(const S: UnicodeString; const D: WideChar): UnicodeString;
var I : Integer;
begin
  I := PosCharU(D, S);
  if I = 0 then
    Result := ''
  else
    Result := CopyFromU(S, I + 1);
end;

function StrAfterChar(const S: String; const D: ByteCharSet): String;
var I : Integer;
begin
  I := PosCharSet(D, S);
  if I = 0 then
    Result := ''
  else
    Result := CopyFrom(S, I + 1);
end;

function StrAfterChar(const S: String; const D: Char): String;
var I : Integer;
begin
  I := PosChar(D, S);
  if I = 0 then
    Result := ''
  else
    Result := CopyFrom(S, I + 1);
end;

{$IFDEF SupportAnsiString}
function StrCopyToCharA(const S: AnsiString; const D: ByteCharSet;
    const Optional: Boolean): AnsiString;
var I : Integer;
begin
  I := PosCharSetA(D, S);
  if I = 0 then
    if Optional then
      Result := S
    else
      Result := ''
  else
    Result := CopyLeftA(S, I);
end;

function StrCopyToCharA(const S: AnsiString; const D: AnsiChar;
    const Optional: Boolean): AnsiString;
var I : Integer;
begin
  I := PosCharA(D, S);
  if I = 0 then
    if Optional then
      Result := S
    else
      Result := ''
  else
    Result := CopyLeftA(S, I);
end;
{$ENDIF}

function StrCopyToCharB(const S: RawByteString; const D: ByteCharSet;
    const Optional: Boolean): RawByteString;
var I : Integer;
begin
  I := PosCharSetB(D, S);
  if I = 0 then
    if Optional then
      Result := S
    else
      Result := ''
  else
    Result := CopyLeftB(S, I);
end;

function StrCopyToCharB(const S: RawByteString; const D: ByteChar;
    const Optional: Boolean): RawByteString;
var I : Integer;
begin
  I := PosCharB(D, S);
  if I = 0 then
    if Optional then
      Result := S
    else
      Result := ''
  else
    Result := CopyLeftB(S, I);
end;

function StrCopyToCharU(const S: UnicodeString; const D: ByteCharSet;
    const Optional: Boolean): UnicodeString;
var I : Integer;
begin
  I := PosCharSetU(D, S);
  if I = 0 then
    if Optional then
      Result := S
    else
      Result := ''
  else
    Result := CopyLeftU(S, I);
end;

function StrCopyToCharU(const S: UnicodeString; const D: WideChar;
    const Optional: Boolean): UnicodeString;
var I : Integer;
begin
  I := PosCharU(D, S);
  if I = 0 then
    if Optional then
      Result := S
    else
      Result := ''
  else
    Result := CopyLeftU(S, I);
end;

function StrCopyToChar(const S: String; const D: ByteCharSet;
    const Optional: Boolean): String;
var I : Integer;
begin
  I := PosCharSet(D, S);
  if I = 0 then
    if Optional then
      Result := S
    else
      Result := ''
  else
    Result := CopyLeft(S, I);
end;

function StrCopyToChar(const S: String; const D: Char;
    const Optional: Boolean): String;
var I : Integer;
begin
  I := PosChar(D, S);
  if I = 0 then
    if Optional then
      Result := S
    else
      Result := ''
  else
    Result := CopyLeft(S, I);
end;

{$IFDEF SupportAnsiString}
function StrCopyFromCharA(const S: AnsiString; const D: ByteCharSet): AnsiString;
var I : Integer;
begin
  I := PosCharSetA(D, S);
  if I = 0 then
    Result := ''
  else
    Result := CopyFromA(S, I);
end;

function StrCopyFromCharA(const S: AnsiString; const D: AnsiChar): AnsiString;
var I : Integer;
begin
  I := PosCharB(D, S);
  if I = 0 then
    Result := ''
  else
    Result := CopyFromB(S, I);
end;
{$ENDIF}

function StrCopyFromCharB(const S: RawByteString; const D: ByteCharSet): RawByteString;
var I : Integer;
begin
  I := PosCharSetB(D, S);
  if I = 0 then
    Result := ''
  else
    Result := CopyFromB(S, I);
end;

function StrCopyFromCharB(const S: RawByteString; const D: ByteChar): RawByteString;
var I : Integer;
begin
  I := PosCharB(D, S);
  if I = 0 then
    Result := ''
  else
    Result := CopyFromB(S, I);
end;

function StrCopyFromCharU(const S: UnicodeString; const D: ByteCharSet): UnicodeString;
var I : Integer;
begin
  I := PosCharSetU(D, S);
  if I = 0 then
    Result := ''
  else
    Result := CopyFromU(S, I);
end;

function StrCopyFromCharU(const S: UnicodeString; const D: WideChar): UnicodeString;
var I : Integer;
begin
  I := PosCharU(D, S);
  if I = 0 then
    Result := ''
  else
    Result := CopyFromU(S, I);
end;

function StrCopyFromChar(const S: String; const D: ByteCharSet): String;
var I : Integer;
begin
  I := PosCharSet(D, S);
  if I = 0 then
    Result := ''
  else
    Result := CopyFrom(S, I);
end;

function StrCopyFromChar(const S: String; const D: Char): String;
var I : Integer;
begin
  I := PosChar(D, S);
  if I = 0 then
    Result := ''
  else
    Result := CopyFrom(S, I);
end;

{$IFDEF SupportAnsiString}
function StrRemoveCharDelimitedA(var S: AnsiString;
    const FirstDelim, SecondDelim: AnsiChar): AnsiString;
var I, J : Integer;
begin
  Result := '';
  I := PosCharA(FirstDelim, S);
  if I = 0 then
    exit;
  J := PosCharA(SecondDelim, S, I + 1);
  if J = 0 then
    exit;
  Result := CopyRangeA(S, I + 1, J - 1);
  Delete(S, I, J - I + 1);
end;
{$ENDIF}

function StrRemoveCharDelimitedB(var S: RawByteString;
    const FirstDelim, SecondDelim: ByteChar): RawByteString;
var I, J : Integer;
begin
  Result := '';
  I := PosCharB(FirstDelim, S);
  if I = 0 then
    exit;
  J := PosCharB(SecondDelim, S, I + 1);
  if J = 0 then
    exit;
  Result := CopyRangeB(S, I + 1, J - 1);
  Delete(S, I, J - I + 1);
end;

function StrRemoveCharDelimitedU(var S: UnicodeString;
    const FirstDelim, SecondDelim: WideChar): UnicodeString;
var I, J : Integer;
begin
  Result := '';
  I := PosCharU(FirstDelim, S);
  if I = 0 then
    exit;
  J := PosCharU(SecondDelim, S, I + 1);
  if J = 0 then
    exit;
  Result := CopyRangeU(S, I + 1, J - 1);
  Delete(S, I, J - I + 1);
end;

function StrRemoveCharDelimited(var S: String;
    const FirstDelim, SecondDelim: Char): String;
var I, J : Integer;
begin
  Result := '';
  I := PosChar(FirstDelim, S);
  if I = 0 then
    exit;
  J := PosChar(SecondDelim, S, I + 1);
  if J = 0 then
    exit;
  Result := CopyRange(S, I + 1, J - 1);
  Delete(S, I, J - I + 1);
end;



{                                                                              }
{ Count                                                                        }
{                                                                              }
{$IFDEF SupportAnsiString}
function StrCountCharA(const S: AnsiString; const C: AnsiChar): Integer;
var P : PAnsiChar;
    I : Integer;
begin
  Result := 0;
  P := Pointer(S);
  for I := 1 to Length(S) do
    begin
      if P^ = C then
        Inc(Result);
      Inc(P);
    end;
end;
{$ENDIF}

function StrCountCharB(const S: RawByteString; const C: ByteChar): Integer;
var P : PByteChar;
    I : Integer;
begin
  Result := 0;
  P := Pointer(S);
  for I := 1 to Length(S) do
    begin
      if P^ = C then
        Inc(Result);
      Inc(P);
    end;
end;

function StrCountCharU(const S: UnicodeString; const C: WideChar): Integer;
var P : PWideChar;
    I : Integer;
begin
  Result := 0;
  P := Pointer(S);
  for I := 1 to Length(S) do
    begin
      if P^ = C then
        Inc(Result);
      Inc(P);
    end;
end;

function StrCountChar(const S: String; const C: Char): Integer;
var P : PChar;
    I : Integer;
begin
  Result := 0;
  P := Pointer(S);
  for I := 1 to Length(S) do
    begin
      if P^ = C then
        Inc(Result);
      Inc(P);
    end;
end;

{$IFDEF SupportAnsiString}
function StrCountCharA(const S: AnsiString; const C: ByteCharSet): Integer;
var P : PAnsiChar;
    I : Integer;
begin
  Result := 0;
  P := Pointer(S);
  for I := 1 to Length(S) do
    begin
      if P^ in C then
        Inc(Result);
      Inc(P);
    end;
end;
{$ENDIF}

function StrCountCharB(const S: RawByteString; const C: ByteCharSet): Integer;
var P : PByteChar;
    I : Integer;
begin
  Result := 0;
  P := Pointer(S);
  for I := 1 to Length(S) do
    begin
      if P^ in C then
        Inc(Result);
      Inc(P);
    end;
end;

function StrCountCharU(const S: UnicodeString; const C: ByteCharSet): Integer;
var P : PWideChar;
    D : WideChar;
    I : Integer;
begin
  Result := 0;
  P := Pointer(S);
  for I := 1 to Length(S) do
    begin
      D := P^;
      if WideCharInCharSet(D, C) then
        Inc(Result);
      Inc(P);
    end;
end;

function StrCountChar(const S: String; const C: ByteCharSet): Integer;
var P : PChar;
    D : Char;
    I : Integer;
begin
  Result := 0;
  P := Pointer(S);
  for I := 1 to Length(S) do
    begin
      D := P^;
      {$IFDEF StringIsUnicode}
      if Ord(D) <= $FF then
      {$ENDIF}
        if ByteChar(Ord(D)) in C then
          Inc(Result);
      Inc(P);
    end;
end;



{                                                                              }
{ Replace                                                                      }
{                                                                              }
{$IFDEF SupportAnsiString}
function StrReplaceCharA(const Find, Replace: AnsiChar; const S: AnsiString): AnsiString;
var P, Q : PAnsiChar;
    I, J : Integer;
begin
  Result := S;
  I := PosCharA(Find, S);
  if I = 0 then
    exit;
  UniqueString(Result);
  Q := Pointer(Result);
  Inc(Q, I - 1);
  P := Pointer(S);
  Inc(P, I - 1);
  for J := I to Length(S) do
    begin
      if P^ = Find then
        Q^ := Replace;
      Inc(P);
      Inc(Q);
    end;
end;
{$ENDIF}

function StrReplaceCharB(const Find, Replace: ByteChar; const S: RawByteString): RawByteString;
var I, J : Integer;
begin
  Result := S;
  I := PosCharB(Find, S);
  if I = 0 then
    exit;
  for J := I to Length(S) do
    if S[J] = Find then
      Result[J] := Replace;
end;

function StrReplaceCharU(const Find, Replace: WideChar; const S: UnicodeString): UnicodeString;
var I, J : Integer;
begin
  Result := S;
  I := PosCharU(Find, S);
  if I = 0 then
    exit;
  for J := I to Length(S) do
    if S[J] = Find then
      Result[J] := Replace;
end;

function StrReplaceChar(const Find, Replace: Char; const S: String): String;
var I, J : Integer;
begin
  Result := S;
  I := PosChar(Find, S);
  if I = 0 then
    exit;
  for J := I to Length(S) do
    if S[J] = Find then
      Result[J] := Replace;
end;

{$IFDEF SupportAnsiString}
function StrReplaceCharA(const Find: ByteCharSet; const Replace: AnsiChar;
    const S: AnsiString): AnsiString;
var P, Q : PAnsiChar;
    I, J : Integer;
begin
  Result := S;
  I := PosCharSetA(Find, S);
  if I = 0 then
    exit;
  UniqueString(Result);
  Q := Pointer(Result);
  Inc(Q, I - 1);
  P := Pointer(S);
  Inc(P, I - 1);
  for J := I to Length(S) do
    begin
      if P^ in Find then
        Q^ := Replace;
      Inc(P);
      Inc(Q);
    end;
end;
{$ENDIF}

function StrReplaceCharB(const Find: ByteCharSet; const Replace: ByteChar;
    const S: RawByteString): RawByteString;
var I, J : Integer;
begin
  Result := S;
  I := PosCharSetB(Find, S);
  if I = 0 then
    exit;
  for J := I to Length(S) do
    if S[J] in Find then
      Result[J] := Replace;
end;

function StrReplaceCharU(const Find: ByteCharSet; const Replace: WideChar;
    const S: UnicodeString): UnicodeString;
var P, Q : PWideChar;
    I, J : Integer;
    C    : WideChar;
begin
  Result := S;
  I := PosCharSetU(Find, S);
  if I = 0 then
    exit;
  {$IFNDEF DELPHI5}
  UniqueString(Result);
  {$ENDIF}
  Q := Pointer(Result);
  Inc(Q, I - 1);
  P := Pointer(S);
  Inc(P, I - 1);
  for J := I to Length(S) do
    begin
      C := P^;
      if Ord(C) <= $FF then
        if ByteChar(Ord(C)) in Find then
          Q^ := Replace;
      Inc(P);
      Inc(Q);
    end;
end;

function StrReplaceChar(const Find: ByteCharSet; const Replace: Char;
    const S: String): String;
var P, Q : PChar;
    I, J : Integer;
    C    : Char;
begin
  Result := S;
  I := PosCharSet(Find, S);
  if I = 0 then
    exit;
  UniqueString(Result);
  Q := Pointer(Result);
  Inc(Q, I - 1);
  P := Pointer(S);
  Inc(P, I - 1);
  for J := I to Length(S) do
    begin
      C := P^;
      {$IFDEF StringIsUnicode}
      if Ord(C) <= $FF then
      {$ENDIF}
        if ByteChar(Ord(C)) in Find then
          Q^ := Replace;
      Inc(P);
      Inc(Q);
    end;
end;

{                                                                              }
{ StrReplace operates by replacing in 'batches' of 4096 matches. This has the  }
{ advantage of fewer memory allocations and limited stack usage when there is  }
{ a large number of matches.                                                   }
{                                                                              }
type
  StrReplaceMatchArray = Array[0..4095] of Integer;

{$IFDEF SupportAnsiString}
function StrReplaceBlockA( // used by StrReplaceA
    const FindLen: Integer; const Replace, S: AnsiString;
    const StartIndex, StopIndex: Integer;
    const MatchCount: Integer;
    const Matches: StrReplaceMatchArray): AnsiString;
var StrLen     : Integer;
    ReplaceLen : Integer;
    NewLen     : Integer;
    I, J, F, G : Integer;
    P, Q       : PAnsiChar;
begin
  ReplaceLen := Length(Replace);
  StrLen := StopIndex - StartIndex + 1;
  NewLen := StrLen + (ReplaceLen - FindLen) * MatchCount;
  if NewLen = 0 then
    begin
      Result := '';
      exit;
    end;
  SetString(Result, nil, NewLen);
  P := Pointer(Result);
  Q := Pointer(S);
  F := StartIndex;
  Inc(Q, F - 1);
  for I := 0 to MatchCount - 1 do
    begin
      G := Matches[I];
      J := G - F;
      if J > 0 then
        begin
          MoveMem(Q^, P^, J);
          Inc(P, J);
          Inc(Q, J);
          Inc(F, J);
        end;
      Inc(Q, FindLen);
      Inc(F, FindLen);
      if ReplaceLen > 0 then
        begin
          MoveMem(Pointer(Replace)^, P^, ReplaceLen);
          Inc(P, ReplaceLen);
        end;
    end;
  if F <= StopIndex then
    MoveMem(Q^, P^, StopIndex - F + 1);
end;
{$ENDIF}

function StrReplaceBlockB( // used by StrReplaceB
    const FindLen: Integer; const Replace, S: RawByteString;
    const StartIndex, StopIndex: Integer;
    const MatchCount: Integer;
    const Matches: StrReplaceMatchArray): RawByteString;
var StrLen     : Integer;
    ReplaceLen : Integer;
    NewLen     : Integer;
    I, J, F, G : Integer;
    P, Q       : PByteChar;
begin
  ReplaceLen := Length(Replace);
  StrLen := StopIndex - StartIndex + 1;
  NewLen := StrLen + (ReplaceLen - FindLen) * MatchCount;
  if NewLen = 0 then
    begin
      Result := '';
      exit;
    end;
  SetLength(Result, NewLen); // FPC 3.0 has issue with SetString(Result, nil, NewLen) here
  P := PByteChar(Result);
  Q := PByteChar(S);
  F := StartIndex;
  Inc(Q, F - 1);
  for I := 0 to MatchCount - 1 do
    begin
      G := Matches[I];
      J := G - F;
      if J > 0 then
        begin
          MoveMem(Q^, P^, J);
          Inc(P, J);
          Inc(Q, J);
          Inc(F, J);
        end;
      Inc(Q, FindLen);
      Inc(F, FindLen);
      if ReplaceLen > 0 then
        begin
          MoveMem(PByteChar(Replace)^, P^, ReplaceLen);
          Inc(P, ReplaceLen);
        end;
    end;
  if F <= StopIndex then
    MoveMem(Q^, P^, StopIndex - F + 1);
end;

function StrReplaceBlockU( // used by StrReplaceU
    const FindLen: Integer; const Replace, S: UnicodeString;
    const StartIndex, StopIndex: Integer;
    const MatchCount: Integer;
    const Matches: StrReplaceMatchArray): UnicodeString;
var StrLen     : Integer;
    ReplaceLen : Integer;
    NewLen     : Integer;
    I, J, F, G : Integer;
    P, Q       : PWideChar;
begin
  ReplaceLen := Length(Replace);
  StrLen := StopIndex - StartIndex + 1;
  NewLen := StrLen + (ReplaceLen - FindLen) * MatchCount;
  if NewLen = 0 then
    begin
      Result := '';
      exit;
    end;
  SetString(Result, nil, NewLen);
  P := Pointer(Result);
  Q := Pointer(S);
  F := StartIndex;
  Inc(Q, F - 1);
  for I := 0 to MatchCount - 1 do
    begin
      G := Matches[I];
      J := G - F;
      if J > 0 then
        begin
          MoveMem(Q^, P^, J * SizeOf(WideChar));
          Inc(P, J);
          Inc(Q, J);
          Inc(F, J);
        end;
      Inc(Q, FindLen);
      Inc(F, FindLen);
      if ReplaceLen > 0 then
        begin
          MoveMem(Pointer(Replace)^, P^, ReplaceLen * SizeOf(WideChar));
          Inc(P, ReplaceLen);
        end;
    end;
  if F <= StopIndex then
    MoveMem(Q^, P^, (StopIndex - F + 1) * SizeOf(WideChar));
end;

function StrReplaceBlock( // used by StrReplace
    const FindLen: Integer; const Replace, S: String;
    const StartIndex, StopIndex: Integer;
    const MatchCount: Integer;
    const Matches: StrReplaceMatchArray): String;
var StrLen     : Integer;
    ReplaceLen : Integer;
    NewLen     : Integer;
    I, J, F, G : Integer;
    P, Q       : PChar;
begin
  ReplaceLen := Length(Replace);
  StrLen := StopIndex - StartIndex + 1;
  NewLen := StrLen + (ReplaceLen - FindLen) * MatchCount;
  if NewLen = 0 then
    begin
      Result := '';
      exit;
    end;
  SetString(Result, nil, NewLen);
  P := Pointer(Result);
  Q := Pointer(S);
  F := StartIndex;
  Inc(Q, F - 1);
  for I := 0 to MatchCount - 1 do
    begin
      G := Matches[I];
      J := G - F;
      if J > 0 then
        begin
          MoveMem(Q^, P^, J * SizeOf(Char));
          Inc(P, J);
          Inc(Q, J);
          Inc(F, J);
        end;
      Inc(Q, FindLen);
      Inc(F, FindLen);
      if ReplaceLen > 0 then
        begin
          MoveMem(Pointer(Replace)^, P^, ReplaceLen * SizeOf(Char));
          Inc(P, ReplaceLen);
        end;
    end;
  if F <= StopIndex then
    MoveMem(Q^, P^, (StopIndex - F + 1) * SizeOf(Char));
end;

{$IFDEF SupportAnsiString}
function StrReplaceA(const Find, Replace, S: AnsiString; const AsciiCaseSensitive: Boolean): AnsiString;
var FindLen    : Integer;
    Matches    : StrReplaceMatchArray;
    C, I, J, K : Integer;
begin
  FindLen := Length(Find);
  if FindLen = 0 then // nothing to find
    begin
      Result := S;
      exit;
    end;
  I := PosStrA(Find, S, 1, AsciiCaseSensitive);
  if I = 0 then // not found
    begin
      Result := S;
      exit;
    end;
  J := 1;
  Result := '';
  repeat
    C := 0;
    repeat
      Matches[C] := I;
      Inc(C);
      Inc(I, FindLen);
      I := PosStrA(Find, S, I, AsciiCaseSensitive);
    until (I = 0) or (C = 4096);
    if I = 0 then
      K := Length(S)
    else
      K := I - 1;
    Result := Result + StrReplaceBlockA(FindLen, Replace, S, J, K, C, Matches);
    J := K + 1;
  until I = 0;
end;
{$ENDIF}

function StrReplaceB(const Find, Replace, S: RawByteString; const AsciiCaseSensitive: Boolean): RawByteString;
var FindLen    : Integer;
    Matches    : StrReplaceMatchArray;
    C, I, J, K : Integer;
begin
  FindLen := Length(Find);
  if FindLen = 0 then // nothing to find
    begin
      Result := S;
      exit;
    end;
  I := PosStrB(Find, S, 1, AsciiCaseSensitive);
  if I = 0 then // not found
    begin
      Result := S;
      exit;
    end;
  J := 1;
  Result := '';
  repeat
    C := 0;
    repeat
      Matches[C] := I;
      Inc(C);
      Inc(I, FindLen);
      I := PosStrB(Find, S, I, AsciiCaseSensitive);
    until (I = 0) or (C = 4096);
    if I = 0 then
      K := Length(S)
    else
      K := I - 1;
    Result := Result + StrReplaceBlockB(FindLen, Replace, S, J, K, C, Matches);
    J := K + 1;
  until I = 0;
end;

function StrReplaceU(const Find, Replace, S: UnicodeString; const AsciiCaseSensitive: Boolean): UnicodeString;
var FindLen    : Integer;
    Matches    : StrReplaceMatchArray;
    C, I, J, K : Integer;
begin
  FindLen := Length(Find);
  if FindLen = 0 then // nothing to find
    begin
      Result := S;
      exit;
    end;
  I := PosStrU(Find, S, 1, AsciiCaseSensitive);
  if I = 0 then // not found
    begin
      Result := S;
      exit;
    end;
  J := 1;
  Result := '';
  repeat
    C := 0;
    repeat
      Matches[C] := I;
      Inc(C);
      Inc(I, FindLen);
      I := PosStrU(Find, S, I, AsciiCaseSensitive);
    until (I = 0) or (C = 4096);
    if I = 0 then
      K := Length(S)
    else
      K := I - 1;
    Result := Result + StrReplaceBlockU(FindLen, Replace, S, J, K, C, Matches);
    J := K + 1;
  until I = 0;
end;

function StrReplace(const Find, Replace, S: String; const AsciiCaseSensitive: Boolean): String;
var FindLen    : Integer;
    Matches    : StrReplaceMatchArray;
    C, I, J, K : Integer;
begin
  FindLen := Length(Find);
  if FindLen = 0 then // nothing to find
    begin
      Result := S;
      exit;
    end;
  I := PosStr(Find, S, 1, AsciiCaseSensitive);
  if I = 0 then // not found
    begin
      Result := S;
      exit;
    end;
  J := 1;
  Result := '';
  repeat
    C := 0;
    repeat
      Matches[C] := I;
      Inc(C);
      Inc(I, FindLen);
      I := PosStr(Find, S, I, AsciiCaseSensitive);
    until (I = 0) or (C = 4096);
    if I = 0 then
      K := Length(S)
    else
      K := I - 1;
    Result := Result + StrReplaceBlock(FindLen, Replace, S, J, K, C, Matches);
    J := K + 1;
  until I = 0;
end;

{$IFDEF SupportAnsiString}
function StrReplaceA(const Find: ByteCharSet; const Replace, S: AnsiString): AnsiString;
var Matches    : StrReplaceMatchArray;
    C, I, J, K : Integer;
begin
  I := PosCharSetA(Find, S, 1);
  if I = 0 then // not found
    begin
      Result := S;
      exit;
    end;
  J := 1;
  Result := '';
  repeat
    C := 0;
    repeat
      Matches[C] := I;
      Inc(C);
      Inc(I);
      I := PosCharSetA(Find, S, I);
    until (I = 0) or (C = 4096);
    if I = 0 then
      K := Length(S)
    else
      K := I - 1;
    Result := Result + StrReplaceBlockA(1, Replace, S, J, K, C, Matches);
    J := K + 1;
  until I = 0;
end;
{$ENDIF}

function StrReplaceB(const Find: ByteCharSet; const Replace, S: RawByteString): RawByteString;
var Matches    : StrReplaceMatchArray;
    C, I, J, K : Integer;
begin
  I := PosCharSetB(Find, S, 1);
  if I = 0 then // not found
    begin
      Result := S;
      exit;
    end;
  J := 1;
  Result := '';
  repeat
    C := 0;
    repeat
      Matches[C] := I;
      Inc(C);
      Inc(I);
      I := PosCharSetB(Find, S, I);
    until (I = 0) or (C = 4096);
    if I = 0 then
      K := Length(S)
    else
      K := I - 1;
    Result := Result + StrReplaceBlockB(1, Replace, S, J, K, C, Matches);
    J := K + 1;
  until I = 0;
end;

function StrReplaceU(const Find: ByteCharSet; const Replace, S: UnicodeString): UnicodeString;
var Matches    : StrReplaceMatchArray;
    C, I, J, K : Integer;
begin
  I := PosCharSetU(Find, S, 1);
  if I = 0 then // not found
    begin
      Result := S;
      exit;
    end;
  J := 1;
  Result := '';
  repeat
    C := 0;
    repeat
      Matches[C] := I;
      Inc(C);
      Inc(I);
      I := PosCharSetU(Find, S, I);
    until (I = 0) or (C = 4096);
    if I = 0 then
      K := Length(S)
    else
      K := I - 1;
    Result := Result + StrReplaceBlockU(1, Replace, S, J, K, C, Matches);
    J := K + 1;
  until I = 0;
end;

function StrReplace(const Find: ByteCharSet; const Replace, S: String): String;
var Matches    : StrReplaceMatchArray;
    C, I, J, K : Integer;
begin
  I := PosCharSet(Find, S, 1);
  if I = 0 then // not found
    begin
      Result := S;
      exit;
    end;
  J := 1;
  Result := '';
  repeat
    C := 0;
    repeat
      Matches[C] := I;
      Inc(C);
      Inc(I);
      I := PosCharSet(Find, S, I);
    until (I = 0) or (C = 4096);
    if I = 0 then
      K := Length(S)
    else
      K := I - 1;
    Result := Result + StrReplaceBlock(1, Replace, S, J, K, C, Matches);
    J := K + 1;
  until I = 0;
end;

{$IFDEF SupportAnsiString}
function StrReplaceCharStrA(const Find: AnsiChar; const Replace, S: AnsiString): AnsiString;
var Matches    : StrReplaceMatchArray;
    C, I, J, K : Integer;
begin
  I := PosCharA(Find, S, 1);
  if I = 0 then // not found
    begin
      Result := S;
      exit;
    end;
  J := 1;
  Result := '';
  repeat
    C := 0;
    repeat
      Matches[C] := I;
      Inc(C);
      Inc(I);
      I := PosCharA(Find, S, I);
    until (I = 0) or (C = 4096);
    if I = 0 then
      K := Length(S)
    else
      K := I - 1;
    Result := Result + StrReplaceBlockA(1, Replace, S, J, K, C, Matches);
    J := K + 1;
  until I = 0;
end;
{$ENDIF}

function StrReplaceCharStrU(const Find: WideChar; const Replace, S: UnicodeString): UnicodeString;
var Matches    : StrReplaceMatchArray;
    C, I, J, K : Integer;
begin
  I := PosCharU(Find, S, 1);
  if I = 0 then // not found
    begin
      Result := S;
      exit;
    end;
  J := 1;
  Result := '';
  repeat
    C := 0;
    repeat
      Matches[C] := I;
      Inc(C);
      Inc(I);
      I := PosCharU(Find, S, I);
    until (I = 0) or (C = 4096);
    if I = 0 then
      K := Length(S)
    else
      K := I - 1;
    Result := Result + StrReplaceBlockU(1, Replace, S, J, K, C, Matches);
    J := K + 1;
  until I = 0;
end;

{$IFDEF SupportAnsiString}
function StrRemoveDupA(const S: AnsiString; const C: AnsiChar): AnsiString;
var P, Q    : PAnsiChar;
    D, E    : AnsiChar;
    I, L, M : Integer;
    R       : Boolean;
begin
  L := Length(S);
  if L <= 1 then
    begin
      Result := S;
      exit;
    end;
  // Check for duplicate
  P := Pointer(S);
  D := P^;
  Inc(P);
  R := False;
  for I := 2 to L do
    if (D = C) and (P^ = C) then
      begin
        R := True;
        break;
      end
    else
      begin
        D := P^;
        Inc(P);
      end;
  if not R then
    begin
      Result := S;
      exit;
    end;
  // Remove duplicates
  Result := S;
  UniqueString(Result);
  P := Pointer(S);
  Q := Pointer(Result);
  D := P^;
  Q^ := D;
  Inc(P);
  Inc(Q);
  M := 1;
  for I := 2 to L do
    begin
      E := P^;
      if (D <> C) or (E <> C) then
        begin
          D := E;
          Q^ := E;
          Inc(M);
          Inc(Q);
        end;
      Inc(P);
    end;
  if M < L then
    SetLength(Result, M);
end;
{$ENDIF}

function StrRemoveDupU(const S: UnicodeString; const C: WideChar): UnicodeString;
var P, Q    : PWideChar;
    D, E    : WideChar;
    I, L, M : Integer;
    R       : Boolean;
begin
  L := Length(S);
  if L <= 1 then
    begin
      Result := S;
      exit;
    end;
  // Check for duplicate
  P := Pointer(S);
  D := P^;
  Inc(P);
  R := False;
  for I := 2 to L do
    if (D = C) and (P^ = C) then
      begin
        R := True;
        break;
      end
    else
      begin
        D := P^;
        Inc(P);
      end;
  if not R then
    begin
      Result := S;
      exit;
    end;
  // Remove duplicates
  Result := S;
  {$IFNDEF DELPHI5}
  UniqueString(Result);
  {$ENDIF}
  P := Pointer(S);
  Q := Pointer(Result);
  D := P^;
  Q^ := D;
  Inc(P);
  Inc(Q);
  M := 1;
  for I := 2 to L do
    begin
      E := P^;
      if (D <> C) or (E <> C) then
        begin
          D := E;
          Q^ := E;
          Inc(M);
          Inc(Q);
        end;
      Inc(P);
    end;
  if M < L then
    SetLength(Result, M);
end;

function StrRemoveDup(const S: String; const C: Char): String;
var P, Q    : PChar;
    D, E    : Char;
    I, L, M : Integer;
    R       : Boolean;
begin
  L := Length(S);
  if L <= 1 then
    begin
      Result := S;
      exit;
    end;
  // Check for duplicate
  P := Pointer(S);
  D := P^;
  Inc(P);
  R := False;
  for I := 2 to L do
    if (D = C) and (P^ = C) then
      begin
        R := True;
        break;
      end
    else
      begin
        D := P^;
        Inc(P);
      end;
  if not R then
    begin
      Result := S;
      exit;
    end;
  // Remove duplicates
  Result := S;
  UniqueString(Result);
  P := Pointer(S);
  Q := Pointer(Result);
  D := P^;
  Q^ := D;
  Inc(P);
  Inc(Q);
  M := 1;
  for I := 2 to L do
    begin
      E := P^;
      if (D <> C) or (E <> C) then
        begin
          D := E;
          Q^ := E;
          Inc(M);
          Inc(Q);
        end;
      Inc(P);
    end;
  if M < L then
    SetLength(Result, M);
end;

{$IFDEF SupportAnsiString}
function StrRemoveCharA(const S: AnsiString; const C: AnsiChar): AnsiString;
var P, Q    : PAnsiChar;
    I, L, M : Integer;
begin
  L := Length(S);
  if L = 0 then
    begin
      Result := '';
      exit;
    end;
  M := 0;
  P := Pointer(S);
  for I := 1 to L do
    begin
      if P^ = C then
        Inc(M);
      Inc(P);
    end;
  if M = 0 then
    begin
      Result := S;
      exit;
    end;
  SetLength(Result, L - M);
  Q := Pointer(Result);
  P := Pointer(S);
  for I := 1 to L do
    begin
      if P^ <> C then
        begin
          Q^ := P^;
          Inc(Q);
        end;
      Inc(P);
    end;
end;
{$ENDIF}

function StrRemoveCharU(const S: UnicodeString; const C: WideChar): UnicodeString;
var P, Q    : PWideChar;
    I, L, M : Integer;
begin
  L := Length(S);
  if L = 0 then
    begin
      Result := '';
      exit;
    end;
  M := 0;
  P := Pointer(S);
  for I := 1 to L do
    begin
      if P^ = C then
        Inc(M);
      Inc(P);
    end;
  if M = 0 then
    begin
      Result := S;
      exit;
    end;
  SetLength(Result, L - M);
  Q := Pointer(Result);
  P := Pointer(S);
  for I := 1 to L do
    begin
      if P^ <> C then
        begin
          Q^ := P^;
          Inc(Q);
        end;
      Inc(P);
    end;
end;

function StrRemoveChar(const S: String; const C: Char): String;
var P, Q    : PChar;
    I, L, M : Integer;
begin
  L := Length(S);
  if L = 0 then
    begin
      Result := '';
      exit;
    end;
  M := 0;
  P := Pointer(S);
  for I := 1 to L do
    begin
      if P^ = C then
        Inc(M);
      Inc(P);
    end;
  if M = 0 then
    begin
      Result := S;
      exit;
    end;
  SetLength(Result, L - M);
  Q := Pointer(Result);
  P := Pointer(S);
  for I := 1 to L do
    begin
      if P^ <> C then
        begin
          Q^ := P^;
          Inc(Q);
        end;
      Inc(P);
    end;
end;

{$IFDEF SupportAnsiString}
function StrRemoveCharSetA(const S: AnsiString; const C: ByteCharSet): AnsiString;
var P, Q    : PAnsiChar;
    I, L, M : Integer;
begin
  L := Length(S);
  if L = 0 then
    begin
      Result := '';
      exit;
    end;
  M := 0;
  P := Pointer(S);
  for I := 1 to L do
    begin
      if P^ in C then
        Inc(M);
      Inc(P);
    end;
  if M = 0 then
    begin
      Result := S;
      exit;
    end;
  SetLength(Result, L - M);
  Q := Pointer(Result);
  P := Pointer(S);
  for I := 1 to L do
    begin
      if not (P^ in C) then
        begin
          Q^ := P^;
          Inc(Q);
        end;
      Inc(P);
    end;
end;
{$ENDIF}

function StrRemoveCharSetB(const S: RawByteString; const C: ByteCharSet): RawByteString;
var P, Q    : PByteChar;
    I, L, M : Integer;
begin
  L := Length(S);
  if L = 0 then
    begin
      Result := '';
      exit;
    end;
  M := 0;
  P := Pointer(S);
  for I := 1 to L do
    begin
      if P^ in C then
        Inc(M);
      Inc(P);
    end;
  if M = 0 then
    begin
      Result := S;
      exit;
    end;
  SetLength(Result, L - M);
  Q := Pointer(Result);
  P := Pointer(S);
  for I := 1 to L do
    begin
      if not (P^ in C) then
        begin
          Q^ := P^;
          Inc(Q);
        end;
      Inc(P);
    end;
end;

function StrRemoveCharSetU(const S: UnicodeString; const C: ByteCharSet): UnicodeString;
var P, Q    : PWideChar;
    D       : WideChar;
    I, L, M : Integer;
    R       : Boolean;
begin
  L := Length(S);
  if L = 0 then
    begin
      Result := '';
      exit;
    end;
  M := 0;
  P := Pointer(S);
  for I := 1 to L do
    begin
      D := P^;
      if Ord(D) <= $FF then
        if ByteChar(Ord(D)) in C then
          Inc(M);
      Inc(P);
    end;
  if M = 0 then
    begin
      Result := S;
      exit;
    end;
  SetLength(Result, L - M);
  Q := Pointer(Result);
  P := Pointer(S);
  for I := 1 to L do
    begin
      D := P^;
      R := Ord(D) > $FF;
      if not R then
        R := not (ByteChar(Ord(D)) in C);
      if R then
        begin
          Q^ := P^;
          Inc(Q);
        end;
      Inc(P);
    end;
end;

function StrRemoveCharSet(const S: String; const C: ByteCharSet): String;
var P, Q    : PChar;
    D       : Char;
    I, L, M : Integer;
    R       : Boolean;
begin
  L := Length(S);
  if L = 0 then
    begin
      Result := '';
      exit;
    end;
  M := 0;
  P := Pointer(S);
  for I := 1 to L do
    begin
      D := P^;
      {$IFDEF StringIsUnicode}
      if Ord(D) <= $FF then
      {$ENDIF}
        if ByteChar(Ord(D)) in C then
          Inc(M);
      Inc(P);
    end;
  if M = 0 then
    begin
      Result := S;
      exit;
    end;
  SetLength(Result, L - M);
  Q := Pointer(Result);
  P := Pointer(S);
  for I := 1 to L do
    begin
      D := P^;
      {$IFDEF StringIsUnicode}
      R := Ord(D) > $FF;
      if not R then
      {$ENDIF}
        R := not (ByteChar(Ord(D)) in C);
      if R then
        begin
          Q^ := P^;
          Inc(Q);
        end;
      Inc(P);
    end;
end;



{                                                                              }
{ Split                                                                        }
{                                                                              }
{$IFDEF SupportAnsiString}
function StrSplitAtA(const S: AnsiString; const C: AnsiString;
         var Left, Right: AnsiString; const AsciiCaseSensitive: Boolean;
         const Optional: Boolean): Boolean;
var I : Integer;
    T : AnsiString;
begin
  I := PosStrA(C, S, 1, AsciiCaseSensitive);
  Result := I > 0;
  if Result then
    begin
      T := S;
      Left := Copy(T, 1, I - 1);
      Right := CopyFromA(T, I + Length(C));
    end
  else
    begin
      if Optional then
        Left := S
      else
        Left := '';
      Right := '';
    end;
end;
{$ENDIF}

function StrSplitAtB(const S: RawByteString; const C: RawByteString;
         var Left, Right: RawByteString; const AsciiCaseSensitive: Boolean;
         const Optional: Boolean): Boolean;
var I : Integer;
    T : RawByteString;
begin
  I := PosStrB(C, S, 1, AsciiCaseSensitive);
  Result := I > 0;
  if Result then
    begin
      T := S;
      Left := Copy(T, 1, I - 1);
      Right := CopyFromB(T, I + Length(C));
    end
  else
    begin
      if Optional then
        Left := S
      else
        Left := '';
      Right := '';
    end;
end;

function StrSplitAtU(const S: UnicodeString; const C: UnicodeString;
         var Left, Right: UnicodeString; const AsciiCaseSensitive: Boolean;
         const Optional: Boolean): Boolean;
var I : Integer;
    T : UnicodeString;
begin
  I := PosStrU(C, S, 1, AsciiCaseSensitive);
  Result := I > 0;
  if Result then
    begin
      T := S;
      Left := Copy(T, 1, I - 1);
      Right := CopyFromU(T, I + Length(C));
    end
  else
    begin
      if Optional then
        Left := S
      else
        Left := '';
      Right := '';
    end;
end;

function StrSplitAt(const S: String; const C: String;
         var Left, Right: String; const AsciiCaseSensitive: Boolean;
         const Optional: Boolean): Boolean;
var I : Integer;
    T : String;
begin
  I := PosStr(C, S, 1, AsciiCaseSensitive);
  Result := I > 0;
  if Result then
    begin
      T := S;
      Left := Copy(T, 1, I - 1);
      Right := CopyFrom(T, I + Length(C));
    end
  else
    begin
      if Optional then
        Left := S
      else
        Left := '';
      Right := '';
    end;
end;

{$IFDEF SupportAnsiString}
function StrSplitAtCharA(const S: AnsiString; const C: AnsiChar;
         var Left, Right: AnsiString; const Optional: Boolean): Boolean;
var I : Integer;
    T : AnsiString;
begin
  I := PosCharA(C, S);
  Result := I > 0;
  if Result then
    begin
      T := S; // add reference to S (in case it is also Left or Right)
      Left := Copy(T, 1, I - 1);
      Right := CopyFromA(T, I + 1);
    end
  else
    begin
      if Optional then
        Left := S
      else
        Left := '';
      Right := '';
    end;
end;
{$ENDIF}

function StrSplitAtCharB(const S: RawByteString; const C: ByteChar;
         var Left, Right: RawByteString; const Optional: Boolean): Boolean;
var I : Integer;
    T : RawByteString;
begin
  I := PosCharB(C, S);
  Result := I > 0;
  if Result then
    begin
      T := S; // add reference to S (in case it is also Left or Right)
      Left := Copy(T, 1, I - 1);
      Right := CopyFromB(T, I + 1);
    end
  else
    begin
      if Optional then
        Left := S
      else
        Left := '';
      Right := '';
    end;
end;

function StrSplitAtCharU(const S: UnicodeString; const C: WideChar;
         var Left, Right: UnicodeString; const Optional: Boolean): Boolean;
var I : Integer;
    T : UnicodeString;
begin
  I := PosCharU(C, S);
  Result := I > 0;
  if Result then
    begin
      T := S; // add reference to S (in case it is also Left or Right)
      Left := Copy(T, 1, I - 1);
      Right := CopyFromU(T, I + 1);
    end
  else
    begin
      if Optional then
        Left := S
      else
        Left := '';
      Right := '';
    end;
end;

function StrSplitAtChar(const S: String; const C: Char;
         var Left, Right: String; const Optional: Boolean): Boolean;
var I : Integer;
    T : String;
begin
  I := PosChar(C, S);
  Result := I > 0;
  if Result then
    begin
      T := S; // add reference to S (in case it is also Left or Right)
      Left := Copy(T, 1, I - 1);
      Right := CopyFrom(T, I + 1);
    end
  else
    begin
      if Optional then
        Left := S
      else
        Left := '';
      Right := '';
    end;
end;

{$IFDEF SupportAnsiString}
function StrSplitAtCharSetA(const S: AnsiString; const C: ByteCharSet;
         var Left, Right: AnsiString; const Optional: Boolean): Boolean;
var I : Integer;
    T : AnsiString;
begin
  I := PosCharSetA(C, S);
  Result := I > 0;
  if Result then
    begin
      T := S;
      Left := Copy(T, 1, I - 1);
      Right := CopyFromA(T, I + 1);
    end else
    begin
      if Optional then
        Left := S
      else
        Left := '';
      Right := '';
    end;
end;
{$ENDIF}

function StrSplitAtCharSetB(const S: RawByteString; const C: ByteCharSet;
         var Left, Right: RawByteString; const Optional: Boolean): Boolean;
var I : Integer;
    T : RawByteString;
begin
  I := PosCharSetB(C, S);
  Result := I > 0;
  if Result then
    begin
      T := S;
      Left := Copy(T, 1, I - 1);
      Right := CopyFromB(T, I + 1);
    end else
    begin
      if Optional then
        Left := S
      else
        Left := '';
      Right := '';
    end;
end;

{$IFDEF SupportAnsiString}
function StrSplitA(const S, D: AnsiString): AnsiStringArray;
var I, J, L, M : Integer;
begin
  // Check valid parameters
  if S = '' then
    begin
      Result := nil;
      exit;
    end;
  M := Length(D);
  if M = 0 then
    begin
      SetLength(Result, 1);
      Result[0] := S;
      exit;
    end;
  // Count
  L := 0;
  I := 1;
  repeat
    I := PosStrA(D, S, I, True);
    if I = 0 then
      break;
    Inc(L);
    Inc(I, M);
  until False;
  SetLength(Result, L + 1);
  if L = 0 then
    begin
      // No split
      Result[0] := S;
      exit;
    end;
  // Split
  L := 0;
  I := 1;
  repeat
    J := PosStrA(D, S, I, True);
    if J = 0 then
      begin
        Result[L] := CopyFromA(S, I);
        break;
      end;
    Result[L] := CopyRangeA(S, I, J - 1);
    Inc(L);
    I := J + M;
  until False;
end;
{$ENDIF}

function StrSplitB(const S, D: RawByteString): RawByteStringArray;
var I, J, L, M : Integer;
begin
  // Check valid parameters
  if S = '' then
    begin
      Result := nil;
      exit;
    end;
  M := Length(D);
  if M = 0 then
    begin
      SetLength(Result, 1);
      Result[0] := S;
      exit;
    end;
  // Count
  L := 0;
  I := 1;
  repeat
    I := PosStrB(D, S, I, True);
    if I = 0 then
      break;
    Inc(L);
    Inc(I, M);
  until False;
  SetLength(Result, L + 1);
  if L = 0 then
    begin
      // No split
      Result[0] := S;
      exit;
    end;
  // Split
  L := 0;
  I := 1;
  repeat
    J := PosStrB(D, S, I, True);
    if J = 0 then
      begin
        Result[L] := CopyFromB(S, I);
        break;
      end;
    Result[L] := CopyRangeB(S, I, J - 1);
    Inc(L);
    I := J + M;
  until False;
end;

function StrSplitU(const S, D: UnicodeString): UnicodeStringArray;
var I, J, L, M : Integer;
begin
  // Check valid parameters
  if S = '' then
    begin
      Result := nil;
      exit;
    end;
  M := Length(D);
  if M = 0 then
    begin
      SetLength(Result, 1);
      Result[0] := S;
      exit;
    end;
  // Count
  L := 0;
  I := 1;
  repeat
    I := PosStrU(D, S, I, True);
    if I = 0 then
      break;
    Inc(L);
    Inc(I, M);
  until False;
  SetLength(Result, L + 1);
  if L = 0 then
    begin
      // No split
      Result[0] := S;
      exit;
    end;
  // Split
  L := 0;
  I := 1;
  repeat
    J := PosStrU(D, S, I, True);
    if J = 0 then
      begin
        Result[L] := CopyFromU(S, I);
        break;
      end;
    Result[L] := CopyRangeU(S, I, J - 1);
    Inc(L);
    I := J + M;
  until False;
end;

function StrSplit(const S, D: String): StringArray;
var I, J, L, M : Integer;
begin
  // Check valid parameters
  if S = '' then
    begin
      Result := nil;
      exit;
    end;
  M := Length(D);
  if M = 0 then
    begin
      SetLength(Result, 1);
      Result[0] := S;
      exit;
    end;
  // Count
  L := 0;
  I := 1;
  repeat
    I := PosStr(D, S, I, True);
    if I = 0 then
      break;
    Inc(L);
    Inc(I, M);
  until False;
  SetLength(Result, L + 1);
  if L = 0 then
    begin
      // No split
      Result[0] := S;
      exit;
    end;
  // Split
  L := 0;
  I := 1;
  repeat
    J := PosStr(D, S, I, True);
    if J = 0 then
      begin
        Result[L] := CopyFrom(S, I);
        break;
      end;
    Result[L] := CopyRange(S, I, J - 1);
    Inc(L);
    I := J + M;
  until False;
end;

{$IFDEF SupportAnsiString}
function StrSplitCharA(const S: AnsiString; const D: AnsiChar): AnsiStringArray;
var I, J, L : Integer;
begin
  // Check valid parameters
  if S = '' then
    begin
      Result := nil;
      exit;
    end;
  // Count
  L := 0;
  I := 1;
  repeat
    I := PosCharA(D, S, I);
    if I = 0 then
      break;
    Inc(L);
    Inc(I);
  until False;
  SetLength(Result, L + 1);
  if L = 0 then
    begin
      // No split
      Result[0] := S;
      exit;
    end;
  // Split
  L := 0;
  I := 1;
  repeat
    J := PosCharA(D, S, I);
    if J = 0 then
      begin
        Result[L] := CopyFromA(S, I);
        break;
      end;
    Result[L] := CopyRangeA(S, I, J - 1);
    Inc(L);
    I := J + 1;
  until False;
end;
{$ENDIF}

function StrSplitCharB(const S: RawByteString; const D: ByteChar): RawByteStringArray;
var I, J, L : Integer;
begin
  // Check valid parameters
  if S = '' then
    begin
      Result := nil;
      exit;
    end;
  // Count
  L := 0;
  I := 1;
  repeat
    I := PosCharB(D, S, I);
    if I = 0 then
      break;
    Inc(L);
    Inc(I);
  until False;
  SetLength(Result, L + 1);
  if L = 0 then
    begin
      // No split
      Result[0] := S;
      exit;
    end;
  // Split
  L := 0;
  I := 1;
  repeat
    J := PosCharB(D, S, I);
    if J = 0 then
      begin
        Result[L] := CopyFromB(S, I);
        break;
      end;
    Result[L] := CopyRangeB(S, I, J - 1);
    Inc(L);
    I := J + 1;
  until False;
end;

function StrSplitCharU(const S: UnicodeString; const D: WideChar): UnicodeStringArray;
var I, J, L : Integer;
begin
  // Check valid parameters
  if S = '' then
    begin
      Result := nil;
      exit;
    end;
  // Count
  L := 0;
  I := 1;
  repeat
    I := PosCharU(D, S, I);
    if I = 0 then
      break;
    Inc(L);
    Inc(I);
  until False;
  SetLength(Result, L + 1);
  if L = 0 then
    begin
      // No split
      Result[0] := S;
      exit;
    end;
  // Split
  L := 0;
  I := 1;
  repeat
    J := PosCharU(D, S, I);
    if J = 0 then
      begin
        Result[L] := CopyFromU(S, I);
        break;
      end;
    Result[L] := CopyRangeU(S, I, J - 1);
    Inc(L);
    I := J + 1;
  until False;
end;

function StrSplitChar(const S: String; const D: Char): StringArray;
var I, J, L : Integer;
begin
  // Check valid parameters
  if S = '' then
    begin
      Result := nil;
      exit;
    end;
  // Count
  L := 0;
  I := 1;
  repeat
    I := PosChar(D, S, I);
    if I = 0 then
      break;
    Inc(L);
    Inc(I);
  until False;
  SetLength(Result, L + 1);
  if L = 0 then
    begin
      // No split
      Result[0] := S;
      exit;
    end;
  // Split
  L := 0;
  I := 1;
  repeat
    J := PosChar(D, S, I);
    if J = 0 then
      begin
        Result[L] := CopyFrom(S, I);
        break;
      end;
    Result[L] := CopyRange(S, I, J - 1);
    Inc(L);
    I := J + 1;
  until False;
end;

{$IFDEF SupportAnsiString}
function StrSplitCharSetA(const S: AnsiString; const D: ByteCharSet): AnsiStringArray;
var I, J, L : Integer;
begin
  // Check valid parameters
  if S = '' then
    begin
      Result := nil;
      exit;
    end;
  // Count
  L := 0;
  I := 1;
  repeat
    I := PosCharSetA(D, S, I);
    if I = 0 then
      break;
    Inc(L);
    Inc(I);
  until False;
  SetLength(Result, L + 1);
  if L = 0 then
    begin
      // No split
      Result[0] := S;
      exit;
    end;
  // Split
  L := 0;
  I := 1;
  repeat
    J := PosCharSetA(D, S, I);
    if J = 0 then
      begin
        Result[L] := CopyFromA(S, I);
        break;
      end;
    Result[L] := CopyRangeA(S, I, J - 1);
    Inc(L);
    I := J + 1;
  until False;
end;
{$ENDIF}

function StrSplitCharSetB(const S: RawByteString; const D: ByteCharSet): RawByteStringArray;
var I, J, L : Integer;
begin
  // Check valid parameters
  if S = '' then
    begin
      Result := nil;
      exit;
    end;
  // Count
  L := 0;
  I := 1;
  repeat
    I := PosCharSetB(D, S, I);
    if I = 0 then
      break;
    Inc(L);
    Inc(I);
  until False;
  SetLength(Result, L + 1);
  if L = 0 then
    begin
      // No split
      Result[0] := S;
      exit;
    end;
  // Split
  L := 0;
  I := 1;
  repeat
    J := PosCharSetB(D, S, I);
    if J = 0 then
      begin
        Result[L] := CopyFromB(S, I);
        break;
      end;
    Result[L] := CopyRangeB(S, I, J - 1);
    Inc(L);
    I := J + 1;
  until False;
end;

function StrSplitCharSetU(const S: UnicodeString; const D: ByteCharSet): UnicodeStringArray;
var I, J, L : Integer;
begin
  // Check valid parameters
  if S = '' then
    begin
      Result := nil;
      exit;
    end;
  // Count
  L := 0;
  I := 1;
  repeat
    I := PosCharSetU(D, S, I);
    if I = 0 then
      break;
    Inc(L);
    Inc(I);
  until False;
  SetLength(Result, L + 1);
  if L = 0 then
    begin
      // No split
      Result[0] := S;
      exit;
    end;
  // Split
  L := 0;
  I := 1;
  repeat
    J := PosCharSetU(D, S, I);
    if J = 0 then
      begin
        Result[L] := CopyFromU(S, I);
        break;
      end;
    Result[L] := CopyRangeU(S, I, J - 1);
    Inc(L);
    I := J + 1;
  until False;
end;

function StrSplitCharSet(const S: String; const D: ByteCharSet): StringArray;
var I, J, L : Integer;
begin
  // Check valid parameters
  if S = '' then
    begin
      Result := nil;
      exit;
    end;
  // Count
  L := 0;
  I := 1;
  repeat
    I := PosCharSet(D, S, I);
    if I = 0 then
      break;
    Inc(L);
    Inc(I);
  until False;
  SetLength(Result, L + 1);
  if L = 0 then
    begin
      // No split
      Result[0] := S;
      exit;
    end;
  // Split
  L := 0;
  I := 1;
  repeat
    J := PosCharSet(D, S, I);
    if J = 0 then
      begin
        Result[L] := CopyFrom(S, I);
        break;
      end;
    Result[L] := CopyRange(S, I, J - 1);
    Inc(L);
    I := J + 1;
  until False;
end;

{$IFDEF SupportAnsiString}
function StrSplitWords(const S: AnsiString; const C: ByteCharSet): AnsiStringArray;
var P, Q : PAnsiChar;
    L, M : Integer;
    N    : Integer;
    T    : AnsiString;
begin
  Result := nil;
  L := Length(S);
  P := Pointer(S);
  Q := P;
  M := 0;
  N := 0;
  while L > 0 do
    if P^ in C then
      begin
        Inc(P);
        Dec(L);
        Inc(M);
      end else
      begin
        if M > 0 then
          begin
            SetLength(T, M);
            MoveMem(Q^, Pointer(T)^, M);
            SetLength(Result, N + 1);
            Result[N] := T;
            Inc(N);
          end;
        M := 0;
        Inc(P);
        Dec(L);
        Q := P;
      end;
  if M > 0 then
    begin
      SetLength(T, M);
      MoveMem(Q^, Pointer(T)^, M);
      SetLength(Result, N + 1);
      Result[N] := T;
    end;
end;
{$ENDIF}

{$IFDEF SupportAnsiString}
function StrJoinA(const S: array of AnsiString; const D: AnsiString): AnsiString;
var I, L, M, C : Integer;
    P : PAnsiChar;
    T : AnsiString;
begin
  L := Length(S);
  if L = 0 then
    begin
      Result := '';
      exit;
    end;
  M := Length(D);
  SetLength(Result, StringsTotalLengthA(S) + (L - 1) * M);
  P := Pointer(Result);
  for I := 0 to L - 1 do
    begin
      if (I > 0) and (M > 0) then
        begin
          MoveMem(Pointer(D)^, P^, M);
          Inc(P, M);
        end;
      T := S[I];
      C := Length(T);
      if C > 0 then
        begin
          MoveMem(Pointer(T)^, P^, C);
          Inc(P, C);
        end;
    end;
end;
{$ENDIF}

function StrJoinB(const S: array of RawByteString; const D: RawByteString): RawByteString;
var I, L, M, C : Integer;
    P : PByteChar;
    T : RawByteString;
begin
  L := Length(S);
  if L = 0 then
    begin
      Result := '';
      exit;
    end;
  M := Length(D);
  SetLength(Result, StringsTotalLengthB(S) + (L - 1) * M);
  P := Pointer(Result);
  for I := 0 to L - 1 do
    begin
      if (I > 0) and (M > 0) then
        begin
          MoveMem(Pointer(D)^, P^, M);
          Inc(P, M);
        end;
      T := S[I];
      C := Length(T);
      if C > 0 then
        begin
          MoveMem(Pointer(T)^, P^, C);
          Inc(P, C);
        end;
    end;
end;

function StrJoinU(const S: array of UnicodeString; const D: UnicodeString): UnicodeString;
var I, L, M, C : Integer;
    P : PWideChar;
    T : UnicodeString;
begin
  L := Length(S);
  if L = 0 then
    begin
      Result := '';
      exit;
    end;
  M := Length(D);
  SetLength(Result, StringsTotalLengthU(S) + (L - 1) * M);
  P := Pointer(Result);
  for I := 0 to L - 1 do
    begin
      if (I > 0) and (M > 0) then
        begin
          MoveMem(Pointer(D)^, P^, M * SizeOf(WideChar));
          Inc(P, M);
        end;
      T := S[I];
      C := Length(T);
      if C > 0 then
        begin
          MoveMem(Pointer(T)^, P^, C * SizeOf(WideChar));
          Inc(P, C);
        end;
    end;
end;

function StrJoin(const S: array of String; const D: String): String;
var I, L, M, C : Integer;
    P : PChar;
    T : String;
begin
  L := Length(S);
  if L = 0 then
    begin
      Result := '';
      exit;
    end;
  M := Length(D);
  SetLength(Result, StringsTotalLength(S) + (L - 1) * M);
  P := Pointer(Result);
  for I := 0 to L - 1 do
    begin
      if (I > 0) and (M > 0) then
        begin
          MoveMem(Pointer(D)^, P^, M * SizeOf(Char));
          Inc(P, M);
        end;
      T := S[I];
      C := Length(T);
      if C > 0 then
        begin
          MoveMem(Pointer(T)^, P^, C * SizeOf(Char));
          Inc(P, C);
        end;
    end;
end;

{$IFDEF SupportAnsiString}
function StrJoinCharA(const S: array of AnsiString; const D: AnsiChar): AnsiString;
var I, L, C : Integer;
    P : PAnsiChar;
    T : AnsiString;
begin
  L := Length(S);
  if L = 0 then
    begin
      Result := '';
      exit;
    end;
  SetLength(Result, StringsTotalLengthA(S) + L - 1);
  P := Pointer(Result);
  for I := 0 to L - 1 do
    begin
      if I > 0 then
        begin
          P^ := D;
          Inc(P);
        end;
      T := S[I];
      C := Length(T);
      if C > 0 then
        begin
          MoveMem(Pointer(T)^, P^, C);
          Inc(P, C);
        end;
    end;
end;
{$ENDIF}

function StrJoinCharB(const S: array of RawByteString; const D: ByteChar): RawByteString;
var I, L, C : Integer;
    P : PByteChar;
    T : RawByteString;
begin
  L := Length(S);
  if L = 0 then
    begin
      Result := '';
      exit;
    end;
  SetLength(Result, StringsTotalLengthB(S) + L - 1);
  P := Pointer(Result);
  for I := 0 to L - 1 do
    begin
      if I > 0 then
        begin
          P^ := D;
          Inc(P);
        end;
      T := S[I];
      C := Length(T);
      if C > 0 then
        begin
          MoveMem(Pointer(T)^, P^, C);
          Inc(P, C);
        end;
    end;
end;

function StrJoinCharU(const S: array of UnicodeString; const D: WideChar): UnicodeString;
var I, L, C : Integer;
    P : PWideChar;
    T : UnicodeString;
begin
  L := Length(S);
  if L = 0 then
    begin
      Result := '';
      exit;
    end;
  SetLength(Result, StringsTotalLengthU(S) + L - 1);
  P := Pointer(Result);
  for I := 0 to L - 1 do
    begin
      if I > 0 then
        begin
          P^ := D;
          Inc(P);
        end;
      T := S[I];
      C := Length(T);
      if C > 0 then
        begin
          MoveMem(Pointer(T)^, P^, C * SizeOf(WideChar));
          Inc(P, C);
        end;
    end;
end;

function StrJoinChar(const S: array of String; const D: Char): String;
var I, L, C : Integer;
    P : PChar;
    T : String;
begin
  L := Length(S);
  if L = 0 then
    begin
      Result := '';
      exit;
    end;
  SetLength(Result, StringsTotalLength(S) + L - 1);
  P := Pointer(Result);
  for I := 0 to L - 1 do
    begin
      if I > 0 then
        begin
          P^ := D;
          Inc(P);
        end;
      T := S[I];
      C := Length(T);
      if C > 0 then
        begin
          MoveMem(Pointer(T)^, P^, C * SizeOf(Char));
          Inc(P, C);
        end;
    end;
end;



{                                                                              }
{ Quoting                                                                      }
{                                                                              }
{$IFDEF SupportAnsiString}
function StrHasSurroundingQuotesA(const S: AnsiString; const Quotes: ByteCharSet): Boolean;
var P : PAnsiChar;
    Q : AnsiChar;
    L : Integer;
begin
  Result := False;
  L := Length(S);
  if L >= 2 then
    begin
      P := Pointer(S);
      Q := P^;
      if Q in Quotes then
        begin
          Inc(P, L - 1);
          if P^ = Q then
            Result := True;
        end;
    end;
end;
{$ENDIF}

function StrHasSurroundingQuotesB(const S: RawByteString; const Quotes: ByteCharSet): Boolean;
var P : PByteChar;
    Q : ByteChar;
    L : Integer;
begin
  Result := False;
  L := Length(S);
  if L >= 2 then
    begin
      P := Pointer(S);
      Q := P^;
      if Q in Quotes then
        begin
          Inc(P, L - 1);
          if P^ = Q then
            Result := True;
        end;
    end;
end;

function StrHasSurroundingQuotesU(const S: UnicodeString; const Quotes: ByteCharSet): Boolean;
var P : PWideChar;
    Q : WideChar;
    L : Integer;
begin
  Result := False;
  L := Length(S);
  if L >= 2 then
    begin
      P := Pointer(S);
      Q := P^;
      if WideCharInCharSet(Q, Quotes) then
        begin
          Inc(P, L - 1);
          if P^ = Q then
            Result := True;
        end;
    end;
end;

function StrHasSurroundingQuotes(const S: String; const Quotes: ByteCharSet): Boolean;
var P : PChar;
    Q : Char;
    L : Integer;
begin
  Result := False;
  L := Length(S);
  if L >= 2 then
    begin
      P := Pointer(S);
      Q := P^;
      {$IFDEF CharIsWide}
      if Ord(Q) <= $FF then
      {$ENDIF}
        if ByteChar(Byte(Q)) in Quotes then
          begin
            Inc(P, L - 1);
            if P^ = Q then
              Result := True;
          end;
    end;
end;

{$IFDEF SupportAnsiString}
function StrRemoveSurroundingQuotesA(const S: AnsiString; const Quotes: ByteCharSet): AnsiString;
begin
  if StrHasSurroundingQuotesA(S, Quotes) then
    Result := Copy(S, 2, Length(S) - 2)
  else
    Result := S;
end;
{$ENDIF}

function StrRemoveSurroundingQuotesB(const S: RawByteString; const Quotes: ByteCharSet): RawByteString;
begin
  if StrHasSurroundingQuotesB(S, Quotes) then
    Result := Copy(S, 2, Length(S) - 2)
  else
    Result := S;
end;

function StrRemoveSurroundingQuotesU(const S: UnicodeString; const Quotes: ByteCharSet): UnicodeString;
begin
  if StrHasSurroundingQuotesU(S, Quotes) then
    Result := Copy(S, 2, Length(S) - 2)
  else
    Result := S;
end;

function StrRemoveSurroundingQuotes(const S: String; const Quotes: ByteCharSet): String;
begin
  if StrHasSurroundingQuotes(S, Quotes) then
    Result := Copy(S, 2, Length(S) - 2)
  else
    Result := S;
end;

{$IFDEF SupportAnsiString}
function StrQuoteA(const S: AnsiString; const Quote: AnsiChar): AnsiString;
begin
  Result := Quote + StrReplaceA(Quote, DupCharA(Quote, 2), S) + Quote;
end;
{$ENDIF}

function StrQuoteB(const S: RawByteString; const Quote: AnsiChar): RawByteString;
begin
  Result := Quote + StrReplaceB(Quote, DupCharB(Quote, 2), S) + Quote;
end;

function StrQuoteU(const S: UnicodeString; const Quote: WideChar): UnicodeString;
begin
  Result := Quote + StrReplaceU(Quote, DupCharU(Quote, 2), S) + Quote;
end;

function StrQuote(const S: String; const Quote: Char): String;
begin
  Result := Quote + StrReplace(Quote, DupChar(Quote, 2), S) + Quote;
end;

{$IFDEF SupportAnsiString}
function StrUnquoteA(const S: AnsiString): AnsiString;
var Quote : AnsiChar;
begin
  if not StrHasSurroundingQuotesA(S, csQuotes) then
    begin
      Result := S;
      exit;
    end;
  Quote := S[1];
  Result := StrRemoveSurroundingQuotesA(S, csQuotes);
  Result := StrReplaceA(DupCharA(Quote, 2), Quote, Result);
end;
{$ENDIF}

function StrUnquoteB(const S: RawByteString): RawByteString;
var Quote : ByteChar;
begin
  if not StrHasSurroundingQuotesB(S, csQuotes) then
    begin
      Result := S;
      exit;
    end;
  Quote := S[1];
  Result := StrRemoveSurroundingQuotesB(S, csQuotes);
  Result := StrReplaceB(DupCharB(Quote, 2), Quote, Result);
end;

function StrUnquoteU(const S: UnicodeString): UnicodeString;
var Quote : WideChar;
begin
  if not StrHasSurroundingQuotesU(S, csQuotes) then
    begin
      Result := S;
      exit;
    end;
  Quote := S[1];
  Result := StrRemoveSurroundingQuotesU(S, csQuotes);
  Result := StrReplaceU(DupCharU(Quote, 2), Quote, Result);
end;

function StrUnquote(const S: String): String;
var Quote : Char;
begin
  if not StrHasSurroundingQuotes(S, csQuotes) then
    begin
      Result := S;
      exit;
    end;
  Quote := S[1];
  Result := StrRemoveSurroundingQuotes(S, csQuotes);
  Result := StrReplace(DupChar(Quote, 2), Quote, Result);
end;

{$IFDEF SupportAnsiString}
function StrMatchQuotedStrA(const S: AnsiString; const ValidQuotes: ByteCharSet;
    const Index: Integer): Integer;
var Quote : AnsiChar;
    I, L  : Integer;
    R     : Boolean;
begin
  L := Length(S);
  if (Index < 1) or (L < Index + 1) or not (S[Index] in ValidQuotes) then
    begin
      Result := 0;
      exit;
    end;
  Quote := S[Index];
  I := Index + 1;
  R := False;
  repeat
    I := PosCharA(Quote, S, I);
    if I = 0 then // no closing quote
      begin
        Result := 0;
        exit;
      end else
      if I = L then // closing quote is last character
        R := True else
        if S[I + 1] <> Quote then // not double quoted
          R := True
        else
          Inc(I, 2);
  until R;
  Result := I - Index + 1;
end;

function StrIsQuotedStrA(const S: AnsiString; const ValidQuotes: ByteCharSet): Boolean;
var L : Integer;
begin
  L := Length(S);
  if (L < 2) or (S[1] <> S[L]) or not (S[1] in ValidQuotes) then
    Result := False
  else
    Result := StrMatchQuotedStrA(S, ValidQuotes) = L;
end;

function StrFindClosingQuoteA(const S: AnsiString; const OpenQuotePos: Integer): Integer;
var I : Integer;
    OpenQuote : AnsiChar;
    R : Boolean;
begin
  if (OpenQuotePos <= 0) or (OpenQuotePos > Length(S)) then
    begin
      Result := 0;
      exit;
    end;
  I := OpenQuotePos;
  OpenQuote := S[I];
  repeat
    I := PosCharA(OpenQuote, S, I + 1);
    if I = 0 then
      begin
        Result := 0;
        exit;
      end;
    R := (I = Length(S)) or (S[I + 1] <> OpenQuote);
    if not R then
      Inc(I);
  until R;
  Result := I;
end;
{$ENDIF}



{                                                                              }
{ Bracketing                                                                   }
{                                                                              }
function StrFindClosingBracketB(const S: RawByteString;
    const OpenBracketPos: Integer; const CloseBracket: ByteChar): Integer;
var OpenBracket : ByteChar;
    Brackets    : ByteCharSet;
    I, C        : Integer;
begin
  Result := 0;
  I := OpenBracketPos;
  if (I <= 0) or (I > Length(S)) then
    exit;
  OpenBracket := S[OpenBracketPos];
  Brackets := [OpenBracket, CloseBracket];
  C := 1;
  repeat
    I := PosCharSetB(Brackets, S, I + 1);
    if I = 0 then
      exit;
    if S[I] = OpenBracket then
      Inc(C)
    else
      Dec(C);
  until C = 0;
  Result := I;
end;



{                                                                              }
{ Escaping                                                                     }
{                                                                              }
function StrHexEscapeB(const S: RawByteString; const C: ByteCharSet;
    const EscPrefix: RawByteString; const EscSuffix: RawByteString;
    const UpperHex: Boolean; const TwoDigitHex: Boolean): RawByteString;
var I, J   : Integer;
    HexStr : RawByteString;
begin
  Result := '';
  J := 1;
  I := PosCharSetB(C, S);
  while I > 0 do
    begin
      if TwoDigitHex then
        HexStr := Word32ToHexB(Ord(S[I]), 2)
      else
        HexStr := Word32ToHexB(Ord(S[I]), 1);
      if UpperHex then
        AsciiConvertUpperB(HexStr)
      else
        AsciiConvertLowerB(HexStr);
      Result := Result + CopyRangeB(S, J, I - 1) +
                EscPrefix + HexStr + EscSuffix;
      J := I + 1;
      I := PosCharSetB(C, S, J);
    end;
  if J = 1 then
    Result := S
  else
    Result := Result + CopyFromB(S, J);
end;

function StrHexUnescapeB(const S: RawByteString; const EscPrefix: RawByteString;
    const AsciiCaseSensitive: Boolean): RawByteString;
var I, J, L, M : Integer;
    V : Byte;
    R : RawByteString;
    H1, H2 : ByteChar;
    H2Ch : Boolean;
begin
  R := '';
  L := Length(S);
  if L = 0 then
    exit;
  M := Length(EscPrefix);
  if M = 0 then
    exit;
  // Replace
  J := 1;
  repeat
    I := PosStrB(EscPrefix, S, J, AsciiCaseSensitive);
    if I > 0 then
      begin
        R := R + CopyRangeB(S, J, I - 1);
        Inc(I, M);
        if I <= L then
          begin
            H1 := S[I];
            if IsHexByteCharDigit(H1) then
              begin
                H2Ch := False;
                if I < L then
                  begin
                    H2 := S[I + 1];
                    if IsHexByteCharDigit(H2) then
                      begin
                        V := HexByteCharDigitToInt(H1) * 16 + HexByteCharDigitToInt(H2);
                        R := R + ByteChar(V);
                        Inc(I, 2);
                        H2Ch := True;
                      end;
                  end;
                if not H2Ch then
                  begin
                    V := HexByteCharDigitToInt(H1);
                    R := R + ByteChar(V);
                    Inc(I);
                  end;
              end;
          end;
        J := I;
      end;
  until I = 0;
  if (I = 0) and (J = 0) then
    Result := S
  else
    Result := R + CopyFromB(S, J);
end;

{$IFDEF SupportAnsiString}
function StrCharEscapeA(const S: AnsiString; const C: array of AnsiChar;
    const EscPrefix: AnsiString; const EscSeq: array of AnsiString): AnsiString;
var I, J, L : Integer;
    F       : ByteCharSet;
    T       : AnsiChar;
    Lookup  : Array[AnsiChar] of Integer;
begin
  L := Length(C);
  if L = 0 then
    begin
      Result := S;
      exit;
    end;
  if L <> Length(EscSeq) then
    raise EStrInvalidArgument.Create('Invalid arguments');
  // Initialize lookup
  ZeroMem(Lookup, Sizeof(Lookup));
  F := [];
  for I := 0 to Length(C) - 1 do
    begin
      T := C[I];
      Include(F, T);
      Lookup[T] := I;
    end;
  // Replace
  Result := '';
  J := 1;
  I := PosCharSetA(F, S);
  while I > 0 do
    begin
      Result := Result + CopyRangeA(S, J, I - 1) +
                EscPrefix + EscSeq[Lookup[S[I]]];
      J := I + 1;
      I := PosCharSetA(F, S, J);
    end;
  if J = 1 then
    Result := S
  else
    Result := Result + CopyFromA(S, J);
end;

function StrCharUnescapeA(const S: AnsiString; const EscPrefix: AnsiString;
    const C: array of AnsiChar; const Replace: array of AnsiString;
    const PrefixAsciiCaseSensitive: Boolean;
    const AlwaysDropPrefix: Boolean): AnsiString;
var I, J, L : Integer;
    F, G, M : Integer;
    D       : AnsiChar;
begin
  if High(C) <> High(Replace) then
    raise EStrInvalidArgument.Create('Invalid arguments');
  L := Length(EscPrefix);
  M := Length(S);
  if (L = 0) or (M <= L) then
    begin
      Result := S;
      exit;
    end;
  // Replace
  Result := '';
  J := 1;
  repeat
    I := PosStrA(EscPrefix, S, J, PrefixAsciiCaseSensitive);
    if I > 0 then
      begin
        G := -1;
        if I < Length(S) then
          begin
            D := S[I + L];
            for F := 0 to High(C) do
              if C[F] = D then
                begin
                  G := F;
                  break;
                end;
          end;
        Result := Result + CopyRangeA(S, J, I - 1);
        if G >= 0 then
          Result := Result + Replace[G] else
          if not AlwaysDropPrefix then
            Result := Result + EscPrefix;
        J := I + L + 1;
      end;
  until I = 0;
  if (I = 0) and (J = 0) then
    Result := S
  else
    Result := Result + CopyFromA(S, J);
end;
{$ENDIF}

function StrCharUnescapeU(const S: UnicodeString; const EscPrefix: UnicodeString;
    const C: array of WideChar; const Replace: array of UnicodeString;
    const PrefixAsciiCaseSensitive: Boolean;
    const AlwaysDropPrefix: Boolean): UnicodeString;
var I, J, L : Integer;
    F, G, M : Integer;
    D       : WideChar;
begin
  if High(C) <> High(Replace) then
    raise EStrInvalidArgument.Create('Invalid arguments');
  L := Length(EscPrefix);
  M := Length(S);
  if (L = 0) or (M <= L) then
    begin
      Result := S;
      exit;
    end;
  // Replace
  Result := '';
  J := 1;
  repeat
    I := PosStrU(EscPrefix, S, J, PrefixAsciiCaseSensitive);
    if I > 0 then
      begin
        G := -1;
        if I < Length(S) then
          begin
            D := S[I + L];
            for F := 0 to High(C) do
              if C[F] = D then
                begin
                  G := F;
                  break;
                end;
          end;
        Result := Result + CopyRangeU(S, J, I - 1);
        if G >= 0 then
          Result := Result + Replace[G] else
          if not AlwaysDropPrefix then
            Result := Result + EscPrefix;
        J := I + L + 1;
      end;
  until I = 0;
  if (I = 0) and (J = 0) then
    Result := S
  else
    Result := Result + CopyFromU(S, J);
end;

{$IFDEF SupportAnsiString}
function StrCStyleEscapeA(const S: AnsiString): AnsiString;
begin
  Result := StrCharEscapeA(S,
      [AsciiCR, AsciiLF, AsciiNULL, AsciiBEL, AsciiBS, AsciiESC, AsciiHT,
       AsciiFF, AsciiVT, '\'], '\',
      ['n',     'l',     '0',       'a',      'b',     'e',      't',
       'f',     'v',     '\']);
end;

function StrCStyleUnescapeA(const S: AnsiString): AnsiString;
begin
  Result := StrCharUnescapeA(S, '\',
      ['n',     'l',     '0',       'a',      'b',     'e',      't',
       'f',     'v',     '\',     '''',      '"',      '?'],
      [AsciiCR, AsciiLF, AsciiNULL, AsciiBEL, AsciiBS, AsciiESC, AsciiHT,
       AsciiFF, AsciiVT, '\',     '''',      '"',      '?'], True, False);
  Result := StrHexUnescapeB(Result, '\x', True);
end;
{$ENDIF}



{                                                                              }
{ Prefix and Suffix                                                            }
{                                                                              }
{$IFDEF SupportAnsiString}
function StrInclPrefixA(const S: AnsiString; const Prefix: AnsiString;
  const AsciiCaseSensitive: Boolean): AnsiString;
begin
  if not StrMatchLeftA(S, Prefix, AsciiCaseSensitive) then
    Result := Prefix + S
  else
    Result := S;
end;
{$ENDIF}

function StrInclPrefixB(const S: RawByteString; const Prefix: RawByteString;
  const AsciiCaseSensitive: Boolean): RawByteString;
begin
  if not StrMatchLeftB(S, Prefix, AsciiCaseSensitive) then
    Result := Prefix + S
  else
    Result := S;
end;

function StrInclPrefixU(const S: UnicodeString; const Prefix: UnicodeString;
  const AsciiCaseSensitive: Boolean): UnicodeString;
begin
  if not StrMatchLeftU(S, Prefix, AsciiCaseSensitive) then
    Result := Prefix + S
  else
    Result := S;
end;

function StrInclPrefix(const S: String; const Prefix: String;
  const AsciiCaseSensitive: Boolean): String;
begin
  if not StrMatchLeft(S, Prefix, AsciiCaseSensitive) then
    Result := Prefix + S
  else
    Result := S;
end;

{$IFDEF SupportAnsiString}
function StrInclSuffixA(const S: AnsiString; const Suffix: AnsiString;
  const AsciiCaseSensitive: Boolean): AnsiString;
begin
  if not StrMatchRightA(S, Suffix, AsciiCaseSensitive) then
    Result := S + Suffix
  else
    Result := S;
end;
{$ENDIF}

function StrInclSuffixB(const S: RawByteString; const Suffix: RawByteString;
  const AsciiCaseSensitive: Boolean): RawByteString;
begin
  if not StrMatchRightB(S, Suffix, AsciiCaseSensitive) then
    Result := S + Suffix
  else
    Result := S;
end;

function StrInclSuffixU(const S: UnicodeString; const Suffix: UnicodeString;
  const AsciiCaseSensitive: Boolean): UnicodeString;
begin
  if not StrMatchRightU(S, Suffix, AsciiCaseSensitive) then
    Result := S + Suffix
  else
    Result := S;
end;

function StrInclSuffix(const S: String; const Suffix: String;
  const AsciiCaseSensitive: Boolean): String;
begin
  if not StrMatchRight(S, Suffix, AsciiCaseSensitive) then
    Result := S + Suffix
  else
    Result := S;
end;

{$IFDEF SupportAnsiString}
function StrExclPrefixA(const S: AnsiString; const Prefix: AnsiString;
  const AsciiCaseSensitive: Boolean): AnsiString;
begin
  if StrMatchLeftA(S, Prefix, AsciiCaseSensitive) then
    Result := CopyFromA(S, Length(Prefix) + 1)
  else
    Result := S;
end;
{$ENDIF}

function StrExclPrefixB(const S: RawByteString; const Prefix: RawByteString;
  const AsciiCaseSensitive: Boolean): RawByteString;
begin
  if StrMatchLeftB(S, Prefix, AsciiCaseSensitive) then
    Result := CopyFromB(S, Length(Prefix) + 1)
  else
    Result := S;
end;

function StrExclPrefixU(const S: UnicodeString; const Prefix: UnicodeString;
  const AsciiCaseSensitive: Boolean): UnicodeString;
begin
  if StrMatchLeftU(S, Prefix, AsciiCaseSensitive) then
    Result := CopyFromU(S, Length(Prefix) + 1)
  else
    Result := S;
end;

function StrExclPrefix(const S: String; const Prefix: String;
  const AsciiCaseSensitive: Boolean): String;
begin
  if StrMatchLeft(S, Prefix, AsciiCaseSensitive) then
    Result := CopyFrom(S, Length(Prefix) + 1)
  else
    Result := S;
end;

{$IFDEF SupportAnsiString}
function StrExclSuffixA(const S: AnsiString; const Suffix: AnsiString;
  const AsciiCaseSensitive: Boolean): AnsiString;
begin
  if StrMatchRightA(S, Suffix, AsciiCaseSensitive) then
    Result := Copy(S, 1, Length(S) - Length(Suffix))
  else
    Result := S;
end;
{$ENDIF}

function StrExclSuffixB(const S: RawByteString; const Suffix: RawByteString;
  const AsciiCaseSensitive: Boolean): RawByteString;
begin
  if StrMatchRightB(S, Suffix, AsciiCaseSensitive) then
    Result := Copy(S, 1, Length(S) - Length(Suffix))
  else
    Result := S;
end;

function StrExclSuffixU(const S: UnicodeString; const Suffix: UnicodeString;
  const AsciiCaseSensitive: Boolean): UnicodeString;
begin
  if StrMatchRightU(S, Suffix, AsciiCaseSensitive) then
    Result := Copy(S, 1, Length(S) - Length(Suffix))
  else
    Result := S;
end;

function StrExclSuffix(const S: String; const Suffix: String;
  const AsciiCaseSensitive: Boolean): String;
begin
  if StrMatchRight(S, Suffix, AsciiCaseSensitive) then
    Result := Copy(S, 1, Length(S) - Length(Suffix))
  else
    Result := S;
end;

{$IFDEF SupportAnsiString}
procedure StrEnsurePrefixA(var S: AnsiString; const Prefix: AnsiString;
  const AsciiCaseSensitive: Boolean);
var L, M : Integer;
    P : PAnsiChar;
begin
  if (Prefix <> '') and not StrMatchLeftA(S, Prefix, AsciiCaseSensitive) then
    begin
      L := Length(S);
      M := Length(Prefix);
      SetLength(S, L + M);
      if L > 0 then
        begin
          P := Pointer(S);
          Inc(P, M);
          MoveMem(Pointer(S)^, P^, L);
        end;
      MoveMem(Pointer(Prefix)^, Pointer(S)^, M);
    end;
end;
{$ENDIF}

procedure StrEnsurePrefixB(var S: RawByteString; const Prefix: RawByteString;
  const AsciiCaseSensitive: Boolean);
begin
  if (Prefix <> '') and not StrMatchLeftB(S, Prefix, AsciiCaseSensitive) then
    S := Prefix + S;
end;

procedure StrEnsurePrefixU(var S: UnicodeString; const Prefix: UnicodeString;
  const AsciiCaseSensitive: Boolean);
begin
  if (Prefix <> '') and not StrMatchLeftU(S, Prefix, AsciiCaseSensitive) then
    S := Prefix + S;
end;

procedure StrEnsurePrefix(var S: String; const Prefix: String;
  const AsciiCaseSensitive: Boolean);
begin
  if (Prefix <> '') and not StrMatchLeft(S, Prefix, AsciiCaseSensitive) then
    S := Prefix + S;
end;

{$IFDEF SupportAnsiString}
procedure StrEnsureSuffixA(var S: AnsiString; const Suffix: AnsiString;
  const AsciiCaseSensitive: Boolean);
var L, M : Integer;
    P : PAnsiChar;
begin
  if (Suffix <> '') and not StrMatchRightA(S, Suffix, AsciiCaseSensitive) then
    begin
      L := Length(S);
      M := Length(Suffix);
      SetLength(S, L + M);
      P := Pointer(S);
      Inc(P, L);
      MoveMem(Pointer(Suffix)^, P^, M);
    end;
end;
{$ENDIF}

procedure StrEnsureSuffixB(var S: RawByteString; const Suffix: RawByteString;
  const AsciiCaseSensitive: Boolean);
begin
  if (Suffix <> '') and not StrMatchRightB(S, Suffix, AsciiCaseSensitive) then
    S := S + Suffix;
end;

procedure StrEnsureSuffixU(var S: UnicodeString; const Suffix: UnicodeString;
  const AsciiCaseSensitive: Boolean);
begin
  if (Suffix <> '') and not StrMatchRightU(S, Suffix, AsciiCaseSensitive) then
    S := S + Suffix;
end;

procedure StrEnsureSuffix(var S: String; const Suffix: String;
  const AsciiCaseSensitive: Boolean);
begin
  if (Suffix <> '') and not StrMatchRight(S, Suffix, AsciiCaseSensitive) then
    S := S + Suffix;
end;

{$IFDEF SupportAnsiString}
procedure StrEnsureNoPrefixA(var S: AnsiString; const Prefix: AnsiString;
  const AsciiCaseSensitive: Boolean);
var L, M : Integer;
    P : PAnsiChar;
begin
  if StrMatchLeftA(S, Prefix, AsciiCaseSensitive) then
    begin
      L := Length(S);
      M := Length(Prefix);
      P := Pointer(S);
      Inc(P, M);
      MoveMem(P^, Pointer(S)^, L - M);
      SetLength(S, L - M);
    end;
end;
{$ENDIF}

procedure StrEnsureNoPrefixB(var S: RawByteString; const Prefix: RawByteString;
  const AsciiCaseSensitive: Boolean);
begin
  if StrMatchLeftB(S, Prefix, AsciiCaseSensitive) then
    Delete(S, 1, Length(Prefix));
end;

procedure StrEnsureNoPrefixU(var S: UnicodeString; const Prefix: UnicodeString;
  const AsciiCaseSensitive: Boolean);
begin
  if StrMatchLeftU(S, Prefix, AsciiCaseSensitive) then
    Delete(S, 1, Length(Prefix));
end;

procedure StrEnsureNoPrefix(var S: String; const Prefix: String;
  const AsciiCaseSensitive: Boolean);
begin
  if StrMatchLeft(S, Prefix, AsciiCaseSensitive) then
    Delete(S, 1, Length(Prefix));
end;

{$IFDEF SupportAnsiString}
procedure StrEnsureNoSuffixA(var S: AnsiString; const Suffix: AnsiString;
  const AsciiCaseSensitive: Boolean);
begin
  if StrMatchRightA(S, Suffix, AsciiCaseSensitive) then
    SetLength(S, Length(S) - Length(Suffix));
end;
{$ENDIF}

procedure StrEnsureNoSuffixB(var S: RawByteString; const Suffix: RawByteString;
  const AsciiCaseSensitive: Boolean);
begin
  if StrMatchRightB(S, Suffix, AsciiCaseSensitive) then
    SetLength(S, Length(S) - Length(Suffix));
end;

procedure StrEnsureNoSuffixU(var S: UnicodeString; const Suffix: UnicodeString;
  const AsciiCaseSensitive: Boolean);
begin
  if StrMatchRightU(S, Suffix, AsciiCaseSensitive) then
    SetLength(S, Length(S) - Length(Suffix));
end;

procedure StrEnsureNoSuffix(var S: String; const Suffix: String;
  const AsciiCaseSensitive: Boolean);
begin
  if StrMatchRight(S, Suffix, AsciiCaseSensitive) then
    SetLength(S, Length(S) - Length(Suffix));
end;



{                                                                              }
{ Reverse                                                                      }
{                                                                              }
{$IFDEF SupportAnsiString}
function StrReverseA(const S: AnsiString): AnsiString;
var I, L : Integer;
    P, Q : PAnsiChar;
begin
  L := Length(S);
  if L = 0 then
    begin
      Result := '';
      exit;
    end;
  if L = 1 then
    begin
      Result := S;
      exit;
    end;
  SetLength(Result, L);
  P := Pointer(S);
  Q := Pointer(Result);
  Inc(Q, L - 1);
  for I := 1 to L do
    begin
      Q^ := P^;
      Dec(Q);
      Inc(P);
    end;
end;
{$ENDIF}

function StrReverseB(const S: RawByteString): RawByteString;
var I, L : Integer;
    P, Q : PByteChar;
begin
  L := Length(S);
  if L = 0 then
    begin
      Result := '';
      exit;
    end;
  if L = 1 then
    begin
      Result := S;
      exit;
    end;
  SetLength(Result, L);
  P := Pointer(S);
  Q := Pointer(Result);
  Inc(Q, L - 1);
  for I := 1 to L do
    begin
      Q^ := P^;
      Dec(Q);
      Inc(P);
    end;
end;

function StrReverseU(const S: UnicodeString): UnicodeString;
var I, L : Integer;
    P, Q : PWideChar;
begin
  L := Length(S);
  if L = 0 then
    begin
      Result := '';
      exit;
    end;
  if L = 1 then
    begin
      Result := S;
      exit;
    end;
  SetLength(Result, L);
  P := Pointer(S);
  Q := Pointer(Result);
  Inc(Q, L - 1);
  for I := 1 to L do
    begin
      Q^ := P^;
      Dec(Q);
      Inc(P);
    end;
end;

function StrReverse(const S: String): String;
var I, L : Integer;
    P, Q : PChar;
begin
  L := Length(S);
  if L = 0 then
    begin
      Result := '';
      exit;
    end;
  if L = 1 then
    begin
      Result := S;
      exit;
    end;
  SetLength(Result, L);
  P := Pointer(S);
  Q := Pointer(Result);
  Inc(Q, L - 1);
  for I := 1 to L do
    begin
      Q^ := P^;
      Dec(Q);
      Inc(P);
    end;
end;



{                                                                              }
{ Type conversion                                                              }
{                                                                              }
{$IFDEF DELPHI5}
function StrToFloatDef(const S: String; const Default: Extended): Extended;
begin
  try
    Result := StrToFloat(S);
  except
    Result := Default;
  end;
end;
{$ELSE}
function StrToFloatDef(const S: String; const Default: Extended): Extended;
begin
  if not TryStrToFloat(S, Result) then
    Result := Default;
end;
{$ENDIF}

{$IFDEF SupportAnsiString}
function BooleanToStrA(const B: Boolean): AnsiString;
begin
  if B then
    Result := 'True'
  else
    Result := 'False';
end;
{$ENDIF}

function BooleanToStrB(const B: Boolean): RawByteString;
begin
  if B then
    Result := 'True'
  else
    Result := 'False';
end;

function BooleanToStrU(const B: Boolean): UnicodeString;
begin
  if B then
    Result := 'True'
  else
    Result := 'False';
end;

function BooleanToStr(const B: Boolean): String;
begin
  if B then
    Result := 'True'
  else
    Result := 'False';
end;

{$IFDEF SupportAnsiString}
function StrToBooleanA(const S: AnsiString): Boolean;
begin
  Result := StrEqualNoAsciiCaseA(S, 'True');
end;
{$ENDIF}

function StrToBooleanB(const S: RawByteString): Boolean;
begin
  Result := StrEqualNoAsciiCaseB(S, 'True');
end;

function StrToBooleanU(const S: UnicodeString): Boolean;
begin
  Result := StrEqualNoAsciiCaseU(S, 'True');
end;

function StrToBoolean(const S: String): Boolean;
begin
  Result := StrEqualNoAsciiCase(S, 'True');
end;



{                                                                              }
{ Dynamic array functions                                                      }
{                                                                              }
{$IFDEF SupportAnsiString}
function StringsTotalLengthA(const S: array of AnsiString): Integer;
var I : Integer;
begin
  Result := 0;
  for I := 0 to Length(S) - 1 do
    Inc(Result, Length(S[I]));
end;
{$ENDIF}

function StringsTotalLengthB(const S: array of RawByteString): Integer;
var I : Integer;
begin
  Result := 0;
  for I := 0 to Length(S) - 1 do
    Inc(Result, Length(S[I]));
end;

function StringsTotalLengthU(const S: array of UnicodeString): Integer;
var I : Integer;
begin
  Result := 0;
  for I := 0 to Length(S) - 1 do
    Inc(Result, Length(S[I]));
end;

function StringsTotalLength(const S: array of String): Integer;
var I : Integer;
begin
  Result := 0;
  for I := 0 to Length(S) - 1 do
    Inc(Result, Length(S[I]));
end;

{$IFDEF SupportAnsiString}
function PosNextNoCaseA(const Find: AnsiString; const V: array of AnsiString;
    const PrevPos: Integer; const IsSortedAscending: Boolean): Integer;
var I, L, H : Integer;
begin
  if IsSortedAscending then // binary search
    begin
      if MaxInt(PrevPos + 1, 0) = 0 then // find first
        begin
          L := 0;
          H := Length(V) - 1;
          repeat
            I := (L + H) div 2;
            if StrEqualNoAsciiCaseA(V[I], Find) then
              begin
                while (I > 0) and StrEqualNoAsciiCaseA(V[I - 1], Find) do
                  Dec(I);
                Result := I;
                exit;
              end else
            if StrCompareNoAsciiCaseA(V[I], Find) = 1 then
              H := I - 1 else
              L := I + 1;
          until L > H;
          Result := -1;
        end else // find next
        if PrevPos >= Length(V) - 1 then
          Result := -1 else
          if StrEqualNoAsciiCaseA(V[PrevPos + 1], Find) then
            Result := PrevPos + 1 else
            Result := -1;
    end else
    begin // linear search
      for I := MaxInt(PrevPos + 1, 0) to Length(V) - 1 do
        if StrEqualNoAsciiCaseA(V[I], Find) then
          begin
            Result := I;
            exit;
          end;
      Result := -1;
    end;
end;
{$ENDIF}

function PosNextNoCaseB(const Find: RawByteString; const V: array of RawByteString;
    const PrevPos: Integer; const IsSortedAscending: Boolean): Integer;
var I, L, H : Integer;
begin
  if IsSortedAscending then // binary search
    begin
      if MaxInt(PrevPos + 1, 0) = 0 then // find first
        begin
          L := 0;
          H := Length(V) - 1;
          repeat
            I := (L + H) div 2;
            if StrEqualNoAsciiCaseB(V[I], Find) then
              begin
                while (I > 0) and StrEqualNoAsciiCaseB(V[I - 1], Find) do
                  Dec(I);
                Result := I;
                exit;
              end else
            if StrCompareNoAsciiCaseB(V[I], Find) = 1 then
              H := I - 1 else
              L := I + 1;
          until L > H;
          Result := -1;
        end else // find next
        if PrevPos >= Length(V) - 1 then
          Result := -1 else
          if StrEqualNoAsciiCaseB(V[PrevPos + 1], Find) then
            Result := PrevPos + 1 else
            Result := -1;
    end else
    begin // linear search
      for I := MaxInt(PrevPos + 1, 0) to Length(V) - 1 do
        if StrEqualNoAsciiCaseB(V[I], Find) then
          begin
            Result := I;
            exit;
          end;
      Result := -1;
    end;
end;

function PosNextNoCaseU(const Find: UnicodeString; const V: array of UnicodeString;
    const PrevPos: Integer; const IsSortedAscending: Boolean): Integer;
var I, L, H : Integer;
begin
  if IsSortedAscending then // binary search
    begin
      if MaxInt(PrevPos + 1, 0) = 0 then // find first
        begin
          L := 0;
          H := Length(V) - 1;
          repeat
            I := (L + H) div 2;
            if StrEqualNoAsciiCaseU(V[I], Find) then
              begin
                while (I > 0) and StrEqualNoAsciiCaseU(V[I - 1], Find) do
                  Dec(I);
                Result := I;
                exit;
              end else
            if StrCompareNoAsciiCaseU(V[I], Find) = 1 then
              H := I - 1 else
              L := I + 1;
          until L > H;
          Result := -1;
        end else // find next
        if PrevPos >= Length(V) - 1 then
          Result := -1 else
          if StrEqualNoAsciiCaseU(V[PrevPos + 1], Find) then
            Result := PrevPos + 1 else
            Result := -1;
    end else
    begin // linear search
      for I := MaxInt(PrevPos + 1, 0) to Length(V) - 1 do
        if StrEqualNoAsciiCaseU(V[I], Find) then
          begin
            Result := I;
            exit;
          end;
      Result := -1;
    end;
end;



{                                                                              }
{ Natural language                                                             }
{                                                                              }
function StorageSize(const Bytes: Int64; const ShortFormat: Boolean): String;
var Size, Suffix : String;
    Fmt          : String;
    Len          : Integer;
begin
  Fmt := iif(ShortFormat, '%1.0f', '%0.1f');
  if Bytes < 1024 then
    begin
      Size := IntToStr(Bytes);
      Suffix := iif(ShortFormat, 'B', 'bytes');
    end else
  if Bytes < 1024 * 1024 then
    begin
      Size := Format(Fmt, [Bytes / 1024.0]);
      Suffix := iif(ShortFormat, 'K', 'KB');
    end else
  if Bytes < 1024 * 1024 * 1024 then
    begin
      Size := Format(Fmt, [Bytes / (1024.0 * 1024.0)]);
      Suffix := iif(ShortFormat, 'M', 'MB');
    end else
  if Bytes < Int64(1024) * 1024 * 1024 * 1024 then
    begin
      Size := Format(Fmt, [Bytes / (1024.0 * 1024.0 * 1024.0)]);
      Suffix := iif(ShortFormat, 'G', 'GB');
    end else
  if Bytes < Int64(1024) * 1024 * 1024 * 1024 * 1024 then
    begin
      Size := Format(Fmt, [Bytes / (1024.0 * 1024.0 * 1024.0 * 1024.0)]);
      Suffix := iif(ShortFormat, 'T', 'TB');
    end
  else
    begin
      Size := Format(Fmt, [Bytes / (1024.0 * 1024.0 * 1024.0 * 1024.0 * 1024.0)]);
      Suffix := iif(ShortFormat, 'P', 'PB');
    end;
  Len := Length(Size);
  if Copy(Size, Len - 1, 2) = '.0' then
    SetLength(Size, Len - 2);
  Result := Size + ' ' + Suffix;
end;

function TransferRate(const Bytes, MillisecondsElapsed: Int64;
    const ShortFormat: Boolean): String;
begin
  if MillisecondsElapsed <= 0 then
    Result := ''
  else
    Result := StorageSize(Trunc(Bytes / (MillisecondsElapsed / 1000.0)), ShortFormat) + '/s';
end;




{                                                                              }
{ Test cases                                                                   }
{                                                                              }
{$IFDEF STRINGS_TEST}
{$ASSERTIONS ON}
procedure Test;
var {$IFDEF SupportAnsiString}
    C    : AnsiChar;
    {$ENDIF}
    D    : WideChar;
    E    : Char;
    {$IFDEF SupportAnsiString}
    S, T : AnsiString;
    {$ENDIF}
    W, X : UnicodeString;
    Y, Z : String;
    {$IFDEF SupportAnsiString}
    L    : AnsiStringArray;
    I    : Integer;
    {$ENDIF}
begin
  { CharMatch                                                                  }
  Assert(CharMatchB(AnsiChar('A'), AnsiChar('a'), False));
  Assert(CharMatchB(AnsiChar('a'), AnsiChar('A'), False));
  Assert(CharMatchB(AnsiChar('A'), AnsiChar('A'), False));

  { CharMatchNoAsciiCase                                                       }
  Assert(CharMatchNoAsciiCaseBW(AnsiChar('A'), 'a'), 'CharMatchNoAsciiCase');
  Assert(CharMatchNoAsciiCaseBW(AnsiChar('z'), 'Z'), 'CharMatchNoAsciiCase');
  Assert(CharMatchNoAsciiCaseBW(AnsiChar('1'), '1'), 'CharMatchNoAsciiCase');
  Assert(not CharMatchNoAsciiCaseBW(AnsiChar('A'), 'B'), 'CharMatchNoAsciiCase');
  Assert(not CharMatchNoAsciiCaseBW(AnsiChar('0'), 'A'), 'CharMatchNoAsciiCase');

  { CharSetMatchChar                                                           }
  Assert(CharSetMatchCharB([AnsiChar('a')..AnsiChar('z')], AnsiChar('a'), False));
  Assert(CharSetMatchCharB([AnsiChar('a')..AnsiChar('z')], AnsiChar('A'), False));
  Assert(not CharSetMatchCharB([AnsiChar('a')..AnsiChar('z')], AnsiChar('-'), False));
  Assert(not CharSetMatchCharB([AnsiChar('a')..AnsiChar('z')], AnsiChar('A'), True));
  Assert(not CharSetMatchCharB([], AnsiChar('A')));

  { Type matching                                                              }
  {$IFDEF SupportAnsiChar}
  Assert(StrIsNumericA('1234567890'), 'StrIsNumeric');
  Assert(not StrIsNumericA('1234567890X'), 'StrIsNumeric');
  Assert(not StrIsNumericA(''), 'StrIsNumeric');
  Assert(StrIsIntegerA('-1234567890'), 'StrIsInteger');
  Assert(StrIsIntegerA('0'), 'StrIsInteger');
  Assert(not StrIsIntegerA('-1234567890X'), 'StrIsInteger');
  Assert(not StrIsIntegerA('-'), 'StrIsInteger');

  Assert(StrIsNumericB('1234567890'), 'StrIsNumeric');
  Assert(not StrIsNumericB('1234567890X'), 'StrIsNumeric');
  Assert(not StrIsNumericB(''), 'StrIsNumeric');
  Assert(StrIsIntegerB('-1234567890'), 'StrIsInteger');
  Assert(StrIsIntegerB('0'), 'StrIsInteger');
  Assert(not StrIsIntegerB('-1234567890X'), 'StrIsInteger');
  Assert(not StrIsIntegerB('-'), 'StrIsInteger');
  {$ENDIF}

  Assert(StrIsNumericU('1234567890'), 'StrIsNumeric');
  Assert(not StrIsNumericU('1234567890X'), 'StrIsNumeric');
  Assert(not StrIsNumericU(''), 'StrIsNumeric');
  Assert(StrIsIntegerU('-1234567890'), 'StrIsInteger');
  Assert(StrIsIntegerU('0'), 'StrIsInteger');
  Assert(not StrIsIntegerU('-1234567890X'), 'StrIsInteger');
  Assert(not StrIsIntegerU('-'), 'StrIsInteger');

  Assert(StrIsNumeric('1234567890'), 'StrIsNumeric');
  Assert(not StrIsNumeric('1234567890X'), 'StrIsNumeric');
  Assert(not StrIsNumeric(''), 'StrIsNumeric');
  Assert(StrIsInteger('-1234567890'), 'StrIsInteger');
  Assert(StrIsInteger('0'), 'StrIsInteger');
  Assert(not StrIsInteger('-1234567890X'), 'StrIsInteger');
  Assert(not StrIsInteger('-'), 'StrIsInteger');

  { CopyRange                                                                  }
  {$IFDEF SupportAnsiChar}
  Assert(CopyRangeA('', 1, 2) =  '', 'CopyRange');
  Assert(CopyRangeA('', -1, -2) = '', 'CopyRange');
  Assert(CopyRangeA('1234567890', 5, 7) = '567', 'CopyRange');
  Assert(CopyRangeA('1234567890', 1, 1) = '1', 'CopyRange');
  Assert(CopyRangeA('1234567890', 0, 11) = '1234567890', 'CopyRange');
  Assert(CopyRangeA('1234567890', 7, 4) = '', 'CopyRange');
  Assert(CopyRangeA('1234567890', 1, 0) = '', 'CopyRange');
  Assert(CopyRangeA('1234567890', -2, 3) = '123', 'CopyRange');
  Assert(CopyRangeA('1234567890', 2, -1) = '', 'CopyRange');
  Assert(CopyRangeA('1234567890', -4, -2) = '', 'CopyRange');

  Assert(CopyRangeB('', 1, 2) =  '', 'CopyRange');
  Assert(CopyRangeB('', -1, -2) = '', 'CopyRange');
  Assert(CopyRangeB('1234567890', 5, 7) = '567', 'CopyRange');
  Assert(CopyRangeB('1234567890', 1, 1) = '1', 'CopyRange');
  Assert(CopyRangeB('1234567890', 0, 11) = '1234567890', 'CopyRange');
  Assert(CopyRangeB('1234567890', 7, 4) = '', 'CopyRange');
  Assert(CopyRangeB('1234567890', 1, 0) = '', 'CopyRange');
  Assert(CopyRangeB('1234567890', -2, 3) = '123', 'CopyRange');
  Assert(CopyRangeB('1234567890', 2, -1) = '', 'CopyRange');
  Assert(CopyRangeB('1234567890', -4, -2) = '', 'CopyRange');
  {$ENDIF}

  Assert(CopyRangeU('', 1, 2) =  '', 'CopyRange');
  Assert(CopyRangeU('', -1, -2) = '', 'CopyRange');
  Assert(CopyRangeU('1234567890', 5, 7) = '567', 'CopyRange');
  Assert(CopyRangeU('1234567890', 1, 1) = '1', 'CopyRange');
  Assert(CopyRangeU('1234567890', 0, 11) = '1234567890', 'CopyRange');
  Assert(CopyRangeU('1234567890', 7, 4) = '', 'CopyRange');
  Assert(CopyRangeU('1234567890', 1, 0) = '', 'CopyRange');
  Assert(CopyRangeU('1234567890', -2, 3) = '123', 'CopyRange');
  Assert(CopyRangeU('1234567890', 2, -1) = '', 'CopyRange');
  Assert(CopyRangeU('1234567890', -4, -2) = '', 'CopyRange');

  Assert(CopyRange('', 1, 2) =  '', 'CopyRange');
  Assert(CopyRange('', -1, -2) = '', 'CopyRange');
  Assert(CopyRange('1234567890', 5, 7) = '567', 'CopyRange');
  Assert(CopyRange('1234567890', 1, 1) = '1', 'CopyRange');
  Assert(CopyRange('1234567890', 0, 11) = '1234567890', 'CopyRange');
  Assert(CopyRange('1234567890', 7, 4) = '', 'CopyRange');
  Assert(CopyRange('1234567890', 1, 0) = '', 'CopyRange');
  Assert(CopyRange('1234567890', -2, 3) = '123', 'CopyRange');
  Assert(CopyRange('1234567890', 2, -1) = '', 'CopyRange');
  Assert(CopyRange('1234567890', -4, -2) = '', 'CopyRange');

  { CopyFrom                                                                   }
  {$IFDEF SupportAnsiChar}
  Assert(CopyFromA('a', 0) = 'a', 'CopyFrom');
  Assert(CopyFromA('a', -1) = 'a', 'CopyFrom');
  Assert(CopyFromA('', 1) = '', 'CopyFrom');
  Assert(CopyFromA('', -2) = '', 'CopyFrom');
  Assert(CopyFromA('1234567890', 8) = '890', 'CopyFrom');
  Assert(CopyFromA('1234567890', 11) = '', 'CopyFrom');
  Assert(CopyFromA('1234567890', 0) = '1234567890', 'CopyFrom');
  Assert(CopyFromA('1234567890', -2) = '1234567890', 'CopyFrom');
  {$ENDIF}

  Assert(CopyFromU('a', 0) = 'a', 'CopyFrom');
  Assert(CopyFromU('a', -1) = 'a', 'CopyFrom');
  Assert(CopyFromU('', 1) = '', 'CopyFrom');
  Assert(CopyFromU('', -2) = '', 'CopyFrom');
  Assert(CopyFromU('1234567890', 8) = '890', 'CopyFrom');
  Assert(CopyFromU('1234567890', 11) = '', 'CopyFrom');
  Assert(CopyFromU('1234567890', 0) = '1234567890', 'CopyFrom');
  Assert(CopyFromU('1234567890', -2) = '1234567890', 'CopyFrom');

  Assert(CopyFrom('a', 0) = 'a', 'CopyFrom');
  Assert(CopyFrom('a', -1) = 'a', 'CopyFrom');
  Assert(CopyFrom('', 1) = '', 'CopyFrom');
  Assert(CopyFrom('', -2) = '', 'CopyFrom');
  Assert(CopyFrom('1234567890', 8) = '890', 'CopyFrom');
  Assert(CopyFrom('1234567890', 11) = '', 'CopyFrom');
  Assert(CopyFrom('1234567890', 0) = '1234567890', 'CopyFrom');
  Assert(CopyFrom('1234567890', -2) = '1234567890', 'CopyFrom');

  { CopyLeft                                                                   }
  {$IFDEF SupportAnsiChar}
  Assert(CopyLeftA('a', 0) = '', 'CopyLeft');
  Assert(CopyLeftA('a', -1) = '', 'CopyLeft');
  Assert(CopyLeftA('', 1) = '', 'CopyLeft');
  Assert(CopyLeftA('b', 1) = 'b', 'CopyLeft');
  Assert(CopyLeftA('', -1) = '', 'CopyLeft');
  Assert(CopyLeftA('1234567890', 3) = '123', 'CopyLeft');
  Assert(CopyLeftA('1234567890', 11) = '1234567890', 'CopyLeft');
  Assert(CopyLeftA('1234567890', 0) = '', 'CopyLeft');
  Assert(CopyLeftA('1234567890', -2) = '', 'CopyLeft');

  Assert(CopyLeftB('a', 0) = '', 'CopyLeft');
  Assert(CopyLeftB('a', -1) = '', 'CopyLeft');
  Assert(CopyLeftB('', 1) = '', 'CopyLeft');
  Assert(CopyLeftB('b', 1) = 'b', 'CopyLeft');
  Assert(CopyLeftB('', -1) = '', 'CopyLeft');
  Assert(CopyLeftB('1234567890', 3) = '123', 'CopyLeft');
  Assert(CopyLeftB('1234567890', 11) = '1234567890', 'CopyLeft');
  Assert(CopyLeftB('1234567890', 0) = '', 'CopyLeft');
  Assert(CopyLeftB('1234567890', -2) = '', 'CopyLeft');
  {$ENDIF}

  Assert(CopyLeftU('a', 0) = '', 'CopyLeft');
  Assert(CopyLeftU('a', -1) = '', 'CopyLeft');
  Assert(CopyLeftU('', 1) = '', 'CopyLeft');
  Assert(CopyLeftU('b', 1) = 'b', 'CopyLeft');
  Assert(CopyLeftU('', -1) = '', 'CopyLeft');
  Assert(CopyLeftU('1234567890', 3) = '123', 'CopyLeft');
  Assert(CopyLeftU('1234567890', 11) = '1234567890', 'CopyLeft');
  Assert(CopyLeftU('1234567890', 0) = '', 'CopyLeft');
  Assert(CopyLeftU('1234567890', -2) = '', 'CopyLeft');

  Assert(CopyLeft('a', 0) = '', 'CopyLeft');
  Assert(CopyLeft('a', -1) = '', 'CopyLeft');
  Assert(CopyLeft('', 1) = '', 'CopyLeft');
  Assert(CopyLeft('b', 1) = 'b', 'CopyLeft');
  Assert(CopyLeft('', -1) = '', 'CopyLeft');
  Assert(CopyLeft('1234567890', 3) = '123', 'CopyLeft');
  Assert(CopyLeft('1234567890', 11) = '1234567890', 'CopyLeft');
  Assert(CopyLeft('1234567890', 0) = '', 'CopyLeft');
  Assert(CopyLeft('1234567890', -2) = '', 'CopyLeft');

  { CopyRight                                                                  }
  {$IFDEF SupportAnsiChar}
  Assert(CopyRightA('a', 0) = '', 'CopyRight');
  Assert(CopyRightA('a', -1) = '', 'CopyRight');
  Assert(CopyRightA('', 1) = '', 'CopyRight');
  Assert(CopyRightA('', -2) = '', 'CopyRight');
  Assert(CopyRightA('1234567890', 3) = '890', 'CopyRight');
  Assert(CopyRightA('1234567890', 11) = '1234567890', 'CopyRight');
  Assert(CopyRightA('1234567890', 0) = '', 'CopyRight');
  Assert(CopyRightA('1234567890', -2) = '', 'CopyRight');

  Assert(CopyRightB('a', 0) = '', 'CopyRight');
  Assert(CopyRightB('a', -1) = '', 'CopyRight');
  Assert(CopyRightB('', 1) = '', 'CopyRight');
  Assert(CopyRightB('', -2) = '', 'CopyRight');
  Assert(CopyRightB('1234567890', 3) = '890', 'CopyRight');
  Assert(CopyRightB('1234567890', 11) = '1234567890', 'CopyRight');
  Assert(CopyRightB('1234567890', 0) = '', 'CopyRight');
  Assert(CopyRightB('1234567890', -2) = '', 'CopyRight');
  {$ENDIF}

  Assert(CopyRightU('a', 0) = '', 'CopyRight');
  Assert(CopyRightU('a', -1) = '', 'CopyRight');
  Assert(CopyRightU('', 1) = '', 'CopyRight');
  Assert(CopyRightU('', -2) = '', 'CopyRight');
  Assert(CopyRightU('1234567890', 3) = '890', 'CopyRight');
  Assert(CopyRightU('1234567890', 11) = '1234567890', 'CopyRight');
  Assert(CopyRightU('1234567890', 0) = '', 'CopyRight');
  Assert(CopyRightU('1234567890', -2) = '', 'CopyRight');

  Assert(CopyRight('a', 0) = '', 'CopyRight');
  Assert(CopyRight('a', -1) = '', 'CopyRight');
  Assert(CopyRight('', 1) = '', 'CopyRight');
  Assert(CopyRight('', -2) = '', 'CopyRight');
  Assert(CopyRight('1234567890', 3) = '890', 'CopyRight');
  Assert(CopyRight('1234567890', 11) = '1234567890', 'CopyRight');
  Assert(CopyRight('1234567890', 0) = '', 'CopyRight');
  Assert(CopyRight('1234567890', -2) = '', 'CopyRight');

  { CopyEx                                                               }
  {$IFDEF SupportAnsiChar}
  Assert(CopyExA('', 1, 1) = '');
  Assert(CopyExA('', -2, -1) = '');
  Assert(CopyExA('12345', -2, 2) = '45');
  Assert(CopyExA('12345', -1, 2) = '5');
  Assert(CopyExA('12345', -7, 2) = '12');
  Assert(CopyExA('12345', -5, 2) = '12');
  Assert(CopyExA('12345', 2, -2) = '');
  Assert(CopyExA('12345', -4, 0) = '');
  Assert(CopyExA('12345', -4, 7) = '2345');
  Assert(CopyExA('12345', 2, 2) = '23');
  Assert(CopyExA('12345', -7, -6) = '');
  Assert(CopyExA('12345', 0, 2) = '12');
  Assert(CopyExA('12345', 0, 7) = '12345');
  {$ENDIF}

  Assert(CopyExW('', 1, 1) = '');
  Assert(CopyExW('', -2, -1) = '');
  Assert(CopyExW('12345', -2, 2) = '45');
  Assert(CopyExW('12345', -1, 2) = '5');
  Assert(CopyExW('12345', -7, 2) = '12');
  Assert(CopyExW('12345', -5, 2) = '12');
  Assert(CopyExW('12345', 2, -2) = '');
  Assert(CopyExW('12345', -4, 0) = '');
  Assert(CopyExW('12345', -4, 7) = '2345');
  Assert(CopyExW('12345', 2, 2) = '23');
  Assert(CopyExW('12345', -7, -6) = '');
  Assert(CopyExW('12345', 0, 2) = '12');
  Assert(CopyExW('12345', 0, 7) = '12345');

  Assert(CopyEx('', 1, 1) = '');
  Assert(CopyEx('', -2, -1) = '');
  Assert(CopyEx('12345', -2, 2) = '45');
  Assert(CopyEx('12345', -1, 2) = '5');
  Assert(CopyEx('12345', -7, 2) = '12');
  Assert(CopyEx('12345', -5, 2) = '12');
  Assert(CopyEx('12345', 2, -2) = '');
  Assert(CopyEx('12345', -4, 0) = '');
  Assert(CopyEx('12345', -4, 7) = '2345');
  Assert(CopyEx('12345', 2, 2) = '23');
  Assert(CopyEx('12345', -7, -6) = '');
  Assert(CopyEx('12345', 0, 2) = '12');
  Assert(CopyEx('12345', 0, 7) = '12345');

  { CopyRangeEx                                                          }
  Assert(CopyRangeEx('', -2, -1) = '');
  Assert(CopyRangeEx('', 0, 0) = '');
  Assert(CopyRangeEx('12345', -2, -1) = '45');
  Assert(CopyRangeEx('12345', -2, -1) = '45');
  Assert(CopyRangeEx('12345', -2, 5) = '45');
  Assert(CopyRangeEx('12345', 2, -2) = '234');
  Assert(CopyRangeEx('12345', 0, -2) = '1234');
  Assert(CopyRangeEx('12345', 1, -7) = '');
  Assert(CopyRangeEx('12345', 7, -1) = '');
  Assert(CopyRangeEx('12345', -10, 2) = '12');
  Assert(CopyRangeEx('12345', -10, -7) = '');
  Assert(CopyRangeEx('12345', 2, -6) = '');
  Assert(CopyRangeEx('12345', 0, -2) = '1234');
  Assert(CopyRangeEx('12345', 2, 0) = '');
  Assert(CopyRangeEx('', -1, 2) = '');

  { CopyFromEx                                                           }
  Assert(CopyFromEx('', 0) = '');
  Assert(CopyFromEx('', -1) = '');
  Assert(CopyFromEx('12345', 0) = '12345');
  Assert(CopyFromEx('12345', 1) = '12345');
  Assert(CopyFromEx('12345', -5) = '12345');
  Assert(CopyFromEx('12345', -6) = '12345');
  Assert(CopyFromEx('12345', 2) = '2345');
  Assert(CopyFromEx('12345', -4) =  '2345');
  Assert(CopyFromEx('12345', 6) = '');

  { StrEqualNoCase                                                             }
  {$IFDEF SupportAnsiChar}
  Assert(StrEqualNoAsciiCaseA('A', 'a'), 'StrEqualNoCase');
  Assert(not StrEqualNoAsciiCaseA('A', 'B'), 'StrEqualNoCase');
  Assert(StrEqualNoAsciiCaseA('@ABCDEFGHIJKLMNOPQRSTUVWXYZ` ', '@abcdefghijklmnopqrstuvwxyz` '), 'StrEqualNoCase');
  Assert(not StrEqualNoAsciiCaseA('@ABCDEFGHIJKLMNOPQRSTUVWXY-` ', '@abcdefghijklmnopqrstuvwxyz` '), 'StrEqualNoCase');

  Assert(StrEqualNoAsciiCaseB('A', 'a'), 'StrEqualNoCase');
  Assert(not StrEqualNoAsciiCaseB('A', 'B'), 'StrEqualNoCase');
  Assert(StrEqualNoAsciiCaseB('@ABCDEFGHIJKLMNOPQRSTUVWXYZ` ', '@abcdefghijklmnopqrstuvwxyz` '), 'StrEqualNoCase');
  Assert(not StrEqualNoAsciiCaseB('@ABCDEFGHIJKLMNOPQRSTUVWXY-` ', '@abcdefghijklmnopqrstuvwxyz` '), 'StrEqualNoCase');
  {$ENDIF}

  Assert(StrEqualNoAsciiCaseU('A', 'a'), 'StrEqualNoCase');
  Assert(not StrEqualNoAsciiCaseU('A', 'B'), 'StrEqualNoCase');
  Assert(StrEqualNoAsciiCaseU('@ABCDEFGHIJKLMNOPQRSTUVWXYZ` ', '@abcdefghijklmnopqrstuvwxyz` '), 'StrEqualNoCase');
  Assert(not StrEqualNoAsciiCaseU('@ABCDEFGHIJKLMNOPQRSTUVWXY-` ', '@abcdefghijklmnopqrstuvwxyz` '), 'StrEqualNoCase');

  Assert(StrEqualNoAsciiCase('A', 'a'), 'StrEqualNoCase');
  Assert(not StrEqualNoAsciiCase('A', 'B'), 'StrEqualNoCase');
  Assert(StrEqualNoAsciiCase('@ABCDEFGHIJKLMNOPQRSTUVWXYZ` ', '@abcdefghijklmnopqrstuvwxyz` '), 'StrEqualNoCase');
  Assert(not StrEqualNoAsciiCase('@ABCDEFGHIJKLMNOPQRSTUVWXY-` ', '@abcdefghijklmnopqrstuvwxyz` '), 'StrEqualNoCase');

  { StrReverse                                                                 }
  {$IFDEF SupportAnsiChar}
  Assert(StrReverseA('12345') = '54321', 'StrReverse');
  Assert(StrReverseA('1234') = '4321', 'StrReverse');

  Assert(StrReverseB('12345') = '54321', 'StrReverse');
  Assert(StrReverseB('1234') = '4321', 'StrReverse');
  {$ENDIF}

  Assert(StrReverseU('12345') = '54321', 'StrReverse');
  Assert(StrReverseU('1234') = '4321', 'StrReverse');

  Assert(StrReverse('12345') = '54321', 'StrReverse');
  Assert(StrReverse('1234') = '4321', 'StrReverse');

  { Compare                                                                    }
  {$IFDEF SupportAnsiChar}
  Assert(StrCompareA('A', 'a') = -1, 'StrCompareNoCase');
  Assert(StrCompareA('a', 'A') = 1, 'StrCompareNoCase');
  Assert(StrCompareA('a', 'aa') = -1, 'StrCompareNoCase');
  Assert(StrCompareA('', '') = 0, 'StrCompareNoCase');
  Assert(StrCompareA('', 'a') = -1, 'StrCompareNoCase');
  Assert(StrCompareA('a', '') = 1, 'StrCompareNoCase');

  Assert(StrCompareB('A', 'a') = -1, 'StrCompareNoCase');
  Assert(StrCompareB('a', 'A') = 1, 'StrCompareNoCase');
  Assert(StrCompareB('a', 'aa') = -1, 'StrCompareNoCase');
  Assert(StrCompareB('', '') = 0, 'StrCompareNoCase');
  Assert(StrCompareB('', 'a') = -1, 'StrCompareNoCase');
  Assert(StrCompareB('a', '') = 1, 'StrCompareNoCase');
  {$ENDIF}

  Assert(StrCompareU('A', 'a') = -1, 'StrCompareNoCase');
  Assert(StrCompareU('a', 'A') = 1, 'StrCompareNoCase');
  Assert(StrCompareU('a', 'aa') = -1, 'StrCompareNoCase');
  Assert(StrCompareU('', '') = 0, 'StrCompareNoCase');
  Assert(StrCompareU('', 'a') = -1, 'StrCompareNoCase');
  Assert(StrCompareU('a', '') = 1, 'StrCompareNoCase');

  Assert(StrCompare('A', 'a') = -1, 'StrCompareNoCase');
  Assert(StrCompare('a', 'A') = 1, 'StrCompareNoCase');
  Assert(StrCompare('a', 'aa') = -1, 'StrCompareNoCase');
  Assert(StrCompare('', '') = 0, 'StrCompareNoCase');
  Assert(StrCompare('', 'a') = -1, 'StrCompareNoCase');
  Assert(StrCompare('a', '') = 1, 'StrCompareNoCase');

  { Match                                                                      }
  {$IFDEF SupportAnsiChar}
  Assert(not StrMatchA('', '', 1), 'StrMatch');
  Assert(not StrMatchA('', 'a', 1), 'StrMatch');
  Assert(not StrMatchA('a', '', 1), 'StrMatch');
  Assert(not StrMatchA('a', 'A', 1), 'StrMatch');
  Assert(StrMatchA('A', 'A', 1), 'StrMatch');
  Assert(not StrMatchA('abcdef', 'xx', 1), 'StrMatch');
  Assert(StrMatchA('xbcdef', 'x', 1), 'StrMatch');
  Assert(StrMatchA('abcdxxxxx', 'xxxxx', 5), 'StrMatch');
  Assert(StrMatchA('abcdef', 'abcdef', 1), 'StrMatch');
  Assert(not StrMatchNoAsciiCaseA('abcdef', 'xx', 1), 'StrMatchNoCase');
  Assert(StrMatchNoAsciiCaseA('xbCDef', 'xBCd', 1), 'StrMatchNoCase');
  Assert(StrMatchNoAsciiCaseA('abcdxxX-xx', 'Xxx-xX', 5), 'StrMatchNoCase');
  Assert(StrMatchA('abcde', 'abcd', 1), 'StrMatch');
  Assert(StrMatchA('abcde', 'abc', 1), 'StrMatch');
  Assert(StrMatchA('abcde', 'ab', 1), 'StrMatch');
  Assert(StrMatchA('abcde', 'a', 1), 'StrMatch');
  Assert(StrMatchNoAsciiCaseA(' abC-Def{', ' AbC-def{', 1), 'StrMatchNoCase');
  Assert(StrMatchLeftA('ABC1D', 'aBc1', False), 'StrMatchLeft');
  Assert(StrMatchLeftA('aBc1D', 'aBc1', True), 'StrMatchLeft');
  Assert(not StrMatchLeftA('AB1D', 'ABc1', False), 'StrMatchLeft');
  Assert(not StrMatchLeftA('aBC1D', 'aBc1', True), 'StrMatchLeft');
  Assert(not StrMatchCharA('', ['a', 'b', 'c']), 'StrMatchChar');
  Assert(StrMatchCharA('a', ['a', 'b', 'c']), 'StrMatchChar');
  Assert(not StrMatchCharA('d', ['a', 'b', 'c']), 'StrMatchChar');
  Assert(StrMatchCharA('acbba', ['a', 'b', 'c']), 'StrMatchChar');
  Assert(not StrMatchCharA('acbd', ['a', 'b', 'c']), 'StrMatchChar');
  Assert(StrMatchLenA('abcd', ['a', 'b', 'c'], 1) = 3, 'StrMatchLen');
  Assert(StrMatchLenA('abcd', ['a', 'b', 'c'], 3) = 1, 'StrMatchLen');
  Assert(StrMatchLenA('abcd', ['a', 'b', 'c'], 4) = 0, 'StrMatchLen');
  Assert(StrMatchLenA('', ['a', 'b', 'c'], 1) = 0, 'StrMatchLen');
  Assert(StrMatchLenA('dcba', ['a', 'b', 'c'], 2) = 3, 'StrMatchLen');
  Assert(StrMatchLenA('dcba', ['a', 'b', 'c'], 1) = 0, 'StrMatchLen');

  Assert(not StrMatchB('', '', 1), 'StrMatch');
  Assert(not StrMatchB('', 'a', 1), 'StrMatch');
  Assert(not StrMatchB('a', '', 1), 'StrMatch');
  Assert(not StrMatchB('a', 'A', 1), 'StrMatch');
  Assert(StrMatchB('A', 'A', 1), 'StrMatch');
  Assert(not StrMatchB('abcdef', 'xx', 1), 'StrMatch');
  Assert(StrMatchB('xbcdef', 'x', 1), 'StrMatch');
  Assert(StrMatchB('abcdxxxxx', 'xxxxx', 5), 'StrMatch');
  Assert(StrMatchB('abcdef', 'abcdef', 1), 'StrMatch');
  Assert(not StrMatchNoAsciiCaseB('abcdef', 'xx', 1), 'StrMatchNoCase');
  Assert(StrMatchNoAsciiCaseB('xbCDef', 'xBCd', 1), 'StrMatchNoCase');
  Assert(StrMatchNoAsciiCaseB('abcdxxX-xx', 'Xxx-xX', 5), 'StrMatchNoCase');
  Assert(StrMatchB('abcde', 'abcd', 1), 'StrMatch');
  Assert(StrMatchB('abcde', 'abc', 1), 'StrMatch');
  Assert(StrMatchB('abcde', 'ab', 1), 'StrMatch');
  Assert(StrMatchB('abcde', 'a', 1), 'StrMatch');
  Assert(StrMatchNoAsciiCaseB(' abC-Def{', ' AbC-def{', 1), 'StrMatchNoCase');
  Assert(StrMatchLeftB('ABC1D', 'aBc1', False), 'StrMatchLeft');
  Assert(StrMatchLeftB('aBc1D', 'aBc1', True), 'StrMatchLeft');
  Assert(not StrMatchLeftB('AB1D', 'ABc1', False), 'StrMatchLeft');
  Assert(not StrMatchLeftB('aBC1D', 'aBc1', True), 'StrMatchLeft');
  Assert(not StrMatchCharB('', ['a', 'b', 'c']), 'StrMatchChar');
  Assert(StrMatchCharB('a', ['a', 'b', 'c']), 'StrMatchChar');
  Assert(not StrMatchCharB('d', ['a', 'b', 'c']), 'StrMatchChar');
  Assert(StrMatchCharB('acbba', ['a', 'b', 'c']), 'StrMatchChar');
  Assert(not StrMatchCharB('acbd', ['a', 'b', 'c']), 'StrMatchChar');
  Assert(StrMatchLenB('abcd', ['a', 'b', 'c'], 1) = 3, 'StrMatchLen');
  Assert(StrMatchLenB('abcd', ['a', 'b', 'c'], 3) = 1, 'StrMatchLen');
  Assert(StrMatchLenB('abcd', ['a', 'b', 'c'], 4) = 0, 'StrMatchLen');
  Assert(StrMatchLenB('', ['a', 'b', 'c'], 1) = 0, 'StrMatchLen');
  Assert(StrMatchLenB('dcba', ['a', 'b', 'c'], 2) = 3, 'StrMatchLen');
  Assert(StrMatchLenB('dcba', ['a', 'b', 'c'], 1) = 0, 'StrMatchLen');
  {$ENDIF}

  Assert(not StrMatchU('', '', 1), 'StrMatch');
  Assert(not StrMatchU('', 'a', 1), 'StrMatch');
  Assert(not StrMatchU('a', '', 1), 'StrMatch');
  Assert(not StrMatchU('a', 'A', 1), 'StrMatch');
  Assert(StrMatchU('A', 'A', 1), 'StrMatch');
  Assert(not StrMatchU('abcdef', 'xx', 1), 'StrMatch');
  Assert(StrMatchU('xbcdef', 'x', 1), 'StrMatch');
  Assert(StrMatchU('abcdxxxxx', 'xxxxx', 5), 'StrMatch');
  Assert(StrMatchU('abcdef', 'abcdef', 1), 'StrMatch');
  Assert(not StrMatchNoAsciiCaseU('abcdef', 'xx', 1), 'StrMatchNoCase');
  Assert(StrMatchNoAsciiCaseU('xbCDef', 'xBCd', 1), 'StrMatchNoCase');
  Assert(StrMatchNoAsciiCaseU('abcdxxX-xx', 'Xxx-xX', 5), 'StrMatchNoCase');
  Assert(StrMatchU('abcde', 'abcd', 1), 'StrMatch');
  Assert(StrMatchU('abcde', 'abc', 1), 'StrMatch');
  Assert(StrMatchU('abcde', 'ab', 1), 'StrMatch');
  Assert(StrMatchU('abcde', 'a', 1), 'StrMatch');
  Assert(StrMatchNoAsciiCaseU(' abC-Def{', ' AbC-def{', 1), 'StrMatchNoCase');
  Assert(StrMatchLeftU('ABC1D', 'aBc1', False), 'StrMatchLeft');
  Assert(StrMatchLeftU('aBc1D', 'aBc1', True), 'StrMatchLeft');
  Assert(not StrMatchLeftU('AB1D', 'ABc1', False), 'StrMatchLeft');
  Assert(not StrMatchLeftU('aBC1D', 'aBc1', True), 'StrMatchLeft');
  {$IFDEF SupportAnsiChar}
  Assert(not StrMatchCharU('', ['a', 'b', 'c']), 'StrMatchChar');
  Assert(StrMatchCharU('a', ['a', 'b', 'c']), 'StrMatchChar');
  Assert(not StrMatchCharU('d', ['a', 'b', 'c']), 'StrMatchChar');
  Assert(StrMatchCharU('acbba', ['a', 'b', 'c']), 'StrMatchChar');
  Assert(not StrMatchCharU('acbd', ['a', 'b', 'c']), 'StrMatchChar');
  Assert(StrMatchLenU('abcd', ['a', 'b', 'c'], 1) = 3, 'StrMatchLen');
  Assert(StrMatchLenU('abcd', ['a', 'b', 'c'], 3) = 1, 'StrMatchLen');
  Assert(StrMatchLenU('abcd', ['a', 'b', 'c'], 4) = 0, 'StrMatchLen');
  Assert(StrMatchLenU('', ['a', 'b', 'c'], 1) = 0, 'StrMatchLen');
  Assert(StrMatchLenU('dcba', ['a', 'b', 'c'], 2) = 3, 'StrMatchLen');
  Assert(StrMatchLenU('dcba', ['a', 'b', 'c'], 1) = 0, 'StrMatchLen');
  {$ENDIF}

  Assert(not StrMatch('', '', 1), 'StrMatch');
  Assert(not StrMatch('', 'a', 1), 'StrMatch');
  Assert(not StrMatch('a', '', 1), 'StrMatch');
  Assert(not StrMatch('a', 'A', 1), 'StrMatch');
  Assert(StrMatch('A', 'A', 1), 'StrMatch');
  Assert(not StrMatch('abcdef', 'xx', 1), 'StrMatch');
  Assert(StrMatch('xbcdef', 'x', 1), 'StrMatch');
  Assert(StrMatch('abcdxxxxx', 'xxxxx', 5), 'StrMatch');
  Assert(StrMatch('abcdef', 'abcdef', 1), 'StrMatch');
  Assert(not StrMatchNoAsciiCase('abcdef', 'xx', 1), 'StrMatchNoCase');
  Assert(StrMatchNoAsciiCase('xbCDef', 'xBCd', 1), 'StrMatchNoCase');
  Assert(StrMatchNoAsciiCase('abcdxxX-xx', 'Xxx-xX', 5), 'StrMatchNoCase');
  Assert(StrMatch('abcde', 'abcd', 1), 'StrMatch');
  Assert(StrMatch('abcde', 'abc', 1), 'StrMatch');
  Assert(StrMatch('abcde', 'ab', 1), 'StrMatch');
  Assert(StrMatch('abcde', 'a', 1), 'StrMatch');
  Assert(StrMatchNoAsciiCase(' abC-Def{', ' AbC-def{', 1), 'StrMatchNoCase');
  Assert(StrMatchLeft('ABC1D', 'aBc1', False), 'StrMatchLeft');
  Assert(StrMatchLeft('aBc1D', 'aBc1', True), 'StrMatchLeft');
  Assert(not StrMatchLeft('AB1D', 'ABc1', False), 'StrMatchLeft');
  Assert(not StrMatchLeft('aBC1D', 'aBc1', True), 'StrMatchLeft');
  {$IFDEF SupportAnsiChar}
  Assert(not StrMatchChar('', ['a', 'b', 'c']), 'StrMatchChar');
  Assert(StrMatchChar('a', ['a', 'b', 'c']), 'StrMatchChar');
  Assert(not StrMatchChar('d', ['a', 'b', 'c']), 'StrMatchChar');
  Assert(StrMatchChar('acbba', ['a', 'b', 'c']), 'StrMatchChar');
  Assert(not StrMatchChar('acbd', ['a', 'b', 'c']), 'StrMatchChar');
  Assert(StrMatchLen('abcd', ['a', 'b', 'c'], 1) = 3, 'StrMatchLen');
  Assert(StrMatchLen('abcd', ['a', 'b', 'c'], 3) = 1, 'StrMatchLen');
  Assert(StrMatchLen('abcd', ['a', 'b', 'c'], 4) = 0, 'StrMatchLen');
  Assert(StrMatchLen('', ['a', 'b', 'c'], 1) = 0, 'StrMatchLen');
  Assert(StrMatchLen('dcba', ['a', 'b', 'c'], 2) = 3, 'StrMatchLen');
  Assert(StrMatchLen('dcba', ['a', 'b', 'c'], 1) = 0, 'StrMatchLen');
  {$ENDIF}

  { Pos                                                                        }
  {$IFDEF SupportAnsiString}
  Assert(PosStrA('', 'ABCABC') = 0, 'PosStr');
  Assert(PosStrA('', 'a') = 0, 'PosStr');
  Assert(PosStrA('A', '') = 0, 'PosStr');
  Assert(PosStrA('A', 'ABCABC') = 1, 'PosStr');
  Assert(PosStrA('A', 'ABCABC', 2) = 4, 'PosStr');
  Assert(PosStrA('ab', 'a') = 0, 'PosStr');
  Assert(PosStrA('ab', 'ab') = 1, 'PosStr');
  Assert(PosStrA('ab', 'zxab') = 3, 'PosStr');
  Assert(PosStrA('ab', '') = 0, 'PosStr');
  Assert(PosStrA('ab', 'axdba') = 0, 'PosStr');
  Assert(PosStrA('a', 'AbAc', 1, False) = 1, 'PosStr');
  Assert(PosStrA('ba', 'ABAcabac', 1, False) = 2, 'PosStr');
  Assert(PosStrA('a', 'abac', 2) = 3, 'PosStr');
  Assert(PosStrA('ab', 'abacabac', 2) = 5, 'PosStr');
  Assert(PosStrRevA('A', 'ABCABC') = 4, 'PosStrRev');
  Assert(PosStrRevA('A', 'ABCABCA') = 7, 'PosStrRev');
  Assert(PosStrRevA('CA', 'ABCABCA') = 6, 'PosStrRev');
  Assert(PosStrRevA('ab', 'abacabac') = 5, 'PosStrRev');
  Assert(PosNStrA('AB', 'ABCABCDAB', 3) = 8, 'PosNStr');
  Assert(PosCharSetA([], 'a') = 0, 'PosChar');
  Assert(PosCharSetA(['a'], 'a') = 1, 'PosChar');
  Assert(PosCharSetA(['a'], '') = 0, 'PosChar');
  Assert(PosCharSetA(['a'], 'aa') = 1, 'PosChar');
  Assert(PosCharSetA(['a'], 'ba') = 2, 'PosChar');
  Assert(PosCharSetA(['a'], 'zx') = 0, 'PosChar');
  Assert(PosCharA(AnsiChar('a'), 'a') = 1, 'PosChar');
  Assert(PosCharA(AnsiChar('a'), '') = 0, 'PosChar');
  Assert(PosCharA(AnsiChar('a'), 'aa') = 1, 'PosChar');
  Assert(PosCharA(AnsiChar('a'), 'ba') = 2, 'PosChar');
  Assert(PosCharA(AnsiChar('a'), 'zx') = 0, 'PosChar');
  Assert(PosCharSetA(['a'], 'abac', 2) = 3, 'PosChar');
  Assert(PosCharRevA(AnsiChar('a'), 'abac') = 3, 'PosCharRev');
  Assert(PosCharSetRevA(['a'..'z'], 'abac') = 4, 'PosCharRev');
  Assert(PosNotCharA(AnsiChar('a'), 'abac') = 2, 'PosNotChar');
  Assert(PosNotCharSetA(['a'..'z'], 'abac1a') = 5, 'PosNotChar');

  Assert(PosStrB('', 'ABCABC') = 0, 'PosStr');
  Assert(PosStrB('', 'a') = 0, 'PosStr');
  Assert(PosStrB('A', '') = 0, 'PosStr');
  Assert(PosStrB('A', 'ABCABC') = 1, 'PosStr');
  Assert(PosStrB('A', 'ABCABC', 2) = 4, 'PosStr');
  Assert(PosStrB('ab', 'a') = 0, 'PosStr');
  Assert(PosStrB('ab', 'ab') = 1, 'PosStr');
  Assert(PosStrB('ab', 'zxab') = 3, 'PosStr');
  Assert(PosStrB('ab', '') = 0, 'PosStr');
  Assert(PosStrB('ab', 'axdba') = 0, 'PosStr');
  Assert(PosStrB('a', 'AbAc', 1, False) = 1, 'PosStr');
  Assert(PosStrB('ba', 'ABAcabac', 1, False) = 2, 'PosStr');
  Assert(PosStrB('a', 'abac', 2) = 3, 'PosStr');
  Assert(PosStrB('ab', 'abacabac', 2) = 5, 'PosStr');
  Assert(PosStrRevB('A', 'ABCABC') = 4, 'PosStrRev');
  Assert(PosStrRevB('A', 'ABCABCA') = 7, 'PosStrRev');
  Assert(PosStrRevB('CA', 'ABCABCA') = 6, 'PosStrRev');
  Assert(PosStrRevB('ab', 'abacabac') = 5, 'PosStrRev');
  Assert(PosNStrB('AB', 'ABCABCDAB', 3) = 8, 'PosNStr');
  Assert(PosCharSetB([], 'a') = 0, 'PosChar');
  Assert(PosCharSetB(['a'], 'a') = 1, 'PosChar');
  Assert(PosCharSetB(['a'], '') = 0, 'PosChar');
  Assert(PosCharSetB(['a'], 'aa') = 1, 'PosChar');
  Assert(PosCharSetB(['a'], 'ba') = 2, 'PosChar');
  Assert(PosCharSetB(['a'], 'zx') = 0, 'PosChar');
  Assert(PosCharB(AnsiChar('a'), 'a') = 1, 'PosChar');
  Assert(PosCharB(AnsiChar('a'), '') = 0, 'PosChar');
  Assert(PosCharB(AnsiChar('a'), 'aa') = 1, 'PosChar');
  Assert(PosCharB(AnsiChar('a'), 'ba') = 2, 'PosChar');
  Assert(PosCharB(AnsiChar('a'), 'zx') = 0, 'PosChar');
  Assert(PosCharSetB(['a'], 'abac', 2) = 3, 'PosChar');
  Assert(PosCharRevB(AnsiChar('a'), 'abac') = 3, 'PosCharRev');
  Assert(PosCharSetRevB(['a'..'z'], 'abac') = 4, 'PosCharRev');
  Assert(PosNotCharB(AnsiChar('a'), 'abac') = 2, 'PosNotChar');
  Assert(PosNotCharSetB(['a'..'z'], 'abac1a') = 5, 'PosNotChar');
  {$ENDIF}

  Assert(PosStrU('', 'ABCABC') = 0, 'PosStr');
  Assert(PosStrU('', 'a') = 0, 'PosStr');
  Assert(PosStrU('A', '') = 0, 'PosStr');
  Assert(PosStrU('A', 'ABCABC') = 1, 'PosStr');
  Assert(PosStrU('A', 'ABCABC', 2) = 4, 'PosStr');
  Assert(PosStrU('ab', 'a') = 0, 'PosStr');
  Assert(PosStrU('ab', 'ab') = 1, 'PosStr');
  Assert(PosStrU('ab', 'zxab') = 3, 'PosStr');
  Assert(PosStrU('ab', '') = 0, 'PosStr');
  Assert(PosStrU('ab', 'axdba') = 0, 'PosStr');
  Assert(PosStrU('a', 'AbAc', 1, False) = 1, 'PosStr');
  Assert(PosStrU('ba', 'ABAcabac', 1, False) = 2, 'PosStr');
  Assert(PosStrU('a', 'abac', 2) = 3, 'PosStr');
  Assert(PosStrU('ab', 'abacabac', 2) = 5, 'PosStr');
  Assert(PosStrRevU('A', 'ABCABC') = 4, 'PosStrRev');
  Assert(PosStrRevU('A', 'ABCABCA') = 7, 'PosStrRev');
  Assert(PosStrRevU('CA', 'ABCABCA') = 6, 'PosStrRev');
  Assert(PosStrRevU('ab', 'abacabac') = 5, 'PosStrRev');
  Assert(PosNStrU('AB', 'ABCABCDAB', 3) = 8, 'PosNStr');
  Assert(PosCharSetU([], 'a') = 0, 'PosChar');
  {$IFDEF SupportAnsiChar}
  Assert(PosCharSetU(['a'], 'a') = 1, 'PosChar');
  Assert(PosCharSetU(['a'], '') = 0, 'PosChar');
  Assert(PosCharSetU(['a'], 'aa') = 1, 'PosChar');
  Assert(PosCharSetU(['a'], 'ba') = 2, 'PosChar');
  Assert(PosCharSetU(['a'], 'zx') = 0, 'PosChar');
  Assert(PosCharU(AnsiChar('a'), 'a') = 1, 'PosChar');
  Assert(PosCharU(AnsiChar('a'), '') = 0, 'PosChar');
  Assert(PosCharU(AnsiChar('a'), 'aa') = 1, 'PosChar');
  Assert(PosCharU(AnsiChar('a'), 'ba') = 2, 'PosChar');
  Assert(PosCharU(AnsiChar('a'), 'zx') = 0, 'PosChar');
  Assert(PosCharSetU(['a'], 'abac', 2) = 3, 'PosChar');
  Assert(PosCharRevU(AnsiChar('a'), 'abac') = 3, 'PosCharRev');
  Assert(PosCharSetRevU(['a'..'z'], 'abac') = 4, 'PosCharRev');
  Assert(PosNotCharU(AnsiChar('a'), 'abac') = 2, 'PosNotChar');
  Assert(PosNotCharSetU(['a'..'z'], 'abac1a') = 5, 'PosNotChar');
  {$ENDIF}

  Assert(PosStr('', 'ABCABC') = 0, 'PosStr');
  Assert(PosStr('', 'a') = 0, 'PosStr');
  Assert(PosStr('A', '') = 0, 'PosStr');
  Assert(PosStr('A', 'ABCABC') = 1, 'PosStr');
  Assert(PosStr('A', 'ABCABC', 2) = 4, 'PosStr');
  Assert(PosStr('ab', 'a') = 0, 'PosStr');
  Assert(PosStr('ab', 'ab') = 1, 'PosStr');
  Assert(PosStr('ab', 'zxab') = 3, 'PosStr');
  Assert(PosStr('ab', '') = 0, 'PosStr');
  Assert(PosStr('ab', 'axdba') = 0, 'PosStr');
  Assert(PosStr('a', 'AbAc', 1, False) = 1, 'PosStr');
  Assert(PosStr('ba', 'ABAcabac', 1, False) = 2, 'PosStr');
  Assert(PosStr('a', 'abac', 2) = 3, 'PosStr');
  Assert(PosStr('ab', 'abacabac', 2) = 5, 'PosStr');
  Assert(PosStrRev('A', 'ABCABC') = 4, 'PosStrRev');
  Assert(PosStrRev('A', 'ABCABCA') = 7, 'PosStrRev');
  Assert(PosStrRev('CA', 'ABCABCA') = 6, 'PosStrRev');
  Assert(PosStrRev('ab', 'abacabac') = 5, 'PosStrRev');
  Assert(PosNStr('AB', 'ABCABCDAB', 3) = 8, 'PosNStr');
  Assert(PosCharSet([], 'a') = 0, 'PosChar');
  {$IFDEF SupportAnsiChar}
  Assert(PosCharSet(['a'], 'a') = 1, 'PosChar');
  Assert(PosCharSet(['a'], '') = 0, 'PosChar');
  Assert(PosCharSet(['a'], 'aa') = 1, 'PosChar');
  Assert(PosCharSet(['a'], 'ba') = 2, 'PosChar');
  Assert(PosCharSet(['a'], 'zx') = 0, 'PosChar');
  Assert(PosChar(AnsiChar('a'), 'a') = 1, 'PosChar');
  Assert(PosChar(AnsiChar('a'), '') = 0, 'PosChar');
  Assert(PosChar(AnsiChar('a'), 'aa') = 1, 'PosChar');
  Assert(PosChar(AnsiChar('a'), 'ba') = 2, 'PosChar');
  Assert(PosChar(AnsiChar('a'), 'zx') = 0, 'PosChar');
  Assert(PosCharSet(['a'], 'abac', 2) = 3, 'PosChar');
  Assert(PosCharRev(AnsiChar('a'), 'abac') = 3, 'PosCharRev');
  Assert(PosCharSetRev(['a'..'z'], 'abac') = 4, 'PosCharRev');
  Assert(PosNotChar(AnsiChar('a'), 'abac') = 2, 'PosNotChar');
  Assert(PosNotCharSet(['a'..'z'], 'abac1a') = 5, 'PosNotChar');
  {$ENDIF}

  Assert(PosStrU('AB', 'XYZABCAACDEF', 1) = 4, 'PosStrU');
  Assert(PosStrU('AA', 'XYZABCAACDEF', 1) = 7, 'PosStrU');
  Assert(PosStrU('A', 'XYZABCAACDEF', 8) = 8, 'PosStrU');
  Assert(PosStrU('AA', 'XYZABCAACDEF', 8) = 0, 'PosStrU');
  Assert(PosStrU('AAQ', 'XYZABCAACDEF', 1) = 0, 'PosStrU');

  Assert(PosCharU(WideChar('A'), 'XYZABCAACDEF', 1) = 4, 'PosCharU');
  Assert(PosCharU(WideChar('A'), 'XYZABCAACDEF', 5) = 7, 'PosCharU');
  Assert(PosCharU(WideChar('A'), 'XYZABCAACDEF', 8) = 8, 'PosCharU');
  Assert(PosCharU(WideChar('A'), 'XYZABCAACDEF', 9) = 0, 'PosCharU');
  Assert(PosCharU(WideChar('Q'), 'XYZABCAACDEF', 1) = 0, 'PosCharU');

  { Trim                                                                       }
  {$IFDEF SupportAnsiString}
  Assert(StrTrimLeftA('   123   ') = '123   ', 'TrimLeft');
  Assert(StrTrimLeftStrNoCaseA('   123   ', '  ') = ' 123   ', 'TrimLeftStrNoCase');
  Assert(StrTrimRightA('   123   ') = '   123', 'TrimRight');
  Assert(StrTrimRightStrNoCaseA('   123   ', '  ') = '   123 ', 'TrimRightStrNoCase');
  Assert(StrTrimA('   123   ', [' ']) = '123', 'Trim');
  Assert(StrTrimA('', [' ']) = '', 'Trim');
  Assert(StrTrimA('X', [' ']) = 'X', 'Trim');

  Assert(StrTrimLeftB('   123   ') = '123   ', 'TrimLeft');
  Assert(StrTrimLeftStrNoCaseB('   123   ', '  ') = ' 123   ', 'TrimLeftStrNoCase');
  Assert(StrTrimRightB('   123   ') = '   123', 'TrimRight');
  Assert(StrTrimRightStrNoCaseB('   123   ', '  ') = '   123 ', 'TrimRightStrNoCase');
  Assert(StrTrimB('   123   ', [' ']) = '123', 'Trim');
  Assert(StrTrimB('', [' ']) = '', 'Trim');
  Assert(StrTrimB('X', [' ']) = 'X', 'Trim');
  {$ENDIF}

  Assert(StrTrimLeftU('   123   ') = '123   ', 'TrimLeft');
  Assert(StrTrimLeftStrNoCaseU('   123   ', '  ') = ' 123   ', 'TrimLeftStrNoCase');
  Assert(StrTrimRightU('   123   ') = '   123', 'TrimRight');
  Assert(StrTrimRightStrNoCaseU('   123   ', '  ') = '   123 ', 'TrimRightStrNoCase');
  Assert(StrTrimU('   123   ', [' ']) = '123', 'Trim');
  Assert(StrTrimU('', [' ']) = '', 'Trim');
  Assert(StrTrimU('X', [' ']) = 'X', 'Trim');

  Assert(StrTrimLeft('   123   ') = '123   ', 'TrimLeft');
  Assert(StrTrimLeftStrNoCase('   123   ', '  ') = ' 123   ', 'TrimLeftStrNoCase');
  Assert(StrTrimRight('   123   ') = '   123', 'TrimRight');
  Assert(StrTrimRightStrNoCase('   123   ', '  ') = '   123 ', 'TrimRightStrNoCase');
  Assert(StrTrim('   123   ', [' ']) = '123', 'Trim');
  Assert(StrTrim('', [' ']) = '', 'Trim');
  Assert(StrTrim('X', [' ']) = 'X', 'Trim');

  Assert(StrTrimLeftU(' X ') = 'X ', 'StrTrimLeft');
  Assert(StrTrimRightU(' X ') = ' X', 'StrTrimRight');
  Assert(StrTrimU(' X ') = 'X', 'StrTrim');

  { Dup                                                                        }
  {$IFDEF SupportAnsiString}
  Assert(DupStrA('xy', 3) = 'xyxyxy', 'Dup');
  Assert(DupStrA('', 3) = '', 'Dup');
  Assert(DupStrA('a', 0) = '', 'Dup');
  Assert(DupStrA('a', -1) = '', 'Dup');
  C := 'x';
  Assert(DupCharA(C, 6) = 'xxxxxx', 'Dup');
  Assert(DupCharA(C, 0) = '', 'Dup');
  Assert(DupCharA(C, -1) = '', 'Dup');

  Assert(DupStrB('xy', 3) = 'xyxyxy', 'Dup');
  Assert(DupStrB('', 3) = '', 'Dup');
  Assert(DupStrB('a', 0) = '', 'Dup');
  Assert(DupStrB('a', -1) = '', 'Dup');
  C := 'x';
  Assert(DupCharB(C, 6) = 'xxxxxx', 'Dup');
  Assert(DupCharB(C, 0) = '', 'Dup');
  Assert(DupCharB(C, -1) = '', 'Dup');
  {$ENDIF}

  Assert(DupStrU('xy', 3) = 'xyxyxy', 'Dup');
  Assert(DupStrU('', 3) = '', 'Dup');
  Assert(DupStrU('a', 0) = '', 'Dup');
  Assert(DupStrU('a', -1) = '', 'Dup');
  D := 'x';
  Assert(DupCharU(D, 6) = 'xxxxxx', 'Dup');
  Assert(DupCharU(D, 0) = '', 'Dup');
  Assert(DupCharU(D, -1) = '', 'Dup');

  Assert(DupStr('xy', 3) = 'xyxyxy', 'Dup');
  Assert(DupStr('', 3) = '', 'Dup');
  Assert(DupStr('a', 0) = '', 'Dup');
  Assert(DupStr('a', -1) = '', 'Dup');
  E := 'x';
  Assert(DupChar(E, 6) = 'xxxxxx', 'Dup');
  Assert(DupChar(E, 0) = '', 'Dup');
  Assert(DupChar(E, -1) = '', 'Dup');

  { Pad                                                                        }
  {$IFDEF SupportAnsiString}
  Assert(StrPadLeftA('xxx', 'y', 6) = 'yyyxxx', 'PadLeft');
  Assert(StrPadLeftA('xxx', 'y', 2, True) = 'xx', 'PadLeft');
  Assert(StrPadLeftA('x', ' ', 3, True) = '  x', 'PadLeft');
  Assert(StrPadLeftA('xabc', ' ', 3, True) = 'xab', 'PadLeft');
  Assert(StrPadRightA('xxx', 'y', 6) = 'xxxyyy', 'PadRight');
  Assert(StrPadRightA('xxx', 'y', 2, True) = 'xx', 'PadRight');
  Assert(StrPadA('xxx', 'y', 7) = 'yyxxxyy', 'Pad');

  Assert(StrPadLeftB('xxx', 'y', 6) = 'yyyxxx', 'PadLeft');
  Assert(StrPadLeftB('xxx', 'y', 2, True) = 'xx', 'PadLeft');
  Assert(StrPadLeftB('x', ' ', 3, True) = '  x', 'PadLeft');
  Assert(StrPadLeftB('xabc', ' ', 3, True) = 'xab', 'PadLeft');
  Assert(StrPadRightB('xxx', 'y', 6) = 'xxxyyy', 'PadRight');
  Assert(StrPadRightB('xxx', 'y', 2, True) = 'xx', 'PadRight');
  Assert(StrPadB('xxx', 'y', 7) = 'yyxxxyy', 'Pad');
  {$ENDIF}

  Assert(StrPadLeftU('xxx', 'y', 6) = 'yyyxxx', 'PadLeft');
  Assert(StrPadLeftU('xxx', 'y', 2, True) = 'xx', 'PadLeft');
  Assert(StrPadLeftU('x', ' ', 3, True) = '  x', 'PadLeft');
  Assert(StrPadLeftU('xabc', ' ', 3, True) = 'xab', 'PadLeft');
  Assert(StrPadRightU('xxx', 'y', 6) = 'xxxyyy', 'PadRight');
  Assert(StrPadRightU('xxx', 'y', 2, True) = 'xx', 'PadRight');
  Assert(StrPadU('xxx', 'y', 7) = 'yyxxxyy', 'Pad');

  Assert(StrPadLeft('xxx', 'y', 6) = 'yyyxxx', 'PadLeft');
  Assert(StrPadLeft('xxx', 'y', 2, True) = 'xx', 'PadLeft');
  Assert(StrPadLeft('x', ' ', 3, True) = '  x', 'PadLeft');
  Assert(StrPadLeft('xabc', ' ', 3, True) = 'xab', 'PadLeft');
  Assert(StrPadRight('xxx', 'y', 6) = 'xxxyyy', 'PadRight');
  Assert(StrPadRight('xxx', 'y', 2, True) = 'xx', 'PadRight');
  Assert(StrPad('xxx', 'y', 7) = 'yyxxxyy', 'Pad');

  { Prefix/Suffix                                                              }
  {$IFDEF SupportAnsiString}
  S := 'ABC';
  StrEnsurePrefixA(S, '\');
  Assert(S = '\ABC', 'StrEnsurePrefix');
  StrEnsureSuffixA(S, '\');
  Assert(S = '\ABC\', 'StrEnsureSuffix');
  StrEnsureNoPrefixA(S, '\');
  Assert(S = 'ABC\', 'StrEnsureNoPrefix');
  StrEnsureNoSuffixA(S, '\');
  Assert(S = 'ABC', 'StrEnsureNoSuffix');
  for I := 0 to 256 do
    begin
      T := DupCharA('A', I);
      S := T;
      StrEnsurePrefixA(S, '\');
      Assert(S = '\' + T, 'StrEnsurePrefix');
      StrEnsureNoPrefixA(S, '\');
      Assert(S = T, 'StrEnsureNoPrefix');
      StrEnsureSuffixA(S, '\');
      Assert(S = T + '\', 'StrEnsureSuffix');
      StrEnsureNoSuffixA(S, '\');
      Assert(S = T, 'StrEnsureSuffix');
    end;
  {$ENDIF}

  { Split                                                                      }
  {$IFDEF SupportAnsiString}
  Assert(StrSplitAtCharA('ABC:X', AnsiChar(':'), S, T), 'StrSplitAtChar');
  Assert(S = 'ABC', 'StrSplitAtChar');
  Assert(T = 'X', 'StrSplitAtChar');
  Assert(not StrSplitAtCharA('ABC:X', AnsiChar(','), S, T), 'StrSplitAtChar');
  Assert(S = 'ABC:X', 'StrSplitAtChar');
  Assert(T = '', 'StrSplitAtChar');

  L := StrSplitA('', ',');
  Assert(Length(L) = 0, 'StrSplit');
  L := StrSplitA('ABC', ',');
  Assert(Length(L) = 1, 'StrSplit');
  Assert(L[0] = 'ABC', 'StrSplit');
  L := StrSplitA('ABC', '');
  Assert(Length(L) = 1, 'StrSplit');
  Assert(L[0] = 'ABC', 'StrSplit');
  L := StrSplitA('A,B,C', ',');
  Assert(Length(L) = 3, 'StrSplit');
  Assert(L[0] = 'A', 'StrSplit');
  Assert(L[1] = 'B', 'StrSplit');
  Assert(L[2] = 'C', 'StrSplit');
  L := StrSplitA('1,23,456', ',');
  Assert(Length(L) = 3, 'StrSplit');
  Assert(L[0] = '1', 'StrSplit');
  Assert(L[1] = '23', 'StrSplit');
  Assert(L[2] = '456', 'StrSplit');
  L := StrSplitA(',1,2,,3,', ',');
  Assert(Length(L) = 6, 'StrSplit');
  Assert(L[0] = '', 'StrSplit');
  Assert(L[1] = '1', 'StrSplit');
  Assert(L[2] = '2', 'StrSplit');
  Assert(L[3] = '', 'StrSplit');
  Assert(L[4] = '3', 'StrSplit');
  Assert(L[5] = '', 'StrSplit');
  L := StrSplitA('1..23..456', '..');
  Assert(Length(L) = 3, 'StrSplit');
  Assert(L[0] = '1', 'StrSplit');
  Assert(L[1] = '23', 'StrSplit');
  Assert(L[2] = '456', 'StrSplit');
  {$ENDIF}

  { Count                                                                      }
  {$IFDEF SupportAnsiString}
  Assert(StrCountCharA('abcxyzdexxyxyz', AnsiChar('x')) = 4);
  Assert(StrCountCharA('abcxyzdexxyxyz', AnsiChar('q')) = 0);
  Assert(StrCountCharA('abcxyzdexxyxyz', ['a'..'z']) = 14);

  Assert(StrCountCharB('abcxyzdexxyxyz', AnsiChar('x')) = 4);
  Assert(StrCountCharB('abcxyzdexxyxyz', AnsiChar('q')) = 0);
  Assert(StrCountCharB('abcxyzdexxyxyz', ['a'..'z']) = 14);
  {$ENDIF}

  Assert(StrCountCharU('abcxyzdexxyxyz', WideChar('x')) = 4);
  Assert(StrCountCharU('abcxyzdexxyxyz', WideChar('q')) = 0);
  {$IFDEF SupportAnsiChar}
  Assert(StrCountCharU('abcxyzdexxyxyz', ['a'..'z']) = 14);
  {$ENDIF}

  Assert(StrCountChar('abcxyzdexxyxyz', Char('x')) = 4);
  Assert(StrCountChar('abcxyzdexxyxyz', Char('q')) = 0);
  {$IFDEF SupportAnsiChar}
  Assert(StrCountChar('abcxyzdexxyxyz', ['a'..'z']) = 14);
  {$ENDIF}

  { Quoting                                                                    }
  {$IFDEF SupportAnsiString}
  Assert(StrRemoveSurroundingQuotesA('"123"') = '123', 'StrRemoveSurroundingQuotes');
  Assert(StrRemoveSurroundingQuotesA('"1""23"') = '1""23', 'StrRemoveSurroundingQuotes');
  Assert(StrQuoteA('Abe''s', '''') = '''Abe''''s''', 'StrQuote');
  Assert(StrUnquoteA('"123"') = '123', 'StrUnQuote');
  Assert(StrUnquoteA('"1""23"') = '1"23', 'StrUnQuote');
  {$ENDIF}

  Assert(StrRemoveSurroundingQuotesU('"123"') = '123', 'StrRemoveSurroundingQuotes');
  Assert(StrRemoveSurroundingQuotesU('"1""23"') = '1""23', 'StrRemoveSurroundingQuotes');
  Assert(StrQuoteU('Abe''s', '''') = '''Abe''''s''', 'StrQuote');
  Assert(StrUnquoteU('"123"') = '123', 'StrUnQuote');
  Assert(StrUnquoteU('"1""23"') = '1"23', 'StrUnQuote');

  Assert(StrRemoveSurroundingQuotes('"123"') = '123', 'StrRemoveSurroundingQuotes');
  Assert(StrRemoveSurroundingQuotes('"1""23"') = '1""23', 'StrRemoveSurroundingQuotes');
  Assert(StrQuote('Abe''s', '''') = '''Abe''''s''', 'StrQuote');
  Assert(StrUnquote('"123"') = '123', 'StrUnQuote');
  Assert(StrUnquote('"1""23"') = '1"23', 'StrUnQuote');

  {$IFDEF SupportAnsiString}
  Assert(StrIsQuotedStrA('"ABC""D"'), 'StrIsQuotedStr');
  Assert(StrIsQuotedStrA('"A"'), 'StrIsQuotedStr');
  Assert(not StrIsQuotedStrA('"ABC""D'''), 'StrIsQuotedStr');
  Assert(not StrIsQuotedStrA('"ABC""D'), 'StrIsQuotedStr');
  Assert(not StrIsQuotedStrA('"'), 'StrIsQuotedStr');
  Assert(not StrIsQuotedStrA(''), 'StrIsQuotedStr');
  Assert(StrIsQuotedStrA(''''''), 'StrIsQuotedStr');
  Assert(not StrIsQuotedStrA('''a'''''), 'StrIsQuotedStr');
  {$ENDIF}

  { Delimited                                                                  }
  {$IFDEF SupportAnsiString}
  Assert(StrAfterA('ABCDEF', 'CD') = 'EF', 'StrAfter');
  Assert(StrAfterA('ABCDEF', 'CE') = '', 'StrAfter');
  Assert(StrAfterA('ABCDEF', 'CE', True) = 'ABCDEF', 'StrAfter');
  Assert(StrAfterRevA('ABCABCABC', 'CA') = 'BC', 'StrAfterRev');
  Assert(StrAfterRevA('ABCABCABC', 'CD') = '', 'StrAfterRev');
  Assert(StrAfterRevA('ABCABCABC', 'CD', True) = 'ABCABCABC', 'StrAfterRev');

  Assert(StrBetweenCharA('ABC', AnsiChar('<'), AnsiChar('>')) = '', 'StrBetweenChar');
  Assert(StrBetweenCharA('ABC<D>', AnsiChar('<'), AnsiChar('>')) = 'D', 'StrBetweenChar');
  Assert(StrBetweenCharA('A*BC*D', AnsiChar('*'), AnsiChar('*')) = 'BC', 'StrBetweenChar');
  Assert(StrBetweenCharA('(ABC)', AnsiChar('('), AnsiChar(')')) = 'ABC', 'StrBetweenChar');
  Assert(StrBetweenCharA('XYZ(ABC)(DEF)', AnsiChar('('), AnsiChar(')')) = 'ABC', 'StrBetweenChar');
  Assert(StrBetweenCharA('XYZ"ABC', AnsiChar('"'), AnsiChar('"')) = '', 'StrBetweenChar');
  Assert(StrBetweenCharA('1234543210', AnsiChar('3'), AnsiChar('3'), False, False) = '454', 'StrBetweenChar');
  Assert(StrBetweenCharA('1234543210', AnsiChar('3'), AnsiChar('4'), False, False) = '', 'StrBetweenChar');
  Assert(StrBetweenCharA('1234543210', AnsiChar('4'), AnsiChar('3'), False, False) = '54', 'StrBetweenChar');
  Assert(StrBetweenCharA('1234543210', AnsiChar('4'), AnsiChar('6'), False, False) = '', 'StrBetweenChar');
  Assert(StrBetweenCharA('1234543210', AnsiChar('4'), AnsiChar('6'), False, True) = '543210', 'StrBetweenChar');
  {$ENDIF}

  Assert(StrBetweenCharU('ABC', WideChar('<'), WideChar('>')) = '', 'StrBetweenChar');
  Assert(StrBetweenCharU('ABC<D>', WideChar('<'), WideChar('>')) = 'D', 'StrBetweenChar');

  Assert(StrBetweenChar('ABC', Char('<'), Char('>')) = '', 'StrBetweenChar');
  Assert(StrBetweenChar('ABC<D>', Char('<'), Char('>')) = 'D', 'StrBetweenChar');

  {$IFDEF SupportAnsiString}
  Assert(StrBetweenA('XYZ(ABC)(DEF)', '(', [')']) = 'ABC', 'StrBetween');
  Assert(StrBetweenA('XYZ(ABC)(DEF)', 'Z(', [')']) = 'ABC', 'StrBetween');

  S := 'XYZ(ABC)<DEF>G"H"IJ"KLM"<N';
  Assert(StrRemoveCharDelimitedA(S, '<', '>') = 'DEF', 'StrRemoveCharDelimited');
  Assert(S = 'XYZ(ABC)G"H"IJ"KLM"<N', 'StrRemoveCharDelimited');
  Assert(StrRemoveCharDelimitedA(S, '<', '>') = '', 'StrRemoveCharDelimited');
  Assert(S = 'XYZ(ABC)G"H"IJ"KLM"<N', 'StrRemoveCharDelimited');
  Assert(StrRemoveCharDelimitedA(S, '(', ')') = 'ABC', 'StrRemoveCharDelimited');
  Assert(S = 'XYZG"H"IJ"KLM"<N', 'StrRemoveCharDelimited');
  Assert(StrRemoveCharDelimitedA(S, '"', '"') = 'H', 'StrRemoveCharDelimited');
  Assert(S = 'XYZGIJ"KLM"<N', 'StrRemoveCharDelimited');
  Assert(StrRemoveCharDelimitedA(S, '"', '"') = 'KLM', 'StrRemoveCharDelimited');
  Assert(S = 'XYZGIJ<N', 'StrRemoveCharDelimited');
  {$ENDIF}

  { Replace                                                                    }
  {$IFDEF SupportAnsiString}
  Assert(StrReplaceCharA(AnsiChar('X'), AnsiChar('A'), '') = '', 'StrReplaceChar');
  Assert(StrReplaceCharA(AnsiChar('X'), AnsiChar('A'), 'XXX') = 'AAA', 'StrReplaceChar');
  Assert(StrReplaceCharA(AnsiChar('X'), AnsiChar('A'), 'X') = 'A', 'StrReplaceChar');
  Assert(StrReplaceCharA(AnsiChar('X'), AnsiChar('!'), 'ABCXXBXAC') = 'ABC!!B!AC', 'StrReplaceChar');
  Assert(StrReplaceCharA(['A', 'B'], AnsiChar('C'), 'ABCDABCD') = 'CCCDCCCD', 'StrReplaceChar');
  Assert(StrReplaceA('', 'A', 'ABCDEF') = 'ABCDEF', 'StrReplace');
  Assert(StrReplaceA('B', 'A', 'ABCDEFEDCBA') = 'AACDEFEDCAA', 'StrReplace');
  Assert(StrReplaceA('BC', '', 'ABCDEFEDCBA') = 'ADEFEDCBA', 'StrReplace');
  Assert(StrReplaceA('A', '', 'ABAABAA') = 'BB', 'StrReplace');
  Assert(StrReplaceA('C', 'D', 'ABAABAA') = 'ABAABAA', 'StrReplace');
  Assert(StrReplaceA('B', 'CC', 'ABAABAA') = 'ACCAACCAA', 'StrReplace');
  Assert(StrReplaceA('a', 'b', 'bababa') = 'bbbbbb', 'StrReplace');
  Assert(StrReplaceA('a', '', 'bababa') = 'bbb', 'StrReplace');
  Assert(StrReplaceA('a', '', 'aaa') = '', 'StrReplace');
  Assert(StrReplaceA('aba', 'x', 'bababa') = 'bxba', 'StrReplace');
  Assert(StrReplaceA('b', 'bb', 'bababa') = 'bbabbabba', 'StrReplace');
  Assert(StrReplaceA('c', 'aa', 'bababa') = 'bababa', 'StrReplace');
  Assert(StrReplaceA('ba', '', 'bababa') = '', 'StrReplace');
  Assert(StrReplaceA('BA', '', 'bababa', False) = '', 'StrReplace');
  Assert(StrReplaceA('BA', 'X', 'bababa', False) = 'XXX', 'StrReplace');
  Assert(StrReplaceA('aa', '12', 'aaaaa') = '1212a', 'StrReplace');
  Assert(StrReplaceA('aa', 'a', 'aaaaa') = 'aaa', 'StrReplace');
  Assert(StrReplaceA(['b'], 'z', 'bababa') = 'zazaza', 'StrReplace');
  Assert(StrReplaceA(['b', 'a'], 'z', 'bababa') = 'zzzzzz', 'StrReplace');
  Assert(StrReplaceA('a', 'b', 'bababa') = 'bbbbbb', 'StrReplace');
  Assert(StrReplaceA('a', '', 'bababa') = 'bbb', 'StrReplace');
  Assert(StrReplaceA('a', '', 'aaa') = '', 'StrReplace');
  S := DupStrA('ABCDEFGH', 100000);
  S := StrReplaceA('BC', 'X', S);
  Assert(S = DupStrA('AXDEFGH', 100000), 'StrReplace');
  Assert(StrRemoveDupA('BBBAABABBA', 'B') = 'BAABABA', 'StrRemoveDup');
  Assert(StrRemoveDupA('azaazzel', 'a') = 'azazzel', 'StrRemoveDup');
  Assert(StrRemoveDupA('BBBAABABBA', 'A') = 'BBBABABBA', 'StrRemoveDup');
  Assert(StrRemoveCharSetA('BBBAABABBA', ['B']) = 'AAAA', 'StrRemoveChar');

  Assert(StrReplaceCharB(AnsiChar('X'), AnsiChar('A'), '') = '', 'StrReplaceChar');
  Assert(StrReplaceCharB(AnsiChar('X'), AnsiChar('A'), 'XXX') = 'AAA', 'StrReplaceChar');
  Assert(StrReplaceCharB(AnsiChar('X'), AnsiChar('A'), 'X') = 'A', 'StrReplaceChar');
  Assert(StrReplaceCharB(AnsiChar('X'), AnsiChar('!'), 'ABCXXBXAC') = 'ABC!!B!AC', 'StrReplaceChar');
  Assert(StrReplaceCharB(['A', 'B'], AnsiChar('C'), 'ABCDABCD') = 'CCCDCCCD', 'StrReplaceChar');
  Assert(StrReplaceB('', 'A', 'ABCDEF') = 'ABCDEF', 'StrReplace');
  Assert(StrReplaceB('B', 'A', 'ABCDEFEDCBA') = 'AACDEFEDCAA', 'StrReplace');
  Assert(StrReplaceB('BC', '', 'ABCDEFEDCBA') = 'ADEFEDCBA', 'StrReplace');
  Assert(StrReplaceB('A', '', 'ABAABAA') = 'BB', 'StrReplace');
  Assert(StrReplaceB('C', 'D', 'ABAABAA') = 'ABAABAA', 'StrReplace');
  Assert(StrReplaceB('B', 'CC', 'ABAABAA') = 'ACCAACCAA', 'StrReplace');
  Assert(StrReplaceB('a', 'b', 'bababa') = 'bbbbbb', 'StrReplace');
  Assert(StrReplaceB('a', '', 'bababa') = 'bbb', 'StrReplace');
  Assert(StrReplaceB('a', '', 'aaa') = '', 'StrReplace');
  Assert(StrReplaceB('aba', 'x', 'bababa') = 'bxba', 'StrReplace');
  Assert(StrReplaceB('b', 'bb', 'bababa') = 'bbabbabba', 'StrReplace');
  Assert(StrReplaceB('c', 'aa', 'bababa') = 'bababa', 'StrReplace');
  Assert(StrReplaceB('ba', '', 'bababa') = '', 'StrReplace');
  Assert(StrReplaceB('BA', '', 'bababa', False) = '', 'StrReplace');
  Assert(StrReplaceB('BA', 'X', 'bababa', False) = 'XXX', 'StrReplace');
  Assert(StrReplaceB('aa', '12', 'aaaaa') = '1212a', 'StrReplace');
  Assert(StrReplaceB('aa', 'a', 'aaaaa') = 'aaa', 'StrReplace');
  Assert(StrReplaceB(['b'], 'z', 'bababa') = 'zazaza', 'StrReplace');
  Assert(StrReplaceB(['b', 'a'], 'z', 'bababa') = 'zzzzzz', 'StrReplace');
  Assert(StrReplaceB('a', 'b', 'bababa') = 'bbbbbb', 'StrReplace');
  Assert(StrReplaceB('a', '', 'bababa') = 'bbb', 'StrReplace');
  Assert(StrReplaceB('a', '', 'aaa') = '', 'StrReplace');
  S := DupStrB('ABCDEFGH', 100000);
  S := StrReplaceB('BC', 'X', S);
  Assert(S = DupStrB('AXDEFGH', 100000), 'StrReplace');
  // Assert(StrRemoveDupB('BBBAABABBA', 'B') = 'BAABABA', 'StrRemoveDup');
  // Assert(StrRemoveDupB('azaazzel', 'a') = 'azazzel', 'StrRemoveDup');
  // Assert(StrRemoveDupB('BBBAABABBA', 'A') = 'BBBABABBA', 'StrRemoveDup');
  Assert(StrRemoveCharSetB('BBBAABABBA', ['B']) = 'AAAA', 'StrRemoveChar');
  {$ENDIF}

  Assert(StrReplaceCharU(WideChar('X'), WideChar('A'), '') = '', 'StrReplaceChar');
  Assert(StrReplaceCharU(WideChar('X'), WideChar('A'), 'XXX') = 'AAA', 'StrReplaceChar');
  Assert(StrReplaceCharU(WideChar('X'), WideChar('A'), 'X') = 'A', 'StrReplaceChar');
  Assert(StrReplaceCharU(WideChar('X'), WideChar('!'), 'ABCXXBXAC') = 'ABC!!B!AC', 'StrReplaceChar');
  {$IFDEF SupportAnsiChar}
  Assert(StrReplaceCharU(['A', 'B'], WideChar('C'), 'ABCDABCD') = 'CCCDCCCD', 'StrReplaceChar');
  {$ENDIF}
  Assert(StrReplaceU('', 'A', 'ABCDEF') = 'ABCDEF', 'StrReplace');
  Assert(StrReplaceU('B', 'A', 'ABCDEFEDCBA') = 'AACDEFEDCAA', 'StrReplace');
  Assert(StrReplaceU('BC', '', 'ABCDEFEDCBA') = 'ADEFEDCBA', 'StrReplace');
  Assert(StrReplaceU('A', '', 'ABAABAA') = 'BB', 'StrReplace');
  Assert(StrReplaceU('C', 'D', 'ABAABAA') = 'ABAABAA', 'StrReplace');
  Assert(StrReplaceU('B', 'CC', 'ABAABAA') = 'ACCAACCAA', 'StrReplace');
  Assert(StrReplaceU('a', 'b', 'bababa') = 'bbbbbb', 'StrReplace');
  Assert(StrReplaceU('a', '', 'bababa') = 'bbb', 'StrReplace');
  Assert(StrReplaceU('a', '', 'aaa') = '', 'StrReplace');
  Assert(StrReplaceU('aba', 'x', 'bababa') = 'bxba', 'StrReplace');
  Assert(StrReplaceU('b', 'bb', 'bababa') = 'bbabbabba', 'StrReplace');
  Assert(StrReplaceU('c', 'aa', 'bababa') = 'bababa', 'StrReplace');
  Assert(StrReplaceU('ba', '', 'bababa') = '', 'StrReplace');
  Assert(StrReplaceU('BA', '', 'bababa', False) = '', 'StrReplace');
  Assert(StrReplaceU('BA', 'X', 'bababa', False) = 'XXX', 'StrReplace');
  Assert(StrReplaceU('aa', '12', 'aaaaa') = '1212a', 'StrReplace');
  Assert(StrReplaceU('aa', 'a', 'aaaaa') = 'aaa', 'StrReplace');
  {$IFDEF SupportAnsiChar}
  Assert(StrReplaceU(['b'], 'z', 'bababa') = 'zazaza', 'StrReplace');
  Assert(StrReplaceU(['b', 'a'], 'z', 'bababa') = 'zzzzzz', 'StrReplace');
  {$ENDIF}
  Assert(StrReplaceU('a', 'b', 'bababa') = 'bbbbbb', 'StrReplace');
  Assert(StrReplaceU('a', '', 'bababa') = 'bbb', 'StrReplace');
  Assert(StrReplaceU('a', '', 'aaa') = '', 'StrReplace');
  W := DupStrU('ABCDEFGH', 100000);
  X := StrReplaceU('BC', 'X', W);
  Assert(X = DupStrU('AXDEFGH', 100000), 'StrReplace');
  Assert(StrRemoveDupU('BBBAABABBA', 'B') = 'BAABABA', 'StrRemoveDup');
  Assert(StrRemoveDupU('azaazzel', 'a') = 'azazzel', 'StrRemoveDup');
  Assert(StrRemoveDupU('BBBAABABBA', 'A') = 'BBBABABBA', 'StrRemoveDup');
  {$IFDEF SupportAnsiChar}
  Assert(StrRemoveCharSetU('BBBAABABBA', ['B']) = 'AAAA', 'StrRemoveChar');
  {$ENDIF}

  Assert(StrReplaceChar(Char('X'), Char('A'), '') = '', 'StrReplaceChar');
  Assert(StrReplaceChar(Char('X'), Char('A'), 'XXX') = 'AAA', 'StrReplaceChar');
  Assert(StrReplaceChar(Char('X'), Char('A'), 'X') = 'A', 'StrReplaceChar');
  Assert(StrReplaceChar(Char('X'), Char('!'), 'ABCXXBXAC') = 'ABC!!B!AC', 'StrReplaceChar');
  {$IFDEF SupportAnsiChar}
  Assert(StrReplaceChar(['A', 'B'], Char('C'), 'ABCDABCD') = 'CCCDCCCD', 'StrReplaceChar');
  {$ENDIF}
  Assert(StrReplace('', 'A', 'ABCDEF') = 'ABCDEF', 'StrReplace');
  Assert(StrReplace('B', 'A', 'ABCDEFEDCBA') = 'AACDEFEDCAA', 'StrReplace');
  Assert(StrReplace('BC', '', 'ABCDEFEDCBA') = 'ADEFEDCBA', 'StrReplace');
  Assert(StrReplace('A', '', 'ABAABAA') = 'BB', 'StrReplace');
  Assert(StrReplace('C', 'D', 'ABAABAA') = 'ABAABAA', 'StrReplace');
  Assert(StrReplace('B', 'CC', 'ABAABAA') = 'ACCAACCAA', 'StrReplace');
  Assert(StrReplace('a', 'b', 'bababa') = 'bbbbbb', 'StrReplace');
  Assert(StrReplace('a', '', 'bababa') = 'bbb', 'StrReplace');
  Assert(StrReplace('a', '', 'aaa') = '', 'StrReplace');
  Assert(StrReplace('aba', 'x', 'bababa') = 'bxba', 'StrReplace');
  Assert(StrReplace('b', 'bb', 'bababa') = 'bbabbabba', 'StrReplace');
  Assert(StrReplace('c', 'aa', 'bababa') = 'bababa', 'StrReplace');
  Assert(StrReplace('ba', '', 'bababa') = '', 'StrReplace');
  Assert(StrReplace('BA', '', 'bababa', False) = '', 'StrReplace');
  Assert(StrReplace('BA', 'X', 'bababa', False) = 'XXX', 'StrReplace');
  Assert(StrReplace('aa', '12', 'aaaaa') = '1212a', 'StrReplace');
  Assert(StrReplace('aa', 'a', 'aaaaa') = 'aaa', 'StrReplace');
  {$IFDEF SupportAnsiChar}
  Assert(StrReplace(['b'], 'z', 'bababa') = 'zazaza', 'StrReplace');
  Assert(StrReplace(['b', 'a'], 'z', 'bababa') = 'zzzzzz', 'StrReplace');
  {$ENDIF}
  Assert(StrReplace('a', 'b', 'bababa') = 'bbbbbb', 'StrReplace');
  Assert(StrReplace('a', '', 'bababa') = 'bbb', 'StrReplace');
  Assert(StrReplace('a', '', 'aaa') = '', 'StrReplace');
  Y := DupStr('ABCDEFGH', 100000);
  Z := StrReplace('BC', 'X', Y);
  Assert(Z = DupStr('AXDEFGH', 100000), 'StrReplace');
  Assert(StrRemoveDup('BBBAABABBA', 'B') = 'BAABABA', 'StrRemoveDup');
  Assert(StrRemoveDup('azaazzel', 'a') = 'azazzel', 'StrRemoveDup');
  Assert(StrRemoveDup('BBBAABABBA', 'A') = 'BBBABABBA', 'StrRemoveDup');
  {$IFDEF SupportAnsiChar}
  Assert(StrRemoveCharSet('BBBAABABBA', ['B']) = 'AAAA', 'StrRemoveChar');
  {$ENDIF}

  {$IFDEF SupportAnsiString}
  Assert(StrReplaceCharStrA('A', '', 'AXAYAA') = 'XY');
  Assert(StrReplaceCharStrA('A', 'B', 'AXAYAA') = 'BXBYBB');
  Assert(StrReplaceCharStrA('A', 'CC', 'AXAYAA') = 'CCXCCYCCCC');
  Assert(StrReplaceCharStrA('A', 'AIJK', 'AXAYAA') = 'AIJKXAIJKYAIJKAIJK');
  {$ENDIF}


  { Escaping                                                                   }
  {$IFDEF SupportAnsiString}
  Assert(StrHexEscapeB('ABCDE', ['C', 'D'], '\\', '//', False, True) =
         'AB\\43//\\44//E', 'StrHexEscape');
  Assert(StrHexEscapeB('ABCDE', ['C', 'E'], '\', '', False, True) =
         'AB\43D\45', 'StrHexEscape');
  Assert(StrHexEscapeB('ABCDE', ['F'], '\', '', False, True) =
         'ABCDE', 'StrHexEscape');
  Assert(StrHexUnescapeB('AB\\43\\44XYZ', '\\') = 'ABCDXYZ', 'StrHexUnescape');
  Assert(StrHexUnescapeB('ABC', '\') = 'ABC', 'StrHexUnescape');
  Assert(StrHexUnescapeB('ABC\44', '\') = 'ABCD', 'StrHexUnescape');
  Assert(StrHexUnescapeB('AB\\43\\44XYZ', '\\') = 'ABCDXYZ', 'StrHexUnescape');
  Assert(StrHexUnescapeB('AB%43%44XYZ', '%') = 'ABCDXYZ', 'StrHexUnescape');
  Assert(StrHexUnescapeB('AB%XYZ', '%') = 'ABXYZ', 'StrHexUnescape');

  Assert(StrCharEscapeA('ABCDE', ['C', 'D'], '\\', ['c', 'd']) =
         'AB\\c\\dE', 'StrCharEscape');
  Assert(StrCharEscapeA('ABCDE', ['C', 'E'], '\', ['c', 'e']) =
         'AB\cD\e', 'StrCharEscape');
  Assert(StrCharEscapeA('ABCDE', ['F'], '\', ['f']) =
         'ABCDE', 'StrCharEscape');
  Assert(StrCharUnescapeA('AB\\c\\dE', '\\', ['c', 'd'], ['C', 'D'], True, True) =
         'ABCDE', 'StrCharUnescape');
  {$ENDIF}
end;
{$ENDIF}



end.

