{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals 5.00                                        }
{   File name:        flcUnicodeCodecs.pas                                     }
{   File version:     5.27                                                     }
{   Description:      Unicode codecs                                           }
{                                                                              }
{   Copyright:        Copyright (c) 2002-2020                                  }
{                     David J Butler and Dieter Köhler                         }
{                     All rights reserved.                                     }
{                     See license below.                                       }
{                                                                              }
{   Github:           https://github.com/fundamentalslib                       }
{   E-mail:           fundamentals.library at gmail.com                        }
{                                                                              }
{   This unit is also part of the Open XML Utility Library.                    }
{   http://www.philo.de/xml/                                                   }
{                                                                              }
{ License:                                                                     }
{                                                                              }
{ The contents of this file are subject to the Mozilla Public License Version  }
{ 1.1 (the "License"); you may not use this file except in compliance with     }
{ the License. You may obtain a copy of the License at                         }
{ "http://www.mozilla.org/MPL/"                                                }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ The Original Code is "cUnicodeCodecs.pas".                                   }
{                                                                              }
{ The Initial Developers of the Original Code are David J Butler (Pretoria,    }
{ South Africa, "http://fundementals.sourceforge.net/") and Dieter Köhler      }
{ (Heidelberg, Germany, "http://www.philo.de/"). Portions created by the       }
{ Initial Developers are Copyright (C) 2002-2004 David J Butler and            }
{ Dieter Köhler. All Rights Reserved.                                          }
{                                                                              }
{ Alternatively, the contents of this file may be used under the terms of the  }
{ GNU General Public License Version 2 or later (the "GPL"), in which case the }
{ provisions of the GPL are applicable instead of those above. If you wish to  }
{ allow use of your version of this file only under the terms of the GPL, and  }
{ not to allow others to use your version of this file under the terms of the  }
{ MPL, indicate your decision by deleting the provisions above and replace     }
{ them with the notice and other provisions required by the GPL. If you do not }
{ delete the provisions above, a recipient may use your version of this file   }
{ under the terms of any one of the MPL or the GPL.                            }
{                                                                              }
{ Description:                                                                 }
{                                                                              }
{   Codecs (encoders/decoders) for Unicode text.                               }
{                                                                              }
{   To decode or encode Unicode text, use one of the EncodingToUTF16 or        }
{   UTF16ToEncoding functions.                                                 }
{                                                                              }
{   For example, to convert an ISO-8859-1 string into an Unicode string:       }
{                                                                              }
{     WideStr := EncodingToUTF16(TISO8859_1Codec, 'ISO-8859-1 String');        }
{                                                                              }
{   or alternatively, using an alias:                                          }
{                                                                              }
{     WideStr := EncodingToUTF16('iso-8859-1', 'ISO-8859-1 String');           }
{     WideStr := EncodingToUTF16('latin1', 'ISO-8859-1 String');               }
{                                                                              }
{ Revision history:                                                            }
{                                                                              }
{   2002/04/17  0.01  Initial version. ISO8859, Mac, Win1250-1252, UTF.        }
{   2002/04/20  0.02  EBCDIC-US.                                               }
{   2002/10/28  3.03  Refactored.                                              }
{   2002/10/29  3.04  UTF-8 string functions.                                  }
{   2002/11/04  3.05  Test cases. Fixed bug in UTF-8 encoding function.        }
{   2003/05/23  3.06  Detection routines.                                      }
{   2003/09/28  3.07  Renamed ASCII to USASCII for clarity.                    }
{   2003/10/30  3.08  Moved character mappings to unit cUnicodeMaps.           }
{   2004/01/10  3.09  Moved generic functions to cUnicodeChar and cUnicode.    }
{                     Revision of codec classes.                               }
{   2004/03/15  3.10  Moved character mappings into codec classes.             }
{                     UCS2 codec.                                              }
{   2004/04/11  3.11  Improved Read/Write functions.                           }
{   2004/04/19  3.12  Small revisions.                                         }
{   2004/08/11  3.13  Minor change to UCS4 functions.                          }
{   2004/11/06  3.14  Codec alias functions.                                   }
{   2005/08/27  4.15  Revised for Fundamentals 4.                              }
{   2008/12/30  4.16  Revision.                                                }
{   2011/09/28  4.17  Revision for Delphi 2009 Win32.                          }
{   2012/05/23  4.18  Fixed bug in TUCS2Codec.Decode reported by brianjford.   }
{   2012/08/25  4.19  UnicodeString functions.                                 }
{   2015/03/14  4.20  Use RawByteString to avoid auto string conversions.      }
{   2015/05/06  4.21  Move UTF functions to unit cUtils.                       }
{   2016/01/09  5.22  Revised for Fundamentals 5.                              }
{   2016/04/16  5.23  Use virtual GetAlias instead of GetAliasList.            }
{   2018/08/12  5.24  String type changes.                                     }
{   2019/04/28  5.25  Reduce dependencies.                                     }
{   2019/10/03  5.26  RegisterDefaultCodecs not called in unit initialization  }
{                     but in GetCodecByAlias.                                  }
{   2020/06/10  5.27  Fix little endian, big endian issue.                     }
{                                                                              }
{ Supported compilers:                                                         }
{                                                                              }
{   Delphi 2010-10.4 Win32/Win64        5.26  2020/06/02                       }
{   Delphi 10.2-10.4 Linux64            5.26  2020/06/02                       }
{   FreePascal 3.0.4 Win64              5.26  2020/06/02                       }
{                                                                              }
{******************************************************************************}

{$INCLUDE ..\flcInclude.inc}

{$IFDEF FREEPASCAL}
  {$WARNINGS OFF}
  {$HINTS OFF}
  {$NOTES OFF}
{$ENDIF}

{$IFDEF DEBUG}
{$IFDEF TEST}
  {$DEFINE UNICODECODECS_TEST}
{$ENDIF}
{$ENDIF}

unit flcUnicodeCodecs;

interface

uses
  { System }
  SysUtils,

  { Fundamentals }
  flcStdTypes;



{                                                                              }
{ TCustomUnicodeCodec                                                          }
{   Base class for Unicode Codec implementations.                              }
{                                                                              }
type
  TCodecErrorAction = (
      eaException,   // Raise an exception (default)
      eaStop,        // Stop encoding/decoding
      eaIgnore,      // Ignore error and continue
      eaSkip,        // Skip character and continue
      eaReplace);    // Replace invalid character and continue

  TCodecReadLFOption = (
      lrPass,        // No normalization takes place (default)
      lrNormalize);  // Line breaks are adjusted to Linux-style breaks with a
                     // single LINE FEED, i.e. a sequence of CARRIAGE RETURN
                     // ($0D) + LINE FEED ($0A) or a single CARRIAGE RETURN is
                     // normalized to a single LINE FEED ($0A)

  TCodecWriteLFOption = (
      lwLF,          // Transcode LINE FEED into LINE FEED (default)
      lwCR,          // Transcode LINE FEED into CARRIAGE RETURN
      lwCRLF);       // Transcode LINE FEED into CARRIAGE RETURN + LINE FEED

  TCodecReadEvent = procedure (Sender: TObject; var Buf; Count: Longint;
                               var Ok: Boolean) of object;
  TCodecWriteEvent = procedure (Sender: TObject; const Buf; Count: Longint)
                                of object;

  TCustomUnicodeCodec = class
  private
    FErrorAction        : TCodecErrorAction;
    FDecodeReplaceChar  : WideChar;
    FReadLFOption       : TCodecReadLFOption;
    FWriteLFOption      : TCodecWriteLFOption;
    FOnRead             : TCodecReadEvent;
    FOnWrite            : TCodecWriteEvent;
    FReadAhead          : Boolean;   // Flag used for LF input normalization
    FReadAheadBuffer    : UCS4Char;  // Buffer storage LF input normalization
    FReadAheadByteCount : Integer;   // Buffer storage LF input normalization

  protected
    procedure ResetReadAhead;

    procedure SetDecodeReplaceChar(const Value: WideChar);
    procedure SetErrorAction(const Value: TCodecErrorAction);
    procedure SetReadLFOption(const Value: TCodecReadLFOption); virtual;
    procedure SetWriteLFOption(const Value: TCodecWriteLFOption); virtual;
    procedure SetOnRead(const Value: TCodecReadEvent);

    function  ReadBuffer(var Buf; Count: Integer): Boolean;
    procedure WriteBuffer(const Buf; Count: Integer);

    procedure InternalReadUCS4Char(out C: UCS4Char;
              out ByteCount: Integer); virtual; abstract;
              // Implementation guidelines for derived classes:
              // ReadUCS4Char calls (if necessary repeatedly and sometimes
              // ahead) the InternalReadUCS4Char function, which must call
              // ReadBuffer to request buffer values. ReadBuffer calls the
              // OnRead event to request buffer values, similar to the Delphi
              // VCL TStream.Read function.
              // It must raise an EConvertError exception if the byte values
              // returned by the OnRead event contain code that cannot be
              // converted to a UCS-4 character or if the result value falls
              // into the reserved surrogate area [$D800..$DFFF].
              // If ReadBuffer returns False, the UCS-4 character $9C
              // (STRING TERMINATOR) must be returned.
              // LINE FEED characters ($A) are transformed according to the
              // value of ReadLFOption property by the ReadUCS4Char function.
    procedure InternalWriteUCS4Char(const C: UCS4Char;
              out ByteCount: Integer); virtual; abstract;
              // Implementation guideline for derived classes:
              // WriteUCS4Char calls (if necessary repeatedly) the
              // InternalWriteUCS4Char procedure, which must call WriteBuffer
              // to write buffer values. WriteBuffer calls the OnWrite event
              // to send the buffer values, similar to the Delphi VCL
              // TStream.Write function.
              // It must raise an EConvertError exception if the specified
              // UCS-4 character cannot be converted into the target encoding.
              // If no OnWrite event handler is assigned calling WriteUCS4Char
              // simply has no effect.
              // LINE FEED characters ($A) are transformed according to the
              // value of the WriteLFOption property by the WriteUCS4Char
              // procedure.

  public
    constructor Create; virtual;
    constructor CreateEx(const AErrorAction: TCodecErrorAction = eaException;
                const ADecodeReplaceChar: WideChar = WideChar(#$FFFD);
                const AReadLFOption: TCodecReadLFOption = lrPass;
                const AWriteLFOption: TCodecWriteLFOption = lwCRLF);

    class function GetAliasCount: Integer; virtual;
    class function GetAlias(const Idx: Integer): String; virtual;

    class function IsAlias(const Alias: String): Boolean;
    {$IFDEF SupportAnsiString}
    class function IsAliasA(const Alias: AnsiString): Boolean;
    {$ENDIF}
    class function IsAliasU(const Alias: UnicodeString): Boolean;

    procedure Decode(const Buf: Pointer; const BufSize: Integer;
              const DestBuf: Pointer; const DestSize: Integer;
              out ProcessedBytes, DestLength: Integer); virtual; abstract;
    function  Encode(const S: PWideChar; const Length: Integer;
              out ProcessedChars: Integer): RawByteString; virtual; abstract;

    procedure DecodeStr(const Buf: Pointer; const BufSize: Integer;
              var Dest: UnicodeString);
    function  EncodeStr(const S: UnicodeString): RawByteString;

    procedure ReadUCS4Char(out C: UCS4Char; out ByteCount: Integer);
    procedure WriteUCS4Char(const C: UCS4Char; out ByteCount: Integer); virtual;
              // Implementation guideline for derived classes:
              // WriteUCS4Char can be overridden to implement more efficient
              // handling of LINE FEED character transformations.

    property  ErrorAction: TCodecErrorAction read FErrorAction write SetErrorAction default eaException;
    property  DecodeReplaceChar: WideChar read FDecodeReplaceChar write SetDecodeReplaceChar default #$FFFD;
    property  ReadLFOption: TCodecReadLFOption read FReadLFOption write SetReadLFOption default lrPass;
    property  WriteLFOption: TCodecWriteLFOption read FWriteLFOption write SetWriteLFOption default lwLF;

    property  OnRead: TCodecReadEvent read FOnRead write SetOnRead;
    property  OnWrite: TCodecWriteEvent read FOnWrite write FOnWrite;
  end;

  TUnicodeCodecClass = class of TCustomUnicodeCodec;

  EUnicodeCodecException = class(Exception)
    ProcessedBytes : Integer;
  end;



{                                                                              }
{ Codec registration                                                           }
{                                                                              }
procedure RegisterCodecs(const Codecs: array of TUnicodeCodecClass);



{                                                                              }
{ Codec aliases                                                                }
{                                                                              }
function  GetCodecClassByAlias(const CodecAlias: String): TUnicodeCodecClass;
function  GetCodecClassByAliasA(const CodecAlias: RawByteString): TUnicodeCodecClass;
function  GetCodecClassByAliasU(const CodecAlias: UnicodeString): TUnicodeCodecClass;



{$IFDEF OS_MSWIN}
{                                                                              }
{ Windows system encoding functions                                            }
{                                                                              }
function  GetSystemEncodingName: String; {$IFDEF DELPHI6_UP}platform;{$ENDIF}
function  GetSystemEncodingCodecClass: TUnicodeCodecClass; {$IFDEF DELPHI6_UP}platform;{$ENDIF}
{$ENDIF}



{                                                                              }
{ Encoding detection                                                           }
{                                                                              }
function  DetectUTFEncoding(const Buf: Pointer; const BufSize: Integer;
          out BOMSize: Integer): TUnicodeCodecClass;



{                                                                              }
{ Encoding conversion functions                                                }
{                                                                              }
function  EncodingToUTF16U(const CodecClass: TUnicodeCodecClass;
          const Buf: Pointer; const BufSize: Integer): UnicodeString; overload;
function  EncodingToUTF16U(const CodecClass: TUnicodeCodecClass;
          const S: RawByteString): UnicodeString; overload;

function  EncodingToUTF16U(const CodecAlias: String;
          const Buf: Pointer; const BufSize: Integer): UnicodeString; overload;
function  EncodingToUTF16U(const CodecAlias: String;
          const S: RawByteString): UnicodeString; overload;

function  UTF16ToEncodingU(const CodecClass: TUnicodeCodecClass;
          const S: UnicodeString): RawByteString; overload;
function  UTF16ToEncodingU(const CodecAlias: String;
          const S: UnicodeString): RawByteString; overload;



{                                                                              }
{ TUTF8Codec                                                                   }
{   Unicode Codec implementation for UTF-8.                                    }
{                                                                              }
type
  TUTF8Codec = class(TCustomUnicodeCodec)
  protected
    procedure InternalReadUCS4Char(out C: UCS4Char;
              out ByteCount: Integer); override;
    procedure InternalWriteUCS4Char(const C: UCS4Char;
              out ByteCount: Integer); override;

  public
    class function GetAliasCount: Integer; override;
    class function GetAlias(const Idx: Integer): String; override;

    procedure Decode(const Buf: Pointer; const BufSize: Integer;
              const DestBuf: Pointer; const DestSize: Integer;
              out ProcessedBytes, DestLength: Integer); override;
    function  Encode(const S: PWideChar; const Length: Integer;
              out ProcessedChars: Integer): RawByteString; override;
    procedure WriteUCS4Char(const C: UCS4Char; out ByteCount: Integer); override;
  end;



{                                                                              }
{ TUTF16LECodec                                                                }
{   Unicode Codec implementation for UTF-16LE.                                 }
{                                                                              }
type
  TUTF16LECodec = class(TCustomUnicodeCodec)
  protected
    procedure InternalReadUCS4Char(out C: UCS4Char;
              out ByteCount: Integer); override;
    procedure InternalWriteUCS4Char(const C: UCS4Char;
              out ByteCount: Integer); override;

  public
    class function GetAliasCount: Integer; override;
    class function GetAlias(const Idx: Integer): String; override;

    procedure Decode(const Buf: Pointer; const BufSize: Integer;
              const DestBuf: Pointer; const DestSize: Integer;
              out ProcessedBytes, DestLength: Integer); override;
    function  Encode(const S: PWideChar; const Length: Integer;
              out ProcessedChars: Integer): RawByteString; override;
    procedure WriteUCS4Char(const C: UCS4Char; out ByteCount: Integer); override;
  end;



{                                                                              }
{ TUTF16BECodec                                                                }
{   Unicode Codec implementation for UTF-16BE.                                 }
{                                                                              }
type
  TUTF16BECodec = class(TCustomUnicodeCodec)
  protected
    procedure InternalReadUCS4Char(out C: UCS4Char;
              out ByteCount: Integer); override;
    procedure InternalWriteUCS4Char(const C: UCS4Char;
              out ByteCount: Integer); override;

  public
    class function GetAliasCount: Integer; override;
    class function GetAlias(const Idx: Integer): String; override;

    procedure Decode(const Buf: Pointer; const BufSize: Integer;
              const DestBuf: Pointer; const DestSize: Integer;
              out ProcessedBytes, DestLength: Integer); override;
    function  Encode(const S: PWideChar; const Length: Integer;
              out ProcessedChars: Integer): RawByteString; override;
    procedure WriteUCS4Char(const C: UCS4Char; out ByteCount: Integer); override;
  end;



{                                                                              }
{ TUCS4BECodec                                                                 }
{   Unicode Codec implementation for ISO 10646 UCS-4BE.                        }
{                                                                              }
type
  TUCS4BECodec = class(TCustomUnicodeCodec)
  protected
    procedure InternalReadUCS4Char(out C: UCS4Char;
              out ByteCount: Integer); override;
    procedure InternalWriteUCS4Char(const C: UCS4Char;
              out ByteCount: Integer); override;

  public
    procedure Decode(const Buf: Pointer; const BufSize: Integer;
              const DestBuf: Pointer; const DestSize: Integer;
              out ProcessedBytes, DestLength: Integer); override;
    function  Encode(const S: PWideChar; const Length: Integer;
              out ProcessedChars: Integer): RawByteString; override;
    procedure WriteUCS4Char(const C: UCS4Char; out ByteCount: Integer); override;
  end;



{                                                                              }
{ TUCS4LECodec                                                                 }
{   Unicode Codec implementation for ISO 10646 UCS-4LE.                        }
{                                                                              }
type
  TUCS4LECodec = class(TCustomUnicodeCodec)
  protected
    procedure InternalReadUCS4Char(out C: UCS4Char;
              out ByteCount: Integer); override;
    procedure InternalWriteUCS4Char(const C: UCS4Char;
              out ByteCount: Integer); override;

  public
    procedure Decode(const Buf: Pointer; const BufSize: Integer;
              const DestBuf: Pointer; const DestSize: Integer;
              out ProcessedBytes, DestLength: Integer); override;
    function  Encode(const S: PWideChar; const Length: Integer;
              out ProcessedChars: Integer): RawByteString; override;
    procedure WriteUCS4Char(const C: UCS4Char; out ByteCount: Integer); override;
  end;



{                                                                              }
{ TUCS4_2143Codec                                                              }
{   Unicode Codec implementation for UCS-4 2143.                               }
{                                                                              }
type
  TUCS4_2143Codec = class(TCustomUnicodeCodec)
  protected
    procedure InternalReadUCS4Char(out C: UCS4Char;
              out ByteCount: Integer); override;
    procedure InternalWriteUCS4Char(const C: UCS4Char;
              out ByteCount: Integer); override;

  public
    procedure Decode(const Buf: Pointer; const BufSize: Integer;
              const DestBuf: Pointer; const DestSize: Integer;
              out ProcessedBytes, DestLength: Integer); override;
    function  Encode(const S: PWideChar; const Length: Integer;
              out ProcessedChars: Integer): RawByteString; override;
    procedure WriteUCS4Char(const C: UCS4Char; out ByteCount: Integer); override;
  end;



{                                                                              }
{ TUCS4_3412Codec                                                              }
{   Unicode Codec implementation for UCS-4 3412.                               }
{                                                                              }
type
  TUCS4_3412Codec = class(TCustomUnicodeCodec)
  protected
    procedure InternalReadUCS4Char(out C: UCS4Char;
              out ByteCount: Integer); override;
    procedure InternalWriteUCS4Char(const C: UCS4Char;
              out ByteCount: Integer); override;

  public
    procedure Decode(const Buf: Pointer; const BufSize: Integer;
              const DestBuf: Pointer; const DestSize: Integer;
              out ProcessedBytes, DestLength: Integer); override;
    function  Encode(const S: PWideChar; const Length: Integer;
              out ProcessedChars: Integer): RawByteString; override;
    procedure WriteUCS4Char(const C: UCS4Char; out ByteCount: Integer); override;
  end;



{                                                                              }
{ TUCS2Codec                                                                   }
{   Unicode Codec implementation for ISO 10646 UCS-2.                          }
{                                                                              }
type
  TUCS2Codec = class(TCustomUnicodeCodec)
  protected
    procedure InternalReadUCS4Char(out C: UCS4Char;
              out ByteCount: Integer); override;
    procedure InternalWriteUCS4Char(const C: UCS4Char;
              out ByteCount: Integer); override;

  public
    procedure Decode(const Buf: Pointer; const BufSize: Integer;
              const DestBuf: Pointer; const DestSize: Integer;
              out ProcessedBytes, DestLength: Integer); override;
    function  Encode(const S: PWideChar; const Length: Integer;
              out ProcessedChars: Integer): RawByteString; override;
    procedure WriteUCS4Char(const C: UCS4Char; out ByteCount: Integer); override;
  end;



{                                                                              }
{ TCustomSingleByteCodec                                                       }
{   Base class for single-byte encodings.                                      }
{                                                                              }
type
  TCustomSingleByteCodec = class(TCustomUnicodeCodec)
  protected
    FEncodeReplaceChar : ByteChar;

    procedure InternalReadUCS4Char(out C: UCS4Char;
              out ByteCount: Integer); override;
    procedure InternalWriteUCS4Char(const C: UCS4Char;
              out ByteCount: Integer); override;

  public
    constructor Create; override;
    constructor CreateEx(const ErrorAction: TCodecErrorAction = eaException;
                const DecodeReplaceChar: WideChar = WideChar(#$FFFD);
                const EncodeReplaceChar: ByteChar = ByteChar(#32));

    property  EncodeReplaceChar: ByteChar read FEncodeReplaceChar write FEncodeReplaceChar;

    procedure Decode(const Buf: Pointer; const BufSize: Integer;
              const DestBuf: Pointer; const DestSize: Integer;
              out ProcessedBytes, DestLength: Integer); override;
    function  Encode(const S: PWideChar; const Length: Integer;
              out ProcessedChars: Integer): RawByteString; override;

    function  DecodeChar(const P: ByteChar): WideChar; virtual; abstract;
    function  EncodeChar(const Ch: WideChar): ByteChar; virtual; abstract;

    function  DecodeUCS4Char(const P: ByteChar): UCS4Char; virtual;
    function  EncodeUCS4Char(const Ch: UCS4Char): ByteChar; virtual;
  end;

  TUnicodeSingleByteCodecClass = class of TCustomSingleByteCodec;



{                                                                              }
{ ISO-8859                                                                     }
{                                                                              }
type
  TISO8859_1Codec = class(TCustomSingleByteCodec)
  public
    class function GetAliasCount: Integer; override;
    class function GetAlias(const Idx: Integer): String; override;

    procedure Decode(const Buf: Pointer; const BufSize: Integer;
              const DestBuf: Pointer; const DestSize: Integer;
              out ProcessedBytes, DestLength: Integer); override;
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TISO8859_2Codec = class(TCustomSingleByteCodec)
  public
    class function GetAliasCount: Integer; override;
    class function GetAlias(const Idx: Integer): String; override;

    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TISO8859_3Codec = class(TCustomSingleByteCodec)
  public
    class function GetAliasCount: Integer; override;
    class function GetAlias(const Idx: Integer): String; override;

    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TISO8859_4Codec = class(TCustomSingleByteCodec)
  public
    class function GetAliasCount: Integer; override;
    class function GetAlias(const Idx: Integer): String; override;

    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TISO8859_5Codec = class(TCustomSingleByteCodec)
  public
    class function GetAliasCount: Integer; override;
    class function GetAlias(const Idx: Integer): String; override;

    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TISO8859_6Codec = class(TCustomSingleByteCodec)
  public
    class function GetAliasCount: Integer; override;
    class function GetAlias(const Idx: Integer): String; override;

    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TISO8859_7Codec = class(TCustomSingleByteCodec)
  public
    class function GetAliasCount: Integer; override;
    class function GetAlias(const Idx: Integer): String; override;

    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TISO8859_8Codec = class(TCustomSingleByteCodec)
  public
    class function GetAliasCount: Integer; override;
    class function GetAlias(const Idx: Integer): String; override;

    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TISO8859_9Codec = class(TCustomSingleByteCodec)
  public
    class function GetAliasCount: Integer; override;
    class function GetAlias(const Idx: Integer): String; override;

    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TISO8859_10Codec = class(TCustomSingleByteCodec)
  public
    class function GetAliasCount: Integer; override;
    class function GetAlias(const Idx: Integer): String; override;

    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TISO8859_13Codec = class(TCustomSingleByteCodec)
  public
    class function GetAliasCount: Integer; override;
    class function GetAlias(const Idx: Integer): String; override;

    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TISO8859_14Codec = class(TCustomSingleByteCodec)
  public
    class function GetAliasCount: Integer; override;
    class function GetAlias(const Idx: Integer): String; override;

    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TISO8859_15Codec = class(TCustomSingleByteCodec)
  public
    class function GetAliasCount: Integer; override;
    class function GetAlias(const Idx: Integer): String; override;

    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;



{                                                                              }
{ Windows                                                                      }
{                                                                              }
type
  TWindows37Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TWindows437Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TWindows500Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TWindows708Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TWindows737Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TWindows775Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TWindows850Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TWindows852Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TWindows855Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TWindows857Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TWindows858Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TWindows861Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TWindows862Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TWindows863Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TWindows864Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TWindows865Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TWindows866Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TWindows869Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TWindows870Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TWindows874Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TWindows875Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TWindows1026Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TWindows1047Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TWindows1140Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TWindows1141Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TWindows1142Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TWindows1143Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TWindows1144Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TWindows1145Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TWindows1146Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TWindows1147Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TWindows1148Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TWindows1149Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TWindows1250Codec = class(TCustomSingleByteCodec)
  public
    class function GetAliasCount: Integer; override;
    class function GetAlias(const Idx: Integer): String; override;

    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TWindows1251Codec = class(TCustomSingleByteCodec)
  public
    class function GetAliasCount: Integer; override;
    class function GetAlias(const Idx: Integer): String; override;

    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TWindows1252Codec = class(TCustomSingleByteCodec)
  public
    class function GetAliasCount: Integer; override;
    class function GetAlias(const Idx: Integer): String; override;

    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TWindows1253Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TWindows1254Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TWindows1255Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TWindows1256Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TWindows1257Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TWindows1258Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;



{                                                                              }
{ IBM                                                                          }
{                                                                              }
type
  TIBM037Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TIBM038Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TIBM256Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TIBM273Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TIBM274Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TIBM275Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TIBM277Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TIBM278Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TIBM280Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TIBM281Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TIBM284Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TIBM285Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TIBM290Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TIBM297Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TIBM420Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TIBM423Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TIBM424Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TIBM437Codec = class(TCustomSingleByteCodec)
  public
    class function GetAliasCount: Integer; override;
    class function GetAlias(const Idx: Integer): String; override;

    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TIBM500Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TIBM850Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TIBM851Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TIBM852Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TIBM855Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TIBM857Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TIBM860Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TIBM861Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TIBM862Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TIBM863Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TIBM864Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TIBM865Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TIBM866Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TIBM868Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TIBM869Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TIBM870Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TIBM871Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TIBM874Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TIBM875Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TIBM880Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TIBM904Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TIBM905Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TIBM918Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TIBM1004Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TIBM1026Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TIBM1047Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;




{                                                                              }
{ Macintosh                                                                    }
{                                                                              }
type
  TMacLatin2Codec = class(TCustomSingleByteCodec)
  public
    class function GetAliasCount: Integer; override;
    class function GetAlias(const Idx: Integer): String; override;

    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TMacRomanCodec = class(TCustomSingleByteCodec)
  public
    class function GetAliasCount: Integer; override;
    class function GetAlias(const Idx: Integer): String; override;

    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TMacCyrillicCodec = class(TCustomSingleByteCodec)
  public
    class function GetAliasCount: Integer; override;
    class function GetAlias(const Idx: Integer): String; override;

    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TMacGreekCodec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TMacIcelandicCodec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TMacTurkishCodec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;




{                                                                              }
{ International                                                                }
{                                                                              }
type
  TUSASCIICodec = class(TCustomSingleByteCodec)
  public
    class function GetAliasCount: Integer; override;
    class function GetAlias(const Idx: Integer): String; override;

    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TEBCDIC_USCodec = class(TCustomSingleByteCodec)
  public
    class function GetAliasCount: Integer; override;
    class function GetAlias(const Idx: Integer): String; override;

    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TKOI8_RCodec = class(TCustomSingleByteCodec)
  public
    class function GetAliasCount: Integer; override;
    class function GetAlias(const Idx: Integer): String; override;

    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TJIS_X0201Codec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;

  TNextStepCodec = class(TCustomSingleByteCodec)
  public
    function  DecodeChar(const P: ByteChar): WideChar; override;
    function  EncodeChar(const Ch: WideChar): ByteChar; override;
  end;



{                                                                              }
{ Test cases                                                                   }
{                                                                              }
{$IFDEF UNICODECODECS_TEST}
procedure Test;
{$ENDIF}



implementation

uses
  { System }
  {$IFDEF MSWIN}
  Windows,
  {$ENDIF}

  { Fundamentals }
  flcUTF;



resourcestring
  SCannotConvert          = 'Unicode code point $%x has no equivalent in %s';
  SCannotConvertUCS4      = 'Cannot convert $%8.8X to %s';
  SHighSurrogateNotFound  = 'High surrogate not found';
  SInvalidCodePoint       = '$%x is not a valid %s code point';
  SInvalidEncoding        = 'Invalid %s encoding';
  SLongStringConvertError = 'Long string conversion error';
  SLowSurrogateNotFound   = 'Low surrogate not found';
  SSurrogateNotAllowed    = 'Surrogate value $%x found in %s. Values between $D800 and $DFFF are reserved for use with UTF-16';
  SEncodingOutOfRange     = '%s encoding out of range';
  SUTF8Error              = 'UTF-8 error %d';
  SAliasIndexOutOfRange   = 'Alias index out of range';



{                                                                              }
{ Helper Functions                                                             }
{                                                                              }
type
  AnsiCharMap = array[AnsiChar] of WideChar;

function CharFromMap(const Ch: WideChar; const Map: AnsiCharMap;
    const Encoding: String): AnsiChar;
var I : AnsiChar;
    P : PWideChar;
begin
  if Ch = #$FFFF then
    raise EConvertError.CreateFmt(SCannotConvert, [Ord(Ch), Encoding]);
  P := @Map;
  for I := AnsiChar($00) to AnsiChar($FF) do
    if P^ <> Ch then
      Inc(P)
    else
      begin
        Result := I;
        exit;
      end;
  raise EConvertError.CreateFmt(SCannotConvert, [Ord(Ch), Encoding]);
end;

type
  AnsiCharHighMap = array[$80..$FF] of WideChar;

function CharFromHighMap(const Ch: WideChar; const Map: AnsiCharHighMap;
    const Encoding: String): AnsiChar;
var I : AnsiChar;
    P : PWideChar;
begin
  if Ord(Ch) < $80 then
    begin
      Result := AnsiChar(Ch);
      exit;
    end;
  if Ch = #$FFFF then
    raise EConvertError.CreateFmt(SCannotConvert, [Ord(Ch), Encoding]);
  P := @Map;
  for I := AnsiChar($80) to AnsiChar($FF) do
    if P^ <> Ch then
      Inc(P)
    else
      begin
        Result := I;
        exit;
      end;
  raise EConvertError.CreateFmt(SCannotConvert, [Ord(Ch), Encoding]);
end;

type
  AnsiCharISOMap = array[$A0..$FF] of WideChar;

function CharFromISOMap(const Ch: WideChar; const Map: AnsiCharISOMap;
    const Encoding: String): AnsiChar;
var I : AnsiChar;
    P : PWideChar;
begin
  if Ord(Ch) < $A0 then
    begin
      Result := AnsiChar(Ch);
      exit;
    end;
  if Ch = #$FFFF then
    raise EConvertError.CreateFmt(SCannotConvert, [Ord(Ch), Encoding]);
  P := @Map;
  for I := AnsiChar($A0) to AnsiChar($FF) do
    if P^ <> Ch then
      Inc(P)
    else
      begin
        Result := I;
        exit;
      end;
  raise EConvertError.CreateFmt(SCannotConvert, [Ord(Ch), Encoding]);
end;

procedure RawByteBufToWideBuf(const Buf: Pointer; const BufSize: Integer;
    const DestBuf: Pointer);
var I : Integer;
    P : Pointer;
    Q : Pointer;
    V : Word32;
begin
  if BufSize <= 0 then
    exit;
  P := Buf;
  Q := DestBuf;
  for I := 1 to BufSize div 4 do
    begin
      // convert 4 characters per iteration
      V := PWord32(P)^;
      Inc(PWord32(P));
      PWord32(Q)^ := (V and $FF) or ((V and $FF00) shl 8);
      Inc(PWord32(Q));
      V := V shr 16;
      PWord32(Q)^ := (V and $FF) or ((V and $FF00) shl 8);
      Inc(PWord32(Q));
    end;
  // convert remaining (<4)
  for I := 1 to BufSize mod 4 do
    begin
      PWord(Q)^ := PByte(P)^;
      Inc(PByte(P));
      Inc(PWord(Q));
    end;
end;



{                                                                              }
{ Codec registration                                                           }
{                                                                              }
var
  CodecList : array of TUnicodeCodecClass;

procedure RegisterCodecs(const Codecs: array of TUnicodeCodecClass);
var I, K, L : Integer;
begin
  L := Length(CodecList);
  K := Length(Codecs);
  SetLength(CodecList, L + K);
  for I := 0 to K - 1 do
    CodecList[L + I] := Codecs[I];
end;

var
  DefaultCodecsRegistered : Boolean = False;

procedure RegisterDefaultCodecs;
begin
  if DefaultCodecsRegistered then
    exit;
  DefaultCodecsRegistered := True;
  RegisterCodecs([
      TUSASCIICodec,
      TUTF8Codec, TUTF16LECodec, TUTF16BECodec,
      TISO8859_1Codec, TISO8859_2Codec, TISO8859_3Codec, TISO8859_4Codec,
      TISO8859_5Codec, TISO8859_6Codec, TISO8859_7Codec, TISO8859_8Codec,
      TISO8859_9Codec, TISO8859_10Codec, TISO8859_13Codec, TISO8859_14Codec,
      TISO8859_15Codec,
      TWindows1250Codec, TWindows1251Codec, TWindows1252Codec,
      TMacLatin2Codec, TMacRomanCodec, TMacCyrillicCodec,
      TEBCDIC_USCodec, TKOI8_RCodec]);
end;



{                                                                              }
{ Codec aliases                                                                }
{                                                                              }
function GetCodecClassByAlias(const CodecAlias: String): TUnicodeCodecClass;
var I : Integer;
    C : TUnicodeCodecClass;
begin
  RegisterDefaultCodecs;
  for I := 0 to Length(CodecList) - 1 do
    begin
      C := CodecList[I];
      if C.IsAlias(CodecAlias) then
        begin
          Result := C;
          exit;
        end;
    end;
  Result := nil;
end;

function GetCodecClassByAliasA(const CodecAlias: RawByteString): TUnicodeCodecClass;
var S : String;
begin
  S := String(CodecAlias);
  Result := GetCodecClassByAlias(S);
end;

function GetCodecClassByAliasU(const CodecAlias: UnicodeString): TUnicodeCodecClass;
var S : String;
begin
  S := String(CodecAlias);
  Result := GetCodecClassByAlias(S);
end;



{$IFDEF MSWIN}
{                                                                              }
{ MS Windows system encoding functions                                         }
{                                                                              }
function GetSystemEncodingName: String;
begin
  // GetACP returns the current ANSI code-page identifier for the system,
  // or a default identifier if no code page is current.
  case GetACP of
    874  : Result := 'cp874';            // Thai
    932  : Result := 'cp932';            // Japan
    936  : Result := 'cp936';            // Chinese (PRC, Singapore)
    949  : Result := 'cp949';            // Korean
    950  : Result := 'cp950';            // Chinese (Taiwan, Hong Kong)
    1200 : Result := 'ISO-10646-UCS-2';  // Unicode (BMP of ISO 10646)
    1250 : Result := 'windows-1250';     // Windows 3.1 Eastern European
    1251 : Result := 'windows-1251';     // Windows 3.1 Cyrillic
    1252 : Result := 'windows-1252';     // Windows 3.1 Latin 1 (US, Western Europe)
    1253 : Result := 'windows-1253';     // Windows 3.1 Greek
    1254 : Result := 'windows-1254';     // Windows 3.1 Turkish
    1255 : Result := 'windows-1255';     // Hebrew
    1256 : Result := 'windows-1256';     // Arabic
    1257 : Result := 'windows-1257';     // Baltic
  else
    Result := '';
  end;
end;

function GetSystemEncodingCodecClass: TUnicodeCodecClass;
begin
  case GetACP of
    874  : Result := TWindows874Codec;   // Thai
    932  : Result := nil;                // Japan  --  Not supported
    936  : Result := nil;                // Chinese (PRC, Singapore) --  Not supported
    949  : Result := nil;                // Korean  --  Not supported
    950  : Result := nil;                // Chinese (Taiwan, Hong Kong)  --  Not supported
    1200 : Result := nil;                // Unicode (BMP of ISO 10646)  --  Not supported
    1250 : Result := TWindows1250Codec;  // Windows 3.1 Eastern European
    1251 : Result := TWindows1251Codec;  // Windows 3.1 Cyrillic
    1252 : Result := TWindows1252Codec;  // Windows 3.1 Latin 1 (US, Western Europe)
    1253 : Result := TWindows1253Codec;  // Windows 3.1 Greek
    1254 : Result := TWindows1254Codec;  // Windows 3.1 Turkish
    1255 : Result := TWindows1255Codec;  // Hebrew
    1256 : Result := TWindows1256Codec;  // Arabic
    1257 : Result := TWindows1257Codec;  // Baltic
  else
    Result := nil;
  end;
end;
{$ENDIF}



{                                                                              }
{ Encoding detection                                                           }
{                                                                              }
function DetectUTFEncoding(const Buf: Pointer; const BufSize: Integer;
    out BOMSize: Integer): TUnicodeCodecClass;
var R : Boolean;
begin
  if DetectUTF16BOM(Buf, BufSize, R) then
    begin
      BOMSize := UTF16BOMSize;
      if R then
        Result := TUTF16BECodec
      else
        Result := TUTF16LECodec
    end
  else
    if DetectUTF8BOM(Buf, BufSize) then
      begin
        BOMSize := UTF8BOMSize;
        Result := TUTF8Codec;
      end
    else
      begin
        BOMSize := 0;
        Result := nil;
      end;
end;



{                                                                              }
{ Unicode conversion functions                                                 }
{                                                                              }
function EncodingToUTF16U(const CodecClass: TUnicodeCodecClass;
    const Buf: Pointer; const BufSize: Integer): UnicodeString;
var C : TCustomUnicodeCodec;
begin
  if not Assigned(CodecClass) then
    begin
      Result := '';
      exit;
    end;
  C := CodecClass.Create;
  try
    C.DecodeStr(Buf, BufSize, Result);
  finally
    C.Free;
  end;
end;

function EncodingToUTF16U(const CodecClass: TUnicodeCodecClass;
    const S: RawByteString): UnicodeString;
var C : TCustomUnicodeCodec;
begin
  if not Assigned(CodecClass) then
    begin
      Result := '';
      exit;
    end;
  C := CodecClass.Create;
  try
    C.DecodeStr(PByteChar(S), Length(S), Result);
  finally
    C.Free;
  end;
end;

function EncodingToUTF16U(const CodecAlias: String;
    const Buf: Pointer; const BufSize: Integer): UnicodeString;
begin
  Result := EncodingToUTF16U(GetCodecClassByAlias(CodecAlias),
      Buf, BufSize);
end;

function EncodingToUTF16U(const CodecAlias: String; const S: RawByteString): UnicodeString;
begin
  Result := EncodingToUTF16U(GetCodecClassByAlias(CodecAlias), S);
end;

function UTF16ToEncodingU(const CodecClass: TUnicodeCodecClass;
    const S: UnicodeString): RawByteString;
var C : TCustomUnicodeCodec;
    I : Integer;
begin
  if not Assigned(CodecClass) then
    begin
      SetLength(Result, 0);
      exit;
    end;
  C := CodecClass.Create;
  try
    Result := C.Encode(Pointer(S), Length(S), I);
  finally
    C.Free;
  end;
end;

function UTF16ToEncodingU(const CodecAlias: String; const S: UnicodeString): RawByteString;
begin
  Result := UTF16ToEncodingU(GetCodecClassByAlias(CodecAlias), S);
end;



{                                                                              }
{ EUnicodeCodecException helper functions                                      }
{                                                                              }
procedure RaiseUnicodeCodecException(const Msg: String;
    const ProcessedBytes: Integer); overload;
var E : EUnicodeCodecException;
begin
  E := EUnicodeCodecException.Create(Msg);
  E.ProcessedBytes := ProcessedBytes;
  raise E;
end;

procedure RaiseUnicodeCodecException(const Msg: string; const Args: array of const;
    const ProcessedBytes: Integer); overload;
var E : EUnicodeCodecException;
begin
  E := EUnicodeCodecException.CreateFmt(Msg, Args);
  E.ProcessedBytes := ProcessedBytes;
  raise E;
end;



{                                                                              }
{ TCustomUnicodeCodec                                                          }
{                                                                              }
constructor TCustomUnicodeCodec.Create;
begin
  inherited Create;
  FDecodeReplaceChar := WideChar(#$FFFD);
  FErrorAction := eaException;
  FReadLFOption := lrPass;
  FWriteLFOption := lwLF;
  ResetReadAhead;
end;

constructor TCustomUnicodeCodec.CreateEx(const AErrorAction: TCodecErrorAction;
    const ADecodeReplaceChar: WideChar; const AReadLFOption: TCodecReadLFOption;
    const AWriteLFOption: TCodecWriteLFOption);
begin
  inherited Create;
  FErrorAction := AErrorAction;
  FDecodeReplaceChar := ADecodeReplaceChar;
  FReadLFOption := AReadLFOption;
  FWriteLFOption := AWriteLFOption;
  ResetReadAhead;
end;

class function TCustomUnicodeCodec.GetAliasCount: Integer;
begin
  Result := 0;
end;

class function TCustomUnicodeCodec.GetAlias(const Idx: Integer): String;
begin
  Result := ClassName;
end;

class function TCustomUnicodeCodec.IsAlias(const Alias: String): Boolean;
var I : Integer;
begin
  for I := 0 to GetAliasCount - 1 do
    if SameText(Alias, GetAlias(I)) then
      begin
        Result := True;
        exit;
      end;
  Result := False;
end;

{$IFDEF SupportAnsiString}
class function TCustomUnicodeCodec.IsAliasA(const Alias: AnsiString): Boolean;
var S : String;
begin
  S := String(Alias);
  Result := IsAlias(S);
end;
{$ENDIF}

class function TCustomUnicodeCodec.IsAliasU(const Alias: UnicodeString): Boolean;
var S : String;
begin
  S := String(Alias);
  Result := IsAlias(S);
end;

procedure TCustomUnicodeCodec.ResetReadAhead;
begin
  FReadAhead := False;
  FReadAheadBuffer := 0;
end;

procedure TCustomUnicodeCodec.SetDecodeReplaceChar(const Value: WideChar);
begin
  FDecodeReplaceChar := Value;
end;

procedure TCustomUnicodeCodec.SetErrorAction(const Value: TCodecErrorAction);
begin
  FErrorAction := Value;
end;

procedure TCustomUnicodeCodec.SetReadLFOption(const Value: TCodecReadLFOption);
begin
  FReadLFOption := Value;
end;

procedure TCustomUnicodeCodec.SetWriteLFOption(const Value: TCodecWriteLFOption);
begin
  FWriteLFOption := Value;
end;

procedure TCustomUnicodeCodec.SetOnRead(const Value: TCodecReadEvent);
begin
  if @Value <> @FOnRead then
    begin
      ResetReadAhead;
      FOnRead := Value;
    end;
end;

procedure TCustomUnicodeCodec.DecodeStr(const Buf: Pointer; const BufSize: Integer;
    var Dest: UnicodeString);
var P    : PByteChar;
    Q    : PWideChar;
    L, M : Integer;
    I, J : Integer;
begin
  P := Buf;
  L := BufSize;
  if not Assigned(P) or (L <= 0) then
    begin
      Dest := '';
      exit;
    end;
  SetLength(Dest, BufSize);
  M := 0;
  repeat
    Q := Pointer(Dest);
    Inc(Q, M);
    Decode(P, L, Q, BufSize * Sizeof(WideChar), I, J);
    Dec(L, I);
    Inc(P, I);
    Inc(M, J);
    if (J < BufSize) or (L <= 0) then
      break;
    SetLength(Dest, M + BufSize);
  until False;
  if Length(Dest) <> M then
    SetLength(Dest, M);
end;

function TCustomUnicodeCodec.EncodeStr(const S: UnicodeString): RawByteString;
var I : Integer;
begin
  Result := Encode(Pointer(S), Length(S), I);
end;

function TCustomUnicodeCodec.ReadBuffer(var Buf; Count: Integer): Boolean;
begin
  Result := False;
  if Assigned(FOnRead) then
    FOnRead(self, Buf, Count, Result);
end;

procedure TCustomUnicodeCodec.WriteBuffer(const Buf; Count: Integer);
begin
  if Assigned(FOnWrite) then
    FOnWrite(self, Buf, Count);
end;

const
  UCS4_STRING_TERMINATOR = $9C;
  UCS4_LF                = $0A;
  UCS4_CR                = $0D;

procedure TCustomUnicodeCodec.ReadUCS4Char(out C: UCS4Char;
    out ByteCount: Integer);
begin
  // Get UCS4 character from read-ahead buffer or from InternalReadUCS4Char
  if FReadAhead then
    begin
      C := FReadAheadBuffer;
      ByteCount := FReadAheadByteCount;
      FReadAhead := False;
    end
  else
    InternalReadUCS4Char(C, ByteCount);
  // Adjust line breaks to Linux-style breaks with a single LINE FEED character
  if (C = UCS4_CR) and (ReadLFOption = lrNormalize) then
    begin
      InternalReadUCS4Char(FReadAheadBuffer, FReadAheadByteCount);
      if FReadAheadBuffer = UCS4_LF then
        Inc(ByteCount, FReadAheadByteCount)
      else
        FReadAhead := True;
      C := UCS4_LF;
    end;
end;

procedure TCustomUnicodeCodec.WriteUCS4Char(const C: UCS4Char;
    out ByteCount: Integer);
var ByteCount2 : Integer;
begin
  if C = UCS4_LF then
    // Transform LINE FEED character
    case WriteLFOption of
      lwLF : InternalWriteUCS4Char(UCS4_LF, ByteCount);
      lwCR : InternalWriteUCS4Char(UCS4_CR, ByteCount);
      lwCRLF :
        begin
          InternalWriteUCS4Char(UCS4_CR, ByteCount);
          InternalWriteUCS4Char(UCS4_LF, ByteCount2);
          Inc(ByteCount, ByteCount2);
        end;
    end
  else
    InternalWriteUCS4Char(C, ByteCount);
end;



{                                                                              }
{ TCustomSingleByteCodec                                                       }
{                                                                              }
constructor TCustomSingleByteCodec.Create;
begin
  inherited Create;
  FEncodeReplaceChar := AnsiChar(#32);
end;

constructor TCustomSingleByteCodec.CreateEx(const ErrorAction: TCodecErrorAction;
    const DecodeReplaceChar: WideChar; const EncodeReplaceChar: AnsiChar);
begin
  inherited CreateEx(ErrorAction, DecodeReplaceChar);
  FEncodeReplaceChar := EncodeReplaceChar;
end;

procedure TCustomSingleByteCodec.Decode(const Buf: Pointer; const BufSize: Integer;
    const DestBuf: Pointer; const DestSize: Integer;
    out ProcessedBytes, DestLength: Integer);
var P       : PByteChar;
    Q       : PWideChar;
    I, L, C : Integer;
begin
  P := Buf;
  Q := DestBuf;
  C := DestSize div Sizeof(WideChar);
  if not Assigned(P) or (BufSize <= 0) or not Assigned(Q) or (C <= 0) then
    begin
      ProcessedBytes := 0;
      DestLength := 0;
      exit;
    end;
  L := 0;
  for I := 1 to BufSize do
    try
      if L >= C then
        break;
      Q^ := DecodeChar(P^);
      Inc(P);
      Inc(Q);
      Inc(L);
    except
      on E : Exception do
        case FErrorAction of
          eaException :
            RaiseUnicodeCodecException(E.Message, NativeUInt(P) - NativeUInt(Buf));
          eaStop :
            break;
          eaSkip :
            Inc(P);
          eaIgnore :
            begin
              Q^ := WideChar(P^);
              Inc(P);
              Inc(Q);
              Inc(L);
            end;
          eaReplace :
            begin
              Q^ := FDecodeReplaceChar;
              Inc(P);
              Inc(Q);
              Inc(L);
            end;
        end;
    end;
  DestLength := L;
  ProcessedBytes := NativeUInt(P) - NativeUInt(Buf);
end;

function TCustomSingleByteCodec.DecodeUCS4Char(const P: AnsiChar): UCS4Char;
begin
  Result := Ord(DecodeChar(P));
end;

function TCustomSingleByteCodec.Encode(const S: PWideChar; const Length: Integer;
    out ProcessedChars: Integer): RawByteString;
var P       : PByteChar;
    Q       : PWideChar;
    I, L, M : Integer;
begin
  Q := S;
  if not Assigned(Q) or (Length <= 0) then
    begin
      ProcessedChars := 0;
      SetLength(Result, 0);
      exit;
    end;
  SetLength(Result, Length);
  L := 0;
  M := 0;
  P := Pointer(Result);
  for I := 1 to Length do
    try
      P^ := EncodeChar(Q^);
      Inc(P);
      Inc(Q);
      Inc(L);
      Inc(M);
    except
      on E : Exception do
        case FErrorAction of
          eaException :
            RaiseUnicodeCodecException(E.Message, L);
          eaStop :
            break;
          eaSkip :
            begin
              Inc(Q);
              Inc(L);
            end;
          eaIgnore :
            begin
              P^ := AnsiChar(Q^);
              Inc(P);
              Inc(Q);
              Inc(L);
              Inc(M);
            end;
          eaReplace :
            begin
              P^ := FEncodeReplaceChar;
              Inc(P);
              Inc(Q);
              Inc(L);
              Inc(M);
            end;
        end;
    end;
  if Length <> M then
    SetLength(Result, M);
  ProcessedChars := L;
end;

function TCustomSingleByteCodec.EncodeUCS4Char(const Ch: UCS4Char): AnsiChar;
begin
  if Ch < $10000 then
    Result := EncodeChar(WideChar(Ch))
  else
    raise EConvertError.CreateFmt(SInvalidCodePoint, [Ch, '']);
end;

procedure TCustomSingleByteCodec.InternalReadUCS4Char(out C: UCS4Char;
    out ByteCount: Integer);
var B : AnsiChar;
begin
  if ReadBuffer(B, 1) then
    begin
      C := Ord(DecodeChar(B));
      ByteCount := 1;
    end
  else
    begin
      C := UCS4_STRING_TERMINATOR;
      ByteCount := 0;
    end;
end;

procedure TCustomSingleByteCodec.InternalWriteUCS4Char(const C: UCS4Char;
    out ByteCount: Integer);
var E : AnsiChar;
begin
  E := EncodeUCS4Char(C);
  WriteBuffer(E, 1);
  ByteCount := 1;
end;



{                                                                              }
{ UTF-8                                                                        }
{                                                                              }
const
  UTF8Aliases = 2;
  UTF8Alias : array[0..UTF8Aliases - 1] of String = (
      'UTF-8', 'utf8');

class function TUTF8Codec.GetAliasCount: Integer;
begin
  Result := UTF8Aliases;
end;

class function TUTF8Codec.GetAlias(const Idx: Integer): String;
begin
  if Idx < 0 then
    raise EUnicodeCodecException.Create(SAliasIndexOutOfRange);
  if Idx >= GetAliasCount then
    raise EUnicodeCodecException.Create(SAliasIndexOutOfRange);
  Result := UTF8Alias[Idx];
end;

procedure TUTF8Codec.Decode(const Buf: Pointer; const BufSize: Integer;
    const DestBuf: Pointer; const DestSize: Integer;
    out ProcessedBytes, DestLength: Integer);
var P    : PByteChar;
    Q    : PWideChar;
    L, I : Integer;
    M, N : Integer;
    R    : TUTF8Error;
    C    : WideChar;
begin
  P := Buf;
  L := BufSize;
  Q := DestBuf;
  N := DestSize div Sizeof(WideChar);
  if not Assigned(P) or (L <= 0) or not Assigned(Q) or (N <= 0) then
    begin
      ProcessedBytes := 0;
      DestLength := 0;
      exit;
    end;
  M := 0;
  repeat
    if M >= N then
      break;
    try
      R := UTF8ToWideChar(P, L, I, C);
      case R of
        UTF8ErrorNone :
          begin
            Q^ := C;
            Inc(Q);
            Inc(M);
            Inc(P, I);
            Dec(L, I);
          end;
        UTF8ErrorInvalidEncoding :
          raise EConvertError.CreateFmt(SInvalidEncoding, ['UTF-8']);
        UTF8ErrorIncompleteEncoding :
          begin
            ProcessedBytes := BufSize - L;
            DestLength := M;
            exit;
          end;
        UTF8ErrorOutOfRange :
          raise EConvertError.CreateFmt(SEncodingOutOfRange, ['UTF-8']);
      else
        raise EConvertError.CreateFmt(SUTF8Error, [Ord(R)]);
      end;
    except
      on E : Exception do
        case FErrorAction of
          eaException :
            RaiseUnicodeCodecException(E.Message, BufSize - L);
          eaStop :
            break;
          eaSkip :
            begin
              Inc(P, I);
              Dec(L, I);
            end;
          eaIgnore :
            begin
              Q^ := C;
              Inc(Q);
              Inc(M);
              Inc(P, I);
              Dec(L, I);
            end;
          eaReplace :
            begin
              Q^ := FDecodeReplaceChar;
              Inc(Q);
              Inc(M);
              Inc(P, I);
              Dec(L, I);
            end;
        end;
    end;
  until L <= 0;
  ProcessedBytes := BufSize - L;
  DestLength := M;
end;

function TUTF8Codec.Encode(const S: PWideChar; const Length: Integer;
    out ProcessedChars: Integer): RawByteString;
var P     : PWideChar;
    Q     : PByteChar;
    I, L,
    M, J  : Integer;
begin
  P := S;
  if not Assigned(P) or (Length <= 0) then
    begin
      ProcessedChars := 0;
      SetLength(Result, 0);
      exit;
    end;
  L := Length * 3;
  SetLength(Result, L);
  Q := Pointer(Result);
  M := 0;
  for I := 1 to Length do
    begin
      WideCharToUTF8(P^, Q, L, J);
      Inc(P);
      Inc(Q, J);
      Dec(L, J);
      Inc(M, J);
    end;
  SetLength(Result, M);
  ProcessedChars := Length;
end;

procedure TUTF8Codec.InternalReadUCS4Char(out C: UCS4Char;
    out ByteCount: Integer);
const
  MaxCode: array[1..6] of LongWord =
      ($7F, $7FF, $FFFF, $1FFFFF, $3FFFFFF, $7FFFFFFF);
var B, First, Mask: Byte;
begin
  if not ReadBuffer(B, 1) then
    begin
      C := UCS4_STRING_TERMINATOR;
      ByteCount := 0;
      exit;
    end;
  C := B;
  ByteCount := 1;
  if C >= $80 then
    begin // UTF-8 sequence
      First := B;
      Mask := $40;
      if (B and $C0 <> $C0) then
        raise EConvertError.CreateFmt(SInvalidEncoding, ['UTF-8']);
      while (Mask and First <> 0) do
        begin
          if not ReadBuffer(B, 1) then
            raise EConvertError.CreateFmt(SInvalidEncoding, ['UTF-8']);
          if (B and $C0) <> $80 then
            raise EConvertError.CreateFmt(SInvalidEncoding, ['UTF-8']);
          C := (C shl 6) or (B and $3F);  // Add bits to C
          Inc(ByteCount);                 // Increase sequence length
          Mask := Mask shr 1;             // Adjust Mask
        end;
      if ByteCount > 6 then  // No 0 bit in sequence header 'First'
        raise EConvertError.CreateFmt(SInvalidEncoding, ['UTF-8']);
      C := C and MaxCode[ByteCount];	// dispose of header bits
      // Check for invalid sequence as suggested by RFC2279
      if ((ByteCount > 1) and (C <= MaxCode[ByteCount - 1])) then
        raise EConvertError.CreateFmt(SInvalidEncoding, ['UTF-8']);
    end;
end;

procedure TUTF8Codec.InternalWriteUCS4Char(const C: UCS4Char;
    out ByteCount: Integer);
var Buffer : array[0..3] of Byte;
begin
  UCS4CharToUTF8(C, @Buffer, 4, ByteCount);
  WriteBuffer(Buffer, ByteCount);
end;

procedure TUTF8Codec.WriteUCS4Char(const C: UCS4Char; out ByteCount: Integer);
const
  UTF8_LF   : Byte = $0A;
  UTF8_CR   : Byte = $0D;
  UTF8_CRLF : array[0..1] of Byte = ($0D, $0A);
begin
  if C = UCS4_LF then
    case WriteLFOption of
      lwLF:
        begin
          WriteBuffer(UTF8_LF, 1);
          ByteCount := 1;
        end;
      lwCR:
        begin
          WriteBuffer(UTF8_CR, 1);
          ByteCount := 1;
        end;
      lwCRLF:
        begin
          WriteBuffer(UTF8_CRLF, 2);
          ByteCount := 2;
        end;
    end
  else
    InternalWriteUCS4Char(C, ByteCount);
end;



{                                                                              }
{ UTF-16BE                                                                     }
{                                                                              }
const
  UTF16BEAliases = 2;
  UTF16BEAlias : array[0..UTF16BEAliases - 1] of String = (
      'UTF-16BE', 'UTF16be');

class function TUTF16BECodec.GetAliasCount: Integer;
begin
  Result := UTF16BEAliases;
end;

class function TUTF16BECodec.GetAlias(const Idx: Integer): String;
begin
  if Idx < 0 then
    raise EUnicodeCodecException.Create(SAliasIndexOutOfRange);
  if Idx >= GetAliasCount then
    raise EUnicodeCodecException.Create(SAliasIndexOutOfRange);
  Result := UTF16BEAlias[Idx];
end;

procedure TUTF16BECodec.Decode(const Buf: Pointer; const BufSize: Integer;
    const DestBuf: Pointer; const DestSize: Integer;
    out ProcessedBytes, DestLength: Integer);
var
  L, M : Integer;
  I    : Integer;
  P, Q : PWideChar;
begin
  L := BufSize;
  if L > DestSize then
    L := DestSize;
  if L <= 1 then
    begin
      ProcessedBytes := 0;
      DestLength := 0;
      exit;
    end;
  Dec(L, L mod Sizeof(WideChar));
  M := L div Sizeof(WideChar);
  P := Buf;
  Q := DestBuf;
  for I := 1 to M do
    begin
      Q^ := SwapUTF16Endian(P^);
      Inc(P);
      Inc(Q);
    end;
  DestLength := M;
  ProcessedBytes := L;
end;

function TUTF16BECodec.Encode(const S: PWideChar; const Length: Integer;
    out ProcessedChars: Integer): RawByteString;
var I, L : Integer;
    P, Q : PWideChar;
begin
  if Length <= 0 then
    begin
      ProcessedChars := 0;
      SetLength(Result, 0);
      exit;
    end;
  L := Length * 2;
  SetLength(Result, L);
  P := S;
  Q := Pointer(Result);
  for I := 1 to Length do
    begin
      Q^ := SwapUTF16Endian(P^);
      Inc(P);
      Inc(Q);
    end;
  ProcessedChars := Length;
end;

procedure TUTF16BECodec.InternalReadUCS4Char(out C: UCS4Char;
    out ByteCount: Integer);
var LowSurrogate: array[0..1] of Byte; // We do not use Word, because the byte order of a Word is CPU dependant
begin
  C := 0;
  // C must be initialized, because the ReadBuffer(C, 2) call below does
  // not fill the whole variable!
  if not ReadBuffer(C, 2) then
    begin
      C := UCS4_STRING_TERMINATOR;
      ByteCount := 0;
      Exit;
    end;
  C := System.Swap(C); // UCS4Chars are stored in Little Endian mode; so we need to swap the bytes.
  case C of
    $D800..$DBFF: // High surrogate of Unicode character [$10000..$10FFFF]
      begin
        if not ReadBuffer(LowSurrogate, 2) then
          raise EConvertError.CreateFmt(SInvalidEncoding, ['UTF-16BE']);
        case LowSurrogate[0] of
          $DC..$DF:
          begin
            C := ((C - $D7C0) shl 10) + ((LowSurrogate[0] xor $DC) shl 8) + LowSurrogate[1];
            ByteCount := 4;
          end;
        else
          raise EConvertError.CreateFmt(SInvalidEncoding, ['UTF-16BE']);
        end;
      end;
    $DC00..$DFFF: // Low surrogate of Unicode character [$10000..$10FFFF]
      raise EConvertError.CreateFmt(SInvalidEncoding, ['UTF-16BE']);
    else
      ByteCount := 2;
  end;
end;

procedure TUTF16BECodec.InternalWriteUCS4Char(const C: UCS4Char;
    out ByteCount: Integer);
var Buffer : array[0..3] of Byte;
begin
  UCS4CharToUTF16BE(C, @Buffer, 4, ByteCount);
  WriteBuffer(Buffer[0], ByteCount);
end;

procedure TUTF16BECodec.WriteUCS4Char(const C: UCS4Char;
    out ByteCount: Integer);
const
  UTF16BE_LF   : array[0..1] of Byte = ($00, $0A);
  UTF16BE_CR   : array[0..1] of Byte = ($00, $0D);
  UTF16BE_CRLF : array[0..3] of Byte = ($00, $0D, $00, $0A);
begin
  if C = UCS4_LF then
    case WriteLFOption of
      lwLF:
        begin
          WriteBuffer(UTF16BE_LF, 2);
          ByteCount := 2;
        end;
      lwCR:
        begin
          WriteBuffer(UTF16BE_CR, 2);
          ByteCount := 2;
        end;
      lwCRLF:
        begin
          WriteBuffer(UTF16BE_CRLF, 4);
          ByteCount := 4;
        end;
    end
  else
    InternalWriteUCS4Char(C, ByteCount);
end;



{                                                                              }
{ UTF-16LE                                                                     }
{                                                                              }
const
  UTF16LEAliases = 2;
  UTF16LEAlias : array[0..UTF16LEAliases - 1] of String = (
      'UTF-16LE', 'utf16le');

class function TUTF16LECodec.GetAliasCount: Integer;
begin
  Result := UTF16LEAliases;
end;

class function TUTF16LECodec.GetAlias(const Idx: Integer): String;
begin
  if Idx < 0 then
    raise EUnicodeCodecException.Create(SAliasIndexOutOfRange);
  if Idx >= GetAliasCount then
    raise EUnicodeCodecException.Create(SAliasIndexOutOfRange);
  Result := UTF16LEAlias[Idx];
end;

procedure TUTF16LECodec.Decode(const Buf: Pointer; const BufSize: Integer;
  const DestBuf: Pointer; const DestSize: Integer; out ProcessedBytes, DestLength: Integer);
var L, M : Integer;
    P, Q : PWideChar;
begin
  L := BufSize;
  if L > DestSize then
    L := DestSize;
  if L <= 1 then
    begin
      ProcessedBytes := 0;
      DestLength := 0;
      exit;
    end;
  Dec(L, L mod Sizeof(WideChar));
  M := L div Sizeof(WideChar);
  P := Buf;
  Q := DestBuf;
  Move(P^, Q^, L);
  DestLength := M;
  ProcessedBytes := L;
end;

function TUTF16LECodec.Encode(const S: PWideChar; const Length: Integer;
    out ProcessedChars: Integer): RawByteString;
var L : Integer;
begin
  if Length <= 0 then
    begin
      ProcessedChars := 0;
      SetLength(Result, 0);
      exit;
    end;
  L := Length * 2;
  SetLength(Result, L);
  Move(S^, Pointer(Result)^, L);
  ProcessedChars := Length;
end;

procedure TUTF16LECodec.InternalReadUCS4Char(out C: UCS4Char;
    out ByteCount: Integer);
var LowSurrogate : array[0..1] of Byte; // We do not use Word, because the byte order of a Word is CPU dependant
begin
  C := 0;
  // C must be initialized, because the ReadBuffer(C, 2) call below does
  // not fill the whole variable!
  if not ReadBuffer(C, 2) then
    begin
      C := UCS4_STRING_TERMINATOR;
      ByteCount := 0;
      Exit;
    end;
  case C of  // UCS4Chars are stored in Little Endian mode; so we just can go on with it.
    $D800..$DBFF: // High surrogate of Unicode character [$10000..$10FFFF]
      begin
        if not ReadBuffer(LowSurrogate, 2) then
          raise EConvertError.CreateFmt(SInvalidEncoding, ['UTF-16LE']);
        case LowSurrogate[1] of
          $DC..$DF:
          begin
            C := ((C - $D7C0) shl 10) + ((LowSurrogate[1] xor $DC) shl 8) + LowSurrogate[0];
            ByteCount := 4;
          end;
        else
          raise EConvertError.CreateFmt(SInvalidEncoding, ['UTF-16LE']);
        end;
      end;
    $DC00..$DFFF: // Low surrogate of Unicode character [$10000..$10FFFF]
      raise EConvertError.CreateFmt(SInvalidEncoding, ['UTF-16LE']);
    else
      ByteCount := 2;
  end;
end;

procedure TUTF16LECodec.InternalWriteUCS4Char(const C: UCS4Char;
    out ByteCount: Integer);
var Buffer : array[0..3] of Byte;
begin
  UCS4CharToUTF16LE(C, @Buffer, 4, ByteCount);
  WriteBuffer(Buffer[0], ByteCount);
end;

procedure TUTF16LECodec.WriteUCS4Char(const C: UCS4Char;
    out ByteCount: Integer);
const
  UTF16LE_LF   : array[0..1] of Byte = ($0A, $00);
  UTF16LE_CR   : array[0..1] of Byte = ($0D, $00);
  UTF16LE_CRLF : array[0..3] of Byte = ($0D, $00, $0A, $00);
begin
  if C = UCS4_LF then
    case WriteLFOption of
      lwLF:
        begin
          WriteBuffer(UTF16LE_LF, 2);
          ByteCount := 2;
        end;
      lwCR:
        begin
          WriteBuffer(UTF16LE_CR, 2);
          ByteCount := 2;
        end;
      lwCRLF:
        begin
          WriteBuffer(UTF16LE_CRLF, 4);
          ByteCount := 4;
        end;
    end
  else
    InternalWriteUCS4Char(C, ByteCount);
end;



{                                                                              }
{ TUCS4BECodec                                                                 }
{                                                                              }
procedure TUCS4BECodec.Decode(const Buf: Pointer; const BufSize: Integer;
  const DestBuf: Pointer; const DestSize: Integer;
  out ProcessedBytes, DestLength: Integer);
var Ch4     : UCS4Char;
    N, P, Q : PByteChar;
    T, U, V : PByte;
    L, C    : Integer;
begin
  P := Buf;
  Q := DestBuf;
  C := DestSize div 2;
  if not Assigned(P) or (BufSize <= 0) or not Assigned(Q) or (C <= 0) then
    begin
      ProcessedBytes := 0;
      DestLength := 0;
      exit;
    end;
  L := 0;
  N := Pointer(NativeUInt(P) + NativeUInt(BufSize) - 4);
  while NativeUInt(P) <= NativeUInt(N) do
    begin
      T := Pointer(P);
      Ch4 := T^ * $1000000;
      Inc(T);
      Inc(Ch4, T^ * $10000);
      Inc(T);
      Inc(Ch4, T^ * $100);
      Inc(T);
      Inc(Ch4, T^);
    if ((Ch4 >= $D800) and (Ch4 < $E000)) or (Ch4 > $10FFFF) then
      case FErrorAction of
        eaException :
          if Ch4 > $10FFFF then
            RaiseUnicodeCodecException(SCannotConvertUCS4, [Ch4, 'UTF-16'], NativeUInt(P) - NativeUInt(Buf))
          else
            RaiseUnicodeCodecException(SSurrogateNotAllowed, [Ch4, 'UCS-4'], NativeUInt(P) - NativeUInt(Buf));
        eaStop :
          break;
        eaSkip :
          Inc(P, 4);
        eaIgnore :
          begin
            if L + 1 >= C then
              break;
            T := Pointer(Q);
            U := Pointer(P);
            Inc(U);
            T^ := U^;     // Nevertheless change Big Endian to Little Endian ...
            Inc(T);
            T^ := Byte(P^);
            Inc(T);
            U := Pointer(P);
            Inc(U, 3);
            T^ := U^;
            Inc(T);
            U := Pointer(P);
            Inc(U, 2);
            T^ := U^;
            Inc(Q, 4);
            Inc(P, 4);
            Inc(L, 2);
          end;
        eaReplace :
          begin
            if L >= C then
              break;
            T := Pointer(Q);
            T^ := Lo(Ord(FDecodeReplaceChar));
            Inc(T);
            T^ := Hi(Ord(FDecodeReplaceChar));
            Inc(Q, 2);
            Inc(P, 4);
            Inc(L);
          end;
      end
    else
      begin
        if Ch4 > $FFFF then
          begin
            if L + 1 >= C then
              break;
            T := Pointer(Q);
            U := Pointer(P);
            Inc(U);
            V := Pointer(P);
            Inc(V, 2);
            T^ := (U^ shl 6) + (V^ shr 2);
            Inc(T);
            U := Pointer(P);
            Inc(U);
            T^ := $D8 + (U^ shr 2);
            Inc(T);
            V := Pointer(P);
            Inc(V, 3);
            T^ := V^;
            Inc(T);
            U := Pointer(P);
            Inc(U, 2);
            T^ := $DC + (3 and U^);
            Inc(Q, 4);
            Inc(P, 4);
            Inc(L, 2);
          end
        else
          begin
            if L >= C then
              break;
            T := Pointer(Q);
            U := Pointer(P);
            Inc(U, 3);
            T^ := U^;
            Inc(T);
            U := Pointer(P);
            Inc(U, 2);
            T^ := U^;
            Inc(Q, 2);
            Inc(P, 4);
            Inc(L);
          end;
      end;
  end;
  DestLength := L;
  ProcessedBytes := NativeUInt(P) - NativeUInt(Buf);
end;

function TUCS4BECodec.Encode(const S: PWideChar; const Length: Integer;
  out ProcessedChars: Integer): RawByteString;
var P, N          : PWideChar;
    Q             : PByteChar;
    M             : Integer;
    HighSurrogate : Word;
    LowSurrogate  : Word;
begin
  P := S;
  if not Assigned(P) or (Length <= 0) then
    begin
      ProcessedChars := 0;
      SetLength(Result, 0);
      exit;
    end;
  SetLength(Result, Length * 4);
  Q := Pointer(Result);
  M := 0;
  N := P + Length;
  while P < N do
    case Ord(P^) of
      $D800..$DBFF: // High surrogate of Unicode character [$10000..$10FFFF]
        begin
          if P = N - 1 then // End of UnicodeString?
            raise EConvertError.Create(SLowSurrogateNotFound);
          HighSurrogate := Ord(P^);
          Inc(P);
          Inc(M, 2);
          LowSurrogate := Ord(P^);
          case LowSurrogate of  // Low Surrogate following?
            $DC00..$DF00:
              begin
                Q^ := AnsiChar(0);
                Inc(Q);
                Q^ := AnsiChar((HighSurrogate - $D7C0) shr 6);
                Inc(Q);
                Q^ := AnsiChar(((HighSurrogate and $3F) shl 2) + ((LowSurrogate - $DC00) shr 8));
                Inc(Q);
                Q^ := AnsiChar(Lo(LowSurrogate));
                Inc(Q);
                Inc(P);
                Inc(M, 4);
              end;
          else
            raise EConvertError.Create(SLowSurrogateNotFound);
          end;
        end;
      $DC00..$DFFF: // Low surrogate of Unicode character [$10000..$10FFFF]
        raise EConvertError.Create(SHighSurrogateNotFound);
    else
      Q^ := AnsiChar(0);
      Inc(Q);
      Q^ := AnsiChar(0);
      Inc(Q);
      Q^ := AnsiChar(Hi(Ord(P^)));
      Inc(Q);
      Q^ := AnsiChar(Lo(Ord(P^)));
      Inc(Q);
      Inc(P);
      Inc(M, 4);
    end;
  SetLength(Result, M);
  ProcessedChars := Length;
end;

procedure TUCS4BECodec.InternalReadUCS4Char(out C: UCS4Char;
    out ByteCount: Integer);
var B : array[0..3] of Byte;
begin
  if ReadBuffer(B, 4) then
    begin
      C := B[0] * $1000000 + B[1] * $10000 + B[2] * $100 + B[3];
      case C of
        $D800..$DFFF: // Do not accept surrogates
          raise EConvertError.CreateFmt(SSurrogateNotAllowed, [C, 'UCS-4BE']);
      end;
      ByteCount := 4;
    end
  else
    begin
      C := UCS4_STRING_TERMINATOR;
      ByteCount := 0;
    end;
end;

procedure TUCS4BECodec.InternalWriteUCS4Char(const C: UCS4Char;
    out ByteCount: Integer);
var Buffer : array[0..3] of Byte;
begin
  Buffer[0] := C shr 24;
  Buffer[1] := (C and $FF0000) shr 16;
  Buffer[2] := (C and $FF00) shr 8;
  Buffer[3] := C and $FF;
  WriteBuffer(Buffer, 4);
  ByteCount := 4;
end;

procedure TUCS4BECodec.WriteUCS4Char(const C: UCS4Char; out ByteCount: Integer);
const
  UCS4BE_LF   : array[0..3] of Byte = ($0A, $00, $00, $00);
  UCS4BE_CR   : array[0..3] of Byte = ($0D, $00, $00, $00);
  UCS4BE_CRLF : array[0..7] of Byte = ($0D, $00, $00, $00, $0A, $00, $00, $00);
begin
  if C = UCS4_LF then
    case WriteLFOption of
      lwLF:
        begin
          WriteBuffer(UCS4BE_LF, 4);
          ByteCount := 4;
        end;
      lwCR:
        begin
          WriteBuffer(UCS4BE_CR, 4);
          ByteCount := 4;
        end;
      lwCRLF:
        begin
          WriteBuffer(UCS4BE_CRLF, 8);
          ByteCount := 8;
        end;
    end
  else
    InternalWriteUCS4Char(C, ByteCount);
end;



{                                                                              }
{ TUCS4LECodec                                                                 }
{                                                                              }
procedure TUCS4LECodec.Decode(const Buf: Pointer; const BufSize: Integer;
    const DestBuf: Pointer; const DestSize: Integer;
    out ProcessedBytes, DestLength: Integer);
var Ch4     : UCS4Char;
    N, P, Q : PByteChar;
    T, U, V : PByte;
    L, C    : Integer;
begin
  P := Buf;
  Q := DestBuf;
  C := DestSize div 2;
  if not Assigned(P) or (BufSize <= 0) or not Assigned(Q) or (C <= 0) then
    begin
      ProcessedBytes := 0;
      DestLength := 0;
      exit;
    end;
  L := 0;
  N := Pointer(NativeUInt(P) + NativeUInt(BufSize) - 4);
  while NativeUInt(P) <= NativeUInt(N) do
    begin
      T := Pointer(P);
      Inc(T, 3);
      Ch4 := T^ * $1000000;
      Dec(T);
      Inc(Ch4, T^ * $10000);
      Dec(T);
      Inc(Ch4, T^ * $100);
      Dec(T);
      Inc(Ch4, T^);
      if ((Ch4 >= $D800) and (Ch4 < $E000)) or (Ch4 > $10FFFF) then
        case FErrorAction of
          eaException :
            if Ch4 > $10FFFF then
              RaiseUnicodeCodecException(SCannotConvertUCS4, [Ch4, 'UTF-16'], NativeUInt(P) - NativeUInt(Buf))
            else
              RaiseUnicodeCodecException(SSurrogateNotAllowed, [Ch4, 'UCS-4'], NativeUInt(P) - NativeUInt(Buf));
          eaStop :
            break;
          eaSkip :
            Inc(P, 4);
          eaIgnore :
            begin
              if L + 1 >= C then
                break;
              T := Pointer(Q);
              U := Pointer(P);
              T^ := U^;
              Inc(T);
              Inc(U);
              T^ := U^;
              Inc(T);
              Inc(U);
              T^ := U^;
              Inc(T);
              Inc(U);
              T^ := U^;
              Inc(Q, 4);
              Inc(P, 4);
              Inc(L, 2);
            end;
          eaReplace :
            begin
              if L >= C then
                break;
              T := Pointer(Q);
              T^ := Lo(Ord(FDecodeReplaceChar));
              Inc(T);
              T^ := Hi(Ord(FDecodeReplaceChar));
              Inc(Q, 2);
              Inc(P, 4);
              Inc(L);
            end;
        end
      else
        if Ch4 > $FFFF then
          begin
            if L + 1 >= C then
              break;
            T := Pointer(Q);
            U := Pointer(P);
            Inc(U, 2);
            V := Pointer(P);
            Inc(V, 1);
            T^ := (U^ shl 6) + (V^ shr 2);
            Inc(T);
            U := Pointer(P);
            Inc(U, 2);
            T^ := $D8 + (U^ shr 2);
            Inc(T);
            T^ := Byte(P^);
            Inc(T);
            U := Pointer(P);
            Inc(U, 1);
            T^ := $DC + (3 and U^);
            Inc(Q, 4);
            Inc(P, 4);
            Inc(L, 2);
          end
        else
          begin
            if L >= C then
              break;
            T := Pointer(Q);
            Q := Pointer(P);
            T^ := Byte(Q^);
            Inc(T);
            Inc(Q);
            T^ := Byte(Q^);
            Inc(Q, 2);
            Inc(P, 4);
            Inc(L);
          end;
    end;
  DestLength := L;
  ProcessedBytes := NativeUInt(P) - NativeUInt(Buf);
end;

function TUCS4LECodec.Encode(const S: PWideChar; const Length: Integer;
    out ProcessedChars: Integer): RawByteString;
var P, N          : PWideChar;
    Q             : PByteChar;
    M             : Integer;
    HighSurrogate : Word;
    LowSurrogate  : Word;
begin
  P := S;
  if not Assigned(P) or (Length <= 0) then
    begin
      ProcessedChars := 0;
      SetLength(Result, 0);
      exit;
    end;
  SetLength(Result, Length * 4);
  Q := Pointer(Result);
  M := 0;
  N := P + Length;
  while P < N do
    case Ord(P^) of
      $D800..$DBFF: // High surrogate of Unicode character [$10000..$10FFFF]
        begin
          if P = N - 1 then // End of UnicodeString?
            raise EConvertError.Create(SLowSurrogateNotFound);
          HighSurrogate := Ord(P^);
          Inc(P);
          Inc(M, 2);
          LowSurrogate := Ord(P^);
          case LowSurrogate of  // Low Surrogate following?
            $DC00..$DF00:
              begin
                Q^ := AnsiChar(Lo(LowSurrogate));
                Inc(Q);
                Q^ := AnsiChar(((HighSurrogate and $3F) shl 2) + ((LowSurrogate - $DC00) shr 8));
                Inc(Q);
                Q^ := AnsiChar((HighSurrogate - $D7C0) shr 6);
                Inc(Q);
                Q^ := AnsiChar(0);
                Inc(Q);
                Inc(P);
                Inc(M, 4);
              end;
          else
            raise EConvertError.Create(SLowSurrogateNotFound);
          end;
        end;
      $DC00..$DFFF: // Low surrogate of Unicode character [$10000..$10FFFF]
        raise EConvertError.Create(SHighSurrogateNotFound);
    else
      Q^ := AnsiChar(0);
      Inc(Q);
      Q^ := AnsiChar(0);
      Inc(Q);
      Q^ := AnsiChar(Hi(Ord(P^)));
      Inc(Q);
      Q^ := AnsiChar(Lo(Ord(P^)));
      Inc(Q);
      Inc(P);
      Inc(Q, 4);
      Inc(M, 4);
    end;
  SetLength(Result, M);
  ProcessedChars := Length;
end;

procedure TUCS4LECodec.InternalReadUCS4Char(out C: UCS4Char;
    out ByteCount: Integer);
var B : array[0..3] of Byte;
begin
  if ReadBuffer(B, 4) then
    begin
      C := B[3] * $1000000 + B[2] * $10000 + B[1] * $100 + B[0];
      case C of
        $D800..$DFFF: // Do not accept surrogates
          raise EConvertError.CreateFmt(SSurrogateNotAllowed, [C, 'UCS-4LE']);
      end;
      ByteCount := 4;
    end
  else
    begin
      C := UCS4_STRING_TERMINATOR;
      ByteCount := 0;
    end;
end;

procedure TUCS4LECodec.InternalWriteUCS4Char(const C: UCS4Char;
    out ByteCount: Integer);
var Buffer : array[0..3] of Byte;
begin
  Buffer[0] := C and $FF;
  Buffer[1] := (C and $FF00) shr 8;
  Buffer[2] := (C and $FF0000) shr 16;
  Buffer[3] := C shr 24;
  WriteBuffer(Buffer, 4);
  ByteCount := 4;
end;

procedure TUCS4LECodec.WriteUCS4Char(const C: UCS4Char; out ByteCount: Integer);
const
  UCS4LE_LF   : array[0..3] of Byte = ($00, $00, $00, $0A);
  UCS4LE_CR   : array[0..3] of Byte = ($00, $00, $00, $0D);
  UCS4LE_CRLF : array[0..7] of Byte = ($00, $00, $00, $0D, $00, $00, $00, $0A);
begin
  if C = UCS4_LF then
    case WriteLFOption of
      lwLF:
        begin
          WriteBuffer(UCS4LE_LF, 4);
          ByteCount := 4;
        end;
      lwCR:
        begin
          WriteBuffer(UCS4LE_CR, 4);
          ByteCount := 4;
        end;
      lwCRLF:
        begin
          WriteBuffer(UCS4LE_CRLF, 8);
          ByteCount := 8;
        end;
    end
  else
    InternalWriteUCS4Char(C, ByteCount);
end;



{                                                                              }
{ TUCS4_2143Codec                                                              }
{                                                                              }
procedure TUCS4_2143Codec.Decode(const Buf: Pointer; const BufSize: Integer;
    const DestBuf: Pointer; const DestSize: Integer;
    out ProcessedBytes, DestLength: Integer);
var Ch4     : UCS4Char;
    N, P, Q : PByteChar;
    T       : PByte;
    L, C    : Integer;
begin
  P := Buf;
  Q := DestBuf;
  C := DestSize div 2;
  if not Assigned(P) or (BufSize <= 0) or not Assigned(Q) or (C <= 0) then
    begin
      ProcessedBytes := 0;
      DestLength := 0;
      exit;
    end;
  L := 0;
  N := Pointer(NativeUInt(P) + NativeUInt(BufSize) - 4);
  while NativeUInt(P) <= NativeUInt(N) do
    begin
      T := Pointer(P);
      Inc(T);
      Ch4 := T^ * $1000000;
      T := Pointer(P);
      Inc(Ch4, T^ * $10000);
      T := Pointer(P);
      Inc(T, 3);
      Inc(Ch4, T^ * $100);
      T := Pointer(P);
      Inc(T, 2);
      Inc(Ch4, T^);
      if ((Ch4 >= $D800) and (Ch4 < $E000)) or (Ch4 > $10FFFF) then
        case FErrorAction of
          eaException :
            if Ch4 > $10FFFF
              then RaiseUnicodeCodecException(SCannotConvertUCS4, [Ch4, 'UTF-16'], NativeUInt(P) - NativeUInt(Buf))
              else RaiseUnicodeCodecException(SSurrogateNotAllowed, [Ch4, 'UCS-4'], NativeUInt(P) - NativeUInt(Buf));
          eaStop :
            break;
          eaSkip :
            Inc(P, 4);
          eaIgnore :
            begin
              if L + 1 >= C then
                break;
              Q^ := P^;
              Inc(Q);
              Inc(P);
              Q^ := P^;
              Inc(Q);
              Inc(P);
              Q^ := P^;
              Inc(Q);
              Inc(P);
              Q^ := P^;
              Inc(Q);
              Inc(P);
              Inc(L, 2);
            end;
          eaReplace :
            begin
              if L >= C then
                break;
              Q^ := AnsiChar(Lo(Ord(FDecodeReplaceChar)));
              Inc(Q);
              Q^ := AnsiChar(Hi(Ord(FDecodeReplaceChar)));
              Inc(Q);
              Inc(P, 4);
              Inc(L);
            end;
        end
      else
        if Ch4 > $FFFF then
          begin
            if L + 1 >= C then
              break;
            T := Pointer(P);
            Inc(T, 3);
            Q^ := AnsiChar((Ord(P^) shl 6) + (T^ shr 2));
            Inc(Q);
            Q^ := AnsiChar($D8 + (Ord(P^) shr 2));
            Inc(Q);
            T := Pointer(P);
            Inc(T, 2);
            Q^ := AnsiChar(T^);
            Inc(Q);
            T := Pointer(P);
            Inc(T, 3);
            Q^ := AnsiChar($DC +  (3 and T^));
            Inc(Q);
            Inc(P, 4);
            Inc(L, 2);
          end
        else
          begin
            if L >= C then
              break;
            T := Pointer(P);
            Inc(T, 2);
            Q^ := AnsiChar(T^);
            Inc(Q);
            T := Pointer(P);
            Inc(T, 3);
            Q^ := AnsiChar(T^);
            Inc(Q);
            Inc(P, 4);
            Inc(L);
          end;
    end;
  DestLength := L;
  ProcessedBytes := NativeUInt(P) - NativeUInt(Buf);
end;

function TUCS4_2143Codec.Encode(const S: PWideChar; const Length: Integer;
    out ProcessedChars: Integer): RawByteString;
var P, N          : PWideChar;
    Q             : PByteChar;
    M             : Integer;
    HighSurrogate : Word;
    LowSurrogate  : Word;
begin
  P := S;
  if not Assigned(P) or (Length <= 0) then
    begin
      ProcessedChars := 0;
      SetLength(Result, 0);
      exit;
    end;
  SetLength(Result, Length * 4);
  Q := Pointer(Result);
  M := 0;
  N := P + Length;
  while P < N do
    begin
      case Ord(P^) of
        $D800..$DBFF: // High surrogate of Unicode character [$10000..$10FFFF]
          begin
            if P = N - 1 then // End of UnicodeString?
              raise EConvertError.Create(SLowSurrogateNotFound);
            HighSurrogate := Ord(P^);
            Inc(P);
            Inc(M, 2);
            LowSurrogate := Ord(P^);
            case LowSurrogate of  // Low Surrogate following?
              $DC00..$DF00:
                begin
                  Q^ := AnsiChar((HighSurrogate - $D7C0) shr 6);
                  Inc(Q);
                  Q^ := AnsiChar(0);
                  Inc(Q);
                  Q^ := AnsiChar(Lo(LowSurrogate));
                  Inc(Q);
                  Q^ := AnsiChar(((HighSurrogate and $3F) shl 2) + ((LowSurrogate - $DC00) shr 8));
                  Inc(Q);
                  Inc(P);
                  Inc(M, 4);
                end;
            else
              raise EConvertError.Create(SLowSurrogateNotFound);
            end;
          end;
        $DC00..$DFFF: // Low surrogate of Unicode character [$10000..$10FFFF]
          raise EConvertError.Create(SHighSurrogateNotFound);
      else
        Q^ := AnsiChar(0);
        Inc(Q);
        Q^ := AnsiChar(0);
        Inc(Q);
        Q^ := AnsiChar(Lo(Ord(P^)));
        Inc(Q);
        Q^ := AnsiChar(Hi(Ord(P^)));
        Inc(Q);
        Inc(P);
        Inc(M, 4);
      end;
    end;
  SetLength(Result, M);
  ProcessedChars := Length;
end;

procedure TUCS4_2143Codec.InternalReadUCS4Char(out C: UCS4Char;
    out ByteCount: Integer);
var B : array[0..3] of Byte;
begin
  if ReadBuffer(B, 4) then
    begin
      C := B[1] * $1000000 + B[0] * $10000 + B[3] * $100 + B[2];
      case C of
        $D800..$DFFF: // Do not accept surrogates
          raise EConvertError.CreateFmt(SSurrogateNotAllowed, [C, 'UCS-4']);
      end;
    end
  else
    C := UCS4_STRING_TERMINATOR;
  ByteCount := 4;
end;

procedure TUCS4_2143Codec.InternalWriteUCS4Char(const C: UCS4Char;
    out ByteCount: Integer);
var Buffer : array[0..3] of Byte;
begin
  Buffer[0] := (C and $FF0000) shr 16;
  Buffer[1] := C shr 24;
  Buffer[2] := C and $FF;
  Buffer[3] := (C and $FF00) shr 8;
  WriteBuffer(Buffer, 4);
  ByteCount := 4;
end;

procedure TUCS4_2143Codec.WriteUCS4Char(const C: UCS4Char; out ByteCount: Integer);
const
  UCS4_2143_LF   : array[0..3] of Byte = ($00, $0A, $00, $00);
  UCS4_2143_CR   : array[0..3] of Byte = ($00, $0D, $00, $00);
  UCS4_2143_CRLF : array[0..7] of Byte = ($00, $0D, $00, $00, $00, $0A, $00, $00);
begin
  if C = UCS4_LF then
    case WriteLFOption of
      lwLF:
        begin
          WriteBuffer(UCS4_2143_LF, 4);
          ByteCount := 4;
        end;
      lwCR:
        begin
          WriteBuffer(UCS4_2143_CR, 4);
          ByteCount := 4;
        end;
      lwCRLF:
        begin
          WriteBuffer(UCS4_2143_CRLF, 8);
          ByteCount := 8;
        end;
    end
  else
    InternalWriteUCS4Char(C, ByteCount);
end;



{                                                                              }
{ TUCS4_3412Codec                                                              }
{                                                                              }
procedure TUCS4_3412Codec.Decode(const Buf: Pointer; const BufSize: Integer;
    const DestBuf: Pointer; const DestSize: Integer;
    out ProcessedBytes, DestLength: Integer);
var Ch4     : UCS4Char;
    N, P, Q : PByteChar;
    T, U    : PByte;
    L, C    : Integer;
begin
  P := Buf;
  Q := DestBuf;
  C := DestSize div 2;
  if not Assigned(P) or (BufSize <= 0) or not Assigned(Q) or (C <= 0) then
    begin
      ProcessedBytes := 0;
      DestLength := 0;
      exit;
    end;
  L := 0;
  N := Pointer(NativeUInt(P) + NativeUInt(BufSize) - 4);
  while NativeUInt(P) <= NativeUInt(N) do
    begin
      T := Pointer(P);
      Inc(T, 2);
      Ch4 := T^ * $1000000;
      T := Pointer(P);
      Inc(T, 3);
      Inc(Ch4, T^ * $10000);
      T := Pointer(P);
      Inc(Ch4, T^ * $100);
      T := Pointer(P);
      Inc(T);
      Inc(Ch4, T^);
      if ((Ch4 >= $D800) and (Ch4 < $E000)) or (Ch4 > $10FFFF) then
        case FErrorAction of
          eaException :
            if Ch4 > $10FFFF then
              RaiseUnicodeCodecException(SCannotConvertUCS4, [Ch4, 'UTF-16'], NativeUInt(P) - NativeUInt(Buf))
            else
              RaiseUnicodeCodecException(SSurrogateNotAllowed, [Ch4, 'UCS-4'], NativeUInt(P) - NativeUInt(Buf));
          eaStop :
            break;
          eaSkip :
            Inc(P, 4);
          eaIgnore :
            begin
              if L + 1 >= C then
                break;
              T := Pointer(P);
              Inc(T);
              Q^ := AnsiChar(T^);     // Nevertheless change Big Endian to Little Endian ...
              Inc(Q);
              T := Pointer(P);
              Q^ := AnsiChar(T^);
              Inc(Q);
              T := Pointer(P);
              Inc(T, 3);
              Q^ := AnsiChar(T^);
              Inc(Q);
              T := Pointer(P);
              Inc(T, 2);
              Q^ := AnsiChar(T^);
              Inc(Q);
              Inc(P, 4);
              Inc(L, 2);
            end;
          eaReplace :
            begin
              if L >= C then
                break;
              Q^ := AnsiChar(Lo(Ord(FDecodeReplaceChar)));
              Inc(Q);
              Q^ := AnsiChar(Hi(Ord(FDecodeReplaceChar)));
              Inc(Q);
              Inc(P, 4);
              Inc(L);
            end;
        end
      else
        if Ch4 > $FFFF then
          begin
            if L + 1 >= C then
              break;
            T := Pointer(P);
            Inc(T, 3);
            U := Pointer(P);
            Q^ := AnsiChar((T^ shl 6) + (U^ shr 2));
            Inc(Q);
            T := Pointer(P);
            Inc(T, 3);
            Q^ := AnsiChar($D8 + (T^ shr 2));
            Inc(Q);
            T := Pointer(P);
            Inc(T);
            Q^ := AnsiChar(T^);
            Inc(Q);
            U := Pointer(P);
            Q^ := AnsiChar($DC + (3 and U^));
            Inc(Q);
            Inc(P, 4);
            Inc(L, 2);
          end
        else
          begin
            if L >= C then
              break;
            T := Pointer(P);
            Inc(T);
            Q^ := AnsiChar(T^);
            Inc(Q);
            T := Pointer(P);
            Q^ := AnsiChar(T^);
            Inc(Q);
            Inc(P, 4);
            Inc(L);
          end;
    end;
  DestLength := L;
  ProcessedBytes := NativeUInt(P) - NativeUInt(Buf);
end;

function TUCS4_3412Codec.Encode(const S: PWideChar; const Length: Integer;
    out ProcessedChars: Integer): RawByteString;
var P, N          : PWideChar;
    Q             : PByteChar;
    M             : Integer;
    HighSurrogate : Word;
    LowSurrogate  : Word;
begin
  P := S;
  if not Assigned(P) or (Length <= 0) then
    begin
      ProcessedChars := 0;
      SetLength(Result, 0);
      exit;
    end;
  SetLength(Result, Length * 4);
  Q := Pointer(Result);
  M := 0;
  N := P + Length;
  while P < N do
    case Ord(P^) of
      $D800..$DBFF: // High surrogate of Unicode character [$10000..$10FFFF]
        begin
          if P = N - 1 then // End of UnicodeString?
            raise EConvertError.Create(SLowSurrogateNotFound);
          HighSurrogate := Ord(P^);
          Inc(P);
          Inc(M, 2);
          LowSurrogate := Ord(P^);
          case LowSurrogate of  // Low Surrogate following?
            $DC00..$DF00:
              begin
                Q^ := AnsiChar(((HighSurrogate and $3F) shl 2) + ((LowSurrogate - $DC00) shr 8));
                Inc(Q);
                Q^ := AnsiChar(Lo(LowSurrogate));
                Inc(Q);
                Q^ := AnsiChar(0);
                Inc(Q);
                Q^ := AnsiChar((HighSurrogate - $D7C0) shr 6);
                Inc(Q);
                Inc(P);
                Inc(M, 4);
              end;
          else
            raise EConvertError.Create(SLowSurrogateNotFound);
          end;
        end;
      $DC00..$DFFF: // Low surrogate of Unicode character [$10000..$10FFFF]
        raise EConvertError.Create(SHighSurrogateNotFound);
    else
      Q^ := AnsiChar(Hi(Ord(P^)));
      Inc(Q);
      Q^ := AnsiChar(Lo(Ord(P^)));
      Inc(Q);
      Q^ := AnsiChar(0);
      Inc(Q);
      Q^ := AnsiChar(0);
      Inc(Q);
      Inc(P);
      Inc(M, 4);
    end;
  SetLength(Result, M);
  ProcessedChars := Length;
end;

procedure TUCS4_3412Codec.InternalReadUCS4Char(out C: UCS4Char;
    out ByteCount: Integer);
var B : array[0..3] of Byte;
begin
  if ReadBuffer(B, 4) then
    begin
      C := B[2] * $1000000 + B[3] * $10000 + B[0] * $100 + B[1];
      case C of
        $D800..$DFFF: // Do not accept surrogates
          raise EConvertError.CreateFmt(SSurrogateNotAllowed, [C, 'UCS-4']);
      end;
      ByteCount := 4;
    end
  else
    begin
      C := UCS4_STRING_TERMINATOR;
      ByteCount := 0;
    end;
end;

procedure TUCS4_3412Codec.InternalWriteUCS4Char(const C: UCS4Char;
    out ByteCount: Integer);
var Buffer : array[0..3] of Byte;
begin
  Buffer[0] := (C and $FF00) shr 8;
  Buffer[1] := C and $FF;
  Buffer[2] := C shr 24;
  Buffer[3] := (C and $FF0000) shr 16;
  WriteBuffer(Buffer, 4);
  ByteCount := 4;
end;

procedure TUCS4_3412Codec.WriteUCS4Char(const C: UCS4Char; out ByteCount: Integer);
const
  UCS4_3412_LF   : array[0..3] of Byte = ($00, $00, $0A, $00);
  UCS4_3412_CR   : array[0..3] of Byte = ($00, $00, $0D, $00);
  UCS4_3412_CRLF : array[0..7] of Byte = ($00, $00, $0D, $00, $00, $00, $0A, $00);
begin
  if C = UCS4_LF then
    case WriteLFOption of
      lwLF:
        begin
          WriteBuffer(UCS4_3412_LF, 4);
          ByteCount := 4;
        end;
      lwCR:
        begin
          WriteBuffer(UCS4_3412_CR, 4);
          ByteCount := 4;
        end;
      lwCRLF:
        begin
          WriteBuffer(UCS4_3412_CRLF, 8);
          ByteCount := 8;
        end;
    end
  else
    InternalWriteUCS4Char(C, ByteCount);
end;



{                                                                              }
{ TUCS2Codec                                                                   }
{                                                                              }
procedure TUCS2Codec.Decode(const Buf: Pointer; const BufSize: Integer;
    const DestBuf: Pointer; const DestSize: Integer;
    out ProcessedBytes, DestLength: Integer);
var P       : PWideChar;
    Q       : PWideChar;
    I, L, C : Integer;
begin
  P := Buf;
  Q := DestBuf;
  C := DestSize div Sizeof(WideChar);
  if not Assigned(P) or (BufSize <= 0) or not Assigned(Q) or (C <= 0) then
    begin
      ProcessedBytes := 0;
      DestLength := 0;
      exit;
    end;
  L := 0;
  for I := 1 to BufSize div SizeOf(WideChar) do
    if (Ord(P^) >= $D800) and (Ord(P^) < $E000) then // Do not accept surrogates
      case FErrorAction of
        eaException :
          RaiseUnicodeCodecException(SSurrogateNotAllowed, [Ord(P^), 'UCS-2'], P - Buf);
        eaStop :
          break;
        eaSkip :
          Inc(P);
        eaIgnore :
          begin
            Q^ := WideChar(P^);
            Inc(P);
            Inc(Q);
            Inc(L);
          end;
        eaReplace :
          begin
            Q^ := FDecodeReplaceChar;
            Inc(P);
            Inc(Q);
            Inc(L);
          end;
      end
    else
      begin
        if L >= C then
          break;
        Q^ := P^;
        Inc(P);
        Inc(Q);
        Inc(L);
      end;
  DestLength := L;
  ProcessedBytes := P - Buf;
end;

function TUCS2Codec.Encode(const S: PWideChar; const Length: Integer;
    out ProcessedChars: Integer): RawByteString;
var P       : PWideChar;
    Q       : PWideChar;
    I, L, M : Integer;
begin
  Q := S;
  if not Assigned(Q) or (Length <= 0) then
    begin
      ProcessedChars := 0;
      SetLength(Result, 0);
      exit;
    end;
  SetLength(Result, Length*2);
  L := 0;
  M := 0;
  P := Pointer(Result);
  for I := 1 to Length do
    if (Ord(P^) >= $D800) and (Ord(P^) < $E000) then // Do not accept surrogates
      case FErrorAction of
        eaException :
          RaiseUnicodeCodecException(SSurrogateNotAllowed, [Ord(P^), 'UCS-2'], L * 2);
        eaStop :
          break;
        eaSkip :
          begin
            Inc(Q);
            Inc(L);
          end;
        eaIgnore :
          begin
            P^ := Q^;
            Inc(P);
            Inc(Q);
            Inc(L);
            Inc(M);
          end;
        eaReplace :
          begin
            P^ := FDecodeReplaceChar;
            Inc(P);
            Inc(Q);
            Inc(L);
            Inc(M);
          end;
      end
    else
      begin
        P^ := Q^;
        Inc(P);
        Inc(Q);
        Inc(L);
        Inc(M);
      end;
  if Length <> M then
    SetLength(Result, M * 2);
  ProcessedChars := L;
end;

procedure TUCS2Codec.InternalReadUCS4Char(out C: UCS4Char;
    out ByteCount: Integer);
begin
  C := 0;
  // C must be initialized, because the ReadBuffer(C, 2) call below does
  // not fill the whole variable!
  if not ReadBuffer(C, 2) then
    begin
      C := UCS4_STRING_TERMINATOR;
      ByteCount := 0;
      exit;
    end;
  C := System.Swap(C);  // UCS4Chars are stored in Little Endian mode; so we need to swap the bytes.
  ByteCount := 2;
  case C of
    $D800..$DFFF: // Do not accept surrogates
      raise EConvertError.CreateFmt(SSurrogateNotAllowed, [C, 'UCS-2']);
  end;
end;

procedure TUCS2Codec.InternalWriteUCS4Char(const C: UCS4Char;
    out ByteCount: Integer);
var HighByte, LowByte: Byte;
begin
  if C > $FFFF then
    raise EConvertError.CreateFmt(SEncodingOutOfRange, ['UCS-2']);
  {$IFDEF FREEPASCAL}
  HighByte := Byte((C and $FF00) shr 8);
  LowByte := Byte(C and $FF);
  {$ELSE}
  HighByte := Hi(C);
  LowByte := Lo(C);
  {$ENDIF}
  WriteBuffer(HighByte, 1);
  WriteBuffer(LowByte, 1);
  ByteCount := 2;
end;

procedure TUCS2Codec.WriteUCS4Char(const C: UCS4Char; out ByteCount: Integer);
const
  UCS2_LF   : array[0..1] of Byte = ($00, $0A);
  UCS2_CR   : array[0..1] of Byte = ($00, $0D);
  UCS2_CRLF : array[0..3] of Byte = ($00, $0D, $00, $0A);
begin
  if C = UCS4_LF then
    case WriteLFOption of
      lwLF:
        begin
          WriteBuffer(UCS2_LF, 2);
          ByteCount := 2;
        end;
      lwCR:
        begin
          WriteBuffer(UCS2_CR, 2);
          ByteCount := 2;
        end;
      lwCRLF:
        begin
          WriteBuffer(UCS2_CRLF, 4);
          ByteCount := 4;
        end;
    end
  else
    InternalWriteUCS4Char(C, ByteCount);
end;



{                                                                              }
{ ISO-8859-1 - Latin 1                                                         }
{ Western Europe and Americas: Afrikaans, Basque, Catalan, Danish, Dutch,      }
{ English, Faeroese, Finnish, French, Galician, German, Icelandic, Irish,      }
{ Italian, Norwegian, Portuguese, Spanish and Swedish.                         }
{ Default for HTTP Protocol                                                    }
{                                                                              }
const
  ISO8859_1Aliases = 8;
  ISO8859_1Alias : array[0..ISO8859_1Aliases - 1] of String = (
      'ISO-8859-1', 'ISO_8859-1:1987', 'ISO_8859-1',
      'iso-ir-100', 'latin1', 'l1', 'IBM819', 'cp819');

class function TISO8859_1Codec.GetAliasCount: Integer;
begin
  Result := ISO8859_1Aliases;
end;

class function TISO8859_1Codec.GetAlias(const Idx: Integer): String;
begin
  if Idx < 0 then
    raise EUnicodeCodecException.Create(SAliasIndexOutOfRange);
  if Idx >= GetAliasCount then
    raise EUnicodeCodecException.Create(SAliasIndexOutOfRange);
  Result := ISO8859_1Alias[Idx];
end;

function TISO8859_1Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  Result := WideChar(P);
end;

function TISO8859_1Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  if Ord(Ch) >= $100 then
    raise EConvertError.CreateFmt(SCannotConvert, [Ord(Ch), 'ISO-8859-1']);
  Result := AnsiChar(Ch);
end;

procedure TISO8859_1Codec.Decode(const Buf: Pointer; const BufSize: Integer;
    const DestBuf: Pointer; const DestSize: Integer;
    out ProcessedBytes, DestLength: Integer);
var L, C: Integer;
begin
  L := BufSize;
  C := DestSize div Sizeof(WideChar);
  if C < L then
    L := C;
  if L < 0 then
    L := 0;
  ProcessedBytes := L;
  DestLength := L;
  RawByteBufToWideBuf(Buf, L, DestBuf);
end;



{                                                                              }
{ ISO-8859-2 Latin 2                                                           }
{ Latin-written Slavic and Central European languages: Czech, German,          }
{ Hungarian, Polish, Romanian, Croatian, Slovak, Slovene.                      }
{                                                                              }
const
  ISO8859_2Aliases = 6;
  ISO8859_2Alias : array[0..ISO8859_2Aliases - 1] of String = (
      'ISO-8859-2', 'ISO_8859-2:1987', 'ISO_8859-2',
      'iso-ir-101', 'latin2', 'l2');

class function TISO8859_2Codec.GetAliasCount: Integer;
begin
  Result := ISO8859_2Aliases;
end;

class function TISO8859_2Codec.GetAlias(const Idx: Integer): String;
begin
  if Idx < 0 then
    raise EUnicodeCodecException.Create(SAliasIndexOutOfRange);
  if Idx >= GetAliasCount then
    raise EUnicodeCodecException.Create(SAliasIndexOutOfRange);
  Result := ISO8859_2Alias[Idx];
end;

const
  ISO8859_2Map : AnsiCharISOMap = (
      #$00A0, #$0104, #$02D8, #$0141, #$00A4, #$013D, #$015A, #$00A7,
      #$00A8, #$0160, #$015E, #$0164, #$0179, #$00AD, #$017D, #$017B,
      #$00B0, #$0105, #$02DB, #$0142, #$00B4, #$013E, #$015B, #$02C7,
      #$00B8, #$0161, #$015F, #$0165, #$017A, #$02DD, #$017E, #$017C,
      #$0154, #$00C1, #$00C2, #$0102, #$00C4, #$0139, #$0106, #$00C7,
      #$010C, #$00C9, #$0118, #$00CB, #$011A, #$00CD, #$00CE, #$010E,
      #$0110, #$0143, #$0147, #$00D3, #$00D4, #$0150, #$00D6, #$00D7,
      #$0158, #$016E, #$00DA, #$0170, #$00DC, #$00DD, #$0162, #$00DF,
      #$0155, #$00E1, #$00E2, #$0103, #$00E4, #$013A, #$0107, #$00E7,
      #$010D, #$00E9, #$0119, #$00EB, #$011B, #$00ED, #$00EE, #$010F,
      #$0111, #$0144, #$0148, #$00F3, #$00F4, #$0151, #$00F6, #$00F7,
      #$0159, #$016F, #$00FA, #$0171, #$00FC, #$00FD, #$0163, #$02D9);

function TISO8859_2Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  if Ord(P) < $A0 then
    Result := WideChar(P)
  else
    Result := ISO8859_2Map[Ord(P)];
end;

function TISO8859_2Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromISOMap(Ch, ISO8859_2Map, 'ISO-8859-2');
end;



{                                                                              }
{ ISO-8859-3 - Latin 3                                                         }
{ Esperanto, Galician, Maltese, and Turkish.                                   }
{                                                                              }
const
  ISO8859_3Aliases = 6;
  ISO8859_3Alias : array[0..ISO8859_3Aliases - 1] of String = (
      'ISO-8859-3', 'ISO_8859-3:1988', 'ISO_8859-3',
      'iso-ir-109', 'latin3', 'l3');

class function TISO8859_3Codec.GetAliasCount: Integer;
begin
  Result := ISO8859_3Aliases;
end;

class function TISO8859_3Codec.GetAlias(const Idx: Integer): String;
begin
  if Idx < 0 then
    raise EUnicodeCodecException.Create(SAliasIndexOutOfRange);
  if Idx >= GetAliasCount then
    raise EUnicodeCodecException.Create(SAliasIndexOutOfRange);
  Result := ISO8859_3Alias[Idx];
end;

const
  ISO8859_3Map : AnsiCharISOMap = (
      #$00A0, #$0126, #$02D8, #$00A3, #$00A4, #$FFFF, #$0124, #$00A7,
      #$00A8, #$0130, #$015E, #$011E, #$0134, #$00AD, #$FFFF, #$017B,
      #$00B0, #$0127, #$00B2, #$00B3, #$00B4, #$00B5, #$0125, #$00B7,
      #$00B8, #$0131, #$015F, #$011F, #$0135, #$00BD, #$FFFF, #$017C,
      #$00C0, #$00C1, #$00C2, #$FFFF, #$00C4, #$010A, #$0108, #$00C7,
      #$00C8, #$00C9, #$00CA, #$00CB, #$00CC, #$00CD, #$00CE, #$00CF,
      #$FFFF, #$00D1, #$00D2, #$00D3, #$00D4, #$0120, #$00D6, #$00D7,
      #$011C, #$00D9, #$00DA, #$00DB, #$00DC, #$016C, #$015C, #$00DF,
      #$00E0, #$00E1, #$00E2, #$FFFF, #$00E4, #$010B, #$0109, #$00E7,
      #$00E8, #$00E9, #$00EA, #$00EB, #$00EC, #$00ED, #$00EE, #$00EF,
      #$FFFF, #$00F1, #$00F2, #$00F3, #$00F4, #$0121, #$00F6, #$00F7,
      #$011D, #$00F9, #$00FA, #$00FB, #$00FC, #$016D, #$015D, #$02D9);

function TISO8859_3Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  if Ord(P) < $A0 then
    Result := WideChar(P)
  else
    Result := ISO8859_3Map[Ord(P)];
end;

function TISO8859_3Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromISOMap(Ch, ISO8859_3Map, 'ISO-8859-3');
end;



{                                                                              }
{ ISO-8859-4 - Latin 4                                                         }
{ Scandinavia/Baltic (mostly covered by 8859-1 also): Estonian, Latvian, and   }
{ Lithuanian. It is an incomplete predecessor of Latin 6.                      }
{                                                                              }
const
  ISO8859_4Aliases = 6;
  ISO8859_4Alias : array[0..ISO8859_4Aliases - 1] of String = (
      'ISO-8859-4', 'ISO_8859-4:1988', 'ISO_8859-4',
      'iso-ir-110', 'latin4', 'l4');

class function TISO8859_4Codec.GetAliasCount: Integer;
begin
  Result := ISO8859_4Aliases;
end;

class function TISO8859_4Codec.GetAlias(const Idx: Integer): String;
begin
  if Idx < 0 then
    raise EUnicodeCodecException.Create(SAliasIndexOutOfRange);
  if Idx >= GetAliasCount then
    raise EUnicodeCodecException.Create(SAliasIndexOutOfRange);
  Result := ISO8859_4Alias[Idx];
end;

const
  ISO8859_4Map : AnsiCharISOMap = (
      #$00A0, #$0104, #$0138, #$0156, #$00A4, #$0128, #$013B, #$00A7,
      #$00A8, #$0160, #$0112, #$0122, #$0166, #$00AD, #$017D, #$00AF,
      #$00B0, #$0105, #$02DB, #$0157, #$00B4, #$0129, #$013C, #$02C7,
      #$00B8, #$0161, #$0113, #$0123, #$0167, #$014A, #$017E, #$014B,
      #$0100, #$00C1, #$00C2, #$00C3, #$00C4, #$00C5, #$00C6, #$012E,
      #$010C, #$00C9, #$0118, #$00CB, #$0116, #$00CD, #$00CE, #$012A,
      #$0110, #$0145, #$014C, #$0136, #$00D4, #$00D5, #$00D6, #$00D7,
      #$00D8, #$0172, #$00DA, #$00DB, #$00DC, #$0168, #$016A, #$00DF,
      #$0101, #$00E1, #$00E2, #$00E3, #$00E4, #$00E5, #$00E6, #$012F,
      #$010D, #$00E9, #$0119, #$00EB, #$0117, #$00ED, #$00EE, #$012B,
      #$0111, #$0146, #$014D, #$0137, #$00F4, #$00F5, #$00F6, #$00F7,
      #$00F8, #$0173, #$00FA, #$00FB, #$00FC, #$0169, #$016B, #$02D9);

function TISO8859_4Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  if Ord(P) < $A0 then
    Result := WideChar(P)
  else
    Result := ISO8859_4Map[Ord(P)];
end;

function TISO8859_4Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromISOMap(Ch, ISO8859_4Map, 'ISO-8859-4');
end;



{                                                                              }
{ ISO-8859-5 - Cyrillic                                                        }
{ Bulgarian, Byelorussian, Macedonian, Russian, Serbian and Ukrainian.         }
{                                                                              }
const
  ISO8859_5Aliases = 5;
  ISO8859_5Alias : array[0..ISO8859_5Aliases - 1] of String = (
      'ISO-8859-5', 'ISO_8859-5:1988', 'ISO_8859-5',
      'iso-ir-144', 'cyrillic');

class function TISO8859_5Codec.GetAliasCount: Integer;
begin
  Result := ISO8859_5Aliases;
end;

class function TISO8859_5Codec.GetAlias(const Idx: Integer): String;
begin
  if Idx < 0 then
    raise EUnicodeCodecException.Create(SAliasIndexOutOfRange);
  if Idx >= GetAliasCount then
    raise EUnicodeCodecException.Create(SAliasIndexOutOfRange);
  Result := ISO8859_5Alias[Idx];
end;

function TISO8859_5Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  case Ord(P) of
    $00..$A0, $AD : Result := WideChar(P);
    $F0 : Result := #$2116;
    $FD : Result := #$00A7;
  else
    Result := WideChar(Ord(P) + $0360);
  end;
end;

function TISO8859_5Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  if Ord(Ch) <= $A0 then
    Result := AnsiChar(Ch)
  else
    case Ch of
      #$2116 : Result := AnsiChar($F0);
      #$00A7 : Result := AnsiChar($FD);
      #$00AD : Result := AnsiChar($AD);
      #$0401..#$045F :
        case Ch of
          #$0450, #$045D, #$040D :
            raise EConvertError.CreateFmt(SCannotConvert, [Ord(Ch), 'ISO-8859-5']);
        else
          Result := AnsiChar(Ord(Ch) - $0360);
        end;
    else
      raise EConvertError.CreateFmt(SCannotConvert, [Ord(Ch), 'ISO-8859-5']);
    end;
end;



{                                                                              }
{ ISO-8859-6 - Arabic                                                          }
{ Non-accented Arabic.                                                         }
{                                                                              }
const
  ISO8859_6Aliases = 7;
  ISO8859_6Alias : array[0..ISO8859_6Aliases - 1] of String = (
      'ISO-8859-6', 'ISO_8859-6:1987', 'ISO_8859-6',
      'iso-ir-127', 'ECMA-114', 'ASMO-708', 'arabic');

class function TISO8859_6Codec.GetAliasCount: Integer;
begin
  Result := ISO8859_6Aliases;
end;

class function TISO8859_6Codec.GetAlias(const Idx: Integer): String;
begin
  if Idx < 0 then
    raise EUnicodeCodecException.Create(SAliasIndexOutOfRange);
  if Idx >= GetAliasCount then
    raise EUnicodeCodecException.Create(SAliasIndexOutOfRange);
  Result := ISO8859_6Alias[Idx];
end;

function TISO8859_6Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  case Ord(P) of
    $00..$A0, $A4, $AD                : Result := WideChar(P);
    $AC, $BB, $BF, $C1..$DA, $E0..$F2 : Result := WideChar(Ord(P) + $0580);
  else
    Result := #$FFFF;
  end;
end;

function TISO8859_6Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  if Ord(Ch) <= $A0 then
    Result := AnsiChar(Ch)
  else
    case Ch of
      #$00A4 : Result := AnsiChar($A4);
      #$00AD : Result := AnsiChar($AD);
      #$062C, #$063B, #$063F, #$0641..#$065A, #$0660..#$0672 :
        Result := AnsiChar(Ord(Ch) - $0580);
    else
      raise EConvertError.CreateFmt(SCannotConvert, [Ord(Ch), 'ISO-8859-6']);
    end;
end;



{                                                                              }
{ ISO-8859-7 - Modern Greek                                                    }
{ Greek.                                                                       }
{                                                                              }
const
  ISO8859_7Aliases = 8;
  ISO8859_7Alias : array[0..ISO8859_7Aliases - 1] of String = (
      'ISO-8859-7', 'ISO_8859-7:1987', 'ISO_8859-7',
      'iso-ir-126', 'ELOT_928', 'ECMA-118', 'greek', 'greek8');

class function TISO8859_7Codec.GetAliasCount: Integer;
begin
  Result := ISO8859_7Aliases;
end;

class function TISO8859_7Codec.GetAlias(const Idx: Integer): String;
begin
  if Idx < 0 then
    raise EUnicodeCodecException.Create(SAliasIndexOutOfRange);
  if Idx >= GetAliasCount then
    raise EUnicodeCodecException.Create(SAliasIndexOutOfRange);
  Result := ISO8859_7Alias[Idx];
end;

function TISO8859_7Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  case Ord(P) of
    $00..$A0, $A6..$A9, $AB..$AD, $B0..$B3, $B7, $BB, $BD :
      Result := WideChar(P);
    $A1      : Result := #$2018;
    $A2      : Result := #$2019;
    $AF      : Result := #$2015;
    $D2, $FF : Result := #$FFFF;
  else
    Result := WideChar(Ord(P) + $02D0);
  end;
end;

function TISO8859_7Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  if Ord(Ch) <= $A0 then
    Result := AnsiChar(Ch)
  else
    case Ch of
      #$00A6..#$00A9, #$00AB..#$00AD, #$00B0..#$00B3, #$00B7, #$00BB, #$00BD :
        Result := AnsiChar(Ch);
      #$0373..#$03CE : Result := AnsiChar(Ord(Ch) - $02D0);
      #$2018         : Result := AnsiChar($A1);
      #$2019         : Result := AnsiChar($A2);
      #$2015         : Result := AnsiChar($AF);
    else
      raise EConvertError.CreateFmt(SCannotConvert, [Ord(Ch), 'ISO-8859-7']);
    end;
end;



{                                                                              }
{ ISO-8859-8 - Hebrew                                                          }
{ Non-accented Hebrew.                                                         }
{                                                                              }
const
  ISO8859_8Aliases = 5;
  ISO8859_8Alias : array[0..ISO8859_8Aliases - 1] of String = (
      'ISO-8859-8', 'ISO_8859-8:1988', 'ISO_8859-8',
      'iso-ir-138', 'hebrew');

class function TISO8859_8Codec.GetAliasCount: Integer;
begin
  Result := ISO8859_8Aliases;
end;

class function TISO8859_8Codec.GetAlias(const Idx: Integer): String;
begin
  if Idx < 0 then
    raise EUnicodeCodecException.Create(SAliasIndexOutOfRange);
  if Idx >= GetAliasCount then
    raise EUnicodeCodecException.Create(SAliasIndexOutOfRange);
  Result := ISO8859_8Alias[Idx];
end;

function TISO8859_8Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  case Ord(P) of
    $00..$A0, $A2..$A9, $AB..$AE, $B0..$B9, $BB..$BE :
      Result := WideChar(P);
    $AA      : Result := #$00D7;
    $AF      : Result := #$203E;
    $BA      : Result := #$00F7;
    $DF      : Result := #$2017;
    $E0..$FA : Result := WideChar(Ord(P) + $04E0);
  else
    Result := #$FFFF;
  end;
end;

function TISO8859_8Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  if Ord(Ch) <= $A0 then
    Result := AnsiChar(Ch)
  else
    case Ch of
      #$00A2..#$00A9, #$00AB..#$00AE, #$00B0..#$00B9, #$00BB..#$00BE :
        Result := AnsiChar(Ord(Ch));
      #$00D7 : Result := AnsiChar($AA);
      #$203E : Result := AnsiChar($AF);
      #$00F7 : Result := AnsiChar($BA);
      #$2017 : Result := AnsiChar($DF);
      #$05C0..#$05DA : Result := AnsiChar(Ord(Ch) - $04E0);
    else
      raise EConvertError.CreateFmt(SCannotConvert, [Ord(Ch), 'ISO-8859-8']);
    end;
end;




{                                                                              }
{ ISO-8859-9 - Latin 5                                                         }
{ Same as 8859-1 except for Turkish instead of Icelandic                       }
{                                                                              }
const
  ISO8859_9Aliases = 6;
  ISO8859_9Alias : array[0..ISO8859_9Aliases - 1] of String = (
      'ISO-8859-9', 'ISO_8859-9:1989', 'ISO_8859-9',
      'iso-ir-148', 'latin5', 'l5');

class function TISO8859_9Codec.GetAliasCount: Integer;
begin
  Result := ISO8859_9Aliases;
end;

class function TISO8859_9Codec.GetAlias(const Idx: Integer): String;
begin
  if Idx < 0 then
    raise EUnicodeCodecException.Create(SAliasIndexOutOfRange);
  if Idx >= GetAliasCount then
    raise EUnicodeCodecException.Create(SAliasIndexOutOfRange);
  Result := ISO8859_9Alias[Idx];
end;

function TISO8859_9Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  case Ord(P) of
    $D0 : Result := #$011E;
    $DD : Result := #$0130;
    $DE : Result := #$015E;
    $F0 : Result := #$011F;
    $FD : Result := #$0131;
    $FE : Result := #$015F;
  else
    Result := WideChar(P);
  end;
end;

function TISO8859_9Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  case Ch of
    #$011E : Result := AnsiChar($D0);
    #$0130 : Result := AnsiChar($DD);
    #$015E : Result := AnsiChar($DE);
    #$011F : Result := AnsiChar($F0);
    #$0131 : Result := AnsiChar($FD);
    #$015F : Result := AnsiChar($FE);
  else
    if Ord(Ch) <= $00FF then
      Result := AnsiChar(Ch)
    else
      raise EConvertError.CreateFmt(SCannotConvert, [Ord(Ch), 'ISO-8859-9']);
  end;
end;



{                                                                              }
{ ISO-8859-10 - Latin 6                                                        }
{ Latin6, for Lappish/Nordic/Eskimo languages: Adds the last Inuit             }
{ (Greenlandic) and Sami (Lappish) letters that were missing in Latin 4 to     }
{ cover the entire Nordic area.                                                }
{                                                                              }
const
  ISO8859_10Aliases = 6;
  ISO8859_10Alias : array[0..ISO8859_10Aliases - 1] of String = (
      'ISO-8859-10', 'ISO_8859-10:1992', 'ISO_8859-10',
      'iso-ir-157', 'latin6', 'l6');

class function TISO8859_10Codec.GetAliasCount: Integer;
begin
  Result := ISO8859_10Aliases;
end;

class function TISO8859_10Codec.GetAlias(const Idx: Integer): String;
begin
  if Idx < 0 then
    raise EUnicodeCodecException.Create(SAliasIndexOutOfRange);
  if Idx >= GetAliasCount then
    raise EUnicodeCodecException.Create(SAliasIndexOutOfRange);
  Result := ISO8859_10Alias[Idx];
end;

const
  ISO8859_10Map : AnsiCharISOMap = (
      #$00A0, #$0104, #$0112, #$0122, #$012A, #$0128, #$0136, #$00A7,
      #$013B, #$0110, #$0160, #$0166, #$017D, #$00AD, #$016A, #$014A,
      #$00B0, #$0105, #$0113, #$0123, #$012B, #$0129, #$0137, #$00B7,
      #$013C, #$0111, #$0161, #$0167, #$017E, #$2014, #$016B, #$014B,
      #$0100, #$00C1, #$00C2, #$00C3, #$00C4, #$00C5, #$00C6, #$012E,
      #$010C, #$00C9, #$0118, #$00CB, #$0116, #$00CD, #$00CE, #$00CF,
      #$00D0, #$0145, #$014C, #$00D3, #$00D4, #$00D5, #$00D6, #$0168,
      #$00D8, #$0172, #$00DA, #$00DB, #$00DC, #$00DD, #$00DE, #$00DF,
      #$0101, #$00E1, #$00E2, #$00E3, #$00E4, #$00E5, #$00E6, #$012F,
      #$010D, #$00E9, #$0119, #$00EB, #$0117, #$00ED, #$00EE, #$00EF,
      #$00F0, #$0146, #$014D, #$00F3, #$00F4, #$00F5, #$00F6, #$0169,
      #$00F8, #$0173, #$00FA, #$00FB, #$00FC, #$00FD, #$00FE, #$0138);

function TISO8859_10Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  if Ord(P) < $A0 then
    Result := WideChar(P)
  else
    Result := ISO8859_10Map[Ord(P)];
end;

function TISO8859_10Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromISOMap(Ch, ISO8859_10Map, 'ISO-8859-10');
end;



{                                                                              }
{ ISO-8859-13 - Latin 7                                                        }
{                                                                              }
const
  ISO8859_13Aliases = 4;
  ISO8859_13Alias : array[0..ISO8859_13Aliases - 1] of String = (
      'ISO-8859-13', 'ISO_8859-13', 'latin7', 'l7');

class function TISO8859_13Codec.GetAliasCount: Integer;
begin
  Result := ISO8859_13Aliases;
end;

class function TISO8859_13Codec.GetAlias(const Idx: Integer): String;
begin
  if Idx < 0 then
    raise EUnicodeCodecException.Create(SAliasIndexOutOfRange);
  if Idx >= GetAliasCount then
    raise EUnicodeCodecException.Create(SAliasIndexOutOfRange);
  Result := ISO8859_13Alias[Idx];
end;

const
  ISO8859_13Map : AnsiCharISOMap = (
    #$00A0, #$201D, #$00A2, #$00A3, #$00A4, #$201E, #$00A6, #$00A7,
    #$00D8, #$00A9, #$0156, #$00AB, #$00AC, #$00AD, #$00AE, #$00C6,
    #$00B0, #$00B1, #$00B2, #$00B3, #$201C, #$00B5, #$00B6, #$00B7,
    #$00F8, #$00B9, #$0157, #$00BB, #$00BC, #$00BD, #$00BE, #$00E6,
    #$0104, #$012E, #$0100, #$0106, #$00C4, #$00C5, #$0118, #$0112,
    #$010C, #$00C9, #$0179, #$0116, #$0122, #$0136, #$012A, #$013B,
    #$0160, #$0143, #$0145, #$00D3, #$014C, #$00D5, #$00D6, #$00D7,
    #$0172, #$0141, #$015A, #$016A, #$00DC, #$017B, #$017D, #$00DF,
    #$0105, #$012F, #$0101, #$0107, #$00E4, #$00E5, #$0119, #$0113,
    #$010D, #$00E9, #$017A, #$0117, #$0123, #$0137, #$012B, #$013C,
    #$0161, #$0144, #$0146, #$00F3, #$014D, #$00F5, #$00F6, #$00F7,
    #$0173, #$0142, #$015B, #$016B, #$00FC, #$017B, #$017E, #$2019);

function TISO8859_13Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  if Ord(P) < $A0 then
    Result := WideChar(P)
  else
    Result := ISO8859_13Map[Ord(P)];
end;

function TISO8859_13Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromISOMap(Ch, ISO8859_13Map, 'ISO-8859-13');
end;



{                                                                              }
{ ISO-8859-14 - Latin 8                                                        }
{                                                                              }
const
  ISO8859_14Aliases = 7;
  ISO8859_14Alias : array[0..ISO8859_14Aliases - 1] of String = (
      'ISO-8859-14', 'ISO_8859-14:1998', 'ISO_8859-14',
      'iso-ir-199', 'latin8', 'l8', 'iso-celtic');

class function TISO8859_14Codec.GetAliasCount: Integer;
begin
  Result := ISO8859_14Aliases;
end;

class function TISO8859_14Codec.GetAlias(const Idx: Integer): String;
begin
  if Idx < 0 then
    raise EUnicodeCodecException.Create(SAliasIndexOutOfRange);
  if Idx >= GetAliasCount then
    raise EUnicodeCodecException.Create(SAliasIndexOutOfRange);
  Result := ISO8859_14Alias[Idx];
end;

const
  ISO8859_14Map : AnsiCharISOMap = (
    #$00A0, #$1E02, #$1E03, #$00A3, #$010A, #$010B, #$1E0A, #$00A7,
    #$1E80, #$00A9, #$1E82, #$1E0B, #$1EF2, #$00AD, #$00AE, #$0178,
    #$1E1E, #$1E1F, #$0120, #$0121, #$1E40, #$1E41, #$00B6, #$1E56,
    #$1E81, #$1E57, #$1E83, #$1E60, #$1EF3, #$1E84, #$1E85, #$1E61,
    #$00C0, #$00C1, #$00C2, #$00C3, #$00C4, #$00C5, #$00C6, #$00C7,
    #$00C8, #$00C9, #$00CA, #$00CB, #$00CC, #$00CD, #$00CE, #$00CF,
    #$0174, #$00D1, #$00D2, #$00D3, #$00D4, #$00D5, #$00D6, #$1E6A,
    #$00D8, #$00D9, #$00DA, #$00DB, #$00DC, #$00DD, #$0176, #$00DF,
    #$00E0, #$00E1, #$00E2, #$00E3, #$00E4, #$00E5, #$00E6, #$00E7,
    #$00E8, #$00E9, #$00EA, #$00EB, #$00EC, #$00ED, #$00EE, #$00EF,
    #$0175, #$00F1, #$00F2, #$00F3, #$00F4, #$00F5, #$00F6, #$1E6B,
    #$00F8, #$00F9, #$00FA, #$00FB, #$00FC, #$00FD, #$0177, #$00FF);

function TISO8859_14Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  if Ord(P) < $A0 then
    Result := WideChar(P)
  else
    Result := ISO8859_14Map[Ord(P)];
end;

function TISO8859_14Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromISOMap(Ch, ISO8859_14Map, 'ISO-8859-14');
end;



{                                                                              }
{ ISO-8859-15 - Latin 9                                                        }
{                                                                              }
const
  ISO8859_15Aliases = 6;
  ISO8859_15Alias : array[0..ISO8859_15Aliases - 1] of String = (
      'ISO-8859-15', 'ISO_8859-15',
      'latin9', 'l9', 'latin0', 'l0');

class function TISO8859_15Codec.GetAliasCount: Integer;
begin
  Result := ISO8859_15Aliases;
end;

class function TISO8859_15Codec.GetAlias(const Idx: Integer): String;
begin
  if Idx < 0 then
    raise EUnicodeCodecException.Create(SAliasIndexOutOfRange);
  if Idx >= GetAliasCount then
    raise EUnicodeCodecException.Create(SAliasIndexOutOfRange);
  Result := ISO8859_15Alias[Idx];
end;

function TISO8859_15Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  case Ord(P) of
    $A4 : Result := #$20AC;
    $A6 : Result := #$00A6;
    $A8 : Result := #$0161;
    $B4 : Result := #$017D;
    $B8 : Result := #$017E;
    $BC : Result := #$0152;
    $BD : Result := #$0153;
    $BE : Result := #$0178;
  else
    Result := WideChar(P);
  end;
end;

function TISO8859_15Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  case Ch of
    #$20AC : Result := AnsiChar($A4);
    #$00A6 : Result := AnsiChar($A6);
    #$0161 : Result := AnsiChar($A8);
    #$017D : Result := AnsiChar($B4);
    #$017E : Result := AnsiChar($B8);
    #$0152 : Result := AnsiChar($BC);
    #$0153 : Result := AnsiChar($BD);
    #$0178 : Result := AnsiChar($BE);
  else
    if Ord(Ch) <= $00FF then
      Result := AnsiChar(Ch)
    else
      raise EConvertError.CreateFmt(SCannotConvert, [Ord(Ch), 'ISO-8859-15']);
  end;
end;



{                                                                              }
{ CP37                                                                         }
{   Map shared by IBM037 and Windows-37.                                       }
{                                                                              }
const
  CP37Map : AnsiCharMap = (
      #$0000, #$0001, #$0002, #$0003, #$009C, #$0009, #$0086, #$007F,
      #$0097, #$008D, #$008E, #$000B, #$000C, #$000D, #$000E, #$000F,
      #$0010, #$0011, #$0012, #$0013, #$009D, #$0085, #$0008, #$0087,
      #$0018, #$0019, #$0092, #$008F, #$001C, #$001D, #$001E, #$001F,
      #$0080, #$0081, #$0082, #$0083, #$0084, #$000A, #$0017, #$001B,
      #$0088, #$0089, #$008A, #$008B, #$008C, #$0005, #$0006, #$0007,
      #$0090, #$0091, #$0016, #$0093, #$0094, #$0095, #$0096, #$0004,
      #$0098, #$0099, #$009A, #$009B, #$0014, #$0015, #$009E, #$001A,
      #$0020, #$00A0, #$00E2, #$00E4, #$00E0, #$00E1, #$00E3, #$00E5,
      #$00E7, #$00F1, #$00A2, #$002E, #$003C, #$0028, #$002B, #$007C,
      #$0026, #$00E9, #$00EA, #$00EB, #$00E8, #$00ED, #$00EE, #$00EF,
      #$00EC, #$00DF, #$0021, #$0024, #$002A, #$0029, #$003B, #$00AC,
      #$002D, #$002F, #$00C2, #$00C4, #$00C0, #$00C1, #$00C3, #$00C5,
      #$00C7, #$00D1, #$00A6, #$002C, #$0025, #$005F, #$003E, #$003F,
      #$00F8, #$00C9, #$00CA, #$00CB, #$00C8, #$00CD, #$00CE, #$00CF,
      #$00CC, #$0060, #$003A, #$0023, #$0040, #$0027, #$003D, #$0022,
      #$00D8, #$0061, #$0062, #$0063, #$0064, #$0065, #$0066, #$0067,
      #$0068, #$0069, #$00AB, #$00BB, #$00F0, #$00FD, #$00FE, #$00B1,
      #$00B0, #$006A, #$006B, #$006C, #$006D, #$006E, #$006F, #$0070,
      #$0071, #$0072, #$00AA, #$00BA, #$00E6, #$00B8, #$00C6, #$00A4,
      #$00B5, #$007E, #$0073, #$0074, #$0075, #$0076, #$0077, #$0078,
      #$0079, #$007A, #$00A1, #$00BF, #$00D0, #$00DD, #$00DE, #$00AE,
      #$005E, #$00A3, #$00A5, #$00B7, #$00A9, #$00A7, #$00B6, #$00BC,
      #$00BD, #$00BE, #$005B, #$005D, #$00AF, #$00A8, #$00B4, #$00D7,
      #$007B, #$0041, #$0042, #$0043, #$0044, #$0045, #$0046, #$0047,
      #$0048, #$0049, #$00AD, #$00F4, #$00F6, #$00F2, #$00F3, #$00F5,
      #$007D, #$004A, #$004B, #$004C, #$004D, #$004E, #$004F, #$0050,
      #$0051, #$0052, #$00B9, #$00FB, #$00FC, #$00F9, #$00FA, #$00FF,
      #$005C, #$00F7, #$0053, #$0054, #$0055, #$0056, #$0057, #$0058,
      #$0059, #$005A, #$00B2, #$00D4, #$00D6, #$00D2, #$00D3, #$00D5,
      #$0030, #$0031, #$0032, #$0033, #$0034, #$0035, #$0036, #$0037,
      #$0038, #$0039, #$00B3, #$00DB, #$00DC, #$00D9, #$00DA, #$009F);



{                                                                              }
{ Windows-37                                                                   }
{                                                                              }
function TWindows37Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  Result := CP37Map[P];
end;

function TWindows37Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromMap(Ch, CP37Map, 'Windows-37');
end;



{                                                                              }
{ IBM037                                                                       }
{                                                                              }
function TIBM037Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  Result := CP37Map[P];
end;

function TIBM037Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromMap(Ch, CP37Map, 'IBM037');
end;



{                                                                              }
{ IBM038                                                                       }
{                                                                              }
const
  IBM038Map : AnsiCharMap = (
      #$0000, #$0001, #$0002, #$0003, #$009C, #$0009, #$0086, #$007F,
      #$0097, #$008D, #$008E, #$000B, #$000C, #$000D, #$000E, #$000F,
      #$0010, #$0011, #$0012, #$0013, #$009D, #$0085, #$0008, #$0087,
      #$0018, #$0019, #$0092, #$008F, #$001C, #$001D, #$001E, #$001F,
      #$0080, #$0081, #$0082, #$0083, #$0084, #$000A, #$0017, #$001B,
      #$0088, #$0089, #$008A, #$008B, #$008C, #$0005, #$0006, #$0007,
      #$0090, #$0091, #$0016, #$0093, #$0094, #$0095, #$0096, #$0004,
      #$0098, #$0099, #$009A, #$009B, #$0014, #$0015, #$009E, #$001A,
      #$0020, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF,
      #$FFFF, #$FFFF, #$005B, #$002E, #$003C, #$0028, #$002B, #$0021,
      #$0026, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF,
      #$FFFF, #$FFFF, #$005D, #$0024, #$002A, #$0029, #$003B, #$005E,
      #$002D, #$002F, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF,
      #$FFFF, #$FFFF, #$00A6, #$002C, #$0025, #$005F, #$003E, #$003F,
      #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF,
      #$FFFF, #$0060, #$003A, #$0023, #$0040, #$0027, #$003D, #$0022,
      #$FFFF, #$0061, #$0062, #$0063, #$0064, #$0065, #$0066, #$0067,
      #$0068, #$0069, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF,
      #$FFFF, #$006A, #$006B, #$006C, #$006D, #$006E, #$006F, #$0070,
      #$0071, #$0072, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF,
      #$FFFF, #$007E, #$0073, #$0074, #$0075, #$0076, #$0077, #$0078,
      #$0079, #$007A, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF,
      #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF,
      #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF,
      #$007B, #$0041, #$0042, #$0043, #$0044, #$0045, #$0046, #$0047,
      #$0048, #$0049, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF,
      #$007D, #$004A, #$004B, #$004C, #$004D, #$004E, #$004F, #$0050,
      #$0051, #$0052, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF,
      #$005C, #$FFFF, #$0053, #$0054, #$0055, #$0056, #$0057, #$0058,
      #$0059, #$005A, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF,
      #$0030, #$0031, #$0032, #$0033, #$0034, #$0035, #$0036, #$0037,
      #$0038, #$0039, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$009F);

function TIBM038Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  Result := IBM038Map[P];
end;

function TIBM038Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromMap(Ch, IBM038Map, 'IBM038');
end;



{                                                                              }
{ IBM256                                                                       }
{                                                                              }
const
  IBM256Map : AnsiCharMap = (
      #$0000, #$0001, #$0002, #$0003, #$009C, #$0009, #$0086, #$007F,
      #$0097, #$008D, #$008E, #$000B, #$000C, #$000D, #$000E, #$000F,
      #$0010, #$0011, #$0012, #$0013, #$009D, #$0085, #$0008, #$0087,
      #$0018, #$0019, #$0092, #$008F, #$001C, #$001D, #$001E, #$001F,
      #$0080, #$0081, #$0082, #$0083, #$0084, #$000A, #$0017, #$001B,
      #$0088, #$0089, #$008A, #$008B, #$008C, #$0005, #$0006, #$0007,
      #$0090, #$0091, #$0016, #$0093, #$0094, #$0095, #$0096, #$0004,
      #$0098, #$0099, #$009A, #$009B, #$0014, #$0015, #$009E, #$001A,
      #$0020, #$00A0, #$00E2, #$00E4, #$00E0, #$00E1, #$00E3, #$00E5,
      #$00E7, #$00F1, #$005B, #$002E, #$003C, #$0028, #$002B, #$0021,
      #$0026, #$00E9, #$00EA, #$00EB, #$00E8, #$00ED, #$00EE, #$00EF,
      #$00EC, #$00DF, #$005D, #$0024, #$002A, #$0029, #$003B, #$005E,
      #$002D, #$002F, #$00C2, #$00C4, #$00C0, #$00C1, #$00C3, #$00C5,
      #$00C7, #$00D1, #$00A6, #$002C, #$0025, #$005F, #$003E, #$003F,
      #$00F8, #$00C9, #$00CA, #$00CB, #$00C8, #$00CD, #$00CE, #$00CF,
      #$00CC, #$0060, #$003A, #$0023, #$0040, #$0027, #$003D, #$0022,
      #$00D8, #$0061, #$0062, #$0063, #$0064, #$0065, #$0066, #$0067,
      #$0068, #$0069, #$00AB, #$00BB, #$00F0, #$00FD, #$00FE, #$00B1,
      #$00B0, #$006A, #$006B, #$006C, #$006D, #$006E, #$006F, #$0070,
      #$0071, #$0072, #$00AA, #$00BA, #$00E6, #$00B8, #$00C6, #$00A4,
      #$00B5, #$007E, #$0073, #$0074, #$0075, #$0076, #$0077, #$0078,
      #$0079, #$007A, #$00A1, #$00BF, #$00D0, #$00DD, #$00DE, #$00AE,
      #$00A2, #$00A3, #$00A5, #$20A7, #$0192, #$00A7, #$00B6, #$00BC,
      #$00BD, #$00BE, #$00AC, #$007C, #$203E, #$00A8, #$00B4, #$2017,
      #$007B, #$0041, #$0042, #$0043, #$0044, #$0045, #$0046, #$0047,
      #$0048, #$0049, #$00AD, #$00F4, #$00F6, #$00F2, #$00F3, #$00F5,
      #$007D, #$004A, #$004B, #$004C, #$004D, #$004E, #$004F, #$0050,
      #$0051, #$0052, #$00B9, #$00FB, #$00FC, #$00F9, #$00FA, #$00FF,
      #$005C, #$2003, #$0053, #$0054, #$0055, #$0056, #$0057, #$0058,
      #$0059, #$005A, #$00B2, #$00D4, #$00D6, #$00D2, #$00D3, #$00D5,
      #$0030, #$0031, #$0032, #$0033, #$0034, #$0035, #$0036, #$0037,
      #$0038, #$0039, #$00B3, #$00DB, #$00DC, #$00D9, #$00DA, #$009F);

function TIBM256Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  Result := IBM256Map[P];
end;

function TIBM256Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromMap(Ch, IBM256Map, 'IBM256');
end;



{                                                                              }
{ IBM273                                                                       }
{                                                                              }
const
  IBM273Map : AnsiCharMap = (
      #$0000, #$0001, #$0002, #$0003, #$009C, #$0009, #$0086, #$007F,
      #$0097, #$008D, #$008E, #$000B, #$000C, #$000D, #$000E, #$000F,
      #$0010, #$0011, #$0012, #$0013, #$009D, #$0085, #$0008, #$0087,
      #$0018, #$0019, #$0092, #$008F, #$001C, #$001D, #$001E, #$001F,
      #$0080, #$0081, #$0082, #$0083, #$0084, #$000A, #$0017, #$001B,
      #$0088, #$0089, #$008A, #$008B, #$008C, #$0005, #$0006, #$0007,
      #$0090, #$0091, #$0016, #$0093, #$0094, #$0095, #$0096, #$0004,
      #$0098, #$0099, #$009A, #$009B, #$0014, #$0015, #$009E, #$001A,
      #$0020, #$00A0, #$00E2, #$007B, #$00E0, #$00E1, #$00E3, #$00E5,
      #$00E7, #$00F1, #$00C4, #$002E, #$003C, #$0028, #$002B, #$0021,
      #$0026, #$00E9, #$00EA, #$00EB, #$00E8, #$00ED, #$00EE, #$00EF,
      #$00EC, #$007E, #$00DC, #$0024, #$002A, #$0029, #$003B, #$005E,
      #$002D, #$002F, #$00C2, #$005B, #$00C0, #$00C1, #$00C3, #$00C5,
      #$00C7, #$00D1, #$00F6, #$002C, #$0025, #$005F, #$003E, #$003F,
      #$00F8, #$00C9, #$00CA, #$00CB, #$00C8, #$00CD, #$00CE, #$00CF,
      #$00CC, #$0060, #$003A, #$0023, #$00A7, #$0027, #$003D, #$0022,
      #$00D8, #$0061, #$0062, #$0063, #$0064, #$0065, #$0066, #$0067,
      #$0068, #$0069, #$00AB, #$00BB, #$00F0, #$00FD, #$00FE, #$00B1,
      #$00B0, #$006A, #$006B, #$006C, #$006D, #$006E, #$006F, #$0070,
      #$0071, #$0072, #$00AA, #$00BA, #$00E6, #$00B8, #$00C6, #$00A4,
      #$00B5, #$00DF, #$0073, #$0074, #$0075, #$0076, #$0077, #$0078,
      #$0079, #$007A, #$00A1, #$00BF, #$00D0, #$00DD, #$00DE, #$00AE,
      #$00A2, #$00A3, #$00A5, #$00B7, #$00A9, #$0040, #$00B6, #$00BC,
      #$00BD, #$00BE, #$00AC, #$007C, #$203E, #$00A8, #$00B4, #$00D7,
      #$00E4, #$0041, #$0042, #$0043, #$0044, #$0045, #$0046, #$0047,
      #$0048, #$0049, #$00AD, #$00F4, #$00A6, #$00F2, #$00F3, #$00F5,
      #$00FC, #$004A, #$004B, #$004C, #$004D, #$004E, #$004F, #$0050,
      #$0051, #$0052, #$00B9, #$00FB, #$007D, #$00F9, #$00FA, #$00FF,
      #$00D6, #$00F7, #$0053, #$0054, #$0055, #$0056, #$0057, #$0058,
      #$0059, #$005A, #$00B2, #$00D4, #$005C, #$00D2, #$00D3, #$00D5,
      #$0030, #$0031, #$0032, #$0033, #$0034, #$0035, #$0036, #$0037,
      #$0038, #$0039, #$00B3, #$00DB, #$005D, #$00D9, #$00DA, #$009F);

function TIBM273Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  Result := IBM273Map[P];
end;

function TIBM273Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromMap(Ch, IBM273Map, 'IBM273');
end;



{                                                                              }
{ IBM274                                                                       }
{                                                                              }
const
  IBM274Map : AnsiCharMap = (
      #$0000, #$0001, #$0002, #$0003, #$009C, #$0009, #$0086, #$007F,
      #$0097, #$008D, #$008E, #$000B, #$000C, #$000D, #$000E, #$000F,
      #$0010, #$0011, #$0012, #$0013, #$009D, #$0085, #$0008, #$0087,
      #$0018, #$0019, #$0092, #$008F, #$001C, #$001D, #$001E, #$001F,
      #$0080, #$0081, #$0082, #$0083, #$0084, #$000A, #$0017, #$001B,
      #$0088, #$0089, #$008A, #$008B, #$008C, #$0005, #$0006, #$0007,
      #$0090, #$0091, #$0016, #$0093, #$0094, #$0095, #$0096, #$0004,
      #$0098, #$0099, #$009A, #$009B, #$0014, #$0015, #$009E, #$001A,
      #$0020, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF,
      #$FFFF, #$FFFF, #$005B, #$002E, #$003C, #$0028, #$002B, #$0021, 
      #$0026, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF,
      #$FFFF, #$FFFF, #$005D, #$0024, #$002A, #$0029, #$003B, #$005E,
      #$002D, #$002F, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF,
      #$FFFF, #$FFFF, #$00F9, #$002C, #$0025, #$005F, #$003E, #$003F,
      #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF,
      #$FFFF, #$0060, #$003A, #$0023, #$00E0, #$0027, #$003D, #$0022,
      #$FFFF, #$0061, #$0062, #$0063, #$0064, #$0065, #$0066, #$0067,
      #$0068, #$0069, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF,
      #$FFFF, #$006A, #$006B, #$006C, #$006D, #$006E, #$006F, #$0070,
      #$0071, #$0072, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF,
      #$FFFF, #$00A8, #$0073, #$0074, #$0075, #$0076, #$0077, #$0078,
      #$0079, #$007A, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF,
      #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF,
      #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF,
      #$00E9, #$0041, #$0042, #$0043, #$0044, #$0045, #$0046, #$0047,
      #$0048, #$0049, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF,
      #$00E8, #$004A, #$004B, #$004C, #$004D, #$004E, #$004F, #$0050,
      #$0051, #$0052, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF,
      #$00E7, #$FFFF, #$0053, #$0054, #$0055, #$0056, #$0057, #$0058,
      #$0059, #$005A, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF,
      #$0030, #$0031, #$0032, #$0033, #$0034, #$0035, #$0036, #$0037,
      #$0038, #$0039, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$009F);

function TIBM274Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  Result := IBM274Map[P];
end;

function TIBM274Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromMap(Ch, IBM274Map, 'IBM274');
end;



{                                                                              }
{ IBM275                                                                       }
{                                                                              }
const
  IBM275Map : AnsiCharMap = (
      #$0000, #$0001, #$0002, #$0003, #$009C, #$0009, #$0086, #$007F,
      #$0097, #$008D, #$008E, #$000B, #$000C, #$000D, #$000E, #$000F,
      #$0010, #$0011, #$0012, #$0013, #$009D, #$0085, #$0008, #$0087,
      #$0018, #$0019, #$0092, #$008F, #$001C, #$001D, #$001E, #$001F,
      #$0080, #$0081, #$0082, #$0083, #$0084, #$000A, #$0017, #$001B,
      #$0088, #$0089, #$008A, #$008B, #$008C, #$0005, #$0006, #$0007,
      #$0090, #$0091, #$0016, #$0093, #$0094, #$0095, #$0096, #$0004,
      #$0098, #$0099, #$009A, #$009B, #$0014, #$0015, #$009E, #$001A,
      #$0020, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF,
      #$FFFF, #$FFFF, #$00C9, #$002E, #$003C, #$0028, #$002B, #$0021,
      #$0026, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF,
      #$FFFF, #$FFFF, #$0024, #$00C7, #$002A, #$0029, #$003B, #$005E,
      #$002D, #$002F, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF,
      #$FFFF, #$FFFF, #$00E7, #$002C, #$0025, #$005F, #$003E, #$003F,
      #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF,
      #$FFFF, #$00E3, #$003A, #$00D5, #$00C3, #$0027, #$003D, #$0022,
      #$FFFF, #$0061, #$0062, #$0063, #$0064, #$0065, #$0066, #$0067,
      #$0068, #$0069, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF,
      #$FFFF, #$006A, #$006B, #$006C, #$006D, #$006E, #$006F, #$0070,
      #$0071, #$0072, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF,
      #$FFFF, #$007E, #$0073, #$0074, #$0075, #$0076, #$0077, #$0078,
      #$0079, #$007A, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF,
      #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF,
      #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF,
      #$00F5, #$0041, #$0042, #$0043, #$0044, #$0045, #$0046, #$0047,
      #$0048, #$0049, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF,
      #$00E9, #$004A, #$004B, #$004C, #$004D, #$004E, #$004F, #$0050,
      #$0051, #$0052, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF,
      #$005C, #$FFFF, #$0053, #$0054, #$0055, #$0056, #$0057, #$0058,
      #$0059, #$005A, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF,
      #$0030, #$0031, #$0032, #$0033, #$0034, #$0035, #$0036, #$0037,
      #$0038, #$0039, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$009F);

function TIBM275Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  Result := IBM275Map[P];
end;

function TIBM275Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromMap(Ch, IBM275Map, 'IBM275');
end;



{                                                                              }
{ IBM277                                                                       }
{                                                                              }
const
  IBM277Map : AnsiCharMap = (
      #$0000, #$0001, #$0002, #$0003, #$009C, #$0009, #$0086, #$007F,
      #$0097, #$008D, #$008E, #$000B, #$000C, #$000D, #$000E, #$000F,
      #$0010, #$0011, #$0012, #$0013, #$009D, #$0085, #$0008, #$0087,
      #$0018, #$0019, #$0092, #$008F, #$001C, #$001D, #$001E, #$001F,
      #$0080, #$0081, #$0082, #$0083, #$0084, #$000A, #$0017, #$001B,
      #$0088, #$0089, #$008A, #$008B, #$008C, #$0005, #$0006, #$0007,
      #$0090, #$0091, #$0016, #$0093, #$0094, #$0095, #$0096, #$0004,
      #$0098, #$0099, #$009A, #$009B, #$0014, #$0015, #$009E, #$001A,
      #$0020, #$00A0, #$00E2, #$00E4, #$00E0, #$00E1, #$00E3, #$007D,
      #$00E7, #$00F1, #$0023, #$002E, #$003C, #$0028, #$002B, #$0021,
      #$0026, #$00E9, #$00EA, #$00EB, #$00E8, #$00ED, #$00EE, #$00EF, 
      #$00EC, #$00DF, #$00A4, #$00C5, #$002A, #$0029, #$003B, #$005E,
      #$002D, #$002F, #$00C2, #$00C4, #$00C0, #$00C1, #$00C3, #$0024, 
      #$00C7, #$00D1, #$00F8, #$002C, #$0025, #$005F, #$003E, #$003F,
      #$00A6, #$00C9, #$00CA, #$00CB, #$00C8, #$00CD, #$00CE, #$00CF,
      #$00CC, #$0060, #$003A, #$00C6, #$00D8, #$0027, #$003D, #$0022, 
      #$0040, #$0061, #$0062, #$0063, #$0064, #$0065, #$0066, #$0067,
      #$0068, #$0069, #$00AB, #$00BB, #$00F0, #$00FD, #$00FE, #$00B1, 
      #$00B0, #$006A, #$006B, #$006C, #$006D, #$006E, #$006F, #$0070, 
      #$0071, #$0072, #$00AA, #$00BA, #$007B, #$00B8, #$005B, #$005D,
      #$00B5, #$00FC, #$0073, #$0074, #$0075, #$0076, #$0077, #$0078,
      #$0079, #$007A, #$00A1, #$00BF, #$00D0, #$00DD, #$00DE, #$00AE,
      #$00A2, #$00A3, #$00A5, #$00B7, #$00A9, #$00A7, #$00B6, #$00BC,
      #$00BD, #$00BE, #$00AC, #$007C, #$203E, #$00A8, #$00B4, #$00D7, 
      #$00E6, #$0041, #$0042, #$0043, #$0044, #$0045, #$0046, #$0047,
      #$0048, #$0049, #$00AD, #$00F4, #$00F6, #$00F2, #$00F3, #$00F5, 
      #$00E5, #$004A, #$004B, #$004C, #$004D, #$004E, #$004F, #$0050, 
      #$0051, #$0052, #$00B9, #$00FB, #$007E, #$00F9, #$00FA, #$00FF,
      #$005C, #$00F7, #$0053, #$0054, #$0055, #$0056, #$0057, #$0058, 
      #$0059, #$005A, #$00B2, #$00D4, #$00D6, #$00D2, #$00D3, #$00D5,
      #$0030, #$0031, #$0032, #$0033, #$0034, #$0035, #$0036, #$0037, 
      #$0038, #$0039, #$00B3, #$00DB, #$00DC, #$00D9, #$00DA, #$009F);

function TIBM277Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  Result := IBM277Map[P];
end;

function TIBM277Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromMap(Ch, IBM277Map, 'IBM277');
end;



{                                                                              }
{ IBM278                                                                       }
{                                                                              }
const
  IBM278Map : AnsiCharMap = (
      #$0000, #$0001, #$0002, #$0003, #$009C, #$0009, #$0086, #$007F,
      #$0097, #$008D, #$008E, #$000B, #$000C, #$000D, #$000E, #$000F,
      #$0010, #$0011, #$0012, #$0013, #$009D, #$0085, #$0008, #$0087,
      #$0018, #$0019, #$0092, #$008F, #$001C, #$001D, #$001E, #$001F,
      #$0080, #$0081, #$0082, #$0083, #$0084, #$000A, #$0017, #$001B,
      #$0088, #$0089, #$008A, #$008B, #$008C, #$0005, #$0006, #$0007, 
      #$0090, #$0091, #$0016, #$0093, #$0094, #$0095, #$0096, #$0004,
      #$0098, #$0099, #$009A, #$009B, #$0014, #$0015, #$009E, #$001A, 
      #$0020, #$00A0, #$00E2, #$007B, #$00E0, #$00E1, #$00E3, #$007D,
      #$00E7, #$00F1, #$00A7, #$002E, #$003C, #$0028, #$002B, #$0021, 
      #$0026, #$0060, #$00EA, #$00EB, #$00E8, #$00ED, #$00EE, #$00EF,
      #$00EC, #$00DF, #$00A4, #$00C5, #$002A, #$0029, #$003B, #$005E, 
      #$002D, #$002F, #$00C2, #$0023, #$00C0, #$00C1, #$00C3, #$0024, 
      #$00C7, #$00D1, #$00F6, #$002C, #$0025, #$005F, #$003E, #$003F,
      #$00F8, #$00C9, #$00CA, #$00CB, #$00C8, #$00CD, #$00CE, #$00CF, 
      #$00CC, #$00E9, #$003A, #$00C4, #$00D6, #$0027, #$003D, #$0022,
      #$00D8, #$0061, #$0062, #$0063, #$0064, #$0065, #$0066, #$0067,
      #$0068, #$0069, #$00AB, #$00BB, #$00F0, #$00FD, #$00FE, #$00B1,
      #$00B0, #$006A, #$006B, #$006C, #$006D, #$006E, #$006F, #$0070, 
      #$0071, #$0072, #$00AA, #$00BA, #$00E6, #$00B8, #$00C6, #$005D, 
      #$00B5, #$00FC, #$0073, #$0074, #$0075, #$0076, #$0077, #$0078,
      #$0079, #$007A, #$00A1, #$00BF, #$00D0, #$00DD, #$00DE, #$00AE,
      #$00A2, #$00A3, #$00A5, #$00B7, #$00A9, #$005B, #$00B6, #$00BC,
      #$00BD, #$00BE, #$00AC, #$007C, #$203E, #$00A8, #$00B4, #$00D7, 
      #$00E4, #$0041, #$0042, #$0043, #$0044, #$0045, #$0046, #$0047, 
      #$0048, #$0049, #$00AD, #$00F4, #$00A6, #$00F2, #$00F3, #$00F5, 
      #$00E5, #$004A, #$004B, #$004C, #$004D, #$004E, #$004F, #$0050, 
      #$0051, #$0052, #$00B9, #$00FB, #$007E, #$00F9, #$00FA, #$00FF,
      #$005C, #$00F7, #$0053, #$0054, #$0055, #$0056, #$0057, #$0058, 
      #$0059, #$005A, #$00B2, #$00D4, #$0040, #$00D2, #$00D3, #$00D5, 
      #$0030, #$0031, #$0032, #$0033, #$0034, #$0035, #$0036, #$0037, 
      #$0038, #$0039, #$00B3, #$00DB, #$00DC, #$00D9, #$00DA, #$009F);

function TIBM278Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  Result := IBM278Map[P];
end;

function TIBM278Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromMap(Ch, IBM278Map, 'IBM278');
end;



{                                                                              }
{ IBM280                                                                       }
{                                                                              }
const
  IBM280Map : AnsiCharMap = (
      #$0000, #$0001, #$0002, #$0003, #$009C, #$0009, #$0086, #$007F,
      #$0097, #$008D, #$008E, #$000B, #$000C, #$000D, #$000E, #$000F,
      #$0010, #$0011, #$0012, #$0013, #$009D, #$0085, #$0008, #$0087,
      #$0018, #$0019, #$0092, #$008F, #$001C, #$001D, #$001E, #$001F, 
      #$0080, #$0081, #$0082, #$0083, #$0084, #$000A, #$0017, #$001B,
      #$0088, #$0089, #$008A, #$008B, #$008C, #$0005, #$0006, #$0007, 
      #$0090, #$0091, #$0016, #$0093, #$0094, #$0095, #$0096, #$0004, 
      #$0098, #$0099, #$009A, #$009B, #$0014, #$0015, #$009E, #$001A, 
      #$0020, #$00A0, #$00E2, #$00E4, #$007B, #$00E1, #$00E3, #$00E5, 
      #$005C, #$00F1, #$00B0, #$002E, #$003C, #$0028, #$002B, #$0021,
      #$0026, #$005D, #$00EA, #$00EB, #$007D, #$00ED, #$00EE, #$00EF, 
      #$007E, #$00DF, #$00E9, #$0024, #$002A, #$0029, #$003B, #$005E,
      #$002D, #$002F, #$00C2, #$00C4, #$00C0, #$00C1, #$00C3, #$00C5,
      #$00C7, #$00D1, #$00F2, #$002C, #$0025, #$005F, #$003E, #$003F,
      #$00F8, #$00C9, #$00CA, #$00CB, #$00C8, #$00CD, #$00CE, #$00CF,
      #$00CC, #$00F9, #$003A, #$00A3, #$00A7, #$0027, #$003D, #$0022, 
      #$00D8, #$0061, #$0062, #$0063, #$0064, #$0065, #$0066, #$0067,
      #$0068, #$0069, #$00AB, #$00BB, #$00F0, #$00FD, #$00FE, #$00B1, 
      #$005B, #$006A, #$006B, #$006C, #$006D, #$006E, #$006F, #$0070,
      #$0071, #$0072, #$00AA, #$00BA, #$00E6, #$00B8, #$00C6, #$00A4,
      #$00B5, #$00EC, #$0073, #$0074, #$0075, #$0076, #$0077, #$0078,
      #$0079, #$007A, #$00A1, #$00BF, #$00D0, #$00DD, #$00DE, #$00AE,
      #$00A2, #$0023, #$00A5, #$00B7, #$00A9, #$0040, #$00B6, #$00BC,
      #$00BD, #$00BE, #$00AC, #$007C, #$203E, #$00A8, #$00B4, #$00D7,
      #$00E0, #$0041, #$0042, #$0043, #$0044, #$0045, #$0046, #$0047,
      #$0048, #$0049, #$00AD, #$00F4, #$00F6, #$00A6, #$00F3, #$00F5,
      #$00E8, #$004A, #$004B, #$004C, #$004D, #$004E, #$004F, #$0050,
      #$0051, #$0052, #$00B9, #$00FB, #$00FC, #$0060, #$00FA, #$00FF,
      #$00E7, #$00F7, #$0053, #$0054, #$0055, #$0056, #$0057, #$0058,
      #$0059, #$005A, #$00B2, #$00D4, #$00D6, #$00D2, #$00D3, #$00D5,
      #$0030, #$0031, #$0032, #$0033, #$0034, #$0035, #$0036, #$0037,
      #$0038, #$0039, #$00B3, #$00DB, #$00DC, #$00D9, #$00DA, #$009F);

function TIBM280Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  Result := IBM280Map[P];
end;

function TIBM280Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromMap(Ch, IBM280Map, 'IBM280');
end;



{                                                                              }
{ IBM281                                                                       }
{   Similar to IBM038.                                                         }
{                                                                              }
function TIBM281Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  case Ord(P) of
    $4A : Result := #$00A3;
    $4F : Result := #$007C;
    $5A : Result := #$0021;
    $5B : Result := #$00A5;
    $5F : Result := #$00AC;
    $A1 : Result := #$203E;
    $E0 : Result := #$0024;
  else
    Result := IBM038Map[P];
  end;
end;

function TIBM281Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  case Ord(Ch) of
    $00A3 : Result := AnsiChar($4A);
    $007C : Result := AnsiChar($4F);
    $0021 : Result := AnsiChar($5A);
    $00A5 : Result := AnsiChar($5B);
    $00AC : Result := AnsiChar($5F);
    $203E : Result := AnsiChar($A1);
    $0024 : Result := AnsiChar($E0);
  else
    Result := CharFromMap(Ch, IBM038Map, 'IBM281');
  end;
end;



{                                                                              }
{ IBM284                                                                       }
{                                                                              }
const
  IBM284Map : AnsiCharMap = (
      #$0000, #$0001, #$0002, #$0003, #$009C, #$0009, #$0086, #$007F,
      #$0097, #$008D, #$008E, #$000B, #$000C, #$000D, #$000E, #$000F,
      #$0010, #$0011, #$0012, #$0013, #$009D, #$0085, #$0008, #$0087,
      #$0018, #$0019, #$0092, #$008F, #$001C, #$001D, #$001E, #$001F,
      #$0080, #$0081, #$0082, #$0083, #$0084, #$000A, #$0017, #$001B,
      #$0088, #$0089, #$008A, #$008B, #$008C, #$0005, #$0006, #$0007,
      #$0090, #$0091, #$0016, #$0093, #$0094, #$0095, #$0096, #$0004,
      #$0098, #$0099, #$009A, #$009B, #$0014, #$0015, #$009E, #$001A,
      #$0020, #$00A0, #$00E2, #$00E4, #$00E0, #$00E1, #$00E3, #$00E5,
      #$00E7, #$00A6, #$005B, #$002E, #$003C, #$0028, #$002B, #$007C,
      #$0026, #$00E9, #$00EA, #$00EB, #$00E8, #$00ED, #$00EE, #$00EF,
      #$00EC, #$00DF, #$005D, #$0024, #$002A, #$0029, #$003B, #$00AC,
      #$002D, #$002F, #$00C2, #$00C4, #$00C0, #$00C1, #$00C3, #$00C5, 
      #$00C7, #$0023, #$00F1, #$002C, #$0025, #$005F, #$003E, #$003F, 
      #$00F8, #$00C9, #$00CA, #$00CB, #$00C8, #$00CD, #$00CE, #$00CF,
      #$00CC, #$0060, #$003A, #$00D1, #$0040, #$0027, #$003D, #$0022,
      #$00D8, #$0061, #$0062, #$0063, #$0064, #$0065, #$0066, #$0067, 
      #$0068, #$0069, #$00AB, #$00BB, #$00F0, #$00FD, #$00FE, #$00B1,
      #$00B0, #$006A, #$006B, #$006C, #$006D, #$006E, #$006F, #$0070, 
      #$0071, #$0072, #$00AA, #$00BA, #$00E6, #$00B8, #$00C6, #$00A4,
      #$00B5, #$00A8, #$0073, #$0074, #$0075, #$0076, #$0077, #$0078,
      #$0079, #$007A, #$00A1, #$00BF, #$00D0, #$00DD, #$00DE, #$00AE, 
      #$00A2, #$00A3, #$00A5, #$00B7, #$00A9, #$00A7, #$00B6, #$00BC,
      #$00BD, #$00BE, #$005E, #$0021, #$203E, #$007E, #$00B4, #$00D7,
      #$007B, #$0041, #$0042, #$0043, #$0044, #$0045, #$0046, #$0047, 
      #$0048, #$0049, #$00AD, #$00F4, #$00F6, #$00F2, #$00F3, #$00F5, 
      #$007D, #$004A, #$004B, #$004C, #$004D, #$004E, #$004F, #$0050, 
      #$0051, #$0052, #$00B9, #$00FB, #$00FC, #$00F9, #$00FA, #$00FF,
      #$005C, #$00F7, #$0053, #$0054, #$0055, #$0056, #$0057, #$0058,
      #$0059, #$005A, #$00B2, #$00D4, #$00D6, #$00D2, #$00D3, #$00D5,
      #$0030, #$0031, #$0032, #$0033, #$0034, #$0035, #$0036, #$0037,
      #$0038, #$0039, #$00B3, #$00DB, #$00DC, #$00D9, #$00DA, #$009F);

function TIBM284Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  Result := IBM284Map[P];
end;

function TIBM284Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromMap(Ch, IBM284Map, 'IBM284');
end;



{                                                                              }
{ IBM285                                                                       }
{                                                                              }
const
  IBM285Map : AnsiCharMap = (
      #$0000, #$0001, #$0002, #$0003, #$009C, #$0009, #$0086, #$007F,
      #$0097, #$008D, #$008E, #$000B, #$000C, #$000D, #$000E, #$000F,
      #$0010, #$0011, #$0012, #$0013, #$009D, #$0085, #$0008, #$0087,
      #$0018, #$0019, #$0092, #$008F, #$001C, #$001D, #$001E, #$001F,
      #$0080, #$0081, #$0082, #$0083, #$0084, #$000A, #$0017, #$001B,
      #$0088, #$0089, #$008A, #$008B, #$008C, #$0005, #$0006, #$0007,
      #$0090, #$0091, #$0016, #$0093, #$0094, #$0095, #$0096, #$0004,
      #$0098, #$0099, #$009A, #$009B, #$0014, #$0015, #$009E, #$001A,
      #$0020, #$00A0, #$00E2, #$00E4, #$00E0, #$00E1, #$00E3, #$00E5,
      #$00E7, #$00F1, #$0024, #$002E, #$003C, #$0028, #$002B, #$007C,
      #$0026, #$00E9, #$00EA, #$00EB, #$00E8, #$00ED, #$00EE, #$00EF,
      #$00EC, #$00DF, #$0021, #$00A3, #$002A, #$0029, #$003B, #$00AC,
      #$002D, #$002F, #$00C2, #$00C4, #$00C0, #$00C1, #$00C3, #$00C5,
      #$00C7, #$00D1, #$00A6, #$002C, #$0025, #$005F, #$003E, #$003F,
      #$00F8, #$00C9, #$00CA, #$00CB, #$00C8, #$00CD, #$00CE, #$00CF,
      #$00CC, #$0060, #$003A, #$0023, #$0040, #$0027, #$003D, #$0022, 
      #$00D8, #$0061, #$0062, #$0063, #$0064, #$0065, #$0066, #$0067, 
      #$0068, #$0069, #$00AB, #$00BB, #$00F0, #$00FD, #$00FE, #$00B1,
      #$00B0, #$006A, #$006B, #$006C, #$006D, #$006E, #$006F, #$0070, 
      #$0071, #$0072, #$00AA, #$00BA, #$00E6, #$00B8, #$00C6, #$00A4, 
      #$00B5, #$203E, #$0073, #$0074, #$0075, #$0076, #$0077, #$0078,
      #$0079, #$007A, #$00A1, #$00BF, #$00D0, #$00DD, #$00DE, #$00AE,
      #$00A2, #$005B, #$00A5, #$00B7, #$00A9, #$00A7, #$00B6, #$00BC,
      #$00BD, #$00BE, #$005E, #$005D, #$007E, #$00A8, #$00B4, #$00D7,
      #$007B, #$0041, #$0042, #$0043, #$0044, #$0045, #$0046, #$0047, 
      #$0048, #$0049, #$00AD, #$00F4, #$00F6, #$00F2, #$00F3, #$00F5,
      #$007D, #$004A, #$004B, #$004C, #$004D, #$004E, #$004F, #$0050, 
      #$0051, #$0052, #$00B9, #$00FB, #$00FC, #$00F9, #$00FA, #$00FF,
      #$005C, #$00F7, #$0053, #$0054, #$0055, #$0056, #$0057, #$0058, 
      #$0059, #$005A, #$00B2, #$00D4, #$00D6, #$00D2, #$00D3, #$00D5, 
      #$0030, #$0031, #$0032, #$0033, #$0034, #$0035, #$0036, #$0037, 
      #$0038, #$0039, #$00B3, #$00DB, #$00DC, #$00D9, #$00DA, #$009F);

function TIBM285Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  Result := IBM285Map[P];
end;

function TIBM285Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromMap(Ch, IBM285Map, 'IBM285');
end;



{                                                                              }
{ IBM290                                                                       }
{                                                                              }
const
  IBM290Map : AnsiCharMap = (
      #$0000, #$0001, #$0002, #$0003, #$009C, #$0009, #$0086, #$007F, 
      #$0097, #$008D, #$008E, #$000B, #$000C, #$000D, #$000E, #$000F,
      #$0010, #$0011, #$0012, #$0013, #$009D, #$0085, #$0008, #$0087,
      #$0018, #$0019, #$0092, #$008F, #$001C, #$001D, #$001E, #$001F,
      #$0080, #$0081, #$0082, #$0083, #$0084, #$000A, #$0017, #$001B,
      #$0088, #$0089, #$008A, #$008B, #$008C, #$0005, #$0006, #$0007, 
      #$0090, #$0091, #$0016, #$0093, #$0094, #$0095, #$0096, #$0004,
      #$0098, #$0099, #$009A, #$009B, #$0014, #$0015, #$009E, #$001A, 
      #$0020, #$3002, #$300C, #$300D, #$3001, #$30FB, #$30F2, #$30A1, 
      #$30A3, #$30A5, #$00A3, #$002E, #$003C, #$0028, #$002B, #$007C,
      #$0026, #$30A7, #$30A9, #$30E3, #$30E5, #$30E7, #$30C3, #$FFFF, 
      #$30FC, #$FFFF, #$0021, #$00A5, #$002A, #$0029, #$003B, #$00AC,
      #$002D, #$002F, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF,
      #$FFFF, #$FFFF, #$00A6, #$002C, #$0025, #$005F, #$003E, #$003F, 
      #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, 
      #$FFFF, #$0060, #$003A, #$0023, #$0040, #$0027, #$003D, #$0022, 
      #$FFFF, #$30A2, #$30A4, #$30A6, #$30A8, #$30AA, #$30AB, #$30AD,
      #$30AF, #$30B1, #$30B3, #$FFFF, #$30B5, #$30B7, #$30B9, #$30BB,
      #$30BD, #$30BF, #$30C1, #$30C4, #$30C6, #$30C8, #$30CA, #$30CB,
      #$30CC, #$30CD, #$30CE, #$FFFF, #$FFFF, #$30CF, #$30D2, #$30D5, 
      #$FFFF, #$203E, #$30D8, #$30DB, #$30DE, #$30DF, #$30E0, #$30E1,
      #$30E2, #$30E4, #$30E6, #$FFFF, #$30E8, #$30E9, #$30EA, #$30EB, 
      #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF,
      #$FFFF, #$FFFF, #$30EC, #$30ED, #$30EF, #$30F3, #$309B, #$309C, 
      #$FFFF, #$0041, #$0042, #$0043, #$0044, #$0045, #$0046, #$0047, 
      #$0048, #$0049, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF,
      #$FFFF, #$004A, #$004B, #$004C, #$004D, #$004E, #$004F, #$0050,
      #$0051, #$0052, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, 
      #$0024, #$FFFF, #$0053, #$0054, #$0055, #$0056, #$0057, #$0058, 
      #$0059, #$005A, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF,
      #$0030, #$0031, #$0032, #$0033, #$0034, #$0035, #$0036, #$0037, 
      #$0038, #$0039, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$009F);

function TIBM290Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  Result := IBM290Map[P];
end;

function TIBM290Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromMap(Ch, IBM290Map, 'IBM290');
end;



{                                                                              }
{ IBM297                                                                       }
{                                                                              }
const
  IBM297Map : AnsiCharMap = (
      #$0000, #$0001, #$0002, #$0003, #$009C, #$0009, #$0086, #$007F,
      #$0097, #$008D, #$008E, #$000B, #$000C, #$000D, #$000E, #$000F,
      #$0010, #$0011, #$0012, #$0013, #$009D, #$0085, #$0008, #$0087,
      #$0018, #$0019, #$0092, #$008F, #$001C, #$001D, #$001E, #$001F,
      #$0080, #$0081, #$0082, #$0083, #$0084, #$000A, #$0017, #$001B,
      #$0088, #$0089, #$008A, #$008B, #$008C, #$0005, #$0006, #$0007,
      #$0090, #$0091, #$0016, #$0093, #$0094, #$0095, #$0096, #$0004,
      #$0098, #$0099, #$009A, #$009B, #$0014, #$0015, #$009E, #$001A,
      #$0020, #$00A0, #$00E2, #$00E4, #$0040, #$00E1, #$00E3, #$00E5,
      #$005C, #$00F1, #$00B0, #$002E, #$003C, #$0028, #$002B, #$0021,
      #$0026, #$007B, #$00EA, #$00EB, #$007D, #$00ED, #$00EE, #$00EF,
      #$00EC, #$00DF, #$00A7, #$0024, #$002A, #$0029, #$003B, #$005E, 
      #$002D, #$002F, #$00C2, #$00C4, #$00C0, #$00C1, #$00C3, #$00C5,
      #$00C7, #$00D1, #$00F9, #$002C, #$0025, #$005F, #$003E, #$003F, 
      #$00F8, #$00C9, #$00CA, #$00CB, #$00C8, #$00CD, #$00CE, #$00CF,
      #$00CC, #$00B5, #$003A, #$00A3, #$00E0, #$0027, #$003D, #$0022,
      #$00D8, #$0061, #$0062, #$0063, #$0064, #$0065, #$0066, #$0067, 
      #$0068, #$0069, #$00AB, #$00BB, #$00F0, #$00FD, #$00FE, #$00B1, 
      #$005B, #$006A, #$006B, #$006C, #$006D, #$006E, #$006F, #$0070,
      #$0071, #$0072, #$00AA, #$00BA, #$00E6, #$00B8, #$00C6, #$00A4, 
      #$0060, #$00A8, #$0073, #$0074, #$0075, #$0076, #$0077, #$0078, 
      #$0079, #$007A, #$00A1, #$00BF, #$00D0, #$00DD, #$00DE, #$00AE,
      #$00A2, #$0023, #$00A5, #$00B7, #$00A9, #$005D, #$00B6, #$00BC,
      #$00BD, #$00BE, #$00AC, #$007C, #$203E, #$007E, #$00B4, #$00D7, 
      #$00E9, #$0041, #$0042, #$0043, #$0044, #$0045, #$0046, #$0047,
      #$0048, #$0049, #$00AD, #$00F4, #$00F6, #$00F2, #$00F3, #$00F5,
      #$00E8, #$004A, #$004B, #$004C, #$004D, #$004E, #$004F, #$0050,
      #$0051, #$0052, #$00B9, #$00FB, #$00FC, #$00A6, #$00FA, #$00FF, 
      #$00E7, #$00F7, #$0053, #$0054, #$0055, #$0056, #$0057, #$0058,
      #$0059, #$005A, #$00B2, #$00D4, #$00D6, #$00D2, #$00D3, #$00D5,
      #$0030, #$0031, #$0032, #$0033, #$0034, #$0035, #$0036, #$0037, 
      #$0038, #$0039, #$00B3, #$00DB, #$00DC, #$00D9, #$00DA, #$009F);

function TIBM297Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  Result := IBM297Map[P];
end;

function TIBM297Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromMap(Ch, IBM297Map, 'IBM297');
end;



{                                                                              }
{ IBM420                                                                       }
{                                                                              }
const
  IBM420Map : AnsiCharMap = (
      #$0000, #$0001, #$0002, #$0003, #$009C, #$0009, #$0086, #$007F,
      #$0097, #$008D, #$008E, #$000B, #$000C, #$000D, #$000E, #$000F,
      #$0010, #$0011, #$0012, #$0013, #$009D, #$0085, #$0008, #$0087, 
      #$0018, #$0019, #$0092, #$008F, #$001C, #$001D, #$001E, #$001F, 
      #$0080, #$0081, #$0082, #$0083, #$0084, #$000A, #$0017, #$001B,
      #$0088, #$0089, #$008A, #$008B, #$008C, #$0005, #$0006, #$0007,
      #$0090, #$0091, #$0016, #$0093, #$0094, #$0095, #$0096, #$0004, 
      #$0098, #$0099, #$009A, #$009B, #$0014, #$0015, #$009E, #$001A, 
      #$0020, #$00A0, #$0651, #$FE7D, #$0640, #$FFFF, #$0621, #$0622, 
      #$FE82, #$0623, #$00A2, #$002E, #$003C, #$0028, #$002B, #$007C, 
      #$0026, #$FE84, #$0624, #$FFFF, #$FFFF, #$0626, #$0627, #$FE8E,
      #$0628, #$FE91, #$0021, #$0024, #$002A, #$0029, #$003B, #$00AC, 
      #$002D, #$002F, #$0629, #$062A, #$FE97, #$062B, #$FE9B, #$062C, 
      #$FE9F, #$062D, #$00A6, #$002C, #$0025, #$005F, #$003E, #$003F,
      #$FEA3, #$062E, #$FEA7, #$062F, #$0630, #$0631, #$0632, #$0633, 
      #$FEB3, #$060C, #$003A, #$0023, #$0040, #$0027, #$003D, #$0022,
      #$0634, #$0061, #$0062, #$0063, #$0064, #$0065, #$0066, #$0067, 
      #$0068, #$0069, #$FEB7, #$0635, #$FEBB, #$0636, #$FEBF, #$0637, 
      #$0638, #$006A, #$006B, #$006C, #$006D, #$006E, #$006F, #$0070,
      #$0071, #$0072, #$0639, #$FECA, #$FECB, #$FECC, #$063A, #$FECE, 
      #$FECF, #$00F7, #$0073, #$0074, #$0075, #$0076, #$0077, #$0078,
      #$0079, #$007A, #$FED0, #$0641, #$FED3, #$0642, #$FED7, #$0643, 
      #$FEDB, #$0644, #$FEF5, #$FEF6, #$FEF7, #$FEF8, #$FFFF, #$FFFF,
      #$FEFB, #$FEFC, #$FEDF, #$0645, #$FEE3, #$0646, #$FEE7, #$0647, 
      #$061B, #$0041, #$0042, #$0043, #$0044, #$0045, #$0046, #$0047,
      #$0048, #$0049, #$00AD, #$FEEB, #$FFFF, #$FEEC, #$FFFF, #$0648,
      #$061F, #$004A, #$004B, #$004C, #$004D, #$004E, #$004F, #$0050,
      #$0051, #$0052, #$0649, #$FEF0, #$064A, #$FEF2, #$FEF3, #$0660,
      #$00D7, #$FFFF, #$0053, #$0054, #$0055, #$0056, #$0057, #$0058,
      #$0059, #$005A, #$0661, #$0662, #$FFFF, #$0663, #$0664, #$0665,
      #$0030, #$0031, #$0032, #$0033, #$0034, #$0035, #$0036, #$0037,
      #$0038, #$0039, #$FFFF, #$0666, #$0667, #$0668, #$0669, #$009F);

function TIBM420Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  Result := IBM420Map[P];
end;

function TIBM420Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromMap(Ch, IBM420Map, 'IBM420');
end;



{                                                                              }
{ IBM423                                                                       }
{                                                                              }
const
  IBM423Map : AnsiCharMap = (
      #$0000, #$0001, #$0002, #$0003, #$009C, #$0009, #$0086, #$007F, 
      #$0097, #$008D, #$008E, #$000B, #$000C, #$000D, #$000E, #$000F, 
      #$0010, #$0011, #$0012, #$0013, #$009D, #$0085, #$0008, #$0087, 
      #$0018, #$0019, #$0092, #$008F, #$001C, #$001D, #$001E, #$001F,
      #$0080, #$0081, #$0082, #$0083, #$0084, #$000A, #$0017, #$001B,
      #$0088, #$0089, #$008A, #$008B, #$008C, #$0005, #$0006, #$0007, 
      #$0090, #$0091, #$0016, #$0093, #$0094, #$0095, #$0096, #$0004, 
      #$0098, #$0099, #$009A, #$009B, #$0014, #$0015, #$009E, #$001A,
      #$0020, #$0391, #$0392, #$0393, #$0394, #$0395, #$0396, #$0397, 
      #$0398, #$0399, #$005B, #$002E, #$003C, #$0028, #$002B, #$0021, 
      #$0026, #$039A, #$039B, #$039C, #$039D, #$039E, #$039F, #$03A0, 
      #$03A1, #$03A3, #$005D, #$0024, #$002A, #$0029, #$003B, #$005E, 
      #$002D, #$002F, #$03A4, #$03A5, #$03A6, #$03A7, #$03A8, #$03A9,
      #$FFFF, #$FFFF, #$FFFF, #$002C, #$0025, #$005F, #$003E, #$003F,
      #$FFFF, #$0386, #$0388, #$0389, #$FFFF, #$038A, #$038C, #$038E, 
      #$038F, #$0060, #$003A, #$00A3, #$00A7, #$0027, #$003D, #$0022,
      #$00C4, #$0061, #$0062, #$0063, #$0064, #$0065, #$0066, #$0067,
      #$0068, #$0069, #$03B1, #$03B2, #$03B3, #$03B4, #$03B5, #$03B6,
      #$00D6, #$006A, #$006B, #$006C, #$006D, #$006E, #$006F, #$0070, 
      #$0071, #$0072, #$03B7, #$03B8, #$03B9, #$03BA, #$03BB, #$03BC,
      #$00DC, #$00A8, #$0073, #$0074, #$0075, #$0076, #$0077, #$0078,
      #$0079, #$007A, #$03BD, #$03BE, #$03BF, #$03C0, #$03C1, #$03C2,
      #$FFFF, #$03AC, #$03AD, #$03AE, #$03CA, #$03AF, #$03CC, #$03CD,
      #$03CB, #$03CE, #$03C3, #$03C4, #$03C5, #$03C6, #$03C7, #$03C8, 
      #$00B8, #$0041, #$0042, #$0043, #$0044, #$0045, #$0046, #$0047,
      #$0048, #$0049, #$FFFF, #$03C9, #$00C2, #$00E0, #$00E4, #$00EA, 
      #$00B4, #$004A, #$004B, #$004C, #$004D, #$004E, #$004F, #$0050,
      #$0051, #$0052, #$00B1, #$00E9, #$00E8, #$00EB, #$00EE, #$00EF,
      #$00B0, #$FFFF, #$0053, #$0054, #$0055, #$0056, #$0057, #$0058,
      #$0059, #$005A, #$00BD, #$00F6, #$00F4, #$00FB, #$00F9, #$00FC, 
      #$0030, #$0031, #$0032, #$0033, #$0034, #$0035, #$0036, #$0037, 
      #$0038, #$0039, #$00FF, #$00E7, #$00C7, #$FFFF, #$FFFF, #$009F);

function TIBM423Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  Result := IBM423Map[P];
end;

function TIBM423Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromMap(Ch, IBM423Map, 'IBM423');
end;



{                                                                              }
{ IBM424                                                                       }
{                                                                              }
const
  IBM424Map : AnsiCharMap = (
      #$0000, #$0001, #$0002, #$0003, #$009C, #$0009, #$0086, #$007F,
      #$0097, #$008D, #$008E, #$000B, #$000C, #$000D, #$000E, #$000F,
      #$0010, #$0011, #$0012, #$0013, #$009D, #$0085, #$0008, #$0087,
      #$0018, #$0019, #$0092, #$008F, #$001C, #$001D, #$001E, #$001F,
      #$0080, #$0081, #$0082, #$0083, #$0084, #$000A, #$0017, #$001B,
      #$0088, #$0089, #$008A, #$008B, #$008C, #$0005, #$0006, #$0007,
      #$0090, #$0091, #$0016, #$0093, #$0094, #$0095, #$0096, #$0004,
      #$0098, #$0099, #$009A, #$009B, #$0014, #$0015, #$009E, #$001A,
      #$0020, #$05D0, #$05D1, #$05D2, #$05D3, #$05D4, #$05D5, #$05D6,
      #$05D7, #$05D8, #$00A2, #$002E, #$003C, #$0028, #$002B, #$007C,
      #$0026, #$05D9, #$05DA, #$05DB, #$05DC, #$05DD, #$05DE, #$05DF,
      #$05E0, #$05E1, #$0021, #$0024, #$002A, #$0029, #$003B, #$00AC,
      #$002D, #$002F, #$05E2, #$05E3, #$05E4, #$05E5, #$05E6, #$05E7,
      #$05E8, #$05E9, #$00A6, #$002C, #$0025, #$005F, #$003E, #$003F,
      #$FFFF, #$05EA, #$FFFF, #$FFFF, #$00A0, #$FFFF, #$FFFF, #$FFFF,
      #$21D4, #$0060, #$003A, #$0023, #$0040, #$0027, #$003D, #$0022,
      #$FFFF, #$0061, #$0062, #$0063, #$0064, #$0065, #$0066, #$0067,
      #$0068, #$0069, #$00AB, #$00BB, #$FFFF, #$FFFF, #$FFFF, #$FFFF,
      #$00B0, #$006A, #$006B, #$006C, #$006D, #$006E, #$006F, #$0070,
      #$0071, #$0072, #$FFFF, #$FFFF, #$FFFF, #$00B8, #$FFFF, #$00A4,
      #$00B5, #$007E, #$0073, #$0074, #$0075, #$0076, #$0077, #$0078,
      #$0079, #$007A, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$00AE,
      #$005E, #$00A3, #$00A5, #$00B7, #$00A9, #$00A7, #$00B6, #$00BC,
      #$00BD, #$00BE, #$005B, #$005D, #$203E, #$00A8, #$00B4, #$00D7,
      #$007B, #$0041, #$0042, #$0043, #$0044, #$0045, #$0046, #$0047, 
      #$0048, #$0049, #$00AD, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF,
      #$007D, #$004A, #$004B, #$004C, #$004D, #$004E, #$004F, #$0050, 
      #$0051, #$0052, #$00B9, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF,
      #$005C, #$00F7, #$0053, #$0054, #$0055, #$0056, #$0057, #$0058,
      #$0059, #$005A, #$00B2, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF,
      #$0030, #$0031, #$0032, #$0033, #$0034, #$0035, #$0036, #$0037,
      #$0038, #$0039, #$00B3, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$009F);

function TIBM424Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  Result := IBM424Map[P];
end;

function TIBM424Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromMap(Ch, IBM424Map, 'IBM424');
end;



{                                                                              }
{ CP437 - DOSLatinUS                                                           }
{   Original IBM PC encoding                                                   }
{   Map shared by IBM437 and Windows-437.                                      }
{                                                                              }
const
  CP437Aliases = 3;
  CP437Alias : array[0..CP437Aliases - 1] of String = (
      'IBM437', 'cp437', 'DOSLatinUS');

const
  CP437Map : AnsiCharHighMap = (
      #$00C7, #$00FC, #$00E9, #$00E2, #$00E4, #$00E0, #$00E5, #$00E7,
      #$00EA, #$00EB, #$00E8, #$00EF, #$00EE, #$00EC, #$00C4, #$00C5,
      #$00C9, #$00E6, #$00C6, #$00F4, #$00F6, #$00F2, #$00FB, #$00F9,
      #$00FF, #$00D6, #$00DC, #$00A2, #$00A3, #$00A5, #$20A7, #$0192,
      #$00E1, #$00ED, #$00F3, #$00FA, #$00F1, #$00D1, #$00AA, #$00BA,
      #$00BF, #$2310, #$00AC, #$00BD, #$00BC, #$00A1, #$00AB, #$00BB,
      #$2591, #$2592, #$2593, #$2502, #$2524, #$2561, #$2562, #$2556,
      #$2555, #$2563, #$2551, #$2557, #$255D, #$255C, #$255B, #$2510,
      #$2514, #$2534, #$252C, #$251C, #$2500, #$253C, #$255E, #$255F,
      #$255A, #$2554, #$2569, #$2566, #$2560, #$2550, #$256C, #$2567,
      #$2568, #$2564, #$2565, #$2559, #$2558, #$2552, #$2553, #$256B,
      #$256A, #$2518, #$250C, #$2588, #$2584, #$258C, #$2590, #$2580,
      #$03B1, #$00DF, #$0393, #$03C0, #$03A3, #$03C3, #$00B5, #$03C4,
      #$03A6, #$0398, #$03A9, #$03B4, #$221E, #$03C6, #$03B5, #$2229,
      #$2261, #$00B1, #$2265, #$2264, #$2320, #$2321, #$00F7, #$2248,
      #$00B0, #$2219, #$00B7, #$221A, #$207F, #$00B2, #$25A0, #$00A0);



{                                                                              }
{ Windows-437                                                                  }
{                                                                              }
function TWindows437Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  if Ord(P) < $80 then
    Result := WideChar(P)
  else
    Result := CP437Map[Ord(P)];
end;

function TWindows437Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromHighMap(Ch, CP437Map, 'Windows-437');
end;



{                                                                              }
{ IBM437                                                                       }
{                                                                              }
class function TIBM437Codec.GetAliasCount: Integer;
begin
  Result := CP437Aliases;
end;

class function TIBM437Codec.GetAlias(const Idx: Integer): String;
begin
  if Idx < 0 then
    raise EUnicodeCodecException.Create(SAliasIndexOutOfRange);
  if Idx >= GetAliasCount then
    raise EUnicodeCodecException.Create(SAliasIndexOutOfRange);
  Result := CP437Alias[Idx];
end;

function TIBM437Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  if Ord(P) < $80 then
    Result := WideChar(P)
  else
    Result := CP437Map[Ord(P)];
end;

function TIBM437Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromHighMap(Ch, CP437Map, 'IBM437');
end;



{                                                                              }
{ CP500                                                                        }
{   Map shared by IBM500 and Windows-500.                                      }
{                                                                              }
const
  CP500Map : AnsiCharMap = (
      #$0000, #$0001, #$0002, #$0003, #$009C, #$0009, #$0086, #$007F,
      #$0097, #$008D, #$008E, #$000B, #$000C, #$000D, #$000E, #$000F,
      #$0010, #$0011, #$0012, #$0013, #$009D, #$0085, #$0008, #$0087,
      #$0018, #$0019, #$0092, #$008F, #$001C, #$001D, #$001E, #$001F,
      #$0080, #$0081, #$0082, #$0083, #$0084, #$000A, #$0017, #$001B,
      #$0088, #$0089, #$008A, #$008B, #$008C, #$0005, #$0006, #$0007,
      #$0090, #$0091, #$0016, #$0093, #$0094, #$0095, #$0096, #$0004, 
      #$0098, #$0099, #$009A, #$009B, #$0014, #$0015, #$009E, #$001A,
      #$0020, #$00A0, #$00E2, #$00E4, #$00E0, #$00E1, #$00E3, #$00E5, 
      #$00E7, #$00F1, #$005B, #$002E, #$003C, #$0028, #$002B, #$0021,
      #$0026, #$00E9, #$00EA, #$00EB, #$00E8, #$00ED, #$00EE, #$00EF,
      #$00EC, #$00DF, #$005D, #$0024, #$002A, #$0029, #$003B, #$005E,
      #$002D, #$002F, #$00C2, #$00C4, #$00C0, #$00C1, #$00C3, #$00C5,
      #$00C7, #$00D1, #$00A6, #$002C, #$0025, #$005F, #$003E, #$003F, 
      #$00F8, #$00C9, #$00CA, #$00CB, #$00C8, #$00CD, #$00CE, #$00CF, 
      #$00CC, #$0060, #$003A, #$0023, #$0040, #$0027, #$003D, #$0022, 
      #$00D8, #$0061, #$0062, #$0063, #$0064, #$0065, #$0066, #$0067,
      #$0068, #$0069, #$00AB, #$00BB, #$00F0, #$00FD, #$00FE, #$00B1,
      #$00B0, #$006A, #$006B, #$006C, #$006D, #$006E, #$006F, #$0070, 
      #$0071, #$0072, #$00AA, #$00BA, #$00E6, #$00B8, #$00C6, #$00A4,
      #$00B5, #$007E, #$0073, #$0074, #$0075, #$0076, #$0077, #$0078, 
      #$0079, #$007A, #$00A1, #$00BF, #$00D0, #$00DD, #$00DE, #$00AE, 
      #$00A2, #$00A3, #$00A5, #$00B7, #$00A9, #$00A7, #$00B6, #$00BC,
      #$00BD, #$00BE, #$00AC, #$007C, #$00AF, #$00A8, #$00B4, #$00D7, 
      #$007B, #$0041, #$0042, #$0043, #$0044, #$0045, #$0046, #$0047,
      #$0048, #$0049, #$00AD, #$00F4, #$00F6, #$00F2, #$00F3, #$00F5,
      #$007D, #$004A, #$004B, #$004C, #$004D, #$004E, #$004F, #$0050,
      #$0051, #$0052, #$00B9, #$00FB, #$00FC, #$00F9, #$00FA, #$00FF,
      #$005C, #$00F7, #$0053, #$0054, #$0055, #$0056, #$0057, #$0058,
      #$0059, #$005A, #$00B2, #$00D4, #$00D6, #$00D2, #$00D3, #$00D5,
      #$0030, #$0031, #$0032, #$0033, #$0034, #$0035, #$0036, #$0037,
      #$0038, #$0039, #$00B3, #$00DB, #$00DC, #$00D9, #$00DA, #$009F);



{                                                                              }
{ Windows-500                                                                  }
{                                                                              }
function TWindows500Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  Result := CP500Map[P];
end;

function TWindows500Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromMap(Ch, CP500Map, 'Windows-500');
end;



{                                                                              }
{ IBM500                                                                       }
{                                                                              }
function TIBM500Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  Result := CP500Map[P];
end;

function TIBM500Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromMap(Ch, CP500Map, 'IBM500');
end;



{                                                                              }
{ Windows-708                                                                  }
{                                                                              }
const
  Windows708Map : AnsiCharHighMap = (
      #$2502, #$2524, #$00E9, #$00E2, #$2561, #$00E0, #$2562, #$00E7,
      #$00EA, #$00EB, #$00E8, #$00EF, #$00EE, #$2556, #$2555, #$2563,
      #$2551, #$2557, #$255D, #$00F4, #$255C, #$255B, #$00FB, #$00F9,
      #$2510, #$2514, #$009A, #$009B, #$009C, #$009D, #$009E, #$009F,
      #$F8C1, #$2534, #$252C, #$251C, #$00A4, #$2500, #$253C, #$255E,
      #$255F, #$255A, #$2554, #$2569, #$060C, #$2566, #$00AB, #$00BB,
      #$2591, #$2592, #$2593, #$2560, #$2550, #$256C, #$2567, #$2568,
      #$2564, #$2565, #$2559, #$061B, #$2558, #$2552, #$2553, #$061F,
      #$256B, #$0621, #$0622, #$0623, #$0624, #$0625, #$0626, #$0627,
      #$0628, #$0629, #$062A, #$062B, #$062C, #$062D, #$062E, #$062F,
      #$0630, #$0631, #$0632, #$0633, #$0634, #$0635, #$0636, #$0637,
      #$0638, #$0639, #$063A, #$2588, #$2584, #$258C, #$2590, #$2580,
      #$0640, #$0641, #$0642, #$0643, #$0644, #$0645, #$0646, #$0647,
      #$0648, #$0649, #$064A, #$064B, #$064C, #$064D, #$064E, #$064F,
      #$0650, #$0651, #$0652, #$F8C2, #$F8C3, #$F8C4, #$F8C5, #$F8C6,
      #$F8C7, #$256A, #$2518, #$250C, #$00B5, #$00A3, #$25A0, #$00A0);

function TWindows708Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  if Ord(P) < $80 then
    Result := WideChar(P)
  else
    Result := Windows708Map[Ord(P)];
end;

function TWindows708Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromHighMap(Ch, Windows708Map, 'Windows-708');
end;



{                                                                              }
{ Windows-737                                                                  }
{                                                                              }
const
  Windows737Map : AnsiCharHighMap = (
      #$0391, #$0392, #$0393, #$0394, #$0395, #$0396, #$0397, #$0398,
      #$0399, #$039A, #$039B, #$039C, #$039D, #$039E, #$039F, #$03A0,
      #$03A1, #$03A3, #$03A4, #$03A5, #$03A6, #$03A7, #$03A8, #$03A9,
      #$03B1, #$03B2, #$03B3, #$03B4, #$03B5, #$03B6, #$03B7, #$03B8,
      #$03B9, #$03BA, #$03BB, #$03BC, #$03BD, #$03BE, #$03BF, #$03C0,
      #$03C1, #$03C3, #$03C2, #$03C4, #$03C5, #$03C6, #$03C7, #$03C8,
      #$2591, #$2592, #$2593, #$2502, #$2524, #$2561, #$2562, #$2556,
      #$2555, #$2563, #$2551, #$2557, #$255D, #$255C, #$255B, #$2510,
      #$2514, #$2534, #$252C, #$251C, #$2500, #$253C, #$255E, #$255F,
      #$255A, #$2554, #$2569, #$2566, #$2560, #$2550, #$256C, #$2567,
      #$2568, #$2564, #$2565, #$2559, #$2558, #$2552, #$2553, #$256B,
      #$256A, #$2518, #$250C, #$2588, #$2584, #$258C, #$2590, #$2580,
      #$03C9, #$03AC, #$03AD, #$03AE, #$03CA, #$03AF, #$03CC, #$03CD,
      #$03CB, #$03CE, #$0386, #$0388, #$0389, #$038A, #$038C, #$038E,
      #$038F, #$00B1, #$2265, #$2264, #$03AA, #$03AB, #$00F7, #$2248,
      #$00B0, #$2219, #$00B7, #$221A, #$207F, #$00B2, #$25A0, #$00A0);

function TWindows737Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  if Ord(P) < $80 then
    Result := WideChar(P)
  else
    Result := Windows737Map[Ord(P)];
end;

function TWindows737Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromHighMap(Ch, Windows737Map, 'Windows-737');
end;



{                                                                              }
{ Windows-775                                                                  }
{                                                                              }
const
  Windows775Map : AnsiCharHighMap = (
      #$0106, #$00FC, #$00E9, #$0101, #$00E4, #$0123, #$00E5, #$0107,
      #$0142, #$0113, #$0156, #$0157, #$012B, #$0179, #$00C4, #$00C5,
      #$00C9, #$00E6, #$00C6, #$014D, #$00F6, #$0122, #$00A2, #$015A,
      #$015B, #$00D6, #$00DC, #$00F8, #$00A3, #$00D8, #$00D7, #$00A4,
      #$0100, #$012A, #$00F3, #$017B, #$017C, #$017A, #$201D, #$00A6,
      #$00A9, #$00AE, #$00AC, #$00BD, #$00BC, #$0141, #$00AB, #$00BB,
      #$2591, #$2592, #$2593, #$2502, #$2524, #$0104, #$010C, #$0118,
      #$0116, #$2563, #$2551, #$2557, #$255D, #$012E, #$0160, #$2510,
      #$2514, #$2534, #$252C, #$251C, #$2500, #$253C, #$0172, #$016A,
      #$255A, #$2554, #$2569, #$2566, #$2560, #$2550, #$256C, #$017D, 
      #$0105, #$010D, #$0119, #$0117, #$012F, #$0161, #$0173, #$016B, 
      #$017E, #$2518, #$250C, #$2588, #$2584, #$258C, #$2590, #$2580,
      #$00D3, #$00DF, #$014C, #$0143, #$00F5, #$00D5, #$00B5, #$0144,
      #$0136, #$0137, #$013B, #$013C, #$0146, #$0112, #$0145, #$2019,
      #$00AD, #$00B1, #$201C, #$00BE, #$00B6, #$00A7, #$00F7, #$201E,
      #$00B0, #$2219, #$00B7, #$00B9, #$00B3, #$00B2, #$25A0, #$00A0);

function TWindows775Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  if Ord(P) < $80 then
    Result := WideChar(P)
  else
    Result := Windows775Map[Ord(P)];
end;

function TWindows775Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromHighMap(Ch, Windows775Map, 'Windows-775');
end;



{                                                                              }
{ CP850                                                                        }
{   Map shared by IBM850 and Windows-850.                                      }
{                                                                              }
const
  CP850Map : AnsiCharHighMap = (
      #$00C7, #$00FC, #$00E9, #$00E2, #$00E4, #$00E0, #$00E5, #$00E7,
      #$00EA, #$00EB, #$00E8, #$00EF, #$00EE, #$00EC, #$00C4, #$00C5,
      #$00C9, #$00E6, #$00C6, #$00F4, #$00F6, #$00F2, #$00FB, #$00F9,
      #$00FF, #$00D6, #$00DC, #$00F8, #$00A3, #$00D8, #$00D7, #$0192,
      #$00E1, #$00ED, #$00F3, #$00FA, #$00F1, #$00D1, #$00AA, #$00BA,
      #$00BF, #$00AE, #$00AC, #$00BD, #$00BC, #$00A1, #$00AB, #$00BB,
      #$2591, #$2592, #$2593, #$2502, #$2524, #$00C1, #$00C2, #$00C0,
      #$00A9, #$2563, #$2551, #$2557, #$255D, #$00A2, #$00A5, #$2510,
      #$2514, #$2534, #$252C, #$251C, #$2500, #$253C, #$00E3, #$00C3,
      #$255A, #$2554, #$2569, #$2566, #$2560, #$2550, #$256C, #$00A4,
      #$00F0, #$00D0, #$00CA, #$00CB, #$00C8, #$0131, #$00CD, #$00CE,
      #$00CF, #$2518, #$250C, #$2588, #$2584, #$00A6, #$00CC, #$2580,
      #$00D3, #$00DF, #$00D4, #$00D2, #$00F5, #$00D5, #$00B5, #$00FE,
      #$00DE, #$00DA, #$00DB, #$00D9, #$00FD, #$00DD, #$00AF, #$00B4,
      #$00AD, #$00B1, #$2017, #$00BE, #$00B6, #$00A7, #$00F7, #$00B8,
      #$00B0, #$00A8, #$00B7, #$00B9, #$00B3, #$00B2, #$25A0, #$00A0);



{                                                                              }
{ Windows-850                                                                  }
{                                                                              }
function TWindows850Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  if Ord(P) < $80 then
    Result := WideChar(P)
  else
    Result := CP850Map[Ord(P)];
end;

function TWindows850Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromHighMap(Ch, CP850Map, 'Windows-850');
end;



{                                                                              }
{ IBM850                                                                       }
{                                                                              }
function TIBM850Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  if Ord(P) < $80 then
    Result := WideChar(P)
  else
    Result := CP850Map[Ord(P)];
end;

function TIBM850Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromHighMap(Ch, CP850Map, 'IBM850');
end;



{                                                                              }
{ IBM851                                                                       }
{                                                                              }
const
  IBM851Map : AnsiCharHighMap = (
      #$00C7, #$00FC, #$00E9, #$00E2, #$00E4, #$00E0, #$0386, #$00E7,
      #$00EA, #$00EB, #$00E8, #$00EF, #$00EE, #$0388, #$00C4, #$0389,
      #$038A, #$FFFF, #$038C, #$00F4, #$00F6, #$038E, #$00FB, #$00F9,
      #$038F, #$00D6, #$00DC, #$03AC, #$00A3, #$03AD, #$03AE, #$03AF,
      #$03CA, #$0390, #$03CC, #$03CD, #$0391, #$0392, #$0393, #$0394,
      #$0395, #$0396, #$0397, #$00BD, #$0398, #$0399, #$00AB, #$00BB,
      #$2591, #$2592, #$2593, #$2502, #$2524, #$039A, #$039B, #$039D,
      #$039C, #$2563, #$2551, #$2557, #$255D, #$039E, #$039F, #$2510,
      #$2514, #$2534, #$252C, #$251C, #$2500, #$253C, #$03A0, #$03A1,
      #$255A, #$2554, #$2569, #$2566, #$2560, #$2550, #$256C, #$03A3,
      #$03A4, #$03A5, #$03A6, #$03A7, #$03A8, #$03A9, #$03B1, #$03B2,
      #$03B3, #$2518, #$250C, #$2588, #$2584, #$03B4, #$03B5, #$2580,
      #$03B6, #$03B7, #$03B8, #$03B9, #$03BA, #$03BB, #$03BC, #$03BD,
      #$03BE, #$03BF, #$03C0, #$03C1, #$03C3, #$03C2, #$03C4, #$00B4,
      #$00AD, #$00B1, #$03C5, #$03C6, #$03C7, #$00A7, #$03C8, #$02DB,
      #$00B0, #$00A8, #$03C9, #$03CB, #$03B0, #$03CE, #$25A0, #$00A0);

function TIBM851Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  if Ord(P) < $80 then
    Result := WideChar(P)
  else
    Result := IBM851Map[Ord(P)];
end;

function TIBM851Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromHighMap(Ch, IBM851Map, 'IBM851');
end;



{                                                                              }
{ CP852                                                                        }
{   Map shared by IBM852 and Windows-852.                                      }
{                                                                              }
const
  CP852Map : AnsiCharHighMap = (
      #$00C7, #$00FC, #$00E9, #$00E2, #$00E4, #$016F, #$0107, #$00E7,
      #$0142, #$00EB, #$0150, #$0151, #$00EE, #$0179, #$00C4, #$0106,
      #$00C9, #$0139, #$013A, #$00F4, #$00F6, #$013D, #$013E, #$015A,
      #$015B, #$00D6, #$00DC, #$0164, #$0165, #$0141, #$00D7, #$010D,
      #$00E1, #$00ED, #$00F3, #$00FA, #$0104, #$0105, #$017D, #$017E,
      #$0118, #$0119, #$00AC, #$017A, #$010C, #$015F, #$00AB, #$00BB,
      #$2591, #$2592, #$2593, #$2502, #$2524, #$00C1, #$00C2, #$011A,
      #$015E, #$2563, #$2551, #$2557, #$255D, #$017B, #$017C, #$2510,
      #$2514, #$2534, #$252C, #$251C, #$2500, #$253C, #$0102, #$0103,
      #$255A, #$2554, #$2569, #$2566, #$2560, #$2550, #$256C, #$00A4,
      #$0111, #$0110, #$010E, #$00CB, #$010F, #$0147, #$00CD, #$00CE,
      #$011B, #$2518, #$250C, #$2588, #$2584, #$0162, #$016E, #$2580,
      #$00D3, #$00DF, #$00D4, #$0143, #$0144, #$0148, #$0160, #$0161,
      #$0154, #$00DA, #$0155, #$0170, #$00FD, #$00DD, #$0163, #$00B4,
      #$00AD, #$02DD, #$02DB, #$02C7, #$02D8, #$00A7, #$00F7, #$00B8,
      #$00B0, #$00A8, #$02D9, #$0171, #$0158, #$0159, #$25A0, #$00A0);



{                                                                              }
{ Windows-852                                                                  }
{                                                                              }
function TWindows852Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  if Ord(P) < $80 then
    Result := WideChar(P)
  else
    Result := CP852Map[Ord(P)];
end;

function TWindows852Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromHighMap(Ch, CP852Map, 'Windows-852');
end;



{                                                                              }
{ IBM852                                                                       }
{                                                                              }
function TIBM852Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  if Ord(P) < $80 then
    Result := WideChar(P)
  else
    Result := CP852Map[Ord(P)];
end;

function TIBM852Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromHighMap(Ch, CP852Map, 'IBM852');
end;



{                                                                              }
{ CP855                                                                        }
{   Map shared by IBM855 and Windows-855.                                      }
{                                                                              }
const
  CP855Map : AnsiCharHighMap = (
      #$0452, #$0402, #$0453, #$0403, #$0451, #$0401, #$0454, #$0404,
      #$0455, #$0405, #$0456, #$0406, #$0457, #$0407, #$0458, #$0408,
      #$0459, #$0409, #$045A, #$040A, #$045B, #$040B, #$045C, #$040C,
      #$045E, #$040E, #$045F, #$040F, #$044E, #$042E, #$044A, #$042A,
      #$0430, #$0410, #$0431, #$0411, #$0446, #$0426, #$0434, #$0414,
      #$0435, #$0415, #$0444, #$0424, #$0433, #$0413, #$00AB, #$00BB,
      #$2591, #$2592, #$2593, #$2502, #$2524, #$0445, #$0425, #$0438,
      #$0418, #$2563, #$2551, #$2557, #$255D, #$0439, #$0419, #$2510,
      #$2514, #$2534, #$252C, #$251C, #$2500, #$253C, #$043A, #$041A,
      #$255A, #$2554, #$2569, #$2566, #$2560, #$2550, #$256C, #$00A4,
      #$043B, #$041B, #$043C, #$041C, #$043D, #$041D, #$043E, #$041E,
      #$043F, #$2518, #$250C, #$2588, #$2584, #$041F, #$044F, #$2580,
      #$042F, #$0440, #$0420, #$0441, #$0421, #$0442, #$0422, #$0443,
      #$0423, #$0436, #$0416, #$0432, #$0412, #$044C, #$042C, #$2116,
      #$00AD, #$044B, #$042B, #$0437, #$0417, #$0448, #$0428, #$044D,
      #$042D, #$0449, #$0429, #$0447, #$0427, #$00A7, #$25A0, #$00A0);



{                                                                              }
{ Windows-855                                                                  }
{                                                                              }
function TWindows855Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  if Ord(P) < $80 then
    Result := WideChar(P)
  else
    Result := CP855Map[Ord(P)];
end;

function TWindows855Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromHighMap(Ch, CP855Map, 'Windows-855');
end;



{                                                                              }
{ IBM855                                                                       }
{                                                                              }
function TIBM855Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  if Ord(P) < $80 then
    Result := WideChar(P)
  else
    Result := CP855Map[Ord(P)];
end;

function TIBM855Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromHighMap(Ch, CP855Map, 'IBM855');
end;



{                                                                              }
{ CP857                                                                        }
{   Map shared by IBM857 and Windows-857.                                      }
{                                                                              }
const
  CP857Map : AnsiCharHighMap = (
      #$00C7, #$00FC, #$00E9, #$00E2, #$00E4, #$00E0, #$00E5, #$00E7,
      #$00EA, #$00EB, #$00E8, #$00EF, #$00EE, #$0131, #$00C4, #$00C5,
      #$00C9, #$00E6, #$00C6, #$00F4, #$00F6, #$00F2, #$00FB, #$00F9,
      #$0130, #$00D6, #$00DC, #$00F8, #$00A3, #$00D8, #$015E, #$015F,
      #$00E1, #$00ED, #$00F3, #$00FA, #$00F1, #$00D1, #$011E, #$011F,
      #$00BF, #$00AE, #$00AC, #$00BD, #$00BC, #$00A1, #$00AB, #$00BB,
      #$2591, #$2592, #$2593, #$2502, #$2524, #$00C1, #$00C2, #$00C0,
      #$00A9, #$2563, #$2551, #$2557, #$255D, #$00A2, #$00A5, #$2510,
      #$2514, #$2534, #$252C, #$251C, #$2500, #$253C, #$00E3, #$00C3,
      #$255A, #$2554, #$2569, #$2566, #$2560, #$2550, #$256C, #$00A4,
      #$00BA, #$00AA, #$00CA, #$00CB, #$00C8, #$F8BB, #$00CD, #$00CE,
      #$00CF, #$2518, #$250C, #$2588, #$2584, #$00A6, #$00CC, #$2580,
      #$00D3, #$00DF, #$00D4, #$00D2, #$00F5, #$00D5, #$00B5, #$F8BC,
      #$00D7, #$00DA, #$00DB, #$00D9, #$00EC, #$00FF, #$00AF, #$00B4,
      #$00AD, #$00B1, #$F8BD, #$00BE, #$00B6, #$00A7, #$00F7, #$00B8,
      #$00B0, #$00A8, #$00B7, #$00B9, #$00B3, #$00B2, #$25A0, #$00A0);



{                                                                              }
{ Windows-857                                                                  }
{                                                                              }
function TWindows857Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  if Ord(P) < $80 then
    Result := WideChar(P)
  else
    Result := CP857Map[Ord(P)];
end;

function TWindows857Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromHighMap(Ch, CP857Map, 'Windows-857');
end;



{                                                                              }
{ IBM857                                                                       }
{                                                                              }
function TIBM857Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  case Ord(P) of
    $00..$7F      : Result := WideChar(P);
    $D5, $E7, $F2 : Result := #$FFFF;
  else
    Result := CP857Map[Ord(P)];
  end;
end;

function TIBM857Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromHighMap(Ch, CP857Map, 'IBM857');
  if Result in [AnsiChar($D5), AnsiChar($E7), AnsiChar($F2)] then
    raise EConvertError.CreateFmt(SCannotConvert, [Ord(Ch), 'IBM857']);
end;



{                                                                              }
{ Windows-858                                                                  }
{   Similar to CP850.                                                          }
{                                                                              }
function TWindows858Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  case Ord(P) of
    $00..$7F : Result := WideChar(P);
    $D5      : Result := #$20AC;
  else
    Result := CP850Map[Ord(P)];
  end;
end;

function TWindows858Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  case Ord(Ch) of
    $20AC : Result := AnsiChar($D5);
  else
    Result := CharFromHighMap(Ch, CP850Map, 'Windows-858');
  end;
end;



{                                                                              }
{ IBM860                                                                       }
{                                                                              }
const
  IBM860Map : AnsiCharHighMap = (
      #$00C7, #$00FC, #$00E9, #$00E2, #$00E3, #$00E0, #$00C1, #$00E7,
      #$00EA, #$00CA, #$00E8, #$00CD, #$00D4, #$00EC, #$00C3, #$00C2,
      #$00C9, #$00C0, #$00C8, #$00F4, #$00F5, #$00F2, #$00DA, #$00F9,
      #$00CC, #$00D5, #$00DC, #$00A2, #$00A3, #$00D9, #$20A7, #$00D3,
      #$00E1, #$00ED, #$00F3, #$00FA, #$00F1, #$00D1, #$00AA, #$00BA,
      #$00BF, #$00D2, #$00AC, #$00BD, #$00BC, #$00A1, #$00AB, #$00BB,
      #$2591, #$2592, #$2593, #$2502, #$2524, #$2561, #$2562, #$2556,
      #$2555, #$2563, #$2551, #$2557, #$255D, #$255C, #$255B, #$2510,
      #$2514, #$2534, #$252C, #$251C, #$2500, #$253C, #$255E, #$255F, 
      #$255A, #$2554, #$2569, #$2566, #$2560, #$2550, #$256C, #$2567, 
      #$2568, #$2564, #$2565, #$2559, #$2558, #$2552, #$2553, #$256B, 
      #$256A, #$2518, #$250C, #$2588, #$2584, #$258C, #$2590, #$2580,
      #$03B1, #$00DF, #$0393, #$03C0, #$03A3, #$03C3, #$00B5, #$03C4,
      #$03A6, #$0398, #$03A9, #$03B4, #$221E, #$03C6, #$03B5, #$2229, 
      #$2261, #$00B1, #$2265, #$2264, #$2320, #$2321, #$00F7, #$2248,
      #$00B0, #$2219, #$00B7, #$221A, #$207F, #$00B2, #$25A0, #$00A0);

function TIBM860Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  if Ord(P) < $80 then
    Result := WideChar(P)
  else
    Result := IBM860Map[Ord(P)];
end;

function TIBM860Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromHighMap(Ch, IBM860Map, 'IBM860');
end;



{                                                                              }
{ CP861                                                                        }
{   Map shared by IBM861 and Windows-861.                                      }
{                                                                              }
const
  CP861Map : AnsiCharHighMap = (
      #$00C7, #$00FC, #$00E9, #$00E2, #$00E4, #$00E0, #$00E5, #$00E7,
      #$00EA, #$00EB, #$00E8, #$00D0, #$00F0, #$00DE, #$00C4, #$00C5,
      #$00C9, #$00E6, #$00C6, #$00F4, #$00F6, #$00FE, #$00FB, #$00DD,
      #$00FD, #$00D6, #$00DC, #$00F8, #$00A3, #$00D8, #$20A7, #$0192,
      #$00E1, #$00ED, #$00F3, #$00FA, #$00C1, #$00CD, #$00D3, #$00DA,
      #$00BF, #$2310, #$00AC, #$00BD, #$00BC, #$00A1, #$00AB, #$00BB,
      #$2591, #$2592, #$2593, #$2502, #$2524, #$2561, #$2562, #$2556,
      #$2555, #$2563, #$2551, #$2557, #$255D, #$255C, #$255B, #$2510,
      #$2514, #$2534, #$252C, #$251C, #$2500, #$253C, #$255E, #$255F,
      #$255A, #$2554, #$2569, #$2566, #$2560, #$2550, #$256C, #$2567,
      #$2568, #$2564, #$2565, #$2559, #$2558, #$2552, #$2553, #$256B,
      #$256A, #$2518, #$250C, #$2588, #$2584, #$258C, #$2590, #$2580,
      #$03B1, #$00DF, #$0393, #$03C0, #$03A3, #$03C3, #$00B5, #$03C4,
      #$03A6, #$0398, #$03A9, #$03B4, #$221E, #$03C6, #$03B5, #$2229,
      #$2261, #$00B1, #$2265, #$2264, #$2320, #$2321, #$00F7, #$2248,
      #$00B0, #$2219, #$00B7, #$221A, #$207F, #$00B2, #$25A0, #$00A0);



{                                                                              }
{ Windows-861                                                                  }
{                                                                              }
function TWindows861Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  if Ord(P) < $80 then
    Result := WideChar(P)
  else
    Result := CP861Map[Ord(P)];
end;

function TWindows861Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromHighMap(Ch, CP861Map, 'Windows-861');
end;



{                                                                              }
{ IBM861                                                                       }
{                                                                              }
function TIBM861Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  if Ord(P) < $80 then
    Result := WideChar(P)
  else
    Result := CP861Map[Ord(P)]
end;

function TIBM861Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromHighMap(Ch, CP861Map, 'IBM861');
end;



{                                                                              }
{ CP862                                                                        }
{   Mapping shared by IBM-862 and Windows-862.                                 }
{                                                                              }
const
  CP862Map : AnsiCharHighMap = (
      #$05D0, #$05D1, #$05D2, #$05D3, #$05D4, #$05D5, #$05D6, #$05D7,
      #$05D8, #$05D9, #$05DA, #$05DB, #$05DC, #$05DD, #$05DE, #$05DF,
      #$05E0, #$05E1, #$05E2, #$05E3, #$05E4, #$05E5, #$05E6, #$05E7,
      #$05E8, #$05E9, #$05EA, #$00A2, #$00A3, #$00A5, #$20A7, #$0192,
      #$00E1, #$00ED, #$00F3, #$00FA, #$00F1, #$00D1, #$00AA, #$00BA,
      #$00BF, #$2310, #$00AC, #$00BD, #$00BC, #$00A1, #$00AB, #$00BB,
      #$2591, #$2592, #$2593, #$2502, #$2524, #$2561, #$2562, #$2556,
      #$2555, #$2563, #$2551, #$2557, #$255D, #$255C, #$255B, #$2510,
      #$2514, #$2534, #$252C, #$251C, #$2500, #$253C, #$255E, #$255F,
      #$255A, #$2554, #$2569, #$2566, #$2560, #$2550, #$256C, #$2567,
      #$2568, #$2564, #$2565, #$2559, #$2558, #$2552, #$2553, #$256B,
      #$256A, #$2518, #$250C, #$2588, #$2584, #$258C, #$2590, #$2580,
      #$03B1, #$00DF, #$0393, #$03C0, #$03A3, #$03C3, #$00B5, #$03C4,
      #$03A6, #$0398, #$03A9, #$03B4, #$221E, #$03C6, #$03B5, #$2229,
      #$2261, #$00B1, #$2265, #$2264, #$2320, #$2321, #$00F7, #$2248,
      #$00B0, #$2219, #$00B7, #$221A, #$207F, #$00B2, #$25A0, #$00A0);



{                                                                              }
{ Windows-862                                                                  }
{                                                                              }
function TWindows862Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  if Ord(P) < $80 then
    Result := WideChar(P)
  else
    Result := CP862Map[Ord(P)];
end;

function TWindows862Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromHighMap(Ch, CP862Map, 'Windows-862');
end;



{                                                                              }
{ IBM862                                                                       }
{                                                                              }
function TIBM862Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  if Ord(P) < $80 then
    Result := WideChar(P)
  else
    Result := CP862Map[Ord(P)];
end;

function TIBM862Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromHighMap(Ch, CP862Map, 'IBM862');
end;



{                                                                              }
{ CP863                                                                        }
{   Map shared by IBM863 and Windows-863.                                      }
{                                                                              }
const
  CP863Map : AnsiCharHighMap = (
      #$00C7, #$00FC, #$00E9, #$00E2, #$00C2, #$00E0, #$00B6, #$00E7,
      #$00EA, #$00EB, #$00E8, #$00EF, #$00EE, #$2017, #$00C0, #$00A7,
      #$00C9, #$00C8, #$00CA, #$00F4, #$00CB, #$00CF, #$00FB, #$00F9,
      #$00A4, #$00D4, #$00DC, #$00A2, #$00A3, #$00D9, #$00DB, #$0192,
      #$00A6, #$00B4, #$00F3, #$00FA, #$00A8, #$00B8, #$00B3, #$00AF,
      #$00CE, #$2310, #$00AC, #$00BD, #$00BC, #$00BE, #$00AB, #$00BB,
      #$2591, #$2592, #$2593, #$2502, #$2524, #$2561, #$2562, #$2556,
      #$2555, #$2563, #$2551, #$2557, #$255D, #$255C, #$255B, #$2510,
      #$2514, #$2534, #$252C, #$251C, #$2500, #$253C, #$255E, #$255F,
      #$255A, #$2554, #$2569, #$2566, #$2560, #$2550, #$256C, #$2567,
      #$2568, #$2564, #$2565, #$2559, #$2558, #$2552, #$2553, #$256B,
      #$256A, #$2518, #$250C, #$2588, #$2584, #$258C, #$2590, #$2580,
      #$03B1, #$00DF, #$0393, #$03C0, #$03A3, #$03C3, #$00B5, #$03C4,
      #$03A6, #$0398, #$03A9, #$03B4, #$221E, #$03C6, #$03B5, #$2229,
      #$2261, #$00B1, #$2265, #$2264, #$2320, #$2321, #$00F7, #$2248,
      #$00B0, #$2219, #$00B7, #$221A, #$207F, #$00B2, #$25A0, #$00A0);



{                                                                              }
{ Windows-863                                                                  }
{                                                                              }
function TWindows863Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  if Ord(P) < $80 then
    Result := WideChar(P)
  else
    Result := CP863Map[Ord(P)];
end;

function TWindows863Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromHighMap(Ch, CP863Map, 'Windows-863');
end;



{                                                                              }
{ IBM863                                                                       }
{                                                                              }
function TIBM863Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  if Ord(P) < $80 then
    Result := WideChar(P)
  else
    Result := CP863Map[Ord(P)];
end;

function TIBM863Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromHighMap(Ch, CP863Map, 'IBM863');
end;



{                                                                              }
{ Windows-864                                                                  }
{                                                                              }
const
  Windows864Map : AnsiCharHighMap = (
      #$00B0, #$00B7, #$2219, #$221A, #$2592, #$2500, #$2502, #$253C,
      #$2524, #$252C, #$251C, #$2534, #$2510, #$250C, #$2514, #$2518,
      #$03B2, #$221E, #$03C6, #$00B1, #$00BD, #$00BC, #$2248, #$00AB,
      #$00BB, #$FEF7, #$FEF8, #$009B, #$009C, #$FEFB, #$FEFC, #$009F,
      #$00A0, #$00AD, #$FE82, #$00A3, #$00A4, #$FE84, #$F8BE, #$F8BF,
      #$FE8E, #$FE8F, #$FE95, #$FE99, #$060C, #$FE9D, #$FEA1, #$FEA5,
      #$0660, #$0661, #$0662, #$0663, #$0664, #$0665, #$0666, #$0667,
      #$0668, #$0669, #$FED1, #$061B, #$FEB1, #$FEB5, #$FEB9, #$061F,
      #$00A2, #$FE80, #$FE81, #$FE83, #$FE85, #$FECA, #$FE8B, #$FE8D,
      #$FE91, #$FE93, #$FE97, #$FE9B, #$FE9F, #$FEA3, #$FEA7, #$FEA9,
      #$FEAB, #$FEAD, #$FEAF, #$FEB3, #$FEB7, #$FEBB, #$FEBF, #$FEC1,
      #$FEC5, #$FECB, #$FECF, #$00A6, #$00AC, #$00F7, #$00D7, #$FEC9,
      #$0640, #$FED3, #$FED7, #$FEDB, #$FEDF, #$FEE3, #$FEE7, #$FEEB,
      #$FEED, #$FEEF, #$FEF3, #$FEBD, #$FECC, #$FECE, #$FECD, #$FEE1,
      #$FE7D, #$0651, #$FEE5, #$FEE9, #$FEEC, #$FEF0, #$FEF2, #$FED0,
      #$FED5, #$FEF5, #$FEF6, #$FEDD, #$FED9, #$FEF1, #$25A0, #$F8C0);

function TWindows864Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  if Ord(P) < $80 then
    Result := WideChar(P)
  else
    Result := Windows864Map[Ord(P)];
end;

function TWindows864Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromHighMap(Ch, Windows864Map, 'Windows-864');
end;



{                                                                              }
{ IBM864                                                                       }
{                                                                              }
const
  IBM864Map : array[$25..$FF] of WideChar = (
      #$FFFF, #$0026, #$0027, #$0028, #$0029, #$002A, #$002B, #$002C,
      #$002D, #$002E, #$002F, #$0030, #$0031, #$0032, #$0033, #$0034,
      #$0035, #$0036, #$0037, #$0038, #$0039, #$003A, #$003B, #$003C,
      #$003D, #$003E, #$003F, #$0040, #$0041, #$0042, #$0043, #$0044,
      #$0045, #$0046, #$0047, #$0048, #$0049, #$004A, #$004B, #$004C, 
      #$004D, #$004E, #$004F, #$0050, #$0051, #$0052, #$0053, #$0054, 
      #$0055, #$0056, #$0057, #$0058, #$0059, #$005A, #$005B, #$005C, 
      #$005D, #$005E, #$005F, #$0060, #$0061, #$0062, #$0063, #$0064,
      #$0065, #$0066, #$0067, #$0068, #$0069, #$006A, #$006B, #$006C, 
      #$006D, #$006E, #$006F, #$0070, #$0071, #$0072, #$0073, #$0074, 
      #$0075, #$0076, #$0077, #$0078, #$0079, #$007A, #$007B, #$007C,
      #$007D, #$007E, #$007F, #$00B0, #$00B7, #$2219, #$221A, #$2592,
      #$2500, #$2502, #$253C, #$2524, #$252C, #$251C, #$2534, #$2510,
      #$250C, #$2514, #$2518, #$03B2, #$221E, #$03C6, #$00B1, #$00BD, 
      #$00BC, #$2248, #$00AB, #$00BB, #$FEF7, #$FEF8, #$FFFF, #$FFFF, 
      #$FEFB, #$FEFC, #$FFFF, #$00A0, #$00AD, #$FE82, #$00A3, #$00A4, 
      #$FE84, #$FFFF, #$FFFF, #$FE8E, #$FE8F, #$FE95, #$FE99, #$060C,
      #$FE9D, #$FEA1, #$FEA5, #$0660, #$0661, #$0662, #$0663, #$0664, 
      #$0665, #$0666, #$0667, #$0668, #$0669, #$FED1, #$061B, #$FEB1,
      #$FEB5, #$FEB9, #$061F, #$00A2, #$FFFF, #$FFFF, #$FFFF, #$FFFF,
      #$FECA, #$FFFF, #$FE8D, #$FE91, #$FE93, #$FE97, #$FE9B, #$FE9F,
      #$FEA3, #$FEA7, #$FEA9, #$FEAB, #$FEAD, #$FEAF, #$FEB3, #$FEB7,
      #$FEBB, #$FEBF, #$FEC1, #$FEC5, #$FECB, #$FECF, #$00A6, #$00AC,
      #$00F7, #$00D7, #$FEC9, #$0640, #$FED3, #$FED7, #$FEDB, #$FEDF,
      #$FEE3, #$FEE7, #$FEEB, #$FEED, #$FEEF, #$FEF3, #$FEBD, #$FECC,
      #$FECE, #$FECD, #$FEE1, #$FE7D, #$0651, #$FEE5, #$FEE9, #$FEEC,
      #$FEF0, #$FEF2, #$FED0, #$FED5, #$FEF5, #$FEF6, #$FEDD, #$FED9,
      #$FEF1, #$25A0, #$FFFF);

function TIBM864Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  if Ord(P) >= $25 then
    Result := IBM864Map[Ord(P)]
  else
    Result := WideChar(P);
end;

function TIBM864Codec.EncodeChar(const Ch: WideChar): ByteChar;
var I : Byte;
begin
  if Ch = #$FFFF then
    raise EConvertError.CreateFmt(SCannotConvert, [Ord(Ch), 'IBM864']);
  if Ord(Ch) < $25 then
    begin
      Result := AnsiChar(Ord(Ch));
      exit;
    end;
  for I := $25 to $FF do
    if IBM864Map[I] = Ch then
      begin
        Result := AnsiChar(I);
        exit;
      end;
  raise EConvertError.CreateFmt(SCannotConvert, [Ord(Ch), 'IBM864']);
end;



{                                                                              }
{ CP865                                                                        }
{   Mapping shared by IBM-865 and Windows-865.                                 }
{                                                                              }
const
  CP865Map : AnsiCharHighMap = (
      #$00C7, #$00FC, #$00E9, #$00E2, #$00E4, #$00E0, #$00E5, #$00E7,
      #$00EA, #$00EB, #$00E8, #$00EF, #$00EE, #$00EC, #$00C4, #$00C5,
      #$00C9, #$00E6, #$00C6, #$00F4, #$00F6, #$00F2, #$00FB, #$00F9,
      #$00FF, #$00D6, #$00DC, #$00F8, #$00A3, #$00D8, #$20A7, #$0192,
      #$00E1, #$00ED, #$00F3, #$00FA, #$00F1, #$00D1, #$00AA, #$00BA,
      #$00BF, #$2310, #$00AC, #$00BD, #$00BC, #$00A1, #$00AB, #$00A4,
      #$2591, #$2592, #$2593, #$2502, #$2524, #$2561, #$2562, #$2556,
      #$2555, #$2563, #$2551, #$2557, #$255D, #$255C, #$255B, #$2510,
      #$2514, #$2534, #$252C, #$251C, #$2500, #$253C, #$255E, #$255F,
      #$255A, #$2554, #$2569, #$2566, #$2560, #$2550, #$256C, #$2567,
      #$2568, #$2564, #$2565, #$2559, #$2558, #$2552, #$2553, #$256B,
      #$256A, #$2518, #$250C, #$2588, #$2584, #$258C, #$2590, #$2580,
      #$03B1, #$00DF, #$0393, #$03C0, #$03A3, #$03C3, #$00B5, #$03C4,
      #$03A6, #$0398, #$03A9, #$03B4, #$221E, #$03C6, #$03B5, #$2229,
      #$2261, #$00B1, #$2265, #$2264, #$2320, #$2321, #$00F7, #$2248,
      #$00B0, #$2219, #$00B7, #$221A, #$207F, #$00B2, #$25A0, #$00A0);

{                                                                              }
{ Windows-865                                                                  }
{                                                                              }
function TWindows865Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  if Ord(P) < $80 then
    Result := WideChar(P)
  else
    Result := CP865Map[Ord(P)];
end;

function TWindows865Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromHighMap(Ch, CP865Map, 'Windows-865');
end;



{                                                                              }
{ IBM865                                                                       }
{                                                                              }
function TIBM865Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  if Ord(P) < $80 then
    Result := WideChar(P)
  else
    Result := CP865Map[Ord(P)];
end;

function TIBM865Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromHighMap(Ch, CP437Map, 'IBM865');
end;



{                                                                              }
{ CP866                                                                        }
{   Map shared by IBM866 and Windows-866.                                      }
{                                                                              }
const
  CP866Map : AnsiCharHighMap = (
      #$0410, #$0411, #$0412, #$0413, #$0414, #$0415, #$0416, #$0417,
      #$0418, #$0419, #$041A, #$041B, #$041C, #$041D, #$041E, #$041F,
      #$0420, #$0421, #$0422, #$0423, #$0424, #$0425, #$0426, #$0427,
      #$0428, #$0429, #$042A, #$042B, #$042C, #$042D, #$042E, #$042F,
      #$0430, #$0431, #$0432, #$0433, #$0434, #$0435, #$0436, #$0437,
      #$0438, #$0439, #$043A, #$043B, #$043C, #$043D, #$043E, #$043F,
      #$2591, #$2592, #$2593, #$2502, #$2524, #$2561, #$2562, #$2556,
      #$2555, #$2563, #$2551, #$2557, #$255D, #$255C, #$255B, #$2510,
      #$2514, #$2534, #$252C, #$251C, #$2500, #$253C, #$255E, #$255F,
      #$255A, #$2554, #$2569, #$2566, #$2560, #$2550, #$256C, #$2567,
      #$2568, #$2564, #$2565, #$2559, #$2558, #$2552, #$2553, #$256B,
      #$256A, #$2518, #$250C, #$2588, #$2584, #$258C, #$2590, #$2580,
      #$0440, #$0441, #$0442, #$0443, #$0444, #$0445, #$0446, #$0447,
      #$0448, #$0449, #$044A, #$044B, #$044C, #$044D, #$044E, #$044F,
      #$0401, #$0451, #$0404, #$0454, #$0407, #$0457, #$040E, #$045E,
      #$00B0, #$2219, #$00B7, #$221A, #$2116, #$00A4, #$25A0, #$00A0);



{                                                                              }
{ Windows-866                                                                  }
{                                                                              }
function TWindows866Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  if Ord(P) < $80 then
    Result := WideChar(P)
  else
    Result := CP866Map[Ord(P)];
end;

function TWindows866Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromHighMap(Ch, CP866Map, 'Windows-866');
end;



{                                                                              }
{ IBM866                                                                       }
{                                                                              }
function TIBM866Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  if Ord(P) < $80 then
    Result := WideChar(P)
  else
    Result := CP866Map[Ord(P)];
end;

function TIBM866Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromHighMap(Ch, CP866Map, 'IBM866');
end;



{                                                                              }
{ IBM868                                                                       }
{                                                                              }
const
  IBM868Map : AnsiCharHighMap = (
      #$0660, #$0661, #$0662, #$0663, #$0664, #$0665, #$0666, #$0667,
      #$0668, #$0669, #$060C, #$061B, #$061F, #$0622, #$0627, #$FE8E,
      #$E016, #$0628, #$FE91, #$067E, #$FFFF, #$0629, #$062A, #$FE97,
      #$FFFF, #$FFFF, #$062B, #$FE9B, #$062C, #$FE9F, #$FFFF, #$FFFF,
      #$062D, #$FEA3, #$062E, #$FEA7, #$062F, #$FFFF, #$0630, #$0631,
      #$FFFF, #$0632, #$FFFF, #$0633, #$FEB3, #$0634, #$00AB, #$00BB,
      #$FEB7, #$0635, #$2591, #$2592, #$2593, #$2502, #$2524, #$FEBB,
      #$0636, #$FEBF, #$0637, #$2563, #$2551, #$2557, #$255D, #$0638,
      #$0639, #$2510, #$2514, #$2534, #$252C, #$251C, #$2500, #$253C,
      #$FECA, #$FECB, #$255A, #$2554, #$2569, #$2566, #$2560, #$2550,
      #$256C, #$FECC, #$063A, #$FECE, #$FECF, #$FED0, #$0641, #$FED3,
      #$0642, #$FED7, #$FEDA, #$2518, #$250C, #$2588, #$2580, #$FEDB,
      #$FFFF, #$2584, #$FFFF, #$0644, #$FEDE, #$FEE0, #$0645, #$FEE3,
      #$FFFF, #$0646, #$FEE7, #$FFFF, #$0648, #$FFFF, #$FFFF, #$FFFF,
      #$FFFF, #$0621, #$00AD, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF,
      #$FFFF, #$FFFF, #$FFFF, #$0651, #$FE7D, #$FFFF, #$25A0, #$00A0);

function TIBM868Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  if Ord(P) < $80 then
    Result := WideChar(P)
  else
    Result := IBM868Map[Ord(P)];
end;

function TIBM868Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromHighMap(Ch, CP866Map, 'IBM868');
end;



{                                                                              }
{ CP869                                                                        }
{   Map shared by IBM869 and Windows-869.                                      }
{                                                                              }
const
  CP869Map : AnsiCharHighMap = (
      #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$0386, #$FFFF,
      #$00B7, #$00AC, #$00A6, #$2018, #$2019, #$0388, #$2015, #$0389,
      #$038A, #$03AA, #$038C, #$FFFF, #$FFFF, #$038E, #$03AB, #$00A9,
      #$038F, #$00B2, #$00B3, #$03AC, #$00A3, #$03AD, #$03AE, #$03AF,
      #$03CA, #$0390, #$03CC, #$03CD, #$0391, #$0392, #$0393, #$0394,
      #$0395, #$0396, #$0397, #$00BD, #$0398, #$0399, #$00AB, #$00BB,
      #$2591, #$2592, #$2593, #$2502, #$2524, #$039A, #$039B, #$039C,
      #$039D, #$2563, #$2551, #$2557, #$255D, #$039E, #$039F, #$2510,
      #$2514, #$2534, #$252C, #$251C, #$2500, #$253C, #$03A0, #$03A1,
      #$255A, #$2554, #$2569, #$2566, #$2560, #$2550, #$256C, #$03A3,
      #$03A4, #$03A5, #$03A6, #$03A7, #$03A8, #$03A9, #$03B1, #$03B2,
      #$03B3, #$2518, #$250C, #$2588, #$2584, #$03B4, #$03B5, #$2580,
      #$03B6, #$03B7, #$03B8, #$03B9, #$03BA, #$03BB, #$03BC, #$03BD,
      #$03BE, #$03BF, #$03C0, #$03C1, #$03C3, #$03C2, #$03C4, #$0384,
      #$00AD, #$00B1, #$03C5, #$03C6, #$03C7, #$00A7, #$03C8, #$0385,
      #$00B0, #$00A8, #$03C9, #$03CB, #$03B0, #$03CE, #$25A0, #$00A0);



{                                                                              }
{ Windows-869                                                                  }
{                                                                              }
function TWindows869Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  case Ord(P) of
    $00..$7F : Result := WideChar(P);
    $87      : Result := #$0087;
    $93      : Result := #$0093;
    $94      : Result := #$0094;
  else
    Result := CP869Map[Ord(P)];
  end;
end;

function TWindows869Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  case Ord(Ch) of
    $0087 : Result := AnsiChar($87);
    $0093 : Result := AnsiChar($93);
    $0094 : Result := AnsiChar($94);
  else
    Result := CharFromHighMap(Ch, CP869Map, 'Windows-869');
  end;
end;



{                                                                              }
{ IBM869                                                                       }
{                                                                              }
function TIBM869Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  if Ord(P) < $80 then
    Result := WideChar(P)
  else
    Result := CP869Map[Ord(P)];
end;

function TIBM869Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromHighMap(Ch, CP869Map, 'IBM869');
end;



{                                                                              }
{ Windows-870                                                                  }
{                                                                              }
const
  Windows870Map : AnsiCharMap = (
      #$0000, #$0001, #$0002, #$0003, #$009C, #$0009, #$0086, #$007F,
      #$0097, #$008D, #$008E, #$000B, #$000C, #$000D, #$000E, #$000F,
      #$0010, #$0011, #$0012, #$0013, #$009D, #$0085, #$0008, #$0087,
      #$0018, #$0019, #$0092, #$008F, #$001C, #$001D, #$001E, #$001F,
      #$0080, #$0081, #$0082, #$0083, #$0084, #$000A, #$0017, #$001B,
      #$0088, #$0089, #$008A, #$008B, #$008C, #$0005, #$0006, #$0007,
      #$0090, #$0091, #$0016, #$0093, #$0094, #$0095, #$0096, #$0004,
      #$0098, #$0099, #$009A, #$009B, #$0014, #$0015, #$009E, #$001A,
      #$0020, #$00A0, #$00E2, #$00E4, #$0163, #$00E1, #$0103, #$010D,
      #$00E7, #$0107, #$005B, #$002E, #$003C, #$0028, #$002B, #$0021,
      #$0026, #$00E9, #$0119, #$00EB, #$016F, #$00ED, #$00EE, #$013E,
      #$013A, #$00DF, #$005D, #$0024, #$002A, #$0029, #$003B, #$005E,
      #$002D, #$002F, #$00C2, #$00C4, #$02DD, #$00C1, #$0102, #$010C,
      #$00C7, #$0106, #$007C, #$002C, #$0025, #$005F, #$003E, #$003F,
      #$02C7, #$00C9, #$0118, #$00CB, #$016E, #$00CD, #$00CE, #$013D,
      #$0139, #$0060, #$003A, #$0023, #$0040, #$0027, #$003D, #$0022,
      #$02D8, #$0061, #$0062, #$0063, #$0064, #$0065, #$0066, #$0067,
      #$0068, #$0069, #$015B, #$0148, #$0111, #$00FD, #$0159, #$015F,
      #$00B0, #$006A, #$006B, #$006C, #$006D, #$006E, #$006F, #$0070,
      #$0071, #$0072, #$0142, #$0144, #$0161, #$00B8, #$02DB, #$00A4,
      #$0105, #$007E, #$0073, #$0074, #$0075, #$0076, #$0077, #$0078,
      #$0079, #$007A, #$015A, #$0147, #$0110, #$00DD, #$0158, #$015E,
      #$02D9, #$0104, #$017C, #$0162, #$017B, #$00A7, #$017E, #$017A,
      #$017D, #$0179, #$0141, #$0143, #$0160, #$00A8, #$00B4, #$00D7,
      #$007B, #$0041, #$0042, #$0043, #$0044, #$0045, #$0046, #$0047,
      #$0048, #$0049, #$00AD, #$00F4, #$00F6, #$0155, #$00F3, #$0151,
      #$007D, #$004A, #$004B, #$004C, #$004D, #$004E, #$004F, #$0050,
      #$0051, #$0052, #$011A, #$0171, #$00FC, #$0165, #$00FA, #$011B,
      #$005C, #$00F7, #$0053, #$0054, #$0055, #$0056, #$0057, #$0058,
      #$0059, #$005A, #$010F, #$00D4, #$00D6, #$0154, #$00D3, #$0150,
      #$0030, #$0031, #$0032, #$0033, #$0034, #$0035, #$0036, #$0037,
      #$0038, #$0039, #$010E, #$0170, #$00DC, #$0164, #$00DA, #$009F);

function TWindows870Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  Result := Windows870Map[P];
end;

function TWindows870Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromMap(Ch, Windows870Map, 'Windows-870');
end;



{                                                                              }
{ IBM870                                                                       }
{                                                                              }
const
  IBM870Map : AnsiCharMap = (
      #$0000, #$0001, #$0002, #$0003, #$009C, #$0009, #$0086, #$007F,
      #$0097, #$008D, #$008E, #$000B, #$000C, #$000D, #$000E, #$000F,
      #$0010, #$0011, #$0012, #$0013, #$009D, #$0085, #$0008, #$0087,
      #$0018, #$0019, #$0092, #$008F, #$001C, #$001D, #$001E, #$001F,
      #$0080, #$0081, #$0082, #$0083, #$0084, #$000A, #$0017, #$001B,
      #$0088, #$0089, #$008A, #$008B, #$008C, #$0005, #$0006, #$0007,
      #$0090, #$0091, #$0016, #$0093, #$0094, #$0095, #$0096, #$0004, 
      #$0098, #$0099, #$009A, #$009B, #$0014, #$0015, #$009E, #$001A, 
      #$0020, #$00A0, #$FFFF, #$00E4, #$FFFF, #$00E1, #$0103, #$010D, 
      #$00E7, #$0107, #$005B, #$002E, #$003C, #$0028, #$002B, #$0021,
      #$0026, #$00E9, #$FFFF, #$00EB, #$016F, #$00ED, #$FFFF, #$013E, 
      #$013A, #$00DF, #$005D, #$0024, #$002A, #$0029, #$003B, #$005E,
      #$002D, #$002F, #$FFFF, #$00C4, #$02DD, #$00C1, #$FFFF, #$010C, 
      #$00C7, #$0106, #$007C, #$002C, #$0025, #$005F, #$003E, #$003F, 
      #$02C7, #$00C9, #$FFFF, #$00CB, #$016E, #$00CD, #$FFFF, #$013D, 
      #$0139, #$0060, #$003A, #$0023, #$0040, #$0027, #$003D, #$0022,
      #$02D8, #$0061, #$0062, #$0063, #$0064, #$0065, #$0066, #$0067, 
      #$0068, #$0069, #$015B, #$0148, #$0111, #$00FD, #$0159, #$FFFF, 
      #$00B0, #$006A, #$006B, #$006C, #$006D, #$006E, #$006F, #$0070,
      #$0071, #$0072, #$0142, #$0144, #$0161, #$00B8, #$02DB, #$00A4,
      #$0105, #$007E, #$0073, #$0074, #$0075, #$0076, #$0077, #$0078,
      #$0079, #$007A, #$015A, #$0147, #$0110, #$00DD, #$0158, #$FFFF,
      #$00B7, #$0104, #$017C, #$FFFF, #$017B, #$00A7, #$00B6, #$017E,
      #$017A, #$017D, #$0179, #$0143, #$0160, #$00A8, #$00B4, #$00D7,
      #$007B, #$0041, #$0042, #$0043, #$0044, #$0045, #$0046, #$0047,
      #$0048, #$0049, #$00AD, #$00F4, #$00F6, #$0155, #$00F3, #$0151,
      #$007D, #$004A, #$004B, #$004C, #$004D, #$004E, #$004F, #$0050,
      #$0051, #$0052, #$011A, #$0171, #$00FC, #$0165, #$00FA, #$011B,
      #$005C, #$00F7, #$0053, #$0054, #$0055, #$0056, #$0057, #$0058,
      #$0059, #$005A, #$010F, #$00D4, #$00D6, #$0154, #$00D3, #$0150,
      #$0030, #$0031, #$0032, #$0033, #$0034, #$0035, #$0036, #$0037,
      #$0038, #$0039, #$010E, #$0170, #$00DC, #$0164, #$00DA, #$009F);

function TIBM870Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  Result := IBM870Map[P];
end;

function TIBM870Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromMap(Ch, Windows870Map, 'IBM870');
end;



{                                                                              }
{ IBM871                                                                       }
{                                                                              }
const
  IBM871Map : AnsiCharMap = (
      #$0000, #$0001, #$0002, #$0003, #$009C, #$0009, #$0086, #$007F,
      #$0097, #$008D, #$008E, #$000B, #$000C, #$000D, #$000E, #$000F,
      #$0010, #$0011, #$0012, #$0013, #$009D, #$0085, #$0008, #$0087,
      #$0018, #$0019, #$0092, #$008F, #$001C, #$001D, #$001E, #$001F,
      #$0080, #$0081, #$0082, #$0083, #$0084, #$000A, #$0017, #$001B,
      #$0088, #$0089, #$008A, #$008B, #$008C, #$0005, #$0006, #$0007,
      #$0090, #$0091, #$0016, #$0093, #$0094, #$0095, #$0096, #$0004,
      #$0098, #$0099, #$009A, #$009B, #$0014, #$0015, #$009E, #$001A,
      #$0020, #$00A0, #$00E2, #$00E4, #$00E0, #$00E1, #$00E3, #$00E5,
      #$00E7, #$00F1, #$00FE, #$002E, #$003C, #$0028, #$002B, #$0021,
      #$0026, #$00E9, #$00EA, #$00EB, #$00E8, #$00ED, #$00EE, #$00EF,
      #$00EC, #$00DF, #$00C6, #$0024, #$002A, #$0029, #$003B, #$00D6,
      #$002D, #$002F, #$00C2, #$00C4, #$00C0, #$00C1, #$00C3, #$00C5,
      #$00C7, #$00D1, #$00A6, #$002C, #$0025, #$005F, #$003E, #$003F,
      #$00F8, #$00C9, #$00CA, #$00CB, #$00C8, #$00CD, #$00CE, #$00CF,
      #$00CC, #$00F0, #$003A, #$0023, #$00D0, #$0027, #$003D, #$0022,
      #$00D8, #$0061, #$0062, #$0063, #$0064, #$0065, #$0066, #$0067,
      #$0068, #$0069, #$00AB, #$00BB, #$0060, #$00FD, #$007B, #$00B1,
      #$00B0, #$006A, #$006B, #$006C, #$006D, #$006E, #$006F, #$0070,
      #$0071, #$0072, #$00AA, #$00BA, #$007D, #$00B8, #$005D, #$00A4,
      #$00B5, #$00F6, #$0073, #$0074, #$0075, #$0076, #$0077, #$0078,
      #$0079, #$007A, #$00A1, #$00BF, #$0040, #$00DD, #$005B, #$00AE,
      #$00A2, #$00A3, #$00A5, #$00B7, #$00A9, #$00A7, #$00B6, #$00BC,
      #$00BD, #$00BE, #$00AC, #$007C, #$00AF, #$00A8, #$005C, #$00D7,
      #$00DE, #$0041, #$0042, #$0043, #$0044, #$0045, #$0046, #$0047,
      #$0048, #$0049, #$00AD, #$00F4, #$007E, #$00F2, #$00F3, #$00F5,
      #$00E6, #$004A, #$004B, #$004C, #$004D, #$004E, #$004F, #$0050,
      #$0051, #$0052, #$00B9, #$00FB, #$00FC, #$00F9, #$00FA, #$00FF,
      #$00B4, #$00F7, #$0053, #$0054, #$0055, #$0056, #$0057, #$0058,
      #$0059, #$005A, #$00B2, #$00D4, #$005E, #$00D2, #$00D3, #$00D5,
      #$0030, #$0031, #$0032, #$0033, #$0034, #$0035, #$0036, #$0037,
      #$0038, #$0039, #$00B3, #$00DB, #$00DC, #$00D9, #$00DA, #$009F);

function TIBM871Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  Result := IBM871Map[P];
end;

function TIBM871Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromMap(Ch, IBM871Map, 'IBM871');
end;



{                                                                              }
{ Windows-874                                                                  }
{                                                                              }
const
  Windows874Map : AnsiCharHighMap = (
      #$20AC, #$0081, #$0082, #$0083, #$0084, #$2026, #$0086, #$0087,
      #$0088, #$0089, #$008A, #$008B, #$008C, #$008D, #$008E, #$008F,
      #$0090, #$2018, #$2019, #$201C, #$201D, #$2022, #$2013, #$2014,
      #$0098, #$0099, #$009A, #$009B, #$009C, #$009D, #$009E, #$009F,
      #$00A0, #$0E01, #$0E02, #$0E03, #$0E04, #$0E05, #$0E06, #$0E07,
      #$0E08, #$0E09, #$0E0A, #$0E0B, #$0E0C, #$0E0D, #$0E0E, #$0E0F,
      #$0E10, #$0E11, #$0E12, #$0E13, #$0E14, #$0E15, #$0E16, #$0E17,
      #$0E18, #$0E19, #$0E1A, #$0E1B, #$0E1C, #$0E1D, #$0E1E, #$0E1F,
      #$0E20, #$0E21, #$0E22, #$0E23, #$0E24, #$0E25, #$0E26, #$0E27,
      #$0E28, #$0E29, #$0E2A, #$0E2B, #$0E2C, #$0E2D, #$0E2E, #$0E2F,
      #$0E30, #$0E31, #$0E32, #$0E33, #$0E34, #$0E35, #$0E36, #$0E37,
      #$0E38, #$0E39, #$0E3A, #$F8C1, #$F8C2, #$F8C3, #$F8C4, #$0E3F,
      #$0E40, #$0E41, #$0E42, #$0E43, #$0E44, #$0E45, #$0E46, #$0E47,
      #$0E48, #$0E49, #$0E4A, #$0E4B, #$0E4C, #$0E4D, #$0E4E, #$0E4F,
      #$0E50, #$0E51, #$0E52, #$0E53, #$0E54, #$0E55, #$0E56, #$0E57,
      #$0E58, #$0E59, #$0E5A, #$0E5B, #$F8C5, #$F8C6, #$F8C7, #$F8C8);

function TWindows874Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  if Ord(P) < $80 then
    Result := WideChar(P)
  else
    Result := Windows874Map[Ord(P)];
end;

function TWindows874Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromHighMap(Ch, Windows874Map, 'Windows-874');
end;



{                                                                              }
{ IBM874                                                                       }
{                                                                              }
const
  IBM874Map : AnsiCharHighMap = (
      #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$2026, #$FFFF, #$FFFF,
      #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF,
      #$FFFF, #$2018, #$2019, #$201C, #$201D, #$2022, #$2013, #$2014,
      #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF,
      #$00A0, #$0E01, #$0E02, #$0E03, #$0E04, #$0E05, #$0E06, #$0E07,
      #$0E08, #$0E09, #$0E0A, #$0E0B, #$0E0C, #$0E0D, #$0E0E, #$0E0F,
      #$0E10, #$0E11, #$0E12, #$0E13, #$0E14, #$0E15, #$0E16, #$0E17,
      #$0E18, #$0E19, #$0E1A, #$0E1B, #$0E1C, #$0E1D, #$0E1E, #$0E1F, 
      #$0E20, #$0E21, #$0E22, #$0E23, #$0E24, #$0E25, #$0E26, #$0E27,
      #$0E28, #$0E29, #$0E2A, #$0E2B, #$0E2C, #$0E2D, #$0E2E, #$0E2F,
      #$0E30, #$0E31, #$0E32, #$0E33, #$0E34, #$0E35, #$0E36, #$0E37,
      #$0E38, #$0E39, #$0E3A, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$0E3F,
      #$0E40, #$0E41, #$0E42, #$0E43, #$0E44, #$0E45, #$0E46, #$0E47,
      #$0E48, #$0E49, #$0E4A, #$0E4B, #$0E4C, #$0E4D, #$0E4E, #$0E4F,
      #$0E50, #$0E51, #$0E52, #$0E53, #$0E54, #$0E55, #$0E56, #$0E57,
      #$0E58, #$0E59, #$0E5A, #$0E5B, #$FFFF, #$FFFF, #$FFFF, #$FFFF);

function TIBM874Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  if Ord(P) < $80 then
    Result := WideChar(P)
  else
    Result := IBM874Map[Ord(P)];
end;

function TIBM874Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromHighMap(Ch, IBM874Map, 'IBM874');
end;



{                                                                              }
{ Windows-875                                                                  }
{                                                                              }
const
  Windows875Map : AnsiCharMap = (
      #$0000, #$0001, #$0002, #$0003, #$009C, #$0009, #$0086, #$007F,
      #$0097, #$008D, #$008E, #$000B, #$000C, #$000D, #$000E, #$000F,
      #$0010, #$0011, #$0012, #$0013, #$009D, #$0085, #$0008, #$0087,
      #$0018, #$0019, #$0092, #$008F, #$001C, #$001D, #$001E, #$001F,
      #$0080, #$0081, #$0082, #$0083, #$0084, #$000A, #$0017, #$001B,
      #$0088, #$0089, #$008A, #$008B, #$008C, #$0005, #$0006, #$0007,
      #$0090, #$0091, #$0016, #$0093, #$0094, #$0095, #$0096, #$0004,
      #$0098, #$0099, #$009A, #$009B, #$0014, #$0015, #$009E, #$001A,
      #$0020, #$0391, #$0392, #$0393, #$0394, #$0395, #$0396, #$0397,
      #$0398, #$0399, #$005B, #$002E, #$003C, #$0028, #$002B, #$0021,
      #$0026, #$039A, #$039B, #$039C, #$039D, #$039E, #$039F, #$03A0,
      #$03A1, #$03A3, #$005D, #$0024, #$002A, #$0029, #$003B, #$005E,
      #$002D, #$002F, #$03A4, #$03A5, #$03A6, #$03A7, #$03A8, #$03A9, 
      #$03AA, #$03AB, #$007C, #$002C, #$0025, #$005F, #$003E, #$003F,
      #$00A8, #$0386, #$0388, #$0389, #$00A0, #$038A, #$038C, #$038E,
      #$038F, #$0060, #$003A, #$0023, #$0040, #$0027, #$003D, #$0022, 
      #$0385, #$0061, #$0062, #$0063, #$0064, #$0065, #$0066, #$0067, 
      #$0068, #$0069, #$03B1, #$03B2, #$03B3, #$03B4, #$03B5, #$03B6, 
      #$00B0, #$006A, #$006B, #$006C, #$006D, #$006E, #$006F, #$0070,
      #$0071, #$0072, #$03B7, #$03B8, #$03B9, #$03BA, #$03BB, #$03BC,
      #$00B4, #$007E, #$0073, #$0074, #$0075, #$0076, #$0077, #$0078, 
      #$0079, #$007A, #$03BD, #$03BE, #$03BF, #$03C0, #$03C1, #$03C3,
      #$00A3, #$03AC, #$03AD, #$03AE, #$03CA, #$03AF, #$03CC, #$03CD,
      #$03CB, #$03CE, #$03C2, #$03C4, #$03C5, #$03C6, #$03C7, #$03C8, 
      #$007B, #$0041, #$0042, #$0043, #$0044, #$0045, #$0046, #$0047, 
      #$0048, #$0049, #$00AD, #$03C9, #$0390, #$03B0, #$2018, #$2015,
      #$007D, #$004A, #$004B, #$004C, #$004D, #$004E, #$004F, #$0050,
      #$0051, #$0052, #$00B1, #$00BD, #$FFFF, #$0387, #$2019, #$00A6,
      #$005C, #$FFFF, #$0053, #$0054, #$0055, #$0056, #$0057, #$0058,
      #$0059, #$005A, #$00B2, #$00A7, #$FFFF, #$FFFF, #$00AB, #$00AC,
      #$0030, #$0031, #$0032, #$0033, #$0034, #$0035, #$0036, #$0037,
      #$0038, #$0039, #$00B3, #$00A9, #$FFFF, #$FFFF, #$00BB, #$009F);

function TWindows875Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  Result := Windows875Map[P];
end;

function TWindows875Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromMap(Ch, Windows875Map, 'Windows-875');
end;



{                                                                              }
{ IBM875                                                                       }
{                                                                              }
const
  IBM875Map : AnsiCharMap = (
      #$0000, #$0001, #$0002, #$0003, #$009C, #$0009, #$0086, #$007F,
      #$0097, #$008D, #$008E, #$000B, #$000C, #$000D, #$000E, #$000F,
      #$0010, #$0011, #$0012, #$0013, #$009D, #$0085, #$0008, #$0087,
      #$0018, #$0019, #$0092, #$008F, #$001C, #$001D, #$001E, #$001F,
      #$0080, #$0081, #$0082, #$0083, #$0084, #$000A, #$0017, #$001B,
      #$0088, #$0089, #$008A, #$008B, #$008C, #$0005, #$0006, #$0007,
      #$0090, #$0091, #$0016, #$0093, #$0094, #$0095, #$0096, #$0004,
      #$0098, #$0099, #$009A, #$009B, #$0014, #$0015, #$009E, #$001A,
      #$0020, #$0391, #$0392, #$0393, #$0394, #$0395, #$0396, #$0397,
      #$0398, #$0399, #$005B, #$002E, #$003C, #$0028, #$002B, #$0021,
      #$0026, #$039A, #$039B, #$039C, #$039D, #$039E, #$039F, #$03A0, 
      #$03A1, #$03A3, #$005D, #$0024, #$002A, #$0029, #$003B, #$005E, 
      #$002D, #$002F, #$03A4, #$03A5, #$03A6, #$03A7, #$03A8, #$03A9,
      #$03AA, #$03AB, #$FFFF, #$002C, #$0025, #$005F, #$003E, #$003F, 
      #$00A8, #$0386, #$0388, #$0389, #$2207, #$038A, #$038C, #$038E,
      #$038F, #$0060, #$003A, #$0023, #$0040, #$0027, #$003D, #$0022, 
      #$0385, #$0061, #$0062, #$0063, #$0064, #$0065, #$0066, #$0067,
      #$0068, #$0069, #$03B1, #$03B2, #$03B3, #$03B4, #$03B5, #$03B6,
      #$00B0, #$006A, #$006B, #$006C, #$006D, #$006E, #$006F, #$0070,
      #$0071, #$0072, #$03B7, #$03B8, #$03B9, #$03BA, #$03BB, #$03BC,
      #$00B4, #$007E, #$0073, #$0074, #$0075, #$0076, #$0077, #$0078,
      #$0079, #$007A, #$03BD, #$03BE, #$03BF, #$03C0, #$03C1, #$03C3,
      #$00A3, #$03AC, #$03AD, #$03AE, #$0390, #$03AF, #$03CC, #$03CD, 
      #$03B0, #$03CE, #$03C2, #$03C4, #$03C5, #$03C6, #$03C7, #$03C8,
      #$007B, #$0041, #$0042, #$0043, #$0044, #$0045, #$0046, #$0047,
      #$0048, #$0049, #$00AD, #$03C9, #$03CA, #$03CB, #$2018, #$2015,
      #$007D, #$004A, #$004B, #$004C, #$004D, #$004E, #$004F, #$0050,
      #$0051, #$0052, #$00B1, #$00BD, #$FFFF, #$00B7, #$2019, #$00A6,
      #$005C, #$FFFF, #$0053, #$0054, #$0055, #$0056, #$0057, #$0058,
      #$0059, #$005A, #$00B2, #$00A7, #$FFFF, #$FFFF, #$00AB, #$00AC,
      #$0030, #$0031, #$0032, #$0033, #$0034, #$0035, #$0036, #$0037,
      #$0038, #$0039, #$00B3, #$00A9, #$FFFF, #$FFFF, #$00BB, #$009F);

function TIBM875Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  Result := IBM875Map[P];
end;

function TIBM875Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromMap(Ch, IBM875Map, 'IBM875');
end;



{                                                                              }
{ IBM880                                                                       }
{                                                                              }
const
  IBM880Map : AnsiCharMap = (
      #$0000, #$0001, #$0002, #$0003, #$009C, #$0009, #$0086, #$007F,
      #$0097, #$008D, #$008E, #$000B, #$000C, #$000D, #$000E, #$000F,
      #$0010, #$0011, #$0012, #$0013, #$009D, #$0085, #$0008, #$0087,
      #$0018, #$0019, #$0092, #$008F, #$001C, #$001D, #$001E, #$001F,
      #$0080, #$0081, #$0082, #$0083, #$0084, #$000A, #$0017, #$001B,
      #$0088, #$0089, #$008A, #$008B, #$008C, #$0005, #$0006, #$0007, 
      #$0090, #$0091, #$0016, #$0093, #$0094, #$0095, #$0096, #$0004,
      #$0098, #$0099, #$009A, #$009B, #$0014, #$0015, #$009E, #$001A, 
      #$0020, #$FFFF, #$0452, #$0453, #$0451, #$FFFF, #$0455, #$0456, 
      #$0457, #$0458, #$005B, #$002E, #$003C, #$0028, #$002B, #$0021,
      #$0026, #$0459, #$045A, #$045B, #$045C, #$FFFF, #$045F, #$042A, 
      #$2116, #$0402, #$005D, #$0024, #$002A, #$0029, #$003B, #$005E, 
      #$002D, #$002F, #$0403, #$0401, #$FFFF, #$0405, #$0406, #$0407, 
      #$0408, #$0409, #$00A6, #$002C, #$0025, #$005F, #$003E, #$003F,
      #$040A, #$040B, #$040C, #$FFFF, #$FFFF, #$040F, #$044E, #$0430, 
      #$0431, #$FFFF, #$003A, #$0023, #$0040, #$0027, #$003D, #$0022, 
      #$0446, #$0061, #$0062, #$0063, #$0064, #$0065, #$0066, #$0067,
      #$0068, #$0069, #$0434, #$0435, #$0444, #$0433, #$0445, #$0438, 
      #$0439, #$006A, #$006B, #$006C, #$006D, #$006E, #$006F, #$0070,
      #$0071, #$0072, #$043A, #$043B, #$043C, #$043D, #$043E, #$043F, 
      #$044F, #$FFFF, #$0073, #$0074, #$0075, #$0076, #$0077, #$0078,
      #$0079, #$007A, #$0440, #$0441, #$0442, #$0443, #$0436, #$0432,
      #$044C, #$044B, #$0437, #$0448, #$044D, #$0449, #$0447, #$044A,
      #$042E, #$0410, #$0411, #$0426, #$0414, #$0415, #$0424, #$0413, 
      #$FFFF, #$0041, #$0042, #$0043, #$0044, #$0045, #$0046, #$0047,
      #$0048, #$0049, #$0425, #$0418, #$0419, #$041A, #$041B, #$041C, 
      #$FFFF, #$004A, #$004B, #$004C, #$004D, #$004E, #$004F, #$0050,
      #$0051, #$0052, #$041D, #$041E, #$041F, #$042F, #$0420, #$0421,
      #$005C, #$00A4, #$0053, #$0054, #$0055, #$0056, #$0057, #$0058,
      #$0059, #$005A, #$0422, #$0423, #$0416, #$0412, #$042C, #$042B, 
      #$0030, #$0031, #$0032, #$0033, #$0034, #$0035, #$0036, #$0037, 
      #$0038, #$0039, #$0417, #$0428, #$042D, #$0429, #$0427, #$009F);

function TIBM880Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  Result := IBM880Map[P];
end;

function TIBM880Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromMap(Ch, IBM880Map, 'IBM880');
end;



{                                                                              }
{ IBM904                                                                       }
{                                                                              }
function TIBM904Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  case Ord(P) of
    $00..$7F : Result := WideChar(P);
    $80      : Result := #$00A2;
    $FD      : Result := #$00AC;
    $FE      : Result := #$00A6;
  else
    Result := #$FFFF;
  end;
end;

function TIBM904Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  case Ord(Ch) of
    $00..$7F : Result := AnsiChar(Ch);
    $00A2    : Result := AnsiChar($80);
    $00AC    : Result := AnsiChar($FD);
    $00A6    : Result := AnsiChar($FE);
  else
    raise EConvertError.CreateFmt(SCannotConvert, [Ord(Ch), 'IBM904']);
  end;
end;



{                                                                              }
{ IBM905                                                                       }
{                                                                              }
const
  IBM905Map : AnsiCharMap = (
      #$0000, #$0001, #$0002, #$0003, #$009C, #$0009, #$0086, #$007F, 
      #$0097, #$008D, #$008E, #$000B, #$000C, #$000D, #$000E, #$000F,
      #$0010, #$0011, #$0012, #$0013, #$009D, #$0085, #$0008, #$0087,
      #$0018, #$0019, #$0092, #$008F, #$001C, #$001D, #$001E, #$001F,
      #$0080, #$0081, #$0082, #$0083, #$0084, #$000A, #$0017, #$001B, 
      #$0088, #$0089, #$008A, #$008B, #$008C, #$0005, #$0006, #$0007, 
      #$0090, #$0091, #$0016, #$0093, #$0094, #$0095, #$0096, #$0004, 
      #$0098, #$0099, #$009A, #$009B, #$0014, #$0015, #$009E, #$001A, 
      #$0020, #$FFFF, #$00E2, #$00E4, #$00E0, #$00E1, #$FFFF, #$010B,
      #$007B, #$00F1, #$00C7, #$002E, #$003C, #$0028, #$002B, #$0021,
      #$0026, #$00E9, #$00EA, #$00EB, #$00E8, #$00ED, #$00EE, #$00EF,
      #$00EC, #$00DF, #$011E, #$0130, #$002A, #$0029, #$003B, #$005E,
      #$002D, #$002F, #$00C2, #$00C4, #$00C0, #$00C1, #$FFFF, #$010A, 
      #$005B, #$00D1, #$015F, #$002C, #$0025, #$005F, #$003E, #$003F, 
      #$FFFF, #$00C9, #$00CA, #$00CB, #$00C8, #$00CD, #$00CE, #$00CF, 
      #$00CC, #$0131, #$003A, #$00D6, #$015E, #$0027, #$003D, #$00DC,
      #$02D8, #$0061, #$0062, #$0063, #$0064, #$0065, #$0066, #$0067, 
      #$0068, #$0069, #$0127, #$0109, #$015D, #$016D, #$FFFF, #$007C, 
      #$00B0, #$006A, #$006B, #$006C, #$006D, #$006E, #$006F, #$0070, 
      #$0071, #$0072, #$0125, #$011D, #$0135, #$02DB, #$FFFF, #$00A4, 
      #$00B5, #$00F6, #$0073, #$0074, #$0075, #$0076, #$0077, #$0078,
      #$0079, #$007A, #$0126, #$0108, #$015C, #$016C, #$FFFF, #$0040, 
      #$00B7, #$00A3, #$017C, #$007D, #$017B, #$00A7, #$005D, #$FFFF,
      #$00BD, #$0024, #$0124, #$011C, #$0134, #$00A8, #$00B4, #$00D7, 
      #$00E7, #$0041, #$0042, #$0043, #$0044, #$0045, #$0046, #$0047, 
      #$0048, #$0049, #$00AD, #$00F4, #$007E, #$00F2, #$00F3, #$0121, 
      #$011F, #$004A, #$004B, #$004C, #$004D, #$004E, #$004F, #$0050,
      #$0051, #$0052, #$0060, #$00FB, #$005C, #$00F9, #$00FA, #$FFFF, 
      #$00FC, #$00F7, #$0053, #$0054, #$0055, #$0056, #$0057, #$0058,
      #$0059, #$005A, #$00B2, #$00D4, #$0023, #$00D2, #$00D3, #$0120,
      #$0030, #$0031, #$0032, #$0033, #$0034, #$0035, #$0036, #$0037,
      #$0038, #$0039, #$00B3, #$00DB, #$0022, #$00D9, #$00DA, #$009F);

function TIBM905Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  Result := IBM905Map[P];
end;

function TIBM905Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromMap(Ch, IBM905Map, 'IBM905');
end;



{                                                                              }
{ IBM918                                                                       }
{                                                                              }
const
  IBM918Map : AnsiCharMap = (
      #$0000, #$0001, #$0002, #$0003, #$009C, #$0009, #$0086, #$007F,
      #$0097, #$008D, #$008E, #$000B, #$000C, #$000D, #$000E, #$000F,
      #$0010, #$0011, #$0012, #$0013, #$009D, #$0085, #$0008, #$0087,
      #$0018, #$0019, #$0092, #$008F, #$001C, #$001D, #$001E, #$001F,
      #$0080, #$0081, #$0082, #$0083, #$0084, #$000A, #$0017, #$001B,
      #$0088, #$0089, #$008A, #$008B, #$008C, #$0005, #$0006, #$0007,
      #$0090, #$0091, #$0016, #$0093, #$0094, #$0095, #$0096, #$0004,
      #$0098, #$0099, #$009A, #$009B, #$0014, #$0015, #$009E, #$001A,
      #$0020, #$00A0, #$060C, #$061B, #$061F, #$0623, #$0627, #$FE8E,
      #$FFFF, #$0628, #$005B, #$002E, #$003C, #$0028, #$002B, #$0021,
      #$0026, #$FE91, #$067E, #$FFFF, #$0629, #$062A, #$FE97, #$FFFF,
      #$FFFF, #$062B, #$005D, #$0024, #$002A, #$0029, #$003B, #$005E,
      #$002D, #$002F, #$FE9B, #$062C, #$FE9F, #$FFFF, #$FFFF, #$062D,
      #$FEA3, #$062E, #$0060, #$002C, #$0025, #$005F, #$003E, #$003F,
      #$0660, #$0661, #$0662, #$0663, #$0664, #$0665, #$0666, #$0667,
      #$0668, #$0669, #$003A, #$0023, #$0040, #$0027, #$003D, #$0022,
      #$FEA7, #$0061, #$0062, #$0063, #$0064, #$0065, #$0066, #$0067,
      #$0068, #$0069, #$062F, #$FFFF, #$0630, #$0631, #$FFFF, #$0632, 
      #$FFFF, #$006A, #$006B, #$006C, #$006D, #$006E, #$006F, #$0070,
      #$0071, #$0072, #$0633, #$FEB3, #$0634, #$FEB7, #$0635, #$FEBB, 
      #$0636, #$007E, #$0073, #$0074, #$0075, #$0076, #$0077, #$0078,
      #$0079, #$007A, #$FEBF, #$0637, #$0638, #$0639, #$FECA, #$FECB,
      #$FECC, #$063A, #$FECE, #$FECF, #$FED0, #$0641, #$FED3, #$0642,
      #$FED7, #$0643, #$FEDB, #$007C, #$FFFF, #$FFFF, #$0644, #$FEDE,
      #$007B, #$0041, #$0042, #$0043, #$0044, #$0045, #$0046, #$0047,
      #$0048, #$0049, #$00AD, #$FFFF, #$0645, #$FEE3, #$FFFF, #$0646, 
      #$007D, #$004A, #$004B, #$004C, #$004D, #$004E, #$004F, #$0050, 
      #$0051, #$0052, #$FEE7, #$FFFF, #$0648, #$FFFF, #$FFFF, #$FFFF,
      #$005C, #$FFFF, #$0053, #$0054, #$0055, #$0056, #$0057, #$0058,
      #$0059, #$005A, #$0621, #$FFFF, #$FFFF, #$FFFF, #$FFFF, #$FFFF,
      #$0030, #$0031, #$0032, #$0033, #$0034, #$0035, #$0036, #$0037,
      #$0038, #$0039, #$FFFF, #$FFFF, #$FFFF, #$0651, #$FE7D, #$009F);

function TIBM918Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  Result := IBM918Map[P];
end;

function TIBM918Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromMap(Ch, IBM905Map, 'IBM918');
end;



{                                                                              }
{ IBM1004                                                                      }
{                                                                              }
const
  IBM1004Map : array[$80..$FF] of WideChar = (
      #$FFFF, #$FFFF, #$201A, #$FFFF, #$201E, #$2026, #$2020, #$2021,
      #$02C6, #$2030, #$0160, #$2039, #$0152, #$FFFF, #$FFFF, #$FFFF,
      #$FFFF, #$2018, #$2019, #$201C, #$201D, #$2022, #$2013, #$2014,
      #$02DC, #$2122, #$0161, #$203A, #$0153, #$FFFF, #$FFFF, #$0178,
      #$00A0, #$00A1, #$00A2, #$00A3, #$00A4, #$00A5, #$00A6, #$00A7,
      #$00A8, #$00A9, #$00AA, #$00AB, #$00AC, #$00AD, #$00AE, #$00AF,
      #$00B0, #$00B1, #$00B2, #$00B3, #$00B4, #$00B5, #$00B6, #$00B7,
      #$00B8, #$00B9, #$00BA, #$00BB, #$00BC, #$00BD, #$00BE, #$00BF,
      #$00C0, #$00C1, #$00C2, #$00C3, #$00C4, #$00C5, #$00C6, #$00C7,
      #$00C8, #$00C9, #$00CA, #$00CB, #$00CC, #$00CD, #$00CE, #$00CF,
      #$00D0, #$00D1, #$00D2, #$00D3, #$00D4, #$00D5, #$00D6, #$00D7,
      #$00D8, #$00D9, #$00DA, #$00DB, #$00DC, #$00DD, #$00DE, #$00DF,
      #$00E0, #$00E1, #$00E2, #$00E3, #$00E4, #$00E5, #$00E6, #$00E7,
      #$00E8, #$00E9, #$00EA, #$00EB, #$00EC, #$00ED, #$00EE, #$00EF,
      #$00F0, #$00F1, #$00F2, #$00F3, #$00F4, #$00F5, #$00F6, #$00F7,
      #$00F8, #$00F9, #$00FA, #$00FB, #$00FC, #$00FD, #$00FE, #$00FF);

function TIBM1004Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  if Ord(P) >= $80 then
      Result := IBM1004Map[Ord(P)]
    else
      Result := WideChar(P);
end;

function TIBM1004Codec.EncodeChar(const Ch: WideChar): ByteChar;
var
  I: Byte;
begin
  if Ch = #$FFFF then
    raise EConvertError.CreateFmt(SCannotConvert, [Ord(Ch), 'IBM1004']);
  if Ord(Ch) < $80 then
    begin
      Result := AnsiChar(Ord(Ch));
      Exit;
    end;
  for I := $80 to $FF do
    if IBM1004Map[I] = Ch then
      begin
        Result := AnsiChar(I);
        Exit;
      end;
  raise EConvertError.CreateFmt(SCannotConvert, [Ord(Ch), 'IBM1004']);
end;



{                                                                              }
{ CP1026                                                                       }
{   Map shared by IBM1026 and Windows-1026.                                    }
{                                                                              }
const
  CP1026Map : AnsiCharMap = (
      #$0000, #$0001, #$0002, #$0003, #$009C, #$0009, #$0086, #$007F,
      #$0097, #$008D, #$008E, #$000B, #$000C, #$000D, #$000E, #$000F,
      #$0010, #$0011, #$0012, #$0013, #$009D, #$0085, #$0008, #$0087,
      #$0018, #$0019, #$0092, #$008F, #$001C, #$001D, #$001E, #$001F,
      #$0080, #$0081, #$0082, #$0083, #$0084, #$000A, #$0017, #$001B,
      #$0088, #$0089, #$008A, #$008B, #$008C, #$0005, #$0006, #$0007,
      #$0090, #$0091, #$0016, #$0093, #$0094, #$0095, #$0096, #$0004,
      #$0098, #$0099, #$009A, #$009B, #$0014, #$0015, #$009E, #$001A,
      #$0020, #$00A0, #$00E2, #$00E4, #$00E0, #$00E1, #$00E3, #$00E5,
      #$007B, #$00F1, #$00C7, #$002E, #$003C, #$0028, #$002B, #$0021,
      #$0026, #$00E9, #$00EA, #$00EB, #$00E8, #$00ED, #$00EE, #$00EF,
      #$00EC, #$00DF, #$011E, #$0130, #$002A, #$0029, #$003B, #$005E,
      #$002D, #$002F, #$00C2, #$00C4, #$00C0, #$00C1, #$00C3, #$00C5,
      #$005B, #$00D1, #$015F, #$002C, #$0025, #$005F, #$003E, #$003F,
      #$00F8, #$00C9, #$00CA, #$00CB, #$00C8, #$00CD, #$00CE, #$00CF,
      #$00CC, #$0131, #$003A, #$00D6, #$015E, #$0027, #$003D, #$00DC,
      #$00D8, #$0061, #$0062, #$0063, #$0064, #$0065, #$0066, #$0067,
      #$0068, #$0069, #$00AB, #$00BB, #$007D, #$0060, #$00A6, #$00B1,
      #$00B0, #$006A, #$006B, #$006C, #$006D, #$006E, #$006F, #$0070,
      #$0071, #$0072, #$00AA, #$00BA, #$00E6, #$FFFF, #$00C6, #$00A4,
      #$00B5, #$00F6, #$0073, #$0074, #$0075, #$0076, #$0077, #$0078,
      #$0079, #$007A, #$00A1, #$00BF, #$005D, #$0024, #$0040, #$00AE,
      #$00A2, #$00A3, #$00A5, #$00B7, #$00A9, #$00A7, #$00B6, #$00BC,
      #$00BD, #$00BE, #$00AC, #$007C, #$FFFF, #$00A8, #$00B4, #$00D7,
      #$00E7, #$0041, #$0042, #$0043, #$0044, #$0045, #$0046, #$0047,
      #$0048, #$0049, #$00AD, #$00F4, #$007E, #$00F2, #$00F3, #$00F5,
      #$011F, #$004A, #$004B, #$004C, #$004D, #$004E, #$004F, #$0050,
      #$0051, #$0052, #$00B9, #$00FB, #$005C, #$00F9, #$00FA, #$00FF,
      #$00FC, #$00F7, #$0053, #$0054, #$0055, #$0056, #$0057, #$0058,
      #$0059, #$005A, #$00B2, #$00D4, #$0023, #$00D2, #$00D3, #$00D5,
      #$0030, #$0031, #$0032, #$0033, #$0034, #$0035, #$0036, #$0037,
      #$0038, #$0039, #$00B3, #$00DB, #$0022, #$00D9, #$00DA, #$009F);



{                                                                              }
{ Windows-1026                                                                 }
{                                                                              }
function TWindows1026Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  case Ord(P) of
    $9D : Result := #$00B8;
    $BC : Result := #$00AF;
  else
    Result := CP1026Map[P];
  end;
end;

function TWindows1026Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  case Ord(Ch) of
    $00B8 : Result := AnsiChar($9D);
    $00AF : Result := AnsiChar($BC);
  else
    Result := CharFromMap(Ch, CP1026Map, 'Windows-1026');
  end;
end;



{                                                                              }
{ IBM1026                                                                      }
{                                                                              }
function TIBM1026Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  case Ord(P) of
    $9D : Result := #$02DB;
    $BC : Result := #$2014;
  else
    Result := CP1026Map[P];
  end;
end;

function TIBM1026Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  case Ord(Ch) of
    $02DB : Result := AnsiChar($9D);
    $2014 : Result := AnsiChar($BC);
  else
    Result := CharFromMap(Ch, CP1026Map, 'IBM1026');
  end;
end;



{                                                                              }
{ CP1047                                                                       }
{   Map shared by IBM1047 and Windows-1047.                                    }
{   Similar to CP37.                                                           }
{                                                                              }
const
  CP1047Map : AnsiCharMap = (
      #$0000, #$0001, #$0002, #$0003, #$009C, #$0009, #$0086, #$007F,
      #$0097, #$008D, #$008E, #$000B, #$000C, #$000D, #$000E, #$000F,
      #$0010, #$0011, #$0012, #$0013, #$009D, #$FFFF, #$0008, #$0087,
      #$0018, #$0019, #$0092, #$008F, #$001C, #$001D, #$001E, #$001F,
      #$0080, #$0081, #$0082, #$0083, #$0084, #$FFFF, #$0017, #$001B,
      #$0088, #$0089, #$008A, #$008B, #$008C, #$0005, #$0006, #$0007,
      #$0090, #$0091, #$0016, #$0093, #$0094, #$0095, #$0096, #$0004,
      #$0098, #$0099, #$009A, #$009B, #$0014, #$0015, #$009E, #$001A,
      #$0020, #$00A0, #$00E2, #$00E4, #$00E0, #$00E1, #$00E3, #$00E5,
      #$00E7, #$00F1, #$00A2, #$002E, #$003C, #$0028, #$002B, #$007C,
      #$0026, #$00E9, #$00EA, #$00EB, #$00E8, #$00ED, #$00EE, #$00EF,
      #$00EC, #$00DF, #$0021, #$0024, #$002A, #$0029, #$003B, #$005E,
      #$002D, #$002F, #$00C2, #$00C4, #$00C0, #$00C1, #$00C3, #$00C5,
      #$00C7, #$00D1, #$00A6, #$002C, #$0025, #$005F, #$003E, #$003F,
      #$00F8, #$00C9, #$00CA, #$00CB, #$00C8, #$00CD, #$00CE, #$00CF,
      #$00CC, #$0060, #$003A, #$0023, #$0040, #$0027, #$003D, #$0022,
      #$00D8, #$0061, #$0062, #$0063, #$0064, #$0065, #$0066, #$0067,
      #$0068, #$0069, #$00AB, #$00BB, #$00F0, #$00FD, #$00FE, #$00B1,
      #$00B0, #$006A, #$006B, #$006C, #$006D, #$006E, #$006F, #$0070,
      #$0071, #$0072, #$00AA, #$00BA, #$00E6, #$00B8, #$00C6, #$00A4,
      #$00B5, #$007E, #$0073, #$0074, #$0075, #$0076, #$0077, #$0078,
      #$0079, #$007A, #$00A1, #$00BF, #$00D0, #$005B, #$00DE, #$00AE,
      #$00AC, #$00A3, #$00A5, #$00B7, #$00A9, #$00A7, #$00B6, #$00BC,
      #$00BD, #$00BE, #$00DD, #$00A8, #$00AF, #$005D, #$00B4, #$00D7,
      #$007B, #$0041, #$0042, #$0043, #$0044, #$0045, #$0046, #$0047,
      #$0048, #$0049, #$00AD, #$00F4, #$00F6, #$00F2, #$00F3, #$00F5,
      #$007D, #$004A, #$004B, #$004C, #$004D, #$004E, #$004F, #$0050,
      #$0051, #$0052, #$00B9, #$00FB, #$00FC, #$00F9, #$00FA, #$00FF,
      #$005C, #$00F7, #$0053, #$0054, #$0055, #$0056, #$0057, #$0058,
      #$0059, #$005A, #$00B2, #$00D4, #$00D6, #$00D2, #$00D3, #$00D5,
      #$0030, #$0031, #$0032, #$0033, #$0034, #$0035, #$0036, #$0037,
      #$0038, #$0039, #$00B3, #$00DB, #$00DC, #$00D9, #$00DA, #$009F);



{                                                                              }
{ Windows-1047                                                                 }
{                                                                              }
function TWindows1047Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  case Ord(P) of
    $15 : Result := #$000A;
    $25 : Result := #$0085;
  else
    Result := CP1047Map[P];
  end;
end;

function TWindows1047Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  case Ord(Ch) of
    $000A : Result := AnsiChar($15);
    $0085 : Result := AnsiChar($25);
  else
    Result := CharFromMap(Ch, CP1047Map, 'Windows-1047');
  end;
end;



{                                                                              }
{ IBM1047                                                                      }
{                                                                              }
function TIBM1047Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  case Ord(P) of
    $15 : Result := #$0085;
    $25 : Result := #$000A;
  else
    Result := CP1047Map[P];
  end;
end;

function TIBM1047Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  case Ord(Ch) of
    $000A : Result := AnsiChar($25);
    $0085 : Result := AnsiChar($15);
  else
    Result := CharFromMap(Ch, CP1047Map, 'IBM1047');
  end;
end;



{                                                                              }
{ Windows-1140                                                                 }
{   Similar to CP37.                                                           }
{                                                                              }
function TWindows1140Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  case Ord(P) of
    $9F : Result := #$20AC;
  else
    Result := CP37Map[P];
  end;
end;

function TWindows1140Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  case Ord(Ch) of
    $20AC : Result := AnsiChar($9F);
  else
    begin
      Result := CharFromMap(Ch, CP37Map, 'Windows-1140');
      if Result = AnsiChar($9F) then
        raise EConvertError.CreateFmt(SCannotConvert, [Ord(Ch), 'Windows-1140']);
    end;
  end;
end;



{                                                                              }
{ Windows-1141                                                                 }
{   Similar to IBM273.                                                         }
{                                                                              }
const
  Windows1141Map : AnsiCharMap = (
      #$0000, #$0001, #$0002, #$0003, #$009C, #$0009, #$0086, #$007F,
      #$0097, #$008D, #$008E, #$000B, #$000C, #$000D, #$000E, #$000F,
      #$0010, #$0011, #$0012, #$0013, #$009D, #$0085, #$0008, #$0087,
      #$0018, #$0019, #$0092, #$008F, #$001C, #$001D, #$001E, #$001F,
      #$0080, #$0081, #$0082, #$0083, #$0084, #$000A, #$0017, #$001B,
      #$0088, #$0089, #$008A, #$008B, #$008C, #$0005, #$0006, #$0007,
      #$0090, #$0091, #$0016, #$0093, #$0094, #$0095, #$0096, #$0004,
      #$0098, #$0099, #$009A, #$009B, #$0014, #$0015, #$009E, #$001A,
      #$0020, #$00A0, #$00E2, #$007B, #$00E0, #$00E1, #$00E3, #$00E5,
      #$00E7, #$00F1, #$00C4, #$002E, #$003C, #$0028, #$002B, #$0021, 
      #$0026, #$00E9, #$00EA, #$00EB, #$00E8, #$00ED, #$00EE, #$00EF, 
      #$00EC, #$007E, #$00DC, #$0024, #$002A, #$0029, #$003B, #$005E, 
      #$002D, #$002F, #$00C2, #$005B, #$00C0, #$00C1, #$00C3, #$00C5,
      #$00C7, #$00D1, #$00F6, #$002C, #$0025, #$005F, #$003E, #$003F, 
      #$00F8, #$00C9, #$00CA, #$00CB, #$00C8, #$00CD, #$00CE, #$00CF,
      #$00CC, #$0060, #$003A, #$0023, #$00A7, #$0027, #$003D, #$0022, 
      #$00D8, #$0061, #$0062, #$0063, #$0064, #$0065, #$0066, #$0067, 
      #$0068, #$0069, #$00AB, #$00BB, #$00F0, #$00FD, #$00FE, #$00B1,
      #$00B0, #$006A, #$006B, #$006C, #$006D, #$006E, #$006F, #$0070, 
      #$0071, #$0072, #$00AA, #$00BA, #$00E6, #$00B8, #$00C6, #$20AC,
      #$00B5, #$00DF, #$0073, #$0074, #$0075, #$0076, #$0077, #$0078, 
      #$0079, #$007A, #$00A1, #$00BF, #$00D0, #$00DD, #$00DE, #$00AE, 
      #$00A2, #$00A3, #$00A5, #$00B7, #$00A9, #$0040, #$00B6, #$00BC,
      #$00BD, #$00BE, #$00AC, #$007C, #$00AF, #$00A8, #$00B4, #$00D7,
      #$00E4, #$0041, #$0042, #$0043, #$0044, #$0045, #$0046, #$0047,
      #$0048, #$0049, #$00AD, #$00F4, #$00A6, #$00F2, #$00F3, #$00F5,
      #$00FC, #$004A, #$004B, #$004C, #$004D, #$004E, #$004F, #$0050,
      #$0051, #$0052, #$00B9, #$00FB, #$007D, #$00F9, #$00FA, #$00FF,
      #$00D6, #$00F7, #$0053, #$0054, #$0055, #$0056, #$0057, #$0058,
      #$0059, #$005A, #$00B2, #$00D4, #$005C, #$00D2, #$00D3, #$00D5,
      #$0030, #$0031, #$0032, #$0033, #$0034, #$0035, #$0036, #$0037,
      #$0038, #$0039, #$00B3, #$00DB, #$005D, #$00D9, #$00DA, #$009F);

function TWindows1141Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  Result := Windows1141Map[P];
end;

function TWindows1141Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromMap(Ch, Windows1141Map, 'Windows-1141');
end;



{                                                                              }
{ Windows-1142                                                                 }
{   Similar to IBM277.                                                         }
{                                                                              }
const
  Windows1142Map : AnsiCharMap = (
      #$0000, #$0001, #$0002, #$0003, #$009C, #$0009, #$0086, #$007F, 
      #$0097, #$008D, #$008E, #$000B, #$000C, #$000D, #$000E, #$000F, 
      #$0010, #$0011, #$0012, #$0013, #$009D, #$0085, #$0008, #$0087,
      #$0018, #$0019, #$0092, #$008F, #$001C, #$001D, #$001E, #$001F,
      #$0080, #$0081, #$0082, #$0083, #$0084, #$000A, #$0017, #$001B, 
      #$0088, #$0089, #$008A, #$008B, #$008C, #$0005, #$0006, #$0007, 
      #$0090, #$0091, #$0016, #$0093, #$0094, #$0095, #$0096, #$0004,
      #$0098, #$0099, #$009A, #$009B, #$0014, #$0015, #$009E, #$001A, 
      #$0020, #$00A0, #$00E2, #$00E4, #$00E0, #$00E1, #$00E3, #$007D, 
      #$00E7, #$00F1, #$0023, #$002E, #$003C, #$0028, #$002B, #$0021, 
      #$0026, #$00E9, #$00EA, #$00EB, #$00E8, #$00ED, #$00EE, #$00EF,
      #$00EC, #$00DF, #$20AC, #$00C5, #$002A, #$0029, #$003B, #$005E, 
      #$002D, #$002F, #$00C2, #$00C4, #$00C0, #$00C1, #$00C3, #$0024, 
      #$00C7, #$00D1, #$00F8, #$002C, #$0025, #$005F, #$003E, #$003F,
      #$00A6, #$00C9, #$00CA, #$00CB, #$00C8, #$00CD, #$00CE, #$00CF, 
      #$00CC, #$0060, #$003A, #$00C6, #$00D8, #$0027, #$003D, #$0022, 
      #$0040, #$0061, #$0062, #$0063, #$0064, #$0065, #$0066, #$0067, 
      #$0068, #$0069, #$00AB, #$00BB, #$00F0, #$00FD, #$00FE, #$00B1, 
      #$00B0, #$006A, #$006B, #$006C, #$006D, #$006E, #$006F, #$0070, 
      #$0071, #$0072, #$00AA, #$00BA, #$007B, #$00B8, #$005B, #$005D,
      #$00B5, #$00FC, #$0073, #$0074, #$0075, #$0076, #$0077, #$0078, 
      #$0079, #$007A, #$00A1, #$00BF, #$00D0, #$00DD, #$00DE, #$00AE, 
      #$00A2, #$00A3, #$00A5, #$00B7, #$00A9, #$00A7, #$00B6, #$00BC, 
      #$00BD, #$00BE, #$00AC, #$007C, #$00AF, #$00A8, #$00B4, #$00D7, 
      #$00E6, #$0041, #$0042, #$0043, #$0044, #$0045, #$0046, #$0047,
      #$0048, #$0049, #$00AD, #$00F4, #$00F6, #$00F2, #$00F3, #$00F5,
      #$00E5, #$004A, #$004B, #$004C, #$004D, #$004E, #$004F, #$0050, 
      #$0051, #$0052, #$00B9, #$00FB, #$007E, #$00F9, #$00FA, #$00FF,
      #$005C, #$00F7, #$0053, #$0054, #$0055, #$0056, #$0057, #$0058,
      #$0059, #$005A, #$00B2, #$00D4, #$00D6, #$00D2, #$00D3, #$00D5, 
      #$0030, #$0031, #$0032, #$0033, #$0034, #$0035, #$0036, #$0037,
      #$0038, #$0039, #$00B3, #$00DB, #$00DC, #$00D9, #$00DA, #$009F);

function TWindows1142Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  Result := Windows1142Map[P];
end;

function TWindows1142Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromMap(Ch, Windows1142Map, 'Windows-1142');
end;



{                                                                              }
{ Windows-1143                                                                 }
{   Similar to IBM278.                                                         }
{                                                                              }
const
  Windows1143Map : AnsiCharMap = (
      #$0000, #$0001, #$0002, #$0003, #$009C, #$0009, #$0086, #$007F,
      #$0097, #$008D, #$008E, #$000B, #$000C, #$000D, #$000E, #$000F,
      #$0010, #$0011, #$0012, #$0013, #$009D, #$0085, #$0008, #$0087,
      #$0018, #$0019, #$0092, #$008F, #$001C, #$001D, #$001E, #$001F,
      #$0080, #$0081, #$0082, #$0083, #$0084, #$000A, #$0017, #$001B,
      #$0088, #$0089, #$008A, #$008B, #$008C, #$0005, #$0006, #$0007, 
      #$0090, #$0091, #$0016, #$0093, #$0094, #$0095, #$0096, #$0004,
      #$0098, #$0099, #$009A, #$009B, #$0014, #$0015, #$009E, #$001A, 
      #$0020, #$00A0, #$00E2, #$007B, #$00E0, #$00E1, #$00E3, #$007D,
      #$00E7, #$00F1, #$00A7, #$002E, #$003C, #$0028, #$002B, #$0021, 
      #$0026, #$0060, #$00EA, #$00EB, #$00E8, #$00ED, #$00EE, #$00EF,
      #$00EC, #$00DF, #$20AC, #$00C5, #$002A, #$0029, #$003B, #$005E,
      #$002D, #$002F, #$00C2, #$0023, #$00C0, #$00C1, #$00C3, #$0024, 
      #$00C7, #$00D1, #$00F6, #$002C, #$0025, #$005F, #$003E, #$003F, 
      #$00F8, #$005C, #$00CA, #$00CB, #$00C8, #$00CD, #$00CE, #$00CF,
      #$00CC, #$00E9, #$003A, #$00C4, #$00D6, #$0027, #$003D, #$0022, 
      #$00D8, #$0061, #$0062, #$0063, #$0064, #$0065, #$0066, #$0067, 
      #$0068, #$0069, #$00AB, #$00BB, #$00F0, #$00FD, #$00FE, #$00B1, 
      #$00B0, #$006A, #$006B, #$006C, #$006D, #$006E, #$006F, #$0070,
      #$0071, #$0072, #$00AA, #$00BA, #$00E6, #$00B8, #$00C6, #$005D,
      #$00B5, #$00FC, #$0073, #$0074, #$0075, #$0076, #$0077, #$0078,
      #$0079, #$007A, #$00A1, #$00BF, #$00D0, #$00DD, #$00DE, #$00AE,
      #$00A2, #$00A3, #$00A5, #$00B7, #$00A9, #$005B, #$00B6, #$00BC, 
      #$00BD, #$00BE, #$00AC, #$007C, #$00AF, #$00A8, #$00B4, #$00D7, 
      #$00E4, #$0041, #$0042, #$0043, #$0044, #$0045, #$0046, #$0047,
      #$0048, #$0049, #$00AD, #$00F4, #$00A6, #$00F2, #$00F3, #$00F5,
      #$00E5, #$004A, #$004B, #$004C, #$004D, #$004E, #$004F, #$0050,
      #$0051, #$0052, #$00B9, #$00FB, #$007E, #$00F9, #$00FA, #$00FF, 
      #$00C9, #$00F7, #$0053, #$0054, #$0055, #$0056, #$0057, #$0058, 
      #$0059, #$005A, #$00B2, #$00D4, #$0040, #$00D2, #$00D3, #$00D5, 
      #$0030, #$0031, #$0032, #$0033, #$0034, #$0035, #$0036, #$0037, 
      #$0038, #$0039, #$00B3, #$00DB, #$00DC, #$00D9, #$00DA, #$009F);

function TWindows1143Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  Result := Windows1143Map[P];
end;

function TWindows1143Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromMap(Ch, Windows1143Map, 'Windows-1143');
end;



{                                                                              }
{ Windows-1144                                                                 }
{   Similar to IBM280.                                                         }
{                                                                              }
const
  Windows1144Map : AnsiCharMap = (
      #$0000, #$0001, #$0002, #$0003, #$009C, #$0009, #$0086, #$007F,
      #$0097, #$008D, #$008E, #$000B, #$000C, #$000D, #$000E, #$000F,
      #$0010, #$0011, #$0012, #$0013, #$009D, #$0085, #$0008, #$0087,
      #$0018, #$0019, #$0092, #$008F, #$001C, #$001D, #$001E, #$001F,
      #$0080, #$0081, #$0082, #$0083, #$0084, #$000A, #$0017, #$001B,
      #$0088, #$0089, #$008A, #$008B, #$008C, #$0005, #$0006, #$0007, 
      #$0090, #$0091, #$0016, #$0093, #$0094, #$0095, #$0096, #$0004,
      #$0098, #$0099, #$009A, #$009B, #$0014, #$0015, #$009E, #$001A, 
      #$0020, #$00A0, #$00E2, #$00E4, #$007B, #$00E1, #$00E3, #$00E5, 
      #$005C, #$00F1, #$00B0, #$002E, #$003C, #$0028, #$002B, #$0021, 
      #$0026, #$005D, #$00EA, #$00EB, #$007D, #$00ED, #$00EE, #$00EF, 
      #$007E, #$00DF, #$00E9, #$0024, #$002A, #$0029, #$003B, #$005E, 
      #$002D, #$002F, #$00C2, #$00C4, #$00C0, #$00C1, #$00C3, #$00C5, 
      #$00C7, #$00D1, #$00F2, #$002C, #$0025, #$005F, #$003E, #$003F, 
      #$00F8, #$00C9, #$00CA, #$00CB, #$00C8, #$00CD, #$00CE, #$00CF, 
      #$00CC, #$00F9, #$003A, #$00A3, #$00A7, #$0027, #$003D, #$0022,
      #$00D8, #$0061, #$0062, #$0063, #$0064, #$0065, #$0066, #$0067, 
      #$0068, #$0069, #$00AB, #$00BB, #$00F0, #$00FD, #$00FE, #$00B1, 
      #$005B, #$006A, #$006B, #$006C, #$006D, #$006E, #$006F, #$0070, 
      #$0071, #$0072, #$00AA, #$00BA, #$00E6, #$00B8, #$00C6, #$20AC,
      #$00B5, #$00EC, #$0073, #$0074, #$0075, #$0076, #$0077, #$0078,
      #$0079, #$007A, #$00A1, #$00BF, #$00D0, #$00DD, #$00DE, #$00AE,
      #$00A2, #$0023, #$00A5, #$00B7, #$00A9, #$0040, #$00B6, #$00BC, 
      #$00BD, #$00BE, #$00AC, #$007C, #$00AF, #$00A8, #$00B4, #$00D7,
      #$00E0, #$0041, #$0042, #$0043, #$0044, #$0045, #$0046, #$0047,
      #$0048, #$0049, #$00AD, #$00F4, #$00F6, #$00A6, #$00F3, #$00F5,
      #$00E8, #$004A, #$004B, #$004C, #$004D, #$004E, #$004F, #$0050, 
      #$0051, #$0052, #$00B9, #$00FB, #$00FC, #$0060, #$00FA, #$00FF, 
      #$00E7, #$00F7, #$0053, #$0054, #$0055, #$0056, #$0057, #$0058, 
      #$0059, #$005A, #$00B2, #$00D4, #$00D6, #$00D2, #$00D3, #$00D5,
      #$0030, #$0031, #$0032, #$0033, #$0034, #$0035, #$0036, #$0037, 
      #$0038, #$0039, #$00B3, #$00DB, #$00DC, #$00D9, #$00DA, #$009F);

function TWindows1144Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  Result := Windows1144Map[P];
end;

function TWindows1144Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromMap(Ch, Windows1144Map, 'Windows-1144');
end;



{                                                                              }
{ Windows-1145                                                                 }
{   Similar to IBM284.                                                         }
{                                                                              }
const
  Windows1145Map : AnsiCharMap = (
      #$0000, #$0001, #$0002, #$0003, #$009C, #$0009, #$0086, #$007F,
      #$0097, #$008D, #$008E, #$000B, #$000C, #$000D, #$000E, #$000F,
      #$0010, #$0011, #$0012, #$0013, #$009D, #$0085, #$0008, #$0087,
      #$0018, #$0019, #$0092, #$008F, #$001C, #$001D, #$001E, #$001F,
      #$0080, #$0081, #$0082, #$0083, #$0084, #$000A, #$0017, #$001B,
      #$0088, #$0089, #$008A, #$008B, #$008C, #$0005, #$0006, #$0007,
      #$0090, #$0091, #$0016, #$0093, #$0094, #$0095, #$0096, #$0004,
      #$0098, #$0099, #$009A, #$009B, #$0014, #$0015, #$009E, #$001A, 
      #$0020, #$00A0, #$00E2, #$00E4, #$00E0, #$00E1, #$00E3, #$00E5,
      #$00E7, #$00A6, #$005B, #$002E, #$003C, #$0028, #$002B, #$007C, 
      #$0026, #$00E9, #$00EA, #$00EB, #$00E8, #$00ED, #$00EE, #$00EF,
      #$00EC, #$00DF, #$005D, #$0024, #$002A, #$0029, #$003B, #$00AC,
      #$002D, #$002F, #$00C2, #$00C4, #$00C0, #$00C1, #$00C3, #$00C5, 
      #$00C7, #$0023, #$00F1, #$002C, #$0025, #$005F, #$003E, #$003F, 
      #$00F8, #$00C9, #$00CA, #$00CB, #$00C8, #$00CD, #$00CE, #$00CF, 
      #$00CC, #$0060, #$003A, #$00D1, #$0040, #$0027, #$003D, #$0022, 
      #$00D8, #$0061, #$0062, #$0063, #$0064, #$0065, #$0066, #$0067, 
      #$0068, #$0069, #$00AB, #$00BB, #$00F0, #$00FD, #$00FE, #$00B1, 
      #$00B0, #$006A, #$006B, #$006C, #$006D, #$006E, #$006F, #$0070, 
      #$0071, #$0072, #$00AA, #$00BA, #$00E6, #$00B8, #$00C6, #$20AC,
      #$00B5, #$00A8, #$0073, #$0074, #$0075, #$0076, #$0077, #$0078, 
      #$0079, #$007A, #$00A1, #$00BF, #$00D0, #$00DD, #$00DE, #$00AE, 
      #$00A2, #$00A3, #$00A5, #$00B7, #$00A9, #$00A7, #$00B6, #$00BC, 
      #$00BD, #$00BE, #$005E, #$0021, #$00AF, #$007E, #$00B4, #$00D7, 
      #$007B, #$0041, #$0042, #$0043, #$0044, #$0045, #$0046, #$0047,
      #$0048, #$0049, #$00AD, #$00F4, #$00F6, #$00F2, #$00F3, #$00F5,
      #$007D, #$004A, #$004B, #$004C, #$004D, #$004E, #$004F, #$0050,
      #$0051, #$0052, #$00B9, #$00FB, #$00FC, #$00F9, #$00FA, #$00FF,
      #$005C, #$00F7, #$0053, #$0054, #$0055, #$0056, #$0057, #$0058,
      #$0059, #$005A, #$00B2, #$00D4, #$00D6, #$00D2, #$00D3, #$00D5, 
      #$0030, #$0031, #$0032, #$0033, #$0034, #$0035, #$0036, #$0037,
      #$0038, #$0039, #$00B3, #$00DB, #$00DC, #$00D9, #$00DA, #$009F);

function TWindows1145Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  Result := Windows1145Map[P];
end;

function TWindows1145Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromMap(Ch, Windows1145Map, 'Windows-1145');
end;



{                                                                              }
{ Windows-1146                                                                 }
{   Similar to IBM285.                                                         }
{                                                                              }
const
  Windows1146Map : AnsiCharMap = (
      #$0000, #$0001, #$0002, #$0003, #$009C, #$0009, #$0086, #$007F,
      #$0097, #$008D, #$008E, #$000B, #$000C, #$000D, #$000E, #$000F,
      #$0010, #$0011, #$0012, #$0013, #$009D, #$0085, #$0008, #$0087,
      #$0018, #$0019, #$0092, #$008F, #$001C, #$001D, #$001E, #$001F,
      #$0080, #$0081, #$0082, #$0083, #$0084, #$000A, #$0017, #$001B,
      #$0088, #$0089, #$008A, #$008B, #$008C, #$0005, #$0006, #$0007,
      #$0090, #$0091, #$0016, #$0093, #$0094, #$0095, #$0096, #$0004,
      #$0098, #$0099, #$009A, #$009B, #$0014, #$0015, #$009E, #$001A, 
      #$0020, #$00A0, #$00E2, #$00E4, #$00E0, #$00E1, #$00E3, #$00E5, 
      #$00E7, #$00F1, #$0024, #$002E, #$003C, #$0028, #$002B, #$007C, 
      #$0026, #$00E9, #$00EA, #$00EB, #$00E8, #$00ED, #$00EE, #$00EF,
      #$00EC, #$00DF, #$0021, #$00A3, #$002A, #$0029, #$003B, #$00AC,
      #$002D, #$002F, #$00C2, #$00C4, #$00C0, #$00C1, #$00C3, #$00C5,
      #$00C7, #$00D1, #$00A6, #$002C, #$0025, #$005F, #$003E, #$003F,
      #$00F8, #$00C9, #$00CA, #$00CB, #$00C8, #$00CD, #$00CE, #$00CF, 
      #$00CC, #$0060, #$003A, #$0023, #$0040, #$0027, #$003D, #$0022,
      #$00D8, #$0061, #$0062, #$0063, #$0064, #$0065, #$0066, #$0067,
      #$0068, #$0069, #$00AB, #$00BB, #$00F0, #$00FD, #$00FE, #$00B1, 
      #$00B0, #$006A, #$006B, #$006C, #$006D, #$006E, #$006F, #$0070,
      #$0071, #$0072, #$00AA, #$00BA, #$00E6, #$00B8, #$00C6, #$20AC,
      #$00B5, #$00AF, #$0073, #$0074, #$0075, #$0076, #$0077, #$0078, 
      #$0079, #$007A, #$00A1, #$00BF, #$00D0, #$00DD, #$00DE, #$00AE, 
      #$00A2, #$005B, #$00A5, #$00B7, #$00A9, #$00A7, #$00B6, #$00BC, 
      #$00BD, #$00BE, #$005E, #$005D, #$007E, #$00A8, #$00B4, #$00D7, 
      #$007B, #$0041, #$0042, #$0043, #$0044, #$0045, #$0046, #$0047,
      #$0048, #$0049, #$00AD, #$00F4, #$00F6, #$00F2, #$00F3, #$00F5,
      #$007D, #$004A, #$004B, #$004C, #$004D, #$004E, #$004F, #$0050, 
      #$0051, #$0052, #$00B9, #$00FB, #$00FC, #$00F9, #$00FA, #$00FF, 
      #$005C, #$00F7, #$0053, #$0054, #$0055, #$0056, #$0057, #$0058,
      #$0059, #$005A, #$00B2, #$00D4, #$00D6, #$00D2, #$00D3, #$00D5, 
      #$0030, #$0031, #$0032, #$0033, #$0034, #$0035, #$0036, #$0037, 
      #$0038, #$0039, #$00B3, #$00DB, #$00DC, #$00D9, #$00DA, #$009F);

function TWindows1146Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  Result := Windows1146Map[P];
end;

function TWindows1146Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromMap(Ch, Windows1146Map, 'Windows-1146');
end;



{                                                                              }
{ Windows-1147                                                                 }
{   Similar to IBM297.                                                         }
{                                                                              }
const
  Windows1147Map : AnsiCharMap = (
      #$0000, #$0001, #$0002, #$0003, #$009C, #$0009, #$0086, #$007F,
      #$0097, #$008D, #$008E, #$000B, #$000C, #$000D, #$000E, #$000F,
      #$0010, #$0011, #$0012, #$0013, #$009D, #$0085, #$0008, #$0087,
      #$0018, #$0019, #$0092, #$008F, #$001C, #$001D, #$001E, #$001F,
      #$0080, #$0081, #$0082, #$0083, #$0084, #$000A, #$0017, #$001B,
      #$0088, #$0089, #$008A, #$008B, #$008C, #$0005, #$0006, #$0007, 
      #$0090, #$0091, #$0016, #$0093, #$0094, #$0095, #$0096, #$0004, 
      #$0098, #$0099, #$009A, #$009B, #$0014, #$0015, #$009E, #$001A, 
      #$0020, #$00A0, #$00E2, #$00E4, #$0040, #$00E1, #$00E3, #$00E5, 
      #$005C, #$00F1, #$00B0, #$002E, #$003C, #$0028, #$002B, #$0021, 
      #$0026, #$007B, #$00EA, #$00EB, #$007D, #$00ED, #$00EE, #$00EF,
      #$00EC, #$00DF, #$00A7, #$0024, #$002A, #$0029, #$003B, #$005E, 
      #$002D, #$002F, #$00C2, #$00C4, #$00C0, #$00C1, #$00C3, #$00C5, 
      #$00C7, #$00D1, #$00F9, #$002C, #$0025, #$005F, #$003E, #$003F, 
      #$00F8, #$00C9, #$00CA, #$00CB, #$00C8, #$00CD, #$00CE, #$00CF, 
      #$00CC, #$00B5, #$003A, #$00A3, #$00E0, #$0027, #$003D, #$0022, 
      #$00D8, #$0061, #$0062, #$0063, #$0064, #$0065, #$0066, #$0067, 
      #$0068, #$0069, #$00AB, #$00BB, #$00F0, #$00FD, #$00FE, #$00B1, 
      #$005B, #$006A, #$006B, #$006C, #$006D, #$006E, #$006F, #$0070, 
      #$0071, #$0072, #$00AA, #$00BA, #$00E6, #$00B8, #$00C6, #$20AC,
      #$0060, #$00A8, #$0073, #$0074, #$0075, #$0076, #$0077, #$0078,
      #$0079, #$007A, #$00A1, #$00BF, #$00D0, #$00DD, #$00DE, #$00AE,
      #$00A2, #$0023, #$00A5, #$00B7, #$00A9, #$005D, #$00B6, #$00BC,
      #$00BD, #$00BE, #$00AC, #$007C, #$00AF, #$007E, #$00B4, #$00D7,
      #$00E9, #$0041, #$0042, #$0043, #$0044, #$0045, #$0046, #$0047,
      #$0048, #$0049, #$00AD, #$00F4, #$00F6, #$00F2, #$00F3, #$00F5,
      #$00E8, #$004A, #$004B, #$004C, #$004D, #$004E, #$004F, #$0050,
      #$0051, #$0052, #$00B9, #$00FB, #$00FC, #$00A6, #$00FA, #$00FF,
      #$00E7, #$00F7, #$0053, #$0054, #$0055, #$0056, #$0057, #$0058,
      #$0059, #$005A, #$00B2, #$00D4, #$00D6, #$00D2, #$00D3, #$00D5,
      #$0030, #$0031, #$0032, #$0033, #$0034, #$0035, #$0036, #$0037,
      #$0038, #$0039, #$00B3, #$00DB, #$00DC, #$00D9, #$00DA, #$009F);

function TWindows1147Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  Result := Windows1147Map[P];
end;

function TWindows1147Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromMap(Ch, Windows1147Map, 'Windows-1147');
end;



{                                                                              }
{ Windows-1148                                                                 }
{   Similar to CP500.                                                          }
{                                                                              }
const
  Windows1148Map : AnsiCharMap = (
      #$0000, #$0001, #$0002, #$0003, #$009C, #$0009, #$0086, #$007F,
      #$0097, #$008D, #$008E, #$000B, #$000C, #$000D, #$000E, #$000F,
      #$0010, #$0011, #$0012, #$0013, #$009D, #$0085, #$0008, #$0087,
      #$0018, #$0019, #$0092, #$008F, #$001C, #$001D, #$001E, #$001F,
      #$0080, #$0081, #$0082, #$0083, #$0084, #$000A, #$0017, #$001B,
      #$0088, #$0089, #$008A, #$008B, #$008C, #$0005, #$0006, #$0007,
      #$0090, #$0091, #$0016, #$0093, #$0094, #$0095, #$0096, #$0004, 
      #$0098, #$0099, #$009A, #$009B, #$0014, #$0015, #$009E, #$001A,
      #$0020, #$00A0, #$00E2, #$00E4, #$00E0, #$00E1, #$00E3, #$00E5, 
      #$00E7, #$00F1, #$005B, #$002E, #$003C, #$0028, #$002B, #$0021,
      #$0026, #$00E9, #$00EA, #$00EB, #$00E8, #$00ED, #$00EE, #$00EF, 
      #$00EC, #$00DF, #$005D, #$0024, #$002A, #$0029, #$003B, #$005E, 
      #$002D, #$002F, #$00C2, #$00C4, #$00C0, #$00C1, #$00C3, #$00C5, 
      #$00C7, #$00D1, #$00A6, #$002C, #$0025, #$005F, #$003E, #$003F, 
      #$00F8, #$00C9, #$00CA, #$00CB, #$00C8, #$00CD, #$00CE, #$00CF, 
      #$00CC, #$0060, #$003A, #$0023, #$0040, #$0027, #$003D, #$0022,
      #$00D8, #$0061, #$0062, #$0063, #$0064, #$0065, #$0066, #$0067, 
      #$0068, #$0069, #$00AB, #$00BB, #$00F0, #$00FD, #$00FE, #$00B1, 
      #$00B0, #$006A, #$006B, #$006C, #$006D, #$006E, #$006F, #$0070,
      #$0071, #$0072, #$00AA, #$00BA, #$00E6, #$00B8, #$00C6, #$20AC,
      #$00B5, #$007E, #$0073, #$0074, #$0075, #$0076, #$0077, #$0078, 
      #$0079, #$007A, #$00A1, #$00BF, #$00D0, #$00DD, #$00DE, #$00AE, 
      #$00A2, #$00A3, #$00A5, #$00B7, #$00A9, #$00A7, #$00B6, #$00BC, 
      #$00BD, #$00BE, #$00AC, #$007C, #$00AF, #$00A8, #$00B4, #$00D7, 
      #$007B, #$0041, #$0042, #$0043, #$0044, #$0045, #$0046, #$0047,
      #$0048, #$0049, #$00AD, #$00F4, #$00F6, #$00F2, #$00F3, #$00F5,
      #$007D, #$004A, #$004B, #$004C, #$004D, #$004E, #$004F, #$0050, 
      #$0051, #$0052, #$00B9, #$00FB, #$00FC, #$00F9, #$00FA, #$00FF, 
      #$005C, #$00F7, #$0053, #$0054, #$0055, #$0056, #$0057, #$0058,
      #$0059, #$005A, #$00B2, #$00D4, #$00D6, #$00D2, #$00D3, #$00D5,
      #$0030, #$0031, #$0032, #$0033, #$0034, #$0035, #$0036, #$0037,
      #$0038, #$0039, #$00B3, #$00DB, #$00DC, #$00D9, #$00DA, #$009F);

function TWindows1148Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  Result := Windows1148Map[P];
end;

function TWindows1148Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromMap(Ch, Windows1148Map, 'Windows-1148');
end;



{                                                                              }
{ Windows-1149                                                                 }
{   Similar to IBM871.                                                         }
{                                                                              }
const
  Windows1149Map : AnsiCharMap = (
      #$0000, #$0001, #$0002, #$0003, #$009C, #$0009, #$0086, #$007F,
      #$0097, #$008D, #$008E, #$000B, #$000C, #$000D, #$000E, #$000F,
      #$0010, #$0011, #$0012, #$0013, #$009D, #$0085, #$0008, #$0087,
      #$0018, #$0019, #$0092, #$008F, #$001C, #$001D, #$001E, #$001F,
      #$0080, #$0081, #$0082, #$0083, #$0084, #$000A, #$0017, #$001B,
      #$0088, #$0089, #$008A, #$008B, #$008C, #$0005, #$0006, #$0007, 
      #$0090, #$0091, #$0016, #$0093, #$0094, #$0095, #$0096, #$0004, 
      #$0098, #$0099, #$009A, #$009B, #$0014, #$0015, #$009E, #$001A, 
      #$0020, #$00A0, #$00E2, #$00E4, #$00E0, #$00E1, #$00E3, #$00E5, 
      #$00E7, #$00F1, #$00DE, #$002E, #$003C, #$0028, #$002B, #$0021,
      #$0026, #$00E9, #$00EA, #$00EB, #$00E8, #$00ED, #$00EE, #$00EF,
      #$00EC, #$00DF, #$00C6, #$0024, #$002A, #$0029, #$003B, #$00D6,
      #$002D, #$002F, #$00C2, #$00C4, #$00C0, #$00C1, #$00C3, #$00C5,
      #$00C7, #$00D1, #$00A6, #$002C, #$0025, #$005F, #$003E, #$003F,
      #$00F8, #$00C9, #$00CA, #$00CB, #$00C8, #$00CD, #$00CE, #$00CF,
      #$00CC, #$00F0, #$003A, #$0023, #$00D0, #$0027, #$003D, #$0022,
      #$00D8, #$0061, #$0062, #$0063, #$0064, #$0065, #$0066, #$0067, 
      #$0068, #$0069, #$00AB, #$00BB, #$0060, #$00FD, #$007B, #$00B1, 
      #$00B0, #$006A, #$006B, #$006C, #$006D, #$006E, #$006F, #$0070,
      #$0071, #$0072, #$00AA, #$00BA, #$007D, #$00B8, #$005D, #$20AC,
      #$00B5, #$00F6, #$0073, #$0074, #$0075, #$0076, #$0077, #$0078,
      #$0079, #$007A, #$00A1, #$00BF, #$0040, #$00DD, #$005B, #$00AE, 
      #$00A2, #$00A3, #$00A5, #$00B7, #$00A9, #$00A7, #$00B6, #$00BC,
      #$00BD, #$00BE, #$00AC, #$007C, #$00AF, #$00A8, #$005C, #$00D7, 
      #$00FE, #$0041, #$0042, #$0043, #$0044, #$0045, #$0046, #$0047,
      #$0048, #$0049, #$00AD, #$00F4, #$007E, #$00F2, #$00F3, #$00F5,
      #$00E6, #$004A, #$004B, #$004C, #$004D, #$004E, #$004F, #$0050,
      #$0051, #$0052, #$00B9, #$00FB, #$00FC, #$00F9, #$00FA, #$00FF,
      #$00B4, #$00F7, #$0053, #$0054, #$0055, #$0056, #$0057, #$0058,
      #$0059, #$005A, #$00B2, #$00D4, #$005E, #$00D2, #$00D3, #$00D5,
      #$0030, #$0031, #$0032, #$0033, #$0034, #$0035, #$0036, #$0037,
      #$0038, #$0039, #$00B3, #$00DB, #$00DC, #$00D9, #$00DA, #$009F);

function TWindows1149Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  Result := Windows1149Map[P];
end;

function TWindows1149Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromMap(Ch, Windows1149Map, 'Windows-1149');
end;



{                                                                              }
{ Windows-1250                                                                 }
{                                                                              }
const
  Win1250Aliases = 3;
  Win1250Alias : array[0..Win1250Aliases - 1] of String = (
      'windows-1250', 'cp1250', 'WinLatin2');

class function TWindows1250Codec.GetAliasCount: Integer;
begin
  Result := Win1250Aliases;
end;

class function TWindows1250Codec.GetAlias(const Idx: Integer): String;
begin
  if Idx < 0 then
    raise EUnicodeCodecException.Create(SAliasIndexOutOfRange);
  if Idx >= GetAliasCount then
    raise EUnicodeCodecException.Create(SAliasIndexOutOfRange);
  Result := Win1250Alias[Idx];
end;

const
  Windows1250Map : AnsiCharHighMap = (
      #$20AC, #$0081, #$201A, #$0083, #$201E, #$2026, #$2020, #$2021, 
      #$0088, #$2030, #$0160, #$2039, #$015A, #$0164, #$017D, #$0179,
      #$0090, #$2018, #$2019, #$201C, #$201D, #$2022, #$2013, #$2014, 
      #$0098, #$2122, #$0161, #$203A, #$015B, #$0165, #$017E, #$017A, 
      #$00A0, #$02C7, #$02D8, #$0141, #$00A4, #$0104, #$00A6, #$00A7,
      #$00A8, #$00A9, #$015E, #$00AB, #$00AC, #$00AD, #$00AE, #$017B, 
      #$00B0, #$00B1, #$02DB, #$0142, #$00B4, #$00B5, #$00B6, #$00B7,
      #$00B8, #$0105, #$015F, #$00BB, #$013D, #$02DD, #$013E, #$017C,
      #$0154, #$00C1, #$00C2, #$0102, #$00C4, #$0139, #$0106, #$00C7,
      #$010C, #$00C9, #$0118, #$00CB, #$011A, #$00CD, #$00CE, #$010E,
      #$0110, #$0143, #$0147, #$00D3, #$00D4, #$0150, #$00D6, #$00D7,
      #$0158, #$016E, #$00DA, #$0170, #$00DC, #$00DD, #$0162, #$00DF,
      #$0155, #$00E1, #$00E2, #$0103, #$00E4, #$013A, #$0107, #$00E7,
      #$010D, #$00E9, #$0119, #$00EB, #$011B, #$00ED, #$00EE, #$010F,
      #$0111, #$0144, #$0148, #$00F3, #$00F4, #$0151, #$00F6, #$00F7,
      #$0159, #$016F, #$00FA, #$0171, #$00FC, #$00FD, #$0163, #$02D9);

function TWindows1250Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  if Ord(P) < $80 then
    Result := WideChar(P)
  else
    Result := Windows1250Map[Ord(P)];
end;

function TWindows1250Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromHighMap(Ch, Windows1250Map, 'Windows-1250');
end;



{                                                                              }
{ Windows-1251                                                                 }
{                                                                              }
const
  Win1251Aliases = 3;
  Win1251Alias : array[0..Win1251Aliases - 1] of String = (
      'windows-1251', 'cp1251', 'WinCyrillic');

class function TWindows1251Codec.GetAliasCount: Integer;
begin
  Result := Win1251Aliases;
end;

class function TWindows1251Codec.GetAlias(const Idx: Integer): String;
begin
  if Idx < 0 then
    raise EUnicodeCodecException.Create(SAliasIndexOutOfRange);
  if Idx >= GetAliasCount then
    raise EUnicodeCodecException.Create(SAliasIndexOutOfRange);
  Result := Win1251Alias[Idx];
end;

const
  Windows1251Map : AnsiCharHighMap = (
      #$0402, #$0403, #$201A, #$0453, #$201E, #$2026, #$2020, #$2021,
      #$20AC, #$2030, #$0409, #$2039, #$040A, #$040C, #$040B, #$040F,
      #$0452, #$2018, #$2019, #$201C, #$201D, #$2022, #$2013, #$2014,
      #$0098, #$2122, #$0459, #$203A, #$045A, #$045C, #$045B, #$045F,
      #$00A0, #$040E, #$045E, #$0408, #$00A4, #$0490, #$00A6, #$00A7,
      #$0401, #$00A9, #$0404, #$00AB, #$00AC, #$00AD, #$00AE, #$0407,
      #$00B0, #$00B1, #$0406, #$0456, #$0491, #$00B5, #$00B6, #$00B7,
      #$0451, #$2116, #$0454, #$00BB, #$0458, #$0405, #$0455, #$0457,
      #$0410, #$0411, #$0412, #$0413, #$0414, #$0415, #$0416, #$0417,
      #$0418, #$0419, #$041A, #$041B, #$041C, #$041D, #$041E, #$041F,
      #$0420, #$0421, #$0422, #$0423, #$0424, #$0425, #$0426, #$0427,
      #$0428, #$0429, #$042A, #$042B, #$042C, #$042D, #$042E, #$042F,
      #$0430, #$0431, #$0432, #$0433, #$0434, #$0435, #$0436, #$0437,
      #$0438, #$0439, #$043A, #$043B, #$043C, #$043D, #$043E, #$043F,
      #$0440, #$0441, #$0442, #$0443, #$0444, #$0445, #$0446, #$0447,
      #$0448, #$0449, #$044A, #$044B, #$044C, #$044D, #$044E, #$044F);

function TWindows1251Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  if Ord(P) < $80 then
    Result := WideChar(P)
  else
    Result := Windows1251Map[Ord(P)];
end;

function TWindows1251Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromHighMap(Ch, Windows1251Map, 'Windows-1251');
end;



{                                                                              }
{ Windows-1252                                                                 }
{                                                                              }
const
  Win1252Aliases = 3;
  Win1252Alias : array[0..Win1252Aliases - 1] of String = (
      'windows-1252', 'cp1252', 'WinLatin1');

class function TWindows1252Codec.GetAliasCount: Integer;
begin
  Result := Win1252Aliases;
end;

class function TWindows1252Codec.GetAlias(const Idx: Integer): String;
begin
  if Idx < 0 then
    raise EUnicodeCodecException.Create(SAliasIndexOutOfRange);
  if Idx >= GetAliasCount then
    raise EUnicodeCodecException.Create(SAliasIndexOutOfRange);
  Result := Win1252Alias[Idx];
end;

const
  Windows1252Map : array[$80..$9F] of WideChar = (
      #$20AC, #$0081, #$201A, #$0192, #$201E, #$2026, #$2020, #$2021,
      #$02C6, #$2030, #$0160, #$2039, #$0152, #$008D, #$017D, #$008F,
      #$0090, #$2018, #$2019, #$201C, #$201D, #$2022, #$2013, #$2014,
      #$02DC, #$2122, #$0161, #$203A, #$0153, #$009D, #$017E, #$0178);

function TWindows1252Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  if Ord(P) in [$80..$9F] then
    Result := Windows1252Map[Ord(P)]
  else
    Result := WideChar(P);
end;

function TWindows1252Codec.EncodeChar(const Ch: WideChar): ByteChar;
var I : Byte;
begin
  if (Ord(Ch) < $80) or
     ((Ord(Ch) < $100) and (Ord(Ch) > $9F)) then
    begin
      Result := AnsiChar(Ch);
      exit;
    end;
  for I := $80 to $9F do
    if Windows1252Map[I] = Ch then
      begin
        Result := AnsiChar(I);
        exit;
      end;
  raise EConvertError.CreateFmt(SCannotConvert, [Ord(Ch), 'Windows-1252']);
end;



{                                                                              }
{ Windows-1253                                                                 }
{                                                                              }
const
  Windows1253Map : AnsiCharHighMap = (
      #$20AC, #$0081, #$201A, #$0192, #$201E, #$2026, #$2020, #$2021,
      #$0088, #$2030, #$008A, #$2039, #$008C, #$008D, #$008E, #$008F,
      #$0090, #$2018, #$2019, #$201C, #$201D, #$2022, #$2013, #$2014,
      #$0098, #$2122, #$009A, #$203A, #$009C, #$009D, #$009E, #$009F,
      #$00A0, #$0385, #$0386, #$00A3, #$00A4, #$00A5, #$00A6, #$00A7,
      #$00A8, #$00A9, #$F8F9, #$00AB, #$00AC, #$00AD, #$00AE, #$2015,
      #$00B0, #$00B1, #$00B2, #$00B3, #$0384, #$00B5, #$00B6, #$00B7,
      #$0388, #$0389, #$038A, #$00BB, #$038C, #$00BD, #$038E, #$038F,
      #$0390, #$0391, #$0392, #$0393, #$0394, #$0395, #$0396, #$0397, 
      #$0398, #$0399, #$039A, #$039B, #$039C, #$039D, #$039E, #$039F, 
      #$03A0, #$03A1, #$F8FA, #$03A3, #$03A4, #$03A5, #$03A6, #$03A7, 
      #$03A8, #$03A9, #$03AA, #$03AB, #$03AC, #$03AD, #$03AE, #$03AF,
      #$03B0, #$03B1, #$03B2, #$03B3, #$03B4, #$03B5, #$03B6, #$03B7,
      #$03B8, #$03B9, #$03BA, #$03BB, #$03BC, #$03BD, #$03BE, #$03BF,
      #$03C0, #$03C1, #$03C2, #$03C3, #$03C4, #$03C5, #$03C6, #$03C7,
      #$03C8, #$03C9, #$03CA, #$03CB, #$03CC, #$03CD, #$03CE, #$F8FB);

function TWindows1253Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  if Ord(P) < $80 then
    Result := WideChar(P)
  else
    Result := Windows1253Map[Ord(P)];
end;

function TWindows1253Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromHighMap(Ch, Windows1253Map, 'Windows-1253');
end;



{                                                                              }
{ Windows-1254                                                                 }
{                                                                              }
const
  Windows1254Map : AnsiCharHighMap = (
      #$20AC, #$0081, #$201A, #$0192, #$201E, #$2026, #$2020, #$2021,
      #$02C6, #$2030, #$0160, #$2039, #$0152, #$008D, #$008E, #$008F,
      #$0090, #$2018, #$2019, #$201C, #$201D, #$2022, #$2013, #$2014,
      #$02DC, #$2122, #$0161, #$203A, #$0153, #$009D, #$009E, #$0178,
      #$00A0, #$00A1, #$00A2, #$00A3, #$00A4, #$00A5, #$00A6, #$00A7,
      #$00A8, #$00A9, #$00AA, #$00AB, #$00AC, #$00AD, #$00AE, #$00AF,
      #$00B0, #$00B1, #$00B2, #$00B3, #$00B4, #$00B5, #$00B6, #$00B7,
      #$00B8, #$00B9, #$00BA, #$00BB, #$00BC, #$00BD, #$00BE, #$00BF,
      #$00C0, #$00C1, #$00C2, #$00C3, #$00C4, #$00C5, #$00C6, #$00C7, 
      #$00C8, #$00C9, #$00CA, #$00CB, #$00CC, #$00CD, #$00CE, #$00CF, 
      #$011E, #$00D1, #$00D2, #$00D3, #$00D4, #$00D5, #$00D6, #$00D7,
      #$00D8, #$00D9, #$00DA, #$00DB, #$00DC, #$0130, #$015E, #$00DF, 
      #$00E0, #$00E1, #$00E2, #$00E3, #$00E4, #$00E5, #$00E6, #$00E7, 
      #$00E8, #$00E9, #$00EA, #$00EB, #$00EC, #$00ED, #$00EE, #$00EF,
      #$011F, #$00F1, #$00F2, #$00F3, #$00F4, #$00F5, #$00F6, #$00F7,
      #$00F8, #$00F9, #$00FA, #$00FB, #$00FC, #$0131, #$015F, #$00FF);

function TWindows1254Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  if Ord(P) < $80 then
    Result := WideChar(P)
  else
    Result := Windows1254Map[Ord(P)];
end;

function TWindows1254Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromHighMap(Ch, Windows1254Map, 'Windows-1254');
end;



{                                                                              }
{ Windows-1255                                                                 }
{                                                                              }
const
  Windows1255Map : AnsiCharHighMap = (
      #$20AC, #$0081, #$201A, #$0192, #$201E, #$2026, #$2020, #$2021,
      #$02C6, #$2030, #$008A, #$2039, #$008C, #$008D, #$008E, #$008F,
      #$0090, #$2018, #$2019, #$201C, #$201D, #$2022, #$2013, #$2014,
      #$02DC, #$2122, #$009A, #$203A, #$009C, #$009D, #$009E, #$009F,
      #$00A0, #$00A1, #$00A2, #$00A3, #$20AA, #$00A5, #$00A6, #$00A7,
      #$00A8, #$00A9, #$00D7, #$00AB, #$00AC, #$00AD, #$00AE, #$00AF,
      #$00B0, #$00B1, #$00B2, #$00B3, #$00B4, #$00B5, #$00B6, #$00B7,
      #$00B8, #$00B9, #$00F7, #$00BB, #$00BC, #$00BD, #$00BE, #$00BF,
      #$05B0, #$05B1, #$05B2, #$05B3, #$05B4, #$05B5, #$05B6, #$05B7,
      #$05B8, #$05B9, #$05BA, #$05BB, #$05BC, #$05BD, #$05BE, #$05BF, 
      #$05C0, #$05C1, #$05C2, #$05C3, #$05F0, #$05F1, #$05F2, #$05F3,
      #$05F4, #$F88D, #$F88E, #$F88F, #$F890, #$F891, #$F892, #$F893,
      #$05D0, #$05D1, #$05D2, #$05D3, #$05D4, #$05D5, #$05D6, #$05D7, 
      #$05D8, #$05D9, #$05DA, #$05DB, #$05DC, #$05DD, #$05DE, #$05DF,
      #$05E0, #$05E1, #$05E2, #$05E3, #$05E4, #$05E5, #$05E6, #$05E7, 
      #$05E8, #$05E9, #$05EA, #$F894, #$F895, #$200E, #$200F, #$F896);

function TWindows1255Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  if Ord(P) < $80 then
    Result := WideChar(P)
  else
    Result := Windows1255Map[Ord(P)];
end;

function TWindows1255Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromHighMap(Ch, Windows1255Map, 'Windows-1255');
end;



{                                                                              }
{ Windows-1256                                                                 }
{                                                                              }
const
  Windows1256Map : AnsiCharHighMap = (
      #$20AC, #$067E, #$201A, #$0192, #$201E, #$2026, #$2020, #$2021,
      #$02C6, #$2030, #$0679, #$2039, #$0152, #$0686, #$0698, #$0688,
      #$06AF, #$2018, #$2019, #$201C, #$201D, #$2022, #$2013, #$2014,
      #$06A9, #$2122, #$0691, #$203A, #$0153, #$200C, #$200D, #$06BA,
      #$00A0, #$060C, #$00A2, #$00A3, #$00A4, #$00A5, #$00A6, #$00A7,
      #$00A8, #$00A9, #$06BE, #$00AB, #$00AC, #$00AD, #$00AE, #$00AF,
      #$00B0, #$00B1, #$00B2, #$00B3, #$00B4, #$00B5, #$00B6, #$00B7,
      #$00B8, #$00B9, #$061B, #$00BB, #$00BC, #$00BD, #$00BE, #$061F,
      #$06C1, #$0621, #$0622, #$0623, #$0624, #$0625, #$0626, #$0627, 
      #$0628, #$0629, #$062A, #$062B, #$062C, #$062D, #$062E, #$062F,
      #$0630, #$0631, #$0632, #$0633, #$0634, #$0635, #$0636, #$00D7, 
      #$0637, #$0638, #$0639, #$063A, #$0640, #$0641, #$0642, #$0643, 
      #$00E0, #$0644, #$00E2, #$0645, #$0646, #$0647, #$0648, #$00E7, 
      #$00E8, #$00E9, #$00EA, #$00EB, #$0649, #$064A, #$00EE, #$00EF,
      #$064B, #$064C, #$064D, #$064E, #$00F4, #$064F, #$0650, #$00F7,
      #$0651, #$00F9, #$0652, #$00FB, #$00FC, #$200E, #$200F, #$06D2);

function TWindows1256Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  if Ord(P) < $80 then
    Result := WideChar(P)
  else
    Result := Windows1256Map[Ord(P)];
end;

function TWindows1256Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromHighMap(Ch, Windows1256Map, 'Windows-1256');
end;



{                                                                              }
{ Windows-1257                                                                 }
{                                                                              }
const
  Windows1257Map : AnsiCharHighMap = (
      #$20AC, #$0081, #$201A, #$0083, #$201E, #$2026, #$2020, #$2021,
      #$0088, #$2030, #$008A, #$2039, #$008C, #$00A8, #$02C7, #$00B8,
      #$0090, #$2018, #$2019, #$201C, #$201D, #$2022, #$2013, #$2014,
      #$0098, #$2122, #$009A, #$203A, #$009C, #$00AF, #$02DB, #$009F,
      #$00A0, #$F8FC, #$00A2, #$00A3, #$00A4, #$F8FD, #$00A6, #$00A7,
      #$00D8, #$00A9, #$0156, #$00AB, #$00AC, #$00AD, #$00AE, #$00C6, 
      #$00B0, #$00B1, #$00B2, #$00B3, #$00B4, #$00B5, #$00B6, #$00B7, 
      #$00F8, #$00B9, #$0157, #$00BB, #$00BC, #$00BD, #$00BE, #$00E6, 
      #$0104, #$012E, #$0100, #$0106, #$00C4, #$00C5, #$0118, #$0112, 
      #$010C, #$00C9, #$0179, #$0116, #$0122, #$0136, #$012A, #$013B,
      #$0160, #$0143, #$0145, #$00D3, #$014C, #$00D5, #$00D6, #$00D7, 
      #$0172, #$0141, #$015A, #$016A, #$00DC, #$017B, #$017D, #$00DF, 
      #$0105, #$012F, #$0101, #$0107, #$00E4, #$00E5, #$0119, #$0113, 
      #$010D, #$00E9, #$017A, #$0117, #$0123, #$0137, #$012B, #$013C, 
      #$0161, #$0144, #$0146, #$00F3, #$014D, #$00F5, #$00F6, #$00F7,
      #$0173, #$0142, #$015B, #$016B, #$00FC, #$017C, #$017E, #$02D9);

function TWindows1257Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  if Ord(P) < $80 then
    Result := WideChar(P)
  else
    Result := Windows1257Map[Ord(P)];
end;

function TWindows1257Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromHighMap(Ch, Windows1257Map, 'Windows-1257');
end;



{                                                                              }
{ Windows-1258                                                                 }
{                                                                              }
const
  Windows1258Map : AnsiCharHighMap = (
      #$20AC, #$0081, #$201A, #$0192, #$201E, #$2026, #$2020, #$2021,
      #$02C6, #$2030, #$008A, #$2039, #$0152, #$008D, #$008E, #$008F,
      #$0090, #$2018, #$2019, #$201C, #$201D, #$2022, #$2013, #$2014,
      #$02DC, #$2122, #$009A, #$203A, #$0153, #$009D, #$009E, #$0178,
      #$00A0, #$00A1, #$00A2, #$00A3, #$00A4, #$00A5, #$00A6, #$00A7,
      #$00A8, #$00A9, #$00AA, #$00AB, #$00AC, #$00AD, #$00AE, #$00AF,
      #$00B0, #$00B1, #$00B2, #$00B3, #$00B4, #$00B5, #$00B6, #$00B7,
      #$00B8, #$00B9, #$00BA, #$00BB, #$00BC, #$00BD, #$00BE, #$00BF,
      #$00C0, #$00C1, #$00C2, #$0102, #$00C4, #$00C5, #$00C6, #$00C7,
      #$00C8, #$00C9, #$00CA, #$00CB, #$0300, #$00CD, #$00CE, #$00CF,
      #$0110, #$00D1, #$0309, #$00D3, #$00D4, #$01A0, #$00D6, #$00D7,
      #$00D8, #$00D9, #$00DA, #$00DB, #$00DC, #$01AF, #$0303, #$00DF,
      #$00E0, #$00E1, #$00E2, #$0103, #$00E4, #$00E5, #$00E6, #$00E7,
      #$00E8, #$00E9, #$00EA, #$00EB, #$0301, #$00ED, #$00EE, #$00EF,
      #$0111, #$00F1, #$0323, #$00F3, #$00F4, #$01A1, #$00F6, #$00F7,
      #$00F8, #$00F9, #$00FA, #$00FB, #$00FC, #$01B0, #$20AB, #$00FF);

function TWindows1258Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  if Ord(P) < $80 then
    Result := WideChar(P)
  else
    Result := Windows1258Map[Ord(P)];
end;

function TWindows1258Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromHighMap(Ch, Windows1258Map, 'Windows-1258');
end;



{                                                                              }
{ Mac Latin-2                                                                  }
{                                                                              }
const
  MacLatin2Aliases = 3;
  MacLatin2Alias : array[0..MacLatin2Aliases - 1] of String = (
      'MacLatin2', 'Mac', 'Macintosh');

class function TMacLatin2Codec.GetAliasCount: Integer;
begin
  Result := MacLatin2Aliases;
end;

class function TMacLatin2Codec.GetAlias(const Idx: Integer): String;
begin
  if Idx < 0 then
    raise EUnicodeCodecException.Create(SAliasIndexOutOfRange);
  if Idx >= GetAliasCount then
    raise EUnicodeCodecException.Create(SAliasIndexOutOfRange);
  Result := MacLatin2Alias[Idx];
end;

const
  MacLatin2Map : AnsiCharHighMap = (
    #$00C4, #$0100, #$0101, #$00C9, #$0104, #$00D6, #$00DC, #$00E1,
    #$0105, #$010C, #$00E4, #$010D, #$0106, #$0107, #$00E9, #$0179,
    #$017A, #$010E, #$00ED, #$010F, #$0112, #$0113, #$0116, #$00F3,
    #$0117, #$00F4, #$00F6, #$00F5, #$00FA, #$011A, #$011B, #$00FC,
    #$2020, #$00B0, #$0118, #$00A3, #$00A7, #$2022, #$00B6, #$00DF,
    #$00AE, #$00A9, #$2122, #$0119, #$00A8, #$2260, #$0123, #$012E,
    #$012F, #$012A, #$2264, #$2265, #$012B, #$0136, #$2202, #$2211,
    #$0142, #$013B, #$013C, #$013D, #$013E, #$0139, #$013A, #$0145,
    #$0146, #$0143, #$00AC, #$221A, #$0144, #$0147, #$2206, #$00AB,
    #$00BB, #$2026, #$00A0, #$0148, #$0150, #$00D5, #$0151, #$014C,
    #$2013, #$2014, #$201C, #$201D, #$2018, #$2019, #$00F7, #$25CA,
    #$014D, #$0154, #$0155, #$0158, #$2039, #$203A, #$0159, #$0156,
    #$0157, #$0160, #$201A, #$201E, #$0161, #$015A, #$015B, #$00C1,
    #$0164, #$0165, #$00CD, #$017D, #$017E, #$016A, #$00D3, #$00D4,
    #$016B, #$016E, #$00DA, #$016F, #$0170, #$0171, #$0172, #$0173,
    #$00DD, #$00FD, #$0137, #$017B, #$0141, #$017C, #$0122, #$02C7);

function TMacLatin2Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  if Ord(P) < $80 then
    Result := WideChar(P)
  else
    Result := MacLatin2Map[Ord(P)];
end;

function TMacLatin2Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromHighMap(Ch, MacLatin2Map, 'MacLatin2');
end;



{                                                                              }
{ Mac Roman                                                                    }
{                                                                              }
const
  MacRomanAliases = 1;
  MacRomanAlias : array[0..MacRomanAliases - 1] of String = (
      'MacRoman');

class function TMacRomanCodec.GetAliasCount: Integer;
begin
  Result := MacRomanAliases;
end;

class function TMacRomanCodec.GetAlias(const Idx: Integer): String;
begin
  if Idx < 0 then
    raise EUnicodeCodecException.Create(SAliasIndexOutOfRange);
  if Idx >= GetAliasCount then
    raise EUnicodeCodecException.Create(SAliasIndexOutOfRange);
  Result := MacRomanAlias[Idx];
end;

const
  MacRomanMap : AnsiCharHighMap = (
    #$00C4, #$00C5, #$00C7, #$00C9, #$00D1, #$00D6, #$00DC, #$00E1,
    #$00E0, #$00E2, #$00E4, #$00E3, #$00E5, #$00E7, #$00E9, #$00E8,
    #$00EA, #$00EB, #$00ED, #$00EC, #$00EE, #$00EF, #$00F1, #$00F3,
    #$00F2, #$00F4, #$00F6, #$00F5, #$00FA, #$00F9, #$00FB, #$00FC,
    #$2020, #$00B0, #$00A2, #$00A3, #$00A7, #$2022, #$00B6, #$00DF,
    #$00AE, #$00A9, #$2122, #$00B4, #$00A8, #$2260, #$00C6, #$00D8,
    #$221E, #$00B1, #$2264, #$2265, #$00A5, #$00B5, #$2202, #$2211,
    #$220F, #$03C0, #$222B, #$00AA, #$00BA, #$2126, #$00E6, #$00F8,
    #$00BF, #$00A1, #$00AC, #$221A, #$0192, #$2248, #$2206, #$00AB,
    #$00BB, #$2026, #$00A0, #$00C0, #$00C3, #$00D5, #$0152, #$0153,
    #$2013, #$2014, #$201C, #$201D, #$2018, #$2019, #$00F7, #$25CA,
    #$00FF, #$0178, #$2044, #$00A4, #$2039, #$203A, #$FB01, #$FB02,
    #$2021, #$00B7, #$201A, #$201E, #$2030, #$00C2, #$00CA, #$00C1,
    #$00CB, #$00C8, #$00CD, #$00CE, #$00CF, #$00CC, #$00D3, #$00D4,
    #$FFFF, #$00D2, #$00DA, #$00DB, #$00D9, #$0131, #$02C6, #$02DC,
    #$00AF, #$02D8, #$02D9, #$02DA, #$00B8, #$02DD, #$02DB, #$02C7);

function TMacRomanCodec.DecodeChar(const P: ByteChar): WideChar;
begin
  if Ord(P) < $80 then
    Result := WideChar(P)
  else
    Result := MacRomanMap[Ord(P)];
end;

function TMacRomanCodec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromHighMap(Ch, MacRomanMap, 'MacRoman');
end;



{                                                                              }
{ Mac Cyrillic                                                                 }
{                                                                              }
const
  MacCyrillicAliases = 1;
  MacCyrillicAlias : array[0..MacCyrillicAliases - 1] of String = (
      'MacCyrillic');

class function TMacCyrillicCodec.GetAliasCount: Integer;
begin
  Result := MacCyrillicAliases;
end;

class function TMacCyrillicCodec.GetAlias(const Idx: Integer): String;
begin
  if Idx < 0 then
    raise EUnicodeCodecException.Create(SAliasIndexOutOfRange);
  if Idx >= GetAliasCount then
    raise EUnicodeCodecException.Create(SAliasIndexOutOfRange);
  Result := MacCyrillicAlias[Idx];
end;

const
  MacCyrillicMap : AnsiCharHighMap = (
    #$0410, #$0411, #$0412, #$0413, #$0414, #$0415, #$0416, #$0417,
    #$0418, #$0419, #$041A, #$041B, #$041C, #$041D, #$041E, #$041F,
    #$0420, #$0421, #$0422, #$0423, #$0424, #$0425, #$0426, #$0427,
    #$0428, #$0429, #$042A, #$042B, #$042C, #$042D, #$042E, #$042F,
    #$2020, #$00B0, #$00A2, #$00A3, #$00A7, #$2022, #$00B6, #$0406,
    #$00AE, #$00A9, #$2122, #$0402, #$0452, #$2260, #$0403, #$0453,
    #$221E, #$00B1, #$2264, #$2265, #$0456, #$00B5, #$2202, #$0408,
    #$0404, #$0454, #$0407, #$0457, #$0409, #$0459, #$040A, #$045A,
    #$0458, #$0405, #$00AC, #$221A, #$0192, #$2248, #$2206, #$00AB,
    #$00BB, #$2026, #$00A0, #$040B, #$045B, #$040C, #$045C, #$0455,
    #$2013, #$2014, #$201C, #$201D, #$2018, #$2019, #$00F7, #$201E,
    #$040E, #$045E, #$040F, #$045F, #$2116, #$0401, #$0451, #$044F,
    #$0430, #$0431, #$0432, #$0433, #$0434, #$0435, #$0436, #$0437,
    #$0438, #$0439, #$043A, #$043B, #$043C, #$043D, #$043E, #$043F,
    #$0440, #$0441, #$0442, #$0443, #$0444, #$0445, #$0446, #$0447,
    #$0448, #$0449, #$044A, #$044B, #$044C, #$044D, #$044E, #$00A4);

function TMacCyrillicCodec.DecodeChar(const P: ByteChar): WideChar;
begin
  if Ord(P) < $80 then
    Result := WideChar(P)
  else
    Result := MacCyrillicMap[Ord(P)];
end;

function TMacCyrillicCodec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromHighMap(Ch, MacCyrillicMap, 'MacCyrillic');
end;



{                                                                              }
{ Mac Greek                                                                    }
{                                                                              }
const
  MacGreekMap : AnsiCharHighMap = (
    #$00C4, #$00B9, #$00B2, #$00C9, #$00B3, #$00D6, #$00DC, #$0385,
    #$00E0, #$00E2, #$00E4, #$0384, #$00A8, #$00E7, #$00E9, #$00E8,
    #$00EA, #$00EB, #$00A3, #$2122, #$00EE, #$00EF, #$2022, #$00BD,
    #$2030, #$00F4, #$00F6, #$00A6, #$00AD, #$00F9, #$00FB, #$00FC,
    #$2020, #$0393, #$0394, #$0398, #$039B, #$039E, #$03A0, #$00DF,
    #$00AE, #$00A9, #$03A3, #$03AA, #$00A7, #$2260, #$00B0, #$0387,
    #$0391, #$00B1, #$2264, #$2265, #$00A5, #$0392, #$0395, #$0396,
    #$0397, #$0399, #$039A, #$039C, #$03A6, #$03AB, #$03A8, #$03A9,
    #$03AC, #$039D, #$00AC, #$039F, #$03A1, #$2248, #$03A4, #$00AB,
    #$00BB, #$2026, #$00A0, #$03A5, #$03A7, #$0386, #$0388, #$0153,
    #$2013, #$2015, #$201C, #$201D, #$2018, #$2019, #$00F7, #$0389,
    #$038A, #$038C, #$038E, #$03AD, #$03AE, #$03AF, #$03CC, #$038F,
    #$03CD, #$03B1, #$03B2, #$03C8, #$03B4, #$03B5, #$03C6, #$03B3,
    #$03B7, #$03B9, #$03BE, #$03BA, #$03BB, #$03BC, #$03BD, #$03BF,
    #$03C0, #$03CE, #$03C1, #$03C3, #$03C4, #$03B8, #$03C9, #$03C2,
    #$03C7, #$03C5, #$03B6, #$03CA, #$03CB, #$0390, #$03B0, #$FFFF);

function TMacGreekCodec.DecodeChar(const P: ByteChar): WideChar;
begin
  if Ord(P) < $80 then
    Result := WideChar(P)
  else
    Result := MacGreekMap[Ord(P)];
end;

function TMacGreekCodec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromHighMap(Ch, MacGreekMap, 'MacGreek');
end;



{                                                                              }
{ Mac Icelandic                                                                }
{                                                                              }
const
  MacIcelandicMap : AnsiCharHighMap = (
    #$00C4, #$00C5, #$00C7, #$00C9, #$00D1, #$00D6, #$00DC, #$00E1,
    #$00E0, #$00E2, #$00E4, #$00E3, #$00E5, #$00E7, #$00E9, #$00E8,
    #$00EA, #$00EB, #$00ED, #$00EC, #$00EE, #$00EF, #$00F1, #$00F3,
    #$00F2, #$00F4, #$00F6, #$00F5, #$00FA, #$00F9, #$00FB, #$00FC,
    #$00DD, #$00B0, #$00A2, #$00A3, #$00A7, #$2022, #$00B6, #$00DF,
    #$00AE, #$00A9, #$2122, #$00B4, #$00A8, #$2260, #$00C6, #$00D8,
    #$221E, #$00B1, #$2264, #$2265, #$00A5, #$00B5, #$2202, #$2211,
    #$220F, #$03C0, #$222B, #$00AA, #$00BA, #$2126, #$00E6, #$00F8,
    #$00BF, #$00A1, #$00AC, #$221A, #$0192, #$2248, #$2206, #$00AB,
    #$00BB, #$2026, #$00A0, #$00C0, #$00C3, #$00D5, #$0152, #$0153,
    #$2013, #$2014, #$201C, #$201D, #$2018, #$2019, #$00F7, #$25CA,
    #$00FF, #$0178, #$2044, #$00A4, #$00D0, #$00F0, #$00DE, #$00FE,
    #$00FD, #$00B7, #$201A, #$201E, #$2030, #$00C2, #$00CA, #$00C1,
    #$00CB, #$00C8, #$00CD, #$00CE, #$00CF, #$00CC, #$00D3, #$00D4,
    #$FFFF, #$00D2, #$00DA, #$00DB, #$00D9, #$0131, #$02C6, #$02DC,
    #$00AF, #$02D8, #$02D9, #$02DA, #$00B8, #$02DD, #$02DB, #$02C7);

function TMacIcelandicCodec.DecodeChar(const P: ByteChar): WideChar;
begin
  if Ord(P) < $80 then
    Result := WideChar(P)
  else
    Result := MacIcelandicMap[Ord(P)];
end;

function TMacIcelandicCodec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromHighMap(Ch, MacIcelandicMap, 'MacIcelandic');
end;



{                                                                              }
{ Mac Turkish                                                                  }
{                                                                              }
const
  MacTurkishMap : AnsiCharHighMap = (
    #$00C4, #$00C5, #$00C7, #$00C9, #$00D1, #$00D6, #$00DC, #$00E1,
    #$00E0, #$00E2, #$00E4, #$00E3, #$00E5, #$00E7, #$00E9, #$00E8,
    #$00EA, #$00EB, #$00ED, #$00EC, #$00EE, #$00EF, #$00F1, #$00F3,
    #$00F2, #$00F4, #$00F6, #$00F5, #$00FA, #$00F9, #$00FB, #$00FC,
    #$2020, #$00B0, #$00A2, #$00A3, #$00A7, #$2022, #$00B6, #$00DF,
    #$00AE, #$00A9, #$2122, #$00B4, #$00A8, #$2260, #$00C6, #$00D8,
    #$221E, #$00B1, #$2264, #$2265, #$00A5, #$00B5, #$2202, #$2211,
    #$220F, #$03C0, #$222B, #$00AA, #$00BA, #$2126, #$00E6, #$00F8,
    #$00BF, #$00A1, #$00AC, #$221A, #$0192, #$2248, #$2206, #$00AB,
    #$00BB, #$2026, #$00A0, #$00C0, #$00C3, #$00D5, #$0152, #$0153,
    #$2013, #$2014, #$201C, #$201D, #$2018, #$2019, #$00F7, #$25CA,
    #$00FF, #$0178, #$011E, #$011F, #$0130, #$0131, #$015E, #$015F,
    #$2021, #$00B7, #$201A, #$201E, #$2030, #$00C2, #$00CA, #$00C1,
    #$00CB, #$00C8, #$00CD, #$00CE, #$00CF, #$00CC, #$00D3, #$00D4,
    #$FFFF, #$00D2, #$00DA, #$00DB, #$00D9, #$FFFF, #$02C6, #$02DC,
    #$00AF, #$02D8, #$02D9, #$02DA, #$00B8, #$02DD, #$02DB, #$02C7);

function TMacTurkishCodec.DecodeChar(const P: ByteChar): WideChar;
begin
  if Ord(P) < $80 then
    Result := WideChar(P)
  else
    Result := MacTurkishMap[Ord(P)];
end;

function TMacTurkishCodec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromHighMap(Ch, MacTurkishMap, 'MacTurkish');
end;



{                                                                              }
{ US-ASCII                                                                     }
{                                                                              }
const
  USASCIIAliases = 17;
  USASCIIAlias : array[0..USASCIIAliases - 1] of String = (
      'ASCII', 'US-ASCII', 'us',
      'ANSI_X3.4-1968', 'ANSI_X3.4-1986', 'iso-ir-6',
      'ISO_646.irv:1991', 'ISO_646.irv', 'ISO_646',
      'ISO-646', 'ISO646', 'ISO646-US',
      'IBM367', 'cp367', 'csASCII', 'IBM891', 'IBM903');

class function TUSASCIICodec.GetAliasCount: Integer;
begin
  Result := USASCIIAliases;
end;

class function TUSASCIICodec.GetAlias(const Idx: Integer): String;
begin
  if Idx < 0 then
    raise EUnicodeCodecException.Create(SAliasIndexOutOfRange);
  if Idx >= GetAliasCount then
    raise EUnicodeCodecException.Create(SAliasIndexOutOfRange);
  Result := USASCIIAlias[Idx];
end;

function TUSASCIICodec.DecodeChar(const P: ByteChar): WideChar;
begin
  if Ord(P) < $80 then
    Result := WideChar(P)
  else
    raise EConvertError.CreateFmt(SInvalidCodePoint, [Ord(P), 'US-ASCII']);
end;

function TUSASCIICodec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  if Ord(Ch) < $80 then
    Result := AnsiChar(Ch)
  else
    raise EConvertError.CreateFmt(SCannotConvert, [Ord(Ch), 'US-ASCII']);
end;



{                                                                              }
{ EBCDIC-US                                                                    }
{                                                                              }
const
  EBCDIC_USAliases = 2;
  EBCDIC_USAlias : array[0..EBCDIC_USAliases - 1] of String = (
      'ebcdic-us', 'ebcdic');

class function TEBCDIC_USCodec.GetAliasCount: Integer;
begin
  Result := EBCDIC_USAliases;
end;

class function TEBCDIC_USCodec.GetAlias(const Idx: Integer): String;
begin
  if Idx < 0 then
    raise EUnicodeCodecException.Create(SAliasIndexOutOfRange);
  if Idx >= GetAliasCount then
    raise EUnicodeCodecException.Create(SAliasIndexOutOfRange);
  Result := EBCDIC_USAlias[Idx];
end;

function TEBCDIC_USCodec.DecodeChar(const P: ByteChar): WideChar;
begin
  case Ord(P) of
    $40      : Result := #$0020;                         // SPACE
    $4A      : Result := #$00A2;                         // CENT SIGN
    $4B      : Result := #$002E;                         // FULL STOP
    $4C      : Result := #$003C;                         // LESS-THAN SIGN
    $4D      : Result := #$0028;                         // LEFT PARENTHESIS
    $4E      : Result := #$002B;                         // PLUS SIGN
    $4F      : Result := #$007C;                         // VERTICAL LINE
    $50      : Result := #$0026;                         // AMPERSAND
    $5A      : Result := #$0021;                         // EXCLAMATION MARK
    $5B      : Result := #$0024;                         // DOLLAR SIGN
    $5C      : Result := #$002A;                         // ASTERISK
    $5D      : Result := #$0029;                         // RIGHT PARENTHESIS
    $5E      : Result := #$003B;                         // SEMICOLON
    $5F      : Result := #$00AC;                         // NOT SIGN
    $60      : Result := #$002D;                         // HYPHEN-MINUS
    $61      : Result := #$002F;                         // SOLIDUS
    $6A      : Result := #$00A6;                         // BROKEN BAR
    $6B      : Result := #$002C;                         // COMMA
    $6C      : Result := #$0025;                         // PERCENT SIGN
    $6D      : Result := #$005F;                         // LOW LINE
    $6E      : Result := #$003E;                         // GREATER-THAN SIGN
    $6F      : Result := #$003F;                         // QUESTION MARK
    $79      : Result := #$0060;                         // GRAVE ACCENT
    $7A      : Result := #$003A;                         // COLON
    $7B      : Result := #$0023;                         // NUMBER SIGN
    $7C      : Result := #$0040;                         // COMMERCIAL AT
    $7D      : Result := #$0027;                         // APOSTROPHE
    $7E      : Result := #$003D;                         // EQUALS SIGN
    $7F      : Result := #$0022;                         // QUOTATION MARK
    $81..$89 : Result := WideChar(Ord(P) - $81 + $0061); // LATIN SMALL LETTER A..I
    $91..$99 : Result := WideChar(Ord(P) - $91 + $006A); // LATIN SMALL LETTER J..R
    $A1      : Result := #$007E;                         // TILDE
    $A2..$A9 : Result := WideChar(Ord(P) - $A2 + $0073); // LATIN SMALL LETTER S..Z
    $C0      : Result := #$007B;                         // LEFT CURLY BRACKET
    $C1..$C9 : Result := WideChar(Ord(P) - $C1 + $0041); // LATIN CAPITAL LETTER A..I
    $D0      : Result := #$007D;                         // RIGHT CURLY BRACKET
    $D1..$D9 : Result := WideChar(Ord(P) - $D1 + $004A); // LATIN CAPITAL LETTER J..R
    $E0      : Result := #$005C;                         // REVERSE SOLIDUS
    $E2..$E9 : Result := WideChar(Ord(P) - $E2 + $0053); // LATIN CAPITAL LETTER S
    $F0..$F9 : Result := WideChar(Ord(P) - $F0 + $0030); // DIGIT ZERO
  else
    Result := #$FFFF;
  end;
end;

function TEBCDIC_USCodec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  case Ord(Ch) of
    $0020        : Result := AnsiChar($40);                            // SPACE
    $0021        : Result := AnsiChar($5A);                            // EXCLAMATION MARK
    $0022        : Result := AnsiChar($7F);                            // QUOTATION MARK
    $0023        : Result := AnsiChar($7B);                            // NUMBER SIGN
    $0024        : Result := AnsiChar($5B);                            // DOLLAR SIGN
    $0025        : Result := AnsiChar($6C);                            // PERCENT SIGN
    $0026        : Result := AnsiChar($50);                            // AMPERSAND
    $0027        : Result := AnsiChar($7D);                            // APOSTROPHE
    $0028        : Result := AnsiChar($4D);                            // LEFT PARENTHESIS
    $0029        : Result := AnsiChar($5D);                            // RIGHT PARENTHESIS
    $002A        : Result := AnsiChar($5C);                            // ASTERISK
    $002B        : Result := AnsiChar($4E);                            // PLUS SIGN
    $002C        : Result := AnsiChar($6B);                            // COMMA
    $002D        : Result := AnsiChar($60);                            // HYPHEN-MINUS
    $002E        : Result := AnsiChar($4B);                            // FULL STOP
    $002F        : Result := AnsiChar($61);                            // SOLIDUS
    $0030..$0039 : Result := AnsiChar(Ord(Ch) - $0030 + $F0); // DIGIT ZERO-NINE
    $003A        : Result := AnsiChar($7A);                            // COLON
    $003B        : Result := AnsiChar($5E);                            // SEMICOLON
    $003C        : Result := AnsiChar($4C);                            // LESS-THAN SIGN
    $003D        : Result := AnsiChar($7E);                            // EQUALS SIGN
    $003E        : Result := AnsiChar($6E);                            // GREATER-THAN SIGN
    $003F        : Result := AnsiChar($6F);                            // QUESTION MARK
    $0040        : Result := AnsiChar($7C);                            // COMMERCIAL AT
    $0041..$0049 : Result := AnsiChar(Ord(Ch) - $0041 + $C1); // LATIN CAPITAL LETTER A..I
    $004A..$0052 : Result := AnsiChar(Ord(Ch) - $004A + $D1); // LATIN CAPITAL LETTER J..R
    $0053..$005A : Result := AnsiChar(Ord(Ch) - $0053 + $E2); // LATIN CAPITAL LETTER S..Z
    $005C        : Result := AnsiChar($E0);                            // REVERSE SOLIDUS
    $005F        : Result := AnsiChar($6D);                            // LOW LINE
    $0060        : Result := AnsiChar($79);                            // GRAVE ACCENT
    $0061..$0069 : Result := AnsiChar(Ord(Ch) - $0061 + $81); // LATIN SMALL LETTER A..I
    $006A..$0072 : Result := AnsiChar(Ord(Ch) - $006A + $91); // LATIN SMALL LETTER J..R
    $0073..$007A : Result := AnsiChar(Ord(Ch) - $0073 + $A2); // LATIN SMALL LETTER S..Z
    $007B        : Result := AnsiChar($C0);                            // LEFT CURLY BRACKET
    $007C        : Result := AnsiChar($4F);                            // VERTICAL LINE
    $007D        : Result := AnsiChar($D0);                            // RIGHT CURLY BRACKET
    $007E        : Result := AnsiChar($A1);                            // TILDE
    $00A2        : Result := AnsiChar($4A);                            // CENT SIGN
    $00A6        : Result := AnsiChar($6A);                            // BROKEN BAR
    $00AC        : Result := AnsiChar($5F);                            // NOT SIGN
  else
    raise EConvertError.CreateFmt(SCannotConvert, [Ord(Ch), 'EBCDIC-US']);
  end;
end;



{                                                                              }
{ KOI8-R                                                                       }
{                                                                              }
const
  KOI8_RAliases = 1;
  KOI8_RAlias : array[0..KOI8_RAliases - 1] of String = (
      'KOI8-R');

class function TKOI8_RCodec.GetAliasCount: Integer;
begin
  Result := KOI8_RAliases;
end;

class function TKOI8_RCodec.GetAlias(const Idx: Integer): String;
begin
  if Idx < 0 then
    raise EUnicodeCodecException.Create(SAliasIndexOutOfRange);
  if Idx >= GetAliasCount then
    raise EUnicodeCodecException.Create(SAliasIndexOutOfRange);
  Result := KOI8_RAlias[Idx];
end;

const
  KOI8_RMap : AnsiCharHighMap = (
    #$2500, #$2502, #$250C, #$2510, #$2514, #$2518, #$251C, #$2524,
    #$252C, #$2534, #$253C, #$2580, #$2584, #$2588, #$258C, #$2590,
    #$2591, #$2592, #$2593, #$2320, #$25A0, #$2219, #$221A, #$2248,
    #$2264, #$2265, #$00A0, #$2321, #$00B0, #$00B2, #$00B7, #$00F7,
    #$2550, #$2551, #$2552, #$0451, #$2553, #$2554, #$2555, #$2556,
    #$2557, #$2558, #$2559, #$255A, #$255B, #$255C, #$255D, #$255E,
    #$255F, #$2560, #$2561, #$0401, #$2562, #$2563, #$2564, #$2565,
    #$2566, #$2567, #$2568, #$2569, #$256A, #$256B, #$256C, #$00A9,
    #$044E, #$0430, #$0431, #$0446, #$0434, #$0435, #$0444, #$0433,
    #$0445, #$0438, #$0439, #$043A, #$043B, #$043C, #$043D, #$043E,
    #$043F, #$044F, #$0440, #$0441, #$0442, #$0443, #$0436, #$0432,
    #$044C, #$044B, #$0437, #$0448, #$044D, #$0449, #$0447, #$044A,
    #$042E, #$0410, #$0411, #$0426, #$0414, #$0415, #$0424, #$0413,
    #$0425, #$0418, #$0419, #$041A, #$041B, #$041C, #$041D, #$041E,
    #$041F, #$042F, #$0420, #$0421, #$0422, #$0423, #$0416, #$0412,
    #$042C, #$042B, #$0417, #$0428, #$042D, #$0429, #$0427, #$042A);

function TKOI8_RCodec.DecodeChar(const P: ByteChar): WideChar;
begin
  if Ord(P) < $80 then
    Result := WideChar(P)
  else
    Result := KOI8_RMap[Ord(P)];
end;

function TKOI8_RCodec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  Result := CharFromHighMap(Ch, KOI8_RMap, 'KOI8-R');
end;



{                                                                              }
{ JIS_X0201                                                                    }
{                                                                              }
function TJIS_X0201Codec.DecodeChar(const P: ByteChar): WideChar;
begin
  case Ord(P) of
    $20..$5B,
    $5D..$7D  : Result := WideChar(P);
    $5C       : Result := #$00A5;  //  YEN SIGN
    $7E       : Result := #$203E;  //  OVERLINE
    $A1..$DF  : Result := WideChar(Ord(P) + $FEC0);
  else
    Result := #$FFFF; // <not a character>
  end;
end;

function TJIS_X0201Codec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  case Ord(Ch) of
    $0020..$005B,
    $005D..$007D  : Result := AnsiChar(Ch);
    $00A5         : Result := AnsiChar($5C);  //  YEN SIGN
    $203E         : Result := AnsiChar($7E);  //  OVERLINE
    $FF61..$FF9F  : Result := AnsiChar(Ord(Ch) - $FEC0);
  else
    raise EConvertError.CreateFmt(SCannotConvert, [Ord(Ch), 'JIS_X0201']);
  end;
end;



{                                                                              }
{ NextStep                                                                     }
{                                                                              }
const
  NextStepMap : AnsiCharHighMap = (
    #$00A0,  //  NO-BREAK SPACE
    #$00C0,  //  LATIN CAPITAL LETTER A WITH GRAVE
    #$00C1,  //  LATIN CAPITAL LETTER A WITH ACUTE
    #$00C2,  //  LATIN CAPITAL LETTER A WITH CIRCUMFLEX
    #$00C3,  //  LATIN CAPITAL LETTER A WITH TILDE
    #$00C4,  //  LATIN CAPITAL LETTER A WITH DIAERESIS
    #$00C5,  //  LATIN CAPITAL LETTER A WITH RING
    #$00C7,  //  LATIN CAPITAL LETTER C WITH CEDILLA
    #$00C8,  //  LATIN CAPITAL LETTER E WITH GRAVE
    #$00C9,  //  LATIN CAPITAL LETTER E WITH ACUTE
    #$00CA,  //  LATIN CAPITAL LETTER E WITH CIRCUMFLEX
    #$00CB,  //  LATIN CAPITAL LETTER E WITH DIAERESIS
    #$00CC,  //  LATIN CAPITAL LETTER I WITH GRAVE
    #$00CD,  //  LATIN CAPITAL LETTER I WITH ACUTE
    #$00CE,  //  LATIN CAPITAL LETTER I WITH CIRCUMFLEX
    #$00CF,  //  LATIN CAPITAL LETTER I WITH DIAERESIS
    #$00D0,  //  LATIN CAPITAL LETTER ETH
    #$00D1,  //  LATIN CAPITAL LETTER N WITH TILDE
    #$00D2,  //  LATIN CAPITAL LETTER O WITH GRAVE
    #$00D3,  //  LATIN CAPITAL LETTER O WITH ACUTE
    #$00D4,  //  LATIN CAPITAL LETTER O WITH CIRCUMFLEX
    #$00D5,  //  LATIN CAPITAL LETTER O WITH TILDE
    #$00D6,  //  LATIN CAPITAL LETTER O WITH DIAERESIS
    #$00D9,  //  LATIN CAPITAL LETTER U WITH GRAVE
    #$00DA,  //  LATIN CAPITAL LETTER U WITH ACUTE
    #$00DB,  //  LATIN CAPITAL LETTER U WITH CIRCUMFLEX
    #$00DC,  //  LATIN CAPITAL LETTER U WITH DIAERESIS
    #$00DD,  //  LATIN CAPITAL LETTER Y WITH ACUTE
    #$00DE,  //  LATIN CAPITAL LETTER THORN
    #$00B5,  //  MICRO SIGN
    #$00D7,  //  MULTIPLICATION SIGN
    #$00F7,  //  DIVISION SIGN
    #$00A9,  //  COPYRIGHT SIGN
    #$00A1,
    #$00A2,
    #$00A3,
    #$2044,  //  FRACTION SLASH
    #$00A5,
    #$0192,  //  LATIN SMALL LETTER F WITH HOOK
    #$00A7,
    #$00A4,  //  CURRENCY SIGN
    #$2019,  //  RIGHT SINGLE QUOTATION MARK
    #$201C,  //  LEFT DOUBLE QUOTATION MARK
    #$00AB,
    #$2039,  //  LATIN SMALL LETTER
    #$203A,  //  LATIN SMALL LETTER
    #$FB01,  //  LATIN SMALL LIGATURE FI
    #$FB02,  //  LATIN SMALL LIGATURE FL
    #$00AE,  //  REGISTERED SIGN
    #$2013,  //  EN DASH
    #$2020,  //  DAGGER
    #$2021,  //  DOUBLE DAGGER
    #$00B7,  //  MIDDLE DOT
    #$00A6,  //  BROKEN BAR
    #$00B6,
    #$2022,  //  BULLET
    #$201A,  //  SINGLE LOW-9 QUOTATION MARK
    #$201E,  //  DOUBLE LOW-9 QUOTATION MARK
    #$201D,  //  RIGHT DOUBLE QUOTATION MARK
    #$00BB,
    #$2026,  //  HORIZONTAL ELLIPSIS
    #$2030,  //  PER MILLE SIGN
    #$00AC,  //  NOT SIGN
    #$00BF,
    #$00B9,  //  SUPERSCRIPT ONE
    #$02CB,  //  MODIFIER LETTER GRAVE ACCENT
    #$00B4,  //  ACUTE ACCENT
    #$02C6,  //  MODIFIER LETTER CIRCUMFLEX ACCENT
    #$02DC,  //  SMALL TILDE
    #$00AF,  //  MACRON
    #$02D8,  //  BREVE
    #$02D9,  //  DOT ABOVE
    #$00A8,  //  DIAERESIS
    #$00B2,  //  SUPERSCRIPT TWO
    #$02DA,  //  RING ABOVE
    #$00B8,  //  CEDILLA
    #$00B3,  //  SUPERSCRIPT THREE
    #$02DD,  //  DOUBLE ACUTE ACCENT
    #$02DB,  //  OGONEK
    #$02C7,  //  CARON
    #$2014,  //  EM DASH
    #$00B1,  //  PLUS-MINUS SIGN
    #$00BC,  //  VULGAR FRACTION ONE QUARTER
    #$00BD,  //  VULGAR FRACTION ONE HALF
    #$00BE,  //  VULGAR FRACTION THREE QUARTERS
    #$00E0,  //  LATIN SMALL LETTER A WITH GRAVE
    #$00E1,  //  LATIN SMALL LETTER A WITH ACUTE
    #$00E2,  //  LATIN SMALL LETTER A WITH CIRCUMFLEX
    #$00E3,  //  LATIN SMALL LETTER A WITH TILDE
    #$00E4,  //  LATIN SMALL LETTER A WITH DIAERESIS
    #$00E5,  //  LATIN SMALL LETTER A WITH RING ABOVE
    #$00E7,  //  LATIN SMALL LETTER C WITH CEDILLA
    #$00E8,  //  LATIN SMALL LETTER E WITH GRAVE
    #$00E9,  //  LATIN SMALL LETTER E WITH ACUTE
    #$00EA,  //  LATIN SMALL LETTER E WITH CIRCUMFLEX
    #$00EB,  //  LATIN SMALL LETTER E WITH DIAERESIS
    #$00EC,  //  LATIN SMALL LETTER I WITH GRAVE
    #$00C6,  //  LATIN CAPITAL LETTER AE
    #$00ED,  //  LATIN SMALL LETTER I WITH ACUTE
    #$00AA,  //  FEMININE ORDINAL INDICATOR
    #$00EE,  //  LATIN SMALL LETTER I WITH CIRCUMFLEX
    #$00EF,  //  LATIN SMALL LETTER I WITH DIAERESIS
    #$00F0,  //  LATIN SMALL LETTER ETH
    #$00F1,  //  LATIN SMALL LETTER N WITH TILDE
    #$0141,  //  LATIN CAPITAL LETTER L WITH STROKE
    #$00D8,  //  LATIN CAPITAL LETTER O WITH STROKE
    #$0152,  //  LATIN CAPITAL LIGATURE OE
    #$00BA,  //  MASCULINE ORDINAL INDICATOR
    #$00F2,  //  LATIN SMALL LETTER O WITH GRAVE
    #$00F3,  //  LATIN SMALL LETTER O WITH ACUTE
    #$00F4,  //  LATIN SMALL LETTER O WITH CIRCUMFLEX
    #$00F5,  //  LATIN SMALL LETTER O WITH TILDE
    #$00F6,  //  LATIN SMALL LETTER O WITH DIAERESIS
    #$00E6,  //  LATIN SMALL LETTER AE
    #$00F9,  //  LATIN SMALL LETTER U WITH GRAVE
    #$00FA,  //  LATIN SMALL LETTER U WITH ACUTE
    #$00FB,  //  LATIN SMALL LETTER U WITH CIRCUMFLEX
    #$0131,  //  LATIN SMALL LETTER DOTLESS I
    #$00FC,  //  LATIN SMALL LETTER U WITH DIAERESIS
    #$00FD,  //  LATIN SMALL LETTER Y WITH ACUTE
    #$0142,  //  LATIN SMALL LETTER L WITH STROKE
    #$00F8,  //  LATIN SMALL LETTER O WITH STROKE
    #$0153,  //  LATIN SMALL LIGATURE OE
    #$00DF,  //  LATIN SMALL LETTER SHARP S
    #$00FE,  //  LATIN SMALL LETTER THORN
    #$00FF,  //  LATIN SMALL LETTER Y WITH DIAERESIS
    #$FFFF,  //  Not defined
    #$FFFF   //  Not defined
  );

function TNextStepCodec.DecodeChar(const P: ByteChar): WideChar;
begin
  if Ord(P) < $80 then
    Result := WideChar(P)
  else
    Result := NextStepMap[Ord(P)];
end;

function TNextStepCodec.EncodeChar(const Ch: WideChar): ByteChar;
begin
  case Ord(Ch) of
    $0000..$007F,
    $00A1..$00A3,
    $00A5,
    $00A7,
    $00AB,
    $00B6,
    $00BB,
    $00BF : Result := AnsiChar(Ch);
    $00A0 : Result := AnsiChar($80);  //  NO-BREAK SPACE
    $00A4 : Result := AnsiChar($A8);  //  CURRENCY SIGN
    $00A6 : Result := AnsiChar($B5);  //  BROKEN BAR
    $00A8 : Result := AnsiChar($C8);  //  DIAERESIS
    $00A9 : Result := AnsiChar($A0);  //  COPYRIGHT SIGN
    $00AA : Result := AnsiChar($E3);  //  FEMININE ORDINAL INDICATOR
    $00AC : Result := AnsiChar($BE);  //  NOT SIGN
    $00AE : Result := AnsiChar($B0);  //  REGISTERED SIGN
    $00AF : Result := AnsiChar($C5);  //  MACRON
    $00B1 : Result := AnsiChar($D1);  //  PLUS-MINUS SIGN
    $00B2 : Result := AnsiChar($C9);  //  SUPERSCRIPT TWO
    $00B3 : Result := AnsiChar($CC);  //  SUPERSCRIPT THREE
    $00B4 : Result := AnsiChar($C2);  //  ACUTE ACCENT
    $00B5 : Result := AnsiChar($9D);  //  MICRO SIGN
    $00B7 : Result := AnsiChar($B4);  //  MIDDLE DOT
    $00B8 : Result := AnsiChar($CB);  //  CEDILLA
    $00B9 : Result := AnsiChar($C0);  //  SUPERSCRIPT ONE
    $00BA : Result := AnsiChar($EB);  //  MASCULINE ORDINAL INDICATOR
    $00BC : Result := AnsiChar($D2);  //  VULGAR FRACTION ONE QUARTER
    $00BD : Result := AnsiChar($D3);  //  VULGAR FRACTION ONE HALF
    $00BE : Result := AnsiChar($D4);  //  VULGAR FRACTION THREE QUARTERS
    $00C0 : Result := AnsiChar($81);  //  LATIN CAPITAL LETTER A WITH GRAVE
    $00C1 : Result := AnsiChar($82);  //  LATIN CAPITAL LETTER A WITH ACUTE
    $00C2 : Result := AnsiChar($83);  //  LATIN CAPITAL LETTER A WITH CIRCUMFLEX
    $00C3 : Result := AnsiChar($84);  //  LATIN CAPITAL LETTER A WITH TILDE
    $00C4 : Result := AnsiChar($85);  //  LATIN CAPITAL LETTER A WITH DIAERESIS
    $00C5 : Result := AnsiChar($86);  //  LATIN CAPITAL LETTER A WITH RING
    $00C6 : Result := AnsiChar($E1);  //  LATIN CAPITAL LETTER AE
    $00C7 : Result := AnsiChar($87);  //  LATIN CAPITAL LETTER C WITH CEDILLA
    $00C8 : Result := AnsiChar($88);  //  LATIN CAPITAL LETTER E WITH GRAVE
    $00C9 : Result := AnsiChar($89);  //  LATIN CAPITAL LETTER E WITH ACUTE
    $00CA : Result := AnsiChar($8A);  //  LATIN CAPITAL LETTER E WITH CIRCUMFLEX
    $00CB : Result := AnsiChar($8B);  //  LATIN CAPITAL LETTER E WITH DIAERESIS
    $00CC : Result := AnsiChar($8C);  //  LATIN CAPITAL LETTER I WITH GRAVE
    $00CD : Result := AnsiChar($8D);  //  LATIN CAPITAL LETTER I WITH ACUTE
    $00CE : Result := AnsiChar($8E);  //  LATIN CAPITAL LETTER I WITH CIRCUMFLEX
    $00CF : Result := AnsiChar($8F);  //  LATIN CAPITAL LETTER I WITH DIAERESIS
    $00D0 : Result := AnsiChar($90);  //  LATIN CAPITAL LETTER ETH
    $00D1 : Result := AnsiChar($91);  //  LATIN CAPITAL LETTER N WITH TILDE
    $00D2 : Result := AnsiChar($92);  //  LATIN CAPITAL LETTER O WITH GRAVE
    $00D3 : Result := AnsiChar($93);  //  LATIN CAPITAL LETTER O WITH ACUTE
    $00D4 : Result := AnsiChar($94);  //  LATIN CAPITAL LETTER O WITH CIRCUMFLEX
    $00D5 : Result := AnsiChar($95);  //  LATIN CAPITAL LETTER O WITH TILDE
    $00D6 : Result := AnsiChar($96);  //  LATIN CAPITAL LETTER O WITH DIAERESIS
    $00D7 : Result := AnsiChar($9E);  //  MULTIPLICATION SIGN
    $00D8 : Result := AnsiChar($E9);  //  LATIN CAPITAL LETTER O WITH STROKE
    $00D9 : Result := AnsiChar($97);  //  LATIN CAPITAL LETTER U WITH GRAVE
    $00DA : Result := AnsiChar($98);  //  LATIN CAPITAL LETTER U WITH ACUTE
    $00DB : Result := AnsiChar($99);  //  LATIN CAPITAL LETTER U WITH CIRCUMFLEX
    $00DC : Result := AnsiChar($9A);  //  LATIN CAPITAL LETTER U WITH DIAERESIS
    $00DD : Result := AnsiChar($9B);  //  LATIN CAPITAL LETTER Y WITH ACUTE
    $00DE : Result := AnsiChar($9C);  //  LATIN CAPITAL LETTER THORN
    $00DF : Result := AnsiChar($FB);  //  LATIN SMALL LETTER SHARP S
    $00E0 : Result := AnsiChar($D5);  //  LATIN SMALL LETTER A WITH GRAVE
    $00E1 : Result := AnsiChar($D6);  //  LATIN SMALL LETTER A WITH ACUTE
    $00E2 : Result := AnsiChar($D7);  //  LATIN SMALL LETTER A WITH CIRCUMFLEX
    $00E3 : Result := AnsiChar($D8);  //  LATIN SMALL LETTER A WITH TILDE
    $00E4 : Result := AnsiChar($D9);  //  LATIN SMALL LETTER A WITH DIAERESIS
    $00E5 : Result := AnsiChar($DA);  //  LATIN SMALL LETTER A WITH RING ABOVE
    $00E6 : Result := AnsiChar($F1);  //  LATIN SMALL LETTER AE
    $00E7 : Result := AnsiChar($DB);  //  LATIN SMALL LETTER C WITH CEDILLA
    $00E8 : Result := AnsiChar($DC);  //  LATIN SMALL LETTER E WITH GRAVE
    $00E9 : Result := AnsiChar($DD);  //  LATIN SMALL LETTER E WITH ACUTE
    $00EA : Result := AnsiChar($DE);  //  LATIN SMALL LETTER E WITH CIRCUMFLEX
    $00EB : Result := AnsiChar($DF);  //  LATIN SMALL LETTER E WITH DIAERESIS
    $00EC : Result := AnsiChar($E0);  //  LATIN SMALL LETTER I WITH GRAVE
    $00ED : Result := AnsiChar($E2);  //  LATIN SMALL LETTER I WITH ACUTE
    $00EE : Result := AnsiChar($E4);  //  LATIN SMALL LETTER I WITH CIRCUMFLEX
    $00EF : Result := AnsiChar($E5);  //  LATIN SMALL LETTER I WITH DIAERESIS
    $00F0 : Result := AnsiChar($E6);  //  LATIN SMALL LETTER ETH
    $00F1 : Result := AnsiChar($E7);  //  LATIN SMALL LETTER N WITH TILDE
    $00F2 : Result := AnsiChar($EC);  //  LATIN SMALL LETTER O WITH GRAVE
    $00F3 : Result := AnsiChar($ED);  //  LATIN SMALL LETTER O WITH ACUTE
    $00F4 : Result := AnsiChar($EE);  //  LATIN SMALL LETTER O WITH CIRCUMFLEX
    $00F5 : Result := AnsiChar($EF);  //  LATIN SMALL LETTER O WITH TILDE
    $00F6 : Result := AnsiChar($F0);  //  LATIN SMALL LETTER O WITH DIAERESIS
    $00F7 : Result := AnsiChar($9F);  //  DIVISION SIGN
    $00F8 : Result := AnsiChar($F9);  //  LATIN SMALL LETTER O WITH STROKE
    $00F9 : Result := AnsiChar($F2);  //  LATIN SMALL LETTER U WITH GRAVE
    $00FA : Result := AnsiChar($F3);  //  LATIN SMALL LETTER U WITH ACUTE
    $00FB : Result := AnsiChar($F4);  //  LATIN SMALL LETTER U WITH CIRCUMFLEX
    $00FC : Result := AnsiChar($F6);  //  LATIN SMALL LETTER U WITH DIAERESIS
    $00FD : Result := AnsiChar($F7);  //  LATIN SMALL LETTER Y WITH ACUTE
    $00FE : Result := AnsiChar($FC);  //  LATIN SMALL LETTER THORN
    $00FF : Result := AnsiChar($FD);  //  LATIN SMALL LETTER Y WITH DIAERESIS
    $0131 : Result := AnsiChar($F5);  //  LATIN SMALL LETTER DOTLESS I
    $0141 : Result := AnsiChar($E8);  //  LATIN CAPITAL LETTER L WITH STROKE
    $0142 : Result := AnsiChar($F8);  //  LATIN SMALL LETTER L WITH STROKE
    $0152 : Result := AnsiChar($EA);  //  LATIN CAPITAL LIGATURE OE
    $0153 : Result := AnsiChar($FA);  //  LATIN SMALL LIGATURE OE
    $0192 : Result := AnsiChar($A6);  //  LATIN SMALL LETTER F WITH HOOK
    $02C6 : Result := AnsiChar($C3);  //  MODIFIER LETTER CIRCUMFLEX ACCENT
    $02C7 : Result := AnsiChar($CF);  //  CARON
    $02CB : Result := AnsiChar($C1);  //  MODIFIER LETTER GRAVE ACCENT
    $02D8 : Result := AnsiChar($C6);  //  BREVE
    $02D9 : Result := AnsiChar($C7);  //  DOT ABOVE
    $02DA : Result := AnsiChar($CA);  //  RING ABOVE
    $02DB : Result := AnsiChar($CE);  //  OGONEK
    $02DC : Result := AnsiChar($C4);  //  SMALL TILDE
    $02DD : Result := AnsiChar($CD);  //  DOUBLE ACUTE ACCENT
    $2013 : Result := AnsiChar($B1);  //  EN DASH
    $2014 : Result := AnsiChar($D0);  //  EM DASH
    $2019 : Result := AnsiChar($A9);  //  RIGHT SINGLE QUOTATION MARK
    $201A : Result := AnsiChar($B8);  //  SINGLE LOW-9 QUOTATION MARK
    $201C : Result := AnsiChar($AA);  //  LEFT DOUBLE QUOTATION MARK
    $201D : Result := AnsiChar($BA);  //  RIGHT DOUBLE QUOTATION MARK
    $201E : Result := AnsiChar($B9);  //  DOUBLE LOW-9 QUOTATION MARK
    $2020 : Result := AnsiChar($B2);  //  DAGGER
    $2021 : Result := AnsiChar($B3);  //  DOUBLE DAGGER
    $2022 : Result := AnsiChar($B7);  //  BULLET
    $2026 : Result := AnsiChar($BC);  //  HORIZONTAL ELLIPSIS
    $2030 : Result := AnsiChar($BD);  //  PER MILLE SIGN
    $2039 : Result := AnsiChar($AC);  //  LATIN SMALL LETTER
    $203A : Result := AnsiChar($AD);  //  LATIN SMALL LETTER
    $2044 : Result := AnsiChar($A4);  //  FRACTION SLASH
    $FB01 : Result := AnsiChar($AE);  //  LATIN SMALL LIGATURE FI
    $FB02 : Result := AnsiChar($AF);  //  LATIN SMALL LIGATURE FL
    $FFFD : Result := AnsiChar($FE);  //  Not Defined
    $FFFF : Result := AnsiChar($FE);  //  Not Defined
  else
    raise EConvertError.CreateFmt(SCannotConvert, [Ord(Ch), 'NextStep']);
  end;
end;



{                                                                              }
{ Test cases                                                                   }
{                                                                              }
{$IFDEF UNICODECODECS_TEST}
{$ASSERTIONS ON}
procedure Test;
begin
  // Codecs
  RegisterDefaultCodecs;
  Assert(not Assigned(GetCodecClassByAlias('')), 'GetCodecClassByAlias');
  Assert(GetCodecClassByAlias('ascii') = TUSASCIICodec, 'GetCodecClassByAlias');
  Assert(GetCodecClassByAlias('us-ascii') = TUSASCIICodec, 'GetCodecClassByAlias');
  Assert(GetCodecClassByAlias('ISO-8859-10') = TISO8859_10Codec, 'GetCodecClassByAlias');
end;
{$ENDIF}



end.

