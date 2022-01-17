{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals 5.00                                        }
{   File name:        flcStdTypes.pas                                          }
{   File version:     5.07                                                     }
{   Description:      Standard type definitions.                               }
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
{   2000-2018   1.01  Initial versions                                         }
{   2018/07/11  5.02  Move to flcStdTypes unit from flcUtils.                  }
{   2018/08/12  5.03  String type changes.                                     }
{   2019/04/02  5.04  LongInt/LongWord changes.                                }
{   2020/03/30  5.05  Add NativeWord.                                          }
{   2020/06/02  5.06  UInt arrays.                                             }
{   2020/06/09  5.07  Define PAnsiChar when not supported.                     }
{                                                                              }
{ Supported compilers:                                                         }
{                                                                              }
{   Delphi 2010-10.4 Win32/Win64        5.06  2020/06/02                       }
{   Delphi 10.2 Linux64                 5.06  2020/06/02                       }
{   FreePascal 3.0.4 Win64              5.06  2020/06/02                       }
{                                                                              }
{******************************************************************************}

{$INCLUDE ..\flcInclude.inc}

{$IFDEF FREEPASCAL}
  {$WARNINGS OFF}
  {$HINTS OFF}
{$ENDIF}

{$IFDEF DEBUG}
{$IFDEF TEST}
  {$DEFINE STDTYPES_TEST}
{$ENDIF}
{$ENDIF}

unit flcStdTypes;

interface

uses
  { System }
  SysUtils;



{                                                                              }
{ Integer types                                                                }
{                                                                              }
type
  Int8      = ShortInt;
  Int16     = SmallInt;
  {$IFDEF SupportFixedInt}
  Int32     = FixedInt;
  {$ELSE}
  Int32     = LongInt;
  {$ENDIF}

  UInt8     = Byte;
  UInt16    = Word;
  {$IFDEF SupportFixedUInt}
  UInt32    = FixedUInt;
  {$ELSE}
  UInt32    = LongWord;
  {$ENDIF}
  {$IFNDEF SupportUInt64}
  UInt64    = type Int64;
  {$ENDIF}

  Word8     = UInt8;
  Word16    = UInt16;
  Word32    = UInt32;
  Word64    = UInt64;

  {$IFNDEF SupportNativeInt}
  {$IFDEF CPU_X86_64}
  NativeInt   = type Int64;
  {$ELSE}
  NativeInt   = type Int32;
  {$ENDIF}
  PNativeInt  = ^NativeInt;
  {$ENDIF}
  {$IFDEF FREEPASCAL}
  PNativeInt  = ^NativeInt;
  {$ENDIF}
  {$IFDEF DELPHI2010}
  PNativeInt  = ^NativeInt;
  {$ENDIF}

  {$IFNDEF SupportNativeUInt}
  {$IFDEF CPU_X86_64}
  NativeUInt  = type Word64;
  {$ELSE}
  NativeUInt  = type Word32;
  {$ENDIF}
  PNativeUInt = ^NativeUInt;
  {$ENDIF}
  {$IFDEF FREEPASCAL}
  PNativeUInt = ^NativeUInt;
  {$ENDIF}
  {$IFDEF DELPHI2010}
  PNativeUInt = ^NativeUInt;
  {$ENDIF}

  NativeWord = NativeUInt;
  PNativeWord = ^NativeWord;

  {$IFDEF DELPHI5_DOWN}
  PByte       = ^Byte;
  PWord       = ^Word;
  PLongWord   = ^LongWord;
  PShortInt   = ^ShortInt;
  PSmallInt   = ^SmallInt;
  PLongInt    = ^LongInt;
  PInteger    = ^Integer;
  PInt64      = ^Int64;
  {$ENDIF}

  PInt8     = ^Int8;
  PInt16    = ^Int16;
  PInt32    = ^Int32;

  PWord8    = ^Word8;
  PWord16   = ^Word16;
  PWord32   = ^Word32;
  PWord64   = ^Word64;

  PUInt8    = ^UInt8;
  PUInt16   = ^UInt16;
  PUInt32   = ^UInt32;
  PUInt64   = ^UInt64;

  Int16Rec = packed record
    case Integer of
      0 : (Lo, Hi : Byte);
      1 : (Bytes  : array[0..1] of Byte);
  end;

  Int32Rec = packed record
    case Integer of
      0 : (Lo, Hi : Word);
      1 : (Words  : array[0..1] of Word);
      2 : (Bytes  : array[0..3] of Byte);
  end;
  PInt32Rec = ^Int32Rec;

const
  MinByte       = Low(Byte);
  MaxByte       = High(Byte);
  MinWord       = Low(Word);
  MaxWord       = High(Word);
  MinWord16     = Low(Word16);
  MaxWord16     = High(Word16);
  MinWord32     = Low(Word32);
  MaxWord32     = High(Word32);
  MinWord64     = Low(Word64);
  MaxWord64     = High(Word64);
  MinUInt8      = Low(UInt8);
  MaxUInt8      = High(UInt8);
  MinUInt16     = Low(UInt16);
  MaxUInt16     = High(UInt16);
  MinUInt32     = Low(UInt32);
  MaxUInt32     = High(UInt32);
  MinUInt64     = UInt64(Low(UInt64));
  MaxUInt64     = UInt64(High(UInt64));
  MinShortInt   = Low(ShortInt);
  MaxShortInt   = High(ShortInt);
  MinSmallInt   = Low(SmallInt);
  MaxSmallInt   = High(SmallInt);
  MinLongWord   = LongWord(Low(LongWord));
  MaxLongWord   = LongWord(High(LongWord));
  MinLongInt    = LongInt(Low(LongInt));
  MaxLongInt    = LongInt(High(LongInt));
  MinInt8       = Low(Int8);
  MaxInt8       = High(Int8);
  MinInt16      = Low(Int16);
  MaxInt16      = High(Int16);
  MinInt32      = Low(Int32);
  MaxInt32      = High(Int32);
  MinInt64      = Int64(Low(Int64));
  MaxInt64      = Int64(High(Int64));
  MinInteger    = Integer(Low(Integer));
  MaxInteger    = Integer(High(Integer));
  MinCardinal   = Cardinal(Low(Cardinal));
  MaxCardinal   = Cardinal(High(Cardinal));
  MinNativeUInt = NativeUInt(Low(NativeUInt));
  MaxNativeUInt = NativeUInt(High(NativeUInt));
  MinNativeInt  = NativeInt(Low(NativeInt));
  MaxNativeInt  = NativeInt(High(NativeInt));
  MinNativeWord = NativeWord(Low(NativeWord));
  MaxNativeWord = NativeWord(High(NativeWord));

const
  LongIntSize    = SizeOf(LongInt);
  LongIntBits    = SizeOf(LongInt) * 8;
  LongWordSize   = SizeOf(LongWord);
  LongWordBits   = SizeOf(LongWord) * 8;
  NativeWordSize = SizeOf(NativeInt);
  NativeWordBits = NativeWordSize * 8;



{                                                                              }
{ Boolean types                                                                }
{                                                                              }
type
  Bool8     = ByteBool;
  Bool16    = WordBool;
  Bool32    = LongBool;

  {$IFDEF DELPHI5_DOWN}
  PBoolean  = ^Boolean;
  PByteBool = ^ByteBool;
  PWordBool = ^WordBool;
  {$ENDIF}
  {$IFNDEF FREEPASCAL}
  PLongBool = ^LongBool;
  {$ENDIF}

  PBool8    = ^Bool8;
  PBool16   = ^Bool16;
  PBool32   = ^Bool32;



{                                                                              }
{ Real number types                                                            }
{                                                                              }
type
  {$IFDEF DELPHI5_DOWN}
  PSingle   = ^Single;
  PDouble   = ^Double;
  PExtended = ^Extended;
  {$ENDIF}

  {$IFNDEF ExtendedIsDouble}
  ExtendedRec = packed record
    case Boolean of
      True: (
        Mantissa : packed array[0..1] of LongWord; { MSB of [1] is the normalized 1 bit }
        Exponent : Word;                           { MSB is the sign bit                }
      );
      False: (Value: Extended);
  end;
  {$ENDIF}

  Float32 = Single;
  Float64 = Double;
  {$IFNDEF ExtendedIsDouble}
  Float80 = Extended;
  {$ENDIF}

  PFloat32 = ^Float32;
  PFloat64 = ^Float64;
  {$IFNDEF ExtendedIsDouble}
  PFloat80 = ^Float80;
  {$ENDIF}

  {$IFDEF ExtendedIsDouble}
  Float = Double;
  {$DEFINE FloatIsDouble}
  {$ELSE}
  Float = Extended;
  {$DEFINE FloatIsExtended}
  {$ENDIF}
  PFloat = ^Float;

const
  MinSingle   : Single   = 1.5E-45;
  MaxSingle   : Single   = 3.4E+38;
  MinDouble   : Double   = 5.0E-324;
  MaxDouble   : Double   = 1.7E+308;
  {$IFDEF ExtendedIsDouble}
  MinExtended : Extended = 5.0E-324;
  MaxExtended : Extended = 1.7E+308;
  {$ELSE}
  MinExtended : Extended = 3.4E-4932;
  MaxExtended : Extended = 1.1E+4932;
  {$ENDIF}

{$IFNDEF ExtendedIsDouble}
const
  ExtendedNan      : ExtendedRec = (Mantissa:($FFFFFFFF, $FFFFFFFF); Exponent:$7FFF);
  ExtendedInfinity : ExtendedRec = (Mantissa:($00000000, $80000000); Exponent:$7FFF);
{$ENDIF}

{$IFDEF SupportCurrency}
{$IFDEF DELPHI5_DOWN}
type
  PCurrency = ^Currency;
{$ENDIF}

const
  MinCurrency : Currency = -922337203685477.5807;
  MaxCurrency : Currency = 922337203685477.5807;
{$ENDIF}



{                                                                              }
{ Pointer                                                                      }
{                                                                              }
{$IFDEF DELPHI5_DOWN}
type
  PPointer = ^Pointer;
{$ENDIF}



{                                                                              }
{ ByteChar                                                                     }
{   ByteChar is an one byte character.                                         }
{                                                                              }
{$IFDEF SupportAnsiChar}
type
  ByteChar = AnsiChar;
{$ELSE}
{$IFDEF SupportUTF8Char}
type
  ByteChar = UTF8Char;
{$ELSE}
type
  ByteChar = Byte;
  {$DEFINE ByteCharIsOrd}
{$ENDIF}
{$ENDIF}
type
  PByteChar = ^ByteChar;



{                                                                              }
{ AnsiChar                                                                     }
{                                                                              }
{$IFNDEF SupportAnsiChar}
type
  AnsiChar = ByteChar;
  PAnsiChar = ^AnsiChar;
{$IFDEF ByteCharIsOrd}
  {$DEFINE AnsiCharIsOrd}
{$ENDIF}
{$ENDIF}




{                                                                              }
{ RawByteString                                                                }
{   RawByteString is an alias for AnsiString where all bytes are raw bytes     }
{   that do not undergo any character set translation.                         }
{   Under Delphi 2009 RawByteString is defined as "type AnsiString($FFFF)".    }
{                                                                              }
type
  RawByteChar = ByteChar;
  PRawByteChar = ^RawByteChar;

{$IFNDEF SupportRawByteString}
{$IFDEF SupportAnsiString}
type
  RawByteString = AnsiString;
{$ENDIF}
{$ENDIF}

{$IFDEF SupportRawByteString}
{$IFDEF FREEPASCAL}
type
  PRawByteString = ^RawByteString;
{$ENDIF}
{$ENDIF}



{                                                                              }
{ UTF8String                                                                   }
{   UTF8String is a variable character length encoding for Unicode strings.    }
{   For Ascii values, a UTF8String is the same as a AsciiString.               }
{   Under Delphi 2009 UTF8String is defined as "type AnsiString($FDE9)"        }
{                                                                              }
{$IFNDEF SupportUTF8Char}
type
  UTF8Char = ByteChar;
  PUTF8Char = ^UTF8Char;
{$ENDIF}

{$IFNDEF SupportUTF8String}
{$IFDEF SupportAnsiString}
type
  UTF8String = AnsiString;
{$ENDIF}
{$ENDIF}



{                                                                              }
{ WideString                                                                   }
{   WideChar is a 16-bit character.                                            }
{   WideString is not reference counted.                                       }
{                                                                              }
{$IFNDEF SupportWideChar}
type
  WideChar = Word;
  PWideChar = ^WideChar;
{$DEFINE WideCharIsOrd}
{$ENDIF}

type
  TWideCharMatchFunction = function (const Ch: WideChar): Boolean;



{                                                                              }
{ UnicodeString                                                                }
{   UnicodeString in Delphi 2009 is reference counted, code page aware,        }
{   variable character length unicode string. Defaults to UTF-16 encoding.     }
{                                                                              }
type
  UnicodeChar = WideChar;
  PUnicodeChar = ^UnicodeChar;

{$IFNDEF SupportUnicodeString}
{$IFDEF SupportWideString}
type
  UnicodeString = WideString;
{$ENDIF}
{$ENDIF}



{                                                                              }
{ UCS4String                                                                   }
{   UCS4Char is a 32-bit character from the Unicode character set.             }
{   UCS4String is a reference counted string of UCS4Char characters.           }
{                                                                              }
{$IFNDEF SupportUCS4String}
type
  UCS4Char = LongWord;
  PUCS4Char = ^UCS4Char;
{$ENDIF}



{                                                                              }
{ Sets                                                                         }
{                                                                              }
type
  ByteSet = set of Byte;
  ByteCharSet = set of ByteChar;

  PByteSet = ^ByteSet;
  PByteCharSet = ^ByteCharSet;



{                                                                              }
{ TBytes                                                                       }
{   TBytes is a dynamic array of bytes.                                        }
{                                                                              }
{$IFNDEF TBytesDeclared}
type
  TBytes = array of Bytes;
{$ENDIF}



{                                                                              }
{ Dynamic arrays                                                               }
{                                                                              }
type
  ByteArray = TBytes;
  WordArray = array of Word;
  LongWordArray = array of LongWord;
  CardinalArray = array of Cardinal;
  Word8Array = ByteArray;
  Word16Array = array of Word16;
  Word32Array = array of Word32;
  Word64Array = array of Word64;
  UInt8Array = ByteArray;
  UInt16Array = array of UInt16;
  UInt32Array = array of UInt32;
  UInt64Array = array of UInt64;
  NativeUIntArray = array of NativeUInt;
  NativeWordArray = NativeUIntArray;
  ShortIntArray = array of ShortInt;
  SmallIntArray = array of SmallInt;
  LongIntArray = array of LongInt;
  IntegerArray = array of Integer;
  Int8Array = array of Int8;
  Int16Array = array of Int16;
  Int32Array = array of Int32;
  Int64Array = array of Int64;
  NativeIntArray = array of NativeInt;
  SingleArray = array of Single;
  DoubleArray = array of Double;
  ExtendedArray = array of Extended;
  FloatArray = array of Float;
  CurrencyArray = array of Currency;
  BooleanArray = array of Boolean;
  {$IFDEF SupportAnsiString}
  AnsiStringArray = array of AnsiString;
  {$ENDIF}
  RawByteStringArray = array of RawByteString;
  UnicodeStringArray = array of UnicodeString;
  StringArray = array of String;
  PointerArray = array of Pointer;
  ObjectArray = array of TObject;
  InterfaceArray = array of IInterface;
  ByteCharSetArray = array of ByteCharSet;
  ByteSetArray = array of ByteSet;



{                                                                              }
{ Array pointers                                                               }
{                                                                              }

{ Maximum array elements                                                       }
const
  MaxArraySize = $7FFFFFFF; // 2 Gigabytes
  MaxByteArrayElements = MaxArraySize div Sizeof(Byte);
  MaxWord16ArrayElements = MaxArraySize div Sizeof(Word16);
  MaxWord32ArrayElements = MaxArraySize div Sizeof(Word32);
  MaxWord64ArrayElements = MaxArraySize div Sizeof(Word64);
  MaxLongWordArrayElements = MaxArraySize div Sizeof(LongWord);
  MaxCardinalArrayElements = MaxArraySize div Sizeof(Cardinal);
  MaxNativeUIntArrayElements = MaxArraySize div Sizeof(NativeUInt);
  MaxNativeWordArrayElements = MaxArraySize div Sizeof(NativeWord);
  MaxShortIntArrayElements = MaxArraySize div Sizeof(ShortInt);
  MaxSmallIntArrayElements = MaxArraySize div Sizeof(SmallInt);
  MaxLongIntArrayElements = MaxArraySize div Sizeof(LongInt);
  MaxIntegerArrayElements = MaxArraySize div Sizeof(Integer);
  MaxInt32ArrayElements = MaxArraySize div Sizeof(Int32);
  MaxInt64ArrayElements = MaxArraySize div Sizeof(Int64);
  MaxNativeIntArrayElements = MaxArraySize div Sizeof(NativeInt);
  MaxSingleArrayElements = MaxArraySize div Sizeof(Single);
  MaxDoubleArrayElements = MaxArraySize div Sizeof(Double);
  MaxExtendedArrayElements = MaxArraySize div Sizeof(Extended);
  MaxBooleanArrayElements = MaxArraySize div Sizeof(Boolean);
  {$IFDEF SupportCurrency}
  MaxCurrencyArrayElements = MaxArraySize div Sizeof(Currency);
  {$ENDIF}
  {$IFDEF SupportAnsiString}
  MaxAnsiStringArrayElements = MaxArraySize div Sizeof(AnsiString);
  {$ENDIF}
  MaxRawByteStringArrayElements = MaxArraySize div Sizeof(RawByteString);
  MaxUnicodeStringArrayElements = MaxArraySize div Sizeof(UnicodeString);
  MaxStringArrayElements = MaxArraySize div Sizeof(String);
  MaxPointerArrayElements = MaxArraySize div Sizeof(Pointer);
  MaxObjectArrayElements = MaxArraySize div Sizeof(TObject);
  MaxInterfaceArrayElements = MaxArraySize div Sizeof(IInterface);
  MaxCharSetArrayElements = MaxArraySize div Sizeof(ByteCharSet);
  MaxByteSetArrayElements = MaxArraySize div Sizeof(ByteSet);

{ Static array types                                                           }
type
  TStaticByteArray = array[0..MaxByteArrayElements - 1] of Byte;
  TStaticWord16Array = array[0..MaxWord16ArrayElements - 1] of Word16;
  TStaticWord32Array = array[0..MaxWord32ArrayElements - 1] of Word32;
  TStaticWord64Array = array[0..MaxWord64ArrayElements - 1] of Word64;
  TStaticLongWordArray = array[0..MaxLongWordArrayElements - 1] of LongWord;
  TStaticCardinalArray = array[0..MaxCardinalArrayElements - 1] of Cardinal;
  TStaticNativeUIntArray = array[0..MaxNativeUIntArrayElements - 1] of NativeUInt;
  TStaticNativeWordArray = array[0..MaxNativeWordArrayElements - 1] of NativeWord;
  TStaticShortIntArray = array[0..MaxShortIntArrayElements - 1] of ShortInt;
  TStaticSmallIntArray = array[0..MaxSmallIntArrayElements - 1] of SmallInt;
  TStaticLongIntArray = array[0..MaxLongIntArrayElements - 1] of LongInt;
  TStaticIntegerArray = array[0..MaxIntegerArrayElements - 1] of Integer;
  TStaticInt32Array = array[0..MaxInt32ArrayElements - 1] of Int32;
  TStaticInt64Array = array[0..MaxInt64ArrayElements - 1] of Int64;
  TStaticNativeIntArray = array[0..MaxNativeIntArrayElements - 1] of NativeInt;
  TStaticSingleArray = array[0..MaxSingleArrayElements - 1] of Single;
  TStaticDoubleArray = array[0..MaxDoubleArrayElements - 1] of Double;
  TStaticExtendedArray = array[0..MaxExtendedArrayElements - 1] of Extended;
  TStaticBooleanArray = array[0..MaxBooleanArrayElements - 1] of Boolean;
  {$IFDEF SupportCurrency}
  TStaticCurrencyArray = array[0..MaxCurrencyArrayElements - 1] of Currency;
  {$ENDIF}
  {$IFDEF SupportAnsiString}
  TStaticAnsiStringArray = array[0..MaxAnsiStringArrayElements - 1] of AnsiString;
  {$ENDIF}
  TStaticRawByteStringArray = array[0..MaxRawByteStringArrayElements - 1] of RawByteString;
  TStaticUnicodeStringArray = array[0..MaxUnicodeStringArrayElements - 1] of UnicodeString;
  {$IFDEF StringIsUnicode}
  TStaticStringArray = TStaticUnicodeStringArray;
  {$ELSE}
  TStaticStringArray = TStaticAnsiStringArray;
  {$ENDIF}
  TStaticPointerArray = array[0..MaxPointerArrayElements - 1] of Pointer;
  TStaticObjectArray = array[0..MaxObjectArrayElements - 1] of TObject;
  TStaticInterfaceArray = array[0..MaxInterfaceArrayElements - 1] of IInterface;
  TStaticCharSetArray = array[0..MaxCharSetArrayElements - 1] of ByteCharSet;
  TStaticByteSetArray = array[0..MaxByteSetArrayElements - 1] of ByteSet;

{ Static array pointers                                                        }
type
  PStaticByteArray = ^TStaticByteArray;
  PStaticWord16Array = ^TStaticWord16Array;
  PStaticWord32Array = ^TStaticWord32Array;
  PStaticWord64Array = ^TStaticWord64Array;
  PStaticLongWordArray = ^TStaticLongWordArray;
  PStaticCardinalArray = ^TStaticCardinalArray;
  PStaticNativeUIntArray = ^TStaticNativeUIntArray;
  PStaticNativeWordArray = ^TStaticNativeWordArray;
  PStaticShortIntArray = ^TStaticShortIntArray;
  PStaticSmallIntArray = ^TStaticSmallIntArray;
  PStaticLongIntArray = ^TStaticLongIntArray;
  PStaticIntegerArray = ^TStaticIntegerArray;
  PStaticInt32Array = ^TStaticInt32Array;
  PStaticInt64Array = ^TStaticInt64Array;
  PStaticNativeIntArray = ^TStaticNativeIntArray;
  PStaticSingleArray = ^TStaticSingleArray;
  PStaticDoubleArray = ^TStaticDoubleArray;
  PStaticExtendedArray = ^TStaticExtendedArray;
  PStaticBooleanArray = ^TStaticBooleanArray;
  {$IFDEF SupportCurrency}
  PStaticCurrencyArray = ^TStaticCurrencyArray;
  {$ENDIF}
  {$IFDEF SupportAnsiString}
  PStaticAnsiStringArray = ^TStaticAnsiStringArray;
  {$ENDIF}
  PStaticRawByteStringArray = ^TStaticRawByteStringArray;
  PStaticUnicodeStringArray = ^TStaticUnicodeStringArray;
  PStaticStringArray = ^TStaticStringArray;
  PStaticPointerArray = ^TStaticPointerArray;
  PStaticObjectArray = ^TStaticObjectArray;
  PStaticInterfaceArray = ^TStaticInterfaceArray;
  PStaticCharSetArray = ^TStaticCharSetArray;
  PStaticByteSetArray = ^TStaticByteSetArray;



{$IFDEF STDTYPES_TEST}
procedure Test;
{$ENDIF}



implementation



{$IFDEF STDTYPES_TEST}
{$ASSERTIONS ON}
procedure Test;
begin
  Assert(SizeOf(Int8) = 1);
  Assert(SizeOf(Int16) = 2);
  Assert(SizeOf(Int32) = 4);
  Assert(SizeOf(Int64) = 8);

  Assert(SizeOf(UInt8) = 1);
  Assert(SizeOf(UInt16) = 2);
  Assert(SizeOf(UInt32) = 4);
  Assert(SizeOf(UInt64) = 8);

  Assert(SizeOf(UInt8) = SizeOf(Word8));
  Assert(SizeOf(UInt16) = SizeOf(Word16));
  Assert(SizeOf(UInt32) = SizeOf(Word32));
  Assert(SizeOf(UInt64) = SizeOf(Word64));

  Assert(SizeOf(ByteChar) = 1);

  Assert(SizeOf(Double) = 8);

  {$IFDEF ExtendedIsDouble}
  Assert(SizeOf(Extended) = SizeOf(Double));
  {$ENDIF}
  {$IFDEF ExtendedIs16Bytes}
  Assert(SizeOf(Extended) = 16);
  {$ENDIF}

  {$IFDEF FloatIsDouble}
  Assert(SizeOf(Float) = SizeOf(Double));
  {$ENDIF}
  {$IFDEF FloatIsExtended}
  Assert(SizeOf(Float) = SizeOf(Extended));
  {$ENDIF}
end;
{$ENDIF}



end.

