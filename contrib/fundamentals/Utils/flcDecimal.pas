{******************************************************************************}
{                                                                              }
{   File name:        flcDecimal.pas                                           }
{   File version:     5.12                                                     }
{   Description:      Decimal number functions                                 }
{                                                                              }
{   Copyright:        Copyright (c) 2014-2020, David J Butler                  }
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
{   2014/10/19  0.01  Decimal32, Decimal64, Decimal128.                        }
{   2014/10/20  0.02  SDecimal32, SDecimal64, SDecimal128.                     }
{   2014/11/10  0.03  HugeDecimal.                                             }
{   2014/11/21  0.04  SHugeDecimal.                                            }
{   2015/05/05  0.05  RawByteString changes.                                   }
{   2015/05/06  4.06  Revised for Fundamentals 4.                              }
{   2016/01/09  5.07  Revised for Fundamentals 5.                              }
{   2016/01/10  5.08  Make rounding same under 32/64-bit compilers.            }
{   2018/07/17  5.09  Word32/Int32 changes.                                    }
{   2018/08/12  5.10  String type changes.                                     }
{   2019/02/22  5.11  Compilable with Delphi 7.                                }
{   2020/03/10  5.12  Modify for changes to flcInteger.                        }
{                                                                              }
{ Supported compilers:                                                         }
{                                                                              }
{   Delphi 2010-10.4 Win32/Win64        5.12  2020/06/02                       }
{   Delphi 10.2-10.4 Linux64            5.12  2020/06/02                       }
{   FreePascal 3.0.4 Win64              5.12  2020/06/02                       }
{                                                                              }
{******************************************************************************}

{$INCLUDE ..\flcInclude.inc}

{$IFDEF DEBUG}
{$IFDEF TEST}
  {$DEFINE DECIMAL_TEST}
{$ENDIF}
{$ENDIF}

unit flcDecimal;

interface

uses
  { System }
  SysUtils,

  { Fundamentals }
  flcStdTypes,
  flcInteger;



{ Error }

type
  EDecimalError = class(Exception);
  TDecimalConvertErrorType = (dceNoError, dceConvertError, dceOverflowError);



{                                                                              }
{ Structures                                                                   }
{                                                                              }
type
  // 5.4 digits
  // encoded as Word32 scaled 10^4
  // limited to 99999.9999
  Decimal32 = packed record
    Value32 : Word32;
  end;
  PDecimal32 = ^Decimal32;

  // 10.9 digits
  // encoded as Word64 scaled 10^9
  // limited to 9999999999.999999999
  Decimal64 = packed record
    Value64 : Word64Rec;
  end;
  PDecimal64 = ^Decimal64;

  // 19.19 digits
  // encoded as Word128 scaled 10^19
  // limited to 9999999999999999999.9999999999999999999
  Decimal128 = packed record
    Value128 : Word128;
  end;
  PDecimal128 = ^Decimal128;

  // arbitrary number of digits
  // internally represented as array of bytes and a count of
  // the number of decimal digits to the right of the decimal point
  HugeDecimal = packed record
    Precision : Integer;
    Digits : array of Byte;
  end;
  PHugeDecimal = ^HugeDecimal;

  // signed Decimal32
  SDecimal32 = packed record
    Sign  : Int8;
    Value : Decimal32;
  end;
  PSDecimal32 = ^SDecimal32;

  // signed Decimal64
  SDecimal64 = packed record
    Sign  : Int8;
    Value : Decimal64;
  end;
  PSDecimal64 = ^SDecimal64;

  // signed Decimal128
  SDecimal128 = packed record
    Sign  : Int8;
    Value : Decimal128;
  end;
  PSDecimal128 = ^SDecimal128;

  // signed HugeDecimal
  SHugeDecimal = packed record
    Sign  : Int8;
    Value : HugeDecimal;
  end;
  PSHugeDecimal = ^SHugeDecimal;



{                                                                              }
{ Decimal32                                                                    }
{                                                                              }
const
  Decimal32Digits       = 9;
  Decimal32Precision    = 4;
  Decimal32Scale        = 10000;
  Decimal32MaxInt       = 99999;
  Decimal32MaxValue     = 999999999;
  Decimal32RoundTerm    = Decimal32Scale div 2; // 5000
  Decimal32MinFloat     = -1.0 / Decimal32Scale / 2.0; // -0.00005
  Decimal32MinFloatD    : Double = Decimal32MinFloat;
  Decimal32MaxFloatLim  = (1 + Decimal32MaxValue) div Decimal32Scale + Decimal32MinFloat; // 99999.99995
  Decimal32MaxFloatLimD : Double = Decimal32MaxFloatLim;

procedure Decimal32InitZero(var A: Decimal32); {$IFDEF UseInline}inline;{$ENDIF}
procedure Decimal32InitOne(var A: Decimal32);
procedure Decimal32InitMax(var A: Decimal32);

function  Decimal32IsZero(const A: Decimal32): Boolean; {$IFDEF UseInline}inline;{$ENDIF}
function  Decimal32IsOne(const A: Decimal32): Boolean;
function  Decimal32IsMaximum(const A: Decimal32): Boolean;
function  Decimal32IsOverflow(const A: Decimal32): Boolean;

function  Word32IsDecimal32Range(const A: Word32): Boolean;
function  Int16IsDecimal32Range(const A: SmallInt): Boolean;
function  Int32IsDecimal32Range(const A: Int32): Boolean;
function  FloatIsDecimal32Range(const A: Double): Boolean;

procedure Decimal32InitWord8(var A: Decimal32; const B: Byte);
procedure Decimal32InitWord16(var A: Decimal32; const B: Word);
procedure Decimal32InitWord32(var A: Decimal32; const B: Word32);
procedure Decimal32InitInt32(var A: Decimal32; const B: Int32);
procedure Decimal32InitDecimal32(var A: Decimal32; const B: Decimal32);
procedure Decimal32InitFloat(var A: Decimal32; const B: Double);

function  Decimal32ToWord8(const A: Decimal32): Byte;
function  Decimal32ToWord16(const A: Decimal32): Word;
function  Decimal32ToWord32(const A: Decimal32): Word32;
function  Decimal32ToInt32(const A: Decimal32): Int32;
function  Decimal32ToFloat(const A: Decimal32): Double;

function  Decimal32Trunc(const A: Decimal32): Word32;
function  Decimal32Round(const A: Decimal32): Word32;
function  Decimal32FracWord(const A: Decimal32): Word;

function  Decimal32EqualsWord8(const A: Decimal32; const B: Byte): Boolean;
function  Decimal32EqualsWord16(const A: Decimal32; const B: Word): Boolean;
function  Decimal32EqualsWord32(const A: Decimal32; const B: Word32): Boolean;
function  Decimal32EqualsInt32(const A: Decimal32; const B: Int32): Boolean;
function  Decimal32EqualsDecimal32(const A: Decimal32; const B: Decimal32): Boolean;
function  Decimal32EqualsFloat(const A: Decimal32; const B: Double): Boolean;

function  Decimal32CompareWord8(const A: Decimal32; const B: Byte): Integer;
function  Decimal32CompareWord16(const A: Decimal32; const B: Word): Integer;
function  Decimal32CompareWord32(const A: Decimal32; const B: Word32): Integer;
function  Decimal32CompareInt32(const A: Decimal32; const B: Int32): Integer;
function  Decimal32CompareDecimal32(const A: Decimal32; const B: Decimal32): Integer;
function  Decimal32CompareFloat(const A: Decimal32; const B: Double): Integer;

procedure Decimal32AddWord8(var A: Decimal32; const B: Byte);
procedure Decimal32AddWord16(var A: Decimal32; const B: Word);
procedure Decimal32AddWord32(var A: Decimal32; const B: Word32);
procedure Decimal32AddDecimal32(var A: Decimal32; const B: Decimal32);

procedure Decimal32SubtractWord8(var A: Decimal32; const B: Byte);
procedure Decimal32SubtractWord16(var A: Decimal32; const B: Word);
procedure Decimal32SubtractWord32(var A: Decimal32; const B: Word32);
procedure Decimal32SubtractDecimal32(var A: Decimal32; const B: Decimal32);

procedure Decimal32MultiplyWord8(var A: Decimal32; const B: Byte);
procedure Decimal32MultiplyWord16(var A: Decimal32; const B: Word);
procedure Decimal32MultiplyWord32(var A: Decimal32; const B: Word32);
procedure Decimal32MultiplyDecimal32(var A: Decimal32; const B: Decimal32);

procedure Decimal32Sqr(var A: Decimal32);

procedure Decimal32DivideWord8(var A: Decimal32; const B: Byte);
procedure Decimal32DivideWord16(var A: Decimal32; const B: Word);
procedure Decimal32DivideWord32(var A: Decimal32; const B: Word32);
procedure Decimal32DivideDecimal32(var A: Decimal32; const B: Decimal32);

function  Decimal32ToStr(const A: Decimal32): String;
function  Decimal32ToStrB(const A: Decimal32): RawByteString;
function  Decimal32ToStrU(const A: Decimal32): UnicodeString;

function  TryStrToDecimal32(const A: String; out B: Decimal32): TDecimalConvertErrorType;
function  TryStrToDecimal32B(const A: RawByteString; out B: Decimal32): TDecimalConvertErrorType;

function  StrToDecimal32(const A: String): Decimal32;
function  StrToDecimal32B(const A: RawByteString): Decimal32;



{                                                                              }
{ Decimal64                                                                    }
{                                                                              }
const
  Decimal64Digits       = 19;
  Decimal64Precision    = 9;
  Decimal64Scale        = 1000000000;
  Decimal64MaxInt       = 9999999999; // 10 9's
  {$IFNDEF DELPHI7_DOWN}
  Decimal64MaxValue     = 9999999999999999999; // 19 9's
  {$ENDIF}
  Decimal64MaxValueW64  : Word64Rec = (Word32s:($89E7FFFF, $8AC72304)); // 9999999999999999999
  Decimal64RoundTerm    = Decimal64Scale div 2; // 500000000
  Decimal64MinFloat     = -1.0 / Decimal64Scale / 2.0; // -0.0000000005
  Decimal64MinFloatD    : Double = Decimal64MinFloat;
  {$IFDEF DELPHI7_DOWN}
  Decimal64MaxFloatLim  : Double = 999999999999999.9999999995;
  Decimal64MaxFloatLimD : Double = 999999999999999.9999999995;
  {$ELSE}
  Decimal64MaxFloatLim  = ((1 + Decimal64MaxValue) div Decimal64Scale) + Decimal64MinFloat; // 999999999999999.9999999995
  Decimal64MaxFloatLimD : Double = Decimal64MaxFloatLim;
  {$ENDIF}

procedure Decimal64InitZero(var A: Decimal64);
procedure Decimal64InitOne(var A: Decimal64);
procedure Decimal64InitMax(var A: Decimal64);

function  Decimal64IsZero(const A: Decimal64): Boolean;
function  Decimal64IsOne(const A: Decimal64): Boolean;
function  Decimal64IsMaximum(const A: Decimal64): Boolean;
function  Decimal64IsOverflow(const A: Decimal64): Boolean;

function  Word64IsDecimal64Range(const A: Word64Rec): Boolean;
function  Int32IsDecimal64Range(const A: Int32): Boolean;
function  Int64IsDecimal64Range(const A: Int64): Boolean;
function  FloatIsDecimal64Range(const A: Double): Boolean;

procedure Decimal64InitWord8(var A: Decimal64; const B: Byte);
procedure Decimal64InitWord16(var A: Decimal64; const B: Word);
procedure Decimal64InitWord32(var A: Decimal64; const B: Word32);
procedure Decimal64InitWord64(var A: Decimal64; const B: Word64Rec);
procedure Decimal64InitInt32(var A: Decimal64; const B: Int32);
procedure Decimal64InitInt64(var A: Decimal64; const B: Int64);
procedure Decimal64InitDecimal32(var A: Decimal64; const B: Decimal32);
procedure Decimal64InitDecimal64(var A: Decimal64; const B: Decimal64);
procedure Decimal64InitFloat(var A: Decimal64; const B: Double);

function  Decimal64ToWord8(const A: Decimal64): Byte;
function  Decimal64ToWord16(const A: Decimal64): Word;
function  Decimal64ToWord32(const A: Decimal64): Word32;
function  Decimal64ToWord64(const A: Decimal64): Word64Rec;
function  Decimal64ToInt32(const A: Decimal64): Int32;
function  Decimal64ToInt64(const A: Decimal64): Int64;
function  Decimal64ToDecimal32(const A: Decimal64): Decimal32;
function  Decimal64ToFloat(const A: Decimal64): Double;

function  Decimal64Trunc(const A: Decimal64): Int64;
function  Decimal64Round(const A: Decimal64): Int64;
function  Decimal64FracWord(const A: Decimal64): Word32;

function  Decimal64EqualsWord8(const A: Decimal64; const B: Byte): Boolean;
function  Decimal64EqualsWord16(const A: Decimal64; const B: Word): Boolean;
function  Decimal64EqualsWord32(const A: Decimal64; const B: Word32): Boolean;
function  Decimal64EqualsInt32(const A: Decimal64; const B: Int32): Boolean;
function  Decimal64EqualsInt64(const A: Decimal64; const B: Int64): Boolean;
function  Decimal64EqualsDecimal64(const A: Decimal64; const B: Decimal64): Boolean;
function  Decimal64EqualsFloat(const A: Decimal64; const B: Double): Boolean;

function  Decimal64CompareWord8(const A: Decimal64; const B: Byte): Integer;
function  Decimal64CompareWord16(const A: Decimal64; const B: Word): Integer;
function  Decimal64CompareWord32(const A: Decimal64; const B: Word32): Integer;
function  Decimal64CompareInt32(const A: Decimal64; const B: Int32): Integer;
function  Decimal64CompareInt64(const A: Decimal64; const B: Int64): Integer;
function  Decimal64CompareDecimal64(const A: Decimal64; const B: Decimal64): Integer;
function  Decimal64CompareFloat(const A: Decimal64; const B: Double): Integer;

procedure Decimal64AddWord8(var A: Decimal64; const B: Byte);
procedure Decimal64AddWord16(var A: Decimal64; const B: Word);
procedure Decimal64AddWord32(var A: Decimal64; const B: Word32);
procedure Decimal64AddDecimal64(var A: Decimal64; const B: Decimal64);

procedure Decimal64SubtractWord8(var A: Decimal64; const B: Byte);
procedure Decimal64SubtractWord16(var A: Decimal64; const B: Word);
procedure Decimal64SubtractWord32(var A: Decimal64; const B: Word32);
procedure Decimal64SubtractDecimal64(var A: Decimal64; const B: Decimal64);

procedure Decimal64MultiplyWord8(var A: Decimal64; const B: Byte);
procedure Decimal64MultiplyWord16(var A: Decimal64; const B: Word);
procedure Decimal64MultiplyWord32(var A: Decimal64; const B: Word32);
procedure Decimal64MultiplyDecimal64(var A: Decimal64; const B: Decimal64);

procedure Decimal64Sqr(var A: Decimal64);

procedure Decimal64DivideWord8(var A: Decimal64; const B: Byte);
procedure Decimal64DivideWord16(var A: Decimal64; const B: Word);
procedure Decimal64DivideWord32(var A: Decimal64; const B: Word32);
procedure Decimal64DivideDecimal64(var A: Decimal64; const B: Decimal64);

function  Decimal64ToStr(const A: Decimal64): String;
function  Decimal64ToStrB(const A: Decimal64): RawByteString;
function  Decimal64ToStrU(const A: Decimal64): UnicodeString;

function  TryStrToDecimal64(const A: String; out B: Decimal64): TDecimalConvertErrorType;
function  TryStrToDecimal64B(const A: RawByteString; out B: Decimal64): TDecimalConvertErrorType;

function  StrToDecimal64(const A: String): Decimal64;
function  StrToDecimal64B(const A: RawByteString): Decimal64;



{                                                                              }
{ Decimal128                                                                   }
{                                                                              }
const
  Decimal128Digits       = 38;
  Decimal128Precision    = 19;
  Decimal128Scale        : Word64Rec  = (Word32s:($89E80000, $8AC72304)); // 10000000000000000000
  Decimal128ScaleW128    : Word128 = (Word32s:($89E80000, $8AC72304, 0, 0)); // 10000000000000000000
  Decimal128ScaleF       : Double = 10000000000000000000.0;
  Decimal128MaxInt       : Word64Rec  = (Word32s:($89E7FFFF, $8AC72304)); // 9999999999999999999 (19 9's)
  Decimal128MaxValue     : Word128 = (Word32s:($FFFFFFFF, $98A223F, $5A86C47A, $4B3B4CA8)); // 99999999999999999999999999999999999999 (38 9's)
  Decimal128RoundTerm    : Word64Rec  = (Word32s:($44F40000, $45639182)); // Decimal128Scale div 2 = 5000000000000000000
  Decimal128MinFloat     = -0.00000000000000000005;
  Decimal128MinFloatD    : Double = Decimal128MinFloat;
  Decimal128MaxFloatLim  = 9999999999999999999.9999999999999999999;
  Decimal128MaxFloatLimD : Double = Decimal128MaxFloatLim;

procedure Decimal128InitZero(var A: Decimal128);
procedure Decimal128InitOne(var A: Decimal128);
procedure Decimal128InitMax(var A: Decimal128);

function  Decimal128IsZero(const A: Decimal128): Boolean;
function  Decimal128IsOne(const A: Decimal128): Boolean;
function  Decimal128IsMaximum(const A: Decimal128): Boolean;
function  Decimal128IsOverflow(const A: Decimal128): Boolean;

function  Word64IsDecimal128Range(const A: Word64Rec): Boolean;
function  Word128IsDecimal128Range(const A: Word128): Boolean;
function  Int64IsDecimal128Range(const A: Int64): Boolean;
function  Int128IsDecimal128Range(const A: Int128): Boolean;
function  FloatIsDecimal128Range(const A: Double): Boolean;

procedure Decimal128InitWord8(var A: Decimal128; const B: Byte);
procedure Decimal128InitWord16(var A: Decimal128; const B: Word);
procedure Decimal128InitWord32(var A: Decimal128; const B: Word32);
procedure Decimal128InitWord64(var A: Decimal128; const B: Word64Rec);
procedure Decimal128InitInt32(var A: Decimal128; const B: Int32);
procedure Decimal128InitInt64(var A: Decimal128; const B: Int64);
procedure Decimal128InitDecimal64(var A: Decimal128; const B: Decimal64);
procedure Decimal128InitDecimal128(var A: Decimal128; const B: Decimal128);
procedure Decimal128InitFloat(var A: Decimal128; const B: Double);

function  Decimal128ToWord8(const A: Decimal128): Byte;
function  Decimal128ToWord16(const A: Decimal128): Word;
function  Decimal128ToWord32(const A: Decimal128): Word32;
function  Decimal128ToWord64(const A: Decimal128): Word64Rec;
function  Decimal128ToInt32(const A: Decimal128): Int32;
function  Decimal128ToInt64(const A: Decimal128): Int64;
function  Decimal128ToFloat(const A: Decimal128): Double;

function  Decimal128Trunc(const A: Decimal128): Word64Rec;
function  Decimal128Round(const A: Decimal128): Word64Rec;
function  Decimal128FracWord(const A: Decimal128): Word64Rec;

function  Decimal128EqualsWord8(const A: Decimal128; const B: Byte): Boolean;
function  Decimal128EqualsWord16(const A: Decimal128; const B: Word): Boolean;
function  Decimal128EqualsWord32(const A: Decimal128; const B: Word32): Boolean;
function  Decimal128EqualsWord64(const A: Decimal128; const B: Word64Rec): Boolean;
function  Decimal128EqualsInt32(const A: Decimal128; const B: Int32): Boolean;
function  Decimal128EqualsInt64(const A: Decimal128; const B: Int64): Boolean;
function  Decimal128EqualsDecimal128(const A: Decimal128; const B: Decimal128): Boolean;
function  Decimal128EqualsFloat(const A: Decimal128; const B: Double): Boolean;

function  Decimal128CompareWord8(const A: Decimal128; const B: Byte): Integer;
function  Decimal128CompareWord16(const A: Decimal128; const B: Word): Integer;
function  Decimal128CompareWord32(const A: Decimal128; const B: Word32): Integer;
function  Decimal128CompareWord64(const A: Decimal128; const B: Word64Rec): Integer;
function  Decimal128CompareDecimal128(const A: Decimal128; const B: Decimal128): Integer;
function  Decimal128CompareFloat(const A: Decimal128; const B: Double): Integer;

procedure Decimal128AddWord8(var A: Decimal128; const B: Byte);
procedure Decimal128AddWord16(var A: Decimal128; const B: Word);
procedure Decimal128AddWord32(var A: Decimal128; const B: Word32);
procedure Decimal128AddWord64(var A: Decimal128; const B: Word64Rec);
procedure Decimal128AddDecimal128(var A: Decimal128; const B: Decimal128);

procedure Decimal128SubtractWord8(var A: Decimal128; const B: Byte);
procedure Decimal128SubtractWord16(var A: Decimal128; const B: Word);
procedure Decimal128SubtractWord32(var A: Decimal128; const B: Word32);
procedure Decimal128SubtractWord64(var A: Decimal128; const B: Word64Rec);
procedure Decimal128SubtractDecimal128(var A: Decimal128; const B: Decimal128);

procedure Decimal128MultiplyWord8(var A: Decimal128; const B: Byte);
procedure Decimal128MultiplyWord16(var A: Decimal128; const B: Word);
procedure Decimal128MultiplyWord32(var A: Decimal128; const B: Word32);
procedure Decimal128MultiplyWord64(var A: Decimal128; const B: Word64Rec);
procedure Decimal128MultiplyDecimal128(var A: Decimal128; const B: Decimal128);

procedure Decimal128Sqr(var A: Decimal128);

procedure Decimal128DivideWord8(var A: Decimal128; const B: Byte);
procedure Decimal128DivideWord16(var A: Decimal128; const B: Word);
procedure Decimal128DivideWord32(var A: Decimal128; const B: Word32);
procedure Decimal128DivideDecimal128(var A: Decimal128; const B: Decimal128);

function  Decimal128ToStr(const A: Decimal128): String;
function  Decimal128ToStrB(const A: Decimal128): RawByteString;
function  Decimal128ToStrU(const A: Decimal128): UnicodeString;

function  TryStrToDecimal128(const A: String; out B: Decimal128): TDecimalConvertErrorType;
function  TryStrToDecimal128B(const A: RawByteString; out B: Decimal128): TDecimalConvertErrorType;

function  StrToDecimal128(const A: String): Decimal128;
function  StrToDecimal128B(const A: RawByteString): Decimal128;



{                                                                              }
{ HugeDecimal                                                                  }
{                                                                              }
procedure HugeDecimalInit(out A: HugeDecimal);
procedure HugeDecimalFinalise(var A: HugeDecimal);

procedure HugeDecimalInitZero(out A: HugeDecimal);
procedure HugeDecimalInitOne(out A: HugeDecimal);
procedure HugeDecimalInitWord8(out A: HugeDecimal; const B: Byte);
procedure HugeDecimalInitWord32(out A: HugeDecimal; const B: Word32);
procedure HugeDecimalInitWord64(out A: HugeDecimal; const B: Word64Rec);
procedure HugeDecimalInitWord128(out A: HugeDecimal; const B: Word128);
procedure HugeDecimalInitDecimal32(out A: HugeDecimal; const B: Decimal32);
procedure HugeDecimalInitDecimal64(out A: HugeDecimal; const B: Decimal64);
procedure HugeDecimalInitDecimal128(out A: HugeDecimal; const B: Decimal128);
procedure HugeDecimalInitHugeDecimal(out A: HugeDecimal; const B: HugeDecimal);

procedure HugeDecimalAssignZero(var A: HugeDecimal);
procedure HugeDecimalAssignOne(var A: HugeDecimal);
procedure HugeDecimalAssignWord8(var A: HugeDecimal; const B: Byte);
procedure HugeDecimalAssignWord32(var A: HugeDecimal; const B: Word32);
procedure HugeDecimalAssignWord64(var A: HugeDecimal; const B: Word64Rec);
procedure HugeDecimalAssignWord128(var A: HugeDecimal; const B: Word128);
procedure HugeDecimalAssignDecimal32(var A: HugeDecimal; const B: Decimal32);
procedure HugeDecimalAssignDecimal64(var A: HugeDecimal; const B: Decimal64);
procedure HugeDecimalAssignDecimal128(var A: HugeDecimal; const B: Decimal128);
procedure HugeDecimalAssignHugeDecimal(var A: HugeDecimal; const B: HugeDecimal);

function  HugeDecimalIsZero(const A: HugeDecimal): Boolean; {$IFDEF UseInline}inline;{$ENDIF}
function  HugeDecimalIsOne(const A: HugeDecimal): Boolean;
function  HugeDecimalIsOdd(const A: HugeDecimal): Boolean;
function  HugeDecimalIsEven(const A: HugeDecimal): Boolean;
function  HugeDecimalIsInteger(const A: HugeDecimal): Boolean;
function  HugeDecimalIsLessThanOne(const A: HugeDecimal): Boolean;
function  HugeDecimalIsOneOrGreater(const A: HugeDecimal): Boolean;
function  HugeDecimalIsWord8Range(const A: HugeDecimal): Boolean;

function  HugeDecimalDigits(const A: HugeDecimal): Integer;
function  HugeDecimalIntegerDigits(const A: HugeDecimal): Integer;
function  HugeDecimalDecimalDigits(const A: HugeDecimal): Integer;

function  HugeDecimalGetDigit(const A: HugeDecimal; const DigitIdx: Integer): Byte;
procedure HugeDecimalSetDigit(var A: HugeDecimal; const DigitIdx: Integer; const DigitValue: Byte);

function  HugeDecimalToWord8(const A: HugeDecimal): Byte;
function  HugeDecimalToWord32(const A: HugeDecimal): Word32;
function  HugeDecimalToWord64(const A: HugeDecimal): Word64Rec;
function  HugeDecimalToWord128(const A: HugeDecimal): Word128;
function  HugeDecimalToDecimal32(const A: HugeDecimal): Decimal32;
function  HugeDecimalToDecimal64(const A: HugeDecimal): Decimal64;
function  HugeDecimalToDecimal128(const A: HugeDecimal): Decimal128;

procedure HugeDecimalMul10(var A: HugeDecimal);
procedure HugeDecimalDiv10(var A: HugeDecimal);

procedure HugeDecimalInc(var A: HugeDecimal; const N: Byte = 1);
procedure HugeDecimalDec(var A: HugeDecimal; const N: Byte = 1);

procedure HugeDecimalTrunc(var A: HugeDecimal);
function  HugeDecimalFracCompareHalf(var A: HugeDecimal): Integer;
procedure HugeDecimalRound(var A: HugeDecimal);

function  HugeDecimalEqualsWord8(const A: HugeDecimal; const B: Byte): Boolean;
function  HugeDecimalEqualsHugeDecimal(const A, B: HugeDecimal): Boolean;

function  HugeDecimalCompareWord8(const A: HugeDecimal; const B: Byte): Integer;
function  HugeDecimalCompareHugeDecimal(const A, B: HugeDecimal): Integer;

procedure HugeDecimalAddHugeDecimal(var A: HugeDecimal; const B: HugeDecimal);

procedure HugeDecimalSubtractHugeDecimal(var A: HugeDecimal; const B: HugeDecimal);

function  TryStrToHugeDecimal(const S: String; var R: HugeDecimal): TDecimalConvertErrorType;
procedure StrToHugeDecimal(const S: String; var R: HugeDecimal);

function  HugeDecimalToStr(const A: HugeDecimal): String;



{                                                                              }
{ SDecimal32                                                                   }
{                                                                              }
const
  SDecimal32MinInt       = -99999;
  SDecimal32MaxInt       = 99999;
  SDecimal32MinFloatLim  = -Decimal32MaxFloatLim;
  SDecimal32MinFloatLimD : Double = SDecimal32MinFloatLim;
  SDecimal32MaxFloatLim  = Decimal32MaxFloatLim;
  SDecimal32MaxFloatLimD : Double = SDecimal32MaxFloatLim;

procedure SDecimal32InitZero(var A: SDecimal32);
procedure SDecimal32InitOne(var A: SDecimal32);
procedure SDecimal32InitMinusOne(var A: SDecimal32);
procedure SDecimal32InitMin(var A: SDecimal32);
procedure SDecimal32InitMax(var A: SDecimal32);

function  SDecimal32IsZero(const A: SDecimal32): Boolean; {$IFDEF UseInline}inline;{$ENDIF}
function  SDecimal32IsOne(const A: SDecimal32): Boolean;
function  SDecimal32IsMinusOne(const A: SDecimal32): Boolean;
function  SDecimal32IsMinimum(const A: SDecimal32): Boolean;
function  SDecimal32IsMaximum(const A: SDecimal32): Boolean;
function  SDecimal32IsOverflow(const A: SDecimal32): Boolean;

function  Word32IsSDecimal32Range(const A: Word32): Boolean;
function  Int32IsSDecimal32Range(const A: Int32): Boolean;
function  FloatIsSDecimal32Range(const A: Double): Boolean;

function  SDecimal32Sign(const A: SDecimal32): Integer; {$IFDEF UseInline}inline;{$ENDIF}
procedure SDecimal32Negate(var A: SDecimal32);
procedure SDecimal32AbsInPlace(var A: SDecimal32);

procedure SDecimal32InitWord8(var A: SDecimal32; const B: Byte);
procedure SDecimal32InitWord16(var A: SDecimal32; const B: Word);
procedure SDecimal32InitWord32(var A: SDecimal32; const B: Word32);
procedure SDecimal32InitInt32(var A: SDecimal32; const B: Int32);
procedure SDecimal32InitSDecimal32(var A: SDecimal32; const B: SDecimal32);
procedure SDecimal32InitFloat(var A: SDecimal32; const B: Double);

function  SDecimal32ToWord8(const A: SDecimal32): Byte;
function  SDecimal32ToWord16(const A: SDecimal32): Word;
function  SDecimal32ToWord32(const A: SDecimal32): Word32;
function  SDecimal32ToInt32(const A: SDecimal32): Int32;
function  SDecimal32ToFloat(const A: SDecimal32): Double;

function  SDecimal32Trunc(const A: SDecimal32): Int32;
function  SDecimal32Round(const A: SDecimal32): Int32;
function  SDecimal32FracWord(const A: SDecimal32): Word;

function  SDecimal32EqualsWord8(const A: SDecimal32; const B: Byte): Boolean;
function  SDecimal32EqualsWord16(const A: SDecimal32; const B: Word): Boolean;
function  SDecimal32EqualsWord32(const A: SDecimal32; const B: Word32): Boolean;
function  SDecimal32EqualsInt32(const A: SDecimal32; const B: Int32): Boolean;
function  SDecimal32EqualsSDecimal32(const A: SDecimal32; const B: SDecimal32): Boolean;
function  SDecimal32EqualsFloat(const A: SDecimal32; const B: Double): Boolean;

function  SDecimal32CompareWord8(const A: SDecimal32; const B: Byte): Integer;
function  SDecimal32CompareWord16(const A: SDecimal32; const B: Word): Integer;
function  SDecimal32CompareWord32(const A: SDecimal32; const B: Word32): Integer;
function  SDecimal32CompareInt32(const A: SDecimal32; const B: Int32): Integer;
function  SDecimal32CompareSDecimal32(const A: SDecimal32; const B: SDecimal32): Integer;
function  SDecimal32CompareFloat(const A: SDecimal32; const B: Double): Integer;

procedure SDecimal32AddWord8(var A: SDecimal32; const B: Byte);
procedure SDecimal32AddWord16(var A: SDecimal32; const B: Word);
procedure SDecimal32AddWord32(var A: SDecimal32; const B: Word32);
procedure SDecimal32AddSDecimal32(var A: SDecimal32; const B: SDecimal32);

procedure SDecimal32SubtractWord8(var A: SDecimal32; const B: Byte);
procedure SDecimal32SubtractWord16(var A: SDecimal32; const B: Word);
procedure SDecimal32SubtractWord32(var A: SDecimal32; const B: Word32);
procedure SDecimal32SubtractSDecimal32(var A: SDecimal32; const B: SDecimal32);

procedure SDecimal32MultiplyWord8(var A: SDecimal32; const B: Byte);
procedure SDecimal32MultiplyWord16(var A: SDecimal32; const B: Word);
procedure SDecimal32MultiplyWord32(var A: SDecimal32; const B: Word32);
procedure SDecimal32MultiplySDecimal32(var A: SDecimal32; const B: SDecimal32);

procedure SDecimal32DivideWord8(var A: SDecimal32; const B: Byte);
procedure SDecimal32DivideWord16(var A: SDecimal32; const B: Word);
procedure SDecimal32DivideWord32(var A: SDecimal32; const B: Word32);
procedure SDecimal32DivideSDecimal32(var A: SDecimal32; const B: SDecimal32);

function  SDecimal32ToStr(const A: SDecimal32): String;
function  SDecimal32ToStrB(const A: SDecimal32): RawByteString;
function  SDecimal32ToStrU(const A: SDecimal32): UnicodeString;

function  TryStrToSDecimal32(const A: String; out B: SDecimal32): TDecimalConvertErrorType;
function  TryStrToSDecimal32B(const A: RawByteString; out B: SDecimal32): TDecimalConvertErrorType;

function  StrToSDecimal32(const A: String): SDecimal32;
function  StrToSDecimal32B(const A: RawByteString): SDecimal32;



{                                                                              }
{ SDecimal64                                                                   }
{                                                                              }
const
  SDecimal64MinInt       = -9999999999;
  SDecimal64MaxInt       = 9999999999;
  {$IFDEF DELPHI7_DOWN}
  SDecimal64MinFloatLim  : Double = -999999999999999.9999999995;
  SDecimal64MinFloatLimD : Double = -999999999999999.9999999995;
  SDecimal64MaxFloatLim  : Double = 999999999999999.9999999995;
  SDecimal64MaxFloatLimD : Double = 999999999999999.9999999995;
  {$ELSE}
  SDecimal64MinFloatLim  = -Decimal64MaxFloatLim;
  SDecimal64MinFloatLimD : Double = SDecimal64MinFloatLim;
  SDecimal64MaxFloatLim  = Decimal64MaxFloatLim;
  SDecimal64MaxFloatLimD : Double = SDecimal64MaxFloatLim;
  {$ENDIF}

procedure SDecimal64InitZero(var A: SDecimal64);
procedure SDecimal64InitOne(var A: SDecimal64);
procedure SDecimal64InitMinusOne(var A: SDecimal64);
procedure SDecimal64InitMin(var A: SDecimal64);
procedure SDecimal64InitMax(var A: SDecimal64);

function  SDecimal64IsZero(const A: SDecimal64): Boolean; {$IFDEF UseInline}inline;{$ENDIF}
function  SDecimal64IsOne(const A: SDecimal64): Boolean;
function  SDecimal64IsMinusOne(const A: SDecimal64): Boolean;
function  SDecimal64IsMinimum(const A: SDecimal64): Boolean;
function  SDecimal64IsMaximum(const A: SDecimal64): Boolean;
function  SDecimal64IsOverflow(const A: SDecimal64): Boolean;

function  Word64IsSDecimal64Range(const A: Word64Rec): Boolean;
function  Int64IsSDecimal64Range(const A: Int64): Boolean;
function  FloatIsSDecimal64Range(const A: Double): Boolean;

function  SDecimal64Sign(const A: SDecimal64): Integer; {$IFDEF UseInline}inline;{$ENDIF}
procedure SDecimal64Negate(var A: SDecimal64);
procedure SDecimal64AbsInPlace(var A: SDecimal64);

procedure SDecimal64InitWord8(var A: SDecimal64; const B: Byte);
procedure SDecimal64InitWord16(var A: SDecimal64; const B: Word);
procedure SDecimal64InitWord32(var A: SDecimal64; const B: Word32);
procedure SDecimal64InitWord64(var A: SDecimal64; const B: Word64Rec);
procedure SDecimal64InitInt32(var A: SDecimal64; const B: Int32);
procedure SDecimal64InitInt64(var A: SDecimal64; const B: Int64);
procedure SDecimal64InitSDecimal64(var A: SDecimal64; const B: SDecimal64);
procedure SDecimal64InitFloat(var A: SDecimal64; const B: Double);

function  SDecimal64ToWord8(const A: SDecimal64): Byte;
function  SDecimal64ToWord16(const A: SDecimal64): Word;
function  SDecimal64ToWord32(const A: SDecimal64): Word32;
function  SDecimal64ToWord64(const A: SDecimal64): Word64Rec;
function  SDecimal64ToInt32(const A: SDecimal64): Int32;
function  SDecimal64ToInt64(const A: SDecimal64): Int64;
function  SDecimal64ToSDecimal32(const A: SDecimal64): SDecimal32; // TODO
function  SDecimal64ToFloat(const A: SDecimal64): Double;

function  SDecimal64Trunc(const A: SDecimal64): Int64;
function  SDecimal64Round(const A: SDecimal64): Int64;
function  SDecimal64FracWord(const A: SDecimal64): Word32;

function  SDecimal64EqualsWord8(const A: SDecimal64; const B: Byte): Boolean;
function  SDecimal64EqualsWord16(const A: SDecimal64; const B: Word): Boolean;
function  SDecimal64EqualsWord32(const A: SDecimal64; const B: Word32): Boolean;
function  SDecimal64EqualsInt32(const A: SDecimal64; const B: Int32): Boolean;
function  SDecimal64EqualsInt64(const A: SDecimal64; const B: Int64): Boolean;
function  SDecimal64EqualsSDecimal64(const A: SDecimal64; const B: SDecimal64): Boolean;
function  SDecimal64EqualsFloat(const A: SDecimal64; const B: Double): Boolean;

function  SDecimal64CompareWord8(const A: SDecimal64; const B: Byte): Integer;
function  SDecimal64CompareWord16(const A: SDecimal64; const B: Word): Integer;
function  SDecimal64CompareWord32(const A: SDecimal64; const B: Word32): Integer;
function  SDecimal64CompareInt32(const A: SDecimal64; const B: Int32): Integer;
function  SDecimal64CompareInt64(const A: SDecimal64; const B: Int64): Integer;
function  SDecimal64CompareSDecimal64(const A: SDecimal64; const B: SDecimal64): Integer;
function  SDecimal64CompareFloat(const A: SDecimal64; const B: Double): Integer;

procedure SDecimal64AddWord8(var A: SDecimal64; const B: Byte);
procedure SDecimal64AddWord16(var A: SDecimal64; const B: Word);
procedure SDecimal64AddWord32(var A: SDecimal64; const B: Word32);
procedure SDecimal64AddSDecimal64(var A: SDecimal64; const B: SDecimal64);

procedure SDecimal64SubtractWord8(var A: SDecimal64; const B: Byte);
procedure SDecimal64SubtractWord16(var A: SDecimal64; const B: Word);
procedure SDecimal64SubtractWord32(var A: SDecimal64; const B: Word32);
procedure SDecimal64SubtractSDecimal64(var A: SDecimal64; const B: SDecimal64);

procedure SDecimal64MultiplyWord8(var A: SDecimal64; const B: Byte);
procedure SDecimal64MultiplyWord16(var A: SDecimal64; const B: Word);
procedure SDecimal64MultiplyWord32(var A: SDecimal64; const B: Word32);
procedure SDecimal64MultiplySDecimal64(var A: SDecimal64; const B: SDecimal64);

procedure SDecimal64DivideWord8(var A: SDecimal64; const B: Byte);
procedure SDecimal64DivideWord16(var A: SDecimal64; const B: Word);
procedure SDecimal64DivideWord32(var A: SDecimal64; const B: Word32);
procedure SDecimal64DivideSDecimal64(var A: SDecimal64; const B: SDecimal64);

function  SDecimal64ToStr(const A: SDecimal64): String;
function  SDecimal64ToStrB(const A: SDecimal64): RawByteString;
function  SDecimal64ToStrU(const A: SDecimal64): UnicodeString;

function  TryStrToSDecimal64(const A: String; out B: SDecimal64): TDecimalConvertErrorType;
function  TryStrToSDecimal64B(const A: RawByteString; out B: SDecimal64): TDecimalConvertErrorType;

function  StrToSDecimal64(const A: String): SDecimal64;
function  StrToSDecimal64B(const A: RawByteString): SDecimal64;



{                                                                              }
{ SDecimal128                                                                  }
{                                                                              }
const
  SDecimal128MinFloatLim  = -Decimal128MaxFloatLim;
  SDecimal128MinFloatLimD : Double = SDecimal128MinFloatLim;
  SDecimal128MaxFloatLim  = Decimal128MaxFloatLim;
  SDecimal128MaxFloatLimD : Double = SDecimal128MaxFloatLim;

procedure SDecimal128InitZero(var A: SDecimal128);
procedure SDecimal128InitOne(var A: SDecimal128);
procedure SDecimal128InitMax(var A: SDecimal128);

function  SDecimal128IsZero(const A: SDecimal128): Boolean; {$IFDEF UseInline}inline;{$ENDIF}
function  SDecimal128IsOne(const A: SDecimal128): Boolean;
function  SDecimal128IsMaximum(const A: SDecimal128): Boolean;
function  SDecimal128IsOverflow(const A: SDecimal128): Boolean;

function  Word64IsSDecimal128Range(const A: Word64Rec): Boolean;
function  Word128IsSDecimal128Range(const A: Word128): Boolean;
function  Int128IsSDecimal128Range(const A: Int128): Boolean;
function  FloatIsSDecimal128Range(const A: Double): Boolean;

function  SDecimal128Sign(const A: SDecimal128): Integer;
procedure SDecimal128Negate(var A: SDecimal128);
procedure SDecimal128AbsInPlace(var A: SDecimal128);

procedure SDecimal128InitWord8(var A: SDecimal128; const B: Byte);
procedure SDecimal128InitWord16(var A: SDecimal128; const B: Word);
procedure SDecimal128InitWord32(var A: SDecimal128; const B: Word32);
procedure SDecimal128InitWord64(var A: SDecimal128; const B: Word64Rec);
procedure SDecimal128InitInt32(var A: SDecimal128; const B: Int32);
procedure SDecimal128InitInt64(var A: SDecimal128; const B: Int64);
procedure SDecimal128InitSDecimal128(var A: SDecimal128; const B: SDecimal128);
procedure SDecimal128InitFloat(var A: SDecimal128; const B: Double);

function  SDecimal128ToWord8(const A: SDecimal128): Byte;
function  SDecimal128ToWord16(const A: SDecimal128): Word;
function  SDecimal128ToWord32(const A: SDecimal128): Word32;
function  SDecimal128ToWord64(const A: SDecimal128): Word64Rec;
function  SDecimal128ToInt32(const A: SDecimal128): Int32;
function  SDecimal128ToInt64(const A: SDecimal128): Int64;
function  SDecimal128ToFloat(const A: SDecimal128): Double;

function  SDecimal128Trunc(const A: SDecimal128): Int128;
function  SDecimal128Round(const A: SDecimal128): Int128;
function  SDecimal128FracWord(const A: SDecimal128): Word64Rec;

function  SDecimal128EqualsWord8(const A: SDecimal128; const B: Byte): Boolean;
function  SDecimal128EqualsWord16(const A: SDecimal128; const B: Word): Boolean;
function  SDecimal128EqualsWord32(const A: SDecimal128; const B: Word32): Boolean;
function  SDecimal128EqualsWord64(const A: SDecimal128; const B: Word64Rec): Boolean;
function  SDecimal128EqualsSDecimal128(const A: SDecimal128; const B: SDecimal128): Boolean;
function  SDecimal128EqualsFloat(const A: SDecimal128; const B: Double): Boolean;

function  SDecimal128CompareWord8(const A: SDecimal128; const B: Byte): Integer;
function  SDecimal128CompareWord16(const A: SDecimal128; const B: Word): Integer;
function  SDecimal128CompareWord32(const A: SDecimal128; const B: Word32): Integer;
function  SDecimal128CompareWord64(const A: SDecimal128; const B: Word64Rec): Integer;
function  SDecimal128CompareInt32(const A: SDecimal128; const B: Int32): Integer;
function  SDecimal128CompareInt64(const A: SDecimal128; const B: Int64): Integer;
function  SDecimal128CompareSDecimal128(const A: SDecimal128; const B: SDecimal128): Integer;
function  SDecimal128CompareFloat(const A: SDecimal128; const B: Double): Integer;

procedure SDecimal128AddWord8(var A: SDecimal128; const B: Byte);
procedure SDecimal128AddWord16(var A: SDecimal128; const B: Word);
procedure SDecimal128AddWord32(var A: SDecimal128; const B: Word32);
procedure SDecimal128AddWord64(var A: SDecimal128; const B: Word64Rec);
procedure SDecimal128AddSDecimal128(var A: SDecimal128; const B: SDecimal128);

procedure SDecimal128SubtractWord8(var A: SDecimal128; const B: Byte);
procedure SDecimal128SubtractWord16(var A: SDecimal128; const B: Word);
procedure SDecimal128SubtractWord32(var A: SDecimal128; const B: Word32);
procedure SDecimal128SubtractWord64(var A: SDecimal128; const B: Word64Rec);
procedure SDecimal128SubtractSDecimal128(var A: SDecimal128; const B: SDecimal128);

procedure SDecimal128MultiplyWord8(var A: SDecimal128; const B: Byte);
procedure SDecimal128MultiplyWord16(var A: SDecimal128; const B: Word);
procedure SDecimal128MultiplyWord32(var A: SDecimal128; const B: Word32);
procedure SDecimal128MultiplyWord64(var A: SDecimal128; const B: Word64Rec);
procedure SDecimal128MultiplySDecimal128(var A: SDecimal128; const B: SDecimal128);

procedure SDecimal128DivideWord8(var A: SDecimal128; const B: Byte);
procedure SDecimal128DivideWord16(var A: SDecimal128; const B: Word);
procedure SDecimal128DivideWord32(var A: SDecimal128; const B: Word32);
procedure SDecimal128DivideWord64(var A: SDecimal128; const B: Word64Rec);
procedure SDecimal128DivideSDecimal128(var A: SDecimal128; const B: SDecimal128);

function  SDecimal128ToStr(const A: SDecimal128): String;
function  SDecimal128ToStrB(const A: SDecimal128): RawByteString;
function  SDecimal128ToStrU(const A: SDecimal128): UnicodeString;

function  TryStrToSDecimal128(const A: String; out B: SDecimal128): TDecimalConvertErrorType;
function  TryStrToSDecimal128B(const A: RawByteString; out B: SDecimal128): TDecimalConvertErrorType;

function  StrToSDecimal128(const A: String): SDecimal128;
function  StrToSDecimal128B(const A: RawByteString): SDecimal128;



{                                                                              }
{ SHugeDecimal                                                                 }
{                                                                              }
procedure SHugeDecimalInit(out A: SHugeDecimal);
procedure SHugeDecimalInitZero(out A: SHugeDecimal);
procedure SHugeDecimalInitOne(out A: SHugeDecimal);
procedure SHugeDecimalInitMinusOne(out A: SHugeDecimal);

procedure SHugeDecimalAssignZero(var A: SHugeDecimal);
procedure SHugeDecimalAssignOne(var A: SHugeDecimal);
procedure SHugeDecimalAssignMinusOne(var A: SHugeDecimal);
procedure SHugeDecimalAssignWord8(var A: SHugeDecimal; const B: Byte);
procedure SHugeDecimalAssignWord32(var A: SHugeDecimal; const B: Word32);
procedure SHugeDecimalAssignWord64(var A: SHugeDecimal; const B: Word64Rec);
procedure SHugeDecimalAssignWord128(var A: SHugeDecimal; const B: Word128);
procedure SHugeDecimalAssignInt8(var A: SHugeDecimal; const B: ShortInt);
procedure SHugeDecimalAssignInt32(var A: SHugeDecimal; const B: Int32);
procedure SHugeDecimalAssignInt64(var A: SHugeDecimal; const B: Int64);
procedure SHugeDecimalAssignDecimal32(var A: SHugeDecimal; const B: Decimal32);
procedure SHugeDecimalAssignDecimal64(var A: SHugeDecimal; const B: Decimal64);
procedure SHugeDecimalAssignDecimal128(var A: SHugeDecimal; const B: Decimal128);
procedure SHugeDecimalAssignHugeDecimal(var A: SHugeDecimal; const B: HugeDecimal);
procedure SHugeDecimalAssignSHugeDecimal(var A: SHugeDecimal; const B: SHugeDecimal);

function  SHugeDecimalIsZero(const A: SHugeDecimal): Boolean; {$IFDEF UseInline}inline;{$ENDIF}
function  SHugeDecimalIsOne(const A: SHugeDecimal): Boolean;
function  SHugeDecimalIsMinusOne(const A: SHugeDecimal): Boolean;

function  SHugeDecimalSign(const A: SHugeDecimal): Integer; {$IFDEF UseInline}inline;{$ENDIF}
procedure SHugeDecimalNegate(var A: SHugeDecimal);
procedure SHugeDecimalAbsInPlace(var A: SHugeDecimal);

function  SHugeDecimalToWord8(const A: SHugeDecimal): Byte;
function  SHugeDecimalToWord32(const A: SHugeDecimal): Word32;
function  SHugeDecimalToWord64(const A: SHugeDecimal): Word64Rec;
function  SHugeDecimalToWord128(const A: SHugeDecimal): Word128;
function  SHugeDecimalToInt8(const A: SHugeDecimal): ShortInt;
function  SHugeDecimalToInt32(const A: SHugeDecimal): Int32;
function  SHugeDecimalToInt64(const A: SHugeDecimal): Int64;
function  SHugeDecimalToDecimal32(const A: SHugeDecimal): Decimal32;
function  SHugeDecimalToDecimal64(const A: SHugeDecimal): Decimal64;
function  SHugeDecimalToDecimal128(const A: SHugeDecimal): Decimal128;

procedure SHugeDecimalTrunc(var A: SHugeDecimal);
function  SHugeDecimalFracCompareHalf(var A: SHugeDecimal): Integer;
procedure SHugeDecimalRound(var A: SHugeDecimal);

function  SHugeDecimalEqualsWord8(const A: SHugeDecimal; const B: Byte): Boolean;
function  SHugeDecimalEqualsHugeDecimal(const A: SHugeDecimal; const B: HugeDecimal): Boolean;
function  SHugeDecimalEqualsSHugeDecimal(const A, B: SHugeDecimal): Boolean;

function  SHugeDecimalCompareWord8(const A: SHugeDecimal; const B: Byte): Integer;
function  SHugeDecimalCompareHugeDecimal(const A: SHugeDecimal; const B: HugeDecimal): Integer;
function  SHugeDecimalCompareSHugeDecimal(const A, B: SHugeDecimal): Integer;

procedure SHugeDecimalAddHugeDecimal(var A: SHugeDecimal; const B: HugeDecimal);
procedure SHugeDecimalAddSHugeDecimal(var A: SHugeDecimal; const B: SHugeDecimal);

procedure SHugeDecimalSubtractSHugeDecimal(var A: SHugeDecimal; const B: SHugeDecimal);

function  TryStrToSHugeDecimal(const S: String; var R: SHugeDecimal): TDecimalConvertErrorType;
procedure StrToSHugeDecimal(const S: String; var R: SHugeDecimal);

function  SHugeDecimalToStr(const A: SHugeDecimal): String;



{                                                                              }
{ Test cases                                                                   }
{                                                                              }
{$IFDEF DECIMAL_TEST}
procedure Test;
{$ENDIF}



implementation

uses
  { System }
  Math,

  { Fundamentals }
  flcUtils,
  flcStrings;



{                                                                              }
{ Errors                                                                       }
{                                                                              }
const
  SOverflowError    = 'Overflow error';
  SConvertError     = 'Convert error';
  SDivisionByZero   = 'Division by zero';
  SIndexOutOfRange  = 'Index out of range';
  SInvalidParameter = 'Invalid parameter';



{                                                                              }
{ Decimal32                                                                    }
{                                                                              }
procedure Decimal32InitZero(var A: Decimal32);
begin
  A.Value32 := 0;
end;

procedure Decimal32InitOne(var A: Decimal32);
begin
  A.Value32 := Decimal32Scale;
end;

procedure Decimal32InitMax(var A: Decimal32);
begin
  A.Value32 := Decimal32MaxValue;
end;

function Decimal32IsZero(const A: Decimal32): Boolean;
begin
  Result := A.Value32 = 0;
end;

function Decimal32IsOne(const A: Decimal32): Boolean;
begin
  Result := A.Value32 = Decimal32Scale;
end;

function Decimal32IsMaximum(const A: Decimal32): Boolean;
begin
  Result := A.Value32 = Decimal32MaxValue;
end;

function Decimal32IsOverflow(const A: Decimal32): Boolean;
begin
  Result := A.Value32 > Decimal32MaxValue;
end;

function Word32IsDecimal32Range(const A: Word32): Boolean;
begin
  Result := A <= Decimal32MaxInt;
end;

function Int16IsDecimal32Range(const A: SmallInt): Boolean;
begin
  Result := A >= 0;
end;

function Int32IsDecimal32Range(const A: Int32): Boolean;
begin
  Result := (A >= 0) and (A <= Decimal32MaxInt);
end;

function FloatIsDecimal32Range(const A: Double): Boolean;
begin
  Result :=
    (A >= Decimal32MinFloatD) and
    (A < Decimal32MaxFloatLimD);
end;

procedure Decimal32InitWord8(var A: Decimal32; const B: Byte);
begin
  A.Value32 := B * Decimal32Scale;
end;

procedure Decimal32InitWord16(var A: Decimal32; const B: Word);
begin
  A.Value32 := B * Decimal32Scale;
end;

procedure Decimal32InitWord32(var A: Decimal32; const B: Word32);
begin
  if B > Decimal32MaxInt then
    raise EDecimalError.Create(SOverflowError);
  A.Value32 := B * Decimal32Scale;
end;

procedure Decimal32InitInt32(var A: Decimal32; const B: Int32);
begin
  if (B < 0) or (B > Decimal32MaxInt) then
    raise EDecimalError.Create(SOverflowError);
  A.Value32 := B * Decimal32Scale;
end;

procedure Decimal32InitDecimal32(var A: Decimal32; const B: Decimal32);
begin
  A.Value32 := B.Value32;
end;

procedure Decimal32InitFloat(var A: Decimal32; const B: Double);
var D : Double;
    I : Int64;
begin
  if not FloatIsDecimal32Range(B) then
    raise EDecimalError.Create(SOverflowError);
  D := B * Decimal32Scale;
  I := Round(D);
  Assert(I >= 0);
  Assert(I <= Decimal32MaxValue);
  A.Value32 := I;
end;

function Decimal32ToWord8(const A: Decimal32): Byte;
var I : Word32;
begin
  if A.Value32 mod Decimal32Scale <> 0 then
    raise EDecimalError.Create(SConvertError);
  I := A.Value32 div Decimal32Scale;
  if not Word32IsWord8Range(I) then
    raise EDecimalError.Create(SOverflowError);
  Result := I;
end;

function Decimal32ToWord16(const A: Decimal32): Word;
var I : Word32;
begin
  if A.Value32 mod Decimal32Scale <> 0 then
    raise EDecimalError.Create(SConvertError);
  I := A.Value32 div Decimal32Scale;
  if not Word32IsWord16Range(I) then
    raise EDecimalError.Create(SOverflowError);
  Result := I;
end;

function Decimal32ToWord32(const A: Decimal32): Word32;
begin
  if A.Value32 mod Decimal32Scale <> 0 then
    raise EDecimalError.Create(SConvertError);
  Result := A.Value32 div Decimal32Scale;
end;

function Decimal32ToInt32(const A: Decimal32): Int32;
begin
  if A.Value32 mod Decimal32Scale <> 0 then
    raise EDecimalError.Create(SConvertError);
  Result := A.Value32 div Decimal32Scale;
end;

function Decimal32ToFloat(const A: Decimal32): Double;
var D, E : Double;
begin
  D := A.Value32;
  E := Decimal32Scale;
  D := D / E;
  Result := D;
end;

function Decimal32Trunc(const A: Decimal32): Word32;
begin
  Result := A.Value32 div Decimal32Scale;
end;

function Decimal32Round(const A: Decimal32): Word32;
var F, R : Word32;
begin
  R := A.Value32 div Decimal32Scale;
  F := A.Value32 mod Decimal32Scale;
  if (F > Decimal32RoundTerm) or
     ((F = Decimal32RoundTerm) and Word32IsOdd(R)) then
    Inc(R);
  Result := R;
end;

function Decimal32FracWord(const A: Decimal32): Word;
begin
  Result := A.Value32 mod Decimal32Scale;
end;

function Decimal32EqualsWord8(const A: Decimal32; const B: Byte): Boolean;
begin
  Result := A.Value32 = B * Decimal32Scale;
end;

function Decimal32EqualsWord16(const A: Decimal32; const B: Word): Boolean;
begin
  Result := A.Value32 = B * Decimal32Scale;
end;

function Decimal32EqualsWord32(const A: Decimal32; const B: Word32): Boolean;
begin
  Result := Int64(A.Value32) = Int64(B) * Decimal32Scale;
end;

function Decimal32EqualsInt32(const A: Decimal32; const B: Int32): Boolean;
begin
  if B < 0 then
    Result := False
  else
    Result := Int64(A.Value32) = Int64(B) * Decimal32Scale;
end;

function Decimal32EqualsDecimal32(const A: Decimal32; const B: Decimal32): Boolean;
begin
  Result := A.Value32 = B.Value32;
end;

function Decimal32EqualsFloat(const A: Decimal32; const B: Double): Boolean;
var T : Decimal32;
begin
  if not FloatIsDecimal32Range(B) then
    Result := False
  else
    begin
      Decimal32InitFloat(T, B);
      Result := T.Value32 = A.Value32;
    end;
end;

function Decimal32CompareWord8(const A: Decimal32; const B: Byte): Integer;
var C : Word32;
begin
  C := B * Decimal32Scale;
  Result := Word32Compare(A.Value32, C);
end;

function Decimal32CompareWord16(const A: Decimal32; const B: Word): Integer;
var C : Word32;
begin
  C := B * Decimal32Scale;
  Result := Word32Compare(A.Value32, C);
end;

function Decimal32CompareWord32(const A: Decimal32; const B: Word32): Integer;
var C : Int64;
begin
  C := Int64(B) * Decimal32Scale;
  if A.Value32 > C then
    Result := 1
  else
  if A.Value32 < C then
    Result := -1
  else
    Result := 0;
end;

function Decimal32CompareInt32(const A: Decimal32; const B: Int32): Integer;
var C : Int64;
begin
  if B < 0 then
    Result := 1
  else
    begin
      C := Int64(B) * Decimal32Scale;
      if A.Value32 > C then
        Result := 1
      else
      if A.Value32 < C then
        Result := -1
      else
        Result := 0;
    end;
end;

function Decimal32CompareDecimal32(const A: Decimal32; const B: Decimal32): Integer;
begin
  Result := Word32Compare(A.Value32, B.Value32);
end;

function Decimal32CompareFloat(const A: Decimal32; const B: Double): Integer;
var T : Decimal32;
begin
  if B < Decimal32MinFloatD then
    Result := 1
  else
  if B >= Decimal32MaxFloatLimD then
    Result := -1
  else
    begin
      Assert(FloatIsDecimal32Range(B));
      Decimal32InitFloat(T, B);
      Result := Word32Compare(A.Value32, T.Value32);
    end;
end;

procedure Decimal32AddWord8(var A: Decimal32; const B: Byte);
var T : Word32;
begin
  T := A.Value32 + (B * Decimal32Scale);
  if T > Decimal32MaxValue then
    raise EDecimalError.Create(SOverflowError);
  A.Value32 := T;
end;

procedure Decimal32AddWord16(var A: Decimal32; const B: Word);
var T : Word32;
begin
  T := A.Value32 + (B * Decimal32Scale);
  if T > Decimal32MaxValue then
    raise EDecimalError.Create(SOverflowError);
  A.Value32 := T;
end;

procedure Decimal32AddWord32(var A: Decimal32; const B: Word32);
var T : Word32;
begin
  T := A.Value32 + (B * Decimal32Scale);
  if T > Decimal32MaxValue then
    raise EDecimalError.Create(SOverflowError);
  A.Value32 := T;
end;

procedure Decimal32AddDecimal32(var A: Decimal32; const B: Decimal32);
var T : Word32;
begin
  T := A.Value32 + B.Value32;
  if T > Decimal32MaxValue then
    raise EDecimalError.Create(SOverflowError);
  A.Value32 := T;
end;

procedure Decimal32SubtractWord8(var A: Decimal32; const B: Byte);
var T : Int64;
begin
  T := Int64(A.Value32) - (B * Decimal32Scale);
  if T < 0 then
    raise EDecimalError.Create(SOverflowError);
  A.Value32 := T;
end;

procedure Decimal32SubtractWord16(var A: Decimal32; const B: Word);
var T : Int64;
begin
  T := Int64(A.Value32) - (B * Decimal32Scale);
  if T < 0 then
    raise EDecimalError.Create(SOverflowError);
  A.Value32 := T;
end;

procedure Decimal32SubtractWord32(var A: Decimal32; const B: Word32);
var T : Int64;
begin
  T := Int64(A.Value32) - (B * Decimal32Scale);
  if T < 0 then
    raise EDecimalError.Create(SOverflowError);
  A.Value32 := T;
end;

procedure Decimal32SubtractDecimal32(var A: Decimal32; const B: Decimal32);
var T : Int64;
begin
  T := Int64(A.Value32) - B.Value32;
  if T < 0 then
    raise EDecimalError.Create(SOverflowError);
  A.Value32 := T;
end;

procedure Decimal32MultiplyWord8(var A: Decimal32; const B: Byte);
var T : Int64;
begin
  T := Int64(A.Value32) * B;
  if T > Decimal32MaxValue then
    raise EDecimalError.Create(SOverflowError);
  A.Value32 := T;
end;

procedure Decimal32MultiplyWord16(var A: Decimal32; const B: Word);
var T : Int64;
begin
  T := Int64(A.Value32) * B;
  if T > Decimal32MaxValue then
    raise EDecimalError.Create(SOverflowError);
  A.Value32 := T;
end;

procedure Decimal32MultiplyWord32(var A: Decimal32; const B: Word32);
var T : Int64;
begin
  T := Int64(A.Value32) * B;
  if T > Decimal32MaxValue then
    raise EDecimalError.Create(SOverflowError);
  A.Value32 := T;
end;

procedure Decimal32MultiplyDecimal32(var A: Decimal32; const B: Decimal32);
var T : Int64;
begin
  T := Int64(A.Value32) * B.Value32;
  T := (T + Decimal32RoundTerm) div Decimal32Scale;
  if T > Decimal32MaxValue then
    raise EDecimalError.Create(SOverflowError);
  A.Value32 := T;
end;

procedure Decimal32Sqr(var A: Decimal32);
begin
  Decimal32MultiplyDecimal32(A, A);
end;

procedure Decimal32DivideWord8(var A: Decimal32; const B: Byte);
begin
  A.Value32 := A.Value32 div B;
end;

procedure Decimal32DivideWord16(var A: Decimal32; const B: Word);
begin
  A.Value32 := A.Value32 div B;
end;

procedure Decimal32DivideWord32(var A: Decimal32; const B: Word32);
begin
  A.Value32 := A.Value32 div B;
end;

procedure Decimal32DivideDecimal32(var A: Decimal32; const B: Decimal32);
var T : Int64;
begin
  T := Int64(A.Value32) * Decimal32Scale;
  A.Value32 := T div B.Value32;
end;

function Decimal32ToStr(const A: Decimal32): String;
var
  S : String;
  T : Word64Rec;
  L : Integer;
begin
  if A.Value32 = 0 then
    begin
      Result := '0.0000';
      exit;
    end;
  Word64InitWord32(T, A.Value32);
  S := StrPadLeft(Word64ToStr(T), '0', Decimal32Precision + 1, False);
  L := Length(S);
  S :=
    CopyLeft(S, L - Decimal32Precision) + '.' +
    CopyRight(S, Decimal32Precision);
  Result := S;
end;

function Decimal32ToStrB(const A: Decimal32): RawByteString;
var
  S : RawByteString;
  T : Word64Rec;
  L : Integer;
begin
  if A.Value32 = 0 then
    begin
      Result := '0.0000';
      exit;
    end;
  Word64InitWord32(T, A.Value32);
  S := StrPadLeftB(Word64ToStrB(T), '0', Decimal32Precision + 1, False);
  L := Length(S);
  S :=
    CopyLeftB(S, L - Decimal32Precision) + '.' +
    CopyRightB(S, Decimal32Precision);
  Result := S;
end;

function Decimal32ToStrU(const A: Decimal32): UnicodeString;
var
  S : UnicodeString;
  T : Word64Rec;
  L : Integer;
begin
  if A.Value32 = 0 then
    begin
      Result := '0.0000';
      exit;
    end;
  Word64InitWord32(T, A.Value32);
  S := StrPadLeftU(Word64ToStrU(T), '0', Decimal32Precision + 1, False);
  L := Length(S);
  S :=
    CopyLeftU(S, L - Decimal32Precision) + '.' +
    CopyRightU(S, Decimal32Precision);
  Result := S;
end;

function TryStrToDecimal32_1(const A: String; out B: Decimal32): TDecimalConvertErrorType;
var
  ResInt : Word32;
  ResFrac : Word32;
  Res : Word32;
  I, L : Integer;
  C : Char;
  Digit : Integer;
  DigitFactor : Word32;
  RoundingDigit : Integer;
begin
  L := Length(A);
  if L = 0 then
    begin
      Result := dceConvertError;
      exit;
    end;
  I := 1;
  // integer part
  ResInt := 0;
  while I <= L do
    begin
      C := A[I];
      if C = '.' then
        if I = 1 then
          begin
            Result := dceConvertError;
            exit;
          end
        else
          begin
            if I = L then
              begin
                Result := dceConvertError;
                exit;
              end;
            Inc(I);
            break;
          end;
      Digit := CharDigitToInt(C);
      if Digit < 0 then
        begin
          Result := dceConvertError;
          exit;
        end;
      ResInt := ResInt * 10;
      Inc(ResInt, Digit);
      if ResInt > Decimal32MaxInt then
        begin
          Result := dceOverflowError;
          exit;
        end;
      Inc(I);
    end;
  // fraction part
  ResFrac := 0;
  DigitFactor := Decimal32Scale div 10;
  while I <= L do
    begin
      C := A[I];
      Digit := CharDigitToInt(C);
      Inc(I);
      if Digit < 0 then
        begin
          Result := dceConvertError;
          exit;
        end;
      Inc(ResFrac, Byte(Digit) * DigitFactor);
      DigitFactor := DigitFactor div 10;
      if DigitFactor = 0 then
        break;
    end;
  // rounding
  if I <= L then
    begin
      C := A[I];
      RoundingDigit := CharDigitToInt(C);
      Inc(I);
      if RoundingDigit < 0 then
        begin
          Result := dceConvertError;
          exit;
        end;
      if RoundingDigit = 5 then
        while I <= L do
          begin
            C := A[I];
            Digit := CharDigitToInt(C);
            Inc(I);
            if Digit < 0 then
              begin
                Result := dceConvertError;
                exit;
              end;
            if Digit > 0 then
              begin
                RoundingDigit := 6;
                break;
              end;
          end;
    end
  else
    RoundingDigit := 0;
  // validate remaining digits
  while I <= L do
    begin
      C := A[I];
      Digit := CharDigitToInt(C);
      Inc(I);
      if Digit < 0 then
        begin
          Result := dceConvertError;
          exit;
        end;
    end;
  // combine integer and fractional parts
  Res := ResInt * Decimal32Scale + ResFrac;
  // round
  if (RoundingDigit > 5) or
     ((RoundingDigit = 5) and (Res and 1 = 1)) then
    begin
      Inc(Res);
      if Res > Decimal32MaxValue then
        begin
          Result := dceOverflowError;
          exit;
        end;
    end;
  // result
  B.Value32 := Res;
  Result := dceNoError;
end;

function TryStrToDecimal32_2(const A: String; out B: Decimal32): TDecimalConvertErrorType;
var
  ResInt : Word64Rec;
  ResIntDigits : Integer;
  ResFrac : Word32;
  ResFracDigits : Integer;
  ResFracRoundDigit : Integer;
  ResExp : Integer;
  ResExpSign : Integer;
  Res : Word32;
  I, L : Integer;
  C : Char;
  Digit : Integer;
  DigitFactor : Word32;
begin
  L := Length(A);
  if L = 0 then
    begin
      Result := dceConvertError;
      exit;
    end;
  I := 1;
  // integer part
  Word64InitZero(ResInt);
  ResIntDigits := 0;
  repeat
    C := A[I];
    if (C = '.') or (C = 'E') then
      break;
    Digit := CharDigitToInt(C);
    if Digit < 0 then
      begin
        Result := dceConvertError;
        exit;
      end;
    Inc(ResIntDigits);
    Word64MultiplyWord8(ResInt, 10);
    Word64AddWord8(ResInt, Digit);
    if Word64CompareWord32(ResInt, Decimal32MaxValue) > 0 then
      begin
        Result := dceOverflowError;
        exit;
      end;
    Inc(I);
  until I > L;
  if ResIntDigits = 0 then
    begin
      Result := dceConvertError;
      exit;
    end;
  // fraction part
  ResFrac := 0;
  ResFracDigits := 0;
  ResFracRoundDigit := 0;
  if I <= L then
    begin
      C := A[I];
      if C = '.' then
        begin
          Inc(I);
          if I > L then
            begin
              Result := dceConvertError;
              exit;
            end;
          DigitFactor := Decimal32Scale div 10;
          repeat
            C := A[I];
            if C = 'E' then
              break;
            Digit := CharDigitToInt(C);
            if Digit < 0 then
              begin
                Result := dceConvertError;
                exit;
              end;
            Inc(I);
            Inc(ResFracDigits);
            Inc(ResFrac, Byte(Digit) * DigitFactor);
            DigitFactor := DigitFactor div 10;
            if DigitFactor = 0 then
              break;
          until I > L;
          if ResFracDigits = 0 then
            begin
              Result := dceConvertError;
              exit;
            end;
          // fraction rounding
          if (I <= L) and (A[I] <> 'E') then
            begin
              C := A[I];
              ResFracRoundDigit := CharDigitToInt(C);
              Inc(I);
              if ResFracRoundDigit < 0 then
                begin
                  Result := dceConvertError;
                  exit;
                end;
              if ResFracRoundDigit = 5 then
                while I <= L do
                  begin
                    C := A[I];
                    if C = 'E' then
                      break;
                    Digit := CharDigitToInt(C);
                    if Digit < 0 then
                      begin
                        Result := dceConvertError;
                        exit;
                      end;
                    Inc(I);
                    if Digit > 0 then
                      begin
                        ResFracRoundDigit := 6;
                        break;
                      end;
                  end;
              // validate remaining fraction digits
              while I <= L do
                begin
                  C := A[I];
                  if C = 'E' then
                    break;
                  Digit := CharDigitToInt(C);
                  if Digit < 0 then
                    begin
                      Result := dceConvertError;
                      exit;
                    end;
                  Inc(I);
                end;
            end;
        end;
    end;
  // exponent
  ResExp := 0;
  ResExpSign := 0;
  if I <= L then
    begin
      C := A[I];
      if C = 'E' then
        begin
          Inc(I);
          if I > L then
            begin
              Result := dceConvertError;
              exit;
            end;
          C := A[I];
          if (C = '+') or (C = '-') then
            begin
              if C = '+' then
                ResExpSign := 1
              else
                ResExpSign := -1;
              Inc(I);
              if I > L then
                begin
                  Result := dceConvertError;
                  exit;
                end;
            end;
          repeat
            C := A[I];
            Digit := CharDigitToInt(C);
            if Digit < 0 then
              begin
                Result := dceConvertError;
                exit;
              end;
            ResExp := ResExp * 10 + Digit;
            if ResExp > 9999 then
              begin
                Result := dceOverflowError;
                exit;
              end;
            Inc(I);
          until I > L;
        end;
    end;
  // end of string
  if I <= L then
    begin
      Result := dceConvertError;
      exit;
    end;
  // combine integer and fractional parts
  if Word64CompareWord32(ResInt, Decimal32MaxInt) > 0 then
    begin
      Result := dceOverflowError;
      exit;
    end;
  Res := Word64ToWord32(ResInt) * Decimal32Scale + ResFrac;
  // round
  if (ResFracRoundDigit > 5) or
     ((ResFracRoundDigit = 5) and (Res and 1 = 1)) then
    begin
      Inc(Res);
      if Res > Decimal32MaxValue then
        begin
          Result := dceOverflowError;
          exit;
        end;
    end;
  // exponent
  if Res <> 0 then
    if ResExpSign >= 0 then
      while ResExp > 0 do
        begin
          Res := Res * 10;
          if Res > Decimal32MaxValue then
            begin
              Result := dceOverflowError;
              exit;
            end;
          Dec(ResExp);
        end
    else
      while ResExp > 0 do
        begin
          Res := Res div 10;
          if Res = 0 then
            break;
          Dec(ResExp);
        end;
  // result
  B.Value32 := Res;
  Result := dceNoError;
end;

function TryStrToDecimal32(const A: String; out B: Decimal32): TDecimalConvertErrorType;
var
  ResInt : Word64Rec;
  ResIntDigits : Integer;
  ResFrac : Word32;
  ResFracDigits : Integer;
  ResFracRoundDigit : Integer;
  ResExp : Integer;
  ResExpSign : Integer;
  Res : Word64Rec;
  I, L : Integer;
  C : Char;
  Digit : Integer;
  DigitFactor : Word32;
  R : Byte;
begin
  L := Length(A);
  if L = 0 then
    begin
      Result := dceConvertError;
      exit;
    end;
  I := 1;
  // integer part
  Word64InitZero(ResInt);
  ResIntDigits := 0;
  repeat
    C := A[I];
    if (C = '.') or (C = 'E') then
      break;
    Digit := CharDigitToInt(C);
    if Digit < 0 then
      begin
        Result := dceConvertError;
        exit;
      end;
    Inc(ResIntDigits);
    Word64MultiplyWord8(ResInt, 10);
    Word64AddWord8(ResInt, Digit);
    if Word64CompareWord32(ResInt, Decimal32MaxValue) > 0 then
      begin
        Result := dceOverflowError;
        exit;
      end;
    Inc(I);
  until I > L;
  if ResIntDigits = 0 then
    begin
      Result := dceConvertError;
      exit;
    end;
  // fraction part
  ResFrac := 0;
  ResFracDigits := 0;
  ResFracRoundDigit := 0;
  if I <= L then
    begin
      C := A[I];
      if C = '.' then
        begin
          Inc(I);
          if I > L then
            begin
              Result := dceConvertError;
              exit;
            end;
          DigitFactor := Decimal32Scale div 10;
          repeat
            C := A[I];
            if C = 'E' then
              break;
            Digit := CharDigitToInt(C);
            if Digit < 0 then
              begin
                Result := dceConvertError;
                exit;
              end;
            Inc(I);
            Inc(ResFracDigits);
            Inc(ResFrac, Byte(Digit) * DigitFactor);
            DigitFactor := DigitFactor div 10;
            if DigitFactor = 0 then
              break;
          until I > L;
          if ResFracDigits = 0 then
            begin
              Result := dceConvertError;
              exit;
            end;
          // fraction rounding
          if (I <= L) and (A[I] <> 'E') then
            begin
              C := A[I];
              ResFracRoundDigit := CharDigitToInt(C);
              Inc(I);
              if ResFracRoundDigit < 0 then
                begin
                  Result := dceConvertError;
                  exit;
                end;
              if ResFracRoundDigit = 5 then
                while I <= L do
                  begin
                    C := A[I];
                    if C = 'E' then
                      break;
                    Digit := CharDigitToInt(C);
                    if Digit < 0 then
                      begin
                        Result := dceConvertError;
                        exit;
                      end;
                    Inc(I);
                    if Digit > 0 then
                      begin
                        ResFracRoundDigit := 6;
                        break;
                      end;
                  end;
              // validate remaining fraction digits
              while I <= L do
                begin
                  C := A[I];
                  if C = 'E' then
                    break;
                  Digit := CharDigitToInt(C);
                  if Digit < 0 then
                    begin
                      Result := dceConvertError;
                      exit;
                    end;
                  Inc(I);
                end;
            end;
        end;
    end;
  // exponent
  ResExp := 0;
  ResExpSign := 0;
  if I <= L then
    begin
      C := A[I];
      if C = 'E' then
        begin
          Inc(I);
          if I > L then
            begin
              Result := dceConvertError;
              exit;
            end;
          C := A[I];
          if (C = '+') or (C = '-') then
            begin
              if C = '+' then
                ResExpSign := 1
              else
                ResExpSign := -1;
              Inc(I);
              if I > L then
                begin
                  Result := dceConvertError;
                  exit;
                end;
            end;
          repeat
            C := A[I];
            Digit := CharDigitToInt(C);
            if Digit < 0 then
              begin
                Result := dceConvertError;
                exit;
              end;
            ResExp := ResExp * 10 + Digit;
            if ResExp > 9999 then
              begin
                Result := dceOverflowError;
                exit;
              end;
            Inc(I);
          until I > L;
        end;
    end;
  // end of string
  if I <= L then
    begin
      Result := dceConvertError;
      exit;
    end;
  // combine integer and fractional parts
  Res := ResInt;
  Word64MultiplyWord32(Res, Decimal32Scale);
  Word64AddWord32(Res, ResFrac);
  // round
  if (ResFracRoundDigit > 5) or
     ((ResFracRoundDigit = 5) and Word64IsOdd(Res)) then
    Word64Inc(Res);
  // exponent
  if not Word64IsZero(Res) then
    if ResExpSign >= 0 then
      while ResExp > 0 do
        begin
          Word64MultiplyWord8(Res, 10);
          if Word64CompareWord32(Res, Decimal32MaxValue) > 0 then
            begin
              Result := dceOverflowError;
              exit;
            end;
          Dec(ResExp);
        end
    else
      while ResExp > 0 do
        begin
          Word64DivideWord8(Res, 10, Res, R);
          if Word64IsZero(Res) then
            break;
          Dec(ResExp);
        end;
  // result
  if Word64CompareWord32(Res, Decimal32MaxValue) > 0 then
    begin
      Result := dceOverflowError;
      exit;
    end;
  B.Value32 := Word64ToWord32(Res);
  Result := dceNoError;
end;

function TryStrToDecimal32_Tst_Frac64(const A: String; out B: Decimal32): TDecimalConvertErrorType;
var
  ResInt : Word64Rec;
  ResIntDigits : Integer;
  ResFrac : Word64Rec;
  ResFracDigits : Integer;
  ResFracRoundDigit : Integer;
  ResExp : Integer;
  ResExpSign : Integer;
  Res : Word64Rec;
  I, L, N : Integer;
  C : Char;
  Digit : Integer;
  DigitFactor, T : Word64Rec;
  R : Byte;
begin
  L := Length(A);
  if L = 0 then
    begin
      Result := dceConvertError;
      exit;
    end;
  I := 1;
  // integer part
  Word64InitZero(ResInt);
  ResIntDigits := 0;
  repeat
    C := A[I];
    if (C = '.') or (C = 'E') then
      break;
    Digit := CharDigitToInt(C);
    if Digit < 0 then
      begin
        Result := dceConvertError;
        exit;
      end;
    Inc(ResIntDigits);
    Word64MultiplyWord8(ResInt, 10);
    Word64AddWord8(ResInt, Digit);
    if Word64CompareWord32(ResInt, Decimal32MaxValue) > 0 then
      begin
        Result := dceOverflowError;
        exit;
      end;
    Inc(I);
  until I > L;
  if ResIntDigits = 0 then
    begin
      Result := dceConvertError;
      exit;
    end;
  // fraction part
  Word64InitZero(ResFrac);
  ResFracDigits := 0;
  ResFracRoundDigit := 0;
  if I <= L then
    begin
      C := A[I];
      if C = '.' then
        begin
          Inc(I);
          if I > L then
            begin
              Result := dceConvertError;
              exit;
            end;
          Word64InitWord32(DigitFactor, Decimal64Scale div 10);
          repeat
            C := A[I];
            if C = 'E' then
              break;
            Digit := CharDigitToInt(C);
            if Digit < 0 then
              begin
                Result := dceConvertError;
                exit;
              end;
            Inc(I);
            Inc(ResFracDigits);
            T := DigitFactor;
            Word64MultiplyWord8(T, Byte(Digit));
            Word64AddWord64(ResFrac, T);
            Word64DivideWord8(DigitFactor, 10, DigitFactor, R);
            if Word64IsZero(DigitFactor) then
              break;
          until I > L;
          if ResFracDigits = 0 then
            begin
              Result := dceConvertError;
              exit;
            end;
          // fraction rounding
          if (I <= L) and (A[I] <> 'E') then
            begin
              C := A[I];
              ResFracRoundDigit := CharDigitToInt(C);
              Inc(I);
              if ResFracRoundDigit < 0 then
                begin
                  Result := dceConvertError;
                  exit;
                end;
              if ResFracRoundDigit = 5 then
                while I <= L do
                  begin
                    C := A[I];
                    if C = 'E' then
                      break;
                    Digit := CharDigitToInt(C);
                    if Digit < 0 then
                      begin
                        Result := dceConvertError;
                        exit;
                      end;
                    Inc(I);
                    if Digit > 0 then
                      begin
                        ResFracRoundDigit := 6;
                        break;
                      end;
                  end;
              // validate remaining fraction digits
              while I <= L do
                begin
                  C := A[I];
                  if C = 'E' then
                    break;
                  Digit := CharDigitToInt(C);
                  if Digit < 0 then
                    begin
                      Result := dceConvertError;
                      exit;
                    end;
                  Inc(I);
                end;
            end;
        end;
    end;
  // exponent
  ResExp := 0;
  ResExpSign := 0;
  if I <= L then
    begin
      C := A[I];
      if C = 'E' then
        begin
          Inc(I);
          if I > L then
            begin
              Result := dceConvertError;
              exit;
            end;
          C := A[I];
          if (C = '+') or (C = '-') then
            begin
              if C = '+' then
                ResExpSign := 1
              else
                ResExpSign := -1;
              Inc(I);
              if I > L then
                begin
                  Result := dceConvertError;
                  exit;
                end;
            end;
          repeat
            C := A[I];
            Digit := CharDigitToInt(C);
            if Digit < 0 then
              begin
                Result := dceConvertError;
                exit;
              end;
            ResExp := ResExp * 10 + Digit;
            if ResExp > 9999 then
              begin
                Result := dceOverflowError;
                exit;
              end;
            Inc(I);
          until I > L;
        end;
    end;
  // end of string
  if I <= L then
    begin
      Result := dceConvertError;
      exit;
    end;
  // combine integer and fractional parts
  Res := ResInt;
  Word64MultiplyWord32(Res, Decimal64Scale);
  Word64AddWord64(Res, ResFrac);
  // round
  if (ResFracRoundDigit > 5) or
     ((ResFracRoundDigit = 5) and Word64IsOdd(Res)) then
    Word64Inc(Res);
  // exponent
  if not Word64IsZero(Res) then
    if ResExpSign >= 0 then
      while ResExp > 0 do
        begin
          Word64MultiplyWord8(Res, 10);
          if Word64CompareWord64(Res, Decimal64MaxValueW64) > 0 then
            begin
              Result := dceOverflowError;
              exit;
            end;
          Dec(ResExp);
        end
    else
      while ResExp > 0 do
        begin
          Word64DivideWord8(Res, 10, Res, R);
          if Word64IsZero(Res) then
            break;
          Dec(ResExp);
        end;
  N := Decimal64Precision - Decimal32Precision;
  while N > 0 do
    begin
      Word64DivideWord8(Res, 10, Res, R);
      if Word64IsZero(Res) then
        break;
      Dec(N);
    end;
  // result
  if Word64CompareWord32(Res, Decimal32MaxValue) > 0 then
    begin
      Result := dceOverflowError;
      exit;
    end;
  B.Value32 := Word64ToWord32(Res);
  Result := dceNoError;
end;

function TryStrToDecimal32B(const A: RawByteString; out B: Decimal32): TDecimalConvertErrorType;
var
  ResInt : Word64Rec;
  ResIntDigits : Integer;
  ResFrac : Word32;
  ResFracDigits : Integer;
  ResFracRoundDigit : Integer;
  ResExp : Integer;
  ResExpSign : Integer;
  Res : Word32;
  I, L : Integer;
  C : AnsiChar;
  Digit : Integer;
  DigitFactor : Word32;
begin
  L := Length(A);
  if L = 0 then
    begin
      Result := dceConvertError;
      exit;
    end;
  I := 1;
  // integer part
  Word64InitZero(ResInt);
  ResIntDigits := 0;
  repeat
    C := A[I];
    if (C = '.') or (C = 'E') then
      break;
    Digit := ByteCharDigitToInt(C);
    if Digit < 0 then
      begin
        Result := dceConvertError;
        exit;
      end;
    Inc(ResIntDigits);
    Word64MultiplyWord8(ResInt, 10);
    Word64AddWord8(ResInt, Digit);
    if Word64CompareWord32(ResInt, Decimal32MaxValue) > 0 then
      begin
        Result := dceOverflowError;
        exit;
      end;
    Inc(I);
  until I > L;
  if ResIntDigits = 0 then
    begin
      Result := dceConvertError;
      exit;
    end;
  // fraction part
  ResFrac := 0;
  ResFracDigits := 0;
  ResFracRoundDigit := 0;
  if I <= L then
    begin
      C := A[I];
      if C = '.' then
        begin
          Inc(I);
          if I > L then
            begin
              Result := dceConvertError;
              exit;
            end;
          DigitFactor := Decimal32Scale div 10;
          repeat
            C := A[I];
            if C = 'E' then
              break;
            Digit := ByteCharDigitToInt(C);
            Inc(I);
            if Digit < 0 then
              begin
                Result := dceConvertError;
                exit;
              end;
            Inc(ResFracDigits);
            Inc(ResFrac, Byte(Digit) * DigitFactor);
            DigitFactor := DigitFactor div 10;
            if DigitFactor = 0 then
              break;
          until I > L;
          if ResFracDigits = 0 then
            begin
              Result := dceConvertError;
              exit;
            end;
          // fraction rounding
          if (I <= L) and (A[I] <> 'E') then
            begin
              C := A[I];
              ResFracRoundDigit := ByteCharDigitToInt(C);
              Inc(I);
              if ResFracRoundDigit < 0 then
                begin
                  Result := dceConvertError;
                  exit;
                end;
              if ResFracRoundDigit = 5 then
                while I <= L do
                  begin
                    C := A[I];
                    if C = 'E' then
                      break;
                    Digit := ByteCharDigitToInt(C);
                    if Digit < 0 then
                      begin
                        Result := dceConvertError;
                        exit;
                      end;
                    Inc(I);
                    if Digit > 0 then
                      begin
                        ResFracRoundDigit := 6;
                        break;
                      end;
                  end;
              // validate remaining fraction digits
              while I <= L do
                begin
                  C := A[I];
                  if C = 'E' then
                    break;
                  Digit := ByteCharDigitToInt(C);
                  if Digit < 0 then
                    begin
                      Result := dceConvertError;
                      exit;
                    end;
                  Inc(I);
                end;
            end;
        end;
    end;
  // exponent
  ResExp := 0;
  ResExpSign := 0;
  if I <= L then
    begin
      C := A[I];
      if C = 'E' then
        begin
          Inc(I);
          if I > L then
            begin
              Result := dceConvertError;
              exit;
            end;
          C := A[I];
          if (C = '+') or (C = '-') then
            begin
              if C = '+' then
                ResExpSign := 1
              else
                ResExpSign := -1;
              Inc(I);
              if I > L then
                begin
                  Result := dceConvertError;
                  exit;
                end;
            end;
          repeat
            C := A[I];
            Digit := ByteCharDigitToInt(C);
            if Digit < 0 then
              begin
                Result := dceConvertError;
                exit;
              end;
            ResExp := ResExp * 10 + Digit;
            if ResExp > 9999 then
              begin
                Result := dceOverflowError;
                exit;
              end;
            Inc(I);
          until I > L;
        end;
    end;
  // end of string
  if I <= L then
    begin
      Result := dceConvertError;
      exit;
    end;
  // combine integer and fractional parts
  if Word64CompareWord32(ResInt, Decimal32MaxInt) > 0 then
    begin
      Result := dceOverflowError;
      exit;
    end;
  Res := Word64ToWord32(ResInt) * Decimal32Scale + ResFrac;
  // round
  if (ResFracRoundDigit > 5) or
     ((ResFracRoundDigit = 5) and (Res and 1 = 1)) then
    begin
      Inc(Res);
      if Res > Decimal32MaxValue then
        begin
          Result := dceOverflowError;
          exit;
        end;
    end;
  // exponent
  if Res <> 0 then
    if ResExpSign >= 0 then
      while ResExp > 0 do
        begin
          Res := Res * 10;
          if Res > Decimal32MaxValue then
            begin
              Result := dceOverflowError;
              exit;
            end;
          Dec(ResExp);
        end
    else
      while ResExp > 0 do
        begin
          Res := Res div 10;
          if Res = 0 then
            break;
          Dec(ResExp);
        end;
  // result
  B.Value32 := Res;
  Result := dceNoError;
end;

function StrToDecimal32(const A: String): Decimal32;
begin
  case TryStrToDecimal32(A, Result) of
    dceNoError       : ;
    dceConvertError  : raise EDecimalError.Create(SConvertError);
    dceOverflowError : raise EDecimalError.Create(SOverflowError);
  end;
end;

function StrToDecimal32B(const A: RawByteString): Decimal32;
begin
  case TryStrToDecimal32B(A, Result) of
    dceNoError       : ;
    dceConvertError  : raise EDecimalError.Create(SConvertError);
    dceOverflowError : raise EDecimalError.Create(SOverflowError);
  end;
end;



{                                                                              }
{ Decimal64                                                                    }
{                                                                              }
procedure Decimal64InitZero(var A: Decimal64);
begin
  Word64InitZero(A.Value64);
end;

procedure Decimal64InitOne(var A: Decimal64);
begin
  Word64InitInt64(A.Value64, Decimal64Scale);
end;

procedure Decimal64InitMax(var A: Decimal64);
begin
  A.Value64 := Decimal64MaxValueW64;
end;

function Decimal64IsZero(const A: Decimal64): Boolean;
begin
  Result := Word64IsZero(A.Value64);
end;

function Decimal64IsOne(const A: Decimal64): Boolean;
begin
  Result := Word64EqualsWord32(A.Value64, Decimal64Scale);
end;

function Decimal64IsMaximum(const A: Decimal64): Boolean;
begin
  Result := Word64EqualsWord64(A.Value64, Decimal64MaxValueW64);
end;

function Decimal64IsOverflow(const A: Decimal64): Boolean;
begin
  Result := Word64CompareWord64(A.Value64, Decimal64MaxValueW64) > 0;
end;

function Word64IsDecimal64Range(const A: Word64Rec): Boolean;
begin
  Result := Word64CompareInt64(A, Decimal64MaxInt) <= 0;
end;

function Int32IsDecimal64Range(const A: Int32): Boolean;
begin
  Result := A >= 0;
end;

function Int64IsDecimal64Range(const A: Int64): Boolean;
begin
  Result := (A >= 0) and (A <= Decimal64MaxInt);
end;

function FloatIsDecimal64Range(const A: Double): Boolean;
begin
  Result :=
    (A >= Decimal64MinFloatD) and
    (A < Decimal64MaxFloatLimD);
end;

procedure Decimal64InitWord8(var A: Decimal64; const B: Byte);
begin
  Word64InitInt64(A.Value64, Int64(B) * Decimal64Scale);
end;

procedure Decimal64InitWord16(var A: Decimal64; const B: Word);
begin
  Word64InitInt64(A.Value64, Int64(B) * Decimal64Scale);
end;

procedure Decimal64InitWord32(var A: Decimal64; const B: Word32);
begin
  Word64InitInt64(A.Value64, Int64(B) * Decimal64Scale);
end;

procedure Decimal64InitWord64(var A: Decimal64; const B: Word64Rec);
var T, F : Word64Rec;
begin
  if Word64CompareInt64(B, Decimal64MaxInt) > 0 then
    raise EDecimalError.Create(SOverflowError);
  T := B;
  Word64InitInt64(F, Decimal64Scale);
  Word64MultiplyWord64InPlace(T, F);
  A.Value64 := T;
end;

procedure Decimal64InitInt32(var A: Decimal64; const B: Int32);
var T, F : Word64Rec;
begin
  if (B < 0) or (B > Decimal64MaxInt) then
    raise EDecimalError.Create(SOverflowError);
  Word64InitInt32(T, B);
  Word64InitInt64(F, Decimal64Scale);
  Word64MultiplyWord64InPlace(T, F);
  A.Value64 := T;
end;

procedure Decimal64InitInt64(var A: Decimal64; const B: Int64);
var T, F : Word64Rec;
begin
  if (B < 0) or (B > Decimal64MaxInt) then
    raise EDecimalError.Create(SOverflowError);
  Word64InitInt64(T, B);
  Word64InitInt64(F, Decimal64Scale);
  Word64MultiplyWord64InPlace(T, F);
  A.Value64 := T;
end;

procedure Decimal64InitDecimal32(var A: Decimal64; const B: Decimal32);
var T, Q : Word64Rec;
    R : Word;
begin
  Word64InitWord32(T, B.Value32);
  Word64MultiplyWord32(T, Decimal64Scale);
  Word64DivideWord16(T, Decimal32Scale, Q, R);
  A.Value64 := Q;
end;

procedure Decimal64InitDecimal64(var A: Decimal64; const B: Decimal64);
begin
  A.Value64 := B.Value64;
end;

procedure Decimal64InitFloat(var A: Decimal64; const B: Double);
var D : Double;
    I : Int64;
begin
  if not FloatIsDecimal64Range(B) then
    raise EDecimalError.Create(SOverflowError);
  D := B * Decimal64Scale;
  I := Round(D);
  Word64InitInt64(A.Value64, I);
end;

function Decimal64ToWord8(const A: Decimal64): Byte;
var Q : Word64Rec;
    R : Word32;
begin
  Word64DivideWord32(A.Value64, Decimal64Scale, Q, R);
  if R <> 0 then
    raise EDecimalError.Create(SConvertError);
  if not Word64IsWord8Range(Q) then
    raise EDecimalError.Create(SOverflowError);
  Result := Word64ToWord32(Q);
end;

function Decimal64ToWord16(const A: Decimal64): Word;
var Q : Word64Rec;
    R : Word32;
begin
  Word64DivideWord32(A.Value64, Decimal64Scale, Q, R);
  if R <> 0 then
    raise EDecimalError.Create(SConvertError);
  if not Word64IsWord16Range(Q) then
    raise EDecimalError.Create(SOverflowError);
  Result := Word64ToWord32(Q);
end;

function Decimal64ToWord32(const A: Decimal64): Word32;
var Q : Word64Rec;
    R : Word32;
begin
  Word64DivideWord32(A.Value64, Decimal64Scale, Q, R);
  if R <> 0 then
    raise EDecimalError.Create(SConvertError);
  Result := Word64ToWord32(Q);
end;

function Decimal64ToWord64(const A: Decimal64): Word64Rec;
var Q : Word64Rec;
    R : Word32;
begin
  Word64DivideWord32(A.Value64, Decimal64Scale, Q, R);
  if R <> 0 then
    raise EDecimalError.Create(SConvertError);
  Result := Q;
end;

function Decimal64ToInt32(const A: Decimal64): Int32;
var Q : Word64Rec;
    R : Word32;
begin
  Word64DivideWord32(A.Value64, Decimal64Scale, Q, R);
  if R <> 0 then
    raise EDecimalError.Create(SConvertError);
  if not Word64IsInt32Range(Q) then
    raise EDecimalError.Create(SConvertError);
  Result := Word64ToInt32(Q);
end;

function Decimal64ToInt64(const A: Decimal64): Int64;
var Q : Word64Rec;
    R : Word32;
begin
  Word64DivideWord32(A.Value64, Decimal64Scale, Q, R);
  if R <> 0 then
    raise EDecimalError.Create(SConvertError);
  Result := Word64ToInt64(Q);
end;

function Decimal64ToDecimal32(const A: Decimal64): Decimal32;
var T, Q : Word128;
    R : Word32;
begin
  Word128InitWord64(T, A.Value64);
  Word128MultiplyWord16(T, Decimal32Scale);
  Word128DivideWord32(T, Decimal64Scale, Q, R);
  if Word128CompareWord32(Q, Decimal32MaxValue) > 0 then
    raise EDecimalError.Create(SOverflowError);
  Result.Value32 := Word128ToWord32(Q);
end;

function Decimal64ToFloat(const A: Decimal64): Double;
begin
  Result := Word64ToFloat(A.Value64) / Decimal64Scale;
end;

function Decimal64Trunc(const A: Decimal64): Int64;
var Q : Word64Rec;
    R : Word32;
begin
  Word64DivideWord32(A.Value64, Decimal64Scale, Q, R);
  Result := Word64ToInt64(Q);
end;

function Decimal64Round(const A: Decimal64): Int64;
var F : Word32;
    R : Word64Rec;
begin
  Word64DivideWord32(A.Value64, Decimal64Scale, R, F);
  if (F > Decimal64RoundTerm) or
     ((F = Decimal64RoundTerm) and Word64IsOdd(R)) then
    Word64Inc(R);
  Result := Word64ToInt64(R);
end;

function Decimal64FracWord(const A: Decimal64): Word32;
var R : Word64Rec;
    F : Word32;
begin
  Word64DivideWord32(A.Value64, Decimal64Scale, R, F);
  Result := F;
end;

function Decimal64EqualsWord8(const A: Decimal64; const B: Byte): Boolean;
begin
  Result := Word64EqualsInt64(A.Value64, Int64(B) * Decimal64Scale);
end;

function Decimal64EqualsWord16(const A: Decimal64; const B: Word): Boolean;
begin
  Result := Word64EqualsInt64(A.Value64, Int64(B) * Decimal64Scale);
end;

function Decimal64EqualsWord32(const A: Decimal64; const B: Word32): Boolean;
begin
  Result := Word64EqualsInt64(A.Value64, Int64(B) * Decimal64Scale);
end;

function Decimal64EqualsInt32(const A: Decimal64; const B: Int32): Boolean;
begin
  if B < 0 then
    Result := False
  else
    Result := Word64EqualsInt64(A.Value64, Int64(B) * Decimal64Scale);
end;

function Decimal64EqualsInt64(const A: Decimal64; const B: Int64): Boolean;
var T : Word128;
begin
  if B < 0 then
    Result := False
  else
  if B > Decimal64MaxInt then
    Result := False
  else
    begin
      Word128InitInt64(T, B);
      Word128MultiplyWord32(T, Decimal64Scale);
      Result := Word128EqualsWord64(T, A.Value64);
    end;
end;

function Decimal64EqualsDecimal64(const A: Decimal64; const B: Decimal64): Boolean;
begin
  Result := Word64EqualsWord64(A.Value64, B.Value64);
end;

function Decimal64EqualsFloat(const A: Decimal64; const B: Double): Boolean;
var T : Decimal64;
begin
  if not FloatIsDecimal64Range(B) then
    Result := False
  else
    begin
      Decimal64InitFloat(T, B);
      Result := Word64EqualsWord64(T.Value64, A.Value64);
    end;
end;

function Decimal64CompareWord8(const A: Decimal64; const B: Byte): Integer;
var C : Int64;
begin
  C := Int64(B) * Decimal64Scale;
  Result := Word64CompareInt64(A.Value64, C);
end;

function Decimal64CompareWord16(const A: Decimal64; const B: Word): Integer;
var C : Int64;
begin
  C := Int64(B) * Decimal64Scale;
  Result := Word64CompareInt64(A.Value64, C);
end;

function Decimal64CompareWord32(const A: Decimal64; const B: Word32): Integer;
var C : Int64;
begin
  C := Int64(B) * Decimal64Scale;
  Result := Word64CompareInt64(A.Value64, C);
end;

function Decimal64CompareInt32(const A: Decimal64; const B: Int32): Integer;
var C : Int64;
begin
  if B < 0 then
    Result := 1
  else
    begin
      C := Int64(B) * Decimal64Scale;
      Result := Word64CompareInt64(A.Value64, C);
    end;
end;

function Decimal64CompareInt64(const A: Decimal64; const B: Int64): Integer;
var T : Word128;
begin
  if B < 0 then
    Result := 1
  else
    begin
      Word128InitInt64(T, B);
      Word128MultiplyWord32(T, Decimal64Scale);
      Result := -Word128CompareWord64(T, A.Value64);
    end;
end;

function Decimal64CompareDecimal64(const A: Decimal64; const B: Decimal64): Integer;
begin
  Result := Word64CompareWord64(A.Value64, B.Value64);
end;

function Decimal64CompareFloat(const A: Decimal64; const B: Double): Integer;
var T : Decimal64;
begin
  if B < Decimal64MinFloatD then
    Result := 1
  else
  if B >= Decimal64MaxFloatLimD then
    Result := -1
  else
    begin
      Assert(FloatIsDecimal64Range(B));
      Decimal64InitFloat(T, B);
      Result := Word64CompareWord64(A.Value64, T.Value64);
    end;
end;

procedure Decimal64AddWord8(var A: Decimal64; const B: Byte);
var T, Q : Word64Rec;
begin
  T := A.Value64;
  Word64InitWord32(Q, B);
  Word64MultiplyWord32(Q, Decimal64Scale);
  Word64AddWord64(T, Q);
  if Word64CompareWord64(T, Decimal64MaxValueW64) > 0 then
    raise EDecimalError.Create(SOverflowError);
  A.Value64 := T;
end;

procedure Decimal64AddWord16(var A: Decimal64; const B: Word);
var T, Q : Word64Rec;
begin
  T := A.Value64;
  Word64InitWord32(Q, B);
  Word64MultiplyWord32(Q, Decimal64Scale);
  Word64AddWord64(T, Q);
  if Word64CompareWord64(T, Decimal64MaxValueW64) > 0 then
    raise EDecimalError.Create(SOverflowError);
  A.Value64 := T;
end;

procedure Decimal64AddWord32(var A: Decimal64; const B: Word32);
var T, Q : Word128;
begin
  Word128InitWord64(T, A.Value64);
  Word128InitWord32(Q, B);
  Word128MultiplyWord32(Q, Decimal64Scale);
  Word128AddWord128(T, Q);
  if Word128CompareWord64(T, Decimal64MaxValueW64) > 0 then
    raise EDecimalError.Create(SOverflowError);
  A.Value64 := Word128ToWord64(T);
end;

procedure Decimal64AddDecimal64(var A: Decimal64; const B: Decimal64);
var T : Word128;
begin
  Word128InitWord64(T, A.Value64);
  Word128AddWord64(T, B.Value64);
  if Word128CompareWord64(T, Decimal64MaxValueW64) > 0 then
    raise EDecimalError.Create(SOverflowError);
  A.Value64 := Word128ToWord64(T);
end;

procedure Decimal64SubtractWord8(var A: Decimal64; const B: Byte);
var T, Q : Word64Rec;
begin
  T := A.Value64;
  Word64InitWord32(Q, B);
  Word64MultiplyWord32(Q, Decimal64Scale);
  if Word64CompareWord64(T, Q) < 0 then
    raise EDecimalError.Create(SOverflowError);
  Word64SubtractWord64(T, Q);
  A.Value64 := T;
end;

procedure Decimal64SubtractWord16(var A: Decimal64; const B: Word);
var T, Q : Word64Rec;
begin
  T := A.Value64;
  Word64InitWord32(Q, B);
  Word64MultiplyWord32(Q, Decimal64Scale);
  if Word64CompareWord64(T, Q) < 0 then
    raise EDecimalError.Create(SOverflowError);
  Word64SubtractWord64(T, Q);
  A.Value64 := T;
end;

procedure Decimal64SubtractWord32(var A: Decimal64; const B: Word32);
var T, Q : Word64Rec;
begin
  T := A.Value64;
  Word64InitWord32(Q, B);
  Word64MultiplyWord32(Q, Decimal64Scale);
  if Word64CompareWord64(T, Q) < 0 then
    raise EDecimalError.Create(SOverflowError);
  Word64SubtractWord64(T, Q);
  A.Value64 := T;
end;

procedure Decimal64SubtractDecimal64(var A: Decimal64; const B: Decimal64);
begin
  if Word64CompareWord64(A.Value64, B.Value64) < 0 then
    raise EDecimalError.Create(SOverflowError);
  Word64SubtractWord64(A.Value64, B.Value64);
end;

procedure Decimal64MultiplyWord8(var A: Decimal64; const B: Byte);
var T : Word128;
begin
  Word128InitWord64(T, A.Value64);
  Word128MultiplyWord8(T, B);
  if Word128CompareWord64(T, Decimal64MaxValueW64) > 0 then
    raise EDecimalError.Create(SOverflowError);
  A.Value64 := Word128ToWord64(T);
end;

procedure Decimal64MultiplyWord16(var A: Decimal64; const B: Word);
var T : Word128;
begin
  Word128InitWord64(T, A.Value64);
  Word128MultiplyWord16(T, B);
  if Word128CompareWord64(T, Decimal64MaxValueW64) > 0 then
    raise EDecimalError.Create(SOverflowError);
  A.Value64 := Word128ToWord64(T);
end;

procedure Decimal64MultiplyWord32(var A: Decimal64; const B: Word32);
var T : Word128;
begin
  Word128InitWord64(T, A.Value64);
  Word128MultiplyWord32(T, B);
  if Word128CompareWord64(T, Decimal64MaxValueW64) > 0 then
    raise EDecimalError.Create(SOverflowError);
  A.Value64 := Word128ToWord64(T);
end;

procedure Decimal64MultiplyDecimal64(var A: Decimal64; const B: Decimal64);
var T, Q : Word128;
    R : Word32;
begin
  Word128InitWord64(T, A.Value64);
  Word128MultiplyWord64(T, B.Value64);
  Word128AddWord32(T, Decimal64RoundTerm);
  Word128DivideWord32(T, Decimal64Scale, Q, R);
  if Word128CompareWord64(Q, Decimal64MaxValueW64) > 0 then
    raise EDecimalError.Create(SOverflowError);
  A.Value64 := Word128ToWord64(Q);
end;

procedure Decimal64Sqr(var A: Decimal64);
begin
  Decimal64MultiplyDecimal64(A, A);
end;

procedure Decimal64DivideWord8(var A: Decimal64; const B: Byte);
var Q : Word64Rec;
    R : Byte;
begin
  Word64DivideWord8(A.Value64, B, Q, R);
  A.Value64 := Q;
end;

procedure Decimal64DivideWord16(var A: Decimal64; const B: Word);
var Q : Word64Rec;
    R : Word;
begin
  Word64DivideWord16(A.Value64, B, Q, R);
  A.Value64 := Q;
end;

procedure Decimal64DivideWord32(var A: Decimal64; const B: Word32);
var Q : Word64Rec;
    R : Word32;
begin
  Word64DivideWord32(A.Value64, B, Q, R);
  A.Value64 := Q;
end;

procedure Decimal64DivideDecimal64(var A: Decimal64; const B: Decimal64);
var T : Word128;
    Q : Word64Rec;
    F : Word128;
    G : Word64Rec;
begin
  Word128InitWord64(T, A.Value64);
  Word64InitInt64(Q, Decimal64Scale);
  Word128MultiplyWord64(T, Q);
  Word128DivideWord64(T, B.Value64, F, G);
  A.Value64 := Word128ToWord64(F);
end;

function Decimal64ToStr(const A: Decimal64): String;
var
  S : String;
  L : Integer;
begin
  if Word64IsZero(A.Value64) then
    begin
      Result := '0.000000000';
      exit;
    end;
  S := StrPadLeft(Word64ToStr(A.Value64), '0', Decimal64Precision + 1, False);
  L := Length(S);
  Result :=
    CopyLeft(S, L - Decimal64Precision) + '.' +
    CopyRight(S, Decimal64Precision);
end;

function Decimal64ToStrB(const A: Decimal64): RawByteString;
var
  S : RawByteString;
  L : Integer;
begin
  if Word64IsZero(A.Value64) then
    begin
      Result := '0.000000000';
      exit;
    end;
  S := StrPadLeftB(Word64ToStrB(A.Value64), '0', Decimal64Precision + 1, False);
  L := Length(S);
  Result :=
    CopyLeftB(S, L - Decimal64Precision) + '.' +
    CopyRightB(S, Decimal64Precision);
end;

function Decimal64ToStrU(const A: Decimal64): UnicodeString;
var
  S : UnicodeString;
  L : Integer;
begin
  if Word64IsZero(A.Value64) then
    begin
      Result := '0.000000000';
      exit;
    end;
  S := StrPadLeftU(Word64ToStrU(A.Value64), '0', Decimal64Precision + 1, False);
  L := Length(S);
  Result :=
    CopyLeftU(S, L - Decimal64Precision) + '.' +
    CopyRightU(S, Decimal64Precision);
end;

function TryStrToDecimal64(const A: String; out B: Decimal64): TDecimalConvertErrorType;
var
  ResInt : Word128;
  ResIntDigits : Integer;
  ResFrac : Word64Rec;
  ResFracDigits : Integer;
  ResFracRoundDigit : Integer;
  ResExp : Integer;
  ResExpSign : Integer;
  Res : Word64Rec;
  R : Byte;
  I, L : Integer;
  C : Char;
  Digit : Integer;
  DigitFactor : Word64Rec;
  T : Word64Rec;
begin
  L := Length(A);
  if L = 0 then
    begin
      Result := dceConvertError;
      exit;
    end;
  I := 1;
  // integer part
  Word128InitZero(ResInt);
  ResIntDigits := 0;
  repeat
    C := A[I];
    if (C = '.') or (C = 'E') then
      break;
    Digit := CharDigitToInt(C);
    if Digit < 0 then
      begin
        Result := dceConvertError;
        exit;
      end;
    Inc(ResIntDigits);
    Word128MultiplyWord8(ResInt, 10);
    Word128AddWord8(ResInt, Digit);
    if Word128CompareWord64(ResInt, Decimal64MaxValueW64) > 0 then
      begin
        Result := dceOverflowError;
        exit;
      end;
    Inc(I);
  until I > L;
  if ResIntDigits = 0 then
    begin
      Result := dceConvertError;
      exit;
    end;
  // fraction part
  Word64InitZero(ResFrac);
  ResFracDigits := 0;
  ResFracRoundDigit := 0;
  if I <= L then
    begin
      C := A[I];
      if C = '.' then
        begin
          Inc(I);
          if I > L then
            begin
              Result := dceConvertError;
              exit;
            end;
          Word64InitWord32(DigitFactor, Decimal64Scale);
          Word64DivideWord8(DigitFactor, 10, DigitFactor, R);
          repeat
            C := A[I];
            if C = 'E' then
              break;
            Digit := CharDigitToInt(C);
            Inc(I);
            if Digit < 0 then
              begin
                Result := dceConvertError;
                exit;
              end;
            Inc(ResFracDigits);
            T := DigitFactor;
            Word64MultiplyWord8(T, Byte(Digit));
            Word64AddWord64(ResFrac, T);
            Word64DivideWord8(DigitFactor, 10, DigitFactor, R);
            if Word64IsZero(DigitFactor) then
              break;
          until I > L;
          if ResFracDigits = 0 then
            begin
              Result := dceConvertError;
              exit;
            end;
          // fraction rounding
          if (I <= L) and (A[I] <> 'E') then
            begin
              C := A[I];
              ResFracRoundDigit := CharDigitToInt(C);
              Inc(I);
              if ResFracRoundDigit < 0 then
                begin
                  Result := dceConvertError;
                  exit;
                end;
              if ResFracRoundDigit = 5 then
                while I <= L do
                  begin
                    C := A[I];
                    if C = 'E' then
                      break;
                    Digit := CharDigitToInt(C);
                    if Digit < 0 then
                      begin
                        Result := dceConvertError;
                        exit;
                      end;
                    Inc(I);
                    if Digit > 0 then
                      begin
                        ResFracRoundDigit := 6;
                        break;
                      end;
                  end;
              // validate remaining fraction digits
              while I <= L do
                begin
                  C := A[I];
                  if C = 'E' then
                    break;
                  Digit := CharDigitToInt(C);
                  if Digit < 0 then
                    begin
                      Result := dceConvertError;
                      exit;
                    end;
                  Inc(I);
                end;
            end;
        end;
    end;
  // exponent
  ResExp := 0;
  ResExpSign := 0;
  if I <= L then
    begin
      C := A[I];
      if C = 'E' then
        begin
          Inc(I);
          if I > L then
            begin
              Result := dceConvertError;
              exit;
            end;
          C := A[I];
          if (C = '+') or (C = '-') then
            begin
              if C = '+' then
                ResExpSign := 1
              else
                ResExpSign := -1;
              Inc(I);
              if I > L then
                begin
                  Result := dceConvertError;
                  exit;
                end;
            end;
          repeat
            C := A[I];
            Digit := CharDigitToInt(C);
            if Digit < 0 then
              begin
                Result := dceConvertError;
                exit;
              end;
            ResExp := ResExp * 10 + Digit;
            if ResExp > 9999 then
              begin
                Result := dceOverflowError;
                exit;
              end;
            Inc(I);
          until I > L;
        end;
    end;
  // end of string
  if I <= L then
    begin
      Result := dceConvertError;
      exit;
    end;
  // combine integer and fractional parts
  if Word128CompareInt64(ResInt, Decimal64MaxInt) > 0 then
    begin
      Result := dceOverflowError;
      exit;
    end;
  Res := Word128ToWord64(ResInt);
  Word64MultiplyWord32(Res, Decimal64Scale);
  Word64AddWord64(Res, ResFrac);
  // round
  if (ResFracRoundDigit > 5) or
     ((ResFracRoundDigit = 5) and Word64IsOdd(Res)) then
    begin
      Word64Inc(Res);
      if Word64CompareWord64(Res, Decimal64MaxValueW64) > 0 then
        begin
          Result := dceOverflowError;
          exit;
        end;
    end;
  // exponent
  if not Word64IsZero(Res) then
    if ResExpSign >= 0 then
      while ResExp > 0 do
        begin
          Word64MultiplyWord8(Res, 10);
          if Word64CompareWord64(Res, Decimal64MaxValueW64) > 0 then
            begin
              Result := dceOverflowError;
              exit;
            end;
          Dec(ResExp);
        end
    else
      while ResExp > 0 do
        begin
          Word64DivideWord8(Res, 10, Res, R);
          if Word64IsZero(Res) then
            break;
          Dec(ResExp);
        end;
  // result
  B.Value64 := Res;
  Result := dceNoError;
end;

function TryStrToDecimal64B(const A: RawByteString; out B: Decimal64): TDecimalConvertErrorType;
var
  ResInt : Word128;
  ResIntDigits : Integer;
  ResFrac : Word64Rec;
  ResFracDigits : Integer;
  ResFracRoundDigit : Integer;
  ResExp : Integer;
  ResExpSign : Integer;
  Res : Word64Rec;
  R : Byte;
  I, L : Integer;
  C : AnsiChar;
  Digit : Integer;
  DigitFactor : Word64Rec;
  T : Word64Rec;
begin
  L := Length(A);
  if L = 0 then
    begin
      Result := dceConvertError;
      exit;
    end;
  I := 1;
  // integer part
  Word128InitZero(ResInt);
  ResIntDigits := 0;
  repeat
    C := A[I];
    if (C = '.') or (C = 'E') then
      break;
    Digit := ByteCharDigitToInt(C);
    if Digit < 0 then
      begin
        Result := dceConvertError;
        exit;
      end;
    Inc(ResIntDigits);
    Word128MultiplyWord8(ResInt, 10);
    Word128AddWord8(ResInt, Digit);
    if Word128CompareWord64(ResInt, Decimal64MaxValueW64) > 0 then
      begin
        Result := dceOverflowError;
        exit;
      end;
    Inc(I);
  until I > L;
  if ResIntDigits = 0 then
    begin
      Result := dceConvertError;
      exit;
    end;
  // fraction part
  Word64InitZero(ResFrac);
  ResFracDigits := 0;
  ResFracRoundDigit := 0;
  if I <= L then
    begin
      C := A[I];
      if C = '.' then
        begin
          Inc(I);
          if I > L then
            begin
              Result := dceConvertError;
              exit;
            end;
          Word64InitWord32(DigitFactor, Decimal64Scale);
          Word64DivideWord8(DigitFactor, 10, DigitFactor, R);
          repeat
            C := A[I];
            if C = 'E' then
              break;
            Digit := ByteCharDigitToInt(C);
            Inc(I);
            if Digit < 0 then
              begin
                Result := dceConvertError;
                exit;
              end;
            Inc(ResFracDigits);
            T := DigitFactor;
            Word64MultiplyWord8(T, Byte(Digit));
            Word64AddWord64(ResFrac, T);
            Word64DivideWord8(DigitFactor, 10, DigitFactor, R);
            if Word64IsZero(DigitFactor) then
              break;
          until I > L;
          if ResFracDigits = 0 then
            begin
              Result := dceConvertError;
              exit;
            end;
          // fraction rounding
          if (I <= L) and (A[I] <> 'E') then
            begin
              C := A[I];
              ResFracRoundDigit := ByteCharDigitToInt(C);
              Inc(I);
              if ResFracRoundDigit < 0 then
                begin
                  Result := dceConvertError;
                  exit;
                end;
              if ResFracRoundDigit = 5 then
                while I <= L do
                  begin
                    C := A[I];
                    if C = 'E' then
                      break;
                    Digit := ByteCharDigitToInt(C);
                    if Digit < 0 then
                      begin
                        Result := dceConvertError;
                        exit;
                      end;
                    Inc(I);
                    if Digit > 0 then
                      begin
                        ResFracRoundDigit := 6;
                        break;
                      end;
                  end;
              // validate remaining fraction digits
              while I <= L do
                begin
                  C := A[I];
                  if C = 'E' then
                    break;
                  Digit := ByteCharDigitToInt(C);
                  if Digit < 0 then
                    begin
                      Result := dceConvertError;
                      exit;
                    end;
                  Inc(I);
                end;
            end;
        end;
    end;
  // exponent
  ResExp := 0;
  ResExpSign := 0;
  if I <= L then
    begin
      C := A[I];
      if C = 'E' then
        begin
          Inc(I);
          if I > L then
            begin
              Result := dceConvertError;
              exit;
            end;
          C := A[I];
          if (C = '+') or (C = '-') then
            begin
              if C = '+' then
                ResExpSign := 1
              else
                ResExpSign := -1;
              Inc(I);
              if I > L then
                begin
                  Result := dceConvertError;
                  exit;
                end;
            end;
          repeat
            C := A[I];
            Digit := ByteCharDigitToInt(C);
            if Digit < 0 then
              begin
                Result := dceConvertError;
                exit;
              end;
            ResExp := ResExp * 10 + Digit;
            if ResExp > 9999 then
              begin
                Result := dceOverflowError;
                exit;
              end;
            Inc(I);
          until I > L;
        end;
    end;
  // end of string
  if I <= L then
    begin
      Result := dceConvertError;
      exit;
    end;
  // combine integer and fractional parts
  if Word128CompareInt64(ResInt, Decimal64MaxInt) > 0 then
    begin
      Result := dceOverflowError;
      exit;
    end;
  Res := Word128ToWord64(ResInt);
  Word64MultiplyWord32(Res, Decimal64Scale);
  Word64AddWord64(Res, ResFrac);
  // round
  if (ResFracRoundDigit > 5) or
     ((ResFracRoundDigit = 5) and Word64IsOdd(Res)) then
    begin
      Word64Inc(Res);
      if Word64CompareWord64(Res, Decimal64MaxValueW64) > 0 then
        begin
          Result := dceOverflowError;
          exit;
        end;
    end;
  // exponent
  if not Word64IsZero(Res) then
    if ResExpSign >= 0 then
      while ResExp > 0 do
        begin
          Word64MultiplyWord8(Res, 10);
          if Word64CompareWord64(Res, Decimal64MaxValueW64) > 0 then
            begin
              Result := dceOverflowError;
              exit;
            end;
          Dec(ResExp);
        end
    else
      while ResExp > 0 do
        begin
          Word64DivideWord8(Res, 10, Res, R);
          if Word64IsZero(Res) then
            break;
          Dec(ResExp);
        end;
  // result
  B.Value64 := Res;
  Result := dceNoError;
end;

function StrToDecimal64(const A: String): Decimal64;
begin
  case TryStrToDecimal64(A, Result) of
    dceNoError       : ;
    dceConvertError  : raise EDecimalError.Create(SConvertError);
    dceOverflowError : raise EDecimalError.Create(SOverflowError);
  end;
end;

function StrToDecimal64B(const A: RawByteString): Decimal64;
begin
  case TryStrToDecimal64B(A, Result) of
    dceNoError       : ;
    dceConvertError  : raise EDecimalError.Create(SConvertError);
    dceOverflowError : raise EDecimalError.Create(SOverflowError);
  end;
end;



{                                                                              }
{ Decimal128                                                                   }
{                                                                              }
procedure Decimal128InitZero(var A: Decimal128);
begin
  Word128InitZero(A.Value128);
end;

procedure Decimal128InitOne(var A: Decimal128);
begin
  Word128InitWord64(A.Value128, Decimal128Scale);
end;

procedure Decimal128InitMax(var A: Decimal128);
begin
  A.Value128 := Decimal128MaxValue;
end;

function Decimal128IsZero(const A: Decimal128): Boolean;
begin
  Result := Word128IsZero(A.Value128);
end;

function Decimal128IsOne(const A: Decimal128): Boolean;
begin
  Result := Word128EqualsWord64(A.Value128, Decimal128Scale);
end;

function Decimal128IsMaximum(const A: Decimal128): Boolean;
begin
  Result := Word128EqualsWord128(A.Value128, Decimal128MaxValue);
end;

function Decimal128IsOverflow(const A: Decimal128): Boolean;
begin
  Result := Word128CompareWord128(A.Value128, Decimal128MaxValue) > 0;
end;

function Word64IsDecimal128Range(const A: Word64Rec): Boolean;
begin
  Result := Word64CompareWord64(A, Decimal128MaxInt) <= 0;
end;

function Word128IsDecimal128Range(const A: Word128): Boolean;
begin
  Result := Word128CompareWord64(A, Decimal128MaxInt) <= 0;
end;

function Int64IsDecimal128Range(const A: Int64): Boolean;
begin
  Result := A >= 0;
end;

function Int128IsDecimal128Range(const A: Int128): Boolean;
begin
  Result :=
    not Int128IsNegative(A) and
    (Int128CompareWord64(A, Decimal128MaxInt) <= 0);
end;

function FloatIsDecimal128Range(const A: Double): Boolean;
begin
  Result :=
    (A >= Decimal128MinFloatD) and
    (A < Decimal128MaxFloatLimD);
end;

procedure Decimal128InitWord8(var A: Decimal128; const B: Byte);
var T : Word128;
begin
  Word128InitWord64(T, Decimal128Scale);
  Word128MultiplyWord8(T, B);
  A.Value128 := T;
end;

procedure Decimal128InitWord16(var A: Decimal128; const B: Word);
var T : Word128;
begin
  Word128InitWord64(T, Decimal128Scale);
  Word128MultiplyWord16(T, B);
  A.Value128 := T;
end;

procedure Decimal128InitWord32(var A: Decimal128; const B: Word32);
var T : Word128;
begin
  Word128InitWord64(T, Decimal128Scale);
  Word128MultiplyWord32(T, B);
  A.Value128 := T;
end;

procedure Decimal128InitWord64(var A: Decimal128; const B: Word64Rec);
var T : Word128;
begin
  if Word64CompareWord64(B, Decimal128MaxInt) > 0 then
    raise EDecimalError.Create(SOverflowError);
  Word128InitWord64(T, Decimal128Scale);
  Word128MultiplyWord64(T, B);
  A.Value128 := T;
end;

procedure Decimal128InitInt32(var A: Decimal128; const B: Int32);
var T : Word128;
begin
  if B < 0 then
    raise EDecimalError.Create(SOverflowError);
  Word128InitWord64(T, Decimal128Scale);
  Word128MultiplyWord32(T, B);
  A.Value128 := T;
end;

procedure Decimal128InitInt64(var A: Decimal128; const B: Int64);
var T : Word128;
    Q : Word64Rec;
begin
  if B < 0 then
    raise EDecimalError.Create(SOverflowError);
  Word128InitWord64(T, Decimal128Scale);
  Word64InitInt64(Q, B);
  Word128MultiplyWord64(T, Q);
  A.Value128 := T;
end;

procedure Decimal128InitDecimal64(var A: Decimal128; const B: Decimal64);
var T, Q : Word128;
    R : Word32;
begin
  Word128InitWord64(T, B.Value64);
  Word128MultiplyWord64(T, Decimal128Scale);
  Word128DivideWord32(T, Decimal64Scale, Q, R);
  A.Value128 := Q;
end;

procedure Decimal128InitDecimal128(var A: Decimal128; const B: Decimal128);
begin
  A.Value128 := B.Value128;
end;

procedure Decimal128InitFloat(var A: Decimal128; const B: Double);
var C, I, F : Double;
    L : Int64;
begin
  if not FloatIsDecimal128Range(B) then
    raise EDecimalError.Create(SOverflowError);
  C := B * Decimal128ScaleF;
  I := Int(C);
  F := Frac(C);
  L := Round(I - (Int(I / 10) * 10));
  if (F > 0.5) or
     ((F = 0.5) and (L and 1 = 1)) then
    I := I + 1.0;
  Word128InitFloat(A.Value128, I);
end;

function Decimal128ToWord8(const A: Decimal128): Byte;
var Q : Word128;
    R : Word64Rec;
begin
  Word128DivideWord64(A.Value128, Decimal128Scale, Q, R);
  if not Word64IsZero(R) then
    raise EDecimalError.Create(SConvertError);
  if not Word128IsWord8Range(Q) then
    raise EDecimalError.Create(SOverflowError);
  Result := Word128ToWord32(Q);
end;

function Decimal128ToWord16(const A: Decimal128): Word;
var Q : Word128;
    R : Word64Rec;
begin
  Word128DivideWord64(A.Value128, Decimal128Scale, Q, R);
  if not Word64IsZero(R) then
    raise EDecimalError.Create(SConvertError);
  if not Word128IsWord16Range(Q) then
    raise EDecimalError.Create(SOverflowError);
  Result := Word128ToWord32(Q);
end;

function Decimal128ToWord32(const A: Decimal128): Word32;
var Q : Word128;
    R : Word64Rec;
begin
  Word128DivideWord64(A.Value128, Decimal128Scale, Q, R);
  if not Word64IsZero(R) then
    raise EDecimalError.Create(SConvertError);
  if not Word128IsWord32Range(Q) then
    raise EDecimalError.Create(SOverflowError);
  Result := Word128ToWord32(Q);
end;

function Decimal128ToWord64(const A: Decimal128): Word64Rec;
var Q : Word128;
    R : Word64Rec;
begin
  Word128DivideWord64(A.Value128, Decimal128Scale, Q, R);
  if not Word64IsZero(R) then
    raise EDecimalError.Create(SConvertError);
  if not Word128IsWord64Range(Q) then
    raise EDecimalError.Create(SOverflowError);
  Result := Word128ToWord64(Q);
end;

function Decimal128ToInt32(const A: Decimal128): Int32;
var Q : Word128;
    R : Word64Rec;
begin
  Word128DivideWord64(A.Value128, Decimal128Scale, Q, R);
  if not Word64IsZero(R) then
    raise EDecimalError.Create(SConvertError);
  if not Word128IsInt32Range(Q) then
    raise EDecimalError.Create(SOverflowError);
  Result := Word128ToInt32(Q);
end;

function Decimal128ToInt64(const A: Decimal128): Int64;
var Q : Word128;
    R : Word64Rec;
begin
  Word128DivideWord64(A.Value128, Decimal128Scale, Q, R);
  if not Word64IsZero(R) then
    raise EDecimalError.Create(SConvertError);
  if not Word128IsInt64Range(Q) then
    raise EDecimalError.Create(SOverflowError);
  Result := Word128ToInt64(Q);
end;

function Decimal128ToFloat(const A: Decimal128): Double;
begin
  Result := Word128ToFloat(A.Value128) / Decimal128ScaleF;
end;

function Decimal128Trunc(const A: Decimal128): Word64Rec;
var Q : Word128;
    R : Word64Rec;
begin
  Word128DivideWord64(A.Value128, Decimal128Scale, Q, R);
  Result := Word128ToWord64(Q);
end;

function Decimal128Round(const A: Decimal128): Word64Rec;
var F : Word64Rec;
    R : Word128;
begin
  Word128DivideWord64(A.Value128, Decimal128Scale, R, F);
  if (Word64CompareWord64(F, Decimal128RoundTerm) > 0) or
     (Word64EqualsWord64(F, Decimal128RoundTerm) and Word128IsOdd(R)) then
    Word128Inc(R);
  Result := Word128ToWord64(R);
end;

function Decimal128FracWord(const A: Decimal128): Word64Rec;
var R : Word128;
    F : Word64Rec;
begin
  Word128DivideWord64(A.Value128, Decimal128Scale, R, F);
  Result := F;
end;

function Decimal128EqualsWord8(const A: Decimal128; const B: Byte): Boolean;
var T : Word128;
begin
  Word128InitWord64(T, Decimal128Scale);
  Word128MultiplyWord8(T, B);
  Result := Word128EqualsWord128(A.Value128, T);
end;

function Decimal128EqualsWord16(const A: Decimal128; const B: Word): Boolean;
var T : Word128;
begin
  Word128InitWord64(T, Decimal128Scale);
  Word128MultiplyWord16(T, B);
  Result := Word128EqualsWord128(A.Value128, T);
end;

function Decimal128EqualsWord32(const A: Decimal128; const B: Word32): Boolean;
var T : Word128;
begin
  Word128InitWord64(T, Decimal128Scale);
  Word128MultiplyWord32(T, B);
  Result := Word128EqualsWord128(A.Value128, T);
end;

function Decimal128EqualsWord64(const A: Decimal128; const B: Word64Rec): Boolean;
var T : Word128;
begin
  Word128InitWord64(T, Decimal128Scale);
  Word128MultiplyWord64(T, B);
  Result := Word128EqualsWord128(A.Value128, T);
end;

function Decimal128EqualsInt32(const A: Decimal128; const B: Int32): Boolean;
begin
  if B < 0 then
    Result := False
  else
    Result := Decimal128EqualsWord32(A, B);
end;

function Decimal128EqualsInt64(const A: Decimal128; const B: Int64): Boolean;
var T : Word64Rec;
begin
  if B < 0 then
    Result := False
  else
    begin
      Word64InitInt64(T, B);
      Result := Decimal128EqualsWord64(A, T);
    end;
end;

function Decimal128EqualsDecimal128(const A: Decimal128; const B: Decimal128): Boolean;
begin
  Result := Word128EqualsWord128(A.Value128, B.Value128);
end;

function Decimal128EqualsFloat(const A: Decimal128; const B: Double): Boolean;
var T : Decimal128;
begin
  if not FloatIsDecimal128Range(B) then
    Result := False
  else
    begin
      Decimal128InitFloat(T, B);
      Result := Word128EqualsWord128(T.Value128, A.Value128);
    end;
end;

function Decimal128CompareWord8(const A: Decimal128; const B: Byte): Integer;
var T : Word128;
begin
  Word128InitWord64(T, Decimal128Scale);
  Word128MultiplyWord8(T, B);
  Result := Word128CompareWord128(A.Value128, T);
end;

function Decimal128CompareWord16(const A: Decimal128; const B: Word): Integer;
var T : Word128;
begin
  Word128InitWord64(T, Decimal128Scale);
  Word128MultiplyWord16(T, B);
  Result := Word128CompareWord128(A.Value128, T);
end;

function Decimal128CompareWord32(const A: Decimal128; const B: Word32): Integer;
var T : Word128;
begin
  Word128InitWord64(T, Decimal128Scale);
  Word128MultiplyWord32(T, B);
  Result := Word128CompareWord128(A.Value128, T);
end;

function Decimal128CompareWord64(const A: Decimal128; const B: Word64Rec): Integer;
var T : Word128;
begin
  Word128InitWord64(T, Decimal128Scale);
  Word128MultiplyWord64(T, B);
  Result := Word128CompareWord128(A.Value128, T);
end;

function Decimal128CompareDecimal128(const A: Decimal128; const B: Decimal128): Integer;
begin
  Result := Word128CompareWord128(A.Value128, B.Value128);
end;

function Decimal128CompareFloat(const A: Decimal128; const B: Double): Integer;
var T : Decimal128;
begin
  if B < Decimal128MinFloatD then
    Result := 1
  else
  if B >= Decimal128MaxFloatLimD then
    Result := -1
  else
    begin
      Assert(FloatIsDecimal128Range(B));
      Decimal128InitFloat(T, B);
      Result := Word128CompareWord128(A.Value128, T.Value128);
    end;
end;

procedure Decimal128AddWord8(var A: Decimal128; const B: Byte);
var T, Q : Word128;
begin
  T := A.Value128;
  Word128InitWord32(Q, B);
  Word128MultiplyWord64(Q, Decimal128Scale);
  Word128AddWord128(T, Q);
  if Word128CompareWord128(T, Decimal128MaxValue) > 0 then
    raise EDecimalError.Create(SOverflowError);
  A.Value128 := T;
end;

procedure Decimal128AddWord16(var A: Decimal128; const B: Word);
var T, Q : Word128;
begin
  T := A.Value128;
  Word128InitWord32(Q, B);
  Word128MultiplyWord64(Q, Decimal128Scale);
  Word128AddWord128(T, Q);
  if Word128CompareWord128(T, Decimal128MaxValue) > 0 then
    raise EDecimalError.Create(SOverflowError);
  A.Value128 := T;
end;

procedure Decimal128AddWord32(var A: Decimal128; const B: Word32);
var T, Q : Word128;
begin
  T := A.Value128;
  Word128InitWord32(Q, B);
  Word128MultiplyWord64(Q, Decimal128Scale);
  Word128AddWord128(T, Q);
  if Word128CompareWord128(T, Decimal128MaxValue) > 0 then
    raise EDecimalError.Create(SOverflowError);
  A.Value128 := T;
end;

procedure Decimal128AddWord64(var A: Decimal128; const B: Word64Rec);
var T, Q : Word128;
begin
  T := A.Value128;
  Word128InitWord64(Q, B);
  Word128MultiplyWord64(Q, Decimal128Scale);
  Word128AddWord128(T, Q);
  if Word128CompareWord128(T, Decimal128MaxValue) > 0 then
    raise EDecimalError.Create(SOverflowError);
  A.Value128 := T;
end;

procedure Decimal128AddDecimal128(var A: Decimal128; const B: Decimal128);
var T : Word256;
begin
  Word256InitWord128(T, A.Value128);
  Word256AddWord128(T, B.Value128);
  if Word256CompareWord128(T, Decimal128MaxValue) > 0 then
    raise EDecimalError.Create(SOverflowError);
  A.Value128 := Word256ToWord128(T);
end;

procedure Decimal128SubtractWord8(var A: Decimal128; const B: Byte);
var T, Q : Word128;
begin
  T := A.Value128;
  Word128InitWord32(Q, B);
  Word128MultiplyWord64(Q, Decimal128Scale);
  if Word128CompareWord128(T, Q) < 0 then
    raise EDecimalError.Create(SOverflowError);
  Word128SubtractWord128(T, Q);
  A.Value128 := T;
end;

procedure Decimal128SubtractWord16(var A: Decimal128; const B: Word);
var T, Q : Word128;
begin
  T := A.Value128;
  Word128InitWord32(Q, B);
  Word128MultiplyWord64(Q, Decimal128Scale);
  if Word128CompareWord128(T, Q) < 0 then
    raise EDecimalError.Create(SOverflowError);
  Word128SubtractWord128(T, Q);
  A.Value128 := T;
end;

procedure Decimal128SubtractWord32(var A: Decimal128; const B: Word32);
var T, Q : Word128;
begin
  T := A.Value128;
  Word128InitWord32(Q, B);
  Word128MultiplyWord64(Q, Decimal128Scale);
  if Word128CompareWord128(T, Q) < 0 then
    raise EDecimalError.Create(SOverflowError);
  Word128SubtractWord128(T, Q);
  A.Value128 := T;
end;

procedure Decimal128SubtractWord64(var A: Decimal128; const B: Word64Rec);
var T, Q : Word128;
begin
  T := A.Value128;
  Word128InitWord64(Q, B);
  Word128MultiplyWord64(Q, Decimal128Scale);
  if Word128CompareWord128(T, Q) < 0 then
    raise EDecimalError.Create(SOverflowError);
  Word128SubtractWord128(T, Q);
  A.Value128 := T;
end;

procedure Decimal128SubtractDecimal128(var A: Decimal128; const B: Decimal128);
begin
  if Word128CompareWord128(A.Value128, B.Value128) < 0 then
    raise EDecimalError.Create(SOverflowError);
  Word128SubtractWord128(A.Value128, B.Value128);
end;

procedure Decimal128MultiplyWord8(var A: Decimal128; const B: Byte);
var T : Word256;
begin
  Word256InitWord128(T, A.Value128);
  Word256MultiplyWord8(T, B);
  if Word256CompareWord128(T, Decimal128MaxValue) > 0 then
    raise EDecimalError.Create(SOverflowError);
  A.Value128 := Word256ToWord128(T);
end;

procedure Decimal128MultiplyWord16(var A: Decimal128; const B: Word);
var T : Word256;
begin
  Word256InitWord128(T, A.Value128);
  Word256MultiplyWord16(T, B);
  if Word256CompareWord128(T, Decimal128MaxValue) > 0 then
    raise EDecimalError.Create(SOverflowError);
  A.Value128 := Word256ToWord128(T);
end;

procedure Decimal128MultiplyWord32(var A: Decimal128; const B: Word32);
var T : Word256;
begin
  Word256InitWord128(T, A.Value128);
  Word256MultiplyWord32(T, B);
  if Word256CompareWord128(T, Decimal128MaxValue) > 0 then
    raise EDecimalError.Create(SOverflowError);
  A.Value128 := Word256ToWord128(T);
end;

procedure Decimal128MultiplyWord64(var A: Decimal128; const B: Word64Rec);
var T : Word256;
begin
  Word256InitWord128(T, A.Value128);
  Word256MultiplyWord64(T, B);
  if Word256CompareWord128(T, Decimal128MaxValue) > 0 then
    raise EDecimalError.Create(SOverflowError);
  A.Value128 := Word256ToWord128(T);
end;

procedure Decimal128MultiplyDecimal128(var A: Decimal128; const B: Decimal128);
var T, Q : Word256;
    R : Word128;
begin
  Word256InitWord128(T, A.Value128);
  Word256MultiplyWord128(T, B.Value128);
  Word256AddWord64(T, Decimal128RoundTerm);
  Word256DivideWord128(T, Decimal128ScaleW128, Q, R);
  if Word256CompareWord128(Q, Decimal128MaxValue) > 0 then
    raise EDecimalError.Create(SOverflowError);
  A.Value128 := Word256ToWord128(Q);
end;

procedure Decimal128Sqr(var A: Decimal128);
begin
  Decimal128MultiplyDecimal128(A, A);
end;

procedure Decimal128DivideWord8(var A: Decimal128; const B: Byte);
var Q : Word128;
    R : Byte;
begin
  Word128DivideWord8(A.Value128, B, Q, R);
  A.Value128 := Q;
end;

procedure Decimal128DivideWord16(var A: Decimal128; const B: Word);
var Q : Word128;
    R : Word;
begin
  Word128DivideWord16(A.Value128, B, Q, R);
  A.Value128 := Q;
end;

procedure Decimal128DivideWord32(var A: Decimal128; const B: Word32);
var Q : Word128;
    R : Word32;
begin
  Word128DivideWord32(A.Value128, B, Q, R);
  A.Value128 := Q;
end;

procedure Decimal128DivideWord64(var A: Decimal128; const B: Word64Rec);
var Q : Word128;
    R : Word64Rec;
begin
  Word128DivideWord64(A.Value128, B, Q, R);
  A.Value128 := Q;
end;

procedure Decimal128DivideDecimal128(var A: Decimal128; const B: Decimal128);
var T : Word256;
    Q : Word128;
    F : Word256;
    G : Word128;
begin
  Word256InitWord128(T, A.Value128);
  Word128InitWord64(Q, Decimal128Scale);
  Word256MultiplyWord128(T, Q);
  Word256DivideWord128(T, B.Value128, F, G);
  A.Value128 := Word256ToWord128(F);
end;

function Decimal128ToStr(const A: Decimal128): String;
var
  S : String;
  L : Integer;
begin
  if Word128IsZero(A.Value128) then
    begin
      Result := '0.0000000000000000000';
      exit;
    end;
  S := StrPadLeft(Word128ToStr(A.Value128), '0', Decimal128Precision + 1, False);
  L := Length(S);
  Result :=
    CopyLeft(S, L - Decimal128Precision) + '.' +
    CopyRight(S, Decimal128Precision);
end;

function Decimal128ToStrB(const A: Decimal128): RawByteString;
var
  S : RawByteString;
  L : Integer;
begin
  if Word128IsZero(A.Value128) then
    begin
      Result := '0.0000000000000000000';
      exit;
    end;
  S := StrPadLeftB(Word128ToStrB(A.Value128), '0', Decimal128Precision + 1, False);
  L := Length(S);
  Result :=
    CopyLeftB(S, L - Decimal128Precision) + '.' +
    CopyRightB(S, Decimal128Precision);
end;

function Decimal128ToStrU(const A: Decimal128): UnicodeString;
var
  S : UnicodeString;
  L : Integer;
begin
  if Word128IsZero(A.Value128) then
    begin
      Result := '0.0000000000000000000';
      exit;
    end;
  S := StrPadLeftU(Word128ToStrU(A.Value128), '0', Decimal128Precision + 1, False);
  L := Length(S);
  Result :=
    CopyLeftU(S, L - Decimal128Precision) + '.' +
    CopyRightU(S, Decimal128Precision);
end;

function TryStrToDecimal128(const A: String; out B: Decimal128): TDecimalConvertErrorType;
var
  ResInt : Word128;
  ResFrac : Word128;
  Res : Word128;
  I, L : Integer;
  Ch : Char;
  Digit : Integer;
  C, D : Word64Rec;
  E : Byte;
  T : Word128;
  RoundingDigit : Integer;
begin
  L := Length(A);
  if L = 0 then
    begin
      Result := dceConvertError;
      exit;
    end;
  // integer part
  Word128InitZero(ResInt);
  I := 1;
  while I <= L do
    begin
      Ch := A[I];
      if Ch = '.' then
        if I = 1 then
          begin
            Result := dceConvertError;
            exit;
          end
        else
          begin
            if I = L then
              begin
                Result := dceConvertError;
                exit;
              end;
            Inc(I);
            break;
          end;
      Digit := CharDigitToInt(Ch);
      if Digit < 0 then
        begin
          Result := dceConvertError;
          exit;
        end;
      Word128MultiplyWord8(ResInt, 10);
      Word128AddWord8(ResInt, Digit);
      if Word128CompareWord64(ResInt, Decimal128MaxInt) > 0 then
        begin
          Result := dceOverflowError;
          exit;
        end;
      Inc(I);
    end;
  // fraction part
  Word128InitZero(ResFrac);
  Word64DivideWord8(Decimal128Scale, 10, C, E);
  while I <= L do
    begin
      Digit := CharDigitToInt(A[I]);
      Inc(I);
      if Digit < 0 then
        begin
          Result := dceConvertError;
          exit;
        end;
      Word128InitWord64(T, C);
      Word128MultiplyWord8(T, Byte(Digit));
      Word128AddWord128(ResFrac, T);
      Word64DivideWord8(C, 10, D, E);
      C := D;
      if Word64IsZero(C) then
        break;
    end;
  // rounding
  if I <= L then
    begin
      RoundingDigit := CharDigitToInt(A[I]);
      Inc(I);
      if RoundingDigit < 0 then
        begin
          Result := dceConvertError;
          exit;
        end;
      if RoundingDigit = 5 then
        while I <= L do
          begin
            Digit := CharDigitToInt(A[I]);
            Inc(I);
            if Digit < 0 then
              begin
                Result := dceConvertError;
                exit;
              end;
            if Digit > 0 then
              begin
                RoundingDigit := 6;
                break;
              end;
          end;
    end
  else
    RoundingDigit := 0;
  // validate remaining digits
  while I <= L do
    begin
      Digit := CharDigitToInt(A[I]);
      Inc(I);
      if Digit < 0 then
        begin
          Result := dceConvertError;
          exit;
        end;
    end;
  // combine integer and fractional parts
  Res := ResInt;
  Word128MultiplyWord64(Res, Decimal128Scale);
  Word128AddWord128(Res, ResFrac);
  // round
  if (RoundingDigit > 5) or
     ((RoundingDigit = 5) and Word128IsOdd(Res)) then
    begin
      Word128Inc(Res);
      if Word128CompareWord128(Res, Decimal128MaxValue) > 0 then
        begin
          Result := dceOverflowError;
          exit;
        end;
    end;
  // result
  B.Value128 := Res;
  Result := dceNoError;
end;

function TryStrToDecimal128B(const A: RawByteString; out B: Decimal128): TDecimalConvertErrorType;
var
  ResInt : Word128;
  ResFrac : Word128;
  Res : Word128;
  I, L : Integer;
  Ch : AnsiChar;
  Digit : Integer;
  C, D : Word64Rec;
  E : Byte;
  T : Word128;
  RoundingDigit : Integer;
begin
  L := Length(A);
  if L = 0 then
    begin
      Result := dceConvertError;
      exit;
    end;
  // integer part
  Word128InitZero(ResInt);
  I := 1;
  while I <= L do
    begin
      Ch := A[I];
      if Ch = '.' then
        if I = 1 then
          begin
            Result := dceConvertError;
            exit;
          end
        else
          begin
            if I = L then
              begin
                Result := dceConvertError;
                exit;
              end;
            Inc(I);
            break;
          end;
      Digit := ByteCharDigitToInt(Ch);
      if Digit < 0 then
        begin
          Result := dceConvertError;
          exit;
        end;
      Word128MultiplyWord8(ResInt, 10);
      Word128AddWord8(ResInt, Digit);
      if Word128CompareWord64(ResInt, Decimal128MaxInt) > 0 then
        begin
          Result := dceOverflowError;
          exit;
        end;
      Inc(I);
    end;
  // fraction part
  Word128InitZero(ResFrac);
  Word64DivideWord8(Decimal128Scale, 10, C, E);
  while I <= L do
    begin
      Digit := ByteCharDigitToInt(A[I]);
      Inc(I);
      if Digit < 0 then
        begin
          Result := dceConvertError;
          exit;
        end;
      Word128InitWord64(T, C);
      Word128MultiplyWord8(T, Byte(Digit));
      Word128AddWord128(ResFrac, T);
      Word64DivideWord8(C, 10, D, E);
      C := D;
      if Word64IsZero(C) then
        break;
    end;
  // rounding
  if I <= L then
    begin
      RoundingDigit := ByteCharDigitToInt(A[I]);
      Inc(I);
      if RoundingDigit < 0 then
        begin
          Result := dceConvertError;
          exit;
        end;
      if RoundingDigit = 5 then
        while I <= L do
          begin
            Digit := ByteCharDigitToInt(A[I]);
            Inc(I);
            if Digit < 0 then
              begin
                Result := dceConvertError;
                exit;
              end;
            if Digit > 0 then
              begin
                RoundingDigit := 6;
                break;
              end;
          end;
    end
  else
    RoundingDigit := 0;
  // validate remaining digits
  while I <= L do
    begin
      Digit := ByteCharDigitToInt(A[I]);
      Inc(I);
      if Digit < 0 then
        begin
          Result := dceConvertError;
          exit;
        end;
    end;
  // combine integer and fractional parts
  Res := ResInt;
  Word128MultiplyWord64(Res, Decimal128Scale);
  Word128AddWord128(Res, ResFrac);
  // round
  if (RoundingDigit > 5) or
     ((RoundingDigit = 5) and Word128IsOdd(Res)) then
    begin
      Word128Inc(Res);
      if Word128CompareWord128(Res, Decimal128MaxValue) > 0 then
        begin
          Result := dceOverflowError;
          exit;
        end;
    end;
  // result
  B.Value128 := Res;
  Result := dceNoError;
end;

function StrToDecimal128(const A: String): Decimal128;
begin
  case TryStrToDecimal128(A, Result) of
    dceNoError       : ;
    dceConvertError  : raise EDecimalError.Create(SConvertError);
    dceOverflowError : raise EDecimalError.Create(SOverflowError);
  end;
end;

function StrToDecimal128B(const A: RawByteString): Decimal128;
begin
  case TryStrToDecimal128B(A, Result) of
    dceNoError       : ;
    dceConvertError  : raise EDecimalError.Create(SConvertError);
    dceOverflowError : raise EDecimalError.Create(SOverflowError);
  end;
end;



{                                                                              }
{ HugeDecimal                                                                  }
{                                                                              }

// Returns number of bytes required to encode value B
function HugeDecimalEncLength32(const B: Word32): Integer;
var
  L : Word32;
  C : Integer;
begin
  if B = 0 then
    Result := 0
  else
    begin
      L := B;
      C := 0;
      repeat
        L := L div 10;
        Inc(C);
      until L = 0;
      Result := C;
    end;
end;

// Returns number of bytes required to encode value B
function HugeDecimalEncLength64(const B: Word64Rec): Integer;
var
  L : Word64Rec;
  R : Byte;
  C : Integer;
begin
  if Word64IsZero(B) then
    Result := 0
  else
    begin
      L := B;
      C := 0;
      repeat
        Word64DivideWord8(L, 10, L, R);
        Inc(C);
      until Word64IsZero(L);
      Result := C;
    end;
end;

// Returns number of bytes required to encode value B
function HugeDecimalEncLength128(const B: Word128): Integer;
var
  L : Word128;
  R : Byte;
  C : Integer;
begin
  if Word128IsZero(B) then
    Result := 0
  else
    begin
      L := B;
      C := 0;
      repeat
        Word128DivideWord8(L, 10, L, R);
        Inc(C);
      until Word128IsZero(L);
      Result := C;
    end;
end;

// Pre: A not initialised
// Post: A = 0
procedure HugeDecimalInit(out A: HugeDecimal);
begin
  FillChar(A, SizeOf(HugeDecimal), 0);
end;

// Pre: A initialised
procedure HugeDecimalFinalise(var A: HugeDecimal);
begin
  A.Digits := nil;
end;

// Pre: A not initialised
// Post: A = 0
procedure HugeDecimalInitZero(out A: HugeDecimal);
begin
  HugeDecimalInit(A);
end;

// Pre: A not initialised
// Post: A = 1
procedure HugeDecimalInitOne(out A: HugeDecimal);
begin
  HugeDecimalInit(A);
  HugeDecimalAssignOne(A);
end;

// Pre: A not initialised
procedure HugeDecimalInitWord8(out A: HugeDecimal; const B: Byte);
begin
  HugeDecimalInit(A);
  HugeDecimalAssignWord8(A, B);
end;

// Pre: A not initialised
procedure HugeDecimalInitWord32(out A: HugeDecimal; const B: Word32);
begin
  HugeDecimalInit(A);
  HugeDecimalAssignWord32(A, B);
end;

// Pre: A not initialised
procedure HugeDecimalInitWord64(out A: HugeDecimal; const B: Word64Rec);
begin
  HugeDecimalInit(A);
  HugeDecimalAssignWord64(A, B);
end;

// Pre: A not initialised
procedure HugeDecimalInitWord128(out A: HugeDecimal; const B: Word128);
begin
  HugeDecimalInit(A);
  HugeDecimalAssignWord128(A, B);
end;

// Pre: A not initialised
procedure HugeDecimalInitDecimal32(out A: HugeDecimal; const B: Decimal32);
begin
  HugeDecimalInit(A);
  HugeDecimalAssignDecimal32(A, B);
end;

// Pre: A not initialised
procedure HugeDecimalInitDecimal64(out A: HugeDecimal; const B: Decimal64);
begin
  HugeDecimalInit(A);
  HugeDecimalAssignDecimal64(A, B);
end;

// Pre: A not initialised
procedure HugeDecimalInitDecimal128(out A: HugeDecimal; const B: Decimal128);
begin
  HugeDecimalInit(A);
  HugeDecimalAssignDecimal128(A, B);
end;

// Pre: A not initialised
//      B initialised
procedure HugeDecimalInitHugeDecimal(out A: HugeDecimal; const B: HugeDecimal);
begin
  HugeDecimalInit(A);
  HugeDecimalAssignHugeDecimal(A, B);
end;

// Pre: A initialised
// Post: A normalised
//       A = 0
procedure HugeDecimalAssignZero(var A: HugeDecimal);
begin
  A.Precision := 0;
  SetLength(A.Digits, 0);
end;

// Pre: A initialised
// Post: A normalised
//       A = 1
procedure HugeDecimalAssignOne(var A: HugeDecimal);
begin
  A.Precision := 0;
  SetLength(A.Digits, 1);
  A.Digits[0] := 1;
end;

// Pre: A initialised
// Post: A normalised
procedure HugeDecimalAssignWord8(var A: HugeDecimal; const B: Byte);
begin
  A.Precision := 0;
  if B = 0 then
    SetLength(A.Digits, 0)
  else
  if B < 10 then
    begin
      SetLength(A.Digits, 1);
      A.Digits[0] := B;
    end
  else
  if B < 100 then
    begin
      SetLength(A.Digits, 2);
      A.Digits[0] := B mod 10;
      A.Digits[1] := B div 10;
    end
  else
    begin
      SetLength(A.Digits, 3);
      A.Digits[0] := B mod 10;
      A.Digits[1] := (B div 10) mod 10;
      A.Digits[2] := B div 100;
    end;
end;

// Pre: A initialised
// Post: A normalised
procedure HugeDecimalAssignWord32(var A: HugeDecimal; const B: Word32);
var
  L : Word32;
  I : Integer;
begin
  A.Precision := 0;
  if B = 0 then
    SetLength(A.Digits, 0)
  else
    begin
      SetLength(A.Digits, HugeDecimalEncLength32(B));
      L := B;
      I := 0;
      repeat
        A.Digits[I] := L mod 10;
        L := L div 10;
        Inc(I);
      until L = 0;
    end;
end;

// Pre: A initialised
// Post: A normalised
procedure HugeDecimalAssignWord64(var A: HugeDecimal; const B: Word64Rec);
var
  L : Word64Rec;
  R : Byte;
  I : Integer;
begin
  A.Precision := 0;
  if Word64IsZero(B) then
    SetLength(A.Digits, 0)
  else
    begin
      SetLength(A.Digits, HugeDecimalEncLength64(B));
      L := B;
      I := 0;
      repeat
        Word64DivideWord8(L, 10, L, R);
        A.Digits[I] := R;
        Inc(I);
      until Word64IsZero(L);
    end;
end;

// Pre: A initialised
// Post: A normalised
procedure HugeDecimalAssignWord128(var A: HugeDecimal; const B: Word128);
var
  L : Word128;
  R : Byte;
  I : Integer;
begin
  A.Precision := 0;
  if Word128IsZero(B) then
    SetLength(A.Digits, 0)
  else
    begin
      SetLength(A.Digits, HugeDecimalEncLength128(B));
      L := B;
      I := 0;
      repeat
        Word128DivideWord8(L, 10, L, R);
        A.Digits[I] := R;
        Inc(I);
      until Word128IsZero(L);
    end;
end;

// Post: A normalised
procedure HugeDecimalAssignDecimal32(var A: HugeDecimal; const B: Decimal32);
var
  Val32 : Word32;
  DigitCnt, DigitIdx : Integer;
  Digit : Byte;
const
  MaxDigits = Decimal32Digits;
  Precision = Decimal32Precision;
begin
  A.Precision := 0;
  Val32 := B.Value32;
  if Val32 = 0 then
    begin
      SetLength(A.Digits, 0);
      exit;
    end;
  SetLength(A.Digits, MaxDigits);
  DigitIdx := 0;
  // skip trailing decimal zeros
  Digit := Val32 mod 10;
  Val32 := Val32 div 10;
  DigitCnt := 1;
  while Digit = 0 do
    begin
      Digit := Val32 mod 10;
      Val32 := Val32 div 10;
      Inc(DigitCnt);
      if DigitCnt > Precision then
        break;
    end;
  // store significant decimal digits
  while DigitCnt <= Precision do
    begin
      Inc(A.Precision);
      A.Digits[DigitIdx] := Digit;
      Inc(DigitIdx);
      Digit := Val32 mod 10;
      Val32 := Val32 div 10;
      Inc(DigitCnt);
    end;
  // store interger digits
  while (Digit > 0) or (Val32 > 0) do
    begin
      A.Digits[DigitIdx] := Digit;
      Inc(DigitIdx);
      Digit := Val32 mod 10;
      Val32 := Val32 div 10;
    end;
  // set length
  SetLength(A.Digits, DigitIdx);
end;

// Post: A normalised
procedure HugeDecimalAssignDecimal64(var A: HugeDecimal; const B: Decimal64);
var
  Val64 : Word64Rec;
  DigitCnt, DigitIdx : Integer;
  Digit : Byte;
const
  MaxDigits = Decimal64Digits;
  Precision = Decimal64Precision;
begin
  A.Precision := 0;
  Val64 := B.Value64;
  if Word64IsZero(Val64) then
    begin
      SetLength(A.Digits, 0);
      exit;
    end;
  SetLength(A.Digits, MaxDigits);
  DigitIdx := 0;
  // skip trailing decimal zeros
  Word64DivideWord8(Val64, 10, Val64, Digit);
  DigitCnt := 1;
  while Digit = 0 do
    begin
      Word64DivideWord8(Val64, 10, Val64, Digit);
      Inc(DigitCnt);
      if DigitCnt > Precision then
        break;
    end;
  // store significant decimal digits
  while DigitCnt <= Precision do
    begin
      Inc(A.Precision);
      A.Digits[DigitIdx] := Digit;
      Inc(DigitIdx);
      Word64DivideWord8(Val64, 10, Val64, Digit);
      Inc(DigitCnt);
    end;
  // store interger digits
  while (Digit > 0) or not Word64IsZero(Val64) do
    begin
      A.Digits[DigitIdx] := Digit;
      Inc(DigitIdx);
      Word64DivideWord8(Val64, 10, Val64, Digit);
    end;
  // set length
  SetLength(A.Digits, DigitIdx);
end;

// Post: A normalised
procedure HugeDecimalAssignDecimal128(var A: HugeDecimal; const B: Decimal128);
var
  Val128 : Word128;
  DigitCnt, DigitIdx : Integer;
  Digit : Byte;
const
  MaxDigits = Decimal128Digits;
  Precision = Decimal128Precision;
begin
  A.Precision := 0;
  Val128 := B.Value128;
  if Word128IsZero(Val128) then
    begin
      SetLength(A.Digits, 0);
      exit;
    end;
  SetLength(A.Digits, MaxDigits);
  DigitIdx := 0;
  // skip trailing decimal zeros
  Word128DivideWord8(Val128, 10, Val128, Digit);
  DigitCnt := 1;
  while Digit = 0 do
    begin
      Word128DivideWord8(Val128, 10, Val128, Digit);
      Inc(DigitCnt);
      if DigitCnt > Precision then
        break;
    end;
  // store significant decimal digits
  while DigitCnt <= Precision do
    begin
      Inc(A.Precision);
      A.Digits[DigitIdx] := Digit;
      Inc(DigitIdx);
      Word128DivideWord8(Val128, 10, Val128, Digit);
      Inc(DigitCnt);
    end;
  // store interger digits
  while (Digit > 0) or not Word128IsZero(Val128) do
    begin
      A.Digits[DigitIdx] := Digit;
      Inc(DigitIdx);
      Word128DivideWord8(Val128, 10, Val128, Digit);
    end;
  // set length
  SetLength(A.Digits, DigitIdx);
end;

// Pre: A is initialised
procedure HugeDecimalAssignHugeDecimal(var A: HugeDecimal; const B: HugeDecimal);
begin
  A.Precision := B.Precision;
  A.Digits := Copy(B.Digits);
end;

// Pre: A is normalised
function HugeDecimalIsZero(const A: HugeDecimal): Boolean;
begin
  Result := Length(A.Digits) = 0;
end;

// Pre: A is normalised
function HugeDecimalIsOne(const A: HugeDecimal): Boolean;
begin
  Result :=
    (Length(A.Digits) = 1) and
    (A.Precision = 0) and
    (A.Digits[0] = 1);
end;

// Pre: A is normalised
function HugeDecimalIsOdd(const A: HugeDecimal): Boolean;
begin
  Result :=
    (Length(A.Digits) > 0) and
    (A.Precision = 0) and
    (A.Digits[0] and 1 = 1);
end;

// Pre: A is normalised
function HugeDecimalIsEven(const A: HugeDecimal): Boolean;
var L : Integer;
begin
  L := Length(A.Digits);
  if L = 0 then
    Result := True
  else
    Result :=
      (A.Precision = 0) and
      (A.Digits[0] and 1 = 0);
end;

// Pre: A is normalised
function HugeDecimalIsInteger(const A: HugeDecimal): Boolean;
begin
  Result := A.Precision = 0;
end;

// Pre: A is normalised
function HugeDecimalIsLessThanOne(const A: HugeDecimal): Boolean;
begin
  Result := A.Precision = Length(A.Digits);
end;

// Pre: A is normalised
function HugeDecimalIsOneOrGreater(const A: HugeDecimal): Boolean;
begin
  Result := A.Precision < Length(A.Digits);
end;

// Pre: A is normalised
function HugeDecimalIsWord8Range(const A: HugeDecimal): Boolean;
var
  DigitsLen : Integer;
  T : Word16;
begin
  if A.Precision > 0 then
    Result := False
  else
    begin
      DigitsLen := Length(A.Digits);
      if DigitsLen < 3 then
        Result := True
      else
      if DigitsLen > 3 then
        Result := False
      else
        begin
          T := A.Digits[0] +
               A.Digits[1] * 10 +
               A.Digits[2] * 100;
          Result := T <= $00FF;
        end;
    end;
end;

// Pre: A is normalised
function HugeDecimalDigits(const A: HugeDecimal): Integer;
var
  L : Integer;
begin
  L := Length(A.Digits);
  if L = 0 then
    Result := 1
  else
    Result := L;
end;

// Pre: A is normalised
function HugeDecimalIntegerDigits(const A: HugeDecimal): Integer;
begin
  Result := HugeDecimalDigits(A) - A.Precision;
end;

// Pre: A is normalised
function HugeDecimalDecimalDigits(const A: HugeDecimal): Integer;
begin
  Result := A.Precision;
end;

// Pre: A is normalised
function HugeDecimalGetDigit(const A: HugeDecimal; const DigitIdx: Integer): Byte;
var
  DigitsLen, DigitIdxRev : Integer;
  Digit : Byte;
begin
  if DigitIdx < 0 then
    raise EDecimalError.Create(SIndexOutOfRange);
  DigitsLen := Length(A.Digits);
  if DigitsLen = 0 then
    begin
      Result := 0;
      exit;
    end;
  if DigitIdx >= DigitsLen then
    raise EDecimalError.Create(SIndexOutOfRange);
  DigitIdxRev := DigitsLen - DigitIdx - 1;
  Digit := A.Digits[DigitIdxRev];
  Result := Digit;
end;

// Post: A not normalised
procedure HugeDecimalSetDigit(var A: HugeDecimal; const DigitIdx: Integer; const DigitValue: Byte);
var
  TotDigits, DigitsLen, DigitIdxRev : Integer;
begin
  if DigitValue > 9 then
    raise EDecimalError.Create(SInvalidParameter);
  if DigitIdx < 0 then
    raise EDecimalError.Create(SIndexOutOfRange);
  TotDigits := HugeDecimalDigits(A);
  if DigitIdx >= TotDigits then
    raise EDecimalError.Create(SIndexOutOfRange);
  DigitsLen := Length(A.Digits);
  if DigitsLen = 0 then
    begin
      if DigitValue = 0 then
        exit;
      SetLength(A.Digits, 1);
      A.Digits[0] := DigitValue;
      exit;
    end;
  DigitIdxRev := TotDigits - DigitIdx - 1;
  A.Digits[DigitIdxRev] := DigitValue;
end;

// Pre: A is normalised
function HugeDecimalToWord8(const A: HugeDecimal): Byte;
var
  TotDigits : Integer;
  T : Word;
begin
  if A.Precision > 0 then
    raise EDecimalError.Create(SConvertError);
  TotDigits := Length(A.Digits);
  if TotDigits = 0 then
    Result := 0
  else
  if TotDigits = 1 then
    Result := A.Digits[0]
  else
  if TotDigits = 2 then
    Result := A.Digits[0] + A.Digits[1] * 10
  else
  if TotDigits = 3 then
    begin
      T := A.Digits[0] +
           A.Digits[1] * 10 +
           A.Digits[2] * 100;
      if T > 255 then
        raise EDecimalError.Create(SOverflowError);
      Result := T;
    end
  else
    raise EDecimalError.Create(SOverflowError);
end;

// Pre: A is normalised
function HugeDecimalToWord32(const A: HugeDecimal): Word32;
var
  DigitsLen, DigIdx : Integer;
  T : Word64Rec;
  Digit : Byte;
begin
  if A.Precision > 0 then
    raise EDecimalError.Create(SConvertError);
  DigitsLen := Length(A.Digits);
  if DigitsLen = 0 then
    Result := 0
  else
    begin
      Word64InitZero(T);
      for DigIdx := 0 to DigitsLen - 1 do
        begin
          if DigIdx > 0 then
            Word64MultiplyWord8(T, 10);
          Digit := A.Digits[DigitsLen - DigIdx - 1];
          Word64AddWord8(T, Digit);
          if Word64CompareWord32(T, $FFFFFFFF) > 0 then
            raise EDecimalError.Create(SOverflowError);
        end;
      Result := Word64ToWord32(T);
    end;
end;

// Pre: A is normalised
function HugeDecimalToWord64(const A: HugeDecimal): Word64Rec;
var
  DigitsLen, DigIdx : Integer;
  T : Word128;
  Digit : Byte;
begin
  if A.Precision > 0 then
    raise EDecimalError.Create(SConvertError);
  DigitsLen := Length(A.Digits);
  if DigitsLen = 0 then
    Word64InitZero(Result)
  else
    begin
      Word128InitZero(T);
      for DigIdx := 0 to DigitsLen - 1 do
        begin
          if DigIdx > 0 then
            Word128MultiplyWord8(T, 10);
          Digit := A.Digits[DigitsLen - DigIdx - 1];
          Word128AddWord8(T, Digit);
          if Word128CompareWord64(T, Word64ConstMaximum) > 0 then
            raise EDecimalError.Create(SOverflowError);
        end;
      Result := Word128ToWord64(T);
    end;
end;

// Pre: A is normalised
function HugeDecimalToWord128(const A: HugeDecimal): Word128;
var
  DigitsLen, DigIdx : Integer;
  T : Word256;
  Digit : Byte;
begin
  if A.Precision > 0 then
    raise EDecimalError.Create(SConvertError);
  DigitsLen := Length(A.Digits);
  if DigitsLen = 0 then
    Word128InitZero(Result)
  else
    begin
      Word256InitZero(T);
      for DigIdx := 0 to DigitsLen - 1 do
        begin
          if DigIdx > 0 then
            Word256MultiplyWord8(T, 10);
          Digit := A.Digits[DigitsLen - DigIdx - 1];
          Word256AddWord32(T, Digit);
          if Word256CompareWord128(T, Word128ConstMaximum) > 0 then
            raise EDecimalError.Create(SOverflowError);
        end;
      Result := Word256ToWord128(T);
    end;
end;

// Pre: A is normalised
function HugeDecimalToDecimal32(const A: HugeDecimal): Decimal32;
var
  Digit : Byte;
  DigitIdx, DigitsLen, IntDigits : Integer;
  Val32 : Word32;
begin
  DigitsLen := Length(A.Digits);
  if DigitsLen = 0 then
    begin
      Decimal32InitZero(Result);
      exit;
    end;
  IntDigits := DigitsLen - A.Precision;
  if IntDigits > Decimal32Digits - Decimal32Precision then
    raise EDecimalError.Create(SOverflowError);
  Val32 := 0;
  DigitIdx := 0;
  while DigitIdx < DigitsLen do
    begin
      Digit := A.Digits[DigitsLen - DigitIdx - 1];
      Inc(DigitIdx);
      Val32 := Val32 * 10 + Digit;
      if DigitIdx = DigitsLen then
        break;
      if DigitIdx = IntDigits + Decimal32Precision then
        break;
    end;
  while DigitIdx < IntDigits + Decimal32Precision do
    begin
      Val32 := Val32 * 10;
      Inc(DigitIdx);
    end;
  Result.Value32 := Val32;
end;

// Pre: A is normalised
function HugeDecimalToDecimal64(const A: HugeDecimal): Decimal64;
var
  Digit : Byte;
  DigitIdx, DigitsLen, IntDigits : Integer;
  Val64 : Word64Rec;
begin
  DigitsLen := Length(A.Digits);
  if DigitsLen = 0 then
    begin
      Decimal64InitZero(Result);
      exit;
    end;
  IntDigits := DigitsLen - A.Precision;
  if IntDigits > Decimal64Digits - Decimal64Precision then
    raise EDecimalError.Create(SOverflowError);
  Word64InitZero(Val64);
  DigitIdx := 0;
  while DigitIdx < DigitsLen do
    begin
      Digit := A.Digits[DigitsLen - DigitIdx - 1];
      Inc(DigitIdx);
      Word64MultiplyWord8(Val64, 10);
      Word64AddWord8(Val64, Digit);
      if DigitIdx = DigitsLen then
        break;
      if DigitIdx = IntDigits + Decimal64Precision then
        break;
    end;
  while DigitIdx < IntDigits + Decimal64Precision do
    begin
      Word64MultiplyWord8(Val64, 10);
      Inc(DigitIdx);
    end;
  Result.Value64 := Val64;
end;

// Pre: A is normalised
function HugeDecimalToDecimal128(const A: HugeDecimal): Decimal128;
var
  Digit : Byte;
  DigitIdx, DigitsLen, IntDigits : Integer;
  Val128 : Word128;
begin
  DigitsLen := Length(A.Digits);
  if DigitsLen = 0 then
    begin
      Decimal128InitZero(Result);
      exit;
    end;
  IntDigits := DigitsLen - A.Precision;
  if IntDigits > Decimal128Digits - Decimal128Precision then
    raise EDecimalError.Create(SOverflowError);
  Word128InitZero(Val128);
  DigitIdx := 0;
  while DigitIdx < DigitsLen do
    begin
      Digit := A.Digits[DigitsLen - DigitIdx - 1];
      Inc(DigitIdx);
      Word128MultiplyWord8(Val128, 10);
      Word128AddWord8(Val128, Digit);
      if DigitIdx = DigitsLen then
        break;
      if DigitIdx = IntDigits + Decimal128Precision then
        break;
    end;
  while DigitIdx < IntDigits + Decimal128Precision do
    begin
      Word128MultiplyWord8(Val128, 10);
      Inc(DigitIdx);
    end;
  Result.Value128 := Val128;
end;

// Pre: A is normalised
// Post: A is normalised
procedure HugeDecimalMul10(var A: HugeDecimal);
var
  DigitsLen, DecPrecision, DigitIdx : Integer;
begin
  DigitsLen := Length(A.Digits);
  if DigitsLen = 0 then
    exit;
  DecPrecision := A.Precision;
  if DecPrecision > 0 then
    begin
      A.Precision := DecPrecision - 1;
      if A.Digits[DigitsLen - 1] = 0 then
        SetLength(A.Digits, DigitsLen - 1);
    end
  else
    begin
      Inc(DigitsLen);
      SetLength(A.Digits, DigitsLen);
      for DigitIdx := DigitsLen - 1 downto 1 do
        A.Digits[DigitIdx] := A.Digits[DigitIdx - 1];
      A.Digits[0] := 0;
    end;
end;

// Pre: A is normalised
// Post: A is normalised
procedure HugeDecimalDiv10(var A: HugeDecimal);
var
  DigitsLen, DecPrecision, DigitIdx : Integer;
begin
  DigitsLen := Length(A.Digits);
  if DigitsLen = 0 then
    exit;
  DecPrecision := A.Precision;
  if DecPrecision > 0 then
    begin
      Inc(DecPrecision);
      A.Precision := DecPrecision;
      if DecPrecision > DigitsLen then
        begin
          Inc(DigitsLen);
          SetLength(A.Digits, DigitsLen);
          A.Digits[DigitsLen - 1] := 0;
        end;
    end
  else
  if A.Digits[0] = 0 then
    begin
      for DigitIdx := 0 to DigitsLen - 2 do
        A.Digits[DigitIdx] := A.Digits[DigitIdx + 1];
      Dec(DigitsLen);
      SetLength(A.Digits, DigitsLen);
    end
  else
    begin
      Inc(DecPrecision);
      A.Precision := DecPrecision;
    end;
end;

// Post: A normalised
procedure HugeDecimalInternalNormalise(var A: HugeDecimal);
var
  DigitsLen, ZeroCnt, DigitIdx : Integer;
begin
  DigitsLen := Length(A.Digits);
  // trim zeros preceding integer
  ZeroCnt := 0;
  while ZeroCnt < DigitsLen - A.Precision do
    begin
      if A.Digits[DigitsLen - ZeroCnt - 1] <> 0 then
        break;
      Inc(ZeroCnt);
    end;
  if ZeroCnt > 0 then
    begin
      Dec(DigitsLen, ZeroCnt);
      SetLength(A.Digits, DigitsLen);
    end;
  // trim zeros trailing fraction
  ZeroCnt := 0;
  while ZeroCnt < A.Precision do
    begin
      if A.Digits[ZeroCnt] <> 0 then
        break;
      Inc(ZeroCnt);
    end;
  if ZeroCnt > 0 then
    begin
      for DigitIdx := 0 to DigitsLen - ZeroCnt - 1 do
        A.Digits[DigitIdx] := A.Digits[DigitIdx + ZeroCnt];
      Dec(DigitsLen, ZeroCnt);
      SetLength(A.Digits, DigitsLen);
      Dec(A.Precision, ZeroCnt);
    end;
end;

procedure HugeDecimalInternalAddDigit(var A: HugeDecimal; const StartDigitIdx: Integer;
          const DigitValue: Byte);
var
  DigitsLen, DigitIdx : Integer;
  Digit, AddValue : Byte;
begin
  Assert(StartDigitIdx >= 0);
  Assert(StartDigitIdx < Length(A.Digits));
  Assert(DigitValue <= 9);

  if DigitValue = 0 then
    exit;
  DigitsLen := Length(A.Digits);
  DigitIdx := StartDigitIdx;
  AddValue := DigitValue;
  while DigitIdx < DigitsLen do
    begin
      Digit := A.Digits[DigitIdx];
      Inc(Digit, AddValue);
      A.Digits[DigitIdx] := Digit mod 10;
      AddValue := Digit div 10;
      if AddValue = 0 then
        exit;
      Inc(DigitIdx);
    end;
  Assert((AddValue > 0) and (AddValue <= 9));
  SetLength(A.Digits, DigitsLen + 1);
  A.Digits[DigitsLen] := AddValue;
end;

procedure HugeDecimalInternalSubtractDigit(var A: HugeDecimal; const StartDigitIdx: Integer;
          const DigitValue: Byte);
var
  DigitsLen, DigitIdx : Integer;
  Digit : Byte;
  SDigit, SubValue : ShortInt;
  Borrow : Boolean;
begin
  Assert(StartDigitIdx >= 0);
  Assert(StartDigitIdx < Length(A.Digits));
  Assert(DigitValue <= 9);

  if DigitValue = 0 then
    exit;
  DigitsLen := Length(A.Digits);
  DigitIdx := StartDigitIdx;
  SubValue := DigitValue;
  Borrow := False;
  while DigitIdx < DigitsLen do
    begin
      Digit := A.Digits[DigitIdx];
      Assert(Digit <= 9);
      SDigit := Digit - SubValue;
      if SDigit < 0 then
        begin
          Borrow := True;
          Inc(SDigit, 10);
          SubValue := 1;
        end
      else
        Borrow := False;
      A.Digits[DigitIdx] := SDigit;
      if not Borrow then
        begin
          if (SDigit = 0) and (DigitIdx = DigitsLen - 1) then
            SetLength(A.Digits, DigitsLen - 1);
          exit;
        end;
      Inc(DigitIdx);
    end;
  Assert(Borrow);
  raise EDecimalError.Create(SOverflowError);
end;

// Pre: A is normalised, N <= 9
// Post: A is normalised
procedure HugeDecimalInc(var A: HugeDecimal; const N: Byte);
var
  DigitsLen, IntDigits : Integer;
begin
  if N = 0 then
    exit;
  if N > 9 then
    raise EConvertError.Create(SInvalidParameter);
  DigitsLen := Length(A.Digits);
  if DigitsLen = 0 then
    begin
      SetLength(A.Digits, 1);
      A.Digits[0] := N;
      exit;
    end;
  IntDigits := DigitsLen - A.Precision;
  if IntDigits = 0 then
    begin
      SetLength(A.Digits, DigitsLen + 1);
      A.Digits[DigitsLen] := N;
      exit;
    end;
  HugeDecimalInternalAddDigit(A, A.Precision, N);
end;

// Pre: A is normalised, N <= 9
// Post: A is normalised
procedure HugeDecimalDec(var A: HugeDecimal; const N: Byte);
var
  DigitsLen, IntDigits : Integer;
begin
  if N = 0 then
    exit;
  if N > 9 then
    raise EConvertError.Create(SInvalidParameter);
  DigitsLen := Length(A.Digits);
  if DigitsLen = 0 then
    raise EDecimalError.Create(SOverflowError);
  IntDigits := DigitsLen - A.Precision;
  if IntDigits = 0 then
    raise EDecimalError.Create(SOverflowError);
  HugeDecimalInternalSubtractDigit(A, A.Precision, N);
end;

// Pre: A is normalised
// Post: A is normalised
procedure HugeDecimalTrunc(var A: HugeDecimal);
var
  DigitsLen, IntDigits, DecDigits, DigitIdx : Integer;
begin
  DecDigits := A.Precision;
  if DecDigits = 0 then
    exit;
  DigitsLen := Length(A.Digits);
  IntDigits := DigitsLen - DecDigits;
  if IntDigits = 0 then
    begin
      HugeDecimalInitZero(A);
      exit;
    end;
  for DigitIdx := 0 to IntDigits - 1 do
    A.Digits[DigitIdx] := A.Digits[DigitIdx + DecDigits];
  SetLength(A.Digits, IntDigits);
  A.Precision := 0;
end;

// Pre: A is normalised
function HugeDecimalFracCompareHalf(var A: HugeDecimal): Integer;
var
  TotDigits, IntDigits, DigitIdx, Digit1 : Integer;
begin
  if A.Precision = 0 then
    begin
      Result := -1;
      exit;
    end;
  TotDigits := HugeDecimalDigits(A);
  IntDigits := TotDigits - A.Precision;
  Digit1 := HugeDecimalGetDigit(A, IntDigits);
  if Digit1 < 5 then
    begin
      Result := -1;
      exit;
    end;
  if Digit1 > 5 then
    begin
      Result := 1;
      exit;
    end;
  for DigitIdx := IntDigits + 1 to TotDigits - 1 do
    begin
      Digit1 := HugeDecimalGetDigit(A, IntDigits);
      if Digit1 > 0 then
        begin
          Result := 1;
          exit;
        end;
    end;
  Result := 0;
end;

procedure HugeDecimalRound(var A: HugeDecimal);
var
  F : Integer;
begin
  if A.Precision = 0 then
    exit;
  F := HugeDecimalFracCompareHalf(A);
  HugeDecimalTrunc(A);
  if (F > 0) or
     ((F = 0) and HugeDecimalIsOdd(A)) then
    HugeDecimalInc(A);
end;

function HugeDecimalEqualsWord8(const A: HugeDecimal; const B: Byte): Boolean;
var
  DigitsLen : Integer;
begin
  if A.Precision > 0 then
    Result := False
  else
  if B = 0 then
    Result := HugeDecimalIsZero(A)
  else
    begin
      DigitsLen := Length(A.Digits);
      if B < 10 then
        begin
          if DigitsLen <> 1 then
            Result := False
          else
            Result := A.Digits[0] = B;
        end
      else
      if B < 100 then
        begin
          if DigitsLen <> 2 then
            Result := False
          else
            Result := (A.Digits[0] + A.Digits[1] * 10) = B;
        end
      else
        begin
          if DigitsLen <> 3 then
            Result := False
          else
            Result := (A.Digits[0] + A.Digits[1] * 10 + A.Digits[2] * 100) = B;
        end;
    end;
end;

// Pre: A and B normalised
function HugeDecimalEqualsHugeDecimal(const A, B: HugeDecimal): Boolean;
var
  DigitsLen, DigitsIdx : Integer;
begin
  if A.Precision <> B.Precision then
    begin
      Result := False;
      exit;
    end;
  DigitsLen := Length(A.Digits);
  if DigitsLen <> Length(B.Digits) then
    begin
      Result := False;
      exit;
    end;
  for DigitsIdx := 0 to DigitsLen - 1 do
    if A.Digits[DigitsIdx] <> B.Digits[DigitsIdx] then
      begin
        Result := False;
        exit;
      end;
  Result := True;
end;

// Pre: A normalised
function HugeDecimalCompareWord8(const A: HugeDecimal; const B: Byte): Integer;
var
  DigitsLen, IntDigits, Precision, Cmp : Integer;
begin
  DigitsLen := Length(A.Digits);
  Precision := A.Precision;
  IntDigits := DigitsLen - Precision;
  if B = 0 then
    begin
      if DigitsLen = 0 then
        Result := 0
      else
        Result := 1;
    end
  else
  if B < 10 then
    begin
      if IntDigits < 1 then
        Result := -1
      else
      if IntDigits > 1 then
        Result := 1
      else
        begin
          Cmp := Word8Compare(A.Digits[Precision], B);
          if (Cmp = 0) and (Precision > 0) then
            Cmp := 1;
          Result := Cmp;
        end;
    end
  else
  if B < 100 then
    begin
      if IntDigits < 2 then
        Result := -1
      else
      if IntDigits > 2 then
        Result := 1
      else
        begin
          Cmp := Word8Compare(
              A.Digits[Precision] +
              A.Digits[Precision + 1] * 10, B);
          if (Cmp = 0) and (Precision > 0) then
            Cmp := 1;
          Result := Cmp;
        end;
    end
  else
    begin
      if IntDigits < 3 then
        Result := -1
      else
      if IntDigits > 3 then
        Result := 1
      else
        begin
          Cmp := Word16Compare(
              A.Digits[Precision] +
              A.Digits[Precision + 1] * 10 +
              A.Digits[Precision + 2] * 100, B);
          if (Cmp = 0) and (Precision > 0) then
            Cmp := 1;
          Result := Cmp;
        end;
    end;
end;

// Pre: A and B normalised
function HugeDecimalCompareHugeDecimal(const A, B: HugeDecimal): Integer;
var
  ADigitsLen, BDigitsLen : Integer;
  AIntDigits, BIntDigits : Integer;
  DigitIdx, MinPrecision : Integer;
  Digit1, Digit2 : Byte;
begin
  ADigitsLen := Length(A.Digits);
  BDigitsLen := Length(B.Digits);
  if ADigitsLen = 0 then
    begin
      if BDigitsLen = 0 then
        Result := 0 // A = B = 0
      else
        Result := -1; // A = 0, B > 0 ... A < B
      exit;
    end;
  // compare integer digits
  AIntDigits := ADigitsLen - A.Precision;
  BIntDigits := BDigitsLen - B.Precision;
  if AIntDigits < BIntDigits then
    begin
      Result := -1;
      exit;
    end
  else
  if AIntDigits > BIntDigits then
    begin
      Result := 1;
      exit;
    end;
  for DigitIdx := 0 to AIntDigits - 1 do
    begin
      Digit1 := A.Digits[ADigitsLen - DigitIdx - 1];
      Digit2 := B.Digits[BDigitsLen - DigitIdx - 1];
      if Digit1 < Digit2 then
        begin
          Result := -1;
          exit;
        end
      else
      if Digit1 > Digit2 then
        begin
          Result := 1;
          exit;
        end;
    end;
  // compare fractional part
  if A.Precision < B.Precision then
    MinPrecision := A.Precision
  else
    MinPrecision := B.Precision;
  for DigitIdx := 0 to MinPrecision - 1 do
    begin
      Digit1 := A.Digits[ADigitsLen - AIntDigits - DigitIdx - 1];
      Digit2 := B.Digits[BDigitsLen - BIntDigits - DigitIdx - 1];
      if Digit1 < Digit2 then
        begin
          Result := -1;
          exit;
        end
      else
      if Digit1 > Digit2 then
        begin
          Result := 1;
          exit;
        end;
    end;
  // check if digits remain
  if A.Precision = B.Precision then
    Result := 0
  else
  if A.Precision > B.Precision then
    Result := 1
  else
    Result := -1;
end;

// Pre: A and B normalised
// Post: A contains result normalised
procedure HugeDecimalAddHugeDecimal(var A: HugeDecimal; const B: HugeDecimal);
var
  Res : HugeDecimal;
  DigitsLenA, DigitsLenB : Integer;
  PrecisionA, PrecisionB, Precision : Integer;
  IntDigitsA, IntDigitsB, IntDigits : Integer;
  Idx, T : Integer;
  Digit : Byte;
begin
  DigitsLenB := Length(B.Digits);
  if DigitsLenB = 0 then
    exit;
  DigitsLenA := Length(A.Digits);
  if DigitsLenA = 0 then
    begin
      HugeDecimalAssignHugeDecimal(A, B);
      exit;
    end;
  PrecisionA := A.Precision;
  PrecisionB := B.Precision;
  if PrecisionA > PrecisionB then
    Precision := PrecisionA
  else
    Precision := PrecisionB;
  IntDigitsA := DigitsLenA - PrecisionA;
  IntDigitsB := DigitsLenB - PrecisionB;
  if IntDigitsA > IntDigitsB then
    IntDigits := IntDigitsA
  else
    IntDigits := IntDigitsB;
  HugeDecimalInit(Res);
  Res.Precision := Precision;
  SetLength(Res.Digits, Precision + IntDigits);
  Digit := 0;
  for Idx := 0 to Precision - 1 do
    begin
      T := Precision - Idx;
      if PrecisionA >= T then
        Inc(Digit, A.Digits[PrecisionA - T]);
      if PrecisionB >= T then
        Inc(Digit, B.Digits[PrecisionB - T]);
      Res.Digits[Idx] := Digit mod 10;
      Digit := Digit div 10;
    end;
  for Idx := 0 to IntDigits - 1 do
    begin
      if Idx < IntDigitsA then
        Inc(Digit, A.Digits[PrecisionA + Idx]);
      if Idx < IntDigitsB then
        Inc(Digit, B.Digits[PrecisionB + Idx]);
      Res.Digits[Precision + Idx] := Digit mod 10;
      Digit := Digit div 10;
    end;
  if Digit > 0 then
    begin
      SetLength(Res.Digits, Precision + IntDigits + 1);
      Res.Digits[Precision + IntDigits] := Digit;
    end;
  HugeDecimalInternalNormalise(Res);
  HugeDecimalAssignHugeDecimal(A, Res);
end;

// Pre: A and B normalised
// Post: A contains result normalised
procedure HugeDecimalSubtractHugeDecimal(var A: HugeDecimal; const B: HugeDecimal);
var
  Res : HugeDecimal;
  DigitsLenA, DigitsLenB, DigitsLen : Integer;
  Cmp : Integer;
  PrecisionA, PrecisionB, Precision : Integer;
  IntDigitsA, IntDigitsB, IntDigits : Integer;
  J, I : Integer;
  Digit, DigitB : Byte;
  SDigit : ShortInt;
  Borrow : Boolean;
begin
  DigitsLenB := Length(B.Digits);
  if DigitsLenB = 0 then
    exit;
  Cmp := HugeDecimalCompareHugeDecimal(A, B);
  if Cmp = 0 then
    begin
      HugeDecimalAssignZero(A);
      exit;
    end;
  if Cmp < 0 then
    raise EDecimalError.Create(SOverflowError);
  DigitsLenA := Length(A.Digits);
  PrecisionA := A.Precision;
  PrecisionB := B.Precision;
  if PrecisionA > PrecisionB then
    Precision := PrecisionA
  else
    Precision := PrecisionB;
  IntDigitsA := DigitsLenA - PrecisionA;
  IntDigitsB := DigitsLenB - PrecisionB;
  Assert(IntDigitsA >= IntDigitsB);
  IntDigits := IntDigitsA;
  HugeDecimalInit(Res);
  Res.Precision := Precision;
  DigitsLen := Precision + IntDigits;
  SetLength(Res.Digits, DigitsLen);
  J := 0;
  while J < PrecisionB - PrecisionA do
    begin
      Res.Digits[J] := 0;
      Inc(J);
    end;
  for I := 0 to DigitsLenA - 1 do
    Res.Digits[J + I] := A.Digits[I];
  if PrecisionB < PrecisionA then
    J := PrecisionA - PrecisionB
  else
    J := 0;
  Borrow := False;
  for I := 0 to DigitsLen - 1 do
    begin
      Digit := Res.Digits[I];
      if I < J then
        DigitB := 0
      else
        if I - J < DigitsLenB then
          DigitB := B.Digits[I - J]
        else
          begin
            DigitB := 0;
            if not Borrow then
              break;
          end;
      SDigit := Digit - DigitB;
      if Borrow then
        Dec(SDigit);
      if SDigit < 0 then
        begin
          Inc(SDigit, 10);
          Borrow := True;
        end
      else
        Borrow := False;
      Res.Digits[I] := SDigit;
    end;
  HugeDecimalInternalNormalise(Res);
  HugeDecimalAssignHugeDecimal(A, Res);
end;

// Post: R normalised if result is dceNoError
function TryStrToHugeDecimal(const S: String; var R: HugeDecimal): TDecimalConvertErrorType;
var
  LenS, IdxS, IdxZ, Precision, DigitI : Integer;
  DigitC : Char;
  NonZero : Boolean;
begin
  LenS := Length(S);
  if LenS = 0 then
    begin
      Result := dceConvertError;
      exit;
    end;
  HugeDecimalAssignZero(R);
  IdxS := 1;
  // skip preceding zeros
  while IdxS <= LenS do
    begin
      DigitC := S[IdxS];
      if DigitC <> '0' then
        break;
      Inc(IdxS);
    end;
  // parse integer part
  while IdxS <= LenS do
    begin
      DigitC := S[IdxS];
      if DigitC = '.' then
        if IdxS = 1 then
          begin
            Result := dceConvertError;
            exit;
          end
        else
          break;
      DigitI := CharDigitToInt(DigitC);
      if DigitI < 0 then
        begin
          Result := dceConvertError;
          exit;
        end;
      HugeDecimalMul10(R);
      HugeDecimalInc(R, DigitI);
      Inc(IdxS);
    end;
  // parse decimal part
  if (IdxS <= LenS) and (S[IdxS] = '.') then
    begin
      Inc(IdxS);
      if IdxS > LenS then
        begin
          Result := dceConvertError;
          exit;
        end;
      Precision := 0;
      while IdxS <= LenS do
        begin
          DigitC := S[IdxS];
          DigitI := CharDigitToInt(DigitC);
          if DigitI < 0 then
            begin
              Result := dceConvertError;
              exit;
            end;
          if DigitI = 0 then
            begin
              // check if all traling digits are zero
              NonZero := False;
              for IdxZ := IdxS + 1 to LenS do
                if S[IdxZ] <> '0' then
                  begin
                    NonZero := True;
                    break;
                  end;
              if not NonZero then
                break;
            end;
          HugeDecimalMul10(R);
          HugeDecimalInc(R, DigitI);
          Inc(Precision);
          Inc(IdxS);
        end;
      R.Precision := Precision;
    end;
  // success
  Result := dceNoError;
end;

// Post: R normalised if no exception
procedure StrToHugeDecimal(const S: String; var R: HugeDecimal);
begin
  case TryStrToHugeDecimal(S, R) of
    dceNoError      : ;
    dceConvertError : raise EDecimalError.Create(SConvertError);
  end;
end;

// Pre: A normalised
function HugeDecimalToStr(const A: HugeDecimal): String;
var
  DigitsLen, Digits, StrLen, DigitIdx, ResPos, DecimalPos : Integer;
  ResStr : String;
  Digit : Byte;
  ZeroIntDigits : Boolean;
begin
  DigitsLen := Length(A.Digits);
  if DigitsLen = 0 then
    begin
      Result := '0';
      exit;
    end;
  Digits := HugeDecimalDigits(A);
  StrLen := Digits;
  if A.Precision > 0 then
    Inc(StrLen);
  ZeroIntDigits := A.Precision = Digits;
  if A.Precision > 0 then
    begin
      if ZeroIntDigits then
        DecimalPos := -1
      else
        DecimalPos := Digits - A.Precision + 1;
    end
  else
    DecimalPos := -1;
  if ZeroIntDigits then
    Inc(StrLen);
  SetLength(ResStr, StrLen);
  ResPos := 1;
  if ZeroIntDigits then
    begin
      ResStr[ResPos] := '0';
      Inc(ResPos);
      ResStr[ResPos] := '.';
      Inc(ResPos);
    end;
  for DigitIdx := 0 to DigitsLen - 1 do
    begin
      Digit := A.Digits[DigitsLen - DigitIdx - 1];
      ResStr[ResPos] := IntToCharDigit(Digit);
      Inc(ResPos);
      if ResPos = DecimalPos then
        begin
          ResStr[ResPos] := '.';
          Inc(ResPos);
        end;
    end;
  Result := ResStr;
end;



{                                                                              }
{ SDecimal32                                                                   }
{                                                                              }
procedure SDecimal32InitZero(var A: SDecimal32);
begin
  A.Sign := 0;
  Decimal32InitZero(A.Value);
end;

procedure SDecimal32InitOne(var A: SDecimal32);
begin
  A.Sign := 1;
  Decimal32InitOne(A.Value);
end;

procedure SDecimal32InitMinusOne(var A: SDecimal32);
begin
  A.Sign := -1;
  Decimal32InitOne(A.Value);
end;

procedure SDecimal32InitMin(var A: SDecimal32);
begin
  A.Sign := -1;
  Decimal32InitMax(A.Value);
end;

procedure SDecimal32InitMax(var A: SDecimal32);
begin
  A.Sign := 1;
  Decimal32InitMax(A.Value);
end;

function SDecimal32IsZero(const A: SDecimal32): Boolean;
begin
  Result := A.Sign = 0;
end;

function SDecimal32IsOne(const A: SDecimal32): Boolean;
begin
  Result := (A.Sign > 0) and Decimal32IsOne(A.Value);
end;

function SDecimal32IsMinusOne(const A: SDecimal32): Boolean;
begin
  Result := (A.Sign < 0) and Decimal32IsOne(A.Value);
end;

function SDecimal32IsMinimum(const A: SDecimal32): Boolean;
begin
  Result := (A.Sign < 0) and Decimal32IsMaximum(A.Value);
end;

function SDecimal32IsMaximum(const A: SDecimal32): Boolean;
begin
  Result := (A.Sign > 0) and Decimal32IsMaximum(A.Value);
end;

function SDecimal32IsOverflow(const A: SDecimal32): Boolean;
begin
  Result := Decimal32IsOverflow(A.Value);
end;

function Word32IsSDecimal32Range(const A: Word32): Boolean;
begin
  Result := A <= SDecimal32MaxInt;
end;

function Int32IsSDecimal32Range(const A: Int32): Boolean;
begin
  Result := (A >= SDecimal32MinInt) and (A <= SDecimal32MaxInt);
end;

function FloatIsSDecimal32Range(const A: Double): Boolean;
begin
  Result :=
    (A > SDecimal32MinFloatLimD) and
    (A < SDecimal32MaxFloatLimD);
end;

function SDecimal32Sign(const A: SDecimal32): Integer;
begin
  Result := A.Sign;
end;

procedure SDecimal32Negate(var A: SDecimal32);
begin
  A.Sign := -A.Sign;
end;

procedure SDecimal32AbsInPlace(var A: SDecimal32);
begin
  if A.Sign < 0 then
    A.Sign := 1;
end;

procedure SDecimal32InitWord8(var A: SDecimal32; const B: Byte);
begin
  if B = 0 then
    SDecimal32InitZero(A)
  else
    begin
      A.Sign := 1;
      Decimal32InitWord8(A.Value, B);
    end;
end;

procedure SDecimal32InitWord16(var A: SDecimal32; const B: Word);
begin
  if B = 0 then
    SDecimal32InitZero(A)
  else
    begin
      A.Sign := 1;
      Decimal32InitWord16(A.Value, B);
    end;
end;

procedure SDecimal32InitWord32(var A: SDecimal32; const B: Word32);
begin
  if B = 0 then
    SDecimal32InitZero(A)
  else
    begin
      A.Sign := 1;
      Decimal32InitWord32(A.Value, B);
    end;
end;

procedure SDecimal32InitInt32(var A: SDecimal32; const B: Int32);
begin
  if B = 0 then
    SDecimal32InitZero(A)
  else
  if B < 0 then
    begin
      A.Sign := -1;
      Decimal32InitWord32(A.Value, -B);
    end
  else
    begin
      A.Sign := 1;
      Decimal32InitWord32(A.Value, B);
    end;
end;

procedure SDecimal32InitSDecimal32(var A: SDecimal32; const B: SDecimal32);
begin
  A.Sign := B.Sign;
  A.Value := B.Value;
end;

procedure SDecimal32InitFloat(var A: SDecimal32; const B: Double);
begin
  if B < Decimal32MinFloatD then
    begin
      A.Sign := -1;
      Decimal32InitFloat(A.Value, -B);
    end
  else
  if B > -Decimal32MinFloatD then
    begin
      A.Sign := 1;
      Decimal32InitFloat(A.Value, B);
    end
  else
    SDecimal32InitZero(A);
end;

function SDecimal32ToWord8(const A: SDecimal32): Byte;
begin
  if A.Sign < 0 then
    raise EDecimalError.Create(SOverflowError);
  Result := Decimal32ToWord8(A.Value);
end;

function SDecimal32ToWord16(const A: SDecimal32): Word;
begin
  if A.Sign < 0 then
    raise EDecimalError.Create(SOverflowError);
  Result := Decimal32ToWord16(A.Value);
end;

function SDecimal32ToWord32(const A: SDecimal32): Word32;
begin
  if A.Sign < 0 then
    raise EDecimalError.Create(SOverflowError);
  Result := Decimal32ToWord32(A.Value);
end;

function SDecimal32ToInt32(const A: SDecimal32): Int32;
var T : Int32;
begin
  T := Decimal32ToInt32(A.Value);
  if A.Sign < 0 then
    T := -T;
  Result := T;
end;

function  SDecimal32ToFloat(const A: SDecimal32): Double;
var T : Double;
begin
  T := Decimal32ToFloat(A.Value);
  if A.Sign < 0 then
    T := -T;
  Result := T;
end;

function SDecimal32Trunc(const A: SDecimal32): Int32;
var T : Int32;
begin
  T := Decimal32Trunc(A.Value);
  if A.Sign < 0 then
    T := -T;
  Result := T;
end;

function SDecimal32Round(const A: SDecimal32): Int32;
var T : Int32;
begin
  T := Decimal32Round(A.Value);
  if A.Sign < 0 then
    T := -T;
  Result := T;
end;

function SDecimal32FracWord(const A: SDecimal32): Word;
begin
  Result := Decimal32FracWord(A.Value);
end;

function SDecimal32EqualsWord8(const A: SDecimal32; const B: Byte): Boolean;
begin
  Result := (A.Sign >= 0) and Decimal32EqualsWord8(A.Value, B);
end;

function SDecimal32EqualsWord16(const A: SDecimal32; const B: Word): Boolean;
begin
  Result := (A.Sign >= 0) and Decimal32EqualsWord16(A.Value, B);
end;

function SDecimal32EqualsWord32(const A: SDecimal32; const B: Word32): Boolean;
begin
  Result := (A.Sign >= 0) and Decimal32EqualsWord32(A.Value, B);
end;

function SDecimal32EqualsInt32(const A: SDecimal32; const B: Int32): Boolean;
begin
  if A.Sign = 0 then
    Result := (B = 0)
  else
  if A.Sign > 0 then
    begin
      if B <= 0 then
        Result := False
      else
        Result := Decimal32EqualsWord32(A.Value, B);
    end
  else
    begin
      if B >= 0 then
        Result := False
      else
        Result := Decimal32EqualsWord32(A.Value, -B);
    end;
end;

function SDecimal32EqualsSDecimal32(const A: SDecimal32; const B: SDecimal32): Boolean;
begin
  Result :=
    (A.Sign = B.Sign) and
    Decimal32EqualsDecimal32(A.Value, B.Value);
end;

function SDecimal32EqualsFloat(const A: SDecimal32; const B: Double): Boolean;
begin
  if A.Sign >= 0 then
    Result := Decimal32EqualsFloat(A.Value, B)
  else
    Result := Decimal32EqualsFloat(A.Value, -B);
end;

function SDecimal32CompareWord8(const A: SDecimal32; const B: Byte): Integer;
begin
  if A.Sign = 0 then
    if B = 0 then
      Result := 0
    else
      Result := -1
  else
  if A.Sign > 0 then
    Result := Decimal32CompareWord8(A.Value, B)
  else
    Result := -1;
end;

function SDecimal32CompareWord16(const A: SDecimal32; const B: Word): Integer;
begin
  if A.Sign = 0 then
    if B = 0 then
      Result := 0
    else
      Result := -1
  else
  if A.Sign > 0 then
    Result := Decimal32CompareWord16(A.Value, B)
  else
    Result := -1;
end;

function SDecimal32CompareWord32(const A: SDecimal32; const B: Word32): Integer;
begin
  if A.Sign = 0 then
    if B = 0 then
      Result := 0
    else
      Result := -1
  else
  if A.Sign > 0 then
    Result := Decimal32CompareWord32(A.Value, B)
  else
    Result := -1;
end;

function SDecimal32CompareInt32(const A: SDecimal32; const B: Int32): Integer;
begin
  if A.Sign = 0 then
    begin
      if B < 0 then
        Result := 1
      else
      if B > 0 then
        Result := -1
      else
        Result := 0;
    end
  else
  if A.Sign > 0 then
    begin
      if B <= 0 then
        Result := 1
      else
        Result := Decimal32CompareWord32(A.Value, B);
    end
  else
    begin
      if B >= 0 then
        Result := -1
      else
        Result := -Decimal32CompareWord32(A.Value, -B);
    end;
end;

function SDecimal32CompareSDecimal32(const A: SDecimal32; const B: SDecimal32): Integer;
begin
  if A.Sign = 0 then
    begin
      if B.Sign < 0 then
        Result := 1
      else
      if B.Sign > 0 then
        Result := -1
      else
        Result := 0;
    end
  else
  if A.Sign > 0 then
    begin
      if B.Sign <= 0 then
        Result := 1
      else
        Result := Decimal32CompareDecimal32(A.Value, B.Value);
    end
  else
    begin
      if B.Sign >= 0 then
        Result := -1
      else
        Result := -Decimal32CompareDecimal32(A.Value, B.Value);
    end;
end;

function SDecimal32CompareFloat(const A: SDecimal32; const B: Double): Integer;
begin
  if A.Sign >= 0 then
    Result := Decimal32CompareFloat(A.Value, B)
  else
    Result := -Decimal32CompareFloat(A.Value, -B);
end;

procedure SDecimal32AddWord8(var A: SDecimal32; const B: Byte);
var S : Int8;
    T : Decimal32;
begin
  if B = 0 then
    exit;
  if A.Sign = 0 then
    begin
      A.Sign := 1;
      Decimal32InitWord8(A.Value, B);
    end
  else
  if A.Sign > 0 then
    Decimal32AddWord8(A.Value, B)
  else
    begin
      S := Decimal32CompareWord8(A.Value, B);
      if S = 0 then
        SDecimal32InitZero(A)
      else
      if S > 0 then
        Decimal32SubtractWord8(A.Value, B)
      else
        begin
          A.Sign := 1;
          Decimal32InitWord8(T, B);
          Decimal32SubtractDecimal32(T, A.Value);
          A.Value := T;
        end;
    end;
end;

procedure SDecimal32AddWord16(var A: SDecimal32; const B: Word);
var S : Int8;
    T : Decimal32;
begin
  if B = 0 then
    exit;
  if A.Sign = 0 then
    begin
      A.Sign := 1;
      Decimal32InitWord16(A.Value, B);
    end
  else
  if A.Sign > 0 then
    Decimal32AddWord16(A.Value, B)
  else
    begin
      S := Decimal32CompareWord16(A.Value, B);
      if S = 0 then
        SDecimal32InitZero(A)
      else
      if S > 0 then
        Decimal32SubtractWord16(A.Value, B)
      else
        begin
          A.Sign := 1;
          Decimal32InitWord16(T, B);
          Decimal32SubtractDecimal32(T, A.Value);
          A.Value := T;
        end;
    end;
end;

procedure SDecimal32AddWord32(var A: SDecimal32; const B: Word32);
var S : Int8;
    T : Decimal32;
begin
  if B = 0 then
    exit;
  if A.Sign = 0 then
    begin
      A.Sign := 1;
      Decimal32InitWord32(A.Value, B);
    end
  else
  if A.Sign > 0 then
    Decimal32AddWord32(A.Value, B)
  else
    begin
      S := Decimal32CompareWord32(A.Value, B);
      if S = 0 then
        SDecimal32InitZero(A)
      else
      if S > 0 then
        Decimal32SubtractWord32(A.Value, B)
      else
        begin
          A.Sign := 1;
          Decimal32InitWord32(T, B);
          Decimal32SubtractDecimal32(T, A.Value);
          A.Value := T;
        end;
    end;
end;

procedure SDecimal32AddSDecimal32(var A: SDecimal32; const B: SDecimal32);
var S : Int8;
    T : Decimal32;
begin
  if B.Sign = 0 then
    exit;
  if A.Sign = 0 then
    begin
      A.Sign := 1;
      Decimal32InitDecimal32(A.Value, B.Value);
    end
  else
  if (A.Sign = B.Sign) then
    Decimal32AddDecimal32(A.Value, B.Value)
  else
    begin
      S := Decimal32CompareDecimal32(A.Value, B.Value);
      if S = 0 then
        SDecimal32InitZero(A)
      else
      if S > 0 then
        Decimal32SubtractDecimal32(A.Value, B.Value)
      else
        begin
          A.Sign := -A.Sign;
          Decimal32InitDecimal32(T, B.Value);
          Decimal32SubtractDecimal32(T, A.Value);
          A.Value := T;
        end;
    end;
end;

procedure SDecimal32SubtractWord8(var A: SDecimal32; const B: Byte);
var S : Int8;
    T : Decimal32;
begin
  if B = 0 then
    exit;
  if A.Sign = 0 then
    begin
      A.Sign := -1;
      Decimal32InitWord8(A.Value, B);
    end
  else
  if A.Sign < 0 then
    Decimal32AddWord8(A.Value, B)
  else
    begin
      S := Decimal32CompareWord8(A.Value, B);
      if S = 0 then
        SDecimal32InitZero(A)
      else
      if S > 0 then
        Decimal32SubtractWord8(A.Value, B)
      else
        begin
          A.Sign := -1;
          Decimal32InitWord8(T, B);
          Decimal32SubtractDecimal32(T, A.Value);
          A.Value := T;
        end;
    end;
end;

procedure SDecimal32SubtractWord16(var A: SDecimal32; const B: Word);
var S : Int8;
    T : Decimal32;
begin
  if B = 0 then
    exit;
  if A.Sign = 0 then
    begin
      A.Sign := -1;
      Decimal32InitWord16(A.Value, B);
    end
  else
  if A.Sign < 0 then
    Decimal32AddWord8(A.Value, B)
  else
    begin
      S := Decimal32CompareWord16(A.Value, B);
      if S = 0 then
        SDecimal32InitZero(A)
      else
      if S > 0 then
        Decimal32SubtractWord16(A.Value, B)
      else
        begin
          A.Sign := -1;
          Decimal32InitWord16(T, B);
          Decimal32SubtractDecimal32(T, A.Value);
          A.Value := T;
        end;
    end;
end;

procedure SDecimal32SubtractWord32(var A: SDecimal32; const B: Word32);
var S : Int8;
    T : Decimal32;
begin
  if B = 0 then
    exit;
  if A.Sign = 0 then
    begin
      A.Sign := -1;
      Decimal32InitWord32(A.Value, B);
    end
  else
  if A.Sign < 0 then
    Decimal32AddWord32(A.Value, B)
  else
    begin
      S := Decimal32CompareWord32(A.Value, B);
      if S = 0 then
        SDecimal32InitZero(A)
      else
      if S > 0 then
        Decimal32SubtractWord32(A.Value, B)
      else
        begin
          A.Sign := -1;
          Decimal32InitWord32(T, B);
          Decimal32SubtractDecimal32(T, A.Value);
          A.Value := T;
        end;
    end;
end;

procedure SDecimal32SubtractSDecimal32(var A: SDecimal32; const B: SDecimal32);
var S : Int8;
    T : Decimal32;
begin
  if B.Sign = 0 then
    exit;
  if A.Sign = 0 then
    begin
      A.Sign := -1;
      Decimal32InitDecimal32(A.Value, B.Value);
    end
  else
  if A.Sign <> B.Sign then
    Decimal32AddDecimal32(A.Value, B.Value)
  else
    begin
      S := Decimal32CompareDecimal32(A.Value, B.Value);
      if S = 0 then
        SDecimal32InitZero(A)
      else
      if S > 0 then
        Decimal32SubtractDecimal32(A.Value, B.Value)
      else
        begin
          A.Sign := -A.Sign;
          Decimal32InitDecimal32(T, B.Value);
          Decimal32SubtractDecimal32(T, A.Value);
          A.Value := T;
        end;
    end;
end;

procedure SDecimal32MultiplyWord8(var A: SDecimal32; const B: Byte);
begin
  if A.Sign = 0 then
    exit;
  if B = 0 then
    begin
      SDecimal32InitZero(A);
      exit;
    end;
  Decimal32MultiplyWord8(A.Value, B);
end;

procedure SDecimal32MultiplyWord16(var A: SDecimal32; const B: Word);
begin
  if A.Sign = 0 then
    exit;
  if B = 0 then
    begin
      SDecimal32InitZero(A);
      exit;
    end;
  Decimal32MultiplyWord16(A.Value, B);
end;

procedure SDecimal32MultiplyWord32(var A: SDecimal32; const B: Word32);
begin
  if A.Sign = 0 then
    exit;
  if B = 0 then
    begin
      SDecimal32InitZero(A);
      exit;
    end;
  Decimal32MultiplyWord32(A.Value, B);
end;

procedure SDecimal32MultiplySDecimal32(var A: SDecimal32; const B: SDecimal32);
begin
  if A.Sign = 0 then
    exit;
  if B.Sign = 0 then
    begin
      SDecimal32InitZero(A);
      exit;
    end;
  Decimal32MultiplyDecimal32(A.Value, B.Value);
  if A.Sign = B.Sign then
    A.Sign := 1
  else
    A.Sign := -1;
end;

procedure SDecimal32DivideWord8(var A: SDecimal32; const B: Byte);
begin
  if B = 0 then
    raise EDecimalError.Create(SDivisionByZero);
  if A.Sign = 0 then
    exit;
  Decimal32DivideWord8(A.Value, B);
end;

procedure SDecimal32DivideWord16(var A: SDecimal32; const B: Word);
begin
  if B = 0 then
    raise EDecimalError.Create(SDivisionByZero);
  if A.Sign = 0 then
    exit;
  Decimal32DivideWord16(A.Value, B);
end;

procedure SDecimal32DivideWord32(var A: SDecimal32; const B: Word32);
begin
  if B = 0 then
    raise EDecimalError.Create(SDivisionByZero);
  if A.Sign = 0 then
    exit;
  Decimal32DivideWord32(A.Value, B);
end;

procedure SDecimal32DivideSDecimal32(var A: SDecimal32; const B: SDecimal32);
begin
  if B.Sign = 0 then
    raise EDecimalError.Create(SDivisionByZero);
  if A.Sign = 0 then
    exit;
  Decimal32DivideDecimal32(A.Value, B.Value);
  if A.Sign = B.Sign then
    A.Sign := 1
  else
    A.Sign := -1;
end;

function SDecimal32ToStr(const A: SDecimal32): String;
var S : String;
begin
  if A.Sign < 0 then
    S := '-'
  else
    S := '';
  S := S + Decimal32ToStr(A.Value);
  Result := S;
end;

function SDecimal32ToStrB(const A: SDecimal32): RawByteString;
var S : RawByteString;
begin
  if A.Sign < 0 then
    S := '-'
  else
    S := '';
  S := S + Decimal32ToStrB(A.Value);
  Result := S;
end;

function SDecimal32ToStrU(const A: SDecimal32): UnicodeString;
var S : UnicodeString;
begin
  if A.Sign < 0 then
    S := '-'
  else
    S := '';
  S := S + Decimal32ToStrU(A.Value);
  Result := S;
end;

function TryStrToSDecimal32(const A: String; out B: SDecimal32): TDecimalConvertErrorType;
var C : Char;
    T : String;
    R : TDecimalConvertErrorType;
begin
  if A = '' then
    raise EDecimalError.Create(SConvertError);
  C := A[1];
  if (C = '+') or (C = '-') then
    T := CopyFrom(A, 2)
  else
    begin
      C := '+';
      T := A;
    end;
  R := TryStrToDecimal32(T, B.Value);
  if R = dceNoError then
    begin
      if Decimal32IsZero(B.Value) then
        B.Sign := 0
      else
        if C = '+' then
          B.Sign := 1
        else
          B.Sign := -1;
    end;
  Result := R;
end;

function TryStrToSDecimal32B(const A: RawByteString; out B: SDecimal32): TDecimalConvertErrorType;
var C : AnsiChar;
    T : RawByteString;
    R : TDecimalConvertErrorType;
begin
  if A = '' then
    raise EDecimalError.Create(SConvertError);
  C := A[1];
  if (C = '+') or (C = '-') then
    T := CopyFromB(A, 2)
  else
    begin
      C := '+';
      T := A;
    end;
  R := TryStrToDecimal32B(T, B.Value);
  if R = dceNoError then
    begin
      if Decimal32IsZero(B.Value) then
        B.Sign := 0
      else
        if C = '+' then
          B.Sign := 1
        else
          B.Sign := -1;
    end;
  Result := R;
end;

function StrToSDecimal32(const A: String): SDecimal32;
begin
  case TryStrToSDecimal32(A, Result) of
    dceNoError       : ;
    dceConvertError  : raise EDecimalError.Create(SConvertError);
    dceOverflowError : raise EDecimalError.Create(SOverflowError);
  end;
end;

function StrToSDecimal32B(const A: RawByteString): SDecimal32;
begin
  case TryStrToSDecimal32B(A, Result) of
    dceNoError       : ;
    dceConvertError  : raise EDecimalError.Create(SConvertError);
    dceOverflowError : raise EDecimalError.Create(SOverflowError);
  end;
end;



{                                                                              }
{ SDecimal64                                                                   }
{                                                                              }
procedure SDecimal64InitZero(var A: SDecimal64);
begin
  A.Sign := 0;
  Decimal64InitZero(A.Value);
end;

procedure SDecimal64InitOne(var A: SDecimal64);
begin
  A.Sign := 1;
  Decimal64InitOne(A.Value);
end;

procedure SDecimal64InitMinusOne(var A: SDecimal64);
begin
  A.Sign := -1;
  Decimal64InitOne(A.Value);
end;

procedure SDecimal64InitMin(var A: SDecimal64);
begin
  A.Sign := -1;
  Decimal64InitMax(A.Value);
end;

procedure SDecimal64InitMax(var A: SDecimal64);
begin
  A.Sign := 1;
  Decimal64InitMax(A.Value);
end;

function SDecimal64IsZero(const A: SDecimal64): Boolean;
begin
  Result := A.Sign = 0;
end;

function SDecimal64IsOne(const A: SDecimal64): Boolean;
begin
  Result := (A.Sign > 0) and Decimal64IsOne(A.Value);
end;

function SDecimal64IsMinusOne(const A: SDecimal64): Boolean;
begin
  Result := (A.Sign < 0) and Decimal64IsOne(A.Value);
end;

function SDecimal64IsMinimum(const A: SDecimal64): Boolean;
begin
  Result := (A.Sign < 0) and Decimal64IsMaximum(A.Value);
end;

function SDecimal64IsMaximum(const A: SDecimal64): Boolean;
begin
  Result := (A.Sign > 0) and Decimal64IsMaximum(A.Value);
end;

function SDecimal64IsOverflow(const A: SDecimal64): Boolean;
begin
  Result := Decimal64IsOverflow(A.Value);
end;

function Word64IsSDecimal64Range(const A: Word64Rec): Boolean;
begin
  Result := Word64CompareInt64(A, Decimal64MaxInt) <= 0;
end;

function Int64IsSDecimal64Range(const A: Int64): Boolean;
begin
  Result := (A >= SDecimal64MinInt) and (A <= SDecimal64MaxInt);
end;

function FloatIsSDecimal64Range(const A: Double): Boolean;
begin
  Result :=
    (A > SDecimal64MinFloatLimD) and
    (A < SDecimal64MaxFloatLimD);
end;

function SDecimal64Sign(const A: SDecimal64): Integer;
begin
  Result := A.Sign;
end;

procedure SDecimal64Negate(var A: SDecimal64);
begin
  A.Sign := -A.Sign;
end;

procedure SDecimal64AbsInPlace(var A: SDecimal64);
begin
  if A.Sign < 0 then
    A.Sign := 1;
end;

procedure SDecimal64InitWord8(var A: SDecimal64; const B: Byte);
begin
  if B = 0 then
    SDecimal64InitZero(A)
  else
    begin
      A.Sign := 1;
      Decimal64InitWord8(A.Value, B);
    end;
end;

procedure SDecimal64InitWord16(var A: SDecimal64; const B: Word);
begin
  if B = 0 then
    SDecimal64InitZero(A)
  else
    begin
      A.Sign := 1;
      Decimal64InitWord16(A.Value, B);
    end;
end;

procedure SDecimal64InitWord32(var A: SDecimal64; const B: Word32);
begin
  if B = 0 then
    SDecimal64InitZero(A)
  else
    begin
      A.Sign := 1;
      Decimal64InitWord32(A.Value, B);
    end;
end;

procedure SDecimal64InitWord64(var A: SDecimal64; const B: Word64Rec);
begin
  if Word64IsZero(B) then
    SDecimal64InitZero(A)
  else
    begin
      A.Sign := 1;
      Decimal64InitWord64(A.Value, B);
    end;
end;

procedure SDecimal64InitInt32(var A: SDecimal64; const B: Int32);
begin
  if B = 0 then
    SDecimal64InitZero(A)
  else
  if B < 0 then
    begin
      A.Sign := -1;
      Decimal64InitWord32(A.Value, -B);
    end
  else
    begin
      A.Sign := 1;
      Decimal64InitWord32(A.Value, B);
    end;
end;

procedure SDecimal64InitInt64(var A: SDecimal64; const B: Int64);
begin
  if B = 0 then
    SDecimal64InitZero(A)
  else
  if B < 0 then
    begin
      A.Sign := -1;
      Decimal64InitInt64(A.Value, -B);
    end
  else
    begin
      A.Sign := 1;
      Decimal64InitInt64(A.Value, B);
    end;
end;

procedure SDecimal64InitSDecimal64(var A: SDecimal64; const B: SDecimal64);
begin
  A.Sign := B.Sign;
  A.Value := B.Value;
end;

procedure SDecimal64InitFloat(var A: SDecimal64; const B: Double);
begin
  if B < Decimal64MinFloatD then
    begin
      A.Sign := -1;
      Decimal64InitFloat(A.Value, -B);
    end
  else
  if B > -Decimal64MinFloatD then
    begin
      A.Sign := 1;
      Decimal64InitFloat(A.Value, B);
    end
  else
    SDecimal64InitZero(A);
end;

function SDecimal64ToWord8(const A: SDecimal64): Byte;
begin
  if A.Sign < 0 then
    raise EDecimalError.Create(SOverflowError);
  Result := Decimal64ToWord8(A.Value);
end;

function SDecimal64ToWord16(const A: SDecimal64): Word;
begin
  if A.Sign < 0 then
    raise EDecimalError.Create(SOverflowError);
  Result := Decimal64ToWord16(A.Value);
end;

function SDecimal64ToWord32(const A: SDecimal64): Word32;
begin
  if A.Sign < 0 then
    raise EDecimalError.Create(SOverflowError);
  Result := Decimal64ToWord32(A.Value);
end;

function SDecimal64ToWord64(const A: SDecimal64): Word64Rec;
begin
  if A.Sign < 0 then
    raise EDecimalError.Create(SOverflowError);
  Result := Decimal64ToWord64(A.Value);
end;

function SDecimal64ToInt32(const A: SDecimal64): Int32;
var T : Int32;
begin
  T := Decimal64ToInt32(A.Value);
  if A.Sign < 0 then
    T := -T;
  Result := T;
end;

function SDecimal64ToInt64(const A: SDecimal64): Int64;
var T : Int64;
begin
  T := Decimal64ToInt64(A.Value);
  if A.Sign < 0 then
    T := -T;
  Result := T;
end;

function SDecimal64ToSDecimal32(const A: SDecimal64): SDecimal32;
begin
  // TODO
end;

function SDecimal64ToFloat(const A: SDecimal64): Double;
var T : Double;
begin
  T := Decimal64ToFloat(A.Value);
  if A.Sign < 0 then
    T := -T;
  Result := T;
end;

function SDecimal64Trunc(const A: SDecimal64): Int64;
var T : Int64;
begin
  T := Decimal64Trunc(A.Value);
  if A.Sign < 0 then
    T := -T;
  Result := T;
end;

function SDecimal64Round(const A: SDecimal64): Int64;
var T : Int64;
begin
  T := Decimal64Round(A.Value);
  if A.Sign < 0 then
    T := -T;
  Result := T;
end;

function SDecimal64FracWord(const A: SDecimal64): Word32;
begin
  Result := Decimal64FracWord(A.Value);
end;

function SDecimal64EqualsWord8(const A: SDecimal64; const B: Byte): Boolean;
begin
  Result := (A.Sign >= 0) and Decimal64EqualsWord8(A.Value, B);
end;

function SDecimal64EqualsWord16(const A: SDecimal64; const B: Word): Boolean;
begin
  Result := (A.Sign >= 0) and Decimal64EqualsWord16(A.Value, B);
end;

function SDecimal64EqualsWord32(const A: SDecimal64; const B: Word32): Boolean;
begin
  Result := (A.Sign >= 0) and Decimal64EqualsWord32(A.Value, B);
end;

function SDecimal64EqualsInt32(const A: SDecimal64; const B: Int32): Boolean;
begin
  if A.Sign = 0 then
    Result := (B = 0)
  else
  if A.Sign > 0 then
    begin
      if B <= 0 then
        Result := False
      else
        Result := Decimal64EqualsWord32(A.Value, B);
    end
  else
    begin
      if B >= 0 then
        Result := False
      else
        Result := Decimal64EqualsWord32(A.Value, -B);
    end;
end;

function SDecimal64EqualsInt64(const A: SDecimal64; const B: Int64): Boolean;
begin
  if A.Sign = 0 then
    Result := (B = 0)
  else
  if A.Sign > 0 then
    begin
      if B <= 0 then
        Result := False
      else
        Result := Decimal64EqualsInt64(A.Value, B);
    end
  else
    begin
      if B >= 0 then
        Result := False
      else
        Result := Decimal64EqualsInt64(A.Value, -B);
    end;
end;

function SDecimal64EqualsSDecimal64(const A: SDecimal64; const B: SDecimal64): Boolean;
begin
  Result :=
    (A.Sign = B.Sign) and
    Decimal64EqualSDecimal64(A.Value, B.Value);
end;

function SDecimal64EqualsFloat(const A: SDecimal64; const B: Double): Boolean;
begin
  if A.Sign >= 0 then
    Result := Decimal64EqualsFloat(A.Value, B)
  else
    Result := Decimal64EqualsFloat(A.Value, -B);
end;

function SDecimal64CompareWord8(const A: SDecimal64; const B: Byte): Integer;
begin
  if A.Sign = 0 then
    if B = 0 then
      Result := 0
    else
      Result := -1
  else
  if A.Sign > 0 then
    Result := Decimal64CompareWord8(A.Value, B)
  else
    Result := -1;
end;

function SDecimal64CompareWord16(const A: SDecimal64; const B: Word): Integer;
begin
  if A.Sign = 0 then
    if B = 0 then
      Result := 0
    else
      Result := -1
  else
  if A.Sign > 0 then
    Result := Decimal64CompareWord16(A.Value, B)
  else
    Result := -1;
end;

function SDecimal64CompareWord32(const A: SDecimal64; const B: Word32): Integer;
begin
  if A.Sign = 0 then
    if B = 0 then
      Result := 0
    else
      Result := -1
  else
  if A.Sign > 0 then
    Result := Decimal64CompareWord32(A.Value, B)
  else
    Result := -1;
end;

function SDecimal64CompareInt32(const A: SDecimal64; const B: Int32): Integer;
begin
  if A.Sign = 0 then
    begin
      if B < 0 then
        Result := 1
      else
      if B > 0 then
        Result := -1
      else
        Result := 0;
    end
  else
  if A.Sign > 0 then
    begin
      if B <= 0 then
        Result := 1
      else
        Result := Decimal64CompareWord32(A.Value, B);
    end
  else
    begin
      if B >= 0 then
        Result := -1
      else
        Result := -Decimal64CompareWord32(A.Value, -B);
    end;
end;

function SDecimal64CompareInt64(const A: SDecimal64; const B: Int64): Integer;
begin
  if A.Sign = 0 then
    begin
      if B < 0 then
        Result := 1
      else
      if B > 0 then
        Result := -1
      else
        Result := 0;
    end
  else
  if A.Sign > 0 then
    begin
      if B <= 0 then
        Result := 1
      else
        Result := Decimal64CompareInt64(A.Value, B);
    end
  else
    begin
      if B >= 0 then
        Result := -1
      else
        Result := -Decimal64CompareInt64(A.Value, -B);
    end;
end;

function SDecimal64CompareSDecimal64(const A: SDecimal64; const B: SDecimal64): Integer;
begin
  if A.Sign = 0 then
    begin
      if B.Sign < 0 then
        Result := 1
      else
      if B.Sign > 0 then
        Result := -1
      else
        Result := 0;
    end
  else
  if A.Sign > 0 then
    begin
      if B.Sign <= 0 then
        Result := 1
      else
        Result := Decimal64CompareDecimal64(A.Value, B.Value);
    end
  else
    begin
      if B.Sign >= 0 then
        Result := -1
      else
        Result := -Decimal64CompareDecimal64(A.Value, B.Value);
    end;
end;

function SDecimal64CompareFloat(const A: SDecimal64; const B: Double): Integer;
begin
  if A.Sign >= 0 then
    Result := Decimal64CompareFloat(A.Value, B)
  else
    Result := -Decimal64CompareFloat(A.Value, -B);
end;

procedure SDecimal64AddWord8(var A: SDecimal64; const B: Byte);
var S : Int8;
    T : Decimal64;
begin
  if B = 0 then
    exit;
  if A.Sign = 0 then
    begin
      A.Sign := 1;
      Decimal64InitWord8(A.Value, B);
    end
  else
  if A.Sign > 0 then
    Decimal64AddWord8(A.Value, B)
  else
    begin
      S := Decimal64CompareWord8(A.Value, B);
      if S = 0 then
        SDecimal64InitZero(A)
      else
      if S > 0 then
        Decimal64SubtractWord8(A.Value, B)
      else
        begin
          A.Sign := 1;
          Decimal64InitWord8(T, B);
          Decimal64SubtractDecimal64(T, A.Value);
          A.Value := T;
        end;
    end;
end;

procedure SDecimal64AddWord16(var A: SDecimal64; const B: Word);
var S : Int8;
    T : Decimal64;
begin
  if B = 0 then
    exit;
  if A.Sign = 0 then
    begin
      A.Sign := 1;
      Decimal64InitWord16(A.Value, B);
    end
  else
  if A.Sign > 0 then
    Decimal64AddWord16(A.Value, B)
  else
    begin
      S := Decimal64CompareWord16(A.Value, B);
      if S = 0 then
        SDecimal64InitZero(A)
      else
      if S > 0 then
        Decimal64SubtractWord16(A.Value, B)
      else
        begin
          A.Sign := 1;
          Decimal64InitWord16(T, B);
          Decimal64SubtractDecimal64(T, A.Value);
          A.Value := T;
        end;
    end;
end;

procedure SDecimal64AddWord32(var A: SDecimal64; const B: Word32);
var S : Int8;
    T : Decimal64;
begin
  if B = 0 then
    exit;
  if A.Sign = 0 then
    begin
      A.Sign := 1;
      Decimal64InitWord32(A.Value, B);
    end
  else
  if A.Sign > 0 then
    Decimal64AddWord32(A.Value, B)
  else
    begin
      S := Decimal64CompareWord32(A.Value, B);
      if S = 0 then
        SDecimal64InitZero(A)
      else
      if S > 0 then
        Decimal64SubtractWord32(A.Value, B)
      else
        begin
          A.Sign := 1;
          Decimal64InitWord32(T, B);
          Decimal64SubtractDecimal64(T, A.Value);
          A.Value := T;
        end;
    end;
end;

procedure SDecimal64AddSDecimal64(var A: SDecimal64; const B: SDecimal64);
var S : Int8;
    T : Decimal64;
begin
  if B.Sign = 0 then
    exit;
  if A.Sign = 0 then
    begin
      A.Sign := 1;
      Decimal64InitDecimal64(A.Value, B.Value);
    end
  else
  if A.Sign = B.Sign then
    Decimal64AddDecimal64(A.Value, B.Value)
  else
    begin
      S := Decimal64CompareDecimal64(A.Value, B.Value);
      if S = 0 then
        SDecimal64InitZero(A)
      else
      if S > 0 then
        Decimal64SubtractDecimal64(A.Value, B.Value)
      else
        begin
          A.Sign := -A.Sign;
          Decimal64InitDecimal64(T, B.Value);
          Decimal64SubtractDecimal64(T, A.Value);
          A.Value := T;
        end;
    end;
end;

procedure SDecimal64SubtractWord8(var A: SDecimal64; const B: Byte);
var S : Int8;
    T : Decimal64;
begin
  if B = 0 then
    exit;
  if A.Sign = 0 then
    begin
      A.Sign := -1;
      Decimal64InitWord8(A.Value, B);
    end
  else
  if A.Sign < 0 then
    Decimal64AddWord8(A.Value, B)
  else
    begin
      S := Decimal64CompareWord8(A.Value, B);
      if S = 0 then
        SDecimal64InitZero(A)
      else
      if S > 0 then
        Decimal64SubtractWord8(A.Value, B)
      else
        begin
          A.Sign := -1;
          Decimal64InitWord8(T, B);
          Decimal64SubtractDecimal64(T, A.Value);
          A.Value := T;
        end;
    end;
end;

procedure SDecimal64SubtractWord16(var A: SDecimal64; const B: Word);
var S : Int8;
    T : Decimal64;
begin
  if B = 0 then
    exit;
  if A.Sign = 0 then
    begin
      A.Sign := -1;
      Decimal64InitWord16(A.Value, B);
    end
  else
  if A.Sign < 0 then
    Decimal64AddWord8(A.Value, B)
  else
    begin
      S := Decimal64CompareWord16(A.Value, B);
      if S = 0 then
        SDecimal64InitZero(A)
      else
      if S > 0 then
        Decimal64SubtractWord16(A.Value, B)
      else
        begin
          A.Sign := -1;
          Decimal64InitWord16(T, B);
          Decimal64SubtractDecimal64(T, A.Value);
          A.Value := T;
        end;
    end;
end;

procedure SDecimal64SubtractWord32(var A: SDecimal64; const B: Word32);
var S : Int8;
    T : Decimal64;
begin
  if B = 0 then
    exit;
  if A.Sign = 0 then
    begin
      A.Sign := -1;
      Decimal64InitWord32(A.Value, B);
    end
  else
  if A.Sign < 0 then
    Decimal64AddWord32(A.Value, B)
  else
    begin
      S := Decimal64CompareWord32(A.Value, B);
      if S = 0 then
        SDecimal64InitZero(A)
      else
      if S > 0 then
        Decimal64SubtractWord32(A.Value, B)
      else
        begin
          A.Sign := -1;
          Decimal64InitWord32(T, B);
          Decimal64SubtractDecimal64(T, A.Value);
          A.Value := T;
        end;
    end;
end;

procedure SDecimal64SubtractSDecimal64(var A: SDecimal64; const B: SDecimal64);
var S : Int8;
    T : Decimal64;
begin
  if B.Sign = 0 then
    exit;
  if A.Sign = 0 then
    begin
      A.Sign := -1;
      Decimal64InitDecimal64(A.Value, B.Value);
    end
  else
  if A.Sign <> B.Sign then
    Decimal64AddDecimal64(A.Value, B.Value)
  else
    begin
      S := Decimal64CompareDecimal64(A.Value, B.Value);
      if S = 0 then
        SDecimal64InitZero(A)
      else
      if S > 0 then
        Decimal64SubtractDecimal64(A.Value, B.Value)
      else
        begin
          A.Sign := -A.Sign;
          Decimal64InitDecimal64(T, B.Value);
          Decimal64SubtractDecimal64(T, A.Value);
          A.Value := T;
        end;
    end;
end;

procedure SDecimal64MultiplyWord8(var A: SDecimal64; const B: Byte);
begin
  if A.Sign = 0 then
    exit;
  if B = 0 then
    begin
      SDecimal64InitZero(A);
      exit;
    end;
  Decimal64MultiplyWord8(A.Value, B);
end;

procedure SDecimal64MultiplyWord16(var A: SDecimal64; const B: Word);
begin
  if A.Sign = 0 then
    exit;
  if B = 0 then
    begin
      SDecimal64InitZero(A);
      exit;
    end;
  Decimal64MultiplyWord16(A.Value, B);
end;

procedure SDecimal64MultiplyWord32(var A: SDecimal64; const B: Word32);
begin
  if A.Sign = 0 then
    exit;
  if B = 0 then
    begin
      SDecimal64InitZero(A);
      exit;
    end;
  Decimal64MultiplyWord32(A.Value, B);
end;

procedure SDecimal64MultiplySDecimal64(var A: SDecimal64; const B: SDecimal64);
begin
  if A.Sign = 0 then
    exit;
  if B.Sign = 0 then
    begin
      SDecimal64InitZero(A);
      exit;
    end;
  Decimal64MultiplyDecimal64(A.Value, B.Value);
  if A.Sign = B.Sign then
    A.Sign := 1
  else
    A.Sign := -1;
end;

procedure SDecimal64DivideWord8(var A: SDecimal64; const B: Byte);
begin
  if B = 0 then
    raise EDecimalError.Create(SDivisionByZero);
  if A.Sign = 0 then
    exit;
  Decimal64DivideWord8(A.Value, B);
end;

procedure SDecimal64DivideWord16(var A: SDecimal64; const B: Word);
begin
  if B = 0 then
    raise EDecimalError.Create(SDivisionByZero);
  if A.Sign = 0 then
    exit;
  Decimal64DivideWord16(A.Value, B);
end;

procedure SDecimal64DivideWord32(var A: SDecimal64; const B: Word32);
begin
  if B = 0 then
    raise EDecimalError.Create(SDivisionByZero);
  if A.Sign = 0 then
    exit;
  Decimal64DivideWord32(A.Value, B);
end;

procedure SDecimal64DivideSDecimal64(var A: SDecimal64; const B: SDecimal64);
begin
  if B.Sign = 0 then
    raise EDecimalError.Create(SDivisionByZero);
  if A.Sign = 0 then
    exit;
  Decimal64DivideDecimal64(A.Value, B.Value);
  if A.Sign = B.Sign then
    A.Sign := 1
  else
    A.Sign := -1;
end;

function SDecimal64ToStr(const A: SDecimal64): String;
var S : String;
begin
  if A.Sign < 0 then
    S := '-'
  else
    S := '';
  S := S + Decimal64ToStr(A.Value);
  Result := S;
end;

function SDecimal64ToStrB(const A: SDecimal64): RawByteString;
var S : RawByteString;
begin
  if A.Sign < 0 then
    S := '-'
  else
    S := '';
  S := S + Decimal64ToStrB(A.Value);
  Result := S;
end;

function SDecimal64ToStrU(const A: SDecimal64): UnicodeString;
var S : UnicodeString;
begin
  if A.Sign < 0 then
    S := '-'
  else
    S := '';
  S := S + Decimal64ToStrU(A.Value);
  Result := S;
end;

function TryStrToSDecimal64(const A: String; out B: SDecimal64): TDecimalConvertErrorType;
var C : Char;
    T : String;
    R : TDecimalConvertErrorType;
begin
  if A = '' then
    raise EDecimalError.Create(SConvertError);
  C := A[1];
  if (C = '+') or (C = '-') then
    T := CopyFrom(A, 2)
  else
    begin
      C := '+';
      T := A;
    end;
  R := TryStrToDecimal64(T, B.Value);
  if R = dceNoError then
    begin
      if Decimal64IsZero(B.Value) then
        B.Sign := 0
      else
        if C = '+' then
          B.Sign := 1
        else
          B.Sign := -1;
    end;
  Result := R;
end;

function TryStrToSDecimal64B(const A: RawByteString; out B: SDecimal64): TDecimalConvertErrorType;
var C : AnsiChar;
    T : RawByteString;
    R : TDecimalConvertErrorType;
begin
  if A = '' then
    raise EDecimalError.Create(SConvertError);
  C := A[1];
  if (C = '+') or (C = '-') then
    T := CopyFromB(A, 2)
  else
    begin
      C := '+';
      T := A;
    end;
  R := TryStrToDecimal64B(T, B.Value);
  if R = dceNoError then
    begin
      if Decimal64IsZero(B.Value) then
        B.Sign := 0
      else
        if C = '+' then
          B.Sign := 1
        else
          B.Sign := -1;
    end;
  Result := R;
end;

function StrToSDecimal64(const A: String): SDecimal64;
begin
  case TryStrToSDecimal64(A, Result) of
    dceNoError       : ;
    dceConvertError  : raise EDecimalError.Create(SConvertError);
    dceOverflowError : raise EDecimalError.Create(SOverflowError);
  end;
end;

function StrToSDecimal64B(const A: RawByteString): SDecimal64;
begin
  case TryStrToSDecimal64B(A, Result) of
    dceNoError       : ;
    dceConvertError  : raise EDecimalError.Create(SConvertError);
    dceOverflowError : raise EDecimalError.Create(SOverflowError);
  end;
end;


{                                                                              }
{ SDecimal128                                                                  }
{                                                                              }
procedure SDecimal128InitZero(var A: SDecimal128);
begin
  A.Sign := 0;
  Decimal128InitZero(A.Value);
end;

procedure SDecimal128InitOne(var A: SDecimal128);
begin
  A.Sign := 1;
  Decimal128InitOne(A.Value);
end;

procedure SDecimal128InitMinusOne(var A: SDecimal128);
begin
  A.Sign := -1;
  Decimal128InitOne(A.Value);
end;

procedure SDecimal128InitMin(var A: SDecimal128);
begin
  A.Sign := -1;
  Decimal128InitMax(A.Value);
end;

procedure SDecimal128InitMax(var A: SDecimal128);
begin
  A.Sign := 1;
  Decimal128InitMax(A.Value);
end;

function SDecimal128IsZero(const A: SDecimal128): Boolean;
begin
  Result := A.Sign = 0;
end;

function SDecimal128IsOne(const A: SDecimal128): Boolean;
begin
  Result := (A.Sign > 0) and Decimal128IsOne(A.Value);
end;

function SDecimal128IsMinusOne(const A: SDecimal128): Boolean;
begin
  Result := (A.Sign < 0) and Decimal128IsOne(A.Value);
end;

function SDecimal128IsMinimum(const A: SDecimal128): Boolean;
begin
  Result := (A.Sign < 0) and Decimal128IsMaximum(A.Value);
end;

function SDecimal128IsMaximum(const A: SDecimal128): Boolean;
begin
  Result := (A.Sign > 0) and Decimal128IsMaximum(A.Value);
end;

function SDecimal128IsOverflow(const A: SDecimal128): Boolean;
begin
  Result := Decimal128IsOverflow(A.Value);
end;

function Word64IsSDecimal128Range(const A: Word64Rec): Boolean;
begin
  Result := Word64CompareWord64(A, Decimal128MaxInt) <= 0;
end;

function Word128IsSDecimal128Range(const A: Word128): Boolean;
begin
  Result := Word128CompareWord64(A, Decimal128MaxInt) <= 0;
end;

function Int128IsSDecimal128Range(const A: Int128): Boolean;
var T : Int128;
begin
  T := A;
  Int128AbsInPlace(T);
  Result := Int128CompareWord64(T, Decimal128MaxInt) <= 0;
end;

function FloatIsSDecimal128Range(const A: Double): Boolean;
begin
  Result :=
    (A > SDecimal128MinFloatLimD) and
    (A < SDecimal128MaxFloatLimD);
end;

procedure SDecimal128InitWord8(var A: SDecimal128; const B: Byte);
begin
  if B = 0 then
    SDecimal128InitZero(A)
  else
    begin
      A.Sign := 1;
      Decimal128InitWord8(A.Value, B);
    end;
end;

function SDecimal128Sign(const A: SDecimal128): Integer;
begin
  Result := A.Sign;
end;

procedure SDecimal128Negate(var A: SDecimal128);
begin
  A.Sign := -A.Sign;
end;

procedure SDecimal128AbsInPlace(var A: SDecimal128);
begin
  if A.Sign < 0 then
    A.Sign := 1;
end;

procedure SDecimal128InitWord16(var A: SDecimal128; const B: Word);
begin
  if B = 0 then
    SDecimal128InitZero(A)
  else
    begin
      A.Sign := 1;
      Decimal128InitWord16(A.Value, B);
    end;
end;

procedure SDecimal128InitWord32(var A: SDecimal128; const B: Word32);
begin
  if B = 0 then
    SDecimal128InitZero(A)
  else
    begin
      A.Sign := 1;
      Decimal128InitWord32(A.Value, B);
    end;
end;

procedure SDecimal128InitWord64(var A: SDecimal128; const B: Word64Rec);
begin
  if Word64IsZero(B) then
    SDecimal128InitZero(A)
  else
    begin
      A.Sign := 1;
      Decimal128InitWord64(A.Value, B);
    end;
end;

procedure SDecimal128InitInt32(var A: SDecimal128; const B: Int32);
begin
  if B = 0 then
    SDecimal128InitZero(A)
  else
  if B < 0 then
    begin
      A.Sign := -1;
      Decimal128InitWord32(A.Value, -B);
    end
  else
    begin
      A.Sign := 1;
      Decimal128InitWord32(A.Value, B);
    end;
end;

procedure SDecimal128InitInt64(var A: SDecimal128; const B: Int64);
begin
  if B = 0 then
    SDecimal128InitZero(A)
  else
  if B < 0 then
    begin
      A.Sign := -1;
      Decimal128InitInt64(A.Value, -B);
    end
  else
    begin
      A.Sign := 1;
      Decimal128InitInt64(A.Value, B);
    end;
end;

procedure SDecimal128InitSDecimal128(var A: SDecimal128; const B: SDecimal128);
begin
  A.Sign := B.Sign;
  A.Value := B.Value;
end;

procedure SDecimal128InitFloat(var A: SDecimal128; const B: Double);
begin
  if B < Decimal128MinFloatD then
    begin
      A.Sign := -1;
      Decimal128InitFloat(A.Value, -B);
    end
  else
  if B > -Decimal128MinFloatD then
    begin
      A.Sign := 1;
      Decimal128InitFloat(A.Value, B);
    end
  else
    SDecimal128InitZero(A);
end;

function SDecimal128ToWord8(const A: SDecimal128): Byte;
begin
  if A.Sign < 0 then
    raise EDecimalError.Create(SOverflowError);
  Result := Decimal128ToWord8(A.Value);
end;

function SDecimal128ToWord16(const A: SDecimal128): Word;
begin
  if A.Sign < 0 then
    raise EDecimalError.Create(SOverflowError);
  Result := Decimal128ToWord16(A.Value);
end;

function SDecimal128ToWord32(const A: SDecimal128): Word32;
begin
  if A.Sign < 0 then
    raise EDecimalError.Create(SOverflowError);
  Result := Decimal128ToWord32(A.Value);
end;

function SDecimal128ToWord64(const A: SDecimal128): Word64Rec;
begin
  if A.Sign < 0 then
    raise EDecimalError.Create(SOverflowError);
  Result := Decimal128ToWord64(A.Value);
end;

function SDecimal128ToInt32(const A: SDecimal128): Int32;
var T : Int32;
begin
  T := Decimal128ToInt32(A.Value);
  if A.Sign < 0 then
    T := -T;
  Result := T;
end;

function SDecimal128ToInt64(const A: SDecimal128): Int64;
var T : Int64;
begin
  T := Decimal128ToInt64(A.Value);
  if A.Sign < 0 then
    T := -T;
  Result := T;
end;

function SDecimal128ToFloat(const A: SDecimal128): Double;
var T : Double;
begin
  T := Decimal128ToFloat(A.Value);
  if A.Sign < 0 then
    T := -T;
  Result := T;
end;

function SDecimal128Trunc(const A: SDecimal128): Int128;
var T : Word64Rec;
    Q : Int128;
begin
  T := Decimal128Trunc(A.Value);
  Int128InitWord64(Q, T);
  if A.Sign < 0 then
    Int128Negate(Q);
  Result := Q;
end;

function SDecimal128Round(const A: SDecimal128): Int128;
var T : Word64Rec;
    Q : Int128;
begin
  T := Decimal128Round(A.Value);
  Int128InitWord64(Q, T);
  if A.Sign < 0 then
    Int128Negate(Q);
  Result := Q;
end;

function SDecimal128FracWord(const A: SDecimal128): Word64Rec;
begin
  Result := Decimal128FracWord(A.Value);
end;

function SDecimal128EqualsWord8(const A: SDecimal128; const B: Byte): Boolean;
begin
  Result := (A.Sign >= 0) and Decimal128EqualsWord8(A.Value, B);
end;

function SDecimal128EqualsWord16(const A: SDecimal128; const B: Word): Boolean;
begin
  Result := (A.Sign >= 0) and Decimal128EqualsWord16(A.Value, B);
end;

function SDecimal128EqualsWord32(const A: SDecimal128; const B: Word32): Boolean;
begin
  Result := (A.Sign >= 0) and Decimal128EqualsWord32(A.Value, B);
end;

function SDecimal128EqualsWord64(const A: SDecimal128; const B: Word64Rec): Boolean;
begin
  Result := (A.Sign >= 0) and Decimal128EqualsWord64(A.Value, B);
end;

function SDecimal128EqualsInt32(const A: SDecimal128; const B: Int32): Boolean;
begin
  if A.Sign = 0 then
    Result := (B = 0)
  else
  if A.Sign > 0 then
    begin
      if B <= 0 then
        Result := False
      else
        Result := Decimal128EqualsWord32(A.Value, B);
    end
  else
    begin
      if B >= 0 then
        Result := False
      else
        Result := Decimal128EqualsWord32(A.Value, -B);
    end;
end;

function SDecimal128EqualsInt64(const A: SDecimal128; const B: Int64): Boolean;
begin
  if A.Sign = 0 then
    Result := (B = 0)
  else
  if A.Sign > 0 then
    begin
      if B <= 0 then
        Result := False
      else
        Result := Decimal128EqualsInt64(A.Value, B);
    end
  else
    begin
      if B >= 0 then
        Result := False
      else
        Result := Decimal128EqualsInt64(A.Value, -B);
    end;
end;

function SDecimal128EqualsSDecimal128(const A: SDecimal128; const B: SDecimal128): Boolean;
begin
  Result :=
    (A.Sign = B.Sign) and
    Decimal128EqualSDecimal128(A.Value, B.Value);
end;

function SDecimal128EqualsFloat(const A: SDecimal128; const B: Double): Boolean;
begin
  if A.Sign >= 0 then
    Result := Decimal128EqualsFloat(A.Value, B)
  else
    Result := Decimal128EqualsFloat(A.Value, -B);
end;

function SDecimal128CompareWord8(const A: SDecimal128; const B: Byte): Integer;
begin
  if A.Sign = 0 then
    if B = 0 then
      Result := 0
    else
      Result := -1
  else
  if A.Sign > 0 then
    Result := Decimal128CompareWord8(A.Value, B)
  else
    Result := -1;
end;

function SDecimal128CompareWord16(const A: SDecimal128; const B: Word): Integer;
begin
  if A.Sign = 0 then
    if B = 0 then
      Result := 0
    else
      Result := -1
  else
  if A.Sign > 0 then
    Result := Decimal128CompareWord16(A.Value, B)
  else
    Result := -1;
end;

function SDecimal128CompareWord32(const A: SDecimal128; const B: Word32): Integer;
begin
  if A.Sign = 0 then
    if B = 0 then
      Result := 0
    else
      Result := -1
  else
  if A.Sign > 0 then
    Result := Decimal128CompareWord32(A.Value, B)
  else
    Result := -1;
end;

function SDecimal128CompareWord64(const A: SDecimal128; const B: Word64Rec): Integer;
begin
  if A.Sign = 0 then
    if Word64IsZero(B) then
      Result := 0
    else
      Result := -1
  else
  if A.Sign > 0 then
    Result := Decimal128CompareWord64(A.Value, B)
  else
    Result := -1;
end;

function SDecimal128CompareInt32(const A: SDecimal128; const B: Int32): Integer;
begin
  if A.Sign = 0 then
    begin
      if B < 0 then
        Result := 1
      else
      if B > 0 then
        Result := -1
      else
        Result := 0;
    end
  else
  if A.Sign > 0 then
    begin
      if B <= 0 then
        Result := 1
      else
        Result := Decimal128CompareWord32(A.Value, B);
    end
  else
    begin
      if B >= 0 then
        Result := -1
      else
        Result := -Decimal128CompareWord32(A.Value, -B);
    end;
end;

function SDecimal128CompareInt64(const A: SDecimal128; const B: Int64): Integer;
var T : Word64Rec;
begin
  if A.Sign = 0 then
    begin
      if B < 0 then
        Result := 1
      else
      if B > 0 then
        Result := -1
      else
        Result := 0;
    end
  else
  if A.Sign > 0 then
    begin
      if B <= 0 then
        Result := 1
      else
        begin
          Word64InitInt64(T, B);
          Result := Decimal128CompareWord64(A.Value, T);
        end;
    end
  else
    begin
      if B >= 0 then
        Result := -1
      else
        begin
          Word64InitInt64(T, -B);
          Result := -Decimal128CompareWord64(A.Value, T);
        end;
    end;
end;

function SDecimal128CompareSDecimal128(const A: SDecimal128; const B: SDecimal128): Integer;
begin
  if A.Sign = 0 then
    begin
      if B.Sign < 0 then
        Result := 1
      else
      if B.Sign > 0 then
        Result := -1
      else
        Result := 0;
    end
  else
  if A.Sign > 0 then
    begin
      if B.Sign <= 0 then
        Result := 1
      else
        Result := Decimal128CompareDecimal128(A.Value, B.Value);
    end
  else
    begin
      if B.Sign >= 0 then
        Result := -1
      else
        Result := -Decimal128CompareDecimal128(A.Value, B.Value);
    end;
end;

function SDecimal128CompareFloat(const A: SDecimal128; const B: Double): Integer;
begin
  if A.Sign >= 0 then
    Result := Decimal128CompareFloat(A.Value, B)
  else
    Result := -Decimal128CompareFloat(A.Value, -B);
end;

procedure SDecimal128AddWord8(var A: SDecimal128; const B: Byte);
var S : Int8;
    T : Decimal128;
begin
  if B = 0 then
    exit;
  if A.Sign = 0 then
    begin
      A.Sign := 1;
      Decimal128InitWord8(A.Value, B);
    end
  else
  if A.Sign > 0 then
    Decimal128AddWord8(A.Value, B)
  else
    begin
      S := Decimal128CompareWord8(A.Value, B);
      if S = 0 then
        SDecimal128InitZero(A)
      else
      if S > 0 then
        Decimal128SubtractWord8(A.Value, B)
      else
        begin
          A.Sign := 1;
          Decimal128InitWord8(T, B);
          Decimal128SubtractDecimal128(T, A.Value);
          A.Value := T;
        end;
    end;
end;

procedure SDecimal128AddWord16(var A: SDecimal128; const B: Word);
var S : Int8;
    T : Decimal128;
begin
  if B = 0 then
    exit;
  if A.Sign = 0 then
    begin
      A.Sign := 1;
      Decimal128InitWord16(A.Value, B);
    end
  else
  if A.Sign > 0 then
    Decimal128AddWord16(A.Value, B)
  else
    begin
      S := Decimal128CompareWord16(A.Value, B);
      if S = 0 then
        SDecimal128InitZero(A)
      else
      if S > 0 then
        Decimal128SubtractWord16(A.Value, B)
      else
        begin
          A.Sign := 1;
          Decimal128InitWord16(T, B);
          Decimal128SubtractDecimal128(T, A.Value);
          A.Value := T;
        end;
    end;
end;

procedure SDecimal128AddWord32(var A: SDecimal128; const B: Word32);
var S : Int8;
    T : Decimal128;
begin
  if B = 0 then
    exit;
  if A.Sign = 0 then
    begin
      A.Sign := 1;
      Decimal128InitWord32(A.Value, B);
    end
  else
  if A.Sign > 0 then
    Decimal128AddWord32(A.Value, B)
  else
    begin
      S := Decimal128CompareWord32(A.Value, B);
      if S = 0 then
        SDecimal128InitZero(A)
      else
      if S > 0 then
        Decimal128SubtractWord32(A.Value, B)
      else
        begin
          A.Sign := 1;
          Decimal128InitWord32(T, B);
          Decimal128SubtractDecimal128(T, A.Value);
          A.Value := T;
        end;
    end;
end;

procedure SDecimal128AddWord64(var A: SDecimal128; const B: Word64Rec);
var S : Int8;
    T : Decimal128;
begin
  if Word64IsZero(B) then
    exit;
  if A.Sign = 0 then
    begin
      A.Sign := 1;
      Decimal128InitWord64(A.Value, B);
    end
  else
  if A.Sign > 0 then
    Decimal128AddWord64(A.Value, B)
  else
    begin
      S := Decimal128CompareWord64(A.Value, B);
      if S = 0 then
        SDecimal128InitZero(A)
      else
      if S > 0 then
        Decimal128SubtractWord64(A.Value, B)
      else
        begin
          A.Sign := 1;
          Decimal128InitWord64(T, B);
          Decimal128SubtractDecimal128(T, A.Value);
          A.Value := T;
        end;
    end;
end;

procedure SDecimal128AddSDecimal128(var A: SDecimal128; const B: SDecimal128);
var S : Int8;
    T : Decimal128;
begin
  if B.Sign = 0 then
    exit;
  if A.Sign = 0 then
    begin
      A.Sign := 1;
      Decimal128InitDecimal128(A.Value, B.Value);
    end
  else
  if A.Sign = B.Sign then
    Decimal128AddDecimal128(A.Value, B.Value)
  else
    begin
      S := Decimal128CompareDecimal128(A.Value, B.Value);
      if S = 0 then
        SDecimal128InitZero(A)
      else
      if S > 0 then
        Decimal128SubtractDecimal128(A.Value, B.Value)
      else
        begin
          A.Sign := -A.Sign;
          Decimal128InitDecimal128(T, B.Value);
          Decimal128SubtractDecimal128(T, A.Value);
          A.Value := T;
        end;
    end;
end;

procedure SDecimal128SubtractWord8(var A: SDecimal128; const B: Byte);
var S : Int8;
    T : Decimal128;
begin
  if B = 0 then
    exit;
  if A.Sign = 0 then
    begin
      A.Sign := -1;
      Decimal128InitWord8(A.Value, B);
    end
  else
  if A.Sign < 0 then
    Decimal128AddWord8(A.Value, B)
  else
    begin
      S := Decimal128CompareWord8(A.Value, B);
      if S = 0 then
        SDecimal128InitZero(A)
      else
      if S > 0 then
        Decimal128SubtractWord8(A.Value, B)
      else
        begin
          A.Sign := -1;
          Decimal128InitWord8(T, B);
          Decimal128SubtractDecimal128(T, A.Value);
          A.Value := T;
        end;
    end;
end;

procedure SDecimal128SubtractWord16(var A: SDecimal128; const B: Word);
var S : Int8;
    T : Decimal128;
begin
  if B = 0 then
    exit;
  if A.Sign = 0 then
    begin
      A.Sign := -1;
      Decimal128InitWord16(A.Value, B);
    end
  else
  if A.Sign < 0 then
    Decimal128AddWord8(A.Value, B)
  else
    begin
      S := Decimal128CompareWord16(A.Value, B);
      if S = 0 then
        SDecimal128InitZero(A)
      else
      if S > 0 then
        Decimal128SubtractWord16(A.Value, B)
      else
        begin
          A.Sign := -1;
          Decimal128InitWord16(T, B);
          Decimal128SubtractDecimal128(T, A.Value);
          A.Value := T;
        end;
    end;
end;

procedure SDecimal128SubtractWord32(var A: SDecimal128; const B: Word32);
var S : Int8;
    T : Decimal128;
begin
  if B = 0 then
    exit;
  if A.Sign = 0 then
    begin
      A.Sign := -1;
      Decimal128InitWord32(A.Value, B);
    end
  else
  if A.Sign < 0 then
    Decimal128AddWord32(A.Value, B)
  else
    begin
      S := Decimal128CompareWord32(A.Value, B);
      if S = 0 then
        SDecimal128InitZero(A)
      else
      if S > 0 then
        Decimal128SubtractWord32(A.Value, B)
      else
        begin
          A.Sign := -1;
          Decimal128InitWord32(T, B);
          Decimal128SubtractDecimal128(T, A.Value);
          A.Value := T;
        end;
    end;
end;

procedure SDecimal128SubtractWord64(var A: SDecimal128; const B: Word64Rec);
var S : Int8;
    T : Decimal128;
begin
  if Word64IsZero(B) then
    exit;
  if A.Sign = 0 then
    begin
      A.Sign := -1;
      Decimal128InitWord64(A.Value, B);
    end
  else
  if A.Sign < 0 then
    Decimal128AddWord64(A.Value, B)
  else
    begin
      S := Decimal128CompareWord64(A.Value, B);
      if S = 0 then
        SDecimal128InitZero(A)
      else
      if S > 0 then
        Decimal128SubtractWord64(A.Value, B)
      else
        begin
          A.Sign := -1;
          Decimal128InitWord64(T, B);
          Decimal128SubtractDecimal128(T, A.Value);
          A.Value := T;
        end;
    end;
end;

procedure SDecimal128SubtractSDecimal128(var A: SDecimal128; const B: SDecimal128);
var S : Int8;
    T : Decimal128;
begin
  if B.Sign = 0 then
    exit;
  if A.Sign = 0 then
    begin
      A.Sign := -1;
      Decimal128InitDecimal128(A.Value, B.Value);
    end
  else
  if A.Sign <> B.Sign then
    Decimal128AddDecimal128(A.Value, B.Value)
  else
    begin
      S := Decimal128CompareDecimal128(A.Value, B.Value);
      if S = 0 then
        SDecimal128InitZero(A)
      else
      if S > 0 then
        Decimal128SubtractDecimal128(A.Value, B.Value)
      else
        begin
          A.Sign := -A.Sign;
          Decimal128InitDecimal128(T, B.Value);
          Decimal128SubtractDecimal128(T, A.Value);
          A.Value := T;
        end;
    end;
end;

procedure SDecimal128MultiplyWord8(var A: SDecimal128; const B: Byte);
begin
  if A.Sign = 0 then
    exit;
  if B = 0 then
    begin
      SDecimal128InitZero(A);
      exit;
    end;
  Decimal128MultiplyWord8(A.Value, B);
end;

procedure SDecimal128MultiplyWord16(var A: SDecimal128; const B: Word);
begin
  if A.Sign = 0 then
    exit;
  if B = 0 then
    begin
      SDecimal128InitZero(A);
      exit;
    end;
  Decimal128MultiplyWord16(A.Value, B);
end;

procedure SDecimal128MultiplyWord32(var A: SDecimal128; const B: Word32);
begin
  if A.Sign = 0 then
    exit;
  if B = 0 then
    begin
      SDecimal128InitZero(A);
      exit;
    end;
  Decimal128MultiplyWord32(A.Value, B);
end;

procedure SDecimal128MultiplyWord64(var A: SDecimal128; const B: Word64Rec);
begin
  if A.Sign = 0 then
    exit;
  if Word64IsZero(B) then
    begin
      SDecimal128InitZero(A);
      exit;
    end;
  Decimal128MultiplyWord64(A.Value, B);
end;

procedure SDecimal128MultiplySDecimal128(var A: SDecimal128; const B: SDecimal128);
begin
  if A.Sign = 0 then
    exit;
  if B.Sign = 0 then
    begin
      SDecimal128InitZero(A);
      exit;
    end;
  Decimal128MultiplyDecimal128(A.Value, B.Value);
  if A.Sign = B.Sign then
    A.Sign := 1
  else
    A.Sign := -1;
end;

procedure SDecimal128DivideWord8(var A: SDecimal128; const B: Byte);
begin
  if B = 0 then
    raise EDecimalError.Create(SDivisionByZero);
  if A.Sign = 0 then
    exit;
  Decimal128DivideWord8(A.Value, B);
end;

procedure SDecimal128DivideWord16(var A: SDecimal128; const B: Word);
begin
  if B = 0 then
    raise EDecimalError.Create(SDivisionByZero);
  if A.Sign = 0 then
    exit;
  Decimal128DivideWord16(A.Value, B);
end;

procedure SDecimal128DivideWord32(var A: SDecimal128; const B: Word32);
begin
  if B = 0 then
    raise EDecimalError.Create(SDivisionByZero);
  if A.Sign = 0 then
    exit;
  Decimal128DivideWord32(A.Value, B);
end;

procedure SDecimal128DivideWord64(var A: SDecimal128; const B: Word64Rec);
begin
  if Word64IsZero(B) then
    raise EDecimalError.Create(SDivisionByZero);
  if A.Sign = 0 then
    exit;
  Decimal128DivideWord64(A.Value, B);
end;

procedure SDecimal128DivideSDecimal128(var A: SDecimal128; const B: SDecimal128);
begin
  if B.Sign = 0 then
    raise EDecimalError.Create(SDivisionByZero);
  if A.Sign = 0 then
    exit;
  Decimal128DivideDecimal128(A.Value, B.Value);
  if A.Sign = B.Sign then
    A.Sign := 1
  else
    A.Sign := -1;
end;

function SDecimal128ToStr(const A: SDecimal128): String;
var S : String;
begin
  if A.Sign < 0 then
    S := '-'
  else
    S := '';
  S := S + Decimal128ToStr(A.Value);
  Result := S;
end;

function SDecimal128ToStrB(const A: SDecimal128): RawByteString;
var S : RawByteString;
begin
  if A.Sign < 0 then
    S := '-'
  else
    S := '';
  S := S + Decimal128ToStrB(A.Value);
  Result := S;
end;

function SDecimal128ToStrU(const A: SDecimal128): UnicodeString;
var S : UnicodeString;
begin
  if A.Sign < 0 then
    S := '-'
  else
    S := '';
  S := S + Decimal128ToStrU(A.Value);
  Result := S;
end;

function TryStrToSDecimal128(const A: String; out B: SDecimal128): TDecimalConvertErrorType;
var C : Char;
    T : String;
    R : TDecimalConvertErrorType;
begin
  if A = '' then
    raise EDecimalError.Create(SConvertError);
  C := A[1];
  if (C = '+') or (C = '-') then
    T := CopyFrom(A, 2)
  else
    begin
      C := '+';
      T := A;
    end;
  R := TryStrToDecimal128(T, B.Value);
  if R = dceNoError then
    begin
      if Decimal128IsZero(B.Value) then
        B.Sign := 0
      else
        if C = '+' then
          B.Sign := 1
        else
          B.Sign := -1;
    end;
  Result := R;
end;

function TryStrToSDecimal128B(const A: RawByteString; out B: SDecimal128): TDecimalConvertErrorType;
var C : AnsiChar;
    T : RawByteString;
    R : TDecimalConvertErrorType;
begin
  if A = '' then
    raise EDecimalError.Create(SConvertError);
  C := A[1];
  if (C = '+') or (C = '-') then
    T := CopyFromB(A, 2)
  else
    begin
      C := '+';
      T := A;
    end;
  R := TryStrToDecimal128B(T, B.Value);
  if R = dceNoError then
    begin
      if Decimal128IsZero(B.Value) then
        B.Sign := 0
      else
        if C = '+' then
          B.Sign := 1
        else
          B.Sign := -1;
    end;
  Result := R;
end;

function StrToSDecimal128(const A: String): SDecimal128;
begin
  case TryStrToSDecimal128(A, Result) of
    dceNoError       : ;
    dceConvertError  : raise EDecimalError.Create(SConvertError);
    dceOverflowError : raise EDecimalError.Create(SOverflowError);
  end;
end;

function StrToSDecimal128B(const A: RawByteString): SDecimal128;
begin
  case TryStrToSDecimal128B(A, Result) of
    dceNoError       : ;
    dceConvertError  : raise EDecimalError.Create(SConvertError);
    dceOverflowError : raise EDecimalError.Create(SOverflowError);
  end;
end;



{                                                                              }
{ SHugeDecimal                                                                 }
{                                                                              }
procedure SHugeDecimalInit(out A: SHugeDecimal);
begin
  A.Sign := 0;
  HugeDecimalInit(A.Value);
end;

procedure SHugeDecimalInitZero(out A: SHugeDecimal);
begin
  SHugeDecimalInit(A);
  SHugeDecimalAssignZero(A);
end;

procedure SHugeDecimalInitOne(out A: SHugeDecimal);
begin
  SHugeDecimalInit(A);
  SHugeDecimalAssignOne(A);
end;

procedure SHugeDecimalInitMinusOne(out A: SHugeDecimal);
begin
  SHugeDecimalInit(A);
  SHugeDecimalAssignMinusOne(A);
end;

procedure SHugeDecimalAssignZero(var A: SHugeDecimal);
begin
  A.Sign := 0;
  HugeDecimalAssignZero(A.Value);
end;

procedure SHugeDecimalAssignOne(var A: SHugeDecimal);
begin
  A.Sign := 1;
  HugeDecimalAssignOne(A.Value);
end;

procedure SHugeDecimalAssignMinusOne(var A: SHugeDecimal);
begin
  A.Sign := -1;
  HugeDecimalAssignOne(A.Value);
end;

procedure SHugeDecimalAssignWord8(var A: SHugeDecimal; const B: Byte);
begin
  if B = 0 then
    SHugeDecimalAssignZero(A)
  else
    begin
      A.Sign := 1;
      HugeDecimalAssignWord8(A.Value, B);
    end;
end;

procedure SHugeDecimalAssignWord32(var A: SHugeDecimal; const B: Word32);
begin
  if B = 0 then
    SHugeDecimalAssignZero(A)
  else
    begin
      A.Sign := 1;
      HugeDecimalAssignWord32(A.Value, B);
    end;
end;

procedure SHugeDecimalAssignWord64(var A: SHugeDecimal; const B: Word64Rec);
begin
  if Word64IsZero(B) then
    SHugeDecimalAssignZero(A)
  else
    begin
      A.Sign := 1;
      HugeDecimalAssignWord64(A.Value, B);
    end;
end;

procedure SHugeDecimalAssignWord128(var A: SHugeDecimal; const B: Word128);
begin
  if Word128IsZero(B) then
    SHugeDecimalAssignZero(A)
  else
    begin
      A.Sign := 1;
      HugeDecimalAssignWord128(A.Value, B);
    end;
end;

procedure SHugeDecimalAssignInt8(var A: SHugeDecimal; const B: ShortInt);
begin
  if B = 0 then
    SHugeDecimalAssignZero(A)
  else
    begin
      if B < 0 then
        A.Sign := -1
      else
        A.Sign := 1;
      HugeDecimalAssignWord8(A.Value, Abs(Int32(B)));
    end;
end;

procedure SHugeDecimalAssignInt32(var A: SHugeDecimal; const B: Int32);
begin
  if B = 0 then
    SHugeDecimalAssignZero(A)
  else
    begin
      if B < 0 then
        A.Sign := -1
      else
        A.Sign := 1;
      HugeDecimalAssignWord32(A.Value, Abs(Int64(B)));
    end;
end;

procedure SHugeDecimalAssignInt64(var A: SHugeDecimal; const B: Int64);
var
  R : Int128;
begin
  if B = 0 then
    SHugeDecimalAssignZero(A)
  else
    begin
      if B < 0 then
        A.Sign := -1
      else
        A.Sign := 1;
      Int128InitInt64(R, B);
      Int128AbsInPlace(R);
      HugeDecimalAssignWord64(A.Value, Int128ToWord64(R));
    end;
end;

procedure SHugeDecimalAssignDecimal32(var A: SHugeDecimal; const B: Decimal32);
begin
  if Decimal32IsZero(B) then
    SHugeDecimalAssignZero(A)
  else
    begin
      A.Sign := 1;
      HugeDecimalAssignDecimal32(A.Value, B);
    end;
end;

procedure SHugeDecimalAssignDecimal64(var A: SHugeDecimal; const B: Decimal64);
begin
  if Decimal64IsZero(B) then
    SHugeDecimalAssignZero(A)
  else
    begin
      A.Sign := 1;
      HugeDecimalAssignDecimal64(A.Value, B);
    end;
end;

procedure SHugeDecimalAssignDecimal128(var A: SHugeDecimal; const B: Decimal128);
begin
  if Decimal128IsZero(B) then
    SHugeDecimalAssignZero(A)
  else
    begin
      A.Sign := 1;
      HugeDecimalAssignDecimal128(A.Value, B);
    end;
end;

procedure SHugeDecimalAssignHugeDecimal(var A: SHugeDecimal; const B: HugeDecimal);
begin
  if HugeDecimalIsZero(B) then
    SHugeDecimalAssignZero(A)
  else
    begin
      A.Sign := 1;
      HugeDecimalAssignHugeDecimal(A.Value, B);
    end;
end;

procedure SHugeDecimalAssignSHugeDecimal(var A: SHugeDecimal; const B: SHugeDecimal);
begin
  A.Sign := B.Sign;
  HugeDecimalAssignHugeDecimal(A.Value, B.Value);
end;

function SHugeDecimalIsZero(const A: SHugeDecimal): Boolean;
begin
  Result := A.Sign = 0;
end;

function SHugeDecimalIsOne(const A: SHugeDecimal): Boolean;
begin
  Result := (A.Sign = 1) and HugeDecimalIsOne(A.Value);
end;

function SHugeDecimalIsMinusOne(const A: SHugeDecimal): Boolean;
begin
  Result := (A.Sign = -1) and HugeDecimalIsOne(A.Value);
end;

function SHugeDecimalSign(const A: SHugeDecimal): Integer;
begin
  Result := A.Sign;
end;

procedure SHugeDecimalNegate(var A: SHugeDecimal);
begin
  if A.Sign <> 0 then
    A.Sign := -A.Sign;
end;

procedure SHugeDecimalAbsInPlace(var A: SHugeDecimal);
begin
  if A.Sign < 0 then
    A.Sign := 1;
end;

function SHugeDecimalToWord8(const A: SHugeDecimal): Byte;
begin
  if A.Sign = 0 then
    Result := 0
  else
  if A.Sign < 0 then
    raise EDecimalError.Create(SOverflowError)
  else
    Result := HugeDecimalToWord8(A.Value);
end;

function SHugeDecimalToWord32(const A: SHugeDecimal): Word32;
begin
  if A.Sign = 0 then
    Result := 0
  else
  if A.Sign < 0 then
    raise EDecimalError.Create(SOverflowError)
  else
    Result := HugeDecimalToWord32(A.Value);
end;

function SHugeDecimalToWord64(const A: SHugeDecimal): Word64Rec;
begin
  if A.Sign = 0 then
    Word64InitZero(Result)
  else
  if A.Sign < 0 then
    raise EDecimalError.Create(SOverflowError)
  else
    Result := HugeDecimalToWord64(A.Value);
end;

function SHugeDecimalToWord128(const A: SHugeDecimal): Word128;
begin
  if A.Sign = 0 then
    Word128InitZero(Result)
  else
  if A.Sign < 0 then
    raise EDecimalError.Create(SOverflowError)
  else
    Result := HugeDecimalToWord128(A.Value);
end;

function SHugeDecimalToInt8(const A: SHugeDecimal): ShortInt;
var
  T : Int16;
begin
  if A.Sign = 0 then
    Result := 0
  else
    begin
      T := HugeDecimalToWord8(A.Value);
      if A.Sign < 0 then
        T := -T;
      if not Int16IsInt8Range(T) then
        raise EDecimalError.Create(SOverflowError);
      Result := T;
    end;
end;

function SHugeDecimalToInt32(const A: SHugeDecimal): Int32;
var
  T : Int64;
begin
  if A.Sign = 0 then
    Result := 0
  else
    begin
      T := HugeDecimalToWord32(A.Value);
      if A.Sign < 0 then
        T := -T;
      if not Int64IsInt32Range(T) then
        raise EDecimalError.Create(SOverflowError);
      Result := T;
    end;
end;

function SHugeDecimalToInt64(const A: SHugeDecimal): Int64;
var
  T : Int128;
begin
  if A.Sign = 0 then
    Result := 0
  else
    begin
      Int128InitWord64(T, HugeDecimalToWord64(A.Value));
      if A.Sign < 0 then
        Int128Negate(T);
      if not Int128IsInt64Range(T) then
        raise EDecimalError.Create(SOverflowError);
      Result := Int128ToInt64(T);
    end;
end;

function SHugeDecimalToDecimal32(const A: SHugeDecimal): Decimal32;
begin
  if A.Sign = 0 then
    Decimal32InitZero(Result)
  else
  if A.Sign < 0 then
    raise EDecimalError.Create(SOverflowError)
  else
    Result := HugeDecimalToDecimal32(A.Value);
end;

function SHugeDecimalToDecimal64(const A: SHugeDecimal): Decimal64;
begin
  if A.Sign = 0 then
    Decimal64InitZero(Result)
  else
  if A.Sign < 0 then
    raise EDecimalError.Create(SOverflowError)
  else
    Result := HugeDecimalToDecimal64(A.Value);
end;

function SHugeDecimalToDecimal128(const A: SHugeDecimal): Decimal128;
begin
  if A.Sign = 0 then
    Decimal128InitZero(Result)
  else
  if A.Sign < 0 then
    raise EDecimalError.Create(SOverflowError)
  else
    Result := HugeDecimalToDecimal128(A.Value);
end;

procedure SHugeDecimalTrunc(var A: SHugeDecimal);
begin
  if A.Sign = 0 then
    exit;
  HugeDecimalTrunc(A.Value);
end;

function SHugeDecimalFracCompareHalf(var A: SHugeDecimal): Integer;
begin
  if A.Sign = 0 then
    Result := -1
  else
    Result := HugeDecimalFracCompareHalf(A.Value);
end;

procedure SHugeDecimalRound(var A: SHugeDecimal);
begin
  if A.Sign = 0 then
    exit;
  HugeDecimalRound(A.Value);
end;

function SHugeDecimalEqualsWord8(const A: SHugeDecimal; const B: Byte): Boolean;
begin
  if A.Sign = 0 then
    Result := (B = 0)
  else
  if A.Sign > 0 then
    Result := HugeDecimalEqualsWord8(A.Value, B)
  else
    Result := False;
end;

function SHugeDecimalEqualsHugeDecimal(const A: SHugeDecimal; const B: HugeDecimal): Boolean;
begin
  if A.Sign = 0 then
    Result := HugeDecimalIsZero(B)
  else
  if A.Sign > 0 then
    Result := HugeDecimalEqualsHugeDecimal(A.Value, B)
  else
    Result := False;
end;

function SHugeDecimalEqualsSHugeDecimal(const A, B: SHugeDecimal): Boolean;
begin
  if A.Sign <> B.Sign then
    Result := False
  else
    Result := HugeDecimalEqualsHugeDecimal(A.Value, B.Value);
end;

function SHugeDecimalCompareWord8(const A: SHugeDecimal; const B: Byte): Integer;
begin
  if A.Sign = 0 then
    if B = 0 then
      Result := 0
    else
      Result := -1
  else
  if A.Sign > 0 then
    Result := HugeDecimalCompareWord8(A.Value, B)
  else
    Result := -1;
end;

function SHugeDecimalCompareHugeDecimal(const A: SHugeDecimal; const B: HugeDecimal): Integer;
begin
  if A.Sign = 0 then
    if HugeDecimalIsZero(B) then
      Result := 0
    else
      Result := -1
  else
  if A.Sign > 0 then
    Result := HugeDecimalCompareHugeDecimal(A.Value, B)
  else
    Result := -1;
end;

function SHugeDecimalCompareSHugeDecimal(const A, B: SHugeDecimal): Integer;
begin
  if A.Sign = 0 then
    if B.Sign = 0 then
      Result := 0
    else
    if B.Sign < 0 then
      Result := 1
    else
      Result := -1
  else
  if A.Sign > 0 then
    if B.Sign <= 0 then
      Result := 1
    else
      Result := HugeDecimalCompareHugeDecimal(A.Value, B.Value)
  else
    if B.Sign >= 0 then
      Result := -1
    else
      Result := HugeDecimalCompareHugeDecimal(B.Value, A.Value);
end;

procedure SHugeDecimalAddHugeDecimal(var A: SHugeDecimal; const B: HugeDecimal);
var
  C : Integer;
  R : HugeDecimal;
begin
  // Handle B = 0 or A = 0
  if HugeDecimalIsZero(B) then
    exit;
  if A.Sign = 0 then
    begin
      HugeDecimalAssignHugeDecimal(A.Value, B);
      A.Sign := 1;
      exit;
    end;
  // A <> 0 and B <> 0
  // Handle A > 0
  if A.Sign > 0 then
    begin
      HugeDecimalAddHugeDecimal(A.Value, B);
      exit;
    end;
  // A < 0
  // Compare absolute values
  C := HugeDecimalCompareHugeDecimal(A.Value, B);
  // Handle Abs(A) = B
  if C = 0 then
    begin
      SHugeDecimalAssignZero(A);
      exit;
    end;
  // Abs(A) <> B
  // Calculate result value = Abs(A - B)
  if C < 0 then
    begin
      HugeDecimalInitHugeDecimal(R, B);
      HugeDecimalSubtractHugeDecimal(R, A.Value);
      HugeDecimalAssignHugeDecimal(A.Value, R);
    end
  else
    HugeDecimalSubtractHugeDecimal(A.Value, B);
  // Set result sign
  if ((A.Sign > 0) and (C > 0)) or
     ((A.Sign < 0) and (C < 0)) then
    A.Sign := 1
  else
    A.Sign := -1;
end;

procedure SHugeDecimalAddSHugeDecimal(var A: SHugeDecimal; const B: SHugeDecimal);
var
  C : Integer;
  R : HugeDecimal;
begin
  // Handle B = 0 or A = 0
  if B.Sign = 0 then
    exit;
  if A.Sign = 0 then
    begin
      SHugeDecimalAssignSHugeDecimal(A, B);
      exit;
    end;
  // A <> 0 and B <> 0
  // Handle Sign(A) = Sign(B)
  if A.Sign = B.Sign then
    begin
      HugeDecimalAddHugeDecimal(A.Value, B.Value);
      exit;
    end;
  // Sign(A) <> Sign(B)
  // Compare absolute values
  C := HugeDecimalCompareHugeDecimal(A.Value, B.Value);
  // Handle Abs(A) = Abs(B)
  if C = 0 then
    begin
      SHugeDecimalAssignZero(A);
      exit;
    end;
  // Abs(A) <> Abs(B)
  // Calculate result value = Abs(A - B)
  if C < 0 then
    begin
      HugeDecimalInitHugeDecimal(R, B.Value);
      HugeDecimalSubtractHugeDecimal(R, A.Value);
      HugeDecimalAssignHugeDecimal(A.Value, R);
    end
  else
    HugeDecimalSubtractHugeDecimal(A.Value, B.Value);
  // Set result sign
  if ((A.Sign > 0) and (C > 0)) or
     ((A.Sign < 0) and (C < 0)) then
    A.Sign := 1
  else
    A.Sign := -1;
end;

procedure SHugeDecimalSubtractSHugeDecimal(var A: SHugeDecimal; const B: SHugeDecimal);
var
  C : Integer;
  R : HugeDecimal;
begin
  // Handle B = 0 or A = 0
  if B.Sign = 0 then
    exit;
  if A.Sign = 0 then
    begin
      HugeDecimalAssignHugeDecimal(A.Value, B.Value);
      A.Sign := -B.Sign;
      exit;
    end;
  // A <> 0 and B <> 0
  // Handle Sign(A) <> Sign(B)
  if A.Sign <> B.Sign then
    begin
      HugeDecimalAddHugeDecimal(A.Value, B.Value);
      exit;
    end;
  // Sign(A) = Sign(B)
  // Compare absolute values
  C := HugeDecimalCompareHugeDecimal(A.Value, B.Value);
  // Handle Abs(A) = Abs(B)
  if C = 0 then
    begin
      SHugeDecimalAssignZero(A);
      exit;
    end;
  // Abs(A) <> Abs(B)
  // Calculate result value = Abs(A - B)
  if C < 0 then
    begin
      HugeDecimalInitHugeDecimal(R, B.Value);
      HugeDecimalSubtractHugeDecimal(R, A.Value);
      HugeDecimalAssignHugeDecimal(A.Value, R);
    end
  else
    HugeDecimalSubtractHugeDecimal(A.Value, B.Value);
  // Set result sign
  if ((A.Sign > 0) and (C > 0)) or
     ((A.Sign < 0) and (C < 0)) then
    A.Sign := 1
  else
    A.Sign := -1;
end;

function TryStrToSHugeDecimal(const S: String; var R: SHugeDecimal): TDecimalConvertErrorType;
var
  LenS : Integer;
  SignCh : Char;
  T : String;
  ValA : HugeDecimal;
  ConvErr : TDecimalConvertErrorType;
begin
  LenS := Length(S);
  if LenS = 0 then
    begin
      Result := dceConvertError;
      exit;
    end;
  SignCh := S[1];
  if (SignCh = '+') or (SignCh = '-') then
    T := Copy(S, 2, LenS - 1)
  else
    T := S;
  ConvErr := TryStrToHugeDecimal(T, ValA);
  if ConvErr <> dceNoError then
    begin
      Result := ConvErr;
      exit;
    end;
  if HugeDecimalIsZero(ValA) then
    begin
      SHugeDecimalAssignZero(R);
      Result := dceNoError;
      exit;
    end;
  HugeDecimalAssignHugeDecimal(R.Value, ValA);
  if SignCh = '-' then
    R.Sign := -1
  else
    R.Sign := 1;
  Result := dceNoError;
end;

procedure StrToSHugeDecimal(const S: String; var R: SHugeDecimal);
begin
  case TryStrToSHugeDecimal(S, R) of
    dceNoError       : ;
    dceConvertError  : raise EDecimalError.Create(SConvertError);
    dceOverflowError : raise EDecimalError.Create(SOverflowError);
  end;
end;

function SHugeDecimalToStr(const A: SHugeDecimal): String;
var
  R : String;
begin
  if A.Sign = 0 then
    Result := '0'
  else
    begin
      R := HugeDecimalToStr(A.Value);
      if A.Sign < 0 then
        R := '-' + R;
      Result := R;
    end;
end;



{                                                                              }
{ Test cases                                                                   }
{                                                                              }
{$IFDEF DECIMAL_TEST}
{$ASSERTIONS ON}
procedure Test_Decimal32;
var A, B : Decimal32;
begin
  // Zero
  Decimal32InitZero(A);
  Assert(Decimal32IsZero(A));
  Assert(not Decimal32IsOne(A));
  Assert(not Decimal32IsMaximum(A));
  Assert(not Decimal32IsOverflow(A));
  Assert(Decimal32ToWord8(A) = 0);
  Assert(Decimal32ToStr(A) = '0.0000');
  Assert(Decimal32EqualsWord8(A, 0));
  Assert(Decimal32CompareWord8(A, 0) = 0);
  Assert(Decimal32CompareWord8(A, 1) = -1);
  Assert(Decimal32Trunc(A) = 0);
  Assert(Decimal32Round(A) = 0);
  Assert(Decimal32FracWord(A) = 0);
  Assert(Decimal32EqualsFloat(A, 0.0));
  Assert(Decimal32EqualsFloat(A, 0.000049));
  Assert(Decimal32EqualsFloat(A, 0.00005));
  Assert(not Decimal32EqualsFloat(A, 0.000051));
  Assert(Decimal32CompareFloat(A, 0.0000) = 0);
  Assert(Decimal32CompareFloat(A, -0.0001) = 1);
  Assert(Decimal32CompareFloat(A, 0.0001) = -1);

  // One
  Decimal32InitOne(A);
  Assert(not Decimal32IsZero(A));
  Assert(Decimal32IsOne(A));
  Assert(Decimal32ToWord8(A) = 1);
  Assert(Decimal32ToStr(A) = '1.0000');
  Assert(Decimal32ToStrB(A) = '1.0000');
  Assert(Decimal32EqualsWord8(A, 1));
  Assert(Decimal32CompareWord8(A, 0) = 1);
  Assert(Decimal32CompareWord8(A, 1) = 0);
  Assert(Decimal32CompareWord8(A, 2) = -1);
  Assert(Decimal32Trunc(A) = 1);
  Assert(Decimal32Round(A) = 1);
  Assert(Decimal32EqualsFloat(A, 1.0));
  Assert(Decimal32EqualsFloat(A, 1.000049));
  Assert(not Decimal32EqualsFloat(A, 1.000051));

  // 2
  Decimal32InitWord8(A, 2);
  Assert(Decimal32ToWord8(A) = 2);
  Assert(Decimal32EqualsWord16(A, 2));
  Assert(Decimal32EqualsWord32(A, 2));
  Assert(Decimal32EqualsInt32(A, 2));
  Assert(not Decimal32EqualsInt32(A, -1));
  Assert(not Decimal32EqualsWord32(A, 999999999));
  Assert(Decimal32CompareWord16(A, 2) = 0);
  Assert(Decimal32CompareWord32(A, 999999999) = -1);
  Assert(Decimal32CompareInt32(A, 2) = 0);
  Assert(Decimal32CompareInt32(A, -1) = 1);
  Assert(Decimal32CompareInt32(A, 3) = -1);

  // MaxWord16
  Decimal32InitWord16(A, $FFFF);
  Assert(Decimal32ToWord16(A) = $FFFF);
  Assert(Decimal32ToStr(A) = '65535.0000');

  Assert(FloatIsDecimal32Range(0.0000));
  Assert(not FloatIsDecimal32Range(-0.0001));
  Assert(FloatIsDecimal32Range(-0.00005));
  Assert(not FloatIsDecimal32Range(-0.000051));
  Assert(FloatIsDecimal32Range(99999.9999));
  Assert(FloatIsDecimal32Range(99999.999949));
  Assert(not FloatIsDecimal32Range(99999.99995));
  Assert(not FloatIsDecimal32Range(100000.0000));

  // 99999
  Decimal32InitWord32(A, 99999);
  Assert(Decimal32ToWord32(A) = 99999);
  Assert(Decimal32ToStr(A) = '99999.0000');
  Assert(Decimal32Trunc(A) = 99999);

  // 1.2345
  Decimal32InitFloat(A, 1.2345);
  Assert(Abs(Decimal32ToFloat(A) - 1.2345) < 1e-9);
  Assert(Decimal32ToStr(A) = '1.2345');
  Assert(Decimal32Trunc(A) = 1);
  Assert(Decimal32Round(A) = 1);
  Assert(Decimal32EqualsFloat(A, 1.2345));

  // 1.23445
  Decimal32InitFloat(A, 1.23445);
  Assert(Abs(Decimal32ToFloat(A) - 1.2344) < 1e-9);
  Assert(Decimal32ToStr(A) = '1.2344');

  // 1.23455
  Decimal32InitFloat(A, 1.23455);
  Assert(Abs(Decimal32ToFloat(A) - 1.2346) < 1e-9);
  Assert(Decimal32ToStr(A) = '1.2346');

  // MaxDecimal32
  Decimal32InitFloat(A, 99999.9999);
  Assert(Decimal32IsMaximum(A));

  // MaxDecimal32
  Decimal32InitMax(A);
  Assert(Decimal32IsMaximum(A));
  Assert(not Decimal32IsOverflow(A));
  Assert(Decimal32ToStr(A) = '99999.9999');
  Assert(Decimal32Trunc(A) = 99999);
  Assert(Decimal32Round(A) = 100000);
  Assert(Decimal32FracWord(A) = 9999);
  Assert(Decimal32EqualsFloat(A, 99999.9999));
  Assert(Decimal32EqualsFloat(A, 99999.999949));
  Assert(not Decimal32EqualsFloat(A, 99999.99995));

  // Trunc/Round
  Decimal32InitFloat(A, 99999.4999);
  Assert(Decimal32Trunc(A) = 99999);
  Assert(Decimal32Round(A) = 99999);

  Decimal32InitFloat(A, 99999.5000);
  Assert(Decimal32Trunc(A) = 99999);
  Assert(Decimal32Round(A) = 100000);
  Assert(Decimal32FracWord(A) = 5000);

  Decimal32InitFloat(A, 99998.5000);
  Assert(Decimal32Trunc(A) = 99998);
  Assert(Decimal32Round(A) = 99998);

  Decimal32InitFloat(A, 99998.5001);
  Assert(Decimal32Trunc(A) = 99998);
  Assert(Decimal32Round(A) = 99999);

  // 0.1
  Decimal32InitFloat(A, 0.1);
  Assert(Decimal32ToStr(A) = '0.1000');
  Assert(Decimal32EqualsFloat(A, 0.1));

  // 0.0001
  Decimal32InitFloat(A, 0.0001);
  Assert(Decimal32ToStr(A) = '0.0001');
  Assert(Decimal32EqualsFloat(A, 0.0001));

  // InitFloat Rounding
  Decimal32InitFloat(A, 0.00005);
  Assert(Decimal32ToStr(A) = '0.0000');
  Decimal32InitFloat(A, 0.000051);
  Assert(Decimal32ToStr(A) = '0.0001');
  Decimal32InitFloat(A, 0.00025);
  Assert(Decimal32ToStr(A) = '0.0002');
  Decimal32InitFloat(A, 0.00055);
  Assert(Decimal32ToStr(A) = '0.0006');
  Decimal32InitFloat(A, -0.000049);
  Assert(Decimal32ToStr(A) = '0.0000');
  Decimal32InitFloat(A, -0.000050);
  Assert(Decimal32ToStr(A) = '0.0000');

  // Add/Subtract
  Decimal32InitFloat(A, 1.9375);
  Assert(Decimal32ToFloat(A) = 1.9375);
  Decimal32AddWord8(A, 1);
  Assert(Decimal32ToFloat(A) = 2.9375);
  Decimal32AddWord16(A, 10000);
  Assert(Decimal32ToFloat(A) = 10002.9375);
  Decimal32AddWord32(A, 10000);
  Assert(Decimal32ToFloat(A) = 20002.9375);
  Assert(Decimal32ToStr(A) = '20002.9375');
  Decimal32SubtractWord32(A, 10000);
  Assert(Decimal32ToFloat(A) = 10002.9375);
  Decimal32SubtractWord16(A, 10000);
  Assert(Decimal32ToFloat(A) = 2.9375);
  Decimal32SubtractWord8(A, 1);
  Assert(Decimal32ToFloat(A) = 1.9375);

  // Add/Subtract
  Decimal32InitFloat(A, 1.9375);
  Decimal32InitFloat(B, 2.5000);
  Decimal32AddDecimal32(A, B);
  Assert(Decimal32ToFloat(A) = 4.4375);
  Decimal32SubtractDecimal32(A, B);
  Assert(Decimal32ToFloat(A) = 1.9375);

  // Equals/Compare
  Decimal32InitFloat(A, 1.9375);
  Decimal32InitFloat(B, 2.5000);
  Assert(not Decimal32EqualsDecimal32(A, B));
  Assert(Decimal32EqualsDecimal32(A, A));
  Assert(Decimal32CompareDecimal32(A, B) = -1);
  Assert(Decimal32CompareDecimal32(A, A) = 0);
  Assert(Decimal32CompareDecimal32(B, A) = 1);

  // Multiply/Divide
  Decimal32InitFloat(A, 1.9375);
  Decimal32MultiplyWord8(A, 3);
  Assert(Decimal32ToFloat(A) = 5.8125);
  Decimal32MultiplyWord16(A, 101);
  Assert(Decimal32ToFloat(A) = 587.0625);
  Decimal32MultiplyWord32(A, 13);
  Assert(Decimal32ToFloat(A) = 7631.8125);
  Assert(Decimal32ToStr(A) = '7631.8125');
  Decimal32DivideWord32(A, 13);
  Assert(Decimal32ToFloat(A) = 587.0625);
  Decimal32DivideWord16(A, 101);
  Assert(Decimal32ToFloat(A) = 5.8125);
  Decimal32DivideWord8(A, 3);
  Assert(Decimal32ToStr(A) = '1.9375');

  // Multiply/Divide
  Decimal32InitFloat(A, 1.9375);
  Decimal32InitFloat(B, 2.5000);
  Decimal32MultiplyDecimal32(A, B);
  Assert(Abs(Decimal32ToFloat(A) - 4.8438) < 1e-9);
  Assert(Decimal32ToStr(A) = '4.8438');
  Decimal32DivideDecimal32(A, B);
  Assert(Decimal32ToStr(A) = '1.9375');

  // Multiply
  Decimal32InitFloat(A, 1.9375);
  Decimal32InitFloat(B, 3.1200);
  Decimal32MultiplyDecimal32(A, B);
  Assert(Decimal32ToFloat(A) - 6.045 < 1e-10);
  Assert(Decimal32ToStr(A) = '6.0450');

  // Multiply
  Decimal32InitFloat(A, 16.9375);
  Decimal32InitFloat(B, 3.1200);
  Decimal32MultiplyDecimal32(A, B);
  Assert(Decimal32ToFloat(A) - 52.8450 < 1e-10);
  Assert(Decimal32ToStr(A) = '52.8450');

  // Str
  A := StrToDecimal32('0');
  Assert(Decimal32ToWord8(A) = 0);
  A := StrToDecimal32('0000000.0000000');
  Assert(Decimal32ToWord8(A) = 0);
  A := StrToDecimal32('1');
  Assert(Decimal32ToWord8(A) = 1);
  A := StrToDecimal32('1.0');
  Assert(Decimal32ToWord8(A) = 1);
  A := StrToDecimal32('00000000000001.000000000000000');
  Assert(Decimal32ToWord8(A) = 1);
  A := StrToDecimal32('1.0000000000000001');
  Assert(Decimal32ToWord8(A) = 1);
  A := StrToDecimal32('123.9385');
  Assert(Decimal32ToStr(A) = '123.9385');
  A := StrToDecimal32('123.93856');
  Assert(Decimal32ToStr(A) = '123.9386');
  A := StrToDecimal32('123.93855');
  Assert(Decimal32ToStr(A) = '123.9386');
  A := StrToDecimal32('123.93865');
  Assert(Decimal32ToStr(A) = '123.9386');
  A := StrToDecimal32('123.938650000000010');
  Assert(Decimal32ToStr(A) = '123.9387');
  A := StrToDecimal32('12345.99995');
  Assert(Decimal32ToStr(A) = '12346.0000');
  A := StrToDecimal32('99999.9999');
  Assert(Decimal32ToStr(A) = '99999.9999');

  // Str
  Assert(TryStrToDecimal32('0', A) = dceNoError);
  Assert(Decimal32ToWord32(A) = 0);
  Assert(TryStrToDecimal32('99999', A) = dceNoError);
  Assert(Decimal32ToWord32(A) = 99999);
  Assert(TryStrToDecimal32('12345.99995', A) = dceNoError);
  Assert(Decimal32ToStr(A) = '12346.0000');
  Assert(TryStrToDecimal32('99999.9999', A) = dceNoError);
  Assert(Decimal32ToStr(A) = '99999.9999');

  // Str exponent
  Assert(TryStrToDecimal32('1E0', A) = dceNoError);
  Assert(Decimal32ToStr(A) = '1.0000');
  Assert(TryStrToDecimal32('1E+1', A) = dceNoError);
  Assert(Decimal32ToStr(A) = '10.0000');
  Assert(TryStrToDecimal32('1E-1', A) = dceNoError);
  Assert(Decimal32ToStr(A) = '0.1000');
  Assert(TryStrToDecimal32('1.23E0', A) = dceNoError);
  Assert(Decimal32ToStr(A) = '1.2300');
  Assert(TryStrToDecimal32('1.23E1', A) = dceNoError);
  Assert(Decimal32ToStr(A) = '12.3000');
  Assert(TryStrToDecimal32('1.23E+2', A) = dceNoError);
  Assert(Decimal32ToStr(A) = '123.0000');
  Assert(TryStrToDecimal32('1.23E+3', A) = dceNoError);
  Assert(Decimal32ToStr(A) = '1230.0000');
  Assert(TryStrToDecimal32('1.23E+4', A) = dceNoError);
  Assert(Decimal32ToStr(A) = '12300.0000');
  Assert(TryStrToDecimal32('1.23E-1', A) = dceNoError);
  Assert(Decimal32ToStr(A) = '0.1230');
  Assert(TryStrToDecimal32('1.23E-2', A) = dceNoError);
  Assert(Decimal32ToStr(A) = '0.0123');
  Assert(TryStrToDecimal32('1.23E-3', A) = dceNoError);
  Assert(Decimal32ToStr(A) = '0.0012');
  Assert(TryStrToDecimal32('1.23E-4', A) = dceNoError);
  Assert(Decimal32ToStr(A) = '0.0001');
  Assert(TryStrToDecimal32('1.23E-5', A) = dceNoError);
  Assert(Decimal32ToStr(A) = '0.0000');
  Assert(TryStrToDecimal32('1.23E-100', A) = dceNoError);
  Assert(Decimal32ToStr(A) = '0.0000');
  Assert(TryStrToDecimal32('0.1234E+0', A) = dceNoError);
  Assert(Decimal32ToStr(A) = '0.1234');
  Assert(TryStrToDecimal32('0.1234E+1', A) = dceNoError);
  Assert(Decimal32ToStr(A) = '1.2340');
  Assert(TryStrToDecimal32('0.1234E+2', A) = dceNoError);
  Assert(Decimal32ToStr(A) = '12.3400');
  Assert(TryStrToDecimal32('0.1234E-1', A) = dceNoError);
  Assert(Decimal32ToStr(A) = '0.0123');
  Assert(TryStrToDecimal32('1234321E-4', A) = dceNoError);
  Assert(Decimal32ToStr(A) = '123.4321');
  // Assert(TryStrToDecimal32('0.1234321E+3', A) = dceNoError); // TODO FIX
  // Assert(Decimal32ToStr(A) = '123.4321');

  // Str overflow error
  Assert(TryStrToDecimal32('100000', A) = dceOverflowError);
  Assert(TryStrToDecimal32('99999.99995', A) = dceOverflowError);

  // Str convert error
  Assert(TryStrToDecimal32('', A) = dceConvertError);
  Assert(TryStrToDecimal32('-', A) = dceConvertError);
  Assert(TryStrToDecimal32('+', A) = dceConvertError);
  Assert(TryStrToDecimal32(' ', A) = dceConvertError);
  Assert(TryStrToDecimal32('.', A) = dceConvertError);
  Assert(TryStrToDecimal32('0.', A) = dceConvertError);
  Assert(TryStrToDecimal32('.0', A) = dceConvertError);
  Assert(TryStrToDecimal32(' 0', A) = dceConvertError);
  Assert(TryStrToDecimal32('0 ', A) = dceConvertError);
  Assert(TryStrToDecimal32('E', A) = dceConvertError);
  Assert(TryStrToDecimal32('E-', A) = dceConvertError);
  Assert(TryStrToDecimal32('1E', A) = dceConvertError);
  Assert(TryStrToDecimal32('1E+', A) = dceConvertError);
  Assert(TryStrToDecimal32('E+0', A) = dceConvertError);
  Assert(TryStrToDecimal32('1.E+0', A) = dceConvertError);
end;

procedure Test_Decimal64;
var A, B : Decimal64;
    C : Decimal32;
begin
  Decimal64InitZero(A);
  Assert(Decimal64IsZero(A));
  Assert(not Decimal64IsOne(A));
  Assert(not Decimal64IsMaximum(A));
  Assert(not Decimal64IsOverflow(A));
  Assert(Decimal64ToWord8(A) = 0);
  Assert(Decimal64ToStr(A) = '0.000000000');
  Assert(Decimal64EqualsWord8(A, 0));
  Assert(Decimal64CompareWord8(A, 0) = 0);
  Assert(Decimal64CompareWord8(A, 1) = -1);
  Assert(Decimal64Trunc(A) = 0);
  Assert(Decimal64Round(A) = 0);
  Assert(Decimal64FracWord(A) = 0);
  Assert(Decimal64EqualsFloat(A, 0.0));
  Assert(Decimal64EqualsFloat(A, 0.00000000049));
  Assert(Decimal64EqualsFloat(A, 0.0000000005));
  Assert(not Decimal64EqualsFloat(A, 0.00000000051));
  Assert(Decimal64CompareFloat(A, 0.000000000) = 0);
  Assert(Decimal64CompareFloat(A, -0.000000001) = 1);
  Assert(Decimal64CompareFloat(A, 0.000000001) = -1);

  Decimal64InitOne(A);
  Assert(not Decimal64IsZero(A));
  Assert(Decimal64IsOne(A));
  Assert(Decimal64ToWord8(A) = 1);
  Assert(Decimal64ToStr(A) = '1.000000000');
  Assert(Decimal64ToStrB(A) = '1.000000000');
  Assert(Decimal64EqualsWord8(A, 1));
  Assert(Decimal64CompareWord8(A, 0) = 1);
  Assert(Decimal64CompareWord8(A, 1) = 0);
  Assert(Decimal64CompareWord8(A, 2) = -1);
  Assert(Decimal64Trunc(A) = 1);
  Assert(Decimal64Round(A) = 1);
  Assert(Decimal64EqualsFloat(A, 1.0));
  Assert(Decimal64EqualsFloat(A, 1.00000000049));
  Assert(not Decimal64EqualsFloat(A, 1.00000000051));

  Decimal64InitWord8(A, 2);
  Assert(Decimal64ToWord8(A) = 2);
  Assert(Decimal64EqualsWord16(A, 2));
  Assert(Decimal64EqualsWord32(A, 2));
  Assert(not Decimal64EqualsWord32(A, 999999999));
  Assert(Decimal64CompareWord16(A, 2) = 0);
  Assert(Decimal64CompareWord32(A, 999999999) = -1);
  Assert(Decimal64EqualsInt32(A, 2));
  Assert(not Decimal64EqualsInt32(A, -1));
  Assert(not Decimal64EqualsInt64(A, MaxInt64));
  Assert(Decimal64CompareInt32(A, 2) = 0);
  Assert(Decimal64CompareInt32(A, -1) = 1);
  Assert(Decimal64CompareInt32(A, MaxInt32) = -1);
  Assert(Decimal64CompareInt64(A, -1) = 1);
  Assert(Decimal64CompareInt64(A, 2) = 0);
  Assert(Decimal64CompareInt64(A, MaxInt64) = -1);

  Decimal64InitWord16(A, $FFFF);
  Assert(Decimal64ToWord16(A) = $FFFF);
  Assert(Decimal64ToStr(A) = '65535.000000000');

  Decimal64InitWord32(A, $FFFFFFFF);
  Assert(Decimal64ToWord32(A) = $FFFFFFFF);
  Assert(Decimal64ToStr(A) = '4294967295.000000000');

  Assert(FloatIsDecimal64Range(0.000000000));
  Assert(not FloatIsDecimal64Range(-0.000000001));
  Assert(FloatIsDecimal64Range(-0.0000000005));
  Assert(not FloatIsDecimal64Range(-0.00000000051));
  {$IFNDEF DELPHI7_DOWN}
  Assert(not FloatIsDecimal64Range(9999999999.999999999));
  Assert(not FloatIsDecimal64Range(10000000000.000000000));
  {$ENDIF}

  Decimal64InitInt64(A, Decimal64MaxInt);
  Assert(Decimal64ToInt64(A) = 9999999999);
  Assert(Decimal64ToStr(A) = '9999999999.000000000');
  Assert(Decimal64Trunc(A) = 9999999999);

  Decimal64InitFloat(A, 1.234567890);
  Assert(Abs(Decimal64ToFloat(A) - 1.234567890) < 1e-12);
  Assert(Decimal64ToStr(A) = '1.234567890');
  Assert(Decimal64Trunc(A) = 1);
  Assert(Decimal64Round(A) = 1);
  Assert(Decimal64EqualsFloat(A, 1.234567890));

  Decimal64InitFloat(A, 1.2345111115);
  Assert(Abs(Decimal64ToFloat(A) - 1.234511112) < 1e-12);
  Assert(Decimal64ToStr(A) = '1.234511112');

  Decimal64InitFloat(A, 1.2345111125);
  Assert(Abs(Decimal64ToFloat(A) - 1.234511112) < 1e-12);
  Assert(Decimal64ToStr(A) = '1.234511112');

  Decimal64InitFloat(A, 1.23451111251);
  Assert(Abs(Decimal64ToFloat(A) - 1.234511113) < 1e-12);
  Assert(Decimal64ToStr(A) = '1.234511113');

  A := StrToDecimal64('9999999999.999999999');
  Assert(Decimal64IsMaximum(A));

  Decimal64InitMax(A);
  Assert(Decimal64IsMaximum(A));
  Assert(not Decimal64IsOverflow(A));
  Assert(Decimal64ToStr(A) = '9999999999.999999999');
  Assert(Decimal64Trunc(A) = 9999999999);
  Assert(Decimal64Round(A) = 10000000000);
  Assert(Decimal64FracWord(A) = 999999999);

  A := StrToDecimal64('9999999999.499999999');
  Assert(Decimal64Trunc(A) = 9999999999);
  Assert(Decimal64Round(A) = 9999999999);

  A := StrToDecimal64('9999999999.500000000');
  Assert(Decimal64Trunc(A) = 9999999999);
  Assert(Decimal64Round(A) = 10000000000);
  Assert(Decimal64FracWord(A) = 500000000);

  A := StrToDecimal64('9999999998.500000000');
  Assert(Decimal64Trunc(A) = 9999999998);
  Assert(Decimal64Round(A) = 9999999998);

  A := StrToDecimal64('9999999998.500000001');
  Assert(Decimal64Trunc(A) = 9999999998);
  Assert(Decimal64Round(A) = 9999999999);

  Decimal64InitFloat(A, 0.1);
  Assert(Decimal64ToStr(A) = '0.100000000');
  Assert(Decimal64EqualsFloat(A, 0.1));

  Decimal64InitFloat(A, 0.000000001);
  Assert(Decimal64ToStr(A) = '0.000000001');
  Assert(Decimal64EqualsFloat(A, 0.000000001));

  Decimal64InitFloat(A, 0.0000000005);
  Assert(Decimal64ToStr(A) = '0.000000000');
  Decimal64InitFloat(A, 0.00000000051);
  Assert(Decimal64ToStr(A) = '0.000000001');
  Decimal64InitFloat(A, 0.0000000025);
  Assert(Decimal64ToStr(A) = '0.000000002');
  Decimal64InitFloat(A, 0.0000000055);
  Assert(Decimal64ToStr(A) = '0.000000006');
  Decimal64InitFloat(A, -0.00000000049);
  Assert(Decimal64ToStr(A) = '0.000000000');
  Decimal64InitFloat(A, -0.00000000050);
  Assert(Decimal64ToStr(A) = '0.000000000');

  Decimal64InitFloat(A, 1.9375);
  Assert(Decimal64ToFloat(A) = 1.9375);
  Decimal64AddWord8(A, 1);
  Assert(Decimal64ToFloat(A) = 2.9375);
  Decimal64AddWord16(A, 10000);
  Assert(Decimal64ToFloat(A) = 10002.9375);
  Decimal64AddWord32(A, 10000);
  Assert(Decimal64ToFloat(A) = 20002.9375);
  Assert(Decimal64ToStr(A) = '20002.937500000');
  Decimal64SubtractWord32(A, 10000);
  Assert(Decimal64ToFloat(A) = 10002.9375);
  Decimal64SubtractWord16(A, 10000);
  Assert(Decimal64ToFloat(A) = 2.9375);
  Decimal64SubtractWord8(A, 1);
  Assert(Decimal64ToFloat(A) = 1.9375);

  Decimal64InitFloat(A, 1.9375);
  Decimal64InitFloat(B, 2.5000);
  Decimal64AddDecimal64(A, B);
  Assert(Decimal64ToFloat(A) = 4.4375);
  Decimal64SubtractDecimal64(A, B);
  Assert(Decimal64ToFloat(A) = 1.9375);

  Decimal64InitFloat(A, 1.9375);
  Decimal64InitFloat(B, 2.5000);
  Assert(not Decimal64EqualsDecimal64(A, B));
  Assert(Decimal64EqualsDecimal64(A, A));
  Assert(Decimal64CompareDecimal64(A, B) = -1);
  Assert(Decimal64CompareDecimal64(A, A) = 0);
  Assert(Decimal64CompareDecimal64(B, A) = 1);

  Decimal64InitFloat(A, 1.9375);
  Decimal64MultiplyWord8(A, 3);
  Assert(Decimal64ToFloat(A) = 5.8125);
  Decimal64MultiplyWord16(A, 101);
  Assert(Decimal64ToFloat(A) = 587.0625);
  Decimal64MultiplyWord32(A, 13);
  Assert(Decimal64ToFloat(A) = 7631.8125);
  Assert(Decimal64ToStr(A) = '7631.812500000');
  Decimal64DivideWord32(A, 13);
  Assert(Decimal64ToFloat(A) = 587.0625);
  Decimal64DivideWord16(A, 101);
  Assert(Decimal64ToFloat(A) = 5.8125);
  Decimal64DivideWord8(A, 3);
  Assert(Decimal64ToStr(A) = '1.937500000');

  Decimal64InitFloat(A, 1.9375);
  Decimal64InitFloat(B, 2.5000);
  Decimal64MultiplyDecimal64(A, B);
  Assert(Decimal64ToFloat(A) = 4.84375);
  Assert(Decimal64ToStr(A) = '4.843750000');
  Decimal64DivideDecimal64(A, B);
  Assert(Decimal64ToStr(A) = '1.937500000');

  Decimal64InitFloat(A, 12.937512);
  Decimal64InitFloat(B, 3.123433);
  Decimal64MultiplyDecimal64(A, B);
  Assert(Decimal64ToFloat(A) - 40.409451919 < 1e-10);
  Assert(Decimal64ToStr(A) = '40.409451919');

  Decimal32InitFloat(C, 1.9375);
  Decimal64InitDecimal32(A, C);
  Assert(Decimal64ToStr(A) = '1.937500000');
  Assert(Decimal32ToStr(Decimal64ToDecimal32(A)) = '1.9375');

  A := StrToDecimal64('0');
  Assert(Decimal64ToWord8(A) = 0);
  A := StrToDecimal64('000000000000.000000000000');
  Assert(Decimal64ToWord8(A) = 0);
  A := StrToDecimal64('1');
  Assert(Decimal64ToWord8(A) = 1);
  A := StrToDecimal64('1.0');
  Assert(Decimal64ToWord8(A) = 1);
  A := StrToDecimal64('0000000000000000001.00000000000000000000');
  Assert(Decimal64ToWord8(A) = 1);
  A := StrToDecimal64('1.0000000000000001');
  Assert(Decimal64ToWord8(A) = 1);
  A := StrToDecimal64('123.938500000');
  Assert(Decimal64ToStr(A) = '123.938500000');
  A := StrToDecimal64('123.9385000005');
  Assert(Decimal64ToStr(A) = '123.938500000');
  A := StrToDecimal64('123.9385000015');
  Assert(Decimal64ToStr(A) = '123.938500002');
  A := StrToDecimal64('123.9386000025');
  Assert(Decimal64ToStr(A) = '123.938600002');
  A := StrToDecimal64('123.93860000251');
  Assert(Decimal64ToStr(A) = '123.938600003');
  A := StrToDecimal64('9999999999.999999999');
  Assert(Decimal64ToStr(A) = '9999999999.999999999');

  Assert(TryStrToDecimal64('0', A) = dceNoError);
  Assert(Decimal64ToWord32(A) = 0);
  Assert(TryStrToDecimal64('9999999999', A) = dceNoError);
  Assert(Decimal64ToInt64(A) = 9999999999);

  Assert(TryStrToDecimal64('10000000000', A) = dceOverflowError);
  Assert(TryStrToDecimal64('9999999999.9999999995', A) = dceOverflowError);

  Assert(TryStrToDecimal64('', A) = dceConvertError);
  Assert(TryStrToDecimal64('-', A) = dceConvertError);
  Assert(TryStrToDecimal64('+', A) = dceConvertError);
  Assert(TryStrToDecimal64(' ', A) = dceConvertError);
  Assert(TryStrToDecimal64('.', A) = dceConvertError);
  Assert(TryStrToDecimal64('0.', A) = dceConvertError);
  Assert(TryStrToDecimal64('.0', A) = dceConvertError);
  Assert(TryStrToDecimal64(' 0', A) = dceConvertError);
  Assert(TryStrToDecimal64('0 ', A) = dceConvertError);
end;

procedure Test_Decimal128;
var A, B : Decimal128;
    C : Decimal64;
begin
  Decimal128InitZero(A);
  Assert(Decimal128IsZero(A));
  Assert(not Decimal128IsOne(A));
  Assert(not Decimal128IsMaximum(A));
  Assert(not Decimal128IsOverflow(A));
  Assert(Decimal128ToWord8(A) = 0);
  Assert(Decimal128ToStr(A) = '0.0000000000000000000');
  Assert(Decimal128EqualsWord8(A, 0));
  Assert(Decimal128CompareWord8(A, 0) = 0);
  Assert(Decimal128CompareWord8(A, 1) = -1);
  Assert(Word64ToStr(Decimal128Trunc(A)) = '0');
  Assert(Word64ToStr(Decimal128Round(A)) = '0');
  Assert(Word64ToStr(Decimal128FracWord(A)) = '0');
  Assert(Decimal128EqualsFloat(A, 0.0));
  Assert(Decimal128EqualsFloat(A, 0.000000000000000000049));
  Assert(Decimal128EqualsFloat(A, 0.00000000000000000005));
  Assert(not Decimal128EqualsFloat(A, 0.000000000000000000051));
  Assert(Decimal128CompareFloat(A, 0.0000000000000000000) = 0);
  Assert(Decimal128CompareFloat(A, -0.0000000000000000001) = 1);
  Assert(Decimal128CompareFloat(A, 0.0000000000000000001) = -1);

  Decimal128InitOne(A);
  Assert(not Decimal128IsZero(A));
  Assert(Decimal128IsOne(A));
  Assert(Decimal128ToWord8(A) = 1);
  Assert(Decimal128ToStr(A) = '1.0000000000000000000');
  Assert(Decimal128ToStrB(A) = '1.0000000000000000000');
  Assert(Decimal128EqualsWord8(A, 1));
  Assert(Decimal128CompareWord8(A, 0) = 1);
  Assert(Decimal128CompareWord8(A, 1) = 0);
  Assert(Decimal128CompareWord8(A, 2) = -1);
  Assert(Word64ToStr(Decimal128Trunc(A)) = '1');
  Assert(Word64ToStr(Decimal128Round(A)) = '1');
  Assert(Decimal128EqualsFloat(A, 1.0));
  Assert(Decimal128EqualsFloat(A, 1.000000000000000000049));
  Assert(Decimal128EqualsFloat(A, 1.000000000000000000051));

  Decimal128InitWord8(A, 2);
  Assert(Decimal128ToWord8(A) = 2);
  Assert(Decimal128EqualsWord16(A, 2));
  Assert(Decimal128EqualsWord32(A, 2));
  Assert(not Decimal128EqualsWord32(A, 999999999));
  Assert(Decimal128CompareWord16(A, 2) = 0);
  Assert(Decimal128CompareWord32(A, 999999999) = -1);

  Decimal128InitWord16(A, $FFFF);
  Assert(Decimal128ToWord16(A) = $FFFF);
  Assert(Decimal128ToStr(A) = '65535.0000000000000000000');

  Decimal128InitWord32(A, $FFFFFFFF);
  Assert(Decimal128ToWord32(A) = $FFFFFFFF);
  Assert(Decimal128ToStr(A) = '4294967295.0000000000000000000');

  Assert(FloatIsDecimal128Range(0.000000000));
  Assert(not FloatIsDecimal128Range(-0.0000000000000000001));
  Assert(FloatIsDecimal128Range(-0.00000000000000000005));
  Assert(not FloatIsDecimal128Range(-0.00000000051));
  Assert(not FloatIsDecimal128Range(9999999999999999999.999999999));
  Assert(not FloatIsDecimal128Range(10000000000000000000.000000000));

  Decimal128InitWord64(A, Decimal128MaxInt);
  Assert(Word64ToStr(Decimal128ToWord64(A)) = '9999999999999999999');
  Assert(Decimal128ToStr(A) = '9999999999999999999.0000000000000000000');
  Assert(Word64ToStr(Decimal128Trunc(A)) = '9999999999999999999');

  Decimal128InitFloat(A, 1.234567890123456789);
  Assert(Abs(Decimal128ToFloat(A) - 1.23456789012346) < 1e-10);
  Assert(CopyLeft(Decimal128ToStr(A), 16) = '1.23456789012345');
  Assert(Word64ToStr(Decimal128Trunc(A)) = '1');
  Assert(Word64ToStr(Decimal128Round(A)) = '1');
  Assert(Decimal128EqualsFloat(A, 1.234567890123456789));

  Decimal128InitFloat(A, 1.2345111115);
  Assert(Abs(Decimal128ToFloat(A) - 1.2345111115) < 1e-12);
  Assert(CopyLeft(Decimal128ToStr(A), 16) = '1.23451111150000');

  A := StrToDecimal128('9999999999999999999.9999999999999999999');
  Assert(Decimal128IsMaximum(A));

  A := StrToDecimal128B('9999999999999999999.9999999999999999999');
  Assert(Decimal128IsMaximum(A));

  Decimal128InitMax(A);
  Assert(Decimal128IsMaximum(A));
  Assert(not Decimal128IsOverflow(A));
  Assert(Decimal128ToStr(A) = '9999999999999999999.9999999999999999999');
  Assert(Word64ToStr(Decimal128Trunc(A)) = '9999999999999999999');
  Assert(Word64ToStr(Decimal128Round(A)) = '10000000000000000000');
  Assert(Word64ToStr(Decimal128FracWord(A)) = '9999999999999999999');

  A := StrToDecimal128('9999999999999999999.4999999999999999999');
  Assert(Word64ToStr(Decimal128Trunc(A)) = '9999999999999999999');
  Assert(Word64ToStr(Decimal128Round(A)) = '9999999999999999999');
  Assert(Word64ToStr(Decimal128FracWord(A)) = '4999999999999999999');

  A := StrToDecimal128('9999999999999999999.500000000');
  Assert(Word64ToStr(Decimal128Trunc(A)) = '9999999999999999999');
  Assert(Word64ToStr(Decimal128Round(A)) = '10000000000000000000');
  Assert(Word64ToStr(Decimal128FracWord(A)) = '5000000000000000000');

  A := StrToDecimal128('9999999999999999998.500000000');
  Assert(Word64ToStr(Decimal128Trunc(A)) = '9999999999999999998');
  Assert(Word64ToStr(Decimal128Round(A)) = '9999999999999999998');
  Assert(Word64ToStr(Decimal128FracWord(A)) = '5000000000000000000');

  A := StrToDecimal128('9999999999999999998.5000000000000000001');
  Assert(Word64ToStr(Decimal128Trunc(A)) = '9999999999999999998');
  Assert(Word64ToStr(Decimal128Round(A)) = '9999999999999999999');
  Assert(Word64ToStr(Decimal128FracWord(A)) = '5000000000000000001');

  Decimal128InitFloat(A, 0.1);
  Assert(CopyLeft(Decimal128ToStr(A), 16) = '0.10000000000000');
  Assert(Decimal128EqualsFloat(A, 0.1));

  Decimal128InitFloat(A, 1.9375);
  Assert(Decimal128ToFloat(A) = 1.9375);
  Decimal128AddWord8(A, 1);
  Assert(Decimal128ToFloat(A) = 2.9375);
  Decimal128AddWord16(A, 10000);
  Assert(Decimal128ToFloat(A) = 10002.9375);
  Decimal128AddWord32(A, 10000);
  Assert(Decimal128ToFloat(A) = 20002.9375);
  Assert(Decimal128ToStr(A) = '20002.9375000000000000000');
  Decimal128SubtractWord32(A, 10000);
  Assert(Decimal128ToFloat(A) = 10002.9375);
  Decimal128SubtractWord16(A, 10000);
  Assert(Decimal128ToFloat(A) = 2.9375);
  Decimal128SubtractWord8(A, 1);
  Assert(Decimal128ToFloat(A) = 1.9375);

  Decimal128InitFloat(A, 1.9375);
  Decimal128InitFloat(B, 2.5000);
  Decimal128AddDecimal128(A, B);
  Assert(Decimal128ToFloat(A) = 4.4375);
  Decimal128SubtractDecimal128(A, B);
  Assert(Decimal128ToFloat(A) = 1.9375);

  Decimal128InitFloat(A, 1.9375);
  Decimal128InitFloat(B, 2.5000);
  Assert(not Decimal128EqualsDecimal128(A, B));
  Assert(Decimal128EqualsDecimal128(A, A));
  Assert(Decimal128CompareDecimal128(A, B) = -1);
  Assert(Decimal128CompareDecimal128(A, A) = 0);
  Assert(Decimal128CompareDecimal128(B, A) = 1);

  Decimal128InitFloat(A, 1.9375);
  Decimal128MultiplyWord8(A, 3);
  Assert(Decimal128ToFloat(A) = 5.8125);
  Decimal128MultiplyWord16(A, 101);
  Assert(Decimal128ToFloat(A) = 587.0625);
  Decimal128MultiplyWord32(A, 13);
  Assert(Abs(Decimal128ToFloat(A) - 7631.8125) < 1e-10);
  Assert(Decimal128ToStr(A) = '7631.8125000000000000000');
  Decimal128DivideWord32(A, 13);
  Assert(Decimal128ToFloat(A) = 587.0625);
  Decimal128DivideWord16(A, 101);
  Assert(Decimal128ToFloat(A) = 5.8125);
  Decimal128DivideWord8(A, 3);
  Assert(Decimal128ToStr(A) = '1.9375000000000000000');

  Decimal128InitFloat(A, 1.9375);
  Decimal128InitFloat(B, 2.5000);
  Decimal128MultiplyDecimal128(A, B);
  Assert(Decimal128ToFloat(A) = 4.84375);
  Assert(Decimal128ToStr(A) = '4.8437500000000000000');
  Decimal128DivideDecimal128(A, B);
  Assert(Decimal128ToStr(A) = '1.9375000000000000000');

  Decimal128InitFloat(A, 12.937512);
  Decimal128InitFloat(B, 3.123433);
  Decimal128MultiplyDecimal128(A, B);
  Assert(Decimal128ToFloat(A) - 40.409451919 < 1e-10);
  Assert(Decimal128ToStr(A) = '40.4094519186960000000');

  Decimal64InitFloat(C, 1.9375);
  Decimal128InitDecimal64(A, C);
  Assert(Decimal128ToStr(A) = '1.9375000000000000000');

  A := StrToDecimal128('12.3456789');
  Decimal128Sqr(A);
  Assert(Decimal128ToStr(A) = '152.4157875019052100000');

  A := StrToDecimal128('0');
  Assert(Decimal128ToWord8(A) = 0);
  A := StrToDecimal128('000000000000000000000.000000000000000000000');
  Assert(Decimal128ToWord8(A) = 0);
  A := StrToDecimal128('1');
  Assert(Decimal128ToWord8(A) = 1);
  A := StrToDecimal128('1.0');
  Assert(Decimal128ToWord8(A) = 1);
  A := StrToDecimal128('0000000000000000000000000001.00000000000000000000000000000');
  Assert(Decimal128ToWord8(A) = 1);
  A := StrToDecimal128('1.00000000000000000000000001');
  Assert(Decimal128ToWord8(A) = 1);
  A := StrToDecimal128('123.938500000');
  Assert(Decimal128ToStr(A) = '123.9385000000000000000');
  A := StrToDecimal128('123.93850000000000000005');
  Assert(Decimal128ToStr(A) = '123.9385000000000000000');
  A := StrToDecimal128('123.93850000000000000015');
  Assert(Decimal128ToStr(A) = '123.9385000000000000002');
  A := StrToDecimal128('123.93860000000000000025');
  Assert(Decimal128ToStr(A) = '123.9386000000000000002');
  A := StrToDecimal128('123.938600000000000000251');
  Assert(Decimal128ToStr(A) = '123.9386000000000000003');
  A := StrToDecimal128('9999999999999999999.9999999999999999999');
  Assert(Decimal128ToStr(A) = '9999999999999999999.9999999999999999999');

  Assert(TryStrToDecimal128('0', A) = dceNoError);
  Assert(Decimal128ToWord32(A) = 0);
  Assert(TryStrToDecimal128('9999999999999999999', A) = dceNoError);
  Assert(Word64ToStr(Decimal128ToWord64(A)) = '9999999999999999999');

  Assert(TryStrToDecimal128('10000000000000000000', A) = dceOverflowError);
  Assert(TryStrToDecimal128('9999999999999999999.99999999999999999995', A) = dceOverflowError);

  Assert(TryStrToDecimal128('', A) = dceConvertError);
  Assert(TryStrToDecimal128('-', A) = dceConvertError);
  Assert(TryStrToDecimal128('+', A) = dceConvertError);
  Assert(TryStrToDecimal128(' ', A) = dceConvertError);
  Assert(TryStrToDecimal128('.', A) = dceConvertError);
  Assert(TryStrToDecimal128('0.', A) = dceConvertError);
  Assert(TryStrToDecimal128('.0', A) = dceConvertError);
  Assert(TryStrToDecimal128(' 0', A) = dceConvertError);
  Assert(TryStrToDecimal128('0 ', A) = dceConvertError);
end;

procedure Test_HugeDecimal;
var
  A, B : HugeDecimal;
  F : Word64Rec;
  G : Word128;
begin
  HugeDecimalInit(A);
  HugeDecimalInit(B);

  // Zero
  HugeDecimalAssignZero(A);
  Assert(HugeDecimalIsZero(A));
  Assert(not HugeDecimalIsOne(A));
  Assert(HugeDecimalDigits(A) = 1);
  Assert(HugeDecimalIntegerDigits(A) = 1);
  Assert(HugeDecimalDecimalDigits(A) = 0);
  Assert(HugeDecimalToWord8(A) = 0);
  Assert(HugeDecimalToWord32(A) = 0);
  Assert(Word64ToWord32(HugeDecimalToWord64(A)) = 0);
  Assert(Word128ToWord32(HugeDecimalToWord128(A)) = 0);
  Assert(HugeDecimalToStr(A) = '0');
  Assert(HugeDecimalGetDigit(A, 0) = 0);
  Assert(HugeDecimalFracCompareHalf(A) = -1);
  Assert(HugeDecimalIsInteger(A));
  Assert(HugeDecimalIsWord8Range(A));
  Assert(HugeDecimalIsEven(A));
  Assert(not HugeDecimalIsOdd(A));
  Assert(HugeDecimalIsLessThanOne(A));
  Assert(not HugeDecimalIsOneOrGreater(A));
  Assert(HugeDecimalEqualsHugeDecimal(A, A));
  Assert(HugeDecimalCompareHugeDecimal(A, A) = 0);
  Assert(HugeDecimalEqualsWord8(A, 0));
  Assert(not HugeDecimalEqualsWord8(A, 1));
  Assert(not HugeDecimalEqualsWord8(A, 10));
  Assert(not HugeDecimalEqualsWord8(A, 100));
  Assert(HugeDecimalCompareWord8(A, 0) = 0);
  Assert(HugeDecimalCompareWord8(A, 1) = -1);
  Assert(HugeDecimalCompareWord8(A, 10) = -1);
  Assert(HugeDecimalCompareWord8(A, 100) = -1);

  // One
  HugeDecimalAssignOne(A);
  Assert(not HugeDecimalIsZero(A));
  Assert(HugeDecimalIsOne(A));
  Assert(HugeDecimalDigits(A) = 1);
  Assert(HugeDecimalIntegerDigits(A) = 1);
  Assert(HugeDecimalDecimalDigits(A) = 0);
  Assert(HugeDecimalToWord8(A) = 1);
  Assert(HugeDecimalToWord32(A) = 1);
  Assert(Word64ToWord32(HugeDecimalToWord64(A)) = 1);
  Assert(Word128ToWord32(HugeDecimalToWord128(A)) = 1);
  Assert(HugeDecimalToStr(A) = '1');
  Assert(HugeDecimalGetDigit(A, 0) = 1);
  Assert(HugeDecimalFracCompareHalf(A) = -1);
  Assert(HugeDecimalIsInteger(A));
  Assert(HugeDecimalIsWord8Range(A));
  Assert(not HugeDecimalIsEven(A));
  Assert(HugeDecimalIsOdd(A));
  Assert(not HugeDecimalIsLessThanOne(A));
  Assert(HugeDecimalIsOneOrGreater(A));
  Assert(HugeDecimalEqualsHugeDecimal(A, A));
  Assert(HugeDecimalCompareHugeDecimal(A, A) = 0);
  Assert(HugeDecimalEqualsWord8(A, 1));
  Assert(not HugeDecimalEqualsWord8(A, 2));
  Assert(not HugeDecimalEqualsWord8(A, 10));
  Assert(not HugeDecimalEqualsWord8(A, 100));
  Assert(HugeDecimalCompareWord8(A, 0) = 1);
  Assert(HugeDecimalCompareWord8(A, 1) = 0);
  Assert(HugeDecimalCompareWord8(A, 2) = -1);
  Assert(HugeDecimalCompareWord8(A, 12) = -1);
  Assert(HugeDecimalCompareWord8(A, 123) = -1);

  // 10
  HugeDecimalAssignWord8(A, 10);
  Assert(not HugeDecimalIsZero(A));
  Assert(not HugeDecimalIsOne(A));
  Assert(HugeDecimalDigits(A) = 2);
  Assert(HugeDecimalIntegerDigits(A) = 2);
  Assert(HugeDecimalDecimalDigits(A) = 0);
  Assert(HugeDecimalToWord8(A) = 10);
  Assert(HugeDecimalToWord32(A) = 10);
  Assert(HugeDecimalToStr(A) = '10');
  Assert(HugeDecimalGetDigit(A, 0) = 1);
  Assert(HugeDecimalGetDigit(A, 1) = 0);
  Assert(HugeDecimalIsEven(A));
  Assert(not HugeDecimalIsOdd(A));
  Assert(not HugeDecimalIsLessThanOne(A));
  Assert(HugeDecimalIsOneOrGreater(A));
  Assert(HugeDecimalEqualsHugeDecimal(A, A));
  Assert(HugeDecimalCompareHugeDecimal(A, A) = 0);
  Assert(HugeDecimalEqualsWord8(A, 10));
  Assert(not HugeDecimalEqualsWord8(A, 1));
  Assert(not HugeDecimalEqualsWord8(A, 11));
  Assert(not HugeDecimalEqualsWord8(A, 100));
  Assert(HugeDecimalCompareWord8(A, 0) = 1);
  Assert(HugeDecimalCompareWord8(A, 9) = 1);
  Assert(HugeDecimalCompareWord8(A, 10) = 0);
  Assert(HugeDecimalCompareWord8(A, 11) = -1);
  Assert(HugeDecimalCompareWord8(A, 123) = -1);

  // 123
  HugeDecimalAssignWord8(A, 123);
  Assert(HugeDecimalDigits(A) = 3);
  Assert(HugeDecimalIntegerDigits(A) = 3);
  Assert(HugeDecimalDecimalDigits(A) = 0);
  Assert(HugeDecimalToWord8(A) = 123);
  Assert(HugeDecimalToWord32(A) = 123);
  Assert(HugeDecimalToStr(A) = '123');
  Assert(HugeDecimalGetDigit(A, 0) = 1);
  Assert(HugeDecimalGetDigit(A, 1) = 2);
  Assert(HugeDecimalGetDigit(A, 2) = 3);
  Assert(HugeDecimalIsWord8Range(A));
  Assert(HugeDecimalIsOdd(A));
  Assert(HugeDecimalEqualsWord8(A, 123));
  Assert(not HugeDecimalEqualsWord8(A, 1));
  Assert(not HugeDecimalEqualsWord8(A, 11));
  Assert(not HugeDecimalEqualsWord8(A, 255));
  Assert(HugeDecimalCompareWord8(A, 0) = 1);
  Assert(HugeDecimalCompareWord8(A, 1) = 1);
  Assert(HugeDecimalCompareWord8(A, 12) = 1);
  Assert(HugeDecimalCompareWord8(A, 122) = 1);
  Assert(HugeDecimalCompareWord8(A, 123) = 0);
  Assert(HugeDecimalCompareWord8(A, 124) = -1);

  // 256
  HugeDecimalAssignWord32(A, 256);
  Assert(HugeDecimalDigits(A) = 3);
  Assert(HugeDecimalIntegerDigits(A) = 3);
  Assert(HugeDecimalDecimalDigits(A) = 0);
  Assert(HugeDecimalToWord32(A) = 256);
  Assert(not HugeDecimalIsWord8Range(A));
  Assert(not HugeDecimalEqualsWord8(A, 1));
  Assert(not HugeDecimalEqualsWord8(A, 11));
  Assert(not HugeDecimalEqualsWord8(A, 255));
  Assert(HugeDecimalCompareWord8(A, 255) = 1);

  // 1234
  HugeDecimalAssignWord32(A, 1234);
  Assert(HugeDecimalDigits(A) = 4);
  Assert(HugeDecimalToWord32(A) = 1234);
  Assert(HugeDecimalToStr(A) = '1234');
  Assert(HugeDecimalGetDigit(A, 0) = 1);
  Assert(HugeDecimalGetDigit(A, 1) = 2);
  Assert(HugeDecimalGetDigit(A, 2) = 3);
  Assert(HugeDecimalGetDigit(A, 3) = 4);
  Assert(not HugeDecimalIsWord8Range(A));

  // 12345
  HugeDecimalAssignWord32(A, 12345);
  Assert(HugeDecimalDigits(A) = 5);
  Assert(HugeDecimalToWord32(A) = 12345);
  Assert(HugeDecimalToStr(A) = '12345');

  // 123456
  HugeDecimalAssignWord32(A, 123456);
  Assert(HugeDecimalDigits(A) = 6);
  Assert(HugeDecimalToWord32(A) = 123456);
  Assert(HugeDecimalToStr(A) = '123456');

  Word64InitOne(F);
  HugeDecimalAssignWord64(A, F);
  Assert(HugeDecimalIsOne(A));

  // Word64Max
  Word64InitMaximum(F);
  HugeDecimalAssignWord64(A, F);
  Assert(HugeDecimalToStr(A) = '18446744073709551615');
  Assert(HugeDecimalDigits(A) = 20);
  Assert(HugeDecimalIntegerDigits(A) = 20);
  Assert(HugeDecimalDecimalDigits(A) = 0);
  Assert(HugeDecimalGetDigit(A, 0) = 1);
  Assert(HugeDecimalGetDigit(A, 19) = 5);
  Assert(Word64ToStr(HugeDecimalToWord64(A)) = '18446744073709551615');
  Assert(Word128ToStr(HugeDecimalToWord128(A)) = '18446744073709551615');

  Word128InitOne(G);
  HugeDecimalAssignWord128(A, G);
  Assert(HugeDecimalIsOne(A));

  // Word128Max
  Word128InitMaximum(G);
  HugeDecimalAssignWord128(A, G);
  Assert(HugeDecimalToStr(A) = '340282366920938463463374607431768211455');
  Assert(HugeDecimalDigits(A) = 39);
  Assert(HugeDecimalIntegerDigits(A) = 39);
  Assert(HugeDecimalDecimalDigits(A) = 0);
  Assert(HugeDecimalGetDigit(A, 0) = 3);
  Assert(HugeDecimalGetDigit(A, 36) = 4);
  Assert(HugeDecimalGetDigit(A, 37) = 5);
  Assert(HugeDecimalGetDigit(A, 38) = 5);
  Assert(Word128ToStr(HugeDecimalToWord128(A)) = '340282366920938463463374607431768211455');

  // 0.0000
  HugeDecimalAssignDecimal32(A, StrToDecimal32('0'));
  Assert(HugeDecimalToStr(A) = '0');
  Assert(Decimal32ToStr(HugeDecimalToDecimal32(A)) = '0.0000');
  Assert(HugeDecimalIsInteger(A));

  // 1.0000
  HugeDecimalAssignDecimal32(A, StrToDecimal32('1'));
  Assert(HugeDecimalToStr(A) = '1');
  Assert(Decimal32ToStr(HugeDecimalToDecimal32(A)) = '1.0000');
  Assert(HugeDecimalIsInteger(A));

  // 12.0000
  HugeDecimalAssignDecimal32(A, StrToDecimal32('12'));
  Assert(HugeDecimalToStr(A) = '12');
  Assert(Decimal32ToStr(HugeDecimalToDecimal32(A)) = '12.0000');

  // 123.0000
  HugeDecimalAssignDecimal32(A, StrToDecimal32('123'));
  Assert(HugeDecimalToStr(A) = '123');
  Assert(Decimal32ToStr(HugeDecimalToDecimal32(A)) = '123.0000');

  // 1234.0000
  HugeDecimalAssignDecimal32(A, StrToDecimal32('1234'));
  Assert(HugeDecimalToStr(A) = '1234');
  Assert(Decimal32ToStr(HugeDecimalToDecimal32(A)) = '1234.0000');

  // 12345.0000
  HugeDecimalAssignDecimal32(A, StrToDecimal32('12345'));
  Assert(HugeDecimalToStr(A) = '12345');
  Assert(Decimal32ToStr(HugeDecimalToDecimal32(A)) = '12345.0000');

  // 0.1
  HugeDecimalAssignDecimal32(A, StrToDecimal32('0.1'));
  Assert(HugeDecimalToStr(A) = '0.1');
  Assert(Decimal32ToStr(HugeDecimalToDecimal32(A)) = '0.1000');
  Assert(HugeDecimalDigits(A) = 1);
  Assert(HugeDecimalIntegerDigits(A) = 0);
  Assert(HugeDecimalDecimalDigits(A) = 1);
  Assert(HugeDecimalGetDigit(A, 0) = 1);
  Assert(HugeDecimalFracCompareHalf(A) = -1);
  Assert(not HugeDecimalIsInteger(A));
  Assert(HugeDecimalIsLessThanOne(A));
  Assert(not HugeDecimalIsOneOrGreater(A));
  Assert(HugeDecimalEqualsHugeDecimal(A, A));
  Assert(HugeDecimalCompareHugeDecimal(A, A) = 0);
  Assert(HugeDecimalCompareWord8(A, 0) = 1);
  Assert(HugeDecimalCompareWord8(A, 1) = -1);

  // 0.01
  HugeDecimalAssignDecimal32(A, StrToDecimal32('0.01'));
  Assert(HugeDecimalDigits(A) = 2);
  Assert(HugeDecimalIntegerDigits(A) = 0);
  Assert(HugeDecimalDecimalDigits(A) = 2);
  Assert(HugeDecimalToStr(A) = '0.01');
  Assert(Decimal32ToStr(HugeDecimalToDecimal32(A)) = '0.0100');
  Assert(HugeDecimalIsLessThanOne(A));
  Assert(not HugeDecimalIsOneOrGreater(A));
  Assert(HugeDecimalEqualsHugeDecimal(A, A));
  Assert(HugeDecimalCompareHugeDecimal(A, A) = 0);
  Assert(HugeDecimalCompareWord8(A, 0) = 1);
  Assert(HugeDecimalCompareWord8(A, 1) = -1);

  // 20.01
  HugeDecimalAssignDecimal32(A, StrToDecimal32('20.01'));
  Assert(HugeDecimalDigits(A) = 4);
  Assert(HugeDecimalIntegerDigits(A) = 2);
  Assert(HugeDecimalDecimalDigits(A) = 2);
  Assert(HugeDecimalToStr(A) = '20.01');
  Assert(Decimal32ToStr(HugeDecimalToDecimal32(A)) = '20.0100');
  Assert(not HugeDecimalIsLessThanOne(A));
  Assert(HugeDecimalIsOneOrGreater(A));
  Assert(HugeDecimalEqualsHugeDecimal(A, A));
  Assert(HugeDecimalCompareHugeDecimal(A, A) = 0);
  Assert(not HugeDecimalEqualsWord8(A, 20));
  Assert(not HugeDecimalEqualsWord8(A, 21));
  Assert(HugeDecimalCompareWord8(A, 20) = 1);
  Assert(HugeDecimalCompareWord8(A, 21) = -1);

  // 0.12
  HugeDecimalAssignDecimal32(A, StrToDecimal32('0.12'));
  Assert(HugeDecimalToStr(A) = '0.12');
  Assert(Decimal32ToStr(HugeDecimalToDecimal32(A)) = '0.1200');
  Assert(HugeDecimalDigits(A) = 2);
  Assert(HugeDecimalIntegerDigits(A) = 0);
  Assert(HugeDecimalDecimalDigits(A) = 2);
  Assert(HugeDecimalGetDigit(A, 0) = 1);
  Assert(HugeDecimalGetDigit(A, 1) = 2);
  Assert(not HugeDecimalIsInteger(A));
  Assert(HugeDecimalIsLessThanOne(A));
  Assert(not HugeDecimalEqualsWord8(A, 0));

  // 0.123
  HugeDecimalAssignDecimal32(A, StrToDecimal32('0.123'));
  Assert(HugeDecimalToStr(A) = '0.123');
  Assert(Decimal32ToStr(HugeDecimalToDecimal32(A)) = '0.1230');
  Assert(HugeDecimalDigits(A) = 3);
  Assert(HugeDecimalIntegerDigits(A) = 0);
  Assert(HugeDecimalDecimalDigits(A) = 3);
  Assert(HugeDecimalGetDigit(A, 0) = 1);
  Assert(HugeDecimalGetDigit(A, 1) = 2);
  Assert(HugeDecimalGetDigit(A, 2) = 3);
  Assert(not HugeDecimalEqualsWord8(A, 0));

  // 0.1234
  HugeDecimalAssignDecimal32(A, StrToDecimal32('0.1234'));
  Assert(HugeDecimalToStr(A) = '0.1234');
  Assert(Decimal32ToStr(HugeDecimalToDecimal32(A)) = '0.1234');
  Assert(HugeDecimalDigits(A) = 4);
  Assert(HugeDecimalIntegerDigits(A) = 0);
  Assert(HugeDecimalDecimalDigits(A) = 4);

  // 1.2
  HugeDecimalAssignDecimal32(A, StrToDecimal32('1.2'));
  Assert(HugeDecimalToStr(A) = '1.2');
  Assert(Decimal32ToStr(HugeDecimalToDecimal32(A)) = '1.2000');
  Assert(HugeDecimalDigits(A) = 2);
  Assert(HugeDecimalIntegerDigits(A) = 1);
  Assert(HugeDecimalDecimalDigits(A) = 1);
  Assert(HugeDecimalGetDigit(A, 0) = 1);
  Assert(HugeDecimalGetDigit(A, 1) = 2);
  Assert(not HugeDecimalIsLessThanOne(A));

  // 1.23
  HugeDecimalAssignDecimal32(A, StrToDecimal32('1.23'));
  Assert(HugeDecimalToStr(A) = '1.23');
  Assert(Decimal32ToStr(HugeDecimalToDecimal32(A)) = '1.2300');
  Assert(HugeDecimalDigits(A) = 3);
  Assert(HugeDecimalIntegerDigits(A) = 1);
  Assert(HugeDecimalDecimalDigits(A) = 2);
  Assert(HugeDecimalGetDigit(A, 0) = 1);
  Assert(HugeDecimalGetDigit(A, 1) = 2);
  Assert(HugeDecimalGetDigit(A, 2) = 3);
  Assert(not HugeDecimalEqualsWord8(A, 1));
  Assert(HugeDecimalCompareWord8(A, 0) = 1);
  Assert(HugeDecimalCompareWord8(A, 1) = 1);
  Assert(HugeDecimalCompareWord8(A, 2) = -1);

  // 1.234
  HugeDecimalAssignDecimal32(A, StrToDecimal32('1.234'));
  Assert(HugeDecimalToStr(A) = '1.234');
  Assert(Decimal32ToStr(HugeDecimalToDecimal32(A)) = '1.2340');
  Assert(HugeDecimalDigits(A) = 4);
  Assert(HugeDecimalIntegerDigits(A) = 1);
  Assert(HugeDecimalDecimalDigits(A) = 3);
  Assert(HugeDecimalGetDigit(A, 0) = 1);
  Assert(HugeDecimalGetDigit(A, 1) = 2);
  Assert(HugeDecimalGetDigit(A, 2) = 3);
  Assert(HugeDecimalGetDigit(A, 3) = 4);

  // 1.2345
  HugeDecimalAssignDecimal32(A, StrToDecimal32('1.2345'));
  Assert(HugeDecimalToStr(A) = '1.2345');
  Assert(Decimal32ToStr(HugeDecimalToDecimal32(A)) = '1.2345');
  Assert(HugeDecimalDigits(A) = 5);
  Assert(HugeDecimalIntegerDigits(A) = 1);
  Assert(HugeDecimalDecimalDigits(A) = 4);

  // 21.2345
  HugeDecimalAssignDecimal32(A, StrToDecimal32('21.2345'));
  Assert(HugeDecimalToStr(A) = '21.2345');
  Assert(Decimal32ToStr(HugeDecimalToDecimal32(A)) = '21.2345');
  Assert(HugeDecimalDigits(A) = 6);
  Assert(HugeDecimalIntegerDigits(A) = 2);
  Assert(HugeDecimalDecimalDigits(A) = 4);
  Assert(HugeDecimalGetDigit(A, 0) = 2);
  Assert(HugeDecimalGetDigit(A, 1) = 1);
  Assert(HugeDecimalGetDigit(A, 2) = 2);
  Assert(HugeDecimalGetDigit(A, 3) = 3);
  Assert(HugeDecimalGetDigit(A, 4) = 4);
  Assert(HugeDecimalGetDigit(A, 5) = 5);
  Assert(HugeDecimalCompareWord8(A, 1) = 1);
  Assert(HugeDecimalCompareWord8(A, 21) = 1);
  Assert(HugeDecimalCompareWord8(A, 22) = -1);
  Assert(HugeDecimalCompareWord8(A, 123) = -1);

  // 321.2345
  HugeDecimalAssignDecimal32(A, StrToDecimal32('321.2345'));
  Assert(HugeDecimalToStr(A) = '321.2345');
  Assert(Decimal32ToStr(HugeDecimalToDecimal32(A)) = '321.2345');
  Assert(HugeDecimalDigits(A) = 7);
  Assert(HugeDecimalIntegerDigits(A) = 3);
  Assert(HugeDecimalDecimalDigits(A) = 4);
  Assert(HugeDecimalGetDigit(A, 0) = 3);
  Assert(HugeDecimalGetDigit(A, 1) = 2);
  Assert(HugeDecimalGetDigit(A, 2) = 1);
  Assert(HugeDecimalGetDigit(A, 3) = 2);
  Assert(HugeDecimalGetDigit(A, 4) = 3);
  Assert(HugeDecimalGetDigit(A, 5) = 4);
  Assert(HugeDecimalGetDigit(A, 6) = 5);
  Assert(HugeDecimalCompareWord8(A, 1) = 1);
  Assert(HugeDecimalCompareWord8(A, 21) = 1);
  Assert(HugeDecimalCompareWord8(A, 255) = 1);

  // 4321.2345
  HugeDecimalAssignDecimal32(A, StrToDecimal32('4321.2345'));
  Assert(HugeDecimalToStr(A) = '4321.2345');
  Assert(Decimal32ToStr(HugeDecimalToDecimal32(A)) = '4321.2345');

  // 54321.2345
  HugeDecimalAssignDecimal32(A, StrToDecimal32('54321.2345'));
  Assert(HugeDecimalToStr(A) = '54321.2345');
  Assert(Decimal32ToStr(HugeDecimalToDecimal32(A)) = '54321.2345');

  // 54321.234
  HugeDecimalAssignDecimal32(A, StrToDecimal32('54321.234'));
  Assert(HugeDecimalToStr(A) = '54321.234');
  Assert(Decimal32ToStr(HugeDecimalToDecimal32(A)) = '54321.2340');

  // 54321.23
  HugeDecimalAssignDecimal32(A, StrToDecimal32('54321.23'));
  Assert(HugeDecimalToStr(A) = '54321.23');
  Assert(Decimal32ToStr(HugeDecimalToDecimal32(A)) = '54321.2300');

  // 54321.2
  HugeDecimalAssignDecimal32(A, StrToDecimal32('54321.2'));
  Assert(HugeDecimalToStr(A) = '54321.2');
  Assert(Decimal32ToStr(HugeDecimalToDecimal32(A)) = '54321.2000');

  // 54321
  HugeDecimalAssignDecimal32(A, StrToDecimal32('54321'));
  Assert(HugeDecimalToStr(A) = '54321');
  Assert(Decimal32ToStr(HugeDecimalToDecimal32(A)) = '54321.0000');

  // 0.000000000
  HugeDecimalAssignDecimal64(A, StrToDecimal64('0'));
  Assert(HugeDecimalToStr(A) = '0');
  Assert(Decimal64ToStr(HugeDecimalToDecimal64(A)) = '0.000000000');

  // 0.01
  HugeDecimalAssignDecimal64(A, StrToDecimal64('0.01'));
  Assert(HugeDecimalToStr(A) = '0.01');
  Assert(Decimal64ToStr(HugeDecimalToDecimal64(A)) = '0.010000000');

  // 0.00102
  HugeDecimalAssignDecimal64(A, StrToDecimal64('0.00102'));
  Assert(HugeDecimalToStr(A) = '0.00102');
  Assert(Decimal64ToStr(HugeDecimalToDecimal64(A)) = '0.001020000');

  // 0.1
  HugeDecimalAssignDecimal64(A, StrToDecimal64('0.1'));
  Assert(HugeDecimalToStr(A) = '0.1');
  Assert(Decimal64ToStr(HugeDecimalToDecimal64(A)) = '0.100000000');

  // 0.12
  HugeDecimalAssignDecimal64(A, StrToDecimal64('0.12'));
  Assert(HugeDecimalToStr(A) = '0.12');
  Assert(Decimal64ToStr(HugeDecimalToDecimal64(A)) = '0.120000000');

  // 0.123
  HugeDecimalAssignDecimal64(A, StrToDecimal64('0.123'));
  Assert(HugeDecimalToStr(A) = '0.123');
  Assert(Decimal64ToStr(HugeDecimalToDecimal64(A)) = '0.123000000');

  // 0.12345678
  HugeDecimalAssignDecimal64(A, StrToDecimal64('0.12345678'));
  Assert(HugeDecimalToStr(A) = '0.12345678');
  Assert(Decimal64ToStr(HugeDecimalToDecimal64(A)) = '0.123456780');

  // 0.123456789
  HugeDecimalAssignDecimal64(A, StrToDecimal64('0.123456789'));
  Assert(HugeDecimalToStr(A) = '0.123456789');
  Assert(Decimal64ToStr(HugeDecimalToDecimal64(A)) = '0.123456789');

  // 1.2
  HugeDecimalAssignDecimal64(A, StrToDecimal64('1.2'));
  Assert(HugeDecimalToStr(A) = '1.2');
  Assert(Decimal64ToStr(HugeDecimalToDecimal64(A)) = '1.200000000');

  // 1.23
  HugeDecimalAssignDecimal64(A, StrToDecimal64('1.23'));
  Assert(HugeDecimalToStr(A) = '1.23');
  Assert(Decimal64ToStr(HugeDecimalToDecimal64(A)) = '1.230000000');

  // 1.234
  HugeDecimalAssignDecimal64(A, StrToDecimal64('1.234'));
  Assert(HugeDecimalToStr(A) = '1.234');
  Assert(Decimal64ToStr(HugeDecimalToDecimal64(A)) = '1.234000000');

  // 123456789.123456789
  HugeDecimalAssignDecimal64(A, StrToDecimal64('123456789.123456789'));
  Assert(HugeDecimalToStr(A) = '123456789.123456789');
  Assert(Decimal64ToStr(HugeDecimalToDecimal64(A)) = '123456789.123456789');
  Assert(HugeDecimalDigits(A) = 18);
  Assert(HugeDecimalIntegerDigits(A) = 9);
  Assert(HugeDecimalDecimalDigits(A) = 9);

  // 1234567890.123456789
  HugeDecimalAssignDecimal64(A, StrToDecimal64('1234567890.123456789'));
  Assert(HugeDecimalToStr(A) = '1234567890.123456789');
  Assert(Decimal64ToStr(HugeDecimalToDecimal64(A)) = '1234567890.123456789');
  Assert(HugeDecimalDigits(A) = 19);
  Assert(HugeDecimalIntegerDigits(A) = 10);
  Assert(HugeDecimalDecimalDigits(A) = 9);

  // 9999999999.999999999
  HugeDecimalAssignDecimal64(A, StrToDecimal64('9999999999.999999999'));
  Assert(HugeDecimalToStr(A) = '9999999999.999999999');
  Assert(Decimal64ToStr(HugeDecimalToDecimal64(A)) = '9999999999.999999999');
  Assert(HugeDecimalDigits(A) = 19);
  Assert(HugeDecimalIntegerDigits(A) = 10);
  Assert(HugeDecimalDecimalDigits(A) = 9);

  // 0.0000000000000000000
  HugeDecimalAssignDecimal128(A, StrToDecimal128('0.0'));
  Assert(Decimal128ToStr(HugeDecimalToDecimal128(A)) = '0.0000000000000000000');

  // 0.1234567890123456789
  HugeDecimalAssignDecimal128(A, StrToDecimal128('0.1234567890123456789'));
  Assert(Decimal128ToStr(HugeDecimalToDecimal128(A)) = '0.1234567890123456789');

  // Trunc
  HugeDecimalAssignDecimal32(A, StrToDecimal32('0.0'));
  HugeDecimalTrunc(A);
  Assert(HugeDecimalToStr(A) = '0');
  HugeDecimalAssignDecimal32(A, StrToDecimal32('0.1'));
  HugeDecimalTrunc(A);
  Assert(HugeDecimalToStr(A) = '0');
  HugeDecimalAssignDecimal32(A, StrToDecimal32('1.0'));
  HugeDecimalTrunc(A);
  Assert(HugeDecimalToStr(A) = '1');
  HugeDecimalAssignDecimal32(A, StrToDecimal32('1.2'));
  HugeDecimalTrunc(A);
  Assert(HugeDecimalToStr(A) = '1');
  HugeDecimalAssignDecimal32(A, StrToDecimal32('1.23'));
  HugeDecimalTrunc(A);
  Assert(HugeDecimalToStr(A) = '1');
  HugeDecimalAssignDecimal32(A, StrToDecimal32('1.234'));
  HugeDecimalTrunc(A);
  Assert(HugeDecimalToStr(A) = '1');
  HugeDecimalAssignDecimal32(A, StrToDecimal32('1.2345'));
  HugeDecimalTrunc(A);
  Assert(HugeDecimalToStr(A) = '1');
  HugeDecimalAssignDecimal32(A, StrToDecimal32('12.0'));
  HugeDecimalTrunc(A);
  Assert(HugeDecimalToStr(A) = '12');
  HugeDecimalAssignDecimal32(A, StrToDecimal32('12.3'));
  HugeDecimalTrunc(A);
  Assert(HugeDecimalToStr(A) = '12');
  HugeDecimalAssignDecimal32(A, StrToDecimal32('12.34'));
  HugeDecimalTrunc(A);
  Assert(HugeDecimalToStr(A) = '12');
  HugeDecimalAssignDecimal32(A, StrToDecimal32('12.345'));
  HugeDecimalTrunc(A);
  Assert(HugeDecimalToStr(A) = '12');
  HugeDecimalAssignDecimal32(A, StrToDecimal32('12.3456'));
  HugeDecimalTrunc(A);
  Assert(HugeDecimalToStr(A) = '12');
  HugeDecimalAssignDecimal32(A, StrToDecimal32('123.0'));
  HugeDecimalTrunc(A);
  Assert(HugeDecimalToStr(A) = '123');
  HugeDecimalAssignDecimal32(A, StrToDecimal32('123.4'));
  HugeDecimalTrunc(A);
  Assert(HugeDecimalToStr(A) = '123');
  HugeDecimalAssignDecimal32(A, StrToDecimal32('123.45'));
  HugeDecimalTrunc(A);
  Assert(HugeDecimalToStr(A) = '123');
  HugeDecimalAssignDecimal32(A, StrToDecimal32('123.456'));
  HugeDecimalTrunc(A);
  Assert(HugeDecimalToStr(A) = '123');
  HugeDecimalAssignDecimal32(A, StrToDecimal32('123.4567'));
  HugeDecimalTrunc(A);
  Assert(HugeDecimalToStr(A) = '123');
  HugeDecimalAssignDecimal32(A, StrToDecimal32('1234.0'));
  HugeDecimalTrunc(A);
  Assert(HugeDecimalToStr(A) = '1234');
  HugeDecimalAssignDecimal32(A, StrToDecimal32('1234.5'));
  HugeDecimalTrunc(A);
  Assert(HugeDecimalToStr(A) = '1234');
  HugeDecimalAssignDecimal32(A, StrToDecimal32('1234.56'));
  HugeDecimalTrunc(A);
  Assert(HugeDecimalToStr(A) = '1234');
  HugeDecimalAssignDecimal32(A, StrToDecimal32('1234.567'));
  HugeDecimalTrunc(A);
  Assert(HugeDecimalToStr(A) = '1234');
  HugeDecimalAssignDecimal32(A, StrToDecimal32('1234.5678'));
  HugeDecimalTrunc(A);
  Assert(HugeDecimalToStr(A) = '1234');
  HugeDecimalAssignDecimal64(A, StrToDecimal64('123456789.123456789'));
  HugeDecimalTrunc(A);
  Assert(HugeDecimalToStr(A) = '123456789');
  HugeDecimalAssignDecimal64(A, StrToDecimal64('1234567890.123456789'));
  HugeDecimalTrunc(A);
  Assert(HugeDecimalToStr(A) = '1234567890');

  // SetDigit
  HugeDecimalAssignDecimal32(A, StrToDecimal32('1234.5678'));
  HugeDecimalSetDigit(A, 0, 9);
  Assert(HugeDecimalToStr(A) = '9234.5678');
  HugeDecimalSetDigit(A, 1, 8);
  Assert(HugeDecimalToStr(A) = '9834.5678');
  HugeDecimalSetDigit(A, 2, 7);
  Assert(HugeDecimalToStr(A) = '9874.5678');
  HugeDecimalSetDigit(A, 3, 6);
  Assert(HugeDecimalToStr(A) = '9876.5678');
  HugeDecimalSetDigit(A, 4, 1);
  Assert(HugeDecimalToStr(A) = '9876.1678');
  HugeDecimalSetDigit(A, 5, 2);
  Assert(HugeDecimalToStr(A) = '9876.1278');
  HugeDecimalSetDigit(A, 6, 3);
  Assert(HugeDecimalToStr(A) = '9876.1238');
  HugeDecimalSetDigit(A, 7, 4);
  Assert(HugeDecimalToStr(A) = '9876.1234');

  // SetDigit
  HugeDecimalAssignDecimal32(A, StrToDecimal32('0.1'));
  HugeDecimalSetDigit(A, 0, 9);
  Assert(HugeDecimalToStr(A) = '0.9');
  HugeDecimalAssignDecimal32(A, StrToDecimal32('1.0'));
  HugeDecimalSetDigit(A, 0, 9);
  Assert(HugeDecimalToStr(A) = '9');
  HugeDecimalAssignDecimal32(A, StrToDecimal32('1.2'));
  HugeDecimalSetDigit(A, 0, 9);
  Assert(HugeDecimalToStr(A) = '9.2');
  HugeDecimalSetDigit(A, 1, 8);
  Assert(HugeDecimalToStr(A) = '9.8');

  // FracCompareHalf
  HugeDecimalAssignDecimal32(A, StrToDecimal32('0.4999'));
  Assert(HugeDecimalFracCompareHalf(A) = -1);
  Assert(not HugeDecimalIsInteger(A));
  HugeDecimalAssignDecimal32(A, StrToDecimal32('0.5000'));
  Assert(HugeDecimalFracCompareHalf(A) = 0);
  HugeDecimalAssignDecimal32(A, StrToDecimal32('0.5001'));
  Assert(HugeDecimalFracCompareHalf(A) = 1);

  // Zero Mul/Div
  HugeDecimalAssignZero(A);
  HugeDecimalMul10(A);
  Assert(HugeDecimalIsZero(A));
  HugeDecimalDiv10(A);
  Assert(HugeDecimalIsZero(A));

  // Mul10
  HugeDecimalAssignDecimal32(A, StrToDecimal32('0.001'));
  HugeDecimalMul10(A);
  Assert(HugeDecimalToStr(A) = '0.01');
  HugeDecimalMul10(A);
  Assert(HugeDecimalToStr(A) = '0.1');
  HugeDecimalMul10(A);
  Assert(HugeDecimalToStr(A) = '1');
  HugeDecimalMul10(A);
  Assert(HugeDecimalToStr(A) = '10');
  HugeDecimalMul10(A);
  Assert(HugeDecimalToStr(A) = '100');

  // Div10
  HugeDecimalAssignDecimal32(A, StrToDecimal32('100'));
  HugeDecimalDiv10(A);
  Assert(HugeDecimalToStr(A) = '10');
  HugeDecimalDiv10(A);
  Assert(HugeDecimalToStr(A) = '1');
  HugeDecimalDiv10(A);
  Assert(HugeDecimalToStr(A) = '0.1');
  HugeDecimalDiv10(A);
  Assert(HugeDecimalToStr(A) = '0.01');
  HugeDecimalDiv10(A);
  Assert(HugeDecimalToStr(A) = '0.001');

  // Mul10
  HugeDecimalAssignDecimal32(A, StrToDecimal32('0.0012'));
  HugeDecimalMul10(A);
  Assert(HugeDecimalToStr(A) = '0.012');
  HugeDecimalMul10(A);
  Assert(HugeDecimalToStr(A) = '0.12');
  HugeDecimalMul10(A);
  Assert(HugeDecimalToStr(A) = '1.2');
  HugeDecimalMul10(A);
  Assert(HugeDecimalToStr(A) = '12');
  HugeDecimalMul10(A);
  Assert(HugeDecimalToStr(A) = '120');

  // Div10
  HugeDecimalAssignDecimal32(A, StrToDecimal32('120'));
  HugeDecimalDiv10(A);
  Assert(HugeDecimalToStr(A) = '12');
  HugeDecimalDiv10(A);
  Assert(HugeDecimalToStr(A) = '1.2');
  HugeDecimalDiv10(A);
  Assert(HugeDecimalToStr(A) = '0.12');
  HugeDecimalDiv10(A);
  Assert(HugeDecimalToStr(A) = '0.012');
  HugeDecimalDiv10(A);
  Assert(HugeDecimalToStr(A) = '0.0012');

  // Mul10
  HugeDecimalAssignDecimal64(A, StrToDecimal64('0.00102'));
  Assert(HugeDecimalToStr(A) = '0.00102');
  HugeDecimalMul10(A);
  Assert(HugeDecimalToStr(A) = '0.0102');
  HugeDecimalMul10(A);
  Assert(HugeDecimalToStr(A) = '0.102');
  HugeDecimalMul10(A);
  Assert(HugeDecimalToStr(A) = '1.02');
  HugeDecimalMul10(A);
  Assert(HugeDecimalToStr(A) = '10.2');
  HugeDecimalMul10(A);
  Assert(HugeDecimalToStr(A) = '102');
  HugeDecimalMul10(A);
  Assert(HugeDecimalToStr(A) = '1020');

  // Div10
  HugeDecimalAssignDecimal32(A, StrToDecimal32('1020'));
  HugeDecimalDiv10(A);
  Assert(HugeDecimalToStr(A) = '102');
  HugeDecimalDiv10(A);
  Assert(HugeDecimalToStr(A) = '10.2');
  HugeDecimalDiv10(A);
  Assert(HugeDecimalToStr(A) = '1.02');
  HugeDecimalDiv10(A);
  Assert(HugeDecimalToStr(A) = '0.102');
  HugeDecimalDiv10(A);
  Assert(HugeDecimalToStr(A) = '0.0102');
  HugeDecimalDiv10(A);
  Assert(HugeDecimalToStr(A) = '0.00102');

  // Compare
  HugeDecimalAssignDecimal32(A, StrToDecimal32('12.34'));
  HugeDecimalAssignDecimal32(B, StrToDecimal32('12.341'));
  Assert(not HugeDecimalEqualsHugeDecimal(A, B));
  Assert(HugeDecimalCompareHugeDecimal(A, B) = -1);
  Assert(HugeDecimalCompareHugeDecimal(B, A) = 1);

  HugeDecimalAssignDecimal32(A, StrToDecimal32('0.0012'));
  HugeDecimalAssignDecimal32(B, StrToDecimal32('0.001'));
  Assert(not HugeDecimalEqualsHugeDecimal(A, B));
  Assert(HugeDecimalCompareHugeDecimal(A, B) = 1);
  Assert(HugeDecimalCompareHugeDecimal(B, A) = -1);

  HugeDecimalAssignDecimal32(A, StrToDecimal32('123.0054'));
  HugeDecimalAssignDecimal32(B, StrToDecimal32('122.0054'));
  Assert(not HugeDecimalEqualsHugeDecimal(A, B));
  Assert(HugeDecimalCompareHugeDecimal(A, B) = 1);
  Assert(HugeDecimalCompareHugeDecimal(B, A) = -1);

  HugeDecimalAssignDecimal32(A, StrToDecimal32('1121.0052'));
  HugeDecimalAssignDecimal32(B, StrToDecimal32('122.0052'));
  Assert(not HugeDecimalEqualsHugeDecimal(A, B));
  Assert(HugeDecimalCompareHugeDecimal(A, B) = 1);
  Assert(HugeDecimalCompareHugeDecimal(B, A) = -1);

  HugeDecimalAssignDecimal32(A, StrToDecimal32('1423.0034'));
  HugeDecimalAssignDecimal32(B, StrToDecimal32('1423.0034'));
  Assert(HugeDecimalEqualsHugeDecimal(A, B));
  Assert(HugeDecimalCompareHugeDecimal(A, B) = 0);
  Assert(HugeDecimalCompareHugeDecimal(B, A) = 0);

  // Inc/Dec
  HugeDecimalAssignDecimal32(A, StrToDecimal32('0'));
  HugeDecimalInc(A);
  Assert(HugeDecimalToStr(A) = '1');
  HugeDecimalInc(A);
  Assert(HugeDecimalToStr(A) = '2');
  HugeDecimalDec(A);
  Assert(HugeDecimalToStr(A) = '1');
  HugeDecimalDec(A);
  Assert(HugeDecimalToStr(A) = '0');

  HugeDecimalAssignDecimal32(A, StrToDecimal32('9'));
  HugeDecimalInc(A);
  Assert(HugeDecimalToStr(A) = '10');
  HugeDecimalInc(A);
  Assert(HugeDecimalToStr(A) = '11');
  HugeDecimalDec(A);
  Assert(HugeDecimalToStr(A) = '10');
  HugeDecimalDec(A);
  Assert(HugeDecimalToStr(A) = '9');

  HugeDecimalAssignDecimal32(A, StrToDecimal32('0.01'));
  HugeDecimalInc(A);
  Assert(HugeDecimalToStr(A) = '1.01');
  HugeDecimalInc(A);
  Assert(HugeDecimalToStr(A) = '2.01');
  HugeDecimalDec(A);
  Assert(HugeDecimalToStr(A) = '1.01');
  HugeDecimalDec(A);
  Assert(HugeDecimalToStr(A) = '0.01');

  HugeDecimalAssignDecimal32(A, StrToDecimal32('9.01'));
  HugeDecimalInc(A);
  Assert(HugeDecimalToStr(A) = '10.01');
  HugeDecimalInc(A);
  Assert(HugeDecimalToStr(A) = '11.01');
  HugeDecimalDec(A);
  Assert(HugeDecimalToStr(A) = '10.01');
  HugeDecimalDec(A);
  Assert(HugeDecimalToStr(A) = '9.01');

  HugeDecimalAssignDecimal32(A, StrToDecimal32('0'));
  HugeDecimalInc(A, 2);
  Assert(HugeDecimalToStr(A) = '2');
  HugeDecimalInc(A, 9);
  Assert(HugeDecimalToStr(A) = '11');
  HugeDecimalDec(A, 9);
  Assert(HugeDecimalToStr(A) = '2');
  HugeDecimalDec(A, 2);
  Assert(HugeDecimalToStr(A) = '0');

  // Round
  HugeDecimalAssignDecimal32(A, StrToDecimal32('0.0'));
  HugeDecimalRound(A);
  Assert(HugeDecimalToStr(A) = '0');

  HugeDecimalAssignDecimal32(A, StrToDecimal32('0.49'));
  HugeDecimalRound(A);
  Assert(HugeDecimalToStr(A) = '0');

  HugeDecimalAssignDecimal32(A, StrToDecimal32('0.5'));
  HugeDecimalRound(A);
  Assert(HugeDecimalToStr(A) = '0');

  HugeDecimalAssignDecimal32(A, StrToDecimal32('0.51'));
  HugeDecimalRound(A);
  Assert(HugeDecimalToStr(A) = '1');

  HugeDecimalAssignDecimal32(A, StrToDecimal32('1'));
  HugeDecimalRound(A);
  Assert(HugeDecimalToStr(A) = '1');

  HugeDecimalAssignDecimal32(A, StrToDecimal32('1.4999'));
  Assert(HugeDecimalToStr(A) = '1.4999');
  HugeDecimalRound(A);
  Assert(HugeDecimalToStr(A) = '1');

  HugeDecimalAssignDecimal32(A, StrToDecimal32('1.5'));
  HugeDecimalRound(A);
  Assert(HugeDecimalToStr(A) = '2');

  // StrToHugeDecimal
  Assert(TryStrToHugeDecimal('', A) = dceConvertError);
  Assert(TryStrToHugeDecimal(' ', A) = dceConvertError);
  Assert(TryStrToHugeDecimal('x', A) = dceConvertError);
  Assert(TryStrToHugeDecimal('.', A) = dceConvertError);
  Assert(TryStrToHugeDecimal('0.', A) = dceConvertError);
  Assert(TryStrToHugeDecimal('.0', A) = dceConvertError);

  Assert(TryStrToHugeDecimal('0', A) = dceNoError);
  Assert(HugeDecimalToStr(A) = '0');
  Assert(HugeDecimalIsZero(A));

  Assert(TryStrToHugeDecimal('000', A) = dceNoError);
  Assert(HugeDecimalToStr(A) = '0');
  Assert(HugeDecimalIsZero(A));

  Assert(TryStrToHugeDecimal('1', A) = dceNoError);
  Assert(HugeDecimalToStr(A) = '1');
  Assert(HugeDecimalIsOne(A));

  Assert(TryStrToHugeDecimal('001', A) = dceNoError);
  Assert(HugeDecimalToStr(A) = '1');
  Assert(HugeDecimalIsOne(A));

  Assert(TryStrToHugeDecimal('10', A) = dceNoError);
  Assert(HugeDecimalToStr(A) = '10');

  Assert(TryStrToHugeDecimal('123', A) = dceNoError);
  Assert(HugeDecimalToStr(A) = '123');
  Assert(HugeDecimalToWord8(A) = 123);

  Assert(TryStrToHugeDecimal('0.1', A) = dceNoError);
  Assert(HugeDecimalToStr(A) = '0.1');

  Assert(TryStrToHugeDecimal('00.1', A) = dceNoError);
  Assert(HugeDecimalToStr(A) = '0.1');

  Assert(TryStrToHugeDecimal('00.10', A) = dceNoError);
  Assert(HugeDecimalToStr(A) = '0.1');

  Assert(TryStrToHugeDecimal('1.2', A) = dceNoError);
  Assert(HugeDecimalToStr(A) = '1.2');

  Assert(TryStrToHugeDecimal('001.200', A) = dceNoError);
  Assert(HugeDecimalToStr(A) = '1.2');

  Assert(TryStrToHugeDecimal('3004.2001', A) = dceNoError);
  Assert(HugeDecimalToStr(A) = '3004.2001');

  Assert(TryStrToHugeDecimal('123456.7890123', A) = dceNoError);
  Assert(HugeDecimalToStr(A) = '123456.7890123');

  StrToHugeDecimal('010123456.7890123990', A);
  Assert(HugeDecimalToStr(A) = '10123456.789012399');

  // Add
  HugeDecimalAssignDecimal32(A, StrToDecimal32('1.1'));
  HugeDecimalAssignDecimal32(B, StrToDecimal32('0'));
  HugeDecimalAddHugeDecimal(A, B);
  Assert(HugeDecimalToStr(A) = '1.1');

  HugeDecimalAssignDecimal32(A, StrToDecimal32('0'));
  HugeDecimalAssignDecimal32(B, StrToDecimal32('1.1'));
  HugeDecimalAddHugeDecimal(A, B);
  Assert(HugeDecimalToStr(A) = '1.1');

  HugeDecimalAssignDecimal32(A, StrToDecimal32('0'));
  HugeDecimalAssignDecimal32(B, StrToDecimal32('0'));
  HugeDecimalAddHugeDecimal(A, B);
  Assert(HugeDecimalToStr(A) = '0');

  HugeDecimalAssignDecimal32(A, StrToDecimal32('1.2'));
  HugeDecimalAssignDecimal32(B, StrToDecimal32('3.4'));
  HugeDecimalAddHugeDecimal(A, B);
  Assert(HugeDecimalToStr(A) = '4.6');

  HugeDecimalAssignDecimal32(A, StrToDecimal32('1.23'));
  HugeDecimalAssignDecimal32(B, StrToDecimal32('54.1'));
  HugeDecimalAddHugeDecimal(A, B);
  Assert(HugeDecimalToStr(A) = '55.33');

  HugeDecimalAssignDecimal32(A, StrToDecimal32('0.001'));
  HugeDecimalAssignDecimal32(B, StrToDecimal32('123'));
  HugeDecimalAddHugeDecimal(A, B);
  Assert(HugeDecimalToStr(A) = '123.001');

  HugeDecimalAssignDecimal32(A, StrToDecimal32('123.99'));
  HugeDecimalAssignDecimal32(B, StrToDecimal32('0.01'));
  HugeDecimalAddHugeDecimal(A, B);
  Assert(HugeDecimalToStr(A) = '124');

  HugeDecimalAssignDecimal32(A, StrToDecimal32('98.99'));
  HugeDecimalAssignDecimal32(B, StrToDecimal32('1.01'));
  HugeDecimalAddHugeDecimal(A, B);
  Assert(HugeDecimalToStr(A) = '100');

  HugeDecimalAssignDecimal32(A, StrToDecimal32('19'));
  HugeDecimalAssignDecimal32(B, StrToDecimal32('0.001'));
  HugeDecimalAddHugeDecimal(A, B);
  Assert(HugeDecimalToStr(A) = '19.001');

  // Subtract
  HugeDecimalAssignDecimal32(A, StrToDecimal32('1.1'));
  HugeDecimalAssignDecimal32(B, StrToDecimal32('0'));
  HugeDecimalSubtractHugeDecimal(A, B);
  Assert(HugeDecimalToStr(A) = '1.1');

  HugeDecimalAssignDecimal32(A, StrToDecimal32('1.1'));
  HugeDecimalAssignDecimal32(B, StrToDecimal32('1.1'));
  HugeDecimalSubtractHugeDecimal(A, B);
  Assert(HugeDecimalToStr(A) = '0');
  Assert(HugeDecimalIsZero(A));

  HugeDecimalAssignDecimal32(A, StrToDecimal32('1.1'));
  HugeDecimalAssignDecimal32(B, StrToDecimal32('1'));
  HugeDecimalSubtractHugeDecimal(A, B);
  Assert(HugeDecimalToStr(A) = '0.1');

  HugeDecimalAssignDecimal32(A, StrToDecimal32('1.1'));
  HugeDecimalAssignDecimal32(B, StrToDecimal32('0.1'));
  HugeDecimalSubtractHugeDecimal(A, B);
  Assert(HugeDecimalToStr(A) = '1');

  HugeDecimalAssignDecimal32(A, StrToDecimal32('21'));
  HugeDecimalAssignDecimal32(B, StrToDecimal32('1'));
  HugeDecimalSubtractHugeDecimal(A, B);
  Assert(HugeDecimalToStr(A) = '20');

  HugeDecimalAssignDecimal32(A, StrToDecimal32('12'));
  HugeDecimalAssignDecimal32(B, StrToDecimal32('3'));
  HugeDecimalSubtractHugeDecimal(A, B);
  Assert(HugeDecimalToStr(A) = '9');

  HugeDecimalAssignDecimal32(A, StrToDecimal32('123.456'));
  HugeDecimalAssignDecimal32(B, StrToDecimal32('1.2'));
  HugeDecimalSubtractHugeDecimal(A, B);
  Assert(HugeDecimalToStr(A) = '122.256');

  HugeDecimalAssignDecimal32(A, StrToDecimal32('123.456'));
  HugeDecimalAssignDecimal32(B, StrToDecimal32('1.4567'));
  HugeDecimalSubtractHugeDecimal(A, B);
  Assert(HugeDecimalToStr(A) = '121.9993');

  HugeDecimalAssignDecimal64(A, StrToDecimal64('1.00200101'));
  HugeDecimalAssignDecimal64(B, StrToDecimal64('1.00100202'));
  HugeDecimalSubtractHugeDecimal(A, B);
  Assert(HugeDecimalToStr(A) = '0.00099899');

  HugeDecimalAssignDecimal64(A, StrToDecimal64('2.12345678'));
  HugeDecimalAssignDecimal64(B, StrToDecimal64('1.23456789'));
  HugeDecimalSubtractHugeDecimal(A, B);
  Assert(HugeDecimalToStr(A) = '0.88888889');

  HugeDecimalAssignDecimal64(A, StrToDecimal64('111.12345678'));
  HugeDecimalAssignDecimal64(B, StrToDecimal64('0.123'));
  HugeDecimalSubtractHugeDecimal(A, B);
  Assert(HugeDecimalToStr(A) = '111.00045678');

  HugeDecimalAssignDecimal64(A, StrToDecimal64('123.4'));
  HugeDecimalAssignDecimal64(B, StrToDecimal64('0.12345678'));
  HugeDecimalSubtractHugeDecimal(A, B);
  Assert(HugeDecimalToStr(A) = '123.27654322');

  HugeDecimalAssignDecimal64(A, StrToDecimal64('1'));
  HugeDecimalAssignDecimal64(B, StrToDecimal64('0.12345678'));
  HugeDecimalSubtractHugeDecimal(A, B);
  Assert(HugeDecimalToStr(A) = '0.87654322');

  HugeDecimalAssignDecimal64(A, StrToDecimal64('1.12345678'));
  HugeDecimalAssignDecimal64(B, StrToDecimal64('0.12345678'));
  HugeDecimalSubtractHugeDecimal(A, B);
  Assert(HugeDecimalToStr(A) = '1');
  Assert(HugeDecimalIsOne(A));

  // Finalise
  HugeDecimalFinalise(B);
  HugeDecimalFinalise(A);
end;

procedure Test_SDecimal32;
var A, B : SDecimal32;
begin
  SDecimal32InitZero(A);
  Assert(SDecimal32IsZero(A));
  Assert(not SDecimal32IsOne(A));
  Assert(not SDecimal32IsMaximum(A));
  Assert(not SDecimal32IsMinimum(A));
  Assert(not SDecimal32IsOverflow(A));
  Assert(SDecimal32ToInt32(A) = 0);
  Assert(SDecimal32ToStr(A) = '0.0000');
  Assert(SDecimal32EqualsInt32(A, 0));
  Assert(SDecimal32CompareWord8(A, 0) = 0);
  Assert(SDecimal32CompareWord8(A, 1) = -1);
  Assert(SDecimal32CompareInt32(A, 0) = 0);
  Assert(SDecimal32CompareInt32(A, -1) = 1);
  Assert(SDecimal32CompareInt32(A, 1) = -1);
  Assert(SDecimal32Trunc(A) = 0);
  Assert(SDecimal32Round(A) = 0);
  Assert(SDecimal32FracWord(A) = 0);
  Assert(SDecimal32EqualsFloat(A, 0.0));
  Assert(not SDecimal32EqualsFloat(A, 0.000051));
  Assert(SDecimal32CompareFloat(A, 0.0000) = 0);
  Assert(SDecimal32CompareFloat(A, -0.0001) = 1);
  Assert(SDecimal32CompareFloat(A, 0.0001) = -1);
  Assert(SDecimal32Sign(A) = 0);

  SDecimal32InitOne(A);
  Assert(not SDecimal32IsZero(A));
  Assert(SDecimal32IsOne(A));
  Assert(not SDecimal32IsMinusOne(A));
  Assert(SDecimal32ToWord8(A) = 1);
  Assert(SDecimal32ToStr(A) = '1.0000');
  Assert(SDecimal32ToStrB(A) = '1.0000');
  Assert(SDecimal32EqualsWord8(A, 1));
  Assert(SDecimal32EqualsInt32(A, 1));
  Assert(not SDecimal32EqualsInt32(A, -1));
  Assert(SDecimal32CompareWord8(A, 0) = 1);
  Assert(SDecimal32CompareWord8(A, 1) = 0);
  Assert(SDecimal32CompareWord8(A, 2) = -1);
  Assert(SDecimal32CompareInt32(A, -1) = 1);
  Assert(SDecimal32CompareInt32(A, 1) = 0);
  Assert(SDecimal32CompareInt32(A, 2) = -1);
  Assert(SDecimal32EqualsFloat(A, 1.0));
  Assert(SDecimal32EqualsFloat(A, 1.000049));
  Assert(SDecimal32Sign(A) = 1);

  SDecimal32InitMinusOne(A);
  Assert(not SDecimal32IsZero(A));
  Assert(not SDecimal32IsOne(A));
  Assert(SDecimal32IsMinusOne(A));
  Assert(SDecimal32ToInt32(A) = -1);
  Assert(SDecimal32ToStr(A) = '-1.0000');
  Assert(SDecimal32ToStrB(A) = '-1.0000');
  Assert(not SDecimal32EqualsWord8(A, 1));
  Assert(SDecimal32EqualsInt32(A, -1));
  Assert(not SDecimal32EqualsInt32(A, 1));
  Assert(SDecimal32CompareWord8(A, 0) = -1);
  Assert(SDecimal32CompareWord8(A, 1) = -1);
  Assert(SDecimal32CompareInt32(A, 0) = -1);
  Assert(SDecimal32CompareInt32(A, 1) = -1);
  Assert(SDecimal32CompareInt32(A, -1) = 0);
  Assert(SDecimal32CompareInt32(A, -2) = 1);
  Assert(SDecimal32EqualsFloat(A, -1.0));
  Assert(SDecimal32EqualsFloat(A, -1.000049));
  Assert(SDecimal32Sign(A) = -1);

  Assert(FloatIsSDecimal32Range(0.0000));
  Assert(FloatIsSDecimal32Range(-0.0001));
  Assert(FloatIsSDecimal32Range(-0.00005));
  Assert(FloatIsSDecimal32Range(+99999.9999));
  Assert(FloatIsSDecimal32Range(-99999.9999));
  Assert(FloatIsSDecimal32Range(+99999.999949));
  Assert(FloatIsSDecimal32Range(-99999.999949));
  Assert(not FloatIsSDecimal32Range(+99999.99995));
  Assert(not FloatIsSDecimal32Range(-99999.99995));
  Assert(not FloatIsSDecimal32Range(+100000.0000));
  Assert(not FloatIsSDecimal32Range(-100000.0000));

  SDecimal32InitWord32(A, 99999);
  Assert(SDecimal32ToWord32(A) = 99999);
  Assert(SDecimal32ToStr(A) = '99999.0000');
  Assert(SDecimal32Trunc(A) = 99999);
  Assert(SDecimal32Sign(A) = 1);

  SDecimal32InitInt32(A, -99999);
  Assert(SDecimal32ToInt32(A) = -99999);
  Assert(SDecimal32ToStr(A) = '-99999.0000');
  Assert(SDecimal32Trunc(A) = -99999);
  Assert(SDecimal32Sign(A) = -1);

  SDecimal32InitFloat(A, -1.2345);
  Assert(Abs(SDecimal32ToFloat(A) - -1.2345) < 1e-9);
  Assert(SDecimal32ToStr(A) = '-1.2345');
  Assert(SDecimal32Trunc(A) = -1);
  Assert(SDecimal32Round(A) = -1);
  Assert(SDecimal32EqualsFloat(A, -1.2345));

  SDecimal32InitFloat(A, -1.23445);
  Assert(Abs(SDecimal32ToFloat(A) - -1.2344) < 1e-9);
  Assert(SDecimal32ToStr(A) = '-1.2344');

  SDecimal32InitFloat(A, -1.23455);
  Assert(Abs(SDecimal32ToFloat(A) - -1.2346) < 1e-9);
  Assert(SDecimal32ToStr(A) = '-1.2346');

  SDecimal32InitFloat(A, 99999.9999);
  Assert(SDecimal32IsMaximum(A));
  Assert(not SDecimal32IsMinimum(A));

  SDecimal32InitFloat(A, -99999.9999);
  Assert(not SDecimal32IsMaximum(A));
  Assert(SDecimal32IsMinimum(A));

  SDecimal32InitMax(A);
  Assert(SDecimal32IsMaximum(A));
  Assert(not SDecimal32IsOverflow(A));
  Assert(SDecimal32ToStr(A) = '99999.9999');
  Assert(SDecimal32Trunc(A) = 99999);
  Assert(SDecimal32Round(A) = 100000);
  Assert(SDecimal32FracWord(A) = 9999);
  Assert(SDecimal32EqualsFloat(A, 99999.9999));
  Assert(SDecimal32EqualsFloat(A, 99999.999949));
  Assert(not SDecimal32EqualsFloat(A, 99999.99995));

  SDecimal32InitMin(A);
  Assert(SDecimal32IsMinimum(A));
  Assert(not SDecimal32IsOverflow(A));
  Assert(SDecimal32ToStr(A) = '-99999.9999');
  Assert(SDecimal32Trunc(A) = -99999);
  Assert(SDecimal32Round(A) = -100000);
  Assert(SDecimal32FracWord(A) = 9999);
  Assert(SDecimal32EqualsFloat(A, -99999.9999));
  Assert(SDecimal32EqualsFloat(A, -99999.999949));
  Assert(not SDecimal32EqualsFloat(A, -99999.99995));

  SDecimal32InitFloat(A, -99999.4999);
  Assert(SDecimal32Trunc(A) = -99999);
  Assert(SDecimal32Round(A) = -99999);

  SDecimal32InitFloat(A, -99999.5000);
  Assert(SDecimal32Trunc(A) = -99999);
  Assert(SDecimal32Round(A) = -100000);
  Assert(SDecimal32FracWord(A) = 5000);

  SDecimal32InitFloat(A, -99998.5000);
  Assert(SDecimal32Trunc(A) = -99998);
  Assert(SDecimal32Round(A) = -99998);

  SDecimal32InitFloat(A, -99998.5001);
  Assert(SDecimal32Trunc(A) = -99998);
  Assert(SDecimal32Round(A) = -99999);

  SDecimal32InitFloat(A, -0.1);
  Assert(SDecimal32ToStr(A) = '-0.1000');
  Assert(SDecimal32EqualsFloat(A, -0.1));

  SDecimal32InitFloat(A, -0.00005);
  Assert(SDecimal32ToStr(A) = '0.0000');
  SDecimal32InitFloat(A, -0.000051);
  Assert(SDecimal32ToStr(A) = '-0.0001');
  SDecimal32InitFloat(A, -0.00025);
  Assert(SDecimal32ToStr(A) = '-0.0002');
  SDecimal32InitFloat(A, -0.00055);
  Assert(SDecimal32ToStr(A) = '-0.0006');
  SDecimal32InitFloat(A, -0.000049);
  Assert(SDecimal32ToStr(A) = '0.0000');
  SDecimal32InitFloat(A, -0.000050);
  Assert(SDecimal32ToStr(A) = '0.0000');

  SDecimal32InitFloat(A, -1.9375);
  Assert(SDecimal32ToFloat(A) = -1.9375);
  SDecimal32AddWord8(A, 1);
  Assert(SDecimal32ToFloat(A) = -0.9375);
  SDecimal32AddWord8(A, 1);
  Assert(SDecimal32ToFloat(A) = 0.0625);
  SDecimal32AddWord8(A, 1);
  Assert(SDecimal32ToFloat(A) = 1.0625);
  SDecimal32SubtractWord8(A, 1);
  Assert(SDecimal32ToFloat(A) = 0.0625);
  SDecimal32SubtractWord8(A, 1);
  Assert(SDecimal32ToFloat(A) = -0.9375);
  SDecimal32SubtractWord8(A, 1);
  Assert(SDecimal32ToFloat(A) = -1.9375);

  SDecimal32InitFloat(A, -1.9375);
  SDecimal32InitFloat(B, 2.5000);
  SDecimal32AddSDecimal32(A, B);
  Assert(SDecimal32ToFloat(A) = 0.5625);
  SDecimal32AddSDecimal32(A, B);
  Assert(SDecimal32ToFloat(A) = 3.0625);
  SDecimal32SubtractSDecimal32(A, B);
  Assert(SDecimal32ToFloat(A) = 0.5625);
  SDecimal32SubtractSDecimal32(A, B);
  Assert(SDecimal32ToFloat(A) = -1.9375);
  SDecimal32SubtractSDecimal32(A, B);
  Assert(SDecimal32ToFloat(A) = -4.4375);

  SDecimal32InitFloat(A, -1.9375);
  SDecimal32InitFloat(B, -2.5000);
  SDecimal32AddSDecimal32(A, B);
  Assert(SDecimal32ToFloat(A) = -4.4375);
  SDecimal32SubtractSDecimal32(A, B);
  Assert(SDecimal32ToFloat(A) = -1.9375);

  SDecimal32InitFloat(A, 1.9375);
  SDecimal32InitFloat(B, 2.5000);
  Assert(not SDecimal32EqualsSDecimal32(A, B));
  Assert(SDecimal32EqualsSDecimal32(A, A));
  Assert(SDecimal32CompareSDecimal32(A, B) = -1);
  Assert(SDecimal32CompareSDecimal32(A, A) = 0);
  Assert(SDecimal32CompareSDecimal32(B, A) = 1);

  SDecimal32InitFloat(A, -1.9375);
  SDecimal32InitFloat(B, 2.5000);
  Assert(not SDecimal32EqualsSDecimal32(A, B));
  Assert(SDecimal32EqualsSDecimal32(A, A));
  Assert(SDecimal32CompareSDecimal32(A, B) = -1);
  Assert(SDecimal32CompareSDecimal32(A, A) = 0);
  Assert(SDecimal32CompareSDecimal32(B, A) = 1);

  SDecimal32InitFloat(A, -1.9375);
  SDecimal32InitFloat(B, -2.5000);
  Assert(not SDecimal32EqualsSDecimal32(A, B));
  Assert(SDecimal32EqualsSDecimal32(A, A));
  Assert(SDecimal32CompareSDecimal32(A, B) = 1);
  Assert(SDecimal32CompareSDecimal32(A, A) = 0);
  Assert(SDecimal32CompareSDecimal32(B, A) = -1);

  SDecimal32InitFloat(A, -1.9375);
  SDecimal32MultiplyWord8(A, 3);
  Assert(SDecimal32ToFloat(A) = -5.8125);

  SDecimal32InitFloat(A, 1.9375);
  SDecimal32InitFloat(B, -3.1200);
  SDecimal32MultiplySDecimal32(A, B);
  Assert(SDecimal32ToFloat(A) + 6.045 < 1e-10);
  Assert(SDecimal32ToStr(A) = '-6.0450');
  SDecimal32MultiplySDecimal32(A, B);
  Assert(SDecimal32ToStr(A) = '18.8604');
  SDecimal32DivideSDecimal32(A, B);
  Assert(SDecimal32ToStr(A) = '-6.0450');
  SDecimal32DivideSDecimal32(A, B);
  Assert(SDecimal32ToStr(A) = '1.9375');

  SDecimal32InitFloat(A, -16.9375);
  SDecimal32InitFloat(B, -3.1200);
  SDecimal32MultiplySDecimal32(A, B);
  Assert(SDecimal32ToFloat(A) - 52.8450 < 1e-10);
  Assert(SDecimal32ToStr(A) = '52.8450');
  SDecimal32DivideSDecimal32(A, B);
  Assert(SDecimal32ToStr(A) = '-16.9375');
  SDecimal32DivideSDecimal32(A, B);
  Assert(SDecimal32ToStr(A) = '5.4286');

  A := StrToSDecimal32('0');
  Assert(SDecimal32ToWord8(A) = 0);
  A := StrToSDecimal32('+0');
  Assert(SDecimal32ToWord8(A) = 0);
  A := StrToSDecimal32('-0');
  Assert(SDecimal32ToWord8(A) = 0);
  A := StrToSDecimal32('+1');
  Assert(SDecimal32ToWord8(A) = 1);
  A := StrToSDecimal32('-1.0');
  Assert(SDecimal32ToInt32(A) = -1);
  A := StrToSDecimal32('-1.0000000000000001');
  Assert(SDecimal32ToInt32(A) = -1);
  A := StrToSDecimal32('-123.9385');
  Assert(SDecimal32ToStr(A) = '-123.9385');
  A := StrToSDecimal32('-123.93856');
  Assert(SDecimal32ToStr(A) = '-123.9386');
  A := StrToSDecimal32('-123.93855');
  Assert(SDecimal32ToStr(A) = '-123.9386');
  A := StrToSDecimal32('-123.93865');
  Assert(SDecimal32ToStr(A) = '-123.9386');
  A := StrToSDecimal32('-123.938650000000010');
  Assert(SDecimal32ToStr(A) = '-123.9387');
  A := StrToSDecimal32('-99999.9999');
  Assert(SDecimal32ToStr(A) = '-99999.9999');

  Assert(TryStrToSDecimal32('0', A) = dceNoError);
  Assert(SDecimal32ToInt32(A) = 0);
  Assert(TryStrToSDecimal32('-99999', A) = dceNoError);
  Assert(SDecimal32ToInt32(A) = -99999);

  Assert(TryStrToSDecimal32('-100000', A) = dceOverflowError);
  Assert(TryStrToSDecimal32('-99999.99995', A) = dceOverflowError);
end;

procedure Test_SDecimal64;
var A, B : SDecimal64;
begin
  SDecimal64InitZero(A);
  Assert(SDecimal64IsZero(A));
  Assert(not SDecimal64IsOne(A));
  Assert(not SDecimal64IsMaximum(A));
  Assert(not SDecimal64IsOverflow(A));
  Assert(SDecimal64ToWord8(A) = 0);
  Assert(SDecimal64ToStr(A) = '0.000000000');
  Assert(SDecimal64EqualsWord8(A, 0));
  Assert(SDecimal64CompareWord8(A, 0) = 0);
  Assert(SDecimal64CompareWord8(A, 1) = -1);
  Assert(SDecimal64Trunc(A) = 0);
  Assert(SDecimal64Round(A) = 0);
  Assert(SDecimal64FracWord(A) = 0);
  Assert(SDecimal64EqualsFloat(A, 0.0));
  Assert(SDecimal64EqualsFloat(A, 0.00000000049));
  Assert(SDecimal64EqualsFloat(A, 0.0000000005));
  Assert(not SDecimal64EqualsFloat(A, 0.00000000051));
  Assert(SDecimal64CompareFloat(A, 0.000000000) = 0);
  Assert(SDecimal64CompareFloat(A, -0.000000001) = 1);
  Assert(SDecimal64CompareFloat(A, 0.000000001) = -1);
  Assert(SDecimal64Sign(A) = 0);

  SDecimal64InitOne(A);
  Assert(not SDecimal64IsZero(A));
  Assert(SDecimal64IsOne(A));
  Assert(not SDecimal64IsMinusOne(A));
  Assert(SDecimal64ToInt32(A) = 1);
  Assert(SDecimal64ToStr(A) = '1.000000000');
  Assert(SDecimal64ToStrB(A) = '1.000000000');
  Assert(SDecimal64EqualsWord8(A, 1));
  Assert(SDecimal64CompareWord8(A, 0) = 1);
  Assert(SDecimal64CompareWord8(A, 1) = 0);
  Assert(SDecimal64CompareWord8(A, 2) = -1);
  Assert(SDecimal64Trunc(A) = 1);
  Assert(SDecimal64Round(A) = 1);
  Assert(SDecimal64EqualsFloat(A, 1.0));
  Assert(SDecimal64EqualsFloat(A, 1.00000000049));
  Assert(not SDecimal64EqualsFloat(A, 1.00000000051));
  Assert(SDecimal64Sign(A) = 1);

  SDecimal64InitMinusOne(A);
  Assert(not SDecimal64IsZero(A));
  Assert(not SDecimal64IsOne(A));
  Assert(SDecimal64IsMinusOne(A));
  Assert(SDecimal64ToInt32(A) = -1);
  Assert(SDecimal64ToStr(A) = '-1.000000000');
  Assert(SDecimal64ToStrB(A) = '-1.000000000');
  Assert(not SDecimal64EqualsWord8(A, 1));
  Assert(SDecimal64EqualsInt32(A, -1));
  Assert(SDecimal64CompareWord8(A, 0) = -1);
  Assert(SDecimal64CompareInt32(A, -2) = 1);
  Assert(SDecimal64CompareInt32(A, -1) = 0);
  Assert(SDecimal64CompareInt32(A, 0) = -1);
  Assert(SDecimal64Trunc(A) = -1);
  Assert(SDecimal64Round(A) = -1);
  Assert(SDecimal64EqualsFloat(A, -1.0));
  Assert(SDecimal64EqualsFloat(A, -1.00000000049));
  Assert(not SDecimal64EqualsFloat(A, -1.00000000051));
  Assert(SDecimal64Sign(A) = -1);

  Assert(FloatIsSDecimal64Range(0.000000000));
  Assert(FloatIsSDecimal64Range(-0.000000001));
  Assert(FloatIsSDecimal64Range(-0.0000000005));
  {$IFNDEF DELPHI7_DOWN}
  Assert(not FloatIsSDecimal64Range(9999999999.999999999));
  Assert(not FloatIsSDecimal64Range(10000000000.000000000));
  {$ENDIF}

  SDecimal64InitInt64(A, SDecimal64MaxInt);
  Assert(SDecimal64ToInt64(A) = 9999999999);
  Assert(SDecimal64ToStr(A) = '9999999999.000000000');
  Assert(SDecimal64Trunc(A) = 9999999999);
  Assert(SDecimal64Sign(A) = 1);

  SDecimal64InitInt64(A, SDecimal64MinInt);
  Assert(SDecimal64ToInt64(A) = -9999999999);
  Assert(SDecimal64ToStr(A) = '-9999999999.000000000');
  Assert(SDecimal64Trunc(A) = -9999999999);
  Assert(SDecimal64Sign(A) = -1);

  SDecimal64InitFloat(A, -1.234567890);
  Assert(Abs(SDecimal64ToFloat(A) - -1.234567890) < 1e-12);
  Assert(SDecimal64ToStr(A) = '-1.234567890');
  Assert(SDecimal64Trunc(A) = -1);
  Assert(SDecimal64Round(A) = -1);
  Assert(SDecimal64EqualsFloat(A, -1.234567890));

  SDecimal64InitFloat(A, -1.2345111115);
  Assert(Abs(SDecimal64ToFloat(A) - -1.234511112) < 1e-12);
  Assert(SDecimal64ToStr(A) = '-1.234511112');

  SDecimal64InitFloat(A, -1.2345111125);
  Assert(Abs(SDecimal64ToFloat(A) - -1.234511112) < 1e-12);
  Assert(SDecimal64ToStr(A) = '-1.234511112');

  SDecimal64InitFloat(A, -1.23451111251);
  Assert(Abs(SDecimal64ToFloat(A) - -1.234511113) < 1e-12);
  Assert(SDecimal64ToStr(A) = '-1.234511113');

  A := StrToSDecimal64('9999999999.999999999');
  Assert(SDecimal64IsMaximum(A));

  A := StrToSDecimal64('-9999999999.999999999');
  Assert(SDecimal64IsMinimum(A));

  SDecimal64InitMax(A);
  Assert(SDecimal64IsMaximum(A));
  Assert(not SDecimal64IsOverflow(A));
  Assert(SDecimal64ToStr(A) = '9999999999.999999999');
  Assert(SDecimal64Trunc(A) = 9999999999);
  Assert(SDecimal64Round(A) = 10000000000);
  Assert(SDecimal64FracWord(A) = 999999999);

  SDecimal64InitMin(A);
  Assert(SDecimal64IsMinimum(A));
  Assert(not SDecimal64IsOverflow(A));
  Assert(SDecimal64ToStr(A) = '-9999999999.999999999');
  Assert(SDecimal64Trunc(A) = -9999999999);
  Assert(SDecimal64Round(A) = -10000000000);
  Assert(SDecimal64FracWord(A) = 999999999);

  A := StrToSDecimal64('-9999999999.499999999');
  Assert(SDecimal64Trunc(A) = -9999999999);
  Assert(SDecimal64Round(A) = -9999999999);

  A := StrToSDecimal64('-9999999999.500000000');
  Assert(SDecimal64Trunc(A) = -9999999999);
  Assert(SDecimal64Round(A) = -10000000000);
  Assert(SDecimal64FracWord(A) = 500000000);

  A := StrToSDecimal64('-9999999998.500000000');
  Assert(SDecimal64Trunc(A) = -9999999998);
  Assert(SDecimal64Round(A) = -9999999998);

  A := StrToSDecimal64('-9999999998.500000001');
  Assert(SDecimal64Trunc(A) = -9999999998);
  Assert(SDecimal64Round(A) = -9999999999);

  SDecimal64InitFloat(A, -0.1);
  Assert(SDecimal64ToStr(A) = '-0.100000000');
  Assert(SDecimal64EqualsFloat(A, -0.1));

  SDecimal64InitFloat(A, -0.000000001);
  Assert(SDecimal64ToStr(A) = '-0.000000001');
  Assert(SDecimal64EqualsFloat(A, -0.000000001));

  SDecimal64InitFloat(A, -0.0000000005);
  Assert(SDecimal64ToStr(A) = '0.000000000');
  SDecimal64InitFloat(A, -0.00000000051);
  Assert(SDecimal64ToStr(A) = '-0.000000001');
  SDecimal64InitFloat(A, -0.0000000025);
  Assert(SDecimal64ToStr(A) = '-0.000000002');
  SDecimal64InitFloat(A, -0.0000000055);
  Assert(SDecimal64ToStr(A) = '-0.000000006');
  SDecimal64InitFloat(A, -0.00000000049);
  Assert(SDecimal64ToStr(A) = '0.000000000');
  SDecimal64InitFloat(A, -0.00000000050);
  Assert(SDecimal64ToStr(A) = '0.000000000');

  SDecimal64InitFloat(A, -1.9375);
  Assert(SDecimal64ToFloat(A) = -1.9375);
  SDecimal64AddWord8(A, 1);
  Assert(SDecimal64ToFloat(A) = -0.9375);
  SDecimal64AddWord8(A, 1);
  Assert(SDecimal64ToFloat(A) = 0.0625);
  SDecimal64AddWord8(A, 1);
  Assert(SDecimal64ToFloat(A) = 1.0625);
  SDecimal64SubtractWord8(A, 1);
  Assert(SDecimal64ToFloat(A) = 0.0625);
  SDecimal64SubtractWord8(A, 1);
  Assert(SDecimal64ToFloat(A) = -0.9375);
  SDecimal64SubtractWord8(A, 1);
  Assert(SDecimal64ToFloat(A) = -1.9375);

  SDecimal64InitFloat(A, -1.9375);
  SDecimal64InitFloat(B, -2.5000);
  SDecimal64AddSDecimal64(A, B);
  Assert(SDecimal64ToFloat(A) = -4.4375);
  SDecimal64SubtractSDecimal64(A, B);
  Assert(SDecimal64ToFloat(A) = -1.9375);

  SDecimal64InitFloat(A, -1.9375);
  SDecimal64InitFloat(B, -2.5000);
  Assert(not SDecimal64EqualsSDecimal64(A, B));
  Assert(SDecimal64EqualsSDecimal64(A, A));
  Assert(SDecimal64CompareSDecimal64(A, B) = 1);
  Assert(SDecimal64CompareSDecimal64(A, A) = 0);
  Assert(SDecimal64CompareSDecimal64(B, A) = -1);

  SDecimal64InitFloat(A, -1.9375);
  SDecimal64MultiplyWord8(A, 3);
  Assert(SDecimal64ToFloat(A) = -5.8125);
  SDecimal64DivideWord8(A, 3);
  Assert(SDecimal64ToStr(A) = '-1.937500000');

  SDecimal64InitFloat(A, 1.9375);
  SDecimal64InitFloat(B, -2.5000);
  SDecimal64MultiplySDecimal64(A, B);
  Assert(SDecimal64ToFloat(A) = -4.84375);
  Assert(SDecimal64ToStr(A) = '-4.843750000');
  SDecimal64DivideSDecimal64(A, B);
  Assert(SDecimal64ToStr(A) = '1.937500000');

  SDecimal64InitFloat(A, -12.937512);
  SDecimal64InitFloat(B, -3.123433);
  SDecimal64MultiplySDecimal64(A, B);
  Assert(SDecimal64ToFloat(A) - 40.409451919 < 1e-10);
  Assert(SDecimal64ToStr(A) = '40.409451919');

  A := StrToSDecimal64('0');
  Assert(SDecimal64ToWord8(A) = 0);
  A := StrToSDecimal64('-0');
  Assert(SDecimal64ToWord8(A) = 0);
  A := StrToSDecimal64('+0');
  Assert(SDecimal64ToWord8(A) = 0);
  A := StrToSDecimal64('-0.0000');
  A := StrToSDecimal64('000000000000.000000000000');
  Assert(SDecimal64ToWord8(A) = 0);
  A := StrToSDecimal64('-1');
  Assert(SDecimal64ToInt32(A) = -1);
  A := StrToSDecimal64('-1.0');
  Assert(SDecimal64ToInt32(A) = -1);
  A := StrToSDecimal64('-1.0000000000000001');
  Assert(SDecimal64ToInt32(A) = -1);
  A := StrToSDecimal64('-123.938500000');
  Assert(SDecimal64ToStr(A) = '-123.938500000');
  A := StrToSDecimal64('-123.9385000005');
  Assert(SDecimal64ToStr(A) = '-123.938500000');
  A := StrToSDecimal64('-123.9385000015');
  Assert(SDecimal64ToStr(A) = '-123.938500002');
  A := StrToSDecimal64('-123.9386000025');
  Assert(SDecimal64ToStr(A) = '-123.938600002');
  A := StrToSDecimal64('-123.93860000251');
  Assert(SDecimal64ToStr(A) = '-123.938600003');
  A := StrToSDecimal64('-9999999999.999999999');
  Assert(SDecimal64ToStr(A) = '-9999999999.999999999');

  Assert(TryStrToSDecimal64('0', A) = dceNoError);
  Assert(SDecimal64ToWord32(A) = 0);
  Assert(TryStrToSDecimal64('-9999999999', A) = dceNoError);
  Assert(SDecimal64ToInt64(A) = -9999999999);

  Assert(TryStrToSDecimal64('-10000000000', A) = dceOverflowError);
  Assert(TryStrToSDecimal64('-9999999999.9999999995', A) = dceOverflowError);
end;

procedure Test_SDecimal128;
var A, B : SDecimal128;
    C : Word64Rec;
begin
  SDecimal128InitZero(A);
  Assert(SDecimal128IsZero(A));
  Assert(not SDecimal128IsOne(A));
  Assert(not SDecimal128IsMaximum(A));
  Assert(not SDecimal128IsOverflow(A));
  Assert(SDecimal128Sign(A) = 0);
  Assert(SDecimal128ToWord8(A) = 0);
  Assert(SDecimal128ToInt64(A) = 0);
  Assert(SDecimal128ToStr(A) = '0.0000000000000000000');
  Assert(SDecimal128EqualsWord8(A, 0));
  Assert(SDecimal128EqualsWord16(A, 0));
  Assert(SDecimal128EqualsWord32(A, 0));
  Assert(not SDecimal128EqualsWord32(A, 1));
  Word64InitZero(C);
  Assert(SDecimal128EqualsWord64(A, C));
  Assert(SDecimal128EqualsInt64(A, 0));
  Assert(not SDecimal128EqualsInt64(A, 1));
  Assert(SDecimal128CompareWord8(A, 0) = 0);
  Assert(SDecimal128CompareWord8(A, 1) = -1);
  Assert(SDecimal128CompareWord16(A, 0) = 0);
  Assert(SDecimal128CompareWord16(A, 1) = -1);
  Assert(Int128ToStr(SDecimal128Trunc(A)) = '0');
  Assert(Int128ToStr(SDecimal128Round(A)) = '0');
  Assert(Word64ToStr(SDecimal128FracWord(A)) = '0');
  Assert(SDecimal128EqualsFloat(A, 0.0));
  Assert(SDecimal128Sign(A) = 0);
  Assert(SDecimal128CompareFloat(A, 0.0) = 0);
  Assert(SDecimal128CompareFloat(A, 0.1) = -1);
  Assert(SDecimal128CompareFloat(A, -0.1) = 1);
  SDecimal128Negate(A);
  Assert(SDecimal128Sign(A) = 0);
  Assert(SDecimal128IsZero(A));
  SDecimal128AbsInPlace(A);
  Assert(SDecimal128Sign(A) = 0);

  SDecimal128InitOne(A);
  Assert(not SDecimal128IsZero(A));
  Assert(SDecimal128IsOne(A));
  Assert(not SDecimal128IsMinusOne(A));
  Assert(SDecimal128Sign(A) = 1);
  Assert(SDecimal128ToInt32(A) = 1);
  Assert(SDecimal128ToStr(A) = '1.0000000000000000000');
  Assert(SDecimal128ToStrB(A) = '1.0000000000000000000');
  Assert(SDecimal128ToStrU(A) = '1.0000000000000000000');
  Assert(SDecimal128EqualsWord8(A, 1));
  Assert(not SDecimal128EqualsWord8(A, 0));
  Assert(SDecimal128EqualsWord16(A, 1));
  Assert(SDecimal128EqualsWord32(A, 1));
  Assert(not SDecimal128EqualsWord32(A, 0));
  Word64InitOne(C);
  Assert(SDecimal128EqualsWord64(A, C));
  Assert(SDecimal128EqualsInt64(A, 1));
  Assert(not SDecimal128EqualsInt64(A, -1));
  Assert(SDecimal128CompareWord8(A, 0) = 1);
  Assert(SDecimal128CompareWord8(A, 1) = 0);
  Assert(SDecimal128CompareWord8(A, 2) = -1);
  Assert(SDecimal128CompareWord16(A, 0) = 1);
  Assert(SDecimal128CompareWord16(A, 1) = 0);
  Assert(SDecimal128CompareWord16(A, 2) = -1);
  Word64InitZero(C);
  Assert(SDecimal128CompareWord64(A, C) = 1);
  Word64InitOne(C);
  Assert(SDecimal128CompareWord64(A, C) = 0);
  Word64InitWord32(C, 2);
  Assert(SDecimal128CompareWord64(A, C) = -1);
  Assert(Int128ToStr(SDecimal128Trunc(A)) = '1');
  Assert(Int128ToStr(SDecimal128Round(A)) = '1');
  Assert(SDecimal128Sign(A) = 1);
  Assert(SDecimal128CompareFloat(A, 1.0) = 0);
  Assert(SDecimal128CompareFloat(A, 1.1) = -1);
  Assert(SDecimal128CompareFloat(A, 0.9) = 1);
  Assert(SDecimal128CompareFloat(A, -0.1) = 1);
  SDecimal128Negate(A);
  Assert(SDecimal128Sign(A) = -1);
  Assert(SDecimal128IsMinusOne(A));
  SDecimal128Negate(A);
  Assert(SDecimal128Sign(A) = 1);
  Assert(SDecimal128IsOne(A));
  SDecimal128AbsInPlace(A);
  Assert(SDecimal128Sign(A) = 1);
  Assert(SDecimal128IsOne(A));
  Assert(SDecimal128ToWord16(A) = 1);
  Assert(SDecimal128ToWord32(A) = 1);
  C := SDecimal128ToWord64(A);
  Assert(Word64IsOne(C));
  Assert(SDecimal128ToInt64(A) = 1);
  SDecimal128InitWord8(A, 1);
  Assert(SDecimal128IsOne(A));
  SDecimal128InitWord16(A, 1);
  Assert(SDecimal128IsOne(A));
  SDecimal128InitWord32(A, 1);
  Assert(SDecimal128IsOne(A));
  Word64InitOne(C);
  SDecimal128InitWord64(A, C);
  Assert(SDecimal128IsOne(A));

  SDecimal128InitMinusOne(A);
  Assert(not SDecimal128IsZero(A));
  Assert(not SDecimal128IsOne(A));
  Assert(SDecimal128IsMinusOne(A));
  Assert(SDecimal128Sign(A) = -1);
  Assert(SDecimal128ToInt32(A) = -1);
  Assert(SDecimal128ToInt64(A) = -1);
  Assert(SDecimal128ToStr(A) = '-1.0000000000000000000');
  Assert(SDecimal128ToStrB(A) = '-1.0000000000000000000');
  Assert(not SDecimal128EqualsWord8(A, 1));
  Assert(SDecimal128EqualsInt32(A, -1));
  Assert(SDecimal128EqualsInt64(A, -1));
  Assert(not SDecimal128EqualsInt64(A, 1));
  Assert(SDecimal128CompareWord8(A, 0) = -1);
  Assert(SDecimal128CompareWord16(A, 0) = -1);
  Assert(SDecimal128CompareWord32(A, 0) = -1);
  Word64InitZero(C);
  Assert(SDecimal128CompareWord64(A, C) = -1);
  Assert(SDecimal128CompareInt32(A, -2) = 1);
  Assert(SDecimal128CompareInt32(A, -1) = 0);
  Assert(SDecimal128CompareInt32(A, 0) = -1);
  Assert(SDecimal128CompareInt64(A, -2) = 1);
  Assert(SDecimal128CompareInt64(A, -1) = 0);
  Assert(SDecimal128CompareInt64(A, 0) = -1);
  Assert(Int128ToStr(SDecimal128Trunc(A)) = '-1');
  Assert(Int128ToStr(SDecimal128Round(A)) = '-1');
  Assert(SDecimal128EqualsFloat(A, -1.0));
  Assert(SDecimal128Sign(A) = -1);
  Assert(SDecimal128CompareFloat(A, -1.0) = 0);
  Assert(SDecimal128CompareFloat(A, -1.1) = 1);
  Assert(SDecimal128CompareFloat(A, -0.9) = -1);
  SDecimal128AbsInPlace(A);
  Assert(SDecimal128Sign(A) = 1);
  Assert(SDecimal128IsOne(A));
  SDecimal128Negate(A);
  Assert(SDecimal128IsMinusOne(A));
  SDecimal128InitInt32(A, -1);
  Assert(SDecimal128IsMinusOne(A));
  SDecimal128InitInt64(A, -1);
  Assert(SDecimal128IsMinusOne(A));

  Assert(FloatIsSDecimal128Range(9999999999999990000.0));
  Assert(not FloatIsSDecimal128Range(10000000000000000000.0));

  SDecimal128InitFloat(A, -1.234567890);
  Assert(Abs(SDecimal128ToFloat(A) - -1.234567890) < 1e-12);
  Assert(Int128ToStr(SDecimal128Trunc(A)) = '-1');
  Assert(Int128ToStr(SDecimal128Round(A)) = '-1');
  Assert(SDecimal128EqualsFloat(A, -1.234567890));

  A := StrToSDecimal128('9999999999999999999.9999999999999999999');
  Assert(SDecimal128IsMaximum(A));
  Assert(SDecimal128Sign(A) = 1);

  A := StrToSDecimal128('-9999999999999999999.9999999999999999999');
  Assert(SDecimal128IsMinimum(A));
  Assert(SDecimal128Sign(A) = -1);

  SDecimal128InitMax(A);
  Assert(SDecimal128IsMaximum(A));
  Assert(not SDecimal128IsOverflow(A));
  Assert(SDecimal128ToStr(A) = '9999999999999999999.9999999999999999999');
  Assert(Int128ToStr(SDecimal128Trunc(A)) = '9999999999999999999');
  Assert(Int128ToStr(SDecimal128Round(A)) = '10000000000000000000');
  Assert(Word64ToStr(SDecimal128FracWord(A)) = '9999999999999999999');

  SDecimal128InitMin(A);
  Assert(SDecimal128IsMinimum(A));
  Assert(not SDecimal128IsOverflow(A));
  Assert(SDecimal128ToStr(A) = '-9999999999999999999.9999999999999999999');
  Assert(Int128ToStr(SDecimal128Trunc(A)) = '-9999999999999999999');
  Assert(Int128ToStr(SDecimal128Round(A)) = '-10000000000000000000');
  Assert(Word64ToStr(SDecimal128FracWord(A)) = '9999999999999999999');

  SDecimal128InitFloat(A, -1.9375);
  Assert(SDecimal128ToFloat(A) = -1.9375);
  SDecimal128AddWord8(A, 1);
  Assert(SDecimal128ToFloat(A) = -0.9375);
  SDecimal128AddWord8(A, 1);
  Assert(SDecimal128ToFloat(A) = 0.0625);
  SDecimal128AddWord8(A, 1);
  Assert(SDecimal128ToFloat(A) = 1.0625);
  SDecimal128SubtractWord8(A, 1);
  Assert(SDecimal128ToFloat(A) = 0.0625);
  SDecimal128SubtractWord8(A, 1);
  Assert(SDecimal128ToFloat(A) = -0.9375);
  SDecimal128SubtractWord8(A, 1);
  Assert(SDecimal128ToFloat(A) = -1.9375);
  SDecimal128AddWord16(A, 2);
  Assert(SDecimal128ToFloat(A) = 0.0625);
  SDecimal128SubtractWord16(A, 2);
  Assert(SDecimal128ToFloat(A) = -1.9375);
  SDecimal128AddWord32(A, 2);
  Assert(SDecimal128ToFloat(A) = 0.0625);
  SDecimal128AddWord16(A, 123);
  Assert(SDecimal128ToStrB(A) = '123.0625000000000000000');
  SDecimal128AddWord32(A, 1000000);
  Assert(SDecimal128ToStrB(A) = '1000123.0625000000000000000');
  Word64InitWord32(C, 10000000);
  SDecimal128AddWord64(A, C);
  Assert(SDecimal128ToStrB(A) = '11000123.0625000000000000000');
  SDecimal128SubtractWord64(A, C);
  Assert(SDecimal128ToStrB(A) = '1000123.0625000000000000000');
  SDecimal128SubtractWord32(A, 1000000);
  Assert(SDecimal128ToStrB(A) = '123.0625000000000000000');

  SDecimal128InitFloat(A, -1.9375);
  SDecimal128InitFloat(B, -2.5000);
  SDecimal128AddSDecimal128(A, B);
  Assert(SDecimal128ToFloat(A) = -4.4375);
  SDecimal128SubtractSDecimal128(A, B);
  Assert(SDecimal128ToFloat(A) = -1.9375);

  SDecimal128InitFloat(A, -1.9375);
  SDecimal128InitFloat(B, -2.5000);
  Assert(not SDecimal128EqualsSDecimal128(A, B));
  Assert(SDecimal128EqualsSDecimal128(A, A));
  Assert(SDecimal128CompareSDecimal128(A, B) = 1);
  Assert(SDecimal128CompareSDecimal128(A, A) = 0);
  Assert(SDecimal128CompareSDecimal128(B, A) = -1);

  SDecimal128InitFloat(A, -1.9375);
  SDecimal128MultiplyWord8(A, 3);
  Assert(SDecimal128ToFloat(A) = -5.8125);
  SDecimal128DivideWord8(A, 3);
  Assert(SDecimal128ToStr(A) = '-1.9375000000000000000');
  SDecimal128MultiplyWord16(A, 3);
  Assert(SDecimal128ToFloat(A) = -5.8125);
  SDecimal128DivideWord16(A, 3);
  Assert(SDecimal128ToStr(A) = '-1.9375000000000000000');
  SDecimal128MultiplyWord32(A, 3);
  Assert(SDecimal128ToFloat(A) = -5.8125);
  SDecimal128DivideWord32(A, 3);
  Assert(SDecimal128ToStr(A) = '-1.9375000000000000000');
  Word64InitWord32(C, 3);
  SDecimal128MultiplyWord64(A, C);
  Assert(SDecimal128ToFloat(A) = -5.8125);
  SDecimal128DivideWord64(A, C);
  Assert(SDecimal128ToStr(A) = '-1.9375000000000000000');

  SDecimal128InitFloat(A, 1.9375);
  SDecimal128InitFloat(B, -2.5000);
  SDecimal128MultiplySDecimal128(A, B);
  Assert(SDecimal128ToFloat(A) = -4.84375);
  Assert(SDecimal128ToStr(A) = '-4.8437500000000000000');
  SDecimal128DivideSDecimal128(A, B);
  Assert(SDecimal128ToStr(A) = '1.9375000000000000000');

  SDecimal128InitFloat(A, -12.937512);
  SDecimal128InitFloat(B, -3.123433);
  SDecimal128MultiplySDecimal128(A, B);
  Assert(SDecimal128ToFloat(A) - 40.409451919 < 1e-10);
  Assert(CopyLeft(SDecimal128ToStr(A), 16) = '40.4094519186960');

  A := StrToSDecimal128('0');
  Assert(SDecimal128ToWord8(A) = 0);
  A := StrToSDecimal128('-0');
  Assert(SDecimal128ToWord8(A) = 0);
  A := StrToSDecimal128('+0');
  Assert(SDecimal128ToWord8(A) = 0);
  A := StrToSDecimal128('-0.0000');
  Assert(SDecimal128ToWord8(A) = 0);
  A := StrToSDecimal128('000000000000.000000000000');
  Assert(SDecimal128ToWord8(A) = 0);
  A := StrToSDecimal128('-1');
  Assert(SDecimal128ToInt32(A) = -1);
  A := StrToSDecimal128('-1.0');
  Assert(SDecimal128ToInt32(A) = -1);
  A := StrToSDecimal128('-123.938500000');
  Assert(SDecimal128ToStr(A) = '-123.9385000000000000000');

  Assert(TryStrToSDecimal128('0', A) = dceNoError);
  Assert(SDecimal128ToWord32(A) = 0);
  Assert(TryStrToSDecimal128('-9999999999999999999', A) = dceNoError);

  Assert(TryStrToSDecimal128('-10000000000000000000', A) = dceOverflowError);
  Assert(TryStrToSDecimal128('-9999999999999999999.99999999999999999995', A) = dceOverflowError);
end;

procedure Test_SHugeDecimal;
var
  A, B : SHugeDecimal;
  D : HugeDecimal;
begin
  SHugeDecimalInit(A);
  SHugeDecimalInit(B);
  HugeDecimalInit(D);

  SHugeDecimalAssignZero(A);
  Assert(SHugeDecimalIsZero(A));
  Assert(not SHugeDecimalIsOne(A));
  Assert(not SHugeDecimalIsMinusOne(A));
  Assert(SHugeDecimalToStr(A) = '0');
  Assert(SHugeDecimalToWord8(A) = 0);
  Assert(SHugeDecimalToWord32(A) = 0);
  Assert(SHugeDecimalToInt8(A) = 0);
  Assert(SHugeDecimalToInt64(A) = 0);
  Assert(SHugeDecimalSign(A) = 0);
  Assert(SHugeDecimalFracCompareHalf(A) = -1);
  Assert(SHugeDecimalEqualsWord8(A, 0));
  Assert(not SHugeDecimalEqualsWord8(A, 1));
  Assert(SHugeDecimalCompareWord8(A, 0) = 0);
  Assert(SHugeDecimalCompareWord8(A, 1) = -1);

  SHugeDecimalAssignOne(A);
  Assert(not SHugeDecimalIsZero(A));
  Assert(SHugeDecimalIsOne(A));
  Assert(not SHugeDecimalIsMinusOne(A));
  Assert(SHugeDecimalToStr(A) = '1');
  Assert(SHugeDecimalToWord8(A) = 1);
  Assert(SHugeDecimalToWord32(A) = 1);
  Assert(SHugeDecimalToInt8(A) = 1);
  Assert(SHugeDecimalToInt64(A) = 1);
  Assert(SHugeDecimalSign(A) = 1);
  Assert(SHugeDecimalFracCompareHalf(A) = -1);
  Assert(not SHugeDecimalEqualsWord8(A, 0));
  Assert(SHugeDecimalEqualsWord8(A, 1));
  Assert(SHugeDecimalCompareWord8(A, 0) = 1);
  Assert(SHugeDecimalCompareWord8(A, 1) = 0);
  Assert(SHugeDecimalCompareWord8(A, 2) = -1);

  SHugeDecimalAssignMinusOne(A);
  Assert(not SHugeDecimalIsZero(A));
  Assert(not SHugeDecimalIsOne(A));
  Assert(SHugeDecimalIsMinusOne(A));
  Assert(SHugeDecimalToStr(A) = '-1');
  Assert(SHugeDecimalToInt8(A) = -1);
  Assert(SHugeDecimalToInt64(A) = -1);
  Assert(SHugeDecimalSign(A) = -1);
  Assert(SHugeDecimalFracCompareHalf(A) = -1);
  Assert(not SHugeDecimalEqualsWord8(A, 0));
  Assert(not SHugeDecimalEqualsWord8(A, 1));
  Assert(SHugeDecimalCompareWord8(A, 0) = -1);
  Assert(SHugeDecimalCompareWord8(A, 1) = -1);

  SHugeDecimalAssignInt8(A, 0);
  Assert(SHugeDecimalIsZero(A));

  SHugeDecimalAssignInt8(A, -1);
  Assert(SHugeDecimalIsMinusOne(A));

  SHugeDecimalAssignInt8(A, 1);
  Assert(SHugeDecimalIsOne(A));

  SHugeDecimalAssignInt8(A, MinInt8);
  Assert(SHugeDecimalToStr(A) = '-128');
  Assert(SHugeDecimalToInt8(A) = -128);
  Assert(SHugeDecimalToInt64(A) = -128);
  Assert(SHugeDecimalSign(A) = -1);
  Assert(not SHugeDecimalEqualsWord8(A, 128));
  Assert(SHugeDecimalCompareWord8(A, 0) = -1);

  SHugeDecimalAssignInt8(A, MaxInt8);
  Assert(SHugeDecimalToStr(A) = '127');
  Assert(SHugeDecimalToWord8(A) = 127);
  Assert(SHugeDecimalToWord32(A) = 127);
  Assert(SHugeDecimalToInt8(A) = 127);
  Assert(SHugeDecimalToInt64(A) = 127);
  Assert(SHugeDecimalSign(A) = 1);
  Assert(SHugeDecimalEqualsWord8(A, 127));
  Assert(not SHugeDecimalEqualsWord8(A, 255));
  Assert(SHugeDecimalCompareWord8(A, 0) = 1);
  Assert(SHugeDecimalCompareWord8(A, 127) = 0);

  SHugeDecimalAssignInt32(A, MinInt32);
  Assert(SHugeDecimalToStr(A) = '-2147483648');

  SHugeDecimalAssignInt32(A, MaxInt32);
  Assert(SHugeDecimalToStr(A) = '2147483647');
  Assert(SHugeDecimalToWord32(A) = 2147483647);

  SHugeDecimalAssignInt64(A, MinInt64);
  Assert(SHugeDecimalToStr(A) = '-9223372036854775808');
  Assert(SHugeDecimalToInt64(A) = MinInt64);

  SHugeDecimalAssignInt64(A, MaxInt64);
  Assert(SHugeDecimalToStr(A) = '9223372036854775807');
  Assert(SHugeDecimalToInt64(A) = MaxInt64);

  // Negate
  SHugeDecimalAssignZero(A);
  SHugeDecimalNegate(A);
  Assert(SHugeDecimalToWord32(A) = 0);

  SHugeDecimalAssignOne(A);
  SHugeDecimalNegate(A);
  Assert(SHugeDecimalToInt32(A) = -1);

  SHugeDecimalAssignMinusOne(A);
  SHugeDecimalNegate(A);
  Assert(SHugeDecimalToInt32(A) = 1);

  // Abs
  SHugeDecimalAssignZero(A);
  SHugeDecimalAbsInPlace(A);
  Assert(SHugeDecimalToInt32(A) = 0);

  SHugeDecimalAssignOne(A);
  SHugeDecimalAbsInPlace(A);
  Assert(SHugeDecimalToInt32(A) = 1);

  SHugeDecimalAssignMinusOne(A);
  SHugeDecimalAbsInPlace(A);
  Assert(SHugeDecimalToInt32(A) = 1);

  // Trunc
  SHugeDecimalAssignDecimal32(A, StrToDecimal32('0.2000'));
  Assert(SHugeDecimalToStr(A) = '0.2');
  SHugeDecimalTrunc(A);
  Assert(SHugeDecimalToStr(A) = '0');

  SHugeDecimalAssignDecimal32(A, StrToDecimal32('1.2000'));
  Assert(SHugeDecimalToStr(A) = '1.2');
  SHugeDecimalTrunc(A);
  Assert(SHugeDecimalToStr(A) = '1');

  SHugeDecimalAssignDecimal32(A, StrToDecimal32('1.9999'));
  Assert(SHugeDecimalToStr(A) = '1.9999');
  SHugeDecimalTrunc(A);
  Assert(SHugeDecimalToStr(A) = '1');

  // Round
  SHugeDecimalAssignDecimal32(A, StrToDecimal32('0.9999'));
  Assert(SHugeDecimalToStr(A) = '0.9999');
  Assert(SHugeDecimalFracCompareHalf(A) = 1);
  SHugeDecimalRound(A);
  Assert(SHugeDecimalToStr(A) = '1');

  SHugeDecimalAssignDecimal32(A, StrToDecimal32('0.5000'));
  Assert(SHugeDecimalToStr(A) = '0.5');
  Assert(SHugeDecimalFracCompareHalf(A) = 0);
  SHugeDecimalRound(A);
  Assert(SHugeDecimalToStr(A) = '0');

  SHugeDecimalAssignDecimal32(A, StrToDecimal32('0.5001'));
  Assert(SHugeDecimalToStr(A) = '0.5001');
  Assert(SHugeDecimalFracCompareHalf(A) = 1);
  SHugeDecimalRound(A);
  Assert(SHugeDecimalToStr(A) = '1');

  SHugeDecimalAssignDecimal32(A, StrToDecimal32('1.4999'));
  Assert(SHugeDecimalToStr(A) = '1.4999');
  Assert(SHugeDecimalFracCompareHalf(A) = -1);
  SHugeDecimalRound(A);
  Assert(SHugeDecimalToStr(A) = '1');

  SHugeDecimalAssignDecimal32(A, StrToDecimal32('1.5000'));
  Assert(SHugeDecimalToStr(A) = '1.5');
  Assert(SHugeDecimalFracCompareHalf(A) = 0);
  SHugeDecimalRound(A);
  Assert(SHugeDecimalToStr(A) = '2');

  // Compare/Equals
  SHugeDecimalAssignInt8(A, 10);
  SHugeDecimalAssignInt8(B, 0);
  Assert(SHugeDecimalCompareSHugeDecimal(A, B) = 1);
  Assert(not SHugeDecimalEqualsSHugeDecimal(A, B));

  SHugeDecimalAssignInt8(A, 0);
  SHugeDecimalAssignInt8(B, 10);
  Assert(SHugeDecimalCompareSHugeDecimal(A, B) = -1);
  Assert(not SHugeDecimalEqualsSHugeDecimal(A, B));

  SHugeDecimalAssignInt8(A, 10);
  SHugeDecimalAssignInt8(B, 10);
  Assert(SHugeDecimalCompareSHugeDecimal(A, B) = 0);
  Assert(SHugeDecimalEqualsSHugeDecimal(A, B));

  SHugeDecimalAssignInt8(A, -10);
  SHugeDecimalAssignInt8(B, -10);
  Assert(SHugeDecimalCompareSHugeDecimal(A, B) = 0);
  Assert(SHugeDecimalEqualsSHugeDecimal(A, B));

  SHugeDecimalAssignInt8(A, 10);
  SHugeDecimalAssignInt8(B, -10);
  Assert(SHugeDecimalCompareSHugeDecimal(A, B) = 1);
  Assert(not SHugeDecimalEqualsSHugeDecimal(A, B));

  SHugeDecimalAssignInt8(A, -10);
  SHugeDecimalAssignInt8(B, 10);
  Assert(SHugeDecimalCompareSHugeDecimal(A, B) = -1);
  Assert(not SHugeDecimalEqualsSHugeDecimal(A, B));

  // Compare/Equals
  SHugeDecimalAssignInt8(A, 10);
  HugeDecimalAssignWord8(D, 0);
  Assert(SHugeDecimalCompareHugeDecimal(A, D) = 1);
  Assert(not SHugeDecimalEqualsHugeDecimal(A, D));

  SHugeDecimalAssignInt8(A, 0);
  HugeDecimalAssignWord8(D, 10);
  Assert(SHugeDecimalCompareHugeDecimal(A, D) = -1);
  Assert(not SHugeDecimalEqualsHugeDecimal(A, D));

  SHugeDecimalAssignInt8(A, 10);
  HugeDecimalAssignWord8(D, 10);
  Assert(SHugeDecimalCompareHugeDecimal(A, D) = 0);
  Assert(SHugeDecimalEqualsHugeDecimal(A, D));

  SHugeDecimalAssignInt8(A, -10);
  HugeDecimalAssignWord8(D, 10);
  Assert(SHugeDecimalCompareHugeDecimal(A, D) = -1);
  Assert(not SHugeDecimalEqualsHugeDecimal(A, D));

  // Add
  SHugeDecimalAssignInt8(A, 10);
  SHugeDecimalAssignInt8(B, 0);
  SHugeDecimalAddSHugeDecimal(A, B);
  Assert(SHugeDecimalToInt8(A) = 10);

  SHugeDecimalAssignInt8(A, 0);
  SHugeDecimalAssignInt8(B, 10);
  SHugeDecimalAddSHugeDecimal(A, B);
  Assert(SHugeDecimalToInt8(A) = 10);

  SHugeDecimalAssignInt8(A, 10);
  SHugeDecimalAssignInt8(B, 10);
  SHugeDecimalAddSHugeDecimal(A, B);
  Assert(SHugeDecimalToInt8(A) = 20);

  SHugeDecimalAssignInt8(A, -10);
  SHugeDecimalAssignInt8(B, -10);
  SHugeDecimalAddSHugeDecimal(A, B);
  Assert(SHugeDecimalToInt8(A) = -20);

  SHugeDecimalAssignInt8(A, 10);
  SHugeDecimalAssignInt8(B, -10);
  SHugeDecimalAddSHugeDecimal(A, B);
  Assert(SHugeDecimalToInt8(A) = 0);

  SHugeDecimalAssignInt8(A, -10);
  SHugeDecimalAssignInt8(B, 10);
  SHugeDecimalAddSHugeDecimal(A, B);
  Assert(SHugeDecimalToInt8(A) = 0);

  // Add
  SHugeDecimalAssignInt8(A, 10);
  HugeDecimalAssignWord8(D, 0);
  SHugeDecimalAddHugeDecimal(A, D);
  Assert(SHugeDecimalToInt8(A) = 10);

  SHugeDecimalAssignInt8(A, 0);
  HugeDecimalAssignWord8(D, 10);
  SHugeDecimalAddHugeDecimal(A, D);
  Assert(SHugeDecimalToInt8(A) = 10);

  SHugeDecimalAssignInt8(A, 10);
  HugeDecimalAssignWord8(D, 10);
  SHugeDecimalAddHugeDecimal(A, D);
  Assert(SHugeDecimalToInt8(A) = 20);

  SHugeDecimalAssignInt8(A, -10);
  HugeDecimalAssignWord8(D, 10);
  SHugeDecimalAddHugeDecimal(A, D);
  Assert(SHugeDecimalToInt8(A) = 0);

  // Subtract
  SHugeDecimalAssignInt8(A, 10);
  SHugeDecimalAssignInt8(B, 0);
  SHugeDecimalSubtractSHugeDecimal(A, B);
  Assert(SHugeDecimalToInt8(A) = 10);

  SHugeDecimalAssignInt8(A, 0);
  SHugeDecimalAssignInt8(B, 10);
  SHugeDecimalSubtractSHugeDecimal(A, B);
  Assert(SHugeDecimalToInt8(A) = -10);

  SHugeDecimalAssignInt8(A, 10);
  SHugeDecimalAssignInt8(B, 10);
  SHugeDecimalSubtractSHugeDecimal(A, B);
  Assert(SHugeDecimalToInt8(A) = 0);

  SHugeDecimalAssignInt8(A, -10);
  SHugeDecimalAssignInt8(B, -10);
  SHugeDecimalSubtractSHugeDecimal(A, B);
  Assert(SHugeDecimalToInt8(A) = 0);

  SHugeDecimalAssignInt8(A, 10);
  SHugeDecimalAssignInt8(B, -10);
  SHugeDecimalSubtractSHugeDecimal(A, B);
  Assert(SHugeDecimalToInt8(A) = 20);

  SHugeDecimalAssignInt8(A, -10);
  SHugeDecimalAssignInt8(B, 10);
  SHugeDecimalSubtractSHugeDecimal(A, B);
  Assert(SHugeDecimalToInt8(A) = -20);

  // StrToSHugeDecimal
  Assert(TryStrToSHugeDecimal('', A) = dceConvertError);
  Assert(TryStrToSHugeDecimal('+', A) = dceConvertError);
  Assert(TryStrToSHugeDecimal('-', A) = dceConvertError);
  Assert(TryStrToSHugeDecimal('--0', A) = dceConvertError);
  Assert(TryStrToSHugeDecimal('-.', A) = dceConvertError);
  Assert(TryStrToSHugeDecimal(' 0', A) = dceConvertError);

  Assert(TryStrToSHugeDecimal('0', A) = dceNoError);
  Assert(SHugeDecimalToStr(A) = '0');
  Assert(SHugeDecimalIsZero(A));

  Assert(TryStrToSHugeDecimal('+0', A) = dceNoError);
  Assert(SHugeDecimalToStr(A) = '0');
  Assert(SHugeDecimalIsZero(A));

  Assert(TryStrToSHugeDecimal('-0', A) = dceNoError);
  Assert(SHugeDecimalToStr(A) = '0');
  Assert(SHugeDecimalIsZero(A));

  Assert(TryStrToSHugeDecimal('1', A) = dceNoError);
  Assert(SHugeDecimalToStr(A) = '1');

  Assert(TryStrToSHugeDecimal('-1', A) = dceNoError);
  Assert(SHugeDecimalToStr(A) = '-1');

  Assert(TryStrToSHugeDecimal('+1', A) = dceNoError);
  Assert(SHugeDecimalToStr(A) = '1');

  Assert(TryStrToSHugeDecimal('-1.23', A) = dceNoError);
  Assert(SHugeDecimalToStr(A) = '-1.23');
end;

procedure Test;
begin
  SetRoundMode(rmNearest);

  Test_Decimal32;
  Test_Decimal64;
  Test_Decimal128;
  Test_HugeDecimal;

  Test_SDecimal32;
  Test_SDecimal64;
  Test_SDecimal128;
  Test_SHugeDecimal;
end;
{$ENDIF}



end.

