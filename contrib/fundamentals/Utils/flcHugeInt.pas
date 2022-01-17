{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals 5.00                                        }
{   File name:        flcHugeInt.pas                                           }
{   File version:     5.33                                                     }
{   Description:      HugeInt functions                                        }
{                                                                              }
{   Copyright:        Copyright (c) 2001-2020, David J Butler                  }
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
{   2001/11/18  0.01  HugeWord.                                                }
{   2001/11/19  0.02  HugeMultiplyFFT.                                         }
{   2003/10/01  0.03  HugeInt.                                                 }
{   2007/08/08  4.04  Revised for Fundamentals 4.                              }
{   2007/08/16  4.05  Revised HugeWord.                                        }
{   2007/08/22  4.06  PowerAndMod.                                             }
{   2008/01/20  4.07  HugeWord primality testing.                              }
{   2010/01/07  4.08  Minor revision.                                          }
{   2010/08/05  4.09  Changed HugeWord structure to separately keep track of   }
{                     allocated and used size.                                 }
{   2010/08/06  4.10  Revision, improved tests and bug fixes.                  }
{   2010/08/08  4.11  Improved ISqrt algorithm, HugeInt tests.                 }
{   2010/08/09  4.12  Optimisation and bug fix.                                }
{   2010/12/01  4.13  HugeWordAssignBuf.                                       }
{   2011/01/24  4.14  Revised for FreePascal 2.4.2.                            }
{   2011/01/25  4.15  THugeInt class.                                          }
{   2011/04/02  4.16  Compilable with Delphi 5.                                }
{   2011/09/03  4.17  Fix for Delphi 7 in HugeIntToInt32.                      }
{   2011/10/18  4.18  Minor optimisation.                                      }
{   2012/11/15  4.19  Improvements to HugeWordIsPrime_MillerRabin courtesy of  }
{                     Wolfgang Ehrhardt.                                       }
{   2015/03/29  4.20  Minor optimisations.                                     }
{   2015/04/01  4.21  Compilable with FreePascal 2.6.2.                        }
{   2015/04/20  4.22  Float-type conversion functions.                         }
{   2015/06/10  4.23  Fix in HugeWordIsPrime_MillerRabin.                      }
{   2016/01/09  5.24  Revised for Fundamentals 5.                              }
{   2018/07/17  5.25  Word32 changes.                                          }
{   2018/08/12  5.26  String type changes.                                     }
{   2019/09/02  5.27  Initial static buffer for HugeWord.                      }
{   2019/09/02  5.28  Optimisation to HugeWordSubtract and HugeWordMod.        }
{   2019/09/02  5.29  Unroll loops in HugeWordShl1 and HugeWordCompare.        }
{   2019/10/07  5.30  Optimisation to HugeWordDivide and HugeWordMod.          }
{   2020/03/08  5.31  HugeIntMod_T, HugeIntMod_F, HugeIntMod_E.                }
{   2020/03/10  5.32  Minor optimisations.                                     }
{   2020/03/20  5.33  Define exception classes.                                }
{                                                                              }
{ Supported compilers:                                                         }
{                                                                              }
{   Delphi 2010-10.4 Win32/Win64        5.33  2020/06/02                       }
{   Delphi 10.2-10.4 Linux64            5.33  2020/06/02                       }
{   FreePascal 3.0.4 Win64              5.33  2020/06/02                       }
{                                                                              }
{******************************************************************************}

{$INCLUDE ..\flcInclude.inc}

{$IFDEF FREEPASCAL}
  {$Q-}
{$ENDIF}

{$IFDEF DEBUG}
{$IFDEF TEST}
  {$DEFINE HUGEINT_TEST}
{$ENDIF}
{$ENDIF}

{$IFDEF DEBUG}
{$IFDEF PROFILE}
{$IFDEF OS_MSWIN}
  {$DEFINE HUGEINT_PROFILE}
{$ENDIF}
{$ENDIF}
{$ENDIF}

{$Q-,R-}

unit flcHugeInt;

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
  EHugeIntDivByZero = class(Exception);
  EHugeIntRangeError = class(Exception);
  EHugeIntConvertError = class(EConvertError);
  EHugeIntInvalidOp = class(Exception);



{                                                                              }
{ Structures                                                                   }
{                                                                              }
type
  HugeWordElement = Word32;
  PHugeWordElement = ^HugeWordElement;

const
  HugeWordElementSize = SizeOf(HugeWordElement); // 4 bytes
  HugeWordElementBits = HugeWordElementSize * 8; // 32 bits

const
  HugeWordStaticBufferSize = 132; // 528 bytes, 4224 bits

type
  HugeWord = record
    Used    : Integer;
    Alloc   : Integer;
    Data    : Pointer;
    InitBuf : array[0..HugeWordStaticBufferSize - 1] of HugeWordElement;
  end;
  PHugeWord = ^HugeWord;

type
  THugeWordCallbackProc = function (const Data: NativeInt): Boolean;

type
  HugeInt = record
    Sign  : Int8; // -1, 0 or 1
    Value : HugeWord;
  end;
  PHugeInt = ^HugeInt;



{                                                                              }
{ HugeWord                                                                     }
{                                                                              }
procedure HugeWordInit(out A: HugeWord);
procedure HugeWordFinalise(var A: HugeWord);
procedure HugeWordClearAndFinalise(var A: HugeWord);

function  HugeWordGetSize(const A: HugeWord): Integer; {$IFDEF UseInline}inline;{$ENDIF}
function  HugeWordGetSizeInBits(const A: HugeWord): Integer; {$IFDEF UseInline}inline;{$ENDIF}
procedure HugeWordSetSize_NoZeroMem(var A: HugeWord; const Size: Integer);
procedure HugeWordSetSize(var A: HugeWord; const Size: Integer);
procedure HugeWordSetSizeInBits(var A: HugeWord; const Size: Integer);

function  HugeWordGetElement(const A: HugeWord; const I: Integer): Word32; {$IFDEF UseInline}inline;{$ENDIF}
procedure HugeWordSetElement(const A: HugeWord; const I: Integer; const B: Word32); {$IFDEF UseInline}inline;{$ENDIF}
function  HugeWordGetFirstElementPtr(const A: HugeWord): PHugeWordElement; {$IFDEF UseInline}inline;{$ENDIF}
function  HugeWordGetLastElementPtr(const A: HugeWord): PHugeWordElement; {$IFDEF UseInline}inline;{$ENDIF}

procedure HugeWordNormalise(var A: HugeWord);

procedure HugeWordInitZero(out A: HugeWord); {$IFDEF UseInline}inline;{$ENDIF}
procedure HugeWordInitOne(out A: HugeWord);
procedure HugeWordInitWord32(out A: HugeWord; const B: Word32);
procedure HugeWordInitInt32(out A: HugeWord; const B: Int32);
procedure HugeWordInitInt64(out A: HugeWord; const B: Int64);
procedure HugeWordInitDouble(out A: HugeWord; const B: Double);
procedure HugeWordInitHugeWord(out A: HugeWord; const B: HugeWord);

procedure HugeWordAssignZero(var A: HugeWord); {$IFDEF UseInline}inline;{$ENDIF}
procedure HugeWordAssignOne(var A: HugeWord); {$IFDEF UseInline}inline;{$ENDIF}
procedure HugeWordAssignWord32(var A: HugeWord; const B: Word32);
procedure HugeWordAssignInt32(var A: HugeWord; const B: Int32);
procedure HugeWordAssignInt64(var A: HugeWord; const B: Int64);
procedure HugeWordAssignDouble(var A: HugeWord; const B: Double);
procedure HugeWordAssign(var A: HugeWord; const B: HugeWord);
procedure HugeWordAssignHugeIntAbs(var A: HugeWord; const B: HugeInt);
procedure HugeWordAssignBuf(var A: HugeWord; const Buf; const BufSize: Integer;
          const ReverseByteOrder: Boolean);
procedure HugeWordAssignBufStrB(var A: HugeWord; const Buf: RawByteString;
          const ReverseByteOrder: Boolean);

procedure HugeWordSwap(var A, B: HugeWord);

function  HugeWordIsZero(const A: HugeWord): Boolean; {$IFDEF UseInline}inline;{$ENDIF}
function  HugeWordIsOne(const A: HugeWord): Boolean; {$IFDEF UseInline}inline;{$ENDIF}
function  HugeWordIsTwo(const A: HugeWord): Boolean; {$IFDEF UseInline}inline;{$ENDIF}
function  HugeWordIsOdd(const A: HugeWord): Boolean;
function  HugeWordIsEven(const A: HugeWord): Boolean;

function  HugeWordIsWord32Range(const A: HugeWord): Boolean;
function  HugeWordIsWord64Range(const A: HugeWord): Boolean;
function  HugeWordIsWord128Range(const A: HugeWord): Boolean;
function  HugeWordIsWord256Range(const A: HugeWord): Boolean;
function  HugeWordIsInt32Range(const A: HugeWord): Boolean;
function  HugeWordIsInt64Range(const A: HugeWord): Boolean;
function  HugeWordIsInt128Range(const A: HugeWord): Boolean;
function  HugeWordIsInt256Range(const A: HugeWord): Boolean;

function  HugeWordToWord32(const A: HugeWord): Word32;
function  HugeWordToInt32(const A: HugeWord): Int32;
function  HugeWordToInt64(const A: HugeWord): Int64;
function  HugeWordToDouble(const A: HugeWord): Double;

function  HugeWordEqualsWord32(const A: HugeWord; const B: Word32): Boolean;
function  HugeWordEqualsInt32(const A: HugeWord; const B: Int32): Boolean;
function  HugeWordEqualsInt64(const A: HugeWord; const B: Int64): Boolean;
function  HugeWordEquals(const A, B: HugeWord): Boolean;

function  HugeWordCompareWord32(const A: HugeWord; const B: Word32): Integer;
function  HugeWordCompareInt32(const A: HugeWord; const B: Int32): Integer;
function  HugeWordCompareInt64(const A: HugeWord; const B: Int64): Integer;
function  HugeWordCompare(const A, B: HugeWord): Integer;

procedure HugeWordMin(var A: HugeWord; const B: HugeWord);
procedure HugeWordMax(var A: HugeWord; const B: HugeWord);

function  HugeWordGetBitCount(const A: HugeWord): Integer;
procedure HugeWordSetBitCount(var A: HugeWord; const Bits: Integer);

function  HugeWordIsBitSet(const A: HugeWord; const B: Integer): Boolean;
procedure HugeWordSetBit(var A: HugeWord; const B: Integer);
procedure HugeWordSetBit0(var A: HugeWord); {$IFDEF UseInline}inline;{$ENDIF}
procedure HugeWordClearBit(var A: HugeWord; const B: Integer);
procedure HugeWordToggleBit(var A: HugeWord; const B: Integer);

function  HugeWordSetBitScanForward(const A: HugeWord): Integer;
function  HugeWordSetBitScanReverse(const A: HugeWord): Integer;
function  HugeWordClearBitScanForward(const A: HugeWord): Integer;
function  HugeWordClearBitScanReverse(const A: HugeWord): Integer;

procedure HugeWordShl(var A: HugeWord; const B: Integer);
procedure HugeWordShl1(var A: HugeWord);
procedure HugeWordShr(var A: HugeWord; const B: Integer);
procedure HugeWordShr1(var A: HugeWord);

procedure HugeWordNot(var A: HugeWord);
procedure HugeWordOrHugeWord(var A: HugeWord; const B: HugeWord);
procedure HugeWordAndHugeWord(var A: HugeWord; const B: HugeWord);
procedure HugeWordXorHugeWord(var A: HugeWord; const B: HugeWord);

procedure HugeWordAddWord32(var A: HugeWord; const B: Word32);
procedure HugeWordAdd(var A: HugeWord; const B: HugeWord);

procedure HugeWordInc(var A: HugeWord);

function  HugeWordSubtractWord32(var A: HugeWord; const B: Word32): Integer;
function  HugeWordSubtract(var A: HugeWord; const B: HugeWord): Integer;

procedure HugeWordDec(var A: HugeWord);

procedure HugeWordMultiplyWord8(var A: HugeWord; const B: Byte);
procedure HugeWordMultiplyWord16(var A: HugeWord; const B: Word);
procedure HugeWordMultiplyWord32(var A: HugeWord; const B: Word32);
procedure HugeWordMultiply_Long_NN_Unsafe(var Res: HugeWord; const A, B: HugeWord);
procedure HugeWordMultiply_Long_NN_Safe(var Res: HugeWord; const A, B: HugeWord);
procedure HugeWordMultiply_Long_NN(var Res: HugeWord; const A, B: HugeWord);
procedure HugeWordMultiply_Long(var Res: HugeWord; const A, B: HugeWord);
procedure HugeWordMultiply_ShiftAdd(var Res: HugeWord; const A, B: HugeWord);
procedure HugeWordMultiply(var Res: HugeWord; const A, B: HugeWord); {$IFDEF UseInline}inline;{$ENDIF}

procedure HugeWordSqr(var Res: HugeWord; const A: HugeWord);

procedure HugeWordDivideWord32(const A: HugeWord; const B: Word32; var Q: HugeWord; out R: Word32);
procedure HugeWordDivide_RR_Unsafe(const A, B: HugeWord; var Q, R: HugeWord);
procedure HugeWordDivide_RR_Safe(const A, B: HugeWord; var Q, R: HugeWord);
procedure HugeWordDivide(const A, B: HugeWord; var Q, R: HugeWord);

procedure HugeWordMod(const A, B: HugeWord; var R: HugeWord);

procedure HugeWordGCD(const A, B: HugeWord; var R: HugeWord);

procedure HugeWordPower(var A: HugeWord; const B: Word32);
procedure HugeWordPowerAndMod(var Res: HugeWord; const A, E, M: HugeWord);

function  HugeWordToStrB(const A: HugeWord): UTF8String;
function  HugeWordToStrU(const A: HugeWord): UnicodeString;
function  HugeWordToStr(const A: HugeWord): String;
procedure StrToHugeWordB(const A: RawByteString; var R: HugeWord);
procedure StrToHugeWordU(const A: UnicodeString; var R: HugeWord);
procedure StrToHugeWord(const A: String; var R: HugeWord);

function  HugeWordToHexB(const A: HugeWord; const LowCase: Boolean = False): UTF8String;
function  HugeWordToHex(const A: HugeWord; const LowCase: Boolean = False): String;
procedure HexToHugeWordB(const A: RawByteString; var R: HugeWord);
procedure HexToHugeWord(const A: String; var R: HugeWord);

procedure HugeWordISqrt(var A: HugeWord);

procedure HugeWordExtendedEuclid(const A, B: HugeWord; var R: HugeWord; var X, Y: HugeInt);
function  HugeWordModInv(const E, M: HugeWord; var R: HugeWord): Boolean;

procedure HugeWordRandom(var A: HugeWord; const Size: Integer);
procedure HugeWordRandomN(var A: HugeWord; const N: HugeWord);

type
  TPrimality = (
    pPotentialPrime,
    pNotPrime,
    pPrime);

function  HugeWordIsPrime_QuickTrial(const A: HugeWord): TPrimality;
function  HugeWordIsPrime_MillerRabin(const A: HugeWord): TPrimality;
function  HugeWordIsPrime(const A: HugeWord): TPrimality;

procedure HugeWordNextPotentialPrime(var A: HugeWord;
          const CallbackProc: THugeWordCallbackProc = nil;
          const CallbackData: Integer = 0);



{                                                                              }
{ HugeInt                                                                      }
{                                                                              }
procedure HugeIntInit(out A: HugeInt); {$IFDEF UseInline}inline;{$ENDIF}
procedure HugeIntFinalise(var A: HugeInt); {$IFDEF UseInline}inline;{$ENDIF}
procedure HugeIntClearAndFinalise(var A: HugeInt); {$IFDEF UseInline}inline;{$ENDIF}

procedure HugeIntNormalise(var A: HugeInt);

procedure HugeIntInitZero(out A: HugeInt); {$IFDEF UseInline}inline;{$ENDIF}
procedure HugeIntInitOne(out A: HugeInt);
procedure HugeIntInitMinusOne(out A: HugeInt);
procedure HugeIntInitWord32(out A: HugeInt; const B: Word32);
procedure HugeIntInitInt32(out A: HugeInt; const B: Int32);
procedure HugeIntInitInt64(out A: HugeInt; const B: Int64);
procedure HugeIntInitDouble(out A: HugeInt; const B: Double);
procedure HugeIntInitHugeWord(out A: HugeInt; const B: HugeWord);
procedure HugeIntInitHugeInt(out A: HugeInt; const B: HugeInt);

procedure HugeIntAssignZero(var A: HugeInt); {$IFDEF UseInline}inline;{$ENDIF}
procedure HugeIntAssignOne(var A: HugeInt);
procedure HugeIntAssignMinusOne(var A: HugeInt);
procedure HugeIntAssignWord32(var A: HugeInt; const B: Word32);
procedure HugeIntAssignInt32(var A: HugeInt; const B: Int32);
procedure HugeIntAssignInt64(var A: HugeInt; const B: Int64);
procedure HugeIntAssignDouble(var A: HugeInt; const B: Double);
procedure HugeIntAssignHugeWord(var A: HugeInt; const B: HugeWord);
procedure HugeIntAssignHugeWordNegated(var A: HugeInt; const B: HugeWord);
procedure HugeIntAssign(var A: HugeInt; const B: HugeInt);

procedure HugeIntSwap(var A, B: HugeInt);

function  HugeIntIsZero(const A: HugeInt): Boolean; {$IFDEF UseInline}inline;{$ENDIF}
function  HugeIntIsNegative(const A: HugeInt): Boolean; {$IFDEF UseInline}inline;{$ENDIF}
function  HugeIntIsNegativeOrZero(const A: HugeInt): Boolean; {$IFDEF UseInline}inline;{$ENDIF}
function  HugeIntIsPositive(const A: HugeInt): Boolean; {$IFDEF UseInline}inline;{$ENDIF}
function  HugeIntIsPositiveOrZero(const A: HugeInt): Boolean; {$IFDEF UseInline}inline;{$ENDIF}
function  HugeIntIsOne(const A: HugeInt): Boolean;
function  HugeIntIsMinusOne(const A: HugeInt): Boolean;
function  HugeIntIsOdd(const A: HugeInt): Boolean; {$IFDEF UseInline}inline;{$ENDIF}
function  HugeIntIsEven(const A: HugeInt): Boolean; {$IFDEF UseInline}inline;{$ENDIF}

function  HugeIntIsWord32Range(const A: HugeInt): Boolean;
function  HugeIntIsWord64Range(const A: HugeInt): Boolean;
function  HugeIntIsWord128Range(const A: HugeInt): Boolean;
function  HugeIntIsWord256Range(const A: HugeInt): Boolean;
function  HugeIntIsInt32Range(const A: HugeInt): Boolean;
function  HugeIntIsInt64Range(const A: HugeInt): Boolean;
function  HugeIntIsInt128Range(const A: HugeInt): Boolean;
function  HugeIntIsInt256Range(const A: HugeInt): Boolean;

function  HugeIntSign(const A: HugeInt): Integer; {$IFDEF UseInline}inline;{$ENDIF}
procedure HugeIntNegate(var A: HugeInt); {$IFDEF UseInline}inline;{$ENDIF}
function  HugeIntAbsInPlace(var A: HugeInt): Boolean;
function  HugeIntAbs(const A: HugeInt; var B: HugeWord): Boolean;

function  HugeIntToWord32(const A: HugeInt): Word32;
function  HugeIntToInt32(const A: HugeInt): Int32;
function  HugeIntToInt64(const A: HugeInt): Int64;

function  HugeIntToDouble(const A: HugeInt): Double;

function  HugeIntEqualsWord32(const A: HugeInt; const B: Word32): Boolean;
function  HugeIntEqualsInt32(const A: HugeInt; const B: Int32): Boolean;
function  HugeIntEqualsInt64(const A: HugeInt; const B: Int64): Boolean;
function  HugeIntEqualsHugeInt(const A, B: HugeInt): Boolean;

function  HugeIntCompareWord32(const A: HugeInt; const B: Word32): Integer;
function  HugeIntCompareInt32(const A: HugeInt; const B: Int32): Integer;
function  HugeIntCompareInt64(const A: HugeInt; const B: Int64): Integer;
function  HugeIntCompareHugeInt(const A, B: HugeInt): Integer;
function  HugeIntCompareHugeIntAbs(const A, B: HugeInt): Integer;

procedure HugeIntMin(var A: HugeInt; const B: HugeInt);
procedure HugeIntMax(var A: HugeInt; const B: HugeInt);

procedure HugeIntAddWord32(var A: HugeInt; const B: Word32);
procedure HugeIntAddInt32(var A: HugeInt; const B: Int32);
procedure HugeIntAddHugeInt(var A: HugeInt; const B: HugeInt);

procedure HugeIntInc(var A: HugeInt);

procedure HugeIntSubtractWord32(var A: HugeInt; const B: Word32);
procedure HugeIntSubtractInt32(var A: HugeInt; const B: Int32);
procedure HugeIntSubtractHugeInt(var A: HugeInt; const B: HugeInt);

procedure HugeIntDec(var A: HugeInt);

procedure HugeIntMultiplyWord8(var A: HugeInt; const B: Byte);
procedure HugeIntMultiplyWord16(var A: HugeInt; const B: Word);
procedure HugeIntMultiplyWord32(var A: HugeInt; const B: Word32);
procedure HugeIntMultiplyInt8(var A: HugeInt; const B: ShortInt);
procedure HugeIntMultiplyInt16(var A: HugeInt; const B: SmallInt);
procedure HugeIntMultiplyInt32(var A: HugeInt; const B: Int32);
procedure HugeIntMultiplyHugeWord(var A: HugeInt; const B: HugeWord);
procedure HugeIntMultiplyHugeInt(var A: HugeInt; const B: HugeInt);

procedure HugeIntSqr(var A: HugeInt);

procedure HugeIntDivideWord32(const A: HugeInt; const B: Word32; var Q: HugeInt; out R: Word32);
procedure HugeIntDivideInt32(const A: HugeInt; const B: Int32; var Q: HugeInt; out R: Int32);

procedure HugeIntDivideHugeInt_T(const A, B: HugeInt; var Q, R: HugeInt);
procedure HugeIntDivideHugeInt_F(const A, B: HugeInt; var Q, R: HugeInt);
procedure HugeIntDivideHugeInt_E(const A, B: HugeInt; var Q, R: HugeInt);

procedure HugeIntDivideHugeInt(const A, B: HugeInt; var Q, R: HugeInt);

procedure HugeIntMod_T(const A, B: HugeInt; var R: HugeInt);
procedure HugeIntMod_F(const A, B: HugeInt; var R: HugeInt);
procedure HugeIntMod_E(const A, B: HugeInt; var R: HugeInt);

procedure HugeIntMod(const A, B: HugeInt; var R: HugeInt);

procedure HugeIntPower(var A: HugeInt; const B: Word32);

function  HugeIntToStrB(const A: HugeInt): UTF8String;
function  HugeIntToStrU(const A: HugeInt): UnicodeString;
procedure StrToHugeIntB(const A: RawByteString; var R: HugeInt);
procedure StrToHugeIntU(const A: UnicodeString; var R: HugeInt);
function  HugeIntToHexB(const A: HugeInt): UTF8String;
procedure HexToHugeIntB(const A: RawByteString; var R: HugeInt);

procedure HugeIntISqrt(var A: HugeInt);

procedure HugeIntRandom(var A: HugeInt; const Size: Integer);



{                                                                              }
{ HugeInt class                                                                }
{                                                                              }
type
  THugeInt = class
  private
    FValue : HugeInt;

  public
    constructor Create; overload;
    constructor Create(const A: Int64); overload;
    constructor Create(const A: THugeInt); overload;
    destructor Destroy; override;

    procedure AssignZero;
    procedure AssignOne;
    procedure AssignMinusOne;
    procedure Assign(const A: Int64); overload;
    procedure Assign(const A: THugeInt); overload;

    function  IsZero: Boolean;
    function  IsNegative: Boolean;
    function  IsPositive: Boolean;
    function  IsOne: Boolean;
    function  IsMinusOne: Boolean;
    function  IsOdd: Boolean;
    function  IsEven: Boolean;

    function  Sign: Integer;
    procedure Negate;
    procedure Abs;

    function  ToWord32: Word32;
    function  ToInt32: Int32;
    function  ToInt64: Int64;

    function  EqualTo(const A: Word32): Boolean; overload;
    function  EqualTo(const A: Int32): Boolean; overload;
    function  EqualTo(const A: Int64): Boolean; overload;
    function  EqualTo(const A: THugeInt): Boolean; overload;

    function  Compare(const A: Word32): Integer; overload;
    function  Compare(const A: Int32): Integer; overload;
    function  Compare(const A: Int64): Integer; overload;
    function  Compare(const A: THugeInt): Integer; overload;

    procedure Add(const A: Int32); overload;
    procedure Add(const A: THugeInt); overload;
    procedure Inc;

    procedure Subtract(const A: Int32); overload;
    procedure Subtract(const A: THugeInt); overload;
    procedure Dec;

    procedure Multiply(const A: Int32); overload;
    procedure Multiply(const A: THugeInt); overload;
    procedure Sqr;

    procedure Divide(const B: Int32; out R: Int32); overload;
    procedure Divide(const B: THugeInt; var R: THugeInt); overload;

    procedure Power(const B: Word32);

    function  ToStr: UTF8String;
    function  ToHex: UTF8String;

    procedure AssignStr(const A: RawByteString);
    procedure AssignHex(const A: RawByteString);

    procedure ISqrt;

    procedure Random(const Size: Integer);
  end;



{                                                                              }
{ Test cases                                                                   }
{                                                                              }
{$IFDEF HUGEINT_TEST}
procedure Test;
{$ENDIF}
{$IFDEF HUGEINT_PROFILE}
procedure Profile;
{$ENDIF}



implementation

uses
  { System }
  {$IFDEF HUGEINT_PROFILE}
  Windows,
  {$ENDIF}

  { Fundamentals }
  flcRandom;



{                                                                              }
{ Utilities                                                                    }
{                                                                              }
const
  BitMaskTable32: array[0..31] of Word32 =
    ($00000001, $00000002, $00000004, $00000008,
     $00000010, $00000020, $00000040, $00000080,
     $00000100, $00000200, $00000400, $00000800,
     $00001000, $00002000, $00004000, $00008000,
     $00010000, $00020000, $00040000, $00080000,
     $00100000, $00200000, $00400000, $00800000,
     $01000000, $02000000, $04000000, $08000000,
     $10000000, $20000000, $40000000, $80000000);

{$IFDEF SupportUInt64}
{$IFDEF CPU_64}
  {$DEFINE Pas64}
{$ENDIF}
{$ENDIF}

{$IFDEF DELPHI}
{$IFDEF CPU_X86_64}
{$IFNDEF PurePascal}
  {$DEFINE Asm64}
{$ENDIF}
{$ENDIF}
{$ENDIF}

{$IFDEF Pas64}
{$IFDEF FREEPASCAL}
type
  PUInt64 = ^UInt64;
{$ENDIF}
{$ENDIF}



{                                                                              }
{ Error routines                                                               }
{                                                                              }
const
  SDivByZeroError = 'Division by zero';
  SRangeError = 'Range error';
  SConvertError = 'Conversion error';
  SInvalidOpError = 'Invalid operation';

procedure RaiseDivByZeroError; {$IFDEF UseInline}inline;{$ENDIF}
begin
  raise EHugeIntDivByZero.Create(SDivByZeroError);
end;

procedure RaiseRangeError; {$IFDEF UseInline}inline;{$ENDIF}
begin
  raise EHugeIntRangeError.Create(SRangeError);
end;

procedure RaiseConvertError; {$IFDEF UseInline}inline;{$ENDIF}
begin
  raise EHugeIntConvertError.Create(SConvertError);
end;

procedure RaiseInvalidOpError; {$IFDEF UseInline}inline;{$ENDIF}
begin
  raise EHugeIntInvalidOp.Create(SInvalidOpError);
end;



{                                                                              }
{ HugeWord                                                                     }
{                                                                              }

{ HugeWord Init                                                                }
{   HugeWordInit needs to be called on every instance of HugeWord (except      }
{   where it can be assured the HugeWord structure is zero) before using it    }
{   in calls to other HugeWord routines.                                       }
{   Every HugeWord instance must be finalised with a call to HugeWordFinalise. }
procedure HugeWordInit(out A: HugeWord);
begin
  A.Used := 0;
  A.Alloc := 0;
  A.Data := @A.InitBuf;
end;

{ HugeWord Alloc                                                               }
{   Post: HugeWord data is undefined.                                          }
procedure HugeWordAlloc(var A: HugeWord; const Size: Integer);
var L : Integer;
begin
  Assert(Size > 0);

  L := Size * HugeWordElementSize;
  GetMem(A.Data, L);
  A.Alloc := Size;
end;

{ HugeWord AllocZero                                                           }
{   Post: HugeWord data is zero.                                               }
procedure HugeWordAllocZero(var A: HugeWord; const Size: Integer);
var L : Integer;
begin
  Assert(Size > 0);

  L := Size * HugeWordElementSize;
  GetMem(A.Data, L);
  A.Alloc := Size;
  FillChar(A.Data^, L, 0);
end;

{ HugeWord Free                                                                }
procedure HugeWordFree(var A: HugeWord);
begin
  Assert(A.Alloc > 0);
  Assert(A.Data <> nil);

  FreeMem(A.Data);
  A.Alloc := 0;
  A.Data := @A.InitBuf;
  A.Used := 0;
end;

{ HugeWord Realloc                                                             }
{   Post: If expanding, expanded HugeWord data is not zerod.                   }
procedure HugeWordRealloc(var A: HugeWord; const Size: Integer);
var OldSize, L : Integer;
begin
  Assert(Size >= 0);

  OldSize := A.Alloc;
  if OldSize = Size then
    exit;
  if Size <= 0 then
    begin
      HugeWordFree(A);
      exit;
    end;
  if OldSize = 0 then
    begin
      HugeWordAlloc(A, Size);
      exit;
    end;
  L := Size * HugeWordElementSize;
  ReallocMem(A.Data, L);
  A.Alloc := Size;
end;

{ HugeWord Finalise                                                            }
{ Release resources allocated by the HugeWord.                                 }
procedure HugeWordFinalise(var A: HugeWord);
begin
  if A.Alloc > 0 then
    HugeWordFree(A);
end;

{ HugeWord Clear And Finalise                                                  }
{ Clear used data before finalising HugeWord.                                  }
procedure HugeWordClearAndFinalise(var A: HugeWord);
begin
  if Assigned(A.Data) then
    begin
      if A.Used > 0 then
        FillChar(A.Data^, A.Used, 0);
      if A.Data <> @A.InitBuf then
        FillChar(A.InitBuf, SizeOf(A.InitBuf), 0);
    end;
  HugeWordFinalise(A);
end;

{ HugeWord GetSize                                                             }
{   Post: Returns number of HugeWordItems in the HugeWord structure.           }
function HugeWordGetSize(const A: HugeWord): Integer;
begin
  Result := A.Used;
end;

{ HugeWord GetSizeInBits                                                       }
{   Post: Returns number of bits in the HugeWord structure.                    }
function HugeWordGetSizeInBits(const A: HugeWord): Integer;
begin
  Result := A.Used * HugeWordElementBits;
end;

{ HugeWord SetSize NoZeroMem                                                   }
{   Post: Expanded data is not set to zero.                                    }
procedure HugeWordSetSize_NoZeroMem(var A: HugeWord; const Size: Integer);
var OldUsed, OldAlloc, NewAlloc : Integer;
begin
  Assert(Size >= 0);

  OldUsed := A.Used;
  if Size = OldUsed then // unchanged
    exit;
  if Size < OldUsed then
    begin
      // shrink: keep allocated memory
      A.Used := Size;
      exit;
    end;
  // expand
  OldAlloc := A.Alloc;
  if OldAlloc = 0 then
    begin
      // no dynamic memory allocated
      if Size <= HugeWordStaticBufferSize then
        begin
          // fits in static buffer
          A.Used := Size;
          exit;
        end;
      // first dynamic allocation
      HugeWordAlloc(A, Size);
      if OldUsed > 0 then
        Move(A.InitBuf[0], A.Data^, OldUsed * HugeWordElementSize);
      A.Used := Size;
      exit;
    end;
  if Size > OldAlloc then
    begin
      // expanding block: allocate more memory than requested, this reduces
      // the number of future Realloc calls
      NewAlloc := OldAlloc * 2;
      if NewAlloc < Size then
        NewAlloc := Size;
      HugeWordRealloc(A, NewAlloc);
    end;
  A.Used := Size;
end;

{ HugeWord SetSize                                                             }
{   Post: Expanded data is set to zero.                                        }
procedure HugeWordSetSize(var A: HugeWord; const Size: Integer);
var OldUsed, OldAlloc, NewAlloc : Integer;
    P : PByte;
begin
  Assert(Size >= 0);

  OldUsed := A.Used;
  if Size = OldUsed then // unchanged
    exit;
  if Size < OldUsed then
    begin
      // shrink: keep allocated memory
      A.Used := Size;
      exit;
    end;
  // expand
  OldAlloc := A.Alloc;
  if OldAlloc = 0 then
    begin
      // no dynamic memory allocated
      if Size <= HugeWordStaticBufferSize then
        begin
          // fits in static buffer
          FillChar(A.InitBuf[OldUsed], (Size - OldUsed) * HugeWordElementSize, 0);
          A.Used := Size;
          exit;
        end;
      // first dynamic allocation
      HugeWordAlloc(A, Size);
      if OldUsed > 0 then
        Move(A.InitBuf[0], A.Data^, OldUsed * HugeWordElementSize);
    end
  else
  if Size > OldAlloc then
    begin
      // expanding block: allocate more memory than requested, this reduces
      // the number of future Realloc calls
      NewAlloc := OldAlloc * 2;
      if NewAlloc < Size then
        NewAlloc := Size;
      HugeWordRealloc(A, NewAlloc);
    end;
  // set expanded elements to zero
  P := A.Data;
  Inc(P, OldUsed * HugeWordElementSize);
  FillChar(P^, (Size - OldUsed) * HugeWordElementSize, 0);
  A.Used := Size;
end;

{ HugeWord SetSizeInBits                                                       }
{   Post: Expanded data is set to zero.                                        }
{         Size in bits is multiple of HugeWordElementBits.                     }
procedure HugeWordSetSizeInBits(var A: HugeWord; const Size: Integer);
begin
  Assert(Size >= 0);

  HugeWordSetSize(A, (Size + HugeWordElementBits - 1) div HugeWordElementBits);
end;

{ HugeWord GetElement                                                          }
{   Pre: Index is 0 based                                                      }
function HugeWordGetElement(const A: HugeWord; const I: Integer): Word32;
var P : PWord32;
begin
  Assert(I < A.Used);
  Assert(I >= 0);

  P := A.Data;
  Inc(P, I);
  Result := P^;
end;

procedure HugeWordSetElement(const A: HugeWord; const I: Integer; const B: Word32);
var P : PWord32;
begin
  Assert(I < A.Used);
  Assert(I >= 0);

  P := A.Data;
  Inc(P, I);
  P^ := B;
end;

function HugeWordGetFirstElementPtr(const A: HugeWord): PHugeWordElement;
begin
  if A.Used = 0 then
    Result := nil
  else
    Result := A.Data;
end;

function HugeWordGetLastElementPtr(const A: HugeWord): PHugeWordElement;
var L : Integer;
begin
  L := A.Used;
  if L = 0 then
    Result := nil
  else
    begin
      Result := A.Data;
      Inc(Result, L - 1);
    end;
end;

{ HugeWord Normalise (helper function)                                         }
{   A 'normalised' HugeWord has no trailing zeros (ie the most significant     }
{   value is non-zero) or it is nil (to represent a value of 0).               }
procedure HugeWordNormalise(var A: HugeWord);
var I, L : Integer;
    P : PWord32;
begin
  L := A.Used;
  if L = 0 then
    exit;
  I := 0;
  P := A.Data;
  Inc(P, L - 1);
  while (I < L) and (P^ = 0) do
    begin
      Dec(P);
      Inc(I);
    end;
  if I = 0 then
    exit;
  HugeWordSetSize(A, L - I);
end;

{ HugeWord Init Zero                                                           }
{   Post:  A is zero                                                           }
{          A normalised                                                        }
procedure HugeWordInitZero(out A: HugeWord);
begin
  HugeWordInit(A);
end;

{ HugeWord Init One                                                            }
{   Post:  A is zero                                                           }
{          A normalised                                                        }
procedure HugeWordInitOne(out A: HugeWord);
begin
  HugeWordInit(A);
  HugeWordAssignOne(A);
end;

procedure HugeWordInitWord32(out A: HugeWord; const B: Word32);
begin
  HugeWordInit(A);
  HugeWordAssignWord32(A, B);
end;

procedure HugeWordInitInt32(out A: HugeWord; const B: Int32);
begin
  HugeWordInit(A);
  HugeWordAssignInt32(A, B);
end;

procedure HugeWordInitInt64(out A: HugeWord; const B: Int64);
begin
  HugeWordInit(A);
  HugeWordAssignInt64(A, B);
end;

procedure HugeWordInitDouble(out A: HugeWord; const B: Double);
begin
  HugeWordInit(A);
  HugeWordAssignDouble(A, B);
end;

procedure HugeWordInitHugeWord(out A: HugeWord; const B: HugeWord);
var L : Integer;
begin
  HugeWordInit(A);
  L := B.Used;
  if L = 0 then
    exit;
  HugeWordSetSize(A, L);
  Move(B.Data^, A.Data^, L * HugeWordElementSize);
end;

procedure HugeWordAssignZero(var A: HugeWord);
begin
  HugeWordSetSize(A, 0);
end;

procedure HugeWordAssignOne(var A: HugeWord);
begin
  HugeWordSetSize_NoZeroMem(A, 1);
  PWord32(A.Data)^ := 1;
end;

procedure HugeWordAssignWord32(var A: HugeWord; const B: Word32);
begin
  if B = 0 then
    HugeWordAssignZero(A)
  else
    begin
      HugeWordSetSize_NoZeroMem(A, 1);
      PWord32(A.Data)^ := B;
    end;
end;

procedure HugeWordAssignInt32(var A: HugeWord; const B: Int32);
begin
  {$IFOPT R+}
  if B < 0 then
    RaiseRangeError else
  {$ENDIF}
  if B = 0 then
    HugeWordAssignZero(A)
  else
    begin
      HugeWordSetSize_NoZeroMem(A, 1);
      PWord32(A.Data)^ := Word32(B);
    end;
end;

{ HugeWord AssignInt64                                                         }
{   Post:  A normalised                                                        }
procedure HugeWordAssignInt64(var A: HugeWord; const B: Int64);
var P : PWord32;
begin
  {$IFOPT R+}
  if B < 0 then
    RaiseRangeError else
  {$ENDIF}
  if Int64Rec(B).Hi = 0 then
    HugeWordAssignWord32(A, Int64Rec(B).Lo)
  else
    begin
      HugeWordSetSize_NoZeroMem(A, 2);
      P := A.Data;
      P^ := Int64Rec(B).Lo;
      Inc(P);
      P^ := Int64Rec(B).Hi;
      HugeWordNormalise(A);
    end;
end;

procedure HugeWordAssignDouble(var A: HugeWord; const B: Double);
var C, D, E : Double;
    V : Word32;
    L, I : Integer;
    P : PWord32;
begin
  if Abs(Frac(B)) > 1.0E-10 then
    RaiseConvertError;
  if B < -1.0E-10 then
    RaiseConvertError;
  L := 0;
  C := Abs(B);
  while C >= 1.0 do
    begin
      C := C / 4294967296.0;
      Inc(L);
    end;
  HugeWordSetSize(A, L);
  if L = 0 then
    exit;
  P := A.Data;
  C := Abs(B);
  for I := 0 to L - 1 do
    begin
      D := C / 4294967296.0;
      E := C - Trunc(D) * 4294967296.0;
      V := Word32(Trunc(E));
      P^ := V;
      Inc(P);
      C := D;
    end;
end;

procedure HugeWordAssign(var A: HugeWord; const B: HugeWord);
var L : Integer;
begin
  L := B.Used;
  HugeWordSetSize_NoZeroMem(A, L);
  if L = 0 then
    exit;
  Move(B.Data^, A.Data^, L * HugeWordElementSize);
end;

{ HugeWord Assign HugeInt Abs                                                  }
procedure HugeWordAssignHugeIntAbs(var A: HugeWord; const B: HugeInt);
begin
  if B.Sign = 0 then
    HugeWordAssignZero(A)
  else
    HugeWordAssign(A, B.Value);
end;

procedure HugeWordAssignBuf(var A: HugeWord; const Buf; const BufSize: Integer;
          const ReverseByteOrder: Boolean);
var L, I : Integer;
    P, Q : PByte;
begin
  if BufSize <= 0 then
    HugeWordAssignZero(A)
  else
    begin
      L := (BufSize + HugeWordElementSize - 1) div HugeWordElementSize;
      HugeWordSetSize_NoZeroMem(A, L);
      P := @Buf;
      Q := A.Data;
      if ReverseByteOrder then
        Inc(P, BufSize - 1);
      for I := 0 to BufSize - 1 do
        begin
          Q^ := P^;
          Inc(Q);
          if ReverseByteOrder then
            Dec(P)
          else
            Inc(P);
        end;
      for I := 0 to BufSize mod 4 - 1 do
        begin
          Q^ := 0;
          Inc(Q);
        end;
    end;
end;

procedure HugeWordAssignBufStrB(var A: HugeWord; const Buf: RawByteString;
          const ReverseByteOrder: Boolean);
var L : Integer;
begin
  L := Length(Buf);
  if L = 0 then
    HugeWordAssignZero(A)
  else
    HugeWordAssignBuf(A, Buf[1], L, ReverseByteOrder);
end;

procedure HugeWordSwap(var A, B: HugeWord);
var C : HugeWord;
begin
  HugeWordInitHugeWord(C, A);      // C := A
  try
    HugeWordAssign(A, B);          // A := B
    HugeWordAssign(B, C);          // B := C
  finally
    HugeWordFinalise(C);
  end;
end;

{ HugeWord IsZero                                                              }
{   Pre:   A normalised                                                        }
{   Post:  Result is True if A is zero                                         }
function HugeWordIsZero(const A: HugeWord): Boolean;
begin
  Result := A.Used = 0;
end;

{ HugeWord IsOne                                                               }
{   Pre:   A normalised                                                        }
{   Post:  Result is True if A is one                                          }
function HugeWordIsOne(const A: HugeWord): Boolean;
begin
  if A.Used <> 1 then
    Result := False
  else
    Result := PWord32(A.Data)^ = 1;
end;

{ HugeWord IsTwo                                                               }
{   Pre:   A normalised                                                        }
{   Post:  Result is True if A is two                                          }
function HugeWordIsTwo(const A: HugeWord): Boolean;
begin
  if A.Used <> 1 then
    Result := False
  else
    Result := PWord32(A.Data)^ = 2;
end;

{ HugeWord IsOdd                                                               }
{   Pre:   A normalised                                                        }
{   Post:  Result is True if A is odd                                          }
function HugeWordIsOdd(const A: HugeWord): Boolean;
begin
  if A.Used = 0 then
    Result := False
  else
    Result := PWord32(A.Data)^ and 1 = 1;
end;

{ HugeWord IsEven                                                              }
{   Pre:   A normalised                                                        }
{   Post:  Result is True if A is even (zero is even)                          }
function HugeWordIsEven(const A: HugeWord): Boolean;
begin
  if A.Used = 0 then
    Result := True
  else
    Result := PWord32(A.Data)^ and 1 = 0;
end;

{ HugeWord Word32 range checking                                               }
{   Pre:   A normalised                                                        }
{   Post:  Result is True if A is in 32-bit word range                         }
function HugeWordIsWord32Range(const A: HugeWord): Boolean;
begin
  Result := (A.Used <= 1);
end;

{ HugeWord Word64 range checking                                               }
{   Pre:   A normalised                                                        }
{   Post:  Result is True if A is in 64-bit word range                         }
function HugeWordIsWord64Range(const A: HugeWord): Boolean;
begin
  Result := (A.Used <= 2);
end;

{ HugeWord Word128 range checking                                              }
{   Pre:   A normalised                                                        }
{   Post:  Result is True if A is in 128-bit word range                        }
function HugeWordIsWord128Range(const A: HugeWord): Boolean;
begin
  Result := (A.Used <= 4);
end;

{ HugeWord Word256 range checking                                              }
{   Pre:   A normalised                                                        }
{   Post:  Result is True if A is in 128-bit word range                        }
function HugeWordIsWord256Range(const A: HugeWord): Boolean;
begin
  Result := (A.Used <= 8);
end;

{ HugeWord Int32 range checking                                                }
{   Pre:   A normalised                                                        }
{   Post:  Result is True if A is in Int32 range                               }
function HugeWordIsInt32Range(const A: HugeWord): Boolean;
var L : Integer;
begin
  L := A.Used;
  if L = 0 then
    Result := True else
  if L > 1 then
    Result := False
  else
    Result := PWord32(A.Data)^ < $80000000;
end;

{ HugeWord Int64 range checking                                                }
{   Pre:   A normalised                                                        }
{   Post:  Result is True if A is in Int64 range                               }
function HugeWordIsInt64Range(const A: HugeWord): Boolean;
var L : Integer;
    P : PWord32;
begin
  L := A.Used;
  if L <= 1 then
    Result := True else
  if L > 2 then
    Result := False
  else
    begin
      P := A.Data;
      Inc(P);
      Result := P^ < $80000000;
    end;
end;

{ HugeWord Int128 range checking                                               }
{   Pre:   A normalised                                                        }
{   Post:  Result is True if A is in Int128 range                              }
function HugeWordIsInt128Range(const A: HugeWord): Boolean;
var L : Integer;
    P : PWord32;
begin
  L := A.Used;
  if L <= 1 then
    Result := True else
  if L > 4 then
    Result := False
  else
    begin
      P := A.Data;
      Inc(P, 3);
      Result := P^ < $80000000;
    end;
end;

{ HugeWord Int256 range checking                                               }
{   Pre:   A normalised                                                        }
{   Post:  Result is True if A is in Int256 range                              }
function HugeWordIsInt256Range(const A: HugeWord): Boolean;
var L : Integer;
    P : PWord32;
begin
  L := A.Used;
  if L <= 1 then
    Result := True else
  if L > 8 then
    Result := False
  else
    begin
      P := A.Data;
      Inc(P, 7);
      Result := P^ < $80000000;
    end;
end;

function HugeWordToWord32(const A: HugeWord): Word32;
var L : Integer;
begin
  L := A.Used;
  {$IFOPT R+}
  if L > 1 then
    RaiseRangeError;
  {$ENDIF};
  if L = 0 then
    Result := 0
  else
    Result := PWord32(A.Data)^;
end;

function HugeWordToInt32(const A: HugeWord): Int32;
var L : Integer;
begin
  L := A.Used;
  {$IFOPT R+}
  if L > 1 then
    RaiseRangeError;
  if L > 0 then
    if PWord32(A.Data)^ >= $80000000 then
      RaiseRangeError;
  {$ENDIF};
  if L = 0 then
    Result := 0
  else
    Result := PInt32(A.Data)^;
end;

function HugeWordToInt64(const A: HugeWord): Int64;
var L : Integer;
    P : PWord32;
begin
  L := A.Used;
  if L = 0 then
    begin
      Result := 0;
      exit;
    end;
  if L = 1 then
    begin
      Result := PWord32(A.Data)^;
      exit;
    end;
  {$IFOPT R+}
  if L > 2 then
    RaiseRangeError;
  if L > 1 then
    begin
      P := A.Data;
      Inc(P);
      if P^ >= $80000000 then
        RaiseRangeError;
    end;
  {$ENDIF};
  P := A.Data;
  Int64Rec(Result).Lo := P^;
  Inc(P);
  Int64Rec(Result).Hi := P^;
end;

function HugeWordToDouble(const A: HugeWord): Double;
var L, I : Integer;
    P  : PWord32;
    R, F, T : Double;
{$IFOPT R+}
const
  MaxF = 1.7E+308 / 4294967296.0 / 4294967296.0;
{$ENDIF}
begin
  L := A.Used;
  if L = 0 then
    begin
      Result := 0.0;
      exit;
    end;
  P := A.Data;
  R := P^;
  F := 1.0;
  for I := 0 to L - 2 do
    begin
      Inc(P);
      F := F * 4294967296.0;
      {$IFOPT R+}
      if F >= MaxF then
        RaiseRangeError;
      {$ENDIF}
      T := P^;
      R := R + F * T;
    end;
  Result := R;
end;



{ HugeWord equals Word                                                         }
{   Pre:   A normalised                                                        }
function HugeWordEqualsWord32(const A: HugeWord; const B: Word32): Boolean;
var L : Integer;
begin
  L := A.Used;
  if L = 0 then
    Result := (B = 0) else
  if L = 1 then
    Result := (B = PWord32(A.Data)^)
  else
    Result := False;
end;

{ HugeWord equals Int                                                          }
{   Pre:   A normalised                                                        }
function HugeWordEqualsInt32(const A: HugeWord; const B: Int32): Boolean;
var L : Integer;
begin
  if B < 0 then
    Result := False
  else
    begin
      L := A.Used;
      if L = 0 then
        Result := (B = 0) else
      if L = 1 then
        Result := (PWord32(A.Data)^ = Word32(B))
      else
        Result := False;
    end;
end;

function HugeWordEqualsInt64(const A: HugeWord; const B: Int64): Boolean;
var L : Integer;
    P : PWord32;
begin
  if B < 0 then
    Result := False
  else
    begin
      L := A.Used;
      if L = 0 then
        Result := (B = 0) else
      if L = 1 then
        Result := (PWord32(A.Data)^ = B) else
      if L = 2 then
        begin
          P := A.Data;
          Result := P^ = Int64Rec(B).Lo;
          if not Result then
            exit;
          Inc(P);
          Result := P^ = Int64Rec(B).Hi;
        end
      else
        Result := False;
    end;
end;

{ HugeWord equals                                                              }
{   Pre:   A and B normalised                                                  }
{$IFDEF Pas64}
function HugeWordEquals(const A, B: HugeWord): Boolean;
var L, M, I : Integer;
    P, Q    : PWord32;
    T, U    : PUInt64;
begin
  L := A.Used;
  M := B.Used;
  if L <> M then
    begin
      Result := False;
      exit;
    end;
  P := A.Data;
  Q := B.Data;
  if P = Q then
    begin
      Result := True;
      exit;
    end;
  T := Pointer(P);
  U := Pointer(Q);
  for I := 0 to (L div 2) - 1 do
    begin
      if T^ <> U^ then
        begin
          Result := False;
          exit;
        end;
      Inc(T);
      Inc(U);
    end;
  if L and 1 <> 0 then
    begin
      P := Pointer(T);
      Q := Pointer(U);
      if P^ <> Q^ then
        begin
          Result := False;
          exit;
        end;
    end;
  Result := True;
end;
{$ELSE}
function HugeWordEquals(const A, B: HugeWord): Boolean;
var L, M, I : Integer;
    P, Q : PWord32;
begin
  L := A.Used;
  M := B.Used;
  if L <> M then
    begin
      Result := False;
      exit;
    end;
  P := A.Data;
  Q := B.Data;
  if P = Q then
    begin
      Result := True;
      exit;
    end;
  for I := 0 to L - 1 do
    if P^ <> Q^ then
      begin
        Result := False;
        exit;
      end
    else
      begin
        Inc(P);
        Inc(Q);
      end;
  Result := True;
end;
{$ENDIF}

{ HugeWord Compare Word                                                        }
{   Pre:   A normalised                                                        }
{   Post:  Result is -1 if A < B, 1 if A > B or 0 if A = B                     }
function HugeWordCompareWord32(const A: HugeWord; const B: Word32): Integer;
var L : Integer;
    F : Word32;
begin
  L := A.Used;
  if L = 0 then
    begin
      if B = 0 then
        Result := 0
      else
        Result := -1;
      exit;
    end;
  if L > 1 then
    begin
      Result := 1;
      exit;
    end;
  F := PWord32(A.Data)^;
  if F < B then
    Result := -1 else
  if F > B then
    Result := 1
  else
    Result := 0;
end;

{ HugeWord Compare Int                                                         }
{   Pre:   A normalised                                                        }
{   Post:  Result is -1 if A < B, 1 if A > B or 0 if A = B                     }
function HugeWordCompareInt32(const A: HugeWord; const B: Int32): Integer;
var L : Integer;
    F : Word32;
begin
  if B < 0 then
    begin
      Result := 1;
      exit;
    end;
  L := A.Used;
  if L = 0 then
    begin
      if B = 0 then
        Result := 0
      else
        Result := -1;
      exit;
    end;
  if L > 1 then
    begin
      Result := 1;
      exit;
    end;
  F := PWord32(A.Data)^;
  if F < Word32(B) then
    Result := -1 else
  if F > Word32(B) then
    Result := 1
  else
    Result := 0;
end;

function HugeWordCompareInt64(const A: HugeWord; const B: Int64): Integer;
var L : Integer;
    F, G : Word32;
    P : PWord32;
begin
  if B < 0 then
    begin
      Result := 1;
      exit;
    end;
  L := A.Used;
  case L of
    0 : begin
          if B = 0 then
            Result := 0
          else
            Result := -1;
          exit;
        end;
    1 : if Int64Rec(B).Hi = 0 then
          begin
            F := PWord32(A.Data)^;
            G := Int64Rec(B).Lo;
          end
        else
          begin
            Result := -1;
            exit;
          end;
    2 : begin
          P := A.Data;
          Inc(P);
          F := P^;
          G := Int64Rec(B).Hi;
          if F = G then
            begin
              F := PWord32(A.Data)^;
              G := Int64Rec(B).Lo;
            end;
        end;
  else
    begin
      Result := 1;
      exit;
    end;
  end;
  if F < G then
    Result := -1 else
  if F > G then
    Result := 1
  else
    Result := 0;
end;

{ HugeWord Compare                                                             }
{   Pre:   A and B normalised                                                  }
{   Post:  Result is -1 if A < B, 1 if A > B or 0 if A = B                     }
{$IFDEF Pas64}
function HugeWordCompare_Original(const A, B: HugeWord): Integer;
var I, L, M : Integer;
    F, G    : Word32;
    P, Q    : PWord32;
    T, U    : PUInt64;
    X, Y    : UInt64;
begin
  L := A.Used;
  M := B.Used;
  if L > M then
    Result := 1 else
  if L < M then
    Result := -1
  else
    begin
      P := A.Data;
      Q := B.Data;
      if P = Q then
        begin
          Result := 0;
          exit;
        end;
      Inc(P, L);
      Inc(Q, L);
      T := Pointer(P);
      U := Pointer(Q);
      for I := (L div 2) - 1 downto 0 do
        begin
          Dec(T);
          Dec(U);
          X := T^;
          Y := U^;
          if X <> Y then
            begin
              if X < Y then
                Result := -1
              else
                Result := 1;
              exit;
            end;
        end;
      if L mod 2 = 1 then
        begin
          P := A.Data;
          Q := B.Data;
          F := P^;
          G := Q^;
          if F <> G then
            begin
              if F < G then
                Result := -1
              else
                Result := 1;
              exit;
            end;
        end;
      Result := 0;
    end;
end;

function HugeWordCompare(const A, B: HugeWord): Integer;
var L, M : Integer;
    F, G : Word32;
    P, Q : PWord32;
    T, U : PUInt64;
    X, Y : UInt64;
begin
  L := A.Used;
  M := B.Used;
  if L > M then
    Result := 1 else
  if L < M then
    Result := -1
  else
    begin
      P := A.Data;
      Q := B.Data;
      if P = Q then
        begin
          Result := 0;
          exit;
        end;
      Inc(P, L);
      Inc(Q, L);
      T := Pointer(P);
      U := Pointer(Q);
      while L >= 8 do
        begin
          Dec(T);
          Dec(U);
          X := T^;
          Y := U^;
          if X = Y then
            begin
              Dec(T);
              Dec(U);
              X := T^;
              Y := U^;
              if X = Y then
                begin
                  Dec(T);
                  Dec(U);
                  X := T^;
                  Y := U^;
                  if X = Y then
                    begin
                      Dec(T);
                      Dec(U);
                      X := T^;
                      Y := U^;
                    end;
                end;
            end;
          if X <> Y then
            begin
              if X < Y then
                Result := -1
              else
                Result := 1;
              exit;
            end;
          Dec(L, 8);
        end;
      while L >= 2 do
        begin
          Dec(T);
          Dec(U);
          X := T^;
          Y := U^;
          if X <> Y then
            begin
              if X < Y then
                Result := -1
              else
                Result := 1;
              exit;
            end;
          Dec(L, 2);
        end;
      if L = 1 then
        begin
          P := A.Data;
          Q := B.Data;
          F := P^;
          G := Q^;
          if F <> G then
            begin
              if F < G then
                Result := -1
              else
                Result := 1;
              exit;
            end;
        end;
      Result := 0;
    end;
end;
{$ELSE}
function HugeWordCompare(const A, B: HugeWord): Integer;
var I, L, M : Integer;
    F, G    : Word32;
    P, Q    : PWord32;
begin
  L := A.Used;
  M := B.Used;
  if L > M then
    Result := 1 else
  if L < M then
    Result := -1
  else
    begin
      P := A.Data;
      Q := B.Data;
      if P = Q then
        begin
          Result := 0;
          exit;
        end;
      Inc(P, L);
      Inc(Q, L);
      for I := L - 1 downto 0 do
        begin
          Dec(P);
          Dec(Q);
          F := P^;
          G := Q^;
          if F <> G then
            begin
              if F < G then
                Result := -1
              else
                Result := 1;
              exit;
            end;
        end;
      Result := 0;
    end;
end;
{$ENDIF}

{ HugeWord Min/Max                                                             }
{   Post:  A is minimum/maximum of A and B.                                    }
procedure HugeWordMin(var A: HugeWord; const B: HugeWord);
begin
  if HugeWordCompare(A, B) <= 0 then
    exit;
  HugeWordAssign(A, B);
end;

procedure HugeWordMax(var A: HugeWord; const B: HugeWord);
begin
  if HugeWordCompare(A, B) >= 0 then
    exit;
  HugeWordAssign(A, B);
end;

function HugeWordGetBitCount(const A: HugeWord): Integer;
begin
  Result := HugeWordGetSize(A) * HugeWordElementBits;
end;

{ HugeWord SetBits                                                             }
{   Sets the number of bits in the HugeWord.                                   }
{   Pre:   Bits must be multiple of 32.                                        }
{   Post:  A NOT normalised.                                                   }
procedure HugeWordSetBitCount(var A: HugeWord; const Bits: Integer);
begin
  if Bits mod HugeWordElementBits <> 0 then
    RaiseInvalidOpError;
  HugeWordSetSize(A, Bits div HugeWordElementBits);
end;

{ HugeWord Bit State                                                           }
{   Pre:   B is bit index (0 based)                                            }
function HugeWordIsBitSet(const A: HugeWord; const B: Integer): Boolean;
var L : Integer;
    P : PWord32;
begin
  L := A.Used;
  if (B < 0) or (B >= L * HugeWordElementBits) then
    Result := False
  else
    begin
      P := A.Data;
      Inc(P, B shr 5);
      Result := (P^ and Word32(1 shl (B and $1F)) <> 0);
    end;
end;

{   Pre:   B is in range                                                       }
function HugeWordIsBitSet_IR(const A: HugeWord; const B: Integer): Boolean; {$IFDEF UseInline}inline;{$ENDIF}
var P : PWord32;
begin
  P := A.Data;
  Inc(P, B shr 5);
  Result := (P^ and Word32(1 shl (B and $1F)) <> 0);
end;

procedure HugeWordSetBit(var A: HugeWord; const B: Integer);
var I, L : Integer;
    P : PWord32;
begin
  L := A.Used;
  if B < 0 then
    exit;
  I := B shr 5;
  if I >= L then
    HugeWordSetSize(A, I + 1);
  P := A.Data;
  Inc(P, I);
  P^ := P^ or Word32(1 shl (B and $1F));
end;

procedure HugeWordSetBit0(var A: HugeWord);
var P : PWord32;
begin
  if A.Used > 0 then
    begin
      P := A.Data;
      P^ := P^ or 1;
    end
  else
    HugeWordAssignOne(A);
end;

procedure HugeWordClearBit(var A: HugeWord; const B: Integer);
var I, L : Integer;
    P : PWord32;
begin
  L := A.Used;
  if (B < 0) or (B >= L * HugeWordElementBits) then
    exit;
  I := B shr 5;
  P := A.Data;
  Inc(P, I);
  P^ := P^ and not Word32(1 shl (B and $1F));
end;

procedure HugeWordToggleBit(var A: HugeWord; const B: Integer);
var I, L : Integer;
    P : PWord32;
begin
  L := A.Used;
  if B < 0 then
    exit;
  I := B shr 5;
  if I >= L then
    HugeWordSetSize(A, I + 1);
  P := A.Data;
  Inc(P, I);
  P^ := P^ xor Word32(1 shl (B and $1F));
end;

{ HugeWord Bit Scan                                                            }
{   Post:  Returns index of bit in A, or -1 if none found                      }
function HugeWordSetBitScanForward(const A: HugeWord): Integer;
var P : PWord32;
    V : Word32;
    I : Integer;
    J : Byte;
begin
  P := A.Data;
  for I := 0 to A.Used - 1 do
    begin
      V := P^;
      if V <> 0 then
        for J := 0 to HugeWordElementBits - 1 do
          if V and BitMaskTable32[J] <> 0 then
            begin
              Result := I * HugeWordElementBits + J;
              exit;
            end;
      Inc(P);
    end;
  Result := -1;
end;

const
  Word8SetBitScanReverseLookup: array[Byte] of Integer = (
      -1, 0, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3,
      4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
      5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
      5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
      6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
      6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
      6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
      6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
      7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
      7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
      7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
      7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
      7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
      7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
      7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
      7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7
    );

function Word32SetBitScanReverse(const A: Word32): Integer; {$IFDEF UseInline}inline;{$ENDIF}
begin
  if A and $FFFF0000 <> 0 then
    if A and $FF000000 <> 0 then
      Result := Word8SetBitScanReverseLookup[Byte(A shr 24)] + 24
    else
      Result := Word8SetBitScanReverseLookup[Byte(A shr 16)] + 16
  else
    if A and $0000FF00 <> 0 then
      Result := Word8SetBitScanReverseLookup[Byte(A shr 8)] + 8
    else
      Result := Word8SetBitScanReverseLookup[Byte(A)];
end;

function HugeWordSetBitScanReverse(const A: HugeWord): Integer;
var P : PWord32;
    L : Integer;
    V : Word32;
    I : Integer;
    J : Byte;
begin
  P := A.Data;
  L := A.Used;
  Inc(P, L - 1);
  for I := L - 1 downto 0 do
    begin
      V := P^;
      if V <> 0 then
        begin
          J := Word32SetBitScanReverse(V);
          Result := I * HugeWordElementBits + J;
          exit;
        end;
      Dec(P);
    end;
  Result := -1;
end;

function HugeWordClearBitScanForward(const A: HugeWord): Integer;
var P : PWord32;
    V : Word32;
    I : Integer;
    J : Byte;
begin
  if A.Used = 0 then
    begin
      Result := 0;
      exit;
    end;
  P := A.Data;
  for I := 0 to A.Used - 1 do
    begin
      V := P^;
      if V <> $FFFFFFFF then
        for J := 0 to HugeWordElementBits - 1 do
          if V and BitMaskTable32[J] = 0 then
            begin
              Result := I * HugeWordElementBits + J;
              exit;
            end;
      Inc(P);
    end;
  Result := A.Used * HugeWordElementBits;
end;

function HugeWordClearBitScanReverse(const A: HugeWord): Integer;
var B : Integer;
begin
  if A.Used = 0 then
    begin
      Result := 0;
      exit;
    end;
  B := HugeWordSetBitScanReverse(A);
  if B < 0 then
    Result := A.Used * HugeWordElementBits
  else
    Result := B + 1;
end;

{ HugeWord Bit Shift                                                           }
{   Post: A not normalised                                                     }
{         A's size expanded if required to accommodate result                  }
{         A's size may be reduced                                              }
procedure HugeWordShl(var A: HugeWord; const B: Integer);
var E, I, L, N : Integer;
    C, D       : Byte;
    P, Q, T    : PWord32;
    F          : Word32;
begin
  if B = 0 then
    exit;
  if B < 0 then
    begin
      HugeWordShr(A, -B);
      exit;
    end;
  L := A.Used;
  if L = 0 then
    exit;
  E := B div 32; // number of new full Word32s
  D := B mod 32; // number of new remaining bits
  N := E;
  // check if high bits require additional Word32
  // expand size of A if required 
  if D > 0 then
    begin
      P := A.Data;
      Inc(P, L - 1);
      F := P^ shr (32 - D);
      if F <> 0 then
        Inc(N);
      if N > 0 then
        HugeWordSetSize(A, L + N);
      if F <> 0 then
        begin
          P := A.Data;
          Inc(P, L + N - 1);
          P^ := F;
        end;
    end else
    if N > 0 then
      HugeWordSetSize(A, L + N);
  // shift A
  C := 32 - D;
  P := A.Data;
  Inc(P, L + E - 1); // P = A[L + E - 1]
  for I := L + E - 1 downto E + 1 do
    begin
      Q := A.Data;
      Inc(Q, I - E); // Q = A[I - E]
      T := Q;
      Dec(T);        // T = A[I - E - 1]
      P^ := (Q^ shl D) or (T^ shr C);
      Dec(P);
    end;
  P^ := PWord32(A.Data)^ shl D; // A[E] := A[0] shl D
  Dec(P);
  for I := E - 1 downto 0 do
    begin
      P^ := 0; // A[I] := 0
      Dec(P);
    end;
end;

{$IFDEF _Asm64}
procedure HugeWordShl1(var A: HugeWord); assembler;
asm
end;
{$ELSE}
{$IFDEF Pas64}
procedure HugeWordShl1_Pas64_Original(var A: HugeWord);
var I, L : Integer;
    M : PWord32;
    F : Word32;
    P, Q : PUInt64;
    V, W : UInt64;
begin
  L := A.Used;
  if L = 0 then
    exit;
  M := A.Data;
  Inc(M, L - 1);
  if M^ and $80000000 <> 0 then // A[L - 1] high bit set
    begin
      HugeWordSetSize_NoZeroMem(A, L + 1);
      M := A.Data;
      Inc(M, L);
      M^ := 1; // A[L] := 1
      Dec(M);
    end;
  if L = 1 then
    begin
      M^ := Word32(M^ shl 1);
      exit;
    end;
  Dec(M);
  P := Pointer(M);
  W := P^;
  Q := P;
  Dec(Q);
  for I := (L div 2) - 1 downto 1 do
    begin
      V := Q^;
      // A[I] := (A[I] shl 1) or (A[I - 1] shr 63)
      // P^ := (P^ shl 1) or (Q^ shr 63);
      P^ := (W shl 1) or (V shr 63);
      W := V;
      Dec(P);
      Dec(Q);
    end;
  // P^ := P^ shl 1; // A[0] := A[0] shl 1
  W := W shl 1;
  if L and 1 = 1 then
    begin
      M := Pointer(P);
      Dec(M);
      F := M^;
      M^ := Word32(F shl 1);
      W := W or (F shr 31);
    end;
  P^ := W;
end;

procedure HugeWordShl1(var A: HugeWord);
var N : Integer;
    L : Integer;
    P : PUInt64;
    C : UInt64;
    F : UInt64;
    Q : PUInt32;
    G : UInt32;
begin
  N := A.Used;
  if N = 0 then
    exit;
  P := A.Data;
  C := 0;
  L := N;
  while L >= 8 do
    begin
      F := P^;
      P^ := (F shl 1) or C;
      Inc(P);
      C := F shr 63;

      F := P^;
      P^ := (F shl 1) or C;
      Inc(P);
      C := F shr 63;

      F := P^;
      P^ := (F shl 1) or C;
      Inc(P);
      C := F shr 63;

      F := P^;
      P^ := (F shl 1) or C;
      Inc(P);
      C := F shr 63;

      Dec(L, 8);
    end;
  while L >= 2 do
    begin
      F := P^;
      P^ := (F shl 1) or C;
      C := F shr 63;
      Inc(P);

      Dec(L, 2);
    end;
  if L = 1 then
    begin
      Q := Pointer(P);
      G := Q^;
      Q^ := (G shl 1) or C;
      C := G shr 31;
    end;
  if C = 1 then
    begin
      HugeWordSetSize_NoZeroMem(A, N + 1);
      Q := A.Data;
      Inc(Q, N);
      Q^ := 1;
    end;
end;
{$ELSE}
procedure HugeWordShl1(var A: HugeWord);
var I, L : Integer;
    P, Q : PWord32;
    V, W : Word32;
begin
  L := A.Used;
  if L = 0 then
    exit;
  P := A.Data;
  Inc(P, L - 1);
  if P^ and $80000000 <> 0 then // A[L - 1] high bit set
    begin
      HugeWordSetSize(A, L + 1);
      P := A.Data;
      Inc(P, L);
      P^ := 1; // A[L] := 1
      Dec(P);
    end;
  W := P^;
  Q := P;
  Dec(Q);
  V := Q^;
  for I := L - 1 downto 1 do
    begin
      // A[I] := (A[I] shl 1) or (A[I - 1] shr 31)
      // P^ := (P^ shl 1) or (Q^ shr 31);
      P^ := (W shl 1) or (V shr 31);
      W := V;
      Dec(P);
      Dec(Q);
      V := Q^;
    end;
  // P^ := P^ shl 1; // A[0] := A[0] shl 1
  P^ := W shl 1;
end;
{$ENDIF}
{$ENDIF}

procedure HugeWordShr(var A: HugeWord; const B: Integer);
var E, I, L : Integer;
    C, D    : Byte;
    P, Q, T : PWord32;
    F       : Word32;
begin
  if B = 0 then
    exit;
  if B < 0 then
    begin
      HugeWordShl(A, -B);
      exit;
    end;
  L := A.Used;
  if L = 0 then
    exit;
  if B >= L * 32 then
    begin
      HugeWordAssignZero(A);
      exit;
    end;
  E := B div 32;
  D := B mod 32;
  C := 32 - D;
  P := A.Data; // P = A[0]
  Q := P;
  Inc(Q, E);   // Q = A[E]
  T := Q;
  Inc(T);      // T = A[E + 1]
  for I := 0 to L - E - 2 do
    begin
      // A[I] := (A[I + E] shr D) or (A[I + E + 1] shl C)
      F := (Q^ shr D);
      if C < 32 then // note: T^ shl 32 evaluates to T^ and not 0
        F := F or (T^ shl C);
      P^ := F;
      Inc(P);
      Inc(Q);
      Inc(T);
    end;
  Q := A.Data;
  Inc(Q, L - 1); // Q = A[L - 1]
  P^ := Q^ shr D; // A[L - E - 1] := A[L - 1] shr D
  for I := L - E to L - 1 do
    begin
      Inc(P);
      P^ := 0; // A[I] := 0
    end;
end;

procedure HugeWordShr1_Original(var A: HugeWord);
var I, L : Integer;
    P, Q : PWord32;
    V, W : Word32;
begin
  L := A.Used;
  if L = 0 then
    exit;
  P := A.Data; // P := A[0]
  Q := P;
  Inc(Q);      // Q := A[1]
  W := P^;
  for I := 0 to L - 2 do
    begin
      // A[I] := (A[I] shr 1) or (A[I + 1] shl 31)
      V := W;
      W := Q^;
      P^ := (V shr 1) or (W shl 31);
      // ----
      Inc(P);
      Inc(Q);
    end;
  P^ := W shr 1; // A[L - 1] := A[L - 1] shr 1
end;

procedure HugeWordShr1(var A: HugeWord);
var L    : Integer;
    P, Q : PWord32;
    V, W : Word32;
begin
  L := A.Used;
  if L = 0 then
    exit;
  P := A.Data; // P := A[0]
  Q := P;
  Inc(Q);      // Q := A[1]
  W := P^;
  while L >= 5 do
    begin
      V := W;
      W := Q^;
      P^ := (V shr 1) or (W shl 31);
      Inc(P);
      Inc(Q);

      V := W;
      W := Q^;
      P^ := (V shr 1) or (W shl 31);
      Inc(P);
      Inc(Q);

      V := W;
      W := Q^;
      P^ := (V shr 1) or (W shl 31);
      Inc(P);
      Inc(Q);

      V := W;
      W := Q^;
      P^ := (V shr 1) or (W shl 31);
      Inc(P);
      Inc(Q);

      Dec(L, 4);
    end;
  while L >= 2 do
    begin
      V := W;
      W := Q^;
      P^ := (V shr 1) or (W shl 31);
      Inc(P);
      Inc(Q);

      Dec(L);
    end;
  P^ := W shr 1; // A[L - 1] := A[L - 1] shr 1
end;

{ HugeWord Logical NOT                                                         }
{   Pre:   Any A                                                               }
{   Post:  A's length unchanged                                                }
{          A not normalised                                                    }
procedure HugeWordNot(var A: HugeWord);
var I, L : Integer;
    P : PWord32;
begin
  L := A.Used;
  if L = 0 then
    exit;
  P := A.Data;
  for I := 0 to L - 1 do
    begin
      P^ := not P^;
      Inc(P);
    end;
end;

{ HugeWord Logical OR                                                          }
{   Pre:   Any A and B                                                         }
{   Post:  A as large as largest of A and B                                    }
{          A normalised if A and B was normalised                              }
procedure HugeWordOrHugeWord(var A: HugeWord; const B: HugeWord);
var I, L, M, N : Integer;
    P, Q : PWord32;
begin
  L := A.Used;
  if L = 0 then
    begin
      HugeWordAssign(A, B);
      exit;
    end;
  M := B.Used;
  if M = 0 then
    exit;
  if L > M then
    N := L
  else
    N := M;
  if L < N then
    HugeWordSetSize(A, N);
  P := A.Data;
  Q := B.Data;
  for I := 0 to M - 1 do
    begin
      P^ := P^ or Q^;
      Inc(P);
      Inc(Q);
    end;
end;

{ HugeWord Logical AND                                                         }
{   Pre:   Any A and B                                                         }
{   Post:  A as large as smallest of A and B                                   }
{          A not normalised.                                                   }
procedure HugeWordAndHugeWord(var A: HugeWord; const B: HugeWord);
var I, L, M, N : Integer;
    P, Q : PWord32;
begin
  L := A.Used;
  if L = 0 then
    exit;
  M := B.Used;
  if M = 0 then
    begin
      HugeWordAssignZero(A);
      exit;
    end;
  if L < M then
    N := L
  else
    N := M;
  HugeWordSetSize(A, N);
  P := A.Data;
  Q := B.Data;
  for I := 0 to M - 1 do
    begin
      P^ := P^ and Q^;
      Inc(P);
      Inc(Q);
    end;
end;

{ HugeWord Logical XOR                                                         }
{   Pre:   Any A and B                                                         }
{   Post:  A as large as largest of A and B                                    }
{          A not normalised.                                                   }
procedure HugeWordXorHugeWord(var A: HugeWord; const B: HugeWord);
var I, L, M, N : Integer;
    P, Q : PWord32;
begin
  L := A.Used;
  if L = 0 then
    begin
      HugeWordAssign(A, B);
      exit;
    end;
  M := B.Used;
  if M = 0 then
    exit;
  if L > M then
    N := L
  else
    N := M;
  if L < N then
    HugeWordSetSize(A, N);
  P := A.Data;
  Q := B.Data;
  for I := 0 to M - 1 do
    begin
      P^ := P^ xor Q^;
      Inc(P);
      Inc(Q);
    end;
end;

{ HugeWord Add Word                                                            }
{   Pre:   A normalised                                                        }
{   Post:  A contains result of A + B                                          }
{          A normalised                                                        }
procedure HugeWordAddWord32(var A: HugeWord; const B: Word32);
var L, I : Integer;
    R    : Int64;
    C    : Word32;
    P    : PWord32;
begin
  if B = 0 then
    exit;
  L := A.Used;
  if L = 0 then
    begin
      HugeWordAssignWord32(A, B);
      exit;
    end;
  P := A.Data;
  C := B;
  for I := 0 to L - 1 do
    begin
      R := C;
      Inc(R, P^);
      P^ := Int64Rec(R).Lo;
      C := Int64Rec(R).Hi;
      if C = 0 then
        exit;
      Inc(P);
    end;
  HugeWordSetSize(A, L + 1);
  P := A.Data;
  Inc(P, L);
  P^ := C;
end;

{ HugeWord Add                                                                 }
{   Pre:   A and B normalised                                                  }
{   Post:  A contains result of A + B                                          }
{          A normalised                                                        }
procedure HugeWordAdd(var A: HugeWord; const B: HugeWord);
var L, M : Integer;
    I    : Integer;
    R    : UInt64;
    P, Q : PWord32;
begin
  M := B.Used;
  if M = 0 then
    exit;
  L := A.Used;
  if L = 0 then
    begin
      HugeWordAssign(A, B);
      exit;
    end;
  if L < M then
    begin
      HugeWordSetSize(A, M);
      L := M;
    end;
  P := A.Data;
  Q := B.Data;
  R := 0;
  for I := 0 to M - 1 do
    begin
      Inc(R, P^);
      Inc(R, Q^);
      P^ := Word32(R);   // Int64Rec(R).Lo
      R := R shr 32;     // Int64Rec(R).Hi
      Inc(P);
      Inc(Q);
    end;
  if R = 0 then
    exit;
  for I := M to L - 1 do
    begin
      Inc(R, P^);
      P^ := Word32(R);   // Int64Rec(R).Lo;
      R := R shr 32;     // Int64Rec(R).Hi;
      if R = 0 then
        exit;
      Inc(P);
    end;
  HugeWordSetSize(A, L + 1);
  P := A.Data;
  Inc(P, L);
  P^ := R;
end;

procedure HugeWordInc(var A: HugeWord);
begin
  HugeWordAddWord32(A, 1);
end;

{ HugeWord Subtract Word                                                       }
{   Pre:   A normalised                                                        }
{   Post:  A contains result of A - B                                          }
{          A normalised                                                        }
{          Result is sign of A (-1 or +1) or 0 if A is zero                    }
function HugeWordSubtractWord32(var A: HugeWord; const B: Word32): Integer;
var C    : Integer;
    L, I : Integer;
    R    : Int64;
    P    : PWord32;
begin
  L := A.Used;
  // Handle A = 0 or B = 0
  if L = 0 then
    begin
      if B = 0 then
        begin
          Result := 0;
          exit;
        end;
      HugeWordAssignWord32(A, B);
      Result := -1;
      exit;
    end;
  if B = 0 then
    begin
      Result := 1;
      exit;
    end;
  // Handle A = B
  C := HugeWordCompareWord32(A, B);
  if C = 0 then
    begin
      HugeWordAssignZero(A);
      Result := 0;
      exit;
    end;
  // Handle A < B
  if C < 0 then
    begin
      HugeWordAssignWord32(A, B - PWord32(A.Data)^);
      Result := -1;
      exit;
    end;
  // Handle A > B
  // Subtract
  P := A.Data;
  for I := 0 to L - 1 do
    begin
      if I = 0 then
        begin
          R := $100000000;
          Dec(R, B);
        end
      else
        R := $FFFFFFFF;
      Inc(R, P^);
      P^ := Int64Rec(R).Lo;
      if Int64Rec(R).Hi > 0 then
        break;
      Inc(P);
    end;
  // Normalise
  HugeWordNormalise(A);
  // Return sign
  if HugeWordIsZero(A) then
    Result := 0
  else
    Result := 1;
end;

{ HugeWord Subtract (A Larger)                                                 }
{   Pre:   A and B normalised                                                  }
{          A is larger than B                                                  }
{          A is not zero, B is not zero                                        }
{   Post:  A contains result of A - B                                          }
{          A normalised                                                        }
procedure HugeWordSubtract_ALarger(var A: HugeWord; const B: HugeWord);
var
  L, M : Integer;
  R    : UInt64;
  I    : Integer;
  P, Q : PWord32;
begin
  Assert(A.Used > 0);
  Assert(B.Used > 0);
  Assert(A.Used >= B.Used);

  // Subtract
  P := A.Data;
  Q := B.Data;
  L := A.Used;
  M := B.Used;
  R := 1;
  I := M;
  while I >= 2 do
    begin
      Inc(R, $FFFFFFFF);
      Inc(R, P^);
      Dec(R, Q^);
      P^ := Word32(R);
      R := R shr 32;
      Inc(P);
      Inc(Q);

      Inc(R, $FFFFFFFF);
      Inc(R, P^);
      Dec(R, Q^);
      P^ := Word32(R);
      R := R shr 32;
      Inc(P);
      Inc(Q);

      Dec(I, 2);
    end;
  if I = 1 then
    begin
      Inc(R, $FFFFFFFF);
      Inc(R, P^);
      Dec(R, Q^);
      P^ := Word32(R);
      Inc(P);
      R := R shr 32;
      Dec(I);
    end;
  Assert(I = 0);
  if R = 0 then // borrowed
    begin
      I := L - M;
      while I > 0 do
        begin
          R := $FFFFFFFF;
          Inc(R, P^);
          P^ := Word32(R);
          R := R shr 32;
          if R > 0 then
            break; // not borrowed
          Inc(P);
          Dec(I);
        end;
    end;
  Assert(R = 1);
  // Normalise (leading zeros)
  HugeWordNormalise(A);
end;

{ HugeWord Subtract                                                            }
{   Pre:   A and B normalised                                                  }
{   Post:  A contains result of A - B                                          }
{          A normalised                                                        }
{          Result is sign of A (-1 or +1) or 0 if A is zero                    }
function HugeWordSubtract(var A: HugeWord; const B: HugeWord): Integer;
var C       : Integer;
    D, E    : PHugeWord;
    L, M    : Integer;
    R       : UInt64;
    I       : Integer;
    P, Q, Z : PWord32;
begin
  // Handle A = 0 or B = 0
  if HugeWordIsZero(A) then
    begin
      if HugeWordIsZero(B) then
        begin
          Result := 0;
          exit;
        end;
      HugeWordAssign(A, B);
      Result := -1;
      exit;
    end;
  if HugeWordIsZero(B) then
    begin
      Result := 1;
      exit;
    end;
  // Handle A = B
  C := HugeWordCompare(A, B);
  if C = 0 then
    begin
      HugeWordAssignZero(A);
      Result := 0;
      exit;
    end;
  // Swap around if A < B
  if C > 0 then
    begin
      D := @A;
      E := @B;
    end
  else
    begin
      HugeWordSetSize(A, B.Used);
      D := @B;
      E := @A;
    end;
  // Subtract
  P := D^.Data;
  Q := E^.Data;
  Z := A.Data;
  L := D^.Used;
  M := E^.Used;
  R := 1;
  I := M;
  while I >= 2 do
    begin
      Inc(R, $FFFFFFFF);
      Inc(R, P^);
      Dec(R, Q^);
      Z^ := Word32(R);
      R := R shr 32;
      Inc(P);
      Inc(Q);
      Inc(Z);

      Inc(R, $FFFFFFFF);
      Inc(R, P^);
      Dec(R, Q^);
      Z^ := Word32(R);
      R := R shr 32;
      Inc(P);
      Inc(Q);
      Inc(Z);

      Dec(I, 2);
    end;
  if I = 1 then
    begin
      Inc(R, $FFFFFFFF);
      Inc(R, P^);
      Dec(R, Q^);
      Z^ := Word32(R);
      R := R shr 32;
      Inc(P);
      Inc(Z);
    end;
  if R = 0 then // borrowed
    begin
      I := L - M;
      while I > 0 do
        begin
          R := $FFFFFFFF;
          Inc(R, P^);
          Z^ := Word32(R);
          R := R shr 32;
          if R > 0 then
            break; // not borrowed
          Inc(P);
          Inc(Z);
          Dec(I);
        end;
    end;
  Assert(R = 1);
  // Normalise (leading zeros)
  HugeWordNormalise(A);
  // Return sign
  if C > 0 then
    Result := 1
  else
    Result := -1;
end;

procedure HugeWordDec(var A: HugeWord);
begin
  {$IFOPT R+}
  if HugeWordIsZero(A) then
    RaiseRangeError;
  {$ENDIF}
  HugeWordSubtractWord32(A, 1);
end;

{ HugeWord Multiply Word                                                       }
{   Pre:   A and B normalised                                                  }
{   Post:  Result contains A * B                                               }
{          Result NOT normalised. Result has size of Length(A) + Length(B)     }
procedure HugeWordMultiplyWord8(var A: HugeWord; const B: Byte);
var I, L : Integer;
    C    : Int64;
    P    : PWord32;
begin
  L := A.Used;
  if L = 0 then
    exit;
  if B = 0 then
    begin
      HugeWordAssignZero(A);
      exit;
    end;
  if B = 1 then
    exit;
  if B = 2 then
    begin
      HugeWordShl1(A);
      exit;
    end;
  P := A.Data;
  C := Int64(P^) * B;
  P^ := Int64Rec(C).Lo;
  for I := 1 to L - 1 do
    begin
      Inc(P);
      C := C shr 32;
      C := C + (Int64(P^) * B);
      // P^ := Int64Rec(C).Lo;
      P^ := Word32(C);
    end;
  C := C shr 32;
  if C > 0 then
    begin
      HugeWordSetSize(A, L + 1);
      P := A.Data;
      Inc(P, L);
      // P^ := Int64Rec(C).Lo;
      P^ := Word32(C);
    end;
end;

procedure HugeWordMultiplyWord16(var A: HugeWord; const B: Word);
var I, L : Integer;
    C    : Int64;
    P    : PWord32;
begin
  L := A.Used;
  if L = 0 then
    exit;
  if B = 0 then
    begin
      HugeWordAssignZero(A);
      exit;
    end;
  if B = 1 then
    exit;
  P := A.Data;
  C := Int64(P^) * B;
  P^ := Int64Rec(C).Lo;
  for I := 1 to L - 1 do
    begin
      Inc(P);
      C := C shr 32;
      C := C + (Int64(P^) * B);
      // P^ := Int64Rec(C).Lo;
      P^ := Word32(C);
    end;
  C := C shr 32;
  if C > 0 then
    begin
      HugeWordSetSize(A, L + 1);
      P := A.Data;
      Inc(P, L);
      // P^ := Int64Rec(C).Lo;
      P^ := Word32(C);
    end;
end;

procedure HugeWordMultiplyWord32(var A: HugeWord; const B: Word32);
var F : HugeWord;
begin
  HugeWordInitWord32(F, B);
  HugeWordMultiply(A, A, F);
  HugeWordFinalise(F);
end;

{$IFDEF ASM386_DELPHI}
type
  Word64 = Int64;

procedure Word32MultiplyWord32(const A, B: Word32; var R: Word64);
asm
    // EAX = A, EDX = B, [ECX] = R
    mul edx
    mov [ecx], eax
    mov [ecx + 4], edx
end;
{$ENDIF}

{ HugeWord Long Multiplication                                                 }
{   Multiply using the 'long multiplication' method. Time is o(n^2).           }
{   Pre:   Res initialised, A and B normalised                                 }
{          Res is not the same instance as A or B                              }
{   Post:  Res contains A * B                                                  }
{          Res NOT normalised. Res has size of Size(A) + Size(B)               }
{$IFOPT Q+}{$DEFINE QOn}{$Q-}{$ELSE}{$UNDEF QOn}{$ENDIF}
procedure HugeWordMultiply_Long_NN_Unsafe(var Res: HugeWord; const A, B: HugeWord);
var L, M : Integer;
    I, J : Integer;
    R    : Word64;
    {$IFDEF ASM386_DELPHI}
    TM   : Int64;
    V    : Word32;
    {$ELSE}
    V    : UInt64;
    {$ENDIF}
    P, Q : PWord32;
    F    : PWord32;
    ADat : PWord32;
    RDat : PWord32;
begin
  Assert(Res.Data <> A.Data);
  Assert(Res.Data <> B.Data);
  // handle zero
  L := A.Used;
  M := B.Used;
  if (L = 0) or (M = 0) then
    begin
      HugeWordAssignZero(Res);
      exit;
    end;
  // handle one and two
  if (L = 1) or (M = 1) then
    begin
      if L = 1 then
        if HugeWordIsOne(A) then
          begin
            HugeWordAssign(Res, B);
            exit;
          end;
      if M = 1 then
        if HugeWordIsOne(B) then
          begin
            HugeWordAssign(Res, A);
            exit;
          end;
      if L = 1 then
        if HugeWordIsTwo(A) then
          begin
            HugeWordAssign(Res, B);
            HugeWordShl1(Res);
            exit;
          end;
      if M = 1 then
        if HugeWordIsTwo(B) then
          begin
            HugeWordAssign(Res, A);
            HugeWordShl1(Res);
            exit;
          end;
    end;
  // multiply
  HugeWordAssignZero(Res);
  HugeWordSetSize(Res, L + M);
  ADat := A.Data;
  Q := B.Data;
  RDat := Res.Data;
  for I := 0 to M - 1 do
    begin
      V := Q^;
      R := 0;
      P := ADat;
      F := RDat;
      Inc(F, I);
      J := L;
      while J > 0 do
        begin
          Inc(R, F^);
          {$IFDEF FREEPASCAL}
          R := R + (V * P^);
          {$ELSE}
            {$IFDEF ASM386_DELPHI}
            Word32MultiplyWord32(Word32(V), P^, TM);
            Inc(R, TM);
            {$ELSE}
              {$IFDEF CPU_X86_64}
              R := R + V * P^;
              {$ELSE}
              Inc(R, V * P^);
              {$ENDIF}
            {$ENDIF}
          {$ENDIF}
          F^ := Word32(R);
          R := R shr 32;
          Inc(P);
          Inc(F);
          Dec(J);
        end;
      F^ := Word32(R);
      Inc(Q);
    end;
end;
{$IFDEF QOn}{$Q+}{$ENDIF}

procedure HugeWordMultiply_Long_NN_Safe(var Res: HugeWord; const A, B: HugeWord);
var T : HugeWord;
begin
  HugeWordInit(T);
  try
    HugeWordMultiply_Long_NN_Unsafe(T, A, B);
    HugeWordAssign(Res, T);
  finally
    HugeWordFinalise(T);
  end;
end;

procedure HugeWordMultiply_Long_NN(var Res: HugeWord; const A, B: HugeWord);
begin
  if (Res.Data = A.Data) or (Res.Data = B.Data) then
    HugeWordMultiply_Long_NN_Safe(Res, A, B)
  else
    HugeWordMultiply_Long_NN_Unsafe(Res, A, B);
end;

procedure HugeWordMultiply_Long(var Res: HugeWord; const A, B: HugeWord);
begin
  HugeWordMultiply_Long_NN(Res, A, B);
  HugeWordNormalise(Res);
end;

{ HugeWord Shift-Add Multiplication                                            }
{   Multiply using the 'shift-add' method:                                     }
{     + Set the Result to 0                                                    }
{     + For each bit of the Multiplier (from right to left) do                 }
{     +   If the bit is 1 then add the Multiplicand to the Result              }
{     +   Shift the Multiplicand left one bit                                  }
{   Pre:   Res initialised, A and B normalised                                 }
{          Res may be same instance as A or B                                  }
{   Post:  Res contains A * B                                                  }
{          Res normalised.                                                     }
procedure HugeWordMultiply_ShiftAdd_Unsafe(var Res: HugeWord; const A, B: HugeWord);
var L, M, C, I : Integer;
    D : HugeWord;
    AP : PWord32;
    AD : Word32;
    AB : Word32;
begin
  // handle zero
  L := A.Used;
  M := B.Used;
  if (L = 0) or (M = 0) then
    begin
      HugeWordAssignZero(Res);
      exit;
    end;
  // handle one and two
  if (L = 1) or (M = 1) then
    begin
      if L = 1 then
        if HugeWordIsOne(A) then
          begin
            HugeWordAssign(Res, B);
            exit;
          end;
      if M = 1 then
        if HugeWordIsOne(B) then
          begin
            HugeWordAssign(Res, A);
            exit;
          end;
      if L = 1 then
        if HugeWordIsTwo(A) then
          begin
            HugeWordAssign(Res, B);
            HugeWordShl1(Res);
            exit;
          end;
      if M = 1 then
        if HugeWordIsTwo(B) then
          begin
            HugeWordAssign(Res, A);
            HugeWordShl1(Res);
            exit;
          end;
    end;
  // multiply
  HugeWordAssignZero(Res);
  HugeWordInitHugeWord(D, B);
  try
    C := HugeWordSetBitScanReverse(A);
    AP := A.Data;
    AD := 0;
    AB := $80000000;
    for I := 0 to C do
      begin
        if AB = $80000000 then
          begin
            AD := AP^;
            Inc(AP);
            AB := 1;
          end
        else
          AB := AB shl 1;
        if AD and AB <> 0 then //if HugeWordIsBitSet_IR(A, I) then
          HugeWordAdd(Res, D);
        HugeWordShl1(D);
      end;
  finally
    HugeWordFinalise(D);
  end;
end;

procedure HugeWordMultiply_ShiftAdd_Safe(var Res: HugeWord; const A, B: HugeWord);
var R : HugeWord;
begin
  HugeWordInitZero(R);
  try
    HugeWordMultiply_ShiftAdd_Unsafe(R, A, B);
    HugeWordAssign(Res, R);
  finally
    HugeWordFinalise(R);
  end;
end;

procedure HugeWordMultiply_ShiftAdd(var Res: HugeWord; const A, B: HugeWord);
begin
  if (Res.Data = A.Data) or (Res.Data = B.Data) then
    HugeWordMultiply_ShiftAdd_Safe(Res, A, B)
  else
    HugeWordMultiply_ShiftAdd_Unsafe(Res, A, B);
end;

(*
procedure karatsuba(num1, num2)
  if (num1 < 10) or (num2 < 10)
    return num1*num2

  /* calculates the size of the numbers */
  m = min(size_base10(num1), size_base10(num2))
  m2 = floor(m/2)
  /*m2 = ceil(m/2) will also work */

  /* split the digit sequences in the middle */
  high1, low1 = split_at(num1, m2)
  high2, low2 = split_at(num2, m2)

  /* 3 calls made to numbers approximately half the size */
  z0 = karatsuba(low1, low2)
  z1 = karatsuba((low1 + high1), (low2 + high2))
  z2 = karatsuba(high1, high2)

  return (z2 * 10 ^ (m2 * 2)) + ((z1 - z2 - z0) * 10 ^ m2) + z0
*)
procedure HugeWordMultiply_Karatsuba(var Res: HugeWord; const A, B: HugeWord);
var
  M, M2 : Integer;
  Hi1, Lo1 : HugeWord;
  Hi2, Lo2 : HugeWord;
  P : PWord32;
  Z0, Z1, Z2 : HugeWord;
  R1, R2 : HugeWord;
begin
  if (A.Used <= 4) or (B.Used <= 4) then
    begin
      HugeWordMultiply_Long(Res, A, B);
      exit;
    end;
  if B.Used > A.Used then
    M := B.Used
  else
    M := A.Used;
  M2 := M div 2;

  HugeWordInit(Hi1);
  HugeWordInit(Lo1);
  HugeWordSetSize(Hi1, A.Used - M2);
  HugeWordSetSize(Lo1, M2);
  P := A.Data;
  Move(P^, Lo1.Data^, Lo1.Used * 4);
  Inc(P, Lo1.Used);
  Move(P^, Hi1.Data^, Hi1.Used * 4);

  HugeWordInit(Hi2);
  HugeWordInit(Lo2);
  HugeWordSetSize(Hi2, B.Used - M2);
  HugeWordSetSize(Lo2, M2);
  P := B.Data;
  Move(P^, Lo2.Data^, Lo2.Used * 4);
  Inc(P, Lo2.Used);
  Move(P^, Hi2.Data^, Hi2.Used * 4);

  HugeWordInit(Z0);
  HugeWordInit(Z1);
  HugeWordInit(Z2);

  // z0 = karatsuba(low1, low2)
  HugeWordMultiply_Karatsuba(Z0, Lo1, Lo2);
  // z2 = karatsuba(high1, high2)
  HugeWordMultiply_Karatsuba(Z2, Hi1, Hi2);
  // z1 = karatsuba((low1 + high1), (low2 + high2))
  HugeWordAdd(Lo1, Hi1);
  HugeWordAdd(Lo2, Hi2);
  HugeWordMultiply_Karatsuba(Z1, Lo1, Lo2);

  // return (z2 * 10 ^ (m2 * 2)) + ((z1 - z2 - z0) * 10 ^ m2) + z0
  HugeWordInit(R1);
  HugeWordSetSize(R1, Z2.Used + M2 * 2);
  P := R1.Data;
  Inc(P, M2 * 2);
  Move(Z2.Data^, P^, Z2.Used * 4);
  FillChar(R1.Data^, M2 * 2 * 4, 0);
  HugeWordNormalise(R1);

  HugeWordInit(R2);
  HugeWordSetSize(R2, Z1.Used + M2);
  P := R2.Data;
  Inc(P, M2);
  Move(Z1.Data^, P^, Z1.Used * 4);
  FillChar(R2.Data^, M2 * 4, 0);
  HugeWordNormalise(R2);

  HugeWordAdd(R1, R2);

  HugeWordSetSize(R2, Z2.Used + M2);
  P := R2.Data;
  Inc(P, M2);
  Move(Z2.Data^, P^, Z2.Used * 4);
  FillChar(R2.Data^, M2 * 4, 0);
  HugeWordNormalise(R2);

  HugeWordSubtract(R1, R2);

  HugeWordSetSize(R2, Z0.Used + M2);
  P := R2.Data;
  Inc(P, M2);
  Move(Z0.Data^, P^, Z0.Used * 4);
  FillChar(R2.Data^, M2 * 4, 0);
  HugeWordNormalise(R2);

  HugeWordSubtract(R1, R2);

  HugeWordAdd(R1, Z0);

  HugeWordAssign(Res, R1);

  HugeWordFinalise(R2);
  HugeWordFinalise(R1);
  HugeWordFinalise(Z2);
  HugeWordFinalise(Z1);
  HugeWordFinalise(Z0);
  HugeWordFinalise(Lo2);
  HugeWordFinalise(Hi2);
  HugeWordFinalise(Lo1);
  HugeWordFinalise(Hi1);
end;

{ HugeWord Multiplication                                                      }
{   Pre:   A, B normalised                                                     }
{          Res initialised                                                     }
{   Post:  Res contains A * B                                                  }
{          Res normalised                                                      }
{.DEFINE HugeInt_AlwaysMultiplyLong}
{.DEFINE HugeInt_AlwaysMultiplyShiftAdd}
procedure HugeWordMultiply(var Res: HugeWord; const A, B: HugeWord);
begin
  {$IFDEF HugeInt_AlwaysMultiplyLong}
  HugeWordMultiply_Long(Res, A, B);
  {$ELSE}{$IFDEF HugeInt_AlwaysMultiplyShiftAdd}
  HugeWordMultiply_ShiftAdd(Res, A, B);
  {$ELSE}
  HugeWordMultiply_Long(Res, A, B);
  {$ENDIF}{$ENDIF}
end;

{ HugeWord Sqr                                                                 }
{   Pre:   Res initialised, A normalised                                       }
{   Post:  Res contains A * A                                                  }
{          Res normalised                                                      }
procedure HugeWordSqr(var Res: HugeWord; const A: HugeWord);
begin
  HugeWordMultiply(Res, A, A);
end;

{ HugeWord Divide                                                              }
procedure HugeWordDivideWord32(const A: HugeWord; const B: Word32; var Q: HugeWord; out R: Word32);
var C, T : HugeWord;
begin
  HugeWordInitWord32(C, B);
  HugeWordInit(T);
  try
    HugeWordDivide(A, C, Q, T);
    R := HugeWordToWord32(T);
  finally
    HugeWordFinalise(T);
    HugeWordFinalise(C);
  end;
end;

{                                                                              }
{ HugeWord Divide                                                              }
{                                                                              }
{   Divide using the "restoring radix two division" method.                    }
{                                                                              }
{     R := N                                                                   }
{     D := D << n               -- R and D need twice the word width of N and Q  }
{     for i := n - 1 .. 0 do    -- For example 31..0 for 32 bits               }
{       R := 2 * R - D          -- Trial subtraction from shifted value (multiplication by 2 is a shift in binary representation)  }
{       if R >= 0 then                                                         }
{         q(i) := 1             -- Result-bit 1                                }
{       else                                                                   }
{         q(i) := 0             -- Result-bit 0                                }
{         R := R + D            -- New partial remainder is (restored) shifted value  }
{       end                                                                    }
{     end                                                                      }
{                                                                              }
{     Where: N = Numerator, D = Denominator, n = #bits,                        }
{            R = Partial remainder, q(i) = bit #i of quotient                  }
{                                                                              }
procedure HugeWordDivide_RR_Unsafe_Orig(const A, B: HugeWord; var Q, R: HugeWord);
var C, F, D : Integer;
begin
  // Handle special cases
  if HugeWordIsZero(B) then          // B = 0
    RaiseDivByZeroError else
  if HugeWordIsOne(B) then           // B = 1
    begin
      HugeWordAssign(Q, A);          // Q = A
      HugeWordAssignZero(R);         // R = 0
      exit;
    end;
  if HugeWordIsZero(A) then          // A = 0
    begin
      HugeWordAssignZero(Q);         // Q = 0
      HugeWordAssignZero(R);         // R = 0
      exit;
    end;
  C := HugeWordCompare(A, B);
  if C < 0 then                      // A < B
    begin
      HugeWordAssign(R, A);          // R = A
      HugeWordAssignZero(Q);         // Q = 0
      exit;
    end else
  if C = 0 then                      // A = B
    begin
      HugeWordAssignOne(Q);          // Q = 1
      HugeWordAssignZero(R);         // R = 0
      exit;
    end;
  // Divide using "restoring radix two division"
  HugeWordAssignZero(R);             // R = remainder
  HugeWordAssignZero(Q);             // Q = quotient
  F := HugeWordSetBitScanReverse(A); // F = high bit index in dividend
  for C := 0 to F do
    begin
      // Shift high bit of dividend A into low bit of remainder R
      HugeWordShl1(R);
      if HugeWordIsBitSet_IR(A, F - C) then
        HugeWordSetBit0(R);
      // Shift quotient
      HugeWordShl1(Q);
      // Subtract divisor from remainder if large enough
      D := HugeWordCompare(R, B);
      if D >= 0 then
        begin
          if D = 0 then
            HugeWordAssignZero(R)
          else
            HugeWordSubtract_ALarger(R, B);
          // Set result bit in quotient
          HugeWordSetBit0(Q);
        end;
    end;
end;

procedure HugeWordDivide_RR_Unsafe(const A, B: HugeWord; var Q, R: HugeWord);
var C, F, D, G, I : Integer;
begin
  // Handle special cases
  if HugeWordIsZero(B) then // B = 0
    RaiseDivByZeroError else
  if HugeWordIsOne(B) then  // B = 1
    begin
      HugeWordAssign(Q, A);         // Q = A
      HugeWordAssignZero(R);        // R = 0
      exit;
    end;
  if HugeWordIsZero(A) then // A = 0
    begin
      HugeWordAssignZero(Q);        // Q = 0
      HugeWordAssignZero(R);        // R = 0
      exit;
    end;
  C := HugeWordCompare(A, B);
  if C < 0 then             // A < B
    begin
      HugeWordAssign(R, A);         // R = A
      HugeWordAssignZero(Q);        // Q = 0
      exit;
    end else
  if C = 0 then             // A = B
    begin
      HugeWordAssignOne(Q);         // Q = 1
      HugeWordAssignZero(R);        // R = 0
      exit;
    end;
  // Divide using "restoring radix two division"
  HugeWordAssignZero(R);             // R = remainder
  HugeWordAssignZero(Q);             // Q = quotient
  F := HugeWordSetBitScanReverse(A); // F = high bit index in dividend
  G := HugeWordSetBitScanReverse(B); // G = high bit index in divisor
  C := 0;
  // First iteration over G + 1 bits
  Assert(G > 0);
  Assert(G <= F);
  HugeWordSetSize(R, (G + 1 + 31) div 32);
  for I := 0 to G do
    if HugeWordIsBitSet_IR(A, F - I) then
      HugeWordSetBit(R, G - I);
  Inc(C, G + 1);
  D := HugeWordCompare(R, B);
  if D >= 0 then
    begin
      if D = 0 then
        HugeWordAssignZero(R)
      else
        HugeWordSubtract_ALarger(R, B);
      HugeWordSetBit0(Q);
    end;
  // Remaining iterations one bit at a time
  while C <= F do
    begin
      // Shift high bit of dividend D into low bit of remainder R
      HugeWordShl1(R);
      if HugeWordIsBitSet_IR(A, F - C) then
        HugeWordSetBit0(R);
      // Shift quotient
      HugeWordShl1(Q);
      // Subtract divisor from remainder if large enough
      D := HugeWordCompare(R, B);
      if D >= 0 then
        begin
          if D = 0 then
            HugeWordAssignZero(R)
          else
            HugeWordSubtract_ALarger(R, B);
          // Set result bit in quotient
          HugeWordSetBit0(Q);
        end;
      Inc(C);
    end;
end;

procedure HugeWordDivide_RR_Safe(const A, B: HugeWord; var Q, R: HugeWord);
var D, E : HugeWord;
begin
  HugeWordInit(D);
  HugeWordInit(E);
  try
    HugeWordDivide_RR_Unsafe_Orig(A, B, D, E);
    HugeWordAssign(Q, D);
    HugeWordAssign(R, E);
  finally
    HugeWordFinalise(E);
    HugeWordFinalise(D);
  end;
end;

{ HugeWord Divide                                                              }
{   Pre:   Q, R initialised, A, B normalised                                   }
{   Post:  Q is Quotient                                                       }
{          R is Remainder                                                      }
{          Q and R normalised                                                  }
procedure HugeWordDivide(const A, B: HugeWord; var Q, R: HugeWord);
begin
  if (A.Data = Q.Data) or (A.Data = R.Data) or
     (B.Data = Q.Data) or (B.Data = R.Data) then
    HugeWordDivide_RR_Safe(A, B, Q, R)
  else
    HugeWordDivide_RR_Unsafe_Orig(A, B, Q, R);
end;

{ HugeWord Mod                                                                 }
{   Pre:  A and B normalised, R initialised                                    }
{         Length(A) >= Length(B)                                               }
{   Post: R is Remainder when A is divided by B                                }
{         R normalised                                                         }
procedure HugeWordMod_Orig(const A, B: HugeWord; var R: HugeWord);
var Q : HugeWord;
begin
  HugeWordInit(Q);
  try
    HugeWordDivide(A, B, Q, R);
  finally
    HugeWordFinalise(Q);
  end;
end;

procedure HugeWordMod_Alt(const A, B: HugeWord; var R: HugeWord);
var
  Y, Z : HugeWord;
  YP, ZP, TP : PHugeWord;
  C : Integer;
begin
  if HugeWordIsZero(B) then // B = 0
    RaiseDivByZeroError;
  HugeWordAssign(R, A);
  if HugeWordCompare(R, B) > 0 then
    begin
      HugeWordInitHugeWord(Y, B);
      HugeWordInitHugeWord(Z, B);
      YP := @Y;
      ZP := @Z;
      repeat
        TP := YP;
        YP := ZP;
        ZP := TP;
        HugeWordAdd(ZP^, YP^);
      until HugeWordCompare(R, ZP^) <= 0;
      repeat
        if not HugeWordIsZero(YP^) then
          begin
            C := HugeWordCompare(R, YP^);
            if C > 0 then
              HugeWordSubtract_ALarger(R, YP^)
            else
            if C = 0 then
              HugeWordAssignZero(R);
          end;
        TP := YP;
        YP := ZP;
        ZP := TP;
        HugeWordSubtract(YP^, ZP^);
      until HugeWordCompare(YP^, ZP^) > 0;
      HugeWordFinalise(Z);
      HugeWordFinalise(Y);
    end;
end;

procedure HugeWordMod_Unsafe_Orig(const A, B: HugeWord; var R: HugeWord);
var
  I : integer;
  BI : integer;
  C : Integer;
begin
  if HugeWordIsZero(B) then // B = 0
    RaiseDivByZeroError;
  if HugeWordIsOne(B) then  // B = 1
    begin
      HugeWordAssignZero(R);        // R = 0
      exit;
    end;
  if HugeWordIsZero(A) then // A = 0
    begin
      HugeWordAssignZero(R);        // R = 0
      exit;
    end;
  C := HugeWordCompare(A, B);
  if C < 0 then             // A < B
    begin
      HugeWordAssign(R, A);         // R = A
      exit;
    end else
  if C = 0 then             // A = B
    begin
      HugeWordAssignZero(R);        // R = 0
      exit;
    end;
  HugeWordAssignZero(R);
  BI := HugeWordSetBitScanReverse(A);
  for I := BI downto 0 do
    begin
      HugeWordShl1(R);
      if HugeWordIsBitSet_IR(A, I) then
        HugeWordSetBit0(R);
      C := HugeWordCompare(R, B);
      if C > 0 then
        HugeWordSubtract_ALarger(R, B)
      else
      if C = 0 then
        HugeWordAssignZero(R);
    end;
end;

procedure HugeWordMod_Unsafe(const A, B: HugeWord; var R: HugeWord);
var
  I, J : Integer;
  AI, BI : Integer;
  C : Integer;
begin
  if HugeWordIsZero(B) then         // B = 0
    RaiseDivByZeroError;
  if HugeWordIsOne(B) then          // B = 1
    begin
      HugeWordAssignZero(R);        // R = 0
      exit;
    end;
  if HugeWordIsZero(A) then         // A = 0
    begin
      HugeWordAssignZero(R);        // R = 0
      exit;
    end;
  C := HugeWordCompare(A, B);
  if C < 0 then                     // A < B
    begin
      HugeWordAssign(R, A);         // R = A
      exit;
    end else
  if C = 0 then                     // A = B
    begin
      HugeWordAssignZero(R);        // R = 0
      exit;
    end;
  HugeWordAssignZero(R);
  AI := HugeWordSetBitScanReverse(A);
  BI := HugeWordSetBitScanReverse(B);
  // First iteration over BI + 1 bits
  Assert(BI > 0);
  Assert(BI <= AI);
  HugeWordSetSize(R, (BI + 1 + 31) div 32);
  for J := 0 to BI do
    if HugeWordIsBitSet_IR(A, AI - J) then
      HugeWordSetBit(R, BI - J);
  C := HugeWordCompare(R, B);
  if C > 0 then
    HugeWordSubtract_ALarger(R, B)
  else
  if C = 0 then
    HugeWordAssignZero(R);
  I := AI;
  Dec(I, BI + 1);
  // Remaining iterations one bit at a time
  while I >= 0 do
    begin
      HugeWordShl1(R);
      if HugeWordIsBitSet_IR(A, I) then
        HugeWordSetBit0(R);
      C := HugeWordCompare(R, B);
      if C > 0 then
        HugeWordSubtract_ALarger(R, B)
      else
      if C = 0 then
        HugeWordAssignZero(R);
      Dec(I);
    end;
end;

procedure HugeWordMod(const A, B: HugeWord; var R: HugeWord);
var
  T : HugeWord;
begin
  if (A.Data = R.Data) or (B.Data = R.Data) then
    begin
      HugeWordInit(T);
      HugeWordMod_Unsafe(A, B, T);
      HugeWordAssign(R, T);
      HugeWordFinalise(T);
    end
  else
    HugeWordMod_Unsafe(A, B, R);
end;



{ HugeWord GCD                                                                 }
{   Post:  R contains GCD(A, B)                                                }
{   Uses the Euclidean algorithm                                               }
procedure HugeWordGCD(const A, B: HugeWord; var R: HugeWord);
var C, D, T : HugeWord;
begin
  HugeWordInitHugeWord(C, A);
  HugeWordInitHugeWord(D, B);
  HugeWordInit(T);
  try
    while not HugeWordIsZero(D) do
      begin
        HugeWordAssign(T, D);
        HugeWordMod(C, D, D);
        HugeWordAssign(C, T);
      end;
    HugeWordAssign(R, C);
  finally
    HugeWordFinalise(T);
    HugeWordFinalise(D);
    HugeWordFinalise(C);
  end;
end;

{ HugeWord Power operation                                                     }
{ Calculates A^B                                                               }
{   Pre:  A initialised                                                        }
procedure HugeWordPower(var A: HugeWord; const B: Word32);
var R, C : HugeWord;
    D : Word32;
begin
  if B = 0 then
    begin
      HugeWordAssignOne(A);
      exit;
    end;
  if HugeWordIsZero(A) or HugeWordIsOne(A) then
    exit;
  if B = 1 then
    exit;
  if B = 2 then
    begin
      HugeWordSqr(A, A);
      exit;
    end;
  HugeWordInitHugeWord(C, A);
  HugeWordInitOne(R);
  try
    D := B;
    while D > 0 do
      if D and 1 = 0 then
        begin
          HugeWordSqr(C, C);
          D := D shr 1;
        end
      else
        begin
          HugeWordMultiply(R, R, C);
          Dec(D);
        end;
    HugeWordAssign(A, R);
  finally
    HugeWordFinalise(R);
    HugeWordFinalise(C);
  end;
end;

{ HugeWord PowerAndMod                                                         }
{ Calculates A^E mod M (Modular exponentiation)                                }
{   Pseudocode:                                                                }
{      Mod-Exp (a, e, m)                                                       }
{      product = 1                                                             }
{      y = a                                                                   }
{      while e > 0 do                                                          }
{        if e is odd then                                                      }
{          product = (product * y) % m;                                        }
{        endif                                                                 }
{        y = (y * y) % m;                                                      }
{        e = e / 2                                                             }
{      end while                                                               }
{      return product                                                          }
{   Pre:  Res initialised                                                      }
procedure HugeWordPowerAndMod(var Res: HugeWord; const A, E, M: HugeWord);
var P, T, Y, F{, Q} : HugeWord;
begin
  HugeWordInitOne(P);                                  // P = 1
  HugeWordInit(T);
  HugeWordInitHugeWord(Y, A);                          // Y = A
  HugeWordInitHugeWord(F, E);                          // F = E
  //HugeWordInit(Q);
  try
    while not HugeWordIsZero(F) do
      begin
        if HugeWordIsOdd(F) then
          begin
            HugeWordMultiply_Long_NN_Unsafe(T, P, Y);  // T = P * Y             HugeWordMultiply(T, P, Y)
            HugeWordNormalise(T);
            HugeWordMod_Unsafe(T, M, P);
            //HugeWordDivide_RR_Unsafe(T, M, Q, P);      // P = (P * Y) mod M
          end;
        HugeWordMultiply_Long_NN_Unsafe(T, Y, Y);      // T = Y * Y             HugeWordSqr(T, Y)
        HugeWordNormalise(T);
        HugeWordMod_Unsafe(T, M, Y);
        //HugeWordDivide_RR_Unsafe(T, M, Q, Y);          // Y = (Y * Y) mod M
        HugeWordShr1(F);                               // F = F / 2
        HugeWordNormalise(F);
      end;
    HugeWordAssign(Res, P);
  finally
    //HugeWordFinalise(Q);
    HugeWordFinalise(F);
    HugeWordFinalise(Y);
    HugeWordFinalise(T);
    HugeWordFinalise(P);
  end;
end;

{ HugeWord String conversion                                                   }
function HugeWordToStrB(const A: HugeWord): UTF8String;
var B, C : HugeWord;
    D    : Word32;
    S    : RawByteString;
    I, L : Integer;
begin
  if HugeWordIsZero(A) then
    begin
      Result := '0';
      exit;
    end;
  HugeWordInitHugeWord(B, A);
  HugeWordInit(C);
  try
    S := '';
    repeat
      HugeWordDivideWord32(B, 10, C, D);
      S := S + AnsiChar(Byte(D) + Ord('0'));
      HugeWordAssign(B, C);
    until HugeWordIsZero(B);
  finally
    HugeWordFinalise(C);
    HugeWordFinalise(B);
  end;
  L := Length(S);
  SetLength(Result, L);
  for I := 1 to L do
    Result[I] := S[L - I + 1];
end;

function HugeWordToStrU(const A: HugeWord): UnicodeString;
var B, C : HugeWord;
    D    : Word32;
    S    : UnicodeString;
    I, L : Integer;
begin
  if HugeWordIsZero(A) then
    begin
      Result := '0';
      exit;
    end;
  HugeWordInitHugeWord(B, A);
  HugeWordInit(C);
  try
    S := '';
    repeat
      HugeWordDivideWord32(B, 10, C, D);
      S := S + WideChar(Byte(D) + Ord('0'));
      HugeWordAssign(B, C);
    until HugeWordIsZero(B);
  finally
    HugeWordFinalise(C);
    HugeWordFinalise(B);
  end;
  L := Length(S);
  SetLength(Result, L);
  for I := 1 to L do
    Result[I] := S[L - I + 1];
end;

function HugeWordToStr(const A: HugeWord): String;
begin
  {$IFDEF StringIsUnicode}
  Result := HugeWordToStrU(A);
  {$ELSE}
  Result := HugeWordToStrB(A);
  {$ENDIF}
end;

{ HugeWord String conversion                                                   }
procedure StrToHugeWordB(const A: RawByteString; var R: HugeWord);
var I : Integer;
    B : AnsiChar;
    C : Word32;
begin
  if A = '' then
    RaiseConvertError;
  HugeWordAssignZero(R);
  for I := 1 to Length(A) do
    begin
      B := A[I];
      if not (B in ['0'..'9']) then
        RaiseConvertError;
      C := Ord(A[I]) - Ord('0');
      HugeWordMultiplyWord8(R, 10);
      HugeWordAddWord32(R, C);
    end;
end;

procedure StrToHugeWordU(const A: UnicodeString; var R: HugeWord);
var I : Integer;
    B : WideChar;
    C : Word32;
begin
  if A = '' then
    RaiseConvertError;
  HugeWordAssignZero(R);
  for I := 1 to Length(A) do
    begin
      B := A[I];
      if (B < '0') or (B > '9') then
        RaiseConvertError;
      C := Ord(A[I]) - Ord('0');
      HugeWordMultiplyWord8(R, 10);
      HugeWordAddWord32(R, C);
    end;
end;

procedure StrToHugeWord(const A: String; var R: HugeWord);
begin
  {$IFDEF StringIsUnicode}
  StrToHugeWordU(A, R);
  {$ELSE}
  StrToHugeWordB(A, R);
  {$ENDIF}
end;

{ HugeWord Hex conversion                                                      }
const
  HexLookupU = '0123456789ABCDEF';
  HexLookupL = '0123456789abcdef';
  HexLookupU_ByteStr : UTF8String = HexLookupU;
  HexLookupL_ByteStr : UTF8String = HexLookupL;
  HexLookupU_Str : String = HexLookupU;
  HexLookupL_Str : String = HexLookupL;

function HugeWordToHexB(const A: HugeWord; const LowCase: Boolean): UTF8String;
var L, I, J : Integer;
    P : PWord32;
    F : Word32;
begin
  if HugeWordIsZero(A) then
    begin
      Result := '00000000';
      exit;
    end;
  L := A.Used;
  SetLength(Result, L * 8);
  P := A.Data;
  Inc(P, L - 1);
  for I := 0 to L - 1 do
    begin
      F := P^;
      for J := 0 to 7 do
        begin
          if LowCase then
            Result[I * 8 + J + 1] := AnsiChar(HexLookupL[(F shr 28) + 1])
          else
            Result[I * 8 + J + 1] := AnsiChar(HexLookupU[(F shr 28) + 1]);
          F := F shl 4;
        end;
      Dec(P);
    end;
end;

function HugeWordToHex(const A: HugeWord; const LowCase: Boolean): String;
var L, I, J : Integer;
    P : PWord32;
    F : Word32;
begin
  if HugeWordIsZero(A) then
    begin
      Result := '00000000';
      exit;
    end;
  L := A.Used;
  SetLength(Result, L * 8);
  P := A.Data;
  Inc(P, L - 1);
  for I := 0 to L - 1 do
    begin
      F := P^;
      for J := 0 to 7 do
        begin
          if LowCase then
            Result[I * 8 + J + 1] := Char(HexLookupL[(F shr 28) + 1])
          else
            Result[I * 8 + J + 1] := Char(HexLookupU[(F shr 28) + 1]);
          F := F shl 4;
        end;
      Dec(P);
    end;
end;

procedure HexToHugeWordB(const A: RawByteString; var R: HugeWord);
var L, N, C, I, J, K : Integer;
    B : AnsiChar;
    D, E : Byte;
    F : Word32;
begin
  L := Length(A);
  if L = 0 then
    RaiseConvertError;
  // L = number of characters in strings
  N := (L div 2) + (L mod 2);
  // N = number of bytes
  C := (N + 3) div 4;
  // C = number of Word32s
  HugeWordSetSize(R, C);
  for I := 0 to C - 1 do
    begin
      F := 0;
      for J := 0 to 7 do
        begin
          K := L - I * 8 - J;
          if K < 1 then
            B := '0'
          else
            B := A[L - I * 8 - J];
          E := 16;
          for D := 1 to 16 do
            if (B = HexLookupU_ByteStr[D]) or
               (B = HexLookupL_ByteStr[D]) then
              begin
                E := D - 1;
                break;
              end;
          if E = 16 then
            RaiseConvertError;
          F := F or (E shl (J * 4));
        end;
      HugeWordSetElement(R, I, F);
    end;
end;

procedure HexToHugeWord(const A: String; var R: HugeWord);
var L, N, C, I, J, K : Integer;
    B : Char;
    D, E : Byte;
    F : Word32;
begin
  L := Length(A);
  if L = 0 then
    RaiseConvertError;
  // L = number of characters in strings
  N := (L div 2) + (L mod 2);
  // N = number of bytes
  C := (N + 3) div 4;
  // C = number of Word32s
  HugeWordSetSize(R, C);
  for I := 0 to C - 1 do
    begin
      F := 0;
      for J := 0 to 7 do
        begin
          K := L - I * 8 - J;
          if K < 1 then
            B := '0'
          else
            B := A[L - I * 8 - J];
          E := 16;
          for D := 1 to 16 do
            if (B = HexLookupU_Str[D]) or
               (B = HexLookupL_Str[D]) then
              begin
                E := D - 1;
                break;
              end;
          if E = 16 then
            RaiseConvertError;
          F := F or (E shl (J * 4));
        end;
      HugeWordSetElement(R, I, F);
    end;
end;

{ HugeWord ISqrt                                                               }
{   Calculates integer square root of A using Newton's method.                 }
{   Pre:  A normalised                                                         }
{   Post: A normalised                                                         }
procedure HugeWordISqrt(var A: HugeWord);
var B, C, D, E, F : HugeWord;
    I, K, L, P    : Integer;
    R             : Boolean;
begin
  // Handle special cases
  if HugeWordCompareWord32(A, 1) <= 0 then // A <= 1
    exit;
  HugeWordInit(B);
  HugeWordInit(C);
  HugeWordInit(D);
  HugeWordInit(E);
  HugeWordInit(F);
  try
    // Shift A left by 8 bits for extra precision
    HugeWordAssign(E, A);
    HugeWordShl(E, 8);
    // Divide algorithm based on Netwon's method for f(y,x) = y – x^2
    // xest <- (xest + y/xest) / 2
    // Initial estimate for xest is 1 shl (HighestSetBit div 2)
    K := HugeWordSetBitScanReverse(E);
    I := K div 2;
    HugeWordAssignOne(C);
    HugeWordShl(C, I);
    // Iterate until a) estimate converges
    //               b) the estimate alternates between two values
    //               c) a maximum number of iterations is reached
    // Allow for one iteration per bit in A (plus extra); this is more than
    // enough since Newton's method doubles precision with every iteration
    // and the initial estimate should be close
    L := 8 + K;
    I := 0;
    P := 0;
    R := False;
    repeat
      HugeWordAssign(B, C);                   // B = previous xest
      HugeWordDivide(E, B, C, D);             // C = y/xest
      HugeWordAdd(C, B);                      // C = xest + y/xest
      HugeWordShr1(C);                        // C = (xest + y/xest) / 2
      // finish if maximum iteration reached
      if I = L then
        R := True else
      // finish if xest converged on exact value
      if HugeWordEquals(B, C) then
        R := True
      else
        begin
          // finish if this is the third iteration where difference is one
          HugeWordAssign(F, C);
          HugeWordSubtract(F, B);           // F = difference in xest and previous xest
          if HugeWordCompareWord32(F, 1) <= 0 then  // F <= 1
            if P = 2 then
              begin
                // xest is alternating between two sequential values
                // Take smallest of the two
                HugeWordMin(C, B);
                R := True
              end
            else
              Inc(P);
        end;
      Inc(I);
    until R;
    // Restore precision in result by shifting right
    HugeWordShr(C, 4);
    HugeWordNormalise(C);
    // Return result
    HugeWordAssign(A, C);
  finally
    HugeWordFinalise(F);
    HugeWordFinalise(E);
    HugeWordFinalise(D);
    HugeWordFinalise(C);
    HugeWordFinalise(B);
  end;
end;

{ HugeWord Extended Euclid                                                     }
{                                                                              }
{ Pseudocode:                                                                  }
{    function extended_gcd(a, b)                                               }
{    x := 0    lastx := 1                                                      }
{    y := 1    lasty := 0                                                      }
{    while b <> 0                                                              }
{        quotient := a div b                                                   }
{                                                                              }
{        temp := b                                                             }
{        b := a mod b                                                          }
{        a := temp                                                             }
{                                                                              }
{        temp := x                                                             }
{        x := lastx-quotient*x                                                 }
{        lastx := temp                                                         }
{                                                                              }
{        temp := y                                                             }
{        y := lasty-quotient*y                                                 }
{        lasty := temp                                                         }
{    return (lastx, lasty, a)                                                  }
{                                                                              }
{   Post:  R contains GCD(A, B)                                                }
{          X and Y contains values that solve AX + BY = GCD(A, B)              }
procedure HugeWordExtendedEuclid(const A, B: HugeWord; var R: HugeWord; var X, Y: HugeInt);
var C, D, T, Q : HugeWord;
    I, J, U    : HugeInt;
begin
  HugeWordInitHugeWord(C, A);                    // C = A
  HugeWordInitHugeWord(D, B);                    // D = B
  HugeWordInit(T);
  HugeWordInit(Q);
  HugeIntInitOne(I);                             // I = 1                       lastx = 1
  HugeIntInitZero(J);                            // J = 0                       lasty = 0
  HugeIntInit(U);
  try
    HugeIntAssignZero(X);                        // X = 0                       x = 0
    HugeIntAssignOne(Y);                         // Y = 1                       y = 1
    while not HugeWordIsZero(D) do               //                             while b <> 0
      begin
        HugeWordAssign(T, D);                    // T = D                       temp = b
        HugeWordDivide(C, D, Q, D);              // D = C mod D, Q = C div D    Q = quotient
        HugeWordAssign(C, T);                    // C = T                       a = temp

        // x[i+1] = x[i-1] - q[i]*x[i]
        HugeIntAssign(U, X);                     // U = X                       temp = x
        HugeIntMultiplyHugeWord(X, Q);           // X = X * Q                   quotient * x
        HugeIntSubtractHugeInt(I, X);            // I = I - X                   lastx - quotient * x
        HugeIntAssign(X, I);                     // X = I                       x = lastx - quotient * x
        HugeIntAssign(I, U);                     // I = U                       lastx = temp

        HugeIntAssign(U, Y);                     // U = Y                       temp = y
        HugeIntMultiplyHugeWord(Y, Q);           // Y = Y * Q                   quotient * y
        HugeIntSubtractHugeInt(J, Y);            // J = J - Y                   lasty - quotient * y
        HugeIntAssign(Y, J);                     // Y = J                       y = lasty - quotient * y
        HugeIntAssign(J, U);                     // J = U                       lasty = temp
      end;
    HugeIntAssign(X, I);                         // X = I                       x = lastx
    HugeIntAssign(Y, J);                         // Y = J                       y = lasty
    HugeWordAssign(R, C);                        // R = C                       r = a
  finally
    HugeIntFinalise(U);
    HugeIntFinalise(J);
    HugeIntFinalise(I);
    HugeWordFinalise(Q);
    HugeWordFinalise(T);
    HugeWordFinalise(D);
    HugeWordFinalise(C);
  end;
end;

{ HugeWord Modular Inverse and Mod                                             }
{   Calculates modular inverse(E) mod M using extended Euclidean algorithm     }
{   Post:  Returns False if modular inverse does not exist                     }
{          R contains modular inverse(E) mod M if modular inverse exists       }
function HugeWordModInv(const E, M: HugeWord; var R: HugeWord): Boolean;
var GCD : HugeWord;
    X, Y, F, G : HugeInt;
begin
  HugeWordInit(GCD);
  HugeIntInit(X);
  HugeIntInit(Y);
  HugeIntInit(F);
  HugeIntInit(G);
  try
    HugeWordExtendedEuclid(E, M, GCD, X, Y);
    if HugeWordIsOne(GCD) then
      begin
        HugeIntAssign(F, X);
        if HugeIntIsNegative(F) then
          begin
            HugeIntAssignHugeWord(G, M);
            HugeIntAddHugeInt(F, G);
          end;
        HugeWordAssign(R, F.Value);
        Result := True;
      end
    else
      Result := False;
  finally
    HugeIntFinalise(G);
    HugeIntFinalise(F);
    HugeIntFinalise(Y);
    HugeIntFinalise(X);
    HugeWordFinalise(GCD);
  end;
end;

{ HugeWord Random                                                              }
{   Generates a random HugeWord with Size elements.                            }
{   Pre:   Size is number of elements in result                                }
{   Post:  A normalised                                                        }
procedure HugeWordRandom(var A: HugeWord; const Size: Integer);
var I : Integer;
    P : PWord32;
begin
  HugeWordSetSize(A, Size);
  if Size <= 0 then
    exit;
  P := A.Data;
  for I := 0 to Size - 1 do
    begin
      P^ := RandomUniform32;
      Inc(P);
    end;
  HugeWordNormalise(A);
end;

{ HugeWord RandomN                                                             }
{   Generates a random HugeWord with value in range 0 to N.                    }
{   Pre:   N is maximum random value                                           }
{   Post:  A normalised                                                        }
procedure HugeWordRandomN(var A: HugeWord; const N: HugeWord);
var L, I : Integer;
    P : PWord32;
begin
  L := N.Used;
  if L = 0 then
    begin
      HugeWordAssignZero(A);
      exit;
    end;
  HugeWordSetSize(A, N.Used);
  P := A.Data;
  for I := 0 to L - 1 do
    begin
      P^ := RandomUniform32;
      Inc(P);
    end;
  repeat
    HugeWordNormalise(A);
    if HugeWordCompare(A, N) <= 0 then
      exit;
    HugeWordShr1(A);
  until False;
end;

{ HugeWord Primality testing                                                   }
const
  PrimeTableCount = 54;
  PrimeTable: array[0..PrimeTableCount - 1] of Byte = (
      2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61,
      67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137,
      139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 211,
      223, 227, 229, 233, 239, 241, 251);

{ HugeWord IsPrime Quick Trial                                                 }
{   Quick check for primality using trial division of the first few prime      }
{   numbers.                                                                   }
function HugeWordIsPrime_QuickTrial(const A: HugeWord): TPrimality;
var L, I    : Integer;
    F, G, H : Word32;
    C, Q, R : HugeWord;
begin
  L := A.Used;
  if L = 0 then
    Result := pNotPrime else
  if L = 1 then
    begin
      F := HugeWordToWord32(A);
      if F = 1 then
        Result := pNotPrime else
      if F = 2 then
        Result := pPrime else
      if F and 1 = 0 then
        Result := pNotPrime
      else
        begin
          G := Trunc(Sqrt(F)) + 1;
          for I := 1 to PrimeTableCount - 1 do
            begin
              H := PrimeTable[I];
              if H > G then
                begin
                  Result := pPrime;
                  exit;
                end;
              if F mod H = 0 then
                begin
                  Result := pNotPrime;
                  exit;
                end;
            end;
          Result := pPotentialPrime;
        end;
    end
  else
    if not HugeWordIsOdd(A) then
      Result := pNotPrime
    else
      begin
        HugeWordInit(C);
        HugeWordInit(Q);
        HugeWordInit(R);
        try
          for I := 1 to PrimeTableCount - 1 do
            begin
              H := PrimeTable[I];
              HugeWordAssignWord32(C, H);
              HugeWordDivide(A, C, Q, R);
              if HugeWordIsZero(R) then
                begin
                  Result := pNotPrime;
                  exit;
                end;
            end;
          Result := pPotentialPrime;
        finally
          HugeWordFinalise(R);
          HugeWordFinalise(Q);
          HugeWordFinalise(C);
        end;
      end;
end;

{ HugeWord IsPrime MillerRabin                                                 }
{   Check primality using Miller-Rabin:                                        }
{   "pick some a and take a^(n-1) mod n. If you don't get 1, then you know     }
{   that n is not prime."                                                      }
{   +---+---+---+---+---+---+---+---+----+                                     }
{   | k |256|342|384|410|512|683|768|1024|                                     }
{   +---+---+---+---+---+---+---+---+----+                                     }
{   | t |17 |12 |11 |10 |8  |6  |5  |4   |                                     }
{   +---+---+---+---+---+---+---+---+----+                                     }
{   k = bits in A                                                              }
{   t = PrimeTableMRCount sufficient for 2^-100 probability of not being prime }
function HugeWordIsPrime_MillerRabin_Basic(const A: HugeWord): TPrimality;
var I, L, N : Integer;
    B, C, D : HugeWord;
begin
  HugeWordInit(B);
  HugeWordInitHugeWord(C, A);
  HugeWordInit(D);
  try
    HugeWordSubtractWord32(C, 1);
    // determine number of checks to do according to number of bits in A
    L := HugeWordSetBitScanReverse(A) + 1;
    if L >= 1024 then
      N := 4 else
    if L >= 512 then
      N := 8 else
    if L >= 256 then
      N := 17
    else
      N := 25;
    // do check using first N prime numbers as "a".
    // this may be sufficient for actual primality testing for certain A
    for I := 0 to N - 1 do
      begin
        HugeWordAssignWord32(B, PrimeTable[I]);
        HugeWordPowerAndMod(D, B, C, A);
        if not HugeWordIsOne(D) then
          begin
            Result := pNotPrime;
            exit;
          end;
      end;
  finally
    HugeWordFinalise(D);
    HugeWordFinalise(C);
    HugeWordFinalise(B);
  end;
  Result := pPotentialPrime;
end;

function HugeWordIsPrime_MillerRabin(const A: HugeWord): TPrimality;
var I, L, N, J, P : Integer;
    B, C, D, E, X : HugeWord;
begin
  HugeWordInit(B);
  HugeWordInitHugeWord(C, A);
  HugeWordInit(D);
  HugeWordInit(E);
  HugeWordInit(X);
  try
    HugeWordSubtractWord32(C, 1);

    // determine number of checks to do according to number of bits in A
    L := HugeWordSetBitScanReverse(A) + 1;
    if L >= 1024 then
      N := 4 else
    if L >= 512 then
      N := 8 else
    if L >= 256 then
      N := 17
    else
      N := 25;

    // calculate P and E so that C = A - 1 = 2^P * E
    HugeWordAssign(E, C); // E = A - 1
    P := 0;
    while HugeWordIsEven(E) do
      begin
        Inc(P);
        HugeWordShr1(E);
      end;
    // here C = A - 1 = 2^P * E

    // do check using first N prime numbers as "a".
    // this may be sufficient for actual primality testing for certain A
    for I := 0 to N - 1 do
      begin
        HugeWordAssignWord32(B, PrimeTable[I]);
        // X = B^E mod A
        HugeWordPowerAndMod(X, B, E, A);
        if HugeWordIsOne(X) or HugeWordEquals(X, C) then
          continue;
        for J := 1 to P - 1 do
          begin
            HugeWordMultiply(D, X, X); // D = X^2
            HugeWordMod(D, A, X);      // X = X^2 mod A
            if HugeWordIsOne(X) then
              begin
                Result := pNotPrime;
                exit;
              end;
            if HugeWordEquals(X, C) then // if X = A - 1
              break;
          end;
         if HugeWordEquals(X, C) then // if X = A - 1
           continue;
         Result := pNotPrime;
         exit;
      end;
  finally
    HugeWordFinalise(D);
    HugeWordFinalise(C);
    HugeWordFinalise(B);
    HugeWordFinalise(E);
    HugeWordFinalise(X);
  end;
  Result := pPotentialPrime;
end;



{ HugeWord IsPrime                                                             }
function HugeWordIsPrime(const A: HugeWord): TPrimality;
begin
  Result := HugeWordIsPrime_QuickTrial(A);
  if Result <> pPotentialPrime then
    exit;
  Result := HugeWordIsPrime_MillerRabin(A);
end;



{ HugeWord NextPotentialPrime                                                  }
{   Returns the next potential prime after A.                                  }
{   Returns A = 0 if process was aborted by callback procedure.                } 
procedure HugeWordNextPotentialPrime(var A: HugeWord;
          const CallbackProc: THugeWordCallbackProc;
          const CallbackData: Integer);
begin
  if not HugeWordIsOdd(A) then
    HugeWordAddWord32(A, 1)
  else
    HugeWordAddWord32(A, 2);
  while HugeWordIsPrime(A) = pNotPrime do
    begin
      if Assigned(CallbackProc) then
        if not CallbackProc(CallbackData) then
          begin
            // aborted
            HugeWordInitZero(A);
            exit;
          end;
      HugeWordAddWord32(A, 2);
    end;
end;



{                                                                              }
{ HugeInt                                                                      }
{                                                                              }
procedure HugeIntInit(out A: HugeInt);
begin
  A.Sign := 0;
  HugeWordInit(A.Value);
end;

procedure HugeIntFinalise(var A: HugeInt);
begin
  HugeWordFinalise(A.Value);
end;

procedure HugeIntClearAndFinalise(var A: HugeInt);
begin
  HugeWordClearAndFinalise(A.Value);
end;

{ HugeIntNormalise                                                             }
{   A 'normalised' HugeInt has a normalised HugeWord Value and has a Sign of   }
{   zero if the Value is zero, otherwise a Sign of +1 or -1 to indicate the    }
{   sign of Value.                                                             }
procedure HugeIntNormalise(var A: HugeInt);
begin
  HugeWordNormalise(A.Value);
  if HugeWordIsZero(A.Value) then
    A.Sign := 0;
end;

procedure HugeIntInitZero(out A: HugeInt);
begin
  HugeIntInit(A);
end;

procedure HugeIntInitOne(out A: HugeInt);
begin
  HugeIntInit(A);
  HugeIntAssignOne(A);
end;

procedure HugeIntInitMinusOne(out A: HugeInt);
begin
  HugeIntInit(A);
  HugeIntAssignMinusOne(A);
end;

procedure HugeIntInitWord32(out A: HugeInt; const B: Word32);
begin
  HugeIntInit(A);
  HugeIntAssignWord32(A, B);
end;

procedure HugeIntInitInt32(out A: HugeInt; const B: Int32);
begin
  HugeIntInit(A);
  HugeIntAssignInt32(A, B);
end;

procedure HugeIntInitInt64(out A: HugeInt; const B: Int64);
begin
  HugeIntInit(A);
  HugeIntAssignInt64(A, B);
end;

procedure HugeIntInitDouble(out A: HugeInt; const B: Double);
begin
  HugeIntInit(A);
  HugeIntAssignDouble(A, B);
end;

procedure HugeIntInitHugeWord(out A: HugeInt; const B: HugeWord);
begin
  if HugeWordIsZero(B) then
    HugeIntInit(A)
  else
    begin
      A.Sign := 1;
      HugeWordInitHugeWord(A.Value, B);
    end;
end;

procedure HugeIntInitHugeInt(out A: HugeInt; const B: HugeInt);
begin
  A.Sign := B.Sign;
  HugeWordInitHugeWord(A.Value, B.Value);
end;

procedure HugeIntAssignZero(var A: HugeInt);
begin
  A.Sign := 0;
  HugeWordAssignZero(A.Value);
end;

procedure HugeIntAssignOne(var A: HugeInt);
begin
  A.Sign := 1;
  HugeWordAssignOne(A.Value);
end;

procedure HugeIntAssignMinusOne(var A: HugeInt);
begin
  A.Sign := -1;
  HugeWordAssignOne(A.Value);
end;

procedure HugeIntAssignWord32(var A: HugeInt; const B: Word32);
begin
  if B = 0 then
    begin
      A.Sign := 0;
      HugeWordAssignZero(A.Value);
    end
  else
    begin
      A.Sign := 1;
      HugeWordAssignWord32(A.Value, B);
    end;
end;

procedure HugeIntAssignInt32(var A: HugeInt; const B: Int32);
begin
  if B = 0 then
    begin
      A.Sign := 0;
      HugeWordAssignZero(A.Value);
    end else
  if B < 0 then
    begin
      A.Sign := -1;
      HugeWordAssignWord32(A.Value, Word32(-Int64(B)));
    end
  else
    begin
      A.Sign := 1;
      HugeWordAssignInt32(A.Value, B);
    end;
end;

procedure HugeIntAssignInt64(var A: HugeInt; const B: Int64);
var T : Int64;
begin
  if B = 0 then
    begin
      A.Sign := 0;
      HugeWordAssignZero(A.Value);
    end else
  if B < 0 then
    begin
      A.Sign := -1;
      if B = MinInt64 {-$8000000000000000} then
        begin
          HugeWordSetSize(A.Value, 2);
          HugeWordSetElement(A.Value, 0, $00000000);
          HugeWordSetElement(A.Value, 1, $80000000);
        end
      else
        begin
          T := -B;
          HugeWordAssignInt64(A.Value, T);
        end;
    end
  else
    begin
      A.Sign := 1;
      HugeWordAssignInt64(A.Value, B);
    end;
end;

procedure HugeIntAssignDouble(var A: HugeInt; const B: Double);
begin
  HugeWordAssignDouble(A.Value, Abs(B));
  if HugeWordIsZero(A.Value) then
    A.Sign := 0
  else
    if B < 0.0 then
      A.Sign := -1
    else
      A.Sign := 1;
end;

procedure HugeIntAssignHugeWord(var A: HugeInt; const B: HugeWord);
begin
  if HugeWordIsZero(B) then
    begin
      A.Sign := 0;
      HugeWordAssignZero(A.Value);
    end
  else
    begin
      A.Sign := 1;
      HugeWordAssign(A.Value, B);
    end;
end;

procedure HugeIntAssignHugeWordNegated(var A: HugeInt; const B: HugeWord);
begin
  if HugeWordIsZero(B) then
    begin
      A.Sign := 0;
      HugeWordAssignZero(A.Value);
    end
  else
    begin
      A.Sign := -1;
      HugeWordAssign(A.Value, B);
    end;
end;

procedure HugeIntAssign(var A: HugeInt; const B: HugeInt);
begin
  A.Sign := B.Sign;
  HugeWordAssign(A.Value, B.Value);
end;

procedure HugeIntSwap(var A, B: HugeInt);
var C : HugeInt;
begin
  HugeIntInitHugeInt(C, A);      // C := A
  try
    HugeIntAssign(A, B);  // A := B
    HugeIntAssign(B, C);  // B := C
  finally
    HugeIntFinalise(C);
  end;
end;

function HugeIntIsZero(const A: HugeInt): Boolean;
begin
  Result := A.Sign = 0;
end;

function HugeIntIsNegative(const A: HugeInt): Boolean;
begin
  Result := A.Sign < 0;
end;

function HugeIntIsNegativeOrZero(const A: HugeInt): Boolean;
begin
  Result := A.Sign <= 0;
end;

function HugeIntIsPositive(const A: HugeInt): Boolean;
begin
  Result := A.Sign > 0;
end;

function HugeIntIsPositiveOrZero(const A: HugeInt): Boolean;
begin
  Result := A.Sign >= 0;
end;

function HugeIntIsOne(const A: HugeInt): Boolean;
begin
  Result := (A.Sign > 0) and HugeWordIsOne(A.Value);
end;

function HugeIntIsMinusOne(const A: HugeInt): Boolean;
begin
  Result := (A.Sign < 0) and HugeWordIsOne(A.Value);
end;

function HugeIntIsOdd(const A: HugeInt): Boolean;
begin
  Result := HugeWordIsOdd(A.Value);
end;

function HugeIntIsEven(const A: HugeInt): Boolean;
begin
  Result := HugeWordIsOdd(A.Value);
end;

function HugeIntIsWord32Range(const A: HugeInt): Boolean;
begin
  if A.Sign = 0 then
    Result := True else
  if A.Sign < 0 then
    Result := False
  else
    Result := HugeWordGetSize(A.Value) <= 1;
end;

function HugeIntIsWord64Range(const A: HugeInt): Boolean;
begin
  if A.Sign = 0 then
    Result := True else
  if A.Sign < 0 then
    Result := False
  else
    Result := HugeWordGetSize(A.Value) <= 2;
end;

function HugeIntIsWord128Range(const A: HugeInt): Boolean;
begin
  if A.Sign = 0 then
    Result := True else
  if A.Sign < 0 then
    Result := False
  else
    Result := HugeWordGetSize(A.Value) <= 4;
end;

function HugeIntIsWord256Range(const A: HugeInt): Boolean;
begin
  if A.Sign = 0 then
    Result := True else
  if A.Sign < 0 then
    Result := False
  else
    Result := HugeWordGetSize(A.Value) <= 8;
end;

function HugeIntIsInt32Range(const A: HugeInt): Boolean;
begin
  if A.Sign = 0 then
    Result := True else
  if HugeWordGetSize(A.Value) > 1 then
    Result := False
  else
    if A.Sign > 0 then
      Result := HugeWordIsInt32Range(A.Value)
    else
      Result := HugeWordGetElement(A.Value, 0) <= $80000000;
end;

function HugeIntIsInt64Range(const A: HugeInt): Boolean;
var F : Word32;
begin
  if A.Sign = 0 then
    Result := True
  else
  begin
    F := HugeWordGetSize(A.Value);
    if F = 1 then
      Result := True else
    if F > 2 then
      Result := False
    else
      if A.Sign > 0 then
        Result := HugeWordIsInt64Range(A.Value)
      else
        begin
          F := HugeWordGetElement(A.Value, 1);
          if F > $80000000 then
            Result := False else
          if F < $80000000 then
            Result := True
          else
            Result := HugeWordGetElement(A.Value, 0) = $00000000;
        end;
  end;
end;

function HugeIntIsInt128Range(const A: HugeInt): Boolean;
var F : Word32;
begin
  if A.Sign = 0 then
    Result := True
  else
  begin
    F := HugeWordGetSize(A.Value);
    if F < 4 then
      Result := True else
    if F > 4 then
      Result := False
    else
      if A.Sign > 0 then
        Result := HugeWordIsInt128Range(A.Value)
      else
        begin
          F := HugeWordGetElement(A.Value, 3);
          if F > $80000000 then
            Result := False else
          if F < $80000000 then
            Result := True
          else
            Result := (HugeWordGetElement(A.Value, 0) = $00000000)
                  and (HugeWordGetElement(A.Value, 1) = $00000000)
                  and (HugeWordGetElement(A.Value, 2) = $00000000);
        end;
  end;
end;

function HugeIntIsInt256Range(const A: HugeInt): Boolean;
var F : Word32;
begin
  if A.Sign = 0 then
    Result := True
  else
  begin
    F := HugeWordGetSize(A.Value);
    if F < 8 then
      Result := True else
    if F > 8 then
      Result := False
    else
      if A.Sign > 0 then
        Result := HugeWordIsInt256Range(A.Value)
      else
        begin
          F := HugeWordGetElement(A.Value, 7);
          if F > $80000000 then
            Result := False else
          if F < $80000000 then
            Result := True
          else
            Result := (HugeWordGetElement(A.Value, 0) = $00000000)
                  and (HugeWordGetElement(A.Value, 1) = $00000000)
                  and (HugeWordGetElement(A.Value, 2) = $00000000)
                  and (HugeWordGetElement(A.Value, 3) = $00000000)
                  and (HugeWordGetElement(A.Value, 4) = $00000000)
                  and (HugeWordGetElement(A.Value, 5) = $00000000)
                  and (HugeWordGetElement(A.Value, 6) = $00000000);
        end;
  end;
end;

function HugeIntSign(const A: HugeInt): Integer;
begin
  Result := A.Sign;
end;

procedure HugeIntNegate(var A: HugeInt);
begin
  A.Sign := -A.Sign;
end;

function HugeIntAbsInPlace(var A: HugeInt): Boolean;
begin
  if A.Sign < 0 then
    begin
      A.Sign := 1;
      Result := True;
    end
  else
    Result := False;
end;

function HugeIntAbs(const A: HugeInt; var B: HugeWord): Boolean;
begin
  HugeWordAssign(B, A.Value);
  Result := A.Sign < 0;
end;

function HugeIntToWord32(const A: HugeInt): Word32;
begin
  {$IFOPT R+}
  if (A.Sign < 0) or not HugeWordIsWord32Range(A.Value) then
    RaiseRangeError;
  {$ENDIF}
  Result := HugeWordToWord32(A.Value);
end;

function HugeIntToInt32(const A: HugeInt): Int32;
begin
  {$IFOPT R+}
  if ((A.Sign > 0) and (HugeWordCompareWord32(A.Value, $7FFFFFFF) > 0)) or
     ((A.Sign < 0) and (HugeWordCompareWord32(A.Value, $80000000) > 0)) then
    RaiseRangeError;
  {$ENDIF}
  if A.Sign > 0 then
    Result := HugeWordToWord32(A.Value) else
  if A.Sign < 0 then
    begin
      // Delphi5/7 incorrectly raises an exception if the following is done
      // in one statement, i.e.
      //   Result := -HugeWordToWord32(A.Value)
      Result := HugeWordToWord32(A.Value);
      Result := -Result;
    end
  else
    Result := 0;
end;

function HugeIntToInt64(const A: HugeInt): Int64;
begin
  {$IFOPT R+}
  if A.Value.Used > 2 then
    RaiseRangeError else
  if A.Value.Used = 2 then
    if A.Sign > 0 then
      begin
        if HugeWordGetElement(A.Value, 1) > $7FFFFFFF then
          RaiseRangeError;
      end else
    if A.Sign < 0 then
      begin
        if HugeWordGetElement(A.Value, 1) > $80000000 then
          RaiseRangeError;
        if (HugeWordGetElement(A.Value, 1) = $80000000) and
           (HugeWordGetElement(A.Value, 0) > $00000000) then
          RaiseRangeError;
      end;
  {$ENDIF}
  if A.Sign > 0 then
    Result := HugeWordToInt64(A.Value) else
  if A.Sign < 0 then
    begin
      if (A.Value.Used = 2) and
         (HugeWordGetElement(A.Value, 1) = $80000000) and
         (HugeWordGetElement(A.Value, 0) = $00000000) then
        Result := MinInt64 { -$8000000000000000 }
      else
        begin
          {$IFDEF DELPHI5}
          // Delphi5 incorrectly raises an overflow with 7FFFFFFFFFFFFFFF
          if (A.Value.Used = 2) and
             (HugeWordGetElement(A.Value, 1) = $7FFFFFFF) and
             (HugeWordGetElement(A.Value, 0) = $FFFFFFFF) then
            Result := -$7FFFFFFFFFFFFFFF
          else
            Result := -HugeWordToInt64(A.Value);
          {$ELSE}
          Result := -HugeWordToInt64(A.Value);
          {$ENDIF}
        end;
    end
  else
    Result := 0;
end;

function HugeIntToDouble(const A: HugeInt): Double;
var V : Double;
begin
  V := HugeWordToDouble(A.Value);
  if A.Sign < 0 then
    V := -V;
  Result := V;
end;

function HugeIntEqualsWord32(const A: HugeInt; const B: Word32): Boolean;
begin
  if A.Sign < 0 then
    Result := False else
  if not HugeWordIsWord32Range(A.Value) then
    Result := False
  else
    Result := HugeWordToWord32(A.Value) = B;
end;

function HugeIntEqualsInt32(const A: HugeInt; const B: Int32): Boolean;
begin
  if A.Sign < 0 then
    if B >= 0 then
      Result := False
    else
      Result := HugeWordEqualsWord32(A.Value, Word32(-Int64(B)))
  else
    Result := HugeWordEqualsInt32(A.Value, B);
end;

function HugeIntEqualsInt64(const A: HugeInt; const B: Int64): Boolean;
begin
  if A.Sign < 0 then
    if B >= 0 then
      Result := False
    else
      begin
        if B = MinInt64 { -$8000000000000000 } then
          begin
            if HugeWordGetSize(A.Value) <> 2 then
              Result := False
            else
              Result :=
                  (HugeWordGetElement(A.Value, 0) = $00000000) and
                  (HugeWordGetElement(A.Value, 1) = $80000000);
          end
        else
          Result := HugeWordEqualsInt64(A.Value, -B);
      end
  else
    Result := HugeWordEqualsInt64(A.Value, B);
end;

function HugeIntEqualsHugeInt(const A, B: HugeInt): Boolean;
begin
  if A.Sign <> B.Sign then
    Result := False else
  if A.Sign = 0 then
    Result := True
  else
    Result := HugeWordEquals(A.Value, B.Value);
end;

function HugeIntCompareWord32(const A: HugeInt; const B: Word32): Integer;
begin
  if A.Sign < 0 then
    Result := -1 else
  if A.Sign = 0 then
    if B = 0 then
      Result := 0
    else
      Result := -1
  else
    Result := HugeWordCompareWord32(A.Value, B);
end;

function HugeIntCompareInt32(const A: HugeInt; const B: Int32): Integer;
begin
  if A.Sign < 0 then
    if B >= 0 then
      Result := -1
    else
      Result := -HugeWordCompareWord32(A.Value, Word32(-Int64(B)))
  else
  if A.Sign = 0 then
    if B < 0 then
      Result := 1 else
    if B = 0 then
      Result := 0
    else
      Result := -1
  else
    Result := HugeWordCompareInt32(A.Value, B);
end;

function HugeIntCompareInt64(const A: HugeInt; const B: Int64): Integer;
var L : Integer;
    F, G : Word32;
begin
  if A.Sign < 0 then
    if B >= 0 then
      Result := -1
    else
      begin
        if B = MinInt64 { -$8000000000000000 } then
          begin
            L := HugeWordGetSize(A.Value);
            if L < 2 then
              Result := 1 else
            if L > 2 then
              Result := -1
            else
              begin
                F := HugeWordGetElement(A.Value, 0);
                G := HugeWordGetElement(A.Value, 1);
                if G > $80000000 then
                  Result := -1 else
                if G < $80000000 then
                  Result := 1
                else
                  if F > $00000000 then
                    Result := -1
                  else
                    Result := 0;
              end;
          end
        else
          Result := -HugeWordCompareInt64(A.Value, -B);
      end
  else
  if A.Sign = 0 then
    if B < 0 then
      Result := 1 else
    if B = 0 then
      Result := 0
    else
      Result := -1
  else
    Result := HugeWordCompareInt64(A.Value, B);
end;

function HugeIntCompareHugeInt(const A, B: HugeInt): Integer;
begin
  if A.Sign < 0 then
    if B.Sign >= 0 then
      Result := -1
    else
      Result := -HugeWordCompare(A.Value, B.Value)
  else
  if A.Sign = 0 then
    if B.Sign < 0 then
      Result := 1 else
    if B.Sign = 0 then
      Result := 0
    else
      Result := -1
  else
    if B.Sign <= 0 then
      Result := 1
    else
      Result := HugeWordCompare(A.Value, B.Value);
end;

{ HugeInt CompareHugeIntAbs                                                    }
{   Compares the absolute values of two HugeInts.                              }
function HugeIntCompareHugeIntAbs(const A, B: HugeInt): Integer;
begin
  if A.Sign = 0 then
    if B.Sign = 0 then
      Result := 0
    else
      Result := -1
  else
  if B.Sign = 0 then
    Result := 1
  else
    Result := HugeWordCompare(A.Value, B.Value);
end;

procedure HugeIntMin(var A: HugeInt; const B: HugeInt);
begin
  if HugeIntCompareHugeInt(A, B) <= 0 then
    exit;
  HugeIntAssign(A, B);
end;

procedure HugeIntMax(var A: HugeInt; const B: HugeInt);
begin
  if HugeIntCompareHugeInt(A, B) >= 0 then
    exit;
  HugeIntAssign(A, B);
end;

procedure HugeIntAddWord32(var A: HugeInt; const B: Word32);
var C : Integer;
begin
  if B = 0 then
    exit;
  if A.Sign = 0 then
    HugeIntAssignWord32(A, B) else
  if A.Sign < 0 then
    begin
      C := HugeWordSubtractWord32(A.Value, B);
      if C = 0 then
        HugeIntAssignZero(A) else
      if C < 0 then
        A.Sign := 1;
    end
  else
    HugeWordAddWord32(A.Value, B);
end;

procedure HugeIntAddInt32(var A: HugeInt; const B: Int32);
var C : Integer;
begin
  if B = 0 then
    exit;
  if A.Sign = 0 then
    HugeIntAssignInt32(A, B)
  else
    if ((B > 0) and (A.Sign < 0)) or
       ((B < 0) and (A.Sign > 0)) then
      begin
        C := HugeWordSubtractWord32(A.Value, Abs(B));
        if C = 0 then
          HugeIntAssignZero(A) else
        if C < 0 then
          A.Sign := -A.Sign;
      end
    else
      HugeWordAddWord32(A.Value, Abs(B));
end;

procedure HugeIntAddHugeInt(var A: HugeInt; const B: HugeInt);
var C : Integer;
begin
  if B.Sign = 0 then
    exit;
  if A.Sign = 0 then
    HugeIntAssign(A, B)
  else
    if A.Sign <> B.Sign then
      begin
        C := HugeWordSubtract(A.Value, B.Value);
        if C = 0 then
          HugeIntAssignZero(A) else
        if C < 0 then
          A.Sign := -A.Sign;
      end
    else
      HugeWordAdd(A.Value, B.Value);
end;

procedure HugeIntInc(var A: HugeInt);
begin
  HugeIntAddWord32(A, 1);
end;

procedure HugeIntSubtractWord32(var A: HugeInt; const B: Word32);
var C : Integer;
begin
  if B = 0 then
    exit;
  if A.Sign = 0 then
    begin
      A.Sign := -1;
      HugeWordInitWord32(A.Value, B);
    end else
  if A.Sign < 0 then
    HugeWordAddWord32(A.Value, B)
  else
    begin
      C := HugeWordSubtractWord32(A.Value, B);
      if C = 0 then
        HugeIntAssignZero(A) else
      if C < 0 then
        A.Sign := -1;
    end;
end;

procedure HugeIntSubtractInt32(var A: HugeInt; const B: Int32);
var C : Integer;
begin
  if B = 0 then
    exit;
  if A.Sign = 0 then
    begin
      HugeIntAssignInt32(A, B);
      HugeIntNegate(A);
    end
  else
    if ((B > 0) and (A.Sign < 0)) or
       ((B < 0) and (A.Sign > 0)) then
      HugeWordAddWord32(A.Value, Abs(B))
    else
      begin
        C := HugeWordSubtractWord32(A.Value, Abs(B));
        if C = 0 then
          HugeIntAssignZero(A) else
        if C < 0 then
          A.Sign := -A.Sign;
      end;
end;

procedure HugeIntSubtractHugeInt(var A: HugeInt; const B: HugeInt);
var C : Integer;
begin
  if B.Sign = 0 then
    exit;
  if A.Sign = 0 then
    begin
      HugeIntAssign(A, B);
      A.Sign := -B.Sign;
    end
  else
    if A.Sign <> B.Sign then
      HugeWordAdd(A.Value, B.Value)
    else
      begin
        C := HugeWordSubtract(A.Value, B.Value);
        if C = 0 then
          HugeIntAssignZero(A) else
        if C < 0 then
          A.Sign := -A.Sign;
      end;
end;

procedure HugeIntDec(var A: HugeInt);
begin
  HugeIntSubtractWord32(A, 1);
end;

procedure HugeIntMultiplyWord8(var A: HugeInt; const B: Byte);
begin
  if A.Sign = 0 then
    exit;
  if B = 0 then
    HugeIntAssignZero(A)
  else
    HugeWordMultiplyWord8(A.Value, B);
end;

procedure HugeIntMultiplyWord16(var A: HugeInt; const B: Word);
begin
  if A.Sign = 0 then
    exit;
  if B = 0 then
    HugeIntAssignZero(A)
  else
    HugeWordMultiplyWord16(A.Value, B);
end;

procedure HugeIntMultiplyWord32(var A: HugeInt; const B: Word32);
begin
  if A.Sign = 0 then
    exit;
  if B = 0 then
    HugeIntAssignZero(A)
  else
    HugeWordMultiplyWord32(A.Value, B);
end;

procedure HugeIntMultiplyInt8(var A: HugeInt; const B: ShortInt);
begin
  if A.Sign = 0 then
    exit;
  if B = 0 then
    HugeIntAssignZero(A)
  else
    begin
      HugeWordMultiplyWord8(A.Value, Abs(B));
      if ((B < 0) and (A.Sign > 0)) or
         ((B > 0) and (A.Sign < 0)) then
        A.Sign := -1
      else
        A.Sign := 1;
    end;
end;

procedure HugeIntMultiplyInt16(var A: HugeInt; const B: SmallInt);
begin
  if A.Sign = 0 then
    exit;
  if B = 0 then
    HugeIntAssignZero(A)
  else
    begin
      HugeWordMultiplyWord16(A.Value, Abs(B));
      if ((B < 0) and (A.Sign > 0)) or
         ((B > 0) and (A.Sign < 0)) then
        A.Sign := -1
      else
        A.Sign := 1;
    end;
end;

procedure HugeIntMultiplyInt32(var A: HugeInt; const B: Int32);
begin
  if A.Sign = 0 then
    exit;
  if B = 0 then
    HugeIntAssignZero(A)
  else
    begin
      HugeWordMultiplyWord32(A.Value, Abs(B));
      if ((B < 0) and (A.Sign > 0)) or
         ((B > 0) and (A.Sign < 0)) then
        A.Sign := -1
      else
        A.Sign := 1;
    end;
end;

procedure HugeIntMultiplyHugeWord(var A: HugeInt; const B: HugeWord);
begin
  if A.Sign = 0 then
    exit;
  if HugeWordIsZero(B) then
    HugeIntAssignZero(A)
  else
    HugeWordMultiply(A.Value, A.Value, B);
end;

procedure HugeIntMultiplyHugeInt(var A: HugeInt; const B: HugeInt);
begin
  if A.Sign = 0 then
    exit;
  if B.Sign = 0 then
    HugeIntAssignZero(A)
  else
    begin
      HugeWordMultiply(A.Value, A.Value, B.Value);
      if A.Sign <> B.Sign then
        A.Sign := -1
      else
        A.Sign := 1;
    end;
end;

procedure HugeIntSqr(var A: HugeInt);
begin
  if A.Sign = 0 then
    exit;
  A.Sign := 1;
  HugeWordSqr(A.Value, A.Value);
end;

// Truncated division:
// Qt = Trunc(A/B)        Qt = Trunc(1000/3) = 333
// Rt = A - B * Qt        Rt = -1000 - (3 * -333) = -1000 + 999 = -1
// Sign of Rt always Sign of A

// Floor division:
// Qf = Floor(A/B) = Qt - I
// Rf = Rt + I * B
// I = if Sign(Rt) = -Sign(B) then 1 else 0

// Euclidian division:
// Qf = Floor(A/B) = Qt - I
// Rf = Rt + I * B
// 0 <= Rf < Abs(B)
// I = if Rt >= 0 then 0 else if B > 0 then 1 else -1

procedure HugeIntDivideWord32(const A: HugeInt; const B: Word32; var Q: HugeInt; out R: Word32);
begin
  if B = 0 then
    RaiseDivByZeroError;
  if A.Sign = 0 then
    begin
      HugeIntAssignZero(Q);
      R := 0;
      exit;
    end;
  HugeWordDivideWord32(A.Value, B, Q.Value, R);
  if HugeWordIsZero(Q.Value) then
    Q.Sign := 0
  else
    Q.Sign := A.Sign;
end;

procedure HugeIntDivideInt32(const A: HugeInt; const B: Int32; var Q: HugeInt; out R: Int32);
var C : Word32;
begin
  if B = 0 then
    RaiseDivByZeroError;
  if A.Sign = 0 then
    begin
      HugeIntAssignZero(Q);
      R := 0;
      exit;
    end;
  HugeWordDivideWord32(A.Value, Abs(B), Q.Value, C);
  if HugeWordIsZero(Q.Value) then
    Q.Sign := 0 else
  if ((B > 0) and (A.Sign < 0)) or
     ((B < 0) and (A.Sign > 0)) then
    Q.Sign := -1
  else
    Q.Sign := 1;
  R := Int32(C);
end;

/// See https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf

// Q = Trunc(A/B)
// Also known as truncated division
// Remainder has same sign as dividend
// C++, C#, Go, Java % operators
procedure HugeIntDivideHugeInt_T(const A, B: HugeInt; var Q, R: HugeInt);
begin
  if B.Sign = 0 then
    RaiseDivByZeroError;
  if A.Sign = 0 then
    begin
      HugeIntAssignZero(Q);
      HugeIntAssignZero(R);
      exit;
    end;
  HugeWordDivide(A.Value, B.Value, Q.Value, R.Value);
  if HugeWordIsZero(Q.Value) then
    begin
      Q.Sign := 0;
      R.Sign := A.Sign;
      exit;
    end;
  if HugeWordIsZero(R.Value) then
    begin
      R.Sign := 0;
      if A.Sign <> B.Sign then
        Q.Sign := -1
      else
        Q.Sign := 1;
      exit;
    end;
  R.Sign := A.Sign;
  if A.Sign <> B.Sign then
    Q.Sign := -1
  else
    Q.Sign := 1;
end;

// Q = Floor(A/B)
// Also known as Knuth method
// Remainder has same sign as divisor
// R = Python % operator
procedure HugeIntDivideHugeInt_F(const A, B: HugeInt; var Q, R: HugeInt);
var
  L : Integer;
begin
  HugeIntDivideHugeInt_T(A, B, Q, R);
  if R.Sign = -B.Sign then
    L := 1
  else
    L := 0;
  if L = 1 then
    begin
      HugeIntDec(Q);
      HugeIntAddHugeInt(R, B);
    end;
end;

// Euclidian division
// Remainder always positive
procedure HugeIntDivideHugeInt_E(const A, B: HugeInt; var Q, R: HugeInt);
var
  L : Integer;
begin
  HugeIntDivideHugeInt_T(A, B, Q, R);
  if R.Sign >= 0 then
    L := 0
  else
  if B.Sign > 0 then
    L := 1
  else
    L := -1;
  if L <> 0 then
    begin
      HugeIntSubtractInt32(Q, L);
      if L < 0 then
        HugeIntSubtractHugeInt(R, B)
      else
        HugeIntAddHugeInt(R, B)
    end;
end;

procedure HugeIntDivideHugeInt(const A, B: HugeInt; var Q, R: HugeInt);
begin
  if B.Sign = 0 then
    RaiseDivByZeroError;
  if A.Sign = 0 then
    begin
      HugeIntAssignZero(Q);
      HugeIntAssignZero(R);
      exit;
    end;
  HugeWordDivide(A.Value, B.Value, Q.Value, R.Value);
  if HugeWordIsZero(Q.Value) then
    Q.Sign := 0 else
  if A.Sign <> B.Sign then
    Q.Sign := -1
  else
    Q.Sign := 1;
  if HugeWordIsZero(R.Value) then
    R.Sign := 0
  else
    R.Sign := 1;
end;

procedure HugeIntMod_T(const A, B: HugeInt; var R: HugeInt);
var Q : HugeInt;
begin
  HugeIntInit(Q);
  try
    HugeIntDivideHugeInt_T(A, B, Q, R);
  finally
    HugeIntFinalise(Q);
  end;
end;

procedure HugeIntMod_F(const A, B: HugeInt; var R: HugeInt);
var Q : HugeInt;
begin
  HugeIntInit(Q);
  try
    HugeIntDivideHugeInt_F(A, B, Q, R);
  finally
    HugeIntFinalise(Q);
  end;
end;

procedure HugeIntMod_E(const A, B: HugeInt; var R: HugeInt);
var Q : HugeInt;
begin
  HugeIntInit(Q);
  try
    HugeIntDivideHugeInt_E(A, B, Q, R);
  finally
    HugeIntFinalise(Q);
  end;
end;

procedure HugeIntMod(const A, B: HugeInt; var R: HugeInt);
var Q : HugeInt;
begin
  HugeIntInit(Q);
  try
    HugeIntDivideHugeInt(A, B, Q, R);
  finally
    HugeIntFinalise(Q);
  end;
end;

procedure HugeIntPower(var A: HugeInt; const B: Word32);
begin
  if B = 0 then
    begin
      HugeIntAssignOne(A);
      exit;
    end;
  if HugeIntIsZero(A) or HugeIntIsOne(A) then
    exit;
  if B = 1 then
    exit;
  if B = 2 then
    begin
      HugeIntSqr(A);
      exit;
    end;
  HugeWordPower(A.Value, B);
  if (A.Sign < 0) and (B and 1 = 0) then
    A.Sign := 1;
end;

function HugeIntToStrB(const A: HugeInt): UTF8String;
var S : UTF8String;
begin
  if A.Sign = 0 then
    Result := '0'
  else
    begin
      S := HugeWordToStrB(A.Value);
      if A.Sign < 0 then
        Result := '-' + S
      else
        Result := S;
    end;
end;

function HugeIntToStrU(const A: HugeInt): UnicodeString;
var S : UnicodeString;
begin
  if A.Sign = 0 then
    Result := '0'
  else
    begin
      S := HugeWordToStrU(A.Value);
      if A.Sign < 0 then
        Result := '-' + S
      else
        Result := S;
    end;
end;

procedure StrToHugeIntB(const A: RawByteString; var R: HugeInt);
var B : RawByteString;
begin
  if A = '' then
    RaiseConvertError;
  if A[1] = '-' then
    begin
      R.Sign := -1;
      B := Copy(A, 2, Length(A) - 1);
    end
  else
    begin
      R.Sign := 1;
      B := A;
    end;
  StrToHugeWordB(B, R.Value);
  if HugeWordIsZero(R.Value) then
    R.Sign := 0;
end;

procedure StrToHugeIntU(const A: UnicodeString; var R: HugeInt);
var B : UnicodeString;
begin
  if A = '' then
    RaiseConvertError;
  if A[1] = '-' then
    begin
      R.Sign := -1;
      B := Copy(A, 2, Length(A) - 1);
    end
  else
    begin
      R.Sign := 1;
      B := A;
    end;
  StrToHugeWordU(B, R.Value);
  if HugeWordIsZero(R.Value) then
    R.Sign := 0;
end;

function HugeIntToHexB(const A: HugeInt): UTF8String;
var S : RawByteString;
begin
  if A.Sign = 0 then
    Result := '00000000'
  else
    begin
      S := HugeWordToHexB(A.Value);
      if A.Sign < 0 then
        Result := '-' + S
      else
        Result := S;
    end;
end;

procedure HexToHugeIntB(const A: RawByteString; var R: HugeInt);
var B : RawByteString;
begin
  if A = '' then
    RaiseConvertError;
  if A[1] = '-' then
    begin
      R.Sign := -1;
      B := Copy(A, 2, Length(A) - 1);
    end
  else
    begin
      R.Sign := 1;
      B := A;
    end;
  HexToHugeWordB(B, R.Value);
  if HugeWordIsZero(R.Value) then
    R.Sign := 0;
end;

procedure HugeIntISqrt(var A: HugeInt);
begin
  if A.Sign = 0 then
    exit;
  if A.Sign < 0 then
    RaiseInvalidOpError;
  HugeWordISqrt(A.Value);
end;

procedure HugeIntRandom(var A: HugeInt; const Size: Integer);
begin
  HugeWordRandom(A.Value, Size);
  if HugeWordIsZero(A.Value) then
    A.Sign := 0 else
  if RandomBoolean then
    A.Sign := 1
  else
    A.Sign := -1;
end;



{                                                                              }
{ HugeInt class                                                                }
{                                                                              }
constructor THugeInt.Create;
begin
  inherited Create;
  HugeIntInit(FValue);
end;

constructor THugeInt.Create(const A: Int64);
begin
  inherited Create;
  HugeIntInitInt64(FValue, A);
end;

constructor THugeInt.Create(const A: THugeInt);
begin
  Assert(Assigned(A));
  inherited Create;
  HugeIntInitHugeInt(FValue, A.FValue);
end;

destructor THugeInt.Destroy;
begin
  HugeIntFinalise(FValue);
  inherited Destroy;
end;

procedure THugeInt.AssignZero;
begin
  HugeIntAssignZero(FValue);;
end;

procedure THugeInt.AssignOne;
begin
  HugeIntAssignOne(FValue);;
end;

procedure THugeInt.AssignMinusOne;
begin
  HugeIntAssignMinusOne(FValue);;
end;

procedure THugeInt.Assign(const A: Int64);
begin
  HugeIntAssignInt64(FValue, A);
end;

procedure THugeInt.Assign(const A: THugeInt);
begin
  HugeIntAssign(FValue, A.FValue);
end;

function THugeInt.IsZero: Boolean;
begin
  Result := HugeIntIsZero(FValue);
end;

function THugeInt.IsNegative: Boolean;
begin
  Result := HugeIntIsNegative(FValue);
end;

function THugeInt.IsPositive: Boolean;
begin
  Result := HugeIntIsPositive(FValue);
end;

function THugeInt.IsOne: Boolean;
begin
  Result := HugeIntIsOne(FValue);
end;

function THugeInt.IsMinusOne: Boolean;
begin
  Result := HugeIntIsMinusOne(FValue);
end;

function THugeInt.IsOdd: Boolean;
begin
  Result := HugeIntIsOdd(FValue);
end;

function THugeInt.IsEven: Boolean;
begin
  Result := HugeIntIsEven(FValue);
end;

function THugeInt.Sign: Integer;
begin
  Result := HugeIntSign(FValue);
end;

procedure THugeInt.Negate;
begin
  HugeIntNegate(FValue);
end;

procedure THugeInt.Abs;
begin
  HugeIntAbsInPlace(FValue);
end;

function THugeInt.ToWord32: Word32;
begin
  Result := HugeIntToWord32(FValue);
end;

function THugeInt.ToInt32: Int32;
begin
  Result := HugeIntToInt32(FValue);
end;

function THugeInt.ToInt64: Int64;
begin
  Result := HugeIntToInt64(FValue);
end;

function THugeInt.EqualTo(const A: Word32): Boolean;
begin
  Result := HugeIntEqualsWord32(FValue, A);
end;

function THugeInt.EqualTo(const A: Int32): Boolean;
begin
  Result := HugeIntEqualsInt32(FValue, A);
end;

function THugeInt.EqualTo(const A: Int64): Boolean;
begin
  Result := HugeIntEqualsInt64(FValue, A);
end;

function THugeInt.EqualTo(const A: THugeInt): Boolean;
begin
  Assert(Assigned(A));
  Result := HugeIntEqualsHugeInt(FValue, A.FValue);
end;

function THugeInt.Compare(const A: Word32): Integer;
begin
  Result := HugeIntCompareWord32(FValue, A);
end;

function THugeInt.Compare(const A: Int32): Integer;
begin
  Result := HugeIntCompareInt32(FValue, A);
end;

function THugeInt.Compare(const A: Int64): Integer; 
begin
  Result := HugeIntCompareInt64(FValue, A);
end;

function THugeInt.Compare(const A: THugeInt): Integer; 
begin
  Assert(Assigned(A));
  Result := HugeIntCompareHugeInt(FValue, A.FValue);
end;

procedure THugeInt.Add(const A: Int32);
begin
  HugeIntAddInt32(FValue, A);
end;

procedure THugeInt.Add(const A: THugeInt);
begin
  HugeIntAddHugeInt(FValue, A.FValue);
end;

procedure THugeInt.Inc;
begin
  HugeIntInc(FValue);
end;

procedure THugeInt.Subtract(const A: Int32);
begin
  HugeIntSubtractInt32(FValue, A);
end;

procedure THugeInt.Subtract(const A: THugeInt);
begin
  HugeIntSubtractHugeInt(FValue, A.FValue);
end;

procedure THugeInt.Dec;
begin
  HugeIntDec(FValue);
end;

procedure THugeInt.Multiply(const A: Int32);
begin
  HugeIntMultiplyInt32(FValue, A);
end;

procedure THugeInt.Multiply(const A: THugeInt);
begin
  HugeIntMultiplyHugeInt(FValue, A.FValue);
end;

procedure THugeInt.Sqr;
begin
  HugeIntSqr(FValue);
end;

procedure THugeInt.Divide(const B: Int32; out R: Int32);
begin
  HugeIntDivideInt32(FValue, B, FValue, R);
end;

procedure THugeInt.Divide(const B: THugeInt; var R: THugeInt);
begin
  HugeIntDivideHugeInt(FValue, B.FValue, FValue, R.FValue);
end;

procedure THugeInt.Power(const B: Word32);
begin
  HugeIntPower(FValue, B);
end;

function THugeInt.ToStr: UTF8String;
begin
  Result := HugeIntToStrB(FValue);
end;

function THugeInt.ToHex: UTF8String;
begin
  Result := HugeIntToHexB(FValue);
end;

procedure THugeInt.AssignStr(const A: RawByteString);
begin
  StrToHugeIntB(A, FValue);
end;

procedure THugeInt.AssignHex(const A: RawByteString);
begin
  HexToHugeIntB(A, FValue);
end;

procedure THugeInt.ISqrt;
begin
  HugeIntISqrt(FValue);
end;

procedure THugeInt.Random(const Size: Integer);
begin
  HugeIntRandom(FValue, Size);
end;



{                                                                              }
{ Tests                                                                        }
{                                                                              }
{$IFDEF HUGEINT_TEST}
{$ASSERTIONS ON}
procedure Test_HugeWord;
var A, B, C, D : HugeWord;
    X, Y : HugeInt;
    I : Integer;
    S : UTF8String;
    F : Word32;
begin
  HugeWordInit(A);
  HugeWordInit(B);
  HugeWordInit(C);
  HugeWordInit(D);
  HugeIntInit(X);
  HugeIntInit(Y);

  // Zero
  HugeWordAssignZero(A);
  Assert(HugeWordGetSize(A) = 0);
  Assert(HugeWordIsZero(A));
  Assert(HugeWordToWord32(A) = 0);
  Assert(HugeWordToInt32(A) = 0);
  Assert(HugeWordToInt64(A) = 0);
  Assert(HugeWordCompareWord32(A, 0) = 0);
  Assert(HugeWordCompareWord32(A, 1) = -1);
  Assert(HugeWordCompare(A, A) = 0);
  Assert(HugeWordIsWord32Range(A));
  Assert(HugeWordIsWord64Range(A));
  Assert(HugeWordIsInt32Range(A));
  Assert(HugeWordIsInt64Range(A));
  Assert(HugeWordIsEven(A));
  Assert(not HugeWordIsOdd(A));
  Assert(HugeWordToStrB(A) = '0');
  Assert(HugeWordToHexB(A) = '00000000');
  Assert(HugeWordSetBitScanForward(A) = -1);
  Assert(HugeWordSetBitScanReverse(A) = -1);
  Assert(HugeWordClearBitScanForward(A) = 0);
  Assert(HugeWordClearBitScanReverse(A) = 0);
  Assert(HugeWordToDouble(A) = 0.0);

  // One
  HugeWordAssignOne(A);
  Assert(not HugeWordIsEven(A));
  Assert(HugeWordIsOdd(A));
  Assert(not HugeWordIsZero(A));
  Assert(HugeWordIsOne(A));
  Assert(HugeWordToInt32(A) = 1);
  Assert(HugeWordCompareWord32(A, 0) = 1);
  Assert(HugeWordToHexB(A) = '00000001');
  Assert(HugeWordSetBitScanForward(A) = 0);
  Assert(HugeWordSetBitScanReverse(A) = 0);
  Assert(HugeWordClearBitScanForward(A) = 1);
  Assert(HugeWordClearBitScanReverse(A) = 1);
  Assert(HugeWordToDouble(A) = 1.0);

  // $FFFFFFFF
  HugeWordAssignZero(A);
  HugeWordAddWord32(A, $FFFFFFFF);
  Assert(HugeWordGetSize(A) = 1);
  Assert(HugeWordGetElement(A, 0) = $FFFFFFFF);
  Assert(HugeWordIsWord32Range(A));
  Assert(not HugeWordIsInt32Range(A));
  Assert(HugeWordIsInt64Range(A));
  Assert(HugeWordToWord32(A) = $FFFFFFFF);
  Assert(HugeWordToInt64(A) = $FFFFFFFF);
  Assert(not HugeWordIsZero(A));
  Assert(HugeWordCompareWord32(A, 0) = 1);
  HugeWordAddWord32(A, $FFFFFFFF);
  Assert(HugeWordGetSize(A) = 2);
  Assert((HugeWordGetElement(A, 0) = $FFFFFFFE) and (HugeWordGetElement(A, 1) = 1));
  Assert(not HugeWordIsWord32Range(A));
  Assert(HugeWordToInt64(A) = $1FFFFFFFE);
  HugeWordAddWord32(A, $FFFFFFFF);
  Assert(HugeWordGetSize(A) = 2);
  Assert((HugeWordGetElement(A, 0) = $FFFFFFFD) and (HugeWordGetElement(A, 1) = 2));
  Assert(HugeWordToInt64(A) = $2FFFFFFFD);
  Assert(HugeWordSubtractWord32(A, $FFFFFFFF) = 1);
  Assert(HugeWordGetSize(A) = 2);
  Assert((HugeWordGetElement(A, 0) = $FFFFFFFE) and (HugeWordGetElement(A, 1) = 1));
  Assert(HugeWordSubtractWord32(A, $FFFFFFFF) = 1);
  Assert(HugeWordToWord32(A) = $FFFFFFFF);
  Assert(HugeWordSubtractWord32(A, $FFFFFFFF) = 0);
  Assert(HugeWordToWord32(A) = 0);
  Assert(HugeWordSubtractWord32(A, $FFFFFFFF) = -1);
  Assert(HugeWordToWord32(A) = $FFFFFFFF);
  Assert(HugeWordToHexB(A) = 'FFFFFFFF');
  Assert(HugeWordSetBitScanForward(A) = 0);
  Assert(HugeWordSetBitScanReverse(A) = 31);
  Assert(HugeWordClearBitScanForward(A) = 32);
  Assert(HugeWordClearBitScanReverse(A) = 32);
  Assert(HugeWordToDouble(A) = 4294967295.0);

  // $80000000
  HugeWordAssignWord32(A, $80000000);
  Assert(HugeWordIsWord32Range(A));
  Assert(not HugeWordIsInt32Range(A));
  Assert(HugeWordIsInt64Range(A));
  Assert(HugeWordToWord32(A) = $80000000);
  Assert(HugeWordEqualsWord32(A, $80000000));
  Assert(HugeWordSetBitScanForward(A) = 31);
  Assert(HugeWordSetBitScanReverse(A) = 31);
  Assert(HugeWordClearBitScanForward(A) = 0);
  Assert(HugeWordClearBitScanReverse(A) = 32);

  // $100000000
  HugeWordAssignWord32(A, $80000000);
  HugeWordAdd(A, A);
  Assert(HugeWordToInt64(A) = $100000000);
  Assert(not HugeWordIsWord32Range(A));
  Assert(HugeWordEqualsInt64(A, $100000000));
  Assert(HugeWordToHexB(A) = '0000000100000000');
  Assert(HugeWordSetBitScanForward(A) = 32);
  Assert(HugeWordSetBitScanReverse(A) = 32);
  Assert(HugeWordClearBitScanForward(A) = 0);
  Assert(HugeWordClearBitScanReverse(A) = 33);
  Assert(HugeWordToDouble(A) = 4294967296.0);

  // $1234567890ABCDEF
  HugeWordAssignInt64(A, $1234567890ABCDEF);
  Assert(HugeWordToInt64(A) = $1234567890ABCDEF);
  Assert(not HugeWordIsWord32Range(A));
  Assert(not HugeWordIsZero(A));
  Assert(HugeWordIsInt64Range(A));
  Assert(HugeWordToHexB(A) = '1234567890ABCDEF');
  Assert(Abs(HugeWordToDouble(A) - 1311768467294899695.0) <= 1E12);

  // $7654321800000000
  HugeWordAssignInt64(A, $7654321800000000);
  Assert(HugeWordToInt64(A) = $7654321800000000);
  Assert(not HugeWordIsZero(A));
  Assert(not HugeWordIsWord32Range(A));
  Assert(not HugeWordIsInt32Range(A));
  Assert(HugeWordIsInt64Range(A));
  Assert(HugeWordToStrB(A) = '8526495073179795456');
  Assert(HugeWordToDouble(A) = 8526495073179795456.0);
  Assert(HugeWordToHexB(A) = '7654321800000000');

  // Swap
  HugeWordAssignInt32(A, 0);
  HugeWordAssignInt32(B, 1);
  HugeWordSwap(A, B);
  Assert(HugeWordToInt32(A) = 1);
  Assert(HugeWordToInt32(B) = 0);

  // Compare/Subtract
  HugeWordAssignZero(A);
  HugeWordAssignInt64(B, $FFFFFFFF);
  Assert(HugeWordToWord32(B) = $FFFFFFFF);
  Assert(HugeWordCompare(A, B) = -1);
  Assert(HugeWordCompare(B, A) = 1);
  Assert(HugeWordCompareWord32(B, $FFFFFFFF) = 0);
  Assert(HugeWordCompareWord32(B, 0) = 1);
  Assert(not HugeWordEquals(A, B));
  Assert(HugeWordEquals(B, B));
  HugeWordAdd(A, B);
  Assert(HugeWordGetSize(A) = 1);
  Assert(HugeWordGetElement(A, 0) = $FFFFFFFF);
  HugeWordAdd(A, B);
  Assert(HugeWordGetSize(A) = 2);
  Assert((HugeWordGetElement(A, 0) = $FFFFFFFE) and (HugeWordGetElement(A, 1) = 1));
  Assert(HugeWordCompare(A, B) = 1);
  Assert(HugeWordCompare(B, A) = -1);
  HugeWordAdd(A, B);
  Assert(HugeWordGetSize(A) = 2);
  Assert((HugeWordGetElement(A, 0) = $FFFFFFFD) and (HugeWordGetElement(A, 1) = 2));
  Assert(HugeWordSubtract(A, B) = 1);
  Assert(HugeWordGetSize(A) = 2);
  Assert((HugeWordGetElement(A, 0) = $FFFFFFFE) and (HugeWordGetElement(A, 1) = 1));
  Assert(HugeWordSubtract(A, B) = 1);
  Assert(HugeWordToWord32(A) = $FFFFFFFF);
  Assert(HugeWordSubtract(A, B) = 0);
  Assert(HugeWordToWord32(A) = 0);
  Assert(HugeWordSubtract(A, B) = -1);
  Assert(HugeWordToWord32(A) = $FFFFFFFF);

  // And/Or/Xor/Not
  HugeWordAssignInt64(A, $1234678FFFFFFFF);
  HugeWordAssignWord32(B, 0);
  HugeWordAndHugeWord(B, A);
  Assert(HugeWordToInt64(B) = 0);
  HugeWordOrHugeWord(B, A);
  Assert(HugeWordToInt64(B) = $1234678FFFFFFFF);
  HugeWordXorHugeWord(B, A);
  Assert(HugeWordToInt64(B) = 0);
  HugeWordAssignInt64(A, $FFFFFFFF);
  HugeWordNot(A);
  Assert(HugeWordToWord32(A) = 0);

  // Shl/Shr
  HugeWordAssignWord32(A, $101);
  HugeWordShr(A, 1);
  Assert(HugeWordToWord32(A) = $80);
  HugeWordShl(A, 1);
  Assert(HugeWordToWord32(A) = $100);
  HugeWordShl1(A);
  Assert(HugeWordToWord32(A) = $200);
  HugeWordShr1(A);
  Assert(HugeWordToWord32(A) = $100);

  // Shl1/Shl/Shr1/Shr
  HugeWordAssignWord32(A, 1);
  HugeWordAssignWord32(B, 1);
  for I := 0 to 50 do
    begin
      Assert(HugeWordToInt64(A) = Int64(1) shl I);
      Assert(HugeWordToInt64(B) = Int64(1) shl I);
      HugeWordShl1(A);
      HugeWordShl(B, 1);
    end;
  for I := 1 to 32 do
    HugeWordShl1(A);
  HugeWordShl(B, 32);
  Assert(HugeWordEquals(A, B));
  for I := 1 to 1000 do
    HugeWordShl1(A);
  HugeWordShl(B, 1000);
  Assert(HugeWordEquals(A, B));
  for I := 1 to 1032 do
    HugeWordShr1(A);
  HugeWordShr(B, 1000);
  HugeWordShr(B, 32);
  HugeWordNormalise(A);
  HugeWordNormalise(B);
  Assert(HugeWordEquals(A, B));
  for I := 51 downto 1 do
    begin
      Assert(HugeWordToInt64(A) = Int64(1) shl I);
      Assert(HugeWordToInt64(B) = Int64(1) shl I);
      HugeWordShr1(A);
      HugeWordShr(B, 1);
      HugeWordNormalise(A);
      HugeWordNormalise(B);
    end;

  // Shl/Shr
  HugeWordAssignInt64(A, $1234678FFFFFFFF);
  HugeWordShl1(A);
  Assert(HugeWordToInt64(A) = $2468CF1FFFFFFFE);
  HugeWordShr1(A);
  Assert(HugeWordToInt64(A) = $1234678FFFFFFFF);

  // Add/Subtract
  HugeWordAssignZero(A);
  HugeWordAddWord32(A, 1);
  Assert(HugeWordToWord32(A) = 1);
  Assert(HugeWordSubtractWord32(A, 1) = 0);
  Assert(HugeWordToWord32(A) = 0);
  Assert(HugeWordSubtractWord32(A, 1) = -1);
  Assert(HugeWordToWord32(A) = 1);

  // Add/Subtract
  HugeWordAssignZero(A);
  HugeWordAssignWord32(B, 1);
  HugeWordAdd(A, B);
  Assert(HugeWordToWord32(A) = 1);
  Assert(HugeWordSubtract(A, B) = 0);
  Assert(HugeWordToWord32(A) = 0);
  Assert(HugeWordSubtract(A, B) = -1);
  Assert(HugeWordToWord32(A) = 1);

  // Add/Subtract
  HugeWordAssignInt64(A, $FFFFFFFF);
  HugeWordAddWord32(A, 1);
  Assert(HugeWordGetSize(A) = 2);
  Assert((HugeWordGetElement(A, 0) = 0) and (HugeWordGetElement(A, 1) = 1));
  Assert(HugeWordSubtractWord32(A, 1) = 1);
  Assert(HugeWordToWord32(A) = $FFFFFFFF);
  Assert(HugeWordSubtractWord32(A, 1) = 1);
  Assert(HugeWordToWord32(A) = $FFFFFFFE);

  // Add/Subtract
  HugeWordAssignInt64(A, $FFFFFFFF);
  HugeWordAssignWord32(B, 1);
  HugeWordAdd(A, B);
  Assert(HugeWordGetSize(A) = 2);
  Assert((HugeWordGetElement(A, 0) = 0) and (HugeWordGetElement(A, 1) = 1));
  Assert(HugeWordSubtract(A, B) = 1);
  Assert(HugeWordToWord32(A) = $FFFFFFFF);
  Assert(HugeWordSubtract(A, B) = 1);
  Assert(HugeWordToWord32(A) = $FFFFFFFE);

  // Add/Subtract
  StrToHugeWordB('111111111111111111111111111111111111111111111111111111111', A);
  StrToHugeWordB('222222222222222222222222222222222222222222222222222222222', B);
  HugeWordAdd(A, B);
  Assert(HugeWordToStrB(A) = '333333333333333333333333333333333333333333333333333333333');
  HugeWordSubtract(A, B);
  Assert(HugeWordToStrB(A) = '111111111111111111111111111111111111111111111111111111111');
  HugeWordSubtract(A, A);
  Assert(HugeWordIsZero(A));

  // Multiply/Divide
  HugeWordAssignWord32(A, $10000000);
  HugeWordAssignWord32(B, $20000000);
  HugeWordMultiply(C, A, B);
  Assert(HugeWordToInt64(C) = $200000000000000);
  HugeWordDivide(C, B, D, C);
  Assert(HugeWordToInt64(D) = $10000000);
  Assert(HugeWordIsZero(C));

  // Multiply/Divide
  StrToHugeWordB('111111111111111111111111111111111111', A);
  StrToHugeWordB('100000000000000000000000000000000000', B);
  HugeWordMultiply(C, A, B);
  Assert(HugeWordToStrB(A) = '111111111111111111111111111111111111');
  Assert(HugeWordToStrB(C) = '11111111111111111111111111111111111100000000000000000000000000000000000');
  HugeWordDivide(C, B, D, C);
  Assert(HugeWordToStrB(D) = '111111111111111111111111111111111111');
  Assert(HugeWordToStrB(C) = '0');
  HugeWordMultiplyWord8(D, 10);
  Assert(HugeWordToStrB(D) = '1111111111111111111111111111111111110');
  HugeWordMultiplyWord16(D, 100);
  Assert(HugeWordToStrB(D) = '111111111111111111111111111111111111000');
  HugeWordMultiplyWord32(D, 1000);
  Assert(HugeWordToStrB(D) = '111111111111111111111111111111111111000000');
  HugeWordDivideWord32(D, 1000000, D, F);
  Assert(HugeWordToStrB(D) = '111111111111111111111111111111111111');
  Assert(F = 0);
  StrToHugeWordB('1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111', A);
  StrToHugeWordB('1000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000', B);
  HugeWordMultiply(C, A, B);
  Assert(HugeWordToStrB(C) = '1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000');

  // Multiply_ShiftAdd
  StrToHugeWordB('111111111111111111111111111111111111', A);
  StrToHugeWordB('100000000000000000000000000000000000', B);
  HugeWordMultiply_ShiftAdd(C, A, B);
  Assert(HugeWordToStrB(C) = '11111111111111111111111111111111111100000000000000000000000000000000000');
  StrToHugeWordB('1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111', A);
  StrToHugeWordB('1000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000', B);
  HugeWordMultiply_ShiftAdd(C, A, B);
  Assert(HugeWordToStrB(C) = '1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000');

  // Multiply_Karatsuba
  StrToHugeWordB('111111111111111111111111111111111111', A);
  StrToHugeWordB('100000000000000000000000000000000000', B);
  HugeWordMultiply_Karatsuba(C, A, B);
  Assert(HugeWordToStrB(C) = '11111111111111111111111111111111111100000000000000000000000000000000000');
  StrToHugeWordB('1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111', A);
  StrToHugeWordB('1000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000', B);
  HugeWordMultiply_Karatsuba(C, A, B);
  Assert(HugeWordToStrB(C) = '1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000');

  // ISqrt/Sqr
  HugeWordAssignWord32(A, $FFFF);
  HugeWordISqrt(A);
  Assert(HugeWordToInt64(A) = $FF);
  HugeWordAssignWord32(A, $10000);
  HugeWordISqrt(A);
  Assert(HugeWordToInt64(A) = $100);
  HugeWordAssignInt64(A, $FFFFFFFF);
  HugeWordISqrt(A);
  Assert(HugeWordToInt64(A) = $FFFF);
  HugeWordAssignInt64(A, $100000000);
  HugeWordISqrt(A);
  Assert(HugeWordToInt64(A) = $10000);
  HugeWordAssignInt64(A, $10000FFFF);
  HugeWordISqrt(A);
  Assert(HugeWordToInt64(A) = $10000);
  StrToHugeWordB('10000000000000000000000000000000000000000', A);
  HugeWordISqrt(A);
  Assert(HugeWordToStrB(A) = '100000000000000000000');
  HugeWordSqr(A, A);
  Assert(HugeWordToStrB(A) = '10000000000000000000000000000000000000000');
  HugeWordAssignWord32(A, $10000000);
  HugeWordSqr(A, A);
  Assert(HugeWordToInt64(A) = $100000000000000);
  HugeWordISqrt(A);
  Assert(HugeWordToInt64(A) = $10000000);

  // GCD
  HugeWordAssignWord32(A, 111);
  HugeWordAssignWord32(B, 159);
  HugeWordGCD(A, B, C);
  Assert(HugeWordToStrB(C) = '3');

  // GCD
  StrToHugeWordB('359334085968622831041960188598043661065388726959079837', A);   // Bell number prime
  StrToHugeWordB('1298074214633706835075030044377087', B);                       // Carol prime
  HugeWordGCD(A, B, C);
  Assert(HugeWordToStrB(C) = '1');

  // PowerAndMod
  HugeWordAssignWord32(A, 3);
  HugeWordAssignWord32(B, 500);
  HugeWordAssignWord32(C, 5);
  HugeWordPowerAndMod(D, A, B, C);
  Assert(HugeWordToStrB(D) = '1');

  // PowerAndMod
  HugeWordAssignWord32(A, 3);
  HugeWordAssignWord32(B, 123456);
  HugeWordAssignWord32(C, 7);
  HugeWordPowerAndMod(D, A, B, C);
  Assert(HugeWordToStrB(D) = '1');

  // PowerAndMod
  HugeWordAssignWord32(A, 2905);
  HugeWordAssignWord32(B, 323);
  HugeWordAssignWord32(C, 245363);
  HugeWordPowerAndMod(D, A, B, C);
  Assert(HugeWordToStrB(D) = '13388');

  // PowerAndMod
  StrToHugeWordB('9999999999', A);
  HugeWordAssignWord32(B, 10);
  HugeWordPower(B, 100);
  HugeWordAssignWord32(C, 700);
  HugeWordPowerAndMod(D, A, B, C);
  Assert(HugeWordToStrB(D) = '501');

  // Power/Mod
  HugeWordAssignWord32(A, 3);
  HugeWordAssignWord32(C, 5);
  HugeWordPower(A, 500);
  Assert(HugeWordToStrB(A) =
      '36360291795869936842385267079543319118023385026001623040346035832580600191583895' +
      '48419850826297938878330817970253440385575285593151701306614299243091656202578002' +
      '1771247847643450125342836565813209972590371590152578728008385990139795377610001');
  HugeWordMod(A, C, D);
  Assert(HugeWordToStrB(D) = '1');

  // Power/Mod
  HugeWordAssignWord32(A, 3);
  HugeWordAssignWord32(C, 7);
  HugeWordPower(A, 123456);
  HugeWordMod(A, C, D);
  Assert(HugeWordToStrB(D) = '1');

  // Power
  HugeWordAssignZero(A);
  HugeWordPower(A, 0);
  Assert(HugeWordToInt32(A) = 1);
  HugeWordAssignZero(A);
  HugeWordPower(A, 1);
  Assert(HugeWordToInt32(A) = 0);
  HugeWordAssignOne(A);
  HugeWordPower(A, 0);
  Assert(HugeWordToInt32(A) = 1);
  HugeWordAssignOne(A);
  HugeWordPower(A, 1);
  Assert(HugeWordToInt32(A) = 1);

  // AssignDouble
  HugeWordAssignDouble(A, 0.0);
  Assert(HugeWordToInt64(A) = 0);
  HugeWordAssignDouble(A, 1.0);
  Assert(HugeWordToInt64(A) = 1);
  HugeWordAssignDouble(A, 4294967295.0);
  Assert(HugeWordToInt64(A) = $FFFFFFFF);
  HugeWordAssignDouble(A, 4294967296.0);
  Assert(HugeWordToInt64(A) = $100000000);

  // HexTo/ToHex
  HexToHugeWordB('0', A);
  Assert(HugeWordToHexB(A) = '00000000');
  StrToHugeWordB('123456789', A);
  Assert(HugeWordToHexB(A) = '075BCD15');
  HexToHugeWordB('123456789ABCDEF', A);
  Assert(HugeWordToHexB(A) = '0123456789ABCDEF');
  Assert(HugeWordToStrB(A) = '81985529216486895');
  HexToHugeWordB('0123456789ABCDEF00112233F', A);
  Assert(HugeWordToHexB(A) = '00000000123456789ABCDEF00112233F');

  // StrTo/ToStr
  StrToHugeWordB('12345', A);
  Assert(HugeWordToWord32(A) = 12345);
  Assert(HugeWordToStrB(A) = '12345');

  // StrTo/ToStr
  S := '123456789012345678901234567890123456789012345678901234567890';
  StrToHugeWordB(S, A);
  for I := 1 to 100 do
    begin
      HugeWordMultiplyWord8(A, 10);
      S := S + '0';
      Assert(HugeWordToStrB(A) = S);
      StrToHugeWordB(S, B);
      Assert(HugeWordEquals(A, B));
    end;

  // Prime
  HugeWordAssignWord32(A, 1);
  Assert(HugeWordIsPrime(A) = pNotPrime);
  HugeWordAssignWord32(A, 31);
  Assert(HugeWordIsPrime(A) = pPrime);
  HugeWordAssignWord32(A, 982451653);
  Assert(HugeWordIsPrime(A) <> pNotPrime);
  HugeWordAssignWord32(A, 3464946713);
  Assert(HugeWordIsPrime(A) <> pNotPrime);
  HugeWordAssignWord32(A, 3464946767);
  Assert(HugeWordIsPrime(A) = pNotPrime);
  HugeWordAssignWord32(A, 3464946769);
  Assert(HugeWordIsPrime(A) <> pNotPrime);
  StrToHugeWordB('359334085968622831041960188598043661065388726959079837', A);     // Bell number prime
  Assert(HugeWordIsPrime(A) <> pNotPrime);
  StrToHugeWordB('1298074214633706835075030044377087', A);                         // Carol prime
  Assert(HugeWordIsPrime(A) <> pNotPrime);
  StrToHugeWordB('393050634124102232869567034555427371542904833', A);              // Cullen prime
  Assert(HugeWordIsPrime(A) <> pNotPrime);
  StrToHugeWordB('8683317618811886495518194401279999999', A);                      // Factorial prime
  Assert(HugeWordIsPrime(A) <> pNotPrime);
  StrToHugeWordB('19134702400093278081449423917', A);                              // Fibonacci prime
  Assert(HugeWordIsPrime(A) <> pNotPrime);
  StrToHugeWordB('1363005552434666078217421284621279933627102780881053358473', A); // Padovan prime
  Assert(HugeWordIsPrime(A) <> pNotPrime);
  StrToHugeWordB('1363005552434666078217421284621279933627102780881053358473', A); // Padovan prime
  HugeWordNextPotentialPrime(A);
  Assert(HugeWordToStrB(A) = '1363005552434666078217421284621279933627102780881053358551');
  HugeWordAssignWord32(A, 340561);                                                 // Carmichael number 340561 = 13 * 17 * 23 * 67
  Assert(HugeWordIsPrime(A) = pNotPrime);
  HugeWordAssignWord32(A, 82929001);                                               // Carmichael number 82929001 = 281 * 421 * 701
  Assert(HugeWordIsPrime(A) = pNotPrime);
  StrToHugeWordB('975177403201', A);                                               // Carmichael number 975177403201 = 2341 * 2861 * 145601
  Assert(HugeWordIsPrime(A) = pNotPrime);
  StrToHugeWordB('989051977369', A);                                               // Carmichael number 989051977369 = 173 * 36809 * 155317
  Assert(HugeWordIsPrime(A) = pNotPrime);
  StrToHugeWordB('999629786233', A);                                               // Carmichael number 999629786233 = 13 * 43 * 127 * 1693 * 8317
  Assert(HugeWordIsPrime(A) = pNotPrime);

  // ExtendedEuclid
  HugeWordAssignWord32(A, 120);
  HugeWordAssignWord32(B, 23);
  HugeWordExtendedEuclid(A, B, C, X, Y);
  Assert(HugeWordToWord32(C) = 1);
  Assert(HugeIntToInt32(X) = -9);
  Assert(HugeIntToInt32(Y) = 47);

  // ExtendedEuclid
  HugeWordAssignWord32(A, 11391);
  HugeWordAssignWord32(B, 5673);
  HugeWordExtendedEuclid(A, B, C, X, Y);
  Assert(HugeWordToWord32(C) = 3);
  Assert(HugeIntToInt32(X) = -126);
  Assert(HugeIntToInt32(Y) = 253);

  // ModInv
  HugeWordAssignWord32(A, 3);
  HugeWordAssignWord32(B, 26);
  Assert(HugeWordModInv(A, B, C));
  Assert(HugeWordToWord32(C) = 9);

  // ModInv
  HugeWordAssignWord32(A, 6);
  HugeWordAssignWord32(B, 3);
  Assert(not HugeWordModInv(A, B, C));

  // ModInv
  HugeWordAssignWord32(A, 31);
  HugeWordAssignWord32(B, 8887231);
  Assert(HugeWordModInv(A, B, C));
  Assert(HugeWordToWord32(C) = 2293479);

  // ModInv
  HugeWordAssignWord32(A, 999961543);
  StrToHugeWordB('3464946713311', B);
  Assert(HugeWordModInv(A, B, C));
  Assert(HugeWordToStrB(C) = '2733464305244');

  HugeIntFinalise(Y);
  HugeIntFinalise(X);
  HugeWordFinalise(D);
  HugeWordFinalise(C);
  HugeWordFinalise(B);
  HugeWordFinalise(A);
end;

procedure Test_HugeInt;
var A, B, C, D : HugeInt;
    F : HugeWord;
    K : Word32;
    L : Int32;
begin
  HugeIntInit(A);
  HugeIntInit(B);
  HugeIntInit(C);
  HugeIntInit(D);
  HugeWordInit(F);

  // Zero
  HugeIntAssignZero(A);
  Assert(HugeIntIsZero(A));
  Assert(HugeIntIsPositiveOrZero(A));
  Assert(HugeIntIsNegativeOrZero(A));
  Assert(not HugeIntIsPositive(A));
  Assert(not HugeIntIsNegative(A));
  Assert(HugeIntIsInt32Range(A));
  Assert(HugeIntIsWord32Range(A));
  Assert(HugeIntToStrB(A) = '0');
  Assert(HugeIntToHexB(A) = '00000000');
  Assert(HugeIntToWord32(A) = 0);
  Assert(HugeIntToInt32(A) = 0);
  Assert(HugeIntToDouble(A) = 0.0);
  StrToHugeIntB('0', A);
  Assert(HugeIntIsZero(A));
  Assert(HugeIntCompareInt64(A, MinInt64 { -$8000000000000000 }) = 1);
  Assert(HugeIntCompareInt64(A, $7FFFFFFFFFFFFFFF) = -1);
  Assert(not HugeIntEqualsInt64(A, MinInt64 { -$8000000000000000 }));
  HugeIntAddInt32(A, 0);
  Assert(HugeIntIsZero(A));
  HugeIntSubtractInt32(A, 0);
  Assert(HugeIntIsZero(A));
  HugeIntMultiplyInt8(A, 0);
  Assert(HugeIntIsZero(A));
  HugeIntMultiplyInt8(A, 1);
  Assert(HugeIntIsZero(A));
  HugeIntMultiplyInt8(A, -1);
  Assert(HugeIntIsZero(A));
  HugeIntMultiplyWord8(A, 0);
  Assert(HugeIntIsZero(A));
  HugeIntMultiplyWord8(A, 1);
  Assert(HugeIntIsZero(A));
  HugeIntMultiplyHugeWord(A, F);
  Assert(HugeIntIsZero(A));
  HugeIntMultiplyHugeInt(A, A);
  Assert(HugeIntIsZero(A));
  HugeIntSqr(A);
  Assert(HugeIntIsZero(A));
  HugeIntISqrt(A);
  Assert(HugeIntIsZero(A));

  // One
  HugeIntAssignOne(A);
  Assert(not HugeIntIsZero(A));
  Assert(HugeIntIsPositiveOrZero(A));
  Assert(not HugeIntIsNegativeOrZero(A));
  Assert(HugeIntIsOne(A));
  Assert(not HugeIntIsMinusOne(A));
  Assert(HugeIntToStrB(A) = '1');
  Assert(HugeIntToHexB(A) = '00000001');
  Assert(HugeIntIsPositive(A));
  Assert(not HugeIntIsNegative(A));
  Assert(HugeIntIsInt32Range(A));
  Assert(HugeIntIsWord32Range(A));
  Assert(HugeIntToDouble(A) = 1.0);
  StrToHugeIntB('1', A);
  Assert(HugeIntIsOne(A));
  Assert(HugeIntCompareInt64(A, MinInt64 { -$8000000000000000 }) = 1);
  Assert(HugeIntCompareInt64(A, $7FFFFFFFFFFFFFFF) = -1);
  Assert(not HugeIntEqualsInt64(A, MinInt64 { -$8000000000000000 }));
  HugeIntAddInt32(A, 0);
  Assert(HugeIntIsOne(A));
  HugeIntSubtractInt32(A, 0);
  Assert(HugeIntIsOne(A));
  HugeIntMultiplyInt8(A, 1);
  Assert(HugeIntIsOne(A));
  HugeIntMultiplyInt8(A, -1);
  Assert(HugeIntIsMinusOne(A));
  HugeIntMultiplyInt8(A, -1);
  Assert(HugeIntIsOne(A));
  HugeIntMultiplyWord8(A, 1);
  Assert(HugeIntIsOne(A));
  HugeIntSqr(A);
  Assert(HugeIntIsOne(A));
  HugeIntISqrt(A);
  Assert(HugeIntIsOne(A));

  // MinusOne
  HugeIntAssignMinusOne(A);
  Assert(not HugeIntIsZero(A));
  Assert(not HugeIntIsPositiveOrZero(A));
  Assert(HugeIntIsNegativeOrZero(A));
  Assert(not HugeIntIsOne(A));
  Assert(HugeIntIsMinusOne(A));
  Assert(HugeIntToStrB(A) = '-1');
  Assert(HugeIntToHexB(A) = '-00000001');
  Assert(not HugeIntIsPositive(A));
  Assert(HugeIntIsNegative(A));
  Assert(HugeIntIsInt32Range(A));
  Assert(HugeIntIsInt64Range(A));
  Assert(not HugeIntIsWord32Range(A));
  Assert(HugeIntToDouble(A) = -1.0);
  StrToHugeIntB('-1', A);
  Assert(HugeIntIsMinusOne(A));
  Assert(HugeIntCompareInt64(A, MinInt64 { -$8000000000000000 }) = 1);
  Assert(HugeIntCompareInt64(A, $7FFFFFFFFFFFFFFF) = -1);
  Assert(not HugeIntEqualsInt64(A, MinInt64 { -$8000000000000000 }));
  HugeIntMultiplyInt8(A, 1);
  Assert(HugeIntIsMinusOne(A));
  HugeIntAddWord32(A, 1);
  Assert(HugeIntIsZero(A));
  HugeIntAddInt32(A, -1);
  Assert(HugeIntIsMinusOne(A));
  HugeIntAddInt32(A, 0);
  Assert(HugeIntIsMinusOne(A));
  HugeIntSubtractInt32(A, 0);
  Assert(HugeIntIsMinusOne(A));
  HugeWordAssignHugeIntAbs(F, A);
  Assert(HugeWordIsOne(F));

  // MinInt64 (-$8000000000000000)
  HugeIntAssignInt64(A, MinInt64 { -$8000000000000000 });
  Assert(HugeIntToInt64(A) = MinInt64 { -$8000000000000000 });
  Assert(HugeIntToStrB(A) = '-9223372036854775808');
  Assert(HugeIntToHexB(A) = '-8000000000000000');
  Assert(HugeIntToDouble(A) = -9223372036854775808.0);
  Assert(HugeIntEqualsInt64(A, MinInt64 { -$8000000000000000 }));
  Assert(not HugeIntEqualsInt64(A, MinInt32 { -$80000000 }));
  Assert(HugeIntCompareInt64(A, MinInt64 { -$8000000000000000 }) = 0);
  Assert(HugeIntCompareInt64(A, -$7FFFFFFFFFFFFFFF) = -1);
  Assert(not HugeIntIsInt32Range(A));
  Assert(HugeIntIsInt64Range(A));
  StrToHugeIntB('-9223372036854775808', A);
  Assert(HugeIntToStrB(A) = '-9223372036854775808');
  HugeIntAbsInPlace(A);
  Assert(HugeIntToStrB(A) = '9223372036854775808');
  Assert(HugeIntToHexB(A) = '8000000000000000');
  Assert(not HugeIntEqualsInt64(A, MinInt64 { -$8000000000000000 }));
  Assert(HugeIntCompareInt64(A, MinInt64 { -$8000000000000000 }) = 1);
  Assert(not HugeIntIsInt64Range(A));
  HugeIntNegate(A);
  Assert(HugeIntToInt64(A) = MinInt64 { -$8000000000000000 });

  // MinInt64 + 1 (-$7FFFFFFFFFFFFFFF)
  HugeIntAssignInt64(A, -$7FFFFFFFFFFFFFFF);
  Assert(HugeIntToInt64(A) = -$7FFFFFFFFFFFFFFF);
  Assert(HugeIntToStrB(A) = '-9223372036854775807');
  Assert(HugeIntToHexB(A) = '-7FFFFFFFFFFFFFFF');
  {$IFNDEF DELPHIXE2_UP}
  {$IFNDEF FREEPASCAL}
  {$IFNDEF CPU_32}
  Assert(HugeIntToDouble(A) = Double(-9223372036854775807.0));
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
  Assert(HugeIntEqualsInt64(A, -$7FFFFFFFFFFFFFFF));
  Assert(not HugeIntEqualsInt64(A, MinInt64 { -$8000000000000000 }));
  Assert(HugeIntCompareInt64(A, -$7FFFFFFFFFFFFFFE) = -1);
  Assert(HugeIntCompareInt64(A, -$7FFFFFFFFFFFFFFF) = 0);
  Assert(HugeIntCompareInt64(A, MinInt64 { -$8000000000000000 }) = 1);
  Assert(HugeIntIsInt64Range(A));
  HugeIntAbsInPlace(A);
  Assert(HugeIntToStrB(A) = '9223372036854775807');
  Assert(HugeIntToHexB(A) = '7FFFFFFFFFFFFFFF');
  Assert(HugeIntToInt64(A) = $7FFFFFFFFFFFFFFF);
  Assert(HugeIntEqualsInt64(A, $7FFFFFFFFFFFFFFF));
  Assert(not HugeIntEqualsInt64(A, MinInt64 { -$8000000000000000 }));
  Assert(HugeIntCompareInt64(A, MinInt64 { -$8000000000000000 }) = 1);
  Assert(HugeIntIsInt64Range(A));
  HugeIntNegate(A);
  Assert(HugeIntToInt64(A) = -$7FFFFFFFFFFFFFFF);

  // MinInt64 - 1 (-$8000000000000001)
  HugeIntAssignInt64(A, MinInt64 { -$8000000000000000 });
  HugeIntSubtractInt32(A, 1);
  Assert(HugeIntToStrB(A) = '-9223372036854775809');
  Assert(HugeIntToHexB(A) = '-8000000000000001');
  {$IFNDEF DELPHIXE2_UP}
  {$IFNDEF FREEPASCAL}
  {$IFNDEF CPU_32}
  Assert(HugeIntToDouble(A) = Double(-9223372036854775809.0));
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
  Assert(not HugeIntEqualsInt64(A, MinInt64 { -$8000000000000000 }));
  Assert(HugeIntCompareInt64(A, MinInt64 { -$8000000000000000 }) = -1);
  Assert(not HugeIntIsInt64Range(A));
  HugeIntAbsInPlace(A);
  Assert(HugeIntToStrB(A) = '9223372036854775809');
  Assert(not HugeIntEqualsInt64(A, MinInt64 { -$8000000000000000 }));
  Assert(HugeIntCompareInt64(A, MinInt64 { -$8000000000000000 }) = 1);
  HugeIntNegate(A);
  Assert(HugeIntToStrB(A) = '-9223372036854775809');

  // Equals/Compare
  HugeIntAssignInt32(A, -1);
  HugeIntAssignWord32(B, 2);
  HugeIntAssignZero(C);
  Assert(HugeIntEqualsInt32(A, -1));
  Assert(not HugeIntEqualsInt32(A, 1));
  Assert(HugeIntEqualsWord32(B, 2));
  Assert(HugeIntEqualsInt32(B, 2));
  Assert(not HugeIntEqualsInt32(B, -2));
  Assert(HugeIntEqualsInt32(C, 0));
  Assert(HugeIntEqualsWord32(C, 0));
  Assert(not HugeIntEqualsWord32(C, 1));
  Assert(HugeIntEqualsInt64(C, 0));
  Assert(not HugeIntEqualsInt64(A, 1));
  Assert(HugeIntCompareWord32(A, 0) = -1);
  Assert(HugeIntCompareWord32(A, 1) = -1);
  Assert(HugeIntCompareWord32(B, 1) = 1);
  Assert(HugeIntCompareWord32(B, 2) = 0);
  Assert(HugeIntCompareWord32(C, 0) = 0);
  Assert(HugeIntCompareWord32(C, 1) = -1);
  Assert(HugeIntCompareInt32(A, 0) = -1);
  Assert(HugeIntCompareInt32(A, -1) = 0);
  Assert(HugeIntCompareInt32(A, -2) = 1);
  Assert(HugeIntCompareInt32(C, -1) = 1);
  Assert(HugeIntCompareInt32(C, 0) = 0);
  Assert(HugeIntCompareInt32(C, 1) = -1);
  Assert(HugeIntCompareInt64(A, 0) = -1);
  Assert(HugeIntCompareInt64(A, -1) = 0);
  Assert(HugeIntCompareInt64(A, -2) = 1);
  Assert(HugeIntCompareInt64(C, 0) = 0);
  Assert(HugeIntCompareInt64(C, 1) = -1);
  Assert(not HugeIntEqualsHugeInt(A, B));
  Assert(not HugeIntEqualsHugeInt(B, C));
  Assert(HugeIntEqualsHugeInt(A, A));
  Assert(HugeIntEqualsHugeInt(B, B));
  Assert(HugeIntEqualsHugeInt(C, C));
  Assert(HugeIntCompareHugeInt(A, B) = -1);
  Assert(HugeIntCompareHugeInt(B, A) = 1);
  Assert(HugeIntCompareHugeInt(A, A) = 0);
  Assert(HugeIntCompareHugeInt(B, B) = 0);
  Assert(HugeIntCompareHugeInt(C, A) = 1);
  Assert(HugeIntCompareHugeInt(C, B) = -1);
  Assert(HugeIntCompareHugeInt(C, C) = 0);
  Assert(HugeIntCompareHugeInt(A, C) = -1);
  Assert(HugeIntCompareHugeInt(B, C) = 1);
  Assert(HugeIntCompareHugeIntAbs(A, B) = -1);
  Assert(HugeIntCompareHugeIntAbs(B, A) = 1);
  Assert(HugeIntCompareHugeIntAbs(A, C) = 1);
  Assert(HugeIntCompareHugeIntAbs(B, C) = 1);
  Assert(HugeIntCompareHugeIntAbs(C, A) = -1);
  Assert(HugeIntCompareHugeIntAbs(C, B) = -1);
  Assert(HugeIntCompareHugeIntAbs(A, A) = 0);
  Assert(HugeIntCompareHugeIntAbs(B, B) = 0);
  Assert(HugeIntCompareHugeIntAbs(C, C) = 0);

  // Min/Max
  HugeIntAssignInt32(A, -1);
  HugeIntAssignInt32(B, 0);
  HugeIntAssignInt32(C, 1);
  HugeIntMin(A, B);
  Assert(HugeIntToInt32(A) = -1);
  HugeIntMin(B, A);
  Assert(HugeIntToInt32(B) = -1);
  HugeIntMax(C, A);
  Assert(HugeIntToInt32(C) = 1);
  HugeIntMax(A, C);
  Assert(HugeIntToInt32(A) = 1);

  // Swap
  HugeIntAssignInt32(A, 0);
  HugeIntAssignInt32(B, 1);
  HugeIntSwap(A, B);
  Assert(HugeIntToInt32(A) = 1);
  Assert(HugeIntToInt32(B) = 0);

  // Add/Subtract
  HugeIntAssignInt32(A, 0);
  HugeIntAssignInt32(B, 1);
  HugeIntAssignInt32(C, -1);
  HugeIntAddHugeInt(A, B);
  Assert(HugeIntToInt32(A) = 1);
  HugeIntAddHugeInt(A, B);
  Assert(HugeIntToInt32(A) = 2);
  HugeIntAddHugeInt(A, C);
  Assert(HugeIntToInt32(A) = 1);
  HugeIntAddHugeInt(A, C);
  Assert(HugeIntToInt32(A) = 0);
  HugeIntAddHugeInt(A, C);
  Assert(HugeIntToInt32(A) = -1);
  HugeIntAddHugeInt(A, C);
  Assert(HugeIntToInt32(A) = -2);
  HugeIntAddHugeInt(A, B);
  Assert(HugeIntToInt32(A) = -1);
  HugeIntAddHugeInt(A, B);
  Assert(HugeIntToInt32(A) = 0);
  HugeIntAddHugeInt(A, B);
  Assert(HugeIntToInt32(A) = 1);
  HugeIntSubtractHugeInt(A, B);
  Assert(HugeIntToInt32(A) = 0);
  HugeIntSubtractHugeInt(A, B);
  Assert(HugeIntToInt32(A) = -1);
  HugeIntSubtractHugeInt(A, B);
  Assert(HugeIntToInt32(A) = -2);
  HugeIntSubtractHugeInt(A, C);
  Assert(HugeIntToInt32(A) = -1);
  HugeIntSubtractHugeInt(A, C);
  Assert(HugeIntToInt32(A) = 0);
  HugeIntSubtractHugeInt(A, C);
  Assert(HugeIntToInt32(A) = 1);
  HugeIntSubtractHugeInt(A, C);
  Assert(HugeIntToInt32(A) = 2);

  // Add/Subtract
  HugeIntAssignInt32(A, 0);
  HugeIntAddInt32(A, 1);
  Assert(HugeIntToInt32(A) = 1);
  HugeIntAddInt32(A, -1);
  Assert(HugeIntToInt32(A) = 0);
  HugeIntAddInt32(A, -1);
  Assert(HugeIntToInt32(A) = -1);
  HugeIntAddInt32(A, -1);
  Assert(HugeIntToInt32(A) = -2);
  HugeIntAddInt32(A, 1);
  Assert(HugeIntToInt32(A) = -1);
  HugeIntAddInt32(A, 1);
  Assert(HugeIntToInt32(A) = 0);
  HugeIntAddInt32(A, 1);
  Assert(HugeIntToInt32(A) = 1);
  HugeIntAddInt32(A, 1);
  Assert(HugeIntToInt32(A) = 2);
  HugeIntSubtractInt32(A, 1);
  Assert(HugeIntToInt32(A) = 1);
  HugeIntSubtractInt32(A, 1);
  Assert(HugeIntToInt32(A) = 0);
  HugeIntSubtractInt32(A, 1);
  Assert(HugeIntToInt32(A) = -1);
  HugeIntSubtractInt32(A, 1);
  Assert(HugeIntToInt32(A) = -2);
  HugeIntSubtractInt32(A, -1);
  Assert(HugeIntToInt32(A) = -1);
  HugeIntSubtractInt32(A, -1);
  Assert(HugeIntToInt32(A) = 0);
  HugeIntSubtractInt32(A, -1);
  Assert(HugeIntToInt32(A) = 1);
  HugeIntSubtractInt32(A, -1);
  Assert(HugeIntToInt32(A) = 2);

  // Add/Subtract
  HugeIntAssignInt32(A, -1);
  HugeIntAddWord32(A, 1);
  Assert(HugeIntToInt32(A) = 0);
  HugeIntAddWord32(A, 1);
  Assert(HugeIntToInt32(A) = 1);
  HugeIntAddWord32(A, 1);
  Assert(HugeIntToInt32(A) = 2);
  HugeIntSubtractWord32(A, 1);
  Assert(HugeIntToInt32(A) = 1);
  HugeIntSubtractWord32(A, 1);
  Assert(HugeIntToInt32(A) = 0);
  HugeIntSubtractWord32(A, 1);
  Assert(HugeIntToInt32(A) = -1);
  HugeIntSubtractWord32(A, 1);
  Assert(HugeIntToInt32(A) = -2);

  // Multiply
  HugeIntAssignInt32(A, 10);
  HugeIntMultiplyWord8(A, 10);
  Assert(HugeIntToInt32(A) = 100);
  HugeIntMultiplyWord16(A, 10);
  Assert(HugeIntToInt32(A) = 1000);
  HugeIntMultiplyWord32(A, 10);
  Assert(HugeIntToInt32(A) = 10000);
  HugeIntAssignInt32(A, -10);
  HugeIntMultiplyWord8(A, 10);
  Assert(HugeIntToInt32(A) = -100);
  HugeIntMultiplyWord16(A, 10);
  Assert(HugeIntToInt32(A) = -1000);
  HugeIntMultiplyWord32(A, 10);
  Assert(HugeIntToInt32(A) = -10000);

  // Multiply
  HugeIntAssignInt32(A, -10);
  HugeIntMultiplyInt8(A, -10);
  Assert(HugeIntToInt32(A) = 100);
  HugeIntMultiplyInt8(A, 10);
  Assert(HugeIntToInt32(A) = 1000);
  HugeIntMultiplyInt8(A, -10);
  Assert(HugeIntToInt32(A) = -10000);
  HugeIntMultiplyInt8(A, 10);
  Assert(HugeIntToInt32(A) = -100000);
  HugeIntMultiplyInt8(A, 0);
  Assert(HugeIntToInt32(A) = 0);

  // Multiply
  HugeIntAssignInt32(A, -10);
  HugeIntMultiplyInt16(A, -10);
  Assert(HugeIntToInt32(A) = 100);
  HugeIntMultiplyInt16(A, 10);
  Assert(HugeIntToInt32(A) = 1000);
  HugeIntMultiplyInt16(A, -10);
  Assert(HugeIntToInt32(A) = -10000);
  HugeIntMultiplyInt16(A, 10);
  Assert(HugeIntToInt32(A) = -100000);
  HugeIntMultiplyInt16(A, 0);
  Assert(HugeIntToInt32(A) = 0);

  // Multiply
  HugeIntAssignInt32(A, -10);
  HugeIntMultiplyInt32(A, -10);
  Assert(HugeIntToInt32(A) = 100);
  HugeIntMultiplyInt32(A, 10);
  Assert(HugeIntToInt32(A) = 1000);
  HugeIntMultiplyInt32(A, -10);
  Assert(HugeIntToInt32(A) = -10000);
  HugeIntMultiplyInt32(A, 10);
  Assert(HugeIntToInt32(A) = -100000);
  HugeIntMultiplyInt32(A, 0);
  Assert(HugeIntToInt32(A) = 0);

  // Multiply
  HugeIntAssignInt32(A, 10);
  HugeIntAssignInt32(B, 10);
  HugeIntAssignInt32(C, -10);
  HugeIntMultiplyHugeInt(A, B);
  Assert(HugeIntToInt32(A) = 100);
  HugeIntMultiplyHugeInt(A, C);
  Assert(HugeIntToInt32(A) = -1000);
  HugeIntMultiplyHugeInt(A, B);
  Assert(HugeIntToInt32(A) = -10000);
  HugeIntMultiplyHugeInt(A, C);
  Assert(HugeIntToInt32(A) = 100000);
  HugeIntAssignInt32(B, 1);
  HugeIntMultiplyHugeInt(A, B);
  Assert(HugeIntToInt32(A) = 100000);
  HugeIntAssignInt32(B, -1);
  HugeIntMultiplyHugeInt(A, B);
  Assert(HugeIntToInt32(A) = -100000);
  HugeIntAssignInt32(B, 0);
  HugeIntMultiplyHugeInt(A, B);
  Assert(HugeIntToInt32(A) = 0);

  // Multiply
  HugeIntAssignInt32(A, 10);
  HugeWordAssignWord32(F, 10);
  HugeIntMultiplyHugeWord(A, F);
  Assert(HugeIntToInt32(A) = 100);
  HugeIntAssignInt32(A, -10);
  HugeIntMultiplyHugeWord(A, F);
  Assert(HugeIntToInt32(A) = -100);

  // Sqr
  HugeIntAssignInt32(A, -17);
  HugeIntSqr(A);
  Assert(HugeIntToInt32(A) = 289);

  // ISqrt
  HugeIntAssignInt32(A, 289);
  HugeIntISqrt(A);
  Assert(HugeIntToInt32(A) = 17);

  // Divide
  HugeIntAssignInt32(A, -1000);
  HugeIntDivideWord32(A, 3, B, K);
  Assert(HugeIntToInt32(B) = -333);
  Assert(K = 1);

  // Divide
  HugeIntAssignInt32(A, -1000);
  HugeIntDivideInt32(A, 3, B, L);
  Assert(HugeIntToInt32(B) = -333);
  Assert(L = 1);
  HugeIntDivideInt32(A, -3, B, L);
  Assert(HugeIntToInt32(B) = 333);
  Assert(L = 1);
  HugeIntAssignInt32(A, 1000);
  HugeIntDivideInt32(A, 3, B, L);
  Assert(HugeIntToInt32(B) = 333);
  Assert(L = 1);
  HugeIntDivideInt32(A, -3, B, L);
  Assert(HugeIntToInt32(B) = -333);
  Assert(L = 1);

  // Divide
  HugeIntAssignInt32(A, -1000);
  HugeIntAssignInt32(B, 3);
  HugeIntDivideHugeInt(A, B, C, D);
  Assert(HugeIntToInt32(C) = -333);
  Assert(HugeIntToInt32(D) = 1);
  HugeIntAssignInt32(B, -3);
  HugeIntDivideHugeInt(A, B, C, D);
  Assert(HugeIntToInt32(C) = 333);
  Assert(HugeIntToInt32(D) = 1);
  HugeIntAssignInt32(A, 1000);
  HugeIntAssignInt32(B, 3);
  HugeIntDivideHugeInt(A, B, C, D);
  Assert(HugeIntToInt32(C) = 333);
  Assert(HugeIntToInt32(D) = 1);
  HugeIntAssignInt32(B, -3);
  HugeIntDivideHugeInt(A, B, C, D);
  Assert(HugeIntToInt32(C) = -333);
  Assert(HugeIntToInt32(D) = 1);

  // Mod
  HugeIntAssignInt32(A, -1000);
  HugeIntAssignInt32(B, 3);
  HugeIntMod(A, B, C);
  Assert(HugeIntToInt32(C) = 1);

  // Power
  HugeIntAssignInt32(A, -2);
  HugeIntPower(A, 0);
  Assert(HugeIntToInt32(A) = 1);
  HugeIntAssignInt32(A, -2);
  HugeIntPower(A, 1);
  Assert(HugeIntToInt32(A) = -2);
  HugeIntAssignInt32(A, -2);
  HugeIntPower(A, 2);
  Assert(HugeIntToInt32(A) = 4);
  HugeIntAssignInt32(A, -2);
  HugeIntPower(A, 3);
  Assert(HugeIntToInt32(A) = -8);
  HugeIntAssignInt32(A, -2);
  HugeIntPower(A, 4);
  Assert(HugeIntToInt32(A) = 16);

  // Power
  HugeIntAssignZero(A);
  HugeIntPower(A, 0);
  Assert(HugeIntToInt32(A) = 1);
  HugeIntAssignZero(A);
  HugeIntPower(A, 1);
  Assert(HugeIntToInt32(A) = 0);
  HugeIntAssignOne(A);
  HugeIntPower(A, 0);
  Assert(HugeIntToInt32(A) = 1);
  HugeIntAssignOne(A);
  HugeIntPower(A, 1);
  Assert(HugeIntToInt32(A) = 1);
  HugeIntAssignMinusOne(A);
  HugeIntPower(A, 0);
  Assert(HugeIntToInt32(A) = 1);
  HugeIntAssignMinusOne(A);
  HugeIntPower(A, 1);
  Assert(HugeIntToInt32(A) = -1);
  HugeIntAssignMinusOne(A);
  HugeIntPower(A, 2);
  Assert(HugeIntToInt32(A) = 1);

  // AssignDouble
  HugeIntAssignDouble(A, 0.0);
  Assert(HugeIntToDouble(A) = 0.0);
  HugeIntAssignDouble(A, 1.0);
  Assert(HugeIntToDouble(A) = 1.0);
  HugeIntAssignDouble(A, -1.0);
  Assert(HugeIntToDouble(A) = -1.0);

  // ToStr/StrTo
  StrToHugeIntB('-1234567890', A);
  Assert(HugeIntToInt32(A) = -1234567890);
  Assert(HugeIntToStrB(A) = '-1234567890');
  Assert(HugeIntToHexB(A) = '-499602D2');
  StrToHugeIntB('123456789012345678901234567890123456789012345678901234567890', A);
  Assert(HugeIntToStrB(A) = '123456789012345678901234567890123456789012345678901234567890');

  // ToHex/HexTo
  HexToHugeIntB('-0123456789ABCDEF', A);
  Assert(HugeIntToHexB(A) = '-0123456789ABCDEF');
  HexToHugeIntB('-F1230', A);
  Assert(HugeIntToHexB(A) = '-000F1230');

  HugeWordFinalise(F);
  HugeIntFinalise(D);
  HugeIntFinalise(C);
  HugeIntFinalise(B);
  HugeIntFinalise(A);
end;

procedure Test;
begin
  Assert(HugeWordElementBits = 32);
  Test_HugeWord;
  Test_HugeInt;
end;
{$ENDIF}

{$IFDEF HUGEINT_PROFILE}
procedure Profile;
const
  Digit_Test_Count = 3;
  Digits_Default: array[0..Digit_Test_Count - 1] of Integer = (1000, 10000, 25000);
  Digits_Multiply: array[0..Digit_Test_Count - 1] of Integer = (10, 50, 100);
  Digits_PowerMod: array[0..Digit_Test_Count - 1] of Integer = (32, 64, 128);
var
  A, B, C, D : HugeWord;
  I, J, Di : Integer;
  T : Word32;
begin
  HugeWordInit(A);
  HugeWordInit(B);
  HugeWordInit(C);
  HugeWordInit(D);


  for J := 0 to Digit_Test_Count - 1 do
    begin
      Di := Digits_Default[J];
      HugeWordRandom(C, Di);
      HugeWordRandom(D, Di - 2);
      HugeWordAssign(B, D);
      T := GetTickCount;
      for I := 1 to 250000 do
        begin
          HugeWordAssign(A, C);
          HugeWordAdd(A, B);
        end;
      T := GetTickCount - T;
      Writeln('Add:':25, Di*32:10, ' ', 1000 / (T / 250000):0:1, '/s');
    end;

  for J := 0 to Digit_Test_Count - 1 do
    begin
      Di := Digits_Default[J];
      repeat
        HugeWordRandom(C, Di);
        HugeWordRandom(D, Di);
      until HugeWordCompare(C, D) > 0;
      HugeWordAssign(B, D);
      T := GetTickCount;
      for I := 1 to 100000 do
        begin
          HugeWordAssign(A, C);
          HugeWordSubtract(A, B);
        end;
      T := GetTickCount - T;
      Writeln('Sub:':25, Di*32:10, ' ', 1000 / (T / 100000):0:1, '/s');
    end;

  for J := 0 to Digit_Test_Count - 1 do
    begin
      Di := Digits_Default[J];
      repeat
        HugeWordRandom(C, Di);
        HugeWordRandom(D, Di);
      until HugeWordCompare(C, D) > 0;
      HugeWordAssign(B, D);
      T := GetTickCount;
      for I := 1 to 100000 do
        begin
          HugeWordAssign(A, C);
          HugeWordSubtract_ALarger(A, B);
        end;
      T := GetTickCount - T;
      Writeln('Sub_ALarger:':25, Di*32:10, ' ', 1000 / (T / 100000):0:1, '/s');
    end;

  for J := 0 to Digit_Test_Count - 1 do
    begin
      Di := Digits_Multiply[J];
      repeat
        HugeWordRandom(A, Di);
        HugeWordRandom(B, Di - 2);
      until HugeWordCompare(A, B) > 0;

      T := GetTickCount;
      for I := 1 to 9000 do
        HugeWordDivide(A, B, C, D);
      T := GetTickCount - T;
      Writeln('Div:':25, Di*32:10, ' ', 1000 / (T / 9000):0:1, '/s');
    end;

  for J := 0 to Digit_Test_Count - 1 do
    begin
      Di := Digits_Default[J];
      HugeWordRandom(C, Di);
      T := GetTickCount;
      for I := 1 to 100000 do
        begin
          HugeWordAssign(A, C);
          HugeWordShl1(A);
        end;
      T := GetTickCount - T;
      Writeln('Shl1:':25, Di*32:10, ' ', 1000 / (T / 100000):0:1, '/s');
  end;

  for J := 0 to Digit_Test_Count - 1 do
    begin
      Di := Digits_Multiply[J];
      HugeWordRandom(A, Di);
      HugeWordRandom(B, Di - 3);

      T := GetTickCount;
      for I := 1 to 10000 do
        HugeWordMod(A, B, C);
      T := GetTickCount - T;
      Writeln('Mod:':25, Di*32:10, ' ', 1000 / (T / 10000):0:1, '/s');
    end;

  for J := 0 to Digit_Test_Count - 1 do
    begin
      Di := Digits_PowerMod[J];
      HugeWordRandom(A, Di);
      HugeWordRandom(B, Di);
      HugeWordRandom(C, Di);
      T := GetTickCount;
      for I := 1 to 1 do
        begin
          HugeWordPowerAndMod(D, A, B, C);
        end;
      T := GetTickCount - T;
      Writeln('PowerAndMod:':25, Di*32:10, ' ', 1000 / (T / 1):0:5, '/s');
    end;

  for J := 0 to Digit_Test_Count - 1 do
    begin
      Di := Digits_Multiply[J];
      HugeWordRandom(A, Di);
      HugeWordRandom(B, Di);

      T := GetTickCount;
      for I := 1 to 1000000 do
        HugeWordMultiply_Long(C, A, B);
      T := GetTickCount - T;
      Writeln('Mul_Long:':25, Di*32:10, ' ', 1000 / (T / 1000000):0:1, '/s');
    end;

  for J := 0 to Digit_Test_Count - 1 do
    begin
      Di := Digits_Multiply[J];
      HugeWordRandom(A, Di);
      HugeWordRandom(B, Di);

      T := GetTickCount;
      for I := 1 to 20000 do
        HugeWordMultiply_ShiftAdd_Unsafe(C, A, B);
      T := GetTickCount - T;
      Writeln('Mul_ShiftAdd:':25, Di*32:10, ' ', 1000 / (T / 20000):0:1, '/s');
    end;

  for J := 0 to Digit_Test_Count - 1 do
    begin
      Di := Digits_Multiply[J];
      HugeWordRandom(A, Di);
      HugeWordRandom(B, Di);

      T := GetTickCount;
      for I := 1 to 50000 do
        HugeWordMultiply_Karatsuba(C, A, B);
      T := GetTickCount - T;
      Writeln('Mul_Karatsuba:':25, Di*32:10, ' ', 1000 / (T / 50000):0:1, '/s');
    end;

  for J := 0 to Digit_Test_Count - 1 do
    begin
      Di := Digits_Default[J];
      HugeWordRandom(A, Di);
      HugeWordAssign(B, A);

      T := GetTickCount;
      for I := 1 to 250000 do
        HugeWordEquals(A, B);
      T := GetTickCount - T;
      Writeln('Equals (worst case):':25, Di*32:10, ' ', 1000 / (T / 250000):0:1, '/s');
    end;

  for J := 0 to Digit_Test_Count - 1 do
    begin
      Di := Digits_Default[J];
      repeat
        HugeWordRandom(A, Di);
        HugeWordRandom(B, Di);
      until HugeWordGetElement(A, 0) <> HugeWordGetElement(B, 0);

      T := GetTickCount;
      for I := 1 to 100000000 do
        HugeWordCompare(A, B);
      T := GetTickCount - T;
      Writeln('Compare (best case):':25, Di*32:10, ' ', 1000 / (T / 100000000):0:1, '/s');
    end;

  for J := 0 to Digit_Test_Count - 1 do
    begin
      Di := Digits_Default[J];
      HugeWordRandom(A, Di);
      HugeWordAssign(B, A);

      T := GetTickCount;
      for I := 1 to 500000 do
        HugeWordCompare(A, B);
      T := GetTickCount - T;
      Writeln('Compare (worst case):':25, Di*32:10, ' ', 1000 / (T / 500000):0:1, '/s');
    end;

  for J := 0 to Digit_Test_Count - 1 do
    begin
      Di := Digits_Default[J];
      HugeWordRandom(A, Di);
      HugeWordSetElement(A, Di - 1, 0);
      T := GetTickCount;
      for I := 1 to 10000000 do
        HugeWordSetBitScanReverse(A);
      T := GetTickCount - T;
      Writeln('SetBitScanReverse:':25, Di*32:10, ' ', 1000 / (T / 10000000):0:1, '/s');
    end;

  for J := 0 to Digit_Test_Count - 1 do
    begin
      Di := Digits_Multiply[J];
      HugeWordRandom(A, Di);
      HugeWordAssign(B, A);

      T := GetTickCount;
      for I := 1 to 3000 do
        begin
          HugeWordAssign(A, B);
          HugeWordISqrt(A);
        end;
      T := GetTickCount - T;
      Writeln('ISqrt:':25, Di*32:10, ' ', 1000 / (T / 3000):0:1, '/s');
    end;

  for J := 0 to Digit_Test_Count - 1 do
    begin
      Di := Digits_Default[J];
      HugeWordRandom(C, Di);
      T := GetTickCount;
      for I := 1 to 100000 do
        begin
          HugeWordAssign(A, C);
          HugeWordShr1(A);
        end;
      T := GetTickCount - T;
      Writeln('Shr1:':25, Di*32:10, ' ', 1000 / (T / 100000):0:1, '/s');
  end;

  HugeWordFinalise(D);
  HugeWordFinalise(C);
  HugeWordFinalise(B);
  HugeWordFinalise(A);
end;
{$ENDIF}



end.

