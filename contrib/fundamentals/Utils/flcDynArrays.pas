{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals 5.00                                        }
{   File name:        flcDynArrays.pas                                         }
{   File version:     5.34                                                     }
{   Description:      Utility functions for dynamic arrays                     }
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
{   2000/03/08  1.01  Initial verison: array functions.                        }
{   2000/04/10  1.02  Added Append, Renamed Delete to Remove and added         }
{                     StringArrays.                                            }
{   2000/06/01  1.03  Added Range and Dup constructors for dynamic arrays.     }
{   2000/06/03  1.04  Added ArrayInsert functions.                             }
{   2000/06/14  1.05  Unit now generated from a template using a source        }
{                     pre-processor.                                           }
{   2000/07/04  1.06  Revision for first Fundamentals release.                 }
{   2000/07/24  1.07  Added TrimArray functions.                               }
{   2000/07/26  1.08  Added Difference functions.                              }
{   2000/09/02  1.09  Added RemoveDuplicates functions.                        }
{                     Added Count functions.                                   }
{                     Fixed bug in Sort.                                       }
{   2000/09/27  1.10  Fixed bug in ArrayInsert.                                }
{   2001/11/11  2.11  Revision.                                                }
{   2002/05/31  3.12  Refactored for Fundamentals 3.                           }
{   2003/09/11  3.13  Added InterfaceArray functions.                          }
{   2004/01/18  3.14  Added WideStringArray functions.                         }
{   2004/07/24  3.15  Optimizations of Sort functions.                         }
{   2004/08/22  3.16  Compilable with Delphi 8.                                }
{   2005/06/10  4.17  Compilable with FreePascal 2 Win32 i386.                 }
{   2005/08/19  4.18  Compilable with FreePascal 2 Linux i386.                 }
{   2005/09/21  4.19  Revised for Fundamentals 4.                              }
{   2006/03/04  4.20  Compilable with Delphi 2006 Win32/.NET.                  }
{   2007/06/08  4.21  Compilable with FreePascal 2.04 Win32 i386               }
{   2009/10/09  4.22  Compilable with Delphi 2009 Win32/.NET.                  }
{   2010/06/27  4.23  Compilable with FreePascal 2.4.0 OSX x86-64.             }
{   2011/04/03  4.24  Support for Delphi XE string and integer types.          }
{   2011/04/04  4.25  Split dynamic array functions from Utils unit.           }
{   2012/08/26  4.26  UnicodeString functions.                                 }
{   2015/03/13  4.27  RawByteString functions.                                 }
{   2016/01/09  5.28  Revised for Fundamentals 5.                              }
{   2016/04/16  5.29  Changes for FreePascal 3.0.0.                            }
{   2018/07/17  5.30  Int32/Word32 functions.                                  }
{   2018/08/12  5.31  String type changes.                                     }
{   2019/03/22  5.32  FreePascal 3.04 Win64 changes.                           }
{   2019/04/02  5.33  Integer/Cardinal functions.                              }
{   2020/03/31  5.34  Word64 functions.                                        }
{                                                                              }
{ Supported compilers:                                                         }
{                                                                              }
{   Delphi 2010-10.4 Win32/Win64        5.34  2020/06/02                       }
{   Delphi 10.2-10.4 Linux64            5.34  2020/06/02                       }
{   FreePascal 3.0.4 Win64              5.34  2020/06/02                       }
{                                                                              }
{******************************************************************************}

{$INCLUDE ..\flcInclude.inc}

{$IFDEF FREEPASCAL}
  {$WARNINGS OFF}
  {$HINTS OFF}
{$ENDIF}

{$IFDEF DEBUG}
{$IFDEF TEST}
  {$DEFINE DYNARRAYS_TEST}
{$ENDIF}
{$ENDIF}

unit flcDynArrays;

interface

uses
  { System }
  SysUtils,

  { Fundamentals }
  flcStdTypes,
  flcUtils;



{                                                                              }
{ Dynamic arrays                                                               }
{                                                                              }
function  DynArrayAppend(var V: ByteArray; const R: Byte): Integer; overload; {$IFDEF UseInline}inline;{$ENDIF}
function  DynArrayAppend(var V: Word16Array; const R: Word16): Integer; overload; {$IFDEF UseInline}inline;{$ENDIF}
function  DynArrayAppend(var V: Word32Array; const R: Word32): Integer; overload; {$IFDEF UseInline}inline;{$ENDIF}
function  DynArrayAppend(var V: Word64Array; const R: Word64): Integer; overload; {$IFDEF UseInline}inline;{$ENDIF}
function  DynArrayAppend(var V: LongWordArray; const R: LongWord): Integer; overload; {$IFDEF UseInline}inline;{$ENDIF}
function  DynArrayAppend(var V: CardinalArray; const R: Cardinal): Integer; overload; {$IFDEF UseInline}inline;{$ENDIF}
function  DynArrayAppend(var V: NativeUIntArray; const R: NativeUInt): Integer; overload; {$IFDEF UseInline}inline;{$ENDIF}
function  DynArrayAppend(var V: ShortIntArray; const R: ShortInt): Integer; overload; {$IFDEF UseInline}inline;{$ENDIF}
function  DynArrayAppend(var V: SmallIntArray; const R: SmallInt): Integer; overload; {$IFDEF UseInline}inline;{$ENDIF}
function  DynArrayAppend(var V: LongIntArray; const R: LongInt): Integer; overload; {$IFDEF UseInline}inline;{$ENDIF}
function  DynArrayAppend(var V: IntegerArray; const R: Integer): Integer; overload; {$IFDEF UseInline}inline;{$ENDIF}
function  DynArrayAppend(var V: Int32Array; const R: Int32): Integer; overload; {$IFDEF UseInline}inline;{$ENDIF}
function  DynArrayAppend(var V: Int64Array; const R: Int64): Integer; overload; {$IFDEF UseInline}inline;{$ENDIF}
function  DynArrayAppend(var V: NativeIntArray; const R: NativeInt): Integer; overload; {$IFDEF UseInline}inline;{$ENDIF}
function  DynArrayAppend(var V: SingleArray; const R: Single): Integer; overload; {$IFDEF UseInline}inline;{$ENDIF}
function  DynArrayAppend(var V: DoubleArray; const R: Double): Integer; overload; {$IFDEF UseInline}inline;{$ENDIF}
function  DynArrayAppend(var V: ExtendedArray; const R: Extended): Integer; overload; {$IFDEF UseInline}inline;{$ENDIF}
function  DynArrayAppend(var V: CurrencyArray; const R: Currency): Integer; overload; {$IFDEF UseInline}inline;{$ENDIF}
function  DynArrayAppend(var V: BooleanArray; const R: Boolean): Integer; overload; {$IFDEF UseInline}inline;{$ENDIF}
{$IFDEF SupportAnsiString}
function  DynArrayAppendA(var V: AnsiStringArray; const R: AnsiString): Integer; overload; {$IFDEF UseInline}inline;{$ENDIF}
{$ENDIF}
function  DynArrayAppendB(var V: RawByteStringArray; const R: RawByteString): Integer; overload; {$IFDEF UseInline}inline;{$ENDIF}
function  DynArrayAppendU(var V: UnicodeStringArray; const R: UnicodeString): Integer; overload; {$IFDEF UseInline}inline;{$ENDIF}
function  DynArrayAppend(var V: StringArray; const R: String): Integer; overload; {$IFDEF UseInline}inline;{$ENDIF}
function  DynArrayAppend(var V: PointerArray; const R: Pointer): Integer; overload; {$IFDEF UseInline}inline;{$ENDIF}
function  DynArrayAppend(var V: ObjectArray; const R: TObject): Integer; overload; {$IFDEF UseInline}inline;{$ENDIF}
function  DynArrayAppend(var V: InterfaceArray; const R: IInterface): Integer; overload; {$IFDEF UseInline}inline;{$ENDIF}
function  DynArrayAppend(var V: ByteSetArray; const R: ByteSet): Integer; overload; {$IFDEF UseInline}inline;{$ENDIF}
function  DynArrayAppend(var V: ByteCharSetArray; const R: ByteCharSet): Integer; overload; {$IFDEF UseInline}inline;{$ENDIF}

function  DynArrayAppendByteArray(var V: ByteArray; const R: array of Byte): Integer; overload;
function  DynArrayAppendWord16Array(var V: Word16Array; const R: array of Word16): Integer; overload;
function  DynArrayAppendWord32Array(var V: Word32Array; const R: array of Word32): Integer; overload;
function  DynArrayAppendWord64Array(var V: Word64Array; const R: array of Word64): Integer; overload;
function  DynArrayAppendCardinalArray(var V: CardinalArray; const R: array of Cardinal): Integer; overload;
function  DynArrayAppendNativeUIntArray(var V: NativeUIntArray; const R: array of NativeUInt): Integer; overload;
function  DynArrayAppendShortIntArray(var V: ShortIntArray; const R: array of ShortInt): Integer; overload;
function  DynArrayAppendSmallIntArray(var V: SmallIntArray; const R: array of SmallInt): Integer; overload;
function  DynArrayAppendLongIntArray(var V: LongIntArray; const R: array of LongInt): Integer; overload;
function  DynArrayAppendIntegerArray(var V: IntegerArray; const R: array of Integer): Integer; overload;
function  DynArrayAppendInt32Array(var V: Int32Array; const R: array of Int32): Integer; overload;
function  DynArrayAppendInt64Array(var V: Int64Array; const R: array of Int64): Integer; overload;
function  DynArrayAppendNativeIntArray(var V: NativeIntArray; const R: array of NativeInt): Integer; overload;
function  DynArrayAppendSingleArray(var V: SingleArray; const R: array of Single): Integer; overload;
function  DynArrayAppendDoubleArray(var V: DoubleArray; const R: array of Double): Integer; overload;
function  DynArrayAppendExtendedArray(var V: ExtendedArray; const R: array of Extended): Integer; overload;
{$IFDEF SupportAnsiString}
function  DynArrayAppendAnsiStringArray(var V: AnsiStringArray; const R: array of AnsiString): Integer; overload;
{$ENDIF}
function  DynArrayAppendRawByteStringArray(var V: RawByteStringArray; const R: array of RawByteString): Integer; overload;
function  DynArrayAppendUnicodeStringArray(var V: UnicodeStringArray; const R: array of UnicodeString): Integer; overload;
function  DynArrayAppendStringArray(var V: StringArray; const R: array of String): Integer; overload;
function  DynArrayAppendCurrencyArray(var V: CurrencyArray; const R: array of Currency): Integer; overload;
function  DynArrayAppendPointerArray(var V: PointerArray; const R: array of Pointer): Integer; overload;
function  DynArrayAppendByteCharSetArray(var V: ByteCharSetArray; const R: array of ByteCharSet): Integer; overload;
function  DynArrayAppendByteSetArray(var V: ByteSetArray; const R: array of ByteSet): Integer; overload;
function  DynArrayAppendObjectArray(var V: ObjectArray; const R: ObjectArray): Integer; overload;

function  DynArrayRemove(var V: ByteArray; const Idx: Integer; const Count: Integer = 1): Integer; overload;
function  DynArrayRemove(var V: Word16Array; const Idx: Integer; const Count: Integer = 1): Integer; overload;
function  DynArrayRemove(var V: Word32Array; const Idx: Integer; const Count: Integer = 1): Integer; overload;
function  DynArrayRemove(var V: Word64Array; const Idx: Integer; const Count: Integer = 1): Integer; overload;
function  DynArrayRemove(var V: LongWordArray; const Idx: Integer; const Count: Integer = 1): Integer; overload;
function  DynArrayRemove(var V: CardinalArray; const Idx: Integer; const Count: Integer = 1): Integer; overload;
function  DynArrayRemove(var V: NativeUIntArray; const Idx: Integer; const Count: Integer = 1): Integer; overload;
function  DynArrayRemove(var V: ShortIntArray; const Idx: Integer; const Count: Integer = 1): Integer; overload;
function  DynArrayRemove(var V: SmallIntArray; const Idx: Integer; const Count: Integer = 1): Integer; overload;
function  DynArrayRemove(var V: LongIntArray; const Idx: Integer; const Count: Integer = 1): Integer; overload;
function  DynArrayRemove(var V: IntegerArray; const Idx: Integer; const Count: Integer = 1): Integer; overload;
function  DynArrayRemove(var V: Int32Array; const Idx: Integer; const Count: Integer = 1): Integer; overload;
function  DynArrayRemove(var V: Int64Array; const Idx: Integer; const Count: Integer = 1): Integer; overload;
function  DynArrayRemove(var V: NativeIntArray; const Idx: Integer; const Count: Integer = 1): Integer; overload;
function  DynArrayRemove(var V: SingleArray; const Idx: Integer; const Count: Integer = 1): Integer; overload;
function  DynArrayRemove(var V: DoubleArray; const Idx: Integer; const Count: Integer = 1): Integer; overload;
function  DynArrayRemove(var V: ExtendedArray; const Idx: Integer; const Count: Integer = 1): Integer; overload;
{$IFDEF SupportAnsiString}
function  DynArrayRemoveA(var V: AnsiStringArray; const Idx: Integer; const Count: Integer = 1): Integer; overload;
{$ENDIF}
function  DynArrayRemoveB(var V: RawByteStringArray; const Idx: Integer; const Count: Integer = 1): Integer; overload;
function  DynArrayRemoveU(var V: UnicodeStringArray; const Idx: Integer; const Count: Integer = 1): Integer; overload;
function  DynArrayRemove(var V: StringArray; const Idx: Integer; const Count: Integer = 1): Integer; overload;
function  DynArrayRemove(var V: PointerArray; const Idx: Integer; const Count: Integer = 1): Integer; overload;
function  DynArrayRemove(var V: CurrencyArray; const Idx: Integer; const Count: Integer = 1): Integer; overload;
function  DynArrayRemove(var V: ObjectArray; const Idx: Integer; const Count: Integer = 1;
          const FreeObjects: Boolean = False): Integer; overload;
function  DynArrayRemove(var V: InterfaceArray; const Idx: Integer; const Count: Integer = 1): Integer; overload;

procedure DynArrayRemoveDuplicates(var V: ByteArray; const IsSorted: Boolean); overload;
procedure DynArrayRemoveDuplicates(var V: Word16Array; const IsSorted: Boolean); overload;
procedure DynArrayRemoveDuplicates(var V: LongWordArray; const IsSorted: Boolean); overload;
procedure DynArrayRemoveDuplicates(var V: ShortIntArray; const IsSorted: Boolean); overload;
procedure DynArrayRemoveDuplicates(var V: SmallIntArray; const IsSorted: Boolean); overload;
procedure DynArrayRemoveDuplicates(var V: LongIntArray; const IsSorted: Boolean); overload;
procedure DynArrayRemoveDuplicates(var V: Int64Array; const IsSorted: Boolean); overload;
procedure DynArrayRemoveDuplicates(var V: SingleArray; const IsSorted: Boolean); overload;
procedure DynArrayRemoveDuplicates(var V: DoubleArray; const IsSorted: Boolean); overload;
procedure DynArrayRemoveDuplicates(var V: ExtendedArray; const IsSorted: Boolean); overload;
{$IFDEF SupportAnsiString}
procedure DynArrayRemoveDuplicatesA(var V: AnsiStringArray; const IsSorted: Boolean); overload;
{$ENDIF}
procedure DynArrayRemoveDuplicatesU(var V: UnicodeStringArray; const IsSorted: Boolean); overload;
procedure DynArrayRemoveDuplicates(var V: StringArray; const IsSorted: Boolean); overload;
procedure DynArrayRemoveDuplicates(var V: PointerArray; const IsSorted: Boolean); overload;

procedure DynArrayTrimLeft(var S: ByteArray; const TrimList: array of Byte); overload;
procedure DynArrayTrimLeft(var S: Word16Array; const TrimList: array of Word16); overload;
procedure DynArrayTrimLeft(var S: LongWordArray; const TrimList: array of LongWord); overload;
procedure DynArrayTrimLeft(var S: ShortIntArray; const TrimList: array of ShortInt); overload;
procedure DynArrayTrimLeft(var S: SmallIntArray; const TrimList: array of SmallInt); overload;
procedure DynArrayTrimLeft(var S: LongIntArray; const TrimList: array of LongInt); overload;
procedure DynArrayTrimLeft(var S: Int64Array; const TrimList: array of Int64); overload;
procedure DynArrayTrimLeft(var S: SingleArray; const TrimList: array of Single); overload;
procedure DynArrayTrimLeft(var S: DoubleArray; const TrimList: array of Double); overload;
procedure DynArrayTrimLeft(var S: ExtendedArray; const TrimList: array of Extended); overload;
{$IFDEF SupportAnsiString}
procedure DynArrayTrimLeftA(var S: AnsiStringArray; const TrimList: array of AnsiString); overload;
{$ENDIF}
procedure DynArrayTrimLeftU(var S: UnicodeStringArray; const TrimList: array of UnicodeString); overload;
procedure DynArrayTrimLeft(var S: PointerArray; const TrimList: array of Pointer); overload;

procedure DynArrayTrimRight(var S: ByteArray; const TrimList: array of Byte); overload;
procedure DynArrayTrimRight(var S: Word16Array; const TrimList: array of Word16); overload;
procedure DynArrayTrimRight(var S: LongWordArray; const TrimList: array of LongWord); overload;
procedure DynArrayTrimRight(var S: ShortIntArray; const TrimList: array of ShortInt); overload;
procedure DynArrayTrimRight(var S: SmallIntArray; const TrimList: array of SmallInt); overload;
procedure DynArrayTrimRight(var S: LongIntArray; const TrimList: array of LongInt); overload;
procedure DynArrayTrimRight(var S: Int64Array; const TrimList: array of Int64); overload;
procedure DynArrayTrimRight(var S: SingleArray; const TrimList: array of Single); overload;
procedure DynArrayTrimRight(var S: DoubleArray; const TrimList: array of Double); overload;
procedure DynArrayTrimRight(var S: ExtendedArray; const TrimList: array of Extended); overload;
{$IFDEF SupportAnsiString}
procedure DynArrayTrimRightA(var S: AnsiStringArray; const TrimList: array of AnsiString); overload;
{$ENDIF}
procedure DynArrayTrimRightU(var S: UnicodeStringArray; const TrimList: array of UnicodeString); overload;
procedure DynArrayTrimRight(var S: StringArray; const TrimList: array of String); overload;
procedure DynArrayTrimRight(var S: PointerArray; const TrimList: array of Pointer); overload;

function  DynArrayInsert(var V: ByteArray; const Idx: Integer; const Count: Integer): Integer; overload;
function  DynArrayInsert(var V: Word16Array; const Idx: Integer; const Count: Integer): Integer; overload;
function  DynArrayInsert(var V: Word32Array; const Idx: Integer; const Count: Integer): Integer; overload;
function  DynArrayInsert(var V: Word64Array; const Idx: Integer; const Count: Integer): Integer; overload;
function  DynArrayInsert(var V: LongWordArray; const Idx: Integer; const Count: Integer): Integer; overload;
function  DynArrayInsert(var V: NativeUIntArray; const Idx: Integer; const Count: Integer): Integer; overload;
function  DynArrayInsert(var V: ShortIntArray; const Idx: Integer; const Count: Integer): Integer; overload;
function  DynArrayInsert(var V: SmallIntArray; const Idx: Integer; const Count: Integer): Integer; overload;
function  DynArrayInsert(var V: LongIntArray; const Idx: Integer; const Count: Integer): Integer; overload;
function  DynArrayInsert(var V: Int32Array; const Idx: Integer; const Count: Integer): Integer; overload;
function  DynArrayInsert(var V: Int64Array; const Idx: Integer; const Count: Integer): Integer; overload;
function  DynArrayInsert(var V: NativeIntArray; const Idx: Integer; const Count: Integer): Integer; overload;
function  DynArrayInsert(var V: SingleArray; const Idx: Integer; const Count: Integer): Integer; overload;
function  DynArrayInsert(var V: DoubleArray; const Idx: Integer; const Count: Integer): Integer; overload;
function  DynArrayInsert(var V: ExtendedArray; const Idx: Integer; const Count: Integer): Integer; overload;
function  DynArrayInsert(var V: CurrencyArray; const Idx: Integer; const Count: Integer): Integer; overload;
{$IFDEF SupportAnsiString}
function  DynArrayInsertA(var V: AnsiStringArray; const Idx: Integer; const Count: Integer): Integer; overload;
{$ENDIF}
function  DynArrayInsertB(var V: RawByteStringArray; const Idx: Integer; const Count: Integer): Integer; overload;
function  DynArrayInsertU(var V: UnicodeStringArray; const Idx: Integer; const Count: Integer): Integer; overload;
function  DynArrayInsert(var V: StringArray; const Idx: Integer; const Count: Integer): Integer; overload;
function  DynArrayInsert(var V: PointerArray; const Idx: Integer; const Count: Integer): Integer; overload;
function  DynArrayInsert(var V: ObjectArray; const Idx: Integer; const Count: Integer): Integer; overload;
function  DynArrayInsert(var V: InterfaceArray; const Idx: Integer; const Count: Integer): Integer; overload;

function  DynArrayPosNext(const Find: Byte; const V: ByteArray; const PrevPos: Integer = -1;
          const IsSortedAscending: Boolean = False): Integer; overload;
function  DynArrayPosNext(const Find: Word16; const V: Word16Array; const PrevPos: Integer = -1;
          const IsSortedAscending: Boolean = False): Integer; overload;
function  DynArrayPosNext(const Find: Word32; const V: Word32Array; const PrevPos: Integer = -1;
          const IsSortedAscending: Boolean = False): Integer; overload;
function  DynArrayPosNext(const Find: Word64; const V: Word64Array; const PrevPos: Integer = -1;
          const IsSortedAscending: Boolean = False): Integer; overload;
function  DynArrayPosNext(const Find: LongWord; const V: LongWordArray; const PrevPos: Integer = -1;
          const IsSortedAscending: Boolean = False): Integer; overload;
function  DynArrayPosNext(const Find: Cardinal; const V: CardinalArray; const PrevPos: Integer = -1;
          const IsSortedAscending: Boolean = False): Integer; overload;
function  DynArrayPosNext(const Find: NativeUInt; const V: NativeUIntArray; const PrevPos: Integer = -1;
          const IsSortedAscending: Boolean = False): Integer; overload;
function  DynArrayPosNext(const Find: ShortInt; const V: ShortIntArray; const PrevPos: Integer = -1;
          const IsSortedAscending: Boolean = False): Integer; overload;
function  DynArrayPosNext(const Find: SmallInt; const V: SmallIntArray; const PrevPos: Integer = -1;
          const IsSortedAscending: Boolean = False): Integer; overload;
function  DynArrayPosNext(const Find: LongInt; const V: LongIntArray; const PrevPos: Integer = -1;
          const IsSortedAscending: Boolean = False): Integer; overload;
function  DynArrayPosNext(const Find: Integer; const V: IntegerArray; const PrevPos: Integer = -1;
          const IsSortedAscending: Boolean = False): Integer; overload;
function  DynArrayPosNext(const Find: Int32; const V: Int32Array; const PrevPos: Integer = -1;
          const IsSortedAscending: Boolean = False): Integer; overload;
function  DynArrayPosNext(const Find: Int64; const V: Int64Array; const PrevPos: Integer = -1;
          const IsSortedAscending: Boolean = False): Integer; overload;
function  DynArrayPosNext(const Find: NativeInt; const V: NativeIntArray; const PrevPos: Integer = -1;
          const IsSortedAscending: Boolean = False): Integer; overload;
function  DynArrayPosNext(const Find: Single; const V: SingleArray; const PrevPos: Integer = -1;
          const IsSortedAscending: Boolean = False): Integer; overload;
function  DynArrayPosNext(const Find: Double; const V: DoubleArray; const PrevPos: Integer = -1;
          const IsSortedAscending: Boolean = False): Integer; overload;
function  DynArrayPosNext(const Find: Extended; const V: ExtendedArray; const PrevPos: Integer = -1;
          const IsSortedAscending: Boolean = False): Integer; overload;
function  DynArrayPosNext(const Find: Boolean; const V: BooleanArray; const PrevPos: Integer = -1;
          const IsSortedAscending: Boolean = False): Integer; overload;
{$IFDEF SupportAnsiString}
function  DynArrayPosNextA(const Find: AnsiString; const V: AnsiStringArray; const PrevPos: Integer = -1;
          const IsSortedAscending: Boolean = False): Integer; overload;
{$ENDIF}
function  DynArrayPosNextB(const Find: RawByteString; const V: RawByteStringArray; const PrevPos: Integer = -1;
          const IsSortedAscending: Boolean = False): Integer; overload;
function  DynArrayPosNextU(const Find: UnicodeString; const V: UnicodeStringArray; const PrevPos: Integer = -1;
          const IsSortedAscending: Boolean = False): Integer; overload;
function  DynArrayPosNext(const Find: String; const V: StringArray; const PrevPos: Integer = -1;
          const IsSortedAscending: Boolean = False): Integer; overload;
function  DynArrayPosNext(const Find: Pointer; const V: PointerArray;
          const PrevPos: Integer = -1): Integer; overload;
function  DynArrayPosNext(const Find: TObject; const V: ObjectArray;
          const PrevPos: Integer = -1): Integer; overload;
function  DynArrayPosNext(const ClassType: TClass; const V: ObjectArray;
          const PrevPos: Integer = -1): Integer; overload;
function  DynArrayPosNext(const ClassName: String; const V: ObjectArray;
          const PrevPos: Integer = -1): Integer; overload;

function  DynArrayCount(const Find: Byte; const V: ByteArray;
          const IsSortedAscending: Boolean = False): Integer; overload;
function  DynArrayCount(const Find: Word16; const V: Word16Array;
          const IsSortedAscending: Boolean = False): Integer; overload;
function  DynArrayCount(const Find: LongWord; const V: LongWordArray;
          const IsSortedAscending: Boolean = False): Integer; overload;
function  DynArrayCount(const Find: ShortInt; const V: ShortIntArray;
          const IsSortedAscending: Boolean = False): Integer; overload;
function  DynArrayCount(const Find: SmallInt; const V: SmallIntArray;
          const IsSortedAscending: Boolean = False): Integer; overload;
function  DynArrayCount(const Find: LongInt; const V: LongIntArray;
          const IsSortedAscending: Boolean = False): Integer; overload;
function  DynArrayCount(const Find: Int64; const V: Int64Array;
          const IsSortedAscending: Boolean = False): Integer; overload;
function  DynArrayCount(const Find: Single; const V: SingleArray;
          const IsSortedAscending: Boolean = False): Integer; overload;
function  DynArrayCount(const Find: Double; const V: DoubleArray;
          const IsSortedAscending: Boolean = False): Integer; overload;
function  DynArrayCount(const Find: Extended; const V: ExtendedArray;
          const IsSortedAscending: Boolean = False): Integer; overload;
{$IFDEF SupportAnsiString}
function  DynArrayCountA(const Find: AnsiString; const V: AnsiStringArray;
          const IsSortedAscending: Boolean = False): Integer; overload;
{$ENDIF}
function  DynArrayCountB(const Find: RawByteString; const V: RawByteStringArray;
          const IsSortedAscending: Boolean = False): Integer; overload;
function  DynArrayCountU(const Find: UnicodeString; const V: UnicodeStringArray;
          const IsSortedAscending: Boolean = False): Integer; overload;
function  DynArrayCount(const Find: String; const V: StringArray;
          const IsSortedAscending: Boolean = False): Integer; overload;
function  DynArrayCount(const Find: Boolean; const V: BooleanArray;
          const IsSortedAscending: Boolean = False): Integer; overload;

procedure DynArrayRemoveAll(const Find: Byte; var V: ByteArray;
          const IsSortedAscending: Boolean = False); overload; 
procedure DynArrayRemoveAll(const Find: Word16; var V: Word16Array;
          const IsSortedAscending: Boolean = False); overload; 
procedure DynArrayRemoveAll(const Find: LongWord; var V: LongWordArray;
          const IsSortedAscending: Boolean = False); overload; 
procedure DynArrayRemoveAll(const Find: ShortInt; var V: ShortIntArray;
          const IsSortedAscending: Boolean = False); overload; 
procedure DynArrayRemoveAll(const Find: SmallInt; var V: SmallIntArray;
          const IsSortedAscending: Boolean = False); overload; 
procedure DynArrayRemoveAll(const Find: LongInt; var V: LongIntArray;
          const IsSortedAscending: Boolean = False); overload; 
procedure DynArrayRemoveAll(const Find: Int64; var V: Int64Array;
          const IsSortedAscending: Boolean = False); overload; 
procedure DynArrayRemoveAll(const Find: Single; var V: SingleArray;
          const IsSortedAscending: Boolean = False); overload; 
procedure DynArrayRemoveAll(const Find: Double; var V: DoubleArray;
          const IsSortedAscending: Boolean = False); overload; 
procedure DynArrayRemoveAll(const Find: Extended; var V: ExtendedArray;
          const IsSortedAscending: Boolean = False); overload; 
{$IFDEF SupportAnsiString}
procedure DynArrayRemoveAllA(const Find: AnsiString; var V: AnsiStringArray;
          const IsSortedAscending: Boolean = False); overload; 
{$ENDIF}
procedure DynArrayRemoveAllU(const Find: UnicodeString; var V: UnicodeStringArray;
          const IsSortedAscending: Boolean = False); overload; 
procedure DynArrayRemoveAll(const Find: String; var V: StringArray;
          const IsSortedAscending: Boolean = False); overload; 

function  DynArrayIntersection(const V1, V2: ByteArray;
          const IsSortedAscending: Boolean = False): ByteArray; overload;
function  DynArrayIntersection(const V1, V2: Word16Array;
          const IsSortedAscending: Boolean = False): Word16Array; overload;
function  DynArrayIntersection(const V1, V2: LongWordArray;
          const IsSortedAscending: Boolean = False): LongWordArray; overload;
function  DynArrayIntersection(const V1, V2: ShortIntArray;
          const IsSortedAscending: Boolean = False): ShortIntArray; overload;
function  DynArrayIntersection(const V1, V2: SmallIntArray;
          const IsSortedAscending: Boolean = False): SmallIntArray; overload;
function  DynArrayIntersection(const V1, V2: LongIntArray;
          const IsSortedAscending: Boolean = False): LongIntArray; overload;
function  DynArrayIntersection(const V1, V2: Int64Array;
          const IsSortedAscending: Boolean = False): Int64Array; overload;
function  DynArrayIntersection(const V1, V2: SingleArray;
          const IsSortedAscending: Boolean = False): SingleArray; overload;
function  DynArrayIntersection(const V1, V2: DoubleArray;
          const IsSortedAscending: Boolean = False): DoubleArray; overload;
function  DynArrayIntersection(const V1, V2: ExtendedArray;
          const IsSortedAscending: Boolean = False): ExtendedArray; overload;
{$IFDEF SupportAnsiString}
function  DynArrayIntersectionA(const V1, V2: AnsiStringArray;
          const IsSortedAscending: Boolean = False): AnsiStringArray; overload;
{$ENDIF}
function  DynArrayIntersectionU(const V1, V2: UnicodeStringArray;
          const IsSortedAscending: Boolean = False): UnicodeStringArray; overload;
function  DynArrayIntersection(const V1, V2: StringArray;
          const IsSortedAscending: Boolean = False): StringArray; overload;

function  DynArrayDifference(const V1, V2: ByteArray;
          const IsSortedAscending: Boolean = False): ByteArray; overload;
function  DynArrayDifference(const V1, V2: Word16Array;
          const IsSortedAscending: Boolean = False): Word16Array; overload;
function  DynArrayDifference(const V1, V2: LongWordArray;
          const IsSortedAscending: Boolean = False): LongWordArray; overload;
function  DynArrayDifference(const V1, V2: ShortIntArray;
          const IsSortedAscending: Boolean = False): ShortIntArray; overload;
function  DynArrayDifference(const V1, V2: SmallIntArray;
          const IsSortedAscending: Boolean = False): SmallIntArray; overload;
function  DynArrayDifference(const V1, V2: LongIntArray;
          const IsSortedAscending: Boolean = False): LongIntArray; overload;
function  DynArrayDifference(const V1, V2: Int64Array;
          const IsSortedAscending: Boolean = False): Int64Array; overload;
function  DynArrayDifference(const V1, V2: SingleArray;
          const IsSortedAscending: Boolean = False): SingleArray; overload;
function  DynArrayDifference(const V1, V2: DoubleArray;
          const IsSortedAscending: Boolean = False): DoubleArray; overload;
function  DynArrayDifference(const V1, V2: ExtendedArray;
          const IsSortedAscending: Boolean = False): ExtendedArray; overload;
{$IFDEF SupportAnsiString}
function  DynArrayDifferenceA(const V1, V2: AnsiStringArray;
          const IsSortedAscending: Boolean = False): AnsiStringArray; overload;
{$ENDIF}
function  DynArrayDifferenceU(const V1, V2: UnicodeStringArray;
          const IsSortedAscending: Boolean = False): UnicodeStringArray; overload;
function  DynArrayDifference(const V1, V2: StringArray;
          const IsSortedAscending: Boolean = False): StringArray; overload;

procedure DynArrayReverse(var V: ByteArray); overload;
procedure DynArrayReverse(var V: Word16Array); overload;
procedure DynArrayReverse(var V: LongWordArray); overload;
procedure DynArrayReverse(var V: ShortIntArray); overload;
procedure DynArrayReverse(var V: SmallIntArray); overload;
procedure DynArrayReverse(var V: LongIntArray); overload;
procedure DynArrayReverse(var V: Int64Array); overload;
procedure DynArrayReverse(var V: SingleArray); overload;
procedure DynArrayReverse(var V: DoubleArray); overload;
procedure DynArrayReverse(var V: ExtendedArray); overload;
{$IFDEF SupportAnsiString}
procedure DynArrayReverseA(var V: AnsiStringArray); overload;
{$ENDIF}
procedure DynArrayReverseU(var V: UnicodeStringArray); overload;
procedure DynArrayReverse(var V: StringArray); overload;
procedure DynArrayReverse(var V: PointerArray); overload;
procedure DynArrayReverse(var V: ObjectArray); overload;

function  AsBooleanArray(const V: array of Boolean): BooleanArray; overload;
function  AsByteArray(const V: array of Byte): ByteArray; overload;
function  AsWord16Array(const V: array of Word16): Word16Array; overload;
function  AsWord32Array(const V: array of Word32): Word32Array; overload;
function  AsWord64Array(const V: array of Word64): Word64Array; overload;
function  AsLongWordArray(const V: array of LongWord): LongWordArray; overload;
function  AsCardinalArray(const V: array of Cardinal): CardinalArray; overload;
function  AsNativeUIntArray(const V: array of NativeUInt): NativeUIntArray; overload;
function  AsShortIntArray(const V: array of ShortInt): ShortIntArray; overload;
function  AsSmallIntArray(const V: array of SmallInt): SmallIntArray; overload;
function  AsLongIntArray(const V: array of LongInt): LongIntArray; overload;
function  AsIntegerArray(const V: array of Integer): IntegerArray; overload;
function  AsInt32Array(const V: array of Int32): Int32Array; overload;
function  AsInt64Array(const V: array of Int64): Int64Array; overload;
function  AsNativeIntArray(const V: array of NativeInt): NativeIntArray; overload;
function  AsSingleArray(const V: array of Single): SingleArray; overload;
function  AsDoubleArray(const V: array of Double): DoubleArray; overload;
function  AsExtendedArray(const V: array of Extended): ExtendedArray; overload;
function  AsCurrencyArray(const V: array of Currency): CurrencyArray; overload;
{$IFDEF SupportAnsiString}
function  AsAnsiStringArray(const V: array of AnsiString): AnsiStringArray; overload;
{$ENDIF}
function  AsRawByteStringArray(const V: array of RawByteString): RawByteStringArray; overload;
function  AsUnicodeStringArray(const V: array of UnicodeString): UnicodeStringArray; overload;
function  AsStringArray(const V: array of String): StringArray; overload;
function  AsPointerArray(const V: array of Pointer): PointerArray; overload;
function  AsByteCharSetArray(const V: array of ByteCharSet): ByteCharSetArray; overload;
function  AsObjectArray(const V: array of TObject): ObjectArray; overload;
function  AsInterfaceArray(const V: array of IInterface): InterfaceArray; overload;

function  DynArrayRangeByte(const First: Byte; const Count: Integer;
          const Increment: Byte = 1): ByteArray;
function  DynArrayRangeWord16(const First: Word16; const Count: Integer;
          const Increment: Word16 = 1): Word16Array;
function  DynArrayRangeLongWord(const First: LongWord; const Count: Integer;
          const Increment: LongWord = 1): LongWordArray;
function  DynArrayRangeCardinal(const First: Cardinal; const Count: Integer;
          const Increment: Cardinal = 1): CardinalArray;
function  DynArrayRangeShortInt(const First: ShortInt; const Count: Integer;
          const Increment: ShortInt = 1): ShortIntArray;
function  DynArrayRangeSmallInt(const First: SmallInt; const Count: Integer;
          const Increment: SmallInt = 1): SmallIntArray;
function  DynArrayRangeLongInt(const First: LongInt; const Count: Integer;
          const Increment: LongInt = 1): LongIntArray;
function  DynArrayRangeInteger(const First: Integer; const Count: Integer;
          const Increment: Integer = 1): IntegerArray;
function  DynArrayRangeInt64(const First: Int64; const Count: Integer;
          const Increment: Int64 = 1): Int64Array;
function  DynArrayRangeSingle(const First: Single; const Count: Integer;
          const Increment: Single = 1): SingleArray;
function  DynArrayRangeDouble(const First: Double; const Count: Integer;
          const Increment: Double = 1): DoubleArray;
function  DynArrayRangeExtended(const First: Extended; const Count: Integer;
          const Increment: Extended = 1): ExtendedArray;

function  DynArrayDupByte(const V: Byte; const Count: Integer): ByteArray;
function  DynArrayDupWord16(const V: Word16; const Count: Integer): Word16Array;
function  DynArrayDupLongWord(const V: LongWord; const Count: Integer): LongWordArray;
function  DynArrayDupCardinal(const V: Cardinal; const Count: Integer): CardinalArray;
function  DynArrayDupNativeUInt(const V: NativeUInt; const Count: Integer): NativeUIntArray;
function  DynArrayDupShortInt(const V: ShortInt; const Count: Integer): ShortIntArray;
function  DynArrayDupSmallInt(const V: SmallInt; const Count: Integer): SmallIntArray;
function  DynArrayDupLongInt(const V: LongInt; const Count: Integer): LongIntArray;
function  DynArrayDupInteger(const V: Integer; const Count: Integer): IntegerArray;
function  DynArrayDupInt64(const V: Int64; const Count: Integer): Int64Array;
function  DynArrayDupNativeInt(const V: NativeInt; const Count: Integer): NativeIntArray;
function  DynArrayDupSingle(const V: Single; const Count: Integer): SingleArray;
function  DynArrayDupDouble(const V: Double; const Count: Integer): DoubleArray;
function  DynArrayDupExtended(const V: Extended; const Count: Integer): ExtendedArray;
function  DynArrayDupCurrency(const V: Currency; const Count: Integer): CurrencyArray;
{$IFDEF SupportAnsiString}
function  DynArrayDupAnsiString(const V: AnsiString; const Count: Integer): AnsiStringArray;
{$ENDIF}
function  DynArrayDupUnicodeString(const V: UnicodeString; const Count: Integer): UnicodeStringArray;
function  DynArrayDupString(const V: String; const Count: Integer): StringArray;
function  DynArrayDupByteCharSet(const V: ByteCharSet; const Count: Integer): ByteCharSetArray;
function  DynArrayDupObject(const V: TObject; const Count: Integer): ObjectArray;

procedure SetLengthAndZero(var V: ByteArray; const NewLength: Integer); overload;
procedure SetLengthAndZero(var V: Word16Array; const NewLength: Integer); overload;
procedure SetLengthAndZero(var V: Word32Array; const NewLength: Integer); overload;
procedure SetLengthAndZero(var V: Word64Array; const NewLength: Integer); overload;
procedure SetLengthAndZero(var V: LongWordArray; const NewLength: Integer); overload;
procedure SetLengthAndZero(var V: CardinalArray; const NewLength: Integer); overload;
procedure SetLengthAndZero(var V: NativeUIntArray; const NewLength: Integer); overload;
procedure SetLengthAndZero(var V: ShortIntArray; const NewLength: Integer); overload;
procedure SetLengthAndZero(var V: SmallIntArray; const NewLength: Integer); overload;
procedure SetLengthAndZero(var V: LongIntArray; const NewLength: Integer); overload;
procedure SetLengthAndZero(var V: IntegerArray; const NewLength: Integer); overload;
procedure SetLengthAndZero(var V: Int32Array; const NewLength: Integer); overload;
procedure SetLengthAndZero(var V: Int64Array; const NewLength: Integer); overload;
procedure SetLengthAndZero(var V: NativeIntArray; const NewLength: Integer); overload;
procedure SetLengthAndZero(var V: SingleArray; const NewLength: Integer); overload;
procedure SetLengthAndZero(var V: DoubleArray; const NewLength: Integer); overload;
procedure SetLengthAndZero(var V: ExtendedArray; const NewLength: Integer); overload;
procedure SetLengthAndZero(var V: CurrencyArray; const NewLength: Integer); overload;
procedure SetLengthAndZero(var V: ByteCharSetArray; const NewLength: Integer); overload;
procedure SetLengthAndZero(var V: BooleanArray; const NewLength: Integer); overload;
procedure SetLengthAndZero(var V: PointerArray; const NewLength: Integer); overload;
procedure SetLengthAndZero(var V: ObjectArray; const NewLength: Integer;
          const FreeObjects: Boolean = False); overload;

function  DynArrayIsEqual(const V1, V2: ByteArray): Boolean; overload;
function  DynArrayIsEqual(const V1, V2: Word16Array): Boolean; overload;
function  DynArrayIsEqual(const V1, V2: LongWordArray): Boolean; overload;
function  DynArrayIsEqual(const V1, V2: ShortIntArray): Boolean; overload;
function  DynArrayIsEqual(const V1, V2: SmallIntArray): Boolean; overload;
function  DynArrayIsEqual(const V1, V2: LongIntArray): Boolean; overload;
function  DynArrayIsEqual(const V1, V2: Int64Array): Boolean; overload;
function  DynArrayIsEqual(const V1, V2: SingleArray): Boolean; overload;
function  DynArrayIsEqual(const V1, V2: DoubleArray): Boolean; overload;
function  DynArrayIsEqual(const V1, V2: ExtendedArray): Boolean; overload;
function  DynArrayIsEqual(const V1, V2: CurrencyArray): Boolean; overload;
{$IFDEF SupportAnsiString}
function  DynArrayIsEqualA(const V1, V2: AnsiStringArray): Boolean; overload;
{$ENDIF}
function  DynArrayIsEqualB(const V1, V2: RawByteStringArray): Boolean; overload;
function  DynArrayIsEqualU(const V1, V2: UnicodeStringArray): Boolean; overload;
function  DynArrayIsEqual(const V1, V2: StringArray): Boolean; overload;
function  DynArrayIsEqual(const V1, V2: ByteCharSetArray): Boolean; overload;

function  ByteArrayToLongIntArray(const V: ByteArray): LongIntArray;
function  Word16ArrayToLongIntArray(const V: Word16Array): LongIntArray;
function  ShortIntArrayToLongIntArray(const V: ShortIntArray): LongIntArray;
function  SmallIntArrayToLongIntArray(const V: SmallIntArray): LongIntArray;
function  LongIntArrayToInt64Array(const V: LongIntArray): Int64Array;
function  LongIntArrayToSingleArray(const V: LongIntArray): SingleArray;
function  LongIntArrayToDoubleArray(const V: LongIntArray): DoubleArray;
function  LongIntArrayToExtendedArray(const V: LongIntArray): ExtendedArray;
function  SingleArrayToDoubleArray(const V: SingleArray): DoubleArray;
function  SingleArrayToExtendedArray(const V: SingleArray): ExtendedArray;
function  SingleArrayToCurrencyArray(const V: SingleArray): CurrencyArray;
function  SingleArrayToLongIntArray(const V: SingleArray): LongIntArray;
function  SingleArrayToInt64Array(const V: SingleArray): Int64Array;
function  DoubleArrayToExtendedArray(const V: DoubleArray): ExtendedArray;
function  DoubleArrayToCurrencyArray(const V: DoubleArray): CurrencyArray;
function  DoubleArrayToLongIntArray(const V: DoubleArray): LongIntArray;
function  DoubleArrayToInt64Array(const V: DoubleArray): Int64Array;
function  ExtendedArrayToCurrencyArray(const V: ExtendedArray): CurrencyArray;
function  ExtendedArrayToLongIntArray(const V: ExtendedArray): LongIntArray;
function  ExtendedArrayToInt64Array(const V: ExtendedArray): Int64Array;

function  ByteArrayFromIndexes(const V: ByteArray;
          const Indexes: IntegerArray): ByteArray;
function  Word16ArrayFromIndexes(const V: Word16Array;
          const Indexes: IntegerArray): Word16Array;
function  LongWordArrayFromIndexes(const V: LongWordArray;
          const Indexes: IntegerArray): LongWordArray;
function  CardinalArrayFromIndexes(const V: CardinalArray;
          const Indexes: IntegerArray): CardinalArray;
function  ShortIntArrayFromIndexes(const V: ShortIntArray;
          const Indexes: IntegerArray): ShortIntArray;
function  SmallIntArrayFromIndexes(const V: SmallIntArray;
          const Indexes: IntegerArray): SmallIntArray;
function  LongIntArrayFromIndexes(const V: LongIntArray;
          const Indexes: IntegerArray): LongIntArray;
function  IntegerArrayFromIndexes(const V: IntegerArray;
          const Indexes: IntegerArray): IntegerArray;
function  Int64ArrayFromIndexes(const V: Int64Array;
          const Indexes: IntegerArray): Int64Array;
function  SingleArrayFromIndexes(const V: SingleArray;
          const Indexes: IntegerArray): SingleArray;
function  DoubleArrayFromIndexes(const V: DoubleArray;
          const Indexes: IntegerArray): DoubleArray;
function  ExtendedArrayFromIndexes(const V: ExtendedArray;
          const Indexes: IntegerArray): ExtendedArray;
function  StringArrayFromIndexes(const V: StringArray;
          const Indexes: IntegerArray): StringArray;

procedure DynArraySort(const V: ByteArray); overload;
procedure DynArraySort(const V: Word16Array); overload;
procedure DynArraySort(const V: LongWordArray); overload;
procedure DynArraySort(const V: CardinalArray); overload;
procedure DynArraySort(const V: NativeUIntArray); overload;
procedure DynArraySort(const V: ShortIntArray); overload;
procedure DynArraySort(const V: SmallIntArray); overload;
procedure DynArraySort(const V: LongIntArray); overload;
procedure DynArraySort(const V: IntegerArray); overload;
procedure DynArraySort(const V: Int64Array); overload;
procedure DynArraySort(const V: NativeIntArray); overload;
procedure DynArraySort(const V: SingleArray); overload;
procedure DynArraySort(const V: DoubleArray); overload;
procedure DynArraySort(const V: ExtendedArray); overload;
{$IFDEF SupportAnsiString}
procedure DynArraySortA(const V: AnsiStringArray); overload;
{$ENDIF}
procedure DynArraySortB(const V: RawByteStringArray); overload;
procedure DynArraySortU(const V: UnicodeStringArray); overload;
procedure DynArraySort(const V: StringArray); overload;

procedure DynArraySort(const Key: IntegerArray; const Data: IntegerArray); overload;
procedure DynArraySort(const Key: IntegerArray; const Data: Int64Array); overload;
{$IFDEF SupportAnsiString}
procedure DynArraySort(const Key: IntegerArray; const Data: AnsiStringArray); overload;
{$ENDIF}
procedure DynArraySort(const Key: IntegerArray; const Data: ExtendedArray); overload;
procedure DynArraySort(const Key: IntegerArray; const Data: PointerArray); overload;
{$IFDEF SupportAnsiString}
procedure DynArraySort(const Key: AnsiStringArray; const Data: IntegerArray); overload;
procedure DynArraySort(const Key: AnsiStringArray; const Data: Int64Array); overload;
procedure DynArraySort(const Key: AnsiStringArray; const Data: AnsiStringArray); overload;
procedure DynArraySort(const Key: AnsiStringArray; const Data: ExtendedArray); overload;
procedure DynArraySort(const Key: AnsiStringArray; const Data: PointerArray); overload;
{$ENDIF}
procedure DynArraySort(const Key: ExtendedArray; const Data: IntegerArray); overload;
procedure DynArraySort(const Key: ExtendedArray; const Data: Int64Array); overload;
{$IFDEF SupportAnsiString}
procedure DynArraySort(const Key: ExtendedArray; const Data: AnsiStringArray); overload;
{$ENDIF}
procedure DynArraySort(const Key: ExtendedArray; const Data: ExtendedArray); overload;
procedure DynArraySort(const Key: ExtendedArray; const Data: PointerArray); overload;



{                                                                              }
{ Test cases                                                                   }
{                                                                              }
{$IFDEF DYNARRAYS_TEST}
procedure Test;
{$ENDIF}



implementation



{                                                                              }
{ DynArrayAppend                                                               }
{                                                                              }
function DynArrayAppend(var V: ByteArray; const R: Byte): Integer;
begin
  Result := Length(V);
  SetLength(V, Result + 1);
  V[Result] := R;
end;

function DynArrayAppend(var V: Word16Array; const R: Word16): Integer;
begin
  Result := Length(V);
  SetLength(V, Result + 1);
  V[Result] := R;
end;

function DynArrayAppend(var V: Word32Array; const R: Word32): Integer;
begin
  Result := Length(V);
  SetLength(V, Result + 1);
  V[Result] := R;
end;

function DynArrayAppend(var V: Word64Array; const R: Word64): Integer;
begin
  Result := Length(V);
  SetLength(V, Result + 1);
  V[Result] := R;
end;

function DynArrayAppend(var V: LongWordArray; const R: LongWord): Integer;
begin
  Result := Length(V);
  SetLength(V, Result + 1);
  V[Result] := R;
end;

function DynArrayAppend(var V: CardinalArray; const R: Cardinal): Integer;
begin
  Result := Length(V);
  SetLength(V, Result + 1);
  V[Result] := R;
end;

function DynArrayAppend(var V: NativeUIntArray; const R: NativeUInt): Integer;
begin
  Result := Length(V);
  SetLength(V, Result + 1);
  V[Result] := R;
end;

function DynArrayAppend(var V: ShortIntArray; const R: ShortInt): Integer;
begin
  Result := Length(V);
  SetLength(V, Result + 1);
  V[Result] := R;
end;

function DynArrayAppend(var V: SmallIntArray; const R: SmallInt): Integer;
begin
  Result := Length(V);
  SetLength(V, Result + 1);
  V[Result] := R;
end;

function DynArrayAppend(var V: Int32Array; const R: Int32): Integer;
begin
  Result := Length(V);
  SetLength(V, Result + 1);
  V[Result] := R;
end;

function DynArrayAppend(var V: LongIntArray; const R: LongInt): Integer;
begin
  Result := Length(V);
  SetLength(V, Result + 1);
  V[Result] := R;
end;

function DynArrayAppend(var V: IntegerArray; const R: Integer): Integer;
begin
  Result := Length(V);
  SetLength(V, Result + 1);
  V[Result] := R;
end;

function DynArrayAppend(var V: Int64Array; const R: Int64): Integer;
begin
  Result := Length(V);
  SetLength(V, Result + 1);
  V[Result] := R;
end;

function DynArrayAppend(var V: NativeIntArray; const R: NativeInt): Integer;
begin
  Result := Length(V);
  SetLength(V, Result + 1);
  V[Result] := R;
end;

function DynArrayAppend(var V: SingleArray; const R: Single): Integer;
begin
  Result := Length(V);
  SetLength(V, Result + 1);
  V[Result] := R;
end;

function DynArrayAppend(var V: DoubleArray; const R: Double): Integer;
begin
  Result := Length(V);
  SetLength(V, Result + 1);
  V[Result] := R;
end;

function DynArrayAppend(var V: ExtendedArray; const R: Extended): Integer;
begin
  Result := Length(V);
  SetLength(V, Result + 1);
  V[Result] := R;
end;

function DynArrayAppend(var V: CurrencyArray; const R: Currency): Integer;
begin
  Result := Length(V);
  SetLength(V, Result + 1);
  V[Result] := R;
end;

function DynArrayAppend(var V: BooleanArray; const R: Boolean): Integer;
begin
  Result := Length(V);
  SetLength(V, Result + 1);
  V[Result] := R;
end;

{$IFDEF SupportAnsiString}
function DynArrayAppendA(var V: AnsiStringArray; const R: AnsiString): Integer;
begin
  Result := Length(V);
  SetLength(V, Result + 1);
  V[Result] := R;
end;

{$ENDIF}
function DynArrayAppendB(var V: RawByteStringArray; const R: RawByteString): Integer;
begin
  Result := Length(V);
  SetLength(V, Result + 1);
  V[Result] := R;
end;

function DynArrayAppendU(var V: UnicodeStringArray; const R: UnicodeString): Integer;
begin
  Result := Length(V);
  SetLength(V, Result + 1);
  V[Result] := R;
end;

function DynArrayAppend(var V: StringArray; const R: String): Integer;
begin
  Result := Length(V);
  SetLength(V, Result + 1);
  V[Result] := R;
end;

function DynArrayAppend(var V: PointerArray; const R: Pointer): Integer;
begin
  Result := Length(V);
  SetLength(V, Result + 1);
  V[Result] := R;
end;

function DynArrayAppend(var V: ObjectArray; const R: TObject): Integer;
begin
  Result := Length(V);
  SetLength(V, Result + 1);
  V[Result] := R;
end;

function DynArrayAppend(var V: InterfaceArray; const R: IInterface): Integer;
begin
  Result := Length(V);
  SetLength(V, Result + 1);
  V[Result] := R;
end;

function DynArrayAppend(var V: ByteSetArray; const R: ByteSet): Integer;
begin
  Result := Length(V);
  SetLength(V, Result + 1);
  V[Result] := R;
end;

function DynArrayAppend(var V: ByteCharSetArray; const R: ByteCharSet): Integer;
begin
  Result := Length(V);
  SetLength(V, Result + 1);
  V[Result] := R;
end;



function DynArrayAppendByteArray(var V: ByteArray; const R: array of Byte): Integer;
var L : Integer;
begin
  Result := Length(V);
  L := Length(R);
  if L > 0 then
    begin
      SetLength(V, Result + L);
      Move(R[0], V[Result], Sizeof(Byte) * L);
    end;
end;

function DynArrayAppendWord16Array(var V: Word16Array; const R: array of Word16): Integer;
var L : Integer;
begin
  Result := Length(V);
  L := Length(R);
  if L > 0 then
    begin
      SetLength(V, Result + L);
      Move(R[0], V[Result], Sizeof(Word16) * L);
    end;
end;

function DynArrayAppendWord32Array(var V: Word32Array; const R: array of Word32): Integer;
var L : Integer;
begin
  Result := Length(V);
  L := Length(R);
  if L > 0 then
    begin
      SetLength(V, Result + L);
      Move(R[0], V[Result], Sizeof(Word32) * L);
    end;
end;

function DynArrayAppendWord64Array(var V: Word64Array; const R: array of Word64): Integer;
var L : Integer;
begin
  Result := Length(V);
  L := Length(R);
  if L > 0 then
    begin
      SetLength(V, Result + L);
      Move(R[0], V[Result], Sizeof(Word64) * L);
    end;
end;

function DynArrayAppendCardinalArray(var V: CardinalArray; const R: array of Cardinal): Integer;
var L : Integer;
begin
  Result := Length(V);
  L := Length(R);
  if L > 0 then
    begin
      SetLength(V, Result + L);
      Move(R[0], V[Result], Sizeof(Cardinal) * L);
    end;
end;

function DynArrayAppendNativeUIntArray(var V: NativeUIntArray; const R: array of NativeUInt): Integer;
var L : Integer;
begin
  Result := Length(V);
  L := Length(R);
  if L > 0 then
    begin
      SetLength(V, Result + L);
      Move(R[0], V[Result], Sizeof(NativeUInt) * L);
    end;
end;

function DynArrayAppendShortIntArray(var V: ShortIntArray; const R: array of ShortInt): Integer;
var L : Integer;
begin
  Result := Length(V);
  L := Length(R);
  if L > 0 then
    begin
      SetLength(V, Result + L);
      Move(R[0], V[Result], Sizeof(ShortInt) * L);
    end;
end;

function DynArrayAppendSmallIntArray(var V: SmallIntArray; const R: array of SmallInt): Integer;
var L : Integer;
begin
  Result := Length(V);
  L := Length(R);
  if L > 0 then
    begin
      SetLength(V, Result + L);
      Move(R[0], V[Result], Sizeof(SmallInt) * L);
    end;
end;

function DynArrayAppendLongIntArray(var V: LongIntArray; const R: array of LongInt): Integer;
var L : Integer;
begin
  Result := Length(V);
  L := Length(R);
  if L > 0 then
    begin
      SetLength(V, Result + L);
      Move(R[0], V[Result], Sizeof(LongInt) * L);
    end;
end;

function DynArrayAppendInt32Array(var V: Int32Array; const R: array of Int32): Integer;
var L : Integer;
begin
  Result := Length(V);
  L := Length(R);
  if L > 0 then
    begin
      SetLength(V, Result + L);
      Move(R[0], V[Result], Sizeof(Int32) * L);
    end;
end;

function DynArrayAppendIntegerArray(var V: IntegerArray; const R: array of Integer): Integer;
var L : Integer;
begin
  Result := Length(V);
  L := Length(R);
  if L > 0 then
    begin
      SetLength(V, Result + L);
      Move(R[0], V[Result], Sizeof(Integer) * L);
    end;
end;

function DynArrayAppendInt64Array(var V: Int64Array; const R: array of Int64): Integer;
var L : Integer;
begin
  Result := Length(V);
  L := Length(R);
  if L > 0 then
    begin
      SetLength(V, Result + L);
      Move(R[0], V[Result], Sizeof(Int64) * L);
    end;
end;

function DynArrayAppendNativeIntArray(var V: NativeIntArray; const R: array of NativeInt): Integer;
var L : Integer;
begin
  Result := Length(V);
  L := Length(R);
  if L > 0 then
    begin
      SetLength(V, Result + L);
      Move(R[0], V[Result], Sizeof(NativeInt) * L);
    end;
end;

function DynArrayAppendSingleArray(var V: SingleArray; const R: array of Single): Integer;
var L : Integer;
begin
  Result := Length(V);
  L := Length(R);
  if L > 0 then
    begin
      SetLength(V, Result + L);
      Move(R[0], V[Result], Sizeof(Single) * L);
    end;
end;

function DynArrayAppendDoubleArray(var V: DoubleArray; const R: array of Double): Integer;
var L : Integer;
begin
  Result := Length(V);
  L := Length(R);
  if L > 0 then
    begin
      SetLength(V, Result + L);
      Move(R[0], V[Result], Sizeof(Double) * L);
    end;
end;

function DynArrayAppendExtendedArray(var V: ExtendedArray; const R: array of Extended): Integer;
var L : Integer;
begin
  Result := Length(V);
  L := Length(R);
  if L > 0 then
    begin
      SetLength(V, Result + L);
      Move(R[0], V[Result], Sizeof(Extended) * L);
    end;
end;

function DynArrayAppendCurrencyArray(var V: CurrencyArray; const R: array of Currency): Integer;
var L : Integer;
begin
  Result := Length(V);
  L := Length(R);
  if L > 0 then
    begin
      SetLength(V, Result + L);
      Move(R[0], V[Result], Sizeof(Currency) * L);
    end;
end;

function DynArrayAppendPointerArray(var V: PointerArray; const R: array of Pointer): Integer;
var L : Integer;
begin
  Result := Length(V);
  L := Length(R);
  if L > 0 then
    begin
      SetLength(V, Result + L);
      Move(R[0], V[Result], Sizeof(Pointer) * L);
    end;
end;

function DynArrayAppendByteCharSetArray(var V: ByteCharSetArray; const R: array of ByteCharSet): Integer;
var L : Integer;
begin
  Result := Length(V);
  L := Length(R);
  if L > 0 then
    begin
      SetLength(V, Result + L);
      Move(R[0], V[Result], Sizeof(ByteCharSet) * L);
    end;
end;

function DynArrayAppendByteSetArray(var V: ByteSetArray; const R: array of ByteSet): Integer;
var L : Integer;
begin
  Result := Length(V);
  L := Length(R);
  if L > 0 then
    begin
      SetLength(V, Result + L);
      Move(R[0], V[Result], Sizeof(ByteSet) * L);
    end;
end;


function DynArrayAppendObjectArray(var V: ObjectArray; const R: ObjectArray): Integer;
var I, LR : Integer;
begin
  Result := Length(V);
  LR := Length(R);
  if LR > 0 then
    begin
      SetLength(V, Result + LR);
      for I := 0 to LR - 1 do
        V[Result + I] := R[I];
    end;
end;

{$IFDEF SupportAnsiString}
function DynArrayAppendAnsiStringArray(var V: AnsiStringArray; const R: array of AnsiString): Integer;
var I, LR : Integer;
begin
  Result := Length(V);
  LR := Length(R);
  if LR > 0 then
    begin
      SetLength(V, Result + LR);
      for I := 0 to LR - 1 do
        V[Result + I] := R[I];
    end;
end;
{$ENDIF}

function DynArrayAppendRawByteStringArray(var V: RawByteStringArray; const R: array of RawByteString): Integer;
var I, LR : Integer;
begin
  Result := Length(V);
  LR := Length(R);
  if LR > 0 then
    begin
      SetLength(V, Result + LR);
      for I := 0 to LR - 1 do
        V[Result + I] := R[I];
    end;
end;

function DynArrayAppendUnicodeStringArray(var V: UnicodeStringArray; const R: array of UnicodeString): Integer;
var I, LR : Integer;
begin
  Result := Length(V);
  LR := Length(R);
  if LR > 0 then
    begin
      SetLength(V, Result + LR);
      for I := 0 to LR - 1 do
        V[Result + I] := R[I];
    end;
end;

function DynArrayAppendStringArray(var V: StringArray; const R: array of String): Integer;
var I, LR : Integer;
begin
  Result := Length(V);
  LR := Length(R);
  if LR > 0 then
    begin
      SetLength(V, Result + LR);
      for I := 0 to LR - 1 do
        V[Result + I] := R[I];
    end;
end;



{                                                                              }
{ DynArrayRemove                                                               }
{                                                                              }
function DynArrayRemove(var V: ByteArray; const Idx: Integer; const Count: Integer): Integer;
var I, J, L, M: Integer;
begin
  L := Length(V);
  if (Idx >= L) or (Idx + Count <= 0) or (L = 0) or (Count = 0) then
    begin
      Result := 0;
      exit;
    end;
  I := MaxInt(Idx, 0);
  J := MinInt(Count, L - I);
  M := L - J - I;
  if M > 0 then
    Move(V[I + J], V[I], M * SizeOf(Byte));
  SetLength(V, L - J);
  Result := J;
end;

function DynArrayRemove(var V: Word16Array; const Idx: Integer; const Count: Integer): Integer;
var I, J, L, M: Integer;
begin
  L := Length(V);
  if (Idx >= L) or (Idx + Count <= 0) or (L = 0) or (Count = 0) then
    begin
      Result := 0;
      exit;
    end;
  I := MaxInt(Idx, 0);
  J := MinInt(Count, L - I);
  M := L - J - I;
  if M > 0 then
    Move(V[I + J], V[I], M * SizeOf(Word16));
  SetLength(V, L - J);
  Result := J;
end;

function DynArrayRemove(var V: Word32Array; const Idx: Integer; const Count: Integer): Integer;
var I, J, L, M: Integer;
begin
  L := Length(V);
  if (Idx >= L) or (Idx + Count <= 0) or (L = 0) or (Count = 0) then
    begin
      Result := 0;
      exit;
    end;
  I := MaxInt(Idx, 0);
  J := MinInt(Count, L - I);
  M := L - J - I;
  if M > 0 then
    Move(V[I + J], V[I], M * SizeOf(Word32));
  SetLength(V, L - J);
  Result := J;
end;

function DynArrayRemove(var V: Word64Array; const Idx: Integer; const Count: Integer): Integer;
var I, J, L, M: Integer;
begin
  L := Length(V);
  if (Idx >= L) or (Idx + Count <= 0) or (L = 0) or (Count = 0) then
    begin
      Result := 0;
      exit;
    end;
  I := MaxInt(Idx, 0);
  J := MinInt(Count, L - I);
  M := L - J - I;
  if M > 0 then
    Move(V[I + J], V[I], M * SizeOf(Word64));
  SetLength(V, L - J);
  Result := J;
end;

function DynArrayRemove(var V: LongWordArray; const Idx: Integer; const Count: Integer): Integer;
var I, J, L, M: Integer;
begin
  L := Length(V);
  if (Idx >= L) or (Idx + Count <= 0) or (L = 0) or (Count = 0) then
    begin
      Result := 0;
      exit;
    end;
  I := MaxInt(Idx, 0);
  J := MinInt(Count, L - I);
  M := L - J - I;
  if M > 0 then
    Move(V[I + J], V[I], M * SizeOf(LongWord));
  SetLength(V, L - J);
  Result := J;
end;

function DynArrayRemove(var V: CardinalArray; const Idx: Integer; const Count: Integer): Integer;
var I, J, L, M: Integer;
begin
  L := Length(V);
  if (Idx >= L) or (Idx + Count <= 0) or (L = 0) or (Count = 0) then
    begin
      Result := 0;
      exit;
    end;
  I := MaxInt(Idx, 0);
  J := MinInt(Count, L - I);
  M := L - J - I;
  if M > 0 then
    Move(V[I + J], V[I], M * SizeOf(Cardinal));
  SetLength(V, L - J);
  Result := J;
end;

function DynArrayRemove(var V: NativeUIntArray; const Idx: Integer; const Count: Integer): Integer;
var I, J, L, M: Integer;
begin
  L := Length(V);
  if (Idx >= L) or (Idx + Count <= 0) or (L = 0) or (Count = 0) then
    begin
      Result := 0;
      exit;
    end;
  I := MaxInt(Idx, 0);
  J := MinInt(Count, L - I);
  M := L - J - I;
  if M > 0 then
    Move(V[I + J], V[I], M * SizeOf(NativeUInt));
  SetLength(V, L - J);
  Result := J;
end;

function DynArrayRemove(var V: ShortIntArray; const Idx: Integer; const Count: Integer): Integer;
var I, J, L, M: Integer;
begin
  L := Length(V);
  if (Idx >= L) or (Idx + Count <= 0) or (L = 0) or (Count = 0) then
    begin
      Result := 0;
      exit;
    end;
  I := MaxInt(Idx, 0);
  J := MinInt(Count, L - I);
  M := L - J - I;
  if M > 0 then
    Move(V[I + J], V[I], M * SizeOf(ShortInt));
  SetLength(V, L - J);
  Result := J;
end;

function DynArrayRemove(var V: SmallIntArray; const Idx: Integer; const Count: Integer): Integer;
var I, J, L, M: Integer;
begin
  L := Length(V);
  if (Idx >= L) or (Idx + Count <= 0) or (L = 0) or (Count = 0) then
    begin
      Result := 0;
      exit;
    end;
  I := MaxInt(Idx, 0);
  J := MinInt(Count, L - I);
  M := L - J - I;
  if M > 0 then
    Move(V[I + J], V[I], M * SizeOf(SmallInt));
  SetLength(V, L - J);
  Result := J;
end;

function DynArrayRemove(var V: LongIntArray; const Idx: Integer; const Count: Integer): Integer;
var I, J, L, M: Integer;
begin
  L := Length(V);
  if (Idx >= L) or (Idx + Count <= 0) or (L = 0) or (Count = 0) then
    begin
      Result := 0;
      exit;
    end;
  I := MaxInt(Idx, 0);
  J := MinInt(Count, L - I);
  M := L - J - I;
  if M > 0 then
    Move(V[I + J], V[I], M * SizeOf(LongInt));
  SetLength(V, L - J);
  Result := J;
end;

function DynArrayRemove(var V: IntegerArray; const Idx: Integer; const Count: Integer): Integer;
var I, J, L, M: Integer;
begin
  L := Length(V);
  if (Idx >= L) or (Idx + Count <= 0) or (L = 0) or (Count = 0) then
    begin
      Result := 0;
      exit;
    end;
  I := MaxInt(Idx, 0);
  J := MinInt(Count, L - I);
  M := L - J - I;
  if M > 0 then
    Move(V[I + J], V[I], M * SizeOf(Integer));
  SetLength(V, L - J);
  Result := J;
end;

function DynArrayRemove(var V: Int32Array; const Idx: Integer; const Count: Integer): Integer;
var I, J, L, M: Integer;
begin
  L := Length(V);
  if (Idx >= L) or (Idx + Count <= 0) or (L = 0) or (Count = 0) then
    begin
      Result := 0;
      exit;
    end;
  I := MaxInt(Idx, 0);
  J := MinInt(Count, L - I);
  M := L - J - I;
  if M > 0 then
    Move(V[I + J], V[I], M * SizeOf(Int32));
  SetLength(V, L - J);
  Result := J;
end;

function DynArrayRemove(var V: Int64Array; const Idx: Integer; const Count: Integer): Integer;
var I, J, L, M: Integer;
begin
  L := Length(V);
  if (Idx >= L) or (Idx + Count <= 0) or (L = 0) or (Count = 0) then
    begin
      Result := 0;
      exit;
    end;
  I := MaxInt(Idx, 0);
  J := MinInt(Count, L - I);
  M := L - J - I;
  if M > 0 then
    Move(V[I + J], V[I], M * SizeOf(Int64));
  SetLength(V, L - J);
  Result := J;
end;

function DynArrayRemove(var V: NativeIntArray; const Idx: Integer; const Count: Integer): Integer;
var I, J, L, M: Integer;
begin
  L := Length(V);
  if (Idx >= L) or (Idx + Count <= 0) or (L = 0) or (Count = 0) then
    begin
      Result := 0;
      exit;
    end;
  I := MaxInt(Idx, 0);
  J := MinInt(Count, L - I);
  M := L - J - I;
  if M > 0 then
    Move(V[I + J], V[I], M * SizeOf(NativeInt));
  SetLength(V, L - J);
  Result := J;
end;

function DynArrayRemove(var V: SingleArray; const Idx: Integer; const Count: Integer): Integer;
var I, J, L, M: Integer;
begin
  L := Length(V);
  if (Idx >= L) or (Idx + Count <= 0) or (L = 0) or (Count = 0) then
    begin
      Result := 0;
      exit;
    end;
  I := MaxInt(Idx, 0);
  J := MinInt(Count, L - I);
  M := L - J - I;
  if M > 0 then
    Move(V[I + J], V[I], M * SizeOf(Single));
  SetLength(V, L - J);
  Result := J;
end;

function DynArrayRemove(var V: DoubleArray; const Idx: Integer; const Count: Integer): Integer;
var I, J, L, M: Integer;
begin
  L := Length(V);
  if (Idx >= L) or (Idx + Count <= 0) or (L = 0) or (Count = 0) then
    begin
      Result := 0;
      exit;
    end;
  I := MaxInt(Idx, 0);
  J := MinInt(Count, L - I);
  M := L - J - I;
  if M > 0 then
    Move(V[I + J], V[I], M * SizeOf(Double));
  SetLength(V, L - J);
  Result := J;
end;

function DynArrayRemove(var V: ExtendedArray; const Idx: Integer; const Count: Integer): Integer;
var I, J, L, M: Integer;
begin
  L := Length(V);
  if (Idx >= L) or (Idx + Count <= 0) or (L = 0) or (Count = 0) then
    begin
      Result := 0;
      exit;
    end;
  I := MaxInt(Idx, 0);
  J := MinInt(Count, L - I);
  M := L - J - I;
  if M > 0 then
    Move(V[I + J], V[I], M * SizeOf(Extended));
  SetLength(V, L - J);
  Result := J;
end;

function DynArrayRemove(var V: CurrencyArray; const Idx: Integer; const Count: Integer): Integer;
var I, J, L, M: Integer;
begin
  L := Length(V);
  if (Idx >= L) or (Idx + Count <= 0) or (L = 0) or (Count = 0) then
    begin
      Result := 0;
      exit;
    end;
  I := MaxInt(Idx, 0);
  J := MinInt(Count, L - I);
  M := L - J - I;
  if M > 0 then
    Move(V[I + J], V[I], M * SizeOf(Currency));
  SetLength(V, L - J);
  Result := J;
end;

function DynArrayRemove(var V: PointerArray; const Idx: Integer; const Count: Integer): Integer;
var I, J, L, M: Integer;
begin
  L := Length(V);
  if (Idx >= L) or (Idx + Count <= 0) or (L = 0) or (Count = 0) then
    begin
      Result := 0;
      exit;
    end;
  I := MaxInt(Idx, 0);
  J := MinInt(Count, L - I);
  M := L - J - I;
  if M > 0 then
    Move(V[I + J], V[I], M * SizeOf(Pointer));
  SetLength(V, L - J);
  Result := J;
end;


function DynArrayRemove(var V: ObjectArray; const Idx: Integer; const Count: Integer;
    const FreeObjects: Boolean): Integer;
var I, J, K, L, M, F : Integer;
begin
  L := Length(V);
  if (Idx >= L) or (Idx + Count <= 0) or (L = 0) or (Count = 0) then
    begin
      Result := 0;
      exit;
    end;
  I := MaxInt(Idx, 0);
  J := MinInt(Count, L - I);
  if FreeObjects then
    for K := I to I + J - 1 do
      FreeAndNil(V[K]);
  M := L - J - I;
  for F := I to I + M - 1 do
    V[F] := V[F + J];
  SetLength(V, L - J);
  Result := J;
end;

{$IFDEF SupportAnsiString}
function DynArrayRemoveA(var V: AnsiStringArray; const Idx: Integer; const Count: Integer): Integer;
var I, J, K, L : Integer;
begin
  L := Length(V);
  if (Idx >= L) or (Idx + Count <= 0) or (L = 0) or (Count = 0) then
    begin
      Result := 0;
      exit;
    end;
  I := MaxInt(Idx, 0);
  J := MinInt(Count, L - I);
  for K := I to L - J - 1 do
    V[K] := V[K + J];
  SetLength(V, L - J);
  Result := J;
end;
{$ENDIF}

function DynArrayRemoveB(var V: RawByteStringArray; const Idx: Integer; const Count: Integer): Integer;
var I, J, K, L : Integer;
begin
  L := Length(V);
  if (Idx >= L) or (Idx + Count <= 0) or (L = 0) or (Count = 0) then
    begin
      Result := 0;
      exit;
    end;
  I := MaxInt(Idx, 0);
  J := MinInt(Count, L - I);
  for K := I to L - J - 1 do
    V[K] := V[K + J];
  SetLength(V, L - J);
  Result := J;
end;

function DynArrayRemoveU(var V: UnicodeStringArray; const Idx: Integer; const Count: Integer): Integer;
var I, J, K, L : Integer;
begin
  L := Length(V);
  if (Idx >= L) or (Idx + Count <= 0) or (L = 0) or (Count = 0) then
    begin
      Result := 0;
      exit;
    end;
  I := MaxInt(Idx, 0);
  J := MinInt(Count, L - I);
  for K := I to L - J - 1 do
    V[K] := V[K + J];
  SetLength(V, L - J);
  Result := J;
end;

function DynArrayRemove(var V: StringArray; const Idx: Integer; const Count: Integer): Integer;
var I, J, K, L : Integer;
begin
  L := Length(V);
  if (Idx >= L) or (Idx + Count <= 0) or (L = 0) or (Count = 0) then
    begin
      Result := 0;
      exit;
    end;
  I := MaxInt(Idx, 0);
  J := MinInt(Count, L - I);
  for K := I to L - J - 1 do
    V[K] := V[K + J];
  SetLength(V, L - J);
  Result := J;
end;

function DynArrayRemove(var V: InterfaceArray; const Idx: Integer; const Count: Integer): Integer;
var I, J, K, L, M : Integer;
begin
  L := Length(V);
  if (Idx >= L) or (Idx + Count <= 0) or (L = 0) or (Count = 0) then
    begin
      Result := 0;
      exit;
    end;
  I := MaxInt(Idx, 0);
  J := MinInt(Count, L - I);
  for K := I to I + J - 1 do
    V[K] := nil;
  M := L - J - I;
  if M > 0 then
    Move(V[I + J], V[I], M * SizeOf(IInterface));
  FillChar(V[L - J], J * SizeOf(IInterface), #0);
  SetLength(V, L - J);
  Result := J;
end;



{                                                                              }
{ DynArrayRemoveDuplicates                                                     }
{                                                                              }
procedure DynArrayRemoveDuplicates(var V: ByteArray; const IsSorted: Boolean);
var I, C, J, L : Integer;
    F          : Byte;
begin
  L := Length(V);
  if L = 0 then
    exit;

  if IsSorted then
    begin
      J := 0;
      repeat
        F := V[J];
        I := J + 1;
        while (I < L) and (V[I] = F) do
          Inc(I);
        C := I - J;
        if C > 1 then
          begin
            DynArrayRemove(V, J + 1, C - 1);
            Dec(L, C - 1);
            Inc(J);
          end
        else
          J := I;
      until J >= L;
    end else
    begin
      J := 0;
      repeat
        repeat
          I := DynArrayPosNext(V[J], V, J);
          if I >= 0 then
            DynArrayRemove(V, I, 1);
        until I < 0;
        Inc(J);
      until J >= Length(V);
    end;
end;

procedure DynArrayRemoveDuplicates(var V: Word16Array; const IsSorted: Boolean);
var I, C, J, L : Integer;
    F          : Word16;
begin
  L := Length(V);
  if L = 0 then
    exit;

  if IsSorted then
    begin
      J := 0;
      repeat
        F := V[J];
        I := J + 1;
        while (I < L) and (V[I] = F) do
          Inc(I);
        C := I - J;
        if C > 1 then
          begin
            DynArrayRemove(V, J + 1, C - 1);
            Dec(L, C - 1);
            Inc(J);
          end
        else
          J := I;
      until J >= L;
    end else
    begin
      J := 0;
      repeat
        repeat
          I := DynArrayPosNext(V[J], V, J);
          if I >= 0 then
            DynArrayRemove(V, I, 1);
        until I < 0;
        Inc(J);
      until J >= Length(V);
    end;
end;

procedure DynArrayRemoveDuplicates(var V: LongWordArray; const IsSorted: Boolean);
var I, C, J, L : Integer;
    F          : LongWord;
begin
  L := Length(V);
  if L = 0 then
    exit;

  if IsSorted then
    begin
      J := 0;
      repeat
        F := V[J];
        I := J + 1;
        while (I < L) and (V[I] = F) do
          Inc(I);
        C := I - J;
        if C > 1 then
          begin
            DynArrayRemove(V, J + 1, C - 1);
            Dec(L, C - 1);
            Inc(J);
          end
        else
          J := I;
      until J >= L;
    end else
    begin
      J := 0;
      repeat
        repeat
          I := DynArrayPosNext(V[J], V, J);
          if I >= 0 then
            DynArrayRemove(V, I, 1);
        until I < 0;
        Inc(J);
      until J >= Length(V);
    end;
end;

procedure DynArrayRemoveDuplicates(var V: ShortIntArray; const IsSorted: Boolean);
var I, C, J, L : Integer;
    F          : ShortInt;
begin
  L := Length(V);
  if L = 0 then
    exit;

  if IsSorted then
    begin
      J := 0;
      repeat
        F := V[J];
        I := J + 1;
        while (I < L) and (V[I] = F) do
          Inc(I);
        C := I - J;
        if C > 1 then
          begin
            DynArrayRemove(V, J + 1, C - 1);
            Dec(L, C - 1);
            Inc(J);
          end
        else
          J := I;
      until J >= L;
    end else
    begin
      J := 0;
      repeat
        repeat
          I := DynArrayPosNext(V[J], V, J);
          if I >= 0 then
            DynArrayRemove(V, I, 1);
        until I < 0;
        Inc(J);
      until J >= Length(V);
    end;
end;

procedure DynArrayRemoveDuplicates(var V: SmallIntArray; const IsSorted: Boolean);
var I, C, J, L : Integer;
    F          : SmallInt;
begin
  L := Length(V);
  if L = 0 then
    exit;

  if IsSorted then
    begin
      J := 0;
      repeat
        F := V[J];
        I := J + 1;
        while (I < L) and (V[I] = F) do
          Inc(I);
        C := I - J;
        if C > 1 then
          begin
            DynArrayRemove(V, J + 1, C - 1);
            Dec(L, C - 1);
            Inc(J);
          end
        else
          J := I;
      until J >= L;
    end else
    begin
      J := 0;
      repeat
        repeat
          I := DynArrayPosNext(V[J], V, J);
          if I >= 0 then
            DynArrayRemove(V, I, 1);
        until I < 0;
        Inc(J);
      until J >= Length(V);
    end;
end;

procedure DynArrayRemoveDuplicates(var V: LongIntArray; const IsSorted: Boolean);
var I, C, J, L : Integer;
    F          : LongInt;
begin
  L := Length(V);
  if L = 0 then
    exit;

  if IsSorted then
    begin
      J := 0;
      repeat
        F := V[J];
        I := J + 1;
        while (I < L) and (V[I] = F) do
          Inc(I);
        C := I - J;
        if C > 1 then
          begin
            DynArrayRemove(V, J + 1, C - 1);
            Dec(L, C - 1);
            Inc(J);
          end
        else
          J := I;
      until J >= L;
    end else
    begin
      J := 0;
      repeat
        repeat
          I := DynArrayPosNext(V[J], V, J);
          if I >= 0 then
            DynArrayRemove(V, I, 1);
        until I < 0;
        Inc(J);
      until J >= Length(V);
    end;
end;

procedure DynArrayRemoveDuplicates(var V: Int64Array; const IsSorted: Boolean);
var I, C, J, L : Integer;
    F          : Int64;
begin
  L := Length(V);
  if L = 0 then
    exit;

  if IsSorted then
    begin
      J := 0;
      repeat
        F := V[J];
        I := J + 1;
        while (I < L) and (V[I] = F) do
          Inc(I);
        C := I - J;
        if C > 1 then
          begin
            DynArrayRemove(V, J + 1, C - 1);
            Dec(L, C - 1);
            Inc(J);
          end
        else
          J := I;
      until J >= L;
    end else
    begin
      J := 0;
      repeat
        repeat
          I := DynArrayPosNext(V[J], V, J);
          if I >= 0 then
            DynArrayRemove(V, I, 1);
        until I < 0;
        Inc(J);
      until J >= Length(V);
    end;
end;

procedure DynArrayRemoveDuplicates(var V: SingleArray; const IsSorted: Boolean);
var I, C, J, L : Integer;
    F          : Single;
begin
  L := Length(V);
  if L = 0 then
    exit;

  if IsSorted then
    begin
      J := 0;
      repeat
        F := V[J];
        I := J + 1;
        while (I < L) and (V[I] = F) do
          Inc(I);
        C := I - J;
        if C > 1 then
          begin
            DynArrayRemove(V, J + 1, C - 1);
            Dec(L, C - 1);
            Inc(J);
          end
        else
          J := I;
      until J >= L;
    end else
    begin
      J := 0;
      repeat
        repeat
          I := DynArrayPosNext(V[J], V, J);
          if I >= 0 then
            DynArrayRemove(V, I, 1);
        until I < 0;
        Inc(J);
      until J >= Length(V);
    end;
end;

procedure DynArrayRemoveDuplicates(var V: DoubleArray; const IsSorted: Boolean);
var I, C, J, L : Integer;
    F          : Double;
begin
  L := Length(V);
  if L = 0 then
    exit;

  if IsSorted then
    begin
      J := 0;
      repeat
        F := V[J];
        I := J + 1;
        while (I < L) and (V[I] = F) do
          Inc(I);
        C := I - J;
        if C > 1 then
          begin
            DynArrayRemove(V, J + 1, C - 1);
            Dec(L, C - 1);
            Inc(J);
          end
        else
          J := I;
      until J >= L;
    end else
    begin
      J := 0;
      repeat
        repeat
          I := DynArrayPosNext(V[J], V, J);
          if I >= 0 then
            DynArrayRemove(V, I, 1);
        until I < 0;
        Inc(J);
      until J >= Length(V);
    end;
end;

procedure DynArrayRemoveDuplicates(var V: ExtendedArray; const IsSorted: Boolean);
var I, C, J, L : Integer;
    F          : Extended;
begin
  L := Length(V);
  if L = 0 then
    exit;

  if IsSorted then
    begin
      J := 0;
      repeat
        F := V[J];
        I := J + 1;
        while (I < L) and (V[I] = F) do
          Inc(I);
        C := I - J;
        if C > 1 then
          begin
            DynArrayRemove(V, J + 1, C - 1);
            Dec(L, C - 1);
            Inc(J);
          end
        else
          J := I;
      until J >= L;
    end else
    begin
      J := 0;
      repeat
        repeat
          I := DynArrayPosNext(V[J], V, J);
          if I >= 0 then
            DynArrayRemove(V, I, 1);
        until I < 0;
        Inc(J);
      until J >= Length(V);
    end;
end;

{$IFDEF SupportAnsiString}
procedure DynArrayRemoveDuplicatesA(var V: AnsiStringArray; const IsSorted: Boolean);
var I, C, J, L : Integer;
    F          : AnsiString;
begin
  L := Length(V);
  if L = 0 then
    exit;

  if IsSorted then
    begin
      J := 0;
      repeat
        F := V[J];
        I := J + 1;
        while (I < L) and (V[I] = F) do
          Inc(I);
        C := I - J;
        if C > 1 then
          begin
            DynArrayRemoveA(V, J + 1, C - 1);
            Dec(L, C - 1);
            Inc(J);
          end
        else
          J := I;
      until J >= L;
    end else
    begin
      J := 0;
      repeat
        repeat
          I := DynArrayPosNextA(V[J], V, J);
          if I >= 0 then
            DynArrayRemoveA(V, I, 1);
        until I < 0;
        Inc(J);
      until J >= Length(V);
    end;
end;

{$ENDIF}
procedure DynArrayRemoveDuplicatesU(var V: UnicodeStringArray; const IsSorted: Boolean);
var I, C, J, L : Integer;
    F          : UnicodeString;
begin
  L := Length(V);
  if L = 0 then
    exit;

  if IsSorted then
    begin
      J := 0;
      repeat
        F := V[J];
        I := J + 1;
        while (I < L) and (V[I] = F) do
          Inc(I);
        C := I - J;
        if C > 1 then
          begin
            DynArrayRemoveU(V, J + 1, C - 1);
            Dec(L, C - 1);
            Inc(J);
          end
        else
          J := I;
      until J >= L;
    end else
    begin
      J := 0;
      repeat
        repeat
          I := DynArrayPosNextU(V[J], V, J);
          if I >= 0 then
            DynArrayRemoveU(V, I, 1);
        until I < 0;
        Inc(J);
      until J >= Length(V);
    end;
end;

procedure DynArrayRemoveDuplicates(var V: StringArray; const IsSorted: Boolean);
var I, C, J, L : Integer;
    F          : String;
begin
  L := Length(V);
  if L = 0 then
    exit;

  if IsSorted then
    begin
      J := 0;
      repeat
        F := V[J];
        I := J + 1;
        while (I < L) and (V[I] = F) do
          Inc(I);
        C := I - J;
        if C > 1 then
          begin
            DynArrayRemove(V, J + 1, C - 1);
            Dec(L, C - 1);
            Inc(J);
          end
        else
          J := I;
      until J >= L;
    end else
    begin
      J := 0;
      repeat
        repeat
          I := DynArrayPosNext(V[J], V, J);
          if I >= 0 then
            DynArrayRemove(V, I, 1);
        until I < 0;
        Inc(J);
      until J >= Length(V);
    end;
end;

procedure DynArrayRemoveDuplicates(var V: PointerArray; const IsSorted: Boolean);
var I, C, J, L : Integer;
    F          : Pointer;
begin
  L := Length(V);
  if L = 0 then
    exit;

  if IsSorted then
    begin
      J := 0;
      repeat
        F := V[J];
        I := J + 1;
        while (I < L) and (V[I] = F) do
          Inc(I);
        C := I - J;
        if C > 1 then
          begin
            DynArrayRemove(V, J + 1, C - 1);
            Dec(L, C - 1);
            Inc(J);
          end
        else
          J := I;
      until J >= L;
    end else
    begin
      J := 0;
      repeat
        repeat
          I := DynArrayPosNext(V[J], V, J);
          if I >= 0 then
            DynArrayRemove(V, I, 1);
        until I < 0;
        Inc(J);
      until J >= Length(V);
    end;
end;



procedure DynArrayTrimLeft(var S: ByteArray; const TrimList: array of Byte); overload;
var I, J : Integer;
    R    : Boolean;
begin
  I := 0;
  R := True;
  while R and (I < Length(S)) do
    begin
      R := False;
      for J := 0 to High(TrimList) do
        if S[I] = TrimList[J] then
          begin
            R := True;
            Inc(I);
            break;
          end;
    end;
  if I > 0 then
    DynArrayRemove(S, 0, I - 1);
end;

procedure DynArrayTrimLeft(var S: Word16Array; const TrimList: array of Word16); overload;
var I, J : Integer;
    R    : Boolean;
begin
  I := 0;
  R := True;
  while R and (I < Length(S)) do
    begin
      R := False;
      for J := 0 to High(TrimList) do
        if S[I] = TrimList[J] then
          begin
            R := True;
            Inc(I);
            break;
          end;
    end;
  if I > 0 then
    DynArrayRemove(S, 0, I - 1);
end;

procedure DynArrayTrimLeft(var S: LongWordArray; const TrimList: array of LongWord); overload;
var I, J : Integer;
    R    : Boolean;
begin
  I := 0;
  R := True;
  while R and (I < Length(S)) do
    begin
      R := False;
      for J := 0 to High(TrimList) do
        if S[I] = TrimList[J] then
          begin
            R := True;
            Inc(I);
            break;
          end;
    end;
  if I > 0 then
    DynArrayRemove(S, 0, I - 1);
end;

procedure DynArrayTrimLeft(var S: ShortIntArray; const TrimList: array of ShortInt); overload;
var I, J : Integer;
    R    : Boolean;
begin
  I := 0;
  R := True;
  while R and (I < Length(S)) do
    begin
      R := False;
      for J := 0 to High(TrimList) do
        if S[I] = TrimList[J] then
          begin
            R := True;
            Inc(I);
            break;
          end;
    end;
  if I > 0 then
    DynArrayRemove(S, 0, I - 1);
end;

procedure DynArrayTrimLeft(var S: SmallIntArray; const TrimList: array of SmallInt); overload;
var I, J : Integer;
    R    : Boolean;
begin
  I := 0;
  R := True;
  while R and (I < Length(S)) do
    begin
      R := False;
      for J := 0 to High(TrimList) do
        if S[I] = TrimList[J] then
          begin
            R := True;
            Inc(I);
            break;
          end;
    end;
  if I > 0 then
    DynArrayRemove(S, 0, I - 1);
end;

procedure DynArrayTrimLeft(var S: LongIntArray; const TrimList: array of LongInt); overload;
var I, J : Integer;
    R    : Boolean;
begin
  I := 0;
  R := True;
  while R and (I < Length(S)) do
    begin
      R := False;
      for J := 0 to High(TrimList) do
        if S[I] = TrimList[J] then
          begin
            R := True;
            Inc(I);
            break;
          end;
    end;
  if I > 0 then
    DynArrayRemove(S, 0, I - 1);
end;

procedure DynArrayTrimLeft(var S: Int64Array; const TrimList: array of Int64); overload;
var I, J : Integer;
    R    : Boolean;
begin
  I := 0;
  R := True;
  while R and (I < Length(S)) do
    begin
      R := False;
      for J := 0 to High(TrimList) do
        if S[I] = TrimList[J] then
          begin
            R := True;
            Inc(I);
            break;
          end;
    end;
  if I > 0 then
    DynArrayRemove(S, 0, I - 1);
end;

procedure DynArrayTrimLeft(var S: SingleArray; const TrimList: array of Single); overload;
var I, J : Integer;
    R    : Boolean;
begin
  I := 0;
  R := True;
  while R and (I < Length(S)) do
    begin
      R := False;
      for J := 0 to High(TrimList) do
        if S[I] = TrimList[J] then
          begin
            R := True;
            Inc(I);
            break;
          end;
    end;
  if I > 0 then
    DynArrayRemove(S, 0, I - 1);
end;

procedure DynArrayTrimLeft(var S: DoubleArray; const TrimList: array of Double); overload;
var I, J : Integer;
    R    : Boolean;
begin
  I := 0;
  R := True;
  while R and (I < Length(S)) do
    begin
      R := False;
      for J := 0 to High(TrimList) do
        if S[I] = TrimList[J] then
          begin
            R := True;
            Inc(I);
            break;
          end;
    end;
  if I > 0 then
    DynArrayRemove(S, 0, I - 1);
end;

procedure DynArrayTrimLeft(var S: ExtendedArray; const TrimList: array of Extended); overload;
var I, J : Integer;
    R    : Boolean;
begin
  I := 0;
  R := True;
  while R and (I < Length(S)) do
    begin
      R := False;
      for J := 0 to High(TrimList) do
        if S[I] = TrimList[J] then
          begin
            R := True;
            Inc(I);
            break;
          end;
    end;
  if I > 0 then
    DynArrayRemove(S, 0, I - 1);
end;

{$IFDEF SupportAnsiString}
procedure DynArrayTrimLeftA(var S: AnsiStringArray; const TrimList: array of AnsiString); overload;
var I, J : Integer;
    R    : Boolean;
begin
  I := 0;
  R := True;
  while R and (I < Length(S)) do
    begin
      R := False;
      for J := 0 to High(TrimList) do
        if S[I] = TrimList[J] then
          begin
            R := True;
            Inc(I);
            break;
          end;
    end;
  if I > 0 then
    DynArrayRemoveA(S, 0, I - 1);
end;

{$ENDIF}
procedure DynArrayTrimLeftU(var S: UnicodeStringArray; const TrimList: array of UnicodeString); overload;
var I, J : Integer;
    R    : Boolean;
begin
  I := 0;
  R := True;
  while R and (I < Length(S)) do
    begin
      R := False;
      for J := 0 to High(TrimList) do
        if S[I] = TrimList[J] then
          begin
            R := True;
            Inc(I);
            break;
          end;
    end;
  if I > 0 then
    DynArrayRemoveU(S, 0, I - 1);
end;

procedure DynArrayTrimLeft(var S: StringArray; const TrimList: array of String); overload;
var I, J : Integer;
    R    : Boolean;
begin
  I := 0;
  R := True;
  while R and (I < Length(S)) do
    begin
      R := False;
      for J := 0 to High(TrimList) do
        if S[I] = TrimList[J] then
          begin
            R := True;
            Inc(I);
            break;
          end;
    end;
  if I > 0 then
    DynArrayRemove(S, 0, I - 1);
end;

procedure DynArrayTrimLeft(var S: PointerArray; const TrimList: array of Pointer); overload;
var I, J : Integer;
    R    : Boolean;
begin
  I := 0;
  R := True;
  while R and (I < Length(S)) do
    begin
      R := False;
      for J := 0 to High(TrimList) do
        if S[I] = TrimList[J] then
          begin
            R := True;
            Inc(I);
            break;
          end;
    end;
  if I > 0 then
    DynArrayRemove(S, 0, I - 1);
end;


procedure DynArrayTrimRight(var S: ByteArray; const TrimList: array of Byte); overload;
var I, J : Integer;
    R    : Boolean;
begin
  I := Length(S) - 1;
  R := True;
  while R and (I >= 0) do
    begin
      R := False;
      for J := 0 to High(TrimList) do
        if S[I] = TrimList[J] then
          begin
            R := True;
            Dec(I);
            break;
          end;
    end;
  if I < Length(S) - 1 then
    SetLength(S, I + 1);
end;

procedure DynArrayTrimRight(var S: Word16Array; const TrimList: array of Word16); overload;
var I, J : Integer;
    R    : Boolean;
begin
  I := Length(S) - 1;
  R := True;
  while R and (I >= 0) do
    begin
      R := False;
      for J := 0 to High(TrimList) do
        if S[I] = TrimList[J] then
          begin
            R := True;
            Dec(I);
            break;
          end;
    end;
  if I < Length(S) - 1 then
    SetLength(S, I + 1);
end;

procedure DynArrayTrimRight(var S: LongWordArray; const TrimList: array of LongWord); overload;
var I, J : Integer;
    R    : Boolean;
begin
  I := Length(S) - 1;
  R := True;
  while R and (I >= 0) do
    begin
      R := False;
      for J := 0 to High(TrimList) do
        if S[I] = TrimList[J] then
          begin
            R := True;
            Dec(I);
            break;
          end;
    end;
  if I < Length(S) - 1 then
    SetLength(S, I + 1);
end;

procedure DynArrayTrimRight(var S: ShortIntArray; const TrimList: array of ShortInt); overload;
var I, J : Integer;
    R    : Boolean;
begin
  I := Length(S) - 1;
  R := True;
  while R and (I >= 0) do
    begin
      R := False;
      for J := 0 to High(TrimList) do
        if S[I] = TrimList[J] then
          begin
            R := True;
            Dec(I);
            break;
          end;
    end;
  if I < Length(S) - 1 then
    SetLength(S, I + 1);
end;

procedure DynArrayTrimRight(var S: SmallIntArray; const TrimList: array of SmallInt); overload;
var I, J : Integer;
    R    : Boolean;
begin
  I := Length(S) - 1;
  R := True;
  while R and (I >= 0) do
    begin
      R := False;
      for J := 0 to High(TrimList) do
        if S[I] = TrimList[J] then
          begin
            R := True;
            Dec(I);
            break;
          end;
    end;
  if I < Length(S) - 1 then
    SetLength(S, I + 1);
end;

procedure DynArrayTrimRight(var S: LongIntArray; const TrimList: array of LongInt); overload;
var I, J : Integer;
    R    : Boolean;
begin
  I := Length(S) - 1;
  R := True;
  while R and (I >= 0) do
    begin
      R := False;
      for J := 0 to High(TrimList) do
        if S[I] = TrimList[J] then
          begin
            R := True;
            Dec(I);
            break;
          end;
    end;
  if I < Length(S) - 1 then
    SetLength(S, I + 1);
end;

procedure DynArrayTrimRight(var S: Int64Array; const TrimList: array of Int64); overload;
var I, J : Integer;
    R    : Boolean;
begin
  I := Length(S) - 1;
  R := True;
  while R and (I >= 0) do
    begin
      R := False;
      for J := 0 to High(TrimList) do
        if S[I] = TrimList[J] then
          begin
            R := True;
            Dec(I);
            break;
          end;
    end;
  if I < Length(S) - 1 then
    SetLength(S, I + 1);
end;

procedure DynArrayTrimRight(var S: SingleArray; const TrimList: array of Single); overload;
var I, J : Integer;
    R    : Boolean;
begin
  I := Length(S) - 1;
  R := True;
  while R and (I >= 0) do
    begin
      R := False;
      for J := 0 to High(TrimList) do
        if S[I] = TrimList[J] then
          begin
            R := True;
            Dec(I);
            break;
          end;
    end;
  if I < Length(S) - 1 then
    SetLength(S, I + 1);
end;

procedure DynArrayTrimRight(var S: DoubleArray; const TrimList: array of Double); overload;
var I, J : Integer;
    R    : Boolean;
begin
  I := Length(S) - 1;
  R := True;
  while R and (I >= 0) do
    begin
      R := False;
      for J := 0 to High(TrimList) do
        if S[I] = TrimList[J] then
          begin
            R := True;
            Dec(I);
            break;
          end;
    end;
  if I < Length(S) - 1 then
    SetLength(S, I + 1);
end;

procedure DynArrayTrimRight(var S: ExtendedArray; const TrimList: array of Extended); overload;
var I, J : Integer;
    R    : Boolean;
begin
  I := Length(S) - 1;
  R := True;
  while R and (I >= 0) do
    begin
      R := False;
      for J := 0 to High(TrimList) do
        if S[I] = TrimList[J] then
          begin
            R := True;
            Dec(I);
            break;
          end;
    end;
  if I < Length(S) - 1 then
    SetLength(S, I + 1);
end;

{$IFDEF SupportAnsiString}
procedure DynArrayTrimRightA(var S: AnsiStringArray; const TrimList: array of AnsiString); overload;
var I, J : Integer;
    R    : Boolean;
begin
  I := Length(S) - 1;
  R := True;
  while R and (I >= 0) do
    begin
      R := False;
      for J := 0 to High(TrimList) do
        if S[I] = TrimList[J] then
          begin
            R := True;
            Dec(I);
            break;
          end;
    end;
  if I < Length(S) - 1 then
    SetLength(S, I + 1);
end;

{$ENDIF}
procedure DynArrayTrimRightU(var S: UnicodeStringArray; const TrimList: array of UnicodeString); overload;
var I, J : Integer;
    R    : Boolean;
begin
  I := Length(S) - 1;
  R := True;
  while R and (I >= 0) do
    begin
      R := False;
      for J := 0 to High(TrimList) do
        if S[I] = TrimList[J] then
          begin
            R := True;
            Dec(I);
            break;
          end;
    end;
  if I < Length(S) - 1 then
    SetLength(S, I + 1);
end;

procedure DynArrayTrimRight(var S: StringArray; const TrimList: array of String); overload;
var I, J : Integer;
    R    : Boolean;
begin
  I := Length(S) - 1;
  R := True;
  while R and (I >= 0) do
    begin
      R := False;
      for J := 0 to High(TrimList) do
        if S[I] = TrimList[J] then
          begin
            R := True;
            Dec(I);
            break;
          end;
    end;
  if I < Length(S) - 1 then
    SetLength(S, I + 1);
end;

procedure DynArrayTrimRight(var S: PointerArray; const TrimList: array of Pointer); overload;
var I, J : Integer;
    R    : Boolean;
begin
  I := Length(S) - 1;
  R := True;
  while R and (I >= 0) do
    begin
      R := False;
      for J := 0 to High(TrimList) do
        if S[I] = TrimList[J] then
          begin
            R := True;
            Dec(I);
            break;
          end;
    end;
  if I < Length(S) - 1 then
    SetLength(S, I + 1);
end;


{                                                                              }
{ DynArrayInsert                                                               }
{                                                                              }
function DynArrayInsert(var V: ByteArray; const Idx: Integer; const Count: Integer): Integer;
var I, L : Integer;
    P    : Pointer;
begin
  L := Length(V);
  if (Idx > L) or (Idx + Count <= 0) or (Count <= 0) then
    begin
      Result := -1;
      exit;
    end;
  SetLength(V, L + Count);
  I := Idx;
  if I < 0 then
    I := 0;
  P := @V[I];
  if I < L then
    Move(P^, V[I + Count], (L - I) * Sizeof(Byte));
  FillChar(P^, Count * Sizeof(Byte), #0);
  Result := I;
end;

function DynArrayInsert(var V: Word16Array; const Idx: Integer; const Count: Integer): Integer;
var I, L : Integer;
    P    : Pointer;
begin
  L := Length(V);
  if (Idx > L) or (Idx + Count <= 0) or (Count <= 0) then
    begin
      Result := -1;
      exit;
    end;
  SetLength(V, L + Count);
  I := Idx;
  if I < 0 then
    I := 0;
  P := @V[I];
  if I < L then
    Move(P^, V[I + Count], (L - I) * Sizeof(Word16));
  FillChar(P^, Count * Sizeof(Word16), #0);
  Result := I;
end;

function DynArrayInsert(var V: Word32Array; const Idx: Integer; const Count: Integer): Integer;
var I, L : Integer;
    P    : Pointer;
begin
  L := Length(V);
  if (Idx > L) or (Idx + Count <= 0) or (Count <= 0) then
    begin
      Result := -1;
      exit;
    end;
  SetLength(V, L + Count);
  I := Idx;
  if I < 0 then
    I := 0;
  P := @V[I];
  if I < L then
    Move(P^, V[I + Count], (L - I) * Sizeof(Word32));
  FillChar(P^, Count * Sizeof(Word32), #0);
  Result := I;
end;

function DynArrayInsert(var V: Word64Array; const Idx: Integer; const Count: Integer): Integer;
var I, L : Integer;
    P    : Pointer;
begin
  L := Length(V);
  if (Idx > L) or (Idx + Count <= 0) or (Count <= 0) then
    begin
      Result := -1;
      exit;
    end;
  SetLength(V, L + Count);
  I := Idx;
  if I < 0 then
    I := 0;
  P := @V[I];
  if I < L then
    Move(P^, V[I + Count], (L - I) * Sizeof(Word64));
  FillChar(P^, Count * Sizeof(Word64), #0);
  Result := I;
end;

function DynArrayInsert(var V: LongWordArray; const Idx: Integer; const Count: Integer): Integer;
var I, L : Integer;
    P    : Pointer;
begin
  L := Length(V);
  if (Idx > L) or (Idx + Count <= 0) or (Count <= 0) then
    begin
      Result := -1;
      exit;
    end;
  SetLength(V, L + Count);
  I := Idx;
  if I < 0 then
    I := 0;
  P := @V[I];
  if I < L then
    Move(P^, V[I + Count], (L - I) * Sizeof(LongWord));
  FillChar(P^, Count * Sizeof(LongWord), #0);
  Result := I;
end;

function DynArrayInsert(var V: NativeUIntArray; const Idx: Integer; const Count: Integer): Integer;
var I, L : Integer;
    P    : Pointer;
begin
  L := Length(V);
  if (Idx > L) or (Idx + Count <= 0) or (Count <= 0) then
    begin
      Result := -1;
      exit;
    end;
  SetLength(V, L + Count);
  I := Idx;
  if I < 0 then
    I := 0;
  P := @V[I];
  if I < L then
    Move(P^, V[I + Count], (L - I) * Sizeof(NativeUInt));
  FillChar(P^, Count * Sizeof(NativeUInt), #0);
  Result := I;
end;

function DynArrayInsert(var V: ShortIntArray; const Idx: Integer; const Count: Integer): Integer;
var I, L : Integer;
    P    : Pointer;
begin
  L := Length(V);
  if (Idx > L) or (Idx + Count <= 0) or (Count <= 0) then
    begin
      Result := -1;
      exit;
    end;
  SetLength(V, L + Count);
  I := Idx;
  if I < 0 then
    I := 0;
  P := @V[I];
  if I < L then
    Move(P^, V[I + Count], (L - I) * Sizeof(ShortInt));
  FillChar(P^, Count * Sizeof(ShortInt), #0);
  Result := I;
end;

function DynArrayInsert(var V: SmallIntArray; const Idx: Integer; const Count: Integer): Integer;
var I, L : Integer;
    P    : Pointer;
begin
  L := Length(V);
  if (Idx > L) or (Idx + Count <= 0) or (Count <= 0) then
    begin
      Result := -1;
      exit;
    end;
  SetLength(V, L + Count);
  I := Idx;
  if I < 0 then
    I := 0;
  P := @V[I];
  if I < L then
    Move(P^, V[I + Count], (L - I) * Sizeof(SmallInt));
  FillChar(P^, Count * Sizeof(SmallInt), #0);
  Result := I;
end;

function DynArrayInsert(var V: LongIntArray; const Idx: Integer; const Count: Integer): Integer;
var I, L : Integer;
    P    : Pointer;
begin
  L := Length(V);
  if (Idx > L) or (Idx + Count <= 0) or (Count <= 0) then
    begin
      Result := -1;
      exit;
    end;
  SetLength(V, L + Count);
  I := Idx;
  if I < 0 then
    I := 0;
  P := @V[I];
  if I < L then
    Move(P^, V[I + Count], (L - I) * Sizeof(LongInt));
  FillChar(P^, Count * Sizeof(LongInt), #0);
  Result := I;
end;

function DynArrayInsert(var V: Int32Array; const Idx: Integer; const Count: Integer): Integer;
var I, L : Integer;
    P    : Pointer;
begin
  L := Length(V);
  if (Idx > L) or (Idx + Count <= 0) or (Count <= 0) then
    begin
      Result := -1;
      exit;
    end;
  SetLength(V, L + Count);
  I := Idx;
  if I < 0 then
    I := 0;
  P := @V[I];
  if I < L then
    Move(P^, V[I + Count], (L - I) * Sizeof(Int32));
  FillChar(P^, Count * Sizeof(Int32), #0);
  Result := I;
end;

function DynArrayInsert(var V: Int64Array; const Idx: Integer; const Count: Integer): Integer;
var I, L : Integer;
    P    : Pointer;
begin
  L := Length(V);
  if (Idx > L) or (Idx + Count <= 0) or (Count <= 0) then
    begin
      Result := -1;
      exit;
    end;
  SetLength(V, L + Count);
  I := Idx;
  if I < 0 then
    I := 0;
  P := @V[I];
  if I < L then
    Move(P^, V[I + Count], (L - I) * Sizeof(Int64));
  FillChar(P^, Count * Sizeof(Int64), #0);
  Result := I;
end;

function DynArrayInsert(var V: NativeIntArray; const Idx: Integer; const Count: Integer): Integer;
var I, L : Integer;
    P    : Pointer;
begin
  L := Length(V);
  if (Idx > L) or (Idx + Count <= 0) or (Count <= 0) then
    begin
      Result := -1;
      exit;
    end;
  SetLength(V, L + Count);
  I := Idx;
  if I < 0 then
    I := 0;
  P := @V[I];
  if I < L then
    Move(P^, V[I + Count], (L - I) * Sizeof(NativeInt));
  FillChar(P^, Count * Sizeof(NativeInt), #0);
  Result := I;
end;

function DynArrayInsert(var V: SingleArray; const Idx: Integer; const Count: Integer): Integer;
var I, L : Integer;
    P    : Pointer;
begin
  L := Length(V);
  if (Idx > L) or (Idx + Count <= 0) or (Count <= 0) then
    begin
      Result := -1;
      exit;
    end;
  SetLength(V, L + Count);
  I := Idx;
  if I < 0 then
    I := 0;
  P := @V[I];
  if I < L then
    Move(P^, V[I + Count], (L - I) * Sizeof(Single));
  FillChar(P^, Count * Sizeof(Single), #0);
  Result := I;
end;

function DynArrayInsert(var V: DoubleArray; const Idx: Integer; const Count: Integer): Integer;
var I, L : Integer;
    P    : Pointer;
begin
  L := Length(V);
  if (Idx > L) or (Idx + Count <= 0) or (Count <= 0) then
    begin
      Result := -1;
      exit;
    end;
  SetLength(V, L + Count);
  I := Idx;
  if I < 0 then
    I := 0;
  P := @V[I];
  if I < L then
    Move(P^, V[I + Count], (L - I) * Sizeof(Double));
  FillChar(P^, Count * Sizeof(Double), #0);
  Result := I;
end;

function DynArrayInsert(var V: ExtendedArray; const Idx: Integer; const Count: Integer): Integer;
var I, L : Integer;
    P    : Pointer;
begin
  L := Length(V);
  if (Idx > L) or (Idx + Count <= 0) or (Count <= 0) then
    begin
      Result := -1;
      exit;
    end;
  SetLength(V, L + Count);
  I := Idx;
  if I < 0 then
    I := 0;
  P := @V[I];
  if I < L then
    Move(P^, V[I + Count], (L - I) * Sizeof(Extended));
  FillChar(P^, Count * Sizeof(Extended), #0);
  Result := I;
end;

function DynArrayInsert(var V: CurrencyArray; const Idx: Integer; const Count: Integer): Integer;
var I, L : Integer;
    P    : Pointer;
begin
  L := Length(V);
  if (Idx > L) or (Idx + Count <= 0) or (Count <= 0) then
    begin
      Result := -1;
      exit;
    end;
  SetLength(V, L + Count);
  I := Idx;
  if I < 0 then
    I := 0;
  P := @V[I];
  if I < L then
    Move(P^, V[I + Count], (L - I) * Sizeof(Currency));
  FillChar(P^, Count * Sizeof(Currency), #0);
  Result := I;
end;

{$IFDEF SupportAnsiString}
function DynArrayInsertA(var V: AnsiStringArray; const Idx: Integer; const Count: Integer): Integer;
var I, L : Integer;
    P    : Pointer;
begin
  L := Length(V);
  if (Idx > L) or (Idx + Count <= 0) or (Count <= 0) then
    begin
      Result := -1;
      exit;
    end;
  SetLength(V, L + Count);
  I := Idx;
  if I < 0 then
    I := 0;
  P := @V[I];
  if I < L then
    Move(P^, V[I + Count], (L - I) * Sizeof(AnsiString));
  FillChar(P^, Count * Sizeof(AnsiString), #0);
  Result := I;
end;

{$ENDIF}
function DynArrayInsertB(var V: RawByteStringArray; const Idx: Integer; const Count: Integer): Integer;
var I, L : Integer;
    P    : Pointer;
begin
  L := Length(V);
  if (Idx > L) or (Idx + Count <= 0) or (Count <= 0) then
    begin
      Result := -1;
      exit;
    end;
  SetLength(V, L + Count);
  I := Idx;
  if I < 0 then
    I := 0;
  P := @V[I];
  if I < L then
    Move(P^, V[I + Count], (L - I) * Sizeof(RawByteString));
  FillChar(P^, Count * Sizeof(RawByteString), #0);
  Result := I;
end;

function DynArrayInsertU(var V: UnicodeStringArray; const Idx: Integer; const Count: Integer): Integer;
var I, L : Integer;
    P    : Pointer;
begin
  L := Length(V);
  if (Idx > L) or (Idx + Count <= 0) or (Count <= 0) then
    begin
      Result := -1;
      exit;
    end;
  SetLength(V, L + Count);
  I := Idx;
  if I < 0 then
    I := 0;
  P := @V[I];
  if I < L then
    Move(P^, V[I + Count], (L - I) * Sizeof(UnicodeString));
  FillChar(P^, Count * Sizeof(UnicodeString), #0);
  Result := I;
end;

function DynArrayInsert(var V: StringArray; const Idx: Integer; const Count: Integer): Integer;
var I, L : Integer;
    P    : Pointer;
begin
  L := Length(V);
  if (Idx > L) or (Idx + Count <= 0) or (Count <= 0) then
    begin
      Result := -1;
      exit;
    end;
  SetLength(V, L + Count);
  I := Idx;
  if I < 0 then
    I := 0;
  P := @V[I];
  if I < L then
    Move(P^, V[I + Count], (L - I) * Sizeof(String));
  FillChar(P^, Count * Sizeof(String), #0);
  Result := I;
end;

function DynArrayInsert(var V: PointerArray; const Idx: Integer; const Count: Integer): Integer;
var I, L : Integer;
    P    : Pointer;
begin
  L := Length(V);
  if (Idx > L) or (Idx + Count <= 0) or (Count <= 0) then
    begin
      Result := -1;
      exit;
    end;
  SetLength(V, L + Count);
  I := Idx;
  if I < 0 then
    I := 0;
  P := @V[I];
  if I < L then
    Move(P^, V[I + Count], (L - I) * Sizeof(Pointer));
  FillChar(P^, Count * Sizeof(Pointer), #0);
  Result := I;
end;

function DynArrayInsert(var V: ObjectArray; const Idx: Integer; const Count: Integer): Integer;
var I, L : Integer;
    P    : Pointer;
begin
  L := Length(V);
  if (Idx > L) or (Idx + Count <= 0) or (Count <= 0) then
    begin
      Result := -1;
      exit;
    end;
  SetLength(V, L + Count);
  I := Idx;
  if I < 0 then
    I := 0;
  P := @V[I];
  if I < L then
    Move(P^, V[I + Count], (L - I) * Sizeof(Pointer));
  FillChar(P^, Count * Sizeof(Pointer), #0);
  Result := I;
end;

function DynArrayInsert(var V: InterfaceArray; const Idx: Integer; const Count: Integer): Integer;
var I, L : Integer;
    P    : Pointer;
begin
  L := Length(V);
  if (Idx > L) or (Idx + Count <= 0) or (Count <= 0) then
    begin
      Result := -1;
      exit;
    end;
  SetLength(V, L + Count);
  I := Idx;
  if I < 0 then
    I := 0;
  P := @V[I];
  if I < L then
    Move(P^, V[I + Count], (L - I) * Sizeof(IInterface));
  FillChar(P^, Count * Sizeof(IInterface), #0);
  Result := I;
end;



{                                                                              }
{ DynArrayPosNext                                                              }
{   PosNext finds the next occurance of Find in V, -1 if it was not found.     }
{     Searches from Item[PrevPos + 1], ie PrevPos = -1 to find first           }
{     occurance.                                                               }
{                                                                              }
function DynArrayPosNext(const Find: Byte; const V: ByteArray; const PrevPos: Integer;
    const IsSortedAscending: Boolean): Integer;
var I, L, H : Integer;
    D       : Byte;
begin
  if IsSortedAscending then // binary search
    begin
      if MaxInt(PrevPos + 1, 0) = 0 then // find first
        begin
          L := 0;
          H := Length(V) - 1;
          while L <= H do
            begin
              I := (L + H) div 2;
              D := V[I];
              if Find = D then
                begin
                  while (I > 0) and (V[I - 1] = Find) do
                    Dec(I);
                  Result := I;
                  exit;
                end else
              if D > Find then
                H := I - 1
              else
                L := I + 1;
            end;
          Result := -1;
        end
      else // find next
        if PrevPos >= Length(V) - 1 then
          Result := -1
        else
          if V[PrevPos + 1] = Find then
            Result := PrevPos + 1
          else
            Result := -1;
    end
  else
    begin // linear search
      for I := MaxInt(PrevPos + 1, 0) to Length(V) - 1 do
        if V[I] = Find then
          begin
            Result := I;
            exit;
          end;
      Result := -1;
    end;
end;

function DynArrayPosNext(const Find: Word16; const V: Word16Array; const PrevPos: Integer;
    const IsSortedAscending: Boolean): Integer;
var I, L, H : Integer;
    D       : Word16;
begin
  if IsSortedAscending then // binary search
    begin
      if MaxInt(PrevPos + 1, 0) = 0 then // find first
        begin
          L := 0;
          H := Length(V) - 1;
          while L <= H do
            begin
              I := (L + H) div 2;
              D := V[I];
              if Find = D then
                begin
                  while (I > 0) and (V[I - 1] = Find) do
                    Dec(I);
                  Result := I;
                  exit;
                end else
              if D > Find then
                H := I - 1
              else
                L := I + 1;
            end;
          Result := -1;
        end
      else // find next
        if PrevPos >= Length(V) - 1 then
          Result := -1
        else
          if V[PrevPos + 1] = Find then
            Result := PrevPos + 1
          else
            Result := -1;
    end
  else
    begin // linear search
      for I := MaxInt(PrevPos + 1, 0) to Length(V) - 1 do
        if V[I] = Find then
          begin
            Result := I;
            exit;
          end;
      Result := -1;
    end;
end;

function DynArrayPosNext(const Find: Word32; const V: Word32Array; const PrevPos: Integer;
    const IsSortedAscending: Boolean): Integer;
var I, L, H : Integer;
    D       : Word32;
begin
  if IsSortedAscending then // binary search
    begin
      if MaxInt(PrevPos + 1, 0) = 0 then // find first
        begin
          L := 0;
          H := Length(V) - 1;
          while L <= H do
            begin
              I := (L + H) div 2;
              D := V[I];
              if Find = D then
                begin
                  while (I > 0) and (V[I - 1] = Find) do
                    Dec(I);
                  Result := I;
                  exit;
                end else
              if D > Find then
                H := I - 1
              else
                L := I + 1;
            end;
          Result := -1;
        end
      else // find next
        if PrevPos >= Length(V) - 1 then
          Result := -1
        else
          if V[PrevPos + 1] = Find then
            Result := PrevPos + 1
          else
            Result := -1;
    end
  else
    begin // linear search
      for I := MaxInt(PrevPos + 1, 0) to Length(V) - 1 do
        if V[I] = Find then
          begin
            Result := I;
            exit;
          end;
      Result := -1;
    end;
end;

function DynArrayPosNext(const Find: Word64; const V: Word64Array; const PrevPos: Integer;
    const IsSortedAscending: Boolean): Integer;
var I, L, H : Integer;
    D       : Word64;
begin
  if IsSortedAscending then // binary search
    begin
      if MaxInt(PrevPos + 1, 0) = 0 then // find first
        begin
          L := 0;
          H := Length(V) - 1;
          while L <= H do
            begin
              I := (L + H) div 2;
              D := V[I];
              if Find = D then
                begin
                  while (I > 0) and (V[I - 1] = Find) do
                    Dec(I);
                  Result := I;
                  exit;
                end else
              if D > Find then
                H := I - 1
              else
                L := I + 1;
            end;
          Result := -1;
        end
      else // find next
        if PrevPos >= Length(V) - 1 then
          Result := -1
        else
          if V[PrevPos + 1] = Find then
            Result := PrevPos + 1
          else
            Result := -1;
    end
  else
    begin // linear search
      for I := MaxInt(PrevPos + 1, 0) to Length(V) - 1 do
        if V[I] = Find then
          begin
            Result := I;
            exit;
          end;
      Result := -1;
    end;
end;

function DynArrayPosNext(const Find: LongWord; const V: LongWordArray; const PrevPos: Integer;
    const IsSortedAscending: Boolean): Integer;
var I, L, H : Integer;
    D       : LongWord;
begin
  if IsSortedAscending then // binary search
    begin
      if MaxInt(PrevPos + 1, 0) = 0 then // find first
        begin
          L := 0;
          H := Length(V) - 1;
          while L <= H do
            begin
              I := (L + H) div 2;
              D := V[I];
              if Find = D then
                begin
                  while (I > 0) and (V[I - 1] = Find) do
                    Dec(I);
                  Result := I;
                  exit;
                end else
              if D > Find then
                H := I - 1
              else
                L := I + 1;
            end;
          Result := -1;
        end
      else // find next
        if PrevPos >= Length(V) - 1 then
          Result := -1
        else
          if V[PrevPos + 1] = Find then
            Result := PrevPos + 1
          else
            Result := -1;
    end
  else
    begin // linear search
      for I := MaxInt(PrevPos + 1, 0) to Length(V) - 1 do
        if V[I] = Find then
          begin
            Result := I;
            exit;
          end;
      Result := -1;
    end;
end;

function DynArrayPosNext(const Find: Cardinal; const V: CardinalArray; const PrevPos: Integer;
    const IsSortedAscending: Boolean): Integer;
var I, L, H : Integer;
    D       : Cardinal;
begin
  if IsSortedAscending then // binary search
    begin
      if MaxInt(PrevPos + 1, 0) = 0 then // find first
        begin
          L := 0;
          H := Length(V) - 1;
          while L <= H do
            begin
              I := (L + H) div 2;
              D := V[I];
              if Find = D then
                begin
                  while (I > 0) and (V[I - 1] = Find) do
                    Dec(I);
                  Result := I;
                  exit;
                end else
              if D > Find then
                H := I - 1
              else
                L := I + 1;
            end;
          Result := -1;
        end
      else // find next
        if PrevPos >= Length(V) - 1 then
          Result := -1
        else
          if V[PrevPos + 1] = Find then
            Result := PrevPos + 1
          else
            Result := -1;
    end
  else
    begin // linear search
      for I := MaxInt(PrevPos + 1, 0) to Length(V) - 1 do
        if V[I] = Find then
          begin
            Result := I;
            exit;
          end;
      Result := -1;
    end;
end;

function DynArrayPosNext(const Find: NativeUInt; const V: NativeUIntArray; const PrevPos: Integer;
    const IsSortedAscending: Boolean): Integer;
var I, L, H : Integer;
    D       : NativeUInt;
begin
  if IsSortedAscending then // binary search
    begin
      if MaxInt(PrevPos + 1, 0) = 0 then // find first
        begin
          L := 0;
          H := Length(V) - 1;
          while L <= H do
            begin
              I := (L + H) div 2;
              D := V[I];
              if Find = D then
                begin
                  while (I > 0) and (V[I - 1] = Find) do
                    Dec(I);
                  Result := I;
                  exit;
                end else
              if D > Find then
                H := I - 1
              else
                L := I + 1;
            end;
          Result := -1;
        end
      else // find next
        if PrevPos >= Length(V) - 1 then
          Result := -1
        else
          if V[PrevPos + 1] = Find then
            Result := PrevPos + 1
          else
            Result := -1;
    end
  else
    begin // linear search
      for I := MaxInt(PrevPos + 1, 0) to Length(V) - 1 do
        if V[I] = Find then
          begin
            Result := I;
            exit;
          end;
      Result := -1;
    end;
end;

function DynArrayPosNext(const Find: ShortInt; const V: ShortIntArray; const PrevPos: Integer;
    const IsSortedAscending: Boolean): Integer;
var I, L, H : Integer;
    D       : ShortInt;
begin
  if IsSortedAscending then // binary search
    begin
      if MaxInt(PrevPos + 1, 0) = 0 then // find first
        begin
          L := 0;
          H := Length(V) - 1;
          while L <= H do
            begin
              I := (L + H) div 2;
              D := V[I];
              if Find = D then
                begin
                  while (I > 0) and (V[I - 1] = Find) do
                    Dec(I);
                  Result := I;
                  exit;
                end else
              if D > Find then
                H := I - 1
              else
                L := I + 1;
            end;
          Result := -1;
        end
      else // find next
        if PrevPos >= Length(V) - 1 then
          Result := -1
        else
          if V[PrevPos + 1] = Find then
            Result := PrevPos + 1
          else
            Result := -1;
    end
  else
    begin // linear search
      for I := MaxInt(PrevPos + 1, 0) to Length(V) - 1 do
        if V[I] = Find then
          begin
            Result := I;
            exit;
          end;
      Result := -1;
    end;
end;

function DynArrayPosNext(const Find: SmallInt; const V: SmallIntArray; const PrevPos: Integer;
    const IsSortedAscending: Boolean): Integer;
var I, L, H : Integer;
    D       : SmallInt;
begin
  if IsSortedAscending then // binary search
    begin
      if MaxInt(PrevPos + 1, 0) = 0 then // find first
        begin
          L := 0;
          H := Length(V) - 1;
          while L <= H do
            begin
              I := (L + H) div 2;
              D := V[I];
              if Find = D then
                begin
                  while (I > 0) and (V[I - 1] = Find) do
                    Dec(I);
                  Result := I;
                  exit;
                end else
              if D > Find then
                H := I - 1
              else
                L := I + 1;
            end;
          Result := -1;
        end
      else // find next
        if PrevPos >= Length(V) - 1 then
          Result := -1
        else
          if V[PrevPos + 1] = Find then
            Result := PrevPos + 1
          else
            Result := -1;
    end
  else
    begin // linear search
      for I := MaxInt(PrevPos + 1, 0) to Length(V) - 1 do
        if V[I] = Find then
          begin
            Result := I;
            exit;
          end;
      Result := -1;
    end;
end;

function DynArrayPosNext(const Find: LongInt; const V: LongIntArray; const PrevPos: Integer;
    const IsSortedAscending: Boolean): Integer;
var I, L, H : Integer;
    D       : LongInt;
begin
  if IsSortedAscending then // binary search
    begin
      if MaxInt(PrevPos + 1, 0) = 0 then // find first
        begin
          L := 0;
          H := Length(V) - 1;
          while L <= H do
            begin
              I := (L + H) div 2;
              D := V[I];
              if Find = D then
                begin
                  while (I > 0) and (V[I - 1] = Find) do
                    Dec(I);
                  Result := I;
                  exit;
                end else
              if D > Find then
                H := I - 1
              else
                L := I + 1;
            end;
          Result := -1;
        end
      else // find next
        if PrevPos >= Length(V) - 1 then
          Result := -1
        else
          if V[PrevPos + 1] = Find then
            Result := PrevPos + 1
          else
            Result := -1;
    end
  else
    begin // linear search
      for I := MaxInt(PrevPos + 1, 0) to Length(V) - 1 do
        if V[I] = Find then
          begin
            Result := I;
            exit;
          end;
      Result := -1;
    end;
end;

function DynArrayPosNext(const Find: Integer; const V: IntegerArray; const PrevPos: Integer;
    const IsSortedAscending: Boolean): Integer;
var I, L, H : Integer;
    D       : Integer;
begin
  if IsSortedAscending then // binary search
    begin
      if MaxInt(PrevPos + 1, 0) = 0 then // find first
        begin
          L := 0;
          H := Length(V) - 1;
          while L <= H do
            begin
              I := (L + H) div 2;
              D := V[I];
              if Find = D then
                begin
                  while (I > 0) and (V[I - 1] = Find) do
                    Dec(I);
                  Result := I;
                  exit;
                end else
              if D > Find then
                H := I - 1
              else
                L := I + 1;
            end;
          Result := -1;
        end
      else // find next
        if PrevPos >= Length(V) - 1 then
          Result := -1
        else
          if V[PrevPos + 1] = Find then
            Result := PrevPos + 1
          else
            Result := -1;
    end
  else
    begin // linear search
      for I := MaxInt(PrevPos + 1, 0) to Length(V) - 1 do
        if V[I] = Find then
          begin
            Result := I;
            exit;
          end;
      Result := -1;
    end;
end;

function DynArrayPosNext(const Find: Int32; const V: Int32Array; const PrevPos: Integer;
    const IsSortedAscending: Boolean): Integer;
var I, L, H : Integer;
    D       : Int32;
begin
  if IsSortedAscending then // binary search
    begin
      if MaxInt(PrevPos + 1, 0) = 0 then // find first
        begin
          L := 0;
          H := Length(V) - 1;
          while L <= H do
            begin
              I := (L + H) div 2;
              D := V[I];
              if Find = D then
                begin
                  while (I > 0) and (V[I - 1] = Find) do
                    Dec(I);
                  Result := I;
                  exit;
                end else
              if D > Find then
                H := I - 1
              else
                L := I + 1;
            end;
          Result := -1;
        end
      else // find next
        if PrevPos >= Length(V) - 1 then
          Result := -1
        else
          if V[PrevPos + 1] = Find then
            Result := PrevPos + 1
          else
            Result := -1;
    end
  else
    begin // linear search
      for I := MaxInt(PrevPos + 1, 0) to Length(V) - 1 do
        if V[I] = Find then
          begin
            Result := I;
            exit;
          end;
      Result := -1;
    end;
end;

function DynArrayPosNext(const Find: Int64; const V: Int64Array; const PrevPos: Integer;
    const IsSortedAscending: Boolean): Integer;
var I, L, H : Integer;
    D       : Int64;
begin
  if IsSortedAscending then // binary search
    begin
      if MaxInt(PrevPos + 1, 0) = 0 then // find first
        begin
          L := 0;
          H := Length(V) - 1;
          while L <= H do
            begin
              I := (L + H) div 2;
              D := V[I];
              if Find = D then
                begin
                  while (I > 0) and (V[I - 1] = Find) do
                    Dec(I);
                  Result := I;
                  exit;
                end else
              if D > Find then
                H := I - 1
              else
                L := I + 1;
            end;
          Result := -1;
        end
      else // find next
        if PrevPos >= Length(V) - 1 then
          Result := -1
        else
          if V[PrevPos + 1] = Find then
            Result := PrevPos + 1
          else
            Result := -1;
    end
  else
    begin // linear search
      for I := MaxInt(PrevPos + 1, 0) to Length(V) - 1 do
        if V[I] = Find then
          begin
            Result := I;
            exit;
          end;
      Result := -1;
    end;
end;

function DynArrayPosNext(const Find: NativeInt; const V: NativeIntArray; const PrevPos: Integer;
    const IsSortedAscending: Boolean): Integer;
var I, L, H : Integer;
    D       : NativeInt;
begin
  if IsSortedAscending then // binary search
    begin
      if MaxInt(PrevPos + 1, 0) = 0 then // find first
        begin
          L := 0;
          H := Length(V) - 1;
          while L <= H do
            begin
              I := (L + H) div 2;
              D := V[I];
              if Find = D then
                begin
                  while (I > 0) and (V[I - 1] = Find) do
                    Dec(I);
                  Result := I;
                  exit;
                end else
              if D > Find then
                H := I - 1
              else
                L := I + 1;
            end;
          Result := -1;
        end
      else // find next
        if PrevPos >= Length(V) - 1 then
          Result := -1
        else
          if V[PrevPos + 1] = Find then
            Result := PrevPos + 1
          else
            Result := -1;
    end
  else
    begin // linear search
      for I := MaxInt(PrevPos + 1, 0) to Length(V) - 1 do
        if V[I] = Find then
          begin
            Result := I;
            exit;
          end;
      Result := -1;
    end;
end;

function DynArrayPosNext(const Find: Single; const V: SingleArray; const PrevPos: Integer;
    const IsSortedAscending: Boolean): Integer;
var I, L, H : Integer;
    D       : Single;
begin
  if IsSortedAscending then // binary search
    begin
      if MaxInt(PrevPos + 1, 0) = 0 then // find first
        begin
          L := 0;
          H := Length(V) - 1;
          while L <= H do
            begin
              I := (L + H) div 2;
              D := V[I];
              if Find = D then
                begin
                  while (I > 0) and (V[I - 1] = Find) do
                    Dec(I);
                  Result := I;
                  exit;
                end else
              if D > Find then
                H := I - 1
              else
                L := I + 1;
            end;
          Result := -1;
        end
      else // find next
        if PrevPos >= Length(V) - 1 then
          Result := -1
        else
          if V[PrevPos + 1] = Find then
            Result := PrevPos + 1
          else
            Result := -1;
    end
  else
    begin // linear search
      for I := MaxInt(PrevPos + 1, 0) to Length(V) - 1 do
        if V[I] = Find then
          begin
            Result := I;
            exit;
          end;
      Result := -1;
    end;
end;

function DynArrayPosNext(const Find: Double; const V: DoubleArray; const PrevPos: Integer;
    const IsSortedAscending: Boolean): Integer;
var I, L, H : Integer;
    D       : Double;
begin
  if IsSortedAscending then // binary search
    begin
      if MaxInt(PrevPos + 1, 0) = 0 then // find first
        begin
          L := 0;
          H := Length(V) - 1;
          while L <= H do
            begin
              I := (L + H) div 2;
              D := V[I];
              if Find = D then
                begin
                  while (I > 0) and (V[I - 1] = Find) do
                    Dec(I);
                  Result := I;
                  exit;
                end else
              if D > Find then
                H := I - 1
              else
                L := I + 1;
            end;
          Result := -1;
        end
      else // find next
        if PrevPos >= Length(V) - 1 then
          Result := -1
        else
          if V[PrevPos + 1] = Find then
            Result := PrevPos + 1
          else
            Result := -1;
    end
  else
    begin // linear search
      for I := MaxInt(PrevPos + 1, 0) to Length(V) - 1 do
        if V[I] = Find then
          begin
            Result := I;
            exit;
          end;
      Result := -1;
    end;
end;

function DynArrayPosNext(const Find: Extended; const V: ExtendedArray; const PrevPos: Integer;
    const IsSortedAscending: Boolean): Integer;
var I, L, H : Integer;
    D       : Extended;
begin
  if IsSortedAscending then // binary search
    begin
      if MaxInt(PrevPos + 1, 0) = 0 then // find first
        begin
          L := 0;
          H := Length(V) - 1;
          while L <= H do
            begin
              I := (L + H) div 2;
              D := V[I];
              if Find = D then
                begin
                  while (I > 0) and (V[I - 1] = Find) do
                    Dec(I);
                  Result := I;
                  exit;
                end else
              if D > Find then
                H := I - 1
              else
                L := I + 1;
            end;
          Result := -1;
        end
      else // find next
        if PrevPos >= Length(V) - 1 then
          Result := -1
        else
          if V[PrevPos + 1] = Find then
            Result := PrevPos + 1
          else
            Result := -1;
    end
  else
    begin // linear search
      for I := MaxInt(PrevPos + 1, 0) to Length(V) - 1 do
        if V[I] = Find then
          begin
            Result := I;
            exit;
          end;
      Result := -1;
    end;
end;

function DynArrayPosNext(const Find: Boolean; const V: BooleanArray; const PrevPos: Integer;
    const IsSortedAscending: Boolean): Integer;
var I, L, H : Integer;
    D       : Boolean;
begin
  if IsSortedAscending then // binary search
    begin
      if MaxInt(PrevPos + 1, 0) = 0 then // find first
        begin
          L := 0;
          H := Length(V) - 1;
          while L <= H do
            begin
              I := (L + H) div 2;
              D := V[I];
              if Find = D then
                begin
                  while (I > 0) and (V[I - 1] = Find) do
                    Dec(I);
                  Result := I;
                  exit;
                end else
              if D > Find then
                H := I - 1
              else
                L := I + 1;
            end;
          Result := -1;
        end
      else // find next
        if PrevPos >= Length(V) - 1 then
          Result := -1
        else
          if V[PrevPos + 1] = Find then
            Result := PrevPos + 1
          else
            Result := -1;
    end
  else
    begin // linear search
      for I := MaxInt(PrevPos + 1, 0) to Length(V) - 1 do
        if V[I] = Find then
          begin
            Result := I;
            exit;
          end;
      Result := -1;
    end;
end;

{$IFDEF SupportAnsiString}
function DynArrayPosNextA(const Find: AnsiString; const V: AnsiStringArray; const PrevPos: Integer;
    const IsSortedAscending: Boolean): Integer;
var I, L, H : Integer;
    D       : AnsiString;
begin
  if IsSortedAscending then // binary search
    begin
      if MaxInt(PrevPos + 1, 0) = 0 then // find first
        begin
          L := 0;
          H := Length(V) - 1;
          while L <= H do
            begin
              I := (L + H) div 2;
              D := V[I];
              if Find = D then
                begin
                  while (I > 0) and (V[I - 1] = Find) do
                    Dec(I);
                  Result := I;
                  exit;
                end else
              if D > Find then
                H := I - 1
              else
                L := I + 1;
            end;
          Result := -1;
        end
      else // find next
        if PrevPos >= Length(V) - 1 then
          Result := -1
        else
          if V[PrevPos + 1] = Find then
            Result := PrevPos + 1
          else
            Result := -1;
    end
  else
    begin // linear search
      for I := MaxInt(PrevPos + 1, 0) to Length(V) - 1 do
        if V[I] = Find then
          begin
            Result := I;
            exit;
          end;
      Result := -1;
    end;
end;

{$ENDIF}
function DynArrayPosNextB(const Find: RawByteString; const V: RawByteStringArray; const PrevPos: Integer;
    const IsSortedAscending: Boolean): Integer;
var I, L, H : Integer;
    D       : RawByteString;
begin
  if IsSortedAscending then // binary search
    begin
      if MaxInt(PrevPos + 1, 0) = 0 then // find first
        begin
          L := 0;
          H := Length(V) - 1;
          while L <= H do
            begin
              I := (L + H) div 2;
              D := V[I];
              if Find = D then
                begin
                  while (I > 0) and (V[I - 1] = Find) do
                    Dec(I);
                  Result := I;
                  exit;
                end else
              if D > Find then
                H := I - 1
              else
                L := I + 1;
            end;
          Result := -1;
        end
      else // find next
        if PrevPos >= Length(V) - 1 then
          Result := -1
        else
          if V[PrevPos + 1] = Find then
            Result := PrevPos + 1
          else
            Result := -1;
    end
  else
    begin // linear search
      for I := MaxInt(PrevPos + 1, 0) to Length(V) - 1 do
        if V[I] = Find then
          begin
            Result := I;
            exit;
          end;
      Result := -1;
    end;
end;

function DynArrayPosNextU(const Find: UnicodeString; const V: UnicodeStringArray; const PrevPos: Integer;
    const IsSortedAscending: Boolean): Integer;
var I, L, H : Integer;
    D       : UnicodeString;
begin
  if IsSortedAscending then // binary search
    begin
      if MaxInt(PrevPos + 1, 0) = 0 then // find first
        begin
          L := 0;
          H := Length(V) - 1;
          while L <= H do
            begin
              I := (L + H) div 2;
              D := V[I];
              if Find = D then
                begin
                  while (I > 0) and (V[I - 1] = Find) do
                    Dec(I);
                  Result := I;
                  exit;
                end else
              if D > Find then
                H := I - 1
              else
                L := I + 1;
            end;
          Result := -1;
        end
      else // find next
        if PrevPos >= Length(V) - 1 then
          Result := -1
        else
          if V[PrevPos + 1] = Find then
            Result := PrevPos + 1
          else
            Result := -1;
    end
  else
    begin // linear search
      for I := MaxInt(PrevPos + 1, 0) to Length(V) - 1 do
        if V[I] = Find then
          begin
            Result := I;
            exit;
          end;
      Result := -1;
    end;
end;

function DynArrayPosNext(const Find: String; const V: StringArray; const PrevPos: Integer;
    const IsSortedAscending: Boolean): Integer;
var I, L, H : Integer;
    D       : String;
begin
  if IsSortedAscending then // binary search
    begin
      if MaxInt(PrevPos + 1, 0) = 0 then // find first
        begin
          L := 0;
          H := Length(V) - 1;
          while L <= H do
            begin
              I := (L + H) div 2;
              D := V[I];
              if Find = D then
                begin
                  while (I > 0) and (V[I - 1] = Find) do
                    Dec(I);
                  Result := I;
                  exit;
                end else
              if D > Find then
                H := I - 1
              else
                L := I + 1;
            end;
          Result := -1;
        end
      else // find next
        if PrevPos >= Length(V) - 1 then
          Result := -1
        else
          if V[PrevPos + 1] = Find then
            Result := PrevPos + 1
          else
            Result := -1;
    end
  else
    begin // linear search
      for I := MaxInt(PrevPos + 1, 0) to Length(V) - 1 do
        if V[I] = Find then
          begin
            Result := I;
            exit;
          end;
      Result := -1;
    end;
end;

function DynArrayPosNext(const Find: TObject; const V: ObjectArray; const PrevPos: Integer): Integer;
var I : Integer;
begin
  for I := MaxInt(PrevPos + 1, 0) to Length(V) - 1 do
    if V[I] = Find then
      begin
        Result := I;
        exit;
       end;
  Result := -1;
end;

function DynArrayPosNext(const ClassType: TClass; const V: ObjectArray; const PrevPos: Integer): Integer;
var I : Integer;
begin
  for I := MaxInt(PrevPos + 1, 0) to Length(V) - 1 do
    if V[I] is ClassType then
      begin
        Result := I;
        exit;
       end;
  Result := -1;
end;

function DynArrayPosNext(const ClassName: String; const V: ObjectArray; const PrevPos: Integer): Integer;
var I : Integer;
    T : TObject;
begin
  for I := MaxInt(PrevPos + 1, 0) to Length(V) - 1 do
    begin
      T := V[I];
      if Assigned(T) and (T.ClassName = ClassName) then
        begin
          Result := I;
          exit;
         end;
    end;
  Result := -1;
end;

function DynArrayPosNext(const Find: Pointer; const V: PointerArray; const PrevPos: Integer): Integer;
var I : Integer;
begin
  for I := MaxInt(PrevPos + 1, 0) to Length(V) - 1 do
    if V[I] = Find then
      begin
        Result := I;
        exit;
       end;
  Result := -1;
end;



{                                                                              }
{ DynArrayCount                                                                }
{                                                                              }
function DynArrayCount(const Find: Byte; const V: ByteArray; const IsSortedAscending: Boolean = False): Integer;
var I, J : Integer;
begin
  if IsSortedAscending then
    begin
      I := DynArrayPosNext(Find, V, -1, True);
      if I = -1 then
        Result := 0 else
        begin
          Result := 1;
          J := Length(V);
          while (I + Result < J) and (V[I + Result] = Find) do
            Inc(Result);
        end;
    end
  else
    begin
      J := -1;
      Result := 0;
      repeat
        I := DynArrayPosNext(Find, V, J, False);
        if I >= 0 then
          begin
            Inc(Result);
            J := I;
          end;
      until I < 0;
    end;
end;

function DynArrayCount(const Find: Word16; const V: Word16Array; const IsSortedAscending: Boolean = False): Integer;
var I, J : Integer;
begin
  if IsSortedAscending then
    begin
      I := DynArrayPosNext(Find, V, -1, True);
      if I = -1 then
        Result := 0 else
        begin
          Result := 1;
          J := Length(V);
          while (I + Result < J) and (V[I + Result] = Find) do
            Inc(Result);
        end;
    end
  else
    begin
      J := -1;
      Result := 0;
      repeat
        I := DynArrayPosNext(Find, V, J, False);
        if I >= 0 then
          begin
            Inc(Result);
            J := I;
          end;
      until I < 0;
    end;
end;

function DynArrayCount(const Find: LongWord; const V: LongWordArray; const IsSortedAscending: Boolean = False): Integer;
var I, J : Integer;
begin
  if IsSortedAscending then
    begin
      I := DynArrayPosNext(Find, V, -1, True);
      if I = -1 then
        Result := 0 else
        begin
          Result := 1;
          J := Length(V);
          while (I + Result < J) and (V[I + Result] = Find) do
            Inc(Result);
        end;
    end
  else
    begin
      J := -1;
      Result := 0;
      repeat
        I := DynArrayPosNext(Find, V, J, False);
        if I >= 0 then
          begin
            Inc(Result);
            J := I;
          end;
      until I < 0;
    end;
end;

function DynArrayCount(const Find: ShortInt; const V: ShortIntArray; const IsSortedAscending: Boolean = False): Integer;
var I, J : Integer;
begin
  if IsSortedAscending then
    begin
      I := DynArrayPosNext(Find, V, -1, True);
      if I = -1 then
        Result := 0 else
        begin
          Result := 1;
          J := Length(V);
          while (I + Result < J) and (V[I + Result] = Find) do
            Inc(Result);
        end;
    end
  else
    begin
      J := -1;
      Result := 0;
      repeat
        I := DynArrayPosNext(Find, V, J, False);
        if I >= 0 then
          begin
            Inc(Result);
            J := I;
          end;
      until I < 0;
    end;
end;

function DynArrayCount(const Find: SmallInt; const V: SmallIntArray; const IsSortedAscending: Boolean = False): Integer;
var I, J : Integer;
begin
  if IsSortedAscending then
    begin
      I := DynArrayPosNext(Find, V, -1, True);
      if I = -1 then
        Result := 0 else
        begin
          Result := 1;
          J := Length(V);
          while (I + Result < J) and (V[I + Result] = Find) do
            Inc(Result);
        end;
    end
  else
    begin
      J := -1;
      Result := 0;
      repeat
        I := DynArrayPosNext(Find, V, J, False);
        if I >= 0 then
          begin
            Inc(Result);
            J := I;
          end;
      until I < 0;
    end;
end;

function DynArrayCount(const Find: LongInt; const V: LongIntArray; const IsSortedAscending: Boolean = False): Integer;
var I, J : Integer;
begin
  if IsSortedAscending then
    begin
      I := DynArrayPosNext(Find, V, -1, True);
      if I = -1 then
        Result := 0 else
        begin
          Result := 1;
          J := Length(V);
          while (I + Result < J) and (V[I + Result] = Find) do
            Inc(Result);
        end;
    end
  else
    begin
      J := -1;
      Result := 0;
      repeat
        I := DynArrayPosNext(Find, V, J, False);
        if I >= 0 then
          begin
            Inc(Result);
            J := I;
          end;
      until I < 0;
    end;
end;

function DynArrayCount(const Find: Int64; const V: Int64Array; const IsSortedAscending: Boolean = False): Integer;
var I, J : Integer;
begin
  if IsSortedAscending then
    begin
      I := DynArrayPosNext(Find, V, -1, True);
      if I = -1 then
        Result := 0 else
        begin
          Result := 1;
          J := Length(V);
          while (I + Result < J) and (V[I + Result] = Find) do
            Inc(Result);
        end;
    end
  else
    begin
      J := -1;
      Result := 0;
      repeat
        I := DynArrayPosNext(Find, V, J, False);
        if I >= 0 then
          begin
            Inc(Result);
            J := I;
          end;
      until I < 0;
    end;
end;

function DynArrayCount(const Find: Single; const V: SingleArray; const IsSortedAscending: Boolean = False): Integer;
var I, J : Integer;
begin
  if IsSortedAscending then
    begin
      I := DynArrayPosNext(Find, V, -1, True);
      if I = -1 then
        Result := 0 else
        begin
          Result := 1;
          J := Length(V);
          while (I + Result < J) and (V[I + Result] = Find) do
            Inc(Result);
        end;
    end
  else
    begin
      J := -1;
      Result := 0;
      repeat
        I := DynArrayPosNext(Find, V, J, False);
        if I >= 0 then
          begin
            Inc(Result);
            J := I;
          end;
      until I < 0;
    end;
end;

function DynArrayCount(const Find: Double; const V: DoubleArray; const IsSortedAscending: Boolean = False): Integer;
var I, J : Integer;
begin
  if IsSortedAscending then
    begin
      I := DynArrayPosNext(Find, V, -1, True);
      if I = -1 then
        Result := 0 else
        begin
          Result := 1;
          J := Length(V);
          while (I + Result < J) and (V[I + Result] = Find) do
            Inc(Result);
        end;
    end
  else
    begin
      J := -1;
      Result := 0;
      repeat
        I := DynArrayPosNext(Find, V, J, False);
        if I >= 0 then
          begin
            Inc(Result);
            J := I;
          end;
      until I < 0;
    end;
end;

function DynArrayCount(const Find: Extended; const V: ExtendedArray; const IsSortedAscending: Boolean = False): Integer;
var I, J : Integer;
begin
  if IsSortedAscending then
    begin
      I := DynArrayPosNext(Find, V, -1, True);
      if I = -1 then
        Result := 0 else
        begin
          Result := 1;
          J := Length(V);
          while (I + Result < J) and (V[I + Result] = Find) do
            Inc(Result);
        end;
    end
  else
    begin
      J := -1;
      Result := 0;
      repeat
        I := DynArrayPosNext(Find, V, J, False);
        if I >= 0 then
          begin
            Inc(Result);
            J := I;
          end;
      until I < 0;
    end;
end;

{$IFDEF SupportAnsiString}
function DynArrayCountA(const Find: AnsiString; const V: AnsiStringArray; const IsSortedAscending: Boolean = False): Integer;
var I, J : Integer;
begin
  if IsSortedAscending then
    begin
      I := DynArrayPosNextA(Find, V, -1, True);
      if I = -1 then
        Result := 0 else
        begin
          Result := 1;
          J := Length(V);
          while (I + Result < J) and (V[I + Result] = Find) do
            Inc(Result);
        end;
    end
  else
    begin
      J := -1;
      Result := 0;
      repeat
        I := DynArrayPosNextA(Find, V, J, False);
        if I >= 0 then
          begin
            Inc(Result);
            J := I;
          end;
      until I < 0;
    end;
end;

{$ENDIF}
function DynArrayCountB(const Find: RawByteString; const V: RawByteStringArray; const IsSortedAscending: Boolean = False): Integer;
var I, J : Integer;
begin
  if IsSortedAscending then
    begin
      I := DynArrayPosNextB(Find, V, -1, True);
      if I = -1 then
        Result := 0 else
        begin
          Result := 1;
          J := Length(V);
          while (I + Result < J) and (V[I + Result] = Find) do
            Inc(Result);
        end;
    end
  else
    begin
      J := -1;
      Result := 0;
      repeat
        I := DynArrayPosNextB(Find, V, J, False);
        if I >= 0 then
          begin
            Inc(Result);
            J := I;
          end;
      until I < 0;
    end;
end;

function DynArrayCountU(const Find: UnicodeString; const V: UnicodeStringArray; const IsSortedAscending: Boolean = False): Integer;
var I, J : Integer;
begin
  if IsSortedAscending then
    begin
      I := DynArrayPosNextU(Find, V, -1, True);
      if I = -1 then
        Result := 0 else
        begin
          Result := 1;
          J := Length(V);
          while (I + Result < J) and (V[I + Result] = Find) do
            Inc(Result);
        end;
    end
  else
    begin
      J := -1;
      Result := 0;
      repeat
        I := DynArrayPosNextU(Find, V, J, False);
        if I >= 0 then
          begin
            Inc(Result);
            J := I;
          end;
      until I < 0;
    end;
end;

function DynArrayCount(const Find: String; const V: StringArray; const IsSortedAscending: Boolean = False): Integer;
var I, J : Integer;
begin
  if IsSortedAscending then
    begin
      I := DynArrayPosNext(Find, V, -1, True);
      if I = -1 then
        Result := 0 else
        begin
          Result := 1;
          J := Length(V);
          while (I + Result < J) and (V[I + Result] = Find) do
            Inc(Result);
        end;
    end
  else
    begin
      J := -1;
      Result := 0;
      repeat
        I := DynArrayPosNext(Find, V, J, False);
        if I >= 0 then
          begin
            Inc(Result);
            J := I;
          end;
      until I < 0;
    end;
end;

function DynArrayCount(const Find: Boolean; const V: BooleanArray; const IsSortedAscending: Boolean = False): Integer;
var I, J : Integer;
begin
  if IsSortedAscending then
    begin
      I := DynArrayPosNext(Find, V, -1, True);
      if I = -1 then
        Result := 0 else
        begin
          Result := 1;
          J := Length(V);
          while (I + Result < J) and (V[I + Result] = Find) do
            Inc(Result);
        end;
    end
  else
    begin
      J := -1;
      Result := 0;
      repeat
        I := DynArrayPosNext(Find, V, J, False);
        if I >= 0 then
          begin
            Inc(Result);
            J := I;
          end;
      until I < 0;
    end;
end;



{                                                                              }
{ DynArrayRemoveAll                                                            }
{                                                                              }
procedure DynArrayRemoveAll(const Find: Byte; var V: ByteArray; const IsSortedAscending: Boolean = False);
var I, J : Integer;
begin
  I := DynArrayPosNext(Find, V, -1, IsSortedAscending);
  while I >= 0 do
    begin
      J := 1;
      while (I + J < Length(V)) and (V[I + J] = Find) do
        Inc(J);
      DynArrayRemove(V, I, J);
      I := DynArrayPosNext(Find, V, I, IsSortedAscending);
    end;
end;

procedure DynArrayRemoveAll(const Find: Word16; var V: Word16Array; const IsSortedAscending: Boolean = False);
var I, J : Integer;
begin
  I := DynArrayPosNext(Find, V, -1, IsSortedAscending);
  while I >= 0 do
    begin
      J := 1;
      while (I + J < Length(V)) and (V[I + J] = Find) do
        Inc(J);
      DynArrayRemove(V, I, J);
      I := DynArrayPosNext(Find, V, I, IsSortedAscending);
    end;
end;

procedure DynArrayRemoveAll(const Find: LongWord; var V: LongWordArray; const IsSortedAscending: Boolean = False);
var I, J : Integer;
begin
  I := DynArrayPosNext(Find, V, -1, IsSortedAscending);
  while I >= 0 do
    begin
      J := 1;
      while (I + J < Length(V)) and (V[I + J] = Find) do
        Inc(J);
      DynArrayRemove(V, I, J);
      I := DynArrayPosNext(Find, V, I, IsSortedAscending);
    end;
end;

procedure DynArrayRemoveAll(const Find: ShortInt; var V: ShortIntArray; const IsSortedAscending: Boolean = False);
var I, J : Integer;
begin
  I := DynArrayPosNext(Find, V, -1, IsSortedAscending);
  while I >= 0 do
    begin
      J := 1;
      while (I + J < Length(V)) and (V[I + J] = Find) do
        Inc(J);
      DynArrayRemove(V, I, J);
      I := DynArrayPosNext(Find, V, I, IsSortedAscending);
    end;
end;

procedure DynArrayRemoveAll(const Find: SmallInt; var V: SmallIntArray; const IsSortedAscending: Boolean = False);
var I, J : Integer;
begin
  I := DynArrayPosNext(Find, V, -1, IsSortedAscending);
  while I >= 0 do
    begin
      J := 1;
      while (I + J < Length(V)) and (V[I + J] = Find) do
        Inc(J);
      DynArrayRemove(V, I, J);
      I := DynArrayPosNext(Find, V, I, IsSortedAscending);
    end;
end;

procedure DynArrayRemoveAll(const Find: LongInt; var V: LongIntArray; const IsSortedAscending: Boolean = False);
var I, J : Integer;
begin
  I := DynArrayPosNext(Find, V, -1, IsSortedAscending);
  while I >= 0 do
    begin
      J := 1;
      while (I + J < Length(V)) and (V[I + J] = Find) do
        Inc(J);
      DynArrayRemove(V, I, J);
      I := DynArrayPosNext(Find, V, I, IsSortedAscending);
    end;
end;

procedure DynArrayRemoveAll(const Find: Int64; var V: Int64Array; const IsSortedAscending: Boolean = False);
var I, J : Integer;
begin
  I := DynArrayPosNext(Find, V, -1, IsSortedAscending);
  while I >= 0 do
    begin
      J := 1;
      while (I + J < Length(V)) and (V[I + J] = Find) do
        Inc(J);
      DynArrayRemove(V, I, J);
      I := DynArrayPosNext(Find, V, I, IsSortedAscending);
    end;
end;

procedure DynArrayRemoveAll(const Find: Single; var V: SingleArray; const IsSortedAscending: Boolean = False);
var I, J : Integer;
begin
  I := DynArrayPosNext(Find, V, -1, IsSortedAscending);
  while I >= 0 do
    begin
      J := 1;
      while (I + J < Length(V)) and (V[I + J] = Find) do
        Inc(J);
      DynArrayRemove(V, I, J);
      I := DynArrayPosNext(Find, V, I, IsSortedAscending);
    end;
end;

procedure DynArrayRemoveAll(const Find: Double; var V: DoubleArray; const IsSortedAscending: Boolean = False);
var I, J : Integer;
begin
  I := DynArrayPosNext(Find, V, -1, IsSortedAscending);
  while I >= 0 do
    begin
      J := 1;
      while (I + J < Length(V)) and (V[I + J] = Find) do
        Inc(J);
      DynArrayRemove(V, I, J);
      I := DynArrayPosNext(Find, V, I, IsSortedAscending);
    end;
end;

procedure DynArrayRemoveAll(const Find: Extended; var V: ExtendedArray; const IsSortedAscending: Boolean = False);
var I, J : Integer;
begin
  I := DynArrayPosNext(Find, V, -1, IsSortedAscending);
  while I >= 0 do
    begin
      J := 1;
      while (I + J < Length(V)) and (V[I + J] = Find) do
        Inc(J);
      DynArrayRemove(V, I, J);
      I := DynArrayPosNext(Find, V, I, IsSortedAscending);
    end;
end;

{$IFDEF SupportAnsiString}
procedure DynArrayRemoveAllA(const Find: AnsiString; var V: AnsiStringArray; const IsSortedAscending: Boolean = False);
var I, J : Integer;
begin
  I := DynArrayPosNextA(Find, V, -1, IsSortedAscending);
  while I >= 0 do
    begin
      J := 1;
      while (I + J < Length(V)) and (V[I + J] = Find) do
        Inc(J);
      DynArrayRemoveA(V, I, J);
      I := DynArrayPosNextA(Find, V, I, IsSortedAscending);
    end;
end;

{$ENDIF}
procedure DynArrayRemoveAllU(const Find: UnicodeString; var V: UnicodeStringArray; const IsSortedAscending: Boolean = False);
var I, J : Integer;
begin
  I := DynArrayPosNextU(Find, V, -1, IsSortedAscending);
  while I >= 0 do
    begin
      J := 1;
      while (I + J < Length(V)) and (V[I + J] = Find) do
        Inc(J);
      DynArrayRemoveU(V, I, J);
      I := DynArrayPosNextU(Find, V, I, IsSortedAscending);
    end;
end;

procedure DynArrayRemoveAll(const Find: String; var V: StringArray; const IsSortedAscending: Boolean = False);
var I, J : Integer;
begin
  I := DynArrayPosNext(Find, V, -1, IsSortedAscending);
  while I >= 0 do
    begin
      J := 1;
      while (I + J < Length(V)) and (V[I + J] = Find) do
        Inc(J);
      DynArrayRemove(V, I, J);
      I := DynArrayPosNext(Find, V, I, IsSortedAscending);
    end;
end;



{                                                                              }
{ DynArrayIntersection                                                         }
{   If both arrays are sorted ascending then time is o(n) instead of o(n^2).   }
{                                                                              }
function DynArrayIntersection(const V1, V2: SingleArray; const IsSortedAscending: Boolean): SingleArray;
var I, J, L, LV : Integer;
begin
  SetLength(Result, 0);
  if IsSortedAscending then
    begin
      I := 0;
      J := 0;
      L := Length(V1);
      LV := Length(V2);
      while (I < L) and (J < LV) do
        begin
          while (I < L) and (V1[I] < V2[J]) do
            Inc(I);
          if I < L then
            begin
              if V1[I] = V2[J] then
                DynArrayAppend(Result, V1[I]);
              while (J < LV) and (V2[J] <= V1[I]) do
                Inc(J);
            end;
        end;
    end
  else
    for I := 0 to Length(V1) - 1 do
      if (DynArrayPosNext(V1[I], V2) >= 0) and (DynArrayPosNext(V1[I], Result) = -1) then
        DynArrayAppend(Result, V1[I]);
end;

function DynArrayIntersection(const V1, V2: DoubleArray; const IsSortedAscending: Boolean): DoubleArray;
var I, J, L, LV : Integer;
begin
  SetLength(Result, 0);
  if IsSortedAscending then
    begin
      I := 0;
      J := 0;
      L := Length(V1);
      LV := Length(V2);
      while (I < L) and (J < LV) do
        begin
          while (I < L) and (V1[I] < V2[J]) do
            Inc(I);
          if I < L then
            begin
              if V1[I] = V2[J] then
                DynArrayAppend(Result, V1[I]);
              while (J < LV) and (V2[J] <= V1[I]) do
                Inc(J);
            end;
        end;
    end
  else
    for I := 0 to Length(V1) - 1 do
      if (DynArrayPosNext(V1[I], V2) >= 0) and (DynArrayPosNext(V1[I], Result) = -1) then
        DynArrayAppend(Result, V1[I]);
end;

function DynArrayIntersection(const V1, V2: ExtendedArray; const IsSortedAscending: Boolean): ExtendedArray;
var I, J, L, LV : Integer;
begin
  SetLength(Result, 0);
  if IsSortedAscending then
    begin
      I := 0;
      J := 0;
      L := Length(V1);
      LV := Length(V2);
      while (I < L) and (J < LV) do
        begin
          while (I < L) and (V1[I] < V2[J]) do
            Inc(I);
          if I < L then
            begin
              if V1[I] = V2[J] then
                DynArrayAppend(Result, V1[I]);
              while (J < LV) and (V2[J] <= V1[I]) do
                Inc(J);
            end;
        end;
    end
  else
    for I := 0 to Length(V1) - 1 do
      if (DynArrayPosNext(V1[I], V2) >= 0) and (DynArrayPosNext(V1[I], Result) = -1) then
        DynArrayAppend(Result, V1[I]);
end;

function DynArrayIntersection(const V1, V2: ByteArray; const IsSortedAscending: Boolean): ByteArray;
var I, J, L, LV : Integer;
begin
  SetLength(Result, 0);
  if IsSortedAscending then
    begin
      I := 0;
      J := 0;
      L := Length(V1);
      LV := Length(V2);
      while (I < L) and (J < LV) do
        begin
          while (I < L) and (V1[I] < V2[J]) do
            Inc(I);
          if I < L then
            begin
              if V1[I] = V2[J] then
                DynArrayAppend(Result, V1[I]);
              while (J < LV) and (V2[J] <= V1[I]) do
                Inc(J);
            end;
        end;
    end
  else
    for I := 0 to Length(V1) - 1 do
      if (DynArrayPosNext(V1[I], V2) >= 0) and (DynArrayPosNext(V1[I], Result) = -1) then
        DynArrayAppend(Result, V1[I]);
end;

function DynArrayIntersection(const V1, V2: Word16Array; const IsSortedAscending: Boolean): Word16Array;
var I, J, L, LV : Integer;
begin
  SetLength(Result, 0);
  if IsSortedAscending then
    begin
      I := 0;
      J := 0;
      L := Length(V1);
      LV := Length(V2);
      while (I < L) and (J < LV) do
        begin
          while (I < L) and (V1[I] < V2[J]) do
            Inc(I);
          if I < L then
            begin
              if V1[I] = V2[J] then
                DynArrayAppend(Result, V1[I]);
              while (J < LV) and (V2[J] <= V1[I]) do
                Inc(J);
            end;
        end;
    end
  else
    for I := 0 to Length(V1) - 1 do
      if (DynArrayPosNext(V1[I], V2) >= 0) and (DynArrayPosNext(V1[I], Result) = -1) then
        DynArrayAppend(Result, V1[I]);
end;

function DynArrayIntersection(const V1, V2: LongWordArray; const IsSortedAscending: Boolean): LongWordArray;
var I, J, L, LV : Integer;
begin
  SetLength(Result, 0);
  if IsSortedAscending then
    begin
      I := 0;
      J := 0;
      L := Length(V1);
      LV := Length(V2);
      while (I < L) and (J < LV) do
        begin
          while (I < L) and (V1[I] < V2[J]) do
            Inc(I);
          if I < L then
            begin
              if V1[I] = V2[J] then
                DynArrayAppend(Result, V1[I]);
              while (J < LV) and (V2[J] <= V1[I]) do
                Inc(J);
            end;
        end;
    end
  else
    for I := 0 to Length(V1) - 1 do
      if (DynArrayPosNext(V1[I], V2) >= 0) and (DynArrayPosNext(V1[I], Result) = -1) then
        DynArrayAppend(Result, V1[I]);
end;

function DynArrayIntersection(const V1, V2: ShortIntArray; const IsSortedAscending: Boolean): ShortIntArray;
var I, J, L, LV : Integer;
begin
  SetLength(Result, 0);
  if IsSortedAscending then
    begin
      I := 0;
      J := 0;
      L := Length(V1);
      LV := Length(V2);
      while (I < L) and (J < LV) do
        begin
          while (I < L) and (V1[I] < V2[J]) do
            Inc(I);
          if I < L then
            begin
              if V1[I] = V2[J] then
                DynArrayAppend(Result, V1[I]);
              while (J < LV) and (V2[J] <= V1[I]) do
                Inc(J);
            end;
        end;
    end
  else
    for I := 0 to Length(V1) - 1 do
      if (DynArrayPosNext(V1[I], V2) >= 0) and (DynArrayPosNext(V1[I], Result) = -1) then
        DynArrayAppend(Result, V1[I]);
end;

function DynArrayIntersection(const V1, V2: SmallIntArray; const IsSortedAscending: Boolean): SmallIntArray;
var I, J, L, LV : Integer;
begin
  SetLength(Result, 0);
  if IsSortedAscending then
    begin
      I := 0;
      J := 0;
      L := Length(V1);
      LV := Length(V2);
      while (I < L) and (J < LV) do
        begin
          while (I < L) and (V1[I] < V2[J]) do
            Inc(I);
          if I < L then
            begin
              if V1[I] = V2[J] then
                DynArrayAppend(Result, V1[I]);
              while (J < LV) and (V2[J] <= V1[I]) do
                Inc(J);
            end;
        end;
    end
  else
    for I := 0 to Length(V1) - 1 do
      if (DynArrayPosNext(V1[I], V2) >= 0) and (DynArrayPosNext(V1[I], Result) = -1) then
        DynArrayAppend(Result, V1[I]);
end;

function DynArrayIntersection(const V1, V2: LongIntArray; const IsSortedAscending: Boolean): LongIntArray;
var I, J, L, LV : Integer;
begin
  SetLength(Result, 0);
  if IsSortedAscending then
    begin
      I := 0;
      J := 0;
      L := Length(V1);
      LV := Length(V2);
      while (I < L) and (J < LV) do
        begin
          while (I < L) and (V1[I] < V2[J]) do
            Inc(I);
          if I < L then
            begin
              if V1[I] = V2[J] then
                DynArrayAppend(Result, V1[I]);
              while (J < LV) and (V2[J] <= V1[I]) do
                Inc(J);
            end;
        end;
    end
  else
    for I := 0 to Length(V1) - 1 do
      if (DynArrayPosNext(V1[I], V2) >= 0) and (DynArrayPosNext(V1[I], Result) = -1) then
        DynArrayAppend(Result, V1[I]);
end;

function DynArrayIntersection(const V1, V2: Int64Array; const IsSortedAscending: Boolean): Int64Array;
var I, J, L, LV : Integer;
begin
  SetLength(Result, 0);
  if IsSortedAscending then
    begin
      I := 0;
      J := 0;
      L := Length(V1);
      LV := Length(V2);
      while (I < L) and (J < LV) do
        begin
          while (I < L) and (V1[I] < V2[J]) do
            Inc(I);
          if I < L then
            begin
              if V1[I] = V2[J] then
                DynArrayAppend(Result, V1[I]);
              while (J < LV) and (V2[J] <= V1[I]) do
                Inc(J);
            end;
        end;
    end
  else
    for I := 0 to Length(V1) - 1 do
      if (DynArrayPosNext(V1[I], V2) >= 0) and (DynArrayPosNext(V1[I], Result) = -1) then
        DynArrayAppend(Result, V1[I]);
end;

{$IFDEF SupportAnsiString}
function DynArrayIntersectionA(const V1, V2: AnsiStringArray; const IsSortedAscending: Boolean): AnsiStringArray;
var I, J, L, LV : Integer;
begin
  SetLength(Result, 0);
  if IsSortedAscending then
    begin
      I := 0;
      J := 0;
      L := Length(V1);
      LV := Length(V2);
      while (I < L) and (J < LV) do
        begin
          while (I < L) and (V1[I] < V2[J]) do
            Inc(I);
          if I < L then
            begin
              if V1[I] = V2[J] then
                DynArrayAppendA(Result, V1[I]);
              while (J < LV) and (V2[J] <= V1[I]) do
                Inc(J);
            end;
        end;
    end
  else
    for I := 0 to Length(V1) - 1 do
      if (DynArrayPosNextA(V1[I], V2) >= 0) and (DynArrayPosNextA(V1[I], Result) = -1) then
        DynArrayAppendA(Result, V1[I]);
end;

{$ENDIF}
function DynArrayIntersectionU(const V1, V2: UnicodeStringArray; const IsSortedAscending: Boolean): UnicodeStringArray;
var I, J, L, LV : Integer;
begin
  SetLength(Result, 0);
  if IsSortedAscending then
    begin
      I := 0;
      J := 0;
      L := Length(V1);
      LV := Length(V2);
      while (I < L) and (J < LV) do
        begin
          while (I < L) and (V1[I] < V2[J]) do
            Inc(I);
          if I < L then
            begin
              if V1[I] = V2[J] then
                DynArrayAppendU(Result, V1[I]);
              while (J < LV) and (V2[J] <= V1[I]) do
                Inc(J);
            end;
        end;
    end
  else
    for I := 0 to Length(V1) - 1 do
      if (DynArrayPosNextU(V1[I], V2) >= 0) and (DynArrayPosNextU(V1[I], Result) = -1) then
        DynArrayAppendU(Result, V1[I]);
end;

function DynArrayIntersection(const V1, V2: StringArray; const IsSortedAscending: Boolean): StringArray;
var I, J, L, LV : Integer;
begin
  SetLength(Result, 0);
  if IsSortedAscending then
    begin
      I := 0;
      J := 0;
      L := Length(V1);
      LV := Length(V2);
      while (I < L) and (J < LV) do
        begin
          while (I < L) and (V1[I] < V2[J]) do
            Inc(I);
          if I < L then
            begin
              if V1[I] = V2[J] then
                DynArrayAppend(Result, V1[I]);
              while (J < LV) and (V2[J] <= V1[I]) do
                Inc(J);
            end;
        end;
    end
  else
    for I := 0 to Length(V1) - 1 do
      if (DynArrayPosNext(V1[I], V2) >= 0) and (DynArrayPosNext(V1[I], Result) = -1) then
        DynArrayAppend(Result, V1[I]);
end;



{                                                                              }
{ DynArrayDifference                                                           }
{   Returns elements in V1 but not in V2.                                      }
{   If both arrays are sorted ascending then time is o(n) instead of o(n^2).   }
{                                                                              }
function DynArrayDifference(const V1, V2: SingleArray; const IsSortedAscending: Boolean): SingleArray;
var I, J, L, LV : Integer;
begin
  SetLength(Result, 0);
  if IsSortedAscending then
    begin
      I := 0;
      J := 0;
      L := Length(V1);
      LV := Length(V2);
      while (I < L) and (J < LV) do
        begin
          while (I < L) and (V1[I] < V2[J]) do
            Inc(I);
          if I < L then
            begin
              if V1[I] <> V2[J] then
                DynArrayAppend(Result, V1[I]);
              while (J < LV) and (V2[J] <= V1[I]) do
                Inc(J);
            end;
        end;
    end
  else
    for I := 0 to Length(V1) - 1 do
      if (DynArrayPosNext(V1[I], V2) = -1) and (DynArrayPosNext(V1[I], Result) = -1) then
        DynArrayAppend(Result, V1[I]);
end;

function DynArrayDifference(const V1, V2: DoubleArray; const IsSortedAscending: Boolean): DoubleArray;
var I, J, L, LV : Integer;
begin
  SetLength(Result, 0);
  if IsSortedAscending then
    begin
      I := 0;
      J := 0;
      L := Length(V1);
      LV := Length(V2);
      while (I < L) and (J < LV) do
        begin
          while (I < L) and (V1[I] < V2[J]) do
            Inc(I);
          if I < L then
            begin
              if V1[I] <> V2[J] then
                DynArrayAppend(Result, V1[I]);
              while (J < LV) and (V2[J] <= V1[I]) do
                Inc(J);
            end;
        end;
    end
  else
    for I := 0 to Length(V1) - 1 do
      if (DynArrayPosNext(V1[I], V2) = -1) and (DynArrayPosNext(V1[I], Result) = -1) then
        DynArrayAppend(Result, V1[I]);
end;

function DynArrayDifference(const V1, V2: ExtendedArray; const IsSortedAscending: Boolean): ExtendedArray;
var I, J, L, LV : Integer;
begin
  SetLength(Result, 0);
  if IsSortedAscending then
    begin
      I := 0;
      J := 0;
      L := Length(V1);
      LV := Length(V2);
      while (I < L) and (J < LV) do
        begin
          while (I < L) and (V1[I] < V2[J]) do
            Inc(I);
          if I < L then
            begin
              if V1[I] <> V2[J] then
                DynArrayAppend(Result, V1[I]);
              while (J < LV) and (V2[J] <= V1[I]) do
                Inc(J);
            end;
        end;
    end
  else
    for I := 0 to Length(V1) - 1 do
      if (DynArrayPosNext(V1[I], V2) = -1) and (DynArrayPosNext(V1[I], Result) = -1) then
        DynArrayAppend(Result, V1[I]);
end;

function DynArrayDifference(const V1, V2: ByteArray; const IsSortedAscending: Boolean): ByteArray;
var I, J, L, LV : Integer;
begin
  SetLength(Result, 0);
  if IsSortedAscending then
    begin
      I := 0;
      J := 0;
      L := Length(V1);
      LV := Length(V2);
      while (I < L) and (J < LV) do
        begin
          while (I < L) and (V1[I] < V2[J]) do
            Inc(I);
          if I < L then
            begin
              if V1[I] <> V2[J] then
                DynArrayAppend(Result, V1[I]);
              while (J < LV) and (V2[J] <= V1[I]) do
                Inc(J);
            end;
        end;
    end
  else
    for I := 0 to Length(V1) - 1 do
      if (DynArrayPosNext(V1[I], V2) = -1) and (DynArrayPosNext(V1[I], Result) = -1) then
        DynArrayAppend(Result, V1[I]);
end;

function DynArrayDifference(const V1, V2: Word16Array; const IsSortedAscending: Boolean): Word16Array;
var I, J, L, LV : Integer;
begin
  SetLength(Result, 0);
  if IsSortedAscending then
    begin
      I := 0;
      J := 0;
      L := Length(V1);
      LV := Length(V2);
      while (I < L) and (J < LV) do
        begin
          while (I < L) and (V1[I] < V2[J]) do
            Inc(I);
          if I < L then
            begin
              if V1[I] <> V2[J] then
                DynArrayAppend(Result, V1[I]);
              while (J < LV) and (V2[J] <= V1[I]) do
                Inc(J);
            end;
        end;
    end
  else
    for I := 0 to Length(V1) - 1 do
      if (DynArrayPosNext(V1[I], V2) = -1) and (DynArrayPosNext(V1[I], Result) = -1) then
        DynArrayAppend(Result, V1[I]);
end;

function DynArrayDifference(const V1, V2: LongWordArray; const IsSortedAscending: Boolean): LongWordArray;
var I, J, L, LV : Integer;
begin
  SetLength(Result, 0);
  if IsSortedAscending then
    begin
      I := 0;
      J := 0;
      L := Length(V1);
      LV := Length(V2);
      while (I < L) and (J < LV) do
        begin
          while (I < L) and (V1[I] < V2[J]) do
            Inc(I);
          if I < L then
            begin
              if V1[I] <> V2[J] then
                DynArrayAppend(Result, V1[I]);
              while (J < LV) and (V2[J] <= V1[I]) do
                Inc(J);
            end;
        end;
    end
  else
    for I := 0 to Length(V1) - 1 do
      if (DynArrayPosNext(V1[I], V2) = -1) and (DynArrayPosNext(V1[I], Result) = -1) then
        DynArrayAppend(Result, V1[I]);
end;

function DynArrayDifference(const V1, V2: ShortIntArray; const IsSortedAscending: Boolean): ShortIntArray;
var I, J, L, LV : Integer;
begin
  SetLength(Result, 0);
  if IsSortedAscending then
    begin
      I := 0;
      J := 0;
      L := Length(V1);
      LV := Length(V2);
      while (I < L) and (J < LV) do
        begin
          while (I < L) and (V1[I] < V2[J]) do
            Inc(I);
          if I < L then
            begin
              if V1[I] <> V2[J] then
                DynArrayAppend(Result, V1[I]);
              while (J < LV) and (V2[J] <= V1[I]) do
                Inc(J);
            end;
        end;
    end
  else
    for I := 0 to Length(V1) - 1 do
      if (DynArrayPosNext(V1[I], V2) = -1) and (DynArrayPosNext(V1[I], Result) = -1) then
        DynArrayAppend(Result, V1[I]);
end;

function DynArrayDifference(const V1, V2: SmallIntArray; const IsSortedAscending: Boolean): SmallIntArray;
var I, J, L, LV : Integer;
begin
  SetLength(Result, 0);
  if IsSortedAscending then
    begin
      I := 0;
      J := 0;
      L := Length(V1);
      LV := Length(V2);
      while (I < L) and (J < LV) do
        begin
          while (I < L) and (V1[I] < V2[J]) do
            Inc(I);
          if I < L then
            begin
              if V1[I] <> V2[J] then
                DynArrayAppend(Result, V1[I]);
              while (J < LV) and (V2[J] <= V1[I]) do
                Inc(J);
            end;
        end;
    end
  else
    for I := 0 to Length(V1) - 1 do
      if (DynArrayPosNext(V1[I], V2) = -1) and (DynArrayPosNext(V1[I], Result) = -1) then
        DynArrayAppend(Result, V1[I]);
end;

function DynArrayDifference(const V1, V2: LongIntArray; const IsSortedAscending: Boolean): LongIntArray;
var I, J, L, LV : Integer;
begin
  SetLength(Result, 0);
  if IsSortedAscending then
    begin
      I := 0;
      J := 0;
      L := Length(V1);
      LV := Length(V2);
      while (I < L) and (J < LV) do
        begin
          while (I < L) and (V1[I] < V2[J]) do
            Inc(I);
          if I < L then
            begin
              if V1[I] <> V2[J] then
                DynArrayAppend(Result, V1[I]);
              while (J < LV) and (V2[J] <= V1[I]) do
                Inc(J);
            end;
        end;
    end
  else
    for I := 0 to Length(V1) - 1 do
      if (DynArrayPosNext(V1[I], V2) = -1) and (DynArrayPosNext(V1[I], Result) = -1) then
        DynArrayAppend(Result, V1[I]);
end;

function DynArrayDifference(const V1, V2: Int64Array; const IsSortedAscending: Boolean): Int64Array;
var I, J, L, LV : Integer;
begin
  SetLength(Result, 0);
  if IsSortedAscending then
    begin
      I := 0;
      J := 0;
      L := Length(V1);
      LV := Length(V2);
      while (I < L) and (J < LV) do
        begin
          while (I < L) and (V1[I] < V2[J]) do
            Inc(I);
          if I < L then
            begin
              if V1[I] <> V2[J] then
                DynArrayAppend(Result, V1[I]);
              while (J < LV) and (V2[J] <= V1[I]) do
                Inc(J);
            end;
        end;
    end
  else
    for I := 0 to Length(V1) - 1 do
      if (DynArrayPosNext(V1[I], V2) = -1) and (DynArrayPosNext(V1[I], Result) = -1) then
        DynArrayAppend(Result, V1[I]);
end;

{$IFDEF SupportAnsiString}
function DynArrayDifferenceA(const V1, V2: AnsiStringArray; const IsSortedAscending: Boolean): AnsiStringArray;
var I, J, L, LV : Integer;
begin
  SetLength(Result, 0);
  if IsSortedAscending then
    begin
      I := 0;
      J := 0;
      L := Length(V1);
      LV := Length(V2);
      while (I < L) and (J < LV) do
        begin
          while (I < L) and (V1[I] < V2[J]) do
            Inc(I);
          if I < L then
            begin
              if V1[I] <> V2[J] then
                DynArrayAppendA(Result, V1[I]);
              while (J < LV) and (V2[J] <= V1[I]) do
                Inc(J);
            end;
        end;
    end
  else
    for I := 0 to Length(V1) - 1 do
      if (DynArrayPosNextA(V1[I], V2) = -1) and (DynArrayPosNextA(V1[I], Result) = -1) then
        DynArrayAppendA(Result, V1[I]);
end;

{$ENDIF}
function DynArrayDifferenceU(const V1, V2: UnicodeStringArray; const IsSortedAscending: Boolean): UnicodeStringArray;
var I, J, L, LV : Integer;
begin
  SetLength(Result, 0);
  if IsSortedAscending then
    begin
      I := 0;
      J := 0;
      L := Length(V1);
      LV := Length(V2);
      while (I < L) and (J < LV) do
        begin
          while (I < L) and (V1[I] < V2[J]) do
            Inc(I);
          if I < L then
            begin
              if V1[I] <> V2[J] then
                DynArrayAppendU(Result, V1[I]);
              while (J < LV) and (V2[J] <= V1[I]) do
                Inc(J);
            end;
        end;
    end
  else
    for I := 0 to Length(V1) - 1 do
      if (DynArrayPosNextU(V1[I], V2) = -1) and (DynArrayPosNextU(V1[I], Result) = -1) then
        DynArrayAppendU(Result, V1[I]);
end;

function DynArrayDifference(const V1, V2: StringArray; const IsSortedAscending: Boolean): StringArray;
var I, J, L, LV : Integer;
begin
  SetLength(Result, 0);
  if IsSortedAscending then
    begin
      I := 0;
      J := 0;
      L := Length(V1);
      LV := Length(V2);
      while (I < L) and (J < LV) do
        begin
          while (I < L) and (V1[I] < V2[J]) do
            Inc(I);
          if I < L then
            begin
              if V1[I] <> V2[J] then
                DynArrayAppend(Result, V1[I]);
              while (J < LV) and (V2[J] <= V1[I]) do
                Inc(J);
            end;
        end;
    end
  else
    for I := 0 to Length(V1) - 1 do
      if (DynArrayPosNext(V1[I], V2) = -1) and (DynArrayPosNext(V1[I], Result) = -1) then
        DynArrayAppend(Result, V1[I]);
end;



{                                                                              }
{ DynArrayReverse                                                              }
{                                                                              }
procedure DynArrayReverse(var V: ByteArray);
var I, L : Integer;
begin
  L := Length(V);
  for I := 1 to L div 2 do
    Swap(V[I - 1], V[L - I]);
end;

procedure DynArrayReverse(var V: Word16Array);
var I, L : Integer;
begin
  L := Length(V);
  for I := 1 to L div 2 do
    Swap(V[I - 1], V[L - I]);
end;

procedure DynArrayReverse(var V: LongWordArray);
var I, L : Integer;
begin
  L := Length(V);
  for I := 1 to L div 2 do
    SwapLW(V[I - 1], V[L - I]);
end;

procedure DynArrayReverse(var V: ShortIntArray);
var I, L : Integer;
begin
  L := Length(V);
  for I := 1 to L div 2 do
    Swap(V[I - 1], V[L - I]);
end;

procedure DynArrayReverse(var V: SmallIntArray);
var I, L : Integer;
begin
  L := Length(V);
  for I := 1 to L div 2 do
    Swap(V[I - 1], V[L - I]);
end;

procedure DynArrayReverse(var V: LongIntArray);
var I, L : Integer;
begin
  L := Length(V);
  for I := 1 to L div 2 do
    SwapLI(V[I - 1], V[L - I]);
end;

procedure DynArrayReverse(var V: Int64Array);
var I, L : Integer;
begin
  L := Length(V);
  for I := 1 to L div 2 do
    Swap(V[I - 1], V[L - I]);
end;

{$IFDEF SupportAnsiString}
procedure DynArrayReverseA(var V: AnsiStringArray);
var I, L : Integer;
begin
  L := Length(V);
  for I := 1 to L div 2 do
    SwapA(V[I - 1], V[L - I]);
end;

{$ENDIF}
procedure DynArrayReverseU(var V: UnicodeStringArray);
var I, L : Integer;
begin
  L := Length(V);
  for I := 1 to L div 2 do
    SwapU(V[I - 1], V[L - I]);
end;

procedure DynArrayReverse(var V: StringArray);
var I, L : Integer;
begin
  L := Length(V);
  for I := 1 to L div 2 do
    Swap(V[I - 1], V[L - I]);
end;

procedure DynArrayReverse(var V: PointerArray);
var I, L : Integer;
begin
  L := Length(V);
  for I := 1 to L div 2 do
    Swap(V[I - 1], V[L - I]);
end;

procedure DynArrayReverse(var V: ObjectArray);
var I, L : Integer;
begin
  L := Length(V);
  for I := 1 to L div 2 do
    Swap(V[I - 1], V[L - I]);
end;

procedure DynArrayReverse(var V: SingleArray);
var I, L : Integer;
begin
  L := Length(V);
  for I := 1 to L div 2 do
    Swap(V[I - 1], V[L - I]);
end;

procedure DynArrayReverse(var V: DoubleArray);
var I, L : Integer;
begin
  L := Length(V);
  for I := 1 to L div 2 do
    Swap(V[I - 1], V[L - I]);
end;

procedure DynArrayReverse(var V: ExtendedArray);
var I, L : Integer;
begin
  L := Length(V);
  for I := 1 to L div 2 do
    SwapExt(V[I - 1], V[L - I]);
end;



{                                                                              }
{ Returns an open array (V) as a dynamic array.                                }
{                                                                              }
function AsBooleanArray(const V: array of Boolean): BooleanArray;
var I : Integer;
begin
  SetLength(Result, High(V) + 1);
  for I := 0 to High(V) do
    Result[I] := V[I];
end;

function AsByteArray(const V: array of Byte): ByteArray;
var I : Integer;
begin
  SetLength(Result, High(V) + 1);
  for I := 0 to High(V) do
    Result[I] := V[I];
end;

function AsWord16Array(const V: array of Word16): Word16Array;
var I : Integer;
begin
  SetLength(Result, High(V) + 1);
  for I := 0 to High(V) do
    Result[I] := V[I];
end;

function AsWord32Array(const V: array of Word32): Word32Array;
var I : Integer;
begin
  SetLength(Result, High(V) + 1);
  for I := 0 to High(V) do
    Result[I] := V[I];
end;

function AsWord64Array(const V: array of Word64): Word64Array;
var I : Integer;
begin
  SetLength(Result, High(V) + 1);
  for I := 0 to High(V) do
    Result[I] := V[I];
end;

function AsLongWordArray(const V: array of LongWord): LongWordArray;
var I : Integer;
begin
  SetLength(Result, High(V) + 1);
  for I := 0 to High(V) do
    Result[I] := V[I];
end;

function AsCardinalArray(const V: array of Cardinal): CardinalArray;
var I : Integer;
begin
  SetLength(Result, High(V) + 1);
  for I := 0 to High(V) do
    Result[I] := V[I];
end;

function AsNativeUIntArray(const V: array of NativeUInt): NativeUIntArray;
var I : Integer;
begin
  SetLength(Result, High(V) + 1);
  for I := 0 to High(V) do
    Result[I] := V[I];
end;

function AsShortIntArray(const V: array of ShortInt): ShortIntArray;
var I : Integer;
begin
  SetLength(Result, High(V) + 1);
  for I := 0 to High(V) do
    Result[I] := V[I];
end;

function AsSmallIntArray(const V: array of SmallInt): SmallIntArray;
var I : Integer;
begin
  SetLength(Result, High(V) + 1);
  for I := 0 to High(V) do
    Result[I] := V[I];
end;

function AsLongIntArray(const V: array of LongInt): LongIntArray;
var I : Integer;
begin
  SetLength(Result, High(V) + 1);
  for I := 0 to High(V) do
    Result[I] := V[I];
end;

function AsIntegerArray(const V: array of Integer): IntegerArray;
var I : Integer;
begin
  SetLength(Result, High(V) + 1);
  for I := 0 to High(V) do
    Result[I] := V[I];
end;

function AsInt32Array(const V: array of Int32): Int32Array;
var I : Integer;
begin
  SetLength(Result, High(V) + 1);
  for I := 0 to High(V) do
    Result[I] := V[I];
end;

function AsInt64Array(const V: array of Int64): Int64Array;
var I : Integer;
begin
  SetLength(Result, High(V) + 1);
  for I := 0 to High(V) do
    Result[I] := V[I];
end;

function AsNativeIntArray(const V: array of NativeInt): NativeIntArray;
var I : Integer;
begin
  SetLength(Result, High(V) + 1);
  for I := 0 to High(V) do
    Result[I] := V[I];
end;

function AsSingleArray(const V: array of Single): SingleArray;
var I : Integer;
begin
  SetLength(Result, High(V) + 1);
  for I := 0 to High(V) do
    Result[I] := V[I];
end;

function AsDoubleArray(const V: array of Double): DoubleArray;
var I : Integer;
begin
  SetLength(Result, High(V) + 1);
  for I := 0 to High(V) do
    Result[I] := V[I];
end;

function AsExtendedArray(const V: array of Extended): ExtendedArray;
var I : Integer;
begin
  SetLength(Result, High(V) + 1);
  for I := 0 to High(V) do
    Result[I] := V[I];
end;

function AsCurrencyArray(const V: array of Currency): CurrencyArray;
var I : Integer;
begin
  SetLength(Result, High(V) + 1);
  for I := 0 to High(V) do
    Result[I] := V[I];
end;

{$IFDEF SupportAnsiString}
function AsAnsiStringArray(const V: array of AnsiString): AnsiStringArray;
var I : Integer;
begin
  SetLength(Result, High(V) + 1);
  for I := 0 to High(V) do
    Result[I] := V[I];
end;

{$ENDIF}
function AsRawByteStringArray(const V: array of RawByteString): RawByteStringArray;
var I : Integer;
begin
  SetLength(Result, High(V) + 1);
  for I := 0 to High(V) do
    Result[I] := V[I];
end;

function AsUnicodeStringArray(const V: array of UnicodeString): UnicodeStringArray;
var I : Integer;
begin
  SetLength(Result, High(V) + 1);
  for I := 0 to High(V) do
    Result[I] := V[I];
end;

function AsStringArray(const V: array of String): StringArray;
var I : Integer;
begin
  SetLength(Result, High(V) + 1);
  for I := 0 to High(V) do
    Result[I] := V[I];
end;

function AsPointerArray(const V: array of Pointer): PointerArray;
var I : Integer;
begin
  SetLength(Result, High(V) + 1);
  for I := 0 to High(V) do
    Result[I] := V[I];
end;

function AsByteCharSetArray(const V: array of ByteCharSet): ByteCharSetArray;
var I : Integer;
begin
  SetLength(Result, High(V) + 1);
  for I := 0 to High(V) do
    Result[I] := V[I];
end;

function AsObjectArray(const V: array of TObject): ObjectArray;
var I : Integer;
begin
  SetLength(Result, High(V) + 1);
  for I := 0 to High(V) do
    Result[I] := V[I];
end;

function AsInterfaceArray(const V: array of IInterface): InterfaceArray;
var I : Integer;
begin
  SetLength(Result, High(V) + 1);
  for I := 0 to High(V) do
    Result[I] := V[I];
end;



function DynArrayRangeByte(const First: Byte; const Count: Integer; const Increment: Byte): ByteArray;
var I : Integer;
    J : Byte;
begin
  SetLength(Result, Count);
  J := First;
  for I := 0 to Count - 1 do
    begin
      Result[I] := J;
      J := J + Increment;
    end;
end;

function DynArrayRangeWord16(const First: Word16; const Count: Integer; const Increment: Word16): Word16Array;
var I : Integer;
    J : Word16;
begin
  SetLength(Result, Count);
  J := First;
  for I := 0 to Count - 1 do
    begin
      Result[I] := J;
      J := J + Increment;
    end;
end;

function DynArrayRangeLongWord(const First: LongWord; const Count: Integer; const Increment: LongWord): LongWordArray;
var I : Integer;
    J : LongWord;
begin
  SetLength(Result, Count);
  J := First;
  for I := 0 to Count - 1 do
    begin
      Result[I] := J;
      J := J + Increment;
    end;
end;

function DynArrayRangeCardinal(const First: Cardinal; const Count: Integer; const Increment: Cardinal): CardinalArray;
var I : Integer;
    J : Cardinal;
begin
  SetLength(Result, Count);
  J := First;
  for I := 0 to Count - 1 do
    begin
      Result[I] := J;
      J := J + Increment;
    end;
end;

function DynArrayRangeShortInt(const First: ShortInt; const Count: Integer; const Increment: ShortInt): ShortIntArray;
var I : Integer;
    J : ShortInt;
begin
  SetLength(Result, Count);
  J := First;
  for I := 0 to Count - 1 do
    begin
      Result[I] := J;
      J := J + Increment;
    end;
end;

function DynArrayRangeSmallInt(const First: SmallInt; const Count: Integer; const Increment: SmallInt): SmallIntArray;
var I : Integer;
    J : SmallInt;
begin
  SetLength(Result, Count);
  J := First;
  for I := 0 to Count - 1 do
    begin
      Result[I] := J;
      J := J + Increment;
    end;
end;

function DynArrayRangeLongInt(const First: LongInt; const Count: Integer; const Increment: LongInt): LongIntArray;
var I : Integer;
    J : LongInt;
begin
  SetLength(Result, Count);
  J := First;
  for I := 0 to Count - 1 do
    begin
      Result[I] := J;
      J := J + Increment;
    end;
end;

function DynArrayRangeInteger(const First: Integer; const Count: Integer; const Increment: Integer): IntegerArray;
var I : Integer;
    J : Integer;
begin
  SetLength(Result, Count);
  J := First;
  for I := 0 to Count - 1 do
    begin
      Result[I] := J;
      J := J + Increment;
    end;
end;

function DynArrayRangeInt64(const First: Int64; const Count: Integer; const Increment: Int64): Int64Array;
var I : Integer;
    J : Int64;
begin
  SetLength(Result, Count);
  J := First;
  for I := 0 to Count - 1 do
    begin
      Result[I] := J;
      J := J + Increment;
    end;
end;

function DynArrayRangeSingle(const First: Single; const Count: Integer; const Increment: Single): SingleArray;
var I : Integer;
    J : Single;
begin
  SetLength(Result, Count);
  J := First;
  for I := 0 to Count - 1 do
    begin
      Result[I] := J;
      J := J + Increment;
    end;
end;

function DynArrayRangeDouble(const First: Double; const Count: Integer; const Increment: Double): DoubleArray;
var I : Integer;
    J : Double;
begin
  SetLength(Result, Count);
  J := First;
  for I := 0 to Count - 1 do
    begin
      Result[I] := J;
      J := J + Increment;
    end;
end;

function DynArrayRangeExtended(const First: Extended; const Count: Integer; const Increment: Extended): ExtendedArray;
var I : Integer;
    J : Extended;
begin
  SetLength(Result, Count);
  J := First;
  for I := 0 to Count - 1 do
    begin
      Result[I] := J;
      J := J + Increment;
    end;
end;



{                                                                              }
{ DynArrayDup                                                                  }
{                                                                              }
function DynArrayDupByte(const V: Byte; const Count: Integer): ByteArray;
begin
  if Count <= 0 then
    begin
      SetLength(Result, 0);
      exit;
    end;
  SetLength(Result, Count);
  FillChar(Result[0], Count, V);
end;

function DynArrayDupWord16(const V: Word16; const Count: Integer): Word16Array;
var I : Integer;
begin
  if Count <= 0 then
    begin
      SetLength(Result, 0);
      exit;
    end;
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
    Result[I] := V;
end;

function DynArrayDupLongWord(const V: LongWord; const Count: Integer): LongWordArray;
var I : Integer;
begin
  if Count <= 0 then
    begin
      SetLength(Result, 0);
      exit;
    end;
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
    Result[I] := V;
end;

function DynArrayDupCardinal(const V: Cardinal; const Count: Integer): CardinalArray;
var I : Integer;
begin
  if Count <= 0 then
    begin
      SetLength(Result, 0);
      exit;
    end;
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
    Result[I] := V;
end;

function DynArrayDupNativeUInt(const V: NativeUInt; const Count: Integer): NativeUIntArray;
var I : Integer;
begin
  if Count <= 0 then
    begin
      SetLength(Result, 0);
      exit;
    end;
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
    Result[I] := V;
end;

function DynArrayDupShortInt(const V: ShortInt; const Count: Integer): ShortIntArray;
var I : Integer;
begin
  if Count <= 0 then
    begin
      SetLength(Result, 0);
      exit;
    end;
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
    Result[I] := V;
end;

function DynArrayDupSmallInt(const V: SmallInt; const Count: Integer): SmallIntArray;
var I : Integer;
begin
  if Count <= 0 then
    begin
      SetLength(Result, 0);
      exit;
    end;
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
    Result[I] := V;
end;

function DynArrayDupLongInt(const V: LongInt; const Count: Integer): LongIntArray;
var I : Integer;
begin
  if Count <= 0 then
    begin
      SetLength(Result, 0);
      exit;
    end;
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
    Result[I] := V;
end;

function DynArrayDupInteger(const V: Integer; const Count: Integer): IntegerArray;
var I : Integer;
begin
  if Count <= 0 then
    begin
      SetLength(Result, 0);
      exit;
    end;
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
    Result[I] := V;
end;

function DynArrayDupInt64(const V: Int64; const Count: Integer): Int64Array;
var I : Integer;
begin
  if Count <= 0 then
    begin
      SetLength(Result, 0);
      exit;
    end;
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
    Result[I] := V;
end;

function DynArrayDupNativeInt(const V: NativeInt; const Count: Integer): NativeIntArray;
var I : Integer;
begin
  if Count <= 0 then
    begin
      SetLength(Result, 0);
      exit;
    end;
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
    Result[I] := V;
end;

function DynArrayDupSingle(const V: Single; const Count: Integer): SingleArray;
var I : Integer;
begin
  if Count <= 0 then
    begin
      SetLength(Result, 0);
      exit;
    end;
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
    Result[I] := V;
end;

function DynArrayDupDouble(const V: Double; const Count: Integer): DoubleArray;
var I : Integer;
begin
  if Count <= 0 then
    begin
      SetLength(Result, 0);
      exit;
    end;
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
    Result[I] := V;
end;

function DynArrayDupExtended(const V: Extended; const Count: Integer): ExtendedArray;
var I : Integer;
begin
  if Count <= 0 then
    begin
      SetLength(Result, 0);
      exit;
    end;
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
    Result[I] := V;
end;

function DynArrayDupCurrency(const V: Currency; const Count: Integer): CurrencyArray;
var I : Integer;
begin
  if Count <= 0 then
    begin
      SetLength(Result, 0);
      exit;
    end;
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
    Result[I] := V;
end;

{$IFDEF SupportAnsiString}
function DynArrayDupAnsiString(const V: AnsiString; const Count: Integer): AnsiStringArray;
var I : Integer;
begin
  if Count <= 0 then
    begin
      SetLength(Result, 0);
      exit;
    end;
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
    Result[I] := V;
end;

{$ENDIF}
function DynArrayDupUnicodeString(const V: UnicodeString; const Count: Integer): UnicodeStringArray;
var I : Integer;
begin
  if Count <= 0 then
    begin
      SetLength(Result, 0);
      exit;
    end;
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
    Result[I] := V;
end;

function DynArrayDupString(const V: String; const Count: Integer): StringArray;
var I : Integer;
begin
  if Count <= 0 then
    begin
      SetLength(Result, 0);
      exit;
    end;
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
    Result[I] := V;
end;

function DynArrayDupByteCharSet(const V: ByteCharSet; const Count: Integer): ByteCharSetArray;
var I : Integer;
begin
  if Count <= 0 then
    begin
      SetLength(Result, 0);
      exit;
    end;
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
    Result[I] := V;
end;

function DynArrayDupObject(const V: TObject; const Count: Integer): ObjectArray;
var I : Integer;
begin
  if Count <= 0 then
    begin
      SetLength(Result, 0);
      exit;
    end;
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
    Result[I] := V;
end;



{                                                                              }
{ SetLengthAndZero                                                             }
{                                                                              }
procedure SetLengthAndZero(var V: ByteArray; const NewLength: Integer);
var OldLen, NewLen : Integer;
begin
  NewLen := NewLength;
  if NewLen < 0 then
    NewLen := 0;
  OldLen := Length(V);
  if OldLen = NewLen then
    exit;
  SetLength(V, NewLen);
  if OldLen > NewLen then
    exit;
  FillChar(Pointer(@V[OldLen])^, Sizeof(Byte) * (NewLen - OldLen), #0);
end;

procedure SetLengthAndZero(var V: Word16Array; const NewLength: Integer);
var OldLen, NewLen : Integer;
begin
  NewLen := NewLength;
  if NewLen < 0 then
    NewLen := 0;
  OldLen := Length(V);
  if OldLen = NewLen then
    exit;
  SetLength(V, NewLen);
  if OldLen > NewLen then
    exit;
  FillChar(Pointer(@V[OldLen])^, Sizeof(Word16) * (NewLen - OldLen), #0);
end;

procedure SetLengthAndZero(var V: Word32Array; const NewLength: Integer);
var OldLen, NewLen : Integer;
begin
  NewLen := NewLength;
  if NewLen < 0 then
    NewLen := 0;
  OldLen := Length(V);
  if OldLen = NewLen then
    exit;
  SetLength(V, NewLen);
  if OldLen > NewLen then
    exit;
  FillChar(Pointer(@V[OldLen])^, Sizeof(Word32) * (NewLen - OldLen), #0);
end;

procedure SetLengthAndZero(var V: Word64Array; const NewLength: Integer);
var OldLen, NewLen : Integer;
begin
  NewLen := NewLength;
  if NewLen < 0 then
    NewLen := 0;
  OldLen := Length(V);
  if OldLen = NewLen then
    exit;
  SetLength(V, NewLen);
  if OldLen > NewLen then
    exit;
  FillChar(Pointer(@V[OldLen])^, Sizeof(Word64) * (NewLen - OldLen), #0);
end;

procedure SetLengthAndZero(var V: LongWordArray; const NewLength: Integer);
var OldLen, NewLen : Integer;
begin
  NewLen := NewLength;
  if NewLen < 0 then
    NewLen := 0;
  OldLen := Length(V);
  if OldLen = NewLen then
    exit;
  SetLength(V, NewLen);
  if OldLen > NewLen then
    exit;
  FillChar(Pointer(@V[OldLen])^, Sizeof(LongWord) * (NewLen - OldLen), #0);
end;

procedure SetLengthAndZero(var V: CardinalArray; const NewLength: Integer);
var OldLen, NewLen : Integer;
begin
  NewLen := NewLength;
  if NewLen < 0 then
    NewLen := 0;
  OldLen := Length(V);
  if OldLen = NewLen then
    exit;
  SetLength(V, NewLen);
  if OldLen > NewLen then
    exit;
  FillChar(Pointer(@V[OldLen])^, Sizeof(Cardinal) * (NewLen - OldLen), #0);
end;

procedure SetLengthAndZero(var V: NativeUIntArray; const NewLength: Integer);
var OldLen, NewLen : Integer;
begin
  NewLen := NewLength;
  if NewLen < 0 then
    NewLen := 0;
  OldLen := Length(V);
  if OldLen = NewLen then
    exit;
  SetLength(V, NewLen);
  if OldLen > NewLen then
    exit;
  FillChar(Pointer(@V[OldLen])^, Sizeof(NativeUInt) * (NewLen - OldLen), #0);
end;

procedure SetLengthAndZero(var V: ShortIntArray; const NewLength: Integer);
var OldLen, NewLen : Integer;
begin
  NewLen := NewLength;
  if NewLen < 0 then
    NewLen := 0;
  OldLen := Length(V);
  if OldLen = NewLen then
    exit;
  SetLength(V, NewLen);
  if OldLen > NewLen then
    exit;
  FillChar(Pointer(@V[OldLen])^, Sizeof(ShortInt) * (NewLen - OldLen), #0);
end;

procedure SetLengthAndZero(var V: SmallIntArray; const NewLength: Integer);
var OldLen, NewLen : Integer;
begin
  NewLen := NewLength;
  if NewLen < 0 then
    NewLen := 0;
  OldLen := Length(V);
  if OldLen = NewLen then
    exit;
  SetLength(V, NewLen);
  if OldLen > NewLen then
    exit;
  FillChar(Pointer(@V[OldLen])^, Sizeof(SmallInt) * (NewLen - OldLen), #0);
end;

procedure SetLengthAndZero(var V: LongIntArray; const NewLength: Integer);
var OldLen, NewLen : Integer;
begin
  NewLen := NewLength;
  if NewLen < 0 then
    NewLen := 0;
  OldLen := Length(V);
  if OldLen = NewLen then
    exit;
  SetLength(V, NewLen);
  if OldLen > NewLen then
    exit;
  FillChar(Pointer(@V[OldLen])^, Sizeof(LongInt) * (NewLen - OldLen), #0);
end;

procedure SetLengthAndZero(var V: IntegerArray; const NewLength: Integer);
var OldLen, NewLen : Integer;
begin
  NewLen := NewLength;
  if NewLen < 0 then
    NewLen := 0;
  OldLen := Length(V);
  if OldLen = NewLen then
    exit;
  SetLength(V, NewLen);
  if OldLen > NewLen then
    exit;
  FillChar(Pointer(@V[OldLen])^, Sizeof(Integer) * (NewLen - OldLen), #0);
end;

procedure SetLengthAndZero(var V: Int32Array; const NewLength: Integer);
var OldLen, NewLen : Integer;
begin
  NewLen := NewLength;
  if NewLen < 0 then
    NewLen := 0;
  OldLen := Length(V);
  if OldLen = NewLen then
    exit;
  SetLength(V, NewLen);
  if OldLen > NewLen then
    exit;
  FillChar(Pointer(@V[OldLen])^, Sizeof(Int32) * (NewLen - OldLen), #0);
end;

procedure SetLengthAndZero(var V: Int64Array; const NewLength: Integer);
var OldLen, NewLen : Integer;
begin
  NewLen := NewLength;
  if NewLen < 0 then
    NewLen := 0;
  OldLen := Length(V);
  if OldLen = NewLen then
    exit;
  SetLength(V, NewLen);
  if OldLen > NewLen then
    exit;
  FillChar(Pointer(@V[OldLen])^, Sizeof(Int64) * (NewLen - OldLen), #0);
end;

procedure SetLengthAndZero(var V: NativeIntArray; const NewLength: Integer);
var OldLen, NewLen : Integer;
begin
  NewLen := NewLength;
  if NewLen < 0 then
    NewLen := 0;
  OldLen := Length(V);
  if OldLen = NewLen then
    exit;
  SetLength(V, NewLen);
  if OldLen > NewLen then
    exit;
  FillChar(Pointer(@V[OldLen])^, Sizeof(NativeInt) * (NewLen - OldLen), #0);
end;

procedure SetLengthAndZero(var V: SingleArray; const NewLength: Integer);
var OldLen, NewLen : Integer;
begin
  NewLen := NewLength;
  if NewLen < 0 then
    NewLen := 0;
  OldLen := Length(V);
  if OldLen = NewLen then
    exit;
  SetLength(V, NewLen);
  if OldLen > NewLen then
    exit;
  FillChar(Pointer(@V[OldLen])^, Sizeof(Single) * (NewLen - OldLen), #0);
end;

procedure SetLengthAndZero(var V: DoubleArray; const NewLength: Integer);
var OldLen, NewLen : Integer;
begin
  NewLen := NewLength;
  if NewLen < 0 then
    NewLen := 0;
  OldLen := Length(V);
  if OldLen = NewLen then
    exit;
  SetLength(V, NewLen);
  if OldLen > NewLen then
    exit;
  FillChar(Pointer(@V[OldLen])^, Sizeof(Double) * (NewLen - OldLen), #0);
end;

procedure SetLengthAndZero(var V: ExtendedArray; const NewLength: Integer);
var OldLen, NewLen : Integer;
begin
  NewLen := NewLength;
  if NewLen < 0 then
    NewLen := 0;
  OldLen := Length(V);
  if OldLen = NewLen then
    exit;
  SetLength(V, NewLen);
  if OldLen > NewLen then
    exit;
  FillChar(Pointer(@V[OldLen])^, Sizeof(Extended) * (NewLen - OldLen), #0);
end;

procedure SetLengthAndZero(var V: CurrencyArray; const NewLength: Integer);
var OldLen, NewLen : Integer;
begin
  NewLen := NewLength;
  if NewLen < 0 then
    NewLen := 0;
  OldLen := Length(V);
  if OldLen = NewLen then
    exit;
  SetLength(V, NewLen);
  if OldLen > NewLen then
    exit;
  FillChar(Pointer(@V[OldLen])^, Sizeof(Currency) * (NewLen - OldLen), #0);
end;

procedure SetLengthAndZero(var V: ByteCharSetArray; const NewLength: Integer);
var OldLen, NewLen : Integer;
begin
  NewLen := NewLength;
  if NewLen < 0 then
    NewLen := 0;
  OldLen := Length(V);
  if OldLen = NewLen then
    exit;
  SetLength(V, NewLen);
  if OldLen > NewLen then
    exit;
  FillChar(Pointer(@V[OldLen])^, Sizeof(ByteCharSet) * (NewLen - OldLen), #0);
end;

procedure SetLengthAndZero(var V: BooleanArray; const NewLength: Integer);
var OldLen, NewLen : Integer;
begin
  NewLen := NewLength;
  if NewLen < 0 then
    NewLen := 0;
  OldLen := Length(V);
  if OldLen = NewLen then
    exit;
  SetLength(V, NewLen);
  if OldLen > NewLen then
    exit;
  FillChar(Pointer(@V[OldLen])^, Sizeof(Boolean) * (NewLen - OldLen), #0);
end;

procedure SetLengthAndZero(var V: PointerArray; const NewLength: Integer);
var OldLen, NewLen : Integer;
begin
  NewLen := NewLength;
  if NewLen < 0 then
    NewLen := 0;
  OldLen := Length(V);
  if OldLen = NewLen then
    exit;
  SetLength(V, NewLen);
  if OldLen > NewLen then
    exit;
  FillChar(Pointer(@V[OldLen])^, Sizeof(Pointer) * (NewLen - OldLen), #0);
end;

procedure SetLengthAndZero(var V: ObjectArray; const NewLength: Integer;
    const FreeObjects: Boolean);
var I, L : Integer;
begin
  L := Length(V);
  if L = NewLength then
    exit;
  if (L > NewLength) and FreeObjects then
    for I := NewLength to L - 1 do
      FreeAndNil(V[I]);
  SetLength(V, NewLength);
  if L > NewLength then
    exit;
  FillChar(V[L], Sizeof(Pointer) * (NewLength - L), #0);
end;



{                                                                              }
{ DynArrayIsEqual                                                              }
{                                                                              }
function DynArrayIsEqual(const V1, V2: ByteArray): Boolean;
var L : Integer;
begin
  L := Length(V1);
  if L <> Length(V2) then
    begin
      Result := False;
      exit;
    end;
  Result := EqualMem(Pointer(V1)^, Pointer(V2)^, Sizeof(Byte) * L);
end;

function DynArrayIsEqual(const V1, V2: Word16Array): Boolean;
var L : Integer;
begin
  L := Length(V1);
  if L <> Length(V2) then
    begin
      Result := False;
      exit;
    end;
  Result := EqualMem(Pointer(V1)^, Pointer(V2)^, Sizeof(Word16) * L);
end;

function DynArrayIsEqual(const V1, V2: LongWordArray): Boolean;
var L : Integer;
begin
  L := Length(V1);
  if L <> Length(V2) then
    begin
      Result := False;
      exit;
    end;
  Result := EqualMem(Pointer(V1)^, Pointer(V2)^, Sizeof(LongWord) * L);
end;

function DynArrayIsEqual(const V1, V2: ShortIntArray): Boolean;
var L : Integer;
begin
  L := Length(V1);
  if L <> Length(V2) then
    begin
      Result := False;
      exit;
    end;
  Result := EqualMem(Pointer(V1)^, Pointer(V2)^, Sizeof(ShortInt) * L);
end;

function DynArrayIsEqual(const V1, V2: SmallIntArray): Boolean;
var L : Integer;
begin
  L := Length(V1);
  if L <> Length(V2) then
    begin
      Result := False;
      exit;
    end;
  Result := EqualMem(Pointer(V1)^, Pointer(V2)^, Sizeof(SmallInt) * L);
end;

function DynArrayIsEqual(const V1, V2: LongIntArray): Boolean;
var L : Integer;
begin
  L := Length(V1);
  if L <> Length(V2) then
    begin
      Result := False;
      exit;
    end;
  Result := EqualMem(Pointer(V1)^, Pointer(V2)^, Sizeof(LongInt) * L);
end;

function DynArrayIsEqual(const V1, V2: Int64Array): Boolean;
var L : Integer;
begin
  L := Length(V1);
  if L <> Length(V2) then
    begin
      Result := False;
      exit;
    end;
  Result := EqualMem(Pointer(V1)^, Pointer(V2)^, Sizeof(Int64) * L);
end;

function DynArrayIsEqual(const V1, V2: SingleArray): Boolean;
var L : Integer;
begin
  L := Length(V1);
  if L <> Length(V2) then
    begin
      Result := False;
      exit;
    end;
  Result := EqualMem(Pointer(V1)^, Pointer(V2)^, Sizeof(Single) * L);
end;

function DynArrayIsEqual(const V1, V2: DoubleArray): Boolean;
var L : Integer;
begin
  L := Length(V1);
  if L <> Length(V2) then
    begin
      Result := False;
      exit;
    end;
  Result := EqualMem(Pointer(V1)^, Pointer(V2)^, Sizeof(Double) * L);
end;

function DynArrayIsEqual(const V1, V2: ExtendedArray): Boolean;
var L : Integer;
begin
  L := Length(V1);
  if L <> Length(V2) then
    begin
      Result := False;
      exit;
    end;
  Result := EqualMem(Pointer(V1)^, Pointer(V2)^, Sizeof(Extended) * L);
end;

function DynArrayIsEqual(const V1, V2: CurrencyArray): Boolean;
var L : Integer;
begin
  L := Length(V1);
  if L <> Length(V2) then
    begin
      Result := False;
      exit;
    end;
  Result := EqualMem(Pointer(V1)^, Pointer(V2)^, Sizeof(Currency) * L);
end;

{$IFDEF SupportAnsiString}
function DynArrayIsEqualA(const V1, V2: AnsiStringArray): Boolean;
var I, L : Integer;
begin
  L := Length(V1);
  if L <> Length(V2) then
    begin
      Result := False;
      exit;
    end;
  for I := 0 to L - 1 do
    if V1[I] <> V2[I] then
      begin
        Result := False;
        exit;
      end;
  Result := True;
end;

{$ENDIF}
function DynArrayIsEqualB(const V1, V2: RawByteStringArray): Boolean;
var I, L : Integer;
begin
  L := Length(V1);
  if L <> Length(V2) then
    begin
      Result := False;
      exit;
    end;
  for I := 0 to L - 1 do
    if V1[I] <> V2[I] then
      begin
        Result := False;
        exit;
      end;
  Result := True;
end;

function DynArrayIsEqualU(const V1, V2: UnicodeStringArray): Boolean;
var I, L : Integer;
begin
  L := Length(V1);
  if L <> Length(V2) then
    begin
      Result := False;
      exit;
    end;
  for I := 0 to L - 1 do
    if V1[I] <> V2[I] then
      begin
        Result := False;
        exit;
      end;
  Result := True;
end;

function DynArrayIsEqual(const V1, V2: StringArray): Boolean;
var I, L : Integer;
begin
  L := Length(V1);
  if L <> Length(V2) then
    begin
      Result := False;
      exit;
    end;
  for I := 0 to L - 1 do
    if V1[I] <> V2[I] then
      begin
        Result := False;
        exit;
      end;
  Result := True;
end;

function DynArrayIsEqual(const V1, V2: ByteCharSetArray): Boolean;
var I, L : Integer;
begin
  L := Length(V1);
  if L <> Length(V2) then
    begin
      Result := False;
      exit;
    end;
  for I := 0 to L - 1 do
    if V1[I] <> V2[I] then
      begin
        Result := False;
        exit;
      end;
  Result := True;
end;



{                                                                              }
{ Dynamic array to Dynamic array                                               }
{                                                                              }
function ByteArrayToLongIntArray(const V: ByteArray): LongIntArray;
var I, L : Integer;
begin
  L := Length(V);
  SetLength(Result, L);
  for I := 0 to L - 1 do
    Result[I] := V[I];
end;

function Word16ArrayToLongIntArray(const V: Word16Array): LongIntArray;
var I, L : Integer;
begin
  L := Length(V);
  SetLength(Result, L);
  for I := 0 to L - 1 do
    Result[I] := V[I];
end;

function ShortIntArrayToLongIntArray(const V: ShortIntArray): LongIntArray;
var I, L : Integer;
begin
  L := Length(V);
  SetLength(Result, L);
  for I := 0 to L - 1 do
    Result[I] := V[I];
end;

function SmallIntArrayToLongIntArray(const V: SmallIntArray): LongIntArray;
var I, L : Integer;
begin
  L := Length(V);
  SetLength(Result, L);
  for I := 0 to L - 1 do
    Result[I] := V[I];
end;

function LongIntArrayToInt64Array(const V: LongIntArray): Int64Array;
var I, L : Integer;
begin
  L := Length(V);
  SetLength(Result, L);
  for I := 0 to L - 1 do
    Result[I] := V[I];
end;

function LongIntArrayToSingleArray(const V: LongIntArray): SingleArray;
var I, L : Integer;
begin
  L := Length(V);
  SetLength(Result, L);
  for I := 0 to L - 1 do
    Result[I] := V[I];
end;

function LongIntArrayToDoubleArray(const V: LongIntArray): DoubleArray;
var I, L : Integer;
begin
  L := Length(V);
  SetLength(Result, L);
  for I := 0 to L - 1 do
    Result[I] := V[I];
end;

function LongIntArrayToExtendedArray(const V: LongIntArray): ExtendedArray;
var I, L : Integer;
begin
  L := Length(V);
  SetLength(Result, L);
  for I := 0 to L - 1 do
    Result[I] := V[I];
end;

function SingleArrayToDoubleArray(const V: SingleArray): DoubleArray;
var I, L : Integer;
begin
  L := Length(V);
  SetLength(Result, L);
  for I := 0 to L - 1 do
    Result[I] := V[I];
end;

function SingleArrayToExtendedArray(const V: SingleArray): ExtendedArray;
var I, L : Integer;
begin
  L := Length(V);
  SetLength(Result, L);
  for I := 0 to L - 1 do
    Result[I] := V[I];
end;

function SingleArrayToCurrencyArray(const V: SingleArray): CurrencyArray;
var I, L : Integer;
begin
  L := Length(V);
  SetLength(Result, L);
  for I := 0 to L - 1 do
    Result[I] := V[I];
end;

function SingleArrayToLongIntArray(const V: SingleArray): LongIntArray;
var I, L : Integer;
begin
  L := Length(V);
  SetLength(Result, L);
  for I := 0 to L - 1 do
    Result[I] := LongInt(Trunc(V[I]));
end;

function SingleArrayToInt64Array(const V: SingleArray): Int64Array;
var I, L : Integer;
begin
  L := Length(V);
  SetLength(Result, L);
  for I := 0 to L - 1 do
    Result[I] := Trunc(V[I]);
end;

function DoubleArrayToExtendedArray(const V: DoubleArray): ExtendedArray;
var I, L : Integer;
begin
  L := Length(V);
  SetLength(Result, L);
  for I := 0 to L - 1 do
    Result[I] := V[I];
end;

function DoubleArrayToCurrencyArray(const V: DoubleArray): CurrencyArray;
var I, L : Integer;
begin
  L := Length(V);
  SetLength(Result, L);
  for I := 0 to L - 1 do
    Result[I] := V[I];
end;

function DoubleArrayToLongIntArray(const V: DoubleArray): LongIntArray;
var I, L : Integer;
begin
  L := Length(V);
  SetLength(Result, L);
  for I := 0 to L - 1 do
    Result[I] := LongInt(Trunc(V[I]));
end;

function DoubleArrayToInt64Array(const V: DoubleArray): Int64Array;
var I, L : Integer;
begin
  L := Length(V);
  SetLength(Result, L);
  for I := 0 to L - 1 do
    Result[I] := Trunc(V[I]);
end;

function ExtendedArrayToCurrencyArray(const V: ExtendedArray): CurrencyArray;
var I, L : Integer;
begin
  L := Length(V);
  SetLength(Result, L);
  for I := 0 to L - 1 do
    Result[I] := V[I];
end;

function ExtendedArrayToLongIntArray(const V: ExtendedArray): LongIntArray;
var I, L : Integer;
begin
  L := Length(V);
  SetLength(Result, L);
  for I := 0 to L - 1 do
    Result[I] := LongInt(Trunc(V[I]));
end;

function ExtendedArrayToInt64Array(const V: ExtendedArray): Int64Array;
var I, L : Integer;
begin
  L := Length(V);
  SetLength(Result, L);
  for I := 0 to L - 1 do
    Result[I] := Trunc(V[I]);
end;



{                                                                              }
{ Array from indexes                                                           }
{                                                                              }
function ByteArrayFromIndexes(const V: ByteArray; const Indexes: IntegerArray): ByteArray;
var I, L : Integer;
begin
  L := Length(Indexes);
  SetLength(Result, L);
  for I := 0 to L - 1 do
    Result[I] := V[Indexes[I]];
end;

function Word16ArrayFromIndexes(const V: Word16Array; const Indexes: IntegerArray): Word16Array;
var I, L : Integer;
begin
  L := Length(Indexes);
  SetLength(Result, L);
  for I := 0 to L - 1 do
    Result[I] := V[Indexes[I]];
end;

function LongWordArrayFromIndexes(const V: LongWordArray; const Indexes: IntegerArray): LongWordArray;
var I, L : Integer;
begin
  L := Length(Indexes);
  SetLength(Result, L);
  for I := 0 to L - 1 do
    Result[I] := V[Indexes[I]];
end;

function CardinalArrayFromIndexes(const V: CardinalArray; const Indexes: IntegerArray): CardinalArray;
var I, L : Integer;
begin
  L := Length(Indexes);
  SetLength(Result, L);
  for I := 0 to L - 1 do
    Result[I] := V[Indexes[I]];
end;

function ShortIntArrayFromIndexes(const V: ShortIntArray; const Indexes: IntegerArray): ShortIntArray;
var I, L : Integer;
begin
  L := Length(Indexes);
  SetLength(Result, L);
  for I := 0 to L - 1 do
    Result[I] := V[Indexes[I]];
end;

function SmallIntArrayFromIndexes(const V: SmallIntArray; const Indexes: IntegerArray): SmallIntArray;
var I, L : Integer;
begin
  L := Length(Indexes);
  SetLength(Result, L);
  for I := 0 to L - 1 do
    Result[I] := V[Indexes[I]];
end;

function LongIntArrayFromIndexes(const V: LongIntArray; const Indexes: IntegerArray): LongIntArray;
var I, L : Integer;
begin
  L := Length(Indexes);
  SetLength(Result, L);
  for I := 0 to L - 1 do
    Result[I] := V[Indexes[I]];
end;

function IntegerArrayFromIndexes(const V: IntegerArray; const Indexes: IntegerArray): IntegerArray;
var I, L : Integer;
begin
  L := Length(Indexes);
  SetLength(Result, L);
  for I := 0 to L - 1 do
    Result[I] := V[Indexes[I]];
end;

function Int64ArrayFromIndexes(const V: Int64Array; const Indexes: IntegerArray): Int64Array;
var I, L : Integer;
begin
  L := Length(Indexes);
  SetLength(Result, L);
  for I := 0 to L - 1 do
    Result[I] := V[Indexes[I]];
end;

function SingleArrayFromIndexes(const V: SingleArray; const Indexes: IntegerArray): SingleArray;
var I, L : Integer;
begin
  L := Length(Indexes);
  SetLength(Result, L);
  for I := 0 to L - 1 do
    Result[I] := V[Indexes[I]];
end;

function DoubleArrayFromIndexes(const V: DoubleArray; const Indexes: IntegerArray): DoubleArray;
var I, L : Integer;
begin
  L := Length(Indexes);
  SetLength(Result, L);
  for I := 0 to L - 1 do
    Result[I] := V[Indexes[I]];
end;

function ExtendedArrayFromIndexes(const V: ExtendedArray; const Indexes: IntegerArray): ExtendedArray;
var I, L : Integer;
begin
  L := Length(Indexes);
  SetLength(Result, L);
  for I := 0 to L - 1 do
    Result[I] := V[Indexes[I]];
end;

function StringArrayFromIndexes(const V: StringArray; const Indexes: IntegerArray): StringArray;
var I, L : Integer;
begin
  L := Length(Indexes);
  SetLength(Result, L);
  for I := 0 to L - 1 do
    Result[I] := V[Indexes[I]];
end;



{                                                                              }
{ Dynamic array sort                                                           }
{                                                                              }
procedure DynArraySort(const V: ByteArray);

  procedure QuickSort(L, R: Integer);
  var I, J, M : Integer;
      W, T    : Byte;
  begin
    repeat
      I := L;
      J := R;
      M := (L + R) shr 1;
      W := V[M];
      repeat
        while V[I] < W do
          Inc(I);
        while V[J] > W do
          Dec(J);
        if I <= J then
          begin
            T := V[I];
            V[I] := V[J];
            V[J] := T;
            if M = I then
              begin
                M := J;
                W := V[J];
              end else
              if M = J then
                begin
                  M := I;
                  W := V[I];
                end;
            Inc(I);
            Dec(J);
          end;
      until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    until I >= R;
  end;

var I : Integer;
begin
  I := Length(V);
  if I > 0 then
    QuickSort(0, I - 1);
end;

procedure DynArraySort(const V: Word16Array);

  procedure QuickSort(L, R: Integer);
  var I, J, M : Integer;
      W, T    : Word16;
  begin
    repeat
      I := L;
      J := R;
      M := (L + R) shr 1;
      W := V[M];
      repeat
        while V[I] < W do
          Inc(I);
        while V[J] > W do
          Dec(J);
        if I <= J then
          begin
            T := V[I];
            V[I] := V[J];
            V[J] := T;
            if M = I then
              begin
                M := J;
                W := V[J];
              end else
              if M = J then
                begin
                  M := I;
                  W := V[I];
                end;
            Inc(I);
            Dec(J);
          end;
      until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    until I >= R;
  end;

var I : Integer;
begin
  I := Length(V);
  if I > 0 then
    QuickSort(0, I - 1);
end;

procedure DynArraySort(const V: LongWordArray);

  procedure QuickSort(L, R: Integer);
  var I, J, M : Integer;
      W, T    : LongWord;
  begin
    repeat
      I := L;
      J := R;
      M := (L + R) shr 1;
      W := V[M];
      repeat
        while V[I] < W do
          Inc(I);
        while V[J] > W do
          Dec(J);
        if I <= J then
          begin
            T := V[I];
            V[I] := V[J];
            V[J] := T;
            if M = I then
              begin
                M := J;
                W := V[J];
              end else
              if M = J then
                begin
                  M := I;
                  W := V[I];
                end;
            Inc(I);
            Dec(J);
          end;
      until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    until I >= R;
  end;

var I : Integer;
begin
  I := Length(V);
  if I > 0 then
    QuickSort(0, I - 1);
end;

procedure DynArraySort(const V: CardinalArray);

  procedure QuickSort(L, R: Integer);
  var I, J, M : Integer;
      W, T    : Cardinal;
  begin
    repeat
      I := L;
      J := R;
      M := (L + R) shr 1;
      W := V[M];
      repeat
        while V[I] < W do
          Inc(I);
        while V[J] > W do
          Dec(J);
        if I <= J then
          begin
            T := V[I];
            V[I] := V[J];
            V[J] := T;
            if M = I then
              begin
                M := J;
                W := V[J];
              end else
              if M = J then
                begin
                  M := I;
                  W := V[I];
                end;
            Inc(I);
            Dec(J);
          end;
      until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    until I >= R;
  end;

var I : Integer;
begin
  I := Length(V);
  if I > 0 then
    QuickSort(0, I - 1);
end;

procedure DynArraySort(const V: NativeUIntArray);

  procedure QuickSort(L, R: Integer);
  var I, J, M : Integer;
      W, T    : NativeUInt;
  begin
    repeat
      I := L;
      J := R;
      M := (L + R) shr 1;
      W := V[M];
      repeat
        while V[I] < W do
          Inc(I);
        while V[J] > W do
          Dec(J);
        if I <= J then
          begin
            T := V[I];
            V[I] := V[J];
            V[J] := T;
            if M = I then
              begin
                M := J;
                W := V[J];
              end else
              if M = J then
                begin
                  M := I;
                  W := V[I];
                end;
            Inc(I);
            Dec(J);
          end;
      until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    until I >= R;
  end;

var I : Integer;
begin
  I := Length(V);
  if I > 0 then
    QuickSort(0, I - 1);
end;

procedure DynArraySort(const V: ShortIntArray);

  procedure QuickSort(L, R: Integer);
  var I, J, M : Integer;
      W, T    : ShortInt;
  begin
    repeat
      I := L;
      J := R;
      M := (L + R) shr 1;
      W := V[M];
      repeat
        while V[I] < W do
          Inc(I);
        while V[J] > W do
          Dec(J);
        if I <= J then
          begin
            T := V[I];
            V[I] := V[J];
            V[J] := T;
            if M = I then
              begin
                M := J;
                W := V[J];
              end else
              if M = J then
                begin
                  M := I;
                  W := V[I];
                end;
            Inc(I);
            Dec(J);
          end;
      until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    until I >= R;
  end;

var I : Integer;
begin
  I := Length(V);
  if I > 0 then
    QuickSort(0, I - 1);
end;

procedure DynArraySort(const V: SmallIntArray);

  procedure QuickSort(L, R: Integer);
  var I, J, M : Integer;
      W, T    : SmallInt;
  begin
    repeat
      I := L;
      J := R;
      M := (L + R) shr 1;
      W := V[M];
      repeat
        while V[I] < W do
          Inc(I);
        while V[J] > W do
          Dec(J);
        if I <= J then
          begin
            T := V[I];
            V[I] := V[J];
            V[J] := T;
            if M = I then
              begin
                M := J;
                W := V[J];
              end else
              if M = J then
                begin
                  M := I;
                  W := V[I];
                end;
            Inc(I);
            Dec(J);
          end;
      until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    until I >= R;
  end;

var I : Integer;
begin
  I := Length(V);
  if I > 0 then
    QuickSort(0, I - 1);
end;

procedure DynArraySort(const V: LongIntArray);

  procedure QuickSort(L, R: Integer);
  var I, J, M : Integer;
      W, T    : LongInt;
  begin
    repeat
      I := L;
      J := R;
      M := (L + R) shr 1;
      W := V[M];
      repeat
        while V[I] < W do
          Inc(I);
        while V[J] > W do
          Dec(J);
        if I <= J then
          begin
            T := V[I];
            V[I] := V[J];
            V[J] := T;
            if M = I then
              begin
                M := J;
                W := V[J];
              end else
              if M = J then
                begin
                  M := I;
                  W := V[I];
                end;
            Inc(I);
            Dec(J);
          end;
      until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    until I >= R;
  end;

var I : Integer;
begin
  I := Length(V);
  if I > 0 then
    QuickSort(0, I - 1);
end;

procedure DynArraySort(const V: IntegerArray);

  procedure QuickSort(L, R: Integer);
  var I, J, M : Integer;
      W, T    : Integer;
  begin
    repeat
      I := L;
      J := R;
      M := (L + R) shr 1;
      W := V[M];
      repeat
        while V[I] < W do
          Inc(I);
        while V[J] > W do
          Dec(J);
        if I <= J then
          begin
            T := V[I];
            V[I] := V[J];
            V[J] := T;
            if M = I then
              begin
                M := J;
                W := V[J];
              end else
              if M = J then
                begin
                  M := I;
                  W := V[I];
                end;
            Inc(I);
            Dec(J);
          end;
      until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    until I >= R;
  end;

var I : Integer;
begin
  I := Length(V);
  if I > 0 then
    QuickSort(0, I - 1);
end;

procedure DynArraySort(const V: Int64Array);

  procedure QuickSort(L, R: Integer);
  var I, J, M : Integer;
      W, T    : Int64;
  begin
    repeat
      I := L;
      J := R;
      M := (L + R) shr 1;
      W := V[M];
      repeat
        while V[I] < W do
          Inc(I);
        while V[J] > W do
          Dec(J);
        if I <= J then
          begin
            T := V[I];
            V[I] := V[J];
            V[J] := T;
            if M = I then
              begin
                M := J;
                W := V[J];
              end else
              if M = J then
                begin
                  M := I;
                  W := V[I];
                end;
            Inc(I);
            Dec(J);
          end;
      until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    until I >= R;
  end;

var I : Integer;
begin
  I := Length(V);
  if I > 0 then
    QuickSort(0, I - 1);
end;

procedure DynArraySort(const V: NativeIntArray);

  procedure QuickSort(L, R: Integer);
  var I, J, M : Integer;
      W, T    : NativeInt;
  begin
    repeat
      I := L;
      J := R;
      M := (L + R) shr 1;
      W := V[M];
      repeat
        while V[I] < W do
          Inc(I);
        while V[J] > W do
          Dec(J);
        if I <= J then
          begin
            T := V[I];
            V[I] := V[J];
            V[J] := T;
            if M = I then
              begin
                M := J;
                W := V[J];
              end else
              if M = J then
                begin
                  M := I;
                  W := V[I];
                end;
            Inc(I);
            Dec(J);
          end;
      until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    until I >= R;
  end;

var I : Integer;
begin
  I := Length(V);
  if I > 0 then
    QuickSort(0, I - 1);
end;

procedure DynArraySort(const V: SingleArray);

  procedure QuickSort(L, R: Integer);
  var I, J, M : Integer;
      W, T    : Single;
  begin
    repeat
      I := L;
      J := R;
      M := (L + R) shr 1;
      W := V[M];
      repeat
        while V[I] < W do
          Inc(I);
        while V[J] > W do
          Dec(J);
        if I <= J then
          begin
            T := V[I];
            V[I] := V[J];
            V[J] := T;
            if M = I then
              begin
                M := J;
                W := V[J];
              end else
              if M = J then
                begin
                  M := I;
                  W := V[I];
                end;
            Inc(I);
            Dec(J);
          end;
      until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    until I >= R;
  end;

var I : Integer;
begin
  I := Length(V);
  if I > 0 then
    QuickSort(0, I - 1);
end;

procedure DynArraySort(const V: DoubleArray);

  procedure QuickSort(L, R: Integer);
  var I, J, M : Integer;
      W, T    : Double;
  begin
    repeat
      I := L;
      J := R;
      M := (L + R) shr 1;
      W := V[M];
      repeat
        while V[I] < W do
          Inc(I);
        while V[J] > W do
          Dec(J);
        if I <= J then
          begin
            T := V[I];
            V[I] := V[J];
            V[J] := T;
            if M = I then
              begin
                M := J;
                W := V[J];
              end else
              if M = J then
                begin
                  M := I;
                  W := V[I];
                end;
            Inc(I);
            Dec(J);
          end;
      until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    until I >= R;
  end;

var I : Integer;
begin
  I := Length(V);
  if I > 0 then
    QuickSort(0, I - 1);
end;

procedure DynArraySort(const V: ExtendedArray);

  procedure QuickSort(L, R: Integer);
  var I, J, M : Integer;
      W, T    : Extended;
  begin
    repeat
      I := L;
      J := R;
      M := (L + R) shr 1;
      W := V[M];
      repeat
        while V[I] < W do
          Inc(I);
        while V[J] > W do
          Dec(J);
        if I <= J then
          begin
            T := V[I];
            V[I] := V[J];
            V[J] := T;
            if M = I then
              begin
                M := J;
                W := V[J];
              end else
              if M = J then
                begin
                  M := I;
                  W := V[I];
                end;
            Inc(I);
            Dec(J);
          end;
      until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    until I >= R;
  end;

var I : Integer;
begin
  I := Length(V);
  if I > 0 then
    QuickSort(0, I - 1);
end;

{$IFDEF SupportAnsiString}
procedure DynArraySortA(const V: AnsiStringArray);

  procedure QuickSort(L, R: Integer);
  var I, J, M : Integer;
      W, T    : AnsiString;
  begin
    repeat
      I := L;
      J := R;
      M := (L + R) shr 1;
      W := V[M];
      repeat
        while V[I] < W do
          Inc(I);
        while V[J] > W do
          Dec(J);
        if I <= J then
          begin
            T := V[I];
            V[I] := V[J];
            V[J] := T;
            if M = I then
              begin
                M := J;
                W := V[J];
              end else
              if M = J then
                begin
                  M := I;
                  W := V[I];
                end;
            Inc(I);
            Dec(J);
          end;
      until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    until I >= R;
  end;

var I : Integer;
begin
  I := Length(V);
  if I > 0 then
    QuickSort(0, I - 1);
end;

{$ENDIF}
procedure DynArraySortB(const V: RawByteStringArray);

  procedure QuickSort(L, R: Integer);
  var I, J, M : Integer;
      W, T    : RawByteString;
  begin
    repeat
      I := L;
      J := R;
      M := (L + R) shr 1;
      W := V[M];
      repeat
        while V[I] < W do
          Inc(I);
        while V[J] > W do
          Dec(J);
        if I <= J then
          begin
            T := V[I];
            V[I] := V[J];
            V[J] := T;
            if M = I then
              begin
                M := J;
                W := V[J];
              end else
              if M = J then
                begin
                  M := I;
                  W := V[I];
                end;
            Inc(I);
            Dec(J);
          end;
      until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    until I >= R;
  end;

var I : Integer;
begin
  I := Length(V);
  if I > 0 then
    QuickSort(0, I - 1);
end;

procedure DynArraySortU(const V: UnicodeStringArray);

  procedure QuickSort(L, R: Integer);
  var I, J, M : Integer;
      W, T    : UnicodeString;
  begin
    repeat
      I := L;
      J := R;
      M := (L + R) shr 1;
      W := V[M];
      repeat
        while V[I] < W do
          Inc(I);
        while V[J] > W do
          Dec(J);
        if I <= J then
          begin
            T := V[I];
            V[I] := V[J];
            V[J] := T;
            if M = I then
              begin
                M := J;
                W := V[J];
              end else
              if M = J then
                begin
                  M := I;
                  W := V[I];
                end;
            Inc(I);
            Dec(J);
          end;
      until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    until I >= R;
  end;

var I : Integer;
begin
  I := Length(V);
  if I > 0 then
    QuickSort(0, I - 1);
end;

procedure DynArraySort(const V: StringArray);

  procedure QuickSort(L, R: Integer);
  var I, J, M : Integer;
      W, T    : String;
  begin
    repeat
      I := L;
      J := R;
      M := (L + R) shr 1;
      W := V[M];
      repeat
        while V[I] < W do
          Inc(I);
        while V[J] > W do
          Dec(J);
        if I <= J then
          begin
            T := V[I];
            V[I] := V[J];
            V[J] := T;
            if M = I then
              begin
                M := J;
                W := V[J];
              end else
              if M = J then
                begin
                  M := I;
                  W := V[I];
                end;
            Inc(I);
            Dec(J);
          end;
      until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    until I >= R;
  end;

var I : Integer;
begin
  I := Length(V);
  if I > 0 then
    QuickSort(0, I - 1);
end;



procedure DynArraySort(const Key: IntegerArray; const Data: IntegerArray);

  procedure QuickSort(L, R: Integer);
  var I, J, M : Integer;
      W, T    : Integer;
      P, Q    : PInteger;
      A       : Integer;
  begin
    repeat
      I := L;
      P := @Key[I];
      J := R;
      Q := @Key[J];
      M := (L + R) shr 1;
      W := Key[M];
      repeat
        while P^ < W do
          begin
            Inc(P);
            Inc(I);
          end;
        while Q^ > W do
          begin
            Dec(Q);
            Dec(J);
          end;
        if I <= J then
          begin
            T := P^;
            P^ := Q^;
            Q^ := T;
            A := Data[I];
            Data[I] := Data[J];
            Data[J] := A;
            if M = I then
              begin
                M := J;
                W := Q^;
              end else
              if M = J then
                begin
                  M := I;
                  W := P^;
                end;
            Inc(P);
            Inc(I);
            Dec(Q);
            Dec(J);
          end;
      until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    until I >= R;
  end;

var I : Integer;
begin
  Assert(Length(Key) = Length(Data));
  I := Length(Key);
  if I > 0 then
    QuickSort(0, I - 1);
end;

procedure DynArraySort(const Key: IntegerArray; const Data: Int64Array);

  procedure QuickSort(L, R: Integer);
  var I, J, M : Integer;
      W, T    : Integer;
      P, Q    : PInteger;
      A       : Int64;
  begin
    repeat
      I := L;
      P := @Key[I];
      J := R;
      Q := @Key[J];
      M := (L + R) shr 1;
      W := Key[M];
      repeat
        while P^ < W do
          begin
            Inc(P);
            Inc(I);
          end;
        while Q^ > W do
          begin
            Dec(Q);
            Dec(J);
          end;
        if I <= J then
          begin
            T := P^;
            P^ := Q^;
            Q^ := T;
            A := Data[I];
            Data[I] := Data[J];
            Data[J] := A;
            if M = I then
              begin
                M := J;
                W := Q^;
              end else
              if M = J then
                begin
                  M := I;
                  W := P^;
                end;
            Inc(P);
            Inc(I);
            Dec(Q);
            Dec(J);
          end;
      until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    until I >= R;
  end;

var I : Integer;
begin
  Assert(Length(Key) = Length(Data));
  I := Length(Key);
  if I > 0 then
    QuickSort(0, I - 1);
end;

{$IFDEF SupportAnsiString}
procedure DynArraySort(const Key: IntegerArray; const Data: AnsiStringArray);

  procedure QuickSort(L, R: Integer);
  var I, J, M : Integer;
      W, T    : Integer;
      P, Q    : PInteger;
      A       : AnsiString;
  begin
    repeat
      I := L;
      P := @Key[I];
      J := R;
      Q := @Key[J];
      M := (L + R) shr 1;
      W := Key[M];
      repeat
        while P^ < W do
          begin
            Inc(P);
            Inc(I);
          end;
        while Q^ > W do
          begin
            Dec(Q);
            Dec(J);
          end;
        if I <= J then
          begin
            T := P^;
            P^ := Q^;
            Q^ := T;
            A := Data[I];
            Data[I] := Data[J];
            Data[J] := A;
            if M = I then
              begin
                M := J;
                W := Q^;
              end else
              if M = J then
                begin
                  M := I;
                  W := P^;
                end;
            Inc(P);
            Inc(I);
            Dec(Q);
            Dec(J);
          end;
      until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    until I >= R;
  end;

var I : Integer;
begin
  Assert(Length(Key) = Length(Data));
  I := Length(Key);
  if I > 0 then
    QuickSort(0, I - 1);
end;

{$ENDIF}
procedure DynArraySort(const Key: IntegerArray; const Data: ExtendedArray);

  procedure QuickSort(L, R: Integer);
  var I, J, M : Integer;
      W, T    : Integer;
      P, Q    : PInteger;
      A       : Extended;
  begin
    repeat
      I := L;
      P := @Key[I];
      J := R;
      Q := @Key[J];
      M := (L + R) shr 1;
      W := Key[M];
      repeat
        while P^ < W do
          begin
            Inc(P);
            Inc(I);
          end;
        while Q^ > W do
          begin
            Dec(Q);
            Dec(J);
          end;
        if I <= J then
          begin
            T := P^;
            P^ := Q^;
            Q^ := T;
            A := Data[I];
            Data[I] := Data[J];
            Data[J] := A;
            if M = I then
              begin
                M := J;
                W := Q^;
              end else
              if M = J then
                begin
                  M := I;
                  W := P^;
                end;
            Inc(P);
            Inc(I);
            Dec(Q);
            Dec(J);
          end;
      until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    until I >= R;
  end;

var I : Integer;
begin
  Assert(Length(Key) = Length(Data));
  I := Length(Key);
  if I > 0 then
    QuickSort(0, I - 1);
end;

procedure DynArraySort(const Key: IntegerArray; const Data: PointerArray);

  procedure QuickSort(L, R: Integer);
  var I, J, M : Integer;
      W, T    : Integer;
      P, Q    : PInteger;
      A       : Pointer;
  begin
    repeat
      I := L;
      P := @Key[I];
      J := R;
      Q := @Key[J];
      M := (L + R) shr 1;
      W := Key[M];
      repeat
        while P^ < W do
          begin
            Inc(P);
            Inc(I);
          end;
        while Q^ > W do
          begin
            Dec(Q);
            Dec(J);
          end;
        if I <= J then
          begin
            T := P^;
            P^ := Q^;
            Q^ := T;
            A := Data[I];
            Data[I] := Data[J];
            Data[J] := A;
            if M = I then
              begin
                M := J;
                W := Q^;
              end else
              if M = J then
                begin
                  M := I;
                  W := P^;
                end;
            Inc(P);
            Inc(I);
            Dec(Q);
            Dec(J);
          end;
      until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    until I >= R;
  end;

var I : Integer;
begin
  Assert(Length(Key) = Length(Data));
  I := Length(Key);
  if I > 0 then
    QuickSort(0, I - 1);
end;

{$IFDEF SupportAnsiString}
procedure DynArraySort(const Key: AnsiStringArray; const Data: IntegerArray);

  procedure QuickSort(L, R: Integer);
  var I, J, M : Integer;
      W, T    : AnsiString;
      P, Q    : PAnsiString;
      A       : Integer;
  begin
    repeat
      I := L;
      P := @Key[I];
      J := R;
      Q := @Key[J];
      M := (L + R) shr 1;
      W := Key[M];
      repeat
        while P^ < W do
          begin
            Inc(P);
            Inc(I);
          end;
        while Q^ > W do
          begin
            Dec(Q);
            Dec(J);
          end;
        if I <= J then
          begin
            T := P^;
            P^ := Q^;
            Q^ := T;
            A := Data[I];
            Data[I] := Data[J];
            Data[J] := A;
            if M = I then
              begin
                M := J;
                W := Q^;
              end else
              if M = J then
                begin
                  M := I;
                  W := P^;
                end;
            Inc(P);
            Inc(I);
            Dec(Q);
            Dec(J);
          end;
      until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    until I >= R;
  end;

var I : Integer;
begin
  Assert(Length(Key) = Length(Data));
  I := Length(Key);
  if I > 0 then
    QuickSort(0, I - 1);
end;

procedure DynArraySort(const Key: AnsiStringArray; const Data: Int64Array);

  procedure QuickSort(L, R: Integer);
  var I, J, M : Integer;
      W, T    : AnsiString;
      P, Q    : PAnsiString;
      A       : Int64;
  begin
    repeat
      I := L;
      P := @Key[I];
      J := R;
      Q := @Key[J];
      M := (L + R) shr 1;
      W := Key[M];
      repeat
        while P^ < W do
          begin
            Inc(P);
            Inc(I);
          end;
        while Q^ > W do
          begin
            Dec(Q);
            Dec(J);
          end;
        if I <= J then
          begin
            T := P^;
            P^ := Q^;
            Q^ := T;
            A := Data[I];
            Data[I] := Data[J];
            Data[J] := A;
            if M = I then
              begin
                M := J;
                W := Q^;
              end else
              if M = J then
                begin
                  M := I;
                  W := P^;
                end;
            Inc(P);
            Inc(I);
            Dec(Q);
            Dec(J);
          end;
      until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    until I >= R;
  end;

var I : Integer;
begin
  Assert(Length(Key) = Length(Data));
  I := Length(Key);
  if I > 0 then
    QuickSort(0, I - 1);
end;

procedure DynArraySort(const Key: AnsiStringArray; const Data: AnsiStringArray);

  procedure QuickSort(L, R: Integer);
  var I, J, M : Integer;
      W, T    : AnsiString;
      P, Q    : PAnsiString;
      A       : AnsiString;
  begin
    repeat
      I := L;
      P := @Key[I];
      J := R;
      Q := @Key[J];
      M := (L + R) shr 1;
      W := Key[M];
      repeat
        while P^ < W do
          begin
            Inc(P);
            Inc(I);
          end;
        while Q^ > W do
          begin
            Dec(Q);
            Dec(J);
          end;
        if I <= J then
          begin
            T := P^;
            P^ := Q^;
            Q^ := T;
            A := Data[I];
            Data[I] := Data[J];
            Data[J] := A;
            if M = I then
              begin
                M := J;
                W := Q^;
              end else
              if M = J then
                begin
                  M := I;
                  W := P^;
                end;
            Inc(P);
            Inc(I);
            Dec(Q);
            Dec(J);
          end;
      until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    until I >= R;
  end;

var I : Integer;
begin
  Assert(Length(Key) = Length(Data));
  I := Length(Key);
  if I > 0 then
    QuickSort(0, I - 1);
end;

procedure DynArraySort(const Key: AnsiStringArray; const Data: ExtendedArray);

  procedure QuickSort(L, R: Integer);
  var I, J, M : Integer;
      W, T    : AnsiString;
      P, Q    : PAnsiString;
      A       : Extended;
  begin
    repeat
      I := L;
      P := @Key[I];
      J := R;
      Q := @Key[J];
      M := (L + R) shr 1;
      W := Key[M];
      repeat
        while P^ < W do
          begin
            Inc(P);
            Inc(I);
          end;
        while Q^ > W do
          begin
            Dec(Q);
            Dec(J);
          end;
        if I <= J then
          begin
            T := P^;
            P^ := Q^;
            Q^ := T;
            A := Data[I];
            Data[I] := Data[J];
            Data[J] := A;
            if M = I then
              begin
                M := J;
                W := Q^;
              end else
              if M = J then
                begin
                  M := I;
                  W := P^;
                end;
            Inc(P);
            Inc(I);
            Dec(Q);
            Dec(J);
          end;
      until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    until I >= R;
  end;

var I : Integer;
begin
  Assert(Length(Key) = Length(Data));
  I := Length(Key);
  if I > 0 then
    QuickSort(0, I - 1);
end;

procedure DynArraySort(const Key: AnsiStringArray; const Data: PointerArray);

  procedure QuickSort(L, R: Integer);
  var I, J, M : Integer;
      W, T    : AnsiString;
      P, Q    : PAnsiString;
      A       : Pointer;
  begin
    repeat
      I := L;
      P := @Key[I];
      J := R;
      Q := @Key[J];
      M := (L + R) shr 1;
      W := Key[M];
      repeat
        while P^ < W do
          begin
            Inc(P);
            Inc(I);
          end;
        while Q^ > W do
          begin
            Dec(Q);
            Dec(J);
          end;
        if I <= J then
          begin
            T := P^;
            P^ := Q^;
            Q^ := T;
            A := Data[I];
            Data[I] := Data[J];
            Data[J] := A;
            if M = I then
              begin
                M := J;
                W := Q^;
              end else
              if M = J then
                begin
                  M := I;
                  W := P^;
                end;
            Inc(P);
            Inc(I);
            Dec(Q);
            Dec(J);
          end;
      until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    until I >= R;
  end;

var I : Integer;
begin
  Assert(Length(Key) = Length(Data));
  I := Length(Key);
  if I > 0 then
    QuickSort(0, I - 1);
end;

{$ENDIF}
procedure DynArraySort(const Key: ExtendedArray; const Data: IntegerArray);

  procedure QuickSort(L, R: Integer);
  var I, J, M : Integer;
      W, T    : Extended;
      P, Q    : PExtended;
      A       : Integer;
  begin
    repeat
      I := L;
      P := @Key[I];
      J := R;
      Q := @Key[J];
      M := (L + R) shr 1;
      W := Key[M];
      repeat
        while P^ < W do
          begin
            Inc(P);
            Inc(I);
          end;
        while Q^ > W do
          begin
            Dec(Q);
            Dec(J);
          end;
        if I <= J then
          begin
            T := P^;
            P^ := Q^;
            Q^ := T;
            A := Data[I];
            Data[I] := Data[J];
            Data[J] := A;
            if M = I then
              begin
                M := J;
                W := Q^;
              end else
              if M = J then
                begin
                  M := I;
                  W := P^;
                end;
            Inc(P);
            Inc(I);
            Dec(Q);
            Dec(J);
          end;
      until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    until I >= R;
  end;

var I : Integer;
begin
  Assert(Length(Key) = Length(Data));
  I := Length(Key);
  if I > 0 then
    QuickSort(0, I - 1);
end;

procedure DynArraySort(const Key: ExtendedArray; const Data: Int64Array);

  procedure QuickSort(L, R: Integer);
  var I, J, M : Integer;
      W, T    : Extended;
      P, Q    : PExtended;
      A       : Int64;
  begin
    repeat
      I := L;
      P := @Key[I];
      J := R;
      Q := @Key[J];
      M := (L + R) shr 1;
      W := Key[M];
      repeat
        while P^ < W do
          begin
            Inc(P);
            Inc(I);
          end;
        while Q^ > W do
          begin
            Dec(Q);
            Dec(J);
          end;
        if I <= J then
          begin
            T := P^;
            P^ := Q^;
            Q^ := T;
            A := Data[I];
            Data[I] := Data[J];
            Data[J] := A;
            if M = I then
              begin
                M := J;
                W := Q^;
              end else
              if M = J then
                begin
                  M := I;
                  W := P^;
                end;
            Inc(P);
            Inc(I);
            Dec(Q);
            Dec(J);
          end;
      until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    until I >= R;
  end;

var I : Integer;
begin
  Assert(Length(Key) = Length(Data));
  I := Length(Key);
  if I > 0 then
    QuickSort(0, I - 1);
end;

{$IFDEF SupportAnsiString}
procedure DynArraySort(const Key: ExtendedArray; const Data: AnsiStringArray);

  procedure QuickSort(L, R: Integer);
  var I, J, M : Integer;
      W, T    : Extended;
      P, Q    : PExtended;
      A       : AnsiString;
  begin
    repeat
      I := L;
      P := @Key[I];
      J := R;
      Q := @Key[J];
      M := (L + R) shr 1;
      W := Key[M];
      repeat
        while P^ < W do
          begin
            Inc(P);
            Inc(I);
          end;
        while Q^ > W do
          begin
            Dec(Q);
            Dec(J);
          end;
        if I <= J then
          begin
            T := P^;
            P^ := Q^;
            Q^ := T;
            A := Data[I];
            Data[I] := Data[J];
            Data[J] := A;
            if M = I then
              begin
                M := J;
                W := Q^;
              end else
              if M = J then
                begin
                  M := I;
                  W := P^;
                end;
            Inc(P);
            Inc(I);
            Dec(Q);
            Dec(J);
          end;
      until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    until I >= R;
  end;

var I : Integer;
begin
  Assert(Length(Key) = Length(Data));
  I := Length(Key);
  if I > 0 then
    QuickSort(0, I - 1);
end;

{$ENDIF}
procedure DynArraySort(const Key: ExtendedArray; const Data: ExtendedArray);

  procedure QuickSort(L, R: Integer);
  var I, J, M : Integer;
      W, T    : Extended;
      P, Q    : PExtended;
      A       : Extended;
  begin
    repeat
      I := L;
      P := @Key[I];
      J := R;
      Q := @Key[J];
      M := (L + R) shr 1;
      W := Key[M];
      repeat
        while P^ < W do
          begin
            Inc(P);
            Inc(I);
          end;
        while Q^ > W do
          begin
            Dec(Q);
            Dec(J);
          end;
        if I <= J then
          begin
            T := P^;
            P^ := Q^;
            Q^ := T;
            A := Data[I];
            Data[I] := Data[J];
            Data[J] := A;
            if M = I then
              begin
                M := J;
                W := Q^;
              end else
              if M = J then
                begin
                  M := I;
                  W := P^;
                end;
            Inc(P);
            Inc(I);
            Dec(Q);
            Dec(J);
          end;
      until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    until I >= R;
  end;

var I : Integer;
begin
  Assert(Length(Key) = Length(Data));
  I := Length(Key);
  if I > 0 then
    QuickSort(0, I - 1);
end;

procedure DynArraySort(const Key: ExtendedArray; const Data: PointerArray);

  procedure QuickSort(L, R: Integer);
  var I, J, M : Integer;
      W, T    : Extended;
      P, Q    : PExtended;
      A       : Pointer;
  begin
    repeat
      I := L;
      P := @Key[I];
      J := R;
      Q := @Key[J];
      M := (L + R) shr 1;
      W := Key[M];
      repeat
        while P^ < W do
          begin
            Inc(P);
            Inc(I);
          end;
        while Q^ > W do
          begin
            Dec(Q);
            Dec(J);
          end;
        if I <= J then
          begin
            T := P^;
            P^ := Q^;
            Q^ := T;
            A := Data[I];
            Data[I] := Data[J];
            Data[J] := A;
            if M = I then
              begin
                M := J;
                W := Q^;
              end else
              if M = J then
                begin
                  M := I;
                  W := P^;
                end;
            Inc(P);
            Inc(I);
            Dec(Q);
            Dec(J);
          end;
      until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    until I >= R;
  end;

var I : Integer;
begin
  Assert(Length(Key) = Length(Data));
  I := Length(Key);
  if I > 0 then
    QuickSort(0, I - 1);
end;




{                                                                              }
{ Test cases                                                                   }
{                                                                              }
{$IFDEF DYNARRAYS_TEST}
{$ASSERTIONS ON}
procedure Test_IntegerArray;
var S, T : IntegerArray;
    F    : Integer;
begin
  S := nil;
  for F := 1 to 100 do
    begin
      DynArrayAppend(S, F);
      Assert(Length(S) = F,                 'Append');
      Assert(S[F - 1] = F,                  'Append');
    end;

  T := Copy(S);
  DynArrayAppendIntegerArray(S, T);
  for F := 1 to 100 do
    Assert(S[F + 99] = F,                   'Append');
  Assert(DynArrayPosNext(60, S) = 59,               'PosNext');
  Assert(DynArrayPosNext(60, T) = 59,               'PosNext');
  Assert(DynArrayPosNext(60, S, 59) = 159,          'PosNext');
  Assert(DynArrayPosNext(60, T, 59) = -1,           'PosNext');
  Assert(DynArrayPosNext(60, T, -1, True) = 59,     'PosNext');
  Assert(DynArrayPosNext(60, T, 59, True) = -1,     'PosNext');

  for F := 1 to 100 do
    begin
      DynArrayRemove(S, DynArrayPosNext(F, S), 1);
      Assert(Length(S) = 200 - F,           'Remove');
    end;
  for F := 99 downto 0 do
    begin
      DynArrayRemove(S, DynArrayPosNext(F xor 3 + 1, S), 1);
      Assert(Length(S) = F,                 'Remove');
    end;

  S := AsIntegerArray([3, 1, 2, 5, 4]);
  DynArraySort(S);
  Assert(S[0] = 1, 'Sort');
  Assert(S[1] = 2, 'Sort');
  Assert(S[2] = 3, 'Sort');
  Assert(S[3] = 4, 'Sort');
  Assert(S[4] = 5, 'Sort');

  S := AsIntegerArray([3, 5, 5, 2, 5, 5, 1]);
  DynArraySort(S);
  Assert(S[0] = 1, 'Sort');
  Assert(S[1] = 2, 'Sort');
  Assert(S[2] = 3, 'Sort');
  Assert(S[3] = 5, 'Sort');
  Assert(S[4] = 5, 'Sort');
  Assert(S[5] = 5, 'Sort');
  Assert(S[6] = 5, 'Sort');

  SetLength(S, 1000);
  for F := 0 to 999 do
    S[F] := F mod 5;
  DynArraySort(S);
  for F := 0 to 999 do
    Assert(S[F] = F div 200, 'Sort');

  S := AsIntegerArray([6, 3, 5, 1]);
  T := AsIntegerArray([1, 2, 3, 4]);
  DynArraySort(S, T);
  Assert(S[0] = 1, 'Sort');
  Assert(S[1] = 3, 'Sort');
  Assert(S[2] = 5, 'Sort');
  Assert(S[3] = 6, 'Sort');
  Assert(T[0] = 4, 'Sort');
  Assert(T[1] = 2, 'Sort');
  Assert(T[2] = 3, 'Sort');
  Assert(T[3] = 1, 'Sort');
end;

procedure Test_ObjectArray;
var S, T : ObjectArray;
    F    : Integer;
    V    : TObject;
begin
  S := nil;
  V := TObject.Create;
  try
    for F := 1 to 100 do
      begin
        DynArrayAppend(S, V);
        Assert(Length(S) = F,            'Append');
        Assert(S[F - 1] = V,             'Append');
      end;
    T := Copy(S);
    for F := 1 to 10 do
      begin
        DynArrayRemove(S, F - 1, 1, False);
        Assert(Length(S) = 100 - F,      'Remove');
      end;
    DynArrayRemove(S, 89, 1, False);
    Assert(Length(S) = 89,               'Remove');
    DynArrayRemove(S, 87, 1, False);
    Assert(Length(S) = 88,               'Remove');
    DynArrayAppendObjectArray(S, T);
    Assert(Length(S) = 188,              'AppendObjectArray');
    DynArrayRemove(S, 10, 88, False);
    Assert(Length(S) = 100,              'Remove');
    DynArrayRemove(S, 0, 100, False);
    Assert(Length(S) = 0,                'Remove');
  finally
    V.Free;
  end;
end;

procedure Test_StringArray;
var S, T : StringArray;
    U    : String;
    F    : Integer;
begin
  S := nil;
  for F := 1 to 100 do
    begin
      U := IntToStr(F);
      DynArrayAppend(S, U);
      Assert(Length(S) = F,                 'Append');
      Assert(S[F - 1] = U,                  'Append');
    end;

  T := Copy(S);
  DynArrayAppendStringArray(S, T);
  for F := 1 to 100 do
    Assert(S[F + 99] = IntToStr(F),         'Append');
  Assert(DynArrayPosNext('60', S) = 59,               'PosNext');
  Assert(DynArrayPosNext('60', T) = 59,               'PosNext');
  Assert(DynArrayPosNext('60', S, 59) = 159,          'PosNext');
  Assert(DynArrayPosNext('60', T, 59) = -1,           'PosNext');
  Assert(DynArrayPosNext('60', T, -1, True) = 59,     'PosNext');
  Assert(DynArrayPosNext('60', T, 59, True) = -1,     'PosNext');

  for F := 1 to 100 do
    begin
      DynArrayRemove(S, DynArrayPosNext(IntToStr(F), S), 1);
      Assert(Length(S) = 200 - F,           'Remove');
    end;
  for F := 99 downto 0 do
    begin
      DynArrayRemove(S, DynArrayPosNext(IntToStr(F xor 3 + 1), S), 1);
      Assert(Length(S) = F,                 'Remove');
    end;

  S := AsStringArray(['3', '1', '2', '5', '4']);
  DynArraySort(S);
  Assert(S[0] = '1', 'Sort');
  Assert(S[1] = '2', 'Sort');
  Assert(S[2] = '3', 'Sort');
  Assert(S[3] = '4', 'Sort');
  Assert(S[4] = '5', 'Sort');

  S := AsStringArray(['3', '5', '5', '2', '5', '5', '1']);
  DynArraySort(S);
  Assert(S[0] = '1', 'Sort');
  Assert(S[1] = '2', 'Sort');
  Assert(S[2] = '3', 'Sort');
  Assert(S[3] = '5', 'Sort');
  Assert(S[4] = '5', 'Sort');
  Assert(S[5] = '5', 'Sort');
  Assert(S[6] = '5', 'Sort');

  SetLength(S, 1000);
  for F := 0 to 999 do
    S[F] := IntToStr(F mod 5);
  DynArraySort(S);
  for F := 0 to 999 do
    Assert(S[F] = IntToStr(F div 200), 'Sort');
end;

procedure Test;
begin
  Test_IntegerArray;
  Test_ObjectArray;
  Test_StringArray;
end;
{$ENDIF}



end.

