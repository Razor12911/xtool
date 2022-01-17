{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals 5.00                                        }
{   File name:        flcDataStructArrays.pas                                  }
{   File version:     5.32                                                     }
{   Description:      Data structures: Arrays                                  }
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
{ Description:                                                                 }
{                                                                              }
{   Array classes for various item types.                                      }
{                                                                              }
{ Revision history:                                                            }
{                                                                              }
{   1999/11/12  0.01  Initial development.                                     }
{   2000/02/08  1.02  Initial version. AArray, TArray..                        }
{   2000/06/07  1.03  Base classes (AIntegerArray).                            }
{   2000/06/08  1.04  Added AObjectArray.                                      }
{   2000/06/03  1.05  Added AArray, AIntegerArray, AExtendedArray,             }
{                     AStringArray with some implementations.                  }
{   2000/06/06  1.06  Added AInt64Array.                                       }
{   2000/06/08  1.07  Added TObjectArray.                                      }
{   2000/06/14  1.08  Converted cDataStructs to template.                      }
{   2001/07/15  1.09  Changed memory arrays to pre-allocate when growing.      }
{   2002/05/15  3.10  Created cArrays unit from cDataStructs.                  }
{                     Refactored for Fundamentals 3.                           }
{   2002/09/30  3.11  Moved stream array classes to unit cStreamArrays.        }
{   2003/03/08  3.12  Renamed Add methods to Append.                           }
{   2003/05/26  3.13  Added Remove methods to object array.                    }
{   2003/09/11  3.14  Added TInterfaceArray.                                   }
{   2004/01/02  3.15  Bug fixed in TStringArray.SetAsString by Eb.             }
{   2004/01/18  3.16  Added TWideStringArray.                                  }
{   2004/07/24  3.17  Fixed bug in Sort with duplicate values. Thanks to Eb    }
{                     and others for reporting it.                             }
{   2007/09/27  4.18  Merged into single unit for Fundamentals 4.              }
{   2012/04/11  4.19  Unicode string changes.                                  }
{   2012/09/01  4.20  Unicode string changes.                                  }
{   2015/03/13  4.21  RawByteString support.                                   }
{   2016/01/16  5.22  Revised for Fundamentals 5.                              }
{   2018/07/17  5.23  Int32/Word32 arrays.                                     }
{   2018/08/12  5.24  String type changes.                                     }
{   2019/04/02  5.25  Integer/Cardinal array changes.                          }
{   2020/03/22  5.26  Rename parameters to avoid conflict with properties.     }
{   2020/03/31  5.27  Integer array changes.                                   }
{   2020/06/02  5.28  UInt64 changes.                                          }
{   2020/07/02  5.29  Split arrays into separate unit.                         }
{   2020/07/03  5.30  Factor out methods from base class to concrete classes.  }
{                     Remove unused types and define equivalent types.         }
{   2020/07/05  5.31  Move bit array into seperate unit.                       }
{                     Remove dependencies on units flcDynArrays, flcStrings.   }
{   2020/07/07  5.32  Refactor and remove dependency on unit flcUtils.         }
{                     Added TByteArray.                                        }
{                                                                              }
{ Supported compilers:                                                         }
{                                                                              }
{   Delphi 2010-10.4 Win32/Win64        5.29  2020/06/02                       }
{   Delphi 10.2-10.4 Linux64            5.29  2020/06/02                       }
{   FreePascal 3.0.4 Win64              5.29  2020/06/02                       }
{                                                                              }
{******************************************************************************}

{$INCLUDE ..\flcInclude.inc}

{$IFDEF DEBUG}
{$IFDEF TEST}
  {$DEFINE ARRAY_TEST}
{$ENDIF}
{$ENDIF}

{$IFDEF FREEPASCAL}
  {$WARNINGS OFF}
  {$HINTS OFF}
{$ENDIF}

unit flcDataStructArrays;

interface

uses
  { System }
  SysUtils,

  { Fundamentals }
  flcStdTypes;



{                                                                              }
{ EArrayError                                                                  }
{   Exception raised by array objects.                                         }
{                                                                              }
type
  EArrayError = class(Exception);



{                                                                              }
{ TArrayBase                                                                   }
{   Base class for an array class.                                             }
{                                                                              }
type
  TArrayBase = class
  end;

  TArrayBaseClass = class of TArrayBase;



{                                                                              }
{ TObjectArray                                                                 }
{   An TObjectArray implemented using a dynamic array.                         }
{                                                                              }
type
  TObjectArray = class(TArrayBase)
  protected
    FData        : ObjectArray;
    FCapacity    : NativeInt;
    FCount       : NativeInt;
    FIsItemOwner : Boolean;

    procedure Init; virtual;

    procedure FreeItems;

    procedure SetData(const AData: ObjectArray); virtual;

    procedure SetCount(const ANewCount: NativeInt);

    function  GetItem(const AIdx: NativeInt): TObject;                {$IFDEF UseInline}inline;{$ENDIF}
    procedure SetItem(const AIdx: NativeInt; const AValue: TObject);  {$IFDEF UseInline}inline;{$ENDIF}

    function  GetTailItem: TObject;

    function  CompareItems(const AIdx1, AIdx2: NativeInt): Int32; virtual;

  public
    class function CreateInstance(const AIsItemOwner: Boolean = False): TObjectArray; virtual;

    constructor Create(
                const AIsItemOwner: Boolean = False); overload; virtual;
    constructor Create(
                const AData: ObjectArray = nil;
                const AIsItemOwner: Boolean = False); overload;

    destructor Destroy; override;

    property  Data: ObjectArray read FData write SetData;
    property  IsItemOwner: Boolean read FIsItemOwner;

    procedure Clear;

    procedure Assign(const ASource: TObjectArray);

    function  Duplicate: TObjectArray;

    function  IsEqual(const V: TObjectArray): Boolean;

    property  Count: NativeInt read FCount write SetCount;
    property  Item[const AIdx: NativeInt]: TObject read GetItem write SetItem; default;

    property  TailItem: TObject read GetTailItem;

    function  PosNext(const AItem: TObject; const APrevPos: NativeInt): NativeInt; overload;
    function  PosNext(var AItem: TObject; const AClassType: TClass; const APrevPos: NativeInt = -1): NativeInt; overload;
    function  PosNext(var AItem: TObject; const AClassName: String; const APrevPos: NativeInt = -1): NativeInt; overload;

    function  GetIndex(const AValue: TObject): NativeInt;
    function  HasValue(const AValue: TObject): Boolean;

    function  Add(const AValue: TObject): NativeInt;
    function  AddIfNotExists(const AValue: TObject): NativeInt;

    function  AddArray(const AArray: ObjectArray): NativeInt; overload;
    function  AddArray(const AArray: TObjectArray): NativeInt; overload;

    procedure Insert(const AIdx: NativeInt; const ACount: NativeInt = 1);

    procedure Delete(const AIdx: NativeInt; const ACount: NativeInt = 1);

    function  ReleaseItem(const AIdx: NativeInt): TObject;
    function  ReleaseValue(const AValue: TObject): Boolean;

    function  RemoveItem(const AIdx: NativeInt): TObject;
    function  RemoveValue(const AValue: TObject): Boolean;

    function  DeleteValue(const AValue: TObject): Boolean;
    function  DeleteAll(const AValue: TObject): NativeInt;

    procedure Sort;

    function  GetRange(const ALoIdx, AHiIdx: NativeInt): ObjectArray;
    procedure SetRange(const ALoIdx, AHiIdx: NativeInt; const V: ObjectArray);
  end;



{                                                                              }
{ TInt32Array                                                                  }
{   An TInt32Array implemented using a dynamic array.                          }
{                                                                              }
type
  TInt32Array = class(TArrayBase)
  protected
    FData     : Int32Array;
    FCapacity : NativeInt;
    FCount    : NativeInt;

    procedure SetData(const AData: Int32Array); virtual;

    procedure SetCount(const ANewCount: NativeInt);

    function  GetItem(const AIdx: NativeInt): Int32;                {$IFDEF UseInline}inline;{$ENDIF}
    procedure SetItem(const AIdx: NativeInt; const AValue: Int32);  {$IFDEF UseInline}inline;{$ENDIF}

    function  CompareItems(const AIdx1, AIdx2: NativeInt): Int32; virtual;

    function  GetItemAsString(const AIdx: NativeInt): String;
    procedure SetItemAsString(const AIdx: NativeInt; const AValue: String);

    function  GetAsString: String;
    procedure SetAsString(const S: String);

  public
    class function CreateInstance: TInt32Array; virtual;

    constructor Create; overload; virtual;
    constructor Create(const V: Int32Array); overload;

    property  Data: Int32Array read FData write SetData;

    procedure Clear;

    procedure Assign(const ASource: TInt32Array); overload;
    procedure Assign(const ASource: Int32Array); overload;
    procedure Assign(const ASource: Array of Int32); overload;

    function  Duplicate: TInt32Array;

    function  IsEqual(const V: TInt32Array): Boolean;

    property  Count: NativeInt read FCount write SetCount;
    property  Item[const AIdx: NativeInt]: Int32 read GetItem write SetItem; default;

    function  PosNext(
              const AItem: Int32;
              const APrevPos: NativeInt = -1;
              const IsSortedAscending: Boolean = False): NativeInt;

    function  GetIndex(const AValue: Int32): NativeInt;
    function  HasValue(const AValue: Int32): Boolean;

    function  Add(const AValue: Int32): NativeInt;
    function  AddIfNotExists(const AValue: Int32): NativeInt;

    function  AddArray(const AArray: Int32Array): NativeInt; overload;
    function  AddArray(const AArray: TInt32Array): NativeInt; overload;

    procedure Insert(const AIdx: NativeInt; const ACount: NativeInt = 1);

    procedure Delete(const AIdx: NativeInt; const ACount: NativeInt = 1);

    procedure Sort;

    function  GetRange(const ALoIdx, AHiIdx: NativeInt): Int32Array;
    procedure SetRange(const ALoIdx, AHiIdx: NativeInt; const V: Int32Array);

    procedure Fill(const AIdx, ACount: NativeInt; const AValue: Int32);

    property  ItemAsString[const AIdx: NativeInt]: String read GetItemAsString write SetItemAsString;
    property  AsString: String read GetAsString write SetAsString;
  end;



{                                                                              }
{ TInt64Array                                                                  }
{   An TInt64Array implemented using a dynamic array.                          }
{                                                                              }
type
  TInt64Array = class(TArrayBase)
  protected
    FData     : Int64Array;
    FCapacity : NativeInt;
    FCount    : NativeInt;

    procedure SetData(const AData: Int64Array); virtual;

    procedure SetCount(const ANewCount: NativeInt);

    function  GetItem(const AIdx: NativeInt): Int64;                {$IFDEF UseInline}inline;{$ENDIF}
    procedure SetItem(const AIdx: NativeInt; const AValue: Int64);  {$IFDEF UseInline}inline;{$ENDIF}

    function  CompareItems(const AIdx1, AIdx2: NativeInt): Int32; virtual;

    function  GetItemAsString(const AIdx: NativeInt): String;
    procedure SetItemAsString(const AIdx: NativeInt; const AValue: String);

    function  GetAsString: String;
    procedure SetAsString(const S: String);

  public
    class function CreateInstance: TInt64Array; virtual;

    constructor Create; overload; virtual;
    constructor Create(const V: Int64Array); overload;

    property  Data: Int64Array read FData write SetData;

    procedure Clear;

    procedure Assign(const ASource: TInt64Array); overload;
    procedure Assign(const ASource: Int64Array); overload;
    procedure Assign(const ASource: Array of Int64); overload;

    function  Duplicate: TInt64Array;

    function  IsEqual(const V: TInt64Array): Boolean;

    property  Count: NativeInt read FCount write SetCount;
    property  Item[const AIdx: NativeInt]: Int64 read GetItem write SetItem; default;

    function  PosNext(
              const AItem: Int64;
              const APrevPos: NativeInt = -1;
              const IsSortedAscending: Boolean = False): NativeInt;

    function  GetIndex(const AValue: Int64): NativeInt;
    function  HasValue(const AValue: Int64): Boolean;

    function  Add(const AValue: Int64): NativeInt;
    function  AddIfNotExists(const AValue: Int64): NativeInt;

    function  AddArray(const AArray: Int64Array): NativeInt; overload;
    function  AddArray(const AArray: TInt64Array): NativeInt; overload;

    procedure Insert(const AIdx: NativeInt; const ACount: NativeInt = 1);

    procedure Delete(const AIdx: NativeInt; const ACount: NativeInt = 1);

    procedure Sort;

    function  GetRange(const ALoIdx, AHiIdx: NativeInt): Int64Array;
    procedure SetRange(const ALoIdx, AHiIdx: NativeInt; const V: Int64Array);

    procedure Fill(const AIdx, ACount: NativeInt; const AValue: Int64);

    property  ItemAsString[const AIdx: NativeInt]: String read GetItemAsString write SetItemAsString;
    property  AsString: String read GetAsString write SetAsString;
  end;



{                                                                              }
{ Equivalent Integer types                                                     }
{                                                                              }
{$IFDEF LongIntIs32Bits}
type
  TLongIntArray = TInt32Array;
{$ELSE}{$IFDEF LongIntIs64Bits}
type
  TLongIntArray = TInt64Array;
{$ENDIF}{$ENDIF}

type
  TIntegerArray = TInt32Array;

{$IFDEF NativeIntIs32Bits}
type
  TNativeIntArray = TInt32Array;
{$ELSE}{$IFDEF NativeIntIs64Bits}
type
  TNativeIntArray = TInt64Array;
{$ENDIF}{$ENDIF}

type
  TIntArray = TInt64Array;



{                                                                              }
{ TByteArray                                                                   }
{   An TByteArray implemented using a dynamic array.                           }
{                                                                              }
type
  TByteArray = class(TArrayBase)
  protected
    FData     : ByteArray;
    FCapacity : NativeInt;
    FCount    : NativeInt;

    procedure SetData(const AData: ByteArray); virtual;

    procedure SetCount(const ANewCount: NativeInt);

    function  GetItem(const AIdx: NativeInt): Byte;                {$IFDEF UseInline}inline;{$ENDIF}
    procedure SetItem(const AIdx: NativeInt; const AValue: Byte);  {$IFDEF UseInline}inline;{$ENDIF}

    function  CompareItems(const AIdx1, AIdx2: NativeInt): Int32; virtual;

    function  GetItemAsString(const AIdx: NativeInt): String;
    procedure SetItemAsString(const AIdx: NativeInt; const AValue: String);

    function  GetAsString: String;
    procedure SetAsString(const S: String);

  public
    class function CreateInstance: TByteArray; virtual;

    constructor Create; overload; virtual;
    constructor Create(const V: ByteArray); overload;

    property  Data: ByteArray read FData write SetData;

    procedure Clear;

    procedure Assign(const ASource: TByteArray); overload;
    procedure Assign(const ASource: ByteArray); overload;
    procedure Assign(const ASource: Array of Byte); overload;

    function  Duplicate: TByteArray;

    function  IsEqual(const V: TByteArray): Boolean;

    property  Count: NativeInt read FCount write SetCount;
    property  Item[const AIdx: NativeInt]: Byte read GetItem write SetItem; default;

    function  PosNext(
              const AItem: Byte;
              const APrevPos: NativeInt = -1;
              const IsSortedAscending: Boolean = False): NativeInt;

    function  GetIndex(const AValue: Byte): NativeInt;
    function  HasValue(const AValue: Byte): Boolean;

    function  Add(const AValue: Byte): NativeInt;
    function  AddIfNotExists(const AValue: Byte): NativeInt;

    function  AddArray(const AArray: ByteArray): NativeInt; overload;
    function  AddArray(const AArray: TByteArray): NativeInt; overload;

    procedure Insert(const AIdx: NativeInt; const ACount: NativeInt = 1);

    procedure Delete(const AIdx: NativeInt; const ACount: NativeInt = 1);

    procedure Sort;

    function  GetRange(const ALoIdx, AHiIdx: NativeInt): ByteArray;
    procedure SetRange(const ALoIdx, AHiIdx: NativeInt; const V: ByteArray);

    procedure Fill(const AIdx, ACount: NativeInt; const AValue: Byte);

    property  ItemAsString[const AIdx: NativeInt]: String read GetItemAsString write SetItemAsString;
    property  AsString: String read GetAsString write SetAsString;
  end;



{                                                                              }
{ TWord32Array                                                                 }
{   An TWord32Array implemented using a dynamic array.                         }
{                                                                              }
type
  TWord32Array = class(TArrayBase)
  protected
    FData     : Word32Array;
    FCapacity : NativeInt;
    FCount    : NativeInt;

    procedure SetData(const AData: Word32Array); virtual;

    procedure SetCount(const ANewCount: NativeInt);

    function  GetItem(const AIdx: NativeInt): Word32;                {$IFDEF UseInline}inline;{$ENDIF}
    procedure SetItem(const AIdx: NativeInt; const AValue: Word32);  {$IFDEF UseInline}inline;{$ENDIF}

    function  CompareItems(const AIdx1, AIdx2: NativeInt): Int32; virtual;

    function  GetItemAsString(const AIdx: NativeInt): String;
    procedure SetItemAsString(const AIdx: NativeInt; const AValue: String);

    function  GetAsString: String;
    procedure SetAsString(const S: String);

  public
    class function CreateInstance: TWord32Array; virtual;

    constructor Create; overload; virtual;
    constructor Create(const V: Word32Array); overload;

    property  Data: Word32Array read FData write SetData;

    procedure Clear;

    procedure Assign(const ASource: TWord32Array); overload;
    procedure Assign(const ASource: Word32Array); overload;
    procedure Assign(const ASource: Array of Word32); overload;

    function  Duplicate: TWord32Array;

    function  IsEqual(const V: TWord32Array): Boolean;

    property  Count: NativeInt read FCount write SetCount;
    property  Item[const AIdx: NativeInt]: Word32 read GetItem write SetItem; default;

    function  PosNext(
              const AItem: Word32;
              const APrevPos: NativeInt = -1;
              const IsSortedAscending: Boolean = False): NativeInt;

    function  GetIndex(const AValue: Word32): NativeInt;
    function  HasValue(const AValue: Word32): Boolean;

    function  Add(const AValue: Word32): NativeInt;
    function  AddIfNotExists(const AValue: Word32): NativeInt;

    function  AddArray(const AArray: Word32Array): NativeInt; overload;
    function  AddArray(const AArray: TWord32Array): NativeInt; overload;

    procedure Insert(const AIdx: NativeInt; const ACount: NativeInt = 1);

    procedure Delete(const AIdx: NativeInt; const ACount: NativeInt = 1);

    procedure Sort;

    function  GetRange(const ALoIdx, AHiIdx: NativeInt): Word32Array;
    procedure SetRange(const ALoIdx, AHiIdx: NativeInt; const V: Word32Array);

    procedure Fill(const AIdx, ACount: NativeInt; const AValue: Word32);

    property  ItemAsString[const AIdx: NativeInt]: String read GetItemAsString write SetItemAsString;
    property  AsString: String read GetAsString write SetAsString;
  end;



{                                                                              }
{ TWord64Array                                                                 }
{   An TWord64Array implemented using a dynamic array.                         }
{                                                                              }
type
  TWord64Array = class(TArrayBase)
  protected
    FData     : Word64Array;
    FCapacity : NativeInt;
    FCount    : NativeInt;

    procedure SetData(const AData: Word64Array); virtual;

    procedure SetCount(const ANewCount: NativeInt);

    function  GetItem(const AIdx: NativeInt): Word64;                {$IFDEF UseInline}inline;{$ENDIF}
    procedure SetItem(const AIdx: NativeInt; const AValue: Word64);  {$IFDEF UseInline}inline;{$ENDIF}

    function  CompareItems(const AIdx1, AIdx2: NativeInt): Int32; virtual;

  public
    class function CreateInstance: TWord64Array; virtual;

    constructor Create; overload; virtual;
    constructor Create(const V: Word64Array); overload;

    property  Data: Word64Array read FData write SetData;

    procedure Clear;

    procedure Assign(const ASource: TWord64Array); overload;
    procedure Assign(const ASource: Word64Array); overload;
    procedure Assign(const ASource: Array of Word64); overload;

    function  Duplicate: TWord64Array;

    function  IsEqual(const V: TWord64Array): Boolean;

    property  Count: NativeInt read FCount write SetCount;
    property  Item[const AIdx: NativeInt]: Word64 read GetItem write SetItem; default;

    function  PosNext(
              const AItem: Word64;
              const APrevPos: NativeInt = -1;
              const IsSortedAscending: Boolean = False): NativeInt;

    function  GetIndex(const AValue: Word64): NativeInt;
    function  HasValue(const AValue: Word64): Boolean;

    function  Add(const AValue: Word64): NativeInt;
    function  AddIfNotExists(const AValue: Word64): NativeInt;

    function  AddArray(const AArray: Word64Array): NativeInt; overload;
    function  AddArray(const AArray: TWord64Array): NativeInt; overload;

    procedure Insert(const AIdx: NativeInt; const ACount: NativeInt = 1);

    procedure Delete(const AIdx: NativeInt; const ACount: NativeInt = 1);

    procedure Sort;

    function  GetRange(const ALoIdx, AHiIdx: NativeInt): Word64Array;
    procedure SetRange(const ALoIdx, AHiIdx: NativeInt; const V: Word64Array);

    procedure Fill(const AIdx, ACount: NativeInt; const AValue: Word64);
  end;



{                                                                              }
{ Equivalent Unsigned Integer types                                            }
{                                                                              }
{$IFDEF LongWordIs32Bits}
type
  TLongWordArray = TWord32Array;
{$ELSE}{$IFDEF LongWordIs64Bits}
type
  TLongWordArray = TWord64Array;
{$ENDIF}{$ENDIF}

type
  TCardinalArray = TWord32Array;
  TUInt32Array = TWord32Array;
  TUInt64Array = TWord64Array;

{$IFDEF NativeUIntIs32Bits}
type
  TNativeUIntArray = TUInt32Array;
{$ELSE}{$IFDEF NativeUIntIs64Bits}
type
  TNativeUIntArray = TUInt64Array;
{$ENDIF}{$ENDIF}

type
  TNativeWordArray = TNativeUIntArray;
  TUIntArray = TUInt64Array;



{                                                                              }
{ TSingleArray                                                                 }
{   An TSingleArray implemented using a dynamic array.                         }
{                                                                              }
type
  TSingleArray = class(TArrayBase)
  protected
    FData     : SingleArray;
    FCapacity : NativeInt;
    FCount    : NativeInt;

    procedure SetData(const AData: SingleArray); virtual;

    procedure SetCount(const ANewCount: NativeInt);

    function  GetItem(const AIdx: NativeInt): Single;                {$IFDEF UseInline}inline;{$ENDIF}
    procedure SetItem(const AIdx: NativeInt; const AValue: Single);  {$IFDEF UseInline}inline;{$ENDIF}

    function  CompareItems(const AIdx1, AIdx2: NativeInt): Int32; virtual;

    function  GetItemAsString(const AIdx: NativeInt): String;
    procedure SetItemAsString(const AIdx: NativeInt; const AValue: String);

    function  GetAsString: String;
    procedure SetAsString(const S: String);

  public
    class function CreateInstance: TSingleArray; virtual;

    constructor Create; overload; virtual;
    constructor Create(const V: SingleArray); overload;

    property  Data: SingleArray read FData write SetData;

    procedure Clear;

    procedure Assign(const ASource: TSingleArray); overload;
    procedure Assign(const ASource: SingleArray); overload;
    procedure Assign(const ASource: Array of Single); overload;

    function  Duplicate: TSingleArray;

    function  IsEqual(const V: TSingleArray): Boolean;

    property  Count: NativeInt read FCount write SetCount;
    property  Item[const AIdx: NativeInt]: Single read GetItem write SetItem; default;

    function  PosNext(
              const AItem: Single;
              const APrevPos: NativeInt = -1;
              const IsSortedAscending: Boolean = False): NativeInt;

    function  GetIndex(const AValue: Single): NativeInt;
    function  HasValue(const AValue: Single): Boolean;

    function  Add(const AValue: Single): NativeInt;
    function  AddIfNotExists(const AValue: Single): NativeInt;

    function  AddArray(const AArray: SingleArray): NativeInt; overload;
    function  AddArray(const AArray: TSingleArray): NativeInt; overload;

    procedure Insert(const AIdx: NativeInt; const ACount: NativeInt = 1);

    procedure Delete(const AIdx: NativeInt; const ACount: NativeInt = 1);

    procedure Sort;

    function  GetRange(const ALoIdx, AHiIdx: NativeInt): SingleArray;
    procedure SetRange(const ALoIdx, AHiIdx: NativeInt; const V: SingleArray);

    procedure Fill(const AIdx, ACount: NativeInt; const AValue: Single);

    property  ItemAsString[const AIdx: NativeInt]: String read GetItemAsString write SetItemAsString;
    property  AsString: String read GetAsString write SetAsString;
  end;



{                                                                              }
{ TDoubleArray                                                                 }
{   An TDoubleArray implemented using a dynamic array.                         }
{                                                                              }
type
  TDoubleArray = class(TArrayBase)
  protected
    FData     : DoubleArray;
    FCapacity : NativeInt;
    FCount    : NativeInt;

    procedure SetData(const AData: DoubleArray); virtual;

    procedure SetCount(const ANewCount: NativeInt);

    function  GetItem(const AIdx: NativeInt): Double;                {$IFDEF UseInline}inline;{$ENDIF}
    procedure SetItem(const AIdx: NativeInt; const AValue: Double);  {$IFDEF UseInline}inline;{$ENDIF}

    function  CompareItems(const AIdx1, AIdx2: NativeInt): Int32; virtual;

    function  GetItemAsString(const AIdx: NativeInt): String;
    procedure SetItemAsString(const AIdx: NativeInt; const AValue: String);

    function  GetAsString: String;
    procedure SetAsString(const S: String);

  public
    class function CreateInstance: TDoubleArray; virtual;

    constructor Create; overload; virtual;
    constructor Create(const V: DoubleArray); overload;

    property  Data: DoubleArray read FData write SetData;

    procedure Clear;

    procedure Assign(const ASource: TDoubleArray); overload;
    procedure Assign(const ASource: DoubleArray); overload;
    procedure Assign(const ASource: Array of Double); overload;

    function  Duplicate: TDoubleArray;

    function  IsEqual(const V: TDoubleArray): Boolean;

    property  Count: NativeInt read FCount write SetCount;
    property  Item[const AIdx: NativeInt]: Double read GetItem write SetItem; default;

    function  PosNext(
              const AItem: Double;
              const APrevPos: NativeInt = -1;
              const IsSortedAscending: Boolean = False): NativeInt;

    function  GetIndex(const AValue: Double): NativeInt;
    function  HasValue(const AValue: Double): Boolean;

    function  Add(const AValue: Double): NativeInt;
    function  AddIfNotExists(const AValue: Double): NativeInt;

    function  AddArray(const AArray: DoubleArray): NativeInt; overload;
    function  AddArray(const AArray: TDoubleArray): NativeInt; overload;

    procedure Insert(const AIdx: NativeInt; const ACount: NativeInt = 1);

    procedure Delete(const AIdx: NativeInt; const ACount: NativeInt = 1);

    procedure Sort;

    function  GetRange(const ALoIdx, AHiIdx: NativeInt): DoubleArray;
    procedure SetRange(const ALoIdx, AHiIdx: NativeInt; const V: DoubleArray);

    procedure Fill(const AIdx, ACount: NativeInt; const AValue: Double);

    property  ItemAsString[const AIdx: NativeInt]: String read GetItemAsString write SetItemAsString;
    property  AsString: String read GetAsString write SetAsString;
  end;



{                                                                              }
{ Equivalent Float type (Double)                                               }
{                                                                              }
type
  TFloatArray = TDoubleArray;



{$IFDEF SupportAnsiString}
{                                                                              }
{ TAnsiStringArray                                                             }
{   An TAnsiStringArray implemented using a dynamic array.                     }
{                                                                              }
type
  TAnsiStringArray = class(TArrayBase)
  protected
    FData     : AnsiStringArray;
    FCapacity : NativeInt;
    FCount    : NativeInt;

    procedure SetData(const AData: AnsiStringArray); virtual;

    procedure SetCount(const ANewCount: NativeInt);

    function  GetItem(const AIdx: NativeInt): AnsiString;                {$IFDEF UseInline}inline;{$ENDIF}
    procedure SetItem(const AIdx: NativeInt; const AValue: AnsiString);  {$IFDEF UseInline}inline;{$ENDIF}

    function  CompareItems(const AIdx1, AIdx2: NativeInt): Int32; virtual;

    function  GetItemAsString(const AIdx: NativeInt): String;
    procedure SetItemAsString(const AIdx: NativeInt; const AValue: String);

    function  GetAsString: String;
    procedure SetAsString(const S: String);

  public
    class function CreateInstance: TAnsiStringArray; virtual;

    constructor Create; overload; virtual;
    constructor Create(const V: AnsiStringArray); overload;

    property  Data: AnsiStringArray read FData write SetData;

    procedure Clear;

    procedure Assign(const ASource: TAnsiStringArray); overload;
    procedure Assign(const ASource: AnsiStringArray); overload;
    procedure Assign(const ASource: Array of AnsiString); overload;

    function  Duplicate: TAnsiStringArray;

    function  IsEqual(const V: TAnsiStringArray): Boolean;

    property  Count: NativeInt read FCount write SetCount;
    property  Item[const AIdx: NativeInt]: AnsiString read GetItem write SetItem; default;

    function  PosNext(
              const AItem: AnsiString;
              const APrevPos: NativeInt = -1;
              const IsSortedAscending: Boolean = False): NativeInt;

    function  GetIndex(const AValue: AnsiString): NativeInt;
    function  HasValue(const AValue: AnsiString): Boolean;

    function  Add(const AValue: AnsiString): NativeInt;
    function  AddIfNotExists(const AValue: AnsiString): NativeInt;

    function  AddArray(const AArray: AnsiStringArray): NativeInt; overload;
    function  AddArray(const AArray: TAnsiStringArray): NativeInt; overload;

    procedure Insert(const AIdx: NativeInt; const ACount: NativeInt = 1);

    procedure Delete(const AIdx: NativeInt; const ACount: NativeInt = 1);

    procedure Sort;

    function  GetRange(const ALoIdx, AHiIdx: NativeInt): AnsiStringArray;
    procedure SetRange(const ALoIdx, AHiIdx: NativeInt; const V: AnsiStringArray);

    procedure Fill(const AIdx, ACount: NativeInt; const AValue: AnsiString);

    property  ItemAsString[const AIdx: NativeInt]: String read GetItemAsString write SetItemAsString;
    property  AsString: String read GetAsString write SetAsString;
  end;



{$ENDIF}
{                                                                              }
{ TRawByteStringArray                                                          }
{   An TRawByteStringArray implemented using a dynamic array.                  }
{                                                                              }
type
  TRawByteStringArray = class(TArrayBase)
  protected
    FData     : RawByteStringArray;
    FCapacity : NativeInt;
    FCount    : NativeInt;

    procedure SetData(const AData: RawByteStringArray); virtual;

    procedure SetCount(const ANewCount: NativeInt);

    function  GetItem(const AIdx: NativeInt): RawByteString;                {$IFDEF UseInline}inline;{$ENDIF}
    procedure SetItem(const AIdx: NativeInt; const AValue: RawByteString);  {$IFDEF UseInline}inline;{$ENDIF}

    function  CompareItems(const AIdx1, AIdx2: NativeInt): Int32; virtual;

    function  GetItemAsString(const AIdx: NativeInt): String;
    procedure SetItemAsString(const AIdx: NativeInt; const AValue: String);

    function  GetAsString: String;
    procedure SetAsString(const S: String);

  public
    class function CreateInstance: TRawByteStringArray; virtual;

    constructor Create; overload; virtual;
    constructor Create(const V: RawByteStringArray); overload;

    property  Data: RawByteStringArray read FData write SetData;

    procedure Clear;

    procedure Assign(const ASource: TRawByteStringArray); overload;
    procedure Assign(const ASource: RawByteStringArray); overload;
    procedure Assign(const ASource: Array of RawByteString); overload;

    function  Duplicate: TRawByteStringArray;

    function  IsEqual(const V: TRawByteStringArray): Boolean;

    property  Count: NativeInt read FCount write SetCount;
    property  Item[const AIdx: NativeInt]: RawByteString read GetItem write SetItem; default;

    function  PosNext(
              const AItem: RawByteString;
              const APrevPos: NativeInt = -1;
              const IsSortedAscending: Boolean = False): NativeInt;

    function  GetIndex(const AValue: RawByteString): NativeInt;
    function  HasValue(const AValue: RawByteString): Boolean;

    function  Add(const AValue: RawByteString): NativeInt;
    function  AddIfNotExists(const AValue: RawByteString): NativeInt;

    function  AddArray(const AArray: RawByteStringArray): NativeInt; overload;
    function  AddArray(const AArray: TRawByteStringArray): NativeInt; overload;

    procedure Insert(const AIdx: NativeInt; const ACount: NativeInt = 1);

    procedure Delete(const AIdx: NativeInt; const ACount: NativeInt = 1);

    procedure Sort;

    function  GetRange(const ALoIdx, AHiIdx: NativeInt): RawByteStringArray;
    procedure SetRange(const ALoIdx, AHiIdx: NativeInt; const V: RawByteStringArray);

    procedure Fill(const AIdx, ACount: NativeInt; const AValue: RawByteString);

    property  ItemAsString[const AIdx: NativeInt]: String read GetItemAsString write SetItemAsString;
    property  AsString: String read GetAsString write SetAsString;
  end;



{                                                                              }
{ TUnicodeStringArray                                                          }
{   An TUnicodeStringArray implemented using a dynamic array.                  }
{                                                                              }
type
  TUnicodeStringArray = class(TArrayBase)
  protected
    FData     : UnicodeStringArray;
    FCapacity : NativeInt;
    FCount    : NativeInt;

    procedure SetData(const AData: UnicodeStringArray); virtual;

    procedure SetCount(const ANewCount: NativeInt);

    function  GetItem(const AIdx: NativeInt): UnicodeString;                {$IFDEF UseInline}inline;{$ENDIF}
    procedure SetItem(const AIdx: NativeInt; const AValue: UnicodeString);  {$IFDEF UseInline}inline;{$ENDIF}

    function  CompareItems(const AIdx1, AIdx2: NativeInt): Int32; virtual;

    function  GetItemAsString(const AIdx: NativeInt): String;
    procedure SetItemAsString(const AIdx: NativeInt; const AValue: String);

    function  GetAsString: String;
    procedure SetAsString(const S: String);

  public
    class function CreateInstance: TUnicodeStringArray; virtual;

    constructor Create; overload; virtual;
    constructor Create(const V: UnicodeStringArray); overload;

    property  Data: UnicodeStringArray read FData write SetData;

    procedure Clear;

    procedure Assign(const ASource: TUnicodeStringArray); overload;
    procedure Assign(const ASource: UnicodeStringArray); overload;
    procedure Assign(const ASource: Array of UnicodeString); overload;

    function  Duplicate: TUnicodeStringArray;

    function  IsEqual(const V: TUnicodeStringArray): Boolean;

    property  Count: NativeInt read FCount write SetCount;
    property  Item[const AIdx: NativeInt]: UnicodeString read GetItem write SetItem; default;

    function  PosNext(
              const AItem: UnicodeString;
              const APrevPos: NativeInt = -1;
              const IsSortedAscending: Boolean = False): NativeInt;

    function  GetIndex(const AValue: UnicodeString): NativeInt;
    function  HasValue(const AValue: UnicodeString): Boolean;

    function  Add(const AValue: UnicodeString): NativeInt;
    function  AddIfNotExists(const AValue: UnicodeString): NativeInt;

    function  AddArray(const AArray: UnicodeStringArray): NativeInt; overload;
    function  AddArray(const AArray: TUnicodeStringArray): NativeInt; overload;

    procedure Insert(const AIdx: NativeInt; const ACount: NativeInt = 1);

    procedure Delete(const AIdx: NativeInt; const ACount: NativeInt = 1);

    procedure Sort;

    function  GetRange(const ALoIdx, AHiIdx: NativeInt): UnicodeStringArray;
    procedure SetRange(const ALoIdx, AHiIdx: NativeInt; const V: UnicodeStringArray);

    procedure Fill(const AIdx, ACount: NativeInt; const AValue: UnicodeString);

    property  ItemAsString[const AIdx: NativeInt]: String read GetItemAsString write SetItemAsString;
    property  AsString: String read GetAsString write SetAsString;
  end;



{                                                                              }
{ Equivalent String types                                                      }
{                                                                              }
type
  TUTF8StringArray = TRawByteStringArray;

{$IFDEF StringIsUnicode}
type
  TStringArray = TUnicodeStringArray;
{$ELSE}{$IFDEF SupportAnsiString}
type
  TStringArray = TAnsiStringArray;
{$ENDIF}{$ENDIF}



{                                                                              }
{ TPointerArray                                                                }
{   An TPointerArray implemented using a dynamic array.                        }
{                                                                              }
type
  TPointerArray = class(TArrayBase)
  protected
    FData     : PointerArray;
    FCapacity : NativeInt;
    FCount    : NativeInt;

    procedure SetData(const AData: PointerArray); virtual;

    procedure SetCount(const ANewCount: NativeInt);

    function  GetItem(const AIdx: NativeInt): Pointer;                {$IFDEF UseInline}inline;{$ENDIF}
    procedure SetItem(const AIdx: NativeInt; const AValue: Pointer);  {$IFDEF UseInline}inline;{$ENDIF}

    function  CompareItems(const AIdx1, AIdx2: NativeInt): Int32; virtual;

  public
    class function CreateInstance: TPointerArray; virtual;

    constructor Create; overload; virtual;
    constructor Create(const V: PointerArray); overload;

    property  Data: PointerArray read FData write SetData;

    procedure Clear;

    procedure Assign(const ASource: TPointerArray); overload;
    procedure Assign(const ASource: PointerArray); overload;
    procedure Assign(const ASource: Array of Pointer); overload;

    function  Duplicate: TPointerArray;

    function  IsEqual(const V: TPointerArray): Boolean;

    property  Count: NativeInt read FCount write SetCount;
    property  Item[const AIdx: NativeInt]: Pointer read GetItem write SetItem; default;

    function  PosNext(
              const AItem: Pointer;
              const APrevPos: NativeInt = -1;
              const IsSortedAscending: Boolean = False): NativeInt;

    function  GetIndex(const AValue: Pointer): NativeInt;
    function  HasValue(const AValue: Pointer): Boolean;

    function  Add(const AValue: Pointer): NativeInt;
    function  AddIfNotExists(const AValue: Pointer): NativeInt;

    function  AddArray(const AArray: PointerArray): NativeInt; overload;
    function  AddArray(const AArray: TPointerArray): NativeInt; overload;

    procedure Insert(const AIdx: NativeInt; const ACount: NativeInt = 1);

    procedure Delete(const AIdx: NativeInt; const ACount: NativeInt = 1);

    procedure Sort;

    function  GetRange(const ALoIdx, AHiIdx: NativeInt): PointerArray;
    procedure SetRange(const ALoIdx, AHiIdx: NativeInt; const V: PointerArray);

    procedure Fill(const AIdx, ACount: NativeInt; const AValue: Pointer);
  end;



{                                                                              }
{ TInterfaceArray                                                              }
{   An TInterfaceArray implemented using a dynamic array.                      }
{                                                                              }
type
  TInterfaceArray = class(TArrayBase)
  protected
    FData     : InterfaceArray;
    FCapacity : NativeInt;
    FCount    : NativeInt;

    procedure SetData(const AData: InterfaceArray); virtual;

    procedure SetCount(const ANewCount: NativeInt);

    function  GetItem(const AIdx: NativeInt): IInterface;                {$IFDEF UseInline}inline;{$ENDIF}
    procedure SetItem(const AIdx: NativeInt; const AValue: IInterface);  {$IFDEF UseInline}inline;{$ENDIF}

    function  CompareItems(const AIdx1, AIdx2: NativeInt): Int32; virtual;

  public
    class function CreateInstance: TInterfaceArray; virtual;

    constructor Create; overload; virtual;
    constructor Create(const V: InterfaceArray); overload;

    property  Data: InterfaceArray read FData write SetData;

    procedure Clear;

    procedure Assign(const ASource: TInterfaceArray); overload;
    procedure Assign(const ASource: InterfaceArray); overload;
    procedure Assign(const ASource: Array of IInterface); overload;

    function  Duplicate: TInterfaceArray;

    function  IsEqual(const V: TInterfaceArray): Boolean;

    property  Count: NativeInt read FCount write SetCount;
    property  Item[const AIdx: NativeInt]: IInterface read GetItem write SetItem; default;

    function  PosNext(
              const AItem: IInterface;
              const APrevPos: NativeInt = -1;
              const IsSortedAscending: Boolean = False): NativeInt;

    function  GetIndex(const AValue: IInterface): NativeInt;
    function  HasValue(const AValue: IInterface): Boolean;

    function  Add(const AValue: IInterface): NativeInt;
    function  AddIfNotExists(const AValue: IInterface): NativeInt;

    function  AddArray(const AArray: InterfaceArray): NativeInt; overload;
    function  AddArray(const AArray: TInterfaceArray): NativeInt; overload;

    procedure Insert(const AIdx: NativeInt; const ACount: NativeInt = 1);

    procedure Delete(const AIdx: NativeInt; const ACount: NativeInt = 1);

    procedure Sort;

    function  GetRange(const ALoIdx, AHiIdx: NativeInt): InterfaceArray;
    procedure SetRange(const ALoIdx, AHiIdx: NativeInt; const V: InterfaceArray);

    procedure Fill(const AIdx, ACount: NativeInt; const AValue: IInterface);
  end;



{                                                                              }
{ Error strings                                                                }
{                                                                              }
const
  SErrArrayIndexOutOfBounds = 'Array index out of bounds (%d)';
  SErrCannotDuplicate       = '%s cannot duplicate: %s';
  SErrInvalidCountValue     = 'Invalid count value (%d)';
  SErrSourceNotAssigned     = 'Source not assigned';



implementation



{                                                                              }
{ Utility functions                                                            }
{                                                                              }
function MinNativeInt(const A, B: NativeInt): NativeInt; inline;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

function MaxNativeInt(const A, B: NativeInt): NativeInt; inline;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;



{                                                                              }
{ TObjectArray                                                                 }
{                                                                              }
class function TObjectArray.CreateInstance(const AIsItemOwner: Boolean): TObjectArray;
begin
  Result := TObjectArray.Create(nil, AIsItemOwner);
end;

constructor TObjectArray.Create(
            const AIsItemOwner: Boolean);
begin
  inherited Create;
  Init;
  FIsItemOwner := AIsItemOwner;
  FData := nil;
  FCount := 0;
  FCapacity := 0;
end;

constructor TObjectArray.Create(
            const AData: ObjectArray;
            const AIsItemOwner: Boolean);
begin
  inherited Create;
  Init;
  FIsItemOwner := AIsItemOwner;
  FData := AData;
  FCount := Length(FData);
  FCapacity := FCount;
end;

destructor TObjectArray.Destroy;
begin
  if FIsItemOwner then
    FreeItems;
  inherited Destroy;
end;

procedure TObjectArray.Init;
begin
end;

procedure TObjectArray.FreeItems;
var
  C : NativeInt;
  L : NativeInt;
  I : NativeInt;
begin
  C := FCount;
  L := Length(FData);
  if L < C then
    C := L;
  for I := C - 1 downto 0 do
    FreeAndNil(FData[I]);
  FData := nil;
  FCapacity := 0;
  FCount := 0;
end;

procedure TObjectArray.Clear;
begin
  if FIsItemOwner then
    FreeItems
  else
    begin
      FData := nil;
      FCapacity := 0;
      FCount := 0;
    end;
end;

procedure TObjectArray.SetData(const AData: ObjectArray);
begin
  Clear;
  FData := AData;
  FCount := Length(FData);
  FCapacity := FCount;
end;

procedure TObjectArray.Assign(const ASource: TObjectArray);
var
  D : ObjectArray;
begin
  if not Assigned(ASource) then
    raise EArrayError.Create(SErrSourceNotAssigned);

  D := Copy(ASource.FData);
  SetLength(D, ASource.FCount);
  SetData(D);
end;

function TObjectArray.Duplicate: TObjectArray;
var
  Obj : TObjectArray;
begin
  try
    Obj := CreateInstance(False);
    try
      Obj.Assign(self);
    except
      Obj.Free;
      raise;
    end;
  except
    on E : Exception do
      raise EArrayError.CreateFmt(SErrCannotDuplicate, [ClassName, E.Message]);
  end;
  Result := Obj;
end;

function TObjectArray.IsEqual(const V: TObjectArray): Boolean;
var
  I : NativeInt;
  L : NativeInt;
  A : TObject;
  B : TObject;
begin
  L := V.Count;
  if FCount <> L then
    begin
      Result := False;
      exit;
    end;
  for I := 0 to L - 1 do
    begin
      A := FData[I];
      B := V.FData[I];
      if A <> B then
        begin
          Result := False;
          exit;
        end;
    end;
  Result := True;
end;

{ Memory allocation strategy to reduce memory copies:                          }
{   * For first allocation: allocate the exact size.                           }
{   * For change to < 16: allocate 16 entries.                                 }
{   * For growing to >= 16: pre-allocate 1/8th of ANewCount.                   }
{   * For shrinking blocks: shrink actual allocation when Count is less        }
{     than half of the allocated size.                                         }
procedure TObjectArray.SetCount(const ANewCount: NativeInt);
var
  N : NativeInt;
  C : NativeInt;
  I : NativeInt;
  L : NativeInt;
begin
  N := ANewCount;
  if N < 0 then
    raise EArrayError.CreateFmt(SErrInvalidCountValue, [N]);

  C := FCount;
  if N = C then
    exit;

  if (N < C) and FIsItemOwner then
    for I := C - 1 downto N do
      FreeAndNil(FData[I]);

  FCount := N;
  L := FCapacity;
  if L > 0 then
    if N < 16 then
      N := 16
    else
    if N > L then
      N := N + N shr 3
    else
    if N > L shr 1 then
      exit;

  if N <> L then
    begin
      SetLength(FData, N);
      if N > L then
        FillChar(FData[L], SizeOf(TObject) * (N - L), 0);
      FCapacity := N;
    end;
end;

function TObjectArray.GetItem(const AIdx: NativeInt): TObject;
begin
  {$IFOPT R+}
  if (AIdx < 0) or (AIdx >= FCount) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AIdx]);
  {$ELSE}
  Assert(AIdx >= 0);
  Assert(AIdx < FCount);
  {$ENDIF}

  Result := FData[AIdx];
end;

procedure TObjectArray.SetItem(const AIdx: NativeInt; const AValue: TObject);
var
  P : ^TObject;
  V : TObject;
begin
  {$IFOPT R+}
  if (AIdx < 0) or (AIdx >= FCount) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AIdx]);
  {$ELSE}
  Assert(AIdx >= 0);
  Assert(AIdx < FCount);
  {$ENDIF}

  P := Pointer(FData);
  Inc(P, AIdx);
  if FIsItemOwner then
    begin
      V := P^;
      if V = AValue then
        exit;
      V.Free;
    end;
  P^ := AValue;
end;

function TObjectArray.GetTailItem: TObject;
var
  C : NativeInt;
begin
  C := FCount;
  if C <= 0 then
    Result := nil
  else
    Result := FData[C - 1];
end;

function TObjectArray.PosNext(const AItem: TObject; const APrevPos: NativeInt): NativeInt;
var
  F : NativeInt;
  I : NativeInt;
begin
  F := APrevPos + 1;
  if F < 0 then
    F := 0;
  for I := F to FCount - 1 do
    if FData[I] = AItem then
      begin
        Result := I;
        exit;
      end;
  Result := -1;
end;

function TObjectArray.PosNext(
         var AItem: TObject;
         const AClassType: TClass;
         const APrevPos: NativeInt): NativeInt;
var
  F : NativeInt;
  I : NativeInt;
begin
  F := APrevPos + 1;
  if F < 0 then
    F := 0;
  for I := F to FCount - 1 do
    begin
      AItem := FData[I];
      if AItem.InheritsFrom(AClassType) then
        begin
          Result := I;
          exit;
        end;
    end;
  AItem := nil;
  Result := -1;
end;

function TObjectArray.PosNext(
         var AItem: TObject;
         const AClassName: String;
         const APrevPos: NativeInt): NativeInt;
var
  F : NativeInt;
  I : NativeInt;
begin
  F := APrevPos + 1;
  if F < 0 then
    F := 0;
  for I := F to FCount - 1 do
    begin
      AItem := FData[I];
      if Assigned(AItem) and AItem.ClassNameIs(AClassName) then
        begin
          Result := I;
          exit;
        end;
    end;
  AItem := nil;
  Result := -1;
end;

function TObjectArray.GetIndex(const AValue: TObject): NativeInt;
begin
  Result := PosNext(AValue, -1);
end;

function TObjectArray.HasValue(const AValue: TObject): Boolean;
begin
  Result := PosNext(AValue, -1) >= 0;
end;

function TObjectArray.Add(const AValue: TObject): NativeInt;
var
  C : NativeInt;
begin
  C := FCount;
  if C >= FCapacity then
    SetCount(C + 1)
  else
    FCount := C + 1;
  FData[C] := AValue;
  Result := C;
end;

function TObjectArray.AddIfNotExists(const AValue: TObject): NativeInt;
var
  I : NativeInt;
begin
  I := PosNext(AValue, -1);
  if I < 0 then
    I := Add(AValue);
  Result := I;
end;

function TObjectArray.AddArray(const AArray: ObjectArray): NativeInt;
var
  C : NativeInt;
  I : NativeInt;
  L : NativeInt;
begin
  C := FCount;
  L := Length(AArray);
  SetCount(C + L);

  for I := 0 to L - 1 do
    FData[C + I] := AArray[I];

  Result := C;
end;

function TObjectArray.AddArray(const AArray: TObjectArray): NativeInt;
var
  C : NativeInt;
  I : NativeInt;
  L : NativeInt;
begin
  if not Assigned(AArray) then
    raise EArrayError.Create(SErrSourceNotAssigned);

  C := FCount;
  L := AArray.FCount;
  SetCount(C + L);

  for I := 0 to L - 1 do
    FData[C + I] := AArray.FData[I];

  Result := C;
end;

procedure TObjectArray.Insert(const AIdx: NativeInt; const ACount: NativeInt = 1);
var
  A : NativeInt;
  C : NativeInt;
  N : NativeInt;
begin
  A := ACount;
  if A <= 0 then
    exit;

  C := FCount;
  if (AIdx < 0) or (AIdx > C) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AIdx]);

  N := C + A;
  if N > FCapacity then
    SetCount(N)
  else
    FCount := N;

  if AIdx < C then
    Move(FData[AIdx], FData[AIdx + A], (C - AIdx) * SizeOf(TObject));

  FillChar(FData[AIdx], A * SizeOf(TObject), 0);
end;

procedure TObjectArray.Delete(const AIdx: NativeInt; const ACount: NativeInt);
var
  A : NativeInt;
  C : NativeInt;
  L : NativeInt;
  I : NativeInt;
begin
  A := ACount;
  if A <= 0 then
    exit;

  C := FCount;
  if (AIdx < 0) or (AIdx >= C) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AIdx]);

  L := AIdx + A;
  if L > C then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AIdx]);

  if FIsItemOwner then
    for I := AIdx to L - 1 do
      FreeAndNil(FData[AIdx])
  else
    for I := AIdx to L - 1 do
      FData[AIdx] := nil;

  if L < C then
    begin
      Move(FData[L], FData[AIdx], SizeOf(TObject) * (C - AIdx - A));
      FillChar(FData[C - A], A * SizeOf(TObject), 0);
    end;

  SetCount(C - A);
end;

function TObjectArray.ReleaseItem(const AIdx: NativeInt): TObject;
var
  Itm : TObject;
begin
  {$IFOPT R+}
  if (AIdx < 0) or (AIdx >= FCount) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AIdx]);
  {$ELSE}
  Assert(AIdx >= 0);
  Assert(AIdx < FCount);
  {$ENDIF}

  Itm := FData[AIdx];
  if Assigned(Itm) then
    FData[AIdx] := nil;
  Result := Itm;
end;

function TObjectArray.DeleteValue(const AValue: TObject): Boolean;
var
  I : NativeInt;
begin
  I := PosNext(AValue, -1);
  Result := I >= 0;
  if Result then
    Delete(I, 1);
end;

function TObjectArray.DeleteAll(const AValue: TObject): NativeInt;
begin
  Result := 0;
  while DeleteValue(AValue) do
    Inc(Result);
end;

function TObjectArray.ReleaseValue(const AValue: TObject): Boolean;
var I : NativeInt;
begin
  I := PosNext(AValue, -1);
  Result := I >= 0;
  if Result then
    ReleaseItem(I);
end;

function TObjectArray.RemoveItem(const AIdx: NativeInt): TObject;
begin
  Result := ReleaseItem(AIdx);
  Delete(AIdx, 1);
end;

function TObjectArray.RemoveValue(const AValue: TObject): Boolean;
var
  I : NativeInt;
begin
  I := PosNext(AValue, -1);
  Result := I >= 0;
  if Result then
    RemoveItem(I);
end;

function TObjectArray.CompareItems(const AIdx1, AIdx2: NativeInt): Int32;
var
  A : TObject;
  B : TObject;
begin
  Assert(AIdx1 >= 0);
  Assert(AIdx1 < FCount);
  Assert(AIdx2 >= 0);
  Assert(AIdx2 < FCount);

  A := FData[AIdx1];
  B := FData[AIdx2];
  if NativeUInt(A) = NativeUInt(B) then
    Result := 0
  else
  if NativeUInt(A) < NativeUInt(B) then
    Result := -1
  else
    Result := 1;
end;

procedure TObjectArray.Sort;

  procedure QuickSort(L, R: NativeInt);
  var
    I : NativeInt;
    J : NativeInt;
    M : NativeInt;
    T : TObject;
  begin
    repeat
      I := L;
      J := R;
      M := (L + R) shr 1;
      repeat
        while CompareItems(I, M) < 0 do
          Inc(I);
        while CompareItems(J, M) > 0 do
          Dec(J);
        if I <= J then
          begin
            T := FData[I];
            FData[I] := FData[J];
            FData[J] := T;
            if M = I then
              M := J
            else
            if M = J then
              M := I;
            Inc(I);
            Dec(J);
          end;
      until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    until I >= R;
  end;

var
  C : NativeInt;
begin
  C := Count;
  if C > 0 then
    QuickSort(0, C - 1);
end;

function TObjectArray.GetRange(const ALoIdx, AHiIdx: NativeInt): ObjectArray;
begin
  {$IFOPT R+}
  if (ALoIdx < 0) or (ALoIdx >= FCount) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [ALoIdx]);
  if (AHiIdx < 0) or (AHiIdx >= FCount) or (AHiIdx < ALoIdx) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AHiIdx]);
  {$ELSE}
  Assert((ALoIdx >= 0) and (ALoIdx < FCount));
  Assert((AHiIdx >= 0) and (AHiIdx < FCount) and (AHiIdx >= ALoIdx));
  {$ENDIF}

  Result := Copy(FData, ALoIdx, MinNativeInt(AHiIdx, FCount - 1) - ALoIdx + 1);
end;

procedure TObjectArray.SetRange(const ALoIdx, AHiIdx: NativeInt; const V: ObjectArray);
var
  I : NativeInt;
  L : NativeInt;
  H : NativeInt;
  C : NativeInt;
begin
  {$IFOPT R+}
  if (ALoIdx < 0) or (ALoIdx >= FCount) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [ALoIdx]);
  if (AHiIdx < 0) or (AHiIdx >= FCount) or (AHiIdx < ALoIdx) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AHiIdx]);
  {$ELSE}
  Assert((ALoIdx >= 0) and (ALoIdx < FCount));
  Assert((AHiIdx >= 0) and (AHiIdx < FCount) and (AHiIdx >= ALoIdx));
  {$ENDIF}

  L := MaxNativeInt(0, ALoIdx);
  H := MinNativeInt(Count - 1, AHiIdx);
  C := MinNativeInt(Length(V), H - L  + 1);

  for I := 0 to C - 1 do
    Item[L + I] := V[I];
end;




{                                                                              }
{ TInt32Array                                                                  }
{                                                                              }
class function TInt32Array.CreateInstance: TInt32Array;
begin
  Result := TInt32Array.Create;
end;

constructor TInt32Array.Create;
begin
  inherited Create;
end;

constructor TInt32Array.Create(const V: Int32Array);
begin
  inherited Create;
  SetData(V);
end;

procedure TInt32Array.SetData(const AData: Int32Array);
begin
  FData := AData;
  FCount := Length(FData);
  FCapacity := FCount;
end;

procedure TInt32Array.Clear;
begin
  FData := nil;
  FCapacity := 0;
  FCount := 0;
end;

procedure TInt32Array.Assign(const ASource: Int32Array);
begin
  SetData(Copy(ASource));
end;

procedure TInt32Array.Assign(const ASource: Array of Int32);
var
  H : NativeInt;
  L : NativeInt;
  I : NativeInt;
begin
  H := High(ASource);
  L := H + 1;
  SetLength(FData, L);
  for I := 0 to H do
    FData[I] := ASource[I];
  FCount := L;
  FCapacity := L;
end;

procedure TInt32Array.Assign(const ASource: TInt32Array);
var
  D : Int32Array;
begin
  if not Assigned(ASource) then
    raise EArrayError.Create(SErrSourceNotAssigned);

  D := Copy(ASource.FData);
  SetLength(D, ASource.FCount);
  SetData(D);
end;

function TInt32Array.Duplicate: TInt32Array;
var
  Obj : TInt32Array;
begin
  try
    Obj := CreateInstance;
    try
      Obj.Assign(self);
    except
      Obj.Free;
      raise;
    end;
  except
    on E : Exception do
      raise EArrayError.CreateFmt(SErrCannotDuplicate, [ClassName, E.Message]);
  end;
  Result := Obj;
end;

function TInt32Array.IsEqual(const V: TInt32Array): Boolean;
var
  I, L : NativeInt;
begin
  L := V.Count;
  Result := L = Count;
  if not Result then
    exit;
  for I := 0 to L - 1 do
    if FData[I] <> V.FData[I] then
      begin
        Result := False;
        exit;
      end;
end;

procedure TInt32Array.SetCount(const ANewCount: NativeInt);
var
  L : NativeInt;
  C : NativeInt;
  N : NativeInt;
begin
  N := ANewCount;
  if N < 0 then
    raise EArrayError.CreateFmt(SErrInvalidCountValue, [N]);
  C := FCount;
  if N = C then
    exit;

  FCount := N;
  L := FCapacity;
  if L > 0 then
    if N < 16 then // pre-allocate first 16 entries
      N := 16
    else
    if N > L then
      N := N + N shr 3 // pre-allocate 1/8th extra if growing
    else
    if N > L shr 1 then // only reduce capacity if size is at least half
      exit;

  if N <> L then
    begin
      SetLength(FData, N);
      if N > L then
        FillChar(FData[L], SizeOf(Int32) * (N - L), 0);
      
      FCapacity := N;
    end;
end;

function TInt32Array.GetItem(const AIdx: NativeInt): Int32;
begin
  {$IFOPT R+}
  if (AIdx < 0) or (AIdx >= FCount) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AIdx]);
  {$ELSE}
  Assert(AIdx >= 0);
  Assert(AIdx < FCount);
  {$ENDIF}

  Result := FData[AIdx];
end;

procedure TInt32Array.SetItem(const AIdx: NativeInt; const AValue: Int32);
begin
  {$IFOPT R+}
  if (AIdx < 0) or (AIdx >= FCount) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AIdx]);
  {$ELSE}
  Assert(AIdx >= 0);
  Assert(AIdx < FCount);
  {$ENDIF}

  FData[AIdx] := AValue;
end;

function TInt32Array.PosNext(
         const AItem: Int32;
         const APrevPos: NativeInt;
         const IsSortedAscending: Boolean): NativeInt;
var
  F : NativeInt;
  I : NativeInt;
  L : NativeInt;
  H : NativeInt;
  D : Int32;
begin
  F := APrevPos + 1;
  if F < 0 then
    F := 0;
  if IsSortedAscending then // binary search
    begin
      if F = 0 then // find first
        begin
          L := 0;
          H := Count - 1;
          repeat
            I := (L + H) div 2;
            D := FData[I];
            if D = AItem then
              begin
                while (I > 0) and (FData[I - 1] = AItem) do
                  Dec(I);
                Result := I;
                exit;
              end
            else
            if D > AItem then
              H := I - 1
            else
              L := I + 1;
          until L > H;
          Result := -1;
        end
      else // find next
        if APrevPos >= Count - 1 then
          Result := -1
        else
          if FData[APrevPos + 1] = AItem then
            Result := APrevPos + 1
          else
            Result := -1;
    end
  else // linear search
    begin
      for I := F to Count - 1 do
        if FData[I] = AItem then
          begin
            Result := I;
            exit;
          end;
      Result := -1;
    end;
end;

function TInt32Array.GetIndex(const AValue: Int32): NativeInt;
begin
  Result := PosNext(AValue, -1, False);
end;

function TInt32Array.HasValue(const AValue: Int32): Boolean;
begin
  Result := PosNext(AValue, -1, False) >= 0;
end;

function TInt32Array.Add(const AValue: Int32): NativeInt;
var
  C : NativeInt;
begin
  C := FCount;
  if C >= FCapacity then
    SetCount(C + 1)
  else
    FCount := C + 1;
  FData[C] := AValue;
  Result := C;
end;

function TInt32Array.AddIfNotExists(const AValue: Int32): NativeInt;
var
  I : NativeInt;
begin
  I := PosNext(AValue, -1);
  if I < 0 then
    I := Add(AValue);
  Result := I;
end;

function TInt32Array.AddArray(const AArray: Int32Array): NativeInt;
var
  C : NativeInt;
  I : NativeInt;
  L : NativeInt;
begin
  C := FCount;
  L := Length(AArray);
  SetCount(C + L);

  for I := 0 to L - 1 do
    FData[C + I] := AArray[I];

  Result := C;
end;

function TInt32Array.AddArray(const AArray: TInt32Array): NativeInt;
var
  C : NativeInt;
  I : NativeInt;
  L : NativeInt;
begin
  if not Assigned(AArray) then
    raise EArrayError.Create(SErrSourceNotAssigned);

  C := FCount;
  L := AArray.FCount;
  SetCount(C + L);

  for I := 0 to L - 1 do
    FData[C + I] := AArray.FData[I];

  Result := C;
end;

procedure TInt32Array.Insert(const AIdx: NativeInt; const ACount: NativeInt = 1);
var
  C : NativeInt;
  A : NativeInt;
  N : NativeInt;
begin
  C := FCount;
  if (AIdx < 0) or (AIdx > C) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AIdx]);
  if ACount <= 0 then
    exit;

  A := ACount;
  N := C + A;
  if N > FCapacity then
    SetCount(N)
  else
    FCount := N;

  if AIdx < C then
    Move(FData[AIdx], FData[AIdx + A], (C - AIdx) * SizeOf(Int32));

  FillChar(FData[AIdx], A * SizeOf(Int32), 0);
end;

procedure TInt32Array.Delete(const AIdx: NativeInt; const ACount: NativeInt = 1);
var
  A : NativeInt;
  C : NativeInt;
  L : NativeInt;
begin
  A := ACount;
  if A <= 0 then
    exit;

  C := FCount;
  if (AIdx < 0) or (AIdx >= C) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AIdx]);

  L := AIdx + A;
  if L > C then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AIdx]);

  if L < C then    Move(FData[AIdx + A], FData[AIdx], SizeOf(Int32) * (C - AIdx - A));

  SetCount(C - A);
end;

function TInt32Array.CompareItems(const AIdx1, AIdx2: NativeInt): Int32;
var
  I, J : Int32;
begin
  Assert(AIdx1 >= 0);
  Assert(AIdx1 < FCount);
  Assert(AIdx2 >= 0);
  Assert(AIdx2 < FCount);

  I := FData[AIdx1];
  J := FData[AIdx2];
  if I < J then
    Result := -1
  else
  if I > J then
    Result := 1
  else
    Result := 0;
end;

procedure TInt32Array.Sort;

  procedure QuickSort(L, R: NativeInt);
  var
    I : NativeInt;
    J : NativeInt;
    M : NativeInt;
    T : Int32;
  begin
    repeat
      I := L;
      J := R;
      M := (L + R) shr 1;
      repeat
        while CompareItems(I, M) < 0 do
          Inc(I);
        while CompareItems(J, M) > 0 do
          Dec(J);
        if I <= J then
          begin
            T := FData[I];
            FData[I] := FData[J];
            FData[J] := T;
            if M = I then
              M := J
            else
            if M = J then
              M := I;
            Inc(I);
            Dec(J);
          end;
      until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    until I >= R;
  end;

var
  C : NativeInt;
begin
  C := Count;
  if C > 0 then
    QuickSort(0, C - 1);
end;

procedure TInt32Array.Fill(const AIdx, ACount: NativeInt; const AValue: Int32);
var
  I : NativeInt;
begin
  for I := AIdx to AIdx + ACount - 1 do
    FData[I] := AValue;
end;

function TInt32Array.GetRange(const ALoIdx, AHiIdx: NativeInt): Int32Array;
var
  L : NativeInt;
  H : NativeInt;
begin
  {$IFOPT R+}
  if (ALoIdx < 0) or (ALoIdx >= FCount) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [ALoIdx]);
  if (AHiIdx < 0) or (AHiIdx >= FCount) or (AHiIdx < ALoIdx) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AHiIdx]);
  {$ELSE}
  Assert((ALoIdx >= 0) and (ALoIdx < FCount));
  Assert((AHiIdx >= 0) and (AHiIdx < FCount) and (AHiIdx >= ALoIdx));
  {$ENDIF}

  L := MaxNativeInt(0, ALoIdx);
  H := MinNativeInt(AHiIdx, FCount);
  if H >= L then
    Result := Copy(FData, L, H - L + 1)
  else
    Result := nil;
end;

procedure TInt32Array.SetRange(const ALoIdx, AHiIdx: NativeInt; const V: Int32Array);
var
  L : NativeInt;
  H : NativeInt;
  C : NativeInt;
begin
  {$IFOPT R+}
  if (ALoIdx < 0) or (ALoIdx >= FCount) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [ALoIdx]);
  if (AHiIdx < 0) or (AHiIdx >= FCount) or (AHiIdx < ALoIdx) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AHiIdx]);
  {$ELSE}
  Assert((ALoIdx >= 0) and (ALoIdx < FCount));
  Assert((AHiIdx >= 0) and (AHiIdx < FCount) and (AHiIdx >= ALoIdx));
  {$ENDIF}

  L := MaxNativeInt(0, ALoIdx);
  H := MinNativeInt(AHiIdx, FCount);
  C := MaxNativeInt(MinNativeInt(Length(V), H - L + 1), 0);
  if C > 0 then
    Move(V[0], FData[L], C * Sizeof(Int32));
end;

function TInt32Array.GetItemAsString(const AIdx: NativeInt): String;
begin
  Result := IntToStr(GetItem(AIdx));
end;

function TInt32Array.GetAsString: String;
var
  I : NativeInt;
  L : NativeInt;
begin
  L := FCount;
  if L = 0 then
    begin
      Result := '';
      exit;
    end;
  Result := GetItemAsString(0);
  for I := 1 to L - 1 do
    Result := Result + ',' + GetItemAsString(I);
end;

procedure TInt32Array.SetItemAsString(const AIdx: NativeInt; const AValue: String);
begin
  SetItem(AIdx, StrToInt(AValue));
end;

procedure TInt32Array.SetAsString(const S: String);
var
  L : NativeInt;
  F : NativeInt;
  C : NativeInt;
  G : NativeInt;
begin
  L := Length(S);
  if L = 0 then
    begin
      Count := 0;
      exit;
    end;
  L := 0;
  F := 1;
  C := Length(S);
  while F < C do
    begin
      G := 0;
      while (F + G <= C) and (S[F + G] <> ',') do
        Inc(G);
      Inc(L);
      Count := L;
      SetItemAsString(L - 1, Copy(S, F, G));
      Inc(F, G + 1);
    end;
end;


{                                                                              }
{ TInt64Array                                                                  }
{                                                                              }
class function TInt64Array.CreateInstance: TInt64Array;
begin
  Result := TInt64Array.Create;
end;

constructor TInt64Array.Create;
begin
  inherited Create;
end;

constructor TInt64Array.Create(const V: Int64Array);
begin
  inherited Create;
  SetData(V);
end;

procedure TInt64Array.SetData(const AData: Int64Array);
begin
  FData := AData;
  FCount := Length(FData);
  FCapacity := FCount;
end;

procedure TInt64Array.Clear;
begin
  FData := nil;
  FCapacity := 0;
  FCount := 0;
end;

procedure TInt64Array.Assign(const ASource: Int64Array);
begin
  SetData(Copy(ASource));
end;

procedure TInt64Array.Assign(const ASource: Array of Int64);
var
  H : NativeInt;
  L : NativeInt;
  I : NativeInt;
begin
  H := High(ASource);
  L := H + 1;
  SetLength(FData, L);
  for I := 0 to H do
    FData[I] := ASource[I];
  FCount := L;
  FCapacity := L;
end;

procedure TInt64Array.Assign(const ASource: TInt64Array);
var
  D : Int64Array;
begin
  if not Assigned(ASource) then
    raise EArrayError.Create(SErrSourceNotAssigned);

  D := Copy(ASource.FData);
  SetLength(D, ASource.FCount);
  SetData(D);
end;

function TInt64Array.Duplicate: TInt64Array;
var
  Obj : TInt64Array;
begin
  try
    Obj := CreateInstance;
    try
      Obj.Assign(self);
    except
      Obj.Free;
      raise;
    end;
  except
    on E : Exception do
      raise EArrayError.CreateFmt(SErrCannotDuplicate, [ClassName, E.Message]);
  end;
  Result := Obj;
end;

function TInt64Array.IsEqual(const V: TInt64Array): Boolean;
var
  I, L : NativeInt;
begin
  L := V.Count;
  Result := L = Count;
  if not Result then
    exit;
  for I := 0 to L - 1 do
    if FData[I] <> V.FData[I] then
      begin
        Result := False;
        exit;
      end;
end;

procedure TInt64Array.SetCount(const ANewCount: NativeInt);
var
  L : NativeInt;
  C : NativeInt;
  N : NativeInt;
begin
  N := ANewCount;
  if N < 0 then
    raise EArrayError.CreateFmt(SErrInvalidCountValue, [N]);
  C := FCount;
  if N = C then
    exit;

  FCount := N;
  L := FCapacity;
  if L > 0 then
    if N < 16 then // pre-allocate first 16 entries
      N := 16
    else
    if N > L then
      N := N + N shr 3 // pre-allocate 1/8th extra if growing
    else
    if N > L shr 1 then // only reduce capacity if size is at least half
      exit;

  if N <> L then
    begin
      SetLength(FData, N);
      if N > L then
        FillChar(FData[L], SizeOf(Int64) * (N - L), 0);
      
      FCapacity := N;
    end;
end;

function TInt64Array.GetItem(const AIdx: NativeInt): Int64;
begin
  {$IFOPT R+}
  if (AIdx < 0) or (AIdx >= FCount) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AIdx]);
  {$ELSE}
  Assert(AIdx >= 0);
  Assert(AIdx < FCount);
  {$ENDIF}

  Result := FData[AIdx];
end;

procedure TInt64Array.SetItem(const AIdx: NativeInt; const AValue: Int64);
begin
  {$IFOPT R+}
  if (AIdx < 0) or (AIdx >= FCount) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AIdx]);
  {$ELSE}
  Assert(AIdx >= 0);
  Assert(AIdx < FCount);
  {$ENDIF}

  FData[AIdx] := AValue;
end;

function TInt64Array.PosNext(
         const AItem: Int64;
         const APrevPos: NativeInt;
         const IsSortedAscending: Boolean): NativeInt;
var
  F : NativeInt;
  I : NativeInt;
  L : NativeInt;
  H : NativeInt;
  D : Int64;
begin
  F := APrevPos + 1;
  if F < 0 then
    F := 0;
  if IsSortedAscending then // binary search
    begin
      if F = 0 then // find first
        begin
          L := 0;
          H := Count - 1;
          repeat
            I := (L + H) div 2;
            D := FData[I];
            if D = AItem then
              begin
                while (I > 0) and (FData[I - 1] = AItem) do
                  Dec(I);
                Result := I;
                exit;
              end
            else
            if D > AItem then
              H := I - 1
            else
              L := I + 1;
          until L > H;
          Result := -1;
        end
      else // find next
        if APrevPos >= Count - 1 then
          Result := -1
        else
          if FData[APrevPos + 1] = AItem then
            Result := APrevPos + 1
          else
            Result := -1;
    end
  else // linear search
    begin
      for I := F to Count - 1 do
        if FData[I] = AItem then
          begin
            Result := I;
            exit;
          end;
      Result := -1;
    end;
end;

function TInt64Array.GetIndex(const AValue: Int64): NativeInt;
begin
  Result := PosNext(AValue, -1, False);
end;

function TInt64Array.HasValue(const AValue: Int64): Boolean;
begin
  Result := PosNext(AValue, -1, False) >= 0;
end;

function TInt64Array.Add(const AValue: Int64): NativeInt;
var
  C : NativeInt;
begin
  C := FCount;
  if C >= FCapacity then
    SetCount(C + 1)
  else
    FCount := C + 1;
  FData[C] := AValue;
  Result := C;
end;

function TInt64Array.AddIfNotExists(const AValue: Int64): NativeInt;
var
  I : NativeInt;
begin
  I := PosNext(AValue, -1);
  if I < 0 then
    I := Add(AValue);
  Result := I;
end;

function TInt64Array.AddArray(const AArray: Int64Array): NativeInt;
var
  C : NativeInt;
  I : NativeInt;
  L : NativeInt;
begin
  C := FCount;
  L := Length(AArray);
  SetCount(C + L);

  for I := 0 to L - 1 do
    FData[C + I] := AArray[I];

  Result := C;
end;

function TInt64Array.AddArray(const AArray: TInt64Array): NativeInt;
var
  C : NativeInt;
  I : NativeInt;
  L : NativeInt;
begin
  if not Assigned(AArray) then
    raise EArrayError.Create(SErrSourceNotAssigned);

  C := FCount;
  L := AArray.FCount;
  SetCount(C + L);

  for I := 0 to L - 1 do
    FData[C + I] := AArray.FData[I];

  Result := C;
end;

procedure TInt64Array.Insert(const AIdx: NativeInt; const ACount: NativeInt = 1);
var
  C : NativeInt;
  A : NativeInt;
  N : NativeInt;
begin
  C := FCount;
  if (AIdx < 0) or (AIdx > C) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AIdx]);
  if ACount <= 0 then
    exit;

  A := ACount;
  N := C + A;
  if N > FCapacity then
    SetCount(N)
  else
    FCount := N;

  if AIdx < C then
    Move(FData[AIdx], FData[AIdx + A], (C - AIdx) * SizeOf(Int64));

  FillChar(FData[AIdx], A * SizeOf(Int64), 0);
end;

procedure TInt64Array.Delete(const AIdx: NativeInt; const ACount: NativeInt = 1);
var
  A : NativeInt;
  C : NativeInt;
  L : NativeInt;
begin
  A := ACount;
  if A <= 0 then
    exit;

  C := FCount;
  if (AIdx < 0) or (AIdx >= C) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AIdx]);

  L := AIdx + A;
  if L > C then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AIdx]);

  if L < C then    Move(FData[AIdx + A], FData[AIdx], SizeOf(Int64) * (C - AIdx - A));

  SetCount(C - A);
end;

function TInt64Array.CompareItems(const AIdx1, AIdx2: NativeInt): Int32;
var
  I, J : Int64;
begin
  Assert(AIdx1 >= 0);
  Assert(AIdx1 < FCount);
  Assert(AIdx2 >= 0);
  Assert(AIdx2 < FCount);

  I := FData[AIdx1];
  J := FData[AIdx2];
  if I < J then
    Result := -1
  else
  if I > J then
    Result := 1
  else
    Result := 0;
end;

procedure TInt64Array.Sort;

  procedure QuickSort(L, R: NativeInt);
  var
    I : NativeInt;
    J : NativeInt;
    M : NativeInt;
    T : Int64;
  begin
    repeat
      I := L;
      J := R;
      M := (L + R) shr 1;
      repeat
        while CompareItems(I, M) < 0 do
          Inc(I);
        while CompareItems(J, M) > 0 do
          Dec(J);
        if I <= J then
          begin
            T := FData[I];
            FData[I] := FData[J];
            FData[J] := T;
            if M = I then
              M := J
            else
            if M = J then
              M := I;
            Inc(I);
            Dec(J);
          end;
      until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    until I >= R;
  end;

var
  C : NativeInt;
begin
  C := Count;
  if C > 0 then
    QuickSort(0, C - 1);
end;

procedure TInt64Array.Fill(const AIdx, ACount: NativeInt; const AValue: Int64);
var
  I : NativeInt;
begin
  for I := AIdx to AIdx + ACount - 1 do
    FData[I] := AValue;
end;

function TInt64Array.GetRange(const ALoIdx, AHiIdx: NativeInt): Int64Array;
var
  L : NativeInt;
  H : NativeInt;
begin
  {$IFOPT R+}
  if (ALoIdx < 0) or (ALoIdx >= FCount) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [ALoIdx]);
  if (AHiIdx < 0) or (AHiIdx >= FCount) or (AHiIdx < ALoIdx) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AHiIdx]);
  {$ELSE}
  Assert((ALoIdx >= 0) and (ALoIdx < FCount));
  Assert((AHiIdx >= 0) and (AHiIdx < FCount) and (AHiIdx >= ALoIdx));
  {$ENDIF}

  L := MaxNativeInt(0, ALoIdx);
  H := MinNativeInt(AHiIdx, FCount);
  if H >= L then
    Result := Copy(FData, L, H - L + 1)
  else
    Result := nil;
end;

procedure TInt64Array.SetRange(const ALoIdx, AHiIdx: NativeInt; const V: Int64Array);
var
  L : NativeInt;
  H : NativeInt;
  C : NativeInt;
begin
  {$IFOPT R+}
  if (ALoIdx < 0) or (ALoIdx >= FCount) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [ALoIdx]);
  if (AHiIdx < 0) or (AHiIdx >= FCount) or (AHiIdx < ALoIdx) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AHiIdx]);
  {$ELSE}
  Assert((ALoIdx >= 0) and (ALoIdx < FCount));
  Assert((AHiIdx >= 0) and (AHiIdx < FCount) and (AHiIdx >= ALoIdx));
  {$ENDIF}

  L := MaxNativeInt(0, ALoIdx);
  H := MinNativeInt(AHiIdx, FCount);
  C := MaxNativeInt(MinNativeInt(Length(V), H - L + 1), 0);
  if C > 0 then
    Move(V[0], FData[L], C * Sizeof(Int64));
end;

function TInt64Array.GetItemAsString(const AIdx: NativeInt): String;
begin
  Result := IntToStr(GetItem(AIdx));
end;

function TInt64Array.GetAsString: String;
var
  I : NativeInt;
  L : NativeInt;
begin
  L := FCount;
  if L = 0 then
    begin
      Result := '';
      exit;
    end;
  Result := GetItemAsString(0);
  for I := 1 to L - 1 do
    Result := Result + ',' + GetItemAsString(I);
end;

procedure TInt64Array.SetItemAsString(const AIdx: NativeInt; const AValue: String);
begin
  SetItem(AIdx, StrToInt64(AValue));
end;

procedure TInt64Array.SetAsString(const S: String);
var
  L : NativeInt;
  F : NativeInt;
  C : NativeInt;
  G : NativeInt;
begin
  L := Length(S);
  if L = 0 then
    begin
      Count := 0;
      exit;
    end;
  L := 0;
  F := 1;
  C := Length(S);
  while F < C do
    begin
      G := 0;
      while (F + G <= C) and (S[F + G] <> ',') do
        Inc(G);
      Inc(L);
      Count := L;
      SetItemAsString(L - 1, Copy(S, F, G));
      Inc(F, G + 1);
    end;
end;


{                                                                              }
{ TByteArray                                                                   }
{                                                                              }
class function TByteArray.CreateInstance: TByteArray;
begin
  Result := TByteArray.Create;
end;

constructor TByteArray.Create;
begin
  inherited Create;
end;

constructor TByteArray.Create(const V: ByteArray);
begin
  inherited Create;
  SetData(V);
end;

procedure TByteArray.SetData(const AData: ByteArray);
begin
  FData := AData;
  FCount := Length(FData);
  FCapacity := FCount;
end;

procedure TByteArray.Clear;
begin
  FData := nil;
  FCapacity := 0;
  FCount := 0;
end;

procedure TByteArray.Assign(const ASource: ByteArray);
begin
  SetData(Copy(ASource));
end;

procedure TByteArray.Assign(const ASource: Array of Byte);
var
  H : NativeInt;
  L : NativeInt;
  I : NativeInt;
begin
  H := High(ASource);
  L := H + 1;
  SetLength(FData, L);
  for I := 0 to H do
    FData[I] := ASource[I];
  FCount := L;
  FCapacity := L;
end;

procedure TByteArray.Assign(const ASource: TByteArray);
var
  D : ByteArray;
begin
  if not Assigned(ASource) then
    raise EArrayError.Create(SErrSourceNotAssigned);

  D := Copy(ASource.FData);
  SetLength(D, ASource.FCount);
  SetData(D);
end;

function TByteArray.Duplicate: TByteArray;
var
  Obj : TByteArray;
begin
  try
    Obj := CreateInstance;
    try
      Obj.Assign(self);
    except
      Obj.Free;
      raise;
    end;
  except
    on E : Exception do
      raise EArrayError.CreateFmt(SErrCannotDuplicate, [ClassName, E.Message]);
  end;
  Result := Obj;
end;

function TByteArray.IsEqual(const V: TByteArray): Boolean;
var
  I, L : NativeInt;
begin
  L := V.Count;
  Result := L = Count;
  if not Result then
    exit;
  for I := 0 to L - 1 do
    if FData[I] <> V.FData[I] then
      begin
        Result := False;
        exit;
      end;
end;

procedure TByteArray.SetCount(const ANewCount: NativeInt);
var
  L : NativeInt;
  C : NativeInt;
  N : NativeInt;
begin
  N := ANewCount;
  if N < 0 then
    raise EArrayError.CreateFmt(SErrInvalidCountValue, [N]);
  C := FCount;
  if N = C then
    exit;

  FCount := N;
  L := FCapacity;
  if L > 0 then
    if N < 16 then // pre-allocate first 16 entries
      N := 16
    else
    if N > L then
      N := N + N shr 3 // pre-allocate 1/8th extra if growing
    else
    if N > L shr 1 then // only reduce capacity if size is at least half
      exit;

  if N <> L then
    begin
      SetLength(FData, N);
      if N > L then
        FillChar(FData[L], SizeOf(Byte) * (N - L), 0);
      
      FCapacity := N;
    end;
end;

function TByteArray.GetItem(const AIdx: NativeInt): Byte;
begin
  {$IFOPT R+}
  if (AIdx < 0) or (AIdx >= FCount) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AIdx]);
  {$ELSE}
  Assert(AIdx >= 0);
  Assert(AIdx < FCount);
  {$ENDIF}

  Result := FData[AIdx];
end;

procedure TByteArray.SetItem(const AIdx: NativeInt; const AValue: Byte);
begin
  {$IFOPT R+}
  if (AIdx < 0) or (AIdx >= FCount) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AIdx]);
  {$ELSE}
  Assert(AIdx >= 0);
  Assert(AIdx < FCount);
  {$ENDIF}

  FData[AIdx] := AValue;
end;

function TByteArray.PosNext(
         const AItem: Byte;
         const APrevPos: NativeInt;
         const IsSortedAscending: Boolean): NativeInt;
var
  F : NativeInt;
  I : NativeInt;
  L : NativeInt;
  H : NativeInt;
  D : Byte;
begin
  F := APrevPos + 1;
  if F < 0 then
    F := 0;
  if IsSortedAscending then // binary search
    begin
      if F = 0 then // find first
        begin
          L := 0;
          H := Count - 1;
          repeat
            I := (L + H) div 2;
            D := FData[I];
            if D = AItem then
              begin
                while (I > 0) and (FData[I - 1] = AItem) do
                  Dec(I);
                Result := I;
                exit;
              end
            else
            if D > AItem then
              H := I - 1
            else
              L := I + 1;
          until L > H;
          Result := -1;
        end
      else // find next
        if APrevPos >= Count - 1 then
          Result := -1
        else
          if FData[APrevPos + 1] = AItem then
            Result := APrevPos + 1
          else
            Result := -1;
    end
  else // linear search
    begin
      for I := F to Count - 1 do
        if FData[I] = AItem then
          begin
            Result := I;
            exit;
          end;
      Result := -1;
    end;
end;

function TByteArray.GetIndex(const AValue: Byte): NativeInt;
begin
  Result := PosNext(AValue, -1, False);
end;

function TByteArray.HasValue(const AValue: Byte): Boolean;
begin
  Result := PosNext(AValue, -1, False) >= 0;
end;

function TByteArray.Add(const AValue: Byte): NativeInt;
var
  C : NativeInt;
begin
  C := FCount;
  if C >= FCapacity then
    SetCount(C + 1)
  else
    FCount := C + 1;
  FData[C] := AValue;
  Result := C;
end;

function TByteArray.AddIfNotExists(const AValue: Byte): NativeInt;
var
  I : NativeInt;
begin
  I := PosNext(AValue, -1);
  if I < 0 then
    I := Add(AValue);
  Result := I;
end;

function TByteArray.AddArray(const AArray: ByteArray): NativeInt;
var
  C : NativeInt;
  I : NativeInt;
  L : NativeInt;
begin
  C := FCount;
  L := Length(AArray);
  SetCount(C + L);

  for I := 0 to L - 1 do
    FData[C + I] := AArray[I];

  Result := C;
end;

function TByteArray.AddArray(const AArray: TByteArray): NativeInt;
var
  C : NativeInt;
  I : NativeInt;
  L : NativeInt;
begin
  if not Assigned(AArray) then
    raise EArrayError.Create(SErrSourceNotAssigned);

  C := FCount;
  L := AArray.FCount;
  SetCount(C + L);

  for I := 0 to L - 1 do
    FData[C + I] := AArray.FData[I];

  Result := C;
end;

procedure TByteArray.Insert(const AIdx: NativeInt; const ACount: NativeInt = 1);
var
  C : NativeInt;
  A : NativeInt;
  N : NativeInt;
begin
  C := FCount;
  if (AIdx < 0) or (AIdx > C) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AIdx]);
  if ACount <= 0 then
    exit;

  A := ACount;
  N := C + A;
  if N > FCapacity then
    SetCount(N)
  else
    FCount := N;

  if AIdx < C then
    Move(FData[AIdx], FData[AIdx + A], (C - AIdx) * SizeOf(Byte));

  FillChar(FData[AIdx], A * SizeOf(Byte), 0);
end;

procedure TByteArray.Delete(const AIdx: NativeInt; const ACount: NativeInt = 1);
var
  A : NativeInt;
  C : NativeInt;
  L : NativeInt;
begin
  A := ACount;
  if A <= 0 then
    exit;

  C := FCount;
  if (AIdx < 0) or (AIdx >= C) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AIdx]);

  L := AIdx + A;
  if L > C then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AIdx]);

  if L < C then    Move(FData[AIdx + A], FData[AIdx], SizeOf(Byte) * (C - AIdx - A));

  SetCount(C - A);
end;

function TByteArray.CompareItems(const AIdx1, AIdx2: NativeInt): Int32;
var
  I, J : Byte;
begin
  Assert(AIdx1 >= 0);
  Assert(AIdx1 < FCount);
  Assert(AIdx2 >= 0);
  Assert(AIdx2 < FCount);

  I := FData[AIdx1];
  J := FData[AIdx2];
  if I < J then
    Result := -1
  else
  if I > J then
    Result := 1
  else
    Result := 0;
end;

procedure TByteArray.Sort;

  procedure QuickSort(L, R: NativeInt);
  var
    I : NativeInt;
    J : NativeInt;
    M : NativeInt;
    T : Byte;
  begin
    repeat
      I := L;
      J := R;
      M := (L + R) shr 1;
      repeat
        while CompareItems(I, M) < 0 do
          Inc(I);
        while CompareItems(J, M) > 0 do
          Dec(J);
        if I <= J then
          begin
            T := FData[I];
            FData[I] := FData[J];
            FData[J] := T;
            if M = I then
              M := J
            else
            if M = J then
              M := I;
            Inc(I);
            Dec(J);
          end;
      until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    until I >= R;
  end;

var
  C : NativeInt;
begin
  C := Count;
  if C > 0 then
    QuickSort(0, C - 1);
end;

procedure TByteArray.Fill(const AIdx, ACount: NativeInt; const AValue: Byte);
var
  I : NativeInt;
begin
  for I := AIdx to AIdx + ACount - 1 do
    FData[I] := AValue;
end;

function TByteArray.GetRange(const ALoIdx, AHiIdx: NativeInt): ByteArray;
var
  L : NativeInt;
  H : NativeInt;
begin
  {$IFOPT R+}
  if (ALoIdx < 0) or (ALoIdx >= FCount) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [ALoIdx]);
  if (AHiIdx < 0) or (AHiIdx >= FCount) or (AHiIdx < ALoIdx) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AHiIdx]);
  {$ELSE}
  Assert((ALoIdx >= 0) and (ALoIdx < FCount));
  Assert((AHiIdx >= 0) and (AHiIdx < FCount) and (AHiIdx >= ALoIdx));
  {$ENDIF}

  L := MaxNativeInt(0, ALoIdx);
  H := MinNativeInt(AHiIdx, FCount);
  if H >= L then
    Result := Copy(FData, L, H - L + 1)
  else
    Result := nil;
end;

procedure TByteArray.SetRange(const ALoIdx, AHiIdx: NativeInt; const V: ByteArray);
var
  L : NativeInt;
  H : NativeInt;
  C : NativeInt;
begin
  {$IFOPT R+}
  if (ALoIdx < 0) or (ALoIdx >= FCount) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [ALoIdx]);
  if (AHiIdx < 0) or (AHiIdx >= FCount) or (AHiIdx < ALoIdx) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AHiIdx]);
  {$ELSE}
  Assert((ALoIdx >= 0) and (ALoIdx < FCount));
  Assert((AHiIdx >= 0) and (AHiIdx < FCount) and (AHiIdx >= ALoIdx));
  {$ENDIF}

  L := MaxNativeInt(0, ALoIdx);
  H := MinNativeInt(AHiIdx, FCount);
  C := MaxNativeInt(MinNativeInt(Length(V), H - L + 1), 0);
  if C > 0 then
    Move(V[0], FData[L], C * Sizeof(Byte));
end;

function TByteArray.GetItemAsString(const AIdx: NativeInt): String;
begin
  Result := IntToStr(GetItem(AIdx));
end;

function TByteArray.GetAsString: String;
var
  I : NativeInt;
  L : NativeInt;
begin
  L := FCount;
  if L = 0 then
    begin
      Result := '';
      exit;
    end;
  Result := GetItemAsString(0);
  for I := 1 to L - 1 do
    Result := Result + ',' + GetItemAsString(I);
end;

procedure TByteArray.SetItemAsString(const AIdx: NativeInt; const AValue: String);
begin
  SetItem(AIdx, StrToInt(AValue));
end;

procedure TByteArray.SetAsString(const S: String);
var
  L : NativeInt;
  F : NativeInt;
  C : NativeInt;
  G : NativeInt;
begin
  L := Length(S);
  if L = 0 then
    begin
      Count := 0;
      exit;
    end;
  L := 0;
  F := 1;
  C := Length(S);
  while F < C do
    begin
      G := 0;
      while (F + G <= C) and (S[F + G] <> ',') do
        Inc(G);
      Inc(L);
      Count := L;
      SetItemAsString(L - 1, Copy(S, F, G));
      Inc(F, G + 1);
    end;
end;


{                                                                              }
{ TWord32Array                                                                 }
{                                                                              }
class function TWord32Array.CreateInstance: TWord32Array;
begin
  Result := TWord32Array.Create;
end;

constructor TWord32Array.Create;
begin
  inherited Create;
end;

constructor TWord32Array.Create(const V: Word32Array);
begin
  inherited Create;
  SetData(V);
end;

procedure TWord32Array.SetData(const AData: Word32Array);
begin
  FData := AData;
  FCount := Length(FData);
  FCapacity := FCount;
end;

procedure TWord32Array.Clear;
begin
  FData := nil;
  FCapacity := 0;
  FCount := 0;
end;

procedure TWord32Array.Assign(const ASource: Word32Array);
begin
  SetData(Copy(ASource));
end;

procedure TWord32Array.Assign(const ASource: Array of Word32);
var
  H : NativeInt;
  L : NativeInt;
  I : NativeInt;
begin
  H := High(ASource);
  L := H + 1;
  SetLength(FData, L);
  for I := 0 to H do
    FData[I] := ASource[I];
  FCount := L;
  FCapacity := L;
end;

procedure TWord32Array.Assign(const ASource: TWord32Array);
var
  D : Word32Array;
begin
  if not Assigned(ASource) then
    raise EArrayError.Create(SErrSourceNotAssigned);

  D := Copy(ASource.FData);
  SetLength(D, ASource.FCount);
  SetData(D);
end;

function TWord32Array.Duplicate: TWord32Array;
var
  Obj : TWord32Array;
begin
  try
    Obj := CreateInstance;
    try
      Obj.Assign(self);
    except
      Obj.Free;
      raise;
    end;
  except
    on E : Exception do
      raise EArrayError.CreateFmt(SErrCannotDuplicate, [ClassName, E.Message]);
  end;
  Result := Obj;
end;

function TWord32Array.IsEqual(const V: TWord32Array): Boolean;
var
  I, L : NativeInt;
begin
  L := V.Count;
  Result := L = Count;
  if not Result then
    exit;
  for I := 0 to L - 1 do
    if FData[I] <> V.FData[I] then
      begin
        Result := False;
        exit;
      end;
end;

procedure TWord32Array.SetCount(const ANewCount: NativeInt);
var
  L : NativeInt;
  C : NativeInt;
  N : NativeInt;
begin
  N := ANewCount;
  if N < 0 then
    raise EArrayError.CreateFmt(SErrInvalidCountValue, [N]);
  C := FCount;
  if N = C then
    exit;

  FCount := N;
  L := FCapacity;
  if L > 0 then
    if N < 16 then // pre-allocate first 16 entries
      N := 16
    else
    if N > L then
      N := N + N shr 3 // pre-allocate 1/8th extra if growing
    else
    if N > L shr 1 then // only reduce capacity if size is at least half
      exit;

  if N <> L then
    begin
      SetLength(FData, N);
      if N > L then
        FillChar(FData[L], SizeOf(Word32) * (N - L), 0);
      
      FCapacity := N;
    end;
end;

function TWord32Array.GetItem(const AIdx: NativeInt): Word32;
begin
  {$IFOPT R+}
  if (AIdx < 0) or (AIdx >= FCount) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AIdx]);
  {$ELSE}
  Assert(AIdx >= 0);
  Assert(AIdx < FCount);
  {$ENDIF}

  Result := FData[AIdx];
end;

procedure TWord32Array.SetItem(const AIdx: NativeInt; const AValue: Word32);
begin
  {$IFOPT R+}
  if (AIdx < 0) or (AIdx >= FCount) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AIdx]);
  {$ELSE}
  Assert(AIdx >= 0);
  Assert(AIdx < FCount);
  {$ENDIF}

  FData[AIdx] := AValue;
end;

function TWord32Array.PosNext(
         const AItem: Word32;
         const APrevPos: NativeInt;
         const IsSortedAscending: Boolean): NativeInt;
var
  F : NativeInt;
  I : NativeInt;
  L : NativeInt;
  H : NativeInt;
  D : Word32;
begin
  F := APrevPos + 1;
  if F < 0 then
    F := 0;
  if IsSortedAscending then // binary search
    begin
      if F = 0 then // find first
        begin
          L := 0;
          H := Count - 1;
          repeat
            I := (L + H) div 2;
            D := FData[I];
            if D = AItem then
              begin
                while (I > 0) and (FData[I - 1] = AItem) do
                  Dec(I);
                Result := I;
                exit;
              end
            else
            if D > AItem then
              H := I - 1
            else
              L := I + 1;
          until L > H;
          Result := -1;
        end
      else // find next
        if APrevPos >= Count - 1 then
          Result := -1
        else
          if FData[APrevPos + 1] = AItem then
            Result := APrevPos + 1
          else
            Result := -1;
    end
  else // linear search
    begin
      for I := F to Count - 1 do
        if FData[I] = AItem then
          begin
            Result := I;
            exit;
          end;
      Result := -1;
    end;
end;

function TWord32Array.GetIndex(const AValue: Word32): NativeInt;
begin
  Result := PosNext(AValue, -1, False);
end;

function TWord32Array.HasValue(const AValue: Word32): Boolean;
begin
  Result := PosNext(AValue, -1, False) >= 0;
end;

function TWord32Array.Add(const AValue: Word32): NativeInt;
var
  C : NativeInt;
begin
  C := FCount;
  if C >= FCapacity then
    SetCount(C + 1)
  else
    FCount := C + 1;
  FData[C] := AValue;
  Result := C;
end;

function TWord32Array.AddIfNotExists(const AValue: Word32): NativeInt;
var
  I : NativeInt;
begin
  I := PosNext(AValue, -1);
  if I < 0 then
    I := Add(AValue);
  Result := I;
end;

function TWord32Array.AddArray(const AArray: Word32Array): NativeInt;
var
  C : NativeInt;
  I : NativeInt;
  L : NativeInt;
begin
  C := FCount;
  L := Length(AArray);
  SetCount(C + L);

  for I := 0 to L - 1 do
    FData[C + I] := AArray[I];

  Result := C;
end;

function TWord32Array.AddArray(const AArray: TWord32Array): NativeInt;
var
  C : NativeInt;
  I : NativeInt;
  L : NativeInt;
begin
  if not Assigned(AArray) then
    raise EArrayError.Create(SErrSourceNotAssigned);

  C := FCount;
  L := AArray.FCount;
  SetCount(C + L);

  for I := 0 to L - 1 do
    FData[C + I] := AArray.FData[I];

  Result := C;
end;

procedure TWord32Array.Insert(const AIdx: NativeInt; const ACount: NativeInt = 1);
var
  C : NativeInt;
  A : NativeInt;
  N : NativeInt;
begin
  C := FCount;
  if (AIdx < 0) or (AIdx > C) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AIdx]);
  if ACount <= 0 then
    exit;

  A := ACount;
  N := C + A;
  if N > FCapacity then
    SetCount(N)
  else
    FCount := N;

  if AIdx < C then
    Move(FData[AIdx], FData[AIdx + A], (C - AIdx) * SizeOf(Word32));

  FillChar(FData[AIdx], A * SizeOf(Word32), 0);
end;

procedure TWord32Array.Delete(const AIdx: NativeInt; const ACount: NativeInt = 1);
var
  A : NativeInt;
  C : NativeInt;
  L : NativeInt;
begin
  A := ACount;
  if A <= 0 then
    exit;

  C := FCount;
  if (AIdx < 0) or (AIdx >= C) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AIdx]);

  L := AIdx + A;
  if L > C then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AIdx]);

  if L < C then    Move(FData[AIdx + A], FData[AIdx], SizeOf(Word32) * (C - AIdx - A));

  SetCount(C - A);
end;

function TWord32Array.CompareItems(const AIdx1, AIdx2: NativeInt): Int32;
var
  I, J : Word32;
begin
  Assert(AIdx1 >= 0);
  Assert(AIdx1 < FCount);
  Assert(AIdx2 >= 0);
  Assert(AIdx2 < FCount);

  I := FData[AIdx1];
  J := FData[AIdx2];
  if I < J then
    Result := -1
  else
  if I > J then
    Result := 1
  else
    Result := 0;
end;

procedure TWord32Array.Sort;

  procedure QuickSort(L, R: NativeInt);
  var
    I : NativeInt;
    J : NativeInt;
    M : NativeInt;
    T : Word32;
  begin
    repeat
      I := L;
      J := R;
      M := (L + R) shr 1;
      repeat
        while CompareItems(I, M) < 0 do
          Inc(I);
        while CompareItems(J, M) > 0 do
          Dec(J);
        if I <= J then
          begin
            T := FData[I];
            FData[I] := FData[J];
            FData[J] := T;
            if M = I then
              M := J
            else
            if M = J then
              M := I;
            Inc(I);
            Dec(J);
          end;
      until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    until I >= R;
  end;

var
  C : NativeInt;
begin
  C := Count;
  if C > 0 then
    QuickSort(0, C - 1);
end;

procedure TWord32Array.Fill(const AIdx, ACount: NativeInt; const AValue: Word32);
var
  I : NativeInt;
begin
  for I := AIdx to AIdx + ACount - 1 do
    FData[I] := AValue;
end;

function TWord32Array.GetRange(const ALoIdx, AHiIdx: NativeInt): Word32Array;
var
  L : NativeInt;
  H : NativeInt;
begin
  {$IFOPT R+}
  if (ALoIdx < 0) or (ALoIdx >= FCount) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [ALoIdx]);
  if (AHiIdx < 0) or (AHiIdx >= FCount) or (AHiIdx < ALoIdx) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AHiIdx]);
  {$ELSE}
  Assert((ALoIdx >= 0) and (ALoIdx < FCount));
  Assert((AHiIdx >= 0) and (AHiIdx < FCount) and (AHiIdx >= ALoIdx));
  {$ENDIF}

  L := MaxNativeInt(0, ALoIdx);
  H := MinNativeInt(AHiIdx, FCount);
  if H >= L then
    Result := Copy(FData, L, H - L + 1)
  else
    Result := nil;
end;

procedure TWord32Array.SetRange(const ALoIdx, AHiIdx: NativeInt; const V: Word32Array);
var
  L : NativeInt;
  H : NativeInt;
  C : NativeInt;
begin
  {$IFOPT R+}
  if (ALoIdx < 0) or (ALoIdx >= FCount) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [ALoIdx]);
  if (AHiIdx < 0) or (AHiIdx >= FCount) or (AHiIdx < ALoIdx) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AHiIdx]);
  {$ELSE}
  Assert((ALoIdx >= 0) and (ALoIdx < FCount));
  Assert((AHiIdx >= 0) and (AHiIdx < FCount) and (AHiIdx >= ALoIdx));
  {$ENDIF}

  L := MaxNativeInt(0, ALoIdx);
  H := MinNativeInt(AHiIdx, FCount);
  C := MaxNativeInt(MinNativeInt(Length(V), H - L + 1), 0);
  if C > 0 then
    Move(V[0], FData[L], C * Sizeof(Word32));
end;

function TWord32Array.GetItemAsString(const AIdx: NativeInt): String;
begin
  Result := IntToStr(GetItem(AIdx));
end;

function TWord32Array.GetAsString: String;
var
  I : NativeInt;
  L : NativeInt;
begin
  L := FCount;
  if L = 0 then
    begin
      Result := '';
      exit;
    end;
  Result := GetItemAsString(0);
  for I := 1 to L - 1 do
    Result := Result + ',' + GetItemAsString(I);
end;

procedure TWord32Array.SetItemAsString(const AIdx: NativeInt; const AValue: String);
begin
  SetItem(AIdx, StrToInt(AValue));
end;

procedure TWord32Array.SetAsString(const S: String);
var
  L : NativeInt;
  F : NativeInt;
  C : NativeInt;
  G : NativeInt;
begin
  L := Length(S);
  if L = 0 then
    begin
      Count := 0;
      exit;
    end;
  L := 0;
  F := 1;
  C := Length(S);
  while F < C do
    begin
      G := 0;
      while (F + G <= C) and (S[F + G] <> ',') do
        Inc(G);
      Inc(L);
      Count := L;
      SetItemAsString(L - 1, Copy(S, F, G));
      Inc(F, G + 1);
    end;
end;


{                                                                              }
{ TWord64Array                                                                 }
{                                                                              }
class function TWord64Array.CreateInstance: TWord64Array;
begin
  Result := TWord64Array.Create;
end;

constructor TWord64Array.Create;
begin
  inherited Create;
end;

constructor TWord64Array.Create(const V: Word64Array);
begin
  inherited Create;
  SetData(V);
end;

procedure TWord64Array.SetData(const AData: Word64Array);
begin
  FData := AData;
  FCount := Length(FData);
  FCapacity := FCount;
end;

procedure TWord64Array.Clear;
begin
  FData := nil;
  FCapacity := 0;
  FCount := 0;
end;

procedure TWord64Array.Assign(const ASource: Word64Array);
begin
  SetData(Copy(ASource));
end;

procedure TWord64Array.Assign(const ASource: Array of Word64);
var
  H : NativeInt;
  L : NativeInt;
  I : NativeInt;
begin
  H := High(ASource);
  L := H + 1;
  SetLength(FData, L);
  for I := 0 to H do
    FData[I] := ASource[I];
  FCount := L;
  FCapacity := L;
end;

procedure TWord64Array.Assign(const ASource: TWord64Array);
var
  D : Word64Array;
begin
  if not Assigned(ASource) then
    raise EArrayError.Create(SErrSourceNotAssigned);

  D := Copy(ASource.FData);
  SetLength(D, ASource.FCount);
  SetData(D);
end;

function TWord64Array.Duplicate: TWord64Array;
var
  Obj : TWord64Array;
begin
  try
    Obj := CreateInstance;
    try
      Obj.Assign(self);
    except
      Obj.Free;
      raise;
    end;
  except
    on E : Exception do
      raise EArrayError.CreateFmt(SErrCannotDuplicate, [ClassName, E.Message]);
  end;
  Result := Obj;
end;

function TWord64Array.IsEqual(const V: TWord64Array): Boolean;
var
  I, L : NativeInt;
begin
  L := V.Count;
  Result := L = Count;
  if not Result then
    exit;
  for I := 0 to L - 1 do
    if FData[I] <> V.FData[I] then
      begin
        Result := False;
        exit;
      end;
end;

procedure TWord64Array.SetCount(const ANewCount: NativeInt);
var
  L : NativeInt;
  C : NativeInt;
  N : NativeInt;
begin
  N := ANewCount;
  if N < 0 then
    raise EArrayError.CreateFmt(SErrInvalidCountValue, [N]);
  C := FCount;
  if N = C then
    exit;

  FCount := N;
  L := FCapacity;
  if L > 0 then
    if N < 16 then // pre-allocate first 16 entries
      N := 16
    else
    if N > L then
      N := N + N shr 3 // pre-allocate 1/8th extra if growing
    else
    if N > L shr 1 then // only reduce capacity if size is at least half
      exit;

  if N <> L then
    begin
      SetLength(FData, N);
      if N > L then
        FillChar(FData[L], SizeOf(Word64) * (N - L), 0);
      
      FCapacity := N;
    end;
end;

function TWord64Array.GetItem(const AIdx: NativeInt): Word64;
begin
  {$IFOPT R+}
  if (AIdx < 0) or (AIdx >= FCount) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AIdx]);
  {$ELSE}
  Assert(AIdx >= 0);
  Assert(AIdx < FCount);
  {$ENDIF}

  Result := FData[AIdx];
end;

procedure TWord64Array.SetItem(const AIdx: NativeInt; const AValue: Word64);
begin
  {$IFOPT R+}
  if (AIdx < 0) or (AIdx >= FCount) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AIdx]);
  {$ELSE}
  Assert(AIdx >= 0);
  Assert(AIdx < FCount);
  {$ENDIF}

  FData[AIdx] := AValue;
end;

function TWord64Array.PosNext(
         const AItem: Word64;
         const APrevPos: NativeInt;
         const IsSortedAscending: Boolean): NativeInt;
var
  F : NativeInt;
  I : NativeInt;
  L : NativeInt;
  H : NativeInt;
  D : Word64;
begin
  F := APrevPos + 1;
  if F < 0 then
    F := 0;
  if IsSortedAscending then // binary search
    begin
      if F = 0 then // find first
        begin
          L := 0;
          H := Count - 1;
          repeat
            I := (L + H) div 2;
            D := FData[I];
            if D = AItem then
              begin
                while (I > 0) and (FData[I - 1] = AItem) do
                  Dec(I);
                Result := I;
                exit;
              end
            else
            if D > AItem then
              H := I - 1
            else
              L := I + 1;
          until L > H;
          Result := -1;
        end
      else // find next
        if APrevPos >= Count - 1 then
          Result := -1
        else
          if FData[APrevPos + 1] = AItem then
            Result := APrevPos + 1
          else
            Result := -1;
    end
  else // linear search
    begin
      for I := F to Count - 1 do
        if FData[I] = AItem then
          begin
            Result := I;
            exit;
          end;
      Result := -1;
    end;
end;

function TWord64Array.GetIndex(const AValue: Word64): NativeInt;
begin
  Result := PosNext(AValue, -1, False);
end;

function TWord64Array.HasValue(const AValue: Word64): Boolean;
begin
  Result := PosNext(AValue, -1, False) >= 0;
end;

function TWord64Array.Add(const AValue: Word64): NativeInt;
var
  C : NativeInt;
begin
  C := FCount;
  if C >= FCapacity then
    SetCount(C + 1)
  else
    FCount := C + 1;
  FData[C] := AValue;
  Result := C;
end;

function TWord64Array.AddIfNotExists(const AValue: Word64): NativeInt;
var
  I : NativeInt;
begin
  I := PosNext(AValue, -1);
  if I < 0 then
    I := Add(AValue);
  Result := I;
end;

function TWord64Array.AddArray(const AArray: Word64Array): NativeInt;
var
  C : NativeInt;
  I : NativeInt;
  L : NativeInt;
begin
  C := FCount;
  L := Length(AArray);
  SetCount(C + L);

  for I := 0 to L - 1 do
    FData[C + I] := AArray[I];

  Result := C;
end;

function TWord64Array.AddArray(const AArray: TWord64Array): NativeInt;
var
  C : NativeInt;
  I : NativeInt;
  L : NativeInt;
begin
  if not Assigned(AArray) then
    raise EArrayError.Create(SErrSourceNotAssigned);

  C := FCount;
  L := AArray.FCount;
  SetCount(C + L);

  for I := 0 to L - 1 do
    FData[C + I] := AArray.FData[I];

  Result := C;
end;

procedure TWord64Array.Insert(const AIdx: NativeInt; const ACount: NativeInt = 1);
var
  C : NativeInt;
  A : NativeInt;
  N : NativeInt;
begin
  C := FCount;
  if (AIdx < 0) or (AIdx > C) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AIdx]);
  if ACount <= 0 then
    exit;

  A := ACount;
  N := C + A;
  if N > FCapacity then
    SetCount(N)
  else
    FCount := N;

  if AIdx < C then
    Move(FData[AIdx], FData[AIdx + A], (C - AIdx) * SizeOf(Word64));

  FillChar(FData[AIdx], A * SizeOf(Word64), 0);
end;

procedure TWord64Array.Delete(const AIdx: NativeInt; const ACount: NativeInt = 1);
var
  A : NativeInt;
  C : NativeInt;
  L : NativeInt;
begin
  A := ACount;
  if A <= 0 then
    exit;

  C := FCount;
  if (AIdx < 0) or (AIdx >= C) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AIdx]);

  L := AIdx + A;
  if L > C then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AIdx]);

  if L < C then    Move(FData[AIdx + A], FData[AIdx], SizeOf(Word64) * (C - AIdx - A));

  SetCount(C - A);
end;

function TWord64Array.CompareItems(const AIdx1, AIdx2: NativeInt): Int32;
var
  I, J : Word64;
begin
  Assert(AIdx1 >= 0);
  Assert(AIdx1 < FCount);
  Assert(AIdx2 >= 0);
  Assert(AIdx2 < FCount);

  I := FData[AIdx1];
  J := FData[AIdx2];
  if I < J then
    Result := -1
  else
  if I > J then
    Result := 1
  else
    Result := 0;
end;

procedure TWord64Array.Sort;

  procedure QuickSort(L, R: NativeInt);
  var
    I : NativeInt;
    J : NativeInt;
    M : NativeInt;
    T : Word64;
  begin
    repeat
      I := L;
      J := R;
      M := (L + R) shr 1;
      repeat
        while CompareItems(I, M) < 0 do
          Inc(I);
        while CompareItems(J, M) > 0 do
          Dec(J);
        if I <= J then
          begin
            T := FData[I];
            FData[I] := FData[J];
            FData[J] := T;
            if M = I then
              M := J
            else
            if M = J then
              M := I;
            Inc(I);
            Dec(J);
          end;
      until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    until I >= R;
  end;

var
  C : NativeInt;
begin
  C := Count;
  if C > 0 then
    QuickSort(0, C - 1);
end;

procedure TWord64Array.Fill(const AIdx, ACount: NativeInt; const AValue: Word64);
var
  I : NativeInt;
begin
  for I := AIdx to AIdx + ACount - 1 do
    FData[I] := AValue;
end;

function TWord64Array.GetRange(const ALoIdx, AHiIdx: NativeInt): Word64Array;
var
  L : NativeInt;
  H : NativeInt;
begin
  {$IFOPT R+}
  if (ALoIdx < 0) or (ALoIdx >= FCount) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [ALoIdx]);
  if (AHiIdx < 0) or (AHiIdx >= FCount) or (AHiIdx < ALoIdx) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AHiIdx]);
  {$ELSE}
  Assert((ALoIdx >= 0) and (ALoIdx < FCount));
  Assert((AHiIdx >= 0) and (AHiIdx < FCount) and (AHiIdx >= ALoIdx));
  {$ENDIF}

  L := MaxNativeInt(0, ALoIdx);
  H := MinNativeInt(AHiIdx, FCount);
  if H >= L then
    Result := Copy(FData, L, H - L + 1)
  else
    Result := nil;
end;

procedure TWord64Array.SetRange(const ALoIdx, AHiIdx: NativeInt; const V: Word64Array);
var
  L : NativeInt;
  H : NativeInt;
  C : NativeInt;
begin
  {$IFOPT R+}
  if (ALoIdx < 0) or (ALoIdx >= FCount) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [ALoIdx]);
  if (AHiIdx < 0) or (AHiIdx >= FCount) or (AHiIdx < ALoIdx) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AHiIdx]);
  {$ELSE}
  Assert((ALoIdx >= 0) and (ALoIdx < FCount));
  Assert((AHiIdx >= 0) and (AHiIdx < FCount) and (AHiIdx >= ALoIdx));
  {$ENDIF}

  L := MaxNativeInt(0, ALoIdx);
  H := MinNativeInt(AHiIdx, FCount);
  C := MaxNativeInt(MinNativeInt(Length(V), H - L + 1), 0);
  if C > 0 then
    Move(V[0], FData[L], C * Sizeof(Word64));
end;




{                                                                              }
{ TSingleArray                                                                 }
{                                                                              }
class function TSingleArray.CreateInstance: TSingleArray;
begin
  Result := TSingleArray.Create;
end;

constructor TSingleArray.Create;
begin
  inherited Create;
end;

constructor TSingleArray.Create(const V: SingleArray);
begin
  inherited Create;
  SetData(V);
end;

procedure TSingleArray.SetData(const AData: SingleArray);
begin
  FData := AData;
  FCount := Length(FData);
  FCapacity := FCount;
end;

procedure TSingleArray.Clear;
begin
  FData := nil;
  FCapacity := 0;
  FCount := 0;
end;

procedure TSingleArray.Assign(const ASource: SingleArray);
begin
  SetData(Copy(ASource));
end;

procedure TSingleArray.Assign(const ASource: Array of Single);
var
  H : NativeInt;
  L : NativeInt;
  I : NativeInt;
begin
  H := High(ASource);
  L := H + 1;
  SetLength(FData, L);
  for I := 0 to H do
    FData[I] := ASource[I];
  FCount := L;
  FCapacity := L;
end;

procedure TSingleArray.Assign(const ASource: TSingleArray);
var
  D : SingleArray;
begin
  if not Assigned(ASource) then
    raise EArrayError.Create(SErrSourceNotAssigned);

  D := Copy(ASource.FData);
  SetLength(D, ASource.FCount);
  SetData(D);
end;

function TSingleArray.Duplicate: TSingleArray;
var
  Obj : TSingleArray;
begin
  try
    Obj := CreateInstance;
    try
      Obj.Assign(self);
    except
      Obj.Free;
      raise;
    end;
  except
    on E : Exception do
      raise EArrayError.CreateFmt(SErrCannotDuplicate, [ClassName, E.Message]);
  end;
  Result := Obj;
end;

function TSingleArray.IsEqual(const V: TSingleArray): Boolean;
var
  I, L : NativeInt;
begin
  L := V.Count;
  Result := L = Count;
  if not Result then
    exit;
  for I := 0 to L - 1 do
    if FData[I] <> V.FData[I] then
      begin
        Result := False;
        exit;
      end;
end;

procedure TSingleArray.SetCount(const ANewCount: NativeInt);
var
  L : NativeInt;
  C : NativeInt;
  N : NativeInt;
begin
  N := ANewCount;
  if N < 0 then
    raise EArrayError.CreateFmt(SErrInvalidCountValue, [N]);
  C := FCount;
  if N = C then
    exit;

  FCount := N;
  L := FCapacity;
  if L > 0 then
    if N < 16 then // pre-allocate first 16 entries
      N := 16
    else
    if N > L then
      N := N + N shr 3 // pre-allocate 1/8th extra if growing
    else
    if N > L shr 1 then // only reduce capacity if size is at least half
      exit;

  if N <> L then
    begin
      SetLength(FData, N);
      if N > L then
        FillChar(FData[L], SizeOf(Single) * (N - L), 0);
      
      FCapacity := N;
    end;
end;

function TSingleArray.GetItem(const AIdx: NativeInt): Single;
begin
  {$IFOPT R+}
  if (AIdx < 0) or (AIdx >= FCount) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AIdx]);
  {$ELSE}
  Assert(AIdx >= 0);
  Assert(AIdx < FCount);
  {$ENDIF}

  Result := FData[AIdx];
end;

procedure TSingleArray.SetItem(const AIdx: NativeInt; const AValue: Single);
begin
  {$IFOPT R+}
  if (AIdx < 0) or (AIdx >= FCount) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AIdx]);
  {$ELSE}
  Assert(AIdx >= 0);
  Assert(AIdx < FCount);
  {$ENDIF}

  FData[AIdx] := AValue;
end;

function TSingleArray.PosNext(
         const AItem: Single;
         const APrevPos: NativeInt;
         const IsSortedAscending: Boolean): NativeInt;
var
  F : NativeInt;
  I : NativeInt;
  L : NativeInt;
  H : NativeInt;
  D : Single;
begin
  F := APrevPos + 1;
  if F < 0 then
    F := 0;
  if IsSortedAscending then // binary search
    begin
      if F = 0 then // find first
        begin
          L := 0;
          H := Count - 1;
          repeat
            I := (L + H) div 2;
            D := FData[I];
            if D = AItem then
              begin
                while (I > 0) and (FData[I - 1] = AItem) do
                  Dec(I);
                Result := I;
                exit;
              end
            else
            if D > AItem then
              H := I - 1
            else
              L := I + 1;
          until L > H;
          Result := -1;
        end
      else // find next
        if APrevPos >= Count - 1 then
          Result := -1
        else
          if FData[APrevPos + 1] = AItem then
            Result := APrevPos + 1
          else
            Result := -1;
    end
  else // linear search
    begin
      for I := F to Count - 1 do
        if FData[I] = AItem then
          begin
            Result := I;
            exit;
          end;
      Result := -1;
    end;
end;

function TSingleArray.GetIndex(const AValue: Single): NativeInt;
begin
  Result := PosNext(AValue, -1, False);
end;

function TSingleArray.HasValue(const AValue: Single): Boolean;
begin
  Result := PosNext(AValue, -1, False) >= 0;
end;

function TSingleArray.Add(const AValue: Single): NativeInt;
var
  C : NativeInt;
begin
  C := FCount;
  if C >= FCapacity then
    SetCount(C + 1)
  else
    FCount := C + 1;
  FData[C] := AValue;
  Result := C;
end;

function TSingleArray.AddIfNotExists(const AValue: Single): NativeInt;
var
  I : NativeInt;
begin
  I := PosNext(AValue, -1);
  if I < 0 then
    I := Add(AValue);
  Result := I;
end;

function TSingleArray.AddArray(const AArray: SingleArray): NativeInt;
var
  C : NativeInt;
  I : NativeInt;
  L : NativeInt;
begin
  C := FCount;
  L := Length(AArray);
  SetCount(C + L);

  for I := 0 to L - 1 do
    FData[C + I] := AArray[I];

  Result := C;
end;

function TSingleArray.AddArray(const AArray: TSingleArray): NativeInt;
var
  C : NativeInt;
  I : NativeInt;
  L : NativeInt;
begin
  if not Assigned(AArray) then
    raise EArrayError.Create(SErrSourceNotAssigned);

  C := FCount;
  L := AArray.FCount;
  SetCount(C + L);

  for I := 0 to L - 1 do
    FData[C + I] := AArray.FData[I];

  Result := C;
end;

procedure TSingleArray.Insert(const AIdx: NativeInt; const ACount: NativeInt = 1);
var
  C : NativeInt;
  A : NativeInt;
  N : NativeInt;
begin
  C := FCount;
  if (AIdx < 0) or (AIdx > C) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AIdx]);
  if ACount <= 0 then
    exit;

  A := ACount;
  N := C + A;
  if N > FCapacity then
    SetCount(N)
  else
    FCount := N;

  if AIdx < C then
    Move(FData[AIdx], FData[AIdx + A], (C - AIdx) * SizeOf(Single));

  FillChar(FData[AIdx], A * SizeOf(Single), 0);
end;

procedure TSingleArray.Delete(const AIdx: NativeInt; const ACount: NativeInt = 1);
var
  A : NativeInt;
  C : NativeInt;
  L : NativeInt;
begin
  A := ACount;
  if A <= 0 then
    exit;

  C := FCount;
  if (AIdx < 0) or (AIdx >= C) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AIdx]);

  L := AIdx + A;
  if L > C then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AIdx]);

  if L < C then    Move(FData[AIdx + A], FData[AIdx], SizeOf(Single) * (C - AIdx - A));

  SetCount(C - A);
end;

function TSingleArray.CompareItems(const AIdx1, AIdx2: NativeInt): Int32;
var
  I, J : Single;
begin
  Assert(AIdx1 >= 0);
  Assert(AIdx1 < FCount);
  Assert(AIdx2 >= 0);
  Assert(AIdx2 < FCount);

  I := FData[AIdx1];
  J := FData[AIdx2];
  if I < J then
    Result := -1
  else
  if I > J then
    Result := 1
  else
    Result := 0;
end;

procedure TSingleArray.Sort;

  procedure QuickSort(L, R: NativeInt);
  var
    I : NativeInt;
    J : NativeInt;
    M : NativeInt;
    T : Single;
  begin
    repeat
      I := L;
      J := R;
      M := (L + R) shr 1;
      repeat
        while CompareItems(I, M) < 0 do
          Inc(I);
        while CompareItems(J, M) > 0 do
          Dec(J);
        if I <= J then
          begin
            T := FData[I];
            FData[I] := FData[J];
            FData[J] := T;
            if M = I then
              M := J
            else
            if M = J then
              M := I;
            Inc(I);
            Dec(J);
          end;
      until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    until I >= R;
  end;

var
  C : NativeInt;
begin
  C := Count;
  if C > 0 then
    QuickSort(0, C - 1);
end;

procedure TSingleArray.Fill(const AIdx, ACount: NativeInt; const AValue: Single);
var
  I : NativeInt;
begin
  for I := AIdx to AIdx + ACount - 1 do
    FData[I] := AValue;
end;

function TSingleArray.GetRange(const ALoIdx, AHiIdx: NativeInt): SingleArray;
var
  L : NativeInt;
  H : NativeInt;
begin
  {$IFOPT R+}
  if (ALoIdx < 0) or (ALoIdx >= FCount) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [ALoIdx]);
  if (AHiIdx < 0) or (AHiIdx >= FCount) or (AHiIdx < ALoIdx) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AHiIdx]);
  {$ELSE}
  Assert((ALoIdx >= 0) and (ALoIdx < FCount));
  Assert((AHiIdx >= 0) and (AHiIdx < FCount) and (AHiIdx >= ALoIdx));
  {$ENDIF}

  L := MaxNativeInt(0, ALoIdx);
  H := MinNativeInt(AHiIdx, FCount);
  if H >= L then
    Result := Copy(FData, L, H - L + 1)
  else
    Result := nil;
end;

procedure TSingleArray.SetRange(const ALoIdx, AHiIdx: NativeInt; const V: SingleArray);
var
  L : NativeInt;
  H : NativeInt;
  C : NativeInt;
begin
  {$IFOPT R+}
  if (ALoIdx < 0) or (ALoIdx >= FCount) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [ALoIdx]);
  if (AHiIdx < 0) or (AHiIdx >= FCount) or (AHiIdx < ALoIdx) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AHiIdx]);
  {$ELSE}
  Assert((ALoIdx >= 0) and (ALoIdx < FCount));
  Assert((AHiIdx >= 0) and (AHiIdx < FCount) and (AHiIdx >= ALoIdx));
  {$ENDIF}

  L := MaxNativeInt(0, ALoIdx);
  H := MinNativeInt(AHiIdx, FCount);
  C := MaxNativeInt(MinNativeInt(Length(V), H - L + 1), 0);
  if C > 0 then
    Move(V[0], FData[L], C * Sizeof(Single));
end;

function TSingleArray.GetItemAsString(const AIdx: NativeInt): String;
begin
  Result := FloatToStr(GetItem(AIdx));
end;

function TSingleArray.GetAsString: String;
var
  I : NativeInt;
  L : NativeInt;
begin
  L := FCount;
  if L = 0 then
    begin
      Result := '';
      exit;
    end;
  Result := GetItemAsString(0);
  for I := 1 to L - 1 do
    Result := Result + ',' + GetItemAsString(I);
end;

procedure TSingleArray.SetItemAsString(const AIdx: NativeInt; const AValue: String);
begin
  SetItem(AIdx, StrToFloat(AValue));
end;

procedure TSingleArray.SetAsString(const S: String);
var
  L : NativeInt;
  F : NativeInt;
  C : NativeInt;
  G : NativeInt;
begin
  L := Length(S);
  if L = 0 then
    begin
      Count := 0;
      exit;
    end;
  L := 0;
  F := 1;
  C := Length(S);
  while F < C do
    begin
      G := 0;
      while (F + G <= C) and (S[F + G] <> ',') do
        Inc(G);
      Inc(L);
      Count := L;
      SetItemAsString(L - 1, Copy(S, F, G));
      Inc(F, G + 1);
    end;
end;


{                                                                              }
{ TDoubleArray                                                                 }
{                                                                              }
class function TDoubleArray.CreateInstance: TDoubleArray;
begin
  Result := TDoubleArray.Create;
end;

constructor TDoubleArray.Create;
begin
  inherited Create;
end;

constructor TDoubleArray.Create(const V: DoubleArray);
begin
  inherited Create;
  SetData(V);
end;

procedure TDoubleArray.SetData(const AData: DoubleArray);
begin
  FData := AData;
  FCount := Length(FData);
  FCapacity := FCount;
end;

procedure TDoubleArray.Clear;
begin
  FData := nil;
  FCapacity := 0;
  FCount := 0;
end;

procedure TDoubleArray.Assign(const ASource: DoubleArray);
begin
  SetData(Copy(ASource));
end;

procedure TDoubleArray.Assign(const ASource: Array of Double);
var
  H : NativeInt;
  L : NativeInt;
  I : NativeInt;
begin
  H := High(ASource);
  L := H + 1;
  SetLength(FData, L);
  for I := 0 to H do
    FData[I] := ASource[I];
  FCount := L;
  FCapacity := L;
end;

procedure TDoubleArray.Assign(const ASource: TDoubleArray);
var
  D : DoubleArray;
begin
  if not Assigned(ASource) then
    raise EArrayError.Create(SErrSourceNotAssigned);

  D := Copy(ASource.FData);
  SetLength(D, ASource.FCount);
  SetData(D);
end;

function TDoubleArray.Duplicate: TDoubleArray;
var
  Obj : TDoubleArray;
begin
  try
    Obj := CreateInstance;
    try
      Obj.Assign(self);
    except
      Obj.Free;
      raise;
    end;
  except
    on E : Exception do
      raise EArrayError.CreateFmt(SErrCannotDuplicate, [ClassName, E.Message]);
  end;
  Result := Obj;
end;

function TDoubleArray.IsEqual(const V: TDoubleArray): Boolean;
var
  I, L : NativeInt;
begin
  L := V.Count;
  Result := L = Count;
  if not Result then
    exit;
  for I := 0 to L - 1 do
    if FData[I] <> V.FData[I] then
      begin
        Result := False;
        exit;
      end;
end;

procedure TDoubleArray.SetCount(const ANewCount: NativeInt);
var
  L : NativeInt;
  C : NativeInt;
  N : NativeInt;
begin
  N := ANewCount;
  if N < 0 then
    raise EArrayError.CreateFmt(SErrInvalidCountValue, [N]);
  C := FCount;
  if N = C then
    exit;

  FCount := N;
  L := FCapacity;
  if L > 0 then
    if N < 16 then // pre-allocate first 16 entries
      N := 16
    else
    if N > L then
      N := N + N shr 3 // pre-allocate 1/8th extra if growing
    else
    if N > L shr 1 then // only reduce capacity if size is at least half
      exit;

  if N <> L then
    begin
      SetLength(FData, N);
      if N > L then
        FillChar(FData[L], SizeOf(Double) * (N - L), 0);
      
      FCapacity := N;
    end;
end;

function TDoubleArray.GetItem(const AIdx: NativeInt): Double;
begin
  {$IFOPT R+}
  if (AIdx < 0) or (AIdx >= FCount) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AIdx]);
  {$ELSE}
  Assert(AIdx >= 0);
  Assert(AIdx < FCount);
  {$ENDIF}

  Result := FData[AIdx];
end;

procedure TDoubleArray.SetItem(const AIdx: NativeInt; const AValue: Double);
begin
  {$IFOPT R+}
  if (AIdx < 0) or (AIdx >= FCount) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AIdx]);
  {$ELSE}
  Assert(AIdx >= 0);
  Assert(AIdx < FCount);
  {$ENDIF}

  FData[AIdx] := AValue;
end;

function TDoubleArray.PosNext(
         const AItem: Double;
         const APrevPos: NativeInt;
         const IsSortedAscending: Boolean): NativeInt;
var
  F : NativeInt;
  I : NativeInt;
  L : NativeInt;
  H : NativeInt;
  D : Double;
begin
  F := APrevPos + 1;
  if F < 0 then
    F := 0;
  if IsSortedAscending then // binary search
    begin
      if F = 0 then // find first
        begin
          L := 0;
          H := Count - 1;
          repeat
            I := (L + H) div 2;
            D := FData[I];
            if D = AItem then
              begin
                while (I > 0) and (FData[I - 1] = AItem) do
                  Dec(I);
                Result := I;
                exit;
              end
            else
            if D > AItem then
              H := I - 1
            else
              L := I + 1;
          until L > H;
          Result := -1;
        end
      else // find next
        if APrevPos >= Count - 1 then
          Result := -1
        else
          if FData[APrevPos + 1] = AItem then
            Result := APrevPos + 1
          else
            Result := -1;
    end
  else // linear search
    begin
      for I := F to Count - 1 do
        if FData[I] = AItem then
          begin
            Result := I;
            exit;
          end;
      Result := -1;
    end;
end;

function TDoubleArray.GetIndex(const AValue: Double): NativeInt;
begin
  Result := PosNext(AValue, -1, False);
end;

function TDoubleArray.HasValue(const AValue: Double): Boolean;
begin
  Result := PosNext(AValue, -1, False) >= 0;
end;

function TDoubleArray.Add(const AValue: Double): NativeInt;
var
  C : NativeInt;
begin
  C := FCount;
  if C >= FCapacity then
    SetCount(C + 1)
  else
    FCount := C + 1;
  FData[C] := AValue;
  Result := C;
end;

function TDoubleArray.AddIfNotExists(const AValue: Double): NativeInt;
var
  I : NativeInt;
begin
  I := PosNext(AValue, -1);
  if I < 0 then
    I := Add(AValue);
  Result := I;
end;

function TDoubleArray.AddArray(const AArray: DoubleArray): NativeInt;
var
  C : NativeInt;
  I : NativeInt;
  L : NativeInt;
begin
  C := FCount;
  L := Length(AArray);
  SetCount(C + L);

  for I := 0 to L - 1 do
    FData[C + I] := AArray[I];

  Result := C;
end;

function TDoubleArray.AddArray(const AArray: TDoubleArray): NativeInt;
var
  C : NativeInt;
  I : NativeInt;
  L : NativeInt;
begin
  if not Assigned(AArray) then
    raise EArrayError.Create(SErrSourceNotAssigned);

  C := FCount;
  L := AArray.FCount;
  SetCount(C + L);

  for I := 0 to L - 1 do
    FData[C + I] := AArray.FData[I];

  Result := C;
end;

procedure TDoubleArray.Insert(const AIdx: NativeInt; const ACount: NativeInt = 1);
var
  C : NativeInt;
  A : NativeInt;
  N : NativeInt;
begin
  C := FCount;
  if (AIdx < 0) or (AIdx > C) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AIdx]);
  if ACount <= 0 then
    exit;

  A := ACount;
  N := C + A;
  if N > FCapacity then
    SetCount(N)
  else
    FCount := N;

  if AIdx < C then
    Move(FData[AIdx], FData[AIdx + A], (C - AIdx) * SizeOf(Double));

  FillChar(FData[AIdx], A * SizeOf(Double), 0);
end;

procedure TDoubleArray.Delete(const AIdx: NativeInt; const ACount: NativeInt = 1);
var
  A : NativeInt;
  C : NativeInt;
  L : NativeInt;
begin
  A := ACount;
  if A <= 0 then
    exit;

  C := FCount;
  if (AIdx < 0) or (AIdx >= C) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AIdx]);

  L := AIdx + A;
  if L > C then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AIdx]);

  if L < C then    Move(FData[AIdx + A], FData[AIdx], SizeOf(Double) * (C - AIdx - A));

  SetCount(C - A);
end;

function TDoubleArray.CompareItems(const AIdx1, AIdx2: NativeInt): Int32;
var
  I, J : Double;
begin
  Assert(AIdx1 >= 0);
  Assert(AIdx1 < FCount);
  Assert(AIdx2 >= 0);
  Assert(AIdx2 < FCount);

  I := FData[AIdx1];
  J := FData[AIdx2];
  if I < J then
    Result := -1
  else
  if I > J then
    Result := 1
  else
    Result := 0;
end;

procedure TDoubleArray.Sort;

  procedure QuickSort(L, R: NativeInt);
  var
    I : NativeInt;
    J : NativeInt;
    M : NativeInt;
    T : Double;
  begin
    repeat
      I := L;
      J := R;
      M := (L + R) shr 1;
      repeat
        while CompareItems(I, M) < 0 do
          Inc(I);
        while CompareItems(J, M) > 0 do
          Dec(J);
        if I <= J then
          begin
            T := FData[I];
            FData[I] := FData[J];
            FData[J] := T;
            if M = I then
              M := J
            else
            if M = J then
              M := I;
            Inc(I);
            Dec(J);
          end;
      until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    until I >= R;
  end;

var
  C : NativeInt;
begin
  C := Count;
  if C > 0 then
    QuickSort(0, C - 1);
end;

procedure TDoubleArray.Fill(const AIdx, ACount: NativeInt; const AValue: Double);
var
  I : NativeInt;
begin
  for I := AIdx to AIdx + ACount - 1 do
    FData[I] := AValue;
end;

function TDoubleArray.GetRange(const ALoIdx, AHiIdx: NativeInt): DoubleArray;
var
  L : NativeInt;
  H : NativeInt;
begin
  {$IFOPT R+}
  if (ALoIdx < 0) or (ALoIdx >= FCount) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [ALoIdx]);
  if (AHiIdx < 0) or (AHiIdx >= FCount) or (AHiIdx < ALoIdx) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AHiIdx]);
  {$ELSE}
  Assert((ALoIdx >= 0) and (ALoIdx < FCount));
  Assert((AHiIdx >= 0) and (AHiIdx < FCount) and (AHiIdx >= ALoIdx));
  {$ENDIF}

  L := MaxNativeInt(0, ALoIdx);
  H := MinNativeInt(AHiIdx, FCount);
  if H >= L then
    Result := Copy(FData, L, H - L + 1)
  else
    Result := nil;
end;

procedure TDoubleArray.SetRange(const ALoIdx, AHiIdx: NativeInt; const V: DoubleArray);
var
  L : NativeInt;
  H : NativeInt;
  C : NativeInt;
begin
  {$IFOPT R+}
  if (ALoIdx < 0) or (ALoIdx >= FCount) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [ALoIdx]);
  if (AHiIdx < 0) or (AHiIdx >= FCount) or (AHiIdx < ALoIdx) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AHiIdx]);
  {$ELSE}
  Assert((ALoIdx >= 0) and (ALoIdx < FCount));
  Assert((AHiIdx >= 0) and (AHiIdx < FCount) and (AHiIdx >= ALoIdx));
  {$ENDIF}

  L := MaxNativeInt(0, ALoIdx);
  H := MinNativeInt(AHiIdx, FCount);
  C := MaxNativeInt(MinNativeInt(Length(V), H - L + 1), 0);
  if C > 0 then
    Move(V[0], FData[L], C * Sizeof(Double));
end;

function TDoubleArray.GetItemAsString(const AIdx: NativeInt): String;
begin
  Result := FloatToStr(GetItem(AIdx));
end;

function TDoubleArray.GetAsString: String;
var
  I : NativeInt;
  L : NativeInt;
begin
  L := FCount;
  if L = 0 then
    begin
      Result := '';
      exit;
    end;
  Result := GetItemAsString(0);
  for I := 1 to L - 1 do
    Result := Result + ',' + GetItemAsString(I);
end;

procedure TDoubleArray.SetItemAsString(const AIdx: NativeInt; const AValue: String);
begin
  SetItem(AIdx, StrToFloat(AValue));
end;

procedure TDoubleArray.SetAsString(const S: String);
var
  L : NativeInt;
  F : NativeInt;
  C : NativeInt;
  G : NativeInt;
begin
  L := Length(S);
  if L = 0 then
    begin
      Count := 0;
      exit;
    end;
  L := 0;
  F := 1;
  C := Length(S);
  while F < C do
    begin
      G := 0;
      while (F + G <= C) and (S[F + G] <> ',') do
        Inc(G);
      Inc(L);
      Count := L;
      SetItemAsString(L - 1, Copy(S, F, G));
      Inc(F, G + 1);
    end;
end;


{$IFDEF SupportAnsiString}
{                                                                              }
{ TAnsiStringArray                                                             }
{                                                                              }
class function TAnsiStringArray.CreateInstance: TAnsiStringArray;
begin
  Result := TAnsiStringArray.Create;
end;

constructor TAnsiStringArray.Create;
begin
  inherited Create;
end;

constructor TAnsiStringArray.Create(const V: AnsiStringArray);
begin
  inherited Create;
  SetData(V);
end;

procedure TAnsiStringArray.SetData(const AData: AnsiStringArray);
begin
  FData := AData;
  FCount := Length(FData);
  FCapacity := FCount;
end;

procedure TAnsiStringArray.Clear;
begin
  FData := nil;
  FCapacity := 0;
  FCount := 0;
end;

procedure TAnsiStringArray.Assign(const ASource: AnsiStringArray);
begin
  SetData(Copy(ASource));
end;

procedure TAnsiStringArray.Assign(const ASource: Array of AnsiString);
var
  H : NativeInt;
  L : NativeInt;
  I : NativeInt;
begin
  H := High(ASource);
  L := H + 1;
  SetLength(FData, L);
  for I := 0 to H do
    FData[I] := ASource[I];
  FCount := L;
  FCapacity := L;
end;

procedure TAnsiStringArray.Assign(const ASource: TAnsiStringArray);
var
  D : AnsiStringArray;
begin
  if not Assigned(ASource) then
    raise EArrayError.Create(SErrSourceNotAssigned);

  D := Copy(ASource.FData);
  SetLength(D, ASource.FCount);
  SetData(D);
end;

function TAnsiStringArray.Duplicate: TAnsiStringArray;
var
  Obj : TAnsiStringArray;
begin
  try
    Obj := CreateInstance;
    try
      Obj.Assign(self);
    except
      Obj.Free;
      raise;
    end;
  except
    on E : Exception do
      raise EArrayError.CreateFmt(SErrCannotDuplicate, [ClassName, E.Message]);
  end;
  Result := Obj;
end;

function TAnsiStringArray.IsEqual(const V: TAnsiStringArray): Boolean;
var
  I, L : NativeInt;
begin
  L := V.Count;
  Result := L = Count;
  if not Result then
    exit;
  for I := 0 to L - 1 do
    if FData[I] <> V.FData[I] then
      begin
        Result := False;
        exit;
      end;
end;

procedure TAnsiStringArray.SetCount(const ANewCount: NativeInt);
var
  L : NativeInt;
  C : NativeInt;
  N : NativeInt;
begin
  N := ANewCount;
  if N < 0 then
    raise EArrayError.CreateFmt(SErrInvalidCountValue, [N]);
  C := FCount;
  if N = C then
    exit;

  FCount := N;
  L := FCapacity;
  if L > 0 then
    if N < 16 then // pre-allocate first 16 entries
      N := 16
    else
    if N > L then
      N := N + N shr 3 // pre-allocate 1/8th extra if growing
    else
    if N > L shr 1 then // only reduce capacity if size is at least half
      exit;

  if N <> L then
    begin
      SetLength(FData, N);
      if N > L then
        FillChar(FData[L], SizeOf(AnsiString) * (N - L), 0);
      
      FCapacity := N;
    end;
end;

function TAnsiStringArray.GetItem(const AIdx: NativeInt): AnsiString;
begin
  {$IFOPT R+}
  if (AIdx < 0) or (AIdx >= FCount) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AIdx]);
  {$ELSE}
  Assert(AIdx >= 0);
  Assert(AIdx < FCount);
  {$ENDIF}

  Result := FData[AIdx];
end;

procedure TAnsiStringArray.SetItem(const AIdx: NativeInt; const AValue: AnsiString);
begin
  {$IFOPT R+}
  if (AIdx < 0) or (AIdx >= FCount) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AIdx]);
  {$ELSE}
  Assert(AIdx >= 0);
  Assert(AIdx < FCount);
  {$ENDIF}

  FData[AIdx] := AValue;
end;

function TAnsiStringArray.PosNext(
         const AItem: AnsiString;
         const APrevPos: NativeInt;
         const IsSortedAscending: Boolean): NativeInt;
var
  F : NativeInt;
  I : NativeInt;
  L : NativeInt;
  H : NativeInt;
  D : AnsiString;
begin
  F := APrevPos + 1;
  if F < 0 then
    F := 0;
  if IsSortedAscending then // binary search
    begin
      if F = 0 then // find first
        begin
          L := 0;
          H := Count - 1;
          repeat
            I := (L + H) div 2;
            D := FData[I];
            if D = AItem then
              begin
                while (I > 0) and (FData[I - 1] = AItem) do
                  Dec(I);
                Result := I;
                exit;
              end
            else
            if D > AItem then
              H := I - 1
            else
              L := I + 1;
          until L > H;
          Result := -1;
        end
      else // find next
        if APrevPos >= Count - 1 then
          Result := -1
        else
          if FData[APrevPos + 1] = AItem then
            Result := APrevPos + 1
          else
            Result := -1;
    end
  else // linear search
    begin
      for I := F to Count - 1 do
        if FData[I] = AItem then
          begin
            Result := I;
            exit;
          end;
      Result := -1;
    end;
end;

function TAnsiStringArray.GetIndex(const AValue: AnsiString): NativeInt;
begin
  Result := PosNext(AValue, -1, False);
end;

function TAnsiStringArray.HasValue(const AValue: AnsiString): Boolean;
begin
  Result := PosNext(AValue, -1, False) >= 0;
end;

function TAnsiStringArray.Add(const AValue: AnsiString): NativeInt;
var
  C : NativeInt;
begin
  C := FCount;
  if C >= FCapacity then
    SetCount(C + 1)
  else
    FCount := C + 1;
  FData[C] := AValue;
  Result := C;
end;

function TAnsiStringArray.AddIfNotExists(const AValue: AnsiString): NativeInt;
var
  I : NativeInt;
begin
  I := PosNext(AValue, -1);
  if I < 0 then
    I := Add(AValue);
  Result := I;
end;

function TAnsiStringArray.AddArray(const AArray: AnsiStringArray): NativeInt;
var
  C : NativeInt;
  I : NativeInt;
  L : NativeInt;
begin
  C := FCount;
  L := Length(AArray);
  SetCount(C + L);

  for I := 0 to L - 1 do
    FData[C + I] := AArray[I];

  Result := C;
end;

function TAnsiStringArray.AddArray(const AArray: TAnsiStringArray): NativeInt;
var
  C : NativeInt;
  I : NativeInt;
  L : NativeInt;
begin
  if not Assigned(AArray) then
    raise EArrayError.Create(SErrSourceNotAssigned);

  C := FCount;
  L := AArray.FCount;
  SetCount(C + L);

  for I := 0 to L - 1 do
    FData[C + I] := AArray.FData[I];

  Result := C;
end;

procedure TAnsiStringArray.Insert(const AIdx: NativeInt; const ACount: NativeInt = 1);
var
  C : NativeInt;
  A : NativeInt;
  N : NativeInt;
begin
  C := FCount;
  if (AIdx < 0) or (AIdx > C) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AIdx]);
  if ACount <= 0 then
    exit;

  A := ACount;
  N := C + A;
  if N > FCapacity then
    SetCount(N)
  else
    FCount := N;

  if AIdx < C then
    Move(FData[AIdx], FData[AIdx + A], (C - AIdx) * SizeOf(AnsiString));

  FillChar(FData[AIdx], A * SizeOf(AnsiString), 0);
end;

procedure TAnsiStringArray.Delete(const AIdx: NativeInt; const ACount: NativeInt = 1);
var
  A : NativeInt;
  C : NativeInt;
  L : NativeInt;
  I : NativeInt;
begin
  A := ACount;
  if A <= 0 then
    exit;

  C := FCount;
  if (AIdx < 0) or (AIdx >= C) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AIdx]);

  L := AIdx + A;
  if L > C then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AIdx]);

  for I := AIdx to L - 1 do
    FData[I] := '';

  if L < C then
    begin
      Move(FData[AIdx + A], FData[AIdx], SizeOf(AnsiString) * (C - AIdx - A));
      FillChar(FData[C - A], SizeOf(AnsiString) * A, 0);
    end;

  SetCount(C - A);
end;

function TAnsiStringArray.CompareItems(const AIdx1, AIdx2: NativeInt): Int32;
var
  I, J : AnsiString;
begin
  Assert(AIdx1 >= 0);
  Assert(AIdx1 < FCount);
  Assert(AIdx2 >= 0);
  Assert(AIdx2 < FCount);

  I := FData[AIdx1];
  J := FData[AIdx2];
  if I < J then
    Result := -1
  else
  if I > J then
    Result := 1
  else
    Result := 0;
end;

procedure TAnsiStringArray.Sort;

  procedure QuickSort(L, R: NativeInt);
  var
    I : NativeInt;
    J : NativeInt;
    M : NativeInt;
    T : AnsiString;
  begin
    repeat
      I := L;
      J := R;
      M := (L + R) shr 1;
      repeat
        while CompareItems(I, M) < 0 do
          Inc(I);
        while CompareItems(J, M) > 0 do
          Dec(J);
        if I <= J then
          begin
            T := FData[I];
            FData[I] := FData[J];
            FData[J] := T;
            if M = I then
              M := J
            else
            if M = J then
              M := I;
            Inc(I);
            Dec(J);
          end;
      until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    until I >= R;
  end;

var
  C : NativeInt;
begin
  C := Count;
  if C > 0 then
    QuickSort(0, C - 1);
end;

procedure TAnsiStringArray.Fill(const AIdx, ACount: NativeInt; const AValue: AnsiString);
var
  I : NativeInt;
begin
  for I := AIdx to AIdx + ACount - 1 do
    FData[I] := AValue;
end;

function TAnsiStringArray.GetRange(const ALoIdx, AHiIdx: NativeInt): AnsiStringArray;
var
  L : NativeInt;
  H : NativeInt;
begin
  {$IFOPT R+}
  if (ALoIdx < 0) or (ALoIdx >= FCount) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [ALoIdx]);
  if (AHiIdx < 0) or (AHiIdx >= FCount) or (AHiIdx < ALoIdx) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AHiIdx]);
  {$ELSE}
  Assert((ALoIdx >= 0) and (ALoIdx < FCount));
  Assert((AHiIdx >= 0) and (AHiIdx < FCount) and (AHiIdx >= ALoIdx));
  {$ENDIF}

  L := MaxNativeInt(0, ALoIdx);
  H := MinNativeInt(AHiIdx, FCount);
  if H >= L then
    Result := Copy(FData, L, H - L + 1)
  else
    Result := nil;
end;

procedure TAnsiStringArray.SetRange(const ALoIdx, AHiIdx: NativeInt; const V: AnsiStringArray);
var
  L : NativeInt;
  H : NativeInt;
  C : NativeInt;
begin
  {$IFOPT R+}
  if (ALoIdx < 0) or (ALoIdx >= FCount) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [ALoIdx]);
  if (AHiIdx < 0) or (AHiIdx >= FCount) or (AHiIdx < ALoIdx) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AHiIdx]);
  {$ELSE}
  Assert((ALoIdx >= 0) and (ALoIdx < FCount));
  Assert((AHiIdx >= 0) and (AHiIdx < FCount) and (AHiIdx >= ALoIdx));
  {$ENDIF}

  L := MaxNativeInt(0, ALoIdx);
  H := MinNativeInt(AHiIdx, FCount);
  C := MaxNativeInt(MinNativeInt(Length(V), H - L + 1), 0);
  if C > 0 then
    Move(V[0], FData[L], C * Sizeof(AnsiString));
end;

function TAnsiStringArray.GetItemAsString(const AIdx: NativeInt): String;
begin
  Result := String(GetItem(AIdx));
end;

function TAnsiStringArray.GetAsString: String;
var
  I : NativeInt;
  L : NativeInt;
begin
  L := FCount;
  if L = 0 then
    begin
      Result := '';
      exit;
    end;
  Result := GetItemAsString(0);
  for I := 1 to L - 1 do
    Result := Result + ',' + GetItemAsString(I);
end;

procedure TAnsiStringArray.SetItemAsString(const AIdx: NativeInt; const AValue: String);
begin
  SetItem(AIdx, AnsiString(AValue));
end;

procedure TAnsiStringArray.SetAsString(const S: String);
var
  L : NativeInt;
  F : NativeInt;
  C : NativeInt;
  G : NativeInt;
begin
  L := Length(S);
  if L = 0 then
    begin
      Count := 0;
      exit;
    end;
  L := 0;
  F := 1;
  C := Length(S);
  while F < C do
    begin
      G := 0;
      while (F + G <= C) and (S[F + G] <> ',') do
        Inc(G);
      Inc(L);
      Count := L;
      SetItemAsString(L - 1, Copy(S, F, G));
      Inc(F, G + 1);
    end;
end;


{$ENDIF}
{                                                                              }
{ TRawByteStringArray                                                          }
{                                                                              }
class function TRawByteStringArray.CreateInstance: TRawByteStringArray;
begin
  Result := TRawByteStringArray.Create;
end;

constructor TRawByteStringArray.Create;
begin
  inherited Create;
end;

constructor TRawByteStringArray.Create(const V: RawByteStringArray);
begin
  inherited Create;
  SetData(V);
end;

procedure TRawByteStringArray.SetData(const AData: RawByteStringArray);
begin
  FData := AData;
  FCount := Length(FData);
  FCapacity := FCount;
end;

procedure TRawByteStringArray.Clear;
begin
  FData := nil;
  FCapacity := 0;
  FCount := 0;
end;

procedure TRawByteStringArray.Assign(const ASource: RawByteStringArray);
begin
  SetData(Copy(ASource));
end;

procedure TRawByteStringArray.Assign(const ASource: Array of RawByteString);
var
  H : NativeInt;
  L : NativeInt;
  I : NativeInt;
begin
  H := High(ASource);
  L := H + 1;
  SetLength(FData, L);
  for I := 0 to H do
    FData[I] := ASource[I];
  FCount := L;
  FCapacity := L;
end;

procedure TRawByteStringArray.Assign(const ASource: TRawByteStringArray);
var
  D : RawByteStringArray;
begin
  if not Assigned(ASource) then
    raise EArrayError.Create(SErrSourceNotAssigned);

  D := Copy(ASource.FData);
  SetLength(D, ASource.FCount);
  SetData(D);
end;

function TRawByteStringArray.Duplicate: TRawByteStringArray;
var
  Obj : TRawByteStringArray;
begin
  try
    Obj := CreateInstance;
    try
      Obj.Assign(self);
    except
      Obj.Free;
      raise;
    end;
  except
    on E : Exception do
      raise EArrayError.CreateFmt(SErrCannotDuplicate, [ClassName, E.Message]);
  end;
  Result := Obj;
end;

function TRawByteStringArray.IsEqual(const V: TRawByteStringArray): Boolean;
var
  I, L : NativeInt;
begin
  L := V.Count;
  Result := L = Count;
  if not Result then
    exit;
  for I := 0 to L - 1 do
    if FData[I] <> V.FData[I] then
      begin
        Result := False;
        exit;
      end;
end;

procedure TRawByteStringArray.SetCount(const ANewCount: NativeInt);
var
  L : NativeInt;
  C : NativeInt;
  N : NativeInt;
begin
  N := ANewCount;
  if N < 0 then
    raise EArrayError.CreateFmt(SErrInvalidCountValue, [N]);
  C := FCount;
  if N = C then
    exit;

  FCount := N;
  L := FCapacity;
  if L > 0 then
    if N < 16 then // pre-allocate first 16 entries
      N := 16
    else
    if N > L then
      N := N + N shr 3 // pre-allocate 1/8th extra if growing
    else
    if N > L shr 1 then // only reduce capacity if size is at least half
      exit;

  if N <> L then
    begin
      SetLength(FData, N);
      if N > L then
        FillChar(FData[L], SizeOf(RawByteString) * (N - L), 0);
      
      FCapacity := N;
    end;
end;

function TRawByteStringArray.GetItem(const AIdx: NativeInt): RawByteString;
begin
  {$IFOPT R+}
  if (AIdx < 0) or (AIdx >= FCount) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AIdx]);
  {$ELSE}
  Assert(AIdx >= 0);
  Assert(AIdx < FCount);
  {$ENDIF}

  Result := FData[AIdx];
end;

procedure TRawByteStringArray.SetItem(const AIdx: NativeInt; const AValue: RawByteString);
begin
  {$IFOPT R+}
  if (AIdx < 0) or (AIdx >= FCount) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AIdx]);
  {$ELSE}
  Assert(AIdx >= 0);
  Assert(AIdx < FCount);
  {$ENDIF}

  FData[AIdx] := AValue;
end;

function TRawByteStringArray.PosNext(
         const AItem: RawByteString;
         const APrevPos: NativeInt;
         const IsSortedAscending: Boolean): NativeInt;
var
  F : NativeInt;
  I : NativeInt;
  L : NativeInt;
  H : NativeInt;
  D : RawByteString;
begin
  F := APrevPos + 1;
  if F < 0 then
    F := 0;
  if IsSortedAscending then // binary search
    begin
      if F = 0 then // find first
        begin
          L := 0;
          H := Count - 1;
          repeat
            I := (L + H) div 2;
            D := FData[I];
            if D = AItem then
              begin
                while (I > 0) and (FData[I - 1] = AItem) do
                  Dec(I);
                Result := I;
                exit;
              end
            else
            if D > AItem then
              H := I - 1
            else
              L := I + 1;
          until L > H;
          Result := -1;
        end
      else // find next
        if APrevPos >= Count - 1 then
          Result := -1
        else
          if FData[APrevPos + 1] = AItem then
            Result := APrevPos + 1
          else
            Result := -1;
    end
  else // linear search
    begin
      for I := F to Count - 1 do
        if FData[I] = AItem then
          begin
            Result := I;
            exit;
          end;
      Result := -1;
    end;
end;

function TRawByteStringArray.GetIndex(const AValue: RawByteString): NativeInt;
begin
  Result := PosNext(AValue, -1, False);
end;

function TRawByteStringArray.HasValue(const AValue: RawByteString): Boolean;
begin
  Result := PosNext(AValue, -1, False) >= 0;
end;

function TRawByteStringArray.Add(const AValue: RawByteString): NativeInt;
var
  C : NativeInt;
begin
  C := FCount;
  if C >= FCapacity then
    SetCount(C + 1)
  else
    FCount := C + 1;
  FData[C] := AValue;
  Result := C;
end;

function TRawByteStringArray.AddIfNotExists(const AValue: RawByteString): NativeInt;
var
  I : NativeInt;
begin
  I := PosNext(AValue, -1);
  if I < 0 then
    I := Add(AValue);
  Result := I;
end;

function TRawByteStringArray.AddArray(const AArray: RawByteStringArray): NativeInt;
var
  C : NativeInt;
  I : NativeInt;
  L : NativeInt;
begin
  C := FCount;
  L := Length(AArray);
  SetCount(C + L);

  for I := 0 to L - 1 do
    FData[C + I] := AArray[I];

  Result := C;
end;

function TRawByteStringArray.AddArray(const AArray: TRawByteStringArray): NativeInt;
var
  C : NativeInt;
  I : NativeInt;
  L : NativeInt;
begin
  if not Assigned(AArray) then
    raise EArrayError.Create(SErrSourceNotAssigned);

  C := FCount;
  L := AArray.FCount;
  SetCount(C + L);

  for I := 0 to L - 1 do
    FData[C + I] := AArray.FData[I];

  Result := C;
end;

procedure TRawByteStringArray.Insert(const AIdx: NativeInt; const ACount: NativeInt = 1);
var
  C : NativeInt;
  A : NativeInt;
  N : NativeInt;
begin
  C := FCount;
  if (AIdx < 0) or (AIdx > C) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AIdx]);
  if ACount <= 0 then
    exit;

  A := ACount;
  N := C + A;
  if N > FCapacity then
    SetCount(N)
  else
    FCount := N;

  if AIdx < C then
    Move(FData[AIdx], FData[AIdx + A], (C - AIdx) * SizeOf(RawByteString));

  FillChar(FData[AIdx], A * SizeOf(RawByteString), 0);
end;

procedure TRawByteStringArray.Delete(const AIdx: NativeInt; const ACount: NativeInt = 1);
var
  A : NativeInt;
  C : NativeInt;
  L : NativeInt;
  I : NativeInt;
begin
  A := ACount;
  if A <= 0 then
    exit;

  C := FCount;
  if (AIdx < 0) or (AIdx >= C) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AIdx]);

  L := AIdx + A;
  if L > C then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AIdx]);

  for I := AIdx to L - 1 do
    FData[I] := '';

  if L < C then
    begin
      Move(FData[AIdx + A], FData[AIdx], SizeOf(RawByteString) * (C - AIdx - A));
      FillChar(FData[C - A], SizeOf(RawByteString) * A, 0);
    end;

  SetCount(C - A);
end;

function TRawByteStringArray.CompareItems(const AIdx1, AIdx2: NativeInt): Int32;
var
  I, J : RawByteString;
begin
  Assert(AIdx1 >= 0);
  Assert(AIdx1 < FCount);
  Assert(AIdx2 >= 0);
  Assert(AIdx2 < FCount);

  I := FData[AIdx1];
  J := FData[AIdx2];
  if I < J then
    Result := -1
  else
  if I > J then
    Result := 1
  else
    Result := 0;
end;

procedure TRawByteStringArray.Sort;

  procedure QuickSort(L, R: NativeInt);
  var
    I : NativeInt;
    J : NativeInt;
    M : NativeInt;
    T : RawByteString;
  begin
    repeat
      I := L;
      J := R;
      M := (L + R) shr 1;
      repeat
        while CompareItems(I, M) < 0 do
          Inc(I);
        while CompareItems(J, M) > 0 do
          Dec(J);
        if I <= J then
          begin
            T := FData[I];
            FData[I] := FData[J];
            FData[J] := T;
            if M = I then
              M := J
            else
            if M = J then
              M := I;
            Inc(I);
            Dec(J);
          end;
      until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    until I >= R;
  end;

var
  C : NativeInt;
begin
  C := Count;
  if C > 0 then
    QuickSort(0, C - 1);
end;

procedure TRawByteStringArray.Fill(const AIdx, ACount: NativeInt; const AValue: RawByteString);
var
  I : NativeInt;
begin
  for I := AIdx to AIdx + ACount - 1 do
    FData[I] := AValue;
end;

function TRawByteStringArray.GetRange(const ALoIdx, AHiIdx: NativeInt): RawByteStringArray;
var
  L : NativeInt;
  H : NativeInt;
begin
  {$IFOPT R+}
  if (ALoIdx < 0) or (ALoIdx >= FCount) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [ALoIdx]);
  if (AHiIdx < 0) or (AHiIdx >= FCount) or (AHiIdx < ALoIdx) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AHiIdx]);
  {$ELSE}
  Assert((ALoIdx >= 0) and (ALoIdx < FCount));
  Assert((AHiIdx >= 0) and (AHiIdx < FCount) and (AHiIdx >= ALoIdx));
  {$ENDIF}

  L := MaxNativeInt(0, ALoIdx);
  H := MinNativeInt(AHiIdx, FCount);
  if H >= L then
    Result := Copy(FData, L, H - L + 1)
  else
    Result := nil;
end;

procedure TRawByteStringArray.SetRange(const ALoIdx, AHiIdx: NativeInt; const V: RawByteStringArray);
var
  L : NativeInt;
  H : NativeInt;
  C : NativeInt;
begin
  {$IFOPT R+}
  if (ALoIdx < 0) or (ALoIdx >= FCount) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [ALoIdx]);
  if (AHiIdx < 0) or (AHiIdx >= FCount) or (AHiIdx < ALoIdx) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AHiIdx]);
  {$ELSE}
  Assert((ALoIdx >= 0) and (ALoIdx < FCount));
  Assert((AHiIdx >= 0) and (AHiIdx < FCount) and (AHiIdx >= ALoIdx));
  {$ENDIF}

  L := MaxNativeInt(0, ALoIdx);
  H := MinNativeInt(AHiIdx, FCount);
  C := MaxNativeInt(MinNativeInt(Length(V), H - L + 1), 0);
  if C > 0 then
    Move(V[0], FData[L], C * Sizeof(RawByteString));
end;

function TRawByteStringArray.GetItemAsString(const AIdx: NativeInt): String;
begin
  Result := String(GetItem(AIdx));
end;

function TRawByteStringArray.GetAsString: String;
var
  I : NativeInt;
  L : NativeInt;
begin
  L := FCount;
  if L = 0 then
    begin
      Result := '';
      exit;
    end;
  Result := GetItemAsString(0);
  for I := 1 to L - 1 do
    Result := Result + ',' + GetItemAsString(I);
end;

procedure TRawByteStringArray.SetItemAsString(const AIdx: NativeInt; const AValue: String);
begin
  SetItem(AIdx, RawByteString(AValue));
end;

procedure TRawByteStringArray.SetAsString(const S: String);
var
  L : NativeInt;
  F : NativeInt;
  C : NativeInt;
  G : NativeInt;
begin
  L := Length(S);
  if L = 0 then
    begin
      Count := 0;
      exit;
    end;
  L := 0;
  F := 1;
  C := Length(S);
  while F < C do
    begin
      G := 0;
      while (F + G <= C) and (S[F + G] <> ',') do
        Inc(G);
      Inc(L);
      Count := L;
      SetItemAsString(L - 1, Copy(S, F, G));
      Inc(F, G + 1);
    end;
end;


{                                                                              }
{ TUnicodeStringArray                                                          }
{                                                                              }
class function TUnicodeStringArray.CreateInstance: TUnicodeStringArray;
begin
  Result := TUnicodeStringArray.Create;
end;

constructor TUnicodeStringArray.Create;
begin
  inherited Create;
end;

constructor TUnicodeStringArray.Create(const V: UnicodeStringArray);
begin
  inherited Create;
  SetData(V);
end;

procedure TUnicodeStringArray.SetData(const AData: UnicodeStringArray);
begin
  FData := AData;
  FCount := Length(FData);
  FCapacity := FCount;
end;

procedure TUnicodeStringArray.Clear;
begin
  FData := nil;
  FCapacity := 0;
  FCount := 0;
end;

procedure TUnicodeStringArray.Assign(const ASource: UnicodeStringArray);
begin
  SetData(Copy(ASource));
end;

procedure TUnicodeStringArray.Assign(const ASource: Array of UnicodeString);
var
  H : NativeInt;
  L : NativeInt;
  I : NativeInt;
begin
  H := High(ASource);
  L := H + 1;
  SetLength(FData, L);
  for I := 0 to H do
    FData[I] := ASource[I];
  FCount := L;
  FCapacity := L;
end;

procedure TUnicodeStringArray.Assign(const ASource: TUnicodeStringArray);
var
  D : UnicodeStringArray;
begin
  if not Assigned(ASource) then
    raise EArrayError.Create(SErrSourceNotAssigned);

  D := Copy(ASource.FData);
  SetLength(D, ASource.FCount);
  SetData(D);
end;

function TUnicodeStringArray.Duplicate: TUnicodeStringArray;
var
  Obj : TUnicodeStringArray;
begin
  try
    Obj := CreateInstance;
    try
      Obj.Assign(self);
    except
      Obj.Free;
      raise;
    end;
  except
    on E : Exception do
      raise EArrayError.CreateFmt(SErrCannotDuplicate, [ClassName, E.Message]);
  end;
  Result := Obj;
end;

function TUnicodeStringArray.IsEqual(const V: TUnicodeStringArray): Boolean;
var
  I, L : NativeInt;
begin
  L := V.Count;
  Result := L = Count;
  if not Result then
    exit;
  for I := 0 to L - 1 do
    if FData[I] <> V.FData[I] then
      begin
        Result := False;
        exit;
      end;
end;

procedure TUnicodeStringArray.SetCount(const ANewCount: NativeInt);
var
  L : NativeInt;
  C : NativeInt;
  N : NativeInt;
begin
  N := ANewCount;
  if N < 0 then
    raise EArrayError.CreateFmt(SErrInvalidCountValue, [N]);
  C := FCount;
  if N = C then
    exit;

  FCount := N;
  L := FCapacity;
  if L > 0 then
    if N < 16 then // pre-allocate first 16 entries
      N := 16
    else
    if N > L then
      N := N + N shr 3 // pre-allocate 1/8th extra if growing
    else
    if N > L shr 1 then // only reduce capacity if size is at least half
      exit;

  if N <> L then
    begin
      SetLength(FData, N);
      if N > L then
        FillChar(FData[L], SizeOf(UnicodeString) * (N - L), 0);
      
      FCapacity := N;
    end;
end;

function TUnicodeStringArray.GetItem(const AIdx: NativeInt): UnicodeString;
begin
  {$IFOPT R+}
  if (AIdx < 0) or (AIdx >= FCount) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AIdx]);
  {$ELSE}
  Assert(AIdx >= 0);
  Assert(AIdx < FCount);
  {$ENDIF}

  Result := FData[AIdx];
end;

procedure TUnicodeStringArray.SetItem(const AIdx: NativeInt; const AValue: UnicodeString);
begin
  {$IFOPT R+}
  if (AIdx < 0) or (AIdx >= FCount) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AIdx]);
  {$ELSE}
  Assert(AIdx >= 0);
  Assert(AIdx < FCount);
  {$ENDIF}

  FData[AIdx] := AValue;
end;

function TUnicodeStringArray.PosNext(
         const AItem: UnicodeString;
         const APrevPos: NativeInt;
         const IsSortedAscending: Boolean): NativeInt;
var
  F : NativeInt;
  I : NativeInt;
  L : NativeInt;
  H : NativeInt;
  D : UnicodeString;
begin
  F := APrevPos + 1;
  if F < 0 then
    F := 0;
  if IsSortedAscending then // binary search
    begin
      if F = 0 then // find first
        begin
          L := 0;
          H := Count - 1;
          repeat
            I := (L + H) div 2;
            D := FData[I];
            if D = AItem then
              begin
                while (I > 0) and (FData[I - 1] = AItem) do
                  Dec(I);
                Result := I;
                exit;
              end
            else
            if D > AItem then
              H := I - 1
            else
              L := I + 1;
          until L > H;
          Result := -1;
        end
      else // find next
        if APrevPos >= Count - 1 then
          Result := -1
        else
          if FData[APrevPos + 1] = AItem then
            Result := APrevPos + 1
          else
            Result := -1;
    end
  else // linear search
    begin
      for I := F to Count - 1 do
        if FData[I] = AItem then
          begin
            Result := I;
            exit;
          end;
      Result := -1;
    end;
end;

function TUnicodeStringArray.GetIndex(const AValue: UnicodeString): NativeInt;
begin
  Result := PosNext(AValue, -1, False);
end;

function TUnicodeStringArray.HasValue(const AValue: UnicodeString): Boolean;
begin
  Result := PosNext(AValue, -1, False) >= 0;
end;

function TUnicodeStringArray.Add(const AValue: UnicodeString): NativeInt;
var
  C : NativeInt;
begin
  C := FCount;
  if C >= FCapacity then
    SetCount(C + 1)
  else
    FCount := C + 1;
  FData[C] := AValue;
  Result := C;
end;

function TUnicodeStringArray.AddIfNotExists(const AValue: UnicodeString): NativeInt;
var
  I : NativeInt;
begin
  I := PosNext(AValue, -1);
  if I < 0 then
    I := Add(AValue);
  Result := I;
end;

function TUnicodeStringArray.AddArray(const AArray: UnicodeStringArray): NativeInt;
var
  C : NativeInt;
  I : NativeInt;
  L : NativeInt;
begin
  C := FCount;
  L := Length(AArray);
  SetCount(C + L);

  for I := 0 to L - 1 do
    FData[C + I] := AArray[I];

  Result := C;
end;

function TUnicodeStringArray.AddArray(const AArray: TUnicodeStringArray): NativeInt;
var
  C : NativeInt;
  I : NativeInt;
  L : NativeInt;
begin
  if not Assigned(AArray) then
    raise EArrayError.Create(SErrSourceNotAssigned);

  C := FCount;
  L := AArray.FCount;
  SetCount(C + L);

  for I := 0 to L - 1 do
    FData[C + I] := AArray.FData[I];

  Result := C;
end;

procedure TUnicodeStringArray.Insert(const AIdx: NativeInt; const ACount: NativeInt = 1);
var
  C : NativeInt;
  A : NativeInt;
  N : NativeInt;
begin
  C := FCount;
  if (AIdx < 0) or (AIdx > C) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AIdx]);
  if ACount <= 0 then
    exit;

  A := ACount;
  N := C + A;
  if N > FCapacity then
    SetCount(N)
  else
    FCount := N;

  if AIdx < C then
    Move(FData[AIdx], FData[AIdx + A], (C - AIdx) * SizeOf(UnicodeString));

  FillChar(FData[AIdx], A * SizeOf(UnicodeString), 0);
end;

procedure TUnicodeStringArray.Delete(const AIdx: NativeInt; const ACount: NativeInt = 1);
var
  A : NativeInt;
  C : NativeInt;
  L : NativeInt;
  I : NativeInt;
begin
  A := ACount;
  if A <= 0 then
    exit;

  C := FCount;
  if (AIdx < 0) or (AIdx >= C) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AIdx]);

  L := AIdx + A;
  if L > C then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AIdx]);

  for I := AIdx to L - 1 do
    FData[I] := '';

  if L < C then
    begin
      Move(FData[AIdx + A], FData[AIdx], SizeOf(UnicodeString) * (C - AIdx - A));
      FillChar(FData[C - A], SizeOf(UnicodeString) * A, 0);
    end;

  SetCount(C - A);
end;

function TUnicodeStringArray.CompareItems(const AIdx1, AIdx2: NativeInt): Int32;
var
  I, J : UnicodeString;
begin
  Assert(AIdx1 >= 0);
  Assert(AIdx1 < FCount);
  Assert(AIdx2 >= 0);
  Assert(AIdx2 < FCount);

  I := FData[AIdx1];
  J := FData[AIdx2];
  if I < J then
    Result := -1
  else
  if I > J then
    Result := 1
  else
    Result := 0;
end;

procedure TUnicodeStringArray.Sort;

  procedure QuickSort(L, R: NativeInt);
  var
    I : NativeInt;
    J : NativeInt;
    M : NativeInt;
    T : UnicodeString;
  begin
    repeat
      I := L;
      J := R;
      M := (L + R) shr 1;
      repeat
        while CompareItems(I, M) < 0 do
          Inc(I);
        while CompareItems(J, M) > 0 do
          Dec(J);
        if I <= J then
          begin
            T := FData[I];
            FData[I] := FData[J];
            FData[J] := T;
            if M = I then
              M := J
            else
            if M = J then
              M := I;
            Inc(I);
            Dec(J);
          end;
      until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    until I >= R;
  end;

var
  C : NativeInt;
begin
  C := Count;
  if C > 0 then
    QuickSort(0, C - 1);
end;

procedure TUnicodeStringArray.Fill(const AIdx, ACount: NativeInt; const AValue: UnicodeString);
var
  I : NativeInt;
begin
  for I := AIdx to AIdx + ACount - 1 do
    FData[I] := AValue;
end;

function TUnicodeStringArray.GetRange(const ALoIdx, AHiIdx: NativeInt): UnicodeStringArray;
var
  L : NativeInt;
  H : NativeInt;
begin
  {$IFOPT R+}
  if (ALoIdx < 0) or (ALoIdx >= FCount) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [ALoIdx]);
  if (AHiIdx < 0) or (AHiIdx >= FCount) or (AHiIdx < ALoIdx) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AHiIdx]);
  {$ELSE}
  Assert((ALoIdx >= 0) and (ALoIdx < FCount));
  Assert((AHiIdx >= 0) and (AHiIdx < FCount) and (AHiIdx >= ALoIdx));
  {$ENDIF}

  L := MaxNativeInt(0, ALoIdx);
  H := MinNativeInt(AHiIdx, FCount);
  if H >= L then
    Result := Copy(FData, L, H - L + 1)
  else
    Result := nil;
end;

procedure TUnicodeStringArray.SetRange(const ALoIdx, AHiIdx: NativeInt; const V: UnicodeStringArray);
var
  L : NativeInt;
  H : NativeInt;
  C : NativeInt;
begin
  {$IFOPT R+}
  if (ALoIdx < 0) or (ALoIdx >= FCount) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [ALoIdx]);
  if (AHiIdx < 0) or (AHiIdx >= FCount) or (AHiIdx < ALoIdx) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AHiIdx]);
  {$ELSE}
  Assert((ALoIdx >= 0) and (ALoIdx < FCount));
  Assert((AHiIdx >= 0) and (AHiIdx < FCount) and (AHiIdx >= ALoIdx));
  {$ENDIF}

  L := MaxNativeInt(0, ALoIdx);
  H := MinNativeInt(AHiIdx, FCount);
  C := MaxNativeInt(MinNativeInt(Length(V), H - L + 1), 0);
  if C > 0 then
    Move(V[0], FData[L], C * Sizeof(UnicodeString));
end;

function TUnicodeStringArray.GetItemAsString(const AIdx: NativeInt): String;
begin
  Result := String(GetItem(AIdx));
end;

function TUnicodeStringArray.GetAsString: String;
var
  I : NativeInt;
  L : NativeInt;
begin
  L := FCount;
  if L = 0 then
    begin
      Result := '';
      exit;
    end;
  Result := GetItemAsString(0);
  for I := 1 to L - 1 do
    Result := Result + ',' + GetItemAsString(I);
end;

procedure TUnicodeStringArray.SetItemAsString(const AIdx: NativeInt; const AValue: String);
begin
  SetItem(AIdx, UnicodeString(AValue));
end;

procedure TUnicodeStringArray.SetAsString(const S: String);
var
  L : NativeInt;
  F : NativeInt;
  C : NativeInt;
  G : NativeInt;
begin
  L := Length(S);
  if L = 0 then
    begin
      Count := 0;
      exit;
    end;
  L := 0;
  F := 1;
  C := Length(S);
  while F < C do
    begin
      G := 0;
      while (F + G <= C) and (S[F + G] <> ',') do
        Inc(G);
      Inc(L);
      Count := L;
      SetItemAsString(L - 1, Copy(S, F, G));
      Inc(F, G + 1);
    end;
end;


{                                                                              }
{ TPointerArray                                                                }
{                                                                              }
class function TPointerArray.CreateInstance: TPointerArray;
begin
  Result := TPointerArray.Create;
end;

constructor TPointerArray.Create;
begin
  inherited Create;
end;

constructor TPointerArray.Create(const V: PointerArray);
begin
  inherited Create;
  SetData(V);
end;

procedure TPointerArray.SetData(const AData: PointerArray);
begin
  FData := AData;
  FCount := Length(FData);
  FCapacity := FCount;
end;

procedure TPointerArray.Clear;
begin
  FData := nil;
  FCapacity := 0;
  FCount := 0;
end;

procedure TPointerArray.Assign(const ASource: PointerArray);
begin
  SetData(Copy(ASource));
end;

procedure TPointerArray.Assign(const ASource: Array of Pointer);
var
  H : NativeInt;
  L : NativeInt;
  I : NativeInt;
begin
  H := High(ASource);
  L := H + 1;
  SetLength(FData, L);
  for I := 0 to H do
    FData[I] := ASource[I];
  FCount := L;
  FCapacity := L;
end;

procedure TPointerArray.Assign(const ASource: TPointerArray);
var
  D : PointerArray;
begin
  if not Assigned(ASource) then
    raise EArrayError.Create(SErrSourceNotAssigned);

  D := Copy(ASource.FData);
  SetLength(D, ASource.FCount);
  SetData(D);
end;

function TPointerArray.Duplicate: TPointerArray;
var
  Obj : TPointerArray;
begin
  try
    Obj := CreateInstance;
    try
      Obj.Assign(self);
    except
      Obj.Free;
      raise;
    end;
  except
    on E : Exception do
      raise EArrayError.CreateFmt(SErrCannotDuplicate, [ClassName, E.Message]);
  end;
  Result := Obj;
end;

function TPointerArray.IsEqual(const V: TPointerArray): Boolean;
var
  I, L : NativeInt;
begin
  L := V.Count;
  Result := L = Count;
  if not Result then
    exit;
  for I := 0 to L - 1 do
    if FData[I] <> V.FData[I] then
      begin
        Result := False;
        exit;
      end;
end;

procedure TPointerArray.SetCount(const ANewCount: NativeInt);
var
  L : NativeInt;
  C : NativeInt;
  N : NativeInt;
begin
  N := ANewCount;
  if N < 0 then
    raise EArrayError.CreateFmt(SErrInvalidCountValue, [N]);
  C := FCount;
  if N = C then
    exit;

  FCount := N;
  L := FCapacity;
  if L > 0 then
    if N < 16 then // pre-allocate first 16 entries
      N := 16
    else
    if N > L then
      N := N + N shr 3 // pre-allocate 1/8th extra if growing
    else
    if N > L shr 1 then // only reduce capacity if size is at least half
      exit;

  if N <> L then
    begin
      SetLength(FData, N);
      if N > L then
        FillChar(FData[L], SizeOf(Pointer) * (N - L), 0);
      
      FCapacity := N;
    end;
end;

function TPointerArray.GetItem(const AIdx: NativeInt): Pointer;
begin
  {$IFOPT R+}
  if (AIdx < 0) or (AIdx >= FCount) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AIdx]);
  {$ELSE}
  Assert(AIdx >= 0);
  Assert(AIdx < FCount);
  {$ENDIF}

  Result := FData[AIdx];
end;

procedure TPointerArray.SetItem(const AIdx: NativeInt; const AValue: Pointer);
begin
  {$IFOPT R+}
  if (AIdx < 0) or (AIdx >= FCount) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AIdx]);
  {$ELSE}
  Assert(AIdx >= 0);
  Assert(AIdx < FCount);
  {$ENDIF}

  FData[AIdx] := AValue;
end;

function TPointerArray.PosNext(
         const AItem: Pointer;
         const APrevPos: NativeInt;
         const IsSortedAscending: Boolean): NativeInt;
var
  F : NativeInt;
  I : NativeInt;
  L : NativeInt;
  H : NativeInt;
  D : Pointer;
begin
  F := APrevPos + 1;
  if F < 0 then
    F := 0;
  if IsSortedAscending then // binary search
    begin
      if F = 0 then // find first
        begin
          L := 0;
          H := Count - 1;
          repeat
            I := (L + H) div 2;
            D := FData[I];
            if D = AItem then
              begin
                while (I > 0) and (FData[I - 1] = AItem) do
                  Dec(I);
                Result := I;
                exit;
              end
            else
            if NativeUInt(D) > NativeUInt(AItem) then
              H := I - 1
            else
              L := I + 1;
          until L > H;
          Result := -1;
        end
      else // find next
        if APrevPos >= Count - 1 then
          Result := -1
        else
          if FData[APrevPos + 1] = AItem then
            Result := APrevPos + 1
          else
            Result := -1;
    end
  else // linear search
    begin
      for I := F to Count - 1 do
        if FData[I] = AItem then
          begin
            Result := I;
            exit;
          end;
      Result := -1;
    end;
end;

function TPointerArray.GetIndex(const AValue: Pointer): NativeInt;
begin
  Result := PosNext(AValue, -1, False);
end;

function TPointerArray.HasValue(const AValue: Pointer): Boolean;
begin
  Result := PosNext(AValue, -1, False) >= 0;
end;

function TPointerArray.Add(const AValue: Pointer): NativeInt;
var
  C : NativeInt;
begin
  C := FCount;
  if C >= FCapacity then
    SetCount(C + 1)
  else
    FCount := C + 1;
  FData[C] := AValue;
  Result := C;
end;

function TPointerArray.AddIfNotExists(const AValue: Pointer): NativeInt;
var
  I : NativeInt;
begin
  I := PosNext(AValue, -1);
  if I < 0 then
    I := Add(AValue);
  Result := I;
end;

function TPointerArray.AddArray(const AArray: PointerArray): NativeInt;
var
  C : NativeInt;
  I : NativeInt;
  L : NativeInt;
begin
  C := FCount;
  L := Length(AArray);
  SetCount(C + L);

  for I := 0 to L - 1 do
    FData[C + I] := AArray[I];

  Result := C;
end;

function TPointerArray.AddArray(const AArray: TPointerArray): NativeInt;
var
  C : NativeInt;
  I : NativeInt;
  L : NativeInt;
begin
  if not Assigned(AArray) then
    raise EArrayError.Create(SErrSourceNotAssigned);

  C := FCount;
  L := AArray.FCount;
  SetCount(C + L);

  for I := 0 to L - 1 do
    FData[C + I] := AArray.FData[I];

  Result := C;
end;

procedure TPointerArray.Insert(const AIdx: NativeInt; const ACount: NativeInt = 1);
var
  C : NativeInt;
  A : NativeInt;
  N : NativeInt;
begin
  C := FCount;
  if (AIdx < 0) or (AIdx > C) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AIdx]);
  if ACount <= 0 then
    exit;

  A := ACount;
  N := C + A;
  if N > FCapacity then
    SetCount(N)
  else
    FCount := N;

  if AIdx < C then
    Move(FData[AIdx], FData[AIdx + A], (C - AIdx) * SizeOf(Pointer));

  FillChar(FData[AIdx], A * SizeOf(Pointer), 0);
end;

procedure TPointerArray.Delete(const AIdx: NativeInt; const ACount: NativeInt = 1);
var
  A : NativeInt;
  C : NativeInt;
  L : NativeInt;
  I : NativeInt;
begin
  A := ACount;
  if A <= 0 then
    exit;

  C := FCount;
  if (AIdx < 0) or (AIdx >= C) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AIdx]);

  L := AIdx + A;
  if L > C then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AIdx]);

  for I := AIdx to L - 1 do
    FData[I] := nil;

  if L < C then
    begin
      Move(FData[AIdx + A], FData[AIdx], SizeOf(Pointer) * (C - AIdx - A));
      FillChar(FData[C - A], SizeOf(Pointer) * A, 0);
    end;

  SetCount(C - A);
end;

function TPointerArray.CompareItems(const AIdx1, AIdx2: NativeInt): Int32;
var
  I, J : Pointer;
begin
  Assert(AIdx1 >= 0);
  Assert(AIdx1 < FCount);
  Assert(AIdx2 >= 0);
  Assert(AIdx2 < FCount);

  I := FData[AIdx1];
  J := FData[AIdx2];
  if NativeUInt(I) < NativeUInt(J) then
    Result := -1
  else
  if NativeUInt(I) > NativeUInt(J) then
    Result := 1
  else
    Result := 0;
end;

procedure TPointerArray.Sort;

  procedure QuickSort(L, R: NativeInt);
  var
    I : NativeInt;
    J : NativeInt;
    M : NativeInt;
    T : Pointer;
  begin
    repeat
      I := L;
      J := R;
      M := (L + R) shr 1;
      repeat
        while CompareItems(I, M) < 0 do
          Inc(I);
        while CompareItems(J, M) > 0 do
          Dec(J);
        if I <= J then
          begin
            T := FData[I];
            FData[I] := FData[J];
            FData[J] := T;
            if M = I then
              M := J
            else
            if M = J then
              M := I;
            Inc(I);
            Dec(J);
          end;
      until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    until I >= R;
  end;

var
  C : NativeInt;
begin
  C := Count;
  if C > 0 then
    QuickSort(0, C - 1);
end;

procedure TPointerArray.Fill(const AIdx, ACount: NativeInt; const AValue: Pointer);
var
  I : NativeInt;
begin
  for I := AIdx to AIdx + ACount - 1 do
    FData[I] := AValue;
end;

function TPointerArray.GetRange(const ALoIdx, AHiIdx: NativeInt): PointerArray;
var
  L : NativeInt;
  H : NativeInt;
begin
  {$IFOPT R+}
  if (ALoIdx < 0) or (ALoIdx >= FCount) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [ALoIdx]);
  if (AHiIdx < 0) or (AHiIdx >= FCount) or (AHiIdx < ALoIdx) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AHiIdx]);
  {$ELSE}
  Assert((ALoIdx >= 0) and (ALoIdx < FCount));
  Assert((AHiIdx >= 0) and (AHiIdx < FCount) and (AHiIdx >= ALoIdx));
  {$ENDIF}

  L := MaxNativeInt(0, ALoIdx);
  H := MinNativeInt(AHiIdx, FCount);
  if H >= L then
    Result := Copy(FData, L, H - L + 1)
  else
    Result := nil;
end;

procedure TPointerArray.SetRange(const ALoIdx, AHiIdx: NativeInt; const V: PointerArray);
var
  L : NativeInt;
  H : NativeInt;
  C : NativeInt;
begin
  {$IFOPT R+}
  if (ALoIdx < 0) or (ALoIdx >= FCount) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [ALoIdx]);
  if (AHiIdx < 0) or (AHiIdx >= FCount) or (AHiIdx < ALoIdx) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AHiIdx]);
  {$ELSE}
  Assert((ALoIdx >= 0) and (ALoIdx < FCount));
  Assert((AHiIdx >= 0) and (AHiIdx < FCount) and (AHiIdx >= ALoIdx));
  {$ENDIF}

  L := MaxNativeInt(0, ALoIdx);
  H := MinNativeInt(AHiIdx, FCount);
  C := MaxNativeInt(MinNativeInt(Length(V), H - L + 1), 0);
  if C > 0 then
    Move(V[0], FData[L], C * Sizeof(Pointer));
end;




{                                                                              }
{ TInterfaceArray                                                              }
{                                                                              }
class function TInterfaceArray.CreateInstance: TInterfaceArray;
begin
  Result := TInterfaceArray.Create;
end;

constructor TInterfaceArray.Create;
begin
  inherited Create;
end;

constructor TInterfaceArray.Create(const V: InterfaceArray);
begin
  inherited Create;
  SetData(V);
end;

procedure TInterfaceArray.SetData(const AData: InterfaceArray);
begin
  FData := AData;
  FCount := Length(FData);
  FCapacity := FCount;
end;

procedure TInterfaceArray.Clear;
begin
  FData := nil;
  FCapacity := 0;
  FCount := 0;
end;

procedure TInterfaceArray.Assign(const ASource: InterfaceArray);
begin
  SetData(Copy(ASource));
end;

procedure TInterfaceArray.Assign(const ASource: Array of IInterface);
var
  H : NativeInt;
  L : NativeInt;
  I : NativeInt;
begin
  H := High(ASource);
  L := H + 1;
  SetLength(FData, L);
  for I := 0 to H do
    FData[I] := ASource[I];
  FCount := L;
  FCapacity := L;
end;

procedure TInterfaceArray.Assign(const ASource: TInterfaceArray);
var
  D : InterfaceArray;
begin
  if not Assigned(ASource) then
    raise EArrayError.Create(SErrSourceNotAssigned);

  D := Copy(ASource.FData);
  SetLength(D, ASource.FCount);
  SetData(D);
end;

function TInterfaceArray.Duplicate: TInterfaceArray;
var
  Obj : TInterfaceArray;
begin
  try
    Obj := CreateInstance;
    try
      Obj.Assign(self);
    except
      Obj.Free;
      raise;
    end;
  except
    on E : Exception do
      raise EArrayError.CreateFmt(SErrCannotDuplicate, [ClassName, E.Message]);
  end;
  Result := Obj;
end;

function TInterfaceArray.IsEqual(const V: TInterfaceArray): Boolean;
var
  I, L : NativeInt;
begin
  L := V.Count;
  Result := L = Count;
  if not Result then
    exit;
  for I := 0 to L - 1 do
    if FData[I] <> V.FData[I] then
      begin
        Result := False;
        exit;
      end;
end;

procedure TInterfaceArray.SetCount(const ANewCount: NativeInt);
var
  L : NativeInt;
  C : NativeInt;
  N : NativeInt;
begin
  N := ANewCount;
  if N < 0 then
    raise EArrayError.CreateFmt(SErrInvalidCountValue, [N]);
  C := FCount;
  if N = C then
    exit;

  FCount := N;
  L := FCapacity;
  if L > 0 then
    if N < 16 then // pre-allocate first 16 entries
      N := 16
    else
    if N > L then
      N := N + N shr 3 // pre-allocate 1/8th extra if growing
    else
    if N > L shr 1 then // only reduce capacity if size is at least half
      exit;

  if N <> L then
    begin
      SetLength(FData, N);
      if N > L then
        FillChar(FData[L], SizeOf(IInterface) * (N - L), 0);
      
      FCapacity := N;
    end;
end;

function TInterfaceArray.GetItem(const AIdx: NativeInt): IInterface;
begin
  {$IFOPT R+}
  if (AIdx < 0) or (AIdx >= FCount) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AIdx]);
  {$ELSE}
  Assert(AIdx >= 0);
  Assert(AIdx < FCount);
  {$ENDIF}

  Result := FData[AIdx];
end;

procedure TInterfaceArray.SetItem(const AIdx: NativeInt; const AValue: IInterface);
begin
  {$IFOPT R+}
  if (AIdx < 0) or (AIdx >= FCount) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AIdx]);
  {$ELSE}
  Assert(AIdx >= 0);
  Assert(AIdx < FCount);
  {$ENDIF}

  FData[AIdx] := AValue;
end;

function TInterfaceArray.PosNext(
         const AItem: IInterface;
         const APrevPos: NativeInt;
         const IsSortedAscending: Boolean): NativeInt;
var
  F : NativeInt;
  I : NativeInt;
  L : NativeInt;
  H : NativeInt;
  D : IInterface;
begin
  F := APrevPos + 1;
  if F < 0 then
    F := 0;
  if IsSortedAscending then // binary search
    begin
      if F = 0 then // find first
        begin
          L := 0;
          H := Count - 1;
          repeat
            I := (L + H) div 2;
            D := FData[I];
            if D = AItem then
              begin
                while (I > 0) and (FData[I - 1] = AItem) do
                  Dec(I);
                Result := I;
                exit;
              end
            else
            if NativeUInt(D) > NativeUInt(AItem) then
              H := I - 1
            else
              L := I + 1;
          until L > H;
          Result := -1;
        end
      else // find next
        if APrevPos >= Count - 1 then
          Result := -1
        else
          if FData[APrevPos + 1] = AItem then
            Result := APrevPos + 1
          else
            Result := -1;
    end
  else // linear search
    begin
      for I := F to Count - 1 do
        if FData[I] = AItem then
          begin
            Result := I;
            exit;
          end;
      Result := -1;
    end;
end;

function TInterfaceArray.GetIndex(const AValue: IInterface): NativeInt;
begin
  Result := PosNext(AValue, -1, False);
end;

function TInterfaceArray.HasValue(const AValue: IInterface): Boolean;
begin
  Result := PosNext(AValue, -1, False) >= 0;
end;

function TInterfaceArray.Add(const AValue: IInterface): NativeInt;
var
  C : NativeInt;
begin
  C := FCount;
  if C >= FCapacity then
    SetCount(C + 1)
  else
    FCount := C + 1;
  FData[C] := AValue;
  Result := C;
end;

function TInterfaceArray.AddIfNotExists(const AValue: IInterface): NativeInt;
var
  I : NativeInt;
begin
  I := PosNext(AValue, -1);
  if I < 0 then
    I := Add(AValue);
  Result := I;
end;

function TInterfaceArray.AddArray(const AArray: InterfaceArray): NativeInt;
var
  C : NativeInt;
  I : NativeInt;
  L : NativeInt;
begin
  C := FCount;
  L := Length(AArray);
  SetCount(C + L);

  for I := 0 to L - 1 do
    FData[C + I] := AArray[I];

  Result := C;
end;

function TInterfaceArray.AddArray(const AArray: TInterfaceArray): NativeInt;
var
  C : NativeInt;
  I : NativeInt;
  L : NativeInt;
begin
  if not Assigned(AArray) then
    raise EArrayError.Create(SErrSourceNotAssigned);

  C := FCount;
  L := AArray.FCount;
  SetCount(C + L);

  for I := 0 to L - 1 do
    FData[C + I] := AArray.FData[I];

  Result := C;
end;

procedure TInterfaceArray.Insert(const AIdx: NativeInt; const ACount: NativeInt = 1);
var
  C : NativeInt;
  A : NativeInt;
  N : NativeInt;
begin
  C := FCount;
  if (AIdx < 0) or (AIdx > C) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AIdx]);
  if ACount <= 0 then
    exit;

  A := ACount;
  N := C + A;
  if N > FCapacity then
    SetCount(N)
  else
    FCount := N;

  if AIdx < C then
    Move(FData[AIdx], FData[AIdx + A], (C - AIdx) * SizeOf(IInterface));

  FillChar(FData[AIdx], A * SizeOf(IInterface), 0);
end;

procedure TInterfaceArray.Delete(const AIdx: NativeInt; const ACount: NativeInt = 1);
var
  A : NativeInt;
  C : NativeInt;
  L : NativeInt;
  I : NativeInt;
begin
  A := ACount;
  if A <= 0 then
    exit;

  C := FCount;
  if (AIdx < 0) or (AIdx >= C) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AIdx]);

  L := AIdx + A;
  if L > C then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AIdx]);

  for I := AIdx to L - 1 do
    FData[I] := nil;

  if L < C then
    begin
      Move(FData[AIdx + A], FData[AIdx], SizeOf(IInterface) * (C - AIdx - A));
      FillChar(FData[C - A], SizeOf(IInterface) * A, 0);
    end;

  SetCount(C - A);
end;

function TInterfaceArray.CompareItems(const AIdx1, AIdx2: NativeInt): Int32;
var
  I, J : IInterface;
begin
  Assert(AIdx1 >= 0);
  Assert(AIdx1 < FCount);
  Assert(AIdx2 >= 0);
  Assert(AIdx2 < FCount);

  I := FData[AIdx1];
  J := FData[AIdx2];
  if NativeUInt(I) < NativeUInt(J) then
    Result := -1
  else
  if NativeUInt(I) > NativeUInt(J) then
    Result := 1
  else
    Result := 0;
end;

procedure TInterfaceArray.Sort;

  procedure QuickSort(L, R: NativeInt);
  var
    I : NativeInt;
    J : NativeInt;
    M : NativeInt;
    T : IInterface;
  begin
    repeat
      I := L;
      J := R;
      M := (L + R) shr 1;
      repeat
        while CompareItems(I, M) < 0 do
          Inc(I);
        while CompareItems(J, M) > 0 do
          Dec(J);
        if I <= J then
          begin
            T := FData[I];
            FData[I] := FData[J];
            FData[J] := T;
            if M = I then
              M := J
            else
            if M = J then
              M := I;
            Inc(I);
            Dec(J);
          end;
      until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    until I >= R;
  end;

var
  C : NativeInt;
begin
  C := Count;
  if C > 0 then
    QuickSort(0, C - 1);
end;

procedure TInterfaceArray.Fill(const AIdx, ACount: NativeInt; const AValue: IInterface);
var
  I : NativeInt;
begin
  for I := AIdx to AIdx + ACount - 1 do
    FData[I] := AValue;
end;

function TInterfaceArray.GetRange(const ALoIdx, AHiIdx: NativeInt): InterfaceArray;
var
  L : NativeInt;
  H : NativeInt;
begin
  {$IFOPT R+}
  if (ALoIdx < 0) or (ALoIdx >= FCount) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [ALoIdx]);
  if (AHiIdx < 0) or (AHiIdx >= FCount) or (AHiIdx < ALoIdx) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AHiIdx]);
  {$ELSE}
  Assert((ALoIdx >= 0) and (ALoIdx < FCount));
  Assert((AHiIdx >= 0) and (AHiIdx < FCount) and (AHiIdx >= ALoIdx));
  {$ENDIF}

  L := MaxNativeInt(0, ALoIdx);
  H := MinNativeInt(AHiIdx, FCount);
  if H >= L then
    Result := Copy(FData, L, H - L + 1)
  else
    Result := nil;
end;

procedure TInterfaceArray.SetRange(const ALoIdx, AHiIdx: NativeInt; const V: InterfaceArray);
var
  L : NativeInt;
  H : NativeInt;
  C : NativeInt;
begin
  {$IFOPT R+}
  if (ALoIdx < 0) or (ALoIdx >= FCount) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [ALoIdx]);
  if (AHiIdx < 0) or (AHiIdx >= FCount) or (AHiIdx < ALoIdx) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AHiIdx]);
  {$ELSE}
  Assert((ALoIdx >= 0) and (ALoIdx < FCount));
  Assert((AHiIdx >= 0) and (AHiIdx < FCount) and (AHiIdx >= ALoIdx));
  {$ENDIF}

  L := MaxNativeInt(0, ALoIdx);
  H := MinNativeInt(AHiIdx, FCount);
  C := MaxNativeInt(MinNativeInt(Length(V), H - L + 1), 0);
  if C > 0 then
    Move(V[0], FData[L], C * Sizeof(IInterface));
end;







end.

