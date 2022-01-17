{*******************************************************************************}
{                                                                              }
{   Library:          Fundamentals 5.00                                        }
{   File name:        flcDataStructs.pas                                       }
{   File version:     5.44                                                     }
{   Description:      Data structures                                          }
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
{   This unit implements classes for the following commonly used data          }
{   structures:                                                                }
{     + Arrays                                                                 }
{     + Dictionaries                                                           }
{     + Sparse Arrays                                                          }
{     + Linked Lists                                                           }
{                                                                              }
{   ARRAYS                                                                     }
{                                                                              }
{   Arrays are ordered collections where items are indexed by consecutive      }
{   integer values.                                                            }
{                                                                              }
{   This unit implements array classes for each of the following types:        }
{     + Int32                                                                  }
{     + LongInt                                                                }
{     + Word32                                                                 }
{     + LongWord                                                               }
{     + Int64                                                                  }
{     + Single                                                                 }
{     + Double                                                                 }
{     + Extended                                                               }
{     + Pointer                                                                }
{     + AnsiString                                                             }
{     + RawByteString                                                          }
{     + String                                                                 }
{     + Object                                                                 }
{                                                                              }
{   DICTIONARIES                                                               }
{                                                                              }
{   Dictionaries are associative arrays where the key value is a string.       }
{                                                                              }
{   Associative arrays, also referred to as mappings, are unordered            }
{   collections where an arbitrary key can be used to index a value.           }
{                                                                              }
{   This unit implements dictionary classes for each of the following types:   }
{     + Integer                                                                }
{     + Cardinal                                                               }
{     + Int64                                                                  }
{     + Single                                                                 }
{     + Double                                                                 }
{     + Extended                                                               }
{     + Pointer                                                                }
{     + AnsiString                                                             }
{     + RawByteString                                                          }
{     + String                                                                 }
{     + TObject                                                                }
{     + IInterface                                                             }
{                                                                              }
{   For example, the class TIntegerDictionary is used where the key is an      }
{   arbitrary string and the value an integer.                                 }
{                                                                              }
{       Ages := TIntegerDictionary.Create;                                     }
{       Ages['John'] := 29;                                                    }
{       Ages['Tori'] := 35;                                                    }
{       if Ages.HasKey['John'] then                                            }
{         Ages.Delete('John');                                                 }
{       Ages.Free;                                                             }
{                                                                              }
{   SPARSE ARRAYS                                                              }
{                                                                              }
{   Sparse arrays are associative arrays where the index value is an           }
{   arbitrary integer.                                                         }
{                                                                              }
{   Associative arrays, also referred to as mappings, are unordered            }
{   collections where an arbitrary key can be used to index a value.           }
{                                                                              }
{   This unit implements sparse arrays that can hold the following values:     }
{     + String                                                                 }
{     + Int64                                                                  }
{     + Extended                                                               }
{     + TObject                                                                }
{                                                                              }
{   For example, the class TSparseStringArray is used where the key is an      }
{   arbitrary integer and the value a string.                                  }
{                                                                              }
{       Names := TSparseStringArray.Create;                                    }
{       Names[123] := 'John';                                                  }
{       Names[999] := 'Tori';                                                  }
{       if Names.HasItem(123) then                                             }
{         Names.Delete(123);                                                   }
{       Names.Free;                                                            }
{                                                                              }
{ Revision history:                                                            }
{                                                                              }
{   1999/11/12  0.01  Split cTypes from cDataStruct and cHolder.               }
{   1999/11/14  0.02  Added AListType.                                         }
{   2000/02/08  1.03  Initial version. AArray, TArray and TStreamArray.        }
{   2000/06/07  1.04  Base classes (AIntegerArray, ASet).                      }
{   2000/06/08  1.05  Added AObjectArray.                                      }
{   2000/06/03  1.06  Added AArray, AIntegerArray, AExtendedArray,             }
{                     AStringArray and ABitArray (formerly ASet) with some     }
{                     implementations.                                         }
{   2000/06/06  1.07  TFlatBitArray implementation.                            }
{                     Added AInt64Array.                                       }
{   2000/06/08  1.08  Added TObjectArray.                                      }
{   2000/06/10  1.09  Added linked lists.                                      }
{   2000/06/14  1.10  Converted cDataStructs to template.                      }
{   2000/06/16  1.11  Added ADictionary.                                       }
{   2000/07/07  1.12  Added ATypeDictionary.                                   }
{   2001/01/19  1.13  Added THashedStringDictionary.                           }
{   2001/04/13  1.14  Added TObjectDictionary.                                 }
{   2001/07/15  1.15  Changed memory arrays to pre-allocate when growing.      }
{   2001/08/20  2.16  Merged cTypes and cDataStructs to allow object           }
{                     interface implementation in base classes.                }
{   2002/05/15  3.17  Created cArrays unit from cDataStructs.                  }
{                     Refactored for Fundamentals 3.                           }
{   2002/09/30  3.18  Moved stream array classes to unit cStreamArrays.        }
{   2002/12/17  3.19  Added THashedStringArray.                                }
{   2003/03/08  3.20  Renamed Add methods to Append.                           }
{   2003/05/26  3.21  Added Remove methods to object array.                    }
{   2003/07/27  3.22  Initial version (sparse object array).                   }
{   2003/09/11  3.23  Added TInterfaceArray.                                   }
{   2004/01/02  3.24  Bug fixed in TStringArray.SetAsString by Eb.             }
{   2004/01/18  3.25  Added TWideStringArray.                                  }
{   2004/03/31  3.26  Added sparse String, WideString and Int64 arrays.        }
{   2004/07/24  3.27  Fixed bug in Sort with duplicate values. Thanks to Eb    }
{                     and others for reporting it.                             }
{   2004/08/01  3.28  Added AArray.RemoveDuplicates.                           }
{   2005/01/27  3.29  Added sparse Extended array.                             }
{   2006/05/10  3.30  Fixed bug in TDoublyLinkedList.DeleteList as reported    }
{                     by Malinovsky Vladimir.                                  }
{   2007/09/27  4.31  Merged into single unit for Fundamentals 4.              }
{   2009/09/23  4.32  Fixed bug in TDoublyLinkedList.InsertBefore/InsertAfter  }
{                     reported by Steffen Thorkildsen.                         }
{   2011/08/27  4.33  Fixed bugs in THashedAnsiStringArray reported by         }
{                     H Visli.                                                 }
{   2012/04/11  4.34  Unicode string changes.                                  }
{   2012/09/01  4.35  Unicode string changes.                                  }
{   2015/03/13  4.36  RawByteString support.                                   }
{   2016/01/16  5.37  Revised for Fundamentals 5.                              }
{   2018/07/17  5.38  Int32/Word32 arrays.                                     }
{   2018/08/12  5.39  String type changes.                                     }
{   2019/04/02  5.40  Integer/Cardinal array changes.                          }
{   2020/03/22  5.41  Rename parameters to avoid conflict with properties.     }
{   2020/03/22  5.42  Remove dependency on flcBits32.                          }
{   2020/03/31  5.43  LongWord changes in bit array. Integer array changes.    }
{   2020/06/02  5.44  UInt64 changes.                                          }
{                                                                              }
{ Supported compilers:                                                         }
{                                                                              }
{   Delphi 2010-10.4 Win32/Win64        5.44  2020/06/02                       }
{   Delphi 10.2-10.4 Linux64            5.44  2020/06/02                       }
{   FreePascal 3.0.4 Win64              5.44  2020/06/02                       }
{                                                                              }
{*******************************************************************************}

{$INCLUDE ..\flcInclude.inc}

{$IFDEF FREEPASCAL}
  {$WARNINGS OFF}
  {$HINTS OFF}
{$ENDIF}

unit flcDataStructs;

interface

uses
  { System }
  SysUtils,

  { Fundamentals }
  flcStdTypes,
  flcUtils;



{                                                                              }
{ A note on the class naming convention used in this unit:                     }
{                                                                              }
{   Classes with the A-prefix are abstract base classes. They define the       }
{   interface for the type and must never be instanciated. Implementation      }
{   classes follow the standard naming convention of using the T-prefix.       }
{                                                                              }



{                                                                              }
{ TYPE BASE CLASS                                                              }
{                                                                              }



{                                                                              }
{ AType                                                                        }
{   Abstract base class for data structures.                                   }
{                                                                              }
{   Provides an interface for commonly used data operations such as            }
{   assigning, comparing and duplicating.                                      }
{                                                                              }
{   Duplicate creates a new instance of the object (using CreateInstance) and  }
{   then copies the content (using Assign). Implementations do not have to     }
{   override Duplicate if both CreateInstance and Assign are implemented.      }
{   Assign's default implementation calls the protected AssignTo.              }
{                                                                              }
{   Clear sets an instance's content (value) to an empty/zero state. This      }
{   state should be similar to the state of a new instance created using       }
{   CreateInstance.                                                            }
{                                                                              }
{   IsEqual compares the content of instances. After a call to Assign, an      }
{   equivalent call to IsEqual should return True.                             }
{                                                                              }
{   Compare is the ranking function used by sorting and searching.             }
{                                                                              }
{   HashValue returns a 'random' number, based on the content (value).         }
{                                                                              }
{   AsString is the default string type representation of the content.         }
{                                                                              }
type
  EType = class(Exception);
  AType = class
  protected
    procedure RaiseTypeError(const Msg: String; const ErrorClass: ExceptClass = nil); virtual;

    procedure Init; virtual;
    procedure AssignTo(const Dest: TObject); virtual;

    function  GetAsString: String; virtual;
    procedure SetAsString(const S: String); virtual;

    function  GetAsUTF8String: RawByteString; virtual;
    procedure SetAsUTF8String(const S: RawByteString); virtual;

    function  GetAsUnicodeString: UnicodeString; virtual;
    procedure SetAsUnicodeString(const S: UnicodeString); virtual;

  public
    constructor Create;
    class function CreateInstance: AType; virtual;

    function  Duplicate: TObject; virtual;
    procedure Assign(const Source: TObject); overload; virtual;

    procedure Clear; virtual;
    function  IsEmpty: Boolean; virtual;
    function  IsEqual(const V: TObject): Boolean; virtual;
    function  Compare(const V: TObject): TCompareResult; virtual;
    function  HashValue: Word32; virtual;

    property  AsString: String read GetAsString write SetAsString;
    property  AsUTF8String: RawByteString read GetAsUTF8String write SetAsUTF8String;
    property  AsUnicodeString: UnicodeString read GetAsUnicodeString write SetAsUnicodeString;
  end;
  TypeClass = class of AType;
  ATypeArray = Array of AType;
  TypeClassArray = Array of TypeClass;



{                                                                              }
{ AType helper functions                                                       }
{                                                                              }
function  TypeDuplicate(const V: TObject): TObject;
procedure TypeAssign(const A, B: TObject);
procedure TypeClear(const V: TObject);
function  TypeIsEqual(const A, B: TObject): Boolean;
function  TypeCompare(const A, B: TObject): TCompareResult;
function  TypeHashValue(const A: TObject): Word32;
function  TypeGetAsString(const V: TObject): String;
procedure TypeSetAsString(const V: TObject; const S: String);
function  TypeGetAsUTF8String(const V: TObject): RawByteString;
procedure TypeSetAsUTF8String(const V: TObject; const S: RawByteString);
function  TypeGetAsUnicodeString(const V: TObject): UnicodeString;
procedure TypeSetAsUnicodeString(const V: TObject; const S: UnicodeString);



{                                                                              }
{ ARRAY BASE CLASSES                                                           }
{                                                                              }



{                                                                              }
{ AArray                                                                       }
{   Base class for an array.                                                   }
{                                                                              }
type
  AArray = class(AType)
  protected
    procedure RaiseIndexError(const Idx: Integer); virtual;

    function  GetAsString: String; override;
    procedure SetAsString(const S: String); override;

    function  GetCount: Integer; virtual; abstract;
    procedure SetCount(const NewCount: Integer); virtual; abstract;

    function  GetItemAsString(const Idx: Integer): String; virtual;
    procedure SetItemAsString(const Idx: Integer; const Value: String); virtual;

  public
    { AType                                                                    }
    procedure Clear; override;

    { AArray                                                                   }
    property  Count: Integer read GetCount write SetCount;
    property  ItemAsString[const Idx: Integer]: String read GetItemAsString write SetItemAsString;

    function  CompareItems(const Idx1, Idx2: Integer): TCompareResult; virtual; abstract;
    procedure ExchangeItems(const Idx1, Idx2: Integer); virtual; abstract;
    procedure Sort; virtual;
    procedure ReverseOrder; virtual;
    procedure RemoveDuplicates(const IsSortedAscending: Boolean); virtual;

    function  DuplicateRange(const LoIdx, HiIdx: Integer): AArray; virtual; abstract;
    procedure Delete(const Idx: Integer; const ACount: Integer = 1); virtual; abstract;
    procedure Insert(const Idx: Integer; const ACount: Integer = 1); virtual; abstract;
    function  AppendArray(const V: AArray): Integer; overload; virtual; abstract;
  end;
  EArray = class(EType);
  ArrayClass = class of AArray;



{%DEFINE ATypeArray}
{                                                                              }
{-A%1%Array                                                                    }
{-  Base class for an array of %1%s.                                           }
{                                                                              }
type
  A%1%Array = class(AArray)
  protected
    function  GetItem(const Idx: Integer): %3%; virtual; abstract;
    procedure SetItem(const Idx: Integer; const Value: %3%); virtual; abstract;{%IF 5}

    function  GetItemAsString(const Idx: Integer): String; override;
    procedure SetItemAsString(const Idx: Integer; const Value: String); override;{%ENDIF}

    function  GetRange(const LoIdx, HiIdx: Integer): %1%Array; virtual;
    procedure SetRange(const LoIdx, HiIdx: Integer; const V: %1%Array); virtual;{%IF 4}

    function  GetAsString: String; override;
    procedure SetAsString(const S: String); override;{%ENDIF}

  public
    { AType                                                                    }
    procedure Assign(const Source: TObject); override;
    function  IsEqual(const V: TObject): Boolean; override;

    { AArray                                                                   }
    procedure ExchangeItems(const Idx1, Idx2: Integer); override;{%IF 2}
    function  CompareItems(const Idx1, Idx2: Integer): TCompareResult; override;{%ENDIF}
    function  AppendArray(const V: AArray): Integer; overload; override;
    function  DuplicateRange(const LoIdx, HiIdx: Integer): AArray; override;
    procedure Delete(const Idx: Integer; const ACount: Integer = 1); override;
    procedure Insert(const Idx: Integer; const ACount: Integer = 1); override;

    {-A%1%Array interface                                                      }
    property  Item[const Idx: Integer]: %3% read GetItem write SetItem; default;
    property  Range[const LoIdx, HiIdx: Integer]: %1%Array read GetRange write SetRange;
    procedure Fill(const Idx, ACount: Integer; const Value: %3%); virtual;
    function  AppendItem(const Value: %3%): Integer; virtual;
    function  AppendArray(const V: %1%Array): Integer; overload; virtual;
    function  PosNext(const Find: %3%; const PrevPos: Integer = -1{%IF 2};
              const IsSortedAscending: Boolean = False{%ENDIF}): Integer;
  end;
  E%1%Array = class(EArray);


{%ENDDEF}
{%TEMPLATE ATypeArray 'Int32'      'B'  'Int32'      ''  'I' }
{%TEMPLATE ATypeArray 'Int64'      'B'  'Int64'      ''  'I' }
{%TEMPLATE ATypeArray 'LongInt'    'B'  'LongInt'    ''  'I' }
{                                                                              }
{ AIntegerArray                                                                }
{                                                                              }
type
  AIntegerArray = AInt32Array;
  EIntegerArray = EInt32Array;



{%TEMPLATE ATypeArray 'Word32'     'B'  'Word32'     '' 'I' }
{%TEMPLATE ATypeArray 'Word64'     'B'  'Word64'     '' 'I' }
{%TEMPLATE ATypeArray 'LongWord'   'B'  'LongWord'   '' 'I' }
{                                                                              }
{ ACardinalArray                                                               }
{                                                                              }
type
  ACardinalArray = AWord32Array;
  ECardinalArray = EWord32Array;



{%TEMPLATE ATypeArray 'Single'        'B'  'Single'        ''  'I' }
{%TEMPLATE ATypeArray 'Double'        'B'  'Double'        ''  'I' }
{%TEMPLATE ATypeArray 'Extended'      'B'  'Extended'      ''  'I' }
{$IFDEF SupportAnsiString}
{%TEMPLATE ATypeArray 'AnsiString'    'B'  'AnsiString'    ''  'I' }
{$ENDIF}
{%TEMPLATE ATypeArray 'RawByteString' 'B'  'RawByteString' ''  'I' }
type
  AUTF8StringArray = ARawByteStringArray;
  EUTF8StringArray = ERawByteStringArray;



{%TEMPLATE ATypeArray 'UnicodeString' 'B'  'UnicodeString' ''  'I' }
{%TEMPLATE ATypeArray 'String'        'B'  'String'        ''  ''  }
{%TEMPLATE ATypeArray 'Pointer'       'B'  'Pointer'       ''  'I' }
{%TEMPLATE ATypeArray 'Interface'     'B'  'IInterface'    ''  ''  }
{                                                                              }
{ AObjectArray                                                                 }
{   Base class for an array of objects.                                        }
{                                                                              }
type
  EObjectArray = class(EArray);
  AObjectArray = class(AArray)
  protected
    function  GetItem(const Idx: Integer): TObject; virtual; abstract;
    procedure SetItem(const Idx: Integer; const Value: TObject); virtual; abstract;
    function  GetRange(const LoIdx, HiIdx: Integer): ObjectArray; virtual;
    procedure SetRange(const LoIdx, HiIdx: Integer; const V: ObjectArray); virtual;
    function  GetAsString: String; override;
    function  GetIsItemOwner: Boolean; virtual; abstract;
    procedure SetIsItemOwner(const AIsItemOwner: Boolean); virtual; abstract;

  public
    { AType                                                                    }
    procedure Clear; override;
    procedure Assign(const Source: TObject); override;
    function  IsEqual(const V: TObject): Boolean; override;
    function  Compare(const V: TObject): TCompareResult; override;

    { AArray                                                                   }
    procedure ExchangeItems(const Idx1, Idx2: Integer); override;
    function  CompareItems(const Idx1, Idx2: Integer): TCompareResult; override;
    function  AppendArray(const V: AArray): Integer; overload; override;
    procedure Delete(const Idx: Integer; const ACount: Integer = 1); override;

    { AObjectArray interface                                                   }
    property  Item[const Idx: Integer]: TObject read GetItem write SetItem; default;
    property  Range[const LoIdx, HiIdx: Integer]: ObjectArray read GetRange write SetRange;
    function  AppendItem(const Value: TObject): Integer; virtual;
    function  AppendArray(const V: ObjectArray): Integer; overload; virtual;

    function  PosNext(const Find: TObject; const PrevPos: Integer): Integer; overload;
    function  PosNext(var AItem: TObject; const AClassType: TClass; const PrevPos: Integer = -1): Integer; overload;
    function  PosNext(var AItem: TObject; const AClassName: String; const PrevPos: Integer = -1): Integer; overload;
    function  Find(const AClassType: TClass; const ACount: Integer = 1): TObject; overload;
    function  Find(const AClassName: String; const ACount: Integer = 1): TObject; overload;
    function  FindAll(const AClassType: TClass): ObjectArray; overload;
    function  FindAll(const AClassName: String): ObjectArray; overload;
    function  CountItems(const AClassType: TClass): Integer; overload;
    function  CountItems(const AClassName: String): Integer; overload;
    function  DeleteValue(const Value: TObject): Boolean;
    function  DeleteAll(const Value: TObject): Integer;

    property  IsItemOwner: Boolean read GetIsItemOwner write SetIsItemOwner;
    procedure ReleaseItems; virtual; abstract;
    procedure FreeItems; virtual; abstract;
    function  ReleaseItem(const Idx: Integer): TObject; virtual; abstract;
    function  ReleaseValue(const Value: TObject): Boolean;
    function  RemoveItem(const Idx: Integer): TObject;
    function  RemoveValue(const Value: TObject): Boolean;
  end;



{                                                                              }
{ ABitArray                                                                    }
{   Base class for bit array implementations.                                  }
{   Bits are defined as False at initialization.                               }
{   FindRange finds Count consecutive bits that are equal to Value. It         }
{   returns the index of the leftmost bit or -1 if not found.                  }
{                                                                              }
type
  EBitArray = class(EArray);
  ABitArray = class(AArray)
  protected
    function  GetBit(const Idx: Integer): Boolean; virtual; abstract;
    procedure SetBit(const Idx: Integer; const Value: Boolean); virtual; abstract;
    function  GetRangeL(const Idx: Integer): Word32; virtual;
    procedure SetRangeL(const Idx: Integer; const Value: Word32); virtual;

  public
    { AType                                                                    }
    procedure Assign(const Source: TObject); override;
    function  IsEqual(const V: TObject): Boolean; override;

    { AArray                                                                   }
    procedure Delete(const Idx: Integer; const ACount: Integer = 1); override;
    procedure Insert(const Idx: Integer; const ACount: Integer = 1); override;
    function  AppendArray(const V: AArray): Integer; override;
    procedure ExchangeItems(const Idx1, Idx2: Integer); override;
    function  CompareItems(const Idx1, Idx2: Integer): TCompareResult; override;
    function  DuplicateRange(const LoIdx, HiIdx: Integer): AArray; override;

    { ABitArray interface                                                      }
    property  Bit[const Idx: Integer]: Boolean read GetBit write SetBit; default;
    property  RangeL[const Idx: Integer]: Word32 read GetRangeL write SetRangeL;
    function  IsRange(const LoIdx, HiIdx: Integer; const Value: Boolean): Boolean; virtual;
    procedure Fill(const Idx, ACount: Integer; const Value: Boolean); virtual;
    function  AppendItem(const Value: Boolean): Integer; virtual;
    procedure Invert; virtual;

    function  Find(const Value: Boolean = False;
              const Start: Integer = 0): Integer; virtual;
    function  FindRange(const Value: Boolean = False;
              const Start: Integer = 0;
              const ACount: Integer = 1): Integer; virtual;
  end;



{                                                                              }
{ ARRAY IMPLEMENTATIONS                                                        }
{                                                                              }



{%DEFINE AArrayDynArray}
{                                                                              }
{-T%1%Array                                                                    }
{-  A%1%Array implemented using a dynamic array.                               }
{                                                                              }
type
  T%1%Array = class(A%1%Array)
  protected
    FData     : %1%Array;
    FCapacity : Integer;
    FCount    : Integer;

    { ACollection                                                              }
    function  GetCount: Integer; override;
    procedure SetCount(const NewCount: Integer); override;

    { A%1%Array                                                            }
    function  GetItem(const Idx: Integer): %2%; override;
    procedure SetItem(const Idx: Integer; const Value: %2%); override;
    function  GetRange(const LoIdx, HiIdx: Integer): %1%Array; override;
    procedure SetRange(const LoIdx, HiIdx: Integer; const V: %1%Array); override;
    procedure SetData(const AData: %1%Array); virtual;

  public
    constructor Create(const V: %1%Array = nil); overload;

    { AType                                                                    }
    procedure Assign(const Source: TObject); overload; override;

    { AArray                                                                   }
    procedure ExchangeItems(const Idx1, Idx2: Integer); override;
    function  DuplicateRange(const LoIdx, HiIdx: Integer): AArray; override;
    procedure Delete(const Idx: Integer; const ACount: Integer = 1); override;
    procedure Insert(const Idx: Integer; const ACount: Integer = 1); override;

    { A%1%Array                                                            }
    procedure Assign(const V: %1%Array); overload;
    procedure Assign(const V: Array of %2%); overload;
    function  AppendItem(const Value: %2%): Integer; override;

    { T%1%Array                                                            }
    property  Data: %1%Array read FData write SetData;
    property  Count: Integer read FCount write SetCount;
  end;


{%ENDDEF}
{%TEMPLATE AArrayDynArray 'Int32'    'Int32'   }
{%TEMPLATE AArrayDynArray 'Int64'    'Int64'   }
{%TEMPLATE AArrayDynArray 'LongInt'  'LongInt' }
{                                                                              }
{ TIntegerArray                                                                }
{                                                                              }
type
  TIntegerArray = TInt32Array;



{                                                                              }
{ TNativeIntArray                                                              }
{                                                                              }
{$IFDEF CPU_32}
type
  TNativeIntArray = TInt32Array;
{$ELSE}{$IFDEF CPU_64}
type
  TNativeIntArray = TInt64Array;
{$ENDIF}{$ENDIF}



{                                                                              }
{ TIntArray                                                                    }
{                                                                              }
type
  TIntArray = TInt64Array;



{%TEMPLATE AArrayDynArray 'Word32'    'Word32'}
{%TEMPLATE AArrayDynArray 'Word64'    'Word64'}
{%TEMPLATE AArrayDynArray 'LongWord'  'LongWord'}
{                                                                              }
{ TCardinalArray                                                               }
{                                                                              }
type
  TCardinalArray = TWord32Array;



{                                                                              }
{ TUInt32Array                                                                 }
{                                                                              }
type
  TUInt32Array = Word32Array;



{                                                                              }
{ TUInt64Array                                                                 }
{                                                                              }
type
  TUInt64Array = Word64Array;



{                                                                              }
{ TNativeUIntArray                                                             }
{                                                                              }
{$IFDEF CPU_32}
type
  TNativeUIntArray = TUInt32Array;
{$ELSE}{$IFDEF CPU_64}
type
  TNativeUIntArray = TUInt64Array;
{$ENDIF}{$ENDIF}



{                                                                              }
{ TNativeWordArray                                                             }
{                                                                              }
type
  TNativeWordArray = TNativeUIntArray;



{                                                                              }
{ TUIntArray                                                                   }
{                                                                              }
type
  TUIntArray = TUInt64Array;



{%TEMPLATE AArrayDynArray 'Single'        'Single'        }
{%TEMPLATE AArrayDynArray 'Double'        'Double'        }
{%TEMPLATE AArrayDynArray 'Extended'      'Extended'      }
{$IFDEF SupportAnsiString}
{%TEMPLATE AArrayDynArray 'AnsiString'    'AnsiString'    }
{$ENDIF}
{%TEMPLATE AArrayDynArray 'RawByteString' 'RawByteString' }
{                                                                              }
{ TUTF8StringArray                                                             }
{                                                                              }
type
  TUTF8StringArray = TRawByteStringArray;



{%TEMPLATE AArrayDynArray 'UnicodeString' 'UnicodeString' }
{%TEMPLATE AArrayDynArray 'String'        'String'        }
{%TEMPLATE AArrayDynArray 'Pointer'       'Pointer'       }
{%TEMPLATE AArrayDynArray 'Interface'     'IInterface'    }
{                                                                              }
{ TObjectArray                                                                 }
{   AObjectArray implemented using a dynamic array.                            }
{                                                                              }
type
  TObjectArray = class(AObjectArray)
  protected
    FData        : ObjectArray;
    FCapacity    : Integer;
    FCount       : Integer;
    FIsItemOwner : Boolean;

    procedure Init; override;
    procedure SetData(const AData: ObjectArray); virtual;

    { AArray                                                                   }
    function  GetCount: Integer; override;
    procedure SetCount(const NewCount: Integer); override;

    { AObjectArray                                                             }
    function  GetItem(const Idx: Integer): TObject; override;
    procedure SetItem(const Idx: Integer; const Value: TObject); override;
    function  GetRange(const LoIdx, HiIdx: Integer): ObjectArray; override;
    function  GetIsItemOwner: Boolean; override;
    procedure SetIsItemOwner(const AIsItemOwner: Boolean); override;

  public
    { TObjectArray interface                                                   }
    constructor Create(const V: ObjectArray = nil;
                const AIsItemOwner: Boolean = False); reintroduce; overload;
    destructor Destroy; override;

    property  Data: ObjectArray read FData write SetData;
    property  Count: Integer read FCount write SetCount;
    property  IsItemOwner: Boolean read FIsItemOwner write FIsItemOwner;
    procedure FreeItems; override;
    procedure ReleaseItems; override;
    function  ReleaseItem(const Idx: Integer): TObject; override;

    { AArray                                                                   }
    function  DuplicateRange(const LoIdx, HiIdx: Integer): AArray; override;
    procedure Delete(const Idx: Integer; const ACount: Integer = 1); override;
    procedure Insert(const Idx: Integer; const ACount: Integer = 1); override;

    { AObjectArray                                                             }
    function  AppendItem(const Value: TObject): Integer; override;
  end;



{                                                                              }
{ TBitArray                                                                    }
{   ABitArray implemented using a dynamic array.                               }
{                                                                              }
type
  TBitArray = class(ABitArray)
  protected
    FData  : Word32Array;
    FCount : Integer;

    { AArray                                                                   }
    function  GetCount: Integer; override;
    procedure SetCount(const NewCount: Integer); override;

    { ABitArray                                                                }
    function  GetBit(const Idx: Integer): Boolean; override;
    procedure SetBit(const Idx: Integer; const Value: Boolean); override;
    function  GetRangeL(const Idx: Integer): Word32; override;
    procedure SetRangeL(const Idx: Integer; const Value: Word32); override;

  public
    { ABitArray                                                                }
    procedure Fill(const LoIdx, HiIdx: Integer; const Value: Boolean); override;
    function  IsRange(const LoIdx, HiIdx: Integer; const Value: Boolean): Boolean; override;
  end;



{$IFDEF SupportAnsiString}
{                                                                              }
{ THashedAnsiStringArray                                                       }
{   AAnsiStringArray that maintains a hash lookup table of array values.       }
{                                                                              }
type
  THashedAnsiStringArray = class(TAnsiStringArray)
  protected
    FLookup        : Array of IntegerArray;
    FCaseSensitive : Boolean;

    function  LocateItemHash(const Value: AnsiString;
              var LookupList, LookupIdx: Integer): Boolean;
    procedure Rehash;

    procedure Init; override;
    procedure SetItem(const Idx: Integer; const Value: AnsiString); override;
    procedure SetData(const AData: AnsiStringArray); override;

  public
    constructor Create(const ACaseSensitive: Boolean = True);

    procedure Assign(const Source: TObject); override;
    procedure Clear; override;

    procedure ExchangeItems(const Idx1, Idx2: Integer); override;
    procedure Delete(const Idx: Integer; const ACount: Integer = 1); override;
    procedure Insert(const Idx: Integer; const ACount: Integer = 1); override;
    function  AppendItem(const Value: AnsiString): Integer; override;

    function  PosNext(const Find: AnsiString; const PrevPos: Integer = -1): Integer;
  end;
{$ENDIF}



{                                                                              }
{ THashedRawByteStringArray                                                    }
{   ARawByteStringArray that maintains a hash lookup table of array values.    }
{                                                                              }
type
  THashedRawByteStringArray = class(TRawByteStringArray)
  protected
    FLookup        : Array of IntegerArray;
    FCaseSensitive : Boolean;

    function  LocateItemHash(const Value: RawByteString;
              var LookupList, LookupIdx: Integer): Boolean;
    procedure Rehash;

    procedure Init; override;
    procedure SetItem(const Idx: Integer; const Value: RawByteString); override;
    procedure SetData(const AData: RawByteStringArray); override;

  public
    constructor Create(const ACaseSensitive: Boolean = True);

    procedure Assign(const Source: TObject); override;
    procedure Clear; override;

    procedure ExchangeItems(const Idx1, Idx2: Integer); override;
    procedure Delete(const Idx: Integer; const ACount: Integer = 1); override;
    procedure Insert(const Idx: Integer; const ACount: Integer = 1); override;
    function  AppendItem(const Value: RawByteString): Integer; override;

    function  PosNext(const Find: RawByteString; const PrevPos: Integer = -1): Integer;
  end;



{                                                                              }
{ THashedUnicodeStringArray                                                    }
{   AUnicodeStringArray that maintains a hash lookup table of array values.    }
{                                                                              }
type
  THashedUnicodeStringArray = class(TUnicodeStringArray)
  protected
    FLookup        : Array of IntegerArray;
    FCaseSensitive : Boolean;

    function  LocateItemHash(const Value: UnicodeString;
              var LookupList, LookupIdx: Integer): Boolean;
    procedure Rehash;

    procedure Init; override;
    procedure SetItem(const Idx: Integer; const Value: UnicodeString); override;
    procedure SetData(const AData: UnicodeStringArray); override;

  public
    constructor Create(const ACaseSensitive: Boolean = True);

    procedure Assign(const Source: TObject); override;
    procedure Clear; override;

    procedure ExchangeItems(const Idx1, Idx2: Integer); override;
    procedure Delete(const Idx: Integer; const ACount: Integer = 1); override;
    procedure Insert(const Idx: Integer; const ACount: Integer = 1); override;
    function  AppendItem(const Value: UnicodeString): Integer; override;

    function  PosNext(const Find: UnicodeString; const PrevPos: Integer = -1): Integer;
  end;



{                                                                              }
{ DICTIONARY BASE CLASSES                                                      }
{                                                                              }



{                                                                              }
{ ADictionary                                                                  }
{                                                                              }
type
  TDictionaryDuplicatesAction = (
      ddError,    // raises an exception on duplicate keys
      ddAccept,   // allow duplicate keys
      ddIgnore);  // silently discard duplicates

  ADictionaryBase = class(AType)
  protected
    function  GetAsString: String; override;

    function  GetAddOnSet: Boolean; virtual; abstract;
    procedure SetAddOnSet(const AAddOnSet: Boolean); virtual; abstract;
    function  GetDuplicatesAction: TDictionaryDuplicatesAction; virtual; abstract;
    procedure SetDuplicatesAction(const Value: TDictionaryDuplicatesAction); virtual; abstract;

    function  GetKeyStrByIndex(const Idx: Integer): String; virtual; abstract;
    function  GetItemStrByIndex(const Idx: Integer): String; virtual;

  public
    { ADictionaryBase                                                          }
    property  AddOnSet: Boolean read GetAddOnSet write SetAddOnSet;
    property  DuplicatesAction: TDictionaryDuplicatesAction
              read GetDuplicatesAction write SetDuplicatesAction;

    function  Count: Integer; virtual; abstract;
  end;

  EDictionary = class(EType);



{%DEFINE ATypeDictionaryBase}
{                                                                              }
{-ADictionary%1%                                                               }
{-  Base class for a dictionary with %2% keys.                                 }
{                                                                              }
type
  ADictionary%1% = class(ADictionaryBase)
  protected
    procedure RaiseKeyNotFoundError(const Key: %2%);
    procedure RaiseDuplicateKeyError(const Key: %2%);

    function  GetKeysCaseSensitive: Boolean; virtual; abstract;

  public
    { ADictionary                                                              }
    procedure Delete(const Key: %2%); virtual; abstract;
    function  HasKey(const Key: %2%): Boolean; virtual; abstract;
    procedure Rename(const Key, NewKey: %2%); virtual; abstract;

    function  GetKeyByIndex(const Idx: Integer): %2%; virtual; abstract;
    function  GetKeyStrByIndex(const Idx: Integer): String; override;
    procedure DeleteItemByIndex(const Idx: Integer); virtual; abstract;

    property  KeysCaseSensitive: Boolean read GetKeysCaseSensitive;
  end;


{%ENDDEF}
{$IFDEF SupportAnsiString}
{%TEMPLATE ATypeDictionaryBase 'A' 'AnsiString'    }
{$ENDIF}
{%TEMPLATE ATypeDictionaryBase 'B' 'RawByteString' }
{%TEMPLATE ATypeDictionaryBase 'U' 'UnicodeString' }
{%TEMPLATE ATypeDictionaryBase ''  'String'        }
{%DEFINE ATypeDictionary}
{                                                                              }
{-A%1%Dictionary%5%                                                            }
{-  A Dictionary with %1% values and %6% keys.                                 }
{                                                                              }
type
  A%1%Dictionary%5% = class(ADictionary%5%)
  protected
    function  GetAsString: String; override;{%IF 7}

    function  GetItemStrByIndex(const Idx: Integer): String; override;{%ENDIF}

    function  GetItem(const Key: %6%): %2%; virtual;
    procedure SetItem(const Key: %6%; const Value: %2%); virtual; abstract;{%IF 8}

    function  GetIsItemOwner: Boolean; virtual; abstract;
    procedure SetIsItemOwner(const AIsItemOwner: Boolean); virtual; abstract;{%ENDIF}

  public
    { AType                                                                    }
    procedure Assign(const Source: TObject); override;{%IF 4}
    procedure StreamOut(const Writer: AWriterEx); override;
    procedure StreamIn(const Reader: AReaderEx); override;{%ENDIF}{%IF 8}
    procedure Clear; override;{%ENDIF}

    { A%1%Dictionary                                                      }
    property  Item[const Key: %6%]: %2% read GetItem write SetItem; default;
    procedure Add(const Key: %6%; const Value: %2%); virtual; abstract;

    function  GetItemByIndex(const Idx: Integer): %2%; virtual; abstract;
    function  LocateItem(const Key: %6%; var Value: %2%): Integer; virtual; abstract;
    function  LocateNext(const Key: %6%; const Idx: Integer;
              var Value: %2%): Integer; virtual; abstract;{%IF 3}

    function  GetItemLength(const Key: %6%): Integer; virtual;
    function  GetTotalLength: Int64; virtual;{%ENDIF}{%IF 8}

    property  IsItemOwner: Boolean read GetIsItemOwner write SetIsItemOwner;
    function  ReleaseItem(const Key: %6%): TObject; virtual; abstract;
    procedure ReleaseItems; virtual; abstract;
    procedure FreeItems; virtual; abstract;{%ENDIF}
  end;
  E%1%Dictionary%5% = class(EDictionary);


{%ENDDEF}
{$IFDEF SupportAnsiString}
{%TEMPLATE ATypeDictionary 'LongInt'  'LongInt'  ''  '' 'A' 'AnsiString'    'I' ''  }
{$ENDIF}
{%TEMPLATE ATypeDictionary 'LongInt'  'LongInt'  ''  '' 'B' 'RawByteString' 'I' ''  }
{%TEMPLATE ATypeDictionary 'LongInt'  'LongInt'  ''  '' 'U' 'UnicodeString' 'I' ''  }
{%TEMPLATE ATypeDictionary 'LongInt'  'LongInt'  ''  '' ''  'String'        'I' ''  }
{                                                                              }
{ AIntegerDictionary                                                           }
{                                                                              }
type
  {$IFDEF SupportAnsiString}
  AIntegerDictionaryA = ALongIntDictionaryA;
  {$ENDIF}
  AIntegerDictionaryB = ALongIntDictionaryB;
  AIntegerDictionaryU = ALongIntDictionaryU;
  AIntegerDictionary  = ALongIntDictionary;



{$IFDEF SupportAnsiString}
{%TEMPLATE ATypeDictionary 'LongWord' 'LongWord' ''  '' 'A' 'AnsiString'    'I' ''  }
{$ENDIF}
{%TEMPLATE ATypeDictionary 'LongWord' 'LongWord' ''  '' 'B' 'RawByteString' 'I' ''  }
{%TEMPLATE ATypeDictionary 'LongWord' 'LongWord' ''  '' 'U' 'UnicodeString' 'I' ''  }
{%TEMPLATE ATypeDictionary 'LongWord' 'LongWord' ''  '' ''  'String'        'I' ''  }
{                                                                              }
{ ACardinalDictionary                                                          }
{                                                                              }
type
  {$IFDEF SupportAnsiString}
  ACardinalDictionaryA = ALongWordDictionaryA;
  {$ENDIF}
  ACardinalDictionaryB = ALongWordDictionaryB;
  ACardinalDictionaryU = ALongWordDictionaryU;
  ACardinalDictionary  = ALongWordDictionary;



{$IFDEF SupportAnsiString}
{%TEMPLATE ATypeDictionary 'Int64'         'Int64'         ''  '' 'A' 'AnsiString'    'I' ''  }
{$ENDIF}
{%TEMPLATE ATypeDictionary 'Int64'         'Int64'         ''  '' 'B' 'RawByteString' 'I' ''  }
{%TEMPLATE ATypeDictionary 'Int64'         'Int64'         ''  '' 'U' 'UnicodeString' 'I' ''  }
{%TEMPLATE ATypeDictionary 'Int64'         'Int64'         ''  '' ''  'String'        'I' ''  }
{$IFDEF SupportAnsiString}
{%TEMPLATE ATypeDictionary 'Single'        'Single'        ''  '' 'A' 'AnsiString'    'I' ''  }
{$ENDIF}
{%TEMPLATE ATypeDictionary 'Single'        'Single'        ''  '' 'B' 'RawByteString' 'I' ''  }
{%TEMPLATE ATypeDictionary 'Single'        'Single'        ''  '' 'U' 'UnicodeString' 'I' ''  }
{%TEMPLATE ATypeDictionary 'Single'        'Single'        ''  '' ''  'String'        'I' ''  }
{$IFDEF SupportAnsiString}
{%TEMPLATE ATypeDictionary 'Double'        'Double'        ''  '' 'A' 'AnsiString'    'I' ''  }
{$ENDIF}
{%TEMPLATE ATypeDictionary 'Double'        'Double'        ''  '' 'B' 'RawByteString' 'I' ''  }
{%TEMPLATE ATypeDictionary 'Double'        'Double'        ''  '' 'U' 'UnicodeString' 'I' ''  }
{%TEMPLATE ATypeDictionary 'Double'        'Double'        ''  '' ''  'String'        'I' ''  }
{$IFDEF SupportAnsiString}
{%TEMPLATE ATypeDictionary 'Extended'      'Extended'      ''  '' 'A' 'AnsiString'    'I' ''  }
{$ENDIF}
{%TEMPLATE ATypeDictionary 'Extended'      'Extended'      ''  '' 'B' 'RawByteString' 'I' ''  }
{%TEMPLATE ATypeDictionary 'Extended'      'Extended'      ''  '' 'U' 'UnicodeString' 'I' ''  }
{%TEMPLATE ATypeDictionary 'Extended'      'Extended'      ''  '' ''  'String'        'I' ''  }
{$IFDEF SupportAnsiString}
{%TEMPLATE ATypeDictionary 'AnsiString'    'AnsiString'    'L' '' 'A' 'AnsiString'    'I' ''  }
{%TEMPLATE ATypeDictionary 'AnsiString'    'AnsiString'    'L' '' 'U' 'UnicodeString' 'I' ''  }
{%TEMPLATE ATypeDictionary 'AnsiString'    'AnsiString'    'L' '' ''  'String'        'I' ''  }
{$ENDIF}
{$IFDEF SupportAnsiString}
{%TEMPLATE ATypeDictionary 'RawByteString' 'RawByteString' 'L' '' 'A' 'AnsiString'    'I' ''  }
{$ENDIF}
{%TEMPLATE ATypeDictionary 'RawByteString' 'RawByteString' 'L' '' 'B' 'RawByteString' 'I' ''  }
{%TEMPLATE ATypeDictionary 'RawByteString' 'RawByteString' 'L' '' 'U' 'UnicodeString' 'I' ''  }
{%TEMPLATE ATypeDictionary 'RawByteString' 'RawByteString' 'L' '' ''  'String'        'I' ''  }
{$IFDEF SupportAnsiString}
{%TEMPLATE ATypeDictionary 'UnicodeString' 'UnicodeString' 'L' '' 'A' 'AnsiString'    'I' ''  }
{$ENDIF}
{%TEMPLATE ATypeDictionary 'UnicodeString' 'UnicodeString' 'L' '' 'U' 'UnicodeString' 'I' ''  }
{%TEMPLATE ATypeDictionary 'UnicodeString' 'UnicodeString' 'L' '' ''  'String'        'I' ''  }
{$IFDEF SupportAnsiString}
{%TEMPLATE ATypeDictionary 'String'        'String'        'L' '' 'A' 'AnsiString'    ''  ''  }
{$ENDIF}
{%TEMPLATE ATypeDictionary 'String'        'String'        'L' '' 'U' 'UnicodeString' ''  ''  }
{%TEMPLATE ATypeDictionary 'String'        'String'        'L' '' ''  'String'        ''  ''  }
{$IFDEF SupportAnsiString}
{%TEMPLATE ATypeDictionary 'Pointer'       'Pointer'       ''  '' 'A' 'AnsiString'    'I' ''  }
{$ENDIF}
{%TEMPLATE ATypeDictionary 'Pointer'       'Pointer'       ''  '' 'B' 'RawByteString' 'I' ''  }
{%TEMPLATE ATypeDictionary 'Pointer'       'Pointer'       ''  '' 'U' 'UnicodeString' 'I' ''  }
{%TEMPLATE ATypeDictionary 'Pointer'       'Pointer'       ''  '' ''  'String'        'I' ''  }
{$IFDEF SupportAnsiString}
{%TEMPLATE ATypeDictionary 'Interface'     'IInterface'    ''  '' 'A' 'AnsiString'    ''  ''  }
{$ENDIF}
{%TEMPLATE ATypeDictionary 'Interface'     'IInterface'    ''  '' 'U' 'UnicodeString' ''  ''  }
{%TEMPLATE ATypeDictionary 'Interface'     'IInterface'    ''  '' ''  'String'        ''  ''  }
{$IFDEF SupportAnsiString}
{%TEMPLATE ATypeDictionary 'Object'        'TObject'       ''  '' 'A' 'AnsiString'    'I' 'O' }
{$ENDIF}
{%TEMPLATE ATypeDictionary 'Object'        'TObject'       ''  '' 'B' 'RawByteString' 'I' 'O' }
{%TEMPLATE ATypeDictionary 'Object'        'TObject'       ''  '' 'U' 'UnicodeString' 'I' 'O' }
{%TEMPLATE ATypeDictionary 'Object'        'TObject'       ''  '' ''  'String'        'I' 'O' }



{                                                                              }
{ DICTIONARY IMPLEMENTATIONS                                                   }
{                                                                              }



{%DEFINE ADictionaryAArray}
{                                                                              }
{-T%1%Dictionary                                                               }
{-  Implements A%1%Dictionary using arrays.                                    }
{-  A 'chained-hash' lookup table is used for quick access.                    }
{                                                                              }
type
  TGeneral%1%Dictionary%4% = class(A%1%Dictionary%4%)
  protected
    FKeys             : T%5%Array;
    FValues           : T%1%Array;
    FLookup           : Array of IntegerArray;
    FHashSize         : Integer;
    FCaseSensitive    : Boolean;
    FAddOnSet         : Boolean;
    FDuplicatesAction : TDictionaryDuplicatesAction;

    function  LocateKey(const Key: %5%; var LookupIdx: Word32;
              const ErrorIfNotFound: Boolean): Integer; virtual;
    procedure DeleteByIndex(const Idx: Integer; const Hash: Integer = -1);
    procedure Rehash;
    function  GetHashTableSize: Integer;
    procedure RaiseIndexError;

    { ADictionary                                                              }
    function  GetKeysCaseSensitive: Boolean; override;
    function  GetAddOnSet: Boolean; override;
    procedure SetAddOnSet(const AAddOnSet: Boolean); override;
    function  GetDuplicatesAction: TDictionaryDuplicatesAction; override;
    procedure SetDuplicatesAction(const ADuplicatesAction: TDictionaryDuplicatesAction); override;

    { A%1%Dictionary                                                    }{%IF 3}
    function  GetIsItemOwner: Boolean; override;
    procedure SetIsItemOwner(const AIsItemOwner: Boolean); override;
    {%ENDIF}
    procedure SetItem(const Key: %5%; const Value: %2%); override;

  public
    { TGeneral%1%Dictionary                                               }
    constructor Create;
    constructor CreateEx(
                const AKeys: T%5%Array = nil;
                const AValues: T%1%Array = nil;{%IF 3}
                const AIsItemOwner: Boolean = False;{%ENDIF}
                const AKeysCaseSensitive: Boolean = True;
                const AAddOnSet: Boolean = True;
                const ADuplicatesAction: TDictionaryDuplicatesAction = ddAccept);
    destructor Destroy; override;

    property  Keys: T%5%Array read FKeys;
    property  Values: T%1%Array read FValues;
    property  HashTableSize: Integer read GetHashTableSize;

    { AType                                                                    }
    procedure Clear; override;

    { ADictionary                                                              }
    procedure Delete(const Key: %5%); override;
    function  HasKey(const Key: %5%): Boolean; override;
    procedure Rename(const Key: %5%; const NewKey: %5%); override;
    function  Count: Integer; override;
    function  GetKeyByIndex(const Idx: Integer): %5%; override;
    procedure DeleteItemByIndex(const Idx: Integer); override;

    { A%1%Dictionary                                                    }
    procedure Add(const Key: %5%; const Value: %2%); override;
    function  GetItemByIndex(const Idx: Integer): %2%; override;
    procedure SetItemByIndex(const Idx: Integer; const Value: %2%);
    function  LocateItem(const Key: %5%; var Value: %2%): Integer; override;
    function  LocateNext(const Key: %5%; const Idx: Integer;
              var Value: %2%): Integer; override;{%IF 3}

    function  ReleaseItem(const Key: %5%): TObject; override;
    procedure ReleaseItems; override;
    procedure FreeItems; override;{%ENDIF}
  end;

  T%1%Dictionary%4% = class(TGeneral%1%Dictionary%4%)
  protected
    function  GetItem(const Key: %5%): %2%; override;
    function  LocateKey(const Key: %5%; var LookupIdx: Word32;
              const ErrorIfNotFound: Boolean): Integer; override;

  public
    constructor CreateEx(
                const AKeys: T%5%Array = nil;
                const AValues: T%1%Array = nil;{%IF 3}
                const AIsItemOwner: Boolean = False;{%ENDIF}
                const AKeysCaseSensitive: Boolean = True;
                const AAddOnSet: Boolean = True;
                const ADuplicatesAction: TDictionaryDuplicatesAction = ddAccept);

    function  LocateItem(const Key: %5%; var Value: %2%): Integer; override;
  end;


{%ENDDEF}
{$IFDEF SupportAnsiString}
{%TEMPLATE ADictionaryAArray 'LongInt'  'LongInt'  ''  'A' 'AnsiString'    }
{$ENDIF}
{%TEMPLATE ADictionaryAArray 'LongInt'  'LongInt'  ''  'B' 'RawByteString' }
{%TEMPLATE ADictionaryAArray 'LongInt'  'LongInt'  ''  'U' 'UnicodeString' }
{%TEMPLATE ADictionaryAArray 'LongInt'  'LongInt'  ''  ''  'String'        }
{                                                                              }
{ TIntegerDictionary                                                           }
{                                                                              }
type
  {$IFDEF SupportAnsiString}
  TGeneralIntegerDictionaryA = TGeneralLongIntDictionaryA;
  {$ENDIF}
  TGeneralIntegerDictionary  = TGeneralLongIntDictionary;
  {$IFDEF SupportAnsiString}
  TIntegerDictionaryA = TLongIntDictionaryA;
  {$ENDIF}
  TIntegerDictionary  = TLongIntDictionary;



{$IFDEF SupportAnsiString}
{%TEMPLATE ADictionaryAArray 'LongWord' 'LongWord' ''  'A' 'AnsiString'    }
{$ENDIF}
{%TEMPLATE ADictionaryAArray 'LongWord' 'LongWord' ''  'B' 'RawByteString' }
{%TEMPLATE ADictionaryAArray 'LongWord' 'LongWord' ''  'U' 'UnicodeString' }
{%TEMPLATE ADictionaryAArray 'LongWord' 'LongWord' ''  ''  'String'        }
{                                                                              }
{ TCardinalDictionary                                                          }
{                                                                              }
type
  {$IFDEF SupportAnsiString}
  TGeneralCardinalDictionaryA = TGeneralLongWordDictionaryA;
  {$ENDIF}
  TGeneralCardinalDictionaryU = TGeneralLongWordDictionaryU;
  TGeneralCardinalDictionary  = TGeneralLongWordDictionary;
  {$IFDEF SupportAnsiString}
  TCardinalDictionaryA = TLongWordDictionaryA;
  {$ENDIF}
  TCardinalDictionaryU = TLongWordDictionaryU;
  TCardinalDictionary  = TLongWordDictionary;



{$IFDEF SupportAnsiString}
{%TEMPLATE ADictionaryAArray 'Int64'         'Int64'         ''  'A' 'AnsiString'    }
{$ENDIF}
{%TEMPLATE ADictionaryAArray 'Int64'         'Int64'         ''  'B' 'RawByteString' }
{%TEMPLATE ADictionaryAArray 'Int64'         'Int64'         ''  'U' 'UnicodeString' }
{%TEMPLATE ADictionaryAArray 'Int64'         'Int64'         ''  ''  'String'        }
{$IFDEF SupportAnsiString}
{%TEMPLATE ADictionaryAArray 'Single'        'Single'        ''  'A' 'AnsiString'    }
{$ENDIF}
{%TEMPLATE ADictionaryAArray 'Single'        'Single'        ''  'B' 'RawByteString' }
{%TEMPLATE ADictionaryAArray 'Single'        'Single'        ''  'U' 'UnicodeString' }
{%TEMPLATE ADictionaryAArray 'Single'        'Single'        ''  ''  'String'        }
{$IFDEF SupportAnsiString}
{%TEMPLATE ADictionaryAArray 'Double'        'Double'        ''  'A' 'AnsiString'    }
{$ENDIF}
{%TEMPLATE ADictionaryAArray 'Double'        'Double'        ''  'B' 'RawByteString' }
{%TEMPLATE ADictionaryAArray 'Double'        'Double'        ''  'U' 'UnicodeString' }
{%TEMPLATE ADictionaryAArray 'Double'        'Double'        ''  ''  'String'        }
{$IFDEF SupportAnsiString}
{%TEMPLATE ADictionaryAArray 'Extended'      'Extended'      ''  'A' 'AnsiString'    }
{$ENDIF}
{%TEMPLATE ADictionaryAArray 'Extended'      'Extended'      ''  'B' 'RawByteString' }
{%TEMPLATE ADictionaryAArray 'Extended'      'Extended'      ''  'U' 'UnicodeString' }
{%TEMPLATE ADictionaryAArray 'Extended'      'Extended'      ''  ''  'String'        }
{$IFDEF SupportAnsiString}
{%TEMPLATE ADictionaryAArray 'AnsiString'    'AnsiString'    ''  'A' 'AnsiString'    }
{%TEMPLATE ADictionaryAArray 'AnsiString'    'AnsiString'    ''  'U' 'UnicodeString' }
{%TEMPLATE ADictionaryAArray 'AnsiString'    'AnsiString'    ''  ''  'String'        }
{$ENDIF}
{$IFDEF SupportAnsiString}
{%TEMPLATE ADictionaryAArray 'RawByteString' 'RawByteString' ''  'A' 'AnsiString'    }
{$ENDIF}
{%TEMPLATE ADictionaryAArray 'RawByteString' 'RawByteString' ''  'B' 'RawByteString' }
{%TEMPLATE ADictionaryAArray 'RawByteString' 'RawByteString' ''  'U' 'UnicodeString' }
{%TEMPLATE ADictionaryAArray 'RawByteString' 'RawByteString' ''  ''  'String'        }
{$IFDEF SupportAnsiString}
{%TEMPLATE ADictionaryAArray 'UnicodeString' 'UnicodeString' ''  'A' 'AnsiString'    }
{$ENDIF}
{%TEMPLATE ADictionaryAArray 'UnicodeString' 'UnicodeString' ''  'U' 'UnicodeString' }
{%TEMPLATE ADictionaryAArray 'UnicodeString' 'UnicodeString' ''  ''  'String'        }
{$IFDEF SupportAnsiString}
{%TEMPLATE ADictionaryAArray 'String'        'String'        ''  'A' 'AnsiString'    }
{$ENDIF}
{%TEMPLATE ADictionaryAArray 'String'        'String'        ''  'U' 'UnicodeString' }
{%TEMPLATE ADictionaryAArray 'String'        'String'        ''  ''  'String'        }
{$IFDEF SupportAnsiString}
{%TEMPLATE ADictionaryAArray 'Pointer'       'Pointer'       ''  'A' 'AnsiString'    }
{$ENDIF}
{%TEMPLATE ADictionaryAArray 'Pointer'       'Pointer'       ''  'B' 'RawByteString' }
{%TEMPLATE ADictionaryAArray 'Pointer'       'Pointer'       ''  'U' 'UnicodeString' }
{%TEMPLATE ADictionaryAArray 'Pointer'       'Pointer'       ''  ''  'String'        }
{$IFDEF SupportAnsiString}
{%TEMPLATE ADictionaryAArray 'Interface'     'IInterface'    ''  'A' 'AnsiString'    }
{$ENDIF}
{%TEMPLATE ADictionaryAArray 'Interface'     'IInterface'    ''  'U' 'UnicodeString' }
{%TEMPLATE ADictionaryAArray 'Interface'     'IInterface'    ''  ''  'String'        }
{$IFDEF SupportAnsiString}
{%TEMPLATE ADictionaryAArray 'Object'        'TObject'       'O' 'A' 'AnsiString'    }
{$ENDIF}
{%TEMPLATE ADictionaryAArray 'Object'        'TObject'       'O' 'B' 'RawByteString' }
{%TEMPLATE ADictionaryAArray 'Object'        'TObject'       'O' 'U' 'UnicodeString' }
{%TEMPLATE ADictionaryAArray 'Object'        'TObject'       'O' ''  'String'        }
{                                                                              }
{ Dictionary functions                                                         }
{                                                                              }
const
  DictionaryAverageHashChainSize = 4;

function  DictionaryRehashSize(const Count: Integer): Integer;



{                                                                              }
{ SPARSE ARRAY BASE CLASSES                                                    }
{                                                                              }



{                                                                              }
{ ASparseArray                                                                 }
{   Sparse array base class.                                                   }
{                                                                              }
type
  ASparseArray = class(AType)
  protected
    procedure IndexError;
    function  GetCount: Integer; virtual; abstract;

  public
    property  Count: Integer read GetCount;
    function  IsEmpty: Boolean; override;
    procedure Delete(const Idx: Integer); virtual; abstract;
    function  HasItem(const Idx: Integer): Boolean; virtual; abstract;
  end;
  ESparseArray = class(EType);



{                                                                              }
{ SPARSE ARRAY IMPLEMENTATIONS                                                 }
{                                                                              }



{$IFDEF SupportAnsiString}
{                                                                              }
{ TSparseAnsiStringArray                                                       }
{   Sparse array that holds String values.                                     }
{                                                                              }
type
  TSparseAnsiStringRecord = record
    Idx   : Integer;
    Value : AnsiString;
  end;
  PSparseAnsiStringRecord = ^TSparseAnsiStringRecord;
  TSparseAnsiStringRecordArray = Array of TSparseAnsiStringRecord;
  TSparseAnsiStringArrayHashList = Array of TSparseAnsiStringRecordArray;

  TSparseAnsiStringArray = class(ASparseArray)
  private
    FHashList : TSparseAnsiStringArrayHashList;
    FHashSize : Integer;
    FCount    : Integer;

  protected
    function  LocateItemRecord(const Idx: Integer;
              var LookupIdx, ChainIdx: Integer): PSparseAnsiStringRecord;
    procedure Rehash;

    function  GetCount: Integer; override;
    function  GetItem(const Idx: Integer): AnsiString;
    procedure SetItem(const Idx: Integer; const Value: AnsiString);

  public
    procedure Assign(const Source: TObject); override;
    function  IsEqual(const V: TObject): Boolean; override;

    property  Item[const Idx: Integer]: AnsiString read GetItem write SetItem; default;
    function  LocateItem(const Idx: Integer; var Value: AnsiString): Boolean;

    property  Count: Integer read FCount;
    function  IsEmpty: Boolean; override;
    procedure Clear; override;

    procedure Delete(const Idx: Integer); override;

    function  HasItem(const Idx: Integer): Boolean; override;
    function  FindFirst(var Idx: Integer; var Value: AnsiString): Boolean;
    function  FindNext(var Idx: Integer; var Value: AnsiString): Boolean;
  end;
  ESparseAnsiStringArray = class(ESparseArray);
{$ENDIF}



{                                                                              }
{ TSparseInt64Array                                                            }
{   Sparse array that holds Int64 values.                                      }
{                                                                              }
type
  TSparseInt64Record = record
    Idx   : Integer;
    Value : Int64;
  end;
  PSparseInt64Record = ^TSparseInt64Record;
  TSparseInt64RecordArray = Array of TSparseInt64Record;
  TSparseInt64ArrayHashList = Array of TSparseInt64RecordArray;

  TSparseInt64Array = class(ASparseArray)
  private
    FHashList : TSparseInt64ArrayHashList;
    FHashSize : Integer;
    FCount    : Integer;

  protected
    function  LocateItemRecord(const Idx: Integer;
              var LookupIdx, ChainIdx: Integer): PSparseInt64Record;
    procedure Rehash;

    function  GetCount: Integer; override;
    function  GetItem(const Idx: Integer): Int64;
    procedure SetItem(const Idx: Integer; const Value: Int64);

  public
    procedure Assign(const Source: TObject); override;
    function  IsEqual(const V: TObject): Boolean; override;

    property  Item[const Idx: Integer]: Int64 read GetItem write SetItem; default;
    function  LocateItem(const Idx: Integer; var Value: Int64): Boolean;

    property  Count: Integer read FCount;
    function  IsEmpty: Boolean; override;
    procedure Clear; override;

    procedure Delete(const Idx: Integer); override;

    function  HasItem(const Idx: Integer): Boolean; override;
    function  FindFirst(var Idx: Integer; var Value: Int64): Boolean;
    function  FindNext(var Idx: Integer; var Value: Int64): Boolean;
  end;
  ESparseInt64Array = class(ESparseArray);



{                                                                              }
{ TSparseExtendedArray                                                         }
{   Sparse array that holds Extended values.                                   }
{                                                                              }
type
  TSparseExtendedRecord = record
    Idx   : Integer;
    Value : Extended;
  end;
  PSparseExtendedRecord = ^TSparseExtendedRecord;
  TSparseExtendedRecordArray = Array of TSparseExtendedRecord;
  TSparseExtendedArrayHashList = Array of TSparseExtendedRecordArray;

  TSparseExtendedArray = class(ASparseArray)
  private
    FHashList : TSparseExtendedArrayHashList;
    FHashSize : Integer;
    FCount    : Integer;

  protected
    function  LocateItemRecord(const Idx: Integer;
              var LookupIdx, ChainIdx: Integer): PSparseExtendedRecord;
    procedure Rehash;

    function  GetCount: Integer; override;
    function  GetItem(const Idx: Integer): Extended;
    procedure SetItem(const Idx: Integer; const Value: Extended);

  public
    procedure Assign(const Source: TObject); override;
    function  IsEqual(const V: TObject): Boolean; override;

    property  Item[const Idx: Integer]: Extended read GetItem write SetItem; default;
    function  LocateItem(const Idx: Integer; var Value: Extended): Boolean;

    property  Count: Integer read FCount;
    function  IsEmpty: Boolean; override;
    procedure Clear; override;

    procedure Delete(const Idx: Integer); override;

    function  HasItem(const Idx: Integer): Boolean; override;
    function  FindFirst(var Idx: Integer; var Value: Extended): Boolean;
    function  FindNext(var Idx: Integer; var Value: Extended): Boolean;
  end;
  ESparseExtendedArray = class(ESparseArray);



{                                                                              }
{ TSparseObjectArray                                                           }
{   Sparse array that holds TObject values.                                    }
{                                                                              }
type
  TSparseObjectRecord = record
    Idx   : Integer;
    Value : TObject;
  end;
  PSparseObjectRecord = ^TSparseObjectRecord;
  TSparseObjectRecordArray = Array of TSparseObjectRecord;
  TSparseObjectArrayHashList = Array of TSparseObjectRecordArray;

  TSparseObjectArray = class(ASparseArray)
  private
    FHashList    : TSparseObjectArrayHashList;
    FHashSize    : Integer;
    FCount       : Integer;
    FIsItemOwner : Boolean;

  protected
    procedure Init; override;

    function  LocateItemRecord(const Idx: Integer;
              var LookupIdx, ChainIdx: Integer): PSparseObjectRecord;
    procedure Rehash;

    function  GetCount: Integer; override;
    function  GetItem(const Idx: Integer): TObject;
    procedure SetItem(const Idx: Integer; const Value: TObject);

  public
    constructor Create(const AIsItemOwner: Boolean = False);
    destructor Destroy; override;

    procedure Assign(const Source: TObject); override;
    function  IsEqual(const V: TObject): Boolean; override;

    property  IsItemOwner: Boolean read FIsItemOwner write FIsItemOwner;
    property  Item[const Idx: Integer]: TObject read GetItem write SetItem; default;
    function  LocateItem(const Idx: Integer; var Value: TObject): Boolean;

    property  Count: Integer read FCount;
    function  IsEmpty: Boolean; override;
    procedure Clear; override;

    procedure Delete(const Idx: Integer); override;
    function  ReleaseItem(const Idx: Integer): TObject;

    function  HasItem(const Idx: Integer): Boolean; override;
    function  FindFirst(var Idx: Integer; var Value: TObject): Boolean;
    function  FindNext(var Idx: Integer; var Value: TObject): Boolean;
  end;
  ESparseObjectArray = class(ESparseArray);



{                                                                              }
{ Linked lists                                                                 }
{                                                                              }
type
  TDoublyLinkedItem = class
  protected
    FNext : TDoublyLinkedItem;
    FPrev : TDoublyLinkedItem;

  public
    destructor DestroyList;

    property  Next: TDoublyLinkedItem read FNext write FNext;
    property  Prev: TDoublyLinkedItem read FPrev write FPrev;

    function  HasNext: Boolean;
    function  HasPrev: Boolean;
    function  Last: TDoublyLinkedItem;
    function  First: TDoublyLinkedItem;
    function  Count: Integer;

    procedure Remove;
    function  RemoveNext: TDoublyLinkedItem;
    procedure DeleteNext;
    function  RemovePrev: TDoublyLinkedItem;
    procedure DeletePrev;
    procedure InsertAfter(const Item: TDoublyLinkedItem);
    procedure InsertBefore(const Item: TDoublyLinkedItem);
    procedure Delete;
  end;

{%DEFINE LinkedList}
  TDoublyLinked%1% = class(TDoublyLinkedItem)
  public
    Value : %2%;

    constructor Create(const V: %2%);

    procedure InsertAfter(const V: %2%); overload;
    procedure InsertBefore(const V: %2%); overload;
    procedure InsertFirst(const V: %2%);
    procedure Append(const V: %2%);
    function  FindNext(const Find: %2%): TDoublyLinked%1%;
    function  FindPrev(const Find: %2%): TDoublyLinked%1%;
  end;
{%ENDDEF}
{%TEMPLATE LinkedList 'Integer'  'Integer'}
{%TEMPLATE LinkedList 'Extended' 'Extended'}
{$IFDEF SupportAnsiString}
{%TEMPLATE LinkedList 'String'   'AnsiString'}
{$ENDIF}
{%TEMPLATE LinkedList 'Object'   'TObject'}

{%DEFINE OpenArrayAsLinkedList}
function  As%1%Linked%2%List(const V: Array of %3%): T%1%Linked%2%;{%ENDDEF}
{%TEMPLATE OpenArrayAsLinkedList 'Doubly' 'Integer'  'Integer'   }
{%TEMPLATE OpenArrayAsLinkedList 'Doubly' 'Extended' 'Extended'  }
{$IFDEF SupportAnsiString}
{%TEMPLATE OpenArrayAsLinkedList 'Doubly' 'String'   'AnsiString'}
{$ENDIF}



{                                                                              }
{ TDoublyLinkedList                                                            }
{                                                                              }
type
  TDoublyLinkedList = class
  protected
    FFirst : TDoublyLinkedItem;
    FLast  : TDoublyLinkedItem;
    FCount : Integer;

  public
    destructor Destroy; override;

    property  First: TDoublyLinkedItem read FFirst;
    property  Last: TDoublyLinkedItem read FLast;
    function  IsEmpty: Boolean;
    property  Count: Integer read FCount;

    procedure Remove(const Item: TDoublyLinkedItem);
    function  RemoveFirst: TDoublyLinkedItem;
    function  RemoveLast: TDoublyLinkedItem;

    procedure Delete(const Item: TDoublyLinkedItem);
    procedure DeleteFirst;
    procedure DeleteLast;
    procedure DeleteList;

    procedure Append(const Item: TDoublyLinkedItem);
    procedure InsertFront(const Item: TDoublyLinkedItem);
  end;



{                                                                              }
{ Self testing code                                                            }
{                                                                              }
{$IFDEF DEBUG}
{$IFDEF TEST}
procedure Test;
{$ENDIF}
{$ENDIF}



implementation

uses
  { Fundamentals }
  flcDynArrays,
  flcUTF,
  flcStrings;



{                                                                              }
{ AType                                                                        }
{                                                                              }
constructor AType.Create;
begin
  inherited Create;
  Init;
end;

procedure AType.Init;
begin
end;

procedure AType.RaiseTypeError(const Msg: String; const ErrorClass: ExceptClass);
begin
  if Assigned(ErrorClass) then
    raise ErrorClass.Create(Msg)
  else
    raise EType.Create(Msg);
end;

class function AType.CreateInstance: AType;
begin
  Result := AType(TypeClass(self).Create);
end;

procedure AType.Clear;
begin
  raise EType.CreateFmt('Method %s.Clear not implemented', [ClassName]);
end;

function AType.IsEmpty: Boolean;
begin
  raise EType.CreateFmt('Method %s.IsEmpty not implemented', [ClassName]);
end;

function AType.Duplicate: TObject;
begin
  try
    Result := CreateInstance;
    try
      AType(Result).Assign(self);
    except
      Result.Free;
      raise;
    end;
  except
    on E : Exception do
      raise EType.CreateFmt('%s cannot duplicate: %s', [ClassName, E.Message]);
  end;
end;

procedure AType.Assign(const Source: TObject);
var R : Boolean;
begin
  if Source is AType then
    try
      AType(Source).AssignTo(self);
      R := True;
    except
      R := False;
    end
  else
    R := False;
  if not R then
    raise EType.CreateFmt('%s cannot assign from %s', [ClassName, ObjectClassName(Source)]);
end;

procedure AType.AssignTo(const Dest: TObject);
begin
  raise EType.CreateFmt('%s cannot assign to %s', [ClassName, ObjectClassName(Dest)]);
end;

function AType.IsEqual(const V: TObject): Boolean;
begin
  raise EType.CreateFmt('%s cannot compare with %s', [ClassName, ObjectClassName(V)]);
end;

function AType.Compare(const V: TObject): TCompareResult;
begin
  raise EType.CreateFmt('%s cannot compare with %s', [ClassName, ObjectClassName(V)]);
end;

function AType.HashValue: Word32;
begin
  try
    Result := HashStr(GetAsString, 1, -1, True);
  except
    on E : Exception do
      raise EType.CreateFmt('Hash error: %s', [E.Message]);
  end;
end;

function AType.GetAsString: String;
begin
  raise EType.CreateFmt('Method %s.GetAsString not implemented', [ClassName]);
end;

function AType.GetAsUTF8String: RawByteString;
begin
  {$IFDEF StringIsUnicode}
  Result := StringToUTF8String(GetAsString);
  {$ELSE}
  Result := GetAsString;
  {$ENDIF}
end;

function AType.GetAsUnicodeString: UnicodeString;
begin
  {$IFDEF StringIsUnicode}
  Result := GetAsString;
  {$ELSE}
  Result := UTF8Decode(GetAsString);
  {$ENDIF}
end;

procedure AType.SetAsUTF8String(const S: RawByteString);
begin
  raise EType.CreateFmt('Method %s.SetAsUTF8String not implemented', [ClassName]);
end;

procedure AType.SetAsUnicodeString(const S: UnicodeString);
begin
  raise EType.CreateFmt('Method %s.SetAsUnicodeString not implemented', [ClassName]);
end;

procedure AType.SetAsString(const S: String);
begin
  raise EType.CreateFmt('Method %s.SetAsString not implemented', [ClassName]);
end;



{                                                                              }
{ AType helper functions                                                       }
{                                                                              }
function TypeGetAsString(const V: TObject): String;
begin
  if V is AType then
    Result := AType(V).GetAsString
  else
    raise EType.CreateFmt('%s cannot convert to string', [ObjectClassName(V)]);
end;

procedure TypeSetAsString(const V: TObject; const S: String);
begin
  if V is AType then
    AType(V).SetAsString(S)
  else
    raise EType.CreateFmt('%s cannot set as string', [ObjectClassName(V)]);
end;

function TypeGetAsUTF8String(const V: TObject): RawByteString;
begin
  if V is AType then
    Result := AType(V).GetAsUTF8String
  else
    raise EType.CreateFmt('%s cannot convert to utf-8 string', [ObjectClassName(V)]);
end;

procedure TypeSetAsUTF8String(const V: TObject; const S: RawByteString);
begin
  if V is AType then
    AType(V).SetAsUTF8String(S)
  else
    raise EType.CreateFmt('%s cannot set as utf-8 string', [ObjectClassName(V)]);
end;

function TypeGetAsUnicodeString(const V: TObject): UnicodeString;
begin
  if V is AType then
    Result := AType(V).GetAsUnicodeString
  else
    raise EType.CreateFmt('%s cannot convert to utf-16 string', [ObjectClassName(V)]);
end;

procedure TypeSetAsUnicodeString(const V: TObject; const S: UnicodeString);
begin
  if V is AType then
    AType(V).SetAsUnicodeString(S)
  else
    raise EType.CreateFmt('%s cannot set as utf-16 string', [ObjectClassName(V)]);
end;

function TypeDuplicate(const V: TObject): TObject;
begin
  if V is AType then
    Result := AType(V).Duplicate else
  if not Assigned(V) then
    Result := nil
  else
    raise EType.CreateFmt('%s cannot duplicate', [ObjectClassName(V)]);
end;

procedure TypeClear(const V: TObject);
begin
  if V is AType then
    AType(V).Clear else
  if Assigned(V) then
    raise EType.CreateFmt('%s cannot clear', [ObjectClassName(V)]);
end;

function TypeIsEqual(const A, B: TObject): Boolean;
begin
  if A = B then
    Result := True else
  if not Assigned(A) or not Assigned(B) then
    Result := False else
  if A is AType then
    Result := AType(A).IsEqual(B) else
  if B is AType then
    Result := AType(B).IsEqual(A)
  else
    raise EType.CreateFmt('%s and %s cannot compare',
        [ObjectClassName(A), ObjectClassName(B)]);
end;

function TypeCompare(const A, B: TObject): TCompareResult;
begin
  if A = B then
    Result := crEqual else
  if A is AType then
    Result := AType(A).Compare(B) else
  if B is AType then
    Result := InverseCompareResult(AType(B).Compare(A))
  else
    Result := crUndefined;
end;

procedure TypeAssign(const A, B: TObject);
begin
  if A = B then
    exit else
  if A is AType then
    AType(A).Assign(B) else
  if B is AType then
    AType(B).AssignTo(A)
  else
    raise EType.CreateFmt('%s cannot assign %s',
        [ObjectClassName(A), ObjectClassName(B)]);
end;

function TypeHashValue(const A: TObject): Word32;
begin
  if not Assigned(A) then
    Result := 0 else
  if A is AType then
    Result := AType(A).HashValue
  else
    raise EType.CreateFmt('%s cannot calculate hash value', [A.ClassName]);
end;



{                                                                              }
{                                                                              }
{ TYPE BASE CLASSES                                                            }
{                                                                              }
{                                                                              }



{                                                                              }
{ AArray                                                                       }
{                                                                              }
procedure AArray.RaiseIndexError(const Idx: Integer);
begin
  raise EArray.Create(
      'Array index out of bounds'
      {$IFDEF DEBUG} + ': ' + IntToStr(Idx) + '/' + IntToStr(GetCount){$ENDIF}
      );
end;

function AArray.GetItemAsString(const Idx: Integer): String;
begin
  raise EArray.CreateFmt('%s.GetItemAsString not implemented', [ClassName]);
end;

procedure AArray.SetItemAsString(const Idx: Integer; const Value: String);
begin
  raise EArray.CreateFmt('%s.SetItemAsString not implemented', [ClassName]);
end;

procedure AArray.Clear;
begin
  Count := 0;
end;

procedure AArray.Sort;

  procedure QuickSort(L, R: Integer);
  var I, J : Integer;
      M    : Integer;
    begin
      repeat
        I := L;
        J := R;
        M := (L + R) shr 1;
        repeat
          while CompareItems(I, M) = crLess do
            Inc(I);
          while CompareItems(J, M) = crGreater do
            Dec(J);
          if I <= J then
            begin
              ExchangeItems(I, J);
              if M = I then
                M := J else
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

var I : Integer;
begin
  I := Count;
  if I > 0 then
    QuickSort(0, I - 1);
end;

procedure AArray.ReverseOrder;
var I, L : Integer;
begin
  L := Count;
  for I := 1 to L div 2 do
    ExchangeItems(I - 1, L - I);
end;

function AArray.GetAsString: String;
var I, L : Integer;
begin
  L := Count;
  if L = 0 then
    begin
      Result := '';
      exit;
    end;
  Result := GetItemAsString(0);
  for I := 1 to L - 1 do
    Result := Result + ',' + GetItemAsString(I);
end;

procedure AArray.SetAsString(const S: String);
var F, G, L, C : Integer;
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

procedure AArray.RemoveDuplicates(const IsSortedAscending: Boolean);
var I, C, J, L : Integer;
begin
  L := GetCount;
  if L = 0 then
    exit;
  if IsSortedAscending then
    begin
      J := 0;
      repeat
        I := J + 1;
        while (I < L) and (CompareItems(I, J) = crEqual) do
          Inc(I);
        C := I - J;
        if C > 1 then
          begin
            Delete(J + 1, C - 1);
            Dec(L, C - 1);
            Inc(J);
          end
        else
          J := I;
      until J >= L;
    end else
    begin
      J := 0;
      while J < L - 1 do
        begin
          I := J + 1;
          while I <= L - 1 do
            if CompareItems(J, I) = crEqual then
              begin
                Delete(I, 1);
                Dec(L);
              end else
              Inc(I);
          Inc(J);
        end;
    end;
end;


{%DEFINE ATypeArrayImpl}

{                                                                              }
{-A%1%Array                                                                    }
{                                                                              }
procedure A%1%Array.ExchangeItems(const Idx1, Idx2: Integer);
var I : %2%;
begin
  I := Item[Idx1];
  Item[Idx1] := Item[Idx2];
  Item[Idx2] := I;
end;

function A%1%Array.AppendItem(const Value: %2%): Integer;
begin
  Result := Count;
  Count := Result + 1;
  Item[Result] := Value;
end;

function A%1%Array.GetRange(const LoIdx, HiIdx: Integer): %1%Array;
var I, L, H, C : Integer;
begin
  L := MaxInt(0, LoIdx);
  H := MinInt(Count - 1, HiIdx);
  C := H - L + 1;
  SetLength(Result, C);
  for I := 0 to C - 1 do
    Result[I] := Item[L + I];
end;

function A%1%Array.DuplicateRange(const LoIdx, HiIdx: Integer): AArray;
var I, L, H, C : Integer;
begin
  Result := A%1%Array(CreateInstance);
  L := MaxInt(0, LoIdx);
  H := MinInt(Count - 1, HiIdx);
  C := H - L + 1;
  A%1%Array(Result).Count := C;
  for I := 0 to C - 1 do
    A%1%Array(Result)[I] := Item[L + I];
end;

procedure A%1%Array.SetRange(const LoIdx, HiIdx: Integer; const V: %1%Array);
var I, L, H, C : Integer;
begin
  L := MaxInt(0, LoIdx);
  H := MinInt(Count - 1, HiIdx);
  C := MinInt(Length(V), H - L + 1);
  for I := 0 to C - 1 do
    Item[L + I] := V[I];
end;

procedure A%1%Array.Fill(const Idx, ACount: Integer; const Value: %2%);
var I : Integer;
begin
  for I := Idx to Idx + ACount - 1 do
    Item[I] := Value;
end;

function A%1%Array.AppendArray(const V: %1%Array): Integer;
begin
  Result := Count;
  Count := Result + Length(V);
  Range[Result, Count - 1] := V;
end;

function A%1%Array.CompareItems(const Idx1, Idx2: Integer): TCompareResult;
var I, J : %2%;
begin
  I := Item[Idx1];
  J := Item[Idx2];
  if %7%I{%IF 7}){%ENDIF} < %7%J{%IF 7}){%ENDIF} then
    Result := crLess else
  if %7%I{%IF 7}){%ENDIF} > %7%J{%IF 7}){%ENDIF} then
    Result := crGreater else
    Result := crEqual;
end;

function A%1%Array.PosNext(const Find: %2%;
    const PrevPos: Integer; const IsSortedAscending: Boolean): Integer;
var I, L, H : Integer;
    D       : %2%;
begin
  if IsSortedAscending then // binary search
    begin
      if MaxInt(PrevPos + 1, 0) = 0 then // find first
        begin
          L := 0;
          H := Count - 1;
          repeat
            I := (L + H) div 2;
            D := Item[I];
            if D = Find then
              begin
                while (I > 0) and (Item[I - 1] = Find) do
                  Dec(I);
                Result := I;
                exit;
              end else
            if %7%D{%IF 7}){%ENDIF} > %7%Find{%IF 7}){%ENDIF} then
              H := I - 1 else
              L := I + 1;
          until L > H;
          Result := -1;
        end else // find next
        if PrevPos >= Count - 1 then
          Result := -1 else
          if Item[PrevPos + 1] = Find then
            Result := PrevPos + 1 else
            Result := -1;
    end else // linear search
    begin
      for I := MaxInt(PrevPos + 1, 0) to Count - 1 do
        if Item[I] = Find then
          begin
            Result := I;
            exit;
          end;
      Result := -1;
    end;
end;

{%IF 8}function A%1%Array.GetItemAsString(const Idx: Integer): String;
begin
  Result := %8%(GetItem(Idx));
end;
{%ENDIF}
{%IF 9}procedure A%1%Array.SetItemAsString(const Idx: Integer; const Value: String);
begin
  SetItem(Idx, %9%(Value));
end;
{%ENDIF}
procedure A%1%Array.Assign(const Source: TObject);
var I, L : Integer;
begin
  if Source is A%1%Array then
    begin
      L := A%1%Array(Source).Count;
      Count := L;
      for I := 0 to L - 1 do
        Item[I] := A%1%Array(Source).Item[I];
    end else{%IF 6}
  if Source is A%6%Array then
    begin
      L := A%6%Array(Source).Count;
      Count := L;
      for I := 0 to L - 1 do
        Item[I] := A%6%Array(Source).Item[I];
    end else{%ENDIF}
    inherited Assign(Source);
end;

function A%1%Array.IsEqual(const V: TObject): Boolean;
var I, L : Integer;
begin
  if V is A%1%Array then
    begin
      L := A%1%Array(V).Count;
      Result := L = Count;
      if not Result then
        exit;
      for I := 0 to L - 1 do
        if Item[I] <> A%1%Array(V).Item[I] then
          begin
            Result := False;
            exit;
          end;
    end else
    Result := inherited IsEqual(V);
end;

function A%1%Array.AppendArray(const V: AArray): Integer;
var I, L : Integer;
begin
  Result := Count;
  if V is A%1%Array then
    begin
      L := V.Count;
      Count := Result + L;
      for I := 0 to L - 1 do
        Item[Result + I] := A%1%Array(V)[I];
    end
  else
    raise E%1%Array.CreateFmt('%s can not append %s', [ClassName, ObjectClassName(V)]);
end;

procedure A%1%Array.Delete(const Idx: Integer; const ACount: Integer);
var I, C, J, L : Integer;
begin
  J := MaxInt(Idx, 0);
  C := GetCount;
  L := MinInt(ACount, C - J);
  if L > 0 then
    begin
      for I := J to J + C - 1 do
        SetItem(I, GetItem(I + ACount));
      SetCount(C - L);
    end;
end;

procedure A%1%Array.Insert(const Idx: Integer; const ACount: Integer);
var I, C, J, L : Integer;
begin
  if ACount <= 0 then
    exit;
  C := GetCount;
  SetCount(C + ACount);
  J := MinInt(MaxInt(Idx, 0), C);
  L := C - J;
  for I := C - 1 downto C - L do
    SetItem(I + ACount, GetItem(I));
end;
{%ENDDEF}
{%TEMPLATE ATypeArrayImpl  'Int32'         'Int32'         ''              ''              '0'    ''         ''           'IntToStr'     'StrToInt'        }
{%TEMPLATE ATypeArrayImpl  'Int64'         'Int64'         ''              ''              '0'    'LongInt'  ''           'IntToStr'     'StrToInt64'      }
{%TEMPLATE ATypeArrayImpl  'LongInt'       'LongInt'       ''              ''              '0'    ''         ''           'IntToStr'     'StrToInt'        }
{%TEMPLATE ATypeArrayImpl  'Word32'        'Word32'        ''              ''              '0'    ''         ''           'IntToStr'     'StrToInt64'      }
{%TEMPLATE ATypeArrayImpl  'Word64'        'Word64'        ''              ''              '0'    ''         ''           'UIntToString' 'StringToUInt64'  }
{%TEMPLATE ATypeArrayImpl  'LongWord'      'LongWord'      ''              ''              '0'    ''         ''           'IntToStr'     'StrToInt64'      }
{%TEMPLATE ATypeArrayImpl  'Single'        'Single'        ''              ''              '0.0'  'Int64'    ''           'FloatToStr'   'StrToFloat'      }
{%TEMPLATE ATypeArrayImpl  'Double'        'Double'        ''              ''              '0.0'  'Int64'    ''           'FloatToStr'   'StrToFloat'      }
{%TEMPLATE ATypeArrayImpl  'Extended'      'Extended'      ''              ''              '0.0'  'Int64'    ''           'FloatToStr'   'StrToFloat'      }
{$IFDEF SupportAnsiString}
{%TEMPLATE ATypeArrayImpl  'AnsiString'    'AnsiString'    ''              ''              '''''' ''         ''           'ToStringA'    'ToAnsiString'    }
{$ENDIF}
{%TEMPLATE ATypeArrayImpl  'RawByteString' 'RawByteString' ''              ''              '''''' ''         ''           'ToStringB'    'ToRawByteString' }
{%TEMPLATE ATypeArrayImpl  'UnicodeString' 'UnicodeString' ''              ''              '''''' ''         ''           'ToStringU'    'ToUnicodeString' }
{%TEMPLATE ATypeArrayImpl  'String'        'String'        ''              ''              '''''' ''         ''           ''             ''                }
{%TEMPLATE ATypeArrayImpl  'Pointer'       'Pointer'       ''              ''              'nil'  ''         'NativeInt(' 'PointerToStr' 'StrToPointer'    }
{%TEMPLATE ATypeArrayImpl  'Interface'     'IInterface'    ''              ''              'nil'  ''         'NativeInt(' ''             ''                }

{                                                                              }
{ AObjectArray                                                                 }
{                                                                              }
procedure AObjectArray.Clear;
begin
  if IsItemOwner then
    FreeItems
  else
    ReleaseItems;
end;

procedure AObjectArray.Assign(const Source: TObject);
var I, L : Integer;
    V    : TObject;
begin
  if Source is AObjectArray then
    begin
      FreeItems;
      IsItemOwner := AObjectArray(Source).IsItemOwner;
      L := AObjectArray(Source).Count;
      Count := L;
      if GetIsItemOwner then
        for I := 0 to L - 1 do
          begin
            V := AObjectArray(Source)[I];
            if V is AArray then
              Item[I] := AArray(V).Duplicate else
              Item[I] := V;
          end
      else
        for I := 0 to L - 1 do
          Item[I] := AObjectArray(Source)[I];
    end else
    inherited Assign(Source);
end;

function AObjectArray.IsEqual(const V: TObject): Boolean;
var I, L : Integer;
    A, B : TObject;
begin
  if V is AObjectArray then
    begin
      L := AArray(V).Count;
      if Count <> L then
        begin
          Result := False;
          exit;
        end;
      for I := 0 to L - 1 do
        begin
          A := Item[I];
          B := AObjectArray(V)[I];
          Result := A = B;
          if not Result then
            exit;
          end;
      Result := True;
    end else
    Result := inherited IsEqual(V);
end;

function AObjectArray.Compare(const V: TObject): TCompareResult;
var I, C1, C2 : Integer;
    A, B : TObject;
begin
  if V is AObjectArray then
    begin
      C1 := GetCount;
      C2 := AObjectArray(V).GetCount;
      if C1 < C2 then
        Result := crLess else
      if C1 > C2 then
        Result := crGreater else
        begin
          Result := crEqual;
          for I := 0 to GetCount - 1 do
            begin
              A := Item[I];
              B := AObjectArray(V)[I];
              if A <> B then
                begin
                  Result := crUndefined;
                  exit;
                end;
            end;
        end;
    end else
    Result := inherited Compare(V);
end;

function AObjectArray.GetRange(const LoIdx, HiIdx: Integer): ObjectArray;
var I, L, H, C : Integer;
begin
  L := MaxInt(0, LoIdx);
  H := MinInt(Count - 1, HiIdx);
  C := H - L  + 1;
  SetLength(Result, C);
  for I := 0 to C - 1 do
    Result[L + I] := Item[I];
end;

procedure AObjectArray.SetRange(const LoIdx, HiIdx: Integer; const V: ObjectArray);
var I, L, H, C : Integer;
begin
  L := MaxInt(0, LoIdx);
  H := MinInt(Count - 1, HiIdx);
  C := MinInt(Length(V), H - L  + 1);
  for I := 0 to C - 1 do
    Item[L + I] := V[I];
end;

function AObjectArray.GetAsString: String;
var I, L : Integer;
    V : TObject;
begin
  Result := '';
  L := Count;
  for I := 0 to L - 1 do
    begin
      V := Item[I];
      Result := Result + PointerToStr(Pointer(V));
      if I < L - 1 then
        Result := Result + ',';
    end;
end;

procedure AObjectArray.ExchangeItems(const Idx1, Idx2: Integer);
var I : TObject;
begin
  I := Item[Idx1];
  Item[Idx1] := Item[Idx2];
  Item[Idx2] := I;
end;

function AObjectArray.AppendItem(const Value: TObject): Integer;
begin
  Result := Count;
  Count := Result + 1;
  Item[Result] := Value;
end;

function AObjectArray.AppendArray(const V: ObjectArray): Integer;
begin
  Result := Count;
  Count := Result + Length(V);
  Range[Result, Count - 1] := V;
end;

{$WARNINGS OFF}
function AObjectArray.AppendArray(const V: AArray): Integer;
var I, L : Integer;
begin
  if V is AObjectArray then
    begin
      Result := Count;
      L := V.Count;
      Count := Result + L;
      for I := 0 to L - 1 do
        Item[Result + I] := AObjectArray(V)[I];
    end
  else
    raise EObjectArray.CreateFmt('%s can not append %s', [ClassName, ObjectClassName(V)]);
end;
{$IFDEF DEBUG}{$IFNDEF FREEPASCAL}{$WARNINGS ON}{$ENDIF}{$ENDIF}

procedure AObjectArray.Delete(const Idx: Integer; const ACount: Integer);
var I, C, J, L : Integer;
begin
  J := MaxInt(Idx, 0);
  C := GetCount;
  L := MinInt(ACount, C - J);
  if L > 0 then
    begin
      for I := J to J + C - 1 do
        SetItem(Idx + I, GetItem(Idx + ACount + I));
      SetCount(C - L);
    end;
end;

function AObjectArray.PosNext(const Find: TObject; const PrevPos: Integer): Integer;
var I : Integer;
begin
  for I := MaxInt(PrevPos + 1, 0) to Count - 1 do
    if Find = Item[I] then
      begin
        Result := I;
        exit;
      end;
  Result := -1;
end;

function AObjectArray.PosNext(var AItem: TObject; const AClassType: TClass;
    const PrevPos: Integer): Integer;
var I : Integer;
begin
  for I := MaxInt(PrevPos + 1, 0) to Count - 1 do
    begin
      AItem := GetItem(I);
      if AItem.InheritsFrom(AClassType) then
        begin
          Result := I;
          exit;
        end;
    end;
  AItem := nil;
  Result := -1;
end;

function AObjectArray.PosNext(var AItem: TObject; const AClassName: String;
    const PrevPos: Integer): Integer;
var I : Integer;
begin
  for I := MaxInt(PrevPos + 1, 0) to Count - 1 do
    begin
      AItem := GetItem(I);
      if Assigned(AItem) and AItem.ClassNameIs(AClassName) then
        begin
          Result := I;
          exit;
        end;
    end;
  AItem := nil;
  Result := -1;
end;

function AObjectArray.Find(const AClassType: TClass; const ACount: Integer): TObject;
var I, J : Integer;
begin
  Result := nil;
  I := -1;
  for J := 1 to ACount do
    begin
      I := PosNext(Result, AClassType, I);
      if I = -1 then
        break;
    end;
end;

function AObjectArray.Find(const AClassName: String; const ACount: Integer): TObject;
var I, J : Integer;
begin
  Result := nil;
  I := -1;
  for J := 1 to ACount do
    begin
      I := PosNext(Result, AClassName, I);
      if I = -1 then
        break;
    end;
end;

function AObjectArray.FindAll(const AClassType: TClass): ObjectArray;
var I : Integer;
    V : TObject;
begin
  SetLength(Result, 0);
  I := PosNext(V, AClassType);
  while I >= 0 do
    begin
      DynArrayAppend(Result, V);
      I := PosNext(V, AClassType, I);
    end;
end;

function AObjectArray.FindAll(const AClassName: String): ObjectArray;
var I : Integer;
    V : TObject;
begin
  SetLength(Result, 0);
  I := PosNext(V, AClassName);
  while I >= 0 do
    begin
      DynArrayAppend(Result, V);
      I := PosNext(V, AClassName, I);
    end;
end;

function AObjectArray.CountItems(const AClassType: TClass): Integer;
var I : Integer;
    V : TObject;
begin
  Result := 0;
  I := PosNext(V, AClassType);
  while I >= 0 do
    begin
      Inc(Result);
      I := PosNext(V, AClassType, I);
    end;
end;

function AObjectArray.CountItems(const AClassName: String): Integer;
var I : Integer;
    V : TObject;
begin
  Result := 0;
  I := PosNext(V, AClassName);
  while I >= 0 do
    begin
      Inc(Result);
      I := PosNext(V, AClassName, I);
    end;
end;

function AObjectArray.CompareItems(const Idx1, Idx2: Integer): TCompareResult;
var A, B : TObject;
begin
  A := Item[Idx1];
  B := Item[Idx2];
  if A = B then
    Result := crEqual else
    Result := crUndefined;
end;

function AObjectArray.DeleteValue(const Value: TObject): Boolean;
var I : Integer;
begin
  I := PosNext(Value, -1);
  Result := I >= 0;
  if Result then
    Delete(I, 1);
end;

function AObjectArray.DeleteAll(const Value: TObject): Integer;
begin
  Result := 0;
  while DeleteValue(Value) do
    Inc(Result);
end;

function AObjectArray.ReleaseValue(const Value: TObject): Boolean;
var I : Integer;
begin
  I := PosNext(Value, -1);
  Result := I >= 0;
  if Result then
    ReleaseItem(I);
end;

function AObjectArray.RemoveItem(const Idx: Integer): TObject;
begin
  Result := ReleaseItem(Idx);
  Delete(Idx, 1);
end;

function AObjectArray.RemoveValue(const Value: TObject): Boolean;
var I : Integer;
begin
  I := PosNext(Value, -1);
  Result := I >= 0;
  if Result then
    RemoveItem(I);
end;



{                                                                              }
{ ABitArray                                                                    }
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

function ABitArray.GetRangeL(const Idx: Integer): Word32;
var I : Integer;
begin
  Result := 0;
  for I := 0 to 31 do
    if Bit[Idx + I] then
      Result := Result or BitMaskTable32[I];
end;

procedure ABitArray.SetRangeL(const Idx: Integer; const Value: Word32);
var
  I : Integer;
  C : Word32;
begin
  C := 1;
  for I := Idx to Idx + 31 do
    begin
      Bit[I] := Value and C <> 0;
      C := C shl 1;
    end;
end;

procedure ABitArray.Fill(const Idx, ACount: Integer; const Value: Boolean);
var
  I : Integer;
begin
  for I := Idx to Idx + ACount - 1 do
    Bit[I] := Value;
end;

function ABitArray.IsRange(const LoIdx, HiIdx: Integer; const Value: Boolean): Boolean;
var
  I : Integer;
begin
  for I := LoIdx to HiIdx do
    if Bit[I] <> Value then
      begin
        Result := False;
        exit;
      end;
  Result := True;
end;

procedure ABitArray.Assign(const Source: TObject);
var
  I, L : Integer;
begin
  if Source is ABitArray then
    begin
      L := AArray(Source).Count;
      Count := L;
      for I := 0 to L - 1 do
        Bit[I] := ABitArray(Source)[I];
    end
  else
    inherited Assign(Source);
end;

function ABitArray.IsEqual(const V: TObject): Boolean;
var
  I, L : Integer;
begin
  if V is ABitArray then
    begin
      L := AArray(V).Count;
      if Count <> L then
        begin
          Result := False;
          exit;
        end;
      for I := 0 to L - 1 do
        if Bit[I] <> ABitArray(V)[I] then
          begin
            Result := False;
            exit;
          end;
      Result := True;
    end
  else
    Result := inherited IsEqual(V);
end;

procedure ABitArray.ExchangeItems(const Idx1, Idx2: Integer);
var
  I : Boolean;
begin
  I := Bit[Idx1];
  Bit[Idx1] := Bit[Idx2];
  Bit[Idx2] := I;
end;

function ABitArray.AppendItem(const Value: Boolean): Integer;
begin
  Result := Count;
  Count := Result + 1;
  Bit[Result] := Value;
end;

function ABitArray.CompareItems(const Idx1, Idx2: Integer): TCompareResult;
begin
  Result := flcUtils.Compare(Bit[Idx1], Bit[Idx2]);
end;

procedure ABitArray.Invert;
var
  I : Integer;
begin
  for I := 0 to Count - 1 do
    Bit[I] := not Bit[I];
end;

function ABitArray.Find(const Value: Boolean; const Start: Integer): Integer;
var
  I, C : Integer;
begin
  if Start < 0 then
    I := 0
  else
    I := Start;
  C := Count;
  while I < C do
    if Bit[I] = Value then
      begin
        Result := I;
        exit;
      end
    else
      Inc(I);
  Result := -1;
end;

function ABitArray.FindRange(const Value: Boolean; const Start: Integer;
    const ACount: Integer): Integer;
var
  I, C, F : Integer;
begin
  if ACount <= 0 then
    begin
      Result := -1;
      exit;
    end;
  if Start < 0 then
    I := 0
  else
    I := Start;
  C := self.Count;
  F := 0;
  while I + F < C do
    if Bit[I + F] = Value then
      begin
        Inc(F);
        if F = ACount then
          begin
            Result := I;
            exit;
          end;
      end
    else
      begin
        Inc(I, F + 1);
        F := 0;
      end;
  Result := -1;
end;

procedure ABitArray.Delete(const Idx: Integer; const ACount: Integer);
var
  I, C : Integer;
begin
  C := GetCount;
  {$IFOPT R+}
  if (Idx < 0) or (Idx + ACount > C) then
    RaiseIndexError(Idx);
  {$ENDIF}
  for I := Idx + ACount to C - 1 do
    SetBit(I - ACount, GetBit(I));
  SetCount(C - ACount);
end;

procedure ABitArray.Insert(const Idx: Integer; const ACount: Integer);
var
  I, C : Integer;
begin
  C := GetCount;
  {$IFOPT R+}
  if (Idx < 0) or (Idx > C) then
    RaiseIndexError(Idx);
  {$ENDIF}
  SetCount(C + ACount);
  for I := Idx to C - 1 do
    SetBit(I + ACount, GetBit(I));
  Fill(Idx, Idx + ACount - 1, False);
end;

function ABitArray.DuplicateRange(const LoIdx, HiIdx: Integer): AArray;
var
  I, C : Integer;
begin
  C := GetCount;
  {$IFOPT R+}
  if (LoIdx < 0) or (LoIdx > HiIdx) or (HiIdx >= C) then
    RaiseIndexError(HiIdx);
  {$ENDIF}
  Result := ABitArray(CreateInstance);
  C := HiIdx - LoIdx + 1;
  Result.Count := C;
  for I := 0 to C - 1 do
    ABitArray(Result)[I] := GetBit(LoIdx + I);
end;

function ABitArray.AppendArray(const V: AArray): Integer;
var
  I, C : Integer;
begin
  if V is ABitArray then
    begin
      Result := Count;
      C := ABitArray(V).Count;
      if C = 0 then
        exit;
      SetCount(Result + C);
      for I := 0 to C - 1 do
        SetBit(Result + I, ABitArray(V).GetBit(I));
    end
  else
    raise EBitArray.CreateFmt('%s can not append %s', [ClassName, ObjectClassName(V)]);
end;



{                                                                              }
{ ARRAY IMPLEMENTATIONS                                                        }
{                                                                              }
{                                                                              }
{%DEFINE AArrayDynArrayImpl}
{                                                                              }
{-T%1%Array                                                                    }
{                                                                              }
function T%1%Array.GetItem(const Idx: Integer): %3%;
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FCount) then
    RaiseIndexError(Idx);
  {$ENDIF}
  Result := FData[Idx];
end;

procedure T%1%Array.SetItem(const Idx: Integer; const Value: %3%);
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FCount) then
    RaiseIndexError(Idx);
  {$ENDIF}
  FData[Idx] := Value;
end;

procedure T%1%Array.ExchangeItems(const Idx1, Idx2: Integer);
var I : %3%;
begin
  {$IFOPT R+}
  if (Idx1 < 0) or (Idx1 >= FCount) then
    RaiseIndexError(Idx1);
  if (Idx2 < 0) or (Idx2 >= FCount) then
    RaiseIndexError(Idx2);
  {$ENDIF}
  I := FData[Idx1];
  FData[Idx1] := FData[Idx2];
  FData[Idx2] := I;
end;

function T%1%Array.GetCount: Integer;
begin
  Result := FCount;
end;

{ Memory allocation strategy to reduce memory copies:                          }
{   * For first allocation: allocate the exact size.                           }
{   * For change to < 16: allocate 16 entries.                                 }
{   * For growing to >= 16: pre-allocate 1/8th of NewCount.                    }
{   * For shrinking blocks: shrink actual allocation when Count is less        }
{     than half of the allocated size.                                         }
procedure T%1%Array.SetCount(const NewCount: Integer);
var L, N : Integer;
begin
  N := NewCount;
  if FCount = N then
    exit;
  FCount := N;
  L := FCapacity;
  if L > 0 then
    if N < 16 then // pre-allocate first 16 entries
      N := 16 else
    if N > L then
      N := N + N shr 3 else // pre-allocate 1/8th extra if growing
    if N > L shr 1 then // only reduce capacity if size is at least half
      exit;
  if N <> L then
    begin
      SetLength{%IF 2}AndZero{%ENDIF}(FData, N);
      FCapacity := N;
    end;
end;

function T%1%Array.AppendItem(const Value: %3%): Integer;
begin
  Result := FCount;
  if Result >= FCapacity then
    SetCount(Result + 1)
  else
    FCount := Result + 1;
  FData[Result] := Value;
end;

procedure T%1%Array.Delete(const Idx: Integer; const ACount: Integer = 1);
var N : Integer;
begin
  N := DynArrayRemove%4%(FData, Idx, ACount);
  Dec(FCapacity, N);
  Dec(FCount, N);
end;

procedure T%1%Array.Insert(const Idx: Integer; const ACount: Integer = 1);
var I : Integer;
begin
  I := DynArrayInsert%4%(FData, Idx, ACount);
  if I >= 0 then
    begin
      Inc(FCapacity, ACount);
      Inc(FCount, ACount);
    end;
end;

function T%1%Array.GetRange(const LoIdx, HiIdx: Integer): %1%Array;
var L, H : Integer;
begin
  L := MaxInt(0, LoIdx);
  H := MinInt(HiIdx, FCount);
  if H >= L then
    Result := Copy(FData, L, H - L + 1) else
    Result := nil;
end;

procedure T%1%Array.SetRange(const LoIdx, HiIdx: Integer; const V: %1%Array);
var L, H, C : Integer;
begin
  L := MaxInt(0, LoIdx);
  H := MinInt(HiIdx, FCount);
  C := MaxInt(MinInt(Length(V), H - L + 1), 0);
  if C > 0 then
    Move(V[0], FData[L], C * Sizeof(%3%));
end;

constructor T%1%Array.Create(const V: %1%Array);
begin
  inherited Create;
  SetData(V);
end;

procedure T%1%Array.SetData(const AData: %1%Array);
begin
  FData := AData;
  FCount := Length(FData);
  FCapacity := FCount;
end;

function T%1%Array.DuplicateRange(const LoIdx, HiIdx: Integer): AArray;
var L, H, C : Integer;
begin
  L := MaxInt(0, LoIdx);
  H := MinInt(HiIdx, FCount);
  C := MaxInt(0, H - L + 1);
  Result := CreateInstance as T%1%Array;
  T%1%Array(Result).FCount := C;
  if C > 0 then
    T%1%Array(Result).FData := Copy(FData, L, C);
end;

procedure T%1%Array.Assign(const V: %1%Array);
begin
  FData := Copy(V);
  FCount := Length(FData);
  FCapacity := FCount;
end;

procedure T%1%Array.Assign(const V: Array of %3%);
begin
  FData := As%1%Array(V);
  FCount := Length(FData);
  FCapacity := FCount;
end;

procedure T%1%Array.Assign(const Source: TObject);
begin
  if Source is T%1%Array then
    begin
      FCount := T%1%Array(Source).FCount;
      FData := Copy(T%1%Array(Source).FData, 0, FCount);
    end
  else
    inherited Assign(Source);
end;


{%ENDDEF}

{%TEMPLATE AArrayDynArrayImpl 'Int32'         'Z'  'Int32'         ''  }
{%TEMPLATE AArrayDynArrayImpl 'Int64'         'Z'  'Int64'         ''  }
{%TEMPLATE AArrayDynArrayImpl 'LongInt'       'Z'  'LongInt'       ''  }
{%TEMPLATE AArrayDynArrayImpl 'Word32'        'Z'  'Word32'        ''  }
{%TEMPLATE AArrayDynArrayImpl 'Word64'        'Z'  'Word64'        ''  }
{%TEMPLATE AArrayDynArrayImpl 'LongWord'      'Z'  'LongWord'      ''  }
{%TEMPLATE AArrayDynArrayImpl 'Single'        'Z'  'Single'        ''  }
{%TEMPLATE AArrayDynArrayImpl 'Double'        'Z'  'Double'        ''  }
{%TEMPLATE AArrayDynArrayImpl 'Extended'      'Z'  'Extended'      ''  }
{$IFDEF SupportAnsiString}
{%TEMPLATE AArrayDynArrayImpl 'AnsiString'    ''   'AnsiString'    'A' }
{$ENDIF}
{%TEMPLATE AArrayDynArrayImpl 'RawByteString' ''   'RawByteString' 'B' }
{%TEMPLATE AArrayDynArrayImpl 'UnicodeString' ''   'UnicodeString' 'U' }
{%TEMPLATE AArrayDynArrayImpl 'String'        ''   'String'        ''  }
{%TEMPLATE AArrayDynArrayImpl 'Pointer'       'Z'  'Pointer'       ''  }
{                                                                              }
{ TObjectArray                                                                 }
{                                                                              }
constructor TObjectArray.Create(const V: ObjectArray; const AIsItemOwner: Boolean);
begin
  inherited Create;
  FData := V;
  FIsItemOwner := AIsItemOwner;
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
  inherited Init;
  FIsItemOwner := False;
end;

procedure TObjectArray.FreeItems;
begin
  FreeObjectArray(FData);
  FData := nil;
  FCapacity := 0;
  FCount := 0;
end;

procedure TObjectArray.ReleaseItems;
begin
  FData := nil;
  FCapacity := 0;
  FCount := 0;
end;

function TObjectArray.GetIsItemOwner: Boolean;
begin
  Result := FIsItemOwner;
end;

procedure TObjectArray.SetIsItemOwner(const AIsItemOwner: Boolean);
begin
  FIsItemOwner := AIsItemOwner;
end;

function TObjectArray.GetCount: Integer;
begin
  Result := FCount;
end;

procedure TObjectArray.SetCount(const NewCount: Integer);
var L, N : Integer;
begin
  N := NewCount;
  if N = FCount then
    exit;
  if (N < FCount) and FIsItemOwner then
    FreeObjectArray(FData, N, FCount - 1);
  FCount := N;
  L := FCapacity;
  if L > 0 then
    if N < 16 then
      N := 16 else
    if N > L then
      N := N + N shr 3 else
    if N > L shr 1 then
      exit;
  if N <> L then
    begin
      SetLengthAndZero(FData, N);
      FCapacity := N;
    end;
end;

function TObjectArray.GetItem(const Idx: Integer): TObject;
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FCount) then
    RaiseIndexError(Idx);
  {$ENDIF}
  Result := FData[Idx];
end;

procedure TObjectArray.SetItem(const Idx: Integer; const Value: TObject);
var P : ^TObject;
    V : TObject;
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FCount) then
    RaiseIndexError(Idx);
  {$ENDIF}
  P := Pointer(FData);
  Inc(P, Idx);
  if FIsItemOwner then
    begin
      V := P^;
      if V = Value then
        exit;
      V.Free;
    end;
  P^ := Value;
end;

function TObjectArray.AppendItem(const Value: TObject): Integer;
begin
  Result := FCount;
  if Result >= FCapacity then
    SetCount(Result + 1)
  else
    FCount := Result + 1;
  FData[Result] := Value;
end;

function TObjectArray.ReleaseItem(const Idx: Integer): TObject;
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FCount) then
    RaiseIndexError(Idx);
  {$ENDIF}
  Result := FData[Idx];
  if Assigned(Result) and FIsItemOwner then
    FData[Idx] := nil;
end;

function TObjectArray.GetRange(const LoIdx, HiIdx: Integer): ObjectArray;
begin
  Result := Copy(FData, LoIdx, MinInt(HiIdx, FCount - 1) - LoIdx + 1);
end;

procedure TObjectArray.SetData(const AData: ObjectArray);
begin
  if FIsItemOwner then
    FreeItems;
  FData := AData;
  FCount := Length(FData);
  FCapacity := FCount;
end;

function TObjectArray.DuplicateRange(const LoIdx, HiIdx: Integer): AArray;
var I : Integer;
    V : TObject;
begin
  Result := CreateInstance as TObjectArray;
  for I := LoIdx to MinInt(HiIdx, FCount - 1) do
    begin
      V := FData[I];
      if V is AType then
        V := AType(V).Duplicate;
      TObjectArray(Result).AppendItem(V);
    end;
end;

procedure TObjectArray.Delete(const Idx: Integer; const ACount: Integer = 1);
var N : Integer;
begin
  N := DynArrayRemove(FData, Idx, ACount, FIsItemOwner);
  Dec(FCapacity, N);
  Dec(FCount, N);
end;

procedure TObjectArray.Insert(const Idx: Integer; const ACount: Integer = 1);
var I : Integer;
begin
  I := DynArrayInsert(FData, Idx, ACount);
  if I >= 0 then
    begin
      Inc(FCapacity, ACount);
      Inc(FCount, ACount);
    end;
end;



{%TEMPLATE AArrayDynArrayImpl 'Interface'  ''  'IInterface' ''  }
{                                                                              }
{ TBitArray                                                                    }
{                                                                              }
function Word32IsBitSet(const A: Word32; const B: Integer): Boolean;
begin
  if (B < 0) or (B > 31) then
    Result := False
  else
    Result := (A and (1 shl B) <> 0);
end;

function Word32SetBitF(const A: Word32; const B: Integer): Word32;
begin
  if (B < 0) or (B > 31) then
    Result := A
  else
    Result := A or (1 shl B);
end;

function Word32ClearBitF(const A: Word32; const B: Integer): Word32;
begin
  if (B < 0) or (B > 31) then
    Result := A
  else
    Result := A and not (1 shl B);
end;

function LowBitMask32(const HighBitIndex: Word32): Word32;
begin
  if HighBitIndex >= 32 then
    Result := 0
  else
    Result := BitMaskTable32[HighBitIndex] - 1;
end;

function HighBitMask32(const LowBitIndex: Word32): Word32;
begin
  if LowBitIndex >= 32 then
    Result := 0
  else
    Result := not BitMaskTable32[LowBitIndex] + 1;
end;

function RangeBitMask32(const LowBitIndex, HighBitIndex: Word32): Word32;
begin
  if (LowBitIndex >= 32) and (HighBitIndex >= 32) then
    begin
      Result := 0;
      exit;
    end;
  Result := $FFFFFFFF;
  if LowBitIndex > 0 then
    Result := Result xor (BitMaskTable32[LowBitIndex] - 1);
  if HighBitIndex < 31 then
    Result := Result xor (not BitMaskTable32[HighBitIndex + 1] + 1);
end;

const
  TrueWord32  : Word32 = $FFFFFFFF;
  FalseWord32 : Word32 = $00000000;

function TBitArray.GetBit(const Idx: Integer): Boolean;
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FCount) then
    RaiseIndexError(Idx);
  {$ENDIF}
  Result := Word32IsBitSet(FData[Idx shr 5], Idx and 31);
end;

procedure TBitArray.SetBit(const Idx: Integer; const Value: Boolean);
var
  L : PWord32;
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FCount) then
    RaiseIndexError(Idx);
  {$ENDIF}
  L := @FData[Idx shr 5];
  if Value then
    L^ := Word32SetBitF(L^, Idx and 31)
  else
    L^ := Word32ClearBitF(L^, Idx and 31);
end;

function TBitArray.GetCount: Integer;
begin
  Result := FCount;
end;

procedure TBitArray.SetCount(const NewCount: Integer);
begin
  if NewCount = FCount then
    exit;
  SetLengthAndZero(FData, (NewCount + 31) div 32);
  FCount := NewCount;
end;

function TBitArray.GetRangeL(const Idx: Integer): Word32;
var
  F : Byte;
  I : Integer;
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FCount) then
    RaiseIndexError(Idx);
  {$ENDIF}
  F := Idx and 31;
  I := Idx shr 5;
  if F = 0 then
    Result := FData[I]
  else
    begin
      Result := FData[I] shr F;
      if I + 1 < Length(FData) then
        Result := Result or (FData[I + 1] shl (32 - F));
    end;
end;

procedure TBitArray.SetRangeL(const Idx: Integer; const Value: Word32);
var
  F : Byte;
  I : Integer;
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FCount) then
    RaiseIndexError(Idx);
  {$ENDIF}
  F := Idx and 31;
  I := Idx shr 5;
  if F = 0 then
    FData[I] := Value
  else
    begin
      FData[I] := (FData[I] and LowBitMask32(F))
               or (Value shl F);
      if I + 1 < Length(FData) then
        FData[I + 1] := (FData[I + 1] and HighBitMask32(F))
                     or (Value shr (32 - F));
    end;
end;

function TBitArray.IsRange(const LoIdx, HiIdx: Integer; const Value: Boolean): Boolean;
var B, I   : Word32;
    IL, IH : Integer;
begin
  {$IFOPT R+}
  if (LoIdx < 0) or (LoIdx > HiIdx) or (HiIdx >= FCount) then
    RaiseIndexError(HiIdx);
  {$ENDIF}
  // Check bits in FData[IL]
  IL := LoIdx shr 5;
  IH := HiIdx shr 5;
  B := HighBitMask32(LoIdx and 31);
  I := FData[IL];
  if Value then
    Result := I or B = I
  else
    Result := I and not B = I;
  if not Result or (IL = IH) then
    exit;
  // Check bits in FData[IH]
  B := LowBitMask32(HiIdx and 31);
  I := FData[IH];
  if Value then
    Result := I or B = I
  else
    Result := I and not B = I;
  if not Result or (IH = IL + 1) then
    exit;
  // Check bits in FStore[IL + 1..IR - 1]
  for I := IL + 1 to IH - 1 do
    if (Value and (FData[I] <> TrueWord32)) or
       (not Value and (FData[I] <> FalseWord32)) then
      begin
        Result := False;
        exit;
      end;
  Result := True;
end;

procedure TBitArray.Fill(const LoIdx, HiIdx: Integer; const Value: Boolean);
var
  B, I   : Word32;
  IL, IH : Integer;
begin
  {$IFOPT R+}
  if (LoIdx < 0) or (LoIdx > HiIdx) or (HiIdx >= FCount) then
    RaiseIndexError(HiIdx);
  {$ENDIF}
  IL := LoIdx shr 5;
  IH := HiIdx shr 5;
  // Set bits in FData[IL]
  if IH = IL then
    B := RangeBitMask32(LoIdx and 31, HiIdx and 31) else
    B := HighBitMask32(LoIdx and 31);
  I := FData[IL];
  if Value then
    FData[IL] := I or B
  else
    FData[IL] := I and not B;
  if IH = IL then
    exit;
  // Set bits in FData[IH]
  B := LowBitMask32(HiIdx and 31);
  I := FData[IH];
  if Value then
    FData[IH] := I or B
  else
    FData[IH] := I and not B;
  if IH = IL + 1 then
    exit;
  // Set bits in FData[IL + 1..IR - 1]
  for I := IL + 1 to IH - 1 do
    if Value then
      FData[I] := TrueWord32
    else
      FData[I] := FalseWord32;
end;



{                                                                              }
{ Hashed Array helper function                                                 }
{                                                                              }
const
  ArrayAverageHashChainSize = 4;

function ArrayRehashSize(const Count: Integer): Integer;
var L : Integer;
begin
  L := Count div ArrayAverageHashChainSize; // Number of slots
  if L <= 16 then                           // Rehash in powers of 16
    Result := 16 else
  if L <= 256 then
    Result := 256 else
  if L <= 4096 then
    Result := 4096 else
  if L <= 65536 then
    Result := 65536 else
  if L <= 1048576 then
    Result := 1048576 else
  if L <= 16777216 then
    Result := 16777216 else
    Result := 268435456;
end;



{$IFDEF SupportAnsiString}
{                                                                              }
{ THashedAnsiStringArray                                                       }
{                                                                              }
constructor THashedAnsiStringArray.Create(const ACaseSensitive: Boolean);
begin
  inherited Create(nil);
  FCaseSensitive := ACaseSensitive;
end;

procedure THashedAnsiStringArray.Init;
begin
  inherited Init;
  FCaseSensitive := True;
end;

procedure THashedAnsiStringArray.Assign(const Source: TObject);
begin
  if Source is THashedAnsiStringArray then
    begin
      // Assign array data
      inherited Assign(Source);
      // Assign hash lookup
      FLookup := Copy(THashedAnsiStringArray(Source).FLookup);
      FCaseSensitive := THashedAnsiStringArray(Source).FCaseSensitive;
    end
  else
    inherited Assign(Source);
end;

procedure THashedAnsiStringArray.Clear;
begin
  inherited Clear;
  Rehash;
end;

function THashedAnsiStringArray.LocateItemHash(const Value: AnsiString;
         var LookupList, LookupIdx: Integer): Boolean;
var I: Integer;
begin
  // Hash value
  LookupList := HashStrA(Value, 1, -1, FCaseSensitive, Length(FLookup));
  // Locate value in hash lookup
  for I := 0 to Length(FLookup[LookupList]) - 1 do
    if StrEqualA(Value, FData[FLookup[LookupList][I]], FCaseSensitive) then
      begin
        LookupIdx := I;
        Result := True;
        exit;
      end;
  // Not found
  LookupIdx := -1;
  Result := False;
end;

procedure THashedAnsiStringArray.Rehash;
var I, C, L : Integer;
begin
  C := FCount;
  L := ArrayRehashSize(C);
  FLookup := nil;
  SetLength(FLookup, L);
  for I := 0 to C - 1 do
    DynArrayAppend(FLookup[HashStrA(FData[I], 1, -1, FCaseSensitive, L)], I);
end;

procedure THashedAnsiStringArray.ExchangeItems(const Idx1, Idx2: Integer);
var L1, L2, I1, I2: Integer;
begin
  // Swap lookup
  if LocateItemHash(FData[Idx1], L1, I1) and
     LocateItemHash(FData[Idx2], L2, I2) then
    Swap(FLookup[L1][I1], FLookup[L2][I2]);
  // Swap array items
  inherited ExchangeItems(Idx1, Idx2);
end;

procedure THashedAnsiStringArray.Delete(const Idx: Integer; const ACount: Integer);
var I, L, V : Integer;
    P : PInteger;
begin
  // Delete lookup
  for I := MaxInt(0, Idx) to MinInt(FCount, Idx + ACount - 1) do
    if LocateItemHash(FData[I], L, V) then
      DynArrayRemove(FLookup[L], V, 1);
  // Delete array
  inherited Delete(Idx, ACount);
  // Reindex
  for I := 0 to Length(FLookup) - 1 do
    for V := 0 to Length(FLookup[I]) - 1 do
      begin
        P := @FLookup[I][V];
        if P^ >= Idx then
          Dec(P^);
      end;
end;

procedure THashedAnsiStringArray.Insert(const Idx: Integer; const ACount: Integer);
begin
  // Insert array
  inherited Insert(Idx, ACount);
  // Rebuild hash table
  Rehash;
end;

procedure THashedAnsiStringArray.SetData(const AData: AnsiStringArray);
begin
  inherited SetData(AData);
  Rehash;
end;

procedure THashedAnsiStringArray.SetItem(const Idx: Integer; const Value: AnsiString);
var S    : AnsiString;
    I, J : Integer;
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FCount) then
    RaiseIndexError(Idx);
  {$ENDIF}
  // Remove old hash
  S := FData[Idx];
  if LocateItemHash(S, I, J) then
    DynArrayRemove(FLookup[I], J, 1);
  // Set array value
  FData[Idx] := Value;
  // Add new hash
  DynArrayAppend(FLookup[HashStrA(Value, 1, -1, FCaseSensitive, Length(FLookup))], Idx);
end;

function THashedAnsiStringArray.AppendItem(const Value: AnsiString): Integer;
var L : Integer;
begin
  // add to array
  Result := Count;
  Count := Result + 1;
  FData[Result] := Value;
  // add lookup
  L := Length(FLookup);
  DynArrayAppend(FLookup[HashStrA(Value, 1, -1, FCaseSensitive, L)], Result);
  if (Result + 1) div ArrayAverageHashChainSize > L then
    Rehash;
end;

function THashedAnsiStringArray.PosNext(const Find: AnsiString; const PrevPos: Integer): Integer;
var I, J, F, L, P : Integer;
begin
  // locate first
  if not LocateItemHash(Find, I, J) then
    begin
      Result := -1;
      exit;
    end;
  if PrevPos < 0 then
    begin
      Result := FLookup[I][J];
      exit;
    end;
  // locate previous
  L := Length(FLookup[I]);
  P := -1;
  for F := J to L - 1 do
    if FLookup[I][F] = PrevPos then
      begin
        P := F;
        break;
      end;
  if P = -1 then
    begin
      Result := 1;
      exit;
    end;
  // locate next
  for F := P + 1 to L - 1 do
    begin
      Result := FLookup[I][F];
      if StrEqualA(Find, FData[Result], FCaseSensitive) then
        // found
        exit;
    end;
  // not found
  Result := 1;
end;
{$ENDIF}



{                                                                              }
{ THashedRawByteStringArray                                                    }
{                                                                              }
constructor THashedRawByteStringArray.Create(const ACaseSensitive: Boolean);
begin
  inherited Create(nil);
  FCaseSensitive := ACaseSensitive;
end;

procedure THashedRawByteStringArray.Init;
begin
  inherited Init;
  FCaseSensitive := True;
end;

procedure THashedRawByteStringArray.Assign(const Source: TObject);
begin
  if Source is THashedRawByteStringArray then
    begin
      // Assign array data
      inherited Assign(Source);
      // Assign hash lookup
      FLookup := Copy(THashedRawByteStringArray(Source).FLookup);
      FCaseSensitive := THashedRawByteStringArray(Source).FCaseSensitive;
    end
  else
    inherited Assign(Source);
end;

procedure THashedRawByteStringArray.Clear;
begin
  inherited Clear;
  Rehash;
end;

function THashedRawByteStringArray.LocateItemHash(const Value: RawByteString;
         var LookupList, LookupIdx: Integer): Boolean;
var I: Integer;
begin
  // Hash value
  LookupList := HashStrB(Value, 1, -1, FCaseSensitive, Length(FLookup));
  // Locate value in hash lookup
  for I := 0 to Length(FLookup[LookupList]) - 1 do
    if StrEqualB(Value, FData[FLookup[LookupList][I]], FCaseSensitive) then
      begin
        LookupIdx := I;
        Result := True;
        exit;
      end;
  // Not found
  LookupIdx := -1;
  Result := False;
end;

procedure THashedRawByteStringArray.Rehash;
var I, C, L : Integer;
begin
  C := FCount;
  L := ArrayRehashSize(C);
  FLookup := nil;
  SetLength(FLookup, L);
  for I := 0 to C - 1 do
    DynArrayAppend(FLookup[HashStrB(FData[I], 1, -1, FCaseSensitive, L)], I);
end;

procedure THashedRawByteStringArray.ExchangeItems(const Idx1, Idx2: Integer);
var L1, L2, I1, I2: Integer;
begin
  // Swap lookup
  if LocateItemHash(FData[Idx1], L1, I1) and
     LocateItemHash(FData[Idx2], L2, I2) then
    SwapInt(FLookup[L1][I1], FLookup[L2][I2]);
  // Swap array items
  inherited ExchangeItems(Idx1, Idx2);
end;

procedure THashedRawByteStringArray.Delete(const Idx: Integer; const ACount: Integer);
var I, L, V : Integer;
    P : PInteger;
begin
  // Delete lookup
  for I := MaxInt(0, Idx) to MinInt(FCount, Idx + ACount - 1) do
    if LocateItemHash(FData[I], L, V) then
      DynArrayRemove(FLookup[L], V, 1);
  // Delete array
  inherited Delete(Idx, ACount);
  // Reindex
  for I := 0 to Length(FLookup) - 1 do
    for V := 0 to Length(FLookup[I]) - 1 do
      begin
        P := @FLookup[I][V];
        if P^ >= Idx then
          Dec(P^);
      end;
end;

procedure THashedRawByteStringArray.Insert(const Idx: Integer; const ACount: Integer);
begin
  // Insert array
  inherited Insert(Idx, ACount);
  // Rebuild hash table
  Rehash;
end;

procedure THashedRawByteStringArray.SetData(const AData: RawByteStringArray);
begin
  inherited SetData(AData);
  Rehash;
end;

procedure THashedRawByteStringArray.SetItem(const Idx: Integer; const Value: RawByteString);
var S    : RawByteString;
    I, J : Integer;
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FCount) then
    RaiseIndexError(Idx);
  {$ENDIF}
  // Remove old hash
  S := FData[Idx];
  if LocateItemHash(S, I, J) then
    DynArrayRemove(FLookup[I], J, 1);
  // Set array value
  FData[Idx] := Value;
  // Add new hash
  DynArrayAppend(FLookup[HashStrB(Value, 1, -1, FCaseSensitive, Length(FLookup))], Idx);
end;

function THashedRawByteStringArray.AppendItem(const Value: RawByteString): Integer;
var L : Integer;
begin
  // add to array
  Result := Count;
  Count := Result + 1;
  FData[Result] := Value;
  // add lookup
  L := Length(FLookup);
  DynArrayAppend(FLookup[HashStrB(Value, 1, -1, FCaseSensitive, L)], Result);
  if (Result + 1) div ArrayAverageHashChainSize > L then
    Rehash;
end;

function THashedRawByteStringArray.PosNext(const Find: RawByteString; const PrevPos: Integer): Integer;
var I, J, F, L, P : Integer;
begin
  // locate first
  if not LocateItemHash(Find, I, J) then
    begin
      Result := -1;
      exit;
    end;
  if PrevPos < 0 then
    begin
      Result := FLookup[I][J];
      exit;
    end;
  // locate previous
  L := Length(FLookup[I]);
  P := -1;
  for F := J to L - 1 do
    if FLookup[I][F] = PrevPos then
      begin
        P := F;
        break;
      end;
  if P = -1 then
    begin
      Result := 1;
      exit;
    end;
  // locate next
  for F := P + 1 to L - 1 do
    begin
      Result := FLookup[I][F];
      if StrEqualB(Find, FData[Result], FCaseSensitive) then
        // found
        exit;
    end;
  // not found
  Result := 1;
end;



{                                                                              }
{ THashedUnicodeStringArray                                                    }
{                                                                              }
constructor THashedUnicodeStringArray.Create(const ACaseSensitive: Boolean);
begin
  inherited Create(nil);
  FCaseSensitive := ACaseSensitive;
end;

procedure THashedUnicodeStringArray.Init;
begin
  inherited Init;
  FCaseSensitive := True;
end;

procedure THashedUnicodeStringArray.Assign(const Source: TObject);
begin
  if Source is THashedUnicodeStringArray then
    begin
      // Assign array data
      inherited Assign(Source);
      // Assign hash lookup
      FLookup := Copy(THashedUnicodeStringArray(Source).FLookup);
      FCaseSensitive := THashedUnicodeStringArray(Source).FCaseSensitive;
    end
  else
    inherited Assign(Source);
end;

procedure THashedUnicodeStringArray.Clear;
begin
  inherited Clear;
  Rehash;
end;

function THashedUnicodeStringArray.LocateItemHash(const Value: UnicodeString;
         var LookupList, LookupIdx: Integer): Boolean;
var I: Integer;
begin
  // Hash value
  LookupList := HashStrU(Value, 1, -1, FCaseSensitive, Length(FLookup));
  // Locate value in hash lookup
  for I := 0 to Length(FLookup[LookupList]) - 1 do
    if StrEqualU(Value, FData[FLookup[LookupList][I]], FCaseSensitive) then
      begin
        LookupIdx := I;
        Result := True;
        exit;
      end;
  // Not found
  LookupIdx := -1;
  Result := False;
end;

procedure THashedUnicodeStringArray.Rehash;
var I, C, L : Integer;
begin
  C := FCount;
  L := ArrayRehashSize(C);
  FLookup := nil;
  SetLength(FLookup, L);
  for I := 0 to C - 1 do
    DynArrayAppend(FLookup[HashStrU(FData[I], 1, -1, FCaseSensitive, L)], I);
end;

procedure THashedUnicodeStringArray.ExchangeItems(const Idx1, Idx2: Integer);
var L1, L2, I1, I2: Integer;
begin
  // Swap lookup
  if LocateItemHash(FData[Idx1], L1, I1) and
     LocateItemHash(FData[Idx2], L2, I2) then
    SwapInt(FLookup[L1][I1], FLookup[L2][I2]);
  // Swap array items
  inherited ExchangeItems(Idx1, Idx2);
end;

procedure THashedUnicodeStringArray.Delete(const Idx: Integer; const ACount: Integer);
var I, L, V : Integer;
    P : PInteger;
begin
  // Delete lookup
  for I := MaxInt(0, Idx) to MinInt(FCount, Idx + ACount - 1) do
    if LocateItemHash(FData[I], L, V) then
      DynArrayRemove(FLookup[L], V, 1);
  // Delete array
  inherited Delete(Idx, ACount);
  // Reindex
  for I := 0 to Length(FLookup) - 1 do
    for V := 0 to Length(FLookup[I]) - 1 do
      begin
        P := @FLookup[I][V];
        if P^ >= Idx then
          Dec(P^);
      end;
end;

procedure THashedUnicodeStringArray.Insert(const Idx: Integer; const ACount: Integer);
begin
  // Insert array
  inherited Insert(Idx, ACount);
  // Rebuild hash table
  Rehash;
end;

procedure THashedUnicodeStringArray.SetData(const AData: UnicodeStringArray);
begin
  inherited SetData(AData);
  Rehash;
end;

procedure THashedUnicodeStringArray.SetItem(const Idx: Integer; const Value: UnicodeString);
var S    : UnicodeString;
    I, J : Integer;
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FCount) then
    RaiseIndexError(Idx);
  {$ENDIF}
  // Remove old hash
  S := FData[Idx];
  if LocateItemHash(S, I, J) then
    DynArrayRemove(FLookup[I], J, 1);
  // Set array value
  FData[Idx] := Value;
  // Add new hash
  DynArrayAppend(FLookup[HashStrU(Value, 1, -1, FCaseSensitive, Length(FLookup))], Idx);
end;

function THashedUnicodeStringArray.AppendItem(const Value: UnicodeString): Integer;
var L : Integer;
begin
  // add to array
  Result := Count;
  Count := Result + 1;
  FData[Result] := Value;
  // add lookup
  L := Length(FLookup);
  DynArrayAppend(FLookup[HashStrU(Value, 1, -1, FCaseSensitive, L)], Result);
  if (Result + 1) div ArrayAverageHashChainSize > L then
    Rehash;
end;

function THashedUnicodeStringArray.PosNext(const Find: UnicodeString; const PrevPos: Integer): Integer;
var I, J, F, L, P : Integer;
begin
  // locate first
  if not LocateItemHash(Find, I, J) then
    begin
      Result := -1;
      exit;
    end;
  if PrevPos < 0 then
    begin
      Result := FLookup[I][J];
      exit;
    end;
  // locate previous
  L := Length(FLookup[I]);
  P := -1;
  for F := J to L - 1 do
    if FLookup[I][F] = PrevPos then
      begin
        P := F;
        break;
      end;
  if P = -1 then
    begin
      Result := 1;
      exit;
    end;
  // locate next
  for F := P + 1 to L - 1 do
    begin
      Result := FLookup[I][F];
      if StrEqualU(Find, FData[Result], FCaseSensitive) then
        // found
        exit;
    end;
  // not found
  Result := 1;
end;



{                                                                              }
{ DICTIONARY BASE CLASSES                                                      }
{                                                                              }



{                                                                              }
{ ADictionaryBase                                                              }
{                                                                              }
function ADictionaryBase.GetItemStrByIndex(const Idx: Integer): String;
begin
  raise EDictionary.CreateFmt('Method %s.GetItemStrByIndex not implemented', [ClassName]);
end;

function ADictionaryBase.GetAsString: String;
var I, L : Integer;
begin
  L := Count - 1;
  for I := 0 to L do
    begin
      Result := Result + GetKeyStrByIndex(I) + ':' + GetItemStrByIndex(I);
      if I < L then
        Result := Result + ',';
    end;
end;



{$IFDEF SupportAnsiString}
{                                                                              }
{ ADictionaryA                                                                 }
{                                                                              }
procedure ADictionaryA.RaiseKeyNotFoundError(const Key: AnsiString);
begin
  raise EDictionary.CreateFmt('Key not found: %s', [Key]);
end;

procedure ADictionaryA.RaiseDuplicateKeyError(const Key: AnsiString);
begin
  raise EDictionary.CreateFmt('Duplicate key: %s', [Key]);
end;

function ADictionaryA.GetKeyStrByIndex(const Idx: Integer): String;
begin
  Result := ToStringA(GetKeyByIndex(Idx));
end;
{$ENDIF}



{                                                                              }
{ ADictionaryB                                                                 }
{                                                                              }
procedure ADictionaryB.RaiseKeyNotFoundError(const Key: RawByteString);
begin
  raise EDictionary.CreateFmt('Key not found: %s', [Key]);
end;

procedure ADictionaryB.RaiseDuplicateKeyError(const Key: RawByteString);
begin
  raise EDictionary.CreateFmt('Duplicate key: %s', [Key]);
end;

function ADictionaryB.GetKeyStrByIndex(const Idx: Integer): String;
begin
  Result := ToStringB(GetKeyByIndex(Idx));
end;



{                                                                              }
{ ADictionaryU                                                                 }
{                                                                              }
procedure ADictionaryU.RaiseKeyNotFoundError(const Key: UnicodeString);
begin
  raise EDictionary.CreateFmt('Key not found: %s', [Key]);
end;

procedure ADictionaryU.RaiseDuplicateKeyError(const Key: UnicodeString);
begin
  raise EDictionary.CreateFmt('Duplicate key: %s', [Key]);
end;

function ADictionaryU.GetKeyStrByIndex(const Idx: Integer): String;
begin
  Result := ToStringU(GetKeyByIndex(Idx));
end;



{                                                                              }
{ ADictionary                                                                  }
{                                                                              }
procedure ADictionary.RaiseKeyNotFoundError(const Key: String);
begin
  raise EDictionary.CreateFmt('Key not found: %s', [Key]);
end;

procedure ADictionary.RaiseDuplicateKeyError(const Key: String);
begin
  raise EDictionary.CreateFmt('Duplicate key: %s', [Key]);
end;

function ADictionary.GetKeyStrByIndex(const Idx: Integer): String;
begin
  Result := GetKeyByIndex(Idx);
end;



{%DEFINE ATypeDictionaryImpl}
{                                                                              }
{-A%1%Dictionary%9%                                                            }
{                                                                              }
function A%1%Dictionary%9%.GetItem(const Key: %3%): %2%;
begin
  if LocateItem(Key, Result) < 0 then
    RaiseKeyNotFoundError(Key);
end;
{%IF 4}
function A%1%Dictionary%9%.GetItemStrByIndex(const Idx: Integer): String;
begin
  Result := %4%(GetItemByIndex(Idx));
end;
{%ENDIF}
procedure A%1%Dictionary%9%.Assign(const Source: TObject);
var I : Integer;
begin
  if Source is A%1%Dictionary%9% then
    begin
      Clear;
      for I := 0 to A%1%Dictionary%9%(Source).Count - 1 do
        Add(A%1%Dictionary%9%(Source).GetKeyByIndex(I),
             A%1%Dictionary%9%(Source).GetItemByIndex(I));
    end else
    inherited Assign(Source);
end;

function A%1%Dictionary%9%.GetAsString: String;
var I, L : Integer;
begin
  L := Count - 1;
  for I := 0 to L do
    begin
      Result := Result + GetKeyStrByIndex(I) + ':' + GetItemStrByIndex(I);
      if I < L then
        Result := Result + ',';
    end;
end;
{%IF 7}
function A%1%Dictionary%9%.GetItemLength(const Key: %3%): Integer;
begin
  Result := Length(GetItem(Key));
end;

function A%1%Dictionary%9%.GetTotalLength: Int64;
var I : Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    Inc(Result, Length(GetItemByIndex(I)));
end;
{%ENDIF}{%IF 5}
procedure A%1%Dictionary%9%.Clear;
begin
  if IsItemOwner then
    FreeItems else
    ReleaseItems;
end;
{%ENDIF}

{%ENDDEF}
{$IFDEF SupportAnsiString}
{%TEMPLATE ATypeDictionaryImpl 'LongInt'       'LongInt'       'AnsiString'    'IntToStr'        ''  '0'    ''  ''  'A' }
{$ENDIF}
{%TEMPLATE ATypeDictionaryImpl 'LongInt'       'LongInt'       'RawByteString' 'IntToStr'        ''  '0'    ''  ''  'B' }
{%TEMPLATE ATypeDictionaryImpl 'LongInt'       'LongInt'       'UnicodeString' 'IntToStr'        ''  '0'    ''  ''  'U' }
{%TEMPLATE ATypeDictionaryImpl 'LongInt'       'LongInt'       'String'        'IntToStr'        ''  '0'    ''  ''  ''  }
{$IFDEF SupportAnsiString}
{%TEMPLATE ATypeDictionaryImpl 'LongWord'      'LongWord'      'AnsiString'    'IntToStr'        ''  '0'    ''  ''  'A' }
{$ENDIF}
{%TEMPLATE ATypeDictionaryImpl 'LongWord'      'LongWord'      'RawByteString' 'IntToStr'        ''  '0'    ''  ''  'B' }
{%TEMPLATE ATypeDictionaryImpl 'LongWord'      'LongWord'      'UnicodeString' 'IntToStr'        ''  '0'    ''  ''  'U' }
{%TEMPLATE ATypeDictionaryImpl 'LongWord'      'LongWord'      'String'        'IntToStr'        ''  '0'    ''  ''  ''  }
{$IFDEF SupportAnsiString}
{%TEMPLATE ATypeDictionaryImpl 'Int64'         'Int64'         'AnsiString'    'IntToStr'        ''  '0'    ''  ''  'A' }
{$ENDIF}
{%TEMPLATE ATypeDictionaryImpl 'Int64'         'Int64'         'RawByteString' 'IntToStr'        ''  '0'    ''  ''  'B' }
{%TEMPLATE ATypeDictionaryImpl 'Int64'         'Int64'         'UnicodeString' 'IntToStr'        ''  '0'    ''  ''  'U' }
{%TEMPLATE ATypeDictionaryImpl 'Int64'         'Int64'         'String'        'IntToStr'        ''  '0'    ''  ''  ''  }
{$IFDEF SupportAnsiString}
{%TEMPLATE ATypeDictionaryImpl 'Single'        'Single'        'AnsiString'    'FloatToStr'      ''  '0.0'  ''  ''  'A' }
{$ENDIF}
{%TEMPLATE ATypeDictionaryImpl 'Single'        'Single'        'RawByteString' 'FloatToStr'      ''  '0.0'  ''  ''  'B' }
{%TEMPLATE ATypeDictionaryImpl 'Single'        'Single'        'UnicodeString' 'FloatToStr'      ''  '0.0'  ''  ''  'U' }
{%TEMPLATE ATypeDictionaryImpl 'Single'        'Single'        'String'        'FloatToStr'      ''  '0.0'  ''  ''  ''  }
{$IFDEF SupportAnsiString}
{%TEMPLATE ATypeDictionaryImpl 'Double'        'Double'        'AnsiString'    'FloatToStr'      ''  '0.0'  ''  ''  'A' }
{$ENDIF}
{%TEMPLATE ATypeDictionaryImpl 'Double'        'Double'        'RawByteString' 'FloatToStr'      ''  '0.0'  ''  ''  'B' }
{%TEMPLATE ATypeDictionaryImpl 'Double'        'Double'        'UnicodeString' 'FloatToStr'      ''  '0.0'  ''  ''  'U' }
{%TEMPLATE ATypeDictionaryImpl 'Double'        'Double'        'String'        'FloatToStr'      ''  '0.0'  ''  ''  ''  }
{$IFDEF SupportAnsiString}
{%TEMPLATE ATypeDictionaryImpl 'Extended'      'Extended'      'AnsiString'    'FloatToStr'      ''  '0.0'  ''  ''  'A' }
{$ENDIF}
{%TEMPLATE ATypeDictionaryImpl 'Extended'      'Extended'      'RawByteString' 'FloatToStr'      ''  '0.0'  ''  ''  'B' }
{%TEMPLATE ATypeDictionaryImpl 'Extended'      'Extended'      'UnicodeString' 'FloatToStr'      ''  '0.0'  ''  ''  'U' }
{%TEMPLATE ATypeDictionaryImpl 'Extended'      'Extended'      'String'        'FloatToStr'      ''  '0.0'  ''  ''  ''  }
{$IFDEF SupportAnsiString}
{%TEMPLATE ATypeDictionaryImpl 'AnsiString'    'AnsiString'    'AnsiString'    'ToStringA'       ''  '''''' 'L' ''  'A' }
{%TEMPLATE ATypeDictionaryImpl 'AnsiString'    'AnsiString'    'UnicodeString' 'ToStringA'       ''  '''''' 'L' ''  'U' }
{%TEMPLATE ATypeDictionaryImpl 'AnsiString'    'AnsiString'    'String'        'ToStringA'       ''  '''''' 'L' ''  ''  }
{$ENDIF}
{$IFDEF SupportAnsiString}
{%TEMPLATE ATypeDictionaryImpl 'RawByteString' 'RawByteString' 'AnsiString'    'ToStringB'       ''  '''''' 'L' ''  'A' }
{$ENDIF}
{%TEMPLATE ATypeDictionaryImpl 'RawByteString' 'RawByteString' 'RawByteString' 'ToStringB'       ''  '''''' 'L' ''  'B' }
{%TEMPLATE ATypeDictionaryImpl 'RawByteString' 'RawByteString' 'UnicodeString' 'ToStringB'       ''  '''''' 'L' ''  'U' }
{%TEMPLATE ATypeDictionaryImpl 'RawByteString' 'RawByteString' 'String'        'ToStringB'       ''  '''''' 'L' ''  ''  }
{$IFDEF SupportAnsiString}
{%TEMPLATE ATypeDictionaryImpl 'UnicodeString' 'UnicodeString' 'AnsiString'    'ToStringU'       ''  '''''' 'L' ''  'A' }
{$ENDIF}
{%TEMPLATE ATypeDictionaryImpl 'UnicodeString' 'UnicodeString' 'UnicodeString' 'ToStringU'       ''  '''''' 'L' ''  'U' }
{%TEMPLATE ATypeDictionaryImpl 'UnicodeString' 'UnicodeString' 'String'        'ToStringU'       ''  '''''' 'L' ''  ''  }
{$IFDEF SupportAnsiString}
{%TEMPLATE ATypeDictionaryImpl 'String'        'String'        'AnsiString'    ''                ''  '''''' 'L' ''  'A' }
{$ENDIF}
{%TEMPLATE ATypeDictionaryImpl 'String'        'String'        'UnicodeString' ''                ''  '''''' 'L' ''  'U' }
{%TEMPLATE ATypeDictionaryImpl 'String'        'String'        'String'        ''                ''  '''''' 'L' ''  ''  }
{$IFDEF SupportAnsiString}
{%TEMPLATE ATypeDictionaryImpl 'Pointer'       'Pointer'       'AnsiString'    'PointerToStr'    ''  ''     ''  ''  'A' }
{$ENDIF}
{%TEMPLATE ATypeDictionaryImpl 'Pointer'       'Pointer'       'RawByteString' 'PointerToStr'    ''  ''     ''  ''  'B' }
{%TEMPLATE ATypeDictionaryImpl 'Pointer'       'Pointer'       'UnicodeString' 'PointerToStr'    ''  ''     ''  ''  'U' }
{%TEMPLATE ATypeDictionaryImpl 'Pointer'       'Pointer'       'String'        'PointerToStr'    ''  ''     ''  ''  ''  }
{$IFDEF SupportAnsiString}
{%TEMPLATE ATypeDictionaryImpl 'Interface'     'IInterface'    'AnsiString'    ''                ''  ''     ''  ''  'A' }
{$ENDIF}
{%TEMPLATE ATypeDictionaryImpl 'Interface'     'IInterface'    'UnicodeString' ''                ''  ''     ''  ''  'U' }
{%TEMPLATE ATypeDictionaryImpl 'Interface'     'IInterface'    'String'        ''                ''  ''     ''  ''  ''  }
{$IFDEF SupportAnsiString}
{%TEMPLATE ATypeDictionaryImpl 'Object'        'TObject'       'AnsiString'    'ObjectClassName' 'O' ''     ''  ''  'A' }
{$ENDIF}
{%TEMPLATE ATypeDictionaryImpl 'Object'        'TObject'       'RawByteString' 'ObjectClassName' 'O' ''     ''  ''  'B' }
{%TEMPLATE ATypeDictionaryImpl 'Object'        'TObject'       'UnicodeString' 'ObjectClassName' 'O' ''     ''  ''  'U' }
{%TEMPLATE ATypeDictionaryImpl 'Object'        'TObject'       'String'        'ObjectClassName' 'O' ''     ''  ''  ''  }
{                                                                              }
{ DICTIONARY IMPLEMENTATIONS                                                   }
{                                                                              }



{ Dictionary helper functions                                                  }
function DictionaryRehashSize(const Count: Integer): Integer;
var L : Integer;
begin
  L := Count div DictionaryAverageHashChainSize; // Number of "slots"
  if L <= $10 then                               // Rehash in powers of 16
    Result := $10 else
  if L <= $100 then
    Result := $100 else
  if L <= $1000 then
    Result := $1000 else
  if L <= $10000 then
    Result := $10000 else
  if L <= $100000 then
    Result := $100000 else
  if L <= $1000000 then
    Result := $1000000 else
    Result := $10000000;
end;

{%DEFINE ObjectDictionaryConstructor}
constructor TGeneralObjectDictionary%2%.CreateEx(
    const AKeys: T%3%Array;
    const AValues: TObjectArray; const AIsItemOwner: Boolean;
    const AKeysCaseSensitive: Boolean; const AAddOnSet: Boolean;
    const ADuplicatesAction: TDictionaryDuplicatesAction);
var L : Integer;
begin
  inherited Create;
  if Assigned(AKeys) then
    begin
      FKeys := AKeys;
      L := FKeys.Count;
    end
  else
    begin
      FKeys := T%3%Array.Create;
      L := 0;
    end;
  if Assigned(AValues) then
    FValues := AValues
  else
    FValues := TObjectArray.Create;
  FValues.Count := L;
  FAddOnSet := AAddOnSet;
  FValues.IsItemOwner := AIsItemOwner;
  FCaseSensitive := AKeysCaseSensitive;
  FDuplicatesAction := ADuplicatesAction;
  if L > 0 then
    Rehash;
end;

constructor TObjectDictionary%2%.CreateEx(
    const AKeys: T%3%Array;
    const AValues: TObjectArray; const AIsItemOwner: Boolean;
    const AKeysCaseSensitive: Boolean; const AAddOnSet: Boolean;
    const ADuplicatesAction: TDictionaryDuplicatesAction);
begin
  inherited CreateEx(AKeys, AValues, AIsItemOwner, AKeysCaseSensitive, AAddOnSet,
      ADuplicatesAction);
end;
{%ENDDEF}
{%DEFINE TTypeDictionaryConstructor}
constructor TGeneral%1%Dictionary%2%.CreateEx(
    const AKeys: T%3%Array;
    const AValues: T%1%Array; const AKeysCaseSensitive: Boolean;
    const AAddOnSet: Boolean;
    const ADuplicatesAction: TDictionaryDuplicatesAction);
var L : Integer;
begin
  inherited Create;
  if Assigned(AKeys) then
    begin
      FKeys := AKeys;
      L := FKeys.Count;
    end
  else
    begin
      FKeys := T%3%Array.Create;
      L := 0;
    end;
  if Assigned(AValues) then
    FValues := AValues
  else
    FValues := T%1%Array.Create;
  FCaseSensitive := AKeysCaseSensitive;
  FValues.Count := L;
  FAddOnSet := AAddOnSet;
  FDuplicatesAction := ADuplicatesAction;
  if L > 0 then
    Rehash;
end;

constructor T%1%Dictionary%2%.CreateEx(
    const AKeys: T%3%Array;
    const AValues: T%1%Array; const AKeysCaseSensitive: Boolean;
    const AAddOnSet: Boolean;
    const ADuplicatesAction: TDictionaryDuplicatesAction);
begin
  inherited CreateEx(AKeys, AValues, AKeysCaseSensitive, AAddOnSet,
      ADuplicatesAction);
end;
{%ENDDEF}
{%DEFINE ADictionaryByArrayImpl}
{                                                                              }
{-TGeneral%1%Dictionary%6%                                                     }
{                                                                              }
constructor TGeneral%1%Dictionary%6%.Create;
begin
  inherited Create;
  FCaseSensitive := True;
  FDuplicatesAction := ddAccept;
  FAddOnSet := True;
  FKeys := T%7%Array.Create;
  FValues := T%1%Array.Create;
end;

{%TEMPLATE %5% '%1%' '%6%' '%7%' }
destructor TGeneral%1%Dictionary%6%.Destroy;
begin
  FreeAndNil(FValues);
  FreeAndNil(FKeys);
  inherited Destroy;
end;

function TGeneral%1%Dictionary%6%.GetKeysCaseSensitive: Boolean;
begin
  Result := FCaseSensitive;
end;

function TGeneral%1%Dictionary%6%.GetAddOnSet: Boolean;
begin
  Result := FAddOnSet;
end;

procedure TGeneral%1%Dictionary%6%.SetAddOnSet(const AAddOnSet: Boolean);
begin
  FAddOnSet := AAddOnSet;
end;

function TGeneral%1%Dictionary%6%.GetHashTableSize: Integer;
begin
  Result := Length(FLookup);
end;{%IF 4}

function TGeneral%1%Dictionary%6%.GetIsItemOwner: Boolean;
begin
  Result := FValues.IsItemOwner;
end;

procedure TGeneral%1%Dictionary%6%.SetIsItemOwner(const AIsItemOwner: Boolean);
begin
  FValues.IsItemOwner := AIsItemOwner;
end;{%ENDIF}

procedure TGeneral%1%Dictionary%6%.Rehash;
var I, C, L : Integer;
begin
  C := FKeys.Count;
  L := DictionaryRehashSize(C);
  FLookup := nil;
  SetLength(FLookup, L);
  FHashSize := L;
  Assert(L > 0);
  Dec(L);
  for I := 0 to C - 1 do
    DynArrayAppend(FLookup[HashStr%6%(FKeys[I], 1, -1, FCaseSensitive, 0) and L], I);
end;

function TGeneral%1%Dictionary%6%.LocateKey(const Key: %7%; var LookupIdx: Word32;
    const ErrorIfNotFound: Boolean): Integer;
var H : Word32;
    I, J, L : Integer;
begin
  L := FHashSize;
  if L > 0 then
    begin
      H := HashStr%6%(Key, 1, -1, FCaseSensitive, 0) and (L - 1);
      LookupIdx := H;
      for I := 0 to Length(FLookup[H]) - 1 do
        begin
          J := FLookup[H, I];
          if StrEqual%6%(Key, FKeys[J], FCaseSensitive) then
            begin
              Result := J;
              exit;
            end;
        end;
    end;
  if ErrorIfNotFound then
    RaiseKeyNotFoundError(Key);
  Result := -1;
end;

procedure TGeneral%1%Dictionary%6%.Add(const Key: %7%; const Value: %3%);
var H : Word32;
    L, I : Integer;
begin
  if FDuplicatesAction in [ddIgnore, ddError] then
    if LocateKey(Key, H, False) >= 0 then
      if FDuplicatesAction = ddIgnore then
        exit
      else
        RaiseDuplicateKeyError(Key);
  L := FHashSize;
  if L = 0 then
    begin
      Rehash;
      L := FHashSize;
      Assert(L > 0);
    end;
  H := Integer(HashStr%6%(Key, 1, -1, FCaseSensitive, 0) and (L - 1));
  I := FKeys.AppendItem(Key);
  DynArrayAppend(FLookup[H], I);
  FValues.AppendItem(Value);
  if (I + 1) div DictionaryAverageHashChainSize > L then
    Rehash;
end;

procedure TGeneral%1%Dictionary%6%.DeleteByIndex(const Idx: Integer; const Hash: Integer);
var I, J, H : Integer;
begin
  if Hash = -1 then
    H := HashStr%6%(FKeys[Idx], 1, -1, FCaseSensitive, 0) and (FHashSize - 1)
  else
    H := Hash;
  FKeys.Delete(Idx);
  FValues.Delete(Idx);
  J := DynArrayPosNext(Idx, FLookup[H]);
  Assert(J >= 0, 'Invalid hash value/lookup table');
  DynArrayRemove(FLookup[H], J, 1);

  for I := 0 to FHashSize - 1 do
    for J := 0 to Length(FLookup[I]) - 1 do
      if FLookup[I][J] > Idx then
        Dec(FLookup[I][J]);
end;

procedure TGeneral%1%Dictionary%6%.Delete(const Key: %7%);
var I : Integer;
    H : Word32;
begin
  I := LocateKey(Key, H, True);
  DeleteByIndex(I, H);
end;

function TGeneral%1%Dictionary%6%.HasKey(const Key: %7%): Boolean;
var H : Word32;
begin
  Result := LocateKey(Key, H, False) >= 0;
end;

procedure TGeneral%1%Dictionary%6%.Rename(const Key, NewKey: %7%);
var I, J : Integer;
    H : Word32;
begin
  I := LocateKey(Key, H, True);
  FKeys[I] := NewKey;
  J := DynArrayPosNext(I, FLookup[H]);
  Assert(J >= 0, 'Invalid hash value/lookup table');
  DynArrayRemove(FLookup[H], J, 1);
  DynArrayAppend(FLookup[HashStr%6%(NewKey, 1, -1, FCaseSensitive, 0) and (FHashSize - 1)], I);
end;

function TGeneral%1%Dictionary%6%.GetDuplicatesAction: TDictionaryDuplicatesAction;
begin
  Result := FDuplicatesAction;
end;

procedure TGeneral%1%Dictionary%6%.SetDuplicatesAction(const ADuplicatesAction: TDictionaryDuplicatesAction);
begin
  FDuplicatesAction := ADuplicatesAction;
end;

function TGeneral%1%Dictionary%6%.LocateItem(const Key: %7%; var Value: %3%): Integer;
var H : Word32;
begin
  Result := LocateKey(Key, H, False);
  if Result >= 0 then
    Value := FValues[Result]
  else
    Value := %2%;
end;

function TGeneral%1%Dictionary%6%.LocateNext(const Key: %7%; const Idx: Integer; var Value: %3%): Integer;
var L, H, I, J, K : Integer;
begin
  Result := -1;
  L := FHashSize;
  if L = 0 then
    RaiseKeyNotFoundError(Key);
  H := HashStr%6%(Key, 1, -1, FCaseSensitive, 0) and (L - 1);
  for I := 0 to Length(FLookup[H]) - 1 do
    begin
      J := FLookup[H, I];
      if J = Idx then
        begin
          if not StrEqual%6%(Key, FKeys[J], FCaseSensitive) then
            RaiseKeyNotFoundError(Key);
          for K := I + 1 to Length(FLookup[H]) - 1 do
            begin
              J := FLookup[H, K];
              if StrEqual%6%(Key, FKeys[J], FCaseSensitive) then
                begin
                  Value := FValues[J];
                  Result := J;
                  exit;
                end;
            end;
          Result := -1;
          exit;
        end;
    end;
  RaiseKeyNotFoundError(Key);
end;

procedure TGeneral%1%Dictionary%6%.SetItem(const Key: %7%; const Value: %3%);
var I : Integer;
    H : Word32;
begin
  I := LocateKey(Key, H, False);
  if I >= 0 then
    FValues[I] := Value else
    if AddOnSet then
      Add(Key, Value) else
      RaiseKeyNotFoundError(Key);
end;

procedure TGeneral%1%Dictionary%6%.RaiseIndexError;
begin
  raise EDictionary.Create('Index out of range');
end;

function TGeneral%1%Dictionary%6%.Count: Integer;
begin
  Result := FKeys.Count;
  Assert(FValues.Count = Result, 'Key/Value count mismatch');
end;

function TGeneral%1%Dictionary%6%.GetKeyByIndex(const Idx: Integer): %7%;
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FKeys.Count) then
    RaiseIndexError;
  {$ENDIF}
  Result := FKeys[Idx];
end;

procedure TGeneral%1%Dictionary%6%.DeleteItemByIndex(const Idx: Integer);
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FValues.Count) then
    RaiseIndexError;
  {$ENDIF}
  DeleteByIndex(Idx, -1);
end;

function TGeneral%1%Dictionary%6%.GetItemByIndex(const Idx: Integer): %3%;
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FValues.Count) then
    RaiseIndexError;
  {$ENDIF}
  Result := FValues[Idx];
end;

procedure TGeneral%1%Dictionary%6%.SetItemByIndex(const Idx: Integer; const Value: %3%);
begin
  {$IFOPT R+}
  if (Idx < 0) or (Idx >= FValues.Count) then
    RaiseIndexError;
  {$ENDIF}
  FValues[Idx] := Value;
end;
{%IF 4}
function TGeneral%1%Dictionary%6%.ReleaseItem(const Key: %7%): TObject;
var I : Integer;
    H : Word32;
begin
  I := LocateKey(Key, H, True);
  Result := FValues.ReleaseItem(I);
end;

procedure TGeneral%1%Dictionary%6%.ReleaseItems;
begin
  FKeys.Clear;
  FValues.ReleaseItems;
  FHashSize := 0;
  FLookup := nil;
end;

procedure TGeneral%1%Dictionary%6%.FreeItems;
begin
  FKeys.Clear;
  FValues.FreeItems;
  FHashSize := 0;
  FLookup := nil;
end;
{%ENDIF}
procedure TGeneral%1%Dictionary%6%.Clear;
begin
  FKeys.Clear;
  FValues.Clear;
  FHashSize := 0;
  FLookup := nil;
end;



{                                                                              }
{-T%1%Dictionary%6%                                                            }
{                                                                              }
function T%1%Dictionary%6%.LocateKey(const Key: %7%; var LookupIdx: Word32;
    const ErrorIfNotFound: Boolean): Integer;
var H : Word32;
    I, L : Integer;
begin
  L := FHashSize;
  if L > 0 then
    begin
      H := HashStr%6%(Key, 1, -1, FCaseSensitive, 0) and (L - 1);
      LookupIdx := H;
      for I := 0 to Length(FLookup[H]) - 1 do
        begin
          Result := FLookup[H][I];
          if StrEqual%6%(Key, T%7%Array(FKeys).Data[Result],
              FCaseSensitive) then
            exit;
        end;
    end;
  if ErrorIfNotFound then
    RaiseKeyNotFoundError(Key);
  Result := -1;
end;

function T%1%Dictionary%6%.GetItem(const Key: %7%): %3%;
var H : Word32;
    I : Integer;
begin
  I := LocateKey(Key, H, False);
  if I >= 0 then
    Result := T%1%Array(FValues).Data[I]
  else
    Result := %2%;
end;

function T%1%Dictionary%6%.LocateItem(const Key: %7%; var Value: %3%): Integer;
var H : Word32;
begin
  Result := LocateKey(Key, H, False);
  if Result >= 0 then
    Value := T%1%Array(FValues).Data[Result]
  else
    Value := %2%;
end;


{%ENDDEF}
{$IFDEF SupportAnsiString}
{%TEMPLATE ADictionaryByArrayImpl 'LongInt'       '0'    'LongInt'       ''  'TTypeDictionaryConstructor'  'A' 'AnsiString'    }
{$ENDIF}
{%TEMPLATE ADictionaryByArrayImpl 'LongInt'       '0'    'LongInt'       ''  'TTypeDictionaryConstructor'  'B' 'RawByteString' }
{%TEMPLATE ADictionaryByArrayImpl 'LongInt'       '0'    'LongInt'       ''  'TTypeDictionaryConstructor'  'U' 'UnicodeString' }
{%TEMPLATE ADictionaryByArrayImpl 'LongInt'       '0'    'LongInt'       ''  'TTypeDictionaryConstructor'  ''  'String'        }
{$IFDEF SupportAnsiString}
{%TEMPLATE ADictionaryByArrayImpl 'LongWord'      '0'    'LongWord'      ''  'TTypeDictionaryConstructor'  'A' 'AnsiString'    }
{$ENDIF}
{%TEMPLATE ADictionaryByArrayImpl 'LongWord'      '0'    'LongWord'      ''  'TTypeDictionaryConstructor'  'B' 'RawByteString' }
{%TEMPLATE ADictionaryByArrayImpl 'LongWord'      '0'    'LongWord'      ''  'TTypeDictionaryConstructor'  'U' 'UnicodeString' }
{%TEMPLATE ADictionaryByArrayImpl 'LongWord'      '0'    'LongWord'      ''  'TTypeDictionaryConstructor'  ''  'String'        }
{$IFDEF SupportAnsiString}
{%TEMPLATE ADictionaryByArrayImpl 'Int64'         '0'    'Int64'         ''  'TTypeDictionaryConstructor'  'A' 'AnsiString'    }
{$ENDIF}
{%TEMPLATE ADictionaryByArrayImpl 'Int64'         '0'    'Int64'         ''  'TTypeDictionaryConstructor'  'B' 'RawByteString' }
{%TEMPLATE ADictionaryByArrayImpl 'Int64'         '0'    'Int64'         ''  'TTypeDictionaryConstructor'  'U' 'UnicodeString' }
{%TEMPLATE ADictionaryByArrayImpl 'Int64'         '0'    'Int64'         ''  'TTypeDictionaryConstructor'  ''  'String'        }
{$IFDEF SupportAnsiString}
{%TEMPLATE ADictionaryByArrayImpl 'Single'        '0.0'  'Single'        ''  'TTypeDictionaryConstructor'  'A' 'AnsiString'    }
{$ENDIF}
{%TEMPLATE ADictionaryByArrayImpl 'Single'        '0.0'  'Single'        ''  'TTypeDictionaryConstructor'  'B' 'RawByteString' }
{%TEMPLATE ADictionaryByArrayImpl 'Single'        '0.0'  'Single'        ''  'TTypeDictionaryConstructor'  'U' 'UnicodeString' }
{%TEMPLATE ADictionaryByArrayImpl 'Single'        '0.0'  'Single'        ''  'TTypeDictionaryConstructor'  ''  'String'        }
{$IFDEF SupportAnsiString}
{%TEMPLATE ADictionaryByArrayImpl 'Double'        '0.0'  'Double'        ''  'TTypeDictionaryConstructor'  'A' 'AnsiString'    }
{$ENDIF}
{%TEMPLATE ADictionaryByArrayImpl 'Double'        '0.0'  'Double'        ''  'TTypeDictionaryConstructor'  'B' 'RawByteString' }
{%TEMPLATE ADictionaryByArrayImpl 'Double'        '0.0'  'Double'        ''  'TTypeDictionaryConstructor'  'U' 'UnicodeString' }
{%TEMPLATE ADictionaryByArrayImpl 'Double'        '0.0'  'Double'        ''  'TTypeDictionaryConstructor'  ''  'String'        }
{$IFDEF SupportAnsiString}
{%TEMPLATE ADictionaryByArrayImpl 'Extended'      '0.0'  'Extended'      ''  'TTypeDictionaryConstructor'  'A' 'AnsiString'    }
{$ENDIF}
{%TEMPLATE ADictionaryByArrayImpl 'Extended'      '0.0'  'Extended'      ''  'TTypeDictionaryConstructor'  'B' 'RawByteString' }
{%TEMPLATE ADictionaryByArrayImpl 'Extended'      '0.0'  'Extended'      ''  'TTypeDictionaryConstructor'  'U' 'UnicodeString' }
{%TEMPLATE ADictionaryByArrayImpl 'Extended'      '0.0'  'Extended'      ''  'TTypeDictionaryConstructor'  ''  'String'        }
{$IFDEF SupportAnsiString}
{%TEMPLATE ADictionaryByArrayImpl 'AnsiString'    '''''' 'AnsiString'    ''  'TTypeDictionaryConstructor'  'A' 'AnsiString'    }
{%TEMPLATE ADictionaryByArrayImpl 'AnsiString'    '''''' 'AnsiString'    ''  'TTypeDictionaryConstructor'  'U' 'UnicodeString' }
{%TEMPLATE ADictionaryByArrayImpl 'AnsiString'    '''''' 'AnsiString'    ''  'TTypeDictionaryConstructor'  ''  'String'        }
{$ENDIF}
{$IFDEF SupportAnsiString}
{%TEMPLATE ADictionaryByArrayImpl 'RawByteString' '''''' 'RawByteString' ''  'TTypeDictionaryConstructor'  'A' 'AnsiString'    }
{$ENDIF}
{%TEMPLATE ADictionaryByArrayImpl 'RawByteString' '''''' 'RawByteString' ''  'TTypeDictionaryConstructor'  'B' 'RawByteString' }
{%TEMPLATE ADictionaryByArrayImpl 'RawByteString' '''''' 'RawByteString' ''  'TTypeDictionaryConstructor'  'U' 'UnicodeString' }
{%TEMPLATE ADictionaryByArrayImpl 'RawByteString' '''''' 'RawByteString' ''  'TTypeDictionaryConstructor'  ''  'String'        }
{$IFDEF SupportAnsiString}
{%TEMPLATE ADictionaryByArrayImpl 'UnicodeString' '''''' 'UnicodeString' ''  'TTypeDictionaryConstructor'  'A' 'AnsiString'    }
{$ENDIF}
{%TEMPLATE ADictionaryByArrayImpl 'UnicodeString' '''''' 'UnicodeString' ''  'TTypeDictionaryConstructor'  'U' 'UnicodeString' }
{%TEMPLATE ADictionaryByArrayImpl 'UnicodeString' '''''' 'UnicodeString' ''  'TTypeDictionaryConstructor'  ''  'String'        }
{$IFDEF SupportAnsiString}
{%TEMPLATE ADictionaryByArrayImpl 'String'        '''''' 'String'        ''  'TTypeDictionaryConstructor'  'A' 'AnsiString'    }
{$ENDIF}
{%TEMPLATE ADictionaryByArrayImpl 'String'        '''''' 'String'        ''  'TTypeDictionaryConstructor'  'U' 'UnicodeString' }
{%TEMPLATE ADictionaryByArrayImpl 'String'        '''''' 'String'        ''  'TTypeDictionaryConstructor'  ''  'String'        }
{$IFDEF SupportAnsiString}
{%TEMPLATE ADictionaryByArrayImpl 'Pointer'       'nil'  'Pointer'       ''  'TTypeDictionaryConstructor'  'A' 'AnsiString'    }
{$ENDIF}
{%TEMPLATE ADictionaryByArrayImpl 'Pointer'       'nil'  'Pointer'       ''  'TTypeDictionaryConstructor'  'B' 'RawByteString' }
{%TEMPLATE ADictionaryByArrayImpl 'Pointer'       'nil'  'Pointer'       ''  'TTypeDictionaryConstructor'  'U' 'UnicodeString' }
{%TEMPLATE ADictionaryByArrayImpl 'Pointer'       'nil'  'Pointer'       ''  'TTypeDictionaryConstructor'  ''  'String'        }
{$IFDEF SupportAnsiString}
{%TEMPLATE ADictionaryByArrayImpl 'Interface'     'nil'  'IInterface'    ''  'TTypeDictionaryConstructor'  'A' 'AnsiString'    }
{$ENDIF}
{%TEMPLATE ADictionaryByArrayImpl 'Interface'     'nil'  'IInterface'    ''  'TTypeDictionaryConstructor'  'U' 'UnicodeString' }
{%TEMPLATE ADictionaryByArrayImpl 'Interface'     'nil'  'IInterface'    ''  'TTypeDictionaryConstructor'  ''  'String'        }
{$IFDEF SupportAnsiString}
{%TEMPLATE ADictionaryByArrayImpl 'Object'        'nil'  'TObject'       'O' 'ObjectDictionaryConstructor' 'A' 'AnsiString'    }
{$ENDIF}
{%TEMPLATE ADictionaryByArrayImpl 'Object'        'nil'  'TObject'       'O' 'ObjectDictionaryConstructor' 'B' 'RawByteString' }
{%TEMPLATE ADictionaryByArrayImpl 'Object'        'nil'  'TObject'       'O' 'ObjectDictionaryConstructor' 'U' 'UnicodeString' }
{%TEMPLATE ADictionaryByArrayImpl 'Object'        'nil'  'TObject'       'O' 'ObjectDictionaryConstructor' ''  'String'        }
{                                                                              }
{ Sparse array functions                                                       }
{                                                                              }
const
  SparseArrayAverageHashChainSize = 4;

function SparseArrayRehashSize(const Count: Integer): Integer;
var L : Integer;
begin
  L := Count div SparseArrayAverageHashChainSize; // Number of "slots"
  if L <= $10 then                                // Rehash in powers of 16
    Result := $10 else
  if L <= $100 then
    Result := $100 else
  if L <= $1000 then
    Result := $1000 else
  if L <= $10000 then
    Result := $10000 else
  if L <= $100000 then
    Result := $100000 else
  if L <= $1000000 then
    Result := $1000000
  else
    Result := $10000000;
end;



{                                                                              }
{ ASparseArray                                                                 }
{                                                                              }
procedure ASparseArray.IndexError;
begin
  raise ESparseArray.Create('Index not found');
end;

function ASparseArray.IsEmpty: Boolean;
begin
  Result := GetCount = 0;
end;



{%DEFINE TSparseArrayImpl}
function TSparse%1%Array.IsEqual(const V: TObject): Boolean;
var I, J : Integer;
    F, G : Integer;
    P, Q : PSparse%1%Record;
begin
  if V is TSparse%1%Array then
    begin
      if FCount <> TSparse%1%Array(V).FCount then
        begin
          Result := False;
          exit;
        end;
      for I := 0 to Length(FHashList) - 1 do
        for J := 0 to Length(FHashList[I]) - 1 do
          begin
            Q := @FHashList[I][J];
            P := TSparse%1%Array(V).LocateItemRecord(Q^.Idx, F, G);
            if not Assigned(P) or (P^.Value <> Q^.Value) then
              begin
                Result := False;
                exit;
              end;
          end;
      Result := True;
    end
  else
    Result := inherited IsEqual(V);
end;

function TSparse%1%Array.LocateItemRecord(const Idx: Integer;
    var LookupIdx, ChainIdx: Integer): PSparse%1%Record;
var H, I, J, L : Integer;
    P : TSparse%1%RecordArray;
begin
  I := FHashSize;
  if (I = 0) or (FCount = 0) then
    begin
      LookupIdx := -1;
      ChainIdx := -1;
      Result := nil;
      exit;
    end;
  H := Integer(HashInteger(Idx) and (I - 1));
  LookupIdx := H;
  P := FHashList[H];
  L := Length(P);
  if L > 0 then
    begin
      Result := @P[0];
      J := Idx;
      for I := 0 to L - 1 do
        if Result^.Idx = J then
          begin
            ChainIdx := I;
            exit;
          end
        else
          Inc(Result);
    end;
  Result := nil;
  ChainIdx := -1;
end;

procedure TSparse%1%Array.Rehash;
var I, J, R, F, H : Integer;
    N    : TSparse%1%ArrayHashList;
    P, Q : PSparse%1%Record;
begin
  R := SparseArrayRehashSize(FCount);
  SetLength(N, R);
  for I := 0 to Length(FHashList) - 1 do
    for J := 0 to Length(FHashList[I]) - 1 do
      begin
        P := @FHashList[I][J];
        H := Integer(HashInteger(P^.Idx) and (R - 1));
        F := Length(N[H]);
        SetLength(N[H], F + 1);
        Q := @N[H][F];
        Q^.Idx := P^.Idx;
        Q^.Value := P^.Value;
      end;
  FHashList := N;
  FHashSize := R;
end;

function TSparse%1%Array.GetCount: Integer;
begin
  Result := FCount;
end;

function TSparse%1%Array.GetItem(const Idx: Integer): %2%;
var P    : PSparse%1%Record;
    I, J : Integer;
begin
  P := LocateItemRecord(Idx, I, J);
  if not Assigned(P) then
    IndexError;
  Result := P^.Value;
end;

function TSparse%1%Array.LocateItem(const Idx: Integer; var Value: %2%): Boolean;
var P    : PSparse%1%Record;
    I, J : Integer;
begin
  P := LocateItemRecord(Idx, I, J);
  if Assigned(P) then
    begin
      Value := P^.Value;
      Result := True;
    end
  else
    begin
      Value := %3%;
      Result := False;
    end;
end;

procedure TSparse%1%Array.SetItem(const Idx: Integer; const Value: %2%);
var P    : PSparse%1%Record;
    I, J : Integer;
    L    : Integer;
begin
  P := LocateItemRecord(Idx, I, J);
  if Assigned(P) then
    P^.Value := Value
  else
    begin
      L := FHashSize;
      if L = 0 then
        begin
          Rehash;
          L := FHashSize;
          Assert(L > 0);
        end;
      I := Integer(HashInteger(Idx) and (L - 1));
      J := Length(FHashList[I]);
      SetLength(FHashList[I], J + 1);
      P := @FHashList[I][J];
      P^.Idx := Idx;
      P^.Value := Value;
      Inc(FCount);
      if (FCount + 1) div SparseArrayAverageHashChainSize > L then
        Rehash;
    end;
end;

function TSparse%1%Array.HasItem(const Idx: Integer): Boolean;
var I, J : Integer;
begin
  Result := Assigned(LocateItemRecord(Idx, I, J));
end;

function TSparse%1%Array.IsEmpty: Boolean;
begin
  Result := FCount = 0;
end;

function TSparse%1%Array.FindFirst(var Idx: Integer; var Value: %2%): Boolean;
var I : Integer;
    P : PSparse%1%Record;
begin
  for I := 0 to Length(FHashList) - 1 do
    if Length(FHashList[I]) > 0 then
      begin
        P := @FHashList[I][0];
        Idx := P^.Idx;
        Value := P^.Value;
        Result := True;
        exit;
      end;
  Idx := -1;
  Value := %3%;
  Result := False;
end;

function TSparse%1%Array.FindNext(var Idx: Integer; var Value: %2%): Boolean;
var P : PSparse%1%Record;
    I, J, L : Integer;
begin
  P := LocateItemRecord(Idx, I, J);
  if not Assigned(P) then
    IndexError;
  Inc(J);
  if J >= Length(FHashList[I]) then
    begin
      J := 0;
      L := Length(FHashList);
      Inc(I);
      while I < L do
        if Length(FHashList[I]) > 0 then
          break
        else
          Inc(I);
      if I >= L then
        begin
          Idx := -1;
          Value := %3%;
          Result := False;
          exit;
        end;
    end;
  P := @FHashList[I][J];
  Idx := P^.Idx;
  Value := P^.Value;
  Result := True;
end;
{%ENDDEF}
{$IFDEF SupportAnsiString}
{                                                                              }
{ TSparseAnsiStringArray                                                       }
{                                                                              }
procedure TSparseAnsiStringArray.Assign(const Source: TObject);
var I, L : Integer;
begin
  if Source is TSparseAnsiStringArray then
    begin
      Clear;
      L := Length(TSparseAnsiStringArray(Source).FHashList);
      SetLength(FHashList, L);
      for I := 0 to L - 1 do
        FHashList[I] := Copy(TSparseAnsiStringArray(Source).FHashList[I]);
      FHashSize := TSparseAnsiStringArray(Source).FHashSize;
      FCount := TSparseAnsiStringArray(Source).FCount;
    end
  else
    inherited Assign(Source);
end;

procedure TSparseAnsiStringArray.Clear;
begin
  FHashList := nil;
  FHashSize := 0;
  FCount := 0;
end;

{%TEMPLATE TSparseArrayImpl 'AnsiString' 'AnsiString' ''''''}
procedure TSparseAnsiStringArray.Delete(const Idx: Integer);
var P    : PSparseAnsiStringRecord;
    I, J : Integer;
    L    : Integer;
begin
  P := LocateItemRecord(Idx, I, J);
  if not Assigned(P) then
    IndexError;
  P^.Value := '';
  L := Length(FHashList[I]);
  if J < L - 1 then
    begin
      Move(FHashList[I][J + 1], FHashList[I][J], (L - J - 1) * Sizeof(TSparseAnsiStringRecord));
      ZeroMem(FHashList[I][L - 1], Sizeof(TSparseAnsiStringRecord));
    end;
  SetLength(FHashList[I], L - 1);
  Dec(FCount);
end;
{$ENDIF}



{                                                                              }
{ TSparseInt64Array                                                            }
{                                                                              }
procedure TSparseInt64Array.Assign(const Source: TObject);
var I, L : Integer;
begin
  if Source is TSparseInt64Array then
    begin
      Clear;
      L := Length(TSparseInt64Array(Source).FHashList);
      SetLength(FHashList, L);
      for I := 0 to L - 1 do
        FHashList[I] := Copy(TSparseInt64Array(Source).FHashList[I]);
      FHashSize := TSparseInt64Array(Source).FHashSize;
      FCount := TSparseInt64Array(Source).FCount;
    end
  else
    inherited Assign(Source);
end;

procedure TSparseInt64Array.Clear;
begin
  FHashList := nil;
  FHashSize := 0;
  FCount := 0;
end;

{%TEMPLATE TSparseArrayImpl 'Int64' 'Int64' '0'}
procedure TSparseInt64Array.Delete(const Idx: Integer);
var P    : PSparseInt64Record;
    I, J : Integer;
    L    : Integer;
begin
  P := LocateItemRecord(Idx, I, J);
  if not Assigned(P) then
    IndexError;
  L := Length(FHashList[I]);
  if J < L - 1 then
    Move(FHashList[I][J + 1], FHashList[I][J], (L - J - 1) * Sizeof(TSparseInt64Record));
  SetLength(FHashList[I], L - 1);
  Dec(FCount);
end;



{                                                                              }
{ TSparseExtendedArray                                                         }
{                                                                              }
procedure TSparseExtendedArray.Assign(const Source: TObject);
var I, L : Integer;
begin
  if Source is TSparseExtendedArray then
    begin
      Clear;
      L := Length(TSparseExtendedArray(Source).FHashList);
      SetLength(FHashList, L);
      for I := 0 to L - 1 do
        FHashList[I] := Copy(TSparseExtendedArray(Source).FHashList[I]);
      FHashSize := TSparseExtendedArray(Source).FHashSize;
      FCount := TSparseExtendedArray(Source).FCount;
    end
  else
    inherited Assign(Source);
end;

procedure TSparseExtendedArray.Clear;
begin
  FHashList := nil;
  FHashSize := 0;
  FCount := 0;
end;

{%TEMPLATE TSparseArrayImpl 'Extended' 'Extended' '0.0'}
procedure TSparseExtendedArray.Delete(const Idx: Integer);
var P    : PSparseExtendedRecord;
    I, J : Integer;
    L    : Integer;
begin
  P := LocateItemRecord(Idx, I, J);
  if not Assigned(P) then
    IndexError;
  L := Length(FHashList[I]);
  if J < L - 1 then
    Move(FHashList[I][J + 1], FHashList[I][J], (L - J - 1) * Sizeof(TSparseExtendedRecord));
  SetLength(FHashList[I], L - 1);
  Dec(FCount);
end;



{                                                                              }
{ TSparseObjectArray                                                           }
{                                                                              }
constructor TSparseObjectArray.Create(const AIsItemOwner: Boolean);
begin
  inherited Create;
  FIsItemOwner := AIsItemOwner;
end;

destructor TSparseObjectArray.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TSparseObjectArray.Init;
begin
  inherited Init;
  FIsItemOwner := False;
end;

procedure TSparseObjectArray.Assign(const Source: TObject);
var I, L : Integer;
begin
  if Source is TSparseObjectArray then
    begin
      Clear;
      L := Length(TSparseObjectArray(Source).FHashList);
      SetLength(FHashList, L);
      for I := 0 to L - 1 do
        FHashList[I] := Copy(TSparseObjectArray(Source).FHashList[I]);
      FHashSize := TSparseObjectArray(Source).FHashSize;
      FCount := TSparseObjectArray(Source).FCount;
      FIsItemOwner := False;
    end
  else
    inherited Assign(Source);
end;

procedure TSparseObjectArray.Clear;
var I, J : Integer;
begin
  if FIsItemOwner then
    for I := 0 to Length(FHashList) - 1 do
      for J := 0 to Length(FHashList[I]) - 1 do
        FreeAndNil(FHashList[I][J].Value);
  FHashList := nil;
  FHashSize := 0;
  FCount := 0;
end;

{%TEMPLATE TSparseArrayImpl 'Object' 'TObject' 'nil'  }
procedure TSparseObjectArray.Delete(const Idx: Integer);
var P    : PSparseObjectRecord;
    I, J : Integer;
    L    : Integer;
begin
  P := LocateItemRecord(Idx, I, J);
  if not Assigned(P) then
    IndexError;
  if FIsItemOwner then
    FreeAndNil(P^.Value);
  L := Length(FHashList[I]);
  if J < L - 1 then
    Move(FHashList[I][J + 1], FHashList[I][J], (L - J - 1) * Sizeof(TSparseObjectRecord));
  SetLength(FHashList[I], L - 1);
  Dec(FCount);
end;

function TSparseObjectArray.ReleaseItem(const Idx: Integer): TObject;
var P    : PSparseObjectRecord;
    I, J : Integer;
begin
  P := LocateItemRecord(Idx, I, J);
  if not Assigned(P) then
    IndexError;
  Result := P^.Value;
  P^.Value := nil;
end;



{                                                                              }
{ TDoublyLinkedItem                                                            }
{                                                                              }
function TDoublyLinkedItem.HasNext: Boolean;
begin
  Result := Assigned(Next);
end;

function TDoublyLinkedItem.Last: TDoublyLinkedItem;
var P : TDoublyLinkedItem;
begin
  P := self;
  repeat
    Result := P;
    P := P.Next;
  until not Assigned(P);
end;

function TDoublyLinkedItem.Count: Integer;
var N : TDoublyLinkedItem;
begin
  Result := 1;
  N := FNext;
  while Assigned(N) do
    begin
      Inc(Result);
      N := N.Next;
    end;
end;

function TDoublyLinkedItem.HasPrev: Boolean;
begin
  Result := Assigned(FPrev);
end;

function TDoublyLinkedItem.First: TDoublyLinkedItem;
var P : TDoublyLinkedItem;
begin
  P := self;
  repeat
    Result := P;
    P := P.Prev;
  until not Assigned(P);
end;

procedure TDoublyLinkedItem.Delete;
begin
  Remove;
  Free;
end;

procedure TDoublyLinkedItem.Remove;
begin
  if Assigned(Next) then
    Next.Prev := FPrev;
  if Assigned(Prev) then
    Prev.Next := FNext;
end;

function TDoublyLinkedItem.RemoveNext: TDoublyLinkedItem;
begin
  Result := FNext;
  if Assigned(Result) then
    begin
      FNext := Result.Next;
      if Assigned(FNext) then
        FNext.Prev := self;
    end;
end;

procedure TDoublyLinkedItem.DeleteNext;
begin
  RemoveNext.Free;
end;

function TDoublyLinkedItem.RemovePrev: TDoublyLinkedItem;
begin
  Result := FPrev;
  if Assigned(Result) then
    begin
      FPrev := Result.Prev;
      if Assigned(FPrev) then
        FPrev.Next := self;
    end;
end;

procedure TDoublyLinkedItem.DeletePrev;
begin
  RemovePrev.Free;
end;

procedure TDoublyLinkedItem.InsertAfter(const Item: TDoublyLinkedItem);
begin
  Assert(Assigned(Item));
  Item.Next := FNext;
  Item.Prev := self;
  if Assigned(FNext) then
    FNext.Prev := Item;
  FNext := Item;
end;

procedure TDoublyLinkedItem.InsertBefore(const Item: TDoublyLinkedItem);
begin
  Assert(Assigned(Item));
  Item.Next := self;
  Item.Prev := FPrev;
  if Assigned(FPrev) then
    FPrev.Next := Item;
  FPrev := Item;
end;

destructor TDoublyLinkedItem.DestroyList;
var N : TDoublyLinkedItem;
begin
  while Assigned(FNext) do
    begin
      N := FNext;
      FNext := N.Next;
      N.Free;
    end;
  inherited Destroy;
end;



{%DEFINE LinkedItemImpl}
{                                                                              }
{-TDoublyLinked%1%                                                             }
{                                                                              }
constructor TDoublyLinked%1%.Create(const V: %2%);
begin
  inherited Create;
  Value := V;
end;

procedure TDoublyLinked%1%.InsertAfter(const V: %2%);
begin
  inherited InsertAfter(TDoublyLinked%1%.Create(V));
end;

procedure TDoublyLinked%1%.InsertBefore(const V: %2%);
begin
  inherited InsertBefore(TDoublyLinked%1%.Create(V));
end;

procedure TDoublyLinked%1%.InsertFirst(const V: %2%);
begin
  TDoublyLinked%1%(First).InsertBefore(V);
end;

procedure TDoublyLinked%1%.Append(const V: %2%);
begin
  TDoublyLinked%1%(Last).InsertAfter(V);
end;

function TDoublyLinked%1%.FindNext(const Find: %2%): TDoublyLinked%1%;
begin
  Result := self;
  repeat
    if Result.Value = Find then
      exit;
    Result := TDoublyLinked%1%(Result.Next);
  until not Assigned(Result);
end;

function TDoublyLinked%1%.FindPrev(const Find: %2%): TDoublyLinked%1%;
begin
  Result := self;
  repeat
    if Result.Value = Find then
      exit;
    Result := TDoublyLinked%1%(Result.Prev);
  until not Assigned(Result);
end;


{%ENDDEF}
{%TEMPLATE LinkedItemImpl 'Integer'  'Integer'}
{%TEMPLATE LinkedItemImpl 'Extended' 'Extended'}
{$IFDEF SupportAnsiString}
{%TEMPLATE LinkedItemImpl 'String'   'AnsiString'}
{$ENDIF}
{%TEMPLATE LinkedItemImpl 'Object'   'TObject'}
{                                                                              }
{ Open array to Linked list                                                    }
{                                                                              }
{%DEFINE OpenArrayToLinkedListImpl}
function As%2%Linked%3%List(const V: Array of %1%): T%2%Linked%3%;
var I, L : T%2%Linked%3%;
    F   : Integer;
begin
  Result := nil;
  L := nil;
  for F := 0 to High(V) do
    begin
      I := T%2%Linked%3%.Create(V [F]);
      if not Assigned(L) then
        begin
          L := I;
          Result := I;
        end else
        begin
          L.InsertAfter(I);
          L := I;
        end;
    end;
end;
{%ENDDEF}
{%TEMPLATE OpenArrayToLinkedListImpl 'Integer'    'Doubly' 'Integer'}
{%TEMPLATE OpenArrayToLinkedListImpl 'Extended'   'Doubly' 'Extended'}
{$IFDEF SupportAnsiString}
{%TEMPLATE OpenArrayToLinkedListImpl 'AnsiString' 'Doubly' 'String'}
{$ENDIF}



{                                                                              }
{ TDoublyLinkedList                                                            }
{                                                                              }
Destructor TDoublyLinkedList.Destroy;
begin
  DeleteList;
  inherited Destroy;
end;

function TDoublyLinkedList.IsEmpty: Boolean;
begin
  Result := not Assigned(FFirst);
end;

procedure TDoublyLinkedList.Append(const Item: TDoublyLinkedItem);
begin
  if not Assigned(Item) then
    exit;
  if not Assigned(FLast) then
    begin
      FFirst := Item;
      FLast := Item;
      Item.Prev := nil;
      Item.Next := nil;
    end else
    begin
      FLast.InsertAfter(Item);
      FLast := Item;
    end;
  Inc(FCount);
end;

procedure TDoublyLinkedList.InsertFront(const Item: TDoublyLinkedItem);
begin
  if not Assigned(Item) then
    exit;
  if not Assigned(FFirst) then
    begin
      FFirst := Item;
      FLast := Item;
      Item.Prev := nil;
      Item.Next := nil;
    end else
    begin
      FFirst.InsertBefore(Item);
      FFirst := Item;
    end;
  Inc(FCount);
end;

procedure TDoublyLinkedList.Remove(const Item: TDoublyLinkedItem);
begin
  if not Assigned(Item) then
    exit;
  if FFirst = Item then
    FFirst := Item.Next;
  if FLast = Item then
    FLast := Item.Prev;
  Item.Remove;
  Dec(FCount);
end;

function TDoublyLinkedList.RemoveFirst: TDoublyLinkedItem;
var N : TDoublyLinkedItem;
begin
  Result := FFirst;
  if not Assigned(Result) then
    exit;
  if Result = FLast then
    begin
      FFirst := nil;
      FLast := nil;
    end else
    begin
      N := Result.Next;
      Result.Remove;
      FFirst := N;
    end;
  Dec(FCount);
end;

function TDoublyLinkedList.RemoveLast: TDoublyLinkedItem;
var P : TDoublyLinkedItem;
begin
  Result := FLast;
  if not Assigned(Result) then
    exit;
  if Result = FFirst then
    begin
      FFirst := nil;
      FLast := nil;
    end
  else
    begin
      P := Result.Prev;
      Result.Remove;
      FLast := P;
    end;
  Dec(FCount);
end;

procedure TDoublyLinkedList.Delete(const Item: TDoublyLinkedItem);
begin
  Remove(Item);
  Item.Free;
end;

procedure TDoublyLinkedList.DeleteFirst;
begin
  RemoveFirst.Free;
end;

procedure TDoublyLinkedList.DeleteLast;
begin
  RemoveLast.Free;
end;

procedure TDoublyLinkedList.DeleteList;
var F : TDoublyLinkedItem;
begin
  F := FFirst;
  FFirst := nil;
  FLast := nil;
  if Assigned(F) then
    F.DestroyList;
  FCount := 0;
end;



{                                                                              }
{ Self testing code                                                            }
{                                                                              }
{$IFDEF DEBUG}
{$IFDEF TEST}
{$ASSERTIONS ON}
procedure Test_Array;
var I : Integer;
    F : TIntegerArray;
begin
  // TIntegerArray
  F := TIntegerArray.Create;
  for I := 0 to 16384 do
    Assert(F.AppendItem(I) = I, 'Array.AppendItem');
  Assert(F.Count = 16385, 'Array.Count');
  for I := 0 to 16384 do
    Assert(F[I] = I,      'Array.GetItem');
  for I := 0 to 16384 do
    F[I] := I + 1;
  for I := 0 to 16384 do
    Assert(F[I] = I + 1,  'Array.SetItem');
  F.Delete(0, 1);
  Assert(F.Count = 16384, 'Array.Delete');
  for I := 0 to 16383 do
    Assert(F[I] = I + 2,  'Array.Delete');
  F.Insert(0, 2);
  F[0] := 0;
  F[1] := 1;
  for I := 0 to 16384 do
    Assert(F[I] = I,      'Array.Insert');

  F.Count := 4;
  Assert(F.Count = 4,     'Array.SetCount');
  F[0] := 9;
  F[1] := -2;
  F[2] := 3;
  F[3] := 4;
  F.Sort;
  Assert(F[0] = -2,       'Array.Sort');
  Assert(F[1] = 3,        'Array.Sort');
  Assert(F[2] = 4,        'Array.Sort');
  Assert(F[3] = 9,        'Array.Sort');

  F.Count := 7;
  F[0] := 3;
  F[1] := 5;
  F[2] := 5;
  F[3] := 2;
  F[4] := 5;
  F[5] := 5;
  F[6] := 1;
  F.Sort;
  Assert(F[0] = 1,        'Array.Sort');
  Assert(F[1] = 2,        'Array.Sort');
  Assert(F[2] = 3,        'Array.Sort');
  Assert(F[3] = 5,        'Array.Sort');
  Assert(F[4] = 5,        'Array.Sort');
  Assert(F[5] = 5,        'Array.Sort');
  Assert(F[6] = 5,        'Array.Sort');

  F.Count := 7;
  F[0] := 1;
  F[1] := 5;
  F[2] := 5;
  F[3] := 1;
  F[4] := 5;
  F[5] := 2;
  F[6] := 1;
  F.RemoveDuplicates(False);
  Assert(F.Count = 3,     'Array.RemoveDuplicates');
  Assert(F[0] = 1,        'Array.RemoveDuplicates');
  Assert(F[1] = 5,        'Array.RemoveDuplicates');
  Assert(F[2] = 2,        'Array.RemoveDuplicates');

  F.Count := 7;
  F[0] := 1;
  F[1] := 1;
  F[2] := 1;
  F[3] := 2;
  F[4] := 5;
  F[5] := 5;
  F[6] := 5;
  F.RemoveDuplicates(True);
  Assert(F.Count = 3,     'Array.RemoveDuplicates');
  Assert(F[0] = 1,        'Array.RemoveDuplicates');
  Assert(F[1] = 2,        'Array.RemoveDuplicates');
  Assert(F[2] = 5,        'Array.RemoveDuplicates');

  F.Clear;
  Assert(F.Count = 0,     'Array.Clear');
  F.Free;
end;

procedure Test_Dictionary;
{$IFDEF SupportAnsiString}
var F : TIntegerDictionaryA;
    G : TStringDictionaryA;
    I : Integer;
{$ENDIF}
begin
  {$IFDEF SupportAnsiString}
  F := TIntegerDictionaryA.Create;
  for I := 0 to 16384 do
    F.Add(IntToStringA(I), I);
  Assert(F.Count = 16385, 'Dictionary.Count');
  for I := 0 to 16384 do
    Assert(F.GetKeyByIndex(I) = IntToStringA(I), 'Dictionary.GetKeyByIndex');
  for I := 0 to 16384 do
    Assert(F[IntToStringA(I)] = I, 'Dictionary.GetItem');
  Assert(F['0'] = 0, 'Dictionary.GetItem');
  Assert(F['4001'] = 4001, 'Dictionary.GetItem');
  Assert(F['16384'] = 16384, 'Dictionary.GetItem');
  for I := 0 to 16384 do
    Assert(F.GetItemByIndex(I) = I, 'Dictionary.GetItemByIndex');
  Assert(F.HasKey('5'), 'Dictionary.HasKey');
  Assert(not F.HasKey('X'), 'Dictionary.HasKey');
  F.Rename('5', 'X');
  Assert(not F.HasKey('5'), 'Dictionary.Rename');
  Assert(F.HasKey('X'), 'Dictionary.Rename');
  Assert(F['X'] = 5, 'Dictionary.Rename');
  F.Delete('X');
  Assert(not F.HasKey('X'), 'Dictionary.Delete');
  Assert(F.Count = 16384, 'Dictionary.Delete');
  F.Delete('0');
  Assert(not F.HasKey('0'), 'Dictionary.Delete');
  Assert(F.Count = 16383, 'Dictionary.Delete');
  F.DeleteItemByIndex(0);
  Assert(not F.HasKey('1'), 'Dictionary.DeleteItemByIndex');
  Assert(F.Count = 16382, 'Dictionary.DeleteItemByIndex');
  F.Free;

  G := TStringDictionaryA.Create;
  for I := 0 to 16384 do
    G.Add(IntToStringA(I), IntToStr(I));
  Assert(G.Count = 16385, 'Dictionary.Count');
  for I := 0 to 16384 do
    Assert(G.GetKeyByIndex(I) = IntToStringA(I), 'Dictionary.GetKeyByIndex');
  Assert(G['0'] = '0', 'Dictionary.GetItem');
  Assert(G['5'] = '5', 'Dictionary.GetItem');
  Assert(G['16384'] = '16384', 'Dictionary.GetItem');
  for I := 0 to 16384 do
    Assert(G.GetItemByIndex(I) = IntToStr(I), 'Dictionary.GetItemByIndex');
  Assert(G.HasKey('5'), 'Dictionary.HasKey');
  Assert(not G.HasKey('X'), 'Dictionary.HasKey');
  G.Rename('5', 'X');
  Assert(not G.HasKey('5'), 'Dictionary.Rename');
  Assert(G.HasKey('X'), 'Dictionary.Rename');
  Assert(G['X'] = '5', 'Dictionary.Rename');
  G.Delete('X');
  Assert(not G.HasKey('X'), 'Dictionary.Delete');
  Assert(G.Count = 16384, 'Dictionary.Delete');
  G.Delete('0');
  Assert(not G.HasKey('0'), 'Dictionary.Delete');
  Assert(G.Count = 16383, 'Dictionary.Delete');
  G.DeleteItemByIndex(0);
  Assert(not G.HasKey('1'), 'Dictionary.DeleteItemByIndex');
  Assert(G.Count = 16382, 'Dictionary.DeleteItemByIndex');
  G.Free;
  {$ENDIF}
end;

procedure Test_SparseArray;
var A, D : TSparseObjectArray;
    B : Array[0..2] of TObject;
    I, J : Integer;
    V : TObject;
    {$IFDEF SupportAnsiString}
    S, T : TSparseAnsiStringArray;
    {$ENDIF}
begin
  B[0] := TObject.Create;
  B[1] := TObject.Create;
  B[2] := TObject.Create;
  A := TSparseObjectArray.Create;
  try
    Assert(A.Count = 0);
    Assert(A.IsEmpty);
    Assert(not A.FindFirst(I, V));
    Assert(A.IsEqual(A));
    Assert(not A.LocateItem(0, V));
    Assert(not Assigned(V));
    A[100] := B[0];
    Assert(A.Count = 1);
    Assert(not A.IsEmpty);
    Assert(A[100] = B[0]);
    Assert(not A.LocateItem(0, V));
    Assert(A.LocateItem(100, V));
    Assert(V = B[0]);
    Assert(not A.HasItem(1000));
    A[1000] := B[1];
    Assert(A.HasItem(1000));
    Assert(A.Count = 2);
    Assert(A[1000] = B[1]);
    A[-50000] := B[2];
    Assert(A.Count = 3);
    Assert(A[100] = B[0]);
    Assert(A[1000] = B[1]);
    Assert(A[-50000] = B[2]);
    Assert(A.IsEqual(A));
    A[100] := B[1];
    Assert(A[100] = B[1]);
    A.Delete(1000);
    Assert(A.Count = 2);
    Assert(not A.HasItem(1000));
    Assert(A.FindFirst(I, V));
    Assert((I = 100) or (I = -50000));
    J := I;
    Assert(A.FindNext(I, V));
    Assert(((I = 100) or (I = -50000)) and (I <> J));
    Assert(not A.FindNext(I, V));
    A.Clear;
    Assert(A.Count = 0);
    Assert(A.IsEmpty);
    Assert(not A.FindFirst(I, V));

    A[0] := B[0];
    A[-10] := B[1];
    A[20] := B[2];
    Assert(A.Count = 3);
    Assert((A[0] = B[0]) and (A[-10] = B[1]) and (A[20] = B[2]));
    D := A.Duplicate as TSparseObjectArray;
    Assert(D.Count = 3);
    Assert((D[0] = B[0]) and (D[-10] = B[1]) and (D[20] = B[2]));
    Assert(A.IsEqual(D));
    Assert(D.IsEqual(A));
    D[0] := B[1];
    Assert(not A.IsEqual(D));
    Assert(not D.IsEqual(A));
    D[1] := B[1];
    Assert(not A.IsEqual(D));
    Assert(D.Count = 4);
    Assert((D[0] = B[1]) and (D[1] = B[1]));
    Assert(A.Count = 3);
    Assert((A[0] = B[0]) and (A[-10] = B[1]) and (A[20] = B[2]));
    Assert(not A.HasItem(1));
    D.Delete(1);
    Assert(D.Count = 3);
    Assert(not D.HasItem(1));
    D[0] := B[0];
    Assert(D.IsEqual(A));
    D.Free;
    Assert((A[0] = B[0]) and (A[-10] = B[1]) and (A[20] = B[2]));
  finally
    A.Free;
    B[2].Free;
    B[1].Free;
    B[0].Free;
  end;

  {$IFDEF SupportAnsiString}
  S := TSparseAnsiStringArray.Create;
  T := TSparseAnsiStringArray.Create;
  try
    Assert(S.IsEmpty);
    Assert(S.Count = 0);
    Assert(S.IsEqual(T));
    for I := 1 to 1000 do
      begin
        S[I * 3] := IntToStringA(I);
        T[I] := IntToStringA(I);
        Assert(S.HasItem(I * 3));
        Assert(not S.HasItem(I * 3 + 1));
      end;
    Assert(S.Count = 1000);
    Assert(T.Count = 1000);
    for I := 1 to 1000 do
      begin
        Assert(S[I * 3] = IntToStringA(I));
        Assert(T[I] = IntToStringA(I));
      end;
    for I := 1 to 1000 do
      begin
        S[I * 3] := IntToStringA(I + 1);
        S[I * 3 - 1] := IntToStringA(I);
        T[1000 + I * 2] := IntToStringA(I);
      end;
    Assert(S.Count = 2000);
    Assert(T.Count = 2000);
    for I := 1 to 1000 do
      begin
        Assert(S[I * 3] = IntToStringA(I + 1));
        Assert(S[I * 3 - 1] = IntToStringA(I));
        Assert(T[I] = IntToStringA(I));
        Assert(T[1000 + I * 2] = IntToStringA(I));
      end;
    Assert(not S.IsEqual(T));
    S.Clear;
    Assert(S.Count = 0);
  finally
    FreeAndNil(T);
    FreeAndNil(S);
  end;
  {$ENDIF}
end;

procedure Test_HashedStringArray;
{$IFDEF SupportAnsiString}
var A : THashedAnsiStringArray;
    B : THashedRawByteStringArray;
{$ENDIF}
begin
  {$IFDEF SupportAnsiString}
  A := THashedAnsiStringArray.Create(True);
  try
    A.AppendItem('abc');
    Assert(A.Count = 1);
    A.Clear;
    Assert(A.Count = 0);
    A.AppendItem('def');
    Assert(A.Count = 1);
  finally
    A.Free;
  end;
  //
  A := THashedAnsiStringArray.Create(False);
  try
    A.AppendItem('123');
    A.AppendItem('267');
    A.AppendItem('328');
    A.AppendItem('423');
    A.AppendItem('523a');
    Assert(A.PosNext('123') = 0);
    Assert(A.PosNext('423') = 3);
    A.Delete(0);
    Assert(A.PosNext('123') = -1);
    Assert(A.PosNext('423') = 2);
  finally
    A.Free;
  end;

  B := THashedRawByteStringArray.Create(True);
  try
    B.AppendItem('abc');
    Assert(B.Count = 1);
    B.Clear;
    Assert(B.Count = 0);
    B.AppendItem('def');
    Assert(B.Count = 1);
  finally
    B.Free;
  end;
  //
  B := THashedRawByteStringArray.Create(False);
  try
    B.AppendItem('123');
    B.AppendItem('267');
    B.AppendItem('328');
    B.AppendItem('423');
    B.AppendItem('523a');
    Assert(B.PosNext('123') = 0);
    Assert(B.PosNext('423') = 3);
    B.Delete(0);
    Assert(B.PosNext('123') = -1);
    Assert(B.PosNext('423') = 2);
  finally
    B.Free;
  end;
  {$ENDIF}
end;

procedure Test;
begin
  Test_Array;
  Test_Dictionary;
  Test_SparseArray;
  Test_HashedStringArray;
end;
{$ENDIF}
{$ENDIF}



end.

