{*******************************************************************************}
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
{*******************************************************************************}

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



{%DEFINE AArrayDynArray}
{                                                                              }
{-T%1%Array                                                                    }
{-  An T%1%Array implemented using a dynamic array.                            }
{                                                                              }
type
  T%1%Array = class(TArrayBase)
  protected
    FData     : %1%Array;
    FCapacity : NativeInt;
    FCount    : NativeInt;

    procedure SetData(const AData: %1%Array); virtual;

    procedure SetCount(const ANewCount: NativeInt);

    function  GetItem(const AIdx: NativeInt): %2%;                {$IFDEF UseInline}inline;{$ENDIF}
    procedure SetItem(const AIdx: NativeInt; const AValue: %2%);  {$IFDEF UseInline}inline;{$ENDIF}

    function  CompareItems(const AIdx1, AIdx2: NativeInt): Int32; virtual;{%IF 3}

    function  GetItemAsString(const AIdx: NativeInt): String;
    procedure SetItemAsString(const AIdx: NativeInt; const AValue: String);

    function  GetAsString: String;
    procedure SetAsString(const S: String);{%ENDIF}

  public
    class function CreateInstance: T%1%Array; virtual;

    constructor Create; overload; virtual;
    constructor Create(const V: %1%Array); overload;

    property  Data: %1%Array read FData write SetData;

    procedure Clear;

    procedure Assign(const ASource: T%1%Array); overload;
    procedure Assign(const ASource: %1%Array); overload;
    procedure Assign(const ASource: Array of %2%); overload;

    function  Duplicate: T%1%Array;

    function  IsEqual(const V: T%1%Array): Boolean;

    property  Count: NativeInt read FCount write SetCount;
    property  Item[const AIdx: NativeInt]: %2% read GetItem write SetItem; default;

    function  PosNext(
              const AItem: %2%;
              const APrevPos: NativeInt = -1;
              const IsSortedAscending: Boolean = False): NativeInt;

    function  GetIndex(const AValue: %2%): NativeInt;
    function  HasValue(const AValue: %2%): Boolean;

    function  Add(const AValue: %2%): NativeInt;
    function  AddIfNotExists(const AValue: %2%): NativeInt;

    function  AddArray(const AArray: %1%Array): NativeInt; overload;
    function  AddArray(const AArray: T%1%Array): NativeInt; overload;

    procedure Insert(const AIdx: NativeInt; const ACount: NativeInt = 1);

    procedure Delete(const AIdx: NativeInt; const ACount: NativeInt = 1);

    procedure Sort;

    function  GetRange(const ALoIdx, AHiIdx: NativeInt): %1%Array;
    procedure SetRange(const ALoIdx, AHiIdx: NativeInt; const V: %1%Array);

    procedure Fill(const AIdx, ACount: NativeInt; const AValue: %2%);{%IF 3}

    property  ItemAsString[const AIdx: NativeInt]: String read GetItemAsString write SetItemAsString;
    property  AsString: String read GetAsString write SetAsString;{%ENDIF}
  end;


{%ENDDEF}
{%TEMPLATE AArrayDynArray 'Int32'    'Int32'    'S' }
{%TEMPLATE AArrayDynArray 'Int64'    'Int64'    'S' }
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



{%TEMPLATE AArrayDynArray 'Byte'      'Byte'      'S' }
{%TEMPLATE AArrayDynArray 'Word32'    'Word32'    'S' }
{%TEMPLATE AArrayDynArray 'Word64'    'Word64'    '' }
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



{%TEMPLATE AArrayDynArray 'Single'        'Single'          'S' }
{%TEMPLATE AArrayDynArray 'Double'        'Double'          'S' }
{                                                                              }
{ Equivalent Float type (Double)                                               }
{                                                                              }
type
  TFloatArray = TDoubleArray;



{$IFDEF SupportAnsiString}
{%TEMPLATE AArrayDynArray 'AnsiString'    'AnsiString'      'S' }
{$ENDIF}
{%TEMPLATE AArrayDynArray 'RawByteString' 'RawByteString'   'S' }
{%TEMPLATE AArrayDynArray 'UnicodeString' 'UnicodeString'   'S' }
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



{%TEMPLATE AArrayDynArray 'Pointer'       'Pointer'         '' }
{%TEMPLATE AArrayDynArray 'Interface'     'IInterface'      '' }
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



{%DEFINE AArrayDynArrayImpl}
{                                                                              }
{-T%1%Array                                                                    }
{                                                                              }
class function T%1%Array.CreateInstance: T%1%Array;
begin
  Result := T%1%Array.Create;
end;

constructor T%1%Array.Create;
begin
  inherited Create;
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

procedure T%1%Array.Clear;
begin
  FData := nil;
  FCapacity := 0;
  FCount := 0;
end;

procedure T%1%Array.Assign(const ASource: %1%Array);
begin
  SetData(Copy(ASource));
end;

procedure T%1%Array.Assign(const ASource: Array of %3%);
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

procedure T%1%Array.Assign(const ASource: T%1%Array);
var
  D : %1%Array;
begin
  if not Assigned(ASource) then
    raise EArrayError.Create(SErrSourceNotAssigned);

  D := Copy(ASource.FData);
  SetLength(D, ASource.FCount);
  SetData(D);
end;

function T%1%Array.Duplicate: T%1%Array;
var
  Obj : T%1%Array;
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

function T%1%Array.IsEqual(const V: T%1%Array): Boolean;
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

procedure T%1%Array.SetCount(const ANewCount: NativeInt);
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
      {%IF 2}if N > L then
        FillChar(FData[L], SizeOf(%3%) * (N - L), 0);
      {%ENDIF}
      FCapacity := N;
    end;
end;

function T%1%Array.GetItem(const AIdx: NativeInt): %3%;
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

procedure T%1%Array.SetItem(const AIdx: NativeInt; const AValue: %3%);
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

function T%1%Array.PosNext(
         const AItem: %3%;
         const APrevPos: NativeInt;
         const IsSortedAscending: Boolean): NativeInt;
var
  F : NativeInt;
  I : NativeInt;
  L : NativeInt;
  H : NativeInt;
  D : %3%;
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
            if %5%D{%IF 5}){%ENDIF} > %5%AItem{%IF 5}){%ENDIF} then
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

function T%1%Array.GetIndex(const AValue: %3%): NativeInt;
begin
  Result := PosNext(AValue, -1, False);
end;

function T%1%Array.HasValue(const AValue: %3%): Boolean;
begin
  Result := PosNext(AValue, -1, False) >= 0;
end;

function T%1%Array.Add(const AValue: %3%): NativeInt;
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

function T%1%Array.AddIfNotExists(const AValue: %3%): NativeInt;
var
  I : NativeInt;
begin
  I := PosNext(AValue, -1);
  if I < 0 then
    I := Add(AValue);
  Result := I;
end;

function T%1%Array.AddArray(const AArray: %1%Array): NativeInt;
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

function T%1%Array.AddArray(const AArray: T%1%Array): NativeInt;
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

procedure T%1%Array.Insert(const AIdx: NativeInt; const ACount: NativeInt = 1);
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
    Move(FData[AIdx], FData[AIdx + A], (C - AIdx) * SizeOf(%3%));

  FillChar(FData[AIdx], A * SizeOf(%3%), 0);
end;

procedure T%1%Array.Delete(const AIdx: NativeInt; const ACount: NativeInt = 1);
var
  A : NativeInt;
  C : NativeInt;
  L : NativeInt;{%IF 6}
  I : NativeInt;{%ENDIF}
begin
  A := ACount;
  if A <= 0 then
    exit;

  C := FCount;
  if (AIdx < 0) or (AIdx >= C) then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AIdx]);

  L := AIdx + A;
  if L > C then
    raise EArrayError.CreateFmt(SErrArrayIndexOutOfBounds, [AIdx]);{%IF 6}

  for I := AIdx to L - 1 do
    FData[I] := %6%;{%ENDIF}

  if L < C then{%IF 6}
    begin
  {%ENDIF}    Move(FData[AIdx + A], FData[AIdx], SizeOf(%3%) * (C - AIdx - A));{%IF 6}
      FillChar(FData[C - A], SizeOf(%3%) * A, 0);
    end;{%ENDIF}

  SetCount(C - A);
end;

function T%1%Array.CompareItems(const AIdx1, AIdx2: NativeInt): Int32;
var
  I, J : %3%;
begin
  Assert(AIdx1 >= 0);
  Assert(AIdx1 < FCount);
  Assert(AIdx2 >= 0);
  Assert(AIdx2 < FCount);

  I := FData[AIdx1];
  J := FData[AIdx2];
  if %5%I{%IF 5}){%ENDIF} < %5%J{%IF 5}){%ENDIF} then
    Result := -1
  else
  if %5%I{%IF 5}){%ENDIF} > %5%J{%IF 5}){%ENDIF} then
    Result := 1
  else
    Result := 0;
end;

procedure T%1%Array.Sort;

  procedure QuickSort(L, R: NativeInt);
  var
    I : NativeInt;
    J : NativeInt;
    M : NativeInt;
    T : %3%;
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

procedure T%1%Array.Fill(const AIdx, ACount: NativeInt; const AValue: %3%);
var
  I : NativeInt;
begin
  for I := AIdx to AIdx + ACount - 1 do
    FData[I] := AValue;
end;

function T%1%Array.GetRange(const ALoIdx, AHiIdx: NativeInt): %1%Array;
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

procedure T%1%Array.SetRange(const ALoIdx, AHiIdx: NativeInt; const V: %1%Array);
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
    Move(V[0], FData[L], C * Sizeof(%3%));
end;

{%IF 7}function T%1%Array.GetItemAsString(const AIdx: NativeInt): String;
begin
  Result := %7%(GetItem(AIdx));
end;

function T%1%Array.GetAsString: String;
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
{%ENDIF}
{%IF 8}procedure T%1%Array.SetItemAsString(const AIdx: NativeInt; const AValue: String);
begin
  SetItem(AIdx, %8%(AValue));
end;

procedure T%1%Array.SetAsString(const S: String);
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
{%ENDIF}
{%ENDDEF}

{%TEMPLATE AArrayDynArrayImpl 'Int32'         'Z'  'Int32'         ''   ''             ''        'IntToStr'     'StrToInt'        }
{%TEMPLATE AArrayDynArrayImpl 'Int64'         'Z'  'Int64'         ''   ''             ''        'IntToStr'     'StrToInt64'      }
{%TEMPLATE AArrayDynArrayImpl 'Byte'          'Z'  'Byte'          ''   ''             ''        'IntToStr'     'StrToInt'        }
{%TEMPLATE AArrayDynArrayImpl 'Word32'        'Z'  'Word32'        ''   ''             ''        'IntToStr'     'StrToInt'        }
{%TEMPLATE AArrayDynArrayImpl 'Word64'        'Z'  'Word64'        ''   ''             ''        ''             ''                }
{%TEMPLATE AArrayDynArrayImpl 'Single'        'Z'  'Single'        ''   ''             ''        'FloatToStr'   'StrToFloat'      }
{%TEMPLATE AArrayDynArrayImpl 'Double'        'Z'  'Double'        ''   ''             ''        'FloatToStr'   'StrToFloat'      }
{$IFDEF SupportAnsiString}
{%TEMPLATE AArrayDynArrayImpl 'AnsiString'    'Z'  'AnsiString'    'A'  ''             ''''''    'String'       'AnsiString'      }
{$ENDIF}
{%TEMPLATE AArrayDynArrayImpl 'RawByteString' 'Z'  'RawByteString' 'B'  ''             ''''''    'String'       'RawByteString'   }
{%TEMPLATE AArrayDynArrayImpl 'UnicodeString' 'Z'  'UnicodeString' 'U'  ''             ''''''    'String'       'UnicodeString'   }
{%TEMPLATE AArrayDynArrayImpl 'Pointer'       'Z'  'Pointer'       ''   'NativeUInt('  'nil'     ''             ''                }
{%TEMPLATE AArrayDynArrayImpl 'Interface'     'Z'  'IInterface'    ''   'NativeUInt('  'nil'     ''             ''                }



end.

