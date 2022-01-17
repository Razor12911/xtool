{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals 5.00                                        }
{   File name:        flcVectors.pas                                           }
{   File version:     5.11                                                     }
{   Description:      Vector class                                             }
{                                                                              }
{   Copyright:        Copyright (c) 1999-2016, David J Butler                  }
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
{   1999/09/27  0.01  Initial version.                                         }
{   1999/10/30  0.02  Added StdDev                                             }
{   1999/11/04  0.03  Added Pos, Append                                        }
{   2000/06/08  0.04  TVector now inherits from TExtendedArray.                }
{   2002/06/01  0.05  Created cVector unit from cMaths.                        }
{   2003/02/16  3.06  Revised for Fundamentals 3.                              }
{   2003/03/08  3.07  Revision and bug fixes.                                  }
{   2003/03/12  3.08  Optimizations.                                           }
{   2003/03/14  3.09  Removed vector based on Int64 values.                    }
{                     Added documentation.                                     }
{   2012/10/26  4.10  Revised for Fundamentals 4.                              }
{   2016/01/17  5.11  Revised for Fundamentals 5.                              }
{                                                                              }
{ Supported compilers:                                                         }
{                                                                              }
{   Delphi 7 Win32                      5.11  2016/01/17                       }
{   Delphi XE7 Win32                    5.11  2016/01/17                       }
{   Delphi XE7 Win64                    5.11  2016/01/17                       }
{                                                                              }
{******************************************************************************}

{$INCLUDE flcMaths.inc}

unit flcVectors;

interface

uses
  { System }
  SysUtils,

  { Fundamentals }
  flcStdTypes,
  flcFloats,
  flcMaths,
  flcDataStructs;



{                                                                              }
{ TVector                                                                      }
{                                                                              }
{   A vector class with mathematical and statistical functions.                }
{                                                                              }
{   Internally the vector stores its values as MFloat type floating-point      }
{   values. The storage functionality is inherited from TBaseVectorArray.      }
{                                                                              }
{   Min and Max return the minimum and maximum vector values. Range is the     }
{   difference between the maximum and minimum values.                         }
{                                                                              }
{   IsZero returns True if all elements in the vector have a zero value.       }
{   HasZero returns True if at least one element has a zero value.             }
{   HasNegative returns True it at least one element has a negative value.     }
{                                                                              }
{   Add, Subtract, Multiply and DotProduct is overloaded to operate on         }
{   Extended and Int64 values.                                                 }
{                                                                              }
{   Normalize divides each element with the Norm of the vector.                }
{                                                                              }
{   Sum returns the sum of all vector elements. SumAndSquares calculates the   }
{   sum of all elements and the sum of each element squared. Likewise for      }
{   SumAndCubes and SumAndQuads.                                               }
{                                                                              }
{   Mean (or average) is the sum of all vector values divided by the number    }
{   of elements in the vector.                                                 }
{                                                                              }
{   Median is the middle-most value.                                           }
{                                                                              }
{   Mode is the most frequently appearing value.                               }
{                                                                              }
{   Variance is a measure of the spread of a distribution about its mean and   }
{   is defined by var(X) = E([X - E(X)]2). The variance is expressed in the    }
{   squared unit of measurement of X.                                          }
{                                                                              }
{   Standard deviation is the square root of the variance and like variance    }
{   is a measure of variability or dispersion of a sample. Standard deviation  }
{   is expressed in the same unit of measurement as the sample values.         }
{                                                                              }
{   StdDev returns the standard deviation of the sample while                  }
{   PopulationStdDev returns the standard deviation of the population.         }
{                                                                              }
{   M1, M2, M3 and M4 are the first four central moments (moments about the    }
{   mean). The second moment about the mean is equal to the variance.          }
{                                                                              }
{   Skewness is the degree of asymmetry about a central value of a             }
{   distribution. A distribution with many small values and few large values   }
{   is positively skewed (right tail), the opposite (left tail) is negatively  }
{   skewed.                                                                    }
{                                                                              }
{   Kurtosis is the degree of peakedness of a distribution, defined as a       }
{   normalized form of the fourth central moment of a distribution. Kurtosis   }
{   is based on the size of a distribution's tails. Distributions with         }
{   relatively large tails are called "leptokurtic"; those with small tails    }
{   are called "platykurtic." A distribution with the same kurtosis as the     }
{   normal distribution is called "mesokurtic."  The kurtosis of a normal      }
{   distribution is 0.                                                         }
{                                                                              }
{   Product returns the product of all vector elements.                        }
{                                                                              }
{   Angle returns the angle in radians between two vectors. Derived from       }
{   the equation: UV = |U| |V| Cos(Angle)                                      }
{                                                                              }
const
  {$IFDEF MFloatIsExtended}
  VectorFloatDelta = ExtendedCompareDelta;
  {$ELSE}
  {$IFDEF MFloatIsDouble}
  VectorFloatDelta = DoubleCompareDelta;
  {$ENDIF}
  {$ENDIF}

type
  {$IFDEF MFloatIsExtended}
  TVectorBaseArray = TExtendedArray;
  {$ELSE}
  {$IFDEF MFloatIsDouble}
  TVectorBaseArray = TDoubleArray;
  {$ENDIF}
  {$ENDIF}

  TVector = class(TVectorBaseArray)
  protected
    { Errors                                                                   }
    procedure CheckVectorSizeMatch(const Size: Integer);

  public
    { AType implementations                                                    }
    class function CreateInstance: AType; override;

    { TVector interface                                                        }
    procedure Add(const V: MFloat); overload;
    procedure Add(const V: PMFloat; const Count: Integer); overload;
    procedure Add(const V: PInt64; const Count: Integer); overload;
    procedure Add(const V: MFloatArray); overload;
    procedure Add(const V: Int64Array); overload;
    procedure Add(const V: TVectorBaseArray); overload;
    procedure Add(const V: TInt64Array); overload;
    procedure Add(Const V: TObject); overload;

    procedure Subtract(const V: MFloat); overload;
    procedure Subtract(const V: PMFloat; const Count: Integer); overload;
    procedure Subtract(const V: PInt64; const Count: Integer); overload;
    procedure Subtract(const V: MFloatArray); overload;
    procedure Subtract(const V: Int64Array); overload;
    procedure Subtract(const V: TVectorBaseArray); overload;
    procedure Subtract(const V: TInt64Array); overload;
    procedure Subtract(Const V: TObject); overload;

    procedure Multiply(const V: MFloat); overload;
    procedure Multiply(const V: PMFloat; const Count: Integer); overload;
    procedure Multiply(const V: PInt64; const Count: Integer); overload;
    procedure Multiply(const V: MFloatArray); overload;
    procedure Multiply(const V: Int64Array); overload;
    procedure Multiply(const V: TVectorBaseArray); overload;
    procedure Multiply(const V: TInt64Array); overload;
    procedure Multiply(const V: TObject); overload;

    function  DotProduct(const V: PMFloat; const Count: Integer): MFloat; overload;
    function  DotProduct(const V: PInt64; const Count: Integer): MFloat; overload;
    function  DotProduct(const V: MFloatArray): MFloat; overload;
    function  DotProduct(const V: Int64Array): MFloat; overload;
    function  DotProduct(const V: TVectorBaseArray): MFloat; overload;
    function  DotProduct(const V: TInt64Array): MFloat; overload;
    function  DotProduct(const V: TObject): MFloat; overload;

    function  Norm: MFloat;
    function  Min: MFloat;
    function  Max: MFloat;
    function  Range(var Min, Max: MFloat): MFloat;

    function  IsZero(const CompareDelta: MFloat = VectorFloatDelta): Boolean;
    function  HasZero(const CompareDelta: MFloat = VectorFloatDelta): Boolean;
    function  HasNegative: Boolean;

    procedure Normalize;
    procedure Negate;
    procedure ValuesInvert;
    procedure ValuesSqr;
    procedure ValuesSqrt;

    function  Sum: MFloat;
    function  SumOfSquares: MFloat;
    procedure SumAndSquares(out Sum, SumOfSquares: MFloat);
    procedure SumAndCubes(out Sum, SumOfSquares, SumOfCubes: MFloat);
    procedure SumAndQuads(out Sum, SumOfSquares, SumOfCubes, SumOfQuads: MFloat);
    function  WeightedSum(const Weights: TVector): MFloat;

    function  Mean: MFloat;
    function  HarmonicMean: MFloat;
    function  GeometricMean: MFloat;
    function  Median: MFloat;
    function  Mode: MFloat;

    function  Variance: MFloat;
    function  StdDev(var Mean: MFloat): MFloat;
    function  PopulationVariance: MFloat;
    function  PopulationStdDev: MFloat;

    function  M1: MFloat;
    function  M2: MFloat;
    function  M3: MFloat;
    function  M4: MFloat;
    function  Skew: MFloat;
    function  Kurtosis: MFloat;

    function  Product: MFloat;
    function  Angle(const V: TVector): MFloat;
  end;



{                                                                              }
{ Exceptions                                                                   }
{                                                                              }
type
  EVector = class(Exception);
  EVectorInvalidSize = class(EVector);
  EVectorInvalidType = class(EVector);
  EVectorEmpty = class(EVector);
  EVectorInvalidValue = class(EVector);
  EVectorDivisionByZero = class(EVector);



{                                                                              }
{ Test cases                                                                   }
{                                                                              }
{$IFDEF MATHS_TEST}
procedure Test;
{$ENDIF}



implementation

uses
  { System }
  Math,
  { Fundamentals }
  flcUtils;



{                                                                              }
{ TVector                                                                      }
{                                                                              }
class function TVector.CreateInstance: AType;
begin
  Result := TVector.Create;
end;

procedure TVector.CheckVectorSizeMatch(const Size: Integer);
begin
  if Size <> FCount then
    raise EVectorInvalidSize.CreateFmt('Vector sizes mismatch (%d, %d)', [FCount, Size]);
end;

procedure TVector.Add(const V: MFloat);
var I : Integer;
    P : PMFloat;
begin
  P := Pointer(FData);
  for I := 0 to FCount - 1 do
    begin
      P^ := P^ + V;
      Inc(P);
    end;
end;

procedure TVector.Add(const V: PMFloat; const Count: Integer);
var I    : Integer;
    P, Q : PMFloat;
begin
  CheckVectorSizeMatch(Count);
  P := Pointer(FData);
  Q := V;
  for I := 0 to Count - 1 do
    begin
      P^ := P^ + Q^;
      Inc(P);
      Inc(Q);
    end;
end;

procedure TVector.Add(const V: PInt64; const Count: Integer);
var I : Integer;
    P : PMFloat;
    Q : PInt64;
begin
  CheckVectorSizeMatch(Count);
  P := Pointer(FData);
  Q := V;
  for I := 0 to Count - 1 do
    begin
      P^ := P^ + Q^;
      Inc(P);
      Inc(Q);
    end;
end;

procedure TVector.Add(const V: MFloatArray);
begin
  Add(PMFloat(V), Length(V));
end;

procedure TVector.Add(const V: Int64Array);
begin
  Add(PInt64(V), Length(V));
end;

procedure TVector.Add(const V: TVectorBaseArray);
begin
  Add(PMFloat(V.Data), V.Count);
end;

procedure TVector.Add(const V: TInt64Array);
begin
  Add(PInt64(V.Data), V.Count);
end;

procedure TVector.Add(const V: TObject);
begin
  if V is TVectorBaseArray then
    Add(TVectorBaseArray(V)) else
  if V is TInt64Array then
    Add(TInt64Array(V))
  else
    raise EVectorInvalidType.CreateFmt('Vector can not add with %s', [ObjectClassName(V)]);
end;

procedure TVector.Subtract(const V: MFloat);
begin
  Add(-V);
end;

procedure TVector.Subtract(const V: PMFloat; const Count: Integer);
var I    : Integer;
    P, Q : PMFloat;
begin
  CheckVectorSizeMatch(Count);
  P := Pointer(FData);
  Q := V;
  for I := 0 to Count - 1 do
    begin
      P^ := P^ - Q^;
      Inc(P);
      Inc(Q);
    end;
end;

procedure TVector.Subtract(const V: PInt64; const Count: Integer);
var I : Integer;
    P : PMFloat;
    Q : PInt64;
begin
  CheckVectorSizeMatch(Count);
  P := Pointer(FData);
  Q := V;
  for I := 0 to Count - 1 do
    begin
      P^ := P^ - Q^;
      Inc(P);
      Inc(Q);
    end;
end;

procedure TVector.Subtract(const V: MFloatArray);
begin
  Subtract(PMFloat(V), Length(V));
end;

procedure TVector.Subtract(const V: Int64Array);
begin
  Subtract(PInt64(V), Length(V));
end;

procedure TVector.Subtract(const V: TVectorBaseArray);
begin
  Subtract(PMFloat(V.Data), V.Count);
end;

procedure TVector.Subtract(const V: TInt64Array);
begin
  Subtract(PInt64(V.Data), V.Count);
end;

procedure TVector.Subtract(const V: TObject);
begin
  if V is TVectorBaseArray then
    Subtract(TVectorBaseArray(V)) else
  if V is TInt64Array then
    Subtract(TInt64Array(V))
  else
    raise EVectorInvalidType.CreateFmt('Vector can not subtract with %s', [ObjectClassName(V)]);
end;

procedure TVector.Multiply(const V: MFloat);
var I : Integer;
    P : PMFloat;
begin
  P := Pointer(FData);
  for I := 0 to FCount - 1 do
    begin
      P^ := P^ * V;
      Inc(P);
    end;
end;

procedure TVector.Multiply(const V: PMFloat; const Count: Integer);
var I    : Integer;
    P, Q : PMFloat;
begin
  CheckVectorSizeMatch(Count);
  P := Pointer(FData);
  Q := V;
  for I := 0 to Count - 1 do
    begin
      P^ := P^ * Q^;
      Inc(P);
      Inc(Q);
    end;
end;

procedure TVector.Multiply(const V: PInt64; const Count: Integer);
var I : Integer;
    P : PMFloat;
    Q : PInt64;
begin
  CheckVectorSizeMatch(Count);
  P := Pointer(FData);
  Q := V;
  for I := 0 to Count - 1 do
    begin
      P^ := P^ * Q^;
      Inc(P);
      Inc(Q);
    end;
end;

procedure TVector.Multiply(const V: MFloatArray);
begin
  Multiply(PMFloat(V), Length(V));
end;

procedure TVector.Multiply(const V: Int64Array);
begin
  Multiply(PInt64(V), Length(V));
end;

procedure TVector.Multiply(const V: TVectorBaseArray);
begin
  Multiply(PMFloat(V.Data), V.Count);
end;

procedure TVector.Multiply(const V: TInt64Array);
begin
  Multiply(PInt64(V.Data), V.Count);
end;

procedure TVector.Multiply(const V: TObject);
begin
  if V is TVectorBaseArray then
    Multiply(TVectorBaseArray(V)) else
  if V is TInt64Array then
    Multiply(TInt64Array(V))
  else
    raise EVectorInvalidType.CreateFmt('Vector can not multiply with %s', [ObjectClassName(V)]);
end;

function TVector.DotProduct(const V: PMFloat; const Count: Integer): MFloat;
var I    : Integer;
    P, Q : PMFloat;
begin
  CheckVectorSizeMatch(Count);
  P := Pointer(FData);
  Q := V;
  Result := 0.0;
  for I := 0 to Count - 1 do
    begin
      Result := Result + P^ * Q^;
      Inc(P);
      Inc(Q);
    end;
end;

function TVector.DotProduct(const V: PInt64; const Count: Integer): MFloat;
var I : Integer;
    P : PMFloat;
    Q : PInt64;
begin
  CheckVectorSizeMatch(Count);
  P := Pointer(FData);
  Q := V;
  Result := 0.0;
  for I := 0 to Count - 1 do
    begin
      Result := Result + P^ * Q^;
      Inc(P);
      Inc(Q);
    end;
end;

function TVector.DotProduct(const V: MFloatArray): MFloat;
begin
  Result := DotProduct(PMFloat(V), Length(V));
end;

function TVector.DotProduct(const V: Int64Array): MFloat;
begin
  Result := DotProduct(PInt64(V), Length(V));
end;

function TVector.DotProduct(const V: TVectorBaseArray): MFloat;
begin
  Result := DotProduct(PMFloat(V.Data), V.Count);
end;

function TVector.DotProduct(const V: TInt64Array): MFloat;
begin
  Result := DotProduct(PInt64(V.Data), V.Count);
end;

function TVector.DotProduct(const V: TObject): MFloat;
begin
  if V is TVectorBaseArray then
    Result := DotProduct(TVectorBaseArray(V)) else
  if V is TInt64Array then
    Result := DotProduct(TInt64Array(V))
  else
    raise EVectorInvalidType.CreateFmt('Vector can not calculate dot product with %s', [ObjectClassName(V)]);
end;

function TVector.Norm: MFloat;
begin
  Result := Sqrt(DotProduct(self));
end;

function TVector.Min: MFloat;
var I : Integer;
    P : PMFloat;
begin
  if FCount = 0 then
    raise EVectorEmpty.Create('No minimum: Vector empty');
  P := Pointer(FData);
  Result := P^;
  Inc(P);
  for I := 1 to FCount - 1 do
    begin
      if P^ < Result then
        Result := P^;
      Inc(P);
    end;
end;

function TVector.Max: MFloat;
var I : Integer;
    P : PMFloat;
begin
  if FCount = 0 then
    raise EVectorEmpty.Create('No maximum: Vector empty');
  P := Pointer(FData);
  Result := P^;
  Inc(P);
  for I := 1 to FCount - 1 do
    begin
      if P^ > Result then
        Result := P^;
      Inc(P);
    end;
end;

function TVector.Range(var Min, Max: MFloat): MFloat;
var I : Integer;
    P : PMFloat;
begin
  if FCount = 0 then
    raise EVectorEmpty.Create('No range: Vector empty');
  P := Pointer(FData);
  Min := P^;
  Max := P^;
  Inc(P);
  for I := 1 to FCount - 1 do
    begin
      if P^ < Min then
        Min := P^ else
        if P^ > Max then
          Max := P^;
      Inc(P);
    end;
  Result := Max - Min;
end;

function TVector.IsZero(const CompareDelta: MFloat): Boolean;
var I : Integer;
    P : PMFloat;
begin
  P := Pointer(FData);
  for I := 0 to FCount - 1 do
    if not FloatZero(P^, CompareDelta) then
      begin
        Result := False;
        exit;
      end else
      Inc(P);
  Result := True;
end;

function TVector.HasZero(const CompareDelta: MFloat): Boolean;
var I : Integer;
    P : PMFloat;
begin
  P := Pointer(FData);
  for I := 0 to FCount - 1 do
    if FloatZero(P^, CompareDelta) then
      begin
        Result := True;
        exit;
      end else
      Inc(P);
  Result := False;
end;

function TVector.HasNegative: Boolean;
var I : Integer;
    P : PMFloat;
begin
  P := Pointer(FData);
  for I := 0 to FCount - 1 do
    if P^ < 0.0 then
      begin
        Result := True;
        exit;
      end else
      Inc(P);
  Result := False;
end;

procedure TVector.Normalize;
var I : Integer;
    P : PMFloat;
    S : MFloat;
begin
  if FCount = 0 then
    exit;
  S := Norm;
  if FloatZero(S, VectorFloatDelta) then
    exit;
  P := Pointer(FData);
  for I := 0 to FCount - 1 do
    begin
      P^ := P^ / S;
      Inc(P);
    end;
end;

procedure TVector.Negate;
var I : Integer;
    P : PMFloat;
begin
  P := Pointer(FData);
  for I := 0 to FCount - 1 do
    begin
      P^ := -P^;
      Inc(P);
    end;
end;

procedure TVector.ValuesInvert;
var I : Integer;
    P : PMFloat;
begin
  P := Pointer(FData);
  for I := 0 to FCount - 1 do
    begin
      if P^ <> 0.0 then
        P^ := 1.0 / P^;
      Inc(P);
    end;
end;

procedure TVector.ValuesSqr;
var I : Integer;
    P : PMFloat;
begin
  P := Pointer(FData);
  for I := 0 to FCount - 1 do
    begin
      P^ := Sqr(P^);
      Inc(P);
    end;
end;

procedure TVector.ValuesSqrt;
var I : Integer;
    P : PMFloat;
begin
  P := Pointer(FData);
  for I := 0 to FCount - 1 do
    begin
      P^ := Sqrt(P^);
      Inc(P);
    end;
end;

function TVector.Sum: MFloat;
var I : Integer;
    P : PMFloat;
begin
  P := Pointer(FData);
  Result := 0.0;
  for I := 0 to FCount - 1 do
    begin
      Result := Result + P^;
      Inc(P);
    end;
end;

function TVector.SumOfSquares: MFloat;
var I : Integer;
    P : PMFloat;
begin
  P := Pointer(FData);
  Result := 0.0;
  for I := 0 to FCount - 1 do
    begin
      Result := Result + Sqr(P^);
      Inc(P);
    end;
end;

procedure TVector.SumAndSquares(out Sum, SumOfSquares: MFloat);
var I : Integer;
    P : PMFloat;
begin
  P := Pointer(FData);
  Sum := 0.0;
  SumOfSquares := 0.0;
  for I := 0 to FCount - 1 do
    begin
      Sum := Sum + P^;
      SumOfSquares := SumOfSquares + Sqr(P^);
      Inc(P);
    end;
end;

procedure TVector.SumAndCubes(out Sum, SumOfSquares, SumOfCubes: MFloat);
var I : Integer;
    P : PMFloat;
    A : MFloat;
begin
  P := Pointer(FData);
  Sum := 0.0;
  SumOfSquares := 0.0;
  SumOfCubes := 0.0;
  for I := 0 to FCount - 1 do
    begin
      Sum := Sum + P^;
      A := Sqr(P^);
      SumOfSquares := SumOfSquares + A;
      A := A * P^;
      SumOfCubes := SumOfCubes + A;
    end;
end;

procedure TVector.SumAndQuads(out Sum, SumOfSquares, SumOfCubes,
    SumOfQuads: MFloat);
var I : Integer;
    P : PMFloat;
    A : MFloat;
begin
  P := Pointer(FData);
  Sum := 0.0;
  SumOfSquares := 0.0;
  SumOfCubes := 0.0;
  SumOfQuads := 0.0;
  for I := 0 to FCount - 1 do
    begin
      Sum := Sum + P^;
      A := Sqr(P^);
      SumOfSquares := SumOfSquares + A;
      A := A * P^;
      SumOfCubes := SumOfCubes + A;
      A := A * P^;
      SumOfQuads := SumOfQuads + A;
    end;
end;

function TVector.WeightedSum(const Weights: TVector): MFloat;
begin
  Result := DotProduct(Weights);
end;

function TVector.Mean: MFloat;
begin
  if FCount = 0 then
    raise EVectorEmpty.Create('No mean: Vector empty');
  Result := Sum / FCount;
end;

function TVector.HarmonicMean: MFloat;
var I : Integer;
    P : PMFloat;
    S : MFloat;
begin
  if FCount = 0 then
    raise EVectorEmpty.Create('No harmonic mean: Vector empty');
  P := Pointer(FData);
  S := 0.0;
  for I := 0 to FCount - 1 do
    begin
      if P^ < 0.0 then
        raise EVectorInvalidValue.Create(
            'No harmonic mean: Vector contains negative values');
      S := S + 1.0 / P^;
      Inc(P);
    end;
  Result := FCount / S;
end;

function TVector.GeometricMean: MFloat;
var I : Integer;
    P : PMFloat;
    S : MFloat;
begin
  if FCount = 0 then
    raise EVectorEmpty.Create('No geometric mean');
  P := Pointer(FData);
  S := 0.0;
  for I := 0 to FCount - 1 do
    begin
      if P^ <= 0.0 then
        raise EVectorInvalidValue.Create(
            'No geometric mean: Vector contains non-positive values');
      S := S + Ln(P^);
    end;
  Result := Exp(S / FCount);
end;

function TVector.Median: MFloat;
var V : TVector;
    I : Integer;
begin
  if FCount = 0 then
    raise EVectorEmpty.Create('No median: Vector empty');
  V := TVector(Duplicate);
  try
    V.Sort;
    I := (FCount - 1) div 2;
    if FCount mod 2 = 0 then
      Result := (V.FData[I] + V.FData[I + 1]) / 2.0
    else
      Result := V.FData[I];
  finally
    V.Free;
  end;
end;

function TVector.Mode: MFloat;
var V         : TVector;
    I         : Integer;
    P         : PMFloat;
    ModeVal   : MFloat;
    ModeCount : Integer;
    CurrVal   : MFloat;
    CurrCount : Integer;
begin
  if FCount = 0 then
    raise EVectorEmpty.Create('No mode: Vector empty');
  V := TVector(Duplicate);
  try
    V.Sort;
    Assert(V.FCount = FCount, 'V.FCount = FCount');
    Assert(V.FCount > 0, 'V.FCount > 0');
    P := Pointer(V.FData);
    ModeVal := P^;
    ModeCount := 0;
    CurrVal := P^;
    CurrCount := 1;
    Inc(P);
    for I := 1 to V.FCount - 1 do
      begin
        if P^ = CurrVal then
          Inc(CurrCount)
        else
          begin
            if CurrCount > ModeCount then
              begin
                ModeVal := CurrVal;
                ModeCount := CurrCount;
              end;
            CurrVal := P^;
            CurrCount := 1;
          end;
        Inc(P);
      end;
    if CurrCount > ModeCount then
      ModeVal := CurrVal;
  finally
    V.Free;
  end;
  Result := ModeVal;
end;

function TVector.Variance: MFloat;
var Sum, SumOfSquares : MFloat;
begin
  if FCount <= 1 then
    Result := 0.0
  else
    begin
      SumAndSquares(Sum, SumOfSquares);
      Result := (SumOfSquares - Sqr(Sum) / FCount) / (FCount - 1);
    end;
end;

function TVector.StdDev(var Mean: MFloat): MFloat;
var S    : MFloat;
    I, N : Integer;
    P    : PMFloat;
begin
  N := FCount;
  if N = 0 then
    begin
      Result := 0.0;
      exit;
    end;
  P := Pointer(FData);
  if N = 1 then
    begin
      Mean := P^;
      Result := P^;
      exit;
    end;
  Mean := self.Mean;
  S := 0.0;
  for I := 0 to N - 1 do
    begin
      S := S + Sqr(P^ - Mean);
      Inc(P);
    end;
  Result := Sqrt(S / (N - 1));
end;

function TVector.PopulationVariance: MFloat;
var Sum, Sum2 : MFloat;
begin
  if FCount = 0 then
    Result := 0.0
  else
    begin
      SumAndSquares(Sum, Sum2);
      Result := (Sum2 - Sqr(Sum) / FCount) / FCount;
    end;
end;

function TVector.PopulationStdDev: MFloat;
begin
  Result := Sqrt(PopulationVariance);
end;

function TVector.M1: MFloat;
begin
  Result := Sum / (FCount + 1.0);
end;

function TVector.M2: MFloat;
var Sum, Sum2, NI : MFloat;
begin
  SumAndSquares(Sum, Sum2);
  NI     := 1.0 / (FCount + 1.0);
  Result := (Sum2 * NI)
          - Sqr(Sum * NI);
end;

function TVector.M3: MFloat;
var Sum, Sum2, Sum3 : MFloat;
    NI, M1          : MFloat;
begin
  SumAndCubes(Sum, Sum2, Sum3);
  NI     := 1.0 / (FCount + 1.0);
  M1     := Sum * NI;
  Result := (Sum3 * NI)
          - (M1 * 3.0 * Sum2 * NI)
          + (2.0 * Sqr(M1) * M1);
end;

function TVector.M4: MFloat;
var Sum, Sum2, Sum3, Sum4 : MFloat;
    NI, M1, M1Sqr         : MFloat;
begin
  SumAndQuads(Sum, Sum2, Sum3, Sum4);
  NI     := 1.0 / (FCount + 1.0);
  M1     := Sum * NI;
  M1Sqr  := Sqr(M1);
  Result := (Sum4 * NI)
          - (M1 * 4.0 * Sum3 * NI)
          + (M1Sqr * 6.0 * Sum2 * NI)
          - (3.0 * Sqr(M1Sqr));
end;

function TVector.Skew: MFloat;
var Sum, Sum2, Sum3     : MFloat;
    M1, M2, M3          : MFloat;
    M1Sqr, S2N, S3N, NI : MFloat;
begin
  SumAndCubes(Sum, Sum2, Sum3);
  NI     := 1.0 / (FCount + 1.0);
  M1     := Sum * NI;
  M1Sqr  := Sqr(M1);
  S2N    := Sum2 * NI;
  S3N    := Sum3 * NI;
  M2     := S2N - M1Sqr;
  M3     := S3N
          - (M1 * 3.0 * S2N)
          + (2.0 * M1Sqr * M1);
  Result := M3 * Power(M2, -3/2);
end;

function TVector.Kurtosis: MFloat;
var Sum, Sum2, Sum3, Sum4    : MFloat;
    M1, M2, M4, M1Sqr, M2Sqr : MFloat;
    S2N, S3N, NI             : MFloat;
begin
  SumAndQuads(Sum, Sum2, Sum3, Sum4);
  NI     := 1.0 / (FCount + 1.0);
  M1     := Sum * NI;
  M1Sqr  := Sqr(M1);
  S2N    := Sum2 * NI;
  S3N    := Sum3 * NI;
  M2     := S2N - M1Sqr;
  M2Sqr  := Sqr(M2);
  M4     := (Sum4 * NI)
          - (M1 * 4.0 * S3N)
          + (M1Sqr * 6.0 * S2N)
          - (3.0 * Sqr(M1Sqr));
  if FloatZero(M2Sqr, VectorFloatDelta) then
    raise EVectorDivisionByZero.Create('Kurtosis: Division by zero');
  Result := M4 / M2Sqr;
end;

function TVector.Product: MFloat;
var I : Integer;
    P : PMFloat;
begin
  P := Pointer(FData);
  Result := 1.0;
  for I := 0 to FCount - 1 do
    begin
      Result := Result * P^;
      Inc(P);
    end;
end;

function TVector.Angle(const V: TVector): MFloat;
begin
  Assert(Assigned(V), 'Assigned(V)');
  Result := ArcCos(DotProduct(V) / (Norm * V.Norm));
end;



{                                                                              }
{ Test cases                                                                   }
{                                                                              }
{$IFDEF MATHS_TEST}
{$ASSERTIONS ON}
procedure Test;
var A, B : TVector;
begin
  A := TVector.Create;
  B := TVector.Create;

  Assert(A.Count = 0);
  Assert(A.IsZero);

  A.AppendItem(1.0);
  A.AppendItem(2.0);
  A.AppendItem(3.0);

  Assert(A.Count = 3);
  Assert(A[0] = 1.0);
  Assert(A[1] = 2.0);
  Assert(A[2] = 3.0);

  Assert(A.Sum = 6.0);
  Assert(A.Min = 1.0);
  Assert(A.Max = 3.0);
  Assert(not A.IsZero);
  Assert(A.Median = 2.0);
  Assert(A.Mean = 2.0);
  Assert(A.Product = 6.0);
  Assert(Abs(A.Norm - Sqrt(14.0)) < 1e-10);

  B.Assign(A);
  Assert(B.Sum = 6.0);

  B.Add(A);
  Assert(B.Sum = 12.0);

  A.Clear;
  Assert(A.Count = 0);

  A.AppendItem(4.0);
  A.AppendItem(10.0);
  A.AppendItem(1.0);

  Assert(A.Count = 3);
  Assert(A[0] = 4.0);
  Assert(A[1] = 10.0);
  Assert(A[2] = 1.0);

  Assert(A.Sum = 15.0);
  Assert(A.Min = 1.0);
  Assert(A.Max = 10.0);
  Assert(not A.IsZero);
  Assert(A.Median = 4.0);
  Assert(A.Mean = 5.0);
  Assert(A.Product = 40.0);
  Assert(Abs(A.Norm - Sqrt(117.0)) < 1e-10);

  B.Free;
  A.Free;
end;
{$ENDIF}



end.

