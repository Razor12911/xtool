{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals 5.00                                        }
{   File name:        flcMatrix.pas                                            }
{   File version:     5.12                                                     }
{   Description:      Matrix                                                   }
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
{   1999/09/27  0.01  Added TMatrix, TVector.                                  }
{   1999/10/03  0.02  Improvements.                                            }
{   1999/11/27  0.03  Added TMatrix.Normalise, TMatrix.Multiply (Row, Value)   }
{   2000/10/03  0.04  Fixed bug in TMatrix.Transposed reported by Jingyi Peng  }
{   2002/06/01  0.05  Created cMatrix unit from cMaths.                        }
{   2003/02/16  3.06  Revised for Fundamentals 3.                              }
{   2003/03/14  3.07  Improvements and documentation.                          }
{   2003/05/27  3.08  Fixed bug in SolveMatrix reported by Karl Hans           }
{   2003/12/20  3.09  Fixed bug in SetRow reported by Mats Josefson and        }
{                     others.                                                  }
{   2012/10/26  4.10  Revised for Fundamentals 4.                              }
{   2016/01/17  5.11  Revised for Fundamentals 5.                              }
{   2016/01/17  5.12  Change CreateIdentity constructor.                       }
{                                                                              }
{ Supported compilers:                                                         }
{                                                                              }
{   Delphi XE7 Win32                    5.12  2016/01/17                       }
{   Delphi XE7 Win64                    5.12  2016/01/17                       }
{                                                                              }
{******************************************************************************}

{$INCLUDE flcMaths.inc}

unit flcMatrix;

interface

uses
  { System }
  SysUtils,

  { Fundamentals }
  flcUtils,
  flcMaths,
  flcVectors;



{                                                                              }
{ Matrix                                                                       }
{                                                                              }
{   CreateIdentity(N) constructs a square matrix and places the unity vector   }
{   in the diagonal, ie the diagonal elements are all 1.0.                     }
{   CreateDiagonal(D) constructs a square matrix and places the elements of    }
{   vector D in the diagonal.                                                  }
{                                                                              }
{   SetSize changes the dimensions of the matrix.                              }
{   Clear resets the dimensions to 0 x 0.                                      }
{                                                                              }
{   AssignZero keeps the current dimensions, but sets all elements to zero.    }
{   AssignIdentity sets the diagional to 1's and the rest to 0's.              }
{                                                                              }
{   Trace is calculated as the sum of the diagonal.                            }
{                                                                              }
{   Transpose called on a square matrix returns a new copy of the matrix,      }
{   but with the rows swapped with the columns.                                }
{                                                                              }
{   Multiply(M) performs matrix multiplication. It returns a new matrix        }
{   with the result.                                                           }
{                                                                              }
{   Normalise(M) makes the diagonal 1's by multiplying each row with a         }
{   factor. M is optional, and if specified, all operations performed during   }
{   normalisation are also performed on M.                                     }
{                                                                              }
{   IsOrtogonal returns True if X'X = I, ie the transposed X, multiplied by X  }
{   is equal to the identity matrix.                                           }
{                                                                              }
{   IsIdempotent returns True if XX = X, ie the matrix is unaffected if        }
{   multiplied by itself.                                                      }
{                                                                              }
{   SolveMatrix returns the determinant.                                       }
{                                                                              }
{   Inverse sets the matrix to Y where XY = I, ie a matrix multiplied by its   }
{   inverse is equal to the identity matrix.                                   }
{                                                                              }
type
  TMatrix = class
  protected
    FColCount : Integer;
    FRows     : array of MFloatArray;

    procedure CheckValidRowIndex(const Row: Integer);
    procedure CheckValidColIndex(const Col: Integer);
    procedure CheckValidIndex(const Row, Col: Integer);
    procedure CheckSquare;
    procedure SizeMismatchError;

    function  GetRowCount: Integer;
    procedure SetRowCount(const NewRowCount: Integer);
    procedure SetColCount(const NewColCount: Integer);

    function  GetItem(const Row, Col: Integer): MFloat;
    procedure SetItem(const Row, Col: Integer; const Value: MFloat);

    function  GetAsString: String; virtual;
    {$IFDEF SupportRawByteString}
    function  GetAsStringB: RawByteString; virtual;
    {$ENDIF}
    function  GetAsStringU: UnicodeString; virtual;

    procedure AddRows(const I, J: Integer; const Factor: MFloat);
    procedure SwapRows(const I, J: Integer);

  public
    constructor CreateSize(const RowCount, ColCount: Integer);
    constructor CreateSquare(const N: Integer; const Identity: Boolean);
    constructor CreateDiagonal(const D: TVector);

    property  ColCount: Integer read FColCount write SetColCount;
    property  RowCount: Integer read GetRowCount write SetRowCount;
    procedure SetSize(const RowCount, ColCount: Integer);
    procedure Clear;

    property  Item[const Row, Col: Integer]: MFloat read GetItem write SetItem; default;

    procedure AssignZero;
    procedure AssignIdentity;
    procedure Assign(const Value: MFloat); overload;
    procedure Assign(const M: TMatrix); overload;
    procedure Assign(const V: TVector); overload;

    function  Duplicate: TMatrix; overload;
    function  DuplicateRange(const R1, C1, R2, C2: Integer): TMatrix; overload;
    function  DuplicateRow(const Row: Integer): TVector;
    function  DuplicateCol(const Col: Integer): TVector;
    function  DuplicateDiagonal: TVector;

    function  IsEqual(const M: TMatrix): Boolean; overload;
    function  IsEqual(const V: TVector): Boolean; overload;

    function  IsSquare: Boolean;
    function  IsZero: Boolean;
    function  IsIdentity: Boolean;

    property  AsString: String read GetAsString;
    {$IFDEF SupportRawByteString}
    property  AsStringB: RawByteString read GetAsStringB;
    {$ENDIF}
    property  AsStringU: UnicodeString read GetAsStringU;
    function  Trace: MFloat;

    procedure SetRow(const Row: Integer; const V: TVector); overload;
    procedure SetRow(const Row: Integer; const Values: array of MFloat); overload;
    procedure SetCol(const Col: Integer; const V: TVector);

    function  Transpose: TMatrix;
    procedure TransposeInPlace;

    procedure Add(const M: TMatrix);
    procedure Subtract(const M: TMatrix);
    procedure Negate;

    procedure MultiplyRow(const Row: Integer; const Value: MFloat);
    procedure Multiply(const Value: MFloat); overload;
    function  Multiply(const M: TMatrix): TMatrix; overload;
    procedure MultiplyInPlace(const M: TMatrix);

    function  IsOrtogonal: Boolean;
    function  IsIdempotent: Boolean;

    function  Normalise(const M: TMatrix = nil): MFloat;
    function  SolveMatrix(var M: TMatrix): MFloat;
    function  Determinant: MFloat;
    function  Inverse: TMatrix;
    procedure InverseInPlace;
    function  SolveLinearSystem(const V: TVector): TVector;
  end;
  EMatrix = class(Exception);



{                                                                              }
{ Self testing code                                                            }
{                                                                              }
{$IFDEF MATHS_TEST}
procedure Test;
{$ENDIF}



implementation

uses
  { Fundamentals }
  flcDynArrays,
  flcFloats;



{                                                                              }
{ TMatrix                                                                      }
{                                                                              }
constructor TMatrix.CreateSize(const RowCount, ColCount: Integer);
begin
  inherited Create;
  SetSize(RowCount, ColCount);
end;

constructor TMatrix.CreateSquare(const N: Integer; const Identity: Boolean);
var I : Integer;
begin
  inherited Create;
  SetSize(N, N);
  if Identity then
    for I := 0 to N - 1 do
      FRows[I, I] := 1.0;
end;

constructor TMatrix.CreateDiagonal(const D: TVector);
var I, N : Integer;
begin
  inherited Create;
  Assert(Assigned(D), 'Assigned(D)');
  N := D.Count;
  SetSize(N, N);
  for I := 0 to N - 1 do
    FRows[I, I] := D.Data[I];
end;

procedure TMatrix.CheckValidRowIndex(const Row: Integer);
begin
  if (Row < 0) or (Row >= Length(FRows)) then
    raise EMatrix.Create('Matrix index out of bounds');
end;

procedure TMatrix.CheckValidColIndex(const Col: Integer);
begin
  if (Col < 0) or (Col >= FColCount) then
    raise EMatrix.Create('Matrix index out of bounds');
end;

procedure TMatrix.CheckValidIndex(const Row, Col: Integer);
begin
  if (Row < 0) or (Row >= Length(FRows)) or
     (Col < 0) or (Col >= FColCount) then
    raise EMatrix.Create('Matrix index out of bounds');
end;

procedure TMatrix.CheckSquare;
begin
  if not IsSquare then
    raise EMatrix.Create('Not a square matrix');
end;

procedure TMatrix.SizeMismatchError;
begin
  raise EMatrix.Create('Matrix size mismatch');
end;

function TMatrix.GetRowCount: Integer;
begin
  Result := Length(FRows);
end;

procedure TMatrix.SetRowCount(const NewRowCount: Integer);
var I, N, P : Integer;
begin
  P := Length(FRows);
  N := NewRowCount;
  if N < 0 then
    N := 0;
  if P = N then
    exit;
  // Resize
  SetLength(FRows, N);
  for I := P to N - 1 do
    SetLengthAndZero(FRows[I], FColCount);
end;

procedure TMatrix.SetColCount(const NewColCount: Integer);
var I, N : Integer;
begin
  N := NewColCount;
  if N < 0 then
    N := 0;
  if FColCount = N then
    exit;
  // Resize
  for I := 0 to Length(FRows) - 1 do
    SetLengthAndZero(FRows[I], N);
  FColCount := N;
end;

procedure TMatrix.SetSize(const RowCount, ColCount: Integer);
begin
  SetRowCount(RowCount);
  SetColCount(ColCount);
end;

procedure TMatrix.Clear;
begin
  SetSize(0, 0);
end;

function TMatrix.GetItem(const Row, Col: Integer): MFloat;
begin
  CheckValidIndex(Row, Col);
  Result := FRows[Row, Col];
end;

procedure TMatrix.SetItem(const Row, Col: Integer; const Value: MFloat);
begin
  CheckValidIndex(Row, Col);
  FRows[Row, Col] := Value;
end;

procedure TMatrix.AssignZero;
var I : Integer;
begin
  if FColCount = 0 then
    exit;
  for I := 0 to Length(FRows) - 1 do
    FillChar(FRows[I, 0], FColCount * Sizeof(MFloat), #0);
end;

procedure TMatrix.AssignIdentity;
var I, N : Integer;
begin
  CheckSquare;
  N := Length(FRows);
  if N = 0 then
    exit;
  AssignZero;
  for I := 0 to N - 1 do
    FRows[I, I] := 1.0;
end;

procedure TMatrix.Assign(const Value: MFloat);
var I, J : Integer;
begin
  for I := 0 to Length(FRows) - 1 do
    for J := 0 to FColCount - 1 do
      FRows[I, J] := Value;
end;

procedure TMatrix.Assign(const M: TMatrix);
var I : Integer;
begin
  Assert(Assigned(M), 'Assigned(M)');
  SetSize(M.RowCount, M.ColCount);
  if FColCount = 0 then
    exit;
  for I := 0 to Length(FRows) - 1 do
    FRows[I] := Copy(M.FRows[I]);
end;

procedure TMatrix.Assign(const V: TVector);
var N: Integer;
begin
  Assert(Assigned(V), 'Assigned(V)');
  N := V.Count;
  SetSize(1, N);
  if N = 0 then
    exit;
  FRows[0] := Copy(V.Data, 0, N);
end;

function TMatrix.Duplicate: TMatrix;
begin
  Result := TMatrix.Create;
  try
    Result.Assign(self);
  except
    Result.Free;
    raise;
  end;
end;

function TMatrix.DuplicateRange(const R1, C1, R2, C2: Integer): TMatrix;
var I, T, L, B, R, W : Integer;
begin
  Result := TMatrix.Create;
  try
    T := MaxInt(R1, 0);
    B := MinInt(R2, Length(FRows));
    L := MaxInt(C1, 0);
    R := MinInt(C2, FColCount);
    if (T > B) or (L > R) then // range has no size
      exit;
    W := R - L + 1;
    Result.SetSize(B - T + 1, W);
    for I := T to B do
      Result.FRows[I - T] := Copy(FRows[I], L, W);
  except
    Result.Free;
    raise;
  end;
end;

function TMatrix.DuplicateRow(const Row: Integer): TVector;
begin
  CheckValidRowIndex(Row);
  Result := TVector.Create(Copy(FRows[Row]));
end;

function TMatrix.DuplicateCol(const Col: Integer): TVector;
var I, N : Integer;
begin
  CheckValidColIndex(Col);
  Result := TVector.Create;
  try
    N := Length(FRows);
    Result.Count := N;
    for I := 0 to N - 1 do
      Result.Data[I] := FRows[I, Col];
  except
    Result.Free;
    raise;
  end;
end;

function TMatrix.DuplicateDiagonal: TVector;
var I, N : Integer;
begin
  CheckSquare;
  Result := TVector.Create;
  try
    N := Length(FRows);
    Result.Count := N;
    for I := 0 to N - 1 do
      Result.Data[I] := FRows[I, I];
  except
    Result.Free;
    raise;
  end;
end;

function TMatrix.IsEqual(const M: TMatrix): Boolean;
var I, J : Integer;
begin
  Assert(Assigned(M), 'Assigned(M)');
  if (Length(FRows) <> Length(M.FRows)) or (FColCount <> M.FColCount) then
    begin
      Result := False;
      exit;
    end;
  for I := 0 to Length(FRows) - 1 do
    for J := 0 to FColCount - 1 do
      if not FloatApproxEqual(FRows[I, J], M.FRows[I, J]) then
        begin
          Result := False;
          exit;
        end;
  Result := True;
end;

function TMatrix.IsEqual(const V: TVector): Boolean;
var I : Integer;
begin
  Assert(Assigned(V), 'Assigned(V)');
  if (Length(FRows) <> 1) or (V.Count <> FColCount) then
    begin
      Result := False;
      exit;
    end;
  for I := 0 to FColCount - 1 do
    if not FloatApproxEqual(V.Data[I], FRows[0, I]) then
      begin
        Result := False;
        exit;
      end;
  Result := True;
end;

function TMatrix.IsSquare: Boolean;
begin
  Result := Length(FRows) = FColCount;
end;

function TMatrix.IsZero: Boolean;
var I, J : Integer;
begin
  for I := 0 to Length(FRows) - 1 do
    for J := 0 to FColCount - 1 do
      if not FloatZero(FRows[I, J]) then
        begin
          Result := False;
          exit;
        end;
  Result := True;
end;

function TMatrix.IsIdentity: Boolean;
var I, J : Integer;
    R    : MFloat;
begin
  if not IsSquare then
    begin
      Result := False;
      exit;
    end;
  for I := 0 to Length(FRows) - 1 do
    for J := 0 to FColCount - 1 do
      begin
        R := FRows[I, J];
        if ((J = I) and not FloatApproxEqual(R, 1.0)) or
           ((J <> I) and not FloatZero(R)) then
          begin
            Result := False;
            exit;
          end;
      end;
  Result := True;
end;

function TMatrix.GetAsString: String;
var I, J, R : Integer;
begin
  Result := '(';
  R := Length(FRows);
  for I := 0 to R - 1 do
    begin
      Result := Result + '(';
      for J := 0 to FColCount - 1 do
        begin
          Result := Result + FloatToString(FRows[I, J]);
          if J < FColCount - 1 then
            Result := Result + ',';
        end;
      Result := Result + ')';
      if I < R - 1 then
        Result := Result + ',';
    end;
  Result := Result + ')';
end;

{$IFDEF SupportRawByteString}
function TMatrix.GetAsStringB: RawByteString;
var I, J, R : Integer;
begin
  Result := '(';
  R := Length(FRows);
  for I := 0 to R - 1 do
    begin
      Result := Result + '(';
      for J := 0 to FColCount - 1 do
        begin
          Result := Result + FloatToStringB(FRows[I, J]);
          if J < FColCount - 1 then
            Result := Result + ',';
        end;
      Result := Result + ')';
      if I < R - 1 then
        Result := Result + ',';
    end;
  Result := Result + ')';
end;
{$ENDIF}

function TMatrix.GetAsStringU: UnicodeString;
var I, J, R : Integer;
begin
  Result := '(';
  R := Length(FRows);
  for I := 0 to R - 1 do
    begin
      Result := Result + '(';
      for J := 0 to FColCount - 1 do
        begin
          Result := Result + FloatToStringU(FRows[I, J]);
          if J < FColCount - 1 then
            Result := Result + ',';
        end;
      Result := Result + ')';
      if I < R - 1 then
        Result := Result + ',';
    end;
  Result := Result + ')';
end;

function TMatrix.Trace: MFloat;
var I : Integer;
begin
  CheckSquare;
  Result := 0.0;
  for I := 0 to Length(FRows) - 1 do
    Result := Result + FRows[I, I];
end;

procedure TMatrix.SetRow(const Row: Integer; const V: TVector);
begin
  CheckValidRowIndex(Row);
  Assert(Assigned(V), 'Assigned(V)');
  if FColCount <> V.Count then
    SizeMismatchError;
  FRows[Row] := Copy(V.Data, 0, FColCount);
end;

procedure TMatrix.SetRow(const Row: Integer; const Values: array of MFloat);
var I : Integer;
begin
  CheckValidRowIndex(Row);
  if FColCount <> Length(Values) then
    SizeMismatchError;
  for I := 0 to FColCount - 1 do
    FRows[Row, I] := Values[I];
end;

procedure TMatrix.SetCol(const Col: Integer; const V: TVector);
var I, N : Integer;
begin
  CheckValidColIndex(Col);
  Assert(Assigned(V), 'Assigned(V)');
  N := Length(FRows);
  if N <> V.Count then
    SizeMismatchError;
  for I := 0 to N - 1 do
    FRows[I, Col] := V.Data[I];
end;

function TMatrix.Transpose: TMatrix;
var I, J : Integer;
begin
  Result := TMatrix.CreateSize(FColCount, Length(FRows));
  try
    for I := 0 to FColCount - 1 do
      for J := 0 to Length(FRows) - 1 do
        Result.FRows[I, J] := FRows[J, I];
  except
    Result.Free;
    raise;
  end;
end;

procedure TMatrix.TransposeInPlace;
var M : TMatrix;
begin
  M := Transpose;
  try
    FColCount := M.FColCount;
    FRows := M.FRows;
  finally
    M.Free;
  end;
end;

procedure TMatrix.Add(const M: TMatrix);
var R, C, I, J : Integer;
    P, Q       : PMFloat;
begin
  Assert(Assigned(M), 'Assigned(M)');
  R := Length(FRows);
  C := FColCount;
  if (M.RowCount <> R) or (M.ColCount <> C) then
    SizeMismatchError;
  if C = 0 then
    exit;
  for I := 0 to R - 1 do
    begin
      P := @FRows[I, 0];
      Q := @M.FRows[I, 0];
      for J := 0 to C - 1 do
        begin
          P^ := P^ + Q^;
          Inc(P);
          Inc(Q);
        end;
    end;
end;

procedure TMatrix.Subtract(const M: TMatrix);
var R, C, I, J : Integer;
    P, Q       : PMFloat;
begin
  Assert(Assigned(M), 'Assigned(M)');
  R := Length(FRows);
  C := FColCount;
  if (M.RowCount <> R) or (M.ColCount <> C) then
    SizeMismatchError;
  if C = 0 then
    exit;
  for I := 0 to R - 1 do
    begin
      P := @FRows[I, 0];
      Q := @M.FRows[I, 0];
      for J := 0 to C - 1 do
        begin
          P^ := P^ - Q^;
          Inc(P);
          Inc(Q);
        end;
    end;
end;

procedure TMatrix.Negate;
var I, J, C : Integer;
    P       : PMFloat;
begin
  C := FColCount;
  if C = 0 then
    exit;
  for I := 0 to Length(FRows) - 1 do
    begin
      P := @FRows[I, 0];
      for J := 0 to C - 1 do
        begin
          P^ := -P^;
          Inc(P);
        end;
    end;
end;

procedure TMatrix.MultiplyRow(const Row: Integer; const Value: MFloat);
var I, C : Integer;
    P    : PMFloat;
begin
  CheckValidRowIndex(Row);
  C := FColCount;
  if C = 0 then
    exit;
  P := @FRows[Row, 0];
  for I := 0 to C - 1 do
    begin
      P^ := P^ * Value;
      Inc(P);
    end;
end;

procedure TMatrix.Multiply(const Value: MFloat);
var I : Integer;
begin
  for I := 0 to Length(FRows) - 1 do
    MultiplyRow(I, Value);
end;

function TMatrix.Multiply(const M: TMatrix): TMatrix;
var I, J, K : Integer;
    C, R, D : Integer;
    A       : MFloat;
    P, Q    : PMFloat;
begin
  Assert(Assigned(M), 'Assigned(M)');
  C := FColCount;
  if C <> M.RowCount then
    SizeMismatchError;
  R := Length(FRows);
  D := M.ColCount;
  Result := TMatrix.CreateSize(R, D);
  try
    if (C = 0) or (D = 0) then
      exit;
    for I := 0 to R - 1 do
      begin
        Q := @Result.FRows[I, 0];
        for J := 0 to D - 1 do
          begin
            A := 0.0;
            P := @FRows[I, 0];
            for K := 0 to C - 1 do
              begin
                A := A + P^ * M.FRows[K, J];
                Inc(P);
              end;
            Q^ := A;
            Inc(Q);
          end;
      end;
  except
    Result.Free;
    raise;
  end;
end;

procedure TMatrix.MultiplyInPlace(const M: TMatrix);
var R : TMatrix;
begin
  Assert(Assigned(M), 'Assigned(M)');
  R := Multiply(M);
  try
    FColCount := R.FColCount;
    FRows := R.FRows;
  finally
    R.Free;
  end;
end;

function TMatrix.IsOrtogonal: Boolean;
var M : TMatrix;
begin
  M := Transpose;
  try
    M.MultiplyInPlace(self);
    Result := M.IsIdentity;
  finally
    M.Free;
  end;
end;

function TMatrix.IsIdempotent: Boolean;
var M : TMatrix;
begin
  M := Multiply(self);
  try
    Result := M.IsEqual(self);
  finally
    M.Free;
  end;
end;

function TMatrix.Normalise(const M: TMatrix): MFloat;
var I : Integer;
    R : MFloat;
begin
  CheckSquare;
  Result := 1.0;
  for I := 0 to Length(FRows) - 1 do
    begin
      R := FRows[I, I];
      Result := Result * R;
      if not FloatZero(R) then
        begin
          R := 1.0 / R;
          MultiplyRow(I, R);
          if Assigned(M) then
            M.MultiplyRow(I, R);
        end;
    end;
end;

procedure TMatrix.AddRows(const I, J: Integer; const Factor: MFloat);
var F, C : Integer;
    P, Q : PMFloat;
begin
  CheckValidRowIndex(I);
  CheckValidRowIndex(J);
  C := FColCount;
  if C = 0 then
    exit;
  P := @FRows[I, 0];
  Q := @FRows[J, 0];
  for F := 0 to C - 1 do
    begin
      P^ := P^ + Q^ * Factor;
      Inc(P);
      Inc(Q);
    end;
end;

procedure TMatrix.SwapRows(const I, J: Integer);
var P : MFloatArray;
begin
  CheckValidRowIndex(I);
  CheckValidRowIndex(J);
  // Swap row references
  P := FRows[I];
  FRows[I] := FRows[J];
  FRows[J] := P;
end;

function TMatrix.SolveMatrix(var M: TMatrix): MFloat;
var I, J, L : Integer;
    E, D    : MFloat;
    P       : PMFloat;
begin
  Assert(Assigned(M), 'Assigned(M)');
  Assert(IsSquare, 'IsSquare');
  Result := 1.0;
  L := Length(FRows);
  for I := 0 to L - 1 do
    begin
      J := I;
      P := @FRows[I, 0];
      while J < L do
        if not FloatZero(P^) then
          break
        else
          begin
            Inc(J);
            Inc(P);
          end;
      if J = L then
        begin
          Result := 0.0;
          exit;
        end;
      if J <> I then
        begin
          SwapRows(I, J);
          Result := -Result;
          M.SwapRows(I, J);
        end;
      D := FRows[I, I];
      for J := I + 1 to L - 1 do
        begin
          E := -(FRows[J, I] / D);
          AddRows(J, I, E);
          M.AddRows(J, I, E);
        end;
    end;
  for I := L - 1 downto 0 do
    begin
      D := FRows[I, I];
      for J := I - 1 downto 0 do
        begin
          E := -(FRows[J, I] / D);
          AddRows(J, I, E);
          M.AddRows(J, I, E);
        end;
    end;
  Result := Result * Normalise(M);
end;

function TMatrix.Determinant: MFloat;
var A, B : TMatrix;
begin
  CheckSquare;
  A := Duplicate;
  try
    B := TMatrix.CreateSquare(Length(FRows), True);
    try
      Result := A.SolveMatrix(B);
    finally
      B.Free;
    end;
  finally
    A.Free;
  end;
end;

function TMatrix.Inverse: TMatrix;
var R : Integer;
begin
  CheckSquare;
  R := Length(FRows);
  Result := TMatrix.CreateSquare(R, True);
  try
    if SolveMatrix(Result) = 0.0 then
      raise EMatrix.Create('Matrix can not invert');
  except
    Result.Free;
    raise;
  end;
end;

procedure TMatrix.InverseInPlace;
var A : TMatrix;
begin
  A := Inverse;
  try
    FColCount := A.FColCount;
    FRows := A.FRows;
  finally
    A.Free;
  end;
end;

function TMatrix.SolveLinearSystem(const V: TVector): TVector;
var C, M : TMatrix;
begin
  Assert(Assigned(V), 'Assigned(V)');
  if not IsSquare or (V.Count <> Length(FRows)) then
    raise EMatrix.Create('Not a linear system');
  Result := nil;
  C := Duplicate;
  try
    M := TMatrix.Create;
    try
      M.Assign(V);
      if C.SolveMatrix(M) = 0.0 then
        raise EMatrix.Create('Can not solve this system');
      Result := M.DuplicateRow(0);
    finally
      M.Free;
    end;
  finally
    C.Free;
  end;
end;



{                                                                              }
{ Self testing code                                                            }
{                                                                              }
{$IFDEF MATHS_TEST}
{$ASSERTIONS ON}
procedure Test;
var A, B : TMatrix;
begin
  A := TMatrix.CreateSquare(3, True);
  B := TMatrix.CreateSquare(3, False);

  Assert(B.IsSquare);
  Assert(B.IsZero);
  Assert(not B.IsIdentity);
  Assert(B.GetAsString = '((0,0,0),(0,0,0),(0,0,0))');

  Assert(A.IsSquare);
  Assert(not A.IsZero);
  Assert(A.IsIdentity);
  Assert(A.GetAsString = '((1,0,0),(0,1,0),(0,0,1))');

  Assert(A.ColCount = 3);
  Assert(A.RowCount = 3);
  Assert(A[0, 0] = 1.0);
  Assert(A[0, 1] = 0.0);
  Assert(A[0, 2] = 0.0);
  Assert(A[1, 0] = 0.0);
  Assert(A[1, 1] = 1.0);
  Assert(A[1, 2] = 0.0);
  Assert(A[2, 0] = 0.0);
  Assert(A[2, 1] = 0.0);
  Assert(A[2, 2] = 1.0);

  B[0, 0] := 1.0;
  B[0, 1] := 2.0;
  B[0, 2] := 3.0;
  B[1, 0] := 4.0;
  B[1, 1] := 5.0;
  B[1, 2] := 6.0;
  B[2, 0] := 7.0;
  B[2, 1] := 8.0;
  B[2, 2] := 9.0;

  Assert(not B.IsZero);
  Assert(not B.IsIdentity);

  A.Add(B);
  Assert(A.GetAsString = '((2,2,3),(4,6,6),(7,8,10))');

  A.Subtract(B);
  Assert(A.GetAsString = '((1,0,0),(0,1,0),(0,0,1))');

  B.Free;
  A.Free;
end;
{$ENDIF}



end.

