{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals 5.00                                        }
{   File name:        flcRational.pas                                          }
{   File version:     5.09                                                     }
{   Description:      Rational numbers                                         }
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
{   1999/11/26  0.01  Initial version.                                         }
{   1999/12/23  0.02  Fixed bug in CalcFrac.                                   }
{   2001/05/21  0.03  Moved rational class to unit cMaths.                     }
{   2002/06/01  0.04  Moved rational class to unit cRational.                  }
{   2003/02/16  3.05  Revised for Fundamentals 3.                              }
{   2003/05/25  3.06  Fixed bug in Subtract. Reported by Karl Hans.            }
{   2003/05/26  3.07  Fixed bug in Sgn and revised unit.                       }
{                     Added self testing code.                                 }
{   2012/10/26  4.08  Revised for Fundamentals 4.                              }
{   2016/01/17  5.09  Revised for Fundamentals 5.                              }
{                                                                              }
{ Supported compilers:                                                         }
{                                                                              }
{   Delphi 7 Win32                      5.09  2016/01/17                       }
{   Delphi XE7 Win32                    5.09  2016/01/17                       }
{   Delphi XE7 Win64                    5.09  2016/01/17                       }
{                                                                              }
{******************************************************************************}

{$INCLUDE flcMaths.inc}

unit flcRational;

interface

uses
  { System }
  SysUtils,

  { Fundamentals }
  flcStdTypes,
  flcMaths;



{                                                                              }
{ Rational number object                                                       }
{   Represents a rational number (Numerator / Denominator pair).               }
{                                                                              }
type
  TRational = class
  private
    FT, FN : Int64;  { FT = Numerator / FN = Denominator }

  protected
    procedure DenominatorZeroError;
    procedure DivisionByZeroError;
    procedure Simplify;

    procedure SetDenominator(const Denominator: Int64);

    function  GetAsString: String;
    {$IFDEF SupportRawByteString}
    function  GetAsStringB: RawByteString;
    {$ENDIF}
    function  GetAsStringU: UnicodeString;

    procedure SetAsString(const S: String);
    {$IFDEF SupportRawByteString}
    procedure SetAsStringB(const S: RawByteString);
    {$ENDIF}
    procedure SetAsStringU(const S: UnicodeString);

    function  GetAsFloat: MFloat;
    procedure SetAsFloat(const R: MFloat);

  public
    constructor Create; overload;
    constructor Create(const Numerator: Int64;
                const Denominator: Int64 = 1); overload;
    constructor Create(const R: Extended); overload;

    property  Numerator: Int64 read FT write FT;
    property  Denominator: Int64 read FN write SetDenominator;

    property  AsString: String read GetAsString write SetAsString;
    {$IFDEF SupportRawByteString}
    property  AsStringB: RawByteString read GetAsStringB write SetAsStringB;
    {$ENDIF}
    property  AsStringU: UnicodeString read GetAsStringU write SetAsStringU;

    property  AsFloat: MFloat read GetAsFloat write SetAsFloat;

    function  Duplicate: TRational;

    procedure Assign(const R: TRational); overload;
    procedure Assign(const R: MFloat); overload;
    procedure Assign(const Numerator: Int64;
              const Denominator: Int64 = 1); overload;
    procedure AssignZero;
    procedure AssignOne;

    function  IsEqual(const R: TRational): Boolean; overload;
    function  IsEqual(const Numerator: Int64;
              const Denominator: Int64 = 1): Boolean; overload;
    function  IsEqual(const R: Extended): Boolean; overload;
    function  IsZero: Boolean;
    function  IsOne: Boolean;

    procedure Add(const R: TRational); overload;
    procedure Add(const V: Extended); overload;
    procedure Add(const V: Int64); overload;

    procedure Subtract(const R: TRational); overload;
    procedure Subtract(const V: Extended); overload;
    procedure Subtract(const V: Int64); overload;

    procedure Negate;
    procedure Abs;
    function  Sgn: Integer;

    procedure Multiply(const R: TRational); overload;
    procedure Multiply(const V: Extended); overload;
    procedure Multiply(const V: Int64); overload;

    procedure Divide(const R: TRational); overload;
    procedure Divide(const V: Extended); overload;
    procedure Divide(const V: Int64); overload;

    procedure Reciprocal;
    procedure Sqrt;
    procedure Sqr;
    procedure Power(const R: TRational); overload;
    procedure Power(const V: Int64); overload;
    procedure Power(const V: Extended); overload;
    procedure Exp;
    procedure Ln;
    procedure Sin;
    procedure Cos;
  end;
  ERational = class(Exception);
  ERationalDivByZero = class(ERational);



{                                                                              }
{ Tests                                                                        }
{                                                                              }
{$IFDEF MATHS_TEST}
procedure Test;
{$ENDIF}



implementation

uses
  { System }
  Math,

  { Fundamentals }
  flcUtils,
  flcStrings,
  flcFloats;



{                                                                              }
{ Rational helper functions                                                    }
{                                                                              }
procedure SimplifyRational(var T, N: Int64);
var I : Int64;
begin
  Assert(N <> 0);

  if N < 0 then // keep the denominator positive
    begin
      T := -T;
      N := -N;
    end;
  if T = 0 then // always represent zero as 0/1
    begin
      N := 1;
      exit;
    end;
  if (T = 1) or (N = 1) then // already simplified
    exit;
  I := GCD(T, N);
  Assert(I > 0);
  T := T div I;
  N := N div I;
end;



{                                                                              }
{ TRational                                                                    }
{                                                                              }
constructor TRational.Create;
begin
  inherited Create;
  AssignZero;
end;

constructor TRational.Create(const Numerator, Denominator: Int64);
begin
  inherited Create;
  Assign(Numerator, Denominator);
end;

constructor TRational.Create(const R: Extended);
begin
  inherited Create;
  Assign(R);
end;

procedure TRational.DenominatorZeroError;
begin
  raise ERational.Create('Invalid rational number: Denominator=0');
end;

procedure TRational.DivisionByZeroError;
begin
  raise ERationalDivByZero.Create('Division by zero');
end;

procedure TRational.Simplify;
begin
  SimplifyRational(FT, FN);
end;

procedure TRational.SetDenominator(const Denominator: Int64);
begin
  if Denominator = 0 then
    DenominatorZeroError;
  FN := Denominator;
end;

procedure TRational.Assign(const Numerator, Denominator: Int64);
begin
  if Denominator = 0 then
    DenominatorZeroError;
  FT := Numerator;
  FN := Denominator;
  Simplify;
end;

procedure TRational.Assign(const R: TRational);
begin
  Assert(Assigned(R));

  FT := R.FT;
  FN := R.FN;
end;

{ See http://forum.swarthmore.edu/dr.math/faq/faq.fractions.html for an        }
{ explanation on how to convert decimal numbers to fractions.                  }
const
  CalcFracMaxLevel = 12;
  CalcFracAccuracy = Int64(1000000000); // 1.0E+9
  CalcFracDelta    = 1.0 / (CalcFracAccuracy * 10); // 1.0E-10
  RationalEpsilon  = CalcFracDelta; // 1.0E-10

procedure TRational.Assign(const R: MFloat);

  function CalcFrac(const R: MFloat; const Level: Integer = 1): TRational;
  var I : Extended;
  begin
    Assert(System.Abs(R) < 1.0);

    if FloatZero(R, CalcFracDelta) or (Level = CalcFracMaxLevel) then
      // Return zero. If Level = CalcFracMaxLevel then the result is an
      // approximation.
      Result := TRational.Create
    else
    if FloatsEqual(R, 1.0, CalcFracDelta) then
      Result := TRational.Create(1, 1) // Return 1
    else
      begin
        I := R * CalcFracAccuracy;
        if System.Abs(I) < 1.0 then // terminating decimal
          Result := TRational.Create(Round(I), CalcFracAccuracy)
        else
          begin // recursive process
            I := 1.0 / R;
            Result := CalcFrac(Frac(I), Level + 1);
            {$IFDEF DELPHI5}
            Result.Add(Trunc(I));
            {$ELSE}
            Result.Add(Int64(Trunc(I)));
            {$ENDIF}
            Result.Reciprocal;
          end;
      end;
  end;

var T : TRational;
    Z : Int64;

begin
  T := CalcFrac(Frac(R));
  Z := Trunc(R);
  T.Add(Z);
  Assign(T);
  T.Free;
end;

procedure TRational.AssignOne;
begin
  FT := 1;
  FN := 1;
end;

procedure TRational.AssignZero;
begin
  FT := 0;
  FN := 1;
end;

function TRational.IsEqual(const Numerator, Denominator: Int64): Boolean;
var T, N : Int64;
begin
  T := Numerator;
  N := Denominator;
  SimplifyRational(T, N);
  Result := (FT = T) and (FN = N);
end;

function TRational.IsEqual(const R: TRational): Boolean;
begin
  Assert(Assigned(R));

  Result := (FT = R.FT) and (FN = R.FN);
end;

function TRational.IsEqual(const R: Extended): Boolean;
begin
  Result := FloatApproxEqual(R, GetAsFloat, RationalEpsilon);
end;

function TRational.IsOne: Boolean;
begin
  Result := (FT = 1) and (FN = 1);
end;

function TRational.IsZero: Boolean;
begin
  Result := FT = 0;
end;

function TRational.Duplicate: TRational;
begin
  Result := TRational.Create(FT, FN);
end;

function TRational.GetAsString: String;
begin
  Result := IntToString(FT) + '/' + IntToString(FN);
end;

{$IFDEF SupportRawByteString}
function TRational.GetAsStringB: RawByteString;
begin
  Result := IntToStringB(FT) + '/' + IntToStringB(FN);
end;
{$ENDIF}

function TRational.GetAsStringU: UnicodeString;
begin
  Result := IntToStringU(FT) + '/' + IntToStringU(FN);
end;

procedure TRational.SetAsString(const S: String);
var F : Integer;
begin
  F := PosStr('/', S);
  if F = 0 then
    Assign(StringToFloat(S))
  else
    Assign(StringToInt(Copy(S, 1, F - 1)), StringToInt(CopyFrom(S, F + 1)));
end;

{$IFDEF SupportRawByteString}
procedure TRational.SetAsStringB(const S: RawByteString);
var F : Integer;
begin
  F := PosStrB('/', S);
  if F = 0 then
    Assign(StringToFloatB(S))
  else
    Assign(StringToIntB(Copy(S, 1, F - 1)), StringToIntB(CopyFromB(S, F + 1)));
end;
{$ENDIF}

procedure TRational.SetAsStringU(const S: UnicodeString);
var F : Integer;
begin
  F := PosStrU('/', S);
  if F = 0 then
    Assign(StringToFloatU(S))
  else
    Assign(StringToIntU(Copy(S, 1, F - 1)), StringToIntU(CopyFromU(S, F + 1)));
end;

function TRational.GetAsFloat: MFloat;
begin
  Result := FT / FN;
end;

procedure TRational.SetAsFloat(const R: MFloat);
begin
  Assign(R);
end;

procedure TRational.Add(const R: TRational);
begin
  Assert(Assigned(R));

  FT := FT * R.FN + R.FT * FN;
  FN := FN * R.FN;
  Simplify;
end;

procedure TRational.Add(const V: Int64);
begin
  Inc(FT, FN * V);
end;

procedure TRational.Add(const V: Extended);
begin
  Assign(GetAsFloat + V);
end;

procedure TRational.Subtract(const V: Extended);
begin
  Assign(GetAsFloat - V);
end;

procedure TRational.Subtract(const R: TRational);
begin
  Assert(Assigned(R));

  FT := FT * R.FN - R.FT * FN;
  FN := FN * R.FN;
  Simplify;
end;

procedure TRational.Subtract(const V: Int64);
begin
  Dec(FT, FN * V);
end;

procedure TRational.Negate;
begin
  FT := -FT;
end;

procedure TRational.Abs;
begin
  FT := System.Abs(FT);
  FN := System.Abs(FN);
end;

function TRational.Sgn: Integer;
begin
  if FT < 0 then
    if FN >= 0 then
      Result := -1
    else
      Result := 1
  else if FT > 0 then
    if FN >= 0 then
      Result := 1
    else
      Result := -1
  else
    Result := 0;
end;

procedure TRational.Divide(const V: Int64);
begin
  if V = 0 then
    DivisionByZeroError;
  FN := FN * V;
  Simplify;
end;

procedure TRational.Divide(const R: TRational);
begin
  Assert(Assigned(R));

  if R.FT = 0 then
    DivisionByZeroError;
  FT := FT * R.FN;
  FN := FN * R.FT;
  Simplify;
end;

procedure TRational.Divide(const V: Extended);
begin
  Assign(GetAsFloat / V);
end;

procedure TRational.Reciprocal;
begin
  if FT = 0 then
    DivisionByZeroError;
  Swap(FT, FN);
end;

procedure TRational.Multiply(const R: TRational);
begin
  Assert(Assigned(R));

  FT := FT * R.FT;
  FN := FN * R.FN;
  Simplify;
end;

procedure TRational.Multiply(const V: Int64);
begin
  FT := FT * V;
  Simplify;
end;

procedure TRational.Multiply(const V: Extended);
begin
  Assign(GetAsFloat * V);
end;

procedure TRational.Power(const R: TRational);
begin
  Assert(Assigned(R));

  Assign(Math.Power(GetAsFloat, R.GetAsFloat));
end;

procedure TRational.Power(const V: Int64);
var T, N : MFloat;
begin
  T := FT;
  N := FN;
  FT := Round(IntPower(T, V));
  FN := Round(IntPower(N, V));
end;

procedure TRational.Power(const V: Extended);
begin
  Assign(Math.Power(FT, V) / Math.Power(FN, V));
end;

procedure TRational.Sqrt;
begin
  Assign(System.Sqrt(FT / FN));
end;

procedure TRational.Sqr;
begin
  FT := System.Sqr(FT);
  FN := System.Sqr(FN);
end;

procedure TRational.Exp;
begin
  Assign(System.Exp(FT / FN));
end;

procedure TRational.Ln;
begin
  Assign(System.Ln(FT / FN));
end;

procedure TRational.Sin;
begin
  Assign(System.Sin(FT / FN));
end;

procedure TRational.Cos;
begin
  Assign(System.Cos(FT / FN));
end;



{                                                                              }
{ Tests                                                                        }
{                                                                              }
{$IFDEF MATHS_TEST}
{$ASSERTIONS ON}
procedure Test;
var R, S, T : TRational;
begin
  R := TRational.Create;
  S := TRational.Create(1, 2);
  try
    Assert(R.Numerator = 0);
    Assert(R.Denominator = 1);
    Assert(R.AsString = '0/1');
    Assert(R.AsFloat = 0.0);
    Assert(R.IsZero);
    Assert(not R.IsOne);
    Assert(R.Sgn = 0);

    Assert(S.AsString = '1/2');
    Assert(S.Numerator = 1);
    Assert(S.Denominator = 2);
    Assert(S.AsFloat = 0.5);
    Assert(not S.IsZero);
    Assert(not S.IsEqual(R));
    Assert(S.IsEqual(1, 2));
    Assert(S.IsEqual(2, 4));

    R.Assign(1, 3);
    R.Add(S);
    Assert(R.AsString = '5/6');
    Assert(S.AsString = '1/2');

    R.Reciprocal;
    Assert(R.AsString = '6/5');

    R.Assign(1, 2);
    S.Assign(1, 3);
    R.Subtract(S);
    Assert(R.AsString = '1/6');
    Assert(R.Sgn = 1);

    R.Negate;
    Assert(R.Sgn = -1);
    Assert(R.AsString = '-1/6');

    T := R.Duplicate;
    Assert(Assigned(T));
    Assert(T <> R);
    Assert(T.AsString = '-1/6');
    Assert(T.IsEqual(R));
    T.Free;

    R.Assign(2, 3);
    S.Assign(5, 2);
    R.Multiply(S);
    Assert(R.AsString = '5/3');
    R.Divide(S);
    Assert(R.AsString = '2/3');
    R.Sqr;
    Assert(R.AsString = '4/9');
    R.Sqrt;
    Assert(R.AsString = '2/3');
    R.Exp;
    R.Ln;
    Assert(R.AsString = '2/3');
    R.Power(3);
    Assert(R.AsString = '8/27');

    S.Assign(1, 3);
    R.Power(S);
    Assert(R.AsString = '2/3');

    R.AsFloat := 0.5;
    Assert(R.AsString = '1/2');
    Assert(R.AsFloat = 0.5);
    Assert(R.IsEqual(0.5));

    R.AsFloat := 1.8;
    Assert(R.AsString = '9/5');
    Assert(Abs(R.AsFloat - 1.8) < 1.0e-9);
    Assert(R.IsEqual(1.8));

    R.AsString := '11/12';
    Assert(R.AsString = '11/12');
    Assert(R.Numerator = 11);
    Assert(R.Denominator = 12);

    R.AsString := '12/34';
    Assert(R.AsString = '6/17');
  finally
    S.Free;
    R.Free;
  end;
end;
{$ENDIF}



end.

