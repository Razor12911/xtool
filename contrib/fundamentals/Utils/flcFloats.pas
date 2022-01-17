{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals 5.00                                        }
{   File name:        flcFloats.pas                                            }
{   File version:     5.04                                                     }
{   Description:      Floating point types utility functions.                  }
{                                                                              }
{   Copyright:        Copyright (c) 2003-2020, David J Butler                  }
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
{   2003/03/14  3.01  Added FloatZero, FloatsEqual and FloatsCompare.          }
{   2018/07/11  5.02  Move to flcFloats unit from flcUtils.                    }
{   2018/08/12  5.03  String type changes.                                     }
{   2020/06/02  5.04  Float type changes.                                      }
{                                                                              }
{ Supported compilers:                                                         }
{                                                                              }
{   Delphi 2010-10.4 Win32/Win64        5.04  2020/06/02                       }
{   Delphi 10.2-10.4 Linux64            5.04  2020/06/02                       }
{   FreePascal 3.0.4 Win64              5.04  2020/06/02                       }
{                                                                              }
{******************************************************************************}

{$INCLUDE ..\flcInclude.inc}

{$IFDEF FREEPASCAL}
  {$WARNINGS OFF}
  {$HINTS OFF}
{$ENDIF}

{$IFDEF DEBUG}
{$IFDEF TEST}
  {$DEFINE FLOATS_TEST}
{$ENDIF}
{$ENDIF}

unit flcFloats;

interface

uses
  { Fundamentals }
  flcStdTypes,
  flcUtils;



{                                                                              }
{ Float functions                                                              }
{                                                                              }

{ Min returns smallest of A and B                                              }
{ Max returns greatest of A and B                                              }
{ Clip returns Value if in Low..High range, otherwise Low or High              }
function  DoubleMin(const A, B: Double): Double; {$IFDEF UseInline}inline;{$ENDIF}
function  DoubleMax(const A, B: Double): Double; {$IFDEF UseInline}inline;{$ENDIF}

function  ExtendedMin(const A, B: Extended): Extended; {$IFDEF UseInline}inline;{$ENDIF}
function  ExtendedMax(const A, B: Extended): Extended; {$IFDEF UseInline}inline;{$ENDIF}

function  FloatMin(const A, B: Float): Float; {$IFDEF UseInline}inline;{$ENDIF}
function  FloatMax(const A, B: Float): Float; {$IFDEF UseInline}inline;{$ENDIF}
function  FloatClip(const Value: Float; const Low, High: Float): Float;

{ InXXXRange returns True if A in range of type XXX                            }
function  InSingleRange(const A: Float): Boolean; {$IFDEF UseInline}inline;{$ENDIF}
function  InDoubleRange(const A: Float): Boolean; {$IFDEF UseInline}inline;{$ENDIF}
function  InCurrencyRange(const A: Float): Boolean; overload;
function  InCurrencyRange(const A: Int64): Boolean; overload;

{ ExtendedExponent returns the exponent component of an Extended value         }
{$IFNDEF ExtendedIsDouble}
function  ExtendedExponentBase2(const A: Extended; var Exponent: Integer): Boolean;
function  ExtendedExponentBase10(const A: Extended; var Exponent: Integer): Boolean;
{$ENDIF}

{ ExtendedIsInfinity is True if A is a positive or negative infinity.          }
{ ExtendedIsNaN is True if A is Not-a-Number.                                  }
{$IFNDEF ExtendedIsDouble}
function  ExtendedIsInfinity(const A: Extended): Boolean;
function  ExtendedIsNaN(const A: Extended): Boolean;
{$ENDIF}



{                                                                              }
{ Approximate comparison of floating point values                              }
{                                                                              }
{   FloatZero, FloatOne, FloatsEqual and FloatsCompare are functions for       }
{   comparing floating point numbers based on a fixed CompareDelta difference  }
{   between the values. This means that values are considered equal if the     }
{   unsigned difference between the values are less than CompareDelta.         }
{                                                                              }
const
  // Minimum CompareDelta values for the different floating point types:
  // The values were chosen to be slightly higher than the minimum value that
  // the floating-point type can store.
  SingleCompareDelta   = 1.0E-34;
  DoubleCompareDelta   = 1.0E-280;
  {$IFDEF ExtendedIsDouble}
  ExtendedCompareDelta = DoubleCompareDelta;
  {$ELSE}
  ExtendedCompareDelta = 1.0E-4400;
  {$ENDIF}

  // Default CompareDelta is set to SingleCompareDelta. This allows any type
  // of floating-point value to be compared with any other.
  DefaultCompareDelta = SingleCompareDelta;

function  FloatZero(const A: Float;
          const CompareDelta: Float = DefaultCompareDelta): Boolean;
function  FloatOne(const A: Float;
          const CompareDelta: Float = DefaultCompareDelta): Boolean;

function  FloatsEqual(const A, B: Float;
          const CompareDelta: Float = DefaultCompareDelta): Boolean;
function  FloatsCompare(const A, B: Float;
          const CompareDelta: Float = DefaultCompareDelta): TCompareResult;



{                                                                              }
{ Scaled approximate comparison of floating point values                       }
{                                                                              }
{   ApproxEqual and ApproxCompare are functions for comparing floating point   }
{   numbers based on a scaled order of magnitude difference between the        }
{   values. CompareEpsilon is the ratio applied to the largest of the two      }
{   exponents to give the maximum difference (CompareDelta) for comparison.    }
{                                                                              }
{   For example:                                                               }
{                                                                              }
{   When the CompareEpsilon is 1.0E-9, the result of                           }
{                                                                              }
{   ApproxEqual(1.0E+20, 1.000000001E+20) = False, but the result of           }
{   ApproxEqual(1.0E+20, 1.0000000001E+20) = True, ie the first 9 digits of    }
{   the mantissas of the values must be the same.                              }
{                                                                              }
{   Note that for A <> 0.0, the value of ApproxEqual(A, 0.0) will always be    }
{   False. Rather use the unscaled FloatZero, FloatsEqual and FloatsCompare    }
{   functions when specifically testing for zero.                              }
{                                                                              }
const
  // Smallest (most sensitive) CompareEpsilon values allowed for the different
  // floating point types:
  SingleCompareEpsilon   = 1.0E-5;
  DoubleCompareEpsilon   = 1.0E-13;
  ExtendedCompareEpsilon = 1.0E-17;

  // Default CompareEpsilon is set for half the significant digits of the
  // Extended type.
  DefaultCompareEpsilon  = 1.0E-10;

{$IFNDEF ExtendedIsDouble}
function  ExtendedApproxEqual(const A, B: Extended;
          const CompareEpsilon: Double = DefaultCompareEpsilon): Boolean;
function  ExtendedApproxCompare(const A, B: Extended;
          const CompareEpsilon: Double = DefaultCompareEpsilon): TCompareResult;
{$ENDIF}

function  DoubleApproxEqual(const A, B: Double;
          const CompareEpsilon: Double = DefaultCompareEpsilon): Boolean;
function  DoubleApproxCompare(const A, B: Double;
          const CompareEpsilon: Double = DefaultCompareEpsilon): TCompareResult;

function  FloatApproxEqual(const A, B: Float;
          const CompareEpsilon: Float = DefaultCompareEpsilon): Boolean;
function  FloatApproxCompare(const A, B: Float;
          const CompareEpsilon: Float = DefaultCompareEpsilon): TCompareResult;



{                                                                              }
{ Float-String conversions                                                     }
{                                                                              }
{$IFDEF SupportAnsiString}
function  FloatToStringA(const A: Float): AnsiString;
{$ENDIF}
function  FloatToStringB(const A: Float): RawByteString;
function  FloatToStringU(const A: Float): UnicodeString;
function  FloatToString(const A: Float): String;

function  TryStringToFloatPA(const BufP: Pointer; const BufLen: Integer; out Value: Float; out StrLen: Integer): TConvertResult;
function  TryStringToFloatPW(const BufP: Pointer; const BufLen: Integer; out Value: Float; out StrLen: Integer): TConvertResult;
function  TryStringToFloatP(const BufP: Pointer; const BufLen: Integer; out Value: Float; out StrLen: Integer): TConvertResult;

function  TryStringToFloatB(const A: RawByteString; out B: Float): Boolean;
function  TryStringToFloatU(const A: UnicodeString; out B: Float): Boolean;
function  TryStringToFloat(const A: String; out B: Float): Boolean;

function  StringToFloatB(const A: RawByteString): Float;
function  StringToFloatU(const A: UnicodeString): Float;
function  StringToFloat(const A: String): Float;

function  StringToFloatDefB(const A: RawByteString; const Default: Float): Float;
function  StringToFloatDefU(const A: UnicodeString; const Default: Float): Float;
function  StringToFloatDef(const A: String; const Default: Float): Float;



{                                                                              }
{ Tests                                                                        }
{                                                                              }
{$IFDEF FLOATS_TEST}
procedure Test;
{$ENDIF}



implementation

uses
  { System }
  SysUtils,
  Math;



{                                                                              }
{ Real                                                                         }
{                                                                              }
function DoubleMin(const A, B: Double): Double;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

function DoubleMax(const A, B: Double): Double;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;

function ExtendedMin(const A, B: Extended): Extended;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

function ExtendedMax(const A, B: Extended): Extended;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;

function FloatMin(const A, B: Float): Float;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

function FloatMax(const A, B: Float): Float;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;

function FloatClip(const Value: Float; const Low, High: Float): Float;
begin
  if Value < Low then
    Result := Low else
  if Value > High then
    Result := High
  else
    Result := Value;
end;

function InSingleRange(const A: Float): Boolean;
var B : Float;
begin
  B := Abs(A);
  Result := (B >= MinSingle) and (B <= MaxSingle);
end;

function InDoubleRange(const A: Float): Boolean;
var B : Float;
begin
  B := Abs(A);
  Result := (B >= MinDouble) and (B <= MaxDouble);
end;

function InCurrencyRange(const A: Float): Boolean;
begin
  Result := (A >= MinCurrency) and (A <= MaxCurrency);
end;

function InCurrencyRange(const A: Int64): Boolean;
begin
  Result := (A >= MinCurrency) and (A <= MaxCurrency);
end;

{$IFNDEF ExtendedIsDouble}
function ExtendedExponentBase2(const A: Extended; var Exponent: Integer): Boolean;
var RecA : ExtendedRec absolute A;
    ExpA : Word;
begin
  ExpA := RecA.Exponent and $7FFF;
  if ExpA = $7FFF then // A is NaN, Infinity, ...
    begin
      Exponent := 0;
      Result := False;
    end
  else
    begin
      Exponent := Integer(ExpA) - 16383;
      Result := True;
    end;
end;

function ExtendedExponentBase10(const A: Extended; var Exponent: Integer): Boolean;
const Log2_10 = 3.32192809488736; // Log2(10)
begin
  Result := ExtendedExponentBase2(A, Exponent);
  if Result then
    Exponent := Round(Exponent / Log2_10);
end;
{$ENDIF}

{$IFNDEF ExtendedIsDouble}
function ExtendedIsInfinity(const A: Extended): Boolean;
var Ext : ExtendedRec absolute A;
begin
  if Ext.Exponent and $7FFF <> $7FFF then
    Result := False
  else
    Result := (Ext.Mantissa[1] = $80000000) and (Ext.Mantissa[0] = 0);
end;

function ExtendedIsNaN(const A: Extended): Boolean;
var Ext : ExtendedRec absolute A;
begin
  if Ext.Exponent and $7FFF <> $7FFF then
    Result := False
  else
    Result := (Ext.Mantissa[1] <> $80000000) or (Ext.Mantissa[0] <> 0)
end;
{$ENDIF}

{                                                                              }
{ Approximate comparison                                                       }
{                                                                              }
function FloatZero(const A: Float; const CompareDelta: Float): Boolean;
begin
  Assert(CompareDelta >= 0.0);
  Result := Abs(A) <= CompareDelta;
end;

function FloatOne(const A: Float; const CompareDelta: Float): Boolean;
begin
  Assert(CompareDelta >= 0.0);
  Result := Abs(A - 1.0) <= CompareDelta;
end;

function FloatsEqual(const A, B: Float; const CompareDelta: Float): Boolean;
begin
  Assert(CompareDelta >= 0.0);
  Result := Abs(A - B) <= CompareDelta;
end;

function FloatsCompare(const A, B: Float; const CompareDelta: Float): TCompareResult;
var D : Float;
begin
  Assert(CompareDelta >= 0.0);
  D := A - B;
  if Abs(D) <= CompareDelta then
    Result := crEqual else
  if D >= CompareDelta then
    Result := crGreater
  else
    Result := crLess;
end;



{$IFNDEF ExtendedIsDouble}
{                                                                              }
{ Scaled approximate comparison                                                }
{                                                                              }
{   The ApproxEqual and ApproxCompare functions were taken from the freeware   }
{   FltMath unit by Tempest Software, as taken from Knuth, Seminumerical       }
{   Algorithms, 2nd ed., Addison-Wesley, 1981, pp. 217-220.                    }
{                                                                              }
function ExtendedApproxEqual(const A, B: Extended; const CompareEpsilon: Double): Boolean;
var ExtA : ExtendedRec absolute A;
    ExtB : ExtendedRec absolute B;
    ExpA : Word;
    ExpB : Word;
    Exp  : ExtendedRec;
begin
  ExpA := ExtA.Exponent and $7FFF;
  ExpB := ExtB.Exponent and $7FFF;
  if (ExpA = $7FFF) and
     ((ExtA.Mantissa[1] <> $80000000) or (ExtA.Mantissa[0] <> 0)) then
    { A is NaN }
    Result := False else
  if (ExpB = $7FFF) and
     ((ExtB.Mantissa[1] <> $80000000) or (ExtB.Mantissa[0] <> 0)) then
    { B is NaN }
    Result := False else
  if (ExpA = $7FFF) or (ExpB = $7FFF) then
    { A or B is infinity. Use the builtin comparison, which will       }
    { properly account for signed infinities, comparing infinity with  }
    { infinity, or comparing infinity with a finite value.             }
    Result := A = B else
  begin
    { We are comparing two finite values, so take the difference and   }
    { compare that against the scaled Epsilon.                         }
    Exp.Value := 1.0;
    if ExpA < ExpB then
      Exp.Exponent := ExpB
    else
      Exp.Exponent := ExpA;
    Result := Abs(A - B) <= (CompareEpsilon * Exp.Value);
  end;
end;

function ExtendedApproxCompare(const A, B: Extended; const CompareEpsilon: Double): TCompareResult;
var ExtA : ExtendedRec absolute A;
    ExtB : ExtendedRec absolute B;
    ExpA : Word;
    ExpB : Word;
    Exp  : ExtendedRec;
    D, E : Extended;
begin
  ExpA := ExtA.Exponent and $7FFF;
  ExpB := ExtB.Exponent and $7FFF;
  if (ExpA = $7FFF) and
     ((ExtA.Mantissa[1] <> $80000000) or (ExtA.Mantissa[0] <> 0)) then
    { A is NaN }
    Result := crUndefined else
  if (ExpB = $7FFF) and
     ((ExtB.Mantissa[1] <> $80000000) or (ExtB.Mantissa[0] <> 0)) then
    { B is NaN }
    Result := crUndefined else
  if (ExpA = $7FFF) or (ExpB = $7FFF) then
    { A or B is infinity. Use the builtin comparison, which will       }
    { properly account for signed infinities, comparing infinity with  }
    { infinity, or comparing infinity with a finite value.             }
    Result := Compare(A, B) else
  begin
    { We are comparing two finite values, so take the difference and   }
    { compare that against the scaled Epsilon.                         }
    Exp.Value := 1.0;
    if ExpA < ExpB then
      Exp.Exponent := ExpB
    else
      Exp.Exponent := ExpA;
    E := CompareEpsilon * Exp.Value;
    D := A - B;
    if Abs(D) <= E then
      Result := crEqual else
    if D >= E then
      Result := crGreater
    else
      Result := crLess;
  end;
end;
{$ENDIF}

// Knuth: approximatelyEqual
// return fabs(a - b) <= ( (fabs(a) < fabs(b) ? fabs(b) : fabs(a)) * epsilon);
function DoubleApproxEqual(const A, B: Double; const CompareEpsilon: Double): Boolean;
var AbsA, AbsB, R : Float;
begin
  AbsA := Abs(A);
  AbsB := Abs(B);
  if AbsA < AbsB then
    R := AbsB
  else
    R := AbsA;
  R := R * CompareEpsilon;
  Result := Abs(A - B) <= R;
end;

function DoubleApproxCompare(const A, B: Double; const CompareEpsilon: Double): TCompareResult;
var AbsA, AbsB, R, D : Float;
begin
  AbsA := Abs(A);
  AbsB := Abs(B);
  if AbsA < AbsB then
    R := AbsB
  else
    R := AbsA;
  R := R * CompareEpsilon;
  D := A - B;
  if Abs(D) <= R then
    Result := crEqual
  else
  if D < 0 then
    Result := crLess
  else
    Result := crGreater;
end;

function FloatApproxEqual(const A, B: Float; const CompareEpsilon: Float): Boolean;
var AbsA, AbsB, R : Float;
begin
  AbsA := Abs(A);
  AbsB := Abs(B);
  if AbsA < AbsB then
    R := AbsB
  else
    R := AbsA;
  R := R * CompareEpsilon;
  Result := Abs(A - B) <= R;
end;

function FloatApproxCompare(const A, B: Float; const CompareEpsilon: Float): TCompareResult;
var AbsA, AbsB, R, D : Float;
begin
  AbsA := Abs(A);
  AbsB := Abs(B);
  if AbsA < AbsB then
    R := AbsB
  else
    R := AbsA;
  R := R * CompareEpsilon;
  D := A - B;
  if Abs(D) <= R then
    Result := crEqual
  else
  if D < 0 then
    Result := crLess
  else
    Result := crGreater;
end;



{                                                                              }
{ Float-String conversions                                                     }
{                                                                              }
function FloatToStringS(const A: Float): String;
var B : Float;
    {$IFNDEF SupportShortString}
    S : String;
    {$ELSE}
    S : ShortString;
    {$ENDIF}
    L, I, J, C : Integer;
    E : Integer;
begin
  // handle special floating point values
  {$IFNDEF ExtendedIsDouble}
  if ExtendedIsInfinity(A) or ExtendedIsNaN(A) then
    begin
      Result := '';
      exit;
    end;
  {$ENDIF}
  B := Abs(A);
  // very small numbers (Double precision) are zero
  if B < 1e-300 then
    begin
      Result := '0';
      exit;
    end;
  // up to 15 digits (around Double precsion) before or after decimal use non-scientific notation
  if (B < 1e-15) or (B >= 1e+15) then
    Str(A, S)
  else
    Str(A:0:15, S);
  // trim preceding spaces
  I := 1;
  while S[I] = ' ' do
    Inc(I);
  if I > 1 then
    S := Copy(S, I, Length(S) - I + 1);
  // find exponent
  L := Length(S);
  E := 0;
  for I := 1 to L do
    if S[I] = 'E' then
      begin
        E := I;
        break;
      end;
  if E = 0 then
    begin
      // trim trailing zeros
      I := L;
      while S[I] = '0' do
        Dec(I);
      if S[I] = '.' then
        Dec(I);
      if I < L then
        SetLength(S, I);
    end
  else
    begin
      // trim preceding zeros in exponent
      I := E + 2;
      J := I;
      while (I <= L) and
            (S[I] = '0') do
        Inc(I);
      C := I - J;
      if C > 0 then
        begin
          Delete(S, J, C);
          Dec(L, C);
        end;
      // trim trailing zeros in mantissa
      I := E - 1;
      while S[I] = '0' do
        Dec(I);
      if S[I] = '.' then
        Dec(I);
      if I < E - 1 then
        S := Copy(S, 1, I) + Copy(S, E, L - E + 1);
    end;
  // return formatted float string
  {$IFDEF SupportShortString}
  Result := String(S);
  {$ELSE}
  Result := S;
  {$ENDIF}
end;

{$IFDEF SupportAnsiString}
function FloatToStringA(const A: Float): AnsiString;
begin
  Result := AnsiString(FloatToStringS(A));
end;
{$ENDIF}

function FloatToStringB(const A: Float): RawByteString;
begin
  Result := RawByteString(FloatToStringS(A));
end;

function FloatToStringU(const A: Float): UnicodeString;
begin
  Result := UnicodeString(FloatToStringS(A));
end;

function FloatToString(const A: Float): String;
begin
  Result := String(FloatToStringS(A));
end;

function TryStringToFloatPA(const BufP: Pointer; const BufLen: Integer; out Value: Float; out StrLen: Integer): TConvertResult;
var Len : Integer;
    DigVal : Integer;
    DigValF : Float;
    P : PByteChar;
    Ch : AnsiChar;
    HasDig : Boolean;
    Neg : Boolean;
    Res : Float;
    Ex : Float;
    ExI : Int64;
    L : Integer;
begin
  if BufLen <= 0 then
    begin
      Value := 0;
      StrLen := 0;
      Result := convertFormatError;
      exit;
    end;
  P := BufP;
  Len := 0;
  // check sign
  Ch := P^;
  if (Ch = AnsiChar(Ord('+'))) or (Ch = AnsiChar(Ord('-'))) then
    begin
      Inc(Len);
      Inc(P);
      Neg := Ch = AnsiChar(Ord('-'));
    end
  else
    Neg := False;
  // skip leading zeros
  HasDig := False;
  while (Len < BufLen) and (P^ = AnsiChar(Ord('0'))) do
    begin
      Inc(Len);
      Inc(P);
      HasDig := True;
    end;
  // convert integer digits
  Res := 0.0;
  while Len < BufLen do
    begin
      Ch := P^;
      if (Ch >= AnsiChar(Ord('0'))) and (Ch <= AnsiChar(Ord('9'))) then
        begin
          HasDig := True;
          // maximum Extended is roughly 1.1e4932, maximum Double is roughly 1.7e308
          if Abs(Res) >= 1.0e+290 then
            begin
              Value := 0;
              StrLen := Len;
              Result := convertOverflow;
              exit;
            end;
          Res := Res * 10.0;
          DigVal := ByteCharDigitToInt(Ch);
          if Neg then
            Res := Res - DigVal
          else
            Res := Res + DigVal;
          Inc(Len);
          Inc(P);
        end
      else
        break;
    end;
  // convert decimal digits
  if (Len < BufLen) and (P^ = AnsiChar(Ord('.'))) then
    begin
      Inc(Len);
      Inc(P);
      ExI := 0;
      while Len < BufLen do
        begin
          Ch := P^;
          if (Ch >= AnsiChar(Ord('0'))) and (Ch <= AnsiChar(Ord('9'))) then
            begin
              HasDig := True;
              // minimum Extended is roughly 3.6e-4951, minimum Double is roughly 5e-324
              if ExI >= 1000 then
                begin
                  Value := 0;
                  StrLen := Len;
                  Result := convertOverflow;
                  exit;
                end;
              DigVal := ByteCharDigitToInt(Ch);
              Inc(ExI);
              DigValF := DigVal;
              DigValF := DigValF / Power(10.0, ExI);
              if Neg then
                Res := Res - DigValF
              else
                Res := Res + DigValF;
              Inc(Len);
              Inc(P);
            end
          else
            break;
        end;
    end;
  // check valid digit
  if not HasDig then
    begin
      Value := 0;
      StrLen := Len;
      Result := convertFormatError;
      exit;
    end;
  // convert exponent
  if Len < BufLen then
    begin
      Ch := P^;
      if (Ch = AnsiChar(Ord('e'))) or (Ch = AnsiChar(Ord('E'))) then
        begin
          Inc(Len);
          Inc(P);
          Result := TryStringToInt64PB(P, BufLen - Len, ExI, L);
          Inc(Len, L);
          if Result <> convertOK then
            begin
              Value := 0;
              StrLen := Len;
              exit;
            end;
          if ExI <> 0 then
            begin
              if (ExI > 1000) or (ExI < -1000) then
                begin
                  Value := 0;
                  StrLen := Len;
                  Result := convertOverflow;
                  exit;
                end;
              Ex := ExI;
              Ex := Power(10.0, Ex);
              Res := Res * Ex;
            end;
        end;
    end;
  // success
  Value := Res;
  StrLen := Len;
  Result := convertOK;
end;

function TryStringToFloatPW(const BufP: Pointer; const BufLen: Integer; out Value: Float; out StrLen: Integer): TConvertResult;
var Len : Integer;
    DigVal : Integer;
    DigValF : Float;
    P : PWideChar;
    Ch : WideChar;
    HasDig : Boolean;
    Neg : Boolean;
    Res : Float;
    Ex : Float;
    ExI : Int64;
    L : Integer;
begin
  if BufLen <= 0 then
    begin
      Value := 0;
      StrLen := 0;
      Result := convertFormatError;
      exit;
    end;
  P := BufP;
  Len := 0;
  // check sign
  Ch := P^;
  if (Ch = '+') or (Ch = '-') then
    begin
      Inc(Len);
      Inc(P);
      Neg := Ch = '-';
    end
  else
    Neg := False;
  // skip leading zeros
  HasDig := False;
  while (Len < BufLen) and (P^ = '0') do
    begin
      Inc(Len);
      Inc(P);
      HasDig := True;
    end;
  // convert integer digits
  Res := 0.0;
  while Len < BufLen do
    begin
      Ch := P^;
      if (Ch >= '0') and (Ch <= '9') then
        begin
          HasDig := True;
          // maximum Extended is roughly 1.1e4932, maximum Double is roughly 1.7e308
          if Abs(Res) >= 1.0e+1000 then
            begin
              Value := 0;
              StrLen := Len;
              Result := convertOverflow;
              exit;
            end;
          Res := Res * 10.0;
          DigVal := WideCharDigitToInt(Ch);
          if Neg then
            Res := Res - DigVal
          else
            Res := Res + DigVal;
          Inc(Len);
          Inc(P);
        end
      else
        break;
    end;
  // convert decimal digits
  if (Len < BufLen) and (P^ = '.') then
    begin
      Inc(Len);
      Inc(P);
      ExI := 0;
      while Len < BufLen do
        begin
          Ch := P^;
          if (Ch >= '0') and (Ch <= '9') then
            begin
              HasDig := True;
              // minimum Extended is roughly 3.6e-4951, minimum Double is roughly 5e-324
              if ExI >= 1000 then
                begin
                  Value := 0;
                  StrLen := Len;
                  Result := convertOverflow;
                  exit;
                end;
              DigVal := WideCharDigitToInt(Ch);
              Inc(ExI);
              DigValF := DigVal;
              DigValF := DigValF / Power(10.0, ExI);
              if Neg then
                Res := Res - DigValF
              else
                Res := Res + DigValF;
              Inc(Len);
              Inc(P);
            end
          else
            break;
        end;
    end;
  // check valid digit
  if not HasDig then
    begin
      Value := 0;
      StrLen := Len;
      Result := convertFormatError;
      exit;
    end;
  // convert exponent
  if Len < BufLen then
    begin
      Ch := P^;
      if (Ch = 'e') or (Ch = 'E') then
        begin
          Inc(Len);
          Inc(P);
          Result := TryStringToInt64PW(P, BufLen - Len, ExI, L);
          Inc(Len, L);
          if Result <> convertOK then
            begin
              Value := 0;
              StrLen := Len;
              exit;
            end;
          if ExI <> 0 then
            begin
              if (ExI > 1000) or (ExI < -1000) then
                begin
                  Value := 0;
                  StrLen := Len;
                  Result := convertOverflow;
                  exit;
                end;
              Ex := ExI;
              Ex := Power(10.0, Ex);
              Res := Res * Ex;
            end;
        end;
    end;
  // success
  Value := Res;
  StrLen := Len;
  Result := convertOK;
end;

function TryStringToFloatP(const BufP: Pointer; const BufLen: Integer; out Value: Float; out StrLen: Integer): TConvertResult;
var Len : Integer;
    DigVal : Integer;
    DigValF : Float;
    P : PChar;
    Ch : Char;
    HasDig : Boolean;
    Neg : Boolean;
    Res : Float;
    Ex : Float;
    ExI : Int64;
    L : Integer;
begin
  if BufLen <= 0 then
    begin
      Value := 0;
      StrLen := 0;
      Result := convertFormatError;
      exit;
    end;
  P := BufP;
  Len := 0;
  // check sign
  Ch := P^;
  if (Ch = '+') or (Ch = '-') then
    begin
      Inc(Len);
      Inc(P);
      Neg := Ch = '-';
    end
  else
    Neg := False;
  // skip leading zeros
  HasDig := False;
  while (Len < BufLen) and (P^ = '0') do
    begin
      Inc(Len);
      Inc(P);
      HasDig := True;
    end;
  // convert integer digits
  Res := 0.0;
  while Len < BufLen do
    begin
      Ch := P^;
      if (Ch >= '0') and (Ch <= '9') then
        begin
          HasDig := True;
          // maximum Extended is roughly 1.1e4932, maximum Double is roughly 1.7e308
          if Abs(Res) >= 1.0e+1000 then
            begin
              Value := 0;
              StrLen := Len;
              Result := convertOverflow;
              exit;
            end;
          Res := Res * 10.0;
          DigVal := CharDigitToInt(Ch);
          if Neg then
            Res := Res - DigVal
          else
            Res := Res + DigVal;
          Inc(Len);
          Inc(P);
        end
      else
        break;
    end;
  // convert decimal digits
  if (Len < BufLen) and (P^ = '.') then
    begin
      Inc(Len);
      Inc(P);
      ExI := 0;
      while Len < BufLen do
        begin
          Ch := P^;
          if (Ch >= '0') and (Ch <= '9') then
            begin
              HasDig := True;
              // minimum Extended is roughly 3.6e-4951, minimum Double is roughly 5e-324
              if ExI >= 1000 then
                begin
                  Value := 0;
                  StrLen := Len;
                  Result := convertOverflow;
                  exit;
                end;
              DigVal := CharDigitToInt(Ch);
              Inc(ExI);
              DigValF := DigVal;
              DigValF := DigValF / Power(10.0, ExI);
              if Neg then
                Res := Res - DigValF
              else
                Res := Res + DigValF;
              Inc(Len);
              Inc(P);
            end
          else
            break;
        end;
    end;
  // check valid digit
  if not HasDig then
    begin
      Value := 0;
      StrLen := Len;
      Result := convertFormatError;
      exit;
    end;
  // convert exponent
  if Len < BufLen then
    begin
      Ch := P^;
      if (Ch = 'e') or (Ch = 'E') then
        begin
          Inc(Len);
          Inc(P);
          Result := TryStringToInt64P(P, BufLen - Len, ExI, L);
          Inc(Len, L);
          if Result <> convertOK then
            begin
              Value := 0;
              StrLen := Len;
              exit;
            end;
          if ExI <> 0 then
            begin
              if (ExI > 1000) or (ExI < -1000) then
                begin
                  Value := 0;
                  StrLen := Len;
                  Result := convertOverflow;
                  exit;
                end;
              Ex := ExI;
              Ex := Power(10.0, Ex);
              Res := Res * Ex;
            end;
        end;
    end;
  // success
  Value := Res;
  StrLen := Len;
  Result := convertOK;
end;

function TryStringToFloatB(const A: RawByteString; out B: Float): Boolean;
var L, N : Integer;
begin
  L := Length(A);
  Result := TryStringToFloatPA(PByteChar(A), L, B, N) = convertOK;
  if Result then
    if N < L then
      Result := False;
end;

function TryStringToFloatU(const A: UnicodeString; out B: Float): Boolean;
var L, N : Integer;
begin
  L := Length(A);
  Result := TryStringToFloatPW(PWideChar(A), L, B, N) = convertOK;
  if Result then
    if N < L then
      Result := False;
end;

function TryStringToFloat(const A: String; out B: Float): Boolean;
var L, N : Integer;
begin
  L := Length(A);
  Result := TryStringToFloatP(PChar(A), L, B, N) = convertOK;
  if Result then
    if N < L then
      Result := False;
end;

resourcestring
  SRangeCheckError = 'Range check error';

procedure RaiseRangeCheckError; {$IFDEF UseInline}inline;{$ENDIF}
begin
  raise ERangeError.Create(SRangeCheckError);
end;

function StringToFloatB(const A: RawByteString): Float;
begin
  if not TryStringToFloatB(A, Result) then
    RaiseRangeCheckError;
end;

function StringToFloatU(const A: UnicodeString): Float;
begin
  if not TryStringToFloatU(A, Result) then
    RaiseRangeCheckError;
end;

function StringToFloat(const A: String): Float;
begin
  if not TryStringToFloat(A, Result) then
    RaiseRangeCheckError;
end;

function StringToFloatDefB(const A: RawByteString; const Default: Float): Float;
begin
  if not TryStringToFloatB(A, Result) then
    Result := Default;
end;

function StringToFloatDefU(const A: UnicodeString; const Default: Float): Float;
begin
  if not TryStringToFloatU(A, Result) then
    Result := Default;
end;

function StringToFloatDef(const A: String; const Default: Float): Float;
begin
  if not TryStringToFloat(A, Result) then
    Result := Default;
end;



{$IFDEF FLOATS_TEST}
{$ASSERTIONS ON}
procedure Test_Float;
{$IFDEF ExtendedIs80Bits}
var E : Integer;
{$ENDIF}
begin
  Assert(FloatMin(-1.0, 1.0) = -1.0, 'FloatMin');
  Assert(FloatMax(-1.0, 1.0) = 1.0,  'FloatMax');

  Assert(not FloatZero(1e-1, 1e-2),   'FloatZero');
  Assert(FloatZero(1e-2, 1e-2),       'FloatZero');
  Assert(not FloatZero(1e-1, 1e-9),   'FloatZero');
  Assert(not FloatZero(1e-8, 1e-9),   'FloatZero');
  Assert(FloatZero(1e-9, 1e-9),       'FloatZero');
  Assert(FloatZero(1e-10, 1e-9),      'FloatZero');
  Assert(not FloatZero(0.2, 1e-1),    'FloatZero');
  Assert(FloatZero(0.09, 1e-1),       'FloatZero');

  Assert(FloatOne(1.0, 1e-1),         'FloatOne');
  Assert(FloatOne(1.09999, 1e-1),     'FloatOne');
  Assert(FloatOne(0.90001, 1e-1),     'FloatOne');
  Assert(not FloatOne(1.10001, 1e-1), 'FloatOne');
  Assert(not FloatOne(1.2, 1e-1),     'FloatOne');
  Assert(not FloatOne(0.89999, 1e-1), 'FloatOne');

  Assert(not FloatsEqual(2.0, -2.0, 1e-1),             'FloatsEqual');
  Assert(not FloatsEqual(1.0, 0.0, 1e-1),              'FloatsEqual');
  Assert(FloatsEqual(2.0, 2.0, 1e-1),                  'FloatsEqual');
  Assert(FloatsEqual(2.0, 2.09, 1e-1),                 'FloatsEqual');
  Assert(FloatsEqual(2.0, 1.90000001, 1e-1),           'FloatsEqual');
  Assert(not FloatsEqual(2.0, 2.10001, 1e-1),          'FloatsEqual');
  Assert(not FloatsEqual(2.0, 2.2, 1e-1),              'FloatsEqual');
  Assert(not FloatsEqual(2.0, 1.8999999, 1e-1),        'FloatsEqual');
  Assert(FloatsEqual(2.00000000011, 2.0, 1e-2),        'FloatsEqual');
  Assert(FloatsEqual(2.00000000011, 2.0, 1e-9),        'FloatsEqual');
  Assert(not FloatsEqual(2.00000000011, 2.0, 1e-10),   'FloatsEqual');
  Assert(not FloatsEqual(2.00000000011, 2.0, 1e-11),   'FloatsEqual');

  {$IFNDEF ExtendedIsDouble}
  Assert(FloatsCompare(0.0, 0.0, MinExtended) = crEqual,  'FloatsCompare');
  Assert(FloatsCompare(1.2, 1.2, MinExtended) = crEqual,  'FloatsCompare');
  Assert(FloatsCompare(1.23456789e-300, 1.23456789e-300, MinExtended) = crEqual, 'FloatsCompare');
  Assert(FloatsCompare(1.23456780e-300, 1.23456789e-300, MinExtended) = crLess,  'FloatsCompare');
  {$ENDIF}
  Assert(FloatsCompare(1.4e-5, 1.5e-5, 1e-4) = crEqual,   'FloatsCompare');
  Assert(FloatsCompare(1.4e-5, 1.5e-5, 1e-5) = crEqual,   'FloatsCompare');
  Assert(FloatsCompare(1.4e-5, 1.5e-5, 1e-6) = crLess,    'FloatsCompare');
  Assert(FloatsCompare(1.4e-5, 1.5e-5, 1e-7) = crLess,    'FloatsCompare');
  Assert(FloatsCompare(0.5003, 0.5001, 1e-1) = crEqual,   'FloatsCompare');
  Assert(FloatsCompare(0.5003, 0.5001, 1e-2) = crEqual,   'FloatsCompare');
  Assert(FloatsCompare(0.5003, 0.5001, 1e-3) = crEqual,   'FloatsCompare');
  Assert(FloatsCompare(0.5003, 0.5001, 1e-4) = crGreater, 'FloatsCompare');
  Assert(FloatsCompare(0.5003, 0.5001, 1e-5) = crGreater, 'FloatsCompare');

  {$IFDEF ExtendedIs80Bits}
  (*
  Assert(ExtendedApproxEqual(0.0, 0.0),                                'ExtendedApproxEqual');
  Assert(not ExtendedApproxEqual(0.0, 1e-100, 1e-10),                  'ExtendedApproxEqual');
  Assert(not ExtendedApproxEqual(1.0, 1e-100, 1e-10),                  'ExtendedApproxEqual');
  Assert(ExtendedApproxEqual(1.0, 1.0),                                'ExtendedApproxEqual');
  Assert(ExtendedApproxEqual(-1.0, -1.0),                              'ExtendedApproxEqual');
  Assert(not ExtendedApproxEqual(1.0, -1.0),                           'ExtendedApproxEqual');
  Assert(ExtendedApproxEqual(1e-100, 1e-100, 1e-10),                   'ExtendedApproxEqual');
  Assert(not ExtendedApproxEqual(0.0, 1.0, 1e-9),                      'ExtendedApproxEqual');
  Assert(not ExtendedApproxEqual(-1.0, 1.0, 1e-9),                     'ExtendedApproxEqual');
  Assert(ExtendedApproxEqual(0.12345, 0.12349, 1e-3),                  'ExtendedApproxEqual');
  Assert(not ExtendedApproxEqual(0.12345, 0.12349, 1e-4),              'ExtendedApproxEqual');
  Assert(not ExtendedApproxEqual(0.12345, 0.12349, 1e-5),              'ExtendedApproxEqual');
  Assert(ExtendedApproxEqual(1.2345e+100, 1.2349e+100, 1e-3),          'ExtendedApproxEqual');
  Assert(not ExtendedApproxEqual(1.2345e+100, 1.2349e+100, 1e-4),      'ExtendedApproxEqual');
  Assert(not ExtendedApproxEqual(1.2345e+100, 1.2349e+100, 1e-5),      'ExtendedApproxEqual');
  Assert(ExtendedApproxEqual(1.2345e-100, 1.2349e-100, 1e-3),          'ExtendedApproxEqual');
  Assert(not ExtendedApproxEqual(1.2345e-100, 1.2349e-100, 1e-4),      'ExtendedApproxEqual');
  Assert(not ExtendedApproxEqual(1.2345e-100, 1.2349e-100, 1e-5),      'ExtendedApproxEqual');
  Assert(not ExtendedApproxEqual(1.0e+20, 1.00000001E+20, 1e-8),       'ExtendedApproxEqual');
  Assert(ExtendedApproxEqual(1.0e+20, 1.000000001E+20, 1e-8),          'ExtendedApproxEqual');
  Assert(not ExtendedApproxEqual(1.0e+20, 1.000000001E+20, 1e-9),      'ExtendedApproxEqual');
  Assert(ExtendedApproxEqual(1.0e+20, 1.0000000001E+20, 1e-9),         'ExtendedApproxEqual');
  Assert(not ExtendedApproxEqual(1.0e+20, 1.0000000001E+20, 1e-10),    'ExtendedApproxEqual');

  Assert(ExtendedApproxCompare(0.0, 0.0) = crEqual,                         'ExtendedApproxCompare');
  Assert(ExtendedApproxCompare(0.0, 1.0) = crLess,                          'ExtendedApproxCompare');
  Assert(ExtendedApproxCompare(1.0, 0.0) = crGreater,                       'ExtendedApproxCompare');
  Assert(ExtendedApproxCompare(-1.0, 1.0) = crLess,                         'ExtendedApproxCompare');
  Assert(ExtendedApproxCompare(1.2345e+10, 1.2349e+10, 1e-3) = crEqual,     'ExtendedApproxCompare');
  Assert(ExtendedApproxCompare(1.2345e+10, 1.2349e+10, 1e-4) = crLess,      'ExtendedApproxCompare');
  Assert(ExtendedApproxCompare(-1.2345e-10, -1.2349e-10, 1e-3) = crEqual,   'ExtendedApproxCompare');
  Assert(ExtendedApproxCompare(-1.2345e-10, -1.2349e-10, 1e-4) = crGreater, 'ExtendedApproxCompare');
  *)
  {$ENDIF}

  Assert(FloatApproxEqual(0.0, 0.0),                                'FloatApproxEqual');
  Assert(not FloatApproxEqual(0.0, 1e-100, 1e-10),                  'FloatApproxEqual');
  Assert(not FloatApproxEqual(1.0, 1e-100, 1e-10),                  'FloatApproxEqual');
  Assert(FloatApproxEqual(1.0, 1.0),                                'FloatApproxEqual');
  Assert(FloatApproxEqual(-1.0, -1.0),                              'FloatApproxEqual');
  Assert(not FloatApproxEqual(1.0, -1.0),                           'FloatApproxEqual');
  Assert(FloatApproxEqual(1e-100, 1e-100, 1e-10),                   'FloatApproxEqual');
  Assert(not FloatApproxEqual(0.0, 1.0, 1e-9),                      'FloatApproxEqual');
  Assert(not FloatApproxEqual(-1.0, 1.0, 1e-9),                     'FloatApproxEqual');
  Assert(FloatApproxEqual(0.12345, 0.12349, 1e-3),                  'FloatApproxEqual');
  Assert(not FloatApproxEqual(0.12345, 0.12349, 1e-4),              'FloatApproxEqual');
  Assert(not FloatApproxEqual(0.12345, 0.12349, 1e-5),              'FloatApproxEqual');
  Assert(FloatApproxEqual(1.2345e+100, 1.2349e+100, 1e-3),          'FloatApproxEqual');
  Assert(not FloatApproxEqual(1.2345e+100, 1.2349e+100, 1e-4),      'FloatApproxEqual');
  Assert(not FloatApproxEqual(1.2345e+100, 1.2349e+100, 1e-5),      'FloatApproxEqual');
  Assert(FloatApproxEqual(1.2345e-100, 1.2349e-100, 1e-3),          'FloatApproxEqual');
  Assert(not FloatApproxEqual(1.2345e-100, 1.2349e-100, 1e-4),      'FloatApproxEqual');
  Assert(not FloatApproxEqual(1.2345e-100, 1.2349e-100, 1e-5),      'FloatApproxEqual');
  // Assert(not FloatApproxEqual(1.0e+20, 1.00000001E+20, 1e-8),       'FloatApproxEqual');
  Assert(FloatApproxEqual(1.0e+20, 1.000000001E+20, 1e-8),          'FloatApproxEqual');
  // Assert(not FloatApproxEqual(1.0e+20, 1.000000001E+20, 1e-9),      'FloatApproxEqual');
  Assert(FloatApproxEqual(1.0e+20, 1.0000000001E+20, 1e-9),         'FloatApproxEqual');
  // Assert(not FloatApproxEqual(1.0e+20, 1.0000000001E+20, 1e-10),    'FloatApproxEqual');

  Assert(FloatApproxCompare(0.0, 0.0) = crEqual,                         'FloatApproxCompare');
  Assert(FloatApproxCompare(0.0, 1.0) = crLess,                          'FloatApproxCompare');
  Assert(FloatApproxCompare(1.0, 0.0) = crGreater,                       'FloatApproxCompare');
  Assert(FloatApproxCompare(-1.0, 1.0) = crLess,                         'FloatApproxCompare');
  Assert(FloatApproxCompare(1.2345e+10, 1.2349e+10, 1e-3) = crEqual,     'FloatApproxCompare');
  Assert(FloatApproxCompare(1.2345e+10, 1.2349e+10, 1e-4) = crLess,      'FloatApproxCompare');
  Assert(FloatApproxCompare(-1.2345e-10, -1.2349e-10, 1e-3) = crEqual,   'FloatApproxCompare');
  Assert(FloatApproxCompare(-1.2345e-10, -1.2349e-10, 1e-4) = crGreater, 'FloatApproxCompare');

  {$IFDEF ExtendedIs80Bits}
  (*
  Assert(ExtendedExponentBase10(1.0, E),    'ExtendedExponent');
  Assert(E = 0,                          'ExtendedExponent');
  Assert(ExtendedExponentBase10(10.0, E),   'ExtendedExponent');
  Assert(E = 1,                          'ExtendedExponent');
  Assert(ExtendedExponentBase10(0.1, E),    'ExtendedExponent');
  Assert(E = -1,                         'ExtendedExponent');
  Assert(ExtendedExponentBase10(1e100, E),  'ExtendedExponent');
  Assert(E = 100,                        'ExtendedExponent');
  Assert(ExtendedExponentBase10(1e-100, E), 'ExtendedExponent');
  Assert(E = -100,                       'ExtendedExponent');
  Assert(ExtendedExponentBase10(0.999, E),  'ExtendedExponent');
  Assert(E = 0,                          'ExtendedExponent');
  Assert(ExtendedExponentBase10(-0.999, E), 'ExtendedExponent');
  Assert(E = 0,                          'ExtendedExponent');
  *)
  {$ENDIF}
end;

procedure Test_FloatStr;
var A : RawByteString;
    E : Float;
    L : Integer;
begin
  // FloatToStr
  {$IFDEF SupportAnsiString}

  {$IFNDEF FREEPASCAL}
  Assert(FloatToStringA(0.0) = ToAnsiString('0'));
  Assert(FloatToStringA(-1.5) = ToAnsiString('-1.5'));
  Assert(FloatToStringA(1.5) = ToAnsiString('1.5'));
  Assert(FloatToStringA(1.1) = ToAnsiString('1.1'));
  Assert(FloatToStringA(123) = ToAnsiString('123'));
  Assert(FloatToStringA(0.00000000000001) = ToAnsiString('0.00000000000001'));
  Assert(FloatToStringA(0.000000000000001) = ToAnsiString('0.000000000000001'));
  Assert(FloatToStringA(0.0000000000000001) = ToAnsiString('1E-16'));
  Assert(FloatToStringA(0.0000000000000012345) = ToAnsiString('0.000000000000001'));
  Assert(FloatToStringA(0.00000000000000012345) = ToAnsiString('1.2345E-16'));
  {$IFDEF ExtendedIs80Bits}
  Assert(FloatToStringA(123456789.123456789) = ToAnsiString('123456789.123456789'));
  {$IFDEF DELPHIXE2_UP}
  Assert(FloatToStringA(123456789012345.1234567890123456789) = ToAnsiString('123456789012345.123'));
  {$ELSE}
  Assert(FloatToStringA(123456789012345.1234567890123456789) = ToAnsiString('123456789012345.1234'));
  {$ENDIF}
  Assert(FloatToStringA(1234567890123456.1234567890123456789) = ToAnsiString('1.23456789012346E+15'));
  {$ENDIF}
  Assert(FloatToStringA(0.12345) = ToAnsiString('0.12345'));
  Assert(FloatToStringA(1e100) = ToAnsiString('1E+100'));
  Assert(FloatToStringA(1.234e+100) = ToAnsiString('1.234E+100'));
  Assert(FloatToStringA(-1.5e-100) = ToAnsiString('-1.5E-100'));
  {$IFDEF ExtendedIs80Bits}
  Assert(FloatToStringA(1.234e+1000) = ToAnsiString('1.234E+1000'));
  Assert(FloatToStringA(-1e-4000) = ToAnsiString('0'));
  {$ENDIF}
  {$ENDIF}

  Assert(FloatToStringB(0.0) = ToRawByteString('0'));
  Assert(FloatToStringB(-1.5) = ToRawByteString('-1.5'));
  Assert(FloatToStringB(1.5) = ToRawByteString('1.5'));
  Assert(FloatToStringB(1.1) = ToRawByteString('1.1'));

  Assert(FloatToStringU(0.0) = '0');
  Assert(FloatToStringU(-1.5) = '-1.5');
  Assert(FloatToStringU(1.5) = '1.5');
  Assert(FloatToStringU(1.1) = '1.1');
  {$IFDEF ExtendedIs80Bits}
  Assert(FloatToStringU(123456789.123456789) = '123456789.123456789');
  {$IFDEF DELPHIXE2_UP}
  Assert(FloatToStringU(123456789012345.1234567890123456789) = '123456789012345.123');
  {$ELSE}
  Assert(FloatToStringU(123456789012345.1234567890123456789) = '123456789012345.1234');
  {$ENDIF}
  Assert(FloatToStringU(1234567890123456.1234567890123456789) = '1.23456789012346E+15');
  {$ENDIF}
  Assert(FloatToStringU(0.12345) = '0.12345');
  Assert(FloatToStringU(1e100) = '1E+100');
  {$IFNDEF FREEPASCAL}
  Assert(FloatToStringU(1.234e+100) = '1.234E+100');
  {$ENDIF}
  Assert(FloatToStringU(1.5e-100) = '1.5E-100');

  Assert(FloatToString(0.0) = '0');
  Assert(FloatToString(-1.5) = '-1.5');
  Assert(FloatToString(1.5) = '1.5');
  Assert(FloatToString(1.1) = '1.1');
  {$IFDEF ExtendedIs80Bits}
  Assert(FloatToString(123456789.123456789) = '123456789.123456789');
  {$IFDEF DELPHIXE2_UP}
  Assert(FloatToString(123456789012345.1234567890123456789) = '123456789012345.123');
  {$ELSE}
  Assert(FloatToString(123456789012345.1234567890123456789) = '123456789012345.1234');
  {$ENDIF}
  Assert(FloatToString(1234567890123456.1234567890123456789) = '1.23456789012346E+15');
  {$ENDIF}
  Assert(FloatToString(0.12345) = '0.12345');
  Assert(FloatToString(1e100) = '1E+100');
  {$IFNDEF FREEPASCAL}
  Assert(FloatToString(1.234e+100) = '1.234E+100');
  {$ENDIF}
  Assert(FloatToString(1.5e-100) = '1.5E-100');
  {$ENDIF}

  // StrToFloat
  {$IFDEF SupportAnsiString}
  A := ToAnsiString('123.456');
  Assert(TryStringToFloatPA(PAnsiChar(A), Length(A), E, L) = convertOK);
  Assert((E = 123.456) and (L = 7));
  A := ToAnsiString('-000.500A');
  Assert(TryStringToFloatPA(PAnsiChar(A), Length(A), E, L) = convertOK);
  Assert((E = -0.5) and (L = 8));
  A := ToAnsiString('1.234e+002X');
  Assert(TryStringToFloatPA(PAnsiChar(A), Length(A), E, L) = convertOK);
  Assert((E = 123.4) and (L = 10));
  A := ToAnsiString('1.2e300x');
  Assert(TryStringToFloatPA(PAnsiChar(A), Length(A), E, L) = convertOK);
  {$IFDEF ExtendedIs80Bits}
  Assert(ExtendedApproxEqual(E, 1.2e300, 1e-2) and (L = 7));
  {$ENDIF}
  A := ToAnsiString('1.2e-300e');
  Assert(TryStringToFloatPA(PAnsiChar(A), Length(A), E, L) = convertOK);
  {$IFDEF ExtendedIs80Bits}
  Assert(ExtendedApproxEqual(E, 1.2e-300, 1e-2) and (L = 8));
  {$ENDIF}

  // 9999..9999 overflow
  {$IFDEF ExtendedIs80Bits}
  A := '';
  for L := 1 to 5000 do
    A := A + '9';
  Assert(TryStringToFloatPA(PAnsiChar(A), Length(A), E, L) = convertOverflow);
  Assert((E = 0.0) and (L >= 200));
  {$ENDIF}

  // 1200..0000
  {$IFDEF ExtendedIs80Bits}
  A := ToAnsiString('12');
  for L := 1 to 100 do
    A := A + '0';
  Assert(TryStringToFloatPA(PAnsiChar(A), Length(A), E, L) = convertOK);
  Assert(ExtendedApproxEqual(E, 1.2e+101, 1e-2) and (L = 102));
  {$ENDIF}

  // 0.0000..0001 overflow
  {$IFDEF ExtendedIs80Bits}
  A := '0.';
  for L := 1 to 5000 do
    A := A + '0';
  A := A + '1';
  Assert(TryStringToFloatPA(PAnsiChar(A), Length(A), E, L) = convertOverflow);
  Assert((E = 0.0) and (L >= 500));
  {$ENDIF}

  // 0.0000..000123
  {$IFDEF ExtendedIs80Bits}
  A := '0.';
  for L := 1 to 100 do
    A := A + '0';
  A := A + '123';
  Assert(TryStringToFloatPA(PAnsiChar(A), Length(A), E, L) = convertOK);
  Assert(ExtendedApproxEqual(E, 1.23e-101, 1e-3) and (L = 105));
  {$ENDIF}

  // 1200..0000e100
  {$IFDEF ExtendedIs80Bits}
  A := '12';
  for L := 1 to 100 do
    A := A + '0';
  A := A + 'e100';
  Assert(TryStringToFloatPA(PAnsiChar(A), Length(A), E, L) = convertOK);
  Assert(ExtendedApproxEqual(E, 1.2e+201, 1e-1) and (L = 106));
  {$ENDIF}

  {$ENDIF} // $ENDIF SupportAnsiString

  Assert(StringToFloatB(ToRawByteString('0')) = 0.0);
  Assert(StringToFloatB(ToRawByteString('1')) = 1.0);
  Assert(StringToFloatB(ToRawByteString('1.5')) = 1.5);
  Assert(StringToFloatB(ToRawByteString('+1.5')) = 1.5);
  Assert(StringToFloatB(ToRawByteString('-1.5')) = -1.5);
  Assert(StringToFloatB(ToRawByteString('1.1')) = 1.1);
  Assert(StringToFloatB(ToRawByteString('-00.00')) = 0.0);
  Assert(StringToFloatB(ToRawByteString('+00.00')) = 0.0);
  Assert(StringToFloatB(ToRawByteString('0000000000000000000000001.1000000000000000000000000')) = 1.1);
  Assert(StringToFloatB(ToRawByteString('.5')) = 0.5);
  Assert(StringToFloatB(ToRawByteString('-.5')) = -0.5);
  {$IFDEF ExtendedIs80Bits}
  Assert(ExtendedApproxEqual(StringToFloatB(ToRawByteString('1.123456789')), 1.123456789, 1e-10));
  Assert(ExtendedApproxEqual(StringToFloatB(ToRawByteString('123456789.123456789')), 123456789.123456789, 1e-10));
  Assert(ExtendedApproxEqual(StringToFloatB(ToRawByteString('1.5e500')), 1.5e500, 1e-2));
  Assert(ExtendedApproxEqual(StringToFloatB(ToRawByteString('+1.5e+500')), 1.5e500, 1e-2));
  Assert(ExtendedApproxEqual(StringToFloatB(ToRawByteString('1.2E-500')), 1.2e-500, 1e-2));
  Assert(ExtendedApproxEqual(StringToFloatB(ToRawByteString('-1.2E-500')), -1.2e-500, 1e-2));
  Assert(ExtendedApproxEqual(StringToFloatB(ToRawByteString('-1.23456789E-500')), -1.23456789e-500, 1e-9));
  {$ENDIF}

  Assert(not TryStringToFloatB(ToRawByteString(''), E));
  Assert(not TryStringToFloatB(ToRawByteString('+'), E));
  Assert(not TryStringToFloatB(ToRawByteString('-'), E));
  Assert(not TryStringToFloatB(ToRawByteString('.'), E));
  Assert(not TryStringToFloatB(ToRawByteString(' '), E));
  Assert(not TryStringToFloatB(ToRawByteString(' 0'), E));
  Assert(not TryStringToFloatB(ToRawByteString('0 '), E));
  Assert(not TryStringToFloatB(ToRawByteString('--0'), E));
  Assert(not TryStringToFloatB(ToRawByteString('0X'), E));
end;

procedure Test;
begin
  Test_Float;
  Test_FloatStr;
end;
{$ENDIF}



end.

