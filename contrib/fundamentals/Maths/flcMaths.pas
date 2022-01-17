{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals 5.00                                        }
{   File name:        flcMaths.pas                                             }
{   File version:     5.64                                                     }
{   Description:      Maths utility functions                                  }
{                                                                              }
{   Copyright:        Copyright (c) 1995-2018, David J Butler                  }
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
{ Thanks to:                                                                   }
{                                                                              }
{   Stephen L. Moshier, Guy Hirson, Ondrej Hrabal, Andrew Driazgov,            }
{   Torsten Wasch, Jingyi Peng, Michael Schuemann, Sebastian Schuberth.        }
{                                                                              }
{ Revision history:                                                            }
{                                                                              }
{   1995-96     0.01  Wrote statistical and actuarial functions.               }
{   1998/03     0.02  Added Solver and SecantSolver.                           }
{   1998/10     0.03  Removed functions now found in Delphi 3's Math unit      }
{                     Uses Delphi's math exceptions (eg EOverflow,             }
{                     EInvalidArgument, etc)                                   }
{   1999/08/29  0.04  Added ASet, TRangeSet, TFlatSet, TSparseFlatSet.         }
{   1999/09/27  0.05  Added TMatrix, TVector.                                  }
{                     Added BinomialCoeff.                                     }
{   1999/10/02  0.06  Added TComplex.                                          }
{   1999/10/03  0.07  Added DerivateSolvers.                                   }
{                     Completed TMatrix.                                       }
{   1999/10/04  0.08  Added TLifeTable.                                        }
{   1999/10/05  0.09  T3DPoint                                                 }
{   1999/10/06  0.10  Transform matrices.                                      }
{   1999/10/13  0.11  TRay, TSphere                                            }
{   1999/10/14  0.12  TPlane                                                   }
{   1999/10/26  1.13  Upgraded to Delphi 4. Compared the assembly code of the  }
{                     new dynamic arrays with that of pointers to arrays.      }
{   1999/10/30  1.14  Added TVector.StdDev                                     }
{                     Changed some functions to the same name (since Delphi    }
{                     now supports overloading).                               }
{                     Removed Min and Max functions (now in Math).             }
{   1999/11/04  1.15  Added TVector.Pos, TVector.Append                        }
{   1999/11/07  1.16  Added RandomSeed function.                               }
{                     Added assembly bit functions.                            }
{   1999/11/10  1.17  Added hashing functions. Coded XOR8 in assembly.         }
{                     Added MD5 hashing.                                       }
{   1999/11/11  1.18  Added EncodeBase.                                        }
{   1999/11/21  1.19  Added TComplex.Power                                     }
{   1999/11/25  1.20  Moved TRay, TSphere and TPlane to cRayTrace.             }
{                     Added Primes.                                            }
{   1999/11/26  1.21  Added Rational numbers (can convert to/from TReal).      }
{                     Added GCD.                                               }
{                     Added RealArray/IntegerArray functions.                  }
{   1999/11/27  1.22  Replaced GCD algorithm with Euclid's algorithm.          }
{                     Added SI constants.                                      }
{                     Added TMatrix.Normalise, TMatrix.Multiply (Row, Value)   }
{   1999/12/01  1.23  Added RandomUniform.                                     }
{   1999/12/03  1.24  Added RandomNormal.                                      }
{   1999/12/16  1.25  Bug fixes.                                               }
{   1999/12/23  1.26  Fixed bug in TRational.CalcFrac.                         }
{   1999/12/26  1.27  Added Reverse procedures for RealArray/IntegerArray.     }
{   2000/01/22  1.28  Added TStatistic.                                        }
{   2000/01/23  1.29  Added RandomPseudoWord.                                  }
{   2000/03/08  1.30  Moved TInteger/TReal, IntegerArray/RealArray to cUtil.   }
{                     Moved AArray to cDataStructs.                            }
{   2000/04/01  1.31  Moved ASet and set implmentations to cDataStructs.       }
{   2000/04/09  1.32  Added SetFPUPrecision.                                   }
{   2000/05/03  1.33  Added Bit functions (ToggleBit, SetBit, ClearBit,        }
{                     IsBitSet).                                               }
{   2000/05/08  1.34  Moved SetFPUPrecision to cUtil.                          }
{   2000/05/25  1.35  Started THugeInteger.                                    }
{   2000/06/06  1.36  Moved bit functions to cUtil.                            }
{   2000/06/08  1.37  TVector now inherits from TExtendedArray.                }
{                     Added TIntegerVector.                                    }
{                     Removed TInteger/TReal, TIntegerArray/TRealArray.        }
{   2000/06/16  1.38  Updated documentation for financial functions.           }
{   2000/06/17  1.39  Recalculated all constants to 34 digits.                 }
{   2000/06/25  1.40  Fixed bug in CalcCheckSum32 reported by Ondrej Hrabal.   }
{   2000/07/13  1.41  Fixed bug in RandomUniform reported by Andrew Driazgov.  }
{   2000/07/22  1.42  Replaced Fibonacci function with a much quicker one by   }
{                     Torsten Wasch.                                           }
{   2000/08/04  1.43  Added TMovingStatistic.                                  }
{   2000/08/22  1.44  Added RandomHex.                                         }
{   2000/09/20  1.45  More random states to RandomSeed.                        }
{   2000/09/23  1.46  Added EncodeBase64.                                      }
{   2000/10/03  1.47  Fixed bug in TMatrix.Transposed reported by Jingyi Peng. }
{   2000/10/22  1.48  Added CalcKeyedMD5 (RFC 2104 - HMAC)                     }
{   2001/05/14  1.49  Moved RandomSeed function to cSysUtils.                  }
{   2001/05/19  1.50  Moved Hashing functions to cUtils.                       }
{   2001/05/21  1.51  Moved TTRational and TTComplex from cExDataStructs.      }
{   2001/07/31  1.52  Minor Revisions and bug fixes.                           }
{   2001/08/18  1.53  Changed Sgn function to return 0 for 0.                  }
{                     Changed Median functions to be average of two middle     }
{                     values for even number of elements and the geometric     }
{                     mean to be calculated using logs (as suggested by        }
{                     Michael Schuemann <schuemann@uke.uni-hamburg.de>)        }
{   2001/11/18  1.54  Added FourierTransform functions.                        }
{                     Added HugeWord functions for Add, Subtract, Invert,      }
{                     MultiplyLong and Divide.                                 }
{   2001/11/19  2.55  Added HugeWord function for MultiplyFFT.                 }
{                     Added THugeInteger.                                      }
{   2002/01/03  2.56  Moved RandomUniform, RandomPseudoWord to cSysUtils.      }
{                     Moved EncodeBase64, DecodeBase64 to cUtils.              }
{   2002/01/31  2.57  Fixed GCD function to always return positive value.      }
{                     Thanks to Sebastian Schuberth <s.schuberth@tu-bs.de>.    }
{   2002/02/03  2.58  Added InvMod, ExpMod and Jacobi, contribution by         }
{                     Sebastian Schuberth <s.schuberth@tu-bs.de>.              }
{   2002/06/01  2.59  Created cHugeInt, cRational, cComplex, cVectors,         }
{                     cMatrix and c3DMath units from cMaths.                   }
{   2003/02/16  3.60  Created cStatistics unit from cMaths.                    }
{                     Revised for Fundamentals 3.                              }
{   2003/03/14  3.61  Added FPU functions.                                     }
{                     Refactored Prime functions.                              }
{   2005/08/14  4.62  Compilable with FreePascal 2 Win32 i386.                 }
{   2016/01/17  5.63  Revised for Fundamentals 5.                              }
{   2018/07/17  5.64  Word32 changes.                                          }
{                                                                              }
{ Supported compilers:                                                         }
{                                                                              }
{   Delphi 7 Win32                      5.63  2016/01/17                       }
{   Delphi XE7 Win32                    5.63  2016/01/17                       }
{   Delphi XE7 Win64                    5.63  2016/01/17                       }
{                                                                              }
{******************************************************************************}

{$INCLUDE flcMaths.inc}

unit flcMaths;

interface

uses
  { System }
  SysUtils,

  { Fundamentals }
  flcStdTypes;



{                                                                              }
{ Mathematical constants                                                       }
{                                                                              }
const
  Pi           = 3.14159265358979323846      +        { Pi (200 digits)        }
                 0.26433832795028841971e-20  +        {                        }
                 0.69399375105820974944e-40  +        {                        }
                 0.59230781640628620899e-60  +        {                        }
                 0.86280348253421170679e-80  +        {                        }
                 0.82148086513282306647e-100 +        {                        }
                 0.09384460955058223172e-120 +        {                        }
                 0.53594081284811174502e-140 +        {                        }
                 0.84102701938521105559e-160 +        {                        }
                 0.64462294895493038196e-180;         {                        }
  Pi2          = 6.283185307179586476925286766559006; { Pi * 2                 }
  Pi3          = 9.424777960769379715387930149838509; { Pi * 3                 }
  Pi4          = 12.56637061435917295385057353311801; { Pi * 4                 }
  PiOn2        = 1.570796326794896619231321691639751; { Pi / 2                 }
  PiOn3        = 1.047197551196597746154214461093168; { Pi / 3                 }
  PiOn4        = 0.785398163397448309615660845819876; { Pi / 4                 }
  PiSq         = 9.869604401089358618834490999876151; { Pi^2                   }
  PiE          = 22.45915771836104547342715220454374; { Pi^e                   }
  LnPi         = 1.144729885849400174143427351353059; { Ln (Pi)                }
  LogPi        = 0.497149872694133854351268288290899; { Log (Pi)               }
  SqrtPi       = 1.772453850905516027298167483341145; { Sqrt (Pi)              }
  Sqrt2Pi      = 2.506628274631000502415765284811045; { Sqrt (2 * Pi)          }
  LnSqrt2Pi    = 0.918938533204672741780329736405618; { Ln (Sqrt (2 * Pi))     }
  DegPerRad    = 57.29577951308232087679815481410517; { 180 / Pi               }
  DegPerGrad   = 0.9;                                 {                        }
  DegPerCycle  = 360.0;                               {                        }
  GradPerCycle = 400.0;                               {                        }
  GradPerDeg   = 1.111111111111111111111111111111111; {                        }
  GradPerRad   = 63.661977236758134307553505349006;   {                        }
  RadPerDeg    = 0.017453292519943295769236907684886; { Pi / 180               }
  RadPerGrad   = 0.015707963267948966192313216916398; {                        }
  RadPerCycle  = 6.283185307179586476925286766559;    {                        }
  CyclePerDeg  = 0.002777777777777777777777777777778; {                        }
  CyclePerRad  = 0.15915494309189533576888376337251;  {                        }
  CyclePerGrad = 0.0025;                              {                        }
  E            = 2.718281828459045235360287471352663; { e                      }
  E2           = 7.389056098930650227230427460575008; { e^2                    }
  ExpM2        = 0.135335283236612691893999494972484; { e^-2                   }
  Ln2          = 0.693147180559945309417232121458177; { Ln (2)                 }
  Ln10         = 2.302585092994045684017991454684364; { Ln (10)                }
  LogE         = 0.434294481903251827651128918916605; { Log (e)                }
  Log2         = 0.301029995663981195213738894724493; { Log (2)                }
  Log3         = 0.477121254719662437295027903255115; { Log (3)                }
  Sqrt2        = 1.414213562373095048801688724209698; { Sqrt (2)               }
  Sqrt3        = 1.732050807568877293527446341505872; { Sqrt (3)               }
  Sqrt5        = 2.236067977499789696409173668731276; { Sqrt (5)               }
  Sqrt7        = 2.645751311064590590501615753639260; { Sqrt (7)               }



{                                                                              }
{ Mathematical types                                                           }
{                                                                              }
type
  {$IFDEF MFloatIsDouble}
  MFloat      = Double;
  MFloatArray = DoubleArray;
  {$ELSE}
  {$IFDEF MFloatIsExtended}
  MFloat      = Extended;
  MFloatArray = ExtendedArray;
  {$ENDIF}
  {$ENDIF}
  PMFloat = ^MFloat;



{                                                                              }
{ FPU (Floating Point Unit) functions                                          }
{                                                                              }
{$IFDEF DELPHI6_UP}
procedure SetFPUPrecisionSingle;
procedure SetFPUPrecisionDouble;
procedure SetFPUPrecisionExtended;

procedure SetFPURoundingNearest;
procedure SetFPURoundingDown;
procedure SetFPURoundingUp;
procedure SetFPURoundingTruncate;
{$ENDIF}



{                                                                              }
{ Polynomial                                                                   }
{                                                                              }
{   Calculates polynomial of degree N.                                         }
{                                                                              }
{   Note that coefficients are stored in reverse order, ie Coef[0] = C         }
{                                                                     N        }
{                        2          N                                          }
{    y  =  C  + C x + C x  +...+ C x                                           }
{           0    1     2          N                                            }
{                                                                              }
function  PolyEval(const X: MFloat; const Coef: array of MFloat;
          const N: Integer): MFloat;



{                                                                              }
{ Miscellaneous functions                                                      }
{                                                                              }
{   Sgn returns the sign of an argument, +1, -1 or 0.                          }
{                                                                              }
{   FloatMod calculates the floating-point value of A mod B.                   }
{                                                                              }
function  Sign(const R: Integer): Integer; overload;
function  Sign(const R: Int64): Integer; overload;
function  Sign(const R: Single): Integer; overload;
function  Sign(const R: Double): Integer; overload;
function  Sign(const R: Extended): Integer; overload;

function  FloatMod(const A, B: MFloat): MFloat;



{                                                                              }
{ Trigonometric functions                                                      }
{                                                                              }
{   Delphi's Math unit includes most commonly used trigonometric functions.    }
{                                                                              }
function  ATan360(const X, Y: MFloat): MFloat;

function  InverseTangentDeg(const X, Y: MFloat): MFloat;
function  InverseTangentRad(const X, Y: MFloat): MFloat;
function  InverseSinDeg(const Y, R: MFloat): MFloat;
function  InverseSinRad(const Y, R: MFloat): MFloat;
function  InverseCosDeg(const X, R: MFloat): MFloat;
function  InverseCosRad(const X, R: MFloat): MFloat;

function  DMSToReal(const Degs, Mins, Secs: MFloat): MFloat;
procedure RealToDMS(const X: MFloat; var Degs, Mins, Secs: MFloat);

procedure PolarToRectangular(const R, Theta: MFloat; var X, Y: MFloat);
procedure RectangularToPolar(const X, Y: MFloat; var R, Theta: MFloat);

function  Distance(const X1, Y1, X2, Y2: MFloat): MFloat;



{                                                                              }
{ Prime numbers                                                                }
{                                                                              }
{   IsPrime returns True if N is a prime number.                               }
{                                                                              }
{   IsPrimeFactor returns True if F is a prime factor of N.                    }
{                                                                              }
{   PrimeFactors returns an array of the prime factors of N.                   }
{                                                                              }
{   GCD returns the Greatest Common Divisor of N1 and N2.                      }
{                                                                              }
{   IsRelativePrime returns True if the GCD(X, Y) = 1. Relative prime is       }
{   also called co-prime.                                                      }
{                                                                              }
{   InvMod calculates the modular inverse of A (mod N).                        }
{   If X is the modular inverse of A modulo N then: (X*A) mod N = 1            }
{   [ in mathematical notation it is: X*A = 1 (mod N) ]                        }
{                                                                              }
{   ExpMod calculates A^Z mod N.                                               }
{                                                                              }
{   Jacobi calculates the 'Jacobi Symbol (a/n)'.                               }
{                                                                              }
function  IsPrime(const N: Int64): Boolean;
function  IsPrimeFactor(const N, F: Int64): Boolean;
function  PrimeFactors(const N: Int64): Int64Array;

function  GCD(const N1, N2: Integer): Integer; overload;
function  GCD(const N1, N2: Int64): Int64; overload;
function  IsRelativePrime(const X, Y: Int64): Boolean;

function  InvMod(const A, N: Integer): Integer; overload;
function  InvMod(const A, N: Int64): Int64; overload;
function  ExpMod(A, Z: Integer; const N: Integer): Integer; overload;
function  ExpMod(A, Z: Int64; const N: Int64): Int64; overload;

function  Jacobi(const A, N: Integer): Integer;



{                                                                              }
{ Combinatoric functions                                                       }
{                                                                              }
const
  {$IFDEF MFloatIsExtended}
  FactorialMaxN   = 1754;
  {$ELSE}
  {$IFDEF MFloatIsDouble}
  FactorialMaxN   = 170;
  {$ENDIF}
  {$ENDIF}

function  Factorial(const N: Integer): MFloat;
function  Combinations(const N, C: Integer): MFloat;
function  Permutations(const N, P: Integer): MFloat;
function  Fibonacci(const N: Integer): Int64;




{                                                                              }
{ Statistical functions                                                        }
{                                                                              }
{   GammaLn returns the natural logarithm of the gamma function.               }
{                                                                              }
function  GammaLn(X: Extended): Extended;




{                                                                              }
{ Fourier Transformations                                                      }
{                                                                              }
procedure FourierTransform(const AngleNumerator: MFloat;
          const RealIn, ImagIn: array of MFloat;
          var RealOut, ImagOut: MFloatArray);
procedure FFT(const RealIn, ImagIn: array of MFloat;
          var RealOut, ImagOut: MFloatArray);
procedure InverseFFT(const RealIn, ImagIn: array of MFloat;
          var RealOut, ImagOut: MFloatArray);
procedure CalcFrequency(const FrequencyIndex: Integer;
          const RealIn, ImagIn: array of MFloat;
          var RealOut, ImagOut: MFloat);



{                                                                              }
{ Numerical routines                                                           }
{                                                                              }
type
  fx = function (const x: MFloat): MFloat;

function  SecantSolver(const f: fx; const y, Guess1, Guess2: MFloat): MFloat;
{ Uses Secant method to solve for x in f(x) = y                                }

function  NewtonSolver(const f, df: fx; const y, Guess: MFloat): MFloat;
{ Uses Newton's method to solve for x in f(x) = y.                             }
{ df = f'(x).                                                                  }
{ Note: This implementation does not check if the solver goes on a tangent     }
{       (which can happen with certain Guess values)                           }

function  FirstDerivative(const f: fx; const x: MFloat): MFloat;
{ Returns the value of f'(x)                                                   }
{ Uses (-f(x+2h) + 8f(x+h) - 8f(x-h) + f(x-2h)) / 12h                          }

function  SecondDerivative(const f: fx; const x: MFloat): MFloat;
{ Returns the value of f''(x)                                                  }
{ Uses (-f(x+2h) + 16f(x+h) - 30f(x) + 16f(x-h) - f(x-2h)) / 12h^2             }

function  ThirdDerivative(const f: fx; const x: MFloat): MFloat;
{ Returns the value of f'''(x)                                                 }
{ Uses (f(x+2h) - 2f(x+h) + 2f(x-h) - f(x-2*h)) / 2h^3                         }

function  FourthDerivative(const f: fx; const x: MFloat): MFloat;
{ Returns the value of f''''(x)                                                }
{ Uses (f(x+2h) - 4f(x+h) + 6f(x) - 4f(x-h) + f(x-2h)) / h^4                   }

function  SimpsonIntegration(const f: fx; const a, b: MFloat; N: Integer): MFloat;
{ Returns the area under f from a to b, by applying Simpson's 1/3 Rule with    }
{ N subdivisions.                                                              }



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
  flcBits32,
  flcDynArrays,
  flcFloats;



{                                                                              }
{ FPU (Floating Point Unit) functions                                          }
{                                                                              }
procedure SetFPUPrecisionSingle;
begin
  {$IFDEF DELPHI6_UP}
  // SetPrecisionMode(pmSingle);
  {$ENDIF}
end;

procedure SetFPUPrecisionDouble;
begin
  {$IFDEF DELPHI6_UP}
  // SetPrecisionMode(pmDouble);
  {$ENDIF}
end;

procedure SetFPUPrecisionExtended;
begin
  {$IFDEF DELPHI6_UP}
  // SetPrecisionMode(pmExtended);
  {$ENDIF}
end;

procedure SetFPURoundingNearest;
begin
  {$IFDEF DELPHI6_UP}
  SetRoundMode(rmNearest);
  {$ENDIF}
end;

procedure SetFPURoundingDown;
begin
  {$IFDEF DELPHI6_UP}
  SetRoundMode(rmDown);
  {$ENDIF}
end;

procedure SetFPURoundingUp;
begin
  {$IFDEF DELPHI6_UP}
  SetRoundMode(rmUp);
  {$ENDIF}
end;

procedure SetFPURoundingTruncate;
begin
  {$IFDEF DELPHI6_UP}
  SetRoundMode(rmTruncate);
  {$ENDIF}
end;



{                                                                              }
{ Polynomial                                                                   }
{                                                                              }
function PolyEval(const X: MFloat; const Coef: array of MFloat; const N: Integer): MFloat;
var P : MFloat;
    L : Integer;
begin
  if Length(Coef) <> N + 1 then
    raise EInvalidArgument.Create('PolyEval: Invalid number of coefficients');
  P := 1.0;
  L := N;
  Result := 0.0;
  while L >= 0 do
    begin
      Result := Result + Coef[L] * P;
      P := P * X;
      Dec(L);
    end;
end;



{                                                                              }
{ Miscellaneous functions                                                      }
{                                                                              }
{$IFDEF ASM386_DELPHI}
function Sign(const R: Integer): Integer;
asm
    TEST EAX, EAX
    JL   @Negative
    JG   @Positive
    RET
  @Negative:
    MOV  EAX, -1
    RET
  @Positive:
    MOV  EAX, 1
end;
{$ELSE}
function Sign(const R: Integer): Integer;
begin
  if R > 0 then
    Result := 1 else
    if R < 0 then
      Result := -1 else
      Result := 0;
end;
{$ENDIF}

function Sign(const R: Int64): Integer;
begin
  if R > 0 then
    Result := 1 else
    if R < 0 then
      Result := -1 else
      Result := 0;
end;

function Sign(const R: Single): Integer;
begin
  if R > 0.0 then
    Result := 1 else
    if R < 0.0 then
      Result := -1 else
      Result := 0;
end;

function Sign(const R: Double): Integer;
begin
  if R > 0.0 then
    Result := 1 else
    if R < 0.0 then
      Result := -1 else
      Result := 0;
end;

function Sign(const R: Extended): Integer;
begin
  if R > 0.0 then
    Result := 1 else
    if R < 0.0 then
      Result := -1 else
      Result := 0;
end;

function FloatMod(const A, B: MFloat): MFloat;
begin
  Result := A - Floor(A / B) * B;
end;



{                                                                              }
{ Trigonometric functions                                                      }
{                                                                              }
function ATan360(const X, Y: MFloat): MFloat;
var Angle: MFloat;
begin
  if FloatZero(X) then
    Angle := PiOn2
  else
    Angle := ArcTan(Y / X);
  Angle := Angle * DegPerRad;
  if (X <= 0.0) and (Y < 0.0) then
    Angle := Angle - 180.0;
  if (X < 0.0) and (Y > 0.0) then
    Angle := Angle + 180.0;
  if Angle < 0.0 then
    Angle := Angle + 360.0;
  ATan360 := Angle;
end;

function InverseTangentDeg(const X, Y: MFloat): MFloat;
{ 0 <= Result <= 360 }
var Angle : MFloat;
begin
  if FloatZero(X) then
    Angle := PiOn2
  else
    Angle := ArcTan (Y / X);
  Angle := Angle * 180.0 / Pi;
  if (X <= 0.0) and (Y < 0.0) then
    Angle := Angle - 180.0
  else
  if (X < 0.0) and (Y > 0.0) then
    Angle := Angle + 180.0;
  if Angle < 0.0 then
    Angle := Angle + 360.0;
  InverseTangentDeg := Angle;
end;

function InverseTangentRad(const X, Y: MFloat): MFloat;
{ 0 <= result <= 2pi }
var Angle : MFloat;
begin
  if FloatZero(X) then
    Angle := PiOn2
  else
    Angle := ArcTan(Y / X);
  if (X <= 0.0) and (Y < 0) then
    Angle := Angle - Pi;
  if (X < 0.0) and (Y > 0) then
    Angle := Angle + Pi;
  If Angle < 0 then
    Angle := Angle + Pi2;
  InverseTangentRad := Angle;
end;

function InverseSinDeg(const Y, R: MFloat): MFloat;
{ -90 <= result <= 90 }
var X : MFloat;
begin
  X := Sqrt(Sqr(R) - Sqr(Y));
  Result := InverseTangentDeg(X, Y);
  If Result > 90.0 then
    Result := Result - 360.0;
end;

function InverseSinRad(const Y, R: MFloat): MFloat;
{ -90 <= result <= 90 }
var X : MFloat;
begin
  X := Sqrt(Sqr(R) - Sqr(Y));
  Result := InverseTangentRad(X, Y);
  if Result > 90.0 then
    Result := Result - 360.0;
end;

function InverseCosDeg(const X, R: MFloat): MFloat;
{ -90 <= result <= 90 }
var Y : MFloat;
begin
  Y := Sqrt(Sqr(R) - Sqr(X));
  Result := InverseTangentDeg(X, Y);
  if Result > 90.0 then
    Result := Result - 360.0;
end;

function InverseCosRad(const X, R: MFloat): MFloat;
{ -90 <= result <= 90 }
var Y : MFloat;
begin
  Y := Sqrt(Sqr(R) - Sqr(X));
  Result := InverseTangentRad(X, Y);
  if Result > 90.0 then
    Result := Result - 360.0;
end;

procedure RealToDMS(const X: MFloat; var Degs, Mins, Secs: MFloat);
var Y : MFloat;
begin
  Degs := Int(X);
  Y := Frac(X) * 60.0;
  Mins := Int(Y);
  Secs := Frac(Y) * 60.0;
end;

function DMSToReal(const Degs, Mins, Secs: MFloat): MFloat;
begin
  Result := Degs + Mins / 60.0 + Secs / 3600.0;
end;

function CanonicalForm(const Theta: MFloat): MFloat;                        {-PI < theta <= PI}
begin
  if Abs(Theta) > Pi then
     Result := Round(Theta / Pi2) * Pi2
  else
     Result := Theta;
end;

procedure PolarToRectangular(const R, Theta: MFloat; var X, Y: MFloat);
var S, C : MFloat;
begin
  SinCos(Theta, S, C);
  X := R * C;
  Y := R * S;
end;

procedure RectangularToPolar(const X, Y: MFloat; var R, Theta: MFloat);
begin
  if FloatZero(X) then
    if FloatZero(Y) then
      R := 0.0
    else
      if Y > 0.0 then
        R := Y
      else
        R := -Y
  else
    R := Sqrt(Sqr(X) + Sqr(Y));
  Theta := ArcTan2(Y, X);
end;

function Distance(const X1, Y1, X2, Y2: MFloat): MFloat;
begin
  Result := Sqrt(Sqr(X1 - X2) + Sqr(Y1 - Y2));
end;



{                                                                              }
{ Prime numbers                                                                }
{                                                                              }
{   The prime functions use the fact that it's only necessary to check up to   }
{   Sqrt(N) for factors of N when checking if N is prime.                      }
{                                                                              }

{ Lookups for prime numbers in the Byte range.                                 }
const
  BytePrimesCount = 54;
  BytePrimesTable: array[0..BytePrimesCount - 1] of Byte = (
      2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61,
      67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137,
      139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 211,
      223, 227, 229, 233, 239, 241, 251);
  BytePrimesSet: set of Byte = [
      2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61,
      67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137,
      139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 211,
      223, 227, 229, 233, 239, 241, 251];

{ Lookup for prime numbers in the Word range.                                  }
const
  WordPrimesLookupEntries = $10000; // 65536
  WordPrimesLookupLength  = WordPrimesLookupEntries div (Sizeof(Word32) * 8); // 2048
  WordPrimesLookupSize    = WordPrimesLookupLength * Sizeof(Word32); // 8192

var
  WordPrimesInit : Boolean = False;
  WordPrimesSet  : array of Word32; // 8192 bytes when initialized, 1 bit for
                                      // each number between 0-65535.

procedure ClearWordPrimesBit(const N: Word);
var I : Word32;
    P : PWord32;
begin
  I := N shr 5;
  P := @WordPrimesSet[I];
  P^ := ClearBit32(P^, N and 31);
end;

procedure InitWordPrimesSet;
var I, J, P : Integer;
begin
  // Initialize WordPrimesSet bits to True; clear bits 0 and 1
  SetLength(WordPrimesSet, WordPrimesLookupLength);
  FillChar(Pointer(WordPrimesSet)^, WordPrimesLookupSize, #$FF);
  ClearWordPrimesBit(0);
  ClearWordPrimesBit(1);
  // Clear bits for multiples of Byte primes
  for I := 0 to BytePrimesCount - 1 do
    begin
      P := BytePrimesTable[I];
      for J := 2 to $FFFF div P do
        ClearWordPrimesBit(P * J);
    end;
  // Set initialized
  WordPrimesInit := True;
end;

{ IsPrime                                                                      }
function IsPrime(const N: Int64): Boolean;
var V : Int64;
    E : MFloat;
    M : Word32;
    B : Word32;
    I : Word32;
    L : Word32;
begin
  // Initialize value
  V := N;
  if V < 0 then
    V := -V;
  // Check if V is prime
  if V <= $FF then
    // V is in Byte range, lookup from BytePrimesSet
    Result := Byte(V) in BytePrimesSet
  else
  if V <= $FFFF then
    begin
      // V is in Word range
      if not WordPrimesInit then
        InitWordPrimesSet;
      Result := IsBitSet32(WordPrimesSet[Word(V) shr 5], Word(V) and 31);
    end
  else
    begin
      // V is in Word32 range
      // Look for factor from primes in Byte range
      for I := 0 to BytePrimesCount - 1 do
        begin
          B := BytePrimesTable[I];
          if V mod B = 0 then
            begin
              // N is divisable by B, N is not prime
              Result := False;
              exit;
            end;
        end;
      // Calculate maximum factor
      E := V;
      M := Ceil(Sqrt(E));
      // Look for factor from primes in Word range
      if not WordPrimesInit then
        InitWordPrimesSet;
      if M > $FFFF then
        L := $FFFF
      else
        L := M;
      for I := $100 to L do
        if IsBitSet32(WordPrimesSet[I shr 5], I and 31) then
          if V mod I = 0 then
            begin
              // N is divisable by I, N is not prime
              Result := False;
              exit;
            end;
      // Look for factor from values in Word32 range
      for I := $10000 to M do
        if V mod I = 0 then
          begin
            // N is divisable by I, N is not prime
            Result := False;
            exit;
          end;
      // Sqrt(N) reached, N is prime
      Result := True;
    end;
end;

{ IsPrimeFactor                                                                }
function IsPrimeFactor(const N, F: Int64): Boolean;
begin
  Result := (N mod F = 0) and IsPrime(F);
end;

{ PrimeFactors                                                                 }
function PrimeFactors(const N: Int64): Int64Array;
var V : Int64;
    E : MFloat;
    M : Word32;
    I : Word32;
begin
  // Initialize
  Result := nil;
  V := N;
  if V < 0 then
    V := -V;
  // 0 and 1 has no prime factors
  if V <= 1 then
    exit;
  // Calculate maximum factor
  E := V;
  M := Ceil(Sqrt(E));
  // Find prime factors
  for I := 2 to M do
    if IsPrime(I) and (V mod I = 0) then
      begin
        // I is a prime factor
        DynArrayAppend(Result, I);
        repeat
          V := V div I;
          if V = 1 then
            // No more factors
            exit;
        until V mod I <> 0;
      end;
  // Check for remaining prime factor
  if IsPrime(V) then
    DynArrayAppend(Result, V);
end;



{ Find the GCD using Euclid's algorithm                                        }
function GCD(const N1, N2: Integer): Integer;
var X, Y, J : Integer;
begin
  X := N1;
  Y := N2;
  if X < Y then
    Swap(X, Y);
  while (X <> 1) and (X <> 0) and (Y <> 1) and (Y <> 0) do
    begin
      J := (X - Y) mod Y;
      if J = 0 then
        begin
          Result := Abs(Y);
          exit;
        end;
      X := Y;
      Y := J;
    end;
  Result := 1;
end;

function GCD(const N1, N2: Int64): Int64;
var X, Y, J : Int64;
begin
  X := N1;
  Y := N2;
  if X < Y then
    Swap(X, Y);
  while (X <> 1) and (X <> 0) and (Y <> 1) and (Y <> 0) do
    begin
      J := (X - Y) mod Y;
      if J = 0 then
        begin
          Result := Abs(Y);
          exit;
        end;
      X := Y;
      Y := J;
    end;
  Result := 1;
end;

function IsRelativePrime(const X, Y: Int64): Boolean;
begin
  Result := GCD(X, Y) = 1;
end;


{ Find the modular inverse of A modulo N using Euclid's algorithm.             }
function InvMod(const A, N: Integer): Integer;
var g0, g1, v0, v1, y, z : Integer;
begin
  if N < 2 then
    raise EInvalidArgument.CreateFmt('InvMod: n=%d < 2', [N]);
  if GCD (A, N) <> 1 then
    raise EInvalidArgument.CreateFmt('InvMod: GCD (a=%d, n=%d) <> 1', [A, N]);
  g0 := N;  g1 := A;
  v0 := 0;  v1 := 1;
  while g1 <> 0 do
    begin
      y := g0 div g1;
      z := g1;
      g1 := g0 - y * g1;
      g0 := z;
      z := v1;
      v1 := v0 - y * v1;
      v0 := z;
    end;
  if v0 > 0 then
    Result := v0 else
    Result := v0 + N;
end;

function InvMod(const A, N: Int64): Int64;
var g0, g1, v0, v1, y, z : Int64;
begin
  if N < 2 then
    raise EInvalidArgument.CreateFmt ('InvMod: n=%d < 2', [N]);
  if GCD(A, N) <> 1 then
    raise EInvalidArgument.CreateFmt ('InvMod: GCD(a=%d, n=%d) <> 1', [A, N]);
  g0 := N;  g1 := A;
  v0 := 0;  v1 := 1;
  while g1 <> 0 do
    begin
      y := g0 div g1;
      z := g1;
      g1 := g0 - y * g1;
      g0 := z;
      z := v1;
      v1 := v0 - y * v1;
      v0 := z;
    end;
  if v0 > 0 then
    Result := v0 else
    Result := v0 + N;
end;



{ Calculates x = a^z mod n                                                     }
function ExpMod(A, Z: Integer; const N: Integer): Integer;
var Signed : Boolean;
begin
  Signed := Z < 0;
  if Signed then
    Z := -Z;
  Result := 1;
  while Z <> 0 do
    begin
      while not Odd(Z) do
        begin
          Z := Z shr 1;
          A := (A * Int64(A)) mod N;
        end;
      Dec (Z);
      Result := (Result * Int64(A)) mod N;
    end;
  if Signed then
    Result := InvMod(Result, N);
end;

function ExpMod(A, Z: Int64; const N: Int64): Int64;
var Signed : Boolean;
begin
  Signed := Z < 0;
  if Signed then
    Z := -Z;
  Result := 1;
  while Z <> 0 do
    begin
      while not Odd(Z) do
        begin
          Z := Z shr 1;
          A := (A * Int64(A)) mod N;
        end;
      Dec (Z);
      Result := (Result * Int64(A)) mod N;
    end;
  if Signed then
    Result := InvMod(Result, N);
end;



{ From: Handbook of Applied Cryptography, Algorithm 2.149 (page 73)            }
function Jacobi(const A, N: Integer): Integer;

  // Calculates a=2^s+t
  procedure GetRepresentationBase2(const a: Integer; var s,t: Integer);
  begin
    s := 0;
    t := a;
    while not Odd(T) do
      begin
        t := t shr 1;
        Inc(s);
      end;
  end;

var e, a1, s : Integer;
begin
  // Check for valid input
  if a < 0 then
    raise EInvalidArgument.CreateFmt('Jacobi: a=%d < 0', [a]);
  if a >= n then
    raise EInvalidArgument.CreateFmt('Jacobi: a=%d >= n=%d', [a, n]);
  if not Odd (n) then
    raise EInvalidArgument.CreateFmt('Jacobi: Odd(n=%d) = False', [n]);
  if n < 3 then
    raise EInvalidArgument.CreateFmt('Jacobi: n=%d < 3', [n]);
  if a > 1 then
    begin
      GetRepresentationBase2(a, e, a1);
      s := 1;
      if Odd (e) and (((n mod 8) = 3) or ((n mod 8) = 5)) then
        s := -s;
      if ((n mod 4) = 3) and ((a1 mod 4) = 3) then
        s := -s;
      if a1=1 then
        Result := s
      else
        Result := s * Jacobi(n mod a1, a1);
    end
  else
    Result := a;
end;



{                                                                              }
{ Numerical solvers                                                            }
{                                                                              }
function SecantSolver(const f: fx; const y, Guess1, Guess2: MFloat): MFloat;
var xn, xnm1, xnp1, fxn, fxnm1 : MFloat;
begin
  xnm1 := Guess1;
  xn := Guess2;
  fxnm1 := f(xnm1) - y;
  repeat
    fxn := f(xn) - y;
    xnp1 := xn - fxn * (xn - xnm1) / (fxn - fxnm1);
    fxnm1 := fxn;
    xnm1 := xn;
    xn := xnp1;
  until (f(xn - 0.00000001) - y) * (f(xn + 0.00000001) - y) <= 0.0;
  Result := xn;
end;

function NewtonSolver(const f, df: fx; const y, Guess: MFloat): MFloat;
var xn, xnp1 : MFloat;
begin
  xnp1 := Guess;
  repeat
    xn := xnp1;
    xnp1 := xn - f(xn) / df(xn);
  until Abs(xnp1 - xn) < 0.000000000000001;
  Result := xn;
end;

const h = 1e-15;

function FirstDerivative(const f: fx; const x: MFloat): MFloat;
begin
  Result := (-f(x + 2 * h) + 8 * f(x + h) - 8 * f(x - h) + f(x - 2 * h)) / (12 * h);
end;

function SecondDerivative(const f: fx; const x: MFloat): MFloat;
begin
  Result := (-f(x + 2 * h) + 16 * f(x + h) - 30 * f(x) +
             16 * f(x - h) - f(x - 2 * h)) / (12 * h * h);
end;

function ThirdDerivative(const f: fx; const x: MFloat): MFloat;
begin
  Result := (f(x + 2 * h) - 2 * f(x + h) + 2 * f(x - h) - f(x - 2 * h)) / (2 * h * h * h);
end;

function FourthDerivative(const f: fx; const x: MFloat): MFloat;
begin
  Result := (f(x + 2 * h) - 4 * f(x + h) + 6 * f(x) - 4 * f(x - h) + f(x - 2 * h)) / (h * h * h * h);
end;

function SimpsonIntegration(const f: fx; const a, b: MFloat; N: Integer): MFloat;
var h : MFloat;
    I : Integer;
begin
  if N mod 2 = 1 then
   Inc(N); // N must be multiple of 2
  h := (b - a) / N;
  Result := 0.0;
  for I := 1 to N - 1 do
    Result := Result + ((I mod 2) * 2 + 2) * f(a + (I - 0.5) * h);
  Result := (Result + f(a) + f(b)) * h / 3.0;
end;





{                                                                              }
{ Fast factorial                                                               }
{                                                                              }
{   For small values of N, calculate using 2*3*..*N, and cache result.         }
{   For larger values of N, calculate using Gamma(N+1)                         }
{                                                                              }
const
  FactorialSmallLimit = 34;
  FactorialCacheLimit = 409;

var
  FactorialCache : MFloatArray = nil;

function Factorial(const N: Integer): MFloat;
var L : MFloat;
    I : Integer;
begin
  // Check that arguments are valid
  if N > FactorialMaxN then
    raise EOverflow.Create('Factorial overflow');
  if N < 0 then
    raise EInvalidArgument.Create('Factorial: Invalid argument');

  // Get result from factorial cache
  if (N <= FactorialCacheLimit) and Assigned(FactorialCache) and
     (FactorialCache[N] >= 1.0) then
    begin
      Result := FactorialCache[N];
      exit;
    end;

  // Calculate
  if N < FactorialSmallLimit then
    begin
      L := 1.0;
      I := 2;
      while I <= N do
        begin
          L := L * I;
          Inc(I);
        end;
      Result := L;
    end
  else
    Result := Exp(GammaLn(N + 1));

  // Save result in cache
  if N <= FactorialCacheLimit then
    begin
      if not Assigned(FactorialCache) then
        SetLength(FactorialCache, FactorialCacheLimit + 1);
      FactorialCache[N] := Result;
    end;
end;



{                                                                              }
{ Combinatorics                                                                }
{                                                                              }
function Combinations(const N, C: Integer): MFloat;
begin
  Result := Factorial(N) / (Factorial(C) * Factorial(N - C));
end;

function Permutations(const N, P: Integer): MFloat;
begin
  Result := Factorial(N) / Factorial(N - P);
end;



{                                                                              }
{ Fibonacci (N) = Fibonacci (N - 1) + Fibonacci (N - 2)                        }
{                                                                              }
function Fibonacci(const N: Integer): Int64;
var I      : Integer;
    f1, f2 : Int64;
begin
  if (N < 0) or (N > 92) then
    raise EInvalidArgument.Create('Fibonacci: Invalid argument');
  Result := 1;
  if N = 1 then
    exit;
  f1 := 0;     // fib(0) = 0
  f2 := 1;     // fib(1) = 1
  for I := 1 to N do
    begin
      Result := f1 + f2;
      f2 := f1;
      f1 := Result;
    end;
end;



{                                                                              }
{ Statistical functions                                                        }
{                                                                              }

{ "For arguments greater than 13, the logarithm of the gamma      }
{ function is approximated by the logarithmic version of          }
{ Stirling's formula using a polynomial approximation of          }
{ degree 4. Arguments between -33 and +33 are reduced by          }
{ recurrence to the interval [2,3] of a rational approximation.   }
{ The cosecant reflection formula is employed for arguments       }
{ less than -33.                                                  }
{                                                                 }
{ Arguments greater than MAXLGM return MAXNUM and an error        }
{ message."                                                       }
{                                                                 }
{ Algorithm translated into Delphi by David Butler from Cephes C  }
{ library with permission from Stephen L. Moshier                 }
{ <moshier@na-net.ornl.gov>.                                      }
function GammaLn (X: Extended): Extended;
const MaxLGM = 2.556348e305; // for Extended type
var P, Q, W, Z : Extended;
{ Stirling's formula expansion of log gamma }
const Stir : array[0..4] of MFloat = (
              8.11614167470508450300E-4,
             -5.95061904284301438324E-4,
              7.93650340457716943945E-4,
             -2.77777777730099687205E-3,
              8.33333333333331927722E-2);
{ B[], C[]: log gamma function between 2 and 3 }
      B    : array[0..5] of MFloat = (
             -1.37825152569120859100E3,
             -3.88016315134637840924E4,
             -3.31612992738871184744E5,
             -1.16237097492762307383E6,
             -1.72173700820839662146E6,
             -8.53555664245765465627E5);
      C    : array[0..7] of MFloat = (
              1.00000000000000000000E0,
             -3.51815701436523470549E2,
             -1.70642106651881159223E4,
             -2.20528590553854454839E5,
             -1.13933444367982507207E6,
             -2.53252307177582951285E6,
             -2.01889141433532773231E6,
              1.00000000000000000000E0);

begin
  if X < -34.0 then
    begin
      Q := -X;
      W := GammaLn(Q);
      P := Trunc(Q);
      if P = Q then
        raise EOverflow.Create('GammaLn')
      else
        begin
          Z := Q - P;
          if Z > 0.5 then
            begin
              P := P + 1.0;
              Z := P - Q;
            end;
          Z := Q * Sin(Pi * Z);
          if Z = 0.0 then
            raise EOverflow.Create('GammaLn')
          else
            GammaLn := LnPi - Ln(Z) - W;
        end;
    end
  else
  if X <= 13 then
    begin
      Z := 1.0;
      while X >= 3.0 do
        begin
          X := X - 1.0;
          Z := Z * X;
        end;
      while (X < 2.0) and (X <> 0.0) do
        begin
          Z := Z / X;
          X := X + 1.0;
        end;
      if X = 0.0 then
        raise EOverflow.Create('GammaLn')
      else
      if Z < 0.0 then
        Z := -Z;
      if X = 2.0 then
        GammaLn := Ln(Z)
      else
        begin
          X := X - 2.0;
          P := X * PolyEval(X, B, 5) / PolyEval(X, C, 7);
          GammaLn := Ln(Z) + P;
        end;
    end
  else
  if X > MAXLGM then
    raise EOverflow.Create('GammaLn')
  else
    begin
      Q := (X - 0.5) * Ln(X) - X + LnSqrt2Pi;
      if X > 1.0e8 then
        GammaLn := Q
      else
        begin
          P := 1.0 / (X * X);
          if X >= 1000.0 then
            GammaLn := Q + ((7.9365079365079365079365e-4 * P
                          - 2.7777777777777777777778e-3)
                          * P + 0.0833333333333333333333) / X
          else
            GammaLn := Q + PolyEval(P, Stir, 4) / X;
        end;
    end;
end;



{                                                                              }
{ Fourier Transformations                                                      }
{   FourierTransform is a FFT routine adapted for Delphi from a Pascal         }
{   version by Don Cross.                                                      }
{                                                                              }
procedure FourierTransform(const AngleNumerator: MFloat;
    const RealIn, ImagIn: array of MFloat;
    var RealOut, ImagOut: MFloatArray);
var I, J, N, L            : Word32;
    NumSamples, NumBits   : Word32;
    BlockSize, BlockEnd   : Word32;
    delta_angle, delta_ar : MFloat;
    alpha, beta           : MFloat;
    tr, ti, ar, ai        : MFloat;
    PRe, PIm, QRe, QIm    : ^MFloat;
begin
  NumSamples := Length(RealIn);
  L := Length(ImagIn);
  if (L > 0) and (NumSamples <> L) then
    raise EInvalidArgument.Create('FourierTransform: RealIn and ImagIn must be of equal length');
  if NumSamples < 2 then
    raise EInvalidArgument.Create('FourierTransform: At least two samples required');
  if not IsPowerOfTwo32(NumSamples) then
    raise EInvalidArgument.Create('FourierTransform: NumSamples not a power of two');

  SetLength(RealOut, NumSamples);
  SetLength(ImagOut, NumSamples);

  NumBits := SetBitScanForward32(NumSamples);
  for I := 0 to NumSamples - 1 do
    begin
      J := ReverseBits32 (I, NumBits);
      RealOut[J] := RealIn[I];
      if L > 0 then
        ImagOut[J] := ImagIn[I];
    end;

  BlockEnd := 1;
  BlockSize := 2;
  while BlockSize <= NumSamples do
    begin
      delta_angle := AngleNumerator / BlockSize;
      alpha := Sin (0.5 * delta_angle);
      alpha := 2.0 * alpha * alpha;
      beta := Sin (delta_angle);

      I := 0;
      while I < NumSamples do
        begin
          ar := 1.0;    { cos(0) }
          ai := 0.0;    { sin(0) }

          PRe := @RealOut[I];
          PIm := @ImagOut[I];
          J := I + BlockEnd;
          QRe := @RealOut[J];
          QIm := @ImagOut[J];
          for N := 0 to BlockEnd - 1 do
            begin
              tr := ar * QRe^ - ai * QIm^;
              ti := ar * QIm^ + ai * QRe^;
              QRe^ := PRe^ - tr;
              QIm^ := PIm^ - ti;
              PRe^ := PRe^ + tr;
              PIm^ := PIm^ + ti;
              delta_ar := alpha * ar + beta * ai;
              ai := ai - (alpha * ai - beta * ar);
              ar := ar - delta_ar;
              Inc(PRe);
              Inc(PIm);
              Inc(QRe);
              Inc(QIm);
            end;

          Inc(I, BlockSize);
        end;

      BlockEnd := BlockSize;
      BlockSize := BlockSize shl 1;
    end;
end;

procedure FFT(const RealIn, ImagIn: array of MFloat; var RealOut, ImagOut: MFloatArray);
begin
  FourierTransform(Pi2, RealIn, ImagIn, RealOut, ImagOut);
end;

procedure InverseFFT(const RealIn, ImagIn: array of MFloat; var RealOut, ImagOut: MFloatArray);
var I, N : Integer;
begin
  FourierTransform (-Pi2, RealIn, ImagIn, RealOut, ImagOut);
  { Normalize the resulting time samples }
  N := Length(RealOut);
  for I := 0 to N - 1 do
    begin
      RealOut[I] := RealOut[I] / N;
      ImagOut[I] := ImagOut[I] / N;
    end;
end;

procedure CalcFrequency(const FrequencyIndex: Integer;
    const RealIn, ImagIn: array of MFloat;
    var RealOut, ImagOut: MFloat);
var K, NumSamples                 : Integer;
    cos1, cos2, cos3, theta, beta : MFloat;
    sin1, sin2, sin3              : MFloat;
begin
  NumSamples := Length(RealIn);
  if NumSamples <> Length(ImagIn) then
    raise EInvalidArgument.Create('CalcFrequency: RealIn and ImagIn must be of equal length');
  RealOut := 0.0;
  ImagOut := 0.0;
  theta := Pi2 * FrequencyIndex / NumSamples;
  sin1 := sin (-2 * theta);
  sin2 := sin (-theta);
  cos1 := cos (-2 * theta);
  cos2 := cos (-theta);
  beta := 2 * cos2;
  for K := 0 to NumSamples - 1 do
    begin
      sin3 := beta * sin2 - sin1;
      sin1 := sin2;
      sin2 := sin3;

      cos3 := beta * cos2 - cos1;
      cos1 := cos2;
      cos2 := cos3;

      RealOut := RealOut + RealIn[K] * cos3 - ImagIn[K] * sin3;
      ImagOut := ImagOut + ImagIn[K] * cos3 + RealIn[K] * sin3;
    end;
end;



{                                                                              }
{ Tests                                                                        }
{                                                                              }
{$IFDEF MATHS_TEST}
{$ASSERTIONS ON}
procedure Test;
var A : Int64Array;
begin
  Assert(flcMaths.Sign(0) = 0,   'Sgn');
  Assert(flcMaths.Sign(1) = 1,   'Sgn');
  Assert(flcMaths.Sign(-1) = -1, 'Sgn');
  Assert(flcMaths.Sign(2) = 1,   'Sgn');
  Assert(flcMaths.Sign(1.1) = 1, 'Sgn');

  Assert(not IsPrime(0),       'IsPrime(0)');
  Assert(not IsPrime(1),       'IsPrime(1)');
  Assert(IsPrime(2),           'IsPrime(2)');
  Assert(IsPrime(3),           'IsPrime(3)');
  Assert(IsPrime(-3),          'IsPrime(-3)');
  Assert(not IsPrime(4),       'IsPrime(4)');
  Assert(not IsPrime(-4),      'IsPrime(-4)');
  Assert(IsPrime(257),         'IsPrime(257)');
  Assert(not IsPrime($10002),  'IsPrime($10002)');
  Assert(IsPrime($10003),      'IsPrime($10003)');

  Assert(IsPrimeFactor(17 * 3, 17),    'IsPrimeFactor');
  Assert(IsPrimeFactor(17 * 3, 3),     'IsPrimeFactor');
  Assert(not IsPrimeFactor(17 * 3, 2), 'IsPrimeFactor');
  Assert(not IsPrimeFactor(17, 1),     'IsPrimeFactor');
  Assert(IsPrimeFactor(17, 17),        'IsPrimeFactor');

  A := PrimeFactors(17 * 3);
  Assert(Length(A) = 2, 'PrimeFactors');
  Assert(A[0] = 3,      'PrimeFactors');
  Assert(A[1] = 17,     'PrimeFactors');

  A := PrimeFactors(2 * 2 * 5 * 11 * 19);
  Assert(Length(A) = 4, 'PrimeFactors');
  Assert(A[0] = 2,      'PrimeFactors');
  Assert(A[1] = 5,      'PrimeFactors');
  Assert(A[2] = 11,     'PrimeFactors');
  Assert(A[3] = 19,     'PrimeFactors');

  Assert(GammaLn(2.0) = 0.0, 'GammaLn');

  Assert(Factorial(0) = 1);
  Assert(Factorial(1) = 1);
  Assert(Factorial(2) = 2);
  Assert(Factorial(3) = 6);
  Assert(Factorial(FactorialMaxN) > 1);
end;
{$ENDIF}



initialization
  {$IFDEF DELPHI6_UP}
  SetFPUPrecisionExtended;
  SetFPURoundingNearest;
  {$ENDIF}
finalization
end.

