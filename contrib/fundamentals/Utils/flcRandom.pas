{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals 5.00                                        }
{   File name:        flcRandom.pas                                            }
{   File version:     5.20                                                     }
{   Description:      Random number functions                                  }
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
{ Revision history:                                                            }
{                                                                              }
{   1999/11/07  0.01  Add RandomSeed.                                          }
{   1999/12/01  0.02  Add RandomUniform.                                       }
{   1999/12/03  0.03  Add RandomNormal.                                        }
{   2000/01/23  1.04  Add RandomPseudoWord.                                    }
{   2000/07/13  1.05  Fix bug reported by Andrew Driazgov.                     }
{   2000/08/22  1.06  Add RandomHex.                                           }
{   2000/09/20  1.07  Improve RandomSeed.                                      }
{   2002/06/01  3.08  Create cRandom unit.                                     }
{   2003/08/09  3.09  Replace random number generator.                         }
{   2005/06/10  4.10  Compilable with FreePascal 2 Win32 i386.                 }
{   2005/08/27  4.11  Revised for Fundamentals 4.                              }
{   2007/06/08  4.12  Compilable with FreePascal 2.04 Win32 i386               }
{   2010/06/27  4.13  Compilable with FreePascal 2.4.0 OSX x86-64              }
{   2015/04/19  4.14  Changes for 64-bit compilers and RawByteString           }
{   2015/04/20  4.15  Revise RandomSeed                                        }
{   2015/05/06  4.16  Prevent mwcRandom32 overflow error.                      }
{   2016/01/09  5.17  Revised for Fundamentals 5.                              }
{   2018/08/12  5.18  String type changes.                                     }
{   2019/03/22  5.19  FreePascal 3.04 Win64 changes.                           }
{   2019/06/06  5.20  Add RandomBytes.                                         }
{                                                                              }
{ Supported compilers:                                                         }
{                                                                              }
{   Delphi 2010-10.4 Win32/Win64        5.20  2020/06/02                       }
{   Delphi 10.2-10.4 Linux64            5.20  2020/06/02                       }
{   FreePascal 3.0.4 Win64              5.20  2020/06/02                       }
{                                                                              }
{******************************************************************************}

// See http://www.romu-random.org/code.c

{$INCLUDE ..\flcInclude.inc}

{$IFDEF FREEPASCAL}{$IFDEF DEBUG}
  {$WARNINGS OFF}{$HINTS OFF}
{$ENDIF}{$ENDIF}

unit flcRandom;

interface

uses
  { System }
  SysUtils,

  { Fundamentals }
  flcStdTypes;



{                                                                              }
{ RandomSeed                                                                   }
{                                                                              }
{   RandomSeed returns a random seed value based on various system states.     }
{   AddEntropy can be called to add additional random state to the values      }
{     returned by RandomSeed.                                                  }
{                                                                              }
procedure AddEntropy(const Value: Int64);
function  RandomSeed32: Word32;



{                                                                              }
{ Uniform random number generator                                              }
{                                                                              }
{   Returns a random number from a uniform density distribution (ie all        }
{     values have an equal probability of being 'chosen')                      }
{   RandomFloat returns an uniformly distributed random floating point value   }
{     between 0 and 1.                                                         }
{   RandomAlphaStr returns a string of random letters (A-Z).                   }
{   RandomPseudoWord returns a random word-like string.                        }
{                                                                              }
procedure SetRandomSeed(const Seed: Word32);

function  RandomUniform32: Word32;
function  RandomUniform64: Word64;
function  RandomUniform(const N: Integer): Integer;
function  RandomUniform16: Word;
function  RandomByte: Byte;
function  RandomByteNonZero: Byte;
function  RandomBoolean: Boolean;
function  RandomInt64: Int64; overload;
function  RandomInt64(const N: Int64): Int64; overload;

function  RandomBytes(const N: Integer): TBytes;

function  RandomHex(const Digits: Integer; const UpperCase: Boolean = True): String;
function  RandomHexB(const Digits: Integer; const UpperCase: Boolean = True): UTF8String;
function  RandomHexU(const Digits: Integer; const UpperCase: Boolean = True): UnicodeString;

function  RandomFloat: Extended;

function  RandomUpperAlphaStrB(const Length: Integer): UTF8String;
function  RandomPseudoWordB(const Length: Integer): UTF8String;
function  RandomPasswordB(const MinLength, MaxLength: Integer;
          const CaseSensitive, UseSymbols, UseNumbers: Boolean): UTF8String;



{                                                                              }
{ Alternative uniform random number generators                                 }
{                                                                              }
function  mwcRandom32: Word32;
function  urnRandom32: Word32;
function  moaRandomFloat: Extended;
function  mwcRandomFloat: Extended;



{                                                                              }
{ Normal distribution random number generator                                  }
{                                                                              }
{   RandomNormalF returns a random number that has a Normal(0,1) distribution  }
{     (Gaussian distribution)                                                  }
{                                                                              }
function  RandomNormalF: Extended;



{                                                                              }
{ Test cases                                                                   }
{                                                                              }
{$IFDEF DEBUG}
{$IFDEF TEST}
procedure Test;
{$ENDIF}
{$ENDIF}



implementation

uses
  { System }
  {$IFDEF MSWIN}
  Windows
  {$ENDIF}

  {$IFDEF UNIX}
  {$IFDEF FREEPASCAL}
  BaseUnix,
  Unix
  {$ENDIF}
  {$ENDIF}

  {$IFDEF POSIX}
  {$IFDEF DELPHI}
  Posix.SysTime
  {$ENDIF}
  {$ENDIF}
  ;



{                                                                              }
{ Linear Congruential Random Number Generators                                 }
{   The general form of a linear congruential generator is:                    }
{   SEED = (A * SEED + C) mod M                                                }
{                                                                              }
function lcRandom1(const Seed: Word32): Word32;
begin
  Result := Word32(29943829 * Int64(Seed) - 1);
end;

function lcRandom2(const Seed: Word32): Word32;
begin
  Result := Word32(69069 * Int64(Seed) + 1);
end;

function lcRandom3(const Seed: Word32): Word32;
begin
  Result := Word32(1103515245 * Int64(Seed) + 12345);
end;

function lcRandom4(const Seed: Word32): Word32;
begin
  Result := Word32(214013 * Int64(Seed) + 2531011);
end;

function lcRandom5(const Seed: Word32): Word32;
begin
  Result := Word32(134775813 * Int64(Seed) + 1);
end;



{                                                                              }
{ System sources of pseudo-randomness                                          }
{                                                                              }
{$IFDEF WindowsPlatform}
function GetHighPrecisionCounter: Int64;
begin
  QueryPerformanceCounter(Result);
end;
{$ENDIF}

{$IFDEF UNIX}
{$IFDEF FREEPASCAL}
function GetHighPrecisionCounter: Int64;
var TV : TTimeVal;
    TZ : PTimeZone;
begin
  TZ := nil;
  fpGetTimeOfDay(@TV, TZ);
  Result := Int64(TV.tv_sec) * 1000000 + Int64(TV.tv_usec);
end;
{$ENDIF}
{$ENDIF}

{$IFDEF POSIX}
{$IFDEF DELPHI}
function GetHighPrecisionCounter: Int64;
var T : timeval;
begin
  GetTimeOfDay(T, nil);
  Result := Int64(T.tv_sec) * 1000000 + Int64(T.tv_usec);
end;
{$ENDIF}
{$ENDIF}

{$IFDEF WindowsPlatform}
function GetTick: Word32;
begin
  Result := GetTickCount;
end;
{$ELSE}{$IFDEF UNIX}
function GetTick: Word32;
begin
  Result := Word32(DateTimeToTimeStamp(Now).Time);
end;
{$ELSE}
{$IFDEF POSIX}
function GetTick: Word32;
begin
  Result := Word32(DateTimeToTimeStamp(Now).Time);
end;
{$ENDIF}
{$ENDIF}{$ENDIF}

function RandomState: Int64;
var
  H, Mi, S, S1 : Word;
  Ye, Mo, Da   : Word;
begin
  Result := 0;
  { Counters }
  {$IFNDEF ANDROID}
  Result := Result xor GetHighPrecisionCounter;
  Result := Result xor (Int64(GetTick) shl 32);
  {$ENDIF}
  { System Time }
  DecodeTime(Time, H, Mi, S, S1);
  Result := Result xor Int64(H) xor (Int64(Mi) shl 8) xor (Int64(S1) shl 16) xor (Int64(S) shl 24);
  { System Date }
  DecodeDate(Date, Ye, Mo, Da);
  Result := Result xor (Int64(Ye) shl 32) xor (Int64(Mo) shl 48) xor (Int64(Da) shl 56);
end;

function HashBuffer(const Buffer: PByte; const Len: Integer): Word32;
var
  I : Integer;
  P : PByte;
begin
  Result := 0;
  P := Buffer;
  for I := 1 to Len do
    begin
      Result := Result xor (P^ shl ((I mod 7) * 4));
      Inc(P);
    end;
end;

function StrHashB(const S: RawByteString): Word32;
var
  L : Integer;
begin
  Result := 0;
  L := Length(S);
  if L <= 0 then
    exit;
  Result := HashBuffer(@S[1], Length(S));
end;

{$IFDEF MSWIN}
function GetCPUFrequency: Int64;
var
  F : Int64;
begin
  F := 0;
  if not QueryPerformanceFrequency(F) then
    F := 0;
  Result := F;
end;
{$ENDIF}

{$IFDEF MSWIN}
function StrLenA(const A: PAnsiChar): Integer;
var L : Integer;
begin
  if not Assigned(A) then
    begin
      Result := 0;
      exit;
    end;
  L := 0;
  while A[L] <> #0 do
    Inc(L);
  Result := L;
end;

function StrZPasB(const A: PAnsiChar): UTF8String;
var
  I, L : Integer;
begin
  L := StrLenA(A);
  SetLength(Result, L);
  if L = 0 then
    exit;
  I := 0;
  while I < L do
    begin
      Result[I + 1] := A[I];
      Inc(I);
    end;
end;

function GetOSUserName: UTF8String;
var
  L : Word32;
  B : array[0..258] of Byte;
begin
  L := 256;
  FillChar(B[0], Sizeof(B), 0);
  if GetUserNameA(@B, L) then
    Result := StrZPasB(@B)
  else
    Result := '';
end;

function GetOSComputerName: UTF8String;
var
  L : Word32;
  B : array[0..258] of Byte;
begin
  L := 256;
  FillChar(B[0], Sizeof(B), 0);
  if GetComputerNameA(@B, L) then
    Result := StrZPasB(@B)
  else
    Result := '';
end;
{$ENDIF}

{$IFDEF UNIX}
function GetOSUserName: UTF8String;
var
  T : RawByteString;
begin
  T := GetEnvironmentVariable('USER');
  if T = '' then
    T := GetEnvironmentVariable('USERNAME');
  Result := T;
end;

function GetOSComputerName: UTF8String;
begin
  Result := GetEnvironmentVariable('HOSTNAME');
end;
{$ENDIF}

{$IFDEF MSWIN}
function WinRandomState: Int64;
var
  F : Word32;
  H : THandle;
  T1, T2, T3, T4 : TFileTime;
  A, B : Word32;
  S : Int64;
begin
  S := 0;
  { Thread times }
  F := GetCurrentThreadID;
  S := S xor F;
  H := GetCurrentThread;
  S := S xor Int64(H);
  GetThreadTimes(H, T1, T2, T3, T4);
  A := T1.dwLowDateTime  xor T2.dwLowDateTime  xor T3.dwLowDateTime  xor T4.dwLowDateTime;
  B := T1.dwHighDateTime xor T2.dwHighDateTime xor T3.dwHighDateTime xor T4.dwHighDateTime;
  S := S xor A;
  S := S xor (Int64(B) shl 32);
  { Process times }
  F := GetCurrentProcessId;
  S := S xor F;
  H := GetCurrentProcess;
  S := S xor Int64(H);
  GetProcessTimes(H, T1, T2, T3, T4);
  A := T1.dwLowDateTime  xor T2.dwLowDateTime  xor T3.dwLowDateTime  xor T4.dwLowDateTime;
  B := T1.dwHighDateTime xor T2.dwHighDateTime xor T3.dwHighDateTime xor T4.dwHighDateTime;
  S := S xor A;
  S := S xor (Int64(B) shl 32);
  { System times }
  {$IFDEF DELPHI2010_UP}
  GetSystemTimes(T1, T2, T3);
  A := T1.dwLowDateTime  xor T2.dwLowDateTime  xor T3.dwLowDateTime;
  B := T1.dwHighDateTime xor T2.dwHighDateTime xor T3.dwHighDateTime;
  S := S xor A;
  S := S xor (Int64(B) shl 32);
  {$ENDIF}
  Result := S;
end;
{$ENDIF}



{                                                                              }
{ RandomSeed                                                                   }
{   The random seed is generated from a startup seed, a fixed seed, a          }
{   variable seed and an entropy seed.                                         }
{   The startup seed is initialised on module initialisation.                  }
{   The fixed seed is randomised on the first call to RandomSeed.              }
{   The variable seed is randomised on every call to RandomSeed.               }
{                                                                              }
var
  EntropySeed   : Int64 = 0;
  StartupSeed   : Int64 = 0;
  FixedSeedInit : Boolean = False;
  FixedSeed     : Int64 = 0;
  VariableSeed  : Int64 = 0;

function SeedMix1(const A, B: Word32): Int64;
begin
  Result :=
       Int64(lcRandom3(A)) or
      (Int64(lcRandom4(B)) shl 32);
end;

function SeedMix2(const A, B: Word32): Int64;
begin
  Result :=
       Int64(lcRandom1(A)) or
      (Int64(lcRandom2(B)) shl 32);
end;

function SeedMix3(const A, B: Word32): Int64;
begin
  Result :=
       Int64(lcRandom2(A)) or
      (Int64(lcRandom5(B)) shl 32);
end;

function SeedMix4(const A, B: Word32): Int64;
begin
  Result :=
       Int64(lcRandom4(A)) or
      (Int64(lcRandom2(B)) shl 32);
end;

function SeedMix5(const A, B: Word32): Int64;
begin
  Result :=
       Int64(lcRandom5(A)) or
      (Int64(lcRandom1(B)) shl 32);
end;

function SeedMix1_64(const S: Int64): Int64;
begin
  Result := SeedMix1(Word32(S), Word32(S shr 32));
end;

function SeedMix2_64(const S: Int64): Int64;
begin
  Result := SeedMix2(Word32(S), Word32(S shr 32));
end;

function SeedMix3_64(const S: Int64): Int64;
begin
  Result := SeedMix3(Word32(S), Word32(S shr 32));
end;

function SeedMix4_64(const S: Int64): Int64;
begin
  Result := SeedMix4(Word32(S), Word32(S shr 32));
end;

function SeedMix5_64(const S: Int64): Int64;
begin
  Result := SeedMix5(Word32(S), Word32(S shr 32));
end;

procedure AddEntropy(const Value: Int64);
var
  S : Int64;
begin
  S := EntropySeed xor Value;
  S := SeedMix1_64(S);
  EntropySeed := S;
end;

// The StartupSeed is initialised on module initialisation
procedure InitStartupSeed;
var
  S : Int64;
begin
  { Initialise startup seed }
  S := RandomState;
  S := SeedMix2_64(S);
  StartupSeed := S;
  { Initialise entropy seed }
  AddEntropy(RandomState);
end;

// The FixedSeed is initialised on the first call to RandomSeed
{$IFDEF DELPHI5}{$OPTIMIZATION OFF}{$ENDIF}
{$IFOPT Q+}{$DEFINE QOn}{$Q-}{$ELSE}{$UNDEF QOn}{$ENDIF}
procedure InitFixedSeed;
var
  S : Int64;
  Q : Pointer;
begin
  { Startup Seed }
  S := StartupSeed;
  { System State }
  S := S xor RandomState;
  {$IFDEF MSWIN}
  S := S xor WinRandomState;
  {$ENDIF}
  { Pointer Values }
  Q := @FixedSeed; // Global variable
  S := Int64(S + Int64(NativeUInt(Q)));
  Q := @S; // Local variable
  S := Int64(S + Int64(NativeUInt(Q)));
  GetMem(Q, 17); // Heap memory
  S := Int64(S + Int64(NativeUInt(Q)));
  FreeMem(Q);
  {$IFDEF MSWIN}
  { CPU Frequency }
  S := S xor GetCPUFrequency;
  { OS User Name }
  S := Int64(S + StrHashB(GetOSUserName));
  { OS Computer Name }
  S := Int64(S + StrHashB(GetOSComputerName));
  {$ENDIF}
  {$IFDEF UNIX}
  { OS User Name }
  S := Int64(S + Int64(StrHashB(GetOSUserName)));
  { OS Computer Name }
  S := Int64(S + Int64(StrHashB(GetOSComputerName)));
  { PPID }
  S := Int64(S + Int64(StrHashB(GetEnvironmentVariable('PPID'))));
  {$ENDIF}
  { System Timing }
  S := Int64(S + RandomState);
  Sleep(0);
  S := Int64(S + RandomState);
  Sleep(1);
  S := Int64(S + RandomState);
  {$IFDEF MSWIN}
  S := Int64(S + WinRandomState);
  {$ENDIF}
  Sleep(0);
  S := Int64(S + RandomState);
  { Mix bits }
  S := SeedMix3_64(S);
  { Save fixed seed }
  FixedSeed := S;
  FixedSeedInit := True;
end;
{$IFDEF QOn}{$Q+}{$ENDIF}
{$IFDEF DELPHI5}{$OPTIMIZATION ON}{$ENDIF}

{$IFDEF DELPHI5}{$OPTIMIZATION OFF}{$ENDIF}
{$IFOPT Q+}{$DEFINE QOn}{$Q-}{$ELSE}{$UNDEF QOn}{$ENDIF}
function RandomSeed32: Word32;
var
  S : Int64;
begin
  { Fixed Seed }
  if not FixedSeedInit then
    InitFixedSeed;
  S := FixedSeed;
  { Entropy Seed }
  S := Int64(S + EntropySeed);
  { Variable Seed }
  S := Int64(S + VariableSeed);
  { System State }
  S := Int64(S + RandomState);
  {$IFDEF MSWIN}
  S := Int64(S + WinRandomState);
  {$ENDIF}
  { Mix bits }
  S := SeedMix5_64(S);
  { Update variable seed }
  VariableSeed := VariableSeed xor S;
  VariableSeed := SeedMix4_64(VariableSeed);
  { Mix/Reduce seed into result }
  Result := Word32(S) xor
            Word32(S shr 32);
end;
{$IFDEF QOn}{$Q+}{$ENDIF}
{$IFDEF DELPHI5}{$OPTIMIZATION ON}{$ENDIF}

procedure RandomSeedFinalise;
begin
  EntropySeed  := 0;
  StartupSeed  := 0;
  FixedSeed    := 0;
  VariableSeed := 0;
end;



{                                                                              }
{ Mother-of-All pseudo random number generator                                 }
{   This is a multiply-with-carry or recursion-with-carry generator.           }
{   It has a cycle length of 3E+47.                                            }
{   It was invented by George Marsaglia.                                       }
{                                                                              }
var
  moaSeeded : Boolean = False;
  moaX      : array[0..3] of Word32;
  moaC      : Word32;

procedure moaInitSeed(const Seed: Word32);
var
  I : Integer;
  S : Word32;
begin
  S := Seed;
  for I := 0 to 3 do
    begin
      S := lcRandom1(S);
      moaX[I] := S;
    end;
  moaC := lcRandom1(S);
  moaSeeded := True;
end;

function moaRandom32: Word32;
var
  S  : Int64;
  Xn : Word32;
begin
  if not moaSeeded then
    moaInitSeed(RandomSeed32);
  S := 2111111111 * Int64(moaX[0]) +
             1492 * Int64(moaX[1]) +
             1776 * Int64(moaX[2]) +
             5115 * Int64(moaX[3]) +
                    Int64(moaC);
  moaC := Word32(S shr 32);
  Xn := Word32(S);
  moaX[0] := moaX[1];
  moaX[1] := moaX[2];
  moaX[2] := moaX[3];
  moaX[3] := Xn;
  Result := Xn;
end;

function moaRandomFloat: Extended;
begin
  Result := moaRandom32 / High(Word32);
end;

procedure moaFinalise;
begin
  if moaSeeded then
    begin
      moaX[0] := 0;
      moaX[1] := 0;
      moaX[2] := 0;
      moaX[3] := 0;
      moaC := 0;
    end;
end;



{                                                                              }
{ Multiply-With-Carry pseudo random number generator mentioned by George       }
{ Marsaglia in his paper on the Mother-of-All generator:                       }
{   " Here is an interesting simple MWC generator with period > 2^92, for      }
{   32-bit arithmetic:                                                         }
{   x[n]=1111111464*(x[n-1]+x[n-2]) + carry mod 2^32.                          }
{   Suppose you have functions, say top() and bot(), that give the top and     }
{   bottom halves of a 64-bit result.  Then, with initial 32-bit x, y and      }
{   carry c,  simple statements such as                                        }
{          y=bot(1111111464*(x+y)+c)                                           }
{          x=y                                                                 }
{          c=top(y)                                                            }
{   will, repeated, give over 2^92 random 32-bit y's. "                        }
{                                                                              }
var
  mwcSeeded : Boolean = False;
  mwcX      : Word32;
  mwcY      : Word32;
  mwcC      : Word32;

procedure mwcInitSeed(const Seed: Word32);
begin
  mwcX := lcRandom2(Seed);
  mwcY := lcRandom2(mwcX);
  mwcC := lcRandom2(mwcY);
  mwcSeeded := True;
end;

function mwcRandom32: Word32;
var S, T : UInt64;
begin
  if not mwcSeeded then
    mwcInitSeed(RandomSeed32);
  S := 1111111464;
  {$IFOPT Q+}{$DEFINE QOn}{$Q-}{$ELSE}{$UNDEF QOn}{$ENDIF}
  T := mwcX;
  T := T + mwcY;
  S := S * T;
  S := S + mwcC;
  {$IFDEF QOn}{$Q+}{$ENDIF}
  Result := Word32(S);
  mwcX := mwcY;
  mwcY := Result;
  mwcC := Word32(S shr 32);
end;

function mwcRandomFloat: Extended;
begin
  Result := mwcRandom32 / High(Word32);
end;

procedure mwcFinalise;
begin
  if mwcSeeded then
    begin
      mwcX := 0;
      mwcY := 0;
      mwcC := 0;
    end;
end;



{                                                                              }
{ Universal random number generator proposed by Marsaglia, Zaman, and Tsang.   }
{ FSU-SCRI-87-50                                                               }
{   It has a period of 2^144 = 2E+43.                                          }
{   Only 24 bits are guarantueed to be completely random.                      }
{   This generator passes all known statistical tests on randomness.           }
{   The algorithm is a combination of a Fibonacci sequence and an arithmetic   }
{   sequence.                                                                  }
{                                                                              }
var
  urnSeeded : Boolean = False;
  urnU      : array[1..97] of Double;
  urnC      : Double;
  urnCD     : Double;
  urnCM     : Double;
  urnI      : Integer;
  urnJ      : Integer;

procedure urnInit(const IJ, KL: Integer);
var I, J, K, L : Integer;
    F, G, M    : Integer;
    S, T       : Double;
begin
  Assert((IJ >= 0) and (IJ <= 31328) and (KL >= 0) and (KL <= 30081));
  I := (IJ div 177) mod 177 + 2;
  J := IJ mod 177 + 2;
  K := (KL div 169) mod 178 + 1;
  L := KL mod 169;
  for F := 1 to 97 do
    begin
      S := 0.0;
      T := 0.5;
      for G := 1 to 24 do
        begin
          M := (((I * J) mod 179) * K) mod 179;
          I := J;
          J := K;
          K := M;
          L := (53 * L + 1) mod 169;
          if ((L * M) mod 64 >= 32) then
            S := S + T;
          T := T * 0.5;
        end;
      urnU[F] := S;
    end;
  urnC  := 362436.0 / 16777216.0;
  urnCD := 7654321.0 / 16777216.0;
  urnCM := 16777213.0 / 16777216.0;
  urnI  := 97;
  urnJ  := 33;
  urnSeeded := True;
end;

procedure urnInitSeed(const Seed: Word32);
begin
  urnInit((Seed and $FFFF) mod 30000, (Seed shr 16) mod 30000);
end;

function urnRandomFloat: Double;
var R : Double;
begin
  if not urnSeeded then
    urnInitSeed(RandomSeed32);
  R := urnU[urnI] - urnU[urnJ];
  if R < 0.0 then
    R := R + 1.0;
  urnU[urnI] := R;
  Dec(urnI);
  if urnI = 0 then
    urnI := 97;
  Dec(urnJ);
  if urnJ = 0 then
    urnJ := 97;
  urnC := urnC - urnCD;
  if urnC < 0.0 then
    urnC := urnC + urnCM;
  R := R - urnC;
  if R < 0.0 then
    R := R + 1.0;
  Result := R;
end;

function urnRandom32: Word32;
begin
  Result := Word32(Trunc(urnRandomFloat * 4294967295.0));
end;

procedure urnFinalise;
var
  I : Integer;
begin
  if urnSeeded then
    begin
      for I := 1 to 97 do
        urnU[I] := 0.0;
      urnC := 0.0;
      urnCD := 0.0;
      urnCM := 0.0;
      urnI := 0;
      urnJ := 0;
    end;
end;



{                                                                              }
{ Uniform Random                                                               }
{                                                                              }
procedure SetRandomSeed(const Seed: Word32);
begin
  moaInitSeed(Seed);
end;

function RandomUniform32: Word32;
begin
  Result := moaRandom32;
end;

function RandomUniform64: Word64;
begin
  Result :=
      Word64(moaRandom32) or
      Word64(Word64(moaRandom32) shl 32);
end;

function RandomUniform(const N: Integer): Integer;
begin
  if N <= 1 then
    Result := 0
  else
    Result := Integer(RandomUniform32 mod Word32(N));
end;

function RandomUniform16: Word;
var I : Word32;
begin
  I := RandomUniform32;
  I := I xor (I shr 16);
  Result := Word(I and $FFFF);
end;

function RandomByte: Byte;
var I : Word32;
begin
  I := RandomUniform32;
  I := I xor (I shr 8) xor (I shr 16) xor (I shr 24);
  Result := Byte(I and $FF);
end;

function RandomByteNonZero: Byte;
begin
  repeat
    Result := RandomByte;
  until Result <> 0;
end;

function RandomBoolean: Boolean;
begin
  Result := RandomUniform32 and 1 = 1;
end;

function RandomFloat: Extended;
begin
  Result := urnRandomFloat;
end;

function RandomInt64: Int64;
begin
  Result :=
     Int64(RandomUniform32) or
     Int64(Int64(RandomUniform32) shl 32);
end;

function RandomInt64(const N: Int64): Int64;
begin
  if N <= 0 then
    Result := 0
  else
    begin
      Result := RandomInt64;
      if Result < 0 then
        Result := -Result;
      Result := Result mod N;
    end;
end;

function RandomBytes(const N: Integer): TBytes;
var
  B : TBytes;
  P, Q : PByte;
  I, L : Integer;
  R : Word32;
begin
  if N <= 0 then
    begin
      Result := nil;
      exit;
    end;
  SetLength(B, N);
  P := Pointer(B);
  L := N div 4;
  for I := 0 to L - 1 do
    begin
      PWord32(P)^ := RandomUniform32;
      Inc(P, SizeOf(Word32));
    end;
  L := N mod 4;
  if L > 0 then
    begin
      R := RandomUniform32;
      Q := @R;
      for I := 0 to L - 1 do
        begin
          P^ := Q^;
          Inc(P);
          Inc(Q);
        end;
    end;
  Result := B;
end;

const
  HexDigitsHi  : String        = '0123456789ABCDEF';
  HexDigitsHiA : UTF8String    = '0123456789ABCDEF';
  HexDigitsHiU : UnicodeString = '0123456789ABCDEF';
  HexDigitsLo  : String        = '0123456789abcdef';
  HexDigitsLoA : UTF8String    = '0123456789abcdef';
  HexDigitsLoU : UnicodeString = '0123456789abcdef';

function RandomHex(const Digits: Integer; const UpperCase: Boolean): String;
var
  I : Integer;
  D : Integer;
  C : Char;
begin
  if Digits <= 0 then
    begin
      Result := '';
      exit;
    end;
  SetLength(Result, Digits);
  for I := 1 to Digits do
    begin
      D := 1 + RandomUniform(16);
      if UpperCase then
        C := HexDigitsHi[D]
      else
        C := HexDigitsLo[D];
      Result[I] := C;
    end;
end;

function RandomHexB(const Digits: Integer; const UpperCase: Boolean): UTF8String;
var
  I : Integer;
  D : Integer;
  C : UTF8Char;
begin
  if Digits <= 0 then
    begin
      Result := '';
      exit;
    end;
  SetLength(Result, Digits);
  for I := 1 to Digits do
    begin
      D := 1 + RandomUniform(16);
      if UpperCase then
        C := HexDigitsHiA[D]
      else
        C := HexDigitsLoA[D];
      Result[I] := C;
    end;
end;

function RandomHexU(const Digits: Integer; const UpperCase: Boolean): UnicodeString;
var
  I : Integer;
  D : Integer;
  C : WideChar;
begin
  if Digits <= 0 then
    begin
      Result := '';
      exit;
    end;
  SetLength(Result, Digits);
  for I := 1 to Digits do
    begin
      D := 1 + RandomUniform(16);
      if UpperCase then
        C := HexDigitsHiU[D]
      else
        C := HexDigitsLoU[D];
      Result[I] := C;
    end;
end;

function RandomUpperAlphaStrB(const Length: Integer): UTF8String;
var
  I : Integer;
begin
  if Length <= 0 then
    begin
      Result := '';
      exit;
    end;
  SetLength(Result, Length);
  for I := 1 to Length do
    Result[I] := AnsiChar(Ord('A') + RandomUniform(26));
end;

const
  Vowels         = 'AEIOUY';
  VowelCount     = Length(Vowels);
  Consonants     = 'BCDFGHJKLMNPQRSTVWXZ';
  ConsonantCount = Length(Consonants);

function RandomPseudoWordB(const Length: Integer): UTF8String;
var
  I, A, P, T : Integer;
begin
  if Length <= 0 then
    begin
      Result := '';
      exit;
    end;
  SetLength(Result, Length);
  P := -1;
  A := RandomUniform(2);
  for I := 1 to Length do
    begin
      case A of
        0 : Result[I] := AnsiChar(Vowels[RandomUniform(VowelCount) + 1]);
        1 : Result[I] := AnsiChar(Consonants[RandomUniform(ConsonantCount) + 1]);
      end;
      T := A;
      if A = P then
        A := A xor 1
      else
        A := RandomUniform(2);
      P := T;
    end;
end;

const
  PasswordSymbolChars = '!?@%$&-*#';
  PasswordSymbolCharCount = Length(PasswordSymbolChars);
  PasswordNumberChars = '0123456789';
  PasswordNumberCharCount = Length(PasswordNumberChars);

function RandomPasswordB(const MinLength, MaxLength: Integer;
         const CaseSensitive, UseSymbols, UseNumbers: Boolean): UTF8String;
var
  I, J, K, N, Length : Integer;
  C : AnsiChar;
begin
  if (MaxLength <= 0) or (MaxLength < MinLength) then
    begin
      Result := '';
      exit;
    end;
  if MinLength = MaxLength then
    Length := MinLength
  else
    Length := MinLength + RandomUniform(MaxLength - MinLength + 1);
  Result := RandomPseudoWordB(Length);
  if CaseSensitive then
    begin
      N := RandomUniform(1 + Length div 2);
      for I := 0 to N - 1 do
        begin
          J := RandomUniform(Length);
          C := Result[J + 1];
          if C in ['A'..'Z'] then
            Result[J + 1] := AnsiChar(Ord(C) + 32);
        end;
    end;
  if UseSymbols then
    begin
      N := RandomUniform(1 + Length div 4);
      for I := 0 to N - 1 do
        begin
          J := RandomUniform(Length);
          K := RandomUniform(PasswordSymbolCharCount);
          Result[J + 1] := AnsiChar(PasswordSymbolChars[K + 1]);
        end;
    end;
  if UseNumbers then
    begin
      N := RandomUniform(1 + Length div 4);
      for I := 0 to N - 1 do
        begin
          J := RandomUniform(Length);
          K := RandomUniform(PasswordNumberCharCount);
          Result[J + 1] := AnsiChar(PasswordNumberChars[K + 1]);
        end;
    end;
end;



{                                                                              }
{ Normal Random                                                                }
{                                                                              }
var
  HasRandomNormal : Boolean = False;
  ARandomNormal   : Extended;

function RandomNormalF: Extended;
var
  fac, r, v1, v2: Extended;
begin
  if not HasRandomNormal then
    begin
      Repeat
        v1 := 2.0 * RandomFloat - 1.0;
        v2 := 2.0 * RandomFloat - 1.0;
        r := Sqr(v1) + Sqr(v2);
      Until r < 1.0;
      fac := Sqrt(-2.0 * ln(r) / r);
      ARandomNormal := v1 * fac;
      Result := v2 * fac;
      HasRandomNormal := True;
    end
  else
    begin
      Result := ARandomNormal;
      HasRandomNormal := False;
    end;
end;

procedure RandomNormalFinalise;
begin
  if HasRandomNormal then
    ARandomNormal := 0.0;
end;



{                                                                              }
{ Test cases                                                                   }
{                                                                              }
{$IFDEF DEBUG}
{$IFDEF TEST}
{$ASSERTIONS ON}
procedure Test;
var I, L : Integer;
    A, B, C, D : Word32;
    V, W : Int64;
    T1, T2 : Int64;
begin
  Assert(Length(RandomPasswordB(0, 0, True, True, True)) = 0);
  Assert(Length(RandomPasswordB(1, 1, True, True, True)) = 1);
  for I := 1 to 100 do
    begin
      L := Length(RandomPasswordB(5, 16, True, True, True));
      Assert((L >= 5) and (L <= 16));
    end;
  Assert(Length(RandomHexB(32)) = 32);
  // RandomSeed/RandomUniform
  // - Check for unique numbers
  // - Check average value of random numbers
  T1 := 0;
  T2 := 0;
  for I := 1 to 10000 do
    begin
      A := RandomSeed32;
      B := RandomSeed32;
      C := RandomSeed32;
      D := RandomSeed32;
      Assert(not ((A = B) and (B = C) and (C = D)), 'RandomSeed');
      T1 := T1 + A + B + C + D;
      A := RandomUniform32;
      B := RandomUniform32;
      C := RandomUniform32;
      D := RandomUniform32;
      Assert(not ((A = B) and (B = C) and (C = D)), 'RandomUniform');
      T2 := T2 + A + B + C + D;
    end;
  T1 := T1 div 40000;
  Assert((T1 > $50000000) and (T1 < $B0000000), 'RandomSeed');
  T2 := T2 div 40000;
  Assert((T2 > $50000000) and (T2 < $B0000000), 'RandomUniform');
  // RandomInt64
  // - Check sign
  I := 0;
  repeat
    Inc(I);
    V := RandomInt64;
    W := RandomInt64;
  until ((V < 0) and (W > 0)) or (I = 32);
  Assert((V < 0) and (W > 0), 'RandomInt64');
end;
{$ENDIF}
{$ENDIF}



initialization
  InitStartupSeed;
finalization
  RandomSeedFinalise;
  moaFinalise;
end.

