{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals 5.00                                        }
{   File name:        flcTimers.pas                                            }
{   File version:     5.20                                                     }
{   Description:      Timer functions                                          }
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
{   1999/11/10  0.01  Initial version.                                         }
{   2005/08/19  4.02  Compilable with FreePascal 2.0.1                         }
{   2005/08/26  4.03  Improvements to timer functions.                         }
{   2005/08/27  4.04  Revised for Fundamentals 4.                              }
{   2007/06/08  4.05  Compilable with FreePascal 2.04 Win32 i386               }
{   2009/10/09  4.06  Compilable with Delphi 2009 Win32/.NET.                  }
{   2010/06/27  4.07  Compilable with FreePascal 2.4.0 OSX x86-64              }
{   2011/05/04  4.08  Split from cDateTime unit.                               }
{   2015/01/16  4.09  Added GetTickAccuracy.                                   }
{   2015/01/17  5.10  Revised for Fundamentals 5.                              }
{   2018/07/17  5.11  Word32 changes.                                          }
{   2019/04/02  5.12  Compilable with Delphi 10.2 Linux64.                     }
{   2019/06/08  5.13  Use CLOCK_MONOTONIC on Delphi Posix.                     }
{   2019/06/08  5.14  Add GetTick64.                                           }
{   2019/10/06  5.15  MicroTick functions.                                     }
{   2020/01/28  5.16  MilliTick and MicroDateTime functions.                   }
{   2020/01/31  5.17  Use DateTime implementations on iOS.                     }
{   2020/03/10  5.18  Use MachAbsoluteTime on OSX.                             }
{   2020/03/22  5.19  Use maximum resolution on OSX and Delphi Posix.          }
{                     Add high resolution tick functions.                      }
{   2020/05/20  5.20  Use MachAbsoluteTime on iOS.                             }
{                     Remove legacy tick timer, use MilliTick timer for same   }
{                     functionality.                                           }
{                     Remote legacy HPTimer, use HighResolutionTick for same   }
{                     functionality.                                           }
{                     Add GetMicroDateTimeNowUT and GetMicroDateTimeNowUTC.    }
{                                                                              }
{ Supported compilers:                                                         }
{                                                                              }
{   Delphi 2010-10.4 Win32/Win64        5.20  2020/06/02                       }
{   Delphi 10.2-10.4 Linux64            5.20  2020/06/02                       }
{   FreePascal 3.0.4 Win64              5.20  2020/06/02                       }
{                                                                              }
{******************************************************************************}

{$INCLUDE ..\flcInclude.inc}

{$IFDEF DEBUG}
{$IFDEF TEST}
  {$DEFINE TIMERS_TEST}
{$ENDIF}
{$ENDIF}

unit flcTimers;

interface

uses
  { System }
  SysUtils,

  { Fundamentals }
  flcStdTypes;



{                                                                              }
{ Error                                                                        }
{                                                                              }
type
  ETimerError = class(Exception);



{                                                                              }
{ High resolution tick                                                         }
{   GetHighResolutionFrequency returns the resolution of the high resolution   }
{   timer in units per second.                                                 }
{                                                                              }
function  GetHighResolutionTick: Word64;
function  GetHighResolutionFrequency: Word64;
function  HighResolutionTickDelta(const T1, T2: Word64): Int64;
function  HighResolutionTickDeltaU(const T1, T2: Word64): Word64;



{                                                                              }
{ MicroTick                                                                    }
{  Timer in microsecond units, based on high resolution timer if available.    }
{                                                                              }
const
  MicroTickFrequency = 1000000;

function  GetMicroTick: Word64;
function  MicroTickDelta(const T1, T2: Word64): Int64;
function  MicroTickDeltaU(const T1, T2: Word64): Word64;



{                                                                              }
{ MilliTick                                                                    }
{  Timer in millisecond units, based on high resolution timer if available.    }
{                                                                              }
const
  MilliTickFrequency = 1000;

function  GetMilliTick: Word64;
function  MilliTickDelta(const T1, T2: Word64): Int64;
function  MilliTickDeltaU(const T1, T2: Word64): Word64;



{                                                                              }
{ MicroDateTime                                                                }
{   Represents a TDateTime in microsecond units.                               }
{                                                                              }
function  DateTimeToMicroDateTime(const DT: TDateTime): Word64;
function  MicroDateTimeToDateTime(const DT: Word64): TDateTime;



{                                                                              }
{ GetMicroDateTimeNow                                                          }
{   Returns current system date/time as a MicroDateTime.                       }
{                                                                              }
function  GetMicroDateTimeNow: Word64;



{                                                                              }
{ GetMicroDateTimeNowUT                                                        }
{   Returns current UT date/time as a MicroDateTime.                           }
{                                                                              }
function  GetNowUT: TDateTime;
function  GetMicroDateTimeNowUT: Word64;



{                                                                              }
{ GetMicroDateTimeNowUTC                                                       }
{   Returns current UT date/time as a MicroDateTime using a cached start       }
{   time to speed up calculation and ensure monotonic values.                  }
{   The UTC version may drift from the uncached UT version.                    }
{   If ReInit is True, the cache start time is resynchronised with UT time     }
{   from the system clock.                                                     }
{                                                                              }
function  GetNowUTC(const ReInit: Boolean = False): TDateTime;
function  GetMicroDateTimeNowUTC(const ReInit: Boolean = False): Word64;



{                                                                              }
{ Tests                                                                        }
{                                                                              }
{$IFDEF TIMERS_TEST}
procedure Test;
{$ENDIF}



implementation

{$IFDEF WindowsPlatform}
uses
  { System }
  Windows,
  DateUtils;
{$ENDIF}

{$IFDEF FREEPASCAL}
{$IFDEF POSIX}
uses
  { System }
  BaseUnix,
  Unix,
  System.DateUtils;
{$ENDIF}
{$ENDIF}

{$IFDEF DELPHI}
{$IFDEF POSIX}
uses
  { System }
  Posix.Time,
  {$IFDEF MACOS}
  Macapi.CoreServices,
  {$ENDIF}
  {$IFDEF IOS}
  Macapi.Mach,
  {$ENDIF}
  System.DateUtils;
{$ENDIF}
{$ENDIF}



// Turn off overflow checking.
// Most functions rely on overflow checking off to correctly handle counters that wrap around.
{$IFOPT Q+}{$DEFINE QOn}{$ELSE}{$UNDEF QOn}{$ENDIF}{$Q-}



{                                                                              }
{ High resolution counters                                                     }
{                                                                              }

{ Windows }

{$IFDEF WindowsPlatform}
{$DEFINE Defined_GetHighResolutionCounter}
const
  SHighResCounterError = 'High resolution counter error';

var
  HighResolutionCounterInit       : Boolean = False;
  HighResolutionFrequency         : Word64 = 0;
  HighResolutionMillisecondFactor : Word64 = 0;
  HighResolutionMicrosecondFactor : Word64 = 0;

function CPUClockFrequency: Word64;
var
  Freq : Windows.TLargeInteger;
begin
  Freq := 0;
  if not QueryPerformanceFrequency(Freq) then
    raise ETimerError.Create(SHighResCounterError);
  if Freq = 0 then
    raise ETimerError.Create(SHighResCounterError);
  Result := Word64(Freq);
end;

procedure InitHighResolutionCounter;
var
  Freq : Word64;
begin
  Freq := CPUClockFrequency;
  HighResolutionFrequency := Freq;
  HighResolutionMillisecondFactor := Freq div 1000;
  HighResolutionMicrosecondFactor := Freq div 1000000;
  if HighResolutionMicrosecondFactor = 0 then
    raise ETimerError.Create(SHighResCounterError);
  HighResolutionCounterInit := True;
end;

function GetHighResolutionCounter: Word64;
var
  Ctr : Windows.TLargeInteger;
begin
  if not HighResolutionCounterInit then
    InitHighResolutionCounter;
  Ctr := 0;
  if not QueryPerformanceCounter(Ctr) then
    raise ETimerError.Create(SHighResCounterError);
  Result := Word64(Ctr);
end;
{$ENDIF}

{ Delphi Posix; excluding IOS and OSX }

{$IFDEF DELPHI}
{$IFDEF POSIX}
{$IFNDEF IOS}
{$IFNDEF MACOS}
{$DEFINE Defined_GetHighResolutionCounter}
const
  HighResolutionFrequency         = Word64(1000000000);
  HighResolutionMillisecondFactor = 1000000;
  HighResolutionMicrosecondFactor = 1000;

function GetHighResolutionCounter: Word64;
var
  TimeVal : timespec;
  Ticks64 : Word64;
begin
  clock_gettime(CLOCK_MONOTONIC, @TimeVal);
  Ticks64 := Word64(Word64(TimeVal.tv_sec) * Word64(1000000000));
  Ticks64 := Word64(Ticks64 + Word64(TimeVal.tv_nsec));
  Result := Ticks64;
end;
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}

{ Delphi OSX/iOS }

// Apple recommends to use the equivalent clock_gettime_nsec_np(CLOCK_UPTIME_RAW) in nanoseconds.

{$IFDEF DELPHI}
{$IFDEF MACOS}
const
{$IFDEF UNDERSCOREIMPORTNAME}
  _PU = '_';
{$ELSE}
  _PU = '';
{$ENDIF}

const
  LibcLib = '/usr/lib/libc.dylib';

function MachAbsoluteTime: UInt64; cdecl external LibcLib name _PU + 'mach_absolute_time';

{$DEFINE Defined_GetHighResolutionCounter}
const
  HighResolutionFrequency         = Word64(1000000000);
  HighResolutionMillisecondFactor = 1000000;
  HighResolutionMicrosecondFactor = 1000;

function GetHighResolutionCounter: Word64;
var
  Ticks64 : Word64;
begin
  Ticks64 := Word64(AbsoluteToNanoseconds(MachAbsoluteTime));
  Result := Ticks64;
end;
{$ENDIF}
{$ENDIF}

{ FreePascal Posix }

{$IFDEF FREEPASCAL}
{$IFDEF POSIX}
{$DEFINE Defined_GetHighResolutionCounter}
const
  HighResolutionFrequency         = 1000000;
  HighResolutionMillisecondFactor = 1000;
  HighResolutionMicrosecondFactor = 1;

function GetHighResolutionCounter: Word64;
var
  TV : TTimeVal;
  TZ : PTimeZone;
  Ticks64 : Word64;
begin
  TZ := nil;
  fpGetTimeOfDay(@TV, TZ);
  Ticks64 := Word64(Word64(TV.tv_sec) * 1000000);
  Ticks64 := Word64(Ticks64 + Word64(TV.tv_usec));
  Result := Ticks64;
end;
{$ENDIF}
{$ENDIF}

{ Default implementation using system time }

{$IFNDEF Defined_GetHighResolutionCounter}
{$DEFINE Defined_GetHighResolutionCounter}
const
  HighResolutionFrequency         = 1000000;
  HighResolutionMillisecondFactor = 1000;
  HighResolutionMicrosecondFactor = 1;

function GetHighResolutionCounter: Word64;
const
  MicroSecPerDay = Word64(24) * 60 * 60 * 1000 * 1000;
var
  N : Double;
  T : Word64;
begin
  N := Now;
  N := N * MicroSecPerDay;
  T := Word64(Trunc(N));
  Result := T;
end;
{$ENDIF}



{                                                                              }
{ High resolution tick                                                         }
{                                                                              }
function GetHighResolutionTick: Word64;
begin
  Result := GetHighResolutionCounter;
end;

function GetHighResolutionFrequency: Word64;
begin
  {$IFDEF WindowsPlatform}
  if not HighResolutionCounterInit then
    InitHighResolutionCounter;
  {$ENDIF}
  Result := HighResolutionFrequency;
end;

// Overflow checking needs to be off here to correctly handle tick values that
// wrap around the maximum value.
function HighResolutionTickDelta(const T1, T2: Word64): Int64;
begin
  Result := Int64(Word64(T2 - T1));
end;

function HighResolutionTickDeltaU(const T1, T2: Word64): Word64;
begin
  Result := Word64(T2 - T1);
end;


{                                                                              }
{ MicroTick                                                                    }
{                                                                              }
function GetMicroTick: Word64;
var
  T : Word64;
begin
  T := GetHighResolutionCounter;
  T := T div HighResolutionMicrosecondFactor;
  Result := T;
end;

// Overflow checking needs to be off here to correctly handle tick values that
// wrap around the maximum value.
function MicroTickDelta(const T1, T2: Word64): Int64;
begin
  {$IFDEF WindowsPlatform}
  if not HighResolutionCounterInit then
    InitHighResolutionCounter;
  {$ENDIF}
  Result := Int64(Word64(
      Word64(T2 * HighResolutionMicrosecondFactor) -
      Word64(T1 * HighResolutionMicrosecondFactor))) div Int64(HighResolutionMicrosecondFactor);
end;

function MicroTickDeltaU(const T1, T2: Word64): Word64;
begin
  {$IFDEF WindowsPlatform}
  if not HighResolutionCounterInit then
    InitHighResolutionCounter;
  {$ENDIF}
  Result := Word64(
      Word64(T2 * HighResolutionMicrosecondFactor) -
      Word64(T1 * HighResolutionMicrosecondFactor)) div HighResolutionMicrosecondFactor;
end;



{                                                                              }
{ MilliTick                                                                    }
{                                                                              }
function GetMilliTick: Word64;
var
  T : Word64;
begin
  T := GetHighResolutionCounter;
  T := T div Word64(HighResolutionMillisecondFactor);
  Result := T;
end;

// Overflow checking needs to be off here to correctly handle tick values that
// wrap around the maximum value.
function MilliTickDelta(const T1, T2: Word64): Int64;
begin
  {$IFDEF WindowsPlatform}
  if not HighResolutionCounterInit then
    InitHighResolutionCounter;
  {$ENDIF}
  Result := Int64(Word64(
      Word64(T2 * HighResolutionMillisecondFactor) -
      Word64(T1 * HighResolutionMillisecondFactor))) div Int64(HighResolutionMillisecondFactor);
end;

function MilliTickDeltaU(const T1, T2: Word64): Word64;
begin
  {$IFDEF WindowsPlatform}
  if not HighResolutionCounterInit then
    InitHighResolutionCounter;
  {$ENDIF}
  Result := Word64(
      Word64(T2 * HighResolutionMillisecondFactor) -
      Word64(T1 * HighResolutionMillisecondFactor)) div HighResolutionMillisecondFactor;
end;



{                                                                              }
{ MicroDateTime                                                                }
{                                                                              }
const
  // Microseconds per day
  // = 24 * 60 * 60 * 1000 * 1000
  // = 86400000000
  // = $141DD76000
  MicroDateTimeFactor = Word64(86400000000);

function DateTimeToMicroDateTime(const DT: TDateTime): Word64;
var
  Fl : Double;
  Da : Word64;
  FT : Double;
  Ti : Word64;
begin
  Fl := DT;
  if (Fl < -1.0e-12) or (Fl >= 106751990.0) then
    raise ETimerError.Create('Invalid date/time');
  Da := Word64(Trunc(Fl));
  Da := Word64(Da * MicroDateTimeFactor);
  FT := Frac(Fl);
  FT := FT * MicroDateTimeFactor;
  Ti := Word64(Trunc(FT));
  Result := Word64(Da + Ti);
end;

function MicroDateTimeToDateTime(const DT: Word64): TDateTime;
var
  Da : Word64;
  Ti : Word64;
  Fl : Double;
begin
  Da := DT div MicroDateTimeFactor;
  Ti := DT;
  Dec(Ti, Da * MicroDateTimeFactor);
  Fl := Ti;
  Fl := Fl / MicroDateTimeFactor;
  Fl := Fl + Da;
  Result := Fl;
end;

function GetMicroDateTimeNow: Word64;
begin
  Result := DateTimeToMicroDateTime(Now);
end;



{                                                                              }
{ GetNowUT                                                                     }
{                                                                              }
{$IFDEF DELPHIXE2_UP}
{$DEFINE SupportUniversalTime}
function GetNowUT: TDateTime;
begin
  Result := System.DateUtils.TTimeZone.Local.ToUniversalTime(Now);
end;
{$ENDIF}

{$IFDEF FREEPASCAL}
{$IFDEF WindowsPlatform}
{$DEFINE SupportUniversalTime}
function GetUTBias: TDateTime;
var
  TZI : TTimeZoneInformation;
  BiasMin : Integer;
begin
  case GetTimeZoneInformation(TZI) of
    TIME_ZONE_ID_STANDARD : BiasMin := TZI.StandardBias;
    TIME_ZONE_ID_DAYLIGHT : BiasMin := TZI.DaylightBias
  else
    BiasMin := 0;
  end;
  Inc(BiasMin, TZI.Bias);
  Result := BiasMin / (24 * 60);
end;

function GetNowUT: TDateTime;
begin
  Result := Now + GetUTBias;
end;
{$ENDIF}
{$ENDIF}

{$IFDEF FREEPASCAL}
{$IFDEF POSIX}
{$DEFINE SupportUniversalTime}
function GetUTBias: TDateTime;
var
  TV : TTimeVal;
  TZ : PTimeZone;
  BiasMin : Integer;
begin
  TZ := nil;
  fpGetTimeOfDay(@TV, TZ);
  if Assigned(TZ) then
    BiasMin := TZ^.tz_minuteswest div 60
  else
    BiasMin := 0;
  Result := BiasMin / (24 * 60);
end;

function GetNowUT: TDateTime;
begin
  Result := Now + GetUTBias;
end;
{$ENDIF}
{$ENDIF}

{$IFNDEF SupportUniversalTime}
function GetNowUT: TDateTime;
begin
  Result := Now;
end;
{$ENDIF}



{                                                                              }
{ GetMicroDateTimeNowUT                                                        }
{                                                                              }
function GetMicroDateTimeNowUT: Word64;
begin
  Result := DateTimeToMicroDateTime(GetNowUT);
end;



{                                                                              }
{ GetMicroDateTimeNowUT(C)ached                                                }
{                                                                              }
var
  NowUTStartInit      : Boolean = False;
  NowUTStartDT        : TDateTime = 0.0;
  NowUTStartMicroTick : Word64 = 0;
  NowUTStartMicroDT   : Word64 = 0;

procedure InitNowUTStart;
var
  DT : TDateTime;
  MT : Word64;
begin
  DT := GetNowUT;
  MT := GetMicroTick;
  NowUTStartDT := DT;
  NowUTStartMicroTick := MT;
  NowUTStartMicroDT := DateTimeToMicroDateTime(DT);
  NowUTStartInit := True;
end;

function GetNowUTC(const ReInit: Boolean): TDateTime;
var
  MT : Word64;
begin
  if ReInit or not NowUTStartInit then
    InitNowUTStart;
  MT := GetMicroTick;
  Result := MicroDateTimeToDateTime(NowUTStartMicroDT + (MT - NowUTStartMicroTick));
end;

function GetMicroDateTimeNowUTC(const ReInit: Boolean): Word64;
var
  MT : Word64;
begin
  if ReInit or not NowUTStartInit then
    InitNowUTStart;
  MT := GetMicroTick;
  Result := NowUTStartMicroDT + (MT - NowUTStartMicroTick);
end;


{$IFDEF QOn}{$Q+}{$ENDIF}



{                                                                              }
{ Tests                                                                        }
{                                                                              }
{$IFDEF TIMERS_TEST}
{$ASSERTIONS ON}
procedure Test_MilliTickDelta;
begin
  Assert(MilliTickDelta(0, 10) = 10);
  Assert(MilliTickDelta(Word64($FFFFFFFFFFFFFFFF), 10) = 11);
  Assert(MilliTickDelta(10, 0) = -10);
  Assert(MilliTickDelta(Word64($FFFFFFFFFFFFFFF6), 0) = 10);
  Assert(MilliTickDeltaU(0, 10) = 10);
  Assert(MilliTickDeltaU(Word64($FFFFFFFFFFFFFFFF), 10) = 11);
end;

procedure Test_MilliTickTimer1;
var
  A, B : Word64;
  I : Integer;
begin
  // test tick timer using sleep
  A := GetMilliTick;
  I := 1;
  repeat
    Sleep(1);
    Inc(I);
    B := GetMilliTick;
  until (I = 2000) or (B <> A);
  Assert(B <> A);
  Assert(I < 100);
  Assert(MilliTickDelta(A, B) > 0);
  Assert(MilliTickDelta(A, B) < 100);
end;

procedure Test_MicroTickTimer1;
var
  A, B : Word64;
  I : Integer;
begin
  // test tick timer using sleep
  A := GetMicroTick;
  I := 1;
  repeat
    Sleep(1);
    Inc(I);
    B := GetMicroTick;
  until (I = 2000) or (B <> A);
  Assert(B <> A);
  Assert(I < 100);
  Assert(MicroTickDelta(A, B) > 0);
  Assert(MicroTickDelta(A, B) < 100000);
end;

procedure Test_MilliTickTimer2;
var
  A, B : Word64;
  P, Q : TDateTime;
  I : Integer;
begin
  // test tick timer using clock
  A := GetMilliTick;
  I := 1;
  P := Now;
  repeat
    Inc(I);
    Q := Now;
    B := GetMilliTick;
  until (I = 100000000) or (B <> A) or
        (Q >= P + 2.0 / (24.0 * 60.0 * 60.0)); // two seconds
  Assert(B <> A);
  Assert(MilliTickDelta(A, B) > 0);
  Assert(MilliTickDelta(A, B) < 100);
end;

procedure Test_MicroTickTimer2;
var
  A, B : Word64;
  P, Q : TDateTime;
  I : Integer;
begin
  // test tick timer using clock
  A := GetMicroTick;
  I := 1;
  P := Now;
  repeat
    Inc(I);
    Q := Now;
    B := GetMicroTick;
  until (I = 100000000) or (B <> A) or
        (Q >= P + 2.0 / (24.0 * 60.0 * 60.0)); // two seconds
  Assert(B <> A);
  Assert(MicroTickDelta(A, B) > 0);
  Assert(MicroTickDelta(A, B) < 100000);
end;

procedure Test_MicroDateTime1;
var
  DT1 : TDateTime;
  DT2 : TDateTime;
  MD1 : Word64;
  MD2 : Word64;
begin
  // Specific TDateTime
  DT1 := 43971.5231084028;
  MD1 := DateTimeToMicroDateTime(DT1);
  Assert(MD1 = 3799139596566001);
  Assert(MD1 < $00FFFFFFFFFFFFFF);
  DT2 := MicroDateTimeToDateTime(MD1);
  Assert(Abs(DT1 - DT2) < 1.0e-11);
  MD2 := DateTimeToMicroDateTime(DT2);
  Assert(MD2 = 3799139596566001);

  // Zero TDatetime
  DT1 := 0.0;
  MD1 := DateTimeToMicroDateTime(DT1);
  Assert(MD1 = 0);
  DT2 := MicroDateTimeToDateTime(MD1);
  Assert(Abs(DT2) < 1.0e-11);
end;

procedure Test_MicroDateTime2;
var
  MD1 : Word64;
  MD2 : Word64;
  D : Int64;
begin
  // NowUT
  MD1 := GetMicroDateTimeNowUT;
  Sleep(5);
  MD2 := GetMicroDateTimeNowUT - MD1;
  Assert(MD2 > 2000);   // 2ms
  Assert(MD2 < 100000); // 100 ms

  // NowUTC
  MD1 := GetMicroDateTimeNowUTC(True);
  Sleep(5);
  MD2 := GetMicroDateTimeNowUTC(False) - MD1;
  Assert(MD2 > 2000);   // 2ms
  Assert(MD2 < 100000); // 100 ms

  // NowUT / NowUTC drift
  Sleep(10);
  MD1 := GetMicroDateTimeNowUT;
  MD2 := GetMicroDateTimeNowUTC(False);
  if MD2 >= MD1 then
    D := MD2 - MD1
  else
    D := MD1 - MD2;
  Assert(D >= 0);
  Assert(D < 100000); // 100ms
end;

procedure Test;
begin
  Test_MilliTickDelta;
  Test_MilliTickTimer1;
  Test_MicroTickTimer1;
  Test_MilliTickTimer2;
  Test_MicroTickTimer2;
  Test_MicroDateTime1;
  Test_MicroDateTime2;
end;
{$ENDIF}



end.

