{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals 5.00                                        }
{   File name:        flcDateTime.pas                                          }
{   File version:     5.24                                                     }
{   Description:      DateTime functions                                       }
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
{   1999/11/10  0.01  Initial version from scratch. Add functions. DayOfYear.  }
{   1999/11/21  0.02  EasterSunday function. Diff functions. ISOInteger.       }
{   2000/03/04  1.03  Moved RFC functions to cInternetStandards.               }
{   2000/03/05  1.04  Added Time Zone functions from cInternetStandards.       }
{   2000/05/03  1.05  Added ISO Week functions, courtesy of Martin Boonstra    }
{   2000/08/16  1.06  Fixed bug in GMTBias reported by Gerhard Steinwedel      }
{   2001/12/22  2.07  Added RFC DateTime functions from cInternetStandards.    }
{   2002/01/10  3.08  Fixed bug with negative values in AddMonths as           }
{                     reported by Michael Valentiner <MichaelVB at gmx.de>     }
{   2004/02/22  3.09  Fixed bug in RFCDateTimeToGMTDateTime.                   }
{   2005/06/17  4.10  English language datetime functions.                     }
{   2005/08/19  4.11  Compilable with FreePascal 2.0.1 Win32 i386.             }
{   2005/08/21  4.12  Compilable with FreePascal 2.0.1 Linux i386.             }
{   2005/08/26  4.13  Improvements to timer functions.                         }
{   2005/08/27  4.14  Revised for Fundamentals 4.                              }
{   2006/01/11  4.15  Fixed bug in Diff functions detected by Juergen.         }
{   2007/06/08  4.16  Compilable with FreePascal 2.04 Win32 i386               }
{   2007/08/08  4.17  Fixes for negative dates.                                }
{   2008/12/30  4.18  Revision.                                                }
{   2009/10/09  4.19  Compilable with Delphi 2009 Win32/.NET.                  }
{   2010/06/27  4.20  Compilable with FreePascal 2.4.0 OSX x86-64              }
{   2011/05/04  4.21  Moved timer functions to cTimers unit.                   }
{   2016/01/09  5.22  Revised for Fundamendals 5.                              }
{   2018/07/17  5.23  Update for string types.                                 }
{   2018/08/13  5.24  String type changes.                                     }
{                                                                              }
{ Supported compilers:                                                         }
{                                                                              }
{   Delphi 2010-10.4 Win32/Win64        5.24  2020/06/02                       }
{   Delphi 10.2-10.4 Linux64            5.24  2020/06/02                       }
{   FreePascal 3.0.4 Win64              5.24  2020/06/02                       }
{                                                                              }
{ References:                                                                  }
{                                                                              }
{   FAQ ABOUT CALENDARS - http://www.tondering.dk/claus/calendar.html          }
{   RFC822, RFC850, RFC1123, RFC1036, RFC1945.                                 }
{                                                                              }
{******************************************************************************}

{$INCLUDE ..\flcInclude.inc}

{$IFDEF FREEPASCAL}
  {$WARNINGS OFF}
  {$HINTS OFF}
{$ENDIF}

{$IFDEF DEBUG}
{$IFDEF TEST}
  {$DEFINE DATETIME_TEST}
{$ENDIF}
{$ENDIF}

unit flcDateTime;

interface

uses
  { System }
  SysUtils,

  { Fundamentals }
  flcStdTypes;



{                                                                              }
{ Exception                                                                    }
{                                                                              }
type
  EDateTime = class(Exception);

  

{                                                                              }
{ Decoding                                                                     }
{                                                                              }
{$IFNDEF DELPHI6_UP}
procedure DecodeDateTime(const DateTime: TDateTime;
          out Year, Month, Day, Hour, Minute, Second, Millisecond: Word);
{$ENDIF}
function  DatePart(const D: TDateTime): Integer;
function  TimePart(const D: TDateTime): Double;
function  Century(const D: TDateTime): Word;
function  Year(const D: TDateTime): Word;
function  Month(const D: TDateTime): Word;
function  Day(const D: TDateTime): Word;
function  Hour(const D: TDateTime): Word;
function  Minute(const D: TDateTime): Word;
function  Second(const D: TDateTime): Word;
function  Millisecond(const D: TDateTime): Word;

const
  OneDay         = 1.0;
  OneHour        = OneDay / 24;
  OneMinute      = OneHour / 60;
  OneSecond      = OneMinute / 60;
  OneMillisecond = OneSecond / 1000;
  OneWeek        = OneDay * 7;

  HoursPerDay      = 24;
  MinutesPerHour   = 60;
  MinutesPerDay    = MinutesPerHour * HoursPerDay;
  SecondsPerMinute = 60;
  SecondsPerHour   = SecondsPerMinute * MinutesPerHour;
  SecondsPerDay    = SecondsPerHour * HoursPerDay;



{                                                                              }
{ Encoding                                                                     }
{                                                                              }
{$IFNDEF DELPHI6_UP}
function  EncodeDateTime(const Year, Month, Day, Hour, Minute, Second, Millisecond: Word): TDateTime;
{$ENDIF}
procedure SetYear(var D: TDateTime; const Year: Word);
procedure SetMonth(var D: TDateTime; const Month: Word);
procedure SetDay(var D: TDateTime; const Day: Word);
procedure SetHour(var D: TDateTime; const Hour: Word);
procedure SetMinute(var D: TDateTime; const Minute: Word);
procedure SetSecond(var D: TDateTime; const Second: Word);
procedure SetMillisecond(var D: TDateTime; const Milliseconds: Word);



{                                                                              }
{ Comparison                                                                   }
{                                                                              }
function  IsEqual(const D1, D2: TDateTime): Boolean; overload;
function  IsEqual(const D1: TDateTime; const Ye, Mo, Da: Word): Boolean; overload;
function  IsEqual(const D1: TDateTime; const Ho, Mi, Se, ms: Word): Boolean; overload;
function  IsAM(const D: TDateTime): Boolean;
function  IsPM(const D: TDateTime): Boolean;
function  IsMidnight(const D: TDateTime): Boolean;
function  IsNoon(const D: TDateTime): Boolean;
function  IsSunday(const D: TDateTime): Boolean;
function  IsMonday(const D: TDateTime): Boolean;
function  IsTuesday(const D: TDateTime): Boolean;
function  IsWedneday(const D: TDateTime): Boolean;
function  IsThursday(const D: TDateTime): Boolean;
function  IsFriday(const D: TDateTime): Boolean;
function  IsSaturday(const D: TDateTime): Boolean;
function  IsWeekend(const D: TDateTime): Boolean;



{                                                                              }
{ Relative date/times                                                          }
{                                                                              }
function  Noon(const D: TDateTime): TDateTime;
function  Midnight(const D: TDateTime): TDateTime;
function  FirstDayOfMonth(const D: TDateTime): TDateTime;
function  LastDayOfMonth(const D: TDateTime): TDateTime;
function  NextWorkday(const D: TDateTime): TDateTime;
function  PreviousWorkday(const D: TDateTime): TDateTime;
function  FirstDayOfYear(const D: TDateTime): TDateTime;
function  LastDayOfYear(const D: TDateTime): TDateTime;
function  EasterSunday(const Year: Word): TDateTime;
function  GoodFriday(const Year: Word): TDateTime;

function  AddMilliseconds(const D: TDateTime; const N: Int64): TDateTime;
function  AddSeconds(const D: TDateTime; const N: Int64): TDateTime;
function  AddMinutes(const D: TDateTime; const N: Integer): TDateTime;
function  AddHours(const D: TDateTime; const N: Integer): TDateTime;
function  AddDays(const D: TDateTime; const N: Integer): TDateTime;
function  AddWeeks(const D: TDateTime; const N: Integer): TDateTime;
function  AddMonths(const D: TDateTime; const N: Integer): TDateTime;
function  AddYears(const D: TDateTime; const N: Integer): TDateTime;



{                                                                              }
{ Counting                                                                     }
{                                                                              }
{   DayOfYear and WeekNumber start at 1.                                       }
{   WeekNumber is not the ISO week number but the week number where week one   }
{     starts at Jan 1.                                                         }
{   For reference: ISO standard 8601:1988 - (European Standard EN 28601).      }
{     "It states that a week is identified by its number in a given year.      }
{      A week begins with a Monday (day 1) and ends with a Sunday (day 7).     }
{      The first week of a year is the one which includes the first Thursday   }
{      (day 4), or equivalently the one which includes January 4.              }
{      In other words, the first week of a new year is the week that has the   }
{      majority of its days in the new year."                                  }
{   ISOFirstWeekOfYear returns the start date (Monday) of the first ISO week   }
{     of a year (may be in the previous year).                                 }
{   ISOWeekNumber returns the ISO Week number and the year to which the week   }
{     number applies.                                                          }
{                                                                              }
function  DayOfYear(const Ye, Mo, Da: Word): Integer; overload;
function  DayOfYear(const D: TDateTime): Integer; overload;
function  DaysInMonth(const Ye, Mo: Word): Integer; overload;
function  DaysInMonth(const D: TDateTime): Integer; overload;
function  DaysInYear(const Ye: Word): Integer;
function  DaysInYearDate(const D: TDateTime): Integer;
function  WeekNumber(const D: TDateTime): Integer;
function  ISOFirstWeekOfYear(const Ye: Word): TDateTime;
procedure ISOWeekNumber(const D: TDateTime; var WeekNumber, WeekYear: Word);



{                                                                              }
{ Difference                                                                   }
{   Returns difference between two dates (D2 - D1).                            }
{                                                                              }
function  DiffMilliseconds(const D1, D2: TDateTime): Int64;
function  DiffSeconds(const D1, D2: TDateTime): Int64;
function  DiffMinutes(const D1, D2: TDateTime): Int64;
function  DiffHours(const D1, D2: TDateTime): Int64;
function  DiffDays(const D1, D2: TDateTime): Integer;
function  DiffWeeks(const D1, D2: TDateTime): Integer;
function  DiffMonths(const D1, D2: TDateTime): Integer;
function  DiffYears(const D1, D2: TDateTime): Integer;



{                                                                              }
{ Time Zone                                                                    }
{   Uses system's regional settings to convert between local and GMT time.     }
{   GMTBias returns the number of minutes difference between GMT and the       }
{   system's time zone.                                                        }
{   NowAsGMTTime returns the current GMT time.                                 }
{                                                                              }
function  GMTBias: Integer;
function  GMTTimeToLocalTime(const D: TDateTime): TDateTime;
function  LocalTimeToGMTTime(const D: TDateTime): TDateTime;
function  NowAsGMTTime: TDateTime;



{                                                                              }
{ Conversions                                                                  }
{                                                                              }
{   ANSI Integer is an integer in the format YYYYDDD (where DDD = day number)  }
{   ISO-8601 DateTime format is YYMMDD 'T' HH ':' MM ':' SS                    }
{   ISO-8601 Integer date is an integer in the format YYYYMMDD.                }
{   TwoDigitYearToYear returns the full year number given a two digit year.    }
{                                                                              }
function  DateTimeToISO8601StringB(const D: TDateTime): RawByteString;
function  ISO8601StringToTime(const D: RawByteString): TDateTime;
function  ISO8601StringAsDateTime(const D: RawByteString): TDateTime;

function  DateTimeToANSI(const D: TDateTime): Integer;
function  ANSIToDateTime(const Julian: Integer): TDateTime;

function  DateTimeToISOInteger(const D: TDateTime): Integer;
function  DateTimeToISOStringB(const D: TDateTime): RawByteString;
function  ISOIntegerToDateTime(const ISOInteger: Integer): TDateTime;

function  TwoDigitRadix2000YearToYear(const Y: Integer): Integer;

function  DateTimeAsElapsedTimeB(const D: TDateTime;
          const IncludeMilliseconds: Boolean = False): RawByteString;

function  UnixTimeToDateTime(const UnixTime: Word32): TDateTime;
function  DateTimeToUnixTime(const D: TDateTime): Word32;



{                                                                              }
{ English Language DateTimes                                                   }
{                                                                              }
{   ShortDay    =  "Mon" | "Tue" | "Wed" | "Thu" |                             }
{                  "Fri" | "Sat" | "Sun"                                       }
{   LongDay     =  "Monday" | "Tuesday" | "Wednesday" | "Thurday" |            }
{                  "Friday" | "Saturday" | "Sunday"                            }
{   ShortMonth  =  "Jan" | "Feb" | "Mar" | "Apr" | "May" | "Jun" |             }
{                  "Jul" | "Aug" | "Sep" | "Oct" | "Nov" | "Dec"               }
{   LongMonth   =  "January" | "February" | "March" | "April" | "May" |        }
{                  "June" | "July" | "August" | "September" | "October" |      }
{                  "November" | "December"                                     }
{                                                                              }
function  EnglishShortDayOfWeekStrA(const DayOfWeek: Integer): RawByteString;
function  EnglishShortDayOfWeekStrU(const DayOfWeek: Integer): UnicodeString;

function  EnglishLongDayOfWeekStrA(const DayOfWeek: Integer): RawByteString;
function  EnglishLongDayOfWeekStrU(const DayOfWeek: Integer): UnicodeString;

function  EnglishShortMonthStrA(const Month: Integer): RawByteString;
function  EnglishShortMonthStrU(const Month: Integer): UnicodeString;

function  EnglishLongMonthStrA(const Month: Integer): RawByteString;
function  EnglishLongMonthStrU(const Month: Integer): UnicodeString;

function  EnglishShortDayOfWeekA(const S: RawByteString): Integer;
function  EnglishShortDayOfWeekU(const S: UnicodeString): Integer;

function  EnglishLongDayOfWeekA(const S: RawByteString): Integer;
function  EnglishLongDayOfWeekU(const S: UnicodeString): Integer;

function  EnglishShortMonthA(const S: RawByteString): Integer;
function  EnglishShortMonthU(const S: UnicodeString): Integer;

function  EnglishLongMonthA(const S: RawByteString): Integer;
function  EnglishLongMonthU(const S: UnicodeString): Integer;



{                                                                              }
{ RFC DateTimes                                                                }
{                                                                              }
{   RFC1123 DateTime is the preferred representation on the Internet for all   }
{   DateTime values.                                                           }
{   Use DateTimeToRFCDateTime to convert local time to RFC1123 DateTime.       }
{   Use RFCDateTimeToDateTime to convert RFC DateTime formats to local time.   }
{   Returns 0.0 if not a recognised RFC DateTime.                              }
{   See RFC822, RFC850, RFC1123, RFC1036, RFC1945.                             }
{                                                                              }
{ From RFC 822 (Standard for the format of ARPA INTERNET Text Messages):       }
{    "time        =  hour zone                      ; ANSI and Military        }
{     hour        =  2DIGIT ":" 2DIGIT [":" 2DIGIT] ; 00:00:00 - 23:59:59      }
{     zone        =  "UT"  / "GMT"                  ; Universal Time           }
{                                                   ; North American : UT      }
{                 /  "EST" / "EDT"                  ;  Eastern:  - 5/ - 4      }
{                 /  "CST" / "CDT"                  ;  Central:  - 6/ - 5      }
{                 /  "MST" / "MDT"                  ;  Mountain: - 7/ - 6      }
{                 /  "PST" / "PDT"                  ;  Pacific:  - 8/ - 7      }
{                 /  1ALPHA                         ; Military: Z = UT;        }
{                                                   ;  A:-1; (J not used)      }
{                                                   ;  M:-12; N:+1; Y:+12      }
{                 / ( ("+" / "-") 4DIGIT )          ; Local differential       }
{                                                   ;  hours+min. (HHMM)       }
{     date-time   =  [ day "," ] date time          ; dd mm yy                 }
{                                                   ;  hh:mm:ss zzz            }
{     day         =  "Mon"  / "Tue" /  "Wed"  / "Thu"                          }
{                 /  "Fri"  / "Sat" /  "Sun"                                   }
{     date        =  1*2DIGIT month 2DIGIT        ; day month year             }
{                                                 ;  e.g. 20 Jun 82            }
{     month       =  "Jan"  /  "Feb" /  "Mar"  /  "Apr"                        }
{                 /  "May"  /  "Jun" /  "Jul"  /  "Aug"                        }
{                 /  "Sep"  /  "Oct" /  "Nov"  /  "Dec"                    "   }
{                                                                              }
{ Note that even though RFC 822 states hour=2DIGIT":"2DIGIT, none of the       }
{   examples given in the appendix include the ":",                            }
{   for example: "26 Aug 76 1429 EDT"                                          }
{                                                                              }
{                                                                              }
{ From RFC 1036 (Standard for Interchange of USENET Messages):                 }
{                                                                              }
{   "Its format must be acceptable both in RFC-822 and to the getdate(3)       }
{    routine that is provided with the Usenet software.   ...                  }
{    One format that is acceptable to both is:                                 }
{                                                                              }
{                      Wdy, DD Mon YY HH:MM:SS TIMEZONE                        }
{                                                                              }
{    Note in particular that ctime(3) format:                                  }
{                                                                              }
{                          Wdy Mon DD HH:MM:SS YYYY                            }
{                                                                              }
{    is not acceptable because it is not a valid RFC-822 date.  However,       }
{    since older software still generates this format, news                    }
{    implementations are encouraged to accept this format and translate        }
{    it into an acceptable format.                                         "   }
{                                                                              }
{   "Here is an example of a message in the old format (before the             }
{    existence of this standard). It is recommended that                       }
{    implementations also accept messages in this format to ease upward        }
{    conversion.                                                               }
{                                                                              }
{               Posted: Fri Nov 19 16:14:55 1982                           "   }
{                                                                              }
{                                                                              }
{ From RFC 1945 (Hypertext Transfer Protocol -- HTTP/1.0)                      }
{                                                                              }
{  "HTTP/1.0 applications have historically allowed three different            }
{   formats for the representation of date/time stamps:                        }
{                                                                              }
{       Sun, 06 Nov 1994 08:49:37 GMT    ; RFC 822, updated by RFC 1123        }
{       Sunday, 06-Nov-94 08:49:37 GMT   ; RFC 850, obsoleted by RFC 1036      }
{       Sun Nov  6 08:49:37 1994         ; ANSI C's asctime() format           }
{                                                                              }
{   The first format is preferred as an Internet standard and represents       }
{   a fixed-length subset of that defined by RFC 1123 [6] (an update to        }
{   RFC 822 [7]). The second format is in common use, but is based on the      }
{   obsolete RFC 850 [10] date format and lacks a four-digit year.             }
{   HTTP/1.0 clients and servers that parse the date value should accept       }
{   all three formats, though they must never generate the third               }
{   (asctime) format.                                                          }
{                                                                              }
{      Note: Recipients of date values are encouraged to be robust in          }
{      accepting date values that may have been generated by non-HTTP          }
{      applications, as is sometimes the case when retrieving or posting       }
{      messages via proxies/gateways to SMTP or NNTP.                       "  }
{                                                                              }
{  "All HTTP/1.0 date/time stamps must be represented in Universal Time        }
{   (UT), also known as Greenwich Mean Time (GMT), without exception.          }
{                                                                              }
{       HTTP-date      = rfc1123-date | rfc850-date | asctime-date             }
{                                                                              }
{       rfc1123-date   = wkday "," SP date1 SP time SP "GMT"                   }
{       rfc850-date    = weekday "," SP date2 SP time SP "GMT"                 }
{       asctime-date   = wkday SP date3 SP time SP 4DIGIT                      }
{                                                                              }
{       date1          = 2DIGIT SP month SP 4DIGIT                             }
{                        ; day month year (e.g., 02 Jun 1982)                  }
{       date2          = 2DIGIT "-" month "-" 2DIGIT                           }
{                        ; day-month-year (e.g., 02-Jun-82)                    }
{       date3          = month SP ( 2DIGIT | ( SP 1DIGIT ))                    }
{                        ; month day (e.g., Jun  2)                            }
{                                                                              }
{       time           = 2DIGIT ":" 2DIGIT ":" 2DIGIT                          }
{                        ; 00:00:00 - 23:59:59                                 }
{                                                                              }
{       wkday          = "Mon" | "Tue" | "Wed"                                 }
{                      | "Thu" | "Fri" | "Sat" | "Sun"                         }
{                                                                              }
{       weekday        = "Monday" | "Tuesday" | "Wednesday"                    }
{                      | "Thursday" | "Friday" | "Saturday" | "Sunday"         }
{                                                                              }
{       month          = "Jan" | "Feb" | "Mar" | "Apr"                         }
{                      | "May" | "Jun" | "Jul" | "Aug"                         }
{                      | "Sep" | "Oct" | "Nov" | "Dec"                      "  }
{                                                                              }
function  RFC850DayOfWeekB(const S: RawByteString): Integer;
function  RFC850DayOfWeekU(const S: UnicodeString): Integer;

function  RFC1123DayOfWeek(const S: String): Integer;
function  RFC1123DayOfWeekB(const S: RawByteString): Integer;
function  RFC1123DayOfWeekU(const S: UnicodeString): Integer;

function  RFCMonth(const S: String): Word;
function  RFCMonthB(const S: RawByteString): Word;
function  RFCMonthU(const S: UnicodeString): Word;

function  GMTTimeToRFC1123Time(const D: TDateTime;
          const IncludeSeconds: Boolean = False): String;
function  GMTTimeToRFC1123TimeB(const D: TDateTime;
          const IncludeSeconds: Boolean = False): RawByteString;
function  GMTTimeToRFC1123TimeU(const D: TDateTime;
          const IncludeSeconds: Boolean = False): UnicodeString;

function  GMTDateTimeToRFC1123DateTime(const D: TDateTime;
          const IncludeDayOfWeek: Boolean = True): String;
function  GMTDateTimeToRFC1123DateTimeB(const D: TDateTime;
          const IncludeDayOfWeek: Boolean = True): RawByteString;
function  GMTDateTimeToRFC1123DateTimeU(const D: TDateTime;
          const IncludeDayOfWeek: Boolean = True): UnicodeString;

function  DateTimeToRFCDateTimeA(const D: TDateTime): RawByteString;
function  DateTimeToRFCDateTimeU(const D: TDateTime): UnicodeString;

function  NowAsRFCDateTimeA: RawByteString;
function  NowAsRFCDateTimeU: UnicodeString;

function  RFCDateTimeToGMTDateTimeA(const S: RawByteString): TDateTime;
function  RFCDateTimeToDateTimeA(const S: RawByteString): TDateTime;

function  RFCTimeZoneToGMTBiasB(const Zone: RawByteString): Integer;



{                                                                              }
{ Constants                                                                    }
{                                                                              }
{   TropicalYear is the time for one orbit of the earth around the sun.        }
{   SynodicMonth is the time between two full moons.                           }
{                                                                              }
const
  TropicalYear = 365.24219 * OneDay;  // 365 days, 5 hr, 48 min, 46 sec
  SynodicMonth = 29.53059 * OneDay;



{                                                                              }
{ Natural language                                                             }
{                                                                              }
function  TimePeriodStr(const D: TDateTime): RawByteString;



{                                                                              }
{ Test                                                                         }
{                                                                              }
{$IFDEF DATETIME_TEST}
procedure Test;
{$ENDIF}



implementation

uses
  { System }
  {$IFDEF MSWIN}
  Windows,
  {$ENDIF}

  {$IFDEF DELPHI6_UP}
  DateUtils,
  {$ENDIF}

  {$IFDEF UNIX}
  {$IFDEF FREEPASCAL}
  BaseUnix,
  Unix,
  {$ENDIF}
  {$ENDIF}

  {$IFDEF POSIX}
  {$IFDEF DELPHI}
  Posix.SysTime,
  {$ENDIF}
  {$ENDIF}

  { Fundamentals }
  flcUtils,
  {$IFDEF DATETIME_TEST}
  flcTimers,
  {$ENDIF}
  flcStrings;



resourcestring
  SInvalidANSIDateFormat       = 'Invalid ANSI date format';
  SInvalidISOIntegerDateFormat = 'Invalid ISO Integer date format';



{                                                                              }
{ Decoding                                                                     }
{                                                                              }
const
  // DateTruncateFraction is the float value adjustment used when truncating.
  // This avoids truncating errors caused by the inexact nature of floating
  // point representation.
  DateTruncateFraction = OneMillisecond / 8.0;

function DatePart(const D: TDateTime): Integer;
begin
  // Adjust away from zero before truncating
  if D < 0 then
    Result := Trunc(D - DateTruncateFraction)
  else
    Result := Trunc(D + DateTruncateFraction);
end;

function TimePart(const D: TDateTime): Double;
begin
  Result := Abs(Frac(D));
end;

function Century(const D: TDateTime): Word;
begin
  Result := Year(D) div 100;
end;

function Year(const D: TDateTime): Word;
var Mo, Da : Word;
begin
  DecodeDate(D, Result, Mo, Da);
end;

function Month(const D: TDateTime): Word;
var Ye, Da : Word;
begin
  DecodeDate(D, Ye, Result, Da);
end;

function Day(const D: TDateTime): Word;
var Ye, Mo : Word;
begin
  DecodeDate(D, Ye, Mo, Result);
end;

function Hour(const D: TDateTime): Word;
var Mi, Se, MS : Word;
begin
  DecodeTime(D, Result, Mi, Se, MS);
end;

function Minute(const D: TDateTime): Word;
var Ho, Se, MS : Word;
begin
  DecodeTime(D, Ho, Result, Se, MS);
end;

function Second(const D: TDateTime): Word;
var Ho, Mi, MS : Word;
begin
  DecodeTime(D, Ho, Mi, Result, MS);
end;

function Millisecond(const D: TDateTime): Word;
var Ho, Mi, Se : Word;
begin
  DecodeTime(D, Ho, Mi, Se, Result);
end;

{$IFNDEF DELPHI6_UP}
procedure DecodeDateTime(const DateTime: TDateTime; out Year, Month, Day, Hour, Minute, Second, Millisecond : Word);
begin
  DecodeDate(DateTime, Year, Month, Day);
  DecodeTime(DateTime, Hour, Minute, Second, Millisecond);
end;

function EncodeDateTime(const Year, Month, Day, Hour, Minute, Second, Millisecond: Word): TDateTime;
var T : TDateTime;
begin
  Result := EncodeDate(Year, Month, Day);
  T := EncodeTime(Hour, Minute, Second, Millisecond);
  if Result >= 0 then
    Result := Result + T
  else
    Result := Result - T;
end;
{$ENDIF}



{                                                                              }
{ Encoding                                                                     }
{                                                                              }
procedure SetYear(var D: TDateTime; const Year: Word);
var Ye, Mo, Da, Ho, Mi, Se, Ms : Word;
begin
  DecodeDateTime(D, Ye, Mo, Da, Ho, Mi, Se, Ms);
  D := EncodeDateTime(Year, Mo, Da, Ho, Mi, Se, Ms);
end;

procedure SetMonth(var D: TDateTime; const Month: Word);
var Ye, Mo, Da, Ho, Mi, Se, Ms : Word;
begin
  DecodeDateTime(D, Ye, Mo, Da, Ho, Mi, Se, Ms);
  D := EncodeDateTime(Ye, Month, Da, Ho, Mi, Se, Ms);
end;

procedure SetDay(var D: TDateTime; const Day: Word);
var Ye, Mo, Da, Ho, Mi, Se, Ms : Word;
begin
  DecodeDateTime(D, Ye, Mo, Da, Ho, Mi, Se, Ms);
  D := EncodeDateTime(Ye, Mo, Day, Ho, Mi, Se, Ms);
end;

procedure SetHour(var D: TDateTime; const Hour: Word);
var Ye, Mo, Da, Ho, Mi, Se, Ms : Word;
begin
  DecodeDateTime(D, Ye, Mo, Da, Ho, Mi, Se, Ms);
  D := EncodeDateTime(Ye, Mo, Da, Hour, Mi, Se, Ms);
end;

procedure SetMinute(var D: TDateTime; const Minute: Word);
var Ye, Mo, Da, Ho, Mi, Se, Ms : Word;
begin
  DecodeDateTime(D, Ye, Mo, Da, Ho, Mi, Se, Ms);
  D := EncodeDateTime(Ye, Mo, Da, Ho, Minute, Se, Ms);
end;

procedure SetSecond(var D: TDateTime; const Second: Word);
var Ye, Mo, Da, Ho, Mi, Se, Ms : Word;
begin
  DecodeDateTime(D, Ye, Mo, Da, Ho, Mi, Se, Ms);
  D := EncodeDateTime(Ye, Mo, Da, Ho, Mi, Second, Ms);
end;

procedure SetMillisecond(var D: TDateTime; const Milliseconds: Word);
var Ye, Mo, Da, Ho, Mi, Se, Ms : Word;
begin
  DecodeDateTime(D, Ye, Mo, Da, Ho, Mi, Se, Ms);
  D := EncodeDateTime(Ye, Mo, Da, Ho, Mi, Se, Milliseconds);
end;



{                                                                              }
{ Comparison                                                                   }
{                                                                              }
function IsEqual(const D1, D2: TDateTime): Boolean;
begin
  Result := Abs(D1 - D2) < OneMillisecond;
end;

function IsEqual(const D1: TDateTime; const Ye, Mo, Da: Word): Boolean;
var Ye1, Mo1, Da1 : Word;
begin
  DecodeDate(D1, Ye1, Mo1, Da1);
  Result := (Da = Da1) and (Mo = Mo1) and (Ye = Ye1);
end;

function IsEqual(const D1: TDateTime; const Ho, Mi, Se, ms: Word): Boolean;
var Ho1, Mi1, Se1, ms1 : Word;
begin
  DecodeTime(D1, Ho1, Mi1, Se1, ms1);
  Result := (ms = ms1) and (Se = Se1) and (Mi = Mi1) and (Ho = Ho1);
end;

function IsAM(const D: TDateTime): Boolean;
begin
  Result := TimePart(D) < 0.5;
end;

function IsPM(const D: TDateTime): Boolean;
begin
  Result := TimePart(D) >= 0.5;
end;

function IsNoon(const D: TDateTime): Boolean;
var T : Double;
begin
  T := TimePart(D);
  Result := (T >= 0.5) and (T < 0.5 + OneMillisecond);
end;

function IsMidnight(const D: TDateTime): Boolean;
begin
  Result := TimePart(D) < OneMillisecond;
end;

function IsSunday(const D: TDateTime): Boolean;
begin
  Result := DayOfWeek(D) = 1;
end;

function IsMonday(const D: TDateTime): Boolean;
begin
  Result := DayOfWeek(D) = 2;
end;

function IsTuesday(const D: TDateTime): Boolean;
begin
  Result := DayOfWeek(D) = 3;
end;

function IsWedneday(const D: TDateTime): Boolean;
begin
  Result := DayOfWeek(D) = 4;
end;

function IsThursday(const D: TDateTime): Boolean;
begin
  Result := DayOfWeek(D) = 5;
end;

function IsFriday(const D: TDateTime): Boolean;
begin
  Result := DayOfWeek(D) = 6;
end;

function IsSaturday(const D: TDateTime): Boolean;
begin
  Result := DayOfWeek(D) = 7;
end;

function IsWeekend(const D: TDateTime): Boolean;
begin
  Result := Byte(DayOfWeek(D)) in [1, 7];
end;



{                                                                              }
{ Relative calculations                                                        }
{                                                                              }
function Noon(const D: TDateTime): TDateTime;
begin
  Result := DatePart(D) + 0.5 * OneDay;
end;

function Midnight(const D: TDateTime): TDateTime;
var Ye, Mo, Da : Word;
begin
  DecodeDate(D, Ye, Mo, Da);
  Result := EncodeDate(Ye, Mo, Da);
end;

function NextWorkday(const D: TDateTime): TDateTime;
begin
  case DayOfWeek(D) of
    1..5 : Result := DatePart(D) + OneDay;      // 1..5 Sun..Thu
    6    : Result := DatePart(D) + 3 * OneDay;  // 6    Fri
  else
    Result := DatePart(D) + 2 * OneDay;         // 7    Sat
  end;
end;

function PreviousWorkday(const D: TDateTime): TDateTime;
begin
  case DayOfWeek(D) of
    1 : Result := DatePart(D) - 2 * OneDay;  // 1    Sun
    2 : Result := DatePart(D) - 3 * OneDay;  // 2    Mon
  else
    Result := DatePart(D) - OneDay;          // 3..7 Tue-Sat
  end;
end;

function LastDayOfMonth(const D: TDateTime): TDateTime;
var Ye, Mo, Da : Word;
begin
  DecodeDate(D, Ye, Mo, Da);
  Result := EncodeDate(Ye, Mo, Word(DaysInMonth(Ye, Mo)));
end;

function FirstDayOfMonth(const D: TDateTime): TDateTime;
var Ye, Mo, Da : Word;
begin
  DecodeDate(D, Ye, Mo, Da);
  Result := EncodeDate(Ye, Mo, 1);
end;

function LastDayOfYear(const D: TDateTime): TDateTime;
var Ye, Mo, Da : Word;
begin
  DecodeDate(D, Ye, Mo, Da);
  Result := EncodeDate(Ye, 12, 31);
end;

function FirstDayOfYear(const D: TDateTime): TDateTime;
var Ye, Mo, Da : Word;
begin
  DecodeDate(D, Ye, Mo, Da);
  Result := EncodeDate(Ye, 1, 1);
end;

{ This algorithm comes from http://www.tondering.dk/claus/calendar.html:       }
{ " This algorithm is based in part on the algorithm of Oudin (1940) as        }
{   quoted in "Explanatory Supplement to the Astronomical Almanac",            }
{   P. Kenneth Seidelmann, editor.                                             }
{   People who want to dig into the workings of this algorithm, may be         }
{   interested to know that                                                    }
{     G is the Golden Number-1                                                 }
{     H is 23-Epact (modulo 30)                                                }
{     I is the number of days from 21 March to the Paschal full moon           }
{     J is the weekday for the Paschal full moon (0=Sunday, 1=Monday,etc.)     }
{     L is the number of days from 21 March to the Sunday on or before         }
{       the Paschal full moon (a number between -6 and 28) "                   }
function EasterSunday(const Year: Word): TDateTime;
var C, I, J, H, G, L : Integer;
    D, M : Word;
begin
  G := Year mod 19;
  C := Year div 100;
  H := (C - C div 4 - (8 * C + 13) div 25 + 19 * G + 15) mod 30;
  I := H - (H div 28) * (1 - (H div 28) * (29 div (H + 1)) * ((21 - G) div 11));
  J := (Year + Year div 4 + I + 2 - C + C div 4) mod 7;
  L := I - J;
  M := 3 + (L + 40) div 44;
  D := L + 28 - 31 * (M div 4);
  Result := EncodeDate(Year, M, D);
end;

function GoodFriday(const Year: Word): TDateTime;
begin
  Result := EasterSunday(Year) - 2 * OneDay;
end;

function AddMilliseconds(const D: TDateTime; const N: Int64): TDateTime;
var R : Integer;
    T : Double;
begin
  R := DatePart(D) + (N div 86400000);
  T := TimePart(D) + (N mod 86400000) / 86400000.0;
  if T >= 1.0 then
  begin
    if R >= 0 then
      Inc(R)
    else
      Dec(R);
    T := Frac(T);
  end;
  if R >= 0 then
    Result := R + T
  else
    Result := R - T;
end;

function AddSeconds(const D: TDateTime; const N: Int64): TDateTime;
var R : Integer;
    T : Double;
begin
  R := DatePart(D) + (N div 86400);
  T := TimePart(D) + (N mod 86400) / 86400.0;
  if T >= 1.0 then
  begin
    if R >= 0 then
      Inc(R)
    else
      Dec(R);
    T := Frac(T);
  end;
  if R >= 0 then
    Result := R + T
  else
    Result := R - T;
end;

function AddMinutes(const D: TDateTime; const N: Integer): TDateTime;
var R : Integer;
    T : Double;
begin
  R := DatePart(D) + (N div 1440);
  T := TimePart(D) + (N mod 1440) / 1440.0;
  if T >= 1.0 then
  begin
    if R >= 0 then
      Inc(R)
    else
      Dec(R);
    T := Frac(T);
  end;
  if R >= 0 then
    Result := R + T
  else
    Result := R - T;
end;

function AddHours(const D: TDateTime; const N: Integer): TDateTime;
var R : Integer;
    T : Double;
begin
  R := DatePart(D) + (N div 24);
  T := TimePart(D) + (N mod 24) / 24.0;
  if T >= 1.0 then
  begin
    if R >= 0 then
      Inc(R)
    else
      Dec(R);
    T := Frac(T);
  end;
  if R >= 0 then
    Result := R + T
  else
    Result := R - T;
end;

function AddDays(const D: TDateTime; const N: Integer): TDateTime;
begin
  Result := D + N;
end;

function AddWeeks(const D: TDateTime; const N: Integer): TDateTime;
begin
  Result := D + N * 7 * OneDay;
end;

function AddMonths(const D: TDateTime; const N: Integer): TDateTime;
var Ye, Mo, Da : Word;
    IMo : Integer;
    T : Double;
begin
  DecodeDate(D, Ye, Mo, Da);
  Inc(Ye, N div 12);
  IMo := Mo;
  Inc(IMo, N mod 12);
  if IMo > 12 then
    begin
      Dec(IMo, 12);
      Inc(Ye);
    end else
    if IMo < 1 then
      begin
        Inc(IMo, 12);
        Dec(Ye);
      end;
  Mo := Word(IMo);
  Da := Word(MinInt(Da, DaysInMonth(Ye, Mo)));
  Result := EncodeDate(Ye, Mo, Da);
  T := TimePart(D);
  if DatePart(Result) >= 0 then
    Result := Result + T
  else
    Result := Result - T;
end;

function AddYears(const D: TDateTime; const N: Integer): TDateTime;
var Ye, Mo, Da : Word;
    T : Double;
begin
  DecodeDate(D, Ye, Mo, Da);
  Inc(Ye, N);
  Da := Word(MinInt(Da, DaysInMonth(Ye, Mo)));
  Result := EncodeDate(Ye, Mo, Da);
  T := TimePart(D);
  if DatePart(Result) >= 0 then
    Result := Result + T
  else
    Result := Result - T;
end;




{                                                                              }
{ Counting                                                                     }
{                                                                              }
const
  DaysInNonLeapMonth : array[1..12] of Integer = (
    31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
  CumDaysInNonLeapMonth : array[1..12] of Integer = (
    0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334);

function DayOfYear(const Ye, Mo, Da: Word): Integer; overload;
begin
  Result := CumDaysInNonLeapMonth[Mo] + Da;
  if (Mo > 2) and IsLeapYear(Ye) then
    Inc(Result);
end;

function DayOfYear(const D: TDateTime): Integer; overload;
var Ye, Mo, Da : Word;
begin
  DecodeDate(D, Ye, Mo, Da);
  Result := DayOfYear(Ye, Mo, Da);
end;

function DaysInMonth(const Ye, Mo: Word): Integer;
begin
  Result := DaysInNonLeapMonth[Mo];
  if (Mo = 2) and IsLeapYear(Ye) then
    Inc(Result);
end;

function DaysInMonth(const D: TDateTime): Integer;
var Ye, Mo, Da : Word;
begin
  DecodeDate(D, Ye, Mo, Da);
  Result := DaysInMonth(Ye, Mo);
end;

function DaysInYear(const Ye: Word): Integer;
begin
  if IsLeapYear(Ye) then
    Result := 366
  else
    Result := 365;
end;

function DaysInYearDate(const D: TDateTime): Integer;
var Ye, Mo, Da : Word;
begin
  DecodeDate(D, Ye, Mo, Da);
  Result := DaysInYear(Ye);
end;

function WeekNumber(const D: TDateTime): Integer;
begin
  Result := (DiffDays(FirstDayOfYear(D), D) div 7) + 1;
end;

{ ISO Week functions courtesy of Martin Boonstra (m.boonstra at imn.nl)        }
function ISOFirstWeekOfYear(const Ye: Word): TDateTime;
const WeekStartOffset: array[1..7] of Integer = (1, 0, -1, -2, -3, 3, 2);
            // Weekday  Start of ISO week 1 is
            //  1 Su          02-01-Year
            //  2 Mo          01-01-Year
            //  3 Tu          31-12-(Year-1)
            //  4 We          30-12-(Year-1)
            //  5 Th          29-12-(Year-1)
            //  6 Fr          04-01-Year
            //  7 Sa          03-01-Year
begin
  // Adjust with an offset from 01-01-Ye
  Result := EncodeDate(Ye, 1, 1);
  Result := AddDays(Result, WeekStartOffset[DayOfWeek(Result)]);
end;

procedure ISOWeekNumber(const D: TDateTime; var WeekNumber, WeekYear : Word);
var Ye : Word;
    ISOFirstWeekOfPrevYear,
    ISOFirstWeekOfCurrYear,
    ISOFirstWeekOfNextYear : TDateTime;
begin
  { 3 cases:                                                       }
  {   1: D < ISOFirstWeekOfCurrYear                                }
  {       D lies in week 52/53 of previous year                    }
  {   2: ISOFirstWeekOfCurrYear <= D < ISOFirstWeekOfNextYear      }
  {       D lies in week N (1..52/53) of this year                 }
  {   3: D >= ISOFirstWeekOfNextYear                               }
  {       D lies in week 1 of next year                            }
  Ye := Year(D);
  ISOFirstWeekOfCurrYear := ISOFirstWeekOfYear(Ye);
  if D >= ISOFirstWeekOfCurrYear then
    begin
      ISOFirstWeekOfNextYear := ISOFirstWeekOfYear(Ye + 1);
      if (D < ISOFirstWeekOfNextYear) then
        begin // case 2
          WeekNumber := DiffDays(ISOFirstWeekOfCurrYear, D) div 7 + 1;
          WeekYear := Ye;
        end else
        begin // case 3
          WeekNumber := 1;
          WeekYear := Ye + 1;
        end;
    end else
    begin // case 1
      ISOFirstWeekOfPrevYear := ISOFirstWeekOfYear(Ye - 1);
      WeekNumber := DiffDays(ISOFirstWeekOfPrevYear, D) div 7 + 1;
      WeekYear := Ye - 1;
    end;
end;



{                                                                              }
{ Difference                                                                   }
{                                                                              }
function DiffDateTime(const D1, D2: TDateTime; const Period: Double): Int64;
var R : Double;
begin
  R := D2 - D1;
  // Adjust away from zero to ensure correct result when truncating
  if R < 0.0 then
    R := R - DateTruncateFraction
  else
    R := R + DateTruncateFraction;
  Result := Trunc(R / Period);
end;

function DiffMilliseconds(const D1, D2: TDateTime): Int64;
begin
  Result := DiffDateTime(D1, D2, OneMillisecond);
end;

function DiffSeconds(const D1, D2: TDateTime): Int64;
begin
  Result := DiffDateTime(D1, D2, OneSecond);
end;

function DiffMinutes(const D1, D2: TDateTime): Int64;
begin
  Result := DiffDateTime(D1, D2, OneMinute);
end;

function DiffHours(const D1, D2: TDateTime): Int64;
begin
  Result := DiffDateTime(D1, D2, OneHour);
end;

function DiffDays(const D1, D2: TDateTime): Integer;
begin
  Result := DatePart(D2 - D1);
end;

function DiffWeeks(const D1, D2: TDateTime): Integer;
begin
  Result := DatePart(D2 - D1) div 7;
end;

function DiffMonths(const D1, D2: TDateTime): Integer;
var Ye1, Mo1, Da1 : Word;
    Ye2, Mo2, Da2 : Word;
    ModMonth1,
    ModMonth2     : TDateTime;
begin
  DecodeDate(D1, Ye1, Mo1, Da1);
  DecodeDate(D2, Ye2, Mo2, Da2);
  Result := (Ye2 - Ye1) * 12 + (Mo2 - Mo1);
  ModMonth1 := Da1 + TimePart(D1);
  ModMonth2 := Da2 + TimePart(D2);
  if (D2 > D1) and (ModMonth2 < ModMonth1) then
    Dec(Result);
  if (D2 < D1) and (ModMonth2 > ModMonth1) then
    Inc(Result);
end;

function DiffYears(const D1, D2: TDateTime): Integer;
var Ye1, Mo1, Da1 : Word;
    Ye2, Mo2, Da2 : Word;
    ModYear1,
    ModYear2      : TDateTime;
begin
  DecodeDate(D1, Ye1, Mo1, Da1);
  DecodeDate(D2, Ye2, Mo2, Da2);
  Result := Ye2 - Ye1;
  ModYear1 := Mo1 * 31 + Da1 + TimePart(Da1);
  ModYear2 := Mo2 * 31 + Da2 + TimePart(Da2);
  if (D2 > D1) and (ModYear2 < ModYear1) then
    Dec(Result);
  if (D2 < D1) and (ModYear2 > ModYear1) then
    Inc(Result);
end;



{                                                                              }
{ Time Zone                                                                    }
{                                                                              }

{ Returns the GMT bias (in minutes) from the operating system's regional       }
{ settings.                                                                    }
{$IFDEF WindowsPlatform}
function GMTBias: Integer;
var TZI : TTimeZoneInformation;
begin
  case GetTimeZoneInformation(TZI) of
    TIME_ZONE_ID_STANDARD : Result := TZI.StandardBias;
    TIME_ZONE_ID_DAYLIGHT : Result := TZI.DaylightBias
  else
    Result := 0;
  end;
  Result := Result + TZI.Bias;
end;
{$ENDIF}

{$IFDEF UNIX}
{$IFDEF FREEPASCAL}
function GMTBias: Integer;
var TV : TTimeVal;
    TZ : PTimeZone;
begin
  TZ := nil;
  fpGetTimeOfDay(@TV, TZ);
  if Assigned(TZ) then
    Result := TZ^.tz_minuteswest div 60
  else
    Result := 0;
end;
{$ENDIF}
{$ENDIF}

{$IFDEF POSIX}
{$IFDEF DELPHI}
function GMTBias: Integer;
begin
  Result := 0;
end;
{$ENDIF}
{$ENDIF}

{ Converts GMT Time to Local Time                                              }
function GMTTimeToLocalTime(const D: TDateTime): TDateTime;
begin
  Result := D - GMTBias / (24.0 * 60.0);
end;

{ Converts Local Time to GMT Time                                              }
function LocalTimeToGMTTime(const D: TDateTime): TDateTime;
begin
  Result := D + GMTBias / (24.0 * 60.0);
end;

function NowAsGMTTime: TDateTime;
begin
  Result := LocalTimeToGMTTime(Now);
end;



{                                                                              }
{ Conversions                                                                  }
{                                                                              }
function DateTimeToISO8601StringB(const D: TDateTime): RawByteString;
begin
  Result :=
      StrPadLeftB(IntToStringB(Year(D)),   '0', 2, False) +
      StrPadLeftB(IntToStringB(Month(D)),  '0', 2, False) +
      StrPadLeftB(IntToStringB(Day(D)),    '0', 2, False) + 'T' +
      StrPadLeftB(IntToStringB(Hour(D)),   '0', 2, False) + ':' +
      StrPadLeftB(IntToStringB(Minute(D)), '0', 2, False) + ':' +
      StrPadLeftB(IntToStringB(Second(D)), '0', 2, False);
end;

function ISO8601StringToTime(const D: RawByteString): TDateTime;
var P : RawByteStringArray;
    L : Integer;
    Ho, Mi, Se, S1 : Word;
begin
  P := StrSplitCharB(D, ':');
  L := Length(P);
  if (L < 2) or (L > 4) then
    raise EDateTime.Create('Invalid time');
  Ho := Word(StringToIntB(P[0]));
  Mi := Word(StringToIntB(P[1]));
  if L >= 3 then
    Se := Word(StringToIntB(P[2]))
  else
    Se := 0;
  if L >= 4 then
    S1 := Word(StringToIntB(P[3]))
  else
    S1 := 0;
  Result := EncodeTime(Ho, Mi, Se, S1);
end;

function ISO8601StringAsDateTime(const D: RawByteString): TDateTime;
var Date, Time : RawByteString;
    Ye, Mo, Da : Word;
begin
  Date := '';
  Time := '';
  StrSplitAtCharSetB(D, ['T', 't'], Date, Time);
  Ye := Word(StringToIntB(CopyLeftB(Date, 4)));
  Mo := Word(StringToIntB(CopyRangeB(Date, 5, 6)));
  Da := Word(StringToIntB(CopyRangeB(Date, 7, 8)));
  Result := EncodeDate(Ye, Mo, Da) + ISO8601StringToTime(Time);
end;

function DateTimeToANSI(const D: TDateTime): Integer;
var Ye, Mo, Da : Word;
begin
  DecodeDate(D, Ye, Mo, Da);
  Result := Ye * 1000 + DayOfYear(Ye, Mo, Da);
end;

function ANSIToDateTime(const Julian: Integer): TDateTime;
const MaxJulian = $FFFF * 1000 + 366;
var DDD     : Integer;
    C, J    : Integer;
    M, Y, I : Word;
begin
  DDD := Julian mod 1000;
  if (DDD = 0) or (DDD > 366) or (Julian > MaxJulian) then
    raise EDateTime.Create(SInvalidANSIDateFormat);

  Y := Julian div 1000;
  M := 0;
  C := 0;
  for I := 1 to 12 do
    begin
      J := DaysInNonLeapMonth[I];
      if (I = 2) and IsLeapYear(Y) then
        Inc(J);
      Inc(C, J);
      if C >= DDD then
        begin
          M := I;
          break;
        end;
    end;
  if M = 0 then // DDD > end of year
    raise EDateTime.Create(SInvalidANSIDateFormat);

  Result := EncodeDate(Y, M, DDD - C + J);
end;

function DateTimeToISOInteger(const D: TDateTime): Integer;
var Ye, Mo, Da : Word;
begin
  DecodeDate(D, Ye, Mo, Da);
  Result := Ye * 10000 + Mo * 100 + Da;
end;

function DateTimeToISOStringB(const D: TDateTime): RawByteString;
var Ye, Mo, Da : Word;
begin
  DecodeDate(D, Ye, Mo, Da);
  Result := IntToStringB(Ye) + '-' +
            StrPadLeftB(IntToStringB(Mo), '0', 2) + '-' +
            StrPadLeftB(IntToStringB(Da), '0', 2);
end;

function ISOIntegerToDateTime(const ISOInteger: Integer): TDateTime;
var Ye, Mo, Da : Word;
begin
  Ye := ISOInteger div 10000;
  Mo := (ISOInteger mod 10000) div 100;
  if (Mo < 1) or (Mo > 12) then
    raise EDateTime.Create(SInvalidISOIntegerDateFormat);
  Da := ISOInteger mod 100;
  if (Da < 1) or (Da > DaysInMonth(Ye, Mo)) then
    raise EDateTime.Create(SInvalidISOIntegerDateFormat);
  Result := EncodeDate(Ye, Mo, Da);
end;

function TwoDigitRadix2000YearToYear(const Y: Integer): Integer;
begin
  if Y < 50 then
    Result := 2000 + Y
  else
    Result := 1900 + Y;
end;

function DateTimeAsElapsedTimeB(const D: TDateTime;
    const IncludeMilliseconds: Boolean): RawByteString;
var I : Integer;
begin
  I := DatePart(D);
  if I > 0 then
    Result := IntToStringB(I) + '.'
  else
    Result := '';
  Result := Result + IntToStringB(Hour(D)) + ':' +
            StrPadLeftB(IntToStringB(Minute(D)), '0', 2) + ':' +
            StrPadLeftB(IntToStringB(Second(D)), '0', 2);
  if IncludeMilliseconds then
    Result := Result + '.' + StrPadLeftB(IntToStringB(Millisecond(D)), '0', 3);
end;

// Unix time is the number of seconds elapsed since 1 Jan 1970
const
  UnixBaseTime = 25569.0; // 1 Jan 1970 as TDateTime

function UnixTimeToDateTime(const UnixTime: Word32): TDateTime;
begin
  Result := UnixBaseTime + UnixTime / SecondsPerDay;
end;

function DateTimeToUnixTime(const D: TDateTime): Word32;
begin
  Result := Trunc((D - UnixBaseTime) * SecondsPerDay);
end;



{                                                                              }
{ English Language DateTimes                                                   }
{                                                                              }
const
  EnglishShortDayNamesA : array[1..7] of RawByteString = (
      'Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat');
  EnglishShortDayNamesU : array[1..7] of UnicodeString = (
      'Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat');

  EnglishLongDayNamesA : array[1..7] of RawByteString = (
      'Sunday', 'Monday', 'Tuesday', 'Wednesday',
      'Thursday', 'Friday', 'Saturday');
  EnglishLongDayNamesU : array[1..7] of UnicodeString = (
      'Sunday', 'Monday', 'Tuesday', 'Wednesday',
      'Thursday', 'Friday', 'Saturday');

  EnglishShortMonthNamesA : array[1..12] of RawByteString = (
      'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
      'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');
  EnglishShortMonthNamesU : array[1..12] of UnicodeString = (
      'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
      'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');

  EnglishLongMonthNamesA : array[1..12] of RawByteString = (
      'January', 'February', 'March', 'April', 'May', 'June',
      'July', 'August', 'September', 'October', 'November', 'December');
  EnglishLongMonthNamesU : array[1..12] of UnicodeString = (
      'January', 'February', 'March', 'April', 'May', 'June',
      'July', 'August', 'September', 'October', 'November', 'December');

function EnglishShortDayOfWeekStrA(const DayOfWeek: Integer): RawByteString;
begin
  if DayOfWeek in [1..7] then
    Result := EnglishShortDayNamesA[DayOfWeek]
  else
    Result := '';
end;

function EnglishShortDayOfWeekStrU(const DayOfWeek: Integer): UnicodeString;
begin
  if DayOfWeek in [1..7] then
    Result := EnglishShortDayNamesU[DayOfWeek]
  else
    Result := '';
end;

function EnglishLongDayOfWeekStrA(const DayOfWeek: Integer): RawByteString;
begin
  if DayOfWeek in [1..7] then
    Result := EnglishLongDayNamesA[DayOfWeek]
  else
    Result := '';
end;

function EnglishLongDayOfWeekStrU(const DayOfWeek: Integer): UnicodeString;
begin
  if DayOfWeek in [1..7] then
    Result := EnglishLongDayNamesU[DayOfWeek]
  else
    Result := '';
end;

function EnglishShortMonthStrA(const Month: Integer): RawByteString;
begin
  if Month in [1..12] then
    Result := EnglishShortMonthNamesA[Month]
  else
    Result := '';
end;

function EnglishShortMonthStrU(const Month: Integer): UnicodeString;
begin
  if Month in [1..12] then
    Result := EnglishShortMonthNamesU[Month]
  else
    Result := '';
end;

function EnglishLongMonthStrA(const Month: Integer): RawByteString;
begin
  if Month in [1..12] then
    Result := EnglishLongMonthNamesA[Month]
  else
    Result := '';
end;

function EnglishLongMonthStrU(const Month: Integer): UnicodeString;
begin
  if Month in [1..12] then
    Result := EnglishLongMonthNamesU[Month]
  else
    Result := '';
end;

function EnglishShortDayOfWeekA(const S: RawByteString): Integer;
var I : Integer;
begin
  for I := 1 to 7 do
    if StrEqualNoAsciiCaseB(EnglishShortDayNamesA[I], S) then
      begin
        Result := I;
        exit;
      end;
  Result := -1;
end;

function EnglishShortDayOfWeekU(const S: UnicodeString): Integer;
var I : Integer;
begin
  for I := 1 to 7 do
    if StrEqualNoAsciiCaseU(EnglishShortDayNamesU[I], S) then
      begin
        Result := I;
        exit;
      end;
  Result := -1;
end;

function EnglishLongDayOfWeekA(const S: RawByteString): Integer;
var I : Integer;
begin
  for I := 1 to 7 do
    if StrEqualNoAsciiCaseB(EnglishLongDayNamesA[I], S) then
      begin
        Result := I;
        exit;
      end;
  Result := -1;
end;

function EnglishLongDayOfWeekU(const S: UnicodeString): Integer;
var I : Integer;
begin
  for I := 1 to 7 do
    if StrEqualNoAsciiCaseU(EnglishLongDayNamesU[I], S) then
      begin
        Result := I;
        exit;
      end;
  Result := -1;
end;

function EnglishShortMonthA(const S: RawByteString): Integer;
var I : Integer;
begin
  for I := 1 to 12 do
    if StrEqualNoAsciiCaseB(EnglishShortMonthNamesA[I], S) then
      begin
        Result := I;
        exit;
      end;
  Result := -1;
end;

function EnglishShortMonthU(const S: UnicodeString): Integer;
var I : Integer;
begin
  for I := 1 to 12 do
    if StrEqualNoAsciiCaseU(EnglishShortMonthNamesU[I], S) then
      begin
        Result := I;
        exit;
      end;
  Result := -1;
end;

function EnglishLongMonthA(const S: RawByteString): Integer;
var I : Integer;
begin
  for I := 1 to 12 do
    if StrEqualNoAsciiCaseB(EnglishLongMonthNamesA[I], S) then
      begin
        Result := I;
        exit;
      end;
  Result := -1;
end;

function EnglishLongMonthU(const S: UnicodeString): Integer;
var I : Integer;
begin
  for I := 1 to 12 do
    if StrEqualNoAsciiCaseU(EnglishLongMonthNamesU[I], S) then
      begin
        Result := I;
        exit;
      end;
  Result := -1;
end;



{                                                                              }
{ RFC DateTime                                                                 }
{                                                                              }
const
  RFC_SPACE = csWhiteSpace;

  RFC850DayNamesA : array[1..7] of RawByteString = (
      'Sunday', 'Monday', 'Tuesday', 'Wednesday',
      'Thursday', 'Friday', 'Saturday');
  RFC850DayNamesU : array[1..7] of UnicodeString = (
      'Sunday', 'Monday', 'Tuesday', 'Wednesday',
      'Thursday', 'Friday', 'Saturday');

  RFC1123DayNames : array[1..7] of String = (
      'Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat');
  RFC1123DayNamesA : array[1..7] of RawByteString = (
      'Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat');
  RFC1123DayNamesU : array[1..7] of UnicodeString = (
      'Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat');

  RFCMonthNames : array[1..12] of String = (
      'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
      'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');
  RFCMonthNamesA : array[1..12] of RawByteString = (
      'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
      'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');
  RFCMonthNamesU : array[1..12] of UnicodeString = (
      'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
      'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');

function RFC850DayOfWeekB(const S: RawByteString): Integer;
var I : Integer;
begin
  for I := 1 to 7 do
    if StrEqualNoAsciiCaseB(RFC850DayNamesA[I], S) then
      begin
        Result := I;
        exit;
      end;
  Result := -1;
end;

function RFC850DayOfWeekU(const S: UnicodeString): Integer;
var I : Integer;
begin
  for I := 1 to 7 do
    if StrEqualNoAsciiCaseU(RFC850DayNamesU[I], S) then
      begin
        Result := I;
        exit;
      end;
  Result := -1;
end;

function RFC1123DayOfWeek(const S: String): Integer;
var I : Integer;
begin
  for I := 1 to 7 do
    if StrEqualNoAsciiCase(RFC1123DayNames[I], S) then
      begin
        Result := I;
        exit;
      end;
  Result := -1;
end;

function RFC1123DayOfWeekB(const S: RawByteString): Integer;
var I : Integer;
begin
  for I := 1 to 7 do
    if StrEqualNoAsciiCaseB(RFC1123DayNamesA[I], S) then
      begin
        Result := I;
        exit;
      end;
  Result := -1;
end;

function RFC1123DayOfWeekU(const S: UnicodeString): Integer;
var I : Integer;
begin
  for I := 1 to 7 do
    if StrEqualNoAsciiCaseU(RFC1123DayNamesU[I], S) then
      begin
        Result := I;
        exit;
      end;
  Result := -1;
end;

function RFCMonth(const S: String): Word;
var I : Word;
begin
  for I := 1 to 12 do
    if StrEqualNoAsciiCase(RFCMonthNames[I], S) then
      begin
        Result := I;
        exit;
      end;
  Result := 0;
end;

function RFCMonthB(const S: RawByteString): Word;
var I : Word;
begin
  for I := 1 to 12 do
    if StrEqualNoAsciiCaseB(RFCMonthNamesA[I], S) then
      begin
        Result := I;
        exit;
      end;
  Result := 0;
end;

function RFCMonthU(const S: UnicodeString): Word;
var I : Word;
begin
  for I := 1 to 12 do
    if StrEqualNoAsciiCaseU(RFCMonthNamesU[I], S) then
      begin
        Result := I;
        exit;
      end;
  Result := 0;
end;

function GMTTimeToRFC1123Time(const D: TDateTime; const IncludeSeconds: Boolean): String;
var Ho, Mi, Se, Ms : Word;
begin
  DecodeTime(D, Ho, Mi, Se, Ms);
  Result := StrPadLeft(IntToString(Ho), '0', 2) + ':' +
            StrPadLeft(IntToString(Mi), '0', 2);
  if IncludeSeconds then
    Result := Result + ':' + StrPadLeft(IntToString(Se), '0', 2);
  Result := Result + ' GMT';
end;

function GMTTimeToRFC1123TimeB(const D: TDateTime; const IncludeSeconds: Boolean): RawByteString;
var Ho, Mi, Se, Ms : Word;
begin
  DecodeTime(D, Ho, Mi, Se, Ms);
  Result := StrPadLeftB(IntToStringB(Ho), '0', 2) + ':' +
            StrPadLeftB(IntToStringB(Mi), '0', 2);
  if IncludeSeconds then
    Result := Result + ':' + StrPadLeftB(IntToStringB(Se), '0', 2);
  Result := Result + ' GMT';
end;

function GMTTimeToRFC1123TimeU(const D: TDateTime; const IncludeSeconds: Boolean): UnicodeString;
var Ho, Mi, Se, Ms : Word;
begin
  DecodeTime(D, Ho, Mi, Se, Ms);
  Result := StrPadLeftU(IntToStringU(Ho), '0', 2) + ':' +
            StrPadLeftU(IntToStringU(Mi), '0', 2);
  if IncludeSeconds then
    Result := Result + ':' + StrPadLeftU(IntToStringU(Se), '0', 2);
  Result := Result + ' GMT';
end;

function GMTDateTimeToRFC1123DateTime(const D: TDateTime; const IncludeDayOfWeek: Boolean): String;
var Ye, Mo, Da : Word;
begin
  DecodeDate(D, Ye, Mo, Da);
  if IncludeDayOfWeek then
    Result := RFC1123DayNames[DayOfWeek(D)] + ', '
  else
    Result := '';
  Result := Result +
            StrPadLeft(IntToString(Da), '0', 2) + ' ' +
            RFCMonthNames[Mo] + ' ' +
            IntToString(Ye) + ' ' +
            GMTTimeToRFC1123Time(D, True);
end;

function GMTDateTimeToRFC1123DateTimeB(const D: TDateTime;
         const IncludeDayOfWeek: Boolean): RawByteString;
var Ye, Mo, Da : Word;
begin
  DecodeDate(D, Ye, Mo, Da);
  if IncludeDayOfWeek then
    Result := RFC1123DayNamesA[DayOfWeek(D)] + ', '
  else
    Result := '';
  Result := Result +
            StrPadLeftB(IntToStringB(Da), '0', 2) + ' ' +
            RFCMonthNamesA[Mo] + ' ' +
            IntToStringB(Ye) + ' ' +
            GMTTimeToRFC1123TimeB(D, True);
end;

function GMTDateTimeToRFC1123DateTimeU(const D: TDateTime; const IncludeDayOfWeek: Boolean): UnicodeString;
var Ye, Mo, Da : Word;
begin
  DecodeDate(D, Ye, Mo, Da);
  if IncludeDayOfWeek then
    Result := RFC1123DayNamesU[DayOfWeek(D)] + ', '
  else
    Result := '';
  Result := Result +
            StrPadLeftU(IntToStringU(Da), '0', 2) + ' ' +
            RFCMonthNamesU[Mo] + ' ' +
            IntToStringU(Ye) + ' ' +
            GMTTimeToRFC1123TimeU(D, True);
end;

function DateTimeToRFCDateTimeA(const D: TDateTime): RawByteString;
begin
  Result := GMTDateTimeToRFC1123DateTimeB(LocalTimeToGMTTime(D), True);
end;

function DateTimeToRFCDateTimeU(const D: TDateTime): UnicodeString;
begin
  Result := GMTDateTimeToRFC1123DateTimeU(LocalTimeToGMTTime(D), True);
end;

function NowAsRFCDateTimeA: RawByteString;
begin
  Result := DateTimeToRFCDateTimeA(Now);
end;

function NowAsRFCDateTimeU: UnicodeString;
begin
  Result := DateTimeToRFCDateTimeU(Now);
end;

type
  TRFCNamedZoneBias = record
     Zone : RawByteString;
     Bias : Integer;
   end;

const
  RFCNamedTimeZones = 76;
  RFCNamedZoneBias : array[1..RFCNamedTimeZones] of TRFCNamedZoneBias =
      ((Zone:'GMT';  Bias:0),       (Zone:'UT';   Bias:0),
       (Zone:'EST';  Bias:-5*60),   (Zone:'EDT';  Bias:-4*60),
       (Zone:'CST';  Bias:-6*60),   (Zone:'CDT';  Bias:-5*60),
       (Zone:'MST';  Bias:-7*60),   (Zone:'MDT';  Bias:-6*60),
       (Zone:'PST';  Bias:-8*60),   (Zone:'PDT';  Bias:-7*60),

       (Zone:'Z';    Bias:0),       (Zone:'A';    Bias:-1*60),
       (Zone:'B';    Bias:-2*60),   (Zone:'C';    Bias:-3*60),
       (Zone:'D';    Bias:-4*60),   (Zone:'E';    Bias:-5*60),
       (Zone:'F';    Bias:-6*60),   (Zone:'G';    Bias:-7*60),
       (Zone:'H';    Bias:-8*60),   (Zone:'I';    Bias:-9*60),
       (Zone:'K';    Bias:-10*60),  (Zone:'L';    Bias:-11*60),
       (Zone:'M';    Bias:-12*60),  (Zone:'N';    Bias:1*60),
       (Zone:'O';    Bias:2*60),    (Zone:'P';    Bias:3*60),
       (Zone:'Q';    Bias:4*60),    (Zone:'R';    Bias:3*60),
       (Zone:'S';    Bias:6*60),    (Zone:'T';    Bias:3*60),
       (Zone:'U';    Bias:8*60),    (Zone:'V';    Bias:3*60),
       (Zone:'W';    Bias:10*60),   (Zone:'X';    Bias:3*60),
       (Zone:'Y';    Bias:12*60),

       // Additional time zones (not specified in RFC)
       (Zone:'NZDT'; Bias:13*60),   (Zone:'IDLE'; Bias:12*60),
       (Zone:'NZST'; Bias:12*60),   (Zone:'NZT';  Bias:12*60),
       (Zone:'EADT'; Bias:11*60),   (Zone:'GST';  Bias:10*60),
       (Zone:'JST';  Bias:9*60),    (Zone:'CCT';  Bias:8*60),
       (Zone:'WADT'; Bias:8*60),    (Zone:'WAST'; Bias:7*60),
       (Zone:'ZP6';  Bias:6*60),    (Zone:'ZP5';  Bias:5*60),
       (Zone:'ZP4';  Bias:4*60),    (Zone:'BT';   Bias:3*60),
       (Zone:'EET';  Bias:2*60),    (Zone:'MEST'; Bias:2*60),
       (Zone:'MESZ'; Bias:2*60),    (Zone:'SST';  Bias:2*60),
       (Zone:'FST';  Bias:2*60),    (Zone:'CEST'; Bias:2*60),
       (Zone:'CET';  Bias:1*60),    (Zone:'FWT';  Bias:1*60),
       (Zone:'MET';  Bias:1*60),    (Zone:'MEWT'; Bias:1*60),
       (Zone:'SWT';  Bias:1*60),    (Zone:'UTC';  Bias:0),
       (Zone:'WET';  Bias:0*60),    (Zone:'WAT';  Bias:-1*60),
       (Zone:'BST';  Bias:-1*60),   (Zone:'AT';   Bias:-2*60),
       (Zone:'ADT';  Bias:-3*60),   (Zone:'AST';  Bias:-4*60),
       (Zone:'YDT';  Bias:-8*60),   (Zone:'YST';  Bias:-9*60),
       (Zone:'HDT';  Bias:-9*60),   (Zone:'AHST'; Bias:-10*60),
       (Zone:'CAT';  Bias:-10*60),  (Zone:'HST';  Bias:-10*60),
       (Zone:'EAST'; Bias:-10*60),  (Zone:'NT';   Bias:-11*60),
       (Zone:'IDLW'; Bias:-12*60) );

function RFCNamedTimeZoneToGMTBiasB(const TimeZone: RawByteString; out Bias: Integer): Boolean;
var I : Integer;
begin
  for I := 1 to RFCNamedTimeZones do
    if StrEqualNoAsciiCaseB(RFCNamedZoneBias[I].Zone, TimeZone) then
      begin
        Bias := RFCNamedZoneBias[I].Bias;
        Result := True;
        exit;
      end;
  Bias := 0;
  Result := False;
end;

function RFCTimeZoneToGMTBiasB(const Zone: RawByteString): Integer;
var
  C : ByteChar;
  S : RawByteString;
begin
  if Zone = '' then
    begin
      Result := 0;
      exit;
    end;
  C := Zone[1];
  if (C = '+') or (C = '-') then // +hhmm format
    begin
      S := StrTrimB(Zone, RFC_SPACE);
      Result := MaxInt(-23, MinInt(23, StringToIntDefB(Copy(S, 2, 2), 0))) * 60;
      S := CopyFromB(S, 4);
      if S <> '' then
        Result := Result + MinInt(59, MaxInt(0, StringToIntDefB(S, 0)));
      if Zone[1] = '-' then
        Result := -Result;
    end
  else
    begin // named format
      S := StrTrimB(Zone, RFC_SPACE);
      if not RFCNamedTimeZoneToGMTBiasB(S, Result) then
        Result := 0;
    end;
end;

procedure RFCTimeToGMTTime(const S: RawByteString; out Hours, Minutes, Seconds: Integer);
var
  I : Integer;
  T : RawByteString;
  Bias, HH, MM, SS : Integer;
  U : RawByteStringArray;
begin
  U := nil;
  Hours := 0;
  Minutes := 0;
  Seconds := 0;
  T := StrTrimB(S, RFC_SPACE);
  if T = '' then
    exit;

  // Get Zone bias
  I := PosCharSetRevB(RFC_SPACE, T);
  if I > 0 then
    begin
      Bias := RFCTimeZoneToGMTBiasB(CopyFromB(T, I + 1));
      T := StrTrimB(CopyLeftB(T, I - 1), RFC_SPACE);
    end
  else
    Bias := 0;

  // Get time
  U := StrSplitB(T, ':');
  if (Length(U) = 1) and (Length(U[0]) = 4) then
    begin // old hhmm format
      HH := StringToIntDefB(Copy(U[0], 1, 2), 0);
      MM := StringToIntDefB(Copy(U[0], 3, 2), 0);
      SS := 0;
    end else
  if (Length(U) >= 2) or (Length(U) <= 3) then // hh:mm[:ss] format (RFC1123)
    begin
      HH := StringToIntDefB(StrTrimB(U[0], RFC_SPACE), 0);
      MM := StringToIntDefB(StrTrimB(U[1], RFC_SPACE), 0);
      if Length(U) = 3 then
        SS := StringToIntDefB(StrTrimB(U[2], RFC_SPACE), 0) else
        SS := 0;
    end
  else
    exit;

  Hours := MaxInt(0, MinInt(23, HH));
  Minutes := MaxInt(0, MinInt(59, MM));
  Seconds := MaxInt(0, MinInt(59, SS));
  Inc(Hours, Bias div 60);
  Inc(Minutes, Bias mod 60);
end;

function EncodeBiasedDateTime(const Year, Month, Day, Hour, Minute, Second: Integer): TDateTime;
var Ho, Mi : Integer;
begin
  Result := EncodeDate(Word(Year), Word(Month), Word(Day));
  Ho := Hour;
  Mi := Minute;
  if Mi < 0 then
    begin
      Inc(Mi, 60);
      Dec(Ho);
    end;
  if Ho < 0 then
    begin
      Inc(Ho, 24);
      Result := AddDays(Result, -1);
    end;
  if Ho >= 24 then
    begin
      Dec(Ho, 24);
      Result := AddDays(Result, 1);
    end;
  Result := Result + EncodeTime(Word(Ho), Word(Mi), Word(Second), 0);
end;

{$IFDEF DELPHI5}{$OPTIMIZATION OFF}{$ENDIF}
function RFCDateTimeToGMTDateTimeA(const S: RawByteString): TDateTime;
var
  T, U : RawByteString;
  I : Integer;
  D, M, Y, DOW, Ho, Mi, Se : Integer;
  V, W : RawByteStringArray;
begin
  Result := 0.0;

  W := nil;
  T := StrTrimB(S, RFC_SPACE);

  // Extract Day of week
  I := PosCharSetB(RFC_SPACE + [','], T);
  if I > 0 then
    begin
      U := CopyLeftB(T, I - 1);
      DOW := RFC850DayOfWeekB(U);
      if DOW = -1 then
        DOW := RFC1123DayOfWeekB(U);
      if DOW <> -1 then
        T := StrTrimB(CopyFromB(S, I + 1), RFC_SPACE);
    end;

  V := StrSplitCharSetB(T, RFC_SPACE);
  if Length(V) < 3 then
    exit;

  if PosCharB('-', V[0]) > 0 then // RFC850 date, eg "Sunday, 06-Nov-94 08:49:37 GMT"
    begin
      W := StrSplitCharB(V[0], AnsiChar('-'));
      if Length(W) <> 3 then
        exit;
      M := RFCMonthB(W[1]);
      if M = 0 then
        exit;
      D := StringToIntDefB(W[0], 0);
      Y := StringToIntDefB(W[2], 0);
      if Y < 100 then
        Y := TwoDigitRadix2000YearToYear(Y);
      RFCTimeToGMTTime(V[1] + ' ' + V[2], Ho, Mi, Se);
      Result := EncodeBiasedDateTime(Y, M, D, Ho, Mi, Se);
      exit;
    end;

  M := RFCMonthB(V[1]);
  if M >= 1 then // RFC822 date, eg Sun, 06 Nov 1994 08:49:37 GMT
    begin
      D := StringToIntDefB(V[0], 0);
      Y := StringToIntDefB(V[2], 0);
      Ho := 0;
      Mi := 0;
      Se := 0;
      if Length(V) = 4 then
        RFCTimeToGMTTime(V[3], Ho, Mi, Se) else
      if Length(V) >= 5 then
        RFCTimeToGMTTime(V[3] + ' ' + V[4], Ho, Mi, Se);
      Result := EncodeBiasedDateTime(Y, M, D, Ho, Mi, Se);
      exit;
    end;

  M := RFCMonthB(V[0]);
  if M >= 1 then // ANSI C asctime() format, eg "Sun Nov  6 08:49:37 1994"
    begin
      D := StringToIntDefB(V[1], 0);
      Y := StringToIntDefB(V[3], 0);
      RFCTimeToGMTTime(V[2], Ho, Mi, Se);
      Result := EncodeBiasedDateTime(Y, M, D, Ho, Mi, Se);
    end;
end;
{$IFDEF DELPHI5}{$OPTIMIZATION ON}{$ENDIF}

function RFCDateTimeToDateTimeA(const S: RawByteString): TDateTime;
begin
  Result := GMTTimeToLocalTime(RFCDateTimeToGMTDateTimeA(S));
end;



{                                                                              }
{ Natural language                                                             }
{                                                                              }
function TimePeriodStr(const D: TDateTime): RawByteString;
var E : TDateTime;
    I : Integer;
begin
  E := Abs(D);
  if E < OneMillisecond then
    Result := '' else
  if E >= OneWeek then
    begin
      I := Trunc(D / OneWeek);
      if I = 1 then
        Result := 'a week'
      else
        Result := IntToStringB(I) + ' weeks';
    end else
  if E >= OneDay then
    begin
      I := Trunc(D / OneDay);
      if I = 1 then
        Result := 'a day'
      else
        Result := IntToStringB(I) + ' days';
    end else
  if E >= OneHour then
    begin
      I := Trunc(D / OneHour);
      if I = 1 then
        Result := 'an hour'
      else
        Result := IntToStringB(I) + ' hours';
    end else
  if E >= OneMinute then
    begin
      I := Trunc(D / OneMinute);
      if I = 1 then
        Result := 'a minute'
      else
        Result := IntToStringB(I) + ' minutes';
    end
  else
    begin
      I := Trunc(D / OneSecond);
      if I = 1 then
        Result := 'a second'
      else
        Result := IntToStringB(I) + ' seconds';
    end;
end;



{                                                                              }
{ Test                                                                         }
{                                                                              }
{$IFDEF DATETIME_TEST}
{$ASSERTIONS ON}
procedure Test;
var Ye, Mo, Da         : Word;
    Ho, Mi, Se, Ms     : Word;
    Ye2, Mo2, Da2      : Word;
    Ho2, Mi2, Se2, Ms2 : Word;
    A, B               : TDateTime;
    S                  : RawByteString;
    I                  : Integer;
begin
  Ho := 7;
  Mi := 10;
  Da := 8;
  Ms := 3;
  for Ye := 1999 to 2001 do
    for Mo := 1 to 12 do
      for Se := 0 to 59 do
        begin
          A := EncodeDateTime(Ye, Mo, Da, Ho, Mi, Se, Ms);
          DecodeDateTime(A, Ye2, Mo2, Da2, Ho2, Mi2, Se2, Ms2);
          Assert(Ye = Ye2, 'DecodeDate');
          Assert(Mo = Mo2, 'DecodeDate');
          Assert(Da = Da2, 'DecodeDate');
          Assert(Ho = Ho2, 'DecodeDate');
          Assert(Mi = Mi2, 'DecodeDate');
          Assert(Se = Se2, 'DecodeDate');
          Assert(Ms = Ms2, 'DecodeDate');
          Assert(Year(A) = Ye, 'Year');
          Assert(Month(A) = Mo, 'Month');
          Assert(Day(A) = Da, 'Day');
          Assert(Hour(A) = Ho, 'Hour');
          Assert(Minute(A) = Mi, 'Minute');
          Assert(Second(A) = Se, 'Second');
          Assert(Millisecond(A) = Ms, 'Millisecond');
        end;
  A := EncodeDateTime(2002, 05, 31, 07, 04, 01, 02);
  Assert(IsEqual(A, 2002, 05, 31), 'IsEqual');
  Assert(IsEqual(A, 07, 04, 01, 02), 'IsEqual');
  Assert(IsFriday(A), 'IsFriday');
  Assert(not IsMonday(A), 'IsMonday');
  A := AddWeeks(A, 2);
  Assert(IsEqual(A, 2002, 06, 14), 'AddWeeks');
  A := AddHours(A, 2);
  Assert(IsEqual(A, 09, 04, 01, 02), 'AddHours');
  A := EncodeDateTime(2004, 03, 01, 0, 0, 0, 0);
  Assert(DayOfYear(A) = 61, 'DayOfYear');
  Assert(DaysInMonth(2004, 02) = 29, 'DaysInMonth');
  Assert(DaysInMonth(2005, 02) = 28, 'DaysInMonth');
  Assert(DaysInMonth(2001, 01) = 31, 'DaysInMonth');
  Assert(DaysInYear(2000) = 366, 'DaysInYear');
  Assert(DaysInYear(2004) = 366, 'DaysInYear');
  Assert(DaysInYear(2006) = 365, 'DaysInYear');
  A := EncodeDateTime(2001, 09, 02, 12, 11, 10, 0);
  Assert(Month(A) = 9, 'EncodeDateTime');
  S := GMTTimeToRFC1123TimeB(A, True);
  Assert(S = '12:11:10 GMT');
  S := GMTDateTimeToRFC1123DateTimeB(A, True);
  Assert(S = 'Sun, 02 Sep 2001 12:11:10 GMT', 'GMTDateTimeToRFC1123DateTime');
  for Ye := 1999 to 2004 do
    for Mo := 1 to 2 do
      for Da := 1 to 2 do
        for Ho := 0 to 23 do
          begin
            A := EncodeDateTime(Ye, Mo, Da, Ho, 11, 10, 0);
            S := GMTDateTimeToRFC1123DateTimeB(A, True);
            B := RFCDateTimeToGMTDateTimeA(S);
            Assert(IsEqual(A, B), 'RFCDateTimeToGMTDateTime');
          end;

  Assert(RFCMonthNamesA[1] = 'Jan', 'RFCMonthNames');
  Assert(RFCMonthNamesA[12] = 'Dec', 'RFCMonthNames');
  Assert(RFC850DayNamesA[1] = 'Sunday', 'RFC850DayNames');
  Assert(RFCMonthB('Jan') = 1, 'RFCMonth');
  Assert(RFCMonthB('Nov') = 11, 'RFCMonth');
  Assert(EnglishLongMonthNamesA[12] = 'December', 'EnglishLongMonthNames');
  Assert(RFCTimeZoneToGMTBiasB('GMT') = 0, 'RFCTimeZoneToGMTBias');
  Assert(RFCTimeZoneToGMTBiasB('est') = -300, 'RFCTimeZoneToGMTBias');

  I := GMTBias;
  Assert((I <= 12 * 60) and (I >= -12 * 60), 'GMTBias');
  A := EncodeDateTime(2002, 05, 31, 07, 04, 01, 02);
  B := UnixTimeToDateTime(DateTimeToUnixTime(A));
  Assert(DiffSeconds(A, B) = 0, 'DateTimeToUnixTime');

  for I := 0 to 59 do
    begin
      Assert(DiffMinutes(EncodeDateTime(2006, 1, 1, 6, I, 0, 0), EncodeDateTime(2006, 1, 1, 7, 30, 0, 0)) = 90 - I, 'DiffMinutes');
      Assert(DiffMinutes(EncodeDateTime(2006, 1, 1, 7, 30, 0, 0), EncodeDateTime(2006, 1, 1, 6, I, 0, 0)) = -(90 - I), 'DiffMinutes');
      Assert(DiffSeconds(EncodeDateTime(2006, 1, 1, 6, I, 0, 0), EncodeDateTime(2006, 1, 1, 7, 30, 0, 0)) = (90 - I) * 60, 'DiffSeconds');
      Assert(DiffMilliseconds(EncodeDateTime(2006, 1, 1, 6, I, 0, 0), EncodeDateTime(2006, 1, 1, 7, 30, 0, I)) = (90 - I) * 60 * 1000 + I, 'DiffMilliseconds');
      Assert(DiffMilliseconds(EncodeDateTime(2006, 1, 1, 7, 30, 0, 0), EncodeDateTime(2006, 1, 1, 6, I, 0, 0)) = -((90 - I) * 60 * 1000), 'DiffMilliseconds');
      Assert(DiffMinutes(EncodeDateTime(2001, 1, 1, 0, 0, 0, 0), EncodeDateTime(2004, 1, 1, 0, I, 0, 0)) = 3 * 365 * 24 * 60 + I, 'DiffMinutes');
      Assert(DiffSeconds(EncodeDateTime(2001, 1, 1, 0, 0, 0, 0), EncodeDateTime(2004, 1, 1, 0, 0, I, 0)) = 3 * 365 * 24 * 60 * 60 + I, 'DiffSeconds');
    end;
  for I := 0 to 999 do
    Assert(DiffMilliseconds(EncodeDateTime(2001, 1, 1, 0, 0, 0, 0), EncodeDateTime(2001, 1, 22, 0, 0, 0, I)) = 21 * 24 * 60 * 60 * 1000 + I, 'DiffMilliseconds');
  Assert(DiffDays(EncodeDateTime(2006, 1, 1, 0, 0, 0, 0), EncodeDateTime(2006, 1, 1, 23, 59, 59, 999)) = 0, 'DiffDays');
  Assert(DiffDays(EncodeDateTime(2006, 1, 1, 0, 0, 0, 0), EncodeDateTime(2006, 1, 2, 0, 00, 0, 0)) = 1, 'DiffDays');
  Assert(DiffDays(EncodeDateTime(2006, 1, 2, 0, 0, 0, 0), EncodeDateTime(2006, 1, 1, 0, 00, 0, 0)) = -1, 'DiffDays');
  Assert(NextWorkDay(EncodeDate(2006, 1, 11)) = EncodeDate(2006, 1, 12), 'NextWorkDay');
  Assert(NextWorkDay(EncodeDate(2006, 1, 12)) = EncodeDate(2006, 1, 13), 'NextWorkDay');
  Assert(NextWorkDay(EncodeDate(2006, 1, 13)) = EncodeDate(2006, 1, 16), 'NextWorkDay');
  Assert(NextWorkDay(EncodeDate(2006, 1, 16)) = EncodeDate(2006, 1, 17), 'NextWorkDay');
end;
{$ENDIF}



end.

