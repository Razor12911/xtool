unit libc;

interface
{$IFDEF MSWINDOWS}
{$ALIGN ON}
{$MINENUMSIZE 4}
{$WEAKPACKAGEUNIT}

uses Winapi.Windows;

const
  msvcrt = 'msvcrt.dll';
  {$EXTERNALSYM msvcrt}

type
{$IFDEF NEXTGEN}
  PAnsiChar = MarshaledAString;
  PPAnsiChar = ^PAnsiChar;
{$ENDIF}

  va_list = Pointer;
  {$EXTERNALSYM va_list}

  qsort_compare_func = function(P1, P2: Pointer): Integer; cdecl;
  {$EXTERNALSYM qsort_compare_func}

  time_t = {$IFDEF Win32} Integer {$ENDIF}
           {$IFDEF Win64} Int64 {$ENDIF};
  {$EXTERNALSYM time_t}
  Ptime_t = ^time_t;
  {$EXTERNALSYM Ptime_t}
  _time64_t = Int64;
  {$EXTERNALSYM _time64_t}
  P_time64_t = ^_time64_t;
  {$EXTERNALSYM P_time64_t}
  tm = packed record
    tm_sec: Integer;            { Seconds.      [0-60] (1 leap second) }
    tm_min: Integer;            { Minutes.      [0-59]  }
    tm_hour: Integer;           { Hours.        [0-23]  }
    tm_mday: Integer;           { Day.          [1-31]  }
    tm_mon: Integer;            { Month.        [0-11]  }
    tm_year: Integer;           { Year          - 1900. }
    tm_wday: Integer;           { Day of week.  [0-6]   }
    tm_yday: Integer;           { Days in year. [0-365] }
    tm_isdst: Integer;          { DST.          [-1/0/1]}
  end;
  {$EXTERNALSYM tm}
  Ptm = ^tm;
  {$EXTERNALSYM Ptm}

{ ----------------------------------------------------- }
{       Memory                                          }
{ ----------------------------------------------------- }

function  malloc(size: size_t): Pointer; cdecl;
{$EXTERNALSYM malloc}

function realloc(P: Pointer; NewSize: size_t): Pointer; cdecl;
{$EXTERNALSYM realloc}

procedure  free(pBlock: Pointer); cdecl;
{$EXTERNALSYM free}

{ ----------------------------------------------------- }
{       CString                                         }
{ ----------------------------------------------------- }

function  memchr(s: Pointer; c: Integer; n: size_t): Pointer; cdecl;
{$EXTERNALSYM memchr}

function  memcmp(buf1: Pointer; buf2: Pointer; n: size_t): Integer; cdecl;
{$EXTERNALSYM memcmp}

function  memcpy(dest, src: Pointer; count: size_t): Pointer; cdecl;
{$EXTERNALSYM memcpy}

function  memmove(dest, src: Pointer; count: size_t): Pointer; cdecl;
{$EXTERNALSYM memmove}

function  memset(dest: Pointer; val: Integer; count: size_t): Pointer; cdecl;
{$EXTERNALSYM memset}

function  strcat(dest: PAnsiChar; src: PAnsiChar): PAnsiChar; cdecl;
{$EXTERNALSYM strcat}

function  strcpy(dest, src: PAnsiChar): PAnsiChar; cdecl;
{$EXTERNALSYM strcpy}

function  strncpy(dest, src: PAnsiChar; n: size_t): PAnsiChar; cdecl;
{$EXTERNALSYM strncpy}

function  strcmp(s1: PAnsiChar; s2: PAnsiChar): Integer; cdecl;
{$EXTERNALSYM strcmp}

function  strncmp(s1: PAnsiChar; s2: PAnsiChar; n: size_t): Integer; cdecl;
{$EXTERNALSYM strncmp}

function  strlen(s: PAnsiChar): size_t; cdecl;
{$EXTERNALSYM strlen}

function  strnlen(s: PAnsiChar; n: size_t): size_t; cdecl;
{$EXTERNALSYM strnlen}

function  strchr(__s: PAnsiChar; __c: Integer): PAnsiChar; cdecl;
{$EXTERNALSYM strchr}

function  strrchr(__s: PAnsiChar; __c: Integer): PAnsiChar; cdecl;
{$EXTERNALSYM strrchr}

function  strerror(__errnum: Integer): PAnsiChar; cdecl;
{$EXTERNALSYM strerror}

function strcspn(const str1, str2: PAnsiChar): size_t; cdecl;
{$EXTERNALSYM strcspn}

function stricmp(const str1, str2: PAnsiChar): Integer; cdecl;
{$EXTERNALSYM stricmp}

function _stricmp(const str1, str2: PAnsiChar): Integer; cdecl;
{$EXTERNALSYM _stricmp}

function _mbscspn(const str, strCharSet: PWideChar): size_t; cdecl;
{$EXTERNALSYM _mbscspn}

function mbstowcs(pwcs: PWideChar; const s: PWideChar;n: size_t): size_t; cdecl;
{$EXTERNALSYM mbstowcs}

function wcslen(str: PWideChar): size_t; cdecl;
{$EXTERNALSYM wcslen}

function wcsnlen(str: PWideChar; n: size_t): size_t; cdecl;
{$EXTERNALSYM wcsnlen}

function wcstombs(s:Pointer; const pwcs:Pointer; n:Integer):Integer; cdecl;
{$EXTERNALSYM wcstombs}

function strstr(const str1, str2: PAnsiChar): PAnsiChar; cdecl;
{$EXTERNALSYM strstr}

function wcscpy(dest, src: PWideChar): PWideChar; cdecl;
{$EXTERNALSYM wcscpy}

{ ----------------------------------------------------- }
{       Locale                                          }
{ ----------------------------------------------------- }

function  tolower(__ch: Integer): Integer; cdecl;
{$EXTERNALSYM tolower}

function  toupper(__ch: Integer): Integer; cdecl;
{$EXTERNALSYM toupper}

function  towlower(__ch: Integer): Integer; cdecl;
{$EXTERNALSYM towlower}

function  towupper(__ch: Integer): Integer; cdecl;
{$EXTERNALSYM towupper}

function  isalnum(__ch: Integer): Integer; cdecl;
{$EXTERNALSYM isalnum}

function  isalpha(__ch: Integer): Integer; cdecl;
{$EXTERNALSYM isalpha}

function  iscntrl(__ch: Integer): Integer; cdecl;
{$EXTERNALSYM iscntrl}

function  isdigit(__ch: Integer): Integer; cdecl;
{$EXTERNALSYM isdigit}

function  isgraph(__ch: Integer): Integer; cdecl;
{$EXTERNALSYM isgraph}

function  islower(__ch: Integer): Integer; cdecl;
{$EXTERNALSYM islower}

function  isprint(__ch: Integer): Integer; cdecl;
{$EXTERNALSYM isprint}

function  ispunct(__ch: Integer): Integer; cdecl;
{$EXTERNALSYM ispunct}

function  isspace(__ch: Integer): Integer; cdecl;
{$EXTERNALSYM isspace}

function  isupper(__ch: Integer): Integer; cdecl;
{$EXTERNALSYM isupper}

function  isxdigit(__ch: Integer): Integer; cdecl;
{$EXTERNALSYM isxdigit}

function _ismbblead(c: Cardinal): Integer; cdecl;
{$EXTERNALSYM _ismbblead}
function __ismbblead(c: Cardinal): Integer; cdecl;
{$EXTERNALSYM __ismbblead}


{ ----------------------------------------------------- }
{       IO                                              }
{ ----------------------------------------------------- }

function _open(const __path: PAnsiChar; __access: Integer; __permission: Integer): Integer; cdecl;
{$EXTERNALSYM _open}

function _wopen(const __path: PChar; __access: Integer; __permission: Integer): Integer; cdecl;
{$EXTERNALSYM _wopen}

function _close(__handle: Integer): Integer; cdecl;
{$EXTERNALSYM _close}

function _lseek(__handle: Integer; __offset: Integer; __fromwhere: Integer): Integer; cdecl;
{$EXTERNALSYM _lseek}

function _read(__handle: Integer; __buf: Pointer; __len: LongWord): Integer; cdecl;
{$EXTERNALSYM _read}

function _write(__handle: Integer; __buf: Pointer; __len: LongWord): Integer; cdecl;
{$EXTERNALSYM _write}

function open(const __path: PAnsiChar; __access: Integer; __permission: Integer): Integer; cdecl;
{$EXTERNALSYM open}
function close(__handle: Integer): Integer; cdecl;
{$EXTERNALSYM close}
function lseek(__handle: Integer; __offset: Integer; __fromwhere: Integer): Integer; cdecl;
{$EXTERNALSYM lseek}
function read(__handle: Integer; __buf: Pointer; __len: LongWord): Integer; cdecl;
{$EXTERNALSYM read}
function write(__handle: Integer; __buf: Pointer; __len: LongWord): Integer; cdecl;
{$EXTERNALSYM write}
function rename(const __oldname, __newname: PAnsiChar): Integer; cdecl;
{$EXTERNALSYM rename}

{ ----------------------------------------------------- }
{       Standard IO                                     }
{ ----------------------------------------------------- }

function  printf(format: PAnsiChar {args}): Integer; cdecl; varargs;
{$EXTERNALSYM printf}

function  fprintf(fHandle: Pointer; format: PAnsiChar {args}): Integer; cdecl; varargs;
{$EXTERNALSYM fprintf}

function  sprintf(buf: Pointer; format: PAnsiChar {args}): Integer; cdecl; varargs;
{$EXTERNALSYM sprintf}

function snprintf(buf: Pointer; nzize: size_t; format: PAnsiChar; param: va_list): Integer; cdecl;
{$EXTERNALSYM snprintf}

function _snprintf(buf: Pointer; nzize: size_t; format: PAnsiChar; param: va_list): Integer; cdecl;
{$EXTERNALSYM _snprintf}

function vsnprintf(buf: Pointer; nzize: size_t; format: PAnsiChar; param: va_list): Integer; cdecl;
{$EXTERNALSYM vsnprintf}

function _vsnprintf(buf: Pointer; nzize: size_t; format: PAnsiChar; param: va_list): Integer; cdecl;
{$EXTERNALSYM _vsnprintf}

{ ----------------------------------------------------- }
{       Conversion                                      }
{ ----------------------------------------------------- }

function _itoa(value: Integer; str: PAnsiChar; radix: Integer): PAnsiChar; cdecl;
{$EXTERNALSYM _itoa}

function itoa(value: Integer; str: PAnsiChar; radix: Integer): PAnsiChar; cdecl;
{$EXTERNALSYM itoa}

function _i64toa(value: Int64; str: PAnsiChar; radix: Integer): PAnsiChar; cdecl;
{$EXTERNALSYM _i64toa}

function _atoi64(const str: PAnsiChar): Int64; cdecl;
{$EXTERNALSYM _atoi64}

function atoi(const str: PAnsiChar): Integer; cdecl;
{$EXTERNALSYM atoi}

function atof(value: PAnsiChar): Double; cdecl;
{$EXTERNALSYM atof}

function atol(const str: PAnsiChar): LongInt; cdecl;
{$EXTERNALSYM atol}

function strtod(value: PAnsiChar; endPtr: PPAnsiChar): Double; cdecl;
{$EXTERNALSYM strtod}

function gcvt(value: double; digits: Integer; buffer: PAnsiChar): PAnsiChar; cdecl;
{$EXTERNALSYM gcvt}
function _gcvt(value: double; digits: Integer; buffer: PAnsiChar): PAnsiChar; cdecl;
{$EXTERNALSYM _gcvt}

const
  _fltused: Integer = $9875;  // from stubs.c in MS crtl
  {$EXTERNALSYM _fltused}
  _streams: array [0..2] of NativeInt = (0, 1, 2);
  {$EXTERNALSYM _streams}
var
  _errno: Integer;
  {$EXTERNALSYM _errno}
  __errno: Integer;
  {$EXTERNALSYM __errno}
  ___errno: Integer;
  {$EXTERNALSYM ___errno}
  __turboFloat: Integer = 0; // Win32
  {$EXTERNALSYM __turboFloat}


procedure _mbctype; // Not a function, pointer to data
{$EXTERNALSYM _mbctype}
{$IFDEF WIN64}
procedure _purecall; cdecl;
function _lseeki64(__handle: Integer; __offset: Int64; __fromwhere: Integer): Int64; cdecl;
{$EXTERNALSYM _lseeki64}
{$ENDIF}
{$IFDEF WIN32}
procedure __pure_error_;
{$EXTERNALSYM __pure_error_}
function GetMem2(Size: NativeInt): Pointer;
{$EXTERNALSYM GetMem2}
function SysFreeMem2(p: Pointer): Integer;
{$EXTERNALSYM SysFreeMem2}
function _malloc(size: size_t): Pointer; cdecl;
{$EXTERNALSYM _malloc}
function _realloc(P: Pointer; NewSize: size_t): Pointer; cdecl;
{$EXTERNALSYM _realloc}
procedure _free(pBlock: Pointer); cdecl;
{$EXTERNALSYM _free}

function __atold(value: PAnsiChar; endPtr: PPAnsiChar): Extended; cdecl;
{$EXTERNALSYM __atold}
procedure _ftol; cdecl; external;
{$EXTERNALSYM _ftol}
procedure __ftol; cdecl; external; {$L ftol.obj}
{$EXTERNALSYM __ftol}
procedure _ftoul; cdecl;
{$EXTERNALSYM _ftoul}
procedure __ftoul; cdecl; external; {$L _ftoul.obj}
{$EXTERNALSYM __ftoul}
{$ENDIF WIN32}

procedure __mbctype; // Not a function, pointer to data
{$EXTERNALSYM __mbctype}
function  _ltolower(__ch: Integer): Integer; cdecl;
{$EXTERNALSYM _ltolower}
function  _ltoupper(__ch: Integer): Integer; cdecl;
{$EXTERNALSYM _ltoupper}
function _ltowlower(c:Integer):Integer; cdecl;
{$EXTERNALSYM _ltowlower}
function _ltowupper(c:Integer):Integer; cdecl;
{$EXTERNALSYM _ltowupper}
procedure __ltolower; cdecl;
{$EXTERNALSYM __ltolower}
procedure __ltoupper; cdecl;
{$EXTERNALSYM __ltoupper}
procedure __ltowlower; cdecl;
{$EXTERNALSYM __ltowlower}
procedure __ltowupper; cdecl;
{$EXTERNALSYM __ltowupper}

{$IFDEF WIN32}
procedure _atof; cdecl;
{$EXTERNALSYM _atof}
procedure _atol; cdecl;
{$EXTERNALSYM _atol}
procedure _strcspn; cdecl;
{$EXTERNALSYM _strcspn}
procedure _strcat; cdecl;
{$EXTERNALSYM _strcat}
procedure _strcmp; cdecl;
{$EXTERNALSYM _strcmp}
procedure _strncmp; cdecl;
{$EXTERNALSYM _strncmp}
procedure _strcpy; cdecl;
{$EXTERNALSYM _strcpy}
procedure _strncpy; cdecl;
{$EXTERNALSYM _strncpy}
procedure _memmove; cdecl;
{$EXTERNALSYM _memmove}
procedure _memset; cdecl;
{$EXTERNALSYM _memset}
procedure _memcpy; cdecl;
{$EXTERNALSYM _memcpy}
procedure _memcmp; cdecl;
{$EXTERNALSYM _memcmp}
procedure _memchr; cdecl;
{$EXTERNALSYM _memchr}
procedure _strlen; cdecl;
{$EXTERNALSYM _strlen}
procedure _islower; cdecl;
{$EXTERNALSYM _islower}
procedure _isdigit; cdecl;
{$EXTERNALSYM _isdigit}
procedure _isupper; cdecl;
{$EXTERNALSYM _isupper}
procedure _isalnum; cdecl;
{$EXTERNALSYM _isalnum}
procedure _isspace; cdecl;
{$EXTERNALSYM _isspace}
procedure _isxdigit; cdecl;
{$EXTERNALSYM _isxdigit}
procedure _isgraph; cdecl;
{$EXTERNALSYM _isgraph}
procedure _isprint; cdecl;
{$EXTERNALSYM _isprint}
procedure _ispunct; cdecl;
{$EXTERNALSYM _ispunct}
procedure _iscntrl; cdecl;
{$EXTERNALSYM _iscntrl}
procedure _isalpha; cdecl;
{$EXTERNALSYM _isalpha}
procedure _strchr; cdecl;
{$EXTERNALSYM _strchr}
procedure _strnlen; cdecl;
{$EXTERNALSYM _strnlen}
procedure _wcslen; cdecl;
{$EXTERNALSYM _wcslen}
procedure _wcsnlen; cdecl;
{$EXTERNALSYM _wcsnlen}
procedure _printf; cdecl;
{$EXTERNALSYM _printf}
procedure _fprintf; cdecl;
{$EXTERNALSYM _fprintf}
procedure _sprintf; cdecl;
{$EXTERNALSYM _sprintf}
procedure __vsnprintf; cdecl;
{$EXTERNALSYM __vsnprintf}
procedure _tolower; cdecl;
{$EXTERNALSYM _tolower}
procedure _toupper; cdecl;
{$EXTERNALSYM _toupper}
procedure __mbscspn; cdecl;
{$EXTERNALSYM __mbscspn}
procedure __i64toa; cdecl;
{$EXTERNALSYM __i64toa}
procedure __atoi64; cdecl;
{$EXTERNALSYM __atoi64}
procedure _strstr; cdecl;
{$EXTERNALSYM _strstr}
procedure _mbstowcs; cdecl;
{$EXTERNALSYM _mbstowcs}
procedure _wcstombs; cdecl;
{$EXTERNALSYM _wcstombs}
procedure _strerror; cdecl;
{$EXTERNALSYM _strerror}
procedure _llmod; cdecl;
{$EXTERNALSYM _llmod}
procedure _lldiv; cdecl;
{$EXTERNALSYM _lldiv}
procedure _lludiv; cdecl;
{$EXTERNALSYM _lludiv}
procedure _llmul; cdecl;
{$EXTERNALSYM _llmul}
procedure _llumod; cdecl;
{$EXTERNALSYM _llumod}
procedure _llshl; cdecl;
{$EXTERNALSYM _llshl}
procedure _llshr; cdecl;
{$EXTERNALSYM _llshr}
procedure _llushr; cdecl;
{$EXTERNALSYM _llushr}
{$ENDIF WIN32}

procedure qsort(baseP: PByte; NElem, Width: size_t; comparF: qsort_compare_func); cdecl;
{$EXTERNALSYM qsort}
function localtime(t: Ptime_t): Ptm; cdecl;
{$EXTERNALSYM localtime}

function _beginthreadex(security_attr: Pointer; stksize: LongWord;
  start: Pointer; arg: Pointer; create_flags: LongWord;
  var thread_id: LongWord): LongWord; cdecl;
{$EXTERNALSYM _beginthreadex}
procedure _endthreadex(thread_retval: LongWord);
{$EXTERNALSYM _endthreadex}

function log(__x: Double): Double; cdecl;
{$EXTERNALSYM log}
{$IFDEF WIN32}
var
  ___favor,
  ___isa_available,
  ___isa_enabled : Cardinal;
  __imp___errno, ___mingw_vsnprintf, __lseeki64, __imp___wopen : UInt64;
{$ENDIF}
{$IFDEF WIN64}
var
  __favor,__isa_available : Cardinal;
  __memset_nt_iters : NativeInt;
  __security_cookie : UInt64;
  __imp__errno,__mingw_vsnprintf,__imp__wopen : UInt64;
{$ENDIF}
{$IFDEF WIN32}
//void *calloc(size_t nitems, size_t size)
function _calloc(nitems,size : NativeInt):Pointer; cdecl;
function ___divdi3(a,b:int64):Int64;cdecl;external;
function ___udivdi3(a,b:UInt64):UInt64; cdecl; external;
procedure ___ctzdi2; cdecl; external;
procedure __aulldiv; cdecl; external;
procedure __allmul; cdecl; external;
procedure __aullshr; cdecl; external;
procedure __alloca; cdecl; external;
{$ENDIF}
{$IFDEF WIN64}
//void *calloc(size_t nitems, size_t size)
function  __imp_malloc(size: size_t): Pointer; cdecl;
function __imp_calloc(nitems,size : NativeInt):Pointer;
procedure __imp_free(pBlock: Pointer); cdecl;
procedure __security_check_cookie(cookie : UInt64);
function calloc(nitems,size : NativeInt):Pointer;
{$ENDIF}
procedure __chkstk; cdecl; external;
procedure ___chkstk_ms; cdecl; external;
{$ENDIF}
implementation
{$IFDEF MSWINDOWS}
uses System.SysUtils, System.Character;

{ ----------------------------------------------------- }
{       Memory                                          }
{ ----------------------------------------------------- }

function  malloc(size: size_t): Pointer; cdecl;
begin
  Result := AllocMem(size);
end;

function realloc(P: Pointer; NewSize: size_t): Pointer; cdecl;
begin
  ReallocMem(P, Newsize);
  Result := P;
end;

procedure free(pBlock: Pointer); cdecl;
begin
  FreeMem(pBlock);
end;

{$IFDEF WIN64}
function  __imp_malloc;
begin
  Result := AllocMem(size);
end;

procedure __imp_free;
begin
  FreeMem(pBlock);
end;

function __imp_calloc(nitems,size : NativeInt):Pointer;
begin
  Result := Allocmem(nitems*size);
end;

procedure __security_check_cookie;
begin
  if cookie=__security_cookie then exit;
  raise Exception.Create('!!! stack overflow check failed in cookie checker!!!');
end;

procedure _purecall; cdecl;
asm
  jmp System.@AbstractError
end;

function _lseeki64; external msvcrt;

{$ENDIF}

{$IFDEF WIN32}
procedure _llmod; cdecl;
asm
  jmp System.@_llmod;
end;

procedure _lldiv; cdecl;
asm
  jmp System.@_lldiv
end;

procedure _lludiv; cdecl;
asm
  jmp System.@_lludiv
end;

procedure _llmul; cdecl;
asm
  jmp System.@_llmul
end;

procedure _llumod; cdecl;
asm
  jmp System.@_llumod
end;

procedure _llshl; cdecl;
asm
  jmp System.@_llshl
end;

procedure _llshr; cdecl;
asm
        AND   CL, $3F
        CMP   CL, 32
        JL    @__llshr@below32
        MOV   EAX, EDX
        CDQ
        SAR   EAX,CL
        RET

@__llshr@below32:
        SHRD  EAX, EDX, CL
        SAR   EDX, CL
        RET
end;

procedure _llushr; cdecl;
asm
  jmp System.@_llushr
end;

function _malloc(size: size_t): Pointer; cdecl;
begin
  try
    Result := AllocMem(size);
  except
    Result := nil;
  end;
end;

function _realloc(P: Pointer; NewSize: size_t): Pointer; cdecl;
begin
  try
    ReallocMem(P, Newsize);
    Result := P;
  except
    Result := nil;
  end;
end;

procedure _free(pBlock: Pointer); cdecl;
begin
  FreeMem(pBlock);
end;

procedure __pure_error_;
asm
  JMP  System.@AbstractError
end;

// C++'s alloc allocates 1 byte if size is 0.
function GetMem2(Size: NativeInt): Pointer;
begin
  if Size = 0 then Inc(Size);
  GetMem(Result, Size);
end;

// C++'s free allow NULL pointer.
function SysFreeMem2(p: Pointer): Integer;
begin
  result := 0;
  if (p <> NIL) then result := FreeMemory(p);
end;

function __atold(value: PAnsiChar; endPtr: PPAnsiChar): Extended; cdecl;
var
  s: string;
begin
  s := string(Value);
  if endPtr <> nil then
    endPtr^ := value;
  if not TryStrToFloat(s, Result) then
    Result := 0
  else if endPtr <> nil then
    endPtr^ := PAnsiChar(PByte(Value) + Length(s));
end;

procedure _ftoul; cdecl;
asm
  JMP  System.@Trunc
end;
{$ENDIF WIN32}

{ ----------------------------------------------------- }
{       CString                                         }
{ ----------------------------------------------------- }

function  memchr; external msvcrt;

function  memcmp; external msvcrt;

function  memcpy; external msvcrt;

function  memmove; external msvcrt;

function  memset; external msvcrt;

function  strcat; external msvcrt;

function  strcpy; external msvcrt;

function  strncpy; external msvcrt;

function  strcmp; external msvcrt;

function  strncmp; external msvcrt;

function  strlen; external msvcrt;

function  strnlen; external msvcrt;

function  strchr; external msvcrt;

function  strrchr; external msvcrt;

function  strerror; external msvcrt;

function strcspn; external msvcrt;

function stricmp; external msvcrt name '_stricmp';

function _stricmp; external msvcrt;

function _mbscspn; external msvcrt;

function mbstowcs; external msvcrt;

function wcslen; external msvcrt;

function wcsnlen; external msvcrt;

function wcstombs; external msvcrt;

function strstr; external msvcrt;

function wcscpy; external msvcrt;

{ ----------------------------------------------------- }
{       Locale                                          }
{ ----------------------------------------------------- }

function  tolower; external msvcrt;

function  toupper; external msvcrt;

function  towlower; external msvcrt;

function  towupper; external msvcrt;

function  isalnum; external msvcrt;

function  isalpha; external msvcrt;

function  iscntrl; external msvcrt;

function  isdigit; external msvcrt;

function  isgraph; external msvcrt;

function  islower; external msvcrt;

function  isprint; external msvcrt;

function  ispunct; external msvcrt;

function  isspace; external msvcrt;

function  isupper; external msvcrt;

function  isxdigit; external msvcrt;

function _ismbblead; external msvcrt;
function __ismbblead; external msvcrt name '_ismbblead';


{ ----------------------------------------------------- }
{       IO                                              }
{ ----------------------------------------------------- }

function _wopen; external msvcrt;

function _open; external msvcrt;

function _close; external msvcrt;

function _lseek; external msvcrt;

function _read; external msvcrt;

function _write; external msvcrt;

function open; external msvcrt name '_open';

function close; external msvcrt name '_close';

function lseek; external msvcrt name '_lseek';

function read; external msvcrt name '_read';

function write; external msvcrt name '_write';

function rename; external msvcrt;

{ ----------------------------------------------------- }
{       Standard IO                                     }
{ ----------------------------------------------------- }

function  printf; external msvcrt;

function  fprintf; external msvcrt;

function  sprintf; external msvcrt;

function snprintf; external msvcrt name '_snprintf';

function _snprintf; external msvcrt;

function vsnprintf; external msvcrt name '_vsnprintf';

function _vsnprintf; external msvcrt;

{ ----------------------------------------------------- }
{       Conversion                                      }
{ ----------------------------------------------------- }

function _itoa; external msvcrt;

function itoa; external msvcrt name '_itoa';

function _i64toa; external msvcrt;

function _atoi64; external msvcrt;

function atoi; external msvcrt;

function atof; external msvcrt;

function atol; external msvcrt;

function strtod; external msvcrt;

function gcvt; external msvcrt name '_gcvt';

function _gcvt; external msvcrt;

procedure _mbctype; external msvcrt; // Not a function, pointer to data

procedure __mbctype; external msvcrt name '_mbctype'; // Not a function, pointer to data

function  _ltolower; external msvcrt name 'tolower';

function  _ltoupper; external msvcrt name 'toupper';

function _ltowlower; external msvcrt name 'towlower';

function _ltowupper; external msvcrt name 'towupper';

procedure __ltolower; external msvcrt name 'tolower';

procedure __ltoupper; external msvcrt name 'toupper';

procedure __ltowlower; external msvcrt name 'towlower';

procedure __ltowupper; external msvcrt name 'towupper';

procedure _atof; external msvcrt name 'atof';

procedure _atol; external msvcrt name 'atol';

procedure _strcspn; external msvcrt name 'strcspn';

procedure _strcat; external msvcrt name 'strcat';

procedure _strcmp; external msvcrt name 'strcmp';

procedure _strncmp; external msvcrt name 'strncmp';

procedure _strcpy; external msvcrt name 'strcpy';

procedure _strncpy; external msvcrt name 'strncpy';

procedure _memmove; external msvcrt name 'memmove';

procedure _memset; external msvcrt name 'memset';

procedure _memcpy; external msvcrt name 'memcpy';

procedure _memcmp; external msvcrt name 'memcmp';

procedure _memchr; external msvcrt name 'memchr';

procedure _strlen; external msvcrt name 'strlen';

procedure _islower; external msvcrt name 'islower';

procedure _isdigit; external msvcrt name 'isdigit';

procedure _isupper; external msvcrt name 'isupper';

procedure _isalnum; external msvcrt name 'isalnum';

procedure _isspace; external msvcrt name 'isspace';

procedure _isxdigit; external msvcrt name 'isxdigit';

procedure _isgraph; external msvcrt name 'isgraph';

procedure _isprint; external msvcrt name 'isprint';

procedure _ispunct; external msvcrt name 'ispunct';

procedure _iscntrl; external msvcrt name 'iscntrl';

procedure _isalpha; external msvcrt name 'isalpha';

procedure _strchr; external msvcrt name 'strchr';

procedure _strrchr; external msvcrt name 'strrchr';

procedure _strnlen; external msvcrt name 'strnlen';

procedure _wcslen; external msvcrt name 'wcslen';

procedure _wcsnlen; external msvcrt name 'wcsnlen';

procedure _printf; external msvcrt name 'printf';

procedure _fprintf; external msvcrt name 'fprintf';

procedure _sprintf; external msvcrt name 'sprintf';

procedure __vsnprintf; external msvcrt name '_vsnprintf';

procedure _tolower; external msvcrt name 'tolower';

procedure _toupper; external msvcrt name 'toupper';

procedure __mbscspn; external msvcrt name '_mbscspn';

procedure __i64toa; external msvcrt name '_i64toa';

procedure __atoi64; external msvcrt name '_atoi64';

procedure _strstr; external msvcrt name 'strstr';

procedure _mbstowcs; external msvcrt name 'mbstowcs';

procedure _wcstombs; external msvcrt name 'wcstombs';

procedure _strerror; external msvcrt name 'strerror';

procedure qsort; external msvcrt;

function localtime; external msvcrt;

function _beginthreadex; external msvcrt;

procedure _endthreadex; external msvcrt;

function log; external msvcrt;
{$IFDEF WIN32}
{$L chkstk.x86.o}
{$L chkstk_ms.x86.o}
{$L divdi3.x86.o}
{$L udivdi3.x86.o}
{$L ctzdi2.x86.o}
{$L ulldiv.x86.o}
{$L ullshr.x86.o}
{$L llmul.x86.o}
{$L _chkstk.x86.o}
function _calloc(nitems,size : NativeInt):Pointer;
begin
  Result := Allocmem(nitems*size);
end;
{$ENDIF}
{$IFDEF WIN64}
{$L chkstk.x64.o}
{$L chkstk_ms.x64.o}
function calloc(nitems,size : NativeInt):Pointer;
begin
  Result := Allocmem(nitems*size);
end;
{$ENDIF}
procedure CallCPUID(ValueEAX, ValueECX: Cardinal; var ReturnedEAX, ReturnedEBX, ReturnedECX, ReturnedEDX);
  asm
    {$IFDEF CPU32BITS}
    // save context
    PUSH    EDI
    PUSH    EBX
    // init parameters
    MOV     EDI, ReturnedEAX
    MOV     EAX, ValueEAX
    MOV     ECX, ValueECX
    CPUID
    // store results
    MOV     Cardinal PTR [EDI], EAX
    MOV     EAX, ReturnedEBX
    MOV     EDI, ReturnedECX
    MOV     Cardinal PTR [EAX], EBX
    MOV     Cardinal PTR [EDI], ECX
    MOV     EAX, ReturnedEDX
    MOV     Cardinal PTR [EAX], EDX
    // restore context
    POP  EBX
    POP  EDI
    {$ELSE}
    // save context
    PUSH    RBX
    // init parameters
    MOV     R8, ReturnedEAX
    MOV     R9, ReturnedEBX
    MOV     R10, ReturnedECX
    MOV     R11, ReturnedEDX
    MOV     EAX, ValueEAX
    MOV     ECX, ValueECX
    // CPUID
    CPUID
    // store results
    MOV     Cardinal PTR [R8], EAX
    MOV     Cardinal PTR [R9], EBX
    MOV     Cardinal PTR [R10], ECX
    MOV     Cardinal PTR [R11], EDX
    // restore context
    POP     RBX
    {$ENDIF}
  end;
var EAX,EBX,ECX,EDX,H :Cardinal;
    CpuName : array[0..47] of ansichar;
type TTT = array[0..7]of byte;
initialization
  var Name : String;
    {$IFDEF CPU32BITS}
    ___isa_available := 0;
    ___favor := 2;
    {$ELSE}
    __isa_available := 0;
    __favor := 2;
    __memset_nt_iters := 63488;
    {$ENDIF}
  CallCPUID(7,0,EAX,EBX,ECX,EDX);
  if EBX and $20 >0 then begin  //Check AVX2
    {$IFDEF CPU32BITS}
    ___isa_available := 5;
    ___isa_enabled := 47;
    {$ELSE}
    __isa_available := 5;
    {$ENDIF}
  end else begin
    CallCPUID(1,0,EAX,EBX,ECX,EDX);
    if ECX and $10000000 >0 then begin  //Check AVX
      {$IFDEF CPU32BITS}
      ___isa_available := 3;
      ___isa_enabled := 15;
      {$ELSE}
      __isa_available := 3;
      {$ENDIF}
    end else begin
      if ECX and $100000>0 then begin  //Check SSE4.2
        {$IFDEF CPU32BITS}
        ___isa_available := 2;
        ___isa_enabled := 7;
        {$ELSE}
        __isa_available := 2;
        {$ENDIF}
      end else if EDX and $4000000>0 then begin //Check SSE2
        {$IFDEF CPU32BITS}
        ___isa_available := 1;
        ___isa_enabled := 3;
        {$ELSE}
        __isa_available := 1;
        {$ENDIF}
      end else begin
        {$IFDEF CPU32BITS}
        ___isa_enabled := 1;
        {$ENDIF}
      end;
      CallCPUID($80000000, 0, H, EBX, ECX, EDX);
      if H >= $80000002 then
        CallCPUID($80000002, 0, CpuName[0], CpuName[4], CpuName[8], CpuName[12]);
      if H >= $80000003 then
        CallCPUID($80000003, 0, CpuName[16], CpuName[20], CpuName[24], CpuName[28]);
      if H >= $80000004 then
        CallCPUID($80000004, 0, CpuName[32], CpuName[36], CpuName[40], CpuName[44]);
      Name := CPUName;
      if Name.ToUpper.IndexOf('ATOM')>=0 then begin
        {$IFDEF CPU32BITS}
        ___favor := 3;     //Set ATOM bit
        {$ELSE}
        __favor := 3;  //Set ATOM bit
        {$ENDIF}
      end;
    end;
  end;
{$IFDEF WIN64}
  randomize;
  PDouble(@__security_cookie)^ := Random;
{$ENDIF}
{$ENDIF}
end.
