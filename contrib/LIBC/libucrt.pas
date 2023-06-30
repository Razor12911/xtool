unit libucrt;
interface
{$IFDEF WIN64}
procedure vsscanf;external;
{$L x64/lib64_libucrt_extra_a-ucrt_vsscanf.o}
procedure vsprintf;external;
{$L x64/lib64_libucrt_extra_a-ucrt_vsprintf.o}
procedure vsnprintf;external;
{$L x64/lib64_libucrt_extra_a-ucrt_vsnprintf.o}
procedure vscanf;external;
{$L x64/lib64_libucrt_extra_a-ucrt_vscanf.o}
procedure vprintf;external;
{$L x64/lib64_libucrt_extra_a-ucrt_vprintf.o}
procedure vfscanf;external;
{$L x64/lib64_libucrt_extra_a-ucrt_vfscanf.o}
procedure vfprintf;external;
{$L x64/lib64_libucrt_extra_a-ucrt_vfprintf.o}
procedure _vsnwprintf;external;
{$L x64/lib64_libucrt_extra_a-ucrt__vsnwprintf.o}
procedure _vsnprintf;external;
{$L x64/lib64_libucrt_extra_a-ucrt__vsnprintf.o}
procedure _vscprintf;external;
{$L x64/lib64_libucrt_extra_a-ucrt__vscprintf.o}
procedure sscanf;external;
{$L x64/lib64_libucrt_extra_a-ucrt_sscanf.o}
procedure sprintf;external;
{$L x64/lib64_libucrt_extra_a-ucrt_sprintf.o}
procedure snprintf;external;
{$L x64/lib64_libucrt_extra_a-ucrt_snprintf.o}
procedure _snwprintf;external;
{$L x64/lib64_libucrt_extra_a-ucrt__snwprintf.o}
procedure scanf;external;
{$L x64/lib64_libucrt_extra_a-ucrt_scanf.o}
procedure printf;external;
{$L x64/lib64_libucrt_extra_a-ucrt_printf.o}
procedure fwprintf;external;
{$L x64/lib64_libucrt_extra_a-ucrt_fwprintf.o}
procedure fscanf;external;
{$L x64/lib64_libucrt_extra_a-ucrt_fscanf.o}
procedure fprintf;external;
{$L x64/lib64_libucrt_extra_a-ucrt_fprintf.o}
procedure _get_output_format;external;
{$L x64/lib64_libucrt_extra_a-ucrtbase_compat.o}
procedure _abs64;external;
{$L x64/libapi-ms-win-crt-utility-l1-1-0s00000.o}
procedure _byteswap_uint64;external;
{$L x64/libapi-ms-win-crt-utility-l1-1-0s00001.o}
procedure _byteswap_ulong;external;
{$L x64/libapi-ms-win-crt-utility-l1-1-0s00002.o}
procedure _byteswap_ushort;external;
{$L x64/libapi-ms-win-crt-utility-l1-1-0s00003.o}
procedure lfind;external;
{$L x64/libapi-ms-win-crt-utility-l1-1-0s00004.o}
procedure _lfind;external;
{$L x64/libapi-ms-win-crt-utility-l1-1-0s00005.o}
procedure _lfind_s;external;
{$L x64/libapi-ms-win-crt-utility-l1-1-0s00006.o}
procedure _lrotl;external;
{$L x64/libapi-ms-win-crt-utility-l1-1-0s00007.o}
procedure _lrotr;external;
{$L x64/libapi-ms-win-crt-utility-l1-1-0s00008.o}
procedure lsearch;external;
{$L x64/libapi-ms-win-crt-utility-l1-1-0s00009.o}
procedure _lsearch;external;
{$L x64/libapi-ms-win-crt-utility-l1-1-0s00010.o}
procedure _lsearch_s;external;
{$L x64/libapi-ms-win-crt-utility-l1-1-0s00011.o}
procedure _rotl;external;
{$L x64/libapi-ms-win-crt-utility-l1-1-0s00012.o}
procedure _rotl64;external;
{$L x64/libapi-ms-win-crt-utility-l1-1-0s00013.o}
procedure _rotr;external;
{$L x64/libapi-ms-win-crt-utility-l1-1-0s00014.o}
procedure _rotr64;external;
{$L x64/libapi-ms-win-crt-utility-l1-1-0s00015.o}
procedure _swab;external;
{$L x64/libapi-ms-win-crt-utility-l1-1-0s00016.o}
procedure swab;external;
{$L x64/libapi-ms-win-crt-utility-l1-1-0s00017.o}
procedure abs;external;
{$L x64/libapi-ms-win-crt-utility-l1-1-0s00018.o}
procedure bsearch;external;
{$L x64/libapi-ms-win-crt-utility-l1-1-0s00019.o}
procedure bsearch_s;external;
{$L x64/libapi-ms-win-crt-utility-l1-1-0s00020.o}
procedure &div;external;
{$L x64/libapi-ms-win-crt-utility-l1-1-0s00021.o}
procedure imaxabs;external;
{$L x64/libapi-ms-win-crt-utility-l1-1-0s00022.o}
procedure imaxdiv;external;
{$L x64/libapi-ms-win-crt-utility-l1-1-0s00023.o}
procedure labs;external;
{$L x64/libapi-ms-win-crt-utility-l1-1-0s00024.o}
procedure ldiv;external;
{$L x64/libapi-ms-win-crt-utility-l1-1-0s00025.o}
procedure llabs;external;
{$L x64/libapi-ms-win-crt-utility-l1-1-0s00026.o}
procedure lldiv;external;
{$L x64/libapi-ms-win-crt-utility-l1-1-0s00027.o}
procedure qsort;external;
{$L x64/libapi-ms-win-crt-utility-l1-1-0s00028.o}
procedure qsort_s;external;
{$L x64/libapi-ms-win-crt-utility-l1-1-0s00029.o}
procedure rand;external;
{$L x64/libapi-ms-win-crt-utility-l1-1-0s00030.o}
procedure rand_s;external;
{$L x64/libapi-ms-win-crt-utility-l1-1-0s00031.o}
procedure srand;external;
{$L x64/libapi-ms-win-crt-utility-l1-1-0s00032.o}
procedure _Getdays;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00000.o}
procedure _Getmonths;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00001.o}
procedure _Gettnames;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00002.o}
procedure _Strftime;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00003.o}
procedure _W_Getdays;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00004.o}
procedure _W_Getmonths;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00005.o}
procedure _W_Gettnames;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00006.o}
procedure _Wcsftime;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00007.o}
procedure __daylight;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00008.o}
procedure __dstbias;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00009.o}
procedure __timezone;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00010.o}
procedure __tzname;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00011.o}
procedure _ctime32;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00012.o}
procedure _ctime32_s;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00013.o}
procedure ctime;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00014.o}
procedure _ctime64;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00015.o}
procedure _ctime64_s;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00016.o}
procedure _difftime32;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00017.o}
procedure _difftime64;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00018.o}
procedure _ftime32;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00019.o}
procedure _ftime32_s;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00020.o}
procedure _ftime;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00021.o}
procedure _ftime64;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00022.o}
procedure _ftime64_s;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00023.o}
procedure _futime32;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00024.o}
procedure _futime;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00025.o}
procedure _futime64;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00026.o}
procedure _get_daylight;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00027.o}
procedure _get_dstbias;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00028.o}
procedure _get_timezone;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00029.o}
procedure _get_tzname;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00030.o}
procedure _getsystime;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00031.o}
procedure _gmtime32;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00032.o}
procedure _gmtime32_s;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00033.o}
procedure gmtime;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00034.o}
procedure _gmtime64;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00035.o}
procedure _gmtime64_s;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00036.o}
procedure _localtime32;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00037.o}
procedure _localtime32_s;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00038.o}
procedure _localtime64;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00039.o}
procedure localtime;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00040.o}
procedure _localtime64_s;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00041.o}
procedure _mkgmtime32;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00042.o}
procedure _mkgmtime64;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00043.o}
procedure _mktime32;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00044.o}
procedure mktime;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00045.o}
procedure _mktime64;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00046.o}
procedure _setsystime;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00047.o}
procedure _strdate;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00048.o}
procedure _strdate_s;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00049.o}
procedure _strftime_l;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00050.o}
procedure _strtime;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00051.o}
procedure _strtime_s;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00052.o}
procedure _time32;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00053.o}
procedure time;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00054.o}
procedure _time64;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00055.o}
procedure _timespec32_get;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00056.o}
procedure _timespec64_get;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00057.o}
procedure _utime32;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00059.o}
procedure _utime;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00060.o}
procedure _utime64;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00061.o}
procedure _wasctime;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00062.o}
procedure utime;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00063.o}
procedure _wasctime_s;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00064.o}
procedure _wcsftime_l;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00065.o}
procedure _wctime32;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00066.o}
procedure _wctime32_s;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00067.o}
procedure _wctime64;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00068.o}
procedure _wctime64_s;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00069.o}
procedure _wutime;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00070.o}
procedure _wstrdate;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00071.o}
procedure _wstrdate_s;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00072.o}
procedure _wstrtime;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00073.o}
procedure _wstrtime_s;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00074.o}
procedure _wutime32;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00075.o}
procedure _wutime64;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00076.o}
procedure asctime;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00077.o}
procedure asctime_s;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00078.o}
procedure clock;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00079.o}
procedure strftime;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00080.o}
procedure timespec_get;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00081.o}
procedure wcsftime;external;
{$L x64/libapi-ms-win-crt-time-l1-1-0s00082.o}
procedure _iswalpha_l;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00000.o}
procedure _strcmpi;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00001.o}
procedure __isascii;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00002.o}
procedure __iscsym;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00003.o}
procedure iscsymf;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00004.o}
procedure __iscsymf;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00005.o}
procedure __iswcsym;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00006.o}
procedure iscsym;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00007.o}
procedure __iswcsymf;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00008.o}
procedure __strncnt;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00009.o}
procedure __wcsncnt;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00010.o}
procedure _isalnum_l;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00011.o}
procedure _isalpha_l;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00012.o}
procedure _isblank_l;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00013.o}
procedure _iscntrl_l;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00014.o}
procedure _isctype;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00015.o}
procedure _isctype_l;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00016.o}
procedure _isdigit_l;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00017.o}
procedure _isgraph_l;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00018.o}
procedure _isleadbyte_l;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00019.o}
procedure _islower_l;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00020.o}
procedure _isprint_l;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00021.o}
procedure isascii;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00022.o}
procedure _ispunct_l;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00023.o}
procedure _isspace_l;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00024.o}
procedure _isupper_l;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00025.o}
procedure _iswalnum_l;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00026.o}
procedure _iswblank_l;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00027.o}
procedure _iswcntrl_l;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00028.o}
procedure _iswcsym_l;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00029.o}
procedure _iswcsymf_l;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00030.o}
procedure _iswctype_l;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00031.o}
procedure _iswdigit_l;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00032.o}
procedure _iswgraph_l;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00033.o}
procedure _iswlower_l;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00034.o}
procedure _iswprint_l;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00035.o}
procedure _iswpunct_l;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00036.o}
procedure _iswspace_l;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00037.o}
procedure _iswupper_l;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00038.o}
procedure _iswxdigit_l;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00039.o}
procedure _isxdigit_l;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00040.o}
procedure _memccpy;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00041.o}
procedure memicmp;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00042.o}
procedure _memicmp;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00043.o}
procedure _memicmp_l;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00044.o}
procedure _strcoll_l;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00045.o}
procedure memccpy;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00046.o}
procedure _strdup;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00047.o}
procedure strcmpi;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00048.o}
procedure stricmp;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00049.o}
procedure strcasecmp;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00050.o}
procedure strdup;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00051.o}
procedure _stricmp;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00052.o}
procedure _stricmp_l;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00053.o}
procedure stricoll;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00054.o}
procedure _stricoll;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00055.o}
procedure _stricoll_l;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00056.o}
procedure _strlwr;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00057.o}
procedure strlwr;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00058.o}
procedure _strlwr_l;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00059.o}
procedure _strlwr_s;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00060.o}
procedure _strlwr_s_l;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00061.o}
procedure _strncoll;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00062.o}
procedure _strncoll_l;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00063.o}
procedure _strnicmp;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00064.o}
procedure strnicmp;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00065.o}
procedure strncasecmp;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00066.o}
procedure _strnicmp_l;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00067.o}
procedure _strnicoll;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00068.o}
procedure _strnicoll_l;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00069.o}
procedure strnset;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00070.o}
procedure _strnset;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00071.o}
procedure _strnset_s;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00072.o}
procedure strrev;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00073.o}
procedure _strrev;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00074.o}
procedure _strset;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00075.o}
procedure strset;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00076.o}
procedure _strset_s;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00077.o}
procedure strupr;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00078.o}
procedure _strupr;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00079.o}
procedure _strupr_l;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00080.o}
procedure _strupr_s;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00081.o}
procedure _strupr_s_l;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00082.o}
procedure _strxfrm_l;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00083.o}
procedure _tolower;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00084.o}
procedure _tolower_l;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00085.o}
procedure _toupper;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00086.o}
procedure _toupper_l;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00087.o}
procedure _towlower_l;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00088.o}
procedure _towupper_l;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00089.o}
procedure _wcscoll_l;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00090.o}
procedure wcsdup;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00091.o}
procedure _wcsdup;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00092.o}
procedure wcsicmp;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00093.o}
procedure wcscmpi;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00094.o}
procedure _wcsicmp;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00095.o}
procedure _wcsicmp_l;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00096.o}
procedure _wcsicoll;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00097.o}
procedure wcsicoll;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00098.o}
procedure _wcsicoll_l;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00099.o}
procedure _wcslwr;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00100.o}
procedure wcslwr;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00101.o}
procedure _wcslwr_l;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00102.o}
procedure _wcslwr_s;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00103.o}
procedure _wcslwr_s_l;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00104.o}
procedure _wcsncoll;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00105.o}
procedure _wcsncoll_l;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00106.o}
procedure _wcsnicmp;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00107.o}
procedure wcsnicmp;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00108.o}
procedure _wcsnicmp_l;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00109.o}
procedure _wcsnicoll;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00110.o}
procedure _wcsnicoll_l;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00111.o}
procedure _wcsnset;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00112.o}
procedure _wcsnset_s;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00113.o}
procedure wcsnset;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00114.o}
procedure _wcsrev;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00115.o}
procedure wcsrev;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00116.o}
procedure _wcsset;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00117.o}
procedure _wcsset_s;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00118.o}
procedure wcsupr;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00119.o}
procedure _wcsupr;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00120.o}
procedure wcsset;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00121.o}
procedure _wcsupr_l;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00122.o}
procedure _wcsupr_s;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00123.o}
procedure _wcsupr_s_l;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00124.o}
procedure _wcsxfrm_l;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00125.o}
procedure _wctype;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00126.o}
procedure is_wctype;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00127.o}
procedure isalnum;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00128.o}
procedure isalpha;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00129.o}
procedure isblank;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00130.o}
procedure iscntrl;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00131.o}
procedure isdigit;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00132.o}
procedure isgraph;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00133.o}
procedure isleadbyte;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00134.o}
procedure islower;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00135.o}
procedure isprint;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00136.o}
procedure ispunct;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00137.o}
procedure isspace;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00138.o}
procedure isupper;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00139.o}
procedure iswalnum;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00140.o}
procedure iswalpha;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00141.o}
procedure iswascii;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00142.o}
procedure iswblank;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00143.o}
procedure iswcntrl;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00144.o}
procedure iswctype;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00145.o}
procedure iswdigit;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00146.o}
procedure iswgraph;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00147.o}
procedure iswlower;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00148.o}
procedure iswprint;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00149.o}
procedure iswpunct;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00150.o}
procedure iswspace;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00151.o}
procedure iswupper;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00152.o}
procedure iswxdigit;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00153.o}
procedure isxdigit;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00154.o}
procedure mblen;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00155.o}
procedure mbrlen;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00156.o}
procedure memcpy_s;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00157.o}
procedure memmove_s;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00158.o}
procedure memset;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00159.o}
procedure strcat;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00160.o}
procedure strcat_s;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00161.o}
procedure strcmp;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00162.o}
procedure strcoll;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00163.o}
procedure strcpy;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00164.o}
procedure strcpy_s;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00165.o}
procedure strcspn;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00166.o}
procedure strlen;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00167.o}
procedure strncat;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00168.o}
procedure strncat_s;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00169.o}
procedure strncmp;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00170.o}
procedure strncpy;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00171.o}
procedure strncpy_s;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00172.o}
procedure strpbrk;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00173.o}
procedure strspn;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00174.o}
procedure strtok;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00175.o}
procedure strtok_s;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00176.o}
procedure strxfrm;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00177.o}
procedure tolower;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00178.o}
procedure toupper;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00179.o}
procedure towctrans;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00180.o}
procedure towlower;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00181.o}
procedure towupper;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00182.o}
procedure wcscat;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00183.o}
procedure wcscat_s;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00184.o}
procedure wcscmp;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00185.o}
procedure wcscoll;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00186.o}
procedure wcscpy;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00187.o}
procedure wcscpy_s;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00188.o}
procedure wcscspn;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00189.o}
procedure wcslen;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00190.o}
procedure wcsncat;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00191.o}
procedure wcsncat_s;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00192.o}
procedure wcsncmp;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00193.o}
procedure wcsncpy;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00194.o}
procedure wcsncpy_s;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00195.o}
procedure wcspbrk;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00197.o}
procedure wcsspn;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00198.o}
procedure wcstok;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00199.o}
procedure wcstok_s;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00200.o}
procedure wcsxfrm;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00201.o}
procedure wctype;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00202.o}
procedure wmemcpy_s;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00203.o}
procedure wmemmove_s;external;
{$L x64/libapi-ms-win-crt-string-l1-1-0s00204.o}
procedure __acrt_iob_func;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00000.o}
procedure __p__commode;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00001.o}
procedure __p__fmode;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00002.o}
procedure __stdio_common_vfprintf;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00003.o}
procedure __stdio_common_vfprintf_p;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00004.o}
procedure __stdio_common_vfprintf_s;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00005.o}
procedure __stdio_common_vfscanf;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00006.o}
procedure __stdio_common_vfwprintf;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00007.o}
procedure __stdio_common_vfwprintf_p;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00008.o}
procedure __stdio_common_vfwprintf_s;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00009.o}
procedure __stdio_common_vfwscanf;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00010.o}
procedure __stdio_common_vsnprintf_s;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00011.o}
procedure __stdio_common_vsnwprintf_s;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00012.o}
procedure __stdio_common_vsprintf;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00013.o}
procedure __stdio_common_vsprintf_p;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00014.o}
procedure __stdio_common_vsprintf_s;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00015.o}
procedure __stdio_common_vsscanf;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00016.o}
procedure __stdio_common_vswprintf;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00017.o}
procedure __stdio_common_vswprintf_p;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00018.o}
procedure __stdio_common_vswprintf_s;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00019.o}
procedure __stdio_common_vswscanf;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00020.o}
procedure _chsize;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00021.o}
procedure chsize;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00022.o}
procedure _chsize_s;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00023.o}
procedure _close;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00024.o}
procedure _commit;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00025.o}
procedure creat;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00026.o}
procedure _creat;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00027.o}
procedure close;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00028.o}
procedure _dup;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00029.o}
procedure dup;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00030.o}
procedure _dup2;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00031.o}
procedure eof;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00032.o}
procedure _eof;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00033.o}
procedure dup2;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00034.o}
procedure _fclose_nolock;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00035.o}
procedure _fcloseall;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00036.o}
procedure _fflush_nolock;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00037.o}
procedure _fgetc_nolock;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00038.o}
procedure _fgetchar;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00039.o}
procedure fgetchar;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00040.o}
procedure _fgetwc_nolock;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00041.o}
procedure _fgetwchar;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00042.o}
procedure filelength;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00043.o}
procedure _filelength;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00044.o}
procedure _filelengthi64;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00045.o}
procedure fgetwchar;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00046.o}
procedure _fileno;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00047.o}
procedure fileno;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00048.o}
procedure _flushall;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00049.o}
procedure _fputc_nolock;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00050.o}
procedure _fputchar;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00051.o}
procedure _fputwc_nolock;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00052.o}
procedure fputchar;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00053.o}
procedure _fputwchar;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00054.o}
procedure fputwchar;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00055.o}
procedure _fread_nolock;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00056.o}
procedure _fread_nolock_s;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00057.o}
procedure _fseek_nolock;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00058.o}
procedure _fseeki64;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00059.o}
procedure _fseeki64_nolock;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00060.o}
procedure _fsopen;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00061.o}
procedure _ftell_nolock;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00062.o}
procedure _ftelli64;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00063.o}
procedure _ftelli64_nolock;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00064.o}
procedure _fwrite_nolock;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00065.o}
procedure _get_fmode;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00066.o}
procedure _get_osfhandle;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00067.o}
procedure _get_printf_count_output;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00068.o}
procedure _get_stream_buffer_pointers;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00069.o}
procedure _getc_nolock;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00070.o}
procedure _getcwd;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00071.o}
procedure getcwd;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00072.o}
procedure _getdcwd;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00073.o}
procedure _getmaxstdio;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00074.o}
procedure _getw;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00075.o}
procedure getw;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00076.o}
procedure _getwc_nolock;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00077.o}
procedure _getws;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00078.o}
procedure _getws_s;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00079.o}
procedure _isatty;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00080.o}
procedure isatty;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00081.o}
procedure _kbhit;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00082.o}
procedure kbhit;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00083.o}
procedure _locking;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00084.o}
procedure _lseek;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00085.o}
procedure _lseeki64;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00086.o}
procedure mktemp;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00087.o}
procedure _mktemp;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00088.o}
procedure _mktemp_s;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00089.o}
procedure _open;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00090.o}
procedure lseek;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00091.o}
procedure open;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00092.o}
procedure pclose;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00093.o}
procedure _pclose;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00094.o}
procedure _open_osfhandle;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00095.o}
procedure _pipe;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00096.o}
procedure _popen;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00097.o}
procedure _putc_nolock;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00098.o}
procedure popen;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00099.o}
procedure _putw;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00100.o}
procedure putw;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00101.o}
procedure _putwc_nolock;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00102.o}
procedure _putws;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00103.o}
procedure _read;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00104.o}
procedure read;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00105.o}
procedure _rmtmp;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00106.o}
procedure rmtmp;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00107.o}
procedure _set_fmode;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00108.o}
procedure _set_printf_count_output;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00109.o}
procedure _setmaxstdio;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00110.o}
procedure setmode;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00111.o}
procedure _setmode;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00112.o}
procedure _sopen;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00113.o}
procedure _sopen_dispatch;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00114.o}
procedure _sopen_s;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00115.o}
procedure sopen;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00116.o}
procedure _tell;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00117.o}
procedure tell;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00118.o}
procedure _telli64;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00119.o}
procedure _tempnam;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00120.o}
procedure _ungetc_nolock;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00121.o}
procedure _ungetwc_nolock;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00122.o}
procedure _wcreat;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00123.o}
procedure tempnam;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00124.o}
procedure _wfdopen;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00125.o}
procedure _wfopen;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00126.o}
procedure _wfopen_s;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00127.o}
procedure _wfreopen;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00128.o}
procedure _wfreopen_s;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00129.o}
procedure _wfsopen;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00130.o}
procedure _wmktemp;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00131.o}
procedure _wmktemp_s;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00132.o}
procedure _wopen;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00133.o}
procedure _wpopen;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00134.o}
procedure wpopen;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00135.o}
procedure _write;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00136.o}
procedure write;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00137.o}
procedure _wsopen;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00138.o}
procedure _wsopen_dispatch;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00139.o}
procedure _wsopen_s;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00140.o}
procedure _wtempnam;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00141.o}
procedure _wtmpnam;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00142.o}
procedure _wtmpnam_s;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00143.o}
procedure clearerr;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00144.o}
procedure clearerr_s;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00145.o}
procedure fclose;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00146.o}
procedure feof;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00147.o}
procedure ferror;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00148.o}
procedure fflush;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00149.o}
procedure fgetc;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00150.o}
procedure fgetpos;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00151.o}
procedure fgets;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00152.o}
procedure fgetwc;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00153.o}
procedure fgetws;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00154.o}
procedure fopen;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00155.o}
procedure fopen_s;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00156.o}
procedure fputc;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00157.o}
procedure fputs;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00158.o}
procedure fputwc;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00159.o}
procedure fputws;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00160.o}
procedure fread;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00161.o}
procedure fread_s;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00162.o}
procedure freopen;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00163.o}
procedure freopen_s;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00164.o}
procedure fseek;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00165.o}
procedure fsetpos;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00166.o}
procedure ftell;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00167.o}
procedure fwrite;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00168.o}
procedure getc;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00169.o}
procedure getchar;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00170.o}
procedure gets;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00171.o}
procedure gets_s;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00172.o}
procedure getwc;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00173.o}
procedure getwchar;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00174.o}
procedure putc;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00175.o}
procedure putchar;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00176.o}
procedure puts;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00177.o}
procedure putwc;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00178.o}
procedure putwchar;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00179.o}
procedure rewind;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00180.o}
procedure setbuf;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00181.o}
procedure setvbuf;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00182.o}
procedure tmpfile;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00183.o}
procedure tmpfile_s;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00184.o}
procedure tmpnam;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00185.o}
procedure tmpnam_s;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00186.o}
procedure ungetc;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00187.o}
procedure ungetwc;external;
{$L x64/libapi-ms-win-crt-stdio-l1-1-0s00188.o}
procedure _Exit;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00000.o}
procedure __doserrno;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00001.o}
procedure __fpe_flt_rounds;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00002.o}
procedure __fpecode;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00003.o}
procedure __p___argc;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00004.o}
procedure __p___argv;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00005.o}
procedure __p___wargv;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00006.o}
procedure __p__acmdln;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00007.o}
procedure __p__pgmptr;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00008.o}
procedure __p__wcmdln;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00009.o}
procedure __p__wpgmptr;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00010.o}
procedure __pxcptinfoptrs;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00011.o}
procedure __sys_errlist;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00012.o}
procedure __sys_nerr;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00013.o}
procedure __threadhandle;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00014.o}
procedure __threadid;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00015.o}
procedure __wcserror;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00016.o}
procedure __wcserror_s;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00017.o}
procedure _assert;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00018.o}
procedure _beginthread;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00019.o}
procedure _beginthreadex;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00020.o}
procedure _c_exit;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00021.o}
procedure _cexit;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00022.o}
procedure _clearfp;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00023.o}
procedure _configure_narrow_argv;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00024.o}
procedure _configure_wide_argv;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00025.o}
procedure _control87;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00026.o}
procedure _controlfp;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00027.o}
procedure _controlfp_s;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00028.o}
procedure _crt_at_quick_exit;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00029.o}
procedure _crt_atexit;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00030.o}
procedure _crt_debugger_hook;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00031.o}
procedure _endthread;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00032.o}
procedure _endthreadex;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00033.o}
procedure _errno;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00034.o}
procedure _execute_onexit_table;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00035.o}
procedure _fpieee_flt;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00037.o}
procedure _get_doserrno;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00039.o}
procedure _get_errno;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00040.o}
procedure _get_initial_narrow_environment;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00041.o}
procedure _get_initial_wide_environment;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00042.o}
procedure _get_invalid_parameter_handler;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00043.o}
procedure _get_narrow_winmain_command_line;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00044.o}
procedure _get_pgmptr;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00045.o}
procedure _get_terminate;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00046.o}
procedure _get_thread_local_invalid_parameter_handler;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00047.o}
procedure _get_wide_winmain_command_line;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00048.o}
procedure _get_wpgmptr;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00049.o}
procedure _getdllprocaddr;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00050.o}
procedure _getpid;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00051.o}
procedure getpid;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00052.o}
procedure _initialize_narrow_environment;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00053.o}
procedure _initialize_onexit_table;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00054.o}
procedure _initialize_wide_environment;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00055.o}
procedure _initterm;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00056.o}
procedure _initterm_e;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00057.o}
procedure _invalid_parameter_noinfo;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00058.o}
procedure _invalid_parameter_noinfo_noreturn;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00059.o}
procedure _invoke_watson;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00060.o}
procedure _query_app_type;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00061.o}
procedure _register_onexit_function;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00062.o}
procedure _register_thread_local_exe_atexit_callback;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00063.o}
procedure _resetstkoflw;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00064.o}
procedure _seh_filter_dll;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00065.o}
procedure _seh_filter_exe;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00066.o}
procedure _set_abort_behavior;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00067.o}
procedure __set_app_type;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00068.o}
procedure _set_app_type;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00069.o}
procedure _set_controlfp;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00070.o}
procedure _set_doserrno;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00071.o}
procedure _set_errno;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00072.o}
procedure _set_error_mode;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00073.o}
procedure _set_invalid_parameter_handler;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00074.o}
procedure _set_new_handler;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00075.o}
procedure _set_thread_local_invalid_parameter_handler;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00076.o}
procedure _seterrormode;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00077.o}
procedure _sleep;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00078.o}
procedure _statusfp;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00079.o}
procedure _strerror;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00080.o}
procedure _strerror_s;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00081.o}
procedure _wassert;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00082.o}
procedure _wcserror;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00083.o}
procedure _wcserror_s;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00084.o}
procedure _wperror;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00085.o}
procedure _wsystem;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00086.o}
procedure abort;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00087.o}
procedure exit;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00088.o}
procedure perror;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00098.o}
procedure quick_exit;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00099.o}
procedure &raise;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00100.o}
procedure set_terminate;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00101.o}
procedure signal;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00102.o}
procedure strerror;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00103.o}
procedure strerror_s;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00104.o}
procedure system;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00105.o}
procedure terminate;external;
{$L x64/libapi-ms-win-crt-runtime-l1-1-0s00106.o}
procedure _beep;external;
{$L x64/libapi-ms-win-crt-process-l1-1-0s00000.o}
procedure _cwait;external;
{$L x64/libapi-ms-win-crt-process-l1-1-0s00001.o}
procedure execl;external;
{$L x64/libapi-ms-win-crt-process-l1-1-0s00002.o}
procedure _execl;external;
{$L x64/libapi-ms-win-crt-process-l1-1-0s00003.o}
procedure cwait;external;
{$L x64/libapi-ms-win-crt-process-l1-1-0s00004.o}
procedure _execle;external;
{$L x64/libapi-ms-win-crt-process-l1-1-0s00005.o}
procedure execle;external;
{$L x64/libapi-ms-win-crt-process-l1-1-0s00006.o}
procedure _execlp;external;
{$L x64/libapi-ms-win-crt-process-l1-1-0s00007.o}
procedure execlpe;external;
{$L x64/libapi-ms-win-crt-process-l1-1-0s00008.o}
procedure _execlpe;external;
{$L x64/libapi-ms-win-crt-process-l1-1-0s00009.o}
procedure execlp;external;
{$L x64/libapi-ms-win-crt-process-l1-1-0s00010.o}
procedure _execv;external;
{$L x64/libapi-ms-win-crt-process-l1-1-0s00011.o}
procedure execv;external;
{$L x64/libapi-ms-win-crt-process-l1-1-0s00012.o}
procedure _execve;external;
{$L x64/libapi-ms-win-crt-process-l1-1-0s00013.o}
procedure execve;external;
{$L x64/libapi-ms-win-crt-process-l1-1-0s00014.o}
procedure _execvp;external;
{$L x64/libapi-ms-win-crt-process-l1-1-0s00015.o}
procedure execvpe;external;
{$L x64/libapi-ms-win-crt-process-l1-1-0s00016.o}
procedure _execvpe;external;
{$L x64/libapi-ms-win-crt-process-l1-1-0s00017.o}
procedure _loaddll;external;
{$L x64/libapi-ms-win-crt-process-l1-1-0s00018.o}
procedure execvp;external;
{$L x64/libapi-ms-win-crt-process-l1-1-0s00019.o}
procedure _spawnl;external;
{$L x64/libapi-ms-win-crt-process-l1-1-0s00020.o}
procedure spawnl;external;
{$L x64/libapi-ms-win-crt-process-l1-1-0s00021.o}
procedure _spawnle;external;
{$L x64/libapi-ms-win-crt-process-l1-1-0s00022.o}
procedure _spawnlp;external;
{$L x64/libapi-ms-win-crt-process-l1-1-0s00023.o}
procedure spawnlpe;external;
{$L x64/libapi-ms-win-crt-process-l1-1-0s00024.o}
procedure spawnle;external;
{$L x64/libapi-ms-win-crt-process-l1-1-0s00025.o}
procedure _spawnlpe;external;
{$L x64/libapi-ms-win-crt-process-l1-1-0s00026.o}
procedure spawnlp;external;
{$L x64/libapi-ms-win-crt-process-l1-1-0s00027.o}
procedure _spawnv;external;
{$L x64/libapi-ms-win-crt-process-l1-1-0s00028.o}
procedure spawnve;external;
{$L x64/libapi-ms-win-crt-process-l1-1-0s00029.o}
procedure _spawnve;external;
{$L x64/libapi-ms-win-crt-process-l1-1-0s00030.o}
procedure spawnvp;external;
{$L x64/libapi-ms-win-crt-process-l1-1-0s00031.o}
procedure _spawnvp;external;
{$L x64/libapi-ms-win-crt-process-l1-1-0s00032.o}
procedure spawnv;external;
{$L x64/libapi-ms-win-crt-process-l1-1-0s00033.o}
procedure _spawnvpe;external;
{$L x64/libapi-ms-win-crt-process-l1-1-0s00034.o}
procedure spawnvpe;external;
{$L x64/libapi-ms-win-crt-process-l1-1-0s00035.o}
procedure _unloaddll;external;
{$L x64/libapi-ms-win-crt-process-l1-1-0s00036.o}
procedure _wexecl;external;
{$L x64/libapi-ms-win-crt-process-l1-1-0s00037.o}
procedure _wexecle;external;
{$L x64/libapi-ms-win-crt-process-l1-1-0s00038.o}
procedure _wexeclp;external;
{$L x64/libapi-ms-win-crt-process-l1-1-0s00039.o}
procedure _wexeclpe;external;
{$L x64/libapi-ms-win-crt-process-l1-1-0s00040.o}
procedure _wexecv;external;
{$L x64/libapi-ms-win-crt-process-l1-1-0s00041.o}
procedure _wexecve;external;
{$L x64/libapi-ms-win-crt-process-l1-1-0s00042.o}
procedure _wexecvp;external;
{$L x64/libapi-ms-win-crt-process-l1-1-0s00043.o}
procedure _wexecvpe;external;
{$L x64/libapi-ms-win-crt-process-l1-1-0s00044.o}
procedure _wspawnl;external;
{$L x64/libapi-ms-win-crt-process-l1-1-0s00045.o}
procedure _wspawnle;external;
{$L x64/libapi-ms-win-crt-process-l1-1-0s00046.o}
procedure _wspawnlp;external;
{$L x64/libapi-ms-win-crt-process-l1-1-0s00047.o}
procedure _wspawnlpe;external;
{$L x64/libapi-ms-win-crt-process-l1-1-0s00048.o}
procedure _wspawnv;external;
{$L x64/libapi-ms-win-crt-process-l1-1-0s00049.o}
procedure _wspawnve;external;
{$L x64/libapi-ms-win-crt-process-l1-1-0s00050.o}
procedure _wspawnvp;external;
{$L x64/libapi-ms-win-crt-process-l1-1-0s00051.o}
procedure _wspawnvpe;external;
{$L x64/libapi-ms-win-crt-process-l1-1-0s00052.o}
procedure _CreateFrameInfo;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00000.o}
procedure _CxxThrowException;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00001.o}
procedure _FindAndUnlinkFrame;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00002.o}
procedure _GetImageBase;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00003.o}
procedure _GetThrowImageBase;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00004.o}
procedure _IsExceptionObjectToBeDestroyed;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00005.o}
procedure _NLG_Dispatch2;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00006.o}
procedure _NLG_Return;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00007.o}
procedure _NLG_Return2;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00008.o}
procedure _SetImageBase;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00009.o}
procedure _SetThrowImageBase;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00010.o}
procedure _SetWinRTOutOfMemoryExceptionCallback;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00011.o}
procedure __AdjustPointer;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00012.o}
procedure __BuildCatchObject;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00013.o}
procedure __BuildCatchObjectHelper;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00014.o}
procedure __C_specific_handler;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00015.o}
procedure __CxxDetectRethrow;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00016.o}
procedure __CxxExceptionFilter;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00017.o}
procedure __CxxFrameHandler;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00018.o}
procedure __CxxFrameHandler2;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00019.o}
procedure __CxxFrameHandler3;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00020.o}
procedure __CxxQueryExceptionSize;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00021.o}
procedure __CxxRegisterExceptionObject;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00022.o}
procedure __CxxUnregisterExceptionObject;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00023.o}
procedure __DestructExceptionObject;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00024.o}
procedure __FrameUnwindFilter;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00025.o}
procedure __GetPlatformExceptionInfo;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00026.o}
procedure __NLG_Dispatch2;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00027.o}
procedure __NLG_Return2;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00028.o}
procedure __RTCastToVoid;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00029.o}
procedure __RTDynamicCast;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00030.o}
procedure __RTtypeid;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00031.o}
procedure __TypeMatch;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00032.o}
procedure __current_exception;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00033.o}
procedure __current_exception_context;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00034.o}
procedure __dcrt_get_wide_environment_from_os;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00035.o}
procedure __dcrt_initial_narrow_environment;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00036.o}
procedure __intrinsic_abnormal_termination;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00037.o}
procedure __intrinsic_setjmp;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00038.o}
procedure __intrinsic_setjmpex;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00039.o}
procedure __processing_throw;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00040.o}
procedure __report_gsfailure;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00041.o}
procedure __std_exception_copy;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00042.o}
procedure __std_exception_destroy;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00043.o}
procedure __std_type_info_compare;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00044.o}
procedure __std_type_info_destroy_list;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00045.o}
procedure __std_type_info_hash;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00046.o}
procedure __std_type_info_name;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00047.o}
procedure __unDName;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00048.o}
procedure __unDNameEx;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00049.o}
procedure __uncaught_exception;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00050.o}
procedure _get_purecall_handler;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00051.o}
procedure _get_unexpected;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00052.o}
procedure _is_exception_typeof;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00053.o}
procedure _local_unwind;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00054.o}
procedure _o__CIacos;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00055.o}
procedure _o__CIasin;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00056.o}
procedure _o__CIatan;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00057.o}
procedure _o__CIatan2;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00058.o}
procedure _o__CIcos;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00059.o}
procedure _o__CIcosh;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00060.o}
procedure _o__CIexp;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00061.o}
procedure _o__CIfmod;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00062.o}
procedure _o__CIlog;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00063.o}
procedure _o__CIlog10;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00064.o}
procedure _o__CIpow;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00065.o}
procedure _o__CIsin;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00066.o}
procedure _o__CIsinh;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00067.o}
procedure _o__CIsqrt;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00068.o}
procedure _o__CItan;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00069.o}
procedure _o__CItanh;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00070.o}
procedure _o__Getdays;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00071.o}
procedure _o__Getmonths;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00072.o}
procedure _o__Gettnames;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00073.o}
procedure _o__Strftime;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00074.o}
procedure _o__W_Getdays;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00075.o}
procedure _o__W_Getmonths;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00076.o}
procedure _o__W_Gettnames;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00077.o}
procedure _o__Wcsftime;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00078.o}
procedure _o___acrt_iob_func;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00079.o}
procedure _o___conio_common_vcprintf;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00080.o}
procedure _o___conio_common_vcprintf_p;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00081.o}
procedure _o___conio_common_vcprintf_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00082.o}
procedure _o___conio_common_vcscanf;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00083.o}
procedure _o___conio_common_vcwprintf;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00084.o}
procedure _o___conio_common_vcwprintf_p;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00085.o}
procedure _o___conio_common_vcwprintf_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00086.o}
procedure _o___conio_common_vcwscanf;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00087.o}
procedure _o___daylight;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00088.o}
procedure _o___dstbias;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00089.o}
procedure _o___fpe_flt_rounds;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00090.o}
procedure _o___libm_sse2_acos;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00091.o}
procedure _o___libm_sse2_acosf;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00092.o}
procedure _o___libm_sse2_asin;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00093.o}
procedure _o___libm_sse2_asinf;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00094.o}
procedure _o___libm_sse2_atan;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00095.o}
procedure _o___libm_sse2_atan2;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00096.o}
procedure _o___libm_sse2_atanf;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00097.o}
procedure _o___libm_sse2_cos;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00098.o}
procedure _o___libm_sse2_cosf;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00099.o}
procedure _o___libm_sse2_exp;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00100.o}
procedure _o___libm_sse2_expf;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00101.o}
procedure _o___libm_sse2_log;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00102.o}
procedure _o___libm_sse2_log10;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00103.o}
procedure _o___libm_sse2_log10f;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00104.o}
procedure _o___libm_sse2_logf;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00105.o}
procedure _o___libm_sse2_pow;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00106.o}
procedure _o___libm_sse2_powf;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00107.o}
procedure _o___libm_sse2_sin;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00108.o}
procedure _o___libm_sse2_sinf;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00109.o}
procedure _o___libm_sse2_tan;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00110.o}
procedure _o___libm_sse2_tanf;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00111.o}
procedure _o___p___argc;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00112.o}
procedure _o___p___argv;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00113.o}
procedure _o___p___wargv;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00114.o}
procedure _o___p__acmdln;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00115.o}
procedure _o___p__commode;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00116.o}
procedure _o___p__environ;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00117.o}
procedure _o___p__fmode;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00118.o}
procedure _o___p__mbcasemap;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00119.o}
procedure _o___p__mbctype;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00120.o}
procedure _o___p__pgmptr;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00121.o}
procedure _o___p__wcmdln;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00122.o}
procedure _o___p__wenviron;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00123.o}
procedure _o___p__wpgmptr;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00124.o}
procedure _o___pctype_func;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00125.o}
procedure _o___pwctype_func;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00126.o}
procedure _o___stdio_common_vfprintf;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00127.o}
procedure _o___stdio_common_vfprintf_p;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00128.o}
procedure _o___stdio_common_vfprintf_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00129.o}
procedure _o___stdio_common_vfscanf;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00130.o}
procedure _o___stdio_common_vfwprintf;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00131.o}
procedure _o___stdio_common_vfwprintf_p;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00132.o}
procedure _o___stdio_common_vfwprintf_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00133.o}
procedure _o___stdio_common_vfwscanf;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00134.o}
procedure _o___stdio_common_vsnprintf_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00135.o}
procedure _o___stdio_common_vsnwprintf_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00136.o}
procedure _o___stdio_common_vsprintf;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00137.o}
procedure _o___stdio_common_vsprintf_p;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00138.o}
procedure _o___stdio_common_vsprintf_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00139.o}
procedure _o___stdio_common_vsscanf;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00140.o}
procedure _o___stdio_common_vswprintf;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00141.o}
procedure _o___stdio_common_vswprintf_p;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00142.o}
procedure _o___stdio_common_vswprintf_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00143.o}
procedure _o___stdio_common_vswscanf;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00144.o}
procedure _o___timezone;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00145.o}
procedure _o___tzname;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00146.o}
procedure _o___wcserror;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00147.o}
procedure _o__access;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00148.o}
procedure _o__access_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00149.o}
procedure _o__aligned_free;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00150.o}
procedure _o__aligned_malloc;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00151.o}
procedure _o__aligned_msize;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00152.o}
procedure _o__aligned_offset_malloc;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00153.o}
procedure _o__aligned_offset_realloc;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00154.o}
procedure _o__aligned_offset_recalloc;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00155.o}
procedure _o__aligned_realloc;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00156.o}
procedure _o__aligned_recalloc;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00157.o}
procedure _o__atodbl;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00158.o}
procedure _o__atodbl_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00159.o}
procedure _o__atof_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00160.o}
procedure _o__atoflt;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00161.o}
procedure _o__atoflt_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00162.o}
procedure _o__atoi64;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00163.o}
procedure _o__atoi64_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00164.o}
procedure _o__atoi_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00165.o}
procedure _o__atol_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00166.o}
procedure _o__atoldbl;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00167.o}
procedure _o__atoldbl_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00168.o}
procedure _o__atoll_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00169.o}
procedure _o__beep;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00170.o}
procedure _o__beginthread;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00171.o}
procedure _o__beginthreadex;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00172.o}
procedure _o__cabs;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00173.o}
procedure _o__callnewh;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00174.o}
procedure _o__calloc_base;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00175.o}
procedure _o__cgets;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00176.o}
procedure _o__cgets_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00177.o}
procedure _o__cgetws;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00178.o}
procedure _o__cgetws_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00179.o}
procedure _o__chdir;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00180.o}
procedure _o__chdrive;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00181.o}
procedure _o__chmod;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00182.o}
procedure _o__chsize;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00183.o}
procedure _o__chsize_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00184.o}
procedure _o__close;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00185.o}
procedure _o__commit;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00186.o}
procedure _o__configure_wide_argv;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00187.o}
procedure _o__cputs;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00188.o}
procedure _o__cputws;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00189.o}
procedure _o__creat;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00190.o}
procedure _o__create_locale;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00191.o}
procedure _o__ctime32_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00192.o}
procedure _o__ctime64_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00193.o}
procedure _o__cwait;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00194.o}
procedure _o__d_int;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00195.o}
procedure _o__dclass;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00196.o}
procedure _o__difftime32;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00197.o}
procedure _o__difftime64;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00198.o}
procedure _o__dlog;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00199.o}
procedure _o__dnorm;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00200.o}
procedure _o__dpcomp;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00201.o}
procedure _o__dpoly;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00202.o}
procedure _o__dscale;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00203.o}
procedure _o__dsign;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00204.o}
procedure _o__dsin;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00205.o}
procedure _o__dtest;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00206.o}
procedure _o__dunscale;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00207.o}
procedure _o__dup;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00208.o}
procedure _o__dup2;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00209.o}
procedure _o__dupenv_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00210.o}
procedure _o__ecvt;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00211.o}
procedure _o__ecvt_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00212.o}
procedure _o__endthread;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00213.o}
procedure _o__endthreadex;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00214.o}
procedure _o__eof;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00215.o}
procedure _o__errno;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00216.o}
procedure _o__except1;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00217.o}
procedure _o__execute_onexit_table;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00218.o}
procedure _o__execv;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00219.o}
procedure _o__execve;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00220.o}
procedure _o__execvp;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00221.o}
procedure _o__execvpe;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00222.o}
procedure _o__expand;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00223.o}
procedure _o__fclose_nolock;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00224.o}
procedure _o__fcloseall;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00225.o}
procedure _o__fcvt;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00226.o}
procedure _o__fcvt_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00227.o}
procedure _o__fd_int;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00228.o}
procedure _o__fdclass;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00229.o}
procedure _o__fdexp;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00230.o}
procedure _o__fdlog;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00231.o}
procedure _o__fdopen;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00232.o}
procedure _o__fdpcomp;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00233.o}
procedure _o__fdpoly;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00234.o}
procedure _o__fdscale;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00235.o}
procedure _o__fdsign;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00236.o}
procedure _o__fdsin;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00237.o}
procedure _o__fflush_nolock;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00238.o}
procedure _o__fgetc_nolock;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00239.o}
procedure _o__fgetchar;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00240.o}
procedure _o__fgetwc_nolock;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00241.o}
procedure _o__fgetwchar;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00242.o}
procedure _o__filelength;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00243.o}
procedure _o__filelengthi64;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00244.o}
procedure _o__fileno;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00245.o}
procedure _o__findclose;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00246.o}
procedure _o__findfirst32;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00247.o}
procedure _o__findfirst32i64;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00248.o}
procedure _o__findfirst64;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00249.o}
procedure _o__findfirst64i32;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00250.o}
procedure _o__findnext32;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00251.o}
procedure _o__findnext32i64;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00252.o}
procedure _o__findnext64;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00253.o}
procedure _o__findnext64i32;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00254.o}
procedure _o__flushall;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00255.o}
procedure _o__fpclass;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00256.o}
procedure _o__fpclassf;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00257.o}
procedure _o__fputc_nolock;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00258.o}
procedure _o__fputchar;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00259.o}
procedure _o__fputwc_nolock;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00260.o}
procedure _o__fputwchar;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00261.o}
procedure _o__fread_nolock;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00262.o}
procedure _o__fread_nolock_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00263.o}
procedure _o__free_base;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00264.o}
procedure _o__free_locale;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00265.o}
procedure _o__fseek_nolock;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00266.o}
procedure _o__fseeki64;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00267.o}
procedure _o__fseeki64_nolock;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00268.o}
procedure _o__fsopen;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00269.o}
procedure _o__fstat32;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00270.o}
procedure _o__fstat32i64;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00271.o}
procedure _o__fstat64;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00272.o}
procedure _o__fstat64i32;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00273.o}
procedure _o__ftell_nolock;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00274.o}
procedure _o__ftelli64;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00275.o}
procedure _o__ftelli64_nolock;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00276.o}
procedure _o__ftime32;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00277.o}
procedure _o__ftime32_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00278.o}
procedure _o__ftime64;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00279.o}
procedure _o__ftime64_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00280.o}
procedure _o__fullpath;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00281.o}
procedure _o__futime32;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00282.o}
procedure _o__futime64;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00283.o}
procedure _o__fwrite_nolock;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00284.o}
procedure _o__gcvt;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00285.o}
procedure _o__gcvt_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00286.o}
procedure _o__get_daylight;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00287.o}
procedure _o__get_doserrno;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00288.o}
procedure _o__get_dstbias;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00289.o}
procedure _o__get_errno;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00290.o}
procedure _o__get_fmode;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00291.o}
procedure _o__get_heap_handle;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00292.o}
procedure _o__get_invalid_parameter_handler;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00293.o}
procedure _o__get_narrow_winmain_command_line;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00294.o}
procedure _o__get_osfhandle;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00295.o}
procedure _o__get_pgmptr;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00296.o}
procedure _o__get_stream_buffer_pointers;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00297.o}
procedure _o__get_terminate;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00298.o}
procedure _o__get_thread_local_invalid_parameter_handler;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00299.o}
procedure _o__get_timezone;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00300.o}
procedure _o__get_tzname;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00301.o}
procedure _o__get_wide_winmain_command_line;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00302.o}
procedure _o__get_wpgmptr;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00303.o}
procedure _o__getc_nolock;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00304.o}
procedure _o__getch;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00305.o}
procedure _o__getch_nolock;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00306.o}
procedure _o__getche;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00307.o}
procedure _o__getche_nolock;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00308.o}
procedure _o__getcwd;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00309.o}
procedure _o__getdcwd;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00310.o}
procedure _o__getdiskfree;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00311.o}
procedure _o__getdllprocaddr;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00312.o}
procedure _o__getdrive;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00313.o}
procedure _o__getdrives;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00314.o}
procedure _o__getmbcp;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00315.o}
procedure _o__getsystime;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00316.o}
procedure _o__getw;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00317.o}
procedure _o__getwc_nolock;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00318.o}
procedure _o__getwch;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00319.o}
procedure _o__getwch_nolock;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00320.o}
procedure _o__getwche;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00321.o}
procedure _o__getwche_nolock;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00322.o}
procedure _o__getws;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00323.o}
procedure _o__getws_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00324.o}
procedure _o__gmtime32;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00325.o}
procedure _o__gmtime32_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00326.o}
procedure _o__gmtime64;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00327.o}
procedure _o__gmtime64_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00328.o}
procedure _o__heapchk;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00329.o}
procedure _o__heapmin;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00330.o}
procedure _o__hypot;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00331.o}
procedure _o__hypotf;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00332.o}
procedure _o__i64toa;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00333.o}
procedure _o__i64toa_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00334.o}
procedure _o__i64tow;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00335.o}
procedure _o__i64tow_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00336.o}
procedure _o__initialize_onexit_table;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00337.o}
procedure _o__invalid_parameter_noinfo;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00338.o}
procedure _o__invalid_parameter_noinfo_noreturn;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00339.o}
procedure _o__isatty;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00340.o}
procedure _o__isctype;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00341.o}
procedure _o__isctype_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00342.o}
procedure _o__isleadbyte_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00343.o}
procedure _o__ismbbalnum;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00344.o}
procedure _o__ismbbalnum_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00345.o}
procedure _o__ismbbalpha;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00346.o}
procedure _o__ismbbalpha_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00347.o}
procedure _o__ismbbblank;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00348.o}
procedure _o__ismbbblank_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00349.o}
procedure _o__ismbbgraph;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00350.o}
procedure _o__ismbbgraph_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00351.o}
procedure _o__ismbbkalnum;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00352.o}
procedure _o__ismbbkalnum_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00353.o}
procedure _o__ismbbkana;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00354.o}
procedure _o__ismbbkana_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00355.o}
procedure _o__ismbbkprint;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00356.o}
procedure _o__ismbbkprint_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00357.o}
procedure _o__ismbbkpunct;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00358.o}
procedure _o__ismbbkpunct_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00359.o}
procedure _o__ismbblead;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00360.o}
procedure _o__ismbblead_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00361.o}
procedure _o__ismbbprint;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00362.o}
procedure _o__ismbbprint_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00363.o}
procedure _o__ismbbpunct;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00364.o}
procedure _o__ismbbpunct_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00365.o}
procedure _o__ismbbtrail;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00366.o}
procedure _o__ismbbtrail_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00367.o}
procedure _o__ismbcalnum;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00368.o}
procedure _o__ismbcalnum_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00369.o}
procedure _o__ismbcalpha;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00370.o}
procedure _o__ismbcalpha_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00371.o}
procedure _o__ismbcblank;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00372.o}
procedure _o__ismbcblank_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00373.o}
procedure _o__ismbcdigit;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00374.o}
procedure _o__ismbcdigit_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00375.o}
procedure _o__ismbcgraph;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00376.o}
procedure _o__ismbcgraph_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00377.o}
procedure _o__ismbchira;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00378.o}
procedure _o__ismbchira_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00379.o}
procedure _o__ismbckata;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00380.o}
procedure _o__ismbckata_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00381.o}
procedure _o__ismbcl0;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00382.o}
procedure _o__ismbcl0_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00383.o}
procedure _o__ismbcl1;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00384.o}
procedure _o__ismbcl1_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00385.o}
procedure _o__ismbcl2;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00386.o}
procedure _o__ismbcl2_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00387.o}
procedure _o__ismbclegal;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00388.o}
procedure _o__ismbclegal_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00389.o}
procedure _o__ismbclower;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00390.o}
procedure _o__ismbclower_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00391.o}
procedure _o__ismbcprint;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00392.o}
procedure _o__ismbcprint_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00393.o}
procedure _o__ismbcpunct;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00394.o}
procedure _o__ismbcpunct_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00395.o}
procedure _o__ismbcspace;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00396.o}
procedure _o__ismbcspace_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00397.o}
procedure _o__ismbcsymbol;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00398.o}
procedure _o__ismbcsymbol_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00399.o}
procedure _o__ismbcupper;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00400.o}
procedure _o__ismbcupper_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00401.o}
procedure _o__ismbslead;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00402.o}
procedure _o__ismbslead_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00403.o}
procedure _o__ismbstrail;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00404.o}
procedure _o__ismbstrail_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00405.o}
procedure _o__iswctype_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00406.o}
procedure _o__itoa;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00407.o}
procedure _o__itoa_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00408.o}
procedure _o__itow;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00409.o}
procedure _o__itow_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00410.o}
procedure _o__j0;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00411.o}
procedure _o__j1;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00412.o}
procedure _o__jn;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00413.o}
procedure _o__kbhit;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00414.o}
procedure _o__ld_int;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00415.o}
procedure _o__ldclass;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00416.o}
procedure _o__ldexp;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00417.o}
procedure _o__ldlog;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00418.o}
procedure _o__ldpcomp;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00419.o}
procedure _o__ldpoly;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00420.o}
procedure _o__ldscale;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00421.o}
procedure _o__ldsign;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00422.o}
procedure _o__ldsin;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00423.o}
procedure _o__ldtest;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00424.o}
procedure _o__ldunscale;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00425.o}
procedure _o__lfind;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00426.o}
procedure _o__lfind_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00427.o}
procedure _o__libm_sse2_acos_precise;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00428.o}
procedure _o__libm_sse2_asin_precise;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00429.o}
procedure _o__libm_sse2_atan_precise;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00430.o}
procedure _o__libm_sse2_cos_precise;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00431.o}
procedure _o__libm_sse2_exp_precise;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00432.o}
procedure _o__libm_sse2_log10_precise;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00433.o}
procedure _o__libm_sse2_log_precise;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00434.o}
procedure _o__libm_sse2_pow_precise;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00435.o}
procedure _o__libm_sse2_sin_precise;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00436.o}
procedure _o__libm_sse2_sqrt_precise;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00437.o}
procedure _o__libm_sse2_tan_precise;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00438.o}
procedure _o__loaddll;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00439.o}
procedure _o__localtime32;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00440.o}
procedure _o__localtime32_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00441.o}
procedure _o__localtime64;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00442.o}
procedure _o__localtime64_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00443.o}
procedure _o__lock_file;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00444.o}
procedure _o__locking;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00445.o}
procedure _o__logb;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00446.o}
procedure _o__logbf;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00447.o}
procedure _o__lsearch;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00448.o}
procedure _o__lsearch_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00449.o}
procedure _o__lseek;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00450.o}
procedure _o__lseeki64;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00451.o}
procedure _o__ltoa;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00452.o}
procedure _o__ltoa_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00453.o}
procedure _o__ltow;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00454.o}
procedure _o__ltow_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00455.o}
procedure _o__makepath;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00456.o}
procedure _o__makepath_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00457.o}
procedure _o__malloc_base;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00458.o}
procedure _o__mbbtombc;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00459.o}
procedure _o__mbbtombc_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00460.o}
procedure _o__mbbtype;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00461.o}
procedure _o__mbbtype_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00462.o}
procedure _o__mbccpy;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00463.o}
procedure _o__mbccpy_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00464.o}
procedure _o__mbccpy_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00465.o}
procedure _o__mbccpy_s_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00466.o}
procedure _o__mbcjistojms;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00467.o}
procedure _o__mbcjistojms_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00468.o}
procedure _o__mbcjmstojis;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00469.o}
procedure _o__mbcjmstojis_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00470.o}
procedure _o__mbclen;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00471.o}
procedure _o__mbclen_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00472.o}
procedure _o__mbctohira;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00473.o}
procedure _o__mbctohira_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00474.o}
procedure _o__mbctokata;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00475.o}
procedure _o__mbctokata_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00476.o}
procedure _o__mbctolower;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00477.o}
procedure _o__mbctolower_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00478.o}
procedure _o__mbctombb;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00479.o}
procedure _o__mbctombb_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00480.o}
procedure _o__mbctoupper;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00481.o}
procedure _o__mbctoupper_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00482.o}
procedure _o__mblen_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00483.o}
procedure _o__mbsbtype;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00484.o}
procedure _o__mbsbtype_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00485.o}
procedure _o__mbscat_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00486.o}
procedure _o__mbscat_s_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00487.o}
procedure _o__mbschr;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00488.o}
procedure _o__mbschr_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00489.o}
procedure _o__mbscmp;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00490.o}
procedure _o__mbscmp_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00491.o}
procedure _o__mbscoll;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00492.o}
procedure _o__mbscoll_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00493.o}
procedure _o__mbscpy_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00494.o}
procedure _o__mbscpy_s_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00495.o}
procedure _o__mbscspn;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00496.o}
procedure _o__mbscspn_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00497.o}
procedure _o__mbsdec;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00498.o}
procedure _o__mbsdec_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00499.o}
procedure _o__mbsicmp;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00500.o}
procedure _o__mbsicmp_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00501.o}
procedure _o__mbsicoll;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00502.o}
procedure _o__mbsicoll_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00503.o}
procedure _o__mbsinc;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00504.o}
procedure _o__mbsinc_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00505.o}
procedure _o__mbslen;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00506.o}
procedure _o__mbslen_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00507.o}
procedure _o__mbslwr;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00508.o}
procedure _o__mbslwr_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00509.o}
procedure _o__mbslwr_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00510.o}
procedure _o__mbslwr_s_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00511.o}
procedure _o__mbsnbcat;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00512.o}
procedure _o__mbsnbcat_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00513.o}
procedure _o__mbsnbcat_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00514.o}
procedure _o__mbsnbcat_s_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00515.o}
procedure _o__mbsnbcmp;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00516.o}
procedure _o__mbsnbcmp_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00517.o}
procedure _o__mbsnbcnt;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00518.o}
procedure _o__mbsnbcnt_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00519.o}
procedure _o__mbsnbcoll;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00520.o}
procedure _o__mbsnbcoll_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00521.o}
procedure _o__mbsnbcpy;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00522.o}
procedure _o__mbsnbcpy_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00523.o}
procedure _o__mbsnbcpy_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00524.o}
procedure _o__mbsnbcpy_s_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00525.o}
procedure _o__mbsnbicmp;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00526.o}
procedure _o__mbsnbicmp_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00527.o}
procedure _o__mbsnbicoll;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00528.o}
procedure _o__mbsnbicoll_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00529.o}
procedure _o__mbsnbset;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00530.o}
procedure _o__mbsnbset_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00531.o}
procedure _o__mbsnbset_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00532.o}
procedure _o__mbsnbset_s_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00533.o}
procedure _o__mbsncat;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00534.o}
procedure _o__mbsncat_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00535.o}
procedure _o__mbsncat_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00536.o}
procedure _o__mbsncat_s_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00537.o}
procedure _o__mbsnccnt;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00538.o}
procedure _o__mbsnccnt_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00539.o}
procedure _o__mbsncmp;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00540.o}
procedure _o__mbsncmp_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00541.o}
procedure _o__mbsncoll;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00542.o}
procedure _o__mbsncoll_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00543.o}
procedure _o__mbsncpy;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00544.o}
procedure _o__mbsncpy_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00545.o}
procedure _o__mbsncpy_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00546.o}
procedure _o__mbsncpy_s_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00547.o}
procedure _o__mbsnextc;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00548.o}
procedure _o__mbsnextc_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00549.o}
procedure _o__mbsnicmp;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00550.o}
procedure _o__mbsnicmp_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00551.o}
procedure _o__mbsnicoll;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00552.o}
procedure _o__mbsnicoll_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00553.o}
procedure _o__mbsninc;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00554.o}
procedure _o__mbsninc_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00555.o}
procedure _o__mbsnlen;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00556.o}
procedure _o__mbsnlen_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00557.o}
procedure _o__mbsnset;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00558.o}
procedure _o__mbsnset_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00559.o}
procedure _o__mbsnset_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00560.o}
procedure _o__mbsnset_s_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00561.o}
procedure _o__mbspbrk;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00562.o}
procedure _o__mbspbrk_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00563.o}
procedure _o__mbsrchr;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00564.o}
procedure _o__mbsrchr_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00565.o}
procedure _o__mbsrev;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00566.o}
procedure _o__mbsrev_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00567.o}
procedure _o__mbsset;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00568.o}
procedure _o__mbsset_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00569.o}
procedure _o__mbsset_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00570.o}
procedure _o__mbsset_s_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00571.o}
procedure _o__mbsspn;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00572.o}
procedure _o__mbsspn_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00573.o}
procedure _o__mbsspnp;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00574.o}
procedure _o__mbsspnp_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00575.o}
procedure _o__mbsstr;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00576.o}
procedure _o__mbsstr_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00577.o}
procedure _o__mbstok;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00578.o}
procedure _o__mbstok_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00579.o}
procedure _o__mbstok_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00580.o}
procedure _o__mbstok_s_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00581.o}
procedure _o__mbstowcs_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00582.o}
procedure _o__mbstowcs_s_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00583.o}
procedure _o__mbstrlen;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00584.o}
procedure _o__mbstrlen_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00585.o}
procedure _o__mbstrnlen;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00586.o}
procedure _o__mbstrnlen_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00587.o}
procedure _o__mbsupr;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00588.o}
procedure _o__mbsupr_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00589.o}
procedure _o__mbsupr_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00590.o}
procedure _o__mbsupr_s_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00591.o}
procedure _o__mbtowc_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00592.o}
procedure _o__memicmp;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00593.o}
procedure _o__memicmp_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00594.o}
procedure _o__mkdir;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00595.o}
procedure _o__mkgmtime32;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00596.o}
procedure _o__mkgmtime64;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00597.o}
procedure _o__mktemp;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00598.o}
procedure _o__mktemp_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00599.o}
procedure _o__mktime32;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00600.o}
procedure _o__mktime64;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00601.o}
procedure _o__msize;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00602.o}
procedure _o__nextafter;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00603.o}
procedure _o__nextafterf;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00604.o}
procedure _o__open_osfhandle;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00605.o}
procedure _o__pclose;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00606.o}
procedure _o__pipe;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00607.o}
procedure _o__popen;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00608.o}
procedure _o__putc_nolock;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00609.o}
procedure _o__putch;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00610.o}
procedure _o__putch_nolock;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00611.o}
procedure _o__putenv;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00612.o}
procedure _o__putenv_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00613.o}
procedure _o__putw;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00614.o}
procedure _o__putwc_nolock;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00615.o}
procedure _o__putwch;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00616.o}
procedure _o__putwch_nolock;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00617.o}
procedure _o__putws;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00618.o}
procedure _o__read;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00619.o}
procedure _o__realloc_base;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00620.o}
procedure _o__recalloc;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00621.o}
procedure _o__register_onexit_function;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00622.o}
procedure _o__resetstkoflw;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00623.o}
procedure _o__rmdir;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00624.o}
procedure _o__rmtmp;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00625.o}
procedure _o__scalb;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00626.o}
procedure _o__scalbf;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00627.o}
procedure _o__searchenv;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00628.o}
procedure _o__searchenv_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00629.o}
procedure _o__set_abort_behavior;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00630.o}
procedure _o__set_doserrno;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00631.o}
procedure _o__set_errno;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00632.o}
procedure _o__set_invalid_parameter_handler;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00633.o}
procedure _o__set_new_handler;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00634.o}
procedure _o__set_new_mode;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00635.o}
procedure _o__set_thread_local_invalid_parameter_handler;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00636.o}
procedure _o__seterrormode;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00637.o}
procedure _o__setmbcp;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00638.o}
procedure _o__setmode;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00639.o}
procedure _o__setsystime;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00640.o}
procedure _o__sleep;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00641.o}
procedure _o__sopen;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00642.o}
procedure _o__sopen_dispatch;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00643.o}
procedure _o__sopen_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00644.o}
procedure _o__spawnv;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00645.o}
procedure _o__spawnve;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00646.o}
procedure _o__spawnvp;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00647.o}
procedure _o__spawnvpe;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00648.o}
procedure _o__splitpath;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00649.o}
procedure _o__splitpath_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00650.o}
procedure _o__stat32;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00651.o}
procedure _o__stat32i64;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00652.o}
procedure _o__stat64;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00653.o}
procedure _o__stat64i32;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00654.o}
procedure _o__strcoll_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00655.o}
procedure _o__strdate;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00656.o}
procedure _o__strdate_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00657.o}
procedure _o__strdup;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00658.o}
procedure _o__strerror;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00659.o}
procedure _o__strerror_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00660.o}
procedure _o__strftime_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00661.o}
procedure _o__stricmp;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00662.o}
procedure _o__stricmp_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00663.o}
procedure _o__stricoll;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00664.o}
procedure _o__stricoll_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00665.o}
procedure _o__strlwr;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00666.o}
procedure _o__strlwr_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00667.o}
procedure _o__strlwr_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00668.o}
procedure _o__strlwr_s_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00669.o}
procedure _o__strncoll;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00670.o}
procedure _o__strncoll_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00671.o}
procedure _o__strnicmp;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00672.o}
procedure _o__strnicmp_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00673.o}
procedure _o__strnicoll;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00674.o}
procedure _o__strnicoll_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00675.o}
procedure _o__strnset_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00676.o}
procedure _o__strset_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00677.o}
procedure _o__strtime;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00678.o}
procedure _o__strtime_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00679.o}
procedure _o__strtod_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00680.o}
procedure _o__strtof_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00681.o}
procedure _o__strtoi64;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00682.o}
procedure _o__strtoi64_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00683.o}
procedure _o__strtol_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00684.o}
procedure _o__strtold_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00685.o}
procedure _o__strtoll_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00686.o}
procedure _o__strtoui64;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00687.o}
procedure _o__strtoui64_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00688.o}
procedure _o__strtoul_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00689.o}
procedure _o__strtoull_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00690.o}
procedure _o__strupr;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00691.o}
procedure _o__strupr_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00692.o}
procedure _o__strupr_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00693.o}
procedure _o__strupr_s_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00694.o}
procedure _o__strxfrm_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00695.o}
procedure _o__swab;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00696.o}
procedure _o__tell;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00697.o}
procedure _o__telli64;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00698.o}
procedure _o__timespec32_get;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00699.o}
procedure _o__timespec64_get;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00700.o}
procedure _o__tolower;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00701.o}
procedure _o__tolower_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00702.o}
procedure _o__toupper;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00703.o}
procedure _o__toupper_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00704.o}
procedure _o__towlower_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00705.o}
procedure _o__towupper_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00706.o}
procedure _o__tzset;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00707.o}
procedure _o__ui64toa;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00708.o}
procedure _o__ui64toa_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00709.o}
procedure _o__ui64tow;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00710.o}
procedure _o__ui64tow_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00711.o}
procedure _o__ultoa;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00712.o}
procedure _o__ultoa_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00713.o}
procedure _o__ultow;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00714.o}
procedure _o__ultow_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00715.o}
procedure _o__umask;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00716.o}
procedure _o__umask_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00717.o}
procedure _o__ungetc_nolock;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00718.o}
procedure _o__ungetch;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00719.o}
procedure _o__ungetch_nolock;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00720.o}
procedure _o__ungetwc_nolock;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00721.o}
procedure _o__ungetwch;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00722.o}
procedure _o__ungetwch_nolock;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00723.o}
procedure _o__unlink;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00724.o}
procedure _o__unloaddll;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00725.o}
procedure _o__unlock_file;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00726.o}
procedure _o__utime32;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00727.o}
procedure _o__utime64;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00728.o}
procedure _o__waccess;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00729.o}
procedure _o__waccess_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00730.o}
procedure _o__wasctime;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00731.o}
procedure _o__wasctime_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00732.o}
procedure _o__wchdir;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00733.o}
procedure _o__wchmod;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00734.o}
procedure _o__wcreat;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00735.o}
procedure _o__wcreate_locale;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00736.o}
procedure _o__wcscoll_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00737.o}
procedure _o__wcsdup;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00738.o}
procedure _o__wcserror;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00739.o}
procedure _o__wcserror_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00740.o}
procedure _o__wcsftime_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00741.o}
procedure _o__wcsicmp;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00742.o}
procedure _o__wcsicmp_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00743.o}
procedure _o__wcsicoll;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00744.o}
procedure _o__wcsicoll_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00745.o}
procedure _o__wcslwr;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00746.o}
procedure _o__wcslwr_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00747.o}
procedure _o__wcslwr_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00748.o}
procedure _o__wcslwr_s_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00749.o}
procedure _o__wcsncoll;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00750.o}
procedure _o__wcsncoll_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00751.o}
procedure _o__wcsnicmp;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00752.o}
procedure _o__wcsnicmp_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00753.o}
procedure _o__wcsnicoll;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00754.o}
procedure _o__wcsnicoll_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00755.o}
procedure _o__wcsnset;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00756.o}
procedure _o__wcsnset_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00757.o}
procedure _o__wcsset;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00758.o}
procedure _o__wcsset_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00759.o}
procedure _o__wcstod_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00760.o}
procedure _o__wcstof_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00761.o}
procedure _o__wcstoi64;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00762.o}
procedure _o__wcstoi64_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00763.o}
procedure _o__wcstol_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00764.o}
procedure _o__wcstold_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00765.o}
procedure _o__wcstoll_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00766.o}
procedure _o__wcstombs_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00767.o}
procedure _o__wcstombs_s_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00768.o}
procedure _o__wcstoui64;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00769.o}
procedure _o__wcstoui64_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00770.o}
procedure _o__wcstoul_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00771.o}
procedure _o__wcstoull_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00772.o}
procedure _o__wcsupr;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00773.o}
procedure _o__wcsupr_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00774.o}
procedure _o__wcsupr_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00775.o}
procedure _o__wcsupr_s_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00776.o}
procedure _o__wcsxfrm_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00777.o}
procedure _o__wctime32;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00778.o}
procedure _o__wctime32_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00779.o}
procedure _o__wctime64;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00780.o}
procedure _o__wctime64_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00781.o}
procedure _o__wctomb_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00782.o}
procedure _o__wctomb_s_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00783.o}
procedure _o__wdupenv_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00784.o}
procedure _o__wexecv;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00785.o}
procedure _o__wexecve;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00786.o}
procedure _o__wexecvp;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00787.o}
procedure _o__wexecvpe;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00788.o}
procedure _o__wfdopen;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00789.o}
procedure _o__wfindfirst32;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00790.o}
procedure _o__wfindfirst32i64;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00791.o}
procedure _o__wfindfirst64;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00792.o}
procedure _o__wfindfirst64i32;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00793.o}
procedure _o__wfindnext32;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00794.o}
procedure _o__wfindnext32i64;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00795.o}
procedure _o__wfindnext64;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00796.o}
procedure _o__wfindnext64i32;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00797.o}
procedure _o__wfopen;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00798.o}
procedure _o__wfopen_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00799.o}
procedure _o__wfreopen;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00800.o}
procedure _o__wfreopen_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00801.o}
procedure _o__wfsopen;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00802.o}
procedure _o__wfullpath;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00803.o}
procedure _o__wgetcwd;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00804.o}
procedure _o__wgetdcwd;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00805.o}
procedure _o__wgetenv;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00806.o}
procedure _o__wgetenv_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00807.o}
procedure _o__wmakepath;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00808.o}
procedure _o__wmakepath_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00809.o}
procedure _o__wmkdir;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00810.o}
procedure _o__wmktemp;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00811.o}
procedure _o__wmktemp_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00812.o}
procedure _o__wperror;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00813.o}
procedure _o__wpopen;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00814.o}
procedure _o__wputenv;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00815.o}
procedure _o__wputenv_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00816.o}
procedure _o__wremove;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00817.o}
procedure _o__wrename;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00818.o}
procedure _o__write;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00819.o}
procedure _o__wrmdir;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00820.o}
procedure _o__wsearchenv;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00821.o}
procedure _o__wsearchenv_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00822.o}
procedure _o__wsetlocale;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00823.o}
procedure _o__wsopen_dispatch;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00824.o}
procedure _o__wsopen_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00825.o}
procedure _o__wspawnv;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00826.o}
procedure _o__wspawnve;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00827.o}
procedure _o__wspawnvp;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00828.o}
procedure _o__wspawnvpe;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00829.o}
procedure _o__wsplitpath;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00830.o}
procedure _o__wsplitpath_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00831.o}
procedure _o__wstat32;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00832.o}
procedure _o__wstat32i64;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00833.o}
procedure _o__wstat64;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00834.o}
procedure _o__wstat64i32;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00835.o}
procedure _o__wstrdate;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00836.o}
procedure _o__wstrdate_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00837.o}
procedure _o__wstrtime;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00838.o}
procedure _o__wstrtime_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00839.o}
procedure _o__wsystem;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00840.o}
procedure _o__wtmpnam_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00841.o}
procedure _o__wtof;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00842.o}
procedure _o__wtof_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00843.o}
procedure _o__wtoi;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00844.o}
procedure _o__wtoi64;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00845.o}
procedure _o__wtoi64_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00846.o}
procedure _o__wtoi_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00847.o}
procedure _o__wtol;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00848.o}
procedure _o__wtol_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00849.o}
procedure _o__wtoll;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00850.o}
procedure _o__wtoll_l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00851.o}
procedure _o__wunlink;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00852.o}
procedure _o__wutime32;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00853.o}
procedure _o__wutime64;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00854.o}
procedure _o__y0;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00855.o}
procedure _o__y1;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00856.o}
procedure _o__yn;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00857.o}
procedure _o_abort;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00858.o}
procedure _o_acos;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00859.o}
procedure _o_acosf;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00860.o}
procedure _o_acosh;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00861.o}
procedure _o_acoshf;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00862.o}
procedure _o_acoshl;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00863.o}
procedure _o_asctime;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00864.o}
procedure _o_asctime_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00865.o}
procedure _o_asin;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00866.o}
procedure _o_asinf;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00867.o}
procedure _o_asinh;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00868.o}
procedure _o_asinhf;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00869.o}
procedure _o_asinhl;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00870.o}
procedure _o_atan;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00871.o}
procedure _o_atan2;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00872.o}
procedure _o_atan2f;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00873.o}
procedure _o_atanf;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00874.o}
procedure _o_atanh;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00875.o}
procedure _o_atanhf;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00876.o}
procedure _o_atanhl;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00877.o}
procedure _o_atof;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00878.o}
procedure _o_atoi;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00879.o}
procedure _o_atol;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00880.o}
procedure _o_atoll;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00881.o}
procedure _o_bsearch;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00882.o}
procedure _o_bsearch_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00883.o}
procedure _o_btowc;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00884.o}
procedure _o_calloc;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00885.o}
procedure _o_cbrt;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00886.o}
procedure _o_cbrtf;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00887.o}
procedure _o_ceil;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00888.o}
procedure _o_ceilf;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00889.o}
procedure _o_clearerr;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00890.o}
procedure _o_clearerr_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00891.o}
procedure _o_cos;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00892.o}
procedure _o_cosf;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00893.o}
procedure _o_cosh;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00894.o}
procedure _o_coshf;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00895.o}
procedure _o_erf;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00896.o}
procedure _o_erfc;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00897.o}
procedure _o_erfcf;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00898.o}
procedure _o_erfcl;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00899.o}
procedure _o_erff;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00900.o}
procedure _o_erfl;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00901.o}
procedure _o_exp;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00902.o}
procedure _o_exp2;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00903.o}
procedure _o_exp2f;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00904.o}
procedure _o_exp2l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00905.o}
procedure _o_expf;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00906.o}
procedure _o_fabs;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00907.o}
procedure _o_fclose;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00908.o}
procedure _o_feof;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00909.o}
procedure _o_ferror;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00910.o}
procedure _o_fflush;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00911.o}
procedure _o_fgetc;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00912.o}
procedure _o_fgetpos;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00913.o}
procedure _o_fgets;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00914.o}
procedure _o_fgetwc;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00915.o}
procedure _o_fgetws;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00916.o}
procedure _o_floor;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00917.o}
procedure _o_floorf;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00918.o}
procedure _o_fma;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00919.o}
procedure _o_fmaf;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00920.o}
procedure _o_fmal;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00921.o}
procedure _o_fmod;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00922.o}
procedure _o_fmodf;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00923.o}
procedure _o_fopen;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00924.o}
procedure _o_fopen_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00925.o}
procedure _o_fputc;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00926.o}
procedure _o_fputs;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00927.o}
procedure _o_fputwc;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00928.o}
procedure _o_fputws;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00929.o}
procedure _o_fread;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00930.o}
procedure _o_fread_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00931.o}
procedure _o_free;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00932.o}
procedure _o_freopen;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00933.o}
procedure _o_freopen_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00934.o}
procedure _o_frexp;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00935.o}
procedure _o_fseek;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00936.o}
procedure _o_fsetpos;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00937.o}
procedure _o_ftell;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00938.o}
procedure _o_fwrite;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00939.o}
procedure _o_getc;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00940.o}
procedure _o_getchar;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00941.o}
procedure _o_getenv;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00942.o}
procedure _o_getenv_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00943.o}
procedure _o_gets;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00944.o}
procedure _o_gets_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00945.o}
procedure _o_getwc;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00946.o}
procedure _o_getwchar;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00947.o}
procedure _o_hypot;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00948.o}
procedure _o_is_wctype;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00949.o}
procedure _o_isalnum;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00950.o}
procedure _o_isalpha;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00951.o}
procedure _o_isblank;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00952.o}
procedure _o_iscntrl;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00953.o}
procedure _o_isdigit;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00954.o}
procedure _o_isgraph;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00955.o}
procedure _o_isleadbyte;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00956.o}
procedure _o_islower;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00957.o}
procedure _o_isprint;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00958.o}
procedure _o_ispunct;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00959.o}
procedure _o_isspace;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00960.o}
procedure _o_isupper;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00961.o}
procedure _o_iswalnum;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00962.o}
procedure _o_iswalpha;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00963.o}
procedure _o_iswascii;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00964.o}
procedure _o_iswblank;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00965.o}
procedure _o_iswcntrl;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00966.o}
procedure _o_iswctype;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00967.o}
procedure _o_iswdigit;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00968.o}
procedure _o_iswgraph;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00969.o}
procedure _o_iswlower;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00970.o}
procedure _o_iswprint;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00971.o}
procedure _o_iswpunct;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00972.o}
procedure _o_iswspace;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00973.o}
procedure _o_iswupper;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00974.o}
procedure _o_iswxdigit;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00975.o}
procedure _o_isxdigit;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00976.o}
procedure _o_ldexp;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00977.o}
procedure _o_lgamma;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00978.o}
procedure _o_lgammaf;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00979.o}
procedure _o_lgammal;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00980.o}
procedure _o_llrint;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00981.o}
procedure _o_llrintf;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00982.o}
procedure _o_llrintl;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00983.o}
procedure _o_llround;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00984.o}
procedure _o_llroundf;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00985.o}
procedure _o_llroundl;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00986.o}
procedure _o_localeconv;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00987.o}
procedure _o_log;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00988.o}
procedure _o_log10;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00989.o}
procedure _o_log10f;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00990.o}
procedure _o_log1p;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00991.o}
procedure _o_log1pf;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00992.o}
procedure _o_log1pl;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00993.o}
procedure _o_log2;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00994.o}
procedure _o_log2f;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00995.o}
procedure _o_log2l;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00996.o}
procedure _o_logb;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00997.o}
procedure _o_logbf;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00998.o}
procedure _o_logbl;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s00999.o}
procedure _o_logf;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01000.o}
procedure _o_lrint;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01001.o}
procedure _o_lrintf;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01002.o}
procedure _o_lrintl;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01003.o}
procedure _o_lround;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01004.o}
procedure _o_lroundf;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01005.o}
procedure _o_lroundl;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01006.o}
procedure _o_malloc;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01007.o}
procedure _o_mblen;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01008.o}
procedure _o_mbrlen;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01009.o}
procedure _o_mbrtoc16;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01010.o}
procedure _o_mbrtoc32;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01011.o}
procedure _o_mbrtowc;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01012.o}
procedure _o_mbsrtowcs;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01013.o}
procedure _o_mbsrtowcs_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01014.o}
procedure _o_mbstowcs;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01015.o}
procedure _o_mbstowcs_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01016.o}
procedure _o_mbtowc;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01017.o}
procedure _o_memset;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01018.o}
procedure _o_modf;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01019.o}
procedure _o_modff;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01020.o}
procedure _o_nan;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01021.o}
procedure _o_nanf;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01022.o}
procedure _o_nanl;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01023.o}
procedure _o_nearbyint;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01024.o}
procedure _o_nearbyintf;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01025.o}
procedure _o_nearbyintl;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01026.o}
procedure _o_nextafter;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01027.o}
procedure _o_nextafterf;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01028.o}
procedure _o_nextafterl;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01029.o}
procedure _o_nexttoward;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01030.o}
procedure _o_nexttowardf;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01031.o}
procedure _o_nexttowardl;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01032.o}
procedure _o_pow;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01033.o}
procedure _o_powf;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01034.o}
procedure _o_putc;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01035.o}
procedure _o_putchar;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01036.o}
procedure _o_puts;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01037.o}
procedure _o_putwc;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01038.o}
procedure _o_putwchar;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01039.o}
procedure _o_qsort;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01040.o}
procedure _o_qsort_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01041.o}
procedure _o_raise;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01042.o}
procedure _o_rand;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01043.o}
procedure _o_rand_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01044.o}
procedure _o_realloc;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01045.o}
procedure _o_remainder;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01046.o}
procedure _o_remainderf;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01047.o}
procedure _o_remainderl;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01048.o}
procedure _o_remove;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01049.o}
procedure _o_remquo;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01050.o}
procedure _o_remquof;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01051.o}
procedure _o_remquol;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01052.o}
procedure _o_rewind;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01053.o}
procedure _o_rint;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01054.o}
procedure _o_rintf;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01055.o}
procedure _o_rintl;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01056.o}
procedure _o_round;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01057.o}
procedure _o_roundf;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01058.o}
procedure _o_roundl;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01059.o}
procedure _o_scalbln;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01060.o}
procedure _o_scalblnf;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01061.o}
procedure _o_scalblnl;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01062.o}
procedure _o_scalbn;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01063.o}
procedure _o_scalbnf;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01064.o}
procedure _o_scalbnl;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01065.o}
procedure _o_set_terminate;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01066.o}
procedure _o_setbuf;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01067.o}
procedure _o_setvbuf;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01068.o}
procedure _o_sin;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01069.o}
procedure _o_sinf;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01070.o}
procedure _o_sinh;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01071.o}
procedure _o_sinhf;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01072.o}
procedure _o_sqrt;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01073.o}
procedure _o_sqrtf;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01074.o}
procedure _o_srand;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01075.o}
procedure _o_strcat_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01076.o}
procedure _o_strcoll;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01077.o}
procedure _o_strcpy_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01078.o}
procedure _o_strerror;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01079.o}
procedure _o_strerror_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01080.o}
procedure _o_strftime;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01081.o}
procedure _o_strncat_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01082.o}
procedure _o_strncpy_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01083.o}
procedure _o_strtod;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01084.o}
procedure _o_strtof;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01085.o}
procedure _o_strtok;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01086.o}
procedure _o_strtok_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01087.o}
procedure _o_strtol;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01088.o}
procedure _o_strtold;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01089.o}
procedure _o_strtoll;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01090.o}
procedure _o_strtoul;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01091.o}
procedure _o_strtoull;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01092.o}
procedure _o_system;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01093.o}
procedure _o_tan;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01094.o}
procedure _o_tanf;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01095.o}
procedure _o_tanh;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01096.o}
procedure _o_tanhf;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01097.o}
procedure _o_terminate;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01098.o}
procedure _o_tgamma;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01099.o}
procedure _o_tgammaf;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01100.o}
procedure _o_tgammal;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01101.o}
procedure _o_tmpfile_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01102.o}
procedure _o_tmpnam_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01103.o}
procedure _o_tolower;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01104.o}
procedure _o_toupper;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01105.o}
procedure _o_towlower;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01106.o}
procedure _o_towupper;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01107.o}
procedure _o_ungetc;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01108.o}
procedure _o_ungetwc;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01109.o}
procedure _o_wcrtomb;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01110.o}
procedure _o_wcrtomb_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01111.o}
procedure _o_wcscat_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01112.o}
procedure _o_wcscoll;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01113.o}
procedure _o_wcscpy;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01114.o}
procedure _o_wcscpy_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01115.o}
procedure _o_wcsftime;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01116.o}
procedure _o_wcsncat_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01117.o}
procedure _o_wcsncpy_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01118.o}
procedure _o_wcsrtombs;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01119.o}
procedure _o_wcsrtombs_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01120.o}
procedure _o_wcstod;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01121.o}
procedure _o_wcstof;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01122.o}
procedure _o_wcstok;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01123.o}
procedure _o_wcstok_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01124.o}
procedure _o_wcstol;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01125.o}
procedure _o_wcstold;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01126.o}
procedure _o_wcstoll;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01127.o}
procedure _o_wcstombs;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01128.o}
procedure _o_wcstombs_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01129.o}
procedure _o_wcstoul;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01130.o}
procedure _o_wcstoull;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01131.o}
procedure _o_wctob;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01132.o}
procedure _o_wctomb;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01133.o}
procedure _o_wctomb_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01134.o}
procedure _o_wmemcpy_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01135.o}
procedure _o_wmemmove_s;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01136.o}
procedure _purecall;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01137.o}
procedure _set_purecall_handler;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01138.o}
procedure _set_se_translator;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01139.o}
procedure longjmp;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01140.o}
procedure memchr;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01141.o}
procedure memcmp;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01142.o}
procedure memcpy;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01143.o}
procedure memmove;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01144.o}
procedure set_unexpected;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01145.o}
procedure setjmp;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01146.o}
procedure strchr;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01147.o}
procedure strrchr;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01148.o}
procedure strstr;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01149.o}
procedure unexpected;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01150.o}
procedure wcschr;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01151.o}
procedure wcsrchr;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01152.o}
procedure wcsstr;external;
{$L x64/libapi-ms-win-crt-private-l1-1-0s01153.o}
procedure __p__mbcasemap;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00000.o}
procedure __p__mbctype;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00001.o}
procedure _ismbbalnum;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00002.o}
procedure _ismbbalnum_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00003.o}
procedure _ismbbalpha;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00004.o}
procedure _ismbbalpha_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00005.o}
procedure _ismbbblank;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00006.o}
procedure _ismbbblank_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00007.o}
procedure _ismbbgraph;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00008.o}
procedure _ismbbgraph_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00009.o}
procedure _ismbbkalnum;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00010.o}
procedure _ismbbkalnum_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00011.o}
procedure _ismbbkana;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00012.o}
procedure _ismbbkana_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00013.o}
procedure _ismbbkprint;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00014.o}
procedure _ismbbkprint_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00015.o}
procedure _ismbbkpunct;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00016.o}
procedure _ismbbkpunct_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00017.o}
procedure _ismbblead;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00018.o}
procedure _ismbblead_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00019.o}
procedure _ismbbprint;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00020.o}
procedure _ismbbprint_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00021.o}
procedure _ismbbpunct;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00022.o}
procedure _ismbbpunct_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00023.o}
procedure _ismbbtrail;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00024.o}
procedure _ismbbtrail_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00025.o}
procedure _ismbcalnum;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00026.o}
procedure _ismbcalnum_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00027.o}
procedure _ismbcalpha;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00028.o}
procedure _ismbcalpha_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00029.o}
procedure _ismbcblank;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00030.o}
procedure _ismbcblank_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00031.o}
procedure _ismbcdigit;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00032.o}
procedure _ismbcdigit_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00033.o}
procedure _ismbcgraph;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00034.o}
procedure _ismbcgraph_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00035.o}
procedure _ismbchira;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00036.o}
procedure _ismbchira_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00037.o}
procedure _ismbckata;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00038.o}
procedure _ismbckata_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00039.o}
procedure _ismbcl0;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00040.o}
procedure _ismbcl0_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00041.o}
procedure _ismbcl1;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00042.o}
procedure _ismbcl1_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00043.o}
procedure _ismbcl2;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00044.o}
procedure _ismbcl2_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00045.o}
procedure _ismbclegal;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00046.o}
procedure _ismbclegal_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00047.o}
procedure _ismbclower;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00048.o}
procedure _ismbclower_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00049.o}
procedure _ismbcprint;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00050.o}
procedure _ismbcprint_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00051.o}
procedure _ismbcpunct;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00052.o}
procedure _ismbcpunct_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00053.o}
procedure _ismbcspace;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00054.o}
procedure _ismbcspace_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00055.o}
procedure _ismbcsymbol;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00056.o}
procedure _ismbcsymbol_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00057.o}
procedure _ismbcupper;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00058.o}
procedure _ismbcupper_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00059.o}
procedure _ismbslead;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00060.o}
procedure _ismbslead_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00061.o}
procedure _ismbstrail;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00062.o}
procedure _ismbstrail_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00063.o}
procedure _mbbtombc;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00064.o}
procedure _mbbtombc_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00065.o}
procedure _mbbtype;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00066.o}
procedure _mbbtype_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00067.o}
procedure _mbccpy;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00069.o}
procedure _mbccpy_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00070.o}
procedure _mbccpy_s;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00071.o}
procedure _mbccpy_s_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00072.o}
procedure _mbcjistojms;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00073.o}
procedure _mbcjistojms_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00074.o}
procedure _mbcjmstojis;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00075.o}
procedure _mbcjmstojis_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00076.o}
procedure _mbclen;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00077.o}
procedure _mbclen_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00078.o}
procedure _mbctohira;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00079.o}
procedure _mbctohira_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00080.o}
procedure _mbctokata;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00081.o}
procedure _mbctokata_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00082.o}
procedure _mbctolower;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00083.o}
procedure _mbctolower_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00084.o}
procedure _mbctombb;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00085.o}
procedure _mbctombb_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00086.o}
procedure _mbctoupper;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00087.o}
procedure _mbctoupper_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00088.o}
procedure _mblen_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00089.o}
procedure _mbsbtype;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00090.o}
procedure _mbsbtype_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00091.o}
procedure _mbscat_s;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00092.o}
procedure _mbscat_s_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00093.o}
procedure _mbschr;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00094.o}
procedure _mbschr_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00095.o}
procedure _mbscmp;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00096.o}
procedure _mbscmp_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00097.o}
procedure _mbscoll;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00098.o}
procedure _mbscoll_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00099.o}
procedure _mbscpy_s;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00100.o}
procedure _mbscpy_s_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00101.o}
procedure _mbscspn;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00102.o}
procedure _mbscspn_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00103.o}
procedure _mbsdec;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00104.o}
procedure _mbsdec_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00105.o}
procedure _mbsdup;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00106.o}
procedure _mbsicmp;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00107.o}
procedure _mbsicmp_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00108.o}
procedure _mbsicoll;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00109.o}
procedure _mbsicoll_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00110.o}
procedure _mbsinc;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00111.o}
procedure _mbsinc_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00112.o}
procedure _mbslen;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00113.o}
procedure _mbslen_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00114.o}
procedure _mbslwr;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00115.o}
procedure _mbslwr_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00116.o}
procedure _mbslwr_s;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00117.o}
procedure _mbslwr_s_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00118.o}
procedure _mbsnbcat;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00119.o}
procedure _mbsnbcat_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00120.o}
procedure _mbsnbcat_s;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00121.o}
procedure _mbsnbcat_s_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00122.o}
procedure _mbsnbcmp;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00123.o}
procedure _mbsnbcmp_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00124.o}
procedure _mbsnbcnt;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00125.o}
procedure _mbsnbcnt_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00126.o}
procedure _mbsnbcoll;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00127.o}
procedure _mbsnbcoll_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00128.o}
procedure _mbsnbcpy;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00129.o}
procedure _mbsnbcpy_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00130.o}
procedure _mbsnbcpy_s;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00131.o}
procedure _mbsnbcpy_s_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00132.o}
procedure _mbsnbicmp;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00133.o}
procedure _mbsnbicmp_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00134.o}
procedure _mbsnbicoll;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00135.o}
procedure _mbsnbicoll_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00136.o}
procedure _mbsnbset;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00137.o}
procedure _mbsnbset_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00138.o}
procedure _mbsnbset_s;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00139.o}
procedure _mbsnbset_s_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00140.o}
procedure _mbsncat;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00141.o}
procedure _mbsncat_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00142.o}
procedure _mbsncat_s;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00143.o}
procedure _mbsncat_s_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00144.o}
procedure _mbsnccnt;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00145.o}
procedure _mbsnccnt_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00146.o}
procedure _mbsncmp;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00147.o}
procedure _mbsncmp_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00148.o}
procedure _mbsncoll;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00149.o}
procedure _mbsncoll_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00150.o}
procedure _mbsncpy;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00151.o}
procedure _mbsncpy_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00152.o}
procedure _mbsncpy_s;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00153.o}
procedure _mbsncpy_s_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00154.o}
procedure _mbsnextc;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00155.o}
procedure _mbsnextc_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00156.o}
procedure _mbsnicmp;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00157.o}
procedure _mbsnicmp_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00158.o}
procedure _mbsnicoll;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00159.o}
procedure _mbsnicoll_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00160.o}
procedure _mbsninc;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00161.o}
procedure _mbsninc_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00162.o}
procedure _mbsnlen;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00163.o}
procedure _mbsnlen_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00164.o}
procedure _mbsnset;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00165.o}
procedure _mbsnset_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00166.o}
procedure _mbsnset_s;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00167.o}
procedure _mbsnset_s_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00168.o}
procedure _mbspbrk;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00169.o}
procedure _mbspbrk_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00170.o}
procedure _mbsrchr;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00171.o}
procedure _mbsrchr_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00172.o}
procedure _mbsrev;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00173.o}
procedure _mbsrev_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00174.o}
procedure _mbsset;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00175.o}
procedure _mbsset_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00176.o}
procedure _mbsset_s;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00177.o}
procedure _mbsset_s_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00178.o}
procedure _mbsspn;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00179.o}
procedure _mbsspn_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00180.o}
procedure _mbsspnp;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00181.o}
procedure _mbsspnp_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00182.o}
procedure _mbsstr;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00183.o}
procedure _mbsstr_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00184.o}
procedure _mbstok;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00185.o}
procedure _mbstok_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00186.o}
procedure _mbstok_s;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00187.o}
procedure _mbstok_s_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00188.o}
procedure _mbstowcs_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00189.o}
procedure _mbstowcs_s_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00190.o}
procedure _mbstrlen;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00191.o}
procedure _mbstrlen_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00192.o}
procedure _mbstrnlen;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00193.o}
procedure _mbstrnlen_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00194.o}
procedure _mbsupr;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00195.o}
procedure _mbsupr_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00196.o}
procedure _mbsupr_s;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00197.o}
procedure _mbsupr_s_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00198.o}
procedure _mbtowc_l;external;
{$L x64/libapi-ms-win-crt-multibyte-l1-1-0s00199.o}
procedure _Cbuild;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00000.o}
procedure _Cmulcc;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00001.o}
procedure _Cmulcr;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00002.o}
procedure _FCbuild;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00003.o}
procedure _FCmulcc;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00004.o}
procedure _FCmulcr;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00005.o}
procedure _LCbuild;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00006.o}
procedure _LCmulcc;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00007.o}
procedure _LCmulcr;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00008.o}
procedure __setusermatherr;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00009.o}
procedure _chgsign;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00011.o}
procedure _chgsignf;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00012.o}
procedure _copysign;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00013.o}
procedure _copysignf;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00014.o}
procedure _d_int;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00015.o}
procedure _dclass;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00016.o}
procedure chgsign;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00017.o}
procedure _dexp;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00018.o}
procedure _dlog;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00019.o}
procedure _dnorm;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00020.o}
procedure _dpcomp;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00021.o}
procedure _dpoly;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00022.o}
procedure _dscale;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00023.o}
procedure _dsign;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00024.o}
procedure _dsin;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00025.o}
procedure _dtest;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00026.o}
procedure _dunscale;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00027.o}
procedure _except1;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00028.o}
procedure _fd_int;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00029.o}
procedure _fdclass;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00030.o}
procedure _fdexp;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00031.o}
procedure _fdlog;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00032.o}
procedure _fdnorm;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00033.o}
procedure _fdopen;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00034.o}
procedure _fdpcomp;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00035.o}
procedure fdopen;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00036.o}
procedure _fdpoly;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00037.o}
procedure _fdscale;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00038.o}
procedure _fdsign;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00039.o}
procedure _fdsin;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00040.o}
procedure _fdtest;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00041.o}
procedure _fdunscale;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00042.o}
procedure _finite;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00043.o}
procedure _finitef;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00044.o}
procedure finite;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00045.o}
procedure _fpclass;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00046.o}
procedure fpclass;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00047.o}
procedure _fpclassf;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00048.o}
procedure _get_FMA3_enable;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00049.o}
procedure _hypot;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00050.o}
procedure _hypotf;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00051.o}
procedure _isnan;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00052.o}
procedure _isnanf;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00053.o}
procedure hypot;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00054.o}
procedure _j0;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00055.o}
procedure j0;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00056.o}
procedure _j1;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00057.o}
procedure jn;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00058.o}
procedure _jn;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00059.o}
procedure _ld_int;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00060.o}
procedure _ldclass;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00061.o}
procedure _ldexp;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00062.o}
procedure _ldlog;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00063.o}
procedure j1;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00064.o}
procedure _ldpcomp;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00065.o}
procedure _ldpoly;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00066.o}
procedure _ldscale;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00067.o}
procedure _ldsign;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00068.o}
procedure _ldsin;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00069.o}
procedure _ldtest;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00070.o}
procedure _ldunscale;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00071.o}
procedure _logb;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00072.o}
procedure _logbf;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00073.o}
procedure _nextafter;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00074.o}
procedure nextafter;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00075.o}
procedure _nextafterf;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00076.o}
procedure _scalb;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00077.o}
procedure _scalbf;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00078.o}
procedure _set_FMA3_enable;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00079.o}
procedure _y0;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00080.o}
procedure y1;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00081.o}
procedure _y1;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00082.o}
procedure y0;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00083.o}
procedure _yn;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00084.o}
procedure yn;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00085.o}
procedure acos;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00086.o}
procedure acosh;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00088.o}
procedure acoshf;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00089.o}
procedure asin;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00091.o}
procedure asinh;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00093.o}
procedure asinhf;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00094.o}
procedure atan;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00096.o}
procedure atanh;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00100.o}
procedure atanhf;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00101.o}
procedure cabs;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00103.o}
procedure cabsf;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00104.o}
procedure cabsl;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00105.o}
procedure cacos;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00106.o}
procedure cacosf;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00107.o}
procedure cacosh;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00108.o}
procedure cacoshf;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00109.o}
procedure cacoshl;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00110.o}
procedure cacosl;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00111.o}
procedure carg;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00112.o}
procedure cargf;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00113.o}
procedure cargl;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00114.o}
procedure casin;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00115.o}
procedure casinf;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00116.o}
procedure casinh;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00117.o}
procedure casinhf;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00118.o}
procedure casinhl;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00119.o}
procedure casinl;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00120.o}
procedure catan;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00121.o}
procedure catanf;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00122.o}
procedure catanh;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00123.o}
procedure catanhf;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00124.o}
procedure catanhl;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00125.o}
procedure catanl;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00126.o}
procedure cbrt;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00127.o}
procedure cbrtf;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00128.o}
procedure ccos;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00130.o}
procedure ccosf;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00131.o}
procedure ccosh;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00132.o}
procedure ccoshf;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00133.o}
procedure ccoshl;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00134.o}
procedure ccosl;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00135.o}
procedure cexp;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00138.o}
procedure cexpf;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00139.o}
procedure cexpl;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00140.o}
procedure cimag;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00141.o}
procedure cimagf;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00142.o}
procedure cimagl;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00143.o}
procedure clog;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00144.o}
procedure clog10;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00145.o}
procedure clog10f;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00146.o}
procedure clog10l;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00147.o}
procedure clogf;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00148.o}
procedure clogl;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00149.o}
procedure conj;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00150.o}
procedure conjf;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00151.o}
procedure conjl;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00152.o}
procedure copysign;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00153.o}
procedure copysignf;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00154.o}
procedure cosh;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00158.o}
procedure cpow;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00160.o}
procedure cpowf;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00161.o}
procedure cpowl;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00162.o}
procedure cproj;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00163.o}
procedure cprojf;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00164.o}
procedure cprojl;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00165.o}
procedure creal;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00166.o}
procedure crealf;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00167.o}
procedure creall;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00168.o}
procedure csin;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00169.o}
procedure csinf;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00170.o}
procedure csinh;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00171.o}
procedure csinhf;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00172.o}
procedure csinhl;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00173.o}
procedure csinl;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00174.o}
procedure csqrt;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00175.o}
procedure csqrtf;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00176.o}
procedure csqrtl;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00177.o}
procedure ctan;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00178.o}
procedure ctanf;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00179.o}
procedure ctanh;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00180.o}
procedure ctanhf;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00181.o}
procedure ctanhl;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00182.o}
procedure ctanl;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00183.o}
procedure erf;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00184.o}
procedure erfc;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00185.o}
procedure erfcf;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00186.o}
procedure erff;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00188.o}
procedure exp2;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00191.o}
procedure exp2f;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00192.o}
procedure expm1;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00195.o}
procedure expm1f;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00196.o}
procedure fdim;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00199.o}
procedure fdimf;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00200.o}
procedure fma;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00204.o}
procedure fmaf;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00205.o}
procedure fmax;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00207.o}
procedure fmaxf;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00208.o}
procedure fmin;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00210.o}
procedure fminf;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00211.o}
procedure frexp;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00215.o}
procedure ilogb;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00217.o}
procedure ilogbf;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00218.o}
procedure llrint;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00224.o}
procedure llrintf;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00225.o}
procedure llround;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00227.o}
procedure llroundf;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00228.o}
procedure log10;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00231.o}
procedure log1p;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00233.o}
procedure log1pf;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00234.o}
procedure log2;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00236.o}
procedure log2f;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00237.o}
procedure logb;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00239.o}
procedure logbf;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00240.o}
procedure lrint;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00243.o}
procedure lrintf;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00244.o}
procedure lround;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00246.o}
procedure lroundf;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00247.o}
procedure nan;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00251.o}
procedure nanf;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00252.o}
procedure nearbyint;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00254.o}
procedure nearbyintf;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00255.o}
procedure nextafterf;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00258.o}
procedure norm;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00263.o}
procedure normf;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00264.o}
procedure norml;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00265.o}
procedure remainder;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00268.o}
procedure remainderf;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00269.o}
procedure remquo;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00271.o}
procedure remquof;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00272.o}
procedure rint;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00274.o}
procedure rintf;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00275.o}
procedure round;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00277.o}
procedure roundf;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00278.o}
procedure scalbln;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00280.o}
procedure scalblnf;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00281.o}
procedure scalbn;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00283.o}
procedure scalbnf;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00284.o}
procedure sinh;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00288.o}
procedure tan;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00292.o}
procedure tanh;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00294.o}
procedure tanhf;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00295.o}
procedure tgamma;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00296.o}
procedure tgammaf;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00297.o}
procedure trunc;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00299.o}
procedure truncf;external;
{$L x64/libapi-ms-win-crt-math-l1-1-0s00300.o}
procedure ___lc_codepage_func;external;
{$L x64/libapi-ms-win-crt-locale-l1-1-0s00000.o}
procedure ___lc_collate_cp_func;external;
{$L x64/libapi-ms-win-crt-locale-l1-1-0s00001.o}
procedure ___lc_locale_name_func;external;
{$L x64/libapi-ms-win-crt-locale-l1-1-0s00002.o}
procedure ___mb_cur_max_func;external;
{$L x64/libapi-ms-win-crt-locale-l1-1-0s00003.o}
procedure ___mb_cur_max_l_func;external;
{$L x64/libapi-ms-win-crt-locale-l1-1-0s00004.o}
procedure __initialize_lconv_for_unsigned_char;external;
{$L x64/libapi-ms-win-crt-locale-l1-1-0s00005.o}
procedure __lconv_init;external;
{$L x64/libapi-ms-win-crt-locale-l1-1-0s00006.o}
procedure __pctype_func;external;
{$L x64/libapi-ms-win-crt-locale-l1-1-0s00007.o}
procedure __pwctype_func;external;
{$L x64/libapi-ms-win-crt-locale-l1-1-0s00008.o}
procedure _configthreadlocale;external;
{$L x64/libapi-ms-win-crt-locale-l1-1-0s00009.o}
procedure _create_locale;external;
{$L x64/libapi-ms-win-crt-locale-l1-1-0s00010.o}
procedure _free_locale;external;
{$L x64/libapi-ms-win-crt-locale-l1-1-0s00011.o}
procedure _get_current_locale;external;
{$L x64/libapi-ms-win-crt-locale-l1-1-0s00012.o}
procedure _getmbcp;external;
{$L x64/libapi-ms-win-crt-locale-l1-1-0s00013.o}
procedure _lock_locales;external;
{$L x64/libapi-ms-win-crt-locale-l1-1-0s00014.o}
procedure _setmbcp;external;
{$L x64/libapi-ms-win-crt-locale-l1-1-0s00015.o}
procedure _unlock_locales;external;
{$L x64/libapi-ms-win-crt-locale-l1-1-0s00016.o}
procedure _wcreate_locale;external;
{$L x64/libapi-ms-win-crt-locale-l1-1-0s00017.o}
procedure _wsetlocale;external;
{$L x64/libapi-ms-win-crt-locale-l1-1-0s00018.o}
procedure localeconv;external;
{$L x64/libapi-ms-win-crt-locale-l1-1-0s00019.o}
procedure setlocale;external;
{$L x64/libapi-ms-win-crt-locale-l1-1-0s00020.o}
procedure _aligned_free;external;
{$L x64/libapi-ms-win-crt-heap-l1-1-0s00000.o}
procedure _aligned_malloc;external;
{$L x64/libapi-ms-win-crt-heap-l1-1-0s00001.o}
procedure _aligned_msize;external;
{$L x64/libapi-ms-win-crt-heap-l1-1-0s00002.o}
procedure _aligned_offset_malloc;external;
{$L x64/libapi-ms-win-crt-heap-l1-1-0s00003.o}
procedure _aligned_offset_realloc;external;
{$L x64/libapi-ms-win-crt-heap-l1-1-0s00004.o}
procedure _aligned_offset_recalloc;external;
{$L x64/libapi-ms-win-crt-heap-l1-1-0s00005.o}
procedure _aligned_realloc;external;
{$L x64/libapi-ms-win-crt-heap-l1-1-0s00006.o}
procedure _aligned_recalloc;external;
{$L x64/libapi-ms-win-crt-heap-l1-1-0s00007.o}
procedure _callnewh;external;
{$L x64/libapi-ms-win-crt-heap-l1-1-0s00008.o}
procedure _calloc_base;external;
{$L x64/libapi-ms-win-crt-heap-l1-1-0s00009.o}
procedure _expand;external;
{$L x64/libapi-ms-win-crt-heap-l1-1-0s00010.o}
procedure _free_base;external;
{$L x64/libapi-ms-win-crt-heap-l1-1-0s00011.o}
procedure _get_heap_handle;external;
{$L x64/libapi-ms-win-crt-heap-l1-1-0s00012.o}
procedure _heapchk;external;
{$L x64/libapi-ms-win-crt-heap-l1-1-0s00013.o}
procedure _heapmin;external;
{$L x64/libapi-ms-win-crt-heap-l1-1-0s00014.o}
procedure _heapwalk;external;
{$L x64/libapi-ms-win-crt-heap-l1-1-0s00015.o}
procedure _malloc_base;external;
{$L x64/libapi-ms-win-crt-heap-l1-1-0s00016.o}
procedure _msize;external;
{$L x64/libapi-ms-win-crt-heap-l1-1-0s00017.o}
procedure _query_new_handler;external;
{$L x64/libapi-ms-win-crt-heap-l1-1-0s00018.o}
procedure _query_new_mode;external;
{$L x64/libapi-ms-win-crt-heap-l1-1-0s00019.o}
procedure heapwalk;external;
{$L x64/libapi-ms-win-crt-heap-l1-1-0s00020.o}
procedure _realloc_base;external;
{$L x64/libapi-ms-win-crt-heap-l1-1-0s00021.o}
procedure _recalloc;external;
{$L x64/libapi-ms-win-crt-heap-l1-1-0s00022.o}
procedure _set_new_mode;external;
{$L x64/libapi-ms-win-crt-heap-l1-1-0s00023.o}
procedure _findclose;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00000.o}
procedure _findfirst;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00001.o}
procedure access;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00002.o}
procedure _access;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00003.o}
procedure _access_s;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00004.o}
procedure _chdir;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00005.o}
procedure _chdrive;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00006.o}
procedure _chmod;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00007.o}
procedure chmod;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00008.o}
procedure chdir;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00009.o}
procedure _findfirst32;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00010.o}
procedure _findfirst32i64;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00011.o}
procedure _findfirst64;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00012.o}
procedure _findfirst64i32;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00013.o}
procedure _findnext;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00014.o}
procedure _findnext32;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00015.o}
procedure _findnext32i64;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00016.o}
procedure _findnext64;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00017.o}
procedure _findnext64i32;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00018.o}
procedure _fstat32;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00019.o}
procedure _fstat32i64;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00020.o}
procedure _fstat64;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00021.o}
procedure _fstat64i32;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00022.o}
procedure _fullpath;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00023.o}
procedure _getdiskfree;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00024.o}
procedure _getdrive;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00025.o}
procedure _getdrives;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00026.o}
procedure _lock_file;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00027.o}
procedure _makepath;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00028.o}
procedure _makepath_s;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00029.o}
procedure _mkdir;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00030.o}
procedure rmdir;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00031.o}
procedure _rmdir;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00032.o}
procedure _splitpath;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00033.o}
procedure _splitpath_s;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00034.o}
procedure _stat32;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00035.o}
procedure mkdir;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00036.o}
procedure _stat32i64;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00037.o}
procedure _stat64;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00038.o}
procedure _stat64i32;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00039.o}
procedure _umask;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00040.o}
procedure umask;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00041.o}
procedure _umask_s;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00042.o}
procedure _unlink;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00043.o}
procedure _unlock_file;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00044.o}
procedure _waccess;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00045.o}
procedure unlink;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00046.o}
procedure _waccess_s;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00047.o}
procedure _wchdir;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00048.o}
procedure _wchmod;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00049.o}
procedure _wfindfirst32;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00050.o}
procedure _wfindfirst32i64;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00051.o}
procedure _wfindfirst64;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00052.o}
procedure _wfindfirst64i32;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00053.o}
procedure _wfindnext32;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00054.o}
procedure _wfindnext32i64;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00055.o}
procedure _wfindnext64;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00056.o}
procedure _wfindnext64i32;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00057.o}
procedure _wfullpath;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00058.o}
procedure _wmakepath;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00059.o}
procedure _wmakepath_s;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00060.o}
procedure _wmkdir;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00061.o}
procedure _wremove;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00062.o}
procedure _wrename;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00063.o}
procedure _wrmdir;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00064.o}
procedure _wsplitpath;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00065.o}
procedure _wsplitpath_s;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00066.o}
procedure _wstat32;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00067.o}
procedure _wstat32i64;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00068.o}
procedure _wstat64;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00069.o}
procedure _wstat64i32;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00070.o}
procedure _wunlink;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00071.o}
procedure remove;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00072.o}
procedure rename;external;
{$L x64/libapi-ms-win-crt-filesystem-l1-1-0s00073.o}
procedure __p__environ;external;
{$L x64/libapi-ms-win-crt-environment-l1-1-0s00000.o}
procedure __p__wenviron;external;
{$L x64/libapi-ms-win-crt-environment-l1-1-0s00001.o}
procedure _dupenv_s;external;
{$L x64/libapi-ms-win-crt-environment-l1-1-0s00002.o}
procedure _putenv;external;
{$L x64/libapi-ms-win-crt-environment-l1-1-0s00003.o}
procedure putenv;external;
{$L x64/libapi-ms-win-crt-environment-l1-1-0s00004.o}
procedure _putenv_s;external;
{$L x64/libapi-ms-win-crt-environment-l1-1-0s00005.o}
procedure _searchenv;external;
{$L x64/libapi-ms-win-crt-environment-l1-1-0s00006.o}
procedure _searchenv_s;external;
{$L x64/libapi-ms-win-crt-environment-l1-1-0s00007.o}
procedure _wdupenv_s;external;
{$L x64/libapi-ms-win-crt-environment-l1-1-0s00008.o}
procedure searchenv;external;
{$L x64/libapi-ms-win-crt-environment-l1-1-0s00009.o}
procedure _wgetcwd;external;
{$L x64/libapi-ms-win-crt-environment-l1-1-0s00010.o}
procedure _wgetdcwd;external;
{$L x64/libapi-ms-win-crt-environment-l1-1-0s00011.o}
procedure _wgetenv;external;
{$L x64/libapi-ms-win-crt-environment-l1-1-0s00012.o}
procedure _wgetenv_s;external;
{$L x64/libapi-ms-win-crt-environment-l1-1-0s00013.o}
procedure _wputenv;external;
{$L x64/libapi-ms-win-crt-environment-l1-1-0s00014.o}
procedure _wputenv_s;external;
{$L x64/libapi-ms-win-crt-environment-l1-1-0s00015.o}
procedure _wsearchenv;external;
{$L x64/libapi-ms-win-crt-environment-l1-1-0s00016.o}
procedure _wsearchenv_s;external;
{$L x64/libapi-ms-win-crt-environment-l1-1-0s00017.o}
procedure getenv;external;
{$L x64/libapi-ms-win-crt-environment-l1-1-0s00018.o}
procedure getenv_s;external;
{$L x64/libapi-ms-win-crt-environment-l1-1-0s00019.o}
procedure __toascii;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00000.o}
procedure _atodbl;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00001.o}
procedure _atodbl_l;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00002.o}
procedure _atof_l;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00003.o}
procedure _atoflt;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00004.o}
procedure _atoflt_l;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00005.o}
procedure toascii;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00006.o}
procedure _atoi64;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00007.o}
procedure _atoi64_l;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00008.o}
procedure _atoi_l;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00009.o}
procedure _atol_l;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00010.o}
procedure _atoldbl;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00011.o}
procedure _atoldbl_l;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00012.o}
procedure _atoll_l;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00013.o}
procedure _ecvt;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00014.o}
procedure ecvt;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00015.o}
procedure _ecvt_s;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00016.o}
procedure _fcvt;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00017.o}
procedure _fcvt_s;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00018.o}
procedure gcvt;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00019.o}
procedure _gcvt;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00020.o}
procedure fcvt;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00021.o}
procedure _gcvt_s;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00022.o}
procedure _i64toa;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00023.o}
procedure _i64toa_s;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00024.o}
procedure _i64tow;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00025.o}
procedure _i64tow_s;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00026.o}
procedure _itoa;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00027.o}
procedure itoa;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00028.o}
procedure _itoa_s;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00029.o}
procedure _itow;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00030.o}
procedure _itow_s;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00031.o}
procedure _ltoa;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00032.o}
procedure _ltoa_s;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00033.o}
procedure _ltow;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00034.o}
procedure _ltow_s;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00035.o}
procedure _strtod_l;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00036.o}
procedure _strtof_l;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00037.o}
procedure ltoa;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00038.o}
procedure _strtoi64;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00039.o}
procedure _strtoi64_l;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00040.o}
procedure _strtoimax_l;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00041.o}
procedure _strtol_l;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00042.o}
procedure _strtold_l;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00043.o}
procedure _strtoll_l;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00044.o}
procedure _strtoui64;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00045.o}
procedure _strtoui64_l;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00046.o}
procedure _strtoul_l;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00047.o}
procedure _strtoull_l;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00048.o}
procedure _strtoumax_l;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00049.o}
procedure _ui64toa;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00050.o}
procedure _ui64toa_s;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00051.o}
procedure _ui64tow;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00052.o}
procedure _ui64tow_s;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00053.o}
procedure _ultoa;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00054.o}
procedure _ultoa_s;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00055.o}
procedure _ultow;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00056.o}
procedure _ultow_s;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00057.o}
procedure _wcstod_l;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00058.o}
procedure _wcstof_l;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00059.o}
procedure _wcstoi64;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00060.o}
procedure _wcstoi64_l;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00061.o}
procedure _wcstoimax_l;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00062.o}
procedure _wcstol_l;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00063.o}
procedure _wcstold_l;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00064.o}
procedure _wcstoll_l;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00065.o}
procedure _wcstombs_l;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00066.o}
procedure _wcstombs_s_l;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00067.o}
procedure _wcstoui64;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00068.o}
procedure _wcstoui64_l;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00069.o}
procedure _wcstoul_l;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00070.o}
procedure _wcstoull_l;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00071.o}
procedure _wcstoumax_l;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00072.o}
procedure _wctomb_l;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00073.o}
procedure _wctomb_s_l;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00074.o}
procedure _wtof;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00075.o}
procedure _wtof_l;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00076.o}
procedure _wtoi;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00077.o}
procedure _wtoi64;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00078.o}
procedure _wtoi64_l;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00079.o}
procedure _wtoi_l;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00080.o}
procedure _wtol;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00081.o}
procedure _wtol_l;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00082.o}
procedure _wtoll;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00083.o}
procedure _wtoll_l;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00084.o}
procedure atof;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00085.o}
procedure atoi;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00086.o}
procedure atol;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00087.o}
procedure atoll;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00088.o}
procedure btowc;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00089.o}
procedure c16rtomb;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00090.o}
procedure c32rtomb;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00091.o}
procedure mbrtoc16;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00092.o}
procedure mbrtoc32;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00093.o}
procedure mbrtowc;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00094.o}
procedure mbsrtowcs;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00095.o}
procedure mbsrtowcs_s;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00096.o}
procedure mbstowcs;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00097.o}
procedure mbstowcs_s;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00098.o}
procedure mbtowc;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00099.o}
procedure strtod;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00100.o}
procedure strtof;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00101.o}
procedure strtoimax;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00102.o}
procedure strtol;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00103.o}
procedure strtoll;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00104.o}
procedure strtoul;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00105.o}
procedure strtoull;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00106.o}
procedure strtoumax;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00107.o}
procedure wcrtomb;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00108.o}
procedure wcrtomb_s;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00109.o}
procedure wcsrtombs;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00110.o}
procedure wcsrtombs_s;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00111.o}
procedure wcstod;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00112.o}
procedure wcstof;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00113.o}
procedure wcstoimax;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00114.o}
procedure wcstol;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00115.o}
procedure wcstoll;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00116.o}
procedure wcstombs;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00117.o}
procedure wcstombs_s;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00118.o}
procedure wcstoul;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00119.o}
procedure wcstoull;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00120.o}
procedure wcstoumax;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00121.o}
procedure wctob;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00122.o}
procedure wctomb;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00123.o}
procedure wctomb_s;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00124.o}
procedure wctrans;external;
{$L x64/libapi-ms-win-crt-convert-l1-1-0s00125.o}
procedure __conio_common_vcprintf;external;
{$L x64/libapi-ms-win-crt-conio-l1-1-0s00000.o}
procedure __conio_common_vcprintf_p;external;
{$L x64/libapi-ms-win-crt-conio-l1-1-0s00001.o}
procedure __conio_common_vcprintf_s;external;
{$L x64/libapi-ms-win-crt-conio-l1-1-0s00002.o}
procedure __conio_common_vcscanf;external;
{$L x64/libapi-ms-win-crt-conio-l1-1-0s00003.o}
procedure __conio_common_vcwprintf;external;
{$L x64/libapi-ms-win-crt-conio-l1-1-0s00004.o}
procedure __conio_common_vcwprintf_p;external;
{$L x64/libapi-ms-win-crt-conio-l1-1-0s00005.o}
procedure __conio_common_vcwprintf_s;external;
{$L x64/libapi-ms-win-crt-conio-l1-1-0s00006.o}
procedure __conio_common_vcwscanf;external;
{$L x64/libapi-ms-win-crt-conio-l1-1-0s00007.o}
procedure _cgets;external;
{$L x64/libapi-ms-win-crt-conio-l1-1-0s00008.o}
procedure _cgets_s;external;
{$L x64/libapi-ms-win-crt-conio-l1-1-0s00009.o}
procedure _cgetws;external;
{$L x64/libapi-ms-win-crt-conio-l1-1-0s00010.o}
procedure _cgetws_s;external;
{$L x64/libapi-ms-win-crt-conio-l1-1-0s00011.o}
procedure _cputs;external;
{$L x64/libapi-ms-win-crt-conio-l1-1-0s00012.o}
procedure _cputws;external;
{$L x64/libapi-ms-win-crt-conio-l1-1-0s00013.o}
procedure _getch;external;
{$L x64/libapi-ms-win-crt-conio-l1-1-0s00014.o}
procedure getch;external;
{$L x64/libapi-ms-win-crt-conio-l1-1-0s00015.o}
procedure _getch_nolock;external;
{$L x64/libapi-ms-win-crt-conio-l1-1-0s00016.o}
procedure _getche;external;
{$L x64/libapi-ms-win-crt-conio-l1-1-0s00017.o}
procedure _getche_nolock;external;
{$L x64/libapi-ms-win-crt-conio-l1-1-0s00018.o}
procedure _getwch;external;
{$L x64/libapi-ms-win-crt-conio-l1-1-0s00019.o}
procedure _getwch_nolock;external;
{$L x64/libapi-ms-win-crt-conio-l1-1-0s00020.o}
procedure _getwche;external;
{$L x64/libapi-ms-win-crt-conio-l1-1-0s00021.o}
procedure _getwche_nolock;external;
{$L x64/libapi-ms-win-crt-conio-l1-1-0s00022.o}
procedure getche;external;
{$L x64/libapi-ms-win-crt-conio-l1-1-0s00023.o}
procedure _putch;external;
{$L x64/libapi-ms-win-crt-conio-l1-1-0s00024.o}
procedure putch;external;
{$L x64/libapi-ms-win-crt-conio-l1-1-0s00025.o}
procedure _putch_nolock;external;
{$L x64/libapi-ms-win-crt-conio-l1-1-0s00026.o}
procedure _putwch;external;
{$L x64/libapi-ms-win-crt-conio-l1-1-0s00027.o}
procedure _putwch_nolock;external;
{$L x64/libapi-ms-win-crt-conio-l1-1-0s00028.o}
procedure _ungetch;external;
{$L x64/libapi-ms-win-crt-conio-l1-1-0s00029.o}
procedure _ungetch_nolock;external;
{$L x64/libapi-ms-win-crt-conio-l1-1-0s00030.o}
procedure ungetch;external;
{$L x64/libapi-ms-win-crt-conio-l1-1-0s00031.o}
procedure _ungetwch;external;
{$L x64/libapi-ms-win-crt-conio-l1-1-0s00032.o}
procedure _ungetwch_nolock;external;
{$L x64/libapi-ms-win-crt-conio-l1-1-0s00033.o}
procedure __chkstk;external;
{$L x64/chkstk.obj}
function  malloc(size: NativeInt): Pointer; cdecl;
function realloc(P: Pointer; NewSize: NativeInt): Pointer; cdecl;
procedure free(pBlock: Pointer); cdecl;
function calloc(nitems,size : NativeInt):Pointer;cdecl;
var
  __imp__abs64 : UInt64;
  __imp__byteswap_uint64 : UInt64;
  __imp__byteswap_ulong : UInt64;
  __imp__byteswap_ushort : UInt64;
  __imp_lfind : UInt64;
  __imp__lfind : UInt64;
  __imp__lfind_s : UInt64;
  __imp__lrotl : UInt64;
  __imp__lrotr : UInt64;
  __imp_lsearch : UInt64;
  __imp__lsearch : UInt64;
  __imp__lsearch_s : UInt64;
  __imp__rotl : UInt64;
  __imp__rotl64 : UInt64;
  __imp__rotr : UInt64;
  __imp__rotr64 : UInt64;
  __imp__swab : UInt64;
  __imp_swab : UInt64;
  __imp_abs : UInt64;
  __imp_bsearch : UInt64;
  __imp_bsearch_s : UInt64;
  __imp_div : UInt64;
  __imp_imaxabs : UInt64;
  __imp_imaxdiv : UInt64;
  __imp_labs : UInt64;
  __imp_ldiv : UInt64;
  __imp_llabs : UInt64;
  __imp_lldiv : UInt64;
  __imp_qsort : UInt64;
  __imp_qsort_s : UInt64;
  __imp_rand : UInt64;
  __imp_rand_s : UInt64;
  __imp_srand : UInt64;
  _head_lib64_libapi_ms_win_crt_utility_l1_1_0_a : UInt64;
  __lib64_libapi_ms_win_crt_utility_l1_1_0_a_iname : UInt64;
  __imp__Getdays : UInt64;
  __imp__Getmonths : UInt64;
  __imp__Gettnames : UInt64;
  __imp__Strftime : UInt64;
  __imp__W_Getdays : UInt64;
  __imp__W_Getmonths : UInt64;
  __imp__W_Gettnames : UInt64;
  __imp__Wcsftime : UInt64;
  __imp___daylight : UInt64;
  __imp___dstbias : UInt64;
  __imp___timezone : UInt64;
  __imp___tzname : UInt64;
  __imp__ctime32 : UInt64;
  __imp__ctime32_s : UInt64;
  __imp_ctime : UInt64;
  __imp__ctime64 : UInt64;
  __imp__ctime64_s : UInt64;
  __imp__difftime32 : UInt64;
  __imp__difftime64 : UInt64;
  __imp__ftime32 : UInt64;
  __imp__ftime32_s : UInt64;
  __imp__ftime : UInt64;
  __imp__ftime64 : UInt64;
  __imp__ftime64_s : UInt64;
  __imp__futime32 : UInt64;
  __imp__futime : UInt64;
  __imp__futime64 : UInt64;
  __imp__get_daylight : UInt64;
  __imp__get_dstbias : UInt64;
  __imp__get_timezone : UInt64;
  __imp__get_tzname : UInt64;
  __imp__getsystime : UInt64;
  __imp__gmtime32 : UInt64;
  __imp__gmtime32_s : UInt64;
  __imp_gmtime : UInt64;
  __imp__gmtime64 : UInt64;
  __imp__gmtime64_s : UInt64;
  __imp__localtime32 : UInt64;
  __imp__localtime32_s : UInt64;
  __imp__localtime64 : UInt64;
  __imp_localtime : UInt64;
  __imp__localtime64_s : UInt64;
  __imp__mkgmtime32 : UInt64;
  __imp__mkgmtime64 : UInt64;
  __imp__mktime32 : UInt64;
  __imp_mktime : UInt64;
  __imp__mktime64 : UInt64;
  __imp__setsystime : UInt64;
  __imp__strdate : UInt64;
  __imp__strdate_s : UInt64;
  __imp__strftime_l : UInt64;
  __imp__strtime : UInt64;
  __imp__strtime_s : UInt64;
  __imp__time32 : UInt64;
  __imp_time : UInt64;
  __imp__time64 : UInt64;
  __imp__timespec32_get : UInt64;
  __imp__timespec64_get : UInt64;
  __imp__tzset : UInt64;
  __imp__utime32 : UInt64;
  __imp__utime : UInt64;
  __imp__utime64 : UInt64;
  __imp__wasctime : UInt64;
  __imp_utime : UInt64;
  __imp__wasctime_s : UInt64;
  __imp__wcsftime_l : UInt64;
  __imp__wctime32 : UInt64;
  __imp__wctime32_s : UInt64;
  __imp__wctime64 : UInt64;
  __imp__wctime64_s : UInt64;
  __imp__wutime : UInt64;
  __imp__wstrdate : UInt64;
  __imp__wstrdate_s : UInt64;
  __imp__wstrtime : UInt64;
  __imp__wstrtime_s : UInt64;
  __imp__wutime32 : UInt64;
  __imp__wutime64 : UInt64;
  __imp_asctime : UInt64;
  __imp_asctime_s : UInt64;
  __imp_clock : UInt64;
  __imp_strftime : UInt64;
  __imp_timespec_get : UInt64;
  __imp_wcsftime : UInt64;
  _head_lib64_libapi_ms_win_crt_time_l1_1_0_a : UInt64;
  __lib64_libapi_ms_win_crt_time_l1_1_0_a_iname : UInt64;
  __imp__iswalpha_l : UInt64;
  __imp__strcmpi : UInt64;
  __imp___isascii : UInt64;
  __imp___iscsym : UInt64;
  __imp_iscsymf : UInt64;
  __imp___iscsymf : UInt64;
  __imp___iswcsym : UInt64;
  __imp_iscsym : UInt64;
  __imp___iswcsymf : UInt64;
  __imp___strncnt : UInt64;
  __imp___wcsncnt : UInt64;
  __imp__isalnum_l : UInt64;
  __imp__isalpha_l : UInt64;
  __imp__isblank_l : UInt64;
  __imp__iscntrl_l : UInt64;
  __imp__isctype : UInt64;
  __imp__isctype_l : UInt64;
  __imp__isdigit_l : UInt64;
  __imp__isgraph_l : UInt64;
  __imp__isleadbyte_l : UInt64;
  __imp__islower_l : UInt64;
  __imp__isprint_l : UInt64;
  __imp_isascii : UInt64;
  __imp__ispunct_l : UInt64;
  __imp__isspace_l : UInt64;
  __imp__isupper_l : UInt64;
  __imp__iswalnum_l : UInt64;
  __imp__iswblank_l : UInt64;
  __imp__iswcntrl_l : UInt64;
  __imp__iswcsym_l : UInt64;
  __imp__iswcsymf_l : UInt64;
  __imp__iswctype_l : UInt64;
  __imp__iswdigit_l : UInt64;
  __imp__iswgraph_l : UInt64;
  __imp__iswlower_l : UInt64;
  __imp__iswprint_l : UInt64;
  __imp__iswpunct_l : UInt64;
  __imp__iswspace_l : UInt64;
  __imp__iswupper_l : UInt64;
  __imp__iswxdigit_l : UInt64;
  __imp__isxdigit_l : UInt64;
  __imp__memccpy : UInt64;
  __imp_memicmp : UInt64;
  __imp__memicmp : UInt64;
  __imp__memicmp_l : UInt64;
  __imp__strcoll_l : UInt64;
  __imp_memccpy : UInt64;
  __imp__strdup : UInt64;
  __imp_strcmpi : UInt64;
  __imp_stricmp : UInt64;
  __imp_strcasecmp : UInt64;
  __imp_strdup : UInt64;
  __imp__stricmp : UInt64;
  __imp__stricmp_l : UInt64;
  __imp_stricoll : UInt64;
  __imp__stricoll : UInt64;
  __imp__stricoll_l : UInt64;
  __imp__strlwr : UInt64;
  __imp_strlwr : UInt64;
  __imp__strlwr_l : UInt64;
  __imp__strlwr_s : UInt64;
  __imp__strlwr_s_l : UInt64;
  __imp__strncoll : UInt64;
  __imp__strncoll_l : UInt64;
  __imp__strnicmp : UInt64;
  __imp_strnicmp : UInt64;
  __imp_strncasecmp : UInt64;
  __imp__strnicmp_l : UInt64;
  __imp__strnicoll : UInt64;
  __imp__strnicoll_l : UInt64;
  __imp_strnset : UInt64;
  __imp__strnset : UInt64;
  __imp__strnset_s : UInt64;
  __imp_strrev : UInt64;
  __imp__strrev : UInt64;
  __imp__strset : UInt64;
  __imp_strset : UInt64;
  __imp__strset_s : UInt64;
  __imp_strupr : UInt64;
  __imp__strupr : UInt64;
  __imp__strupr_l : UInt64;
  __imp__strupr_s : UInt64;
  __imp__strupr_s_l : UInt64;
  __imp__strxfrm_l : UInt64;
  __imp__tolower : UInt64;
  __imp__tolower_l : UInt64;
  __imp__toupper : UInt64;
  __imp__toupper_l : UInt64;
  __imp__towlower_l : UInt64;
  __imp__towupper_l : UInt64;
  __imp__wcscoll_l : UInt64;
  __imp_wcsdup : UInt64;
  __imp__wcsdup : UInt64;
  __imp_wcsicmp : UInt64;
  __imp_wcscmpi : UInt64;
  __imp__wcsicmp : UInt64;
  __imp__wcsicmp_l : UInt64;
  __imp__wcsicoll : UInt64;
  __imp_wcsicoll : UInt64;
  __imp__wcsicoll_l : UInt64;
  __imp__wcslwr : UInt64;
  __imp_wcslwr : UInt64;
  __imp__wcslwr_l : UInt64;
  __imp__wcslwr_s : UInt64;
  __imp__wcslwr_s_l : UInt64;
  __imp__wcsncoll : UInt64;
  __imp__wcsncoll_l : UInt64;
  __imp__wcsnicmp : UInt64;
  __imp_wcsnicmp : UInt64;
  __imp__wcsnicmp_l : UInt64;
  __imp__wcsnicoll : UInt64;
  __imp__wcsnicoll_l : UInt64;
  __imp__wcsnset : UInt64;
  __imp__wcsnset_s : UInt64;
  __imp_wcsnset : UInt64;
  __imp__wcsrev : UInt64;
  __imp_wcsrev : UInt64;
  __imp__wcsset : UInt64;
  __imp__wcsset_s : UInt64;
  __imp_wcsupr : UInt64;
  __imp__wcsupr : UInt64;
  __imp_wcsset : UInt64;
  __imp__wcsupr_l : UInt64;
  __imp__wcsupr_s : UInt64;
  __imp__wcsupr_s_l : UInt64;
  __imp__wcsxfrm_l : UInt64;
  __imp__wctype : UInt64;
  __imp_is_wctype : UInt64;
  __imp_isalnum : UInt64;
  __imp_isalpha : UInt64;
  __imp_isblank : UInt64;
  __imp_iscntrl : UInt64;
  __imp_isdigit : UInt64;
  __imp_isgraph : UInt64;
  __imp_isleadbyte : UInt64;
  __imp_islower : UInt64;
  __imp_isprint : UInt64;
  __imp_ispunct : UInt64;
  __imp_isspace : UInt64;
  __imp_isupper : UInt64;
  __imp_iswalnum : UInt64;
  __imp_iswalpha : UInt64;
  __imp_iswascii : UInt64;
  __imp_iswblank : UInt64;
  __imp_iswcntrl : UInt64;
  __imp_iswctype : UInt64;
  __imp_iswdigit : UInt64;
  __imp_iswgraph : UInt64;
  __imp_iswlower : UInt64;
  __imp_iswprint : UInt64;
  __imp_iswpunct : UInt64;
  __imp_iswspace : UInt64;
  __imp_iswupper : UInt64;
  __imp_iswxdigit : UInt64;
  __imp_isxdigit : UInt64;
  __imp_mblen : UInt64;
  __imp_mbrlen : UInt64;
  __imp_memcpy_s : UInt64;
  __imp_memmove_s : UInt64;
  __imp_memset : UInt64;
  __imp_strcat : UInt64;
  __imp_strcat_s : UInt64;
  __imp_strcmp : UInt64;
  __imp_strcoll : UInt64;
  __imp_strcpy : UInt64;
  __imp_strcpy_s : UInt64;
  __imp_strcspn : UInt64;
  __imp_strlen : UInt64;
  __imp_strncat : UInt64;
  __imp_strncat_s : UInt64;
  __imp_strncmp : UInt64;
  __imp_strncpy : UInt64;
  __imp_strncpy_s : UInt64;
  __imp_strpbrk : UInt64;
  __imp_strspn : UInt64;
  __imp_strtok : UInt64;
  __imp_strtok_s : UInt64;
  __imp_strxfrm : UInt64;
  __imp_tolower : UInt64;
  __imp_toupper : UInt64;
  __imp_towctrans : UInt64;
  __imp_towlower : UInt64;
  __imp_towupper : UInt64;
  __imp_wcscat : UInt64;
  __imp_wcscat_s : UInt64;
  __imp_wcscmp : UInt64;
  __imp_wcscoll : UInt64;
  __imp_wcscpy : UInt64;
  __imp_wcscpy_s : UInt64;
  __imp_wcscspn : UInt64;
  __imp_wcslen : UInt64;
  __imp_wcsncat : UInt64;
  __imp_wcsncat_s : UInt64;
  __imp_wcsncmp : UInt64;
  __imp_wcsncpy : UInt64;
  __imp_wcsncpy_s : UInt64;
  __imp_wcsnlen : UInt64;
  __imp_wcspbrk : UInt64;
  __imp_wcsspn : UInt64;
  __imp_wcstok : UInt64;
  __imp_wcstok_s : UInt64;
  __imp_wcsxfrm : UInt64;
  __imp_wctype : UInt64;
  __imp_wmemcpy_s : UInt64;
  __imp_wmemmove_s : UInt64;
  _head_lib64_libapi_ms_win_crt_string_l1_1_0_a : UInt64;
  __lib64_libapi_ms_win_crt_string_l1_1_0_a_iname : UInt64;
  __imp___acrt_iob_func : UInt64;
  __imp___p__commode : UInt64;
  __imp___p__fmode : UInt64;
  __imp___stdio_common_vfprintf : UInt64;
  __imp___stdio_common_vfprintf_p : UInt64;
  __imp___stdio_common_vfprintf_s : UInt64;
  __imp___stdio_common_vfscanf : UInt64;
  __imp___stdio_common_vfwprintf : UInt64;
  __imp___stdio_common_vfwprintf_p : UInt64;
  __imp___stdio_common_vfwprintf_s : UInt64;
  __imp___stdio_common_vfwscanf : UInt64;
  __imp___stdio_common_vsnprintf_s : UInt64;
  __imp___stdio_common_vsnwprintf_s : UInt64;
  __imp___stdio_common_vsprintf : UInt64;
  __imp___stdio_common_vsprintf_p : UInt64;
  __imp___stdio_common_vsprintf_s : UInt64;
  __imp___stdio_common_vsscanf : UInt64;
  __imp___stdio_common_vswprintf : UInt64;
  __imp___stdio_common_vswprintf_p : UInt64;
  __imp___stdio_common_vswprintf_s : UInt64;
  __imp___stdio_common_vswscanf : UInt64;
  __imp__chsize : UInt64;
  __imp_chsize : UInt64;
  __imp__chsize_s : UInt64;
  __imp__close : UInt64;
  __imp__commit : UInt64;
  __imp_creat : UInt64;
  __imp__creat : UInt64;
  __imp_close : UInt64;
  __imp__dup : UInt64;
  __imp_dup : UInt64;
  __imp__dup2 : UInt64;
  __imp_eof : UInt64;
  __imp__eof : UInt64;
  __imp_dup2 : UInt64;
  __imp__fclose_nolock : UInt64;
  __imp__fcloseall : UInt64;
  __imp__fflush_nolock : UInt64;
  __imp__fgetc_nolock : UInt64;
  __imp__fgetchar : UInt64;
  __imp_fgetchar : UInt64;
  __imp__fgetwc_nolock : UInt64;
  __imp__fgetwchar : UInt64;
  __imp_filelength : UInt64;
  __imp__filelength : UInt64;
  __imp__filelengthi64 : UInt64;
  __imp_fgetwchar : UInt64;
  __imp__fileno : UInt64;
  __imp_fileno : UInt64;
  __imp__flushall : UInt64;
  __imp__fputc_nolock : UInt64;
  __imp__fputchar : UInt64;
  __imp__fputwc_nolock : UInt64;
  __imp_fputchar : UInt64;
  __imp__fputwchar : UInt64;
  __imp_fputwchar : UInt64;
  __imp__fread_nolock : UInt64;
  __imp__fread_nolock_s : UInt64;
  __imp__fseek_nolock : UInt64;
  __imp__fseeki64 : UInt64;
  __imp__fseeki64_nolock : UInt64;
  __imp__fsopen : UInt64;
  __imp__ftell_nolock : UInt64;
  __imp__ftelli64 : UInt64;
  __imp__ftelli64_nolock : UInt64;
  __imp__fwrite_nolock : UInt64;
  __imp__get_fmode : UInt64;
  __imp__get_osfhandle : UInt64;
  __imp__get_printf_count_output : UInt64;
  __imp__get_stream_buffer_pointers : UInt64;
  __imp__getc_nolock : UInt64;
  __imp__getcwd : UInt64;
  __imp_getcwd : UInt64;
  __imp__getdcwd : UInt64;
  __imp__getmaxstdio : UInt64;
  __imp__getw : UInt64;
  __imp_getw : UInt64;
  __imp__getwc_nolock : UInt64;
  __imp__getws : UInt64;
  __imp__getws_s : UInt64;
  __imp__isatty : UInt64;
  __imp_isatty : UInt64;
  __imp__kbhit : UInt64;
  __imp_kbhit : UInt64;
  __imp__locking : UInt64;
  __imp__lseek : UInt64;
  __imp__lseeki64 : UInt64;
  __imp_mktemp : UInt64;
  __imp__mktemp : UInt64;
  __imp__mktemp_s : UInt64;
  __imp__open : UInt64;
  __imp_lseek : UInt64;
  __imp_open : UInt64;
  __imp_pclose : UInt64;
  __imp__pclose : UInt64;
  __imp__open_osfhandle : UInt64;
  __imp__pipe : UInt64;
  __imp__popen : UInt64;
  __imp__putc_nolock : UInt64;
  __imp_popen : UInt64;
  __imp__putw : UInt64;
  __imp_putw : UInt64;
  __imp__putwc_nolock : UInt64;
  __imp__putws : UInt64;
  __imp__read : UInt64;
  __imp_read : UInt64;
  __imp__rmtmp : UInt64;
  __imp_rmtmp : UInt64;
  __imp__set_fmode : UInt64;
  __imp__set_printf_count_output : UInt64;
  __imp__setmaxstdio : UInt64;
  __imp_setmode : UInt64;
  __imp__setmode : UInt64;
  __imp__sopen : UInt64;
  __imp__sopen_dispatch : UInt64;
  __imp__sopen_s : UInt64;
  __imp_sopen : UInt64;
  __imp__tell : UInt64;
  __imp_tell : UInt64;
  __imp__telli64 : UInt64;
  __imp__tempnam : UInt64;
  __imp__ungetc_nolock : UInt64;
  __imp__ungetwc_nolock : UInt64;
  __imp__wcreat : UInt64;
  __imp_tempnam : UInt64;
  __imp__wfdopen : UInt64;
  __imp__wfopen : UInt64;
  __imp__wfopen_s : UInt64;
  __imp__wfreopen : UInt64;
  __imp__wfreopen_s : UInt64;
  __imp__wfsopen : UInt64;
  __imp__wmktemp : UInt64;
  __imp__wmktemp_s : UInt64;
  __imp__wopen : UInt64;
  __imp__wpopen : UInt64;
  __imp_wpopen : UInt64;
  __imp__write : UInt64;
  __imp_write : UInt64;
  __imp__wsopen : UInt64;
  __imp__wsopen_dispatch : UInt64;
  __imp__wsopen_s : UInt64;
  __imp__wtempnam : UInt64;
  __imp__wtmpnam : UInt64;
  __imp__wtmpnam_s : UInt64;
  __imp_clearerr : UInt64;
  __imp_clearerr_s : UInt64;
  __imp_fclose : UInt64;
  __imp_feof : UInt64;
  __imp_ferror : UInt64;
  __imp_fflush : UInt64;
  __imp_fgetc : UInt64;
  __imp_fgetpos : UInt64;
  __imp_fgets : UInt64;
  __imp_fgetwc : UInt64;
  __imp_fgetws : UInt64;
  __imp_fopen : UInt64;
  __imp_fopen_s : UInt64;
  __imp_fputc : UInt64;
  __imp_fputs : UInt64;
  __imp_fputwc : UInt64;
  __imp_fputws : UInt64;
  __imp_fread : UInt64;
  __imp_fread_s : UInt64;
  __imp_freopen : UInt64;
  __imp_freopen_s : UInt64;
  __imp_fseek : UInt64;
  __imp_fsetpos : UInt64;
  __imp_ftell : UInt64;
  __imp_fwrite : UInt64;
  __imp_getc : UInt64;
  __imp_getchar : UInt64;
  __imp_gets : UInt64;
  __imp_gets_s : UInt64;
  __imp_getwc : UInt64;
  __imp_getwchar : UInt64;
  __imp_putc : UInt64;
  __imp_putchar : UInt64;
  __imp_puts : UInt64;
  __imp_putwc : UInt64;
  __imp_putwchar : UInt64;
  __imp_rewind : UInt64;
  __imp_setbuf : UInt64;
  __imp_setvbuf : UInt64;
  __imp_tmpfile : UInt64;
  __imp_tmpfile_s : UInt64;
  __imp_tmpnam : UInt64;
  __imp_tmpnam_s : UInt64;
  __imp_ungetc : UInt64;
  __imp_ungetwc : UInt64;
  _head_lib64_libapi_ms_win_crt_stdio_l1_1_0_a : UInt64;
  __lib64_libapi_ms_win_crt_stdio_l1_1_0_a_iname : UInt64;
  __imp__Exit : UInt64;
  __imp___doserrno : UInt64;
  __imp___fpe_flt_rounds : UInt64;
  __imp___fpecode : UInt64;
  __imp___p___argc : UInt64;
  __imp___p___argv : UInt64;
  __imp___p___wargv : UInt64;
  __imp___p__acmdln : UInt64;
  __imp___p__pgmptr : UInt64;
  __imp___p__wcmdln : UInt64;
  __imp___p__wpgmptr : UInt64;
  __imp___pxcptinfoptrs : UInt64;
  __imp___sys_errlist : UInt64;
  __imp___sys_nerr : UInt64;
  __imp___threadhandle : UInt64;
  __imp___threadid : UInt64;
  __imp___wcserror : UInt64;
  __imp___wcserror_s : UInt64;
  __imp__assert : UInt64;
  __imp__beginthread : UInt64;
  __imp__beginthreadex : UInt64;
  __imp__c_exit : UInt64;
  __imp__cexit : UInt64;
  __imp__clearfp : UInt64;
  __imp__configure_narrow_argv : UInt64;
  __imp__configure_wide_argv : UInt64;
  __imp__control87 : UInt64;
  __imp__controlfp : UInt64;
  __imp__controlfp_s : UInt64;
  __imp__crt_at_quick_exit : UInt64;
  __imp__crt_atexit : UInt64;
  __imp__crt_debugger_hook : UInt64;
  __imp__endthread : UInt64;
  __imp__endthreadex : UInt64;
  __imp__errno : UInt64;
  __imp__execute_onexit_table : UInt64;
  __imp__fpieee_flt : UInt64;
  __imp__fpreset : UInt64;
  __imp__get_doserrno : UInt64;
  __imp__get_errno : UInt64;
  __imp__get_initial_narrow_environment : UInt64;
  __imp__get_initial_wide_environment : UInt64;
  __imp__get_invalid_parameter_handler : UInt64;
  __imp__get_narrow_winmain_command_line : UInt64;
  __imp__get_pgmptr : UInt64;
  __imp__get_terminate : UInt64;
  __imp__get_thread_local_invalid_parameter_handler : UInt64;
  __imp__get_wide_winmain_command_line : UInt64;
  __imp__get_wpgmptr : UInt64;
  __imp__getdllprocaddr : UInt64;
  __imp__getpid : UInt64;
  __imp_getpid : UInt64;
  __imp__initialize_narrow_environment : UInt64;
  __imp__initialize_onexit_table : UInt64;
  __imp__initialize_wide_environment : UInt64;
  __imp__initterm : UInt64;
  __imp__initterm_e : UInt64;
  __imp__invalid_parameter_noinfo : UInt64;
  __imp__invalid_parameter_noinfo_noreturn : UInt64;
  __imp__invoke_watson : UInt64;
  __imp__query_app_type : UInt64;
  __imp__register_onexit_function : UInt64;
  __imp__register_thread_local_exe_atexit_callback : UInt64;
  __imp__resetstkoflw : UInt64;
  __imp__seh_filter_dll : UInt64;
  __imp__seh_filter_exe : UInt64;
  __imp__set_abort_behavior : UInt64;
  __imp___set_app_type : UInt64;
  __imp__set_app_type : UInt64;
  __imp__set_controlfp : UInt64;
  __imp__set_doserrno : UInt64;
  __imp__set_errno : UInt64;
  __imp__set_error_mode : UInt64;
  __imp__set_invalid_parameter_handler : UInt64;
  __imp__set_new_handler : UInt64;
  __imp__set_thread_local_invalid_parameter_handler : UInt64;
  __imp__seterrormode : UInt64;
  __imp__sleep : UInt64;
  __imp__statusfp : UInt64;
  __imp__strerror : UInt64;
  __imp__strerror_s : UInt64;
  __imp__wassert : UInt64;
  __imp__wcserror : UInt64;
  __imp__wcserror_s : UInt64;
  __imp__wperror : UInt64;
  __imp__wsystem : UInt64;
  __imp_abort : UInt64;
  __imp_exit : UInt64;
  __imp_feclearexcept : UInt64;
  __imp_fegetenv : UInt64;
  __imp_fegetexceptflag : UInt64;
  __imp_fegetround : UInt64;
  __imp_feholdexcept : UInt64;
  __imp_fesetenv : UInt64;
  __imp_fesetexceptflag : UInt64;
  __imp_fesetround : UInt64;
  __imp_fetestexcept : UInt64;
  __imp_perror : UInt64;
  __imp_quick_exit : UInt64;
  __imp_raise : UInt64;
  __imp_set_terminate : UInt64;
  __imp_signal : UInt64;
  __imp_strerror : UInt64;
  __imp_strerror_s : UInt64;
  __imp_system : UInt64;
  __imp_terminate : UInt64;
  _head_lib64_libapi_ms_win_crt_runtime_l1_1_0_a : UInt64;
  __lib64_libapi_ms_win_crt_runtime_l1_1_0_a_iname : UInt64;
  __imp__beep : UInt64;
  __imp__cwait : UInt64;
  __imp_execl : UInt64;
  __imp__execl : UInt64;
  __imp_cwait : UInt64;
  __imp__execle : UInt64;
  __imp_execle : UInt64;
  __imp__execlp : UInt64;
  __imp_execlpe : UInt64;
  __imp__execlpe : UInt64;
  __imp_execlp : UInt64;
  __imp__execv : UInt64;
  __imp_execv : UInt64;
  __imp__execve : UInt64;
  __imp_execve : UInt64;
  __imp__execvp : UInt64;
  __imp_execvpe : UInt64;
  __imp__execvpe : UInt64;
  __imp__loaddll : UInt64;
  __imp_execvp : UInt64;
  __imp__spawnl : UInt64;
  __imp_spawnl : UInt64;
  __imp__spawnle : UInt64;
  __imp__spawnlp : UInt64;
  __imp_spawnlpe : UInt64;
  __imp_spawnle : UInt64;
  __imp__spawnlpe : UInt64;
  __imp_spawnlp : UInt64;
  __imp__spawnv : UInt64;
  __imp_spawnve : UInt64;
  __imp__spawnve : UInt64;
  __imp_spawnvp : UInt64;
  __imp__spawnvp : UInt64;
  __imp_spawnv : UInt64;
  __imp__spawnvpe : UInt64;
  __imp_spawnvpe : UInt64;
  __imp__unloaddll : UInt64;
  __imp__wexecl : UInt64;
  __imp__wexecle : UInt64;
  __imp__wexeclp : UInt64;
  __imp__wexeclpe : UInt64;
  __imp__wexecv : UInt64;
  __imp__wexecve : UInt64;
  __imp__wexecvp : UInt64;
  __imp__wexecvpe : UInt64;
  __imp__wspawnl : UInt64;
  __imp__wspawnle : UInt64;
  __imp__wspawnlp : UInt64;
  __imp__wspawnlpe : UInt64;
  __imp__wspawnv : UInt64;
  __imp__wspawnve : UInt64;
  __imp__wspawnvp : UInt64;
  __imp__wspawnvpe : UInt64;
  _head_lib64_libapi_ms_win_crt_process_l1_1_0_a : UInt64;
  __lib64_libapi_ms_win_crt_process_l1_1_0_a_iname : UInt64;
  __imp__CreateFrameInfo : UInt64;
  __imp__CxxThrowException : UInt64;
  __imp__FindAndUnlinkFrame : UInt64;
  __imp__GetImageBase : UInt64;
  __imp__GetThrowImageBase : UInt64;
  __imp__IsExceptionObjectToBeDestroyed : UInt64;
  __imp__NLG_Dispatch2 : UInt64;
  __imp__NLG_Return : UInt64;
  __imp__NLG_Return2 : UInt64;
  __imp__SetImageBase : UInt64;
  __imp__SetThrowImageBase : UInt64;
  __imp__SetWinRTOutOfMemoryExceptionCallback : UInt64;
  __imp___AdjustPointer : UInt64;
  __imp___BuildCatchObject : UInt64;
  __imp___BuildCatchObjectHelper : UInt64;
  __imp___C_specific_handler : UInt64;
  __imp___CxxDetectRethrow : UInt64;
  __imp___CxxExceptionFilter : UInt64;
  __imp___CxxFrameHandler : UInt64;
  __imp___CxxFrameHandler2 : UInt64;
  __imp___CxxFrameHandler3 : UInt64;
  __imp___CxxQueryExceptionSize : UInt64;
  __imp___CxxRegisterExceptionObject : UInt64;
  __imp___CxxUnregisterExceptionObject : UInt64;
  __imp___DestructExceptionObject : UInt64;
  __imp___FrameUnwindFilter : UInt64;
  __imp___GetPlatformExceptionInfo : UInt64;
  __imp___NLG_Dispatch2 : UInt64;
  __imp___NLG_Return2 : UInt64;
  __imp___RTCastToVoid : UInt64;
  __imp___RTDynamicCast : UInt64;
  __imp___RTtypeid : UInt64;
  __imp___TypeMatch : UInt64;
  __imp___current_exception : UInt64;
  __imp___current_exception_context : UInt64;
  __imp___dcrt_get_wide_environment_from_os : UInt64;
  __imp___dcrt_initial_narrow_environment : UInt64;
  __imp___intrinsic_abnormal_termination : UInt64;
  __imp___intrinsic_setjmp : UInt64;
  __imp___intrinsic_setjmpex : UInt64;
  __imp___processing_throw : UInt64;
  __imp___report_gsfailure : UInt64;
  __imp___std_exception_copy : UInt64;
  __imp___std_exception_destroy : UInt64;
  __imp___std_type_info_compare : UInt64;
  __imp___std_type_info_destroy_list : UInt64;
  __imp___std_type_info_hash : UInt64;
  __imp___std_type_info_name : UInt64;
  __imp___unDName : UInt64;
  __imp___unDNameEx : UInt64;
  __imp___uncaught_exception : UInt64;
  __imp__get_purecall_handler : UInt64;
  __imp__get_unexpected : UInt64;
  __imp__is_exception_typeof : UInt64;
  __imp__local_unwind : UInt64;
  __imp__o__CIacos : UInt64;
  __imp__o__CIasin : UInt64;
  __imp__o__CIatan : UInt64;
  __imp__o__CIatan2 : UInt64;
  __imp__o__CIcos : UInt64;
  __imp__o__CIcosh : UInt64;
  __imp__o__CIexp : UInt64;
  __imp__o__CIfmod : UInt64;
  __imp__o__CIlog : UInt64;
  __imp__o__CIlog10 : UInt64;
  __imp__o__CIpow : UInt64;
  __imp__o__CIsin : UInt64;
  __imp__o__CIsinh : UInt64;
  __imp__o__CIsqrt : UInt64;
  __imp__o__CItan : UInt64;
  __imp__o__CItanh : UInt64;
  __imp__o__Getdays : UInt64;
  __imp__o__Getmonths : UInt64;
  __imp__o__Gettnames : UInt64;
  __imp__o__Strftime : UInt64;
  __imp__o__W_Getdays : UInt64;
  __imp__o__W_Getmonths : UInt64;
  __imp__o__W_Gettnames : UInt64;
  __imp__o__Wcsftime : UInt64;
  __imp__o___acrt_iob_func : UInt64;
  __imp__o___conio_common_vcprintf : UInt64;
  __imp__o___conio_common_vcprintf_p : UInt64;
  __imp__o___conio_common_vcprintf_s : UInt64;
  __imp__o___conio_common_vcscanf : UInt64;
  __imp__o___conio_common_vcwprintf : UInt64;
  __imp__o___conio_common_vcwprintf_p : UInt64;
  __imp__o___conio_common_vcwprintf_s : UInt64;
  __imp__o___conio_common_vcwscanf : UInt64;
  __imp__o___daylight : UInt64;
  __imp__o___dstbias : UInt64;
  __imp__o___fpe_flt_rounds : UInt64;
  __imp__o___libm_sse2_acos : UInt64;
  __imp__o___libm_sse2_acosf : UInt64;
  __imp__o___libm_sse2_asin : UInt64;
  __imp__o___libm_sse2_asinf : UInt64;
  __imp__o___libm_sse2_atan : UInt64;
  __imp__o___libm_sse2_atan2 : UInt64;
  __imp__o___libm_sse2_atanf : UInt64;
  __imp__o___libm_sse2_cos : UInt64;
  __imp__o___libm_sse2_cosf : UInt64;
  __imp__o___libm_sse2_exp : UInt64;
  __imp__o___libm_sse2_expf : UInt64;
  __imp__o___libm_sse2_log : UInt64;
  __imp__o___libm_sse2_log10 : UInt64;
  __imp__o___libm_sse2_log10f : UInt64;
  __imp__o___libm_sse2_logf : UInt64;
  __imp__o___libm_sse2_pow : UInt64;
  __imp__o___libm_sse2_powf : UInt64;
  __imp__o___libm_sse2_sin : UInt64;
  __imp__o___libm_sse2_sinf : UInt64;
  __imp__o___libm_sse2_tan : UInt64;
  __imp__o___libm_sse2_tanf : UInt64;
  __imp__o___p___argc : UInt64;
  __imp__o___p___argv : UInt64;
  __imp__o___p___wargv : UInt64;
  __imp__o___p__acmdln : UInt64;
  __imp__o___p__commode : UInt64;
  __imp__o___p__environ : UInt64;
  __imp__o___p__fmode : UInt64;
  __imp__o___p__mbcasemap : UInt64;
  __imp__o___p__mbctype : UInt64;
  __imp__o___p__pgmptr : UInt64;
  __imp__o___p__wcmdln : UInt64;
  __imp__o___p__wenviron : UInt64;
  __imp__o___p__wpgmptr : UInt64;
  __imp__o___pctype_func : UInt64;
  __imp__o___pwctype_func : UInt64;
  __imp__o___stdio_common_vfprintf : UInt64;
  __imp__o___stdio_common_vfprintf_p : UInt64;
  __imp__o___stdio_common_vfprintf_s : UInt64;
  __imp__o___stdio_common_vfscanf : UInt64;
  __imp__o___stdio_common_vfwprintf : UInt64;
  __imp__o___stdio_common_vfwprintf_p : UInt64;
  __imp__o___stdio_common_vfwprintf_s : UInt64;
  __imp__o___stdio_common_vfwscanf : UInt64;
  __imp__o___stdio_common_vsnprintf_s : UInt64;
  __imp__o___stdio_common_vsnwprintf_s : UInt64;
  __imp__o___stdio_common_vsprintf : UInt64;
  __imp__o___stdio_common_vsprintf_p : UInt64;
  __imp__o___stdio_common_vsprintf_s : UInt64;
  __imp__o___stdio_common_vsscanf : UInt64;
  __imp__o___stdio_common_vswprintf : UInt64;
  __imp__o___stdio_common_vswprintf_p : UInt64;
  __imp__o___stdio_common_vswprintf_s : UInt64;
  __imp__o___stdio_common_vswscanf : UInt64;
  __imp__o___timezone : UInt64;
  __imp__o___tzname : UInt64;
  __imp__o___wcserror : UInt64;
  __imp__o__access : UInt64;
  __imp__o__access_s : UInt64;
  __imp__o__aligned_free : UInt64;
  __imp__o__aligned_malloc : UInt64;
  __imp__o__aligned_msize : UInt64;
  __imp__o__aligned_offset_malloc : UInt64;
  __imp__o__aligned_offset_realloc : UInt64;
  __imp__o__aligned_offset_recalloc : UInt64;
  __imp__o__aligned_realloc : UInt64;
  __imp__o__aligned_recalloc : UInt64;
  __imp__o__atodbl : UInt64;
  __imp__o__atodbl_l : UInt64;
  __imp__o__atof_l : UInt64;
  __imp__o__atoflt : UInt64;
  __imp__o__atoflt_l : UInt64;
  __imp__o__atoi64 : UInt64;
  __imp__o__atoi64_l : UInt64;
  __imp__o__atoi_l : UInt64;
  __imp__o__atol_l : UInt64;
  __imp__o__atoldbl : UInt64;
  __imp__o__atoldbl_l : UInt64;
  __imp__o__atoll_l : UInt64;
  __imp__o__beep : UInt64;
  __imp__o__beginthread : UInt64;
  __imp__o__beginthreadex : UInt64;
  __imp__o__cabs : UInt64;
  __imp__o__callnewh : UInt64;
  __imp__o__calloc_base : UInt64;
  __imp__o__cgets : UInt64;
  __imp__o__cgets_s : UInt64;
  __imp__o__cgetws : UInt64;
  __imp__o__cgetws_s : UInt64;
  __imp__o__chdir : UInt64;
  __imp__o__chdrive : UInt64;
  __imp__o__chmod : UInt64;
  __imp__o__chsize : UInt64;
  __imp__o__chsize_s : UInt64;
  __imp__o__close : UInt64;
  __imp__o__commit : UInt64;
  __imp__o__configure_wide_argv : UInt64;
  __imp__o__cputs : UInt64;
  __imp__o__cputws : UInt64;
  __imp__o__creat : UInt64;
  __imp__o__create_locale : UInt64;
  __imp__o__ctime32_s : UInt64;
  __imp__o__ctime64_s : UInt64;
  __imp__o__cwait : UInt64;
  __imp__o__d_int : UInt64;
  __imp__o__dclass : UInt64;
  __imp__o__difftime32 : UInt64;
  __imp__o__difftime64 : UInt64;
  __imp__o__dlog : UInt64;
  __imp__o__dnorm : UInt64;
  __imp__o__dpcomp : UInt64;
  __imp__o__dpoly : UInt64;
  __imp__o__dscale : UInt64;
  __imp__o__dsign : UInt64;
  __imp__o__dsin : UInt64;
  __imp__o__dtest : UInt64;
  __imp__o__dunscale : UInt64;
  __imp__o__dup : UInt64;
  __imp__o__dup2 : UInt64;
  __imp__o__dupenv_s : UInt64;
  __imp__o__ecvt : UInt64;
  __imp__o__ecvt_s : UInt64;
  __imp__o__endthread : UInt64;
  __imp__o__endthreadex : UInt64;
  __imp__o__eof : UInt64;
  __imp__o__errno : UInt64;
  __imp__o__except1 : UInt64;
  __imp__o__execute_onexit_table : UInt64;
  __imp__o__execv : UInt64;
  __imp__o__execve : UInt64;
  __imp__o__execvp : UInt64;
  __imp__o__execvpe : UInt64;
  __imp__o__expand : UInt64;
  __imp__o__fclose_nolock : UInt64;
  __imp__o__fcloseall : UInt64;
  __imp__o__fcvt : UInt64;
  __imp__o__fcvt_s : UInt64;
  __imp__o__fd_int : UInt64;
  __imp__o__fdclass : UInt64;
  __imp__o__fdexp : UInt64;
  __imp__o__fdlog : UInt64;
  __imp__o__fdopen : UInt64;
  __imp__o__fdpcomp : UInt64;
  __imp__o__fdpoly : UInt64;
  __imp__o__fdscale : UInt64;
  __imp__o__fdsign : UInt64;
  __imp__o__fdsin : UInt64;
  __imp__o__fflush_nolock : UInt64;
  __imp__o__fgetc_nolock : UInt64;
  __imp__o__fgetchar : UInt64;
  __imp__o__fgetwc_nolock : UInt64;
  __imp__o__fgetwchar : UInt64;
  __imp__o__filelength : UInt64;
  __imp__o__filelengthi64 : UInt64;
  __imp__o__fileno : UInt64;
  __imp__o__findclose : UInt64;
  __imp__o__findfirst32 : UInt64;
  __imp__o__findfirst32i64 : UInt64;
  __imp__o__findfirst64 : UInt64;
  __imp__o__findfirst64i32 : UInt64;
  __imp__o__findnext32 : UInt64;
  __imp__o__findnext32i64 : UInt64;
  __imp__o__findnext64 : UInt64;
  __imp__o__findnext64i32 : UInt64;
  __imp__o__flushall : UInt64;
  __imp__o__fpclass : UInt64;
  __imp__o__fpclassf : UInt64;
  __imp__o__fputc_nolock : UInt64;
  __imp__o__fputchar : UInt64;
  __imp__o__fputwc_nolock : UInt64;
  __imp__o__fputwchar : UInt64;
  __imp__o__fread_nolock : UInt64;
  __imp__o__fread_nolock_s : UInt64;
  __imp__o__free_base : UInt64;
  __imp__o__free_locale : UInt64;
  __imp__o__fseek_nolock : UInt64;
  __imp__o__fseeki64 : UInt64;
  __imp__o__fseeki64_nolock : UInt64;
  __imp__o__fsopen : UInt64;
  __imp__o__fstat32 : UInt64;
  __imp__o__fstat32i64 : UInt64;
  __imp__o__fstat64 : UInt64;
  __imp__o__fstat64i32 : UInt64;
  __imp__o__ftell_nolock : UInt64;
  __imp__o__ftelli64 : UInt64;
  __imp__o__ftelli64_nolock : UInt64;
  __imp__o__ftime32 : UInt64;
  __imp__o__ftime32_s : UInt64;
  __imp__o__ftime64 : UInt64;
  __imp__o__ftime64_s : UInt64;
  __imp__o__fullpath : UInt64;
  __imp__o__futime32 : UInt64;
  __imp__o__futime64 : UInt64;
  __imp__o__fwrite_nolock : UInt64;
  __imp__o__gcvt : UInt64;
  __imp__o__gcvt_s : UInt64;
  __imp__o__get_daylight : UInt64;
  __imp__o__get_doserrno : UInt64;
  __imp__o__get_dstbias : UInt64;
  __imp__o__get_errno : UInt64;
  __imp__o__get_fmode : UInt64;
  __imp__o__get_heap_handle : UInt64;
  __imp__o__get_invalid_parameter_handler : UInt64;
  __imp__o__get_narrow_winmain_command_line : UInt64;
  __imp__o__get_osfhandle : UInt64;
  __imp__o__get_pgmptr : UInt64;
  __imp__o__get_stream_buffer_pointers : UInt64;
  __imp__o__get_terminate : UInt64;
  __imp__o__get_thread_local_invalid_parameter_handler : UInt64;
  __imp__o__get_timezone : UInt64;
  __imp__o__get_tzname : UInt64;
  __imp__o__get_wide_winmain_command_line : UInt64;
  __imp__o__get_wpgmptr : UInt64;
  __imp__o__getc_nolock : UInt64;
  __imp__o__getch : UInt64;
  __imp__o__getch_nolock : UInt64;
  __imp__o__getche : UInt64;
  __imp__o__getche_nolock : UInt64;
  __imp__o__getcwd : UInt64;
  __imp__o__getdcwd : UInt64;
  __imp__o__getdiskfree : UInt64;
  __imp__o__getdllprocaddr : UInt64;
  __imp__o__getdrive : UInt64;
  __imp__o__getdrives : UInt64;
  __imp__o__getmbcp : UInt64;
  __imp__o__getsystime : UInt64;
  __imp__o__getw : UInt64;
  __imp__o__getwc_nolock : UInt64;
  __imp__o__getwch : UInt64;
  __imp__o__getwch_nolock : UInt64;
  __imp__o__getwche : UInt64;
  __imp__o__getwche_nolock : UInt64;
  __imp__o__getws : UInt64;
  __imp__o__getws_s : UInt64;
  __imp__o__gmtime32 : UInt64;
  __imp__o__gmtime32_s : UInt64;
  __imp__o__gmtime64 : UInt64;
  __imp__o__gmtime64_s : UInt64;
  __imp__o__heapchk : UInt64;
  __imp__o__heapmin : UInt64;
  __imp__o__hypot : UInt64;
  __imp__o__hypotf : UInt64;
  __imp__o__i64toa : UInt64;
  __imp__o__i64toa_s : UInt64;
  __imp__o__i64tow : UInt64;
  __imp__o__i64tow_s : UInt64;
  __imp__o__initialize_onexit_table : UInt64;
  __imp__o__invalid_parameter_noinfo : UInt64;
  __imp__o__invalid_parameter_noinfo_noreturn : UInt64;
  __imp__o__isatty : UInt64;
  __imp__o__isctype : UInt64;
  __imp__o__isctype_l : UInt64;
  __imp__o__isleadbyte_l : UInt64;
  __imp__o__ismbbalnum : UInt64;
  __imp__o__ismbbalnum_l : UInt64;
  __imp__o__ismbbalpha : UInt64;
  __imp__o__ismbbalpha_l : UInt64;
  __imp__o__ismbbblank : UInt64;
  __imp__o__ismbbblank_l : UInt64;
  __imp__o__ismbbgraph : UInt64;
  __imp__o__ismbbgraph_l : UInt64;
  __imp__o__ismbbkalnum : UInt64;
  __imp__o__ismbbkalnum_l : UInt64;
  __imp__o__ismbbkana : UInt64;
  __imp__o__ismbbkana_l : UInt64;
  __imp__o__ismbbkprint : UInt64;
  __imp__o__ismbbkprint_l : UInt64;
  __imp__o__ismbbkpunct : UInt64;
  __imp__o__ismbbkpunct_l : UInt64;
  __imp__o__ismbblead : UInt64;
  __imp__o__ismbblead_l : UInt64;
  __imp__o__ismbbprint : UInt64;
  __imp__o__ismbbprint_l : UInt64;
  __imp__o__ismbbpunct : UInt64;
  __imp__o__ismbbpunct_l : UInt64;
  __imp__o__ismbbtrail : UInt64;
  __imp__o__ismbbtrail_l : UInt64;
  __imp__o__ismbcalnum : UInt64;
  __imp__o__ismbcalnum_l : UInt64;
  __imp__o__ismbcalpha : UInt64;
  __imp__o__ismbcalpha_l : UInt64;
  __imp__o__ismbcblank : UInt64;
  __imp__o__ismbcblank_l : UInt64;
  __imp__o__ismbcdigit : UInt64;
  __imp__o__ismbcdigit_l : UInt64;
  __imp__o__ismbcgraph : UInt64;
  __imp__o__ismbcgraph_l : UInt64;
  __imp__o__ismbchira : UInt64;
  __imp__o__ismbchira_l : UInt64;
  __imp__o__ismbckata : UInt64;
  __imp__o__ismbckata_l : UInt64;
  __imp__o__ismbcl0 : UInt64;
  __imp__o__ismbcl0_l : UInt64;
  __imp__o__ismbcl1 : UInt64;
  __imp__o__ismbcl1_l : UInt64;
  __imp__o__ismbcl2 : UInt64;
  __imp__o__ismbcl2_l : UInt64;
  __imp__o__ismbclegal : UInt64;
  __imp__o__ismbclegal_l : UInt64;
  __imp__o__ismbclower : UInt64;
  __imp__o__ismbclower_l : UInt64;
  __imp__o__ismbcprint : UInt64;
  __imp__o__ismbcprint_l : UInt64;
  __imp__o__ismbcpunct : UInt64;
  __imp__o__ismbcpunct_l : UInt64;
  __imp__o__ismbcspace : UInt64;
  __imp__o__ismbcspace_l : UInt64;
  __imp__o__ismbcsymbol : UInt64;
  __imp__o__ismbcsymbol_l : UInt64;
  __imp__o__ismbcupper : UInt64;
  __imp__o__ismbcupper_l : UInt64;
  __imp__o__ismbslead : UInt64;
  __imp__o__ismbslead_l : UInt64;
  __imp__o__ismbstrail : UInt64;
  __imp__o__ismbstrail_l : UInt64;
  __imp__o__iswctype_l : UInt64;
  __imp__o__itoa : UInt64;
  __imp__o__itoa_s : UInt64;
  __imp__o__itow : UInt64;
  __imp__o__itow_s : UInt64;
  __imp__o__j0 : UInt64;
  __imp__o__j1 : UInt64;
  __imp__o__jn : UInt64;
  __imp__o__kbhit : UInt64;
  __imp__o__ld_int : UInt64;
  __imp__o__ldclass : UInt64;
  __imp__o__ldexp : UInt64;
  __imp__o__ldlog : UInt64;
  __imp__o__ldpcomp : UInt64;
  __imp__o__ldpoly : UInt64;
  __imp__o__ldscale : UInt64;
  __imp__o__ldsign : UInt64;
  __imp__o__ldsin : UInt64;
  __imp__o__ldtest : UInt64;
  __imp__o__ldunscale : UInt64;
  __imp__o__lfind : UInt64;
  __imp__o__lfind_s : UInt64;
  __imp__o__libm_sse2_acos_precise : UInt64;
  __imp__o__libm_sse2_asin_precise : UInt64;
  __imp__o__libm_sse2_atan_precise : UInt64;
  __imp__o__libm_sse2_cos_precise : UInt64;
  __imp__o__libm_sse2_exp_precise : UInt64;
  __imp__o__libm_sse2_log10_precise : UInt64;
  __imp__o__libm_sse2_log_precise : UInt64;
  __imp__o__libm_sse2_pow_precise : UInt64;
  __imp__o__libm_sse2_sin_precise : UInt64;
  __imp__o__libm_sse2_sqrt_precise : UInt64;
  __imp__o__libm_sse2_tan_precise : UInt64;
  __imp__o__loaddll : UInt64;
  __imp__o__localtime32 : UInt64;
  __imp__o__localtime32_s : UInt64;
  __imp__o__localtime64 : UInt64;
  __imp__o__localtime64_s : UInt64;
  __imp__o__lock_file : UInt64;
  __imp__o__locking : UInt64;
  __imp__o__logb : UInt64;
  __imp__o__logbf : UInt64;
  __imp__o__lsearch : UInt64;
  __imp__o__lsearch_s : UInt64;
  __imp__o__lseek : UInt64;
  __imp__o__lseeki64 : UInt64;
  __imp__o__ltoa : UInt64;
  __imp__o__ltoa_s : UInt64;
  __imp__o__ltow : UInt64;
  __imp__o__ltow_s : UInt64;
  __imp__o__makepath : UInt64;
  __imp__o__makepath_s : UInt64;
  __imp__o__malloc_base : UInt64;
  __imp__o__mbbtombc : UInt64;
  __imp__o__mbbtombc_l : UInt64;
  __imp__o__mbbtype : UInt64;
  __imp__o__mbbtype_l : UInt64;
  __imp__o__mbccpy : UInt64;
  __imp__o__mbccpy_l : UInt64;
  __imp__o__mbccpy_s : UInt64;
  __imp__o__mbccpy_s_l : UInt64;
  __imp__o__mbcjistojms : UInt64;
  __imp__o__mbcjistojms_l : UInt64;
  __imp__o__mbcjmstojis : UInt64;
  __imp__o__mbcjmstojis_l : UInt64;
  __imp__o__mbclen : UInt64;
  __imp__o__mbclen_l : UInt64;
  __imp__o__mbctohira : UInt64;
  __imp__o__mbctohira_l : UInt64;
  __imp__o__mbctokata : UInt64;
  __imp__o__mbctokata_l : UInt64;
  __imp__o__mbctolower : UInt64;
  __imp__o__mbctolower_l : UInt64;
  __imp__o__mbctombb : UInt64;
  __imp__o__mbctombb_l : UInt64;
  __imp__o__mbctoupper : UInt64;
  __imp__o__mbctoupper_l : UInt64;
  __imp__o__mblen_l : UInt64;
  __imp__o__mbsbtype : UInt64;
  __imp__o__mbsbtype_l : UInt64;
  __imp__o__mbscat_s : UInt64;
  __imp__o__mbscat_s_l : UInt64;
  __imp__o__mbschr : UInt64;
  __imp__o__mbschr_l : UInt64;
  __imp__o__mbscmp : UInt64;
  __imp__o__mbscmp_l : UInt64;
  __imp__o__mbscoll : UInt64;
  __imp__o__mbscoll_l : UInt64;
  __imp__o__mbscpy_s : UInt64;
  __imp__o__mbscpy_s_l : UInt64;
  __imp__o__mbscspn : UInt64;
  __imp__o__mbscspn_l : UInt64;
  __imp__o__mbsdec : UInt64;
  __imp__o__mbsdec_l : UInt64;
  __imp__o__mbsicmp : UInt64;
  __imp__o__mbsicmp_l : UInt64;
  __imp__o__mbsicoll : UInt64;
  __imp__o__mbsicoll_l : UInt64;
  __imp__o__mbsinc : UInt64;
  __imp__o__mbsinc_l : UInt64;
  __imp__o__mbslen : UInt64;
  __imp__o__mbslen_l : UInt64;
  __imp__o__mbslwr : UInt64;
  __imp__o__mbslwr_l : UInt64;
  __imp__o__mbslwr_s : UInt64;
  __imp__o__mbslwr_s_l : UInt64;
  __imp__o__mbsnbcat : UInt64;
  __imp__o__mbsnbcat_l : UInt64;
  __imp__o__mbsnbcat_s : UInt64;
  __imp__o__mbsnbcat_s_l : UInt64;
  __imp__o__mbsnbcmp : UInt64;
  __imp__o__mbsnbcmp_l : UInt64;
  __imp__o__mbsnbcnt : UInt64;
  __imp__o__mbsnbcnt_l : UInt64;
  __imp__o__mbsnbcoll : UInt64;
  __imp__o__mbsnbcoll_l : UInt64;
  __imp__o__mbsnbcpy : UInt64;
  __imp__o__mbsnbcpy_l : UInt64;
  __imp__o__mbsnbcpy_s : UInt64;
  __imp__o__mbsnbcpy_s_l : UInt64;
  __imp__o__mbsnbicmp : UInt64;
  __imp__o__mbsnbicmp_l : UInt64;
  __imp__o__mbsnbicoll : UInt64;
  __imp__o__mbsnbicoll_l : UInt64;
  __imp__o__mbsnbset : UInt64;
  __imp__o__mbsnbset_l : UInt64;
  __imp__o__mbsnbset_s : UInt64;
  __imp__o__mbsnbset_s_l : UInt64;
  __imp__o__mbsncat : UInt64;
  __imp__o__mbsncat_l : UInt64;
  __imp__o__mbsncat_s : UInt64;
  __imp__o__mbsncat_s_l : UInt64;
  __imp__o__mbsnccnt : UInt64;
  __imp__o__mbsnccnt_l : UInt64;
  __imp__o__mbsncmp : UInt64;
  __imp__o__mbsncmp_l : UInt64;
  __imp__o__mbsncoll : UInt64;
  __imp__o__mbsncoll_l : UInt64;
  __imp__o__mbsncpy : UInt64;
  __imp__o__mbsncpy_l : UInt64;
  __imp__o__mbsncpy_s : UInt64;
  __imp__o__mbsncpy_s_l : UInt64;
  __imp__o__mbsnextc : UInt64;
  __imp__o__mbsnextc_l : UInt64;
  __imp__o__mbsnicmp : UInt64;
  __imp__o__mbsnicmp_l : UInt64;
  __imp__o__mbsnicoll : UInt64;
  __imp__o__mbsnicoll_l : UInt64;
  __imp__o__mbsninc : UInt64;
  __imp__o__mbsninc_l : UInt64;
  __imp__o__mbsnlen : UInt64;
  __imp__o__mbsnlen_l : UInt64;
  __imp__o__mbsnset : UInt64;
  __imp__o__mbsnset_l : UInt64;
  __imp__o__mbsnset_s : UInt64;
  __imp__o__mbsnset_s_l : UInt64;
  __imp__o__mbspbrk : UInt64;
  __imp__o__mbspbrk_l : UInt64;
  __imp__o__mbsrchr : UInt64;
  __imp__o__mbsrchr_l : UInt64;
  __imp__o__mbsrev : UInt64;
  __imp__o__mbsrev_l : UInt64;
  __imp__o__mbsset : UInt64;
  __imp__o__mbsset_l : UInt64;
  __imp__o__mbsset_s : UInt64;
  __imp__o__mbsset_s_l : UInt64;
  __imp__o__mbsspn : UInt64;
  __imp__o__mbsspn_l : UInt64;
  __imp__o__mbsspnp : UInt64;
  __imp__o__mbsspnp_l : UInt64;
  __imp__o__mbsstr : UInt64;
  __imp__o__mbsstr_l : UInt64;
  __imp__o__mbstok : UInt64;
  __imp__o__mbstok_l : UInt64;
  __imp__o__mbstok_s : UInt64;
  __imp__o__mbstok_s_l : UInt64;
  __imp__o__mbstowcs_l : UInt64;
  __imp__o__mbstowcs_s_l : UInt64;
  __imp__o__mbstrlen : UInt64;
  __imp__o__mbstrlen_l : UInt64;
  __imp__o__mbstrnlen : UInt64;
  __imp__o__mbstrnlen_l : UInt64;
  __imp__o__mbsupr : UInt64;
  __imp__o__mbsupr_l : UInt64;
  __imp__o__mbsupr_s : UInt64;
  __imp__o__mbsupr_s_l : UInt64;
  __imp__o__mbtowc_l : UInt64;
  __imp__o__memicmp : UInt64;
  __imp__o__memicmp_l : UInt64;
  __imp__o__mkdir : UInt64;
  __imp__o__mkgmtime32 : UInt64;
  __imp__o__mkgmtime64 : UInt64;
  __imp__o__mktemp : UInt64;
  __imp__o__mktemp_s : UInt64;
  __imp__o__mktime32 : UInt64;
  __imp__o__mktime64 : UInt64;
  __imp__o__msize : UInt64;
  __imp__o__nextafter : UInt64;
  __imp__o__nextafterf : UInt64;
  __imp__o__open_osfhandle : UInt64;
  __imp__o__pclose : UInt64;
  __imp__o__pipe : UInt64;
  __imp__o__popen : UInt64;
  __imp__o__putc_nolock : UInt64;
  __imp__o__putch : UInt64;
  __imp__o__putch_nolock : UInt64;
  __imp__o__putenv : UInt64;
  __imp__o__putenv_s : UInt64;
  __imp__o__putw : UInt64;
  __imp__o__putwc_nolock : UInt64;
  __imp__o__putwch : UInt64;
  __imp__o__putwch_nolock : UInt64;
  __imp__o__putws : UInt64;
  __imp__o__read : UInt64;
  __imp__o__realloc_base : UInt64;
  __imp__o__recalloc : UInt64;
  __imp__o__register_onexit_function : UInt64;
  __imp__o__resetstkoflw : UInt64;
  __imp__o__rmdir : UInt64;
  __imp__o__rmtmp : UInt64;
  __imp__o__scalb : UInt64;
  __imp__o__scalbf : UInt64;
  __imp__o__searchenv : UInt64;
  __imp__o__searchenv_s : UInt64;
  __imp__o__set_abort_behavior : UInt64;
  __imp__o__set_doserrno : UInt64;
  __imp__o__set_errno : UInt64;
  __imp__o__set_invalid_parameter_handler : UInt64;
  __imp__o__set_new_handler : UInt64;
  __imp__o__set_new_mode : UInt64;
  __imp__o__set_thread_local_invalid_parameter_handler : UInt64;
  __imp__o__seterrormode : UInt64;
  __imp__o__setmbcp : UInt64;
  __imp__o__setmode : UInt64;
  __imp__o__setsystime : UInt64;
  __imp__o__sleep : UInt64;
  __imp__o__sopen : UInt64;
  __imp__o__sopen_dispatch : UInt64;
  __imp__o__sopen_s : UInt64;
  __imp__o__spawnv : UInt64;
  __imp__o__spawnve : UInt64;
  __imp__o__spawnvp : UInt64;
  __imp__o__spawnvpe : UInt64;
  __imp__o__splitpath : UInt64;
  __imp__o__splitpath_s : UInt64;
  __imp__o__stat32 : UInt64;
  __imp__o__stat32i64 : UInt64;
  __imp__o__stat64 : UInt64;
  __imp__o__stat64i32 : UInt64;
  __imp__o__strcoll_l : UInt64;
  __imp__o__strdate : UInt64;
  __imp__o__strdate_s : UInt64;
  __imp__o__strdup : UInt64;
  __imp__o__strerror : UInt64;
  __imp__o__strerror_s : UInt64;
  __imp__o__strftime_l : UInt64;
  __imp__o__stricmp : UInt64;
  __imp__o__stricmp_l : UInt64;
  __imp__o__stricoll : UInt64;
  __imp__o__stricoll_l : UInt64;
  __imp__o__strlwr : UInt64;
  __imp__o__strlwr_l : UInt64;
  __imp__o__strlwr_s : UInt64;
  __imp__o__strlwr_s_l : UInt64;
  __imp__o__strncoll : UInt64;
  __imp__o__strncoll_l : UInt64;
  __imp__o__strnicmp : UInt64;
  __imp__o__strnicmp_l : UInt64;
  __imp__o__strnicoll : UInt64;
  __imp__o__strnicoll_l : UInt64;
  __imp__o__strnset_s : UInt64;
  __imp__o__strset_s : UInt64;
  __imp__o__strtime : UInt64;
  __imp__o__strtime_s : UInt64;
  __imp__o__strtod_l : UInt64;
  __imp__o__strtof_l : UInt64;
  __imp__o__strtoi64 : UInt64;
  __imp__o__strtoi64_l : UInt64;
  __imp__o__strtol_l : UInt64;
  __imp__o__strtold_l : UInt64;
  __imp__o__strtoll_l : UInt64;
  __imp__o__strtoui64 : UInt64;
  __imp__o__strtoui64_l : UInt64;
  __imp__o__strtoul_l : UInt64;
  __imp__o__strtoull_l : UInt64;
  __imp__o__strupr : UInt64;
  __imp__o__strupr_l : UInt64;
  __imp__o__strupr_s : UInt64;
  __imp__o__strupr_s_l : UInt64;
  __imp__o__strxfrm_l : UInt64;
  __imp__o__swab : UInt64;
  __imp__o__tell : UInt64;
  __imp__o__telli64 : UInt64;
  __imp__o__timespec32_get : UInt64;
  __imp__o__timespec64_get : UInt64;
  __imp__o__tolower : UInt64;
  __imp__o__tolower_l : UInt64;
  __imp__o__toupper : UInt64;
  __imp__o__toupper_l : UInt64;
  __imp__o__towlower_l : UInt64;
  __imp__o__towupper_l : UInt64;
  __imp__o__tzset : UInt64;
  __imp__o__ui64toa : UInt64;
  __imp__o__ui64toa_s : UInt64;
  __imp__o__ui64tow : UInt64;
  __imp__o__ui64tow_s : UInt64;
  __imp__o__ultoa : UInt64;
  __imp__o__ultoa_s : UInt64;
  __imp__o__ultow : UInt64;
  __imp__o__ultow_s : UInt64;
  __imp__o__umask : UInt64;
  __imp__o__umask_s : UInt64;
  __imp__o__ungetc_nolock : UInt64;
  __imp__o__ungetch : UInt64;
  __imp__o__ungetch_nolock : UInt64;
  __imp__o__ungetwc_nolock : UInt64;
  __imp__o__ungetwch : UInt64;
  __imp__o__ungetwch_nolock : UInt64;
  __imp__o__unlink : UInt64;
  __imp__o__unloaddll : UInt64;
  __imp__o__unlock_file : UInt64;
  __imp__o__utime32 : UInt64;
  __imp__o__utime64 : UInt64;
  __imp__o__waccess : UInt64;
  __imp__o__waccess_s : UInt64;
  __imp__o__wasctime : UInt64;
  __imp__o__wasctime_s : UInt64;
  __imp__o__wchdir : UInt64;
  __imp__o__wchmod : UInt64;
  __imp__o__wcreat : UInt64;
  __imp__o__wcreate_locale : UInt64;
  __imp__o__wcscoll_l : UInt64;
  __imp__o__wcsdup : UInt64;
  __imp__o__wcserror : UInt64;
  __imp__o__wcserror_s : UInt64;
  __imp__o__wcsftime_l : UInt64;
  __imp__o__wcsicmp : UInt64;
  __imp__o__wcsicmp_l : UInt64;
  __imp__o__wcsicoll : UInt64;
  __imp__o__wcsicoll_l : UInt64;
  __imp__o__wcslwr : UInt64;
  __imp__o__wcslwr_l : UInt64;
  __imp__o__wcslwr_s : UInt64;
  __imp__o__wcslwr_s_l : UInt64;
  __imp__o__wcsncoll : UInt64;
  __imp__o__wcsncoll_l : UInt64;
  __imp__o__wcsnicmp : UInt64;
  __imp__o__wcsnicmp_l : UInt64;
  __imp__o__wcsnicoll : UInt64;
  __imp__o__wcsnicoll_l : UInt64;
  __imp__o__wcsnset : UInt64;
  __imp__o__wcsnset_s : UInt64;
  __imp__o__wcsset : UInt64;
  __imp__o__wcsset_s : UInt64;
  __imp__o__wcstod_l : UInt64;
  __imp__o__wcstof_l : UInt64;
  __imp__o__wcstoi64 : UInt64;
  __imp__o__wcstoi64_l : UInt64;
  __imp__o__wcstol_l : UInt64;
  __imp__o__wcstold_l : UInt64;
  __imp__o__wcstoll_l : UInt64;
  __imp__o__wcstombs_l : UInt64;
  __imp__o__wcstombs_s_l : UInt64;
  __imp__o__wcstoui64 : UInt64;
  __imp__o__wcstoui64_l : UInt64;
  __imp__o__wcstoul_l : UInt64;
  __imp__o__wcstoull_l : UInt64;
  __imp__o__wcsupr : UInt64;
  __imp__o__wcsupr_l : UInt64;
  __imp__o__wcsupr_s : UInt64;
  __imp__o__wcsupr_s_l : UInt64;
  __imp__o__wcsxfrm_l : UInt64;
  __imp__o__wctime32 : UInt64;
  __imp__o__wctime32_s : UInt64;
  __imp__o__wctime64 : UInt64;
  __imp__o__wctime64_s : UInt64;
  __imp__o__wctomb_l : UInt64;
  __imp__o__wctomb_s_l : UInt64;
  __imp__o__wdupenv_s : UInt64;
  __imp__o__wexecv : UInt64;
  __imp__o__wexecve : UInt64;
  __imp__o__wexecvp : UInt64;
  __imp__o__wexecvpe : UInt64;
  __imp__o__wfdopen : UInt64;
  __imp__o__wfindfirst32 : UInt64;
  __imp__o__wfindfirst32i64 : UInt64;
  __imp__o__wfindfirst64 : UInt64;
  __imp__o__wfindfirst64i32 : UInt64;
  __imp__o__wfindnext32 : UInt64;
  __imp__o__wfindnext32i64 : UInt64;
  __imp__o__wfindnext64 : UInt64;
  __imp__o__wfindnext64i32 : UInt64;
  __imp__o__wfopen : UInt64;
  __imp__o__wfopen_s : UInt64;
  __imp__o__wfreopen : UInt64;
  __imp__o__wfreopen_s : UInt64;
  __imp__o__wfsopen : UInt64;
  __imp__o__wfullpath : UInt64;
  __imp__o__wgetcwd : UInt64;
  __imp__o__wgetdcwd : UInt64;
  __imp__o__wgetenv : UInt64;
  __imp__o__wgetenv_s : UInt64;
  __imp__o__wmakepath : UInt64;
  __imp__o__wmakepath_s : UInt64;
  __imp__o__wmkdir : UInt64;
  __imp__o__wmktemp : UInt64;
  __imp__o__wmktemp_s : UInt64;
  __imp__o__wperror : UInt64;
  __imp__o__wpopen : UInt64;
  __imp__o__wputenv : UInt64;
  __imp__o__wputenv_s : UInt64;
  __imp__o__wremove : UInt64;
  __imp__o__wrename : UInt64;
  __imp__o__write : UInt64;
  __imp__o__wrmdir : UInt64;
  __imp__o__wsearchenv : UInt64;
  __imp__o__wsearchenv_s : UInt64;
  __imp__o__wsetlocale : UInt64;
  __imp__o__wsopen_dispatch : UInt64;
  __imp__o__wsopen_s : UInt64;
  __imp__o__wspawnv : UInt64;
  __imp__o__wspawnve : UInt64;
  __imp__o__wspawnvp : UInt64;
  __imp__o__wspawnvpe : UInt64;
  __imp__o__wsplitpath : UInt64;
  __imp__o__wsplitpath_s : UInt64;
  __imp__o__wstat32 : UInt64;
  __imp__o__wstat32i64 : UInt64;
  __imp__o__wstat64 : UInt64;
  __imp__o__wstat64i32 : UInt64;
  __imp__o__wstrdate : UInt64;
  __imp__o__wstrdate_s : UInt64;
  __imp__o__wstrtime : UInt64;
  __imp__o__wstrtime_s : UInt64;
  __imp__o__wsystem : UInt64;
  __imp__o__wtmpnam_s : UInt64;
  __imp__o__wtof : UInt64;
  __imp__o__wtof_l : UInt64;
  __imp__o__wtoi : UInt64;
  __imp__o__wtoi64 : UInt64;
  __imp__o__wtoi64_l : UInt64;
  __imp__o__wtoi_l : UInt64;
  __imp__o__wtol : UInt64;
  __imp__o__wtol_l : UInt64;
  __imp__o__wtoll : UInt64;
  __imp__o__wtoll_l : UInt64;
  __imp__o__wunlink : UInt64;
  __imp__o__wutime32 : UInt64;
  __imp__o__wutime64 : UInt64;
  __imp__o__y0 : UInt64;
  __imp__o__y1 : UInt64;
  __imp__o__yn : UInt64;
  __imp__o_abort : UInt64;
  __imp__o_acos : UInt64;
  __imp__o_acosf : UInt64;
  __imp__o_acosh : UInt64;
  __imp__o_acoshf : UInt64;
  __imp__o_acoshl : UInt64;
  __imp__o_asctime : UInt64;
  __imp__o_asctime_s : UInt64;
  __imp__o_asin : UInt64;
  __imp__o_asinf : UInt64;
  __imp__o_asinh : UInt64;
  __imp__o_asinhf : UInt64;
  __imp__o_asinhl : UInt64;
  __imp__o_atan : UInt64;
  __imp__o_atan2 : UInt64;
  __imp__o_atan2f : UInt64;
  __imp__o_atanf : UInt64;
  __imp__o_atanh : UInt64;
  __imp__o_atanhf : UInt64;
  __imp__o_atanhl : UInt64;
  __imp__o_atof : UInt64;
  __imp__o_atoi : UInt64;
  __imp__o_atol : UInt64;
  __imp__o_atoll : UInt64;
  __imp__o_bsearch : UInt64;
  __imp__o_bsearch_s : UInt64;
  __imp__o_btowc : UInt64;
  __imp__o_calloc : UInt64;
  __imp__o_cbrt : UInt64;
  __imp__o_cbrtf : UInt64;
  __imp__o_ceil : UInt64;
  __imp__o_ceilf : UInt64;
  __imp__o_clearerr : UInt64;
  __imp__o_clearerr_s : UInt64;
  __imp__o_cos : UInt64;
  __imp__o_cosf : UInt64;
  __imp__o_cosh : UInt64;
  __imp__o_coshf : UInt64;
  __imp__o_erf : UInt64;
  __imp__o_erfc : UInt64;
  __imp__o_erfcf : UInt64;
  __imp__o_erfcl : UInt64;
  __imp__o_erff : UInt64;
  __imp__o_erfl : UInt64;
  __imp__o_exp : UInt64;
  __imp__o_exp2 : UInt64;
  __imp__o_exp2f : UInt64;
  __imp__o_exp2l : UInt64;
  __imp__o_expf : UInt64;
  __imp__o_fabs : UInt64;
  __imp__o_fclose : UInt64;
  __imp__o_feof : UInt64;
  __imp__o_ferror : UInt64;
  __imp__o_fflush : UInt64;
  __imp__o_fgetc : UInt64;
  __imp__o_fgetpos : UInt64;
  __imp__o_fgets : UInt64;
  __imp__o_fgetwc : UInt64;
  __imp__o_fgetws : UInt64;
  __imp__o_floor : UInt64;
  __imp__o_floorf : UInt64;
  __imp__o_fma : UInt64;
  __imp__o_fmaf : UInt64;
  __imp__o_fmal : UInt64;
  __imp__o_fmod : UInt64;
  __imp__o_fmodf : UInt64;
  __imp__o_fopen : UInt64;
  __imp__o_fopen_s : UInt64;
  __imp__o_fputc : UInt64;
  __imp__o_fputs : UInt64;
  __imp__o_fputwc : UInt64;
  __imp__o_fputws : UInt64;
  __imp__o_fread : UInt64;
  __imp__o_fread_s : UInt64;
  __imp__o_free : UInt64;
  __imp__o_freopen : UInt64;
  __imp__o_freopen_s : UInt64;
  __imp__o_frexp : UInt64;
  __imp__o_fseek : UInt64;
  __imp__o_fsetpos : UInt64;
  __imp__o_ftell : UInt64;
  __imp__o_fwrite : UInt64;
  __imp__o_getc : UInt64;
  __imp__o_getchar : UInt64;
  __imp__o_getenv : UInt64;
  __imp__o_getenv_s : UInt64;
  __imp__o_gets : UInt64;
  __imp__o_gets_s : UInt64;
  __imp__o_getwc : UInt64;
  __imp__o_getwchar : UInt64;
  __imp__o_hypot : UInt64;
  __imp__o_is_wctype : UInt64;
  __imp__o_isalnum : UInt64;
  __imp__o_isalpha : UInt64;
  __imp__o_isblank : UInt64;
  __imp__o_iscntrl : UInt64;
  __imp__o_isdigit : UInt64;
  __imp__o_isgraph : UInt64;
  __imp__o_isleadbyte : UInt64;
  __imp__o_islower : UInt64;
  __imp__o_isprint : UInt64;
  __imp__o_ispunct : UInt64;
  __imp__o_isspace : UInt64;
  __imp__o_isupper : UInt64;
  __imp__o_iswalnum : UInt64;
  __imp__o_iswalpha : UInt64;
  __imp__o_iswascii : UInt64;
  __imp__o_iswblank : UInt64;
  __imp__o_iswcntrl : UInt64;
  __imp__o_iswctype : UInt64;
  __imp__o_iswdigit : UInt64;
  __imp__o_iswgraph : UInt64;
  __imp__o_iswlower : UInt64;
  __imp__o_iswprint : UInt64;
  __imp__o_iswpunct : UInt64;
  __imp__o_iswspace : UInt64;
  __imp__o_iswupper : UInt64;
  __imp__o_iswxdigit : UInt64;
  __imp__o_isxdigit : UInt64;
  __imp__o_ldexp : UInt64;
  __imp__o_lgamma : UInt64;
  __imp__o_lgammaf : UInt64;
  __imp__o_lgammal : UInt64;
  __imp__o_llrint : UInt64;
  __imp__o_llrintf : UInt64;
  __imp__o_llrintl : UInt64;
  __imp__o_llround : UInt64;
  __imp__o_llroundf : UInt64;
  __imp__o_llroundl : UInt64;
  __imp__o_localeconv : UInt64;
  __imp__o_log : UInt64;
  __imp__o_log10 : UInt64;
  __imp__o_log10f : UInt64;
  __imp__o_log1p : UInt64;
  __imp__o_log1pf : UInt64;
  __imp__o_log1pl : UInt64;
  __imp__o_log2 : UInt64;
  __imp__o_log2f : UInt64;
  __imp__o_log2l : UInt64;
  __imp__o_logb : UInt64;
  __imp__o_logbf : UInt64;
  __imp__o_logbl : UInt64;
  __imp__o_logf : UInt64;
  __imp__o_lrint : UInt64;
  __imp__o_lrintf : UInt64;
  __imp__o_lrintl : UInt64;
  __imp__o_lround : UInt64;
  __imp__o_lroundf : UInt64;
  __imp__o_lroundl : UInt64;
  __imp__o_malloc : UInt64;
  __imp__o_mblen : UInt64;
  __imp__o_mbrlen : UInt64;
  __imp__o_mbrtoc16 : UInt64;
  __imp__o_mbrtoc32 : UInt64;
  __imp__o_mbrtowc : UInt64;
  __imp__o_mbsrtowcs : UInt64;
  __imp__o_mbsrtowcs_s : UInt64;
  __imp__o_mbstowcs : UInt64;
  __imp__o_mbstowcs_s : UInt64;
  __imp__o_mbtowc : UInt64;
  __imp__o_memset : UInt64;
  __imp__o_modf : UInt64;
  __imp__o_modff : UInt64;
  __imp__o_nan : UInt64;
  __imp__o_nanf : UInt64;
  __imp__o_nanl : UInt64;
  __imp__o_nearbyint : UInt64;
  __imp__o_nearbyintf : UInt64;
  __imp__o_nearbyintl : UInt64;
  __imp__o_nextafter : UInt64;
  __imp__o_nextafterf : UInt64;
  __imp__o_nextafterl : UInt64;
  __imp__o_nexttoward : UInt64;
  __imp__o_nexttowardf : UInt64;
  __imp__o_nexttowardl : UInt64;
  __imp__o_pow : UInt64;
  __imp__o_powf : UInt64;
  __imp__o_putc : UInt64;
  __imp__o_putchar : UInt64;
  __imp__o_puts : UInt64;
  __imp__o_putwc : UInt64;
  __imp__o_putwchar : UInt64;
  __imp__o_qsort : UInt64;
  __imp__o_qsort_s : UInt64;
  __imp__o_raise : UInt64;
  __imp__o_rand : UInt64;
  __imp__o_rand_s : UInt64;
  __imp__o_realloc : UInt64;
  __imp__o_remainder : UInt64;
  __imp__o_remainderf : UInt64;
  __imp__o_remainderl : UInt64;
  __imp__o_remove : UInt64;
  __imp__o_remquo : UInt64;
  __imp__o_remquof : UInt64;
  __imp__o_remquol : UInt64;
  __imp__o_rewind : UInt64;
  __imp__o_rint : UInt64;
  __imp__o_rintf : UInt64;
  __imp__o_rintl : UInt64;
  __imp__o_round : UInt64;
  __imp__o_roundf : UInt64;
  __imp__o_roundl : UInt64;
  __imp__o_scalbln : UInt64;
  __imp__o_scalblnf : UInt64;
  __imp__o_scalblnl : UInt64;
  __imp__o_scalbn : UInt64;
  __imp__o_scalbnf : UInt64;
  __imp__o_scalbnl : UInt64;
  __imp__o_set_terminate : UInt64;
  __imp__o_setbuf : UInt64;
  __imp__o_setvbuf : UInt64;
  __imp__o_sin : UInt64;
  __imp__o_sinf : UInt64;
  __imp__o_sinh : UInt64;
  __imp__o_sinhf : UInt64;
  __imp__o_sqrt : UInt64;
  __imp__o_sqrtf : UInt64;
  __imp__o_srand : UInt64;
  __imp__o_strcat_s : UInt64;
  __imp__o_strcoll : UInt64;
  __imp__o_strcpy_s : UInt64;
  __imp__o_strerror : UInt64;
  __imp__o_strerror_s : UInt64;
  __imp__o_strftime : UInt64;
  __imp__o_strncat_s : UInt64;
  __imp__o_strncpy_s : UInt64;
  __imp__o_strtod : UInt64;
  __imp__o_strtof : UInt64;
  __imp__o_strtok : UInt64;
  __imp__o_strtok_s : UInt64;
  __imp__o_strtol : UInt64;
  __imp__o_strtold : UInt64;
  __imp__o_strtoll : UInt64;
  __imp__o_strtoul : UInt64;
  __imp__o_strtoull : UInt64;
  __imp__o_system : UInt64;
  __imp__o_tan : UInt64;
  __imp__o_tanf : UInt64;
  __imp__o_tanh : UInt64;
  __imp__o_tanhf : UInt64;
  __imp__o_terminate : UInt64;
  __imp__o_tgamma : UInt64;
  __imp__o_tgammaf : UInt64;
  __imp__o_tgammal : UInt64;
  __imp__o_tmpfile_s : UInt64;
  __imp__o_tmpnam_s : UInt64;
  __imp__o_tolower : UInt64;
  __imp__o_toupper : UInt64;
  __imp__o_towlower : UInt64;
  __imp__o_towupper : UInt64;
  __imp__o_ungetc : UInt64;
  __imp__o_ungetwc : UInt64;
  __imp__o_wcrtomb : UInt64;
  __imp__o_wcrtomb_s : UInt64;
  __imp__o_wcscat_s : UInt64;
  __imp__o_wcscoll : UInt64;
  __imp__o_wcscpy : UInt64;
  __imp__o_wcscpy_s : UInt64;
  __imp__o_wcsftime : UInt64;
  __imp__o_wcsncat_s : UInt64;
  __imp__o_wcsncpy_s : UInt64;
  __imp__o_wcsrtombs : UInt64;
  __imp__o_wcsrtombs_s : UInt64;
  __imp__o_wcstod : UInt64;
  __imp__o_wcstof : UInt64;
  __imp__o_wcstok : UInt64;
  __imp__o_wcstok_s : UInt64;
  __imp__o_wcstol : UInt64;
  __imp__o_wcstold : UInt64;
  __imp__o_wcstoll : UInt64;
  __imp__o_wcstombs : UInt64;
  __imp__o_wcstombs_s : UInt64;
  __imp__o_wcstoul : UInt64;
  __imp__o_wcstoull : UInt64;
  __imp__o_wctob : UInt64;
  __imp__o_wctomb : UInt64;
  __imp__o_wctomb_s : UInt64;
  __imp__o_wmemcpy_s : UInt64;
  __imp__o_wmemmove_s : UInt64;
  __imp__purecall : UInt64;
  __imp__set_purecall_handler : UInt64;
  __imp__set_se_translator : UInt64;
  __imp_longjmp : UInt64;
  __imp_memchr : UInt64;
  __imp_memcmp : UInt64;
  __imp_memcpy : UInt64;
  __imp_memmove : UInt64;
  __imp_set_unexpected : UInt64;
  __imp_setjmp : UInt64;
  __imp_strchr : UInt64;
  __imp_strrchr : UInt64;
  __imp_strstr : UInt64;
  __imp_unexpected : UInt64;
  __imp_wcschr : UInt64;
  __imp_wcsrchr : UInt64;
  __imp_wcsstr : UInt64;
  _head_lib64_libapi_ms_win_crt_private_l1_1_0_a : UInt64;
  __lib64_libapi_ms_win_crt_private_l1_1_0_a_iname : UInt64;
  __imp___p__mbcasemap : UInt64;
  __imp___p__mbctype : UInt64;
  __imp__ismbbalnum : UInt64;
  __imp__ismbbalnum_l : UInt64;
  __imp__ismbbalpha : UInt64;
  __imp__ismbbalpha_l : UInt64;
  __imp__ismbbblank : UInt64;
  __imp__ismbbblank_l : UInt64;
  __imp__ismbbgraph : UInt64;
  __imp__ismbbgraph_l : UInt64;
  __imp__ismbbkalnum : UInt64;
  __imp__ismbbkalnum_l : UInt64;
  __imp__ismbbkana : UInt64;
  __imp__ismbbkana_l : UInt64;
  __imp__ismbbkprint : UInt64;
  __imp__ismbbkprint_l : UInt64;
  __imp__ismbbkpunct : UInt64;
  __imp__ismbbkpunct_l : UInt64;
  __imp__ismbblead : UInt64;
  __imp__ismbblead_l : UInt64;
  __imp__ismbbprint : UInt64;
  __imp__ismbbprint_l : UInt64;
  __imp__ismbbpunct : UInt64;
  __imp__ismbbpunct_l : UInt64;
  __imp__ismbbtrail : UInt64;
  __imp__ismbbtrail_l : UInt64;
  __imp__ismbcalnum : UInt64;
  __imp__ismbcalnum_l : UInt64;
  __imp__ismbcalpha : UInt64;
  __imp__ismbcalpha_l : UInt64;
  __imp__ismbcblank : UInt64;
  __imp__ismbcblank_l : UInt64;
  __imp__ismbcdigit : UInt64;
  __imp__ismbcdigit_l : UInt64;
  __imp__ismbcgraph : UInt64;
  __imp__ismbcgraph_l : UInt64;
  __imp__ismbchira : UInt64;
  __imp__ismbchira_l : UInt64;
  __imp__ismbckata : UInt64;
  __imp__ismbckata_l : UInt64;
  __imp__ismbcl0 : UInt64;
  __imp__ismbcl0_l : UInt64;
  __imp__ismbcl1 : UInt64;
  __imp__ismbcl1_l : UInt64;
  __imp__ismbcl2 : UInt64;
  __imp__ismbcl2_l : UInt64;
  __imp__ismbclegal : UInt64;
  __imp__ismbclegal_l : UInt64;
  __imp__ismbclower : UInt64;
  __imp__ismbclower_l : UInt64;
  __imp__ismbcprint : UInt64;
  __imp__ismbcprint_l : UInt64;
  __imp__ismbcpunct : UInt64;
  __imp__ismbcpunct_l : UInt64;
  __imp__ismbcspace : UInt64;
  __imp__ismbcspace_l : UInt64;
  __imp__ismbcsymbol : UInt64;
  __imp__ismbcsymbol_l : UInt64;
  __imp__ismbcupper : UInt64;
  __imp__ismbcupper_l : UInt64;
  __imp__ismbslead : UInt64;
  __imp__ismbslead_l : UInt64;
  __imp__ismbstrail : UInt64;
  __imp__ismbstrail_l : UInt64;
  __imp__mbbtombc : UInt64;
  __imp__mbbtombc_l : UInt64;
  __imp__mbbtype : UInt64;
  __imp__mbbtype_l : UInt64;
  __imp__mbcasemap : UInt64;
  __imp__mbccpy : UInt64;
  __imp__mbccpy_l : UInt64;
  __imp__mbccpy_s : UInt64;
  __imp__mbccpy_s_l : UInt64;
  __imp__mbcjistojms : UInt64;
  __imp__mbcjistojms_l : UInt64;
  __imp__mbcjmstojis : UInt64;
  __imp__mbcjmstojis_l : UInt64;
  __imp__mbclen : UInt64;
  __imp__mbclen_l : UInt64;
  __imp__mbctohira : UInt64;
  __imp__mbctohira_l : UInt64;
  __imp__mbctokata : UInt64;
  __imp__mbctokata_l : UInt64;
  __imp__mbctolower : UInt64;
  __imp__mbctolower_l : UInt64;
  __imp__mbctombb : UInt64;
  __imp__mbctombb_l : UInt64;
  __imp__mbctoupper : UInt64;
  __imp__mbctoupper_l : UInt64;
  __imp__mblen_l : UInt64;
  __imp__mbsbtype : UInt64;
  __imp__mbsbtype_l : UInt64;
  __imp__mbscat_s : UInt64;
  __imp__mbscat_s_l : UInt64;
  __imp__mbschr : UInt64;
  __imp__mbschr_l : UInt64;
  __imp__mbscmp : UInt64;
  __imp__mbscmp_l : UInt64;
  __imp__mbscoll : UInt64;
  __imp__mbscoll_l : UInt64;
  __imp__mbscpy_s : UInt64;
  __imp__mbscpy_s_l : UInt64;
  __imp__mbscspn : UInt64;
  __imp__mbscspn_l : UInt64;
  __imp__mbsdec : UInt64;
  __imp__mbsdec_l : UInt64;
  __imp__mbsdup : UInt64;
  __imp__mbsicmp : UInt64;
  __imp__mbsicmp_l : UInt64;
  __imp__mbsicoll : UInt64;
  __imp__mbsicoll_l : UInt64;
  __imp__mbsinc : UInt64;
  __imp__mbsinc_l : UInt64;
  __imp__mbslen : UInt64;
  __imp__mbslen_l : UInt64;
  __imp__mbslwr : UInt64;
  __imp__mbslwr_l : UInt64;
  __imp__mbslwr_s : UInt64;
  __imp__mbslwr_s_l : UInt64;
  __imp__mbsnbcat : UInt64;
  __imp__mbsnbcat_l : UInt64;
  __imp__mbsnbcat_s : UInt64;
  __imp__mbsnbcat_s_l : UInt64;
  __imp__mbsnbcmp : UInt64;
  __imp__mbsnbcmp_l : UInt64;
  __imp__mbsnbcnt : UInt64;
  __imp__mbsnbcnt_l : UInt64;
  __imp__mbsnbcoll : UInt64;
  __imp__mbsnbcoll_l : UInt64;
  __imp__mbsnbcpy : UInt64;
  __imp__mbsnbcpy_l : UInt64;
  __imp__mbsnbcpy_s : UInt64;
  __imp__mbsnbcpy_s_l : UInt64;
  __imp__mbsnbicmp : UInt64;
  __imp__mbsnbicmp_l : UInt64;
  __imp__mbsnbicoll : UInt64;
  __imp__mbsnbicoll_l : UInt64;
  __imp__mbsnbset : UInt64;
  __imp__mbsnbset_l : UInt64;
  __imp__mbsnbset_s : UInt64;
  __imp__mbsnbset_s_l : UInt64;
  __imp__mbsncat : UInt64;
  __imp__mbsncat_l : UInt64;
  __imp__mbsncat_s : UInt64;
  __imp__mbsncat_s_l : UInt64;
  __imp__mbsnccnt : UInt64;
  __imp__mbsnccnt_l : UInt64;
  __imp__mbsncmp : UInt64;
  __imp__mbsncmp_l : UInt64;
  __imp__mbsncoll : UInt64;
  __imp__mbsncoll_l : UInt64;
  __imp__mbsncpy : UInt64;
  __imp__mbsncpy_l : UInt64;
  __imp__mbsncpy_s : UInt64;
  __imp__mbsncpy_s_l : UInt64;
  __imp__mbsnextc : UInt64;
  __imp__mbsnextc_l : UInt64;
  __imp__mbsnicmp : UInt64;
  __imp__mbsnicmp_l : UInt64;
  __imp__mbsnicoll : UInt64;
  __imp__mbsnicoll_l : UInt64;
  __imp__mbsninc : UInt64;
  __imp__mbsninc_l : UInt64;
  __imp__mbsnlen : UInt64;
  __imp__mbsnlen_l : UInt64;
  __imp__mbsnset : UInt64;
  __imp__mbsnset_l : UInt64;
  __imp__mbsnset_s : UInt64;
  __imp__mbsnset_s_l : UInt64;
  __imp__mbspbrk : UInt64;
  __imp__mbspbrk_l : UInt64;
  __imp__mbsrchr : UInt64;
  __imp__mbsrchr_l : UInt64;
  __imp__mbsrev : UInt64;
  __imp__mbsrev_l : UInt64;
  __imp__mbsset : UInt64;
  __imp__mbsset_l : UInt64;
  __imp__mbsset_s : UInt64;
  __imp__mbsset_s_l : UInt64;
  __imp__mbsspn : UInt64;
  __imp__mbsspn_l : UInt64;
  __imp__mbsspnp : UInt64;
  __imp__mbsspnp_l : UInt64;
  __imp__mbsstr : UInt64;
  __imp__mbsstr_l : UInt64;
  __imp__mbstok : UInt64;
  __imp__mbstok_l : UInt64;
  __imp__mbstok_s : UInt64;
  __imp__mbstok_s_l : UInt64;
  __imp__mbstowcs_l : UInt64;
  __imp__mbstowcs_s_l : UInt64;
  __imp__mbstrlen : UInt64;
  __imp__mbstrlen_l : UInt64;
  __imp__mbstrnlen : UInt64;
  __imp__mbstrnlen_l : UInt64;
  __imp__mbsupr : UInt64;
  __imp__mbsupr_l : UInt64;
  __imp__mbsupr_s : UInt64;
  __imp__mbsupr_s_l : UInt64;
  __imp__mbtowc_l : UInt64;
  _head_lib64_libapi_ms_win_crt_multibyte_l1_1_0_a : UInt64;
  __lib64_libapi_ms_win_crt_multibyte_l1_1_0_a_iname : UInt64;
  __imp__Cbuild : UInt64;
  __imp__Cmulcc : UInt64;
  __imp__Cmulcr : UInt64;
  __imp__FCbuild : UInt64;
  __imp__FCmulcc : UInt64;
  __imp__FCmulcr : UInt64;
  __imp__LCbuild : UInt64;
  __imp__LCmulcc : UInt64;
  __imp__LCmulcr : UInt64;
  __imp___setusermatherr : UInt64;
  __imp__cabs : UInt64;
  __imp__chgsign : UInt64;
  __imp__chgsignf : UInt64;
  __imp__copysign : UInt64;
  __imp__copysignf : UInt64;
  __imp__d_int : UInt64;
  __imp__dclass : UInt64;
  __imp_chgsign : UInt64;
  __imp__dexp : UInt64;
  __imp__dlog : UInt64;
  __imp__dnorm : UInt64;
  __imp__dpcomp : UInt64;
  __imp__dpoly : UInt64;
  __imp__dscale : UInt64;
  __imp__dsign : UInt64;
  __imp__dsin : UInt64;
  __imp__dtest : UInt64;
  __imp__dunscale : UInt64;
  __imp__except1 : UInt64;
  __imp__fd_int : UInt64;
  __imp__fdclass : UInt64;
  __imp__fdexp : UInt64;
  __imp__fdlog : UInt64;
  __imp__fdnorm : UInt64;
  __imp__fdopen : UInt64;
  __imp__fdpcomp : UInt64;
  __imp_fdopen : UInt64;
  __imp__fdpoly : UInt64;
  __imp__fdscale : UInt64;
  __imp__fdsign : UInt64;
  __imp__fdsin : UInt64;
  __imp__fdtest : UInt64;
  __imp__fdunscale : UInt64;
  __imp__finite : UInt64;
  __imp__finitef : UInt64;
  __imp_finite : UInt64;
  __imp__fpclass : UInt64;
  __imp_fpclass : UInt64;
  __imp__fpclassf : UInt64;
  __imp__get_FMA3_enable : UInt64;
  __imp__hypot : UInt64;
  __imp__hypotf : UInt64;
  __imp__isnan : UInt64;
  __imp__isnanf : UInt64;
  __imp_hypot : UInt64;
  __imp__j0 : UInt64;
  __imp_j0 : UInt64;
  __imp__j1 : UInt64;
  __imp_jn : UInt64;
  __imp__jn : UInt64;
  __imp__ld_int : UInt64;
  __imp__ldclass : UInt64;
  __imp__ldexp : UInt64;
  __imp__ldlog : UInt64;
  __imp_j1 : UInt64;
  __imp__ldpcomp : UInt64;
  __imp__ldpoly : UInt64;
  __imp__ldscale : UInt64;
  __imp__ldsign : UInt64;
  __imp__ldsin : UInt64;
  __imp__ldtest : UInt64;
  __imp__ldunscale : UInt64;
  __imp__logb : UInt64;
  __imp__logbf : UInt64;
  __imp__nextafter : UInt64;
  __imp_nextafter : UInt64;
  __imp__nextafterf : UInt64;
  __imp__scalb : UInt64;
  __imp__scalbf : UInt64;
  __imp__set_FMA3_enable : UInt64;
  __imp__y0 : UInt64;
  __imp_y1 : UInt64;
  __imp__y1 : UInt64;
  __imp_y0 : UInt64;
  __imp__yn : UInt64;
  __imp_yn : UInt64;
  __imp_acos : UInt64;
  __imp_acosf : UInt64;
  __imp_acosh : UInt64;
  __imp_acoshf : UInt64;
  __imp_acoshl : UInt64;
  __imp_asin : UInt64;
  __imp_asinf : UInt64;
  __imp_asinh : UInt64;
  __imp_asinhf : UInt64;
  __imp_asinhl : UInt64;
  __imp_atan : UInt64;
  __imp_atan2 : UInt64;
  __imp_atan2f : UInt64;
  __imp_atanf : UInt64;
  __imp_atanh : UInt64;
  __imp_atanhf : UInt64;
  __imp_atanhl : UInt64;
  __imp_cabs : UInt64;
  __imp_cabsf : UInt64;
  __imp_cabsl : UInt64;
  __imp_cacos : UInt64;
  __imp_cacosf : UInt64;
  __imp_cacosh : UInt64;
  __imp_cacoshf : UInt64;
  __imp_cacoshl : UInt64;
  __imp_cacosl : UInt64;
  __imp_carg : UInt64;
  __imp_cargf : UInt64;
  __imp_cargl : UInt64;
  __imp_casin : UInt64;
  __imp_casinf : UInt64;
  __imp_casinh : UInt64;
  __imp_casinhf : UInt64;
  __imp_casinhl : UInt64;
  __imp_casinl : UInt64;
  __imp_catan : UInt64;
  __imp_catanf : UInt64;
  __imp_catanh : UInt64;
  __imp_catanhf : UInt64;
  __imp_catanhl : UInt64;
  __imp_catanl : UInt64;
  __imp_cbrt : UInt64;
  __imp_cbrtf : UInt64;
  __imp_cbrtl : UInt64;
  __imp_ccos : UInt64;
  __imp_ccosf : UInt64;
  __imp_ccosh : UInt64;
  __imp_ccoshf : UInt64;
  __imp_ccoshl : UInt64;
  __imp_ccosl : UInt64;
  __imp_ceil : UInt64;
  __imp_ceilf : UInt64;
  __imp_cexp : UInt64;
  __imp_cexpf : UInt64;
  __imp_cexpl : UInt64;
  __imp_cimag : UInt64;
  __imp_cimagf : UInt64;
  __imp_cimagl : UInt64;
  __imp_clog : UInt64;
  __imp_clog10 : UInt64;
  __imp_clog10f : UInt64;
  __imp_clog10l : UInt64;
  __imp_clogf : UInt64;
  __imp_clogl : UInt64;
  __imp_conj : UInt64;
  __imp_conjf : UInt64;
  __imp_conjl : UInt64;
  __imp_copysign : UInt64;
  __imp_copysignf : UInt64;
  __imp_copysignl : UInt64;
  __imp_cos : UInt64;
  __imp_cosf : UInt64;
  __imp_cosh : UInt64;
  __imp_coshf : UInt64;
  __imp_cpow : UInt64;
  __imp_cpowf : UInt64;
  __imp_cpowl : UInt64;
  __imp_cproj : UInt64;
  __imp_cprojf : UInt64;
  __imp_cprojl : UInt64;
  __imp_creal : UInt64;
  __imp_crealf : UInt64;
  __imp_creall : UInt64;
  __imp_csin : UInt64;
  __imp_csinf : UInt64;
  __imp_csinh : UInt64;
  __imp_csinhf : UInt64;
  __imp_csinhl : UInt64;
  __imp_csinl : UInt64;
  __imp_csqrt : UInt64;
  __imp_csqrtf : UInt64;
  __imp_csqrtl : UInt64;
  __imp_ctan : UInt64;
  __imp_ctanf : UInt64;
  __imp_ctanh : UInt64;
  __imp_ctanhf : UInt64;
  __imp_ctanhl : UInt64;
  __imp_ctanl : UInt64;
  __imp_erf : UInt64;
  __imp_erfc : UInt64;
  __imp_erfcf : UInt64;
  __imp_erfcl : UInt64;
  __imp_erff : UInt64;
  __imp_erfl : UInt64;
  __imp_exp : UInt64;
  __imp_exp2 : UInt64;
  __imp_exp2f : UInt64;
  __imp_exp2l : UInt64;
  __imp_expf : UInt64;
  __imp_expm1 : UInt64;
  __imp_expm1f : UInt64;
  __imp_expm1l : UInt64;
  __imp_fabs : UInt64;
  __imp_fdim : UInt64;
  __imp_fdimf : UInt64;
  __imp_fdiml : UInt64;
  __imp_floor : UInt64;
  __imp_floorf : UInt64;
  __imp_fma : UInt64;
  __imp_fmaf : UInt64;
  __imp_fmal : UInt64;
  __imp_fmax : UInt64;
  __imp_fmaxf : UInt64;
  __imp_fmaxl : UInt64;
  __imp_fmin : UInt64;
  __imp_fminf : UInt64;
  __imp_fminl : UInt64;
  __imp_fmod : UInt64;
  __imp_fmodf : UInt64;
  __imp_frexp : UInt64;
  __imp_ilogb : UInt64;
  __imp_ilogbf : UInt64;
  __imp_ilogbl : UInt64;
  __imp_ldexp : UInt64;
  __imp_lgamma : UInt64;
  __imp_lgammaf : UInt64;
  __imp_lgammal : UInt64;
  __imp_llrint : UInt64;
  __imp_llrintf : UInt64;
  __imp_llrintl : UInt64;
  __imp_llround : UInt64;
  __imp_llroundf : UInt64;
  __imp_llroundl : UInt64;
  __imp_log : UInt64;
  __imp_log10 : UInt64;
  __imp_log10f : UInt64;
  __imp_log1p : UInt64;
  __imp_log1pf : UInt64;
  __imp_log1pl : UInt64;
  __imp_log2 : UInt64;
  __imp_log2f : UInt64;
  __imp_log2l : UInt64;
  __imp_logb : UInt64;
  __imp_logbf : UInt64;
  __imp_logbl : UInt64;
  __imp_logf : UInt64;
  __imp_lrint : UInt64;
  __imp_lrintf : UInt64;
  __imp_lrintl : UInt64;
  __imp_lround : UInt64;
  __imp_lroundf : UInt64;
  __imp_lroundl : UInt64;
  __imp_modf : UInt64;
  __imp_modff : UInt64;
  __imp_nan : UInt64;
  __imp_nanf : UInt64;
  __imp_nanl : UInt64;
  __imp_nearbyint : UInt64;
  __imp_nearbyintf : UInt64;
  __imp_nearbyintl : UInt64;
  __imp_nextafterf : UInt64;
  __imp_nextafterl : UInt64;
  __imp_nexttoward : UInt64;
  __imp_nexttowardf : UInt64;
  __imp_nexttowardl : UInt64;
  __imp_norm : UInt64;
  __imp_normf : UInt64;
  __imp_norml : UInt64;
  __imp_pow : UInt64;
  __imp_powf : UInt64;
  __imp_remainder : UInt64;
  __imp_remainderf : UInt64;
  __imp_remainderl : UInt64;
  __imp_remquo : UInt64;
  __imp_remquof : UInt64;
  __imp_remquol : UInt64;
  __imp_rint : UInt64;
  __imp_rintf : UInt64;
  __imp_rintl : UInt64;
  __imp_round : UInt64;
  __imp_roundf : UInt64;
  __imp_roundl : UInt64;
  __imp_scalbln : UInt64;
  __imp_scalblnf : UInt64;
  __imp_scalblnl : UInt64;
  __imp_scalbn : UInt64;
  __imp_scalbnf : UInt64;
  __imp_scalbnl : UInt64;
  __imp_sin : UInt64;
  __imp_sinf : UInt64;
  __imp_sinh : UInt64;
  __imp_sinhf : UInt64;
  __imp_sqrt : UInt64;
  __imp_sqrtf : UInt64;
  __imp_tan : UInt64;
  __imp_tanf : UInt64;
  __imp_tanh : UInt64;
  __imp_tanhf : UInt64;
  __imp_tgamma : UInt64;
  __imp_tgammaf : UInt64;
  __imp_tgammal : UInt64;
  __imp_trunc : UInt64;
  __imp_truncf : UInt64;
  __imp_truncl : UInt64;
  _head_lib64_libapi_ms_win_crt_math_l1_1_0_a : UInt64;
  __lib64_libapi_ms_win_crt_math_l1_1_0_a_iname : UInt64;
  __imp____lc_codepage_func : UInt64;
  __imp____lc_collate_cp_func : UInt64;
  __imp____lc_locale_name_func : UInt64;
  __imp____mb_cur_max_func : UInt64;
  __imp____mb_cur_max_l_func : UInt64;
  __imp___initialize_lconv_for_unsigned_char : UInt64;
  __imp___lconv_init : UInt64;
  __imp___pctype_func : UInt64;
  __imp___pwctype_func : UInt64;
  __imp__configthreadlocale : UInt64;
  __imp__create_locale : UInt64;
  __imp__free_locale : UInt64;
  __imp__get_current_locale : UInt64;
  __imp__getmbcp : UInt64;
  __imp__lock_locales : UInt64;
  __imp__setmbcp : UInt64;
  __imp__unlock_locales : UInt64;
  __imp__wcreate_locale : UInt64;
  __imp__wsetlocale : UInt64;
  __imp_localeconv : UInt64;
  __imp_setlocale : UInt64;
  _head_lib64_libapi_ms_win_crt_locale_l1_1_0_a : UInt64;
  __lib64_libapi_ms_win_crt_locale_l1_1_0_a_iname : UInt64;
  __imp__aligned_free : UInt64;
  __imp__aligned_malloc : UInt64;
  __imp__aligned_msize : UInt64;
  __imp__aligned_offset_malloc : UInt64;
  __imp__aligned_offset_realloc : UInt64;
  __imp__aligned_offset_recalloc : UInt64;
  __imp__aligned_realloc : UInt64;
  __imp__aligned_recalloc : UInt64;
  __imp__callnewh : UInt64;
  __imp__calloc_base : UInt64;
  __imp__expand : UInt64;
  __imp__free_base : UInt64;
  __imp__get_heap_handle : UInt64;
  __imp__heapchk : UInt64;
  __imp__heapmin : UInt64;
  __imp__heapwalk : UInt64;
  __imp__malloc_base : UInt64;
  __imp__msize : UInt64;
  __imp__query_new_handler : UInt64;
  __imp__query_new_mode : UInt64;
  __imp_heapwalk : UInt64;
  __imp__realloc_base : UInt64;
  __imp__recalloc : UInt64;
  __imp__set_new_mode : UInt64;
  __imp_calloc : UInt64;
  __imp_free : UInt64;
  __imp_malloc : UInt64;
  __imp_realloc : UInt64;
  _head_lib64_libapi_ms_win_crt_heap_l1_1_0_a : UInt64;
  __lib64_libapi_ms_win_crt_heap_l1_1_0_a_iname : UInt64;
  __imp__findclose : UInt64;
  __imp__findfirst : UInt64;
  __imp_access : UInt64;
  __imp__access : UInt64;
  __imp__access_s : UInt64;
  __imp__chdir : UInt64;
  __imp__chdrive : UInt64;
  __imp__chmod : UInt64;
  __imp_chmod : UInt64;
  __imp_chdir : UInt64;
  __imp__findfirst32 : UInt64;
  __imp__findfirst32i64 : UInt64;
  __imp__findfirst64 : UInt64;
  __imp__findfirst64i32 : UInt64;
  __imp__findnext : UInt64;
  __imp__findnext32 : UInt64;
  __imp__findnext32i64 : UInt64;
  __imp__findnext64 : UInt64;
  __imp__findnext64i32 : UInt64;
  __imp__fstat32 : UInt64;
  __imp__fstat32i64 : UInt64;
  __imp__fstat64 : UInt64;
  __imp__fstat64i32 : UInt64;
  __imp__fullpath : UInt64;
  __imp__getdiskfree : UInt64;
  __imp__getdrive : UInt64;
  __imp__getdrives : UInt64;
  __imp__lock_file : UInt64;
  __imp__makepath : UInt64;
  __imp__makepath_s : UInt64;
  __imp__mkdir : UInt64;
  __imp_rmdir : UInt64;
  __imp__rmdir : UInt64;
  __imp__splitpath : UInt64;
  __imp__splitpath_s : UInt64;
  __imp__stat32 : UInt64;
  __imp_mkdir : UInt64;
  __imp__stat32i64 : UInt64;
  __imp__stat64 : UInt64;
  __imp__stat64i32 : UInt64;
  __imp__umask : UInt64;
  __imp_umask : UInt64;
  __imp__umask_s : UInt64;
  __imp__unlink : UInt64;
  __imp__unlock_file : UInt64;
  __imp__waccess : UInt64;
  __imp_unlink : UInt64;
  __imp__waccess_s : UInt64;
  __imp__wchdir : UInt64;
  __imp__wchmod : UInt64;
  __imp__wfindfirst32 : UInt64;
  __imp__wfindfirst32i64 : UInt64;
  __imp__wfindfirst64 : UInt64;
  __imp__wfindfirst64i32 : UInt64;
  __imp__wfindnext32 : UInt64;
  __imp__wfindnext32i64 : UInt64;
  __imp__wfindnext64 : UInt64;
  __imp__wfindnext64i32 : UInt64;
  __imp__wfullpath : UInt64;
  __imp__wmakepath : UInt64;
  __imp__wmakepath_s : UInt64;
  __imp__wmkdir : UInt64;
  __imp__wremove : UInt64;
  __imp__wrename : UInt64;
  __imp__wrmdir : UInt64;
  __imp__wsplitpath : UInt64;
  __imp__wsplitpath_s : UInt64;
  __imp__wstat32 : UInt64;
  __imp__wstat32i64 : UInt64;
  __imp__wstat64 : UInt64;
  __imp__wstat64i32 : UInt64;
  __imp__wunlink : UInt64;
  __imp_remove : UInt64;
  __imp_rename : UInt64;
  _head_lib64_libapi_ms_win_crt_filesystem_l1_1_0_a : UInt64;
  __lib64_libapi_ms_win_crt_filesystem_l1_1_0_a_iname : UInt64;
  __imp___p__environ : UInt64;
  __imp___p__wenviron : UInt64;
  __imp__dupenv_s : UInt64;
  __imp__putenv : UInt64;
  __imp_putenv : UInt64;
  __imp__putenv_s : UInt64;
  __imp__searchenv : UInt64;
  __imp__searchenv_s : UInt64;
  __imp__wdupenv_s : UInt64;
  __imp_searchenv : UInt64;
  __imp__wgetcwd : UInt64;
  __imp__wgetdcwd : UInt64;
  __imp__wgetenv : UInt64;
  __imp__wgetenv_s : UInt64;
  __imp__wputenv : UInt64;
  __imp__wputenv_s : UInt64;
  __imp__wsearchenv : UInt64;
  __imp__wsearchenv_s : UInt64;
  __imp_getenv : UInt64;
  __imp_getenv_s : UInt64;
  _head_lib64_libapi_ms_win_crt_environment_l1_1_0_a : UInt64;
  __lib64_libapi_ms_win_crt_environment_l1_1_0_a_iname : UInt64;
  __imp___toascii : UInt64;
  __imp__atodbl : UInt64;
  __imp__atodbl_l : UInt64;
  __imp__atof_l : UInt64;
  __imp__atoflt : UInt64;
  __imp__atoflt_l : UInt64;
  __imp_toascii : UInt64;
  __imp__atoi64 : UInt64;
  __imp__atoi64_l : UInt64;
  __imp__atoi_l : UInt64;
  __imp__atol_l : UInt64;
  __imp__atoldbl : UInt64;
  __imp__atoldbl_l : UInt64;
  __imp__atoll_l : UInt64;
  __imp__ecvt : UInt64;
  __imp_ecvt : UInt64;
  __imp__ecvt_s : UInt64;
  __imp__fcvt : UInt64;
  __imp__fcvt_s : UInt64;
  __imp_gcvt : UInt64;
  __imp__gcvt : UInt64;
  __imp_fcvt : UInt64;
  __imp__gcvt_s : UInt64;
  __imp__i64toa : UInt64;
  __imp__i64toa_s : UInt64;
  __imp__i64tow : UInt64;
  __imp__i64tow_s : UInt64;
  __imp__itoa : UInt64;
  __imp_itoa : UInt64;
  __imp__itoa_s : UInt64;
  __imp__itow : UInt64;
  __imp__itow_s : UInt64;
  __imp__ltoa : UInt64;
  __imp__ltoa_s : UInt64;
  __imp__ltow : UInt64;
  __imp__ltow_s : UInt64;
  __imp__strtod_l : UInt64;
  __imp__strtof_l : UInt64;
  __imp_ltoa : UInt64;
  __imp__strtoi64 : UInt64;
  __imp__strtoi64_l : UInt64;
  __imp__strtoimax_l : UInt64;
  __imp__strtol_l : UInt64;
  __imp__strtold_l : UInt64;
  __imp__strtoll_l : UInt64;
  __imp__strtoui64 : UInt64;
  __imp__strtoui64_l : UInt64;
  __imp__strtoul_l : UInt64;
  __imp__strtoull_l : UInt64;
  __imp__strtoumax_l : UInt64;
  __imp__ui64toa : UInt64;
  __imp__ui64toa_s : UInt64;
  __imp__ui64tow : UInt64;
  __imp__ui64tow_s : UInt64;
  __imp__ultoa : UInt64;
  __imp__ultoa_s : UInt64;
  __imp__ultow : UInt64;
  __imp__ultow_s : UInt64;
  __imp__wcstod_l : UInt64;
  __imp__wcstof_l : UInt64;
  __imp__wcstoi64 : UInt64;
  __imp__wcstoi64_l : UInt64;
  __imp__wcstoimax_l : UInt64;
  __imp__wcstol_l : UInt64;
  __imp__wcstold_l : UInt64;
  __imp__wcstoll_l : UInt64;
  __imp__wcstombs_l : UInt64;
  __imp__wcstombs_s_l : UInt64;
  __imp__wcstoui64 : UInt64;
  __imp__wcstoui64_l : UInt64;
  __imp__wcstoul_l : UInt64;
  __imp__wcstoull_l : UInt64;
  __imp__wcstoumax_l : UInt64;
  __imp__wctomb_l : UInt64;
  __imp__wctomb_s_l : UInt64;
  __imp__wtof : UInt64;
  __imp__wtof_l : UInt64;
  __imp__wtoi : UInt64;
  __imp__wtoi64 : UInt64;
  __imp__wtoi64_l : UInt64;
  __imp__wtoi_l : UInt64;
  __imp__wtol : UInt64;
  __imp__wtol_l : UInt64;
  __imp__wtoll : UInt64;
  __imp__wtoll_l : UInt64;
  __imp_atof : UInt64;
  __imp_atoi : UInt64;
  __imp_atol : UInt64;
  __imp_atoll : UInt64;
  __imp_btowc : UInt64;
  __imp_c16rtomb : UInt64;
  __imp_c32rtomb : UInt64;
  __imp_mbrtoc16 : UInt64;
  __imp_mbrtoc32 : UInt64;
  __imp_mbrtowc : UInt64;
  __imp_mbsrtowcs : UInt64;
  __imp_mbsrtowcs_s : UInt64;
  __imp_mbstowcs : UInt64;
  __imp_mbstowcs_s : UInt64;
  __imp_mbtowc : UInt64;
  __imp_strtod : UInt64;
  __imp_strtof : UInt64;
  __imp_strtoimax : UInt64;
  __imp_strtol : UInt64;
  __imp_strtoll : UInt64;
  __imp_strtoul : UInt64;
  __imp_strtoull : UInt64;
  __imp_strtoumax : UInt64;
  __imp_wcrtomb : UInt64;
  __imp_wcrtomb_s : UInt64;
  __imp_wcsrtombs : UInt64;
  __imp_wcsrtombs_s : UInt64;
  __imp_wcstod : UInt64;
  __imp_wcstof : UInt64;
  __imp_wcstoimax : UInt64;
  __imp_wcstol : UInt64;
  __imp_wcstoll : UInt64;
  __imp_wcstombs : UInt64;
  __imp_wcstombs_s : UInt64;
  __imp_wcstoul : UInt64;
  __imp_wcstoull : UInt64;
  __imp_wcstoumax : UInt64;
  __imp_wctob : UInt64;
  __imp_wctomb : UInt64;
  __imp_wctomb_s : UInt64;
  __imp_wctrans : UInt64;
  _head_lib64_libapi_ms_win_crt_convert_l1_1_0_a : UInt64;
  __lib64_libapi_ms_win_crt_convert_l1_1_0_a_iname : UInt64;
  __imp___conio_common_vcprintf : UInt64;
  __imp___conio_common_vcprintf_p : UInt64;
  __imp___conio_common_vcprintf_s : UInt64;
  __imp___conio_common_vcscanf : UInt64;
  __imp___conio_common_vcwprintf : UInt64;
  __imp___conio_common_vcwprintf_p : UInt64;
  __imp___conio_common_vcwprintf_s : UInt64;
  __imp___conio_common_vcwscanf : UInt64;
  __imp__cgets : UInt64;
  __imp__cgets_s : UInt64;
  __imp__cgetws : UInt64;
  __imp__cgetws_s : UInt64;
  __imp__cputs : UInt64;
  __imp__cputws : UInt64;
  __imp__getch : UInt64;
  __imp_getch : UInt64;
  __imp__getch_nolock : UInt64;
  __imp__getche : UInt64;
  __imp__getche_nolock : UInt64;
  __imp__getwch : UInt64;
  __imp__getwch_nolock : UInt64;
  __imp__getwche : UInt64;
  __imp__getwche_nolock : UInt64;
  __imp_getche : UInt64;
  __imp__putch : UInt64;
  __imp_putch : UInt64;
  __imp__putch_nolock : UInt64;
  __imp__putwch : UInt64;
  __imp__putwch_nolock : UInt64;
  __imp__ungetch : UInt64;
  __imp__ungetch_nolock : UInt64;
  __imp_ungetch : UInt64;
  __imp__ungetwch : UInt64;
  __imp__ungetwch_nolock : UInt64;
  _head_lib64_libapi_ms_win_crt_conio_l1_1_0_a : UInt64;
  __lib64_libapi_ms_win_crt_conio_l1_1_0_a_iname : UInt64;
{$ENDIF}
{$IFDEF WIN32}
procedure _vsscanf;external;
{$L x86/lib32_libucrt_extra_a-ucrt_vsscanf.o}
procedure _vsprintf;external;
{$L x86/lib32_libucrt_extra_a-ucrt_vsprintf.o}
procedure _vsnprintf;external;
{$L x86/lib32_libucrt_extra_a-ucrt_vsnprintf.o}
procedure _vscanf;external;
{$L x86/lib32_libucrt_extra_a-ucrt_vscanf.o}
procedure _vprintf;external;
{$L x86/lib32_libucrt_extra_a-ucrt_vprintf.o}
procedure _vfscanf;external;
{$L x86/lib32_libucrt_extra_a-ucrt_vfscanf.o}
procedure _vfprintf;external;
{$L x86/lib32_libucrt_extra_a-ucrt_vfprintf.o}
procedure __vsnwprintf;external;
{$L x86/lib32_libucrt_extra_a-ucrt__vsnwprintf.o}
procedure __vsnprintf;external;
{$L x86/lib32_libucrt_extra_a-ucrt__vsnprintf.o}
procedure __vscprintf;external;
{$L x86/lib32_libucrt_extra_a-ucrt__vscprintf.o}
procedure _sscanf;external;
{$L x86/lib32_libucrt_extra_a-ucrt_sscanf.o}
procedure _sprintf;external;
{$L x86/lib32_libucrt_extra_a-ucrt_sprintf.o}
procedure _snprintf;external;
{$L x86/lib32_libucrt_extra_a-ucrt_snprintf.o}
procedure __snwprintf;external;
{$L x86/lib32_libucrt_extra_a-ucrt__snwprintf.o}
procedure _scanf;external;
{$L x86/lib32_libucrt_extra_a-ucrt_scanf.o}
procedure _printf;external;
{$L x86/lib32_libucrt_extra_a-ucrt_printf.o}
procedure _fwprintf;external;
{$L x86/lib32_libucrt_extra_a-ucrt_fwprintf.o}
procedure _fscanf;external;
{$L x86/lib32_libucrt_extra_a-ucrt_fscanf.o}
procedure _fprintf;external;
{$L x86/lib32_libucrt_extra_a-ucrt_fprintf.o}
procedure __get_output_format;external;
{$L x86/lib32_libucrt_extra_a-ucrtbase_compat.o}
procedure __abs64;external;
{$L x86/libapi-ms-win-crt-utility-l1-1-0s00000.o}
procedure __byteswap_uint64;external;
{$L x86/libapi-ms-win-crt-utility-l1-1-0s00001.o}
procedure __byteswap_ulong;external;
{$L x86/libapi-ms-win-crt-utility-l1-1-0s00002.o}
procedure __byteswap_ushort;external;
{$L x86/libapi-ms-win-crt-utility-l1-1-0s00003.o}
procedure _lfind;external;
{$L x86/libapi-ms-win-crt-utility-l1-1-0s00004.o}
procedure __lfind;external;
{$L x86/libapi-ms-win-crt-utility-l1-1-0s00005.o}
procedure __lfind_s;external;
{$L x86/libapi-ms-win-crt-utility-l1-1-0s00006.o}
procedure __lrotl;external;
{$L x86/libapi-ms-win-crt-utility-l1-1-0s00007.o}
procedure __lrotr;external;
{$L x86/libapi-ms-win-crt-utility-l1-1-0s00008.o}
procedure _lsearch;external;
{$L x86/libapi-ms-win-crt-utility-l1-1-0s00009.o}
procedure __lsearch;external;
{$L x86/libapi-ms-win-crt-utility-l1-1-0s00010.o}
procedure __lsearch_s;external;
{$L x86/libapi-ms-win-crt-utility-l1-1-0s00011.o}
procedure __rotl;external;
{$L x86/libapi-ms-win-crt-utility-l1-1-0s00012.o}
procedure __rotl64;external;
{$L x86/libapi-ms-win-crt-utility-l1-1-0s00013.o}
procedure __rotr;external;
{$L x86/libapi-ms-win-crt-utility-l1-1-0s00014.o}
procedure __rotr64;external;
{$L x86/libapi-ms-win-crt-utility-l1-1-0s00015.o}
procedure __swab;external;
{$L x86/libapi-ms-win-crt-utility-l1-1-0s00016.o}
procedure _swab;external;
{$L x86/libapi-ms-win-crt-utility-l1-1-0s00017.o}
procedure _abs;external;
{$L x86/libapi-ms-win-crt-utility-l1-1-0s00018.o}
procedure _bsearch;external;
{$L x86/libapi-ms-win-crt-utility-l1-1-0s00019.o}
procedure _bsearch_s;external;
{$L x86/libapi-ms-win-crt-utility-l1-1-0s00020.o}
procedure _div;external;
{$L x86/libapi-ms-win-crt-utility-l1-1-0s00021.o}
procedure _imaxabs;external;
{$L x86/libapi-ms-win-crt-utility-l1-1-0s00022.o}
procedure _imaxdiv;external;
{$L x86/libapi-ms-win-crt-utility-l1-1-0s00023.o}
procedure _labs;external;
{$L x86/libapi-ms-win-crt-utility-l1-1-0s00024.o}
procedure _ldiv;external;
{$L x86/libapi-ms-win-crt-utility-l1-1-0s00025.o}
procedure _llabs;external;
{$L x86/libapi-ms-win-crt-utility-l1-1-0s00026.o}
procedure _lldiv;external;
{$L x86/libapi-ms-win-crt-utility-l1-1-0s00027.o}
procedure _qsort;external;
{$L x86/libapi-ms-win-crt-utility-l1-1-0s00028.o}
procedure _qsort_s;external;
{$L x86/libapi-ms-win-crt-utility-l1-1-0s00029.o}
procedure _rand;external;
{$L x86/libapi-ms-win-crt-utility-l1-1-0s00030.o}
procedure _rand_s;external;
{$L x86/libapi-ms-win-crt-utility-l1-1-0s00031.o}
procedure _srand;external;
{$L x86/libapi-ms-win-crt-utility-l1-1-0s00032.o}
procedure __Getdays;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00000.o}
procedure __Getmonths;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00001.o}
procedure __Gettnames;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00002.o}
procedure __Strftime;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00003.o}
procedure __W_Getdays;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00004.o}
procedure __W_Getmonths;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00005.o}
procedure __W_Gettnames;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00006.o}
procedure __Wcsftime;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00007.o}
procedure ___daylight;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00008.o}
procedure ___dstbias;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00009.o}
procedure ___timezone;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00010.o}
procedure ___tzname;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00011.o}
procedure __ctime32;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00012.o}
procedure __ctime32_s;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00013.o}
procedure _ctime;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00014.o}
procedure __ctime64;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00015.o}
procedure __ctime64_s;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00016.o}
procedure __difftime32;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00017.o}
procedure __difftime64;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00018.o}
procedure __ftime32;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00019.o}
procedure __ftime32_s;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00020.o}
procedure __ftime;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00021.o}
procedure __ftime64;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00022.o}
procedure __ftime64_s;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00023.o}
procedure __futime32;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00024.o}
procedure __futime;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00025.o}
procedure __futime64;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00026.o}
procedure __get_daylight;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00027.o}
procedure __get_dstbias;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00028.o}
procedure __get_timezone;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00029.o}
procedure __get_tzname;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00030.o}
procedure __getsystime;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00031.o}
procedure __gmtime32;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00032.o}
procedure __gmtime32_s;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00033.o}
procedure _gmtime;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00034.o}
procedure __gmtime64;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00035.o}
procedure __gmtime64_s;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00036.o}
procedure __localtime32;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00037.o}
procedure __localtime32_s;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00038.o}
procedure __localtime64;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00039.o}
procedure _localtime;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00040.o}
procedure __localtime64_s;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00041.o}
procedure __mkgmtime32;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00042.o}
procedure __mkgmtime64;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00043.o}
procedure __mktime32;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00044.o}
procedure _mktime;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00045.o}
procedure __mktime64;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00046.o}
procedure __setsystime;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00047.o}
procedure __strdate;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00048.o}
procedure __strdate_s;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00049.o}
procedure __strftime_l;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00050.o}
procedure __strtime;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00051.o}
procedure __strtime_s;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00052.o}
procedure __time32;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00053.o}
procedure _time;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00054.o}
procedure __time64;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00055.o}
procedure __timespec32_get;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00056.o}
procedure __timespec64_get;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00057.o}
procedure __utime32;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00059.o}
procedure __utime;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00060.o}
procedure __utime64;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00061.o}
procedure __wasctime;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00062.o}
procedure _utime;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00063.o}
procedure __wasctime_s;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00064.o}
procedure __wcsftime_l;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00065.o}
procedure __wctime32;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00066.o}
procedure __wctime32_s;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00067.o}
procedure __wctime64;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00068.o}
procedure __wctime64_s;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00069.o}
procedure __wutime;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00070.o}
procedure __wstrdate;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00071.o}
procedure __wstrdate_s;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00072.o}
procedure __wstrtime;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00073.o}
procedure __wstrtime_s;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00074.o}
procedure __wutime32;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00075.o}
procedure __wutime64;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00076.o}
procedure _asctime;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00077.o}
procedure _asctime_s;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00078.o}
procedure _clock;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00079.o}
procedure _strftime;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00080.o}
procedure _timespec_get;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00081.o}
procedure _wcsftime;external;
{$L x86/libapi-ms-win-crt-time-l1-1-0s00082.o}
procedure __iswalpha_l;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00000.o}
procedure __strcmpi;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00001.o}
procedure ___isascii;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00002.o}
procedure ___iscsym;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00003.o}
procedure _iscsymf;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00004.o}
procedure ___iscsymf;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00005.o}
procedure ___iswcsym;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00006.o}
procedure _iscsym;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00007.o}
procedure ___iswcsymf;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00008.o}
procedure ___strncnt;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00009.o}
procedure ___wcsncnt;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00010.o}
procedure __isalnum_l;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00011.o}
procedure __isalpha_l;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00012.o}
procedure __isblank_l;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00013.o}
procedure __iscntrl_l;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00014.o}
procedure __isctype;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00015.o}
procedure __isctype_l;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00016.o}
procedure __isdigit_l;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00017.o}
procedure __isgraph_l;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00018.o}
procedure __isleadbyte_l;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00019.o}
procedure __islower_l;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00020.o}
procedure __isprint_l;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00021.o}
procedure _isascii;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00022.o}
procedure __ispunct_l;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00023.o}
procedure __isspace_l;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00024.o}
procedure __isupper_l;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00025.o}
procedure __iswalnum_l;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00026.o}
procedure __iswblank_l;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00027.o}
procedure __iswcntrl_l;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00028.o}
procedure __iswcsym_l;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00029.o}
procedure __iswcsymf_l;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00030.o}
procedure __iswctype_l;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00031.o}
procedure __iswdigit_l;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00032.o}
procedure __iswgraph_l;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00033.o}
procedure __iswlower_l;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00034.o}
procedure __iswprint_l;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00035.o}
procedure __iswpunct_l;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00036.o}
procedure __iswspace_l;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00037.o}
procedure __iswupper_l;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00038.o}
procedure __iswxdigit_l;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00039.o}
procedure __isxdigit_l;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00040.o}
procedure __memccpy;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00041.o}
procedure _memicmp;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00042.o}
procedure __memicmp;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00043.o}
procedure __memicmp_l;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00044.o}
procedure __strcoll_l;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00045.o}
procedure _memccpy;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00046.o}
procedure __strdup;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00047.o}
procedure _strcmpi;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00048.o}
procedure _stricmp;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00049.o}
procedure _strcasecmp;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00050.o}
procedure _strdup;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00051.o}
procedure __stricmp;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00052.o}
procedure __stricmp_l;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00053.o}
procedure _stricoll;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00054.o}
procedure __stricoll;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00055.o}
procedure __stricoll_l;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00056.o}
procedure __strlwr;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00057.o}
procedure _strlwr;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00058.o}
procedure __strlwr_l;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00059.o}
procedure __strlwr_s;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00060.o}
procedure __strlwr_s_l;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00061.o}
procedure __strncoll;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00062.o}
procedure __strncoll_l;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00063.o}
procedure __strnicmp;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00064.o}
procedure _strnicmp;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00065.o}
procedure _strncasecmp;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00066.o}
procedure __strnicmp_l;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00067.o}
procedure __strnicoll;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00068.o}
procedure __strnicoll_l;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00069.o}
procedure _strnset;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00070.o}
procedure __strnset;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00071.o}
procedure __strnset_s;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00072.o}
procedure _strrev;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00073.o}
procedure __strrev;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00074.o}
procedure __strset;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00075.o}
procedure _strset;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00076.o}
procedure __strset_s;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00077.o}
procedure _strupr;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00078.o}
procedure __strupr;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00079.o}
procedure __strupr_l;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00080.o}
procedure __strupr_s;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00081.o}
procedure __strupr_s_l;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00082.o}
procedure __strxfrm_l;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00083.o}
procedure __tolower;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00084.o}
procedure __tolower_l;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00085.o}
procedure __toupper;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00086.o}
procedure __toupper_l;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00087.o}
procedure __towlower_l;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00088.o}
procedure __towupper_l;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00089.o}
procedure __wcscoll_l;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00090.o}
procedure _wcsdup;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00091.o}
procedure __wcsdup;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00092.o}
procedure _wcsicmp;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00093.o}
procedure _wcscmpi;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00094.o}
procedure __wcsicmp;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00095.o}
procedure __wcsicmp_l;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00096.o}
procedure __wcsicoll;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00097.o}
procedure _wcsicoll;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00098.o}
procedure __wcsicoll_l;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00099.o}
procedure __wcslwr;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00100.o}
procedure _wcslwr;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00101.o}
procedure __wcslwr_l;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00102.o}
procedure __wcslwr_s;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00103.o}
procedure __wcslwr_s_l;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00104.o}
procedure __wcsncoll;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00105.o}
procedure __wcsncoll_l;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00106.o}
procedure __wcsnicmp;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00107.o}
procedure _wcsnicmp;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00108.o}
procedure __wcsnicmp_l;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00109.o}
procedure __wcsnicoll;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00110.o}
procedure __wcsnicoll_l;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00111.o}
procedure __wcsnset;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00112.o}
procedure __wcsnset_s;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00113.o}
procedure _wcsnset;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00114.o}
procedure __wcsrev;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00115.o}
procedure _wcsrev;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00116.o}
procedure __wcsset;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00117.o}
procedure __wcsset_s;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00118.o}
procedure _wcsupr;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00119.o}
procedure __wcsupr;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00120.o}
procedure _wcsset;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00121.o}
procedure __wcsupr_l;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00122.o}
procedure __wcsupr_s;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00123.o}
procedure __wcsupr_s_l;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00124.o}
procedure __wcsxfrm_l;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00125.o}
procedure __wctype;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00126.o}
procedure _is_wctype;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00127.o}
procedure _isalnum;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00128.o}
procedure _isalpha;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00129.o}
procedure _isblank;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00130.o}
procedure _iscntrl;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00131.o}
procedure _isdigit;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00132.o}
procedure _isgraph;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00133.o}
procedure _isleadbyte;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00134.o}
procedure _islower;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00135.o}
procedure _isprint;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00136.o}
procedure _ispunct;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00137.o}
procedure _isspace;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00138.o}
procedure _isupper;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00139.o}
procedure _iswalnum;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00140.o}
procedure _iswalpha;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00141.o}
procedure _iswascii;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00142.o}
procedure _iswblank;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00143.o}
procedure _iswcntrl;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00144.o}
procedure _iswctype;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00145.o}
procedure _iswdigit;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00146.o}
procedure _iswgraph;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00147.o}
procedure _iswlower;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00148.o}
procedure _iswprint;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00149.o}
procedure _iswpunct;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00150.o}
procedure _iswspace;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00151.o}
procedure _iswupper;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00152.o}
procedure _iswxdigit;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00153.o}
procedure _isxdigit;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00154.o}
procedure _mblen;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00155.o}
procedure _mbrlen;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00156.o}
procedure _memcpy_s;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00157.o}
procedure _memmove_s;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00158.o}
procedure _memset;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00159.o}
procedure _strcat;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00160.o}
procedure _strcat_s;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00161.o}
procedure _strcmp;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00162.o}
procedure _strcoll;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00163.o}
procedure _strcpy;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00164.o}
procedure _strcpy_s;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00165.o}
procedure _strcspn;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00166.o}
procedure _strlen;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00167.o}
procedure _strncat;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00168.o}
procedure _strncat_s;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00169.o}
procedure _strncmp;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00170.o}
procedure _strncpy;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00171.o}
procedure _strncpy_s;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00172.o}
procedure _strpbrk;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00173.o}
procedure _strspn;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00174.o}
procedure _strtok;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00175.o}
procedure _strtok_s;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00176.o}
procedure _strxfrm;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00177.o}
procedure _tolower;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00178.o}
procedure _toupper;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00179.o}
procedure _towctrans;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00180.o}
procedure _towlower;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00181.o}
procedure _towupper;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00182.o}
procedure _wcscat;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00183.o}
procedure _wcscat_s;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00184.o}
procedure _wcscmp;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00185.o}
procedure _wcscoll;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00186.o}
procedure _wcscpy;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00187.o}
procedure _wcscpy_s;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00188.o}
procedure _wcscspn;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00189.o}
procedure _wcslen;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00190.o}
procedure _wcsncat;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00191.o}
procedure _wcsncat_s;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00192.o}
procedure _wcsncmp;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00193.o}
procedure _wcsncpy;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00194.o}
procedure _wcsncpy_s;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00195.o}
procedure _wcspbrk;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00197.o}
procedure _wcsspn;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00198.o}
procedure _wcstok;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00199.o}
procedure _wcstok_s;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00200.o}
procedure _wcsxfrm;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00201.o}
procedure _wctype;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00202.o}
procedure _wmemcpy_s;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00203.o}
procedure _wmemmove_s;external;
{$L x86/libapi-ms-win-crt-string-l1-1-0s00204.o}
procedure ___acrt_iob_func;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00000.o}
procedure ___p__commode;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00001.o}
procedure ___p__fmode;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00002.o}
procedure ___stdio_common_vfprintf;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00003.o}
procedure ___stdio_common_vfprintf_p;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00004.o}
procedure ___stdio_common_vfprintf_s;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00005.o}
procedure ___stdio_common_vfscanf;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00006.o}
procedure ___stdio_common_vfwprintf;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00007.o}
procedure ___stdio_common_vfwprintf_p;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00008.o}
procedure ___stdio_common_vfwprintf_s;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00009.o}
procedure ___stdio_common_vfwscanf;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00010.o}
procedure ___stdio_common_vsnprintf_s;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00011.o}
procedure ___stdio_common_vsnwprintf_s;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00012.o}
procedure ___stdio_common_vsprintf;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00013.o}
procedure ___stdio_common_vsprintf_p;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00014.o}
procedure ___stdio_common_vsprintf_s;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00015.o}
procedure ___stdio_common_vsscanf;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00016.o}
procedure ___stdio_common_vswprintf;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00017.o}
procedure ___stdio_common_vswprintf_p;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00018.o}
procedure ___stdio_common_vswprintf_s;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00019.o}
procedure ___stdio_common_vswscanf;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00020.o}
procedure __chsize;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00021.o}
procedure _chsize;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00022.o}
procedure __chsize_s;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00023.o}
procedure __close;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00024.o}
procedure __commit;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00025.o}
procedure _creat;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00026.o}
procedure __creat;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00027.o}
procedure _close;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00028.o}
procedure __dup;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00029.o}
procedure _dup;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00030.o}
procedure __dup2;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00031.o}
procedure _eof;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00032.o}
procedure __eof;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00033.o}
procedure _dup2;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00034.o}
procedure __fclose_nolock;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00035.o}
procedure __fcloseall;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00036.o}
procedure __fflush_nolock;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00037.o}
procedure __fgetc_nolock;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00038.o}
procedure __fgetchar;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00039.o}
procedure _fgetchar;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00040.o}
procedure __fgetwc_nolock;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00041.o}
procedure __fgetwchar;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00042.o}
procedure _filelength;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00043.o}
procedure __filelength;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00044.o}
procedure __filelengthi64;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00045.o}
procedure _fgetwchar;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00046.o}
procedure __fileno;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00047.o}
procedure _fileno;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00048.o}
procedure __flushall;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00049.o}
procedure __fputc_nolock;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00050.o}
procedure __fputchar;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00051.o}
procedure __fputwc_nolock;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00052.o}
procedure _fputchar;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00053.o}
procedure __fputwchar;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00054.o}
procedure _fputwchar;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00055.o}
procedure __fread_nolock;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00056.o}
procedure __fread_nolock_s;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00057.o}
procedure __fseek_nolock;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00058.o}
procedure __fseeki64;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00059.o}
procedure __fseeki64_nolock;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00060.o}
procedure __fsopen;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00061.o}
procedure __ftell_nolock;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00062.o}
procedure __ftelli64;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00063.o}
procedure __ftelli64_nolock;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00064.o}
procedure __fwrite_nolock;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00065.o}
procedure __get_fmode;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00066.o}
procedure __get_osfhandle;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00067.o}
procedure __get_printf_count_output;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00068.o}
procedure __get_stream_buffer_pointers;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00069.o}
procedure __getc_nolock;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00070.o}
procedure __getcwd;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00071.o}
procedure _getcwd;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00072.o}
procedure __getdcwd;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00073.o}
procedure __getmaxstdio;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00074.o}
procedure __getw;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00075.o}
procedure _getw;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00076.o}
procedure __getwc_nolock;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00077.o}
procedure __getws;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00078.o}
procedure __getws_s;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00079.o}
procedure __isatty;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00080.o}
procedure _isatty;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00081.o}
procedure __kbhit;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00082.o}
procedure _kbhit;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00083.o}
procedure __locking;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00084.o}
procedure __lseek;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00085.o}
procedure __lseeki64;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00086.o}
procedure _mktemp;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00087.o}
procedure __mktemp;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00088.o}
procedure __mktemp_s;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00089.o}
procedure __open;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00090.o}
procedure _lseek;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00091.o}
procedure _open;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00092.o}
procedure _pclose;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00093.o}
procedure __pclose;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00094.o}
procedure __open_osfhandle;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00095.o}
procedure __pipe;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00096.o}
procedure __popen;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00097.o}
procedure __putc_nolock;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00098.o}
procedure _popen;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00099.o}
procedure __putw;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00100.o}
procedure _putw;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00101.o}
procedure __putwc_nolock;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00102.o}
procedure __putws;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00103.o}
procedure __read;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00104.o}
procedure _read;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00105.o}
procedure __rmtmp;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00106.o}
procedure _rmtmp;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00107.o}
procedure __set_fmode;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00108.o}
procedure __set_printf_count_output;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00109.o}
procedure __setmaxstdio;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00110.o}
procedure _setmode;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00111.o}
procedure __setmode;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00112.o}
procedure __sopen;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00113.o}
procedure __sopen_dispatch;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00114.o}
procedure __sopen_s;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00115.o}
procedure _sopen;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00116.o}
procedure __tell;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00117.o}
procedure _tell;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00118.o}
procedure __telli64;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00119.o}
procedure __tempnam;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00120.o}
procedure __ungetc_nolock;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00121.o}
procedure __ungetwc_nolock;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00122.o}
procedure __wcreat;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00123.o}
procedure _tempnam;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00124.o}
procedure __wfdopen;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00125.o}
procedure __wfopen;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00126.o}
procedure __wfopen_s;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00127.o}
procedure __wfreopen;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00128.o}
procedure __wfreopen_s;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00129.o}
procedure __wfsopen;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00130.o}
procedure __wmktemp;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00131.o}
procedure __wmktemp_s;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00132.o}
procedure __wopen;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00133.o}
procedure __wpopen;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00134.o}
procedure _wpopen;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00135.o}
procedure __write;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00136.o}
procedure _write;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00137.o}
procedure __wsopen;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00138.o}
procedure __wsopen_dispatch;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00139.o}
procedure __wsopen_s;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00140.o}
procedure __wtempnam;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00141.o}
procedure __wtmpnam;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00142.o}
procedure __wtmpnam_s;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00143.o}
procedure _clearerr;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00144.o}
procedure _clearerr_s;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00145.o}
procedure _fclose;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00146.o}
procedure _feof;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00147.o}
procedure _ferror;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00148.o}
procedure _fflush;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00149.o}
procedure _fgetc;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00150.o}
procedure _fgetpos;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00151.o}
procedure _fgets;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00152.o}
procedure _fgetwc;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00153.o}
procedure _fgetws;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00154.o}
procedure _fopen;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00155.o}
procedure _fopen_s;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00156.o}
procedure _fputc;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00157.o}
procedure _fputs;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00158.o}
procedure _fputwc;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00159.o}
procedure _fputws;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00160.o}
procedure _fread;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00161.o}
procedure _fread_s;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00162.o}
procedure _freopen;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00163.o}
procedure _freopen_s;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00164.o}
procedure _fseek;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00165.o}
procedure _fsetpos;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00166.o}
procedure _ftell;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00167.o}
procedure _fwrite;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00168.o}
procedure _getc;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00169.o}
procedure _getchar;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00170.o}
procedure _gets;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00171.o}
procedure _gets_s;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00172.o}
procedure _getwc;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00173.o}
procedure _getwchar;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00174.o}
procedure _putc;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00175.o}
procedure _putchar;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00176.o}
procedure _puts;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00177.o}
procedure _putwc;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00178.o}
procedure _putwchar;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00179.o}
procedure _rewind;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00180.o}
procedure _setbuf;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00181.o}
procedure _setvbuf;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00182.o}
procedure _tmpfile;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00183.o}
procedure _tmpfile_s;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00184.o}
procedure _tmpnam;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00185.o}
procedure _tmpnam_s;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00186.o}
procedure _ungetc;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00187.o}
procedure _ungetwc;external;
{$L x86/libapi-ms-win-crt-stdio-l1-1-0s00188.o}
procedure __Exit;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00000.o}
procedure ___control87_2;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00001.o}
procedure ___doserrno;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00002.o}
procedure ___fpe_flt_rounds;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00003.o}
procedure ___fpecode;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00004.o}
procedure ___p___argc;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00005.o}
procedure ___p___argv;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00006.o}
procedure ___p___wargv;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00007.o}
procedure ___p__acmdln;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00008.o}
procedure ___p__pgmptr;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00009.o}
procedure ___p__wcmdln;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00010.o}
procedure ___p__wpgmptr;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00011.o}
procedure ___pxcptinfoptrs;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00012.o}
procedure ___sys_errlist;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00013.o}
procedure ___sys_nerr;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00014.o}
procedure ___threadhandle;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00015.o}
procedure ___threadid;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00016.o}
procedure ___wcserror;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00017.o}
procedure ___wcserror_s;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00018.o}
procedure __assert;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00019.o}
procedure __beginthread;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00020.o}
procedure __beginthreadex;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00021.o}
procedure __c_exit;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00022.o}
procedure __cexit;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00023.o}
procedure __clearfp;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00024.o}
procedure __configure_narrow_argv;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00025.o}
procedure __configure_wide_argv;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00026.o}
procedure __control87;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00027.o}
procedure __controlfp;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00028.o}
procedure __controlfp_s;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00029.o}
procedure __crt_at_quick_exit;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00030.o}
procedure __crt_atexit;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00031.o}
procedure __crt_debugger_hook;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00032.o}
procedure __endthread;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00033.o}
procedure __endthreadex;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00034.o}
procedure __errno;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00035.o}
procedure __execute_onexit_table;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00036.o}
procedure __get_doserrno;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00039.o}
procedure __get_errno;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00040.o}
procedure __get_initial_narrow_environment;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00041.o}
procedure __get_initial_wide_environment;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00042.o}
procedure __get_invalid_parameter_handler;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00043.o}
procedure __get_narrow_winmain_command_line;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00044.o}
procedure __get_pgmptr;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00045.o}
procedure __get_terminate;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00046.o}
procedure __get_thread_local_invalid_parameter_handler;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00047.o}
procedure __get_wide_winmain_command_line;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00048.o}
procedure __get_wpgmptr;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00049.o}
procedure __getdllprocaddr;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00050.o}
procedure __getpid;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00051.o}
procedure __initialize_narrow_environment;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00052.o}
procedure _getpid;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00053.o}
procedure __initialize_onexit_table;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00054.o}
procedure ___set_app_type;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00055.o}
procedure __initialize_wide_environment;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00056.o}
procedure __initterm;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00057.o}
procedure __initterm_e;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00058.o}
procedure __invalid_parameter_noinfo;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00059.o}
procedure __invalid_parameter_noinfo_noreturn;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00060.o}
procedure __invoke_watson;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00061.o}
procedure __query_app_type;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00062.o}
procedure __register_onexit_function;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00063.o}
procedure __register_thread_local_exe_atexit_callback;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00064.o}
procedure __resetstkoflw;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00065.o}
procedure __seh_filter_dll;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00066.o}
procedure __seh_filter_exe;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00067.o}
procedure __set_abort_behavior;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00068.o}
procedure __set_app_type;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00069.o}
procedure __set_controlfp;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00070.o}
procedure __set_doserrno;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00071.o}
procedure __set_errno;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00072.o}
procedure __set_error_mode;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00073.o}
procedure __set_invalid_parameter_handler;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00074.o}
procedure __set_new_handler;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00075.o}
procedure __set_thread_local_invalid_parameter_handler;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00076.o}
procedure __seterrormode;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00077.o}
procedure __sleep;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00078.o}
procedure __statusfp;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00079.o}
procedure __statusfp2;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00080.o}
procedure __strerror;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00081.o}
procedure __strerror_s;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00082.o}
procedure __wassert;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00083.o}
procedure __wcserror;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00084.o}
procedure __wcserror_s;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00085.o}
procedure __wperror;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00086.o}
procedure __wsystem;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00087.o}
procedure _abort;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00088.o}
procedure _exit;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00089.o}
procedure _perror;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00099.o}
procedure _quick_exit;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00100.o}
procedure _raise;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00101.o}
procedure _set_terminate;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00102.o}
procedure _signal;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00103.o}
procedure _strerror;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00104.o}
procedure _strerror_s;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00105.o}
procedure _system;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00106.o}
procedure _terminate;external;
{$L x86/libapi-ms-win-crt-runtime-l1-1-0s00107.o}
procedure __beep;external;
{$L x86/libapi-ms-win-crt-process-l1-1-0s00000.o}
procedure __cwait;external;
{$L x86/libapi-ms-win-crt-process-l1-1-0s00001.o}
procedure _execl;external;
{$L x86/libapi-ms-win-crt-process-l1-1-0s00002.o}
procedure __execl;external;
{$L x86/libapi-ms-win-crt-process-l1-1-0s00003.o}
procedure _cwait;external;
{$L x86/libapi-ms-win-crt-process-l1-1-0s00004.o}
procedure __execle;external;
{$L x86/libapi-ms-win-crt-process-l1-1-0s00005.o}
procedure _execle;external;
{$L x86/libapi-ms-win-crt-process-l1-1-0s00006.o}
procedure __execlp;external;
{$L x86/libapi-ms-win-crt-process-l1-1-0s00007.o}
procedure _execlpe;external;
{$L x86/libapi-ms-win-crt-process-l1-1-0s00008.o}
procedure __execlpe;external;
{$L x86/libapi-ms-win-crt-process-l1-1-0s00009.o}
procedure _execlp;external;
{$L x86/libapi-ms-win-crt-process-l1-1-0s00010.o}
procedure __execv;external;
{$L x86/libapi-ms-win-crt-process-l1-1-0s00011.o}
procedure _execv;external;
{$L x86/libapi-ms-win-crt-process-l1-1-0s00012.o}
procedure __execve;external;
{$L x86/libapi-ms-win-crt-process-l1-1-0s00013.o}
procedure _execve;external;
{$L x86/libapi-ms-win-crt-process-l1-1-0s00014.o}
procedure __execvp;external;
{$L x86/libapi-ms-win-crt-process-l1-1-0s00015.o}
procedure _execvpe;external;
{$L x86/libapi-ms-win-crt-process-l1-1-0s00016.o}
procedure __execvpe;external;
{$L x86/libapi-ms-win-crt-process-l1-1-0s00017.o}
procedure __loaddll;external;
{$L x86/libapi-ms-win-crt-process-l1-1-0s00018.o}
procedure _execvp;external;
{$L x86/libapi-ms-win-crt-process-l1-1-0s00019.o}
procedure __spawnl;external;
{$L x86/libapi-ms-win-crt-process-l1-1-0s00020.o}
procedure _spawnl;external;
{$L x86/libapi-ms-win-crt-process-l1-1-0s00021.o}
procedure __spawnle;external;
{$L x86/libapi-ms-win-crt-process-l1-1-0s00022.o}
procedure __spawnlp;external;
{$L x86/libapi-ms-win-crt-process-l1-1-0s00023.o}
procedure _spawnlpe;external;
{$L x86/libapi-ms-win-crt-process-l1-1-0s00024.o}
procedure _spawnle;external;
{$L x86/libapi-ms-win-crt-process-l1-1-0s00025.o}
procedure __spawnlpe;external;
{$L x86/libapi-ms-win-crt-process-l1-1-0s00026.o}
procedure _spawnlp;external;
{$L x86/libapi-ms-win-crt-process-l1-1-0s00027.o}
procedure __spawnv;external;
{$L x86/libapi-ms-win-crt-process-l1-1-0s00028.o}
procedure _spawnve;external;
{$L x86/libapi-ms-win-crt-process-l1-1-0s00029.o}
procedure __spawnve;external;
{$L x86/libapi-ms-win-crt-process-l1-1-0s00030.o}
procedure _spawnvp;external;
{$L x86/libapi-ms-win-crt-process-l1-1-0s00031.o}
procedure __spawnvp;external;
{$L x86/libapi-ms-win-crt-process-l1-1-0s00032.o}
procedure _spawnv;external;
{$L x86/libapi-ms-win-crt-process-l1-1-0s00033.o}
procedure __spawnvpe;external;
{$L x86/libapi-ms-win-crt-process-l1-1-0s00034.o}
procedure _spawnvpe;external;
{$L x86/libapi-ms-win-crt-process-l1-1-0s00035.o}
procedure __unloaddll;external;
{$L x86/libapi-ms-win-crt-process-l1-1-0s00036.o}
procedure __wexecl;external;
{$L x86/libapi-ms-win-crt-process-l1-1-0s00037.o}
procedure __wexecle;external;
{$L x86/libapi-ms-win-crt-process-l1-1-0s00038.o}
procedure __wexeclp;external;
{$L x86/libapi-ms-win-crt-process-l1-1-0s00039.o}
procedure __wexeclpe;external;
{$L x86/libapi-ms-win-crt-process-l1-1-0s00040.o}
procedure __wexecv;external;
{$L x86/libapi-ms-win-crt-process-l1-1-0s00041.o}
procedure __wexecve;external;
{$L x86/libapi-ms-win-crt-process-l1-1-0s00042.o}
procedure __wexecvp;external;
{$L x86/libapi-ms-win-crt-process-l1-1-0s00043.o}
procedure __wexecvpe;external;
{$L x86/libapi-ms-win-crt-process-l1-1-0s00044.o}
procedure __wspawnl;external;
{$L x86/libapi-ms-win-crt-process-l1-1-0s00045.o}
procedure __wspawnle;external;
{$L x86/libapi-ms-win-crt-process-l1-1-0s00046.o}
procedure __wspawnlp;external;
{$L x86/libapi-ms-win-crt-process-l1-1-0s00047.o}
procedure __wspawnlpe;external;
{$L x86/libapi-ms-win-crt-process-l1-1-0s00048.o}
procedure __wspawnv;external;
{$L x86/libapi-ms-win-crt-process-l1-1-0s00049.o}
procedure __wspawnve;external;
{$L x86/libapi-ms-win-crt-process-l1-1-0s00050.o}
procedure __wspawnvp;external;
{$L x86/libapi-ms-win-crt-process-l1-1-0s00051.o}
procedure __wspawnvpe;external;
{$L x86/libapi-ms-win-crt-process-l1-1-0s00052.o}
procedure __CreateFrameInfo;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00000.o}
procedure __EH_prolog;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00002.o}
procedure __FindAndUnlinkFrame;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00003.o}
procedure __GetImageBase;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00004.o}
procedure __GetThrowImageBase;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00005.o}
procedure __IsExceptionObjectToBeDestroyed;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00006.o}
procedure __NLG_Dispatch2;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00007.o}
procedure __NLG_Return;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00008.o}
procedure __NLG_Return2;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00009.o}
procedure __SetImageBase;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00010.o}
procedure __SetThrowImageBase;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00011.o}
procedure __SetWinRTOutOfMemoryExceptionCallback;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00012.o}
procedure ___AdjustPointer;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00013.o}
procedure ___BuildCatchObject;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00014.o}
procedure ___BuildCatchObjectHelper;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00015.o}
procedure ___CxxDetectRethrow;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00016.o}
procedure ___CxxExceptionFilter;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00017.o}
procedure ___CxxFrameHandler;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00018.o}
procedure ___CxxFrameHandler2;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00019.o}
procedure ___CxxFrameHandler3;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00020.o}
procedure ___CxxQueryExceptionSize;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00022.o}
procedure ___CxxRegisterExceptionObject;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00023.o}
procedure ___CxxUnregisterExceptionObject;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00024.o}
procedure ___DestructExceptionObject;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00025.o}
procedure ___FrameUnwindFilter;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00026.o}
procedure ___GetPlatformExceptionInfo;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00027.o}
procedure ___NLG_Dispatch2;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00028.o}
procedure ___NLG_Return2;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00029.o}
procedure ___RTCastToVoid;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00030.o}
procedure ___RTDynamicCast;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00031.o}
procedure ___RTtypeid;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00032.o}
procedure ___TypeMatch;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00033.o}
procedure ___current_exception;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00034.o}
procedure ___current_exception_context;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00035.o}
procedure ___dcrt_get_wide_environment_from_os;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00036.o}
procedure ___dcrt_initial_narrow_environment;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00037.o}
procedure ___intrinsic_abnormal_termination;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00038.o}
procedure ___intrinsic_setjmp;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00039.o}
procedure ___processing_throw;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00040.o}
procedure ___report_gsfailure;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00041.o}
procedure ___std_exception_copy;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00042.o}
procedure ___std_exception_destroy;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00043.o}
procedure ___std_type_info_compare;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00044.o}
procedure ___std_type_info_destroy_list;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00045.o}
procedure ___std_type_info_hash;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00046.o}
procedure ___std_type_info_name;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00047.o}
procedure ___unDName;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00048.o}
procedure ___unDNameEx;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00049.o}
procedure ___uncaught_exception;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00050.o}
procedure __chkesp;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00051.o}
procedure __except_handler2;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00052.o}
procedure __except_handler3;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00053.o}
procedure __except_handler4_common;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00054.o}
procedure __get_purecall_handler;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00055.o}
procedure __get_unexpected;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00056.o}
procedure __global_unwind2;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00057.o}
procedure __is_exception_typeof;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00058.o}
procedure __local_unwind2;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00059.o}
procedure __local_unwind4;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00060.o}
procedure __longjmpex;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00061.o}
procedure __o__CIacos;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00062.o}
procedure __o__CIasin;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00063.o}
procedure __o__CIatan;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00064.o}
procedure __o__CIatan2;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00065.o}
procedure __o__CIcos;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00066.o}
procedure __o__CIcosh;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00067.o}
procedure __o__CIexp;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00068.o}
procedure __o__CIfmod;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00069.o}
procedure __o__CIlog;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00070.o}
procedure __o__CIlog10;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00071.o}
procedure __o__CIpow;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00072.o}
procedure __o__CIsin;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00073.o}
procedure __o__CIsinh;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00074.o}
procedure __o__CIsqrt;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00075.o}
procedure __o__CItan;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00076.o}
procedure __o__CItanh;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00077.o}
procedure __o__Getdays;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00078.o}
procedure __o__Getmonths;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00079.o}
procedure __o__Gettnames;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00080.o}
procedure __o__Strftime;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00081.o}
procedure __o__W_Getdays;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00082.o}
procedure __o__W_Getmonths;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00083.o}
procedure __o__W_Gettnames;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00084.o}
procedure __o__Wcsftime;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00085.o}
procedure __o___acrt_iob_func;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00086.o}
procedure __o___conio_common_vcprintf;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00087.o}
procedure __o___conio_common_vcprintf_p;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00088.o}
procedure __o___conio_common_vcprintf_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00089.o}
procedure __o___conio_common_vcscanf;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00090.o}
procedure __o___conio_common_vcwprintf;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00091.o}
procedure __o___conio_common_vcwprintf_p;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00092.o}
procedure __o___conio_common_vcwprintf_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00093.o}
procedure __o___conio_common_vcwscanf;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00094.o}
procedure __o___daylight;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00095.o}
procedure __o___dstbias;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00096.o}
procedure __o___fpe_flt_rounds;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00097.o}
procedure __o___libm_sse2_acos;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00098.o}
procedure __o___libm_sse2_acosf;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00099.o}
procedure __o___libm_sse2_asin;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00100.o}
procedure __o___libm_sse2_asinf;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00101.o}
procedure __o___libm_sse2_atan;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00102.o}
procedure __o___libm_sse2_atan2;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00103.o}
procedure __o___libm_sse2_atanf;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00104.o}
procedure __o___libm_sse2_cos;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00105.o}
procedure __o___libm_sse2_cosf;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00106.o}
procedure __o___libm_sse2_exp;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00107.o}
procedure __o___libm_sse2_expf;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00108.o}
procedure __o___libm_sse2_log;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00109.o}
procedure __o___libm_sse2_log10;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00110.o}
procedure __o___libm_sse2_log10f;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00111.o}
procedure __o___libm_sse2_logf;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00112.o}
procedure __o___libm_sse2_pow;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00113.o}
procedure __o___libm_sse2_powf;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00114.o}
procedure __o___libm_sse2_sin;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00115.o}
procedure __o___libm_sse2_sinf;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00116.o}
procedure __o___libm_sse2_tan;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00117.o}
procedure __o___libm_sse2_tanf;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00118.o}
procedure __o___p___argc;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00119.o}
procedure __o___p___argv;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00120.o}
procedure __o___p___wargv;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00121.o}
procedure __o___p__acmdln;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00122.o}
procedure __o___p__commode;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00123.o}
procedure __o___p__environ;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00124.o}
procedure __o___p__fmode;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00125.o}
procedure __o___p__mbcasemap;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00126.o}
procedure __o___p__mbctype;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00127.o}
procedure __o___p__pgmptr;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00128.o}
procedure __o___p__wcmdln;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00129.o}
procedure __o___p__wenviron;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00130.o}
procedure __o___p__wpgmptr;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00131.o}
procedure __o___pctype_func;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00132.o}
procedure __o___pwctype_func;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00133.o}
procedure __o___stdio_common_vfprintf;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00134.o}
procedure __o___stdio_common_vfprintf_p;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00135.o}
procedure __o___stdio_common_vfprintf_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00136.o}
procedure __o___stdio_common_vfscanf;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00137.o}
procedure __o___stdio_common_vfwprintf;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00138.o}
procedure __o___stdio_common_vfwprintf_p;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00139.o}
procedure __o___stdio_common_vfwprintf_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00140.o}
procedure __o___stdio_common_vfwscanf;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00141.o}
procedure __o___stdio_common_vsnprintf_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00142.o}
procedure __o___stdio_common_vsnwprintf_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00143.o}
procedure __o___stdio_common_vsprintf;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00144.o}
procedure __o___stdio_common_vsprintf_p;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00145.o}
procedure __o___stdio_common_vsprintf_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00146.o}
procedure __o___stdio_common_vsscanf;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00147.o}
procedure __o___stdio_common_vswprintf;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00148.o}
procedure __o___stdio_common_vswprintf_p;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00149.o}
procedure __o___stdio_common_vswprintf_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00150.o}
procedure __o___stdio_common_vswscanf;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00151.o}
procedure __o___timezone;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00152.o}
procedure __o___tzname;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00153.o}
procedure __o___wcserror;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00154.o}
procedure __o__access;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00155.o}
procedure __o__access_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00156.o}
procedure __o__aligned_free;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00157.o}
procedure __o__aligned_malloc;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00158.o}
procedure __o__aligned_msize;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00159.o}
procedure __o__aligned_offset_malloc;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00160.o}
procedure __o__aligned_offset_realloc;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00161.o}
procedure __o__aligned_offset_recalloc;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00162.o}
procedure __o__aligned_realloc;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00163.o}
procedure __o__aligned_recalloc;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00164.o}
procedure __o__atodbl;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00165.o}
procedure __o__atodbl_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00166.o}
procedure __o__atof_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00167.o}
procedure __o__atoflt;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00168.o}
procedure __o__atoflt_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00169.o}
procedure __o__atoi64;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00170.o}
procedure __o__atoi64_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00171.o}
procedure __o__atoi_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00172.o}
procedure __o__atol_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00173.o}
procedure __o__atoldbl;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00174.o}
procedure __o__atoldbl_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00175.o}
procedure __o__atoll_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00176.o}
procedure __o__beep;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00177.o}
procedure __o__beginthread;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00178.o}
procedure __o__beginthreadex;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00179.o}
procedure __o__cabs;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00180.o}
procedure __o__callnewh;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00181.o}
procedure __o__calloc_base;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00182.o}
procedure __o__cgets;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00183.o}
procedure __o__cgets_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00184.o}
procedure __o__cgetws;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00185.o}
procedure __o__cgetws_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00186.o}
procedure __o__chdir;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00187.o}
procedure __o__chdrive;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00188.o}
procedure __o__chmod;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00189.o}
procedure __o__chsize;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00190.o}
procedure __o__chsize_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00191.o}
procedure __o__close;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00192.o}
procedure __o__commit;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00193.o}
procedure __o__configure_wide_argv;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00194.o}
procedure __o__cputs;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00195.o}
procedure __o__cputws;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00196.o}
procedure __o__creat;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00197.o}
procedure __o__create_locale;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00198.o}
procedure __o__ctime32_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00199.o}
procedure __o__ctime64_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00200.o}
procedure __o__cwait;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00201.o}
procedure __o__d_int;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00202.o}
procedure __o__dclass;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00203.o}
procedure __o__difftime32;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00204.o}
procedure __o__difftime64;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00205.o}
procedure __o__dlog;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00206.o}
procedure __o__dnorm;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00207.o}
procedure __o__dpcomp;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00208.o}
procedure __o__dpoly;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00209.o}
procedure __o__dscale;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00210.o}
procedure __o__dsign;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00211.o}
procedure __o__dsin;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00212.o}
procedure __o__dtest;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00213.o}
procedure __o__dunscale;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00214.o}
procedure __o__dup;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00215.o}
procedure __o__dup2;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00216.o}
procedure __o__dupenv_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00217.o}
procedure __o__ecvt;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00218.o}
procedure __o__ecvt_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00219.o}
procedure __o__endthread;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00220.o}
procedure __o__endthreadex;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00221.o}
procedure __o__eof;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00222.o}
procedure __o__errno;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00223.o}
procedure __o__except1;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00224.o}
procedure __o__execute_onexit_table;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00225.o}
procedure __o__execv;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00226.o}
procedure __o__execve;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00227.o}
procedure __o__execvp;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00228.o}
procedure __o__execvpe;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00229.o}
procedure __o__expand;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00230.o}
procedure __o__fclose_nolock;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00231.o}
procedure __o__fcloseall;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00232.o}
procedure __o__fcvt;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00233.o}
procedure __o__fcvt_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00234.o}
procedure __o__fd_int;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00235.o}
procedure __o__fdclass;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00236.o}
procedure __o__fdexp;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00237.o}
procedure __o__fdlog;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00238.o}
procedure __o__fdopen;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00239.o}
procedure __o__fdpcomp;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00240.o}
procedure __o__fdpoly;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00241.o}
procedure __o__fdscale;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00242.o}
procedure __o__fdsign;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00243.o}
procedure __o__fdsin;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00244.o}
procedure __o__fflush_nolock;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00245.o}
procedure __o__fgetc_nolock;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00246.o}
procedure __o__fgetchar;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00247.o}
procedure __o__fgetwc_nolock;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00248.o}
procedure __o__fgetwchar;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00249.o}
procedure __o__filelength;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00250.o}
procedure __o__filelengthi64;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00251.o}
procedure __o__fileno;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00252.o}
procedure __o__findclose;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00253.o}
procedure __o__findfirst32;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00254.o}
procedure __o__findfirst32i64;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00255.o}
procedure __o__findfirst64;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00256.o}
procedure __o__findfirst64i32;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00257.o}
procedure __o__findnext32;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00258.o}
procedure __o__findnext32i64;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00259.o}
procedure __o__findnext64;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00260.o}
procedure __o__findnext64i32;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00261.o}
procedure __o__flushall;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00262.o}
procedure __o__fpclass;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00263.o}
procedure __o__fpclassf;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00264.o}
procedure __o__fputc_nolock;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00265.o}
procedure __o__fputchar;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00266.o}
procedure __o__fputwc_nolock;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00267.o}
procedure __o__fputwchar;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00268.o}
procedure __o__fread_nolock;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00269.o}
procedure __o__fread_nolock_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00270.o}
procedure __o__free_base;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00271.o}
procedure __o__free_locale;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00272.o}
procedure __o__fseek_nolock;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00273.o}
procedure __o__fseeki64;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00274.o}
procedure __o__fseeki64_nolock;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00275.o}
procedure __o__fsopen;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00276.o}
procedure __o__fstat32;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00277.o}
procedure __o__fstat32i64;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00278.o}
procedure __o__fstat64;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00279.o}
procedure __o__fstat64i32;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00280.o}
procedure __o__ftell_nolock;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00281.o}
procedure __o__ftelli64;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00282.o}
procedure __o__ftelli64_nolock;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00283.o}
procedure __o__ftime32;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00284.o}
procedure __o__ftime32_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00285.o}
procedure __o__ftime64;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00286.o}
procedure __o__ftime64_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00287.o}
procedure __o__fullpath;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00288.o}
procedure __o__futime32;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00289.o}
procedure __o__futime64;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00290.o}
procedure __o__fwrite_nolock;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00291.o}
procedure __o__gcvt;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00292.o}
procedure __o__gcvt_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00293.o}
procedure __o__get_daylight;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00294.o}
procedure __o__get_doserrno;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00295.o}
procedure __o__get_dstbias;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00296.o}
procedure __o__get_errno;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00297.o}
procedure __o__get_fmode;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00298.o}
procedure __o__get_heap_handle;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00299.o}
procedure __o__get_invalid_parameter_handler;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00300.o}
procedure __o__get_narrow_winmain_command_line;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00301.o}
procedure __o__get_osfhandle;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00302.o}
procedure __o__get_pgmptr;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00303.o}
procedure __o__get_stream_buffer_pointers;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00304.o}
procedure __o__get_terminate;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00305.o}
procedure __o__get_thread_local_invalid_parameter_handler;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00306.o}
procedure __o__get_timezone;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00307.o}
procedure __o__get_tzname;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00308.o}
procedure __o__get_wide_winmain_command_line;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00309.o}
procedure __o__get_wpgmptr;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00310.o}
procedure __o__getc_nolock;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00311.o}
procedure __o__getch;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00312.o}
procedure __o__getch_nolock;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00313.o}
procedure __o__getche;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00314.o}
procedure __o__getche_nolock;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00315.o}
procedure __o__getcwd;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00316.o}
procedure __o__getdcwd;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00317.o}
procedure __o__getdiskfree;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00318.o}
procedure __o__getdllprocaddr;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00319.o}
procedure __o__getdrive;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00320.o}
procedure __o__getdrives;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00321.o}
procedure __o__getmbcp;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00322.o}
procedure __o__getsystime;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00323.o}
procedure __o__getw;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00324.o}
procedure __o__getwc_nolock;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00325.o}
procedure __o__getwch;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00326.o}
procedure __o__getwch_nolock;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00327.o}
procedure __o__getwche;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00328.o}
procedure __o__getwche_nolock;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00329.o}
procedure __o__getws;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00330.o}
procedure __o__getws_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00331.o}
procedure __o__gmtime32;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00332.o}
procedure __o__gmtime32_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00333.o}
procedure __o__gmtime64;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00334.o}
procedure __o__gmtime64_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00335.o}
procedure __o__heapchk;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00336.o}
procedure __o__heapmin;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00337.o}
procedure __o__hypot;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00338.o}
procedure __o__hypotf;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00339.o}
procedure __o__i64toa;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00340.o}
procedure __o__i64toa_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00341.o}
procedure __o__i64tow;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00342.o}
procedure __o__i64tow_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00343.o}
procedure __o__initialize_onexit_table;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00344.o}
procedure __o__invalid_parameter_noinfo;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00345.o}
procedure __o__invalid_parameter_noinfo_noreturn;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00346.o}
procedure __o__isatty;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00347.o}
procedure __o__isctype;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00348.o}
procedure __o__isctype_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00349.o}
procedure __o__isleadbyte_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00350.o}
procedure __o__ismbbalnum;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00351.o}
procedure __o__ismbbalnum_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00352.o}
procedure __o__ismbbalpha;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00353.o}
procedure __o__ismbbalpha_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00354.o}
procedure __o__ismbbblank;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00355.o}
procedure __o__ismbbblank_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00356.o}
procedure __o__ismbbgraph;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00357.o}
procedure __o__ismbbgraph_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00358.o}
procedure __o__ismbbkalnum;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00359.o}
procedure __o__ismbbkalnum_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00360.o}
procedure __o__ismbbkana;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00361.o}
procedure __o__ismbbkana_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00362.o}
procedure __o__ismbbkprint;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00363.o}
procedure __o__ismbbkprint_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00364.o}
procedure __o__ismbbkpunct;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00365.o}
procedure __o__ismbbkpunct_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00366.o}
procedure __o__ismbblead;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00367.o}
procedure __o__ismbblead_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00368.o}
procedure __o__ismbbprint;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00369.o}
procedure __o__ismbbprint_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00370.o}
procedure __o__ismbbpunct;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00371.o}
procedure __o__ismbbpunct_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00372.o}
procedure __o__ismbbtrail;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00373.o}
procedure __o__ismbbtrail_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00374.o}
procedure __o__ismbcalnum;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00375.o}
procedure __o__ismbcalnum_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00376.o}
procedure __o__ismbcalpha;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00377.o}
procedure __o__ismbcalpha_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00378.o}
procedure __o__ismbcblank;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00379.o}
procedure __o__ismbcblank_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00380.o}
procedure __o__ismbcdigit;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00381.o}
procedure __o__ismbcdigit_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00382.o}
procedure __o__ismbcgraph;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00383.o}
procedure __o__ismbcgraph_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00384.o}
procedure __o__ismbchira;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00385.o}
procedure __o__ismbchira_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00386.o}
procedure __o__ismbckata;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00387.o}
procedure __o__ismbckata_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00388.o}
procedure __o__ismbcl0;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00389.o}
procedure __o__ismbcl0_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00390.o}
procedure __o__ismbcl1;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00391.o}
procedure __o__ismbcl1_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00392.o}
procedure __o__ismbcl2;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00393.o}
procedure __o__ismbcl2_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00394.o}
procedure __o__ismbclegal;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00395.o}
procedure __o__ismbclegal_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00396.o}
procedure __o__ismbclower;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00397.o}
procedure __o__ismbclower_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00398.o}
procedure __o__ismbcprint;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00399.o}
procedure __o__ismbcprint_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00400.o}
procedure __o__ismbcpunct;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00401.o}
procedure __o__ismbcpunct_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00402.o}
procedure __o__ismbcspace;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00403.o}
procedure __o__ismbcspace_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00404.o}
procedure __o__ismbcsymbol;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00405.o}
procedure __o__ismbcsymbol_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00406.o}
procedure __o__ismbcupper;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00407.o}
procedure __o__ismbcupper_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00408.o}
procedure __o__ismbslead;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00409.o}
procedure __o__ismbslead_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00410.o}
procedure __o__ismbstrail;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00411.o}
procedure __o__ismbstrail_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00412.o}
procedure __o__iswctype_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00413.o}
procedure __o__itoa;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00414.o}
procedure __o__itoa_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00415.o}
procedure __o__itow;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00416.o}
procedure __o__itow_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00417.o}
procedure __o__j0;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00418.o}
procedure __o__j1;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00419.o}
procedure __o__jn;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00420.o}
procedure __o__kbhit;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00421.o}
procedure __o__ld_int;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00422.o}
procedure __o__ldclass;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00423.o}
procedure __o__ldexp;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00424.o}
procedure __o__ldlog;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00425.o}
procedure __o__ldpcomp;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00426.o}
procedure __o__ldpoly;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00427.o}
procedure __o__ldscale;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00428.o}
procedure __o__ldsign;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00429.o}
procedure __o__ldsin;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00430.o}
procedure __o__ldtest;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00431.o}
procedure __o__ldunscale;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00432.o}
procedure __o__lfind;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00433.o}
procedure __o__lfind_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00434.o}
procedure __o__libm_sse2_acos_precise;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00435.o}
procedure __o__libm_sse2_asin_precise;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00436.o}
procedure __o__libm_sse2_atan_precise;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00437.o}
procedure __o__libm_sse2_cos_precise;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00438.o}
procedure __o__libm_sse2_exp_precise;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00439.o}
procedure __o__libm_sse2_log10_precise;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00440.o}
procedure __o__libm_sse2_log_precise;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00441.o}
procedure __o__libm_sse2_pow_precise;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00442.o}
procedure __o__libm_sse2_sin_precise;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00443.o}
procedure __o__libm_sse2_sqrt_precise;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00444.o}
procedure __o__libm_sse2_tan_precise;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00445.o}
procedure __o__loaddll;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00446.o}
procedure __o__localtime32;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00447.o}
procedure __o__localtime32_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00448.o}
procedure __o__localtime64;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00449.o}
procedure __o__localtime64_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00450.o}
procedure __o__lock_file;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00451.o}
procedure __o__locking;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00452.o}
procedure __o__logb;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00453.o}
procedure __o__logbf;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00454.o}
procedure __o__lsearch;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00455.o}
procedure __o__lsearch_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00456.o}
procedure __o__lseek;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00457.o}
procedure __o__lseeki64;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00458.o}
procedure __o__ltoa;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00459.o}
procedure __o__ltoa_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00460.o}
procedure __o__ltow;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00461.o}
procedure __o__ltow_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00462.o}
procedure __o__makepath;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00463.o}
procedure __o__makepath_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00464.o}
procedure __o__malloc_base;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00465.o}
procedure __o__mbbtombc;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00466.o}
procedure __o__mbbtombc_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00467.o}
procedure __o__mbbtype;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00468.o}
procedure __o__mbbtype_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00469.o}
procedure __o__mbccpy;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00470.o}
procedure __o__mbccpy_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00471.o}
procedure __o__mbccpy_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00472.o}
procedure __o__mbccpy_s_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00473.o}
procedure __o__mbcjistojms;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00474.o}
procedure __o__mbcjistojms_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00475.o}
procedure __o__mbcjmstojis;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00476.o}
procedure __o__mbcjmstojis_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00477.o}
procedure __o__mbclen;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00478.o}
procedure __o__mbclen_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00479.o}
procedure __o__mbctohira;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00480.o}
procedure __o__mbctohira_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00481.o}
procedure __o__mbctokata;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00482.o}
procedure __o__mbctokata_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00483.o}
procedure __o__mbctolower;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00484.o}
procedure __o__mbctolower_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00485.o}
procedure __o__mbctombb;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00486.o}
procedure __o__mbctombb_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00487.o}
procedure __o__mbctoupper;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00488.o}
procedure __o__mbctoupper_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00489.o}
procedure __o__mblen_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00490.o}
procedure __o__mbsbtype;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00491.o}
procedure __o__mbsbtype_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00492.o}
procedure __o__mbscat_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00493.o}
procedure __o__mbscat_s_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00494.o}
procedure __o__mbschr;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00495.o}
procedure __o__mbschr_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00496.o}
procedure __o__mbscmp;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00497.o}
procedure __o__mbscmp_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00498.o}
procedure __o__mbscoll;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00499.o}
procedure __o__mbscoll_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00500.o}
procedure __o__mbscpy_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00501.o}
procedure __o__mbscpy_s_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00502.o}
procedure __o__mbscspn;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00503.o}
procedure __o__mbscspn_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00504.o}
procedure __o__mbsdec;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00505.o}
procedure __o__mbsdec_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00506.o}
procedure __o__mbsicmp;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00507.o}
procedure __o__mbsicmp_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00508.o}
procedure __o__mbsicoll;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00509.o}
procedure __o__mbsicoll_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00510.o}
procedure __o__mbsinc;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00511.o}
procedure __o__mbsinc_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00512.o}
procedure __o__mbslen;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00513.o}
procedure __o__mbslen_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00514.o}
procedure __o__mbslwr;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00515.o}
procedure __o__mbslwr_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00516.o}
procedure __o__mbslwr_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00517.o}
procedure __o__mbslwr_s_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00518.o}
procedure __o__mbsnbcat;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00519.o}
procedure __o__mbsnbcat_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00520.o}
procedure __o__mbsnbcat_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00521.o}
procedure __o__mbsnbcat_s_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00522.o}
procedure __o__mbsnbcmp;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00523.o}
procedure __o__mbsnbcmp_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00524.o}
procedure __o__mbsnbcnt;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00525.o}
procedure __o__mbsnbcnt_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00526.o}
procedure __o__mbsnbcoll;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00527.o}
procedure __o__mbsnbcoll_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00528.o}
procedure __o__mbsnbcpy;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00529.o}
procedure __o__mbsnbcpy_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00530.o}
procedure __o__mbsnbcpy_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00531.o}
procedure __o__mbsnbcpy_s_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00532.o}
procedure __o__mbsnbicmp;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00533.o}
procedure __o__mbsnbicmp_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00534.o}
procedure __o__mbsnbicoll;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00535.o}
procedure __o__mbsnbicoll_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00536.o}
procedure __o__mbsnbset;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00537.o}
procedure __o__mbsnbset_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00538.o}
procedure __o__mbsnbset_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00539.o}
procedure __o__mbsnbset_s_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00540.o}
procedure __o__mbsncat;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00541.o}
procedure __o__mbsncat_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00542.o}
procedure __o__mbsncat_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00543.o}
procedure __o__mbsncat_s_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00544.o}
procedure __o__mbsnccnt;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00545.o}
procedure __o__mbsnccnt_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00546.o}
procedure __o__mbsncmp;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00547.o}
procedure __o__mbsncmp_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00548.o}
procedure __o__mbsncoll;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00549.o}
procedure __o__mbsncoll_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00550.o}
procedure __o__mbsncpy;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00551.o}
procedure __o__mbsncpy_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00552.o}
procedure __o__mbsncpy_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00553.o}
procedure __o__mbsncpy_s_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00554.o}
procedure __o__mbsnextc;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00555.o}
procedure __o__mbsnextc_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00556.o}
procedure __o__mbsnicmp;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00557.o}
procedure __o__mbsnicmp_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00558.o}
procedure __o__mbsnicoll;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00559.o}
procedure __o__mbsnicoll_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00560.o}
procedure __o__mbsninc;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00561.o}
procedure __o__mbsninc_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00562.o}
procedure __o__mbsnlen;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00563.o}
procedure __o__mbsnlen_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00564.o}
procedure __o__mbsnset;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00565.o}
procedure __o__mbsnset_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00566.o}
procedure __o__mbsnset_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00567.o}
procedure __o__mbsnset_s_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00568.o}
procedure __o__mbspbrk;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00569.o}
procedure __o__mbspbrk_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00570.o}
procedure __o__mbsrchr;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00571.o}
procedure __o__mbsrchr_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00572.o}
procedure __o__mbsrev;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00573.o}
procedure __o__mbsrev_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00574.o}
procedure __o__mbsset;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00575.o}
procedure __o__mbsset_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00576.o}
procedure __o__mbsset_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00577.o}
procedure __o__mbsset_s_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00578.o}
procedure __o__mbsspn;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00579.o}
procedure __o__mbsspn_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00580.o}
procedure __o__mbsspnp;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00581.o}
procedure __o__mbsspnp_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00582.o}
procedure __o__mbsstr;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00583.o}
procedure __o__mbsstr_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00584.o}
procedure __o__mbstok;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00585.o}
procedure __o__mbstok_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00586.o}
procedure __o__mbstok_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00587.o}
procedure __o__mbstok_s_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00588.o}
procedure __o__mbstowcs_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00589.o}
procedure __o__mbstowcs_s_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00590.o}
procedure __o__mbstrlen;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00591.o}
procedure __o__mbstrlen_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00592.o}
procedure __o__mbstrnlen;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00593.o}
procedure __o__mbstrnlen_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00594.o}
procedure __o__mbsupr;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00595.o}
procedure __o__mbsupr_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00596.o}
procedure __o__mbsupr_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00597.o}
procedure __o__mbsupr_s_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00598.o}
procedure __o__mbtowc_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00599.o}
procedure __o__memicmp;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00600.o}
procedure __o__memicmp_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00601.o}
procedure __o__mkdir;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00602.o}
procedure __o__mkgmtime32;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00603.o}
procedure __o__mkgmtime64;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00604.o}
procedure __o__mktemp;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00605.o}
procedure __o__mktemp_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00606.o}
procedure __o__mktime32;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00607.o}
procedure __o__mktime64;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00608.o}
procedure __o__msize;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00609.o}
procedure __o__nextafter;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00610.o}
procedure __o__nextafterf;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00611.o}
procedure __o__open_osfhandle;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00612.o}
procedure __o__pclose;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00613.o}
procedure __o__pipe;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00614.o}
procedure __o__popen;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00615.o}
procedure __o__putc_nolock;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00616.o}
procedure __o__putch;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00617.o}
procedure __o__putch_nolock;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00618.o}
procedure __o__putenv;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00619.o}
procedure __o__putenv_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00620.o}
procedure __o__putw;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00621.o}
procedure __o__putwc_nolock;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00622.o}
procedure __o__putwch;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00623.o}
procedure __o__putwch_nolock;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00624.o}
procedure __o__putws;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00625.o}
procedure __o__read;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00626.o}
procedure __o__realloc_base;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00627.o}
procedure __o__recalloc;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00628.o}
procedure __o__register_onexit_function;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00629.o}
procedure __o__resetstkoflw;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00630.o}
procedure __o__rmdir;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00631.o}
procedure __o__rmtmp;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00632.o}
procedure __o__scalb;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00633.o}
procedure __o__scalbf;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00634.o}
procedure __o__searchenv;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00635.o}
procedure __o__searchenv_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00636.o}
procedure __o__set_abort_behavior;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00637.o}
procedure __o__set_doserrno;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00638.o}
procedure __o__set_errno;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00639.o}
procedure __o__set_invalid_parameter_handler;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00640.o}
procedure __o__set_new_handler;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00641.o}
procedure __o__set_new_mode;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00642.o}
procedure __o__set_thread_local_invalid_parameter_handler;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00643.o}
procedure __o__seterrormode;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00644.o}
procedure __o__setmbcp;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00645.o}
procedure __o__setmode;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00646.o}
procedure __o__setsystime;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00647.o}
procedure __o__sleep;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00648.o}
procedure __o__sopen;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00649.o}
procedure __o__sopen_dispatch;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00650.o}
procedure __o__sopen_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00651.o}
procedure __o__spawnv;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00652.o}
procedure __o__spawnve;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00653.o}
procedure __o__spawnvp;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00654.o}
procedure __o__spawnvpe;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00655.o}
procedure __o__splitpath;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00656.o}
procedure __o__splitpath_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00657.o}
procedure __o__stat32;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00658.o}
procedure __o__stat32i64;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00659.o}
procedure __o__stat64;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00660.o}
procedure __o__stat64i32;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00661.o}
procedure __o__strcoll_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00662.o}
procedure __o__strdate;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00663.o}
procedure __o__strdate_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00664.o}
procedure __o__strdup;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00665.o}
procedure __o__strerror;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00666.o}
procedure __o__strerror_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00667.o}
procedure __o__strftime_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00668.o}
procedure __o__stricmp;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00669.o}
procedure __o__stricmp_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00670.o}
procedure __o__stricoll;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00671.o}
procedure __o__stricoll_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00672.o}
procedure __o__strlwr;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00673.o}
procedure __o__strlwr_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00674.o}
procedure __o__strlwr_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00675.o}
procedure __o__strlwr_s_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00676.o}
procedure __o__strncoll;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00677.o}
procedure __o__strncoll_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00678.o}
procedure __o__strnicmp;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00679.o}
procedure __o__strnicmp_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00680.o}
procedure __o__strnicoll;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00681.o}
procedure __o__strnicoll_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00682.o}
procedure __o__strnset_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00683.o}
procedure __o__strset_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00684.o}
procedure __o__strtime;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00685.o}
procedure __o__strtime_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00686.o}
procedure __o__strtod_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00687.o}
procedure __o__strtof_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00688.o}
procedure __o__strtoi64;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00689.o}
procedure __o__strtoi64_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00690.o}
procedure __o__strtol_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00691.o}
procedure __o__strtold_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00692.o}
procedure __o__strtoll_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00693.o}
procedure __o__strtoui64;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00694.o}
procedure __o__strtoui64_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00695.o}
procedure __o__strtoul_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00696.o}
procedure __o__strtoull_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00697.o}
procedure __o__strupr;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00698.o}
procedure __o__strupr_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00699.o}
procedure __o__strupr_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00700.o}
procedure __o__strupr_s_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00701.o}
procedure __o__strxfrm_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00702.o}
procedure __o__swab;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00703.o}
procedure __o__tell;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00704.o}
procedure __o__telli64;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00705.o}
procedure __o__timespec32_get;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00706.o}
procedure __o__timespec64_get;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00707.o}
procedure __o__tolower;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00708.o}
procedure __o__tolower_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00709.o}
procedure __o__toupper;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00710.o}
procedure __o__toupper_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00711.o}
procedure __o__towlower_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00712.o}
procedure __o__towupper_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00713.o}
procedure __o__tzset;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00714.o}
procedure __o__ui64toa;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00715.o}
procedure __o__ui64toa_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00716.o}
procedure __o__ui64tow;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00717.o}
procedure __o__ui64tow_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00718.o}
procedure __o__ultoa;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00719.o}
procedure __o__ultoa_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00720.o}
procedure __o__ultow;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00721.o}
procedure __o__ultow_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00722.o}
procedure __o__umask;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00723.o}
procedure __o__umask_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00724.o}
procedure __o__ungetc_nolock;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00725.o}
procedure __o__ungetch;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00726.o}
procedure __o__ungetch_nolock;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00727.o}
procedure __o__ungetwc_nolock;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00728.o}
procedure __o__ungetwch;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00729.o}
procedure __o__ungetwch_nolock;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00730.o}
procedure __o__unlink;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00731.o}
procedure __o__unloaddll;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00732.o}
procedure __o__unlock_file;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00733.o}
procedure __o__utime32;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00734.o}
procedure __o__utime64;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00735.o}
procedure __o__waccess;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00736.o}
procedure __o__waccess_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00737.o}
procedure __o__wasctime;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00738.o}
procedure __o__wasctime_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00739.o}
procedure __o__wchdir;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00740.o}
procedure __o__wchmod;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00741.o}
procedure __o__wcreat;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00742.o}
procedure __o__wcreate_locale;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00743.o}
procedure __o__wcscoll_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00744.o}
procedure __o__wcsdup;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00745.o}
procedure __o__wcserror;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00746.o}
procedure __o__wcserror_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00747.o}
procedure __o__wcsftime_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00748.o}
procedure __o__wcsicmp;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00749.o}
procedure __o__wcsicmp_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00750.o}
procedure __o__wcsicoll;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00751.o}
procedure __o__wcsicoll_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00752.o}
procedure __o__wcslwr;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00753.o}
procedure __o__wcslwr_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00754.o}
procedure __o__wcslwr_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00755.o}
procedure __o__wcslwr_s_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00756.o}
procedure __o__wcsncoll;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00757.o}
procedure __o__wcsncoll_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00758.o}
procedure __o__wcsnicmp;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00759.o}
procedure __o__wcsnicmp_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00760.o}
procedure __o__wcsnicoll;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00761.o}
procedure __o__wcsnicoll_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00762.o}
procedure __o__wcsnset;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00763.o}
procedure __o__wcsnset_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00764.o}
procedure __o__wcsset;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00765.o}
procedure __o__wcsset_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00766.o}
procedure __o__wcstod_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00767.o}
procedure __o__wcstof_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00768.o}
procedure __o__wcstoi64;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00769.o}
procedure __o__wcstoi64_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00770.o}
procedure __o__wcstol_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00771.o}
procedure __o__wcstold_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00772.o}
procedure __o__wcstoll_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00773.o}
procedure __o__wcstombs_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00774.o}
procedure __o__wcstombs_s_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00775.o}
procedure __o__wcstoui64;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00776.o}
procedure __o__wcstoui64_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00777.o}
procedure __o__wcstoul_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00778.o}
procedure __o__wcstoull_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00779.o}
procedure __o__wcsupr;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00780.o}
procedure __o__wcsupr_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00781.o}
procedure __o__wcsupr_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00782.o}
procedure __o__wcsupr_s_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00783.o}
procedure __o__wcsxfrm_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00784.o}
procedure __o__wctime32;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00785.o}
procedure __o__wctime32_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00786.o}
procedure __o__wctime64;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00787.o}
procedure __o__wctime64_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00788.o}
procedure __o__wctomb_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00789.o}
procedure __o__wctomb_s_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00790.o}
procedure __o__wdupenv_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00791.o}
procedure __o__wexecv;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00792.o}
procedure __o__wexecve;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00793.o}
procedure __o__wexecvp;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00794.o}
procedure __o__wexecvpe;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00795.o}
procedure __o__wfdopen;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00796.o}
procedure __o__wfindfirst32;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00797.o}
procedure __o__wfindfirst32i64;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00798.o}
procedure __o__wfindfirst64;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00799.o}
procedure __o__wfindfirst64i32;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00800.o}
procedure __o__wfindnext32;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00801.o}
procedure __o__wfindnext32i64;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00802.o}
procedure __o__wfindnext64;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00803.o}
procedure __o__wfindnext64i32;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00804.o}
procedure __o__wfopen;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00805.o}
procedure __o__wfopen_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00806.o}
procedure __o__wfreopen;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00807.o}
procedure __o__wfreopen_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00808.o}
procedure __o__wfsopen;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00809.o}
procedure __o__wfullpath;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00810.o}
procedure __o__wgetcwd;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00811.o}
procedure __o__wgetdcwd;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00812.o}
procedure __o__wgetenv;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00813.o}
procedure __o__wgetenv_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00814.o}
procedure __o__wmakepath;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00815.o}
procedure __o__wmakepath_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00816.o}
procedure __o__wmkdir;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00817.o}
procedure __o__wmktemp;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00818.o}
procedure __o__wmktemp_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00819.o}
procedure __o__wperror;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00820.o}
procedure __o__wpopen;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00821.o}
procedure __o__wputenv;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00822.o}
procedure __o__wputenv_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00823.o}
procedure __o__wremove;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00824.o}
procedure __o__wrename;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00825.o}
procedure __o__write;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00826.o}
procedure __o__wrmdir;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00827.o}
procedure __o__wsearchenv;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00828.o}
procedure __o__wsearchenv_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00829.o}
procedure __o__wsetlocale;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00830.o}
procedure __o__wsopen_dispatch;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00831.o}
procedure __o__wsopen_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00832.o}
procedure __o__wspawnv;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00833.o}
procedure __o__wspawnve;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00834.o}
procedure __o__wspawnvp;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00835.o}
procedure __o__wspawnvpe;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00836.o}
procedure __o__wsplitpath;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00837.o}
procedure __o__wsplitpath_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00838.o}
procedure __o__wstat32;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00839.o}
procedure __o__wstat32i64;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00840.o}
procedure __o__wstat64;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00841.o}
procedure __o__wstat64i32;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00842.o}
procedure __o__wstrdate;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00843.o}
procedure __o__wstrdate_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00844.o}
procedure __o__wstrtime;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00845.o}
procedure __o__wstrtime_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00846.o}
procedure __o__wsystem;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00847.o}
procedure __o__wtmpnam_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00848.o}
procedure __o__wtof;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00849.o}
procedure __o__wtof_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00850.o}
procedure __o__wtoi;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00851.o}
procedure __o__wtoi64;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00852.o}
procedure __o__wtoi64_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00853.o}
procedure __o__wtoi_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00854.o}
procedure __o__wtol;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00855.o}
procedure __o__wtol_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00856.o}
procedure __o__wtoll;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00857.o}
procedure __o__wtoll_l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00858.o}
procedure __o__wunlink;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00859.o}
procedure __o__wutime32;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00860.o}
procedure __o__wutime64;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00861.o}
procedure __o__y0;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00862.o}
procedure __o__y1;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00863.o}
procedure __o__yn;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00864.o}
procedure __o_abort;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00865.o}
procedure __o_acos;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00866.o}
procedure __o_acosf;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00867.o}
procedure __o_acosh;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00868.o}
procedure __o_acoshf;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00869.o}
procedure __o_acoshl;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00870.o}
procedure __o_asctime;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00871.o}
procedure __o_asctime_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00872.o}
procedure __o_asin;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00873.o}
procedure __o_asinf;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00874.o}
procedure __o_asinh;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00875.o}
procedure __o_asinhf;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00876.o}
procedure __o_asinhl;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00877.o}
procedure __o_atan;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00878.o}
procedure __o_atan2;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00879.o}
procedure __o_atan2f;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00880.o}
procedure __o_atanf;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00881.o}
procedure __o_atanh;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00882.o}
procedure __o_atanhf;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00883.o}
procedure __o_atanhl;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00884.o}
procedure __o_atof;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00885.o}
procedure __o_atoi;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00886.o}
procedure __o_atol;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00887.o}
procedure __o_atoll;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00888.o}
procedure __o_bsearch;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00889.o}
procedure __o_bsearch_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00890.o}
procedure __o_btowc;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00891.o}
procedure __o_calloc;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00892.o}
procedure __o_cbrt;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00893.o}
procedure __o_cbrtf;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00894.o}
procedure __o_ceil;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00895.o}
procedure __o_ceilf;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00896.o}
procedure __o_clearerr;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00897.o}
procedure __o_clearerr_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00898.o}
procedure __o_cos;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00899.o}
procedure __o_cosf;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00900.o}
procedure __o_cosh;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00901.o}
procedure __o_coshf;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00902.o}
procedure __o_erf;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00903.o}
procedure __o_erfc;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00904.o}
procedure __o_erfcf;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00905.o}
procedure __o_erfcl;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00906.o}
procedure __o_erff;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00907.o}
procedure __o_erfl;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00908.o}
procedure __o_exp;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00909.o}
procedure __o_exp2;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00910.o}
procedure __o_exp2f;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00911.o}
procedure __o_exp2l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00912.o}
procedure __o_expf;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00913.o}
procedure __o_fabs;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00914.o}
procedure __o_fclose;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00915.o}
procedure __o_feof;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00916.o}
procedure __o_ferror;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00917.o}
procedure __o_fflush;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00918.o}
procedure __o_fgetc;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00919.o}
procedure __o_fgetpos;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00920.o}
procedure __o_fgets;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00921.o}
procedure __o_fgetwc;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00922.o}
procedure __o_fgetws;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00923.o}
procedure __o_floor;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00924.o}
procedure __o_floorf;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00925.o}
procedure __o_fma;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00926.o}
procedure __o_fmaf;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00927.o}
procedure __o_fmal;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00928.o}
procedure __o_fmod;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00929.o}
procedure __o_fmodf;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00930.o}
procedure __o_fopen;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00931.o}
procedure __o_fopen_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00932.o}
procedure __o_fputc;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00933.o}
procedure __o_fputs;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00934.o}
procedure __o_fputwc;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00935.o}
procedure __o_fputws;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00936.o}
procedure __o_fread;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00937.o}
procedure __o_fread_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00938.o}
procedure __o_free;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00939.o}
procedure __o_freopen;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00940.o}
procedure __o_freopen_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00941.o}
procedure __o_frexp;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00942.o}
procedure __o_fseek;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00943.o}
procedure __o_fsetpos;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00944.o}
procedure __o_ftell;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00945.o}
procedure __o_fwrite;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00946.o}
procedure __o_getc;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00947.o}
procedure __o_getchar;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00948.o}
procedure __o_getenv;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00949.o}
procedure __o_getenv_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00950.o}
procedure __o_gets;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00951.o}
procedure __o_gets_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00952.o}
procedure __o_getwc;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00953.o}
procedure __o_getwchar;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00954.o}
procedure __o_hypot;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00955.o}
procedure __o_is_wctype;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00956.o}
procedure __o_isalnum;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00957.o}
procedure __o_isalpha;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00958.o}
procedure __o_isblank;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00959.o}
procedure __o_iscntrl;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00960.o}
procedure __o_isdigit;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00961.o}
procedure __o_isgraph;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00962.o}
procedure __o_isleadbyte;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00963.o}
procedure __o_islower;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00964.o}
procedure __o_isprint;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00965.o}
procedure __o_ispunct;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00966.o}
procedure __o_isspace;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00967.o}
procedure __o_isupper;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00968.o}
procedure __o_iswalnum;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00969.o}
procedure __o_iswalpha;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00970.o}
procedure __o_iswascii;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00971.o}
procedure __o_iswblank;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00972.o}
procedure __o_iswcntrl;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00973.o}
procedure __o_iswctype;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00974.o}
procedure __o_iswdigit;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00975.o}
procedure __o_iswgraph;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00976.o}
procedure __o_iswlower;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00977.o}
procedure __o_iswprint;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00978.o}
procedure __o_iswpunct;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00979.o}
procedure __o_iswspace;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00980.o}
procedure __o_iswupper;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00981.o}
procedure __o_iswxdigit;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00982.o}
procedure __o_isxdigit;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00983.o}
procedure __o_ldexp;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00984.o}
procedure __o_lgamma;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00985.o}
procedure __o_lgammaf;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00986.o}
procedure __o_lgammal;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00987.o}
procedure __o_llrint;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00988.o}
procedure __o_llrintf;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00989.o}
procedure __o_llrintl;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00990.o}
procedure __o_llround;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00991.o}
procedure __o_llroundf;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00992.o}
procedure __o_llroundl;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00993.o}
procedure __o_localeconv;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00994.o}
procedure __o_log;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00995.o}
procedure __o_log10;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00996.o}
procedure __o_log10f;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00997.o}
procedure __o_log1p;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00998.o}
procedure __o_log1pf;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s00999.o}
procedure __o_log1pl;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01000.o}
procedure __o_log2;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01001.o}
procedure __o_log2f;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01002.o}
procedure __o_log2l;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01003.o}
procedure __o_logb;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01004.o}
procedure __o_logbf;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01005.o}
procedure __o_logbl;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01006.o}
procedure __o_logf;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01007.o}
procedure __o_lrint;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01008.o}
procedure __o_lrintf;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01009.o}
procedure __o_lrintl;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01010.o}
procedure __o_lround;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01011.o}
procedure __o_lroundf;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01012.o}
procedure __o_lroundl;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01013.o}
procedure __o_malloc;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01014.o}
procedure __o_mblen;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01015.o}
procedure __o_mbrlen;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01016.o}
procedure __o_mbrtoc16;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01017.o}
procedure __o_mbrtoc32;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01018.o}
procedure __o_mbrtowc;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01019.o}
procedure __o_mbsrtowcs;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01020.o}
procedure __o_mbsrtowcs_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01021.o}
procedure __o_mbstowcs;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01022.o}
procedure __o_mbstowcs_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01023.o}
procedure __o_mbtowc;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01024.o}
procedure __o_memset;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01025.o}
procedure __o_modf;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01026.o}
procedure __o_modff;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01027.o}
procedure __o_nan;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01028.o}
procedure __o_nanf;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01029.o}
procedure __o_nanl;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01030.o}
procedure __o_nearbyint;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01031.o}
procedure __o_nearbyintf;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01032.o}
procedure __o_nearbyintl;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01033.o}
procedure __o_nextafter;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01034.o}
procedure __o_nextafterf;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01035.o}
procedure __o_nextafterl;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01036.o}
procedure __o_nexttoward;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01037.o}
procedure __o_nexttowardf;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01038.o}
procedure __o_nexttowardl;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01039.o}
procedure __o_pow;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01040.o}
procedure __o_powf;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01041.o}
procedure __o_putc;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01042.o}
procedure __o_putchar;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01043.o}
procedure __o_puts;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01044.o}
procedure __o_putwc;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01045.o}
procedure __o_putwchar;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01046.o}
procedure __o_qsort;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01047.o}
procedure __o_qsort_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01048.o}
procedure __o_raise;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01049.o}
procedure __o_rand;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01050.o}
procedure __o_rand_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01051.o}
procedure __o_realloc;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01052.o}
procedure __o_remainder;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01053.o}
procedure __o_remainderf;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01054.o}
procedure __o_remainderl;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01055.o}
procedure __o_remove;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01056.o}
procedure __o_remquo;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01057.o}
procedure __o_remquof;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01058.o}
procedure __o_remquol;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01059.o}
procedure __o_rewind;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01060.o}
procedure __o_rint;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01061.o}
procedure __o_rintf;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01062.o}
procedure __o_rintl;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01063.o}
procedure __o_round;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01064.o}
procedure __o_roundf;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01065.o}
procedure __o_roundl;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01066.o}
procedure __o_scalbln;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01067.o}
procedure __o_scalblnf;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01068.o}
procedure __o_scalblnl;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01069.o}
procedure __o_scalbn;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01070.o}
procedure __o_scalbnf;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01071.o}
procedure __o_scalbnl;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01072.o}
procedure __o_set_terminate;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01073.o}
procedure __o_setbuf;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01074.o}
procedure __o_setvbuf;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01075.o}
procedure __o_sin;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01076.o}
procedure __o_sinf;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01077.o}
procedure __o_sinh;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01078.o}
procedure __o_sinhf;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01079.o}
procedure __o_sqrt;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01080.o}
procedure __o_sqrtf;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01081.o}
procedure __o_srand;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01082.o}
procedure __o_strcat_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01083.o}
procedure __o_strcoll;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01084.o}
procedure __o_strcpy_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01085.o}
procedure __o_strerror;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01086.o}
procedure __o_strerror_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01087.o}
procedure __o_strftime;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01088.o}
procedure __o_strncat_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01089.o}
procedure __o_strncpy_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01090.o}
procedure __o_strtod;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01091.o}
procedure __o_strtof;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01092.o}
procedure __o_strtok;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01093.o}
procedure __o_strtok_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01094.o}
procedure __o_strtol;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01095.o}
procedure __o_strtold;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01096.o}
procedure __o_strtoll;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01097.o}
procedure __o_strtoul;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01098.o}
procedure __o_strtoull;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01099.o}
procedure __o_system;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01100.o}
procedure __o_tan;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01101.o}
procedure __o_tanf;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01102.o}
procedure __o_tanh;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01103.o}
procedure __o_tanhf;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01104.o}
procedure __o_terminate;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01105.o}
procedure __o_tgamma;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01106.o}
procedure __o_tgammaf;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01107.o}
procedure __o_tgammal;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01108.o}
procedure __o_tmpfile_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01109.o}
procedure __o_tmpnam_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01110.o}
procedure __o_tolower;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01111.o}
procedure __o_toupper;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01112.o}
procedure __o_towlower;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01113.o}
procedure __o_towupper;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01114.o}
procedure __o_ungetc;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01115.o}
procedure __o_ungetwc;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01116.o}
procedure __o_wcrtomb;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01117.o}
procedure __o_wcrtomb_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01118.o}
procedure __o_wcscat_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01119.o}
procedure __o_wcscoll;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01120.o}
procedure __o_wcscpy;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01121.o}
procedure __o_wcscpy_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01122.o}
procedure __o_wcsftime;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01123.o}
procedure __o_wcsncat_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01124.o}
procedure __o_wcsncpy_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01125.o}
procedure __o_wcsrtombs;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01126.o}
procedure __o_wcsrtombs_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01127.o}
procedure __o_wcstod;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01128.o}
procedure __o_wcstof;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01129.o}
procedure __o_wcstok;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01130.o}
procedure __o_wcstok_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01131.o}
procedure __o_wcstol;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01132.o}
procedure __o_wcstold;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01133.o}
procedure __o_wcstoll;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01134.o}
procedure __o_wcstombs;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01135.o}
procedure __o_wcstombs_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01136.o}
procedure __o_wcstoul;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01137.o}
procedure __o_wcstoull;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01138.o}
procedure __o_wctob;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01139.o}
procedure __o_wctomb;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01140.o}
procedure __o_wctomb_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01141.o}
procedure __o_wmemcpy_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01142.o}
procedure __o_wmemmove_s;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01143.o}
procedure __purecall;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01144.o}
procedure __set_purecall_handler;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01147.o}
procedure __set_se_translator;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01148.o}
procedure __setjmp3;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01149.o}
procedure _longjmp;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01150.o}
procedure _memchr;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01151.o}
procedure _memcmp;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01152.o}
procedure _memcpy;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01153.o}
procedure _memmove;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01154.o}
procedure _set_unexpected;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01155.o}
procedure _strchr;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01156.o}
procedure _strrchr;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01157.o}
procedure _strstr;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01158.o}
procedure _unexpected;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01159.o}
procedure _wcschr;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01160.o}
procedure _wcsrchr;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01161.o}
procedure _wcsstr;external;
{$L x86/libapi-ms-win-crt-private-l1-1-0s01162.o}
procedure ___p__mbcasemap;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00000.o}
procedure ___p__mbctype;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00001.o}
procedure __ismbbalnum;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00002.o}
procedure __ismbbalnum_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00003.o}
procedure __ismbbalpha;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00004.o}
procedure __ismbbalpha_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00005.o}
procedure __ismbbblank;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00006.o}
procedure __ismbbblank_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00007.o}
procedure __ismbbgraph;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00008.o}
procedure __ismbbgraph_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00009.o}
procedure __ismbbkalnum;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00010.o}
procedure __ismbbkalnum_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00011.o}
procedure __ismbbkana;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00012.o}
procedure __ismbbkana_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00013.o}
procedure __ismbbkprint;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00014.o}
procedure __ismbbkprint_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00015.o}
procedure __ismbbkpunct;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00016.o}
procedure __ismbbkpunct_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00017.o}
procedure __ismbblead;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00018.o}
procedure __ismbblead_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00019.o}
procedure __ismbbprint;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00020.o}
procedure __ismbbprint_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00021.o}
procedure __ismbbpunct;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00022.o}
procedure __ismbbpunct_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00023.o}
procedure __ismbbtrail;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00024.o}
procedure __ismbbtrail_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00025.o}
procedure __ismbcalnum;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00026.o}
procedure __ismbcalnum_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00027.o}
procedure __ismbcalpha;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00028.o}
procedure __ismbcalpha_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00029.o}
procedure __ismbcblank;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00030.o}
procedure __ismbcblank_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00031.o}
procedure __ismbcdigit;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00032.o}
procedure __ismbcdigit_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00033.o}
procedure __ismbcgraph;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00034.o}
procedure __ismbcgraph_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00035.o}
procedure __ismbchira;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00036.o}
procedure __ismbchira_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00037.o}
procedure __ismbckata;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00038.o}
procedure __ismbckata_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00039.o}
procedure __ismbcl0;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00040.o}
procedure __ismbcl0_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00041.o}
procedure __ismbcl1;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00042.o}
procedure __ismbcl1_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00043.o}
procedure __ismbcl2;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00044.o}
procedure __ismbcl2_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00045.o}
procedure __ismbclegal;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00046.o}
procedure __ismbclegal_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00047.o}
procedure __ismbclower;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00048.o}
procedure __ismbclower_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00049.o}
procedure __ismbcprint;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00050.o}
procedure __ismbcprint_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00051.o}
procedure __ismbcpunct;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00052.o}
procedure __ismbcpunct_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00053.o}
procedure __ismbcspace;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00054.o}
procedure __ismbcspace_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00055.o}
procedure __ismbcsymbol;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00056.o}
procedure __ismbcsymbol_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00057.o}
procedure __ismbcupper;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00058.o}
procedure __ismbcupper_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00059.o}
procedure __ismbslead;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00060.o}
procedure __ismbslead_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00061.o}
procedure __ismbstrail;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00062.o}
procedure __ismbstrail_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00063.o}
procedure __mbbtombc;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00064.o}
procedure __mbbtombc_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00065.o}
procedure __mbbtype;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00066.o}
procedure __mbbtype_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00067.o}
procedure __mbccpy;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00069.o}
procedure __mbccpy_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00070.o}
procedure __mbccpy_s;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00071.o}
procedure __mbccpy_s_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00072.o}
procedure __mbcjistojms;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00073.o}
procedure __mbcjistojms_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00074.o}
procedure __mbcjmstojis;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00075.o}
procedure __mbcjmstojis_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00076.o}
procedure __mbclen;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00077.o}
procedure __mbclen_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00078.o}
procedure __mbctohira;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00079.o}
procedure __mbctohira_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00080.o}
procedure __mbctokata;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00081.o}
procedure __mbctokata_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00082.o}
procedure __mbctolower;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00083.o}
procedure __mbctolower_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00084.o}
procedure __mbctombb;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00085.o}
procedure __mbctombb_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00086.o}
procedure __mbctoupper;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00087.o}
procedure __mbctoupper_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00088.o}
procedure __mblen_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00089.o}
procedure __mbsbtype;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00090.o}
procedure __mbsbtype_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00091.o}
procedure __mbscat_s;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00092.o}
procedure __mbscat_s_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00093.o}
procedure __mbschr;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00094.o}
procedure __mbschr_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00095.o}
procedure __mbscmp;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00096.o}
procedure __mbscmp_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00097.o}
procedure __mbscoll;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00098.o}
procedure __mbscoll_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00099.o}
procedure __mbscpy_s;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00100.o}
procedure __mbscpy_s_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00101.o}
procedure __mbscspn;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00102.o}
procedure __mbscspn_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00103.o}
procedure __mbsdec;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00104.o}
procedure __mbsdec_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00105.o}
procedure __mbsdup;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00106.o}
procedure __mbsicmp;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00107.o}
procedure __mbsicmp_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00108.o}
procedure __mbsicoll;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00109.o}
procedure __mbsicoll_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00110.o}
procedure __mbsinc;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00111.o}
procedure __mbsinc_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00112.o}
procedure __mbslen;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00113.o}
procedure __mbslen_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00114.o}
procedure __mbslwr;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00115.o}
procedure __mbslwr_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00116.o}
procedure __mbslwr_s;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00117.o}
procedure __mbslwr_s_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00118.o}
procedure __mbsnbcat;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00119.o}
procedure __mbsnbcat_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00120.o}
procedure __mbsnbcat_s;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00121.o}
procedure __mbsnbcat_s_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00122.o}
procedure __mbsnbcmp;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00123.o}
procedure __mbsnbcmp_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00124.o}
procedure __mbsnbcnt;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00125.o}
procedure __mbsnbcnt_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00126.o}
procedure __mbsnbcoll;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00127.o}
procedure __mbsnbcoll_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00128.o}
procedure __mbsnbcpy;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00129.o}
procedure __mbsnbcpy_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00130.o}
procedure __mbsnbcpy_s;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00131.o}
procedure __mbsnbcpy_s_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00132.o}
procedure __mbsnbicmp;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00133.o}
procedure __mbsnbicmp_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00134.o}
procedure __mbsnbicoll;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00135.o}
procedure __mbsnbicoll_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00136.o}
procedure __mbsnbset;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00137.o}
procedure __mbsnbset_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00138.o}
procedure __mbsnbset_s;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00139.o}
procedure __mbsnbset_s_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00140.o}
procedure __mbsncat;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00141.o}
procedure __mbsncat_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00142.o}
procedure __mbsncat_s;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00143.o}
procedure __mbsncat_s_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00144.o}
procedure __mbsnccnt;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00145.o}
procedure __mbsnccnt_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00146.o}
procedure __mbsncmp;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00147.o}
procedure __mbsncmp_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00148.o}
procedure __mbsncoll;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00149.o}
procedure __mbsncoll_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00150.o}
procedure __mbsncpy;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00151.o}
procedure __mbsncpy_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00152.o}
procedure __mbsncpy_s;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00153.o}
procedure __mbsncpy_s_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00154.o}
procedure __mbsnextc;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00155.o}
procedure __mbsnextc_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00156.o}
procedure __mbsnicmp;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00157.o}
procedure __mbsnicmp_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00158.o}
procedure __mbsnicoll;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00159.o}
procedure __mbsnicoll_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00160.o}
procedure __mbsninc;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00161.o}
procedure __mbsninc_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00162.o}
procedure __mbsnlen;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00163.o}
procedure __mbsnlen_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00164.o}
procedure __mbsnset;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00165.o}
procedure __mbsnset_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00166.o}
procedure __mbsnset_s;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00167.o}
procedure __mbsnset_s_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00168.o}
procedure __mbspbrk;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00169.o}
procedure __mbspbrk_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00170.o}
procedure __mbsrchr;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00171.o}
procedure __mbsrchr_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00172.o}
procedure __mbsrev;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00173.o}
procedure __mbsrev_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00174.o}
procedure __mbsset;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00175.o}
procedure __mbsset_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00176.o}
procedure __mbsset_s;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00177.o}
procedure __mbsset_s_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00178.o}
procedure __mbsspn;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00179.o}
procedure __mbsspn_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00180.o}
procedure __mbsspnp;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00181.o}
procedure __mbsspnp_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00182.o}
procedure __mbsstr;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00183.o}
procedure __mbsstr_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00184.o}
procedure __mbstok;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00185.o}
procedure __mbstok_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00186.o}
procedure __mbstok_s;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00187.o}
procedure __mbstok_s_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00188.o}
procedure __mbstowcs_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00189.o}
procedure __mbstowcs_s_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00190.o}
procedure __mbstrlen;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00191.o}
procedure __mbstrlen_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00192.o}
procedure __mbstrnlen;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00193.o}
procedure __mbstrnlen_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00194.o}
procedure __mbsupr;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00195.o}
procedure __mbsupr_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00196.o}
procedure __mbsupr_s;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00197.o}
procedure __mbsupr_s_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00198.o}
procedure __mbtowc_l;external;
{$L x86/libapi-ms-win-crt-multibyte-l1-1-0s00199.o}
procedure __CIacos;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00000.o}
procedure __CIasin;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00001.o}
procedure __CIatan;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00002.o}
procedure __CIatan2;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00003.o}
procedure __CIcos;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00004.o}
procedure __CIcosh;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00005.o}
procedure __CIexp;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00006.o}
procedure __CIfmod;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00007.o}
procedure __CIlog;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00008.o}
procedure __CIlog10;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00009.o}
procedure __CIpow;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00010.o}
procedure __CIsin;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00011.o}
procedure __CIsinh;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00012.o}
procedure __CIsqrt;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00013.o}
procedure __CItan;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00014.o}
procedure __CItanh;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00015.o}
procedure __Cbuild;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00016.o}
procedure __Cmulcc;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00017.o}
procedure __Cmulcr;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00018.o}
procedure __FCbuild;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00019.o}
procedure __FCmulcc;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00020.o}
procedure __FCmulcr;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00021.o}
procedure __LCbuild;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00022.o}
procedure __LCmulcc;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00023.o}
procedure __LCmulcr;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00024.o}
procedure ___libm_sse2_acos;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00025.o}
procedure ___libm_sse2_acosf;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00026.o}
procedure ___libm_sse2_asin;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00027.o}
procedure ___libm_sse2_asinf;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00028.o}
procedure ___libm_sse2_atan;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00029.o}
procedure ___libm_sse2_atan2;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00030.o}
procedure ___libm_sse2_atanf;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00031.o}
procedure ___libm_sse2_cos;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00032.o}
procedure ___libm_sse2_cosf;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00033.o}
procedure ___libm_sse2_exp;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00034.o}
procedure ___libm_sse2_expf;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00035.o}
procedure ___libm_sse2_log;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00036.o}
procedure ___libm_sse2_log10;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00037.o}
procedure ___libm_sse2_log10f;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00038.o}
procedure ___libm_sse2_logf;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00039.o}
procedure ___libm_sse2_pow;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00040.o}
procedure ___libm_sse2_powf;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00041.o}
procedure ___libm_sse2_sin;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00042.o}
procedure ___libm_sse2_sinf;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00043.o}
procedure ___libm_sse2_tan;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00044.o}
procedure ___libm_sse2_tanf;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00045.o}
procedure ___setusermatherr;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00046.o}
procedure __chgsign;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00048.o}
procedure _chgsign;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00049.o}
procedure __chgsignf;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00050.o}
procedure __copysign;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00051.o}
procedure __copysignf;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00052.o}
procedure __d_int;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00053.o}
procedure __dclass;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00054.o}
procedure __dexp;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00055.o}
procedure __dlog;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00056.o}
procedure __dnorm;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00057.o}
procedure __dpcomp;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00058.o}
procedure __dpoly;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00059.o}
procedure __dscale;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00060.o}
procedure __dsign;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00061.o}
procedure __dsin;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00062.o}
procedure __dtest;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00063.o}
procedure __dunscale;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00064.o}
procedure __except1;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00065.o}
procedure __fd_int;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00066.o}
procedure __fdclass;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00067.o}
procedure __fdexp;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00068.o}
procedure __fdlog;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00069.o}
procedure __fdnorm;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00070.o}
procedure __fdopen;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00071.o}
procedure __fdpcomp;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00072.o}
procedure __fdpoly;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00073.o}
procedure _fdopen;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00074.o}
procedure __fdscale;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00075.o}
procedure __fdsign;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00076.o}
procedure __fdsin;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00077.o}
procedure __fdtest;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00078.o}
procedure __fdunscale;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00079.o}
procedure __finite;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00080.o}
procedure _finite;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00081.o}
procedure __fpclass;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00082.o}
procedure __fpclassf;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00083.o}
procedure __ftol;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00084.o}
procedure _fpclass;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00085.o}
procedure __get_FMA3_enable;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00086.o}
procedure __hypot;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00087.o}
procedure __hypotf;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00088.o}
procedure __isnan;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00089.o}
procedure _hypot;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00090.o}
procedure __j0;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00091.o}
procedure _j0;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00092.o}
procedure __j1;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00093.o}
procedure _jn;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00094.o}
procedure __jn;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00095.o}
procedure __ld_int;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00096.o}
procedure __ldclass;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00097.o}
procedure __ldexp;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00098.o}
procedure __ldlog;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00099.o}
procedure _j1;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00100.o}
procedure __ldpcomp;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00101.o}
procedure __ldpoly;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00102.o}
procedure __ldscale;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00103.o}
procedure __ldsign;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00104.o}
procedure __ldsin;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00105.o}
procedure __ldtest;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00106.o}
procedure __ldunscale;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00107.o}
procedure __libm_sse2_acos_precise;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00108.o}
procedure __libm_sse2_asin_precise;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00109.o}
procedure __libm_sse2_atan_precise;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00110.o}
procedure __libm_sse2_cos_precise;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00111.o}
procedure __libm_sse2_exp_precise;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00112.o}
procedure __libm_sse2_log10_precise;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00113.o}
procedure __libm_sse2_log_precise;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00114.o}
procedure __libm_sse2_pow_precise;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00115.o}
procedure __libm_sse2_sin_precise;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00116.o}
procedure __libm_sse2_sqrt_precise;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00117.o}
procedure __libm_sse2_tan_precise;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00118.o}
procedure __logb;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00119.o}
procedure __nextafter;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00120.o}
procedure _nextafter;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00121.o}
procedure __scalb;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00122.o}
procedure __set_SSE2_enable;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00123.o}
procedure __y0;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00124.o}
procedure _y0;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00125.o}
procedure __y1;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00126.o}
procedure _y1;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00127.o}
procedure __yn;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00128.o}
procedure _acos;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00129.o}
procedure _yn;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00130.o}
procedure _acosh;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00131.o}
procedure _acoshf;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00132.o}
procedure _asin;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00134.o}
procedure _asinh;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00135.o}
procedure _asinhf;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00136.o}
procedure _atan;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00138.o}
procedure _atanh;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00140.o}
procedure _atanhf;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00141.o}
procedure _cabs;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00143.o}
procedure _cabsf;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00144.o}
procedure _cabsl;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00145.o}
procedure _cacos;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00146.o}
procedure _cacosf;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00147.o}
procedure _cacosh;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00148.o}
procedure _cacoshf;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00149.o}
procedure _cacoshl;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00150.o}
procedure _cacosl;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00151.o}
procedure _carg;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00152.o}
procedure _cargf;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00153.o}
procedure _cargl;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00154.o}
procedure _casin;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00155.o}
procedure _casinf;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00156.o}
procedure _casinh;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00157.o}
procedure _casinhf;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00158.o}
procedure _casinhl;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00159.o}
procedure _casinl;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00160.o}
procedure _catan;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00161.o}
procedure _catanf;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00162.o}
procedure _catanh;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00163.o}
procedure _catanhf;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00164.o}
procedure _catanhl;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00165.o}
procedure _catanl;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00166.o}
procedure _cbrt;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00167.o}
procedure _cbrtf;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00168.o}
procedure _ccos;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00170.o}
procedure _ccosf;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00171.o}
procedure _ccosh;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00172.o}
procedure _ccoshf;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00173.o}
procedure _ccoshl;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00174.o}
procedure _ccosl;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00175.o}
procedure _cexp;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00177.o}
procedure _cexpf;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00178.o}
procedure _cexpl;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00179.o}
procedure _cimag;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00180.o}
procedure _cimagf;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00181.o}
procedure _cimagl;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00182.o}
procedure _clog;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00183.o}
procedure _clog10;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00184.o}
procedure _clog10f;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00185.o}
procedure _clog10l;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00186.o}
procedure _clogf;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00187.o}
procedure _clogl;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00188.o}
procedure _conj;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00189.o}
procedure _conjf;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00190.o}
procedure _conjl;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00191.o}
procedure _copysign;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00192.o}
procedure _copysignf;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00193.o}
procedure _cosh;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00196.o}
procedure _cpow;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00197.o}
procedure _cpowf;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00198.o}
procedure _cpowl;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00199.o}
procedure _cproj;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00200.o}
procedure _cprojf;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00201.o}
procedure _cprojl;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00202.o}
procedure _creal;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00203.o}
procedure _crealf;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00204.o}
procedure _creall;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00205.o}
procedure _csin;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00206.o}
procedure _csinf;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00207.o}
procedure _csinh;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00208.o}
procedure _csinhf;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00209.o}
procedure _csinhl;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00210.o}
procedure _csinl;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00211.o}
procedure _csqrt;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00212.o}
procedure _csqrtf;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00213.o}
procedure _csqrtl;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00214.o}
procedure _ctan;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00215.o}
procedure _ctanf;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00216.o}
procedure _ctanh;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00217.o}
procedure _ctanhf;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00218.o}
procedure _ctanhl;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00219.o}
procedure _ctanl;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00220.o}
procedure _erf;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00221.o}
procedure _erfc;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00222.o}
procedure _erfcf;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00223.o}
procedure _erff;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00225.o}
procedure _exp2;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00228.o}
procedure _exp2f;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00229.o}
procedure _expm1;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00231.o}
procedure _expm1f;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00232.o}
procedure _fdim;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00235.o}
procedure _fdimf;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00236.o}
procedure _fma;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00239.o}
procedure _fmaf;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00240.o}
procedure _fmax;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00242.o}
procedure _fmaxf;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00243.o}
procedure _fmin;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00245.o}
procedure _fminf;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00246.o}
procedure _frexp;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00249.o}
procedure _ilogb;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00251.o}
procedure _ilogbf;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00252.o}
procedure _llrint;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00258.o}
procedure _llrintf;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00259.o}
procedure _llround;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00261.o}
procedure _llroundf;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00262.o}
procedure _log10;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00265.o}
procedure _log1p;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00266.o}
procedure _log1pf;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00267.o}
procedure _log2;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00269.o}
procedure _log2f;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00270.o}
procedure _logb;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00272.o}
procedure _logbf;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00273.o}
procedure _lrint;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00275.o}
procedure _lrintf;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00276.o}
procedure _lround;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00278.o}
procedure _lroundf;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00279.o}
procedure _nan;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00282.o}
procedure _nanf;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00283.o}
procedure _nearbyint;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00285.o}
procedure _nearbyintf;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00286.o}
procedure _nextafterf;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00289.o}
procedure _norm;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00294.o}
procedure _normf;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00295.o}
procedure _norml;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00296.o}
procedure _remainder;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00298.o}
procedure _remainderf;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00299.o}
procedure _remquo;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00301.o}
procedure _remquof;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00302.o}
procedure _rint;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00304.o}
procedure _rintf;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00305.o}
procedure _round;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00307.o}
procedure _roundf;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00308.o}
procedure _scalbln;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00310.o}
procedure _scalblnf;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00311.o}
procedure _scalbn;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00313.o}
procedure _scalbnf;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00314.o}
procedure _sinh;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00317.o}
procedure _tan;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00319.o}
procedure _tanh;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00320.o}
procedure _tgamma;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00321.o}
procedure _tgammaf;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00322.o}
procedure _trunc;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00324.o}
procedure _truncf;external;
{$L x86/libapi-ms-win-crt-math-l1-1-0s00325.o}
procedure ____lc_codepage_func;external;
{$L x86/libapi-ms-win-crt-locale-l1-1-0s00000.o}
procedure ____lc_collate_cp_func;external;
{$L x86/libapi-ms-win-crt-locale-l1-1-0s00001.o}
procedure ____lc_locale_name_func;external;
{$L x86/libapi-ms-win-crt-locale-l1-1-0s00002.o}
procedure ____mb_cur_max_func;external;
{$L x86/libapi-ms-win-crt-locale-l1-1-0s00003.o}
procedure ____mb_cur_max_l_func;external;
{$L x86/libapi-ms-win-crt-locale-l1-1-0s00004.o}
procedure ___initialize_lconv_for_unsigned_char;external;
{$L x86/libapi-ms-win-crt-locale-l1-1-0s00005.o}
procedure ___lconv_init;external;
{$L x86/libapi-ms-win-crt-locale-l1-1-0s00006.o}
procedure ___pctype_func;external;
{$L x86/libapi-ms-win-crt-locale-l1-1-0s00007.o}
procedure ___pwctype_func;external;
{$L x86/libapi-ms-win-crt-locale-l1-1-0s00008.o}
procedure __configthreadlocale;external;
{$L x86/libapi-ms-win-crt-locale-l1-1-0s00009.o}
procedure __create_locale;external;
{$L x86/libapi-ms-win-crt-locale-l1-1-0s00010.o}
procedure __free_locale;external;
{$L x86/libapi-ms-win-crt-locale-l1-1-0s00011.o}
procedure __get_current_locale;external;
{$L x86/libapi-ms-win-crt-locale-l1-1-0s00012.o}
procedure __getmbcp;external;
{$L x86/libapi-ms-win-crt-locale-l1-1-0s00013.o}
procedure __lock_locales;external;
{$L x86/libapi-ms-win-crt-locale-l1-1-0s00014.o}
procedure __setmbcp;external;
{$L x86/libapi-ms-win-crt-locale-l1-1-0s00015.o}
procedure __unlock_locales;external;
{$L x86/libapi-ms-win-crt-locale-l1-1-0s00016.o}
procedure __wcreate_locale;external;
{$L x86/libapi-ms-win-crt-locale-l1-1-0s00017.o}
procedure __wsetlocale;external;
{$L x86/libapi-ms-win-crt-locale-l1-1-0s00018.o}
procedure _localeconv;external;
{$L x86/libapi-ms-win-crt-locale-l1-1-0s00019.o}
procedure _setlocale;external;
{$L x86/libapi-ms-win-crt-locale-l1-1-0s00020.o}
procedure __aligned_free;external;
{$L x86/libapi-ms-win-crt-heap-l1-1-0s00000.o}
procedure __aligned_malloc;external;
{$L x86/libapi-ms-win-crt-heap-l1-1-0s00001.o}
procedure __aligned_msize;external;
{$L x86/libapi-ms-win-crt-heap-l1-1-0s00002.o}
procedure __aligned_offset_malloc;external;
{$L x86/libapi-ms-win-crt-heap-l1-1-0s00003.o}
procedure __aligned_offset_realloc;external;
{$L x86/libapi-ms-win-crt-heap-l1-1-0s00004.o}
procedure __aligned_offset_recalloc;external;
{$L x86/libapi-ms-win-crt-heap-l1-1-0s00005.o}
procedure __aligned_realloc;external;
{$L x86/libapi-ms-win-crt-heap-l1-1-0s00006.o}
procedure __aligned_recalloc;external;
{$L x86/libapi-ms-win-crt-heap-l1-1-0s00007.o}
procedure __callnewh;external;
{$L x86/libapi-ms-win-crt-heap-l1-1-0s00008.o}
procedure __calloc_base;external;
{$L x86/libapi-ms-win-crt-heap-l1-1-0s00009.o}
procedure __expand;external;
{$L x86/libapi-ms-win-crt-heap-l1-1-0s00010.o}
procedure __free_base;external;
{$L x86/libapi-ms-win-crt-heap-l1-1-0s00011.o}
procedure __get_heap_handle;external;
{$L x86/libapi-ms-win-crt-heap-l1-1-0s00012.o}
procedure __heapchk;external;
{$L x86/libapi-ms-win-crt-heap-l1-1-0s00013.o}
procedure __heapmin;external;
{$L x86/libapi-ms-win-crt-heap-l1-1-0s00014.o}
procedure __heapwalk;external;
{$L x86/libapi-ms-win-crt-heap-l1-1-0s00015.o}
procedure __malloc_base;external;
{$L x86/libapi-ms-win-crt-heap-l1-1-0s00016.o}
procedure __msize;external;
{$L x86/libapi-ms-win-crt-heap-l1-1-0s00017.o}
procedure __query_new_handler;external;
{$L x86/libapi-ms-win-crt-heap-l1-1-0s00018.o}
procedure __query_new_mode;external;
{$L x86/libapi-ms-win-crt-heap-l1-1-0s00019.o}
procedure _heapwalk;external;
{$L x86/libapi-ms-win-crt-heap-l1-1-0s00020.o}
procedure __realloc_base;external;
{$L x86/libapi-ms-win-crt-heap-l1-1-0s00021.o}
procedure __recalloc;external;
{$L x86/libapi-ms-win-crt-heap-l1-1-0s00022.o}
procedure __set_new_mode;external;
{$L x86/libapi-ms-win-crt-heap-l1-1-0s00023.o}
procedure __findclose;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00000.o}
procedure __findfirst;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00001.o}
procedure _access;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00002.o}
procedure __access;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00003.o}
procedure __access_s;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00004.o}
procedure __chdir;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00005.o}
procedure __chdrive;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00006.o}
procedure __chmod;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00007.o}
procedure _chmod;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00008.o}
procedure _chdir;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00009.o}
procedure __findfirst32;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00010.o}
procedure __findfirst32i64;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00011.o}
procedure __findfirst64;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00012.o}
procedure __findfirst64i32;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00013.o}
procedure __findnext;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00014.o}
procedure __findnext32;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00015.o}
procedure __findnext32i64;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00016.o}
procedure __findnext64;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00017.o}
procedure __findnext64i32;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00018.o}
procedure __fstat32;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00019.o}
procedure __fstat32i64;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00020.o}
procedure __fstat64;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00021.o}
procedure __fstat64i32;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00022.o}
procedure __fullpath;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00023.o}
procedure __getdiskfree;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00024.o}
procedure __getdrive;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00025.o}
procedure __getdrives;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00026.o}
procedure __lock_file;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00027.o}
procedure __makepath;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00028.o}
procedure __makepath_s;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00029.o}
procedure __mkdir;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00030.o}
procedure _rmdir;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00031.o}
procedure __rmdir;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00032.o}
procedure __splitpath;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00033.o}
procedure __splitpath_s;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00034.o}
procedure __stat32;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00035.o}
procedure _mkdir;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00036.o}
procedure __stat32i64;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00037.o}
procedure __stat64;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00038.o}
procedure __stat64i32;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00039.o}
procedure __umask;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00040.o}
procedure _umask;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00041.o}
procedure __umask_s;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00042.o}
procedure __unlink;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00043.o}
procedure __unlock_file;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00044.o}
procedure __waccess;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00045.o}
procedure _unlink;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00046.o}
procedure __waccess_s;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00047.o}
procedure __wchdir;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00048.o}
procedure __wchmod;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00049.o}
procedure __wfindfirst32;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00050.o}
procedure __wfindfirst32i64;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00051.o}
procedure __wfindfirst64;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00052.o}
procedure __wfindfirst64i32;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00053.o}
procedure __wfindnext32;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00054.o}
procedure __wfindnext32i64;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00055.o}
procedure __wfindnext64;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00056.o}
procedure __wfindnext64i32;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00057.o}
procedure __wfullpath;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00058.o}
procedure __wmakepath;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00059.o}
procedure __wmakepath_s;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00060.o}
procedure __wmkdir;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00061.o}
procedure __wremove;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00062.o}
procedure __wrename;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00063.o}
procedure __wrmdir;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00064.o}
procedure __wsplitpath;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00065.o}
procedure __wsplitpath_s;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00066.o}
procedure __wstat32;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00067.o}
procedure __wstat32i64;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00068.o}
procedure __wstat64;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00069.o}
procedure __wstat64i32;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00070.o}
procedure __wunlink;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00071.o}
procedure _remove;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00072.o}
procedure _rename;external;
{$L x86/libapi-ms-win-crt-filesystem-l1-1-0s00073.o}
procedure ___p__environ;external;
{$L x86/libapi-ms-win-crt-environment-l1-1-0s00000.o}
procedure ___p__wenviron;external;
{$L x86/libapi-ms-win-crt-environment-l1-1-0s00001.o}
procedure __dupenv_s;external;
{$L x86/libapi-ms-win-crt-environment-l1-1-0s00002.o}
procedure __putenv;external;
{$L x86/libapi-ms-win-crt-environment-l1-1-0s00003.o}
procedure _putenv;external;
{$L x86/libapi-ms-win-crt-environment-l1-1-0s00004.o}
procedure __putenv_s;external;
{$L x86/libapi-ms-win-crt-environment-l1-1-0s00005.o}
procedure __searchenv;external;
{$L x86/libapi-ms-win-crt-environment-l1-1-0s00006.o}
procedure __searchenv_s;external;
{$L x86/libapi-ms-win-crt-environment-l1-1-0s00007.o}
procedure __wdupenv_s;external;
{$L x86/libapi-ms-win-crt-environment-l1-1-0s00008.o}
procedure _searchenv;external;
{$L x86/libapi-ms-win-crt-environment-l1-1-0s00009.o}
procedure __wgetcwd;external;
{$L x86/libapi-ms-win-crt-environment-l1-1-0s00010.o}
procedure __wgetdcwd;external;
{$L x86/libapi-ms-win-crt-environment-l1-1-0s00011.o}
procedure __wgetenv;external;
{$L x86/libapi-ms-win-crt-environment-l1-1-0s00012.o}
procedure __wgetenv_s;external;
{$L x86/libapi-ms-win-crt-environment-l1-1-0s00013.o}
procedure __wputenv;external;
{$L x86/libapi-ms-win-crt-environment-l1-1-0s00014.o}
procedure __wputenv_s;external;
{$L x86/libapi-ms-win-crt-environment-l1-1-0s00015.o}
procedure __wsearchenv;external;
{$L x86/libapi-ms-win-crt-environment-l1-1-0s00016.o}
procedure __wsearchenv_s;external;
{$L x86/libapi-ms-win-crt-environment-l1-1-0s00017.o}
procedure _getenv;external;
{$L x86/libapi-ms-win-crt-environment-l1-1-0s00018.o}
procedure _getenv_s;external;
{$L x86/libapi-ms-win-crt-environment-l1-1-0s00019.o}
procedure ___toascii;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00000.o}
procedure __atodbl;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00001.o}
procedure __atodbl_l;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00002.o}
procedure __atof_l;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00003.o}
procedure __atoflt;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00004.o}
procedure __atoflt_l;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00005.o}
procedure _toascii;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00006.o}
procedure __atoi64;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00007.o}
procedure __atoi64_l;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00008.o}
procedure __atoi_l;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00009.o}
procedure __atol_l;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00010.o}
procedure __atoldbl;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00011.o}
procedure __atoldbl_l;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00012.o}
procedure __atoll_l;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00013.o}
procedure __ecvt;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00014.o}
procedure _ecvt;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00015.o}
procedure __ecvt_s;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00016.o}
procedure __fcvt;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00017.o}
procedure __fcvt_s;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00018.o}
procedure _gcvt;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00019.o}
procedure __gcvt;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00020.o}
procedure _fcvt;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00021.o}
procedure __gcvt_s;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00022.o}
procedure __i64toa;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00023.o}
procedure __i64toa_s;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00024.o}
procedure __i64tow;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00025.o}
procedure __i64tow_s;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00026.o}
procedure __itoa;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00027.o}
procedure _itoa;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00028.o}
procedure __itoa_s;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00029.o}
procedure __itow;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00030.o}
procedure __itow_s;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00031.o}
procedure __ltoa;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00032.o}
procedure __ltoa_s;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00033.o}
procedure __ltow;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00034.o}
procedure __ltow_s;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00035.o}
procedure __strtod_l;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00036.o}
procedure __strtof_l;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00037.o}
procedure _ltoa;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00038.o}
procedure __strtoi64;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00039.o}
procedure __strtoi64_l;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00040.o}
procedure __strtoimax_l;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00041.o}
procedure __strtol_l;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00042.o}
procedure __strtold_l;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00043.o}
procedure __strtoll_l;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00044.o}
procedure __strtoui64;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00045.o}
procedure __strtoui64_l;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00046.o}
procedure __strtoul_l;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00047.o}
procedure __strtoull_l;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00048.o}
procedure __strtoumax_l;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00049.o}
procedure __ui64toa;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00050.o}
procedure __ui64toa_s;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00051.o}
procedure __ui64tow;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00052.o}
procedure __ui64tow_s;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00053.o}
procedure __ultoa;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00054.o}
procedure __ultoa_s;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00055.o}
procedure __ultow;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00056.o}
procedure __ultow_s;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00057.o}
procedure __wcstod_l;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00058.o}
procedure __wcstof_l;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00059.o}
procedure __wcstoi64;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00060.o}
procedure __wcstoi64_l;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00061.o}
procedure __wcstoimax_l;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00062.o}
procedure __wcstol_l;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00063.o}
procedure __wcstold_l;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00064.o}
procedure __wcstoll_l;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00065.o}
procedure __wcstombs_l;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00066.o}
procedure __wcstombs_s_l;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00067.o}
procedure __wcstoui64;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00068.o}
procedure __wcstoui64_l;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00069.o}
procedure __wcstoul_l;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00070.o}
procedure __wcstoull_l;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00071.o}
procedure __wcstoumax_l;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00072.o}
procedure __wctomb_l;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00073.o}
procedure __wctomb_s_l;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00074.o}
procedure __wtof;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00075.o}
procedure __wtof_l;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00076.o}
procedure __wtoi;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00077.o}
procedure __wtoi64;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00078.o}
procedure __wtoi64_l;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00079.o}
procedure __wtoi_l;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00080.o}
procedure __wtol;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00081.o}
procedure __wtol_l;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00082.o}
procedure __wtoll;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00083.o}
procedure __wtoll_l;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00084.o}
procedure _atof;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00085.o}
procedure _atoi;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00086.o}
procedure _atol;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00087.o}
procedure _atoll;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00088.o}
procedure _btowc;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00089.o}
procedure _c16rtomb;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00090.o}
procedure _c32rtomb;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00091.o}
procedure _mbrtoc16;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00092.o}
procedure _mbrtoc32;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00093.o}
procedure _mbrtowc;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00094.o}
procedure _mbsrtowcs;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00095.o}
procedure _mbsrtowcs_s;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00096.o}
procedure _mbstowcs;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00097.o}
procedure _mbstowcs_s;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00098.o}
procedure _mbtowc;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00099.o}
procedure _strtod;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00100.o}
procedure _strtof;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00101.o}
procedure _strtoimax;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00102.o}
procedure _strtol;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00103.o}
procedure _strtoll;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00104.o}
procedure _strtoul;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00105.o}
procedure _strtoull;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00106.o}
procedure _strtoumax;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00107.o}
procedure _wcrtomb;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00108.o}
procedure _wcrtomb_s;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00109.o}
procedure _wcsrtombs;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00110.o}
procedure _wcsrtombs_s;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00111.o}
procedure _wcstod;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00112.o}
procedure _wcstof;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00113.o}
procedure _wcstoimax;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00114.o}
procedure _wcstol;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00115.o}
procedure _wcstoll;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00116.o}
procedure _wcstombs;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00117.o}
procedure _wcstombs_s;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00118.o}
procedure _wcstoul;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00119.o}
procedure _wcstoull;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00120.o}
procedure _wcstoumax;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00121.o}
procedure _wctob;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00122.o}
procedure _wctomb;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00123.o}
procedure _wctomb_s;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00124.o}
procedure _wctrans;external;
{$L x86/libapi-ms-win-crt-convert-l1-1-0s00125.o}
procedure ___conio_common_vcprintf;external;
{$L x86/libapi-ms-win-crt-conio-l1-1-0s00000.o}
procedure ___conio_common_vcprintf_p;external;
{$L x86/libapi-ms-win-crt-conio-l1-1-0s00001.o}
procedure ___conio_common_vcprintf_s;external;
{$L x86/libapi-ms-win-crt-conio-l1-1-0s00002.o}
procedure ___conio_common_vcscanf;external;
{$L x86/libapi-ms-win-crt-conio-l1-1-0s00003.o}
procedure ___conio_common_vcwprintf;external;
{$L x86/libapi-ms-win-crt-conio-l1-1-0s00004.o}
procedure ___conio_common_vcwprintf_p;external;
{$L x86/libapi-ms-win-crt-conio-l1-1-0s00005.o}
procedure ___conio_common_vcwprintf_s;external;
{$L x86/libapi-ms-win-crt-conio-l1-1-0s00006.o}
procedure ___conio_common_vcwscanf;external;
{$L x86/libapi-ms-win-crt-conio-l1-1-0s00007.o}
procedure __cgets;external;
{$L x86/libapi-ms-win-crt-conio-l1-1-0s00008.o}
procedure __cgets_s;external;
{$L x86/libapi-ms-win-crt-conio-l1-1-0s00009.o}
procedure __cgetws;external;
{$L x86/libapi-ms-win-crt-conio-l1-1-0s00010.o}
procedure __cgetws_s;external;
{$L x86/libapi-ms-win-crt-conio-l1-1-0s00011.o}
procedure __cputs;external;
{$L x86/libapi-ms-win-crt-conio-l1-1-0s00012.o}
procedure __cputws;external;
{$L x86/libapi-ms-win-crt-conio-l1-1-0s00013.o}
procedure __getch;external;
{$L x86/libapi-ms-win-crt-conio-l1-1-0s00014.o}
procedure _getch;external;
{$L x86/libapi-ms-win-crt-conio-l1-1-0s00015.o}
procedure __getch_nolock;external;
{$L x86/libapi-ms-win-crt-conio-l1-1-0s00016.o}
procedure __getche;external;
{$L x86/libapi-ms-win-crt-conio-l1-1-0s00017.o}
procedure __getche_nolock;external;
{$L x86/libapi-ms-win-crt-conio-l1-1-0s00018.o}
procedure __getwch;external;
{$L x86/libapi-ms-win-crt-conio-l1-1-0s00019.o}
procedure __getwch_nolock;external;
{$L x86/libapi-ms-win-crt-conio-l1-1-0s00020.o}
procedure __getwche;external;
{$L x86/libapi-ms-win-crt-conio-l1-1-0s00021.o}
procedure __getwche_nolock;external;
{$L x86/libapi-ms-win-crt-conio-l1-1-0s00022.o}
procedure _getche;external;
{$L x86/libapi-ms-win-crt-conio-l1-1-0s00023.o}
procedure __putch;external;
{$L x86/libapi-ms-win-crt-conio-l1-1-0s00024.o}
procedure _putch;external;
{$L x86/libapi-ms-win-crt-conio-l1-1-0s00025.o}
procedure __putch_nolock;external;
{$L x86/libapi-ms-win-crt-conio-l1-1-0s00026.o}
procedure __putwch;external;
{$L x86/libapi-ms-win-crt-conio-l1-1-0s00027.o}
procedure __putwch_nolock;external;
{$L x86/libapi-ms-win-crt-conio-l1-1-0s00028.o}
procedure __ungetch;external;
{$L x86/libapi-ms-win-crt-conio-l1-1-0s00029.o}
procedure __ungetch_nolock;external;
{$L x86/libapi-ms-win-crt-conio-l1-1-0s00030.o}
procedure _ungetch;external;
{$L x86/libapi-ms-win-crt-conio-l1-1-0s00031.o}
procedure __ungetwch;external;
{$L x86/libapi-ms-win-crt-conio-l1-1-0s00032.o}
procedure __ungetwch_nolock;external;
{$L x86/libapi-ms-win-crt-conio-l1-1-0s00033.o}
procedure __chkstk;external;
{$L x86/chkstk.obj}
function  _malloc(size: NativeInt): Pointer; cdecl;
function _realloc(P: Pointer; NewSize: NativeInt): Pointer; cdecl;
procedure _free(pBlock: Pointer); cdecl;
function _calloc(nitems,size : NativeInt):Pointer;cdecl;
procedure __alloca_probe_16;external;
procedure __alloca_probe_8;external;
{$L x86/alloca16.obj}
procedure __alldiv; external;
{$L x86/lldiv.obj}
procedure __alldvrm; external;
{$L x86/lldvrm.obj}
procedure __allmul; external;
{$L x86/llmul.obj}
procedure __allrem; external;
{$L x86/llrem.obj}
procedure __allshl; external;
{$L x86/llshl.obj}
procedure __allshr; external;
{$L x86/llshr.obj}
procedure __aulldiv; external;
{$L x86/ulldiv.obj}
procedure __aulldvrm; external;
{$L x86/ulldvrm.obj}
procedure __aullrem; external;
{$L x86/ullrem.obj}
procedure __aullshr; external;
{$L x86/ullshr.obj}

var
  __imp___abs64 : UInt64;
  __imp___byteswap_uint64 : UInt64;
  __imp___byteswap_ulong : UInt64;
  __imp___byteswap_ushort : UInt64;
  __imp__lfind : UInt64;
  __imp___lfind : UInt64;
  __imp___lfind_s : UInt64;
  __imp___lrotl : UInt64;
  __imp___lrotr : UInt64;
  __imp__lsearch : UInt64;
  __imp___lsearch : UInt64;
  __imp___lsearch_s : UInt64;
  __imp___rotl : UInt64;
  __imp___rotl64 : UInt64;
  __imp___rotr : UInt64;
  __imp___rotr64 : UInt64;
  __imp___swab : UInt64;
  __imp__swab : UInt64;
  __imp__abs : UInt64;
  __imp__bsearch : UInt64;
  __imp__bsearch_s : UInt64;
  __imp__div : UInt64;
  __imp__imaxabs : UInt64;
  __imp__imaxdiv : UInt64;
  __imp__labs : UInt64;
  __imp__ldiv : UInt64;
  __imp__llabs : UInt64;
  __imp__lldiv : UInt64;
  __imp__qsort : UInt64;
  __imp__qsort_s : UInt64;
  __imp__rand : UInt64;
  __imp__rand_s : UInt64;
  __imp__srand : UInt64;
  __head_lib32_libapi_ms_win_crt_utility_l1_1_0_a : UInt64;
  __lib32_libapi_ms_win_crt_utility_l1_1_0_a_iname : UInt64;
  __imp___Getdays : UInt64;
  __imp___Getmonths : UInt64;
  __imp___Gettnames : UInt64;
  __imp___Strftime : UInt64;
  __imp___W_Getdays : UInt64;
  __imp___W_Getmonths : UInt64;
  __imp___W_Gettnames : UInt64;
  __imp___Wcsftime : UInt64;
  __imp____daylight : UInt64;
  __imp____dstbias : UInt64;
  __imp____timezone : UInt64;
  __imp____tzname : UInt64;
  __imp___ctime32 : UInt64;
  __imp___ctime32_s : UInt64;
  __imp__ctime : UInt64;
  __imp___ctime64 : UInt64;
  __imp___ctime64_s : UInt64;
  __imp___difftime32 : UInt64;
  __imp___difftime64 : UInt64;
  __imp___ftime32 : UInt64;
  __imp___ftime32_s : UInt64;
  __imp___ftime : UInt64;
  __imp___ftime64 : UInt64;
  __imp___ftime64_s : UInt64;
  __imp___futime32 : UInt64;
  __imp___futime : UInt64;
  __imp___futime64 : UInt64;
  __imp___get_daylight : UInt64;
  __imp___get_dstbias : UInt64;
  __imp___get_timezone : UInt64;
  __imp___get_tzname : UInt64;
  __imp___getsystime : UInt64;
  __imp___gmtime32 : UInt64;
  __imp___gmtime32_s : UInt64;
  __imp__gmtime : UInt64;
  __imp___gmtime64 : UInt64;
  __imp___gmtime64_s : UInt64;
  __imp___localtime32 : UInt64;
  __imp___localtime32_s : UInt64;
  __imp___localtime64 : UInt64;
  __imp__localtime : UInt64;
  __imp___localtime64_s : UInt64;
  __imp___mkgmtime32 : UInt64;
  __imp___mkgmtime64 : UInt64;
  __imp___mktime32 : UInt64;
  __imp__mktime : UInt64;
  __imp___mktime64 : UInt64;
  __imp___setsystime : UInt64;
  __imp___strdate : UInt64;
  __imp___strdate_s : UInt64;
  __imp___strftime_l : UInt64;
  __imp___strtime : UInt64;
  __imp___strtime_s : UInt64;
  __imp___time32 : UInt64;
  __imp__time : UInt64;
  __imp___time64 : UInt64;
  __imp___timespec32_get : UInt64;
  __imp___timespec64_get : UInt64;
  __imp___tzset : UInt64;
  __imp___utime32 : UInt64;
  __imp___utime : UInt64;
  __imp___utime64 : UInt64;
  __imp___wasctime : UInt64;
  __imp__utime : UInt64;
  __imp___wasctime_s : UInt64;
  __imp___wcsftime_l : UInt64;
  __imp___wctime32 : UInt64;
  __imp___wctime32_s : UInt64;
  __imp___wctime64 : UInt64;
  __imp___wctime64_s : UInt64;
  __imp___wutime : UInt64;
  __imp___wstrdate : UInt64;
  __imp___wstrdate_s : UInt64;
  __imp___wstrtime : UInt64;
  __imp___wstrtime_s : UInt64;
  __imp___wutime32 : UInt64;
  __imp___wutime64 : UInt64;
  __imp__asctime : UInt64;
  __imp__asctime_s : UInt64;
  __imp__clock : UInt64;
  __imp__strftime : UInt64;
  __imp__timespec_get : UInt64;
  __imp__wcsftime : UInt64;
  __head_lib32_libapi_ms_win_crt_time_l1_1_0_a : UInt64;
  __lib32_libapi_ms_win_crt_time_l1_1_0_a_iname : UInt64;
  __imp___iswalpha_l : UInt64;
  __imp___strcmpi : UInt64;
  __imp____isascii : UInt64;
  __imp____iscsym : UInt64;
  __imp__iscsymf : UInt64;
  __imp____iscsymf : UInt64;
  __imp____iswcsym : UInt64;
  __imp__iscsym : UInt64;
  __imp____iswcsymf : UInt64;
  __imp____strncnt : UInt64;
  __imp____wcsncnt : UInt64;
  __imp___isalnum_l : UInt64;
  __imp___isalpha_l : UInt64;
  __imp___isblank_l : UInt64;
  __imp___iscntrl_l : UInt64;
  __imp___isctype : UInt64;
  __imp___isctype_l : UInt64;
  __imp___isdigit_l : UInt64;
  __imp___isgraph_l : UInt64;
  __imp___isleadbyte_l : UInt64;
  __imp___islower_l : UInt64;
  __imp___isprint_l : UInt64;
  __imp__isascii : UInt64;
  __imp___ispunct_l : UInt64;
  __imp___isspace_l : UInt64;
  __imp___isupper_l : UInt64;
  __imp___iswalnum_l : UInt64;
  __imp___iswblank_l : UInt64;
  __imp___iswcntrl_l : UInt64;
  __imp___iswcsym_l : UInt64;
  __imp___iswcsymf_l : UInt64;
  __imp___iswctype_l : UInt64;
  __imp___iswdigit_l : UInt64;
  __imp___iswgraph_l : UInt64;
  __imp___iswlower_l : UInt64;
  __imp___iswprint_l : UInt64;
  __imp___iswpunct_l : UInt64;
  __imp___iswspace_l : UInt64;
  __imp___iswupper_l : UInt64;
  __imp___iswxdigit_l : UInt64;
  __imp___isxdigit_l : UInt64;
  __imp___memccpy : UInt64;
  __imp__memicmp : UInt64;
  __imp___memicmp : UInt64;
  __imp___memicmp_l : UInt64;
  __imp___strcoll_l : UInt64;
  __imp__memccpy : UInt64;
  __imp___strdup : UInt64;
  __imp__strcmpi : UInt64;
  __imp__stricmp : UInt64;
  __imp__strcasecmp : UInt64;
  __imp__strdup : UInt64;
  __imp___stricmp : UInt64;
  __imp___stricmp_l : UInt64;
  __imp__stricoll : UInt64;
  __imp___stricoll : UInt64;
  __imp___stricoll_l : UInt64;
  __imp___strlwr : UInt64;
  __imp__strlwr : UInt64;
  __imp___strlwr_l : UInt64;
  __imp___strlwr_s : UInt64;
  __imp___strlwr_s_l : UInt64;
  __imp___strncoll : UInt64;
  __imp___strncoll_l : UInt64;
  __imp___strnicmp : UInt64;
  __imp__strnicmp : UInt64;
  __imp__strncasecmp : UInt64;
  __imp___strnicmp_l : UInt64;
  __imp___strnicoll : UInt64;
  __imp___strnicoll_l : UInt64;
  __imp__strnset : UInt64;
  __imp___strnset : UInt64;
  __imp___strnset_s : UInt64;
  __imp__strrev : UInt64;
  __imp___strrev : UInt64;
  __imp___strset : UInt64;
  __imp__strset : UInt64;
  __imp___strset_s : UInt64;
  __imp__strupr : UInt64;
  __imp___strupr : UInt64;
  __imp___strupr_l : UInt64;
  __imp___strupr_s : UInt64;
  __imp___strupr_s_l : UInt64;
  __imp___strxfrm_l : UInt64;
  __imp___tolower : UInt64;
  __imp___tolower_l : UInt64;
  __imp___toupper : UInt64;
  __imp___toupper_l : UInt64;
  __imp___towlower_l : UInt64;
  __imp___towupper_l : UInt64;
  __imp___wcscoll_l : UInt64;
  __imp__wcsdup : UInt64;
  __imp___wcsdup : UInt64;
  __imp__wcsicmp : UInt64;
  __imp__wcscmpi : UInt64;
  __imp___wcsicmp : UInt64;
  __imp___wcsicmp_l : UInt64;
  __imp___wcsicoll : UInt64;
  __imp__wcsicoll : UInt64;
  __imp___wcsicoll_l : UInt64;
  __imp___wcslwr : UInt64;
  __imp__wcslwr : UInt64;
  __imp___wcslwr_l : UInt64;
  __imp___wcslwr_s : UInt64;
  __imp___wcslwr_s_l : UInt64;
  __imp___wcsncoll : UInt64;
  __imp___wcsncoll_l : UInt64;
  __imp___wcsnicmp : UInt64;
  __imp__wcsnicmp : UInt64;
  __imp___wcsnicmp_l : UInt64;
  __imp___wcsnicoll : UInt64;
  __imp___wcsnicoll_l : UInt64;
  __imp___wcsnset : UInt64;
  __imp___wcsnset_s : UInt64;
  __imp__wcsnset : UInt64;
  __imp___wcsrev : UInt64;
  __imp__wcsrev : UInt64;
  __imp___wcsset : UInt64;
  __imp___wcsset_s : UInt64;
  __imp__wcsupr : UInt64;
  __imp___wcsupr : UInt64;
  __imp__wcsset : UInt64;
  __imp___wcsupr_l : UInt64;
  __imp___wcsupr_s : UInt64;
  __imp___wcsupr_s_l : UInt64;
  __imp___wcsxfrm_l : UInt64;
  __imp___wctype : UInt64;
  __imp__is_wctype : UInt64;
  __imp__isalnum : UInt64;
  __imp__isalpha : UInt64;
  __imp__isblank : UInt64;
  __imp__iscntrl : UInt64;
  __imp__isdigit : UInt64;
  __imp__isgraph : UInt64;
  __imp__isleadbyte : UInt64;
  __imp__islower : UInt64;
  __imp__isprint : UInt64;
  __imp__ispunct : UInt64;
  __imp__isspace : UInt64;
  __imp__isupper : UInt64;
  __imp__iswalnum : UInt64;
  __imp__iswalpha : UInt64;
  __imp__iswascii : UInt64;
  __imp__iswblank : UInt64;
  __imp__iswcntrl : UInt64;
  __imp__iswctype : UInt64;
  __imp__iswdigit : UInt64;
  __imp__iswgraph : UInt64;
  __imp__iswlower : UInt64;
  __imp__iswprint : UInt64;
  __imp__iswpunct : UInt64;
  __imp__iswspace : UInt64;
  __imp__iswupper : UInt64;
  __imp__iswxdigit : UInt64;
  __imp__isxdigit : UInt64;
  __imp__mblen : UInt64;
  __imp__mbrlen : UInt64;
  __imp__memcpy_s : UInt64;
  __imp__memmove_s : UInt64;
  __imp__memset : UInt64;
  __imp__strcat : UInt64;
  __imp__strcat_s : UInt64;
  __imp__strcmp : UInt64;
  __imp__strcoll : UInt64;
  __imp__strcpy : UInt64;
  __imp__strcpy_s : UInt64;
  __imp__strcspn : UInt64;
  __imp__strlen : UInt64;
  __imp__strncat : UInt64;
  __imp__strncat_s : UInt64;
  __imp__strncmp : UInt64;
  __imp__strncpy : UInt64;
  __imp__strncpy_s : UInt64;
  __imp__strpbrk : UInt64;
  __imp__strspn : UInt64;
  __imp__strtok : UInt64;
  __imp__strtok_s : UInt64;
  __imp__strxfrm : UInt64;
  __imp__tolower : UInt64;
  __imp__toupper : UInt64;
  __imp__towctrans : UInt64;
  __imp__towlower : UInt64;
  __imp__towupper : UInt64;
  __imp__wcscat : UInt64;
  __imp__wcscat_s : UInt64;
  __imp__wcscmp : UInt64;
  __imp__wcscoll : UInt64;
  __imp__wcscpy : UInt64;
  __imp__wcscpy_s : UInt64;
  __imp__wcscspn : UInt64;
  __imp__wcslen : UInt64;
  __imp__wcsncat : UInt64;
  __imp__wcsncat_s : UInt64;
  __imp__wcsncmp : UInt64;
  __imp__wcsncpy : UInt64;
  __imp__wcsncpy_s : UInt64;
  __imp__wcsnlen : UInt64;
  __imp__wcspbrk : UInt64;
  __imp__wcsspn : UInt64;
  __imp__wcstok : UInt64;
  __imp__wcstok_s : UInt64;
  __imp__wcsxfrm : UInt64;
  __imp__wctype : UInt64;
  __imp__wmemcpy_s : UInt64;
  __imp__wmemmove_s : UInt64;
  __head_lib32_libapi_ms_win_crt_string_l1_1_0_a : UInt64;
  __lib32_libapi_ms_win_crt_string_l1_1_0_a_iname : UInt64;
  __imp____acrt_iob_func : UInt64;
  __imp____p__commode : UInt64;
  __imp____p__fmode : UInt64;
  __imp____stdio_common_vfprintf : UInt64;
  __imp____stdio_common_vfprintf_p : UInt64;
  __imp____stdio_common_vfprintf_s : UInt64;
  __imp____stdio_common_vfscanf : UInt64;
  __imp____stdio_common_vfwprintf : UInt64;
  __imp____stdio_common_vfwprintf_p : UInt64;
  __imp____stdio_common_vfwprintf_s : UInt64;
  __imp____stdio_common_vfwscanf : UInt64;
  __imp____stdio_common_vsnprintf_s : UInt64;
  __imp____stdio_common_vsnwprintf_s : UInt64;
  __imp____stdio_common_vsprintf : UInt64;
  __imp____stdio_common_vsprintf_p : UInt64;
  __imp____stdio_common_vsprintf_s : UInt64;
  __imp____stdio_common_vsscanf : UInt64;
  __imp____stdio_common_vswprintf : UInt64;
  __imp____stdio_common_vswprintf_p : UInt64;
  __imp____stdio_common_vswprintf_s : UInt64;
  __imp____stdio_common_vswscanf : UInt64;
  __imp___chsize : UInt64;
  __imp__chsize : UInt64;
  __imp___chsize_s : UInt64;
  __imp___close : UInt64;
  __imp___commit : UInt64;
  __imp__creat : UInt64;
  __imp___creat : UInt64;
  __imp__close : UInt64;
  __imp___dup : UInt64;
  __imp__dup : UInt64;
  __imp___dup2 : UInt64;
  __imp__eof : UInt64;
  __imp___eof : UInt64;
  __imp__dup2 : UInt64;
  __imp___fclose_nolock : UInt64;
  __imp___fcloseall : UInt64;
  __imp___fflush_nolock : UInt64;
  __imp___fgetc_nolock : UInt64;
  __imp___fgetchar : UInt64;
  __imp__fgetchar : UInt64;
  __imp___fgetwc_nolock : UInt64;
  __imp___fgetwchar : UInt64;
  __imp__filelength : UInt64;
  __imp___filelength : UInt64;
  __imp___filelengthi64 : UInt64;
  __imp__fgetwchar : UInt64;
  __imp___fileno : UInt64;
  __imp__fileno : UInt64;
  __imp___flushall : UInt64;
  __imp___fputc_nolock : UInt64;
  __imp___fputchar : UInt64;
  __imp___fputwc_nolock : UInt64;
  __imp__fputchar : UInt64;
  __imp___fputwchar : UInt64;
  __imp__fputwchar : UInt64;
  __imp___fread_nolock : UInt64;
  __imp___fread_nolock_s : UInt64;
  __imp___fseek_nolock : UInt64;
  __imp___fseeki64 : UInt64;
  __imp___fseeki64_nolock : UInt64;
  __imp___fsopen : UInt64;
  __imp___ftell_nolock : UInt64;
  __imp___ftelli64 : UInt64;
  __imp___ftelli64_nolock : UInt64;
  __imp___fwrite_nolock : UInt64;
  __imp___get_fmode : UInt64;
  __imp___get_osfhandle : UInt64;
  __imp___get_printf_count_output : UInt64;
  __imp___get_stream_buffer_pointers : UInt64;
  __imp___getc_nolock : UInt64;
  __imp___getcwd : UInt64;
  __imp__getcwd : UInt64;
  __imp___getdcwd : UInt64;
  __imp___getmaxstdio : UInt64;
  __imp___getw : UInt64;
  __imp__getw : UInt64;
  __imp___getwc_nolock : UInt64;
  __imp___getws : UInt64;
  __imp___getws_s : UInt64;
  __imp___isatty : UInt64;
  __imp__isatty : UInt64;
  __imp___kbhit : UInt64;
  __imp__kbhit : UInt64;
  __imp___locking : UInt64;
  __imp___lseek : UInt64;
  __imp___lseeki64 : UInt64;
  __imp__mktemp : UInt64;
  __imp___mktemp : UInt64;
  __imp___mktemp_s : UInt64;
  __imp___open : UInt64;
  __imp__lseek : UInt64;
  __imp__open : UInt64;
  __imp__pclose : UInt64;
  __imp___pclose : UInt64;
  __imp___open_osfhandle : UInt64;
  __imp___pipe : UInt64;
  __imp___popen : UInt64;
  __imp___putc_nolock : UInt64;
  __imp__popen : UInt64;
  __imp___putw : UInt64;
  __imp__putw : UInt64;
  __imp___putwc_nolock : UInt64;
  __imp___putws : UInt64;
  __imp___read : UInt64;
  __imp__read : UInt64;
  __imp___rmtmp : UInt64;
  __imp__rmtmp : UInt64;
  __imp___set_fmode : UInt64;
  __imp___set_printf_count_output : UInt64;
  __imp___setmaxstdio : UInt64;
  __imp__setmode : UInt64;
  __imp___setmode : UInt64;
  __imp___sopen : UInt64;
  __imp___sopen_dispatch : UInt64;
  __imp___sopen_s : UInt64;
  __imp__sopen : UInt64;
  __imp___tell : UInt64;
  __imp__tell : UInt64;
  __imp___telli64 : UInt64;
  __imp___tempnam : UInt64;
  __imp___ungetc_nolock : UInt64;
  __imp___ungetwc_nolock : UInt64;
  __imp___wcreat : UInt64;
  __imp__tempnam : UInt64;
  __imp___wfdopen : UInt64;
  __imp___wfopen : UInt64;
  __imp___wfopen_s : UInt64;
  __imp___wfreopen : UInt64;
  __imp___wfreopen_s : UInt64;
  __imp___wfsopen : UInt64;
  __imp___wmktemp : UInt64;
  __imp___wmktemp_s : UInt64;
  __imp___wopen : UInt64;
  __imp___wpopen : UInt64;
  __imp__wpopen : UInt64;
  __imp___write : UInt64;
  __imp__write : UInt64;
  __imp___wsopen : UInt64;
  __imp___wsopen_dispatch : UInt64;
  __imp___wsopen_s : UInt64;
  __imp___wtempnam : UInt64;
  __imp___wtmpnam : UInt64;
  __imp___wtmpnam_s : UInt64;
  __imp__clearerr : UInt64;
  __imp__clearerr_s : UInt64;
  __imp__fclose : UInt64;
  __imp__feof : UInt64;
  __imp__ferror : UInt64;
  __imp__fflush : UInt64;
  __imp__fgetc : UInt64;
  __imp__fgetpos : UInt64;
  __imp__fgets : UInt64;
  __imp__fgetwc : UInt64;
  __imp__fgetws : UInt64;
  __imp__fopen : UInt64;
  __imp__fopen_s : UInt64;
  __imp__fputc : UInt64;
  __imp__fputs : UInt64;
  __imp__fputwc : UInt64;
  __imp__fputws : UInt64;
  __imp__fread : UInt64;
  __imp__fread_s : UInt64;
  __imp__freopen : UInt64;
  __imp__freopen_s : UInt64;
  __imp__fseek : UInt64;
  __imp__fsetpos : UInt64;
  __imp__ftell : UInt64;
  __imp__fwrite : UInt64;
  __imp__getc : UInt64;
  __imp__getchar : UInt64;
  __imp__gets : UInt64;
  __imp__gets_s : UInt64;
  __imp__getwc : UInt64;
  __imp__getwchar : UInt64;
  __imp__putc : UInt64;
  __imp__putchar : UInt64;
  __imp__puts : UInt64;
  __imp__putwc : UInt64;
  __imp__putwchar : UInt64;
  __imp__rewind : UInt64;
  __imp__setbuf : UInt64;
  __imp__setvbuf : UInt64;
  __imp__tmpfile : UInt64;
  __imp__tmpfile_s : UInt64;
  __imp__tmpnam : UInt64;
  __imp__tmpnam_s : UInt64;
  __imp__ungetc : UInt64;
  __imp__ungetwc : UInt64;
  __head_lib32_libapi_ms_win_crt_stdio_l1_1_0_a : UInt64;
  __lib32_libapi_ms_win_crt_stdio_l1_1_0_a_iname : UInt64;
  __imp___Exit : UInt64;
  __imp____control87_2 : UInt64;
  __imp____doserrno : UInt64;
  __imp____fpe_flt_rounds : UInt64;
  __imp____fpecode : UInt64;
  __imp____p___argc : UInt64;
  __imp____p___argv : UInt64;
  __imp____p___wargv : UInt64;
  __imp____p__acmdln : UInt64;
  __imp____p__pgmptr : UInt64;
  __imp____p__wcmdln : UInt64;
  __imp____p__wpgmptr : UInt64;
  __imp____pxcptinfoptrs : UInt64;
  __imp____sys_errlist : UInt64;
  __imp____sys_nerr : UInt64;
  __imp____threadhandle : UInt64;
  __imp____threadid : UInt64;
  __imp____wcserror : UInt64;
  __imp____wcserror_s : UInt64;
  __imp___assert : UInt64;
  __imp___beginthread : UInt64;
  __imp___beginthreadex : UInt64;
  __imp___c_exit : UInt64;
  __imp___cexit : UInt64;
  __imp___clearfp : UInt64;
  __imp___configure_narrow_argv : UInt64;
  __imp___configure_wide_argv : UInt64;
  __imp___control87 : UInt64;
  __imp___controlfp : UInt64;
  __imp___controlfp_s : UInt64;
  __imp___crt_at_quick_exit : UInt64;
  __imp___crt_atexit : UInt64;
  __imp___crt_debugger_hook : UInt64;
  __imp___endthread : UInt64;
  __imp___endthreadex : UInt64;
  __imp___errno : UInt64;
  __imp___execute_onexit_table : UInt64;
  __imp___fpreset : UInt64;
  __imp___get_doserrno : UInt64;
  __imp___get_errno : UInt64;
  __imp___get_initial_narrow_environment : UInt64;
  __imp___get_initial_wide_environment : UInt64;
  __imp___get_invalid_parameter_handler : UInt64;
  __imp___get_narrow_winmain_command_line : UInt64;
  __imp___get_pgmptr : UInt64;
  __imp___get_terminate : UInt64;
  __imp___get_thread_local_invalid_parameter_handler : UInt64;
  __imp___get_wide_winmain_command_line : UInt64;
  __imp___get_wpgmptr : UInt64;
  __imp___getdllprocaddr : UInt64;
  __imp___getpid : UInt64;
  __imp___initialize_narrow_environment : UInt64;
  __imp__getpid : UInt64;
  __imp___initialize_onexit_table : UInt64;
  __imp____set_app_type : UInt64;
  __imp___initialize_wide_environment : UInt64;
  __imp___initterm : UInt64;
  __imp___initterm_e : UInt64;
  __imp___invalid_parameter_noinfo : UInt64;
  __imp___invalid_parameter_noinfo_noreturn : UInt64;
  __imp___invoke_watson : UInt64;
  __imp___query_app_type : UInt64;
  __imp___register_onexit_function : UInt64;
  __imp___register_thread_local_exe_atexit_callback : UInt64;
  __imp___resetstkoflw : UInt64;
  __imp___seh_filter_dll : UInt64;
  __imp___seh_filter_exe : UInt64;
  __imp___set_abort_behavior : UInt64;
  __imp___set_app_type : UInt64;
  __imp___set_controlfp : UInt64;
  __imp___set_doserrno : UInt64;
  __imp___set_errno : UInt64;
  __imp___set_error_mode : UInt64;
  __imp___set_invalid_parameter_handler : UInt64;
  __imp___set_new_handler : UInt64;
  __imp___set_thread_local_invalid_parameter_handler : UInt64;
  __imp___seterrormode : UInt64;
  __imp___sleep : UInt64;
  __imp___statusfp : UInt64;
  __imp___statusfp2 : UInt64;
  __imp___strerror : UInt64;
  __imp___strerror_s : UInt64;
  __imp___wassert : UInt64;
  __imp___wcserror : UInt64;
  __imp___wcserror_s : UInt64;
  __imp___wperror : UInt64;
  __imp___wsystem : UInt64;
  __imp__abort : UInt64;
  __imp__exit : UInt64;
  __imp__feclearexcept : UInt64;
  __imp__fegetenv : UInt64;
  __imp__fegetexceptflag : UInt64;
  __imp__fegetround : UInt64;
  __imp__feholdexcept : UInt64;
  __imp__fesetenv : UInt64;
  __imp__fesetexceptflag : UInt64;
  __imp__fesetround : UInt64;
  __imp__fetestexcept : UInt64;
  __imp__perror : UInt64;
  __imp__quick_exit : UInt64;
  __imp__raise : UInt64;
  __imp__set_terminate : UInt64;
  __imp__signal : UInt64;
  __imp__strerror : UInt64;
  __imp__strerror_s : UInt64;
  __imp__system : UInt64;
  __imp__terminate : UInt64;
  __head_lib32_libapi_ms_win_crt_runtime_l1_1_0_a : UInt64;
  __lib32_libapi_ms_win_crt_runtime_l1_1_0_a_iname : UInt64;
  __imp___beep : UInt64;
  __imp___cwait : UInt64;
  __imp__execl : UInt64;
  __imp___execl : UInt64;
  __imp__cwait : UInt64;
  __imp___execle : UInt64;
  __imp__execle : UInt64;
  __imp___execlp : UInt64;
  __imp__execlpe : UInt64;
  __imp___execlpe : UInt64;
  __imp__execlp : UInt64;
  __imp___execv : UInt64;
  __imp__execv : UInt64;
  __imp___execve : UInt64;
  __imp__execve : UInt64;
  __imp___execvp : UInt64;
  __imp__execvpe : UInt64;
  __imp___execvpe : UInt64;
  __imp___loaddll : UInt64;
  __imp__execvp : UInt64;
  __imp___spawnl : UInt64;
  __imp__spawnl : UInt64;
  __imp___spawnle : UInt64;
  __imp___spawnlp : UInt64;
  __imp__spawnlpe : UInt64;
  __imp__spawnle : UInt64;
  __imp___spawnlpe : UInt64;
  __imp__spawnlp : UInt64;
  __imp___spawnv : UInt64;
  __imp__spawnve : UInt64;
  __imp___spawnve : UInt64;
  __imp__spawnvp : UInt64;
  __imp___spawnvp : UInt64;
  __imp__spawnv : UInt64;
  __imp___spawnvpe : UInt64;
  __imp__spawnvpe : UInt64;
  __imp___unloaddll : UInt64;
  __imp___wexecl : UInt64;
  __imp___wexecle : UInt64;
  __imp___wexeclp : UInt64;
  __imp___wexeclpe : UInt64;
  __imp___wexecv : UInt64;
  __imp___wexecve : UInt64;
  __imp___wexecvp : UInt64;
  __imp___wexecvpe : UInt64;
  __imp___wspawnl : UInt64;
  __imp___wspawnle : UInt64;
  __imp___wspawnlp : UInt64;
  __imp___wspawnlpe : UInt64;
  __imp___wspawnv : UInt64;
  __imp___wspawnve : UInt64;
  __imp___wspawnvp : UInt64;
  __imp___wspawnvpe : UInt64;
  __head_lib32_libapi_ms_win_crt_process_l1_1_0_a : UInt64;
  __lib32_libapi_ms_win_crt_process_l1_1_0_a_iname : UInt64;
  __imp___CreateFrameInfo : UInt64;
  __imp___EH_prolog : UInt64;
  __imp___FindAndUnlinkFrame : UInt64;
  __imp___GetImageBase : UInt64;
  __imp___GetThrowImageBase : UInt64;
  __imp___IsExceptionObjectToBeDestroyed : UInt64;
  __imp___NLG_Dispatch2 : UInt64;
  __imp___NLG_Return : UInt64;
  __imp___NLG_Return2 : UInt64;
  __imp___SetImageBase : UInt64;
  __imp___SetThrowImageBase : UInt64;
  __imp___SetWinRTOutOfMemoryExceptionCallback : UInt64;
  __imp____AdjustPointer : UInt64;
  __imp____BuildCatchObject : UInt64;
  __imp____BuildCatchObjectHelper : UInt64;
  __imp____CxxDetectRethrow : UInt64;
  __imp____CxxExceptionFilter : UInt64;
  __imp____CxxFrameHandler : UInt64;
  __imp____CxxFrameHandler2 : UInt64;
  __imp____CxxFrameHandler3 : UInt64;
  __imp____CxxQueryExceptionSize : UInt64;
  __imp____CxxRegisterExceptionObject : UInt64;
  __imp____CxxUnregisterExceptionObject : UInt64;
  __imp____DestructExceptionObject : UInt64;
  __imp____FrameUnwindFilter : UInt64;
  __imp____GetPlatformExceptionInfo : UInt64;
  __imp____NLG_Dispatch2 : UInt64;
  __imp____NLG_Return2 : UInt64;
  __imp____RTCastToVoid : UInt64;
  __imp____RTDynamicCast : UInt64;
  __imp____RTtypeid : UInt64;
  __imp____TypeMatch : UInt64;
  __imp____current_exception : UInt64;
  __imp____current_exception_context : UInt64;
  __imp____dcrt_get_wide_environment_from_os : UInt64;
  __imp____dcrt_initial_narrow_environment : UInt64;
  __imp____intrinsic_abnormal_termination : UInt64;
  __imp____intrinsic_setjmp : UInt64;
  __imp____processing_throw : UInt64;
  __imp____report_gsfailure : UInt64;
  __imp____std_exception_copy : UInt64;
  __imp____std_exception_destroy : UInt64;
  __imp____std_type_info_compare : UInt64;
  __imp____std_type_info_destroy_list : UInt64;
  __imp____std_type_info_hash : UInt64;
  __imp____std_type_info_name : UInt64;
  __imp____unDName : UInt64;
  __imp____unDNameEx : UInt64;
  __imp____uncaught_exception : UInt64;
  __imp___chkesp : UInt64;
  __imp___except_handler2 : UInt64;
  __imp___except_handler3 : UInt64;
  __imp___except_handler4_common : UInt64;
  __imp___get_purecall_handler : UInt64;
  __imp___get_unexpected : UInt64;
  __imp___global_unwind2 : UInt64;
  __imp___is_exception_typeof : UInt64;
  __imp___local_unwind2 : UInt64;
  __imp___local_unwind4 : UInt64;
  __imp___longjmpex : UInt64;
  __imp___o__CIacos : UInt64;
  __imp___o__CIasin : UInt64;
  __imp___o__CIatan : UInt64;
  __imp___o__CIatan2 : UInt64;
  __imp___o__CIcos : UInt64;
  __imp___o__CIcosh : UInt64;
  __imp___o__CIexp : UInt64;
  __imp___o__CIfmod : UInt64;
  __imp___o__CIlog : UInt64;
  __imp___o__CIlog10 : UInt64;
  __imp___o__CIpow : UInt64;
  __imp___o__CIsin : UInt64;
  __imp___o__CIsinh : UInt64;
  __imp___o__CIsqrt : UInt64;
  __imp___o__CItan : UInt64;
  __imp___o__CItanh : UInt64;
  __imp___o__Getdays : UInt64;
  __imp___o__Getmonths : UInt64;
  __imp___o__Gettnames : UInt64;
  __imp___o__Strftime : UInt64;
  __imp___o__W_Getdays : UInt64;
  __imp___o__W_Getmonths : UInt64;
  __imp___o__W_Gettnames : UInt64;
  __imp___o__Wcsftime : UInt64;
  __imp___o___acrt_iob_func : UInt64;
  __imp___o___conio_common_vcprintf : UInt64;
  __imp___o___conio_common_vcprintf_p : UInt64;
  __imp___o___conio_common_vcprintf_s : UInt64;
  __imp___o___conio_common_vcscanf : UInt64;
  __imp___o___conio_common_vcwprintf : UInt64;
  __imp___o___conio_common_vcwprintf_p : UInt64;
  __imp___o___conio_common_vcwprintf_s : UInt64;
  __imp___o___conio_common_vcwscanf : UInt64;
  __imp___o___daylight : UInt64;
  __imp___o___dstbias : UInt64;
  __imp___o___fpe_flt_rounds : UInt64;
  __imp___o___libm_sse2_acos : UInt64;
  __imp___o___libm_sse2_acosf : UInt64;
  __imp___o___libm_sse2_asin : UInt64;
  __imp___o___libm_sse2_asinf : UInt64;
  __imp___o___libm_sse2_atan : UInt64;
  __imp___o___libm_sse2_atan2 : UInt64;
  __imp___o___libm_sse2_atanf : UInt64;
  __imp___o___libm_sse2_cos : UInt64;
  __imp___o___libm_sse2_cosf : UInt64;
  __imp___o___libm_sse2_exp : UInt64;
  __imp___o___libm_sse2_expf : UInt64;
  __imp___o___libm_sse2_log : UInt64;
  __imp___o___libm_sse2_log10 : UInt64;
  __imp___o___libm_sse2_log10f : UInt64;
  __imp___o___libm_sse2_logf : UInt64;
  __imp___o___libm_sse2_pow : UInt64;
  __imp___o___libm_sse2_powf : UInt64;
  __imp___o___libm_sse2_sin : UInt64;
  __imp___o___libm_sse2_sinf : UInt64;
  __imp___o___libm_sse2_tan : UInt64;
  __imp___o___libm_sse2_tanf : UInt64;
  __imp___o___p___argc : UInt64;
  __imp___o___p___argv : UInt64;
  __imp___o___p___wargv : UInt64;
  __imp___o___p__acmdln : UInt64;
  __imp___o___p__commode : UInt64;
  __imp___o___p__environ : UInt64;
  __imp___o___p__fmode : UInt64;
  __imp___o___p__mbcasemap : UInt64;
  __imp___o___p__mbctype : UInt64;
  __imp___o___p__pgmptr : UInt64;
  __imp___o___p__wcmdln : UInt64;
  __imp___o___p__wenviron : UInt64;
  __imp___o___p__wpgmptr : UInt64;
  __imp___o___pctype_func : UInt64;
  __imp___o___pwctype_func : UInt64;
  __imp___o___stdio_common_vfprintf : UInt64;
  __imp___o___stdio_common_vfprintf_p : UInt64;
  __imp___o___stdio_common_vfprintf_s : UInt64;
  __imp___o___stdio_common_vfscanf : UInt64;
  __imp___o___stdio_common_vfwprintf : UInt64;
  __imp___o___stdio_common_vfwprintf_p : UInt64;
  __imp___o___stdio_common_vfwprintf_s : UInt64;
  __imp___o___stdio_common_vfwscanf : UInt64;
  __imp___o___stdio_common_vsnprintf_s : UInt64;
  __imp___o___stdio_common_vsnwprintf_s : UInt64;
  __imp___o___stdio_common_vsprintf : UInt64;
  __imp___o___stdio_common_vsprintf_p : UInt64;
  __imp___o___stdio_common_vsprintf_s : UInt64;
  __imp___o___stdio_common_vsscanf : UInt64;
  __imp___o___stdio_common_vswprintf : UInt64;
  __imp___o___stdio_common_vswprintf_p : UInt64;
  __imp___o___stdio_common_vswprintf_s : UInt64;
  __imp___o___stdio_common_vswscanf : UInt64;
  __imp___o___timezone : UInt64;
  __imp___o___tzname : UInt64;
  __imp___o___wcserror : UInt64;
  __imp___o__access : UInt64;
  __imp___o__access_s : UInt64;
  __imp___o__aligned_free : UInt64;
  __imp___o__aligned_malloc : UInt64;
  __imp___o__aligned_msize : UInt64;
  __imp___o__aligned_offset_malloc : UInt64;
  __imp___o__aligned_offset_realloc : UInt64;
  __imp___o__aligned_offset_recalloc : UInt64;
  __imp___o__aligned_realloc : UInt64;
  __imp___o__aligned_recalloc : UInt64;
  __imp___o__atodbl : UInt64;
  __imp___o__atodbl_l : UInt64;
  __imp___o__atof_l : UInt64;
  __imp___o__atoflt : UInt64;
  __imp___o__atoflt_l : UInt64;
  __imp___o__atoi64 : UInt64;
  __imp___o__atoi64_l : UInt64;
  __imp___o__atoi_l : UInt64;
  __imp___o__atol_l : UInt64;
  __imp___o__atoldbl : UInt64;
  __imp___o__atoldbl_l : UInt64;
  __imp___o__atoll_l : UInt64;
  __imp___o__beep : UInt64;
  __imp___o__beginthread : UInt64;
  __imp___o__beginthreadex : UInt64;
  __imp___o__cabs : UInt64;
  __imp___o__callnewh : UInt64;
  __imp___o__calloc_base : UInt64;
  __imp___o__cgets : UInt64;
  __imp___o__cgets_s : UInt64;
  __imp___o__cgetws : UInt64;
  __imp___o__cgetws_s : UInt64;
  __imp___o__chdir : UInt64;
  __imp___o__chdrive : UInt64;
  __imp___o__chmod : UInt64;
  __imp___o__chsize : UInt64;
  __imp___o__chsize_s : UInt64;
  __imp___o__close : UInt64;
  __imp___o__commit : UInt64;
  __imp___o__configure_wide_argv : UInt64;
  __imp___o__cputs : UInt64;
  __imp___o__cputws : UInt64;
  __imp___o__creat : UInt64;
  __imp___o__create_locale : UInt64;
  __imp___o__ctime32_s : UInt64;
  __imp___o__ctime64_s : UInt64;
  __imp___o__cwait : UInt64;
  __imp___o__d_int : UInt64;
  __imp___o__dclass : UInt64;
  __imp___o__difftime32 : UInt64;
  __imp___o__difftime64 : UInt64;
  __imp___o__dlog : UInt64;
  __imp___o__dnorm : UInt64;
  __imp___o__dpcomp : UInt64;
  __imp___o__dpoly : UInt64;
  __imp___o__dscale : UInt64;
  __imp___o__dsign : UInt64;
  __imp___o__dsin : UInt64;
  __imp___o__dtest : UInt64;
  __imp___o__dunscale : UInt64;
  __imp___o__dup : UInt64;
  __imp___o__dup2 : UInt64;
  __imp___o__dupenv_s : UInt64;
  __imp___o__ecvt : UInt64;
  __imp___o__ecvt_s : UInt64;
  __imp___o__endthread : UInt64;
  __imp___o__endthreadex : UInt64;
  __imp___o__eof : UInt64;
  __imp___o__errno : UInt64;
  __imp___o__except1 : UInt64;
  __imp___o__execute_onexit_table : UInt64;
  __imp___o__execv : UInt64;
  __imp___o__execve : UInt64;
  __imp___o__execvp : UInt64;
  __imp___o__execvpe : UInt64;
  __imp___o__expand : UInt64;
  __imp___o__fclose_nolock : UInt64;
  __imp___o__fcloseall : UInt64;
  __imp___o__fcvt : UInt64;
  __imp___o__fcvt_s : UInt64;
  __imp___o__fd_int : UInt64;
  __imp___o__fdclass : UInt64;
  __imp___o__fdexp : UInt64;
  __imp___o__fdlog : UInt64;
  __imp___o__fdopen : UInt64;
  __imp___o__fdpcomp : UInt64;
  __imp___o__fdpoly : UInt64;
  __imp___o__fdscale : UInt64;
  __imp___o__fdsign : UInt64;
  __imp___o__fdsin : UInt64;
  __imp___o__fflush_nolock : UInt64;
  __imp___o__fgetc_nolock : UInt64;
  __imp___o__fgetchar : UInt64;
  __imp___o__fgetwc_nolock : UInt64;
  __imp___o__fgetwchar : UInt64;
  __imp___o__filelength : UInt64;
  __imp___o__filelengthi64 : UInt64;
  __imp___o__fileno : UInt64;
  __imp___o__findclose : UInt64;
  __imp___o__findfirst32 : UInt64;
  __imp___o__findfirst32i64 : UInt64;
  __imp___o__findfirst64 : UInt64;
  __imp___o__findfirst64i32 : UInt64;
  __imp___o__findnext32 : UInt64;
  __imp___o__findnext32i64 : UInt64;
  __imp___o__findnext64 : UInt64;
  __imp___o__findnext64i32 : UInt64;
  __imp___o__flushall : UInt64;
  __imp___o__fpclass : UInt64;
  __imp___o__fpclassf : UInt64;
  __imp___o__fputc_nolock : UInt64;
  __imp___o__fputchar : UInt64;
  __imp___o__fputwc_nolock : UInt64;
  __imp___o__fputwchar : UInt64;
  __imp___o__fread_nolock : UInt64;
  __imp___o__fread_nolock_s : UInt64;
  __imp___o__free_base : UInt64;
  __imp___o__free_locale : UInt64;
  __imp___o__fseek_nolock : UInt64;
  __imp___o__fseeki64 : UInt64;
  __imp___o__fseeki64_nolock : UInt64;
  __imp___o__fsopen : UInt64;
  __imp___o__fstat32 : UInt64;
  __imp___o__fstat32i64 : UInt64;
  __imp___o__fstat64 : UInt64;
  __imp___o__fstat64i32 : UInt64;
  __imp___o__ftell_nolock : UInt64;
  __imp___o__ftelli64 : UInt64;
  __imp___o__ftelli64_nolock : UInt64;
  __imp___o__ftime32 : UInt64;
  __imp___o__ftime32_s : UInt64;
  __imp___o__ftime64 : UInt64;
  __imp___o__ftime64_s : UInt64;
  __imp___o__fullpath : UInt64;
  __imp___o__futime32 : UInt64;
  __imp___o__futime64 : UInt64;
  __imp___o__fwrite_nolock : UInt64;
  __imp___o__gcvt : UInt64;
  __imp___o__gcvt_s : UInt64;
  __imp___o__get_daylight : UInt64;
  __imp___o__get_doserrno : UInt64;
  __imp___o__get_dstbias : UInt64;
  __imp___o__get_errno : UInt64;
  __imp___o__get_fmode : UInt64;
  __imp___o__get_heap_handle : UInt64;
  __imp___o__get_invalid_parameter_handler : UInt64;
  __imp___o__get_narrow_winmain_command_line : UInt64;
  __imp___o__get_osfhandle : UInt64;
  __imp___o__get_pgmptr : UInt64;
  __imp___o__get_stream_buffer_pointers : UInt64;
  __imp___o__get_terminate : UInt64;
  __imp___o__get_thread_local_invalid_parameter_handler : UInt64;
  __imp___o__get_timezone : UInt64;
  __imp___o__get_tzname : UInt64;
  __imp___o__get_wide_winmain_command_line : UInt64;
  __imp___o__get_wpgmptr : UInt64;
  __imp___o__getc_nolock : UInt64;
  __imp___o__getch : UInt64;
  __imp___o__getch_nolock : UInt64;
  __imp___o__getche : UInt64;
  __imp___o__getche_nolock : UInt64;
  __imp___o__getcwd : UInt64;
  __imp___o__getdcwd : UInt64;
  __imp___o__getdiskfree : UInt64;
  __imp___o__getdllprocaddr : UInt64;
  __imp___o__getdrive : UInt64;
  __imp___o__getdrives : UInt64;
  __imp___o__getmbcp : UInt64;
  __imp___o__getsystime : UInt64;
  __imp___o__getw : UInt64;
  __imp___o__getwc_nolock : UInt64;
  __imp___o__getwch : UInt64;
  __imp___o__getwch_nolock : UInt64;
  __imp___o__getwche : UInt64;
  __imp___o__getwche_nolock : UInt64;
  __imp___o__getws : UInt64;
  __imp___o__getws_s : UInt64;
  __imp___o__gmtime32 : UInt64;
  __imp___o__gmtime32_s : UInt64;
  __imp___o__gmtime64 : UInt64;
  __imp___o__gmtime64_s : UInt64;
  __imp___o__heapchk : UInt64;
  __imp___o__heapmin : UInt64;
  __imp___o__hypot : UInt64;
  __imp___o__hypotf : UInt64;
  __imp___o__i64toa : UInt64;
  __imp___o__i64toa_s : UInt64;
  __imp___o__i64tow : UInt64;
  __imp___o__i64tow_s : UInt64;
  __imp___o__initialize_onexit_table : UInt64;
  __imp___o__invalid_parameter_noinfo : UInt64;
  __imp___o__invalid_parameter_noinfo_noreturn : UInt64;
  __imp___o__isatty : UInt64;
  __imp___o__isctype : UInt64;
  __imp___o__isctype_l : UInt64;
  __imp___o__isleadbyte_l : UInt64;
  __imp___o__ismbbalnum : UInt64;
  __imp___o__ismbbalnum_l : UInt64;
  __imp___o__ismbbalpha : UInt64;
  __imp___o__ismbbalpha_l : UInt64;
  __imp___o__ismbbblank : UInt64;
  __imp___o__ismbbblank_l : UInt64;
  __imp___o__ismbbgraph : UInt64;
  __imp___o__ismbbgraph_l : UInt64;
  __imp___o__ismbbkalnum : UInt64;
  __imp___o__ismbbkalnum_l : UInt64;
  __imp___o__ismbbkana : UInt64;
  __imp___o__ismbbkana_l : UInt64;
  __imp___o__ismbbkprint : UInt64;
  __imp___o__ismbbkprint_l : UInt64;
  __imp___o__ismbbkpunct : UInt64;
  __imp___o__ismbbkpunct_l : UInt64;
  __imp___o__ismbblead : UInt64;
  __imp___o__ismbblead_l : UInt64;
  __imp___o__ismbbprint : UInt64;
  __imp___o__ismbbprint_l : UInt64;
  __imp___o__ismbbpunct : UInt64;
  __imp___o__ismbbpunct_l : UInt64;
  __imp___o__ismbbtrail : UInt64;
  __imp___o__ismbbtrail_l : UInt64;
  __imp___o__ismbcalnum : UInt64;
  __imp___o__ismbcalnum_l : UInt64;
  __imp___o__ismbcalpha : UInt64;
  __imp___o__ismbcalpha_l : UInt64;
  __imp___o__ismbcblank : UInt64;
  __imp___o__ismbcblank_l : UInt64;
  __imp___o__ismbcdigit : UInt64;
  __imp___o__ismbcdigit_l : UInt64;
  __imp___o__ismbcgraph : UInt64;
  __imp___o__ismbcgraph_l : UInt64;
  __imp___o__ismbchira : UInt64;
  __imp___o__ismbchira_l : UInt64;
  __imp___o__ismbckata : UInt64;
  __imp___o__ismbckata_l : UInt64;
  __imp___o__ismbcl0 : UInt64;
  __imp___o__ismbcl0_l : UInt64;
  __imp___o__ismbcl1 : UInt64;
  __imp___o__ismbcl1_l : UInt64;
  __imp___o__ismbcl2 : UInt64;
  __imp___o__ismbcl2_l : UInt64;
  __imp___o__ismbclegal : UInt64;
  __imp___o__ismbclegal_l : UInt64;
  __imp___o__ismbclower : UInt64;
  __imp___o__ismbclower_l : UInt64;
  __imp___o__ismbcprint : UInt64;
  __imp___o__ismbcprint_l : UInt64;
  __imp___o__ismbcpunct : UInt64;
  __imp___o__ismbcpunct_l : UInt64;
  __imp___o__ismbcspace : UInt64;
  __imp___o__ismbcspace_l : UInt64;
  __imp___o__ismbcsymbol : UInt64;
  __imp___o__ismbcsymbol_l : UInt64;
  __imp___o__ismbcupper : UInt64;
  __imp___o__ismbcupper_l : UInt64;
  __imp___o__ismbslead : UInt64;
  __imp___o__ismbslead_l : UInt64;
  __imp___o__ismbstrail : UInt64;
  __imp___o__ismbstrail_l : UInt64;
  __imp___o__iswctype_l : UInt64;
  __imp___o__itoa : UInt64;
  __imp___o__itoa_s : UInt64;
  __imp___o__itow : UInt64;
  __imp___o__itow_s : UInt64;
  __imp___o__j0 : UInt64;
  __imp___o__j1 : UInt64;
  __imp___o__jn : UInt64;
  __imp___o__kbhit : UInt64;
  __imp___o__ld_int : UInt64;
  __imp___o__ldclass : UInt64;
  __imp___o__ldexp : UInt64;
  __imp___o__ldlog : UInt64;
  __imp___o__ldpcomp : UInt64;
  __imp___o__ldpoly : UInt64;
  __imp___o__ldscale : UInt64;
  __imp___o__ldsign : UInt64;
  __imp___o__ldsin : UInt64;
  __imp___o__ldtest : UInt64;
  __imp___o__ldunscale : UInt64;
  __imp___o__lfind : UInt64;
  __imp___o__lfind_s : UInt64;
  __imp___o__libm_sse2_acos_precise : UInt64;
  __imp___o__libm_sse2_asin_precise : UInt64;
  __imp___o__libm_sse2_atan_precise : UInt64;
  __imp___o__libm_sse2_cos_precise : UInt64;
  __imp___o__libm_sse2_exp_precise : UInt64;
  __imp___o__libm_sse2_log10_precise : UInt64;
  __imp___o__libm_sse2_log_precise : UInt64;
  __imp___o__libm_sse2_pow_precise : UInt64;
  __imp___o__libm_sse2_sin_precise : UInt64;
  __imp___o__libm_sse2_sqrt_precise : UInt64;
  __imp___o__libm_sse2_tan_precise : UInt64;
  __imp___o__loaddll : UInt64;
  __imp___o__localtime32 : UInt64;
  __imp___o__localtime32_s : UInt64;
  __imp___o__localtime64 : UInt64;
  __imp___o__localtime64_s : UInt64;
  __imp___o__lock_file : UInt64;
  __imp___o__locking : UInt64;
  __imp___o__logb : UInt64;
  __imp___o__logbf : UInt64;
  __imp___o__lsearch : UInt64;
  __imp___o__lsearch_s : UInt64;
  __imp___o__lseek : UInt64;
  __imp___o__lseeki64 : UInt64;
  __imp___o__ltoa : UInt64;
  __imp___o__ltoa_s : UInt64;
  __imp___o__ltow : UInt64;
  __imp___o__ltow_s : UInt64;
  __imp___o__makepath : UInt64;
  __imp___o__makepath_s : UInt64;
  __imp___o__malloc_base : UInt64;
  __imp___o__mbbtombc : UInt64;
  __imp___o__mbbtombc_l : UInt64;
  __imp___o__mbbtype : UInt64;
  __imp___o__mbbtype_l : UInt64;
  __imp___o__mbccpy : UInt64;
  __imp___o__mbccpy_l : UInt64;
  __imp___o__mbccpy_s : UInt64;
  __imp___o__mbccpy_s_l : UInt64;
  __imp___o__mbcjistojms : UInt64;
  __imp___o__mbcjistojms_l : UInt64;
  __imp___o__mbcjmstojis : UInt64;
  __imp___o__mbcjmstojis_l : UInt64;
  __imp___o__mbclen : UInt64;
  __imp___o__mbclen_l : UInt64;
  __imp___o__mbctohira : UInt64;
  __imp___o__mbctohira_l : UInt64;
  __imp___o__mbctokata : UInt64;
  __imp___o__mbctokata_l : UInt64;
  __imp___o__mbctolower : UInt64;
  __imp___o__mbctolower_l : UInt64;
  __imp___o__mbctombb : UInt64;
  __imp___o__mbctombb_l : UInt64;
  __imp___o__mbctoupper : UInt64;
  __imp___o__mbctoupper_l : UInt64;
  __imp___o__mblen_l : UInt64;
  __imp___o__mbsbtype : UInt64;
  __imp___o__mbsbtype_l : UInt64;
  __imp___o__mbscat_s : UInt64;
  __imp___o__mbscat_s_l : UInt64;
  __imp___o__mbschr : UInt64;
  __imp___o__mbschr_l : UInt64;
  __imp___o__mbscmp : UInt64;
  __imp___o__mbscmp_l : UInt64;
  __imp___o__mbscoll : UInt64;
  __imp___o__mbscoll_l : UInt64;
  __imp___o__mbscpy_s : UInt64;
  __imp___o__mbscpy_s_l : UInt64;
  __imp___o__mbscspn : UInt64;
  __imp___o__mbscspn_l : UInt64;
  __imp___o__mbsdec : UInt64;
  __imp___o__mbsdec_l : UInt64;
  __imp___o__mbsicmp : UInt64;
  __imp___o__mbsicmp_l : UInt64;
  __imp___o__mbsicoll : UInt64;
  __imp___o__mbsicoll_l : UInt64;
  __imp___o__mbsinc : UInt64;
  __imp___o__mbsinc_l : UInt64;
  __imp___o__mbslen : UInt64;
  __imp___o__mbslen_l : UInt64;
  __imp___o__mbslwr : UInt64;
  __imp___o__mbslwr_l : UInt64;
  __imp___o__mbslwr_s : UInt64;
  __imp___o__mbslwr_s_l : UInt64;
  __imp___o__mbsnbcat : UInt64;
  __imp___o__mbsnbcat_l : UInt64;
  __imp___o__mbsnbcat_s : UInt64;
  __imp___o__mbsnbcat_s_l : UInt64;
  __imp___o__mbsnbcmp : UInt64;
  __imp___o__mbsnbcmp_l : UInt64;
  __imp___o__mbsnbcnt : UInt64;
  __imp___o__mbsnbcnt_l : UInt64;
  __imp___o__mbsnbcoll : UInt64;
  __imp___o__mbsnbcoll_l : UInt64;
  __imp___o__mbsnbcpy : UInt64;
  __imp___o__mbsnbcpy_l : UInt64;
  __imp___o__mbsnbcpy_s : UInt64;
  __imp___o__mbsnbcpy_s_l : UInt64;
  __imp___o__mbsnbicmp : UInt64;
  __imp___o__mbsnbicmp_l : UInt64;
  __imp___o__mbsnbicoll : UInt64;
  __imp___o__mbsnbicoll_l : UInt64;
  __imp___o__mbsnbset : UInt64;
  __imp___o__mbsnbset_l : UInt64;
  __imp___o__mbsnbset_s : UInt64;
  __imp___o__mbsnbset_s_l : UInt64;
  __imp___o__mbsncat : UInt64;
  __imp___o__mbsncat_l : UInt64;
  __imp___o__mbsncat_s : UInt64;
  __imp___o__mbsncat_s_l : UInt64;
  __imp___o__mbsnccnt : UInt64;
  __imp___o__mbsnccnt_l : UInt64;
  __imp___o__mbsncmp : UInt64;
  __imp___o__mbsncmp_l : UInt64;
  __imp___o__mbsncoll : UInt64;
  __imp___o__mbsncoll_l : UInt64;
  __imp___o__mbsncpy : UInt64;
  __imp___o__mbsncpy_l : UInt64;
  __imp___o__mbsncpy_s : UInt64;
  __imp___o__mbsncpy_s_l : UInt64;
  __imp___o__mbsnextc : UInt64;
  __imp___o__mbsnextc_l : UInt64;
  __imp___o__mbsnicmp : UInt64;
  __imp___o__mbsnicmp_l : UInt64;
  __imp___o__mbsnicoll : UInt64;
  __imp___o__mbsnicoll_l : UInt64;
  __imp___o__mbsninc : UInt64;
  __imp___o__mbsninc_l : UInt64;
  __imp___o__mbsnlen : UInt64;
  __imp___o__mbsnlen_l : UInt64;
  __imp___o__mbsnset : UInt64;
  __imp___o__mbsnset_l : UInt64;
  __imp___o__mbsnset_s : UInt64;
  __imp___o__mbsnset_s_l : UInt64;
  __imp___o__mbspbrk : UInt64;
  __imp___o__mbspbrk_l : UInt64;
  __imp___o__mbsrchr : UInt64;
  __imp___o__mbsrchr_l : UInt64;
  __imp___o__mbsrev : UInt64;
  __imp___o__mbsrev_l : UInt64;
  __imp___o__mbsset : UInt64;
  __imp___o__mbsset_l : UInt64;
  __imp___o__mbsset_s : UInt64;
  __imp___o__mbsset_s_l : UInt64;
  __imp___o__mbsspn : UInt64;
  __imp___o__mbsspn_l : UInt64;
  __imp___o__mbsspnp : UInt64;
  __imp___o__mbsspnp_l : UInt64;
  __imp___o__mbsstr : UInt64;
  __imp___o__mbsstr_l : UInt64;
  __imp___o__mbstok : UInt64;
  __imp___o__mbstok_l : UInt64;
  __imp___o__mbstok_s : UInt64;
  __imp___o__mbstok_s_l : UInt64;
  __imp___o__mbstowcs_l : UInt64;
  __imp___o__mbstowcs_s_l : UInt64;
  __imp___o__mbstrlen : UInt64;
  __imp___o__mbstrlen_l : UInt64;
  __imp___o__mbstrnlen : UInt64;
  __imp___o__mbstrnlen_l : UInt64;
  __imp___o__mbsupr : UInt64;
  __imp___o__mbsupr_l : UInt64;
  __imp___o__mbsupr_s : UInt64;
  __imp___o__mbsupr_s_l : UInt64;
  __imp___o__mbtowc_l : UInt64;
  __imp___o__memicmp : UInt64;
  __imp___o__memicmp_l : UInt64;
  __imp___o__mkdir : UInt64;
  __imp___o__mkgmtime32 : UInt64;
  __imp___o__mkgmtime64 : UInt64;
  __imp___o__mktemp : UInt64;
  __imp___o__mktemp_s : UInt64;
  __imp___o__mktime32 : UInt64;
  __imp___o__mktime64 : UInt64;
  __imp___o__msize : UInt64;
  __imp___o__nextafter : UInt64;
  __imp___o__nextafterf : UInt64;
  __imp___o__open_osfhandle : UInt64;
  __imp___o__pclose : UInt64;
  __imp___o__pipe : UInt64;
  __imp___o__popen : UInt64;
  __imp___o__putc_nolock : UInt64;
  __imp___o__putch : UInt64;
  __imp___o__putch_nolock : UInt64;
  __imp___o__putenv : UInt64;
  __imp___o__putenv_s : UInt64;
  __imp___o__putw : UInt64;
  __imp___o__putwc_nolock : UInt64;
  __imp___o__putwch : UInt64;
  __imp___o__putwch_nolock : UInt64;
  __imp___o__putws : UInt64;
  __imp___o__read : UInt64;
  __imp___o__realloc_base : UInt64;
  __imp___o__recalloc : UInt64;
  __imp___o__register_onexit_function : UInt64;
  __imp___o__resetstkoflw : UInt64;
  __imp___o__rmdir : UInt64;
  __imp___o__rmtmp : UInt64;
  __imp___o__scalb : UInt64;
  __imp___o__scalbf : UInt64;
  __imp___o__searchenv : UInt64;
  __imp___o__searchenv_s : UInt64;
  __imp___o__set_abort_behavior : UInt64;
  __imp___o__set_doserrno : UInt64;
  __imp___o__set_errno : UInt64;
  __imp___o__set_invalid_parameter_handler : UInt64;
  __imp___o__set_new_handler : UInt64;
  __imp___o__set_new_mode : UInt64;
  __imp___o__set_thread_local_invalid_parameter_handler : UInt64;
  __imp___o__seterrormode : UInt64;
  __imp___o__setmbcp : UInt64;
  __imp___o__setmode : UInt64;
  __imp___o__setsystime : UInt64;
  __imp___o__sleep : UInt64;
  __imp___o__sopen : UInt64;
  __imp___o__sopen_dispatch : UInt64;
  __imp___o__sopen_s : UInt64;
  __imp___o__spawnv : UInt64;
  __imp___o__spawnve : UInt64;
  __imp___o__spawnvp : UInt64;
  __imp___o__spawnvpe : UInt64;
  __imp___o__splitpath : UInt64;
  __imp___o__splitpath_s : UInt64;
  __imp___o__stat32 : UInt64;
  __imp___o__stat32i64 : UInt64;
  __imp___o__stat64 : UInt64;
  __imp___o__stat64i32 : UInt64;
  __imp___o__strcoll_l : UInt64;
  __imp___o__strdate : UInt64;
  __imp___o__strdate_s : UInt64;
  __imp___o__strdup : UInt64;
  __imp___o__strerror : UInt64;
  __imp___o__strerror_s : UInt64;
  __imp___o__strftime_l : UInt64;
  __imp___o__stricmp : UInt64;
  __imp___o__stricmp_l : UInt64;
  __imp___o__stricoll : UInt64;
  __imp___o__stricoll_l : UInt64;
  __imp___o__strlwr : UInt64;
  __imp___o__strlwr_l : UInt64;
  __imp___o__strlwr_s : UInt64;
  __imp___o__strlwr_s_l : UInt64;
  __imp___o__strncoll : UInt64;
  __imp___o__strncoll_l : UInt64;
  __imp___o__strnicmp : UInt64;
  __imp___o__strnicmp_l : UInt64;
  __imp___o__strnicoll : UInt64;
  __imp___o__strnicoll_l : UInt64;
  __imp___o__strnset_s : UInt64;
  __imp___o__strset_s : UInt64;
  __imp___o__strtime : UInt64;
  __imp___o__strtime_s : UInt64;
  __imp___o__strtod_l : UInt64;
  __imp___o__strtof_l : UInt64;
  __imp___o__strtoi64 : UInt64;
  __imp___o__strtoi64_l : UInt64;
  __imp___o__strtol_l : UInt64;
  __imp___o__strtold_l : UInt64;
  __imp___o__strtoll_l : UInt64;
  __imp___o__strtoui64 : UInt64;
  __imp___o__strtoui64_l : UInt64;
  __imp___o__strtoul_l : UInt64;
  __imp___o__strtoull_l : UInt64;
  __imp___o__strupr : UInt64;
  __imp___o__strupr_l : UInt64;
  __imp___o__strupr_s : UInt64;
  __imp___o__strupr_s_l : UInt64;
  __imp___o__strxfrm_l : UInt64;
  __imp___o__swab : UInt64;
  __imp___o__tell : UInt64;
  __imp___o__telli64 : UInt64;
  __imp___o__timespec32_get : UInt64;
  __imp___o__timespec64_get : UInt64;
  __imp___o__tolower : UInt64;
  __imp___o__tolower_l : UInt64;
  __imp___o__toupper : UInt64;
  __imp___o__toupper_l : UInt64;
  __imp___o__towlower_l : UInt64;
  __imp___o__towupper_l : UInt64;
  __imp___o__tzset : UInt64;
  __imp___o__ui64toa : UInt64;
  __imp___o__ui64toa_s : UInt64;
  __imp___o__ui64tow : UInt64;
  __imp___o__ui64tow_s : UInt64;
  __imp___o__ultoa : UInt64;
  __imp___o__ultoa_s : UInt64;
  __imp___o__ultow : UInt64;
  __imp___o__ultow_s : UInt64;
  __imp___o__umask : UInt64;
  __imp___o__umask_s : UInt64;
  __imp___o__ungetc_nolock : UInt64;
  __imp___o__ungetch : UInt64;
  __imp___o__ungetch_nolock : UInt64;
  __imp___o__ungetwc_nolock : UInt64;
  __imp___o__ungetwch : UInt64;
  __imp___o__ungetwch_nolock : UInt64;
  __imp___o__unlink : UInt64;
  __imp___o__unloaddll : UInt64;
  __imp___o__unlock_file : UInt64;
  __imp___o__utime32 : UInt64;
  __imp___o__utime64 : UInt64;
  __imp___o__waccess : UInt64;
  __imp___o__waccess_s : UInt64;
  __imp___o__wasctime : UInt64;
  __imp___o__wasctime_s : UInt64;
  __imp___o__wchdir : UInt64;
  __imp___o__wchmod : UInt64;
  __imp___o__wcreat : UInt64;
  __imp___o__wcreate_locale : UInt64;
  __imp___o__wcscoll_l : UInt64;
  __imp___o__wcsdup : UInt64;
  __imp___o__wcserror : UInt64;
  __imp___o__wcserror_s : UInt64;
  __imp___o__wcsftime_l : UInt64;
  __imp___o__wcsicmp : UInt64;
  __imp___o__wcsicmp_l : UInt64;
  __imp___o__wcsicoll : UInt64;
  __imp___o__wcsicoll_l : UInt64;
  __imp___o__wcslwr : UInt64;
  __imp___o__wcslwr_l : UInt64;
  __imp___o__wcslwr_s : UInt64;
  __imp___o__wcslwr_s_l : UInt64;
  __imp___o__wcsncoll : UInt64;
  __imp___o__wcsncoll_l : UInt64;
  __imp___o__wcsnicmp : UInt64;
  __imp___o__wcsnicmp_l : UInt64;
  __imp___o__wcsnicoll : UInt64;
  __imp___o__wcsnicoll_l : UInt64;
  __imp___o__wcsnset : UInt64;
  __imp___o__wcsnset_s : UInt64;
  __imp___o__wcsset : UInt64;
  __imp___o__wcsset_s : UInt64;
  __imp___o__wcstod_l : UInt64;
  __imp___o__wcstof_l : UInt64;
  __imp___o__wcstoi64 : UInt64;
  __imp___o__wcstoi64_l : UInt64;
  __imp___o__wcstol_l : UInt64;
  __imp___o__wcstold_l : UInt64;
  __imp___o__wcstoll_l : UInt64;
  __imp___o__wcstombs_l : UInt64;
  __imp___o__wcstombs_s_l : UInt64;
  __imp___o__wcstoui64 : UInt64;
  __imp___o__wcstoui64_l : UInt64;
  __imp___o__wcstoul_l : UInt64;
  __imp___o__wcstoull_l : UInt64;
  __imp___o__wcsupr : UInt64;
  __imp___o__wcsupr_l : UInt64;
  __imp___o__wcsupr_s : UInt64;
  __imp___o__wcsupr_s_l : UInt64;
  __imp___o__wcsxfrm_l : UInt64;
  __imp___o__wctime32 : UInt64;
  __imp___o__wctime32_s : UInt64;
  __imp___o__wctime64 : UInt64;
  __imp___o__wctime64_s : UInt64;
  __imp___o__wctomb_l : UInt64;
  __imp___o__wctomb_s_l : UInt64;
  __imp___o__wdupenv_s : UInt64;
  __imp___o__wexecv : UInt64;
  __imp___o__wexecve : UInt64;
  __imp___o__wexecvp : UInt64;
  __imp___o__wexecvpe : UInt64;
  __imp___o__wfdopen : UInt64;
  __imp___o__wfindfirst32 : UInt64;
  __imp___o__wfindfirst32i64 : UInt64;
  __imp___o__wfindfirst64 : UInt64;
  __imp___o__wfindfirst64i32 : UInt64;
  __imp___o__wfindnext32 : UInt64;
  __imp___o__wfindnext32i64 : UInt64;
  __imp___o__wfindnext64 : UInt64;
  __imp___o__wfindnext64i32 : UInt64;
  __imp___o__wfopen : UInt64;
  __imp___o__wfopen_s : UInt64;
  __imp___o__wfreopen : UInt64;
  __imp___o__wfreopen_s : UInt64;
  __imp___o__wfsopen : UInt64;
  __imp___o__wfullpath : UInt64;
  __imp___o__wgetcwd : UInt64;
  __imp___o__wgetdcwd : UInt64;
  __imp___o__wgetenv : UInt64;
  __imp___o__wgetenv_s : UInt64;
  __imp___o__wmakepath : UInt64;
  __imp___o__wmakepath_s : UInt64;
  __imp___o__wmkdir : UInt64;
  __imp___o__wmktemp : UInt64;
  __imp___o__wmktemp_s : UInt64;
  __imp___o__wperror : UInt64;
  __imp___o__wpopen : UInt64;
  __imp___o__wputenv : UInt64;
  __imp___o__wputenv_s : UInt64;
  __imp___o__wremove : UInt64;
  __imp___o__wrename : UInt64;
  __imp___o__write : UInt64;
  __imp___o__wrmdir : UInt64;
  __imp___o__wsearchenv : UInt64;
  __imp___o__wsearchenv_s : UInt64;
  __imp___o__wsetlocale : UInt64;
  __imp___o__wsopen_dispatch : UInt64;
  __imp___o__wsopen_s : UInt64;
  __imp___o__wspawnv : UInt64;
  __imp___o__wspawnve : UInt64;
  __imp___o__wspawnvp : UInt64;
  __imp___o__wspawnvpe : UInt64;
  __imp___o__wsplitpath : UInt64;
  __imp___o__wsplitpath_s : UInt64;
  __imp___o__wstat32 : UInt64;
  __imp___o__wstat32i64 : UInt64;
  __imp___o__wstat64 : UInt64;
  __imp___o__wstat64i32 : UInt64;
  __imp___o__wstrdate : UInt64;
  __imp___o__wstrdate_s : UInt64;
  __imp___o__wstrtime : UInt64;
  __imp___o__wstrtime_s : UInt64;
  __imp___o__wsystem : UInt64;
  __imp___o__wtmpnam_s : UInt64;
  __imp___o__wtof : UInt64;
  __imp___o__wtof_l : UInt64;
  __imp___o__wtoi : UInt64;
  __imp___o__wtoi64 : UInt64;
  __imp___o__wtoi64_l : UInt64;
  __imp___o__wtoi_l : UInt64;
  __imp___o__wtol : UInt64;
  __imp___o__wtol_l : UInt64;
  __imp___o__wtoll : UInt64;
  __imp___o__wtoll_l : UInt64;
  __imp___o__wunlink : UInt64;
  __imp___o__wutime32 : UInt64;
  __imp___o__wutime64 : UInt64;
  __imp___o__y0 : UInt64;
  __imp___o__y1 : UInt64;
  __imp___o__yn : UInt64;
  __imp___o_abort : UInt64;
  __imp___o_acos : UInt64;
  __imp___o_acosf : UInt64;
  __imp___o_acosh : UInt64;
  __imp___o_acoshf : UInt64;
  __imp___o_acoshl : UInt64;
  __imp___o_asctime : UInt64;
  __imp___o_asctime_s : UInt64;
  __imp___o_asin : UInt64;
  __imp___o_asinf : UInt64;
  __imp___o_asinh : UInt64;
  __imp___o_asinhf : UInt64;
  __imp___o_asinhl : UInt64;
  __imp___o_atan : UInt64;
  __imp___o_atan2 : UInt64;
  __imp___o_atan2f : UInt64;
  __imp___o_atanf : UInt64;
  __imp___o_atanh : UInt64;
  __imp___o_atanhf : UInt64;
  __imp___o_atanhl : UInt64;
  __imp___o_atof : UInt64;
  __imp___o_atoi : UInt64;
  __imp___o_atol : UInt64;
  __imp___o_atoll : UInt64;
  __imp___o_bsearch : UInt64;
  __imp___o_bsearch_s : UInt64;
  __imp___o_btowc : UInt64;
  __imp___o_calloc : UInt64;
  __imp___o_cbrt : UInt64;
  __imp___o_cbrtf : UInt64;
  __imp___o_ceil : UInt64;
  __imp___o_ceilf : UInt64;
  __imp___o_clearerr : UInt64;
  __imp___o_clearerr_s : UInt64;
  __imp___o_cos : UInt64;
  __imp___o_cosf : UInt64;
  __imp___o_cosh : UInt64;
  __imp___o_coshf : UInt64;
  __imp___o_erf : UInt64;
  __imp___o_erfc : UInt64;
  __imp___o_erfcf : UInt64;
  __imp___o_erfcl : UInt64;
  __imp___o_erff : UInt64;
  __imp___o_erfl : UInt64;
  __imp___o_exp : UInt64;
  __imp___o_exp2 : UInt64;
  __imp___o_exp2f : UInt64;
  __imp___o_exp2l : UInt64;
  __imp___o_expf : UInt64;
  __imp___o_fabs : UInt64;
  __imp___o_fclose : UInt64;
  __imp___o_feof : UInt64;
  __imp___o_ferror : UInt64;
  __imp___o_fflush : UInt64;
  __imp___o_fgetc : UInt64;
  __imp___o_fgetpos : UInt64;
  __imp___o_fgets : UInt64;
  __imp___o_fgetwc : UInt64;
  __imp___o_fgetws : UInt64;
  __imp___o_floor : UInt64;
  __imp___o_floorf : UInt64;
  __imp___o_fma : UInt64;
  __imp___o_fmaf : UInt64;
  __imp___o_fmal : UInt64;
  __imp___o_fmod : UInt64;
  __imp___o_fmodf : UInt64;
  __imp___o_fopen : UInt64;
  __imp___o_fopen_s : UInt64;
  __imp___o_fputc : UInt64;
  __imp___o_fputs : UInt64;
  __imp___o_fputwc : UInt64;
  __imp___o_fputws : UInt64;
  __imp___o_fread : UInt64;
  __imp___o_fread_s : UInt64;
  __imp___o_free : UInt64;
  __imp___o_freopen : UInt64;
  __imp___o_freopen_s : UInt64;
  __imp___o_frexp : UInt64;
  __imp___o_fseek : UInt64;
  __imp___o_fsetpos : UInt64;
  __imp___o_ftell : UInt64;
  __imp___o_fwrite : UInt64;
  __imp___o_getc : UInt64;
  __imp___o_getchar : UInt64;
  __imp___o_getenv : UInt64;
  __imp___o_getenv_s : UInt64;
  __imp___o_gets : UInt64;
  __imp___o_gets_s : UInt64;
  __imp___o_getwc : UInt64;
  __imp___o_getwchar : UInt64;
  __imp___o_hypot : UInt64;
  __imp___o_is_wctype : UInt64;
  __imp___o_isalnum : UInt64;
  __imp___o_isalpha : UInt64;
  __imp___o_isblank : UInt64;
  __imp___o_iscntrl : UInt64;
  __imp___o_isdigit : UInt64;
  __imp___o_isgraph : UInt64;
  __imp___o_isleadbyte : UInt64;
  __imp___o_islower : UInt64;
  __imp___o_isprint : UInt64;
  __imp___o_ispunct : UInt64;
  __imp___o_isspace : UInt64;
  __imp___o_isupper : UInt64;
  __imp___o_iswalnum : UInt64;
  __imp___o_iswalpha : UInt64;
  __imp___o_iswascii : UInt64;
  __imp___o_iswblank : UInt64;
  __imp___o_iswcntrl : UInt64;
  __imp___o_iswctype : UInt64;
  __imp___o_iswdigit : UInt64;
  __imp___o_iswgraph : UInt64;
  __imp___o_iswlower : UInt64;
  __imp___o_iswprint : UInt64;
  __imp___o_iswpunct : UInt64;
  __imp___o_iswspace : UInt64;
  __imp___o_iswupper : UInt64;
  __imp___o_iswxdigit : UInt64;
  __imp___o_isxdigit : UInt64;
  __imp___o_ldexp : UInt64;
  __imp___o_lgamma : UInt64;
  __imp___o_lgammaf : UInt64;
  __imp___o_lgammal : UInt64;
  __imp___o_llrint : UInt64;
  __imp___o_llrintf : UInt64;
  __imp___o_llrintl : UInt64;
  __imp___o_llround : UInt64;
  __imp___o_llroundf : UInt64;
  __imp___o_llroundl : UInt64;
  __imp___o_localeconv : UInt64;
  __imp___o_log : UInt64;
  __imp___o_log10 : UInt64;
  __imp___o_log10f : UInt64;
  __imp___o_log1p : UInt64;
  __imp___o_log1pf : UInt64;
  __imp___o_log1pl : UInt64;
  __imp___o_log2 : UInt64;
  __imp___o_log2f : UInt64;
  __imp___o_log2l : UInt64;
  __imp___o_logb : UInt64;
  __imp___o_logbf : UInt64;
  __imp___o_logbl : UInt64;
  __imp___o_logf : UInt64;
  __imp___o_lrint : UInt64;
  __imp___o_lrintf : UInt64;
  __imp___o_lrintl : UInt64;
  __imp___o_lround : UInt64;
  __imp___o_lroundf : UInt64;
  __imp___o_lroundl : UInt64;
  __imp___o_malloc : UInt64;
  __imp___o_mblen : UInt64;
  __imp___o_mbrlen : UInt64;
  __imp___o_mbrtoc16 : UInt64;
  __imp___o_mbrtoc32 : UInt64;
  __imp___o_mbrtowc : UInt64;
  __imp___o_mbsrtowcs : UInt64;
  __imp___o_mbsrtowcs_s : UInt64;
  __imp___o_mbstowcs : UInt64;
  __imp___o_mbstowcs_s : UInt64;
  __imp___o_mbtowc : UInt64;
  __imp___o_memset : UInt64;
  __imp___o_modf : UInt64;
  __imp___o_modff : UInt64;
  __imp___o_nan : UInt64;
  __imp___o_nanf : UInt64;
  __imp___o_nanl : UInt64;
  __imp___o_nearbyint : UInt64;
  __imp___o_nearbyintf : UInt64;
  __imp___o_nearbyintl : UInt64;
  __imp___o_nextafter : UInt64;
  __imp___o_nextafterf : UInt64;
  __imp___o_nextafterl : UInt64;
  __imp___o_nexttoward : UInt64;
  __imp___o_nexttowardf : UInt64;
  __imp___o_nexttowardl : UInt64;
  __imp___o_pow : UInt64;
  __imp___o_powf : UInt64;
  __imp___o_putc : UInt64;
  __imp___o_putchar : UInt64;
  __imp___o_puts : UInt64;
  __imp___o_putwc : UInt64;
  __imp___o_putwchar : UInt64;
  __imp___o_qsort : UInt64;
  __imp___o_qsort_s : UInt64;
  __imp___o_raise : UInt64;
  __imp___o_rand : UInt64;
  __imp___o_rand_s : UInt64;
  __imp___o_realloc : UInt64;
  __imp___o_remainder : UInt64;
  __imp___o_remainderf : UInt64;
  __imp___o_remainderl : UInt64;
  __imp___o_remove : UInt64;
  __imp___o_remquo : UInt64;
  __imp___o_remquof : UInt64;
  __imp___o_remquol : UInt64;
  __imp___o_rewind : UInt64;
  __imp___o_rint : UInt64;
  __imp___o_rintf : UInt64;
  __imp___o_rintl : UInt64;
  __imp___o_round : UInt64;
  __imp___o_roundf : UInt64;
  __imp___o_roundl : UInt64;
  __imp___o_scalbln : UInt64;
  __imp___o_scalblnf : UInt64;
  __imp___o_scalblnl : UInt64;
  __imp___o_scalbn : UInt64;
  __imp___o_scalbnf : UInt64;
  __imp___o_scalbnl : UInt64;
  __imp___o_set_terminate : UInt64;
  __imp___o_setbuf : UInt64;
  __imp___o_setvbuf : UInt64;
  __imp___o_sin : UInt64;
  __imp___o_sinf : UInt64;
  __imp___o_sinh : UInt64;
  __imp___o_sinhf : UInt64;
  __imp___o_sqrt : UInt64;
  __imp___o_sqrtf : UInt64;
  __imp___o_srand : UInt64;
  __imp___o_strcat_s : UInt64;
  __imp___o_strcoll : UInt64;
  __imp___o_strcpy_s : UInt64;
  __imp___o_strerror : UInt64;
  __imp___o_strerror_s : UInt64;
  __imp___o_strftime : UInt64;
  __imp___o_strncat_s : UInt64;
  __imp___o_strncpy_s : UInt64;
  __imp___o_strtod : UInt64;
  __imp___o_strtof : UInt64;
  __imp___o_strtok : UInt64;
  __imp___o_strtok_s : UInt64;
  __imp___o_strtol : UInt64;
  __imp___o_strtold : UInt64;
  __imp___o_strtoll : UInt64;
  __imp___o_strtoul : UInt64;
  __imp___o_strtoull : UInt64;
  __imp___o_system : UInt64;
  __imp___o_tan : UInt64;
  __imp___o_tanf : UInt64;
  __imp___o_tanh : UInt64;
  __imp___o_tanhf : UInt64;
  __imp___o_terminate : UInt64;
  __imp___o_tgamma : UInt64;
  __imp___o_tgammaf : UInt64;
  __imp___o_tgammal : UInt64;
  __imp___o_tmpfile_s : UInt64;
  __imp___o_tmpnam_s : UInt64;
  __imp___o_tolower : UInt64;
  __imp___o_toupper : UInt64;
  __imp___o_towlower : UInt64;
  __imp___o_towupper : UInt64;
  __imp___o_ungetc : UInt64;
  __imp___o_ungetwc : UInt64;
  __imp___o_wcrtomb : UInt64;
  __imp___o_wcrtomb_s : UInt64;
  __imp___o_wcscat_s : UInt64;
  __imp___o_wcscoll : UInt64;
  __imp___o_wcscpy : UInt64;
  __imp___o_wcscpy_s : UInt64;
  __imp___o_wcsftime : UInt64;
  __imp___o_wcsncat_s : UInt64;
  __imp___o_wcsncpy_s : UInt64;
  __imp___o_wcsrtombs : UInt64;
  __imp___o_wcsrtombs_s : UInt64;
  __imp___o_wcstod : UInt64;
  __imp___o_wcstof : UInt64;
  __imp___o_wcstok : UInt64;
  __imp___o_wcstok_s : UInt64;
  __imp___o_wcstol : UInt64;
  __imp___o_wcstold : UInt64;
  __imp___o_wcstoll : UInt64;
  __imp___o_wcstombs : UInt64;
  __imp___o_wcstombs_s : UInt64;
  __imp___o_wcstoul : UInt64;
  __imp___o_wcstoull : UInt64;
  __imp___o_wctob : UInt64;
  __imp___o_wctomb : UInt64;
  __imp___o_wctomb_s : UInt64;
  __imp___o_wmemcpy_s : UInt64;
  __imp___o_wmemmove_s : UInt64;
  __imp___purecall : UInt64;
  __imp___set_purecall_handler : UInt64;
  __imp___set_se_translator : UInt64;
  __imp___setjmp3 : UInt64;
  __imp__longjmp : UInt64;
  __imp__memchr : UInt64;
  __imp__memcmp : UInt64;
  __imp__memcpy : UInt64;
  __imp__memmove : UInt64;
  __imp__set_unexpected : UInt64;
  __imp__strchr : UInt64;
  __imp__strrchr : UInt64;
  __imp__strstr : UInt64;
  __imp__unexpected : UInt64;
  __imp__wcschr : UInt64;
  __imp__wcsrchr : UInt64;
  __imp__wcsstr : UInt64;
  __head_lib32_libapi_ms_win_crt_private_l1_1_0_a : UInt64;
  __lib32_libapi_ms_win_crt_private_l1_1_0_a_iname : UInt64;
  __imp____p__mbcasemap : UInt64;
  __imp____p__mbctype : UInt64;
  __imp___ismbbalnum : UInt64;
  __imp___ismbbalnum_l : UInt64;
  __imp___ismbbalpha : UInt64;
  __imp___ismbbalpha_l : UInt64;
  __imp___ismbbblank : UInt64;
  __imp___ismbbblank_l : UInt64;
  __imp___ismbbgraph : UInt64;
  __imp___ismbbgraph_l : UInt64;
  __imp___ismbbkalnum : UInt64;
  __imp___ismbbkalnum_l : UInt64;
  __imp___ismbbkana : UInt64;
  __imp___ismbbkana_l : UInt64;
  __imp___ismbbkprint : UInt64;
  __imp___ismbbkprint_l : UInt64;
  __imp___ismbbkpunct : UInt64;
  __imp___ismbbkpunct_l : UInt64;
  __imp___ismbblead : UInt64;
  __imp___ismbblead_l : UInt64;
  __imp___ismbbprint : UInt64;
  __imp___ismbbprint_l : UInt64;
  __imp___ismbbpunct : UInt64;
  __imp___ismbbpunct_l : UInt64;
  __imp___ismbbtrail : UInt64;
  __imp___ismbbtrail_l : UInt64;
  __imp___ismbcalnum : UInt64;
  __imp___ismbcalnum_l : UInt64;
  __imp___ismbcalpha : UInt64;
  __imp___ismbcalpha_l : UInt64;
  __imp___ismbcblank : UInt64;
  __imp___ismbcblank_l : UInt64;
  __imp___ismbcdigit : UInt64;
  __imp___ismbcdigit_l : UInt64;
  __imp___ismbcgraph : UInt64;
  __imp___ismbcgraph_l : UInt64;
  __imp___ismbchira : UInt64;
  __imp___ismbchira_l : UInt64;
  __imp___ismbckata : UInt64;
  __imp___ismbckata_l : UInt64;
  __imp___ismbcl0 : UInt64;
  __imp___ismbcl0_l : UInt64;
  __imp___ismbcl1 : UInt64;
  __imp___ismbcl1_l : UInt64;
  __imp___ismbcl2 : UInt64;
  __imp___ismbcl2_l : UInt64;
  __imp___ismbclegal : UInt64;
  __imp___ismbclegal_l : UInt64;
  __imp___ismbclower : UInt64;
  __imp___ismbclower_l : UInt64;
  __imp___ismbcprint : UInt64;
  __imp___ismbcprint_l : UInt64;
  __imp___ismbcpunct : UInt64;
  __imp___ismbcpunct_l : UInt64;
  __imp___ismbcspace : UInt64;
  __imp___ismbcspace_l : UInt64;
  __imp___ismbcsymbol : UInt64;
  __imp___ismbcsymbol_l : UInt64;
  __imp___ismbcupper : UInt64;
  __imp___ismbcupper_l : UInt64;
  __imp___ismbslead : UInt64;
  __imp___ismbslead_l : UInt64;
  __imp___ismbstrail : UInt64;
  __imp___ismbstrail_l : UInt64;
  __imp___mbbtombc : UInt64;
  __imp___mbbtombc_l : UInt64;
  __imp___mbbtype : UInt64;
  __imp___mbbtype_l : UInt64;
  __imp___mbcasemap : UInt64;
  __imp___mbccpy : UInt64;
  __imp___mbccpy_l : UInt64;
  __imp___mbccpy_s : UInt64;
  __imp___mbccpy_s_l : UInt64;
  __imp___mbcjistojms : UInt64;
  __imp___mbcjistojms_l : UInt64;
  __imp___mbcjmstojis : UInt64;
  __imp___mbcjmstojis_l : UInt64;
  __imp___mbclen : UInt64;
  __imp___mbclen_l : UInt64;
  __imp___mbctohira : UInt64;
  __imp___mbctohira_l : UInt64;
  __imp___mbctokata : UInt64;
  __imp___mbctokata_l : UInt64;
  __imp___mbctolower : UInt64;
  __imp___mbctolower_l : UInt64;
  __imp___mbctombb : UInt64;
  __imp___mbctombb_l : UInt64;
  __imp___mbctoupper : UInt64;
  __imp___mbctoupper_l : UInt64;
  __imp___mblen_l : UInt64;
  __imp___mbsbtype : UInt64;
  __imp___mbsbtype_l : UInt64;
  __imp___mbscat_s : UInt64;
  __imp___mbscat_s_l : UInt64;
  __imp___mbschr : UInt64;
  __imp___mbschr_l : UInt64;
  __imp___mbscmp : UInt64;
  __imp___mbscmp_l : UInt64;
  __imp___mbscoll : UInt64;
  __imp___mbscoll_l : UInt64;
  __imp___mbscpy_s : UInt64;
  __imp___mbscpy_s_l : UInt64;
  __imp___mbscspn : UInt64;
  __imp___mbscspn_l : UInt64;
  __imp___mbsdec : UInt64;
  __imp___mbsdec_l : UInt64;
  __imp___mbsdup : UInt64;
  __imp___mbsicmp : UInt64;
  __imp___mbsicmp_l : UInt64;
  __imp___mbsicoll : UInt64;
  __imp___mbsicoll_l : UInt64;
  __imp___mbsinc : UInt64;
  __imp___mbsinc_l : UInt64;
  __imp___mbslen : UInt64;
  __imp___mbslen_l : UInt64;
  __imp___mbslwr : UInt64;
  __imp___mbslwr_l : UInt64;
  __imp___mbslwr_s : UInt64;
  __imp___mbslwr_s_l : UInt64;
  __imp___mbsnbcat : UInt64;
  __imp___mbsnbcat_l : UInt64;
  __imp___mbsnbcat_s : UInt64;
  __imp___mbsnbcat_s_l : UInt64;
  __imp___mbsnbcmp : UInt64;
  __imp___mbsnbcmp_l : UInt64;
  __imp___mbsnbcnt : UInt64;
  __imp___mbsnbcnt_l : UInt64;
  __imp___mbsnbcoll : UInt64;
  __imp___mbsnbcoll_l : UInt64;
  __imp___mbsnbcpy : UInt64;
  __imp___mbsnbcpy_l : UInt64;
  __imp___mbsnbcpy_s : UInt64;
  __imp___mbsnbcpy_s_l : UInt64;
  __imp___mbsnbicmp : UInt64;
  __imp___mbsnbicmp_l : UInt64;
  __imp___mbsnbicoll : UInt64;
  __imp___mbsnbicoll_l : UInt64;
  __imp___mbsnbset : UInt64;
  __imp___mbsnbset_l : UInt64;
  __imp___mbsnbset_s : UInt64;
  __imp___mbsnbset_s_l : UInt64;
  __imp___mbsncat : UInt64;
  __imp___mbsncat_l : UInt64;
  __imp___mbsncat_s : UInt64;
  __imp___mbsncat_s_l : UInt64;
  __imp___mbsnccnt : UInt64;
  __imp___mbsnccnt_l : UInt64;
  __imp___mbsncmp : UInt64;
  __imp___mbsncmp_l : UInt64;
  __imp___mbsncoll : UInt64;
  __imp___mbsncoll_l : UInt64;
  __imp___mbsncpy : UInt64;
  __imp___mbsncpy_l : UInt64;
  __imp___mbsncpy_s : UInt64;
  __imp___mbsncpy_s_l : UInt64;
  __imp___mbsnextc : UInt64;
  __imp___mbsnextc_l : UInt64;
  __imp___mbsnicmp : UInt64;
  __imp___mbsnicmp_l : UInt64;
  __imp___mbsnicoll : UInt64;
  __imp___mbsnicoll_l : UInt64;
  __imp___mbsninc : UInt64;
  __imp___mbsninc_l : UInt64;
  __imp___mbsnlen : UInt64;
  __imp___mbsnlen_l : UInt64;
  __imp___mbsnset : UInt64;
  __imp___mbsnset_l : UInt64;
  __imp___mbsnset_s : UInt64;
  __imp___mbsnset_s_l : UInt64;
  __imp___mbspbrk : UInt64;
  __imp___mbspbrk_l : UInt64;
  __imp___mbsrchr : UInt64;
  __imp___mbsrchr_l : UInt64;
  __imp___mbsrev : UInt64;
  __imp___mbsrev_l : UInt64;
  __imp___mbsset : UInt64;
  __imp___mbsset_l : UInt64;
  __imp___mbsset_s : UInt64;
  __imp___mbsset_s_l : UInt64;
  __imp___mbsspn : UInt64;
  __imp___mbsspn_l : UInt64;
  __imp___mbsspnp : UInt64;
  __imp___mbsspnp_l : UInt64;
  __imp___mbsstr : UInt64;
  __imp___mbsstr_l : UInt64;
  __imp___mbstok : UInt64;
  __imp___mbstok_l : UInt64;
  __imp___mbstok_s : UInt64;
  __imp___mbstok_s_l : UInt64;
  __imp___mbstowcs_l : UInt64;
  __imp___mbstowcs_s_l : UInt64;
  __imp___mbstrlen : UInt64;
  __imp___mbstrlen_l : UInt64;
  __imp___mbstrnlen : UInt64;
  __imp___mbstrnlen_l : UInt64;
  __imp___mbsupr : UInt64;
  __imp___mbsupr_l : UInt64;
  __imp___mbsupr_s : UInt64;
  __imp___mbsupr_s_l : UInt64;
  __imp___mbtowc_l : UInt64;
  __head_lib32_libapi_ms_win_crt_multibyte_l1_1_0_a : UInt64;
  __lib32_libapi_ms_win_crt_multibyte_l1_1_0_a_iname : UInt64;
  __imp___CIacos : UInt64;
  __imp___CIasin : UInt64;
  __imp___CIatan : UInt64;
  __imp___CIatan2 : UInt64;
  __imp___CIcos : UInt64;
  __imp___CIcosh : UInt64;
  __imp___CIexp : UInt64;
  __imp___CIfmod : UInt64;
  __imp___CIlog : UInt64;
  __imp___CIlog10 : UInt64;
  __imp___CIpow : UInt64;
  __imp___CIsin : UInt64;
  __imp___CIsinh : UInt64;
  __imp___CIsqrt : UInt64;
  __imp___CItan : UInt64;
  __imp___CItanh : UInt64;
  __imp___Cbuild : UInt64;
  __imp___Cmulcc : UInt64;
  __imp___Cmulcr : UInt64;
  __imp___FCbuild : UInt64;
  __imp___FCmulcc : UInt64;
  __imp___FCmulcr : UInt64;
  __imp___LCbuild : UInt64;
  __imp___LCmulcc : UInt64;
  __imp___LCmulcr : UInt64;
  __imp____libm_sse2_acos : UInt64;
  __imp____libm_sse2_acosf : UInt64;
  __imp____libm_sse2_asin : UInt64;
  __imp____libm_sse2_asinf : UInt64;
  __imp____libm_sse2_atan : UInt64;
  __imp____libm_sse2_atan2 : UInt64;
  __imp____libm_sse2_atanf : UInt64;
  __imp____libm_sse2_cos : UInt64;
  __imp____libm_sse2_cosf : UInt64;
  __imp____libm_sse2_exp : UInt64;
  __imp____libm_sse2_expf : UInt64;
  __imp____libm_sse2_log : UInt64;
  __imp____libm_sse2_log10 : UInt64;
  __imp____libm_sse2_log10f : UInt64;
  __imp____libm_sse2_logf : UInt64;
  __imp____libm_sse2_pow : UInt64;
  __imp____libm_sse2_powf : UInt64;
  __imp____libm_sse2_sin : UInt64;
  __imp____libm_sse2_sinf : UInt64;
  __imp____libm_sse2_tan : UInt64;
  __imp____libm_sse2_tanf : UInt64;
  __imp____setusermatherr : UInt64;
  __imp___cabs : UInt64;
  __imp___chgsign : UInt64;
  __imp__chgsign : UInt64;
  __imp___chgsignf : UInt64;
  __imp___copysign : UInt64;
  __imp___copysignf : UInt64;
  __imp___d_int : UInt64;
  __imp___dclass : UInt64;
  __imp___dexp : UInt64;
  __imp___dlog : UInt64;
  __imp___dnorm : UInt64;
  __imp___dpcomp : UInt64;
  __imp___dpoly : UInt64;
  __imp___dscale : UInt64;
  __imp___dsign : UInt64;
  __imp___dsin : UInt64;
  __imp___dtest : UInt64;
  __imp___dunscale : UInt64;
  __imp___except1 : UInt64;
  __imp___fd_int : UInt64;
  __imp___fdclass : UInt64;
  __imp___fdexp : UInt64;
  __imp___fdlog : UInt64;
  __imp___fdnorm : UInt64;
  __imp___fdopen : UInt64;
  __imp___fdpcomp : UInt64;
  __imp___fdpoly : UInt64;
  __imp__fdopen : UInt64;
  __imp___fdscale : UInt64;
  __imp___fdsign : UInt64;
  __imp___fdsin : UInt64;
  __imp___fdtest : UInt64;
  __imp___fdunscale : UInt64;
  __imp___finite : UInt64;
  __imp__finite : UInt64;
  __imp___fpclass : UInt64;
  __imp___fpclassf : UInt64;
  __imp___ftol : UInt64;
  __imp__fpclass : UInt64;
  __imp___get_FMA3_enable : UInt64;
  __imp___hypot : UInt64;
  __imp___hypotf : UInt64;
  __imp___isnan : UInt64;
  __imp__hypot : UInt64;
  __imp___j0 : UInt64;
  __imp__j0 : UInt64;
  __imp___j1 : UInt64;
  __imp__jn : UInt64;
  __imp___jn : UInt64;
  __imp___ld_int : UInt64;
  __imp___ldclass : UInt64;
  __imp___ldexp : UInt64;
  __imp___ldlog : UInt64;
  __imp__j1 : UInt64;
  __imp___ldpcomp : UInt64;
  __imp___ldpoly : UInt64;
  __imp___ldscale : UInt64;
  __imp___ldsign : UInt64;
  __imp___ldsin : UInt64;
  __imp___ldtest : UInt64;
  __imp___ldunscale : UInt64;
  __imp___libm_sse2_acos_precise : UInt64;
  __imp___libm_sse2_asin_precise : UInt64;
  __imp___libm_sse2_atan_precise : UInt64;
  __imp___libm_sse2_cos_precise : UInt64;
  __imp___libm_sse2_exp_precise : UInt64;
  __imp___libm_sse2_log10_precise : UInt64;
  __imp___libm_sse2_log_precise : UInt64;
  __imp___libm_sse2_pow_precise : UInt64;
  __imp___libm_sse2_sin_precise : UInt64;
  __imp___libm_sse2_sqrt_precise : UInt64;
  __imp___libm_sse2_tan_precise : UInt64;
  __imp___logb : UInt64;
  __imp___nextafter : UInt64;
  __imp__nextafter : UInt64;
  __imp___scalb : UInt64;
  __imp___set_SSE2_enable : UInt64;
  __imp___y0 : UInt64;
  __imp__y0 : UInt64;
  __imp___y1 : UInt64;
  __imp__y1 : UInt64;
  __imp___yn : UInt64;
  __imp__acos : UInt64;
  __imp__yn : UInt64;
  __imp__acosh : UInt64;
  __imp__acoshf : UInt64;
  __imp__acoshl : UInt64;
  __imp__asin : UInt64;
  __imp__asinh : UInt64;
  __imp__asinhf : UInt64;
  __imp__asinhl : UInt64;
  __imp__atan : UInt64;
  __imp__atan2 : UInt64;
  __imp__atanh : UInt64;
  __imp__atanhf : UInt64;
  __imp__atanhl : UInt64;
  __imp__cabs : UInt64;
  __imp__cabsf : UInt64;
  __imp__cabsl : UInt64;
  __imp__cacos : UInt64;
  __imp__cacosf : UInt64;
  __imp__cacosh : UInt64;
  __imp__cacoshf : UInt64;
  __imp__cacoshl : UInt64;
  __imp__cacosl : UInt64;
  __imp__carg : UInt64;
  __imp__cargf : UInt64;
  __imp__cargl : UInt64;
  __imp__casin : UInt64;
  __imp__casinf : UInt64;
  __imp__casinh : UInt64;
  __imp__casinhf : UInt64;
  __imp__casinhl : UInt64;
  __imp__casinl : UInt64;
  __imp__catan : UInt64;
  __imp__catanf : UInt64;
  __imp__catanh : UInt64;
  __imp__catanhf : UInt64;
  __imp__catanhl : UInt64;
  __imp__catanl : UInt64;
  __imp__cbrt : UInt64;
  __imp__cbrtf : UInt64;
  __imp__cbrtl : UInt64;
  __imp__ccos : UInt64;
  __imp__ccosf : UInt64;
  __imp__ccosh : UInt64;
  __imp__ccoshf : UInt64;
  __imp__ccoshl : UInt64;
  __imp__ccosl : UInt64;
  __imp__ceil : UInt64;
  __imp__cexp : UInt64;
  __imp__cexpf : UInt64;
  __imp__cexpl : UInt64;
  __imp__cimag : UInt64;
  __imp__cimagf : UInt64;
  __imp__cimagl : UInt64;
  __imp__clog : UInt64;
  __imp__clog10 : UInt64;
  __imp__clog10f : UInt64;
  __imp__clog10l : UInt64;
  __imp__clogf : UInt64;
  __imp__clogl : UInt64;
  __imp__conj : UInt64;
  __imp__conjf : UInt64;
  __imp__conjl : UInt64;
  __imp__copysign : UInt64;
  __imp__copysignf : UInt64;
  __imp__copysignl : UInt64;
  __imp__cos : UInt64;
  __imp__cosh : UInt64;
  __imp__cpow : UInt64;
  __imp__cpowf : UInt64;
  __imp__cpowl : UInt64;
  __imp__cproj : UInt64;
  __imp__cprojf : UInt64;
  __imp__cprojl : UInt64;
  __imp__creal : UInt64;
  __imp__crealf : UInt64;
  __imp__creall : UInt64;
  __imp__csin : UInt64;
  __imp__csinf : UInt64;
  __imp__csinh : UInt64;
  __imp__csinhf : UInt64;
  __imp__csinhl : UInt64;
  __imp__csinl : UInt64;
  __imp__csqrt : UInt64;
  __imp__csqrtf : UInt64;
  __imp__csqrtl : UInt64;
  __imp__ctan : UInt64;
  __imp__ctanf : UInt64;
  __imp__ctanh : UInt64;
  __imp__ctanhf : UInt64;
  __imp__ctanhl : UInt64;
  __imp__ctanl : UInt64;
  __imp__erf : UInt64;
  __imp__erfc : UInt64;
  __imp__erfcf : UInt64;
  __imp__erfcl : UInt64;
  __imp__erff : UInt64;
  __imp__erfl : UInt64;
  __imp__exp : UInt64;
  __imp__exp2 : UInt64;
  __imp__exp2f : UInt64;
  __imp__exp2l : UInt64;
  __imp__expm1 : UInt64;
  __imp__expm1f : UInt64;
  __imp__expm1l : UInt64;
  __imp__fabs : UInt64;
  __imp__fdim : UInt64;
  __imp__fdimf : UInt64;
  __imp__fdiml : UInt64;
  __imp__floor : UInt64;
  __imp__fma : UInt64;
  __imp__fmaf : UInt64;
  __imp__fmal : UInt64;
  __imp__fmax : UInt64;
  __imp__fmaxf : UInt64;
  __imp__fmaxl : UInt64;
  __imp__fmin : UInt64;
  __imp__fminf : UInt64;
  __imp__fminl : UInt64;
  __imp__fmod : UInt64;
  __imp__frexp : UInt64;
  __imp__ilogb : UInt64;
  __imp__ilogbf : UInt64;
  __imp__ilogbl : UInt64;
  __imp__ldexp : UInt64;
  __imp__lgamma : UInt64;
  __imp__lgammaf : UInt64;
  __imp__lgammal : UInt64;
  __imp__llrint : UInt64;
  __imp__llrintf : UInt64;
  __imp__llrintl : UInt64;
  __imp__llround : UInt64;
  __imp__llroundf : UInt64;
  __imp__llroundl : UInt64;
  __imp__log : UInt64;
  __imp__log10 : UInt64;
  __imp__log1p : UInt64;
  __imp__log1pf : UInt64;
  __imp__log1pl : UInt64;
  __imp__log2 : UInt64;
  __imp__log2f : UInt64;
  __imp__log2l : UInt64;
  __imp__logb : UInt64;
  __imp__logbf : UInt64;
  __imp__logbl : UInt64;
  __imp__lrint : UInt64;
  __imp__lrintf : UInt64;
  __imp__lrintl : UInt64;
  __imp__lround : UInt64;
  __imp__lroundf : UInt64;
  __imp__lroundl : UInt64;
  __imp__modf : UInt64;
  __imp__nan : UInt64;
  __imp__nanf : UInt64;
  __imp__nanl : UInt64;
  __imp__nearbyint : UInt64;
  __imp__nearbyintf : UInt64;
  __imp__nearbyintl : UInt64;
  __imp__nextafterf : UInt64;
  __imp__nextafterl : UInt64;
  __imp__nexttoward : UInt64;
  __imp__nexttowardf : UInt64;
  __imp__nexttowardl : UInt64;
  __imp__norm : UInt64;
  __imp__normf : UInt64;
  __imp__norml : UInt64;
  __imp__pow : UInt64;
  __imp__remainder : UInt64;
  __imp__remainderf : UInt64;
  __imp__remainderl : UInt64;
  __imp__remquo : UInt64;
  __imp__remquof : UInt64;
  __imp__remquol : UInt64;
  __imp__rint : UInt64;
  __imp__rintf : UInt64;
  __imp__rintl : UInt64;
  __imp__round : UInt64;
  __imp__roundf : UInt64;
  __imp__roundl : UInt64;
  __imp__scalbln : UInt64;
  __imp__scalblnf : UInt64;
  __imp__scalblnl : UInt64;
  __imp__scalbn : UInt64;
  __imp__scalbnf : UInt64;
  __imp__scalbnl : UInt64;
  __imp__sin : UInt64;
  __imp__sinh : UInt64;
  __imp__sqrt : UInt64;
  __imp__tan : UInt64;
  __imp__tanh : UInt64;
  __imp__tgamma : UInt64;
  __imp__tgammaf : UInt64;
  __imp__tgammal : UInt64;
  __imp__trunc : UInt64;
  __imp__truncf : UInt64;
  __imp__truncl : UInt64;
  __head_lib32_libapi_ms_win_crt_math_l1_1_0_a : UInt64;
  __lib32_libapi_ms_win_crt_math_l1_1_0_a_iname : UInt64;
  __imp_____lc_codepage_func : UInt64;
  __imp_____lc_collate_cp_func : UInt64;
  __imp_____lc_locale_name_func : UInt64;
  __imp_____mb_cur_max_func : UInt64;
  __imp_____mb_cur_max_l_func : UInt64;
  __imp____initialize_lconv_for_unsigned_char : UInt64;
  __imp____lconv_init : UInt64;
  __imp____pctype_func : UInt64;
  __imp____pwctype_func : UInt64;
  __imp___configthreadlocale : UInt64;
  __imp___create_locale : UInt64;
  __imp___free_locale : UInt64;
  __imp___get_current_locale : UInt64;
  __imp___getmbcp : UInt64;
  __imp___lock_locales : UInt64;
  __imp___setmbcp : UInt64;
  __imp___unlock_locales : UInt64;
  __imp___wcreate_locale : UInt64;
  __imp___wsetlocale : UInt64;
  __imp__localeconv : UInt64;
  __imp__setlocale : UInt64;
  __head_lib32_libapi_ms_win_crt_locale_l1_1_0_a : UInt64;
  __lib32_libapi_ms_win_crt_locale_l1_1_0_a_iname : UInt64;
  __imp___aligned_free : UInt64;
  __imp___aligned_malloc : UInt64;
  __imp___aligned_msize : UInt64;
  __imp___aligned_offset_malloc : UInt64;
  __imp___aligned_offset_realloc : UInt64;
  __imp___aligned_offset_recalloc : UInt64;
  __imp___aligned_realloc : UInt64;
  __imp___aligned_recalloc : UInt64;
  __imp___callnewh : UInt64;
  __imp___calloc_base : UInt64;
  __imp___expand : UInt64;
  __imp___free_base : UInt64;
  __imp___get_heap_handle : UInt64;
  __imp___heapchk : UInt64;
  __imp___heapmin : UInt64;
  __imp___heapwalk : UInt64;
  __imp___malloc_base : UInt64;
  __imp___msize : UInt64;
  __imp___query_new_handler : UInt64;
  __imp___query_new_mode : UInt64;
  __imp__heapwalk : UInt64;
  __imp___realloc_base : UInt64;
  __imp___recalloc : UInt64;
  __imp___set_new_mode : UInt64;
  __imp__calloc : UInt64;
  __imp__free : UInt64;
  __imp__malloc : UInt64;
  __imp__realloc : UInt64;
  __head_lib32_libapi_ms_win_crt_heap_l1_1_0_a : UInt64;
  __lib32_libapi_ms_win_crt_heap_l1_1_0_a_iname : UInt64;
  __imp___findclose : UInt64;
  __imp___findfirst : UInt64;
  __imp__access : UInt64;
  __imp___access : UInt64;
  __imp___access_s : UInt64;
  __imp___chdir : UInt64;
  __imp___chdrive : UInt64;
  __imp___chmod : UInt64;
  __imp__chmod : UInt64;
  __imp__chdir : UInt64;
  __imp___findfirst32 : UInt64;
  __imp___findfirst32i64 : UInt64;
  __imp___findfirst64 : UInt64;
  __imp___findfirst64i32 : UInt64;
  __imp___findnext : UInt64;
  __imp___findnext32 : UInt64;
  __imp___findnext32i64 : UInt64;
  __imp___findnext64 : UInt64;
  __imp___findnext64i32 : UInt64;
  __imp___fstat32 : UInt64;
  __imp___fstat32i64 : UInt64;
  __imp___fstat64 : UInt64;
  __imp___fstat64i32 : UInt64;
  __imp___fullpath : UInt64;
  __imp___getdiskfree : UInt64;
  __imp___getdrive : UInt64;
  __imp___getdrives : UInt64;
  __imp___lock_file : UInt64;
  __imp___makepath : UInt64;
  __imp___makepath_s : UInt64;
  __imp___mkdir : UInt64;
  __imp__rmdir : UInt64;
  __imp___rmdir : UInt64;
  __imp___splitpath : UInt64;
  __imp___splitpath_s : UInt64;
  __imp___stat32 : UInt64;
  __imp__mkdir : UInt64;
  __imp___stat32i64 : UInt64;
  __imp___stat64 : UInt64;
  __imp___stat64i32 : UInt64;
  __imp___umask : UInt64;
  __imp__umask : UInt64;
  __imp___umask_s : UInt64;
  __imp___unlink : UInt64;
  __imp___unlock_file : UInt64;
  __imp___waccess : UInt64;
  __imp__unlink : UInt64;
  __imp___waccess_s : UInt64;
  __imp___wchdir : UInt64;
  __imp___wchmod : UInt64;
  __imp___wfindfirst32 : UInt64;
  __imp___wfindfirst32i64 : UInt64;
  __imp___wfindfirst64 : UInt64;
  __imp___wfindfirst64i32 : UInt64;
  __imp___wfindnext32 : UInt64;
  __imp___wfindnext32i64 : UInt64;
  __imp___wfindnext64 : UInt64;
  __imp___wfindnext64i32 : UInt64;
  __imp___wfullpath : UInt64;
  __imp___wmakepath : UInt64;
  __imp___wmakepath_s : UInt64;
  __imp___wmkdir : UInt64;
  __imp___wremove : UInt64;
  __imp___wrename : UInt64;
  __imp___wrmdir : UInt64;
  __imp___wsplitpath : UInt64;
  __imp___wsplitpath_s : UInt64;
  __imp___wstat32 : UInt64;
  __imp___wstat32i64 : UInt64;
  __imp___wstat64 : UInt64;
  __imp___wstat64i32 : UInt64;
  __imp___wunlink : UInt64;
  __imp__remove : UInt64;
  __imp__rename : UInt64;
  __head_lib32_libapi_ms_win_crt_filesystem_l1_1_0_a : UInt64;
  __lib32_libapi_ms_win_crt_filesystem_l1_1_0_a_iname : UInt64;
  __imp____p__environ : UInt64;
  __imp____p__wenviron : UInt64;
  __imp___dupenv_s : UInt64;
  __imp___putenv : UInt64;
  __imp__putenv : UInt64;
  __imp___putenv_s : UInt64;
  __imp___searchenv : UInt64;
  __imp___searchenv_s : UInt64;
  __imp___wdupenv_s : UInt64;
  __imp__searchenv : UInt64;
  __imp___wgetcwd : UInt64;
  __imp___wgetdcwd : UInt64;
  __imp___wgetenv : UInt64;
  __imp___wgetenv_s : UInt64;
  __imp___wputenv : UInt64;
  __imp___wputenv_s : UInt64;
  __imp___wsearchenv : UInt64;
  __imp___wsearchenv_s : UInt64;
  __imp__getenv : UInt64;
  __imp__getenv_s : UInt64;
  __head_lib32_libapi_ms_win_crt_environment_l1_1_0_a : UInt64;
  __lib32_libapi_ms_win_crt_environment_l1_1_0_a_iname : UInt64;
  __imp____toascii : UInt64;
  __imp___atodbl : UInt64;
  __imp___atodbl_l : UInt64;
  __imp___atof_l : UInt64;
  __imp___atoflt : UInt64;
  __imp___atoflt_l : UInt64;
  __imp__toascii : UInt64;
  __imp___atoi64 : UInt64;
  __imp___atoi64_l : UInt64;
  __imp___atoi_l : UInt64;
  __imp___atol_l : UInt64;
  __imp___atoldbl : UInt64;
  __imp___atoldbl_l : UInt64;
  __imp___atoll_l : UInt64;
  __imp___ecvt : UInt64;
  __imp__ecvt : UInt64;
  __imp___ecvt_s : UInt64;
  __imp___fcvt : UInt64;
  __imp___fcvt_s : UInt64;
  __imp__gcvt : UInt64;
  __imp___gcvt : UInt64;
  __imp__fcvt : UInt64;
  __imp___gcvt_s : UInt64;
  __imp___i64toa : UInt64;
  __imp___i64toa_s : UInt64;
  __imp___i64tow : UInt64;
  __imp___i64tow_s : UInt64;
  __imp___itoa : UInt64;
  __imp__itoa : UInt64;
  __imp___itoa_s : UInt64;
  __imp___itow : UInt64;
  __imp___itow_s : UInt64;
  __imp___ltoa : UInt64;
  __imp___ltoa_s : UInt64;
  __imp___ltow : UInt64;
  __imp___ltow_s : UInt64;
  __imp___strtod_l : UInt64;
  __imp___strtof_l : UInt64;
  __imp__ltoa : UInt64;
  __imp___strtoi64 : UInt64;
  __imp___strtoi64_l : UInt64;
  __imp___strtoimax_l : UInt64;
  __imp___strtol_l : UInt64;
  __imp___strtold_l : UInt64;
  __imp___strtoll_l : UInt64;
  __imp___strtoui64 : UInt64;
  __imp___strtoui64_l : UInt64;
  __imp___strtoul_l : UInt64;
  __imp___strtoull_l : UInt64;
  __imp___strtoumax_l : UInt64;
  __imp___ui64toa : UInt64;
  __imp___ui64toa_s : UInt64;
  __imp___ui64tow : UInt64;
  __imp___ui64tow_s : UInt64;
  __imp___ultoa : UInt64;
  __imp___ultoa_s : UInt64;
  __imp___ultow : UInt64;
  __imp___ultow_s : UInt64;
  __imp___wcstod_l : UInt64;
  __imp___wcstof_l : UInt64;
  __imp___wcstoi64 : UInt64;
  __imp___wcstoi64_l : UInt64;
  __imp___wcstoimax_l : UInt64;
  __imp___wcstol_l : UInt64;
  __imp___wcstold_l : UInt64;
  __imp___wcstoll_l : UInt64;
  __imp___wcstombs_l : UInt64;
  __imp___wcstombs_s_l : UInt64;
  __imp___wcstoui64 : UInt64;
  __imp___wcstoui64_l : UInt64;
  __imp___wcstoul_l : UInt64;
  __imp___wcstoull_l : UInt64;
  __imp___wcstoumax_l : UInt64;
  __imp___wctomb_l : UInt64;
  __imp___wctomb_s_l : UInt64;
  __imp___wtof : UInt64;
  __imp___wtof_l : UInt64;
  __imp___wtoi : UInt64;
  __imp___wtoi64 : UInt64;
  __imp___wtoi64_l : UInt64;
  __imp___wtoi_l : UInt64;
  __imp___wtol : UInt64;
  __imp___wtol_l : UInt64;
  __imp___wtoll : UInt64;
  __imp___wtoll_l : UInt64;
  __imp__atof : UInt64;
  __imp__atoi : UInt64;
  __imp__atol : UInt64;
  __imp__atoll : UInt64;
  __imp__btowc : UInt64;
  __imp__c16rtomb : UInt64;
  __imp__c32rtomb : UInt64;
  __imp__mbrtoc16 : UInt64;
  __imp__mbrtoc32 : UInt64;
  __imp__mbrtowc : UInt64;
  __imp__mbsrtowcs : UInt64;
  __imp__mbsrtowcs_s : UInt64;
  __imp__mbstowcs : UInt64;
  __imp__mbstowcs_s : UInt64;
  __imp__mbtowc : UInt64;
  __imp__strtod : UInt64;
  __imp__strtof : UInt64;
  __imp__strtoimax : UInt64;
  __imp__strtol : UInt64;
  __imp__strtoll : UInt64;
  __imp__strtoul : UInt64;
  __imp__strtoull : UInt64;
  __imp__strtoumax : UInt64;
  __imp__wcrtomb : UInt64;
  __imp__wcrtomb_s : UInt64;
  __imp__wcsrtombs : UInt64;
  __imp__wcsrtombs_s : UInt64;
  __imp__wcstod : UInt64;
  __imp__wcstof : UInt64;
  __imp__wcstoimax : UInt64;
  __imp__wcstol : UInt64;
  __imp__wcstoll : UInt64;
  __imp__wcstombs : UInt64;
  __imp__wcstombs_s : UInt64;
  __imp__wcstoul : UInt64;
  __imp__wcstoull : UInt64;
  __imp__wcstoumax : UInt64;
  __imp__wctob : UInt64;
  __imp__wctomb : UInt64;
  __imp__wctomb_s : UInt64;
  __imp__wctrans : UInt64;
  __head_lib32_libapi_ms_win_crt_convert_l1_1_0_a : UInt64;
  __lib32_libapi_ms_win_crt_convert_l1_1_0_a_iname : UInt64;
  __imp____conio_common_vcprintf : UInt64;
  __imp____conio_common_vcprintf_p : UInt64;
  __imp____conio_common_vcprintf_s : UInt64;
  __imp____conio_common_vcscanf : UInt64;
  __imp____conio_common_vcwprintf : UInt64;
  __imp____conio_common_vcwprintf_p : UInt64;
  __imp____conio_common_vcwprintf_s : UInt64;
  __imp____conio_common_vcwscanf : UInt64;
  __imp___cgets : UInt64;
  __imp___cgets_s : UInt64;
  __imp___cgetws : UInt64;
  __imp___cgetws_s : UInt64;
  __imp___cputs : UInt64;
  __imp___cputws : UInt64;
  __imp___getch : UInt64;
  __imp__getch : UInt64;
  __imp___getch_nolock : UInt64;
  __imp___getche : UInt64;
  __imp___getche_nolock : UInt64;
  __imp___getwch : UInt64;
  __imp___getwch_nolock : UInt64;
  __imp___getwche : UInt64;
  __imp___getwche_nolock : UInt64;
  __imp__getche : UInt64;
  __imp___putch : UInt64;
  __imp__putch : UInt64;
  __imp___putch_nolock : UInt64;
  __imp___putwch : UInt64;
  __imp___putwch_nolock : UInt64;
  __imp___ungetch : UInt64;
  __imp___ungetch_nolock : UInt64;
  __imp__ungetch : UInt64;
  __imp___ungetwch : UInt64;
  __imp___ungetwch_nolock : UInt64;
  __head_lib32_libapi_ms_win_crt_conio_l1_1_0_a : UInt64;
  __lib32_libapi_ms_win_crt_conio_l1_1_0_a_iname : UInt64;
{$ENDIF}
{$IFDEF WIN64}
var
  __mingw_module_is_dll : UInt64;
{$ENDIF}
{$IFDEF WIN32}
var
  ___mingw_module_is_dll : UInt64;
{$ENDIF}
implementation
{$IFDEF WIN64}
function  malloc(size: NativeInt): Pointer; cdecl;
begin
  try
    Result := AllocMem(size);
  except
    Result := nil;
  end;
end;

function realloc(P: Pointer; NewSize: NativeInt): Pointer; cdecl;
begin
  try
    ReallocMem(P, Newsize);
    Result := P;
  except
    Result := nil;
  end;
end;

procedure free(pBlock: Pointer); cdecl;
begin
  FreeMem(pBlock);
end;

function calloc(nitems,size : NativeInt):Pointer;cdecl;
begin
  try
    Result := Allocmem(nitems*size);
  except
    Result := nil;
  end;
end;
{$ENDIF}
{$IFDEF WIN32}
function  _malloc(size: NativeInt): Pointer; cdecl;
begin
  try
    Result := AllocMem(size);
  except
    Result := nil;
  end;
end;

function _realloc(P: Pointer; NewSize: NativeInt): Pointer; cdecl;
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

function _calloc(nitems,size : NativeInt):Pointer;cdecl;
begin
  try
    Result := Allocmem(nitems*size);
  except
    Result := nil;
  end;
end;
{$ENDIF}
end.
