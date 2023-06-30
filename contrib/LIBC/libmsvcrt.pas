unit libmsvcrt;
interface
{$IFDEF WIN64}
procedure _set_errno;external;
{$L x64/lib64_libmsvcrt_extra_a-seterrno.o}
procedure __p__wcmdln;external;
{$L x64/lib64_libmsvcrt_extra_a-__p__wcmdln.o}
procedure __p__fmode;external;
{$L x64/lib64_libmsvcrt_extra_a-__p__fmode.o}
procedure __p__commode;external;
{$L x64/lib64_libmsvcrt_extra_a-__p__commode.o}
procedure __p__acmdln;external;
{$L x64/lib64_libmsvcrt_extra_a-__p__acmdln.o}
procedure __p___argv;external;
{$L x64/lib64_libmsvcrt_extra_a-__p___argv.o}
procedure _lock_file;external;
{$L x64/lib64_libmsvcrt_extra_a-mingw_lock.o}
procedure _ftelli64;external;
{$L x64/lib64_libmsvcrt_extra_a-fseeki64.o}
procedure sprintf_s;external;
{$L x64/lib64_libmsvcrt_extra_a-sprintf_s.o}
procedure _vswprintf_p;external;
{$L x64/lib64_libmsvcrt_extra_a-_vswprintf_p.o}
procedure _vscwprintf_p;external;
{$L x64/lib64_libmsvcrt_extra_a-_vscwprintf_p.o}
procedure _vscprintf_p;external;
{$L x64/lib64_libmsvcrt_extra_a-_vscprintf_p.o}
procedure _cwprintf_s_l;external;
{$L x64/lib64_libmsvcrt_extra_a-_cwprintf_s_l.o}
procedure _cwprintf_s;external;
{$L x64/lib64_libmsvcrt_extra_a-_cwprintf_s.o}
procedure _cprintf_s_l;external;
{$L x64/lib64_libmsvcrt_extra_a-_cprintf_s_l.o}
procedure _cprintf_s;external;
{$L x64/lib64_libmsvcrt_extra_a-_cprintf_s.o}
procedure _set_purecall_handler;external;
{$L x64/lib64_libmsvcrt_extra_a-purecall.o}
procedure _get_invalid_parameter_handler;external;
{$L x64/lib64_libmsvcrt_extra_a-invalid_parameter_handler.o}
procedure _configthreadlocale;external;
{$L x64/lib64_libmsvcrt_extra_a-_configthreadlocale.o}
procedure frexp;external;
{$L x64/lib64_libmsvcrt_common_a-frexp.o}
procedure vsnprintf;external;
{$L x64/lib64_libmsvcrt_common_a-vsnprintf_alias.o}
procedure snprintf;external;
{$L x64/lib64_libmsvcrt_common_a-snprintf_alias.o}
procedure __acrt_iob_func;external;
{$L x64/lib64_libmsvcrt_common_a-acrt_iob_func.o}
procedure _putwc_nolock;external;
{$L x64/lib64_libmsvcrt_common_a-_putwc_nolock.o}
procedure _putc_nolock;external;
{$L x64/lib64_libmsvcrt_common_a-_putc_nolock.o}
procedure _getwc_nolock;external;
{$L x64/lib64_libmsvcrt_common_a-_getwc_nolock.o}
procedure _getc_nolock;external;
{$L x64/lib64_libmsvcrt_common_a-_getc_nolock.o}
procedure mbrtoc32;external;
{$L x64/lib64_libmsvcrt_common_a-uchar_mbrtoc32.o}
procedure mbrtoc16;external;
{$L x64/lib64_libmsvcrt_common_a-uchar_mbrtoc16.o}
procedure c32rtomb;external;
{$L x64/lib64_libmsvcrt_common_a-uchar_c32rtomb.o}
procedure c16rtomb;external;
{$L x64/lib64_libmsvcrt_common_a-uchar_c16rtomb.o}
procedure _initialize_onexit_table;external;
{$L x64/lib64_libmsvcrt_common_a-onexit_table.o}
procedure mbsinit;external;
{$L x64/lib64_libmsvcrt_common_a-mbsinit.o}
procedure _CxxThrowException;external;
{$L x64/libmsvcrt_defs00048.o}
procedure _Getdays;external;
{$L x64/libmsvcrt_defs00049.o}
procedure _Getmonths;external;
{$L x64/libmsvcrt_defs00050.o}
procedure _Gettnames;external;
{$L x64/libmsvcrt_defs00051.o}
procedure _Strftime;external;
{$L x64/libmsvcrt_defs00053.o}
procedure _XcptFilter;external;
{$L x64/libmsvcrt_defs00054.o}
procedure __C_specific_handler;external;
{$L x64/libmsvcrt_defs00055.o}
procedure __CppXcptFilter;external;
{$L x64/libmsvcrt_defs00056.o}
procedure __CxxFrameHandler;external;
{$L x64/libmsvcrt_defs00057.o}
procedure __DestructExceptionObject;external;
{$L x64/libmsvcrt_defs00058.o}
procedure __RTCastToVoid;external;
{$L x64/libmsvcrt_defs00059.o}
procedure __RTDynamicCast;external;
{$L x64/libmsvcrt_defs00060.o}
procedure __RTtypeid;external;
{$L x64/libmsvcrt_defs00061.o}
procedure __STRINGTOLD;external;
{$L x64/libmsvcrt_defs00062.o}
procedure ___lc_codepage_func;external;
{$L x64/libmsvcrt_defs00063.o}
procedure ___lc_collate_cp_func;external;
{$L x64/libmsvcrt_defs00064.o}
procedure ___lc_handle_func;external;
{$L x64/libmsvcrt_defs00065.o}
procedure ___mb_cur_max_func;external;
{$L x64/libmsvcrt_defs00066.o}
procedure ___setlc_active_func;external;
{$L x64/libmsvcrt_defs00067.o}
procedure ___unguarded_readlc_active_add_func;external;
{$L x64/libmsvcrt_defs00068.o}
procedure __crtCompareStringA;external;
{$L x64/libmsvcrt_defs00072.o}
procedure __crtCompareStringW;external;
{$L x64/libmsvcrt_defs00073.o}
procedure __crtGetLocaleInfoW;external;
{$L x64/libmsvcrt_defs00074.o}
procedure __crtGetStringTypeW;external;
{$L x64/libmsvcrt_defs00075.o}
procedure __crtLCMapStringA;external;
{$L x64/libmsvcrt_defs00076.o}
procedure __crtLCMapStringW;external;
{$L x64/libmsvcrt_defs00077.o}
procedure __dllonexit;external;
{$L x64/libmsvcrt_defs00078.o}
procedure __doserrno;external;
{$L x64/libmsvcrt_defs00079.o}
procedure __fpecode;external;
{$L x64/libmsvcrt_defs00080.o}
procedure __getmainargs;external;
{$L x64/libmsvcrt_defs00081.o}
procedure __iob_func;external;
{$L x64/libmsvcrt_defs00083.o}
procedure __isascii;external;
{$L x64/libmsvcrt_defs00084.o}
procedure __iscsym;external;
{$L x64/libmsvcrt_defs00085.o}
procedure __iscsymf;external;
{$L x64/libmsvcrt_defs00086.o}
procedure isascii;external;
{$L x64/libmsvcrt_defs00088.o}
procedure __lconv_init;external;
{$L x64/libmsvcrt_defs00091.o}
procedure __pctype_func;external;
{$L x64/libmsvcrt_defs00093.o}
procedure __pwctype_func;external;
{$L x64/libmsvcrt_defs00095.o}
procedure __pxcptinfoptrs;external;
{$L x64/libmsvcrt_defs00096.o}
procedure __set_app_type;external;
{$L x64/libmsvcrt_defs00097.o}
procedure __setusermatherr;external;
{$L x64/libmsvcrt_defs00099.o}
procedure __threadhandle;external;
{$L x64/libmsvcrt_defs00100.o}
procedure __threadid;external;
{$L x64/libmsvcrt_defs00101.o}
procedure __toascii;external;
{$L x64/libmsvcrt_defs00102.o}
procedure toascii;external;
{$L x64/libmsvcrt_defs00103.o}
procedure __unDName;external;
{$L x64/libmsvcrt_defs00104.o}
procedure __unDNameEx;external;
{$L x64/libmsvcrt_defs00105.o}
procedure __uncaught_exception;external;
{$L x64/libmsvcrt_defs00106.o}
procedure __wcserror;external;
{$L x64/libmsvcrt_defs00109.o}
procedure __wcserror_s;external;
{$L x64/libmsvcrt_defs00110.o}
procedure __wgetmainargs;external;
{$L x64/libmsvcrt_defs00111.o}
procedure _abs64;external;
{$L x64/libmsvcrt_defs00113.o}
procedure access;external;
{$L x64/libmsvcrt_defs00114.o}
procedure _access;external;
{$L x64/libmsvcrt_defs00115.o}
procedure _aligned_free;external;
{$L x64/libmsvcrt_defs00118.o}
procedure _aligned_malloc;external;
{$L x64/libmsvcrt_defs00119.o}
procedure _aligned_offset_malloc;external;
{$L x64/libmsvcrt_defs00120.o}
procedure _aligned_offset_realloc;external;
{$L x64/libmsvcrt_defs00121.o}
procedure _aligned_realloc;external;
{$L x64/libmsvcrt_defs00122.o}
procedure _amsg_exit;external;
{$L x64/libmsvcrt_defs00123.o}
procedure _assert;external;
{$L x64/libmsvcrt_defs00124.o}
procedure _atodbl;external;
{$L x64/libmsvcrt_defs00125.o}
procedure _atodbl_l;external;
{$L x64/libmsvcrt_defs00126.o}
procedure _atof_l;external;
{$L x64/libmsvcrt_defs00127.o}
procedure _atoflt_l;external;
{$L x64/libmsvcrt_defs00128.o}
procedure _atoi64;external;
{$L x64/libmsvcrt_defs00129.o}
procedure _atoi64_l;external;
{$L x64/libmsvcrt_defs00130.o}
procedure _atoi_l;external;
{$L x64/libmsvcrt_defs00131.o}
procedure _atol_l;external;
{$L x64/libmsvcrt_defs00132.o}
procedure _atoldbl;external;
{$L x64/libmsvcrt_defs00133.o}
procedure _atoldbl_l;external;
{$L x64/libmsvcrt_defs00134.o}
procedure _beep;external;
{$L x64/libmsvcrt_defs00135.o}
procedure _beginthread;external;
{$L x64/libmsvcrt_defs00136.o}
procedure _beginthreadex;external;
{$L x64/libmsvcrt_defs00137.o}
procedure _c_exit;external;
{$L x64/libmsvcrt_defs00138.o}
procedure _callnewh;external;
{$L x64/libmsvcrt_defs00140.o}
procedure _cexit;external;
{$L x64/libmsvcrt_defs00141.o}
procedure _cgets;external;
{$L x64/libmsvcrt_defs00142.o}
procedure _cgetws;external;
{$L x64/libmsvcrt_defs00143.o}
procedure _chdir;external;
{$L x64/libmsvcrt_defs00144.o}
procedure _chdrive;external;
{$L x64/libmsvcrt_defs00145.o}
procedure _chgsign;external;
{$L x64/libmsvcrt_defs00146.o}
procedure _chgsignf;external;
{$L x64/libmsvcrt_defs00147.o}
procedure chmod;external;
{$L x64/libmsvcrt_defs00148.o}
procedure _chmod;external;
{$L x64/libmsvcrt_defs00149.o}
procedure chsize;external;
{$L x64/libmsvcrt_defs00150.o}
procedure _chsize;external;
{$L x64/libmsvcrt_defs00151.o}
procedure _clearfp;external;
{$L x64/libmsvcrt_defs00152.o}
procedure chgsign;external;
{$L x64/libmsvcrt_defs00153.o}
procedure _close;external;
{$L x64/libmsvcrt_defs00154.o}
procedure close;external;
{$L x64/libmsvcrt_defs00155.o}
procedure _commit;external;
{$L x64/libmsvcrt_defs00156.o}
procedure _control87;external;
{$L x64/libmsvcrt_defs00158.o}
procedure _controlfp;external;
{$L x64/libmsvcrt_defs00159.o}
procedure _copysign;external;
{$L x64/libmsvcrt_defs00160.o}
procedure _copysignf;external;
{$L x64/libmsvcrt_defs00161.o}
procedure _cprintf;external;
{$L x64/libmsvcrt_defs00162.o}
procedure _cprintf_l;external;
{$L x64/libmsvcrt_defs00163.o}
procedure _cprintf_p;external;
{$L x64/libmsvcrt_defs00164.o}
procedure _cprintf_p_l;external;
{$L x64/libmsvcrt_defs00165.o}
procedure chdir;external;
{$L x64/libmsvcrt_defs00166.o}
procedure _cputs;external;
{$L x64/libmsvcrt_defs00167.o}
procedure iscsym;external;
{$L x64/libmsvcrt_defs00168.o}
procedure _cputws;external;
{$L x64/libmsvcrt_defs00169.o}
procedure _creat;external;
{$L x64/libmsvcrt_defs00170.o}
procedure creat;external;
{$L x64/libmsvcrt_defs00171.o}
procedure _cscanf;external;
{$L x64/libmsvcrt_defs00172.o}
procedure _cscanf_l;external;
{$L x64/libmsvcrt_defs00173.o}
procedure _cscanf_s;external;
{$L x64/libmsvcrt_defs00174.o}
procedure _cscanf_s_l;external;
{$L x64/libmsvcrt_defs00175.o}
procedure _ctime64;external;
{$L x64/libmsvcrt_defs00176.o}
procedure _ctype;external;
{$L x64/libmsvcrt_defs00177.o}
procedure _cwait;external;
{$L x64/libmsvcrt_defs00178.o}
procedure _cwprintf;external;
{$L x64/libmsvcrt_defs00179.o}
procedure cwait;external;
{$L x64/libmsvcrt_defs00180.o}
procedure _cwprintf_l;external;
{$L x64/libmsvcrt_defs00181.o}
procedure _cwprintf_p;external;
{$L x64/libmsvcrt_defs00182.o}
procedure _cwprintf_p_l;external;
{$L x64/libmsvcrt_defs00183.o}
procedure _cwscanf;external;
{$L x64/libmsvcrt_defs00184.o}
procedure _cwscanf_l;external;
{$L x64/libmsvcrt_defs00185.o}
procedure _cwscanf_s;external;
{$L x64/libmsvcrt_defs00186.o}
procedure _cwscanf_s_l;external;
{$L x64/libmsvcrt_defs00187.o}
procedure _difftime32;external;
{$L x64/libmsvcrt_defs00189.o}
procedure _difftime64;external;
{$L x64/libmsvcrt_defs00190.o}
procedure daylight;external;
{$L x64/libmsvcrt_defs00191.o}
procedure dup;external;
{$L x64/libmsvcrt_defs00193.o}
procedure _dup;external;
{$L x64/libmsvcrt_defs00194.o}
procedure dup2;external;
{$L x64/libmsvcrt_defs00195.o}
procedure _dup2;external;
{$L x64/libmsvcrt_defs00196.o}
procedure _ecvt;external;
{$L x64/libmsvcrt_defs00197.o}
procedure ecvt;external;
{$L x64/libmsvcrt_defs00198.o}
procedure _ecvt_s;external;
{$L x64/libmsvcrt_defs00199.o}
procedure _endthread;external;
{$L x64/libmsvcrt_defs00200.o}
procedure _endthreadex;external;
{$L x64/libmsvcrt_defs00201.o}
procedure _eof;external;
{$L x64/libmsvcrt_defs00203.o}
procedure _errno;external;
{$L x64/libmsvcrt_defs00204.o}
procedure _execl;external;
{$L x64/libmsvcrt_defs00205.o}
procedure _execle;external;
{$L x64/libmsvcrt_defs00206.o}
procedure execlp;external;
{$L x64/libmsvcrt_defs00207.o}
procedure _execlp;external;
{$L x64/libmsvcrt_defs00208.o}
procedure execle;external;
{$L x64/libmsvcrt_defs00209.o}
procedure _execlpe;external;
{$L x64/libmsvcrt_defs00210.o}
procedure execlpe;external;
{$L x64/libmsvcrt_defs00211.o}
procedure execve;external;
{$L x64/libmsvcrt_defs00212.o}
procedure execv;external;
{$L x64/libmsvcrt_defs00213.o}
procedure _execve;external;
{$L x64/libmsvcrt_defs00214.o}
procedure _execv;external;
{$L x64/libmsvcrt_defs00215.o}
procedure eof;external;
{$L x64/libmsvcrt_defs00216.o}
procedure execl;external;
{$L x64/libmsvcrt_defs00217.o}
procedure execvp;external;
{$L x64/libmsvcrt_defs00218.o}
procedure _execvp;external;
{$L x64/libmsvcrt_defs00219.o}
procedure _execvpe;external;
{$L x64/libmsvcrt_defs00220.o}
procedure _exit;external;
{$L x64/libmsvcrt_defs00221.o}
procedure _expand;external;
{$L x64/libmsvcrt_defs00222.o}
procedure _fcloseall;external;
{$L x64/libmsvcrt_defs00223.o}
procedure execvpe;external;
{$L x64/libmsvcrt_defs00224.o}
procedure fcvt;external;
{$L x64/libmsvcrt_defs00225.o}
procedure _fcvt;external;
{$L x64/libmsvcrt_defs00226.o}
procedure _fcvt_s;external;
{$L x64/libmsvcrt_defs00227.o}
procedure _fdopen;external;
{$L x64/libmsvcrt_defs00228.o}
procedure _fgetchar;external;
{$L x64/libmsvcrt_defs00229.o}
procedure _fgetwchar;external;
{$L x64/libmsvcrt_defs00230.o}
procedure fgetwchar;external;
{$L x64/libmsvcrt_defs00231.o}
procedure fgetchar;external;
{$L x64/libmsvcrt_defs00232.o}
procedure fdopen;external;
{$L x64/libmsvcrt_defs00233.o}
procedure _filbuf;external;
{$L x64/libmsvcrt_defs00234.o}
procedure filelength;external;
{$L x64/libmsvcrt_defs00236.o}
procedure _filelength;external;
{$L x64/libmsvcrt_defs00237.o}
procedure _filelengthi64;external;
{$L x64/libmsvcrt_defs00238.o}
procedure _fileno;external;
{$L x64/libmsvcrt_defs00239.o}
procedure _findclose;external;
{$L x64/libmsvcrt_defs00240.o}
procedure fileno;external;
{$L x64/libmsvcrt_defs00241.o}
procedure _findfirst64i32;external;
{$L x64/libmsvcrt_defs00242.o}
procedure _findfirst;external;
{$L x64/libmsvcrt_defs00243.o}
procedure _findfirst64;external;
{$L x64/libmsvcrt_defs00244.o}
procedure _findfirsti64;external;
{$L x64/libmsvcrt_defs00245.o}
procedure _findnext64i32;external;
{$L x64/libmsvcrt_defs00246.o}
procedure _findnext;external;
{$L x64/libmsvcrt_defs00247.o}
procedure _findnext64;external;
{$L x64/libmsvcrt_defs00248.o}
procedure _findnexti64;external;
{$L x64/libmsvcrt_defs00249.o}
procedure finite;external;
{$L x64/libmsvcrt_defs00250.o}
procedure _finite;external;
{$L x64/libmsvcrt_defs00251.o}
procedure _finitef;external;
{$L x64/libmsvcrt_defs00252.o}
procedure _flushall;external;
{$L x64/libmsvcrt_defs00253.o}
procedure _flsbuf;external;
{$L x64/libmsvcrt_defs00255.o}
procedure fpclass;external;
{$L x64/libmsvcrt_defs00256.o}
procedure _fpclass;external;
{$L x64/libmsvcrt_defs00257.o}
procedure _fpclassf;external;
{$L x64/libmsvcrt_defs00258.o}
procedure _fprintf_l;external;
{$L x64/libmsvcrt_defs00260.o}
procedure _fprintf_p;external;
{$L x64/libmsvcrt_defs00261.o}
procedure _fprintf_p_l;external;
{$L x64/libmsvcrt_defs00262.o}
procedure _fprintf_s_l;external;
{$L x64/libmsvcrt_defs00263.o}
procedure _fputchar;external;
{$L x64/libmsvcrt_defs00264.o}
procedure fputwchar;external;
{$L x64/libmsvcrt_defs00265.o}
procedure _fputwchar;external;
{$L x64/libmsvcrt_defs00266.o}
procedure _fscanf_l;external;
{$L x64/libmsvcrt_defs00267.o}
procedure _fscanf_s_l;external;
{$L x64/libmsvcrt_defs00268.o}
procedure _fsopen;external;
{$L x64/libmsvcrt_defs00269.o}
procedure fputchar;external;
{$L x64/libmsvcrt_defs00270.o}
procedure _fstat;external;
{$L x64/libmsvcrt_defs00271.o}
procedure _fstat64;external;
{$L x64/libmsvcrt_defs00272.o}
procedure _fstati64;external;
{$L x64/libmsvcrt_defs00273.o}
procedure _ftime;external;
{$L x64/libmsvcrt_defs00274.o}
procedure _ftime32;external;
{$L x64/libmsvcrt_defs00275.o}
procedure _ftime32_s;external;
{$L x64/libmsvcrt_defs00276.o}
procedure _ftime64;external;
{$L x64/libmsvcrt_defs00277.o}
procedure _ftime64_s;external;
{$L x64/libmsvcrt_defs00278.o}
procedure _fullpath;external;
{$L x64/libmsvcrt_defs00279.o}
procedure _fstat64i32;external;
{$L x64/libmsvcrt_defs00280.o}
procedure _ftime_s;external;
{$L x64/libmsvcrt_defs00281.o}
procedure _futime;external;
{$L x64/libmsvcrt_defs00282.o}
procedure _futime32;external;
{$L x64/libmsvcrt_defs00283.o}
procedure _futime64;external;
{$L x64/libmsvcrt_defs00284.o}
procedure _fwprintf_l;external;
{$L x64/libmsvcrt_defs00285.o}
procedure _fwprintf_p;external;
{$L x64/libmsvcrt_defs00286.o}
procedure _fwprintf_p_l;external;
{$L x64/libmsvcrt_defs00287.o}
procedure _fwprintf_s_l;external;
{$L x64/libmsvcrt_defs00288.o}
procedure _fwscanf_l;external;
{$L x64/libmsvcrt_defs00289.o}
procedure _fwscanf_s_l;external;
{$L x64/libmsvcrt_defs00290.o}
procedure _gcvt;external;
{$L x64/libmsvcrt_defs00291.o}
procedure gcvt;external;
{$L x64/libmsvcrt_defs00292.o}
procedure _gcvt_s;external;
{$L x64/libmsvcrt_defs00293.o}
procedure _get_heap_handle;external;
{$L x64/libmsvcrt_defs00294.o}
procedure _get_osfhandle;external;
{$L x64/libmsvcrt_defs00295.o}
procedure _get_sbh_threshold;external;
{$L x64/libmsvcrt_defs00296.o}
procedure _getch;external;
{$L x64/libmsvcrt_defs00297.o}
procedure getche;external;
{$L x64/libmsvcrt_defs00298.o}
procedure _getche;external;
{$L x64/libmsvcrt_defs00299.o}
procedure _getcwd;external;
{$L x64/libmsvcrt_defs00300.o}
procedure _getdcwd;external;
{$L x64/libmsvcrt_defs00301.o}
procedure _getdiskfree;external;
{$L x64/libmsvcrt_defs00302.o}
procedure getcwd;external;
{$L x64/libmsvcrt_defs00303.o}
procedure _getdllprocaddr;external;
{$L x64/libmsvcrt_defs00304.o}
procedure getch;external;
{$L x64/libmsvcrt_defs00305.o}
procedure _getdrive;external;
{$L x64/libmsvcrt_defs00306.o}
procedure _getdrives;external;
{$L x64/libmsvcrt_defs00307.o}
procedure _getmaxstdio;external;
{$L x64/libmsvcrt_defs00308.o}
procedure _getmbcp;external;
{$L x64/libmsvcrt_defs00309.o}
procedure getpid;external;
{$L x64/libmsvcrt_defs00310.o}
procedure _getpid;external;
{$L x64/libmsvcrt_defs00311.o}
procedure _getsystime;external;
{$L x64/libmsvcrt_defs00312.o}
procedure getw;external;
{$L x64/libmsvcrt_defs00313.o}
procedure _getw;external;
{$L x64/libmsvcrt_defs00314.o}
procedure _getwch;external;
{$L x64/libmsvcrt_defs00315.o}
procedure _getwche;external;
{$L x64/libmsvcrt_defs00316.o}
procedure _getws;external;
{$L x64/libmsvcrt_defs00317.o}
procedure _gmtime32;external;
{$L x64/libmsvcrt_defs00318.o}
procedure _gmtime64;external;
{$L x64/libmsvcrt_defs00319.o}
procedure _heapadd;external;
{$L x64/libmsvcrt_defs00320.o}
procedure _heapchk;external;
{$L x64/libmsvcrt_defs00321.o}
procedure _heapmin;external;
{$L x64/libmsvcrt_defs00322.o}
procedure _heapset;external;
{$L x64/libmsvcrt_defs00323.o}
procedure _heapused;external;
{$L x64/libmsvcrt_defs00324.o}
procedure hypot;external;
{$L x64/libmsvcrt_defs00325.o}
procedure heapwalk;external;
{$L x64/libmsvcrt_defs00326.o}
procedure _hypotf;external;
{$L x64/libmsvcrt_defs00327.o}
procedure _i64toa;external;
{$L x64/libmsvcrt_defs00328.o}
procedure _i64toa_s;external;
{$L x64/libmsvcrt_defs00329.o}
procedure _i64tow;external;
{$L x64/libmsvcrt_defs00330.o}
procedure _i64tow_s;external;
{$L x64/libmsvcrt_defs00331.o}
procedure _initterm;external;
{$L x64/libmsvcrt_defs00332.o}
procedure _hypot;external;
{$L x64/libmsvcrt_defs00333.o}
procedure _heapwalk;external;
{$L x64/libmsvcrt_defs00334.o}
procedure _isalnum_l;external;
{$L x64/libmsvcrt_defs00336.o}
procedure _isalpha_l;external;
{$L x64/libmsvcrt_defs00337.o}
procedure isatty;external;
{$L x64/libmsvcrt_defs00338.o}
procedure _isatty;external;
{$L x64/libmsvcrt_defs00339.o}
procedure _iscntrl_l;external;
{$L x64/libmsvcrt_defs00340.o}
procedure _isctype;external;
{$L x64/libmsvcrt_defs00341.o}
procedure _isctype_l;external;
{$L x64/libmsvcrt_defs00342.o}
procedure _isdigit_l;external;
{$L x64/libmsvcrt_defs00343.o}
procedure _isgraph_l;external;
{$L x64/libmsvcrt_defs00344.o}
procedure _isleadbyte_l;external;
{$L x64/libmsvcrt_defs00345.o}
procedure _islower_l;external;
{$L x64/libmsvcrt_defs00346.o}
procedure _ismbbalnum;external;
{$L x64/libmsvcrt_defs00347.o}
procedure _ismbbalnum_l;external;
{$L x64/libmsvcrt_defs00348.o}
procedure _ismbbalpha;external;
{$L x64/libmsvcrt_defs00349.o}
procedure _ismbbalpha_l;external;
{$L x64/libmsvcrt_defs00350.o}
procedure _ismbbgraph;external;
{$L x64/libmsvcrt_defs00351.o}
procedure _ismbbgraph_l;external;
{$L x64/libmsvcrt_defs00352.o}
procedure _ismbbkalnum;external;
{$L x64/libmsvcrt_defs00353.o}
procedure _ismbbkalnum_l;external;
{$L x64/libmsvcrt_defs00354.o}
procedure _ismbbkana;external;
{$L x64/libmsvcrt_defs00355.o}
procedure _ismbbkana_l;external;
{$L x64/libmsvcrt_defs00356.o}
procedure _ismbbkprint;external;
{$L x64/libmsvcrt_defs00357.o}
procedure _ismbbkprint_l;external;
{$L x64/libmsvcrt_defs00358.o}
procedure _ismbbkpunct;external;
{$L x64/libmsvcrt_defs00359.o}
procedure _ismbbkpunct_l;external;
{$L x64/libmsvcrt_defs00360.o}
procedure _ismbblead;external;
{$L x64/libmsvcrt_defs00361.o}
procedure _ismbblead_l;external;
{$L x64/libmsvcrt_defs00362.o}
procedure _ismbbprint;external;
{$L x64/libmsvcrt_defs00363.o}
procedure _ismbbprint_l;external;
{$L x64/libmsvcrt_defs00364.o}
procedure _ismbbpunct;external;
{$L x64/libmsvcrt_defs00365.o}
procedure _ismbbpunct_l;external;
{$L x64/libmsvcrt_defs00366.o}
procedure _ismbbtrail;external;
{$L x64/libmsvcrt_defs00367.o}
procedure _ismbbtrail_l;external;
{$L x64/libmsvcrt_defs00368.o}
procedure _ismbcalnum;external;
{$L x64/libmsvcrt_defs00369.o}
procedure _ismbcalnum_l;external;
{$L x64/libmsvcrt_defs00370.o}
procedure _ismbcalpha;external;
{$L x64/libmsvcrt_defs00371.o}
procedure _ismbcalpha_l;external;
{$L x64/libmsvcrt_defs00372.o}
procedure _ismbcdigit;external;
{$L x64/libmsvcrt_defs00373.o}
procedure _ismbcdigit_l;external;
{$L x64/libmsvcrt_defs00374.o}
procedure _ismbcgraph;external;
{$L x64/libmsvcrt_defs00375.o}
procedure _ismbcgraph_l;external;
{$L x64/libmsvcrt_defs00376.o}
procedure _ismbchira;external;
{$L x64/libmsvcrt_defs00377.o}
procedure _ismbchira_l;external;
{$L x64/libmsvcrt_defs00378.o}
procedure _ismbckata;external;
{$L x64/libmsvcrt_defs00379.o}
procedure _ismbckata_l;external;
{$L x64/libmsvcrt_defs00380.o}
procedure _ismbcl0;external;
{$L x64/libmsvcrt_defs00381.o}
procedure _ismbcl0_l;external;
{$L x64/libmsvcrt_defs00382.o}
procedure _ismbcl1;external;
{$L x64/libmsvcrt_defs00383.o}
procedure _ismbcl1_l;external;
{$L x64/libmsvcrt_defs00384.o}
procedure _ismbcl2;external;
{$L x64/libmsvcrt_defs00385.o}
procedure _ismbcl2_l;external;
{$L x64/libmsvcrt_defs00386.o}
procedure _ismbclegal;external;
{$L x64/libmsvcrt_defs00387.o}
procedure _ismbclegal_l;external;
{$L x64/libmsvcrt_defs00388.o}
procedure _ismbclower;external;
{$L x64/libmsvcrt_defs00389.o}
procedure _ismbclower_l;external;
{$L x64/libmsvcrt_defs00390.o}
procedure _ismbcprint;external;
{$L x64/libmsvcrt_defs00391.o}
procedure _ismbcprint_l;external;
{$L x64/libmsvcrt_defs00392.o}
procedure _ismbcpunct;external;
{$L x64/libmsvcrt_defs00393.o}
procedure _ismbcpunct_l;external;
{$L x64/libmsvcrt_defs00394.o}
procedure _ismbcspace;external;
{$L x64/libmsvcrt_defs00395.o}
procedure _ismbcspace_l;external;
{$L x64/libmsvcrt_defs00396.o}
procedure _ismbcsymbol;external;
{$L x64/libmsvcrt_defs00397.o}
procedure _ismbcsymbol_l;external;
{$L x64/libmsvcrt_defs00398.o}
procedure _ismbcupper;external;
{$L x64/libmsvcrt_defs00399.o}
procedure _ismbcupper_l;external;
{$L x64/libmsvcrt_defs00400.o}
procedure _ismbslead;external;
{$L x64/libmsvcrt_defs00401.o}
procedure _ismbslead_l;external;
{$L x64/libmsvcrt_defs00402.o}
procedure _ismbstrail;external;
{$L x64/libmsvcrt_defs00403.o}
procedure _ismbstrail_l;external;
{$L x64/libmsvcrt_defs00404.o}
procedure _isnan;external;
{$L x64/libmsvcrt_defs00405.o}
procedure _isnanf;external;
{$L x64/libmsvcrt_defs00406.o}
procedure _isprint_l;external;
{$L x64/libmsvcrt_defs00407.o}
procedure _isspace_l;external;
{$L x64/libmsvcrt_defs00408.o}
procedure _isupper_l;external;
{$L x64/libmsvcrt_defs00409.o}
procedure _iswalnum_l;external;
{$L x64/libmsvcrt_defs00410.o}
procedure _iswalpha_l;external;
{$L x64/libmsvcrt_defs00411.o}
procedure _iswcntrl_l;external;
{$L x64/libmsvcrt_defs00412.o}
procedure _iswctype_l;external;
{$L x64/libmsvcrt_defs00413.o}
procedure _iswdigit_l;external;
{$L x64/libmsvcrt_defs00414.o}
procedure _iswgraph_l;external;
{$L x64/libmsvcrt_defs00415.o}
procedure _iswlower_l;external;
{$L x64/libmsvcrt_defs00416.o}
procedure _iswprint_l;external;
{$L x64/libmsvcrt_defs00417.o}
procedure _iswpunct_l;external;
{$L x64/libmsvcrt_defs00418.o}
procedure _iswspace_l;external;
{$L x64/libmsvcrt_defs00419.o}
procedure _iswupper_l;external;
{$L x64/libmsvcrt_defs00420.o}
procedure _iswxdigit_l;external;
{$L x64/libmsvcrt_defs00421.o}
procedure _isxdigit_l;external;
{$L x64/libmsvcrt_defs00422.o}
procedure _itoa;external;
{$L x64/libmsvcrt_defs00423.o}
procedure _itoa_s;external;
{$L x64/libmsvcrt_defs00424.o}
procedure _itow;external;
{$L x64/libmsvcrt_defs00425.o}
procedure _itow_s;external;
{$L x64/libmsvcrt_defs00426.o}
procedure j0;external;
{$L x64/libmsvcrt_defs00427.o}
procedure j1;external;
{$L x64/libmsvcrt_defs00428.o}
procedure _jn;external;
{$L x64/libmsvcrt_defs00429.o}
procedure jn;external;
{$L x64/libmsvcrt_defs00430.o}
procedure kbhit;external;
{$L x64/libmsvcrt_defs00431.o}
procedure _kbhit;external;
{$L x64/libmsvcrt_defs00432.o}
procedure _j1;external;
{$L x64/libmsvcrt_defs00433.o}
procedure _j0;external;
{$L x64/libmsvcrt_defs00434.o}
procedure lfind;external;
{$L x64/libmsvcrt_defs00435.o}
procedure _lfind;external;
{$L x64/libmsvcrt_defs00436.o}
procedure _lfind_s;external;
{$L x64/libmsvcrt_defs00437.o}
procedure _loaddll;external;
{$L x64/libmsvcrt_defs00438.o}
procedure _local_unwind;external;
{$L x64/libmsvcrt_defs00439.o}
procedure _localtime32;external;
{$L x64/libmsvcrt_defs00440.o}
procedure _localtime64;external;
{$L x64/libmsvcrt_defs00441.o}
procedure _lock;external;
{$L x64/libmsvcrt_defs00442.o}
procedure _locking;external;
{$L x64/libmsvcrt_defs00443.o}
procedure _logb;external;
{$L x64/libmsvcrt_defs00444.o}
procedure _logbf;external;
{$L x64/libmsvcrt_defs00445.o}
procedure _lrotl;external;
{$L x64/libmsvcrt_defs00446.o}
procedure _lrotr;external;
{$L x64/libmsvcrt_defs00447.o}
procedure lsearch;external;
{$L x64/libmsvcrt_defs00448.o}
procedure _lsearch;external;
{$L x64/libmsvcrt_defs00449.o}
procedure _lsearch_s;external;
{$L x64/libmsvcrt_defs00450.o}
procedure _lseek;external;
{$L x64/libmsvcrt_defs00451.o}
procedure lseek;external;
{$L x64/libmsvcrt_defs00452.o}
procedure itoa;external;
{$L x64/libmsvcrt_defs00453.o}
procedure _lseeki64;external;
{$L x64/libmsvcrt_defs00454.o}
procedure ltoa;external;
{$L x64/libmsvcrt_defs00455.o}
procedure _ltoa;external;
{$L x64/libmsvcrt_defs00456.o}
procedure _ltoa_s;external;
{$L x64/libmsvcrt_defs00457.o}
procedure _ltow;external;
{$L x64/libmsvcrt_defs00458.o}
procedure _ltow_s;external;
{$L x64/libmsvcrt_defs00459.o}
procedure _makepath;external;
{$L x64/libmsvcrt_defs00460.o}
procedure _makepath_s;external;
{$L x64/libmsvcrt_defs00461.o}
procedure _mbbtombc;external;
{$L x64/libmsvcrt_defs00462.o}
procedure _mbbtombc_l;external;
{$L x64/libmsvcrt_defs00463.o}
procedure _mbbtype;external;
{$L x64/libmsvcrt_defs00464.o}
procedure _mbccpy;external;
{$L x64/libmsvcrt_defs00466.o}
procedure _mbccpy_l;external;
{$L x64/libmsvcrt_defs00467.o}
procedure _mbccpy_s;external;
{$L x64/libmsvcrt_defs00468.o}
procedure _mbccpy_s_l;external;
{$L x64/libmsvcrt_defs00469.o}
procedure _mbcjistojms;external;
{$L x64/libmsvcrt_defs00470.o}
procedure _mbcjistojms_l;external;
{$L x64/libmsvcrt_defs00471.o}
procedure _mbcjmstojis;external;
{$L x64/libmsvcrt_defs00472.o}
procedure _mbcjmstojis_l;external;
{$L x64/libmsvcrt_defs00473.o}
procedure _mbclen;external;
{$L x64/libmsvcrt_defs00474.o}
procedure _mbclen_l;external;
{$L x64/libmsvcrt_defs00475.o}
procedure _mbctohira;external;
{$L x64/libmsvcrt_defs00476.o}
procedure _mbctohira_l;external;
{$L x64/libmsvcrt_defs00477.o}
procedure _mbctokata;external;
{$L x64/libmsvcrt_defs00478.o}
procedure _mbctokata_l;external;
{$L x64/libmsvcrt_defs00479.o}
procedure _mbctolower;external;
{$L x64/libmsvcrt_defs00480.o}
procedure _mbctolower_l;external;
{$L x64/libmsvcrt_defs00481.o}
procedure _mbctombb;external;
{$L x64/libmsvcrt_defs00482.o}
procedure _mbctombb_l;external;
{$L x64/libmsvcrt_defs00483.o}
procedure _mbctoupper;external;
{$L x64/libmsvcrt_defs00484.o}
procedure _mbctoupper_l;external;
{$L x64/libmsvcrt_defs00485.o}
procedure _mblen_l;external;
{$L x64/libmsvcrt_defs00487.o}
procedure _mbsbtype;external;
{$L x64/libmsvcrt_defs00488.o}
procedure _mbsbtype_l;external;
{$L x64/libmsvcrt_defs00489.o}
procedure _mbscat;external;
{$L x64/libmsvcrt_defs00490.o}
procedure _mbscat_s;external;
{$L x64/libmsvcrt_defs00491.o}
procedure _mbscat_s_l;external;
{$L x64/libmsvcrt_defs00492.o}
procedure _mbschr;external;
{$L x64/libmsvcrt_defs00493.o}
procedure _mbschr_l;external;
{$L x64/libmsvcrt_defs00494.o}
procedure _mbscmp;external;
{$L x64/libmsvcrt_defs00495.o}
procedure _mbscmp_l;external;
{$L x64/libmsvcrt_defs00496.o}
procedure _mbscoll;external;
{$L x64/libmsvcrt_defs00497.o}
procedure _mbscoll_l;external;
{$L x64/libmsvcrt_defs00498.o}
procedure _mbscpy;external;
{$L x64/libmsvcrt_defs00499.o}
procedure _mbscpy_s;external;
{$L x64/libmsvcrt_defs00500.o}
procedure _mbscpy_s_l;external;
{$L x64/libmsvcrt_defs00501.o}
procedure _mbscspn;external;
{$L x64/libmsvcrt_defs00502.o}
procedure _mbscspn_l;external;
{$L x64/libmsvcrt_defs00503.o}
procedure _mbsdec;external;
{$L x64/libmsvcrt_defs00504.o}
procedure _mbsdec_l;external;
{$L x64/libmsvcrt_defs00505.o}
procedure _mbsdup;external;
{$L x64/libmsvcrt_defs00506.o}
procedure _mbsicmp;external;
{$L x64/libmsvcrt_defs00507.o}
procedure _mbsicmp_l;external;
{$L x64/libmsvcrt_defs00508.o}
procedure _mbsicoll;external;
{$L x64/libmsvcrt_defs00509.o}
procedure _mbsicoll_l;external;
{$L x64/libmsvcrt_defs00510.o}
procedure _mbsinc;external;
{$L x64/libmsvcrt_defs00511.o}
procedure _mbsinc_l;external;
{$L x64/libmsvcrt_defs00512.o}
procedure _mbslen;external;
{$L x64/libmsvcrt_defs00513.o}
procedure _mbslen_l;external;
{$L x64/libmsvcrt_defs00514.o}
procedure _mbslwr;external;
{$L x64/libmsvcrt_defs00515.o}
procedure _mbslwr_l;external;
{$L x64/libmsvcrt_defs00516.o}
procedure _mbslwr_s;external;
{$L x64/libmsvcrt_defs00517.o}
procedure _mbslwr_s_l;external;
{$L x64/libmsvcrt_defs00518.o}
procedure _mbsnbcat;external;
{$L x64/libmsvcrt_defs00519.o}
procedure _mbsnbcat_l;external;
{$L x64/libmsvcrt_defs00520.o}
procedure _mbsnbcat_s;external;
{$L x64/libmsvcrt_defs00521.o}
procedure _mbsnbcat_s_l;external;
{$L x64/libmsvcrt_defs00522.o}
procedure _mbsnbcmp;external;
{$L x64/libmsvcrt_defs00523.o}
procedure _mbsnbcmp_l;external;
{$L x64/libmsvcrt_defs00524.o}
procedure _mbsnbcnt;external;
{$L x64/libmsvcrt_defs00525.o}
procedure _mbsnbcnt_l;external;
{$L x64/libmsvcrt_defs00526.o}
procedure _mbsnbcoll;external;
{$L x64/libmsvcrt_defs00527.o}
procedure _mbsnbcoll_l;external;
{$L x64/libmsvcrt_defs00528.o}
procedure _mbsnbcpy;external;
{$L x64/libmsvcrt_defs00529.o}
procedure _mbsnbcpy_l;external;
{$L x64/libmsvcrt_defs00530.o}
procedure _mbsnbcpy_s;external;
{$L x64/libmsvcrt_defs00531.o}
procedure _mbsnbcpy_s_l;external;
{$L x64/libmsvcrt_defs00532.o}
procedure _mbsnbicmp;external;
{$L x64/libmsvcrt_defs00533.o}
procedure _mbsnbicmp_l;external;
{$L x64/libmsvcrt_defs00534.o}
procedure _mbsnbicoll;external;
{$L x64/libmsvcrt_defs00535.o}
procedure _mbsnbicoll_l;external;
{$L x64/libmsvcrt_defs00536.o}
procedure _mbsnbset;external;
{$L x64/libmsvcrt_defs00537.o}
procedure _mbsnbset_l;external;
{$L x64/libmsvcrt_defs00538.o}
procedure _mbsnbset_s;external;
{$L x64/libmsvcrt_defs00539.o}
procedure _mbsnbset_s_l;external;
{$L x64/libmsvcrt_defs00540.o}
procedure _mbsncat;external;
{$L x64/libmsvcrt_defs00541.o}
procedure _mbsncat_l;external;
{$L x64/libmsvcrt_defs00542.o}
procedure _mbsncat_s;external;
{$L x64/libmsvcrt_defs00543.o}
procedure _mbsncat_s_l;external;
{$L x64/libmsvcrt_defs00544.o}
procedure _mbsnccnt;external;
{$L x64/libmsvcrt_defs00545.o}
procedure _mbsnccnt_l;external;
{$L x64/libmsvcrt_defs00546.o}
procedure _mbsncmp;external;
{$L x64/libmsvcrt_defs00547.o}
procedure _mbsncmp_l;external;
{$L x64/libmsvcrt_defs00548.o}
procedure _mbsncoll;external;
{$L x64/libmsvcrt_defs00549.o}
procedure _mbsncoll_l;external;
{$L x64/libmsvcrt_defs00550.o}
procedure _mbsncpy;external;
{$L x64/libmsvcrt_defs00551.o}
procedure _mbsncpy_l;external;
{$L x64/libmsvcrt_defs00552.o}
procedure _mbsncpy_s;external;
{$L x64/libmsvcrt_defs00553.o}
procedure _mbsncpy_s_l;external;
{$L x64/libmsvcrt_defs00554.o}
procedure _mbsnextc;external;
{$L x64/libmsvcrt_defs00555.o}
procedure _mbsnextc_l;external;
{$L x64/libmsvcrt_defs00556.o}
procedure _mbsnicmp;external;
{$L x64/libmsvcrt_defs00557.o}
procedure _mbsnicmp_l;external;
{$L x64/libmsvcrt_defs00558.o}
procedure _mbsnicoll;external;
{$L x64/libmsvcrt_defs00559.o}
procedure _mbsnicoll_l;external;
{$L x64/libmsvcrt_defs00560.o}
procedure _mbsninc;external;
{$L x64/libmsvcrt_defs00561.o}
procedure _mbsninc_l;external;
{$L x64/libmsvcrt_defs00562.o}
procedure _mbsnlen;external;
{$L x64/libmsvcrt_defs00563.o}
procedure _mbsnlen_l;external;
{$L x64/libmsvcrt_defs00564.o}
procedure _mbsnset;external;
{$L x64/libmsvcrt_defs00565.o}
procedure _mbsnset_l;external;
{$L x64/libmsvcrt_defs00566.o}
procedure _mbsnset_s;external;
{$L x64/libmsvcrt_defs00567.o}
procedure _mbsnset_s_l;external;
{$L x64/libmsvcrt_defs00568.o}
procedure _mbspbrk;external;
{$L x64/libmsvcrt_defs00569.o}
procedure _mbspbrk_l;external;
{$L x64/libmsvcrt_defs00570.o}
procedure _mbsrchr;external;
{$L x64/libmsvcrt_defs00571.o}
procedure _mbsrchr_l;external;
{$L x64/libmsvcrt_defs00572.o}
procedure _mbsrev;external;
{$L x64/libmsvcrt_defs00573.o}
procedure _mbsrev_l;external;
{$L x64/libmsvcrt_defs00574.o}
procedure _mbsset;external;
{$L x64/libmsvcrt_defs00575.o}
procedure _mbsset_l;external;
{$L x64/libmsvcrt_defs00576.o}
procedure _mbsset_s;external;
{$L x64/libmsvcrt_defs00577.o}
procedure _mbsset_s_l;external;
{$L x64/libmsvcrt_defs00578.o}
procedure _mbsspn;external;
{$L x64/libmsvcrt_defs00579.o}
procedure _mbsspn_l;external;
{$L x64/libmsvcrt_defs00580.o}
procedure _mbsspnp;external;
{$L x64/libmsvcrt_defs00581.o}
procedure _mbsspnp_l;external;
{$L x64/libmsvcrt_defs00582.o}
procedure _mbsstr;external;
{$L x64/libmsvcrt_defs00583.o}
procedure _mbsstr_l;external;
{$L x64/libmsvcrt_defs00584.o}
procedure _mbstok;external;
{$L x64/libmsvcrt_defs00585.o}
procedure _mbstok_l;external;
{$L x64/libmsvcrt_defs00586.o}
procedure _mbstok_s;external;
{$L x64/libmsvcrt_defs00587.o}
procedure _mbstok_s_l;external;
{$L x64/libmsvcrt_defs00588.o}
procedure _mbstowcs_l;external;
{$L x64/libmsvcrt_defs00589.o}
procedure _mbstowcs_s_l;external;
{$L x64/libmsvcrt_defs00590.o}
procedure _mbstrlen;external;
{$L x64/libmsvcrt_defs00591.o}
procedure _mbstrlen_l;external;
{$L x64/libmsvcrt_defs00592.o}
procedure _mbstrnlen;external;
{$L x64/libmsvcrt_defs00593.o}
procedure _mbstrnlen_l;external;
{$L x64/libmsvcrt_defs00594.o}
procedure _mbsupr;external;
{$L x64/libmsvcrt_defs00595.o}
procedure _mbsupr_l;external;
{$L x64/libmsvcrt_defs00596.o}
procedure _mbsupr_s;external;
{$L x64/libmsvcrt_defs00597.o}
procedure _mbsupr_s_l;external;
{$L x64/libmsvcrt_defs00598.o}
procedure _mbtowc_l;external;
{$L x64/libmsvcrt_defs00599.o}
procedure memccpy;external;
{$L x64/libmsvcrt_defs00600.o}
procedure _memccpy;external;
{$L x64/libmsvcrt_defs00601.o}
procedure _memicmp;external;
{$L x64/libmsvcrt_defs00602.o}
procedure _memicmp_l;external;
{$L x64/libmsvcrt_defs00603.o}
procedure _mkdir;external;
{$L x64/libmsvcrt_defs00604.o}
procedure _mkgmtime;external;
{$L x64/libmsvcrt_defs00605.o}
procedure _mkgmtime32;external;
{$L x64/libmsvcrt_defs00606.o}
procedure _mkgmtime64;external;
{$L x64/libmsvcrt_defs00607.o}
procedure mktemp;external;
{$L x64/libmsvcrt_defs00608.o}
procedure _mktemp;external;
{$L x64/libmsvcrt_defs00609.o}
procedure _mktime64;external;
{$L x64/libmsvcrt_defs00610.o}
procedure _msize;external;
{$L x64/libmsvcrt_defs00611.o}
procedure _nextafter;external;
{$L x64/libmsvcrt_defs00612.o}
procedure _nextafterf;external;
{$L x64/libmsvcrt_defs00613.o}
procedure _onexit;external;
{$L x64/libmsvcrt_defs00614.o}
procedure nextafter;external;
{$L x64/libmsvcrt_defs00615.o}
procedure mkdir;external;
{$L x64/libmsvcrt_defs00616.o}
procedure open;external;
{$L x64/libmsvcrt_defs00617.o}
procedure _open;external;
{$L x64/libmsvcrt_defs00618.o}
procedure _open_osfhandle;external;
{$L x64/libmsvcrt_defs00619.o}
procedure _pclose;external;
{$L x64/libmsvcrt_defs00622.o}
procedure pclose;external;
{$L x64/libmsvcrt_defs00623.o}
procedure _pipe;external;
{$L x64/libmsvcrt_defs00626.o}
procedure _popen;external;
{$L x64/libmsvcrt_defs00627.o}
procedure popen;external;
{$L x64/libmsvcrt_defs00628.o}
procedure _printf_l;external;
{$L x64/libmsvcrt_defs00629.o}
procedure _printf_p;external;
{$L x64/libmsvcrt_defs00630.o}
procedure _printf_p_l;external;
{$L x64/libmsvcrt_defs00631.o}
procedure _printf_s_l;external;
{$L x64/libmsvcrt_defs00632.o}
procedure _purecall;external;
{$L x64/libmsvcrt_defs00633.o}
procedure _putch;external;
{$L x64/libmsvcrt_defs00634.o}
procedure _putenv;external;
{$L x64/libmsvcrt_defs00635.o}
procedure _putenv_s;external;
{$L x64/libmsvcrt_defs00636.o}
procedure putenv;external;
{$L x64/libmsvcrt_defs00637.o}
procedure putw;external;
{$L x64/libmsvcrt_defs00638.o}
procedure _putw;external;
{$L x64/libmsvcrt_defs00639.o}
procedure _putwch;external;
{$L x64/libmsvcrt_defs00640.o}
procedure putch;external;
{$L x64/libmsvcrt_defs00641.o}
procedure _putws;external;
{$L x64/libmsvcrt_defs00642.o}
procedure _read;external;
{$L x64/libmsvcrt_defs00644.o}
procedure read;external;
{$L x64/libmsvcrt_defs00645.o}
procedure _resetstkoflw;external;
{$L x64/libmsvcrt_defs00646.o}
procedure _rmdir;external;
{$L x64/libmsvcrt_defs00647.o}
procedure _rmtmp;external;
{$L x64/libmsvcrt_defs00648.o}
procedure _rotl;external;
{$L x64/libmsvcrt_defs00649.o}
procedure _rotl64;external;
{$L x64/libmsvcrt_defs00650.o}
procedure _rotr;external;
{$L x64/libmsvcrt_defs00651.o}
procedure _rotr64;external;
{$L x64/libmsvcrt_defs00652.o}
procedure _scalb;external;
{$L x64/libmsvcrt_defs00653.o}
procedure _scalbf;external;
{$L x64/libmsvcrt_defs00654.o}
procedure rmtmp;external;
{$L x64/libmsvcrt_defs00655.o}
procedure rmdir;external;
{$L x64/libmsvcrt_defs00656.o}
procedure _scanf_l;external;
{$L x64/libmsvcrt_defs00657.o}
procedure _scanf_s_l;external;
{$L x64/libmsvcrt_defs00658.o}
procedure _scprintf;external;
{$L x64/libmsvcrt_defs00659.o}
procedure memicmp;external;
{$L x64/libmsvcrt_defs00660.o}
procedure _scprintf_l;external;
{$L x64/libmsvcrt_defs00661.o}
procedure _scprintf_p_l;external;
{$L x64/libmsvcrt_defs00662.o}
procedure _scwprintf;external;
{$L x64/libmsvcrt_defs00663.o}
procedure _scwprintf_l;external;
{$L x64/libmsvcrt_defs00664.o}
procedure _scwprintf_p_l;external;
{$L x64/libmsvcrt_defs00665.o}
procedure searchenv;external;
{$L x64/libmsvcrt_defs00666.o}
procedure _searchenv;external;
{$L x64/libmsvcrt_defs00667.o}
procedure _searchenv_s;external;
{$L x64/libmsvcrt_defs00668.o}
procedure _set_error_mode;external;
{$L x64/libmsvcrt_defs00669.o}
procedure _set_sbh_threshold;external;
{$L x64/libmsvcrt_defs00670.o}
procedure _seterrormode;external;
{$L x64/libmsvcrt_defs00671.o}
procedure _setjmp;external;
{$L x64/libmsvcrt_defs00672.o}
procedure _setjmpex;external;
{$L x64/libmsvcrt_defs00673.o}
procedure _setmaxstdio;external;
{$L x64/libmsvcrt_defs00674.o}
procedure _setmbcp;external;
{$L x64/libmsvcrt_defs00675.o}
procedure setmode;external;
{$L x64/libmsvcrt_defs00676.o}
procedure _setmode;external;
{$L x64/libmsvcrt_defs00677.o}
procedure _setsystime;external;
{$L x64/libmsvcrt_defs00678.o}
procedure _sleep;external;
{$L x64/libmsvcrt_defs00679.o}
procedure _snprintf;external;
{$L x64/libmsvcrt_defs00680.o}
procedure _snprintf_c;external;
{$L x64/libmsvcrt_defs00681.o}
procedure _snprintf_c_l;external;
{$L x64/libmsvcrt_defs00682.o}
procedure _snprintf_l;external;
{$L x64/libmsvcrt_defs00683.o}
procedure _snprintf_s;external;
{$L x64/libmsvcrt_defs00684.o}
procedure _snprintf_s_l;external;
{$L x64/libmsvcrt_defs00685.o}
procedure _snscanf;external;
{$L x64/libmsvcrt_defs00686.o}
procedure _snscanf_l;external;
{$L x64/libmsvcrt_defs00687.o}
procedure _snscanf_s;external;
{$L x64/libmsvcrt_defs00688.o}
procedure _snscanf_s_l;external;
{$L x64/libmsvcrt_defs00689.o}
procedure snwprintf;external;
{$L x64/libmsvcrt_defs00690.o}
procedure _snwprintf;external;
{$L x64/libmsvcrt_defs00691.o}
procedure _snwprintf_l;external;
{$L x64/libmsvcrt_defs00692.o}
procedure _snwprintf_s;external;
{$L x64/libmsvcrt_defs00693.o}
procedure _snwprintf_s_l;external;
{$L x64/libmsvcrt_defs00694.o}
procedure _snwscanf;external;
{$L x64/libmsvcrt_defs00695.o}
procedure _snwscanf_l;external;
{$L x64/libmsvcrt_defs00696.o}
procedure _snwscanf_s;external;
{$L x64/libmsvcrt_defs00697.o}
procedure _snwscanf_s_l;external;
{$L x64/libmsvcrt_defs00698.o}
procedure sopen;external;
{$L x64/libmsvcrt_defs00699.o}
procedure _sopen;external;
{$L x64/libmsvcrt_defs00700.o}
procedure _spawnl;external;
{$L x64/libmsvcrt_defs00701.o}
procedure spawnl;external;
{$L x64/libmsvcrt_defs00702.o}
procedure _spawnle;external;
{$L x64/libmsvcrt_defs00703.o}
procedure spawnlp;external;
{$L x64/libmsvcrt_defs00704.o}
procedure _spawnlp;external;
{$L x64/libmsvcrt_defs00705.o}
procedure _spawnlpe;external;
{$L x64/libmsvcrt_defs00706.o}
procedure _spawnv;external;
{$L x64/libmsvcrt_defs00707.o}
procedure spawnve;external;
{$L x64/libmsvcrt_defs00708.o}
procedure _spawnve;external;
{$L x64/libmsvcrt_defs00709.o}
procedure spawnle;external;
{$L x64/libmsvcrt_defs00710.o}
procedure spawnv;external;
{$L x64/libmsvcrt_defs00711.o}
procedure _spawnvp;external;
{$L x64/libmsvcrt_defs00712.o}
procedure spawnvpe;external;
{$L x64/libmsvcrt_defs00713.o}
procedure _spawnvpe;external;
{$L x64/libmsvcrt_defs00714.o}
procedure _splitpath;external;
{$L x64/libmsvcrt_defs00715.o}
procedure _splitpath_s;external;
{$L x64/libmsvcrt_defs00716.o}
procedure _sprintf_l;external;
{$L x64/libmsvcrt_defs00717.o}
procedure _sprintf_p_l;external;
{$L x64/libmsvcrt_defs00718.o}
procedure _sprintf_s_l;external;
{$L x64/libmsvcrt_defs00719.o}
procedure _sscanf_l;external;
{$L x64/libmsvcrt_defs00720.o}
procedure _sscanf_s_l;external;
{$L x64/libmsvcrt_defs00721.o}
procedure _stat64i32;external;
{$L x64/libmsvcrt_defs00722.o}
procedure _stat;external;
{$L x64/libmsvcrt_defs00723.o}
procedure _stat64;external;
{$L x64/libmsvcrt_defs00724.o}
procedure _stati64;external;
{$L x64/libmsvcrt_defs00725.o}
procedure _statusfp;external;
{$L x64/libmsvcrt_defs00726.o}
procedure strcmpi;external;
{$L x64/libmsvcrt_defs00727.o}
procedure _strcmpi;external;
{$L x64/libmsvcrt_defs00728.o}
procedure _strcoll_l;external;
{$L x64/libmsvcrt_defs00729.o}
procedure _strdate;external;
{$L x64/libmsvcrt_defs00730.o}
procedure _strerror;external;
{$L x64/libmsvcrt_defs00731.o}
procedure _strerror_s;external;
{$L x64/libmsvcrt_defs00732.o}
procedure _stricmp;external;
{$L x64/libmsvcrt_defs00733.o}
procedure strcasecmp;external;
{$L x64/libmsvcrt_defs00734.o}
procedure strdup;external;
{$L x64/libmsvcrt_defs00735.o}
procedure _strdup;external;
{$L x64/libmsvcrt_defs00736.o}
procedure _stricmp_l;external;
{$L x64/libmsvcrt_defs00737.o}
procedure stricoll;external;
{$L x64/libmsvcrt_defs00738.o}
procedure _stricoll;external;
{$L x64/libmsvcrt_defs00739.o}
procedure stricmp;external;
{$L x64/libmsvcrt_defs00740.o}
procedure _stricoll_l;external;
{$L x64/libmsvcrt_defs00741.o}
procedure strlwr;external;
{$L x64/libmsvcrt_defs00742.o}
procedure _strlwr;external;
{$L x64/libmsvcrt_defs00743.o}
procedure _strlwr_l;external;
{$L x64/libmsvcrt_defs00744.o}
procedure _strlwr_s;external;
{$L x64/libmsvcrt_defs00745.o}
procedure spawnvp;external;
{$L x64/libmsvcrt_defs00747.o}
procedure spawnlpe;external;
{$L x64/libmsvcrt_defs00748.o}
procedure _strlwr_s_l;external;
{$L x64/libmsvcrt_defs00749.o}
procedure _strncoll;external;
{$L x64/libmsvcrt_defs00750.o}
procedure _strncoll_l;external;
{$L x64/libmsvcrt_defs00751.o}
procedure _strnicmp;external;
{$L x64/libmsvcrt_defs00752.o}
procedure _strnicmp_l;external;
{$L x64/libmsvcrt_defs00753.o}
procedure _strnicoll;external;
{$L x64/libmsvcrt_defs00754.o}
procedure _strnicoll_l;external;
{$L x64/libmsvcrt_defs00755.o}
procedure strnicmp;external;
{$L x64/libmsvcrt_defs00756.o}
procedure _strnset;external;
{$L x64/libmsvcrt_defs00757.o}
procedure strnset;external;
{$L x64/libmsvcrt_defs00758.o}
procedure _strnset_s;external;
{$L x64/libmsvcrt_defs00759.o}
procedure _strrev;external;
{$L x64/libmsvcrt_defs00760.o}
procedure _strset;external;
{$L x64/libmsvcrt_defs00761.o}
procedure strrev;external;
{$L x64/libmsvcrt_defs00762.o}
procedure strset;external;
{$L x64/libmsvcrt_defs00763.o}
procedure _strset_s;external;
{$L x64/libmsvcrt_defs00764.o}
procedure _strtime;external;
{$L x64/libmsvcrt_defs00765.o}
procedure _strtod_l;external;
{$L x64/libmsvcrt_defs00766.o}
procedure _strtoi64;external;
{$L x64/libmsvcrt_defs00767.o}
procedure _strtoi64_l;external;
{$L x64/libmsvcrt_defs00768.o}
procedure _strtol_l;external;
{$L x64/libmsvcrt_defs00769.o}
procedure _strtoui64;external;
{$L x64/libmsvcrt_defs00770.o}
procedure _strtoui64_l;external;
{$L x64/libmsvcrt_defs00771.o}
procedure strncasecmp;external;
{$L x64/libmsvcrt_defs00772.o}
procedure _strtoul_l;external;
{$L x64/libmsvcrt_defs00773.o}
procedure _strupr;external;
{$L x64/libmsvcrt_defs00774.o}
procedure _strupr_l;external;
{$L x64/libmsvcrt_defs00775.o}
procedure _strupr_s;external;
{$L x64/libmsvcrt_defs00776.o}
procedure _strupr_s_l;external;
{$L x64/libmsvcrt_defs00777.o}
procedure _strxfrm_l;external;
{$L x64/libmsvcrt_defs00778.o}
procedure _swab;external;
{$L x64/libmsvcrt_defs00779.o}
procedure _swprintf_c;external;
{$L x64/libmsvcrt_defs00780.o}
procedure swab;external;
{$L x64/libmsvcrt_defs00781.o}
procedure strupr;external;
{$L x64/libmsvcrt_defs00782.o}
procedure _swprintf_c_l;external;
{$L x64/libmsvcrt_defs00783.o}
procedure _swprintf_p_l;external;
{$L x64/libmsvcrt_defs00784.o}
procedure _swprintf_s_l;external;
{$L x64/libmsvcrt_defs00785.o}
procedure _swscanf_l;external;
{$L x64/libmsvcrt_defs00786.o}
procedure _swscanf_s_l;external;
{$L x64/libmsvcrt_defs00787.o}
procedure tell;external;
{$L x64/libmsvcrt_defs00790.o}
procedure _tell;external;
{$L x64/libmsvcrt_defs00791.o}
procedure _telli64;external;
{$L x64/libmsvcrt_defs00792.o}
procedure tempnam;external;
{$L x64/libmsvcrt_defs00793.o}
procedure _tempnam;external;
{$L x64/libmsvcrt_defs00794.o}
procedure _time64;external;
{$L x64/libmsvcrt_defs00795.o}
procedure timezone;external;
{$L x64/libmsvcrt_defs00797.o}
procedure _tolower;external;
{$L x64/libmsvcrt_defs00798.o}
procedure _tolower_l;external;
{$L x64/libmsvcrt_defs00799.o}
procedure _toupper;external;
{$L x64/libmsvcrt_defs00800.o}
procedure _toupper_l;external;
{$L x64/libmsvcrt_defs00801.o}
procedure _towlower_l;external;
{$L x64/libmsvcrt_defs00802.o}
procedure _towupper_l;external;
{$L x64/libmsvcrt_defs00803.o}
procedure tzname;external;
{$L x64/libmsvcrt_defs00804.o}
procedure tzset;external;
{$L x64/libmsvcrt_defs00806.o}
procedure _tzset;external;
{$L x64/libmsvcrt_defs00807.o}
procedure _ui64toa;external;
{$L x64/libmsvcrt_defs00808.o}
procedure _ui64toa_s;external;
{$L x64/libmsvcrt_defs00809.o}
procedure _ui64tow;external;
{$L x64/libmsvcrt_defs00810.o}
procedure _ui64tow_s;external;
{$L x64/libmsvcrt_defs00811.o}
procedure _ultoa;external;
{$L x64/libmsvcrt_defs00812.o}
procedure _ultoa_s;external;
{$L x64/libmsvcrt_defs00813.o}
procedure _ultow;external;
{$L x64/libmsvcrt_defs00814.o}
procedure _ultow_s;external;
{$L x64/libmsvcrt_defs00815.o}
procedure _umask;external;
{$L x64/libmsvcrt_defs00816.o}
procedure _ungetch;external;
{$L x64/libmsvcrt_defs00817.o}
procedure _ungetwch;external;
{$L x64/libmsvcrt_defs00818.o}
procedure ungetch;external;
{$L x64/libmsvcrt_defs00819.o}
procedure _unlink;external;
{$L x64/libmsvcrt_defs00820.o}
procedure _unloaddll;external;
{$L x64/libmsvcrt_defs00821.o}
procedure unlink;external;
{$L x64/libmsvcrt_defs00822.o}
procedure _unlock;external;
{$L x64/libmsvcrt_defs00823.o}
procedure utime;external;
{$L x64/libmsvcrt_defs00824.o}
procedure _utime;external;
{$L x64/libmsvcrt_defs00825.o}
procedure _utime32;external;
{$L x64/libmsvcrt_defs00826.o}
procedure _utime64;external;
{$L x64/libmsvcrt_defs00827.o}
procedure _vcprintf;external;
{$L x64/libmsvcrt_defs00828.o}
procedure umask;external;
{$L x64/libmsvcrt_defs00829.o}
procedure _vcprintf_l;external;
{$L x64/libmsvcrt_defs00830.o}
procedure _vcprintf_p;external;
{$L x64/libmsvcrt_defs00831.o}
procedure _vcprintf_p_l;external;
{$L x64/libmsvcrt_defs00832.o}
procedure _vcwprintf;external;
{$L x64/libmsvcrt_defs00833.o}
procedure _vcwprintf_l;external;
{$L x64/libmsvcrt_defs00834.o}
procedure _vcwprintf_p;external;
{$L x64/libmsvcrt_defs00835.o}
procedure _vcwprintf_p_l;external;
{$L x64/libmsvcrt_defs00836.o}
procedure _vfprintf_l;external;
{$L x64/libmsvcrt_defs00837.o}
procedure _vfprintf_p;external;
{$L x64/libmsvcrt_defs00838.o}
procedure _vfprintf_p_l;external;
{$L x64/libmsvcrt_defs00839.o}
procedure _vfprintf_s_l;external;
{$L x64/libmsvcrt_defs00840.o}
procedure _vfwprintf_l;external;
{$L x64/libmsvcrt_defs00841.o}
procedure _vfwprintf_p;external;
{$L x64/libmsvcrt_defs00842.o}
procedure _vfwprintf_p_l;external;
{$L x64/libmsvcrt_defs00843.o}
procedure _vfwprintf_s_l;external;
{$L x64/libmsvcrt_defs00844.o}
procedure _vprintf_l;external;
{$L x64/libmsvcrt_defs00845.o}
procedure _vprintf_p;external;
{$L x64/libmsvcrt_defs00846.o}
procedure _vprintf_p_l;external;
{$L x64/libmsvcrt_defs00847.o}
procedure _vprintf_s_l;external;
{$L x64/libmsvcrt_defs00848.o}
procedure _vscprintf;external;
{$L x64/libmsvcrt_defs00849.o}
procedure _vscprintf_l;external;
{$L x64/libmsvcrt_defs00850.o}
procedure _vscprintf_p_l;external;
{$L x64/libmsvcrt_defs00851.o}
procedure _vscwprintf;external;
{$L x64/libmsvcrt_defs00852.o}
procedure _vscwprintf_l;external;
{$L x64/libmsvcrt_defs00853.o}
procedure _vscwprintf_p_l;external;
{$L x64/libmsvcrt_defs00854.o}
procedure _vsnprintf;external;
{$L x64/libmsvcrt_defs00855.o}
procedure _vsnprintf_c;external;
{$L x64/libmsvcrt_defs00856.o}
procedure _vsnprintf_c_l;external;
{$L x64/libmsvcrt_defs00857.o}
procedure _vsnprintf_l;external;
{$L x64/libmsvcrt_defs00858.o}
procedure _vsnprintf_s;external;
{$L x64/libmsvcrt_defs00859.o}
procedure _vsnprintf_s_l;external;
{$L x64/libmsvcrt_defs00860.o}
procedure _vsnwprintf_l;external;
{$L x64/libmsvcrt_defs00861.o}
procedure _vsnwprintf_s;external;
{$L x64/libmsvcrt_defs00862.o}
procedure _vsnwprintf_s_l;external;
{$L x64/libmsvcrt_defs00863.o}
procedure vsnwprintf;external;
{$L x64/libmsvcrt_defs00864.o}
procedure _vsnwprintf;external;
{$L x64/libmsvcrt_defs00865.o}
procedure vsnprintf_s;external;
{$L x64/libmsvcrt_defs00866.o}
procedure _vsprintf_l;external;
{$L x64/libmsvcrt_defs00867.o}
procedure _vsprintf_p;external;
{$L x64/libmsvcrt_defs00868.o}
procedure _vsprintf_p_l;external;
{$L x64/libmsvcrt_defs00869.o}
procedure _vsprintf_s_l;external;
{$L x64/libmsvcrt_defs00870.o}
procedure _vswprintf;external;
{$L x64/libmsvcrt_defs00871.o}
procedure _vswprintf_c;external;
{$L x64/libmsvcrt_defs00872.o}
procedure _vswprintf_c_l;external;
{$L x64/libmsvcrt_defs00873.o}
procedure _vswprintf_l;external;
{$L x64/libmsvcrt_defs00874.o}
procedure _vswprintf_p_l;external;
{$L x64/libmsvcrt_defs00875.o}
procedure _vswprintf_s_l;external;
{$L x64/libmsvcrt_defs00876.o}
procedure _vwprintf_l;external;
{$L x64/libmsvcrt_defs00877.o}
procedure _vwprintf_p;external;
{$L x64/libmsvcrt_defs00878.o}
procedure _vwprintf_p_l;external;
{$L x64/libmsvcrt_defs00879.o}
procedure _vwprintf_s_l;external;
{$L x64/libmsvcrt_defs00880.o}
procedure _waccess;external;
{$L x64/libmsvcrt_defs00881.o}
procedure _wasctime;external;
{$L x64/libmsvcrt_defs00882.o}
procedure _wassert;external;
{$L x64/libmsvcrt_defs00883.o}
procedure _wchdir;external;
{$L x64/libmsvcrt_defs00884.o}
procedure _wchmod;external;
{$L x64/libmsvcrt_defs00885.o}
procedure _wcreat;external;
{$L x64/libmsvcrt_defs00887.o}
procedure _wcscoll_l;external;
{$L x64/libmsvcrt_defs00888.o}
procedure wcsdup;external;
{$L x64/libmsvcrt_defs00889.o}
procedure _wcsdup;external;
{$L x64/libmsvcrt_defs00890.o}
procedure _wcserror;external;
{$L x64/libmsvcrt_defs00891.o}
procedure _wcserror_s;external;
{$L x64/libmsvcrt_defs00892.o}
procedure _wcsftime_l;external;
{$L x64/libmsvcrt_defs00893.o}
procedure _wcsicmp;external;
{$L x64/libmsvcrt_defs00894.o}
procedure wcsicmp;external;
{$L x64/libmsvcrt_defs00895.o}
procedure wcscmpi;external;
{$L x64/libmsvcrt_defs00896.o}
procedure _wcsicmp_l;external;
{$L x64/libmsvcrt_defs00897.o}
procedure wcsicoll;external;
{$L x64/libmsvcrt_defs00898.o}
procedure _wcsicoll;external;
{$L x64/libmsvcrt_defs00899.o}
procedure _wcsicoll_l;external;
{$L x64/libmsvcrt_defs00900.o}
procedure _wcslwr;external;
{$L x64/libmsvcrt_defs00901.o}
procedure _wcslwr_l;external;
{$L x64/libmsvcrt_defs00902.o}
procedure _wcslwr_s;external;
{$L x64/libmsvcrt_defs00903.o}
procedure _wcslwr_s_l;external;
{$L x64/libmsvcrt_defs00904.o}
procedure wcslwr;external;
{$L x64/libmsvcrt_defs00905.o}
procedure _wcsncoll;external;
{$L x64/libmsvcrt_defs00907.o}
procedure _wcsncoll_l;external;
{$L x64/libmsvcrt_defs00908.o}
procedure wcsnicmp;external;
{$L x64/libmsvcrt_defs00909.o}
procedure _wcsnicmp;external;
{$L x64/libmsvcrt_defs00910.o}
procedure _wcsnicmp_l;external;
{$L x64/libmsvcrt_defs00911.o}
procedure _wcsnicoll;external;
{$L x64/libmsvcrt_defs00912.o}
procedure _wcsnicoll_l;external;
{$L x64/libmsvcrt_defs00913.o}
procedure _wcsnset;external;
{$L x64/libmsvcrt_defs00914.o}
procedure wcsnset;external;
{$L x64/libmsvcrt_defs00915.o}
procedure _wcsnset_s;external;
{$L x64/libmsvcrt_defs00916.o}
procedure _wcsrev;external;
{$L x64/libmsvcrt_defs00917.o}
procedure wcsset;external;
{$L x64/libmsvcrt_defs00918.o}
procedure _wcsset;external;
{$L x64/libmsvcrt_defs00919.o}
procedure _wcsset_s;external;
{$L x64/libmsvcrt_defs00920.o}
procedure _wcstoi64;external;
{$L x64/libmsvcrt_defs00921.o}
procedure _wcstoi64_l;external;
{$L x64/libmsvcrt_defs00922.o}
procedure _wcstol_l;external;
{$L x64/libmsvcrt_defs00923.o}
procedure wcsrev;external;
{$L x64/libmsvcrt_defs00924.o}
procedure _wcstombs_l;external;
{$L x64/libmsvcrt_defs00925.o}
procedure _wcstombs_s_l;external;
{$L x64/libmsvcrt_defs00926.o}
procedure _wcstoui64;external;
{$L x64/libmsvcrt_defs00927.o}
procedure _wcstoui64_l;external;
{$L x64/libmsvcrt_defs00928.o}
procedure _wcstoul_l;external;
{$L x64/libmsvcrt_defs00929.o}
procedure wcsupr;external;
{$L x64/libmsvcrt_defs00930.o}
procedure _wcsupr;external;
{$L x64/libmsvcrt_defs00931.o}
procedure _wcsupr_l;external;
{$L x64/libmsvcrt_defs00932.o}
procedure _wcsupr_s;external;
{$L x64/libmsvcrt_defs00933.o}
procedure _wcsupr_s_l;external;
{$L x64/libmsvcrt_defs00934.o}
procedure _wcsxfrm_l;external;
{$L x64/libmsvcrt_defs00935.o}
procedure _wctime;external;
{$L x64/libmsvcrt_defs00936.o}
procedure _wctime64;external;
{$L x64/libmsvcrt_defs00937.o}
procedure _wctomb_l;external;
{$L x64/libmsvcrt_defs00938.o}
procedure _wctomb_s_l;external;
{$L x64/libmsvcrt_defs00939.o}
procedure _wctype;external;
{$L x64/libmsvcrt_defs00940.o}
procedure _wexecl;external;
{$L x64/libmsvcrt_defs00942.o}
procedure _wexecle;external;
{$L x64/libmsvcrt_defs00943.o}
procedure _wexeclp;external;
{$L x64/libmsvcrt_defs00944.o}
procedure _wexeclpe;external;
{$L x64/libmsvcrt_defs00945.o}
procedure _wexecv;external;
{$L x64/libmsvcrt_defs00946.o}
procedure _wexecve;external;
{$L x64/libmsvcrt_defs00947.o}
procedure _wexecvp;external;
{$L x64/libmsvcrt_defs00948.o}
procedure _wexecvpe;external;
{$L x64/libmsvcrt_defs00949.o}
procedure _wfdopen;external;
{$L x64/libmsvcrt_defs00950.o}
procedure _wfindfirst64i32;external;
{$L x64/libmsvcrt_defs00951.o}
procedure _wfindfirst;external;
{$L x64/libmsvcrt_defs00952.o}
procedure _wfindfirst64;external;
{$L x64/libmsvcrt_defs00953.o}
procedure _wfindfirsti64;external;
{$L x64/libmsvcrt_defs00954.o}
procedure _wfindnext;external;
{$L x64/libmsvcrt_defs00955.o}
procedure _wfindnext64i32;external;
{$L x64/libmsvcrt_defs00956.o}
procedure _wfindnext64;external;
{$L x64/libmsvcrt_defs00957.o}
procedure _wfindnexti64;external;
{$L x64/libmsvcrt_defs00958.o}
procedure _wfopen;external;
{$L x64/libmsvcrt_defs00959.o}
procedure _wfopen_s;external;
{$L x64/libmsvcrt_defs00960.o}
procedure _wfreopen;external;
{$L x64/libmsvcrt_defs00961.o}
procedure _wfreopen_s;external;
{$L x64/libmsvcrt_defs00962.o}
procedure _wfsopen;external;
{$L x64/libmsvcrt_defs00963.o}
procedure _wfullpath;external;
{$L x64/libmsvcrt_defs00964.o}
procedure _wgetcwd;external;
{$L x64/libmsvcrt_defs00965.o}
procedure _wgetdcwd;external;
{$L x64/libmsvcrt_defs00966.o}
procedure _wgetenv;external;
{$L x64/libmsvcrt_defs00967.o}
procedure _wgetenv_s;external;
{$L x64/libmsvcrt_defs00968.o}
procedure _winput_s;external;
{$L x64/libmsvcrt_defs00971.o}
procedure _wmakepath;external;
{$L x64/libmsvcrt_defs00973.o}
procedure _wmakepath_s;external;
{$L x64/libmsvcrt_defs00974.o}
procedure _wmkdir;external;
{$L x64/libmsvcrt_defs00975.o}
procedure _wmktemp;external;
{$L x64/libmsvcrt_defs00976.o}
procedure _wopen;external;
{$L x64/libmsvcrt_defs00977.o}
procedure _woutput_s;external;
{$L x64/libmsvcrt_defs00978.o}
procedure _wperror;external;
{$L x64/libmsvcrt_defs00979.o}
procedure wpopen;external;
{$L x64/libmsvcrt_defs00981.o}
procedure _wpopen;external;
{$L x64/libmsvcrt_defs00982.o}
procedure _wprintf_l;external;
{$L x64/libmsvcrt_defs00983.o}
procedure _wprintf_p;external;
{$L x64/libmsvcrt_defs00984.o}
procedure time;external;
{$L x64/libmsvcrt_defs00985.o}
procedure _wprintf_p_l;external;
{$L x64/libmsvcrt_defs00986.o}
procedure _wprintf_s_l;external;
{$L x64/libmsvcrt_defs00987.o}
procedure _wputenv;external;
{$L x64/libmsvcrt_defs00988.o}
procedure _wputenv_s;external;
{$L x64/libmsvcrt_defs00989.o}
procedure _wremove;external;
{$L x64/libmsvcrt_defs00990.o}
procedure _wrename;external;
{$L x64/libmsvcrt_defs00991.o}
procedure _write;external;
{$L x64/libmsvcrt_defs00992.o}
procedure _wrmdir;external;
{$L x64/libmsvcrt_defs00993.o}
procedure _wscanf_l;external;
{$L x64/libmsvcrt_defs00994.o}
procedure _wscanf_s_l;external;
{$L x64/libmsvcrt_defs00995.o}
procedure _wsearchenv;external;
{$L x64/libmsvcrt_defs00996.o}
procedure _wsearchenv_s;external;
{$L x64/libmsvcrt_defs00997.o}
procedure _wsetlocale;external;
{$L x64/libmsvcrt_defs00998.o}
procedure _wsopen;external;
{$L x64/libmsvcrt_defs00999.o}
procedure _wsopen_s;external;
{$L x64/libmsvcrt_defs01000.o}
procedure _wspawnl;external;
{$L x64/libmsvcrt_defs01001.o}
procedure _wspawnle;external;
{$L x64/libmsvcrt_defs01002.o}
procedure _wspawnlp;external;
{$L x64/libmsvcrt_defs01003.o}
procedure _wspawnlpe;external;
{$L x64/libmsvcrt_defs01004.o}
procedure _wspawnv;external;
{$L x64/libmsvcrt_defs01005.o}
procedure _wspawnve;external;
{$L x64/libmsvcrt_defs01006.o}
procedure _wspawnvp;external;
{$L x64/libmsvcrt_defs01007.o}
procedure _wspawnvpe;external;
{$L x64/libmsvcrt_defs01008.o}
procedure _wsplitpath;external;
{$L x64/libmsvcrt_defs01009.o}
procedure _wsplitpath_s;external;
{$L x64/libmsvcrt_defs01010.o}
procedure _wstat64i32;external;
{$L x64/libmsvcrt_defs01011.o}
procedure _wstat;external;
{$L x64/libmsvcrt_defs01012.o}
procedure _wstat64;external;
{$L x64/libmsvcrt_defs01013.o}
procedure _wstati64;external;
{$L x64/libmsvcrt_defs01014.o}
procedure _wstrdate;external;
{$L x64/libmsvcrt_defs01015.o}
procedure _wstrtime;external;
{$L x64/libmsvcrt_defs01016.o}
procedure _wsystem;external;
{$L x64/libmsvcrt_defs01017.o}
procedure _wtempnam;external;
{$L x64/libmsvcrt_defs01018.o}
procedure _wtmpnam;external;
{$L x64/libmsvcrt_defs01019.o}
procedure _wtmpnam_s;external;
{$L x64/libmsvcrt_defs01020.o}
procedure _wtof;external;
{$L x64/libmsvcrt_defs01021.o}
procedure _wtof_l;external;
{$L x64/libmsvcrt_defs01022.o}
procedure _wtoi;external;
{$L x64/libmsvcrt_defs01023.o}
procedure _wtoi64;external;
{$L x64/libmsvcrt_defs01024.o}
procedure _wtoi64_l;external;
{$L x64/libmsvcrt_defs01025.o}
procedure _wtoi_l;external;
{$L x64/libmsvcrt_defs01026.o}
procedure _wtol;external;
{$L x64/libmsvcrt_defs01027.o}
procedure _wtol_l;external;
{$L x64/libmsvcrt_defs01028.o}
procedure _wunlink;external;
{$L x64/libmsvcrt_defs01029.o}
procedure _wutime;external;
{$L x64/libmsvcrt_defs01030.o}
procedure _wutime32;external;
{$L x64/libmsvcrt_defs01031.o}
procedure _wutime64;external;
{$L x64/libmsvcrt_defs01032.o}
procedure _y0;external;
{$L x64/libmsvcrt_defs01033.o}
procedure y0;external;
{$L x64/libmsvcrt_defs01034.o}
procedure y1;external;
{$L x64/libmsvcrt_defs01035.o}
procedure _y1;external;
{$L x64/libmsvcrt_defs01036.o}
procedure _yn;external;
{$L x64/libmsvcrt_defs01037.o}
procedure abort;external;
{$L x64/libmsvcrt_defs01038.o}
procedure abs;external;
{$L x64/libmsvcrt_defs01039.o}
procedure acos;external;
{$L x64/libmsvcrt_defs01040.o}
procedure yn;external;
{$L x64/libmsvcrt_defs01041.o}
procedure asctime;external;
{$L x64/libmsvcrt_defs01043.o}
procedure asin;external;
{$L x64/libmsvcrt_defs01044.o}
procedure atan;external;
{$L x64/libmsvcrt_defs01046.o}
procedure atof;external;
{$L x64/libmsvcrt_defs01051.o}
procedure atoi;external;
{$L x64/libmsvcrt_defs01052.o}
procedure atol;external;
{$L x64/libmsvcrt_defs01053.o}
procedure bsearch;external;
{$L x64/libmsvcrt_defs01054.o}
procedure bsearch_s;external;
{$L x64/libmsvcrt_defs01055.o}
procedure calloc;external;
{$L x64/libmsvcrt_defs01056.o}
procedure clearerr;external;
{$L x64/libmsvcrt_defs01059.o}
procedure clearerr_s;external;
{$L x64/libmsvcrt_defs01060.o}
procedure clock;external;
{$L x64/libmsvcrt_defs01061.o}
procedure cosh;external;
{$L x64/libmsvcrt_defs01064.o}
procedure ctime;external;
{$L x64/libmsvcrt_defs01066.o}
procedure difftime;external;
{$L x64/libmsvcrt_defs01067.o}
procedure div;external;
{$L x64/libmsvcrt_defs01068.o}
procedure exit;external;
{$L x64/libmsvcrt_defs01069.o}
procedure __ms_fwscanf;external;
{$L x64/libmsvcrt_defs01072.o}
procedure fclose;external;
{$L x64/libmsvcrt_defs01074.o}
procedure feof;external;
{$L x64/libmsvcrt_defs01075.o}
procedure ferror;external;
{$L x64/libmsvcrt_defs01076.o}
procedure fflush;external;
{$L x64/libmsvcrt_defs01077.o}
procedure fgetc;external;
{$L x64/libmsvcrt_defs01078.o}
procedure fgetpos;external;
{$L x64/libmsvcrt_defs01079.o}
procedure fgets;external;
{$L x64/libmsvcrt_defs01080.o}
procedure fgetwc;external;
{$L x64/libmsvcrt_defs01081.o}
procedure fgetws;external;
{$L x64/libmsvcrt_defs01082.o}
procedure fopen;external;
{$L x64/libmsvcrt_defs01087.o}
procedure fopen_s;external;
{$L x64/libmsvcrt_defs01088.o}
procedure __ms_fprintf;external;
{$L x64/libmsvcrt_defs01089.o}
procedure fprintf;external;
{$L x64/libmsvcrt_defs01090.o}
procedure fprintf_s;external;
{$L x64/libmsvcrt_defs01091.o}
procedure fputc;external;
{$L x64/libmsvcrt_defs01092.o}
procedure fputs;external;
{$L x64/libmsvcrt_defs01093.o}
procedure fputwc;external;
{$L x64/libmsvcrt_defs01094.o}
procedure fputws;external;
{$L x64/libmsvcrt_defs01095.o}
procedure fread;external;
{$L x64/libmsvcrt_defs01096.o}
procedure free;external;
{$L x64/libmsvcrt_defs01097.o}
procedure freopen;external;
{$L x64/libmsvcrt_defs01098.o}
procedure freopen_s;external;
{$L x64/libmsvcrt_defs01099.o}
procedure __ms_fscanf;external;
{$L x64/libmsvcrt_defs01101.o}
procedure fscanf;external;
{$L x64/libmsvcrt_defs01102.o}
procedure fscanf_s;external;
{$L x64/libmsvcrt_defs01103.o}
procedure __ms_fwprintf;external;
{$L x64/libmsvcrt_defs01104.o}
procedure fseek;external;
{$L x64/libmsvcrt_defs01105.o}
procedure fsetpos;external;
{$L x64/libmsvcrt_defs01106.o}
procedure ftell;external;
{$L x64/libmsvcrt_defs01107.o}
procedure fwprintf;external;
{$L x64/libmsvcrt_defs01108.o}
procedure fwprintf_s;external;
{$L x64/libmsvcrt_defs01109.o}
procedure fwrite;external;
{$L x64/libmsvcrt_defs01110.o}
procedure fwscanf;external;
{$L x64/libmsvcrt_defs01111.o}
procedure fwscanf_s;external;
{$L x64/libmsvcrt_defs01112.o}
procedure getc;external;
{$L x64/libmsvcrt_defs01113.o}
procedure getchar;external;
{$L x64/libmsvcrt_defs01114.o}
procedure getenv;external;
{$L x64/libmsvcrt_defs01115.o}
procedure getenv_s;external;
{$L x64/libmsvcrt_defs01116.o}
procedure gets;external;
{$L x64/libmsvcrt_defs01117.o}
procedure getwc;external;
{$L x64/libmsvcrt_defs01118.o}
procedure getwchar;external;
{$L x64/libmsvcrt_defs01119.o}
procedure gmtime;external;
{$L x64/libmsvcrt_defs01120.o}
procedure is_wctype;external;
{$L x64/libmsvcrt_defs01121.o}
procedure isalnum;external;
{$L x64/libmsvcrt_defs01122.o}
procedure isalpha;external;
{$L x64/libmsvcrt_defs01123.o}
procedure iscntrl;external;
{$L x64/libmsvcrt_defs01124.o}
procedure isdigit;external;
{$L x64/libmsvcrt_defs01125.o}
procedure isgraph;external;
{$L x64/libmsvcrt_defs01126.o}
procedure isleadbyte;external;
{$L x64/libmsvcrt_defs01127.o}
procedure islower;external;
{$L x64/libmsvcrt_defs01128.o}
procedure isprint;external;
{$L x64/libmsvcrt_defs01129.o}
procedure ispunct;external;
{$L x64/libmsvcrt_defs01130.o}
procedure isspace;external;
{$L x64/libmsvcrt_defs01131.o}
procedure isupper;external;
{$L x64/libmsvcrt_defs01132.o}
procedure iswalnum;external;
{$L x64/libmsvcrt_defs01133.o}
procedure iswalpha;external;
{$L x64/libmsvcrt_defs01134.o}
procedure iswascii;external;
{$L x64/libmsvcrt_defs01135.o}
procedure iswcntrl;external;
{$L x64/libmsvcrt_defs01136.o}
procedure iswctype;external;
{$L x64/libmsvcrt_defs01137.o}
procedure iswdigit;external;
{$L x64/libmsvcrt_defs01138.o}
procedure iswgraph;external;
{$L x64/libmsvcrt_defs01139.o}
procedure iswlower;external;
{$L x64/libmsvcrt_defs01140.o}
procedure iswprint;external;
{$L x64/libmsvcrt_defs01141.o}
procedure iswpunct;external;
{$L x64/libmsvcrt_defs01142.o}
procedure iswspace;external;
{$L x64/libmsvcrt_defs01143.o}
procedure iswupper;external;
{$L x64/libmsvcrt_defs01144.o}
procedure iswxdigit;external;
{$L x64/libmsvcrt_defs01145.o}
procedure isxdigit;external;
{$L x64/libmsvcrt_defs01146.o}
procedure labs;external;
{$L x64/libmsvcrt_defs01147.o}
procedure write;external;
{$L x64/libmsvcrt_defs01149.o}
procedure ldiv;external;
{$L x64/libmsvcrt_defs01150.o}
procedure localeconv;external;
{$L x64/libmsvcrt_defs01151.o}
procedure localtime;external;
{$L x64/libmsvcrt_defs01152.o}
procedure log10;external;
{$L x64/libmsvcrt_defs01154.o}
procedure longjmp;external;
{$L x64/libmsvcrt_defs01157.o}
procedure malloc;external;
{$L x64/libmsvcrt_defs01158.o}
procedure mblen;external;
{$L x64/libmsvcrt_defs01159.o}
procedure mbsrtowcs_s;external;
{$L x64/libmsvcrt_defs01160.o}
procedure mbstowcs;external;
{$L x64/libmsvcrt_defs01161.o}
procedure mbstowcs_s;external;
{$L x64/libmsvcrt_defs01162.o}
procedure mbtowc;external;
{$L x64/libmsvcrt_defs01163.o}
procedure memchr;external;
{$L x64/libmsvcrt_defs01164.o}
procedure memcmp;external;
{$L x64/libmsvcrt_defs01165.o}
procedure memcpy;external;
{$L x64/libmsvcrt_defs01166.o}
procedure memmove;external;
{$L x64/libmsvcrt_defs01167.o}
procedure memset;external;
{$L x64/libmsvcrt_defs01168.o}
procedure mktime;external;
{$L x64/libmsvcrt_defs01169.o}
procedure perror;external;
{$L x64/libmsvcrt_defs01172.o}
procedure __ms_printf;external;
{$L x64/libmsvcrt_defs01175.o}
procedure printf;external;
{$L x64/libmsvcrt_defs01176.o}
procedure printf_s;external;
{$L x64/libmsvcrt_defs01177.o}
procedure putc;external;
{$L x64/libmsvcrt_defs01178.o}
procedure putchar;external;
{$L x64/libmsvcrt_defs01179.o}
procedure puts;external;
{$L x64/libmsvcrt_defs01180.o}
procedure putwc;external;
{$L x64/libmsvcrt_defs01181.o}
procedure putwchar;external;
{$L x64/libmsvcrt_defs01182.o}
procedure qsort;external;
{$L x64/libmsvcrt_defs01183.o}
procedure qsort_s;external;
{$L x64/libmsvcrt_defs01184.o}
procedure raise;external;
{$L x64/libmsvcrt_defs01185.o}
procedure realloc;external;
{$L x64/libmsvcrt_defs01186.o}
procedure __ms_scanf;external;
{$L x64/libmsvcrt_defs01187.o}
procedure rand;external;
{$L x64/libmsvcrt_defs01188.o}
procedure remove;external;
{$L x64/libmsvcrt_defs01189.o}
procedure rename;external;
{$L x64/libmsvcrt_defs01190.o}
procedure rewind;external;
{$L x64/libmsvcrt_defs01191.o}
procedure scanf;external;
{$L x64/libmsvcrt_defs01192.o}
procedure scanf_s;external;
{$L x64/libmsvcrt_defs01193.o}
procedure setbuf;external;
{$L x64/libmsvcrt_defs01194.o}
procedure setjmp;external;
{$L x64/libmsvcrt_defs01195.o}
procedure setlocale;external;
{$L x64/libmsvcrt_defs01196.o}
procedure setvbuf;external;
{$L x64/libmsvcrt_defs01197.o}
procedure signal;external;
{$L x64/libmsvcrt_defs01198.o}
procedure sinh;external;
{$L x64/libmsvcrt_defs01201.o}
procedure __ms_sprintf;external;
{$L x64/libmsvcrt_defs01203.o}
procedure sprintf;external;
{$L x64/libmsvcrt_defs01204.o}
procedure srand;external;
{$L x64/libmsvcrt_defs01207.o}
procedure __ms_sscanf;external;
{$L x64/libmsvcrt_defs01208.o}
procedure sscanf;external;
{$L x64/libmsvcrt_defs01209.o}
procedure sscanf_s;external;
{$L x64/libmsvcrt_defs01210.o}
procedure strcat;external;
{$L x64/libmsvcrt_defs01211.o}
procedure strcat_s;external;
{$L x64/libmsvcrt_defs01212.o}
procedure strchr;external;
{$L x64/libmsvcrt_defs01213.o}
procedure strcmp;external;
{$L x64/libmsvcrt_defs01214.o}
procedure strcoll;external;
{$L x64/libmsvcrt_defs01215.o}
procedure strcpy;external;
{$L x64/libmsvcrt_defs01216.o}
procedure strcpy_s;external;
{$L x64/libmsvcrt_defs01217.o}
procedure strcspn;external;
{$L x64/libmsvcrt_defs01218.o}
procedure strerror;external;
{$L x64/libmsvcrt_defs01219.o}
procedure strftime;external;
{$L x64/libmsvcrt_defs01220.o}
procedure strlen;external;
{$L x64/libmsvcrt_defs01221.o}
procedure strncat;external;
{$L x64/libmsvcrt_defs01222.o}
procedure strncat_s;external;
{$L x64/libmsvcrt_defs01223.o}
procedure strncmp;external;
{$L x64/libmsvcrt_defs01224.o}
procedure strncpy;external;
{$L x64/libmsvcrt_defs01225.o}
procedure strncpy_s;external;
{$L x64/libmsvcrt_defs01226.o}
procedure strpbrk;external;
{$L x64/libmsvcrt_defs01227.o}
procedure strrchr;external;
{$L x64/libmsvcrt_defs01228.o}
procedure strspn;external;
{$L x64/libmsvcrt_defs01229.o}
procedure strstr;external;
{$L x64/libmsvcrt_defs01230.o}
procedure strtod;external;
{$L x64/libmsvcrt_defs01231.o}
procedure strtok;external;
{$L x64/libmsvcrt_defs01232.o}
procedure _swprintf;external;
{$L x64/libmsvcrt_defs01233.o}
procedure strtok_s;external;
{$L x64/libmsvcrt_defs01234.o}
procedure strtol;external;
{$L x64/libmsvcrt_defs01235.o}
procedure strtoul;external;
{$L x64/libmsvcrt_defs01236.o}
procedure __ms_swprintf;external;
{$L x64/libmsvcrt_defs01237.o}
procedure swprintf;external;
{$L x64/libmsvcrt_defs01238.o}
procedure swprintf_s;external;
{$L x64/libmsvcrt_defs01239.o}
procedure __ms_swscanf;external;
{$L x64/libmsvcrt_defs01240.o}
procedure strxfrm;external;
{$L x64/libmsvcrt_defs01241.o}
procedure swscanf;external;
{$L x64/libmsvcrt_defs01242.o}
procedure swscanf_s;external;
{$L x64/libmsvcrt_defs01243.o}
procedure system;external;
{$L x64/libmsvcrt_defs01244.o}
procedure tan;external;
{$L x64/libmsvcrt_defs01245.o}
procedure tanh;external;
{$L x64/libmsvcrt_defs01247.o}
procedure tmpfile;external;
{$L x64/libmsvcrt_defs01248.o}
procedure tmpfile_s;external;
{$L x64/libmsvcrt_defs01249.o}
procedure tmpnam;external;
{$L x64/libmsvcrt_defs01250.o}
procedure tmpnam_s;external;
{$L x64/libmsvcrt_defs01251.o}
procedure tolower;external;
{$L x64/libmsvcrt_defs01252.o}
procedure toupper;external;
{$L x64/libmsvcrt_defs01253.o}
procedure towlower;external;
{$L x64/libmsvcrt_defs01254.o}
procedure __ms_vprintf;external;
{$L x64/libmsvcrt_defs01255.o}
procedure __ms_vsprintf;external;
{$L x64/libmsvcrt_defs01256.o}
procedure towupper;external;
{$L x64/libmsvcrt_defs01257.o}
procedure ungetc;external;
{$L x64/libmsvcrt_defs01258.o}
procedure vfprintf;external;
{$L x64/libmsvcrt_defs01259.o}
procedure vfprintf_s;external;
{$L x64/libmsvcrt_defs01260.o}
procedure __ms_vfwprintf;external;
{$L x64/libmsvcrt_defs01261.o}
procedure __ms_vfprintf;external;
{$L x64/libmsvcrt_defs01262.o}
procedure ungetwc;external;
{$L x64/libmsvcrt_defs01263.o}
procedure vfwprintf;external;
{$L x64/libmsvcrt_defs01264.o}
procedure vfwprintf_s;external;
{$L x64/libmsvcrt_defs01265.o}
procedure vprintf;external;
{$L x64/libmsvcrt_defs01266.o}
procedure __ms_vswprintf;external;
{$L x64/libmsvcrt_defs01267.o}
procedure vprintf_s;external;
{$L x64/libmsvcrt_defs01268.o}
procedure vswprintf;external;
{$L x64/libmsvcrt_defs01269.o}
procedure __ms_vwprintf;external;
{$L x64/libmsvcrt_defs01270.o}
procedure vsprintf;external;
{$L x64/libmsvcrt_defs01271.o}
procedure vswprintf_s;external;
{$L x64/libmsvcrt_defs01272.o}
procedure vwprintf;external;
{$L x64/libmsvcrt_defs01273.o}
procedure vwprintf_s;external;
{$L x64/libmsvcrt_defs01274.o}
procedure wcrtomb_s;external;
{$L x64/libmsvcrt_defs01275.o}
procedure wcscat;external;
{$L x64/libmsvcrt_defs01276.o}
procedure wcscat_s;external;
{$L x64/libmsvcrt_defs01277.o}
procedure wcschr;external;
{$L x64/libmsvcrt_defs01278.o}
procedure wcscmp;external;
{$L x64/libmsvcrt_defs01279.o}
procedure wcscoll;external;
{$L x64/libmsvcrt_defs01280.o}
procedure wcscpy;external;
{$L x64/libmsvcrt_defs01281.o}
procedure wcscpy_s;external;
{$L x64/libmsvcrt_defs01282.o}
procedure wcscspn;external;
{$L x64/libmsvcrt_defs01283.o}
procedure wcsftime;external;
{$L x64/libmsvcrt_defs01284.o}
procedure wcslen;external;
{$L x64/libmsvcrt_defs01285.o}
procedure wcsncat;external;
{$L x64/libmsvcrt_defs01286.o}
procedure wcsncat_s;external;
{$L x64/libmsvcrt_defs01287.o}
procedure wcsncmp;external;
{$L x64/libmsvcrt_defs01288.o}
procedure wcsncpy;external;
{$L x64/libmsvcrt_defs01289.o}
procedure wcsncpy_s;external;
{$L x64/libmsvcrt_defs01290.o}
procedure wcspbrk;external;
{$L x64/libmsvcrt_defs01292.o}
procedure wcsrchr;external;
{$L x64/libmsvcrt_defs01293.o}
procedure wcsrtombs_s;external;
{$L x64/libmsvcrt_defs01294.o}
procedure wcsspn;external;
{$L x64/libmsvcrt_defs01295.o}
procedure wcsstr;external;
{$L x64/libmsvcrt_defs01296.o}
procedure wcstod;external;
{$L x64/libmsvcrt_defs01297.o}
procedure wcstok;external;
{$L x64/libmsvcrt_defs01298.o}
procedure wcstok_s;external;
{$L x64/libmsvcrt_defs01299.o}
procedure wcstol;external;
{$L x64/libmsvcrt_defs01300.o}
procedure wcstombs;external;
{$L x64/libmsvcrt_defs01301.o}
procedure wcstombs_s;external;
{$L x64/libmsvcrt_defs01302.o}
procedure wcstoul;external;
{$L x64/libmsvcrt_defs01303.o}
procedure wcsxfrm;external;
{$L x64/libmsvcrt_defs01304.o}
procedure __ms_wprintf;external;
{$L x64/libmsvcrt_defs01305.o}
procedure wctomb;external;
{$L x64/libmsvcrt_defs01306.o}
procedure wctomb_s;external;
{$L x64/libmsvcrt_defs01307.o}
procedure wprintf;external;
{$L x64/libmsvcrt_defs01308.o}
procedure wprintf_s;external;
{$L x64/libmsvcrt_defs01309.o}
procedure __ms_wscanf;external;
{$L x64/libmsvcrt_defs01310.o}
procedure wscanf;external;
{$L x64/libmsvcrt_defs01311.o}
procedure wscanf_s;external;
{$L x64/libmsvcrt_defs01312.o}
procedure iscsymf;external;
{$L x64/libmsvcrt_defs01313.o}
var
  __imp__CxxThrowException : UInt64;
  __imp__Getdays : UInt64;
  __imp__Getmonths : UInt64;
  __imp__Gettnames : UInt64;
  __imp__HUGE : UInt64;
  __imp__Strftime : UInt64;
  __imp__XcptFilter : UInt64;
  __imp___C_specific_handler : UInt64;
  __imp___CppXcptFilter : UInt64;
  __imp___CxxFrameHandler : UInt64;
  __imp___DestructExceptionObject : UInt64;
  __imp___RTCastToVoid : UInt64;
  __imp___RTDynamicCast : UInt64;
  __imp___RTtypeid : UInt64;
  __imp___STRINGTOLD : UInt64;
  __imp____lc_codepage_func : UInt64;
  __imp____lc_collate_cp_func : UInt64;
  __imp____lc_handle_func : UInt64;
  __imp____mb_cur_max_func : UInt64;
  __imp____setlc_active_func : UInt64;
  __imp____unguarded_readlc_active_add_func : UInt64;
  __imp___argc : UInt64;
  __imp___argv : UInt64;
  __imp___badioinfo : UInt64;
  __imp___crtCompareStringA : UInt64;
  __imp___crtCompareStringW : UInt64;
  __imp___crtGetLocaleInfoW : UInt64;
  __imp___crtGetStringTypeW : UInt64;
  __imp___crtLCMapStringA : UInt64;
  __imp___crtLCMapStringW : UInt64;
  __imp___dllonexit : UInt64;
  __imp___doserrno : UInt64;
  __imp___fpecode : UInt64;
  __imp___getmainargs : UInt64;
  __imp___initenv : UInt64;
  __imp___iob_func : UInt64;
  __imp___isascii : UInt64;
  __imp___iscsym : UInt64;
  __imp___iscsymf : UInt64;
  __imp___lc_codepage : UInt64;
  __imp_isascii : UInt64;
  __imp___lc_collate_cp : UInt64;
  __imp___lc_handle : UInt64;
  __imp___lconv_init : UInt64;
  __imp___mb_cur_max : UInt64;
  __imp___pctype_func : UInt64;
  __imp___pioinfo : UInt64;
  __imp___pwctype_func : UInt64;
  __imp___pxcptinfoptrs : UInt64;
  __imp___set_app_type : UInt64;
  __imp___setlc_active : UInt64;
  __imp___setusermatherr : UInt64;
  __imp___threadhandle : UInt64;
  __imp___threadid : UInt64;
  __imp___toascii : UInt64;
  __imp_toascii : UInt64;
  __imp___unDName : UInt64;
  __imp___unDNameEx : UInt64;
  __imp___uncaught_exception : UInt64;
  __imp___unguarded_readlc_active : UInt64;
  __imp___wargv : UInt64;
  __imp___wcserror : UInt64;
  __imp___wcserror_s : UInt64;
  __imp___wgetmainargs : UInt64;
  __imp___winitenv : UInt64;
  __imp__abs64 : UInt64;
  __imp_access : UInt64;
  __imp__access : UInt64;
  __imp__acmdln : UInt64;
  __imp__aexit_rtn : UInt64;
  __imp__aligned_free : UInt64;
  __imp__aligned_malloc : UInt64;
  __imp__aligned_offset_malloc : UInt64;
  __imp__aligned_offset_realloc : UInt64;
  __imp__aligned_realloc : UInt64;
  __imp__amsg_exit : UInt64;
  __imp__assert : UInt64;
  __imp__atodbl : UInt64;
  __imp__atodbl_l : UInt64;
  __imp__atof_l : UInt64;
  __imp__atoflt_l : UInt64;
  __imp__atoi64 : UInt64;
  __imp__atoi64_l : UInt64;
  __imp__atoi_l : UInt64;
  __imp__atol_l : UInt64;
  __imp__atoldbl : UInt64;
  __imp__atoldbl_l : UInt64;
  __imp__beep : UInt64;
  __imp__beginthread : UInt64;
  __imp__beginthreadex : UInt64;
  __imp__c_exit : UInt64;
  __imp__cabs : UInt64;
  __imp__callnewh : UInt64;
  __imp__cexit : UInt64;
  __imp__cgets : UInt64;
  __imp__cgetws : UInt64;
  __imp__chdir : UInt64;
  __imp__chdrive : UInt64;
  __imp__chgsign : UInt64;
  __imp__chgsignf : UInt64;
  __imp_chmod : UInt64;
  __imp__chmod : UInt64;
  __imp_chsize : UInt64;
  __imp__chsize : UInt64;
  __imp__clearfp : UInt64;
  __imp_chgsign : UInt64;
  __imp__close : UInt64;
  __imp_close : UInt64;
  __imp__commit : UInt64;
  __imp__commode : UInt64;
  __imp__control87 : UInt64;
  __imp__controlfp : UInt64;
  __imp__copysign : UInt64;
  __imp__copysignf : UInt64;
  __imp__cprintf : UInt64;
  __imp__cprintf_l : UInt64;
  __imp__cprintf_p : UInt64;
  __imp__cprintf_p_l : UInt64;
  __imp_chdir : UInt64;
  __imp__cputs : UInt64;
  __imp_iscsym : UInt64;
  __imp__cputws : UInt64;
  __imp__creat : UInt64;
  __imp_creat : UInt64;
  __imp__cscanf : UInt64;
  __imp__cscanf_l : UInt64;
  __imp__cscanf_s : UInt64;
  __imp__cscanf_s_l : UInt64;
  __imp__ctime64 : UInt64;
  __imp__ctype : UInt64;
  __imp__cwait : UInt64;
  __imp__cwprintf : UInt64;
  __imp_cwait : UInt64;
  __imp__cwprintf_l : UInt64;
  __imp__cwprintf_p : UInt64;
  __imp__cwprintf_p_l : UInt64;
  __imp__cwscanf : UInt64;
  __imp__cwscanf_l : UInt64;
  __imp__cwscanf_s : UInt64;
  __imp__cwscanf_s_l : UInt64;
  __imp__daylight : UInt64;
  __imp__difftime32 : UInt64;
  __imp__difftime64 : UInt64;
  __imp_daylight : UInt64;
  __imp__dstbias : UInt64;
  __imp_dup : UInt64;
  __imp__dup : UInt64;
  __imp_dup2 : UInt64;
  __imp__dup2 : UInt64;
  __imp__ecvt : UInt64;
  __imp_ecvt : UInt64;
  __imp__ecvt_s : UInt64;
  __imp__endthread : UInt64;
  __imp__endthreadex : UInt64;
  __imp__environ : UInt64;
  __imp__eof : UInt64;
  __imp__errno : UInt64;
  __imp__execl : UInt64;
  __imp__execle : UInt64;
  __imp_execlp : UInt64;
  __imp__execlp : UInt64;
  __imp_execle : UInt64;
  __imp__execlpe : UInt64;
  __imp_execlpe : UInt64;
  __imp_execve : UInt64;
  __imp_execv : UInt64;
  __imp__execve : UInt64;
  __imp__execv : UInt64;
  __imp_eof : UInt64;
  __imp_execl : UInt64;
  __imp_execvp : UInt64;
  __imp__execvp : UInt64;
  __imp__execvpe : UInt64;
  __imp__exit : UInt64;
  __imp__expand : UInt64;
  __imp__fcloseall : UInt64;
  __imp_execvpe : UInt64;
  __imp_fcvt : UInt64;
  __imp__fcvt : UInt64;
  __imp__fcvt_s : UInt64;
  __imp__fdopen : UInt64;
  __imp__fgetchar : UInt64;
  __imp__fgetwchar : UInt64;
  __imp_fgetwchar : UInt64;
  __imp_fgetchar : UInt64;
  __imp_fdopen : UInt64;
  __imp__filbuf : UInt64;
  __imp__fileinfo : UInt64;
  __imp_filelength : UInt64;
  __imp__filelength : UInt64;
  __imp__filelengthi64 : UInt64;
  __imp__fileno : UInt64;
  __imp__findclose : UInt64;
  __imp_fileno : UInt64;
  __imp__findfirst64i32 : UInt64;
  __imp__findfirst : UInt64;
  __imp__findfirst64 : UInt64;
  __imp__findfirsti64 : UInt64;
  __imp__findnext64i32 : UInt64;
  __imp__findnext : UInt64;
  __imp__findnext64 : UInt64;
  __imp__findnexti64 : UInt64;
  __imp_finite : UInt64;
  __imp__finite : UInt64;
  __imp__finitef : UInt64;
  __imp__flushall : UInt64;
  __imp__fmode : UInt64;
  __imp__flsbuf : UInt64;
  __imp_fpclass : UInt64;
  __imp__fpclass : UInt64;
  __imp__fpclassf : UInt64;
  __imp__fpreset : UInt64;
  __imp__fprintf_l : UInt64;
  __imp__fprintf_p : UInt64;
  __imp__fprintf_p_l : UInt64;
  __imp__fprintf_s_l : UInt64;
  __imp__fputchar : UInt64;
  __imp_fputwchar : UInt64;
  __imp__fputwchar : UInt64;
  __imp__fscanf_l : UInt64;
  __imp__fscanf_s_l : UInt64;
  __imp__fsopen : UInt64;
  __imp_fputchar : UInt64;
  __imp__fstat : UInt64;
  __imp__fstat64 : UInt64;
  __imp__fstati64 : UInt64;
  __imp__ftime : UInt64;
  __imp__ftime32 : UInt64;
  __imp__ftime32_s : UInt64;
  __imp__ftime64 : UInt64;
  __imp__ftime64_s : UInt64;
  __imp__fullpath : UInt64;
  __imp__fstat64i32 : UInt64;
  __imp__ftime_s : UInt64;
  __imp__futime : UInt64;
  __imp__futime32 : UInt64;
  __imp__futime64 : UInt64;
  __imp__fwprintf_l : UInt64;
  __imp__fwprintf_p : UInt64;
  __imp__fwprintf_p_l : UInt64;
  __imp__fwprintf_s_l : UInt64;
  __imp__fwscanf_l : UInt64;
  __imp__fwscanf_s_l : UInt64;
  __imp__gcvt : UInt64;
  __imp_gcvt : UInt64;
  __imp__gcvt_s : UInt64;
  __imp__get_heap_handle : UInt64;
  __imp__get_osfhandle : UInt64;
  __imp__get_sbh_threshold : UInt64;
  __imp__getch : UInt64;
  __imp_getche : UInt64;
  __imp__getche : UInt64;
  __imp__getcwd : UInt64;
  __imp__getdcwd : UInt64;
  __imp__getdiskfree : UInt64;
  __imp_getcwd : UInt64;
  __imp__getdllprocaddr : UInt64;
  __imp_getch : UInt64;
  __imp__getdrive : UInt64;
  __imp__getdrives : UInt64;
  __imp__getmaxstdio : UInt64;
  __imp__getmbcp : UInt64;
  __imp_getpid : UInt64;
  __imp__getpid : UInt64;
  __imp__getsystime : UInt64;
  __imp_getw : UInt64;
  __imp__getw : UInt64;
  __imp__getwch : UInt64;
  __imp__getwche : UInt64;
  __imp__getws : UInt64;
  __imp__gmtime32 : UInt64;
  __imp__gmtime64 : UInt64;
  __imp__heapadd : UInt64;
  __imp__heapchk : UInt64;
  __imp__heapmin : UInt64;
  __imp__heapset : UInt64;
  __imp__heapused : UInt64;
  __imp_hypot : UInt64;
  __imp_heapwalk : UInt64;
  __imp__hypotf : UInt64;
  __imp__i64toa : UInt64;
  __imp__i64toa_s : UInt64;
  __imp__i64tow : UInt64;
  __imp__i64tow_s : UInt64;
  __imp__initterm : UInt64;
  __imp__hypot : UInt64;
  __imp__heapwalk : UInt64;
  __imp__iob : UInt64;
  __imp__isalnum_l : UInt64;
  __imp__isalpha_l : UInt64;
  __imp_isatty : UInt64;
  __imp__isatty : UInt64;
  __imp__iscntrl_l : UInt64;
  __imp__isctype : UInt64;
  __imp__isctype_l : UInt64;
  __imp__isdigit_l : UInt64;
  __imp__isgraph_l : UInt64;
  __imp__isleadbyte_l : UInt64;
  __imp__islower_l : UInt64;
  __imp__ismbbalnum : UInt64;
  __imp__ismbbalnum_l : UInt64;
  __imp__ismbbalpha : UInt64;
  __imp__ismbbalpha_l : UInt64;
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
  __imp__isnan : UInt64;
  __imp__isnanf : UInt64;
  __imp__isprint_l : UInt64;
  __imp__isspace_l : UInt64;
  __imp__isupper_l : UInt64;
  __imp__iswalnum_l : UInt64;
  __imp__iswalpha_l : UInt64;
  __imp__iswcntrl_l : UInt64;
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
  __imp__itoa : UInt64;
  __imp__itoa_s : UInt64;
  __imp__itow : UInt64;
  __imp__itow_s : UInt64;
  __imp_j0 : UInt64;
  __imp_j1 : UInt64;
  __imp__jn : UInt64;
  __imp_jn : UInt64;
  __imp_kbhit : UInt64;
  __imp__kbhit : UInt64;
  __imp__j1 : UInt64;
  __imp__j0 : UInt64;
  __imp_lfind : UInt64;
  __imp__lfind : UInt64;
  __imp__lfind_s : UInt64;
  __imp__loaddll : UInt64;
  __imp__local_unwind : UInt64;
  __imp__localtime32 : UInt64;
  __imp__localtime64 : UInt64;
  __imp__lock : UInt64;
  __imp__locking : UInt64;
  __imp__logb : UInt64;
  __imp__logbf : UInt64;
  __imp__lrotl : UInt64;
  __imp__lrotr : UInt64;
  __imp_lsearch : UInt64;
  __imp__lsearch : UInt64;
  __imp__lsearch_s : UInt64;
  __imp__lseek : UInt64;
  __imp_lseek : UInt64;
  __imp_itoa : UInt64;
  __imp__lseeki64 : UInt64;
  __imp_ltoa : UInt64;
  __imp__ltoa : UInt64;
  __imp__ltoa_s : UInt64;
  __imp__ltow : UInt64;
  __imp__ltow_s : UInt64;
  __imp__makepath : UInt64;
  __imp__makepath_s : UInt64;
  __imp__mbbtombc : UInt64;
  __imp__mbbtombc_l : UInt64;
  __imp__mbbtype : UInt64;
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
  __imp__mbctype : UInt64;
  __imp__mblen_l : UInt64;
  __imp__mbsbtype : UInt64;
  __imp__mbsbtype_l : UInt64;
  __imp__mbscat : UInt64;
  __imp__mbscat_s : UInt64;
  __imp__mbscat_s_l : UInt64;
  __imp__mbschr : UInt64;
  __imp__mbschr_l : UInt64;
  __imp__mbscmp : UInt64;
  __imp__mbscmp_l : UInt64;
  __imp__mbscoll : UInt64;
  __imp__mbscoll_l : UInt64;
  __imp__mbscpy : UInt64;
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
  __imp_memccpy : UInt64;
  __imp__memccpy : UInt64;
  __imp__memicmp : UInt64;
  __imp__memicmp_l : UInt64;
  __imp__mkdir : UInt64;
  __imp__mkgmtime : UInt64;
  __imp__mkgmtime32 : UInt64;
  __imp__mkgmtime64 : UInt64;
  __imp_mktemp : UInt64;
  __imp__mktemp : UInt64;
  __imp__mktime64 : UInt64;
  __imp__msize : UInt64;
  __imp__nextafter : UInt64;
  __imp__nextafterf : UInt64;
  __imp__onexit : UInt64;
  __imp_nextafter : UInt64;
  __imp_mkdir : UInt64;
  __imp_open : UInt64;
  __imp__open : UInt64;
  __imp__open_osfhandle : UInt64;
  __imp__osplatform : UInt64;
  __imp__osver : UInt64;
  __imp__pclose : UInt64;
  __imp_pclose : UInt64;
  __imp__pctype : UInt64;
  __imp__pgmptr : UInt64;
  __imp__pipe : UInt64;
  __imp__popen : UInt64;
  __imp_popen : UInt64;
  __imp__printf_l : UInt64;
  __imp__printf_p : UInt64;
  __imp__printf_p_l : UInt64;
  __imp__printf_s_l : UInt64;
  __imp__purecall : UInt64;
  __imp__putch : UInt64;
  __imp__putenv : UInt64;
  __imp__putenv_s : UInt64;
  __imp_putenv : UInt64;
  __imp_putw : UInt64;
  __imp__putw : UInt64;
  __imp__putwch : UInt64;
  __imp_putch : UInt64;
  __imp__putws : UInt64;
  __imp__pwctype : UInt64;
  __imp__read : UInt64;
  __imp_read : UInt64;
  __imp__resetstkoflw : UInt64;
  __imp__rmdir : UInt64;
  __imp__rmtmp : UInt64;
  __imp__rotl : UInt64;
  __imp__rotl64 : UInt64;
  __imp__rotr : UInt64;
  __imp__rotr64 : UInt64;
  __imp__scalb : UInt64;
  __imp__scalbf : UInt64;
  __imp_rmtmp : UInt64;
  __imp_rmdir : UInt64;
  __imp__scanf_l : UInt64;
  __imp__scanf_s_l : UInt64;
  __imp__scprintf : UInt64;
  __imp_memicmp : UInt64;
  __imp__scprintf_l : UInt64;
  __imp__scprintf_p_l : UInt64;
  __imp__scwprintf : UInt64;
  __imp__scwprintf_l : UInt64;
  __imp__scwprintf_p_l : UInt64;
  __imp_searchenv : UInt64;
  __imp__searchenv : UInt64;
  __imp__searchenv_s : UInt64;
  __imp__set_error_mode : UInt64;
  __imp__set_sbh_threshold : UInt64;
  __imp__seterrormode : UInt64;
  __imp__setjmp : UInt64;
  __imp__setjmpex : UInt64;
  __imp__setmaxstdio : UInt64;
  __imp__setmbcp : UInt64;
  __imp_setmode : UInt64;
  __imp__setmode : UInt64;
  __imp__setsystime : UInt64;
  __imp__sleep : UInt64;
  __imp__snprintf : UInt64;
  __imp__snprintf_c : UInt64;
  __imp__snprintf_c_l : UInt64;
  __imp__snprintf_l : UInt64;
  __imp__snprintf_s : UInt64;
  __imp__snprintf_s_l : UInt64;
  __imp__snscanf : UInt64;
  __imp__snscanf_l : UInt64;
  __imp__snscanf_s : UInt64;
  __imp__snscanf_s_l : UInt64;
  __imp_snwprintf : UInt64;
  __imp__snwprintf : UInt64;
  __imp__snwprintf_l : UInt64;
  __imp__snwprintf_s : UInt64;
  __imp__snwprintf_s_l : UInt64;
  __imp__snwscanf : UInt64;
  __imp__snwscanf_l : UInt64;
  __imp__snwscanf_s : UInt64;
  __imp__snwscanf_s_l : UInt64;
  __imp_sopen : UInt64;
  __imp__sopen : UInt64;
  __imp__spawnl : UInt64;
  __imp_spawnl : UInt64;
  __imp__spawnle : UInt64;
  __imp_spawnlp : UInt64;
  __imp__spawnlp : UInt64;
  __imp__spawnlpe : UInt64;
  __imp__spawnv : UInt64;
  __imp_spawnve : UInt64;
  __imp__spawnve : UInt64;
  __imp_spawnle : UInt64;
  __imp_spawnv : UInt64;
  __imp__spawnvp : UInt64;
  __imp_spawnvpe : UInt64;
  __imp__spawnvpe : UInt64;
  __imp__splitpath : UInt64;
  __imp__splitpath_s : UInt64;
  __imp__sprintf_l : UInt64;
  __imp__sprintf_p_l : UInt64;
  __imp__sprintf_s_l : UInt64;
  __imp__sscanf_l : UInt64;
  __imp__sscanf_s_l : UInt64;
  __imp__stat64i32 : UInt64;
  __imp__stat : UInt64;
  __imp__stat64 : UInt64;
  __imp__stati64 : UInt64;
  __imp__statusfp : UInt64;
  __imp_strcmpi : UInt64;
  __imp__strcmpi : UInt64;
  __imp__strcoll_l : UInt64;
  __imp__strdate : UInt64;
  __imp__strerror : UInt64;
  __imp__strerror_s : UInt64;
  __imp__stricmp : UInt64;
  __imp_strcasecmp : UInt64;
  __imp_strdup : UInt64;
  __imp__strdup : UInt64;
  __imp__stricmp_l : UInt64;
  __imp_stricoll : UInt64;
  __imp__stricoll : UInt64;
  __imp_stricmp : UInt64;
  __imp__stricoll_l : UInt64;
  __imp_strlwr : UInt64;
  __imp__strlwr : UInt64;
  __imp__strlwr_l : UInt64;
  __imp__strlwr_s : UInt64;
  __imp_spawnvp : UInt64;
  __imp_spawnlpe : UInt64;
  __imp__strlwr_s_l : UInt64;
  __imp__strncoll : UInt64;
  __imp__strncoll_l : UInt64;
  __imp__strnicmp : UInt64;
  __imp__strnicmp_l : UInt64;
  __imp__strnicoll : UInt64;
  __imp__strnicoll_l : UInt64;
  __imp_strnicmp : UInt64;
  __imp__strnset : UInt64;
  __imp_strnset : UInt64;
  __imp__strnset_s : UInt64;
  __imp__strrev : UInt64;
  __imp__strset : UInt64;
  __imp_strrev : UInt64;
  __imp_strset : UInt64;
  __imp__strset_s : UInt64;
  __imp__strtime : UInt64;
  __imp__strtod_l : UInt64;
  __imp__strtoi64 : UInt64;
  __imp__strtoi64_l : UInt64;
  __imp__strtol_l : UInt64;
  __imp__strtoui64 : UInt64;
  __imp__strtoui64_l : UInt64;
  __imp_strncasecmp : UInt64;
  __imp__strtoul_l : UInt64;
  __imp__strupr : UInt64;
  __imp__strupr_l : UInt64;
  __imp__strupr_s : UInt64;
  __imp__strupr_s_l : UInt64;
  __imp__strxfrm_l : UInt64;
  __imp__swab : UInt64;
  __imp__swprintf_c : UInt64;
  __imp_swab : UInt64;
  __imp_strupr : UInt64;
  __imp__swprintf_c_l : UInt64;
  __imp__swprintf_p_l : UInt64;
  __imp__swprintf_s_l : UInt64;
  __imp__swscanf_l : UInt64;
  __imp__swscanf_s_l : UInt64;
  __imp__sys_errlist : UInt64;
  __imp__sys_nerr : UInt64;
  __imp_tell : UInt64;
  __imp__tell : UInt64;
  __imp__telli64 : UInt64;
  __imp_tempnam : UInt64;
  __imp__tempnam : UInt64;
  __imp__time64 : UInt64;
  __imp__timezone : UInt64;
  __imp_timezone : UInt64;
  __imp__tolower : UInt64;
  __imp__tolower_l : UInt64;
  __imp__toupper : UInt64;
  __imp__toupper_l : UInt64;
  __imp__towlower_l : UInt64;
  __imp__towupper_l : UInt64;
  __imp_tzname : UInt64;
  __imp__tzname : UInt64;
  __imp_tzset : UInt64;
  __imp__tzset : UInt64;
  __imp__ui64toa : UInt64;
  __imp__ui64toa_s : UInt64;
  __imp__ui64tow : UInt64;
  __imp__ui64tow_s : UInt64;
  __imp__ultoa : UInt64;
  __imp__ultoa_s : UInt64;
  __imp__ultow : UInt64;
  __imp__ultow_s : UInt64;
  __imp__umask : UInt64;
  __imp__ungetch : UInt64;
  __imp__ungetwch : UInt64;
  __imp_ungetch : UInt64;
  __imp__unlink : UInt64;
  __imp__unloaddll : UInt64;
  __imp_unlink : UInt64;
  __imp__unlock : UInt64;
  __imp_utime : UInt64;
  __imp__utime : UInt64;
  __imp__utime32 : UInt64;
  __imp__utime64 : UInt64;
  __imp__vcprintf : UInt64;
  __imp_umask : UInt64;
  __imp__vcprintf_l : UInt64;
  __imp__vcprintf_p : UInt64;
  __imp__vcprintf_p_l : UInt64;
  __imp__vcwprintf : UInt64;
  __imp__vcwprintf_l : UInt64;
  __imp__vcwprintf_p : UInt64;
  __imp__vcwprintf_p_l : UInt64;
  __imp__vfprintf_l : UInt64;
  __imp__vfprintf_p : UInt64;
  __imp__vfprintf_p_l : UInt64;
  __imp__vfprintf_s_l : UInt64;
  __imp__vfwprintf_l : UInt64;
  __imp__vfwprintf_p : UInt64;
  __imp__vfwprintf_p_l : UInt64;
  __imp__vfwprintf_s_l : UInt64;
  __imp__vprintf_l : UInt64;
  __imp__vprintf_p : UInt64;
  __imp__vprintf_p_l : UInt64;
  __imp__vprintf_s_l : UInt64;
  __imp__vscprintf : UInt64;
  __imp__vscprintf_l : UInt64;
  __imp__vscprintf_p_l : UInt64;
  __imp__vscwprintf : UInt64;
  __imp__vscwprintf_l : UInt64;
  __imp__vscwprintf_p_l : UInt64;
  __imp__vsnprintf : UInt64;
  __imp__vsnprintf_c : UInt64;
  __imp__vsnprintf_c_l : UInt64;
  __imp__vsnprintf_l : UInt64;
  __imp__vsnprintf_s : UInt64;
  __imp__vsnprintf_s_l : UInt64;
  __imp__vsnwprintf_l : UInt64;
  __imp__vsnwprintf_s : UInt64;
  __imp__vsnwprintf_s_l : UInt64;
  __imp_vsnwprintf : UInt64;
  __imp__vsnwprintf : UInt64;
  __imp_vsnprintf_s : UInt64;
  __imp__vsprintf_l : UInt64;
  __imp__vsprintf_p : UInt64;
  __imp__vsprintf_p_l : UInt64;
  __imp__vsprintf_s_l : UInt64;
  __imp__vswprintf : UInt64;
  __imp__vswprintf_c : UInt64;
  __imp__vswprintf_c_l : UInt64;
  __imp__vswprintf_l : UInt64;
  __imp__vswprintf_p_l : UInt64;
  __imp__vswprintf_s_l : UInt64;
  __imp__vwprintf_l : UInt64;
  __imp__vwprintf_p : UInt64;
  __imp__vwprintf_p_l : UInt64;
  __imp__vwprintf_s_l : UInt64;
  __imp__waccess : UInt64;
  __imp__wasctime : UInt64;
  __imp__wassert : UInt64;
  __imp__wchdir : UInt64;
  __imp__wchmod : UInt64;
  __imp__wcmdln : UInt64;
  __imp__wcreat : UInt64;
  __imp__wcscoll_l : UInt64;
  __imp_wcsdup : UInt64;
  __imp__wcsdup : UInt64;
  __imp__wcserror : UInt64;
  __imp__wcserror_s : UInt64;
  __imp__wcsftime_l : UInt64;
  __imp__wcsicmp : UInt64;
  __imp_wcsicmp : UInt64;
  __imp_wcscmpi : UInt64;
  __imp__wcsicmp_l : UInt64;
  __imp_wcsicoll : UInt64;
  __imp__wcsicoll : UInt64;
  __imp__wcsicoll_l : UInt64;
  __imp__wcslwr : UInt64;
  __imp__wcslwr_l : UInt64;
  __imp__wcslwr_s : UInt64;
  __imp__wcslwr_s_l : UInt64;
  __imp_wcslwr : UInt64;
  __imp__wcsncoll : UInt64;
  __imp__wcsncoll_l : UInt64;
  __imp_wcsnicmp : UInt64;
  __imp__wcsnicmp : UInt64;
  __imp__wcsnicmp_l : UInt64;
  __imp__wcsnicoll : UInt64;
  __imp__wcsnicoll_l : UInt64;
  __imp__wcsnset : UInt64;
  __imp_wcsnset : UInt64;
  __imp__wcsnset_s : UInt64;
  __imp__wcsrev : UInt64;
  __imp_wcsset : UInt64;
  __imp__wcsset : UInt64;
  __imp__wcsset_s : UInt64;
  __imp__wcstoi64 : UInt64;
  __imp__wcstoi64_l : UInt64;
  __imp__wcstol_l : UInt64;
  __imp_wcsrev : UInt64;
  __imp__wcstombs_l : UInt64;
  __imp__wcstombs_s_l : UInt64;
  __imp__wcstoui64 : UInt64;
  __imp__wcstoui64_l : UInt64;
  __imp__wcstoul_l : UInt64;
  __imp_wcsupr : UInt64;
  __imp__wcsupr : UInt64;
  __imp__wcsupr_l : UInt64;
  __imp__wcsupr_s : UInt64;
  __imp__wcsupr_s_l : UInt64;
  __imp__wcsxfrm_l : UInt64;
  __imp__wctime : UInt64;
  __imp__wctime64 : UInt64;
  __imp__wctomb_l : UInt64;
  __imp__wctomb_s_l : UInt64;
  __imp__wctype : UInt64;
  __imp__wenviron : UInt64;
  __imp__wexecl : UInt64;
  __imp__wexecle : UInt64;
  __imp__wexeclp : UInt64;
  __imp__wexeclpe : UInt64;
  __imp__wexecv : UInt64;
  __imp__wexecve : UInt64;
  __imp__wexecvp : UInt64;
  __imp__wexecvpe : UInt64;
  __imp__wfdopen : UInt64;
  __imp__wfindfirst64i32 : UInt64;
  __imp__wfindfirst : UInt64;
  __imp__wfindfirst64 : UInt64;
  __imp__wfindfirsti64 : UInt64;
  __imp__wfindnext : UInt64;
  __imp__wfindnext64i32 : UInt64;
  __imp__wfindnext64 : UInt64;
  __imp__wfindnexti64 : UInt64;
  __imp__wfopen : UInt64;
  __imp__wfopen_s : UInt64;
  __imp__wfreopen : UInt64;
  __imp__wfreopen_s : UInt64;
  __imp__wfsopen : UInt64;
  __imp__wfullpath : UInt64;
  __imp__wgetcwd : UInt64;
  __imp__wgetdcwd : UInt64;
  __imp__wgetenv : UInt64;
  __imp__wgetenv_s : UInt64;
  __imp__winmajor : UInt64;
  __imp__winminor : UInt64;
  __imp__winput_s : UInt64;
  __imp__winver : UInt64;
  __imp__wmakepath : UInt64;
  __imp__wmakepath_s : UInt64;
  __imp__wmkdir : UInt64;
  __imp__wmktemp : UInt64;
  __imp__wopen : UInt64;
  __imp__woutput_s : UInt64;
  __imp__wperror : UInt64;
  __imp__wpgmptr : UInt64;
  __imp_wpopen : UInt64;
  __imp__wpopen : UInt64;
  __imp__wprintf_l : UInt64;
  __imp__wprintf_p : UInt64;
  __imp_time : UInt64;
  __imp__wprintf_p_l : UInt64;
  __imp__wprintf_s_l : UInt64;
  __imp__wputenv : UInt64;
  __imp__wputenv_s : UInt64;
  __imp__wremove : UInt64;
  __imp__wrename : UInt64;
  __imp__write : UInt64;
  __imp__wrmdir : UInt64;
  __imp__wscanf_l : UInt64;
  __imp__wscanf_s_l : UInt64;
  __imp__wsearchenv : UInt64;
  __imp__wsearchenv_s : UInt64;
  __imp__wsetlocale : UInt64;
  __imp__wsopen : UInt64;
  __imp__wsopen_s : UInt64;
  __imp__wspawnl : UInt64;
  __imp__wspawnle : UInt64;
  __imp__wspawnlp : UInt64;
  __imp__wspawnlpe : UInt64;
  __imp__wspawnv : UInt64;
  __imp__wspawnve : UInt64;
  __imp__wspawnvp : UInt64;
  __imp__wspawnvpe : UInt64;
  __imp__wsplitpath : UInt64;
  __imp__wsplitpath_s : UInt64;
  __imp__wstat64i32 : UInt64;
  __imp__wstat : UInt64;
  __imp__wstat64 : UInt64;
  __imp__wstati64 : UInt64;
  __imp__wstrdate : UInt64;
  __imp__wstrtime : UInt64;
  __imp__wsystem : UInt64;
  __imp__wtempnam : UInt64;
  __imp__wtmpnam : UInt64;
  __imp__wtmpnam_s : UInt64;
  __imp__wtof : UInt64;
  __imp__wtof_l : UInt64;
  __imp__wtoi : UInt64;
  __imp__wtoi64 : UInt64;
  __imp__wtoi64_l : UInt64;
  __imp__wtoi_l : UInt64;
  __imp__wtol : UInt64;
  __imp__wtol_l : UInt64;
  __imp__wunlink : UInt64;
  __imp__wutime : UInt64;
  __imp__wutime32 : UInt64;
  __imp__wutime64 : UInt64;
  __imp__y0 : UInt64;
  __imp_y0 : UInt64;
  __imp_y1 : UInt64;
  __imp__y1 : UInt64;
  __imp__yn : UInt64;
  __imp_abort : UInt64;
  __imp_abs : UInt64;
  __imp_acos : UInt64;
  __imp_yn : UInt64;
  __imp_acosf : UInt64;
  __imp_asctime : UInt64;
  __imp_asin : UInt64;
  __imp_asinf : UInt64;
  __imp_atan : UInt64;
  __imp_atan2 : UInt64;
  __imp_atan2f : UInt64;
  __imp_atanf : UInt64;
  __imp_atexit : UInt64;
  __imp_atof : UInt64;
  __imp_atoi : UInt64;
  __imp_atol : UInt64;
  __imp_bsearch : UInt64;
  __imp_bsearch_s : UInt64;
  __imp_calloc : UInt64;
  __imp_ceil : UInt64;
  __imp_ceilf : UInt64;
  __imp_clearerr : UInt64;
  __imp_clearerr_s : UInt64;
  __imp_clock : UInt64;
  __imp_cos : UInt64;
  __imp_cosf : UInt64;
  __imp_cosh : UInt64;
  __imp_coshf : UInt64;
  __imp_ctime : UInt64;
  __imp_difftime : UInt64;
  __imp_div : UInt64;
  __imp_exit : UInt64;
  __imp_exp : UInt64;
  __imp_expf : UInt64;
  __imp___ms_fwscanf : UInt64;
  __imp_fabs : UInt64;
  __imp_fclose : UInt64;
  __imp_feof : UInt64;
  __imp_ferror : UInt64;
  __imp_fflush : UInt64;
  __imp_fgetc : UInt64;
  __imp_fgetpos : UInt64;
  __imp_fgets : UInt64;
  __imp_fgetwc : UInt64;
  __imp_fgetws : UInt64;
  __imp_floor : UInt64;
  __imp_floorf : UInt64;
  __imp_fmod : UInt64;
  __imp_fmodf : UInt64;
  __imp_fopen : UInt64;
  __imp_fopen_s : UInt64;
  __imp___ms_fprintf : UInt64;
  __imp_fprintf : UInt64;
  __imp_fprintf_s : UInt64;
  __imp_fputc : UInt64;
  __imp_fputs : UInt64;
  __imp_fputwc : UInt64;
  __imp_fputws : UInt64;
  __imp_fread : UInt64;
  __imp_free : UInt64;
  __imp_freopen : UInt64;
  __imp_freopen_s : UInt64;
  __imp_frexp : UInt64;
  __imp___ms_fscanf : UInt64;
  __imp_fscanf : UInt64;
  __imp_fscanf_s : UInt64;
  __imp___ms_fwprintf : UInt64;
  __imp_fseek : UInt64;
  __imp_fsetpos : UInt64;
  __imp_ftell : UInt64;
  __imp_fwprintf : UInt64;
  __imp_fwprintf_s : UInt64;
  __imp_fwrite : UInt64;
  __imp_fwscanf : UInt64;
  __imp_fwscanf_s : UInt64;
  __imp_getc : UInt64;
  __imp_getchar : UInt64;
  __imp_getenv : UInt64;
  __imp_getenv_s : UInt64;
  __imp_gets : UInt64;
  __imp_getwc : UInt64;
  __imp_getwchar : UInt64;
  __imp_gmtime : UInt64;
  __imp_is_wctype : UInt64;
  __imp_isalnum : UInt64;
  __imp_isalpha : UInt64;
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
  __imp_labs : UInt64;
  __imp_ldexp : UInt64;
  __imp_write : UInt64;
  __imp_ldiv : UInt64;
  __imp_localeconv : UInt64;
  __imp_localtime : UInt64;
  __imp_log : UInt64;
  __imp_log10 : UInt64;
  __imp_log10f : UInt64;
  __imp_logf : UInt64;
  __imp_longjmp : UInt64;
  __imp_malloc : UInt64;
  __imp_mblen : UInt64;
  __imp_mbsrtowcs_s : UInt64;
  __imp_mbstowcs : UInt64;
  __imp_mbstowcs_s : UInt64;
  __imp_mbtowc : UInt64;
  __imp_memchr : UInt64;
  __imp_memcmp : UInt64;
  __imp_memcpy : UInt64;
  __imp_memmove : UInt64;
  __imp_memset : UInt64;
  __imp_mktime : UInt64;
  __imp_modf : UInt64;
  __imp_modff : UInt64;
  __imp_perror : UInt64;
  __imp_pow : UInt64;
  __imp_powf : UInt64;
  __imp___ms_printf : UInt64;
  __imp_printf : UInt64;
  __imp_printf_s : UInt64;
  __imp_putc : UInt64;
  __imp_putchar : UInt64;
  __imp_puts : UInt64;
  __imp_putwc : UInt64;
  __imp_putwchar : UInt64;
  __imp_qsort : UInt64;
  __imp_qsort_s : UInt64;
  __imp_raise : UInt64;
  __imp_realloc : UInt64;
  __imp___ms_scanf : UInt64;
  __imp_rand : UInt64;
  __imp_remove : UInt64;
  __imp_rename : UInt64;
  __imp_rewind : UInt64;
  __imp_scanf : UInt64;
  __imp_scanf_s : UInt64;
  __imp_setbuf : UInt64;
  __imp_setjmp : UInt64;
  __imp_setlocale : UInt64;
  __imp_setvbuf : UInt64;
  __imp_signal : UInt64;
  __imp_sin : UInt64;
  __imp_sinf : UInt64;
  __imp_sinh : UInt64;
  __imp_sinhf : UInt64;
  __imp___ms_sprintf : UInt64;
  __imp_sprintf : UInt64;
  __imp_sqrt : UInt64;
  __imp_sqrtf : UInt64;
  __imp_srand : UInt64;
  __imp___ms_sscanf : UInt64;
  __imp_sscanf : UInt64;
  __imp_sscanf_s : UInt64;
  __imp_strcat : UInt64;
  __imp_strcat_s : UInt64;
  __imp_strchr : UInt64;
  __imp_strcmp : UInt64;
  __imp_strcoll : UInt64;
  __imp_strcpy : UInt64;
  __imp_strcpy_s : UInt64;
  __imp_strcspn : UInt64;
  __imp_strerror : UInt64;
  __imp_strftime : UInt64;
  __imp_strlen : UInt64;
  __imp_strncat : UInt64;
  __imp_strncat_s : UInt64;
  __imp_strncmp : UInt64;
  __imp_strncpy : UInt64;
  __imp_strncpy_s : UInt64;
  __imp_strpbrk : UInt64;
  __imp_strrchr : UInt64;
  __imp_strspn : UInt64;
  __imp_strstr : UInt64;
  __imp_strtod : UInt64;
  __imp_strtok : UInt64;
  __imp__swprintf : UInt64;
  __imp_strtok_s : UInt64;
  __imp_strtol : UInt64;
  __imp_strtoul : UInt64;
  __imp___ms_swprintf : UInt64;
  __imp_swprintf : UInt64;
  __imp_swprintf_s : UInt64;
  __imp___ms_swscanf : UInt64;
  __imp_strxfrm : UInt64;
  __imp_swscanf : UInt64;
  __imp_swscanf_s : UInt64;
  __imp_system : UInt64;
  __imp_tan : UInt64;
  __imp_tanf : UInt64;
  __imp_tanh : UInt64;
  __imp_tmpfile : UInt64;
  __imp_tmpfile_s : UInt64;
  __imp_tmpnam : UInt64;
  __imp_tmpnam_s : UInt64;
  __imp_tolower : UInt64;
  __imp_toupper : UInt64;
  __imp_towlower : UInt64;
  __imp___ms_vprintf : UInt64;
  __imp___ms_vsprintf : UInt64;
  __imp_towupper : UInt64;
  __imp_ungetc : UInt64;
  __imp_vfprintf : UInt64;
  __imp_vfprintf_s : UInt64;
  __imp___ms_vfwprintf : UInt64;
  __imp___ms_vfprintf : UInt64;
  __imp_ungetwc : UInt64;
  __imp_vfwprintf : UInt64;
  __imp_vfwprintf_s : UInt64;
  __imp_vprintf : UInt64;
  __imp___ms_vswprintf : UInt64;
  __imp_vprintf_s : UInt64;
  __imp_vswprintf : UInt64;
  __imp___ms_vwprintf : UInt64;
  __imp_vsprintf : UInt64;
  __imp_vswprintf_s : UInt64;
  __imp_vwprintf : UInt64;
  __imp_vwprintf_s : UInt64;
  __imp_wcrtomb_s : UInt64;
  __imp_wcscat : UInt64;
  __imp_wcscat_s : UInt64;
  __imp_wcschr : UInt64;
  __imp_wcscmp : UInt64;
  __imp_wcscoll : UInt64;
  __imp_wcscpy : UInt64;
  __imp_wcscpy_s : UInt64;
  __imp_wcscspn : UInt64;
  __imp_wcsftime : UInt64;
  __imp_wcslen : UInt64;
  __imp_wcsncat : UInt64;
  __imp_wcsncat_s : UInt64;
  __imp_wcsncmp : UInt64;
  __imp_wcsncpy : UInt64;
  __imp_wcsncpy_s : UInt64;
  __imp_wcsnlen : UInt64;
  __imp_wcspbrk : UInt64;
  __imp_wcsrchr : UInt64;
  __imp_wcsrtombs_s : UInt64;
  __imp_wcsspn : UInt64;
  __imp_wcsstr : UInt64;
  __imp_wcstod : UInt64;
  __imp_wcstok : UInt64;
  __imp_wcstok_s : UInt64;
  __imp_wcstol : UInt64;
  __imp_wcstombs : UInt64;
  __imp_wcstombs_s : UInt64;
  __imp_wcstoul : UInt64;
  __imp_wcsxfrm : UInt64;
  __imp___ms_wprintf : UInt64;
  __imp_wctomb : UInt64;
  __imp_wctomb_s : UInt64;
  __imp_wprintf : UInt64;
  __imp_wprintf_s : UInt64;
  __imp___ms_wscanf : UInt64;
  __imp_wscanf : UInt64;
  __imp_wscanf_s : UInt64;
  __imp_iscsymf : UInt64;
  _head_lib64_libmsvcrt_def_a : UInt64;
  __lib64_libmsvcrt_def_a_iname : UInt64;
{$ELSE}
procedure _init_scprintf;external;
{$L x86/lib32_libmsvcrt_extra_a-_scprintf.o}
procedure __set_errno;external;
{$L x86/lib32_libmsvcrt_extra_a-seterrno.o}
procedure ____mb_cur_max_func;external;
{$L x86/lib32_libmsvcrt_extra_a-___mb_cur_max_func.o}
procedure __copysignf;external;
{$L x86/lib32_libmsvcrt_extra_a-_copysignf.o}
procedure __lock_file;external;
{$L x86/lib32_libmsvcrt_extra_a-mingw_lock.o}
procedure __ftelli64;external;
{$L x86/lib32_libmsvcrt_extra_a-fseeki64.o}
procedure _sprintf_s;external;
{$L x86/lib32_libmsvcrt_extra_a-sprintf_s.o}
procedure __vswprintf_p;external;
{$L x86/lib32_libmsvcrt_extra_a-_vswprintf_p.o}
procedure __vscwprintf_p;external;
{$L x86/lib32_libmsvcrt_extra_a-_vscwprintf_p.o}
procedure __vscprintf_p;external;
{$L x86/lib32_libmsvcrt_extra_a-_vscprintf_p.o}
procedure __cwprintf_s_l;external;
{$L x86/lib32_libmsvcrt_extra_a-_cwprintf_s_l.o}
procedure __cwprintf_s;external;
{$L x86/lib32_libmsvcrt_extra_a-_cwprintf_s.o}
procedure __cprintf_s_l;external;
{$L x86/lib32_libmsvcrt_extra_a-_cprintf_s_l.o}
procedure __cprintf_s;external;
{$L x86/lib32_libmsvcrt_extra_a-_cprintf_s.o}
procedure __set_purecall_handler;external;
{$L x86/lib32_libmsvcrt_extra_a-purecall.o}
procedure __get_invalid_parameter_handler;external;
{$L x86/lib32_libmsvcrt_extra_a-invalid_parameter_handler.o}
procedure __configthreadlocale;external;
{$L x86/lib32_libmsvcrt_extra_a-_configthreadlocale.o}
procedure _frexp;external;
{$L x86/lib32_libmsvcrt_common_a-frexp.o}
procedure _vsnprintf;external;
{$L x86/lib32_libmsvcrt_common_a-vsnprintf_alias.o}
procedure _snprintf;external;
{$L x86/lib32_libmsvcrt_common_a-snprintf_alias.o}
procedure ___acrt_iob_func;external;
{$L x86/lib32_libmsvcrt_common_a-acrt_iob_func.o}
procedure __putwc_nolock;external;
{$L x86/lib32_libmsvcrt_common_a-_putwc_nolock.o}
procedure __putc_nolock;external;
{$L x86/lib32_libmsvcrt_common_a-_putc_nolock.o}
procedure __getwc_nolock;external;
{$L x86/lib32_libmsvcrt_common_a-_getwc_nolock.o}
procedure __getc_nolock;external;
{$L x86/lib32_libmsvcrt_common_a-_getc_nolock.o}
procedure _mbrtoc32;external;
{$L x86/lib32_libmsvcrt_common_a-uchar_mbrtoc32.o}
procedure _mbrtoc16;external;
{$L x86/lib32_libmsvcrt_common_a-uchar_mbrtoc16.o}
procedure _c32rtomb;external;
{$L x86/lib32_libmsvcrt_common_a-uchar_c32rtomb.o}
procedure _c16rtomb;external;
{$L x86/lib32_libmsvcrt_common_a-uchar_c16rtomb.o}
procedure __initialize_onexit_table;external;
{$L x86/lib32_libmsvcrt_common_a-onexit_table.o}
procedure _mbsinit;external;
{$L x86/lib32_libmsvcrt_common_a-mbsinit.o}
procedure __CIacos;external;
{$L x86/libmsvcrt_defs00000.o}
procedure __CIasin;external;
{$L x86/libmsvcrt_defs00001.o}
procedure __CIatan;external;
{$L x86/libmsvcrt_defs00002.o}
procedure __CIatan2;external;
{$L x86/libmsvcrt_defs00003.o}
procedure __CIcos;external;
{$L x86/libmsvcrt_defs00004.o}
procedure __CIcosh;external;
{$L x86/libmsvcrt_defs00005.o}
procedure __CIexp;external;
{$L x86/libmsvcrt_defs00006.o}
procedure __CIfmod;external;
{$L x86/libmsvcrt_defs00007.o}
procedure __CIlog;external;
{$L x86/libmsvcrt_defs00008.o}
procedure __CIlog10;external;
{$L x86/libmsvcrt_defs00009.o}
procedure __CIpow;external;
{$L x86/libmsvcrt_defs00010.o}
procedure __CIsin;external;
{$L x86/libmsvcrt_defs00011.o}
procedure __CIsinh;external;
{$L x86/libmsvcrt_defs00012.o}
procedure __CIsqrt;external;
{$L x86/libmsvcrt_defs00013.o}
procedure __CItan;external;
{$L x86/libmsvcrt_defs00014.o}
procedure __CItanh;external;
{$L x86/libmsvcrt_defs00015.o}
procedure __CRT_RTC_INIT;external;
{$L x86/libmsvcrt_defs00016.o}
procedure __EH_prolog;external;
{$L x86/libmsvcrt_defs00018.o}
procedure __Getdays;external;
{$L x86/libmsvcrt_defs00019.o}
procedure __Getmonths;external;
{$L x86/libmsvcrt_defs00020.o}
procedure __Gettnames;external;
{$L x86/libmsvcrt_defs00021.o}
procedure __Strftime;external;
{$L x86/libmsvcrt_defs00023.o}
procedure __XcptFilter;external;
{$L x86/libmsvcrt_defs00024.o}
procedure ___CppXcptFilter;external;
{$L x86/libmsvcrt_defs00025.o}
procedure ___CxxCallUnwindDtor;external;
{$L x86/libmsvcrt_defs00026.o}
procedure ___CxxCallUnwindVecDtor;external;
{$L x86/libmsvcrt_defs00027.o}
procedure ___CxxDetectRethrow;external;
{$L x86/libmsvcrt_defs00028.o}
procedure ___CxxExceptionFilter;external;
{$L x86/libmsvcrt_defs00029.o}
procedure ___CxxFrameHandler;external;
{$L x86/libmsvcrt_defs00030.o}
procedure ___CxxLongjmpUnwind;external;
{$L x86/libmsvcrt_defs00031.o}
procedure ___CxxQueryExceptionSize;external;
{$L x86/libmsvcrt_defs00032.o}
procedure ___CxxRegisterExceptionObject;external;
{$L x86/libmsvcrt_defs00033.o}
procedure ___CxxUnregisterExceptionObject;external;
{$L x86/libmsvcrt_defs00034.o}
procedure ___DestructExceptionObject;external;
{$L x86/libmsvcrt_defs00035.o}
procedure ___RTCastToVoid;external;
{$L x86/libmsvcrt_defs00036.o}
procedure ___RTDynamicCast;external;
{$L x86/libmsvcrt_defs00037.o}
procedure ___RTtypeid;external;
{$L x86/libmsvcrt_defs00038.o}
procedure ___STRINGTOLD;external;
{$L x86/libmsvcrt_defs00039.o}
procedure ____lc_collate_cp_func;external;
{$L x86/libmsvcrt_defs00040.o}
procedure ____lc_handle_func;external;
{$L x86/libmsvcrt_defs00041.o}
procedure ____setlc_active_func;external;
{$L x86/libmsvcrt_defs00042.o}
procedure ____unguarded_readlc_active_add_func;external;
{$L x86/libmsvcrt_defs00043.o}
procedure ___buffer_overrun;external;
{$L x86/libmsvcrt_defs00047.o}
procedure ___crtCompareStringA;external;
{$L x86/libmsvcrt_defs00048.o}
procedure ___crtCompareStringW;external;
{$L x86/libmsvcrt_defs00049.o}
procedure ___crtGetLocaleInfoW;external;
{$L x86/libmsvcrt_defs00050.o}
procedure ___crtGetStringTypeW;external;
{$L x86/libmsvcrt_defs00051.o}
procedure ___crtLCMapStringA;external;
{$L x86/libmsvcrt_defs00052.o}
procedure ___crtLCMapStringW;external;
{$L x86/libmsvcrt_defs00053.o}
procedure ___dllonexit;external;
{$L x86/libmsvcrt_defs00054.o}
procedure ___doserrno;external;
{$L x86/libmsvcrt_defs00055.o}
procedure ___fpecode;external;
{$L x86/libmsvcrt_defs00056.o}
procedure ___getmainargs;external;
{$L x86/libmsvcrt_defs00057.o}
procedure ___iob_func;external;
{$L x86/libmsvcrt_defs00059.o}
procedure ___isascii;external;
{$L x86/libmsvcrt_defs00060.o}
procedure ___iscsym;external;
{$L x86/libmsvcrt_defs00061.o}
procedure ___iscsymf;external;
{$L x86/libmsvcrt_defs00062.o}
procedure _isascii;external;
{$L x86/libmsvcrt_defs00063.o}
procedure ___lc_clike;external;
{$L x86/libmsvcrt_defs00064.o}
procedure ___lconv_init;external;
{$L x86/libmsvcrt_defs00068.o}
procedure ___p___argc;external;
{$L x86/libmsvcrt_defs00070.o}
procedure ___p___argv;external;
{$L x86/libmsvcrt_defs00071.o}
procedure ___p___initenv;external;
{$L x86/libmsvcrt_defs00072.o}
procedure ___p___mb_cur_max;external;
{$L x86/libmsvcrt_defs00073.o}
procedure ___p___wargv;external;
{$L x86/libmsvcrt_defs00074.o}
procedure ___p___winitenv;external;
{$L x86/libmsvcrt_defs00075.o}
procedure ___p__acmdln;external;
{$L x86/libmsvcrt_defs00076.o}
procedure ___p__amblksiz;external;
{$L x86/libmsvcrt_defs00077.o}
procedure ___p__commode;external;
{$L x86/libmsvcrt_defs00078.o}
procedure ___p__daylight;external;
{$L x86/libmsvcrt_defs00079.o}
procedure ___p__dstbias;external;
{$L x86/libmsvcrt_defs00080.o}
procedure ___p__environ;external;
{$L x86/libmsvcrt_defs00081.o}
procedure ___p__fileinfo;external;
{$L x86/libmsvcrt_defs00082.o}
procedure ___p__fmode;external;
{$L x86/libmsvcrt_defs00083.o}
procedure ___p__iob;external;
{$L x86/libmsvcrt_defs00084.o}
procedure ___p__mbcasemap;external;
{$L x86/libmsvcrt_defs00085.o}
procedure ___p__mbctype;external;
{$L x86/libmsvcrt_defs00086.o}
procedure ___p__osver;external;
{$L x86/libmsvcrt_defs00087.o}
procedure ___p__pctype;external;
{$L x86/libmsvcrt_defs00088.o}
procedure ___p__pgmptr;external;
{$L x86/libmsvcrt_defs00089.o}
procedure ___p__pwctype;external;
{$L x86/libmsvcrt_defs00090.o}
procedure ___p__timezone;external;
{$L x86/libmsvcrt_defs00091.o}
procedure ___p__tzname;external;
{$L x86/libmsvcrt_defs00092.o}
procedure ___p__wcmdln;external;
{$L x86/libmsvcrt_defs00093.o}
procedure ___p__wenviron;external;
{$L x86/libmsvcrt_defs00094.o}
procedure ___p__winmajor;external;
{$L x86/libmsvcrt_defs00095.o}
procedure ___p__winminor;external;
{$L x86/libmsvcrt_defs00096.o}
procedure ___p__winver;external;
{$L x86/libmsvcrt_defs00097.o}
procedure ___p__wpgmptr;external;
{$L x86/libmsvcrt_defs00098.o}
procedure ___pctype_func;external;
{$L x86/libmsvcrt_defs00099.o}
procedure ___pwctype_func;external;
{$L x86/libmsvcrt_defs00101.o}
procedure ___pxcptinfoptrs;external;
{$L x86/libmsvcrt_defs00102.o}
procedure ___security_error_handler;external;
{$L x86/libmsvcrt_defs00103.o}
procedure ___set_app_type;external;
{$L x86/libmsvcrt_defs00104.o}
procedure ___set_buffer_overrun_handler;external;
{$L x86/libmsvcrt_defs00105.o}
procedure ___setusermatherr;external;
{$L x86/libmsvcrt_defs00107.o}
procedure ___threadhandle;external;
{$L x86/libmsvcrt_defs00108.o}
procedure ___threadid;external;
{$L x86/libmsvcrt_defs00109.o}
procedure ___toascii;external;
{$L x86/libmsvcrt_defs00110.o}
procedure ___unDName;external;
{$L x86/libmsvcrt_defs00111.o}
procedure _toascii;external;
{$L x86/libmsvcrt_defs00112.o}
procedure ___unDNameEx;external;
{$L x86/libmsvcrt_defs00113.o}
procedure ___uncaught_exception;external;
{$L x86/libmsvcrt_defs00114.o}
procedure ___wcserror;external;
{$L x86/libmsvcrt_defs00117.o}
procedure ___wgetmainargs;external;
{$L x86/libmsvcrt_defs00118.o}
procedure __abnormal_termination;external;
{$L x86/libmsvcrt_defs00120.o}
procedure __access;external;
{$L x86/libmsvcrt_defs00121.o}
procedure _access;external;
{$L x86/libmsvcrt_defs00122.o}
procedure __adj_fdiv_m16i;external;
{$L x86/libmsvcrt_defs00124.o}
procedure __adj_fdiv_m32;external;
{$L x86/libmsvcrt_defs00125.o}
procedure __adj_fdiv_m32i;external;
{$L x86/libmsvcrt_defs00126.o}
procedure __adj_fdiv_m64;external;
{$L x86/libmsvcrt_defs00127.o}
procedure __adj_fdiv_r;external;
{$L x86/libmsvcrt_defs00128.o}
procedure __adj_fdivr_m16i;external;
{$L x86/libmsvcrt_defs00129.o}
procedure __adj_fdivr_m32;external;
{$L x86/libmsvcrt_defs00130.o}
procedure __adj_fdivr_m32i;external;
{$L x86/libmsvcrt_defs00131.o}
procedure __adj_fdivr_m64;external;
{$L x86/libmsvcrt_defs00132.o}
procedure __adj_fpatan;external;
{$L x86/libmsvcrt_defs00133.o}
procedure __adj_fprem;external;
{$L x86/libmsvcrt_defs00134.o}
procedure __adj_fprem1;external;
{$L x86/libmsvcrt_defs00135.o}
procedure __adj_fptan;external;
{$L x86/libmsvcrt_defs00136.o}
procedure __aligned_free;external;
{$L x86/libmsvcrt_defs00139.o}
procedure __aligned_malloc;external;
{$L x86/libmsvcrt_defs00140.o}
procedure __aligned_offset_malloc;external;
{$L x86/libmsvcrt_defs00141.o}
procedure __aligned_offset_realloc;external;
{$L x86/libmsvcrt_defs00142.o}
procedure __aligned_realloc;external;
{$L x86/libmsvcrt_defs00143.o}
procedure __amsg_exit;external;
{$L x86/libmsvcrt_defs00144.o}
procedure __assert;external;
{$L x86/libmsvcrt_defs00145.o}
procedure __atodbl;external;
{$L x86/libmsvcrt_defs00146.o}
procedure __atodbl_l;external;
{$L x86/libmsvcrt_defs00147.o}
procedure __atof_l;external;
{$L x86/libmsvcrt_defs00148.o}
procedure __atoflt_l;external;
{$L x86/libmsvcrt_defs00149.o}
procedure __atoi64;external;
{$L x86/libmsvcrt_defs00150.o}
procedure __atoi64_l;external;
{$L x86/libmsvcrt_defs00151.o}
procedure __atoi_l;external;
{$L x86/libmsvcrt_defs00152.o}
procedure __atol_l;external;
{$L x86/libmsvcrt_defs00153.o}
procedure __atoldbl;external;
{$L x86/libmsvcrt_defs00154.o}
procedure __beep;external;
{$L x86/libmsvcrt_defs00155.o}
procedure __beginthread;external;
{$L x86/libmsvcrt_defs00156.o}
procedure __beginthreadex;external;
{$L x86/libmsvcrt_defs00157.o}
procedure __c_exit;external;
{$L x86/libmsvcrt_defs00158.o}
procedure __callnewh;external;
{$L x86/libmsvcrt_defs00160.o}
procedure __cexit;external;
{$L x86/libmsvcrt_defs00161.o}
procedure __cgets;external;
{$L x86/libmsvcrt_defs00162.o}
procedure __cgetws;external;
{$L x86/libmsvcrt_defs00163.o}
procedure __chdir;external;
{$L x86/libmsvcrt_defs00164.o}
procedure _chdir;external;
{$L x86/libmsvcrt_defs00165.o}
procedure __chdrive;external;
{$L x86/libmsvcrt_defs00166.o}
procedure _iscsymf;external;
{$L x86/libmsvcrt_defs00167.o}
procedure _chgsign;external;
{$L x86/libmsvcrt_defs00168.o}
procedure __chgsign;external;
{$L x86/libmsvcrt_defs00169.o}
procedure __chkesp;external;
{$L x86/libmsvcrt_defs00170.o}
procedure _chmod;external;
{$L x86/libmsvcrt_defs00171.o}
procedure __chmod;external;
{$L x86/libmsvcrt_defs00172.o}
procedure __chsize;external;
{$L x86/libmsvcrt_defs00173.o}
procedure __clearfp;external;
{$L x86/libmsvcrt_defs00174.o}
procedure _close;external;
{$L x86/libmsvcrt_defs00175.o}
procedure __close;external;
{$L x86/libmsvcrt_defs00176.o}
procedure __commit;external;
{$L x86/libmsvcrt_defs00177.o}
procedure __control87;external;
{$L x86/libmsvcrt_defs00179.o}
procedure __controlfp;external;
{$L x86/libmsvcrt_defs00180.o}
procedure __copysign;external;
{$L x86/libmsvcrt_defs00181.o}
procedure __cprintf;external;
{$L x86/libmsvcrt_defs00182.o}
procedure __cprintf_l;external;
{$L x86/libmsvcrt_defs00183.o}
procedure __cprintf_p;external;
{$L x86/libmsvcrt_defs00184.o}
procedure __cprintf_p_l;external;
{$L x86/libmsvcrt_defs00185.o}
procedure __cputs;external;
{$L x86/libmsvcrt_defs00186.o}
procedure __cputws;external;
{$L x86/libmsvcrt_defs00187.o}
procedure _creat;external;
{$L x86/libmsvcrt_defs00188.o}
procedure __creat;external;
{$L x86/libmsvcrt_defs00189.o}
procedure __cscanf;external;
{$L x86/libmsvcrt_defs00190.o}
procedure __cscanf_l;external;
{$L x86/libmsvcrt_defs00191.o}
procedure _chsize;external;
{$L x86/libmsvcrt_defs00192.o}
procedure __cscanf_s;external;
{$L x86/libmsvcrt_defs00193.o}
procedure __cscanf_s_l;external;
{$L x86/libmsvcrt_defs00194.o}
procedure __ctime64;external;
{$L x86/libmsvcrt_defs00195.o}
procedure _cwait;external;
{$L x86/libmsvcrt_defs00197.o}
procedure __cwait;external;
{$L x86/libmsvcrt_defs00198.o}
procedure __cwprintf;external;
{$L x86/libmsvcrt_defs00199.o}
procedure __cwprintf_l;external;
{$L x86/libmsvcrt_defs00200.o}
procedure __cwprintf_p;external;
{$L x86/libmsvcrt_defs00201.o}
procedure __cwprintf_p_l;external;
{$L x86/libmsvcrt_defs00202.o}
procedure __cwscanf;external;
{$L x86/libmsvcrt_defs00203.o}
procedure __cwscanf_l;external;
{$L x86/libmsvcrt_defs00204.o}
procedure __cwscanf_s;external;
{$L x86/libmsvcrt_defs00205.o}
procedure __cwscanf_s_l;external;
{$L x86/libmsvcrt_defs00206.o}
procedure __difftime64;external;
{$L x86/libmsvcrt_defs00208.o}
procedure _daylight;external;
{$L x86/libmsvcrt_defs00209.o}
procedure _dup;external;
{$L x86/libmsvcrt_defs00211.o}
procedure __dup;external;
{$L x86/libmsvcrt_defs00212.o}
procedure __dup2;external;
{$L x86/libmsvcrt_defs00213.o}
procedure __ecvt;external;
{$L x86/libmsvcrt_defs00214.o}
procedure _dup2;external;
{$L x86/libmsvcrt_defs00215.o}
procedure __ecvt_s;external;
{$L x86/libmsvcrt_defs00216.o}
procedure _ecvt;external;
{$L x86/libmsvcrt_defs00217.o}
procedure __endthread;external;
{$L x86/libmsvcrt_defs00218.o}
procedure __endthreadex;external;
{$L x86/libmsvcrt_defs00219.o}
procedure __eof;external;
{$L x86/libmsvcrt_defs00221.o}
procedure __errno;external;
{$L x86/libmsvcrt_defs00222.o}
procedure __except_handler2;external;
{$L x86/libmsvcrt_defs00223.o}
procedure __except_handler3;external;
{$L x86/libmsvcrt_defs00224.o}
procedure __execl;external;
{$L x86/libmsvcrt_defs00225.o}
procedure _execle;external;
{$L x86/libmsvcrt_defs00226.o}
procedure __execle;external;
{$L x86/libmsvcrt_defs00227.o}
procedure _execl;external;
{$L x86/libmsvcrt_defs00228.o}
procedure __execlp;external;
{$L x86/libmsvcrt_defs00229.o}
procedure _execlp;external;
{$L x86/libmsvcrt_defs00230.o}
procedure __execlpe;external;
{$L x86/libmsvcrt_defs00231.o}
procedure __execv;external;
{$L x86/libmsvcrt_defs00232.o}
procedure __execve;external;
{$L x86/libmsvcrt_defs00233.o}
procedure _execv;external;
{$L x86/libmsvcrt_defs00234.o}
procedure _execve;external;
{$L x86/libmsvcrt_defs00235.o}
procedure __execvp;external;
{$L x86/libmsvcrt_defs00236.o}
procedure __execvpe;external;
{$L x86/libmsvcrt_defs00237.o}
procedure _execlpe;external;
{$L x86/libmsvcrt_defs00238.o}
procedure __exit;external;
{$L x86/libmsvcrt_defs00239.o}
procedure __expand;external;
{$L x86/libmsvcrt_defs00240.o}
procedure _eof;external;
{$L x86/libmsvcrt_defs00241.o}
procedure _execvp;external;
{$L x86/libmsvcrt_defs00242.o}
procedure _execvpe;external;
{$L x86/libmsvcrt_defs00243.o}
procedure __fcloseall;external;
{$L x86/libmsvcrt_defs00244.o}
procedure __fcvt;external;
{$L x86/libmsvcrt_defs00245.o}
procedure __fcvt_s;external;
{$L x86/libmsvcrt_defs00246.o}
procedure _fdopen;external;
{$L x86/libmsvcrt_defs00247.o}
procedure __fdopen;external;
{$L x86/libmsvcrt_defs00248.o}
procedure _fcvt;external;
{$L x86/libmsvcrt_defs00249.o}
procedure _fgetchar;external;
{$L x86/libmsvcrt_defs00250.o}
procedure __fgetchar;external;
{$L x86/libmsvcrt_defs00251.o}
procedure _fgetwchar;external;
{$L x86/libmsvcrt_defs00252.o}
procedure __fgetwchar;external;
{$L x86/libmsvcrt_defs00253.o}
procedure __filbuf;external;
{$L x86/libmsvcrt_defs00254.o}
procedure _filelength;external;
{$L x86/libmsvcrt_defs00255.o}
procedure __filelength;external;
{$L x86/libmsvcrt_defs00256.o}
procedure __filelengthi64;external;
{$L x86/libmsvcrt_defs00257.o}
procedure __fileno;external;
{$L x86/libmsvcrt_defs00259.o}
procedure __findclose;external;
{$L x86/libmsvcrt_defs00260.o}
procedure _fileno;external;
{$L x86/libmsvcrt_defs00261.o}
procedure __findfirst32i64;external;
{$L x86/libmsvcrt_defs00262.o}
procedure __findnext32i64;external;
{$L x86/libmsvcrt_defs00263.o}
procedure __findfirst32;external;
{$L x86/libmsvcrt_defs00264.o}
procedure __findfirst;external;
{$L x86/libmsvcrt_defs00265.o}
procedure __findfirst64;external;
{$L x86/libmsvcrt_defs00266.o}
procedure __findfirsti64;external;
{$L x86/libmsvcrt_defs00267.o}
procedure __findnext;external;
{$L x86/libmsvcrt_defs00268.o}
procedure __findnext64;external;
{$L x86/libmsvcrt_defs00269.o}
procedure __findnexti64;external;
{$L x86/libmsvcrt_defs00270.o}
procedure __finite;external;
{$L x86/libmsvcrt_defs00271.o}
procedure _finite;external;
{$L x86/libmsvcrt_defs00272.o}
procedure __findnext32;external;
{$L x86/libmsvcrt_defs00273.o}
procedure __flsbuf;external;
{$L x86/libmsvcrt_defs00274.o}
procedure __flushall;external;
{$L x86/libmsvcrt_defs00275.o}
procedure __fpclass;external;
{$L x86/libmsvcrt_defs00277.o}
procedure __fpieee_flt;external;
{$L x86/libmsvcrt_defs00278.o}
procedure __fprintf_l;external;
{$L x86/libmsvcrt_defs00280.o}
procedure _fpclass;external;
{$L x86/libmsvcrt_defs00281.o}
procedure __fprintf_p;external;
{$L x86/libmsvcrt_defs00282.o}
procedure __fprintf_p_l;external;
{$L x86/libmsvcrt_defs00283.o}
procedure __fprintf_s_l;external;
{$L x86/libmsvcrt_defs00284.o}
procedure _fputchar;external;
{$L x86/libmsvcrt_defs00285.o}
procedure __fputchar;external;
{$L x86/libmsvcrt_defs00286.o}
procedure _fputwchar;external;
{$L x86/libmsvcrt_defs00287.o}
procedure __fputwchar;external;
{$L x86/libmsvcrt_defs00288.o}
procedure __fsopen;external;
{$L x86/libmsvcrt_defs00289.o}
procedure __fstat;external;
{$L x86/libmsvcrt_defs00290.o}
procedure __fstat64;external;
{$L x86/libmsvcrt_defs00291.o}
procedure __fstat32;external;
{$L x86/libmsvcrt_defs00292.o}
procedure __fstati64;external;
{$L x86/libmsvcrt_defs00293.o}
procedure __ftime32;external;
{$L x86/libmsvcrt_defs00294.o}
procedure __ftime;external;
{$L x86/libmsvcrt_defs00295.o}
procedure __ftime32_s;external;
{$L x86/libmsvcrt_defs00296.o}
procedure __ftime64;external;
{$L x86/libmsvcrt_defs00297.o}
procedure __ftime64_s;external;
{$L x86/libmsvcrt_defs00298.o}
procedure __ftol;external;
{$L x86/libmsvcrt_defs00299.o}
procedure __ftime_s;external;
{$L x86/libmsvcrt_defs00300.o}
procedure __fullpath;external;
{$L x86/libmsvcrt_defs00301.o}
procedure __futime64;external;
{$L x86/libmsvcrt_defs00302.o}
procedure __fwprintf_l;external;
{$L x86/libmsvcrt_defs00303.o}
procedure __fwprintf_p;external;
{$L x86/libmsvcrt_defs00304.o}
procedure __fwprintf_p_l;external;
{$L x86/libmsvcrt_defs00305.o}
procedure __futime32;external;
{$L x86/libmsvcrt_defs00306.o}
procedure __fwprintf_s_l;external;
{$L x86/libmsvcrt_defs00307.o}
procedure __fwscanf_l;external;
{$L x86/libmsvcrt_defs00308.o}
procedure __fwscanf_s_l;external;
{$L x86/libmsvcrt_defs00309.o}
procedure __gcvt;external;
{$L x86/libmsvcrt_defs00310.o}
procedure __gcvt_s;external;
{$L x86/libmsvcrt_defs00311.o}
procedure __get_heap_handle;external;
{$L x86/libmsvcrt_defs00312.o}
procedure __get_osfhandle;external;
{$L x86/libmsvcrt_defs00313.o}
procedure __get_sbh_threshold;external;
{$L x86/libmsvcrt_defs00314.o}
procedure _gcvt;external;
{$L x86/libmsvcrt_defs00315.o}
procedure _getch;external;
{$L x86/libmsvcrt_defs00316.o}
procedure __getch;external;
{$L x86/libmsvcrt_defs00317.o}
procedure __getche;external;
{$L x86/libmsvcrt_defs00318.o}
procedure __futime;external;
{$L x86/libmsvcrt_defs00319.o}
procedure _getche;external;
{$L x86/libmsvcrt_defs00320.o}
procedure _getcwd;external;
{$L x86/libmsvcrt_defs00321.o}
procedure __getcwd;external;
{$L x86/libmsvcrt_defs00322.o}
procedure __getdcwd;external;
{$L x86/libmsvcrt_defs00323.o}
procedure __getdiskfree;external;
{$L x86/libmsvcrt_defs00324.o}
procedure __getdllprocaddr;external;
{$L x86/libmsvcrt_defs00325.o}
procedure __getdrive;external;
{$L x86/libmsvcrt_defs00326.o}
procedure __getdrives;external;
{$L x86/libmsvcrt_defs00327.o}
procedure __getmaxstdio;external;
{$L x86/libmsvcrt_defs00328.o}
procedure __getmbcp;external;
{$L x86/libmsvcrt_defs00329.o}
procedure _getpid;external;
{$L x86/libmsvcrt_defs00330.o}
procedure _iscsym;external;
{$L x86/libmsvcrt_defs00331.o}
procedure __getpid;external;
{$L x86/libmsvcrt_defs00332.o}
procedure __getsystime;external;
{$L x86/libmsvcrt_defs00333.o}
procedure _getw;external;
{$L x86/libmsvcrt_defs00334.o}
procedure __getw;external;
{$L x86/libmsvcrt_defs00335.o}
procedure __getwch;external;
{$L x86/libmsvcrt_defs00336.o}
procedure __getwche;external;
{$L x86/libmsvcrt_defs00337.o}
procedure __getws;external;
{$L x86/libmsvcrt_defs00338.o}
procedure __global_unwind2;external;
{$L x86/libmsvcrt_defs00339.o}
procedure __gmtime64;external;
{$L x86/libmsvcrt_defs00340.o}
procedure __heapadd;external;
{$L x86/libmsvcrt_defs00341.o}
procedure __heapchk;external;
{$L x86/libmsvcrt_defs00342.o}
procedure __heapmin;external;
{$L x86/libmsvcrt_defs00343.o}
procedure __heapset;external;
{$L x86/libmsvcrt_defs00344.o}
procedure __heapused;external;
{$L x86/libmsvcrt_defs00345.o}
procedure _heapwalk;external;
{$L x86/libmsvcrt_defs00346.o}
procedure __heapwalk;external;
{$L x86/libmsvcrt_defs00347.o}
procedure _hypot;external;
{$L x86/libmsvcrt_defs00348.o}
procedure __hypot;external;
{$L x86/libmsvcrt_defs00349.o}
procedure __i64toa;external;
{$L x86/libmsvcrt_defs00350.o}
procedure __i64toa_s;external;
{$L x86/libmsvcrt_defs00351.o}
procedure __i64tow;external;
{$L x86/libmsvcrt_defs00352.o}
procedure __i64tow_s;external;
{$L x86/libmsvcrt_defs00353.o}
procedure __initterm;external;
{$L x86/libmsvcrt_defs00354.o}
procedure __inp;external;
{$L x86/libmsvcrt_defs00355.o}
procedure __inpd;external;
{$L x86/libmsvcrt_defs00356.o}
procedure __inpw;external;
{$L x86/libmsvcrt_defs00357.o}
procedure __isalnum_l;external;
{$L x86/libmsvcrt_defs00359.o}
procedure __isalpha_l;external;
{$L x86/libmsvcrt_defs00360.o}
procedure __isatty;external;
{$L x86/libmsvcrt_defs00361.o}
procedure _isatty;external;
{$L x86/libmsvcrt_defs00362.o}
procedure __iscntrl_l;external;
{$L x86/libmsvcrt_defs00363.o}
procedure __isctype;external;
{$L x86/libmsvcrt_defs00364.o}
procedure __isctype_l;external;
{$L x86/libmsvcrt_defs00365.o}
procedure __isdigit_l;external;
{$L x86/libmsvcrt_defs00366.o}
procedure __isgraph_l;external;
{$L x86/libmsvcrt_defs00367.o}
procedure __isleadbyte_l;external;
{$L x86/libmsvcrt_defs00368.o}
procedure __islower_l;external;
{$L x86/libmsvcrt_defs00369.o}
procedure __ismbbalnum;external;
{$L x86/libmsvcrt_defs00370.o}
procedure __ismbbalnum_l;external;
{$L x86/libmsvcrt_defs00371.o}
procedure __ismbbalpha;external;
{$L x86/libmsvcrt_defs00372.o}
procedure __ismbbalpha_l;external;
{$L x86/libmsvcrt_defs00373.o}
procedure __ismbbgraph;external;
{$L x86/libmsvcrt_defs00374.o}
procedure __ismbbgraph_l;external;
{$L x86/libmsvcrt_defs00375.o}
procedure __ismbbkalnum;external;
{$L x86/libmsvcrt_defs00376.o}
procedure __ismbbkalnum_l;external;
{$L x86/libmsvcrt_defs00377.o}
procedure __ismbbkana;external;
{$L x86/libmsvcrt_defs00378.o}
procedure __ismbbkana_l;external;
{$L x86/libmsvcrt_defs00379.o}
procedure __ismbbkprint;external;
{$L x86/libmsvcrt_defs00380.o}
procedure __ismbbkprint_l;external;
{$L x86/libmsvcrt_defs00381.o}
procedure __ismbbkpunct;external;
{$L x86/libmsvcrt_defs00382.o}
procedure __ismbbkpunct_l;external;
{$L x86/libmsvcrt_defs00383.o}
procedure __ismbblead;external;
{$L x86/libmsvcrt_defs00384.o}
procedure __ismbblead_l;external;
{$L x86/libmsvcrt_defs00385.o}
procedure __ismbbprint;external;
{$L x86/libmsvcrt_defs00386.o}
procedure __ismbbprint_l;external;
{$L x86/libmsvcrt_defs00387.o}
procedure __ismbbpunct;external;
{$L x86/libmsvcrt_defs00388.o}
procedure __ismbbpunct_l;external;
{$L x86/libmsvcrt_defs00389.o}
procedure __ismbbtrail;external;
{$L x86/libmsvcrt_defs00390.o}
procedure __ismbbtrail_l;external;
{$L x86/libmsvcrt_defs00391.o}
procedure __ismbcalnum;external;
{$L x86/libmsvcrt_defs00392.o}
procedure __ismbcalnum_l;external;
{$L x86/libmsvcrt_defs00393.o}
procedure __ismbcalpha;external;
{$L x86/libmsvcrt_defs00394.o}
procedure __ismbcalpha_l;external;
{$L x86/libmsvcrt_defs00395.o}
procedure __ismbcdigit;external;
{$L x86/libmsvcrt_defs00396.o}
procedure __ismbcdigit_l;external;
{$L x86/libmsvcrt_defs00397.o}
procedure __ismbcgraph;external;
{$L x86/libmsvcrt_defs00398.o}
procedure __ismbcgraph_l;external;
{$L x86/libmsvcrt_defs00399.o}
procedure __ismbchira;external;
{$L x86/libmsvcrt_defs00400.o}
procedure __ismbchira_l;external;
{$L x86/libmsvcrt_defs00401.o}
procedure __ismbckata;external;
{$L x86/libmsvcrt_defs00402.o}
procedure __ismbckata_l;external;
{$L x86/libmsvcrt_defs00403.o}
procedure __ismbcl0;external;
{$L x86/libmsvcrt_defs00404.o}
procedure __ismbcl0_l;external;
{$L x86/libmsvcrt_defs00405.o}
procedure __ismbcl1;external;
{$L x86/libmsvcrt_defs00406.o}
procedure __ismbcl1_l;external;
{$L x86/libmsvcrt_defs00407.o}
procedure __ismbcl2;external;
{$L x86/libmsvcrt_defs00408.o}
procedure __ismbcl2_l;external;
{$L x86/libmsvcrt_defs00409.o}
procedure __ismbclegal;external;
{$L x86/libmsvcrt_defs00410.o}
procedure __ismbclegal_l;external;
{$L x86/libmsvcrt_defs00411.o}
procedure __ismbclower;external;
{$L x86/libmsvcrt_defs00412.o}
procedure __ismbclower_l;external;
{$L x86/libmsvcrt_defs00413.o}
procedure __ismbcprint;external;
{$L x86/libmsvcrt_defs00414.o}
procedure __ismbcprint_l;external;
{$L x86/libmsvcrt_defs00415.o}
procedure __ismbcpunct;external;
{$L x86/libmsvcrt_defs00416.o}
procedure __ismbcpunct_l;external;
{$L x86/libmsvcrt_defs00417.o}
procedure __ismbcspace;external;
{$L x86/libmsvcrt_defs00418.o}
procedure __ismbcspace_l;external;
{$L x86/libmsvcrt_defs00419.o}
procedure __ismbcsymbol;external;
{$L x86/libmsvcrt_defs00420.o}
procedure __ismbcsymbol_l;external;
{$L x86/libmsvcrt_defs00421.o}
procedure __ismbcupper;external;
{$L x86/libmsvcrt_defs00422.o}
procedure __ismbcupper_l;external;
{$L x86/libmsvcrt_defs00423.o}
procedure __ismbslead;external;
{$L x86/libmsvcrt_defs00424.o}
procedure __ismbslead_l;external;
{$L x86/libmsvcrt_defs00425.o}
procedure __ismbstrail;external;
{$L x86/libmsvcrt_defs00426.o}
procedure __ismbstrail_l;external;
{$L x86/libmsvcrt_defs00427.o}
procedure __isnan;external;
{$L x86/libmsvcrt_defs00428.o}
procedure __isprint_l;external;
{$L x86/libmsvcrt_defs00429.o}
procedure __isspace_l;external;
{$L x86/libmsvcrt_defs00430.o}
procedure __isupper_l;external;
{$L x86/libmsvcrt_defs00431.o}
procedure __iswalnum_l;external;
{$L x86/libmsvcrt_defs00432.o}
procedure __iswalpha_l;external;
{$L x86/libmsvcrt_defs00433.o}
procedure __iswcntrl_l;external;
{$L x86/libmsvcrt_defs00434.o}
procedure __iswctype_l;external;
{$L x86/libmsvcrt_defs00435.o}
procedure __iswdigit_l;external;
{$L x86/libmsvcrt_defs00436.o}
procedure __iswgraph_l;external;
{$L x86/libmsvcrt_defs00437.o}
procedure __iswlower_l;external;
{$L x86/libmsvcrt_defs00438.o}
procedure __iswprint_l;external;
{$L x86/libmsvcrt_defs00439.o}
procedure __iswpunct_l;external;
{$L x86/libmsvcrt_defs00440.o}
procedure __iswspace_l;external;
{$L x86/libmsvcrt_defs00441.o}
procedure __iswupper_l;external;
{$L x86/libmsvcrt_defs00442.o}
procedure __iswxdigit_l;external;
{$L x86/libmsvcrt_defs00443.o}
procedure __isxdigit_l;external;
{$L x86/libmsvcrt_defs00444.o}
procedure __itoa;external;
{$L x86/libmsvcrt_defs00445.o}
procedure _itoa;external;
{$L x86/libmsvcrt_defs00446.o}
procedure __itoa_s;external;
{$L x86/libmsvcrt_defs00447.o}
procedure __itow;external;
{$L x86/libmsvcrt_defs00448.o}
procedure __itow_s;external;
{$L x86/libmsvcrt_defs00449.o}
procedure __j0;external;
{$L x86/libmsvcrt_defs00450.o}
procedure __j1;external;
{$L x86/libmsvcrt_defs00451.o}
procedure _j1;external;
{$L x86/libmsvcrt_defs00452.o}
procedure _j0;external;
{$L x86/libmsvcrt_defs00453.o}
procedure __jn;external;
{$L x86/libmsvcrt_defs00454.o}
procedure __kbhit;external;
{$L x86/libmsvcrt_defs00455.o}
procedure __lfind;external;
{$L x86/libmsvcrt_defs00456.o}
procedure _lfind;external;
{$L x86/libmsvcrt_defs00457.o}
procedure __loaddll;external;
{$L x86/libmsvcrt_defs00458.o}
procedure __local_unwind2;external;
{$L x86/libmsvcrt_defs00459.o}
procedure _jn;external;
{$L x86/libmsvcrt_defs00460.o}
procedure _kbhit;external;
{$L x86/libmsvcrt_defs00461.o}
procedure __localtime64;external;
{$L x86/libmsvcrt_defs00462.o}
procedure __lock;external;
{$L x86/libmsvcrt_defs00463.o}
procedure __locking;external;
{$L x86/libmsvcrt_defs00464.o}
procedure __logb;external;
{$L x86/libmsvcrt_defs00465.o}
procedure __longjmpex;external;
{$L x86/libmsvcrt_defs00466.o}
procedure __lrotl;external;
{$L x86/libmsvcrt_defs00467.o}
procedure __lrotr;external;
{$L x86/libmsvcrt_defs00468.o}
procedure _lsearch;external;
{$L x86/libmsvcrt_defs00469.o}
procedure __lsearch;external;
{$L x86/libmsvcrt_defs00470.o}
procedure __lseek;external;
{$L x86/libmsvcrt_defs00471.o}
procedure __lseeki64;external;
{$L x86/libmsvcrt_defs00472.o}
procedure _ltoa;external;
{$L x86/libmsvcrt_defs00473.o}
procedure _lseek;external;
{$L x86/libmsvcrt_defs00474.o}
procedure __ltoa;external;
{$L x86/libmsvcrt_defs00475.o}
procedure __ltow;external;
{$L x86/libmsvcrt_defs00476.o}
procedure __makepath;external;
{$L x86/libmsvcrt_defs00477.o}
procedure __makepath_s;external;
{$L x86/libmsvcrt_defs00478.o}
procedure __mbbtombc;external;
{$L x86/libmsvcrt_defs00479.o}
procedure __mbbtombc_l;external;
{$L x86/libmsvcrt_defs00480.o}
procedure __mbbtype;external;
{$L x86/libmsvcrt_defs00481.o}
procedure __mbcasemap;external;
{$L x86/libmsvcrt_defs00482.o}
procedure __mbccpy;external;
{$L x86/libmsvcrt_defs00483.o}
procedure __mbccpy_l;external;
{$L x86/libmsvcrt_defs00484.o}
procedure __mbccpy_s;external;
{$L x86/libmsvcrt_defs00485.o}
procedure __mbccpy_s_l;external;
{$L x86/libmsvcrt_defs00486.o}
procedure __mbcjistojms;external;
{$L x86/libmsvcrt_defs00487.o}
procedure __mbcjistojms_l;external;
{$L x86/libmsvcrt_defs00488.o}
procedure __mbcjmstojis;external;
{$L x86/libmsvcrt_defs00489.o}
procedure __mbcjmstojis_l;external;
{$L x86/libmsvcrt_defs00490.o}
procedure __mbclen;external;
{$L x86/libmsvcrt_defs00491.o}
procedure __mbclen_l;external;
{$L x86/libmsvcrt_defs00492.o}
procedure __mbctohira;external;
{$L x86/libmsvcrt_defs00493.o}
procedure __mbctohira_l;external;
{$L x86/libmsvcrt_defs00494.o}
procedure __mbctokata;external;
{$L x86/libmsvcrt_defs00495.o}
procedure __mbctokata_l;external;
{$L x86/libmsvcrt_defs00496.o}
procedure __mbctolower;external;
{$L x86/libmsvcrt_defs00497.o}
procedure __mbctolower_l;external;
{$L x86/libmsvcrt_defs00498.o}
procedure __mbctombb;external;
{$L x86/libmsvcrt_defs00499.o}
procedure __mbctombb_l;external;
{$L x86/libmsvcrt_defs00500.o}
procedure __mbctoupper;external;
{$L x86/libmsvcrt_defs00501.o}
procedure __mbctoupper_l;external;
{$L x86/libmsvcrt_defs00502.o}
procedure __mblen_l;external;
{$L x86/libmsvcrt_defs00504.o}
procedure __mbsbtype;external;
{$L x86/libmsvcrt_defs00505.o}
procedure __mbsbtype_l;external;
{$L x86/libmsvcrt_defs00506.o}
procedure __mbscat;external;
{$L x86/libmsvcrt_defs00507.o}
procedure __mbscat_s;external;
{$L x86/libmsvcrt_defs00508.o}
procedure __mbscat_s_l;external;
{$L x86/libmsvcrt_defs00509.o}
procedure __mbschr;external;
{$L x86/libmsvcrt_defs00510.o}
procedure __mbschr_l;external;
{$L x86/libmsvcrt_defs00511.o}
procedure __mbscmp;external;
{$L x86/libmsvcrt_defs00512.o}
procedure __mbscmp_l;external;
{$L x86/libmsvcrt_defs00513.o}
procedure __mbscoll;external;
{$L x86/libmsvcrt_defs00514.o}
procedure __mbscoll_l;external;
{$L x86/libmsvcrt_defs00515.o}
procedure __mbscpy;external;
{$L x86/libmsvcrt_defs00516.o}
procedure __mbscpy_s;external;
{$L x86/libmsvcrt_defs00517.o}
procedure __mbscpy_s_l;external;
{$L x86/libmsvcrt_defs00518.o}
procedure __mbscspn;external;
{$L x86/libmsvcrt_defs00519.o}
procedure __mbscspn_l;external;
{$L x86/libmsvcrt_defs00520.o}
procedure __mbsdec;external;
{$L x86/libmsvcrt_defs00521.o}
procedure __mbsdec_l;external;
{$L x86/libmsvcrt_defs00522.o}
procedure __mbsdup;external;
{$L x86/libmsvcrt_defs00523.o}
procedure __mbsicmp;external;
{$L x86/libmsvcrt_defs00524.o}
procedure __mbsicmp_l;external;
{$L x86/libmsvcrt_defs00525.o}
procedure __mbsicoll;external;
{$L x86/libmsvcrt_defs00526.o}
procedure __mbsicoll_l;external;
{$L x86/libmsvcrt_defs00527.o}
procedure __mbsinc;external;
{$L x86/libmsvcrt_defs00528.o}
procedure __mbsinc_l;external;
{$L x86/libmsvcrt_defs00529.o}
procedure __mbslen;external;
{$L x86/libmsvcrt_defs00530.o}
procedure __mbslen_l;external;
{$L x86/libmsvcrt_defs00531.o}
procedure __mbslwr;external;
{$L x86/libmsvcrt_defs00532.o}
procedure __mbslwr_l;external;
{$L x86/libmsvcrt_defs00533.o}
procedure __mbslwr_s;external;
{$L x86/libmsvcrt_defs00534.o}
procedure __mbslwr_s_l;external;
{$L x86/libmsvcrt_defs00535.o}
procedure __mbsnbcat;external;
{$L x86/libmsvcrt_defs00536.o}
procedure __mbsnbcat_l;external;
{$L x86/libmsvcrt_defs00537.o}
procedure __mbsnbcat_s;external;
{$L x86/libmsvcrt_defs00538.o}
procedure __mbsnbcat_s_l;external;
{$L x86/libmsvcrt_defs00539.o}
procedure __mbsnbcmp;external;
{$L x86/libmsvcrt_defs00540.o}
procedure __mbsnbcmp_l;external;
{$L x86/libmsvcrt_defs00541.o}
procedure __mbsnbcnt;external;
{$L x86/libmsvcrt_defs00542.o}
procedure __mbsnbcnt_l;external;
{$L x86/libmsvcrt_defs00543.o}
procedure __mbsnbcoll;external;
{$L x86/libmsvcrt_defs00544.o}
procedure __mbsnbcoll_l;external;
{$L x86/libmsvcrt_defs00545.o}
procedure __mbsnbcpy;external;
{$L x86/libmsvcrt_defs00546.o}
procedure __mbsnbcpy_l;external;
{$L x86/libmsvcrt_defs00547.o}
procedure __mbsnbcpy_s;external;
{$L x86/libmsvcrt_defs00548.o}
procedure __mbsnbcpy_s_l;external;
{$L x86/libmsvcrt_defs00549.o}
procedure __mbsnbicmp;external;
{$L x86/libmsvcrt_defs00550.o}
procedure __mbsnbicmp_l;external;
{$L x86/libmsvcrt_defs00551.o}
procedure __mbsnbicoll;external;
{$L x86/libmsvcrt_defs00552.o}
procedure __mbsnbicoll_l;external;
{$L x86/libmsvcrt_defs00553.o}
procedure __mbsnbset;external;
{$L x86/libmsvcrt_defs00554.o}
procedure __mbsnbset_l;external;
{$L x86/libmsvcrt_defs00555.o}
procedure __mbsnbset_s;external;
{$L x86/libmsvcrt_defs00556.o}
procedure __mbsnbset_s_l;external;
{$L x86/libmsvcrt_defs00557.o}
procedure __mbsncat;external;
{$L x86/libmsvcrt_defs00558.o}
procedure __mbsncat_l;external;
{$L x86/libmsvcrt_defs00559.o}
procedure __mbsncat_s;external;
{$L x86/libmsvcrt_defs00560.o}
procedure __mbsncat_s_l;external;
{$L x86/libmsvcrt_defs00561.o}
procedure __mbsnccnt;external;
{$L x86/libmsvcrt_defs00562.o}
procedure __mbsnccnt_l;external;
{$L x86/libmsvcrt_defs00563.o}
procedure __mbsncmp;external;
{$L x86/libmsvcrt_defs00564.o}
procedure __mbsncmp_l;external;
{$L x86/libmsvcrt_defs00565.o}
procedure __mbsncoll;external;
{$L x86/libmsvcrt_defs00566.o}
procedure __mbsncoll_l;external;
{$L x86/libmsvcrt_defs00567.o}
procedure __mbsncpy;external;
{$L x86/libmsvcrt_defs00568.o}
procedure __mbsncpy_l;external;
{$L x86/libmsvcrt_defs00569.o}
procedure __mbsncpy_s;external;
{$L x86/libmsvcrt_defs00570.o}
procedure __mbsncpy_s_l;external;
{$L x86/libmsvcrt_defs00571.o}
procedure __mbsnextc;external;
{$L x86/libmsvcrt_defs00572.o}
procedure __mbsnextc_l;external;
{$L x86/libmsvcrt_defs00573.o}
procedure __mbsnicmp;external;
{$L x86/libmsvcrt_defs00574.o}
procedure __mbsnicmp_l;external;
{$L x86/libmsvcrt_defs00575.o}
procedure __mbsnicoll;external;
{$L x86/libmsvcrt_defs00576.o}
procedure __mbsnicoll_l;external;
{$L x86/libmsvcrt_defs00577.o}
procedure __mbsninc;external;
{$L x86/libmsvcrt_defs00578.o}
procedure __mbsninc_l;external;
{$L x86/libmsvcrt_defs00579.o}
procedure __mbsnlen;external;
{$L x86/libmsvcrt_defs00580.o}
procedure __mbsnlen_l;external;
{$L x86/libmsvcrt_defs00581.o}
procedure __mbsnset;external;
{$L x86/libmsvcrt_defs00582.o}
procedure __mbsnset_l;external;
{$L x86/libmsvcrt_defs00583.o}
procedure __mbsnset_s;external;
{$L x86/libmsvcrt_defs00584.o}
procedure __mbsnset_s_l;external;
{$L x86/libmsvcrt_defs00585.o}
procedure __mbspbrk;external;
{$L x86/libmsvcrt_defs00586.o}
procedure __mbspbrk_l;external;
{$L x86/libmsvcrt_defs00587.o}
procedure __mbsrchr;external;
{$L x86/libmsvcrt_defs00588.o}
procedure __mbsrchr_l;external;
{$L x86/libmsvcrt_defs00589.o}
procedure __mbsrev;external;
{$L x86/libmsvcrt_defs00590.o}
procedure __mbsrev_l;external;
{$L x86/libmsvcrt_defs00591.o}
procedure __mbsset;external;
{$L x86/libmsvcrt_defs00592.o}
procedure __mbsset_l;external;
{$L x86/libmsvcrt_defs00593.o}
procedure __mbsset_s;external;
{$L x86/libmsvcrt_defs00594.o}
procedure __mbsset_s_l;external;
{$L x86/libmsvcrt_defs00595.o}
procedure __mbsspn;external;
{$L x86/libmsvcrt_defs00596.o}
procedure __mbsspn_l;external;
{$L x86/libmsvcrt_defs00597.o}
procedure __mbsspnp;external;
{$L x86/libmsvcrt_defs00598.o}
procedure __mbsspnp_l;external;
{$L x86/libmsvcrt_defs00599.o}
procedure __mbsstr;external;
{$L x86/libmsvcrt_defs00600.o}
procedure __mbsstr_l;external;
{$L x86/libmsvcrt_defs00601.o}
procedure __mbstok;external;
{$L x86/libmsvcrt_defs00602.o}
procedure __mbstok_l;external;
{$L x86/libmsvcrt_defs00603.o}
procedure __mbstok_s;external;
{$L x86/libmsvcrt_defs00604.o}
procedure __mbstok_s_l;external;
{$L x86/libmsvcrt_defs00605.o}
procedure __mbstowcs_l;external;
{$L x86/libmsvcrt_defs00606.o}
procedure __mbstowcs_s_l;external;
{$L x86/libmsvcrt_defs00607.o}
procedure __mbstrlen;external;
{$L x86/libmsvcrt_defs00608.o}
procedure __mbstrlen_l;external;
{$L x86/libmsvcrt_defs00609.o}
procedure __mbstrnlen;external;
{$L x86/libmsvcrt_defs00610.o}
procedure __mbstrnlen_l;external;
{$L x86/libmsvcrt_defs00611.o}
procedure __mbsupr;external;
{$L x86/libmsvcrt_defs00612.o}
procedure __mbsupr_l;external;
{$L x86/libmsvcrt_defs00613.o}
procedure __mbsupr_s;external;
{$L x86/libmsvcrt_defs00614.o}
procedure __mbsupr_s_l;external;
{$L x86/libmsvcrt_defs00615.o}
procedure __mbtowc_l;external;
{$L x86/libmsvcrt_defs00616.o}
procedure __memccpy;external;
{$L x86/libmsvcrt_defs00617.o}
procedure __memicmp;external;
{$L x86/libmsvcrt_defs00618.o}
procedure __memicmp_l;external;
{$L x86/libmsvcrt_defs00619.o}
procedure _mkdir;external;
{$L x86/libmsvcrt_defs00620.o}
procedure __mkdir;external;
{$L x86/libmsvcrt_defs00621.o}
procedure __mkgmtime;external;
{$L x86/libmsvcrt_defs00622.o}
procedure __mkgmtime32;external;
{$L x86/libmsvcrt_defs00623.o}
procedure __mkgmtime64;external;
{$L x86/libmsvcrt_defs00624.o}
procedure _memicmp;external;
{$L x86/libmsvcrt_defs00625.o}
procedure _mktemp;external;
{$L x86/libmsvcrt_defs00626.o}
procedure __mktemp;external;
{$L x86/libmsvcrt_defs00627.o}
procedure __mktime64;external;
{$L x86/libmsvcrt_defs00628.o}
procedure __msize;external;
{$L x86/libmsvcrt_defs00629.o}
procedure __nextafter;external;
{$L x86/libmsvcrt_defs00630.o}
procedure __onexit;external;
{$L x86/libmsvcrt_defs00631.o}
procedure _open;external;
{$L x86/libmsvcrt_defs00632.o}
procedure __open;external;
{$L x86/libmsvcrt_defs00633.o}
procedure __open_osfhandle;external;
{$L x86/libmsvcrt_defs00634.o}
procedure _nextafter;external;
{$L x86/libmsvcrt_defs00637.o}
procedure __outp;external;
{$L x86/libmsvcrt_defs00638.o}
procedure __outpd;external;
{$L x86/libmsvcrt_defs00639.o}
procedure __outpw;external;
{$L x86/libmsvcrt_defs00640.o}
procedure _pclose;external;
{$L x86/libmsvcrt_defs00641.o}
procedure __pclose;external;
{$L x86/libmsvcrt_defs00642.o}
procedure __pipe;external;
{$L x86/libmsvcrt_defs00645.o}
procedure __popen;external;
{$L x86/libmsvcrt_defs00646.o}
procedure _popen;external;
{$L x86/libmsvcrt_defs00647.o}
procedure __printf_l;external;
{$L x86/libmsvcrt_defs00648.o}
procedure __printf_p;external;
{$L x86/libmsvcrt_defs00649.o}
procedure __printf_p_l;external;
{$L x86/libmsvcrt_defs00650.o}
procedure __printf_s_l;external;
{$L x86/libmsvcrt_defs00651.o}
procedure __purecall;external;
{$L x86/libmsvcrt_defs00652.o}
procedure __putch;external;
{$L x86/libmsvcrt_defs00653.o}
procedure _putenv;external;
{$L x86/libmsvcrt_defs00654.o}
procedure __putenv;external;
{$L x86/libmsvcrt_defs00655.o}
procedure __putenv_s;external;
{$L x86/libmsvcrt_defs00656.o}
procedure _putw;external;
{$L x86/libmsvcrt_defs00657.o}
procedure _putch;external;
{$L x86/libmsvcrt_defs00658.o}
procedure __putw;external;
{$L x86/libmsvcrt_defs00659.o}
procedure __putwch;external;
{$L x86/libmsvcrt_defs00660.o}
procedure __putws;external;
{$L x86/libmsvcrt_defs00661.o}
procedure _memccpy;external;
{$L x86/libmsvcrt_defs00662.o}
procedure __read;external;
{$L x86/libmsvcrt_defs00664.o}
procedure __resetstkoflw;external;
{$L x86/libmsvcrt_defs00665.o}
procedure _rmdir;external;
{$L x86/libmsvcrt_defs00666.o}
procedure __rmdir;external;
{$L x86/libmsvcrt_defs00667.o}
procedure _rmtmp;external;
{$L x86/libmsvcrt_defs00668.o}
procedure __rmtmp;external;
{$L x86/libmsvcrt_defs00669.o}
procedure __rotl;external;
{$L x86/libmsvcrt_defs00670.o}
procedure __rotr;external;
{$L x86/libmsvcrt_defs00671.o}
procedure __safe_fdiv;external;
{$L x86/libmsvcrt_defs00672.o}
procedure __safe_fdivr;external;
{$L x86/libmsvcrt_defs00673.o}
procedure __safe_fprem;external;
{$L x86/libmsvcrt_defs00674.o}
procedure __safe_fprem1;external;
{$L x86/libmsvcrt_defs00675.o}
procedure __scalb;external;
{$L x86/libmsvcrt_defs00676.o}
procedure __scanf_l;external;
{$L x86/libmsvcrt_defs00677.o}
procedure __scanf_s_l;external;
{$L x86/libmsvcrt_defs00678.o}
procedure __scprintf_l;external;
{$L x86/libmsvcrt_defs00679.o}
procedure __scprintf_p_l;external;
{$L x86/libmsvcrt_defs00680.o}
procedure __scwprintf;external;
{$L x86/libmsvcrt_defs00681.o}
procedure __scwprintf_l;external;
{$L x86/libmsvcrt_defs00682.o}
procedure __scwprintf_p_l;external;
{$L x86/libmsvcrt_defs00683.o}
procedure _searchenv;external;
{$L x86/libmsvcrt_defs00684.o}
procedure __searchenv;external;
{$L x86/libmsvcrt_defs00685.o}
procedure __searchenv_s;external;
{$L x86/libmsvcrt_defs00686.o}
procedure __seh_longjmp_unwind;external;
{$L x86/libmsvcrt_defs00687.o}
procedure __set_SSE2_enable;external;
{$L x86/libmsvcrt_defs00688.o}
procedure __set_error_mode;external;
{$L x86/libmsvcrt_defs00689.o}
procedure __set_sbh_threshold;external;
{$L x86/libmsvcrt_defs00690.o}
procedure __set_security_error_handler;external;
{$L x86/libmsvcrt_defs00691.o}
procedure __seterrormode;external;
{$L x86/libmsvcrt_defs00692.o}
procedure __setjmp;external;
{$L x86/libmsvcrt_defs00693.o}
procedure __setjmp3;external;
{$L x86/libmsvcrt_defs00694.o}
procedure __setmaxstdio;external;
{$L x86/libmsvcrt_defs00695.o}
procedure __setmbcp;external;
{$L x86/libmsvcrt_defs00696.o}
procedure __setmode;external;
{$L x86/libmsvcrt_defs00697.o}
procedure __setsystime;external;
{$L x86/libmsvcrt_defs00698.o}
procedure __sleep;external;
{$L x86/libmsvcrt_defs00699.o}
procedure _setmode;external;
{$L x86/libmsvcrt_defs00700.o}
procedure __snprintf;external;
{$L x86/libmsvcrt_defs00701.o}
procedure __snprintf_c;external;
{$L x86/libmsvcrt_defs00702.o}
procedure __snprintf_c_l;external;
{$L x86/libmsvcrt_defs00703.o}
procedure __snprintf_l;external;
{$L x86/libmsvcrt_defs00704.o}
procedure __snprintf_s;external;
{$L x86/libmsvcrt_defs00705.o}
procedure __snprintf_s_l;external;
{$L x86/libmsvcrt_defs00706.o}
procedure __snscanf;external;
{$L x86/libmsvcrt_defs00707.o}
procedure __snscanf_l;external;
{$L x86/libmsvcrt_defs00708.o}
procedure __snscanf_s;external;
{$L x86/libmsvcrt_defs00709.o}
procedure __snscanf_s_l;external;
{$L x86/libmsvcrt_defs00710.o}
procedure _snwprintf;external;
{$L x86/libmsvcrt_defs00711.o}
procedure __snwprintf;external;
{$L x86/libmsvcrt_defs00712.o}
procedure __snwprintf_l;external;
{$L x86/libmsvcrt_defs00713.o}
procedure __snwprintf_s;external;
{$L x86/libmsvcrt_defs00714.o}
procedure __snwprintf_s_l;external;
{$L x86/libmsvcrt_defs00715.o}
procedure __snwscanf;external;
{$L x86/libmsvcrt_defs00716.o}
procedure __snwscanf_l;external;
{$L x86/libmsvcrt_defs00717.o}
procedure __snwscanf_s;external;
{$L x86/libmsvcrt_defs00718.o}
procedure __snwscanf_s_l;external;
{$L x86/libmsvcrt_defs00719.o}
procedure __sopen;external;
{$L x86/libmsvcrt_defs00720.o}
procedure __spawnl;external;
{$L x86/libmsvcrt_defs00721.o}
procedure __spawnle;external;
{$L x86/libmsvcrt_defs00722.o}
procedure __spawnlp;external;
{$L x86/libmsvcrt_defs00723.o}
procedure _spawnle;external;
{$L x86/libmsvcrt_defs00724.o}
procedure __spawnlpe;external;
{$L x86/libmsvcrt_defs00725.o}
procedure _spawnlpe;external;
{$L x86/libmsvcrt_defs00726.o}
procedure _spawnv;external;
{$L x86/libmsvcrt_defs00727.o}
procedure _spawnlp;external;
{$L x86/libmsvcrt_defs00728.o}
procedure __spawnv;external;
{$L x86/libmsvcrt_defs00729.o}
procedure _spawnl;external;
{$L x86/libmsvcrt_defs00730.o}
procedure _sopen;external;
{$L x86/libmsvcrt_defs00731.o}
procedure __spawnve;external;
{$L x86/libmsvcrt_defs00732.o}
procedure _spawnvp;external;
{$L x86/libmsvcrt_defs00733.o}
procedure _spawnve;external;
{$L x86/libmsvcrt_defs00734.o}
procedure __spawnvp;external;
{$L x86/libmsvcrt_defs00735.o}
procedure __spawnvpe;external;
{$L x86/libmsvcrt_defs00736.o}
procedure __splitpath;external;
{$L x86/libmsvcrt_defs00737.o}
procedure __splitpath_s;external;
{$L x86/libmsvcrt_defs00738.o}
procedure __sprintf_l;external;
{$L x86/libmsvcrt_defs00739.o}
procedure __sprintf_p_l;external;
{$L x86/libmsvcrt_defs00740.o}
procedure __sprintf_s_l;external;
{$L x86/libmsvcrt_defs00741.o}
procedure __sscanf_l;external;
{$L x86/libmsvcrt_defs00742.o}
procedure __sscanf_s_l;external;
{$L x86/libmsvcrt_defs00743.o}
procedure __stat32;external;
{$L x86/libmsvcrt_defs00744.o}
procedure __stat;external;
{$L x86/libmsvcrt_defs00745.o}
procedure __stat64;external;
{$L x86/libmsvcrt_defs00746.o}
procedure _spawnvpe;external;
{$L x86/libmsvcrt_defs00747.o}
procedure __stati64;external;
{$L x86/libmsvcrt_defs00748.o}
procedure _read;external;
{$L x86/libmsvcrt_defs00749.o}
procedure __statusfp;external;
{$L x86/libmsvcrt_defs00750.o}
procedure _strcmpi;external;
{$L x86/libmsvcrt_defs00751.o}
procedure __strcmpi;external;
{$L x86/libmsvcrt_defs00752.o}
procedure __strcoll_l;external;
{$L x86/libmsvcrt_defs00753.o}
procedure __strdate;external;
{$L x86/libmsvcrt_defs00754.o}
procedure _strdup;external;
{$L x86/libmsvcrt_defs00755.o}
procedure __strdup;external;
{$L x86/libmsvcrt_defs00756.o}
procedure __strerror;external;
{$L x86/libmsvcrt_defs00757.o}
procedure __strerror_s;external;
{$L x86/libmsvcrt_defs00758.o}
procedure _stricmp;external;
{$L x86/libmsvcrt_defs00759.o}
procedure __stricmp;external;
{$L x86/libmsvcrt_defs00760.o}
procedure __stricmp_l;external;
{$L x86/libmsvcrt_defs00761.o}
procedure _strcasecmp;external;
{$L x86/libmsvcrt_defs00762.o}
procedure __stricoll;external;
{$L x86/libmsvcrt_defs00763.o}
procedure __stricoll_l;external;
{$L x86/libmsvcrt_defs00764.o}
procedure __strlwr;external;
{$L x86/libmsvcrt_defs00765.o}
procedure _strlwr;external;
{$L x86/libmsvcrt_defs00766.o}
procedure __strlwr_l;external;
{$L x86/libmsvcrt_defs00767.o}
procedure __strlwr_s;external;
{$L x86/libmsvcrt_defs00768.o}
procedure __strlwr_s_l;external;
{$L x86/libmsvcrt_defs00769.o}
procedure __strncoll;external;
{$L x86/libmsvcrt_defs00770.o}
procedure __strncoll_l;external;
{$L x86/libmsvcrt_defs00771.o}
procedure _strnicmp;external;
{$L x86/libmsvcrt_defs00772.o}
procedure __strnicmp;external;
{$L x86/libmsvcrt_defs00773.o}
procedure _strncasecmp;external;
{$L x86/libmsvcrt_defs00774.o}
procedure __strnicmp_l;external;
{$L x86/libmsvcrt_defs00775.o}
procedure __strnicoll;external;
{$L x86/libmsvcrt_defs00776.o}
procedure __strnicoll_l;external;
{$L x86/libmsvcrt_defs00777.o}
procedure __strnset;external;
{$L x86/libmsvcrt_defs00778.o}
procedure __strnset_s;external;
{$L x86/libmsvcrt_defs00779.o}
procedure _strrev;external;
{$L x86/libmsvcrt_defs00780.o}
procedure __strrev;external;
{$L x86/libmsvcrt_defs00781.o}
procedure _strset;external;
{$L x86/libmsvcrt_defs00782.o}
procedure __strset;external;
{$L x86/libmsvcrt_defs00783.o}
procedure __strset_s;external;
{$L x86/libmsvcrt_defs00784.o}
procedure _strnset;external;
{$L x86/libmsvcrt_defs00785.o}
procedure _stricoll;external;
{$L x86/libmsvcrt_defs00786.o}
procedure __strtime;external;
{$L x86/libmsvcrt_defs00787.o}
procedure __strtod_l;external;
{$L x86/libmsvcrt_defs00788.o}
procedure __strtoi64;external;
{$L x86/libmsvcrt_defs00789.o}
procedure __strtoi64_l;external;
{$L x86/libmsvcrt_defs00790.o}
procedure __strtol_l;external;
{$L x86/libmsvcrt_defs00791.o}
procedure __strtoui64;external;
{$L x86/libmsvcrt_defs00792.o}
procedure __strtoui64_l;external;
{$L x86/libmsvcrt_defs00793.o}
procedure __strtoul_l;external;
{$L x86/libmsvcrt_defs00794.o}
procedure __strupr;external;
{$L x86/libmsvcrt_defs00795.o}
procedure _strupr;external;
{$L x86/libmsvcrt_defs00796.o}
procedure __strupr_l;external;
{$L x86/libmsvcrt_defs00797.o}
procedure __strupr_s;external;
{$L x86/libmsvcrt_defs00798.o}
procedure __strupr_s_l;external;
{$L x86/libmsvcrt_defs00799.o}
procedure __strxfrm_l;external;
{$L x86/libmsvcrt_defs00800.o}
procedure _swab;external;
{$L x86/libmsvcrt_defs00801.o}
procedure __swab;external;
{$L x86/libmsvcrt_defs00802.o}
procedure __swprintf_c_l;external;
{$L x86/libmsvcrt_defs00803.o}
procedure __swprintf_p_l;external;
{$L x86/libmsvcrt_defs00804.o}
procedure __swprintf_s_l;external;
{$L x86/libmsvcrt_defs00805.o}
procedure __swscanf_l;external;
{$L x86/libmsvcrt_defs00806.o}
procedure __swscanf_s_l;external;
{$L x86/libmsvcrt_defs00807.o}
procedure _tell;external;
{$L x86/libmsvcrt_defs00810.o}
procedure __tell;external;
{$L x86/libmsvcrt_defs00811.o}
procedure __telli64;external;
{$L x86/libmsvcrt_defs00812.o}
procedure __tempnam;external;
{$L x86/libmsvcrt_defs00813.o}
procedure __time64;external;
{$L x86/libmsvcrt_defs00814.o}
procedure _tempnam;external;
{$L x86/libmsvcrt_defs00816.o}
procedure _timezone;external;
{$L x86/libmsvcrt_defs00817.o}
procedure __tolower;external;
{$L x86/libmsvcrt_defs00818.o}
procedure __tolower_l;external;
{$L x86/libmsvcrt_defs00819.o}
procedure __toupper;external;
{$L x86/libmsvcrt_defs00820.o}
procedure __toupper_l;external;
{$L x86/libmsvcrt_defs00821.o}
procedure __towlower_l;external;
{$L x86/libmsvcrt_defs00822.o}
procedure __towupper_l;external;
{$L x86/libmsvcrt_defs00823.o}
procedure __tzset;external;
{$L x86/libmsvcrt_defs00825.o}
procedure __ui64toa;external;
{$L x86/libmsvcrt_defs00826.o}
procedure __ui64toa_s;external;
{$L x86/libmsvcrt_defs00827.o}
procedure _tzset;external;
{$L x86/libmsvcrt_defs00828.o}
procedure _tzname;external;
{$L x86/libmsvcrt_defs00829.o}
procedure __ui64tow;external;
{$L x86/libmsvcrt_defs00830.o}
procedure __ui64tow_s;external;
{$L x86/libmsvcrt_defs00831.o}
procedure __ultoa;external;
{$L x86/libmsvcrt_defs00832.o}
procedure __ultoa_s;external;
{$L x86/libmsvcrt_defs00833.o}
procedure __ultow;external;
{$L x86/libmsvcrt_defs00834.o}
procedure __ultow_s;external;
{$L x86/libmsvcrt_defs00835.o}
procedure __umask;external;
{$L x86/libmsvcrt_defs00836.o}
procedure __ungetch;external;
{$L x86/libmsvcrt_defs00837.o}
procedure __ungetwch;external;
{$L x86/libmsvcrt_defs00838.o}
procedure __unlink;external;
{$L x86/libmsvcrt_defs00839.o}
procedure _unlink;external;
{$L x86/libmsvcrt_defs00840.o}
procedure __unloaddll;external;
{$L x86/libmsvcrt_defs00841.o}
procedure __unlock;external;
{$L x86/libmsvcrt_defs00842.o}
procedure __utime32;external;
{$L x86/libmsvcrt_defs00843.o}
procedure __utime;external;
{$L x86/libmsvcrt_defs00844.o}
procedure __utime64;external;
{$L x86/libmsvcrt_defs00845.o}
procedure __vcprintf;external;
{$L x86/libmsvcrt_defs00846.o}
procedure __vcprintf_l;external;
{$L x86/libmsvcrt_defs00847.o}
procedure __vcprintf_p;external;
{$L x86/libmsvcrt_defs00848.o}
procedure __vcprintf_p_l;external;
{$L x86/libmsvcrt_defs00849.o}
procedure __vcwprintf;external;
{$L x86/libmsvcrt_defs00850.o}
procedure __vcwprintf_l;external;
{$L x86/libmsvcrt_defs00851.o}
procedure __vcwprintf_p;external;
{$L x86/libmsvcrt_defs00852.o}
procedure __vcwprintf_p_l;external;
{$L x86/libmsvcrt_defs00853.o}
procedure __vfprintf_l;external;
{$L x86/libmsvcrt_defs00854.o}
procedure _umask;external;
{$L x86/libmsvcrt_defs00855.o}
procedure __vfprintf_p;external;
{$L x86/libmsvcrt_defs00856.o}
procedure __vfprintf_p_l;external;
{$L x86/libmsvcrt_defs00857.o}
procedure __vfprintf_s_l;external;
{$L x86/libmsvcrt_defs00858.o}
procedure __vfwprintf_l;external;
{$L x86/libmsvcrt_defs00859.o}
procedure __vfwprintf_p;external;
{$L x86/libmsvcrt_defs00860.o}
procedure __vfwprintf_p_l;external;
{$L x86/libmsvcrt_defs00861.o}
procedure __vfwprintf_s_l;external;
{$L x86/libmsvcrt_defs00862.o}
procedure __vprintf_l;external;
{$L x86/libmsvcrt_defs00863.o}
procedure __vprintf_p;external;
{$L x86/libmsvcrt_defs00864.o}
procedure __vprintf_p_l;external;
{$L x86/libmsvcrt_defs00865.o}
procedure __vprintf_s_l;external;
{$L x86/libmsvcrt_defs00866.o}
procedure __vscprintf_l;external;
{$L x86/libmsvcrt_defs00867.o}
procedure __vscprintf_p_l;external;
{$L x86/libmsvcrt_defs00868.o}
procedure __vscwprintf;external;
{$L x86/libmsvcrt_defs00869.o}
procedure _utime;external;
{$L x86/libmsvcrt_defs00870.o}
procedure __vscwprintf_l;external;
{$L x86/libmsvcrt_defs00871.o}
procedure __vscwprintf_p_l;external;
{$L x86/libmsvcrt_defs00872.o}
procedure __vsnprintf;external;
{$L x86/libmsvcrt_defs00873.o}
procedure __vsnprintf_c;external;
{$L x86/libmsvcrt_defs00874.o}
procedure __vsnprintf_c_l;external;
{$L x86/libmsvcrt_defs00875.o}
procedure __vsnprintf_l;external;
{$L x86/libmsvcrt_defs00876.o}
procedure __vsnprintf_s;external;
{$L x86/libmsvcrt_defs00877.o}
procedure __vsnprintf_s_l;external;
{$L x86/libmsvcrt_defs00878.o}
procedure _vsnwprintf;external;
{$L x86/libmsvcrt_defs00879.o}
procedure __vsnwprintf;external;
{$L x86/libmsvcrt_defs00880.o}
procedure __vsnwprintf_l;external;
{$L x86/libmsvcrt_defs00881.o}
procedure __vsnwprintf_s;external;
{$L x86/libmsvcrt_defs00882.o}
procedure __vsnwprintf_s_l;external;
{$L x86/libmsvcrt_defs00883.o}
procedure __vsprintf_l;external;
{$L x86/libmsvcrt_defs00884.o}
procedure __vsprintf_p;external;
{$L x86/libmsvcrt_defs00885.o}
procedure __vsprintf_p_l;external;
{$L x86/libmsvcrt_defs00886.o}
procedure __vsprintf_s_l;external;
{$L x86/libmsvcrt_defs00887.o}
procedure __vswprintf_c;external;
{$L x86/libmsvcrt_defs00888.o}
procedure __vswprintf_c_l;external;
{$L x86/libmsvcrt_defs00889.o}
procedure __vswprintf_l;external;
{$L x86/libmsvcrt_defs00890.o}
procedure __vswprintf_p_l;external;
{$L x86/libmsvcrt_defs00891.o}
procedure __vswprintf_s_l;external;
{$L x86/libmsvcrt_defs00892.o}
procedure __vwprintf_l;external;
{$L x86/libmsvcrt_defs00893.o}
procedure __vwprintf_p;external;
{$L x86/libmsvcrt_defs00894.o}
procedure __vwprintf_p_l;external;
{$L x86/libmsvcrt_defs00895.o}
procedure __vwprintf_s_l;external;
{$L x86/libmsvcrt_defs00896.o}
procedure __waccess;external;
{$L x86/libmsvcrt_defs00897.o}
procedure __wasctime;external;
{$L x86/libmsvcrt_defs00898.o}
procedure __wchdir;external;
{$L x86/libmsvcrt_defs00899.o}
procedure __wchmod;external;
{$L x86/libmsvcrt_defs00900.o}
procedure __wcreat;external;
{$L x86/libmsvcrt_defs00902.o}
procedure __wcscoll_l;external;
{$L x86/libmsvcrt_defs00903.o}
procedure __wcsdup;external;
{$L x86/libmsvcrt_defs00904.o}
procedure __wcserror;external;
{$L x86/libmsvcrt_defs00905.o}
procedure __wcserror_s;external;
{$L x86/libmsvcrt_defs00906.o}
procedure __wcsftime_l;external;
{$L x86/libmsvcrt_defs00907.o}
procedure _wcsdup;external;
{$L x86/libmsvcrt_defs00908.o}
procedure _wcscmpi;external;
{$L x86/libmsvcrt_defs00909.o}
procedure _wcsicmp;external;
{$L x86/libmsvcrt_defs00910.o}
procedure __wcsicmp;external;
{$L x86/libmsvcrt_defs00911.o}
procedure __wcsicmp_l;external;
{$L x86/libmsvcrt_defs00912.o}
procedure _wcsicoll;external;
{$L x86/libmsvcrt_defs00913.o}
procedure __wcsicoll;external;
{$L x86/libmsvcrt_defs00914.o}
procedure __wcsicoll_l;external;
{$L x86/libmsvcrt_defs00915.o}
procedure _wcslwr;external;
{$L x86/libmsvcrt_defs00916.o}
procedure __wcslwr;external;
{$L x86/libmsvcrt_defs00917.o}
procedure __wcslwr_l;external;
{$L x86/libmsvcrt_defs00918.o}
procedure __wcslwr_s;external;
{$L x86/libmsvcrt_defs00919.o}
procedure __wcslwr_s_l;external;
{$L x86/libmsvcrt_defs00920.o}
procedure __wcsncoll;external;
{$L x86/libmsvcrt_defs00921.o}
procedure __wcsncoll_l;external;
{$L x86/libmsvcrt_defs00922.o}
procedure __wcsnicmp;external;
{$L x86/libmsvcrt_defs00923.o}
procedure __wcsnicmp_l;external;
{$L x86/libmsvcrt_defs00924.o}
procedure __wcsnicoll;external;
{$L x86/libmsvcrt_defs00925.o}
procedure _wcsnicmp;external;
{$L x86/libmsvcrt_defs00926.o}
procedure __wcsnicoll_l;external;
{$L x86/libmsvcrt_defs00927.o}
procedure __wcsnset;external;
{$L x86/libmsvcrt_defs00928.o}
procedure __wcsnset_s;external;
{$L x86/libmsvcrt_defs00929.o}
procedure _wcsrev;external;
{$L x86/libmsvcrt_defs00930.o}
procedure __wcsrev;external;
{$L x86/libmsvcrt_defs00931.o}
procedure _wcsnset;external;
{$L x86/libmsvcrt_defs00932.o}
procedure __wcsset;external;
{$L x86/libmsvcrt_defs00933.o}
procedure _wcsset;external;
{$L x86/libmsvcrt_defs00934.o}
procedure __wcsset_s;external;
{$L x86/libmsvcrt_defs00935.o}
procedure __wcstoi64;external;
{$L x86/libmsvcrt_defs00936.o}
procedure __wcstoi64_l;external;
{$L x86/libmsvcrt_defs00937.o}
procedure __wcstol_l;external;
{$L x86/libmsvcrt_defs00938.o}
procedure __wcstombs_l;external;
{$L x86/libmsvcrt_defs00939.o}
procedure __wcstombs_s_l;external;
{$L x86/libmsvcrt_defs00940.o}
procedure __wcstoui64;external;
{$L x86/libmsvcrt_defs00941.o}
procedure __wcstoui64_l;external;
{$L x86/libmsvcrt_defs00942.o}
procedure __wcstoul_l;external;
{$L x86/libmsvcrt_defs00943.o}
procedure _wcsupr;external;
{$L x86/libmsvcrt_defs00944.o}
procedure __wcsupr;external;
{$L x86/libmsvcrt_defs00945.o}
procedure __wcsupr_l;external;
{$L x86/libmsvcrt_defs00946.o}
procedure __wcsupr_s;external;
{$L x86/libmsvcrt_defs00947.o}
procedure __wcsupr_s_l;external;
{$L x86/libmsvcrt_defs00948.o}
procedure __wcsxfrm_l;external;
{$L x86/libmsvcrt_defs00949.o}
procedure __wctime32;external;
{$L x86/libmsvcrt_defs00950.o}
procedure __wctime;external;
{$L x86/libmsvcrt_defs00951.o}
procedure __wctime64;external;
{$L x86/libmsvcrt_defs00952.o}
procedure __wctomb_l;external;
{$L x86/libmsvcrt_defs00953.o}
procedure __wctomb_s_l;external;
{$L x86/libmsvcrt_defs00954.o}
procedure __wctype;external;
{$L x86/libmsvcrt_defs00955.o}
procedure __wexecl;external;
{$L x86/libmsvcrt_defs00957.o}
procedure __wexecle;external;
{$L x86/libmsvcrt_defs00958.o}
procedure __wexeclp;external;
{$L x86/libmsvcrt_defs00959.o}
procedure __wexeclpe;external;
{$L x86/libmsvcrt_defs00960.o}
procedure __wexecv;external;
{$L x86/libmsvcrt_defs00961.o}
procedure __wexecve;external;
{$L x86/libmsvcrt_defs00962.o}
procedure __wexecvp;external;
{$L x86/libmsvcrt_defs00963.o}
procedure __wexecvpe;external;
{$L x86/libmsvcrt_defs00964.o}
procedure __wfdopen;external;
{$L x86/libmsvcrt_defs00965.o}
procedure __wfindfirst32i64;external;
{$L x86/libmsvcrt_defs00966.o}
procedure __wfindfirst;external;
{$L x86/libmsvcrt_defs00967.o}
procedure __wfindfirst64;external;
{$L x86/libmsvcrt_defs00968.o}
procedure __wfindfirsti64;external;
{$L x86/libmsvcrt_defs00969.o}
procedure __wfindnext;external;
{$L x86/libmsvcrt_defs00970.o}
procedure __wfindfirst32;external;
{$L x86/libmsvcrt_defs00971.o}
procedure __wfindnext32i64;external;
{$L x86/libmsvcrt_defs00972.o}
procedure __wfindnext64;external;
{$L x86/libmsvcrt_defs00973.o}
procedure __wfindnexti64;external;
{$L x86/libmsvcrt_defs00974.o}
procedure __wfindnext32;external;
{$L x86/libmsvcrt_defs00975.o}
procedure __wfopen;external;
{$L x86/libmsvcrt_defs00976.o}
procedure __wfopen_s;external;
{$L x86/libmsvcrt_defs00977.o}
procedure __wfreopen;external;
{$L x86/libmsvcrt_defs00978.o}
procedure __wfreopen_s;external;
{$L x86/libmsvcrt_defs00979.o}
procedure __wfsopen;external;
{$L x86/libmsvcrt_defs00980.o}
procedure __wfullpath;external;
{$L x86/libmsvcrt_defs00981.o}
procedure __wgetcwd;external;
{$L x86/libmsvcrt_defs00982.o}
procedure __wgetdcwd;external;
{$L x86/libmsvcrt_defs00983.o}
procedure __wgetenv;external;
{$L x86/libmsvcrt_defs00984.o}
procedure __wgetenv_s;external;
{$L x86/libmsvcrt_defs00985.o}
procedure __winput_s;external;
{$L x86/libmsvcrt_defs00988.o}
procedure __wmakepath;external;
{$L x86/libmsvcrt_defs00990.o}
procedure __wmakepath_s;external;
{$L x86/libmsvcrt_defs00991.o}
procedure __wmkdir;external;
{$L x86/libmsvcrt_defs00992.o}
procedure _ungetch;external;
{$L x86/libmsvcrt_defs00994.o}
procedure _vsnprintf_s;external;
{$L x86/libmsvcrt_defs00995.o}
procedure __wmktemp;external;
{$L x86/libmsvcrt_defs00996.o}
procedure __wopen;external;
{$L x86/libmsvcrt_defs00997.o}
procedure __woutput_s;external;
{$L x86/libmsvcrt_defs00998.o}
procedure __wperror;external;
{$L x86/libmsvcrt_defs00999.o}
procedure _wpopen;external;
{$L x86/libmsvcrt_defs01001.o}
procedure __wpopen;external;
{$L x86/libmsvcrt_defs01002.o}
procedure __wprintf_l;external;
{$L x86/libmsvcrt_defs01003.o}
procedure __wprintf_p;external;
{$L x86/libmsvcrt_defs01004.o}
procedure __wprintf_p_l;external;
{$L x86/libmsvcrt_defs01005.o}
procedure __wprintf_s_l;external;
{$L x86/libmsvcrt_defs01006.o}
procedure __wputenv;external;
{$L x86/libmsvcrt_defs01007.o}
procedure __wputenv_s;external;
{$L x86/libmsvcrt_defs01008.o}
procedure __wremove;external;
{$L x86/libmsvcrt_defs01009.o}
procedure __wrename;external;
{$L x86/libmsvcrt_defs01010.o}
procedure __write;external;
{$L x86/libmsvcrt_defs01011.o}
procedure __wrmdir;external;
{$L x86/libmsvcrt_defs01012.o}
procedure __wscanf_l;external;
{$L x86/libmsvcrt_defs01013.o}
procedure __wscanf_s_l;external;
{$L x86/libmsvcrt_defs01014.o}
procedure __wsearchenv;external;
{$L x86/libmsvcrt_defs01015.o}
procedure __wsearchenv_s;external;
{$L x86/libmsvcrt_defs01016.o}
procedure __wsetlocale;external;
{$L x86/libmsvcrt_defs01017.o}
procedure _write;external;
{$L x86/libmsvcrt_defs01018.o}
procedure __wsopen;external;
{$L x86/libmsvcrt_defs01019.o}
procedure __wsopen_s;external;
{$L x86/libmsvcrt_defs01020.o}
procedure __wspawnl;external;
{$L x86/libmsvcrt_defs01021.o}
procedure __wspawnle;external;
{$L x86/libmsvcrt_defs01022.o}
procedure __wspawnlp;external;
{$L x86/libmsvcrt_defs01023.o}
procedure __wspawnlpe;external;
{$L x86/libmsvcrt_defs01024.o}
procedure __wspawnv;external;
{$L x86/libmsvcrt_defs01025.o}
procedure __wspawnve;external;
{$L x86/libmsvcrt_defs01026.o}
procedure __wspawnvp;external;
{$L x86/libmsvcrt_defs01027.o}
procedure __wspawnvpe;external;
{$L x86/libmsvcrt_defs01028.o}
procedure __wsplitpath;external;
{$L x86/libmsvcrt_defs01029.o}
procedure __wsplitpath_s;external;
{$L x86/libmsvcrt_defs01030.o}
procedure __wstat32;external;
{$L x86/libmsvcrt_defs01031.o}
procedure __wstat;external;
{$L x86/libmsvcrt_defs01032.o}
procedure __wstat64;external;
{$L x86/libmsvcrt_defs01033.o}
procedure __wstati64;external;
{$L x86/libmsvcrt_defs01034.o}
procedure __wstrdate;external;
{$L x86/libmsvcrt_defs01035.o}
procedure __wstrtime;external;
{$L x86/libmsvcrt_defs01036.o}
procedure __wsystem;external;
{$L x86/libmsvcrt_defs01037.o}
procedure __wtempnam;external;
{$L x86/libmsvcrt_defs01038.o}
procedure __wtmpnam;external;
{$L x86/libmsvcrt_defs01039.o}
procedure __wtmpnam_s;external;
{$L x86/libmsvcrt_defs01040.o}
procedure __wtof;external;
{$L x86/libmsvcrt_defs01041.o}
procedure __wtof_l;external;
{$L x86/libmsvcrt_defs01042.o}
procedure __wtoi;external;
{$L x86/libmsvcrt_defs01043.o}
procedure __wtoi64;external;
{$L x86/libmsvcrt_defs01044.o}
procedure __wtoi64_l;external;
{$L x86/libmsvcrt_defs01045.o}
procedure __wtoi_l;external;
{$L x86/libmsvcrt_defs01046.o}
procedure __wtol;external;
{$L x86/libmsvcrt_defs01047.o}
procedure __wtol_l;external;
{$L x86/libmsvcrt_defs01048.o}
procedure __wunlink;external;
{$L x86/libmsvcrt_defs01049.o}
procedure __wutime32;external;
{$L x86/libmsvcrt_defs01050.o}
procedure __wutime;external;
{$L x86/libmsvcrt_defs01051.o}
procedure __wutime64;external;
{$L x86/libmsvcrt_defs01052.o}
procedure _y0;external;
{$L x86/libmsvcrt_defs01053.o}
procedure __y0;external;
{$L x86/libmsvcrt_defs01054.o}
procedure _y1;external;
{$L x86/libmsvcrt_defs01055.o}
procedure __y1;external;
{$L x86/libmsvcrt_defs01056.o}
procedure __yn;external;
{$L x86/libmsvcrt_defs01057.o}
procedure _yn;external;
{$L x86/libmsvcrt_defs01058.o}
procedure _abort;external;
{$L x86/libmsvcrt_defs01059.o}
procedure _abs;external;
{$L x86/libmsvcrt_defs01060.o}
procedure _acos;external;
{$L x86/libmsvcrt_defs01061.o}
procedure _asctime;external;
{$L x86/libmsvcrt_defs01062.o}
procedure _asin;external;
{$L x86/libmsvcrt_defs01063.o}
procedure _atan;external;
{$L x86/libmsvcrt_defs01064.o}
procedure _atof;external;
{$L x86/libmsvcrt_defs01067.o}
procedure _atoi;external;
{$L x86/libmsvcrt_defs01068.o}
procedure _atol;external;
{$L x86/libmsvcrt_defs01069.o}
procedure _bsearch;external;
{$L x86/libmsvcrt_defs01070.o}
procedure _bsearch_s;external;
{$L x86/libmsvcrt_defs01071.o}
procedure _calloc;external;
{$L x86/libmsvcrt_defs01072.o}
procedure _clearerr;external;
{$L x86/libmsvcrt_defs01074.o}
procedure __difftime32;external;
{$L x86/libmsvcrt_defs01075.o}
procedure _clearerr_s;external;
{$L x86/libmsvcrt_defs01076.o}
procedure _clock;external;
{$L x86/libmsvcrt_defs01077.o}
procedure __ctime32;external;
{$L x86/libmsvcrt_defs01079.o}
procedure _cosh;external;
{$L x86/libmsvcrt_defs01080.o}
procedure _ctime;external;
{$L x86/libmsvcrt_defs01081.o}
procedure _difftime;external;
{$L x86/libmsvcrt_defs01082.o}
procedure _div;external;
{$L x86/libmsvcrt_defs01083.o}
procedure _exit;external;
{$L x86/libmsvcrt_defs01084.o}
procedure _fclose;external;
{$L x86/libmsvcrt_defs01087.o}
procedure _feof;external;
{$L x86/libmsvcrt_defs01088.o}
procedure _ferror;external;
{$L x86/libmsvcrt_defs01089.o}
procedure _fflush;external;
{$L x86/libmsvcrt_defs01090.o}
procedure _fgetc;external;
{$L x86/libmsvcrt_defs01091.o}
procedure _fgetpos;external;
{$L x86/libmsvcrt_defs01092.o}
procedure _fgets;external;
{$L x86/libmsvcrt_defs01093.o}
procedure _fgetwc;external;
{$L x86/libmsvcrt_defs01094.o}
procedure _fgetws;external;
{$L x86/libmsvcrt_defs01095.o}
procedure ___ms_fscanf;external;
{$L x86/libmsvcrt_defs01098.o}
procedure _fopen;external;
{$L x86/libmsvcrt_defs01099.o}
procedure _fopen_s;external;
{$L x86/libmsvcrt_defs01100.o}
procedure ___ms_fprintf;external;
{$L x86/libmsvcrt_defs01101.o}
procedure _fprintf;external;
{$L x86/libmsvcrt_defs01102.o}
procedure _fprintf_s;external;
{$L x86/libmsvcrt_defs01103.o}
procedure _fputc;external;
{$L x86/libmsvcrt_defs01104.o}
procedure _fputs;external;
{$L x86/libmsvcrt_defs01105.o}
procedure _fputwc;external;
{$L x86/libmsvcrt_defs01106.o}
procedure _fputws;external;
{$L x86/libmsvcrt_defs01107.o}
procedure _fread;external;
{$L x86/libmsvcrt_defs01108.o}
procedure _free;external;
{$L x86/libmsvcrt_defs01109.o}
procedure _freopen;external;
{$L x86/libmsvcrt_defs01110.o}
procedure _freopen_s;external;
{$L x86/libmsvcrt_defs01111.o}
procedure _fscanf;external;
{$L x86/libmsvcrt_defs01113.o}
procedure _fscanf_s;external;
{$L x86/libmsvcrt_defs01114.o}
procedure ___ms_fwprintf;external;
{$L x86/libmsvcrt_defs01115.o}
procedure _fseek;external;
{$L x86/libmsvcrt_defs01116.o}
procedure _fsetpos;external;
{$L x86/libmsvcrt_defs01117.o}
procedure _ftell;external;
{$L x86/libmsvcrt_defs01118.o}
procedure _fwprintf;external;
{$L x86/libmsvcrt_defs01119.o}
procedure ___ms_fwscanf;external;
{$L x86/libmsvcrt_defs01120.o}
procedure _fwprintf_s;external;
{$L x86/libmsvcrt_defs01121.o}
procedure _fwrite;external;
{$L x86/libmsvcrt_defs01122.o}
procedure _fwscanf;external;
{$L x86/libmsvcrt_defs01123.o}
procedure _fwscanf_s;external;
{$L x86/libmsvcrt_defs01124.o}
procedure _getc;external;
{$L x86/libmsvcrt_defs01125.o}
procedure _getchar;external;
{$L x86/libmsvcrt_defs01126.o}
procedure __gmtime32;external;
{$L x86/libmsvcrt_defs01127.o}
procedure _getenv;external;
{$L x86/libmsvcrt_defs01128.o}
procedure _getenv_s;external;
{$L x86/libmsvcrt_defs01129.o}
procedure _gets;external;
{$L x86/libmsvcrt_defs01130.o}
procedure _getwc;external;
{$L x86/libmsvcrt_defs01131.o}
procedure _getwchar;external;
{$L x86/libmsvcrt_defs01132.o}
procedure _gmtime;external;
{$L x86/libmsvcrt_defs01133.o}
procedure _is_wctype;external;
{$L x86/libmsvcrt_defs01134.o}
procedure _isalnum;external;
{$L x86/libmsvcrt_defs01135.o}
procedure _isalpha;external;
{$L x86/libmsvcrt_defs01136.o}
procedure _iscntrl;external;
{$L x86/libmsvcrt_defs01137.o}
procedure _isdigit;external;
{$L x86/libmsvcrt_defs01138.o}
procedure _isgraph;external;
{$L x86/libmsvcrt_defs01139.o}
procedure _isleadbyte;external;
{$L x86/libmsvcrt_defs01140.o}
procedure _islower;external;
{$L x86/libmsvcrt_defs01141.o}
procedure _isprint;external;
{$L x86/libmsvcrt_defs01142.o}
procedure _ispunct;external;
{$L x86/libmsvcrt_defs01143.o}
procedure _isspace;external;
{$L x86/libmsvcrt_defs01144.o}
procedure _isupper;external;
{$L x86/libmsvcrt_defs01145.o}
procedure _iswalnum;external;
{$L x86/libmsvcrt_defs01146.o}
procedure _iswalpha;external;
{$L x86/libmsvcrt_defs01147.o}
procedure _iswascii;external;
{$L x86/libmsvcrt_defs01148.o}
procedure _iswcntrl;external;
{$L x86/libmsvcrt_defs01149.o}
procedure _iswctype;external;
{$L x86/libmsvcrt_defs01150.o}
procedure _iswdigit;external;
{$L x86/libmsvcrt_defs01151.o}
procedure _iswgraph;external;
{$L x86/libmsvcrt_defs01152.o}
procedure _iswlower;external;
{$L x86/libmsvcrt_defs01153.o}
procedure _iswprint;external;
{$L x86/libmsvcrt_defs01154.o}
procedure _iswpunct;external;
{$L x86/libmsvcrt_defs01155.o}
procedure _iswspace;external;
{$L x86/libmsvcrt_defs01156.o}
procedure _iswupper;external;
{$L x86/libmsvcrt_defs01157.o}
procedure _iswxdigit;external;
{$L x86/libmsvcrt_defs01158.o}
procedure _isxdigit;external;
{$L x86/libmsvcrt_defs01159.o}
procedure _labs;external;
{$L x86/libmsvcrt_defs01160.o}
procedure _localeconv;external;
{$L x86/libmsvcrt_defs01162.o}
procedure __localtime32;external;
{$L x86/libmsvcrt_defs01163.o}
procedure _ldiv;external;
{$L x86/libmsvcrt_defs01164.o}
procedure _localtime;external;
{$L x86/libmsvcrt_defs01165.o}
procedure _log10;external;
{$L x86/libmsvcrt_defs01167.o}
procedure _longjmp;external;
{$L x86/libmsvcrt_defs01168.o}
procedure _malloc;external;
{$L x86/libmsvcrt_defs01169.o}
procedure _mblen;external;
{$L x86/libmsvcrt_defs01170.o}
procedure _perror;external;
{$L x86/libmsvcrt_defs01171.o}
procedure ___ms_printf;external;
{$L x86/libmsvcrt_defs01172.o}
procedure __mktime32;external;
{$L x86/libmsvcrt_defs01173.o}
procedure _mbsrtowcs_s;external;
{$L x86/libmsvcrt_defs01174.o}
procedure _mbstowcs;external;
{$L x86/libmsvcrt_defs01175.o}
procedure _mbstowcs_s;external;
{$L x86/libmsvcrt_defs01176.o}
procedure _mbtowc;external;
{$L x86/libmsvcrt_defs01177.o}
procedure _memchr;external;
{$L x86/libmsvcrt_defs01178.o}
procedure _memcmp;external;
{$L x86/libmsvcrt_defs01179.o}
procedure _memcpy;external;
{$L x86/libmsvcrt_defs01180.o}
procedure _memmove;external;
{$L x86/libmsvcrt_defs01181.o}
procedure _memset;external;
{$L x86/libmsvcrt_defs01182.o}
procedure _mktime;external;
{$L x86/libmsvcrt_defs01183.o}
procedure _printf;external;
{$L x86/libmsvcrt_defs01186.o}
procedure _printf_s;external;
{$L x86/libmsvcrt_defs01187.o}
procedure _putc;external;
{$L x86/libmsvcrt_defs01188.o}
procedure _putchar;external;
{$L x86/libmsvcrt_defs01189.o}
procedure _puts;external;
{$L x86/libmsvcrt_defs01190.o}
procedure _putwc;external;
{$L x86/libmsvcrt_defs01191.o}
procedure _putwchar;external;
{$L x86/libmsvcrt_defs01192.o}
procedure _qsort;external;
{$L x86/libmsvcrt_defs01193.o}
procedure _qsort_s;external;
{$L x86/libmsvcrt_defs01194.o}
procedure _raise;external;
{$L x86/libmsvcrt_defs01195.o}
procedure _rand;external;
{$L x86/libmsvcrt_defs01196.o}
procedure _realloc;external;
{$L x86/libmsvcrt_defs01197.o}
procedure _remove;external;
{$L x86/libmsvcrt_defs01198.o}
procedure ___ms_sprintf;external;
{$L x86/libmsvcrt_defs01199.o}
procedure _rename;external;
{$L x86/libmsvcrt_defs01200.o}
procedure ___ms_scanf;external;
{$L x86/libmsvcrt_defs01201.o}
procedure _rewind;external;
{$L x86/libmsvcrt_defs01202.o}
procedure _scanf;external;
{$L x86/libmsvcrt_defs01203.o}
procedure _scanf_s;external;
{$L x86/libmsvcrt_defs01204.o}
procedure _setbuf;external;
{$L x86/libmsvcrt_defs01205.o}
procedure _setlocale;external;
{$L x86/libmsvcrt_defs01206.o}
procedure _setvbuf;external;
{$L x86/libmsvcrt_defs01207.o}
procedure _signal;external;
{$L x86/libmsvcrt_defs01208.o}
procedure _sinh;external;
{$L x86/libmsvcrt_defs01210.o}
procedure _sprintf;external;
{$L x86/libmsvcrt_defs01211.o}
procedure _srand;external;
{$L x86/libmsvcrt_defs01212.o}
procedure ___ms_sscanf;external;
{$L x86/libmsvcrt_defs01213.o}
procedure _sscanf;external;
{$L x86/libmsvcrt_defs01215.o}
procedure _sscanf_s;external;
{$L x86/libmsvcrt_defs01216.o}
procedure _strcat;external;
{$L x86/libmsvcrt_defs01217.o}
procedure _strcat_s;external;
{$L x86/libmsvcrt_defs01218.o}
procedure _strchr;external;
{$L x86/libmsvcrt_defs01219.o}
procedure _strcmp;external;
{$L x86/libmsvcrt_defs01220.o}
procedure _strcoll;external;
{$L x86/libmsvcrt_defs01221.o}
procedure _strcpy;external;
{$L x86/libmsvcrt_defs01222.o}
procedure _strcpy_s;external;
{$L x86/libmsvcrt_defs01223.o}
procedure _strcspn;external;
{$L x86/libmsvcrt_defs01224.o}
procedure _strerror;external;
{$L x86/libmsvcrt_defs01225.o}
procedure _strftime;external;
{$L x86/libmsvcrt_defs01226.o}
procedure _strlen;external;
{$L x86/libmsvcrt_defs01227.o}
procedure _strncat;external;
{$L x86/libmsvcrt_defs01228.o}
procedure _strncat_s;external;
{$L x86/libmsvcrt_defs01229.o}
procedure _strncmp;external;
{$L x86/libmsvcrt_defs01230.o}
procedure _strncpy;external;
{$L x86/libmsvcrt_defs01231.o}
procedure _strncpy_s;external;
{$L x86/libmsvcrt_defs01232.o}
procedure _strpbrk;external;
{$L x86/libmsvcrt_defs01233.o}
procedure _strrchr;external;
{$L x86/libmsvcrt_defs01234.o}
procedure _strspn;external;
{$L x86/libmsvcrt_defs01235.o}
procedure _strstr;external;
{$L x86/libmsvcrt_defs01236.o}
procedure _strtod;external;
{$L x86/libmsvcrt_defs01237.o}
procedure _strtok;external;
{$L x86/libmsvcrt_defs01238.o}
procedure _strtok_s;external;
{$L x86/libmsvcrt_defs01239.o}
procedure _strtol;external;
{$L x86/libmsvcrt_defs01240.o}
procedure _strtoul;external;
{$L x86/libmsvcrt_defs01241.o}
procedure _strxfrm;external;
{$L x86/libmsvcrt_defs01242.o}
procedure __swprintf;external;
{$L x86/libmsvcrt_defs01243.o}
procedure _swprintf;external;
{$L x86/libmsvcrt_defs01244.o}
procedure ___ms_swprintf;external;
{$L x86/libmsvcrt_defs01245.o}
procedure _swprintf_s;external;
{$L x86/libmsvcrt_defs01246.o}
procedure __time32;external;
{$L x86/libmsvcrt_defs01247.o}
procedure ___ms_swscanf;external;
{$L x86/libmsvcrt_defs01248.o}
procedure _swscanf;external;
{$L x86/libmsvcrt_defs01249.o}
procedure _swscanf_s;external;
{$L x86/libmsvcrt_defs01250.o}
procedure _system;external;
{$L x86/libmsvcrt_defs01251.o}
procedure _tan;external;
{$L x86/libmsvcrt_defs01252.o}
procedure _tanh;external;
{$L x86/libmsvcrt_defs01253.o}
procedure _time;external;
{$L x86/libmsvcrt_defs01254.o}
procedure _tmpfile;external;
{$L x86/libmsvcrt_defs01255.o}
procedure _tmpfile_s;external;
{$L x86/libmsvcrt_defs01256.o}
procedure _tmpnam;external;
{$L x86/libmsvcrt_defs01257.o}
procedure _tmpnam_s;external;
{$L x86/libmsvcrt_defs01258.o}
procedure _tolower;external;
{$L x86/libmsvcrt_defs01259.o}
procedure _toupper;external;
{$L x86/libmsvcrt_defs01260.o}
procedure _towlower;external;
{$L x86/libmsvcrt_defs01261.o}
procedure _towupper;external;
{$L x86/libmsvcrt_defs01262.o}
procedure _ungetc;external;
{$L x86/libmsvcrt_defs01263.o}
procedure _ungetwc;external;
{$L x86/libmsvcrt_defs01264.o}
procedure ___ms_vprintf;external;
{$L x86/libmsvcrt_defs01265.o}
procedure ___ms_vfprintf;external;
{$L x86/libmsvcrt_defs01266.o}
procedure _vfprintf;external;
{$L x86/libmsvcrt_defs01267.o}
procedure ___ms_vfwprintf;external;
{$L x86/libmsvcrt_defs01268.o}
procedure _vfprintf_s;external;
{$L x86/libmsvcrt_defs01269.o}
procedure _vfwprintf;external;
{$L x86/libmsvcrt_defs01270.o}
procedure _vfwprintf_s;external;
{$L x86/libmsvcrt_defs01271.o}
procedure _vprintf;external;
{$L x86/libmsvcrt_defs01272.o}
procedure _vprintf_s;external;
{$L x86/libmsvcrt_defs01273.o}
procedure ___ms_vswprintf;external;
{$L x86/libmsvcrt_defs01274.o}
procedure ___ms_vsprintf;external;
{$L x86/libmsvcrt_defs01275.o}
procedure _vsprintf;external;
{$L x86/libmsvcrt_defs01276.o}
procedure __vswprintf;external;
{$L x86/libmsvcrt_defs01277.o}
procedure _vswprintf;external;
{$L x86/libmsvcrt_defs01278.o}
procedure _vswprintf_s;external;
{$L x86/libmsvcrt_defs01279.o}
procedure ___ms_vwprintf;external;
{$L x86/libmsvcrt_defs01280.o}
procedure _vwprintf;external;
{$L x86/libmsvcrt_defs01281.o}
procedure _vwprintf_s;external;
{$L x86/libmsvcrt_defs01282.o}
procedure _wcrtomb_s;external;
{$L x86/libmsvcrt_defs01283.o}
procedure _wcscat;external;
{$L x86/libmsvcrt_defs01284.o}
procedure _wcscat_s;external;
{$L x86/libmsvcrt_defs01285.o}
procedure _wcschr;external;
{$L x86/libmsvcrt_defs01286.o}
procedure _wcscmp;external;
{$L x86/libmsvcrt_defs01287.o}
procedure _wcscoll;external;
{$L x86/libmsvcrt_defs01288.o}
procedure _wcscpy;external;
{$L x86/libmsvcrt_defs01289.o}
procedure _wcscpy_s;external;
{$L x86/libmsvcrt_defs01290.o}
procedure _wcscspn;external;
{$L x86/libmsvcrt_defs01291.o}
procedure _wcsftime;external;
{$L x86/libmsvcrt_defs01292.o}
procedure _wcslen;external;
{$L x86/libmsvcrt_defs01293.o}
procedure _wcsncat;external;
{$L x86/libmsvcrt_defs01294.o}
procedure _wcsncat_s;external;
{$L x86/libmsvcrt_defs01295.o}
procedure _wcsncmp;external;
{$L x86/libmsvcrt_defs01296.o}
procedure _wcsncpy;external;
{$L x86/libmsvcrt_defs01297.o}
procedure _wcsncpy_s;external;
{$L x86/libmsvcrt_defs01298.o}
procedure _wcspbrk;external;
{$L x86/libmsvcrt_defs01300.o}
procedure _wcsrchr;external;
{$L x86/libmsvcrt_defs01301.o}
procedure _wcsrtombs_s;external;
{$L x86/libmsvcrt_defs01302.o}
procedure _wcsspn;external;
{$L x86/libmsvcrt_defs01303.o}
procedure _wcsstr;external;
{$L x86/libmsvcrt_defs01304.o}
procedure _wcstod;external;
{$L x86/libmsvcrt_defs01305.o}
procedure _wcstok;external;
{$L x86/libmsvcrt_defs01306.o}
procedure _wcstok_s;external;
{$L x86/libmsvcrt_defs01307.o}
procedure _wcstol;external;
{$L x86/libmsvcrt_defs01308.o}
procedure _wcstombs_s;external;
{$L x86/libmsvcrt_defs01309.o}
procedure _wcstoul;external;
{$L x86/libmsvcrt_defs01310.o}
procedure _wcsxfrm;external;
{$L x86/libmsvcrt_defs01311.o}
procedure _wctomb;external;
{$L x86/libmsvcrt_defs01312.o}
procedure _wctomb_s;external;
{$L x86/libmsvcrt_defs01313.o}
procedure ___ms_wprintf;external;
{$L x86/libmsvcrt_defs01314.o}
procedure _wcstombs;external;
{$L x86/libmsvcrt_defs01315.o}
procedure _wprintf;external;
{$L x86/libmsvcrt_defs01316.o}
procedure ___ms_wscanf;external;
{$L x86/libmsvcrt_defs01317.o}
procedure _wprintf_s;external;
{$L x86/libmsvcrt_defs01318.o}
procedure _wscanf;external;
{$L x86/libmsvcrt_defs01319.o}
procedure _wscanf_s;external;
{$L x86/libmsvcrt_defs01320.o}
var
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
  __imp___CRT_RTC_INIT : UInt64;
  __imp___EH_prolog : UInt64;
  __imp___Getdays : UInt64;
  __imp___Getmonths : UInt64;
  __imp___Gettnames : UInt64;
  __imp___HUGE : UInt64;
  __imp___Strftime : UInt64;
  __imp___XcptFilter : UInt64;
  __imp____CppXcptFilter : UInt64;
  __imp____CxxCallUnwindDtor : UInt64;
  __imp____CxxCallUnwindVecDtor : UInt64;
  __imp____CxxDetectRethrow : UInt64;
  __imp____CxxExceptionFilter : UInt64;
  __imp____CxxFrameHandler : UInt64;
  __imp____CxxLongjmpUnwind : UInt64;
  __imp____CxxQueryExceptionSize : UInt64;
  __imp____CxxRegisterExceptionObject : UInt64;
  __imp____CxxUnregisterExceptionObject : UInt64;
  __imp____DestructExceptionObject : UInt64;
  __imp____RTCastToVoid : UInt64;
  __imp____RTDynamicCast : UInt64;
  __imp____RTtypeid : UInt64;
  __imp____STRINGTOLD : UInt64;
  __imp_____lc_collate_cp_func : UInt64;
  __imp_____lc_handle_func : UInt64;
  __imp_____setlc_active_func : UInt64;
  __imp_____unguarded_readlc_active_add_func : UInt64;
  __imp____argc : UInt64;
  __imp____argv : UInt64;
  __imp____badioinfo : UInt64;
  __imp____buffer_overrun : UInt64;
  __imp____crtCompareStringA : UInt64;
  __imp____crtCompareStringW : UInt64;
  __imp____crtGetLocaleInfoW : UInt64;
  __imp____crtGetStringTypeW : UInt64;
  __imp____crtLCMapStringA : UInt64;
  __imp____crtLCMapStringW : UInt64;
  __imp____dllonexit : UInt64;
  __imp____doserrno : UInt64;
  __imp____fpecode : UInt64;
  __imp____getmainargs : UInt64;
  __imp____initenv : UInt64;
  __imp____iob_func : UInt64;
  __imp____isascii : UInt64;
  __imp____iscsym : UInt64;
  __imp____iscsymf : UInt64;
  __imp__isascii : UInt64;
  __imp____lc_clike : UInt64;
  __imp____lc_codepage : UInt64;
  __imp____lc_collate_cp : UInt64;
  __imp____lc_handle : UInt64;
  __imp____lconv_init : UInt64;
  __imp____mb_cur_max : UInt64;
  __imp____p___argc : UInt64;
  __imp____p___argv : UInt64;
  __imp____p___initenv : UInt64;
  __imp____p___mb_cur_max : UInt64;
  __imp____p___wargv : UInt64;
  __imp____p___winitenv : UInt64;
  __imp____p__acmdln : UInt64;
  __imp____p__amblksiz : UInt64;
  __imp____p__commode : UInt64;
  __imp____p__daylight : UInt64;
  __imp____p__dstbias : UInt64;
  __imp____p__environ : UInt64;
  __imp____p__fileinfo : UInt64;
  __imp____p__fmode : UInt64;
  __imp____p__iob : UInt64;
  __imp____p__mbcasemap : UInt64;
  __imp____p__mbctype : UInt64;
  __imp____p__osver : UInt64;
  __imp____p__pctype : UInt64;
  __imp____p__pgmptr : UInt64;
  __imp____p__pwctype : UInt64;
  __imp____p__timezone : UInt64;
  __imp____p__tzname : UInt64;
  __imp____p__wcmdln : UInt64;
  __imp____p__wenviron : UInt64;
  __imp____p__winmajor : UInt64;
  __imp____p__winminor : UInt64;
  __imp____p__winver : UInt64;
  __imp____p__wpgmptr : UInt64;
  __imp____pctype_func : UInt64;
  __imp____pioinfo : UInt64;
  __imp____pwctype_func : UInt64;
  __imp____pxcptinfoptrs : UInt64;
  __imp____security_error_handler : UInt64;
  __imp____set_app_type : UInt64;
  __imp____set_buffer_overrun_handler : UInt64;
  __imp____setlc_active : UInt64;
  __imp____setusermatherr : UInt64;
  __imp____threadhandle : UInt64;
  __imp____threadid : UInt64;
  __imp____toascii : UInt64;
  __imp____unDName : UInt64;
  __imp__toascii : UInt64;
  __imp____unDNameEx : UInt64;
  __imp____uncaught_exception : UInt64;
  __imp____unguarded_readlc_active : UInt64;
  __imp____wargv : UInt64;
  __imp____wcserror : UInt64;
  __imp____wgetmainargs : UInt64;
  __imp____winitenv : UInt64;
  __imp___abnormal_termination : UInt64;
  __imp___access : UInt64;
  __imp__access : UInt64;
  __imp___acmdln : UInt64;
  __imp___adj_fdiv_m16i : UInt64;
  __imp___adj_fdiv_m32 : UInt64;
  __imp___adj_fdiv_m32i : UInt64;
  __imp___adj_fdiv_m64 : UInt64;
  __imp___adj_fdiv_r : UInt64;
  __imp___adj_fdivr_m16i : UInt64;
  __imp___adj_fdivr_m32 : UInt64;
  __imp___adj_fdivr_m32i : UInt64;
  __imp___adj_fdivr_m64 : UInt64;
  __imp___adj_fpatan : UInt64;
  __imp___adj_fprem : UInt64;
  __imp___adj_fprem1 : UInt64;
  __imp___adj_fptan : UInt64;
  __imp___adjust_fdiv : UInt64;
  __imp___aexit_rtn : UInt64;
  __imp___aligned_free : UInt64;
  __imp___aligned_malloc : UInt64;
  __imp___aligned_offset_malloc : UInt64;
  __imp___aligned_offset_realloc : UInt64;
  __imp___aligned_realloc : UInt64;
  __imp___amsg_exit : UInt64;
  __imp___assert : UInt64;
  __imp___atodbl : UInt64;
  __imp___atodbl_l : UInt64;
  __imp___atof_l : UInt64;
  __imp___atoflt_l : UInt64;
  __imp___atoi64 : UInt64;
  __imp___atoi64_l : UInt64;
  __imp___atoi_l : UInt64;
  __imp___atol_l : UInt64;
  __imp___atoldbl : UInt64;
  __imp___beep : UInt64;
  __imp___beginthread : UInt64;
  __imp___beginthreadex : UInt64;
  __imp___c_exit : UInt64;
  __imp___cabs : UInt64;
  __imp___callnewh : UInt64;
  __imp___cexit : UInt64;
  __imp___cgets : UInt64;
  __imp___cgetws : UInt64;
  __imp___chdir : UInt64;
  __imp__chdir : UInt64;
  __imp___chdrive : UInt64;
  __imp__iscsymf : UInt64;
  __imp__chgsign : UInt64;
  __imp___chgsign : UInt64;
  __imp___chkesp : UInt64;
  __imp__chmod : UInt64;
  __imp___chmod : UInt64;
  __imp___chsize : UInt64;
  __imp___clearfp : UInt64;
  __imp__close : UInt64;
  __imp___close : UInt64;
  __imp___commit : UInt64;
  __imp___commode : UInt64;
  __imp___control87 : UInt64;
  __imp___controlfp : UInt64;
  __imp___copysign : UInt64;
  __imp___cprintf : UInt64;
  __imp___cprintf_l : UInt64;
  __imp___cprintf_p : UInt64;
  __imp___cprintf_p_l : UInt64;
  __imp___cputs : UInt64;
  __imp___cputws : UInt64;
  __imp__creat : UInt64;
  __imp___creat : UInt64;
  __imp___cscanf : UInt64;
  __imp___cscanf_l : UInt64;
  __imp__chsize : UInt64;
  __imp___cscanf_s : UInt64;
  __imp___cscanf_s_l : UInt64;
  __imp___ctime64 : UInt64;
  __imp___ctype : UInt64;
  __imp__cwait : UInt64;
  __imp___cwait : UInt64;
  __imp___cwprintf : UInt64;
  __imp___cwprintf_l : UInt64;
  __imp___cwprintf_p : UInt64;
  __imp___cwprintf_p_l : UInt64;
  __imp___cwscanf : UInt64;
  __imp___cwscanf_l : UInt64;
  __imp___cwscanf_s : UInt64;
  __imp___cwscanf_s_l : UInt64;
  __imp___daylight : UInt64;
  __imp___difftime64 : UInt64;
  __imp__daylight : UInt64;
  __imp___dstbias : UInt64;
  __imp__dup : UInt64;
  __imp___dup : UInt64;
  __imp___dup2 : UInt64;
  __imp___ecvt : UInt64;
  __imp__dup2 : UInt64;
  __imp___ecvt_s : UInt64;
  __imp__ecvt : UInt64;
  __imp___endthread : UInt64;
  __imp___endthreadex : UInt64;
  __imp___environ : UInt64;
  __imp___eof : UInt64;
  __imp___errno : UInt64;
  __imp___except_handler2 : UInt64;
  __imp___except_handler3 : UInt64;
  __imp___execl : UInt64;
  __imp__execle : UInt64;
  __imp___execle : UInt64;
  __imp__execl : UInt64;
  __imp___execlp : UInt64;
  __imp__execlp : UInt64;
  __imp___execlpe : UInt64;
  __imp___execv : UInt64;
  __imp___execve : UInt64;
  __imp__execv : UInt64;
  __imp__execve : UInt64;
  __imp___execvp : UInt64;
  __imp___execvpe : UInt64;
  __imp__execlpe : UInt64;
  __imp___exit : UInt64;
  __imp___expand : UInt64;
  __imp__eof : UInt64;
  __imp__execvp : UInt64;
  __imp__execvpe : UInt64;
  __imp___fcloseall : UInt64;
  __imp___fcvt : UInt64;
  __imp___fcvt_s : UInt64;
  __imp__fdopen : UInt64;
  __imp___fdopen : UInt64;
  __imp__fcvt : UInt64;
  __imp__fgetchar : UInt64;
  __imp___fgetchar : UInt64;
  __imp__fgetwchar : UInt64;
  __imp___fgetwchar : UInt64;
  __imp___filbuf : UInt64;
  __imp__filelength : UInt64;
  __imp___filelength : UInt64;
  __imp___filelengthi64 : UInt64;
  __imp___fileinfo : UInt64;
  __imp___fileno : UInt64;
  __imp___findclose : UInt64;
  __imp__fileno : UInt64;
  __imp___findfirst32i64 : UInt64;
  __imp___findnext32i64 : UInt64;
  __imp___findfirst32 : UInt64;
  __imp___findfirst : UInt64;
  __imp___findfirst64 : UInt64;
  __imp___findfirsti64 : UInt64;
  __imp___findnext : UInt64;
  __imp___findnext64 : UInt64;
  __imp___findnexti64 : UInt64;
  __imp___finite : UInt64;
  __imp__finite : UInt64;
  __imp___findnext32 : UInt64;
  __imp___flsbuf : UInt64;
  __imp___flushall : UInt64;
  __imp___fmode : UInt64;
  __imp___fpclass : UInt64;
  __imp___fpieee_flt : UInt64;
  __imp___fpreset : UInt64;
  __imp___fprintf_l : UInt64;
  __imp__fpclass : UInt64;
  __imp___fprintf_p : UInt64;
  __imp___fprintf_p_l : UInt64;
  __imp___fprintf_s_l : UInt64;
  __imp__fputchar : UInt64;
  __imp___fputchar : UInt64;
  __imp__fputwchar : UInt64;
  __imp___fputwchar : UInt64;
  __imp___fsopen : UInt64;
  __imp___fstat : UInt64;
  __imp___fstat64 : UInt64;
  __imp___fstat32 : UInt64;
  __imp___fstati64 : UInt64;
  __imp___ftime32 : UInt64;
  __imp___ftime : UInt64;
  __imp___ftime32_s : UInt64;
  __imp___ftime64 : UInt64;
  __imp___ftime64_s : UInt64;
  __imp___ftol : UInt64;
  __imp___ftime_s : UInt64;
  __imp___fullpath : UInt64;
  __imp___futime64 : UInt64;
  __imp___fwprintf_l : UInt64;
  __imp___fwprintf_p : UInt64;
  __imp___fwprintf_p_l : UInt64;
  __imp___futime32 : UInt64;
  __imp___fwprintf_s_l : UInt64;
  __imp___fwscanf_l : UInt64;
  __imp___fwscanf_s_l : UInt64;
  __imp___gcvt : UInt64;
  __imp___gcvt_s : UInt64;
  __imp___get_heap_handle : UInt64;
  __imp___get_osfhandle : UInt64;
  __imp___get_sbh_threshold : UInt64;
  __imp__gcvt : UInt64;
  __imp__getch : UInt64;
  __imp___getch : UInt64;
  __imp___getche : UInt64;
  __imp___futime : UInt64;
  __imp__getche : UInt64;
  __imp__getcwd : UInt64;
  __imp___getcwd : UInt64;
  __imp___getdcwd : UInt64;
  __imp___getdiskfree : UInt64;
  __imp___getdllprocaddr : UInt64;
  __imp___getdrive : UInt64;
  __imp___getdrives : UInt64;
  __imp___getmaxstdio : UInt64;
  __imp___getmbcp : UInt64;
  __imp__getpid : UInt64;
  __imp__iscsym : UInt64;
  __imp___getpid : UInt64;
  __imp___getsystime : UInt64;
  __imp__getw : UInt64;
  __imp___getw : UInt64;
  __imp___getwch : UInt64;
  __imp___getwche : UInt64;
  __imp___getws : UInt64;
  __imp___global_unwind2 : UInt64;
  __imp___gmtime64 : UInt64;
  __imp___heapadd : UInt64;
  __imp___heapchk : UInt64;
  __imp___heapmin : UInt64;
  __imp___heapset : UInt64;
  __imp___heapused : UInt64;
  __imp__heapwalk : UInt64;
  __imp___heapwalk : UInt64;
  __imp__hypot : UInt64;
  __imp___hypot : UInt64;
  __imp___i64toa : UInt64;
  __imp___i64toa_s : UInt64;
  __imp___i64tow : UInt64;
  __imp___i64tow_s : UInt64;
  __imp___initterm : UInt64;
  __imp___inp : UInt64;
  __imp___inpd : UInt64;
  __imp___inpw : UInt64;
  __imp___iob : UInt64;
  __imp___isalnum_l : UInt64;
  __imp___isalpha_l : UInt64;
  __imp___isatty : UInt64;
  __imp__isatty : UInt64;
  __imp___iscntrl_l : UInt64;
  __imp___isctype : UInt64;
  __imp___isctype_l : UInt64;
  __imp___isdigit_l : UInt64;
  __imp___isgraph_l : UInt64;
  __imp___isleadbyte_l : UInt64;
  __imp___islower_l : UInt64;
  __imp___ismbbalnum : UInt64;
  __imp___ismbbalnum_l : UInt64;
  __imp___ismbbalpha : UInt64;
  __imp___ismbbalpha_l : UInt64;
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
  __imp___isnan : UInt64;
  __imp___isprint_l : UInt64;
  __imp___isspace_l : UInt64;
  __imp___isupper_l : UInt64;
  __imp___iswalnum_l : UInt64;
  __imp___iswalpha_l : UInt64;
  __imp___iswcntrl_l : UInt64;
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
  __imp___itoa : UInt64;
  __imp__itoa : UInt64;
  __imp___itoa_s : UInt64;
  __imp___itow : UInt64;
  __imp___itow_s : UInt64;
  __imp___j0 : UInt64;
  __imp___j1 : UInt64;
  __imp__j1 : UInt64;
  __imp__j0 : UInt64;
  __imp___jn : UInt64;
  __imp___kbhit : UInt64;
  __imp___lfind : UInt64;
  __imp__lfind : UInt64;
  __imp___loaddll : UInt64;
  __imp___local_unwind2 : UInt64;
  __imp__jn : UInt64;
  __imp__kbhit : UInt64;
  __imp___localtime64 : UInt64;
  __imp___lock : UInt64;
  __imp___locking : UInt64;
  __imp___logb : UInt64;
  __imp___longjmpex : UInt64;
  __imp___lrotl : UInt64;
  __imp___lrotr : UInt64;
  __imp__lsearch : UInt64;
  __imp___lsearch : UInt64;
  __imp___lseek : UInt64;
  __imp___lseeki64 : UInt64;
  __imp__ltoa : UInt64;
  __imp__lseek : UInt64;
  __imp___ltoa : UInt64;
  __imp___ltow : UInt64;
  __imp___makepath : UInt64;
  __imp___makepath_s : UInt64;
  __imp___mbbtombc : UInt64;
  __imp___mbbtombc_l : UInt64;
  __imp___mbbtype : UInt64;
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
  __imp___mbctype : UInt64;
  __imp___mblen_l : UInt64;
  __imp___mbsbtype : UInt64;
  __imp___mbsbtype_l : UInt64;
  __imp___mbscat : UInt64;
  __imp___mbscat_s : UInt64;
  __imp___mbscat_s_l : UInt64;
  __imp___mbschr : UInt64;
  __imp___mbschr_l : UInt64;
  __imp___mbscmp : UInt64;
  __imp___mbscmp_l : UInt64;
  __imp___mbscoll : UInt64;
  __imp___mbscoll_l : UInt64;
  __imp___mbscpy : UInt64;
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
  __imp___memccpy : UInt64;
  __imp___memicmp : UInt64;
  __imp___memicmp_l : UInt64;
  __imp__mkdir : UInt64;
  __imp___mkdir : UInt64;
  __imp___mkgmtime : UInt64;
  __imp___mkgmtime32 : UInt64;
  __imp___mkgmtime64 : UInt64;
  __imp__memicmp : UInt64;
  __imp__mktemp : UInt64;
  __imp___mktemp : UInt64;
  __imp___mktime64 : UInt64;
  __imp___msize : UInt64;
  __imp___nextafter : UInt64;
  __imp___onexit : UInt64;
  __imp__open : UInt64;
  __imp___open : UInt64;
  __imp___open_osfhandle : UInt64;
  __imp___osplatform : UInt64;
  __imp___osver : UInt64;
  __imp__nextafter : UInt64;
  __imp___outp : UInt64;
  __imp___outpd : UInt64;
  __imp___outpw : UInt64;
  __imp__pclose : UInt64;
  __imp___pclose : UInt64;
  __imp___pctype : UInt64;
  __imp___pgmptr : UInt64;
  __imp___pipe : UInt64;
  __imp___popen : UInt64;
  __imp__popen : UInt64;
  __imp___printf_l : UInt64;
  __imp___printf_p : UInt64;
  __imp___printf_p_l : UInt64;
  __imp___printf_s_l : UInt64;
  __imp___purecall : UInt64;
  __imp___putch : UInt64;
  __imp__putenv : UInt64;
  __imp___putenv : UInt64;
  __imp___putenv_s : UInt64;
  __imp__putw : UInt64;
  __imp__putch : UInt64;
  __imp___putw : UInt64;
  __imp___putwch : UInt64;
  __imp___putws : UInt64;
  __imp__memccpy : UInt64;
  __imp___pwctype : UInt64;
  __imp___read : UInt64;
  __imp___resetstkoflw : UInt64;
  __imp__rmdir : UInt64;
  __imp___rmdir : UInt64;
  __imp__rmtmp : UInt64;
  __imp___rmtmp : UInt64;
  __imp___rotl : UInt64;
  __imp___rotr : UInt64;
  __imp___safe_fdiv : UInt64;
  __imp___safe_fdivr : UInt64;
  __imp___safe_fprem : UInt64;
  __imp___safe_fprem1 : UInt64;
  __imp___scalb : UInt64;
  __imp___scanf_l : UInt64;
  __imp___scanf_s_l : UInt64;
  __imp___scprintf_l : UInt64;
  __imp___scprintf_p_l : UInt64;
  __imp___scwprintf : UInt64;
  __imp___scwprintf_l : UInt64;
  __imp___scwprintf_p_l : UInt64;
  __imp__searchenv : UInt64;
  __imp___searchenv : UInt64;
  __imp___searchenv_s : UInt64;
  __imp___seh_longjmp_unwind : UInt64;
  __imp___set_SSE2_enable : UInt64;
  __imp___set_error_mode : UInt64;
  __imp___set_sbh_threshold : UInt64;
  __imp___set_security_error_handler : UInt64;
  __imp___seterrormode : UInt64;
  __imp___setjmp : UInt64;
  __imp___setjmp3 : UInt64;
  __imp___setmaxstdio : UInt64;
  __imp___setmbcp : UInt64;
  __imp___setmode : UInt64;
  __imp___setsystime : UInt64;
  __imp___sleep : UInt64;
  __imp__setmode : UInt64;
  __imp___snprintf : UInt64;
  __imp___snprintf_c : UInt64;
  __imp___snprintf_c_l : UInt64;
  __imp___snprintf_l : UInt64;
  __imp___snprintf_s : UInt64;
  __imp___snprintf_s_l : UInt64;
  __imp___snscanf : UInt64;
  __imp___snscanf_l : UInt64;
  __imp___snscanf_s : UInt64;
  __imp___snscanf_s_l : UInt64;
  __imp__snwprintf : UInt64;
  __imp___snwprintf : UInt64;
  __imp___snwprintf_l : UInt64;
  __imp___snwprintf_s : UInt64;
  __imp___snwprintf_s_l : UInt64;
  __imp___snwscanf : UInt64;
  __imp___snwscanf_l : UInt64;
  __imp___snwscanf_s : UInt64;
  __imp___snwscanf_s_l : UInt64;
  __imp___sopen : UInt64;
  __imp___spawnl : UInt64;
  __imp___spawnle : UInt64;
  __imp___spawnlp : UInt64;
  __imp__spawnle : UInt64;
  __imp___spawnlpe : UInt64;
  __imp__spawnlpe : UInt64;
  __imp__spawnv : UInt64;
  __imp__spawnlp : UInt64;
  __imp___spawnv : UInt64;
  __imp__spawnl : UInt64;
  __imp__sopen : UInt64;
  __imp___spawnve : UInt64;
  __imp__spawnvp : UInt64;
  __imp__spawnve : UInt64;
  __imp___spawnvp : UInt64;
  __imp___spawnvpe : UInt64;
  __imp___splitpath : UInt64;
  __imp___splitpath_s : UInt64;
  __imp___sprintf_l : UInt64;
  __imp___sprintf_p_l : UInt64;
  __imp___sprintf_s_l : UInt64;
  __imp___sscanf_l : UInt64;
  __imp___sscanf_s_l : UInt64;
  __imp___stat32 : UInt64;
  __imp___stat : UInt64;
  __imp___stat64 : UInt64;
  __imp__spawnvpe : UInt64;
  __imp___stati64 : UInt64;
  __imp__read : UInt64;
  __imp___statusfp : UInt64;
  __imp__strcmpi : UInt64;
  __imp___strcmpi : UInt64;
  __imp___strcoll_l : UInt64;
  __imp___strdate : UInt64;
  __imp__strdup : UInt64;
  __imp___strdup : UInt64;
  __imp___strerror : UInt64;
  __imp___strerror_s : UInt64;
  __imp__stricmp : UInt64;
  __imp___stricmp : UInt64;
  __imp___stricmp_l : UInt64;
  __imp__strcasecmp : UInt64;
  __imp___stricoll : UInt64;
  __imp___stricoll_l : UInt64;
  __imp___strlwr : UInt64;
  __imp__strlwr : UInt64;
  __imp___strlwr_l : UInt64;
  __imp___strlwr_s : UInt64;
  __imp___strlwr_s_l : UInt64;
  __imp___strncoll : UInt64;
  __imp___strncoll_l : UInt64;
  __imp__strnicmp : UInt64;
  __imp___strnicmp : UInt64;
  __imp__strncasecmp : UInt64;
  __imp___strnicmp_l : UInt64;
  __imp___strnicoll : UInt64;
  __imp___strnicoll_l : UInt64;
  __imp___strnset : UInt64;
  __imp___strnset_s : UInt64;
  __imp__strrev : UInt64;
  __imp___strrev : UInt64;
  __imp__strset : UInt64;
  __imp___strset : UInt64;
  __imp___strset_s : UInt64;
  __imp__strnset : UInt64;
  __imp__stricoll : UInt64;
  __imp___strtime : UInt64;
  __imp___strtod_l : UInt64;
  __imp___strtoi64 : UInt64;
  __imp___strtoi64_l : UInt64;
  __imp___strtol_l : UInt64;
  __imp___strtoui64 : UInt64;
  __imp___strtoui64_l : UInt64;
  __imp___strtoul_l : UInt64;
  __imp___strupr : UInt64;
  __imp__strupr : UInt64;
  __imp___strupr_l : UInt64;
  __imp___strupr_s : UInt64;
  __imp___strupr_s_l : UInt64;
  __imp___strxfrm_l : UInt64;
  __imp__swab : UInt64;
  __imp___swab : UInt64;
  __imp___swprintf_c_l : UInt64;
  __imp___swprintf_p_l : UInt64;
  __imp___swprintf_s_l : UInt64;
  __imp___swscanf_l : UInt64;
  __imp___swscanf_s_l : UInt64;
  __imp___sys_errlist : UInt64;
  __imp___sys_nerr : UInt64;
  __imp__tell : UInt64;
  __imp___tell : UInt64;
  __imp___telli64 : UInt64;
  __imp___tempnam : UInt64;
  __imp___time64 : UInt64;
  __imp___timezone : UInt64;
  __imp__tempnam : UInt64;
  __imp__timezone : UInt64;
  __imp___tolower : UInt64;
  __imp___tolower_l : UInt64;
  __imp___toupper : UInt64;
  __imp___toupper_l : UInt64;
  __imp___towlower_l : UInt64;
  __imp___towupper_l : UInt64;
  __imp___tzname : UInt64;
  __imp___tzset : UInt64;
  __imp___ui64toa : UInt64;
  __imp___ui64toa_s : UInt64;
  __imp__tzset : UInt64;
  __imp__tzname : UInt64;
  __imp___ui64tow : UInt64;
  __imp___ui64tow_s : UInt64;
  __imp___ultoa : UInt64;
  __imp___ultoa_s : UInt64;
  __imp___ultow : UInt64;
  __imp___ultow_s : UInt64;
  __imp___umask : UInt64;
  __imp___ungetch : UInt64;
  __imp___ungetwch : UInt64;
  __imp___unlink : UInt64;
  __imp__unlink : UInt64;
  __imp___unloaddll : UInt64;
  __imp___unlock : UInt64;
  __imp___utime32 : UInt64;
  __imp___utime : UInt64;
  __imp___utime64 : UInt64;
  __imp___vcprintf : UInt64;
  __imp___vcprintf_l : UInt64;
  __imp___vcprintf_p : UInt64;
  __imp___vcprintf_p_l : UInt64;
  __imp___vcwprintf : UInt64;
  __imp___vcwprintf_l : UInt64;
  __imp___vcwprintf_p : UInt64;
  __imp___vcwprintf_p_l : UInt64;
  __imp___vfprintf_l : UInt64;
  __imp__umask : UInt64;
  __imp___vfprintf_p : UInt64;
  __imp___vfprintf_p_l : UInt64;
  __imp___vfprintf_s_l : UInt64;
  __imp___vfwprintf_l : UInt64;
  __imp___vfwprintf_p : UInt64;
  __imp___vfwprintf_p_l : UInt64;
  __imp___vfwprintf_s_l : UInt64;
  __imp___vprintf_l : UInt64;
  __imp___vprintf_p : UInt64;
  __imp___vprintf_p_l : UInt64;
  __imp___vprintf_s_l : UInt64;
  __imp___vscprintf_l : UInt64;
  __imp___vscprintf_p_l : UInt64;
  __imp___vscwprintf : UInt64;
  __imp__utime : UInt64;
  __imp___vscwprintf_l : UInt64;
  __imp___vscwprintf_p_l : UInt64;
  __imp___vsnprintf : UInt64;
  __imp___vsnprintf_c : UInt64;
  __imp___vsnprintf_c_l : UInt64;
  __imp___vsnprintf_l : UInt64;
  __imp___vsnprintf_s : UInt64;
  __imp___vsnprintf_s_l : UInt64;
  __imp__vsnwprintf : UInt64;
  __imp___vsnwprintf : UInt64;
  __imp___vsnwprintf_l : UInt64;
  __imp___vsnwprintf_s : UInt64;
  __imp___vsnwprintf_s_l : UInt64;
  __imp___vsprintf_l : UInt64;
  __imp___vsprintf_p : UInt64;
  __imp___vsprintf_p_l : UInt64;
  __imp___vsprintf_s_l : UInt64;
  __imp___vswprintf_c : UInt64;
  __imp___vswprintf_c_l : UInt64;
  __imp___vswprintf_l : UInt64;
  __imp___vswprintf_p_l : UInt64;
  __imp___vswprintf_s_l : UInt64;
  __imp___vwprintf_l : UInt64;
  __imp___vwprintf_p : UInt64;
  __imp___vwprintf_p_l : UInt64;
  __imp___vwprintf_s_l : UInt64;
  __imp___waccess : UInt64;
  __imp___wasctime : UInt64;
  __imp___wchdir : UInt64;
  __imp___wchmod : UInt64;
  __imp___wcmdln : UInt64;
  __imp___wcreat : UInt64;
  __imp___wcscoll_l : UInt64;
  __imp___wcsdup : UInt64;
  __imp___wcserror : UInt64;
  __imp___wcserror_s : UInt64;
  __imp___wcsftime_l : UInt64;
  __imp__wcsdup : UInt64;
  __imp__wcscmpi : UInt64;
  __imp__wcsicmp : UInt64;
  __imp___wcsicmp : UInt64;
  __imp___wcsicmp_l : UInt64;
  __imp__wcsicoll : UInt64;
  __imp___wcsicoll : UInt64;
  __imp___wcsicoll_l : UInt64;
  __imp__wcslwr : UInt64;
  __imp___wcslwr : UInt64;
  __imp___wcslwr_l : UInt64;
  __imp___wcslwr_s : UInt64;
  __imp___wcslwr_s_l : UInt64;
  __imp___wcsncoll : UInt64;
  __imp___wcsncoll_l : UInt64;
  __imp___wcsnicmp : UInt64;
  __imp___wcsnicmp_l : UInt64;
  __imp___wcsnicoll : UInt64;
  __imp__wcsnicmp : UInt64;
  __imp___wcsnicoll_l : UInt64;
  __imp___wcsnset : UInt64;
  __imp___wcsnset_s : UInt64;
  __imp__wcsrev : UInt64;
  __imp___wcsrev : UInt64;
  __imp__wcsnset : UInt64;
  __imp___wcsset : UInt64;
  __imp__wcsset : UInt64;
  __imp___wcsset_s : UInt64;
  __imp___wcstoi64 : UInt64;
  __imp___wcstoi64_l : UInt64;
  __imp___wcstol_l : UInt64;
  __imp___wcstombs_l : UInt64;
  __imp___wcstombs_s_l : UInt64;
  __imp___wcstoui64 : UInt64;
  __imp___wcstoui64_l : UInt64;
  __imp___wcstoul_l : UInt64;
  __imp__wcsupr : UInt64;
  __imp___wcsupr : UInt64;
  __imp___wcsupr_l : UInt64;
  __imp___wcsupr_s : UInt64;
  __imp___wcsupr_s_l : UInt64;
  __imp___wcsxfrm_l : UInt64;
  __imp___wctime32 : UInt64;
  __imp___wctime : UInt64;
  __imp___wctime64 : UInt64;
  __imp___wctomb_l : UInt64;
  __imp___wctomb_s_l : UInt64;
  __imp___wctype : UInt64;
  __imp___wenviron : UInt64;
  __imp___wexecl : UInt64;
  __imp___wexecle : UInt64;
  __imp___wexeclp : UInt64;
  __imp___wexeclpe : UInt64;
  __imp___wexecv : UInt64;
  __imp___wexecve : UInt64;
  __imp___wexecvp : UInt64;
  __imp___wexecvpe : UInt64;
  __imp___wfdopen : UInt64;
  __imp___wfindfirst32i64 : UInt64;
  __imp___wfindfirst : UInt64;
  __imp___wfindfirst64 : UInt64;
  __imp___wfindfirsti64 : UInt64;
  __imp___wfindnext : UInt64;
  __imp___wfindfirst32 : UInt64;
  __imp___wfindnext32i64 : UInt64;
  __imp___wfindnext64 : UInt64;
  __imp___wfindnexti64 : UInt64;
  __imp___wfindnext32 : UInt64;
  __imp___wfopen : UInt64;
  __imp___wfopen_s : UInt64;
  __imp___wfreopen : UInt64;
  __imp___wfreopen_s : UInt64;
  __imp___wfsopen : UInt64;
  __imp___wfullpath : UInt64;
  __imp___wgetcwd : UInt64;
  __imp___wgetdcwd : UInt64;
  __imp___wgetenv : UInt64;
  __imp___wgetenv_s : UInt64;
  __imp___winmajor : UInt64;
  __imp___winminor : UInt64;
  __imp___winput_s : UInt64;
  __imp___winver : UInt64;
  __imp___wmakepath : UInt64;
  __imp___wmakepath_s : UInt64;
  __imp___wmkdir : UInt64;
  __imp__ungetch : UInt64;
  __imp__vsnprintf_s : UInt64;
  __imp___wmktemp : UInt64;
  __imp___wopen : UInt64;
  __imp___woutput_s : UInt64;
  __imp___wperror : UInt64;
  __imp___wpgmptr : UInt64;
  __imp__wpopen : UInt64;
  __imp___wpopen : UInt64;
  __imp___wprintf_l : UInt64;
  __imp___wprintf_p : UInt64;
  __imp___wprintf_p_l : UInt64;
  __imp___wprintf_s_l : UInt64;
  __imp___wputenv : UInt64;
  __imp___wputenv_s : UInt64;
  __imp___wremove : UInt64;
  __imp___wrename : UInt64;
  __imp___write : UInt64;
  __imp___wrmdir : UInt64;
  __imp___wscanf_l : UInt64;
  __imp___wscanf_s_l : UInt64;
  __imp___wsearchenv : UInt64;
  __imp___wsearchenv_s : UInt64;
  __imp___wsetlocale : UInt64;
  __imp__write : UInt64;
  __imp___wsopen : UInt64;
  __imp___wsopen_s : UInt64;
  __imp___wspawnl : UInt64;
  __imp___wspawnle : UInt64;
  __imp___wspawnlp : UInt64;
  __imp___wspawnlpe : UInt64;
  __imp___wspawnv : UInt64;
  __imp___wspawnve : UInt64;
  __imp___wspawnvp : UInt64;
  __imp___wspawnvpe : UInt64;
  __imp___wsplitpath : UInt64;
  __imp___wsplitpath_s : UInt64;
  __imp___wstat32 : UInt64;
  __imp___wstat : UInt64;
  __imp___wstat64 : UInt64;
  __imp___wstati64 : UInt64;
  __imp___wstrdate : UInt64;
  __imp___wstrtime : UInt64;
  __imp___wsystem : UInt64;
  __imp___wtempnam : UInt64;
  __imp___wtmpnam : UInt64;
  __imp___wtmpnam_s : UInt64;
  __imp___wtof : UInt64;
  __imp___wtof_l : UInt64;
  __imp___wtoi : UInt64;
  __imp___wtoi64 : UInt64;
  __imp___wtoi64_l : UInt64;
  __imp___wtoi_l : UInt64;
  __imp___wtol : UInt64;
  __imp___wtol_l : UInt64;
  __imp___wunlink : UInt64;
  __imp___wutime32 : UInt64;
  __imp___wutime : UInt64;
  __imp___wutime64 : UInt64;
  __imp__y0 : UInt64;
  __imp___y0 : UInt64;
  __imp__y1 : UInt64;
  __imp___y1 : UInt64;
  __imp___yn : UInt64;
  __imp__yn : UInt64;
  __imp__abort : UInt64;
  __imp__abs : UInt64;
  __imp__acos : UInt64;
  __imp__asctime : UInt64;
  __imp__asin : UInt64;
  __imp__atan : UInt64;
  __imp__atan2 : UInt64;
  __imp__atexit : UInt64;
  __imp__atof : UInt64;
  __imp__atoi : UInt64;
  __imp__atol : UInt64;
  __imp__bsearch : UInt64;
  __imp__bsearch_s : UInt64;
  __imp__calloc : UInt64;
  __imp__ceil : UInt64;
  __imp__clearerr : UInt64;
  __imp___difftime32 : UInt64;
  __imp__clearerr_s : UInt64;
  __imp__clock : UInt64;
  __imp__cos : UInt64;
  __imp___ctime32 : UInt64;
  __imp__cosh : UInt64;
  __imp__ctime : UInt64;
  __imp__difftime : UInt64;
  __imp__div : UInt64;
  __imp__exit : UInt64;
  __imp__exp : UInt64;
  __imp__fabs : UInt64;
  __imp__fclose : UInt64;
  __imp__feof : UInt64;
  __imp__ferror : UInt64;
  __imp__fflush : UInt64;
  __imp__fgetc : UInt64;
  __imp__fgetpos : UInt64;
  __imp__fgets : UInt64;
  __imp__fgetwc : UInt64;
  __imp__fgetws : UInt64;
  __imp__floor : UInt64;
  __imp__fmod : UInt64;
  __imp____ms_fscanf : UInt64;
  __imp__fopen : UInt64;
  __imp__fopen_s : UInt64;
  __imp____ms_fprintf : UInt64;
  __imp__fprintf : UInt64;
  __imp__fprintf_s : UInt64;
  __imp__fputc : UInt64;
  __imp__fputs : UInt64;
  __imp__fputwc : UInt64;
  __imp__fputws : UInt64;
  __imp__fread : UInt64;
  __imp__free : UInt64;
  __imp__freopen : UInt64;
  __imp__freopen_s : UInt64;
  __imp__frexp : UInt64;
  __imp__fscanf : UInt64;
  __imp__fscanf_s : UInt64;
  __imp____ms_fwprintf : UInt64;
  __imp__fseek : UInt64;
  __imp__fsetpos : UInt64;
  __imp__ftell : UInt64;
  __imp__fwprintf : UInt64;
  __imp____ms_fwscanf : UInt64;
  __imp__fwprintf_s : UInt64;
  __imp__fwrite : UInt64;
  __imp__fwscanf : UInt64;
  __imp__fwscanf_s : UInt64;
  __imp__getc : UInt64;
  __imp__getchar : UInt64;
  __imp___gmtime32 : UInt64;
  __imp__getenv : UInt64;
  __imp__getenv_s : UInt64;
  __imp__gets : UInt64;
  __imp__getwc : UInt64;
  __imp__getwchar : UInt64;
  __imp__gmtime : UInt64;
  __imp__is_wctype : UInt64;
  __imp__isalnum : UInt64;
  __imp__isalpha : UInt64;
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
  __imp__labs : UInt64;
  __imp__ldexp : UInt64;
  __imp__localeconv : UInt64;
  __imp___localtime32 : UInt64;
  __imp__ldiv : UInt64;
  __imp__localtime : UInt64;
  __imp__log : UInt64;
  __imp__log10 : UInt64;
  __imp__longjmp : UInt64;
  __imp__malloc : UInt64;
  __imp__mblen : UInt64;
  __imp__perror : UInt64;
  __imp____ms_printf : UInt64;
  __imp___mktime32 : UInt64;
  __imp__mbsrtowcs_s : UInt64;
  __imp__mbstowcs : UInt64;
  __imp__mbstowcs_s : UInt64;
  __imp__mbtowc : UInt64;
  __imp__memchr : UInt64;
  __imp__memcmp : UInt64;
  __imp__memcpy : UInt64;
  __imp__memmove : UInt64;
  __imp__memset : UInt64;
  __imp__mktime : UInt64;
  __imp__modf : UInt64;
  __imp__pow : UInt64;
  __imp__printf : UInt64;
  __imp__printf_s : UInt64;
  __imp__putc : UInt64;
  __imp__putchar : UInt64;
  __imp__puts : UInt64;
  __imp__putwc : UInt64;
  __imp__putwchar : UInt64;
  __imp__qsort : UInt64;
  __imp__qsort_s : UInt64;
  __imp__raise : UInt64;
  __imp__rand : UInt64;
  __imp__realloc : UInt64;
  __imp__remove : UInt64;
  __imp____ms_sprintf : UInt64;
  __imp__rename : UInt64;
  __imp____ms_scanf : UInt64;
  __imp__rewind : UInt64;
  __imp__scanf : UInt64;
  __imp__scanf_s : UInt64;
  __imp__setbuf : UInt64;
  __imp__setlocale : UInt64;
  __imp__setvbuf : UInt64;
  __imp__signal : UInt64;
  __imp__sin : UInt64;
  __imp__sinh : UInt64;
  __imp__sprintf : UInt64;
  __imp__srand : UInt64;
  __imp____ms_sscanf : UInt64;
  __imp__sqrt : UInt64;
  __imp__sscanf : UInt64;
  __imp__sscanf_s : UInt64;
  __imp__strcat : UInt64;
  __imp__strcat_s : UInt64;
  __imp__strchr : UInt64;
  __imp__strcmp : UInt64;
  __imp__strcoll : UInt64;
  __imp__strcpy : UInt64;
  __imp__strcpy_s : UInt64;
  __imp__strcspn : UInt64;
  __imp__strerror : UInt64;
  __imp__strftime : UInt64;
  __imp__strlen : UInt64;
  __imp__strncat : UInt64;
  __imp__strncat_s : UInt64;
  __imp__strncmp : UInt64;
  __imp__strncpy : UInt64;
  __imp__strncpy_s : UInt64;
  __imp__strpbrk : UInt64;
  __imp__strrchr : UInt64;
  __imp__strspn : UInt64;
  __imp__strstr : UInt64;
  __imp__strtod : UInt64;
  __imp__strtok : UInt64;
  __imp__strtok_s : UInt64;
  __imp__strtol : UInt64;
  __imp__strtoul : UInt64;
  __imp__strxfrm : UInt64;
  __imp___swprintf : UInt64;
  __imp__swprintf : UInt64;
  __imp____ms_swprintf : UInt64;
  __imp__swprintf_s : UInt64;
  __imp___time32 : UInt64;
  __imp____ms_swscanf : UInt64;
  __imp__swscanf : UInt64;
  __imp__swscanf_s : UInt64;
  __imp__system : UInt64;
  __imp__tan : UInt64;
  __imp__tanh : UInt64;
  __imp__time : UInt64;
  __imp__tmpfile : UInt64;
  __imp__tmpfile_s : UInt64;
  __imp__tmpnam : UInt64;
  __imp__tmpnam_s : UInt64;
  __imp__tolower : UInt64;
  __imp__toupper : UInt64;
  __imp__towlower : UInt64;
  __imp__towupper : UInt64;
  __imp__ungetc : UInt64;
  __imp__ungetwc : UInt64;
  __imp____ms_vprintf : UInt64;
  __imp____ms_vfprintf : UInt64;
  __imp__vfprintf : UInt64;
  __imp____ms_vfwprintf : UInt64;
  __imp__vfprintf_s : UInt64;
  __imp__vfwprintf : UInt64;
  __imp__vfwprintf_s : UInt64;
  __imp__vprintf : UInt64;
  __imp__vprintf_s : UInt64;
  __imp____ms_vswprintf : UInt64;
  __imp____ms_vsprintf : UInt64;
  __imp__vsprintf : UInt64;
  __imp___vswprintf : UInt64;
  __imp__vswprintf : UInt64;
  __imp__vswprintf_s : UInt64;
  __imp____ms_vwprintf : UInt64;
  __imp__vwprintf : UInt64;
  __imp__vwprintf_s : UInt64;
  __imp__wcrtomb_s : UInt64;
  __imp__wcscat : UInt64;
  __imp__wcscat_s : UInt64;
  __imp__wcschr : UInt64;
  __imp__wcscmp : UInt64;
  __imp__wcscoll : UInt64;
  __imp__wcscpy : UInt64;
  __imp__wcscpy_s : UInt64;
  __imp__wcscspn : UInt64;
  __imp__wcsftime : UInt64;
  __imp__wcslen : UInt64;
  __imp__wcsncat : UInt64;
  __imp__wcsncat_s : UInt64;
  __imp__wcsncmp : UInt64;
  __imp__wcsncpy : UInt64;
  __imp__wcsncpy_s : UInt64;
  __imp__wcsnlen : UInt64;
  __imp__wcspbrk : UInt64;
  __imp__wcsrchr : UInt64;
  __imp__wcsrtombs_s : UInt64;
  __imp__wcsspn : UInt64;
  __imp__wcsstr : UInt64;
  __imp__wcstod : UInt64;
  __imp__wcstok : UInt64;
  __imp__wcstok_s : UInt64;
  __imp__wcstol : UInt64;
  __imp__wcstombs_s : UInt64;
  __imp__wcstoul : UInt64;
  __imp__wcsxfrm : UInt64;
  __imp__wctomb : UInt64;
  __imp__wctomb_s : UInt64;
  __imp____ms_wprintf : UInt64;
  __imp__wcstombs : UInt64;
  __imp__wprintf : UInt64;
  __imp____ms_wscanf : UInt64;
  __imp__wprintf_s : UInt64;
  __imp__wscanf : UInt64;
  __imp__wscanf_s : UInt64;
  __head_lib32_libmsvcrt_def_a : UInt64;
  __lib32_libmsvcrt_def_a_iname : UInt64;
{$ENDIF}
implementation
end.
