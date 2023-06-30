unit libvcruntime140;
interface
{$IFDEF WIN64}
procedure wcsstr;external;
{$L x64/libvcruntime140_apps00070.o}
procedure wcsrchr;external;
{$L x64/libvcruntime140_apps00069.o}
procedure wcschr;external;
{$L x64/libvcruntime140_apps00068.o}
procedure unexpected;external;
{$L x64/libvcruntime140_apps00067.o}
procedure strstr;external;
{$L x64/libvcruntime140_apps00066.o}
procedure strrchr;external;
{$L x64/libvcruntime140_apps00065.o}
procedure strchr;external;
{$L x64/libvcruntime140_apps00064.o}
procedure set_unexpected;external;
{$L x64/libvcruntime140_apps00063.o}
procedure memset;external;
{$L x64/libvcruntime140_apps00062.o}
procedure memmove;external;
{$L x64/libvcruntime140_apps00061.o}
procedure memcpy;external;
{$L x64/libvcruntime140_apps00060.o}
procedure memcmp;external;
{$L x64/libvcruntime140_apps00059.o}
procedure memchr;external;
{$L x64/libvcruntime140_apps00058.o}
procedure longjmp;external;
{$L x64/libvcruntime140_apps00057.o}
procedure _set_se_translator;external;
{$L x64/libvcruntime140_apps00056.o}
procedure _set_purecall_handler;external;
{$L x64/libvcruntime140_apps00055.o}
procedure _purecall;external;
{$L x64/libvcruntime140_apps00054.o}
procedure _local_unwind;external;
{$L x64/libvcruntime140_apps00053.o}
procedure _is_exception_typeof;external;
{$L x64/libvcruntime140_apps00052.o}
procedure _get_unexpected;external;
{$L x64/libvcruntime140_apps00051.o}
procedure _get_purecall_handler;external;
{$L x64/libvcruntime140_apps00050.o}
procedure __vcrt_LoadLibraryExW;external;
{$L x64/libvcruntime140_apps00049.o}
procedure __vcrt_InitializeCriticalSectionEx;external;
{$L x64/libvcruntime140_apps00048.o}
procedure __vcrt_GetModuleHandleW;external;
{$L x64/libvcruntime140_apps00047.o}
procedure __vcrt_GetModuleFileNameW;external;
{$L x64/libvcruntime140_apps00046.o}
procedure __uncaught_exceptions;external;
{$L x64/libvcruntime140_apps00045.o}
procedure __uncaught_exception;external;
{$L x64/libvcruntime140_apps00044.o}
procedure __unDNameEx;external;
{$L x64/libvcruntime140_apps00043.o}
procedure __unDName;external;
{$L x64/libvcruntime140_apps00042.o}
procedure __telemetry_main_return_trigger;external;
{$L x64/libvcruntime140_apps00041.o}
procedure __telemetry_main_invoke_trigger;external;
{$L x64/libvcruntime140_apps00040.o}
procedure __std_type_info_name;external;
{$L x64/libvcruntime140_apps00039.o}
procedure __std_type_info_hash;external;
{$L x64/libvcruntime140_apps00038.o}
procedure __std_type_info_destroy_list;external;
{$L x64/libvcruntime140_apps00037.o}
procedure __std_type_info_compare;external;
{$L x64/libvcruntime140_apps00036.o}
procedure __std_terminate;external;
{$L x64/libvcruntime140_apps00035.o}
procedure __std_exception_destroy;external;
{$L x64/libvcruntime140_apps00034.o}
procedure __std_exception_copy;external;
{$L x64/libvcruntime140_apps00033.o}
procedure __report_gsfailure;external;
{$L x64/libvcruntime140_apps00032.o}
procedure __processing_throw;external;
{$L x64/libvcruntime140_apps00031.o}
procedure __intrinsic_setjmpex;external;
{$L x64/libvcruntime140_apps00030.o}
procedure __intrinsic_setjmp;external;
{$L x64/libvcruntime140_apps00029.o}
procedure __current_exception_context;external;
{$L x64/libvcruntime140_apps00028.o}
procedure __current_exception;external;
{$L x64/libvcruntime140_apps00027.o}
procedure __TypeMatch;external;
{$L x64/libvcruntime140_apps00026.o}
procedure __RTtypeid;external;
{$L x64/libvcruntime140_apps00025.o}
procedure __RTDynamicCast;external;
{$L x64/libvcruntime140_apps00024.o}
procedure __RTCastToVoid;external;
{$L x64/libvcruntime140_apps00023.o}
procedure __NLG_Return2;external;
{$L x64/libvcruntime140_apps00022.o}
procedure __NLG_Dispatch2;external;
{$L x64/libvcruntime140_apps00021.o}
procedure __GetPlatformExceptionInfo;external;
{$L x64/libvcruntime140_apps00020.o}
procedure __FrameUnwindFilter;external;
{$L x64/libvcruntime140_apps00019.o}
procedure __DestructExceptionObject;external;
{$L x64/libvcruntime140_apps00018.o}
procedure __CxxUnregisterExceptionObject;external;
{$L x64/libvcruntime140_apps00017.o}
procedure __CxxRegisterExceptionObject;external;
{$L x64/libvcruntime140_apps00016.o}
procedure __CxxQueryExceptionSize;external;
{$L x64/libvcruntime140_apps00015.o}
procedure __CxxFrameHandler3;external;
{$L x64/libvcruntime140_apps00014.o}
procedure __CxxFrameHandler2;external;
{$L x64/libvcruntime140_apps00013.o}
procedure __CxxFrameHandler;external;
{$L x64/libvcruntime140_apps00012.o}
procedure __CxxExceptionFilter;external;
{$L x64/libvcruntime140_apps00011.o}
procedure __CxxDetectRethrow;external;
{$L x64/libvcruntime140_apps00010.o}
procedure __C_specific_handler_noexcept;external;
{$L x64/libvcruntime140_apps00009.o}
procedure __C_specific_handler;external;
{$L x64/libvcruntime140_apps00008.o}
procedure __BuildCatchObjectHelper;external;
{$L x64/libvcruntime140_apps00007.o}
procedure __BuildCatchObject;external;
{$L x64/libvcruntime140_apps00006.o}
procedure __AdjustPointer;external;
{$L x64/libvcruntime140_apps00005.o}
procedure _SetWinRTOutOfMemoryExceptionCallback;external;
{$L x64/libvcruntime140_apps00004.o}
procedure _IsExceptionObjectToBeDestroyed;external;
{$L x64/libvcruntime140_apps00003.o}
procedure _FindAndUnlinkFrame;external;
{$L x64/libvcruntime140_apps00002.o}
procedure _CxxThrowException;external;
{$L x64/libvcruntime140_apps00001.o}
procedure _CreateFrameInfo;external;
{$L x64/libvcruntime140_apps00000.o}
var
  __lib64_libvcruntime140_app_a_iname : UInt64;
  _head_lib64_libvcruntime140_app_a : UInt64;
  __imp_wcsstr : UInt64;
  __imp_wcsrchr : UInt64;
  __imp_wcschr : UInt64;
  __imp_unexpected : UInt64;
  __imp_strstr : UInt64;
  __imp_strrchr : UInt64;
  __imp_strchr : UInt64;
  __imp_set_unexpected : UInt64;
  __imp_memset : UInt64;
  __imp_memmove : UInt64;
  __imp_memcpy : UInt64;
  __imp_memcmp : UInt64;
  __imp_memchr : UInt64;
  __imp_longjmp : UInt64;
  __imp__set_se_translator : UInt64;
  __imp__set_purecall_handler : UInt64;
  __imp__purecall : UInt64;
  __imp__local_unwind : UInt64;
  __imp__is_exception_typeof : UInt64;
  __imp__get_unexpected : UInt64;
  __imp__get_purecall_handler : UInt64;
  __imp___vcrt_LoadLibraryExW : UInt64;
  __imp___vcrt_InitializeCriticalSectionEx : UInt64;
  __imp___vcrt_GetModuleHandleW : UInt64;
  __imp___vcrt_GetModuleFileNameW : UInt64;
  __imp___uncaught_exceptions : UInt64;
  __imp___uncaught_exception : UInt64;
  __imp___unDNameEx : UInt64;
  __imp___unDName : UInt64;
  __imp___telemetry_main_return_trigger : UInt64;
  __imp___telemetry_main_invoke_trigger : UInt64;
  __imp___std_type_info_name : UInt64;
  __imp___std_type_info_hash : UInt64;
  __imp___std_type_info_destroy_list : UInt64;
  __imp___std_type_info_compare : UInt64;
  __imp___std_terminate : UInt64;
  __imp___std_exception_destroy : UInt64;
  __imp___std_exception_copy : UInt64;
  __imp___report_gsfailure : UInt64;
  __imp___processing_throw : UInt64;
  __imp___intrinsic_setjmpex : UInt64;
  __imp___intrinsic_setjmp : UInt64;
  __imp___current_exception_context : UInt64;
  __imp___current_exception : UInt64;
  __imp___TypeMatch : UInt64;
  __imp___RTtypeid : UInt64;
  __imp___RTDynamicCast : UInt64;
  __imp___RTCastToVoid : UInt64;
  __imp___NLG_Return2 : UInt64;
  __imp___NLG_Dispatch2 : UInt64;
  __imp___GetPlatformExceptionInfo : UInt64;
  __imp___FrameUnwindFilter : UInt64;
  __imp___DestructExceptionObject : UInt64;
  __imp___CxxUnregisterExceptionObject : UInt64;
  __imp___CxxRegisterExceptionObject : UInt64;
  __imp___CxxQueryExceptionSize : UInt64;
  __imp___CxxFrameHandler3 : UInt64;
  __imp___CxxFrameHandler2 : UInt64;
  __imp___CxxFrameHandler : UInt64;
  __imp___CxxExceptionFilter : UInt64;
  __imp___CxxDetectRethrow : UInt64;
  __imp___C_specific_handler_noexcept : UInt64;
  __imp___C_specific_handler : UInt64;
  __imp___BuildCatchObjectHelper : UInt64;
  __imp___BuildCatchObject : UInt64;
  __imp___AdjustPointer : UInt64;
  __imp__SetWinRTOutOfMemoryExceptionCallback : UInt64;
  __imp__IsExceptionObjectToBeDestroyed : UInt64;
  __imp__FindAndUnlinkFrame : UInt64;
  __imp__CxxThrowException : UInt64;
  __imp__CreateFrameInfo : UInt64;
{$ELSE}
procedure _wcsstr;external;
{$L x86/libvcruntime140_apps00080.o}
procedure _wcsrchr;external;
{$L x86/libvcruntime140_apps00079.o}
procedure _wcschr;external;
{$L x86/libvcruntime140_apps00078.o}
procedure _unexpected;external;
{$L x86/libvcruntime140_apps00077.o}
procedure _strstr;external;
{$L x86/libvcruntime140_apps00076.o}
procedure _strrchr;external;
{$L x86/libvcruntime140_apps00075.o}
procedure _strchr;external;
{$L x86/libvcruntime140_apps00074.o}
procedure _set_unexpected;external;
{$L x86/libvcruntime140_apps00073.o}
procedure _memset;external;
{$L x86/libvcruntime140_apps00072.o}
procedure _memmove;external;
{$L x86/libvcruntime140_apps00071.o}
procedure _memcpy;external;
{$L x86/libvcruntime140_apps00070.o}
procedure _memcmp;external;
{$L x86/libvcruntime140_apps00069.o}
procedure _memchr;external;
{$L x86/libvcruntime140_apps00068.o}
procedure _longjmp;external;
{$L x86/libvcruntime140_apps00067.o}
procedure __setjmp3;external;
{$L x86/libvcruntime140_apps00066.o}
procedure __set_se_translator;external;
{$L x86/libvcruntime140_apps00065.o}
procedure __set_purecall_handler;external;
{$L x86/libvcruntime140_apps00064.o}
procedure __purecall;external;
{$L x86/libvcruntime140_apps00061.o}
procedure __longjmpex;external;
{$L x86/libvcruntime140_apps00060.o}
procedure __local_unwind4;external;
{$L x86/libvcruntime140_apps00059.o}
procedure __local_unwind2;external;
{$L x86/libvcruntime140_apps00058.o}
procedure __is_exception_typeof;external;
{$L x86/libvcruntime140_apps00057.o}
procedure __global_unwind2;external;
{$L x86/libvcruntime140_apps00056.o}
procedure __get_unexpected;external;
{$L x86/libvcruntime140_apps00055.o}
procedure __get_purecall_handler;external;
{$L x86/libvcruntime140_apps00054.o}
procedure __except_handler4_common;external;
{$L x86/libvcruntime140_apps00053.o}
procedure __except_handler3;external;
{$L x86/libvcruntime140_apps00052.o}
procedure __except_handler2;external;
{$L x86/libvcruntime140_apps00051.o}
procedure __chkesp;external;
{$L x86/libvcruntime140_apps00050.o}
procedure ___vcrt_LoadLibraryExW;external;
{$L x86/libvcruntime140_apps00049.o}
procedure ___vcrt_InitializeCriticalSectionEx;external;
{$L x86/libvcruntime140_apps00048.o}
procedure ___vcrt_GetModuleHandleW;external;
{$L x86/libvcruntime140_apps00047.o}
procedure ___vcrt_GetModuleFileNameW;external;
{$L x86/libvcruntime140_apps00046.o}
procedure ___uncaught_exceptions;external;
{$L x86/libvcruntime140_apps00045.o}
procedure ___uncaught_exception;external;
{$L x86/libvcruntime140_apps00044.o}
procedure ___unDNameEx;external;
{$L x86/libvcruntime140_apps00043.o}
procedure ___unDName;external;
{$L x86/libvcruntime140_apps00042.o}
procedure ___telemetry_main_return_trigger;external;
{$L x86/libvcruntime140_apps00041.o}
procedure ___telemetry_main_invoke_trigger;external;
{$L x86/libvcruntime140_apps00040.o}
procedure ___std_type_info_name;external;
{$L x86/libvcruntime140_apps00039.o}
procedure ___std_type_info_hash;external;
{$L x86/libvcruntime140_apps00038.o}
procedure ___std_type_info_destroy_list;external;
{$L x86/libvcruntime140_apps00037.o}
procedure ___std_type_info_compare;external;
{$L x86/libvcruntime140_apps00036.o}
procedure ___std_terminate;external;
{$L x86/libvcruntime140_apps00035.o}
procedure ___std_exception_destroy;external;
{$L x86/libvcruntime140_apps00034.o}
procedure ___std_exception_copy;external;
{$L x86/libvcruntime140_apps00033.o}
procedure ___report_gsfailure;external;
{$L x86/libvcruntime140_apps00032.o}
procedure ___processing_throw;external;
{$L x86/libvcruntime140_apps00031.o}
procedure ___intrinsic_setjmp;external;
{$L x86/libvcruntime140_apps00030.o}
procedure ___current_exception_context;external;
{$L x86/libvcruntime140_apps00029.o}
procedure ___current_exception;external;
{$L x86/libvcruntime140_apps00028.o}
procedure ___TypeMatch;external;
{$L x86/libvcruntime140_apps00027.o}
procedure ___RTtypeid;external;
{$L x86/libvcruntime140_apps00026.o}
procedure ___RTDynamicCast;external;
{$L x86/libvcruntime140_apps00025.o}
procedure ___RTCastToVoid;external;
{$L x86/libvcruntime140_apps00024.o}
procedure ___GetPlatformExceptionInfo;external;
{$L x86/libvcruntime140_apps00023.o}
procedure ___FrameUnwindFilter;external;
{$L x86/libvcruntime140_apps00022.o}
procedure ___DestructExceptionObject;external;
{$L x86/libvcruntime140_apps00021.o}
procedure ___CxxUnregisterExceptionObject;external;
{$L x86/libvcruntime140_apps00020.o}
procedure ___CxxRegisterExceptionObject;external;
{$L x86/libvcruntime140_apps00019.o}
procedure ___CxxQueryExceptionSize;external;
{$L x86/libvcruntime140_apps00018.o}
procedure ___CxxFrameHandler3;external;
{$L x86/libvcruntime140_apps00016.o}
procedure ___CxxFrameHandler2;external;
{$L x86/libvcruntime140_apps00015.o}
procedure ___CxxFrameHandler;external;
{$L x86/libvcruntime140_apps00014.o}
procedure ___CxxExceptionFilter;external;
{$L x86/libvcruntime140_apps00013.o}
procedure ___CxxDetectRethrow;external;
{$L x86/libvcruntime140_apps00012.o}
procedure ___BuildCatchObjectHelper;external;
{$L x86/libvcruntime140_apps00011.o}
procedure ___BuildCatchObject;external;
{$L x86/libvcruntime140_apps00010.o}
procedure ___AdjustPointer;external;
{$L x86/libvcruntime140_apps00009.o}
procedure __SetWinRTOutOfMemoryExceptionCallback;external;
{$L x86/libvcruntime140_apps00008.o}
procedure __NLG_Return2;external;
{$L x86/libvcruntime140_apps00007.o}
procedure __NLG_Return;external;
{$L x86/libvcruntime140_apps00006.o}
procedure __NLG_Dispatch2;external;
{$L x86/libvcruntime140_apps00005.o}
procedure __IsExceptionObjectToBeDestroyed;external;
{$L x86/libvcruntime140_apps00004.o}
procedure __FindAndUnlinkFrame;external;
{$L x86/libvcruntime140_apps00003.o}
procedure __EH_prolog;external;
{$L x86/libvcruntime140_apps00002.o}
procedure __CreateFrameInfo;external;
{$L x86/libvcruntime140_apps00000.o}
var
  __lib32_libvcruntime140_app_a_iname : UInt64;
  __head_lib32_libvcruntime140_app_a : UInt64;
  __imp__wcsstr : UInt64;
  __imp__wcsrchr : UInt64;
  __imp__wcschr : UInt64;
  __imp__unexpected : UInt64;
  __imp__strstr : UInt64;
  __imp__strrchr : UInt64;
  __imp__strchr : UInt64;
  __imp__set_unexpected : UInt64;
  __imp__memset : UInt64;
  __imp__memmove : UInt64;
  __imp__memcpy : UInt64;
  __imp__memcmp : UInt64;
  __imp__memchr : UInt64;
  __imp__longjmp : UInt64;
  __imp___setjmp3 : UInt64;
  __imp___set_se_translator : UInt64;
  __imp___set_purecall_handler : UInt64;
  __imp___purecall : UInt64;
  __imp___longjmpex : UInt64;
  __imp___local_unwind4 : UInt64;
  __imp___local_unwind2 : UInt64;
  __imp___is_exception_typeof : UInt64;
  __imp___global_unwind2 : UInt64;
  __imp___get_unexpected : UInt64;
  __imp___get_purecall_handler : UInt64;
  __imp___except_handler4_common : UInt64;
  __imp___except_handler3 : UInt64;
  __imp___except_handler2 : UInt64;
  __imp___chkesp : UInt64;
  __imp____vcrt_LoadLibraryExW : UInt64;
  __imp____vcrt_InitializeCriticalSectionEx : UInt64;
  __imp____vcrt_GetModuleHandleW : UInt64;
  __imp____vcrt_GetModuleFileNameW : UInt64;
  __imp____uncaught_exceptions : UInt64;
  __imp____uncaught_exception : UInt64;
  __imp____unDNameEx : UInt64;
  __imp____unDName : UInt64;
  __imp____telemetry_main_return_trigger : UInt64;
  __imp____telemetry_main_invoke_trigger : UInt64;
  __imp____std_type_info_name : UInt64;
  __imp____std_type_info_hash : UInt64;
  __imp____std_type_info_destroy_list : UInt64;
  __imp____std_type_info_compare : UInt64;
  __imp____std_terminate : UInt64;
  __imp____std_exception_destroy : UInt64;
  __imp____std_exception_copy : UInt64;
  __imp____report_gsfailure : UInt64;
  __imp____processing_throw : UInt64;
  __imp____intrinsic_setjmp : UInt64;
  __imp____current_exception_context : UInt64;
  __imp____current_exception : UInt64;
  __imp____TypeMatch : UInt64;
  __imp____RTtypeid : UInt64;
  __imp____RTDynamicCast : UInt64;
  __imp____RTCastToVoid : UInt64;
  __imp____GetPlatformExceptionInfo : UInt64;
  __imp____FrameUnwindFilter : UInt64;
  __imp____DestructExceptionObject : UInt64;
  __imp____CxxUnregisterExceptionObject : UInt64;
  __imp____CxxRegisterExceptionObject : UInt64;
  __imp____CxxQueryExceptionSize : UInt64;
  __imp____CxxFrameHandler3 : UInt64;
  __imp____CxxFrameHandler2 : UInt64;
  __imp____CxxFrameHandler : UInt64;
  __imp____CxxExceptionFilter : UInt64;
  __imp____CxxDetectRethrow : UInt64;
  __imp____BuildCatchObjectHelper : UInt64;
  __imp____BuildCatchObject : UInt64;
  __imp____AdjustPointer : UInt64;
  __imp___SetWinRTOutOfMemoryExceptionCallback : UInt64;
  __imp___NLG_Return2 : UInt64;
  __imp___NLG_Return : UInt64;
  __imp___NLG_Dispatch2 : UInt64;
  __imp___IsExceptionObjectToBeDestroyed : UInt64;
  __imp___FindAndUnlinkFrame : UInt64;
  __imp___EH_prolog : UInt64;
  __imp___CreateFrameInfo : UInt64;
{$ENDIF}
implementation
end.
