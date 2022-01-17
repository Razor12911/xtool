(* ******************************************************************************
  * Copyright (c) 2008-2009 The Khronos Group Inc.
  *
  * Permission is hereby granted, free of charge, to any person obtaining a
  * copy of this software and/or associated documentation files (the
  * "Materials"), to deal in the Materials without restriction, including
  * without limitation the rights to use, copy, modify, merge, publish,
  * distribute, sublicense, and/or sell copies of the Materials, and to
  * permit persons to whom the Materials are furnished to do so, subject to
  * the following conditions:
  *
  * The above copyright notice and this permission notice shall be included
  * in all copies or substantial portions of the Materials.
  *
  * THE MATERIALS ARE PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
  * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
  * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
  * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
  * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
  * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
  * MATERIALS OR THE USE OR OTHER DEALINGS IN THE MATERIALS.
  ***************************************************************************** *)

// ported to FPC by Dmitry 'skalogryz' Boyarintsev: 28th apr 2009
// due to name conflict with type names, some constants have been renamed

// Original C name           Ported_name
// CL_DEVICE_TYPE            CL_DEVICE_TYPE_INFO
// CL_DEVICE_LOCAL_MEM_TYPE  CL_DEVICE_LOCAL_MEM_TYPE_INFO
// CL_CONTEXT_PROPERTIES     CL_CONTEXT_PROPERTIES_INFO
// CL_CONTEXT_PLATFORM       CL_CONTEXT_PLATFORM_INFO
// CL_FLOAT                  CL_FLOAT_TYPE
// CL_MEM_FLAGS              CL_MEM_FLAGS_INFO
// CL_IMAGE_FORMAT           CL_IMAGE_FORMAT_INFO

unit OpenCL;

interface

uses
  Windows;

const
  OpenCLlib = 'OpenCL.dll';

  { cl_platform.h }

const
  CL_PLATFORM_NVIDIA = $3001; // NVidia specific platform value

  { * scalar types  * }

type
  cl_char = int8;
  cl_uchar = uint8;
  cl_short = int16;
  cl_ushort = uint16;
  cl_int = int32;
  cl_uint = uint32;
  cl_long = int64;
  cl_ulong = uint64;

  cl_half = uint16;
  cl_float = single;
  cl_double = double;

  Pcl_char = ^cl_char;
  Pcl_uchar = ^cl_uchar;
  Pcl_short = ^cl_short;
  Pcl_ushort = ^cl_ushort;
  Pcl_int = ^cl_int;
  Pcl_uint = ^cl_uint;
  Pcl_long = ^cl_long;
  Pcl_ulong = ^cl_ulong;

  Pcl_half = ^cl_half;
  Pcl_float = ^cl_float;
  Pcl_double = ^cl_double;

const
  CL_CHAR_BIT = 8;
  CL_SCHAR_MAX = 127;
  CL_SCHAR_MIN = (-127 - 1);
  CL_CHAR_MAX = CL_SCHAR_MAX;
  CL_CHAR_MIN = CL_SCHAR_MIN;
  CL_UCHAR_MAX = 255;
  CL_SHRT_MAX = 32767;
  CL_SHRT_MIN = (-32767 - 1);
  CL_USHRT_MAX = 65535;
  CL_INT_MAX = 2147483647;
  CL_INT_MIN = (-2147483647 - 1);
  CL_UINT_MAX = $FFFFFFFF;
  CL_LONG_MAX = $7FFFFFFFFFFFFFFF;
  CL_LONG_MIN = -$7FFFFFFFFFFFFFFF - 1;
  CL_ULONG_MAX = $FFFFFFFFFFFFFFFF;

  CL_FLT_DIG = 6;
  CL_FLT_MANT_DIG = 24;
  CL_FLT_MAX_10_EXP = +38;
  CL_FLT_MAX_EXP = +128;
  CL_FLT_MIN_10_EXP = -37;
  CL_FLT_MIN_EXP = -125;
  CL_FLT_RADIX = 2;
  // CL_FLT_MAX          = 0x1.fffffep127f;
  // CL_FLT_MIN          = 0x1.0p-126f;
  // CL_FLT_EPSILON      = 0x1.0p-23f;

  CL_DBL_DIG = 15;
  CL_DBL_MANT_DIG = 53;
  CL_DBL_MAX_10_EXP = +308;
  CL_DBL_MAX_EXP = +1024;
  CL_DBL_MIN_10_EXP = -307;
  CL_DBL_MIN_EXP = -1021;
  CL_DBL_RADIX = 2;
  // CL_DBL_MAX          0x1.fffffffffffffp1023
  // CL_DBL_MIN          0x1.0p-1022
  // CL_DBL_EPSILON      0x1.0p-52

  { *
    * Vector types
    *
    *  Note:   OpenCL requires that all types be naturally aligned.
    *          This means that vector types must be naturally aligned.
    *          For example, a vector of four floats must be aligned to
    *          a 16 byte boundary (calculated as 4 * the natural 4-byte
    *          alignment of the float).  The alignment qualifiers here
    *          will only function properly if your compiler supports them
    *          and if you don't actively work to defeat them.  For example,
    *          in order for a cl_float4 to be 16 byte aligned in a struct,
    *          the start of the struct must itself be 16-byte aligned.
    *
    *          Maintaining proper alignment is the user's responsibility.
    * }
type
  cl_char2 = array [0 .. 1] of int8;
  cl_char4 = array [0 .. 3] of int8;
  cl_char8 = array [0 .. 7] of int8;
  cl_char16 = array [0 .. 15] of int8;

  cl_uchar2 = array [0 .. 1] of uint8;
  cl_uchar4 = array [0 .. 3] of uint8;
  cl_uchar8 = array [0 .. 7] of uint8;
  cl_uchar16 = array [0 .. 15] of uint8;

  cl_short2 = array [0 .. 1] of int16;
  cl_short4 = array [0 .. 3] of int16;
  cl_short8 = array [0 .. 7] of int16;
  cl_short16 = array [0 .. 15] of int16;

  cl_ushort2 = array [0 .. 1] of uint16;
  cl_ushort4 = array [0 .. 3] of uint16;
  cl_ushort8 = array [0 .. 7] of uint16;
  cl_ushort16 = array [0 .. 15] of uint16;

  cl_int2 = array [0 .. 1] of int32;
  cl_int4 = array [0 .. 3] of int32;
  cl_int8 = array [0 .. 7] of int32;
  cl_int16 = array [0 .. 15] of int32;

  cl_uint2 = array [0 .. 1] of uint32;
  cl_uint4 = array [0 .. 3] of uint32;
  cl_uint8 = array [0 .. 7] of uint32;
  cl_uint16 = array [0 .. 15] of uint32;

  cl_long2 = array [0 .. 1] of int64;
  cl_long4 = array [0 .. 3] of int64;
  cl_long8 = array [0 .. 7] of int64;
  cl_long16 = array [0 .. 15] of int64;

  cl_ulong2 = array [0 .. 1] of uint64;
  cl_ulong4 = array [0 .. 3] of uint64;
  cl_ulong8 = array [0 .. 7] of uint64;
  cl_ulong16 = array [0 .. 15] of uint64;

  cl_float2 = array [0 .. 1] of single;
  cl_float4 = array [0 .. 3] of single;
  cl_float8 = array [0 .. 7] of single;
  cl_float16 = array [0 .. 15] of single;

  cl_double2 = array [0 .. 1] of double;
  cl_double4 = array [0 .. 3] of double;
  cl_double8 = array [0 .. 7] of double;
  cl_double16 = array [0 .. 15] of double;

  { * There are no vector types for half * }

  // ****************************************************************************

  { cl.h }

type
  _cl_platform_id = record
  end;

  _cl_device_id = record
  end;

  _cl_context = record
  end;

  _cl_command_queue = record
  end;

  _cl_mem = record
  end;

  _cl_program = record
  end;

  _cl_kernel = record
  end;

  _cl_event = record
  end;

  _cl_sampler = record
  end;

  cl_platform_id = ^_cl_platform_id;
  cl_device_id = ^_cl_device_id;
  cl_context = ^_cl_context;
  cl_command_queue = ^_cl_command_queue;
  cl_mem = ^_cl_mem;
  cl_program = ^_cl_program;
  cl_kernel = ^_cl_kernel;
  cl_event = ^_cl_event;
  cl_sampler = ^_cl_sampler;

  Pcl_platform_id = cl_platform_id;
  Pcl_device_id = cl_device_id;
  Pcl_context = cl_context;
  Pcl_command_queue = cl_command_queue;
  Pcl_mem = cl_mem;
  Pcl_program = cl_program;
  Pcl_kernel = cl_kernel;
  Pcl_event = cl_event;
  Pcl_sampler = cl_sampler;

  cl_bool = cl_uint;
  // WARNING!  Unlike cl_ types in cl_platform.h, cl_bool is not guaranteed to be the same size as the bool in kernels.
  cl_bitfield = cl_ulong;
  cl_device_type = cl_bitfield;
  cl_platform_info = cl_uint;
  cl_device_info = cl_uint;
  cl_device_address_info = cl_bitfield;
  cl_device_fp_config = cl_bitfield;
  cl_device_mem_cache_type = cl_uint;
  cl_device_local_mem_type = cl_uint;
  cl_device_exec_capabilities = cl_bitfield;
  cl_command_queue_properties = cl_bitfield;

  cl_context_properties = intptr;
  cl_context_info = cl_uint;
  cl_command_queue_info = cl_uint;
  cl_channel_order = cl_uint;
  cl_channel_type = cl_uint;
  cl_mem_flags = cl_bitfield;
  cl_mem_object_type = cl_uint;
  cl_mem_info = cl_uint;
  cl_image_info = cl_uint;
  cl_addressing_mode = cl_uint;
  cl_filter_mode = cl_uint;
  cl_sampler_info = cl_uint;
  cl_map_flags = cl_bitfield;
  cl_program_info = cl_uint;
  cl_program_build_info = cl_uint;
  cl_build_status = cl_int;
  cl_kernel_info = cl_uint;
  cl_kernel_work_group_info = cl_uint;
  cl_event_info = cl_uint;
  cl_command_type = cl_uint;
  cl_profiling_info = cl_uint;

  _cl_image_format = packed record
    image_channel_order: cl_channel_order;
    image_channel_data_type: cl_channel_type;
  end;

  cl_image_format = _cl_image_format;

  Pcl_context_properties = ^cl_context_properties;
  Pcl_image_format = ^cl_image_format;

const
  // Error Codes
  CL_SUCCESS = 0;
  CL_DEVICE_NOT_FOUND = -1;
  CL_DEVICE_NOT_AVAILABLE = -2;
  CL_DEVICE_COMPILER_NOT_AVAILABLE = -3;
  CL_MEM_OBJECT_ALLOCATION_FAILURE = -4;
  CL_OUT_OF_RESOURCES = -5;
  CL_OUT_OF_HOST_MEMORY = -6;
  CL_PROFILING_INFO_NOT_AVAILABLE = -7;
  CL_MEM_COPY_OVERLAP = -8;
  CL_IMAGE_FORMAT_MISMATCH = -9;
  CL_IMAGE_FORMAT_NOT_SUPPORTED = -10;
  CL_BUILD_PROGRAM_FAILURE = -11;
  CL_MAP_FAILURE = -12;

  CL_INVALID_VALUE = -30;
  CL_INVALID_DEVICE_TYPE = -31;
  CL_INVALID_PLATFORM = -32;
  CL_INVALID_DEVICE = -33;
  CL_INVALID_CONTEXT = -34;
  CL_INVALID_QUEUE_PROPERTIES = -35;
  CL_INVALID_COMMAND_QUEUE = -36;
  CL_INVALID_HOST_PTR = -37;
  CL_INVALID_MEM_OBJECT = -38;
  CL_INVALID_IMAGE_FORMAT_DESCRIPTOR = -39;
  CL_INVALID_IMAGE_SIZE = -40;
  CL_INVALID_SAMPLER = -41;
  CL_INVALID_BINARY = -42;
  CL_INVALID_BUILD_OPTIONS = -43;
  CL_INVALID_PROGRAM = -44;
  CL_INVALID_PROGRAM_EXECUTABLE = -45;
  CL_INVALID_KERNEL_NAME = -46;
  CL_INVALID_KERNEL_DEFINITION = -47;
  CL_INVALID_KERNEL = -48;
  CL_INVALID_ARG_INDEX = -49;
  CL_INVALID_ARG_VALUE = -50;
  CL_INVALID_ARG_SIZE = -51;
  CL_INVALID_KERNEL_ARGS = -52;
  CL_INVALID_WORK_DIMENSION = -53;
  CL_INVALID_WORK_GROUP_SIZE = -54;
  CL_INVALID_WORK_ITEM_SIZE = -55;
  CL_INVALID_GLOBAL_OFFSET = -56;
  CL_INVALID_EVENT_WAIT_LIST = -57;
  CL_INVALID_EVENT = -58;
  CL_INVALID_OPERATION = -59;
  CL_INVALID_GL_OBJECT = -60;
  CL_INVALID_BUFFER_SIZE = -61;
  CL_INVALID_MIP_LEVEL = -62;

  // OpenCL Version
  CL_VERSION_1_0 = 1;

  // cl_bool
  CL_FALSE = 0;
  CL_TRUE = 1;

  // cl_platform_info
  CL_PLATFORM_PROFILE = $0900;
  CL_PLATFORM_VERSION = $0901;
  CL_PLATFORM_NAME = $0902;
  CL_PLATFORM_VENDOR = $0903;
  CL_PLATFORM_EXTENSIONS = $0904;

  // cl_device_type - bitfield
  CL_DEVICE_TYPE_DEFAULT = (1 shl 0);
  CL_DEVICE_TYPE_CPU = (1 shl 1);
  CL_DEVICE_TYPE_GPU = (1 shl 2);
  CL_DEVICE_TYPE_ACCELERATOR = (1 shl 3);
  CL_DEVICE_TYPE_ALL = $FFFFFFFF;

  // cl_device_info
  CL_DEVICE_TYPE_INFO = $1000; // CL_DEVICE_TYPE
  CL_DEVICE_VENDOR_ID = $1001;
  CL_DEVICE_MAX_COMPUTE_UNITS = $1002;
  CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS = $1003;
  CL_DEVICE_MAX_WORK_GROUP_SIZE = $1004;
  CL_DEVICE_MAX_WORK_ITEM_SIZES = $1005;
  CL_DEVICE_PREFERRED_VECTOR_WIDTH_CHAR = $1006;
  CL_DEVICE_PREFERRED_VECTOR_WIDTH_SHORT = $1007;
  CL_DEVICE_PREFERRED_VECTOR_WIDTH_INT = $1008;
  CL_DEVICE_PREFERRED_VECTOR_WIDTH_LONG = $1009;
  CL_DEVICE_PREFERRED_VECTOR_WIDTH_FLOAT = $100A;
  CL_DEVICE_PREFERRED_VECTOR_WIDTH_DOUBLE = $100B;
  CL_DEVICE_MAX_CLOCK_FREQUENCY = $100C;
  CL_DEVICE_ADDRESS_BITS = $100D;
  CL_DEVICE_MAX_READ_IMAGE_ARGS = $100E;
  CL_DEVICE_MAX_WRITE_IMAGE_ARGS = $100F;
  CL_DEVICE_MAX_MEM_ALLOC_SIZE = $1010;
  CL_DEVICE_IMAGE2D_MAX_WIDTH = $1011;
  CL_DEVICE_IMAGE2D_MAX_HEIGHT = $1012;
  CL_DEVICE_IMAGE3D_MAX_WIDTH = $1013;
  CL_DEVICE_IMAGE3D_MAX_HEIGHT = $1014;
  CL_DEVICE_IMAGE3D_MAX_DEPTH = $1015;
  CL_DEVICE_IMAGE_SUPPORT = $1016;
  CL_DEVICE_MAX_PARAMETER_SIZE = $1017;
  CL_DEVICE_MAX_SAMPLERS = $1018;
  CL_DEVICE_MEM_BASE_ADDR_ALIGN = $1019;
  CL_DEVICE_MIN_DATA_TYPE_ALIGN_SIZE = $101A;
  CL_DEVICE_SINGLE_FP_CONFIG = $101B;
  CL_DEVICE_DOUBLE_FP_CONFIG = $1032;
  CL_DEVICE_PREFERRED_VECTOR_WIDTH_HALF = $1034;
  CL_DEVICE_HOST_UNIFIED_MEMORY = $1035;
  CL_DEVICE_NATIVE_VECTOR_WIDTH_CHAR = $1036;
  CL_DEVICE_NATIVE_VECTOR_WIDTH_SHORT = $1037;
  CL_DEVICE_NATIVE_VECTOR_WIDTH_INT = $1038;
  CL_DEVICE_NATIVE_VECTOR_WIDTH_LONG = $1039;
  CL_DEVICE_NATIVE_VECTOR_WIDTH_FLOAT = $103A;
  CL_DEVICE_NATIVE_VECTOR_WIDTH_DOUBLE = $103B;
  CL_DEVICE_NATIVE_VECTOR_WIDTH_HALF = $103C;
  CL_DEVICE_OPENCL_C_VERSION = $103D;
  CL_DEVICE_LINKER_AVAILABLE = $103E;
  CL_DEVICE_BUILT_IN_KERNELS = $103F;
  CL_DEVICE_IMAGE_MAX_BUFFER_SIZE = $1040;
  CL_DEVICE_IMAGE_MAX_ARRAY_SIZE = $1041;
  CL_DEVICE_PARENT_DEVICE = $1042;
  CL_DEVICE_PARTITION_MAX_SUB_DEVICES = $1043;
  CL_DEVICE_PARTITION_PROPERTIES = $1044;
  CL_DEVICE_PARTITION_AFFINITY_DOMAIN = $1045;
  CL_DEVICE_PARTITION_TYPE = $1046;
  CL_DEVICE_REFERENCE_COUNT = $1047;
  CL_DEVICE_PREFERRED_INTEROP_USER_SYNC = $1048;
  CL_DEVICE_PRINTF_BUFFER_SIZE = $1049;
  CL_DEVICE_GLOBAL_MEM_CACHE_TYPE = $101C;
  CL_DEVICE_GLOBAL_MEM_CACHELINE_SIZE = $101D;
  CL_DEVICE_GLOBAL_MEM_CACHE_SIZE = $101E;
  CL_DEVICE_GLOBAL_MEM_SIZE = $101F;
  CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE = $1020;
  CL_DEVICE_MAX_CONSTANT_ARGS = $1021;
  CL_DEVICE_LOCAL_MEM_TYPE_INFO = $1022; // CL_DEVICE_LOCAL_MEM_TYPE
  CL_DEVICE_LOCAL_MEM_SIZE = $1023;
  CL_DEVICE_ERROR_CORRECTION_SUPPORT = $1024;
  CL_DEVICE_PROFILING_TIMER_RESOLUTION = $1025;
  CL_DEVICE_ENDIAN_LITTLE = $1026;
  CL_DEVICE_AVAILABLE = $1027;
  CL_DEVICE_COMPILER_AVAILABLE = $1028;
  CL_DEVICE_EXECUTION_CAPABILITIES = $1029;
  CL_DEVICE_QUEUE_PROPERTIES = $102A;
  CL_DEVICE_NAME = $102B;
  CL_DEVICE_VENDOR = $102C;
  CL_DRIVER_VERSION = $102D;
  CL_DEVICE_PROFILE = $102E;
  CL_DEVICE_VERSION = $102F;
  CL_DEVICE_EXTENSIONS = $1030;
  CL_DEVICE_PLATFORM = $1031;

  // cl_device_address_info - bitfield
  CL_DEVICE_ADDRESS_32_BITS = (1 shl 0);
  CL_DEVICE_ADDRESS_64_BITS = (1 shl 1);

  // cl_device_fp_config - bitfield
  CL_FP_DENORM = (1 shl 0);
  CL_FP_INF_NAN = (1 shl 1);
  CL_FP_ROUND_TO_NEAREST = (1 shl 2);
  CL_FP_ROUND_TO_ZERO = (1 shl 3);
  CL_FP_ROUND_TO_INF = (1 shl 4);
  CL_FP_FMA = (1 shl 5);

  // cl_device_mem_cache_type
  CL_NONE = $0;
  CL_READ_ONLY_CACHE = $1;
  CL_READ_WRITE_CACHE = $2;

  // cl_device_local_mem_type
  CL_LOCAL = $1;
  CL_GLOBAL = $2;

  // cl_device_exec_capabilities - bitfield
  CL_EXEC_KERNEL = (1 shl 0);
  CL_EXEC_NATIVE_KERNEL = (1 shl 1);

  // cl_command_queue_properties - bitfield
  CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE = (1 shl 0);
  CL_QUEUE_PROFILING_ENABLE = (1 shl 1);

  // cl_context_info
  CL_CONTEXT_REFERENCE_COUNT = $1080;
  CL_CONTEXT_DEVICES = $1081;
  CL_CONTEXT_PROPERTIES_INFO = $1082; // CL_CONTEXT_PROPERTIES
  CL_CONTEXT_NUM_DEVICES = $1083;
  CL_CONTEXT_PLATFORM_INFO = $1084; // CL_CONTEXT_PLATFORM

  // cl_command_queue_info
  CL_QUEUE_CONTEXT = $1090;
  CL_QUEUE_DEVICE = $1091;
  CL_QUEUE_REFERENCE_COUNT = $1092;
  CL_QUEUE_PROPERTIES = $1093;

  // cl_mem_flags - bitfield
  CL_MEM_READ_WRITE = (1 shl 0);
  CL_MEM_WRITE_ONLY = (1 shl 1);
  CL_MEM_READ_ONLY = (1 shl 2);
  CL_MEM_USE_HOST_PTR = (1 shl 3);
  CL_MEM_ALLOC_HOST_PTR = (1 shl 4);
  CL_MEM_COPY_HOST_PTR = (1 shl 5);

  // cl_channel_order
  CL_R = $10B0;
  CL_A = $10B1;
  CL_RG = $10B2;
  CL_RA = $10B3;
  CL_RGB = $10B4;
  CL_RGBA = $10B5;
  CL_BGRA = $10B6;
  CL_ARGB = $10B7;
  CL_INTENSITY = $10B8;
  CL_LUMINANCE = $10B9;

  // cl_channel_type
  CL_SNORM_INT8 = $10D0;
  CL_SNORM_INT16 = $10D1;
  CL_UNORM_INT8 = $10D2;
  CL_UNORM_INT16 = $10D3;
  CL_UNORM_SHORT_565 = $10D4;
  CL_UNORM_SHORT_555 = $10D5;
  CL_UNORM_INT_101010 = $10D6;
  CL_SIGNED_INT8 = $10D7;
  CL_SIGNED_INT16 = $10D8;
  CL_SIGNED_INT32 = $10D9;
  CL_UNSIGNED_INT8 = $10DA;
  CL_UNSIGNED_INT16 = $10DB;
  CL_UNSIGNED_INT32 = $10DC;
  CL_HALF_FLOAT = $10DD;
  CL_FLOAT_TYPE = $10DE; // CL_FLOAT

  // cl_mem_object_type
  CL_MEM_OBJECT_BUFFER = $10F0;
  CL_MEM_OBJECT_IMAGE2D = $10F1;
  CL_MEM_OBJECT_IMAGE3D = $10F2;

  // cl_mem_info
  CL_MEM_TYPE = $1100;
  CL_MEM_FLAGS_INFO = $1101; // CL_MEM_FLAGS
  CL_MEM_SIZE = $1102;
  CL_MEM_HOST_PTR = $1103;
  CL_MEM_MAP_COUNT = $1104;
  CL_MEM_REFERENCE_COUNT = $1105;
  CL_MEM_CONTEXT = $1106;

  // cl_image_info
  CL_IMAGE_FORMAT_INFO = $1110; // CL_IMAGE_FORMAT
  CL_IMAGE_ELEMENT_SIZE = $1111;
  CL_IMAGE_ROW_PITCH = $1112;
  CL_IMAGE_SLICE_PITCH = $1113;
  CL_IMAGE_WIDTH = $1114;
  CL_IMAGE_HEIGHT = $1115;
  CL_IMAGE_DEPTH = $1116;

  // cl_addressing_mode
  CL_ADDRESS_NONE = $1130;
  CL_ADDRESS_CLAMP_TO_EDGE = $1131;
  CL_ADDRESS_CLAMP = $1132;
  CL_ADDRESS_REPEAT = $1133;

  // cl_filter_mode
  CL_FILTER_NEAREST = $1140;
  CL_FILTER_LINEAR = $1141;

  // cl_sampler_info
  CL_SAMPLER_REFERENCE_COUNT = $1150;
  CL_SAMPLER_CONTEXT = $1151;
  CL_SAMPLER_NORMALIZED_COORDS = $1152;
  CL_SAMPLER_ADDRESSING_MODE = $1153;
  CL_SAMPLER_FILTER_MODE = $1154;

  // cl_map_flags - bitfield
  CL_MAP_READ = (1 shl 0);
  CL_MAP_WRITE = (1 shl 1);

  // cl_program_info
  CL_PROGRAM_REFERENCE_COUNT = $1160;
  CL_PROGRAM_CONTEXT = $1161;
  CL_PROGRAM_NUM_DEVICES = $1162;
  CL_PROGRAM_DEVICES = $1163;
  CL_PROGRAM_SOURCE = $1164;
  CL_PROGRAM_BINARY_SIZES = $1165;
  CL_PROGRAM_BINARIES = $1166;

  // cl_program_build_info
  CL_PROGRAM_BUILD_STATUS = $1181;
  CL_PROGRAM_BUILD_OPTIONS = $1182;
  CL_PROGRAM_BUILD_LOG = $1183;

  // cl_build_status
  CL_BUILD_SUCCESS = 0;
  CL_BUILD_NONE = -1;
  CL_BUILD_ERROR = -2;
  CL_BUILD_IN_PROGRESS = -3;

  // cl_kernel_info
  CL_KERNEL_FUNCTION_NAME = $1190;
  CL_KERNEL_NUM_ARGS = $1191;
  CL_KERNEL_REFERENCE_COUNT = $1192;
  CL_KERNEL_CONTEXT = $1193;
  CL_KERNEL_PROGRAM = $1194;

  // cl_kernel_work_group_info
  CL_KERNEL_WORK_GROUP_SIZE = $11B0;
  CL_KERNEL_COMPILE_WORK_GROUP_SIZE = $11B1;
  CL_KERNEL_LOCAL_MEM_SIZE = $11B2;

  // cl_event_info
  CL_EVENT_COMMAND_QUEUE = $11D0;
  CL_EVENT_COMMAND_TYPE = $11D1;
  CL_EVENT_REFERENCE_COUNT = $11D2;
  CL_EVENT_COMMAND_EXECUTION_STATUS = $11D3;

  // cl_command_type
  CL_COMMAND_NDRANGE_KERNEL = $11F0;
  CL_COMMAND_TASK = $11F1;
  CL_COMMAND_NATIVE_KERNEL = $11F2;
  CL_COMMAND_READ_BUFFER = $11F3;
  CL_COMMAND_WRITE_BUFFER = $11F4;
  CL_COMMAND_COPY_BUFFER = $11F5;
  CL_COMMAND_READ_IMAGE = $11F6;
  CL_COMMAND_WRITE_IMAGE = $11F7;
  CL_COMMAND_COPY_IMAGE = $11F8;
  CL_COMMAND_COPY_IMAGE_TO_BUFFER = $11F9;
  CL_COMMAND_COPY_BUFFER_TO_IMAGE = $11FA;
  CL_COMMAND_MAP_BUFFER = $11FB;
  CL_COMMAND_MAP_IMAGE = $11FC;
  CL_COMMAND_UNMAP_MEM_OBJECT = $11FD;
  CL_COMMAND_MARKER = $11FE;
  CL_COMMAND_WAIT_FOR_EVENTS = $11FF;
  CL_COMMAND_BARRIER = $1200;
  CL_COMMAND_ACQUIRE_GL_OBJECTS = $1201;
  CL_COMMAND_RELEASE_GL_OBJECTS = $1202;

  // command execution status
  CL_COMPLETE = $0;
  CL_RUNNING = $1;
  CL_SUBMITTED = $2;
  CL_QUEUED = $3;

  // cl_profiling_info
  CL_PROFILING_COMMAND_QUEUED = $1280;
  CL_PROFILING_COMMAND_SUBMIT = $1281;
  CL_PROFILING_COMMAND_START = $1282;
  CL_PROFILING_COMMAND_END = $1283;

  // ****************************************************************************

  // Platform APIs
function clGetPlatformIDs(num_entries: cl_uint; platforms: Pcl_platform_id;
  num_platforms: Pcl_uint): cl_int; stdcall;
  external OpenCLlib name 'clGetPlatformIDs';

function clGetPlatformInfo(_platform: cl_platform_id;
  param_name: cl_platform_info; value_size: size_t; value: Pointer;
  var size_ret: size_t): cl_int; stdcall;
  external OpenCLlib name 'clGetPlatformInfo';

// Device APIs
function clGetDeviceIDs(_platform: cl_platform_id; device_type: cl_device_type;
  num_entries: cl_uint; devices: Pcl_device_id; num_devices: Pcl_uint): cl_int;
  stdcall; external OpenCLlib name 'clGetDeviceIDs';

function clGetDeviceInfo(device: cl_device_id; param_name: cl_device_info;
  value_size: size_t; value: Pointer; var size_ret: size_t): cl_int; stdcall;
  external OpenCLlib name 'clGetDeviceInfo';

// Context APIs
type
  TContextNotify = procedure(name: Pchar; data: Pointer; size: size_t;
    data2: Pointer); stdcall;

function clCreateContext(properties: Pcl_context_properties;
  num_devices: cl_uint; devices: Pcl_device_id; notify: TContextNotify;
  user_data: Pointer; var errcode_ret: cl_int): cl_context; stdcall;
  external OpenCLlib name 'clCreateContext';

function clCreateContextFromType(properties: Pcl_context_properties;
  device_type: cl_device_type; notify: TContextNotify; user_data: Pointer;
  var errcode_ret: cl_int): cl_context; stdcall;
  external OpenCLlib name 'clCreateContextFromType';

function clRetainContext(context: cl_context): cl_int; stdcall;
  external OpenCLlib name 'clRetainContext';

function clReleaseContext(context: cl_context): cl_int; stdcall;
  external OpenCLlib name 'clReleaseContext';

function clGetContextInfo(context: cl_context; param_name: cl_context_info;
  value_size: size_t; value: Pointer; var size_ret: size_t): cl_int; stdcall;
  external OpenCLlib name 'clGetContextInfo';

// Command Queue APIs
function clCreateCommandQueue(context: cl_context; device: cl_device_id;
  properties: cl_command_queue_properties; errcode_ret: cl_int)
  : cl_command_queue; stdcall; external OpenCLlib name 'clCreateCommandQueue';

function clRetainCommandQueue(command_queue: cl_command_queue): cl_int; stdcall;
  external OpenCLlib name 'clRetainCommandQueue';

function clReleaseCommandQueue(command_queue: cl_command_queue): cl_int;
  stdcall; external OpenCLlib name 'clReleaseCommandQueue';

function clGetCommandQueueInfo(command_queue: cl_command_queue;
  param_name: cl_command_queue_info; value_size: size_t; value: Pointer;
  var size_ret: size_t): cl_int; stdcall;
  external OpenCLlib name 'clGetCommandQueueInfo';

function clSetCommandQueueProperty(command_queue: cl_command_queue;
  properties: cl_command_queue_properties; enable: cl_bool;
  var old_properties: cl_command_queue_properties): cl_int; stdcall;
  external OpenCLlib name 'clSetCommandQueueProperty';

// Memory Object APIs
function clCreateBuffer(context: cl_context; flags: cl_mem_flags; size: size_t;
  host_ptr: Pointer; var errcode_ret: cl_int): cl_mem; stdcall;
  external OpenCLlib name 'clCreateBuffer';

function clCreateImage2D(context: cl_context; flags: cl_mem_flags;
  image_format: Pcl_image_format; image_width: size_t; image_height: size_t;
  image_row_pitch: size_t; host_ptr: Pointer; var errcode_ret: cl_int): cl_mem;
  stdcall; external OpenCLlib name 'clCreateImage2D';

function clCreateImage3D(context: cl_context; flags: cl_mem_flags;
  image_format: Pcl_image_format; image_width: size_t; image_height: size_t;
  image_depth: size_t; image_row_pitch: size_t; image_slice_pitch: size_t;
  host_ptr: Pointer; var errcode_ret: cl_int): cl_mem; stdcall;
  external OpenCLlib name 'clCreateImage3D';

function clRetainMemObject(memobj: cl_mem): cl_int; stdcall;
  external OpenCLlib name 'clRetainMemObject';

function clReleaseMemObject(memobj: cl_mem): cl_int; stdcall;
  external OpenCLlib name 'clReleaseMemObject';

function clGetSupportedImageFormats(context: cl_context; flags: cl_mem_flags;
  image_type: cl_mem_object_type; num_entries: cl_uint;
  image_formats: Pcl_image_format; var num_formats: cl_uint): cl_int; stdcall;
  external OpenCLlib name 'clGetSupportedImageFormats';

function clGetMemObjectInfo(memobj: cl_mem; param_name: cl_mem_info;
  value_size: size_t; value: Pointer; var size_ret: size_t): cl_int; stdcall;
  external OpenCLlib name 'clGetMemObjectInfo';

function clGetImageInfo(image: cl_mem; param_name: cl_image_info;
  value_size: size_t; value: Pointer; var size_ret: size_t): cl_int; stdcall;
  external OpenCLlib name 'clGetImageInfo';

// Sampler APIs
function clCreateSampler(context: cl_context; is_norm_coords: cl_bool;
  addr_mode: cl_addressing_mode; filter_mode: cl_filter_mode;
  var errcode_ret: cl_int): cl_sampler; stdcall;
  external OpenCLlib name 'clCreateSampler';

function clRetainSampler(sampler: cl_sampler): cl_int; stdcall;
  external OpenCLlib name 'clRetainSampler';

function clReleaseSampler(sampler: cl_sampler): cl_int; stdcall;
  external OpenCLlib name 'clReleaseSampler';

function clGetSamplerInfo(sampler: cl_sampler; param_name: cl_sampler_info;
  value_size: size_t; value: Pointer; var size_ret: size_t): cl_int; stdcall;
  external OpenCLlib name 'clGetSamplerInfo';

// Program Object APIs
function clCreateProgramWithSource(context: cl_context; count: cl_uint;
  strings: PPChar; lengths: PSIZE_T; var errcode_ret: cl_int): cl_program;
  stdcall; external OpenCLlib name 'clCreateProgramWithSource';

type
  PPByte = ^PByte;

function clCreateProgramWithBinary(context: cl_context; num_devices: cl_uint;
  device_list: Pcl_device_id; lengths: PSIZE_T; binaries: PPByte;
  var binary_status: cl_int; var errcode_ret: cl_int): cl_program; stdcall;
  external OpenCLlib name 'clCreateProgramWithBinary';

function clRetainProgram(_program: cl_program): cl_int; stdcall;
  external OpenCLlib name 'clRetainProgram';

function clReleaseProgram(_program: cl_program): cl_int; stdcall;
  external OpenCLlib name 'clReleaseProgram';

type
  TProgramNotify = procedure(_program: cl_program; user_data: Pointer); stdcall;

  // extern   cl_int

function clBuildProgram(_program: cl_program; num_devices: cl_uint;
  device_list: Pcl_device_id; options: Pchar; notify: TProgramNotify;
  user_data: Pointer): cl_int; stdcall;
  external OpenCLlib name 'clBuildProgram';

function clUnloadCompiler: cl_int; stdcall;
  external OpenCLlib name 'clUnloadCompiler';

function clGetProgramInfo(_program: cl_program; param_name: cl_program_info;
  value_size: size_t; value: Pointer; var size_ret: size_t): cl_int; stdcall;
  external OpenCLlib name 'clGetProgramInfo';

function clGetProgramBuildInfo(_program: cl_program; device: cl_device_id;
  param_name: cl_program_build_info; value_size: size_t; value: Pointer;
  var size_ret: size_t): cl_int; stdcall;
  external OpenCLlib name 'clGetProgramBuildInfo';

// Kernel Object APIs
function clCreateKernel(_program: cl_program; kernel_name: Pchar;
  var errcode_ret: cl_int): cl_kernel; stdcall;
  external OpenCLlib name 'clCreateKernel';

function clCreateKernelsInProgram(_program: cl_program; num_kernels: cl_uint;
  kernels: Pcl_kernel; var num_ret: cl_uint): cl_int; stdcall;
  external OpenCLlib name 'clCreateKernelsInProgram';

function clRetainKernel(kernel: cl_kernel): cl_int; stdcall;
  external OpenCLlib name 'clRetainKernel';

function clReleaseKernel(kernel: cl_kernel): cl_int; stdcall;
  external OpenCLlib name 'clReleaseKernel';

function clSetKernelArg(kernel: cl_kernel; arg_index: cl_uint; arg_size: size_t;
  arg_value: Pointer): cl_int; stdcall;
  external OpenCLlib name 'clSetKernelArg';

function clGetKernelInfo(kernel: cl_kernel; param_name: cl_kernel_info;
  value_size: size_t; value: Pointer; var size_ret: size_t): cl_int; stdcall;
  external OpenCLlib name 'clGetKernelInfo';

function clGetKernelWorkGroupInfo(kernel: cl_kernel; device: cl_device_id;
  param_name: cl_kernel_work_group_info; value_size: size_t; value: Pointer;
  size_ret: PSIZE_T): cl_int; stdcall;
  external OpenCLlib name 'clGetKernelWorkGroupInfo';

// Event Object APIs
function clWaitForEvents(num_events: cl_uint; event_list: cl_event): cl_int;
  stdcall; external OpenCLlib name 'clWaitForEvents';

function clGetEventInfo(event: cl_event; param_name: cl_event_info;
  value_size: size_t; value: Pointer; var size_ret: size_t): cl_int; stdcall;
  external OpenCLlib name 'clGetEventInfo';

function clRetainEvent(event: cl_event): cl_int; stdcall;
  external OpenCLlib name 'clRetainEvent';

function clReleaseEvent(event: cl_event): cl_int; stdcall;
  external OpenCLlib name 'clReleaseEvent';

// Profiling APIs
function clGetEventProfilingInfo(event: cl_event; param_name: cl_profiling_info;
  value_size: size_t; value: Pointer; var size_ret: size_t): cl_int; stdcall;
  external OpenCLlib name 'clGetEventProfilingInfo';

// Flush and Finish APIs
function clFlush(command_queue: cl_command_queue): cl_int; stdcall;
  external OpenCLlib name 'clFlush';

function clFinish(command_queue: cl_command_queue): cl_int; stdcall;
  external OpenCLlib name 'clFinish';

// Enqueued Commands APIs
function clEnqueueReadBuffer(command_queue: cl_command_queue; buffer: cl_mem;
  blocking_read: cl_bool; offset: size_t; cb: size_t; ptr: Pointer;
  num_events: cl_uint; events_list: Pcl_event; event: Pcl_event): cl_int;
  stdcall; external OpenCLlib name 'clEnqueueReadBuffer';

function clEnqueueWriteBuffer(command_queue: cl_command_queue; buffer: cl_mem;
  blocking_write: cl_bool; offset: size_t; cb: size_t; ptr: Pointer;
  num_events: cl_uint; events_list: Pcl_event; event: Pcl_event): cl_int;
  stdcall; external OpenCLlib name 'clEnqueueWriteBuffer';

function clEnqueueCopyBuffer(command_queue: cl_command_queue;
  src_buffer: cl_mem; dst_buffer: cl_mem; src_offset: size_t;
  dst_offset: size_t; cb: size_t; num_events: cl_uint; events_list: Pcl_event;
  event: Pcl_event): cl_int; stdcall;
  external OpenCLlib name 'clEnqueueCopyBuffer';

function clEnqueueReadImage(command_queue: cl_command_queue; image: cl_mem;
  blocking_read: cl_bool; origin: PSIZE_T; region: PSIZE_T; row_pitch: size_t;
  slice_pitch: size_t; ptr: Pointer; num_events: cl_uint;
  events_list: Pcl_event; event: Pcl_event): cl_int; stdcall;
  external OpenCLlib name 'clEnqueueReadImage';

function clEnqueueWriteImage(command_queue: cl_command_queue; image: cl_mem;
  blocking_write: cl_bool; origin: PSIZE_T; region: PSIZE_T; row_pitch: size_t;
  slice_pitch: size_t; ptr: Pointer; num_events: cl_uint;
  events_list: Pcl_event; event: Pcl_event): cl_int; stdcall;
  external OpenCLlib name 'clEnqueueWriteImage';

function clEnqueueCopyImage(command_queue: cl_command_queue; src_image: cl_mem;
  dst_image: cl_mem; src_origin: PSIZE_T; dst_origin: PSIZE_T; region: PSIZE_T;
  num_events: cl_uint; events_list: Pcl_event; event: Pcl_event): cl_int;
  stdcall; external OpenCLlib name 'clEnqueueCopyImage';

function clEnqueueCopyImageToBuffer(command_queue: cl_command_queue;
  src_image: cl_mem; dst_buffre: cl_mem; src_origin: PSIZE_T; region: PSIZE_T;
  dst_offset: size_t; num_events: cl_uint; events_list: Pcl_event;
  event: Pcl_event): cl_int; stdcall;
  external OpenCLlib name 'clEnqueueCopyImageToBuffer';

function clEnqueueCopyBufferToImage(command_queue: cl_command_queue;
  src_buffer: cl_mem; dst_image: cl_mem; src_offset: size_t;
  dst_origin: PSIZE_T; region: PSIZE_T; num_events: cl_uint;
  events_list: Pcl_event; event: Pcl_event): cl_int; stdcall;
  external OpenCLlib name 'clEnqueueCopyBufferToImage';

function clEnqueueMapBuffer(command_queue: cl_command_queue; buffer: cl_mem;
  blocking_map: cl_bool; map_flags: cl_map_flags; offset: size_t; cb: size_t;
  num_events: cl_uint; events_list: Pcl_event; event: Pcl_event;
  var errcode_ret: cl_int): Pointer; stdcall;
  external OpenCLlib name 'clEnqueueMapBuffer';

function clEnqueueMapImage(command_queue: cl_command_queue; image: cl_mem;
  blocking_map: cl_bool; map_flags: cl_map_flags; origin: PSIZE_T;
  region: PSIZE_T; row_pitch: size_t; slice_pitch: size_t; num_events: cl_uint;
  events_list: Pcl_event; event: Pcl_event; var errcode_ret: cl_int): Pointer;
  stdcall; external OpenCLlib name 'clEnqueueMapImage';

function clEnqueueUnmapMemObject(command_queue: cl_command_queue;
  memobj: cl_mem; mapped_ptr: Pointer; num_events: cl_uint;
  events_list: Pcl_event; event: Pcl_event): cl_int; stdcall;
  external OpenCLlib name 'clEnqueueUnmapMemObject';

function clEnqueueNDRangeKernel(command_queue: cl_command_queue;
  kernel: cl_kernel; work_dim: cl_uint; global_offset, global_size,
  local_size: PSIZE_T; num_events: cl_uint; events_list: Pcl_event;
  event: Pcl_event): cl_int; stdcall;
  external OpenCLlib name 'clEnqueueNDRangeKernel';

function clEnqueueTask(command_queue: cl_command_queue; kernel: cl_kernel;
  num_events: cl_uint; events_list: Pcl_event; event: Pcl_event): cl_int;
  stdcall; external OpenCLlib name 'clEnqueueTask';

type
  TEnqueueUserProc = procedure(userdata: Pointer); stdcall;

function clEnqueueNativeKernel(command_queue: cl_command_queue;
  user_func: TEnqueueUserProc; args: Pointer; cb_args: size_t;
  num_mem_objects: cl_uint; mem_list: Pcl_mem; args_mem_loc: PPointer;
  num_events: cl_uint; event_wait_list: Pcl_event; event: Pcl_event): cl_int;
  stdcall; external OpenCLlib name 'clEnqueueNativeKernel';

function clEnqueueMarker(command_queue: cl_command_queue; event: Pcl_event)
  : cl_int; stdcall; external OpenCLlib name 'clEnqueueMarker';

function clEnqueueWaitForEvents(command_queue: cl_command_queue;
  num_events: cl_uint; event_list: Pcl_event): cl_int; stdcall;
  external OpenCLlib name 'clEnqueueWaitForEvents';

function clEnqueueBarrier(command_queue: cl_command_queue): cl_int; stdcall;
  external OpenCLlib name 'clEnqueueBarrier';

function clErrorText(err: cl_int): string;

implementation

function clErrorText(err: cl_int): string;
begin
  case err of
    CL_DEVICE_NOT_FOUND:
      clErrorText := 'CL_DEVICE_NOT_FOUND';
    CL_DEVICE_NOT_AVAILABLE:
      clErrorText := 'CL_DEVICE_NOT_AVAILABLE';
    CL_DEVICE_COMPILER_NOT_AVAILABLE:
      clErrorText := 'CL_DEVICE_COMPILER_NOT_AVAILABLE';
    CL_MEM_OBJECT_ALLOCATION_FAILURE:
      clErrorText := 'CL_MEM_OBJECT_ALLOCATION_FAILURE';
    CL_OUT_OF_RESOURCES:
      clErrorText := 'CL_OUT_OF_RESOURCES';
    CL_OUT_OF_HOST_MEMORY:
      clErrorText := 'CL_OUT_OF_HOST_MEMORY';
    CL_PROFILING_INFO_NOT_AVAILABLE:
      clErrorText := 'CL_PROFILING_INFO_NOT_AVAILABLE';
    CL_MEM_COPY_OVERLAP:
      clErrorText := 'CL_MEM_COPY_OVERLAP';
    CL_IMAGE_FORMAT_MISMATCH:
      clErrorText := 'CL_IMAGE_FORMAT_MISMATCH';
    CL_IMAGE_FORMAT_NOT_SUPPORTED:
      clErrorText := 'CL_IMAGE_FORMAT_NOT_SUPPORTED';
    CL_BUILD_PROGRAM_FAILURE:
      clErrorText := 'CL_BUILD_PROGRAM_FAILURE';
    CL_MAP_FAILURE:
      clErrorText := 'CL_MAP_FAILURE';

    CL_INVALID_VALUE:
      clErrorText := 'CL_INVALID_VALUE';
    CL_INVALID_DEVICE_TYPE:
      clErrorText := 'CL_INVALID_DEVICE_TYPE';
    CL_INVALID_PLATFORM:
      clErrorText := 'CL_INVALID_PLATFORM';
    CL_INVALID_DEVICE:
      clErrorText := 'CL_INVALID_DEVICE';
    CL_INVALID_CONTEXT:
      clErrorText := 'CL_INVALID_CONTEXT';
    CL_INVALID_QUEUE_PROPERTIES:
      clErrorText := 'CL_INVALID_QUEUE_PROPERTIES';
    CL_INVALID_COMMAND_QUEUE:
      clErrorText := 'CL_INVALID_COMMAND_QUEUE';
    CL_INVALID_HOST_PTR:
      clErrorText := 'CL_INVALID_HOST_PTR';
    CL_INVALID_MEM_OBJECT:
      clErrorText := 'CL_INVALID_MEM_OBJECT';
    CL_INVALID_IMAGE_FORMAT_DESCRIPTOR:
      clErrorText := 'CL_INVALID_IMAGE_FORMAT_DESCRIPTOR';
    CL_INVALID_IMAGE_SIZE:
      clErrorText := 'CL_INVALID_IMAGE_SIZE';
    CL_INVALID_SAMPLER:
      clErrorText := 'CL_INVALID_SAMPLER';
    CL_INVALID_BINARY:
      clErrorText := 'CL_INVALID_BINARY';
    CL_INVALID_BUILD_OPTIONS:
      clErrorText := 'CL_INVALID_BUILD_OPTIONS';
    CL_INVALID_PROGRAM:
      clErrorText := 'CL_INVALID_PROGRAM';
    CL_INVALID_PROGRAM_EXECUTABLE:
      clErrorText := 'CL_INVALID_PROGRAM_EXECUTABLE';
    CL_INVALID_KERNEL_NAME:
      clErrorText := 'CL_INVALID_KERNEL_NAME';
    CL_INVALID_KERNEL_DEFINITION:
      clErrorText := 'CL_INVALID_KERNEL_DEFINITION';
    CL_INVALID_KERNEL:
      clErrorText := 'CL_INVALID_KERNEL';
    CL_INVALID_ARG_INDEX:
      clErrorText := 'CL_INVALID_ARG_INDEX';
    CL_INVALID_ARG_VALUE:
      clErrorText := 'CL_INVALID_ARG_VALUE';
    CL_INVALID_ARG_SIZE:
      clErrorText := 'CL_INVALID_ARG_SIZE';
    CL_INVALID_KERNEL_ARGS:
      clErrorText := 'CL_INVALID_KERNEL_ARGS';
    CL_INVALID_WORK_DIMENSION:
      clErrorText := 'CL_INVALID_WORK_DIMENSION';
    CL_INVALID_WORK_GROUP_SIZE:
      clErrorText := 'CL_INVALID_WORK_GROUP_SIZE';
    CL_INVALID_WORK_ITEM_SIZE:
      clErrorText := 'CL_INVALID_WORK_ITEM_SIZE';
    CL_INVALID_GLOBAL_OFFSET:
      clErrorText := 'CL_INVALID_GLOBAL_OFFSET';
    CL_INVALID_EVENT_WAIT_LIST:
      clErrorText := 'CL_INVALID_EVENT_WAIT_LIST';
    CL_INVALID_EVENT:
      clErrorText := 'CL_INVALID_EVENT';
    CL_INVALID_OPERATION:
      clErrorText := 'CL_INVALID_OPERATION';
    CL_INVALID_GL_OBJECT:
      clErrorText := 'CL_INVALID_GL_OBJECT';
    CL_INVALID_BUFFER_SIZE:
      clErrorText := 'CL_INVALID_BUFFER_SIZE';
    CL_INVALID_MIP_LEVEL:
      clErrorText := 'CL_INVALID_MIP_LEVEL';
  else
    clErrorText := 'Unknown OpenCL error';
  end;
end;

end.
