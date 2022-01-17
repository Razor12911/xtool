unit ZSTDDLL;

interface

uses
  WinAPI.Windows,
  System.SysUtils;

type
  ZSTD_strategy = (ZSTD_fast = 1, ZSTD_dfast = 2, ZSTD_greedy = 3,
    ZSTD_lazy = 4, ZSTD_lazy2 = 5, ZSTD_btlazy2 = 6, ZSTD_btopt = 7,
    ZSTD_btultra = 8, ZSTD_btultra2 = 9);

  ZSTD_ResetDirective = (ZSTD_reset_session_only = 1, ZSTD_reset_parameters = 2,
    ZSTD_reset_session_and_parameters = 3);

  PZSTD_inBuffer = ^ZSTD_inBuffer;

  ZSTD_inBuffer = record
    src: Pointer;
    size: size_t;
    pos: size_t;
  end;

  PZSTD_outBuffer = ^ZSTD_outBuffer;

  ZSTD_outBuffer = record
    dst: Pointer;
    size: size_t;
    pos: size_t;
  end;

  PZSTD_CCtx_params = Pointer;

  ZSTD_cParameter = (ZSTD_c_compressionLevel = 100, ZSTD_c_windowLog = 101,
    ZSTD_c_hashLog = 102, ZSTD_c_chainLog = 103, ZSTD_c_searchLog = 104,
    ZSTD_c_minMatch = 105, ZSTD_c_targetLength = 106, ZSTD_c_strategy = 107,
    ZSTD_c_enableLongDistanceMatching = 160, ZSTD_c_ldmHashLog = 161,
    ZSTD_c_ldmMinMatch = 162, ZSTD_c_ldmBucketSizeLog = 163,
    ZSTD_c_ldmHashRateLog = 164, ZSTD_c_contentSizeFlag = 200,
    ZSTD_c_checksumFlag = 201, ZSTD_c_dictIDFlag = 202, ZSTD_c_nbWorkers = 400,
    ZSTD_c_jobSize = 401, ZSTD_c_overlapLog = 402,
    ZSTD_c_experimentalParam1 = 500, ZSTD_c_experimentalParam2 = 10,
    ZSTD_c_experimentalParam3 = 1000, ZSTD_c_experimentalParam4 = 1001,
    ZSTD_c_experimentalParam5 = 1002, ZSTD_c_experimentalParam6 = 1003,
    ZSTD_c_experimentalParam7 = 1004, ZSTD_c_experimentalParam8 = 1005,
    ZSTD_c_experimentalParam9 = 1006, ZSTD_c_experimentalParam10 = 1007,
    ZSTD_c_experimentalParam11 = 1008, ZSTD_c_experimentalParam12 = 1009,
    ZSTD_c_experimentalParam13 = 1010, ZSTD_c_experimentalParam14 = 1011,
    ZSTD_c_experimentalParam15 = 1012);

  ZSTD_compressionParameters = record
    windowLog: Cardinal;
    chainLogg: Cardinal;
    hashLogg: Cardinal;
    searchLogg: Cardinal;
    minMatchg: Cardinal;
    targetLengthg: Cardinal;
    strategy: ZSTD_strategy;
  end;

  ZSTD_frameParameters = record
    contentSizeFlag: Integer;
    checksumFlag: Integer;
    noDictIDFlag: Integer;
  end;

  ZSTD_parameters = record
    cParams: ZSTD_compressionParameters;
    fParams: ZSTD_frameParameters;
  end;

var
  ZSTD_compress: function(dst: Pointer; dstCapacity: size_t; const src: Pointer;
    srcSize: size_t; compressionLevel: Integer): size_t cdecl;
  ZSTD_decompress: function(dst: Pointer; dstCapacity: size_t;
    const src: Pointer; srcSize: size_t): SSIZE_T cdecl;
  ZSTD_findFrameCompressedSize: function(const src: Pointer; srcSize: size_t)
    : int64 cdecl;
  ZSTD_findDecompressedSize: function(const src: Pointer; srcSize: size_t)
    : int64 cdecl;
  ZSTD_createCCtx: function: Pointer cdecl;
  ZSTD_freeCCtx: function(cctx: Pointer): size_t cdecl;
  ZSTD_compressCCtx: function(cctx: Pointer; dst: Pointer; dstCapacity: size_t;
    src: Pointer; srcSize: size_t; compressionLevel: Integer): size_t cdecl;
  ZSTD_createDCtx: function: Pointer cdecl;
  ZSTD_freeDCtx: function(dctx: Pointer): size_t cdecl;
  ZSTD_decompressDCtx: function(dctx: Pointer; dst: Pointer;
    dstCapacity: size_t; src: Pointer; srcSize: size_t): size_t cdecl;
  ZSTD_createCDict: function(const dict: Pointer; dictSize: size_t;
    compressionLevel: Integer): Pointer cdecl;
  ZSTD_freeCDict: function(ddict: Pointer): size_t cdecl;
  ZSTD_createDDict: function(const dict: Pointer; dictSize: size_t)
    : Pointer cdecl;
  ZSTD_freeDDict: function(ddict: Pointer): size_t cdecl;
  ZSTD_compress_usingCDict: function(cctx: Pointer; dst: Pointer;
    dstCapacity: size_t; const src: Pointer; srcSize: size_t;
    const cdict: Pointer): size_t cdecl;
  ZSTD_decompress_usingDDict: function(dctx: Pointer; dst: Pointer;
    dstCapacity: size_t; const src: Pointer; srcSize: size_t;
    const ddict: Pointer): size_t cdecl;

  ZSTD_getParams: function(compressionLevel: Integer; estimatedSrcSize: UInt64;
    dictSize: size_t): ZSTD_parameters cdecl;
  ZSTD_initCStream: function(zcs: Pointer; compressionLevel: Integer)
    : size_t cdecl;
  ZSTD_initCStream_advanced: function(zcs: Pointer; const dict: Pointer;
    dictSize: size_t; params: ZSTD_parameters; pledgedSrcSize: UInt64)
    : size_t cdecl;
  ZSTD_compressStream: function(zcs: Pointer; output: PZSTD_outBuffer;
    input: PZSTD_inBuffer): size_t cdecl;
  ZSTD_flushStream: function(zcs: Pointer; output: PZSTD_outBuffer)
    : size_t cdecl;
  ZSTD_endStream: function(zcs: Pointer; output: PZSTD_outBuffer): size_t cdecl;

  ZSTD_createCCtxParams: function: PZSTD_CCtx_params cdecl;
  ZSTD_freeCCtxParams: function(params: PZSTD_CCtx_params): size_t cdecl;
  ZSTD_CCtxParams_reset: function(params: PZSTD_CCtx_params): size_t cdecl;
  ZSTD_CCtxParams_init: function(cctxParams: PZSTD_CCtx_params;
    compressionLevel: Integer): size_t cdecl;
  ZSTD_CCtx_setParameter: function(params: PZSTD_CCtx_params;
    param: ZSTD_cParameter; value: Integer): size_t cdecl;
  ZSTD_CCtx_setParametersUsingCCtxParams: function(cctx: Pointer;
    const params: PZSTD_CCtx_params): size_t cdecl;

  ZSTD_CStreamInSize: function: size_t cdecl;
  ZSTD_CStreamOutSize: function: size_t cdecl;
  DLLLoaded: Boolean = False;

function ZSTD_compress_dict(cctx: Pointer; dst: Pointer; dstCapacity: size_t;
  const src: Pointer; srcSize: size_t; const cdict: Pointer): size_t;
function ZSTD_decompress_dict(dctx: Pointer; dst: Pointer; dstCapacity: size_t;
  const src: Pointer; srcSize: size_t; const ddict: Pointer): size_t;

implementation

function ZSTD_compress_dict(cctx: Pointer; dst: Pointer; dstCapacity: size_t;
  const src: Pointer; srcSize: size_t; const cdict: Pointer): size_t;
begin
  cctx := ZSTD_createDCtx;
  Result := ZSTD_compress_usingCDict(cctx, dst, dstCapacity, src,
    srcSize, cdict);
  ZSTD_freeDCtx(cctx);
end;

function ZSTD_decompress_dict(dctx: Pointer; dst: Pointer; dstCapacity: size_t;
  const src: Pointer; srcSize: size_t; const ddict: Pointer): size_t;
begin
  dctx := ZSTD_createDCtx;
  Result := ZSTD_decompress_usingDDict(dctx, dst, dstCapacity, src,
    srcSize, ddict);
  ZSTD_freeDCtx(dctx);
end;

var
  DLLHandle: THandle;

procedure Init;
begin
  if DLLLoaded then
    Exit;
  DLLHandle := LoadLibrary(PChar(ExtractFilePath(ParamStr(0)) + 'libzstd.dll'));
  if DLLHandle >= 32 then
  begin
    DLLLoaded := True;
    @ZSTD_compress := GetProcAddress(DLLHandle, 'ZSTD_compress');
    Assert(@ZSTD_compress <> nil);
    @ZSTD_decompress := GetProcAddress(DLLHandle, 'ZSTD_decompress');
    Assert(@ZSTD_decompress <> nil);
    @ZSTD_findFrameCompressedSize := GetProcAddress(DLLHandle,
      'ZSTD_findFrameCompressedSize');
    Assert(@ZSTD_findFrameCompressedSize <> nil);
    @ZSTD_findDecompressedSize := GetProcAddress(DLLHandle,
      'ZSTD_findDecompressedSize');
    Assert(@ZSTD_findDecompressedSize <> nil);
    @ZSTD_createCCtx := GetProcAddress(DLLHandle, 'ZSTD_createCCtx');
    Assert(@ZSTD_createCCtx <> nil);
    @ZSTD_freeCCtx := GetProcAddress(DLLHandle, 'ZSTD_freeCCtx');
    Assert(@ZSTD_freeCCtx <> nil);
    @ZSTD_createDCtx := GetProcAddress(DLLHandle, 'ZSTD_createDCtx');
    Assert(@ZSTD_createDCtx <> nil);
    @ZSTD_freeDCtx := GetProcAddress(DLLHandle, 'ZSTD_freeDCtx');
    Assert(@ZSTD_freeDCtx <> nil);
    @ZSTD_createCDict := GetProcAddress(DLLHandle, 'ZSTD_createCDict');
    Assert(@ZSTD_createCDict <> nil);
    @ZSTD_freeCDict := GetProcAddress(DLLHandle, 'ZSTD_freeCDict');
    Assert(@ZSTD_freeCDict <> nil);
    @ZSTD_compressCCtx := GetProcAddress(DLLHandle, 'ZSTD_compressCCtx');
    Assert(@ZSTD_compressCCtx <> nil);
    @ZSTD_createDDict := GetProcAddress(DLLHandle, 'ZSTD_createDDict');
    Assert(@ZSTD_createDDict <> nil);
    @ZSTD_freeDDict := GetProcAddress(DLLHandle, 'ZSTD_freeDDict');
    Assert(@ZSTD_freeDDict <> nil);
    @ZSTD_decompressDCtx := GetProcAddress(DLLHandle, 'ZSTD_decompressDCtx');
    Assert(@ZSTD_decompressDCtx <> nil);
    @ZSTD_compress_usingCDict := GetProcAddress(DLLHandle,
      'ZSTD_compress_usingCDict');
    Assert(@ZSTD_compress_usingCDict <> nil);
    @ZSTD_decompress_usingDDict := GetProcAddress(DLLHandle,
      'ZSTD_decompress_usingDDict');
    Assert(@ZSTD_decompress_usingDDict <> nil);

    @ZSTD_getParams := GetProcAddress(DLLHandle, 'ZSTD_getParams');
    Assert(@ZSTD_getParams <> nil);
    @ZSTD_initCStream := GetProcAddress(DLLHandle, 'ZSTD_initCStream');
    Assert(@ZSTD_initCStream <> nil);
    @ZSTD_initCStream_advanced := GetProcAddress(DLLHandle,
      'ZSTD_initCStream_advanced');
    Assert(@ZSTD_initCStream_advanced <> nil);
    @ZSTD_compressStream := GetProcAddress(DLLHandle, 'ZSTD_compressStream');
    Assert(@ZSTD_compressStream <> nil);
    @ZSTD_flushStream := GetProcAddress(DLLHandle, 'ZSTD_flushStream');
    Assert(@ZSTD_flushStream <> nil);
    @ZSTD_endStream := GetProcAddress(DLLHandle, 'ZSTD_endStream');
    Assert(@ZSTD_endStream <> nil);

    @ZSTD_CStreamInSize := GetProcAddress(DLLHandle, 'ZSTD_CStreamInSize');
    Assert(@ZSTD_CStreamInSize <> nil);
    @ZSTD_CStreamOutSize := GetProcAddress(DLLHandle, 'ZSTD_CStreamOutSize');
    Assert(@ZSTD_CStreamOutSize <> nil);

    @ZSTD_createCCtxParams := GetProcAddress(DLLHandle,
      'ZSTD_createCCtxParams');
    Assert(@ZSTD_createCCtxParams <> nil);
    @ZSTD_freeCCtxParams := GetProcAddress(DLLHandle, 'ZSTD_freeCCtxParams');
    Assert(@ZSTD_freeCCtxParams <> nil);
    @ZSTD_CCtxParams_reset := GetProcAddress(DLLHandle,
      'ZSTD_CCtxParams_reset');
    Assert(@ZSTD_CCtxParams_reset <> nil);
    @ZSTD_CCtxParams_init := GetProcAddress(DLLHandle, 'ZSTD_CCtxParams_init');
    Assert(@ZSTD_CCtxParams_init <> nil);
    @ZSTD_CCtx_setParameter := GetProcAddress(DLLHandle,
      'ZSTD_CCtx_setParameter');
    Assert(@ZSTD_CCtx_setParameter <> nil);
    @ZSTD_CCtx_setParametersUsingCCtxParams :=
      GetProcAddress(DLLHandle, 'ZSTD_CCtx_setParametersUsingCCtxParams');
    Assert(@ZSTD_CCtx_setParametersUsingCCtxParams <> nil);
  end
  else
    DLLLoaded := False;
end;

procedure Deinit;
begin
  if not DLLLoaded then
    Exit;
  FreeLibrary(DLLHandle);
end;

initialization

Init;

finalization

Deinit;

end.
