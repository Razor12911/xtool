unit LZ4DLL;

interface

uses
  WinAPI.Windows,
  System.SysUtils, System.Classes;

const
  LZ4F_VERSION = 100;

type
  LZ4F_errorCode_t = type size_t;

  LZ4F_blockSizeID_t = (LZ4F_default = 0, LZ4F_max64KB = 4, LZ4F_max256KB = 5,
    LZ4F_max1MB = 6, LZ4F_max4MB = 7);
  LZ4F_blockMode_t = (LZ4F_blockLinked = 0, LZ4F_blockIndependent);

  LZ4F_contentChecksum_t = (LZ4F_noContentChecksum = 0,
    LZ4F_contentChecksumEnabled);

  LZ4F_blockChecksum_t = (LZ4F_noBlockChecksum = 0, LZ4F_blockChecksumEnabled);

  LZ4F_frameType_t = (LZ4F_frame = 0, LZ4F_skippableFrame);

  LZ4F_frameInfo_t = record
    blockSizeID: LZ4F_blockSizeID_t;
    blockMode: LZ4F_blockMode_t;
    contentChecksumFlag: LZ4F_contentChecksum_t;
    frameType: LZ4F_frameType_t;
    contentSize: UInt64;
    dictID: Cardinal;
    blockChecksumFlag: LZ4F_blockChecksum_t;
  end;

  LZ4F_preferences_t = record
    frameInfo: LZ4F_frameInfo_t;
    compressionLevel: Integer;
    autoFlush: Cardinal;
    favorDecSpeed: Cardinal;
    reserved: packed array [0 .. 2] of Cardinal;
  end;

  PLZ4F_preferences_t = ^LZ4F_preferences_t;

  LZ4F_dctx = type Pointer;

  LZ4F_decompressOptions_t = record
    stableDst: Cardinal;
    reserved: packed array [0 .. 2] of Cardinal;
  end;

  PLZ4F_decompressOptions_t = ^LZ4F_decompressOptions_t;

var
  LZ4_decompress_safe: function(source: Pointer; dest: Pointer;
    compressedSize: Integer; maxDecompressedSize: Integer): Integer cdecl;
  LZ4_decompress_fast: function(source: Pointer; dest: Pointer;
    originalSize: Integer): Integer cdecl;
  LZ4_compress_default: function(src, dst: Pointer;
    srcSize, dstCapacity: Integer): Integer cdecl;
  LZ4_compress_fast: function(src, dst: Pointer; srcSize, dstCapacity: Integer;
    acceleration: Integer): Integer cdecl;
  LZ4_compress_HC: function(const src: Pointer; dst: Pointer; srcSize: Integer;
    maxDstSize: Integer; compressionLevel: Integer): Integer cdecl;
  LZ4F_compressFrame: function(dstBuffer: Pointer; dstCapacity: size_t;
    srcBuffer: Pointer; srcSize: size_t;
    const preferencesPtr: LZ4F_preferences_t): size_t cdecl;
  LZ4F_compressFrameBound: function(srcSize: size_t;
    preferencesPtr: PLZ4F_preferences_t): size_t cdecl;
  LZ4F_createDecompressionContext: function(out dctxPtr: LZ4F_dctx;
    version: Cardinal = LZ4F_VERSION): LZ4F_errorCode_t cdecl;
  LZ4F_freeDecompressionContext: function(dctx: LZ4F_dctx)
    : LZ4F_errorCode_t cdecl;
  LZ4F_decompress: function(dctx: LZ4F_dctx; dstBuffer: Pointer;
    var dstSizePtr: size_t; srcBuffer: Pointer; var srcSizePtr: size_t;
    dOptPtr: PLZ4F_decompressOptions_t): size_t cdecl;
  LZ4F_getFrameInfo: function(dctx: LZ4F_dctx;
    out frameInfoPtr: LZ4F_frameInfo_t; srcBuffer: Pointer;
    out srcSizePtr: size_t): size_t cdecl;
  DLLLoaded: Boolean = False;

function LZ4F_decompress_safe(source: Pointer; dest: Pointer;
  compressedSize: Integer; maxDecompressedSize: Integer): Integer;

implementation

var
  DLLHandle: THandle;

procedure Init;
begin
  if DLLLoaded then
    Exit;
  DLLHandle := 0;
  DLLHandle := LoadLibrary(PWideChar(ExtractFilePath(ParamStr(0)) +
    'liblz4.dll'));
  if DLLHandle >= 32 then
  begin
    DLLLoaded := True;
    @LZ4_decompress_safe := GetProcAddress(DLLHandle, 'LZ4_decompress_safe');
    Assert(@LZ4_decompress_safe <> nil);
    @LZ4_decompress_fast := GetProcAddress(DLLHandle, 'LZ4_decompress_fast');
    Assert(@LZ4_decompress_fast <> nil);
    @LZ4_compress_default := GetProcAddress(DLLHandle, 'LZ4_compress_default');
    Assert(@LZ4_compress_default <> nil);
    @LZ4_compress_fast := GetProcAddress(DLLHandle, 'LZ4_compress_fast');
    Assert(@LZ4_compress_fast <> nil);
    @LZ4_compress_HC := GetProcAddress(DLLHandle, 'LZ4_compress_HC');
    Assert(@LZ4_compress_HC <> nil);
    @LZ4F_compressFrame := GetProcAddress(DLLHandle, 'LZ4F_compressFrame');
    Assert(@LZ4F_compressFrame <> nil);
    @LZ4F_compressFrameBound := GetProcAddress(DLLHandle,
      'LZ4F_compressFrameBound');
    Assert(@LZ4F_compressFrameBound <> nil);
    @LZ4F_createDecompressionContext := GetProcAddress(DLLHandle,
      'LZ4F_createDecompressionContext');
    Assert(@LZ4F_createDecompressionContext <> nil);
    @LZ4F_freeDecompressionContext := GetProcAddress(DLLHandle,
      'LZ4F_freeDecompressionContext');
    Assert(@LZ4F_freeDecompressionContext <> nil);
    @LZ4F_decompress := GetProcAddress(DLLHandle, 'LZ4F_decompress');
    Assert(@LZ4F_decompress <> nil);
    @LZ4F_getFrameInfo := GetProcAddress(DLLHandle, 'LZ4F_getFrameInfo');
    Assert(@LZ4F_getFrameInfo <> nil);
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

function LZ4F_decompress_safe(source: Pointer; dest: Pointer;
  compressedSize: Integer; maxDecompressedSize: Integer): Integer;
var
  ctx: LZ4F_dctx;
  srcSizePtr, dstSizePtr: size_t;
begin
  Result := 0;
  if NativeUInt(LZ4F_createDecompressionContext(ctx)) = 0 then
    try
      srcSizePtr := compressedSize;
      dstSizePtr := maxDecompressedSize;
      try
        if LZ4F_decompress(ctx, dest, dstSizePtr, source, srcSizePtr, nil) = 0
        then
          Result := dstSizePtr;
      finally
        LZ4F_freeDecompressionContext(ctx);
      end;
    except
    end;
end;

initialization

Init;

finalization

Deinit;

end.
