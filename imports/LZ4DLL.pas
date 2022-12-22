unit LZ4DLL;

interface

uses
  Utils, LibImport,
  WinAPI.Windows,
  System.SysUtils, System.Math;

const
  LZ4F_VERSION = 100;

type
  PLZ4_streamDecode_t = ^LZ4_streamDecode_t;
  LZ4_streamDecode_t = array [0 .. 1 shl 9 - 1] of byte;

  PLZ4_stream_t = ^LZ4_stream_t;
  LZ4_stream_t = array [0 .. 1 shl 9 - 1] of byte;

  PLZ4_streamHC_t = ^LZ4_streamHC_t;
  LZ4_streamHC_t = array [0 .. 1 shl 9 - 1] of byte;

  LZ4F_errorCode_t = type size_t;

  LZ4F_blockSizeID_t = (LZ4F_default = 0, LZ4F_max64KB = 4, LZ4F_max256KB = 5,
    LZ4F_max1MB = 6, LZ4F_max4MB = 7, LZ4F_blockSizeID_Force32 = $40000000);
  LZ4F_blockMode_t = (LZ4F_blockLinked = 0, LZ4F_blockIndependent,
    LZ4F_blockMode_Force32 = $40000000);

  LZ4F_contentChecksum_t = (LZ4F_noContentChecksum = 0,
    LZ4F_contentChecksumEnabled, LZ4F_contentChecksum_Force32 = $40000000);

  LZ4F_blockChecksum_t = (LZ4F_noBlockChecksum = 0, LZ4F_blockChecksumEnabled,
    LZ4F_blockChecksum_Force32 = $40000000);

  LZ4F_frameType_t = (LZ4F_frame = 0, LZ4F_skippableFrame,
    LZ4F_frameType_Force32 = $40000000);

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
    srcBuffer: Pointer; srcSize: size_t; preferencesPtr: PLZ4F_preferences_t)
    : size_t cdecl;
  LZ4_compressHC2: function(const src: Pointer; dst: Pointer; srcSize: Integer;
    compressionLevel: Integer): Integer cdecl;
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
  LZ4_createStreamDecode: function: PLZ4_streamDecode_t cdecl;
  LZ4_freeStreamDecode: function(LZ4_stream: PLZ4_streamDecode_t)
    : Integer cdecl;
  LZ4_decompress_safe_continue: function(LZ4_stream: PLZ4_streamDecode_t;
    const src: Pointer; dst: Pointer; srcSize: Integer; dstCapacity: Integer)
    : Integer cdecl;
  LZ4_createStream: function: PLZ4_stream_t cdecl;
  LZ4_freeStream: function(streamPtr: PLZ4_stream_t): Integer cdecl;
  LZ4_resetStream: procedure(streamHCPtr: PLZ4_stream_t)cdecl;
  LZ4_compress_fast_continue: function(streamPtr: PLZ4_stream_t;
    const src: Pointer; dst: Pointer; srcSize: Integer; maxDstSize: Integer;
    acceleration: Integer): Integer cdecl;
  LZ4_createStreamHC: function: PLZ4_streamHC_t cdecl;
  LZ4_freeStreamHC: function(streamHCPtr: PLZ4_streamHC_t): Integer cdecl;
  LZ4_resetStreamHC: procedure(streamHCPtr: PLZ4_streamHC_t;
    compressionLevel: Integer)cdecl;
  LZ4_compress_HC_continue: function(streamHCPtr: PLZ4_streamHC_t;
    const src: Pointer; dst: Pointer; srcSize: Integer; maxDstSize: Integer)
    : Integer cdecl;
  DLLLoaded: Boolean = False;

function LZ4F_decompress_safe(source: Pointer; dest: Pointer;
  sourceSize: Integer; destSize: Integer; compressedSize: PInteger = nil;
  blockSize: PInteger = nil): Integer;
function LZ4_compress_block(src, dst: Pointer;
  srcSize, dstCapacity: Integer): Integer;

implementation

function LZ4F_decompress_safe(source: Pointer; dest: Pointer;
  sourceSize: Integer; destSize: Integer; compressedSize: PInteger;
  blockSize: PInteger): Integer;
var
  ctx: LZ4F_dctx;
  fi: LZ4F_frameInfo_t;
  srcSizePtr, dstSizePtr, srcSizePtr2: size_t;
begin
  Result := 0;
  if Assigned(compressedSize) then
    compressedSize^ := 0;
  if Assigned(blockSize) then
    blockSize^ := 4;
  if NativeUInt(LZ4F_createDecompressionContext(ctx)) = 0 then
    try
      srcSizePtr := sourceSize;
      dstSizePtr := destSize;
      try
        FillChar(fi, SizeOf(LZ4F_frameInfo_t), 0);
        srcSizePtr2 := sourceSize;
        if LZ4F_decompress(ctx, dest, dstSizePtr, source, srcSizePtr, nil) = 0
        then
        begin
          LZ4F_getFrameInfo(ctx, fi, source, srcSizePtr2);
          if Assigned(compressedSize) then
            compressedSize^ := srcSizePtr;
          if Assigned(blockSize) then
            blockSize^ := Max(4, Integer(fi.blockSizeID));
          Result := dstSizePtr;
        end;
      finally
        LZ4F_freeDecompressionContext(ctx);
      end;
    except
    end;
end;

function LZ4_compress_block(src, dst: Pointer;
  srcSize, dstCapacity: Integer): Integer;
const
  blockSize = 64 * 1024;
const
  BuffSize = 256 * 1024;
var
  Buff: array [0 .. BuffSize - 1] of byte;
  ctx: PLZ4_stream_t;
  Pos1, Pos2, Res: Integer;
  X, Y: Integer;
begin
  Result := 0;
  ctx := LZ4_createStream;
  LZ4_resetStream(ctx);
  Pos1 := 0;
  Pos2 := 0;
  try
    while (Pos1 < srcSize) and (Pos2 < dstCapacity) do
    begin
      X := Min(srcSize - Pos1, blockSize);
      Y := dstCapacity - Pos2;
      Res := LZ4_compress_fast_continue(ctx, PByte(src) + Pos1, @Buff[0], X,
        BuffSize, 1);
      if Res <= 0 then
      begin
        LZ4_freeStream(ctx);
        exit(-Pos2);
      end;
      Move(Buff[0], (PByte(dst) + Pos2)^, Res);
      Inc(Pos1, X);
      Inc(Pos2, Res);
    end;
  finally
    LZ4_freeStream(ctx);
  end;
  Result := Pos2;
end;

function UnravelEncode(InBuff: Pointer; InSize: Integer; OutBuff: Pointer;
  OutSize: Integer): Integer;
const
  blockSize = 65536;
var
  ctx: PLZ4_streamHC_t;
  Pos1, Pos2, Res: Integer;
  X, Y: Integer;
begin
  Result := 0;
  ctx := LZ4_createStreamHC;
  LZ4_resetStreamHC(ctx, 9);
  Pos1 := 0;
  Pos2 := 0;
  try
    while (Pos1 < InSize) do
    begin
      X := Min(InSize - Pos1, blockSize);
      Y := OutSize - (Pos2 + Integer.Size);
      Res := LZ4_compress_HC_continue(ctx, PByte(InBuff) + Pos1,
        PByte(OutBuff) + Pos2 + Integer.Size, X, Y);
      if Res <= 0 then
      begin
        LZ4_freeStreamHC(ctx);
        exit(-Pos2);
      end;
      PInteger(PByte(OutBuff) + Pos2)^ := Res;
      Inc(Pos1, X);
      Inc(Pos2, Res + Integer.Size);
    end;
  finally
    LZ4_freeStreamHC(ctx);
  end;
  Result := Pos2;
end;

function UnravelDecode(InBuff: Pointer; InSize: Integer; OutBuff: Pointer;
  OutSize: Integer): Integer;
const
  blockSize = 65536;
var
  ctx: PLZ4_streamDecode_t;
  Pos1, Pos2, Res: Integer;
begin
  Result := 0;
  ctx := LZ4_createStreamDecode;
  Pos1 := 0;
  Pos2 := 0;
  try
    while (Pos1 < InSize) and (Pos2 < OutSize) do
    begin
      Res := LZ4_decompress_safe_continue(ctx, PByte(InBuff) + Pos1 +
        Integer.Size, PByte(OutBuff) + Pos2, PInteger(PByte(InBuff) + Pos1)^,
        Min(OutSize - Pos2, blockSize));
      if Res <= 0 then
      begin
        LZ4_freeStreamDecode(ctx);
        exit(-Pos2);
      end;
      Inc(Pos1, PInteger(PByte(InBuff) + Pos1)^ + Integer.Size);
      Inc(Pos2, Res);
    end;
  finally
    LZ4_freeStreamDecode(ctx);
  end;
  Result := Pos2;
end;

var
  Lib: TLibImport;

procedure Init(Filename: String);
begin
  Lib := TLibImport.Create(ExpandPath(Filename));
  if Lib.Loaded then
  begin
    @LZ4_decompress_safe := Lib.GetProcAddr('LZ4_decompress_safe');
    @LZ4_decompress_fast := Lib.GetProcAddr('LZ4_decompress_fast');
    @LZ4_compress_default := Lib.GetProcAddr('LZ4_compress_default');
    @LZ4_compress_fast := Lib.GetProcAddr('LZ4_compress_fast');
    @LZ4_compress_HC := Lib.GetProcAddr('LZ4_compress_HC');
    @LZ4_compressHC2 := Lib.GetProcAddr('LZ4_compressHC2');
    @LZ4F_compressFrame := Lib.GetProcAddr('LZ4F_compressFrame');
    @LZ4F_compressFrameBound := Lib.GetProcAddr('LZ4F_compressFrameBound');
    @LZ4F_createDecompressionContext :=
      Lib.GetProcAddr('LZ4F_createDecompressionContext');
    @LZ4F_freeDecompressionContext :=
      Lib.GetProcAddr('LZ4F_freeDecompressionContext');
    @LZ4F_decompress := Lib.GetProcAddr('LZ4F_decompress');
    @LZ4F_getFrameInfo := Lib.GetProcAddr('LZ4F_getFrameInfo');
    @LZ4_createStreamDecode := Lib.GetProcAddr('LZ4_createStreamDecode');
    @LZ4_freeStreamDecode := Lib.GetProcAddr('LZ4_freeStreamDecode');
    @LZ4_decompress_safe_continue :=
      Lib.GetProcAddr('LZ4_decompress_safe_continue');
    @LZ4_createStream := Lib.GetProcAddr('LZ4_createStream');
    @LZ4_freeStream := Lib.GetProcAddr('LZ4_freeStream');
    @LZ4_resetStream := Lib.GetProcAddr('LZ4_resetStream');
    @LZ4_compress_fast_continue :=
      Lib.GetProcAddr('LZ4_compress_fast_continue');
    @LZ4_createStreamHC := Lib.GetProcAddr('LZ4_createStreamHC');
    @LZ4_freeStreamHC := Lib.GetProcAddr('LZ4_freeStreamHC');
    @LZ4_resetStreamHC := Lib.GetProcAddr('LZ4_resetStreamHC');
    @LZ4_compress_HC_continue := Lib.GetProcAddr('LZ4_compress_HC_continue');
    DLLLoaded := Assigned(LZ4_decompress_safe);
  end;
end;

procedure Deinit;
begin
  Lib.Free;
end;

const
  DLLParam1 = '--lz4=';
  DLLParam2 = '-l4';

var
  I: Integer;
  DLLFile: String;

initialization

DLLFile := 'liblz4.dll';
for I := 1 to ParamCount do
begin
  if ParamStr(I).StartsWith(DLLParam1) then
  begin
    DLLFile := ParamStr(I).Substring(DLLParam1.Length);
    break;
  end;
  if ParamStr(I).StartsWith(DLLParam2) then
  begin
    DLLFile := ParamStr(I).Substring(DLLParam2.Length);
    break;
  end;
end;

Init(DLLFile);

finalization

Deinit;

end.
