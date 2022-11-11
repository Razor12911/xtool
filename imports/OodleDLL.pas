unit OodleDLL;

interface

uses
  LibImport,
  WinAPI.Windows,
  System.SysUtils, System.Types, System.IOUtils;

type
  POodleLZ_CompressOptions = ^TOodleLZ_CompressOptions;

  TOodleLZ_CompressOptions = record
    verbosity: Integer;
    minMatchLen: Integer;
    seekChunkReset: LongBool;
    seekChunkLen: Integer;
    profile: Cardinal;
    dictionarySize: Integer;
    spaceSpeedTradeoffBytes: Integer;
    maxHuffmansPerChunk: Integer;
    sendQuantumCRCs: LongBool;
    maxLocalDictionarySize: Integer;
    makeLongRangeMatcher: LongBool;
    matchTableSizeLog2: Integer;
    jobify: Integer;
    jobifyUserPtr: Pointer;
    farMatchMinLen: Integer;
    farMatchOffsetLog2: Integer;
    reserved: array [0 .. 3] of Integer;
  end;

var
  Oodle_CheckVersion: function(oodle_header_version: Cardinal;
    pOodleLibVersion: PCardinal = nil): LongBool stdcall;
  OodleLZ_Compress_1: function(compressor: Integer; rawBuf: Pointer;
    rawLen: NativeUInt; compBuf: Pointer; compressSelect: Integer = 6;
    pOptions: POodleLZ_CompressOptions = nil; dictionaryBase: Pointer = nil;
    lrm: Pointer = nil): NativeUInt stdcall;
  OodleLZ_Compress_2: function(compressor: Integer; rawBuf: Pointer;
    rawLen: NativeUInt; compBuf: Pointer; compressSelect: Integer = 6;
    pOptions: POodleLZ_CompressOptions = nil; dictionaryBase: Pointer = nil;
    lrm: Pointer = nil; scratchMem: Pointer = nil; scratchSize: NativeUInt = 0)
    : NativeUInt stdcall;
  OodleLZ_Decompress: function(const compBuf: Pointer; compBufSize: NativeUInt;
    rawBuf: Pointer; rawLen: NativeUInt; OodleLZ_FuzzSafe: Integer = 0;
    OodleLZ_CheckCRC: Integer = 0; OodleLZ_Verbosity: Integer = 0;
    decBufBase: Pointer = nil; decBufSize: NativeUInt = 0;
    fpCallback: Pointer = nil; callbackUserData: Pointer = nil;
    decoderMemory: Pointer = nil; decoderMemorySize: NativeUInt = 0;
    threadPhase: Integer = 0): NativeUInt stdcall;
  OodleLZ_CompressOptions_GetDefault_1: function(compressor: Integer;
    lzLevel: Integer): POodleLZ_CompressOptions stdcall;
  OodleLZ_CompressOptions_GetDefault_2: function
    : POodleLZ_CompressOptions stdcall;
  OodleLZ_GetCompressedBufferSizeNeeded_1: function(rawSize: NativeUInt)
    : NativeUInt stdcall;
  OodleLZ_GetCompressedBufferSizeNeeded_2: function(compressor: Integer;
    rawSize: NativeUInt): NativeUInt stdcall;

  DLLLoaded: Boolean = False;

function OodleLZ_Compress(compressor: Integer; rawBuf: Pointer;
  rawLen: NativeUInt; compBuf: Pointer; compressSelect: Integer = 6;
  pOptions: POodleLZ_CompressOptions = nil; dictionaryBase: Pointer = nil;
  lrm: Pointer = nil; scratchMem: Pointer = nil; scratchSize: NativeUInt = 0)
  : NativeUInt;
function OodleLZ_CompressOptions_GetDefault(compressor: Integer;
  lzLevel: Integer): POodleLZ_CompressOptions;
function OodleLZ_GetCompressedBufferSizeNeeded(compressor: Byte;
  rawSize: NativeUInt): NativeUInt;

implementation

var
  Lib: TLibImport;
  OldCompress, OldCompressOptions_GetDefault,
    OldGetCompressedBufferSizeNeeded: Boolean;

procedure Init(Filename: String);
var
  I: Integer;
  C: Cardinal;
begin
  Lib := TLibImport.Create(ExtractFilePath(ParamStr(0)) + Filename);
  if not Lib.Loaded then
    for I := 3 to 9 do
    begin
      Lib.Free;
      Lib := TLibImport.Create(ExtractFilePath(ParamStr(0)) + 'oo2core_' +
        I.ToString + '_win64.dll');
      if Lib.Loaded then
        break;
    end;
  if not Lib.Loaded then
    for I := 3 to 9 do
    begin
      Lib.Free;
      Lib := TLibImport.Create(ExtractFilePath(ParamStr(0)) + 'oo2ext_' +
        I.ToString + '_win64.dll');
      if Lib.Loaded then
        break;
    end;
  if Lib.Loaded then
  begin
    Oodle_CheckVersion := Lib.GetProcAddr('Oodle_CheckVersion');
    if not Assigned(Oodle_CheckVersion) then
      for I := 0 to 32 do
      begin
        @Oodle_CheckVersion :=
          Lib.GetProcAddr(PAnsiChar('_Oodle_CheckVersion@' + (I * 2).ToString));
        if Assigned(Oodle_CheckVersion) then
          break;
      end;
    DLLLoaded := Assigned(Oodle_CheckVersion);
    Oodle_CheckVersion(0, @C);
    OldCompress := LongRec(C).Hi < $2E06;
    OldGetCompressedBufferSizeNeeded := LongRec(C).Hi < $2E08;
    OldCompressOptions_GetDefault := LongRec(C).Hi < $2E08;
    @OodleLZ_Compress_1 := Lib.GetProcAddr('OodleLZ_Compress');
    if not Assigned(OodleLZ_Compress_1) then
      for I := 0 to 32 do
      begin
        @OodleLZ_Compress_1 :=
          Lib.GetProcAddr(PAnsiChar('_OodleLZ_Compress@' + (I * 2).ToString));
        if Assigned(OodleLZ_Compress_1) then
          break;
      end;
    @OodleLZ_Compress_2 := @OodleLZ_Compress_1;
    OodleLZ_Decompress := Lib.GetProcAddr('OodleLZ_Decompress');
    if not Assigned(OodleLZ_Decompress) then
      for I := 0 to 32 do
      begin
        @OodleLZ_Decompress :=
          Lib.GetProcAddr(PAnsiChar('_OodleLZ_Decompress@' + (I * 2).ToString));
        if Assigned(OodleLZ_Decompress) then
          break;
      end;
    OodleLZ_CompressOptions_GetDefault_1 :=
      Lib.GetProcAddr('OodleLZ_CompressOptions_GetDefault');
    if not Assigned(OodleLZ_CompressOptions_GetDefault_1) then
      for I := 0 to 32 do
      begin
        @OodleLZ_CompressOptions_GetDefault_1 :=
          Lib.GetProcAddr(PAnsiChar('_OodleLZ_CompressOptions_GetDefault@' +
          (I * 2).ToString));
        if Assigned(OodleLZ_CompressOptions_GetDefault_1) then
          break;
      end;
    @OodleLZ_CompressOptions_GetDefault_2 :=
      @OodleLZ_CompressOptions_GetDefault_1;
    OodleLZ_GetCompressedBufferSizeNeeded_1 :=
      Lib.GetProcAddr('OodleLZ_GetCompressedBufferSizeNeeded');
    if not Assigned(OodleLZ_GetCompressedBufferSizeNeeded_1) then
      for I := 0 to 32 do
      begin
        @OodleLZ_GetCompressedBufferSizeNeeded_1 :=
          Lib.GetProcAddr(PAnsiChar('_OodleLZ_GetCompressedBufferSizeNeeded@' +
          (I * 2).ToString));
        if Assigned(OodleLZ_GetCompressedBufferSizeNeeded_1) then
          break;
      end;
    @OodleLZ_GetCompressedBufferSizeNeeded_2 :=
      @OodleLZ_GetCompressedBufferSizeNeeded_1;
  end;
end;

procedure Deinit;
begin
  Lib.Free;
end;

function OodleLZ_Compress(compressor: Integer; rawBuf: Pointer;
  rawLen: NativeUInt; compBuf: Pointer; compressSelect: Integer;
  pOptions: POodleLZ_CompressOptions; dictionaryBase: Pointer; lrm: Pointer;
  scratchMem: Pointer; scratchSize: NativeUInt): NativeUInt;
begin
  if OldCompress then
    Result := OodleLZ_Compress_1(compressor, rawBuf, rawLen, compBuf,
      compressSelect, pOptions, dictionaryBase, lrm)
  else
    Result := OodleLZ_Compress_2(compressor, rawBuf, rawLen, compBuf,
      compressSelect, pOptions, dictionaryBase, lrm, scratchMem, scratchSize);
end;

function OodleLZ_CompressOptions_GetDefault(compressor: Integer;
  lzLevel: Integer): POodleLZ_CompressOptions;
begin
  if OldCompressOptions_GetDefault then
    Result := OodleLZ_CompressOptions_GetDefault_1(compressor, lzLevel)
  else
    Result := OodleLZ_CompressOptions_GetDefault_2;
end;

function OodleLZ_GetCompressedBufferSizeNeeded(compressor: Byte;
  rawSize: NativeUInt): NativeUInt;
begin
  if OldGetCompressedBufferSizeNeeded then
    Result := OodleLZ_GetCompressedBufferSizeNeeded_1(rawSize)
  else
    Result := OodleLZ_GetCompressedBufferSizeNeeded_2(compressor, rawSize);
end;

const
  DLLParam = '--oodle=';

var
  I: Integer;
  DLLFile: String;

initialization

DLLFile := 'oo2core_9_win64.dll';
for I := 1 to ParamCount do
  if ParamStr(I).StartsWith(DLLParam) then
  begin
    DLLFile := ParamStr(I).Substring(DLLParam.Length);
    break;
  end;
Init(DLLFile);

finalization

Deinit;

end.
