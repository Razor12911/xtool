unit FLZMA2DLL;

interface

uses
  InitCode,
  Utils, LibImport,
  WinAPI.Windows,
  System.SysUtils, System.Math, System.Classes, System.Types;

const
  FL2_DICTLOG_MIN = 20;
{$IFDEF CPU32BITS}
  FL2_DICTLOG_MAX = 27;
{$ELSE}
  FL2_DICTLOG_MAX = 30;
{$ENDIF}
  FL2_DICTSIZE_MIN = (1 shl FL2_DICTLOG_MIN);
  FL2_DICTSIZE_MAX = (1 shl FL2_DICTLOG_MAX);

  FL2_BLOCK_OVERLAP_MIN = 0;
  FL2_BLOCK_OVERLAP_MAX = 14;

{$IFDEF CPU32BITS}
  FL2_DEC_MEM = 1 shl 29;
{$ELSE}
  FL2_DEC_MEM = 1 shl 31;
{$ENDIF}

type
  PFL2_inBuffer = ^FL2_inBuffer;

  FL2_inBuffer = record
    src: Pointer;
    size: size_t;
    pos: size_t;
  end;

  PFL2_outBuffer = ^FL2_outBuffer;

  FL2_outBuffer = record
    dst: Pointer;
    size: size_t;
    pos: size_t;
  end;

type
  FL2_cParameter = (
    (* compression parameters *)
    FL2_p_compressionLevel,
    (* Update all compression parameters according to pre-defined cLevel table
      * Default level is FL2_CLEVEL_DEFAULT==6.
      * Setting FL2_p_highCompression to 1 switches to an alternate cLevel table. *)
    FL2_p_highCompression,
    (* Maximize compression ratio for a given dictionary size.
      * Levels 1..10 = dictionaryLog 20..29 (1 Mb..512 Mb).
      * Typically provides a poor speed/ratio tradeoff. *)
    FL2_p_dictionaryLog,
    (* Maximum allowed back-reference distance, expressed as power of 2.
      * Must be clamped between FL2_DICTLOG_MIN and FL2_DICTLOG_MAX.
      * Default = 24 *)
    FL2_p_dictionarySize, (* Same as above but expressed as an absolute value.
      * Must be clamped between FL2_DICTSIZE_MIN and FL2_DICTSIZE_MAX.
      * Default = 16 Mb *)
    FL2_p_overlapFraction,
    (* The radix match finder is block-based, so some overlap is retained from
      * each block to improve compression of the next. This value is expressed
      * as n / 16 of the block size (dictionary size). Larger values are slower.
      * Values above 2 mostly yield only a small improvement in compression.
      * A large value for a small dictionary may worsen multithreaded compression.
      * Default = 2 *)
    FL2_p_resetInterval,
    (* For multithreaded decompression. A dictionary reset will occur
      * after each dictionarySize * resetInterval bytes of input.
      * Default = 4 *)
    FL2_p_bufferResize,
    (* Buffering speeds up the matchfinder. Buffer resize determines the percentage of
      * the normal buffer size used, which depends on dictionary size.
      * 0=50, 1=75, 2=100, 3=150, 4=200. Higher number = slower, better
      * compression, higher memory usage. A CPU with a large memory cache
      * may make effective use of a larger buffer.
      * Default = 2 *)
    FL2_p_hybridChainLog,
    (* Size of the hybrid mode HC3 hash chain, as a power of 2.
      * Resulting table size is (1 << (chainLog+2)) bytes.
      * Larger tables result in better and slower compression.
      * This parameter is only used by the hybrid "ultra" strategy.
      * Default = 9 *)
    FL2_p_hybridCycles,
    (* Number of search attempts made by the HC3 match finder.
      * Used only by the hybrid "ultra" strategy.
      * More attempts result in slightly better and slower compression.
      * Default = 1 *)
    FL2_p_searchDepth,
    (* Match finder will resolve string matches up to this length. If a longer
      * match exists further back in the input, it will not be found.
      * Default = 42 *)
    FL2_p_fastLength, (* Only useful for strategies >= opt.
      * Length of match considered "good enough" to stop search.
      * Larger values make compression stronger and slower.
      * Default = 48 *)
    FL2_p_divideAndConquer,
    (* Split long chains of 2-byte matches into shorter chains with a small overlap
      * for further processing. Allows buffering of all chains at length 2.
      * Faster, less compression. Generally a good tradeoff.
      * Default = enabled *)
    FL2_p_strategy, (* 1 = fast; 2 = optimized, 3 = ultra (hybrid mode).
      * The higher the value of the selected strategy, the more complex it is,
      * resulting in stronger and slower compression.
      * Default = ultra *)
    FL2_p_literalCtxBits, (* lc value for LZMA2 encoder
      * Default = 3 *)
    FL2_p_literalPosBits, (* lp value for LZMA2 encoder
      * Default = 0 *)
    FL2_p_posBits, (* pb value for LZMA2 encoder
      * Default = 2 *)
    FL2_p_omitProperties,
    (* Omit the property byte at the start of the stream. For use within 7-zip *)
    (* or other containers which store the property byte elsewhere. *)
    (* A stream compressed under this setting cannot be decoded by this library. *)
    FL2_cParameter_Force32 = $40000000);

var
  FL2_compress: function(dst: Pointer; dstCapacity: size_t; const src: Pointer;
    srcSize: size_t; compressionLevel: Integer): size_t cdecl;
  FL2_compressMt: function(dst: Pointer; dstCapacity: size_t;
    const src: Pointer; srcSize: size_t; compressionLevel: Integer;
    nbThreads: Cardinal): size_t cdecl;
  FL2_decompress: function(dst: Pointer; dstCapacity: size_t;
    const src: Pointer; srcSize: size_t): size_t cdecl;
  FL2_decompressMt: function(dst: Pointer; dstCapacity: size_t;
    const src: Pointer; srcSize: size_t; nbThreads: Cardinal): size_t cdecl;

  FL2_createCCtx: function: Pointer cdecl;
  FL2_createCCtxMt: function(nbThreads: Cardinal): Pointer cdecl;
  FL2_freeCCtx: procedure(cctx: Pointer)cdecl;
  FL2_compressCCtx: function(cctx: Pointer; dst: Pointer; dstCapacity: size_t;
    src: Pointer; srcSize: size_t; compressionLevel: Integer): size_t cdecl;
  FL2_createDCtx: function: Pointer cdecl;
  FL2_createDCtxMt: function(nbThreads: Cardinal): Pointer cdecl;
  FL2_freeDCtx: function(dctx: Pointer): size_t cdecl;
  FL2_decompressDCtx: function(dctx: Pointer; dst: Pointer; dstCapacity: size_t;
    src: Pointer; srcSize: size_t): size_t cdecl;

  FL2_createCStream: function: Pointer cdecl;
  FL2_createCStreamMt: function(nbThreads: Cardinal; dualBuffer: Integer)
    : Pointer cdecl;
  FL2_freeCStream: procedure(fcs: Pointer)cdecl;
  FL2_initCStream: function(fcs: Pointer; compressionLevel: Integer)
    : size_t cdecl;
  FL2_compressStream: function(fcs: Pointer; output: PFL2_outBuffer;
    input: PFL2_inBuffer): size_t cdecl;

  FL2_createDStream: function: Pointer cdecl;
  FL2_createDStreamMt: function(nbThreads: Cardinal): Pointer cdecl;
  FL2_freeDStream: procedure(fds: Pointer)cdecl;
  FL2_initDStream: function(fds: Pointer): size_t cdecl;
  FL2_decompressStream: function(fds: Pointer; output: PFL2_outBuffer;
    input: PFL2_inBuffer): size_t cdecl;

  FL2_endStream: function(fcs: Pointer; output: PFL2_outBuffer): size_t cdecl;

  FL2_estimateDStreamSize: function(dictSize: size_t; nbThreads: Cardinal)
    : size_t cdecl;
  FL2_isError: function(code: size_t): Cardinal cdecl;
  FL2_CStream_setParameter: function(fcs: Pointer; param: FL2_cParameter;
    value: size_t): size_t cdecl;
  FL2_CStream_getParameter: function(fcs: Pointer; param: FL2_cParameter)
    : size_t cdecl;
  FL2_setDStreamMemoryLimitMt: procedure(fds: Pointer; limit: size_t)cdecl;

  DLLLoaded: boolean = False;

type
  TLZMACompressStream = class(TStream)
  private const
    FBufferSize = 65536;
  private
    FCtx: Pointer;
    FThreads, FLevel, FDictionary, FOverlap: Integer;
    FHighCompress: boolean;
    FOutput: TStream;
    FBuffer: array [0 .. FBufferSize - 1] of Byte;
    FInSize, FOutSize: Int64;
    FInitialized: boolean;
  public
    constructor Create(AOutput: TStream);
    destructor Destroy; override;
    function Write(const Buffer; Count: LongInt): LongInt; override;
    procedure Flush;
    property Threads: Integer read FThreads write FThreads;
    property Level: Integer read FLevel write FLevel;
    property Dictionary: Integer read FDictionary write FDictionary;
    property HighCompress: boolean read FHighCompress write FHighCompress;
    property Overlap: Integer read FOverlap write FOverlap;
    property InSize: Int64 read FInSize;
    property OutSize: Int64 read FOutSize;
  end;

  TLZMADecompressStream = class(TStream)
  private const
    FBufferSize = 65536;
  private
    FCtx: Pointer;
    FThreads, FDictionary: Integer;
    FDecMem: Int64;
    FInp: FL2_inBuffer;
    FInput: TStream;
    FBuffer: array [0 .. FBufferSize - 1] of Byte;
    FInSize, FOutSize: Int64;
    FInitialized: boolean;
  public
    constructor Create(AInput: TStream);
    destructor Destroy; override;
    function Read(var Buffer; Count: Integer): Integer; override;
    property Threads: Integer read FThreads write FThreads;
    property DecodeMemory: Int64 read FDecMem write FDecMem;
    property InSize: Int64 read FInSize;
    property OutSize: Int64 read FOutSize;
  end;

implementation

constructor TLZMACompressStream.Create(AOutput: TStream);
begin
  inherited Create;
  FThreads := ConvertToThreads('50p');
  FLevel := 6;
  FDictionary := 0;
  FHighCompress := False;
  FOverlap := -1;
  FOutput := AOutput;
  FInSize := 0;
  FOutSize := 0;
  FInitialized := False;
end;

destructor TLZMACompressStream.Destroy;
begin
  Flush;
  if FInitialized then
    FL2_freeCCtx(FCtx);
  inherited Destroy;
end;

function TLZMACompressStream.Write(const Buffer; Count: LongInt): LongInt;
var
  Inp: FL2_inBuffer;
  Oup: FL2_outBuffer;
  LDict: Integer;
begin
  Result := 0;
  if not FInitialized then
  begin
    if FThreads > 1 then
      FCtx := FL2_createCStreamMt(FThreads, 0)
    else
      FCtx := FL2_createCStream;
    FL2_CStream_setParameter(FCtx, FL2_cParameter.FL2_p_highCompression,
      Integer(FHighCompress));
    FL2_CStream_setParameter(FCtx,
      FL2_cParameter.FL2_p_compressionLevel, FLevel);
    if FDictionary > 0 then
      FL2_CStream_setParameter(FCtx, FL2_cParameter.FL2_p_dictionarySize,
        FDictionary);
    if FOverlap >= 0 then
      FL2_CStream_setParameter(FCtx, FL2_cParameter.FL2_p_overlapFraction,
        FOverlap);
    FL2_initCStream(FCtx, 0);
    FL2_CStream_setParameter(FCtx, FL2_cParameter.FL2_p_overlapFraction,
      FOverlap);
    LDict := FL2_CStream_getParameter(FCtx,
      FL2_cParameter.FL2_p_dictionarySize);
    FOutput.WriteBuffer(LDict, LDict.size);
    FInitialized := True;
  end;
  Inp.src := PByte(@Buffer);
  Inp.size := Count;
  Inp.pos := 0;
  Oup.dst := @FBuffer[0];
  Oup.size := FBufferSize;
  Oup.pos := 0;
  while Inp.pos < Inp.size do
  begin
    if not boolean(FL2_isError(FL2_compressStream(FCtx, @Oup, @Inp))) then
    begin
      FOutput.WriteBuffer(FBuffer[0], Oup.pos);
      Inc(FOutSize, Oup.pos);
      Oup.pos := 0;
    end;
  end;
  Result := Inp.pos;
  Inc(FInSize, Result);
end;

procedure TLZMACompressStream.Flush;
var
  Oup: FL2_outBuffer;
  Res: size_t;
begin
  if FInitialized then
  begin
    Oup.dst := @FBuffer[0];
    Oup.size := FBufferSize;
    Oup.pos := 0;
    repeat
      Res := FL2_endStream(FCtx, @Oup);
      FOutput.WriteBuffer(FBuffer[0], Oup.pos);
      Inc(FOutSize, Oup.pos);
      Oup.pos := 0;
    until Res = 0;
  end;
end;

constructor TLZMADecompressStream.Create(AInput: TStream);
begin
  inherited Create;
  FThreads := ConvertToThreads('50p');
  AInput.ReadBuffer(FDictionary, FDictionary.size);
  FDecMem := FL2_DEC_MEM;
  FInput := AInput;
  FInSize := 0;
  FOutSize := 0;
  FInitialized := False;
end;

destructor TLZMADecompressStream.Destroy;
begin
  if FInitialized then
    try
      FL2_freeDCtx(FCtx);
    except
    end;
  inherited Destroy;
end;

function TLZMADecompressStream.Read(var Buffer; Count: Integer): Integer;
var
  I: Integer;
  Oup: FL2_outBuffer;
begin
  Result := 0;
  if not FInitialized then
  begin
    for I := FThreads downto 1 do
    begin
      if FL2_estimateDStreamSize(FDictionary, I) <= FDecMem then
      begin
        FThreads := I;
        break;
      end;
    end;
    if FThreads > 1 then
    begin
      FCtx := FL2_createDStreamMt(FThreads);
      FL2_setDStreamMemoryLimitMt(FCtx, NativeInt.MaxValue);
    end
    else
      FCtx := FL2_createDStream;
    FL2_initDStream(FCtx);
    FillChar(FInp, SizeOf(FL2_inBuffer), 0);
    FInitialized := True;
  end;
  if FInp.pos = FInp.size then
  begin
    FInp.src := @FBuffer[0];
    FInp.size := FInput.Read(FBuffer[0], FBufferSize);
    FInp.pos := 0;
    if FInp.size = 0 then
      exit;
  end;
  Oup.dst := PByte(@Buffer);
  Oup.size := Count;
  Oup.pos := 0;
  while Oup.pos < Oup.size do
  begin
    if not boolean(FL2_isError(FL2_decompressStream(FCtx, @Oup, @FInp))) then
    begin
      if FInp.pos = FInp.size then
      begin
        FInp.src := @FBuffer[0];
        FInp.size := FInput.Read(FBuffer[0], FBufferSize);
        FInp.pos := 0;
        if FInp.size = 0 then
          break;
      end;
    end
    else
      break;
  end;
  Result := Oup.pos;
end;

var
  Lib: TLibImport;

procedure Init;
begin
  Lib := TLibImport.Create;
  Lib.LoadLib(ExpandPath(PluginsPath + 'fast-lzma2.dll', True));
  if Lib.Loaded then
  begin
    @FL2_compress := Lib.GetProcAddr('FL2_compress');
    @FL2_compressMt := Lib.GetProcAddr('FL2_compressMt');
    @FL2_decompress := Lib.GetProcAddr('FL2_decompress');
    @FL2_decompressMt := Lib.GetProcAddr('FL2_decompressMt');
    @FL2_createCCtx := Lib.GetProcAddr('FL2_createCCtx');
    @FL2_createCCtxMt := Lib.GetProcAddr('FL2_createCCtxMt');
    @FL2_freeCCtx := Lib.GetProcAddr('FL2_freeCCtx');
    @FL2_compressCCtx := Lib.GetProcAddr('FL2_compressCCtx');
    @FL2_createDCtx := Lib.GetProcAddr('FL2_createDCtx');
    @FL2_createDCtxMt := Lib.GetProcAddr('FL2_createDCtxMt');
    @FL2_freeDCtx := Lib.GetProcAddr('FL2_freeDCtx');
    @FL2_decompressDCtx := Lib.GetProcAddr('FL2_decompressDCtx');
    @FL2_createCStream := Lib.GetProcAddr('FL2_createCStream');
    @FL2_createCStreamMt := Lib.GetProcAddr('FL2_createCStreamMt');
    @FL2_freeCStream := Lib.GetProcAddr('FL2_freeCStream');
    @FL2_initCStream := Lib.GetProcAddr('FL2_initCStream');
    @FL2_compressStream := Lib.GetProcAddr('FL2_compressStream');
    @FL2_createDStream := Lib.GetProcAddr('FL2_createDStream');
    @FL2_createDStreamMt := Lib.GetProcAddr('FL2_createDStreamMt');
    @FL2_freeDStream := Lib.GetProcAddr('FL2_freeDStream');
    @FL2_initDStream := Lib.GetProcAddr('FL2_initDStream');
    @FL2_decompressStream := Lib.GetProcAddr('FL2_decompressStream');
    @FL2_endStream := Lib.GetProcAddr('FL2_endStream');
    @FL2_estimateDStreamSize := Lib.GetProcAddr('FL2_estimateDStreamSize');
    @FL2_isError := Lib.GetProcAddr('FL2_isError');
    @FL2_CStream_setParameter := Lib.GetProcAddr('FL2_CStream_setParameter');
    @FL2_CStream_getParameter := Lib.GetProcAddr('FL2_CStream_getParameter');
    @FL2_setDStreamMemoryLimitMt :=
      Lib.GetProcAddr('FL2_setDStreamMemoryLimitMt');
    DLLLoaded := Assigned(FL2_compress) and Assigned(FL2_decompress);
  end;
end;

procedure Deinit;
begin
  Lib.Free;
end;

initialization

Init;

finalization

Deinit;

end.
