unit FLZMA2DLL;

interface

uses
  MemoryModule,
  WinAPI.Windows,
  System.SysUtils, System.Classes, System.Types;

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
  FL2_isError: function(code: size_t): Cardinal cdecl;
  DLLLoaded: boolean = False;

type
  TLZMACRec = record
    Threads: Integer;
    Level: Integer;
    procedure Parse(S: String);
  end;

  TLZMADRec = record
    Threads: Integer;
    procedure Parse(S: String);
  end;

  TLZMACompressStream = class(TStream)
  private const
    FBufferSize = 65536;
  private
    FCtx: Pointer;
    FProp: TLZMACRec;
    FOutput: TStream;
    FBuffer: array [0 .. FBufferSize - 1] of Byte;
  public
    constructor Create(AOutput: TStream; AConfig: String = 't50p');
    destructor Destroy; override;
    function Write(const Buffer; Count: LongInt): LongInt; override;
  end;

  TLZMADecompressStream = class(TStream)
  private const
    FBufferSize = 65536;
  private
    FCtx: Pointer;
    FProp: TLZMADRec;
    FInp: FL2_inBuffer;
    FInput: TStream;
    FBuffer: array [0 .. FBufferSize - 1] of Byte;
  public
    constructor Create(AInput: TStream; AConfig: String = '');
    destructor Destroy; override;
    function Read(var Buffer; Count: Integer): Integer; override;
  end;

implementation

uses
  Utils;

var
  DLLStream: TResourceStream;
  DLLHandle: TMemoryModule;

procedure TLZMACRec.Parse(S: string);
var
  List: TStringDynArray;
  I, J: Integer;
begin
  Threads := 1;
  Level := 5;
  List := DecodeStr(S, ':');
  for I := Low(List) to High(List) do
  begin
    if List[I].StartsWith('t', True) then
      Threads := ConvertToThreads(List[I].Substring(1));
    if List[I].StartsWith('l', True) then
      Level := List[I].Substring(1).ToInteger;
  end;
end;

procedure TLZMADRec.Parse(S: string);
var
  List: TStringDynArray;
  I: Integer;
begin
  Threads := 1;
  List := DecodeStr(S, ':');
  for I := Low(List) to High(List) do
  begin
    if List[I].StartsWith('t', True) then
      Threads := ConvertToThreads(List[I].Substring(1));
  end;
end;

constructor TLZMACompressStream.Create(AOutput: TStream; AConfig: String);
begin
  inherited Create;
  FProp.Parse(AConfig);
  FOutput := AOutput;
  if FProp.Threads > 1 then
    FCtx := FL2_createCStreamMt(FProp.Threads, 0)
  else
    FCtx := FL2_createCStream;
  FL2_initCStream(FCtx, FProp.Level);
end;

destructor TLZMACompressStream.Destroy;
var
  Oup: FL2_outBuffer;
  Res: size_t;
begin
  Oup.dst := @FBuffer[0];
  Oup.size := FBufferSize;
  Oup.pos := 0;
  repeat
    Res := FL2_endStream(FCtx, @Oup);
    FOutput.WriteBuffer(FBuffer[0], Oup.pos);
    Oup.pos := 0;
  until Res = 0;
  FL2_freeCCtx(FCtx);
  inherited Destroy;
end;

function TLZMACompressStream.Write(const Buffer; Count: LongInt): LongInt;
var
  Inp: FL2_inBuffer;
  Oup: FL2_outBuffer;
begin
  Result := 0;
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
      Oup.pos := 0;
    end;
  end;
  Result := Inp.pos;
end;

constructor TLZMADecompressStream.Create(AInput: TStream; AConfig: String);
begin
  inherited Create;
  FProp.Parse(AConfig);
  FInput := AInput;
  if FProp.Threads > 1 then
    FCtx := FL2_createDStreamMt(FProp.Threads)
  else
    FCtx := FL2_createDStream;
  FL2_initDStream(FCtx);
  FillChar(FInp, SizeOf(FL2_inBuffer), 0);
end;

destructor TLZMADecompressStream.Destroy;
begin
  FL2_freeDCtx(FCtx);
  inherited Destroy;
end;

function TLZMADecompressStream.Read(var Buffer; Count: Integer): Integer;
var
  Oup: FL2_outBuffer;
begin
  Result := 0;
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

procedure Init;
begin
  DLLStream := TResourceStream.Create(HInstance, 'fast_lzma2', RT_RCDATA);
  DLLHandle := MemoryLoadLibary(DLLStream.Memory);
  if Assigned(DLLHandle) then
  begin
    @FL2_compress := MemoryGetProcAddress(DLLHandle, 'FL2_compress');
    @FL2_compressMt := MemoryGetProcAddress(DLLHandle, 'FL2_compressMt');
    @FL2_decompress := MemoryGetProcAddress(DLLHandle, 'FL2_decompress');
    @FL2_decompressMt := MemoryGetProcAddress(DLLHandle, 'FL2_decompressMt');
    @FL2_createCCtx := MemoryGetProcAddress(DLLHandle, 'FL2_createCCtx');
    @FL2_createCCtxMt := MemoryGetProcAddress(DLLHandle, 'FL2_createCCtxMt');
    @FL2_freeCCtx := MemoryGetProcAddress(DLLHandle, 'FL2_freeCCtx');
    @FL2_compressCCtx := MemoryGetProcAddress(DLLHandle, 'FL2_compressCCtx');
    @FL2_createDCtx := MemoryGetProcAddress(DLLHandle, 'FL2_createDCtx');
    @FL2_createDCtxMt := MemoryGetProcAddress(DLLHandle, 'FL2_createDCtxMt');
    @FL2_freeDCtx := MemoryGetProcAddress(DLLHandle, 'FL2_freeDCtx');
    @FL2_decompressDCtx := MemoryGetProcAddress(DLLHandle,
      'FL2_decompressDCtx');
    @FL2_createCStream := MemoryGetProcAddress(DLLHandle, 'FL2_createCStream');
    @FL2_createCStreamMt := MemoryGetProcAddress(DLLHandle,
      'FL2_createCStreamMt');
    @FL2_freeCStream := MemoryGetProcAddress(DLLHandle, 'FL2_freeCStream');
    @FL2_initCStream := MemoryGetProcAddress(DLLHandle, 'FL2_initCStream');
    @FL2_compressStream := MemoryGetProcAddress(DLLHandle,
      'FL2_compressStream');
    @FL2_createDStream := MemoryGetProcAddress(DLLHandle, 'FL2_createDStream');
    @FL2_createDStreamMt := MemoryGetProcAddress(DLLHandle,
      'FL2_createDStreamMt');
    @FL2_freeDStream := MemoryGetProcAddress(DLLHandle, 'FL2_freeDStream');
    @FL2_initDStream := MemoryGetProcAddress(DLLHandle, 'FL2_initDStream');
    @FL2_decompressStream := MemoryGetProcAddress(DLLHandle,
      'FL2_decompressStream');
    @FL2_endStream := MemoryGetProcAddress(DLLHandle, 'FL2_endStream');
    @FL2_isError := MemoryGetProcAddress(DLLHandle, 'FL2_isError');
    DLLLoaded := Assigned(FL2_compress) and Assigned(FL2_decompress);
  end
  else
    DLLLoaded := False;
end;

procedure Deinit;
begin
  if not DLLLoaded then
    exit;
  MemoryFreeLibrary(DLLHandle);
end;

initialization

Init;

finalization

Deinit;

end.
