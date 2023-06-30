unit ZSTD;

interface
uses classes, sysutils, ZSTDLib
{$IFDEF YWRTL},YWTypes{$ENDIF}
;

const
  ZSTD_VERSION_MAJOR = ZSTDLib.ZSTD_VERSION_MAJOR;
  ZSTD_VERSION_MINOR = ZSTDLib.ZSTD_VERSION_MINOR;
  ZSTD_VERSION_RELEASE = ZSTDLib.ZSTD_VERSION_RELEASE;
  ZSTD_VERSION_NUMBER = ZSTD_VERSION_MAJOR*100*100+ZSTD_VERSION_MINOR*100+
    ZSTD_VERSION_RELEASE;
  ZSTD_VERSION_STRING = ZSTDLib.ZSTD_VERSION_STRING;
  ZSTD_CLEVEL_DEFAULT = ZSTDLib.ZSTD_CLEVEL_DEFAULT;

type
  TCustomZSTDStream = class(TStream)
  private
    FStream: TStream;
    FStreamStartPos: Int64;
    FStreamPos: Int64;
    FInBuffer :ZSTD_inBuffer;
    FOutBuffer : ZSTD_outBuffer;
    total_in, total_out : NativeInt;
  protected
    constructor Create(stream: TStream);
  end;

  TZSTDCompressStream=class(TCustomZSTDStream)
  private
    FCStream : ZSTD_CStream;
    flevel : integer;
    function GetCompressionRate: Single;
  public
    constructor Create(dest: TStream; compressionLevel: Integer=3);
    destructor Destroy; override;
    function Read(var buffer; count: Longint): Longint; override;
    function Write(const buffer; count: Longint): Longint; override;
    function Read(Buffer: TBytes; Offset, Count: Longint): Longint; override;
    function Write(const Buffer: TBytes; Offset: Longint=0;Count:LongInt=-1): Longint; override;

    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    property CompressionRate: Single read GetCompressionRate;
  end;

  TZSTDDecompressStream = class(TCustomZSTDStream)
  private
    FOwnsStream: Boolean;
    FDStream : ZSTD_DStream;
    _eof : boolean;
  public
    constructor Create(source: TStream; OwnsStream: Boolean=false);
    destructor Destroy; override;
    function Read(var buffer; count: Longint): Longint; override;
    function Write(const buffer; count: Longint): Longint; override;
    function Read(Buffer: TBytes; Offset, Count: Longint): Longint; override;
    function Write(const Buffer: TBytes; Offset, Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  end;

function CompressData(source : Pointer;srcSize:NativeInt; dst:Pointer;
  dstCapacity:NativeInt; compressionLevel:integer=3):NativeInt; overload;
function DecompressData(Source:Pointer;srcSize:NativeInt;dst:Pointer;
  dstCapacity:NativeInt):NativeInt; overload;
function CompressData(source :TBytes;index:NativeInt=0;size:NativeInt=-1;
  compressionLevel:integer=3):TBytes; overload;
function DecompressData(Source:TBytes;Size:NativeInt=-1):TBytes; overload;
implementation
type
  Context = record
    class var [volatile]_CCTX : ZSTD_CCtx;
    class var [volatile]_DCTX : ZSTD_DCtx;
    class var [volatile]_CStream : ZSTD_CStream;
    class var [volatile]_DStream : ZSTD_DStream;
    class function GetCCTX:ZSTD_CCTX; inline; static;
    class function GetDCTX:ZSTD_DCTX; inline; static;
    class procedure FreeCCTX(C : ZSTD_CCTX); inline; static;
    class procedure FreeDCTX(C : ZSTD_DCTX); inline; static;
    class function GetCStream(cl:integer):ZSTD_CStream; inline; static;
    class function GetDStream:ZSTD_DStream; inline; static;
    class procedure FreeCStream(C : ZSTD_CStream); inline; static;
    class procedure FreeDStream(C : ZSTD_DStream); inline; static;
    class constructor Create;
    class destructor Destroy;
  end;

function GetBuffer : Pointer; inline;
begin
  {$IFDEF YWRTL}Result:=BufferPool256K.GetBuffer{$ELSE}GetMem(Result,256*1024){$ENDIF}
end;

procedure FreeBuffer(var b : Pointer); inline;
begin
  {$IFDEF YWRTL}BufferPool256K.FreeBuffer(b){$ELSE}FreeMem(b){$ENDIF}
end;

function CompressData(source : Pointer;srcSize:NativeInt; dst:Pointer;
  dstCapacity:NativeInt; compressionLevel:integer=3):NativeInt; overload;
var TX : ZSTD_CCTX;
begin
  TX := Context.GetCCTX;
  try
    Result:= ZSTD_CompressCCTX(TX,dst,dstCapacity,source,srcSize,compressionLevel);
  finally
    Context.FreeCCTX(TX);
  end;
end;
function CompressData(source :TBytes;index:NativeInt=0;size:NativeInt=-1;compressionLevel:integer=3):TBytes;
  overload;
begin
  if size=-1 then size := Length(source);
  if size=0 then exit(nil);
  setlength(Result,ZSTD_COMPRESSBOUND(size));
  setLength(Result,CompressData(@Source[0],size,@Result[0],Length(Result),
    compressionLevel));
end;
function DecompressData(Source:Pointer;srcSize:NativeInt;dst:Pointer;
  dstCapacity:NativeInt):NativeInt; overload;
var TX : ZSTD_DCTX;
begin
  TX := Context.GetDCTX;
  try
    Result := ZSTD_decompressDCTX(TX,dst,dstCapacity,source,srcSize);
  finally
    Context.FreeDCTX(TX);
  end;
end;
function DecompressData(Source:TBytes;Size:NativeInt=-1):TBytes; overload;
begin
  if Size=-1 then Size := Length(Source);
  SetLength(Result,Size*32);
  SetLength(Result,DecompressData(@Source[0],Size,@Result[0],Length(Result)));
end;
{ TZSTDStream }

constructor TZSTDCompressStream.Create(dest: TStream; compressionLevel: Integer=3);
begin
  inherited Create(dest);
  flevel :=compressionLevel;
  FCStream := Context.GetCStream(flevel);
  FoutBuffer.size := 256*1024;
  FoutBuffer.dst := GetBuffer;
  FoutBuffer.pos := 0;
end;

destructor TZSTDCompressStream.Destroy;
begin
  ZSTD_flushStream(FCStream,FoutBuffer);
  FStream.Write(FoutBuffer.dst^,FoutBuffer.pos);
  FreeBuffer(FoutBuffer.dst);
  Context.FreeCStream(FCStream);
  inherited;
end;

function TZSTDCompressStream.GetCompressionRate: Single;
begin
  if total_in = 0 then result := 0
  else result := (1.0 - (total_out / total_in)) * 100.0;
end;

function TZSTDCompressStream.Read(var buffer; count: Longint): Longint;
begin
  raise Exception.Create('Compress Stream is WriteOnly');
end;

function TZSTDCompressStream.Read(Buffer: TBytes; Offset, Count: Longint): Longint;
begin
  raise Exception.Create('Compress Stream is WriteOnly');
end;

function TZSTDCompressStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  if (offset = 0) and (origin = soCurrent) then
  begin
    result := total_in;
  end
  else Result := -1;
end;

function TZSTDCompressStream.Write(const Buffer: TBytes; Offset: Longint=0;Count:LongInt=-1): Longint;
begin
  if Count=-1 then Count := Length(Buffer)-Offset;
  Result := Write(Buffer[Offset],Count);
end;

function TZSTDCompressStream.Write(const buffer; count: Longint): Longint;
begin
  Finbuffer.src := @buffer;
  Finbuffer.size := count;
  Finbuffer.pos := 0;
  while Finbuffer.pos<Finbuffer.size do begin
    ZSTD_compressStream2(FCStream,Foutbuffer,FinBuffer,ZSTD_e_continue);
    if FoutBuffer.pos>0 then begin
      FStream.Write(FoutBuffer.dst^,FoutBuffer.pos);
      total_out := total_out+FoutBuffer.pos;
      FoutBuffer.pos := 0;
    end;
  end;
  total_in := total_in+count;
  Result := Count;
end;

{ TCustomZSTDStream }

constructor TCustomZSTDStream.Create(stream: TStream);
begin
  inherited Create;
  FStream := stream;
  FStreamStartPos := Stream.Position;
  FStreamPos := FStreamStartPos;
end;

{ TZDecompressionStream }

constructor TZSTDDecompressStream.Create(source: TStream; OwnsStream: Boolean=false);
begin
  inherited Create(source);
  FOwnsStream := OwnsStream;
  FDStream := Context.GetDStream;
  FinBuffer.size := 256*1024; //128K
  FinBuffer.src := GetBuffer;
  total_in := FStream.Read(FinBuffer.src^,FinBuffer.size);
  _eof := total_in<FinBuffer.size;
  FinBuffer.size := total_in;
end;

destructor TZSTDDecompressStream.Destroy;
begin
  FreeBuffer(FinBuffer.src);
  Context.FreeDStream(FDStream);
  if FOwnsStream then
    FStream.Free;
  inherited;
end;

function TZSTDDecompressStream.Read(Buffer: TBytes; Offset,
  Count: Longint): Longint;
begin
  Result := Read(Buffer[Offset],Count);
end;

function TZSTDDecompressStream.Read(var buffer; count: Longint): Longint;
var a,b : NativeInt;
begin
  FoutBuffer.dst := @Buffer;
  FoutBuffer.size := Count;
  FoutBuffer.pos := 0;
  repeat
    b := ZSTD_decompressStream(FDStream,FoutBuffer,FinBuffer);
    if ZSTD_iserror(b)<>0 then raise Exception.Create('ZSTD Error '+b.toString);
    if (FInBuffer.pos=FInBuffer.size) then begin
      if _eof then break;
      a := FStream.Read(FInBuffer.src^,FInBuffer.size);
      total_in := total_in+a;
      _eof := a<FInBuffer.size;
      FInBuffer.size:= a;
      FInBuffer.pos := 0;
    end;
  until FoutBuffer.pos=FoutBuffer.size;
  total_out:=total_out+FoutBuffer.pos;
  Result := FOutBuffer.pos;
end;

function TZSTDDecompressStream.Seek(const Offset: Int64;
  Origin: TSeekOrigin): Int64;
begin
  Result := -1;
end;

function TZSTDDecompressStream.Write(const Buffer: TBytes; Offset,
  Count: Longint): Longint;
begin
  raise Exception.Create('Compress Stream is ReadOnly');
end;

function TZSTDDecompressStream.Write(const buffer; count: Longint): Longint;
begin
  raise Exception.Create('Compress Stream is ReadOnly');
end;

{ Contex }

class constructor Context.Create;
begin
  _CCtx := nil;
  _DCTX := nil;
  _CStream := nil;
  _DStream := nil;
end;

class destructor Context.Destroy;
begin
  if _CCTX<>nil then ZSTD_FreeCCTX(_CCtx);
  if _DCTX<>nil then ZSTD_FreeDCTX(_DCTX);
  if _CStream<>nil then ZSTD_FreeCStream(_CStream);
  if _DStream<>nil then ZSTD_FreeDStream(_DStream);
end;

class procedure Context.FreeCCTX(C: ZSTD_CCTX);
begin
  ZSTD_CCTX_reset(C,ZSTD_reset_session_only);
  C := atomicExchange(_CCTX,C);
  if C<>nil then ZSTD_FreeCCTX(c);
end;

class procedure Context.FreeCStream(C: ZSTD_CStream);
begin
  C := atomicExchange(_CStream,C);
  if C<>nil then ZSTD_FreeCStream(c);
end;

class procedure Context.FreeDCTX(C: ZSTD_DCTX);
begin
  ZSTD_CCTX_reset(C,ZSTD_reset_session_only);
  C := atomicExchange(_DCTX,C);
  if C<>nil then ZSTD_FreeDCTX(c);
end;

class procedure Context.FreeDStream(C: ZSTD_DStream);
begin
  ZSTD_initDStream(C);
  C := atomicExchange(_DStream,C);
  if C<>nil then ZSTD_FreeDStream(c);
end;

class function Context.GetCCTX: ZSTD_CCTX;
begin
  Result := nil;
  Result := atomicExchange(_CCTX,Result);
  if Result=nil then Result := ZSTD_CreateCCTX;
end;

class function Context.GetCStream(cl: integer): ZSTD_CStream;
begin
  Result := nil;
  Result := atomicExchange(_CStream,Result);
  if Result = nil then Result := ZSTD_createCStream;
  ZSTD_initCStream(Result,cl);
end;

class function Context.GetDCTX: ZSTD_DCTX;
begin
  Result := nil;
  Result := atomicExchange(_DCTX,Result);
  if Result=nil then Result := ZSTD_CreateDCTX;
end;

class function Context.GetDStream: ZSTD_DStream;
begin
  Result := nil;
  Result := atomicExchange(_DStream,Result);
  if Result=nil then Result := ZSTD_CreateDStream;
end;

end.
