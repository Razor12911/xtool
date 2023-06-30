unit LZ4;

interface
uses classes, sysutils, LZ4Lib
{$IFDEF YWRTL},YWTypes{$ENDIF};

const
  LZ4_VERSION_MAJOR = LZ4Lib.LZ4_VERSION_MAJOR;
  LZ4_VERSION_MINOR = LZ4Lib.LZ4_VERSION_MINOR;
  LZ4_VERSION_RELEASE = LZ4Lib.LZ4_VERSION_RELEASE;
  LZ4_VERSION_NUMBER = LZ4_VERSION_MAJOR*100*100+LZ4_VERSION_MINOR*100+
    LZ4_VERSION_RELEASE;
  LZ4_VERSION_STRING = '1.9.3';
  LZ4HC_CLEVEL_DEFAULT = LZ4Lib.LZ4HC_CLEVEL_DEFAULT;

type
  LZ4_Buffer = record
    src : Pointer;
    size, pos : NativeInt;
  end;
  LZ4_outBuffer = record
    dst : Pointer;
    size, pos : NativeInt;
  end;
  TCustomLZ4Stream = class(TStream)
  private
    FStream: TStream;
    FStreamStartPos: Int64;
    FStreamPos: Int64;
    _buf : Pointer;
    _bufPos : NativeUInt;
    total_in, total_out : NativeInt;
  public
    constructor Create(stream: TStream);
    destructor Destroy; override;
  end;

  TLZ4CompressStream=class(TCustomLZ4Stream)
  private
    pref : LZ4F_preferences_t;
    CCTX : PLZ4F_cctx;
    function GetCompressionRate: Single;
    procedure DoCompress(buf : Pointer);
  public
    constructor Create(dest: TStream; compressionLevel: Integer=2);
    function Read(var buffer; count: Longint): Longint; override;
    function Write(const buffer; count: Longint): Longint; override;
    function Read(Buffer: TBytes; Offset, Count: Longint): Longint; override;
    function Write(const Buffer: TBytes; Offset: Longint=0; Count:Longint =-1): Longint; override;
    destructor Destroy; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    property CompressionRate: Single read GetCompressionRate;
  end;

  TLZ4DecompressStream = class(TCustomLZ4Stream)
  private
    FOwnsStream: Boolean;
    _eof : boolean;
    _bufSize : NativeUInt;
    DCTX : PLZ4F_dctx;
  public
    constructor Create(source: TStream; OwnsStream: Boolean=false);
    destructor Destroy; override;
    function Read(var buffer; count: Longint): Longint; override;
    function Write(const buffer; count: Longint): Longint; override;
    function Read(Buffer: TBytes; Offset:LongInt=0;Count: Longint=-1): Longint; override;
    function Write(const Buffer: TBytes; Offset, Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  end;

function CompressData(source : Pointer;srcSize:NativeInt; dst:Pointer;
  dstCapacity:NativeInt; compressionLevel:integer=2):NativeInt; overload;
function DecompressData(Source:Pointer;srcSize:NativeInt;dst:Pointer;
  dstCapacity:NativeInt):NativeInt; overload;
function CompressData(source :TBytes;index:NativeInt=0;size:NativeInt=-1;
  compressionLevel:integer=2):TBytes; overload;
function DecompressData(Source:TBytes;index:NativeInt=0;Size:NativeInt=-1):TBytes; overload;
implementation
uses libc;
type
  Context = record
    class var _DCTX:PLZ4F_dctx;
    class var _CCTX:PLZ4F_cctx;
    class function GetDCTX:PLZ4F_dctx; inline; static;
    class procedure FreeDCTX(C : PLZ4F_dctx); inline; static;
    class function GetCCTX:PLZ4F_cctx; inline; static;
    class procedure FreeCCTX(C : PLZ4F_cctx); inline; static;
    class constructor Create;
    class destructor Destroy;
  end;
var
  [volatile]_state : Pointer;

function GetBuffer : Pointer; inline;
begin
  {$IFDEF YWRTL}Result:=BufferPool256K.GetBuffer{$ELSE}GetMem(Result,256*1024){$ENDIF};
end;
procedure FreeBuffer(b : Pointer); inline;
begin
  {$IFDEF YWRTL}BufferPool256K.FreeBuffer(b){$ELSE}Freemem(b){$ENDIF};
end;
function GetBuffer512:Pointer;inline;
begin
{$IFDEF YWRTL}
  Result := BufferPool512K.GetBuffer
{$ELSE}
  GetMem(Result,512*1024);
{$ENDIF}
end;
procedure FreeBuffer512(var b : Pointer); inline;
begin
{$IFDEF YWRTL}
  BufferPool512K.FreeBuffer(b)
{$ELSE}
  FreeMem(b);
{$ENDIF}
end;

function CompressData(source : Pointer;srcSize:NativeInt; dst:Pointer;
  dstCapacity:NativeInt; compressionLevel:integer=2):NativeInt; overload;
begin
  var P := LZ4F_INIT_PREFERENCES;
  P. compressionLevel :=compressionLevel;
  Result := LZ4F_compressFrame(dst,dstCapacity,source,srcSize,@P);
  if LZ4F_isError(Result)<>0 then raise Exception.Create(LZ4F_getErrorName(Result));
end;

function CompressData(source :TBytes;index:NativeInt=0;size:NativeInt=-1;compressionLevel:integer=2):TBytes;
  overload;
begin
  if size=-1 then size := Length(source);
  if size=0 then exit(nil);
  var P := LZ4F_INIT_PREFERENCES;
  P. compressionLevel :=compressionLevel;
  var s := LZ4F_compressFrameBound(size,@P);
  SetLength(Result,s);
  s := LZ4F_compressFrame(@Result[0],s,@source[index],Size,@P);
  if s>0 then SetLength(Result,s)
  else Result := nil;
end;

function DecompressData(Source:Pointer;srcSize:NativeInt;dst:Pointer;
  dstCapacity:NativeInt):NativeInt; overload;
var TX : PLZ4F_dctx;
    ds,ss,r : NativeUInt;
    s,d : PByte;
begin
  TX := Context.GetDCTX;
  Result := 0;
  try
    s := Source; d := dst;
    repeat
      ds := dstCapacity;
      ss := srcSize;
      r := LZ4F_decompress(TX,d,@ds,s,@ss,nil);
      if LZ4F_isError(r)<>0 then exit(-abs(r));
      inc(Result,ds);
      dec(dstCapacity,ds);
      dec(srcSize,ss);
      s := s+ss;
      d := d+ds;
    until (dstCapacity=0)or(srcSize=0);
  finally
    Context.FreeDCTX(TX);
  end;
end;
function DecompressData(Source:TBytes;index:NativeInt=0;Size:NativeInt=-1):TBytes; overload;
var s : Nativeint;
begin
  if Size=-1 then Size := Length(Source);
  SetLength(Result,Size*32);
  s := DecompressData(@Source[Index],Size,@Result[0],Length(Result));
  if s<0 then raise Exception.Create('LZ4 Decompress Error!');
  SetLength(Result,S);
end;
{ TLZ4Stream }

constructor TLZ4CompressStream.Create(dest: TStream; compressionLevel: Integer=2);
begin
  inherited Create(dest);
  pref := LZ4F_INIT_PREFERENCES;
  pref.compressionLevel :=compressionLevel;
  CCTX := Context.GetCCTX;
  FStream.Write(_buf^,LZ4F_compressBegin(CCTX,_buf,256*1024,@pref));
end;

destructor TLZ4CompressStream.Destroy;
begin
  if _BufPos>0 then begin
    var b := GetBuffer512;
    try
      var s := LZ4F_compressUpdate(CCTX,b,512*1024,_buf,_bufPos,nil);
      if LZ4F_isError(s)<>0 then raise Exception.Create(LZ4F_getErrorName(s));
      FStream.Write(b^,s);
    finally
      FreeBuffer512(b);
    end;
  end;
  FStream.Write(_buf^,LZ4F_compressEnd(CCTX,_buf,256*1024,nil));
  Context.FreeCCTX(CCTX);
  inherited;
end;

procedure TLZ4CompressStream.DoCompress(buf: Pointer);
begin
  var b := GetBuffer512;
  try
    var s := LZ4F_compressUpdate(CCTX,b,512*1024,buf,256*1024,nil);
    if LZ4F_isError(s)<>0 then raise Exception.Create(LZ4F_getErrorName(s));
    inc(total_out,s);
    FStream.Write(b^,s);
  finally
    FreeBuffer512(b);
  end;
end;

function TLZ4CompressStream.GetCompressionRate: Single;
begin
  if total_in = 0 then result := 0
  else result := (1.0 - (total_out / total_in)) * 100.0;
end;

function TLZ4CompressStream.Read(var buffer; count: Longint): Longint;
begin
  raise Exception.Create('Compress Stream is WriteOnly');
end;

function TLZ4CompressStream.Read(Buffer: TBytes; Offset, Count: Longint): Longint;
begin
  raise Exception.Create('Compress Stream is WriteOnly');
end;

function TLZ4CompressStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  if (offset = 0) and (origin = soCurrent) then
  begin
    result := total_in;
  end
  else Result := -1;
end;

function TLZ4CompressStream.Write(const Buffer: TBytes; Offset: Longint=0; Count:Longint =-1): Longint;
begin
  if Count=-1 then Count := Length(Buffer)-Offset;
  Result := Write(Buffer[Offset],Count);
end;

function TLZ4CompressStream.Write(const buffer; count: Longint): Longint;
var s : Cardinal;
    source : PByte;
begin
  Result := Count;
  inc(total_in,Count);
  source := @buffer;
  while count>0 do begin
    if (_bufpos>0)or(count<256*1024) then begin
      s := count; if s>256*1024-_bufpos then s:= 256*1024-_bufpos;
      {$IFDEF POSIX}
      move(source^,(PByte(_buf)+_bufPos)^,s);
      {$ELSE}
      memmove(PByte(_buf)+_bufPos,source,s);
      {$ENDIF}
      inc(_bufPos,s);
      dec(count,s);
      inc(source,s);
      if _bufPos=256*1024 then begin
        DoCompress(_buf);
        _bufPos := 0;
      end
    end else begin
      DoCompress(source);
      dec(count,256*1024);
      inc(source,256*1024);
    end;
  end;
end;

{ TCustomLZ4Stream }

constructor TCustomLZ4Stream.Create(stream: TStream);
begin
  inherited Create;
  FStream := stream;
  FStreamStartPos := Stream.Position;
  FStreamPos := FStreamStartPos;
  _buf := GetBuffer;
end;

{ TZDecompressionStream }

constructor TLZ4DecompressStream.Create(source: TStream; OwnsStream: Boolean=false);
begin
  inherited Create(source);
  FOwnsStream := OwnsStream;
  DCTX := Context.GetDCTX;
  _eof := false;
end;

destructor TLZ4DecompressStream.Destroy;
begin
  if FOwnsStream then
    FStream.Free;
  Context.FreeDCTX(DCTX);
  inherited;
end;

function TLZ4DecompressStream.Read(Buffer: TBytes; Offset:LongInt=0;Count: Longint=-1): Longint;
begin
  if Count=-1 then Count := Length(Buffer)-Offset;
  if Offset+Count>Length(Buffer) then SetLength(Buffer,Offset+Count);
  Result := Read(Buffer[Offset],Count);
end;

function TLZ4DecompressStream.Read(var buffer; count: Longint): Longint;
var dst : PByte;
    t,r,w : NativeInt;
begin
  Dst := @buffer;
  Result := 0;
  while (count>0) do
    if _bufPos<_bufSize then begin
      w := count; r := _bufSize-_bufPos;
      t := LZ4F_decompress(DCTX,Dst,@w,PByte(_buf)+_bufPos,@r,nil);
      if LZ4F_isError(t)<>0 then raise Exception.Create(LZ4F_getErrorName(t));
      inc(total_in,w);
      dec(count,w);
      inc(Dst,w);
      inc(Result,w);
      inc(_bufPos,r);
      inc(total_out,r);
    end else if _eof then break
    else begin
      _BufSize := FStream.Read(_Buf^,256*1024);
      _bufPos := 0;
      _eof := _BufSize<256*1024;
    end;
end;

function TLZ4DecompressStream.Seek(const Offset: Int64;
  Origin: TSeekOrigin): Int64;
begin
  Result := -1;
end;

function TLZ4DecompressStream.Write(const Buffer: TBytes; Offset,
  Count: Longint): Longint;
begin
  raise Exception.Create('Compress Stream is ReadOnly');
end;

function TLZ4DecompressStream.Write(const buffer; count: Longint): Longint;
begin
  raise Exception.Create('Compress Stream is ReadOnly');
end;

destructor TCustomLZ4Stream.Destroy;
begin
  FreeBuffer(_buf);
  inherited;
end;

{ Context }

class constructor Context.Create;
begin
  _DCTX := nil;
  _CCTX := nil;
end;

class destructor Context.Destroy;
begin
  if _DCTX<>nil then LZ4F_freeDecompressionContext(_DCTX);
  if _CCTX<>nil then LZ4F_freeCompressionContext(_CCTX);
end;

class procedure Context.FreeCCTX(C: PLZ4F_cctx);
begin
  C := atomicexchange(_CCTX,C);
  if C<>nil then LZ4F_freeCompressionContext(C);
end;

class procedure Context.FreeDCTX(C: PLZ4F_dctx);
begin
  LZ4F_resetDecompressionContext(C);
  C := atomicexchange(_DCTX,C);
  if C<>nil then LZ4F_freeDecompressionContext(C);
end;

class function Context.GetCCTX: PLZ4F_cctx;
begin
  Result := nil;
  Result := atomicexchange(_CCTX,Result);
  if Result = nil then LZ4F_createCompressionContext(@Result,LZ4F_VERSION);
end;

class function Context.GetDCTX: PLZ4F_dctx;
begin
  Result := nil;
  Result := atomicexchange(_DCTX,Result);
  if Result = nil then LZ4F_createDecompressionContext(@Result,LZ4F_VERSION);
end;

end.
