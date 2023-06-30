unit PrecompUtils;

interface

uses
  InitCode,
  Utils, Threading, XXHASHLIB,
  WinAPI.Windows,
  System.SysUtils, System.Classes, System.StrUtils, System.Types, System.Math,
  System.Generics.Defaults, System.Generics.Collections;

resourcestring
  SPrecompError1 = 'Method ''%s'' not found';
  SPrecompError2 = 'Failed to initialise ''%s''';
  SPrecompError3 = 'Error in the method ''%s''';
  SPrecompSep1 = '+';
  SPrecompSep2 = ':';
  SPrecompSep3 = ',';
  SPrecompSep4 = '/';
  SPrecompSep5 = '\';

const
  SuccessStatus = 4;

  DEFAULT_STREAM = 0;
  EXTENDED_STREAM = 1;
  NESTED_STREAM = 2;
  DUPLICATED_STREAM = 4;

  PRECOMP_FCOUNT = 128;

type
  PPrecompStr = ^TPrecompStr;

  TPrecompStr = array [0 .. 255] of Char;

  TStreamStatus = (None, Invalid, Predicted, Database);

  PDepthInfo = ^TDepthInfo;

  TDepthInfo = packed record
    Codec: array [0 .. 31] of Char;
    OldSize: Integer;
    NewSize: Integer;
  end;

  PEncodeSI = ^TEncodeSI;

  TEncodeSI = record
    ActualPosition: NativeInt;
    StorePosition: NativeInt;
    OldSize, NewSize, Thread: Integer;
    ExtPosition: NativeInt;
    ExtSize, ExtThread: Integer;
    Resource: Integer;
    Codec: Byte;
    Scan2: Boolean;
    Option: Integer;
    Checksum: XXH128_hash_t;
    Status: TStreamStatus;
    DepthInfo: TDepthInfo;
  end;

  PFutureSI = ^TFutureSI;

  TFutureSI = record
    Position: Int64;
    OldSize, NewSize: Integer;
    Resource: Integer;
    Codec: Byte;
    Scan2: Boolean;
    Option: Integer;
    Status: TStreamStatus;
    DepthInfo: TDepthInfo;
  end;

  PStreamHeader = ^TStreamHeader;

  TStreamHeader = packed record
    Kind: Byte;
    OldSize, NewSize: Integer;
    Resource: Integer;
    Codec: Byte;
    Option: Integer;
  end;

  PStrInfo1 = ^_StrInfo1;

  _StrInfo1 = packed record
    Position: Int64;
    OldSize, NewSize: Integer;
    Resource: Integer;
    Status: TStreamStatus;
    Option: Integer;
  end;

  PStrInfo2 = ^_StrInfo2;

  _StrInfo2 = packed record
    OldSize, NewSize: Integer;
    Resource: Integer;
    Status: TStreamStatus;
    Option: Integer;
  end;

  PStrInfo3 = ^_StrInfo3;

  _StrInfo3 = packed record
    OldSize, NewSize, ExtSize: Integer;
    Resource: Integer;
    Option: Integer;
  end;

  PExecOutput = ^_ExecOutput;

  _ExecOutput = reference to procedure(Instance: Integer; const Buffer: Pointer;
    Size: Integer)cdecl;

  PPrecompFuncs = ^_PrecompFuncs;

  _PrecompFuncs = record
    Allocator: function(Index: Integer; Size: Integer): Pointer cdecl;
    GetCodec: function(Cmd: PChar; Index: Integer; Param: Boolean)
      : TPrecompStr cdecl;
    GetParam: function(Cmd: PChar; Index: Integer; Param: PChar)
      : TPrecompStr cdecl;
    GetDepthInfo: function(Index: Integer): TDepthInfo cdecl;
    Compress: function(Codec: PChar; InBuff: Pointer; InSize: Integer;
      OutBuff: Pointer; OutSize: Integer; DictBuff: Pointer; DictSize: Integer)
      : Integer cdecl; // 5
    Decompress: function(Codec: PChar; InBuff: Pointer; InSize: Integer;
      OutBuff: Pointer; OutSize: Integer; DictBuff: Pointer; DictSize: Integer)
      : Integer cdecl;
    Encrypt: function(Codec: PChar; InBuff: Pointer; InSize: Integer;
      KeyBuff: Pointer; KeySize: Integer): Boolean cdecl;
    Decrypt: function(Codec: PChar; InBuff: Pointer; InSize: Integer;
      KeyBuff: Pointer; KeySize: Integer): Boolean cdecl;
    Hash: function(Codec: PChar; InBuff: Pointer; InSize: Integer;
      HashBuff: Pointer; HashSize: Integer): Boolean cdecl;
    EncodePatch: function(OldBuff: Pointer; OldSize: Integer; NewBuff: Pointer;
      NewSize: Integer; PatchBuff: Pointer; PatchSize: Integer): Integer cdecl;
    // 10
    DecodePatch: function(PatchBuff: Pointer; PatchSize: Integer;
      OldBuff: Pointer; OldSize: Integer; NewBuff: Pointer; NewSize: Integer)
      : Integer cdecl;
    AddResource: function(FileName: PChar): Integer cdecl;
    GetResource: function(ID: Integer; Data: Pointer; Size: PInteger)
      : Boolean cdecl;
    SearchBinary: function(SrcMem: Pointer; SrcPos, SrcSize: NativeInt;
      SearchMem: Pointer; SearchSize: NativeInt; ResultPos: PNativeInt)
      : Boolean cdecl;
    SwapBinary: procedure(Source, Dest: Pointer; Size: NativeInt)cdecl; // 15
    Swap16: function(Value: ShortInt): ShortInt cdecl;
    Swap32: function(Value: Integer): Integer cdecl;
    Swap64: function(Value: Int64): Int64 cdecl;
    FileOpen: function(FileName: PChar; Create: Boolean): THandle cdecl;
    FileClose: procedure(Handle: THandle)cdecl; // 20
    FileSeek: function(Handle: THandle; Offset: Int64; Origin: Integer)
      : Int64 cdecl;
    FileSize: function(Handle: THandle): Int64 cdecl;
    FileRead: function(Handle: THandle; Buffer: Pointer; Count: Integer)
      : Integer cdecl;
    FileWrite: function(Handle: THandle; Buffer: Pointer; Count: Integer)
      : Integer cdecl;
    IniRead: function(Section, Key, Default, FileName: PChar)
      : TPrecompStr cdecl;
    // 25
    IniWrite: procedure(Section, Key, Value, FileName: PChar)cdecl;
    Exec: function(Executable, CommandLine, WorkDir: PChar): Boolean cdecl;
    ExecStdin: function(Executable, CommandLine, WorkDir: PChar;
      InBuff: Pointer; InSize: Integer): Boolean cdecl;
    ExecStdout: function(Instance: Integer;
      Executable, CommandLine, WorkDir: PChar; Output: _ExecOutput)
      : Boolean cdecl;
    ExecStdio: function(Instance: Integer;
      Executable, CommandLine, WorkDir: PChar; InBuff: Pointer; InSize: Integer;
      Output: _ExecOutput): Boolean cdecl; // 30
    ExecStdioSync: function(Instance: Integer;
      Executable, CommandLine, WorkDir: PChar; InBuff: Pointer; InSize: Integer;
      Output: _ExecOutput): Boolean cdecl;
    GetDepthCodec: function(Cmd: PChar): TPrecompStr cdecl;
    ReadFuture: function(Index: Integer; Position: NativeInt; Buffer: Pointer;
      Count: Integer): Integer cdecl;
    LogScan1: procedure(Codec: PChar; Position: Int64;
      InSize, OutSize: Integer)cdecl;
    LogScan2: procedure(Codec: PChar; InSize, OutSize: Integer)cdecl; // 35
    LogProcess: procedure(Codec, Method: PChar; Size1, Size2, Size3: Integer;
      Status: Boolean)cdecl;
    LogRestore: procedure(Codec, Method: PChar; Size1, Size2, Size3: Integer;
      Status: Boolean)cdecl;
    LogPatch1: procedure(OldSize, NewSize, PatchSize: Integer;
      Status: Boolean)cdecl;
    LogPatch2: procedure(OldSize, NewSize, PatchSize: Integer;
      Status: Boolean)cdecl;
    AcceptPatch: function(OldSize, NewSize, PatchSize: Integer): Boolean cdecl;
    // 40
    Transfer: procedure(Instance: Integer; Codec: PChar)cdecl;
    Storage: function(Instance: Integer; Size: PInteger): Pointer cdecl;
    AddResourceEx: function(Data: Pointer; Size: Integer): Integer cdecl;
    Reserved: array [0 .. (PRECOMP_FCOUNT - 1) - 43] of Pointer;
  end;

  _PrecompOutput = procedure(Instance: Integer; const Buffer: Pointer;
    Size: Integer)cdecl;
  _PrecompAdd = procedure(Instance: Integer; Info: PStrInfo1; Codec: PChar;
    DepthInfo: PDepthInfo)cdecl;

  _PrecompInit = function(Command: PChar; Count: Integer;
    Funcs: PPrecompFuncs): Boolean;
  _PrecompFree = procedure(Funcs: PPrecompFuncs);
  _PrecompParse = function(Command: PChar; Option: PInteger;
    Funcs: PPrecompFuncs): Boolean;
  _PrecompScan1 = procedure(Instance, Depth: Integer; Input: Pointer;
    Size, SizeEx: NativeInt; Output: _PrecompOutput; Add: _PrecompAdd;
    Funcs: PPrecompFuncs);
  _PrecompScan2 = function(Instance, Depth: Integer; Input: Pointer;
    Size: NativeInt; StreamInfo: PStrInfo2; Offset: PInteger;
    Output: _PrecompOutput; Funcs: PPrecompFuncs): Boolean;
  _PrecompProcess = function(Instance, Depth: Integer;
    OldInput, NewInput: Pointer; StreamInfo: PStrInfo2; Output: _PrecompOutput;
    Funcs: PPrecompFuncs): Boolean;
  _PrecompRestore = function(Instance, Depth: Integer; Input, InputExt: Pointer;
    StreamInfo: _StrInfo3; Output: _PrecompOutput;
    Funcs: PPrecompFuncs): Boolean;

  TPrecompressor = record
    Names: TArray<String>;
    Initialised: Boolean;
    Init: _PrecompInit;
    Free: _PrecompFree;
    Parse: _PrecompParse;
    Scan1: _PrecompScan1;
    Scan2: _PrecompScan2;
    Process: _PrecompProcess;
    Restore: _PrecompRestore;
  end;

  TEncodeSIComparer = class(TComparer<TEncodeSI>)
  public
    function Compare(const Left, Right: TEncodeSI): Integer; override;
  end;

  TFutureSIComparer = class(TComparer<TFutureSI>)
  public
    function Compare(const Left, Right: TFutureSI): Integer; override;
  end;

  PDatabase = ^TDatabase;

  TDatabase = packed record
    Size: Integer;
    Codec: Byte;
    Option: Integer;
    Checksum: XXH128_hash_t;
    Status: TStreamStatus;
  end;

  PDuplicate1 = ^TDuplicate1;

  TDuplicate1 = packed record
    Size: Integer;
    Checksum: XXH128_hash_t;
    Index: Integer;
    Count: Integer;
  end;

  PDuplicate2 = ^TDuplicate2;

  TDuplicate2 = packed record
    Index: Integer;
    Count: Integer;
  end;

  TPrecompVMStream = class(TStream)
  private const
    FSuffix1 = '-vm.tmp';
    FSuffix2 = '_mapped.io';
  protected
    procedure SetSize(const NewSize: Int64); override;
    procedure SetSize(NewSize: Longint); override;
  private
    FInitialised, FDone: Boolean;
    FStream: TStream;
    FFilename: String;
    procedure Initialise;
  public
    constructor Create;
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    procedure Done;
    property FileName: String read FFilename;
  end;

  PResData = ^TResData;

  TResData = record
    Name: String;
    Data: Pointer;
    Size: Integer;
  end;

procedure AddMethod(Method: String);
procedure ClearMethods;

function RegisterResources(Cmd: String): Integer;
procedure FreeResources;

function PrecompGetCodec(Cmd: PChar; Index: Integer; WithParams: Boolean)
  : TPrecompStr cdecl;
function PrecompGetParam(Cmd: PChar; Index: Integer; Param: PChar)
  : TPrecompStr cdecl;
function PrecompGetDepthCodec(Cmd: PChar): TPrecompStr cdecl;
function PrecompCompress(Codec: PChar; InBuff: Pointer; InSize: Integer;
  OutBuff: Pointer; OutSize: Integer; DictBuff: Pointer; DictSize: Integer)
  : Integer cdecl;
function PrecompDecompress(Codec: PChar; InBuff: Pointer; InSize: Integer;
  OutBuff: Pointer; OutSize: Integer; DictBuff: Pointer; DictSize: Integer)
  : Integer cdecl;
function PrecompEncrypt(Codec: PChar; InBuff: Pointer; InSize: Integer;
  KeyBuff: Pointer; KeySize: Integer): Boolean cdecl;
function PrecompDecrypt(Codec: PChar; InBuff: Pointer; InSize: Integer;
  KeyBuff: Pointer; KeySize: Integer): Boolean cdecl;
function PrecompHash(Codec: PChar; InBuff: Pointer; InSize: Integer;
  HashBuff: Pointer; HashSize: Integer): Boolean cdecl;
function PrecompEncodePatch(OldBuff: Pointer; OldSize: Integer;
  NewBuff: Pointer; NewSize: Integer; PatchBuff: Pointer; PatchSize: Integer)
  : Integer cdecl;
function PrecompDecodePatch(PatchBuff: Pointer; PatchSize: Integer;
  OldBuff: Pointer; OldSize: Integer; NewBuff: Pointer; NewSize: Integer)
  : Integer cdecl;
function PrecompAddResource(FileName: PChar): Integer cdecl;
function PrecompGetResource(Index: Integer; Data: Pointer; Size: PInteger)
  : Boolean cdecl;
function PrecompSearchBinary(SrcMem: Pointer; SrcPos, SrcSize: NativeInt;
  SearchMem: Pointer; SearchSize: NativeInt; ResultPos: PNativeInt)
  : Boolean cdecl;
procedure PrecompSwapBinary(Source, Dest: Pointer; Size: NativeInt)cdecl;
function PrecompSwap16(Int: Int16): Int16 cdecl;
function PrecompSwap32(Int: Int32): Int32 cdecl;
function PrecompSwap64(Int: Int64): Int64 cdecl;
function PrecompFileOpen(FileName: PChar; Create: Boolean): THandle cdecl;
procedure PrecompFileClose(Handle: THandle)cdecl;
function PrecompFileSeek(Handle: THandle; Offset: Int64; Origin: Integer)
  : Int64 cdecl;
function PrecompFileSize(Handle: THandle): Int64 cdecl;
function PrecompFileRead(Handle: THandle; Buffer: Pointer; Count: Integer)
  : Integer cdecl;
function PrecompFileWrite(Handle: THandle; Buffer: Pointer; Count: Integer)
  : Integer cdecl;
function PrecompIniRead(Section, Key, Default, FileName: PChar)
  : TPrecompStr cdecl;
procedure PrecompIniWrite(Section, Key, Value, FileName: PChar)cdecl;
function PrecompExec(Executable, CommandLine, WorkDir: PChar): Boolean cdecl;
function PrecompExecStdin(Executable, CommandLine, WorkDir: PChar;
  InBuff: Pointer; InSize: Integer): Boolean cdecl;
function PrecompExecStdout(Instance: Integer;
  Executable, CommandLine, WorkDir: PChar; Output: _ExecOutput): Boolean cdecl;
function PrecompExecStdio(Instance: Integer;
  Executable, CommandLine, WorkDir: PChar; InBuff: Pointer; InSize: Integer;
  Output: _ExecOutput): Boolean cdecl;
function PrecompExecStdioSync(Instance: Integer;
  Executable, CommandLine, WorkDir: PChar; InBuff: Pointer; InSize: Integer;
  Output: _ExecOutput): Boolean cdecl;
function PrecompAcceptPatch(OldSize, NewSize, PatchSize: Integer)
  : Boolean cdecl;

var
  PrecompFunctions: _PrecompFuncs;
  DIFF_TOLERANCE: Single = 0.05;
  OPTIMISE_DEC: Boolean = False;
  EncodeSICmp: TEncodeSIComparer;
  FutureSICmp: TFutureSIComparer;
  StockMethods, ExternalMethods: TStringList;
  Resources: array of TResData;

implementation

uses
  ZLibDLL, LZ4DLL, LZODLL, ZSTDDLL, OodleDLL, XDeltaDLL,
  SynCommons, SynCrypto;

function TEncodeSIComparer.Compare(const Left, Right: TEncodeSI): Integer;
begin
  Result := Integer(CompareValue(Left.ActualPosition, Right.ActualPosition));
end;

function TFutureSIComparer.Compare(const Left, Right: TFutureSI): Integer;
begin
  Result := Integer(CompareValue(Left.Position, Right.Position));
end;

procedure AddMethod(Method: String);
begin
  if (StockMethods.IndexOf(Method) < 0) and (ExternalMethods.IndexOf(Method) < 0)
  then
    ExternalMethods.Add(Method);
end;

procedure ClearMethods;
begin
  ExternalMethods.Clear;
end;

function ResourceExists(FileName: String): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := Low(Resources) to High(Resources) do
  begin
    Result := SameText(Resources[I].Name, ExtractFileName(FileName));
    if Result then
      break;
  end;
end;

function RegisterResources(Cmd: String): Integer;
var
  List0, List1, List2: System.Types.TStringDynArray;
  I, J: Integer;
begin
  Result := -1;
  if Cmd <> '' then
  begin
    List0 := DecodeStr(Cmd, SPrecompSep4);
    List1 := DecodeStr(List0[0], SPrecompSep1);
    for I := Low(List1) to High(List1) do
    begin
      List2 := DecodeStr(List1[I], SPrecompSep2);
      for J := Succ(Low(List2)) to High(List2) do
      begin
        if FileExists(PluginsPath + List2[J]) then
        begin
          Result := PrecompAddResource(PChar(List2[J]));
          break;
        end;
      end;
    end;
  end;
end;

procedure FreeResources;
var
  I: Integer;
begin
  for I := Low(Resources) to High(Resources) do
    FreeMem(Resources[I].Data);
  SetLength(Resources, 0);
end;

constructor TPrecompVMStream.Create;
begin
  inherited Create;
  FInitialised := False;
  FDone := False;
end;

destructor TPrecompVMStream.Destroy;
begin
  if FInitialised then
  begin
    if not FDone then
      FStream.Free;
    DeleteFile(FFilename);
  end;
  inherited Destroy;
end;

procedure TPrecompVMStream.Initialise;
begin
  if FInitialised then
    exit;
  FFilename := LowerCase(ChangeFileExt(ExtractFileName(Utils.GetModuleName),
    FSuffix1));
  if FileExists(FFilename) then
    DeleteFile(FFilename);
{$IFDEF CPU32BITS}
  FStream := TFileStream.Create(FFilename, fmCreate);
{$ELSE}
  FStream := TSharedMemoryStream.Create
    (LowerCase(ChangeFileExt(ExtractFileName(Utils.GetModuleName),
    '_' + Random($7FFFFFFF).ToHexString + FSuffix2)), FFilename);
{$ENDIF}
  FInitialised := True;
end;

procedure TPrecompVMStream.SetSize(const NewSize: Int64);
begin
  if not FInitialised then
    if NewSize > 0 then
      Initialise
    else
      exit;
  FStream.Size := NewSize;
end;

procedure TPrecompVMStream.SetSize(NewSize: Longint);
begin
  SetSize(Int64(NewSize));
end;

function TPrecompVMStream.Read(var Buffer; Count: Longint): Longint;
begin
  if FInitialised then
    Result := FStream.Read(Buffer, Count)
  else
    Result := 0;
end;

function TPrecompVMStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := 0;
  if not FInitialised then
    if Count > 0 then
      Initialise
    else
      exit;
  Result := FStream.Write(Buffer, Count);
end;

function TPrecompVMStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  if FInitialised then
    Result := FStream.Seek(Offset, Origin)
  else
    Result := 0;
end;

procedure TPrecompVMStream.Done;
begin
  FStream.Free;
  FDone := True;
end;

function PrecompGetCodec(Cmd: PChar; Index: Integer; WithParams: Boolean)
  : TPrecompStr;
var
  List0, List1, List2: System.Types.TStringDynArray;
  I: Integer;
  S: String;
begin
  Result := '';
  S := '';
  if Cmd <> nil then
  begin
    List0 := DecodeStr(Cmd, SPrecompSep4);
    List1 := DecodeStr(List0[0], SPrecompSep1);
    if InRange(Index, Low(List1), High(List1)) then
      if WithParams then
      begin
        List2 := DecodeStr(List1[Index], SPrecompSep2);
        S := List2[0] + SPrecompSep2;
        for I := Succ(Low(List2)) to High(List2) do
          if not ResourceExists(List2[I]) then
            S := S + List2[I] + SPrecompSep2;
        if Length(S) > 0 then
          S := S.Remove(Pred(Length(S)));
      end
      else
        S := DecodeStr(List1[Index], SPrecompSep2, 1)[0];
  end;
  StringToWideChar(S, @Result, Length(Result));
end;

function PrecompGetParam(Cmd: PChar; Index: Integer; Param: PChar): TPrecompStr;
var
  List0, List1, List2: System.Types.TStringDynArray;
  I: Integer;
  S: String;
begin
  Result := '';
  if Cmd <> '' then
  begin
    List0 := DecodeStr(Cmd, SPrecompSep4);
    List1 := DecodeStr(List0[0], SPrecompSep1);
    if InRange(Index, Low(List1), High(List1)) then
    begin
      List2 := DecodeStr(List1[Index], SPrecompSep2);
      if Param = '' then
      begin
        S := '';
        for I := Succ(Low(List2)) to High(List2) do
          if ResourceExists(List2[I]) = False then
          begin
            if S <> '' then
              S := S + SPrecompSep2;
            S := S + List2[I];
          end;
        if S = '' then
          S := ' ';
      end
      else
      begin
        for I := Succ(Low(List2)) to High(List2) do
          if List2[I].StartsWith(Param, False) and
            (ResourceExists(List2[I]) = False) then
          begin
            S := List2[I].Substring(Length(Param));
            if S = '' then
              S := ' ';
          end;
      end;
    end;
  end;
  StringToWideChar(S, @Result, Length(Result));
end;

function PrecompGetDepthCodec(Cmd: PChar): TPrecompStr cdecl;
var
  List: System.Types.TStringDynArray;
  I: Integer;
  S: String;
begin
  Result := '';
  S := '';
  if Cmd <> nil then
  begin
    List := DecodeStr(Cmd, SPrecompSep4);
    for I := Succ(Low(List)) to High(List) do
      S := S + List[I] + SPrecompSep4;
    if Length(S) > 0 then
      S := S.Remove(Pred(Length(S)));
  end;
  StringToWideChar(S, @Result, Length(Result));
end;

function PrecompCompress(Codec: PChar; InBuff: Pointer; InSize: Integer;
  OutBuff: Pointer; OutSize: Integer; DictBuff: Pointer;
  DictSize: Integer): Integer;
var
  ZStream: z_stream;
  I, X: Integer;
  S: String;
begin
  Result := 0;
  X := IndexText(PrecompGetCodec(Codec, 0, False),
    ['zlib', 'lz4', 'lz4hc', 'lzo1c', 'lzo1x', 'lzo2a', 'zstd', 'lzna',
    'kraken', 'mermaid', 'selkie', 'hydra', 'leviathan']);
  case X of
    0:
      if ZLibDLL.DLLLoaded then
      begin
        S := PrecompGetParam(Codec, 0, 'l');
        if S = '' then
          S := '68';
        I := EnsureRange(StrToInt(S), 1, 99);
        I := IfThen(I < 10, I * 10, I);
        I := IfThen(I mod 10 = 0, I + 8, I);
        S := PrecompGetParam(Codec, 0, 'w');
        if S = '' then
          S := '15';
        FillChar(ZStream, SizeOf(z_stream), 0);
        ZStream.next_in := InBuff;
        ZStream.avail_in := InSize;
        ZStream.next_out := OutBuff;
        ZStream.avail_out := OutSize;
        deflateInit2(ZStream, I div 10, Z_DEFLATED, -StrToInt(S), I mod 10,
          Z_DEFAULT_STRATEGY);
        try
          if deflate(ZStream, Z_FINISH) = Z_STREAM_END then
            Result := ZStream.total_out;
        finally
          deflateEnd(ZStream);
        end;
      end;
    1:
      if LZ4DLL.DLLLoaded then
        Result := LZ4_compress_default(InBuff, OutBuff, InSize, OutSize);
    2:
      if LZ4DLL.DLLLoaded then
      begin
        S := PrecompGetParam(Codec, 0, 'l');
        if S = '' then
          S := '9';
        Result := LZ4_compress_HC(InBuff, OutBuff, InSize, OutSize,
          StrToInt(S));
      end;
    6:
      if ZSTDDLL.DLLLoaded then
      begin
        S := PrecompGetParam(Codec, 0, 'l');
        if S = '' then
          S := '19';
        Result := ZSTD_compress(OutBuff, OutSize, InBuff, InSize, StrToInt(S));
      end;
    7 .. 12:
      if OodleDLL.DLLLoaded then
      begin
        case X of
          7:
            I := 6;
          8:
            I := 8;
          9:
            I := 9;
          10:
            I := 11;
          11:
            I := 12;
          12:
            I := 13;
        else
          I := 8;
        end;
        S := PrecompGetParam(Codec, 0, 'l');
        if S = '' then
          S := '4';
        Result := OodleLZ_Compress(I, InBuff, InSize, OutBuff, StrToInt(S));
      end;
  end;
end;

function PrecompDecompress(Codec: PChar; InBuff: Pointer; InSize: Integer;
  OutBuff: Pointer; OutSize: Integer; DictBuff: Pointer;
  DictSize: Integer): Integer;
var
  ZStream: z_stream;
  S: String;
begin
  Result := 0;
  case IndexText(Codec, ['zlib', 'lz4', 'lz4hc', 'lz4f', 'lzo1c', 'lzo1x',
    'lzo2a', 'zstd', 'lzna', 'kraken', 'mermaid', 'selkie', 'hydra',
    'leviathan']) of
    0:
      if ZLibDLL.DLLLoaded then
      begin
        S := PrecompGetParam(Codec, 0, 'w');
        if S = '' then
          S := '15';
        FillChar(ZStream, SizeOf(z_stream), 0);
        inflateInit2(ZStream, -StrToInt(S));
        try
          ZStream.next_in := InBuff;
          ZStream.avail_in := InSize;
          ZStream.next_out := OutBuff;
          ZStream.avail_out := OutSize;
          if inflate(ZStream, Z_FINISH) = Z_STREAM_END then
            Result := ZStream.total_out;
        finally
          inflateEnd(ZStream);
        end;
      end;
    1, 2:
      if LZ4DLL.DLLLoaded then
        Result := LZ4_decompress_safe(InBuff, OutBuff, InSize, OutSize);
    3:
      if LZ4DLL.DLLLoaded then
        Result := LZ4F_decompress_safe(InBuff, OutBuff, InSize, OutSize);
    7:
      if ZSTDDLL.DLLLoaded then
        Result := ZSTD_decompress(OutBuff, OutSize, InBuff, InSize);
    8 .. 13:
      if OodleDLL.DLLLoaded then
        Result := OodleLZ_Decompress(InBuff, InSize, OutBuff, OutSize);
  end;
end;

function PrecompEncrypt(Codec: PChar; InBuff: Pointer; InSize: Integer;
  KeyBuff: Pointer; KeySize: Integer): Boolean;
var
  AES: TAESECB;
  RC4: TRC4;
begin
  Result := False;
  case IndexText(Codec, ['xor', 'aes', 'rc4']) of
    0:
      begin
        XorBuffer(InBuff, InSize, KeyBuff, KeySize);
        Result := True;
      end;
    1:
      begin
        AES := TAESECB.Create(KeyBuff^, KeySize * 8);
        try
          AES.Encrypt(InBuff, InBuff, InSize);
          Result := True;
        finally
          AES.Free;
        end;
      end;
    2:
      begin
        RC4.Init(KeyBuff^, KeySize);
        RC4.Encrypt(InBuff^, InBuff^, InSize);
        Result := True;
      end;
  end;
end;

function PrecompDecrypt(Codec: PChar; InBuff: Pointer; InSize: Integer;
  KeyBuff: Pointer; KeySize: Integer): Boolean;
var
  AES: TAESECB;
  RC4: TRC4;
begin
  Result := False;
  case IndexText(Codec, ['xor', 'aes', 'rc4']) of
    0:
      begin
        XorBuffer(InBuff, InSize, KeyBuff, KeySize);
        Result := True;
      end;
    1:
      begin
        AES := TAESECB.Create(KeyBuff^, KeySize * 8);
        try
          AES.Decrypt(InBuff, InBuff, InSize);
          Result := True;
        finally
          AES.Free;
        end;
      end;
    2:
      begin
        RC4.Init(KeyBuff^, KeySize);
        RC4.Encrypt(InBuff^, InBuff^, InSize);
        Result := True;
      end;
  end;
end;

function PrecompHash(Codec: PChar; InBuff: Pointer; InSize: Integer;
  HashBuff: Pointer; HashSize: Integer): Boolean;
var
  LMD5: TMD5;
  LMD5Digest: TMD5Digest;
  LSHA1: TSHA1;
  LSHA1Digest: TSHA1Digest;
  xxSeed32: XXH32_hash_t;
  xxSeed64: XXH32_hash_t;
begin
  Result := False;
  case IndexText(Codec, ['crc32', 'adler32', 'crc64', 'md5', 'sha1', 'xxh32',
    'xxh64', 'xxh128', 'xxh3', 'xxh3_128']) of
    0:
      if HashSize = SizeOf(Cardinal) then
      begin
        PCardinal(HashBuff)^ := crc32c(PCardinal(HashBuff)^, InBuff, InSize);
        Result := True;
      end;
    1:
      if HashSize = SizeOf(Cardinal) then
      begin
        PCardinal(HashBuff)^ := Adler32Asm(PCardinal(HashBuff)^,
          InBuff, InSize);
        Result := True;
      end;
    2:
      if HashSize = SizeOf(Int64) then
      begin
        PInt64(HashBuff)^ := crc64c(InBuff, InSize);
        Result := True;
      end;
    3:
      if HashSize = SizeOf(TMD5Digest) then
      begin
        LMD5.Full(InBuff, InSize, LMD5Digest);
        Move(LMD5Digest, HashBuff^, HashSize);
        Result := True;
      end;
    4:
      if HashSize = SizeOf(TSHA1Digest) then
      begin
        LSHA1.Full(InBuff, InSize, LSHA1Digest);
        Move(LSHA1Digest, HashBuff^, HashSize);
        Result := True;
      end;
    5:
      if HashSize = SizeOf(XXH32_hash_t) then
      begin
        xxSeed32 := 0;
        PXXH32_hash_t(HashBuff)^ := XXH32(InBuff, InSize, xxSeed32);
        Result := True;
      end;
    6:
      if HashSize = SizeOf(XXH64_hash_t) then
      begin
        xxSeed64 := 0;
        PXXH64_hash_t(HashBuff)^ := XXH64(InBuff, InSize, xxSeed64);
        Result := True;
      end;
    7:
      if HashSize = SizeOf(XXH128_hash_t) then
      begin
        xxSeed64 := 0;
        PXXH128_hash_t(HashBuff)^ := XXH128(InBuff, InSize, xxSeed64);
        Result := True;
      end;
    8:
      if HashSize = SizeOf(XXH64_hash_t) then
      begin
        PXXH64_hash_t(HashBuff)^ := XXH3_64bits(InBuff, InSize);
        Result := True;
      end;
    9:
      if HashSize = SizeOf(XXH128_hash_t) then
      begin
        PXXH128_hash_t(HashBuff)^ := XXH3_128bits(InBuff, InSize);
        Result := True;
      end;
  end;
end;

function PrecompEncodePatch(OldBuff: Pointer; OldSize: Integer;
  NewBuff: Pointer; NewSize: Integer; PatchBuff: Pointer;
  PatchSize: Integer): Integer;
var
  Res: NativeUInt;
begin
  Result := 0;
  if not XDeltaDLL.DLLLoaded then
    exit;
  if xd3_encode(OldBuff, OldSize, NewBuff, NewSize, PatchBuff, @Res, PatchSize,
    Integer(XD3_NOCOMPRESS)) = 0 then
    Result := Res;
  // MakeDiff(OldBuff, NewBuff, PatchBuff, OldSize, NewSize, Result);
end;

function PrecompDecodePatch(PatchBuff: Pointer; PatchSize: Integer;
  OldBuff: Pointer; OldSize: Integer; NewBuff: Pointer;
  NewSize: Integer): Integer;
var
  Res: NativeUInt;
begin
  Result := 0;
  if not XDeltaDLL.DLLLoaded then
    exit;
  if xd3_decode(PatchBuff, PatchSize, OldBuff, OldSize, NewBuff, @Res, NewSize,
    Integer(XD3_NOCOMPRESS)) = 0 then
    Result := Res;
  // MakePatch(OldBuff, PatchBuff, NewBuff, OldSize, PatchSize, Result);
end;

function PrecompAddResource(FileName: PChar): Integer;
var
  I: Integer;
  Exists: Boolean;
begin
  Result := -1;
  Exists := False;
  for I := Low(Resources) to High(Resources) do
  begin
    Exists := SameText(Resources[I].Name, ExtractFileName(FileName));
    if Exists then
    begin
      Result := I;
      break;
    end;
  end;
  if not Exists then
  begin
    with TFileStream.Create(PluginsPath + FileName, fmOpenRead or
      fmShareDenyNone) do
      try
        I := Length(Resources);
        SetLength(Resources, Succ(I));
        Resources[I].Name := ExtractFileName(FileName);
        Resources[I].Size := Size;
        GetMem(Resources[I].Data, Resources[I].Size);
        ReadBuffer(Resources[I].Data^, Resources[I].Size);
        Result := I;
      finally
        Free;
      end;
  end;
end;

function PrecompGetResource(Index: Integer; Data: Pointer;
  Size: PInteger): Boolean;
begin
  Result := False;
  if (InRange(Index, 0, Pred(Length(Resources))) = False) or
    (Length(Resources) <= 0) then
  begin
    Size^ := -1;
    exit;
  end;
  if Assigned(Data) then
    Move(Resources[Index].Data^, Data^, Resources[Index].Size);
  Size^ := Resources[Index].Size;
  Result := True;
end;

function PrecompSearchBinary(SrcMem: Pointer; SrcPos, SrcSize: NativeInt;
  SearchMem: Pointer; SearchSize: NativeInt; ResultPos: PNativeInt): Boolean;
begin
  Result := BinarySearch(SrcMem, SrcPos, SrcSize, SearchMem, SearchSize,
    ResultPos^);
end;

procedure PrecompSwapBinary(Source, Dest: Pointer; Size: NativeInt);
begin
  ReverseBytes(Source, Dest, Size);
end;

function PrecompSwap16(Int: Int16): Int16;
begin
  Result := EndianSwap(Int);
end;

function PrecompSwap32(Int: Int32): Int32;
begin
  Result := EndianSwap(Int);
end;

function PrecompSwap64(Int: Int64): Int64;
begin
  Result := EndianSwap(Int);
end;

function PrecompFileOpen(FileName: PChar; Create: Boolean): THandle;
begin
  if Create then
    FileCreate(FileName)
  else
    FileOpen(FileName, fmOpenReadWrite or fmShareDenyNone);
end;

procedure PrecompFileClose(Handle: THandle);
begin
  FileClose(Handle);
end;

function PrecompFileSeek(Handle: THandle; Offset: Int64;
  Origin: Integer): Int64;
begin
  Result := FileSeek(Handle, Offset, Origin);
end;

function PrecompFileSize(Handle: THandle): Int64;
var
  LPos: Int64;
begin
  LPos := FileSeek(Handle, 0, FILE_CURRENT);
  Result := FileSeek(Handle, 0, FILE_END);
  FileSeek(Handle, LPos, FILE_BEGIN);
end;

function PrecompFileRead(Handle: THandle; Buffer: Pointer;
  Count: Integer): Integer;
begin
  Result := FileRead(Handle, Buffer^, Count);
end;

function PrecompFileWrite(Handle: THandle; Buffer: Pointer;
  Count: Integer): Integer;
begin
  Result := FileWrite(Handle, Buffer^, Count);
end;

function PrecompIniRead(Section, Key, Default, FileName: PChar): TPrecompStr;
var
  S: String;
begin
  S := GetIniString(Section, Key, Default, FileName);
  StringToWideChar(S, @Result, Length(Result));
end;

procedure PrecompIniWrite(Section, Key, Value, FileName: PChar);
begin
  SetIniString(Section, Key, Value, FileName);
end;

function PrecompExec(Executable, CommandLine, WorkDir: PChar): Boolean;
begin
  Result := Exec(Executable, CommandLine, WorkDir);
end;

function PrecompExecStdin(Executable, CommandLine, WorkDir: PChar;
  InBuff: Pointer; InSize: Integer): Boolean;
begin
  Result := ExecStdin(Executable, CommandLine, WorkDir, InBuff, InSize);
end;

function PrecompExecStdout(Instance: Integer;
  Executable, CommandLine, WorkDir: PChar; Output: _ExecOutput): Boolean;
const
  PipeSecurityAttributes: TSecurityAttributes =
    (nLength: SizeOf(PipeSecurityAttributes); bInheritHandle: True);
  BufferSize = 65536;
var
  hstdoutr, hstdoutw: THandle;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  dwExitCode: DWORD;
  Buffer: array [0 .. BufferSize - 1] of Byte;
  BytesRead: DWORD;
  LWorkDir: PChar;
begin
  Result := False;
  CreatePipe(hstdoutr, hstdoutw, @PipeSecurityAttributes, 0);
  SetHandleInformation(hstdoutr, HANDLE_FLAG_INHERIT, 0);
  ZeroMemory(@StartupInfo, SizeOf(StartupInfo));
  StartupInfo.cb := SizeOf(StartupInfo);
  StartupInfo.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := SW_HIDE;
  StartupInfo.hStdInput := 0;
  StartupInfo.hStdOutput := hstdoutw;
  StartupInfo.hStdError := 0;
  ZeroMemory(@ProcessInfo, SizeOf(ProcessInfo));
  if WorkDir <> '' then
    LWorkDir := Pointer(WorkDir)
  else
    LWorkDir := Pointer(GetCurrentDir);
  if CreateProcess(nil, PChar('"' + Executable + '" ' + CommandLine), nil, nil,
    True, NORMAL_PRIORITY_CLASS, nil, LWorkDir, StartupInfo, ProcessInfo) then
  begin
    CloseHandleEx(ProcessInfo.hThread);
    CloseHandleEx(hstdoutw);
    try
      while ReadFile(hstdoutr, Buffer, Length(Buffer), BytesRead, nil) and
        (BytesRead > 0) do
        Output(Instance, @Buffer[0], BytesRead);
    finally
      CloseHandleEx(hstdoutr);
      WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
      GetExitCodeProcess(ProcessInfo.hProcess, dwExitCode);
      CloseHandleEx(ProcessInfo.hProcess);
    end;
    Result := dwExitCode = 0;
  end
  else
  begin
    CloseHandleEx(hstdoutr);
    CloseHandleEx(hstdoutw);
    RaiseLastOSError;
  end;
end;

function PrecompExecStdio(Instance: Integer;
  Executable, CommandLine, WorkDir: PChar; InBuff: Pointer; InSize: Integer;
  Output: _ExecOutput): Boolean;
const
  PipeSecurityAttributes: TSecurityAttributes =
    (nLength: SizeOf(PipeSecurityAttributes); bInheritHandle: True);
  BufferSize = 65536;
var
  Buffer: array [0 .. BufferSize - 1] of Byte;
  BytesRead: DWORD;
  hstdinr, hstdinw: THandle;
  hstdoutr, hstdoutw: THandle;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  dwExitCode: DWORD;
  LWorkDir: PChar;
begin
  Result := True;
  CreatePipe(hstdinr, hstdinw, @PipeSecurityAttributes, 0);
  CreatePipe(hstdoutr, hstdoutw, @PipeSecurityAttributes, 0);
  SetHandleInformation(hstdinw, HANDLE_FLAG_INHERIT, 0);
  SetHandleInformation(hstdoutr, HANDLE_FLAG_INHERIT, 0);
  ZeroMemory(@StartupInfo, SizeOf(StartupInfo));
  StartupInfo.cb := SizeOf(StartupInfo);
  StartupInfo.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := SW_HIDE;
  StartupInfo.hStdInput := hstdinr;
  StartupInfo.hStdOutput := hstdoutw;
  StartupInfo.hStdError := 0;
  ZeroMemory(@ProcessInfo, SizeOf(ProcessInfo));
  if WorkDir <> '' then
    LWorkDir := Pointer(WorkDir)
  else
    LWorkDir := Pointer(GetCurrentDir);
  if CreateProcess(nil, PChar('"' + Executable + '" ' + CommandLine), nil, nil,
    True, NORMAL_PRIORITY_CLASS, nil, LWorkDir, StartupInfo, ProcessInfo) then
  begin
    CloseHandleEx(ProcessInfo.hThread);
    CloseHandleEx(hstdinr);
    CloseHandleEx(hstdoutw);
    try
      FileWriteBuffer(hstdinw, InBuff^, InSize);
      CloseHandleEx(hstdinw);
      while ReadFile(hstdoutr, Buffer[0], Length(Buffer), BytesRead, nil) and
        (BytesRead > 0) do
        Output(Instance, @Buffer[0], BytesRead);
    finally
      CloseHandleEx(hstdinw);
      CloseHandleEx(hstdoutr);
      WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
      GetExitCodeProcess(ProcessInfo.hProcess, dwExitCode);
      CloseHandleEx(ProcessInfo.hProcess);
    end;
    Result := dwExitCode = 0;
  end
  else
  begin
    CloseHandleEx(hstdinr);
    CloseHandleEx(hstdinw);
    CloseHandleEx(hstdoutr);
    CloseHandleEx(hstdoutw);
    RaiseLastOSError;
  end;
end;

procedure ExecReadTask(Instance, Handle, Stream: IntPtr);
const
  BufferSize = 65536;
var
  Buffer: array [0 .. BufferSize - 1] of Byte;
  BytesRead: DWORD;
begin
  while ReadFile(Handle, Buffer[0], Length(Buffer), BytesRead, nil) and
    (BytesRead > 0) do
    PExecOutput(Pointer(Stream))^(Instance, @Buffer[0], BytesRead);
end;

function PrecompExecStdioSync(Instance: Integer;
  Executable, CommandLine, WorkDir: PChar; InBuff: Pointer; InSize: Integer;
  Output: _ExecOutput): Boolean;
const
  PipeSecurityAttributes: TSecurityAttributes =
    (nLength: SizeOf(PipeSecurityAttributes); bInheritHandle: True);
var
  hstdinr, hstdinw: THandle;
  hstdoutr, hstdoutw: THandle;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  dwExitCode: DWORD;
  LWorkDir: PChar;
  LTask: TTask;
  LDone: Boolean;
begin
  Result := True;
  CreatePipe(hstdinr, hstdinw, @PipeSecurityAttributes, 0);
  CreatePipe(hstdoutr, hstdoutw, @PipeSecurityAttributes, 0);
  SetHandleInformation(hstdinw, HANDLE_FLAG_INHERIT, 0);
  SetHandleInformation(hstdoutr, HANDLE_FLAG_INHERIT, 0);
  ZeroMemory(@StartupInfo, SizeOf(StartupInfo));
  StartupInfo.cb := SizeOf(StartupInfo);
  StartupInfo.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := SW_HIDE;
  StartupInfo.hStdInput := hstdinr;
  StartupInfo.hStdOutput := hstdoutw;
  StartupInfo.hStdError := 0;
  ZeroMemory(@ProcessInfo, SizeOf(ProcessInfo));
  if WorkDir <> '' then
    LWorkDir := Pointer(WorkDir)
  else
    LWorkDir := Pointer(GetCurrentDir);
  if CreateProcess(nil, PChar('"' + Executable + '" ' + CommandLine), nil, nil,
    True, NORMAL_PRIORITY_CLASS, nil, LWorkDir, StartupInfo, ProcessInfo) then
  begin
    CloseHandleEx(ProcessInfo.hThread);
    CloseHandleEx(hstdinr);
    CloseHandleEx(hstdoutw);
    LTask := TTask.Create(Instance, hstdoutr, NativeInt(@Output),
      NativeInt(@LDone));
    LTask.Perform(ExecReadTask);
    LTask.Start;
    try
      FileWriteBuffer(hstdinw, InBuff^, InSize);
    finally
      CloseHandleEx(hstdinw);
      LTask.Wait;
      if LTask.Status <> TThreadStatus.tsErrored then
        LTask.Free;
      CloseHandleEx(hstdoutr);
    end;
    if Assigned(LTask) then
      if LTask.Status <> TThreadStatus.tsErrored then
        try
          LTask.RaiseLastError;
        finally
          LTask.Free;
        end;
    Result := dwExitCode = 0;
  end
  else
  begin
    CloseHandleEx(hstdinr);
    CloseHandleEx(hstdinw);
    CloseHandleEx(hstdoutr);
    CloseHandleEx(hstdoutw);
    RaiseLastOSError;
  end;
end;

function PrecompAcceptPatch(OldSize, NewSize, PatchSize: Integer): Boolean;
begin
  Result := False;
  if PatchSize > 0 then
    if DIFF_TOLERANCE <= 1 then
      Result := (PatchSize / Max(OldSize, NewSize)) <= DIFF_TOLERANCE
    else
      Result := PatchSize <= DIFF_TOLERANCE;
end;

initialization

EncodeSICmp := TEncodeSIComparer.Create;
FutureSICmp := TFutureSIComparer.Create;
StockMethods := TStringList.Create;
ExternalMethods := TStringList.Create;
if not XDeltaDLL.DLLLoaded then
  DIFF_TOLERANCE := 0;

end.
