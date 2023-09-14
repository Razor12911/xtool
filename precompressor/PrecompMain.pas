unit PrecompMain;

{$POINTERMATH ON}

interface

uses
  InitCode,
  Threading, Utils, SynCommons, ParseClass, ParseExpr, FLZMA2DLL, XXHASHLIB,
  PrecompUtils, PrecompCrypto, PrecompZLib, PrecompLZ4, PrecompLZO, PrecompZSTD,
  PrecompOodle, PrecompMedia, PrecompDStorage, PrecompINI, PrecompINIEx,
  PrecompSearch, PrecompDLL, PrecompEXE,
  WinAPI.Windows, WinAPI.ShlObj,
  System.SysUtils, System.Classes, System.SyncObjs, System.Math, System.Types,
  System.StrUtils, System.RTLConsts, System.TimeSpan, System.Diagnostics,
  System.Generics.Defaults, System.Generics.Collections, System.Character;

const
  XTOOL_PRECOMP = $304C5458;
  XTOOL_BSIZE = 4194304;
{$IFDEF CPU64BITS}
  XTOOL_MEMLIMIT = Int64.MaxValue;
{$ELSE}
  XTOOL_MEMLIMIT = $60000000;
{$ENDIF}
  XTOOL_FREEMEM = $40000000;

type
  PEncodeOptions = ^TEncodeOptions;

  TEncodeOptions = record
    Method: String;
    ChunkSize, Threads: Integer;
    Depth: Integer;
    LowMem: Boolean;
    DBaseFile, ExtractDir: String;
    CThreads, CLevel: Integer;
    CDict, COverlap: Integer;
    CHighCompress: Boolean;
  end;

  PDecodeOptions = ^TDecodeOptions;

  TDecodeOptions = record
    Method: String;
    Threads: Integer;
    Depth: Integer;
    DedupSysMem: Int64;
    DThreads: Integer;
    DMem: Int64;
  end;

procedure PrintHelp;
procedure Parse(ParamArg: TArray<string>; out Options: TEncodeOptions);
  overload;
procedure Parse(ParamArg: TArray<string>; out Options: TDecodeOptions);
  overload;
procedure Encode(Input, Output: TStream; Options: TEncodeOptions);
procedure Decode(Input, Output: TStream; Options: TDecodeOptions);

function PrecompAllocator(Instance: Integer; Size: Integer): Pointer cdecl;
function PrecompReadFuture(Index: Integer; Position: NativeInt; Buffer: Pointer;
  Count: Integer): Integer cdecl;
procedure PrecompLogScan1(Codec: PChar; Position: Int64;
  InSize, OutSize: Integer)cdecl;
procedure PrecompLogScan2(Codec: PChar; InSize, OutSize: Integer)cdecl;
procedure PrecompLogProcess(Codec, Method: PChar; Size1, Size2, Size3: Integer;
  Status: Boolean)cdecl;
procedure PrecompLogRestore(Codec, Method: PChar; Size1, Size2, Size3: Integer;
  Status: Boolean)cdecl;
procedure PrecompLogPatch1(OldSize, NewSize, PatchSize: Integer;
  Status: Boolean)cdecl;
procedure PrecompLogPatch2(OldSize, NewSize, PatchSize: Integer;
  Status: Boolean)cdecl;
procedure PrecompLogReprocess(Method: PChar; Size1, Size2, Size3: Integer;
  Status: Boolean)cdecl;

procedure PrecompOutput1(Instance: Integer; const Buffer: Pointer;
  Size: Integer)cdecl;
procedure PrecompOutput2(Instance: Integer; const Buffer: Pointer;
  Size: Integer)cdecl;
procedure PrecompOutput3(Instance: Integer; const Buffer: Pointer;
  Size: Integer)cdecl;
procedure PrecompAddStream(Instance: Integer; Info: PStrInfo1; Codec: PChar;
  Reserved: Pointer)cdecl;
procedure PrecompAddDepthStream(Instance: Integer; Position: Int64;
  OldSize, NewSize: Integer; Codec: PChar; Option, Resource: Integer)cdecl;
procedure PrecompTransfer(Instance: Integer; Codec: PChar)cdecl;
function PrecompStorage(Instance: Integer; Size: PInteger): Pointer cdecl;
function PrecompAddResourceEx(Data: Pointer; Size: Integer): Integer cdecl;

implementation

procedure EncInit(Input, Output: TStream; Options: PEncodeOptions); forward;
procedure EncFree; forward;
function EncData(Input, Output: TStream; Index, Depth: Integer)
  : Boolean; forward;

procedure DecInit(Input, Output: TStream; Options: PDecodeOptions); forward;
procedure DecFree; forward;
procedure DecChunk(Input, Output: TStream; Index, Depth: Integer); forward;

type
  TEncInfo = record
    Processed, Count: Integer;
    DecMem0, DecMem1, DecMem2, DecMem3: Int64;
    DupSize0, DupSize1, DupSize2: Int64;
    DupCount: Integer;
    InSize, InflSize, SrepSize, CompSize: Int64;
    SrepMem: Integer;
  end;

const
  SREPEXE64 = 'srep64.exe';

var
  GlobalSync: TCriticalSection;
  ThreadSync: TArray<TCriticalSection>;
  IntArray: array [0 .. 1] of Int64;
  Codecs: array of TPrecompressor;
  DBFile: String = '';
  ExtDir: String = '';
  SrepCfg: String;
  ResCount: Integer;
  UseDB: Boolean = False;
  StoreDD: Integer = -2;
  VERBOSE: Boolean = False;
  EXTRACT: Boolean = False;
  NOVERIFY: Boolean = False;
  REPROCESS: String = '';
  REASSIGN: String = '';
  COMPRESS: Byte = 0;
  EXTCOMP: String = '';
  SREPEXE: String = 'srep.exe';
  CACHE: Int64 = 0;
  NULLOUT: Boolean = False;
  DupSysMem: Int64 = 0;
  DecodeMemBlock: Integer = 512 * 1024 * 1024;
  EncInfo: TEncInfo;
  EncFreed: Boolean = False;
  ConTask: TTask;
  Stopwatch: TStopwatch;
  ThrPty: TThreadPriority = TThreadPriority.tpNormal;

function ExtractExec(S: String): String;
begin
  Result := S.Substring(0, Pos('.exe', S) + 3);
end;

function ExtractParams(S: String): String;
begin
  Result := S.Substring(Pos('.exe', S) + 4);
end;

procedure PrintHelp;
var
  I, J: Integer;
  S: string;
begin
  WriteLine('precomp - data precompressor');
  WriteLine('');
  WriteLine('Usage:');
  WriteLine('  xtool precomp [parameters] input output');
  WriteLine('');
  WriteLine('');
  WriteLine('Parameters:');
  WriteLine(
    '  -m#  - codecs to use for precompression (separate with "+" if more than one)');
  WriteLine('  -c#  - scanning range of precompressor [16mb]');
  WriteLine('  -t#  - number of working threads [50p]');
  WriteLine('  -d#  - scan depth [0]');
  WriteLine('  -dd  - use stream deduplication');
  WriteLine(
    '  -l#  - compression level for fast lzma2 (separate params with ":") [0]');
  WriteLine('               d# - dictionary size');
  WriteLine('  -lm  - low memory mode');
  WriteLine('  -s   - skip stream verification');
  WriteLine('  -v   - enables verbose mode');
  WriteLine('  -df# - set patching threshold to accept streams [5p]');
  WriteLine('               l# - patch compression level (1-22) [1]');
  WriteLine('  -x#  - extract streams to directory path');
  WriteLine('  -dm# - deduplication memory usage limit (#=size) [75p]');
  WriteLine('  -db# - decode block size [512mb]');
  WriteLine('  -sp# - srep parameters (separate params with ":")');
  WriteLine('  -p#  - prefetch cache [0mb]');
  WriteLine('  -r#  - recompress streams detected with another codec');
  WriteLine('  -a#  - assign streams detected to another codec');
  WriteLine('');
  WriteLine('  -lz4#, -zstd#, -oodle# - specify custom library name');
  WriteLine('');
end;

procedure Parse(ParamArg: TArray<string>; out Options: TEncodeOptions);
var
  ArgParse: TArgParser;
  ExpParse: TExpressionParser;
  List: TStringDynArray;
  I, J: Integer;
  S: String;
  B: Boolean;
begin
  FillChar(Options, SizeOf(TEncodeOptions), 0);
  Options.Depth := 1;
  ArgParse := TArgParser.Create(ParamArg);
  ExpParse := TExpressionParser.Create;
  try
    I := 0;
    while True do
    begin
      S := ArgParse.AsString('-m', I);
      if S = '' then
        break;
      S := ReplaceStr(S, SPrecompSep3, SPrecompSep2);
      if Options.Method <> '' then
        Options.Method := Options.Method + '+' + S
      else
        Options.Method := S;
      Inc(I);
    end;
    S := ChangeFileExt(ExtractFileName(Utils.GetModuleName), '');
    if FORCEDMETHOD then
      if Options.Method <> '' then
        Options.Method := Options.Method + '+' + S
      else
        Options.Method := S;
    S := ArgParse.AsString('-c', 0, '16mb');
    S := ReplaceText(S, 'KB', '* 1024^1');
    S := ReplaceText(S, 'MB', '* 1024^2');
    S := ReplaceText(S, 'GB', '* 1024^3');
    S := ReplaceText(S, 'K', '* 1024^1');
    S := ReplaceText(S, 'M', '* 1024^2');
    S := ReplaceText(S, 'G', '* 1024^3');
    Options.ChunkSize := EnsureRange(Round(ExpParse.Evaluate(S)),
      4 * 1024 * 1024, $7FFFFFFF);
    S := ArgParse.AsString('-t', 0, '50p');
    S := ReplaceText(S, 'p', '%');
    S := ReplaceText(S, '%', '%*' + CPUCount.ToString);
    Options.Threads := Max(1, Round(ExpParse.Evaluate(S)));
    ThrPty := TThreadPriority(EnsureRange(ArgParse.AsInteger('-T', 0,
      Integer(tpNormal)), Integer(tpIdle), Integer(tpTimeCritical)));
    StoreDD := -2;
    I := 0;
    while True do
    begin
      S := ArgParse.AsString('-d', I);
      if S = '' then
        break;
      case S[1] of
        'b':
          begin
            S := S.Substring(1);
            S := ReplaceText(S, 'KB', '* 1024^1');
            S := ReplaceText(S, 'MB', '* 1024^2');
            S := ReplaceText(S, 'GB', '* 1024^3');
            S := ReplaceText(S, 'K', '* 1024^1');
            S := ReplaceText(S, 'M', '* 1024^2');
            S := ReplaceText(S, 'G', '* 1024^3');
            DecodeMemBlock := EnsureRange(Round(ExpParse.Evaluate(S)), 0,
              $7FFFFFFF);
          end;
        'd':
          begin
            StoreDD := -1;
            if FileExists(ExpandPath(PluginsPath + SREPEXE, True)) then
              if S.Length > 1 then
                StoreDD := StrToInt(S[2]);
          end;
        'f':
          begin
            S := S.Substring(1);
            S := ReplaceText(S, SPrecompSep3, SPrecompSep2);
            List := DecodeStr(S, SPrecompSep2);
            for J := Low(List) to High(List) do
            begin
              S := List[J];
              if J = 0 then
              begin
                S := ReplaceText(S, 'KB', '* 1024^1');
                S := ReplaceText(S, 'MB', '* 1024^2');
                S := ReplaceText(S, 'GB', '* 1024^3');
                S := ReplaceText(S, 'K', '* 1024^1');
                S := ReplaceText(S, 'M', '* 1024^2');
                S := ReplaceText(S, 'G', '* 1024^3');
                S := ReplaceText(S, 'p', '%');
                S := ReplaceText(S, SPrecompSep3, SPrecompSep2);
                DIFF_TOLERANCE := Max(0.00, ExpParse.Evaluate(S));
              end
              else
              begin
                if List[J].StartsWith('l', False) then
                  DIFF_CLEVEL :=
                    EnsureRange(ConvertToBytes(List[J].Substring(1)), 1, 22);
              end;
            end;
          end;
      else
        if S[1].IsDigit then
          Options.Depth := EnsureRange(Succ(S.ToInteger), 1, 10);
      end;
      Inc(I);
    end;
    UseDB := True;
    I := 0;
    while True do
    begin
      S := ArgParse.AsString('-l', I);
      if S = '' then
        break;
      case S[1] of
        'm':
          Options.LowMem := True;
      else
        if S <> '' then
          if S[1].IsDigit and FLZMA2DLL.DLLLoaded then
          begin
            S := ReplaceText(S, SPrecompSep3, SPrecompSep2);
            if (S <> '') then
            begin
              List := DecodeStr(S, SPrecompSep2);
              Options.CThreads := Options.Threads;
              Options.CHighCompress := List[0].Contains('x');
              if List[0].Contains('x') then
                Options.CLevel :=
                  StrToIntDef(Copy(List[0], 1, List[0].Length - 1), 0)
              else
                Options.CLevel := StrToIntDef(List[0], 0);
              Options.COverlap := 2;
              for J := Low(List) to High(List) do
              begin
                if List[J].StartsWith('d', False) then
                  Options.CDict :=
                    EnsureRange(ConvertToBytes(List[J].Substring(1)),
                    FL2_DICTSIZE_MIN, FL2_DICTSIZE_MAX);
                if List[J].StartsWith('o', False) then
                  Options.COverlap :=
                    EnsureRange(ConvertToBytes(List[J].Substring(1)),
                    FL2_BLOCK_OVERLAP_MIN, FL2_BLOCK_OVERLAP_MAX);
              end;
              COMPRESS := Byte(InRange(Options.CLevel, 1, 10));
            end;
          end;
      end;
      Inc(I);
    end;
    I := 0;
    while True do
    begin
      S := ArgParse.AsString('-s', I);
      if S = '' then
      begin
        if ArgParse.AsBoolean('-s', I) then
          NOVERIFY := True
        else
          break;
      end
      else
        case S[1] of
          'p':
            SrepCfg := S.Substring(1).ToLower;
        end;
      Inc(I);
    end;
{$IFDEF CPU64BITS}
    S := ArgParse.AsString('-p', 0, '0mb');
    S := ReplaceText(S, 'KB', '* 1024^1');
    S := ReplaceText(S, 'MB', '* 1024^2');
    S := ReplaceText(S, 'GB', '* 1024^3');
    S := ReplaceText(S, 'K', '* 1024^1');
    S := ReplaceText(S, 'M', '* 1024^2');
    S := ReplaceText(S, 'G', '* 1024^3');
    S := ReplaceText(S, 'p', '%');
    S := ReplaceText(S, '%', '%*' + GetTotalSystemMemory.ToString);
    B := Pos('%', S) = 0;
    CACHE := Round(ExpParse.Evaluate(S));
    if not B then
      CACHE := Max(0, CACHE - GetUsedSystemMemory);
    if CACHE > Max(0, GetFreeSystemMemory - XTOOL_FREEMEM) then
      CACHE := Max(0, GetFreeSystemMemory - XTOOL_FREEMEM);
{$ELSE}
    CACHE := 0;
{$ENDIF}
    VERBOSE := ArgParse.AsBoolean('-v') and (IsLibrary = False);
    OPTIMISE_DEC := ArgParse.AsBoolean('-o');
    FULLSCAN := ArgParse.AsBoolean('-f');
    Options.ExtractDir := ArgParse.AsString('-x');
    if Options.ExtractDir <> '' then
      EXTRACT := DirectoryExists(Options.ExtractDir);
    EXTCOMP := ArgParse.AsString('-e');
    if FileExists(ExpandPath(PluginsPath + ExtractExec(EXTCOMP), True)) then
      COMPRESS := 2;
    REPROCESSED := ArgParse.AsBoolean('-r');
    REPROCESS := ReplaceStr(ArgParse.AsString('-r'), SPrecompSep3,
      SPrecompSep2);
    if REPROCESS <> '' then
      REPROCESSED := False;
    REASSIGN := ReplaceStr(ArgParse.AsString('-a'), SPrecompSep3, SPrecompSep2);
  finally
    ArgParse.Free;
    ExpParse.Free;
  end;
  if VERBOSE or EXTRACT then
    Options.Threads := 1;
end;

procedure Parse(ParamArg: TArray<string>; out Options: TDecodeOptions);
var
  ArgParse: TArgParser;
  ExpParse: TExpressionParser;
  I: Integer;
  S: String;
  B: Boolean;
  I64: Int64;
begin
  ArgParse := TArgParser.Create(ParamArg);
  ExpParse := TExpressionParser.Create;
  try
    Options.Method := ArgParse.AsString('-m');
    S := ArgParse.AsString('-t', 0, '50p');
    S := ReplaceText(S, 'p', '%');
    S := ReplaceText(S, '%', '%*' + CPUCount.ToString);
    Options.Threads := Max(1, Round(ExpParse.Evaluate(S)));
    Options.DThreads := Options.Threads;
    ThrPty := TThreadPriority(EnsureRange(ArgParse.AsInteger('-T', 0,
      Integer(tpNormal)), Integer(tpIdle), Integer(tpTimeCritical)));
    S := ArgParse.AsString('-dm', 0, '75p');
    S := ReplaceText(S, 'KB', '* 1024^1');
    S := ReplaceText(S, 'MB', '* 1024^2');
    S := ReplaceText(S, 'GB', '* 1024^3');
    S := ReplaceText(S, 'K', '* 1024^1');
    S := ReplaceText(S, 'M', '* 1024^2');
    S := ReplaceText(S, 'G', '* 1024^3');
    S := ReplaceText(S, 'p', '%');
    S := ReplaceText(S, '%', '%*' + GetTotalSystemMemory.ToString);
    B := Pos('%', S) = 0;
    Options.DedupSysMem := Max(0, Round(ExpParse.Evaluate(S)));
    if B then
      Options.DedupSysMem := -Options.DedupSysMem;
    SrepCfg := ArgParse.AsString('-sp', 0, '').ToLower;
{$IFDEF CPU64BITS}
    S := ArgParse.AsString('-p', 0, '0mb');
    S := ReplaceText(S, 'KB', '* 1024^1');
    S := ReplaceText(S, 'MB', '* 1024^2');
    S := ReplaceText(S, 'GB', '* 1024^3');
    S := ReplaceText(S, 'K', '* 1024^1');
    S := ReplaceText(S, 'M', '* 1024^2');
    S := ReplaceText(S, 'G', '* 1024^3');
    S := ReplaceText(S, 'p', '%');
    S := ReplaceText(S, '%', '%*' + GetTotalSystemMemory.ToString);
    B := Pos('%', S) = 0;
    CACHE := Round(ExpParse.Evaluate(S));
    if not B then
      CACHE := Max(0, CACHE - GetUsedSystemMemory);
    if CACHE > Max(0, GetFreeSystemMemory - XTOOL_FREEMEM) then
      CACHE := Max(0, GetFreeSystemMemory - XTOOL_FREEMEM);
{$ELSE}
    CACHE := 0;
{$ENDIF}
    VERBOSE := ArgParse.AsBoolean('-v') and (IsLibrary = False);
    EXTCOMP := ArgParse.AsString('-e');
  finally
    ArgParse.Free;
    ExpParse.Free;
  end;
  if VERBOSE then
    Options.Threads := 1;
end;

type
  TCommonVarsEnc = record
    MemStream: TArray<TMemoryStream>;
    DataStore: TDataStore;
    MemOutput1, MemOutput2, MemOutput3: TArray<TMemoryStreamEx>;
    CurPos1, CurPos2: TArray<Int64>;
    CurTransfer: TArray<String>;
    InfoStore1: TArray<TListEx<TEncodeSI>>;
    InfoStore2: TArray<TArray<TListEx<TFutureSI>>>;
    ISIndex: TArray<Boolean>;
    StrIdx: TArray<Integer>;
    DepthIdx1, DepthIdx2: TArray<Integer>;
  end;

var
  DBInfo: TArray<TArray<TDatabase>>;
  DBCount: TArray<Integer>;
  DDInfo1: TArray<TArray<TDuplicate1>>;
  DDInfo2: TArray<Int64>;
  DDCount1: TArray<Integer>;
  DDList1: TArray<Int64>;
  DDIndex, DDInit: Integer;
  ComVars1: TArray<TCommonVarsEnc>;
  Tasks: TArray<TTask>;
  CurCodec: TArray<Byte>;
  CurDepth: TArray<Integer>;
  ThrIdx: TArray<Integer>;
  WorkStream: TArray<TMemoryStreamEx2>;
  WorkLastSize: TArray<NativeInt>;
  Scanned1, Scanned2, Processed, Helping: TArray<Boolean>;
  DoScan2: Boolean;
  LogInt: Integer;
  LogInt64: Int64;
  LogPtr: Pointer;

procedure CodecInit(Count: Integer; Method: String);
var
  I, X, Y: Integer;
  S: String;
  List: TStringDynArray;
begin
  for X := Low(Codecs) to High(Codecs) do
    for Y := Low(Codecs[X].Names) to High(Codecs[X].Names) do
      Insert(Codecs[X].Names[Y], List, Length(List));
  I := 0;
  while PrecompGetCodec(PChar(Method), I, False) <> '' do
  begin
    S := PrecompGetCodec(PChar(Method), I, False);
    if IndexText(PrecompGetCodec(PChar(Method), I, False), List) < 0 then
      raise Exception.CreateFmt(SPrecompError1, [S]);
    Inc(I);
  end;
  for X := Low(Codecs) to High(Codecs) do
  begin
    S := '';
    for Y := Low(Codecs[X].Names) to High(Codecs[X].Names) do
    begin
      I := 0;
      while PrecompGetCodec(PChar(Method), I, False) <> '' do
      begin
        if SameText(PrecompGetCodec(PChar(Method), I, False), Codecs[X].Names[Y])
        then
          S := S + PrecompGetCodec(PChar(Method), I, True) + SPrecompSep1;
        Inc(I);
      end;
    end;
    SetLength(S, Length(S) - 1);
    for I := Low(CurCodec) to High(CurCodec) do
    begin
      CurCodec[I] := X;
      CurDepth[I] := 0;
    end;
    Codecs[X].Initialised := Codecs[X].Init(PChar(S), Count, @PrecompFunctions);
  end;
end;

procedure CodecFree(Count: Integer);
var
  I, X: Integer;
begin
  for I := Low(CurCodec) to High(CurCodec) do
  begin
    CurCodec[I] := 0;
    CurDepth[I] := 0;
  end;
  for I := Low(Codecs) to High(Codecs) do
    if Codecs[I].Initialised then
    begin
      Codecs[I].Free(@PrecompFunctions);
      Codecs[I].Initialised := False;
    end;
end;

function PrecompAllocator(Instance: Integer; Size: Integer): Pointer;
var
  LSize: NativeInt;
begin
  if Size < 0 then
    LSize := WorkLastSize[Instance] + Abs(Size)
  else
    LSize := Size;
  if WorkStream[Instance].Size < LSize then
    WorkStream[Instance].Size := LSize;
  if Size < 0 then
  begin
    Result := PByte(WorkStream[Instance].Memory) + WorkLastSize[Instance];
    Inc(WorkLastSize[Instance], Abs(Size));
  end
  else
  begin
    Result := WorkStream[Instance].Memory;
    WorkLastSize[Instance] := Size;
  end;
end;

function PrecompReadFuture(Index: Integer; Position: NativeInt; Buffer: Pointer;
  Count: Integer): Integer;
var
  X: NativeInt;
begin
  Result := 0;
  with ComVars1[CurDepth[Index]] do
  begin
    if CurDepth[Index] > 0 then
    begin
      X := TDataStore2(ComVars1[CurDepth[Index]].DataStore).ActualSize(Index);
      if Position < X then
      begin
        X := Min(X - Position, Count);
        Move(TDataStore2(ComVars1[CurDepth[Index]].DataStore).Slot(Index)
          .Memory^, Buffer^, X);
        Result := X;
      end;
    end
    else
      Result := TDataStore1(ComVars1[CurDepth[Index]].DataStore).
        Read(Index, Position, Buffer^, Count);
  end;
end;

procedure PrecompLogScan1(Codec: PChar; Position: Int64;
  InSize, OutSize: Integer);
var
  S: String;
begin
  if not VERBOSE then
    exit;
  with ComVars1[CurDepth[0]] do
  begin
    if OutSize < 0 then
      S := '(%d)'
    else
      S := '(%d >> %d)';
    if (OutSize > 0) and (Position < DataStore.Size(0)) and
      (MemOutput1[0].Position - CurPos1[0] = OutSize) then
      WriteLine(Format('[%d] Actual %s stream found at %s ' + S,
        [CurDepth[0], Codec, (DataStore.Position(0) + Position).ToHexString,
        InSize, OutSize]))
    else
      WriteLine(Format('[%d] Possible %s stream located at %s ' + S,
        [CurDepth[0], Codec, (DataStore.Position(0) + Position).ToHexString,
        InSize, OutSize]));
  end;
end;

procedure PrecompLogScan2(Codec: PChar; InSize, OutSize: Integer);
var
  S: String;
begin
  if not VERBOSE then
    exit;
  if OutSize < 0 then
    S := '(%d)'
  else
    S := '(%d >> %d)';
  WriteLine(Format('[%d] Confirmed %s stream at %s ' + S, [CurDepth[0], Codec,
    LogInt64.ToHexString, InSize, OutSize]));
end;

procedure PrecompLogProcess(Codec, Method: PChar; Size1, Size2, Size3: Integer;
  Status: Boolean);
var
  S1, S2: String;
begin
  if VERBOSE then
  begin
    if Size2 < 0 then
      S1 := '(%d)'
    else if Size3 < 0 then
      S1 := '(%d >> %d)'
    else
      S1 := '(%d >> %d >> %d)';
    if Status then
      S2 := '[%d] Processed %s stream at %s ' + S1 +
        IfThen(String(Method) <> '', ' using ' + String(Method), '') +
        ' successfully'
    else
      S2 := '[%d] Processing %s stream at %s ' + S1 +
        IfThen(String(Method) <> '', ' using ' + String(Method), '') +
        ' has failed';
    WriteLine(Format(S2, [CurDepth[0], Codec, LogInt64.ToHexString, Size1,
      Size2, Size3]));
  end;
  if EXTRACT and (CurDepth[0] = 0) then
  begin
    S1 := '%s_%s.raw';
    with TFileStream.Create(ExtDir + Format(S1, [LogInt64.ToHexString, Codec]),
      fmCreate) do
      try
        WriteBuffer(LogPtr^, Size1);
      finally
        Free;
      end;
  end;
end;

procedure PrecompLogRestore(Codec, Method: PChar; Size1, Size2, Size3: Integer;
  Status: Boolean);
var
  S1, S2: String;
begin
  if not VERBOSE then
    exit;
  if Size2 < 0 then
    S1 := '(%d)'
  else if Size3 < 0 then
    S1 := '(%d >> %d)'
  else
    S1 := '(%d >> %d >> %d)';
  if Status then
    S2 := '[%d] Restored %s stream at %s ' + S1 + IfThen(String(Method) <> '',
      ' using ' + String(Method), '') + ' successfully'
  else
    S2 := '[%d] Restoring %s stream at %s ' + S1 + IfThen(String(Method) <> '',
      ' using ' + String(Method), '') + ' has failed';
  WriteLine(Format(S2, [CurDepth[0], Codec, LogInt64.ToHexString, Size1,
    Size2, Size3]));
end;

procedure PrecompLogPatch1(OldSize, NewSize, PatchSize: Integer;
  Status: Boolean);
var
  S: String;
begin
  if not VERBOSE then
    exit;
  if Status then
    S := '[%d] - Patched stream at %s (%d >> %d) [%d] successfully'
  else
    S := '[%d] - Patching stream at %s (%d >> %d) [%d] has failed';
  WriteLine(Format(S, [CurDepth[0], LogInt64.ToHexString, OldSize, NewSize,
    PatchSize]));
end;

procedure PrecompLogPatch2(OldSize, NewSize, PatchSize: Integer;
  Status: Boolean);
var
  S: String;
begin
  if not VERBOSE then
    exit;
  if Status then
    S := '[%d] - Patched stream at %s (%d >> %d) [%d] successfully'
  else
    S := '[%d] - Patching stream at %s (%d >> %d) [%d] has failed';
  WriteLine(Format(S, [CurDepth[0], LogInt64.ToHexString, OldSize, NewSize,
    PatchSize]));
end;

procedure PrecompLogReprocess(Method: PChar; Size1, Size2, Size3: Integer;
  Status: Boolean);
var
  S1, S2: String;
begin
  if VERBOSE then
  begin
    if Size2 < 0 then
      S1 := '(%d)'
    else if Size3 < 0 then
      S1 := '(%d >> %d)'
    else
      S1 := '(%d >> %d >> %d)';
    if Status then
      S2 := '[%d] Reprocessed stream at %s ' + S1 + IfThen(String(Method) <> '',
        ' using ' + String(Method), '') + ' successfully'
    else
      S2 := '[%d] Reprocessing stream at %s ' + S1 +
        IfThen(String(Method) <> '', ' using ' + String(Method), '') +
        ' has failed';
    WriteLine(Format(S2, [CurDepth[0], LogInt64.ToHexString, Size1,
      Size2, Size3]));
  end;
  if EXTRACT and (CurDepth[0] = 0) then
  begin
    S1 := '%s_%s.raw';
    with TFileStream.Create(ExtDir + Format(S1, [LogInt64.ToHexString]),
      fmCreate) do
      try
        WriteBuffer(LogPtr^, Size1);
      finally
        Free;
      end;
  end;
end;

procedure PrecompOutput1(Instance: Integer; const Buffer: Pointer;
  Size: Integer);
begin
  with ComVars1[CurDepth[Instance]] do
  begin
    if Assigned(Buffer) and (Size >= 0) then
      MemOutput1[Instance].WriteBuffer(Buffer^, Size)
    else
      MemOutput1[Instance].Position := CurPos1[Instance];
  end;
end;

function PrecompGetCodecIndex(Codec: PChar; Index: PByte;
  Option: PInteger): Boolean;
var
  I, X, Y: Integer;
  S: String;
begin
  Result := False;
  I := 0;
  while PrecompGetCodec(Codec, I, False) <> '' do
  begin
    for X := Low(Codecs) to High(Codecs) do
    begin
      for Y := Low(Codecs[X].Names) to High(Codecs[X].Names) do
        if SameText(PrecompGetCodec(Codec, I, False), Codecs[X].Names[Y]) then
        begin
          Index^ := X;
          S := PrecompGetCodec(Codec, I, True);
          if Codecs[X].Initialised then
            if Codecs[X].Parse(PChar(S), Option, @PrecompFunctions) then
            begin
              Result := True;
              break;
            end;
        end;
      if Result then
        break;
    end;
    Inc(I);
  end;
end;

procedure PrecompAddStream(Instance: Integer; Info: PStrInfo1; Codec: PChar;
  Reserved: Pointer);
var
  SI1: TEncodeSI;
  SI2: TFutureSI;
  LValid: Boolean;
  LCodec: Byte;
  LOption: Integer;
  I: Integer;
begin
  if CurDepth[Instance] > 0 then
    Inc(Info^.Position, Integer.Size);
  with ComVars1[CurDepth[Instance]] do
  begin
    if (Info^.Position < 0) then
    begin
      MemOutput1[Instance].Position := CurPos1[Instance];
      exit;
    end;
    if Codec <> '' then
    begin
      LValid := PrecompGetCodecIndex(Codec, @LCodec, @LOption);
      if not LValid then
      begin
        MemOutput1[Instance].Position := CurPos1[Instance];
        exit;
      end
      else if LCodec = CurCodec[Instance] then
        LOption := Info^.Option;
    end
    else
    begin
      LCodec := CurCodec[Instance];
      LOption := Info^.Option;
    end;
    if (Info^.NewSize > 0) and (Info^.Position < DataStore.Size(Instance)) and
      (MemOutput1[Instance].Position - CurPos1[Instance] = Info^.NewSize) and
      (MemOutput1[Instance].Position - CurPos1[Instance] < Integer.MaxValue)
    then
    begin
      AtomicIncrement(EncInfo.Count);
      SetLength(SI1.DepthStreams, 0);
      FillChar(SI1, SizeOf(TEncodeSI), 0);
      SI1.ActualPosition := Info^.Position;
      SI1.StorePosition := CurPos1[Instance];
      SI1.OldSize := Info^.OldSize;
      SI1.NewSize := Info^.NewSize;
      SI1.Resource := Info^.Resource;
      SI1.Thread := Instance;
      SI1.Codec := LCodec;
      SI1.Scan2 := False;
      SI1.Option := LOption;
      PrecompHash('xxh3_128', PByte(DataStore.Slot(Instance).Memory) +
        SI1.ActualPosition, SI1.OldSize, @SI1.Checksum, SizeOf(SI1.Checksum));
      SI1.Status := Info^.Status;
      DepthIdx1[Instance] := -1;
      DepthIdx2[Instance] := InfoStore1[Instance].Add(SI1);
    end
    else if (MemOutput1[Instance].Position - CurPos1[Instance] <
      Integer.MaxValue) then
    begin
      MemOutput1[Instance].Position := CurPos1[Instance];
      SetLength(SI2.DepthStreams, 0);
      FillChar(SI2, SizeOf(TFutureSI), 0);
      SI2.Position := DataStore.Position(Instance) + Info^.Position;
      SI2.OldSize := Info^.OldSize;
      SI2.NewSize := Info^.NewSize;
      SI2.Resource := Info^.Resource;
      SI2.Codec := LCodec;
      SI2.Scan2 := True;
      SI2.Option := LOption;
      SI2.Status := Info^.Status;
      if CurDepth[Instance] = 0 then
        I := (SI2.Position div IntArray[0]) mod IntArray[1]
      else
        I := Instance;
      ThreadSync[I].Acquire;
      try
        DepthIdx1[Instance] := I;
        DepthIdx2[Instance] := InfoStore2[I, ISIndex[I].ToInteger].Add(SI2);
      finally
        ThreadSync[I].Release;
      end;
    end;
    CurPos1[Instance] := MemOutput1[Instance].Position;
  end;
end;

procedure PrecompAddDepthStream(Instance: Integer; Position: Int64;
  OldSize, NewSize: Integer; Codec: PChar; Option, Resource: Integer);
var
  I, X: Integer;
  LValid: Boolean;
  LCodec: Byte;
  LOption: Integer;
  SI1: TEncodeSI;
  SI2: TFutureSI;
begin
  with ComVars1[CurDepth[Instance]] do
  begin
    if Codec <> '' then
    begin
      LValid := PrecompGetCodecIndex(Codec, @LCodec, @LOption);
      if not LValid then
        exit
      else if LCodec = CurCodec[Instance] then
        LOption := Option;
    end
    else
    begin
      LCodec := CurCodec[Instance];
      LOption := Option;
    end;
    if DepthIdx2[Instance] >= 0 then
    begin
      if DepthIdx1[Instance] < 0 then
      begin
        SI1 := InfoStore1[Instance][DepthIdx2[Instance]];
        X := Length(SI1.DepthStreams);
        SetLength(SI1.DepthStreams, Succ(X));
        SI1.DepthStreams[X].Position := Position;
        SI1.DepthStreams[X].OldSize := OldSize;
        SI1.DepthStreams[X].NewSize := NewSize;
        SI1.DepthStreams[X].Resource := Resource;
        SI1.DepthStreams[X].Codec := LCodec;
        SI1.DepthStreams[X].Option := LOption;
        InfoStore1[Instance][DepthIdx2[Instance]] := SI1;
      end
      else
      begin
        I := DepthIdx1[Instance];
        ThreadSync[I].Acquire;
        try
          SI2 := InfoStore2[I, ISIndex[I].ToInteger][DepthIdx2[Instance]];
          X := Length(SI2.DepthStreams);
          SetLength(SI2.DepthStreams, Succ(X));
          SI2.DepthStreams[X].Position := Position;
          SI2.DepthStreams[X].OldSize := OldSize;
          SI2.DepthStreams[X].NewSize := NewSize;
          SI2.DepthStreams[X].Resource := Resource;
          SI2.DepthStreams[X].Codec := LCodec;
          SI2.DepthStreams[X].Option := LOption;
          InfoStore2[I, ISIndex[I].ToInteger][DepthIdx2[Instance]] := SI2;
        finally
          ThreadSync[I].Release;
        end;
      end;
    end;
  end;
end;

procedure PrecompTransfer(Instance: Integer; Codec: PChar);
begin
  with ComVars1[CurDepth[Instance]] do
    CurTransfer[Instance] := String(Codec);
end;

function PrecompStorage(Instance: Integer; Size: PInteger): Pointer;
begin
  with ComVars1[CurDepth[Instance]] do
  begin
    Size^ := MemOutput1[Instance].Position - CurPos1[Instance];
    Result := PByte(MemOutput1[Instance].Memory) + CurPos1[Instance];
  end;
end;

function PrecompAddResourceEx(Data: Pointer; Size: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  GlobalSync.Acquire;
  try
    I := Length(Resources);
    SetLength(Resources, Succ(I));
    Resources[I].Name := Utils.Hash32(0, Data, Size).ToHexString;
    Resources[I].Size := Size;
    GetMem(Resources[I].Data, Resources[I].Size);
    Move(Data^, Resources[I].Data^, Resources[I].Size);
    Result := I;
  finally
    GlobalSync.Release;
  end;
end;

function CheckDB(StreamInfo: TEncodeSI; Database: PDatabase): Boolean;
var
  A: Word;
  I: Integer;
  LCount: Integer;
  DB: PDatabase;
begin
  Result := False;
  Move(StreamInfo.Checksum, A, A.Size);
  AtomicExchange(LCount, DBCount[A]);
  for I := 0 to LCount - 1 do
  begin
    DB := @DBInfo[A, I];
    if (DB^.Size = StreamInfo.OldSize) and CompareMem(@DB^.Checksum,
      @StreamInfo.Checksum, SizeOf(DB^.Checksum)) then
    begin
      if Assigned(Database) then
        Move(DB^, Database^, SizeOf(TDatabase));
      Result := True;
      break;
    end;
  end;
end;

procedure AddDB(StreamInfo: TEncodeSI);
var
  A: Word;
  I: Integer;
  DB: TDatabase;
begin
  Move(StreamInfo.Checksum, A, A.Size);
  if not CheckDB(StreamInfo, nil) then
  begin
    GlobalSync.Acquire;
    try
      DB.Size := StreamInfo.OldSize;
      DB.Codec := StreamInfo.Codec;
      DB.Option := StreamInfo.Option;
      Move(StreamInfo.Checksum, DB.Checksum, SizeOf(DB.Checksum));
      DB.Status := StreamInfo.Status;
      Insert(DB, DBInfo[A], Length(DBInfo[A]));
      Inc(DBCount[A]);
    finally
      GlobalSync.Release;
    end;
  end;
end;

function CheckDD(StreamInfo: TEncodeSI; Database: PDuplicate1;
  Index: PInteger): Boolean;
var
  A: Word;
  I: Integer;
  LCount: Integer;
  DD: PDuplicate1;
begin
  Result := False;
  Move(StreamInfo.Checksum, A, A.Size);
  LCount := DDCount1[A];
  for I := 0 to LCount - 1 do
  begin
    DD := @DDInfo1[A, I];
    if (DD^.Size = StreamInfo.OldSize) and CompareMem(@DD^.Checksum,
      @StreamInfo.Checksum, SizeOf(DD^.Checksum)) then
    begin
      if Assigned(Database) then
        Move(DD^, Database^, SizeOf(TDuplicate1));
      if Assigned(Index) then
        Index^ := I;
      Result := True;
      break;
    end;
  end;
end;

function FindDD(StreamInfo: TEncodeSI; Index1, Index2, Count: PInteger)
  : Boolean;
var
  A: Word;
  I: Integer;
  DD: PDuplicate1;
begin
  Result := False;
  if CheckDD(StreamInfo, nil, @I) then
  begin
    Move(StreamInfo.Checksum, A, A.Size);
    DD := @DDInfo1[A, I];
    if Assigned(Index1) then
      Index1^ := DD^.Index1;
    if Assigned(Index2) then
      Index2^ := DD^.Index2;
    if Assigned(Count) then
      Count^ := DD^.Count;
    Result := True;
  end;
end;

function FindOrAddDD(StreamInfo: TEncodeSI;
  Index1, Index2, Count: PInteger): Boolean;
var
  A: Word;
  I: Integer;
  DD: TDuplicate1;
  I64: Int64;
begin
  Result := False;
  Inc(DDIndex);
  Move(StreamInfo.Checksum, A, A.Size);
  if not CheckDD(StreamInfo, nil, @I) then
  begin
    Inc(DDInit);
    DD.Size := StreamInfo.OldSize;
    Move(StreamInfo.Checksum, DD.Checksum, SizeOf(DD.Checksum));
    DD.Index1 := DDIndex;
    DD.Index2 := DDInit;
    DD.Count := 0;
    I := Length(DDInfo1[A]);
    Insert(DD, DDInfo1[A], I);
    Int64Rec(I64).Words[0] := A;
    Int64Rec(I64).Hi := DDCount1[A];
    Insert(I64, DDList1, Length(DDList1));
    Inc(DDCount1[A]);
    Result := True;
  end
  else
    Inc(DDInfo1[A, I].Count);
  if Assigned(Index1) then
    Index1^ := DDInfo1[A, I].Index1;
  if Assigned(Index2) then
    Index2^ := DDInfo1[A, I].Index2;
  if Assigned(Count) then
    Count^ := DDInfo1[A, I].Count;
end;

procedure Scan1(Index, Depth: Integer);
var
  I: Integer;
  LPtr: Pointer;
  LSize, LSizeEx: NativeInt;
begin
  with ComVars1[Depth] do
    for I := Low(Codecs) to High(Codecs) do
    begin
      CurPos1[Index] := MemOutput1[Index].Position;
      CurCodec[Index] := I;
      CurDepth[Index] := Depth;
      if Depth = 0 then
      begin
        LPtr := DataStore.Slot(Index).Memory;
        LSize := DataStore.Size(Index);
        LSizeEx := DataStore.ActualSize(Index);
      end
      else
      begin
        LPtr := PByte(DataStore.Slot(Index).Memory) + Integer.Size;
        LSize := PInteger(DataStore.Slot(Index).Memory)^;
        LSizeEx := LSize;
      end;
      try
        Codecs[I].Scan1(Index, Depth, LPtr, LSize, LSizeEx, @PrecompOutput1,
          @PrecompAddStream, @PrecompFunctions);
      except
        on E: Exception do
      end;
      CurTransfer[Index] := '';
    end;
end;

procedure Scan2(Index, Depth: Integer);
var
  I, J, K: Integer;
  X: NativeInt;
  SI1: _StrInfo2;
  SI2: TFutureSI;
  SI3: TEncodeSI;
  LValid: Boolean;
  LCodec: Byte;
  LOption: Integer;
begin
  with ComVars1[Depth] do
    try
      InfoStore2[Index, (not ISIndex[Index]).ToInteger].Count := 0;
      InfoStore2[Index, ISIndex[Index].ToInteger].Sort;
      InfoStore2[Index, ISIndex[Index].ToInteger].Index := 0;
      I := InfoStore2[Index, ISIndex[Index].ToInteger].Get(SI2);
      while I >= 0 do
      begin
        if SI2.Scan2 and InRange(SI2.Position, DataStore.Position(Index),
          Pred(DataStore.Position(Index) + DataStore.Size(Index))) then
        begin
          CurPos1[Index] := MemOutput1[Index].Position;
          CurCodec[Index] := SI2.Codec;
          CurDepth[Index] := Depth;
          SI1.OldSize := SI2.OldSize;
          SI1.NewSize := SI2.NewSize;
          SI1.Resource := SI2.Resource;
          SI1.Option := SI2.Option;
          SI1.Status := SI2.Status;
          J := 0;
          X := DataStore.ActualSize(Index) -
            NativeInt(SI2.Position - DataStore.Position(Index));
          LogInt64 := SI2.Position;
          LogPtr := PByte(DataStore.Slot(Index).Memory) +
            NativeInt(SI2.Position - DataStore.Position(Index));
          try
            if (SI1.OldSize <= X) and Codecs[SI2.Codec].Scan2(Index, Depth,
              PByte(DataStore.Slot(Index).Memory) +
              NativeInt(SI2.Position - DataStore.Position(Index)), X, @SI1, @J,
              @PrecompOutput1, @PrecompFunctions) then
            begin
              LValid := True;
              if CurTransfer[Index] <> '' then
              begin
                LValid := PrecompGetCodecIndex(PChar(CurTransfer[Index]),
                  @LCodec, @LOption);
                if LValid then
                begin
                  SI2.Codec := LCodec;
                  SI1.Option := LOption;
                  if System.Pos(SPrecompSep2, CurTransfer[Index]) > 0 then
                    SI1.Status := TStreamStatus.Predicted
                  else
                    SI1.Status := TStreamStatus.None;
                end;
                CurTransfer[Index] := '';
              end;
              if LValid and InRange(SI2.Position + J, DataStore.Position(Index),
                DataStore.Position(Index) + DataStore.Size(Index)) and
                InRange(SI2.Position + J + SI1.OldSize,
                DataStore.Position(Index), DataStore.Position(Index) +
                DataStore.ActualSize(Index)) and
                (MemOutput1[Index].Position - CurPos1[Index] = SI1.NewSize) then
              begin
                AtomicIncrement(EncInfo.Count);
                SetLength(SI3.DepthStreams, 0);
                FillChar(SI3, SizeOf(TEncodeSI), 0);
                SI3.ActualPosition :=
                  NativeInt(SI2.Position - DataStore.Position(Index)) + J;
                SI3.StorePosition := CurPos1[Index];
                SI3.OldSize := SI1.OldSize;
                SI3.NewSize := SI1.NewSize;
                SI3.Resource := SI1.Resource;
                SI3.Thread := Index;
                SI3.Codec := SI2.Codec;
                SI3.Scan2 := False;
                SI3.Option := SI1.Option;
                SI3.Status := SI1.Status;
                SetLength(SI3.DepthStreams, Length(SI2.DepthStreams));
                Move(SI2.DepthStreams[0], SI3.DepthStreams[0],
                  SizeOf(TDepthStream) * Length(SI2.DepthStreams));
                SI3.DepthStreams := SI2.DepthStreams;
                PrecompHash('xxh3_128', PByte(DataStore.Slot(Index).Memory) +
                  SI3.ActualPosition, SI3.OldSize, @SI3.Checksum,
                  SizeOf(SI3.Checksum));
                InfoStore1[Index].Add(SI3);
              end
              else
                MemOutput1[Index].Position := CurPos1[Index];
            end
            else
            begin
              LValid := False;
              if CurTransfer[Index] <> '' then
              begin
                LValid := PrecompGetCodecIndex(PChar(CurTransfer[Index]),
                  @LCodec, @LOption);
                if LValid then
                begin
                  SI2.Codec := LCodec;
                  SI2.Option := LOption;
                  if System.Pos(SPrecompSep2, CurTransfer[Index]) > 0 then
                    SI2.Status := TStreamStatus.Predicted
                  else
                    SI2.Status := TStreamStatus.None;
                end;
                CurTransfer[Index] := '';
              end;
              MemOutput1[Index].Position := CurPos1[Index];
              if LValid then
                continue;
            end;
          except
            on E: Exception do
          end;
        end
        else
          InfoStore2[Index, (not ISIndex[Index]).ToInteger].Add(SI2);
        I := InfoStore2[Index, ISIndex[Index].ToInteger].Get(SI2);
      end;
    finally
      ISIndex[Index] := not ISIndex[Index];
    end;
end;

function Process(ThreadIndex, StreamIndex, Index, Depth: Integer;
  Reassigned: Boolean = False): Boolean;
var
  SI1: _StrInfo2;
  SI2: TEncodeSI;
  SI3: TFutureSI;
  DBTyp: TDatabase;
  DBBool: Boolean;
  Errored: Boolean;
  LValid: Boolean;
  LCodec: Byte;
  LOption: Integer;
  I: Integer;
  function Reproc(Method: String): Boolean;
  var
    Buffer: Pointer;
    Res: Integer;
    C1, C2: XXH128_hash_t;
  begin
    Result := False;
    if Depth > 0 then
      exit;
    with ComVars1[Depth] do
    begin
      Buffer := PrecompAllocator(ThreadIndex, SI1.NewSize);
      Res := PrecompCompress(PChar(Method),
        PByte(MemOutput1[ThreadIndex].Memory) + SI2.StorePosition, SI1.NewSize,
        Buffer, SI1.NewSize, nil, 0);
      if (Res > 0) and (Res + Int64.Size < SI1.OldSize) then
      begin
        PrecompHash('xxh3_128', PByte(MemOutput1[ThreadIndex].Memory) +
          SI2.StorePosition, SI1.NewSize, @C1, SizeOf(C1));
        PrecompDecompress(PChar(Method), Buffer, SI1.OldSize,
          PByte(MemOutput1[ThreadIndex].Memory) + SI2.StorePosition,
          SI1.NewSize, nil, 0);
        PrecompHash('xxh3_128', PByte(MemOutput1[ThreadIndex].Memory) +
          SI2.StorePosition, SI1.NewSize, @C2, SizeOf(C2));
        if CompareMem(@C1, @C2, SizeOf(C1)) then
        begin
          ThreadSync[ThreadIndex].Acquire;
          try
            Move(Buffer^, (PByte(DataStore.Slot(ThreadIndex).Memory) +
              SI2.ActualPosition)^, Res);
            FillChar((PByte(DataStore.Slot(ThreadIndex).Memory) +
              SI2.ActualPosition + Res)^, SI1.OldSize - Res, 0);
            Int64Rec(PInt64(PByte(DataStore.Slot(ThreadIndex).Memory) +
              SI2.ActualPosition + SI1.OldSize - Int64.Size)^).Lo :=
              SI1.OldSize;
            Int64Rec(PInt64(PByte(DataStore.Slot(ThreadIndex).Memory) +
              SI2.ActualPosition + SI1.OldSize - Int64.Size)^).Hi := Res;
            Result := True;
          finally
            ThreadSync[ThreadIndex].Release;
          end;
        end;
      end;
      PrecompLogReprocess(PChar(Method), SI1.OldSize, SI1.NewSize, Res, Result);
    end;
  end;

begin
  Result := False;
  with ComVars1[Depth] do
  begin
    SI2 := InfoStore1[ThreadIndex][StreamIndex];
    SI1.OldSize := SI2.OldSize;
    SI1.NewSize := SI2.NewSize;
    SI1.Resource := SI2.Resource;
    SI1.Option := SI2.Option;
    SI1.Status := SI2.Status;
    LogInt64 := DataStore.Position(ThreadIndex) + SI2.ActualPosition;
    LogPtr := PByte(DataStore.Slot(ThreadIndex).Memory) + SI2.ActualPosition;
    if UseDB and (SI2.Codec > 2) then
    begin
      DBBool := CheckDB(SI2, @DBTyp);
      if DBBool and (SI2.Codec = DBTyp.Codec) then
      begin
        if DBTyp.Status = TStreamStatus.Invalid then
          exit
        else
        begin
          SI1.Option := DBTyp.Option;
          SI1.Status := TStreamStatus.Database;
        end;
      end;
    end;
    CurPos1[Index] := MemOutput1[Index].Position;
    CurCodec[Index] := SI2.Codec;
    CurDepth[Index] := Depth;
    try
      if NOVERIFY and not(SI2.Codec in [5]) then
        Result := True
      else if (REPROCESS <> '') and not(SI2.Codec in [5, 13]) then
      begin
        Result := False;
        if Reproc(REPROCESS) then
          AtomicIncrement(EncInfo.Processed);
      end
      else
        Result := Codecs[SI2.Codec].Process(Index, Depth,
          PByte(DataStore.Slot(ThreadIndex).Memory) + SI2.ActualPosition,
          PByte(MemOutput1[ThreadIndex].Memory) + SI2.StorePosition, @SI1,
          @PrecompOutput1, @PrecompFunctions);
    except
      on E: Exception do
        Result := False;
    end;
    LValid := False;
    if (Result = False) and (REASSIGN <> '') and (Reassigned = False) then
    begin
      LValid := PrecompGetCodecIndex(PChar(REASSIGN), @LCodec, @LOption);
      if LValid then
      begin
        SI2.Codec := LCodec;
        SI2.Option := LOption;
        if System.Pos(SPrecompSep2, REASSIGN) > 0 then
          SI2.Status := TStreamStatus.Predicted
        else
          SI2.Status := TStreamStatus.None;
        InfoStore1[ThreadIndex][StreamIndex] := SI2;
      end;
    end;
    if (Result = False) and (CurTransfer[Index] <> '') and (LValid = False) then
    begin
      LValid := PrecompGetCodecIndex(PChar(CurTransfer[Index]), @LCodec,
        @LOption);
      if LValid then
      begin
        SI2.Codec := LCodec;
        SI2.Option := LOption;
        if System.Pos(SPrecompSep2, CurTransfer[Index]) > 0 then
          SI2.Status := TStreamStatus.Predicted
        else
          SI2.Status := TStreamStatus.None;
        InfoStore1[ThreadIndex][StreamIndex] := SI2;
      end;
    end;
    CurTransfer[Index] := '';
    if LValid then
    begin
      MemOutput1[Index].Position := CurPos1[Index];
      Result := Process(ThreadIndex, StreamIndex, Index, Depth, REASSIGN <> '');
      exit;
    end;
    if UseDB then
      if not DBBool then
      begin
        if Result then
        begin
          SI2.Option := SI1.Option;
          SI2.Status := TStreamStatus.Predicted
        end
        else
          SI2.Status := TStreamStatus.Invalid;
        AddDB(SI2);
      end;
    if Result then
    begin
      AtomicIncrement(EncInfo.Processed);
      SI2.OldSize := SI1.OldSize;
      SI2.NewSize := SI1.NewSize;
      SI2.Resource := SI1.Resource;
      SI2.Option := SI1.Option;
      SI2.Status := TStreamStatus(SuccessStatus);
      SI2.ExtPosition := CurPos1[Index];
      SI2.ExtSize := MemOutput1[Index].Position - CurPos1[Index];
      SI2.ExtThread := Index;
      InfoStore1[ThreadIndex][StreamIndex] := SI2;
      CurPos2[Index] := MemOutput2[Index].Position;
      if Succ(Depth) < Length(ComVars1) then
      begin
        with ComVars1[Succ(Depth)].DataStore as TDataStore2 do
        begin
          Reset(Index);
          Load(Index, @SI2.NewSize, SI2.NewSize.Size);
          Load(Index, PByte(MemOutput1[ThreadIndex].Memory) + SI2.StorePosition,
            SI2.NewSize);
          Load(Index, @SI2.ExtSize, SI2.ExtSize.Size);
          Load(Index, PByte(MemOutput1[Index].Memory) + SI2.ExtPosition,
            SI2.ExtSize);
        end;
        ComVars1[Succ(Depth)].InfoStore2[Index, 0].Count := 0;
        ComVars1[Succ(Depth)].InfoStore2[Index, 1].Count := 0;
        ComVars1[Succ(Depth)].ISIndex[Index] := True;
        for I := Low(SI2.DepthStreams) to High(SI2.DepthStreams) do
        begin
          SetLength(SI3.DepthStreams, 0);
          FillChar(SI3, SizeOf(TFutureSI), 0);
          SI3.Position := SI2.NewSize.Size + SI2.DepthStreams[I].Position;
          SI3.OldSize := SI2.DepthStreams[I].OldSize;
          SI3.NewSize := SI2.DepthStreams[I].NewSize;
          SI3.Resource := SI2.DepthStreams[I].Resource;
          SI3.Codec := SI2.DepthStreams[I].Codec;
          SI3.Scan2 := True;
          SI3.Option := SI2.DepthStreams[I].Option;
          ComVars1[Succ(Depth)].InfoStore2
            [Index, ISIndex[Index].ToInteger].Add(SI3);
        end;
        MemOutput3[Index].Position := 0;
        if EncData(nil, MemOutput3[Index], Index, Succ(Depth)) then
        begin
          ThreadSync[Index].Acquire;
          try
            MemOutput2[Index].WriteBuffer(MemOutput3[Index].Memory^,
              MemOutput3[Index].Position);
          finally
            ThreadSync[Index].Release;
          end;
          SI2.StorePosition := CurPos2[Index];
          SI2.NewSize := MemOutput2[Index].Position - CurPos2[Index];
          SI2.Thread := Index;
          SI2.ExtPosition := 0;
          SI2.ExtSize := -1;
          SI2.ExtThread := 0;
          InfoStore1[ThreadIndex][StreamIndex] := SI2;
          CurPos2[Index] := MemOutput2[Index].Position;
        end;
      end;
    end
    else
      MemOutput1[Index].Position := CurPos1[Index];
  end;
end;

procedure EncThreadEx(ThreadIndex, Index, Depth: IntPtr);
var
  X: Integer;
begin
  with ComVars1[Depth] do
  begin
    try
      X := AtomicIncrement(StrIdx[ThreadIndex]);
      while X < InfoStore1[ThreadIndex].Count do
      begin
        Process(ThreadIndex, X, Index, Depth);
        X := AtomicIncrement(StrIdx[ThreadIndex]);
      end;
    finally
      ThreadSync[Index].Acquire;
      try
        Helping[Index] := False;
      finally
        ThreadSync[Index].Release;
      end;
    end;
  end;
end;

procedure EncCallThreads(ThreadIndex, Index, Depth: Integer);
var
  I: Integer;
begin
  for I := Low(Tasks) to High(Tasks) do
    if I <> Index then
    begin
      ThreadSync[I].Acquire;
      try
        if (Tasks[I].Status = TThreadStatus.tsReady) and (Processed[I] = True)
          and (Helping[I] = False) then
        begin
          Helping[I] := True;
          Tasks[I].Update(ThreadIndex, I, Depth);
          Tasks[I].Perform(EncThreadEx);
          Tasks[I].Start;
        end;
      finally
        ThreadSync[I].Release;
      end;
    end;
end;

procedure EncThread(Y, W: IntPtr);

  function GetIndex: Integer;
  var
    I: Integer;
  begin
    if BoolArray(Processed, True) then
    begin
      Result := -2;
      exit;
    end
    else
      Result := -1;
    for I := Low(Scanned2) to High(Scanned2) do
    begin
      if (Scanned2[I] = True) and (Processed[I] = False) then
      begin
        Result := I;
        break;
      end;
    end;
  end;

var
  X, Z: Integer;
begin
  with ComVars1[W] do
  begin
    if InRange(Y, Low(InfoStore1), High(InfoStore1)) then
    begin
      if VERBOSE then
        WriteLine(Format('[%d] Performing scan from block %s to %s (%d)',
          [W, DataStore.Position(0).ToHexString,
          (DataStore.Position(0) + Pred(DataStore.Size(0))).ToHexString,
          DataStore.Size(0)]));
      Scan1(Y, W);
      if VERBOSE then
        WriteLine('');
      if W = 0 then
      begin
        Scanned1[Y] := True;
        if DoScan2 then
          while not BoolArray(Scanned1, True) do
            Sleep(10);
      end;
      if DoScan2 then
        Scan2(Y, W);
      InfoStore1[Y].Sort;
      if W = 0 then
        Scanned2[Y] := True;
    end;
    while True do
    begin
      if W = 0 then
      begin
        Z := GetIndex;
        while Z = -1 do
        begin
          Sleep(10);
          Z := GetIndex;
        end;
        ThrIdx[Y] := Z;
        if Z < -1 then
          break;
      end
      else
        Z := Y;
      if VERBOSE and (InfoStore1[Z].Count > 0) then
        WriteLine(Format('[%d] Processing streams on block %s to %s (%d)',
          [W, DataStore.Position(0).ToHexString,
          (DataStore.Position(0) + Pred(DataStore.Size(0))).ToHexString,
          DataStore.Size(0)]));
      X := AtomicIncrement(StrIdx[Z]);
      while X < InfoStore1[Z].Count do
      begin
        if (Succ(W) = Length(ComVars1)) and (Length(Tasks) > 1) then
          EncCallThreads(Z, Y, W);
        Process(Z, X, Y, W);
        if W = 0 then
        begin
          Z := GetIndex;
          while Z = -1 do
          begin
            Sleep(10);
            Z := GetIndex;
          end;
          ThrIdx[Y] := Z;
          if Z < -1 then
            break;
        end;
        X := AtomicIncrement(StrIdx[Z]);
      end;
      while not BoolArray(Helping, False) do
        Sleep(10);
      if VERBOSE and (InfoStore1[Z].Count > 0) then
        WriteLine('');
      if W = 0 then
      begin
        if Z < -1 then
          break;
        if X >= InfoStore1[Z].Count then
          Processed[Z] := True;
      end
      else
        break;
    end;
  end;
end;

procedure EncInit(Input, Output: TStream; Options: PEncodeOptions);
var
  UI32: UInt32;
  I, J, K: Integer;
  B: Byte;
  W: Word;
  Bytes: TBytes;
  NI: NativeInt;
  S: String;
  DupMethod: Boolean;
begin
  GlobalSync := TCriticalSection.Create;
  _Init(Options^.Threads);
  SetLength(ThreadSync, Options^.Threads);
  for I := Low(ThreadSync) to High(ThreadSync) do
    ThreadSync[I] := TCriticalSection.Create;
  I := XTOOL_PRECOMP;
  if REPROCESS = '' then
    Output.WriteBuffer(I, I.Size);
  if UseDB then
  begin
    SetLength(DBInfo, $10000);
    SetLength(DBCount, $10000);
    for I := Low(DBInfo) to High(DBInfo) do
      DBCount[I] := 0;
  end;
  if StoreDD > -2 then
  begin
    SetLength(DDInfo1, $10000);
    SetLength(DDInfo2, 0);
    SetLength(DDCount1, $10000);
    SetLength(DDList1, 0);
    for I := Low(DDInfo1) to High(DDInfo1) do
      DDCount1[I] := 0;
    DDIndex := -1;
    DDInit := -1;
  end;
  SetLength(Tasks, Options^.Threads);
  SetLength(CurCodec, Options^.Threads);
  SetLength(CurDepth, Options^.Threads);
  SetLength(ThrIdx, Options^.Threads);
  SetLength(WorkStream, Options^.Threads);
  SetLength(WorkLastSize, Options^.Threads);
  for I := Low(Tasks) to High(Tasks) do
  begin
    if Length(Tasks) > 1 then
    begin
      Tasks[I] := TTask.Create;
      Tasks[I].Priority := ThrPty;
    end;
    SetPriorityClass(GetCurrentProcess, CaseInt(Integer(ThrPty),
      [IDLE_PRIORITY_CLASS, IDLE_PRIORITY_CLASS, BELOW_NORMAL_PRIORITY_CLASS,
      NORMAL_PRIORITY_CLASS, ABOVE_NORMAL_PRIORITY_CLASS, HIGH_PRIORITY_CLASS,
      REALTIME_PRIORITY_CLASS]));
    SetThreadPriority(GetCurrentThread, CaseInt(Integer(ThrPty),
      [THREAD_PRIORITY_IDLE, THREAD_PRIORITY_LOWEST,
      THREAD_PRIORITY_BELOW_NORMAL, THREAD_PRIORITY_NORMAL,
      THREAD_PRIORITY_ABOVE_NORMAL, THREAD_PRIORITY_HIGHEST,
      THREAD_PRIORITY_TIME_CRITICAL]));
    WorkStream[I] := TMemoryStreamEx2.Create;
    WorkLastSize[I] := 0;
  end;
  if Options^.LowMem then
    I := 1
  else
    I := Options^.Threads;
  SetLength(Scanned1, I);
  SetLength(Scanned2, I);
  SetLength(Processed, I);
  SetLength(Helping, I);
  SetLength(ComVars1, Options^.Depth);
  for J := Low(ComVars1) to High(ComVars1) do
    with ComVars1[J] do
    begin
      SetLength(MemStream, Options^.Threads);
      SetLength(MemOutput1, Options^.Threads);
      SetLength(MemOutput2, Options^.Threads);
      SetLength(MemOutput3, Options^.Threads);
      SetLength(CurPos1, Options^.Threads);
      SetLength(CurPos2, Options^.Threads);
      SetLength(CurTransfer, Options^.Threads);
      if Options^.LowMem and (J = 0) then
        I := 1
      else
        I := Options^.Threads;
      SetLength(InfoStore1, I);
      SetLength(InfoStore2, I, 2);
      SetLength(ISIndex, I);
      SetLength(StrIdx, I);
      SetLength(DepthIdx1, I);
      SetLength(DepthIdx2, I);
      for I := Low(Tasks) to High(Tasks) do
      begin
        if (J = 0) and (I > 0) then
          MemStream[I] := MemStream[0]
        else
          MemStream[I] := TMemoryStream.Create;
        MemOutput1[I] := TMemoryStreamEx.Create;
        MemOutput2[I] := TMemoryStreamEx.Create;
        MemOutput3[I] := TMemoryStreamEx.Create;
      end;
      for I := Low(InfoStore1) to High(InfoStore1) do
      begin
        InfoStore1[I] := TListEx<TEncodeSI>.Create(EncodeSICmp);
        for K := Low(InfoStore2[I]) to High(InfoStore2[I]) do
          InfoStore2[I, K] := TListEx<TFutureSI>.Create(FutureSICmp);
        ISIndex[I] := False;
      end;
      if J = 0 then
      begin
        DataStore := TDataStore1.Create(Input, True, Length(InfoStore1),
          Options^.ChunkSize,
          LowerCase(ChangeFileExt(ExtractFileName(Utils.GetModuleName),
          '_' + Random($7FFFFFFF).ToHexString + '-storage.tmp')));
        IntArray[0] := Options^.ChunkSize;
        IntArray[1] := I;
      end
      else
        DataStore := TDataStore2.Create(Length(InfoStore1));
    end;
  CodecInit(Options^.Threads, Options^.Method);
  DBFile := ExpandPath(Options^.DBaseFile);
  if FileExists(DBFile) then
  begin
    with TFileStream.Create(DBFile, fmShareDenyNone) do
    begin
      Position := 0;
      if WorkStream[0].Size < Size then
        WorkStream[0].Size := Size;
      ReadBuffer(WorkStream[0].Memory^, Size);
      Free;
    end;
    with WorkStream[0] do
    begin
      Position := 0;
      while Position < Size do
      begin
        ReadBuffer(W, W.Size);
        ReadBuffer(J, J.Size);
        DBCount[W] := J;
        SetLength(DBInfo[W], J);
        for K := 0 to J - 1 do
          ReadBuffer(DBInfo[W, K], SizeOf(TDatabase));
      end;
    end;
  end;
  ExtDir := IncludeTrailingBackSlash(Options^.ExtractDir);
  if REPROCESS = '' then
    Output.WriteBuffer(Options^.Depth, Options^.Depth.Size);
  if REASSIGN <> '' then
    AddMethod(REASSIGN);
  DoScan2 := True;
  for J := 0 to ExternalMethods.Count - 1 do
  begin
    I := 0;
    while PrecompGetCodec(PChar(Options^.Method), I, False) <> '' do
    begin
      if PrecompGetCodec(PChar(S), I, False) = ExternalMethods[J] then
      begin
        DoScan2 := True;
        break;
      end;
      Inc(I);
    end;
    if DoScan2 then
      break;
  end;
  S := '';
  I := 0;
  while PrecompGetCodec(PChar(Options^.Method), I, False) <> '' do
  begin
    if (IndexText(PrecompGetCodec(PChar(Options^.Method), I, False),
      PrecompINI.Codec.Names) < 0) and
      (IndexText(PrecompGetCodec(PChar(Options^.Method), I, False),
      PrecompINIEx.Codec.Names) < 0) and
      (IndexText(PrecompGetCodec(PChar(Options^.Method), I, False),
      PrecompSearch.Codec.Names) < 0) then
    begin
      if S = '' then
        S := PrecompGetCodec(PChar(Options^.Method), I, True)
      else
        S := S + SPrecompSep1 + PrecompGetCodec(PChar(Options^.Method),
          I, True);
    end;
    Inc(I);
  end;
  for J := 0 to ExternalMethods.Count - 1 do
  begin
    DupMethod := False;
    I := 0;
    while PrecompGetCodec(PChar(S), I, False) <> '' do
    begin
      DupMethod := PrecompGetCodec(PChar(S), I, False) = ExternalMethods[J];
      if DupMethod then
        break;
      Inc(I);
    end;
    if not DupMethod then
      if S = '' then
        S := ExternalMethods[J]
      else
        S := S + SPrecompSep1 + ExternalMethods[J];
  end;
  if REPROCESS = '' then
  begin
    Bytes := BytesOf(S);
    B := Length(Bytes);
    Output.WriteBuffer(B, B.Size);
    Output.WriteBuffer(Bytes[0], B);
    I := Length(Resources);
    Output.WriteBuffer(I, I.Size);
    for J := Low(Resources) to High(Resources) do
    begin
      Bytes := BytesOf(Resources[J].Name);
      B := Length(Bytes);
      Output.WriteBuffer(B, B.Size);
      Output.WriteBuffer(Bytes[0], B);
      Output.WriteBuffer(Resources[J].Size, Resources[J].Size.Size);
      Output.WriteBuffer(Resources[J].Data^, Resources[J].Size);
    end;
    ResCount := Length(Resources);
    Output.WriteBuffer(StoreDD, StoreDD.Size);
  end;
end;

procedure EncFree;
var
  UI32: UInt32;
  I, J, K: Integer;
begin
  EncFreed := True;
  if Length(Tasks) > 1 then
    WaitForAll(Tasks);
  CodecFree(Length(Tasks));
  for J := Low(ComVars1) to High(ComVars1) do
    with ComVars1[J] do
    begin
      for I := Low(Tasks) to High(Tasks) do
      begin
        MemOutput1[I].Free;
        MemOutput2[I].Free;
        MemOutput3[I].Free;
        if (J = 0) and (I > 0) then
          continue;
        MemStream[I].Free;
      end;
      for I := Low(InfoStore1) to High(InfoStore1) do
      begin
        InfoStore1[I].Free;
        for K := Low(InfoStore2[I]) to High(InfoStore2[I]) do
          InfoStore2[I, K].Free;
      end;
      DataStore.Free;
    end;
  for I := Low(Tasks) to High(Tasks) do
  begin
    if Length(Tasks) > 1 then
      Tasks[I].Free;
    WorkStream[I].Free;
  end;
  FreeResources;
  _Free;
  GlobalSync.Free;
  for I := Low(ThreadSync) to High(ThreadSync) do
    ThreadSync[I].Free;
end;

function EncData(Input, Output: TStream; Index, Depth: Integer): Boolean;

  function FSMode(OpenAndUse: Boolean): Word;
  begin
    if OpenAndUse then
      Result := fmOpenReadWrite or fmShareDenyNone
    else
      Result := fmCreate;
  end;

var
  TempOutput, LStream: TStream;
  StreamInfo: TEncodeSI;
  StreamHeader: TStreamHeader;
  StreamCount1, StreamCount2: Integer;
  BlockSize: Int64;
  UI32: UInt32;
  I, J, K, L, X, Y, Z: Integer;
  S, T: String;
  W: Word;
  I64: Int64;
  LastStream, LastPos: Int64;
  LastIndex: Integer;
  CurrSize: Cardinal;
  DupBool: Boolean;
  DupIdx1, DupIdx2, DupIdx3, DupCount: Integer;
  DupTyp1: TDuplicate2;
  DupTyp2: TDuplicate3;
  ErrStream: TStringStream;
  LOutput, LCache: TStream;
  List: TStringDynArray;
  procedure SaveResources;
  var
    C, D: Integer;
    B: Byte;
    Bytes: TBytes;
  begin
    if Depth = 0 then
    begin
      GlobalSync.Acquire;
      try
        C := Length(Resources) - ResCount;
        TempOutput.WriteBuffer(C, C.Size);
        for D := ResCount to High(Resources) do
        begin
          Bytes := BytesOf(Resources[D].Name);
          B := Length(Bytes);
          TempOutput.WriteBuffer(B, B.Size);
          TempOutput.WriteBuffer(Bytes[0], B);
          TempOutput.WriteBuffer(Resources[D].Size, Resources[D].Size.Size);
          TempOutput.WriteBuffer(Resources[D].Data^, Resources[D].Size);
        end;
        ResCount := Length(Resources);
      finally
        GlobalSync.Release;
      end;
    end;
  end;

  procedure UpdateInfo;
  begin
    if NULLOUT then
      if StoreDD > 0 then
        EncInfo.SrepSize := TProcessStream(TBufferedStream(LOutput)
          .Instance).OutSize;
    if COMPRESS = 1 then
      EncInfo.CompSize := TLZMACompressStream(Output).OutSize
    else if COMPRESS = 2 then
      EncInfo.CompSize := TProcessStream(TBufferedStream(Output)
        .Instance).OutSize;
  end;

begin
  if (Depth = 0) then
  begin
    LCache := nil;
    I64 := CACHE div 2;
    if (I64 > 0) and ((NULLOUT = False) or (COMPRESS > 0)) then
    begin
      LCache := TMemoryStreamEx.Create(True, GetMemory(I64), I64);
      LCache.Size := I64;
    end;
    ErrStream := TStringStream.Create;
    if NULLOUT then
    begin
      if StoreDD > 0 then
      begin
        T := ReplaceText(SrepCfg, SPrecompSep3, SPrecompSep2);
        if (T <> '') then
        begin
          List := DecodeStr(T, SPrecompSep2);
          T := '';
          for I := Low(List) to High(List) do
            T := T + ' -' + List[I];
        end;
        LOutput := TBufferedStream.Create
          (TProcessStream.Create(ExpandPath(PluginsPath + SREPEXE, True),
          '-m' + StoreDD.ToString + T + ' - -', GetCurrentDir, nil, Output,
          ErrStream), False, XTOOL_BSIZE);
        TProcessStream(TBufferedStream(LOutput).Instance).Execute;
      end
      else
        LOutput := Output;
    end
    else if StoreDD > -2 then
      LOutput := TBufferedStream.Create
        (TFileStream.Create
        (LowerCase(ChangeFileExt(ExtractFileName(Utils.GetModuleName),
        '-dd.tmp')), fmCreate or fmShareDenyNone), False, XTOOL_BSIZE)
    else
      LOutput := Output;
    TempOutput := TCacheWriteStream.Create(LOutput, LCache);
  end
  else
    TempOutput := Output;
  if NULLOUT then
    LStream := TempOutput
  else
    LStream := Output;
  Result := False;
  DupIdx1 := 0;
  with ComVars1[Depth] do
  begin
    LastStream := 0;
    if Depth = 0 then
    begin
      StreamCount2 := 0;
      TDataStore1(DataStore).Load;
    end;
    while not DataStore.Done do
    begin
      if (Depth = 0) and (Length(Tasks) > 1) then
        if IsErrored(Tasks) then
          for I := Low(Tasks) to High(Tasks) do
            Tasks[I].RaiseLastError;
      for I := Low(InfoStore1) to High(InfoStore1) do
      begin
        if (Depth > 0) and (I <> Index) then
          continue;
        InfoStore1[I].Count := 0;
        StrIdx[I] := -1;
        DepthIdx1[I] := -1;
        DepthIdx2[I] := -1;
        if Depth = 0 then
        begin
          Scanned1[I] := False;
          Scanned2[I] := False;
          Processed[I] := False;
          Helping[I] := False;
        end;
      end;
      for I := Low(Tasks) to High(Tasks) do
      begin
        if (Depth > 0) and (I <> Index) then
          continue;
        if Depth = 0 then
          ThrIdx[I] := 0;
        MemOutput1[I].Position := 0;
        MemOutput2[I].Position := 0;
        CurPos1[I] := 0;
        CurPos2[I] := 0;
        CurTransfer[I] := '';
        if (Depth = 0) and (Length(Tasks) > 1) then
        begin
          Tasks[I].Update(I, Depth);
          Tasks[I].Perform(EncThread);
          Tasks[I].Start;
        end
        else
          EncThread(Index, Depth);
      end;
      for I := Low(InfoStore1) to High(InfoStore1) do
      begin
        if Depth = 0 then
        begin
          Inc(EncInfo.InSize, TDataStore1(DataStore).Size(I));
          while Processed[I] = False do
          begin
            if Length(Tasks) > 1 then
              if IsErrored(Tasks) then
                for X := Low(Tasks) to High(Tasks) do
                  Tasks[X].RaiseLastError;
            Sleep(10);
          end;
          for J := Low(ThrIdx) to High(ThrIdx) do
            while ThrIdx[J] = I do
            begin
              if Length(Tasks) > 1 then
                if IsErrored(Tasks) then
                  for X := Low(Tasks) to High(Tasks) do
                    Tasks[X].RaiseLastError;
              Sleep(10);
            end;
        end
        else if I <> Index then
          continue;
        LastIndex := 0;
        repeat
          LastPos := LastStream;
          MemStream[I].Position := 0;
          StreamCount1 := 0;
          BlockSize := 0;
          CurrSize := 0;
          MemStream[I].WriteBuffer(StreamCount1, StreamCount1.Size);
          MemStream[I].WriteBuffer(BlockSize, BlockSize.Size);
          InfoStore1[I].Index := LastIndex;
          J := InfoStore1[I].Get(StreamInfo);
          while J >= 0 do
          begin
            if (Depth = 0) and (StreamCount1 > 0) and
              (CurrSize + Max(StreamInfo.OldSize, StreamInfo.NewSize) >=
              DecodeMemBlock) then
            begin
              InfoStore1[I].Index := InfoStore1[I].Index - 1;
              break;
            end;
            if (Integer(StreamInfo.Status) <> SuccessStatus) or
              (LastStream > StreamInfo.ActualPosition) or
              (StreamInfo.ActualPosition >= DataStore.Size(I)) then
            begin
              if LastStream > StreamInfo.ActualPosition then
              begin
                if StreamInfo.Status = TStreamStatus(SuccessStatus) then
                  AtomicDecrement(EncInfo.Processed);
                AtomicDecrement(EncInfo.Count);
              end;
              InfoStore1[I].Delete(J);
            end
            else
            begin
              Inc(StreamCount1);
              if Depth = 0 then
              begin
                Inc(StreamCount2);
                Inc(EncInfo.DupSize0, StreamInfo.NewSize + StreamInfo.ExtSize);
              end;
              DupBool := False;
              if (Depth = 0) and (StoreDD > -2) then
                DupBool := not FindOrAddDD(StreamInfo, @DupIdx2, @DupIdx3,
                  @DupCount);
              if DupBool then
              begin
                Int64Rec(I64).Lo := StreamCount2 - 1;
                Int64Rec(I64).Hi := DupIdx3;
                Insert(I64, DDInfo2, Length(DDInfo2));
                Inc(EncInfo.DupCount);
                if DupCount = 1 then
                  Inc(EncInfo.DecMem2, StreamInfo.OldSize);
                Inc(EncInfo.DupSize1, StreamInfo.OldSize);
                Inc(EncInfo.DupSize2, StreamInfo.NewSize + StreamInfo.ExtSize);
                FillChar(StreamHeader, SizeOf(TStreamHeader), 0);
                StreamHeader.Kind := DUPLICATED_STREAM;
                StreamHeader.Option := DupIdx2;
              end
              else
              begin
                StreamHeader.Kind := DEFAULT_STREAM;
                if StreamInfo.ExtSize > 0 then
                  StreamHeader.Kind := StreamHeader.Kind or EXTENDED_STREAM;
                if StreamInfo.ExtSize < 0 then
                  StreamHeader.Kind := StreamHeader.Kind or NESTED_STREAM;
                StreamHeader.OldSize := StreamInfo.OldSize;
                StreamHeader.NewSize := StreamInfo.NewSize;
                StreamHeader.Resource := StreamInfo.Resource;
                if StreamInfo.ExtSize > 0 then
                begin
                  Inc(StreamHeader.NewSize, StreamInfo.ExtSize);
                  Inc(StreamHeader.NewSize, StreamInfo.ExtSize.Size);
                end;
                StreamHeader.Codec := StreamInfo.Codec;
                StreamHeader.Option := StreamInfo.Option;
                Inc(BlockSize, StreamHeader.NewSize);
                EncInfo.DecMem0 := Max(EncInfo.DecMem0,
                  Max(StreamHeader.OldSize, StreamHeader.NewSize));
                Inc(CurrSize, Max(StreamHeader.OldSize, StreamHeader.NewSize));
              end;
              MemStream[I].WriteBuffer(StreamHeader, SizeOf(TStreamHeader));
              LastStream := Int64(StreamInfo.ActualPosition) +
                StreamInfo.OldSize;
            end;
            if (Depth = 0) and (CurrSize >= DecodeMemBlock) then
              break;
            J := InfoStore1[I].Get(StreamInfo);
          end;
          EncInfo.DecMem1 := Max(EncInfo.DecMem1, CurrSize);
          if InfoStore1[I].Count > 0 then
            Result := True
          else if Depth > 0 then
            exit;
          I64 := MemStream[I].Position;
          MemStream[I].Position := 0;
          MemStream[I].WriteBuffer(StreamCount1, StreamCount1.Size);
          MemStream[I].WriteBuffer(BlockSize, BlockSize.Size);
          if REPROCESS = '' then
          begin
            SaveResources;
            TempOutput.WriteBuffer(MemStream[I].Memory^, I64);
          end;
          if Depth = 0 then
            Inc(EncInfo.InflSize, I64);
          InfoStore1[I].Index := LastIndex;
          J := InfoStore1[I].Get(StreamInfo);
          while J >= 0 do
          begin
            DupBool := False;
            if (Depth = 0) and (StoreDD > -2) then
              DupBool := FindDD(StreamInfo, @DupIdx2, nil, @DupCount);
            if (DupBool = False) or (DupIdx1 = DupIdx2) then
            begin
              if StreamInfo.ExtSize < 0 then
              begin
                ThreadSync[StreamInfo.Thread].Acquire;
                try
                  TempOutput.WriteBuffer
                    ((PByte(MemOutput2[StreamInfo.Thread].Memory) +
                    StreamInfo.StorePosition)^, StreamInfo.NewSize);
                finally
                  ThreadSync[StreamInfo.Thread].Release;
                end;
              end
              else
                TempOutput.WriteBuffer
                  ((PByte(MemOutput1[StreamInfo.Thread].Memory) +
                  StreamInfo.StorePosition)^, StreamInfo.NewSize);
              if Depth = 0 then
                Inc(EncInfo.InflSize, StreamInfo.NewSize);
              if StreamInfo.ExtSize > 0 then
              begin
                TempOutput.WriteBuffer
                  ((PByte(MemOutput1[StreamInfo.ExtThread].Memory) +
                  StreamInfo.ExtPosition)^, StreamInfo.ExtSize);
                TempOutput.WriteBuffer(StreamInfo.ExtSize,
                  StreamInfo.ExtSize.Size);
                if Depth = 0 then
                  Inc(EncInfo.InflSize, StreamInfo.ExtSize +
                    StreamInfo.ExtSize.Size);
              end;
            end;
            Inc(DupIdx1);
            if Succ(J - LastIndex) = StreamCount1 then
              break;
            J := InfoStore1[I].Get(StreamInfo);
          end;
          InfoStore1[I].Index := LastIndex;
          J := InfoStore1[I].Get(StreamInfo);
          while J >= 0 do
          begin
            UI32 := StreamInfo.ActualPosition - LastPos;
            TempOutput.WriteBuffer(UI32, UI32.Size);
            if UI32 > 0 then
              TempOutput.WriteBuffer
                ((PByte(DataStore.Slot(I).Memory) + LastPos)^, UI32);
            if Depth = 0 then
              Inc(EncInfo.InflSize, UI32 + UI32.Size);
            LastPos := StreamInfo.ActualPosition + StreamInfo.OldSize;
            if Succ(J - LastIndex) = StreamCount1 then
              break;
            J := InfoStore1[I].Get(StreamInfo);
          end;
          Inc(LastIndex, StreamCount1);
          if LastIndex = InfoStore1[I].Count then
            UI32 := Max(DataStore.Size(I) - LastPos, 0)
          else
            UI32 := 0;
          if REPROCESS = '' then
            TempOutput.WriteBuffer(UI32, UI32.Size);
          if UI32 > 0 then
            TempOutput.WriteBuffer
              ((PByte(DataStore.Slot(I).Memory) + LastPos)^, UI32);
          if Depth = 0 then
            Inc(EncInfo.InflSize, UI32 + UI32.Size);
        until LastIndex = InfoStore1[I].Count;
        LastStream := Max(LastStream - DataStore.Size(I), 0);
        if Depth = 0 then
          if I > 0 then
            TDataStore1(DataStore).LoadEx;
      end;
      if Depth = 0 then
      begin
        UpdateInfo;
        TDataStore1(DataStore).LoadEx;
        if Length(Tasks) > 1 then
          WaitForAll(Tasks);
      end
      else
        break;
    end;
    if REPROCESS = '' then
    begin
      SaveResources;
      StreamCount1 := StreamCount1.MinValue;
      TempOutput.WriteBuffer(StreamCount1, StreamCount1.Size);
    end;
    if Depth = 0 then
      Inc(EncInfo.InflSize, StreamCount1.Size);
  end;
  if Depth = 0 then
  begin
    Input.Free;
    if DBFile <> '' then
    begin
      with WorkStream[0] do
      begin
        Position := 0;
        for W := Low(DBInfo) to High(DBInfo) do
        begin
          J := DBCount[W];
          if J > 0 then
          begin
            WriteBuffer(W, W.Size);
            WriteBuffer(J, J.Size);
            for K := 0 to J - 1 do
              WriteBuffer(DBInfo[W, K], SizeOf(TDatabase));
          end;
        end;
      end;
      with TFileStream.Create(DBFile, fmCreate) do
      begin
        WriteBuffer(WorkStream[0].Memory^, WorkStream[0].Position);
        Free;
      end;
    end;
    if StoreDD > -2 then
    begin
      WorkStream[0].Position := 0;
      ComVars1[0].MemStream[0].Position := 0;
      UI32 := 0;
      for I := Low(DDList1) to High(DDList1) do
      begin
        J := Int64Rec(DDList1[I]).Words[0];
        X := Int64Rec(DDList1[I]).Hi;
        DupTyp1.Index := DDInfo1[J, X].Index1;
        DupTyp1.Count := DDInfo1[J, X].Count;
        DupTyp2.Size := DDInfo1[J, X].Size;
        DupTyp2.Index := DDInfo1[J, X].Index1;
        DupTyp2.Count := DDInfo1[J, X].Count;
        if DupTyp1.Count > 0 then
        begin
          WorkStream[0].WriteBuffer(DupTyp1, SizeOf(TDuplicate2));
          Inc(UI32);
        end;
        ComVars1[0].MemStream[0].WriteBuffer(DupTyp2, SizeOf(TDuplicate3));
      end;
      LStream.WriteBuffer(UI32, UI32.Size);
      LStream.WriteBuffer(WorkStream[0].Memory^, WorkStream[0].Position);
      SetLength(DDCount1, 0);
      EncInfo.DecMem3 := 0;
      K := Length(DDList1);
      L := Length(DDInfo2);
      X := 0;
      Y := 0;
      for I := 0 to StreamCount2 - 1 do
      begin
        if X < K then
        begin
          DupTyp2 := (PDuplicate3(ComVars1[0].MemStream[0].Memory) + X)^;
          if DupTyp2.Index = I then
          begin
            if DupTyp2.Count > 0 then
            begin
              DupBool := False;
              for J := Low(DDCount1) to High(DDCount1) do
              begin
                if DDCount1[J] < 0 then
                  if DupTyp2.Size <= -DDCount1[J] then
                  begin
                    DupBool := True;
                    DDCount1[J] := -DDCount1[J];
                    break;
                  end;
              end;
              if not DupBool then
              begin
                J := Length(DDCount1);
                Insert(DupTyp2.Size, DDCount1, J);
                Inc(EncInfo.DecMem3, DupTyp2.Size);
              end;
              (PDuplicate3(ComVars1[0].MemStream[0].Memory) + X)^.Size := J;
            end;
            Inc(X);
          end;
        end;
        if Y < L then
          if Int64Rec(DDInfo2[Y]).Lo = I then
          begin
            Z := Int64Rec(DDInfo2[Y]).Hi;
            Dec((PDuplicate3(ComVars1[0].MemStream[0].Memory) + Z)^.Count);
            if (PDuplicate3(ComVars1[0].MemStream[0].Memory) + Z)^.Count = 0
            then
            begin
              J := (PDuplicate3(ComVars1[0].MemStream[0].Memory) + Z)^.Size;
              DDCount1[J] := -DDCount1[J];
            end;
            Inc(Y);
          end;
      end;
      LStream.WriteBuffer(EncInfo.DecMem3, EncInfo.DecMem3.Size);
      try
        EncFree;
      finally
      end;
      if NULLOUT then
      begin
        if StoreDD > 0 then
        begin
          TempOutput.Free;
          TBufferedStream(LOutput).Flush;
          TProcessStream(TBufferedStream(LOutput).Instance)
            .WriteBuffer(StoreDD, 0);
          TProcessStream(TBufferedStream(LOutput).Instance).Wait;
          TProcessStream(TBufferedStream(LOutput).Instance).Done;
          UpdateInfo;
          LOutput.Free;
        end;
      end
      else
      begin
        S := TFileStream(TBufferedStream(LOutput).Instance).FileName;
        TempOutput.Free;
        TBufferedStream(LOutput).Flush;
        if StoreDD > 0 then
        begin
          T := ReplaceText(SrepCfg, SPrecompSep3, SPrecompSep2);
          if (T <> '') then
          begin
            List := DecodeStr(T, SPrecompSep2);
            T := '';
            for I := Low(List) to High(List) do
              T := T + ' -' + List[I];
          end;
          with TProcessStream.Create(ExpandPath(PluginsPath + SREPEXE, True),
            '-m' + StoreDD.ToString + 'f' + T + ' ' + S + ' -', GetCurrentDir,
            nil, Output, ErrStream) do
            try
              if Execute then
              begin
                while Running do
                begin
                  EncInfo.SrepSize := OutSize;
                  Sleep(100);
                end;
                Done;
                EncInfo.SrepSize := OutSize;
              end;
            finally
              Free;
            end;
        end
        else
          Output.CopyFrom(TBufferedStream(LOutput).Instance, 0);
        LOutput.Free;
        DeleteFile(S);
      end;
    end
    else
    begin
      UpdateInfo;
      TempOutput.Free;
      try
        EncFree;
      finally
      end;
    end;
    S := 'Decompression memory is ';
    I := ErrStream.DataString.IndexOf(S);
    J := 0;
    if I > 0 then
    begin
      while ErrStream.DataString.Substring(I + S.Length + J, 1) <> ' ' do
        Inc(J);
      EncInfo.SrepMem := ErrStream.DataString.Substring(I + S.Length, J)
        .ToInteger;
    end;
    ErrStream.Free;
  end;
end;

type
  PStreamInfo = ^TStreamInfo;

  TStreamInfo = record
    Count: Integer;
    Pos: TArray<Int64>;
    Completed: TArray<Boolean>;
    procedure Init;
    procedure Free;
    procedure SetCount(ACount: Integer);
  end;

  TCommonVarsDec = record
    DecInput, DecOutput: TArray<TStream>;
    MemStream1: TArray<TMemoryStream>;
    MemStream2: TArray<TMemoryStreamEx>;
    MemInput: TArray<TMemoryStream>;
    MemOutput1, MemOutput2: TArray<TMemoryStream>;
    StreamCount: TArray<PInteger>;
    StreamInfo: TArray<PStreamInfo>;
    StreamIdx: TArray<PInteger>;
    BlockPos: TArray<PInt64>;
  end;

procedure TStreamInfo.Init;
begin
  Count := 0;
  SetLength(Pos, 0);
  SetLength(Completed, 0);
end;

procedure TStreamInfo.Free;
begin
  Init;
end;

procedure TStreamInfo.SetCount(ACount: Integer);
begin
  if ACount > Count then
  begin
    SetLength(Pos, ACount);
    SetLength(Completed, ACount);
  end;
  Count := ACount;
end;

function CalcDupSysMem: Int64;
begin
  if DupSysMem <= 0 then
    Result := Max(0, Abs(DupSysMem) - GetUsedProcessMemory(GetCurrentProcess))
  else
    Result := Max(0, DupSysMem - GetUsedSystemMemory);
  Result := Min(Result,
    Max(0, Min(XTOOL_MEMLIMIT - GetUsedProcessMemory(GetCurrentProcess),
    GetFreeSystemMemory - XTOOL_FREEMEM)));
end;

var
  NStream: TArrayStream;
  DataMgr: TDataManager;
  ComVars2: TArray<TCommonVarsDec>;
  DDList2: TArray<TDuplicate2>;
  DDCount2: Integer;
  DDIndex1, DDIndex2: Integer;

procedure PrecompOutput2(Instance: Integer; const Buffer: Pointer;
  Size: Integer);
begin
  with ComVars2[CurDepth[Instance]] do
    DecOutput[Instance].WriteBuffer(Buffer^, Size);
  if (StoreDD > -2) and (CurDepth[Instance] = 0) then
    if ((DDIndex2 < DDCount2) and (DDIndex1 = DDList2[DDIndex2].Index)) then
    begin
      NStream.Update(0, NStream.MaxSize(0) + CalcDupSysMem);
      DataMgr.Write(DDIndex1, Buffer, Size);
    end;
end;

procedure PrecompOutput3(Instance: Integer; const Buffer: Pointer;
  Size: Integer);
begin
  with ComVars2[CurDepth[Instance]] do
    MemOutput1[Instance].WriteBuffer(Buffer^, Size);
end;

procedure DecThread(X, Y, Z: IntPtr); forward;

procedure DecCallThreads(Index, Depth: Integer);
var
  I: Integer;
begin
  for I := Low(Tasks) to High(Tasks) do
    if I <> Index then
    begin
      ThreadSync[I].Acquire;
      try
        if Tasks[I].Status = TThreadStatus.tsReady then
        begin
          Tasks[I].Update(Index, I, Depth);
          Tasks[I].Perform(DecThread);
          Tasks[I].Start;
        end;
      finally
        ThreadSync[I].Release;
      end;
    end;
end;

procedure Restore(MT: Boolean; Index1, Index2, Depth: Integer);
var
  X, Y: Integer;
  Pos: Int64;
  X64: Int64;
  SI: _StrInfo3;
  SH: PStreamHeader;
  UI32: UInt32;
  Ptr1, Ptr2: PByte;
  LOutput: _PrecompOutput;
begin
  with ComVars2[Depth] do
  begin
    CurDepth[Index2] := Depth;
    Pos := 0;
    X := AtomicIncrement(StreamIdx[Index1]^);
    while X < StreamCount[Index1]^ do
    begin
      if (Succ(Depth) = Length(ComVars2)) and (Length(Tasks) > 1) then
        DecCallThreads(Index1, Depth);
      SH := PStreamHeader(MemStream1[Index1].Memory) + X;
      if MT then
      begin
        LOutput := @PrecompOutput3;
        Pos := StreamInfo[Index1]^.Pos[X];
        X64 := Pos + Max(SH^.OldSize, SH^.NewSize);
        while (BlockPos[Index1]^ < X64) do
        begin
          if IsErrored(Tasks) or (BlockPos[Index1]^ < 0) then
            exit;
          Sleep(1);
        end;
        MemOutput1[Index2].Position := 0;
      end
      else
      begin
        if (StoreDD > -2) and (Depth = 0) then
        begin
          Inc(DDIndex1);
          if ((DDIndex2 < DDCount2) and (DDIndex1 = DDList2[DDIndex2].Index))
          then
            DataMgr.Add(DDIndex1, SH^.OldSize, DDList2[DDIndex2].Count);
        end;
        LOutput := @PrecompOutput2;
        DecInput[Index1].ReadBuffer(UI32, UI32.Size);
        if UI32 > 0 then
          CopyStreamEx(DecInput[Index1], DecOutput[Index1], UI32);
      end;
      SI.OldSize := SH^.OldSize;
      SI.NewSize := SH^.NewSize;
      SI.Resource := SH^.Resource;
      SI.Option := SH^.Option;
      Ptr1 := PByte(MemInput[Index1].Memory) + Pos;
      if SH^.Kind and EXTENDED_STREAM = EXTENDED_STREAM then
      begin
        SI.ExtSize := PInteger(Ptr1 + SI.NewSize - SI.NewSize.Size)^;
        SI.NewSize := SI.NewSize - SI.ExtSize - SI.ExtSize.Size;
        Ptr2 := PByte(MemInput[Index1].Memory) + Pos + SI.NewSize;
      end
      else
        Ptr2 := nil;
      if SH^.Kind and NESTED_STREAM = NESTED_STREAM then
      begin
        MemStream2[Index2].Update(Ptr1, SI.NewSize);
        MemStream2[Index2].Size := SI.NewSize;
        MemStream2[Index2].Position := 0;
        MemOutput2[Index2].Position := 0;
        DecChunk(MemStream2[Index2], MemOutput2[Index2], Index2, Succ(Depth));
        SI.NewSize := PInteger(MemOutput2[Index2].Memory)^;
        Ptr1 := PByte(MemOutput2[Index2].Memory) + SI.NewSize.Size;
        SI.ExtSize := PInteger(PByte(MemOutput2[Index2].Memory) +
          SI.NewSize.Size + SI.NewSize)^;
        Ptr2 := PByte(MemOutput2[Index1].Memory) + SI.NewSize.Size + SI.NewSize
          + SI.ExtSize.Size;
      end;
      if SH^.Kind and DUPLICATED_STREAM = DUPLICATED_STREAM then
      begin
        if MT then
          StreamInfo[Index1]^.Completed[X] := True
        else
          DataMgr.CopyData(SH^.Option, DecOutput[Index1]);
        X := AtomicIncrement(StreamIdx[Index1]^);
        continue;
      end;
      CurCodec[Index1] := SH^.Codec;
      CurDepth[Index1] := Depth;
      Y := GetBits(SI.Option, 0, 3);
      if not InRange(Y, 0, Pred(Length(Codecs[SH^.Codec].Names))) then
        Y := 0;
      if (Codecs[SH^.Codec].Restore(Index2, Depth, Ptr1, Ptr2, SI, LOutput,
        @PrecompFunctions) = False) then
        raise Exception.CreateFmt(SPrecompError3, [Codecs[SH^.Codec].Names[Y]]);
      if MT then
      begin
        Ptr1 := PByte(MemInput[Index1].Memory) + Pos;
        Move(MemOutput1[Index2].Memory^, Ptr1^, SI.OldSize);
        StreamInfo[Index1]^.Completed[X] := True;
      end
      else
      begin
        if (StoreDD > -2) and (Depth = 0) then
          if ((DDIndex2 < DDCount2) and (DDIndex1 = DDList2[DDIndex2].Index))
          then
            Inc(DDIndex2);
        Inc(Pos, SH^.NewSize);
      end;
      X := AtomicIncrement(StreamIdx[Index1]^);
    end;
  end;
end;

procedure DecThread(X, Y, Z: IntPtr);
begin
  Restore(True, X, Y, Z);
end;

procedure DecInit(Input, Output: TStream; Options: PDecodeOptions);
var
  I, J, K: Integer;
  I64: Int64;
  B: Byte;
  Bytes: TBytes;
  UI32: UInt32;
  LResData: TResData;
begin
  GlobalSync := TCriticalSection.Create;
  _Init(Options^.Threads);
  SetLength(ThreadSync, Options^.Threads);
  for I := Low(ThreadSync) to High(ThreadSync) do
    ThreadSync[I] := TCriticalSection.Create;
  Input.ReadBuffer(Options^.Depth, Options^.Depth.Size);
  Input.ReadBuffer(B, B.Size);
  SetLength(Bytes, B);
  Input.ReadBuffer(Bytes[0], B);
  Options^.Method := StringOf(Bytes);
  Input.ReadBuffer(I, I.Size);
  for J := 0 to I - 1 do
  begin
    Input.ReadBuffer(B, B.Size);
    SetLength(Bytes, B);
    Input.ReadBuffer(Bytes[0], B);
    LResData.Name := StringOf(Bytes);
    Input.ReadBuffer(LResData.Size, LResData.Size.Size);
    GetMem(LResData.Data, LResData.Size);
    Input.ReadBuffer(LResData.Data^, LResData.Size);
    Insert(LResData, Resources, Length(Resources));
  end;
  if Options^.Threads > 1 then
    SetLength(Tasks, Options^.Threads);
  SetLength(CurCodec, Options^.Threads);
  SetLength(CurDepth, Options^.Threads);
  SetLength(WorkStream, Options^.Threads);
  SetLength(WorkLastSize, Options^.Threads);
  for I := Low(ThreadSync) to High(ThreadSync) do
  begin
    if Length(Tasks) > 1 then
    begin
      Tasks[I] := TTask.Create;
      Tasks[I].Priority := ThrPty;
    end;
    SetPriorityClass(GetCurrentProcess, CaseInt(Integer(ThrPty),
      [IDLE_PRIORITY_CLASS, IDLE_PRIORITY_CLASS, BELOW_NORMAL_PRIORITY_CLASS,
      NORMAL_PRIORITY_CLASS, ABOVE_NORMAL_PRIORITY_CLASS, HIGH_PRIORITY_CLASS,
      REALTIME_PRIORITY_CLASS]));
    SetThreadPriority(GetCurrentThread, CaseInt(Integer(ThrPty),
      [THREAD_PRIORITY_IDLE, THREAD_PRIORITY_LOWEST,
      THREAD_PRIORITY_BELOW_NORMAL, THREAD_PRIORITY_NORMAL,
      THREAD_PRIORITY_ABOVE_NORMAL, THREAD_PRIORITY_HIGHEST,
      THREAD_PRIORITY_TIME_CRITICAL]));
    WorkStream[I] := TMemoryStreamEx2.Create;
    WorkLastSize[I] := 0;
  end;
  CodecInit(Options^.Threads, Options^.Method);
  SetLength(ComVars2, Options^.Depth);
  for J := Low(ComVars2) to High(ComVars2) do
    with ComVars2[J] do
    begin
      SetLength(DecInput, Options^.Threads);
      SetLength(DecOutput, Options^.Threads);
      SetLength(MemStream1, Options^.Threads);
      SetLength(MemStream2, Options^.Threads);
      SetLength(MemInput, Options^.Threads);
      SetLength(MemOutput1, Options^.Threads);
      SetLength(MemOutput2, Options^.Threads);
      SetLength(StreamCount, Options^.Threads);
      SetLength(StreamInfo, Options^.Threads);
      SetLength(StreamIdx, Options^.Threads);
      SetLength(BlockPos, Options^.Threads);
      for I := Low(ThreadSync) to High(ThreadSync) do
      begin
        if (J = 0) and (I > 0) then
        begin
          MemStream1[I] := MemStream1[0];
          MemInput[I] := MemInput[0];
          StreamCount[I] := StreamCount[0];
          StreamInfo[I] := StreamInfo[0];
          StreamIdx[I] := StreamIdx[0];
          BlockPos[I] := BlockPos[0];
        end
        else
        begin
          MemStream1[I] := TMemoryStream.Create;
          MemInput[I] := TMemoryStream.Create;
          New(StreamCount[I]);
          New(StreamInfo[I]);
          StreamInfo[I]^.Init;
          New(StreamIdx[I]);
          New(BlockPos[I]);
        end;
        MemStream2[I] := TMemoryStreamEx.Create(False);
        MemOutput1[I] := TMemoryStream.Create;
        MemOutput2[I] := TMemoryStream.Create;
      end;
    end;
  DupSysMem := Options^.DedupSysMem;
  Input.ReadBuffer(StoreDD, StoreDD.Size);
end;

procedure DecFree;
var
  I, J: Integer;
begin
  WaitForAll(Tasks);
  CodecFree(Length(ThreadSync));
  for J := Low(ComVars2) to High(ComVars2) do
    with ComVars2[J] do
    begin
      for I := Low(ThreadSync) to High(ThreadSync) do
      begin
        MemStream2[I].Free;
        MemOutput1[I].Free;
        MemOutput2[I].Free;
        if (J = 0) and (I > 0) then
          continue;
        MemStream1[I].Free;
        MemInput[I].Free;
        Dispose(StreamCount[I]);
        StreamInfo[I]^.Free;
        Dispose(StreamInfo[I]);
        Dispose(StreamIdx[I]);
        Dispose(BlockPos[I]);
      end;
    end;
  for I := Low(ThreadSync) to High(ThreadSync) do
  begin
    if Length(Tasks) > 1 then
      Tasks[I].Free;
    WorkStream[I].Free;
  end;
  FreeResources;
  _Free;
  GlobalSync.Free;
  for I := Low(ThreadSync) to High(ThreadSync) do
    ThreadSync[I].Free;
end;

procedure DecChunk(Input, Output: TStream; Index, Depth: Integer);
var
  StreamHeader: PStreamHeader;
  BlockSize: Int64;
  CurrPos: Int64;
  UI32: UInt32;
  DDMemSize: Int64;
  I, J: Integer;
  LStream1: TStream;
  LStream2: TProcessStream;
  LCache: TStream;
  T: String;
  List: TStringDynArray;
  procedure LoadResources;
  var
    C, D: Integer;
    B: Byte;
    Bytes: TBytes;
    LResData: TResData;
  begin
    with ComVars2[Depth] do
    begin
      if Depth = 0 then
      begin
        DecInput[Index].ReadBuffer(C, C.Size);
        for D := 0 to C - 1 do
        begin
          DecInput[Index].ReadBuffer(B, B.Size);
          SetLength(Bytes, B);
          DecInput[Index].ReadBuffer(Bytes[0], B);
          LResData.Name := StringOf(Bytes);
          DecInput[Index].ReadBuffer(LResData.Size, LResData.Size.Size);
          GetMem(LResData.Data, LResData.Size);
          DecInput[Index].ReadBuffer(LResData.Data^, LResData.Size);
          Insert(LResData, Resources, Length(Resources));
        end;
      end;
    end;
  end;

begin
  if Depth = 0 then
  begin
    UI32 := 0;
    DDMemSize := 0;
    if (StoreDD > -2) then
    begin
      Input.ReadBuffer(UI32, UI32.Size);
      SetLength(DDList2, UI32);
      DDCount2 := UI32;
      for I := Low(DDList2) to High(DDList2) do
        Input.ReadBuffer(DDList2[I], SizeOf(TDuplicate2));
      DDIndex1 := -1;
      DDIndex2 := 0;
      Input.ReadBuffer(DDMemSize, DDMemSize.Size);
    end;
    NStream.Add(TypeInfo(TMemoryStream), CalcDupSysMem);
    NStream.Add(TypeInfo(TPrecompVMStream));
    DataMgr := TDataManager.Create(NStream);
    LogInt64 := 0;
    LCache := nil;
    if CACHE > 0 then
    begin
      LCache := TMemoryStreamEx.Create(True, GetMemory(CACHE), CACHE);
      LCache.Size := CACHE;
    end;
  end;
  with ComVars2[Depth] do
  begin
    if (Depth = 0) and (StoreDD > 0) then
    begin
      T := ReplaceText(SrepCfg, SPrecompSep3, SPrecompSep2);
      if (T <> '') then
      begin
        List := DecodeStr(T, SPrecompSep2);
        T := '';
        for I := Low(List) to High(List) do
          T := T + ' -' + List[I];
      end;
      LStream2 := TProcessStream.Create(ExpandPath(PluginsPath + SREPEXE, True),
        '-d' + T + ' - -', GetCurrentDir, Input, nil);
      if not LStream2.Execute then
        raise EReadError.CreateRes(@SReadError);
      if Assigned(LCache) then
        DecInput[Index] := TCacheReadStream.Create(LStream2, LCache)
      else
        DecInput[Index] := TBufferedStream.Create(LStream2, True, XTOOL_BSIZE);
    end
    else if Depth = 0 then
      DecInput[Index] := TCacheReadStream.Create(Input, LCache)
    else
      DecInput[Index] := Input;
    DecOutput[Index] := Output;
    LoadResources;
    DecInput[Index].ReadBuffer(StreamCount[Index]^, StreamCount[Index]^.Size);
    while StreamCount[Index]^ >= 0 do
    begin
      if IsErrored(Tasks) then
        for I := Low(Tasks) to High(Tasks) do
          Tasks[I].RaiseLastError;
      DecInput[Index].ReadBuffer(BlockSize, BlockSize.Size);
      if StreamCount[Index]^ > 0 then
      begin
        MemStream1[Index].Position := 0;
        CopyStreamEx(DecInput[Index], MemStream1[Index],
          StreamCount[Index]^ * SizeOf(TStreamHeader));
        CurrPos := 0;
        if ((Depth > 0) and (Length(Tasks) > 1)) or
          ((Length(Tasks) > 1) and (StreamCount[Index]^ > 1)) then
        begin
          BlockPos[Index]^ := 0;
          StreamInfo[Index]^.SetCount(StreamCount[Index]^);
          for J := 0 to StreamCount[Index]^ - 1 do
          begin
            StreamInfo[Index]^.Pos[J] := CurrPos;
            StreamInfo[Index]^.Completed[J] := False;
            StreamHeader := PStreamHeader(MemStream1[Index].Memory) + J;
            Inc(CurrPos, Max(StreamHeader^.OldSize, StreamHeader^.NewSize));
          end;
        end
        else
          CurrPos := BlockSize;
        if MemInput[Index].Size < CurrPos then
          MemInput[Index].Size := CurrPos;
        MemInput[Index].Position := 0;
        StreamIdx[Index]^ := -1;
        if ((Depth > 0) and (Length(Tasks) > 1)) or
          ((Length(Tasks) > 1) and (StreamCount[Index]^ > 1)) then
        begin
          if Depth = 0 then
            for I := Low(Tasks) to High(Tasks) do
            begin
              Tasks[I].Update(I, I, Depth);
              Tasks[I].Perform(DecThread);
              Tasks[I].Start;
            end;
          for J := 0 to StreamCount[Index]^ - 1 do
          begin
            StreamHeader := PStreamHeader(MemStream1[Index].Memory) + J;
            MemInput[Index].Position := StreamInfo[Index]^.Pos[J];
            if CopyStream(DecInput[Index], MemInput[Index],
              StreamHeader^.NewSize) <> StreamHeader^.NewSize then
            begin
              BlockPos[Index]^ := -1;
              raise EReadError.CreateRes(@SReadError);
            end;
            Inc(BlockPos[Index]^, Max(StreamHeader^.OldSize,
              StreamHeader^.NewSize));
          end;
          if Depth > 0 then
            DecThread(Index, Index, Depth);
        end
        else
          CopyStreamEx(DecInput[Index], MemInput[Index], BlockSize);
        if ((Depth > 0) and (Length(Tasks) > 1)) or
          ((Length(Tasks) > 1) and (StreamCount[Index]^ > 1)) then
        begin
          for J := 0 to StreamCount[Index]^ - 1 do
          begin
            StreamHeader := PStreamHeader(MemStream1[Index].Memory) + J;
            DecInput[Index].ReadBuffer(UI32, UI32.Size);
            if UI32 > 0 then
              CopyStreamEx(DecInput[Index], DecOutput[Index], UI32);
            while (StreamInfo[Index]^.Completed[J] = False) and
              (IsErrored(Tasks) = False) do
              Sleep(1);
            if IsErrored(Tasks) then
              for I := Low(Tasks) to High(Tasks) do
                Tasks[I].RaiseLastError;
            if (StoreDD > -2) and (Depth = 0) then
            begin
              Inc(DDIndex1);
              if ((DDIndex2 < DDCount2) and (DDIndex1 = DDList2[DDIndex2].Index))
              then
              begin
                NStream.Update(0, NStream.MaxSize(0) + CalcDupSysMem);
                DataMgr.Add(DDIndex1, StreamHeader^.OldSize,
                  DDList2[DDIndex2].Count);
                DataMgr.Write(DDIndex1,
                  (PByte(MemInput[Index].Memory) + StreamInfo[Index]^.Pos[J]),
                  StreamHeader^.OldSize);
                Inc(DDIndex2);
              end;
            end;
            if StreamHeader^.Kind and DUPLICATED_STREAM = DUPLICATED_STREAM then
              DataMgr.CopyData(StreamHeader^.Option, DecOutput[Index])
            else
              DecOutput[Index].WriteBuffer
                ((PByte(MemInput[Index].Memory) + StreamInfo[Index]^.Pos[J])^,
                StreamHeader^.OldSize);
          end;
          if Depth = 0 then
            WaitForAll(Tasks);
        end
        else
          Restore(False, Index, 0, Depth);
      end;
      DecInput[Index].ReadBuffer(UI32, UI32.Size);
      if UI32 > 0 then
        CopyStreamEx(DecInput[Index], DecOutput[Index], UI32);
      LoadResources;
      DecInput[Index].ReadBuffer(StreamCount[Index]^, StreamCount[Index]^.Size);
    end;
    if (Depth = 0) and (StoreDD > 0) then
    begin
      with LStream2 do
      begin
        Wait;
        Done;
      end;
    end;
    if Depth = 0 then
    begin
      DecInput[Index].Free;
      DataMgr.Free;
      if StoreDD > 0 then
        if Assigned(LCache) then
          LStream2.Free;
    end;
  end;
end;

procedure EncodeStats;
var
  FHandle: THandle;
  SBInfo: TConsoleScreenBufferInfo;
  CLine: Integer;
  SL: TStringList;
  Coords: TCoord;
  ulLength: Cardinal;

  procedure Update;
  var
    I: Integer;
    TS: TTimeSpan;
    CreationTime, ExitTime, KernelTime, UserTime: TFileTime;
    TT: TSystemTime;
    I64: Int64;
  begin
    GetProcessTimes(GetCurrentProcess, CreationTime, ExitTime, KernelTime,
      UserTime);
    FileTimeToSystemTime(TFileTime(Int64(UserTime) + Int64(KernelTime)), TT);
    SL[0] := 'Streams: ' + EncInfo.Processed.ToString + ' / ' +
      EncInfo.Count.ToString;
    TS := Stopwatch.Elapsed;
    SL[1] := 'Time: ' + Format('%0:.2d:%1:.2d:%2:.2d',
      [TS.Hours + TS.Days * 24, TS.Minutes, TS.Seconds]) + ' (CPU ' +
      Format('%0:.2d:%1:.2d:%2:.2d', [TT.wHour + Pred(TT.wDay) * 24, TT.wMinute,
      TT.wSecond]) + ')';
    I64 := EncInfo.DecMem0 + EncInfo.DecMem1;
    I64 := I64 div 1024;
    if StoreDD > -2 then
    begin
      I := 4;
      SL[2] := 'Duplicates: ' + EncInfo.DupCount.ToString + ' (' +
        ConvertKB2TB(EncInfo.DecMem2 div 1024) + ') [' +
        ConvertKB2TB(EncInfo.DupSize1 div 1024) + ' >> ' +
        ConvertKB2TB(EncInfo.DupSize2 div 1024) + ']     ';
      if StoreDD > 0 then
      begin
        I := 5;
        SL[3] := 'Srep decompression memory: ' +
          ConvertKB2TB(EncInfo.SrepMem * 1024) + '     ';
      end;
    end
    else
      I := 3;
    SL[I] := 'Size: ' + ConvertKB2TB(EncInfo.InSize div 1024) +
      IfThen(StoreDD > -2,
      ' >> ' + ConvertKB2TB((EncInfo.InflSize + EncInfo.DupSize2) div 1024), '')
      + ' >> ' + ConvertKB2TB(EncInfo.InflSize div 1024) +
      IfThen(StoreDD > 0, ' >> ' + ConvertKB2TB((EncInfo.SrepSize) div 1024),
      '') + IfThen(COMPRESS > 0, ' >> ' + ConvertKB2TB((EncInfo.CompSize)
      div 1024), '') + '      ';
    SetConsoleCursorPosition(FHandle, Coords);
    WriteConsole(FHandle, PChar(SL.Text), Length(SL.Text), ulLength, nil);
  end;

begin
  FHandle := GetStdHandle(STD_ERROR_HANDLE);
  GetConsoleScreenBufferInfo(FHandle, SBInfo);
  Coords.X := 0;
  Coords.Y := SBInfo.dwCursorPosition.Y;
  SL := TStringList.Create;
  SL.Add('Streams: 0 / 0');
  SL.Add('Time: 00:00:00');
  if StoreDD > -2 then
  begin
    SL.Add('Duplicates: 0 (0.00 MB) [0.00 MB  >> 0.00 MB]');
    if StoreDD > 0 then
      SL.Add('Srep decompression memory: 0.00 MB [0.00MB]');
  end;
  SL.Add('');
  SL.Add('Size: ');
  SL.Add('');
  while Stopwatch.IsRunning do
  begin
    Update;
    Sleep(500);
  end;
  Update;
  SL.Free;
end;

procedure DecodeStats;
var
  FHandle: THandle;
  SBInfo: TConsoleScreenBufferInfo;
  CLine: Integer;
  SL: TStringList;
  Coords: TCoord;
  ulLength: Cardinal;

  procedure Update;
  var
    TS: TTimeSpan;
    CreationTime, ExitTime, KernelTime, UserTime: TFileTime;
    TT: TSystemTime;
  begin
    GetProcessTimes(GetCurrentProcess, CreationTime, ExitTime, KernelTime,
      UserTime);
    FileTimeToSystemTime(TFileTime(Int64(UserTime) + Int64(KernelTime)), TT);
    TS := Stopwatch.Elapsed;
    SL[0] := 'Time: ' + Format('%0:.2d:%1:.2d:%2:.2d',
      [TS.Hours + TS.Days * 24, TS.Minutes, TS.Seconds]) + ' (CPU ' +
      Format('%0:.2d:%1:.2d:%2:.2d', [TT.wHour + Pred(TT.wDay) * 24, TT.wMinute,
      TT.wSecond]) + ')';
    SetConsoleCursorPosition(FHandle, Coords);
    WriteConsole(FHandle, PChar(SL.Text), Length(SL.Text), ulLength, nil);
  end;

begin
  FHandle := GetStdHandle(STD_ERROR_HANDLE);
  GetConsoleScreenBufferInfo(FHandle, SBInfo);
  Coords.X := 0;
  Coords.Y := SBInfo.dwCursorPosition.Y;
  SL := TStringList.Create;
  SL.Add('Time: 00:00:00');
  SL.Add('');
  while Stopwatch.IsRunning do
  begin
    Update;
    Sleep(500);
  end;
  Update;
  SL.Free;
end;

procedure Encode(Input, Output: TStream; Options: TEncodeOptions);
var
  Compressed: Byte;
  LInput, LOutput: TStream;
  LCache: TStream;
  I64: Int64;
begin
  SetLength(Codecs, 0);
  Insert(PrecompINI.Codec, Codecs, Length(Codecs));
  Insert(PrecompINIEx.Codec, Codecs, Length(Codecs));
  Insert(PrecompSearch.Codec, Codecs, Length(Codecs));
  Insert(PrecompDLL.Codec, Codecs, Length(Codecs));
  Insert(PrecompEXE.Codec, Codecs, Length(Codecs));
  Insert(PrecompCrypto.Codec, Codecs, Length(Codecs));
  Insert(PrecompZLib.Codec, Codecs, Length(Codecs));
  Insert(PrecompLZ4.Codec, Codecs, Length(Codecs));
  Insert(PrecompLZO.Codec, Codecs, Length(Codecs));
  Insert(PrecompZSTD.Codec, Codecs, Length(Codecs));
  Insert(PrecompOodle.Codec, Codecs, Length(Codecs));
  Insert(PrecompMedia.Codec, Codecs, Length(Codecs));
  Insert(PrecompDStorage.Codec, Codecs, Length(Codecs));
  Insert(PrecompUtils.Codec, Codecs, Length(Codecs));
  if Output is TBufferedStream then
    NULLOUT := TBufferedStream(Output).Instance is TNullStream;
  I64 := CACHE div IfThen(NULLOUT and (COMPRESS = 0), 1, 2);
  LCache := nil;
  if I64 > 0 then
  begin
    LCache := TMemoryStreamEx.Create(True, GetMemory(I64), I64);
    LCache.Size := I64;
  end;
  LInput := TCacheReadStream.Create(Input, LCache, ccNone);
  FillChar(EncInfo, SizeOf(EncInfo), 0);
  ConTask := TTask.Create;
  ConTask.Priority := tpIdle;
  Stopwatch := TStopwatch.Create;
  Stopwatch.Start;
  ConTask.Perform(EncodeStats);
  if (VERBOSE = False) and (IsLibrary = False) then
    ConTask.Start;
  try
    EncInit(LInput, Output, @Options);
    Compressed := COMPRESS;
    if REPROCESS = '' then
      Output.WriteBuffer(Compressed, Compressed.Size);
    if COMPRESS > 0 then
    begin
      case COMPRESS of
        1:
          begin
            LOutput := TLZMACompressStream.Create(Output);
            with LOutput as TLZMACompressStream do
            begin
              Level := Options.CLevel;
              Threads := Options.CThreads;
              Dictionary := Options.CDict;
              Overlap := Options.COverlap;
              HighCompress := Options.CHighCompress
            end;
          end;
        2:
          begin
            LOutput := TBufferedStream.Create
              (TProcessStream.Create(ExpandPath(PluginsPath +
              ExtractExec(EXTCOMP), True), ExtractParams(EXTCOMP),
              GetCurrentDir, nil, Output), False, XTOOL_BSIZE);
            TProcessStream(TBufferedStream(LOutput).Instance).Execute;
          end;
      end;
    end
    else
      LOutput := Output;
    EncData(LInput, LOutput, 0, 0);
  finally
    if COMPRESS > 0 then
    begin
      case COMPRESS of
        1:
          TLZMACompressStream(LOutput).Flush;
        2:
          begin
            TBufferedStream(LOutput).Flush;
            TProcessStream(TBufferedStream(LOutput).Instance)
              .WriteBuffer(StoreDD, 0);
            TProcessStream(TBufferedStream(LOutput).Instance).Wait;
            TProcessStream(TBufferedStream(LOutput).Instance).Done;
          end;
      end;
      if COMPRESS = 1 then
        EncInfo.CompSize := TLZMACompressStream(LOutput).OutSize
      else if COMPRESS = 2 then
        EncInfo.CompSize := TProcessStream(TBufferedStream(LOutput)
          .Instance).OutSize;
      LOutput.Free;
    end;
    try
      if not EncFreed then
        EncFree;
    finally
      Stopwatch.Stop;
    end;
  end;
  if VERBOSE then
    EncodeStats;
  ConTask.Wait;
  ConTask.Free;
  if (EncInfo.DecMem3 > 0) and (IsLibrary = False) then
  begin
    WriteLine('Additional decoding memory is ' +
      ConvertKB2TB(EncInfo.DecMem3 div 1024));
    WriteLine('Decoding speed improved by up to ' + Format('%.2n',
      [EncInfo.DupSize0 / (EncInfo.DupSize0 - EncInfo.DupSize2)]) + 'x');
    WriteLine('');
  end;
end;

procedure Decode(Input, Output: TStream; Options: TDecodeOptions);
var
  Compressed: Byte;
  LInput: TStream;
begin
  SetLength(Codecs, 0);
  Insert(PrecompINI.Codec, Codecs, Length(Codecs));
  Insert(PrecompINIEx.Codec, Codecs, Length(Codecs));
  Insert(PrecompSearch.Codec, Codecs, Length(Codecs));
  Insert(PrecompDLL.Codec, Codecs, Length(Codecs));
  Insert(PrecompEXE.Codec, Codecs, Length(Codecs));
  Insert(PrecompCrypto.Codec, Codecs, Length(Codecs));
  Insert(PrecompZLib.Codec, Codecs, Length(Codecs));
  Insert(PrecompLZ4.Codec, Codecs, Length(Codecs));
  Insert(PrecompLZO.Codec, Codecs, Length(Codecs));
  Insert(PrecompZSTD.Codec, Codecs, Length(Codecs));
  Insert(PrecompOodle.Codec, Codecs, Length(Codecs));
  Insert(PrecompMedia.Codec, Codecs, Length(Codecs));
  Insert(PrecompDStorage.Codec, Codecs, Length(Codecs));
  Insert(PrecompUtils.Codec, Codecs, Length(Codecs));
  FillChar(EncInfo, SizeOf(EncInfo), 0);
  ConTask := TTask.Create;
  ConTask.Priority := tpIdle;
  Stopwatch := TStopwatch.Create;
  Stopwatch.Start;
  ConTask.Perform(DecodeStats);
  if (VERBOSE = False) and (IsLibrary = False) then
    ConTask.Start;
  NStream := TArrayStream.Create;
  try
    DecInit(Input, Output, @Options);
    Input.ReadBuffer(Compressed, Compressed.Size);
    if Compressed = 1 then
    begin
      LInput := TLZMADecompressStream.Create(Input);
      with LInput as TLZMADecompressStream do
        Threads := Options.DThreads;
    end
    else if Compressed = 2 then
    begin
      LInput := TProcessStream.Create
        (ExpandPath(PluginsPath + ExtractExec(EXTCOMP), True),
        ExtractParams(EXTCOMP), GetCurrentDir, Input);
      TProcessStream(LInput).Execute;
    end
    else
      LInput := Input;
    DecChunk(LInput, Output, 0, 0);
  finally
    if Compressed > 0 then
    begin
      if Compressed = 2 then
        with LInput as TProcessStream do
        begin
          Wait;
          Done;
        end;
      LInput.Free;
    end;
    try
      NStream.Free;
      DecFree;
    finally
      Stopwatch.Stop;
    end;
  end;
  if VERBOSE then
    DecodeStats;
  ConTask.Wait;
  ConTask.Free;
end;

initialization

PrecompFunctions.GetCodec := @PrecompGetCodec;
PrecompFunctions.GetParam := @PrecompGetParam;
PrecompFunctions.Allocator := @PrecompAllocator;
PrecompFunctions.AddDepthStream := @PrecompAddDepthStream;
PrecompFunctions.COMPRESS := @PrecompCompress;
PrecompFunctions.Decompress := @PrecompDecompress;
PrecompFunctions.Encrypt := @PrecompEncrypt;
PrecompFunctions.Decrypt := @PrecompDecrypt;
PrecompFunctions.Hash := @PrecompHash;
PrecompFunctions.EncodePatch := @PrecompEncodePatch;
PrecompFunctions.DecodePatch := @PrecompDecodePatch;
PrecompFunctions.AddResource := @PrecompAddResource;
PrecompFunctions.GetResource := @PrecompGetResource;
PrecompFunctions.SearchBinary := @PrecompSearchBinary;
PrecompFunctions.SwapBinary := @PrecompSwapBinary;
PrecompFunctions.Swap16 := @PrecompSwap16;
PrecompFunctions.Swap32 := @PrecompSwap32;
PrecompFunctions.Swap64 := @PrecompSwap64;
PrecompFunctions.FileOpen := @PrecompFileOpen;
PrecompFunctions.FileClose := @PrecompFileClose;
PrecompFunctions.FileSeek := @PrecompFileSeek;
PrecompFunctions.FileSize := @PrecompFileSize;
PrecompFunctions.FileRead := @PrecompFileRead;
PrecompFunctions.FileWrite := @PrecompFileWrite;
PrecompFunctions.IniRead := @PrecompIniRead;
PrecompFunctions.IniWrite := @PrecompIniWrite;
PrecompFunctions.Exec := @PrecompExec;
PrecompFunctions.ExecStdin := @PrecompExecStdin;
PrecompFunctions.ExecStdout := @PrecompExecStdout;
PrecompFunctions.ExecStdio := @PrecompExecStdio;
PrecompFunctions.ExecStdioSync := @PrecompExecStdioSync;
PrecompFunctions.GetDepthCodec := @PrecompGetDepthCodec;
PrecompFunctions.ReadFuture := @PrecompReadFuture;
PrecompFunctions.LogScan1 := PrecompLogScan1;
PrecompFunctions.LogScan2 := PrecompLogScan2;
PrecompFunctions.LogProcess := PrecompLogProcess;
PrecompFunctions.LogRestore := PrecompLogRestore;
PrecompFunctions.LogPatch1 := PrecompLogPatch1;
PrecompFunctions.LogPatch2 := PrecompLogPatch2;
PrecompFunctions.AcceptPatch := PrecompAcceptPatch;
PrecompFunctions.Transfer := PrecompTransfer;
PrecompFunctions.Storage := PrecompStorage;
PrecompFunctions.AddResourceEx := PrecompAddResourceEx;

{$IFDEF CPU64BITS}
if FileExists(ExpandPath(PluginsPath + SREPEXE64, True)) then
  SREPEXE := SREPEXE64;
{$ENDIF}

end.
