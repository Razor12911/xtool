unit PrecompMain;

interface

uses
  Main, Threading, Utils, ParseClass, ParseExpr,
  PrecompUtils, PrecompZLib,
  WinAPI.Windows,
  System.SysUtils, System.Classes, System.SyncObjs, System.Math, System.Types,
  System.StrUtils, System.RTLConsts,
  System.Generics.Defaults, System.Generics.Collections;

const
  XTOOL_PRECOMP = $304C5458;

type
  TEncodeOptions = record
    Method: AnsiString;
    ChunkSize, Threads: Integer;
    Depth: Integer;
    LowMem: Boolean;
    HistorySize: Boolean;
    HistoryFile: String;
  end;

  TDecodeOptions = record
    Method: AnsiString;
    Threads: Integer;
  end;

procedure PrintHelp;
procedure Parse(ParamArg: TArray<string>; out Options: TEncodeOptions);
  overload;
procedure Parse(ParamArg: TArray<string>; out Options: TDecodeOptions);
  overload;
// reuse resources when going in-depth
// make an array of all common resources for depth
// depth will be hard af to add
// check if at least one of the functions exists in a dll before using it
// number of chunks to process when decoding
procedure Encode(Input, Output: TStream; Options: TEncodeOptions);
procedure Decode(Input, Output: TStream; Options: TDecodeOptions);

function PrecompGetCodec(Cmd: PAnsiChar; Index: Integer; WithParams: Boolean)
  : PAnsiChar stdcall;
function PrecompGetParam(Cmd: PAnsiChar; Index: Integer; Param: PAnsiChar)
  : PAnsiChar stdcall;
function PrecompAllocator(Instance: Integer; Size: Integer): Pointer stdcall;

procedure PrecompOutput1(Instance: Integer; const Buffer: Pointer;
  Size: Integer)stdcall;
procedure PrecompOutput2(Instance: Integer; const Buffer: Pointer;
  Size: Integer)stdcall;
procedure PrecompOutput3(Instance: Integer; const Buffer: Pointer;
  Size: Integer)stdcall;
procedure PrecompAddStream1(Instance: Integer; Info: PStrInfo1;
  Codec: PAnsiChar)stdcall;

implementation

var
  Codecs: array of TPrecompressor;
  PrecompFunctions: _PrecompFuncs;

procedure PrintHelp;
var
  I, J: Integer;
  S: string;
begin
  Console.Write('precomp - data precompressor');
  Console.Write('');
  Console.Write('Usage:');
  Console.Write
    ('  xtool precomp:method1,method2,methodN...:param1,param2,paramN... input output');
  Console.Write('');
  (* Console.Write('Methods:');
    for I := Low(Codecs) to High(Codecs) do
    begin
    S := '';
    for J := Low(Codecs[I]) to High(Codecs[I]) do
    begin
    if (IndexText(Codecs[I][J], Codecs[I]) = J) then
    S := S + Codecs[I][J] + ', ';
    end;
    Delete(S, Length(S) - 1, 2);
    Console.Write('  ' + S);
    end; *)
  Console.Write('');
  Console.Write('Parameters:');
  Console.Write('  c#  - scanning range of precompressor [16mb]');
  Console.Write('  t#  - number of working threads [Threads/2]');
  Console.Write('  lm  - low memory mode');
  Console.Write('  hs  - enable history database');
  Console.Write('  hf# - history database file');
  Console.Write('');
end;

procedure Parse(ParamArg: TArray<string>; out Options: TEncodeOptions);
var
  ArgParse: TArgParser;
  ExpParse: TExpressionParser;
  S: String;
begin
  ArgParse := TArgParser.Create(ParamArg);
  ExpParse := TExpressionParser.Create;
  try
    Options.Method := AnsiString(ArgParse.AsString('-m'));
    S := ArgParse.AsString('-c', '16mb');
    S := ReplaceText(S, 'KB', '* 1024^1');
    S := ReplaceText(S, 'MB', '* 1024^2');
    S := ReplaceText(S, 'GB', '* 1024^3');
    Options.ChunkSize := Max(4194304, Round(ExpParse.Evaluate(S)));
    S := ArgParse.AsString('-t', '50p');
    S := ReplaceText(S, 'p', '%');
    S := ReplaceText(S, '%', '%*' + CPUCount.ToString);
    Options.Threads := Max(1, Round(ExpParse.Evaluate(S)));
    Options.Depth := Succ(ArgParse.AsInteger('-d'));
    Options.LowMem := ArgParse.AsBoolean('-lm');
  finally
    ArgParse.Free;
    ExpParse.Free;
  end;
end;

procedure Parse(ParamArg: TArray<string>; out Options: TDecodeOptions);
var
  ArgParse: TArgParser;
  ExpParse: TExpressionParser;
  S: String;
begin
  ArgParse := TArgParser.Create(ParamArg);
  ExpParse := TExpressionParser.Create;
  try
    Options.Method := AnsiString(ArgParse.AsString('-m'));
    S := ArgParse.AsString('-t', '50p');
    S := ReplaceText(S, 'p', '%');
    S := ReplaceText(S, '%', '%*' + CPUCount.ToString);
    Options.Threads := Max(1, Round(ExpParse.Evaluate(S)));
  finally
    ArgParse.Free;
    ExpParse.Free;
  end;
end;

function GetIndex(Scanned, Processed: TArray<Boolean>): Integer;
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
  for I := Low(Scanned) to High(Scanned) do
  begin
    if (Scanned[I] = True) and (Processed[I] = False) then
    begin
      Result := I;
      break;
    end;
  end;
end;

type
  TCommonVars = record
    MemStream: TMemoryStream;
    DataStore: TDataStore;
    MemOutput: TArray<TMemoryStream>;
    InfoStore1: TArray<TListEx<TEncodeSI>>;
    InfoStore2: TList<TFutureSI>;
    Scanned1, Scanned2, Processed: TArray<Boolean>;
    CurPos: TArray<Int64>;
    CurCodec: TArray<Byte>;
    StrIdx: TArray<Integer>;
    ThrIdx: TArray<Integer>;
  end;

var
  ComVars: TArray<TCommonVars>;
  DepIdx: TArray<Integer>;
  Sync: TCriticalSection;
  Tasks: TArray<TTask>;
  WorkStream: TArray<TMemoryStream>;
  History: TDictionary<Int64, THistory>;
  Duplicates: TDictionary<Int64, TDuplicate>;

  // history should not depend on the streams about to be processed, check before doing this

procedure CodecInit(Count: Integer; Method: AnsiString);
var
  I, X, Y: Integer;
  S: AnsiString;
  List: TStringDynArray;
begin
  PrecompFunctions.GetCodec := @PrecompGetCodec;
  PrecompFunctions.GetParam := @PrecompGetParam;
  PrecompFunctions.Allocator := @PrecompAllocator;
  if Method = '' then
    exit;
  Insert(PrecompZLib.Codec, Codecs, Length(Codecs));
  for X := High(Codecs) downto Low(Codecs) do
    for Y := Low(Codecs[X].Names) to High(Codecs[X].Names) do
      Insert(String(Codecs[X].Names[Y]), List, Length(List));
  I := 0;
  while Assigned(PrecompGetCodec(PAnsiChar(Method), I, False)) do
  begin
    if IndexText(String(PrecompGetCodec(PAnsiChar(Method), I, False)), List) < 0
    then
      raise Exception.CreateFmt(SPrecompError1,
        [String(PrecompGetCodec(PAnsiChar(Method), I, False))]);
    Inc(I);
  end;
  for X := High(Codecs) downto Low(Codecs) do
  begin
    S := '';
    for Y := Low(Codecs[X].Names) to High(Codecs[X].Names) do
    begin
      I := 0;
      while Assigned(PrecompGetCodec(PAnsiChar(Method), I, False)) do
      begin
        if SameText(String(PrecompGetCodec(PAnsiChar(Method), I, False)),
          String(Codecs[X].Names[Y])) then
          S := S + AnsiString(PrecompGetCodec(PAnsiChar(Method), I,
            True)) + '+';
        Inc(I);
      end;
    end;
    if S <> '' then
    begin
      SetLength(S, Length(S) - 1);
      Codecs[X].Initialized := Codecs[X].Init(PAnsiChar(S), Count,
        @PrecompFunctions);
    end
    else
      Delete(Codecs, X, 1);
  end;
end;

procedure CodecFree(Count: Integer);
var
  I: Integer;
begin
  for I := Low(Codecs) to High(Codecs) do
    if Codecs[I].Initialized then
      Codecs[I].Free(@PrecompFunctions);
end;

function PrecompGetCodec(Cmd: PAnsiChar; Index: Integer; WithParams: Boolean)
  : PAnsiChar;
var
  List: TStringDynArray;
begin
  Result := nil;
  if Assigned(Cmd) then
  begin
    List := DecodeStr(String(Cmd), '+');
    if InRange(Index, Low(List), High(List)) then
      if WithParams then
        Result := PAnsiChar(AnsiString(List[Index]))
      else
        Result := PAnsiChar(AnsiString(DecodeStr(List[Index], ':')[0]));
  end;
end;

function PrecompGetParam(Cmd: PAnsiChar; Index: Integer; Param: PAnsiChar)
  : PAnsiChar;
var
  List1, List2: TStringDynArray;
  I: Integer;
begin
  Result := nil;
  if Assigned(Cmd) then
  begin
    List1 := DecodeStr(String(Cmd), '+');
    if InRange(Index, Low(List1), High(List1)) then
    begin
      List2 := DecodeStr(List1[Index], ':');
      if Length(List2) > 1 then
      begin
        if not Assigned(Param) then
          Result := PAnsiChar(AnsiString(List2[1]))
        else
        begin
          List1 := DecodeStr(List2[1], ',');
          for I := Low(List1) to High(List1) do
            if List1[I].StartsWith(String(Param), True) then
              Result := PAnsiChar
                (AnsiString(List1[I].Substring(Length(String(Param)))));
        end;
      end;
    end;
  end;
end;

function PrecompAllocator(Instance: Integer; Size: Integer): Pointer;
begin
  with ComVars[DepIdx[Instance]] do
  begin
    if WorkStream[Instance].Size < Size then
      WorkStream[Instance].Size := Size;
    Result := WorkStream[Instance].Memory;
  end;
end;

procedure PrecompOutput1(Instance: Integer; const Buffer: Pointer;
  Size: Integer);
begin
  with ComVars[DepIdx[Instance]] do
  begin
    case Size of
      - 1:
        MemOutput[Instance].Position := CurPos[Instance];
    else
      MemOutput[Instance].WriteBuffer(Buffer^, Size);
    end;
  end;
end;

// TMemoryMap

procedure PrecompAddStream1(Instance: Integer; Info: PStrInfo1;
  Codec: PAnsiChar);
var
  SI1: TEncodeSI;
  SI2: TFutureSI;
  LValid: Boolean;
  LCodec: Byte;
  LOption: Integer;
  I, X, Y: Integer;
  S: String;
begin
  // add overhead function
  with ComVars[DepIdx[Instance]] do
  begin
    if (Info^.Position < 0) or (MemOutput[Instance].Position - CurPos[Instance]
      <> Info^.NewSize) then
    begin
      MemOutput[Instance].Position := CurPos[Instance];
      exit;
    end;
    if Assigned(Codec) then
    begin
      LValid := False;
      I := 0;
      while Assigned(PrecompGetCodec(Codec, I, False)) do
      begin
        for X := Low(Codecs) to High(Codecs) do
        begin
          for Y := Low(Codecs[X].Names) to High(Codecs[X].Names) do
            if SameText(String(PrecompGetCodec(Codec, I, False)),
              String(Codecs[X].Names[Y])) then
            begin
              LCodec := X;
              if Codecs[X].Initialized then
                if Codecs[X].Parse(PrecompGetCodec(Codec, I, True), @LOption,
                  @PrecompFunctions) then
                begin
                  LValid := True;
                  break;
                end;
            end;
          if LValid then
            break;
        end;
        Inc(I);
      end;
      if not LValid then
      begin
        MemOutput[Instance].Position := CurPos[Instance];
        exit;
      end;
    end
    else
    begin
      LCodec := CurCodec[Instance];
      LOption := Info^.Option;
    end;
    if Info^.Position < Min(DataStore.Size, DataStore.Slot[Instance].Size) then
    begin
      FillChar(SI1, SizeOf(TEncodeSI), 0);
      SI1.ActualPosition := Info^.Position;
      SI1.StorePosition := CurPos[Instance];
      SI1.OriginalSize := Info^.OldSize;
      SI1.UnpackedSize := Info^.NewSize;
      SI1.Codec := LCodec;
      SI1.Option := LOption;
      SI1.Status := Info^.Status;
      InfoStore1[Instance].Add(SI1);
    end
    else
    begin
      FillChar(SI2, SizeOf(TFutureSI), 0);
      SI2.Position := DataStore.Position[Instance] + Info^.Position;
      SI2.OriginalSize := Info^.OldSize;
      SI2.UnpackedSize := Info^.NewSize;
      SI2.Codec := LCodec;
      SI2.Option := LOption;
      SI2.Status := Info^.Status;
      Sync.Acquire;
      InfoStore2.Add(SI2);
      Sync.Release;
    end;
    CurPos[Instance] := MemOutput[Instance].Position;
  end;
end;

// endian(CSize,4)

procedure Scan1(Index: Integer);
var
  I: Integer;
begin
  with ComVars[DepIdx[Index]] do
    for I := Low(Codecs) to High(Codecs) do
    begin
      try
        CurPos[Index] := MemOutput[Index].Position;
        CurCodec[Index] := I;
        Codecs[I].Scan1(Index, DataStore.Slot[Index].Memory,
          Min(DataStore.Size, DataStore.Slot[Index].Size),
          DataStore.Slot[Index].Size, @PrecompOutput1, @PrecompAddStream1,
          @PrecompFunctions);
      except
      end;
    end;
end;

procedure Scan2(Index: Integer);
var
  I: Integer;
begin
  (* while StrIdx2[Index] < InfoStore2.Count do
    begin
    // if InfoStore2[StrIdx2[Index]].Position then
    // there is a problem here with the count when multi threading...
    Inc(StrIdx2[Index]);
    end;
    for I := Low(Codecs) to High(Codecs) do
    begin
    try
    CurPos[Index] := MemOutput[Index].Position;
    CurCodec[Index] := I;
    Codecs[Index][I].Scan2(Index, DataStore.Slot[Index].Memory,
    Min(DataStore.Size, DataStore.Slot[Index].Size),
    DataStore.Slot[Index].Size, @PrecompAllocator, @PrecompCompress,
    @PrecompOutput1, @PrecompAddStream1);
    except
    end;
    end; *)
end;

// use TDictionary for history and deduplication

procedure Process(Index, ThreadIndex, StreamIndex: Integer);
var
  SI1: _StrInfo2;
  SI2: TEncodeSI;
  Res: Boolean;
begin
  with ComVars[DepIdx[Index]] do
  begin
    SI2 := InfoStore1[ThreadIndex][StreamIndex];
    SI1.OldSize := SI2.OriginalSize;
    SI1.NewSize := SI2.UnpackedSize;
    SI1.Option := SI2.Option;
    SI1.Status := SI2.Status;
    CurPos[Index] := MemOutput[Index].Position;
    CurCodec[Index] := SI2.Codec;
    try
      Res := Codecs[SI2.Codec].Process(Index,
        PByte(DataStore.Slot[ThreadIndex].Memory) + SI2.ActualPosition,
        PByte(MemOutput[ThreadIndex].Memory) + SI2.StorePosition, @SI1,
        @PrecompOutput1, @PrecompFunctions);
    except
      Res := False;
    end;
    if Res then
    begin
      SI2.OriginalSize := SI1.OldSize;
      SI2.UnpackedSize := SI1.NewSize;
      SI2.Option := SI1.Option;
      SI2.Status := TStreamStatus(SuccessStatus);
      SI2.ExtPosition := CurPos[Index];
      SI2.ExtSize := MemOutput[Index].Position - CurPos[Index];
      SI2.ExtThread := Index;
      InfoStore1[ThreadIndex][StreamIndex] := SI2;
      CurPos[Index] := MemOutput[Index].Position;
    end;
  end;
end;

procedure EncThread(Y: Integer);
var
  X, Z: Integer;
  History: Boolean;
begin
  with ComVars[DepIdx[Y]] do
  begin
    if InRange(Y, Low(InfoStore1), High(InfoStore1)) then
    begin
      Scan1(Y);
      Scanned1[Y] := True;
      // try to process even before scan finishes
      (* if ExternalInUse(EOptions.Method) then
        while BoolArray(Scanned1, False) do
        Sleep(10); *)
      // Scan2(Y);
      InfoStore1[Y].Sort;
      Scanned2[Y] := True;
    end;
    // if index < count give the thread index, check this for all threads
    // should give more speed
    while True do
    begin
      Z := GetIndex(Scanned2, Processed);
      while Z = -1 do
      begin
        Sleep(10);
        Z := GetIndex(Scanned2, Processed);
      end;
      ThrIdx[Y] := Z;
      if Z < -1 then
        break;
      X := AtomicIncrement(StrIdx[Z]);
      while X < InfoStore1[Z].Count do
      begin
        History := False;
        Process(Y, Z, X);
        if History = False then
        begin
          // Int64Rec(HD.Tag).Lo := SI.Checksum;
          // Int64Rec(HD.Tag).Hi := SI.UnpackedSize;
        end;
        Z := GetIndex(Scanned2, Processed);
        while Z = -1 do
        begin
          Sleep(10);
          Z := GetIndex(Scanned2, Processed);
        end;
        ThrIdx[Y] := Z;
        if Z < -1 then
          break;
        X := AtomicIncrement(StrIdx[Z]);
      end;
      if Z < -1 then
        break;
      if X >= InfoStore1[Z].Count then
        Processed[Z] := True;
    end;
  end;
end;

procedure InternalEncode(Input, Output: TStream; Options: TEncodeOptions;
  Index, Depth: Integer);
var
  GUID: TGUID;
  StreamInfo: TEncodeSI;
  StreamHeader: TStreamHeader;
  StreamCount: Int32;
  BlockSize: Int64;
  UI32: UInt32;
  I, J: Integer;
  LastStream, LastPos: Int64;
begin
  I := XTOOL_PRECOMP;
  Output.WriteBuffer(I, I.Size);
  CreateGUID(GUID);
  Output.WriteBuffer(GUID, SizeOf(GUID));
  LongRec(I).Bytes[0] := Length(Options.Method);
  Output.WriteBuffer(LongRec(I).Bytes[0], LongRec(I).Bytes[0].Size);
  Output.WriteBuffer(Options.Method[1], LongRec(I).Bytes[0]);
  Sync := TCriticalSection.Create;
  if Index = 0 then
  begin
    SetLength(DepIdx, Options.Threads);
    SetLength(ComVars, Options.Depth);
    for J := Low(ComVars) to High(ComVars) do
      with ComVars[J] do
      begin
        MemStream := TMemoryStream.Create;
        SetLength(MemOutput, Options.Threads);
        SetLength(ThrIdx, Options.Threads);
        if Options.LowMem then
          I := 1
        else
          I := Options.Threads;
        SetLength(InfoStore1, I);
        SetLength(StrIdx, I);
        SetLength(Scanned1, I);
        SetLength(Scanned2, I);
        SetLength(Processed, I);
        SetLength(CurPos, I);
        SetLength(CurCodec, I);
      end;
  end;
  SetLength(Tasks, Options.Threads);
  SetLength(WorkStream, Options.Threads);
  for I := Low(Tasks) to High(Tasks) do
  begin
    if Length(Tasks) > 1 then
      Tasks[I] := TTask.Create(I);
    MemOutput[I] := TMemoryStream.Create;
    WorkStream[I] := TMemoryStream.Create;
  end;
  for I := Low(InfoStore1) to High(InfoStore1) do
    InfoStore1[I] := TListEx<TEncodeSI>.Create(EncodeSICmp);
  InfoStore2 := TList<TFutureSI>.Create(FutureSICmp);
  // if FileExists(Options.HistoryFile) then
  // LoadHistory(HistoryList, Options.HistoryFile);
  DataStore := TDataStore.Create(Input, True, Length(InfoStore1),
    Options.ChunkSize);
  CodecInit(Options.Threads, Options.Method);
  LastStream := 0;
  DataStore.Load;
  while not DataStore.Done do
  begin
    if Length(Tasks) > 1 then
      if IsErrored(Tasks) then
        for I := Low(Tasks) to High(Tasks) do
          Tasks[I].RaiseLastError;
    for I := Low(InfoStore1) to High(InfoStore1) do
    begin
      InfoStore1[I].Count := 0;
      StrIdx[I] := -1;
      Scanned1[I] := False;
      Scanned2[I] := False;
      Processed[I] := False;
      CurPos[I] := 0;
    end;
    for I := Low(Tasks) to High(Tasks) do
    begin
      ThrIdx[I] := 0;
      MemOutput[I].Position := 0;
      if Length(Tasks) > 1 then
      begin
        Tasks[I].Perform(EncThread);
        Tasks[I].Start;
      end
      else
        EncThread(0);
    end;
    for I := Low(InfoStore1) to High(InfoStore1) do
    begin
      while Processed[I] = False do
        Sleep(10);
      for J := Low(ThrIdx) to High(ThrIdx) do
        while ThrIdx[J] = I do
          Sleep(10);
      // DEC MEM LIMIT HERE
      LastPos := LastStream;
      MemStream.Position := 0;
      StreamCount := 0;
      BlockSize := 0;
      MemStream.WriteBuffer(StreamCount, StreamCount.Size);
      MemStream.WriteBuffer(BlockSize, BlockSize.Size);
      if InfoStore1[I].Count > 0 then
      begin
        InfoStore1[I].Index := 0;
        J := InfoStore1[I].Get(StreamInfo);
        while J >= 0 do
        begin
          if (Integer(StreamInfo.Status) <> SuccessStatus) or
            (LastStream > StreamInfo.ActualPosition) or
            (StreamInfo.ActualPosition >= Options.ChunkSize) then
            InfoStore1[I].Delete(J)
          else
          begin
            Inc(StreamCount);
            StreamHeader.Kind := DEFAULT_STREAM;
            if StreamInfo.ExtSize > 0 then
              StreamHeader.Kind := StreamHeader.Kind or EXTENDED_STREAM;
            StreamHeader.OldSize := StreamInfo.OriginalSize;
            StreamHeader.NewSize := StreamInfo.UnpackedSize;
            if StreamInfo.ExtSize > 0 then
            begin
              Inc(StreamHeader.NewSize, StreamInfo.ExtSize);
              Inc(StreamHeader.NewSize, StreamInfo.ExtSize.Size);
            end;
            StreamHeader.Codec := StreamInfo.Codec;
            StreamHeader.Option := StreamInfo.Option;
            Inc(BlockSize, StreamHeader.NewSize);
            MemStream.WriteBuffer(StreamHeader, SizeOf(TStreamHeader));
            LastStream := Int64(StreamInfo.ActualPosition) +
              StreamInfo.OriginalSize;
          end;
          J := InfoStore1[I].Get(StreamInfo);
        end;
        MemStream.Position := 0;
        MemStream.WriteBuffer(StreamCount, StreamCount.Size);
        MemStream.WriteBuffer(BlockSize, BlockSize.Size);
        Output.WriteBuffer(MemStream.Memory^, MemStream.Position + StreamCount *
          SizeOf(TStreamHeader));
        InfoStore1[I].Index := 0;
        J := InfoStore1[I].Get(StreamInfo);
        while J >= 0 do
        begin
          Output.WriteBuffer
            ((PByte(MemOutput[I].Memory) + StreamInfo.StorePosition)^,
            StreamInfo.UnpackedSize);
          if StreamInfo.ExtSize > 0 then
          begin
            Output.WriteBuffer((PByte(MemOutput[StreamInfo.ExtThread].Memory) +
              StreamInfo.ExtPosition)^, StreamInfo.ExtSize);
            Output.WriteBuffer(StreamInfo.ExtSize, StreamInfo.ExtSize.Size);
          end;
          J := InfoStore1[I].Get(StreamInfo);
        end;
        InfoStore1[I].Index := 0;
        J := InfoStore1[I].Get(StreamInfo);
        while J >= 0 do
        begin
          UI32 := StreamInfo.ActualPosition - LastPos;
          Output.WriteBuffer(UI32, UI32.Size);
          if UI32 > 0 then
            Output.WriteBuffer((PByte(DataStore.Slot[I].Memory) +
              LastPos)^, UI32);
          LastPos := StreamInfo.ActualPosition + StreamInfo.OriginalSize;
          J := InfoStore1[I].Get(StreamInfo);
        end;
      end
      else
        Output.WriteBuffer(StreamCount, StreamCount.Size);
      UI32 := Max(Min(Options.ChunkSize, DataStore.Slot[I].Size) - LastPos, 0);
      Output.WriteBuffer(UI32, UI32.Size);
      if UI32 > 0 then
        Output.WriteBuffer((PByte(DataStore.Slot[I].Memory) + LastPos)^, UI32);
      LastStream := Max(LastStream - Options.ChunkSize, 0);
      if I > 0 then
        DataStore.LoadEx;
    end;
    DataStore.LoadEx;
    if Length(Tasks) > 1 then
      WaitForAll(Tasks);
  end;
  StreamCount := StreamCount.MinValue;
  Output.WriteBuffer(StreamCount, StreamCount.Size);
  CodecFree(Options.Threads);
  for I := Low(Tasks) to High(Tasks) do
  begin
    if Length(Tasks) > 1 then
      Tasks[I].Free;
    MemOutput[I].Free;
    WorkStream[I].Free;
  end;
  for I := Low(InfoStore1) to High(InfoStore1) do
    InfoStore1[I].Free;
  InfoStore2.Free;
  DataStore.Free;
  // if Options.HistoryFile <> '' then
  // SaveHistory(HistoryList, Options.HistoryFile);
  // SetLength(HistoryList, 0);
  MemStream.Free;
  Sync.Free;
end;

var
  DecInput, DecOutput: TStream;
  Idx: Integer;
  StreamPos: TArray<Int64>;
  Completed: TArray<Boolean>;
  StreamCount: Int32;
  BlockPos: Int64;
  MemInput: TMemoryStream;

procedure PrecompOutput2(Instance: Integer; const Buffer: Pointer;
  Size: Integer);
begin
  DecOutput.WriteBuffer(Buffer^, Size);
end;

procedure PrecompOutput3(Instance: Integer; const Buffer: Pointer;
  Size: Integer);
begin
  MemOutput[Instance].WriteBuffer(Buffer^, Size);
end;

procedure Restore(MT: Boolean; ThreadIndex: Integer);
var
  X: Integer;
  Pos: Int64;
  X64: Int64;
  SI: _StrInfo3;
  SH: PStreamHeader;
  UI32: UInt32;
  Ptr1, Ptr2: PByte;
  LOutput: _PrecompOutput;
begin
  Pos := 0;
  X := AtomicIncrement(Idx);
  while X < StreamCount do
  begin
    SH := PStreamHeader((PByte(MemStream.Memory) + X * SizeOf(TStreamHeader)));
    if MT then
    begin
      LOutput := @PrecompOutput3;
      Pos := StreamPos[X];
      X64 := Pos + SH^.NewSize;
      while (BlockPos < X64) do
      begin
        if IsErrored(Tasks) or (BlockPos < 0) then
          exit;
        Sleep(1);
      end;
      MemOutput[ThreadIndex].Position := 0;
    end
    else
    begin
      LOutput := @PrecompOutput2;
      DecInput.ReadBuffer(UI32, UI32.Size);
      if UI32 > 0 then
        DecOutput.CopyFrom(DecInput, UI32);
    end;
    SI.OldSize := SH^.OldSize;
    SI.NewSize := SH^.NewSize;
    SI.Option := SH^.Option;
    Ptr1 := PByte(MemInput.Memory) + Pos;
    if SH^.Kind and EXTENDED_STREAM = EXTENDED_STREAM then
    begin
      SI.ExtSize := PInteger(Ptr1 + SI.NewSize - SI.NewSize.Size)^;
      SI.NewSize := SI.NewSize - SI.ExtSize - SI.ExtSize.Size;
      Ptr2 := PByte(MemInput.Memory) + Pos + SI.NewSize;
    end
    else
      Ptr2 := nil;
    if (Codecs[SH^.Codec].Restore(ThreadIndex, Ptr1, Ptr2, SI, LOutput,
      @PrecompFunctions) = False) then
      raise Exception.CreateFmt(SPrecompError3,
        [String(Codecs[SH^.Codec].Names[0])]);
    if MT then
    begin
      Move(MemOutput[ThreadIndex].Memory^, Ptr1^, SI.OldSize);
      Completed[X] := True;
    end
    else
    begin
      Inc(Pos, SH^.NewSize);
    end;
    X := AtomicIncrement(Idx);;
  end;
end;

procedure DecThread(Y: Integer);
begin
  Restore(True, Y);
end;

procedure ReadCallback(Pos: Int64);
begin
  BlockPos := Pos;
end;

// restore stuff by chunk

procedure InternalDecode(Input, Output: TStream; Options: TDecodeOptions;
  Index, Depth: Integer);
var
  GUID: TGUID;
  StreamHeader: PStreamHeader;
  BlockSize: Int64;
  CurrPos: Int64;
  UI32: UInt32;
  I, J: Integer;
begin
  DecInput := Input;
  DecOutput := Output;
  Input.ReadBuffer(GUID, SizeOf(GUID));
  Input.ReadBuffer(LongRec(I).Bytes[0], LongRec(I).Bytes[0].Size);
  SetLength(Options.Method, LongRec(I).Bytes[0]);
  Input.ReadBuffer(Options.Method[1], LongRec(I).Bytes[0]);
  MemStream := TMemoryStream.Create;
  SetLength(Tasks, Options.Threads);
  SetLength(MemOutput, Options.Threads);
  SetLength(WorkStream, Options.Threads);
  for I := Low(Tasks) to High(Tasks) do
  begin
    if Length(Tasks) > 1 then
      Tasks[I] := TTask.Create(I);
    MemOutput[I] := TMemoryStream.Create;
    WorkStream[I] := TMemoryStream.Create;
  end;
  MemInput := TMemoryStream.Create;
  CodecInit(Options.Threads, Options.Method);
  Input.ReadBuffer(StreamCount, StreamCount.Size);
  while StreamCount >= 0 do
  begin
    if Length(Tasks) > 1 then
      if IsErrored(Tasks) then
        for I := Low(Tasks) to High(Tasks) do
          Tasks[I].RaiseLastError;
    if StreamCount > 0 then
    begin
      BlockPos := 0;
      Input.ReadBuffer(BlockSize, BlockSize.Size);
      MemStream.Position := 0;
      MemStream.CopyFrom(Input, StreamCount * SizeOf(TStreamHeader));
      CurrPos := 0;
      if (Options.Threads > 1) and (StreamCount > 1) then
      begin
        if StreamCount > Length(StreamPos) then
          SetLength(StreamPos, StreamCount);
        SetLength(Completed, Length(StreamPos));
        for J := 0 to StreamCount - 1 do
        begin
          StreamPos[J] := CurrPos;
          Completed[J] := False;
          StreamHeader :=
            PStreamHeader((PByte(MemStream.Memory) + J *
            SizeOf(TStreamHeader)));
          Inc(CurrPos, Max(StreamHeader^.OldSize, StreamHeader^.NewSize));
        end;
      end;
      if MemInput.Size < BlockSize then
        MemInput.Size := BlockSize;
      MemInput.Position := 0;
      Idx := -1;
      if (Options.Threads > 1) and (StreamCount > 1) then
      begin
        for I := Low(Tasks) to High(Tasks) do
        begin
          Tasks[I].Perform(DecThread);
          Tasks[I].Start;
        end;
        for J := 0 to StreamCount - 1 do
        begin
          StreamHeader :=
            PStreamHeader((PByte(MemStream.Memory) + J *
            SizeOf(TStreamHeader)));
          MemInput.Size := Max(MemInput.Size, MemInput.Position +
            Max(StreamHeader^.OldSize, StreamHeader^.NewSize));
          if CopyStream(Input, MemInput, StreamHeader^.NewSize) <>
            StreamHeader^.NewSize then
          begin
            BlockPos := -1;
            raise EReadError.CreateRes(@SReadError);
          end;
          Inc(BlockPos, Max(StreamHeader^.OldSize, StreamHeader^.NewSize));
        end;
      end
      else
        MemInput.CopyFrom(Input, BlockSize);
      if (Options.Threads > 1) and (StreamCount > 1) then
      begin
        for J := 0 to StreamCount - 1 do
        begin
          Input.ReadBuffer(UI32, UI32.Size);
          if UI32 > 0 then
            Output.CopyFrom(Input, UI32);
          while (Completed[J] = False) and (IsErrored(Tasks) = False) do
            Sleep(1);
          if IsErrored(Tasks) then
            for I := Low(Tasks) to High(Tasks) do
              Tasks[I].RaiseLastError;
          Output.WriteBuffer((PByte(MemInput.Memory) + StreamPos[J])^,
            PStreamHeader((PByte(MemStream.Memory) + J * SizeOf(TStreamHeader)))
            ^.OldSize);
        end;
        WaitForAll(Tasks);
      end
      else
        Restore(False, 0);
    end;
    Input.ReadBuffer(UI32, UI32.Size);
    if UI32 > 0 then
      Output.CopyFrom(Input, UI32);
    Input.ReadBuffer(StreamCount, StreamCount.Size);
  end;
  CodecFree(Options.Threads);
  MemInput.Free;
  for I := Low(Tasks) to High(Tasks) do
  begin
    if Length(Tasks) > 1 then
      Tasks[I].Free;
    MemOutput[I].Free;
    WorkStream[I].Free;
  end;
  MemStream.Free;
end;

procedure Encode(Input, Output: TStream; Options: TEncodeOptions);
begin
  InternalEncode(Input, Output, Options, 0, 0);
end;

procedure Decode(Input, Output: TStream; Options: TDecodeOptions);
begin
  InternalDecode(Input, Output, Options, 0, 0);
end;

end.
