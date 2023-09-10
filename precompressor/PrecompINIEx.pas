unit PrecompINIEx;

interface

uses
  InitCode,
  Utils, ParseExpr,
  PrecompUtils,
  WinAPI.Windows,
  System.SysUtils, System.Classes, System.StrUtils,
  System.Types, System.Math, System.IOUtils, System.IniFiles;

var
  Codec: TPrecompressor;

implementation

type
  PCfgStruct = ^TCfgStruct;

  TCfgStruct = record
    Name: String;
    Data: Pointer;
    Position, Size: NativeInt;
    Value: Double;
    BeforeStream: Boolean;
  end;

  PCfgCounter = ^TCfgCounter;

  TCfgCounter = record
    StartS, EndS, StepS: String;
    StartV, EndV, StepV: Double;
    Current, Min, Max: Double;
  end;

  PConfigRec = ^TConfigRec;

  TConfigRec = record
    Parser: TExpressionParser;
    Name, Codec: String;
    Resource: Integer;
    BigEndian: Boolean;
    Structure: array [0 .. 2] of TArray<TCfgStruct>;
    Counter: TArray<TCfgCounter>;
    StreamPosition, StreamOffset, OldSize, NewSize, DepthSize: String;
    Names, Exprs: TArray<String>;
    Values: TArray<Double>;
    Conditions: array [0 .. 2] of TArray<String>;
  end;

  PCfgRecDynArray = ^TCfgRecDynArray;
  TCfgRecDynArray = TArray<TConfigRec>;

var
  CfgList: TStringDynArray;
  CodecCfg: TArray<TArray<TCfgRecDynArray>>;
  CodecAvailable, CodecEnabled: TArray<TArray<Boolean>>;

procedure EndianMove(Source, Dest: Pointer; Size: NativeInt;
  BigEndian: Boolean = False);
begin
  if BigEndian then
    ReverseBytes(Source, Dest, Size)
  else
    Move(Source^, Dest^, Size);
end;

function ConfigInit(Command: PChar; Count: Integer;
  Funcs: PPrecompFuncs): Boolean;
var
  I, J: Integer;
  X, Y, Z: Integer;
  S: String;
  SList: TStringDynArray;
  ParamsSet: Boolean;
begin
  Result := True;
  for X := Low(CodecAvailable) to High(CodecAvailable) do
    for Y := Low(CodecAvailable[X]) to High(CodecAvailable[X]) do
    begin
      CodecAvailable[X, Y] := True;
      CodecEnabled[X, Y] := False;
    end;
  SetLength(CodecCfg, Count);
  for I := 1 to High(CodecCfg) do
  begin
    SetLength(CodecCfg[I], Length(CodecCfg[0]));
    for J := Low(CodecCfg[I]) to High(CodecCfg[I]) do
      SetLength(CodecCfg[I, J], Length(CodecCfg[0, J]));
  end;
  for I := Low(CodecCfg) to High(CodecCfg) do
    for J := Low(CodecCfg[I]) to High(CodecCfg[I]) do
      for X := Low(CodecCfg[I, J]) to High(CodecCfg[I, J]) do
        with CodecCfg[I, J, X] do
        begin
          if I = 0 then
            Resource := RegisterResources(Codec);
          if I > 0 then
          begin
            Parser := TExpressionParser.Create;
            Name := CodecCfg[0, J, X].Name;
            Codec := CodecCfg[0, J, X].Codec;
            Resource := CodecCfg[0, J, X].Resource;
            BigEndian := CodecCfg[0, J, X].BigEndian;
            for Z := Low(Structure) to High(Structure) do
            begin
              SetLength(Structure[Z], Length(CodecCfg[0, J, X].Structure[Z]));
              for Y := Low(Structure[Z]) to High(Structure[Z]) do
              begin
                Structure[Z, Y].Name := CodecCfg[0, J, X].Structure[Z, Y].Name;
                Structure[Z, Y].Position := CodecCfg[0, J, X].Structure[Z]
                  [Y].Position;
                Structure[Z, Y].Size := CodecCfg[0, J, X].Structure[Z, Y].Size;
                Structure[Z, Y].Value := CodecCfg[0, J, X].Structure
                  [Z, Y].Value;
                Structure[Z, Y].BeforeStream := CodecCfg[0, J, X].Structure
                  [Z, Y].BeforeStream;
                GetMem(Structure[Z, Y].Data, Structure[Z, Y].Size);
                Move(CodecCfg[0, J, X].Structure[Z, Y].Data^,
                  Structure[Z, Y].Data^, Structure[Z, Y].Size);
              end;
            end;
            SetLength(Counter, Length(CodecCfg[0, J, X].Counter));
            for Y := Low(Counter) to High(Counter) do
            begin
              Counter[Y].StartS := CodecCfg[0, J, X].Counter[Y].StartS;
              Counter[Y].EndS := CodecCfg[0, J, X].Counter[Y].EndS;
              Counter[Y].StepS := CodecCfg[0, J, X].Counter[Y].StepS;
              Counter[Y].StartV := CodecCfg[0, J, X].Counter[Y].StartV;
              Counter[Y].EndV := CodecCfg[0, J, X].Counter[Y].EndV;
              Counter[Y].StepV := CodecCfg[0, J, X].Counter[Y].StepV;
              Counter[Y].Current := CodecCfg[0, J, X].Counter[Y].Current;
              Counter[Y].Min := CodecCfg[0, J, X].Counter[Y].Min;
              Counter[Y].Max := CodecCfg[0, J, X].Counter[Y].Max;
            end;
            StreamPosition := CodecCfg[0, J, X].StreamPosition;
            StreamOffset := CodecCfg[0, J, X].StreamOffset;
            OldSize := CodecCfg[0, J, X].OldSize;
            NewSize := CodecCfg[0, J, X].NewSize;
            DepthSize := CodecCfg[0, J, X].DepthSize;
            SetLength(Names, Length(CodecCfg[0, J, X].Names));
            SetLength(Exprs, Length(CodecCfg[0, J, X].Exprs));
            SetLength(Values, Length(CodecCfg[0, J, X].Values));
            for Y := Low(Names) to High(Names) do
            begin
              Names[Y] := CodecCfg[0, J, X].Names[Y];
              Exprs[Y] := CodecCfg[0, J, X].Exprs[Y];
              Values[Y] := CodecCfg[0, J, X].Values[Y];
            end;
            for Z := Low(Conditions) to High(Conditions) do
            begin
              SetLength(Conditions[Z], Length(CodecCfg[0, J, X].Conditions[Z]));
              for Y := Low(Conditions[Z]) to High(Conditions[Z]) do
                Conditions[Z, Y] := CodecCfg[0, J, X].Conditions[Z, Y];
            end;
            for Z := Low(Structure) to High(Structure) do
              for Y := Low(Structure[Z]) to High(Structure[Z]) do
                Parser.DefineVariable(Structure[Z, Y].Name,
                  @Structure[Z, Y].Value);
            for Y := Low(Counter) to High(Counter) do
            begin
              Parser.DefineVariable('Counter' + Succ(Y).ToString,
                @Counter[Y].Current);
              Parser.DefineVariable('CounterMin' + Succ(Y).ToString,
                @Counter[Y].Min);
              Parser.DefineVariable('CounterMax' + Succ(Y).ToString,
                @Counter[Y].Max);
            end;
            for Y := Low(Names) to High(Names) do
              Parser.DefineVariable(Names[Y], @Values[Y]);
          end;
        end;
  X := 0;
  while Funcs^.GetCodec(Command, X, False) <> '' do
  begin
    S := Funcs^.GetCodec(Command, X, False);
    for Y := Low(Codec.Names) to High(Codec.Names) do
    begin
      ParamsSet := False;
      if CompareText(S, Codec.Names[Y]) = 0 then
      begin
        for I := Low(CodecEnabled[Y]) to High(CodecEnabled[Y]) do
          CodecEnabled[Y, I] := True;
        for Z := Low(CodecCfg[0, Y]) to High(CodecCfg[0, Y]) do
        begin
          SList := DecodeStr(CodecCfg[0, Y, Z].Name, ',');
          for J := Low(SList) to High(SList) do
            if Funcs^.GetParam(Command, X, PChar(SList[J])) <> '' then
            begin
              if not ParamsSet then
              begin
                for I := Low(CodecEnabled[Y]) to High(CodecEnabled[Y]) do
                  CodecEnabled[Y, I] := False;
                ParamsSet := True;
              end;
              CodecEnabled[Y, Z] := True;
              break;
            end;
        end;
        break;
      end;
    end;
    Inc(X);
  end;
  for X := Low(CodecEnabled) to High(CodecEnabled) do
    for Y := Low(CodecEnabled[X]) to High(CodecEnabled[X]) do
      if CodecEnabled[X, Y] then
        AddMethod(PrecompGetCodec(PChar(CodecCfg[0, X, Y].Codec), 0, False));
end;

procedure ConfigFree(Funcs: PPrecompFuncs);
var
  I, J: Integer;
  X, Y, Z: Integer;
begin
  for I := Low(CodecCfg) to High(CodecCfg) do
    for J := Low(CodecCfg[I]) to High(CodecCfg[I]) do
      for X := Low(CodecCfg[I, J]) to High(CodecCfg[I, J]) do
        with CodecCfg[I, J, X] do
        begin
          if I > 0 then
          begin
            for Z := Low(Structure) to High(Structure) do
              for Y := Low(Structure[Z]) to High(Structure[Z]) do
                FreeMem(Structure[Z, Y].Data);
          end;
        end;
end;

function ConfigParse(Command: String; Option: PInteger;
  Funcs: PPrecompFuncs): Boolean;
begin
  Result := False;
end;

procedure ConfigScan1(Instance, Depth: Integer; Input: PByte;
  Size, SizeEx: NativeInt; Output: _PrecompOutput; Add: _PrecompAdd;
  Funcs: PPrecompFuncs);
type
  TScanStatus = (None, Success, Fail);
var
  Status: TScanStatus;
  A, B: Integer;
  I, J: Integer;
  X, Y: Integer;
  Pos: NativeInt;
  NI: NativeInt;
  I64: Int64;
  LoopPosInt, StreamPosInt1, StreamPosInt2, StreamOffsetInt, OldSizeInt,
    NewSizeInt, DepthSizeInt: NativeInt;
  SI: _StrInfo1;
  DS: TPrecompStr;

  procedure UpdateCounters(var C: TConfigRec);
  var
    Z: Integer;
  begin
    with C do
      for Z := Low(Counter) to High(Counter) do
      begin
        Counter[Z].StartV := Parser.Evaluate(Counter[Z].StartS);
        Counter[Z].EndV := Parser.Evaluate(Counter[Z].EndS);
        Counter[Z].StepV := Parser.Evaluate(Counter[Z].StepS);
      end;
  end;

  function CheckCounters(C: TConfigRec): Boolean;
  var
    Z: Integer;
  begin
    Result := True;
    with C do
      for Z := Low(Counter) to High(Counter) do
        if not InRange(Round(Counter[Z].Current), Min(Round(Counter[Z].StartV),
          Round(Counter[Z].EndV)), Max(Round(Counter[Z].StartV),
          Round(Counter[Z].EndV))) then
          Exit(False);
  end;

  procedure DoAddStream(C: TConfigRec);
  begin
    with C do
    begin
      Output(Instance, nil, -1);
      SI.Position := StreamPosInt1 + StreamOffsetInt;
      SI.OldSize := OldSizeInt;
      SI.NewSize := NewSizeInt;
      SI.Resource := Resource;
      SI.Option := 0;
      if System.Pos(SPrecompSep2, Codec) > 0 then
        SI.Status := TStreamStatus.Predicted
      else
        SI.Status := TStreamStatus.None;
      Add(Instance, @SI, PChar(Codec), nil);
      DS := Funcs^.GetDepthCodec(PChar(Codec));
      if DS <> '' then
        Funcs^.AddDepthStream(Instance, 0, NewSizeInt, DepthSizeInt, DS, 0, 0);
      Status := TScanStatus.Success;
    end;
  end;

begin
  if Depth > 0 then
    Exit;
  for I := Low(CodecCfg[Instance]) to High(CodecCfg[Instance]) do
    for J := Low(CodecCfg[Instance, I]) to High(CodecCfg[Instance, I]) do
      if CodecEnabled[I, J] then
        with CodecCfg[Instance, I, J] do
          for X := Low(Structure[0]) to High(Structure[0]) do
            if Structure[0, X].Name = 'Signature' then
            begin
              Pos := 0;
              while BinarySearch(Input, Pos, Size, Structure[0, X].Data,
                Structure[0, X].Size, Pos) do
              begin
                Status := TScanStatus.None;
                LoopPosInt := Pos + Structure[0, X].Size;
                for Y := Low(Structure[0]) to High(Structure[0]) do
                begin
                  if (X <> Y) then
                  begin
                    NI := Structure[0, Y].Position - Structure[0, X].Position;
                    LoopPosInt := Pos + Structure[0, Y].Size + NI;
                    if InRange(Pos + NI, 0, SizeEx - Structure[0, Y].Size) then
                    begin
                      Move((Input + Pos + NI)^, Structure[0, Y].Data^,
                        Structure[0, Y].Size);
                      I64 := 0;
                      EndianMove(Structure[0, Y].Data, @I64,
                        Min(Structure[0, Y].Size, I64.Size), BigEndian);
                      Structure[0, Y].Value := I64.ToDouble;
                    end
                    else
                      Status := TScanStatus.Fail;
                  end;
                  if Status = TScanStatus.Fail then
                    break;
                end;
                for Y := Low(Conditions[0]) to High(Conditions[0]) do
                  if Round(Parser.Evaluate(Conditions[0, Y])) = 0 then
                  begin
                    Status := TScanStatus.Fail;
                    break;
                  end;
                if Status = TScanStatus.Fail then
                begin
                  Inc(Pos);
                  continue;
                end;
                UpdateCounters(CodecCfg[Instance, I, J]);
                for Y := Low(Counter) to High(Counter) do
                begin
                  Counter[Y].Current := Counter[Y].StartV;
                  Counter[Y].Min := Min(Counter[Y].StepV,
                    Counter[Y].EndV - Counter[Y].Current);
                  Counter[Y].Max := Max(Counter[Y].StepV,
                    Counter[Y].EndV - Counter[Y].Current);
                end;
                while CheckCounters(CodecCfg[Instance, I, J]) do
                begin
                  Status := TScanStatus.None;
                  for Y := Low(Structure[1]) to High(Structure[1]) do
                  begin
                    if InRange(LoopPosInt, 0, SizeEx - Structure[1, Y].Size)
                    then
                    begin
                      Move((Input + LoopPosInt)^, Structure[1, Y].Data^,
                        Structure[1, Y].Size);
                      I64 := 0;
                      EndianMove(Structure[1, Y].Data, @I64,
                        Min(Structure[1, Y].Size, I64.Size), BigEndian);
                      Structure[1, Y].Value := I64.ToDouble;
                    end
                    else
                    begin
                      Status := TScanStatus.Fail;
                      break;
                    end;
                    Inc(LoopPosInt, Structure[1, Y].Size);
                  end;
                  if Status = TScanStatus.Fail then
                    break;
                  for Y := Low(Conditions[1]) to High(Conditions[1]) do
                    if Round(Parser.Evaluate(Conditions[1, Y])) = 0 then
                    begin
                      Status := TScanStatus.Fail;
                      break;
                    end;
                  if Status = TScanStatus.None then
                  begin
                    StreamPosInt1 := Pos +
                      Round(Parser.Evaluate(StreamPosition));
                    StreamPosInt2 := StreamPosInt1;
                    for Y := Low(Structure[2]) to High(Structure[2]) do
                    begin
                      if (Structure[2, Y].BeforeStream = True) then
                      begin
                        if Structure[2, Y].Name = 'Stream' then
                        begin
                          StreamPosInt1 := StreamPosInt2;
                          continue;
                        end;
                        Funcs^.ReadFuture(Instance, StreamPosInt2,
                          Structure[2, Y].Data, Structure[2, Y].Size);
                        I64 := 0;
                        EndianMove(Structure[2, Y].Data, @I64,
                          Min(Structure[2, Y].Size, I64.Size), BigEndian);
                        Structure[2, Y].Value := I64.ToDouble;
                        Inc(StreamPosInt2, Structure[2, Y].Size);
                      end;
                    end;
                    for A := Low(Exprs) to High(Exprs) do
                    begin
                      for B := Low(Exprs) to High(Exprs) do
                        try
                          if A = B then
                            continue;
                          Values[B] := Parser.Evaluate(Exprs[B]);
                        except
                        end;
                      try
                        Values[A] := Parser.Evaluate(Exprs[A]);
                      except
                      end;
                    end;
                    StreamOffsetInt := Round(Parser.Evaluate(StreamOffset));
                    OldSizeInt := Round(Parser.Evaluate(OldSize));
                    NewSizeInt := Round(Parser.Evaluate(NewSize));
                    DepthSizeInt := Round(Parser.Evaluate(DepthSize));
                    for Y := Low(Structure[2]) to High(Structure[2]) do
                    begin
                      if (Structure[2, Y].BeforeStream = False) then
                      begin
                        Funcs^.ReadFuture(Instance, StreamPosInt2 + OldSizeInt,
                          Structure[2, Y].Data, Structure[2, Y].Size);
                        I64 := 0;
                        EndianMove(Structure[2, Y].Data, @I64,
                          Min(Structure[2, Y].Size, I64.Size), BigEndian);
                        Structure[2, Y].Value := I64.ToDouble;
                        Inc(StreamPosInt2, Structure[2, Y].Size);
                      end;
                    end;
                    for Y := Low(Conditions[2]) to High(Conditions[2]) do
                      if Round(Parser.Evaluate(Conditions[2, Y])) = 0 then
                      begin
                        Status := TScanStatus.Fail;
                        break;
                      end;
                  end;
                  if Status = TScanStatus.None then
                    DoAddStream(CodecCfg[Instance, I, J]);
                  UpdateCounters(CodecCfg[Instance, I, J]);
                  for Y := Low(Counter) to High(Counter) do
                  begin
                    Counter[Y].Current := Counter[Y].Current + Counter[Y].StepV;
                    Counter[Y].Min :=
                      Min(Counter[Y].StepV,
                      Counter[Y].EndV - Counter[Y].Current);
                    Counter[Y].Max :=
                      Max(Counter[Y].StepV,
                      Counter[Y].EndV - Counter[Y].Current);
                  end;
                end;
                Inc(Pos);
              end;
            end;
end;

function ConfigScan2(Instance, Depth: Integer; Input: Pointer; Size: NativeInt;
  StreamInfo: PStrInfo2; Offset: PInteger; Output: _PrecompOutput;
  Funcs: PPrecompFuncs): Boolean;
begin
  Result := False;
end;

function ConfigProcess(Instance, Depth: Integer; OldInput, NewInput: Pointer;
  StreamInfo: PStrInfo2; Output: _PrecompOutput; Funcs: PPrecompFuncs): Boolean;
begin
  Result := False;
end;

function ConfigRestore(Instance, Depth: Integer; Input, InputExt: Pointer;
  StreamInfo: _StrInfo3; Output: _PrecompOutput; Funcs: PPrecompFuncs): Boolean;
begin
  Result := False;
end;

procedure DecodeHeader(const Header: String; out Name, Value: String);
begin
  if (Pos('(', Header) > 0) and (Pos(')', Header) > 0) then
  begin
    Name := ReplaceStr(Header.Substring(0, Header.IndexOf('(')), ' ', '');
    Value := Header.Substring(Succ(Header.IndexOf('(')),
      Header.IndexOf(')') - Succ(Header.IndexOf('(')));
  end
  else
    Name := Header;
end;

procedure ConvertHexChr(var S: String);
begin
  S := ReplaceStr(S, '0x', '$');
  S := ReplaceStr(S, '#', '$');
end;

var
  I, J, K, X, Y, Z: Integer;
  SL: TStringList;
  Ini: TMemIniFile;
  Bytes: TBytes;
  S1, S2, S3: String;
  Pos: Integer;
  BStream: Boolean;
  HexValue: Boolean;
  CfgRec: PConfigRec;
  CfgRecArray: PCfgRecDynArray;
  CfgStruct: PCfgStruct;
  CfgCounter: PCfgCounter;
  SList: TStringDynArray;
  RStream: TResourceStream;

initialization

SL := TStringList.Create;
SetLength(CodecCfg, 1);
CfgList := TDirectory.GetFiles(ExpandPath(PluginsPath, True), '*.ini',
  TSearchOption.soTopDirectoryOnly);
for I := Low(CfgList) to High(CfgList) do
begin
  Ini := TMemIniFile.Create(CfgList[I]);
  with Ini do
    try
      if ReadString('StreamList1', 'Name', '') <> '' then
      begin
        if SameText(ChangeFileExt(ExtractFileName(CfgList[I]), ''),
          ChangeFileExt(ExtractFileName(Utils.GetModuleName), '')) then
          FORCEDMETHOD := True;
        S1 := ChangeFileExt(ExtractFileName(CfgList[I]), '');
        Insert(S1, Codec.Names, Length(Codec.Names));
        if not SameText(ChangeFileExt(ExtractFileName(CfgList[I]), ''),
          ChangeFileExt(ExtractFileName(Utils.GetModuleName), '')) then
          if InitCode.UIDLLLoaded then
            XTLAddplugin(S1, PLUGIN_CONFIG);
        SetLength(CodecCfg[0], Succ(Length(CodecCfg[0])));
        CfgRecArray := @CodecCfg[0, Pred(Length(CodecCfg[0]))];
        X := 1;
        while ReadString('StreamList' + X.ToString, 'Name', '') <> '' do
        begin
          J := Length(CodecCfg[0, Pred(Length(CodecCfg[0]))]);
          SetLength(CodecCfg[0, Pred(Length(CodecCfg[0]))], Succ(J));
          CfgRec := @CodecCfg[0, Pred(Length(CodecCfg[0])), J];
          CfgRec^.Parser := TExpressionParser.Create;
          CfgRec^.Name := ReadString('StreamList' + X.ToString, 'Name', '');
          if InitCode.UIDLLLoaded then
          begin
            SList := DecodeStr(CfgRec^.Name, ',');
            for Y := Low(SList) to High(SList) do
              XTLAddCodec(SList[Y]);
          end;
          CfgRec^.Codec := ReadString('StreamList' + X.ToString, 'Codec', '');
          CfgRec^.BigEndian := ReadBool('StreamList' + X.ToString,
            'BigEndian', False);
          for Z := Low(CfgRec^.Structure) to High(CfgRec^.Structure) do
          begin
            case Z of
              0:
                S3 := 'Structure1';
              1:
                S3 := 'StructureN';
              2:
                S3 := 'StructureS';
            end;
            SList := DecodeStr(ReadString('StreamList' + X.ToString, S3,
              ''), ',');
            Pos := 0;
            BStream := True;
            for Y := Low(SList) to High(SList) do
            begin
              K := Length(CodecCfg[0, Pred(Length(CodecCfg[0])),
                J].Structure[Z]);
              SetLength(CodecCfg[0, Pred(Length(CodecCfg[0])), J].Structure
                [Z], Succ(K));
              CfgStruct := @CodecCfg[0, Pred(Length(CodecCfg[0])), J]
                .Structure[Z, K];
              DecodeHeader(SList[Y], S1, S2);
              ConvertHexChr(S2);
              CfgStruct^.Name := S1;
              CfgStruct^.Size :=
                Round(IfThen(S2 <> '', CfgRec^.Parser.Evaluate(S2), 0));
              GetMem(CfgStruct^.Data, CfgStruct^.Size);
              if (Z = 0) and (CfgStruct^.Name = 'Signature') then
              begin
                S1 := ReplaceStr(ReadString('StreamList' + X.ToString,
                  'Signature', '0'), ' ', '');
                ConvertHexChr(S1);
                HexValue := S1[1] = '$';
                if HexValue then
                begin
                  S1 := S1.Substring(1);
                  while S1.Length < (CfgStruct^.Size * 2) do
                    S1.Insert(0, '0');
                  SetLength(Bytes, CfgStruct^.Size);
                  SetLength(Bytes, HexToBin(BytesOf(S1), 0, Bytes, 0,
                    Length(Bytes)));
                  ReverseBytes(@Bytes[0], CfgStruct^.Data, CfgStruct^.Size);
                end
                else
                begin
                  Bytes := BytesOf(S1);
                  SetLength(Bytes, CfgStruct^.Size);
                  Move(Bytes[0], CfgStruct^.Data^, CfgStruct^.Size);
                end;
              end;
              CfgStruct^.Position := Pos;
              CfgStruct^.Value := 0;
              CfgStruct^.BeforeStream := BStream;
              Inc(Pos, CfgStruct^.Size);
              if (Z = 2) and (CfgStruct^.Name = 'Stream') then
              begin
                Pos := 0;
                BStream := False;
              end;
            end;
          end;
          Y := 1;
          while ReadString('StreamList' + X.ToString,
            'CounterStart' + Y.ToString, '') <> '' do
          begin
            K := Length(CodecCfg[0, Pred(Length(CodecCfg[0])), J].Counter);
            SetLength(CodecCfg[0, Pred(Length(CodecCfg[0])),
              J].Counter, Succ(K));
            CfgCounter := @CodecCfg[0, Pred(Length(CodecCfg[0])), J].Counter[K];
            CfgCounter^.StartS := ReadString('StreamList' + X.ToString,
              'CounterStart' + Y.ToString, '');
            CfgCounter^.EndS := ReadString('StreamList' + X.ToString,
              'CounterEnd' + Y.ToString, '');
            CfgCounter^.StepS := ReadString('StreamList' + X.ToString,
              'CounterStep' + Y.ToString, '');
            CfgCounter^.StartV := 0;
            CfgCounter^.EndV := 0;
            CfgCounter^.StepV := 0;
            CfgCounter^.Current := 0;
            CfgCounter^.Min := 0;
            CfgCounter^.Max := 0;
            Inc(Y);
          end;
          CfgRec^.StreamPosition := ReadString('StreamList' + X.ToString,
            'StreamPosition', '');
          CfgRec^.StreamOffset := ReadString('StreamList' + X.ToString,
            'StreamOffset', '0');
          ConvertHexChr(CfgRec^.StreamOffset);
          CfgRec^.OldSize := ReadString('StreamList' + X.ToString,
            'CompressedSize', '0');
          ConvertHexChr(CfgRec^.OldSize);
          CfgRec^.NewSize := ReadString('StreamList' + X.ToString,
            'DecompressedSize', '0');
          ConvertHexChr(CfgRec^.NewSize);
          CfgRec^.DepthSize := ReadString('StreamList' + X.ToString,
            'DepthSize', '0');
          ConvertHexChr(CfgRec^.DepthSize);
          for Z := Low(CfgRec^.Conditions) to High(CfgRec^.Conditions) do
          begin
            case Z of
              0:
                S3 := 'Condition1_';
              1:
                S3 := 'ConditionN_';
              2:
                S3 := 'ConditionS_';
            end;
            Y := 1;
            while ReadString('StreamList' + X.ToString, S3 + Y.ToString,
              '') <> '' do
            begin
              S2 := ReadString('StreamList' + X.ToString, S3 + Y.ToString, '');
              ConvertHexChr(S2);
              Insert(S2, CfgRec^.Conditions[Z], Length(CfgRec^.Conditions[Z]));
              Inc(Y);
            end;
          end;
          ReadSectionValues('StreamList' + X.ToString, SL);
          for J := SL.Count - 1 downto 0 do
          begin
            S1 := SL[J].Substring(0, SL[J].IndexOf('=')).TrimRight;
            S2 := SL[J].Substring(Succ(SL[J].IndexOf('='))).TrimLeft;
            if (IndexText(S1, ['Name', 'Codec', 'BigEndian', 'Signature',
              'Structure1', 'StructureN', 'StructureS']) >= 0) or
              S1.StartsWith('Condition', True) or S1.StartsWith('CounterStart',
              True) or S1.StartsWith('CounterEnd', True) or
              S1.StartsWith('CounterStep', True) then
              SL.Delete(J);
          end;
          SetLength(CfgRec^.Names, SL.Count);
          SetLength(CfgRec^.Exprs, SL.Count);
          SetLength(CfgRec^.Values, SL.Count);
          for J := 0 to SL.Count - 1 do
          begin
            S1 := SL[J].Substring(0, SL[J].IndexOf('=')).TrimRight;
            S2 := SL[J].Substring(Succ(SL[J].IndexOf('='))).TrimLeft;
            CfgRec^.Names[J] := S1;
            CfgRec^.Exprs[J] := S2;
            CfgRec^.Values[J] := 0;
          end;
          Inc(X);
        end;
      end;
    finally
      Free;
    end;
end;

for J := Low(CodecCfg[0]) to High(CodecCfg[0]) do
  for X := Low(CodecCfg[0, J]) to High(CodecCfg[0, J]) do
  begin
    with CodecCfg[0, J, X] do
    begin
      for Z := Low(Structure) to High(Structure) do
        for Y := Low(Structure[Z]) to High(Structure[Z]) do
          Parser.DefineVariable(Structure[Z, Y].Name, @Structure[Z, Y].Value);
      for Y := Low(Counter) to High(Counter) do
      begin
        Parser.DefineVariable('Counter' + Succ(Y).ToString,
          @Counter[Y].Current);
        Parser.DefineVariable('CounterMin' + Succ(Y).ToString, @Counter[Y].Min);
        Parser.DefineVariable('CounterMax' + Succ(Y).ToString, @Counter[Y].Max);
      end;
      for Y := Low(Names) to High(Names) do
        Parser.DefineVariable(Names[Y], @Values[Y]);
    end;
  end;

Codec.Initialised := False;
Codec.Init := @ConfigInit;
Codec.Free := @ConfigFree;
Codec.Parse := @ConfigParse;
Codec.Scan1 := @ConfigScan1;
Codec.Scan2 := @ConfigScan2;
Codec.Process := @ConfigProcess;
Codec.Restore := @ConfigRestore;
SetLength(CodecAvailable, Length(CodecCfg[0]));
SetLength(CodecEnabled, Length(CodecCfg[0]));
for I := Low(CodecCfg[0]) to High(CodecCfg[0]) do
begin
  SetLength(CodecAvailable[I], Length(CodecCfg[0, I]));
  SetLength(CodecEnabled[I], Length(CodecCfg[0, I]));
end;

end.
