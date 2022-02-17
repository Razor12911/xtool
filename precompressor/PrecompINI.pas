unit PrecompINI;

interface

uses
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

  PConfigRec = ^TConfigRec;

  TConfigRec = record
    Parser: TExpressionParser;
    Name, Codec: String;
    Resource: Integer;
    BigEndian: Boolean;
    Structure: TArray<TCfgStruct>;
    StreamOffset, OldSize, NewSize: String;
    Names, Exprs: TArray<String>;
    Values: TArray<Double>;
    Conditions: TArray<String>;
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
  ParamsSet: Boolean;
begin
  Result := True;
  ParamsSet := False;
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
            SetLength(Structure, Length(CodecCfg[0, J, X].Structure));
            for Y := Low(Structure) to High(Structure) do
            begin
              Structure[Y].Name := CodecCfg[0, J, X].Structure[Y].Name;
              Structure[Y].Position := CodecCfg[0, J, X].Structure[Y].Position;
              Structure[Y].Size := CodecCfg[0, J, X].Structure[Y].Size;
              Structure[Y].Value := CodecCfg[0, J, X].Structure[Y].Value;
              Structure[Y].BeforeStream := CodecCfg[0, J, X].Structure[Y]
                .BeforeStream;
              GetMem(Structure[Y].Data, Structure[Y].Size);
              Move(CodecCfg[0, J, X].Structure[Y].Data^, Structure[Y].Data^,
                Structure[Y].Size);
            end;
            StreamOffset := CodecCfg[0, J, X].StreamOffset;
            OldSize := CodecCfg[0, J, X].OldSize;
            NewSize := CodecCfg[0, J, X].NewSize;
            SetLength(Names, Length(CodecCfg[0, J, X].Names));
            SetLength(Exprs, Length(CodecCfg[0, J, X].Exprs));
            SetLength(Values, Length(CodecCfg[0, J, X].Values));
            for Y := Low(Names) to High(Names) do
            begin
              Names[Y] := CodecCfg[0, J, X].Names[Y];
              Exprs[Y] := CodecCfg[0, J, X].Exprs[Y];
              Values[Y] := CodecCfg[0, J, X].Values[Y];
            end;
            SetLength(Conditions, Length(CodecCfg[0, J, X].Conditions));
            for Y := Low(Conditions) to High(Conditions) do
              Conditions[Y] := CodecCfg[0, J, X].Conditions[Y];
            for Y := Low(Names) to High(Names) do
              Parser.DefineVariable(Names[Y], @Values[Y]);
            for Y := Low(Structure) to High(Structure) do
              Parser.DefineVariable(Structure[Y].Name, @Structure[Y].Value);
          end;
        end;
  X := 0;
  while Funcs^.GetCodec(Command, X, False) <> '' do
  begin
    S := Funcs^.GetCodec(Command, X, False);
    for Y := Low(Codec.Names) to High(Codec.Names) do
      if CompareText(S, Codec.Names[Y]) = 0 then
      begin
        for I := Low(CodecEnabled[Y]) to High(CodecEnabled[Y]) do
          CodecEnabled[Y][I] := True;
        for Z := Low(CodecCfg[0, Y]) to High(CodecCfg[0, Y]) do
          if Funcs^.GetParam(Command, X, PChar(CodecCfg[0, Y, Z].Name)) <> ''
          then
          begin
            if not ParamsSet then
            begin
              for I := Low(CodecEnabled[Y]) to High(CodecEnabled[Y]) do
                CodecEnabled[Y][I] := False;
              ParamsSet := True;
            end;
            CodecEnabled[Y, Z] := True;
          end;
        break;
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
  X, Y: Integer;
begin
  for I := Low(CodecCfg) to High(CodecCfg) do
    for J := Low(CodecCfg[I]) to High(CodecCfg[I]) do
      for X := Low(CodecCfg[I, J]) to High(CodecCfg[I, J]) do
        with CodecCfg[I, J, X] do
        begin
          if I > 0 then
          begin
            for Y := Low(Structure) to High(Structure) do
              FreeMem(Structure[Y].Data);
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
  StreamPosInt, StreamOffsetInt, OldSizeInt, NewSizeInt: NativeInt;
  SI: _StrInfo1;
begin
  if Depth > 0 then
    exit;
  for I := Low(CodecCfg[Instance]) to High(CodecCfg[Instance]) do
    for J := Low(CodecCfg[Instance, I]) to High(CodecCfg[Instance, I]) do
      if CodecEnabled[I, J] then
        with CodecCfg[Instance, I, J] do
          for X := Low(Structure) to High(Structure) do
            if Structure[X].Name = 'Signature' then
            begin
              Pos := 0;
              while BinarySearch(Input, Pos, Size, Structure[X].Data,
                Structure[X].Size, Pos) do
              begin
                Status := TScanStatus.None;
                StreamPosInt := Pos;
                for Y := Low(Structure) to High(Structure) do
                begin
                  if (X <> Y) and (Structure[Y].BeforeStream = True) then
                  begin
                    NI := Structure[Y].Position - Structure[X].Position;
                    if Structure[Y].Name = 'Stream' then
                    begin
                      StreamPosInt := Pos + NI;
                      continue;
                    end;
                    if InRange(Pos + NI, 0, SizeEx - Structure[Y].Size) then
                    begin
                      Move((Input + Pos + NI)^, Structure[Y].Data^,
                        Structure[Y].Size);
                      I64 := 0;
                      EndianMove(Structure[Y].Data, @I64,
                        Min(Structure[Y].Size, I64.Size), BigEndian);
                      Structure[Y].Value := I64.ToDouble;
                    end
                    else
                      Status := TScanStatus.Fail;
                  end;
                  if Status = TScanStatus.Fail then
                    break;
                end;
                if Status = TScanStatus.Fail then
                begin
                  Inc(Pos);
                  continue;
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
                for Y := Low(Structure) to High(Structure) do
                begin
                  if (X <> Y) and (Structure[Y].BeforeStream = False) then
                  begin
                    NI := Structure[Y].Position - Structure[X].Position +
                      StreamOffsetInt + OldSizeInt;
                    if InRange(Pos + NI, 0, SizeEx - Structure[Y].Size) then
                    begin
                      Move((Input + Pos + NI)^, Structure[Y].Data^,
                        Structure[Y].Size);
                      I64 := 0;
                      EndianMove(Structure[Y].Data, @I64,
                        Min(Structure[Y].Size, I64.Size), BigEndian);
                      Structure[Y].Value := I64.ToDouble;
                    end
                    else
                      Status := TScanStatus.Fail;
                  end;
                  if Status = TScanStatus.Fail then
                    break;
                end;
                for Y := Low(Conditions) to High(Conditions) do
                begin
                  if Round(Parser.Evaluate(Conditions[Y])) = 0 then
                    break;
                end;
                if (Length(Conditions) = 0) or (Y = High(Conditions)) then
                begin
                  Output(Instance, nil, -1);
                  SI.Position := StreamPosInt + StreamOffsetInt;
                  SI.OldSize := OldSizeInt;
                  SI.NewSize := NewSizeInt;
                  SI.Resource := Resource;
                  SI.Option := 0;
                  if System.Pos(SPrecompSep2, Codec) > 0 then
                    SI.Status := TStreamStatus.Predicted
                  else
                    SI.Status := TStreamStatus.None;
                  Add(Instance, @SI, PChar(Codec), nil);
                  Inc(Pos, Max(OldSizeInt, 1));
                  // fix this
                  Status := TScanStatus.Success;
                end;
                if Status <> TScanStatus.Success then
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
  I, J, X, Y: Integer;
  SL: TStringList;
  Bytes: TBytes;
  S1, S2: String;
  Pos: Integer;
  BStream: Boolean;
  HexValue: Boolean;
  CfgRec: PConfigRec;
  CfgRecArray: PCfgRecDynArray;
  CfgStruct: PCfgStruct;
  SList: TStringDynArray;
  PStr1: PAnsiChar;
  PStr2: PString;

initialization

CfgList := TDirectory.GetFiles(ExtractFilePath(Utils.GetModuleName), '*.ini',
  TSearchOption.soTopDirectoryOnly);
SL := TStringList.Create;
SetLength(CodecCfg, 1);
for I := Low(CfgList) to High(CfgList) do
begin
  with TIniFile.Create(CfgList[I]) do
    try
      if ReadString('Stream1', 'Name', '') <> '' then
      begin
        S1 := ChangeFileExt(ExtractFileName(CfgList[I]), '');
        Insert(S1, Codec.Names, Length(Codec.Names));
        New(CfgRecArray);
        X := 1;
        while ReadString('Stream' + X.ToString, 'Name', '') <> '' do
        begin
          New(CfgRec);
          CfgRec^.Parser := TExpressionParser.Create;
          CfgRec^.Name := ReadString('Stream' + X.ToString, 'Name', '');
          CfgRec^.Codec := ReadString('Stream' + X.ToString, 'Codec', '');
          CfgRec^.BigEndian := ReadBool('Stream' + X.ToString,
            'BigEndian', False);
          SList := DecodeStr(ReadString('Stream' + X.ToString, 'Structure',
            ''), ',');
          Pos := 0;
          BStream := True;
          for Y := Low(SList) to High(SList) do
          begin
            New(CfgStruct);
            DecodeHeader(SList[Y], S1, S2);
            ConvertHexChr(S2);
            CfgStruct^.Name := S1;
            CfgStruct^.Size :=
              Round(IfThen(S2 <> '', CfgRec^.Parser.Evaluate(S2), 0));
            GetMem(CfgStruct^.Data, CfgStruct^.Size);
            if CfgStruct^.Name = 'Signature' then
            begin
              S1 := ReplaceStr(ReadString('Stream' + X.ToString, 'Signature',
                '0'), ' ', '');
              ConvertHexChr(S1);
              HexValue := S1[1] = '$';
              if HexValue then
              begin
                S1 := S1.Substring(1);
                SetLength(Bytes, CfgStruct^.Size);
                SetLength(Bytes, HexToBin(BytesOf(S1), 0, Bytes, 0,
                  Length(Bytes)));
                if CfgRec^.BigEndian then
                  Move(Bytes[0], CfgStruct^.Data^, CfgStruct^.Size)
                else
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
            if (CfgStruct^.Name = 'Stream') or (CfgStruct^.Size > 0) then
              Insert(CfgStruct^, CfgRec^.Structure, Length(CfgRec^.Structure));
            Inc(Pos, CfgStruct^.Size);
            if CfgStruct^.Name = 'Stream' then
            begin
              Pos := 0;
              BStream := False;
            end;
          end;
          CfgRec^.StreamOffset := ReadString('Stream' + X.ToString,
            'StreamOffset', '0');
          ConvertHexChr(CfgRec^.StreamOffset);
          CfgRec^.OldSize := ReadString('Stream' + X.ToString,
            'CompressedSize', '0');
          ConvertHexChr(CfgRec^.OldSize);
          CfgRec^.NewSize := ReadString('Stream' + X.ToString,
            'DecompressedSize', '0');
          ConvertHexChr(CfgRec^.NewSize);
          Y := 1;
          while ReadString('Stream' + X.ToString, 'Condition' + Y.ToString,
            '') <> '' do
          begin
            New(PStr2);
            PStr2^ := ReadString('Stream' + X.ToString,
              'Condition' + Y.ToString, '');
            ConvertHexChr(PStr2^);
            Insert(PStr2^, CfgRec^.Conditions, Length(CfgRec^.Conditions));
            Inc(Y);
          end;
          ReadSectionValues('Stream' + X.ToString, SL);
          for J := SL.Count - 1 downto 0 do
          begin
            S1 := SL[J].Substring(0, SL[J].IndexOf('=')).TrimRight;
            S2 := SL[J].Substring(Succ(SL[J].IndexOf('='))).TrimLeft;
            if (IndexText(S1, ['Name', 'Codec', 'BigEndian', 'Signature',
              'Structure']) >= 0) or S1.StartsWith('Condition', True) then
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
          Insert(CfgRec^, CfgRecArray^, Length(CfgRecArray^));
          Inc(X);
        end;
        Insert(CfgRecArray^, CodecCfg[0], Length(CodecCfg[0]));
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
      for Y := Low(Names) to High(Names) do
        Parser.DefineVariable(Names[Y], @Values[Y]);
      for Y := Low(Structure) to High(Structure) do
        Parser.DefineVariable(Structure[Y].Name, @Structure[Y].Value);
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
