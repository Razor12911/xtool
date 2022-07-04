unit PrecompLZ4;

interface

uses
  LZ4DLL, XDeltaDLL,
  Utils,
  PrecompUtils,
  System.SysUtils, System.StrUtils, System.Classes, System.Math;

var
  Codec: TPrecompressor;

implementation

const
  LZ4Codecs: array of PChar = ['lz4', 'lz4hc', 'lz4f'];
  CODEC_COUNT = 3;
  LZ4_CODEC = 0;
  LZ4HC_CODEC = 1;
  LZ4F_CODEC = 2;

const
  L_MAXSIZE = 16 * 1024 * 1024;
  L_BLOCKSIZE = 0;
  L_BLOCKDEPENDENCY = 0;
  L_ACCELERATION = 1;

var
  SOList: array of array [0 .. CODEC_COUNT - 1] of TSOList;
  CodecAvailable, CodecEnabled: TArray<Boolean>;
  LBlockSize: Integer = L_BLOCKSIZE;
  LBlockDependency: Integer = L_BLOCKDEPENDENCY;
  LAcceleration: Integer = L_ACCELERATION;

function LZ4Init(Command: PChar; Count: Integer; Funcs: PPrecompFuncs): Boolean;
var
  I: Integer;
  Options: TArray<Integer>;
  S: String;
  X, Y: Integer;
begin
  Result := True;
  SetLength(SOList, Count);
  for X := Low(SOList) to High(SOList) do
    for Y := Low(SOList[X]) to High(SOList[X]) do
      SOList[X, Y] := TSOList.Create([], TSOMethod.MTF);
  for X := Low(CodecAvailable) to High(CodecAvailable) do
  begin
    CodecAvailable[X] := False;
    CodecEnabled[X] := False;
  end;
  for X := Low(CodecAvailable) to High(CodecAvailable) do
    CodecAvailable[X] := LZ4DLL.DLLLoaded;
  X := 0;
  while Funcs^.GetCodec(Command, X, False) <> '' do
  begin
    S := Funcs^.GetCodec(Command, X, False);
    if (CompareText(S, LZ4Codecs[LZ4_CODEC]) = 0) and LZ4DLL.DLLLoaded then
    begin
      CodecEnabled[LZ4_CODEC] := True;
      if Funcs^.GetParam(Command, X, 'a') <> '' then
        LAcceleration := StrToInt(Funcs^.GetParam(Command, X, 'a'));
    end
    else if (CompareText(S, LZ4Codecs[LZ4HC_CODEC]) = 0) and LZ4DLL.DLLLoaded
    then
    begin
      CodecEnabled[LZ4HC_CODEC] := True;
      if Funcs^.GetParam(Command, X, 'l') <> '' then
        for I := Low(SOList) to High(SOList) do
          SOList[I][LZ4HC_CODEC].Update
            ([StrToInt(Funcs^.GetParam(Command, X, 'l'))], True);
    end
    else if (CompareText(S, LZ4Codecs[LZ4F_CODEC]) = 0) and LZ4DLL.DLLLoaded
    then
    begin
      CodecEnabled[LZ4F_CODEC] := True;
      if Funcs^.GetParam(Command, X, 'l') <> '' then
        for I := Low(SOList) to High(SOList) do
          SOList[I][LZ4F_CODEC].Update
            ([StrToInt(Funcs^.GetParam(Command, X, 'l'))], True);
      if Funcs^.GetParam(Command, X, 'b') <> '' then
        LBlockSize := StrToInt(Funcs^.GetParam(Command, X, 'b')) - 4;
      if Funcs^.GetParam(Command, X, 'd') <> '' then
        LBlockDependency := StrToInt(Funcs^.GetParam(Command, X, 'd'));
    end;
    Inc(X);
  end;
  for X := Low(SOList) to High(SOList) do
    if SOList[X, LZ4_CODEC].Count = 0 then
      SOList[X, LZ4_CODEC].Update([1]);
  SetLength(Options, 0);
  for I := 2 to 12 do
    Insert(I, Options, Length(Options));
  for X := Low(SOList) to High(SOList) do
    if SOList[X, LZ4HC_CODEC].Count = 0 then
      SOList[X, LZ4HC_CODEC].Update(Options);
  SetLength(Options, 0);
  for I := 2 to 12 do
    Insert(I, Options, Length(Options));
  for X := Low(SOList) to High(SOList) do
    if SOList[X, LZ4F_CODEC].Count = 0 then
      SOList[X, LZ4F_CODEC].Update(Options);
end;

procedure LZ4Free(Funcs: PPrecompFuncs);
var
  X, Y: Integer;
begin
  for X := Low(SOList) to High(SOList) do
    for Y := Low(SOList[X]) to High(SOList[X]) do
      SOList[X, Y].Free;
end;

function LZ4Parse(Command: PChar; Option: PInteger;
  Funcs: PPrecompFuncs): Boolean;
var
  S: String;
  I: Integer;
begin
  Result := False;
  Option^ := 0;
  SetBits(Option^, LBlockSize, 12, 2);
  SetBits(Option^, LBlockDependency, 14, 1);
  SetBits(Option^, LAcceleration, 15, 7);
  I := 0;
  while Funcs^.GetCodec(Command, I, False) <> '' do
  begin
    S := Funcs^.GetCodec(Command, I, False);
    if (CompareText(S, LZ4Codecs[LZ4_CODEC]) = 0) and LZ4DLL.DLLLoaded then
    begin
      SetBits(Option^, LZ4_CODEC, 0, 5);
      if Funcs^.GetParam(Command, I, 'a') <> '' then
        SetBits(Option^, StrToInt(Funcs^.GetParam(Command, I, 'a')), 15, 7);
      Result := True;
    end
    else if (CompareText(S, LZ4Codecs[LZ4HC_CODEC]) = 0) and LZ4DLL.DLLLoaded
    then
    begin
      SetBits(Option^, LZ4HC_CODEC, 0, 5);
      if Funcs^.GetParam(Command, I, 'l') <> '' then
        SetBits(Option^, StrToInt(Funcs^.GetParam(Command, I, 'l')), 5, 7);
      Result := True;
    end
    else if (CompareText(S, LZ4Codecs[LZ4F_CODEC]) = 0) and LZ4DLL.DLLLoaded
    then
    begin
      SetBits(Option^, LZ4F_CODEC, 0, 5);
      if Funcs^.GetParam(Command, I, 'l') <> '' then
        SetBits(Option^, StrToInt(Funcs^.GetParam(Command, I, 'l')), 5, 7);
      if Funcs^.GetParam(Command, I, 'b') <> '' then
        SetBits(Option^, StrToInt(Funcs^.GetParam(Command, I, 'b')) - 4, 12, 2);
      if Funcs^.GetParam(Command, I, 'd') <> '' then
        SetBits(Option^, StrToInt(Funcs^.GetParam(Command, I, 'd')), 14, 1);
      Result := True;
    end;
    Inc(I);
  end;
end;

procedure LZ4Scan1(Instance, Depth: Integer; Input: PByte;
  Size, SizeEx: NativeInt; Output: _PrecompOutput; Add: _PrecompAdd;
  Funcs: PPrecompFuncs);
var
  Buffer: PByte;
  X, Y: Integer;
  SI: _StrInfo1;
  DI1, DI2: TDepthInfo;
  DS: TPrecompStr;
begin
  DI1 := Funcs^.GetDepthInfo(Instance);
  DS := Funcs^.GetCodec(DI1.Codec, 0, False);
  if DS <> '' then
  begin
    X := IndexTextW(@DS[0], LZ4Codecs);
    if (X < 0) or (DI1.OldSize <> SizeEx) then
      exit;
    if not CodecAvailable[X] then
      exit;
    Y := Max(DI1.NewSize, L_MAXSIZE);
    Buffer := Funcs^.Allocator(Instance, Y);
    case X of
      LZ4_CODEC, LZ4HC_CODEC:
        Y := LZ4_decompress_safe(Input, Buffer, DI1.OldSize, Y);
      LZ4F_CODEC:
        Y := LZ4F_decompress_safe(Input, Buffer, DI1.OldSize, Y);
    end;
    if (Y > DI1.OldSize) then
    begin
      Output(Instance, Buffer, Y);
      SI.Position := 0;
      SI.OldSize := DI1.OldSize;
      SI.NewSize := Y;
      SI.Option := 0;
      SetBits(SI.Option, X, 0, 5);
      SetBits(SI.Option, LBlockSize, 12, 2);
      SetBits(SI.Option, LBlockDependency, 14, 1);
      SetBits(SI.Option, LAcceleration, 15, 7);
      if System.Pos(SPrecompSep2, DI1.Codec) > 0 then
        SI.Status := TStreamStatus.Predicted
      else
        SI.Status := TStreamStatus.None;
      DS := Funcs^.GetDepthCodec(DI1.Codec);
      Move(DS[0], DI2.Codec, SizeOf(DI2.Codec));
      DI2.OldSize := SI.NewSize;
      DI2.NewSize := SI.NewSize;
      Funcs^.LogScan1(LZ4Codecs[GetBits(SI.Option, 0, 5)], SI.Position,
        SI.OldSize, SI.NewSize);
      Add(Instance, @SI, DI1.Codec, @DI2);
    end;
    exit;
  end;
  if BoolArray(CodecEnabled, False) then
    exit;
end;

function LZ4Scan2(Instance, Depth: Integer; Input: Pointer; Size: NativeInt;
  StreamInfo: PStrInfo2; Offset: PInteger; Output: _PrecompOutput;
  Funcs: PPrecompFuncs): Boolean;
var
  Buffer: PByte;
  X: Integer;
  Res: Integer;
begin
  Result := False;
  X := GetBits(StreamInfo^.Option, 0, 5);
  if StreamInfo^.OldSize <= 0 then
    exit;
  StreamInfo^.NewSize := Max(StreamInfo^.NewSize, L_MAXSIZE);
  Buffer := Funcs^.Allocator(Instance, StreamInfo^.NewSize);
  case X of
    LZ4_CODEC, LZ4HC_CODEC:
      Res := LZ4_decompress_safe(Input, Buffer, StreamInfo^.OldSize,
        StreamInfo^.NewSize);
    LZ4F_CODEC:
      Res := LZ4F_decompress_safe(Input, Buffer, StreamInfo^.OldSize,
        StreamInfo^.NewSize);
  end;
  if Res > StreamInfo^.OldSize then
  begin
    Output(Instance, Buffer, Res);
    StreamInfo^.NewSize := Res;
    Funcs^.LogScan2(LZ4Codecs[GetBits(StreamInfo^.Option, 0, 5)],
      StreamInfo^.OldSize, StreamInfo^.NewSize);
    Result := True;
  end;
end;

function LZ4Process(Instance, Depth: Integer; OldInput, NewInput: Pointer;
  StreamInfo: PStrInfo2; Output: _PrecompOutput; Funcs: PPrecompFuncs): Boolean;
var
  Buffer, Ptr: PByte;
  Params: String;
  I: Integer;
  X, Y: Integer;
  Res1: Integer;
  Res2: NativeUInt;
  LZ4FT: LZ4F_preferences_t;
begin
  Result := False;
  X := GetBits(StreamInfo^.Option, 0, 5);
  if BoolArray(CodecAvailable, False) or (CodecAvailable[X] = False) then
    exit;
  Y := LZ4F_compressFrameBound(StreamInfo^.NewSize, nil);
  Buffer := Funcs^.Allocator(Instance, Y);
  SOList[Instance][X].Index := 0;
  while SOList[Instance][X].Get(I) >= 0 do
  begin
    if StreamInfo^.Status = TStreamStatus.Predicted then
      if GetBits(StreamInfo^.Option, 5, 7) <> I then
        continue;
    Params := '';
    case X of
      LZ4_CODEC:
        begin
          Params := 'a' + GetBits(StreamInfo^.Option, 15, 7).ToString;
          Res1 := LZ4_compress_fast(NewInput, Buffer, StreamInfo^.NewSize, Y,
            GetBits(StreamInfo^.Option, 15, 7));
        end;
      LZ4HC_CODEC:
        begin
          Params := 'l' + I.ToString;
          Res1 := LZ4_compress_HC(NewInput, Buffer, StreamInfo^.NewSize, Y, I);
        end;
      LZ4F_CODEC:
        begin
          FillChar(LZ4FT, SizeOf(LZ4F_preferences_t), 0);
          LZ4FT.compressionLevel := I;
          LZ4FT.frameInfo.blockSizeID :=
            LZ4F_blockSizeID_t(GetBits(StreamInfo^.Option, 12, 2) + 4);
          LZ4FT.frameInfo.blockMode :=
            LZ4F_blockMode_t(GetBits(StreamInfo^.Option, 14, 1));
          Params := 'l' + I.ToString + ':' + 'b' +
            (GetBits(StreamInfo^.Option, 12, 2) + 4).ToString + ':' + 'd' +
            GetBits(StreamInfo^.Option, 14, 1).ToString;
          Res1 := LZ4F_compressFrame(Buffer, Y, NewInput,
            StreamInfo^.NewSize, @LZ4FT);
        end;
    end;
    Result := (Res1 = StreamInfo^.OldSize) and CompareMem(OldInput, Buffer,
      StreamInfo^.OldSize);
    Funcs^.LogProcess(LZ4Codecs[GetBits(StreamInfo^.Option, 0, 5)],
      PChar(Params), StreamInfo^.OldSize, StreamInfo^.NewSize, Res1, Result);
    if Result or (StreamInfo^.Status = TStreamStatus.Predicted) then
      break;
  end;
  if (Result = False) and ((StreamInfo^.Status = TStreamStatus.Predicted) or
    (SOList[Instance][X].Count = 1)) and (DIFF_TOLERANCE > 0) then
  begin
    Buffer := Funcs^.Allocator(Instance, Res1 + Max(StreamInfo^.OldSize, Res1));
    Res2 := PrecompEncodePatch(OldInput, StreamInfo^.OldSize, Buffer, Res1,
      Buffer + Res1, Max(StreamInfo^.OldSize, Res1));
    Funcs^.LogPatch1(StreamInfo^.OldSize, Res1, Res2,
      Funcs^.AcceptPatch(StreamInfo^.OldSize, Res1, Res2));
    if Funcs^.AcceptPatch(StreamInfo^.OldSize, Res1, Res2) then
    begin
      Output(Instance, Buffer + Res1, Res2);
      SetBits(StreamInfo^.Option, 1, 31, 1);
      SOList[Instance][X].Add(I);
      Result := True;
    end;
  end;
  if Result then
  begin
    SetBits(StreamInfo^.Option, I, 5, 7);
    SOList[Instance][X].Add(I);
  end;
end;

function LZ4Restore(Instance, Depth: Integer; Input, InputExt: Pointer;
  StreamInfo: _StrInfo3; Output: _PrecompOutput; Funcs: PPrecompFuncs): Boolean;
var
  Buffer: PByte;
  Params: String;
  X: Integer;
  Res1: Integer;
  Res2: NativeUInt;
  LZ4FT: LZ4F_preferences_t;
begin
  Result := False;
  X := GetBits(StreamInfo.Option, 0, 5);
  if BoolArray(CodecAvailable, False) or (CodecAvailable[X] = False) then
    exit;
  Params := '';
  Buffer := Funcs^.Allocator(Instance,
    LZ4F_compressFrameBound(StreamInfo.NewSize, nil));
  case X of
    LZ4_CODEC:
      begin
        Params := 'a' + GetBits(StreamInfo.Option, 15, 7).ToString;
        Res1 := LZ4_compress_fast(Input, Buffer, StreamInfo.NewSize,
          LZ4F_compressFrameBound(StreamInfo.NewSize, nil),
          GetBits(StreamInfo.Option, 15, 7));
      end;
    LZ4HC_CODEC:
      begin
        Params := 'l' + GetBits(StreamInfo.Option, 5, 7).ToString;
        Res1 := LZ4_compress_HC(Input, Buffer, StreamInfo.NewSize,
          LZ4F_compressFrameBound(StreamInfo.NewSize, nil),
          GetBits(StreamInfo.Option, 5, 7));
      end;
    LZ4F_CODEC:
      begin
        FillChar(LZ4FT, SizeOf(LZ4F_preferences_t), 0);
        LZ4FT.compressionLevel := GetBits(StreamInfo.Option, 5, 7);
        LZ4FT.frameInfo.blockSizeID :=
          LZ4F_blockSizeID_t(GetBits(StreamInfo.Option, 12, 2) + 4);
        LZ4FT.frameInfo.blockMode :=
          LZ4F_blockMode_t(GetBits(StreamInfo.Option, 14, 1));
        Params := 'l' + GetBits(StreamInfo.Option, 5, 7).ToString + ':' + 'b' +
          (GetBits(StreamInfo.Option, 12, 2) + 4).ToString + ':' + 'd' +
          GetBits(StreamInfo.Option, 14, 1).ToString;
        Res1 := LZ4F_compressFrame(Buffer,
          LZ4F_compressFrameBound(StreamInfo.NewSize, nil), Input,
          StreamInfo.NewSize, @LZ4FT);
      end;
  end;
  Funcs^.LogRestore(LZ4Codecs[GetBits(StreamInfo.Option, 0, 5)], PChar(Params),
    StreamInfo.OldSize, StreamInfo.NewSize, Res1, True);
  if GetBits(StreamInfo.Option, 31, 1) = 1 then
  begin
    Buffer := Funcs^.Allocator(Instance, Res1 + StreamInfo.OldSize);
    Res2 := PrecompDecodePatch(InputExt, StreamInfo.ExtSize, Buffer, Res1,
      Buffer + Res1, StreamInfo.OldSize);
    Funcs^.LogPatch2(StreamInfo.OldSize, Res1, StreamInfo.ExtSize, Res2 > 0);
    if Res2 > 0 then
    begin
      Output(Instance, Buffer + Res1, StreamInfo.OldSize);
      Result := True;
    end;
    exit;
  end;
  if Res1 = StreamInfo.OldSize then
  begin
    Output(Instance, Buffer, StreamInfo.OldSize);
    Result := True;
  end;
end;

var
  I: Integer;

initialization

Codec.Names := [];
for I := Low(LZ4Codecs) to High(LZ4Codecs) do
begin
  Codec.Names := Codec.Names + [LZ4Codecs[I]];
  StockMethods.Add(LZ4Codecs[I]);
end;
Codec.Initialised := False;
Codec.Init := @LZ4Init;
Codec.Free := @LZ4Free;
Codec.Parse := @LZ4Parse;
Codec.Scan1 := @LZ4Scan1;
Codec.Scan2 := @LZ4Scan2;
Codec.Process := @LZ4Process;
Codec.Restore := @LZ4Restore;
SetLength(CodecAvailable, Length(Codec.Names));
SetLength(CodecEnabled, Length(Codec.Names));

end.
