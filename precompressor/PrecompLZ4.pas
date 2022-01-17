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
  LZ4Codecs: array of PChar = ['lz4', 'lz4hc'];
  CODEC_COUNT = 2;
  LZ4_CODEC = 0;
  LZ4HC_CODEC = 1;

const
  L_MAXSIZE = 16 * 1024 * 1024;

var
  SOList: array of array [0 .. CODEC_COUNT - 1] of TSOList;
  CodecAvailable, CodecEnabled: TArray<Boolean>;

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
      SOList[I][LZ4_CODEC].Update([1], True);
    end
    else if (CompareText(S, LZ4Codecs[LZ4HC_CODEC]) = 0) and LZ4DLL.DLLLoaded
    then
    begin
      CodecEnabled[LZ4HC_CODEC] := True;
      if Funcs^.GetParam(Command, X, 'l') <> '' then
        for I := Low(SOList) to High(SOList) do
          SOList[I][LZ4HC_CODEC].Update
            ([StrToInt(Funcs^.GetParam(Command, X, 'l'))], True);
    end;
    Inc(X);
  end;
  SetLength(Options, 0);
  for I := 3 to 12 do
    Insert(I, Options, Length(Options));
  for X := Low(SOList) to High(SOList) do
    for Y := Low(SOList[X]) to High(SOList[X]) do
      if SOList[X, Y].Count = 0 then
        SOList[X, Y].Update(Options);
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
  I := 0;
  while Funcs^.GetCodec(Command, I, False) <> '' do
  begin
    S := Funcs^.GetCodec(Command, I, False);
    if (CompareText(S, LZ4Codecs[LZ4_CODEC]) = 0) and LZ4DLL.DLLLoaded then
    begin
      SetBits(Option^, 0, 0, 5);
      Result := True;
    end
    else if (CompareText(S, LZ4Codecs[LZ4HC_CODEC]) = 0) and LZ4DLL.DLLLoaded
    then
    begin
      SetBits(Option^, 1, 0, 5);
      if Funcs^.GetParam(Command, I, 'l') <> '' then
        SetBits(Option^, StrToInt(Funcs^.GetParam(Command, I, 'l')), 5, 7);
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
  Pos: NativeInt;
  LSize: NativeInt;
  P: Integer;
  Frame: Byte;
  CSize, DSize: Integer;
  SI: _StrInfo1;
  DI: TDepthInfo;
  DS: TPrecompCmd;
begin
  if BoolArray(CodecEnabled, False) then
    exit;
  Buffer := Funcs^.Allocator(Instance, L_MAXSIZE);
  Pos := 0;
  LSize := Size - 11;
  while Pos < LSize do
  begin
    if (PInteger(Input + Pos)^ = $184D2204) then
    begin
      P := 0;
      Inc(P, 4);
      Frame := PByte(Input + Pos + P)^;
      if Frame = $64 then
      begin
        Inc(P, 3);
        CSize := PInteger(Input + Pos + P)^;
        Inc(P, 4);
        DSize := LZ4_decompress_safe((Input + Pos + P), Buffer, CSize,
          L_MAXSIZE);
        if CSize > DSize then
        begin
          Inc(Pos);
          continue;
        end
        else
        begin
          Output(Instance, Buffer, DSize);
          SI.Position := Pos + P;
          SI.OldSize := CSize;
          SI.NewSize := DSize;
          SI.Option := 0;
          SI.Status := TStreamStatus.None;
          Add(Instance, @SI, nil, nil);
          Inc(Pos, P);
          continue;
        end;
      end;
    end;
    Inc(Pos);
  end;
  DI := Funcs^.GetDepthInfo(Instance);
  DS := Funcs^.GetCodec(DI.Codec, 0, False);
  if DS <> '' then
  begin
    if IndexTextW(@DS[0], LZ4Codecs) < 0 then
      exit;
  end
  else
    exit;
  if (DI.OldSize <> Size) or (DI.OldSize >= DI.NewSize) then
    exit;
  Buffer := Funcs^.Allocator(Instance, DI.NewSize);
  DSize := LZ4_decompress_safe(Input, Buffer, Size, DI.NewSize);
  if (DSize > DI.OldSize) then
  begin
    Output(Instance, Buffer, DSize);
    SI.Position := 0;
    SI.OldSize := DI.OldSize;
    SI.NewSize := DSize;
    SI.Option := 0;
    if System.Pos(SPrecompSep2, DI.Codec) > 0 then
      SI.Status := TStreamStatus.Predicted
    else
      SI.Status := TStreamStatus.None;
    Add(Instance, @SI, DI.Codec, nil);
  end;
end;

function LZ4Scan2(Instance, Depth: Integer; Input: Pointer; Size: NativeInt;
  StreamInfo: PStrInfo2; Output: _PrecompOutput; Funcs: PPrecompFuncs): Boolean;
var
  Buffer: PByte;
  X: Integer;
  Res: Integer;
begin
  Result := False;
  X := GetBits(StreamInfo^.Option, 0, 5);
  if StreamInfo^.NewSize <= 0 then
    exit;
  Buffer := Funcs^.Allocator(Instance, StreamInfo^.NewSize);
  case X of
    LZ4_CODEC, LZ4HC_CODEC:
      Res := LZ4_decompress_safe(Input, Buffer, StreamInfo^.OldSize,
        StreamInfo^.NewSize);
  else
    Res := LZ4_decompress_safe(Input, Buffer, StreamInfo^.OldSize,
      StreamInfo^.NewSize);
  end;
  if Res = StreamInfo^.NewSize then
  begin
    StreamInfo^.NewSize := Res;
    Output(Instance, Buffer, Res);
    Result := True;
  end;
end;

function LZ4Process(Instance, Depth: Integer; OldInput, NewInput: Pointer;
  StreamInfo: PStrInfo2; Output: _PrecompOutput; Funcs: PPrecompFuncs): Boolean;
var
  Buffer, Ptr: PByte;
  I: Integer;
  X: Integer;
  Res1: Integer;
  Res2: NativeUInt;
begin
  Result := False;
  X := GetBits(StreamInfo^.Option, 0, 5);
  if BoolArray(CodecAvailable, False) or (CodecAvailable[X] = False) then
    exit;
  Buffer := Funcs^.Allocator(Instance, StreamInfo^.NewSize);
  SOList[Instance][X].Index := 0;
  while SOList[Instance][X].Get(I) >= 0 do
  begin
    if StreamInfo^.Status = TStreamStatus.Predicted then
      if GetBits(StreamInfo^.Option, 5, 7) <> I then
        continue;
    case X of
      LZ4_CODEC:
        Res1 := LZ4_compress_default(NewInput, Buffer, StreamInfo^.NewSize,
          StreamInfo^.NewSize);
      LZ4HC_CODEC:
        Res1 := LZ4_compress_HC(NewInput, Buffer, StreamInfo^.NewSize,
          StreamInfo^.NewSize, I);
    end;
    Result := (Res1 = StreamInfo^.OldSize) and CompareMem(OldInput, Buffer,
      StreamInfo^.OldSize);
    if Result then
    begin
      SetBits(StreamInfo^.Option, I, 5, 7);
      SOList[Instance][X].Add(I);
      break;
    end;
  end;
  if (Result = False) and ((StreamInfo^.Status = TStreamStatus.Predicted) or
    (SOList[Instance][X].Count = 1)) then
  begin
    Buffer := Funcs^.Allocator(Instance, Res1 + Max(StreamInfo^.OldSize, Res1));
    Res2 := PrecompEncodePatch(OldInput, StreamInfo^.OldSize, Buffer, Res1,
      Buffer + Res1, Max(StreamInfo^.OldSize, Res1));
    if (Res2 > 0) and ((Res2 / Max(StreamInfo^.OldSize, Res1)) <= DIFF_TOLERANCE)
    then
    begin
      Output(Instance, Buffer + Res1, Res2);
      SetBits(StreamInfo^.Option, 1, 31, 1);
      SOList[Instance][X].Add(I);
      Result := True;
    end;
  end;
end;

function LZ4Restore(Instance, Depth: Integer; Input, InputExt: Pointer;
  StreamInfo: _StrInfo3; Output: _PrecompOutput; Funcs: PPrecompFuncs): Boolean;
var
  Buffer: PByte;
  X: Integer;
  Res1: Integer;
  Res2: NativeUInt;
begin
  Result := False;
  X := GetBits(StreamInfo.Option, 0, 5);
  if BoolArray(CodecAvailable, False) or (CodecAvailable[X] = False) then
    exit;
  Buffer := Funcs^.Allocator(Instance, StreamInfo.NewSize);
  case X of
    LZ4_CODEC:
      Res1 := LZ4_compress_default(Input, Buffer, StreamInfo.NewSize,
        StreamInfo.NewSize);
    LZ4HC_CODEC:
      Res1 := LZ4_compress_HC(Input, Buffer, StreamInfo.NewSize,
        StreamInfo.NewSize, GetBits(StreamInfo.Option, 5, 7));
  end;
  if GetBits(StreamInfo.Option, 31, 1) = 1 then
  begin
    Buffer := Funcs^.Allocator(Instance, Res1 + StreamInfo.OldSize);
    Res2 := PrecompDecodePatch(InputExt, StreamInfo.ExtSize, Buffer, Res1,
      Buffer + Res1, StreamInfo.OldSize);
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
