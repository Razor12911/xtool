unit PrecompZSTD;

interface

uses
  ZSTDDLL,
  Utils,
  PrecompUtils,
  System.SysUtils, System.Classes, System.Math;

var
  Codec: TPrecompressor;

implementation

const
  ZSTDCodecs: array of PChar = ['zstd'];
  CODEC_COUNT = 1;
  ZSTD_CODEC = 0;

const
  Z_MAXSIZE = 16 * 1024 * 1024;

var
  SOList: array of array [0 .. CODEC_COUNT - 1] of TSOList;
  cctx, dctx: array of Pointer;
  // cdict, ddict: Pointer;
  DStream: TMemoryStream;
  CodecAvailable, CodecEnabled: TArray<Boolean>;

function ZSTDInit(Command: PChar; Count: Integer; Funcs: PPrecompFuncs)
  : Boolean;
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
    CodecAvailable[X] := ZSTDDLL.DLLLoaded;
  X := 0;
  while Funcs^.GetCodec(Command, X, False) <> '' do
  begin
    S := Funcs^.GetCodec(Command, X, False);
    if (CompareText(S, ZSTDCodecs[ZSTD_CODEC]) = 0) and ZSTDDLL.DLLLoaded then
    begin
      CodecEnabled[ZSTD_CODEC] := True;
      if Funcs^.GetParam(Command, X, 'l') <> '' then
        for I := Low(SOList) to High(SOList) do
          SOList[I][ZSTD_CODEC].Update
            ([StrToInt(Funcs^.GetParam(Command, X, 'l'))], True);
    end;
    Inc(X);
  end;
  if CodecAvailable[ZSTD_CODEC] then
  begin
    SetLength(cctx, Count);
    SetLength(dctx, Count);
    for X := Low(cctx) to High(cctx) do
    begin
      cctx[X] := ZSTD_createCCtx;
      dctx[X] := ZSTD_createDCtx;
    end;
  end;
  SetLength(Options, 0);
  for I := 1 to 22 do
    Insert(I, Options, Length(Options));
  for X := Low(SOList) to High(SOList) do
    for Y := Low(SOList[X]) to High(SOList[X]) do
      if SOList[X, Y].Count = 0 then
        SOList[X, Y].Update(Options);
end;

procedure ZSTDFree(Funcs: PPrecompFuncs);
var
  X, Y: Integer;
begin
  for X := Low(SOList) to High(SOList) do
    for Y := Low(SOList[X]) to High(SOList[X]) do
      SOList[X, Y].Free;
  if CodecAvailable[ZSTD_CODEC] then
  begin
    for X := Low(cctx) to High(cctx) do
    begin
      ZSTD_freeCCtx(cctx[X]);
      ZSTD_freeDCtx(dctx[X]);
    end;
  end;
end;

function ZSTDParse(Command: PChar; Option: PInteger;
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
    if (CompareText(S, ZSTDCodecs[ZSTD_CODEC]) = 0) and ZSTDDLL.DLLLoaded then
    begin
      SetBits(Option^, ZSTD_CODEC, 0, 5);
      if Funcs^.GetParam(Command, I, 'l') <> '' then
        SetBits(Option^, StrToInt(Funcs^.GetParam(Command, I, 'l')), 5, 7);
      Result := True;
    end;
    Inc(I);
  end;
end;

procedure ZSTDScan1(Instance, Depth: Integer; Input: PByte;
  Size, SizeEx: NativeInt; Output: _PrecompOutput; Add: _PrecompAdd;
  Funcs: PPrecompFuncs);
var
  Buffer: PByte;
  Pos: NativeInt;
  X, Y, Z: Integer;
  SI: _StrInfo1;
  DI1, DI2: TDepthInfo;
  DS: TPrecompStr;
begin
  DI1 := Funcs^.GetDepthInfo(Instance);
  DS := Funcs^.GetCodec(DI1.Codec, 0, False);
  if DS <> '' then
  begin
    X := IndexTextW(@DS[0], ZSTDCodecs);
    if (X < 0) or (DI1.OldSize <> SizeEx) then
      exit;
    if not CodecAvailable[X] then
      exit;
    Y := ZSTD_findDecompressedSize(Input, SizeEx);
    if Y <= 0 then
      Y := Z_MAXSIZE;
    Buffer := Funcs^.Allocator(Instance, Y);
    case X of
      ZSTD_CODEC:
        Y := ZSTD_decompressDCtx(dctx[Instance], Buffer, Y, Input, X);
    end;
    if (Y > DI1.OldSize) then
    begin
      Output(Instance, Buffer, Y);
      SI.Position := 0;
      SI.OldSize := DI1.OldSize;
      SI.NewSize := Y;
      SI.Option := 0;
      SetBits(SI.Option, X, 0, 5);
      if System.Pos(SPrecompSep2, DI1.Codec) > 0 then
        SI.Status := TStreamStatus.Predicted
      else
        SI.Status := TStreamStatus.None;
      DS := Funcs^.GetDepthCodec(DI1.Codec);
      Move(DS[0], DI2.Codec, SizeOf(DI2.Codec));
      DI2.OldSize := SI.NewSize;
      DI2.NewSize := SI.NewSize;
      Funcs^.LogScan1(ZSTDCodecs[GetBits(SI.Option, 0, 5)], SI.Position,
        SI.OldSize, SI.NewSize);
      Add(Instance, @SI, DI1.Codec, @DI2);
    end;
    exit;
  end;
  if BoolArray(CodecEnabled, False) then
    exit;
  Pos := 0;
  while Pos < Size do
  begin
    if PCardinal(Input + Pos)^ = $FD2FB528 then
    begin
      X := ZSTD_findFrameCompressedSize(Input + Pos, SizeEx - Pos);
      if X > 0 then
      begin
        Z := ZSTD_findDecompressedSize(Input + Pos, X);
        if Z <= 0 then
          Z := Z_MAXSIZE;
        Buffer := Funcs^.Allocator(Instance, Z);
        Y := ZSTD_decompressDCtx(dctx[Instance], Buffer, Z, Input + Pos, X);
        // Y := ZSTD_decompress_usingDDict(dctx[Instance], Buffer, Z, Input + Pos, X, ddict);
        if (X < Y) then
        begin
          Output(Instance, Buffer, Y);
          SI.Position := Pos;
          SI.OldSize := X;
          SI.NewSize := Y;
          SI.Option := 0;
          SI.Status := TStreamStatus.None;
          Funcs^.LogScan1(ZSTDCodecs[GetBits(SI.Option, 0, 5)], SI.Position,
            SI.OldSize, SI.NewSize);
          Add(Instance, @SI, nil, nil);
          Inc(Pos, SI.OldSize);
          continue;
        end;
      end;
    end;
    Inc(Pos);
  end;
end;

function ZSTDScan2(Instance, Depth: Integer; Input: Pointer; Size: NativeInt;
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
    StreamInfo^.OldSize := ZSTD_findFrameCompressedSize(Input, Size);
  if StreamInfo^.OldSize <= 0 then
    exit;
  if StreamInfo^.NewSize <= 0 then
    StreamInfo^.NewSize := ZSTD_findDecompressedSize(Input, Size);
  if StreamInfo^.NewSize <= 0 then
    StreamInfo^.NewSize := Z_MAXSIZE;
  Buffer := Funcs^.Allocator(Instance, StreamInfo^.NewSize);
  case X of
    ZSTD_CODEC:
      Res := ZSTD_decompressDCtx(dctx[Instance], Buffer, StreamInfo^.NewSize,
        Input, StreamInfo^.OldSize);
  end;
  if Res > StreamInfo^.OldSize then
  begin
    Output(Instance, Buffer, Res);
    StreamInfo^.NewSize := Res;
    Funcs^.LogScan2(ZSTDCodecs[GetBits(StreamInfo^.Option, 0, 5)],
      StreamInfo^.OldSize, StreamInfo^.NewSize);
    Result := True;
  end;
end;

function ZSTDProcess(Instance, Depth: Integer; OldInput, NewInput: Pointer;
  StreamInfo: PStrInfo2; Output: _PrecompOutput; Funcs: PPrecompFuncs): Boolean;
var
  Buffer: PByte;
  Params: String;
  I: Integer;
  X: Integer;
  Res1: Integer;
  Res2: NativeUInt;
  Inp: ZSTD_inBuffer;
  Oup: ZSTD_outBuffer;
  Progress: NativeInt;
begin
  Result := False;
  X := GetBits(StreamInfo^.Option, 0, 5);
  if BoolArray(CodecAvailable, False) or (CodecAvailable[X] = False) then
    exit;
  Buffer := Funcs^.Allocator(Instance, StreamInfo^.NewSize);
  SOList[Instance][X].Index := 0;
  while SOList[Instance][X].Get(I) >= 0 do
  begin
    if StreamInfo^.Status >= TStreamStatus.Predicted then
    begin
      if GetBits(StreamInfo^.Option, 5, 7) <> I then
        continue;
      if (StreamInfo^.Status = TStreamStatus.Database) and
        (GetBits(StreamInfo^.Option, 31, 1) = 0) then
      begin
        Res1 := StreamInfo^.OldSize;
        Result := True;
      end;
    end;
    Params := '';
    case X of
      ZSTD_CODEC:
        begin
          Params := 'l' + I.ToString;
          { ZSTD_CCtx_reset(cctx[Instance], ZSTD_reset_session_and_parameters);
            ZSTD_CCtx_setParameter(cctx[Instance], ZSTD_c_strategy, 5);
            ZSTD_CCtx_setParameter(cctx[Instance], ZSTD_c_compressionLevel, I); }
          if not Result then
            Res1 := ZSTD_compressCCtx(cctx[Instance], Buffer,
              StreamInfo^.NewSize, NewInput, StreamInfo^.NewSize, I);
        end;
      { Res1 := ZSTD_compress_usingCDict(cctx[Instance], Buffer,
        StreamInfo^.NewSize, NewInput, StreamInfo^.NewSize, cdict); }
      { begin
        Params := 'l' + I.ToString;
        Progress := 0;
        Oup.dst := Buffer;
        Oup.Size := StreamInfo^.NewSize;
        Oup.Pos := 0;
        ZSTD_initCStream(cctx[Instance], I);
        ZSTD_CCtx_setParameter(cctx[Instance], ZSTD_c_strategy, 9);
        while Progress < StreamInfo^.NewSize do
        begin
        Inp.src := PByte(NewInput) + Progress;
        Inp.Size := Min(StreamInfo^.NewSize - Progress, 64 * 1024);
        Inp.Pos := 0;
        if ZSTD_compressStream(cctx[Instance], @Oup, @Inp) > 0 then
        begin
        ZSTD_flushStream(cctx[Instance], @Oup);
        Inc(Progress, Inp.Size)
        end
        else
        exit;
        end;
        ZSTD_endStream(cctx[Instance], @Oup);
        Res1 := Oup.Pos;
        end; }
    end;
    if not Result then
      Result := (Res1 = StreamInfo^.OldSize) and CompareMem(OldInput, Buffer,
        StreamInfo^.OldSize);
    Funcs^.LogProcess(ZSTDCodecs[GetBits(StreamInfo^.Option, 0, 5)],
      PChar(Params), StreamInfo^.OldSize, StreamInfo^.NewSize, Res1, Result);
    if Result or (StreamInfo^.Status >= TStreamStatus.Predicted) then
      break;
  end;
  if Res1 < 0 then
    exit;
  if (Result = False) and ((StreamInfo^.Status >= TStreamStatus.Predicted) or
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

function ZSTDRestore(Instance, Depth: Integer; Input, InputExt: Pointer;
  StreamInfo: _StrInfo3; Output: _PrecompOutput; Funcs: PPrecompFuncs): Boolean;
var
  Buffer: PByte;
  Params: String;
  X: Integer;
  Res1: Integer;
  Res2: NativeUInt;
begin
  Result := False;
  X := GetBits(StreamInfo.Option, 0, 5);
  if BoolArray(CodecAvailable, False) or (CodecAvailable[X] = False) then
    exit;
  Buffer := Funcs^.Allocator(Instance, StreamInfo.NewSize);
  Params := 'l' + GetBits(StreamInfo.Option, 5, 7).ToString;
  case X of
    ZSTD_CODEC:
      Res1 := ZSTD_compressCCtx(cctx[Instance], Buffer, StreamInfo.NewSize,
        Input, StreamInfo.NewSize, GetBits(StreamInfo.Option, 5, 7));
    { Res1 := ZSTD_compress_usingCDict(cctx[Instance], Buffer,
      StreamInfo.NewSize, Input, StreamInfo.NewSize, cdict); }
  end;
  Funcs^.LogRestore(ZSTDCodecs[GetBits(StreamInfo.Option, 0, 5)], PChar(Params),
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

{ DStream := TMemoryStream.Create;
  DStream.LoadFromFile(ExtractFilePath(Utils.GetModuleName) + 'frostbite3_dict.dat');
  cdict := ZSTD_createCDict(DStream.Memory, DStream.Size, 19);
  ddict := ZSTD_createDDict(DStream.Memory, DStream.Size); }

Codec.Names := [];
for I := Low(ZSTDCodecs) to High(ZSTDCodecs) do
begin
  Codec.Names := Codec.Names + [ZSTDCodecs[I]];
  StockMethods.Add(ZSTDCodecs[I]);
end;
Codec.Initialised := False;
Codec.Init := @ZSTDInit;
Codec.Free := @ZSTDFree;
Codec.Parse := @ZSTDParse;
Codec.Scan1 := @ZSTDScan1;
Codec.Scan2 := @ZSTDScan2;
Codec.Process := @ZSTDProcess;
Codec.Restore := @ZSTDRestore;
SetLength(CodecAvailable, Length(Codec.Names));
SetLength(CodecEnabled, Length(Codec.Names));

end.
