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
  Z_FASTMODE = 0;
  Z_WINDOWLOG = 0;
  Z_BLOCKSIZE = 0;

var
  SOList: array of array [0 .. CODEC_COUNT - 1] of TSOList;
  cctx, dctx: array of Pointer;
  // cdict, ddict: Pointer;
  CodecAvailable, CodecEnabled: TArray<Boolean>;
  ZMaxSize: Integer = Z_MAXSIZE;
  ZFastMode: Integer = Z_FASTMODE;
  ZWindowLog: Integer = Z_WINDOWLOG;
  ZBlockSize: Integer = Z_BLOCKSIZE;

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
      if Funcs^.GetParam(Command, X, 's') <> '' then
        ZMaxSize := ConvertToBytes(Funcs^.GetParam(Command, X, 's'));
      if Funcs^.GetParam(Command, X, 'f') <> '' then
        ZFastMode := ConvertToBytes(Funcs^.GetParam(Command, X, 'f'));
      if Funcs^.GetParam(Command, X, 'w') <> '' then
        ZWindowLog := ConvertToBytes(Funcs^.GetParam(Command, X, 'w'));
      if Funcs^.GetParam(Command, X, 'b') <> '' then
        ZBlockSize := StrToInt(Funcs^.GetParam(Command, X, 'b'));
    end;
    Inc(X);
  end;
  if CodecAvailable[ZSTD_CODEC] then
  begin
    SetLength(cctx, Count);
    SetLength(dctx, Count);
    for X := Low(cctx) to High(cctx) do
    begin
      cctx[X] := nil;
      dctx[X] := nil;
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
      if Assigned(cctx[X]) then
        ZSTD_freeCCtx(cctx[X]);
      if Assigned(dctx[X]) then
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
  SetBits(Option^, ZFastMode, 8, 1);
  SetBits(Option^, ZWindowLog, 9, 5);
  SetBits(Option^, ZBlockSize, 14, 13);
  I := 0;
  while Funcs^.GetCodec(Command, I, False) <> '' do
  begin
    S := Funcs^.GetCodec(Command, I, False);
    if (CompareText(S, ZSTDCodecs[ZSTD_CODEC]) = 0) and ZSTDDLL.DLLLoaded then
    begin
      SetBits(Option^, ZSTD_CODEC, 0, 3);
      if Funcs^.GetParam(Command, I, 'l') <> '' then
        SetBits(Option^, StrToInt(Funcs^.GetParam(Command, I, 'l')), 3, 5);
      if String(Funcs^.GetParam(Command, I, 'f')) = '1' then
        SetBits(Option^, 1, 8, 1);
      if Funcs^.GetParam(Command, I, 'w') <> '' then
        SetBits(Option^, StrToInt(Funcs^.GetParam(Command, I, 'w')), 9, 5);
      if Funcs^.GetParam(Command, I, 'b') <> '' then
        SetBits(Option^, StrToInt(Funcs^.GetParam(Command, I, 'b')), 14, 13);
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
begin
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
          Z := ZMaxSize;
        Buffer := Funcs^.Allocator(Instance, Z);
        if not Assigned(dctx[Instance]) then
          dctx[Instance] := ZSTD_createDCtx;
        Y := ZSTD_decompressDCtx(dctx[Instance], Buffer, Z, Input + Pos, X);
        // Y := ZSTD_decompress_usingDDict(dctx[Instance], Buffer, Z, Input + Pos, X, ddict);
        if (X < Y) then
        begin
          Output(Instance, Buffer, Y);
          SI.Position := Pos;
          SI.OldSize := X;
          SI.NewSize := Y;
          SI.Option := 0;
          SetBits(SI.Option, ZFastMode, 8, 1);
          SetBits(SI.Option, ZWindowLog, 9, 5);
          SetBits(SI.Option, ZBlockSize, 14, 13);
          SI.Status := TStreamStatus.None;
          Funcs^.LogScan1(ZSTDCodecs[GetBits(SI.Option, 0, 3)], SI.Position,
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
  X := GetBits(StreamInfo^.Option, 0, 3);
  if (StreamInfo^.OldSize > 0) and REPROCESSED then
    if (Int64Rec(PInt64(PByte(Input) + StreamInfo^.OldSize - Int64.Size)^)
      .Lo = StreamInfo^.OldSize) and
      (Int64Rec(PInt64(PByte(Input) + StreamInfo^.OldSize - Int64.Size)^).Lo <
      Int64Rec(PInt64(PByte(Input) + StreamInfo^.OldSize - Int64.Size)^).Hi)
    then
      StreamInfo^.OldSize :=
        Int64Rec(PInt64(PByte(Input) + StreamInfo^.OldSize - Int64.Size)^).Hi;
  if StreamInfo^.OldSize <= 0 then
    StreamInfo^.OldSize := ZSTD_findFrameCompressedSize(Input, Size);
  if StreamInfo^.OldSize <= 0 then
    exit;
  if StreamInfo^.NewSize <= 0 then
    StreamInfo^.NewSize := ZSTD_findDecompressedSize(Input, Size);
  if StreamInfo^.NewSize <= 0 then
    StreamInfo^.NewSize := ZMaxSize;
  Buffer := Funcs^.Allocator(Instance, StreamInfo^.NewSize);
  case X of
    ZSTD_CODEC:
      begin
        if not Assigned(dctx[Instance]) then
          dctx[Instance] := ZSTD_createDCtx;
        Res := ZSTD_decompressDCtx(dctx[Instance], Buffer, StreamInfo^.NewSize,
          Input, StreamInfo^.OldSize);
      end;
  end;
  if Res > StreamInfo^.OldSize then
  begin
    Output(Instance, Buffer, Res);
    StreamInfo^.NewSize := Res;
    Funcs^.LogScan2(ZSTDCodecs[GetBits(StreamInfo^.Option, 0, 3)],
      StreamInfo^.OldSize, StreamInfo^.NewSize);
    Result := True;
  end;
end;

function ZSTDProcess(Instance, Depth: Integer; OldInput, NewInput: Pointer;
  StreamInfo: PStrInfo2; Output: _PrecompOutput; Funcs: PPrecompFuncs): Boolean;
var
  Buffer: PByte;
  Params: String;
  A, B: Integer;
  I: Integer;
  X: Integer;
  Res1: NativeInt;
  Res2: NativeUInt;
  Inp: ZSTD_inBuffer;
  Oup: ZSTD_outBuffer;
  Progress: NativeInt;
begin
  Result := False;
  X := GetBits(StreamInfo^.Option, 0, 3);
  if BoolArray(CodecAvailable, False) or (CodecAvailable[X] = False) then
    exit;
  Buffer := Funcs^.Allocator(Instance, StreamInfo^.NewSize);
  SOList[Instance][X].Index := 0;
  while SOList[Instance][X].Get(I) >= 0 do
  begin
    if StreamInfo^.Status >= TStreamStatus.Predicted then
    begin
      if GetBits(StreamInfo^.Option, 3, 5) <> I then
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
          Params := 'l' + I.ToString + ':' + 'f' + GetBits(StreamInfo^.Option,
            8, 1).ToString + ':' + 'w' + GetBits(StreamInfo^.Option, 9, 5)
            .ToString + ':' + 'b' + GetBits(StreamInfo^.Option, 14, 13)
            .ToString;
          if not Assigned(cctx[Instance]) then
            cctx[Instance] := ZSTD_createCCtx;
          if not Result then
          begin
            if GetBits(StreamInfo^.Option, 14, 13) = 0 then
            begin
              ZSTD_CCtx_setParameter(cctx[Instance], ZSTD_c_compressionLevel,
                IfThen(GetBits(StreamInfo^.Option, 8, 1) = 0, I, -I));
              ZSTD_CCtx_setParameter(cctx[Instance], ZSTD_c_windowLog,
                GetBits(StreamInfo^.Option, 9, 5));
              if Assigned(ZSTD_compress2) then
                Res1 := ZSTD_compress2(cctx[Instance], Buffer,
                  StreamInfo^.NewSize, NewInput, StreamInfo^.NewSize)
              else if Assigned(ZSTD_compress_generic) then
              begin
                Oup.dst := Buffer;
                Oup.Size := StreamInfo^.NewSize;
                Oup.Pos := 0;
                Inp.src := PByte(NewInput);
                Inp.Size := StreamInfo^.NewSize;
                Inp.Pos := 0;
                ZSTD_compress_generic(cctx[Instance], @Oup, @Inp, ZSTD_e_end);
                Res1 := Oup.Pos;
              end
              else
                Res1 := ZSTD_compressCCtx(cctx[Instance], Buffer,
                  StreamInfo^.NewSize, NewInput, StreamInfo^.NewSize,
                  IfThen(GetBits(StreamInfo^.Option, 8, 1) = 0, I, -I))
            end
            else
            begin
              Progress := 0;
              Oup.dst := Buffer;
              Oup.Size := StreamInfo^.NewSize;
              Oup.Pos := 0;
              ZSTD_initCStream(cctx[Instance],
                IfThen(GetBits(StreamInfo^.Option, 8, 1) = 0, I, -I));
              while Progress < StreamInfo^.NewSize do
              begin
                Inp.src := PByte(NewInput) + Progress;
                Inp.Size := Min(StreamInfo^.NewSize - Progress,
                  GetBits(StreamInfo^.Option, 14, 13) * 1024);
                Inp.Pos := 0;
                if ZSTD_compressStream(cctx[Instance], @Oup, @Inp) > 0 then
                begin
                  ZSTD_flushStream(cctx[Instance], @Oup);
                  Inc(Progress, Inp.Size)
                end
                else
                  break;
              end;
              ZSTD_endStream(cctx[Instance], @Oup);
              Res1 := Oup.Pos;
            end;
            { Res1 := ZSTD_compress_usingCDict(cctx[Instance], Buffer,
              StreamInfo^.NewSize, NewInput, StreamInfo^.NewSize, cdict); }
          end;
        end;
    end;
    if not Result then
      Result := (Res1 = StreamInfo^.OldSize) and CompareMem(OldInput, Buffer,
        StreamInfo^.OldSize);
    Funcs^.LogProcess(ZSTDCodecs[GetBits(StreamInfo^.Option, 0, 3)],
      PChar(Params), StreamInfo^.OldSize, StreamInfo^.NewSize, Res1, Result);
    if Result or (StreamInfo^.Status >= TStreamStatus.Predicted) then
      break;
  end;
  if Result and OPTIMISE_DEC and (StreamInfo^.Status <> TStreamStatus.Database)
    and (GetBits(StreamInfo^.Option, 8, 1) = 0) then
  begin
    A := Pred(I);
    for B := A downto 1 do
    begin
      if not Assigned(cctx[Instance]) then
        cctx[Instance] := ZSTD_createCCtx;
      Res1 := ZSTD_compressCCtx(cctx[Instance], Buffer, StreamInfo^.NewSize,
        NewInput, StreamInfo^.NewSize, B);
      if (Res1 = StreamInfo^.OldSize) and CompareMem(OldInput, Buffer,
        StreamInfo^.OldSize) then
        I := B
      else
        break;
    end;
  end
  else if (Result = False) and ((StreamInfo^.Status >= TStreamStatus.Predicted)
    or (SOList[Instance][X].Count = 1)) and (DIFF_TOLERANCE > 0) then
  begin
    Res2 := PrecompEncodePatchEx(Instance, OldInput, StreamInfo^.OldSize,
      Buffer, Res1, Output);
    Funcs^.LogPatch1(StreamInfo^.OldSize, Res1, Res2,
      Funcs^.AcceptPatch(StreamInfo^.OldSize, Res1, Res2));
    if Funcs^.AcceptPatch(StreamInfo^.OldSize, Res1, Res2) then
    begin
      SetBits(StreamInfo^.Option, 1, 31, 1);
      SOList[Instance][X].Add(I);
      Result := True;
    end;
  end;
  if Result then
  begin
    SetBits(StreamInfo^.Option, I, 3, 5);
    SOList[Instance][X].Add(I);
  end;
end;

function ZSTDRestore(Instance, Depth: Integer; Input, InputExt: Pointer;
  StreamInfo: _StrInfo3; Output: _PrecompOutput; Funcs: PPrecompFuncs): Boolean;
var
  Buffer: PByte;
  Params: String;
  X: Integer;
  Res1: NativeInt;
  Res2: NativeUInt;
  Inp: ZSTD_inBuffer;
  Oup: ZSTD_outBuffer;
  Progress: NativeInt;
begin
  Result := False;
  X := GetBits(StreamInfo.Option, 0, 3);
  if BoolArray(CodecAvailable, False) or (CodecAvailable[X] = False) then
    exit;
  Buffer := Funcs^.Allocator(Instance, StreamInfo.NewSize);
  Params := 'l' + GetBits(StreamInfo.Option, 3, 5).ToString + ':' + 'f' +
    GetBits(StreamInfo.Option, 8, 1).ToString + ':' + 'b' +
    GetBits(StreamInfo.Option, 14, 13).ToString;
  case X of
    ZSTD_CODEC:
      begin
        if not Assigned(cctx[Instance]) then
          cctx[Instance] := ZSTD_createCCtx;
        if GetBits(StreamInfo.Option, 14, 13) = 0 then
        begin
          ZSTD_CCtx_setParameter(cctx[Instance], ZSTD_c_compressionLevel,
            IfThen(GetBits(StreamInfo.Option, 8, 1) = 0,
            GetBits(StreamInfo.Option, 3, 5),
            -GetBits(StreamInfo.Option, 3, 5)));
          ZSTD_CCtx_setParameter(cctx[Instance], ZSTD_c_windowLog,
            GetBits(StreamInfo.Option, 9, 5));
          if Assigned(ZSTD_compress2) then
            Res1 := ZSTD_compress2(cctx[Instance], Buffer, StreamInfo.NewSize,
              Input, StreamInfo.NewSize)
          else if Assigned(ZSTD_compress_generic) then
          begin
            Oup.dst := Buffer;
            Oup.Size := StreamInfo.NewSize;
            Oup.Pos := 0;
            Inp.src := PByte(Input);
            Inp.Size := StreamInfo.NewSize;
            Inp.Pos := 0;
            ZSTD_compress_generic(cctx[Instance], @Oup, @Inp, ZSTD_e_end);
            Res1 := Oup.Pos;
          end
          else
            Res1 := ZSTD_compressCCtx(cctx[Instance], Buffer,
              StreamInfo.NewSize, Input, StreamInfo.NewSize,
              IfThen(GetBits(StreamInfo.Option, 8, 1) = 0,
              GetBits(StreamInfo.Option, 3, 5),
              -GetBits(StreamInfo.Option, 3, 5)));
        end
        else
        begin
          Progress := 0;
          Oup.dst := Buffer;
          Oup.Size := StreamInfo.NewSize;
          Oup.Pos := 0;
          ZSTD_initCStream(cctx[Instance],
            IfThen(GetBits(StreamInfo.Option, 8, 1) = 0,
            GetBits(StreamInfo.Option, 3, 5),
            -GetBits(StreamInfo.Option, 3, 5)));
          while Progress < StreamInfo.NewSize do
          begin
            Inp.src := PByte(Input) + Progress;
            Inp.Size := Min(StreamInfo.NewSize - Progress,
              GetBits(StreamInfo.Option, 14, 13) * 1024);
            Inp.Pos := 0;
            if ZSTD_compressStream(cctx[Instance], @Oup, @Inp) > 0 then
            begin
              ZSTD_flushStream(cctx[Instance], @Oup);
              Inc(Progress, Inp.Size)
            end
            else
              break;
          end;
          ZSTD_endStream(cctx[Instance], @Oup);
          Res1 := Oup.Pos;
        end;
        { Res1 := ZSTD_compress_usingCDict(cctx[Instance], Buffer,
          StreamInfo.NewSize, Input, StreamInfo.NewSize, cdict); }
      end;
  end;
  Funcs^.LogRestore(ZSTDCodecs[GetBits(StreamInfo.Option, 0, 3)], PChar(Params),
    StreamInfo.OldSize, StreamInfo.NewSize, Res1, True);
  if GetBits(StreamInfo.Option, 31, 1) = 1 then
  begin
    Res2 := PrecompDecodePatchEx(Instance, InputExt, StreamInfo.ExtSize, Buffer,
      Res1, Output);
    Funcs^.LogPatch2(StreamInfo.OldSize, Res1, StreamInfo.ExtSize, Res2 > 0);
    if Res2 = StreamInfo.OldSize then
      Result := True;
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
