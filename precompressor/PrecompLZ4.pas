unit PrecompLZ4;

interface

uses
  LZ4DLL,
  lz4,
  Utils,
  PrecompUtils,
  WinAPI.Windows,
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
  L_ACCELERATION = 1;
  L_BLOCKDEPENDENCY = 0;
  L_BLOCKSIZE1 = 0;
  L_BLOCKSIZE2 = 0;

var
  SOList: array of array [0 .. CODEC_COUNT - 1] of TSOList;
  cctx1, cctx2: array of Pointer;
  CodecAvailable, CodecEnabled: TArray<Boolean>;
  LMaxSize: Integer = L_MAXSIZE;
  LAcceleration: Integer = L_ACCELERATION;
  LBlockDependency: Integer = L_BLOCKDEPENDENCY;
  LBlockSize1: Integer = L_BLOCKSIZE1;
  LBlockSize2: Integer = L_BLOCKSIZE2;

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
      if Funcs^.GetParam(Command, X, 's') <> '' then
        LMaxSize := ConvertToBytes(Funcs^.GetParam(Command, X, 's'));
      if Funcs^.GetParam(Command, X, 'a') <> '' then
        LAcceleration := StrToInt(Funcs^.GetParam(Command, X, 'a'));
      if Funcs^.GetParam(Command, X, 'b') <> '' then
        LBlockSize1 := StrToInt(Funcs^.GetParam(Command, X, 'b'));
    end
    else if (CompareText(S, LZ4Codecs[LZ4HC_CODEC]) = 0) and LZ4DLL.DLLLoaded
    then
    begin
      CodecEnabled[LZ4HC_CODEC] := True;
      if Funcs^.GetParam(Command, X, 'l') <> '' then
        for I := Low(SOList) to High(SOList) do
          SOList[I][LZ4HC_CODEC].Update
            ([StrToInt(Funcs^.GetParam(Command, X, 'l'))], True);
      if Funcs^.GetParam(Command, X, 's') <> '' then
        LMaxSize := ConvertToBytes(Funcs^.GetParam(Command, X, 's'));
      if Funcs^.GetParam(Command, X, 'b') <> '' then
        LBlockSize1 := StrToInt(Funcs^.GetParam(Command, X, 'b'));
    end
    else if (CompareText(S, LZ4Codecs[LZ4F_CODEC]) = 0) and LZ4DLL.DLLLoaded
    then
    begin
      CodecEnabled[LZ4F_CODEC] := True;
      if Funcs^.GetParam(Command, X, 'l') <> '' then
        for I := Low(SOList) to High(SOList) do
          SOList[I][LZ4F_CODEC].Update
            ([StrToInt(Funcs^.GetParam(Command, X, 'l'))], True);
      if Funcs^.GetParam(Command, X, 's') <> '' then
        LMaxSize := ConvertToBytes(Funcs^.GetParam(Command, X, 's'));
      if Funcs^.GetParam(Command, X, 'd') <> '' then
        LBlockDependency := StrToInt(Funcs^.GetParam(Command, X, 'd'));
      if Funcs^.GetParam(Command, X, 'b') <> '' then
        LBlockSize2 := StrToInt(Funcs^.GetParam(Command, X, 'b')) - 4;
    end;
    Inc(X);
  end;
  for X := Low(SOList) to High(SOList) do
    if SOList[X, LZ4_CODEC].Count = 0 then
      SOList[X, LZ4_CODEC].Update([1]);
  if CodecAvailable[LZ4_CODEC] then
  begin
    SetLength(cctx1, Count);
    for X := Low(cctx1) to High(cctx1) do
      cctx1[X] := nil;
  end;
  if CodecAvailable[LZ4HC_CODEC] then
  begin
    SetLength(cctx2, Count);
    for X := Low(cctx2) to High(cctx2) do
      cctx2[X] := nil;
  end;
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
  if CodecAvailable[LZ4_CODEC] then
    for X := Low(cctx1) to High(cctx1) do
      if Assigned(cctx1[X]) then
        LZ4_freeStream(cctx1[X]);
  if CodecAvailable[LZ4HC_CODEC] then
    for X := Low(cctx2) to High(cctx2) do
      if Assigned(cctx2[X]) then
        LZ4_freeStreamHC(cctx2[X]);
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
      SetBits(Option^, LZ4_CODEC, 0, 3);
      SetBits(Option^, LAcceleration, 7, 7);
      SetBits(Option^, LBlockSize1, 15, 13);
      if Funcs^.GetParam(Command, I, 'a') <> '' then
        SetBits(Option^, StrToInt(Funcs^.GetParam(Command, I, 'a')), 7, 7);
      Result := True;
    end
    else if (CompareText(S, LZ4Codecs[LZ4HC_CODEC]) = 0) and LZ4DLL.DLLLoaded
    then
    begin
      SetBits(Option^, LZ4HC_CODEC, 0, 3);
      SetBits(Option^, LBlockSize1, 15, 13);
      if Funcs^.GetParam(Command, I, 'l') <> '' then
        SetBits(Option^, StrToInt(Funcs^.GetParam(Command, I, 'l')), 3, 4);
      if Funcs^.GetParam(Command, I, 'b') <> '' then
        SetBits(Option^, StrToInt(Funcs^.GetParam(Command, I, 'b')), 15, 13);
      Result := True;
    end
    else if (CompareText(S, LZ4Codecs[LZ4F_CODEC]) = 0) and LZ4DLL.DLLLoaded
    then
    begin
      SetBits(Option^, LZ4F_CODEC, 0, 3);
      SetBits(Option^, LBlockDependency, 14, 1);
      SetBits(Option^, LBlockSize2, 15, 13);
      if Funcs^.GetParam(Command, I, 'l') <> '' then
        SetBits(Option^, StrToInt(Funcs^.GetParam(Command, I, 'l')), 3, 4);
      if Funcs^.GetParam(Command, I, 'b') <> '' then
        SetBits(Option^, StrToInt(Funcs^.GetParam(Command, I, 'b')) -
          4, 15, 13);
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
  Pos: NativeInt;
  X, Y, Z: Integer;
  SI: _StrInfo1;
begin
  if BoolArray(CodecEnabled, False) then
    exit;
  Buffer := Funcs^.Allocator(Instance, LMaxSize);
  Pos := 0;
  while Pos < Size do
  begin
    if CodecEnabled[LZ4_CODEC] or (CodecEnabled[LZ4HC_CODEC]) then
    begin
      if (Input + Pos)^ in [$F0 .. $F4] then
      begin
        try
          X := LZ4_decompress_generic(Input + Pos, Buffer, SizeEx - Pos,
            LMaxSize, Integer(endOnOutputSize));
          if X > 256 then
            Y := LZ4_decompress_safe(Input + Pos, Buffer, X, LMaxSize)
          else
            Y := 0;
        except
        end;
        if Y > 256 then
        begin
          if (X < Y) and (X > 256) then
          begin
            Output(Instance, Buffer, Y);
            SI.Position := Pos;
            SI.OldSize := X;
            SI.NewSize := Y;
            SI.Option := 0;
            if CodecEnabled[LZ4_CODEC] then
              SetBits(SI.Option, LZ4_CODEC, 0, 3)
            else
              SetBits(SI.Option, LZ4HC_CODEC, 0, 3);
            SetBits(SI.Option, LAcceleration, 7, 7);
            SetBits(SI.Option, LBlockSize1, 15, 13);
            SI.Status := TStreamStatus.None;
            Funcs^.LogScan1(LZ4Codecs[GetBits(SI.Option, 0, 3)], SI.Position,
              SI.OldSize, SI.NewSize);
            Add(Instance, @SI, nil, nil);
            Inc(Pos, 256);
            continue;
          end;
        end;
      end;
    end;
    if CodecEnabled[LZ4F_CODEC] then
      if PCardinal(Input + Pos)^ = $184D2204 then
      begin
        Y := LZ4F_decompress_safe(Input + Pos, Buffer, SizeEx - Pos,
          LMaxSize, @X, @Z);
        if (X < Y) then
        begin
          Output(Instance, Buffer, Y);
          SI.Position := Pos;
          SI.OldSize := X;
          SI.NewSize := Y;
          SI.Option := 0;
          SetBits(SI.Option, LZ4F_CODEC, 0, 3);
          SetBits(SI.Option, LBlockDependency, 14, 1);
          SetBits(SI.Option, Z - 4, 15, 13);
          SI.Status := TStreamStatus.None;
          Funcs^.LogScan1(LZ4Codecs[GetBits(SI.Option, 0, 3)], SI.Position,
            SI.OldSize, SI.NewSize);
          Add(Instance, @SI, nil, nil);
          Inc(Pos, SI.OldSize);
          continue;
        end;
      end;
    Inc(Pos);
  end;
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
    exit;
  StreamInfo^.NewSize := Max(StreamInfo^.NewSize, LMaxSize);
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
    Funcs^.LogScan2(LZ4Codecs[GetBits(StreamInfo^.Option, 0, 3)],
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
  Res1: NativeInt;
  Res2: NativeUInt;
  Progress: NativeInt;
  A, B: Integer;
  LZ4FT: LZ4F_preferences_t;
begin
  Result := False;
  X := GetBits(StreamInfo^.Option, 0, 3);
  if BoolArray(CodecAvailable, False) or (CodecAvailable[X] = False) then
    exit;
  Y := LZ4F_compressFrameBound(StreamInfo^.NewSize, nil);
  Buffer := Funcs^.Allocator(Instance, Y);
  SOList[Instance][X].Index := 0;
  while SOList[Instance][X].Get(I) >= 0 do
  begin
    if StreamInfo^.Status >= TStreamStatus.Predicted then
    begin
      if GetBits(StreamInfo^.Option, 3, 4) <> I then
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
      LZ4_CODEC:
        begin
          Params := 'a' + GetBits(StreamInfo^.Option, 7, 7).ToString + ':' + 'b'
            + GetBits(StreamInfo^.Option, 15, 13).ToString;
          if not Result then
          begin
            if GetBits(StreamInfo^.Option, 15, 13) = 0 then
              Res1 := LZ4_compress_fast(NewInput, Buffer, StreamInfo^.NewSize,
                Y, GetBits(StreamInfo^.Option, 7, 7))
            else
            begin
              if cctx1[Instance] = nil then
                cctx1[Instance] := LZ4_createStream;
              LZ4_resetStream(cctx1[Instance]);
              B := 0;
              Res1 := 0;
              while B < StreamInfo^.NewSize do
              begin
                A := LZ4_compress_fast_continue(cctx1[Instance],
                  PByte(NewInput) + B, Buffer + Res1,
                  Min(GetBits(StreamInfo^.Option, 15, 13) * 1024,
                  StreamInfo^.NewSize - B), StreamInfo^.NewSize - Res1,
                  GetBits(StreamInfo^.Option, 7, 7));
                if A > 0 then
                begin
                  Inc(Res1, A);
                  Inc(B, GetBits(StreamInfo^.Option, 15, 13) * 1024);
                end
                else
                  break;
              end;
            end;
          end;
        end;
      LZ4HC_CODEC:
        begin
          Params := 'l' + I.ToString + ':' + 'b' + GetBits(StreamInfo^.Option,
            15, 13).ToString;
          if not Result then
          begin
            if GetBits(StreamInfo^.Option, 15, 13) = 0 then
              Res1 := LZ4_compress_HC(NewInput, Buffer,
                StreamInfo^.NewSize, Y, I)
            else
            begin
              if cctx2[Instance] = nil then
                cctx2[Instance] := LZ4_createStreamHC;
              LZ4_resetStreamHC(cctx2[Instance], I);
              B := 0;
              Res1 := 0;
              while B < StreamInfo^.NewSize do
              begin
                A := LZ4_compress_HC_continue(cctx2[Instance],
                  PByte(NewInput) + B, Buffer + Res1,
                  Min(GetBits(StreamInfo^.Option, 15, 13) * 1024,
                  StreamInfo^.NewSize - B), StreamInfo^.NewSize - Res1);
                if A > 0 then
                begin
                  Inc(Res1, A);
                  Inc(B, GetBits(StreamInfo^.Option, 15, 13) * 1024);
                end
                else
                  break;
              end;
            end;
          end;
        end;
      LZ4F_CODEC:
        begin
          FillChar(LZ4FT, SizeOf(LZ4F_preferences_t), 0);
          LZ4FT.compressionLevel := I;
          LZ4FT.frameInfo.blockSizeID :=
            LZ4F_blockSizeID_t(GetBits(StreamInfo^.Option, 15, 13) + 4);
          LZ4FT.frameInfo.blockMode :=
            LZ4F_blockMode_t(GetBits(StreamInfo^.Option, 14, 1));
          Params := 'l' + I.ToString + ':' + 'b' +
            (GetBits(StreamInfo^.Option, 15, 13) + 4).ToString + ':' + 'd' +
            GetBits(StreamInfo^.Option, 14, 1).ToString;
          if not Result then
            Res1 := LZ4F_compressFrame(Buffer, Y, NewInput,
              StreamInfo^.NewSize, @LZ4FT);
        end;
    end;
    if not Result then
      Result := (Res1 = StreamInfo^.OldSize) and CompareMem(OldInput, Buffer,
        StreamInfo^.OldSize);
    Funcs^.LogProcess(LZ4Codecs[GetBits(StreamInfo^.Option, 0, 3)],
      PChar(Params), StreamInfo^.OldSize, StreamInfo^.NewSize, Res1, Result);
    if Result or (StreamInfo^.Status >= TStreamStatus.Predicted) then
      break;
  end;
  if (Result = False) and ((StreamInfo^.Status >= TStreamStatus.Predicted) or
    (SOList[Instance][X].Count = 1)) and (DIFF_TOLERANCE > 0) then
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
    SetBits(StreamInfo^.Option, I, 3, 4);
    SOList[Instance][X].Add(I);
  end;
end;

function LZ4Restore(Instance, Depth: Integer; Input, InputExt: Pointer;
  StreamInfo: _StrInfo3; Output: _PrecompOutput; Funcs: PPrecompFuncs): Boolean;
var
  Buffer: PByte;
  Params: String;
  X: Integer;
  Res1: NativeInt;
  Res2: NativeUInt;
  A, B: Integer;
  LZ4FT: LZ4F_preferences_t;
begin
  Result := False;
  X := GetBits(StreamInfo.Option, 0, 3);
  if BoolArray(CodecAvailable, False) or (CodecAvailable[X] = False) then
    exit;
  Params := '';
  Buffer := Funcs^.Allocator(Instance,
    LZ4F_compressFrameBound(StreamInfo.NewSize, nil));
  case X of
    LZ4_CODEC:
      begin
        Params := 'a' + GetBits(StreamInfo.Option, 7, 7).ToString + ':' + 'b' +
          GetBits(StreamInfo.Option, 15, 13).ToString;
        if GetBits(StreamInfo.Option, 15, 13) = 0 then
          Res1 := LZ4_compress_fast(Input, Buffer, StreamInfo.NewSize,
            LZ4F_compressFrameBound(StreamInfo.NewSize, nil),
            GetBits(StreamInfo.Option, 7, 7))
        else
        begin
          if cctx1[Instance] = nil then
            cctx1[Instance] := LZ4_createStream;
          LZ4_resetStream(cctx1[Instance]);
          B := 0;
          Res1 := 0;
          while B < StreamInfo.NewSize do
          begin
            A := LZ4_compress_fast_continue(cctx1[Instance], PByte(Input) + B,
              Buffer + Res1, Min(GetBits(StreamInfo.Option, 15, 13) * 1024,
              StreamInfo.NewSize - B), StreamInfo.NewSize - Res1,
              GetBits(StreamInfo.Option, 7, 7));
            if A > 0 then
            begin
              Inc(B, GetBits(StreamInfo.Option, 15, 13) * 1024);
              Inc(Res1, A);
            end
            else
              break;
          end;
        end;
      end;
    LZ4HC_CODEC:
      begin
        Params := 'l' + GetBits(StreamInfo.Option, 3, 4).ToString + ':' + 'b' +
          GetBits(StreamInfo.Option, 15, 13).ToString;
        if GetBits(StreamInfo.Option, 15, 13) = 0 then
          Res1 := LZ4_compress_HC(Input, Buffer, StreamInfo.NewSize,
            LZ4F_compressFrameBound(StreamInfo.NewSize, nil),
            GetBits(StreamInfo.Option, 3, 4))
        else
        begin
          if cctx2[Instance] = nil then
            cctx2[Instance] := LZ4_createStreamHC;
          LZ4_resetStreamHC(cctx2[Instance], GetBits(StreamInfo.Option, 3, 4));
          B := 0;
          Res1 := 0;
          while B < StreamInfo.NewSize do
          begin
            A := LZ4_compress_HC_continue(cctx2[Instance], PByte(Input) + B,
              Buffer + Res1, Min(GetBits(StreamInfo.Option, 15, 13) * 1024,
              StreamInfo.NewSize - B), StreamInfo.NewSize - Res1);
            if A > 0 then
            begin
              Inc(B, GetBits(StreamInfo.Option, 15, 13) * 1024);
              Inc(Res1, A);
            end
            else
              break;
          end;
        end;
      end;
    LZ4F_CODEC:
      begin
        FillChar(LZ4FT, SizeOf(LZ4F_preferences_t), 0);
        LZ4FT.compressionLevel := GetBits(StreamInfo.Option, 3, 4);
        LZ4FT.frameInfo.blockSizeID :=
          LZ4F_blockSizeID_t(GetBits(StreamInfo.Option, 15, 13) + 4);
        LZ4FT.frameInfo.blockMode :=
          LZ4F_blockMode_t(GetBits(StreamInfo.Option, 14, 1));
        Params := 'l' + GetBits(StreamInfo.Option, 3, 4).ToString + ':' + 'b' +
          (GetBits(StreamInfo.Option, 15, 13) + 4).ToString + ':' + 'd' +
          GetBits(StreamInfo.Option, 14, 1).ToString;
        Res1 := LZ4F_compressFrame(Buffer,
          LZ4F_compressFrameBound(StreamInfo.NewSize, nil), Input,
          StreamInfo.NewSize, @LZ4FT);
      end;
  end;
  Funcs^.LogRestore(LZ4Codecs[GetBits(StreamInfo.Option, 0, 3)], PChar(Params),
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
