unit PrecompLZO;

interface

uses
  LZODLL,
  Utils,
  PrecompUtils,
  System.SysUtils, System.Classes, System.Math;

var
  Codec: TPrecompressor;

implementation

const
  LZOCodecs: array of PChar = ['lzo1x', 'lzo2a', 'lzo1c'];
  CODEC_COUNT = 3;
  LZO1X_CODEC = 0;
  LZO2A_CODEC = 1;
  LZO1C_CODEC = 2;

const
  L_MAXSIZE = 16 * 1024 * 1024;
  L_WORKMEM = 524288;
  LZO1X_999 = 999;
  LZO2A_999 = 999;
  LZO1C_999 = 999;

var
  SOList: array of array [0 .. CODEC_COUNT - 1] of TSOList;
  WrkMem: array of array [0 .. L_WORKMEM - 1] of Byte;
  LZO1XVariant: Integer = LZO1X_999;
  LZO2AVariant: Integer = LZO2A_999;
  LZO1CVariant: Integer = LZO1C_999;
  CodecAvailable, CodecEnabled: TArray<Boolean>;
  LMaxSize: Integer = L_MAXSIZE;

type
  PLZOSI = ^TLZOSI;

  TLZOSI = record
    CSize, DSize: Integer;
  end;

var
  LZOSB: array of Byte = [$11, $00, $00];

function GetLZO1XSI(InBuff: Pointer; InSize: Integer; OutBuff: Pointer;
  OutSize: Integer; StreamInfo: PLZOSI): Boolean;
const
  MinSize = 256;
  RetryCount = -1;
var
  I, Res: Integer;
  OSize: NativeUInt;
  Pos: NativeInt;
begin
  Result := False;
  if PWord(InBuff)^ = 0 then
    exit;
  StreamInfo^.CSize := Min(InSize, MinSize);
  Res := -1;
  I := 0;
  while Res <> 0 do
  begin
    OSize := OutSize;
    Res := lzo1x_decompress_safe(InBuff, Min(InSize, StreamInfo^.CSize),
      OutBuff, @OSize);
    case Res of
      0:
        begin
          StreamInfo^.DSize := OSize;
          if (StreamInfo^.CSize > MinSize) then
            Result := True;
        end;
      -4, -7:
        begin
          if I = RetryCount then
            break;
          if BinarySearch(InBuff, StreamInfo^.CSize, InSize, @LZOSB[0],
            Length(LZOSB), Pos) then
          begin
            Inc(I);
            StreamInfo^.CSize := Pos + Length(LZOSB)
          end
          else
            break;
        end;
      { -5:
        begin
        // increase output buffer size and try again
        break;
        end; }
      { -8:
        begin
        // reduce input buffer size and try again
        end; }
    else
      break;
    end;
  end;;
end;

function LZOInit(Command: PChar; Count: Integer; Funcs: PPrecompFuncs): Boolean;
var
  I: Integer;
  Options: TArray<Integer>;
  S: String;
  X, Y: Integer;
begin
  Result := True;
  SetLength(SOList, Count);
  SetLength(WrkMem, Count);
  for X := Low(SOList) to High(SOList) do
    for Y := Low(SOList[X]) to High(SOList[X]) do
      SOList[X, Y] := TSOList.Create([], TSOMethod.MTF);
  for X := Low(CodecAvailable) to High(CodecAvailable) do
  begin
    CodecAvailable[X] := False;
    CodecEnabled[X] := False;
  end;
  for X := Low(CodecAvailable) to High(CodecAvailable) do
    CodecAvailable[X] := LZODLL.DLLLoaded;
  X := 0;
  while Funcs^.GetCodec(Command, X, False) <> '' do
  begin
    S := Funcs^.GetCodec(Command, X, False);
    if (CompareText(S, LZOCodecs[LZO1X_CODEC]) = 0) and LZODLL.DLLLoaded then
    begin
      CodecEnabled[LZO1X_CODEC] := True;
      if Funcs^.GetParam(Command, X, 'v') = '999' then
        LZO1XVariant := 999;
      if Funcs^.GetParam(Command, X, 'l') <> '' then
        for I := Low(SOList) to High(SOList) do
          SOList[I][LZO1X_CODEC].Update
            ([StrToInt(Funcs^.GetParam(Command, X, 'l'))], True);
      if Funcs^.GetParam(Command, X, 's') <> '' then
        LMaxSize := ConvertToBytes(Funcs^.GetParam(Command, X, 's'));
    end
    else if (CompareText(S, LZOCodecs[LZO2A_CODEC]) = 0) and LZODLL.DLLLoaded
    then
    begin
      CodecEnabled[LZO2A_CODEC] := True;
      if Funcs^.GetParam(Command, X, 'v') = '999' then
        LZO2AVariant := 999;
      if Funcs^.GetParam(Command, X, 's') <> '' then
        LMaxSize := ConvertToBytes(Funcs^.GetParam(Command, X, 's'));
    end
    else if (CompareText(S, LZOCodecs[LZO1C_CODEC]) = 0) and LZODLL.DLLLoaded
    then
    begin
      CodecEnabled[LZO1C_CODEC] := True;
      if Funcs^.GetParam(Command, X, 'v') = '999' then
        LZO1CVariant := 999;
      if Funcs^.GetParam(Command, X, 's') <> '' then
        LMaxSize := ConvertToBytes(Funcs^.GetParam(Command, X, 's'));
    end;
    Inc(X);
  end;
  SetLength(Options, 0);
  for I := 1 to 9 do
    Insert(I, Options, Length(Options));
  for X := Low(SOList) to High(SOList) do
    if SOList[X, LZO1X_CODEC].Count = 0 then
      SOList[X, LZO1X_CODEC].Update(Options);
  for X := Low(SOList) to High(SOList) do
    if SOList[X, LZO2A_CODEC].Count = 0 then
      SOList[X, LZO2A_CODEC].Update([1]);
  for X := Low(SOList) to High(SOList) do
    if SOList[X, LZO1C_CODEC].Count = 0 then
      SOList[X, LZO1C_CODEC].Update([1]);
end;

procedure LZOFree(Funcs: PPrecompFuncs);
var
  X, Y: Integer;
begin
  for X := Low(SOList) to High(SOList) do
    for Y := Low(SOList[X]) to High(SOList[X]) do
      SOList[X, Y].Free;
end;

function LZOParse(Command: PChar; Option: PInteger;
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
    if (CompareText(S, LZOCodecs[LZO1X_CODEC]) = 0) and LZODLL.DLLLoaded then
    begin
      SetBits(Option^, LZO1X_CODEC, 0, 5);
      SetBits(Option^, LZO1XVariant, 12, 12);
      if Funcs^.GetParam(Command, I, 'l') <> '' then
        SetBits(Option^, StrToInt(Funcs^.GetParam(Command, I, 'l')), 5, 7);
      if Funcs^.GetParam(Command, I, 'v') = '999' then
        SetBits(Option^, 999, 12, 12);
      Result := True;
    end
    else if (CompareText(S, LZOCodecs[LZO2A_CODEC]) = 0) and LZODLL.DLLLoaded
    then
    begin
      SetBits(Option^, LZO2A_CODEC, 0, 5);
      SetBits(Option^, LZO2AVariant, 12, 12);
      if Funcs^.GetParam(Command, I, 'v') = '999' then
        SetBits(Option^, 999, 12, 12);
      Result := True;
    end
    else if (CompareText(S, LZOCodecs[LZO1C_CODEC]) = 0) and LZODLL.DLLLoaded
    then
    begin
      SetBits(Option^, LZO1C_CODEC, 0, 5);
      SetBits(Option^, LZO1CVariant, 12, 12);
      if Funcs^.GetParam(Command, I, 'v') = '999' then
        SetBits(Option^, 999, 12, 12);
      Result := True;
    end;
    Inc(I);
  end;
end;

procedure LZOScan1(Instance, Depth: Integer; Input: PByte;
  Size, SizeEx: NativeInt; Output: _PrecompOutput; Add: _PrecompAdd;
  Funcs: PPrecompFuncs);
var
  Buffer: PByte;
  X: Integer;
  Res: NativeUInt;
  Pos: NativeInt;
  LZOSI: TLZOSI;
  SI: _StrInfo1;
  DI1, DI2: TDepthInfo;
  DS: TPrecompStr;
begin
  DI1 := Funcs^.GetDepthInfo(Instance);
  DS := Funcs^.GetCodec(DI1.Codec, 0, False);
  if DS <> '' then
  begin
    X := IndexTextW(@DS[0], LZOCodecs);
    if (X < 0) or (DI1.OldSize <> SizeEx) then
      exit;
    if not CodecAvailable[X] then
      exit;
    Res := Max(DI1.NewSize, LMaxSize);
    Buffer := Funcs^.Allocator(Instance, Res);
    case X of
      LZO1X_CODEC:
        if not lzo1x_decompress_safe(Input, DI1.OldSize, Buffer, @Res) = 0 then
          Res := 0;
      LZO2A_CODEC:
        if not lzo2a_decompress_safe(Input, DI1.OldSize, Buffer, @Res) = 0 then
          Res := 0;
      LZO1C_CODEC:
        if not lzo1c_decompress_safe(Input, DI1.OldSize, Buffer, @Res) = 0 then
          Res := 0;
    end;
    if (Res > DI1.OldSize) then
    begin
      Output(Instance, Buffer, Res);
      SI.Position := 0;
      SI.OldSize := DI1.OldSize;
      SI.NewSize := Res;
      SI.Option := 0;
      SetBits(SI.Option, X, 0, 5);
      case X of
        LZO1X_CODEC:
          SetBits(SI.Option, LZO1XVariant, 12, 12);
        LZO2A_CODEC:
          SetBits(SI.Option, LZO2AVariant, 12, 12);
        LZO1C_CODEC:
          SetBits(SI.Option, LZO1CVariant, 12, 12);
      end;
      if System.Pos(SPrecompSep2, DI1.Codec) > 0 then
        SI.Status := TStreamStatus.Predicted
      else
        SI.Status := TStreamStatus.None;
      DS := Funcs^.GetDepthCodec(DI1.Codec);
      Move(DS[0], DI2.Codec, SizeOf(DI2.Codec));
      DI2.OldSize := SI.NewSize;
      DI2.NewSize := SI.NewSize;
      Funcs^.LogScan1(LZOCodecs[GetBits(SI.Option, 0, 5)], SI.Position,
        SI.OldSize, SI.NewSize);
      Add(Instance, @SI, DI1.Codec, @DI2);
    end;
    exit;
  end;
  if BoolArray(CodecEnabled, False) then
    exit;
  Buffer := Funcs^.Allocator(Instance, LMaxSize);
  Pos := 0;
  while Pos < Size do
  begin
    if GetLZO1XSI(Input + Pos, SizeEx - Pos, Buffer, LMaxSize, @LZOSI) then
    begin
      Output(Instance, Buffer, LZOSI.DSize);
      SI.Position := Pos;
      SI.OldSize := LZOSI.CSize;
      SI.NewSize := LZOSI.DSize;
      SI.Option := 0;
      SetBits(SI.Option, LZO1X_CODEC, 0, 5);
      SetBits(SI.Option, LZO1XVariant, 12, 12);
      SI.Status := TStreamStatus.None;
      Funcs^.LogScan1(LZOCodecs[GetBits(SI.Option, 0, 5)], SI.Position,
        SI.OldSize, SI.NewSize);
      Add(Instance, @SI, nil, nil);
      Inc(Pos, LZOSI.CSize);
      continue;
    end;
    Inc(Pos);
  end;
end;

function LZOScan2(Instance, Depth: Integer; Input: Pointer; Size: NativeInt;
  StreamInfo: PStrInfo2; Offset: PInteger; Output: _PrecompOutput;
  Funcs: PPrecompFuncs): Boolean;
var
  Buffer: PByte;
  X: Integer;
  Res: NativeUInt;
begin
  Result := False;
  X := GetBits(StreamInfo^.Option, 0, 5);
  if StreamInfo^.OldSize <= 0 then
    exit;
  Res := Max(StreamInfo^.NewSize, LMaxSize);
  Buffer := Funcs^.Allocator(Instance, Res);
  case X of
    LZO1X_CODEC:
      if not lzo1x_decompress_safe(Input, StreamInfo^.OldSize, Buffer, @Res) = 0
      then
        Res := 0;
    LZO2A_CODEC:
      if not lzo2a_decompress_safe(Input, StreamInfo^.OldSize, Buffer, @Res) = 0
      then
        Res := 0;
    LZO1C_CODEC:
      if not lzo1c_decompress_safe(Input, StreamInfo^.OldSize, Buffer, @Res) = 0
      then
        Res := 0;
  end;
  if Res > StreamInfo^.OldSize then
  begin
    Output(Instance, Buffer, Res);
    StreamInfo^.NewSize := Res;
    Funcs^.LogScan2(LZOCodecs[GetBits(StreamInfo^.Option, 0, 5)],
      StreamInfo^.OldSize, StreamInfo^.NewSize);
    Result := True;
  end;
end;

function LZOProcess(Instance, Depth: Integer; OldInput, NewInput: Pointer;
  StreamInfo: PStrInfo2; Output: _PrecompOutput; Funcs: PPrecompFuncs): Boolean;
var
  Buffer: PByte;
  Params: String;
  I: Integer;
  X: Integer;
  Res1: NativeUInt;
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
    Res1 := StreamInfo^.NewSize;
    case X of
      LZO1X_CODEC:
        case GetBits(StreamInfo^.Option, 12, 12) of
          LZO1X_999:
            begin
              Params := 'l' + I.ToString + ':' + 'v' +
                GetBits(StreamInfo^.Option, 12, 12).ToString;
              if not Result then
                if not lzo1x_999_compress_level(NewInput, StreamInfo^.NewSize,
                  Buffer, @Res1, @WrkMem[Instance, 0], nil, 0, nil, I) = 0 then
                  Res1 := 0;
            end;
        end;
      LZO2A_CODEC:
        case GetBits(StreamInfo^.Option, 12, 12) of
          LZO2A_999:
            begin
              Params := 'v' + GetBits(StreamInfo^.Option, 12, 12).ToString;
              if not Result then
                if not lzo2a_999_compress(NewInput, StreamInfo^.NewSize, Buffer,
                  @Res1, @WrkMem[Instance, 0]) = 0 then
                  Res1 := 0;
            end;
        end;
      LZO1C_CODEC:
        case GetBits(StreamInfo^.Option, 12, 12) of
          LZO1C_999:
            begin
              Params := 'v' + GetBits(StreamInfo^.Option, 12, 12).ToString;
              if not Result then
                if not lzo1c_999_compress(NewInput, StreamInfo^.NewSize, Buffer,
                  @Res1, @WrkMem[Instance, 0]) = 0 then
                  Res1 := 0;
            end;
        end;
    end;
    if not Result then
      Result := (Res1 = StreamInfo^.OldSize) and CompareMem(OldInput, Buffer,
        StreamInfo^.OldSize);
    Funcs^.LogProcess(LZOCodecs[GetBits(StreamInfo^.Option, 0, 5)],
      PChar(Params), StreamInfo^.OldSize, StreamInfo^.NewSize, Res1, Result);
    if Result or (StreamInfo^.Status >= TStreamStatus.Predicted) then
      break;
  end;
  { if (Result = False) and ((StreamInfo^.Status >= TStreamStatus.Predicted) or
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
    end; }
  if Result then
  begin
    SetBits(StreamInfo^.Option, I, 5, 7);
    SOList[Instance][X].Add(I);
  end;
end;

function LZORestore(Instance, Depth: Integer; Input, InputExt: Pointer;
  StreamInfo: _StrInfo3; Output: _PrecompOutput; Funcs: PPrecompFuncs): Boolean;
var
  Buffer: PByte;
  Params: String;
  X: Integer;
  Res1: NativeUInt;
  Res2: NativeUInt;
begin
  Result := False;
  X := GetBits(StreamInfo.Option, 0, 5);
  if BoolArray(CodecAvailable, False) or (CodecAvailable[X] = False) then
    exit;
  Params := '';
  Buffer := Funcs^.Allocator(Instance, StreamInfo.NewSize);
  Res1 := StreamInfo.NewSize;
  case X of
    LZO1X_CODEC:
      case GetBits(StreamInfo.Option, 12, 12) of
        LZO1X_999:
          begin
            Params := 'l' + GetBits(StreamInfo.Option, 5, 7).ToString + ':' +
              'v' + GetBits(StreamInfo.Option, 12, 12).ToString;
            if not lzo1x_999_compress_level(Input, StreamInfo.NewSize, Buffer,
              @Res1, @WrkMem[Instance, 0], nil, 0, nil,
              GetBits(StreamInfo.Option, 5, 7)) = 0 then
              Res1 := 0;
          end;
      end;
    LZO2A_CODEC:
      case GetBits(StreamInfo.Option, 12, 12) of
        LZO2A_999:
          begin
            Params := 'v' + GetBits(StreamInfo.Option, 12, 12).ToString;
            if not lzo2a_999_compress(Input, StreamInfo.NewSize, Buffer, @Res1,
              @WrkMem[Instance, 0]) = 0 then
              Res1 := 0;
          end;
      end;
    LZO1C_CODEC:
      case GetBits(StreamInfo.Option, 12, 12) of
        LZO1C_999:
          begin
            Params := 'v' + GetBits(StreamInfo.Option, 12, 12).ToString;
            if not lzo2a_999_compress(Input, StreamInfo.NewSize, Buffer, @Res1,
              @WrkMem[Instance, 0]) = 0 then
              Res1 := 0;
          end;
      end;
  end;
  Funcs^.LogRestore(LZOCodecs[GetBits(StreamInfo.Option, 0, 5)], PChar(Params),
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
for I := Low(LZOCodecs) to High(LZOCodecs) do
begin
  Codec.Names := Codec.Names + [LZOCodecs[I]];
  StockMethods.Add(LZOCodecs[I]);
end;
Codec.Initialised := False;
Codec.Init := @LZOInit;
Codec.Free := @LZOFree;
Codec.Parse := @LZOParse;
Codec.Scan1 := @LZOScan1;
Codec.Scan2 := @LZOScan2;
Codec.Process := @LZOProcess;
Codec.Restore := @LZORestore;
SetLength(CodecAvailable, Length(Codec.Names));
SetLength(CodecEnabled, Length(Codec.Names));

end.
