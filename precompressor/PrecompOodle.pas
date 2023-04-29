unit PrecompOodle;

interface

uses
  OodleDLL,
  Utils,
  PrecompUtils,
  System.SysUtils, System.Classes, System.Types, System.Math;

var
  Codec: TPrecompressor;

implementation

const
  OodleCodecs: array of PChar = ['lzna', 'kraken', 'mermaid', 'selkie', 'hydra',
    'leviathan'];
  CODEC_COUNT = 6;
  LZNA_CODEC = 0;
  KRAKEN_CODEC = 1;
  MERMAID_CODEC = 2;
  SELKIE_CODEC = 3;
  HYDRA_CODEC = 4;
  LEVIATHAN_CODEC = 5;

const
  O_MAXSIZE = 16 * 1024 * 1024;
  O_WORKMEM = 64 * 1024 * 1024;
  O_LENGTH = 32;
  O_TRADEOFF = 256;
  O_DICTIONARY = 0;

var
  SOList: array of array [0 .. CODEC_COUNT - 1] of TSOList;
  OMaxSize: Integer = O_MAXSIZE;
  OLength: Integer = O_LENGTH;
  OTradeOff: Integer = O_TRADEOFF;
  ODictionary: Integer = O_DICTIONARY;
  CodecAvailable, CodecEnabled: TArray<Boolean>;

type
  POodleSI = ^TOodleSI;

  TOodleSI = record
    CSize, DSize: Integer;
    Codec: Integer;
    HasCRC: Boolean;
  end;

procedure GetOodleSI(Buff: PByte; Size: Integer; StreamInfo: POodleSI;
  MaxBlocks: Integer = Integer.MaxValue; First: Boolean = True);
const
  MinSize = 64;
  BlkSize = 262144;
var
  I, J, K: Integer;
  Compressed: Boolean;
begin
  if MaxBlocks <= 0 then
    exit;
  I := 0;
  if First then
  begin
    StreamInfo^.CSize := 0;
    StreamInfo^.DSize := 0;
    StreamInfo^.Codec := 0;
    StreamInfo^.HasCRC := False;
    if Size < 8 then
      exit;
    if ((Buff^ in [$8C, $CC]) = False) then
      exit;
    Compressed := Buff^ = $8C;
    if Compressed then
    begin
      case (Buff + 1)^ of
        { $02:
          if not(((Buff + 2)^ shr 4 = 0) and (((Buff + 4)^ shr 4 = $F) or
          ((Buff + 4)^ and $F = $F))) then
          exit; }
        $06, $0A, $0C:
          begin
            I := EndianSwap(PInteger(Buff + 2)^) shr 8 + 6;
            J := ((EndianSwap(PInteger(Buff + 5)^) shr 8) and $7FFFF) + 8;
            if I > J then
            begin
              K := ((EndianSwap(PInteger(Buff + J)^) shr 8) and $7FFFF) + 3;
              if I <> (J + K) then
                exit;
            end
            else if I <> J then
              exit;
          end;
        $86, $8A, $8C:
          begin
            StreamInfo^.HasCRC := True;
            I := EndianSwap(PInteger(Buff + 2)^) shr 8 + 9;
            J := ((EndianSwap(PInteger(Buff + 8)^) shr 8) and $7FFFF) + 11;
            if I > J then
            begin
              K := ((EndianSwap(PInteger(Buff + J)^) shr 8) and $7FFFF) + 3;
              if I <> (J + K) then
                exit;
            end
            else if I <> J then
              exit;
          end;
      else
        exit;
      end;
    end
    else
    begin
      if not(Buff + 1)^ in [ { $02, } $06, $0A, $0C] then
        exit;
    end;
    case (Buff + 1)^ of
      { $02:
        StreamInfo^.Codec := 0; // Old oodle }
      $06, $86:
        StreamInfo^.Codec := 1; // Kraken
      $0A, $8A:
        StreamInfo^.Codec := 2; // Mermaid/Selkie
      $0C, $8C:
        StreamInfo^.Codec := 3; // Leviathan
    end;
  end
  else
  begin
    if not(Buff^ in [$0C, $4C]) then
      exit;
    Compressed := Buff^ = $0C;
    if Compressed then
    begin
      case (Buff + 1)^ of
        { $02:
          if not(((Buff + 2)^ shr 4 = 0) and (((Buff + 4)^ shr 4 = $F) or
          ((Buff + 4)^ and $F = $F))) then
          exit; }
        $06, $0A, $0C:
          if not(Buff + 5)^ shr 4 in [3, 8] then
            exit;
        $86, $8A, $8C:
          if not(Buff + 8)^ shr 4 in [3, 8] then
            exit;
      end;
    end
    else
    begin
      if not(Buff + 1)^ in [$06, $0A, $0C] then
        exit;
    end;
  end;
  if Compressed then
  begin
    case (Buff + 1)^ of
      { $02:
        I := EndianSwap(PWord(Buff + 2)^) + 5; }
      $06, $0A, $0C:
        I := EndianSwap(PInteger(Buff + 2)^) shr 8 + 6;
      $86, $8A, $8C:
        I := EndianSwap(PInteger(Buff + 2)^) shr 8 + 9;
    else
      exit;
    end;
    if First and (I < MinSize) then
      exit;
    if StreamInfo^.CSize + I > Size then
    begin
      StreamInfo^.CSize := 0;
      StreamInfo^.DSize := 0;
      exit;
    end;
    if I = $00080005 then
      I := 6;
    Inc(StreamInfo^.CSize, I);
    Inc(StreamInfo^.DSize, BlkSize);
    Dec(MaxBlocks);
    GetOodleSI(Buff + I, Size, StreamInfo, MaxBlocks, False);
  end
  else
  begin
    case (Buff + 1)^ of
      $06, $0A, $0C:
        begin
          if (StreamInfo^.CSize + BlkSize + 3 <= Size) and
            ((Buff + BlkSize + 2)^ in [$0C, $4C]) and
            ((Buff + BlkSize + 3)^ = (Buff + 1)^) then
          begin
            Inc(StreamInfo^.CSize, BlkSize + 2);
            Inc(StreamInfo^.DSize, BlkSize);
          end
          else if (First = False) and (StreamInfo^.CSize + 8 <= Size) then
          begin
            Inc(StreamInfo^.CSize, 8 + 2);
            Inc(StreamInfo^.DSize, 8);
            exit;
          end
          else
            exit;
          Dec(MaxBlocks);
          GetOodleSI(Buff + BlkSize + 2, Size, StreamInfo, MaxBlocks, False);
        end;
    else
      exit;
    end;
  end;
end;

procedure OodleDecompressCB(userdata: Pointer; rawBuf: PByte;
  rawLen: NativeUInt; compBuf: PByte; compBufferSize, rawDone,
  compUsed: NativeUInt);
begin
  WriteLn(ErrOutput, rawDone);
end;

function LocalLZ_Decompress(asrc, adst: PByte; asrcSize, adstCapacity: Integer;
  aIdent: Byte; out Res: Integer): Integer;
const
  BlkSize = 262144;
var
  A, B: Integer;
begin
  B := IfThen(adstCapacity mod BlkSize = 0, Pred(adstCapacity div BlkSize),
    adstCapacity div BlkSize) * BlkSize;
  FillChar((adst + B)^, adstCapacity - B, aIdent);
  Res := OodleLZ_Decompress(asrc, asrcSize, adst, adstCapacity);
  A := Pred(adstCapacity);
  while A > B do
  begin
    if (adst + A)^ <> aIdent then
      break;
    Dec(A);
  end;
  Inc(A);
  Result := A;
end;

function CustomLZ_Decompress(src, dst: PByte; srcSize, dstCapacity: Integer;
  var Res: Integer): Boolean;
const
  BlkSize = 262144;
var
  W, X, Y, Z: Integer;
begin
  Result := False;
  Y := 0;
  X := dstCapacity;
  X := Min(Max(LocalLZ_Decompress(src, dst, srcSize, X, 0, Y),
    LocalLZ_Decompress(src, dst, srcSize, X, 1, Z)), Pred(X));
  W := X;
  while (Y = 0) and (X > dstCapacity - BlkSize) and (W - X < OLength) do
  begin
    X := Min(Max(LocalLZ_Decompress(src, dst, srcSize, X, 0, Y),
      LocalLZ_Decompress(src, dst, srcSize, X, 1, Y)), Pred(X));
  end;
  X := Min(Succ(W), dstCapacity);
  while (Z = 0) and (X < Min(W + OLength, dstCapacity)) do
  begin
    LocalLZ_Decompress(src, dst, srcSize, X, 0, Z);
    Inc(X);
  end;
  Y := Max(Y, Z);
  if (Y > 0) then
  begin
    Res := Y;
    Result := True;
  end;
end;

function GetOodleUS(Instance: Integer; Input: PByte; Pos: NativeInt;
  StreamInfo: POodleSI; Output: _PrecompOutput; Add: _PrecompAdd;
  Funcs: PPrecompFuncs): Integer;
const
  MinSize = 64;
var
  Buffer: PByte;
  B: Boolean;
  SI: _StrInfo1;
begin
  Result := 0;
  case StreamInfo^.Codec of
    1:
      if (CodecEnabled[KRAKEN_CODEC] = False) and
        (CodecEnabled[HYDRA_CODEC] = False) then
        exit;
    2:
      if (CodecEnabled[MERMAID_CODEC] = False) and
        (CodecEnabled[SELKIE_CODEC] = False) and
        (CodecEnabled[HYDRA_CODEC] = False) then
        exit;
    3:
      if (CodecEnabled[LEVIATHAN_CODEC] = False) and
        (CodecEnabled[HYDRA_CODEC] = False) then
        exit;
  else
    exit;
  end;
  Buffer := Funcs^.Allocator(Instance, StreamInfo^.DSize);
  B := CustomLZ_Decompress(Input + Pos, Buffer, StreamInfo^.CSize,
    StreamInfo^.DSize, Result);
  If B then
    if (Result > MinSize) and (Result > StreamInfo^.CSize) then
    begin
      Output(Instance, Buffer, Result);
      SI.Position := Pos;
      SI.OldSize := StreamInfo^.CSize;
      SI.Option := 0;
      SetBits(SI.Option, OTradeOff, 13, 11);
      case StreamInfo^.Codec of
        1:
          if CodecEnabled[KRAKEN_CODEC] then
            SetBits(SI.Option, KRAKEN_CODEC, 0, 5);
        2:
          if CodecEnabled[MERMAID_CODEC] then
            SetBits(SI.Option, MERMAID_CODEC, 0, 5)
          else if CodecEnabled[SELKIE_CODEC] then
            SetBits(SI.Option, SELKIE_CODEC, 0, 5);
        3:
          if CodecEnabled[LEVIATHAN_CODEC] then
            SetBits(SI.Option, LEVIATHAN_CODEC, 0, 5);
      end;
      if CodecEnabled[HYDRA_CODEC] then
        SetBits(SI.Option, HYDRA_CODEC, 0, 5);
      SetBits(SI.Option, Integer(StreamInfo^.HasCRC), 12, 1);
      SI.Status := TStreamStatus.None;
      SI.NewSize := Result;
      Funcs^.LogScan1(OodleCodecs[GetBits(SI.Option, 0, 5)], SI.Position,
        SI.OldSize, SI.NewSize);
      Add(Instance, @SI, nil, nil);
    end;
end;

function GetOodleCodec(Index: Integer): Integer;
begin
  case Index of
    LZNA_CODEC:
      Result := 7;
    KRAKEN_CODEC:
      Result := 8;
    MERMAID_CODEC:
      Result := 9;
    SELKIE_CODEC:
      Result := 11;
    HYDRA_CODEC:
      Result := 12;
    LEVIATHAN_CODEC:
      Result := 13;
  else
    Result := 8;
  end;
end;

function OodleInit(Command: PChar; Count: Integer;
  Funcs: PPrecompFuncs): Boolean;
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
    CodecAvailable[X] := OodleDLL.DLLLoaded;
  X := 0;
  while Funcs^.GetCodec(Command, X, False) <> '' do
  begin
    S := Funcs^.GetCodec(Command, X, False);
    for Y := Low(OodleCodecs) to High(OodleCodecs) do
      if (CompareText(S, OodleCodecs[Y]) = 0) and OodleDLL.DLLLoaded then
      begin
        CodecEnabled[Y] := True;
        if Funcs^.GetParam(Command, X, 'l') <> '' then
          for I := Low(SOList) to High(SOList) do
            SOList[I][Y].Update
              ([StrToInt(Funcs^.GetParam(Command, X, 'l'))], True);
        if Funcs^.GetParam(Command, X, 's') <> '' then
          OMaxSize := ConvertToBytes(Funcs^.GetParam(Command, X, 's'));
        if Funcs^.GetParam(Command, X, 'n') <> '' then
          OLength := StrToInt(Funcs^.GetParam(Command, X, 'n'));
        if Funcs^.GetParam(Command, X, 't') <> '' then
          OTradeOff := StrToInt(Funcs^.GetParam(Command, X, 't'));
        if Funcs^.GetParam(Command, X, 'd') <> '' then
          ODictionary := StrToInt(Funcs^.GetParam(Command, X, 'd'));
      end;
    Inc(X);
  end;
  SetLength(Options, 0);
  for I := 1 to 9 do
    Insert(I, Options, Length(Options));
  for X := Low(SOList) to High(SOList) do
    for Y := Low(SOList[X]) to High(SOList[X]) do
      if SOList[X, Y].Count = 0 then
        SOList[X, Y].Update(Options);
end;

procedure OodleFree(Funcs: PPrecompFuncs);
var
  X, Y: Integer;
begin
  for X := Low(SOList) to High(SOList) do
    for Y := Low(SOList[X]) to High(SOList[X]) do
      SOList[X, Y].Free;
end;

function OodleParse(Command: PChar; Option: PInteger;
  Funcs: PPrecompFuncs): Boolean;
var
  S: String;
  I, J: Integer;
begin
  Result := False;
  Option^ := 0;
  SetBits(Option^, OTradeOff, 13, 11);
  I := 0;
  while Funcs^.GetCodec(Command, I, False) <> '' do
  begin
    S := Funcs^.GetCodec(Command, I, False);
    for J := Low(OodleCodecs) to High(OodleCodecs) do
      if (CompareText(S, OodleCodecs[J]) = 0) and OodleDLL.DLLLoaded then
      begin
        SetBits(Option^, J, 0, 5);
        if Funcs^.GetParam(Command, I, 'l') <> '' then
          SetBits(Option^, StrToInt(Funcs^.GetParam(Command, I, 'l')), 5, 7);
        if String(Funcs^.GetParam(Command, I, 'c')) = '1' then
          SetBits(Option^, 1, 12, 1);
        if Funcs^.GetParam(Command, I, 't') <> '' then
          SetBits(Option^, StrToInt(Funcs^.GetParam(Command, I, 't')), 13, 11);
        if Funcs^.GetParam(Command, I, 'd') <> '' then
          SetBits(Option^, StrToInt(Funcs^.GetParam(Command, I, 'd')), 24, 5);
        Result := True;
      end;
    Inc(I);
  end;
end;

procedure OodleScan1(Instance, Depth: Integer; Input: PByte;
  Size, SizeEx: NativeInt; Output: _PrecompOutput; Add: _PrecompAdd;
  Funcs: PPrecompFuncs);
var
  Buffer: PByte;
  Pos: NativeInt;
  X: Integer;
  Res: Integer;
  SI: _StrInfo1;
  OodleSI: TOodleSI;
  DI1, DI2: TDepthInfo;
  DS: TPrecompStr;
begin
  DI1 := Funcs^.GetDepthInfo(Instance);
  DS := Funcs^.GetCodec(DI1.Codec, 0, False);
  if DS <> '' then
  begin
    X := IndexTextW(@DS[0], OodleCodecs);
    if X < 0 then
      exit;
    if DI1.OldSize = 0 then
      DI1.OldSize := SizeEx;
    if not CodecAvailable[X] then
      exit;
    if not CodecAvailable[X] then
      exit;
    if (X in [LZNA_CODEC, LEVIATHAN_CODEC]) and (DI1.NewSize <= 0) then
      exit;
    if DI1.NewSize <= 0 then
      Res := OMaxSize
    else
      Res := DI1.NewSize;
    Buffer := Funcs^.Allocator(Instance, Res);
    case X of
      KRAKEN_CODEC, MERMAID_CODEC, SELKIE_CODEC, HYDRA_CODEC:
        begin
          if DI1.NewSize <= 0 then
          begin
            if not CustomLZ_Decompress(Input, Buffer, DI1.OldSize, Res, Res)
            then
              Res := 0;
          end
          else
            Res := OodleLZ_Decompress(Input, DI1.OldSize, Buffer, Res);
        end;
    else
      begin
        if DI1.NewSize > 0 then
          Res := OodleLZ_Decompress(Input, DI1.OldSize, Buffer, Res)
        else
          Res := 0;
      end;
    end;
    if (Res > DI1.OldSize) then
    begin
      Output(Instance, Buffer, Res);
      SI.Position := 0;
      SI.OldSize := DI1.OldSize;
      SI.NewSize := Res;
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
      Funcs^.LogScan1(OodleCodecs[GetBits(SI.Option, 0, 5)], SI.Position,
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
    try
      while Pos < Size do
      begin
        GetOodleSI(Input + Pos, SizeEx - Pos, @OodleSI);
        if (OodleSI.CSize > 0) then
        begin
          try
            if GetOodleUS(Instance, Input, Pos, @OodleSI, Output, Add, Funcs) > 0
            then
            begin
              Inc(Pos, OodleSI.CSize);
              continue;
            end;
          except
          end;
        end;
        Inc(Pos);
      end;
    except
      Inc(Pos);
    end;
  end;
end;

function OodleScan2(Instance, Depth: Integer; Input: Pointer; Size: cardinal;
  StreamInfo: PStrInfo2; Offset: PInteger; Output: _PrecompOutput;
  Funcs: PPrecompFuncs): Boolean;
var
  Buffer: PByte;
  B: Boolean;
  I: Integer;
  ResultN: TIntegerDynArray;
  X: Integer;
  Res: Integer;
  OodleSI: TOodleSI;
begin
  Result := False;
  X := GetBits(StreamInfo^.Option, 0, 5);
  if (X <> LZNA_CODEC) and (StreamInfo^.OldSize <= 0) then
  begin
    GetOodleSI(Input, Size, @OodleSI);
    StreamInfo^.OldSize := OodleSI.CSize;
  end;
  if StreamInfo^.NewSize > 0 then
  begin
    Buffer := Funcs^.Allocator(Instance, StreamInfo^.NewSize);
    Res := OodleLZ_Decompress(Input, StreamInfo^.OldSize, Buffer,
      StreamInfo^.NewSize);
    if Res = StreamInfo^.NewSize then
    begin
      Output(Instance, Buffer, Res);
      Result := True;
    end;
  end
  else if (not X in [LZNA_CODEC, LEVIATHAN_CODEC]) and (StreamInfo^.NewSize <= 0)
  then
  begin
    Buffer := Funcs^.Allocator(Instance, OodleSI.DSize);
    if CustomLZ_Decompress(Input, Buffer, StreamInfo^.OldSize,
      OodleSI.DSize, Res) then
    begin
      Output(Instance, Buffer, Res);
      StreamInfo^.NewSize := Res;
      Funcs^.LogScan2(OodleCodecs[GetBits(StreamInfo^.Option, 0, 5)],
        StreamInfo^.OldSize, StreamInfo^.NewSize);
      Result := True;
    end;
  end;
end;

function OodleProcess(Instance, Depth: Integer; OldInput, NewInput: Pointer;
  StreamInfo: PStrInfo2; Output: _PrecompOutput; Funcs: PPrecompFuncs): Boolean;
var
  Buffer, Work: PByte;
  Params: String;
  A, B: Integer;
  I: Integer;
  W: Integer;
  X, Y: Integer;
  Res1: Integer;
  Res2: NativeUInt;
  COptions: TOodleLZ_CompressOptions;
begin
  Result := False;
  X := GetBits(StreamInfo^.Option, 0, 5);
  if BoolArray(CodecAvailable, False) or (CodecAvailable[X] = False) then
    exit;
  Y := GetOodleCodec(X);
  Buffer := Funcs^.Allocator(Instance, OodleLZ_GetCompressedBufferSizeNeeded(Y,
    StreamInfo^.NewSize) + IfThen(OldCompress, 0, O_WORKMEM));
  if OldCompress then
  begin
    Work := nil;
    W := 0
  end
  else
  begin
    Work := Buffer + OodleLZ_GetCompressedBufferSizeNeeded(Y,
      StreamInfo^.NewSize);
    W := O_WORKMEM;
  end;
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
    Move(OodleLZ_CompressOptions_GetDefault(Y, I)^, COptions,
      SizeOf(TOodleLZ_CompressOptions));
    COptions.sendQuantumCRCs := GetBits(StreamInfo^.Option, 12, 1) = 1;
    COptions.spaceSpeedTradeoffBytes := GetBits(StreamInfo^.Option, 13, 11);
    COptions.dictionarySize := IfThen(GetBits(StreamInfo^.Option, 24, 5) = 0, 0,
      Round(Power(2, GetBits(StreamInfo^.Option, 24, 5))));
    Params := 'l' + I.ToString + ':' + 'c' + GetBits(StreamInfo^.Option, 12, 1)
      .ToString + ':' + 't' + GetBits(StreamInfo^.Option, 13, 11).ToString + ':'
      + 'd' + GetBits(StreamInfo^.Option, 24, 5).ToString;
    if not Result then
      Res1 := OodleLZ_Compress(Y, NewInput, StreamInfo^.NewSize, Buffer, I,
        @COptions, nil, nil, Work, W);
    if not Result then
      Result := (Res1 = StreamInfo^.OldSize) and CompareMem(OldInput, Buffer,
        StreamInfo^.OldSize);
    Funcs^.LogProcess(OodleCodecs[GetBits(StreamInfo^.Option, 0, 5)],
      PChar(Params), StreamInfo^.OldSize, StreamInfo^.NewSize, Res1, Result);
    if Result or (StreamInfo^.Status = TStreamStatus.Predicted) then
      break;
  end;
  if Result and OPTIMISE_DEC and (StreamInfo^.Status <> TStreamStatus.Database)
  then
  begin
    A := Pred(I);
    for B := A downto 1 do
    begin
      Move(OodleLZ_CompressOptions_GetDefault(Y, B)^, COptions,
        SizeOf(TOodleLZ_CompressOptions));
      COptions.sendQuantumCRCs := GetBits(StreamInfo^.Option, 12, 1) = 1;
      COptions.spaceSpeedTradeoffBytes := GetBits(StreamInfo^.Option, 13, 11);
      COptions.dictionarySize := IfThen(GetBits(StreamInfo^.Option, 24, 5) = 0,
        0, Round(Power(2, GetBits(StreamInfo^.Option, 24, 5))));
      Res1 := OodleLZ_Compress(Y, NewInput, StreamInfo^.NewSize, Buffer, B,
        @COptions, nil, nil, Work, W);
      if (Res1 = StreamInfo^.OldSize) and CompareMem(OldInput, Buffer,
        StreamInfo^.OldSize) then
        I := B
      else
        break;
    end;
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

function OodleRestore(Instance, Depth: Integer; Input, InputExt: Pointer;
  StreamInfo: _StrInfo3; Output: _PrecompOutput; Funcs: PPrecompFuncs): Boolean;
var
  Buffer, Work: PByte;
  Params: String;
  W: Integer;
  X, Y: Integer;
  Res1: Integer;
  Res2: NativeUInt;
  COptions: TOodleLZ_CompressOptions;
begin
  Result := False;
  X := GetBits(StreamInfo.Option, 0, 5);
  if CodecAvailable[X] = False then
    exit;
  Y := GetOodleCodec(X);
  Buffer := Funcs^.Allocator(Instance, OodleLZ_GetCompressedBufferSizeNeeded(Y,
    StreamInfo.NewSize) + IfThen(OldCompress, 0, O_WORKMEM));
  if OldCompress then
  begin
    Work := nil;
    W := 0
  end
  else
  begin
    Work := Buffer + OodleLZ_GetCompressedBufferSizeNeeded(Y,
      StreamInfo.NewSize);
    W := O_WORKMEM;
  end;
  Move(OodleLZ_CompressOptions_GetDefault(Y, GetBits(StreamInfo.Option, 5, 7))^,
    COptions, SizeOf(TOodleLZ_CompressOptions));
  COptions.sendQuantumCRCs := GetBits(StreamInfo.Option, 12, 1) = 1;
  COptions.spaceSpeedTradeoffBytes := GetBits(StreamInfo.Option, 13, 11);
  COptions.dictionarySize := IfThen(GetBits(StreamInfo.Option, 24, 5) = 0, 0,
    Round(Power(2, GetBits(StreamInfo.Option, 24, 5))));
  Params := 'l' + GetBits(StreamInfo.Option, 5, 7).ToString + ':' + 'c' +
    GetBits(StreamInfo.Option, 12, 1).ToString + ':' + 't' +
    GetBits(StreamInfo.Option, 13, 11).ToString + ':' + 'd' +
    GetBits(StreamInfo.Option, 24, 5).ToString;
  Res1 := OodleLZ_Compress(Y, Input, StreamInfo.NewSize, Buffer,
    GetBits(StreamInfo.Option, 5, 7), @COptions, nil, nil, Work, W);
  Funcs^.LogRestore(OodleCodecs[GetBits(StreamInfo.Option, 0, 5)],
    PChar(Params), StreamInfo.OldSize, StreamInfo.NewSize, Res1, True);
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
for I := Low(OodleCodecs) to High(OodleCodecs) do
begin
  Codec.Names := Codec.Names + [OodleCodecs[I]];
  StockMethods.Add(OodleCodecs[I]);
end;
Codec.Initialised := False;
Codec.Init := @OodleInit;
Codec.Free := @OodleFree;
Codec.Parse := @OodleParse;
Codec.Scan1 := @OodleScan1;
Codec.Scan2 := @OodleScan2;
Codec.Process := @OodleProcess;
Codec.Restore := @OodleRestore;
SetLength(CodecAvailable, Length(Codec.Names));
SetLength(CodecEnabled, Length(Codec.Names));

end.
