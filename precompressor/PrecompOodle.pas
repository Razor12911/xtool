unit PrecompOodle;

interface

uses
  OodleDLL, XDeltaDLL,
  Utils,
  PrecompUtils,
  System.SysUtils, System.Classes, System.Types, System.Math;

{ 8C 07 -  0:LZH
  8C 00 -  1:LZHLW
  8C 01 -  2:LZNIB
  CC 07 -  3:None
  8C 02 -  4:LZB16
  8C 03 -  5:LZBLW
  8C 04 -  6:LZA
  8C 05 -  7:LZNA
  8C 06 -  8:Kraken
  8C 0A -  9:Mermaid
  8C 0B - 10:BitKnit
  8C 0A - 11:Selkie
  8C 0A - 12:Hydra
  8C 0C - 13:Leviathan }

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
  O_COUNT = 0;
  O_TRADEOFF = 256;
  O_MAXSIZE = 16 * 1024 * 1024;

var
  SOList: array of array [0 .. CODEC_COUNT - 1] of TSOList;
  OCount: Integer = O_COUNT;
  OTradeOff: Integer = O_TRADEOFF;
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

function CustomLZ_Decompress0(src, dst: PByte; srcSize, dstCapacity: Integer;
  var Res: Integer): Boolean;

type
  T3Res = array [0 .. 2] of Integer;

  procedure AddRes(const I: Integer; var Res: T3Res);
  begin
    Res[0] := Res[1];
    Res[1] := Res[2];
    Res[2] := I;
  end;

const
  MinSize = 64;
  BlkSize = 262144;
  Range = 262144;

  function ValidSize(Res: T3Res): Boolean;
  const
    ThresSize = 32;
  begin
    Result := (Res[0] > 0) and (Res[0] < Res[1]) and
      InRange(Res[0], Res[0], Res[2] + 32);
  end;

var
  LBuffer: array [0 .. BlkSize - 1] of Byte;
  I, J, W, X, Y, Z: Integer;
  LR1, LR2: T3Res;
begin
  Result := False;
  Y := Max(LocalLZ_Decompress(src, dst, srcSize, dstCapacity, 0, Z),
    LocalLZ_Decompress(src, dst, srcSize, dstCapacity, 1, Z));
  if Y > MinSize then
  begin
    W := IfThen(Y mod BlkSize = 0, Pred(Y div BlkSize), Y div BlkSize)
      * BlkSize;
    Move((dst + W)^, LBuffer[0], Y - W);
  end;
  if (Y = Z) and (Y = dstCapacity) then
  begin
    Res := Y;
    I := Max(LocalLZ_Decompress(src, dst, srcSize, dstCapacity - 1, 0, Z),
      LocalLZ_Decompress(src, dst, srcSize, dstCapacity - 1, 1, Z));
    if (Res <> I) and (Res <> Pred(I)) then
    begin
      Move(LBuffer[0], (dst + W)^, Res - W);
      Result := True;
      exit;
    end;
  end;
  FillChar(LR1, SizeOf(T3Res), 0);
  FillChar(LR2, SizeOf(T3Res), 0);
  I := Y;
  J := Min(dstCapacity, Y + Range);
  while I < J do
  begin
    Y := Max(LocalLZ_Decompress(src, dst, srcSize, I, 0, Z),
      LocalLZ_Decompress(src, dst, srcSize, I, 1, Z));
    AddRes(Y, LR1);
    AddRes(Z, LR2);
    if (LR1[1] = LR2[1]) and ValidSize(LR1) then
    begin
      Res := LR1[1];
      Move(LBuffer[0], (dst + W)^, Res - W);
      Result := True;
      break;
    end;
    if Y > MinSize then
    begin
      W := IfThen(Y mod BlkSize = 0, Pred(Y div BlkSize), Y div BlkSize)
        * BlkSize;
      Move((dst + W)^, LBuffer[0], Y - W);
    end;
    Inc(I);
  end;
end;

function CustomLZ_DecompressN(src, dst: PByte; srcSize, dstCapacity: Integer;
  var Res: TIntegerDynArray): Boolean;

const
  BlkSize = 262144;
  UpLen = 128;
  DownLen = 16;
var
  I, J, X, Y, Z: Integer;
  Sizes: array [0 .. UpLen + DownLen - 1] of Integer;
begin
  SetLength(Res, 0);
  Y := Max(LocalLZ_Decompress(src, dst, srcSize, dstCapacity, 0, Z),
    LocalLZ_Decompress(src, dst, srcSize, dstCapacity, 1, Z));
  for I := Low(Sizes) to High(Sizes) do
    Sizes[I] := -1;
  J := Min(dstCapacity, Y + UpLen);
  I := Max(IfThen(dstCapacity mod BlkSize = 0, Pred(dstCapacity div BlkSize),
    dstCapacity div BlkSize) * BlkSize, Y - DownLen);
  X := J - I;
  while (J > I) do
  begin
    Y := Max(LocalLZ_Decompress(src, dst, srcSize, J, 0, Z),
      LocalLZ_Decompress(src, dst, srcSize, J, 1, Z));
    Sizes[Length(Sizes) - (J - I)] := Z;
    Dec(J);
  end;
  for I := Low(Sizes) to High(Sizes) do
  begin
    X := Sizes[I];
    for J := Low(Sizes) to High(Sizes) do
    begin
      Y := Sizes[J];
      if I <> J then
        if X = Y then
        begin
          Sizes[I] := -1;
          Sizes[J] := -1;
        end;
    end;
  end;
  for I := Low(Sizes) to High(Sizes) do
    if Sizes[I] > srcSize then
      if OodleLZ_Decompress(src, srcSize, dst, Sizes[I]) = Sizes[I] then
      begin
        Insert(Sizes[I], Res, Length(Res));
        if Length(Res) >= OCount then
          break;
      end;
  Result := Length(Res) > 0;
end;

function GetOodleUS(Instance: Integer; Input: PByte; Pos: NativeInt;
  StreamInfo: POodleSI; Output: _PrecompOutput; Add: _PrecompAdd;
  Funcs: PPrecompFuncs): Integer;
const
  MinSize = 64;
var
  Buffer: PByte;
  B: Boolean;
  I: Integer;
  ResultN: TIntegerDynArray;
  SI: _StrInfo1;
begin
  Result := 0;
  { if StreamInfo^.Codec = 3 then
    exit; }
  // StreamInfo^.DSize:=$8001;
  Buffer := Funcs^.Allocator(Instance, StreamInfo^.DSize);
  if OCount <= 0 then
    B := CustomLZ_Decompress0(Input + Pos, Buffer, StreamInfo^.CSize,
      StreamInfo^.DSize, Result)
  else
  begin
    B := CustomLZ_DecompressN(Input + Pos, Buffer, StreamInfo^.CSize,
      StreamInfo^.DSize, ResultN);
    if B then
      Result := ResultN[0];
  end;
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
          SetBits(SI.Option, KRAKEN_CODEC, 0, 5);
        2:
          if CodecEnabled[MERMAID_CODEC] then
            SetBits(SI.Option, MERMAID_CODEC, 0, 5)
          else
            SetBits(SI.Option, SELKIE_CODEC, 0, 5);
        3:
          SetBits(SI.Option, LEVIATHAN_CODEC, 0, 5);
      end;
      if CodecEnabled[HYDRA_CODEC] then
        SetBits(SI.Option, HYDRA_CODEC, 0, 5);
      SetBits(SI.Option, Integer(StreamInfo^.HasCRC), 12, 1);
      SI.Status := TStreamStatus.None;
      if OCount <= 0 then
      begin
        SI.NewSize := Result;
        Funcs^.LogScan1(OodleCodecs[GetBits(SI.Option, 0, 5)], SI.Position,
          SI.OldSize, SI.NewSize);
        Add(Instance, @SI, nil, nil);
      end
      else
      begin
        if Length(ResultN) > 0 then
          for I := Low(ResultN) to High(ResultN) do
          begin
            SI.NewSize := ResultN[I];
            Funcs^.LogScan1(OodleCodecs[GetBits(SI.Option, 0, 5)], SI.Position,
              SI.OldSize, SI.NewSize);
            Add(Instance, @SI, nil, nil);
          end;
      end;
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
        if Funcs^.GetParam(Command, X, 'n') <> '' then
          OCount := StrToInt(Funcs^.GetParam(Command, X, 'n'));
        if Funcs^.GetParam(Command, X, 't') <> '' then
          OTradeOff := StrToInt(Funcs^.GetParam(Command, X, 't'));
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
    if (X < 0) or (DI1.OldSize <> SizeEx) then
      exit;
    if not CodecAvailable[X] then
      exit;
    if (X in [LZNA_CODEC, LEVIATHAN_CODEC]) and (DI1.NewSize <= 0) then
      exit;
    if DI1.NewSize <= 0 then
      Res := O_MAXSIZE
    else
      Res := DI1.NewSize;
    Buffer := Funcs^.Allocator(Instance, Res);
    case X of
      KRAKEN_CODEC, MERMAID_CODEC, SELKIE_CODEC, HYDRA_CODEC:
        begin
          if DI1.NewSize <= 0 then
          begin
            if not CustomLZ_Decompress0(Input, Buffer, DI1.OldSize, Res, Res)
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
    GetOodleSI(Input + Pos, SizeEx - Pos, @OodleSI);
    if (OodleSI.CSize > 0) then
    begin
      if GetOodleUS(Instance, Input, Pos, @OodleSI, Output, Add, Funcs) > 0 then
      begin
        Inc(Pos, OodleSI.CSize);
        continue;
      end;
    end;
    Inc(Pos);
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
    if CustomLZ_Decompress0(Input, Buffer, StreamInfo^.OldSize,
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
  Buffer: PByte;
  Params: String;
  I: Integer;
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
    StreamInfo^.NewSize));
  SOList[Instance][X].Index := 0;
  while SOList[Instance][X].Get(I) >= 0 do
  begin
    if StreamInfo^.Status >= TStreamStatus.Predicted then
    begin
      if GetBits(StreamInfo^.Option, 5, 7) <> I then
        continue;
      if (StreamInfo^.Status = TStreamStatus.Database) and
        (GetBits(StreamInfo^.Option, 1, 31) = 0) then
      begin
        Res1 := StreamInfo^.OldSize;
        Result := True;
      end;
    end;
    Move(OodleLZ_CompressOptions_GetDefault(Y, I)^, COptions,
      SizeOf(TOodleLZ_CompressOptions));
    COptions.sendQuantumCRCs := GetBits(StreamInfo^.Option, 12, 1) = 1;
    COptions.spaceSpeedTradeoffBytes := GetBits(StreamInfo^.Option, 13, 11);
    Params := 'l' + I.ToString + ':' + 'c' + GetBits(StreamInfo^.Option, 12, 1)
      .ToString + ':' + 't' + GetBits(StreamInfo^.Option, 13, 11).ToString;
    if not Result then
      Res1 := OodleLZ_Compress(Y, NewInput, StreamInfo^.NewSize, Buffer, I,
        @COptions);
    if not Result then
      Result := (Res1 = StreamInfo^.OldSize) and CompareMem(OldInput, Buffer,
        StreamInfo^.OldSize);
    Funcs^.LogProcess(OodleCodecs[GetBits(StreamInfo^.Option, 0, 5)],
      PChar(Params), StreamInfo^.OldSize, StreamInfo^.NewSize, Res1, Result);
    if Result or (StreamInfo^.Status = TStreamStatus.Predicted) then
      break;
  end;
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

function OodleRestore(Instance, Depth: Integer; Input, InputExt: Pointer;
  StreamInfo: _StrInfo3; Output: _PrecompOutput; Funcs: PPrecompFuncs): Boolean;
var
  Buffer: PByte;
  Params: String;
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
    StreamInfo.NewSize));
  Move(OodleLZ_CompressOptions_GetDefault(Y, GetBits(StreamInfo.Option, 5, 7))^,
    COptions, SizeOf(TOodleLZ_CompressOptions));
  COptions.sendQuantumCRCs := GetBits(StreamInfo.Option, 12, 1) = 1;
  COptions.spaceSpeedTradeoffBytes := GetBits(StreamInfo.Option, 13, 11);
  Params := 'l' + GetBits(StreamInfo.Option, 5, 7).ToString + ':' + 'c' +
    GetBits(StreamInfo.Option, 12, 1).ToString + ':' + 't' +
    GetBits(StreamInfo.Option, 13, 11).ToString;
  Res1 := OodleLZ_Compress(Y, Input, StreamInfo.NewSize, Buffer,
    GetBits(StreamInfo.Option, 5, 7), @COptions);
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
