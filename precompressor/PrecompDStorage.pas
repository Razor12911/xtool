unit PrecompDStorage;

{$POINTERMATH ON}

interface

uses
  DStorage,
  Utils,
  PrecompUtils,
  WinAPI.Windows,
  System.SysUtils, System.Classes, System.Math;

var
  Codec: TPrecompressor;

implementation

const
  DStorageCodecs: array of PChar = ['gdeflate'];
  CODEC_COUNT = 1;
  GDEFLATE_CODEC = 0;

const
  D_MAXSIZE = 16 * 1024 * 1024;

var
  SOList: array of array [0 .. CODEC_COUNT - 1] of TSOList;
  ctx: array of IDStorageCompressionCodec;
  DMaxSize: Integer = D_MAXSIZE;
  CodecAvailable, CodecEnabled: TArray<Boolean>;

const
  GDefSig = $FB04;

type
  PGDefSI = ^TGDefSI;

  TGDefSI = record
    CSize, DSize: Integer;
  end;

function GetGDefSI(Buff: PByte; Size: Integer; StreamInfo: PGDefSI): Boolean;
type
  PHdr = ^THdr;

  THdr = packed record
    Signature, BlkCount: Word;
    Unk1, LastBlkSize: Cardinal;
  end;
const
  MinSize = 64;
  BlkSize = 65536;
  FailSize = 884;
var
  I, J: Integer;
  DefHdr: THdr;
  StartPos1, StartPos2, LastPos: NativeInt;
begin
  Result := False;
  if Size < 12 then
    exit;
  if PHdr(Buff)^.Signature = GDefSig then
  begin
    DefHdr := PHdr(Buff)^;
    if DefHdr.BlkCount = 0 then
      exit;
    StreamInfo^.CSize := 0;
    StreamInfo^.DSize := 0;
    StartPos1 := sizeof(THdr);
    StartPos2 := sizeof(THdr) + Pred(DefHdr.BlkCount) * Integer.Size;
    LastPos := StartPos2;
    for I := 0 to DefHdr.BlkCount - 1 do
    begin
      if (LastPos + Integer.Size > Size) or (PInteger(Buff + LastPos)^ = 0) then
        exit;
      if I = DefHdr.BlkCount - 1 then
        J := DefHdr.LastBlkSize
      else
        J := (PInteger(Buff + StartPos1) + I)^ - (LastPos - StartPos2);
      Inc(LastPos, J);
      if (InRange(J, MinSize, BlkSize + FailSize) = False) or (LastPos > Size)
        or (PInteger(Buff + LastPos - Integer.Size)^ <> 0) then
        exit;
      Inc(StreamInfo^.CSize, J);
      Inc(StreamInfo^.DSize, BlkSize);
    end;
    Inc(StreamInfo^.CSize, StartPos2);
    Result := StreamInfo^.CSize >= MinSize;
  end;
end;

function GetGDefUS(Instance: Integer; Input: PByte; Pos: NativeInt;
  StreamInfo: PGDefSI; Output: _PrecompOutput; Add: _PrecompAdd;
  Funcs: PPrecompFuncs): size_t;
const
  MinSize = 64;
var
  Buffer: PByte;
  SI: _StrInfo1;
begin
  Result := 0;
  Buffer := Funcs^.Allocator(Instance, StreamInfo^.DSize);
  if not Assigned(ctx[Instance]) then
    DStorageCreateCompressionCodec(DSTORAGE_COMPRESSION_FORMAT_GDEFLATE, 1,
      IID_IDStorageCompressionCodec, ctx[Instance]);
  if not Failed(ctx[Instance].DecompressBuffer(Input + Pos, StreamInfo^.CSize,
    Buffer, StreamInfo^.DSize, @Result)) then
    if (Result > MinSize) and (Result > StreamInfo^.CSize) then
    begin
      Output(Instance, Buffer, Result);
      SI.Position := Pos;
      SI.OldSize := StreamInfo^.CSize;
      SI.Option := 0;
      SetBits(SI.Option, GDEFLATE_CODEC, 0, 3);
      SI.Status := TStreamStatus.None;
      SI.NewSize := Result;
      Funcs^.LogScan1(DStorageCodecs[GetBits(SI.Option, 0, 3)], SI.Position,
        SI.OldSize, SI.NewSize);
      Add(Instance, @SI, nil, nil);
    end;
end;

function GetDefLevel(Index: Integer): DSTORAGE_COMPRESSION;
begin
  case Index of
    1:
      Result := DSTORAGE_COMPRESSION_DEFAULT;
    2:
      Result := DSTORAGE_COMPRESSION_FASTEST;
    3:
      Result := DSTORAGE_COMPRESSION_BEST_RATIO;
  else
    Result := DSTORAGE_COMPRESSION_DEFAULT;
  end;
end;

function DStorageInit(Command: PChar; Count: Integer;
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
    CodecAvailable[X] := DStorage.DLLLoaded;
  X := 0;
  while Funcs^.GetCodec(Command, X, False) <> '' do
  begin
    S := Funcs^.GetCodec(Command, X, False);
    if (CompareText(S, DStorageCodecs[GDEFLATE_CODEC]) = 0) and DStorage.DLLLoaded
    then
    begin
      CodecEnabled[GDEFLATE_CODEC] := True;
      if Funcs^.GetParam(Command, X, 'l') <> '' then
        for I := Low(SOList) to High(SOList) do
          SOList[I][GDEFLATE_CODEC].Update
            ([StrToInt(Funcs^.GetParam(Command, X, 'l'))], True);
    end;
    Inc(X);
  end;
  if CodecAvailable[GDEFLATE_CODEC] then
  begin
    SetLength(ctx, Count);
    for X := Low(ctx) to High(ctx) do
      ctx[X] := nil;
  end;
  SetLength(Options, 0);
  for I := 1 to 3 do
    Insert(I, Options, Length(Options));
  for X := Low(SOList) to High(SOList) do
    for Y := Low(SOList[X]) to High(SOList[X]) do
      if SOList[X, Y].Count = 0 then
        SOList[X, Y].Update(Options);
end;

procedure DStorageFree(Funcs: PPrecompFuncs);
var
  X, Y: Integer;
begin
  for X := Low(SOList) to High(SOList) do
    for Y := Low(SOList[X]) to High(SOList[X]) do
      SOList[X, Y].Free;
end;

function DStorageParse(Command: PChar; Option: PInteger;
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
    if (CompareText(S, DStorageCodecs[GDEFLATE_CODEC]) = 0) and DStorage.DLLLoaded
    then
    begin
      SetBits(Option^, GDEFLATE_CODEC, 0, 3);
      if Funcs^.GetParam(Command, I, 'l') <> '' then
        SetBits(Option^, StrToInt(Funcs^.GetParam(Command, I, 'l')), 3, 2);
      Result := True;
    end;
    Inc(I);
  end;
end;

procedure DStorageScan1(Instance, Depth: Integer; Input: PByte;
  Size, SizeEx: NativeInt; Output: _PrecompOutput; Add: _PrecompAdd;
  Funcs: PPrecompFuncs);
var
  Buffer: PByte;
  Pos: NativeInt;
  X, Y, Z: Integer;
  SI: _StrInfo1;
  GDefSI: TGDefSI;
begin
  if BoolArray(CodecEnabled, False) then
    exit;
  Pos := 0;
  while Pos < Size do
  begin
    if PWord(Input + Pos)^ = GDefSig then
      if GetGDefSI(Input + Pos, SizeEx - Pos, @GDefSI) then
      begin
        try
          if GetGDefUS(Instance, Input, Pos, @GDefSI, Output, Add, Funcs) > 0
          then
          begin
            Inc(Pos, GDefSI.CSize);
            continue;
          end;
        except
        end;
      end;
    Inc(Pos);
  end;
end;

function DStorageScan2(Instance, Depth: Integer; Input: Pointer;
  Size: NativeInt; StreamInfo: PStrInfo2; Offset: PInteger;
  Output: _PrecompOutput; Funcs: PPrecompFuncs): Boolean;
var
  Buffer: PByte;
  B: Boolean;
  I: Integer;
  X: Integer;
  Res: size_t;
  GDefSI: TGDefSI;
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
  begin
    if not GetGDefSI(Input, Size, @GDefSI) then
      exit;
    StreamInfo^.OldSize := GDefSI.CSize;
  end;
  if StreamInfo^.NewSize > 0 then
  begin
    Buffer := Funcs^.Allocator(Instance, StreamInfo^.NewSize);
    if not Assigned(ctx[Instance]) then
      DStorageCreateCompressionCodec(DSTORAGE_COMPRESSION_FORMAT_GDEFLATE, 1,
        IID_IDStorageCompressionCodec, ctx[Instance]);
    if (Failed(ctx[Instance].DecompressBuffer(Input, StreamInfo^.OldSize,
      Buffer, StreamInfo^.NewSize, @Res)) = False) and
      (Res = StreamInfo^.NewSize) then
    begin
      Output(Instance, Buffer, Res);
      Result := True;
    end;
  end
  else if (StreamInfo^.NewSize <= 0) then
  begin
    Buffer := Funcs^.Allocator(Instance, GDefSI.DSize);
    if not Assigned(ctx[Instance]) then
      DStorageCreateCompressionCodec(DSTORAGE_COMPRESSION_FORMAT_GDEFLATE, 1,
        IID_IDStorageCompressionCodec, ctx[Instance]);
    if not Failed(ctx[Instance].DecompressBuffer(Input, StreamInfo^.OldSize,
      Buffer, GDefSI.DSize, @Res)) then
    begin
      Output(Instance, Buffer, Res);
      StreamInfo^.NewSize := Res;
      Funcs^.LogScan2(DStorageCodecs[GetBits(StreamInfo^.Option, 0, 3)],
        StreamInfo^.OldSize, StreamInfo^.NewSize);
      Result := True;
    end;
  end;
end;

function DStorageProcess(Instance, Depth: Integer; OldInput, NewInput: Pointer;
  StreamInfo: PStrInfo2; Output: _PrecompOutput; Funcs: PPrecompFuncs): Boolean;
var
  Buffer: PByte;
  Params: String;
  A, B: Integer;
  I: Integer;
  X: Integer;
  Res1: size_t;
  Res2: NativeUInt;
begin
  Result := False;
  X := GetBits(StreamInfo^.Option, 0, 3);
  if BoolArray(CodecAvailable, False) or (CodecAvailable[X] = False) then
    exit;
  if not Assigned(ctx[Instance]) then
    DStorageCreateCompressionCodec(DSTORAGE_COMPRESSION_FORMAT_GDEFLATE, 1,
      IID_IDStorageCompressionCodec, ctx[Instance]);
  A := ctx[Instance].CompressBufferBound(StreamInfo^.NewSize);
  Buffer := Funcs^.Allocator(Instance, A);
  SOList[Instance][X].Index := 0;
  while SOList[Instance][X].Get(I) >= 0 do
  begin
    if StreamInfo^.Status >= TStreamStatus.Predicted then
    begin
      if GetBits(StreamInfo^.Option, 3, 2) <> I then
        continue;
      if (StreamInfo^.Status = TStreamStatus.Database) then
      begin
        Res1 := StreamInfo^.OldSize;
        Result := True;
      end;
    end;
    Params := '';
    case X of
      GDEFLATE_CODEC:
        begin
          Params := 'l' + I.ToString;
          if not Result then
            if Failed(ctx[Instance].CompressBuffer(NewInput,
              StreamInfo^.NewSize, GetDefLevel(I), Buffer, A, @Res1)) then
              Res1 := 0;
        end;
    end;
    if not Result then
      Result := (Res1 = StreamInfo^.OldSize) and CompareMem(OldInput, Buffer,
        StreamInfo^.OldSize);
    Funcs^.LogProcess(DStorageCodecs[GetBits(StreamInfo^.Option, 0, 3)],
      PChar(Params), StreamInfo^.OldSize, StreamInfo^.NewSize, Res1, Result);
    if Result or (StreamInfo^.Status >= TStreamStatus.Predicted) then
      break;
  end;
  if Result then
  begin
    SetBits(StreamInfo^.Option, I, 3, 5);
    SOList[Instance][X].Add(I);
  end;
end;

function DStorageRestore(Instance, Depth: Integer; Input, InputExt: Pointer;
  StreamInfo: _StrInfo3; Output: _PrecompOutput; Funcs: PPrecompFuncs): Boolean;
var
  Buffer: PByte;
  Params: String;
  A: Integer;
  X: Integer;
  Res1: size_t;
  Res2: NativeUInt;
begin
  Result := False;
  X := GetBits(StreamInfo.Option, 0, 3);
  if BoolArray(CodecAvailable, False) or (CodecAvailable[X] = False) then
    exit;
  if not Assigned(ctx[Instance]) then
    DStorageCreateCompressionCodec(DSTORAGE_COMPRESSION_FORMAT_GDEFLATE, 1,
      IID_IDStorageCompressionCodec, ctx[Instance]);
  A := ctx[Instance].CompressBufferBound(StreamInfo.NewSize);
  Buffer := Funcs^.Allocator(Instance, A);
  Params := 'l' + GetBits(StreamInfo.Option, 3, 2).ToString;
  case X of
    GDEFLATE_CODEC:
      begin
        if not Result then
          if Failed(ctx[Instance].CompressBuffer(Input, StreamInfo.NewSize,
            GetDefLevel(GetBits(StreamInfo.Option, 3, 5)), Buffer, A, @Res1))
          then
            Res1 := 0;
      end;
  end;
  Funcs^.LogRestore(DStorageCodecs[GetBits(StreamInfo.Option, 0, 3)],
    PChar(Params), StreamInfo.OldSize, StreamInfo.NewSize, Res1, True);
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
for I := Low(DStorageCodecs) to High(DStorageCodecs) do
begin
  Codec.Names := Codec.Names + [DStorageCodecs[I]];
  StockMethods.Add(DStorageCodecs[I]);
end;
Codec.Initialised := False;
Codec.Init := @DStorageInit;
Codec.Free := @DStorageFree;
Codec.Parse := @DStorageParse;
Codec.Scan1 := @DStorageScan1;
Codec.Scan2 := @DStorageScan2;
Codec.Process := @DStorageProcess;
Codec.Restore := @DStorageRestore;
SetLength(CodecAvailable, Length(Codec.Names));
SetLength(CodecEnabled, Length(Codec.Names));

end.
