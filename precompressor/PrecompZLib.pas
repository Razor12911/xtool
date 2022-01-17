unit PrecompZLib;

interface

uses
  ZLibDLL, ReflateDLL, PreflateDLL, GrittibanzliDLL,
  Utils,
  PrecompUtils,
  System.SysUtils, System.StrUtils, System.Classes, System.Math;

var
  Codec: TPrecompressor;

implementation

const
  ZlibCodecs: array of PChar = ['zlib', 'reflate', 'preflate', 'grittibanzli'];
  CODEC_COUNT = 4;
  ZLIB_CODEC = 0;
  REFLATE_CODEC = 1;
  PREFLATE_CODEC = 2;
  GRITTIBANZLI_CODEC = 3;

const
  Z_WINBITS = 7;
  Z_SCANBYTES = 16;
  Z_WORKMEM = 65536;
  Z_MINSIZE = 128;
  Z_BLKSIZE = 512;
  R_LEVEL = 6;
  R_WORKMEM = 65536;
  P_HIFSIZE = 1048576;
  G_HIFSIZE = 8338608;

var
  SOList: array of array [0 .. CODEC_COUNT - 1] of TSOList;
  ZStream1: array of array [1 .. 9, 1 .. 9, 1 .. 7] of z_stream;
  ZStream2: array of array [0 .. 7] of z_stream;
  ZWinBits: Integer = Z_WINBITS;
  RefInst1, RefInst2: array of Pointer;
  CodecAvailable, CodecEnabled: TArray<Boolean>;
  Storage: TArray<TMemoryStream>;

function ZlibInit(Command: PChar; Count: Integer; Funcs: PPrecompFuncs)
  : Boolean;
var
  I: Integer;
  Options: TArray<Integer>;
  S: String;
  W, X, Y, Z: Integer;
begin
  Result := True;
  SetLength(SOList, Count);
  for X := Low(SOList) to High(SOList) do
    for Y := Low(SOList[X]) to High(SOList[X]) do
      SOList[X, Y] := TSOList.Create([], TSOMethod.MTF);
  SetLength(Storage, Count);
  for X := Low(Storage) to High(Storage) do
    Storage[X] := TMemoryStream.Create;
  for X := Low(CodecAvailable) to High(CodecAvailable) do
  begin
    CodecAvailable[X] := False;
    CodecEnabled[X] := False;
  end;
  CodecAvailable[ZLIB_CODEC] := ZLibDLL.DLLLoaded;
  CodecAvailable[REFLATE_CODEC] := ReflateDLL.DLLLoaded;
  CodecAvailable[PREFLATE_CODEC] := PreflateDLL.DLLLoaded;
  CodecAvailable[GRITTIBANZLI_CODEC] := GrittibanzliDLL.DLLLoaded;
  X := 0;
  while Funcs^.GetCodec(Command, X, False) <> '' do
  begin
    S := Funcs^.GetCodec(Command, X, False);
    if (CompareText(S, ZlibCodecs[ZLIB_CODEC]) = 0) and ZLibDLL.DLLLoaded then
    begin
      CodecEnabled[ZLIB_CODEC] := True;
      if Funcs^.GetParam(Command, X, 'l') <> '' then
        for I := Low(SOList) to High(SOList) do
          SOList[I][ZLIB_CODEC].Update
            ([StrToInt(Funcs^.GetParam(Command, X, 'l'))], True);
      if Funcs^.GetParam(Command, X, 'w') <> '' then
        ZWinBits := EnsureRange(StrToInt(Funcs^.GetParam(Command, X, 'w'))
          - 8, 1, 7);
    end
    else if (CompareText(S, ZlibCodecs[REFLATE_CODEC]) = 0) and ReflateDLL.DLLLoaded
    then
    begin
      CodecEnabled[REFLATE_CODEC] := True;
      if Funcs^.GetParam(Command, X, 'l') <> '' then
        for I := Low(SOList) to High(SOList) do
          SOList[I][REFLATE_CODEC].Update
            ([StrToInt(Funcs^.GetParam(Command, X, 'l'))], True);
    end
    else if (CompareText(S, ZlibCodecs[PREFLATE_CODEC]) = 0) and PreflateDLL.DLLLoaded
    then
      CodecEnabled[PREFLATE_CODEC] := True
    else if (CompareText(S, ZlibCodecs[GRITTIBANZLI_CODEC]) = 0) and
      GrittibanzliDLL.DLLLoaded then
      CodecEnabled[GRITTIBANZLI_CODEC] := True;
    Inc(X);
  end;
  if CodecAvailable[ZLIB_CODEC] then
  begin
    SetLength(ZStream1, Count);
    for W := Low(ZStream1) to High(ZStream1) do
      for X := Low(ZStream1[W]) to High(ZStream1[W]) do
        for Y := Low(ZStream1[W, X]) to High(ZStream1[W, X]) do
          for Z := Low(ZStream1[W, X, Y]) to High(ZStream1[W, X, Y]) do
          begin
            FillChar(ZStream1[W, X, Y, Z], SizeOf(z_stream), 0);
            deflateInit2(ZStream1[W, X, Y, Z], X, Z_DEFLATED, -(Z + 8), Y,
              Z_DEFAULT_STRATEGY);
          end;
  end;
  if CodecAvailable[REFLATE_CODEC] then
  begin
    SetLength(RefInst1, Count);
    SetLength(RefInst2, Count);
    for X := Low(RefInst1) to High(RefInst1) do
    begin
      RefInst1[X] := raw2hif_Alloc;
      RefInst2[X] := hif2raw_Alloc;
    end;
  end;
  if not BoolArray(CodecAvailable, False) then
  begin
    SetLength(ZStream2, Count);
    for X := Low(ZStream2) to High(ZStream2) do
      for Y := Low(ZStream2[X]) to High(ZStream2[X]) do
      begin
        FillChar(ZStream2[X, Y], SizeOf(z_stream), 0);
        inflateInit2(ZStream2[X, Y], -(Y + 8));
      end;
    for X := Low(SOList) to High(SOList) do
      for Y := Low(SOList[X]) to High(SOList[X]) do
      begin
        SetLength(Options, 0);
        case Y of
          ZLIB_CODEC:
            for I := 11 to 99 do
              if I mod 10 <> 0 then
                Insert(I, Options, Length(Options));
          REFLATE_CODEC:
            Options := [R_LEVEL];
        end;
        if SOList[X, Y].Count = 0 then
          SOList[X, Y].Update(Options);
      end;
  end
  else
    Result := False;
end;

procedure ZlibFree(Funcs: PPrecompFuncs);
var
  W, X, Y, Z: Integer;
begin
  for X := Low(SOList) to High(SOList) do
    for Y := Low(SOList[X]) to High(SOList[X]) do
      SOList[X, Y].Free;
  for X := Low(Storage) to High(Storage) do
    Storage[X].Free;
  if CodecAvailable[ZLIB_CODEC] then
  begin
    for W := Low(ZStream1) to High(ZStream1) do
      for X := Low(ZStream1[W]) to High(ZStream1[W]) do
        for Y := Low(ZStream1[W, X]) to High(ZStream1[W, X]) do
          for Z := Low(ZStream1[W, X, Y]) to High(ZStream1[W, X, Y]) do
            deflateEnd(ZStream1[W, X, Y, Z]);
  end;
  if CodecAvailable[REFLATE_CODEC] then
  begin
    for X := Low(RefInst1) to High(RefInst1) do
    begin
      raw2hif_Free(RefInst1[X]);
      hif2raw_Free(RefInst2[X]);
    end;
  end;
  if not BoolArray(CodecAvailable, False) then
  begin
    for X := Low(ZStream2) to High(ZStream2) do
      for Y := Low(ZStream2[X]) to High(ZStream2[X]) do
        inflateEnd(ZStream2[X, Y]);
  end;
end;

function ZlibParse(Command: PChar; Option: PInteger;
  Funcs: PPrecompFuncs): Boolean;
var
  S: String;
  I: Integer;
begin
  Result := False;
  Option^ := 0;
  SetBits(Option^, ZWinBits, 12, 3);
  I := 0;
  while Funcs^.GetCodec(Command, I, False) <> '' do
  begin
    S := Funcs^.GetCodec(Command, I, False);
    if (CompareText(S, ZlibCodecs[ZLIB_CODEC]) = 0) and ZLibDLL.DLLLoaded then
    begin
      SetBits(Option^, 0, 0, 5);
      if Funcs^.GetParam(Command, I, 'l') <> '' then
        SetBits(Option^, StrToInt(Funcs^.GetParam(Command, I, 'l')), 5, 7);
      if Funcs^.GetParam(Command, I, 'w') <> '' then
        SetBits(Option^, EnsureRange(StrToInt(Funcs^.GetParam(Command, I, 'w'))
          - 8, 1, 7), 12, 3);
      Result := True;
    end
    else if (CompareText(S, ZlibCodecs[REFLATE_CODEC]) = 0) and ReflateDLL.DLLLoaded
    then
    begin
      SetBits(Option^, 1, 0, 5);
      if Funcs^.GetParam(Command, I, 'l') <> '' then
        SetBits(Option^, StrToInt(Funcs^.GetParam(Command, I, 'l')), 5, 7);
      Result := True;
    end
    else if (CompareText(S, ZlibCodecs[PREFLATE_CODEC]) = 0) and PreflateDLL.DLLLoaded
    then
    begin
      SetBits(Option^, 2, 0, 5);
      Result := True;
    end
    else if (CompareText(S, ZlibCodecs[GRITTIBANZLI_CODEC]) = 0) and
      GrittibanzliDLL.DLLLoaded then
    begin
      SetBits(Option^, 3, 0, 5);
      Result := True;
    end;
    Inc(I);
  end;
end;

procedure ZlibScan1(Instance, Depth: Integer; Input: PByte;
  Size, SizeEx: NativeInt; Output: _PrecompOutput; Add: _PrecompAdd;
  Funcs: PPrecompFuncs);
var
  Buffer: PByte;
  Pos: NativeInt;
  Res: Integer;
  I: Integer;
  ZStream: z_streamp;
  IsZlib: Boolean;
  Level: Integer;
  WinBits: Byte;
  ScanBytes: Integer;
  SI: _StrInfo1;
  DI: TDepthInfo;
  DS: TPrecompCmd;
  LastIn, LastOut: cardinal;
begin
  if BoolArray(CodecEnabled, False) then
    exit;
  DI := Funcs^.GetDepthInfo(Instance);
  DS := Funcs^.GetCodec(DI.Codec, 0, False);
  if DS <> '' then
    if IndexTextW(@DS[0], ZlibCodecs) < 0 then
      exit;
  Pos := 0;
  Buffer := Funcs^.Allocator(Instance, Z_WORKMEM);
  IsZlib := False;
  while Pos < Size do
  begin
    Res := PInteger(Input + Pos)^;
    for I := 1 to 3 do
    begin
      if LongRec(Res).Bytes[0] <> LongRec(Res).Bytes[I] then
        break;
    end;
    if (I = 3) or (LongRec(Res).Lo = LongRec(Res).Hi) then
    begin
      Inc(Pos);
      continue;
    end;
    if (Pos >= 2) and ((Input + Pos - 2)^ and $F = 8) and
      ((Input + Pos - 1)^ and $20 = 0) and
      (EndianSwap(PWord(Input + Pos - 2)^) mod $1F = 0) then
    begin
      WinBits := (Input + Pos - 2)^ shr 4;
      if WinBits in [0 .. 7] then
      begin
        ZStream := @ZStream2[Instance, WinBits];
        Level := (Input + Pos - 1)^ shr $6;
        IsZlib := True;
        ScanBytes := Z_MINSIZE;
      end;
    end;
    IsZlib := False;
    if IsZlib or ((Input + Pos)^ and 7 in [$4, $5]) then
    begin
      if not IsZlib then
      begin
        WinBits := ZWinBits;
        ZStream := @ZStream2[Instance, WinBits];
        Level := -1;
      end;
      if WinBits = 7 then
        ScanBytes := Z_SCANBYTES;
      IsZlib := False;
      LastIn := 0;
      LastOut := 0;
      ZStream^.next_in := (Input + Pos);
      ZStream^.avail_in := ScanBytes;
      ZStream^.next_out := Buffer;
      ZStream^.avail_out := Z_WORKMEM;
      inflateReset(ZStream^);
      Res := inflate(ZStream^, Z_SYNC_FLUSH);
      if (Res in [Z_OK, Z_STREAM_END]) and (ZStream^.total_in = ScanBytes) then
      begin
        Output(Instance, nil, 0);
        I := Z_WORKMEM - ZStream^.avail_out;
        Output(Instance, Buffer, I);
        ZStream^.avail_in := (SizeEx - Pos) - Z_SCANBYTES;
        while Res <> Z_STREAM_END do
        begin
          ZStream^.next_out := Buffer;
          ZStream^.avail_out := Z_WORKMEM;
          Res := inflate(ZStream^, Z_BLOCK);
          if not(Res in [Z_OK, Z_STREAM_END]) then
          begin
            if (LastIn >= Z_MINSIZE) then
              Res := Z_STREAM_END;
            break;
          end;
          LastIn := ZStream^.total_in;
          LastOut := ZStream^.total_out;
          I := Z_WORKMEM - ZStream^.avail_out;
          Output(Instance, Buffer, I);
        end;
        if (Res = Z_STREAM_END) { and (ZStream^.total_out > ZStream^.total_in) }
        then
        begin
          SI.Position := Pos;
          SI.OldSize := LastIn;
          SI.NewSize := LastOut;
          SI.Option := 0;
          if Level >= 0 then
          begin
            case Level of
              0:
                SetBits(SI.Option, 1, 5, 7);
              1:
                SetBits(SI.Option, 5, 5, 7);
              2:
                SetBits(SI.Option, 6, 5, 7);
              3:
                SetBits(SI.Option, 9, 5, 7);
            end;
            SI.Status := TStreamStatus.Predicted;
          end
          else
            SI.Status := TStreamStatus.None;
          SetBits(SI.Option, WinBits, 12, 3);
          for I := Low(CodecEnabled) to High(CodecEnabled) do
          begin
            if (I = ZLIB_CODEC) and (WinBits = 0) then
              SetBits(SI.Option, 1, 12, 3);
            SetBits(SI.Option, I, 0, 5);
            if CodecEnabled[I] then
            begin
              Add(Instance, @SI, nil, nil);
              break;
            end;
          end;
          Inc(Pos, SI.OldSize);
          continue;
        end
        else
          Output(Instance, nil, 0);
      end;
    end;
    Inc(Pos);
  end;
end;

function ZLibScan2(Instance, Depth: Integer; Input: Pointer; Size: NativeInt;
  StreamInfo: PStrInfo2; Output: _PrecompOutput; Funcs: PPrecompFuncs): Boolean;
var
  Buffer: PByte;
  Res: Integer;
  ZStream: z_streamp;
begin
  Result := False;
  if StreamInfo^.NewSize <= 0 then
    exit;
  Buffer := Funcs^.Allocator(Instance, StreamInfo^.NewSize);
  ZStream := @ZStream2[Instance, GetBits(StreamInfo^.Option, 12, 3)];
  ZStream^.next_in := Input;
  ZStream^.avail_in := StreamInfo^.OldSize;
  ZStream^.next_out := Buffer;
  ZStream^.avail_out := StreamInfo^.NewSize;
  inflateReset(ZStream^);
  Res := inflate(ZStream^, Z_FULL_FLUSH);
  if (Res = Z_STREAM_END) and (ZStream^.total_out = StreamInfo^.NewSize) then
  begin
    Output(Instance, Buffer, ZStream^.total_out);
    Result := True;
  end;
end;

function ZlibProcess(Instance, Depth: Integer; OldInput, NewInput: Pointer;
  StreamInfo: PStrInfo2; Output: _PrecompOutput; Funcs: PPrecompFuncs): Boolean;

  function IsValidLevel(CLevel, ZLevel: Integer): Boolean;
  begin
    case CLevel of
      1, 6:
        if CLevel = ZLevel then
          Result := True;
      2 .. 5:
        if ZLevel = 5 then
          Result := True;
      7 .. 9:
        if ZLevel = 9 then
          Result := True;
    else
      Result := False;
    end;
  end;

var
  Buffer, Ptr: PByte;
  Res1, Res2: Integer;
  L, M: Integer;
  I, J: Integer;
  X: Integer;
  ZStream: z_streamp;
  HR: Pointer;
  Verified: Boolean;
  CRC: cardinal;
begin
  Result := False;
  X := GetBits(StreamInfo^.Option, 0, 5);
  if BoolArray(CodecAvailable, False) or (CodecAvailable[X] = False) then
    exit;
  case X of
    ZLIB_CODEC:
      begin
        Buffer := Funcs^.Allocator(Instance, Z_WORKMEM);
        SOList[Instance][ZLIB_CODEC].Index := 0;
        while SOList[Instance][ZLIB_CODEC].Get(I) >= 0 do
        begin
          L := I div 10;
          M := I mod 10;
          if StreamInfo^.Status = TStreamStatus.Predicted then
          begin
            if InRange(GetBits(StreamInfo^.Option, 5, 7), 1, 9) then
            begin
              if not IsValidLevel(L, GetBits(StreamInfo^.Option, 5, 7)) then
                continue;
            end
            else
            begin
              if GetBits(StreamInfo^.Option, 5, 7) <> I then
                continue;
              { I := GetBits(StreamInfo^.Option, 5, 7);
                SOList[Instance][ZLIB_CODEC].Add(I);
                Result := True;
                break; }
            end;
          end;
          ZStream := @ZStream1[Instance, L, M,
            GetBits(StreamInfo^.Option, 12, 3)];
          ZStream^.next_in := NewInput;
          ZStream^.avail_in := StreamInfo^.NewSize;
          deflateReset(ZStream^);
          repeat
            ZStream^.next_out := Buffer;
            ZStream^.avail_out := Z_BLKSIZE;
            Res1 := deflate(ZStream^, Z_FINISH);
            if Res1 < 0 then
              raise EZCompressionError.Create(string(_z_errmsg[2 - Res1]))
                at ReturnAddress;
            Res2 := Z_BLKSIZE - ZStream^.avail_out;
            Verified := CompareMem(PByte(OldInput) + ZStream^.total_out - Res2,
              Buffer, Res2);
            if not Verified then
              break;
          until (ZStream^.avail_in = 0) and (ZStream^.avail_out > 0);
          if Verified and (Res1 = Z_STREAM_END) then
          begin
            SetBits(StreamInfo^.Option, I, 5, 7);
            SOList[Instance][ZLIB_CODEC].Add(I);
            Result := True;
            break;
          end;
        end;
        if Result = False then
        begin
          if CodecEnabled[REFLATE_CODEC] or CodecEnabled[PREFLATE_CODEC] or
            CodecEnabled[GRITTIBANZLI_CODEC] then
          begin
            if CodecEnabled[REFLATE_CODEC] then
              SetBits(StreamInfo^.Option, REFLATE_CODEC, 0, 5)
            else if CodecEnabled[PREFLATE_CODEC] then
              SetBits(StreamInfo^.Option, PREFLATE_CODEC, 0, 5)
            else
              SetBits(StreamInfo^.Option, GRITTIBANZLI_CODEC, 0, 5);
            Result := ZlibProcess(Instance, Depth, OldInput, NewInput,
              StreamInfo, Output, Funcs);
          end;
        end;
      end;
    REFLATE_CODEC:
      begin
        Buffer := Funcs^.Allocator(Instance, R_WORKMEM * 2);
        Storage[Instance].Position := 0;
        HR := RefInst1[Instance];
        if StreamInfo^.Status = TStreamStatus.Predicted then
          L := GetBits(StreamInfo^.Option, 5, 7)
        else
          L := R_LEVEL;
        if L > 9 then
          L := L div 10;
        L := EnsureRange(L, 1, 9);
        M := 0;
        I := 0;
        raw2hif_Init(HR, L);
        while True do
        begin
          Res1 := raw2hif_Loop(HR);
          if (Res1 in [0, 2]) or (Res1 > 3) then
          begin
            Res2 := raw2hif_getoutlen(HR);
            Output(Instance, Buffer, Res2);
            Storage[Instance].WriteBuffer(Buffer^, Res2);
            raw2hif_addbuf(HR, Buffer, R_WORKMEM);
          end;
          if (Res1 = 3) or (Res1 = 0) then
          begin
            Res2 := raw2hif_getou2len(HR);
            Inc(M, Res2);
            raw2hif_addbuf(HR, Buffer + R_WORKMEM, R_WORKMEM);
            if Res1 = 0 then
              break;
          end;
          if (Res1 = 1) then
          begin
            Res2 := Min(StreamInfo^.OldSize - I, R_WORKMEM);
            raw2hif_addbuf(HR, PByte(OldInput) + I, Res2);
            Inc(I, Res2);
          end;
        end;
        if M = StreamInfo^.NewSize then
        begin
          { HR := RefInst2[Instance];
            I := 0;
            J := 0;
            M := 0;
            CRC := 0;
            M := Storage[Instance].Position;
            Ptr := Storage[Instance].Memory;
            hif2raw_Init(HR, L);
            while True do
            begin
            Res1 := hif2raw_Loop(HR);
            if (Res1 in [0, 2]) or (Res1 > 3) then
            begin
            Res2 := hif2raw_getoutlen(HR);
            if Res2 > 0 then
            CRC := Hash32(CRC, Buffer, Res2);
            hif2raw_addbuf(HR, Buffer, R_WORKMEM);
            if Res1 = 0 then
            break;
            end;
            if Res1 = 1 then
            begin
            Res2 := Min(M - J, R_WORKMEM);
            hif2raw_addbuf(HR, Ptr + J, Res2);
            Inc(J, Res2);
            end;
            if Res1 = 3 then
            begin
            Res2 := Min(StreamInfo^.NewSize - I, R_WORKMEM);
            hif2raw_addbuf(HR, PByte(NewInput) + I, Res2);
            Inc(I, Res2);
            end;
            end;
            if CRC = Hash32(0, OldInput, StreamInfo^.OldSize) then
            begin }
          SetBits(StreamInfo^.Option, L, 5, 7);
          Result := True;
          // end;
        end;
      end;
    PREFLATE_CODEC:
      begin
        Res1 := StreamInfo^.NewSize;
        Res2 := P_HIFSIZE;
        Buffer := Funcs^.Allocator(Instance, Res2);
        if preflate_decode(OldInput, StreamInfo^.OldSize, NewInput, @Res1,
          Buffer, @Res2) then
        begin
          Output(Instance, Buffer, Res2);
          Result := True;
        end;
      end;
    GRITTIBANZLI_CODEC:
      begin
        Res1 := StreamInfo^.NewSize;
        Res2 := G_HIFSIZE;
        Buffer := Funcs^.Allocator(Instance, Res2);
        if Grittibanzli(OldInput, StreamInfo^.OldSize, NewInput, @Res1, Buffer,
          @Res2) then
        begin
          Output(Instance, Buffer, Res2);
          Result := True;
        end;
      end;
  end;
end;

function ZlibRestore(Instance, Depth: Integer; Input, InputExt: Pointer;
  StreamInfo: _StrInfo3; Output: _PrecompOutput; Funcs: PPrecompFuncs): Boolean;
var
  Buffer: PByte;
  Res1, Res2: Integer;
  L, M: Integer;
  I, J: Integer;
  X: Integer;
  ZStream: z_streamp;
  HR: Pointer;
begin
  Result := False;
  X := GetBits(StreamInfo.Option, 0, 5);
  if BoolArray(CodecAvailable, False) or (CodecAvailable[X] = False) then
    exit;
  case X of
    ZLIB_CODEC:
      begin
        Buffer := Funcs^.Allocator(Instance, Z_WORKMEM);
        L := GetBits(StreamInfo.Option, 5, 7) div 10;
        M := GetBits(StreamInfo.Option, 5, 7) mod 10;
        ZStream := @ZStream1[Instance, L, M, GetBits(StreamInfo.Option, 12, 3)];
        ZStream^.next_in := Input;
        ZStream^.avail_in := StreamInfo.NewSize;
        deflateReset(ZStream^);
        repeat
          ZStream^.next_out := Buffer;
          ZStream^.avail_out := Z_WORKMEM;
          Res1 := deflate(ZStream^, Z_FINISH);
          if Res1 < 0 then
            raise EZCompressionError.Create(string(_z_errmsg[2 - Res1]))
              at ReturnAddress;
          Res2 := Z_WORKMEM - ZStream^.avail_out;
          Output(Instance, Buffer, Res2);
        until (ZStream^.avail_in = 0) and (ZStream^.avail_out > 0);
        Result := True;
      end;
    REFLATE_CODEC:
      begin
        Buffer := Funcs^.Allocator(Instance, R_WORKMEM);
        HR := RefInst2[Instance];
        I := 0;
        J := 0;
        hif2raw_Init(HR, GetBits(StreamInfo.Option, 5, 7));
        while True do
        begin
          Res1 := hif2raw_Loop(HR);
          if (Res1 in [0, 2]) or (Res1 > 3) then
          begin
            Res2 := hif2raw_getoutlen(HR);
            Output(Instance, Buffer, Res2);
            hif2raw_addbuf(HR, Buffer, R_WORKMEM);
            if Res1 = 0 then
              break;
          end;
          if Res1 = 1 then
          begin
            Res2 := Min(StreamInfo.ExtSize - J, R_WORKMEM);
            hif2raw_addbuf(HR, PByte(InputExt) + J, Res2);
            Inc(J, Res2);
          end;
          if Res1 = 3 then
          begin
            Res2 := Min(StreamInfo.NewSize - I, R_WORKMEM);
            hif2raw_addbuf(HR, PByte(Input) + I, Res2);
            Inc(I, Res2);
          end;
        end;
        Result := True;
      end;
    PREFLATE_CODEC:
      begin
        Res1 := StreamInfo.OldSize;
        Buffer := Funcs^.Allocator(Instance, Res1);
        if preflate_reencode(Input, StreamInfo.NewSize, InputExt,
          StreamInfo.ExtSize, Buffer, @Res1) then
        begin
          Output(Instance, Buffer, Res1);
          Result := True;
        end;
      end;
    GRITTIBANZLI_CODEC:
      begin
        Res1 := StreamInfo.OldSize;
        Buffer := Funcs^.Allocator(Instance, Res1);
        if Ungrittibanzli(Input, StreamInfo.NewSize, InputExt,
          StreamInfo.ExtSize, Buffer, @Res1) then
        begin
          Output(Instance, Buffer, Res1);
          Result := True;
        end;
      end;
  end;
end;

var
  I: Integer;

initialization

Codec.Names := [];
for I := Low(ZlibCodecs) to High(ZlibCodecs) do
begin
  Codec.Names := Codec.Names + [ZlibCodecs[I]];
  StockMethods.Add(ZlibCodecs[I]);
end;
Codec.Initialised := False;
Codec.Init := @ZlibInit;
Codec.Free := @ZlibFree;
Codec.Parse := @ZlibParse;
Codec.Scan1 := @ZlibScan1;
Codec.Scan2 := @ZLibScan2;
Codec.Process := @ZlibProcess;
Codec.Restore := @ZlibRestore;
SetLength(CodecAvailable, Length(Codec.Names));
SetLength(CodecEnabled, Length(Codec.Names));

end.
