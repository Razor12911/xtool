unit PrecompCrypto;

interface

uses
  Utils,
  PrecompUtils,
  System.SysUtils, System.Classes, System.Math;

var
  Codec: TPrecompressor;

implementation

const
  CryptoCodecs: array of PChar = ['xor', 'aes', 'rc4'];
  CODEC_COUNT = 3;
  XOR_CODEC = 0;
  AES_CODEC = 1;
  RC4_CODEC = 2;

function CryptoInit(Command: PChar; Count: Integer;
  Funcs: PPrecompFuncs): Boolean;
begin
  Result := True;
end;

procedure CryptoFree(Funcs: PPrecompFuncs);
begin
end;

function CryptoParse(Command: PChar; Option: PInteger;
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
    if (CompareText(S, CryptoCodecs[XOR_CODEC]) = 0) then
    begin
      SetBits(Option^, 0, 0, 5);
      Result := True;
    end
    else if (CompareText(S, CryptoCodecs[AES_CODEC]) = 0) then
    begin
      SetBits(Option^, 1, 0, 5);
      Result := True;
    end
    else if (CompareText(S, CryptoCodecs[RC4_CODEC]) = 0) then
    begin
      SetBits(Option^, 2, 0, 5);
      Result := True;
    end;
    Inc(I);
  end;
end;

procedure CryptoScan1(Instance, Depth: Integer; Input: PByte;
  Size, SizeEx: NativeInt; Output: _PrecompOutput; Add: _PrecompAdd;
  Funcs: PPrecompFuncs);
var
  X: Integer;
  SI: _StrInfo1;
  DI1, DI2: TDepthInfo;
  DS: TPrecompStr;
begin
  DI1 := Funcs^.GetDepthInfo(Instance);
  DS := Funcs^.GetCodec(DI1.Codec, 0, False);
  if DS <> '' then
  begin
    X := IndexTextW(@DS[0], CryptoCodecs);
    if (X < 0) or (DI1.OldSize <> SizeEx) then
      exit;
    Output(Instance, Input, DI1.OldSize);
    SI.Position := 0;
    SI.OldSize := DI1.OldSize;
    SI.NewSize := DI1.NewSize;
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
    Funcs^.LogScan1(CryptoCodecs[GetBits(SI.Option, 0, 5)], SI.Position,
      SI.OldSize, SI.NewSize);
    Add(Instance, @SI, DI1.Codec, @DI2);
  end;
end;

function CryptoScan2(Instance, Depth: Integer; Input: Pointer; Size: NativeInt;
  StreamInfo: PStrInfo2; Offset: PInteger; Output: _PrecompOutput;
  Funcs: PPrecompFuncs): Boolean;
var
  Res: Integer;
begin
  Result := False;
  Res := 0;
  if not Funcs^.GetResource(StreamInfo^.Resource, nil, @Res) then
    exit;
  if (Res > 0) and (StreamInfo^.OldSize > 0) then
  begin
    StreamInfo^.NewSize := StreamInfo^.OldSize;
    Output(Instance, Input, StreamInfo^.OldSize);
    Funcs^.LogScan2(CryptoCodecs[GetBits(StreamInfo^.Option, 0, 5)],
      StreamInfo^.OldSize, StreamInfo^.NewSize);
    Result := True;
  end;
end;

function CryptoProcess(Instance, Depth: Integer; OldInput, NewInput: Pointer;
  StreamInfo: PStrInfo2; Output: _PrecompOutput; Funcs: PPrecompFuncs): Boolean;
var
  Buffer: PByte;
  X: Integer;
  Res: Integer;
begin
  Result := False;
  X := GetBits(StreamInfo^.Option, 0, 5);
  Res := 0;
  if not Funcs^.GetResource(StreamInfo^.Resource, nil, @Res) then
    exit;
  Buffer := Funcs^.Allocator(Instance, Res);
  if Funcs^.GetResource(StreamInfo^.Resource, Buffer, @Res) then
  begin
    case X of
      XOR_CODEC:
        Funcs^.Decrypt('xor', NewInput, StreamInfo^.NewSize, Buffer, Res);
      AES_CODEC:
        Funcs^.Decrypt('aes', NewInput, StreamInfo^.NewSize, Buffer, Res);
      RC4_CODEC:
        Funcs^.Decrypt('rc4', NewInput, StreamInfo^.NewSize, Buffer, Res);
    else
      exit;
    end;
    Result := True;
    Funcs^.LogProcess(CryptoCodecs[GetBits(StreamInfo^.Option, 0, 5)], nil,
      StreamInfo^.OldSize, StreamInfo^.NewSize, StreamInfo^.OldSize, Result);
  end;
end;

function CryptoRestore(Instance, Depth: Integer; Input, InputExt: Pointer;
  StreamInfo: _StrInfo3; Output: _PrecompOutput; Funcs: PPrecompFuncs): Boolean;
var
  Buffer: PByte;
  X: Integer;
  Res: Integer;
begin
  Result := False;
  X := GetBits(StreamInfo.Option, 0, 5);
  Res := 0;
  if not Funcs^.GetResource(StreamInfo.Resource, nil, @Res) then
    exit;
  Buffer := Funcs^.Allocator(Instance, Res);
  if Funcs^.GetResource(StreamInfo.Resource, Buffer, @Res) then
  begin
    case X of
      XOR_CODEC:
        Funcs^.Encrypt('xor', Input, StreamInfo.NewSize, Buffer, Res);
      AES_CODEC:
        Funcs^.Encrypt('aes', Input, StreamInfo.NewSize, Buffer, Res);
      RC4_CODEC:
        Funcs^.Encrypt('rc4', Input, StreamInfo.NewSize, Buffer, Res);
    else
      exit;
    end;
    Output(Instance, Input, StreamInfo.OldSize);
    Result := True;
    Funcs^.LogRestore(CryptoCodecs[GetBits(StreamInfo.Option, 0, 5)], nil,
      StreamInfo.OldSize, StreamInfo.NewSize, StreamInfo.OldSize, Result);
  end;
end;

var
  I: Integer;

initialization

Codec.Names := [];
for I := Low(CryptoCodecs) to High(CryptoCodecs) do
begin
  Codec.Names := Codec.Names + [CryptoCodecs[I]];
  StockMethods.Add(CryptoCodecs[I]);
end;
Codec.Initialised := False;
Codec.Init := @CryptoInit;
Codec.Free := @CryptoFree;
Codec.Parse := @CryptoParse;
Codec.Scan1 := @CryptoScan1;
Codec.Scan2 := @CryptoScan2;
Codec.Process := @CryptoProcess;
Codec.Restore := @CryptoRestore;

end.
