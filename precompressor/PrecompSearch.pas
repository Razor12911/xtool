unit PrecompSearch;

interface

uses
  Utils, SynCommons, SynCrypto,
  UIMain,
  PrecompUtils,
  WinAPI.Windows,
  System.SysUtils, System.Classes, System.StrUtils,
  System.Types, System.Math, System.IOUtils;

const
  XTOOL_DB = $31445458;

var
  Codec: TPrecompressor;

implementation

const
  MinSize = 65536;

type
  PEntryStruct = ^TEntryStruct;

  TEntryStruct = packed record
    Position: Int64;
    OldSize, NewSize, DepthSize: Integer;
  end;

  PSearchStruct = ^TSearchStruct;

  PHashStruct = ^THashStruct;

  THashStruct = record
    Size: Integer;
    Hash: Cardinal;
  end;

  TSearchStruct = record
    Name: String;
    SearchInt1, SearchInt2: Integer;
    Hash: Cardinal;
    HashList: TArray<THashStruct>;
    Codec: String;
    Resource: Integer;
    EntryList: TArray<TEntryStruct>;
  end;

var
  SearchList: TStringDynArray;
  SearchInfo: TArray<TArray<TArray<Integer>>>;
  SearchCount: TArray<TArray<Integer>>;
  CodecSearch: TArray<TArray<TSearchStruct>>;
  CodecAvailable, CodecEnabled: TArray<Boolean>;

function CheckHashList(Instance: Integer; Position: NativeInt;
  HashList: TArray<THashStruct>; Funcs: PPrecompFuncs): Boolean;
const
  BufferSize = 65536;
var
  Buffer: array [0 .. BufferSize - 1] of Byte;
  LPos: NativeInt;
  I: Integer;
  X, Y: Integer;
  CRC: Cardinal;
begin
  Result := False;
  LPos := Position;
  for I := Low(HashList) to High(HashList) do
  begin
    X := HashList[I].Size;
    CRC := 0;
    Y := Funcs^.ReadFuture(Instance, LPos, @Buffer[0], Min(X, BufferSize));
    while Y > 0 do
    begin
      Inc(LPos, Y);
      CRC := Utils.Hash32(CRC, @Buffer[0], Y);
      Dec(X, Y);
      Y := Funcs^.ReadFuture(Instance, LPos, @Buffer[0], Min(X, BufferSize));
    end;
    if (X > 0) or (CRC <> HashList[I].Hash) then
      break;
    if I = High(HashList) then
      Result := True;
  end;
end;

function SearchInit(Command: PChar; Count: Integer;
  Funcs: PPrecompFuncs): Boolean;
var
  I: Integer;
  W, X, Y, Z: Integer;
  S: String;
  List: System.Types.TStringDynArray;
begin
  Result := True;
  for X := Low(CodecAvailable) to High(CodecAvailable) do
  begin
    CodecAvailable[X] := True;
    CodecEnabled[X] := False;
  end;
  for X := Low(CodecSearch) to High(CodecSearch) do
    for Y := Low(CodecSearch[X]) to High(CodecSearch[X]) do
      CodecSearch[X, Y].Resource := RegisterResources(CodecSearch[X, Y].Codec);
  X := 0;
  while Funcs^.GetCodec(Command, X, False) <> '' do
  begin
    S := Funcs^.GetCodec(Command, X, False);
    for Y := Low(Codec.Names) to High(Codec.Names) do
      if CompareText(S, Codec.Names[Y]) = 0 then
      begin
        CodecEnabled[Y] := True;
        break;
      end;
    Inc(X);
  end;
  I := 0;
  for X := Low(CodecEnabled) to High(CodecEnabled) do
    if CodecEnabled[X] then
    begin
      for Y := Low(CodecSearch[X]) to High(CodecSearch[X]) do
      begin
        List := DecodeStr(CodecSearch[X, Y].Codec, SPrecompSep4);
        for W := Low(List) to High(List) do
          AddMethod(PrecompGetCodec(PChar(List[W]), 0, False));
      end;
      SetLength(SearchInfo[X], $10000);
      SetLength(SearchCount[X], $10000);
      for Z := Low(SearchInfo[X]) to High(SearchInfo[X]) do
      begin
        SearchCount[X, Z] := 0;
        for Y := Low(CodecSearch[X]) to High(CodecSearch[X]) do
        begin
          LongRec(I).Words[0] := LongRec(CodecSearch[X, Y].SearchInt1).Words[0];
          if Z = I then
          begin
            Inc(SearchCount[X, Z]);
            Insert(Y, SearchInfo[X, Z], Length(SearchInfo[X, Z]));
          end;
        end;
      end;
    end;
end;

procedure SearchFree(Funcs: PPrecompFuncs);
var
  X, Y, Z: Integer;
begin
  for X := Low(CodecEnabled) to High(CodecEnabled) do
    if CodecEnabled[X] then
    begin
      for Y := Low(SearchInfo[X]) to High(SearchInfo[X]) do
        SetLength(SearchInfo[X, Y], 0);
      SetLength(SearchInfo[X], 0);
      SetLength(SearchCount[X], 0);
    end;
end;

function SearchParse(Command: PChar; Option: PInteger;
  Funcs: PPrecompFuncs): Boolean;
begin
  Result := False;
end;

procedure SearchScan1(Instance, Depth: Integer; Input: PByte;
  Size, SizeEx: NativeInt; Output: _PrecompOutput; Add: _PrecompAdd;
  Funcs: PPrecompFuncs);
var
  I: Integer;
  J: Word;
  X, Y: Integer;
  Pos, LSize: NativeInt;
  SI: _StrInfo1;
  DI: TDepthInfo;
  DS: TPrecompStr;
  SS: PSearchStruct;
  CRC: Cardinal;
  Checked: Boolean;
begin
  if Depth > 0 then
    exit;
  for I := Low(CodecSearch) to High(CodecSearch) do
    if CodecEnabled[I] then
    begin
      Pos := 0;
      LSize := Size - Pred(Integer.Size);
      while Pos < LSize do
      begin
        J := PWord(Input + Pos)^;
        if (SearchCount[I, J] > 0) and (MinSize <= (SizeEx - Pos)) then
        begin
          Checked := False;
          for X := 0 to SearchCount[I, J] - 1 do
          begin
            if (PInteger(Input + Pos)^ = CodecSearch[I, SearchInfo[I, J, X]]
              .SearchInt1) and (PInteger(Input + Pos + MinSize - Integer.Size)
              ^ = CodecSearch[I, SearchInfo[I, J, X]].SearchInt2) then
            begin
              if not Checked then
              begin
                CRC := Utils.Hash32(0, Input + Pos, MinSize);
                Checked := True;
              end;
              if (CodecSearch[I, SearchInfo[I, J, X]].Hash = CRC) and
                CheckHashList(Instance, Pos, CodecSearch[I, SearchInfo[I, J, X]]
                .HashList, Funcs) then
              begin
                SS := @CodecSearch[I, SearchInfo[I, J, X]];
                Output(Instance, nil, 0);
                for Y := Low(SS^.EntryList) to High(SS^.EntryList) do
                begin
                  SI.Position := Pos + SS^.EntryList[Y].Position;
                  SI.OldSize := SS^.EntryList[Y].OldSize;
                  SI.NewSize := SS^.EntryList[Y].NewSize;
                  SI.Option := 0;
                  SI.Resource := SS^.Resource;
                  if System.Pos(SPrecompSep2 + 'l', SS^.Codec) > 0 then
                    SI.Status := TStreamStatus.Predicted
                  else
                    SI.Status := TStreamStatus.None;
                  DS := Funcs^.GetDepthCodec(PChar(SS^.Codec));
                  Move(DS[0], DI.Codec, SizeOf(DI.Codec));
                  DI.OldSize := SS^.EntryList[Y].NewSize;
                  DI.NewSize := SS^.EntryList[Y].DepthSize;
                  Add(Instance, @SI, PChar(SS^.Codec), @DI);
                end;
              end;
            end;
          end;
        end;
        Inc(Pos);
      end;
    end;
end;

function SearchScan2(Instance, Depth: Integer; Input: Pointer; Size: NativeInt;
  StreamInfo: PStrInfo2; Offset: PInteger; Output: _PrecompOutput;
  Funcs: PPrecompFuncs): Boolean;
begin
  Result := False;
end;

function SearchProcess(Instance, Depth: Integer; OldInput, NewInput: Pointer;
  StreamInfo: PStrInfo2; Output: _PrecompOutput; Funcs: PPrecompFuncs): Boolean;
begin
  Result := False;
end;

function SearchRestore(Instance, Depth: Integer; Input, InputExt: Pointer;
  StreamInfo: _StrInfo3; Output: _PrecompOutput; Funcs: PPrecompFuncs): Boolean;
begin
  Result := False;
end;

var
  I, J, K: Integer;
  S: String;
  Bytes: TBytes;
  FStream: TFileStream;
  I32: Integer;
  SearchStruct: PSearchStruct;
  HList: TArray<THashStruct>;

initialization

SearchList := TDirectory.GetFiles(ExtractFilePath(Utils.GetModuleName), '*.xtl',
  TSearchOption.soTopDirectoryOnly);
for I := Low(SearchList) to High(SearchList) do
begin
  FStream := TFileStream.Create(SearchList[I], fmShareDenyNone);
  try
    if FStream.Size >= 8 then
    begin
      FStream.ReadBuffer(I32, I32.Size);
      if (I32 = XTOOL_DB) then
      begin
        if FStream.Position < FStream.Size then
        begin
          J := Length(CodecSearch);
          SetLength(CodecSearch, Succ(J));
          S := ChangeFileExt(ExtractFileName(SearchList[I]), '');
          Insert(S, Codec.Names, Length(Codec.Names));
          if UIMain.DLLLoaded then
            XTLAddplugin(S, PLUGIN_DATABASE);
        end;
        while FStream.Position < FStream.Size do
        begin
          New(SearchStruct);
          SearchStruct^.Name := S;
          FStream.ReadBuffer(SearchStruct^.SearchInt1,
            SearchStruct^.SearchInt1.Size);
          FStream.ReadBuffer(SearchStruct^.SearchInt2,
            SearchStruct^.SearchInt2.Size);
          FStream.ReadBuffer(SearchStruct^.Hash, SearchStruct^.Hash.Size);
          FStream.ReadBuffer(I32, I32.Size);
          SetLength(HList, I32);
          FStream.ReadBuffer(HList[0], I32 * SizeOf(THashStruct));
          FStream.ReadBuffer(I32, I32.Size);
          SetLength(Bytes, I32);
          FStream.ReadBuffer(Bytes[0], I32);
          SearchStruct^.Codec := StringOf(Bytes);
          Insert(SearchStruct^, CodecSearch[J], Length(CodecSearch[J]));
          FStream.ReadBuffer(I32, I32.Size);
          K := Pred(Length(CodecSearch[J]));
          SetLength(CodecSearch[J, K].HashList, Length(HList));
          Move(HList[0], CodecSearch[J, K].HashList[0],
            Length(HList) * SizeOf(THashStruct));
          SetLength(CodecSearch[J, K].EntryList, I32);
          FStream.ReadBuffer(CodecSearch[J, K].EntryList[0],
            I32 * SizeOf(TEntryStruct));
        end;
      end;
    end;
  finally
    FStream.Free;
  end;
end;
Codec.Initialised := False;
Codec.Init := @SearchInit;
Codec.Free := @SearchFree;
Codec.Parse := @SearchParse;
Codec.Scan1 := @SearchScan1;
Codec.Scan2 := @SearchScan2;
Codec.Process := @SearchProcess;
Codec.Restore := @SearchRestore;
SetLength(SearchInfo, Length(CodecSearch));
SetLength(SearchCount, Length(CodecSearch));
SetLength(CodecAvailable, Length(CodecSearch));
SetLength(CodecEnabled, Length(CodecSearch));

end.
