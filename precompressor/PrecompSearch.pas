unit PrecompSearch;

interface

uses
  Utils, SynCommons, SynCrypto,
  PrecompUtils,
  WinAPI.Windows,
  System.SysUtils, System.Classes, System.StrUtils,
  System.Types, System.Math, System.IOUtils;

const
  XTOOL_DB = $42445458;

var
  Codec: TPrecompressor;

implementation

type
  PEntryStruct = ^TEntryStruct;

  TEntryStruct = record
    Position: Int64;
    OldSize, NewSize: Integer;
  end;

  PSearchStruct = ^TSearchStruct;

  TSearchStruct = record
    Name: String;
    SearchInt: Int64;
    HashSize: Integer;
    HashDigest: TMD5Digest;
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

function SearchInit(Command: PChar; Count: Integer;
  Funcs: PPrecompFuncs): Boolean;
var
  I: Integer;
  X, Y, Z: Integer;
  S: String;
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
      SetLength(SearchInfo[X], $10000);
      SetLength(SearchCount[X], $10000);
      for Z := Low(SearchInfo[X]) to High(SearchInfo[X]) do
      begin
        SearchCount[X, Z] := 0;
        for Y := Low(CodecSearch[X]) to High(CodecSearch[X]) do
        begin
          LongRec(I).Words[0] := Int64Rec(CodecSearch[X, Y].SearchInt).Words[0];
          if Z = I then
          begin
            Inc(SearchCount[X, Z]);
            Insert(Y, SearchInfo[X, Z], Length(SearchInfo[X, Z]));
          end;
          AddMethod(PrecompGetCodec(PChar(CodecSearch[X, Y].Codec), 0, False));
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
  X, Y, Z: Integer;
  Pos, LSize: NativeInt;
  SI: _StrInfo1;
  SS: PSearchStruct;
  MD5: TMD5;
  Digest: TMD5Digest;
  MD5Checked: Boolean;
begin
  for I := Low(CodecSearch) to High(CodecSearch) do
    if CodecEnabled[I] then
    begin
      Pos := 0;
      LSize := Size - Pred(Int64.Size);
      while Pos < LSize do
      begin
        J := PWord(Input + Pos)^;
        if (SearchCount[I, J] > 0) and
          (CodecSearch[I, 0].HashSize <= (SizeEx - Pos)) then
        begin
          MD5Checked := False;
          for X := 0 to SearchCount[I, J] - 1 do
          begin
            if (PInt64(Input + Pos)^ = CodecSearch[I, SearchInfo[I, J, X]]
              .SearchInt) then
            begin
              if not MD5Checked then
              begin
                MD5.Full(Input + Pos, CodecSearch[I, 0].HashSize, Digest);
                MD5Checked := True;
              end;
              // fix this
              if CompareMem(@CodecSearch[I, SearchInfo[I, J, X]].HashDigest[0],
                @Digest[0], sizeof(TMD5Digest)) then
              begin
                SS := @CodecSearch[I, SearchInfo[I, J, X]];
                Output(Instance, nil, -1);
                for Y := Low(SS^.EntryList) to High(SS^.EntryList) do
                begin
                  SI.Position := Pos + SS^.EntryList[Y].Position;
                  SI.OldSize := SS^.EntryList[Y].OldSize;
                  SI.NewSize := SS^.EntryList[Y].NewSize;
                  SI.Option := 0;
                  SI.Resource := SS^.Resource;
                  if System.Pos(SPrecompSep2, SS^.Codec) > 0 then
                    SI.Status := TStreamStatus.Predicted
                  else
                    SI.Status := TStreamStatus.None;
                  Add(Instance, @SI, PChar(SS^.Codec), nil);
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
  StreamInfo: PStrInfo2; Output: _PrecompOutput; Funcs: PPrecompFuncs): Boolean;
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

initialization

SearchList := TDirectory.GetFiles(ExtractFilePath(Utils.GetModuleName), '*.xtl',
  TSearchOption.soTopDirectoryOnly);
for I := Low(SearchList) to High(SearchList) do
begin
  FStream := TFileStream.Create(SearchList[I], fmShareDenyNone);
  try
    if FStream.Size >= 4 then
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
        end;
        while FStream.Position < FStream.Size do
        begin
          New(SearchStruct);
          SearchStruct^.Name := S;
          FStream.ReadBuffer(SearchStruct^.SearchInt,
            SearchStruct^.SearchInt.Size);
          FStream.ReadBuffer(SearchStruct^.HashSize,
            SearchStruct^.HashSize.Size);
          FStream.ReadBuffer(SearchStruct^.HashDigest, sizeof(THash128));
          FStream.ReadBuffer(I32, I32.Size);
          SetLength(Bytes, I32);
          FStream.ReadBuffer(Bytes[0], I32);
          SearchStruct^.Codec := StringOf(Bytes);
          Insert(SearchStruct^, CodecSearch[J], Length(CodecSearch[J]));
          FStream.ReadBuffer(I32, I32.Size);
          K := Pred(Length(CodecSearch[J]));
          SetLength(CodecSearch[J, K].EntryList, I32);
          FStream.ReadBuffer(CodecSearch[J, K].EntryList[0],
            I32 * sizeof(TEntryStruct));
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
