unit DbgMain;

interface

uses
  Threading, Utils, SynCommons, SynCrypto, ParseClass, ParseExpr,
  DbgUtils,
  WinAPI.Windows, WinAPI.ShlObj,
  System.SysUtils, System.Classes, System.SyncObjs, System.Math, System.Types,
  System.StrUtils, System.RTLConsts, System.TimeSpan, System.Diagnostics,
  System.IOUtils, System.Generics.Defaults, System.Generics.Collections;

type
  PEncodeOptions = ^TEncodeOptions;

  TEncodeOptions = record
    ChunkSize, Threads: Integer;
    Method: String;
    BlockSize: Integer;
  end;

procedure PrintHelp;
procedure Parse(ParamArg: TArray<string>; out Options: TEncodeOptions);
procedure Encode(Input1, Input2, Output: String; Options: TEncodeOptions);

implementation

const
  MinSize1 = 256;
  MinSize2 = 65536;
  HashSize = 4 * 1024 * 1024;

type
  PScanInfo = ^TScanInfo;

  TScanInfo = record
    CRCSize, ActualSize: Integer;
    CRC1, CRC2: Cardinal;
  end;

  PHashStruct = ^THashStruct;

  THashStruct = record
    Size: Integer;
    Hash: Cardinal;
  end;

var
  SearchInfo: TArray<TArray<TArray<TScanInfo>>>;
  SearchCount: TArray<TArray<Integer>>;

procedure PrintHelp;
var
  I, J: Integer;
  S: string;
begin
  WriteLn(ErrOutput, 'generate - database generator');
  WriteLn(ErrOutput, '');
  WriteLn(ErrOutput, 'Usage:');
  WriteLn(ErrOutput,
    '  xtool generate [parameters] extracted_streams original_data database_output');
  WriteLn(ErrOutput, '');
  WriteLn(ErrOutput, '');
  WriteLn(ErrOutput, 'Parameters:');
  WriteLn(ErrOutput, '  -m#  - codec to use for precompression');
  WriteLn(ErrOutput, '  -c#  - scanning range [16mb]');
  WriteLn(ErrOutput, '  -t#  - number of working threads [50p]');
  WriteLn(ErrOutput, '');
end;

procedure Parse(ParamArg: TArray<string>; out Options: TEncodeOptions);
var
  ArgParse: TArgParser;
  ExpParse: TExpressionParser;
  I: Integer;
  S: String;
begin
  ArgParse := TArgParser.Create(ParamArg);
  ExpParse := TExpressionParser.Create;
  try
    Options.Method := '';
    I := 0;
    while True do
    begin
      S := ArgParse.AsString('-m', I);
      if S = '' then
        break;
      S := ReplaceStr(S, SPrecompSep3, SPrecompSep2);
      if Options.Method <> '' then
        Options.Method := Options.Method + '+' + S
      else
        Options.Method := S;
      Inc(I);
    end;
    S := ArgParse.AsString('-c', 0, '16mb');
    S := ReplaceText(S, 'KB', '* 1024^1');
    S := ReplaceText(S, 'MB', '* 1024^2');
    S := ReplaceText(S, 'GB', '* 1024^3');
    S := ReplaceText(S, 'K', '* 1024^1');
    S := ReplaceText(S, 'M', '* 1024^2');
    S := ReplaceText(S, 'G', '* 1024^3');
    Options.ChunkSize := Max(4194304, Round(ExpParse.Evaluate(S)));
    S := ArgParse.AsString('-t', 0, '50p');
    S := ReplaceText(S, 'p', '%');
    S := ReplaceText(S, '%', '%*' + CPUCount.ToString);
    Options.Threads := Max(1, Round(ExpParse.Evaluate(S)));
    S := ArgParse.AsString('-c', 0, '1792mb');
    S := ReplaceText(S, 'KB', '* 1024^1');
    S := ReplaceText(S, 'MB', '* 1024^2');
    S := ReplaceText(S, 'GB', '* 1024^3');
    S := ReplaceText(S, 'K', '* 1024^1');
    S := ReplaceText(S, 'M', '* 1024^2');
    S := ReplaceText(S, 'G', '* 1024^3');
    Options.BlockSize := Round(ExpParse.Evaluate(S));
  finally
    ArgParse.Free;
    ExpParse.Free;
  end;
end;

function GenerateHashList(Stream: TStream;
  var HashList: TArray<THashStruct>): Integer;
const
  BufferSize = 65536;
var
  Buffer: array [0 .. BufferSize - 1] of Byte;
  I: Integer;
  X, Y: Integer;
  OldPos: Int64;
begin
  Result := 0;
  SetLength(HashList, Max(Length(HashList), IfThen(Stream.Size mod HashSize = 0,
    Stream.Size div HashSize, Succ(Stream.Size div HashSize))));
  OldPos := Stream.Position;
  Stream.Position := 0;
  try
    for I := Low(HashList) to High(HashList) do
    begin
      HashList[I].Size := 0;
      HashList[I].Hash := 0;
      X := HashSize;
      Y := Stream.Read(Buffer[0], Min(X, BufferSize));
      while Y > 0 do
      begin
        Inc(HashList[I].Size, Y);
        HashList[I].Hash := Utils.Hash32(HashList[I].Hash, @Buffer[0], Y);
        Dec(X, Y);
        Y := Stream.Read(Buffer[0], Min(X, BufferSize));
      end;
      Inc(Result);
      if HashList[I].Size = 0 then
        break;
    end;
  finally
    Stream.Position := OldPos;
  end;
end;

procedure Encode(Input1, Input2, Output: String; Options: TEncodeOptions);
const
  BufferSize = 65536;
var
  Buffer: array [0 .. BufferSize - 1] of Byte;
  A: Word;
  B: Byte;
  I, J, K: Integer;
  LastStream: Int64;
  Found1, Found2: Boolean;
  CountPos: NativeInt;
  BaseDir: String;
  LList: TArray<String>;
  LSInfo: TScanInfo;
  LEntry: TEntryStruct;
  LBytes: TBytes;
  Hash: Cardinal;
  HashList: TArray<THashStruct>;
  HashCount: Integer;
  FStream: TFileStream;
  OStream, MStream: TMemoryStream;
  DataStore: TDataStore1;
  Tasks: TArray<TTask>;
  NStream: TArray<TMemoryStream>;
  InfoStore: TArray<TListEx<TEntryStruct>>;
begin
  SetLength(SearchInfo, $10000);
  SetLength(SearchCount, $10000);
  for I := Low(SearchInfo) to High(SearchInfo) do
  begin
    SetLength(SearchInfo[I], $100);
    SetLength(SearchCount[I], $100);
    for J := Low(SearchCount[I]) to High(SearchCount[I]) do
      SearchCount[I, J] := 0;
  end;
  if FileExists(Input1) then
    BaseDir := ExtractFilePath(TPath.GetFullPath(Input1))
  else if DirectoryExists(Input1) then
    BaseDir := IncludeTrailingBackSlash(TPath.GetFullPath(Input1))
  else
    BaseDir := ExtractFilePath(TPath.GetFullPath(Input1));
  LList := GetFileList([Input1], True);
  SetLength(Tasks, Options.Threads);
  SetLength(Tasks, Options.Threads);
  for I := Low(Tasks) to High(Tasks) do
  begin
    Tasks[I] := TTask.Create(I);
    NStream[I] := TMemoryStream.Create;
  end;
  for I := Low(LList) to High(LList) do
  begin
    if InRange(FileSize(LList[I]), MinSize1, Integer.MaxValue) then
    begin
      FStream := TFileStream.Create(LList[I], fmShareDenyNone);
      with FStream do
        try
          ReadBuffer(Buffer[0], MinSize1);
          WordRec(A).Bytes[0] := Buffer[0];
          WordRec(A).Bytes[1] := Buffer[1];
          B := Buffer[MinSize1 - 1];
          J := MinSize1;
          LSInfo.CRCSize := J;
          LSInfo.ActualSize := FileSize(LList[I]);
          LSInfo.CRC1 := Utils.Hash32(0, @Buffer[0], J);
          LSInfo.CRC2 := LSInfo.CRC1;
          while (J > 0) and (LSInfo.CRCSize < Options.ChunkSize) do
          begin
            J := Read(Buffer[0], Min(Options.ChunkSize - LSInfo.CRCSize,
              BufferSize));
            Inc(LSInfo.CRCSize, J);
            LSInfo.CRC2 := Utils.Hash32(LSInfo.CRC2, @Buffer[0], J);
          end;
          Insert(LSInfo, SearchInfo[A, B], Length(SearchInfo[A, B]));
          Inc(SearchCount[A, B]);
        finally
          Free;
        end;
    end
    else if FileSize(LList[I]) < MinSize1 then
      WriteLn(ErrOutput, Format('Skipped %s (Smaller than %d)',
        [ReplaceText(LList[I], BaseDir, ''), MinSize1]))
    else if FileSize(LList[I]) > Integer.MaxValue then
      WriteLn(ErrOutput, Format('Skipped %s (Larger than %d)',
        [ReplaceText(LList[I], BaseDir, ''), Integer.MaxValue]));
  end;
  for I := Low(Tasks) to High(Tasks) do
    NStream[I].Free;
  DataStore := TDataStore1.Create(nil, True, Options.Threads,
    Options.ChunkSize);
  SetLength(InfoStore, Options.Threads);
  OStream := TMemoryStream.Create;
  MStream := TMemoryStream.Create;
  if FileExists(Output) then
    OStream.LoadFromFile(Output)
  else
  begin
    K := XTOOL_DB;
    OStream.WriteBuffer(K, K.Size);
  end;
  OStream.Position := OStream.Size;
  Found1 := False;
  try
    for I := Low(InfoStore) to High(InfoStore) do
    begin
      InfoStore[I] := TListEx<TEntryStruct>.Create(EntryStructCmp);
      Tasks[I].Perform(
        procedure(X: IntPtr)
        var
          Ptr: PByte;
          Y: Integer;
          C: Word;
          D: Byte;
          Pos, Size, SizeEx: NativeInt;
          CRC: Cardinal;
          E: TEntryStruct;
          F: Boolean;
        begin
          Ptr := DataStore.Slot(X).Memory;
          Pos := 0;
          Size := DataStore.Size(X) - MinSize1;
          SizeEx := DataStore.ActualSize(X);
          while Pos < Size do
          begin
            C := PWord(Ptr + Pos)^;
            D := (Ptr + Pos + MinSize1 - 1)^;
            if (SearchCount[C, D] > 0) then
            begin
              F := False;
              CRC := Utils.Hash32(0, Ptr + Pos, MinSize1);
              for Y := 0 to SearchCount[C, D] - 1 do
              begin
                if (SearchInfo[C, D, Y].CRCSize <= (SizeEx - Pos)) then
                  if (CRC = SearchInfo[C, D, Y].CRC1) and
                    (Utils.Hash32(CRC, Ptr + Pos + MinSize1, SearchInfo[C, D,
                    Y].CRCSize - MinSize1) = SearchInfo[C, D, Y].CRC2) then
                  begin
                    E.Position := DataStore.Position(X) + Pos;
                    E.OldSize := SearchInfo[C, D, Y].ActualSize;
                    E.NewSize := 0;
                    E.DepthSize := 0;
                    InfoStore[X].Add(E);
                    Inc(Pos, E.OldSize);
                    F := True;
                    break;
                  end;
              end;
              if F then
                continue;
            end;
            Inc(Pos);
          end;
        end);
    end;
    if FileExists(Input2) then
      BaseDir := ExtractFilePath(TPath.GetFullPath(Input2))
    else if DirectoryExists(Input2) then
      BaseDir := IncludeTrailingBackSlash(TPath.GetFullPath(Input2))
    else
      BaseDir := ExtractFilePath(TPath.GetFullPath(Input2));
    LList := GetFileList([Input2], True);
    for I := Low(LList) to High(LList) do
    begin
      if FileSize(LList[I]) >= MinSize2 then
      begin
        FStream := TFileStream.Create(LList[I], fmShareDenyNone);
        try
          HashCount := GenerateHashList(FStream, HashList);
          LastStream := 0;
          MStream.Position := 0;
          Found2 := False;
          DataStore.ChangeInput(FStream);
          DataStore.Load;
          Hash := Utils.Hash32(0, DataStore.Slot(0).Memory, MinSize2);
          MStream.WriteBuffer(DataStore.Slot(0).Memory^, Integer.Size);
          MStream.WriteBuffer(PInteger(PByte(DataStore.Slot(0).Memory) +
            MinSize2 - Integer.Size)^, Integer.Size);
          MStream.WriteBuffer(Hash, Hash.Size);
          MStream.WriteBuffer(HashCount, HashCount.Size);
          MStream.WriteBuffer(HashList[0], HashCount * SizeOf(THashStruct));
          LBytes := BytesOf(Options.Method);
          K := Length(LBytes);
          MStream.WriteBuffer(K, K.Size);
          MStream.WriteBuffer(LBytes[0], K);
          CountPos := MStream.Position;
          K := 0;
          MStream.WriteBuffer(K, K.Size);
          while not DataStore.Done do
          begin
            for J := Low(Tasks) to High(Tasks) do
            begin
              InfoStore[J].Count := 0;
              Tasks[J].Start;
            end;
            WaitForAll(Tasks);
            for J := Low(Tasks) to High(Tasks) do
            begin
              InfoStore[J].Index := 0;
              K := InfoStore[J].Get(LEntry);
              while K >= 0 do
              begin
                if LEntry.Position < LastStream then
                  InfoStore[J].Delete(K)
                else
                  break;
                K := InfoStore[J].Get(LEntry);
              end;
              Inc(PInteger(PByte(MStream.Memory) + CountPos)^,
                InfoStore[J].Count);
              if InfoStore[J].Count > 0 then
              begin
                Found1 := True;
                Found2 := True;
              end;
              InfoStore[J].Index := 0;
              K := InfoStore[J].Get(LEntry);
              while K >= 0 do
              begin
                MStream.WriteBuffer(LEntry, SizeOf(TEntryStruct));
                LastStream := LEntry.Position + LEntry.OldSize;
                K := InfoStore[J].Get(LEntry);
              end;
            end;
            DataStore.Load;
          end;
          if Found2 then
            OStream.WriteBuffer(MStream.Memory^, MStream.Position);
        finally
          FStream.Free;
        end;
      end
      else if FileSize(LList[I]) < MinSize2 then
        WriteLn(ErrOutput, Format('Skipped %s (Smaller than %d)',
          [ReplaceText(LList[I], BaseDir, ''), MinSize2]));
    end;
    if Found1 then
      OStream.SaveToFile(Output);
  finally
    for I := Low(InfoStore) to High(InfoStore) do
    begin
      InfoStore[I].Free;
      Tasks[I].Free;
      NStream[I].Free;
    end;
    DataStore.Free;
    OStream.Free;
    MStream.Free;
  end;
  for I := Low(InfoStore) to High(InfoStore) do
    Tasks[I].Free;
end;

end.
