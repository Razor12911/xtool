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
    Format: String;
  end;

procedure PrintHelp;
procedure Parse(ParamArg: TArray<string>; out Options: TEncodeOptions);
procedure Encode(Input1, Input2, Output: String; Options: TEncodeOptions);

implementation

const
  MinSize1 = 256;
  MinSize2 = 65536;

type
  PScanInfo = ^TScanInfo;

  TScanInfo = record
    Size: Integer;
    CRC1, CRC2: Cardinal;
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
  WriteLn(ErrOutput, '  -c#  - scanning range of precompressor [16mb]');
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
  finally
    ArgParse.Free;
    ExpParse.Free;
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
  LList: TArray<String>;
  LSInfo: PScanInfo;
  LEntry: TEntryStruct;
  LBytes: TBytes;
  LMD5: TMD5;
  Hash: TMD5Digest;
  FStream: TFileStream;
  OStream, MStream: TMemoryStream;
  DataStore: TDataStore1;
  Tasks: TArray<TTask>;
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
  LList := GetFileList([Input1], True);
  for I := Low(LList) to High(LList) do
  begin
    if FileSize(LList[I]) >= MinSize1 then
    begin
      FStream := TFileStream.Create(LList[I], fmShareDenyNone);
      with FStream do
        try
          ReadBuffer(Buffer[0], MinSize1);
          WordRec(A).Bytes[0] := Buffer[0];
          WordRec(A).Bytes[1] := Buffer[1];
          B := Buffer[MinSize1 - 1];
          J := MinSize1;
          New(LSInfo);
          LSInfo^.Size := J;
          LSInfo^.CRC1 := Utils.Hash32(0, @Buffer[0], J);
          LSInfo^.CRC2 := LSInfo^.CRC1;
          while (J > 0) and (LSInfo^.Size < Options.ChunkSize) do
          begin
            J := Read(Buffer[0], Min(Options.ChunkSize - LSInfo^.Size,
              BufferSize));
            Inc(LSInfo^.Size, J);
            LSInfo^.CRC2 := Utils.Hash32(LSInfo^.CRC2, @Buffer[0], J);
          end;
        finally
          Free;
        end;
      Insert(LSInfo^, SearchInfo[A, B], Length(SearchInfo[A, B]));
      Inc(SearchCount[A, B]);
    end;
  end;
  DataStore := TDataStore1.Create(nil, True, Options.Threads,
    Options.ChunkSize);
  SetLength(Tasks, Options.Threads);
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
    for I := Low(Tasks) to High(Tasks) do
    begin
      InfoStore[I] := TListEx<TEntryStruct>.Create(EntryStructCmp);
      Tasks[I] := TTask.Create(I);
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
                if (SearchInfo[C, D, Y].Size <= (SizeEx - Pos)) then
                  if (CRC = SearchInfo[C, D, Y].CRC1) and
                    (Utils.Hash32(CRC, Ptr + Pos + MinSize1, SearchInfo[C, D,
                    Y].Size - MinSize1) = SearchInfo[C, D, Y].CRC2) then
                  begin
                    E.Position := DataStore.Position(X) + Pos;
                    E.OldSize := SearchInfo[C, D, Y].Size;
                    E.NewSize := 0;
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
    LList := GetFileList([Input2], True);
    for I := Low(LList) to High(LList) do
    begin
      if FileSize(LList[I]) >= MinSize2 then
      begin
        FStream := TFileStream.Create(LList[I], fmShareDenyNone);
        try
          LastStream := 0;
          MStream.Position := 0;
          Found2 := False;
          DataStore.ChangeInput(FStream);
          DataStore.Load;
          LMD5.Full(DataStore.Slot(0).Memory, MinSize2, Hash);
          MStream.WriteBuffer(DataStore.Slot(0).Memory^, Int64.Size);
          K := MinSize2;
          MStream.WriteBuffer(K, K.Size);
          MStream.WriteBuffer(Hash, SizeOf(TMD5Digest));
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
      end;
    end;
    if Found1 then
      OStream.SaveToFile(Output);
  finally
    for I := Low(Tasks) to High(Tasks) do
    begin
      InfoStore[I].Free;
      Tasks[I].Free;
    end;
    DataStore.Free;
    OStream.Free;
    MStream.Free;
  end;
end;

end.
