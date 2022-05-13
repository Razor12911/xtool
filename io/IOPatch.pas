unit IOPatch;

{$POINTERMATH ON}

interface

uses
  Threading, Utils, SynCommons, SynCrypto, ParseClass, ParseExpr,
  IOUtils,
  WinAPI.Windows, WinAPI.ShlObj,
  System.SysUtils, System.Classes, System.SyncObjs, System.Math, System.Types,
  System.StrUtils, System.RTLConsts, System.TimeSpan, System.Diagnostics,
  System.IOUtils, System.Generics.Defaults, System.Generics.Collections;

type
  PEncodeOptions = ^TEncodeOptions;

  TEncodeOptions = record
    Threads: Integer;
    MinSize, MaxSize: Int64;
  end;

  PDecodeOptions = ^TDecodeOptions;

  TDecodeOptions = record
    Threads: Integer;
  end;

procedure PrintHelp;
procedure Parse(ParamArg: TArray<string>; out Options: TEncodeOptions);
  overload;
procedure Parse(ParamArg: TArray<string>; out Options: TDecodeOptions);
  overload;
procedure Encode(Input1, Input2: String; Output: TStream;
  Options: TEncodeOptions);
procedure Decode(Input: TStream; Output: String; Options: TDecodeOptions);

implementation

uses
  XDeltaDLL;

procedure PrintHelp;
var
  I, J: Integer;
  S: string;
begin
  WriteLn(ErrOutput, 'patch - creates patches between two data sets');
  WriteLn(ErrOutput, '');
  WriteLn(ErrOutput, 'Usage:');
  WriteLn(ErrOutput, '  xtool patch [parameters] old_data new_data patch_data');
  WriteLn(ErrOutput, '');
  WriteLn(ErrOutput, '');
  WriteLn(ErrOutput, 'Parameters:');
  WriteLn(ErrOutput, '  -t#  - number of working threads [50p]');
  WriteLn(ErrOutput, '');
end;

procedure Parse(ParamArg: TArray<string>; out Options: TEncodeOptions);
var
  ArgParse: TArgParser;
  ExpParse: TExpressionParser;
  S: String;
begin
  ArgParse := TArgParser.Create(ParamArg);
  ExpParse := TExpressionParser.Create;
  try
    S := ArgParse.AsString('-t', 0, '50p');
    S := ReplaceText(S, 'p', '%');
    S := ReplaceText(S, '%', '%*' + CPUCount.ToString);
    Options.Threads := Max(1, Round(ExpParse.Evaluate(S)));
    S := ArgParse.AsString('--min=', 0, '64k');
    S := ReplaceText(S, 'KB', '* 1024^1');
    S := ReplaceText(S, 'MB', '* 1024^2');
    S := ReplaceText(S, 'GB', '* 1024^3');
    S := ReplaceText(S, 'K', '* 1024^1');
    S := ReplaceText(S, 'M', '* 1024^2');
    S := ReplaceText(S, 'G', '* 1024^3');
    Options.MinSize := Max(0, Round(ExpParse.Evaluate(S)));
    S := ArgParse.AsString('--max=', 0, '16g');
    S := ReplaceText(S, 'KB', '* 1024^1');
    S := ReplaceText(S, 'MB', '* 1024^2');
    S := ReplaceText(S, 'GB', '* 1024^3');
    S := ReplaceText(S, 'K', '* 1024^1');
    S := ReplaceText(S, 'M', '* 1024^2');
    S := ReplaceText(S, 'G', '* 1024^3');
    Options.MaxSize := Max(0, Round(ExpParse.Evaluate(S)));
  finally
    ArgParse.Free;
    ExpParse.Free;
  end;
end;

procedure Parse(ParamArg: TArray<string>; out Options: TDecodeOptions);
var
  ArgParse: TArgParser;
  ExpParse: TExpressionParser;
  S: String;
begin
  ArgParse := TArgParser.Create(ParamArg);
  ExpParse := TExpressionParser.Create;
  try
    S := ArgParse.AsString('-t', 0, '50p');
    S := ReplaceText(S, 'p', '%');
    S := ReplaceText(S, '%', '%*' + CPUCount.ToString);
    Options.Threads := Max(1, Round(ExpParse.Evaluate(S)));
  finally
    ArgParse.Free;
    ExpParse.Free;
  end;
end;

procedure Encode(Input1, Input2: String; Output: TStream;
  Options: TEncodeOptions);
var
  I, J, K: Integer;
  I64: Int64;
  B, C: Boolean;
  LFilename: String;
  BaseDir1, BaseDir2: String;
  LList1, LList2: TArray<String>;
  LBytes: TBytes;
  LEntry: TEntryStruct2;
  FStream: TFileStream;
  SStream1, SStream2: TSharedMemoryStream;
  Tasks: TArray<TTask>;
  CS: TCriticalSection;
  TempDir: String;
begin
  C := FileExists(Input1) and FileExists(Input2);
  if FileExists(Input1) then
    BaseDir1 := ExtractFilePath(TPath.GetFullPath(Input1))
  else if DirectoryExists(Input1) then
    BaseDir1 := IncludeTrailingBackSlash(TPath.GetFullPath(Input1))
  else
    BaseDir1 := ExtractFilePath(TPath.GetFullPath(Input1));
  LList1 := GetFileList([Input1], True);
  for I := Low(LList1) to High(LList1) do
    LList1[I] := ReplaceText(LList1[I], BaseDir1, '');
  if FileExists(Input2) then
    BaseDir2 := ExtractFilePath(TPath.GetFullPath(Input2))
  else if DirectoryExists(Input2) then
    BaseDir2 := IncludeTrailingBackSlash(TPath.GetFullPath(Input2))
  else
    BaseDir2 := ExtractFilePath(TPath.GetFullPath(Input2));
  LList2 := GetFileList([Input2], True);
  for I := Low(LList2) to High(LList2) do
    LList2[I] := ReplaceText(LList2[I], BaseDir2, '');
  K := XTOOL_PATCH;
  Output.WriteBuffer(K, K.Size);
  if not C then
    for I := High(LList1) downto Low(LList1) do
    begin
      if IndexText(LList1[I], LList2) < 0 then
      begin
        LEntry.Op := TPatchOp.opDelete;
        LEntry.Filename := LList1[I];
        LEntry.Size := FileSize(BaseDir1 + LList1[I]);
        Output.WriteBuffer(LEntry, SizeOf(TEntryStruct2));
        Delete(LList1, I, 1);
      end;
    end;
  if not C then
    for I := High(LList2) downto Low(LList2) do
    begin
      if IndexText(LList2[I], LList1) < 0 then
      begin
        LEntry.Op := TPatchOp.opMissing;
        LEntry.Filename := LList2[I];
        LEntry.Size := FileSize(BaseDir2 + LList2[I]);
        Output.WriteBuffer(LEntry, SizeOf(TEntryStruct2));
        FStream := TFileStream.Create(BaseDir2 + LList2[I], fmShareDenyNone);
        try
          CopyStreamEx(FStream, Output, LEntry.Size);
        finally
          FStream.Free;
        end;
        Delete(LList2, I, 1);
      end;
    end;
  for I := High(LList2) downto Low(LList2) do
  begin
    if C then
      LFilename := Input1
    else
      LFilename := BaseDir1 + LList2[I];
    if FileSize(LFilename) <> FileSize(BaseDir2 + LList2[I]) then
    begin
      if InRange(FileSize(BaseDir2 + LList2[I]), Options.MinSize,
        Options.MaxSize) then
        continue;
    end
    else
    begin
      SStream1 := TSharedMemoryStream.Create
        (LowerCase(ChangeFileExt(ExtractFileName(Utils.GetModuleName),
        '_' + Random($7FFFFFFF).ToHexString + XTOOL_MAPSUF2)), LFilename);
      SStream2 := TSharedMemoryStream.Create
        (LowerCase(ChangeFileExt(ExtractFileName(Utils.GetModuleName),
        '_' + Random($7FFFFFFF).ToHexString + XTOOL_MAPSUF2)),
        BaseDir2 + LList2[I]);
      try
        B := SynCommons.CompareMem(SStream1.Memory, SStream2.Memory,
          SStream1.Size);
      finally
        SStream1.Free;
        SStream2.Free;
      end;
      if B then
        if InRange(FileSize(BaseDir2 + LList2[I]), Options.MinSize,
          Options.MaxSize) then
          continue;
    end;
    LEntry.Op := TPatchOp.opMissing;
    LEntry.Filename := LList2[I];
    LEntry.Size := FileSize(BaseDir2 + LList2[I]);
    Output.WriteBuffer(LEntry, SizeOf(TEntryStruct2));
    FStream := TFileStream.Create(BaseDir2 + LList2[I], fmShareDenyNone);
    try
      CopyStreamEx(FStream, Output, LEntry.Size);
    finally
      FStream.Free;
    end;
    Delete(LList2, I, 1);
  end;
  TempDir := IncludeTrailingBackSlash(GetCurrentDir) +
    LowerCase(ChangeFileExt(ExtractFileName(Utils.GetModuleName),
    '_' + Random($7FFFFFFF).ToHexString + XTOOL_MAPSUF1));
  if not DirectoryExists(TempDir) then
    CreateDir(TempDir);
  SetLength(LList1, 0);
  CS := TCriticalSection.Create;
  SetLength(Tasks, Options.Threads);
  J := -1;
  K := 0;
  try
    for I := Low(Tasks) to High(Tasks) do
    begin
      Tasks[I] := TTask.Create(I);
      Tasks[I].Perform(
        procedure(X: IntPtr)
        var
          Y, Z: Integer;
          S1, S2: String;
          A: Boolean;
          SS0, SS1, SS2: TSharedMemoryStream;
          Res: NativeUInt;
        begin
          Z := Length(LList2);
          Y := AtomicIncrement(J);
          while Y < Z do
          begin
            S1 := IncludeTrailingBackSlash(TempDir) + Y.ToHexString +
              XTOOL_MAPSUF3;
            if C then
              S2 := Input1
            else
              S2 := BaseDir1 + LList2[Y];
            SS0 := TSharedMemoryStream.Create
              (LowerCase(ChangeFileExt(ExtractFileName(Utils.GetModuleName),
              '_' + Random($7FFFFFFF).ToHexString + XTOOL_MAPSUF2)), S1);
            SS1 := TSharedMemoryStream.Create
              (LowerCase(ChangeFileExt(ExtractFileName(Utils.GetModuleName),
              '_' + Random($7FFFFFFF).ToHexString + XTOOL_MAPSUF2)), S2);
            SS2 := TSharedMemoryStream.Create
              (LowerCase(ChangeFileExt(ExtractFileName(Utils.GetModuleName),
              '_' + Random($7FFFFFFF).ToHexString + XTOOL_MAPSUF2)),
              BaseDir2 + LList2[Y]);
            try
              SS0.Size := Max(SS1.Size, SS2.Size);
              A := xd3_encode(SS2.Memory, SS2.Size, SS1.Memory, SS1.Size,
                SS0.Memory, @Res, SS0.Size, 0) = 0;
              if A then
                SS0.Size := Res;
            finally
              SS0.Free;
              SS1.Free;
              SS2.Free;
            end;
            if not A then
              DeleteFile(S1);
            CS.Acquire;
            try
              Insert(S1, LList1, Length(LList1));
              Inc(K);
            finally
              CS.Release;
            end;
            Y := AtomicIncrement(J);
          end;
        end);
      Tasks[I].Start;
    end;
    I := 0;
    while I < Length(LList2) do
    begin
      while I >= K do
        Sleep(10);
      LFilename := IncludeTrailingBackSlash(TempDir) + I.ToHexString +
        XTOOL_MAPSUF3;
      if FileExists(LFilename) then
      begin
        LEntry.Op := TPatchOp.opDifferent;
        LEntry.Filename := LList2[I];
        LEntry.Size := FileSize(BaseDir2 + LList2[I]);
        Output.WriteBuffer(LEntry, SizeOf(TEntryStruct2));
        I64 := FileSize(LFilename);
        Output.WriteBuffer(I64, I64.Size);
        FStream := TFileStream.Create(LFilename, fmShareDenyNone);
        try
          CopyStreamEx(FStream, Output, I64);
        finally
          FStream.Free;
        end;
        TFile.Delete(LFilename);
      end
      else
      begin
        LEntry.Op := TPatchOp.opMissing;
        LEntry.Filename := LList1[I];
        LEntry.Size := FileSize(BaseDir1 + LList1[I]);
        Output.WriteBuffer(LEntry, SizeOf(TEntryStruct2));
        FStream := TFileStream.Create(BaseDir1 + LList1[I], fmShareDenyNone);
        try
          CopyStreamEx(FStream, Output, LEntry.Size);
        finally
          FStream.Free;
        end;
      end;
      Inc(I);
    end;
    WaitForAll(Tasks);
  finally
    for I := Low(Tasks) to High(Tasks) do
      Tasks[I].Free;
    CS.Free;
    if DirectoryExists(TempDir) then
      TDirectory.Delete(TempDir);
  end;
  FillChar(LEntry, SizeOf(TEntryStruct2), 0);
  LEntry.Op := TPatchOp.opNone;
  Output.WriteBuffer(LEntry, SizeOf(TEntryStruct2));
end;

procedure Decode(Input: TStream; Output: String; Options: TDecodeOptions);
var
  I64: Int64;
  B: Boolean;
  S1, S2: String;
  LFilename: String;
  BaseDir: String;
  LEntry: TEntryStruct2;
  FStream: TFileStream;
  SStream0, SStream1, SStream2: TSharedMemoryStream;
  Res: NativeUInt;
begin
  if FileExists(Output) then
    BaseDir := ExtractFilePath(TPath.GetFullPath(Output))
  else if DirectoryExists(Output) then
    BaseDir := IncludeTrailingBackSlash(TPath.GetFullPath(Output))
  else
    BaseDir := ExtractFilePath(TPath.GetFullPath(Output));
  S1 := IncludeTrailingBackSlash(GetCurrentDir) +
    LowerCase(ChangeFileExt(ExtractFileName(Utils.GetModuleName),
    '_' + Random($7FFFFFFF).ToHexString + XTOOL_MAPSUF3));
  SStream0 := TSharedMemoryStream.Create
    (LowerCase(ChangeFileExt(ExtractFileName(Utils.GetModuleName),
    '_' + Random($7FFFFFFF).ToHexString + XTOOL_MAPSUF2)), S1);
  try
    Input.ReadBuffer(LEntry, SizeOf(TEntryStruct2));
    while LEntry.Op <> TPatchOp.opNone do
    begin
      if FileExists(Output) then
        LFilename := Output
      else
        LFilename := BaseDir + LEntry.Filename;
      case LEntry.Op of
        TPatchOp.opDelete:
          TFile.Delete(LFilename);
        TPatchOp.opMissing:
          begin
            ForceDirectories(ExtractFilePath(LFilename));
            with TFileStream.Create(LFilename, fmCreate) do
              try
                CopyFrom(Input, LEntry.Size);
              finally
                Free;
              end;
          end;
        TPatchOp.opDifferent:
          begin
            Input.ReadBuffer(I64, I64.Size);
            SStream0.Position := 0;
            CopyStreamEx(Input, SStream0, I64);
            S2 := ChangeFileExt(LFilename, '_' + Random($7FFFFFFF).ToHexString +
              XTOOL_MAPSUF3);
            SStream1 := TSharedMemoryStream.Create
              (LowerCase(ChangeFileExt(ExtractFileName(Utils.GetModuleName),
              '_' + Random($7FFFFFFF).ToHexString + XTOOL_MAPSUF2)), LFilename);
            SStream2 := TSharedMemoryStream.Create
              (LowerCase(ChangeFileExt(ExtractFileName(Utils.GetModuleName),
              '_' + Random($7FFFFFFF).ToHexString + XTOOL_MAPSUF2)), S2);
            B := False;
            try
              SStream2.Size := LEntry.Size;
              B := xd3_decode(SStream0.Memory, I64, SStream1.Memory,
                SStream1.Size, SStream2.Memory, @Res, SStream2.Size, 0) = 0;
            finally
              SStream1.Free;
              SStream2.Free;
            end;
            if B then
            begin
              TFile.Delete(LFilename);
              TFile.Move(S2, LFilename);
            end
            else if FileExists(S2) then
              TFile.Delete(S2);
          end;
      end;
      Input.ReadBuffer(LEntry, SizeOf(TEntryStruct2));
    end;
  finally
    SStream0.Free;
    if FileExists(S1) then
      TFile.Delete(S1);
  end;
end;

end.
