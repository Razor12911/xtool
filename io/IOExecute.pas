unit IOExecute;

(* xbcm:t4:c256m
  xtool execute {options} - - <stdin> <stdout> bcm.exe -9 [filein] [fileout]
  xtool decode {options} - - <stdin> <stdout> bcm.exe -d [filein] [fileout] *)

interface

uses
  InitCode,
  Threading, Utils, SynCommons, SynCrypto, ParseClass, ParseExpr,
  IOUtils,
  WinAPI.Windows, WinAPI.ShlObj,
  System.SysUtils, System.Classes, System.SyncObjs, System.Math, System.Types,
  System.StrUtils, System.RTLConsts, System.TimeSpan, System.Diagnostics,
  System.IOUtils, System.Generics.Defaults, System.Generics.Collections;

type
  PEncodeOptions = ^TEncodeOptions;

  TEncodeOptions = record
    ChunkSize, Threads: Integer;
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
procedure Encode(Input, Output: TStream; ParamArg: TArray<string>;
  Options: TEncodeOptions);
procedure Decode(Input, Output: TStream; ParamArg: TArray<string>;
  Options: TDecodeOptions);

implementation

const
  FILE_IN = 'data.in';
  FILE_OUT = 'data.out';
  FILE_MODE = 0;
  STDIN_MODE = 1;
  STDOUT_MODE = 2;
  STDIO_MODE = 3;
  STATE_READY = 0;
  STATE_EXECUTED = 1;
  STATE_ERROR = 2;
  STATE_DONE = 3;

procedure PrintHelp;
var
  I, J: Integer;
  S: string;
begin
  WriteLn(ErrOutput, 'execute - parallel program execution');
  WriteLn(ErrOutput, '');
  WriteLn(ErrOutput, 'Usage:');
  WriteLn(ErrOutput, '  xtool execute [parameters] input output [exec_syntax]');
  WriteLn(ErrOutput, '');
  WriteLn(ErrOutput, '');
  WriteLn(ErrOutput, 'Parameters:');
  WriteLn(ErrOutput, '  -c#  - chunk size [16mb]');
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

function ExtractStr(SubStr, Str: String): String;
var
  I: Integer;
begin
  Result := Str.Substring(Str.IndexOf(SubStr));
  I := Result.IndexOf(' ');
  if I >= 0 then
    Result := Result.Substring(0, Result.IndexOf(' '));
end;

type
  PCtx = ^TCtx;

  TCtx = record
    Exec, Param: String;
    InFile, OutFile: String;
    Mode: Byte;
  end;

procedure Init(ParamArg: TArray<string>; Ctx: PCtx);
var
  I: Integer;
  S: String;
begin
  with Ctx^ do
  begin
    Exec := '';
    Param := '';
    InFile := FILE_IN;
    OutFile := FILE_OUT;
    Mode := 0;
    for I := Low(ParamArg) to High(ParamArg) do
    begin
      S := ParamArg[I];
      if ContainsText(S, '<stdin>') or ContainsText(S, '[stdin]') then
      begin
        SetBits(Mode, 1, 0, 1);
        continue;
      end
      else if ContainsText(S, '<stdout>') or ContainsText(S, '[stdout]') then
      begin
        SetBits(Mode, 1, 1, 1);
        continue;
      end
      else if ContainsText(S, '<filein>') or ContainsText(S, '[filein]') then
      begin
        SetBits(Mode, 0, 0, 1);
        if ContainsText(S, '<filein>') then
          InFile := ExtractStr('<filein>', S)
        else
          InFile := ExtractStr('[filein]', S);
        S := ReplaceText(S, InFile, FILE_IN);
        InFile := ReplaceText(InFile, '<filein>', FILE_IN);
        InFile := ReplaceText(InFile, '[filein]', FILE_IN);
      end
      else if ContainsText(S, '<fileout>') or ContainsText(S, '[fileout]') then
      begin
        SetBits(Mode, 0, 1, 1);
        if ContainsText(S, '<fileout>') then
          OutFile := ExtractStr('<fileout>', S)
        else
          OutFile := ExtractStr('[fileout]', S);
        S := ReplaceText(S, OutFile, FILE_OUT);
        OutFile := ReplaceText(OutFile, '<fileout>', FILE_OUT);
        OutFile := ReplaceText(OutFile, '[fileout]', FILE_OUT);
      end;
      if I = 0 then
        Exec := ExpandPath(PluginsPath + S, True)
      else
        Param := Param + ' ' + IfThen(ContainsText(S, ' ') or (S = ''),
          '"' + S + '"', S);
    end;
  end;
end;

threadvar TFS: TFileStream;

procedure Callback(const Buffer: Pointer; Size: Integer);
begin
  TFS.WriteBuffer(Buffer^, Size);
end;

procedure ExecThread(X, Ctx, WorkDir, State: IntPtr);
var
  SS: TSharedMemoryStream;
  Res: Boolean;
begin
  Res := False;
  with PCtx(Ctx)^ do
    if FileExists(Exec) then
      try
        case Mode of
          FILE_MODE:
            Res := Utils.Exec(Exec, Param, PString(WorkDir)^);
          STDIN_MODE:
            begin
              SS := TSharedMemoryStream.Create
                (LowerCase(ChangeFileExt(ExtractFileName(Utils.GetModuleName),
                '_' + Random($7FFFFFFF).ToHexString + XTOOL_MAPSUF2)),
                IncludeTrailingPathDelimiter(PString(WorkDir)^) + InFile);
              try
                Res := ExecStdin(Exec, Param, PString(WorkDir)^,
                  SS.Memory, SS.Size);
              finally
                SS.Free;
              end;
            end;
          STDOUT_MODE:
            begin
              TFS := TFileStream.Create
                (IncludeTrailingPathDelimiter(PString(WorkDir)^) + OutFile,
                fmCreate);
              try
                Res := ExecStdout(Exec, Param, PString(WorkDir)^, Callback);
              finally
                TFS.Free;
              end;
            end;
          STDIO_MODE:
            begin
              SS := TSharedMemoryStream.Create
                (LowerCase(ChangeFileExt(ExtractFileName(Utils.GetModuleName),
                '_' + Random($7FFFFFFF).ToHexString + XTOOL_MAPSUF2)),
                IncludeTrailingPathDelimiter(PString(WorkDir)^) + InFile);
              TFS := TFileStream.Create
                (IncludeTrailingPathDelimiter(PString(WorkDir)^) + OutFile,
                fmCreate);
              try
                Res := ExecStdio(Exec, Param, PString(WorkDir)^, SS.Memory,
                  SS.Size, Callback);
              finally
                SS.Free;
                TFS.Free;
              end;
            end;
        end;
      except
        Res := False
      end;
  if Res then
    PByte(State)^ := STATE_EXECUTED
  else
    PByte(State)^ := STATE_ERROR;
end;

procedure Encode(Input, Output: TStream; ParamArg: TArray<string>;
  Options: TEncodeOptions);
var
  I: Integer;
  I64: Int64;
  B: Byte;
  S: String;
  First, Done: Boolean;
  FStream: TFileStream;
  SStream: TSharedMemoryStream;
  LCtx: TCtx;
  WorkDir: TArray<String>;
  Tasks: TArray<TTask>;
  State: TArray<Byte>;

  procedure Load(X: Integer);
  begin
    DeleteFile(IncludeTrailingPathDelimiter(WorkDir[X]) + LCtx.InFile);
    DeleteFile(IncludeTrailingPathDelimiter(WorkDir[X]) + LCtx.OutFile);
    if not Done then
    begin
      FStream := TFileStream.Create(IncludeTrailingPathDelimiter(WorkDir[X]) +
        LCtx.InFile, fmCreate);
      try
        Done := CopyStream(Input, FStream, Options.ChunkSize) = 0;
      finally
        FStream.Free;
      end;
    end;
    if Done then
      State[X] := STATE_DONE
    else
      State[X] := STATE_READY;
    if not Done then
      Tasks[X].Start;
  end;

begin
  I := XTOOL_EXEC;
  Output.WriteBuffer(I, I.Size);
  Init(ParamArg, @LCtx);
  SetLength(WorkDir, Options.Threads);
  SetLength(Tasks, Options.Threads);
  SetLength(State, Options.Threads);
  for I := Low(Tasks) to High(Tasks) do
  begin
    WorkDir[I] := IncludeTrailingPathDelimiter(GetCurrentDir) +
      LowerCase(ChangeFileExt(ExtractFileName(Utils.GetModuleName),
      '_' + Random($7FFFFFFF).ToHexString + XTOOL_MAPSUF1));
    CreateDir(WorkDir[I]);
    Tasks[I] := TTask.Create(I, IntPtr(@LCtx), IntPtr(@WorkDir[I]),
      IntPtr(@State[I]));
    Tasks[I].Perform(ExecThread);
  end;
  First := True;
  Done := False;
  try
    while State[0] <> STATE_DONE do
    begin
      if First then
      begin
        for I := Low(Tasks) to High(Tasks) do
          Load(I);
        First := False;
      end;
      for I := Low(Tasks) to High(Tasks) do
      begin
        Tasks[I].Wait;
        if State[I] = STATE_DONE then
          continue;
        B := 0;
        if State[I] = STATE_EXECUTED then
          S := IncludeTrailingPathDelimiter(WorkDir[I]) + LCtx.OutFile
        else
        begin
          S := IncludeTrailingPathDelimiter(WorkDir[I]) + LCtx.InFile;
          B := 1;
        end;
        SStream := TSharedMemoryStream.Create
          (LowerCase(ChangeFileExt(ExtractFileName(Utils.GetModuleName),
          '_' + Random($7FFFFFFF).ToHexString + XTOOL_MAPSUF2)), S);
        try
          Output.WriteBuffer(B, B.Size);
          I64 := SStream.Size;
          Output.WriteBuffer(I64, I64.Size);
          CopyStreamEx(SStream, Output, SStream.Size);
        finally
          SStream.Free;
        end;
        Load(I);
      end;
    end;
    WaitForAll(Tasks);
    B := 0;
    Output.WriteBuffer(B, B.Size);
    I64 := I64.MinValue;
    Output.WriteBuffer(I64, I64.Size);
  finally
    for I := Low(Tasks) to High(Tasks) do
    begin
      if DirectoryExists(WorkDir[I]) then
        TDirectory.Delete(WorkDir[I], True);
      Tasks[I].Free;
    end;
  end;
end;

procedure Decode(Input, Output: TStream; ParamArg: TArray<string>;
  Options: TDecodeOptions);
var
  I: Integer;
  S: String;
  First, Done: Boolean;
  FStream: TFileStream;
  SStream: TSharedMemoryStream;
  LCtx: TCtx;
  WorkDir: TArray<String>;
  Tasks: TArray<TTask>;
  State: TArray<Byte>;

  procedure Load(X: Integer);
  var
    B: Byte;
    I64: Int64;
  begin
    DeleteFile(IncludeTrailingPathDelimiter(WorkDir[X]) + LCtx.InFile);
    DeleteFile(IncludeTrailingPathDelimiter(WorkDir[X]) + LCtx.OutFile);
    if not Done then
    begin
      repeat
        Input.ReadBuffer(B, B.Size);
        Input.ReadBuffer(I64, I64.Size);
        if I64 >= 0 then
        begin
          FStream := TFileStream.Create(IncludeTrailingPathDelimiter(WorkDir[X]) +
            LCtx.InFile, fmCreate);
          try
            if B = 0 then
              CopyStreamEx(Input, FStream, I64)
            else
              CopyStreamEx(Input, Output, I64);
          finally
            FStream.Free;
          end;
        end
        else
          Done := True;
      until (B = 0) or Done;
    end;
    if Done then
      State[X] := STATE_DONE
    else
      State[X] := STATE_READY;
    if not Done then
      Tasks[X].Start;
  end;

begin
  Init(ParamArg, @LCtx);
  SetLength(WorkDir, Options.Threads);
  SetLength(Tasks, Options.Threads);
  SetLength(State, Options.Threads);
  for I := Low(Tasks) to High(Tasks) do
  begin
    WorkDir[I] := IncludeTrailingPathDelimiter(GetCurrentDir) +
      LowerCase(ChangeFileExt(ExtractFileName(Utils.GetModuleName),
      '_' + Random($7FFFFFFF).ToHexString + XTOOL_MAPSUF1));
    CreateDir(WorkDir[I]);
    Tasks[I] := TTask.Create(I, IntPtr(@LCtx), IntPtr(@WorkDir[I]),
      IntPtr(@State[I]));
    Tasks[I].Perform(ExecThread);
  end;
  First := True;
  Done := False;
  try
    while State[0] <> STATE_DONE do
    begin
      if First then
      begin
        for I := Low(Tasks) to High(Tasks) do
          Load(I);
        First := False;
      end;
      for I := Low(Tasks) to High(Tasks) do
      begin
        Tasks[I].Wait;
        if State[I] = STATE_DONE then
          continue;
        if State[I] = STATE_EXECUTED then
          S := IncludeTrailingPathDelimiter(WorkDir[I]) + LCtx.OutFile
        else
          raise Exception.CreateRes(@SWriteError);
        SStream := TSharedMemoryStream.Create
          (LowerCase(ChangeFileExt(ExtractFileName(Utils.GetModuleName),
          '_' + Random($7FFFFFFF).ToHexString + XTOOL_MAPSUF2)), S);
        try
          CopyStreamEx(SStream, Output, SStream.Size);
        finally
          SStream.Free;
        end;
        Load(I);
      end;
    end;
    WaitForAll(Tasks);
  finally
    for I := Low(Tasks) to High(Tasks) do
    begin
      if DirectoryExists(WorkDir[I]) then
        TDirectory.Delete(WorkDir[I], True);
      Tasks[I].Free;
    end;
  end;
end;

end.
