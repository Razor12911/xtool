unit PrecompExe;

interface

uses
  InitCode,
  Utils, Threading,
  SynCommons, SynCrypto,
  PrecompUtils,
  WinAPI.Windows,
  System.SysUtils, System.Classes, System.StrUtils,
  System.Types, System.Math, System.IOUtils, System.IniFiles;

const
  FILE_IN = 'data.in';
  FILE_OUT = 'data.out';
  FILE_MODE = 0;
  STDIN_MODE = 1;
  STDOUT_MODE = 2;
  STDIO_MODE = 3;

type
  PExeStruct = ^TExeStruct;

  TExeStruct = record
    Name: String;
    ID: Cardinal;
    Exec, Param: array [0 .. 1] of String;
    WorkDir: array of array [0 .. 1] of String;
    Mode: array [0 .. 1] of Byte;
    InFile, OutFile: array [0 .. 1] of String;
    IsLib: array [0 .. 1] of Boolean;
    Ctx: array of array [0 .. 1] of Pointer;
  end;

var
  Codec: TPrecompressor;

implementation

const
  E_WORKMEM = 65536;

var
  WrkMem: array of array [0 .. E_WORKMEM - 1] of Byte;
  CodecSize: TArray<Integer>;
  CodecOutput: TArray<_PrecompOutput>;
  CodecAllocator: array of function(
    Index: Integer; Size: Integer): Pointer cdecl;
  CodecExe: TArray<TExeStruct>;

type
  PExecCtx = ^TExecCtx;

  TExecCtx = record
    FInstance: Integer;
    FLib: Boolean;
    FExecutable, FCommandLine, FWorkDir: string;
    hstdinr, hstdinw: THandle;
    hstdoutr, hstdoutw: THandle;
    StartupInfo: TStartupInfo;
    ProcessInfo: TProcessInformation;
    FTask, MTask: TTask;
  end;

procedure ExecReadTask(Instance, Handle, Stream: IntPtr);
const
  BufferSize = 65536;
var
  Buffer: array [0 .. BufferSize - 1] of Byte;
  BytesRead: DWORD;
begin
  while ReadFile(Handle, Buffer[0], Length(Buffer), BytesRead, nil) and
    (BytesRead > 0) do
    PExecOutput(Pointer(Stream))^(Instance, @Buffer[0], BytesRead);
end;

procedure ExecMonTask(Process, Stdin, Stdout: IntPtr);
begin
  WaitForSingleObject(PHandle(Process)^, INFINITE);
  CloseHandleEx(PHandle(Process)^);
  CancelIo(PHandle(Stdin)^);
  CloseHandleEx(PHandle(Stdin)^);
  CancelIo(PHandle(Stdout)^);
  CloseHandleEx(PHandle(Stdout)^);
end;

function ExecStdioInit(Instance: Integer; Executable, CommandLine,
  WorkDir: PChar; IsLib: Boolean): PExecCtx;
begin
  New(Result);
  with Result^ do
  begin
    FInstance := Instance;
    FLib := IsLib;
    FExecutable := Executable;
    FCommandLine := CommandLine;
    if WorkDir <> '' then
      FWorkDir := WorkDir
    else
      FWorkDir := GetCurrentDir;
    ZeroMemory(@ProcessInfo, SizeOf(ProcessInfo));
    FTask := nil;
    MTask := nil;
  end;
end;

procedure ExecStdioFree(Ctx: PExecCtx);
begin
  with Ctx^ do
  begin
    TerminateProcess(ProcessInfo.hProcess, 0);
    WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
    if Assigned(FTask) then
    begin
      FTask.Free;
      MTask.Wait;
      MTask.Free;
    end;
  end;
  Dispose(Ctx);
end;

function ExecStdioProcess(Ctx: PExecCtx; InBuff: Pointer;
  InSize, OutSize: Integer; Output: _ExecOutput): Boolean;

  function ProcessLib(Instance: Integer; Stdin, Stdout: THandle): Boolean;
  const
    BufferSize = 65536;
  var
    Buffer: array [0 .. BufferSize - 1] of Byte;
    BytesRead: DWORD;
    X: Integer;
    LOutSize: Integer;
  begin
    Result := False;
    LOutSize := OutSize;
    try
      FileWriteBuffer(Stdin, InSize, InSize.Size);
      FileWriteBuffer(Stdin, LOutSize, LOutSize.Size);
      FileWriteBuffer(Stdin, InBuff^, InSize);
      FileReadBuffer(Stdout, LOutSize, LOutSize.Size);
      if LOutSize <= 0 then
        exit
      else
      begin
        X := LOutSize;
        while X > 0 do
        begin
          BytesRead := Min(X, Length(Buffer));
          FileReadBuffer(Stdout, Buffer[0], BytesRead);
          Output(Instance, @Buffer[0], BytesRead);
          Dec(X, BytesRead);
        end;
        Result := True;
      end;
    except
    end;
    if not Result then
      with Ctx^ do
      begin
        TerminateProcess(ProcessInfo.hProcess, 0);
        WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
      end;
  end;

const
  PipeSecurityAttributes: TSecurityAttributes =
    (nLength: SizeOf(PipeSecurityAttributes); bInheritHandle: True);
var
  dwExitCode: DWORD;
begin
  Result := False;
  with Ctx^ do
  begin
    if FLib and (WaitForSingleObject(ProcessInfo.hProcess, 0) = WAIT_TIMEOUT)
    then
      Result := ProcessLib(FInstance, hstdinw, hstdoutr)
    else
    begin
      if not Assigned(FTask) then
      begin
        FTask := TTask.Create;
        FTask.Perform(ExecReadTask);
        MTask := TTask.Create(IntPtr(@ProcessInfo.hProcess), IntPtr(@hstdinw),
          IntPtr(@hstdoutr));
        MTask.Perform(ExecMonTask);
      end;
      CreatePipe(hstdinr, hstdinw, @PipeSecurityAttributes, 0);
      CreatePipe(hstdoutr, hstdoutw, @PipeSecurityAttributes, 0);
      SetHandleInformation(hstdinw, HANDLE_FLAG_INHERIT, 0);
      SetHandleInformation(hstdoutr, HANDLE_FLAG_INHERIT, 0);
      ZeroMemory(@StartupInfo, SizeOf(StartupInfo));
      StartupInfo.cb := SizeOf(StartupInfo);
      StartupInfo.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
      StartupInfo.wShowWindow := SW_HIDE;
      StartupInfo.hStdInput := hstdinr;
      StartupInfo.hStdOutput := hstdoutw;
      StartupInfo.hStdError := 0;
      ZeroMemory(@ProcessInfo, SizeOf(ProcessInfo));
      if CreateProcess(nil, PChar('"' + FExecutable + '" ' + FCommandLine), nil,
        nil, True, NORMAL_PRIORITY_CLASS, nil, PChar(FWorkDir), StartupInfo,
        ProcessInfo) then
      begin
        CloseHandleEx(ProcessInfo.hThread);
        CloseHandleEx(hstdinr);
        CloseHandleEx(hstdoutw);
        if FLib then
        begin
          MTask.Start;
          Result := ProcessLib(FInstance, hstdinw, hstdoutr);
        end
        else
        begin
          FTask.Update(FInstance, hstdoutr, NativeInt(@Output));
          FTask.Start;
          try
            FileWriteBuffer(hstdinw, InBuff^, InSize);
          finally
            CloseHandleEx(hstdinw);
            FTask.Wait;
            CloseHandleEx(hstdoutr);
            WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
            GetExitCodeProcess(ProcessInfo.hProcess, dwExitCode);
            CloseHandleEx(ProcessInfo.hProcess);
          end;
          Result := dwExitCode = 0;
        end;
      end
      else
      begin
        CloseHandleEx(hstdinr);
        CloseHandleEx(hstdinw);
        CloseHandleEx(hstdoutr);
        CloseHandleEx(hstdoutw);
      end;
    end;
  end;
end;

procedure ExecOutput1(Instance: Integer; const Buffer: Pointer;
  Size: Integer)cdecl;
begin
  CodecOutput[Instance](Instance, Buffer, Size);
  Inc(CodecSize[Instance], Size);
end;

procedure ExecOutput2(Instance: Integer; const Buffer: Pointer;
  Size: Integer)cdecl;
var
  LBuffer: PByte;
begin
  LBuffer := CodecAllocator[Instance](Instance, CodecSize[Instance] + Size);
  Move(Buffer^, (LBuffer + CodecSize[Instance])^, Size);
  Inc(CodecSize[Instance], Size);
end;

procedure ExecOutput3(Instance: Integer; const Buffer: Pointer;
  Size: Integer)cdecl;
begin
  CodecOutput[Instance](Instance, Buffer, Size);
  Inc(CodecSize[Instance], Size);
end;

function ExeEncode(Index, Instance: Integer; Input: Pointer;
  StreamInfo: PStrInfo2; Output: _PrecompOutput; Funcs: PPrecompFuncs): Boolean;
var
  Buffer: PByte;
  X: Integer;
  Executed: Boolean;
  S, T: String;
  Res: Integer;
begin
  Result := False;
  CodecSize[Instance] := 0;
  CodecOutput[Instance] := Output;
  with CodecExe[Index] do
  begin
    if not DirectoryExists(WorkDir[Instance, 0]) then
      CreateDir(WorkDir[Instance, 0]);
    DeleteFile(WorkDir[Instance, 0] + InFile[0]);
    DeleteFile(WorkDir[Instance, 0] + OutFile[0]);
    S := Param[0];
    S := ReplaceText(S, '<insize>', StreamInfo^.OldSize.ToString);
    S := ReplaceText(S, '<outsize>', StreamInfo^.NewSize.ToString);
    Res := 0;
    if ContainsText(S, '<fileres>') and Funcs^.GetResource(StreamInfo^.Resource,
      nil, @Res) and (Res > 0) then
    begin
      T := StreamInfo^.Resource.ToHexString.ToLower + '.res';
      S := ReplaceText(S, '<fileres>', T);
      S := ReplaceText(S, '<ressize>', Res.ToString);
      T := WorkDir[Instance, 0] + T;
      if not FileExists(T) then
        with TFileStream.Create(T, fmCreate) do
          try
            Buffer := Funcs^.Allocator(Instance, Res);
            if Funcs^.GetResource(StreamInfo^.Resource, Buffer, @Res) then
              WriteBuffer(Buffer^, Res);
          finally
            Free;
          end;
    end;
    case Mode[0] of
      FILE_MODE, STDOUT_MODE:
        begin
          with TFileStream.Create(WorkDir[Instance, 0] + InFile[0], fmCreate) do
            try
              WriteBuffer(Input^, StreamInfo^.OldSize);
            finally
              Free;
            end;
          if Mode[0] = FILE_MODE then
            Executed := PrecompExec(PChar(Exec[0]), PChar(S),
              PChar(WorkDir[Instance, 0]))
          else
            Executed := PrecompExecStdout(Instance, PChar(Exec[0]), PChar(S),
              PChar(WorkDir[Instance, 0]), ExecOutput1);
        end;
    else
      begin
        if Mode[0] = STDIN_MODE then
          Executed := PrecompExecStdin(PChar(Exec[0]), PChar(S),
            PChar(WorkDir[Instance, 0]), Input, StreamInfo^.OldSize)
        else
          Executed := ExecStdioProcess(Ctx[Instance, 0], Input,
            StreamInfo^.OldSize, StreamInfo^.NewSize, ExecOutput1);
      end;
    end;
    if Executed then
    begin
      case Mode[0] of
        FILE_MODE, STDIN_MODE:
          begin
            with TFileStream.Create(WorkDir[Instance, 0] + OutFile[0],
              fmShareDenyNone) do
              try
                X := Read(WrkMem[Instance, 0], E_WORKMEM);
                while X > 0 do
                begin
                  ExecOutput1(Instance, @WrkMem[Instance, 0], X);
                  X := Read(WrkMem[Instance, 0], E_WORKMEM);
                end;
              finally
                Free;
              end;
          end;
      end;
      StreamInfo^.NewSize := CodecSize[Instance];
      Result := True;
    end;
  end;
end;

function ExeDecode(Index, Instance: Integer; Input: Pointer;
  StreamInfo: PStrInfo2; Output: _PrecompOutput; Funcs: PPrecompFuncs): Boolean;
var
  Buffer: PByte;
  X: Integer;
  Executed: Boolean;
  S, T: String;
  Res: Integer;
  LOutput: _ExecOutput;
begin
  Result := False;
  CodecSize[Instance] := 0;
  CodecAllocator[Instance] := Funcs^.Allocator;
  CodecOutput[Instance] := Output;
  if Assigned(Output) then
    LOutput := ExecOutput3
  else
    LOutput := ExecOutput2;
  with CodecExe[Index] do
  begin
    if not DirectoryExists(WorkDir[Instance, 1]) then
      CreateDir(WorkDir[Instance, 1]);
    DeleteFile(WorkDir[Instance, 1] + InFile[1]);
    DeleteFile(WorkDir[Instance, 1] + OutFile[1]);
    S := Param[1];
    S := ReplaceText(S, '<insize>', StreamInfo^.NewSize.ToString);
    S := ReplaceText(S, '<outsize>', StreamInfo^.OldSize.ToString);
    Res := 0;
    if ContainsText(S, '<fileres>') and Funcs^.GetResource(StreamInfo^.Resource,
      nil, @Res) and (Res > 0) then
    begin
      T := StreamInfo^.Resource.ToHexString.ToLower + '.res';
      S := ReplaceText(S, '<fileres>', T);
      S := ReplaceText(S, '<ressize>', Res.ToString);
      T := WorkDir[Instance, 1] + T;
      if not FileExists(T) then
        with TFileStream.Create(T, fmCreate) do
          try
            Buffer := Funcs^.Allocator(Instance, Res);
            if Funcs^.GetResource(StreamInfo^.Resource, Buffer, @Res) then
              WriteBuffer(Buffer^, Res);
          finally
            Free;
          end;
    end;
    case Mode[1] of
      FILE_MODE, STDOUT_MODE:
        begin
          with TFileStream.Create(WorkDir[Instance, 1] + InFile[1], fmCreate) do
            try
              WriteBuffer(Input^, StreamInfo^.NewSize);
            finally
              Free;
            end;
          if Mode[1] = FILE_MODE then
            Executed := PrecompExec(PChar(Exec[1]), PChar(S),
              PChar(WorkDir[Instance, 1]))
          else
            Executed := PrecompExecStdout(Instance, PChar(Exec[1]), PChar(S),
              PChar(WorkDir[Instance, 1]), LOutput);
        end;
    else
      begin
        if Mode[1] = STDIN_MODE then
          Executed := PrecompExecStdin(PChar(Exec[1]), PChar(S),
            PChar(WorkDir[Instance, 1]), Input, StreamInfo^.NewSize)
        else
          Executed := ExecStdioProcess(Ctx[Instance, 1], Input,
            StreamInfo^.NewSize, StreamInfo^.OldSize, LOutput);
      end;
    end;
    if Executed then
    begin
      case Mode[1] of
        FILE_MODE, STDIN_MODE:
          begin
            with TFileStream.Create(WorkDir[Instance, 1] + OutFile[1],
              fmShareDenyNone) do
              try
                X := Read(WrkMem[Instance, 0], E_WORKMEM);
                while X > 0 do
                begin
                  LOutput(Instance, @WrkMem[Instance, 0], X);
                  X := Read(WrkMem[Instance, 0], E_WORKMEM);
                end;
              finally
                Free;
              end;
          end;
      end;
      Result := True;
    end;
  end;
end;

function ExeInit(Command: PChar; Count: Integer; Funcs: PPrecompFuncs): Boolean;
var
  X, Y, Z: Integer;
begin
  Result := True;
  Randomize;
  SetLength(WrkMem, Count);
  SetLength(CodecSize, Count);
  SetLength(CodecOutput, Count);
  SetLength(CodecAllocator, Count);
  for X := Low(CodecExe) to High(CodecExe) do
  begin
    SetLength(CodecExe[X].WorkDir, Count);
    SetLength(CodecExe[X].Ctx, Count);
    for Z := 0 to 1 do
      for Y := Low(CodecSize) to High(CodecSize) do
      begin
        repeat
          CodecExe[X].WorkDir[Y, Z] := IncludeTrailingPathDelimiter
            (IncludeTrailingPathDelimiter(GetCurrentDir) + CodecExe[X].Name +
            '_' + IntToHex(Random($1000000), 4));
        until DirectoryExists(CodecExe[X].WorkDir[Y, Z]) = False;
        IncludeTrailingPathDelimiter(CodecExe[X].WorkDir[Y, Z]);
        if CodecExe[X].Mode[Z] = STDIO_MODE then
          CodecExe[X].Ctx[Y, Z] := ExecStdioInit(Y, PChar(CodecExe[X].Exec[Z]),
            PChar(CodecExe[X].Param[Z]), PChar(CodecExe[X].WorkDir[Y, Z]),
            CodecExe[X].IsLib[Z]);
      end;
  end;
end;

procedure ExeFree(Funcs: PPrecompFuncs);
var
  X, Y, Z: Integer;
begin
  for X := Low(CodecExe) to High(CodecExe) do
    for Z := 0 to 1 do
      for Y := Low(CodecSize) to High(CodecSize) do
      begin
        if CodecExe[X].Mode[Z] = STDIO_MODE then
          ExecStdioFree(CodecExe[X].Ctx[Y, Z]);
        if DirectoryExists(CodecExe[X].WorkDir[Y, Z]) then
          TDirectory.Delete(CodecExe[X].WorkDir[Y, Z], True);
      end;
end;

function ExeParse(Command: PChar; Option: PInteger;
  Funcs: PPrecompFuncs): Boolean;
var
  I: Integer;
begin
  Result := False;
  Option^ := 0;
  for I := Low(CodecExe) to High(CodecExe) do
  begin
    if Funcs^.GetCodec(Command, 0, False) = CodecExe[I].Name then
    begin
      SetBits(Option^, CodecExe[I].ID, 0, 31);
      Result := True;
      break;
    end;
  end;
end;

procedure ExeScan1(Instance, Depth: Integer; Input: PByte;
  Size, SizeEx: NativeInt; Output: _PrecompOutput; Add: _PrecompAdd;
  Funcs: PPrecompFuncs);
begin

end;

function ExeScan2(Instance, Depth: Integer; Input: Pointer; Size: NativeInt;
  StreamInfo: PStrInfo2; Offset: PInteger; Output: _PrecompOutput;
  Funcs: PPrecompFuncs): Boolean;
var
  I: Integer;
  X: Integer;
begin
  Result := False;
  X := -1;
  for I := Low(CodecExe) to High(CodecExe) do
  begin
    if GetBits(CodecExe[I].ID, 0, 31) = GetBits(StreamInfo^.Option, 0, 31) then
    begin
      X := I;
      break;
    end;
  end;
  Result := ExeEncode(X, Instance, Input, StreamInfo, Output, Funcs);
  if Result then
    Funcs^.LogScan2(PChar(Codec.Names[X]), StreamInfo^.OldSize,
      StreamInfo^.NewSize);
end;

function ExeProcess(Instance, Depth: Integer; OldInput, NewInput: Pointer;
  StreamInfo: PStrInfo2; Output: _PrecompOutput; Funcs: PPrecompFuncs): Boolean;
var
  Buffer: PByte;
  I: Integer;
  X: Integer;
  Res1: NativeInt;
  Res2: NativeUInt;
begin
  Result := False;
  X := -1;
  for I := Low(CodecExe) to High(CodecExe) do
  begin
    if GetBits(CodecExe[I].ID, 0, 31) = GetBits(StreamInfo^.Option, 0, 31) then
    begin
      X := I;
      break;
    end;
  end;
  if ExeDecode(X, Instance, NewInput, StreamInfo, nil, Funcs) then
  begin
    Buffer := Funcs^.Allocator(Instance, CodecSize[Instance]);
    Res1 := CodecSize[Instance];
    Result := (Res1 = StreamInfo^.OldSize) and CompareMem(OldInput, Buffer,
      StreamInfo^.OldSize);
    Funcs^.LogProcess(PChar(Codec.Names[X]), nil, StreamInfo^.OldSize,
      StreamInfo^.NewSize, Res1, Result);
    if (Result = False) and (DIFF_TOLERANCE > 0) then
    begin
      Res2 := PrecompEncodePatchEx(Instance, OldInput, StreamInfo^.OldSize,
        Buffer, Res1, Output);
      Funcs^.LogPatch1(StreamInfo^.OldSize, Res1, Res2,
        Funcs^.AcceptPatch(StreamInfo^.OldSize, Res1, Res2));
      if Funcs^.AcceptPatch(StreamInfo^.OldSize, Res1, Res2) then
      begin
        SetBits(StreamInfo^.Option, 1, 31, 1);
        Result := True;
      end;
    end;
  end
  else
    Funcs^.LogProcess(PChar(Codec.Names[X]), nil, StreamInfo^.OldSize,
      StreamInfo^.NewSize, Res1, Result);
end;

function ExeRestore(Instance, Depth: Integer; Input, InputExt: Pointer;
  StreamInfo: _StrInfo3; Output: _PrecompOutput; Funcs: PPrecompFuncs): Boolean;
var
  LOutput: _PrecompOutput;
  Buffer: PByte;
  I: Integer;
  X: Integer;
  Res1: NativeInt;
  Res2: NativeUInt;
  SI: _StrInfo2;
begin
  if GetBits(StreamInfo.Option, 31, 1) = 1 then
    LOutput := nil
  else
    LOutput := Output;
  Result := False;
  X := -1;
  for I := Low(CodecExe) to High(CodecExe) do
  begin
    if GetBits(CodecExe[I].ID, 0, 31) = GetBits(StreamInfo.Option, 0, 31) then
    begin
      X := I;
      break;
    end;
  end;
  SI.OldSize := StreamInfo.OldSize;
  SI.NewSize := StreamInfo.NewSize;
  SI.Resource := StreamInfo.Resource;
  SI.Option := StreamInfo.Option;
  if ExeDecode(X, Instance, Input, @SI, LOutput, Funcs) then
  begin
    Buffer := Funcs^.Allocator(Instance, CodecSize[Instance]);
    Res1 := CodecSize[Instance];
    Funcs^.LogRestore(PChar(Codec.Names[X]), nil, StreamInfo.OldSize,
      StreamInfo.NewSize, Res1, True);
    if GetBits(StreamInfo.Option, 31, 1) = 1 then
    begin
      Res2 := PrecompDecodePatchEx(Instance, InputExt, StreamInfo.ExtSize,
        Buffer, Res1, Output);
      Funcs^.LogPatch2(StreamInfo.OldSize, Res1, StreamInfo.ExtSize, Res2 > 0);
      if Res2 = StreamInfo.OldSize then
        Result := True;
      exit;
    end;
    if Res1 = StreamInfo.OldSize then
      Result := True;
  end
  else
    Funcs^.LogRestore(PChar(Codec.Names[X]), nil, StreamInfo.OldSize,
      StreamInfo.NewSize, Res1, False);
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

var
  A, I, J, K, X: Integer;
  S1, S2, S3: String;
  Bytes: TBytes;
  Ini: TMemIniFile;
  SL: TStringList;
  ExeStruct: TExeStruct;
  Y, Z: Integer;
  List: TStringDynArray;

initialization

S1 := ExpandPath(PluginsPath, True) +
  ChangeFileExt(ExtractFileName(Utils.GetModuleName), '.ini');
if FileExists(S1) then
begin
  Ini := TMemIniFile.Create(S1);
  SL := TStringList.Create;
  Ini.ReadSections(SL);
  for I := 0 to SL.Count - 1 do
  begin
    List := DecodeStr(SL[I], ',');
    if FileExists(ExpandPath(PluginsPath, True) +
      GetCmdStr(Ini.ReadString(SL[I], 'Decode', ''), 0)) then
      for K := Low(List) to High(List) do
      begin
        Insert(List[K], Codec.Names, Length(Codec.Names));
        ExeStruct.Name := List[K];
        Bytes := BytesOf(ExeStruct.Name);
        ExeStruct.ID := Utils.Hash32(0, @Bytes[0], Length(Bytes));
        for X := 0 to 1 do
        begin
          ExeStruct.IsLib[X] := False;
          if X = 0 then
            S1 := Ini.ReadString(SL[I], 'Encode', '')
          else
            S1 := Ini.ReadString(SL[I], 'Decode', '');
          S1 := ReplaceText(S1, '<codec>', List[K]);
          ExeStruct.Exec[X] := ExpandPath(PluginsPath, True) + GetCmdStr(S1, 0);
          ExeStruct.Param[X] := '';
          ExeStruct.Mode[X] := 0;
          for J := 1 to GetCmdCount(S1) - 1 do
          begin
            S2 := GetCmdStr(S1, J);
            if ContainsText(S2, '<library>') then
            begin
              SetBits(ExeStruct.Mode[X], STDIO_MODE, 0, 2);
              ExeStruct.IsLib[X] := True;
              continue;
            end
            else if ContainsText(S2, '<stdin>') or ContainsText(S2, '[stdin]')
            then
            begin
              SetBits(ExeStruct.Mode[X], 1, 0, 1);
              continue;
            end
            else if ContainsText(S2, '<stdout>') or ContainsText(S2, '[stdout]')
            then
            begin
              SetBits(ExeStruct.Mode[X], 1, 1, 1);
              continue;
            end
            else if ContainsText(S2, '<filein>') or ContainsText(S2, '[filein]')
            then
            begin
              S3 := IfThen(X = 0, FILE_IN, FILE_OUT);
              SetBits(ExeStruct.Mode[X], 0, 0, 1);
              if ContainsText(S2, '<filein>') then
                ExeStruct.InFile[X] := ExtractStr('<filein>', S2)
              else
                ExeStruct.InFile[X] := ExtractStr('[filein]', S2);
              S2 := ReplaceText(S2, ExeStruct.InFile[X], S3);
              ExeStruct.InFile[X] := ReplaceText(ExeStruct.InFile[X],
                '<filein>', S3);
              ExeStruct.InFile[X] := ReplaceText(ExeStruct.InFile[X],
                '[filein]', S3);
              S2 := ExeStruct.InFile[X];
              if ContainsText(S2, '[filein]') then
                continue;
            end
            else if ContainsText(S2, '<fileout>') or
              ContainsText(S2, '[fileout]') then
            begin
              S3 := IfThen(X = 0, FILE_OUT, FILE_IN);
              SetBits(ExeStruct.Mode[X], 0, 1, 1);
              if ContainsText(S2, '<fileout>') then
                ExeStruct.OutFile[X] := ExtractStr('<fileout>', S2)
              else
                ExeStruct.OutFile[X] := ExtractStr('[fileout]', S2);
              ExeStruct.OutFile[X] := ReplaceText(ExeStruct.OutFile[X],
                '<fileout>', S3);
              ExeStruct.OutFile[X] := ReplaceText(ExeStruct.OutFile[X],
                '[fileout]', S3);
              S2 := ExeStruct.OutFile[X];
              if ContainsText(S2, '[fileout]') then
                continue;
            end;
            S2 := IfThen((Pos(' ', S2) > 0) or (S2 = ''), '"' + S2 + '"', S2);
            ExeStruct.Param[X] := ExeStruct.Param[X] + ' ' + S2;
          end;
          ExeStruct.Param[X] := Trim(ExeStruct.Param[X]);
        end;
        Insert(ExeStruct, CodecExe, Length(CodecExe));
      end;
  end;
  SL.Free;
  Ini.Free;
end;
Codec.Initialised := False;
Codec.Init := @ExeInit;
Codec.Free := @ExeFree;
Codec.Parse := @ExeParse;
Codec.Scan1 := @ExeScan1;
Codec.Scan2 := @ExeScan2;
Codec.Process := @ExeProcess;
Codec.Restore := @ExeRestore;

finalization

for X := Low(CodecExe) to High(CodecExe) do
  for Z := 0 to 1 do
    for Y := Low(CodecSize) to High(CodecSize) do
      if DirectoryExists(CodecExe[X].WorkDir[Y, Z]) then
        TDirectory.Delete(CodecExe[X].WorkDir[Y, Z], True);

end.
