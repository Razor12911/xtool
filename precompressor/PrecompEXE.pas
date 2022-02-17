unit PrecompExe;

interface

uses
  Utils, Threading,
  SynCommons, SynCrypto,
  PrecompUtils,
  WinAPI.Windows,
  System.SysUtils, System.Classes, System.StrUtils,
  System.Types, System.Math, System.IOUtils, System.IniFiles;

const
  FILE_IN = 'data.in';
  FILE_OUT = 'data.out';
  FILE_RES = 'data.res';
  FILE_STORE = 'data.tmp';
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
    InFile, OutFile: String;
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
    FTask := TTask.Create;
    FTask.Perform(ExecReadTask);
    MTask := TTask.Create(IntPtr(@ProcessInfo.hProcess), IntPtr(@hstdinw),
      IntPtr(@hstdoutr));
    MTask.Perform(ExecMonTask);
    ZeroMemory(@ProcessInfo, SizeOf(ProcessInfo));
  end;
end;

procedure ExecStdioFree(Ctx: PExecCtx);
begin
  with Ctx^ do
  begin
    TerminateProcess(ProcessInfo.hProcess, 0);
    WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
    FTask.Free;
    MTask.Wait;
    MTask.Free;
  end;
  Dispose(Ctx);
end;

function ExecStdioProcess(Ctx: PExecCtx; InBuff: Pointer; InSize: Integer;
  Output: _ExecOutput): Boolean;

  function ProcessLib(Instance: Integer; Stdin, Stdout: THandle): Boolean;
  const
    BufferSize = 65536;
  var
    Buffer: array [0 .. BufferSize - 1] of Byte;
    BytesRead: DWORD;
    OutSize: Integer;
  begin
    Result := False;
    try
      FileWriteBuffer(Stdin, InSize, InSize.Size);
      FileWriteBuffer(Stdin, InBuff^, InSize);
      FileReadBuffer(Stdout, OutSize, OutSize.Size);
      if OutSize <= 0 then
        exit
      else
      begin
        while OutSize > 0 do
        begin
          BytesRead := Min(OutSize, Length(Buffer));
          FileReadBuffer(Stdout, Buffer[0], BytesRead);
          Output(Instance, @Buffer[0], BytesRead);
          Dec(OutSize, BytesRead);
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
          Result := ProcessLib(FInstance, hstdinw, hstdoutr)
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
          end;
          Result := GetExitCodeProcess(ProcessInfo.hProcess, dwExitCode) and
            (dwExitCode = 0);
          CloseHandleEx(ProcessInfo.hProcess);
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

function ExeEncode(Index, Instance: Integer; Input: Pointer;
  StreamInfo: PStrInfo2; Output: _PrecompOutput; Funcs: PPrecompFuncs): Boolean;
var
  X: Integer;
  Executed: Boolean;
begin
  Result := False;
  CodecSize[Instance] := 0;
  CodecOutput[Instance] := Output;
  with CodecExe[Index] do
  begin
    if not DirectoryExists(WorkDir[Instance, 0]) then
      CreateDir(WorkDir[Instance, 0]);
    DeleteFile(WorkDir[Instance, 0] + InFile);
    DeleteFile(WorkDir[Instance, 0] + OutFile);
    case Mode[0] of
      FILE_MODE, STDOUT_MODE:
        begin
          with TFileStream.Create(WorkDir[Instance, 0] + InFile, fmCreate) do
            try
              WriteBuffer(Input^, StreamInfo^.OldSize);
            finally
              Free;
            end;
          if Mode[0] = FILE_MODE then
            Executed := PrecompExec(PChar(Exec[0]), PChar(Param[0]),
              PChar(WorkDir[Instance, 0]))
          else
            Executed := PrecompExecStdout(Instance, PChar(Exec[0]),
              PChar(Param[0]), PChar(WorkDir[Instance, 0]), ExecOutput1);
        end;
    else
      begin
        if Mode[0] = STDIN_MODE then
          Executed := PrecompExecStdin(PChar(Exec[0]), PChar(Param[0]),
            PChar(WorkDir[Instance, 0]), Input, StreamInfo^.OldSize)
        else
          Executed := ExecStdioProcess(Ctx[Instance, 0], Input,
            StreamInfo^.OldSize, ExecOutput1);
      end;
    end;
    if Executed then
    begin
      case Mode[0] of
        FILE_MODE, STDIN_MODE:
          begin
            with TFileStream.Create(WorkDir[Instance, 0] + OutFile,
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
  StreamInfo: _StrInfo2; Funcs: PPrecompFuncs): Boolean;
var
  X: Integer;
  Executed: Boolean;
begin
  Result := False;
  CodecSize[Instance] := 0;
  CodecAllocator[Instance] := Funcs^.Allocator;
  with CodecExe[Index] do
  begin
    if not DirectoryExists(WorkDir[Instance, 1]) then
      CreateDir(WorkDir[Instance, 1]);
    DeleteFile(WorkDir[Instance, 1] + InFile);
    DeleteFile(WorkDir[Instance, 1] + OutFile);
    case Mode[1] of
      FILE_MODE, STDOUT_MODE:
        begin
          with TFileStream.Create(WorkDir[Instance, 1] + OutFile, fmCreate) do
            try
              WriteBuffer(Input^, StreamInfo.NewSize);
            finally
              Free;
            end;
          if Mode[1] = FILE_MODE then
            Executed := PrecompExec(PChar(Exec[1]), PChar(Param[1]),
              PChar(WorkDir[Instance, 1]))
          else
            Executed := PrecompExecStdout(Instance, PChar(Exec[1]),
              PChar(Param[1]), PChar(WorkDir[Instance, 1]), ExecOutput2);
        end;
    else
      begin
        if Mode[1] = STDIN_MODE then
          Executed := PrecompExecStdin(PChar(Exec[1]), PChar(Param[1]),
            PChar(WorkDir[Instance, 1]), Input, StreamInfo.NewSize)
        else
          Executed := ExecStdioProcess(Ctx[Instance, 1], Input,
            StreamInfo.NewSize, ExecOutput2);
      end;
    end;
    if Executed then
    begin
      case Mode[1] of
        FILE_MODE, STDIN_MODE:
          begin
            with TFileStream.Create(WorkDir[Instance, 1] + InFile,
              fmShareDenyNone) do
              try
                X := Read(WrkMem[Instance, 0], E_WORKMEM);
                while X > 0 do
                begin
                  ExecOutput2(Instance, @WrkMem[Instance, 0], X);
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
          CodecExe[X].WorkDir[Y, Z] := IncludeTrailingBackSlash
            (IncludeTrailingBackSlash(GetCurrentDir) + CodecExe[X].Name + '_' +
            IntToHex(Random($10000), 4));
        until DirectoryExists(CodecExe[X].WorkDir[Y, Z]) = False;
        IncludeTrailingBackSlash(CodecExe[X].WorkDir[Y, Z]);
        if CodecExe[X].Mode[Z] = STDIO_MODE then
          CodecExe[X].Ctx[Y, Z] := ExecStdioInit(Y, PChar(CodecExe[X].Exec[Z]),
            PChar(CodecExe[X].Param[Z]), PChar(CodecExe[X].WorkDir[Y, Z]),
            CodecExe[X].IsLib[Z]);
      end;
    AddMethod(CodecExe[X].Name);
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
          RemoveDir(CodecExe[X].WorkDir[Y, Z]);
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
var
  Buffer: PByte;
  X, Y: Integer;
  SI1: _StrInfo1;
  SI2: _StrInfo2;
  DI1, DI2: TDepthInfo;
  DS: TPrecompCmd;
begin
  DI1 := Funcs^.GetDepthInfo(Instance);
  DS := Funcs^.GetCodec(DI1.Codec, 0, False);
  if DS <> '' then
  begin
    X := IndexText(DS, Codec.Names);
    if (X < 0) or (DI1.OldSize <> SizeEx) then
      exit;
    SI2.OldSize := SizeEx;
    SI2.NewSize := 0;
    if ExeEncode(X, Instance, Input, @SI2, Output, Funcs) then
    begin
      Output(Instance, Buffer, Y);
      SI1.Position := 0;
      SI1.OldSize := DI1.OldSize;
      SI1.NewSize := Y;
      ShowMessage(SI1.OldSize.ToString + ' >> ' + SI1.NewSize.ToString);
      SetBits(SI1.Option, CodecExe[X].ID, 0, 31);
      if System.Pos(SPrecompSep2, DI1.Codec) > 0 then
        SI1.Status := TStreamStatus.Predicted
      else
        SI1.Status := TStreamStatus.None;
      DI2.Codec := Funcs^.GetDepthCodec(DI1.Codec);
      DI2.OldSize := SI1.NewSize;
      DI2.NewSize := SI1.NewSize;
      Add(Instance, @SI1, DI1.Codec, @DI2);
    end;
    exit;
  end;
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
end;

function ExeProcess(Instance, Depth: Integer; OldInput, NewInput: Pointer;
  StreamInfo: PStrInfo2; Output: _PrecompOutput; Funcs: PPrecompFuncs): Boolean;
var
  Buffer: PByte;
  I: Integer;
  X: Integer;
  Res1: Integer;
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
  if ExeDecode(X, Instance, NewInput, StreamInfo^, Funcs) then
  begin
    Buffer := Funcs^.Allocator(Instance, CodecSize[Instance]);
    Res1 := CodecSize[Instance];
    Result := (Res1 = StreamInfo^.OldSize) and CompareMem(OldInput, Buffer,
      StreamInfo^.OldSize);
    if Result = False then
    begin
      Buffer := Funcs^.Allocator(Instance,
        Res1 + Max(StreamInfo^.OldSize, Res1));
      Res2 := PrecompEncodePatch(OldInput, StreamInfo^.OldSize, Buffer, Res1,
        Buffer + Res1, Max(StreamInfo^.OldSize, Res1));
      if (Res2 > 0) and ((Res2 / Max(StreamInfo^.OldSize, Res1)) <=
        DIFF_TOLERANCE) then
      begin
        Output(Instance, Buffer + Res1, Res2);
        SetBits(StreamInfo^.Option, 1, 31, 1);
        Result := True;
      end;
    end;
  end;
end;

function ExeRestore(Instance, Depth: Integer; Input, InputExt: Pointer;
  StreamInfo: _StrInfo3; Output: _PrecompOutput; Funcs: PPrecompFuncs): Boolean;
var
  Buffer: PByte;
  I: Integer;
  X: Integer;
  Res1: Integer;
  Res2: NativeUInt;
  SI: _StrInfo2;
begin
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
  if ExeDecode(X, Instance, Input, SI, Funcs) then
  begin
    Buffer := Funcs^.Allocator(Instance, CodecSize[Instance]);
    Res1 := CodecSize[Instance];
    if GetBits(StreamInfo.Option, 31, 1) = 1 then
    begin
      Buffer := Funcs^.Allocator(Instance, Res1 + StreamInfo.OldSize);
      Res2 := PrecompDecodePatch(InputExt, StreamInfo.ExtSize, Buffer, Res1,
        Buffer + Res1, StreamInfo.OldSize);
      if Res2 > 0 then
      begin
        Output(Instance, Buffer + Res1, StreamInfo.OldSize);
        Result := True;
      end;
      exit;
    end;
    if Res1 = StreamInfo.OldSize then
    begin
      Output(Instance, Buffer, StreamInfo.OldSize);
      Result := True;
    end;
  end;
end;

var
  I, J, X: Integer;
  S1, S2: String;
  Bytes: TBytes;
  Ini: TMemIniFile;
  SL: TStringList;
  ExeStruct: PExeStruct;
  Y, Z: Integer;

initialization

S1 := ChangeFileExt(Utils.GetModuleName, '.ini');
if FileExists(S1) then
begin
  Ini := TMemIniFile.Create(S1);
  SL := TStringList.Create;
  Ini.ReadSections(SL);
  for I := 0 to SL.Count - 1 do
    if FileExists(ExtractFilePath(Utils.GetModuleName) +
      GetCmdStr(Ini.ReadString(SL[I], 'Decode', ''), 0)) then
    begin
      New(ExeStruct);
      Insert(SL[I], Codec.Names, Length(Codec.Names));
      ExeStruct^.Name := SL[I];
      Bytes := BytesOf(ExeStruct^.Name);
      ExeStruct^.ID := Utils.Hash32(0, @Bytes[0], Length(Bytes));
      for X := 0 to 1 do
      begin
        ExeStruct^.IsLib[X] := False;
        if X = 0 then
          S1 := Ini.ReadString(SL[I], 'Encode', '')
        else
          S1 := Ini.ReadString(SL[I], 'Decode', '');
        ExeStruct^.Exec[X] := ExtractFilePath(Utils.GetModuleName) +
          GetCmdStr(S1, 0);
        ExeStruct^.Param[X] := '';
        ExeStruct^.Mode[X] := 0;
        for J := 1 to GetCmdCount(S1) do
        begin
          S2 := GetCmdStr(S1, J);
          if ContainsText(S2, '<library>') then
          begin
            SetBits(ExeStruct^.Mode[X], STDIO_MODE, 0, 2);
            ExeStruct^.IsLib[X] := True;
            continue;
          end
          else if ContainsText(S2, '<stdin>') then
          begin
            SetBits(ExeStruct^.Mode[X], 1, 0, 1);
            continue;
          end
          else if ContainsText(S2, '<stdout>') then
          begin
            SetBits(ExeStruct^.Mode[X], 1, 1, 1);
            continue;
          end
          else if ContainsText(S2, '<filein>') or ContainsText(S2, '[filein]')
          then
          begin
            SetBits(ExeStruct^.Mode[X], 0, 0, 1);
            ExeStruct^.InFile := S2;
            ExeStruct^.InFile := ReplaceStr(ExeStruct^.InFile,
              '<filein>', FILE_IN);
            ExeStruct^.InFile := ReplaceStr(ExeStruct^.InFile,
              '[filein]', FILE_IN);
            if ContainsText(S2, '[filein]') then
              continue;
          end
          else if ContainsText(S2, '<fileout>') or ContainsText(S2, '[fileout]')
          then
          begin
            SetBits(ExeStruct^.Mode[X], 0, 1, 1);
            ExeStruct^.OutFile := S2;
            ExeStruct^.OutFile := ReplaceStr(ExeStruct^.OutFile, '<fileout>',
              FILE_OUT);
            ExeStruct^.OutFile := ReplaceStr(ExeStruct^.OutFile, '[fileout]',
              FILE_OUT);
            if ContainsText(S2, '[fileout]') then
              continue;
          end;
          S2 := IfThen(Pos(' ', S2) > 0, '"' + S2 + '"', S2);
          ExeStruct^.Param[X] := ExeStruct^.Param[X] + ' ' + S2;
        end;
        ExeStruct^.Param[X] := Trim(ExeStruct^.Param[X]);
      end;
      Insert(ExeStruct^, CodecExe, Length(CodecExe));
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
        RemoveDir(CodecExe[X].WorkDir[Y, Z]);

end.
