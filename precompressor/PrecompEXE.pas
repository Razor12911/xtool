unit PrecompExe;

interface

uses
  Utils, SynCommons, SynCrypto,
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
    Exec, Param: array [0 .. 1] of String;
    WorkDir: array of array [0 .. 1] of String;
    Mode: array [0 .. 1] of Byte;
    InFile, OutFile: String;
    Continuous: Boolean;
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
            IntToHex(Random($FFFF), 4));
        until DirectoryExists(CodecExe[X].WorkDir[Y, Z]) = False;
        IncludeTrailingBackSlash(CodecExe[X].WorkDir[Y, Z]);
        if CodecExe[X].Mode[Z] = STDIO_MODE then
        begin
          CodecExe[X].Ctx[Y, Z] := PrecompExecStdioInit(Y,
            PChar(CodecExe[X].Exec[Z]), PChar(CodecExe[X].Param[Z]),
            PChar(CodecExe[X].WorkDir[Y, Z]));
        end;
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
        if DirectoryExists(CodecExe[X].WorkDir[Y, Z]) then
          RemoveDir(CodecExe[X].WorkDir[Y, Z]);
        PrecompExecStdioFree(CodecExe[X].Ctx[Y, Z]);
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
      SetBits(Option^, I, 0, 24);
      Result := True;
      break;
    end;
  end;
end;

procedure ExeScan1(Instance, Depth: Integer; Input: PByte;
  Size, SizeEx: NativeInt; Output: _PrecompOutput; Add: _PrecompAdd;
  Funcs: PPrecompFuncs);
begin
  // maybe add feature later...
end;

function ExeScan2(Instance, Depth: Integer; Input: Pointer; Size: NativeInt;
  StreamInfo: PStrInfo2; Output: _PrecompOutput; Funcs: PPrecompFuncs): Boolean;
var
  X, Y: Integer;
  Executed: Boolean;
begin
  Result := False;
  Executed := False;
  X := GetBits(StreamInfo^.Option, 0, 24);
  CodecSize[Instance] := 0;
  CodecOutput[Instance] := Output;
  with CodecExe[X] do
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
          Executed := PrecompExecStdioProcess(Ctx[Instance, 0], Input,
            StreamInfo^.OldSize, ExecOutput1, Continuous);
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
                Y := Read(WrkMem[Instance, 0], E_WORKMEM);
                while Y > 0 do
                begin
                  ExecOutput1(Instance, @WrkMem[Instance, 0], Y);
                  Y := Read(WrkMem[Instance, 0], E_WORKMEM);
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

function ExeProcess(Instance, Depth: Integer; OldInput, NewInput: Pointer;
  StreamInfo: PStrInfo2; Output: _PrecompOutput; Funcs: PPrecompFuncs): Boolean;
var
  Buffer: PByte;
  X, Y: Integer;
  Res1: Integer;
  Res2: NativeUInt;
  Executed: Boolean;
begin
  Result := False;
  X := GetBits(StreamInfo^.Option, 0, 24);
  CodecSize[Instance] := 0;
  CodecAllocator[Instance] := Funcs^.Allocator;
  with CodecExe[X] do
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
              WriteBuffer(NewInput^, StreamInfo^.NewSize);
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
            PChar(WorkDir[Instance, 1]), NewInput, StreamInfo^.NewSize)
        else
          Executed := PrecompExecStdioProcess(Ctx[Instance, 1], NewInput,
            StreamInfo^.NewSize, ExecOutput2, Continuous);
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
                Y := Read(WrkMem[Instance, 0], E_WORKMEM);
                while Y > 0 do
                begin
                  ExecOutput2(Instance, @WrkMem[Instance, 0], Y);
                  Y := Read(WrkMem[Instance, 0], E_WORKMEM);
                end;
              finally
                Free;
              end;
          end;
      end;
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
end;

function ExeRestore(Instance, Depth: Integer; Input, InputExt: Pointer;
  StreamInfo: _StrInfo3; Output: _PrecompOutput; Funcs: PPrecompFuncs): Boolean;
var
  Buffer: PByte;
  X, Y: Integer;
  Res1: Integer;
  Res2: NativeUInt;
  Executed: Boolean;
begin
  Result := False;
  X := GetBits(StreamInfo.Option, 0, 24);
  CodecSize[Instance] := 0;
  CodecAllocator[Instance] := Funcs^.Allocator;
  with CodecExe[X] do
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
          Executed := PrecompExecStdioProcess(Ctx[Instance, 1], Input,
            StreamInfo.NewSize, ExecOutput2, Continuous);
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
                Y := Read(WrkMem[Instance, 0], E_WORKMEM);
                while Y > 0 do
                begin
                  ExecOutput2(Instance, @WrkMem[Instance, 0], Y);
                  Y := Read(WrkMem[Instance, 0], E_WORKMEM);
                end;
              finally
                Free;
              end;
          end;
      end;
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
end;

var
  I, J, X: Integer;
  S1, S2: String;
  Ini: TMemIniFile;
  SL: TStringList;
  ExeStruct: PExeStruct;

initialization

S1 := ChangeFileExt(Utils.GetModuleName, '.ini');
if FileExists(S1) then
begin
  Ini := TMemIniFile.Create(S1);
  SL := TStringList.Create;
  Ini.ReadSections(SL);
  for I := 0 to SL.Count - 1 do
    if FileExists(ExtractFilePath(Utils.GetModuleName) +
      GetCmdStr(Ini.ReadString(SL[I], 'Encode', ''), 0)) then
    begin
      New(ExeStruct);
      Insert(SL[I], Codec.Names, Length(Codec.Names));
      ExeStruct^.Name := SL[I];
      ExeStruct^.Continuous := Ini.ReadBool(SL[I], 'Continous', False);
      for X := 0 to 1 do
      begin
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
          if ContainsText(S2, '<stdin>') then
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

end.
