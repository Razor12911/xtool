{ MIT License

  Copyright (c) 2016-2023 Razor12911

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE. }

program xtool;

{$APPTYPE CONSOLE}
{$R *.res}
{$WEAKLINKRTTI ON}
{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
{$POINTERMATH ON}
{$DEFINE UseFastMM}

uses
{$IFDEF UseFastMM}
  FastMM4 in 'contrib\FastMM4-AVX\FastMM4.pas',
  FastMM4Messages in 'contrib\FastMM4-AVX\FastMM4Messages.pas',
{$ENDIF }
  WinAPI.Windows,
  System.SysUtils,
  System.StrUtils,
  System.Classes,
  System.Types,
  System.Math,
  System.IOUtils,
  System.SyncObjs,
  DStorage in 'common\DStorage.pas',
  LibImport in 'common\LibImport.pas',
  Threading in 'common\Threading.pas',
  Utils in 'common\Utils.pas',
  libc in 'contrib\LIBC\libc.pas',
  lz4lib in 'contrib\LZ4Delphi\lz4lib.pas',
  FuncHook in 'contrib\Delphi_MemoryModule\FuncHook.pas',
  MemoryModule in 'contrib\Delphi_MemoryModule\MemoryModule.pas',
  MemoryModuleHook in 'contrib\Delphi_MemoryModule\MemoryModuleHook.pas',
  SynCommons in 'contrib\mORMot\SynCommons.pas',
  SynCrypto in 'contrib\mORMot\SynCrypto.pas',
  SynLZ in 'contrib\mORMot\SynLZ.pas',
  SynTable in 'contrib\mORMot\SynTable.pas',
  oObjects in 'contrib\ParseExpression\oObjects.pas',
  ParseClass in 'contrib\ParseExpression\ParseClass.pas',
  ParseExpr in 'contrib\ParseExpression\ParseExpr.pas',
  XXHASHLIB in 'contrib\XXHASH4Delphi\XXHASHLIB.pas',
  ZSTDLib in 'contrib\ZSTD4Delphi\ZSTDLib.pas',
  InitCode in 'InitCode.pas',
  BrunsliDLL in 'imports\BrunsliDLL.pas',
  FLACDLL in 'imports\FLACDLL.pas',
  FLZMA2DLL in 'imports\FLZMA2DLL.pas',
  JoJpegDLL in 'imports\JoJpegDLL.pas',
  LZ4DLL in 'imports\LZ4DLL.pas',
  LZODLL in 'imports\LZODLL.pas',
  OodleDLL in 'imports\OodleDLL.pas',
  PackJPGDLL in 'imports\PackJPGDLL.pas',
  PreflateDLL in 'imports\PreflateDLL.pas',
  ReflateDLL in 'imports\ReflateDLL.pas',
  ZLibDLL in 'imports\ZLibDLL.pas',
  ZSTDDLL in 'imports\ZSTDDLL.pas',
  lz4 in 'sources\lz4.pas',
  UIMain in 'ui\UIMain.pas',
  PrecompMain in 'precompressor\PrecompMain.pas',
  PrecompUtils in 'precompressor\PrecompUtils.pas',
  PrecompCrypto in 'precompressor\PrecompCrypto.pas',
  PrecompZLib in 'precompressor\PrecompZLib.pas',
  PrecompLZ4 in 'precompressor\PrecompLZ4.pas',
  PrecompLZO in 'precompressor\PrecompLZO.pas',
  PrecompZSTD in 'precompressor\PrecompZSTD.pas',
  PrecompMedia in 'precompressor\PrecompMedia.pas',
  PrecompOodle in 'precompressor\PrecompOodle.pas',
  PrecompINI in 'precompressor\PrecompINI.pas',
  PrecompINIEx in 'precompressor\PrecompINIEx.pas',
  PrecompSearch in 'precompressor\PrecompSearch.pas',
  PrecompDLL in 'precompressor\PrecompDLL.pas',
  PrecompEXE in 'precompressor\PrecompEXE.pas',
  PrecompDStorage in 'precompressor\PrecompDStorage.pas',
  DbgMain in 'dbgenerator\DbgMain.pas',
  DbgUtils in 'dbgenerator\DbgUtils.pas',
  IOFind in 'io\IOFind.pas',
  IOErase in 'io\IOErase.pas',
  IOReplace in 'io\IOReplace.pas',
  IOExecute in 'io\IOExecute.pas',
  IODecode in 'io\IODecode.pas',
  IOUtils in 'io\IOUtils.pas';

{$SETPEFLAGS IMAGE_FILE_LARGE_ADDRESS_AWARE or IMAGE_FILE_RELOCS_STRIPPED or IMAGE_FILE_DEBUG_STRIPPED}

const
  CommandPrecomp = 'precomp';
  CommandGenerate = 'generate';
  CommandFind = 'find';
  CommandErase = 'erase';
  CommandReplace = 'replace';
  CommandExtract = 'extract';
  CommandPatch = 'patch';
  CommandArchive = 'archive';
  CommandExecute = 'execute';
  CommandDecode = 'decode';

procedure ProgramInfo;
begin
  WriteLine('XTool is created by Razor12911');
  WriteLine('');
end;

procedure ListCommands;
begin
  WriteLine('Available commands:');
  WriteLine('');
  WriteLine('  ' + CommandDecode);
  WriteLine('  ' + CommandArchive);
  WriteLine('  ' + CommandErase);
  WriteLine('  ' + CommandExecute);
  WriteLine('  ' + CommandExtract);
  WriteLine('  ' + CommandFind);
  WriteLine('  ' + CommandGenerate);
  WriteLine('  ' + CommandPatch);
  WriteLine('  ' + CommandPrecomp);
  WriteLine('  ' + CommandReplace);
  WriteLine('');
  WriteLine('Launch program with command to see its usage');
  WriteLine('');
end;

procedure DecodePrintHelp;
begin
  WriteLine('decode - restores data processed by xtool');
  WriteLine('');
  WriteLine('Usage:');
  WriteLine('  xtool decode input [decode_data] output');
  WriteLine('');
  WriteLine('Parameters:');
  WriteLine('  t# - number of working threads [Threads/2]');
  WriteLine('');
end;

function GetInStream(Input: string): TStream;
begin
  if (Input = '-') or (Input = '') then
    Result := THandleStream.Create(GetStdHandle(STD_INPUT_HANDLE))
  else if Pos('://', Input) > 0 then
    Result := TDownloadStream.Create(Input)
  else if FileExists(Input) then
    Result := TFileStream.Create(Input, fmShareDenyNone)
  else
    Result := TDirInputStream.Create(Input);
end;

function GetOutStream(Output: string): TStream;
begin
  if (Output = '') then
    Result := TNullStream.Create
  else if DirectoryExists(Output) then
    Result := TDirOutputStream.Create(Output)
  else if (Output = '-') then
    Result := THandleStream.Create(GetStdHandle(STD_OUTPUT_HANDLE))
  else
    Result := TFileStream.Create(Output, fmCreate);
end;

function CheckInstance(const InstanceName: string): boolean;
var
  Sem: THandle;
begin
  Result := False;
  Sem := CreateSemaphore(nil, 0, 1, PChar(InstanceName));
  if ((Sem <> 0) and (GetLastError = ERROR_ALREADY_EXISTS)) then
  begin
    CloseHandle(Sem);
    exit(True);
  end;
end;

function Exec_(Executable, CommandLine, WorkDir: string): boolean;
var
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  dwExitCode: DWORD;
  LWorkDir: PChar;
begin
  Result := False;
  FillChar(StartupInfo, sizeof(StartupInfo), #0);
  StartupInfo.cb := sizeof(StartupInfo);
  if WorkDir <> '' then
    LWorkDir := Pointer(WorkDir)
  else
    LWorkDir := Pointer(GetCurrentDir);
  if CreateProcess(nil, PChar('"' + Executable + '" ' + CommandLine), nil, nil,
    False, 0, nil, LWorkDir, StartupInfo, ProcessInfo) then
  begin
    CloseHandleEx(ProcessInfo.hThread);
    WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
    GetExitCodeProcess(ProcessInfo.hProcess, dwExitCode);
    CloseHandleEx(ProcessInfo.hProcess);
    Result := dwExitCode = 0;
  end
  else
    RaiseLastOSError;
end;

const
  BufferSize = 4194034;

var
  I, J: Integer;
  S: String;
  LibType: Integer;
  LibPath: String;
  LibList: System.Types.TStringDynArray;
  ParamStr_: TArray<String>;
  ParamArg: array [0 .. 1] of TArray<String>;
  StrArray: TArray<String>;
  IsParam: boolean;
  Input, Output: TStream;
  PrecompEnc: PrecompMain.TEncodeOptions;
  PrecompDec: PrecompMain.TDecodeOptions;
  GenerateEnc: DbgMain.TEncodeOptions;
  FindEnc: IOFind.TEncodeOptions;
  EraseEnc: IOErase.TEncodeOptions;
  ReplaceEnc: IOReplace.TEncodeOptions;
  ExecuteEnc: IOExecute.TEncodeOptions;
  ExecuteDec: IOExecute.TDecodeOptions;
  IODec: IODecode.TDecodeOptions;
  IOExt: IODecode.TExtractOptions;

function ParamArgSafe(I, J: Integer): String;
begin
  Result := '';
  if InRange(I, 0, Pred(Length(ParamArg))) then
    if InRange(J, 0, Pred(Length(ParamArg[I]))) then
      Result := ParamArg[I, J];
end;

function PrecompEncodePatch2(Instance: Integer; OldBuff: Pointer;
  OldSize: Integer; NewBuff: Pointer; NewSize: Integer;
  Output: _PrecompOutput): Integer;

  function highbit64(V: UInt64): Cardinal;
  var
    Count: Cardinal;
  begin
    Count := 0;
    Assert(V <> 0);
    V := V shr 1;
    while V <> 0 do
    begin
      V := V shr 1;
      Inc(Count);
    end;
    Result := Count;
  end;

var
  Buffer: Pointer;
  BufferSize: Integer;
  Ctx: ZSTD_CCtx;
  Inp: ZSTDLib.ZSTD_inBuffer;
  Oup: ZSTDLib.ZSTD_outBuffer;
  Res: NativeInt;

  procedure DoWrite;
  begin
    Output(Instance, Buffer, Oup.Pos);
    Inc(Result, Oup.Pos);
    Oup.dst := Buffer;
    Oup.Size := BufferSize;
    Oup.Pos := 0;
  end;

begin
  Result := 0;
  BufferSize := ZSTDLib.ZSTD_CStreamOutSize;
  GetMem(Buffer, BufferSize);
  Ctx := ZSTDLib.ZSTD_createCCtx;
  try
    Oup.dst := Buffer;
    Oup.Size := BufferSize;
    Oup.Pos := 0;
    Inp.src := OldBuff;
    Inp.Size := OldSize;
    Inp.Pos := 0;
    ZSTDLib.ZSTD_initCStream(Ctx, 1);
    ZSTDLib.ZSTD_CCtx_setParameter(Ctx,
      ZSTDLib.ZSTD_cParameter.ZSTD_c_windowLog, highbit64(NewSize) + 1);
    ZSTDLib.ZSTD_CCtx_setParameter(Ctx,
      ZSTDLib.ZSTD_cParameter.ZSTD_c_enableLongDistanceMatching, 1);
    ZSTDLib.ZSTD_CCtx_refPrefix(Ctx, NewBuff, NewSize);
    while Inp.Pos < Inp.Size do
    begin
      Res := ZSTDLib.ZSTD_compressStream(Ctx, Oup, Inp);
      if Res < 0 then
        exit(0)
      else if Res > 0 then
        DoWrite
      else
        break;
    end;
    ZSTDLib.ZSTD_flushStream(Ctx, Oup);
    DoWrite;
    ZSTDLib.ZSTD_endStream(Ctx, Oup);
    DoWrite;
  finally
    ZSTDLib.ZSTD_freeCCtx(Ctx);
  end;
end;

function PrecompDecodePatch2(Instance: Integer; PatchBuff: Pointer;
  PatchSize: Integer; OldBuff: Pointer; OldSize: Integer;
  Output: _PrecompOutput): Integer;

  function highbit64(V: UInt64): Cardinal;
  var
    Count: Cardinal;
  begin
    Count := 0;
    Assert(V <> 0);
    V := V shr 1;
    while V <> 0 do
    begin
      V := V shr 1;
      Inc(Count);
    end;
    Result := Count;
  end;

var
  Buffer: Pointer;
  BufferSize: Integer;
  Ctx: ZSTD_DCtx;
  Inp: ZSTDLib.ZSTD_inBuffer;
  Oup: ZSTDLib.ZSTD_outBuffer;
  Res: NativeInt;

  procedure DoWrite;
  begin
    Output(Instance, Buffer, Oup.Pos);
    Inc(Result, Oup.Pos);
    Oup.dst := Buffer;
    Oup.Size := BufferSize;
    Oup.Pos := 0;
  end;

begin
  Result := 0;
  BufferSize := ZSTDLib.ZSTD_DStreamOutSize;
  GetMem(Buffer, BufferSize);
  Ctx := ZSTDLib.ZSTD_createDCtx;
  try
    Oup.dst := Buffer;
    Oup.Size := BufferSize;
    Oup.Pos := 0;
    Inp.src := PatchBuff;
    Inp.Size := PatchSize;
    Inp.Pos := 0;
    ZSTDLib.ZSTD_initDStream(Ctx);
    ZSTDLib.ZSTD_DCtx_setParameter(Ctx,
      ZSTDLib.ZSTD_dParameter.ZSTD_d_windowLogMax, highbit64(OldSize) + 1);
    ZSTDLib.ZSTD_DCtx_refPrefix(Ctx, OldBuff, OldSize);
    while Inp.Pos < Inp.Size do
    begin
      Res := ZSTDLib.ZSTD_decompressStream(Ctx, Oup, Inp);
      if Res < 0 then
        exit(0)
      else if Res > 0 then
        DoWrite
      else
        break;
    end;
  finally
    ZSTDLib.ZSTD_freeDCtx(Ctx);
  end;
end;

var
  MS1, MS2, MS3, MS4: TMemoryStream;

procedure ExecOutput1(Instance: Integer; const Buffer: Pointer;
  Size: Integer)cdecl;
begin
  MS3.WriteBuffer(Buffer^, Size);
end;

procedure ExecOutput2(Instance: Integer; const Buffer: Pointer;
  Size: Integer)cdecl;
begin
  MS4.WriteBuffer(Buffer^, Size);
end;

begin
  { try
    MS1 := TMemoryStream.Create;
    MS2 := TMemoryStream.Create;
    MS3 := TMemoryStream.Create;
    MS4 := TMemoryStream.Create;
    MS1.LoadFromFile('UI.sb');
    MS2.LoadFromFile('UI.sb.out');
    MS4.Size := MS1.Size;
    MS3.Size := PrecompEncodePatch2(0, MS1.Memory, MS1.Size, MS2.Memory,
    MS2.Size, ExecOutput1);
    MS3.SaveToFile('UI.sb.diff');
    MS4.Size := PrecompDecodePatch2(0, MS3.Memory, MS3.Size, MS2.Memory,
    MS2.Size, ExecOutput2);
    MS4.SaveToFile('UI.sb.res');
    except
    on E: Exception do
    ShowMessage(E.Message);
    end;
    ShowMessage('');
    exit; }
  FormatSettings := TFormatSettings.Invariant;
  if not CheckInstance('XToolUI_Check') then
    ProgramInfo;
  if InitCode.UIDLLLoaded and (ParamCount = 0) then
  begin
    XTLUI1;
    while XTLUI2(@UIFuncs, ParamStr_, LibType, LibPath) do
    begin
      S := '';
      for I := 1 to High(ParamStr_) do
        S := S + IfThen(ParamStr_[I].Contains(' '), '"' + ParamStr_[I] + '"',
          ParamStr_[I]) + ' ';
      if LibType = 0 then
        Exec_(ParamStr(0), S, '')
      else
      begin
        LibList := TDirectory.GetFiles(LibPath, '*.dll',
          TSearchOption.soAllDirectories);
        for J := Low(LibList) to High(LibList) do
        begin
          S := '';
          for I := 1 to High(ParamStr_) do
          begin
            S := S + IfThen(ParamStr_[I].Contains(' '),
              '"' + ParamStr_[I] + '"', ParamStr_[I]) + ' ';
            if I = 1 then
              case LibType of
                1:
                  S := S + '"' + '-lz4' + LibList[J] + '"' + ' ';
                2:
                  S := S + '"' + '-zstd' + LibList[J] + '"' + ' ';
                3:
                  S := S + '"' + '-oodle' + LibList[J] + '"' + ' ';
              end;
          end;
          WriteLine('Library loaded: ' + ReplaceText(LibList[J],
            IncludeTrailingPathDelimiter(LibPath), ''));
          WriteLine('');
          Exec_(ParamStr(0), S, '');
        end;
      end;
      WriteLine('Done!!!');
      WriteLine('');
    end;
    exit;
  end;
  try
    if ParamCount = 0 then
    begin
      ListCommands;
      exit;
    end;
    IsParam := True;
    for I := 2 to ParamCount do
    begin
      if (IsParam = True) and (FileExists(ParamStr(I)) = False) and
        (DirectoryExists(ParamStr(I)) = False) and (Pos('://', ParamStr(I)) = 0)
        and (Pos('*', ParamStr(I)) = 0) and (ParamStr(I) <> '-') then
        J := 0
      else
      begin
        J := 1;
        IsParam := False;
      end;
      Insert(ParamStr(I), ParamArg[J], Length(ParamArg[J]));
    end;
    if ParamStr(1).StartsWith(CommandPrecomp, True) then
      if (Length(ParamArg[0]) = 0) and (Length(ParamArg[1]) = 0) then
        PrecompMain.PrintHelp
      else
      begin
        Input := TBufferedStream.Create(GetInStream(ParamArgSafe(1, 0)), True,
          BufferSize);
        Output := TBufferedStream.Create(GetOutStream(ParamArgSafe(1, 1)),
          False, BufferSize);
        try
          PrecompMain.Parse(ParamArg[0], PrecompEnc);
          PrecompMain.Encode(Input, Output, PrecompEnc);
        finally
          Input.Free;
          Output.Free;
        end;
      end;
    if ParamStr(1).StartsWith(CommandGenerate, True) then
      if Length(ParamArg[1]) < 3 then
        DbgMain.PrintHelp
      else
      begin
        DbgMain.Parse(ParamArg[0], GenerateEnc);
        DbgMain.Encode(ParamArg[1, 0], ParamArg[1, 1], ParamArg[1, 2],
          GenerateEnc);
      end;
    if ParamStr(1).StartsWith(CommandFind, True) then
      if Length(ParamArg[1]) < 2 then
        IOFind.PrintHelp
      else
      begin
        IOFind.Parse(ParamArg[0], FindEnc);
        IOFind.Encode(ParamArg[1, 0], ParamArg[1, 1],
          ParamArgSafe(1, 2), FindEnc);
      end;
    if ParamStr(1).StartsWith(CommandErase, True) then
      if Length(ParamArg[1]) < 2 then
        IOErase.PrintHelp
      else
      begin
        IOErase.Parse(ParamArg[0], EraseEnc);
        IOErase.Encode(ParamArg[1, 0], ParamArg[1, 1], ParamArgSafe(1, 2),
          EraseEnc);
      end;
    if ParamStr(1).StartsWith(CommandReplace, True) then
      if Length(ParamArg[1]) < 3 then
        IOReplace.PrintHelp
      else
      begin
        IOReplace.Parse(ParamArg[0], ReplaceEnc);
        IOReplace.Encode(ParamArg[1, 0], ParamArg[1, 1], ParamArg[1, 2],
          ParamArgSafe(1, 3), ReplaceEnc);
      end;
    if ParamStr(1).StartsWith(CommandExtract, True) then
      if (Length(ParamArg[0]) = 0) and (Length(ParamArg[1]) = 0) then
        IODecode.PrintHelpExtract
      else
      begin
        Input := TBufferedStream.Create(GetInStream(ParamArgSafe(1, 0)), True,
          BufferSize);
        try
          Input.ReadBuffer(I, I.Size);
          case I of
            XTOOL_IODEC:
              begin
                IODecode.ParseExtract(ParamArg[0], IOExt);
                IODecode.Extract(Input, ParamArgSafe(1, 1),
                  ParamArgSafe(1, 2), IOExt);
              end;
          end;
        finally
          Input.Free;
        end;
      end;
    if ParamStr(1).StartsWith(CommandExecute, True) then
      if (Length(ParamArg[0]) = 0) and (Length(ParamArg[1]) = 0) then
        IOExecute.PrintHelp
      else
      begin
        SetLength(StrArray, 0);
        for I := 2 to High(ParamArg[1]) do
          Insert(ParamArg[1, I], StrArray, Length(StrArray));
        Input := TBufferedStream.Create(GetInStream(ParamArg[1, 0]), True,
          BufferSize);
        Output := TBufferedStream.Create(GetOutStream(ParamArg[1, 1]), False,
          BufferSize);
        try
          IOExecute.Parse(ParamArg[0], ExecuteEnc);
          IOExecute.Encode(Input, Output, StrArray, ExecuteEnc);
        finally
          Input.Free;
          Output.Free;
        end;
      end;
    if ParamStr(1).StartsWith(CommandDecode, True) then
      if (Length(ParamArg[0]) = 0) and (Length(ParamArg[1]) = 0) then
        DecodePrintHelp
      else
      begin
        Input := TBufferedStream.Create(GetInStream(ParamArgSafe(1, 0)), True,
          BufferSize);
        try
          Input.ReadBuffer(I, I.Size);
          case I of
            XTOOL_PRECOMP:
              begin
                Output := TBufferedStream.Create(GetOutStream(ParamArgSafe(1, 1)
                  ), False, BufferSize);
                try
                  PrecompMain.Parse(ParamArg[0], PrecompDec);
                  PrecompMain.Decode(Input, Output, PrecompDec);
                finally
                  Output.Free;
                end;
              end;
            XTOOL_IODEC:
              begin
                IODecode.ParseDecode(ParamArg[0], IODec);
                IODecode.Decode(Input, ParamArg[1, 1], ParamArg[1, 2], IODec);
              end;
            XTOOL_EXEC:
              begin
                SetLength(StrArray, 0);
                for I := 2 to High(ParamArg[1]) do
                  Insert(ParamArg[1, I], StrArray, Length(StrArray));
                Output := TBufferedStream.Create(GetOutStream(ParamArgSafe(1, 1)
                  ), False, BufferSize);
                try
                  IOExecute.Parse(ParamArg[0], ExecuteDec);
                  IOExecute.Decode(Input, Output, StrArray, ExecuteDec);
                finally
                  Output.Free;
                end;
              end;
          end;
        finally
          Input.Free;
        end;
      end;
  except
    on E: Exception do
    begin
      WriteLine(E.ClassName + ': ' + E.Message);
      if DEBUG then
        ShowMessage(E.ClassName + ': ' + E.Message);
      ExitCode := 1;
    end;
  end;

end.
