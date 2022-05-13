{ MIT License

  Copyright (c) 2016-2022 Razor12911

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
{$SETPEOSVERSION 6.0}
{$SETPESUBSYSVERSION 6.0}
{$WEAKLINKRTTI ON}
{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
{$R *.dres}

uses
  WinAPI.Windows,
  System.SysUtils,
  System.StrUtils,
  System.Classes,
  System.Types,
  System.Math,
  System.IOUtils,
  Threading in 'common\Threading.pas',
  Utils in 'common\Utils.pas',
  FuncHook in 'contrib\Delphi_MemoryModule\FuncHook.pas',
  MemoryModule in 'contrib\Delphi_MemoryModule\MemoryModule.pas',
  MemoryModuleHook in 'contrib\Delphi_MemoryModule\MemoryModuleHook.pas',
  SynCommons in 'contrib\mORMot\SynCommons.pas',
  SynCrypto in 'contrib\mORMot\SynCrypto.pas',
  SynLZ in 'contrib\mORMot\SynLZ.pas',
  SynTable in 'contrib\mORMot\SynTable.pas',
  DelphiCL in 'contrib\opencl\DelphiCL.pas',
  OpenCL in 'contrib\opencl\OpenCL.pas',
  oObjects in 'contrib\ParseExpression\oObjects.pas',
  ParseClass in 'contrib\ParseExpression\ParseClass.pas',
  ParseExpr in 'contrib\ParseExpression\ParseExpr.pas',
  GrittibanzliDLL in 'imports\GrittibanzliDLL.pas',
  LZ4DLL in 'imports\LZ4DLL.pas',
  LZODLL in 'imports\LZODLL.pas',
  OodleDLL in 'imports\OodleDLL.pas',
  PreflateDLL in 'imports\PreflateDLL.pas',
  ReflateDLL in 'imports\ReflateDLL.pas',
  XDeltaDLL in 'imports\XDeltaDLL.pas',
  ZLibDLL in 'imports\ZLibDLL.pas',
  ZSTDDLL in 'imports\ZSTDDLL.pas',
  lz4 in 'sources\lz4.pas',
  PrecompMain in 'precompressor\PrecompMain.pas',
  PrecompUtils in 'precompressor\PrecompUtils.pas',
  PrecompCrypto in 'precompressor\PrecompCrypto.pas',
  PrecompZLib in 'precompressor\PrecompZLib.pas',
  PrecompLZ4 in 'precompressor\PrecompLZ4.pas',
  PrecompLZO in 'precompressor\PrecompLZO.pas',
  PrecompZSTD in 'precompressor\PrecompZSTD.pas',
  PrecompOodle in 'precompressor\PrecompOodle.pas',
  PrecompINI in 'precompressor\PrecompINI.pas',
  PrecompSearch in 'precompressor\PrecompSearch.pas',
  PrecompDLL in 'precompressor\PrecompDLL.pas',
  PrecompEXE in 'precompressor\PrecompEXE.pas',
  DbgMain in 'dbgenerator\DbgMain.pas',
  DbgUtils in 'dbgenerator\DbgUtils.pas',
  IOFind in 'io\IOFind.pas',
  IOErase in 'io\IOErase.pas',
  IOReplace in 'io\IOReplace.pas',
  IOPatch in 'io\IOPatch.pas',
  IODecode in 'io\IODecode.pas',
  IOUtils in 'io\IOUtils.pas';

{$SETPEFLAGS IMAGE_FILE_LARGE_ADDRESS_AWARE or IMAGE_FILE_RELOCS_STRIPPED}

const
  CommandPrecomp = 'precomp';
  CommandGenerate = 'generate';
  CommandFind = 'find';
  CommandErase = 'erase';
  CommandReplace = 'replace';
  CommandExtract = 'extract';
  CommandPatch = 'patch';
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
  WriteLine('  ' + CommandErase);
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
  else
    Result := TFileStream.Create(Input, fmShareDenyNone);
end;

function GetOutStream(Output: string): TStream;
begin
  if (Output = '-') or (Output = '') then
    Result := THandleStream.Create(GetStdHandle(STD_OUTPUT_HANDLE))
  else
    Result := TFileStream.Create(Output, fmCreate);
end;

{ changelog
  ES_R32 (0.5.1)
  - added IO functions (find, extract, patch)
  - generate database feature and IO functions now can search for streams larger than chunk size

  ES_R31 (0.5.0)
  - added IO functions (erase, replace)
  - fixed external executable support bugs

  ES_R30 (0.4.8)
  - fixed issue with storing incorrect recompression information when stream patching is performed

  ES_R29 (0.4.7)
  - updated oodle scanner
  - updated external executable support
  - updated configuration based plugin support to add depth information
  - updated verbose mode

  ES_R28 (0.4.6)
  - generate database feature fixed
  - fixed external executable support issues
  - fixed lz4f level setting bug

  ES_R28 (0.4.5)
  - removed leviathan codec restriction

  ES_R27 (0.4.4)
  - fixed issue of lz4 codec loading incorrect library
  - fixed issue with handling endianess via configuration based plugins
  - updated framework of library based plugins

  ES_R26 (0.4.3)
  - added verbose mode
  - added feature that allows you to enforce a different library to be loaded
  - fixed issues related to imperfect stream patching
  - fixed issues with old libraries with missing functions that cause xtool to crash on startup
  - updated oodle codec
  - updated reflate codec
  - updated zstd codec

  ES_R25 (0.4.2)
  - removed debugging code from encryption and executable codec
  - fixed issue with depth when using search codec
  - fixed external executable support issues

  ES_R24 (0.4.1)
  - fixed issue of status not reporting when encoding
  - added depth method support for search support
  - fixed zlib encoding issues for different window bits
  - fixed zlib memory leak issue
  - updated all internal codecs to support information relayed by external codecs
  - updated lz4f codec and removed temporarily removed support for universal scanning
  - added option to change recompression level to be used by reflate
  - updated external executable support
  - generate database feature currently bugged, wait for next update
  - search database structure changed, older database files will no longer work with newer releases

  ES_R23 (0.4.0)
  - project made open source
  - added external executable support
  - added generate database feature
  - fixed search support bug

  ES_R22 (0.3.22)
  - updated search support (speed improvements)
  - updated command line parser
  - added partial universal scanner for lzo1x streams
  - added universal scanner for lz4f streams
  - fixed issue with configuration files failing to execute without conditions

  ES_R21 (0.3.21)
  - updated search support

  ES_R20 (0.3.20)
  - fixed library support bug
  - x86 build discontinued (has bugs from nowhere)

  ES_R19 (0.3.19)
  - updated lzo codec

  ES_R18 (0.3.18)
  - fixed depth bug
  - fixed library plugin bugs

  ES_R17 (0.3.17)
  - fixed multi-threading bug

  ES_R16 (0.3.16)
  - minor bug fixes

  ES_R15 (0.3.15)
  - converted library support to unicode (don't know why I used ansi in the first place)
  - added library support functions
  - added rc4 encryption support

  ES_R14 (0.3.14)
  - fixed library support bug
  - updated library structure

  ES_R13 (0.3.13)
  - updated lz4 codec
  - updated library structure
  - updated depth info functions
  - updated depth feature

  ES_R12 (0.3.12)
  - added depth info functions
  - added support for oodle 2.9.0+ functions
  - fixed data patching bug
  - updated oodle codec
  - updated command line parser

  ES_R11 (0.3.11)
  - fixed x86 build bugs
  - fixed config multi-threading bug
  - fixed resource management bug
  - fixed deduplication bug

  ES_R10 (0.3.10)
  - minor bug fixes
  - added diff tolerance parameter (--diff=)
  - fixed plugin database bug
  - fixed lz4 codec bug
  - updated oodle codec
  - updated library structure
  - added resource management
  - added direct use encryption codecs
  - added embedded deduplication feature (--dedup) [makes temps during encoding]

  ES_R9 (0.3.9)
  - fixed future stream bug

  ES_R8 (0.3.8)
  - fixed command line parser bug
  - updated library support

  ES_R7 (0.3.7)
  - updated library structure

  ES_R6 (0.3.6)
  - updated oodle codec (fixed more lzna bugs)

  ES_R5 (0.3.5)
  - updated oodle codec (fixed lzna bug)
  - added custom method configuration

  ES_R4 (0.3.4)
  - fixed bug depthing

  ES_R3 (0.3.3)
  - updated lz4 codec
  - updated library support

  ES_R2 (0.3.2)
  - improved depthing
  - updated library support
  - fixed zstd codec issues
  - removed fast memory

  ES_R1 (0.3.1)
  - updated library support
  - updated command line parser
  - included x86 build
  - fixed depthing issues

  2012_R2 (0.2.14)
  - added library support
  - added compress, decompress, encrypt, decrypt, hash, delta functions (used by library)
  - added lzo codec placeholders
  - fixed oodle bug
  - fixed lz4 bug
  - removed libdunia codec

  2012_R1 (0.2.13)
  - added oo2ext* dll support
  - updated search support

  2011_R1 (0.2.12)
  - added temporary libdunia codec

  2010_R5 (0.2.11)
  - fixed search/config support bug

  2010_R4 (0.2.10)
  - updated search/config support

  2010_R3 (0.2.9)
  - added database search
  - updated zlib scanner
  - fixed reflate bug
  - fixed 2GB memory limit

  2010_R2 (0.2.8)
  - fixed zstd codec

  2010_R1 (0.2.7)
  - added zstd codec
  - added lz4, lz4hc, lzna, mermaid, selkie, hydra, leviathan codec placeholders
  - added configuration support
  - added xdelta support to handle crc mismatch streams

  2009_R3 (0.2.6)
  - documentation added

  2009_R2 (0.2.5)
  - added kraken codec
  - fixed depthing issues

  2009_R1 (0.2.4)
  - added reflate forced verification
  - updated deflate scanner
  - fixed depthing issues
  - fixed low memory mode issues
  - fixed hanging issues when encoding

  2008_R3 (0.2.3)
  - fixed deduplication memory calculation error
  - added virtual memory support for deduplication
  - added --mem=# parameter to control deduplication memory usage

  2008_R2 (0.2.2)
  - fixed command line parser
  - updated deflate scanner
  - added stream deduplication
  - added stream database
  - added decompression memory limiter
  - added grittibanzli (also handles deflate stream but slow af)

  2008_R1 (0.2.1)
  - initial release

  changelog }

const
  BufferSize = 1048576;

var
  I, J: Integer;
  ParamArg: array [0 .. 1] of TArray<String>;
  IsParam: Boolean;
  Input, Output: TStream;
  PrecompEnc: PrecompMain.TEncodeOptions;
  PrecompDec: PrecompMain.TDecodeOptions;
  GenerateEnc: DbgMain.TEncodeOptions;
  FindEnc: IOFind.TEncodeOptions;
  EraseEnc: IOErase.TEncodeOptions;
  ReplaceEnc: IOReplace.TEncodeOptions;
  PatchEnc: IOPatch.TEncodeOptions;
  PatchDec: IOPatch.TDecodeOptions;
  IODec: IODecode.TDecodeOptions;
  IOExt: IODecode.TExtractOptions;

function ParamArgSafe(I, J: Integer): String;
begin
  Result := '';
  if InRange(I, 0, Pred(Length(ParamArg))) then
    if InRange(J, 0, Pred(Length(ParamArg[I]))) then
      Result := ParamArg[I, J];
end;

begin
  FormatSettings := TFormatSettings.Invariant;
  ProgramInfo;
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
        while Length(ParamArg[1]) < 2 do
          Insert('-', ParamArg[1], Length(ParamArg[1]));
        Input := TBufferedStream.Create(GetInStream(ParamArg[1][0]), True,
          BufferSize);
        Output := TBufferedStream.Create(GetOutStream(ParamArg[1][1]), False,
          BufferSize);
        PrecompMain.Parse(ParamArg[0], PrecompEnc);
        PrecompMain.Encode(Input, Output, PrecompEnc);
        Input.Free;
        Output.Free;
      end;
    if ParamStr(1).StartsWith(CommandGenerate, True) then
      if Length(ParamArg[1]) < 3 then
        DbgMain.PrintHelp
      else
      begin
        DbgMain.Parse(ParamArg[0], GenerateEnc);
        DbgMain.Encode(ParamArg[1][0], ParamArg[1][1], ParamArg[1][2],
          GenerateEnc);
      end;
    if ParamStr(1).StartsWith(CommandFind, True) then
      if Length(ParamArg[1]) < 2 then
        IOFind.PrintHelp
      else
      begin
        IOFind.Parse(ParamArg[0], FindEnc);
        IOFind.Encode(ParamArg[1][0], ParamArg[1][1],
          ParamArgSafe(1, 2), FindEnc);
      end;
    if ParamStr(1).StartsWith(CommandErase, True) then
      if Length(ParamArg[1]) < 2 then
        IOErase.PrintHelp
      else
      begin
        IOErase.Parse(ParamArg[0], EraseEnc);
        IOErase.Encode(ParamArg[1][0], ParamArg[1][1], ParamArgSafe(1, 2),
          EraseEnc);
      end;
    if ParamStr(1).StartsWith(CommandReplace, True) then
      if Length(ParamArg[1]) < 3 then
        IOReplace.PrintHelp
      else
      begin
        IOReplace.Parse(ParamArg[0], ReplaceEnc);
        IOReplace.Encode(ParamArg[1][0], ParamArg[1][1], ParamArg[1][2],
          ParamArgSafe(1, 3), ReplaceEnc);
      end;
    if ParamStr(1).StartsWith(CommandExtract, True) then
      if (Length(ParamArg[0]) = 0) and (Length(ParamArg[1]) = 0) then
        IODecode.PrintHelpExtract
      else
      begin
        Input := TBufferedStream.Create(GetInStream(ParamArgSafe(1, 0)), True,
          BufferSize);
        Input.ReadBuffer(I, I.Size);
        case I of
          XTOOL_IODEC:
            begin
              IODecode.ParseExtract(ParamArg[0], IOExt);
              IODecode.Extract(Input, ParamArgSafe(1, 1),
                ParamArgSafe(1, 2), IOExt);
            end;
        end;
        Input.Free;
      end;
    if ParamStr(1).StartsWith(CommandPatch, True) then
      if Length(ParamArg[1]) < 3 then
        IOPatch.PrintHelp
      else
      begin
        Output := TBufferedStream.Create(GetOutStream(ParamArg[1][2]), False,
          BufferSize);
        IOPatch.Parse(ParamArg[0], PatchEnc);
        IOPatch.Encode(ParamArg[1][0], ParamArg[1][1], Output, PatchEnc);
        Input.Free;
        Output.Free;
      end;
    if ParamStr(1).StartsWith(CommandDecode, True) then
      if (Length(ParamArg[0]) = 0) and (Length(ParamArg[1]) = 0) then
        DecodePrintHelp
      else
      begin
        Input := TBufferedStream.Create(GetInStream(ParamArgSafe(1, 0)), True,
          BufferSize);
        Input.ReadBuffer(I, I.Size);
        case I of
          XTOOL_PRECOMP:
            begin
              Output := TBufferedStream.Create(GetOutStream(ParamArgSafe(1, 1)),
                False, BufferSize);
              PrecompMain.Parse(ParamArg[0], PrecompDec);
              PrecompMain.Decode(Input, Output, PrecompDec);
              Output.Free;
            end;
          XTOOL_IODEC:
            begin
              IODecode.ParseDecode(ParamArg[0], IODec);
              IODecode.Decode(Input, ParamArg[1][1], ParamArg[1][2], IODec);
            end;
          XTOOL_PATCH:
            begin
              IOPatch.Parse(ParamArg[0], PatchDec);
              IOPatch.Decode(Input, ParamArg[1][1], PatchDec);
            end;
        end;
        Input.Free;
      end;
  except
    on E: Exception do
    begin
      WriteLine(E.ClassName + ': ' + E.Message);
      ExitCode := 1;
    end;
  end;

end.
