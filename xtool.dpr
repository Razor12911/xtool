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
{$SETPEFLAGS 1}
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
  System.IOUtils,
  Threading in 'common\Threading.pas',
  Utils in 'common\Utils.pas',
  BDiffDecoder in 'contrib\bdiff\BDiffDecoder.pas',
  BDiffEncoder in 'contrib\bdiff\BDiffEncoder.pas',
  UAppInfo in 'contrib\bdiff\UAppInfo.pas',
  UBaseParams in 'contrib\bdiff\UBaseParams.pas',
  UBDiffInfoWriter in 'contrib\bdiff\UBDiffInfoWriter.pas',
  UBDiffMain in 'contrib\bdiff\UBDiffMain.pas',
  UBDiffParams in 'contrib\bdiff\UBDiffParams.pas',
  UBDiffTypes in 'contrib\bdiff\UBDiffTypes.pas',
  UBDiffUtils in 'contrib\bdiff\UBDiffUtils.pas',
  UBlockSort in 'contrib\bdiff\UBlockSort.pas',
  UBPatchInfoWriter in 'contrib\bdiff\UBPatchInfoWriter.pas',
  UBPatchMain in 'contrib\bdiff\UBPatchMain.pas',
  UBPatchParams in 'contrib\bdiff\UBPatchParams.pas',
  UBPatchUtils in 'contrib\bdiff\UBPatchUtils.pas',
  UDiffer in 'contrib\bdiff\UDiffer.pas',
  UErrors in 'contrib\bdiff\UErrors.pas',
  UFileData in 'contrib\bdiff\UFileData.pas',
  UInfoWriter in 'contrib\bdiff\UInfoWriter.pas',
  ULogger in 'contrib\bdiff\ULogger.pas',
  UPatcher in 'contrib\bdiff\UPatcher.pas',
  UPatchWriters in 'contrib\bdiff\UPatchWriters.pas',
  UUtils in 'contrib\bdiff\UUtils.pas',
  FuncHook in 'contrib\Delphi_MemoryModule\FuncHook.pas',
  MemoryModule in 'contrib\Delphi_MemoryModule\MemoryModule.pas',
  MemoryModuleHook in 'contrib\Delphi_MemoryModule\MemoryModuleHook.pas',
  flcHash in 'contrib\fundamentals\Utils\flcHash.pas',
  flcHugeInt in 'contrib\fundamentals\Utils\flcHugeInt.pas',
  flcInteger in 'contrib\fundamentals\Utils\flcInteger.pas',
  flcRandom in 'contrib\fundamentals\Utils\flcRandom.pas',
  flcStdTypes in 'contrib\fundamentals\Utils\flcStdTypes.pas',
  flcSysUtils in 'contrib\fundamentals\Utils\flcSysUtils.pas',
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
  DbgLog in 'dbgenerator\DbgLog.pas',
  DbgSearch in 'dbgenerator\DbgSearch.pas',
  DbgUtils in 'dbgenerator\DbgUtils.pas';

// {$SETPEFLAGS IMAGE_FILE_LARGE_ADDRESS_AWARE}
{$SETPEFLAGS IMAGE_FILE_LARGE_ADDRESS_AWARE or IMAGE_FILE_RELOCS_STRIPPED}
{ ----------------------------------------------------- }
// Load data into memory
{ ----------------------------------------------------- }

{ archive
  combine
  decode
  extract
  insert
  patch
  precomp
  pipe
  restore
  split
  scan }

const
  CommandPrecomp = 'precomp';
  CommandGenerate = 'generate';
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
  { WriteLn(ErrOutput,'  archive');
    WriteLn(ErrOutput,'  combine'); }
  WriteLine('  ' + CommandDecode);
  WriteLine('  ' + CommandGenerate);
  { WriteLn(ErrOutput,'  extract');
    WriteLn(ErrOutput,'  insert');
    WriteLn(ErrOutput,'  patch'); }
  WriteLine('  ' + CommandPrecomp);
  { WriteLn(ErrOutput,'  restore');
    WriteLn(ErrOutput,'  split'); }
  WriteLine('');
  WriteLine('Launch program with command to see its usage');
  WriteLine('');
end;

procedure DecodePrintHelp;
begin
  WriteLine('decode - restores data processed by xtool');
  WriteLine('');
  WriteLine('Usage:');
  WriteLine('  xtool decode input output');
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

(* type
  XMEMCODEC_TYPE = (XMEMCODEC_DEFAULT = 0, XMEMCODEC_LZX = 1);

  XMEMCODEC_PARAMETERS_LZX = record
  public
  Flags: Integer;
  WindowSize: Integer;
  CompressionPartitionSize: Integer;
  end;

  PXMEMCODEC_PARAMETERS_LZX = ^XMEMCODEC_PARAMETERS_LZX;

  XMEMCOMPRESSION_CONTEXT = IntPtr;
  XMEMDECOMPRESSION_CONTEXT = IntPtr;

  PIntPtr = ^IntPtr;

  function XMemCreateCompressionContext(CodecType: XMEMCODEC_TYPE;
  pCodecParams: PXMEMCODEC_PARAMETERS_LZX; Flags: Integer; pContext: PIntPtr)
  : Integer stdcall; external 'xcompress.dll';
  function XMemCompress(Context: IntPtr; pDestination: Pointer;
  pDestSize: PInteger; pSource: Pointer; SrcSize: Integer): Integer stdcall;
  external 'xcompress.dll';
  function XMemDestroyCompressionContext(pContext: IntPtr): Integer stdcall;
  external 'xcompress.dll';
  function XMemCreateDecompressionContext(CodecType: XMEMCODEC_TYPE;
  pCodecParams: PXMEMCODEC_PARAMETERS_LZX; Flags: Integer; pContext: PIntPtr)
  : Integer stdcall; external 'xcompress.dll';
  function XMemDecompress(Context: IntPtr; pDestination: Pointer;
  pDestSize: PInteger; pSource: Pointer; SrcSize: Integer): Integer stdcall;
  external 'xcompress.dll';
  function XMemDestroyDecompressionContext(pContext: IntPtr): Integer stdcall;
  external 'xcompress.dll';

  type
  xcompress_native_p = ^xcompress_native_t;

  xcompress_native_t = packed record
  Identifier: UInt32;
  Version: UInt16;
  Reserved: UInt16;
  ContextFlags: UInt32;
  Flags: UInt32;
  WindowSize: UInt32;
  CompressionPartitionSize: UInt32;
  UncompressedSize: UInt64;
  CompressedSize: UInt64;
  UncompressedBlockSize: UInt32;
  CompressedBlockSize: UInt32;
  procedure CorrectEndian(out data: xcompress_native_t);
  {  var
  BigEndian: Boolean;
  procedure CorrectEndian;  }
  end;

  procedure xcompress_native_t.CorrectEndian(out data: xcompress_native_t);
  begin
  if Identifier = $EE12F50F then
  end; *)

(*
  //#define XCOMPRESS_FILE_IDENTIFIER_LZXTDECODE        0x0FF512ED
  #pragma pack(2)
  typedef struct {
  u32     Identifier;
  u16     Version;
  u16     Reserved;
  u32     CRC_Hash;
  u32     Flags;
  } xcompress_decode_t;
  #pragma pack()

  //#define XCOMPRESS_FILE_IDENTIFIER_LZXNATIVE         0x0FF512EE
  #pragma pack(2)
  typedef struct {
  u32     Identifier;
  u16     Version;
  u16     Reserved;
  u32     ContextFlags;
  u32   Flags;
  u32   WindowSize;
  u32   CompressionPartitionSize;
  u32     UncompressedSizeHigh;
  u32     UncompressedSizeLow;
  u32     CompressedSizeHigh;
  u32     CompressedSizeLow;
  u32     UncompressedBlockSize;
  u32     CompressedBlockSizeMax;
  } xcompress_native_t;
  #pragma pack()
*)

(* function xmem_compress(inbuf: Pointer; insz: Integer; outbuf: Pointer;
  outsz: Integer): Integer;
  var
  ctx: XMEMCOMPRESSION_CONTEXT;
  param: XMEMCODEC_PARAMETERS_LZX;
  ret: SIZE_T;
  hr: HRESULT;
  begin
  Result := 0;
  param.Flags := 0;
  param.WindowSize := 32 * 1024;
  param.CompressionPartitionSize := 32 * 1024;
  { param.WindowSize := 128 * 1024;
  param.CompressionPartitionSize := 256 * 1024; }
  hr := XMemCreateCompressionContext(XMEMCODEC_DEFAULT, @param, 0, @ctx);
  ret := outsz;
  hr := XMemCompress(ctx, outbuf, @ret, inbuf, insz);
  if hr = 0 then
  Result := ret;
  XMemDestroyCompressionContext(ctx);
  end;

  function xmem_decompress(inbuf: Pointer; insz: Integer; outbuf: Pointer;
  outsz: Integer): Integer;
  var
  ctx: XMEMDECOMPRESSION_CONTEXT;
  param: XMEMCODEC_PARAMETERS_LZX;
  ret: SIZE_T;
  hr: HRESULT;
  begin
  Result := 0;
  param.Flags := 0;
  param.WindowSize := 32 * 1024;
  param.CompressionPartitionSize := 32 * 1024;
  hr := XMemCreateDecompressionContext(XMEMCODEC_DEFAULT, @param, 0, @ctx);
  ret := outsz;
  hr := XMemDecompress(ctx, outbuf, @ret, inbuf, insz);
  if hr = 0 then
  Result := ret;
  XMemDestroyDecompressionContext(ctx);
  end; *)

{ function decode2(src: Pointer; src_size: Integer; dst: Pointer;
  dst_size: Integer): Integer cdecl; external 'libdunia.dll'; }

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
  // MS1, MS2, MS3: TMemoryStream;

begin
  { MS1 := TMemoryStream.Create;
    MS2 := TMemoryStream.Create;
    MS3 := TMemoryStream.Create;
    MS1.LoadFromFile('Untitled3');
    MS2.Size := 32948;
    decode2(MS1.Memory, MS1.Size, MS2.Memory, MS2.Size);
    MS2.SaveToFile('Untitled3.out');
    MS3.Size := MS2.Size;
    MS3.Size := LZ4_compress_HC(MS2.Memory, MS3.Memory, MS2.Size, MS3.Size, 3);
    MS3.SaveToFile('Untitled3.res');
    exit; }
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
      if (IsParam = True) and (FileExists(ParamStr(I)) = false) and
        (DirectoryExists(ParamStr(I)) = false) and (Pos('://', ParamStr(I)) = 0)
        and (Pos('*', ParamStr(I)) = 0) and (ParamStr(I) <> '-') then
        J := 0
      else
      begin
        J := 1;
        IsParam := false;
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
        Output := TBufferedStream.Create(GetOutStream(ParamArg[1][1]), false,
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
    if ParamStr(1).StartsWith(CommandDecode, True) then
      if (Length(ParamArg[0]) = 0) and (Length(ParamArg[1]) = 0) then
        DecodePrintHelp
      else
      begin
        while Length(ParamArg[1]) < 2 do
          Insert('-', ParamArg[1], Length(ParamArg[1]));
        Input := TBufferedStream.Create(GetInStream(ParamArg[1][0]), True,
          BufferSize);
        Input.ReadBuffer(I, I.Size);
        case I of
          XTOOL_PRECOMP:
            begin
              Output := TBufferedStream.Create(GetOutStream(ParamArg[1][1]),
                false, BufferSize);
              PrecompMain.Parse(ParamArg[0], PrecompDec);
              PrecompMain.decode(Input, Output, PrecompDec);
              Output.Free;
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
