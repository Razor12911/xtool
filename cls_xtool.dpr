library cls_xtool;

{$R *.res}
{$WEAKLINKRTTI ON}
{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
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
  CLS in 'common\CLS.pas',
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
  PrecompDStorage in 'precompressor\PrecompDStorage.pas';

{$SETPEFLAGS IMAGE_FILE_LARGE_ADDRESS_AWARE}

function ClsMain(operation: Integer; Callback: CLS_CALLBACK; Instance: Pointer)
  : Integer cdecl;
const
  BufferSize = 1048576;
var
  CLS: TCLSStream;
  PrecompEnc: PrecompMain.TEncodeOptions;
  PrecompDec: PrecompMain.TDecodeOptions;
  I: Integer;
  Str: array [0 .. 255] of AnsiChar;
  StrArray: TArray<String>;
begin
  Result := CLS_ERROR_GENERAL;
  case (operation) of
    CLS_COMPRESS:
      begin
        CLS := TCLSStream.Create(Callback, Instance);
        try
          FillChar(Str, Length(Str), 0);
          Callback(Instance, CLS_GET_PARAMSTR, @Str[0], Length(Str));
          StrArray := DecodeStr(String(Str), ':');
          for I := Low(StrArray) to High(StrArray) do
            StrArray[I] := '-' + StrArray[I];
          PrecompMain.Parse(StrArray, PrecompEnc);
          try
            PrecompMain.Encode(CLS, CLS, PrecompEnc);
            Result := CLS_OK;
          except
            Result := CLS_ERROR_GENERAL;
          end;
        finally
          CLS.Free;
        end;
      end;
    CLS_DECOMPRESS:
      begin
        CLS := TCLSStream.Create(Callback, Instance);
        try
          CLS.ReadBuffer(I, I.Size);
          case I of
            XTOOL_PRECOMP:
              begin
                PrecompMain.Parse(['-t100p'], PrecompDec);
                try
                  PrecompMain.Decode(CLS, CLS, PrecompDec);
                  Result := CLS_OK;
                except
                  Result := CLS_ERROR_GENERAL;
                end;
              end;
          end;
        finally
          CLS.Free;
        end;
      end;
  else
    Result := CLS_ERROR_NOT_IMPLEMENTED;
  end;
end;

exports ClsMain;

begin

end.
