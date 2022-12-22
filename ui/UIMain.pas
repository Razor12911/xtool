unit UIMain;

interface

uses
  LibImport,
  WinAPI.Windows,
  System.SysUtils, System.Classes, System.Types;

type
  PUIFuncs = ^TUIFuncs;

  TUIFuncs = record
    IsZlibLoaded: Boolean;
    IsReflateLoaded: Boolean;
    IsPreflateLoaded: Boolean;
    IsLZ4Loaded: Boolean;
    IsLZOLoaded: Boolean;
    IsZSTDLoaded: Boolean;
    IsOodleLoaded: Boolean;
    IsFLACLoaded: Boolean;
    IsBrunsliLoaded: Boolean;
    IsPackJPGLoaded: Boolean;
    IsJoJpegLoaded: Boolean;
    IsXDeltaLoaded: Boolean;
    IsLZMALoaded: Boolean;
    IsSrepAvailable: Boolean;
  end;

const
  PLUGIN_DATABASE = 0;
  PLUGIN_CONFIG = 1;
  PLUGIN_LIBRARY = 2;

var
  UIFuncs: TUIFuncs;
  XTLUI1: procedure;
  XTLUI2: function(Funcs: PUIFuncs; var Params: TArray<String>;
    out LibType: Integer; out LibPath: String): Boolean;
  XTLAddPlugin: procedure(S: String; I: Integer);
  XTLAddCodec: procedure(S: String);
  DLLLoaded: Boolean = False;

implementation

uses
  BrunsliDLL, FLACDLL, FLZMA2DLL, JoJpegDLL, LZ4DLL, LZODLL, OodleDLL,
  PackJPGDLL, PreflateDLL, ReflateDLL, XDeltaDLL, ZLibDLL, ZSTDDLL,
  Utils;

var
  Lib: TLibImport;

procedure Init;
begin
  Lib := TLibImport.Create(ChangeFileExt(Utils.GetModuleName, 'ui.dll'));
  if Lib.Loaded then
  begin
    @XTLUI1 := Lib.GetProcAddr('XTLUI1');
    @XTLUI2 := Lib.GetProcAddr('XTLUI2');
    @XTLAddPlugin := Lib.GetProcAddr('XTLAddPlugin');
    @XTLAddCodec := Lib.GetProcAddr('XTLAddCodec');
    DLLLoaded := Assigned(XTLUI1);
  end;
end;

procedure Deinit;
begin
  Lib.Free;
end;

initialization

UIFuncs.IsZlibLoaded := ZLibDLL.DLLLoaded;
UIFuncs.IsReflateLoaded := ReflateDLL.DLLLoaded;
UIFuncs.IsPreflateLoaded := PreflateDLL.DLLLoaded;
UIFuncs.IsLZ4Loaded := LZ4DLL.DLLLoaded;
UIFuncs.IsLZOLoaded := LZODLL.DLLLoaded;
UIFuncs.IsZSTDLoaded := ZSTDDLL.DLLLoaded;
UIFuncs.IsOodleLoaded := OodleDLL.DLLLoaded;
UIFuncs.IsFLACLoaded := FLACDLL.DLLLoaded;
UIFuncs.IsBrunsliLoaded := BrunsliDLL.DLLLoaded;
UIFuncs.IsPackJPGLoaded := PackJPGDLL.DLLLoaded;
UIFuncs.IsJoJpegLoaded := JoJpegDLL.DLLLoaded;
UIFuncs.IsXDeltaLoaded := XDeltaDLL.DLLLoaded;
UIFuncs.IsLZMALoaded := FLZMA2DLL.DLLLoaded;
UIFuncs.IsSrepAvailable := FileExists(ExtractFilePath(Utils.GetModuleName) +
  'srep.exe');
Init;

finalization

Deinit;

end.
