unit UIMain;

interface

uses
  InitCode,
  LibImport,
  WinAPI.Windows,
  System.SysUtils;

var
  UIFuncs: TUIFuncs;

implementation

uses
  BrunsliDLL, FLACDLL, FLZMA2DLL, JoJpegDLL, LZ4DLL, LZODLL, OodleDLL,
  PackJPGDLL, PreflateDLL, ReflateDLL, ZLibDLL, ZSTDDLL,
  Utils;

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
UIFuncs.IsLZMALoaded := FLZMA2DLL.DLLLoaded;
UIFuncs.IsSrepAvailable :=
  FileExists(ExpandPath(PluginsPath + 'srep.exe', True))
{$IFDEF CPU64BITS} or FileExists(ExpandPath(PluginsPath + 'srep64.exe',
  True)){$ENDIF};

end.
