unit InitCode;

interface

uses
  Utils, LibImport,
  WinAPI.Windows,
  System.SysUtils, System.Classes, System.Types, System.StrUtils;

const
  PLUGIN_DATABASE = 0;
  PLUGIN_CONFIG = 1;
  PLUGIN_LIBRARY = 2;
  PluginsParam = '-bd';

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

var
  DEBUG: Boolean = False;
  UILib: TLibImport;
  PluginsPath: String = '';
  XTLUI1: procedure;
  XTLUI2: function(Funcs: PUIFuncs; var Params: TArray<String>;
    out LibType: Integer; out LibPath: String): Boolean;
  XTLAddPlugin: procedure(S: String; I: Integer);
  XTLAddCodec: procedure(S: String);
  UIDLLLoaded: Boolean = False;

implementation

procedure Init;
begin
  if Win32MajorVersion < 6 then
    exit;
  UILib := TLibImport.Create;
  UILib.LoadLib(ChangeFileExt(Utils.GetModuleName, 'ui.dll'));
  if UILib.Loaded then
  begin
    @XTLUI1 := UILib.GetProcAddr('XTLUI1');
    @XTLUI2 := UILib.GetProcAddr('XTLUI2');
    @XTLAddPlugin := UILib.GetProcAddr('XTLAddPlugin');
    @XTLAddCodec := UILib.GetProcAddr('XTLAddCodec');
    UIDLLLoaded := Assigned(XTLUI1);
  end;
end;

procedure Deinit;
begin
  if Win32MajorVersion < 6 then
    exit;
  UILib.Free;
end;

var
  I: Integer;

initialization

if not IsLibrary then
begin
  for I := 1 to ParamCount do
  begin
    if (ParamStr(I) = '--debug') then
    begin
      DEBUG := True;
      break;
    end;
  end;
  Init;
  if UIDLLLoaded and (ParamCount = 0) then
    PluginsPath := IncludeTrailingBackSlash
      (ExpandPath(GetIniString('UI', 'Plugins', '',
      ChangeFileExt(Utils.GetModuleName, 'ui.ini'))));
  for I := 1 to ParamCount do
  begin
    if ParamStr(I).StartsWith(PluginsParam) then
    begin
      PluginsPath := ParamStr(I).Substring(PluginsParam.Length);
      break;
    end;
  end;
end;
PluginsPath := IncludeTrailingBackSlash(ExpandPath(PluginsPath));
if not DirectoryExists(ExpandPath(PluginsPath, True)) then
  PluginsPath := ExtractFilePath(Utils.GetModuleName);

finalization

if not IsLibrary then
  Deinit;

end.
