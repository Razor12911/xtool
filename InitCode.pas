unit InitCode;

interface

uses
  Utils, LibImport,
  System.SysUtils;

const
  PluginsParam1 = '--basedir=';
  PluginsParam2 = '-bd';

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
  UILib := TLibImport.Create(ChangeFileExt(Utils.GetModuleName, 'ui.dll'));
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
  UILib.Free;
end;

var
  I: Integer;

initialization

Init;
if UIDLLLoaded and (ParamCount = 0) then
  PluginsPath := IncludeTrailingBackSlash
    (ExpandPath(GetIniString('UI', 'Plugins', '',
    ChangeFileExt(Utils.GetModuleName, 'ui.ini'))));
for I := 1 to ParamCount do
begin
  if ParamStr(I).StartsWith(PluginsParam1) then
  begin
    PluginsPath := ParamStr(I).Substring(PluginsParam1.Length);
    break;
  end;
  if ParamStr(I).StartsWith(PluginsParam2) then
  begin
    PluginsPath := ParamStr(I).Substring(PluginsParam2.Length);
    break;
  end;
end;
PluginsPath := IncludeTrailingBackSlash(ExpandPath(PluginsPath));

finalization

Deinit;

end.
