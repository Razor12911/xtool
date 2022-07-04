unit TTADLL;

interface

uses
  WinAPI.Windows,
  System.SysUtils, System.Classes;

var
  tta_encode: function(const src: Pointer; srcSize: PInteger; dst: Pointer;
    dstCapacity: PInteger): Boolean cdecl;
  tta_decode: function(const src: Pointer; srcSize: Integer; dst: Pointer;
    dstCapacity: PInteger): Boolean cdecl;
  tta_getsize: function(const src: Pointer; srcSize: Integer;
    headerSize: PInteger): Integer cdecl;
  DLLLoaded: Boolean = False;

implementation

var
  DLLHandle: THandle;

procedure Init;
begin
  DLLHandle := LoadLibrary(PChar(ExtractFilePath(ParamStr(0)) +
    'libtta_dll.dll'));
  if DLLHandle >= 32 then
  begin
    @tta_encode := GetProcAddress(DLLHandle, 'encode');
    @tta_decode := GetProcAddress(DLLHandle, 'decode');
    @tta_getsize := GetProcAddress(DLLHandle, 'getsize');
    DLLLoaded := Assigned(tta_encode) and Assigned(tta_decode);
  end
  else
    DLLLoaded := False;
end;

procedure Deinit;
begin
  if not DLLLoaded then
    exit;
  FreeLibrary(DLLHandle);
end;

initialization

Init;

finalization

Deinit;

end.
