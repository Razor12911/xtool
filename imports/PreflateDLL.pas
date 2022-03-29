unit PreflateDLL;

interface

uses
  WinAPI.Windows,
  System.SysUtils, System.Classes;

var
  preflate_decode: function(const src: Pointer; srcSize: integer; dst1: Pointer;
    dst1Capacity: PInteger; dst2: Pointer; dst2Capacity: PInteger)
    : boolean cdecl;
  preflate_reencode: function(const src1: Pointer; src1Size: integer;
    const src2: Pointer; src2Size: integer; dst: Pointer; dstCapacity: PInteger)
    : boolean cdecl;
  DLLLoaded: boolean = False;

implementation

var
  DLLHandle: THandle;

procedure Init;
begin
  DLLHandle := LoadLibrary(PChar(ExtractFilePath(ParamStr(0)) +
    'preflate_dll.dll'));
  if DLLHandle >= 32 then
  begin
    @preflate_decode := GetProcAddress(DLLHandle, 'decode');
    @preflate_reencode := GetProcAddress(DLLHandle, 'reencode');
    DLLLoaded := Assigned(preflate_decode) and Assigned(preflate_reencode);
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
