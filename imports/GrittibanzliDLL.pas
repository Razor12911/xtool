unit GrittibanzliDLL;

interface

uses
  WinAPI.Windows,
  System.SysUtils, System.Classes;

var
  Grittibanzli: function(const src: Pointer; srcSize: Cardinal; dst1: Pointer;
    dst1Capacity: PCardinal; dst2: Pointer; dst2Capacity: PCardinal)
    : boolean cdecl;
  Ungrittibanzli: function(const src1: Pointer; src1Size: Cardinal;
    const src2: Pointer; src2Size: Cardinal; dst: Pointer;
    dstCapacity: PCardinal): boolean cdecl;
  DLLLoaded: boolean = False;

implementation

var
  DLLHandle: THandle;

procedure Init;
begin
  DLLHandle := LoadLibrary(PChar(ExtractFilePath(ParamStr(0)) +
    'grittibanzli_dll.dll'));
  if DLLHandle >= 32 then
  begin
    @Grittibanzli := GetProcAddress(DLLHandle, '__Grittibanzli');
    @Ungrittibanzli := GetProcAddress(DLLHandle, '__Ungrittibanzli');
    DLLLoaded := Assigned(Grittibanzli) and Assigned(Ungrittibanzli);
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
