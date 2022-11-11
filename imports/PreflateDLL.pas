unit PreflateDLL;

interface

uses
  LibImport,
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
  Lib: TLibImport;

procedure Init;
begin
  Lib := TLibImport.Create(ExtractFilePath(ParamStr(0)) + 'preflate_dll.dll');
  if Lib.Loaded then
  begin
    @preflate_decode := Lib.GetProcAddr('decode');
    @preflate_reencode := Lib.GetProcAddr('reencode');
    DLLLoaded := Assigned(preflate_decode) and Assigned(preflate_reencode);
  end;
end;

procedure Deinit;
begin
  Lib.Free;
end;

initialization

Init;

finalization

Deinit;

end.
