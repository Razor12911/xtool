unit ReflateDLL;

interface

uses
  WinAPI.Windows,
  System.SysUtils, System.Classes;

var
  raw2hif_Alloc: function: Pointer stdcall;
  raw2hif_Free: procedure(p: Pointer)stdcall;
  raw2hif_Init: procedure(p: Pointer; level: integer)stdcall;
  raw2hif_Loop: function(p: Pointer): integer stdcall;
  raw2hif_getoutlen: function(p: Pointer): integer stdcall;
  raw2hif_getou2len: function(p: Pointer): integer stdcall;
  raw2hif_addbuf: procedure(p: Pointer; buf: Pointer; bufsize: integer)stdcall;
  hif2raw_Alloc: function: Pointer stdcall;
  hif2raw_Free: procedure(p: Pointer)stdcall;
  hif2raw_Init: procedure(p: Pointer; level: integer)stdcall;
  hif2raw_Loop: function(p: Pointer): integer stdcall;
  hif2raw_getoutlen: function(p: Pointer): integer stdcall;
  hif2raw_addbuf: procedure(p: Pointer; buf: Pointer; bufsize: integer)stdcall;
  DLLLoaded: boolean = False;

implementation

var
  DLLHandle1, DLLHandle2: THandle;

procedure Init;
begin
  DLLHandle1 := LoadLibrary(PChar(ExtractFilePath(ParamStr(0)) +
    'RAW2HIF_DLL.DLL'));
  DLLHandle2 := LoadLibrary(PChar(ExtractFilePath(ParamStr(0)) +
    'HIF2RAW_DLL.DLL'));
  if (DLLHandle1 >= 32) and (DLLHandle2 >= 32) then
  begin
    @raw2hif_Alloc := GetProcAddress(DLLHandle1, 'raw2hif_Alloc');
    @raw2hif_Free := GetProcAddress(DLLHandle1, 'raw2hif_Free');
    @raw2hif_Init := GetProcAddress(DLLHandle1, 'raw2hif_Init');
    @raw2hif_Loop := GetProcAddress(DLLHandle1, 'raw2hif_Loop');
    @raw2hif_getoutlen := GetProcAddress(DLLHandle1, 'raw2hif_getoutlen');
    @raw2hif_getou2len := GetProcAddress(DLLHandle1, 'raw2hif_getou2len');
    @raw2hif_addbuf := GetProcAddress(DLLHandle1, 'raw2hif_addbuf');
    @hif2raw_Alloc := GetProcAddress(DLLHandle2, 'hif2raw_Alloc');
    @hif2raw_Free := GetProcAddress(DLLHandle2, 'hif2raw_Free');
    @hif2raw_Init := GetProcAddress(DLLHandle2, 'hif2raw_Init');
    @hif2raw_Loop := GetProcAddress(DLLHandle2, 'hif2raw_Loop');
    @hif2raw_getoutlen := GetProcAddress(DLLHandle2, 'hif2raw_getoutlen');
    @hif2raw_addbuf := GetProcAddress(DLLHandle2, 'hif2raw_addbuf');
    DLLLoaded := Assigned(raw2hif_Alloc) and Assigned(hif2raw_Alloc);
  end
  else
    DLLLoaded := False;
end;

procedure Deinit;
begin
  if not DLLLoaded then
    exit;
  FreeLibrary(DLLHandle1);
  FreeLibrary(DLLHandle2);
end;

initialization

Init;

finalization

Deinit;

end.
