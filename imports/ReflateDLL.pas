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
    DLLLoaded := True;
    @raw2hif_Alloc := GetProcAddress(DLLHandle1, 'raw2hif_Alloc');
    Assert(@raw2hif_Alloc <> nil);
    @raw2hif_Free := GetProcAddress(DLLHandle1, 'raw2hif_Free');
    Assert(@raw2hif_Free <> nil);
    @raw2hif_Init := GetProcAddress(DLLHandle1, 'raw2hif_Init');
    Assert(@raw2hif_Init <> nil);
    @raw2hif_Loop := GetProcAddress(DLLHandle1, 'raw2hif_Loop');
    Assert(@raw2hif_Loop <> nil);
    @raw2hif_getoutlen := GetProcAddress(DLLHandle1, 'raw2hif_getoutlen');
    Assert(@raw2hif_getoutlen <> nil);
    @raw2hif_getou2len := GetProcAddress(DLLHandle1, 'raw2hif_getou2len');
    Assert(@raw2hif_getou2len <> nil);
    @raw2hif_addbuf := GetProcAddress(DLLHandle1, 'raw2hif_addbuf');
    Assert(@raw2hif_addbuf <> nil);
    @hif2raw_Alloc := GetProcAddress(DLLHandle2, 'hif2raw_Alloc');
    Assert(@hif2raw_Alloc <> nil);
    @hif2raw_Free := GetProcAddress(DLLHandle2, 'hif2raw_Free');
    Assert(@hif2raw_Free <> nil);
    @hif2raw_Init := GetProcAddress(DLLHandle2, 'hif2raw_Init');
    Assert(@hif2raw_Init <> nil);
    @hif2raw_Loop := GetProcAddress(DLLHandle2, 'hif2raw_Loop');
    Assert(@hif2raw_Loop <> nil);
    @hif2raw_getoutlen := GetProcAddress(DLLHandle2, 'hif2raw_getoutlen');
    Assert(@hif2raw_getoutlen <> nil);
    @hif2raw_addbuf := GetProcAddress(DLLHandle2, 'hif2raw_addbuf');
    Assert(@hif2raw_addbuf <> nil);
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
