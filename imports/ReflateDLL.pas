unit ReflateDLL;

interface

uses
  InitCode,
  Utils, LibImport,
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
  Lib1, Lib2: TLibImport;

procedure Init;
begin
  Lib1 := TLibImport.Create;
  Lib1.LoadLib(ExpandPath(PluginsPath + 'RAW2HIF_DLL.DLL', True));
  Lib2 := TLibImport.Create;
  Lib2.LoadLib(ExpandPath(PluginsPath + 'HIF2RAW_DLL.DLL', True));
  if Lib1.Loaded and Lib2.Loaded then
  begin
    @raw2hif_Alloc := Lib1.GetProcAddr('raw2hif_Alloc');
    @raw2hif_Free := Lib1.GetProcAddr('raw2hif_Free');
    @raw2hif_Init := Lib1.GetProcAddr('raw2hif_Init');
    @raw2hif_Loop := Lib1.GetProcAddr('raw2hif_Loop');
    @raw2hif_getoutlen := Lib1.GetProcAddr('raw2hif_getoutlen');
    @raw2hif_getou2len := Lib1.GetProcAddr('raw2hif_getou2len');
    @raw2hif_addbuf := Lib1.GetProcAddr('raw2hif_addbuf');
    @hif2raw_Alloc := Lib2.GetProcAddr('hif2raw_Alloc');
    @hif2raw_Free := Lib2.GetProcAddr('hif2raw_Free');
    @hif2raw_Init := Lib2.GetProcAddr('hif2raw_Init');
    @hif2raw_Loop := Lib2.GetProcAddr('hif2raw_Loop');
    @hif2raw_getoutlen := Lib2.GetProcAddr('hif2raw_getoutlen');
    @hif2raw_addbuf := Lib2.GetProcAddr('hif2raw_addbuf');
    DLLLoaded := Assigned(raw2hif_Alloc) and Assigned(hif2raw_Alloc);
  end;
end;

procedure Deinit;
begin
  Lib1.Free;
  Lib2.Free;
end;

initialization

Init;

finalization

Deinit;

end.
