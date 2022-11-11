unit PackJPGDLL;

interface

uses
  LibImport,
  WinAPI.Windows,
  System.SysUtils, System.Classes;

const
  pjglib_file = 0;
  pjglib_memory = 1;
  pjglib_handle = 2;

var
  pjglib_convert_stream2stream: function(msg: PAnsiChar): Boolean cdecl;
  pjglib_convert_file2file: function(ain, aout, msg: PAnsiChar): Boolean cdecl;
  pjglib_convert_stream2mem: function(out_file: PPAnsiChar; out_size: PCardinal;
    msg: PAnsiChar): Boolean cdecl;
  pjglib_init_streams: procedure(in_src: Pointer; in_type: Integer;
    in_size: Integer; out_dest: Pointer; out_type: Integer)cdecl;
  pjglib_version_info: function: PAnsiChar cdecl;
  pjglib_short_name: function: PAnsiChar cdecl;
  DLLLoaded: Boolean = False;

implementation

var
  Lib: TLibImport;

procedure Init;
begin
  Lib := TLibImport.Create(ExtractFilePath(ParamStr(0)) + 'packjpg_dll.dll');
  if Lib.Loaded then
  begin
    @pjglib_convert_stream2stream :=
      Lib.GetProcAddr('pjglib_convert_stream2stream');
    @pjglib_convert_file2file := Lib.GetProcAddr('pjglib_convert_file2file');
    @pjglib_convert_stream2mem := Lib.GetProcAddr('pjglib_convert_stream2mem');
    @pjglib_init_streams := Lib.GetProcAddr('pjglib_init_streams');
    @pjglib_version_info := Lib.GetProcAddr('pjglib_version_info');
    @pjglib_short_name := Lib.GetProcAddr('pjglib_short_name');
    DLLLoaded := Assigned(pjglib_init_streams) and
      Assigned(pjglib_convert_stream2stream);
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
