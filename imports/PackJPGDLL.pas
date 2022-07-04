unit PackJPGDLL;

interface

uses
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
  DLLHandle: THandle;

procedure Init;
begin
  DLLHandle := LoadLibrary(PChar(ExtractFilePath(ParamStr(0)) +
    'packjpg_dll.dll'));
  if DLLHandle >= 32 then
  begin
    @pjglib_convert_stream2stream := GetProcAddress(DLLHandle,
      'pjglib_convert_stream2stream');
    @pjglib_convert_file2file := GetProcAddress(DLLHandle,
      'pjglib_convert_file2file');
    @pjglib_convert_stream2mem := GetProcAddress(DLLHandle,
      'pjglib_convert_stream2mem');
    @pjglib_init_streams := GetProcAddress(DLLHandle, 'pjglib_init_streams');
    @pjglib_version_info := GetProcAddress(DLLHandle, 'pjglib_version_info');
    @pjglib_short_name := GetProcAddress(DLLHandle, 'pjglib_short_name');
    DLLLoaded := Assigned(pjglib_init_streams) and
      Assigned(pjglib_convert_stream2stream);
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
