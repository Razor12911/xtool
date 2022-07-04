unit JoJpegDLL;

interface

uses
  WinAPI.Windows,
  System.SysUtils, System.Classes;

const
  jojpeg_Size = 51320000;

  jojpeg_Compress = 0;
  jojpeg_Decompress = 1;

  jojpeg_enc_Input = 1;
  jojpeg_enc_Output1 = 2;
  jojpeg_enc_Output2 = 3;

  jojpeg_dec_Input1 = 1;
  jojpeg_dec_Input2 = 3;
  jojpeg_dec_Output = 2;

var
  jojpeg_Init: function(p: Pointer; f_DEC: integer): integer stdcall;
  jojpeg_Quit: procedure(p: Pointer; f_DEC: integer)stdcall;
  jojpeg_Loop: function(p: Pointer; f_DEC: integer): integer stdcall;
  jojpeg_Getvalue: function(p: Pointer; f_DEC, typ: integer): Int64;
  jojpeg_Addbuf: procedure(p: Pointer; f_DEC: integer; buf: Pointer;
    bufsize, state: integer)stdcall;
  DLLLoaded: boolean = False;

implementation

var
  DLLHandle: THandle;

procedure Init;
begin
  DLLHandle := LoadLibrary(PChar(ExtractFilePath(ParamStr(0)) +
    'jojpeg_dll.dll'));
  if DLLHandle >= 32 then
  begin
    @jojpeg_Init := GetProcAddress(DLLHandle, 'jojpeg_Init');
    @jojpeg_Quit := GetProcAddress(DLLHandle, 'jojpeg_Quit');
    @jojpeg_Loop := GetProcAddress(DLLHandle, 'jojpeg_Loop');
    @jojpeg_Getvalue := GetProcAddress(DLLHandle, 'jojpeg_Getvalue');
    @jojpeg_Addbuf := GetProcAddress(DLLHandle, 'jojpeg_Addbuf');
    DLLLoaded := Assigned(jojpeg_Init);
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
