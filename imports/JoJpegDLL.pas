unit JoJpegDLL;

interface

uses
  LibImport,
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
  Lib: TLibImport;

procedure Init;
begin
  Lib := TLibImport.Create(ExtractFilePath(ParamStr(0)) + 'jojpeg_dll.dll');
  if Lib.Loaded then
  begin
    @jojpeg_Init := Lib.GetProcAddr('jojpeg_Init');
    @jojpeg_Quit := Lib.GetProcAddr('jojpeg_Quit');
    @jojpeg_Loop := Lib.GetProcAddr('jojpeg_Loop');
    @jojpeg_Getvalue := Lib.GetProcAddr('jojpeg_Getvalue');
    @jojpeg_Addbuf := Lib.GetProcAddr('jojpeg_Addbuf');
    DLLLoaded := Assigned(jojpeg_Init);
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
