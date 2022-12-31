unit XDeltaDLL;

interface

uses
  InitCode,
  Utils, LibImport,
  WinAPI.Windows,
  System.SysUtils, System.Classes;

type
  xd3_flags = (XD3_JUST_HDR = (1 shl 1), XD3_SKIP_WINDOW = (1 shl 2),
    XD3_SKIP_EMIT = (1 shl 3), XD3_FLUSH = (1 shl 4), XD3_SEC_DJW = (1 shl 5),
    XD3_SEC_FGK = (1 shl 6), XD3_SEC_LZMA = (1 shl 24),
    XD3_SEC_TYPE = (XD3_SEC_DJW or XD3_SEC_FGK or XD3_SEC_LZMA),
    XD3_SEC_NODATA = (1 shl 7), XD3_SEC_NOINST = (1 shl 8),
    XD3_SEC_NOADDR = (1 shl 9), XD3_SEC_NOALL = (XD3_SEC_NODATA or
    XD3_SEC_NOINST or XD3_SEC_NOADDR), XD3_ADLER32 = (1 shl 10),
    XD3_ADLER32_NOVER = (1 shl 11), XD3_NOCOMPRESS = (1 shl 13),
    XD3_BEGREEDY = (1 shl 14), XD3_ADLER32_RECODE = (1 shl 15),
    XD3_COMPLEVEL_SHIFT = 20, XD3_COMPLEVEL_MASK = ($F shl XD3_COMPLEVEL_SHIFT),
    XD3_COMPLEVEL_1 = (1 shl XD3_COMPLEVEL_SHIFT),
    XD3_COMPLEVEL_2 = (2 shl XD3_COMPLEVEL_SHIFT),
    XD3_COMPLEVEL_3 = (3 shl XD3_COMPLEVEL_SHIFT),
    XD3_COMPLEVEL_6 = (6 shl XD3_COMPLEVEL_SHIFT),
    XD3_COMPLEVEL_9 = (9 shl XD3_COMPLEVEL_SHIFT));

var
  xd3_encode: function(const input: PByte; input_size: NativeUInt;
    source: PByte; source_size: NativeUInt; output_buffer: PByte;
    output_size: PNativeUInt; avail_output: NativeUInt; flags: Integer)
    : Integer cdecl;
  xd3_decode: function(const input: PByte; input_size: NativeUInt;
    source: PByte; source_size: NativeUInt; output_buf: PByte;
    output_size: PNativeUInt; avail_output: NativeUInt; flags: Integer)
    : Integer cdecl;
  DLLLoaded: boolean = False;

implementation

var
  Lib: TLibImport;

procedure Init;
begin
  Lib := TLibImport.Create(ExpandPath(PluginsPath + 'xdelta3_dll.dll', True));
  if Lib.Loaded then
  begin
    DLLLoaded := True;
    @xd3_encode := Lib.GetProcAddr('xd3_encode');
    Assert(@xd3_encode <> nil);
    @xd3_decode := Lib.GetProcAddr('xd3_decode');
    Assert(@xd3_decode <> nil);
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
