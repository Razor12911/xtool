unit XDeltaDLL;

interface

uses
  MemoryModule,
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

uses
  Utils;

var
  DLLStream: TResourceStream;
  DLLHandle: TMemoryModule;

procedure Init;
begin
  DLLStream := TResourceStream.Create(HInstance, 'xdelta3_dll', RT_RCDATA);
  DLLHandle := MemoryLoadLibary(DLLStream.Memory);
  if Assigned(DLLHandle) then
  begin
    DLLLoaded := True;
    @xd3_encode := MemoryGetProcAddress(DLLHandle, 'xd3_encode');
    Assert(@xd3_encode <> nil);
    @xd3_decode := MemoryGetProcAddress(DLLHandle, 'xd3_decode');
    Assert(@xd3_decode <> nil);
  end
  else
    DLLLoaded := False;
end;

procedure Deinit;
begin
  if not DLLLoaded then
    exit;
  MemoryFreeLibrary(DLLHandle);
end;

initialization

Init;

finalization

Deinit;

end.
