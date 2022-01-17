unit LZ4DLL;

interface

uses
  MemoryModule,
  WinAPI.Windows,
  System.SysUtils, System.Classes;

var
  LZ4_decompress_safe: function(source: Pointer; dest: Pointer;
    compressedSize: integer; maxDecompressedSize: integer): integer cdecl;
  LZ4_decompress_fast: function(source: Pointer; dest: Pointer;
    originalSize: integer): integer cdecl;
  LZ4_compress_default: function(src, dst: Pointer;
    srcSize, dstCapacity: integer): integer cdecl;
  LZ4_compress_fast: function(src, dst: Pointer; srcSize, dstCapacity: integer;
    acceleration: integer): integer cdecl;
  LZ4_compress_HC: function(const src: Pointer; dst: Pointer; srcSize: integer;
    maxDstSize: integer; compressionLevel: integer): integer cdecl;
  DLLLoaded: Boolean = False;

implementation

var
  DLLHandle: THandle;

procedure Init;
begin
  if DLLLoaded then
    Exit;
  DLLHandle := 0;
  DLLHandle := LoadLibrary(PWideChar(ExtractFilePath(ParamStr(0)) +
    'liblz4.dll'));
  if DLLHandle >= 32 then
  begin
    DLLLoaded := True;
    @LZ4_decompress_safe := GetProcAddress(DLLHandle, 'LZ4_decompress_safe');
    Assert(@LZ4_decompress_safe <> nil);
    @LZ4_decompress_fast := GetProcAddress(DLLHandle, 'LZ4_decompress_fast');
    Assert(@LZ4_decompress_fast <> nil);
    @LZ4_compress_default := GetProcAddress(DLLHandle, 'LZ4_compress_default');
    Assert(@LZ4_compress_default <> nil);
    @LZ4_compress_fast := GetProcAddress(DLLHandle, 'LZ4_compress_fast');
    Assert(@LZ4_compress_fast <> nil);
    @LZ4_compress_HC := GetProcAddress(DLLHandle, 'LZ4_compress_HC');
    Assert(@LZ4_compress_HC <> nil);
  end
  else
    DLLLoaded := False;
end;

procedure Deinit;
begin
  if not DLLLoaded then
    Exit;
  FreeLibrary(DLLHandle);
end;

initialization

Init;

finalization

Deinit;

end.
