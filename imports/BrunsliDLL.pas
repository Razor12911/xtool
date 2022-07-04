unit BrunsliDLL;

interface

uses
  WinAPI.Windows,
  System.SysUtils, System.Classes;

const
  BRUNSLI_OK = 0;
  BRUNSLI_NON_REPRESENTABLE = 1;
  BRUNSLI_MEMORY_ERROR = 2;
  BRUNSLI_INVALID_PARAM = 3;
  BRUNSLI_COMPRESSION_ERROR = 4;
  BRUNSLI_INVALID_BRN = 5;
  BRUNSLI_DECOMPRESSION_ERROR = 6;
  BRUNSLI_NOT_ENOUGH_DATA = 7;

type
  TBrunsliWriter = function(ctx: Pointer; data: Pointer; Size: NativeUInt)
    : Integer cdecl;

var
  brunsli_alloc_JPEGData: function: Pointer cdecl;
  brunsli_free_JPEGData: procedure(P: Pointer)cdecl;
  brunsli_GetMaximumEncodedSize: function(P: Pointer): Integer cdecl;
  brunsli_ReadJpeg: function(P: Pointer; data: Pointer; len: Integer)
    : Integer cdecl;
  brunsli_EncodeJpeg: function(P: Pointer; data: Pointer; len: Integer)
    : Integer cdecl;
  brunsli_DecodeJpeg: function(P: Pointer; data: Pointer; len: Integer)
    : Integer cdecl;
  brunsli_alloc_JPEGOutput: function(P: TBrunsliWriter; data: Pointer)
    : Pointer cdecl;
  brunsli_free_JPEGOutput: procedure(P: Pointer)cdecl;
  brunsli_WriteJpeg: function(P: Pointer; oup: Pointer): Integer cdecl;
  DLLLoaded: Boolean = False;

implementation

var
  DLLHandle: THandle;
  S: String;

procedure Init;
begin
  S := 'brunsli.dll';
  DLLHandle := LoadLibrary(PChar(ExtractFilePath(ParamStr(0)) + S));
  if DLLHandle >= 32 then
  begin
    @brunsli_alloc_JPEGData := GetProcAddress(DLLHandle,
      'brunsli_alloc_JPEGData');
    @brunsli_free_JPEGData := GetProcAddress(DLLHandle,
      'brunsli_free_JPEGData');
    @brunsli_GetMaximumEncodedSize := GetProcAddress(DLLHandle,
      'brunsli_GetMaximumEncodedSize');
    @brunsli_ReadJpeg := GetProcAddress(DLLHandle, 'brunsli_ReadJpeg');
    @brunsli_EncodeJpeg := GetProcAddress(DLLHandle, 'brunsli_EncodeJpeg');
    @brunsli_DecodeJpeg := GetProcAddress(DLLHandle, 'brunsli_DecodeJpeg');
    @brunsli_alloc_JPEGOutput := GetProcAddress(DLLHandle,
      'brunsli_alloc_JPEGOutput');
    @brunsli_free_JPEGOutput := GetProcAddress(DLLHandle,
      'brunsli_free_JPEGOutput');
    @brunsli_WriteJpeg := GetProcAddress(DLLHandle, 'brunsli_WriteJpeg');
    DLLLoaded := Assigned(brunsli_alloc_JPEGData) and
      Assigned(brunsli_alloc_JPEGOutput);
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
