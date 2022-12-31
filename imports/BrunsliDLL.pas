unit BrunsliDLL;

interface

uses
  InitCode,
  Utils, LibImport,
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
  Lib: TLibImport;

procedure Init;
begin
  Lib := TLibImport.Create(ExpandPath(PluginsPath + 'brunsli.dll', True));
  if Lib.Loaded then
  begin
    @brunsli_alloc_JPEGData := Lib.GetProcAddr('brunsli_alloc_JPEGData');
    @brunsli_free_JPEGData := Lib.GetProcAddr('brunsli_free_JPEGData');
    @brunsli_GetMaximumEncodedSize :=
      Lib.GetProcAddr('brunsli_GetMaximumEncodedSize');
    @brunsli_ReadJpeg := Lib.GetProcAddr('brunsli_ReadJpeg');
    @brunsli_EncodeJpeg := Lib.GetProcAddr('brunsli_EncodeJpeg');
    @brunsli_DecodeJpeg := Lib.GetProcAddr('brunsli_DecodeJpeg');
    @brunsli_alloc_JPEGOutput := Lib.GetProcAddr('brunsli_alloc_JPEGOutput');
    @brunsli_free_JPEGOutput := Lib.GetProcAddr('brunsli_free_JPEGOutput');
    @brunsli_WriteJpeg := Lib.GetProcAddr('brunsli_WriteJpeg');
    DLLLoaded := Assigned(brunsli_alloc_JPEGData) and
      Assigned(brunsli_alloc_JPEGOutput);
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
