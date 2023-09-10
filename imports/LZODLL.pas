unit LZODLL;

interface

uses
  InitCode,
  Utils, LibImport,
  WinAPI.Windows,
  System.SysUtils;

const
  Success = 0;
  GenericError = -1;
  OutOfMemory = -2;
  NotCompressible = -3;
  InputOverrun = -4;
  OutputOverrun = -5;
  LookbehindOverrun = -6;
  EndOfFileNotFound = -7;
  InputNotConsumed = -8;
  NotImplemented = -9;
  InvalidArgument = -10;

type
  lzo_free_func_t = procedure(self: Pointer; ptr: Pointer); cdecl;
  lzo_alloc_func_t = function(self: Pointer; items, size: LongWord)
    : Pointer; cdecl;
  lzo_progress_func_t = procedure(self: Pointer; a, b: LongWord;
    c: integer); cdecl;

  lzo_callback_t = Record
    nalloc: lzo_alloc_func_t;
    nfree: lzo_free_func_t;
    nprogress: lzo_progress_func_t;
    user1: Pointer;
    user2: LongWord;
    user3: LongWord;
  end;

var
  lzo1x_1_compress: function(const src: Pointer; src_len: NativeUInt;
    dst: Pointer; dst_len: PNativeUInt; wrkmem: Pointer): integer; cdecl;
  lzo1x_1_11_compress: function(const src: Pointer; src_len: NativeUInt;
    dst: Pointer; dst_len: PNativeUInt; wrkmem: Pointer): integer; cdecl;
  lzo1x_1_12_compress: function(const src: Pointer; src_len: NativeUInt;
    dst: Pointer; dst_len: PNativeUInt; wrkmem: Pointer): integer; cdecl;
  lzo1x_1_15_compress: function(const src: Pointer; src_len: NativeUInt;
    dst: Pointer; dst_len: PNativeUInt; wrkmem: Pointer): integer; cdecl;
  lzo1x_999_compress: function(const src: Pointer; src_len: NativeUInt;
    dst: Pointer; dst_len: PNativeUInt; wrkmem: Pointer): integer; cdecl;
  lzo1x_999_compress_level: function(const src: Pointer; src_len: NativeUInt;
    dst: Pointer; dst_len: PNativeUInt; wrkmem: Pointer; const dict: Pointer;
    dict_len: Cardinal; cb: Pointer; compression_level: integer)
    : integer; cdecl;
  lzo1x_decompress_safe: function(const src: Pointer; src_len: NativeUInt;
    dst: Pointer; dst_len: PNativeUInt): integer cdecl;
  lzo1c_999_compress: function(const src: Pointer; src_len: NativeUInt;
    dst: Pointer; dst_len: PNativeUInt; wrkmem: Pointer): integer; cdecl;
  lzo1c_decompress_safe: function(const src: Pointer; src_len: NativeUInt;
    dst: Pointer; dst_len: PNativeUInt): integer cdecl;
  lzo2a_999_compress: function(const src: Pointer; src_len: NativeUInt;
    dst: Pointer; dst_len: PNativeUInt; wrkmem: Pointer): integer; cdecl;
  lzo2a_decompress_safe: function(const src: Pointer; src_len: NativeUInt;
    dst: Pointer; dst_len: PNativeUInt): integer cdecl;

  DLLLoaded: Boolean = False;

implementation

var
  Lib: TLibImport;

procedure Init(Filename: String);
begin
  Lib := TLibImport.Create;
  Lib.LoadLib(ExpandPath(Filename, True));
  if Lib.Loaded then
  begin
    @lzo1x_1_compress := Lib.GetProcAddr('lzo1x_1_compress');
    @lzo1x_1_11_compress := Lib.GetProcAddr('lzo1x_1_11_compress');
    @lzo1x_1_12_compress := Lib.GetProcAddr('lzo1x_1_12_compress');
    @lzo1x_1_15_compress := Lib.GetProcAddr('lzo1x_1_15_compress');
    @lzo1x_999_compress := Lib.GetProcAddr('lzo1x_999_compress');
    @lzo1x_999_compress_level := Lib.GetProcAddr('lzo1x_999_compress_level');
    @lzo1x_decompress_safe := Lib.GetProcAddr('lzo1x_decompress_safe');
    @lzo1c_999_compress := Lib.GetProcAddr('lzo1c_999_compress');
    @lzo1c_decompress_safe := Lib.GetProcAddr('lzo1c_decompress_safe');
    @lzo2a_999_compress := Lib.GetProcAddr('lzo2a_999_compress');
    @lzo2a_decompress_safe := Lib.GetProcAddr('lzo2a_decompress_safe');
    DLLLoaded := Assigned(lzo1x_decompress_safe);
  end;
end;

procedure Deinit;
begin
  Lib.Free;
end;

const
  DLLParam = '-lzo';

var
  I: integer;
  DLLFile: String;

initialization

DLLFile := PluginsPath + 'lzo2.dll';
for I := 1 to ParamCount do
begin
  if ParamStr(I).StartsWith(DLLParam) then
  begin
    DLLFile := ParamStr(I).Substring(DLLParam.Length);
    break;
  end;
end;

Init(DLLFile);

finalization

Deinit;

end.
