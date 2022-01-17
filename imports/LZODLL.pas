unit LZODLL;

interface

uses
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
  lzopro_lzo1x_w03_15_compress: function(const src: Pointer;
    src_len: NativeUInt; dst: Pointer; dst_len: PNativeUInt; wrkmem: Pointer)
    : integer; cdecl;
  lzopro_lzo1x_99_compress: function(const src; src_len: integer; var dst;
    var dst_len; var cb; compression_level: integer): integer; cdecl;
  DLLLoaded: Boolean = False;

function lzo1x_99_compress(const src: Pointer; src_len: NativeUInt;
  dst: Pointer; dst_len: PNativeUInt; compression_level: integer): integer;

implementation

var
  DLLHandle: THandle;

procedure nfree(self: Pointer; ptr: Pointer); cdecl;
begin
  FreeMem(ptr);
end;

function nalloc(self: Pointer; items, size: LongWord): Pointer; cdecl;
var
  p: Pointer;
begin
  GetMem(p, size * items);
  Result := p;
end;

procedure nprogress(self: Pointer; a, b: LongWord; c: integer); cdecl;
begin
end;

function lzo1x_99_compress(const src: Pointer; src_len: NativeUInt;
  dst: Pointer; dst_len: PNativeUInt; compression_level: integer): integer;
var
  mycb: lzo_callback_t;
begin
  mycb.nalloc := nalloc;
  mycb.nfree := nfree;
  mycb.nprogress := nprogress;
  Result := lzopro_lzo1x_99_compress(src^, src_len, dst^, dst_len^, mycb,
    compression_level);
end;

procedure Init;
begin
  if DLLLoaded then
    Exit;
  DLLHandle := LoadLibrary(PChar(ExtractFilePath(ParamStr(0)) + 'lzo2.dll'));
  if DLLHandle >= 32 then
  begin
    DLLLoaded := True;
    @lzo1x_1_compress := GetProcAddress(DLLHandle, 'lzo1x_1_compress');
    Assert(@lzo1x_1_compress <> nil);
    @lzo1x_1_11_compress := GetProcAddress(DLLHandle, 'lzo1x_1_11_compress');
    Assert(@lzo1x_1_11_compress <> nil);
    @lzo1x_1_12_compress := GetProcAddress(DLLHandle, 'lzo1x_1_12_compress');
    Assert(@lzo1x_1_12_compress <> nil);
    @lzo1x_1_15_compress := GetProcAddress(DLLHandle, 'lzo1x_1_15_compress');
    Assert(@lzo1x_1_15_compress <> nil);
    @lzo1x_999_compress := GetProcAddress(DLLHandle, 'lzo1x_999_compress');
    Assert(@lzo1x_999_compress <> nil);
    @lzo1x_999_compress_level := GetProcAddress(DLLHandle,
      'lzo1x_999_compress_level');
    Assert(@lzo1x_999_compress_level <> nil);
    @lzo1x_decompress_safe := GetProcAddress(DLLHandle,
      'lzo1x_decompress_safe');
    Assert(@lzo1x_decompress_safe <> nil);
    @lzo1c_999_compress := GetProcAddress(DLLHandle, 'lzo1c_999_compress');
    Assert(@lzo1c_999_compress <> nil);
    @lzo1c_decompress_safe := GetProcAddress(DLLHandle,
      'lzo1c_decompress_safe');
    Assert(@lzo1c_decompress_safe <> nil);
    @lzo2a_999_compress := GetProcAddress(DLLHandle, 'lzo2a_999_compress');
    Assert(@lzo2a_999_compress <> nil);
    @lzo2a_decompress_safe := GetProcAddress(DLLHandle,
      'lzo2a_decompress_safe');
    Assert(@lzo2a_decompress_safe <> nil);
    (* if Length(lzoprodll) > 0 then
      begin
      MDLLHandle := MemoryLoadLibary(@lzoprodll[0]);
      @lzopro_lzo1x_w03_15_compress := MemoryGetProcAddress(MDLLHandle,
      'lzopro_lzo1x_w03_15_compress');
      Assert(@lzopro_lzo1x_w03_15_compress <> nil);
      @lzopro_lzo1x_99_compress := MemoryGetProcAddress(MDLLHandle,
      'lzopro_lzo1x_99_compress');
      Assert(@lzopro_lzo1x_99_compress <> nil);
      end; *)
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
