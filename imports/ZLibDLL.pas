unit ZLibDLL;

interface

uses
  LibImport,
  WinAPI.Windows,
  System.SysUtils, System.Types, System.IOUtils, System.ZLib;

const
  Z_NO_FLUSH = 0;
  Z_PARTIAL_FLUSH = 1;
  Z_SYNC_FLUSH = 2;
  Z_FULL_FLUSH = 3;
  Z_FINISH = 4;
  Z_BLOCK = 5;
  Z_TREES = 6;

  Z_OK = 0;
  Z_STREAM_END = 1;
  Z_NEED_DICT = 2;
  Z_ERRNO = (-1);
  Z_STREAM_ERROR = (-2);
  Z_DATA_ERROR = (-3);
  Z_MEM_ERROR = (-4);
  Z_BUF_ERROR = (-5);
  Z_VERSION_ERROR = (-6);

  Z_FILTERED = 1;
  Z_HUFFMAN_ONLY = 2;
  Z_RLE = 3;
  Z_FIXED = 4;
  Z_DEFAULT_STRATEGY = 0;

  Z_DEFLATED = 8;

  _z_errmsg: array [0 .. 9] of PAnsiChar = ('need dictionary', 'stream end', '',
    'file error', 'stream error', 'data error', 'insufficient memory',
    'buffer error', 'incompatible version', '');

type
  alloc_func = function(opaque: Pointer; Items, Size: cardinal): Pointer; cdecl;
  free_func = procedure(opaque, address: Pointer); cdecl;

  internal_state = record
  end;

  Pinternal_state = ^internal_state;

  z_stream = System.ZLib.z_stream;
  z_streamp = ^z_stream;

  EZLibError = class(Exception);
  EZCompressionError = class(EZLibError);
  EZDecompressionError = class(EZLibError);

var
  _zlibVersion: function: PAnsiChar; stdcall;
  _zlibCompileFlags: function: LongWord; stdcall;

  s_deflateInit2_: function(var strm: z_stream;
    level, method, windowBits, memLevel, strategy: integer; version: PAnsiChar;
    stream_size: integer): integer stdcall;
  s_deflate: function(var strm: z_stream; flush: integer): integer stdcall;
  s_deflateEnd: function(var strm: z_stream): integer stdcall;
  s_deflateReset: function(var strm: z_stream): integer stdcall;

  c_deflateInit2_: function(var strm: z_stream;
    level, method, windowBits, memLevel, strategy: integer; version: PAnsiChar;
    stream_size: integer): integer cdecl;
  c_deflate: function(var strm: z_stream; flush: integer): integer cdecl;
  c_deflateEnd: function(var strm: z_stream): integer cdecl;
  c_deflateReset: function(var strm: z_stream): integer cdecl;

  DLLLoaded: boolean = False;

function deflateInit2(var strm: z_stream; level, method, windowBits, memLevel,
  strategy: integer): integer;
function deflate(var strm: z_stream; flush: integer): integer;
function deflateEnd(var strm: z_stream): integer;
function deflateReset(var strm: z_stream): integer;
function inflateInit2(var strm: z_stream; windowBits: integer): integer;
function inflate(var strm: z_stream; flush: integer): integer;
function inflateEnd(var strm: z_stream): integer;
function inflateReset(var strm: z_stream): integer;

implementation

var
  Lib: TLibImport;
  WinAPIDLL: boolean;

function deflateInit2(var strm: z_stream; level, method, windowBits, memLevel,
  strategy: integer): integer;
begin
  if WinAPIDLL then
    Result := s_deflateInit2_(strm, level, method, windowBits, memLevel,
      strategy, _zlibVersion, SizeOf(z_stream))
  else
    Result := c_deflateInit2_(strm, level, method, windowBits, memLevel,
      strategy, _zlibVersion, SizeOf(z_stream));
end;

function deflate(var strm: z_stream; flush: integer): integer;
begin
  if WinAPIDLL then
    Result := s_deflate(strm, flush)
  else
    Result := c_deflate(strm, flush);
end;

function deflateEnd(var strm: z_stream): integer;
begin
  if WinAPIDLL then
    Result := s_deflateEnd(strm)
  else
    Result := c_deflateEnd(strm);
end;

function deflateReset(var strm: z_stream): integer;
begin
  if WinAPIDLL then
    Result := s_deflateReset(strm)
  else
    Result := c_deflateReset(strm);
end;

function inflateInit2(var strm: z_stream; windowBits: integer): integer;
begin
  Result := System.ZLib.inflateInit2_(strm, windowBits,
    System.ZLib.ZLIB_VERSION, SizeOf(z_stream));
end;

function inflate(var strm: z_stream; flush: integer): integer;
begin
  Result := System.ZLib.inflate(strm, flush);
end;

function inflateEnd(var strm: z_stream): integer;
begin
  Result := System.ZLib.inflateEnd(strm);
end;

function inflateReset(var strm: z_stream): integer;
begin
  Result := System.ZLib.inflateReset(strm);
end;

procedure Init(Filename: String);
begin
  Lib := TLibImport.Create(ExtractFilePath(ParamStr(0)) + Filename);
  if not(Lib.Loaded and Assigned(Lib.GetProcAddr('zlibVersion'))) then
  begin
    Lib.Free;
    Lib := TLibImport.Create(ExtractFilePath(ParamStr(0)) + 'zlibwapi.dll');
  end;
  if not(Lib.Loaded and Assigned(Lib.GetProcAddr('zlibVersion'))) then
  begin
    Lib.Free;
    Lib := TLibImport.Create(ExtractFilePath(ParamStr(0)) + 'zlib1.dll');
  end;
  if Lib.Loaded and Assigned(Lib.GetProcAddr('zlibVersion')) then
  begin
    DLLLoaded := True;
    @_zlibVersion := Lib.GetProcAddr('zlibVersion');
    @_zlibCompileFlags := Lib.GetProcAddr('zlibCompileFlags');
    DLLLoaded := Assigned(_zlibVersion) and Assigned(_zlibCompileFlags);
    if DLLLoaded then
    begin
      WinAPIDLL := _zlibCompileFlags and $400 = $400;
      if WinAPIDLL then
      begin
        @s_deflateInit2_ := Lib.GetProcAddr('deflateInit2_');
        @s_deflate := Lib.GetProcAddr('deflate');
        @s_deflateEnd := Lib.GetProcAddr('deflateEnd');
        @s_deflateReset := Lib.GetProcAddr('deflateReset');
      end
      else
      begin
        @c_deflateInit2_ := Lib.GetProcAddr('deflateInit2_');
        @c_deflate := Lib.GetProcAddr('deflate');
        @c_deflateEnd := Lib.GetProcAddr('deflateEnd');
        @c_deflateReset := Lib.GetProcAddr('deflateReset');
      end;
    end;
  end;
end;

procedure Deinit;
begin
  Lib.Free;
end;

const
  DLLParam = '--zlib=';

var
  I: integer;
  DLLFile: String;

initialization

DLLFile := 'zlibwapi.dll';
for I := 1 to ParamCount do
  if ParamStr(I).StartsWith(DLLParam) then
  begin
    DLLFile := ParamStr(I).Substring(DLLParam.Length);
    break;
  end;

Init(DLLFile);

finalization

Deinit;

end.
