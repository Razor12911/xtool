program example;

{ example.c -- usage example of the zlib compression library
  Copyright (C) 1995-1998 Jean-loup Gailly.

  Pascal tranlastion
  Copyright (C) 1998 by Jacques Nomssi Nzali
  For conditions of distribution and use, see copyright notice in readme.txt
}
{-$define MemCheck}
{$DEFINE TEST_COMPRESS}
{$DEFINE TEST_GZIO}
{$DEFINE TEST_INFLATE}
{$DEFINE TEST_DEFLATE}
{$DEFINE TEST_SYNC}
{$DEFINE TEST_DICT}
{$DEFINE TEST_FLUSH}

uses
{$ifdef ver80}
 WinCrt,
{$endif}
{$ifdef you may have to define this in Delphi < 5}
  strings,
{$endif}
{$ifndef MSDOS}
  SysUtils,
{$endif}
  zutil,
  gzLib,
  gzIo,
  zInflate,
  zDeflate,
  zCompres,
  zUnCompr
{$ifdef MemCheck}
  , MemCheck in '..\..\monotekt\pas\memcheck\memcheck.pas'
{$endif}
;

procedure Stop;
begin
  Write('Program halted...');
  ReadLn;
  Halt(1);
end;

procedure CHECK_ERR(err : int; msg : string);
begin
  if (err <> Z_OK) then
  begin
    Write(msg, ' error: ', err);
    Stop;
  end;
end;

const
  hello : PChar = 'hello, hello!';
{ "hello world" would be more standard, but the repeated "hello"
  stresses the compression code better, sorry... }

{$IFDEF TEST_DICT}
const
  dictionary : PChar = 'hello';
var
  dictId : uLong; { Adler32 value of the dictionary }
{$ENDIF}

{ ===========================================================================
  Test compress() and uncompress() }

{$IFDEF TEST_COMPRESS}
procedure test_compress(compr : pBytef; var comprLen : uLong;
                        uncompr : pBytef; uncomprLen : uLong);
var
  err : int;
  len : uLong;
begin
  len := strlen(hello)+1;
  err := compress(compr, comprLen, pBytef(hello)^, len);
  CHECK_ERR(err, 'compress');

  strcopy(PChar(uncompr), 'garbage');

  err := uncompress(uncompr, uncomprLen, compr^, comprLen);
  CHECK_ERR(err, 'uncompress');

  if (strcomp(PChar(uncompr), hello)) <> 0 then
  begin
    WriteLn('bad uncompress');
    Stop;
  end
  else
    WriteLn('uncompress(): ', StrPas(PChar(uncompr)));
end;
{$ENDIF}

{ ===========================================================================
  Test read/write of .gz files }

{$IFDEF TEST_GZIO}
procedure test_gzio(const outf : string; { output file }
                    const inf : string;  { input file }
                    uncompr : pBytef;
                    uncomprLen : int);
var
  err : int;
  len : int;
var
  zfile : gzFile;
  pos : z_off_t;
begin
  len := strlen(hello)+1;

  zfile := gzopen(outf, 'w');
  if (zfile = NIL) then
  begin
    WriteLn('_gzopen error');
    Stop;
  end;
  gzputc(zfile, 'h');
  if (gzputs(zfile, 'ello') <> 4) then
  begin
    WriteLn('gzputs err: ', gzerror(zfile, err));
    Stop;
  end;
  {$ifdef GZ_FORMAT_STRING}
  if (gzprintf(zfile, ', %s!', 'hello') <> 8) then
  begin
    WriteLn('gzprintf err: ', gzerror(zfile, err));
    Stop;
  end;
  {$else}
  if (gzputs(zfile, ', hello!') <> 8) then
  begin
    WriteLn('gzputs err: ', gzerror(zfile, err));
    Stop;
  end;
  {$ENDIF}
  gzseek(zfile, Long(1), SEEK_CUR); { add one zero byte }
  gzclose(zfile);

  zfile := gzopen(inf, 'r');
  if (zfile = NIL) then
    WriteLn('gzopen error');

  strcopy(pchar(uncompr), 'garbage');

  uncomprLen := gzread(zfile, uncompr, uInt(uncomprLen));
  if (uncomprLen <> len) then
  begin
    WriteLn('gzread err: ', gzerror(zfile, err));
    Stop;
  end;
  if (strcomp(pchar(uncompr), hello)) <> 0 then
  begin
    WriteLn('bad gzread: ', pchar(uncompr));
    Stop;
  end
  else
    WriteLn('gzread(): ', pchar(uncompr));

  pos := gzseek(zfile, Long(-8), SEEK_CUR);
  if (pos <> 6) or (gztell(zfile) <> pos) then
  begin
    WriteLn('gzseek error, pos=',pos,', gztell=',gztell(zfile));
    Stop;
  end;

  if (char(gzgetc(zfile)) <> ' ') then
  begin
    WriteLn('gzgetc error');
    Stop;
  end;

  gzgets(zfile, pchar(uncompr), uncomprLen);
  uncomprLen := strlen(pchar(uncompr));
  if (uncomprLen <> 6) then
  begin { "hello!" }
    WriteLn('gzgets err after gzseek: ', gzerror(zfile, err));
    Stop;
  end;
  if (strcomp(pchar(uncompr), hello+7)) <> 0 then
  begin
    WriteLn('bad gzgets after gzseek');
    Stop;
  end
  else
    WriteLn('gzgets() after gzseek: ', PChar(uncompr));

  gzclose(zfile);
end;
{$ENDIF}

{ ===========================================================================
  Test deflate() with small buffers }

{$IFDEF TEST_DEFLATE}
procedure test_deflate(compr : pBytef; comprLen : uLong);
var
  c_stream : z_stream; { compression stream }
  err : int;
  len : int;
begin
  len := strlen(hello)+1;
  c_stream.zalloc := NIL; {alloc_func(0);}
  c_stream.zfree := NIL;  {free_func(0);}
  c_stream.opaque := NIL; {voidpf(0);}

  err := deflateInit(c_stream, Z_DEFAULT_COMPRESSION);
  CHECK_ERR(err, 'deflateInit');

  c_stream.next_in  := pBytef(hello);
  c_stream.next_out := compr;

  while (c_stream.total_in <> uLong(len)) and (c_stream.total_out < comprLen) do
  begin
    c_stream.avail_out := 1; { force small buffers }
    c_stream.avail_in := 1;
    err := deflate(c_stream, Z_NO_FLUSH);
    CHECK_ERR(err, 'deflate');
  end;

  { Finish the stream, still forcing small buffers: }
  while TRUE do
  begin
    c_stream.avail_out := 1;
    err := deflate(c_stream, Z_FINISH);
    if (err = Z_STREAM_END) then
      break;
    CHECK_ERR(err, 'deflate');
  end;

  err := deflateEnd(c_stream);
  CHECK_ERR(err, 'deflateEnd');
end;
{$ENDIF}

{ ===========================================================================
  Test inflate() with small buffers
}

{$IFDEF TEST_INFLATE}
procedure test_inflate(compr : pBytef; comprLen : uLong;
                       uncompr : pBytef;  uncomprLen : uLong);
var
  err : int;
  d_stream : z_stream; { decompression stream }
begin
  strcopy(PChar(uncompr), 'garbage');

  d_stream.zalloc := NIL; {alloc_func(0);}
  d_stream.zfree := NIL; {free_func(0);}
  d_stream.opaque := NIL; {voidpf(0);}

  d_stream.next_in  := compr;
  d_stream.avail_in := 0;
  d_stream.next_out := uncompr;

  err := inflateInit(d_stream);
  CHECK_ERR(err, 'inflateInit');

  while (d_stream.total_out < uncomprLen) and
        (d_stream.total_in < comprLen) do
  begin
    d_stream.avail_out := 1; { force small buffers }
    d_stream.avail_in := 1;
    err := inflate(d_stream, Z_NO_FLUSH);
    if (err = Z_STREAM_END) then
      break;
    CHECK_ERR(err, 'inflate');
  end;

  err := inflateEnd(d_stream);
  CHECK_ERR(err, 'inflateEnd');

  if (strcomp(PChar(uncompr), hello) <> 0) then
  begin
    WriteLn('bad inflate');
    exit;
  end
  else
  begin
    WriteLn('inflate(): ', StrPas(PChar(uncompr)));
  end;
end;
{$ENDIF}

{ ===========================================================================
  Test deflate() with large buffers and dynamic change of compression level
 }

{$IFDEF TEST_DEFLATE}
procedure test_large_deflate(compr : pBytef; comprLen : uLong;
                             uncompr : pBytef;  uncomprLen : uLong);
var
  c_stream : z_stream; { compression stream }
  err : int;
begin
  c_stream.zalloc := NIL; {alloc_func(0);}
  c_stream.zfree := NIL;  {free_func(0);}
  c_stream.opaque := NIL; {voidpf(0);}

  err := deflateInit(c_stream, Z_BEST_SPEED);
  CHECK_ERR(err, 'deflateInit');

  c_stream.next_out := compr;
  c_stream.avail_out := uInt(comprLen);

  { At this point, uncompr is still mostly zeroes, so it should compress
    very well: }

  c_stream.next_in := uncompr;
  c_stream.avail_in := uInt(uncomprLen);
  err := deflate(c_stream, Z_NO_FLUSH);
  CHECK_ERR(err, 'deflate');
  if (c_stream.avail_in <> 0) then
  begin
    WriteLn('deflate not greedy');
    exit;
  end;

  { Feed in already compressed data and switch to no compression: }
  deflateParams(c_stream, Z_NO_COMPRESSION, Z_DEFAULT_STRATEGY);
  c_stream.next_in := compr;
  c_stream.avail_in := uInt(comprLen div 2);
  err := deflate(c_stream, Z_NO_FLUSH);
  CHECK_ERR(err, 'deflate');

  { Switch back to compressing mode: }
  deflateParams(c_stream, Z_BEST_COMPRESSION, Z_FILTERED);
  c_stream.next_in := uncompr;
  c_stream.avail_in := uInt(uncomprLen);
  err := deflate(c_stream, Z_NO_FLUSH);
  CHECK_ERR(err, 'deflate');

  err := deflate(c_stream, Z_FINISH);
  if (err <> Z_STREAM_END) then
  begin
    WriteLn('deflate should report Z_STREAM_END');
    exit;
  end;
  err := deflateEnd(c_stream);
  CHECK_ERR(err, 'deflateEnd');
end;
{$ENDIF}

{ ===========================================================================
  Test inflate() with large buffers }

{$IFDEF TEST_INFLATE}
procedure test_large_inflate(compr : pBytef; comprLen : uLong;
                             uncompr : pBytef;  uncomprLen : uLong);
var
  err : int;
  d_stream : z_stream; { decompression stream }
begin
  strcopy(PChar(uncompr), 'garbage');

  d_stream.zalloc := NIL; {alloc_func(0);}
  d_stream.zfree := NIL; {free_func(0);}
  d_stream.opaque := NIL; {voidpf(0);}

  d_stream.next_in  := compr;
  d_stream.avail_in := uInt(comprLen);

  err := inflateInit(d_stream);
  CHECK_ERR(err, 'inflateInit');

  while TRUE do
  begin
    d_stream.next_out := uncompr;            { discard the output }
    d_stream.avail_out := uInt(uncomprLen);
    err := inflate(d_stream, Z_NO_FLUSH);
    if (err = Z_STREAM_END) then
      break;
    CHECK_ERR(err, 'large inflate');
  end;

  err := inflateEnd(d_stream);
  CHECK_ERR(err, 'inflateEnd');

  if (d_stream.total_out <> 2*uncomprLen + comprLen div 2) then
  begin
    WriteLn('bad large inflate: ', d_stream.total_out);
    Stop;
  end
  else
    WriteLn('large_inflate(): OK');
end;
{$ENDIF}

{ ===========================================================================
  Test deflate() with full flush
 }
{$IFDEF TEST_FLUSH}
procedure test_flush(compr : pBytef; var comprLen : uLong);
var
  c_stream : z_stream; { compression stream }
  err : int;
  len : int;

begin
  len := strlen(hello)+1;
  c_stream.zalloc := NIL;       {alloc_func(0);}
  c_stream.zfree := NIL;        {free_func(0);}
  c_stream.opaque := NIL;       {voidpf(0);}

  err := deflateInit(c_stream, Z_DEFAULT_COMPRESSION);
  CHECK_ERR(err, 'deflateInit');

  c_stream.next_in := pBytef(hello);
  c_stream.next_out := compr;
  c_stream.avail_in := 3;
  c_stream.avail_out := uInt(comprLen);

  err := deflate(c_stream, Z_FULL_FLUSH);
  CHECK_ERR(err, 'deflate');

  Inc(pzByteArray(compr)^[3]); { force an error in first compressed block }
  c_stream.avail_in := len - 3;

  err := deflate(c_stream, Z_FINISH);
  if (err <> Z_STREAM_END) then
    CHECK_ERR(err, 'deflate');

  err := deflateEnd(c_stream);
    CHECK_ERR(err, 'deflateEnd');

  comprLen := c_stream.total_out;
end;
{$ENDIF}

{ ===========================================================================
  Test inflateSync()
 }
{$IFDEF TEST_SYNC}
procedure test_sync(compr : pBytef; comprLen : uLong;
                    uncompr : pBytef; uncomprLen : uLong);
var
  err : int;
  d_stream : z_stream; { decompression stream }
begin
  strcopy(PChar(uncompr), 'garbage');

  d_stream.zalloc := NIL;            {alloc_func(0);}
  d_stream.zfree := NIL;             {free_func(0);}
  d_stream.opaque := NIL;            {voidpf(0);}

  d_stream.next_in  := compr;
  d_stream.avail_in := 2; { just read the zlib header }

  err := inflateInit(d_stream);
  CHECK_ERR(err, 'inflateInit');

  d_stream.next_out := uncompr;
  d_stream.avail_out := uInt(uncomprLen);

  inflate(d_stream, Z_NO_FLUSH);
  CHECK_ERR(err, 'inflate');

  d_stream.avail_in := uInt(comprLen-2);   { read all compressed data }
  err := inflateSync(d_stream);           { but skip the damaged part }
  CHECK_ERR(err, 'inflateSync');

  err := inflate(d_stream, Z_FINISH);
  if (err <> Z_DATA_ERROR) then
  begin
    WriteLn('inflate should report DATA_ERROR');
      { Because of incorrect adler32 }
    Stop;
  end;
  err := inflateEnd(d_stream);
  CHECK_ERR(err, 'inflateEnd');

  WriteLn('after inflateSync(): hel', StrPas(PChar(uncompr)));
end;
{$ENDIF}

{ ===========================================================================
  Test deflate() with preset dictionary
 }
{$IFDEF TEST_DICT}
procedure test_dict_deflate(compr : pBytef; comprLen : uLong);
var
  c_stream : z_stream; { compression stream }
  err : int;
begin
  c_stream.zalloc := NIL; {(alloc_func)0;}
  c_stream.zfree := NIL; {(free_func)0;}
  c_stream.opaque := NIL; {(voidpf)0;}

  err := deflateInit(c_stream, Z_BEST_COMPRESSION);
  CHECK_ERR(err, 'deflateInit');

  err := deflateSetDictionary(c_stream,
                              pBytef(dictionary), StrLen(dictionary));
  CHECK_ERR(err, 'deflateSetDictionary');

  dictId := c_stream.adler;
  c_stream.next_out := compr;
  c_stream.avail_out := uInt(comprLen);

  c_stream.next_in := pBytef(hello);
  c_stream.avail_in := uInt(strlen(hello)+1);

  err := deflate(c_stream, Z_FINISH);
  if (err <> Z_STREAM_END) then
  begin
    WriteLn('deflate should report Z_STREAM_END');
    exit;
  end;
  err := deflateEnd(c_stream);
  CHECK_ERR(err, 'deflateEnd');
end;

{ ===========================================================================
  Test inflate() with a preset dictionary }

procedure test_dict_inflate(compr : pBytef; comprLen : uLong;
                            uncompr : pBytef; uncomprLen : uLong);
var
  err : int;
  d_stream : z_stream; { decompression stream }
begin
  strcopy(PChar(uncompr), 'garbage');

  d_stream.zalloc := NIL;              { alloc_func(0); }
  d_stream.zfree := NIL;               { free_func(0); }
  d_stream.opaque := NIL;              { voidpf(0); }

  d_stream.next_in  := compr;
  d_stream.avail_in := uInt(comprLen);

  err := inflateInit(d_stream);
  CHECK_ERR(err, 'inflateInit');

  d_stream.next_out := uncompr;
  d_stream.avail_out := uInt(uncomprLen);

  while TRUE do
  begin
    err := inflate(d_stream, Z_NO_FLUSH);
    if (err = Z_STREAM_END) then
      break;
    if (err = Z_NEED_DICT) then
    begin
      if (d_stream.adler <> dictId) then
      begin
        WriteLn('unexpected dictionary');
  Stop;
      end;
      err := inflateSetDictionary(d_stream, pBytef(dictionary),
             StrLen(dictionary));
    end;
    CHECK_ERR(err, 'inflate with dict');
  end;

  err := inflateEnd(d_stream);
  CHECK_ERR(err, 'inflateEnd');

  if (strcomp(PChar(uncompr), hello)) <> 0 then
  begin
    WriteLn('bad inflate with dict');
    Stop;
  end
  else
  begin
    WriteLn('inflate with dictionary: ', StrPas(PChar(uncompr)));
  end;
end;
{$ENDIF}

function GetFromFile(buf : pBytef; FName : string;
                     var MaxLen : uInt) : boolean;
const
  zOfs = 0;
var
  f : file;
  Len : uLong;
begin
  assign(f, FName);
  GetFromFile := false;
  {$I-}
  filemode := 0; { read only }
  reset(f, 1);
  if IOresult = 0 then
  begin
    Len := FileSize(f)-zOfs;
    Seek(f, zOfs);
    if Len < MaxLen then
      MaxLen := Len;
    BlockRead(f, buf^, MaxLen);
    close(f);
    WriteLn(FName);
    GetFromFile := (IOresult = 0) and (MaxLen > 0);
  end
  else
    WriteLn('Could not open ', FName);
end;

{ ===========================================================================
  Usage:  example [output.gz  [input.gz]]
}

var
  compr, uncompr : pBytef;
const
  msdoslen = 25000;
  comprLenL : uLong = msdoslen div sizeof(uInt); { don't overflow on MSDOS }
  uncomprLenL : uLong = msdoslen div sizeof(uInt);
var
  zVersion,
  myVersion : string;
var
  comprLen : uInt;
  uncomprLen : uInt;
begin
  {$ifdef MemCheck}
  MemChk;
  {$endif}
  comprLen := comprLenL;
  uncomprLen := uncomprLenL;

  myVersion := ZLIB_VERSION;
  zVersion := zlibVersion;
  if (zVersion[1] <> myVersion[1]) then
  begin
    WriteLn('incompatible zlib version');
    Stop;
  end
  else
    if (zVersion <> ZLIB_VERSION) then
    begin
      WriteLn('warning: different zlib version');
    end;

  GetMem(compr, comprLen*sizeof(uInt));
  GetMem(uncompr, uncomprLen*sizeof(uInt));
  { compr and uncompr are cleared to avoid reading uninitialized
    data and to ensure that uncompr compresses well. }

  if (compr = Z_NULL) or (uncompr = Z_NULL) then
  begin
    WriteLn('out of memory');
    Stop;
  end;
  FillChar(compr^, comprLen*sizeof(uInt), 0);
  FillChar(uncompr^, uncomprLen*sizeof(uInt), 0);

  if (compr = Z_NULL) or (uncompr = Z_NULL) then
  begin
    WriteLn('out of memory');
    Stop;
  end;
  {$IFDEF TEST_COMPRESS}
  test_compress(compr, comprLenL, uncompr, uncomprLen);
  {$ENDIF}

  {$IFDEF TEST_GZIO}
  Case ParamCount of
    0:  test_gzio('foo.gz', 'foo.gz', uncompr, int(uncomprLen));
    1:  test_gzio(ParamStr(1), 'foo.gz', uncompr, int(uncomprLen));
  else
    test_gzio(ParamStr(1), ParamStr(2), uncompr, int(uncomprLen));
  end;
  {$ENDIF}

  {$IFDEF TEST_DEFLATE}
  WriteLn('small buffer Deflate');
  test_deflate(compr, comprLen);
  {$ENDIF}
  {$IFDEF TEST_INFLATE}
  {$IFNDEF TEST_DEFLATE}
  WriteLn('small buffer Inflate');
  if GetFromFile(compr, 'u:\nomssi\paszlib\new\test0.z', comprLen) then
  {$ENDIF}
    test_inflate(compr, comprLen, uncompr, uncomprLen);
  {$ENDIF}
  readln;
  {$IFDEF TEST_DEFLATE}
  WriteLn('large buffer Deflate');
  test_large_deflate(compr, comprLen, uncompr, uncomprLen);
  {$ENDIF}
  {$IFDEF TEST_INFLATE}
  WriteLn('large buffer Inflate');
  test_large_inflate(compr, comprLen, uncompr, uncomprLen);
  {$ENDIF}
  {$IFDEF TEST_FLUSH}
  test_flush(compr, comprLenL);
  {$ENDIF}
  {$IFDEF TEST_SYNC}
  test_sync(compr, comprLen, uncompr, uncomprLen);
  {$ENDIF}
  comprLen := uncomprLen;

  {$IFDEF TEST_DICT}
  test_dict_deflate(compr, comprLen);
  test_dict_inflate(compr, comprLen, uncompr, uncomprLen);
  {$ENDIF}
  readln;
  FreeMem(compr, comprLen*sizeof(uInt));
  FreeMem(uncompr, uncomprLen*sizeof(uInt));
end.
