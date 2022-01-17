Unit zCompres;

{ compress.c -- compress a memory buffer
  Copyright (C) 1995-1998 Jean-loup Gailly.

  Pascal tranlastion
  Copyright (C) 1998 by Jacques Nomssi Nzali
  For conditions of distribution and use, see copyright notice in readme.txt

  Modifiied 02/2003 by Sergey A. Galin for Delphi 6+ and Kylix compatibility.
  See README in directory above for more information.  
}
interface

{$I zconf.inc}

uses
  ZUtil, gZlib, zDeflate;

                        { utility functions }

{EXPORT}
function compress (dest : pBytef;
                   var destLen : uLong;
                   const source : array of Byte;
                   sourceLen : uLong) : int;

 { Compresses the source buffer into the destination buffer.  sourceLen is
   the byte length of the source buffer. Upon entry, destLen is the total
   size of the destination buffer, which must be at least 0.1% larger than
   sourceLen plus 12 bytes. Upon exit, destLen is the actual size of the
   compressed buffer.
     This function can be used to compress a whole file at once if the
   input file is mmap'ed.
     compress returns Z_OK if success, Z_MEM_ERROR if there was not
   enough memory, Z_BUF_ERROR if there was not enough room in the output
   buffer. }

{EXPORT}
function compress2 (dest : pBytef;
                    var destLen : uLong;
                    const source : array of byte;
                    sourceLen : uLong;
                    level : int) : int;
{  Compresses the source buffer into the destination buffer. The level
   parameter has the same meaning as in deflateInit.  sourceLen is the byte
   length of the source buffer. Upon entry, destLen is the total size of the
   destination buffer, which must be at least 0.1% larger than sourceLen plus
   12 bytes. Upon exit, destLen is the actual size of the compressed buffer.

   compress2 returns Z_OK if success, Z_MEM_ERROR if there was not enough
   memory, Z_BUF_ERROR if there was not enough room in the output buffer,
   Z_STREAM_ERROR if the level parameter is invalid. }

implementation

{ ===========================================================================
}
function compress2 (dest : pBytef;
                    var destLen : uLong;
                    const source : array of byte;
                    sourceLen : uLong;
                    level : int) : int;
var
  stream : z_stream;
  err : int;
begin
  stream.next_in := pBytef(@source);
  stream.avail_in := uInt(sourceLen);
{$ifdef MAXSEG_64K}
  { Check for source > 64K on 16-bit machine: }
  if (uLong(stream.avail_in) <> sourceLen) then
  begin
    compress2 := Z_BUF_ERROR;
    exit;
  end;
{$endif}
  stream.next_out := dest;
  stream.avail_out := uInt(destLen);
  if (uLong(stream.avail_out) <> destLen) then
  begin
    compress2 := Z_BUF_ERROR;
    exit;
  end;

  stream.zalloc := NIL;       { alloc_func(0); }
  stream.zfree := NIL;        { free_func(0); }
  stream.opaque := NIL;       { voidpf(0); }

  err := deflateInit(stream, level);
  if (err <> Z_OK) then
  begin
    compress2 := err;
    exit;
  end;

  err := deflate(stream, Z_FINISH);
  if (err <> Z_STREAM_END) then
  begin
    deflateEnd(stream);
    if err = Z_OK then
      compress2 := Z_BUF_ERROR
    else
      compress2 := err;
    exit;
  end;
  destLen := stream.total_out;

  err := deflateEnd(stream);
  compress2 := err;
end;

{ ===========================================================================
 }
function compress (dest : pBytef;
                   var destLen : uLong;
                   const source : array of Byte;
                   sourceLen : uLong) : int;
begin
  compress := compress2(dest, destLen, source, sourceLen, Z_DEFAULT_COMPRESSION);
end;


end.