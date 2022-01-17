Unit ziputils;

{ ziputils.pas - IO on .zip files using zlib
  - definitions, declarations and routines used by both
    zip.pas and unzip.pas
    The file IO is implemented here.

  based on work by Gilles Vollant

  March 23th, 2000,
  Copyright (C) 2000 Jacques Nomssi Nzali }

interface

{$undef UseStream}
{$ifdef WIN32}
  {$define Delphi}
  {$ifdef UseStream}
    {$define Streams}
  {$endif}
{$endif}

uses
  {$ifdef Delphi}
  classes, SysUtils,
  {$endif}
  zutil;

{ -------------------------------------------------------------- }
{$ifdef Streams}
type
  FILEptr = TFileStream;
{$else}
type
  FILEptr = ^file;
{$endif}
type
  seek_mode = (SEEK_SET, SEEK_CUR, SEEK_END);
  open_mode = (fopenread, fopenwrite, fappendwrite);

function fopen(filename : PChar; mode : open_mode) : FILEptr;

procedure fclose(fp : FILEptr);

function fseek(fp : FILEptr; recPos : uLong; mode : seek_mode) : int;

function fread(buf : voidp; recSize : uInt;
               recCount : uInt; fp : FILEptr) : uInt;

function fwrite(buf : voidp;  recSize : uInt;
                recCount : uInt; fp : FILEptr) : uInt;

function ftell(fp : FILEptr) : uLong;  { ZIP }

function feof(fp : FILEptr) : uInt;   { MiniZIP }

{ ------------------------------------------------------------------- }

type
  zipFile = voidp;
  unzFile = voidp;
type
  z_off_t = long;

{ tm_zip contain date/time info }
type
  tm_zip = record
     tm_sec : uInt;            { seconds after the minute - [0,59] }
     tm_min : uInt;            { minutes after the hour - [0,59] }
     tm_hour : uInt;           { hours since midnight - [0,23] }
     tm_mday : uInt;           { day of the month - [1,31] }
     tm_mon : uInt;            { months since January - [0,11] }
     tm_year : uInt;           { years - [1980..2044] }
  end;

 tm_unz = tm_zip;

const
  Z_BUFSIZE = (16384);
  Z_MAXFILENAMEINZIP = (256);

const
  CENTRALHEADERMAGIC = $02014b50;

const
  SIZECENTRALDIRITEM = $2e;
  SIZEZIPLOCALHEADER = $1e;

function ALLOC(size : int) : voidp;

procedure TRYFREE(p : voidp);

const
  Paszip_copyright : PChar = ' Paszip Copyright 2000 Jacques Nomssi Nzali ';

implementation

function ALLOC(size : int) : voidp;
begin
  ALLOC := zcalloc (NIL, size, 1);
end;

procedure TRYFREE(p : voidp);
begin
  if Assigned(p) then
    zcfree(NIL, p);
end;

{$ifdef Streams}
{ ---------------------------------------------------------------- }

function fopen(filename : PChar; mode : open_mode) : FILEptr;
var
  fp : FILEptr;
begin
  fp := NIL;
  try
    Case mode of
    fopenread: fp := TFileStream.Create(filename, fmOpenRead);
    fopenwrite: fp := TFileStream.Create(filename, fmCreate);
    fappendwrite :
      begin
        fp := TFileStream.Create(filename, fmOpenReadWrite);
        fp.Seek(soFromEnd, 0);
      end;
    end;
  except
    on EFOpenError do
      fp := NIL;
  end;
  fopen := fp;
end;

procedure fclose(fp : FILEptr);
begin
  fp.Free;
end;

function fread(buf : voidp;
               recSize : uInt;
               recCount : uInt;
               fp : FILEptr) : uInt;
var
  totalSize, readcount : uInt;
begin
  if Assigned(buf) then
  begin
    totalSize := recCount * uInt(recSize);
    readCount := fp.Read(buf^, totalSize);
    if (readcount <> totalSize) then
      fread := readcount div recSize
    else
      fread := recCount;
  end
  else
    fread := 0;
end;

function fwrite(buf : voidp;
                recSize : uInt;
                recCount : uInt;
                fp : FILEptr) : uInt;
var
  totalSize, written : uInt;
begin
  if Assigned(buf) then
  begin
    totalSize := recCount * uInt(recSize);
    written := fp.Write(buf^, totalSize);
    if (written <> totalSize) then
      fwrite := written div recSize
    else
      fwrite := recCount;
  end
  else
    fwrite := 0;
end;

function fseek(fp : FILEptr;
               recPos : uLong;
               mode : seek_mode) : int;
const
  fsmode : array[seek_mode] of Word
    = (soFromBeginning, soFromCurrent, soFromEnd);
begin
  fp.Seek(recPos, fsmode[mode]);
  fseek := 0; { = 0 for success }
end;

function ftell(fp : FILEptr) : uLong;
begin
  ftell := fp.Position;
end;

function feof(fp : FILEptr) : uInt;
begin
  feof := 0;
  if Assigned(fp) then
    if fp.Position = fp.Size then
      feof := 1
    else
      feof := 0;
end;

{$else}
{ ---------------------------------------------------------------- }

function fopen(filename : PChar; mode : open_mode) : FILEptr;
var
  fp : FILEptr;
  OldFileMode : byte;
begin
  fp := NIL;
  OldFileMode := FileMode;

  GetMem(fp, SizeOf(file));
  Assign(fp^, filename);
  {$i-}
  Case mode of
  fopenread:
    begin
      FileMode := 0;
      Reset(fp^, 1);
    end;
  fopenwrite:
    begin
      FileMode := 1;
      ReWrite(fp^, 1);
    end;
  fappendwrite :
    begin
      FileMode := 2;
      Reset(fp^, 1);
      Seek(fp^, FileSize(fp^));
    end;
  end;
  FileMode := OldFileMode;
  if IOresult<>0 then
  begin
    FreeMem(fp, SizeOf(file));
    fp := NIL;
  end;

  fopen := fp;
end;

procedure fclose(fp : FILEptr);
begin
  if Assigned(fp) then
  begin
    {$i-}
    system.close(fp^);
    if IOresult=0 then;
    FreeMem(fp, SizeOf(file));
  end;
end;

function fread(buf : voidp;
               recSize : uInt;
               recCount : uInt;
               fp : FILEptr) : uInt;
var
  totalSize, readcount : uInt;
begin
  if Assigned(buf) then
  begin
    totalSize := recCount * uInt(recSize);
    {$i-}
    system.BlockRead(fp^, buf^, totalSize, readcount);
    if (readcount <> totalSize) then
      fread := readcount div recSize
    else
      fread := recCount;
  end
  else
    fread := 0;
end;

function fwrite(buf : voidp;
                recSize : uInt;
                recCount : uInt;
                fp : FILEptr) : uInt;
var
  totalSize, written : uInt;
begin
  if Assigned(buf) then
  begin
    totalSize := recCount * uInt(recSize);
    {$i-}
    system.BlockWrite(fp^, buf^, totalSize, written);
    if (written <> totalSize) then
      fwrite := written div recSize
    else
      fwrite := recCount;
  end
  else
    fwrite := 0;
end;

function fseek(fp : FILEptr;
               recPos : uLong;
               mode : seek_mode) : int;
begin
  {$i-}
  case mode of
    SEEK_SET : system.Seek(fp^, recPos);
    SEEK_CUR : system.Seek(fp^, FilePos(fp^)+recPos);
    SEEK_END : system.Seek(fp^, FileSize(fp^)-1-recPos); { ?? check }
  end;
  fseek := IOresult; { = 0 for success }
end;

function ftell(fp : FILEptr) : uLong;
begin
  ftell := FilePos(fp^);
end;

function feof(fp : FILEptr) : uInt;
begin
  feof := 0;
  if Assigned(fp) then
    if eof(fp^) then
      feof := 1
    else
      feof := 0;
end;

{$endif}
{ ---------------------------------------------------------------- }

end.
