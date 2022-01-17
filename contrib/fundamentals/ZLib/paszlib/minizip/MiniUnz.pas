Program MiniUnz;

{ mini unzip demo package by Gilles Vollant

  Usage : miniunz [-exvlo] file.zip [file_to_extract]

  -l or -v list the content of the zipfile.
        -e extract a specific file or all files if [file_to_extract] is missing
        -x like -e, but extract without path information
        -o overwrite an existing file without warning

  Pascal tranlastion
  Copyright (C) 2000 by Jacques Nomssi Nzali
  For conditions of distribution and use, see copyright notice in readme.txt
}

{$ifdef WIN32}
  {$define Delphi}
  {$ifndef FPC}
    {$define Delphi32}
  {$endif}
{$endif}

uses
  {$ifdef Delphi}
  SysUtils, Windows,
  {$else}
  WinDos, strings,
  {$endif}
  zutil,
  gzlib, ziputils,
  unzip;

const
  CASESENSITIVITY = 0;
  WRITEBUFFERSIZE = 8192;


{ change_file_date : change the date/time of a file
    filename : the filename of the file where date/time must be modified
    dosdate : the new date at the MSDos format (4 bytes)
    tmu_date : the SAME new date at the tm_unz format }

procedure change_file_date(const filename : PChar;
                           dosdate : uLong;
                           tmu_date : tm_unz);
{$ifdef Delphi32}
var
  hFile : THandle;
  ftm,ftLocal,ftCreate,ftLastAcc,ftLastWrite : TFileTime;
begin
  hFile := CreateFile(filename,GENERIC_READ or GENERIC_WRITE,
                      0,NIL,OPEN_EXISTING,0,0);
  GetFileTime(hFile, @ftCreate, @ftLastAcc, @ftLastWrite);
  DosDateTimeToFileTime(WORD((dosdate shl 16)), WORD(dosdate), ftLocal);
  LocalFileTimeToFileTime(ftLocal, ftm);
  SetFileTime(hFile,@ftm, @ftLastAcc, @ftm);
  CloseHandle(hFile);
end;
{$else}
  {$ifdef FPC}
var
  hFile : THandle;
  ftm,ftLocal,ftCreate,ftLastAcc,ftLastWrite : TFileTime;
begin
  hFile := CreateFile(filename,GENERIC_READ or GENERIC_WRITE,
                      0,NIL,OPEN_EXISTING,0,0);
  GetFileTime(hFile, @ftCreate, @ftLastAcc, @ftLastWrite);
  DosDateTimeToFileTime(WORD((dosdate shl 16)), WORD(dosdate), @ftLocal);
  LocalFileTimeToFileTime(ftLocal, @ftm);
  SetFileTime(hFile,ftm, ftLastAcc, ftm);
  CloseHandle(hFile);
end;
  {$else} {  msdos }
var
  f: file;
begin
  Assign(f, filename);
  Reset(f, 1); { open file for reading }
   { (Otherwise, close will update time) }
  SetFTime(f,dosDate);
  Close(f);
end;
  {$endif}
{$endif}


{ mymkdir and change_file_date are not 100 % portable
  As I don't know well Unix, I wait feedback for the unix portion }

function mymkdir(dirname : PChar) : boolean;
var
  S : string;
begin
  S := StrPas(dirname);
  {$I-}
  mkdir(S);
  mymkdir := IOresult = 0;
end;

function makedir (newdir : PChar) : boolean;
var
  buffer  : PChar;
  p : PChar;
  len : int;
var
  hold : char;
begin
  makedir := false;
  len := strlen(newdir);

  if (len <= 0) then
    exit;

  buffer := PChar(zcalloc (NIL, len+1, 1));

  strcopy(buffer,newdir);

  if (buffer[len-1] = '/') then
    buffer[len-1] := #0;

  if mymkdir(buffer) then
  begin
    if Assigned(buffer) then
      zcfree(NIL, buffer);
    makedir := true;
    exit;
  end;

  p := buffer+1;
  while true do
  begin
    while( (p^<>#0) and (p^ <> '\') and (p^ <> '/') ) do
      Inc(p);
    hold := p^;
    p^ := #0;
    if (not mymkdir(buffer)) {and (errno = ENOENT)} then
    begin
      WriteLn('couldn''t create directory ',buffer);
      if Assigned(buffer) then
        zcfree(NIL, buffer);
      exit;
    end;
    if (hold = #0) then
      break;
    p^ := hold;
    Inc(p);
  end;
  if Assigned(buffer) then
    zcfree(NIL, buffer);
  makedir := true;
end;

procedure do_banner;
begin
  WriteLn('MiniUnz 0.15, demo package written by Gilles Vollant');
  WriteLn('Pascal port by Jacques Nomssi Nzali');
  WriteLn('more info at http://wwww.tu-chemnitz.de/~nomssi/paszlib.html');
  WriteLn;
end;

procedure do_help;
begin
  WriteLn('Usage : miniunz [-exvlo] file.zip [file_to_extract]');
  WriteLn;
end;

function LeadingZero(w : Word) : String;
var
  s : String;
begin
  Str(w:0,s);
  if Length(s) = 1 then
    s := '0' + s;
  LeadingZero := s;
end;

function HexToStr(w : long) : string;
const
  ByteToChar : array[0..$F] of char ='0123456789ABCDEF';
var
  s : string;
  i : int;
  x : long;
begin
  s := '';
  x := w;
  for i := 0 to 3 do
  begin
    s := ByteToChar[Byte(x) shr 4] + ByteToChar[Byte(x) and $F] + s;
    x := x shr 8;
  end;
  HexToStr := s;
end;

function do_list(uf : unzFile) : int;
var
  i : uLong;
  gi : unz_global_info;
  err : int;
var
  filename_inzip : array[0..255] of char;
  file_info : unz_file_info;
  ratio : uLong;
  string_method : string[255];
var
  iLevel : uInt;
begin
  err := unzGetGlobalInfo(uf, gi);
  if (err <> UNZ_OK) then
    WriteLn('error ',err,' with zipfile in unzGetGlobalInfo');
  WriteLn(' Length  Method   Size  Ratio   Date    Time   CRC-32     Name');
  WriteLn(' ------  ------   ----  -----   ----    ----   ------     ----');
  for i := 0 to gi.number_entry-1 do
  begin
    ratio := 0;
    err := unzGetCurrentFileInfo(uf, @file_info, filename_inzip, sizeof(filename_inzip),NIL,0,NIL,0);
    if (err <> UNZ_OK) then
    begin
      WriteLn('error ',err,' with zipfile in unzGetCurrentFileInfo');
      break;
    end;
    if (file_info.uncompressed_size>0) then
      ratio := (file_info.compressed_size*100) div file_info.uncompressed_size;

    if (file_info.compression_method=0) then
      string_method := 'Stored'
    else
    if (file_info.compression_method=Z_DEFLATED) then
    begin
      iLevel := uInt((file_info.flag and $06) div 2);
      Case iLevel of
       0: string_method := 'Defl:N';
       1: string_method := 'Defl:X';
       2,3: string_method := 'Defl:F'; { 2:fast , 3 : extra fast}
       else
         string_method := 'Unkn. ';
      end;
    end;

    WriteLn(file_info.uncompressed_size:7, '  ',
          string_method:6, ' ',
          file_info.compressed_size:7, ' ',
    ratio:3,'%  ',  LeadingZero(uLong(file_info.tmu_date.tm_mon)+1),'-',
          LeadingZero(uLong(file_info.tmu_date.tm_mday)):2,'-',
    LeadingZero(uLong(file_info.tmu_date.tm_year mod 100)):2,'  ',
    LeadingZero(uLong(file_info.tmu_date.tm_hour)),':',
          LeadingZero(uLong(file_info.tmu_date.tm_min)),'  ',
    HexToStr(uLong(file_info.crc)),'  ',
          filename_inzip);

    if ((i+1)<gi.number_entry) then
    begin
      err := unzGoToNextFile(uf);
      if (err <> UNZ_OK) then
      begin
        WriteLn('error ',err,' with zipfile in unzGoToNextFile');
        break;
      end;
    end;
  end;

  do_list := 0;
end;


function do_extract_currentfile(
    uf : unzFile;
    const popt_extract_without_path : int;
    var popt_overwrite : int) : int;
var
  filename_inzip : packed array[0..255] of char;
  filename_withoutpath : PChar;
  p: PChar;
  err : int;
  fout : FILEptr;
  buf : pointer;
  size_buf : uInt;
  file_info : unz_file_info;
var
  write_filename : PChar;
  skip : int;
var
  rep : char;
  ftestexist : FILEptr;
var
  answer : string[127];
var
  c : char;
begin
  fout := NIL;

  err := unzGetCurrentFileInfo(uf, @file_info, filename_inzip,
    sizeof(filename_inzip), NIL, 0, NIL,0);

  if (err <> UNZ_OK) then
  begin
    WriteLn('error ',err, ' with zipfile in unzGetCurrentFileInfo');
    do_extract_currentfile := err;
    exit;
  end;

  size_buf := WRITEBUFFERSIZE;
  buf := zcalloc (NIL, size_buf, 1);
  if (buf = NIL) then
  begin
    WriteLn('Error allocating memory');
    do_extract_currentfile := UNZ_INTERNALERROR;
    exit;
  end;

  filename_withoutpath := filename_inzip;
  p := filename_withoutpath;
  while (p^ <> #0) do
  begin
    if (p^='/') or (p^='\') then
      filename_withoutpath := p+1;
    Inc(p);
  end;

  if (filename_withoutpath^=#0) then
  begin
    if (popt_extract_without_path=0) then
    begin
      WriteLn('creating directory: ',filename_inzip);
      mymkdir(filename_inzip);
    end;
  end
  else
  begin

    skip := 0;
    if (popt_extract_without_path=0) then
      write_filename := filename_inzip
    else
      write_filename := filename_withoutpath;

    err := unzOpenCurrentFile(uf);
    if (err <> UNZ_OK) then
      WriteLn('error ',err,' with zipfile in unzOpenCurrentFile');


    if ((popt_overwrite=0) and (err=UNZ_OK)) then
    begin
      rep := #0;
      
      ftestexist := fopen(write_filename,fopenread);
      if (ftestexist <> NIL) then
      begin
        fclose(ftestexist);
  repeat
    Write('The file ',write_filename,
            ' exist. Overwrite ? [y]es, [n]o, [A]ll: ');
    ReadLn(answer);

    rep := answer[1] ;
    if ((rep>='a') and (rep<='z')) then
      Dec(rep, $20);
  until (rep = 'Y') or (rep = 'N') or (rep = 'A');
      end;

      if (rep = 'N') then
        skip := 1;

      if (rep = 'A') then
        popt_overwrite := 1;
    end;

    if (skip=0) and (err=UNZ_OK) then
    begin
      fout := fopen(write_filename,fopenwrite);

      { some zipfile don't contain directory alone before file }
      if (fout=NIL) and (popt_extract_without_path=0) and
        (filename_withoutpath <> PChar(@filename_inzip)) then
      begin
        c := (filename_withoutpath-1)^;
        (filename_withoutpath-1)^ := #0;
        makedir(write_filename);
        (filename_withoutpath-1)^ := c;
        fout := fopen(write_filename, fopenwrite);
      end;

      if (fout=NIL) then
        WriteLn('error opening ',write_filename);
    end;

    if (fout <> NIL) then
    begin
      WriteLn(' extracting: ',write_filename);

      repeat
        err := unzReadCurrentFile(uf,buf,size_buf);
  if (err<0) then
  begin
    WriteLn('error ',err,' with zipfile in unzReadCurrentFile');
    break;
  end;
  if (err>0) then
        begin
    if (fwrite(buf,err,1,fout) <> 1) then
    begin
      WriteLn('error in writing extracted file');
            err := UNZ_ERRNO;
      break;
    end;
        end;
      until (err=0);
      fclose(fout);
      if (err=0) then
        change_file_date(write_filename,file_info.dosDate,
     file_info.tmu_date);
    end;

    if (err=UNZ_OK) then
    begin
      err := unzCloseCurrentFile (uf);
      if (err <> UNZ_OK) then
  WriteLn('error ',err,' with zipfile in unzCloseCurrentFile')
      else
        unzCloseCurrentFile(uf); { don't lose the error }
    end;
  end;

  if buf <> NIL then
    zcfree(NIL, buf);
  do_extract_currentfile := err;
end;


function do_extract(uf : unzFile;
                    opt_extract_without_path : int;
                    opt_overwrite : int) : int;
var
  i : uLong;
  gi : unz_global_info;
  err : int;
begin
  err := unzGetGlobalInfo (uf, gi);
  if (err <> UNZ_OK) then
    WriteLn('error ',err,' with zipfile in unzGetGlobalInfo ');

  for i:=0 to gi.number_entry-1 do
  begin
    if (do_extract_currentfile(uf, opt_extract_without_path,
                                   opt_overwrite) <> UNZ_OK) then
      break;

    if ((i+1)<gi.number_entry) then
    begin
      err := unzGoToNextFile(uf);
      if (err <> UNZ_OK) then
      begin
  WriteLn('error ',err,' with zipfile in unzGoToNextFile');
        break;
      end;
    end;
  end;

  do_extract := 0;
end;

function do_extract_onefile(uf : unzFile;
                            const filename : PChar;
                            opt_extract_without_path : int;
                            opt_overwrite : int) : int;
begin
  if (unzLocateFile(uf,filename,CASESENSITIVITY) <> UNZ_OK) then
  begin
    WriteLn('file ',filename,' not found in the zipfile');
    do_extract_onefile := 2;
    exit;
  end;

  if (do_extract_currentfile(uf, opt_extract_without_path,
                              opt_overwrite) = UNZ_OK) then
    do_extract_onefile := 0
  else
    do_extract_onefile := 1;
end;

{ -------------------------------------------------------------------- }
function main : int;
const
  zipfilename : PChar = NIL;
  filename_to_extract : PChar = NIL;
var
  i : int;
  opt_do_list : int;
  opt_do_extract : int;
  opt_do_extract_withoutpath : int;
  opt_overwrite : int;
  filename_try : array[0..512-1] of char;
  uf : unzFile;
var
  p : int;
  pstr : string[255];
  c : char;
begin
  opt_do_list := 0;
  opt_do_extract := 1;
  opt_do_extract_withoutpath := 0;
  opt_overwrite := 0;
  uf := NIL;

  do_banner;
  if (ParamCount=0) then
  begin
    do_help;
    Halt(0);
  end
  else
  begin
    for i := 1 to ParamCount do
    begin
      pstr := ParamStr(i);
      if pstr[1]='-' then
      begin
        for p := 2 to Length(pstr) do
        begin
          c := pstr[p];
          Case UpCase(c) of
          'L',
          'V' : opt_do_list := 1;
          'X' : opt_do_extract := 1;
          'E' : begin
                  opt_do_extract := 1;
                  opt_do_extract_withoutpath := 1;
                end;
          'O' : opt_overwrite := 1;
          end;
        end;
      end
      else
      begin
        pstr := pstr + #0;
        if (zipfilename = NIL) then
          zipfilename := StrNew(PChar(@pstr[1]))
        else
          if (filename_to_extract=NIL) then
            filename_to_extract := StrNew(PChar(@pstr[1]));
      end;
    end; { for }
  end;

  if (zipfilename <> NIL) then
  begin
    strcopy(filename_try,zipfilename);
    uf := unzOpen(zipfilename);
    if (uf = NIL) then
    begin
      strcat(filename_try,'.zip');
      uf := unzOpen(filename_try);
    end;
  end;

  if (uf = NIL) then
  begin
    WriteLn('Cannot open ',zipfilename,' or ',zipfilename,'.zip');
    Halt(1);
  end;

  WriteLn(filename_try,' opened');

  if (opt_do_list=1) then
  begin
    main := do_list(uf);
    exit;
  end
  else
    if (opt_do_extract=1) then
    begin
      if (filename_to_extract = NIL) then
      begin
  main := do_extract(uf,opt_do_extract_withoutpath,opt_overwrite);
        exit;
      end
      else
      begin
        main := do_extract_onefile(uf,filename_to_extract,
                    opt_do_extract_withoutpath,opt_overwrite);
        exit;
      end;
    end;

  unzCloseCurrentFile(uf);

  strDispose(zipfilename);
  strDispose(filename_to_extract);
  main := 0;
end;

begin
  main;
  Write('Done...');
  Readln;
end.
