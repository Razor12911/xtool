{
  * Class that applies patch to source file to re-create destination file.
  *
  * Based on bpatch.c by Stefan Reuther, copyright (c) 1999 Stefan Reuther
  * <Streu@gmx.de>.
}

unit UPatcher;

interface

type
  TPatcher = class(TObject)
  private
    { Compute simple checksum }
    class function CheckSum(Data: PAnsiChar; DataSize: Cardinal;
      const BFCheckSum: Longint): Longint;
    { Get 32-bit quantity from char array }
    class function GetLong(PCh: PAnsiChar): Longint;
    { Copy data from one stream to another, computing checksums
      @param SourceFileHandle [in] Handle to file containing data to be copied.
      @param DestFileHandle [in] Handle to file to receive copied data.
      @param Count [in] Number of bytes to copy.
      @param SourceCheckSum [in] Checksum for data to be copied
      @param SourceIsPatch [in] Flag True when SourceFileHandle is patch file and
      False when SourceFileHandle is source file.
    }
    class procedure CopyData(const SourceFileHandle, DestFileHandle: Integer;
      Count, SourceCheckSum: Longint; const SourceIsPatch: Boolean); overload;
    class procedure CopyData(SourceMemory, DestMemory: Pointer;
      Count, SourceCheckSum: Longint; const SourceIsPatch: Boolean;
      var Position1, Position2, OutSize: Integer); overload;
    { Creates a temporary file in user's temp directory and returns its name }
    class function GetTempFileName: string;
  public
    { Apply patch from standard input to SourceFileName and regenerate
      DestFileName. }
    class procedure Apply(const SourceFileName, DestFileName: string); overload;
    class procedure Apply(OldData, DiffData, NewData: Pointer;
      OldSize, DiffSize: Integer; var NewSize: Integer); overload;
  end;

implementation

{$IOCHECKS OFF}

uses
  // Delphi
  Windows, SysUtils,
  // Project
  UAppInfo, UBPatchInfoWriter, UBPatchParams, UBPatchUtils, UErrors;

procedure ShowMessage(Msg: string; Caption: string = '');
begin
  MessageBox(0, PWideChar(Msg), PWideChar(Caption), MB_OK or MB_TASKMODAL);
end;

const
  FORMAT_VERSION = '02'; // binary diff file format version
  BUFFER_SIZE = 4096; // size of buffer used to read files

  { TPatcher }

class procedure TPatcher.Apply(const SourceFileName, DestFileName: string);
var
  SourceFileHandle: Integer; // source file handle
  DestFileHandle: Integer; // destination file handle
  TempFileName: string; // temporary file name
  Header: array [0 .. 15] of AnsiChar; // patch file header
  SourceLen: Longint; // expected length of source file
  DestLen: Longint; // expected length of destination file
  DataSize: Longint; // size of data to be copied to destination
  SourceFilePos: Longint; // position in source file
  Ch: Integer; // next character from patch, or EOF
const
  ErrorMsg = 'Patch garbled - invalid section ''%''';
begin
  try
    // read header from patch file on standard input
    if FileRead(TIO.StdIn, Header, 16) <> 16 then
      Error('Patch not in BINARY format');
    if StrLComp(Header, PAnsiChar('bdiff' + FORMAT_VERSION + #$1A), 8) <> 0 then
      Error('Patch not in BINARY format');
    // get length of source and destination files from header
    SourceLen := GetLong(@Header[8]);
    DestLen := GetLong(@Header[12]);

    DestFileHandle := 0;
    // open source file
    SourceFileHandle := FileOpen(SourceFileName, fmOpenRead + fmShareDenyNone);
    try
      if SourceFileHandle <= 0 then
        OSError;

      // check destination file name
      if Length(DestFileName) = 0 then
        Error('Empty destination file name');

      // create temporary file
      TempFileName := GetTempFileName;
      DestFileHandle := FileCreate(TempFileName);
      if DestFileHandle <= 0 then
        Error('Can''t create temporary file');

      { apply patch }
      while True do
      begin
        Ch := TIO.GetCh(TIO.StdIn);
        if Ch = EOF then
          Break;
        case Ch of
          Integer('@'):
            begin
              // common block: copy from source
              if FileRead(TIO.StdIn, Header, 12) <> 12 then
                Error('Patch garbled - unexpected end of data');
              DataSize := GetLong(@Header[4]);
              SourceFilePos := GetLong(@Header[0]);
              if (SourceFilePos < 0) or (DataSize <= 0) or
                (SourceFilePos > SourceLen) or (DataSize > SourceLen) or
                (DataSize + SourceFilePos > SourceLen) then
                Error('Patch garbled - invalid change request');
              if not TIO.Seek(SourceFileHandle, SourceFilePos, SEEK_SET) then
                Error('Seek on source file failed');
              CopyData(SourceFileHandle, DestFileHandle, DataSize,
                GetLong(@Header[8]), False);
              Dec(DestLen, DataSize);
            end;
          Integer('+'):
            begin
              // add data from patch file
              if FileRead(TIO.StdIn, Header, 4) <> 4 then
                Error('Patch garbled - unexpected end of data');
              DataSize := GetLong(@Header[0]);
              CopyData(TIO.StdIn, DestFileHandle, DataSize, 0, True);
              Dec(DestLen, DataSize);
            end;
        else
          Error('Patch garbled - invalid section ''%s''', [Char(Ch)]);
        end;
        if DestLen < 0 then
          Error('Patch garbled - patch file longer than announced in header');
      end;
      if DestLen <> 0 then
        Error('Patch garbled - destination file shorter than announced in header');

    finally
      FileClose(SourceFileHandle);
      FileClose(DestFileHandle);
    end;
    // create destination file: overwrites any existing dest file with same name
    SysUtils.DeleteFile(DestFileName);
    if not RenameFile(TempFileName, DestFileName) then
      Error('Can''t rename temporary file');
  except
    on E: Exception do
    begin
      SysUtils.DeleteFile(TempFileName);
      raise;
    end;
  end;
end;

class procedure TPatcher.Apply(OldData, DiffData, NewData: Pointer;
  OldSize, DiffSize: Integer; var NewSize: Integer);
var
  Position1, Position2, Position3, Size1, Size2, Size3: Integer;
  Header: array [0 .. 15] of AnsiChar; // patch file header
  SourceLen: Longint; // expected length of source file
  DestLen: Longint; // expected length of destination file
  DataSize: Longint; // size of data to be copied to destination
  SourceFilePos: Longint; // position in source file
  Ch: Integer; // next character from patch, or EOF
  B: AnsiChar;
const
  ErrorMsg = 'Patch garbled - invalid section ''%''';
begin
  Position1 := 0;
  Position2 := 0;
  Position3 := 0;
  Size1 := OldSize;
  Size2 := DiffSize;
  Size3 := 0;
  try
    // read header from patch file on standard input
    Move((PByte(DiffData) + Position2)^, Header, 16);
    Inc(Position2, 16);
    if StrLComp(Header, PAnsiChar('bdiff' + FORMAT_VERSION + #$1A), 8) <> 0 then
      Error('Patch not in BINARY format');
    // get length of source and destination files from header
    SourceLen := GetLong(@Header[8]);
    DestLen := GetLong(@Header[12]);

    try
      { apply patch }
      while True do
      begin
        if Position2 >= Size2 then
          Ch := EOF
        else
        begin
          Move((PByte(DiffData) + Position2)^, B, SizeOf(B));
          Ch := Integer(B);
          Inc(Position2, SizeOf(AnsiChar));
        end;
        if Ch = EOF then
          Break;
        case Ch of
          Integer('@'):
            begin
              // common block: copy from source
              Move((PByte(DiffData) + Position2)^, Header, 12);
              Inc(Position2, 12);
              DataSize := GetLong(@Header[4]);
              SourceFilePos := GetLong(@Header[0]);
              if (SourceFilePos < 0) or (DataSize <= 0) or
                (SourceFilePos > SourceLen) or (DataSize > SourceLen) or
                (DataSize + SourceFilePos > SourceLen) then
                Error('Patch garbled - invalid change request');
              Position1 := SourceFilePos;
              CopyData((PByte(OldData) + Position1),
                (PByte(NewData) + Position3), DataSize, GetLong(@Header[8]),
                False, Position1, Position3, Size3);
              Dec(DestLen, DataSize);
            end;
          Integer('+'):
            begin
              // add data from patch file
              Move((PByte(DiffData) + Position2)^, Header, 4);
              Inc(Position2, 4);
              DataSize := GetLong(@Header[0]);
              CopyData((PByte(DiffData) + Position2),
                (PByte(NewData) + Position3), DataSize, 0, True, Position2,
                Position3, Size3);
              Dec(DestLen, DataSize);
            end;
        else
          Error('Patch garbled - invalid section ''%s''', [Char(Ch)]);
        end;
        if DestLen < 0 then
          Error('Patch garbled - patch file longer than announced in header');
      end;
      if DestLen <> 0 then
        Error('Patch garbled - destination file shorter than announced in header');

    finally

    end;
  except
    on E: Exception do
    begin
      raise;
    end;
  end;
  NewSize := Size3;
end;

class function TPatcher.CheckSum(Data: PAnsiChar; DataSize: Cardinal;
  const BFCheckSum: Integer): Longint;
begin
  Result := BFCheckSum;
  while DataSize <> 0 do
  begin
    Dec(DataSize);
    Result := ((Result shr 30) and 3) or (Result shl 2);
    Result := Result xor PShortInt(Data)^;
    Inc(Data);
  end;
end;

class procedure TPatcher.CopyData(const SourceFileHandle, DestFileHandle
  : Integer; Count, SourceCheckSum: Integer; const SourceIsPatch: Boolean);
var
  DestCheckSum: Longint;
  Buffer: array [0 .. BUFFER_SIZE - 1] of AnsiChar;
  BytesToCopy: Cardinal;
begin
  DestCheckSum := 0;

  while Count <> 0 do
  begin
    if Count > BUFFER_SIZE then
      BytesToCopy := BUFFER_SIZE
    else
      BytesToCopy := Count;
    if FileRead(SourceFileHandle, Buffer, BytesToCopy) <> Integer(BytesToCopy)
    then
    begin
      if TIO.AtEOF(SourceFileHandle) then
      begin
        if SourceIsPatch then
          Error('Patch garbled - unexpected end of data')
        else
          Error('Source file does not match patch');
      end
      else
      begin
        if SourceIsPatch then
          Error('Error reading patch file')
        else
          Error('Error reading source file');
      end;
    end;
    if DestFileHandle <> 0 then
      if FileWrite(DestFileHandle, Buffer, BytesToCopy) <> Integer(BytesToCopy)
      then
        Error('Error writing temporary file');
    DestCheckSum := CheckSum(Buffer, BytesToCopy, DestCheckSum);
    Dec(Count, BytesToCopy);
  end;
  if not SourceIsPatch and (DestCheckSum <> SourceCheckSum) then
    Error('Source file does not match patch');
end;

class procedure TPatcher.CopyData(SourceMemory, DestMemory: Pointer;
  Count, SourceCheckSum: Integer; const SourceIsPatch: Boolean;
  var Position1, Position2, OutSize: Integer);
var
  DestCheckSum: Longint;
  BytesToCopy: Cardinal;
  Pos: Integer;
begin
  DestCheckSum := 0;
  Pos := 0;
  Inc(Position1, Count);
  Inc(Position2, Count);
  Inc(OutSize, Count);
  while Count <> 0 do
  begin
    if Count > BUFFER_SIZE then
      BytesToCopy := BUFFER_SIZE
    else
      BytesToCopy := Count;

    Move((PByte(SourceMemory) + Pos)^, (PByte(DestMemory) + Pos)^, BytesToCopy);
    // Move((PByte(SourceMemory) + Pos)^, Buffer, BytesToCopy);
    // Move(Buffer, (PByte(DestMemory) + Pos)^, BytesToCopy);
    DestCheckSum := CheckSum(PAnsiChar((PByte(SourceMemory) + Pos)),
      BytesToCopy, DestCheckSum);
    // DestCheckSum := CheckSum(Buffer, BytesToCopy, DestCheckSum);
    Inc(Pos, BytesToCopy);
    Dec(Count, BytesToCopy);
  end;
  if not SourceIsPatch and (DestCheckSum <> SourceCheckSum) then
    Error('Source file does not match patch');
end;

class function TPatcher.GetLong(PCh: PAnsiChar): Longint;
var
  PB: PByte;
  LW: LongWord;
begin
  PB := PByte(PCh);
  LW := PB^;
  Inc(PB);
  LW := LW + 256 * PB^;
  Inc(PB);
  LW := LW + 65536 * PB^;
  Inc(PB);
  LW := LW + 16777216 * PB^;
  Result := LW;
end;

class function TPatcher.GetTempFileName: string;
begin
  // Get temporary folder
  SetLength(Result, Windows.MAX_PATH);
  Windows.GetTempPath(Windows.MAX_PATH, PChar(Result));
  // Get unique temporary file name (it is created as side effect of this call)
  if Windows.GetTempFileName(PChar(Result), '', 0, PChar(Result)) = 0 then
    Error('Can''t create temporary file');
  Result := PChar(Result)
end;

end.
