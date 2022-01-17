{
  * Heirachy of classes used to write various types of patch, along with factory
  * class.
  *
  * Patch generation code based on portions of bdiff.c by Stefan Reuther,
  * copyright (c) 1999 Stefan Reuther <Streu@gmx.de>.
}

unit UPatchWriters;

interface

uses
  // Project
  UBDiffTypes;

type

  TPatchWriter = class(TObject)
  private
    FWriteToMemory: Boolean;
  public
    Memory: Pointer;
    Size: Integer;
    procedure Header(const OldFileName, NewFileName: string;
      const OldFileSize, NewFileSize: Cardinal); virtual; abstract;
    procedure Add(Data: PSignedAnsiChar; Length: Cardinal); virtual; abstract;
    procedure Copy(NewBuf: PSignedAnsiCharArray; NewPos: Cardinal;
      OldPos: Cardinal; Length: Cardinal); virtual; abstract;
    property WriteToMemory: Boolean read FWriteToMemory write FWriteToMemory
      default false;
  end;

  TPatchWriterFactory = class(TObject)
  public
    class function Instance(const Format: TFormat): TPatchWriter;
  end;

implementation

uses
  // Delphi
  SysUtils,
  // Project
  UBDiffUtils;

type
  TBinaryPatchWriter = class(TPatchWriter)
  private
    procedure PackLong(P: PSignedAnsiChar; L: Longint);
    function CheckSum(Data: PSignedAnsiChar; Length: Cardinal): Longint;
  public
    procedure Header(const OldFileName, NewFileName: string;
      const OldFileSize, NewFileSize: Cardinal); override;
    procedure Add(Data: PSignedAnsiChar; Length: Cardinal); override;
    procedure Copy(NewBuf: PSignedAnsiCharArray; NewPos: Cardinal;
      OldPos: Cardinal; Length: Cardinal); override;
  end;

  TTextPatchWriter = class(TPatchWriter)
  protected
    { Checks if an ANSI character is a printable ASCII character. }
    class function IsPrint(const Ch: AnsiChar): Boolean;
    procedure CopyHeader(NewPos: Cardinal; OldPos: Cardinal; Length: Cardinal);
    procedure Header(const OldFileName, NewFileName: string;
      const OldFileSize, NewFileSize: Cardinal); override;
  end;

  TQuotedPatchWriter = class(TTextPatchWriter)
  private
    procedure QuotedData(Data: PSignedAnsiChar; Length: Cardinal);
    { Returns octal representation of given value as a 3 digit string. }
    class function ByteToOct(const Value: Byte): string;
  public
    procedure Add(Data: PSignedAnsiChar; Length: Cardinal); override;
    procedure Copy(NewBuf: PSignedAnsiCharArray; NewPos: Cardinal;
      OldPos: Cardinal; Length: Cardinal); override;
  end;

  TFilteredPatchWriter = class(TTextPatchWriter)
  private
    procedure FilteredData(Data: PSignedAnsiChar; Length: Cardinal);
  public
    procedure Add(Data: PSignedAnsiChar; Length: Cardinal); override;
    procedure Copy(NewBuf: PSignedAnsiCharArray; NewPos: Cardinal;
      OldPos: Cardinal; Length: Cardinal); override;
  end;

  { TPatchWriterFactory }

class function TPatchWriterFactory.Instance(const Format: TFormat)
  : TPatchWriter;
begin
  case Format of
    FMT_BINARY:
      Result := TBinaryPatchWriter.Create;
    FMT_FILTERED:
      Result := TFilteredPatchWriter.Create;
    FMT_QUOTED:
      Result := TQuotedPatchWriter.Create;
  else
    raise Exception.Create('Invalid format type');
  end;
end;

{ TBinaryPatchWriter }

procedure TBinaryPatchWriter.Add(Data: PSignedAnsiChar; Length: Cardinal);
var
  Rec: packed record DataLength: array [0 .. 3] of SignedAnsiChar;
  // length of added adata
end;

const
  cPlusSign: AnsiChar = '+'; // flags added data
begin
  Inc(Size, TIO.WriteStr(TIO.StdOut, cPlusSign, FWriteToMemory, Memory, Size));
  PackLong(@Rec.DataLength, Length);
  Inc(Size, TIO.WriteRaw(TIO.StdOut, @Rec, SizeOf(Rec), FWriteToMemory,
    Memory, Size));
  Inc(Size, TIO.WriteRaw(TIO.StdOut, Data, Length, FWriteToMemory,
    Memory, Size));
  // data added
end;

{ Compute simple checksum }
function TBinaryPatchWriter.CheckSum(Data: PSignedAnsiChar;
  Length: Cardinal): Longint;
begin
  Result := 0;
  while Length <> 0 do
  begin
    Dec(Length);
    Result := ((Result shr 30) and 3) or (Result shl 2);
    Result := Result xor Ord(Data^);
    Inc(Data);
  end;
end;

procedure TBinaryPatchWriter.Copy(NewBuf: PSignedAnsiCharArray;
  NewPos, OldPos, Length: Cardinal);
var
  Rec: packed record CopyStart: array [0 .. 3] of SignedAnsiChar;
  // starting pos of copied data
  CopyLength: array [0 .. 3] of SignedAnsiChar; // length copied data
  CheckSum: array [0 .. 3] of SignedAnsiChar; // validates copied data
end;

const
  cAtSign: AnsiChar = '@'; // flags command data in both file
begin
  Inc(Size, TIO.WriteStr(TIO.StdOut, cAtSign, FWriteToMemory, Memory, Size));
  PackLong(@Rec.CopyStart, OldPos);
  PackLong(@Rec.CopyLength, Length);
  PackLong(@Rec.CheckSum, CheckSum(@NewBuf[NewPos], Length));
  Inc(Size, TIO.WriteRaw(TIO.StdOut, @Rec, SizeOf(Rec), FWriteToMemory,
    Memory, Size));
end;

procedure TBinaryPatchWriter.Header(const OldFileName, NewFileName: string;
  const OldFileSize, NewFileSize: Cardinal);
var
  Head: packed record Signature: array [0 .. 7] of SignedAnsiChar;
  // file signature
  OldDataSize: array [0 .. 3] of SignedAnsiChar; // size of old data file
  NewDataSize: array [0 .. 3] of SignedAnsiChar; // size of new data file
end;

const
  // File signature. Must be 8 bytes. Format is 'bdiff' + file-version + #$1A
  // where file-version is a two char string, here '02'.
  // If file format is changed then increment the file version
  cFileSignature: array [0 .. 7] of AnsiChar = 'bdiff02'#$1A;
begin
  Assert(Length(cFileSignature) = 8);
  Move(cFileSignature, Head.Signature[0], Length(cFileSignature));
  PackLong(@Head.OldDataSize, OldFileSize);
  PackLong(@Head.NewDataSize, NewFileSize);
  Inc(Size, TIO.WriteRaw(TIO.StdOut, @Head, SizeOf(Head), FWriteToMemory,
    Memory, Size));
end;

{ Pack long in little-endian format to P }
{ NOTE: P must point to a block of at least 4 bytes }
procedure TBinaryPatchWriter.PackLong(P: PSignedAnsiChar; L: Integer);
begin
  P^ := L and $FF;
  Inc(P);
  P^ := (L shr 8) and $FF;
  Inc(P);
  P^ := (L shr 16) and $FF;
  Inc(P);
  P^ := (L shr 24) and $FF;
end;

{ TTextPatchWriter }

procedure TTextPatchWriter.CopyHeader(NewPos, OldPos, Length: Cardinal);
begin
  Inc(Size, TIO.WriteStrFmt(TIO.StdOut, '@ -[%d] => +[%d] %d bytes'#13#10' ',
    [OldPos, NewPos, Length], FWriteToMemory, Memory, Size));
end;

procedure TTextPatchWriter.Header(const OldFileName, NewFileName: string;
  const OldFileSize, NewFileSize: Cardinal);
begin
  Inc(Size, TIO.WriteStrFmt(TIO.StdOut,
    '%% --- %s (%d bytes)'#13#10'%% +++ %s (%d bytes)'#13#10,
    [OldFileName, OldFileSize, NewFileName, NewFileSize], FWriteToMemory,
    Memory, Size));
end;

class function TTextPatchWriter.IsPrint(const Ch: AnsiChar): Boolean;
begin
  Result := Ch in [#32 .. #126];
end;

{ TQuotedPatchWriter }

procedure TQuotedPatchWriter.Add(Data: PSignedAnsiChar; Length: Cardinal);
begin
  Inc(Size, TIO.WriteStr(TIO.StdOut, '+', FWriteToMemory, Memory, Size));
  QuotedData(Data, Length);
  Inc(Size, TIO.WriteStr(TIO.StdOut, #13#10, FWriteToMemory, Memory, Size));
end;

class function TQuotedPatchWriter.ByteToOct(const Value: Byte): string;
var
  Idx: Integer;
  Digit: Byte;
  Remainder: Byte;
begin
  Result := '';
  Remainder := Value;
  for Idx := 1 to 3 do
  begin
    Digit := Remainder mod 8;
    Remainder := Remainder div 8;
    Result := Chr(Digit + Ord('0')) + Result;
  end;
end;

procedure TQuotedPatchWriter.Copy(NewBuf: PSignedAnsiCharArray;
  NewPos, OldPos, Length: Cardinal);
begin
  CopyHeader(NewPos, OldPos, Length);
  QuotedData(@NewBuf[NewPos], Length);
  Inc(Size, TIO.WriteStr(TIO.StdOut, #13#10, FWriteToMemory, Memory, Size));
end;

procedure TQuotedPatchWriter.QuotedData(Data: PSignedAnsiChar;
  Length: Cardinal);
begin
  while (Length <> 0) do
  begin
    if IsPrint(AnsiChar(Data^)) and (AnsiChar(Data^) <> '\') then
      Inc(Size, TIO.WriteStr(TIO.StdOut, AnsiChar(Data^), FWriteToMemory,
        Memory, Size))
    else
      Inc(Size, TIO.WriteStr(TIO.StdOut, '\' + ByteToOct(Data^ and $FF),
        FWriteToMemory, Memory, Size));
    Inc(Data);
    Dec(Length);
  end;
end;

{ TFilteredPatchWriter }

procedure TFilteredPatchWriter.Add(Data: PSignedAnsiChar; Length: Cardinal);
begin
  Inc(Size, TIO.WriteStr(TIO.StdOut, '+', FWriteToMemory, Memory, Size));
  FilteredData(Data, Length);
  Inc(Size, TIO.WriteStr(TIO.StdOut, #13#10, FWriteToMemory, Memory, Size));
end;

procedure TFilteredPatchWriter.Copy(NewBuf: PSignedAnsiCharArray;
  NewPos, OldPos, Length: Cardinal);
begin
  CopyHeader(NewPos, OldPos, Length);
  FilteredData(@NewBuf[NewPos], Length);
  Inc(Size, TIO.WriteStr(TIO.StdOut, #13#10, FWriteToMemory, Memory, Size));
end;

procedure TFilteredPatchWriter.FilteredData(Data: PSignedAnsiChar;
  Length: Cardinal);
begin
  while Length <> 0 do
  begin
    if IsPrint(AnsiChar(Data^)) then
      Inc(Size, TIO.WriteStr(TIO.StdOut, AnsiChar(Data^), FWriteToMemory,
        Memory, Size))
    else
      Inc(Size, TIO.WriteStr(TIO.StdOut, '.', FWriteToMemory, Memory, Size));
    Inc(Data);
    Dec(Length);
  end;
end;

end.
