{
  * Contains utility functions used by both BDiff and BPatch.
}

unit UUtils;

interface

type
  TCommonIO = class(TObject)
  public
    { Returns Windows standard input handle }
    class function StdIn: Integer;
    { Returns Windows standard output handle }
    class function StdOut: Integer;
    { Returns Windows standard error handle }
    class function StdErr: Integer;
    { Writes binary data to a file }
    class function WriteRaw(Handle: THandle; BufPtr: Pointer; Size: Integer;
      Memory: Boolean = False; Mem: Pointer = nil;
      Position: Integer = 0): Integer;
    { Writes a string to a file }
    class function WriteStr(Handle: THandle; const S: UnicodeString;
      Memory: Boolean = False; Mem: Pointer = nil; Position: Integer = 0)
      : Integer; overload;
    class function WriteStr(Handle: THandle; const S: AnsiString;
      Memory: Boolean = False; Mem: Pointer = nil; Position: Integer = 0)
      : Integer; overload;
    { Writes a string built from format string and arguments to file }
    class function WriteStrFmt(Handle: THandle; const Fmt: string;
      Args: array of const; Memory: Boolean = False; Mem: Pointer = nil;
      Position: Integer = 0): Integer;
  end;

implementation

uses
  // Delphi
  SysUtils, Windows;

{ TCommonIO }

class function TCommonIO.StdErr: Integer;
begin
  Result := Integer(Windows.GetStdHandle(STD_ERROR_HANDLE));
end;

class function TCommonIO.StdIn: Integer;
begin
  Result := Integer(Windows.GetStdHandle(STD_INPUT_HANDLE));
end;

class function TCommonIO.StdOut: Integer;
begin
  Result := Integer(Windows.GetStdHandle(STD_OUTPUT_HANDLE));
end;

class function TCommonIO.WriteRaw(Handle: THandle; BufPtr: Pointer;
  Size: Integer; Memory: Boolean = False; Mem: Pointer = nil;
  Position: Integer = 0): Integer;
var
  Dummy: DWORD;
begin
  Result := 0;
  if Size <= 0 then
    Exit;
  Result := Size;
  if Memory then
    Move(BufPtr^, (PByte(Mem) + Position)^, Size)
  else
    Windows.WriteFile(Handle, BufPtr^, Size, Dummy, nil);
end;

class function TCommonIO.WriteStr(Handle: THandle; const S: UnicodeString;
  Memory: Boolean = False; Mem: Pointer = nil; Position: Integer = 0): Integer;
var
  Bytes: TBytes;
begin
  Bytes := TEncoding.Default.GetBytes(S);
  Result := WriteRaw(Handle, Bytes, Length(S), Memory, Mem, Position);
end;

class function TCommonIO.WriteStr(Handle: THandle; const S: AnsiString;
  Memory: Boolean = False; Mem: Pointer = nil; Position: Integer = 0): Integer;
begin
  Result := WriteRaw(Handle, PAnsiChar(S), Length(S), Memory, Mem, Position);
end;

class function TCommonIO.WriteStrFmt(Handle: THandle; const Fmt: string;
  Args: array of const; Memory: Boolean = False; Mem: Pointer = nil;
  Position: Integer = 0): Integer;
begin
  Result := WriteStr(Handle, Format(Fmt, Args), Memory, Mem, Position);
end;

end.
