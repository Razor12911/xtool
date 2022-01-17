{
  * Implements class that reads a file and provides access to its data.
}

unit UFileData;

interface

uses
  // Project
  UBDiffTypes;

type

  TFileData = class(TObject)
  private
    fData: PSignedAnsiCharArray;
    fSize: Cardinal;
    fName: string;
    procedure LoadFile;
  public
    constructor Create(const FileName: string); overload;
    constructor Create(Memory: Pointer; Size: Integer); overload;
    destructor Destroy; override;
    property Name: string read fName;
    property Size: Cardinal read fSize;
    property Data: PSignedAnsiCharArray read fData;
  end;

implementation

uses
  // Delphi
  SysUtils, Windows,
  // Project
  UErrors;

{ TFileData }

constructor TFileData.Create(const FileName: string);
begin
  inherited Create;
  fName := FileName;
  LoadFile;
end;

constructor TFileData.Create(Memory: Pointer; Size: Integer);
begin
  inherited Create;
  fName := '';
  fData := Memory;
  fSize := Size;
end;

destructor TFileData.Destroy;
begin
  if fName <> '' then
    if Assigned(fData) then
      FreeMem(fData);
  inherited;
end;

procedure TFileData.LoadFile;
var
  FileHandle: Integer;
  BytesRead: Integer;
begin
  FileHandle := FileOpen(fName, fmOpenRead or fmShareDenyWrite);
  try
    if FileHandle = -1 then
      Error('Cannot open file %s', [fName]);
    fSize := GetFileSize(FileHandle, nil);
    if fSize = Cardinal(-1) then
      Error('Cannot find size of file %s - may be to large', [fName]);
    if fSize = 0 then
      Error('File %s is empty', [fName]);
    try
      GetMem(fData, fSize);
      BytesRead := FileRead(FileHandle, fData^, fSize);
      if BytesRead = -1 then
        Error('Cannot read from file %s', [fName]);
      if fSize <> Cardinal(BytesRead) then
        Error('Error reading from file %s', [fName]);
    except
      if Assigned(fData) then
        FreeMem(fData, fSize);
      raise;
    end;
  finally
    if FileHandle <> -1 then
      FileClose(FileHandle);
  end;
end;

end.
