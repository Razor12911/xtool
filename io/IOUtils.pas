unit IOUtils;

interface

uses
  WinAPI.Windows,
  System.SysUtils, System.Classes, System.StrUtils, System.Types, System.Math,
  System.Generics.Defaults, System.Generics.Collections;

const
  XTOOL_IODEC = $314C5458;
  XTOOL_EXEC = $324C5458;
  XTOOL_MAPSUF1 = '-tmp';
  XTOOL_MAPSUF2 = '_mapped.io';
  XTOOL_MAPSUF3 = '.tmp';

type
  PEntryStruct1 = ^TEntryStruct1;

  TEntryStruct1 = packed record
    Filename: String[255];
    Position: Int64;
    Size: Integer;
  end;

  TEntryStructComparer = class(TComparer<TEntryStruct1>)
  public
    function Compare(const Left, Right: TEntryStruct1): Integer; override;
  end;

  TPatchOp = (opNone, opDelete, opMissing, opDifferent);

  PEntryStruct2 = ^TEntryStruct2;

  TEntryStruct2 = packed record
    Op: TPatchOp;
    Filename: String[255];
    Size: Int64;
  end;

var
  EntryStructCmp: TEntryStructComparer;

implementation

function TEntryStructComparer.Compare(const Left, Right: TEntryStruct1)
  : Integer;
begin
  Result := Integer(CompareValue(Left.Position, Right.Position));
end;

initialization

EntryStructCmp := TEntryStructComparer.Create;

end.
