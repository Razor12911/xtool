unit DbgUtils;

interface

uses
  System.SysUtils, System.Classes, System.StrUtils, System.Types, System.Math,
  System.Generics.Defaults, System.Generics.Collections;

resourcestring
  SPrecompSep1 = '+';
  SPrecompSep2 = ':';
  SPrecompSep3 = ',';

const
  XTOOL_DB = $42445458;

type
  PEntryStruct = ^TEntryStruct;

  TEntryStruct = record
    Position: Int64;
    OldSize, NewSize: Integer;
  end;

  TEntryStructComparer = class(TComparer<TEntryStruct>)
  public
    function Compare(const Left, Right: TEntryStruct): Integer; override;
  end;

var
  EntryStructCmp: TEntryStructComparer;

implementation

function TEntryStructComparer.Compare(const Left, Right: TEntryStruct): Integer;
begin
  Result := Integer(CompareValue(Left.Position, Right.Position));
end;

initialization

EntryStructCmp := TEntryStructComparer.Create;

end.
