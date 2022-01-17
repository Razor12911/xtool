unit BDiffDecoder;

interface

uses
  UPatcher;

procedure MakePatch(Data1, Data2, Data3: Pointer; const Size1, Size2: Integer;
  var Size3: Integer);

implementation

procedure MakePatch(Data1, Data2, Data3: Pointer; const Size1, Size2: Integer;
  var Size3: Integer);
var
  Patcher: TPatcher;
begin
  Patcher := TPatcher.Create;
  try
    Patcher.Apply(Data1, Data2, Data3, Size1, Size2, Size3);
  finally
    Patcher.Free;
  end;
end;

end.
