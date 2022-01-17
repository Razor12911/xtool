unit BDiffEncoder;

interface

uses
  UDiffer, ULogger, UBDiffTypes;

procedure MakeDiff(Data1, Data2, Data3: Pointer; const Size1, Size2: Integer;
  var Size3: Integer);

implementation

procedure MakeDiff(Data1, Data2, Data3: Pointer; const Size1, Size2: Integer;
  var Size3: Integer);
var
  Differ: TDiffer;
  Logger: TLogger;
begin
  Logger := TLoggerFactory.Instance(False);
  try
    Differ := TDiffer.Create;
    try
      Differ.Format := TFormat.FMT_BINARY;
      Differ.MakeDiff(Data1, Data2, Data3, Size1, Size2, Size3, Logger);
    finally
      Differ.Free;
    end;
  finally
    Logger.Free;
  end;
end;

end.
