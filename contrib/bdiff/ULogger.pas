{
 * Classes used to log messages plus a factory class. One logger class logs to
 * standard error while the second does nothing.
}


unit ULogger;

interface

type
  TLogger = class(TObject)
  public
    procedure Log(const Msg: string); virtual; abstract;
  end;

type
  TLoggerFactory = class(TObject)
  public
    class function Instance(Verbose: Boolean): TLogger;
  end;

implementation

uses
  UAppInfo, UBDiffUtils;

type
  TVerboseLogger = class(TLogger)
  public
    procedure Log(const Msg: string); override;
  end;

type
  TSilentLogger = class(TLogger)
  public
    procedure Log(const Msg: string); override;
  end;

{ TVerboseLogger }

procedure TVerboseLogger.Log(const Msg: string);
begin
  TIO.WriteStrFmt(TIO.StdErr, '%s: %s'#13#10, [TAppInfo.ProgramFileName, Msg]);
end;

{ TSilentLogger }

procedure TSilentLogger.Log(const Msg: string);
begin
  // Do nothing: no output required
end;

{ TLoggerFactory }

class function TLoggerFactory.Instance(Verbose: Boolean): TLogger;
begin
  if Verbose then
    Result := TVerboseLogger.Create
  else
    Result := TSilentLogger.Create;
end;

end.
