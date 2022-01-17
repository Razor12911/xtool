{
 * Implements an abstract base class for classes that parse command lines.
}


unit UBaseParams;

interface

uses
  // Delphi
  SysUtils;

type

  TBaseParams = class(TObject)
  private
    fHelp: Boolean;
    fVersion: Boolean;
  protected
    class function StripLeadingChars(const S: string; const Count: Integer):
      string;
    procedure Error(const Msg: string); overload;
    procedure Error(const Fmt: string; const Args: array of const); overload;
    function ParseLongOption(const Option: string; var ParamIdx: Integer;
      var Terminated: Boolean): Boolean; virtual;
    function ParseShortOption(const Options: string; const OptionIdx: Integer;
      var ParamIdx: Integer; var Terminated: Boolean): Boolean; virtual;
    procedure ParseFileName(const FileName: string); virtual; abstract;
    procedure Finalize; virtual; abstract;
  public
    constructor Create;
    procedure Parse;
    property Help: Boolean read fHelp default False;
    property Version: Boolean read fVersion default False;
  end;

type
  EParams = class(Exception);

implementation

uses
  // Delphi
  StrUtils,
  // Project
  UErrors;

{ TBaseParams }

procedure TBaseParams.Error(const Msg: string);
begin
  raise EParams.Create(Msg);
end;

constructor TBaseParams.Create;
begin
  inherited Create;
  fVersion := False;
  fHelp := False;
end;

procedure TBaseParams.Error(const Fmt: string; const Args: array of const);
begin
  raise EParams.CreateFmt(Fmt, Args);
end;

procedure TBaseParams.Parse;
var
  ParamIdx: Integer;
  CharIdx: Integer;
  Param: string;
  Terminated: Boolean;
begin
  // Parse command line
  Terminated := False;
  ParamIdx := 1;
  while ParamIdx <= ParamCount do
  begin
    Param := ParamStr(ParamIdx);
    if AnsiStartsStr('-', Param) then
    begin
      if AnsiStartsStr('--', Param) then
      begin
        // long option
        if not ParseLongOption(Param, ParamIdx, Terminated) then
          Error('unknown option ''%s''', [Param]);
        if Terminated then
          Exit;
      end
      else
      begin
        // short options
        for CharIdx := 2 to Length(Param) do
        begin
          if not ParseShortOption(Param, CharIdx, ParamIdx, Terminated) then
            Error('unknown option ''-%s''', [Param[CharIdx]]);
          if Terminated then
            Exit;
        end;
      end;
    end
    else
      ParseFileName(Param);
    Inc(ParamIdx);
  end;
  Finalize;
end;

function TBaseParams.ParseLongOption(const Option: string;
  var ParamIdx: Integer; var Terminated: Boolean): Boolean;
begin
  Result := True;
  if Option = '--help' then
  begin
    fHelp := True;
    Terminated := True;
  end
  else if Option = '--version' then
  begin
    fVersion := True;
    Terminated := True;
  end
  else
    Result := False;
end;

function TBaseParams.ParseShortOption(const Options: string;
  const OptionIdx: Integer; var ParamIdx: Integer;
  var Terminated: Boolean): Boolean;
begin
  Result := True;
  case Options[OptionIdx] of
    'h':
      if OptionIdx = Length(Options) then
      begin
        fHelp := True;
        Terminated := True;
      end;
    'v':
      if OptionIdx = Length(Options) then
      begin
        fVersion := True;
        Terminated := True;
      end;
    else
      Result := False;
  end;
end;

class function TBaseParams.StripLeadingChars(const S: string;
  const Count: Integer): string;
begin
  if Count > 0 then
    Result := AnsiRightStr(S, Length(S) - Count)
  else
    Result := S;
end;

end.
