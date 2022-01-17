{
 * Implements a class that parses command lines and records parameters.
}


unit UBDiffParams;

interface

uses
  // Project
  UBaseParams, UBDiffTypes;

type

  TParams = class(TBaseParams)
  private
    fVerbose: Boolean;
    fMinEqual: Integer;
    fOldFileName: string;
    fPatchFileName: string;
    fNewFileName: string;
    fFormat: TFormat;
    procedure SetFormat(const Value: string);
    procedure SetMinEqual(const Value: string);
  protected
    function ParseLongOption(const Option: string; var ParamIdx: Integer;
      var Terminated: Boolean): Boolean; override;
    function ParseShortOption(const Options: string; const OptionIdx: Integer;
      var ParamIdx: Integer; var Terminated: Boolean): Boolean; override;
    procedure ParseFileName(const FileName: string); override;
    procedure Finalize; override;
  public
    constructor Create;
    property OldFileName: string read fOldFileName;
    property NewFileName: string read fNewFileName;
    property PatchFileName: string read fPatchFileName;
    property MinEqual: Integer read fMinEqual default 24;
    property Verbose: Boolean read fVerbose default False;
    property Help;
    property Version;
    property Format: TFormat read fFormat default FMT_QUOTED;
  end;

implementation

uses
  // Delphi
  SysUtils, StrUtils;

{ TParams }

constructor TParams.Create;
begin
  inherited;
  fOldFileName := '';
  fNewFileName := '';
  fPatchFileName := '';
  fMinEqual := 24;
  fVerbose := False;
  fFormat := FMT_QUOTED;
end;

procedure TParams.Finalize;
begin
  if fNewFileName = '' then
    Error('need two filenames');
  if SameFileName(fOldFileName, fNewFileName) then
    Error('file names must not be the same');
  if SameFileName(fOldFileName, fPatchFileName)
    or SameFileName(fNewFileName, fPatchFileName) then
    Error('output file name must differ from other file names');
end;

procedure TParams.ParseFileName(const FileName: string);
begin
  if fOldFileName = '' then
    fOldFileName := FileName
  else if fNewFileName = '' then
    fNewFileName := FileName
  else
    Error('too many file names on command line');
end;

function TParams.ParseLongOption(const Option: string; var ParamIdx: Integer;
  var Terminated: Boolean): Boolean;
begin
  Result := inherited ParseLongOption(Option, ParamIdx, Terminated);
  if Result then
    Exit;
  Result := True;
  if Option = '--verbose' then
    fVerbose := True
  else if Option = '--output' then
  begin
    Inc(ParamIdx);
    if ParamStr(ParamIdx) = '' then
      Error('missing argument to ''--output''');
    fPatchFileName := ParamStr(ParamIdx);
  end
  else if AnsiStartsStr('--output=', Option) then
    fPatchFileName := StripLeadingChars(Option, Length('--output='))
  else if Option = '--format' then
  begin
    Inc(ParamIdx);
    SetFormat(ParamStr(ParamIdx));
  end
  else if AnsiStartsStr('--format=', Option) then
    SetFormat(StripLeadingChars(Option, Length('--format=')))
  else if Option = '--min-equal' then
  begin
    Inc(ParamIdx);
    SetMinEqual(ParamStr(ParamIdx));
  end
  else if AnsiStartsStr('--min-equal=', Option) then
    SetMinEqual(StripLeadingChars(Option, Length('--min-equal=')))
  else
    Result := False;
end;

function TParams.ParseShortOption(const Options: string;
  const OptionIdx: Integer; var ParamIdx: Integer; var Terminated: Boolean):
  Boolean;
begin
  Result := inherited ParseShortOption(
    Options, OptionIdx, ParamIdx, Terminated
  );
  if Result then
    Exit;
  Result := True;
  case Options[OptionIdx] of
    'V':
      fVerbose := True;
    'q':
      fFormat := FMT_QUOTED;
    'f':
      fFormat := FMT_FILTERED;
    'b':
      fFormat := FMT_BINARY;
    'm':
    begin
      Inc(ParamIdx);
      SetMinEqual(ParamStr(ParamIdx));
    end;
    'o':
    begin
      Inc(ParamIdx);
      if ParamStr(ParamIdx) = '' then
        Error('missing argument to ''-o''');
      fPatchFileName := ParamStr(ParamIdx);
    end;
    else
      Result := False;
  end;
end;

procedure TParams.SetFormat(const Value: string);
begin
  if Value = '' then
    Error('missing argument to ''--format''');
  if Value = 'quoted' then
    fFormat := FMT_QUOTED
  else if (Value = 'filter') or (Value = 'filtered') then
    fFormat := FMT_FILTERED
  else if Value = 'binary' then
    fFormat := FMT_BINARY
  else
    Error('invalid format specification');
end;

procedure TParams.SetMinEqual(const Value: string);
var
  X: Int64; // number parsed from command line
begin
  if Value = '' then
    Error('missing argument to ''--min-equal'' / ''-m''');
  if not TryStrToInt64(Value, X) or (X < 0) then
    Error('malformed number on command line');
  if (X = 0) or (X > $7FFF) then
    Error('number out of range on command line');
  fMinEqual := Integer(X);
end;

end.

