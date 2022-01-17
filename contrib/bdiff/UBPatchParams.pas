{
 * Implements a class that parses command lines and records parameters.
}


unit UBPatchParams;

interface

uses
  // Project
  UBaseParams;

type

  TParams = class(TBaseParams)
  private
    fOldFileName: string;
    fNewFileName: string;
    fPatchFileName: string;
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
    property Help;
    property Version;
  end;

implementation

uses
  // Delphi
  StrUtils;

{ TParams }

constructor TParams.Create;
begin
  inherited;
  fOldFileName := '';
  fNewFileName := '';
  fPatchFileName := '';
end;

procedure TParams.Finalize;
begin
  if fOldFileName = '' then
    Error('file name argument missing');
  if fNewFileName = '' then
    fNewFileName := fOldFileName;
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
  if Option = '--input' then
  begin
    Inc(ParamIdx);
    if ParamStr(ParamIdx) = '' then
      Error('missing argument to ''--input''');
    fPatchFileName := ParamStr(ParamIdx);
  end
  else if AnsiStartsStr('--input=', Option) then
    fPatchFileName := StripLeadingChars(Option, Length('--input='))
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
    'i':
    begin
      Inc(ParamIdx);
      if ParamStr(ParamIdx) = '' then
        Error('missing argument to ''-i''');
      fPatchFileName := ParamStr(ParamIdx);
    end
    else
      Result := False;
  end;
end;

end.

