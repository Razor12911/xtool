{
 * Class that emits BDiff's version information and help screen on standard
 * output.
}


unit UBDiffInfoWriter;

interface

uses
  UInfoWriter;

type
  TBDiffInfoWriter = class(TInfoWriter)
  protected
    class function HelpText: string; override;
  end;

implementation

uses
  SysUtils,
  UAppInfo;

{ TBDiffInfoWriter }

class function TBDiffInfoWriter.HelpText: string;
begin
  Result := Format(
    '%0:s: binary ''diff'' - compare two binary files'#13#10#13#10
      + 'Usage: %0:s [options] old-file new-file [>patch-file]'#13#10#13#10
      + 'Difference between old-file and new-file written to standard output'
      + #13#10#13#10
      + 'Valid options:'#13#10
      + ' -q                   Use QUOTED format'#13#10
      + ' -f                   Use FILTERED format'#13#10
      + ' -b                   Use BINARY format'#13#10
      + '       --format=FMT   Use format FMT (''quoted'', ''filter[ed]'' '
      + 'or ''binary'')'#13#10
      + ' -m N  --min-equal=N  Minimum equal bytes to recognize an equal chunk'
      + #13#10
      + ' -o FN --output=FN    Set output file name (instead of stdout)'#13#10
      + ' -V    --verbose      Show status messages'#13#10
      + ' -h    --help         Show this help screen'#13#10
      + ' -v    --version      Show version information'#13#10
      + #13#10
      + '(c) copyright 1999 Stefan Reuther <Streu@gmx.de>'#13#10
      + '(c) copyright 2003-2016 Peter Johnson (@delphidabbler)'#13#10,
    [TAppInfo.ProgramFileName]
  );
end;

end.
