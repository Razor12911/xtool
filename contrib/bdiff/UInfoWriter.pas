{
 * Abstract base class for classes that emit version information and help
 * screens on standard output.
}


unit UInfoWriter;

interface

type
  TInfoWriter = class(TObject)
  protected
    class function HelpText: string; virtual; abstract;
  public
    class procedure VersionInfo;
    class procedure HelpScreen;
  end;

implementation

uses
  UAppInfo, UUtils;


{ TInfoWriter }

class procedure TInfoWriter.HelpScreen;
begin
  TCommonIO.WriteStr(TCommonIO.StdOut, HelpText);
end;

class procedure TInfoWriter.VersionInfo;
begin
  // NOTE: original code displayed compile date using C's __DATE__ macro. Since
  // there is no Pascal equivalent of __DATE__ we display update date of program
  // file instead
  TCommonIO.WriteStrFmt(
    TCommonIO.StdOut,
    '%s-%s %s '#13#10,
    [TAppInfo.ProgramBaseName, TAppInfo.ProgramVersion, TAppInfo.ProgramExeDate]
  );
end;

end.
