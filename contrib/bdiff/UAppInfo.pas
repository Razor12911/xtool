{
 * Provides information about program: file name, date and version info.
 * Common code used by both BDiff and BPatch.
}


unit UAppInfo;


interface

type
  TAppInfo = class(TObject)
  private
    { Fully specified file name of program, with absolute path }
    class function ProgramPath: string;
  public
    { Name of program's executable file, without path }
    class function ProgramFileName: string;
    { Name of program, without file extension }
    class function ProgramBaseName: string;
    { Program's product version number }
    class function ProgramVersion: string;
    { Last modification date of program's executable file }
    class function ProgramExeDate: string;
  end;


implementation

uses
  // Delphi
  SysUtils, Windows;


{ TAppInfo }

class function TAppInfo.ProgramBaseName: string;
begin
  Result := ChangeFileExt(ProgramFileName, '');
end;

class function TAppInfo.ProgramExeDate: string;
var
  FileDate: TDateTime;  // date stamp of exe file
begin
  if FileAge(ProgramPath, FileDate) then
    Result := FormatDateTime('dd mmm yyyy', FileDate)
  else
    Result := '';
end;

class function TAppInfo.ProgramFileName: string;
begin
  Result := ExtractFileName(ProgramPath);
end;

class function TAppInfo.ProgramPath: string;
begin
  Result := ParamStr(0);
end;

class function TAppInfo.ProgramVersion: string;
var
  Dummy: DWORD;           // unused variable required in API calls
  VerInfoSize: Integer;   // size of version information data
  VerInfoBuf: Pointer;    // buffer holding version information
  ValPtr: Pointer;        // pointer to a version information value
  FFI: TVSFixedFileInfo;  // fixed file information from version info
begin
  Result := '';
  // Get fixed file info from program's version info
  // get size of version info
  VerInfoSize := GetFileVersionInfoSize(PChar(ProgramPath), Dummy);
  if VerInfoSize > 0 then
  begin
    // create buffer and read version info into it
    GetMem(VerInfoBuf, VerInfoSize);
    try
      if GetFileVersionInfo(
        PChar(ProgramPath), Dummy, VerInfoSize, VerInfoBuf
      ) then
      begin
        // get fixed file info from version info (ValPtr points to it)
        if VerQueryValue(VerInfoBuf, '\', ValPtr, Dummy) then
        begin
          FFI := PVSFixedFileInfo(ValPtr)^;
          // Build version info string from product version field of FFI
          Result := Format(
            '%d.%d.%d',
            [
              HiWord(FFI.dwProductVersionMS),
              LoWord(FFI.dwProductVersionMS),
              HiWord(FFI.dwProductVersionLS)
            ]
          );
        end
      end;
    finally
      FreeMem(VerInfoBuf);
    end;
  end;
end;

end.
