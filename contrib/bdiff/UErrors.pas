{
 * Helper routines to generate exceptions.
 * Common code used by both BDiff and BPatch.
}


unit UErrors;


interface


{ Raises an exception with given message }
procedure Error(const Msg: string); overload;

{ Raises an exception with message created from format string and values }
procedure Error(const Fmt: string; const Args: array of const); overload;

{ Raises exception determined by last operating system error }
procedure OSError;


implementation


uses
  // Project
  Sysutils;


{ Raises an exception with given message }
procedure Error(const Msg: string);
begin
  raise Exception.Create(Msg);
end;

{ Raises an exception with message created from format string and values }
procedure Error(const Fmt: string; const Args: array of const);
begin
  raise Exception.CreateFmt(Fmt, Args);
end;

{ Raises exception determined by last operating system error }
procedure OSError;
var
  LastError: Integer;
  Err: EOSError;
begin
  LastError := GetLastError;
  if LastError <> 0 then
    Err := EOSError.Create(SysErrorMessage(LastError))
  else
    Err := EOSError.Create('Unknown operating system error');
  Err.ErrorCode := LastError;
  raise Err;
end;

end.
