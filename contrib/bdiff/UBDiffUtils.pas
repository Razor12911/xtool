{
 * Contains utility functions used for BDiff. Includes Pascal implementations
 * of, or alternatives for, some standard C library code.
}


unit UBDiffUtils;

interface

uses
  // Project
  UUtils;

type
  TIO = class(TCommonIO)
  public
    { Redirects standard output to a given file handle }
    class procedure RedirectStdOut(const Handle: Integer);
  end;

implementation

uses
  // Delphi
  Windows;

{ TIO }

class procedure TIO.RedirectStdOut(const Handle: Integer);
begin
  Windows.SetStdHandle(STD_OUTPUT_HANDLE, Cardinal(Handle));
end;

end.

