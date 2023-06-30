program testucrt;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  libgcc in 'libgcc.pas',
  testlibgcc in 'testlibgcc.pas';

begin
  try
    { TODO -oUser -cConsole Main : Insert code here }
    test_libgcc;
    readln;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
