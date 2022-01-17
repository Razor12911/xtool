program fmxPerformance;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  CoreCipher in '..\..\Source\CoreCipher.pas',
  CoreClasses in '..\..\Source\CoreClasses.pas',
  DoStatusIO in '..\..\Source\DoStatusIO.pas',
  Fast_MD5 in '..\..\Source\Fast_MD5.pas',
  ListEngine in '..\..\Source\ListEngine.pas',
  MemoryStream64 in '..\..\Source\MemoryStream64.pas',
  PascalStrings in '..\..\Source\PascalStrings.pas',
  UnicodeMixedLib in '..\..\Source\UnicodeMixedLib.pas',
  UPascalStrings in '..\..\Source\UPascalStrings.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
