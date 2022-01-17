{$UNDEF GUI}
{.$DEFINE GUI}
{.$DEFINE MobileGUI}
program DECDUnitXTestSuite;

// In order to run DEC Unit tests via DUnitX framework the $DEFINE DUnitX define
// in TestDefines.inc must be enabled, as it makes all the unit test units DUnitX
// compatible
{$INCLUDE Tests\TestDefines.inc}

{$IFNDEF GUI}
  {$IFNDEF TESTINSIGHT}
    {$APPTYPE CONSOLE}
  {$ENDIF}
{$ENDIF}

{$STRONGLINKTYPES ON}
uses
  System.SysUtils,
  {$IFDEF TESTINSIGHT}
  TestInsight.Client,
  {$ENDIF }
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Xml.NUnit,
  DUnitX.TestFramework,
  TestDECCRC in 'Tests\TestDECCRC.pas',
  TestDECFormatBase in 'Tests\TestDECFormatBase.pas',
  TestDECFormat in 'Tests\TestDECFormat.pas',
  TestDECUtil in 'Tests\TestDECUtil.pas',
  TestDECHash in 'Tests\TestDECHash.pas',
  TestDECCipher in 'Tests\TestDECCipher.pas',
  TestDECCipherModes in 'Tests\TestDECCipherModes.pas',
  TestDECTestDataContainer in 'Tests\TestDECTestDataContainer.pas',
  TestDECBaseClass in 'Tests\TestDECBaseClass.pas',
  TestDECCipherFormats in 'Tests\TestDECCipherFormats.pas',
  TestDECHashKDF in 'Tests\TestDECHashKDF.pas',
  TestDECRandom in 'Tests\TestDECRandom.pas',
  TestDECHashMAC in 'Tests\TestDECHashMAC.pas',
  TestDECHashSHA3 in 'Tests\TestDECHashSHA3.pas';

function IsTestInsightRunning: Boolean;
{$IFDEF TESTINSIGHT}
var
  client: ITestInsightClient;
begin
  client := TTestInsightRestClient.Create;
  client.StartedTesting(0);
  Result := not client.HasError;
end;
{$ELSE}
begin
  result := false;
end;
{$ENDIF}

var
  runner : ITestRunner;
  results : IRunResults;
  logger : ITestLogger;
  nunitLogger : ITestLogger;
begin

//{$IFDEF GUI}
// // DUnitX.Loggers.GUIX.GUIXTestRunner.Run.Execute;
////  DUnitX.Loggers.GUIX.GUIXTestRunner.Run;
//  DUnitX.Loggers.GUI.VCL.Run;
//  exit;
//{$ENDIF}

  try
    if IsTestInsightRunning then
      {$IFDEF TESTINSIGHT}
      TestInsight.DUnitX.RunRegisteredTests
      {$ENDIF}
    else
    begin
      //Check command line options, will exit if invalid
      TDUnitX.CheckCommandLine;
      //Create the test runner
      runner := TDUnitX.CreateRunner;
      //Tell the runner to use RTTI to find Fixtures
      runner.UseRTTI := True;
      //tell the runner how we will log things
      //Log to the console window
//      {$IFDEF GUI}
//      logger := TGUIXTestRunner.Create(nil);
//      {$ELSE}
      logger := TDUnitXConsoleLogger.Create(true);
//      {$ENDIF}
      runner.AddLogger(logger);
      //Generate an NUnit compatible XML File
      nunitLogger := TDUnitXXMLNUnitFileLogger.Create(TDUnitX.Options.XMLOutputFile);
      runner.AddLogger(nunitLogger);
      runner.FailsOnNoAsserts := False; //When true, Assertions must be made during tests;

      //Run tests
      results := runner.Execute;
      if not results.AllPassed then
        System.ExitCode := EXIT_ERRORS;

      {$IFNDEF CI}
      //We don't want this happening when running under CI.
      if TDUnitX.Options.ExitBehavior = TDUnitXExitBehavior.Pause then
      begin
        System.Write('Done.. press <Enter> key to quit.');
        System.Readln;
      end;
      {$ENDIF}
    end;
  except
    on E: Exception do
      System.Writeln(E.ClassName, ': ', E.Message);
  end;
end.
