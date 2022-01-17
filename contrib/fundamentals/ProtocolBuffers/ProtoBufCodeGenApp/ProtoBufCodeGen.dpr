program ProtoBufCodeGen;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  flcDynArrays in '..\..\Utils\flcDynArrays.pas',
  flcStrings in '..\..\Utils\flcStrings.pas',
  flcProtoBufUtils in '..\flcProtoBufUtils.pas',
  flcProtoBufProtoNodes in '..\flcProtoBufProtoNodes.pas',
  flcProtoBufProtoParser in '..\flcProtoBufProtoParser.pas',
  flcProtoBufProtoParserTests in '..\flcProtoBufProtoParserTests.pas',
  flcProtoBufProtoCodeGenPascal in '..\flcProtoBufProtoCodeGenPascal.pas',
  flcUtils in '..\..\Utils\flcUtils.pas',
  flcStdTypes in '..\..\Utils\flcStdTypes.pas',
  flcASCII in '..\..\Utils\flcASCII.pas',
  flcFloats in '..\..\Utils\flcFloats.pas',
  flcStringBuilder in '..\..\Utils\flcStringBuilder.pas';

const
  AppVersion = '1.0.1';

procedure PrintTitle;
begin
  Writeln('Proto code generator ', AppVersion);
end;

procedure PrintHelp;
begin
  Writeln('Usage:');
  Writeln('ProtoCodeGen <input .proto file> [ <options> ]');
  Writeln('<options>');
  Writeln('  --proto_path=<import path for .proto files>');
  Writeln('  -I same as as --proto_path');
  Writeln('  --pas_out=<output path for .pas files>');
  Writeln('  -O same as --pas_out');
  Writeln('  --pas_ver=<output delphi version for .pas files; 0: less delpi XE, 1: only delphi XE, 2: all>');
  Writeln('  -V= same as --pas_ver');

  Writeln('  --help');
end;

procedure PrintError(const ErrorStr: String);
begin
  Writeln('Error: ', ErrorStr);
end;

var
  // command line paramaters
  ParamInputFile  : String;
  ParamOutputPath : String;
  ParamProtoPath  : String;

  ParamSupportVersion: TCodeGenSupportVersion = cgsvLessXE;

  // app
  InputFileFull : String;
  InputFilePath : String;
  InputFileName : String;
  OutputPath    : String;

procedure ProcessParameters;
var L, I, N: Integer;
    S : String;
begin
  L := ParamCount;
  if L = 0 then
    begin
      PrintHelp;
      Halt(1);
    end;
  for I := 1 to L do
    begin
      S := ParamStr(I);

      if StrMatchLeft(S, '--pas_ver=', False) then
      begin
        Delete(S, 1, 10);
        N := StrToIntDef(S, 0);
        if (N >= Integer(Low(TCodeGenSupportVersion))) and (N <= Integer(High(TCodeGenSupportVersion))) then
          ParamSupportVersion := TCodeGenSupportVersion(N);
      end
      else if StrMatchLeft(S, '-V=', False) then
      begin
        Delete(S, 1, 3);
        N := StrToIntDef(S, 0);
        if (N >= Integer(Low(TCodeGenSupportVersion))) and (N <= Integer(High(TCodeGenSupportVersion))) then
          ParamSupportVersion := TCodeGenSupportVersion(N);
      end
      else if StrMatchLeft(S, '--pas_out=', False) then
      begin
        Delete(S, 1, 10);
        ParamOutputPath := S;
      end
      else if StrMatchLeft(S, '-O=', False) then
      begin
        Delete(S, 1, 3);
        ParamOutputPath := S;
      end
      else if StrMatchLeft(S, '--proto_path=', False) then
      begin
        Delete(S, 1, 13);
        ParamProtoPath := S;
      end
      else if StrMatchLeft(S, '-I=', False) then
      begin
        Delete(S, 1, 3);
        ParamProtoPath := S;
      end
      else if StrEqualNoAsciiCase(S, '--help') then
      begin
        PrintHelp;
        Halt(1);
      end
      else
      begin
        ParamInputFile := S;
      end;
    end;
end;

procedure InitialiseApp;
begin
  if ParamInputFile = '' then
    begin
      PrintError('No input file specified');
      PrintHelp;
      Halt(1);
    end;
  InputFileFull := ExpandFileName(ParamInputFile);
  InputFilePath := ExtractFilePath(InputFileFull);
  InputFileName := ExtractFileName(InputFileFull);
  if ParamOutputPath <> '' then
    OutputPath := ParamOutputPath
  else
    OutputPath := InputFilePath;
end;

var
  Package : TpbProtoPackage;

procedure ParseInputFile;
var
  Parser : TpbProtoParser;
begin
  Parser := TpbProtoParser.Create;
  try
    Parser.ProtoPath := ParamProtoPath;
    Parser.SetFileName(ParamInputFile);
    Package := Parser.Parse(GetPascalProtoNodeFactory);
  finally
    Parser.Free;
  end;
end;

procedure ProduceOutputFiles;
var
  CodeGen : TpbProtoCodeGenPascal;
begin
  CodeGen := TpbProtoCodeGenPascal.Create;
  try
    CodeGen.OutputPath := OutputPath;
    CodeGen.GenerateCode(Package, ParamSupportVersion);
  finally
    FreeAndNil(CodeGen);
  end;
end;

begin
  {$IFDEF TEST}
  flcProtoBufUtils.Test;
  flcProtoBufProtoParserTests.Test;
  {$ENDIF}

  PrintTitle;
  try
    ProcessParameters;
    InitialiseApp;
    Writeln(InputFileName);
    ParseInputFile;
    try
      ProduceOutputFiles;
    finally
      FreeAndNil(Package);
    end;
  except
    on E: Exception do
      PrintError(E.Message);
  end;
end.
