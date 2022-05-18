unit IOArchive;

interface

uses
  Threading, Utils, SynCommons, SynCrypto, ParseClass, ParseExpr,
  IOUtils,
  WinAPI.Windows, WinAPI.ShlObj,
  System.SysUtils, System.Classes, System.SyncObjs, System.Math, System.Types,
  System.StrUtils, System.RTLConsts, System.TimeSpan, System.Diagnostics,
  System.IOUtils, System.Generics.Defaults, System.Generics.Collections;

type
  PEncodeOptions = ^TEncodeOptions;

  TEncodeOptions = record

  end;

  PDecodeOptions = ^TDecodeOptions;

  TDecodeOptions = record

  end;

procedure PrintHelp;
procedure Parse(ParamArg: TArray<string>; out Options: TEncodeOptions);
  overload;
procedure Parse(ParamArg: TArray<string>; out Options: TDecodeOptions);
  overload;
procedure Encode(Input: TArray<string>; Output: TStream;
  Options: TEncodeOptions);
procedure Decode(Input: TStream; Output: String; Options: TDecodeOptions);

implementation

procedure PrintHelp;
var
  I, J: Integer;
  S: string;
begin
  WriteLn(ErrOutput, 'archive - convert a group of files into an archive');
  WriteLn(ErrOutput, '');
  WriteLn(ErrOutput, 'Usage:');
  WriteLn(ErrOutput, '  xtool archive files1 files2... archive');
  WriteLn(ErrOutput, '');
  WriteLn(ErrOutput, '');
end;

procedure Parse(ParamArg: TArray<string>; out Options: TEncodeOptions);
var
  ArgParse: TArgParser;
  ExpParse: TExpressionParser;
  S: String;
begin
  ArgParse := TArgParser.Create(ParamArg);
  ExpParse := TExpressionParser.Create;
  try
    S := '';
  finally
    ArgParse.Free;
    ExpParse.Free;
  end;
end;

procedure Parse(ParamArg: TArray<string>; out Options: TDecodeOptions);
var
  ArgParse: TArgParser;
  ExpParse: TExpressionParser;
  S: String;
begin
  ArgParse := TArgParser.Create(ParamArg);
  ExpParse := TExpressionParser.Create;
  try
    S := '';
  finally
    ArgParse.Free;
    ExpParse.Free;
  end;
end;

procedure Encode(Input: TArray<string>; Output: TStream;
  Options: TEncodeOptions);
var
  I, J: Integer;
  K: Word;
  I64: Int64;
  BaseDir: String;
  LList: TArray<String>;
  LBytes: TBytes;
  FStream: TFileStream;
begin
  I := XTOOL_ARCH;
  Output.WriteBuffer(I, I.Size);
  for I := Low(Input) to High(Input) do
  begin
    if FileExists(Input[I]) then
      BaseDir := ExtractFilePath(TPath.GetFullPath(Input[I]))
    else if DirectoryExists(Input[I]) then
      BaseDir := IncludeTrailingBackSlash(TPath.GetFullPath(Input[I]))
    else
      BaseDir := ExtractFilePath(TPath.GetFullPath(Input[I]));
    LList := GetFileList([Input[I]], True);
    if Length(LList) > 0 then
    begin
      J := Length(LList);
      Output.WriteBuffer(J, J.Size);
      for J := Low(LList) to High(LList) do
      begin
        LBytes := BytesOf(ReplaceText(LList[I], BaseDir, ''));
        K := Length(LBytes);
        Output.WriteBuffer(K, K.Size);
        Output.WriteBuffer(LBytes[0], K);
        I64 := FileSize(LList[I]);
        Output.WriteBuffer(I64, I64.Size);
        FStream := TFileStream.Create(LList[I], fmShareDenyNone);
        try
          CopyStreamEx(FStream, Output, I64);
        finally
          FStream.Free;
        end;
      end;
    end;
  end;
  J := J.MinValue;
  Output.WriteBuffer(J, J.Size);
end;

procedure Decode(Input: TStream; Output: String; Options: TDecodeOptions);
var
  I, J: Integer;
  K: Word;
  I64: Int64;
  S: String;
  BaseDir: String;
  LBytes: TBytes;
  FStream: TFileStream;
begin
  BaseDir := IncludeTrailingBackSlash(Output);
  Input.ReadBuffer(I, I.Size);
  while I >= 0 do
  begin
    for J := 1 to I do
    begin
      Input.ReadBuffer(K, K.Size);
      if Length(LBytes) < K then
        SetLength(LBytes, K);
      FillChar(LBytes[0], Length(LBytes), 0);
      Input.ReadBuffer(LBytes[0], K);
      S := BaseDir + StringOf(LBytes);
      if not DirectoryExists(ExtractFilePath(S)) then
        ForceDirectories(ExtractFilePath(S));
      Input.ReadBuffer(I64, I64.Size);
      FStream := TFileStream.Create(S, fmCreate);
      try
        CopyStreamEx(Input, FStream, I64);
      finally
        FStream.Free;
      end;
    end;
    Input.ReadBuffer(I, I.Size);
  end;
end;

end.
