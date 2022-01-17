program Project2;

{$APPTYPE CONSOLE}

uses
  SysUtils, Classes, Windows,
  MemoryModule in '..\MemoryModule.pas',
  FuncHook in '..\FuncHook.pas',
  MemoryModuleHook in '..\MemoryModuleHook.pas';

const
  SUsage =
    'Test project for loading DLL from memory'+sLineBreak+
    'Params:'+sLineBreak+
    '  [DLL name] (required) - full path to DLL to load'+sLineBreak+
    '  [Function name] (optional) - function to execute (no parameters, result is DWORD/Handle/Pointer)'+sLineBreak+
    'Good testing sample is <%WinDir%\System32\KernelBase.dll> and <GetCurrentThread>';
type
  TNativeUIntFunc = function: NativeUInt;
var
  ms: TMemoryStream;
  lib : TMemoryModule;
  func: TNativeUIntFunc;
  res: array[0..2] of NativeUInt;
  i: Integer;

function GetLibPtrProc(lpLibFileName: PWideChar): Pointer;
begin
  // Catch only those paths that start with *, let others go
  if lpLibFileName^ = '*' then
    Result := ms.Memory;
end;

function CheckLoadLib(lib: Pointer): Boolean;
begin
  if lib = nil then
  begin
    Writeln('Error loading lib '+ParamStr(1)+': '+SysErrorMessage(GetLastError));
    Exit(False);
  end;
  Writeln(ParamStr(1)+' loaded');
  Exit(True);
end;

function CheckLoadAndExecFunc(func: TNativeUIntFunc; out res: NativeUInt): Boolean;
begin
  if @func = nil then
  begin
    Writeln('Error loading func '+ParamStr(2)+': '+SysErrorMessage(GetLastError));
    Exit(False);
  end;
  res := func;
  Writeln(Format('Function call result: %u (%x)', [res, res]));
  Exit(True);
end;

begin
  try
    if ParamCount = 0 then
    begin
      Writeln(SUsage);
      Exit;
    end;

    Writeln('===== Test #0, usual load =====');

    try
      lib := Pointer(LoadLibrary(PChar(ParamStr(1))));
      if not CheckLoadLib(lib) then Exit;

      if ParamStr(2) <> '' then
      begin
        func := TNativeUIntFunc(GetProcAddress(HMODULE(lib), PAnsiChar(AnsiString(ParamStr(2)))));
        if not CheckLoadAndExecFunc(func, res[0]) then Exit;
      end;
    finally
      FreeLibrary(HMODULE(lib));
    end;

    Writeln('===== Test #1, load from memory =====');

    try
      ms := TMemoryStream.Create;
      ms.LoadFromFile(ParamStr(1));
      ms.Position := 0;
      lib := MemoryLoadLibary(ms.Memory);
      ms.Free;
      if not CheckLoadLib(lib) then Exit;

      if ParamStr(2) <> '' then
      begin
        func := TNativeUIntFunc(MemoryGetProcAddress(lib, PAnsiChar(AnsiString(ParamStr(2)))));
        if not CheckLoadAndExecFunc(func, res[1]) then Exit;
      end;
    finally
      MemoryFreeLibrary(lib);
    end;

    Writeln('===== Test #2, load with hooking =====');

    if not InstallHook(@GetLibPtrProc) then
    begin
      Writeln('Error installing hook');
      Exit;
    end;

    try
      ms := TMemoryStream.Create;
      ms.LoadFromFile(ParamStr(1));
      ms.Position := 0;
      // Custom lib names example:
      // Adding * char to the lib path for callback to distinguish whether it should act
      lib := Pointer(LoadLibrary(PChar('*'+ParamStr(1))));
      ms.Free;
      if not CheckLoadLib(lib) then Exit;

      if ParamStr(2) <> '' then
      begin
        func := TNativeUIntFunc(GetProcAddress(HMODULE(lib), PAnsiChar(AnsiString(ParamStr(2)))));
        if not CheckLoadAndExecFunc(func, res[2]) then Exit;
      end;
    finally
      FreeLibrary(HMODULE(lib));
      UninstallHook;
    end;

    if ParamStr(2) <> '' then
    begin
      Writeln('===== Test #3, comparing results =====');
      for i := Low(res) to High(res) do
        if res[i] <> res[0] then
        begin
          Writeln('Failure! Results vary');
          Exit;
        end;
      Writeln('Success! Results identical')
    end;

  except on E: Exception do
    Writeln('Error: '+E.Message);
  end;
  Readln;
end.
