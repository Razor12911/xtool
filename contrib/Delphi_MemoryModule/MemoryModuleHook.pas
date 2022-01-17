// Unit that hooks LoadLibrary, GetProcAddress, FreeLibrary for MemoryModule
// to allow transparent DLL loading.

unit MemoryModuleHook;

interface

uses
  Windows,
  MemoryModule, FuncHook;

type
  // Callback function that is called from LoadLibraryHook to determine
  // an address of library data.
  //   lpLibFileName: name of library to load
  // Returns:
  //   Pointer to library data; nil to bypass MemoryModule and use WinAPI
  TGetLibPtrProc = function (lpLibFileName: PWideChar): Pointer;

function InstallHook(AGetLibPtrCallback: TGetLibPtrProc): Boolean;
function UninstallHook: Boolean;

implementation

var
  HookInstalled: Boolean = False;
  GetLibPtrCallback: TGetLibPtrProc;

  LoadedModules: array of HMODULE;
  CS: RTL_CRITICAL_SECTION;

  LoadLibrary_Old: function (lpLibFileName: PWideChar): HMODULE; stdcall;
  GetProcAddress_Old: function (hModule: HMODULE; lpProcName: LPCSTR): FARPROC; stdcall;
  FreeLibrary_Old: function (hLibModule: HMODULE): BOOL; stdcall;

  HI_LL, HI_GPA, HI_FL: THookInfo;

function IndexOfLoadedModule(hModule: HMODULE): Integer;
var i: Integer;
begin
  EnterCriticalSection(CS);
  try
    for i := Low(LoadedModules) to High(LoadedModules) do
      if LoadedModules[i] = hModule then
        Exit(i);
    Result := -1;
  finally
    LeaveCriticalSection(CS);
  end;
end;

// Try to get library address and load it, run WinAPI routine otherwise.
function LoadLibraryHook(lpLibFileName: PWideChar): HMODULE; stdcall;
var
  LibPtr: Pointer;
begin
  Result := 0;

  LibPtr := GetLibPtrCallback(lpLibFileName);
  if LibPtr = nil then
  begin
    LoadLibrary_Old(lpLibFileName);
    Exit;
  end;

  Result := HMODULE(MemoryLoadLibary(LibPtr));
  if Result <> 0 then
  try
    EnterCriticalSection(CS);
    SetLength(LoadedModules, Length(LoadedModules) + 1);
    LoadedModules[Length(LoadedModules) - 1] := Result;
  finally
    LeaveCriticalSection(CS);
  end;
end;

// If hModule was loaded via MM, run MM's routine. Otherwise, run WinAPI one.
function GetProcAddressHook(hModule: HMODULE; lpProcName: LPCSTR): FARPROC; stdcall;
begin
  if IndexOfLoadedModule(hModule) <> -1 then
    Result := FARPROC(MemoryGetProcAddress(TMemoryModule(hModule), lpProcName))
  else
    Result := GetProcAddress_Old(hModule, lpProcName);
end;

// If hLibModule was loaded via MM, run MM's routine. Otherwise, run WinAPI one.
function FreeLibraryHook(hLibModule: HMODULE): BOOL; stdcall;
var idx: Integer;
begin
  idx := IndexOfLoadedModule(hLibModule);
  if idx <> -1 then
  begin
    MemoryFreeLibrary(TMemoryModule(hLibModule));
    Result := BOOL(True);
    // Remove from the list
    try
      EnterCriticalSection(CS);
      LoadedModules[idx] := 0;
      if idx < Length(LoadedModules) - 1 then
        Move(LoadedModules[idx + 1], LoadedModules[idx], (Length(LoadedModules) - idx + 1)*SizeOf(HMODULE));
      SetLength(LoadedModules, Length(LoadedModules) - 1);
    finally
      LeaveCriticalSection(CS);
    end;
  end
  else
    Result := FreeLibrary_Old(hLibModule);
end;

function InstallHook(AGetLibPtrCallback: TGetLibPtrProc): Boolean;
begin
  Result := False;
  if not Assigned(AGetLibPtrCallback) then Exit;

  EnterCriticalSection(CS);
  try
    if HookInstalled then Exit;

    if not HookProcedure(False, HI_LL) or
       not HookProcedure(False, HI_GPA) or
       not HookProcedure(False, HI_FL) then Exit;

    LoadLibrary_Old := HI_LL.OrigProc;
    GetProcAddress_Old := HI_GPA.OrigProc;
    FreeLibrary_Old := HI_FL.OrigProc;

    HookInstalled := True;
    GetLibPtrCallback := AGetLibPtrCallback;

    Result := True;
  finally
    if not Result then
      UninstallHook;
    LeaveCriticalSection(CS);
  end;
end;

function UninstallHook: Boolean;
begin
  Result := False;

  EnterCriticalSection(CS);
  try
    if not HookInstalled then Exit;

    while Length(LoadedModules) > 0 do
      FreeLibrary(LoadedModules[0]);

    Result :=
      UnhookProcedure(HI_LL) and
      UnhookProcedure(HI_GPA) and
      UnhookProcedure(HI_FL);

    GetLibPtrCallback := nil;
    HookInstalled := False;
  finally
    LeaveCriticalSection(CS);
  end;
end;

initialization
  InitializeCriticalSection(CS);
  HI_LL.Init(@LoadLibrary, @LoadLibraryHook);
  HI_GPA.Init(@GetProcAddress, @GetProcAddressHook);
  HI_FL.Init(@FreeLibrary, @FreeLibraryHook);

finalization
  UninstallHook;
  DeleteCriticalSection(CS);

end.
