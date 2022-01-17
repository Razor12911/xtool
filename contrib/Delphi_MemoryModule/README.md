MemoryModule — loading DLL from memory (Delphi adaptation)
==========================================================

*This code is Delphi translation of MemoryModule.c file by [Joachim Bauch](https://github.com/fancycode/MemoryModule) with addition of two helper units that enable using MM engine completely transparently.*

*Resource loading and exe loading, custom functions, user data not implemented yet.*

*Tested under RAD Studio XE2 and XE6 32/64-bit, Lazarus 32-bit. Demo project included.*

Features in brief
-----------------

With the MemoryModule engine you can store all required DLLs inside your binary to keep it standalone. Additional hook units allow transparent using of MM engine thus allowing switching MM/WinAPI loading as well as enabling 3rd party dynamic-load DLL interfaces that are unaware of MM (tested with Interbase Express components and Firebird client library).

In other words, you can do things like this

```delphi
try
  ms := TMemoryStream.Create;
  ms.LoadFromFile(ParamStr(1));
  ms.Position := 0;
  lib := MemoryLoadLibary(ms.Memory);
  ms.Free;
  if lib = nil then Exit;

  func := TNativeUIntFunc(MemoryGetProcAddress(lib, PAnsiChar(AnsiString(ParamStr(2)))));
  if @func = nil then Exit;
  
  WriteLn(func);
finally
  MemoryFreeLibrary(lib);
end;
```

or even like this
```delphi
if not InstallHook(@GetLibPtrProc) then
begin
  Writeln('Error installing hook');
  Exit;
end;

try
  ms := TMemoryStream.Create;
  ms.LoadFromFile(ParamStr(1));
  ms.Position := 0;
  lib := Pointer(LoadLibrary(PChar(ParamStr(1))));
  ms.Free;
  if lib = nil then Exit;

  func := TNativeUIntFunc(GetProcAddress(HMODULE(lib), PAnsiChar(AnsiString(ParamStr(2)))));
  if @func = nil then Exit;
  
  WriteLn(func);
finally
  FreeLibrary(HMODULE(lib));
  UninstallHook;
end;

```

See demo project for samples. Good testing sample of parameters is `%WinDir%\System32\KernelBase.dll` and `GetCurrentThread`. Note that `kernel32.dll` and some of the other Windows libraries couldn't be loaded with MM.

References
----------

* [Joahim's article](https://github.com/fancycode/MemoryModule/tree/master/doc)
* [TLS Callbacks](http://thelegendofrandom.com/blog/archives/2418)
* [PE description](https://code.google.com/p/corkami/wiki/PE)