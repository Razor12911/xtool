// To compile under FPC, Delphi mode must be used
// Also define CPUX64 for simplicity
{$IFDEF FPC}
  {$mode delphi}
  {$IFDEF CPU64}
    {$DEFINE CPUX64}
  {$ENDIF}
{$ENDIF}

unit MemoryModule;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  * Memory DLL loading code
  * ------------------------
  *
  * Original C Code
  * Memory DLL loading code
  * Version 0.0.4
  *
  * Copyright (c) 2004-2015 by Joachim Bauch / mail@joachim-bauch.de
  * http://www.joachim-bauch.de
  *
  * The contents of this file are subject to the Mozilla Public License Version
  * 2.0 (the "License"); you may not use this file except in compliance with
  * the License. You may obtain a copy of the License at
  * http://www.mozilla.org/MPL/
  *
  * Software distributed under the License is distributed on an "AS IS" basis,
  * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  * for the specific language governing rights and limitations under the
  * License.
  *
  * The Original Code is MemoryModule.c
  *
  * The Initial Developer of the Original Code is Joachim Bauch.
  *
  * Portions created by Joachim Bauch are Copyright (C) 2004-2015
  * Joachim Bauch. All Rights Reserved.
  *
  * ================== MemoryModule "Conversion to Delphi" ==================
  *
  * Copyright (c) 2015 by Fr0sT / https://github.com/Fr0sT-Brutal
  *
  * Initially based on the code by:
  *   Copyright (c) 2005 - 2006 by Martin Offenwanger / coder@dsplayer.de / http://www.dsplayer.de
  *   Carlo Pasolini / cdpasop@hotmail.it / http://pasotech.altervista.org
  *
  * NOTE
  *   This code is Delphi translation of original C code taken from https://github.com/fancycode/MemoryModule
  *     (commit dc173ca from Mar 1, 2015).
  *   Resource loading and exe loading, custom functions, user data not implemented yet.
  *   Tested under RAD Studio XE2 and XE6 32/64-bit, Lazarus 32-bit
  * }

interface

uses
  Windows;

type
  TMemoryModule = Pointer;

  { ++++++++++++++++++++++++++++++++++++++++++++++++++
    ***  Memory DLL loading functions Declaration  ***
    -------------------------------------------------- }

// return value is nil if function fails
function MemoryLoadLibary(Data: Pointer): TMemoryModule; stdcall;
// return value is nil if function fails
function MemoryGetProcAddress(Module: TMemoryModule; const Name: PAnsiChar): Pointer; stdcall;
// free module
procedure MemoryFreeLibrary(Module: TMemoryModule); stdcall;

implementation

  { ++++++++++++++++++++++++++++++++++++++++
    ***  Missing Windows API Definitions ***
    ---------------------------------------- }
  {$IF NOT DECLARED(IMAGE_BASE_RELOCATION)}
  type
  {$ALIGN 4}
  IMAGE_BASE_RELOCATION = record
    VirtualAddress: DWORD;
    SizeOfBlock: DWORD;
  end;
  {$ALIGN ON}
  PIMAGE_BASE_RELOCATION = ^IMAGE_BASE_RELOCATION;
  {$IFEND}

  // Types that are declared in Pascal-style (ex.: PImageOptionalHeader); redeclaring them in C-style

  {$IF NOT DECLARED(PIMAGE_DATA_DIRECTORY)}
  type PIMAGE_DATA_DIRECTORY = ^IMAGE_DATA_DIRECTORY;
  {$IFEND}

  {$IF NOT DECLARED(PIMAGE_SECTION_HEADER)}
  type PIMAGE_SECTION_HEADER = ^IMAGE_SECTION_HEADER;
  {$IFEND}

  {$IF NOT DECLARED(PIMAGE_EXPORT_DIRECTORY)}
  type PIMAGE_EXPORT_DIRECTORY = ^IMAGE_EXPORT_DIRECTORY;
  {$IFEND}

  {$IF NOT DECLARED(PIMAGE_DOS_HEADER)}
  type PIMAGE_DOS_HEADER = ^IMAGE_DOS_HEADER;
  {$IFEND}

  {$IF NOT DECLARED(PIMAGE_NT_HEADERS)}
  type PIMAGE_NT_HEADERS = ^IMAGE_NT_HEADERS;
  {$IFEND}

  {$IF NOT DECLARED(PUINT_PTR)}
  type PUINT_PTR = ^UINT_PTR;
  {$IFEND}

// Missing constants
const
  IMAGE_REL_BASED_ABSOLUTE = 0;
  IMAGE_REL_BASED_HIGHLOW = 3;
  IMAGE_REL_BASED_DIR64 = 10;

// Things that are incorrectly defined at least up to XE6 (miss x64 mapping)
{$IFDEF CPUX64}
type
  PIMAGE_TLS_DIRECTORY = PIMAGE_TLS_DIRECTORY64;
const
  IMAGE_ORDINAL_FLAG = IMAGE_ORDINAL_FLAG64;
{$ENDIF}

{ +++++++++++++++++++++++++++++++++++++++++++++++
  ***  Internal MemoryModule Const Definition  ***
  ----------------------------------------------- }
const
  IMAGE_SIZEOF_BASE_RELOCATION = SizeOf(IMAGE_BASE_RELOCATION);
  {$IFDEF CPUX64}
  HOST_MACHINE = IMAGE_FILE_MACHINE_AMD64;
  {$ELSE}
  HOST_MACHINE = IMAGE_FILE_MACHINE_I386;
  {$ENDIF}

type
{ +++++++++++++++++++++++++++++++++++++++++++++++
  ***  Internal MemoryModule Type Definition  ***
  ----------------------------------------------- }
  TMemoryModuleRec = record
    Headers: PIMAGE_NT_HEADERS;
    CodeBase: Pointer;
    Modules: array of HMODULE;
    NumModules: Integer;
    Initialized: Boolean;
    IsRelocated: Boolean;
    PageSize: DWORD;
  end;
  PMemoryModule = ^TMemoryModuleRec;

  TDllEntryProc = function(hinstDLL: HINST; fdwReason: DWORD; lpReserved: Pointer): BOOL; stdcall;

  TSectionFinalizeData = record
    Address: Pointer;
    AlignedAddress: Pointer;
    Size: SIZE_T;
    Characteristics: DWORD;
    Last: Boolean;
  end;

// Explicitly export these functions to allow hooking of their origins
function GetProcAddress_Internal(hModule: HMODULE; lpProcName: LPCSTR): FARPROC; stdcall; external kernel32 name 'GetProcAddress';
function LoadLibraryA_Internal(lpLibFileName: LPCSTR): HMODULE; stdcall; external kernel32 name 'LoadLibraryA';
function FreeLibrary_Internal(hLibModule: HMODULE): BOOL; stdcall; external kernel32 name 'FreeLibrary';

// Just an imitation to allow using try-except block. DO NOT try to handle this
// like "on E do ..." !
procedure Abort;
begin
  raise TObject.Create;
end;

// Copy from SysUtils to get rid of this unit
function StrComp(const Str1, Str2: PAnsiChar): Integer;
var
  P1, P2: PAnsiChar;
begin
  P1 := Str1;
  P2 := Str2;
  while True do
  begin
    if (P1^ <> P2^) or (P1^ = #0) then
      Exit(Ord(P1^) - Ord(P2^));
    Inc(P1);
    Inc(P2);
  end;
end;

  { +++++++++++++++++++++++++++++++++++++++++++++++++++++
    ***                Missing WinAPI macros          ***
    ----------------------------------------------------- }

{$IF NOT DECLARED(IMAGE_ORDINAL)}
//  #define IMAGE_ORDINAL64(Ordinal) (Ordinal & 0xffff)
//  #define IMAGE_ORDINAL32(Ordinal) (Ordinal & 0xffff)
function IMAGE_ORDINAL(Ordinal: NativeUInt): Word; inline;
begin
  Result := Ordinal and $FFFF;
end;
{$IFEND}

{$IF NOT DECLARED(IMAGE_SNAP_BY_ORDINAL)}
//  IMAGE_SNAP_BY_ORDINAL64(Ordinal) ((Ordinal & IMAGE_ORDINAL_FLAG64) != 0)
//  IMAGE_SNAP_BY_ORDINAL32(Ordinal) ((Ordinal & IMAGE_ORDINAL_FLAG32) != 0)
function IMAGE_SNAP_BY_ORDINAL(Ordinal: NativeUInt): Boolean; inline;
begin
  Result := ((Ordinal and IMAGE_ORDINAL_FLAG) <> 0);
end;
{$IFEND}

  { +++++++++++++++++++++++++++++++++++++++++++++++++++++
    ***                 Helper functions              ***
    ----------------------------------------------------- }

function GET_HEADER_DICTIONARY(Module: PMemoryModule; Idx: Integer): PIMAGE_DATA_DIRECTORY;
begin
  Result := PIMAGE_DATA_DIRECTORY(@(Module.Headers.OptionalHeader.DataDirectory[Idx]));
end;

function ALIGN_DOWN(Address: Pointer; Alignment: DWORD): Pointer;
begin
  Result := Pointer(UIntPtr(Address) and not (Alignment - 1));
end;

function CopySections(data: Pointer; old_headers: PIMAGE_NT_HEADERS; module: PMemoryModule): Boolean;
var
  i, Size: Integer;
  CodeBase: Pointer;
  dest: Pointer;
  Section: PIMAGE_SECTION_HEADER;
begin
  CodeBase := Module.CodeBase;
  Section := PIMAGE_SECTION_HEADER(IMAGE_FIRST_SECTION(Module.Headers{$IFNDEF FPC}^{$ENDIF}));
  for i := 0 to Module.Headers.FileHeader.NumberOfSections - 1 do
  begin
    // Section doesn't contain data in the dll itself, but may define
    // uninitialized Data
    if Section.SizeOfRawData = 0 then
    begin
      Size := Old_headers.OptionalHeader.SectionAlignment;
      if Size > 0 then
      begin
        dest := VirtualAlloc(PByte(CodeBase) + Section.VirtualAddress,
                             Size,
                             MEM_COMMIT,
                             PAGE_READWRITE);
        if dest = nil then
          Exit(False);
        // Always use position from file to support alignments smaller
        // than page Size.
        dest := PByte(CodeBase) + Section.VirtualAddress;
        Section.Misc.PhysicalAddress := DWORD(dest);
        ZeroMemory(dest, Size);
      end;
      // Section is empty
      Inc(Section);
      Continue;
    end; // if

    // commit memory block and copy Data from dll
    dest := VirtualAlloc(PByte(CodeBase) + Section.VirtualAddress,
                         Section.SizeOfRawData,
                         MEM_COMMIT,
                         PAGE_READWRITE);
    if dest = nil then
      Exit(False);

    // Always use position from file to support alignments smaller
    // than page Size.
    dest := PByte(CodeBase) + Section.VirtualAddress;
    CopyMemory(dest, PByte(Data) + Section.PointerToRawData, Section.SizeOfRawData);
    Section.Misc.PhysicalAddress := DWORD(dest);
    Inc(Section);
  end; // for

  Result := True;
end;

// Protection flags for memory pages (Executable, Readable, Writeable)
const
  ProtectionFlags: array[Boolean, Boolean, Boolean] of DWORD =
  (
    (
        // not executable
        (PAGE_NOACCESS, PAGE_WRITECOPY),
        (PAGE_READONLY, PAGE_READWRITE)
    ),
    (
        // executable
        (PAGE_EXECUTE, PAGE_EXECUTE_WRITECOPY),
        (PAGE_EXECUTE_READ, PAGE_EXECUTE_READWRITE)
    )
);

function GetRealSectionSize(Module: PMemoryModule; Section: PIMAGE_SECTION_HEADER): DWORD;
begin
  Result := Section.SizeOfRawData;
  if Result = 0 then
    if (Section.Characteristics and IMAGE_SCN_CNT_INITIALIZED_DATA) <> 0 then
      Result := Module.Headers.OptionalHeader.SizeOfInitializedData
    else if (Section.Characteristics and IMAGE_SCN_CNT_UNINITIALIZED_DATA) <> 0 then
      Result := Module.Headers.OptionalHeader.SizeOfUninitializedData;
end;

function FinalizeSection(Module: PMemoryModule; const SectionData: TSectionFinalizeData): Boolean;
var
  protect, oldProtect: DWORD;
  executable, readable, writeable: Boolean;
begin
  if SectionData.Size = 0 then
    Exit(True);

  if (SectionData.Characteristics and IMAGE_SCN_MEM_DISCARDABLE) <> 0 then
  begin
    // Section is not needed any more and can safely be freed
    if (SectionData.Address = SectionData.AlignedAddress) and
       ( SectionData.Last or
         (Module.Headers.OptionalHeader.SectionAlignment = Module.PageSize) or
         (SectionData.Size mod Module.PageSize = 0)
       ) then
         // Only allowed to decommit whole pages
         VirtualFree(SectionData.Address, SectionData.Size, MEM_DECOMMIT);
    Exit(True);
  end;

  // determine protection flags based on Characteristics
  executable := (SectionData.Characteristics and IMAGE_SCN_MEM_EXECUTE) <> 0;
  readable   := (SectionData.Characteristics and IMAGE_SCN_MEM_READ) <> 0;
  writeable  := (SectionData.Characteristics and IMAGE_SCN_MEM_WRITE) <> 0;
  protect := ProtectionFlags[executable][readable][writeable];
  if (SectionData.Characteristics and IMAGE_SCN_MEM_NOT_CACHED) <> 0 then
    protect := protect or PAGE_NOCACHE;

  // change memory access flags
  Result := VirtualProtect(SectionData.Address, SectionData.Size, protect, oldProtect);
end;

function FinalizeSections(Module: PMemoryModule): Boolean;
var
  i: Integer;
  Section: PIMAGE_SECTION_HEADER;
  imageOffset: UIntPtr;
  SectionData: TSectionFinalizeData;
  sectionAddress, AlignedAddress: Pointer;
  sectionSize: DWORD;
begin
  Section := PIMAGE_SECTION_HEADER(IMAGE_FIRST_SECTION(Module.Headers{$IFNDEF FPC}^{$ENDIF}));
  {$IFDEF CPUX64}
  imageOffset := (NativeUInt(Module.CodeBase) and $ffffffff00000000);
  {$ELSE}
  imageOffset := 0;
  {$ENDIF}

  SectionData.Address := Pointer(UIntPtr(Section.Misc.PhysicalAddress) or imageOffset);
  SectionData.AlignedAddress := ALIGN_DOWN(SectionData.Address, Module.PageSize);
  SectionData.Size := GetRealSectionSize(Module, Section);
  SectionData.Characteristics := Section.Characteristics;
  SectionData.Last := False;
  Inc(Section);

  // loop through all sections and change access flags

  for i := 1 to Module.Headers.FileHeader.NumberOfSections - 1 do
  begin
    sectionAddress := Pointer(UIntPtr(Section.Misc.PhysicalAddress) or imageOffset);
    AlignedAddress := ALIGN_DOWN(SectionData.Address, Module.PageSize);
    sectionSize := GetRealSectionSize(Module, Section);
    // Combine access flags of all sections that share a page
    // TODO(fancycode): We currently share flags of a trailing large Section
    //   with the page of a first small Section. This should be optimized.
    if (SectionData.AlignedAddress = AlignedAddress) or
       (PByte(SectionData.Address) + SectionData.Size > PByte(AlignedAddress)) then
    begin
      // Section shares page with previous
      if (Section.Characteristics and IMAGE_SCN_MEM_DISCARDABLE = 0) or
         (SectionData.Characteristics and IMAGE_SCN_MEM_DISCARDABLE = 0) then
        SectionData.Characteristics := (SectionData.Characteristics or Section.Characteristics) and not IMAGE_SCN_MEM_DISCARDABLE
      else
        SectionData.Characteristics := SectionData.Characteristics or Section.Characteristics;
      SectionData.Size := PByte(sectionAddress) + sectionSize - PByte(SectionData.Address);
      Inc(Section);
      Continue;
    end;

    if not FinalizeSection(Module, SectionData) then
      Exit(False);

    SectionData.Address := sectionAddress;
    SectionData.AlignedAddress := AlignedAddress;
    SectionData.Size := sectionSize;
    SectionData.Characteristics := Section.Characteristics;

    Inc(Section);
  end; // for

  SectionData.Last := True;
  if not FinalizeSection(Module, SectionData) then
    Exit(False);

  Result := True;
end;

function ExecuteTLS(Module: PMemoryModule): Boolean;
var
  CodeBase: Pointer;
  directory: PIMAGE_DATA_DIRECTORY;
  tls: PIMAGE_TLS_DIRECTORY;
  callback: PPointer; // =^PIMAGE_TLS_CALLBACK;

  // TLS callback pointers are VA's (ImageBase included) so if the module resides at
  // the other ImageBage they become invalid. This routine relocates them to the
  // actual ImageBase.
  // The case seem to happen with DLLs only and they rarely use TLS callbacks.
  // Moreover, they probably don't work at all when using DLL dynamically which is
  // the case in our code.
  function FixPtr(OldPtr: Pointer): Pointer;
  begin
    Result := Pointer(NativeInt(OldPtr) - Module.Headers.OptionalHeader.ImageBase + NativeInt(CodeBase));
  end;

begin
  Result := True;
  CodeBase := Module.CodeBase;

  directory := GET_HEADER_DICTIONARY(Module, IMAGE_DIRECTORY_ENTRY_TLS);
  if directory.VirtualAddress = 0 then
    Exit;

  tls := PIMAGE_TLS_DIRECTORY(PByte(CodeBase) + directory.VirtualAddress);
  // Delphi syntax is quite awkward when dealing with proc pointers so we have to
  // use casts to untyped pointers
  callback := Pointer(tls.AddressOfCallBacks);
  if callback <> nil then
  begin
    callback := FixPtr(callback);
    while callback^ <> nil do
    begin
      PIMAGE_TLS_CALLBACK(FixPtr(callback^))(CodeBase, DLL_PROCESS_ATTACH, nil);
      Inc(callback);
    end;
  end;
end;

function PerformBaseRelocation(Module: PMemoryModule; Delta: NativeInt): Boolean;
var
  i: Cardinal;
  CodeBase: Pointer;
  directory: PIMAGE_DATA_DIRECTORY;
  relocation: PIMAGE_BASE_RELOCATION;
  dest: Pointer;
  relInfo: ^UInt16;
  patchAddrHL: PDWORD;
  {$IFDEF CPUX64}
  patchAddr64: PULONGLONG;
  {$ENDIF}
  relType, offset: Integer;
begin
  CodeBase := Module.CodeBase;
  directory := GET_HEADER_DICTIONARY(Module, IMAGE_DIRECTORY_ENTRY_BASERELOC);
  if directory.Size = 0 then
    Exit(Delta = 0);

  relocation := PIMAGE_BASE_RELOCATION(PByte(CodeBase) + directory.VirtualAddress);
  while relocation.VirtualAddress > 0 do
  begin
    dest := Pointer(PByte(CodeBase) + relocation.VirtualAddress);
    relInfo := Pointer(PByte(relocation) + IMAGE_SIZEOF_BASE_RELOCATION);
    for i := 0 to Trunc(((relocation.SizeOfBlock - IMAGE_SIZEOF_BASE_RELOCATION) / 2)) - 1 do
    begin
      // the upper 4 bits define the type of relocation
      relType := relInfo^ shr 12;
      // the lower 12 bits define the offset
      offset := relInfo^ and $FFF;

      case relType of
        IMAGE_REL_BASED_ABSOLUTE:
          // skip relocation
          ;
        IMAGE_REL_BASED_HIGHLOW:
          begin
            // change complete 32 bit address
            patchAddrHL := Pointer(PByte(dest) + offset);
            Inc(patchAddrHL^, Delta);
          end;

        {$IFDEF CPUX64}
        IMAGE_REL_BASED_DIR64:
          begin
            patchAddr64 := Pointer(PByte(dest) + offset);
            Inc(patchAddr64^, Delta);
          end;
        {$ENDIF}
      end;

      Inc(relInfo);
    end; // for

    // advance to next relocation block
    relocation := PIMAGE_BASE_RELOCATION(PByte(relocation) + relocation.SizeOfBlock);
  end; // while

  Result := True;
end;

function BuildImportTable(Module: PMemoryModule): Boolean; stdcall;
var
  CodeBase: Pointer;
  directory: PIMAGE_DATA_DIRECTORY;
  importDesc: PIMAGE_IMPORT_DESCRIPTOR;
  thunkRef: PUINT_PTR;
  funcRef: ^FARPROC;
  handle: HMODULE;
  thunkData: PIMAGE_IMPORT_BY_NAME;
begin
  CodeBase := Module.CodeBase;
  Result := True;

  directory := GET_HEADER_DICTIONARY(Module, IMAGE_DIRECTORY_ENTRY_IMPORT);
  if directory.Size = 0 then
    Exit(True);

  importDesc := PIMAGE_IMPORT_DESCRIPTOR(PByte(CodeBase) + directory.VirtualAddress);
  while (not IsBadReadPtr(importDesc, SizeOf(IMAGE_IMPORT_DESCRIPTOR))) and (importDesc.Name <> 0) do
  begin
    handle := LoadLibraryA_Internal(PAnsiChar(PByte(CodeBase) + importDesc.Name));
    if handle = 0 then
    begin
      SetLastError(ERROR_MOD_NOT_FOUND);
      Result := False;
      Break;
    end;

    try
      SetLength(Module.Modules, Module.NumModules + 1);
    except
      FreeLibrary_Internal(handle);
      SetLastError(ERROR_OUTOFMEMORY);
      Result := False;
      Break;
    end;
    Module.Modules[Module.NumModules] := handle;
    Inc(Module.NumModules);

    if importDesc.OriginalFirstThunk <> 0 then
    begin
      thunkRef := Pointer(PByte(CodeBase) + importDesc.OriginalFirstThunk);
      funcRef := Pointer(PByte(CodeBase) + importDesc.FirstThunk);
    end
    else
    begin
      // no hint table
      thunkRef := Pointer(PByte(CodeBase) + importDesc.FirstThunk);
      funcRef := Pointer(PByte(CodeBase) + importDesc.FirstThunk);
    end;

    while thunkRef^ <> 0 do
    begin
      if IMAGE_SNAP_BY_ORDINAL(thunkRef^) then
        funcRef^ := GetProcAddress_Internal(handle, PAnsiChar(IMAGE_ORDINAL(thunkRef^)))
      else
      begin
        thunkData := PIMAGE_IMPORT_BY_NAME(PByte(CodeBase) + thunkRef^);
        funcRef^ := GetProcAddress_Internal(handle, PAnsiChar(@(thunkData.Name)));
      end;
      if funcRef^ = nil then
      begin
        Result := False;
        Break;
      end;
      Inc(funcRef);
      Inc(thunkRef);
    end; // while

    if not Result then
    begin
      FreeLibrary_Internal(handle);
      SetLastError(ERROR_PROC_NOT_FOUND);
      Break;
    end;

    Inc(importDesc);
  end; // while
end;

  { +++++++++++++++++++++++++++++++++++++++++++++++++++++
    ***  Memory DLL loading functions Implementation  ***
    ----------------------------------------------------- }

function MemoryLoadLibary(Data: Pointer): TMemoryModule; stdcall;
var
  dos_header: PIMAGE_DOS_HEADER;
  old_header: PIMAGE_NT_HEADERS;
  code, Headers: Pointer;
  locationdelta: NativeInt;
  sysInfo: SYSTEM_INFO;
  DllEntry: TDllEntryProc;
  successfull: Boolean;
  Module: PMemoryModule;
begin
  Result := nil; Module := nil;

  try
    dos_header := PIMAGE_DOS_HEADER(Data);
    if (dos_header.e_magic <> IMAGE_DOS_SIGNATURE) then
    begin
      SetLastError(ERROR_BAD_EXE_FORMAT);
      Exit;
    end;

    // old_header = (PIMAGE_NT_HEADERS)&((const unsigned char * )(Data))[dos_header->e_lfanew];
    old_header := PIMAGE_NT_HEADERS(PByte(Data) + dos_header._lfanew);
    if old_header.Signature <> IMAGE_NT_SIGNATURE then
    begin
      SetLastError(ERROR_BAD_EXE_FORMAT);
      Exit;
    end;

    {$IFDEF CPUX64}
    if old_header.FileHeader.Machine <> IMAGE_FILE_MACHINE_AMD64 then
    {$ELSE}
    if old_header.FileHeader.Machine <> IMAGE_FILE_MACHINE_I386 then
    {$ENDIF}
    begin
      SetLastError(ERROR_BAD_EXE_FORMAT);
      Exit;
    end;

    if (old_header.OptionalHeader.SectionAlignment and 1) <> 0 then
    begin
      // Only support section alignments that are a multiple of 2
      SetLastError(ERROR_BAD_EXE_FORMAT);
      Exit;
    end;

    // reserve memory for image of library
    // XXX: is it correct to commit the complete memory region at once?
    //      calling DllEntry raises an exception if we don't...
    code := VirtualAlloc(Pointer(old_header.OptionalHeader.ImageBase),
                         old_header.OptionalHeader.SizeOfImage,
                         MEM_RESERVE or MEM_COMMIT,
                         PAGE_READWRITE);
    if code = nil then
    begin
      // try to allocate memory at arbitrary position
      code := VirtualAlloc(nil,
                           old_header.OptionalHeader.SizeOfImage,
                           MEM_RESERVE or MEM_COMMIT,
                           PAGE_READWRITE);
      if code = nil then
      begin
        SetLastError(ERROR_OUTOFMEMORY);
        Exit;
      end;
    end;

    Module := PMemoryModule(HeapAlloc(GetProcessHeap(), HEAP_ZERO_MEMORY, SizeOf(TMemoryModuleRec)));
    if Module = nil then
    begin
      VirtualFree(code, 0, MEM_RELEASE);
      SetLastError(ERROR_OUTOFMEMORY);
      Exit;
    end;

    // memory is zeroed by HeapAlloc
    Module.CodeBase := code;
    GetNativeSystemInfo({$IFDEF FPC}@{$ENDIF}sysInfo);
    Module.PageSize := sysInfo.dwPageSize;

    // commit memory for Headers
    Headers := VirtualAlloc(code,
                            old_header.OptionalHeader.SizeOfHeaders,
                            MEM_COMMIT,
                            PAGE_READWRITE);

    // copy PE header to code
    CopyMemory(Headers, dos_header, old_header.OptionalHeader.SizeOfHeaders);
    // result->Headers = (PIMAGE_NT_HEADERS)&((const unsigned char *)(Headers))[dos_header->e_lfanew];
    Module.Headers := PIMAGE_NT_HEADERS(PByte(Headers) + dos_header._lfanew);

    // copy sections from DLL file block to new memory location
    if not CopySections(Data, old_header, Module) then
      Abort;

    // adjust base address of imported data
    locationdelta := NativeInt(code) - old_header.OptionalHeader.ImageBase;
    if locationdelta <> 0 then
      Module.IsRelocated := PerformBaseRelocation(Module, locationdelta)
    else
      Module.IsRelocated := True;

    // load required dlls and adjust function table of imports
    if not BuildImportTable(Module) then
      Abort;

    // mark memory pages depending on Section Headers and release
    // sections that are marked as "discardable"
    if not FinalizeSections(Module) then
      Abort;

    // TLS callbacks are executed BEFORE the main loading
    if not ExecuteTLS(Module) then
      Abort;

    // get entry point of loaded library
    if Module.Headers.OptionalHeader.AddressOfEntryPoint <> 0 then
    begin
      @DllEntry := Pointer(PByte(code) + Module.Headers.OptionalHeader.AddressOfEntryPoint);
      // notify library about attaching to process
      successfull := DllEntry(HINST(code), DLL_PROCESS_ATTACH, nil);
      if not successfull then
      begin
        SetLastError(ERROR_DLL_INIT_FAILED);
        Abort;
      end;
      Module.Initialized := True;
    end;

    Result := Module;
  except
    // cleanup
    MemoryFreeLibrary(Module);
    Exit;
  end;
end;

function MemoryGetProcAddress(Module: TMemoryModule; const Name: PAnsiChar): Pointer; stdcall;
var
  CodeBase: Pointer;
  Idx: Integer;
  i: DWORD;
  nameRef: PDWORD;
  ordinal: PWord;
  exportDir: PIMAGE_EXPORT_DIRECTORY;
  directory: PIMAGE_DATA_DIRECTORY;
  temp: PDWORD;
  mmodule: PMemoryModule;
begin
  Result := nil;
  mmodule := PMemoryModule(Module);

  CodeBase := mmodule.CodeBase;
  directory := GET_HEADER_DICTIONARY(mmodule, IMAGE_DIRECTORY_ENTRY_EXPORT);
  // no export table found
  if directory.Size = 0 then
  begin
    SetLastError(ERROR_PROC_NOT_FOUND);
    Exit;
  end;

  exportDir := PIMAGE_EXPORT_DIRECTORY(PByte(CodeBase) + directory.VirtualAddress);
  // DLL doesn't export anything
  if (exportDir.NumberOfNames = 0) or (exportDir.NumberOfFunctions = 0) then
  begin
    SetLastError(ERROR_PROC_NOT_FOUND);
    Exit;
  end;

  // search function name in list of exported names
  nameRef := Pointer(PByte(CodeBase) + exportDir.AddressOfNames);
  ordinal := Pointer(PByte(CodeBase) + exportDir.AddressOfNameOrdinals);
  Idx := -1;
  for i := 0 to exportDir.NumberOfNames - 1 do
  begin
    if StrComp(Name, PAnsiChar(PByte(CodeBase) + nameRef^)) = 0 then
    begin
      Idx := ordinal^;
      Break;
    end;
    Inc(nameRef);
    Inc(ordinal);
  end;

  // exported symbol not found
  if (Idx = -1) then
  begin
    SetLastError(ERROR_PROC_NOT_FOUND);
    Exit;
  end;

  // name <-> ordinal number don't match
  if (DWORD(Idx) > exportDir.NumberOfFunctions) then
  begin
    SetLastError(ERROR_PROC_NOT_FOUND);
    Exit;
  end;

  // AddressOfFunctions contains the RVAs to the "real" functions     {}
  temp := Pointer(PByte(CodeBase) + exportDir.AddressOfFunctions + Idx*4);
  Result := Pointer(PByte(CodeBase) + temp^);
end;

procedure MemoryFreeLibrary(Module: TMemoryModule); stdcall;
var
  i: Integer;
  DllEntry: TDllEntryProc;
  mmodule: PMemoryModule;
begin
  if Module = nil then Exit;

  mmodule := PMemoryModule(Module);

  if mmodule.Initialized then
  begin
    // notify library about detaching from process
    @DllEntry := Pointer(PByte(mmodule.CodeBase) + mmodule.Headers.OptionalHeader.AddressOfEntryPoint);
    DllEntry(HINST(mmodule.CodeBase), DLL_PROCESS_DETACH, nil);
  end;

  if Length(mmodule.Modules) <> 0 then
  begin
    // free previously opened libraries
    for i := 0 to mmodule.NumModules - 1 do
      if mmodule.Modules[i] <> 0 then
        FreeLibrary_Internal(mmodule.Modules[i]);

    SetLength(mmodule.Modules, 0);
  end;

  if mmodule.CodeBase <> nil then
    // release memory of library
    VirtualFree(mmodule.CodeBase, 0, MEM_RELEASE);

  HeapFree(GetProcessHeap(), 0, mmodule);
end;

end.
