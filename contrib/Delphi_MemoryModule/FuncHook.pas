(*==============================================================================
                      ====== Function hooking ======
 Functions exported from DLL contain single instruction "JMP [fn_addr]" that
 points to actual code. So there could be 2 ways of hooking:
   1) Patch the JMP not touching the code
    + Original proc could be called via HookInfo.OrigProc
    - (or feature) Only the one imported function is hooked. If there's another
      export definition or dynamic load, the hook won't change them.
   2) Patch the very code as usual
    + All function imports are impacted
    - No way to use original proc
 2nd way is called "strict address mode" here and is controlled by HookProcedure()'s
 StrictMode parameter.

 Compatibility: RAD Studio 2009+ (uses advanced records and Exit(param)), x32/x64.
==============================================================================*)

unit FuncHook;

interface

uses
  Windows;

type
  TInjectInstr = packed record
    Opcode: UInt8;
    Offset: Int32;
  end;
  PInjectInstr = ^TInjectInstr;

  THookInfo = record
  private
    OrigCode: TInjectInstr;   // Original instruction
    HookCode: TInjectInstr;   // Hook instruction (saved only for check on uninstall)
    ProcAddr: Pointer;        // Address of a routine to hook
    TrampAddr: Pointer;       // Address of a trampoline (original routine; DLL exports only)
    HookAddr: Pointer;        // Address of a hook routine
    StrictMode: Boolean;      // Hook was installed in strict address mode
  public
    procedure Init(ProcAddr, HookAddr: Pointer);
    property OrigProc: Pointer read TrampAddr;
  end;

function HookProcedure(StrictMode: Boolean; var HookInfo: THookInfo): Boolean;
function UnhookProcedure(var HookInfo: THookInfo): Boolean;

implementation

const
  INDIRECT_JMP = $25FF;
  RELATIVE_JMP = $E9;
  EmptyInstr: TInjectInstr = (Opcode: 0; Offset: Int32($DEADBEEF));

function IsEqual(const Instr1, Instr2: TInjectInstr): Boolean;
begin
  Result := (Instr1.Opcode = Instr2.Opcode) and (Instr1.Offset = Instr2.Offset);
end;

{ THookInfo }

procedure THookInfo.Init(ProcAddr, HookAddr: Pointer);
begin
  ZeroMemory(@Self, SizeOf(Self));
  OrigCode := EmptyInstr;
  Self.ProcAddr := ProcAddr;
  Self.HookAddr := HookAddr;
end;

// Utility function to (un)install hook by patching function code.
//   Install: (un)install the hook
//   HookInfo: all required data
// Returns: True = success, False = fail
function PatchCode(Install: Boolean; var HookInfo: THookInfo): Boolean;

// Get the real address of a function (for functions exported from DLL)
function GetStrictAddr: Pointer;
type
  TAbsIndirectJmp = packed record
    OpCode: UInt16;   // $FF25 (x32: Jmp, FF /4; x64: Jmp, Rel /4)
    Addr  : Int32;
  end;
  PAbsIndirectJmp = ^TAbsIndirectJmp;
var
  jmp: PAbsIndirectJmp;
begin
  Result := nil;
  jmp := PAbsIndirectJmp(HookInfo.ProcAddr);
  if jmp.OpCode = INDIRECT_JMP then
  {$IFDEF CPUX86}
    Result := PPointer(jmp.Addr)^;
  {$ENDIF}
  {$IFDEF CPUX64}
    Result := PPointer(PByte(HookInfo.ProcAddr) + jmp.Addr + SizeOf(TAbsIndirectJmp))^;
  {$ENDIF}
end;

var
  OldProtect: DWORD;
  DestAddr: PInjectInstr;
begin
  // Check strict address mode
  if HookInfo.StrictMode
    then DestAddr := GetStrictAddr
    else DestAddr := HookInfo.ProcAddr;

  Result := VirtualProtect(DestAddr, SizeOf(TInjectInstr), PAGE_EXECUTE_READWRITE, OldProtect);
  if not Result then Exit;

  if Install then
  begin
    // For functions exported from DLL, the only instruction they contain is
    // "JMP [fn_addr]" so we can save the address as a trampoline
    if not HookInfo.StrictMode
      then HookInfo.TrampAddr := GetStrictAddr
      else HookInfo.TrampAddr := nil;
    HookInfo.OrigCode := DestAddr^;
    DestAddr^ := HookInfo.HookCode;
  end
  else
  begin
    // Check that patch wasn't overwritten
    if IsEqual(HookInfo.HookCode, DestAddr^) then
    begin
      DestAddr^ := HookInfo.OrigCode;
      // Clear OrigCode field thus indicating that hook is not installed
      HookInfo.OrigCode := EmptyInstr;
      HookInfo.StrictMode := False;
    end
    else
      Result := False;
  end;

  FlushInstructionCache(GetCurrentProcess, DestAddr, SizeOf(TInjectInstr));
  VirtualProtect(DestAddr, SizeOf(TInjectInstr), OldProtect, OldProtect);
end;

// Install the hook
//   StrictMode: "strict address mode" flag
//   HookInfo: all required data
// Returns: True = success, False = fail
function HookProcedure(StrictMode: Boolean; var HookInfo: THookInfo): Boolean;
begin
  // Required data is missing?
  if (HookInfo.HookAddr = nil) or (HookInfo.ProcAddr = nil) or
  // Hook is installed already?
     not IsEqual(HookInfo.OrigCode, EmptyInstr) then
    Exit(False);
  HookInfo.HookCode.Opcode := RELATIVE_JMP;
  HookInfo.HookCode.Offset := PByte(HookInfo.HookAddr) - PByte(HookInfo.ProcAddr) - SizeOf(TInjectInstr);
  HookInfo.StrictMode := StrictMode;
  Result := PatchCode(True, HookInfo);
end;

// Uninstall the hook
//   HookInfo: all required data
// Returns: True = success, False = fail
function UnhookProcedure(var HookInfo: THookInfo): Boolean;
begin
  // Required data is missing?
  if (HookInfo.HookAddr = nil) or (HookInfo.ProcAddr = nil) or
  // Hook is not installed yet?
     IsEqual(HookInfo.OrigCode, EmptyInstr) then
    Exit(False);
  Result := PatchCode(False, HookInfo);
end;

end.
