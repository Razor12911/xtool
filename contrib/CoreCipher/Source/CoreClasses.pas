{ ****************************************************************************** }
{ * Core class library  written by QQ 600585@qq.com                            * }
{ ****************************************************************************** }
{ * https://zpascal.net                                                        * }
{ * https://github.com/PassByYou888/zAI                                        * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/PascalString                               * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zChinese                                   * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/FFMPEG-Header                              * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/InfiniteIoT                                * }
{ * https://github.com/PassByYou888/FastMD5                                    * }
{ ****************************************************************************** }

unit CoreClasses;

{$INCLUDE zDefine.inc}

interface

uses SysUtils, Classes, Types,
  {$IFDEF Parallel}
  {$IFNDEF FPC}
  {$IFDEF SystemParallel}
  Threading,
  {$ENDIF SystemParallel}
  {$ENDIF FPC}
  {$ENDIF Parallel}
  SyncObjs,
  {$IFDEF FPC}
  FPCGenericStructlist, fgl,
  {$ELSE FPC}
  System.Generics.Collections,
  {$ENDIF FPC}
  Math;

{$Region 'core defines + class'}
type
  TBytes = SysUtils.TBytes;
  TPoint = Types.TPoint;
  TTimeTick = UInt64;
  PTimeTick = ^TTimeTick;
  TSeekOrigin = Classes.TSeekOrigin;
  TNotify = Classes.TNotifyEvent;

  TCoreClassObject = TObject;
  TCoreClassPersistent = TPersistent;
  TCoreClassStream = TStream;
  TCoreClassFileStream = TFileStream;
  TCoreClassStringStream = TStringStream;
  TCoreClassResourceStream = TResourceStream;
  TCoreClassThread = TThread;
  CoreClassException = Exception;
  TCoreClassMemoryStream = TMemoryStream;
  TCoreClassStrings = TStrings;
  TCoreClassStringList = TStringList;
  TCoreClassReader = TReader;
  TCoreClassWriter = TWriter;
  TCoreClassComponent = TComponent;

  TExecutePlatform = (epWin32, epWin64, epOSX32, epOSX64, epIOS, epIOSSIM, epANDROID32, epANDROID64, epLinux64, epLinux32, epUnknow);

  {$IFDEF FPC}
  // freepascal
  PUInt64 = ^UInt64;

  TCoreClassInterfacedObject = class(TInterfacedObject)
  protected
    function _AddRef: longint; {$IFNDEF WINDOWS} cdecl {$ELSE} stdcall {$ENDIF};
    function _Release: longint; {$IFNDEF WINDOWS} cdecl {$ELSE} stdcall {$ENDIF};
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

  PCoreClassPointerList = Classes.PPointerList;
  TCoreClassPointerList = Classes.TPointerList;
  TCoreClassListSortCompare = Classes.TListSortCompare;
  TCoreClassListNotification = Classes.TListNotification;

  TCoreClassList = class(TList)
    property ListData: PPointerList read GetList;
  end;

  TCoreClassListForObj = specialize TGenericsList<TCoreClassObject>;
  TCoreClassForObjectList = array of TCoreClassObject;
  PCoreClassForObjectList = ^TCoreClassForObjectList;
  {$ELSE FPC}
  // delphi
  TCoreClassInterfacedObject = class(TInterfacedObject)
  protected
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

  TGenericsList<t>=class(System.Generics.Collections.TList<t>)
  private type
    TGArry = array of t;
  public var Arry:TGArry;
    function ListData: Pointer;
  end;

  TGenericsObjectList<t:class>=class(System.Generics.Collections.TList<t>)
  private type
    TGArry = array of t;
  public var Arry:TGArry;
    function ListData: Pointer;
  end;

  TCoreClassPointerList = array of Pointer;
  PCoreClassPointerList = ^TCoreClassPointerList;

  TCoreClassList = class(TGenericsList<Pointer>)
    function ListData: PCoreClassPointerList;
  end;

  TCoreClassForObjectList = array of TCoreClassObject;
  PCoreClassForObjectList = ^TCoreClassForObjectList;

  TCoreClassListForObj = class(TGenericsList<TCoreClassObject>)
    function ListData: PCoreClassForObjectList;
  end;
  {$ENDIF FPC}

  TCoreClassObjectList = class(TCoreClassListForObj)
  public
    AutoFreeObj: Boolean;
    constructor Create; overload;
    constructor Create(AutoFreeObj_: Boolean); overload;
    destructor Destroy; override;

    procedure Remove(obj: TCoreClassObject);
    procedure Delete(index: Integer);
    procedure Clear;
  end;

  TSoftCritical = class(TCoreClassObject)
  private
    L: Boolean;
  public
    constructor Create;
    procedure Acquire; virtual;
    procedure Release; virtual;
    procedure Enter; virtual;
    procedure Leave; virtual;
  end;

{$IFDEF SoftCritical}
  TCritical_ = TSoftCritical;
{$ELSE SoftCritical}
  TCritical_ = TCriticalSection;
{$ENDIF SoftCritical}

{$IFDEF FPC}generic{$ENDIF FPC}TAtomVar<T_> = class
  public type
    PT_ = ^T_;
  private
    FValue__: T_;
    Critical: TCritical_;
    function GetValue: T_;
    procedure SetValue(const Value_: T_);
    function GetValueP: PT_;
  public
    constructor Create(Value_: T_);
    destructor Destroy; override;
    // operation
    function Lock: T_;
    function LockP: PT_;
    property P: PT_ read GetValueP;
    property Pointer_: PT_ read GetValueP;
    procedure UnLock(const Value_: T_); overload;
    procedure UnLock(const Value_: PT_); overload;
    procedure UnLock(); overload;
    // value work in atom read and write
    property V: T_ read GetValue write SetValue;
    property Value: T_ read GetValue write SetValue;
  end;
  TAtomBoolean = {$IFDEF FPC}specialize {$ENDIF FPC}TAtomVar<Boolean>;
  TAtomBool = TAtomBoolean;
  TAtomSmallInt = {$IFDEF FPC}specialize {$ENDIF FPC}TAtomVar<SmallInt>;
  TAtomShortInt = {$IFDEF FPC}specialize {$ENDIF FPC}TAtomVar<ShortInt>;
  TAtomInteger = {$IFDEF FPC}specialize {$ENDIF FPC}TAtomVar<Integer>;
  TAtomInt8 = TAtomSmallInt;
  TAtomInt16 = TAtomShortInt;
  TAtomInt32 = TAtomInteger;
  TAtomInt = TAtomInteger;
  TAtomInt64 = {$IFDEF FPC}specialize {$ENDIF FPC}TAtomVar<Int64>;
  TAtomByte = {$IFDEF FPC}specialize {$ENDIF FPC}TAtomVar<Byte>;
  TAtomWord = {$IFDEF FPC}specialize {$ENDIF FPC}TAtomVar<Word>;
  TAtomCardinal = {$IFDEF FPC}specialize {$ENDIF FPC}TAtomVar<Cardinal>;
  TAtomUInt8 = TAtomByte;
  TAtomUInt16 = TAtomWord;
  TAtomUInt32 = TAtomCardinal;
  TAtomDWord = TAtomCardinal;
  TAtomUInt64 = {$IFDEF FPC}specialize {$ENDIF FPC}TAtomVar<UInt64>;
  TAtomSingle = {$IFDEF FPC}specialize {$ENDIF FPC}TAtomVar<Single>;
  TAtomFloat = TAtomSingle;
  TAtomDouble = {$IFDEF FPC}specialize {$ENDIF FPC}TAtomVar<Double>;
  TAtomExtended = {$IFDEF FPC}specialize {$ENDIF FPC}TAtomVar<Extended>;
  TAtomString = {$IFDEF FPC}specialize {$ENDIF FPC}TAtomVar<string>;

  TCritical = class(TCritical_)
  private
    LNum: TAtomInt;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Acquire;
    procedure Release;
    procedure Enter;
    procedure Leave;
    function IsBusy: Boolean;
    property Busy: Boolean read IsBusy;
  end;

  TComputeThread = class;

  TRunWithThreadCall = procedure(ThSender: TComputeThread);
  TRunWithThreadMethod = procedure(ThSender: TComputeThread) of object;
  TRunWithThreadCall_NP = procedure();
  TRunWithThreadMethod_NP = procedure() of object;
  {$IFDEF FPC}
  TRunWithThreadProc = procedure(ThSender: TComputeThread) is nested;
  TRunWithThreadProc_NP = procedure() is nested;
  {$ELSE FPC}
  TRunWithThreadProc = reference to procedure(ThSender: TComputeThread);
  TRunWithThreadProc_NP = reference to procedure();
  {$ENDIF FPC}

  TComputeThread = class(TCoreClassThread)
  private var
    OnRunCall: TRunWithThreadCall;
    OnRunMethod: TRunWithThreadMethod;
    OnRunProc: TRunWithThreadProc;
    OnRunCall_NP: TRunWithThreadCall_NP;
    OnRunMethod_NP: TRunWithThreadMethod_NP;
    OnRunProc_NP: TRunWithThreadProc_NP;
    OnDoneCall: TRunWithThreadCall;
    OnDoneMethod: TRunWithThreadMethod;
    OnDoneProc: TRunWithThreadProc;
  protected
    procedure Execute; override;
    procedure Done_Sync;
  public
    UserData: Pointer;
    UserObject: TCoreClassObject;

    constructor Create;
    destructor Destroy; override;
    class function ActivtedTask(): Integer;
    class function WaitTask(): Integer;
    class function TotalTask(): Integer;
    class function State(): string;
    class function GetParallelGranularity(): Integer;
    class function GetMaxActivtedParallel(): Integer;

    // synchronization
    class procedure Sync(const OnRun_: TRunWithThreadProc_NP); overload;
    class procedure Sync(const Thread_: TThread; OnRun_: TRunWithThreadProc_NP); overload;

    // asynchronous
    class procedure RunC(const Data: Pointer; const Obj: TCoreClassObject; const OnRun, OnDone: TRunWithThreadCall); overload;
    class procedure RunC(const Data: Pointer; const Obj: TCoreClassObject; const OnRun: TRunWithThreadCall); overload;
    class procedure RunC(const OnRun: TRunWithThreadCall); overload;
    class procedure RunC_NP(const OnRun: TRunWithThreadCall_NP); overload;
    class procedure RunM(const Data: Pointer; const Obj: TCoreClassObject; const OnRun, OnDone: TRunWithThreadMethod); overload;
    class procedure RunM(const Data: Pointer; const Obj: TCoreClassObject; const OnRun: TRunWithThreadMethod); overload;
    class procedure RunM(const OnRun: TRunWithThreadMethod); overload;
    class procedure RunM_NP(const OnRun: TRunWithThreadMethod_NP); overload;
    class procedure RunP(const Data: Pointer; const Obj: TCoreClassObject; const OnRun, OnDone: TRunWithThreadProc); overload;
    class procedure RunP(const Data: Pointer; const Obj: TCoreClassObject; const OnRun: TRunWithThreadProc); overload;
    class procedure RunP(const OnRun: TRunWithThreadProc); overload;
    class procedure RunP_NP(const OnRun: TRunWithThreadProc_NP); overload;
  end;

  // TComputeThread alias
  TCompute = TComputeThread;

  TMT19937Random = class(TCoreClassObject)
  private
    FRndInstance: Pointer;
    function GetSeed: Integer;
    procedure SetSeed(const Value: Integer);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Rndmize();
    function Rand32(L: Integer): Integer; overload;
    procedure Rand32(L: Integer; dest: PInteger; num: NativeInt); overload;
    function Rand64(L: Int64): Int64; overload;
    procedure Rand64(L: Int64; dest: PInt64; num: NativeInt); overload;
    function RandE: Extended; overload;
    procedure RandE(dest: PExtended; num: NativeInt); overload;
    function RandF: Single; overload;
    procedure RandF(dest: PSingle; num: NativeInt); overload;
    function RandD: Double; overload;
    procedure RandD(dest: PDouble; num: NativeInt); overload;
    property seed: Integer read GetSeed write SetSeed;
  end;

  TRandom = TMT19937Random;

  {$IFDEF FPC}generic{$ENDIF FPC}TLineProcessor<T_> = class
  public type
    TTArry_ = array [0 .. 0] of T_;
    PTArry_ = ^TTArry_;
    PT_ = ^T_;
  private var
    FData: PTArry_;
    FWidth, FHeight: NativeInt;
    FValue: T_;
    FLineTail: Boolean;
  public
    procedure CreateDone; virtual;
    constructor Create(const data_: Pointer; const width_, height_: NativeInt; const Value_: T_; const LineTail_: Boolean);
    destructor Destroy; override;
    procedure VertLine(X, y1, y2: NativeInt);
    procedure HorzLine(x1, Y, x2: NativeInt);
    procedure Line(x1, y1, x2, y2: NativeInt);
    procedure FillBox(x1, y1, x2, y2: NativeInt);
    procedure Process(const vp: PT_; const v: T_); virtual;
    property Value: T_ read FValue;
  end;

{$EndRegion 'core defines + class'}
{$Region 'core const'}
const
  {$IF Defined(WIN32)}
  CurrentPlatform: TExecutePlatform = epWin32;
  {$ELSEIF Defined(WIN64)}
  CurrentPlatform: TExecutePlatform = epWin64;
  {$ELSEIF Defined(OSX)}
    {$IFDEF CPU64}
      CurrentPlatform: TExecutePlatform = epOSX64;
    {$ELSE CPU64}
      CurrentPlatform: TExecutePlatform = epOSX32;
    {$IFEND CPU64}
  {$ELSEIF Defined(IOS)}
    {$IFDEF CPUARM}
    CurrentPlatform: TExecutePlatform = epIOS;
    {$ELSE CPUARM}
    CurrentPlatform: TExecutePlatform = epIOSSIM;
    {$ENDIF CPUARM}
  {$ELSEIF Defined(ANDROID)}
    {$IFDEF CPU64}
    CurrentPlatform: TExecutePlatform = epANDROID64;
    {$ELSE CPU64}
    CurrentPlatform: TExecutePlatform = epANDROID32;
    {$IFEND CPU64}
  {$ELSEIF Defined(Linux)}
    {$IFDEF CPU64}
      CurrentPlatform: TExecutePlatform = epLinux64;
    {$ELSE CPU64}
      CurrentPlatform: TExecutePlatform = epLinux32;
    {$IFEND CPU64}
  {$ELSE}
  CurrentPlatform: TExecutePlatform = epUnknow;
  {$IFEND}

  CPU64 = {$IFDEF CPU64}True{$ELSE CPU64}False{$IFEND CPU64};
  X64 = CPU64;

  // timetick define
  C_Tick_Second = TTimeTick(1000);
  C_Tick_Minute = TTimeTick(C_Tick_Second) * 60;
  C_Tick_Hour   = TTimeTick(C_Tick_Minute) * 60;
  C_Tick_Day    = TTimeTick(C_Tick_Hour) * 24;
  C_Tick_Week   = TTimeTick(C_Tick_Day) * 7;
  C_Tick_Year   = TTimeTick(C_Tick_Day) * 365;

  // memory align
  C_MH_MemoryDelta = 0;

  // file mode
  fmCreate         = Classes.fmCreate;
  soFromBeginning  = Classes.soFromBeginning;
  soFromCurrent    = Classes.soFromCurrent;
  soFromEnd        = Classes.soFromEnd;
  fmOpenRead       = SysUtils.fmOpenRead;
  fmOpenWrite      = SysUtils.fmOpenWrite;
  fmOpenReadWrite  = SysUtils.fmOpenReadWrite;
  fmShareExclusive = SysUtils.fmShareExclusive;
  fmShareDenyWrite = SysUtils.fmShareDenyWrite;
  fmShareDenyNone  = SysUtils.fmShareDenyNone;
{$EndRegion 'core const'}
{$Region 'Parallel API'}
{$IFDEF FPC}
type
  TFPCParallelForProcedure32 = procedure(pass: Integer) is nested;
  TFPCParallelForProcedure64 = procedure(pass: Int64) is nested;
procedure FPCParallelFor(parallel: Boolean; OnFor:TFPCParallelForProcedure32; b, e: Integer); overload;
procedure FPCParallelFor(parallel: Boolean; OnFor:TFPCParallelForProcedure64; b, e: Int64); overload;
procedure FPCParallelFor(OnFor:TFPCParallelForProcedure32; b, e: Integer); overload;
procedure FPCParallelFor(OnFor:TFPCParallelForProcedure64; b, e: Int64); overload;
procedure FPCParallelFor(b, e: Integer; OnFor:TFPCParallelForProcedure32); overload;
procedure FPCParallelFor(b, e: Int64; OnFor:TFPCParallelForProcedure64); overload;
procedure FPCParallelFor(parallel: Boolean; b, e: Integer; OnFor:TFPCParallelForProcedure32); overload;
procedure FPCParallelFor(parallel: Boolean; b, e: Int64; OnFor:TFPCParallelForProcedure64); overload;
{$ELSE FPC}
type
{$IFDEF SystemParallel}
  TDelphiParallelForProcedure32 = TProc<Integer>;
  TDelphiParallelForProcedure64 = TProc<Int64>;
{$ELSE SystemParallel}
  TDelphiParallelForProcedure32 = reference to procedure(pass: Integer);
  TDelphiParallelForProcedure64 = reference to procedure(pass: Int64);
{$ENDIF SystemParallel}
procedure DelphiParallelFor(parallel: Boolean; b, e: Integer; OnFor: TDelphiParallelForProcedure32); overload;
procedure DelphiParallelFor(parallel: Boolean; b, e: Int64; OnFor: TDelphiParallelForProcedure64); overload;
procedure DelphiParallelFor(b, e: Integer; OnFor: TDelphiParallelForProcedure32); overload;
procedure DelphiParallelFor(b, e: Int64; OnFor: TDelphiParallelForProcedure64); overload;
procedure DelphiParallelFor(OnFor: TDelphiParallelForProcedure32; b, e: Integer); overload;
procedure DelphiParallelFor(OnFor: TDelphiParallelForProcedure64; b, e: Int64); overload;
procedure DelphiParallelFor(parallel: Boolean; OnFor: TDelphiParallelForProcedure32; b, e: Integer); overload;
procedure DelphiParallelFor(parallel: Boolean; OnFor: TDelphiParallelForProcedure64; b, e: Int64); overload;
{$ENDIF FPC}
{$EndRegion 'Parallel API'}
{$Region 'core api'}

// NoP = No Operation. It's the empty function, whose purpose is only for the
// debugging, or for the piece of code where intentionaly nothing is planned to be.
procedure Nop;

// process Synchronize
procedure CheckThreadSynchronize; overload;
function CheckThreadSynchronize(Timeout: Integer): Boolean; overload;

// core thread pool
procedure FreeCoreThreadPool;

procedure DisposeObject(const Obj: TObject); overload;
procedure DisposeObject(const objs: array of TObject); overload;
procedure FreeObject(const Obj: TObject); overload;
procedure FreeObject(const objs: array of TObject); overload;
procedure DisposeObjectAndNil(var Obj);

procedure LockObject(Obj: TObject);
procedure UnLockObject(Obj: TObject);

function DeltaStep(const value_, Delta_: NativeInt): NativeInt; inline;
procedure AtomInc(var x: Int64); overload;
procedure AtomInc(var x: Int64; const v: Int64); overload;
procedure AtomDec(var x: Int64); overload;
procedure AtomDec(var x: Int64; const v: Int64); overload;
procedure AtomInc(var x: UInt64); overload;
procedure AtomInc(var x: UInt64; const v: UInt64); overload;
procedure AtomDec(var x: UInt64); overload;
procedure AtomDec(var x: UInt64; const v: UInt64); overload;
procedure AtomInc(var x: Integer); overload;
procedure AtomInc(var x: Integer; const v:Integer); overload;
procedure AtomDec(var x: Integer); overload;
procedure AtomDec(var x: Integer; const v:Integer); overload;
procedure AtomInc(var x: Cardinal); overload;
procedure AtomInc(var x: Cardinal; const v:Cardinal); overload;
procedure AtomDec(var x: Cardinal); overload;
procedure AtomDec(var x: Cardinal; const v:Cardinal); overload;

procedure FillPtrByte(const dest:Pointer; Count: NativeUInt; const Value: Byte);
procedure FillPtr(const dest:Pointer; Count: NativeUInt; const Value: Byte);
procedure FillByte(const dest:Pointer; Count: NativeUInt; const Value: Byte);
function CompareMemory(const p1, p2: Pointer; Count: NativeUInt): Boolean;
procedure CopyPtr(const sour, dest:Pointer; Count: NativeUInt);

procedure RaiseInfo(const n: string); overload;
procedure RaiseInfo(const n: string; const Args: array of const); overload;

function IsMobile: Boolean;

function GetTimeTick(): TTimeTick;
function GetTimeTickCount(): TTimeTick;
function GetCrashTimeTick(): TTimeTick;

// MT19937 random num
function MT19937CoreToDelphi: Boolean;
function MT19937InstanceNum(): Integer;
procedure SetMT19937Seed(seed: Integer);
function GetMT19937Seed(): Integer;
procedure MT19937Randomize();
function MT19937Rand32(L: Integer): Integer; overload;
procedure MT19937Rand32(L: Integer; dest: PInteger; num: NativeInt); overload;
function MT19937Rand64(L: Int64): Int64; overload;
procedure MT19937Rand64(L: Int64; dest: PInt64; num: NativeInt); overload;
function MT19937RandE: Extended; overload;
procedure MT19937RandE(dest: PExtended; num: NativeInt); overload;
function MT19937RandF: Single; overload;
procedure MT19937RandF(dest: PSingle; num: NativeInt); overload;
function MT19937RandD: Double; overload;
procedure MT19937RandD(dest: PDouble; num: NativeInt); overload;
procedure MT19937SaveToStream(stream: TCoreClassStream);
procedure MT19937LoadFromStream(stream: TCoreClassStream);

function ROL8(const Value: Byte; Shift: Byte): Byte;
function ROL16(const Value: Word; Shift: Byte): Word;
function ROL32(const Value: Cardinal; Shift: Byte): Cardinal;
function ROL64(const Value: UInt64; Shift: Byte): UInt64;
function ROR8(const Value: Byte; Shift: Byte): Byte;
function ROR16(const Value: Word; Shift: Byte): Word;
function ROR32(const Value: Cardinal; Shift: Byte): Cardinal;
function ROR64(const Value: UInt64; Shift: Byte): UInt64;

function Endian(const AValue: SmallInt): SmallInt; overload;
function Endian(const AValue: Word): Word; overload;
function Endian(const AValue: Integer): Integer; overload;
function Endian(const AValue: Cardinal): Cardinal; overload;
function Endian(const AValue: Int64): Int64; overload;
function Endian(const AValue: UInt64): UInt64; overload;

function BE2N(const AValue: SmallInt): SmallInt; overload;
function BE2N(const AValue: Word): Word; overload;
function BE2N(const AValue: Integer): Integer; overload;
function BE2N(const AValue: Cardinal): Cardinal; overload;
function BE2N(const AValue: Int64): Int64; overload;
function BE2N(const AValue: UInt64): UInt64; overload;

function LE2N(const AValue: SmallInt): SmallInt; overload;
function LE2N(const AValue: Word): Word; overload;
function LE2N(const AValue: Integer): Integer; overload;
function LE2N(const AValue: Cardinal): Cardinal; overload;
function LE2N(const AValue: Int64): Int64; overload;
function LE2N(const AValue: UInt64): UInt64; overload;

function N2BE(const AValue: SmallInt): SmallInt; overload;
function N2BE(const AValue: Word): Word; overload;
function N2BE(const AValue: Integer): Integer; overload;
function N2BE(const AValue: Cardinal): Cardinal; overload;
function N2BE(const AValue: Int64): Int64; overload;
function N2BE(const AValue: UInt64): UInt64; overload;

function N2LE(const AValue: SmallInt): SmallInt; overload;
function N2LE(const AValue: Word): Word; overload;
function N2LE(const AValue: Integer): Integer; overload;
function N2LE(const AValue: Cardinal): Cardinal; overload;
function N2LE(const AValue: Int64): Int64; overload;
function N2LE(const AValue: UInt64): UInt64; overload;

procedure Swap(var v1, v2: Byte); overload;
procedure Swap(var v1, v2: Word); overload;
procedure Swap(var v1, v2: Integer); overload;
procedure Swap(var v1, v2: Cardinal); overload;
procedure Swap(var v1, v2: Int64); overload;
procedure Swap(var v1, v2: UInt64); overload;
{$IFDEF OVERLOAD_NATIVEINT}
procedure Swap(var v1, v2: NativeInt); overload;
procedure Swap(var v1, v2: NativeUInt); overload;
{$ENDIF OVERLOAD_NATIVEINT}
procedure Swap(var v1, v2: string); overload;
procedure Swap(var v1, v2: Single); overload;
procedure Swap(var v1, v2: Double); overload;
procedure Swap(var v1, v2: Pointer); overload;
procedure SwapVariant(var v1, v2: Variant);
function Swap(const v: Word): Word; overload;
function Swap(const v: Cardinal): Cardinal; overload;
function Swap(const v: UInt64): UInt64; overload;

function SAR16(const AValue: SmallInt; const Shift: Byte): SmallInt;
function SAR32(const AValue: Integer; Shift: Byte): Integer;
function SAR64(const AValue: Int64; Shift: Byte): Int64;

function MemoryAlign(addr: Pointer; alignment_: NativeUInt): Pointer;

function if_(const bool_: Boolean; const True_, False_: Boolean): Boolean; overload;
function if_(const bool_: Boolean; const True_, False_: ShortInt): ShortInt; overload;
function if_(const bool_: Boolean; const True_, False_: SmallInt): SmallInt; overload;
function if_(const bool_: Boolean; const True_, False_: Integer): Integer; overload;
function if_(const bool_: Boolean; const True_, False_: Int64): Int64; overload;
function if_(const bool_: Boolean; const True_, False_: Byte): Byte; overload;
function if_(const bool_: Boolean; const True_, False_: Word): Word; overload;
function if_(const bool_: Boolean; const True_, False_: Cardinal): Cardinal; overload;
function if_(const bool_: Boolean; const True_, False_: UInt64): UInt64; overload;
function if_(const bool_: Boolean; const True_, False_: Single): Single; overload;
function if_(const bool_: Boolean; const True_, False_: Double): Double; overload;
function if_(const bool_: Boolean; const True_, False_: string): string; overload;
function ifv_(const bool_: Boolean; const True_, False_: Variant): Variant;

{$EndRegion 'core api'}
{$Region 'core var'}

type TCheckThreadSynchronize = procedure();

var
  OnCheckThreadSynchronize: TCheckThreadSynchronize;

  // DelphiParallelFor and FPCParallelFor work in parallel
  WorkInParallelCore: TAtomBool;

  // default is True
  GlobalMemoryHook: TAtomBool;

  // core init time
  CoreInitedTimeTick: TTimeTick;

  // The life time of working in asynchronous thread consistency,
  MT19937LifeTime: TTimeTick;
{$EndRegion 'core var'}

implementation

{$INCLUDE CoreAtomic.inc}
{$INCLUDE Core_MT19937.inc}

procedure DisposeObject(const Obj: TObject);
begin
  if Obj <> nil then
    begin
      try
        {$IFDEF AUTOREFCOUNT}
        Obj.DisposeOf;
        {$ELSE AUTOREFCOUNT}
        Obj.Free;
        {$ENDIF AUTOREFCOUNT}
        {$IFDEF CriticalSimulateAtomic}
        _RecycleLocker(Obj);
        {$ENDIF CriticalSimulateAtomic}
      except
      end;
    end;
end;

procedure DisposeObject(const objs: array of TObject);
var
  Obj: TObject;
begin
  for Obj in objs do
      DisposeObject(Obj);
end;

procedure FreeObject(const Obj: TObject);
begin
  DisposeObject(Obj);
end;

procedure FreeObject(const objs: array of TObject);
var
  Obj: TObject;
begin
  for Obj in objs do
      DisposeObject(Obj);
end;

procedure DisposeObjectAndNil(var Obj);
begin
  if TObject(Obj) <> nil then
    begin
      DisposeObject(TObject(Obj));
      TObject(Obj) := nil;
    end;
end;

procedure LockObject(Obj: TObject);
{$IFNDEF CriticalSimulateAtomic}
{$IFDEF ANTI_DEAD_ATOMIC_LOCK}
var
  d: TTimeTick;
{$ENDIF ANTI_DEAD_ATOMIC_LOCK}
{$ENDIF CriticalSimulateAtomic}
begin
{$IFDEF FPC}
  _LockCriticalObj(Obj);
{$ELSE FPC}
{$IFDEF CriticalSimulateAtomic}
  _LockCriticalObj(Obj);
{$ELSE CriticalSimulateAtomic}
  {$IFDEF ANTI_DEAD_ATOMIC_LOCK}
  d := GetTimeTick;
  TMonitor.Enter(Obj, 5000);
  if GetTimeTick - d >= 5000 then
      RaiseInfo('dead');
  {$ELSE ANTI_DEAD_ATOMIC_LOCK}
  TMonitor.Enter(Obj);
  {$ENDIF ANTI_DEAD_ATOMIC_LOCK}
{$ENDIF CriticalSimulateAtomic}
{$ENDIF FPC}
end;

procedure UnLockObject(Obj: TObject);
begin
{$IFDEF FPC}
  _UnLockCriticalObj(Obj);
{$ELSE FPC}
  {$IFDEF CriticalSimulateAtomic}
  _UnLockCriticalObj(Obj);
  {$ELSE CriticalSimulateAtomic}
  TMonitor.Exit(Obj);
  {$ENDIF CriticalSimulateAtomic}
{$ENDIF FPC}
end;

procedure FillPtrByte(const dest: Pointer; Count: NativeUInt; const Value: Byte);
var
  d: PByte;
  v: UInt64;
begin
  if Count <= 0 then
      Exit;
  v := Value or (Value shl 8) or (Value shl 16) or (Value shl 24);
  v := v or (v shl 32);
  d := dest;
  while Count >= 8 do
    begin
      PUInt64(d)^ := v;
      dec(Count, 8);
      inc(d, 8);
    end;
  if Count >= 4 then
    begin
      PCardinal(d)^ := PCardinal(@v)^;
      dec(Count, 4);
      inc(d, 4);
    end;
  if Count >= 2 then
    begin
      PWORD(d)^ := PWORD(@v)^;
      dec(Count, 2);
      inc(d, 2);
    end;
  if Count > 0 then
      d^ := Value;
end;

procedure FillPtr(const dest:Pointer; Count: NativeUInt; const Value: Byte);
begin
  FillPtrByte(dest, Count, Value);
end;

procedure FillByte(const dest:Pointer; Count: NativeUInt; const Value: Byte);
begin
  FillPtrByte(dest, Count, Value);
end;

function CompareMemory(const p1, p2: Pointer; Count: NativeUInt): Boolean;
var
  b1, b2: PByte;
begin;
  if Count <= 0 then
    begin
      Result := True;
      Exit;
    end;
  Result := False;
  b1 := p1;
  b2 := p2;
  while (Count >= 8) do
    begin
      if PUInt64(b2)^ <> PUInt64(b1)^ then
          Exit;
      dec(Count, 8);
      inc(b2, 8);
      inc(b1, 8);
    end;
  if Count >= 4 then
    begin
      if PCardinal(b2)^ <> PCardinal(b1)^ then
          Exit;
      dec(Count, 4);
      inc(b2, 4);
      inc(b1, 4);
    end;
  if Count >= 2 then
    begin
      if PWORD(b2)^ <> PWORD(b1)^ then
          Exit;
      dec(Count, 2);
      inc(b2, 2);
      inc(b1, 2);
    end;
  if Count > 0 then
    if b2^ <> b1^ then
        Exit;
  Result := True;
end;

procedure CopyPtr(const sour, dest: Pointer; Count: NativeUInt);
var
  s, d: NativeUInt;
begin
  if Count = 0 then
      exit;
  if sour = dest then
      exit;

  s := NativeUInt(sour);
  d := NativeUInt(dest);
  // overlap solve
  // thanks,qq122742470,wang
  // thanks,qq4700653,LOK
  if d > s then
    begin
      inc(s, Count);
      inc(d, Count);
      while Count >= 8 do
        begin
          dec(d, 8);
          dec(s, 8);
          dec(Count, 8);
          PUInt64(d)^ := PUInt64(s)^;
        end;
      if Count >= 4 then
        begin
          dec(d, 4);
          dec(s, 4);
          dec(Count, 4);
          PCardinal(d)^ := PCardinal(s)^;
        end;
      if Count >= 2 then
        begin
          dec(d, 2);
          dec(s, 2);
          dec(Count, 2);
          PWORD(d)^ := PWORD(s)^;
        end;
      if Count > 0 then
          PByte(d - 1)^ := PByte(s - 1)^;
    end
  else
    begin
      while Count >= 8 do
        begin
          PUInt64(d)^ := PUInt64(s)^;
          dec(Count, 8);
          inc(d, 8);
          inc(s, 8);
        end;
      if Count >= 4 then
        begin
          PCardinal(d)^ := PCardinal(s)^;
          dec(Count, 4);
          inc(d, 4);
          inc(s, 4);
        end;
      if Count >= 2 then
        begin
          PWORD(d)^ := PWORD(s)^;
          dec(Count, 2);
          inc(d, 2);
          inc(s, 2);
        end;
      if Count > 0 then
          PByte(d)^ := PByte(s)^;
    end;
end;

procedure RaiseInfo(const n: string);
begin
  raise Exception.Create(n);
end;

procedure RaiseInfo(const n: string; const Args: array of const);
begin
  raise Exception.Create(Format(n, Args));
end;

function IsMobile: Boolean;
begin
  case CurrentPlatform of
    epIOS, epIOSSIM, epANDROID32, epANDROID64: Result := True;
    else Result := False;
  end;
end;

var
  Core_RunTime_Tick: TTimeTick;
  Core_Step_Tick: Cardinal;

function GetTimeTick(): TTimeTick;
var
  tick: Cardinal;
begin
  CoreTimeTickCritical.Acquire;
  try
    tick := TCoreClassThread.GetTickCount();
    inc(Core_RunTime_Tick, tick - Core_Step_Tick);
    Core_Step_Tick := tick;
    Result := Core_RunTime_Tick;
  finally
      CoreTimeTickCritical.Release;
  end;
end;

function GetTimeTickCount(): TTimeTick;
begin
  Result := GetTimeTick();
end;

function GetCrashTimeTick(): TTimeTick;
begin
  Result := $FFFFFFFFFFFFFFFF - GetTimeTick();
end;

{$INCLUDE CoreEndian.inc}

{$IFDEF FPC}

function TCoreClassInterfacedObject._AddRef: longint; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  Result := 1;
end;

function TCoreClassInterfacedObject._Release: longint; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  Result := 1;
end;

procedure TCoreClassInterfacedObject.AfterConstruction;
begin
end;

procedure TCoreClassInterfacedObject.BeforeDestruction;
begin
end;

{$ELSE}


function TCoreClassInterfacedObject._AddRef: Integer;
begin
  Result := 1;
end;

function TCoreClassInterfacedObject._Release: Integer;
begin
  Result := 1;
end;

procedure TCoreClassInterfacedObject.AfterConstruction;
begin
end;

procedure TCoreClassInterfacedObject.BeforeDestruction;
begin
end;

function TGenericsList<t>.ListData: Pointer;
begin
  // set array pointer
  Arry := TGArry(Pointer(inherited List));
  // @ array
  Result := @Arry;
end;

function TGenericsObjectList<t>.ListData: Pointer;
begin
  // set array pointer
  Arry := TGArry(Pointer(inherited List));
  // @ array
  Result := @Arry;
end;

function TCoreClassList.ListData: PCoreClassPointerList;
begin
  Result := PCoreClassPointerList(inherited ListData);
end;

function TCoreClassListForObj.ListData: PCoreClassForObjectList;
begin
  Result := PCoreClassForObjectList(inherited ListData);
end;

{$ENDIF}

constructor TCoreClassObjectList.Create;
begin
  inherited Create;
  AutoFreeObj := True;
end;

constructor TCoreClassObjectList.Create(AutoFreeObj_: Boolean);
begin
  inherited Create;
  AutoFreeObj := AutoFreeObj_;
end;

destructor TCoreClassObjectList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TCoreClassObjectList.Remove(obj: TCoreClassObject);
begin
  if AutoFreeObj then
      DisposeObject(obj);
  inherited Remove(obj);
end;

procedure TCoreClassObjectList.Delete(index: Integer);
begin
  if (index >= 0) and (index < Count) then
    begin
      if AutoFreeObj then
          disposeObject(Items[index]);
      inherited Delete(index);
    end;
end;

procedure TCoreClassObjectList.Clear;
var
  i: Integer;
begin
  if AutoFreeObj then
    for i := 0 to Count - 1 do
        disposeObject(Items[i]);
  inherited Clear;
end;

{$INCLUDE CoreComputeThread.inc}

{$IFDEF FPC}
{$INCLUDE Core_FPCParallelFor.inc}
{$ELSE FPC}
{$INCLUDE Core_DelphiParallelFor.inc}
{$ENDIF FPC}
{$INCLUDE Core_AtomVar.inc}
{$INCLUDE Core_LineProcessor.inc}


procedure Nop;
begin
end;

var
  CheckThreadSynchronizeing: TAtomBool;

procedure CheckThreadSynchronize;
begin
  CheckThreadSynchronize(0);
end;

function CheckThreadSynchronize(Timeout: Integer): Boolean;
begin
  if TCoreClassThread.CurrentThread.ThreadID <> MainThreadID then
    begin
      TCoreClassThread.Sleep(Timeout);
      Result := False;
    end
  else
    begin
      if not CheckThreadSynchronizeing.V then
        begin
          CheckThreadSynchronizeing.V := True;
          try
              Result := CheckSynchronize(Timeout);
          finally
              CheckThreadSynchronizeing.V := False;
          end;
        end
      else
        Result := False;
    end;
  if Assigned(OnCheckThreadSynchronize) then
    OnCheckThreadSynchronize();
end;

initialization
  OnCheckThreadSynchronize := nil;
  WorkInParallelCore := TAtomBool.Create({$IFDEF FPC}True{$ELSE FPC}DebugHook = 0{$ENDIF FPC});
  GlobalMemoryHook := TAtomBool.Create(True);
  CheckThreadSynchronizeing := TAtomBool.Create(False);
  Core_RunTime_Tick := C_Tick_Day * 3;
  Core_Step_Tick := TCoreClassThread.GetTickCount();
  InitCriticalLock();
  InitMT19937Rand();
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
  CoreInitedTimeTick := GetTimeTick();
  InitCoreThreadPool(CpuCount);
finalization
  FreeCoreThreadPool;
  FreeMT19937Rand();
  FreeCriticalLock;
  CheckThreadSynchronizeing.Free;
  CheckThreadSynchronizeing := nil;
  WorkInParallelCore.Free;
  WorkInParallelCore := nil;
  GlobalMemoryHook.Free;
  GlobalMemoryHook := nil;
end.
