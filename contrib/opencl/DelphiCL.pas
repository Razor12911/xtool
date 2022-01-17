(* ****************************************** *)
(* *)
(* DelphiCL *)
(* *)
(* created by      : Maksym Tymkovych *)
(* (niello) *)
(* *)
(* headers versions: 0.07 *)
(* file name       : DelphiCL.pas *)
(* last modify     : 10.12.11 *)
(* license         : BSD *)
(* *)
(* Site            : www.niello.org.ua *)
(* e-mail          : muxamed13@ukr.net *)
(* ICQ             : 446-769-253 *)
(* *)
(* ********Copyright (c) niello 2008-2011**** *)

unit DelphiCL;

interface

uses
  OpenCL,
  Windows,
  SysUtils;

type

  TDCLMemFlags = (mfReadWrite, mfWriteOnly, mfReadOnly, mfUseHostPtr,
    mfAllocHostPtr, mfCopyHostPtr);
  TDCLMemFlagsSet = set of TDCLMemFlags;

  TDCLBuffer = class
  private
    FMem: PCL_mem;
    FStatus: CL_int;
    FSize: Size_t;
  protected
    constructor Create(const Context: PCL_context; const Flags: TDCLMemFlagsSet;
      const Size: Size_t; const Data: Pointer = nil);
  public
    procedure Free();
    property Size: Size_t read FSize;
    property Status: CL_int read FStatus;
  end;

  TDCLImage2D = class
  private
    FMem: PCL_mem;
    FStatus: CL_int;
    FFormat: CL_image_format;
    FWidth: Size_t;
    FHeight: Size_t;
    FRowPitch: Size_t;
  protected
    constructor Create(const Context: PCL_context; const Flags: TDCLMemFlagsSet;
      const Format: PCL_image_format; const Width, Height: Size_t;
      const RowPitch: Size_t = 0; const Data: Pointer = nil);
  public
    procedure Free();
    property Width: Size_t read FWidth;
    property Height: Size_t read FHeight;
    property RowPitch: Size_t read FRowPitch;
    property Status: CL_int read FStatus;
  end;

  TDCLCommandQueueProperties = (cqpNone, cqpOutOfOrderExecModeEnable);
  TDCLCommandQueuePropertiesSet = set of TDCLCommandQueueProperties;

  TDCLKernel = class
  private
    FKernel: PCL_kernel;
    FStatus: CL_int;
  protected
    constructor Create(const Program_: PCL_program; const KernelName: PPChar);
    function GetFunctionName(): AnsiString;
    function GetNumArgs(): CL_uint;
  public
    property Status: CL_int read FStatus;
    property FunctionName: AnsiString read GetFunctionName;
    property NumArgs: CL_uint read GetNumArgs;
    procedure SetArg(const Index: CL_uint; const Size: Size_t;
      const Value: Pointer); overload;
    procedure SetArg(const Index: CL_uint; const Value: TDCLBuffer); overload;
    procedure SetArg(const Index: CL_uint; const Value: TDCLImage2D); overload;
    procedure Free();
  end;

  TDCLCommandQueue = class
  private
    FCommandQueue: PCL_command_queue;
    FStatus: CL_int;
    FProperties: TDCLCommandQueuePropertiesSet;
    constructor Create(const Device_Id: PCL_device_id;
      const Context: PCL_context;
      const Properties: TDCLCommandQueuePropertiesSet = [cqpNone]);
  public
    procedure ReadBuffer(const Buffer: TDCLBuffer; const Size: Size_t;
      const Data: Pointer);
    procedure WriteBuffer(const Buffer: TDCLBuffer; const Size: Size_t;
      const Data: Pointer);
    procedure ReadImage2D(const Image: TDCLImage2D; const Width, Height: Size_t;
      const Data: Pointer);
    procedure WriteImage2D(const Image: TDCLImage2D;
      const Width, Height: Size_t; const Data: Pointer);
    procedure Execute(const Kernel: TDCLKernel; const Size: Size_t); overload;
    procedure Execute(const Kernel: TDCLKernel; // const Device: PCL_device_id;
      const Size: array of Size_t); overload;
    property Status: CL_int read FStatus;
    property Properties: TDCLCommandQueuePropertiesSet read FProperties;
    procedure Free();
  end;

  TArraySize_t = Array of Size_t;

  TDCLProgram = class
  private
    FProgram: PCL_program;
    FStatus: CL_int;
    FSource: PAnsiChar;
    FBinarySizesCount: Size_t;
    FBinarySizes: TArraySize_t;
    // FBinaries: PByte;
  protected
    constructor Create(const Context: PCL_context; const Source: PPChar;
      const Options: PPChar = nil);
    function GetBinarySizes(const Index: Size_t): Size_t;
  public
    property BinarySizes[const Index: Size_t]: Size_t read GetBinarySizes;
    property BinarySizesCount: Size_t read FBinarySizesCount;
    property Source: PAnsiChar read FSource;
    property Status: CL_int read FStatus;
    function CreateKernel(const KernelName: PPChar): TDCLKernel;
    procedure Free();
  end;

  TDCLContext = class
  private
    FContext: PCL_context;
    FStatus: CL_int;
    FNumDevices: CL_uint;
  protected
    // property Context: PCL_context read FContext;
  public
    constructor Create(Device_Id: PCL_device_id);
    property Status: CL_int read FStatus;
    property NumDevices: CL_uint read FNumDevices;
    procedure Free();
  end;

  TDCLDevice = class
    // private
    FDevice_id: PCL_device_id;
  private
    FStatus: CL_int;

    FName: AnsiString;
    FVendor: AnsiString;
    FVersion: AnsiString;
    FProfile: AnsiString;

    FIsCPU: Boolean;
    FIsGPU: Boolean;
    FIsAccelerator: Boolean;
    FIsDefault: Boolean;

    FMaxWorkGroupSize: Size_t;

    FNativeVectorPreferredChar: CL_uint;
    FNativeVectorPreferredShort: CL_uint;
    FNativeVectorPreferredInt: CL_uint;
    FNativeVectorPreferredLong: CL_uint;
    FNativeVectorPreferredFloat: CL_uint;
    FNativeVectorPreferredDouble: CL_uint;
    FNativeVectorPreferredHalf: CL_uint;
    FNativeVectorWidthChar: CL_uint;
    FNativeVectorWidthShort: CL_uint;
    FNativeVectorWidthInt: CL_uint;
    FNativeVectorWidthLong: CL_uint;
    FNativeVectorWidthFloat: CL_uint;
    FNativeVectorWidthDouble: CL_uint;
    FNativeVectorWidthHalf: CL_uint;

    FMaxClockFrequency: CL_uint;
    FAddressBits: CL_uint;
    FMaxMemAllocSize: CL_ulong;

    FIsImageSupport: Boolean;

    FMaxReadImageArgs: CL_uint;
    FMaxWriteImageArgs: CL_uint;
    FImage2DMaxWidth: Size_t;
    FImage2DMaxHeight: Size_t;
    FImage3DMaxWidth: Size_t;
    FImage3DMaxHeight: Size_t;
    FImage3DMaxDepth: Size_t;
    FMaxSamplers: CL_uint;
    FMaxParameterSize: Size_t;
    FMemBaseAddrAlign: CL_uint;
    FMinDataTypeAlignSize: CL_uint;

    FGlobalMemCacheLineSize: CL_uint;
    FGlobalMemCacheSize: CL_ulong;
    FGlobalMemSize: CL_ulong;
    FMaxConstantBufferSize: CL_ulong;
    FMaxConstantArgs: CL_uint;

    FLocalMemSize: CL_ulong;
    FIsErrorCorrectionSupport: Boolean;
    FIsHostUnifiedMemory: Boolean;
    FProfilingTimerResolution: Size_t;
    FIsEndianLittle: Boolean;
    FIsAvailable: Boolean;
    FIsCompilerAvailable: Boolean;

    FVendorId: CL_uint;
    FMaxComputeUnits: CL_uint;
    FMaxWorkItemDimensions: CL_uint;
    FExtensionsString: AnsiString;
    FOpenCLCVersion: AnsiString;
    FDriverVersion: AnsiString;

    FExtensionsCount: Size_t;
    FExtensions: Array of AnsiString;

    FContext: TDCLContext;

    function GetExtensions(const Index: Size_t): AnsiString;
    function IsPresentExtension(const ExtensionName: AnsiString): Boolean;
  protected
    constructor Create(Device_Id: PCL_device_id);
    property Device_Id: PCL_device_id read FDevice_id;
  public
    property Status: CL_int read FStatus;

    property Name: AnsiString read FName;
    property Vendor: AnsiString read FVendor;
    property Version: AnsiString read FVersion;
    property Profile: AnsiString read FProfile;

    property IsCPU: Boolean read FIsCPU;
    property IsGPU: Boolean read FIsGPU;
    property IsAccelerator: Boolean read FIsAccelerator;
    property IsDefault: Boolean read FIsDefault;

    property MaxWorkGroupSize: Size_t read FMaxWorkGroupSize;

    property NativeVectorPreferredChar: CL_uint read FNativeVectorPreferredChar;
    property NativeVectorPreferredShort: CL_uint
      read FNativeVectorPreferredShort;
    property NativeVectorPreferredInt: CL_uint read FNativeVectorPreferredInt;
    property NativeVectorPreferredLong: CL_uint read FNativeVectorPreferredLong;
    property NativeVectorPreferredFloat: CL_uint
      read FNativeVectorPreferredFloat;
    property NativeVectorPreferredDouble: CL_uint
      read FNativeVectorPreferredDouble;
    property NativeVectorPreferredHalf: CL_uint read FNativeVectorPreferredHalf;
    property NativeVectorWidthChar: CL_uint read FNativeVectorWidthChar;
    property NativeVectorWidthShort: CL_uint read FNativeVectorWidthShort;
    property NativeVectorWidthInt: CL_uint read FNativeVectorWidthInt;
    property NativeVectorWidthLong: CL_uint read FNativeVectorWidthLong;
    property NativeVectorWidthFloat: CL_uint read FNativeVectorWidthFloat;
    property NativeVectorWidthDouble: CL_uint read FNativeVectorWidthDouble;
    property NativeVectorWidthHalf: CL_uint read FNativeVectorWidthHalf;

    property MaxClockFrequency: CL_uint read FMaxClockFrequency;
    property AddressBits: CL_uint read FAddressBits;
    property MaxMemAllocSize: CL_ulong read FMaxMemAllocSize;

    property IsImageSupport: Boolean read FIsImageSupport;

    property MaxReadImageArgs: CL_uint read FMaxReadImageArgs;
    property MaxWriteImageArgs: CL_uint read FMaxWriteImageArgs;
    property Image2DMaxWidth: Size_t read FImage2DMaxWidth;
    property Image2DMaxHeight: Size_t read FImage2DMaxHeight;
    property Image3DMaxWidth: Size_t read FImage3DMaxWidth;
    property Image3DMaxHeight: Size_t read FImage3DMaxHeight;
    property Image3DMaxDepth: Size_t read FImage3DMaxDepth;
    property MaxSamplers: CL_uint read FMaxSamplers;
    property MaxParameterSize: Size_t read FMaxParameterSize;
    property MemBaseAddrAlign: CL_uint read FMemBaseAddrAlign;
    property MinDataTypeAlignSize: CL_uint read FMinDataTypeAlignSize;

    property GlobalMemCacheLineSize: CL_uint read FGlobalMemCacheLineSize;
    property GlobalMemCacheSize: CL_ulong read FGlobalMemCacheSize;
    property GlobalMemSize: CL_ulong read FGlobalMemSize;
    property MaxConstantBufferSize: CL_ulong read FMaxConstantBufferSize;
    property MaxConstantArgs: CL_uint read FMaxConstantArgs;

    property LocalMemSize: CL_ulong read FLocalMemSize;
    property IsErrorCorrectionSupport: Boolean read FIsErrorCorrectionSupport;
    property IsHostUnifiedMemory: Boolean read FIsHostUnifiedMemory;
    property ProfilingTimerResolution: Size_t read FProfilingTimerResolution;
    property IsEndianLittle: Boolean read FIsEndianLittle;
    property IsAvailable: Boolean read FIsAvailable;
    property IsCompilerAvailable: Boolean read FIsCompilerAvailable;

    property VendorId: CL_uint read FVendorId;
    property MaxComputeUnits: CL_uint read FMaxComputeUnits;
    property MaxWorkItemDimensions: CL_uint read FMaxWorkItemDimensions;

    property DriverVersion: AnsiString read FDriverVersion;
    property OpenCLCVersion: AnsiString read FOpenCLCVersion;
    property ExtensionsString: AnsiString read FExtensionsString;

    property Context: TDCLContext read FContext;
    function CreateContext(): TDCLContext;
    function CreateCommandQueue(const Properties
      : TDCLCommandQueuePropertiesSet = [cqpNone]): TDCLCommandQueue;
    function CreateBuffer(const Size: Size_t; const Data: Pointer = nil;
      const Flags: TDCLMemFlagsSet = [mfReadWrite]): TDCLBuffer;
    function CreateImage2D(const Format: PCL_image_format;
      const Width, Height, RowPitch: Size_t; const Data: Pointer = nil;
      const Flags: TDCLMemFlagsSet = [mfReadWrite]): TDCLImage2D;
    function CreateProgram(const Source: PPChar; const Options: PPChar = nil)
      : TDCLProgram; overload;
    function CreateProgram(const FileName: String; const Options: PPChar = nil)
      : TDCLProgram; overload;

    property ExtensionsCount: Size_t read FExtensionsCount;
    property Extensions[const Index: Size_t]: AnsiString read GetExtensions;
    property IsSupportedExtension[const Index: AnsiString]: Boolean
      read IsPresentExtension;
    procedure Free();
  end;

  TDCLPlatform = class
  private
    FPlatform_id: PCL_platform_id;
    FProfile: AnsiString;
    FVersion: AnsiString;
    FName: AnsiString;
    FVendor: AnsiString;
    FExtensionsString: AnsiString;
    FStatus: CL_int;
    FDevices: Array of TDCLDevice;
    FDeviceCount: CL_uint;
    FExtensionsCount: Size_t;
    FExtensions: Array of AnsiString;
    function GetDevice(Index: CL_uint): TDCLDevice;
    function GetExtensions(Index: Size_t): AnsiString;
    function IsPresentExtension(const ExtensionName: AnsiString): Boolean;

    function GetDeviceWithMaxClockFrequency(): TDCLDevice;
    function GetDeviceWithMaxComputeUnits(): TDCLDevice;

    function GetDeviceWithMaxGlobalMemCacheLineSize(): TDCLDevice;
    function GetDeviceWithMaxGlobalMemCacheSize(): TDCLDevice;
    function GetDeviceWithMaxGlobalMemSize(): TDCLDevice;

    function GetDeviceWithMaxImage2DWidth(): TDCLDevice;
    function GetDeviceWithMaxImage2DHeight(): TDCLDevice;
    function GetDeviceWithMaxImage3DWidth(): TDCLDevice;
    function GetDeviceWithMaxImage3DHeight(): TDCLDevice;
    function GetDeviceWithMaxImage3DDepth(): TDCLDevice;

    function GetDeviceWithMaxLocalMemSize(): TDCLDevice;
    function GetDeviceWithMaxConstantArgs(): TDCLDevice;
    function GetDeviceWithMaxConstantBufferSize(): TDCLDevice;
    function GetDeviceWithMaxMemAllocSize(): TDCLDevice;
    function GetDeviceWithMaxParameterSize(): TDCLDevice;
    function GetDeviceWithMaxReadImageArgs(): TDCLDevice;
    function GetDeviceWithMaxSamplers(): TDCLDevice;
    function GetDeviceWithMaxWorkGroupSize(): TDCLDevice;
    function GetDeviceWithMaxWorkItemDimensions(): TDCLDevice;
    function GetDeviceWithMaxWriteImageArgs(): TDCLDevice;
  public
    constructor Create(Platform_id: PCL_platform_id);
    property Profile: AnsiString read FProfile;
    property Version: AnsiString read FVersion;
    property Name: AnsiString read FName;
    property Vendor: AnsiString read FVendor;
    property ExtensionsString: AnsiString read FExtensionsString;

    property DeviceCount: CL_uint read FDeviceCount;
    property Status: CL_int read FStatus;
    property Devices[Index: CL_uint]: TDCLDevice read GetDevice;
    property ExtensionsCount: Size_t read FExtensionsCount;
    property Extensions[Index: Size_t]: AnsiString read GetExtensions;
    property IsSupportedExtension[const Index: AnsiString]: Boolean
      read IsPresentExtension;

    property DeviceWithMaxClockFrequency: TDCLDevice
      read GetDeviceWithMaxClockFrequency;
    property DeviceWithMaxComputeUnits: TDCLDevice
      read GetDeviceWithMaxComputeUnits;
    property DeviceWithMaxGlobalMemCacheLineSize: TDCLDevice
      read GetDeviceWithMaxGlobalMemCacheLineSize;
    property DeviceWithMaxGlobalMemCacheSize: TDCLDevice
      read GetDeviceWithMaxGlobalMemCacheSize;
    property DeviceWithMaxGlobalMemSize: TDCLDevice
      read GetDeviceWithMaxGlobalMemSize;
    property DeviceWithMaxImage2DWidth: TDCLDevice
      read GetDeviceWithMaxImage2DWidth;
    property DeviceWithMaxImage2DHeight: TDCLDevice
      read GetDeviceWithMaxImage2DHeight;
    property DeviceWithMaxImage3DWidth: TDCLDevice
      read GetDeviceWithMaxImage3DWidth;
    property DeviceWithMaxImage3DHeight: TDCLDevice
      read GetDeviceWithMaxImage3DHeight;
    property DeviceWithMaxImage3DDepth: TDCLDevice
      read GetDeviceWithMaxImage3DDepth;
    property DeviceWithMaxLocalMemSize: TDCLDevice
      read GetDeviceWithMaxLocalMemSize;
    property DeviceWithMaxConstantArgs: TDCLDevice
      read GetDeviceWithMaxConstantArgs;
    property DeviceWithMaxConstantBufferSize: TDCLDevice
      read GetDeviceWithMaxConstantBufferSize;
    property DeviceWithMaxMemAllocSize: TDCLDevice
      read GetDeviceWithMaxMemAllocSize;
    property DeviceWithMaxParameterSize: TDCLDevice
      read GetDeviceWithMaxParameterSize;
    property DeviceWithMaxReadImageArgs: TDCLDevice
      read GetDeviceWithMaxReadImageArgs;
    property DeviceWithMaxSamplers: TDCLDevice read GetDeviceWithMaxSamplers;
    property DeviceWithMaxWorkGroupSize: TDCLDevice
      read GetDeviceWithMaxWorkGroupSize;
    property DeviceWithMaxWorkItemDimensions: TDCLDevice
      read GetDeviceWithMaxWorkItemDimensions;
    property DeviceWithMaxWriteImageArgs: TDCLDevice
      read GetDeviceWithMaxWriteImageArgs;
    procedure Free();
  end;

  TDCLPlatforms = class
  private
    FPlatforms: Array of TDCLPlatform;
    FPlatformCount: CL_uint;
    FStatus: CL_int;
    function GetPlatform(Index: CL_uint): TDCLPlatform;
  public
    constructor Create();
    property PlatformCount: CL_uint read FPlatformCount;
    property Status: CL_int read FStatus;
    property Platforms[Index: CL_uint]: TDCLPlatform read GetPlatform;
    procedure Free();
  end;

implementation

function UpperCase(const S: AnsiString): AnsiString;
var
  Ch: AnsiChar;
  L: Integer;
  Source, Dest: PAnsiChar;
begin
  L := Length(S);
  SetLength(Result, L);
  Source := Pointer(S);
  Dest := Pointer(Result);
  while L <> 0 do
  begin
    Ch := Source^;
    if (Ch >= 'a') and (Ch <= 'z') then
      Dec(Ch, 32);
    Dest^ := Ch;
    Inc(Source);
    Inc(Dest);
    Dec(L);
  end;
end;

function IntToStr(Value: Integer): AnsiString;
begin
  Str(Value, Result);
end;

{ TDCLPlatforms }

constructor TDCLPlatforms.Create;
var
  Platforms: Array of PCL_platform_id;
  i: Integer;
begin
  FStatus := clGetPlatformIDs(0, nil, @FPlatformCount);
  if FStatus = CL_SUCCESS then
  begin
    if FPlatformCount > 0 then
    begin
      SetLength(Platforms, FPlatformCount);
      SetLength(FPlatforms, FPlatformCount);
      FStatus := clGetPlatformIDs(FPlatformCount, @Platforms[0], nil);
      for i := 0 to FPlatformCount - 1 do
      begin
        FPlatforms[i] := TDCLPlatform.Create(Platforms[i]);
      end;
      SetLength(Platforms, 0);
    end;
  end;
end;

procedure TDCLPlatforms.Free;
var
  i: Integer;
begin
  for i := 0 to FPlatformCount - 1 do
  begin
    FPlatforms[i].Free();
  end;
  SetLength(FPlatforms, 0);
  inherited Free();
end;

function TDCLPlatforms.GetPlatform(Index: CL_uint): TDCLPlatform;
begin
  if (Index < FPlatformCount) then
    Result := FPlatforms[Index]
  else
    Result := nil;
end;

{ TDCLPlatform }

constructor TDCLPlatform.Create(Platform_id: PCL_platform_id);
var
  Size: Size_t;
  Devices: Array of PCL_device_id;
  i, current, previous: Integer;

begin
  inherited Create();
  FPlatform_id := Platform_id;
  FStatus := clGetPlatformInfo(FPlatform_id, CL_PLATFORM_PROFILE, 0, nil, Size);
  SetLength(FProfile, Size);
  FStatus := clGetPlatformInfo(FPlatform_id, CL_PLATFORM_PROFILE, Size,
    @FProfile[1], Size);
  // FProfile := Buffer;

  FStatus := clGetPlatformInfo(FPlatform_id, CL_PLATFORM_VERSION, 0, nil, Size);
  SetLength(FVersion, Size);
  FStatus := clGetPlatformInfo(FPlatform_id, CL_PLATFORM_VERSION, Size,
    @FVersion[1], Size);
  SetLength(FName, Size);
  FStatus := clGetPlatformInfo(FPlatform_id, CL_PLATFORM_NAME, Size,
    @FName[1], Size);
  SetLength(FVendor, Size);
  FStatus := clGetPlatformInfo(FPlatform_id, CL_PLATFORM_VENDOR, Size,
    @FVendor[1], Size);
  SetLength(FExtensionsString, Size);
  FStatus := clGetPlatformInfo(FPlatform_id, CL_PLATFORM_EXTENSIONS, Size,
    @FExtensionsString[1], Size);
  FExtensionsCount := 0;
  i := 1;
  while (i <= Length(FExtensionsString)) do
  begin
    if ((FExtensionsString[i] = ' ') or (FExtensionsString[i] = #0)) then
      Inc(FExtensionsCount);
    Inc(i);
  end;
  SetLength(FExtensions, FExtensionsCount);
  previous := 1;
  current := 1;
  i := 0;
  while (current <= Length(FExtensionsString)) do
  begin
    if ((FExtensionsString[current] = ' ') or (FExtensionsString[current] = #0))
    then
    begin
      FExtensions[i] := UpperCase(Copy(FExtensionsString, previous,
        current - previous - 1));
      previous := current + 1;
      Inc(i);
    end;
    Inc(current);
  end;

  FStatus := clGetDeviceIDs(FPlatform_id, CL_DEVICE_TYPE_ALL, 0, nil,
    @FDeviceCount);
  if FDeviceCount > 0 then
  begin
    SetLength(Devices, FDeviceCount);
    FStatus := clGetDeviceIDs(FPlatform_id, CL_DEVICE_TYPE_ALL, FDeviceCount,
      @Devices[0], nil);
    SetLength(FDevices, FDeviceCount);
    for i := 0 to FDeviceCount - 1 do
    begin
      FDevices[i] := TDCLDevice.Create(Devices[i]);
    end;
  end;

end;

procedure TDCLPlatform.Free;
var
  i: Integer;
begin
  SetLength(FExtensions, 0);
  FExtensionsString := '';
  for i := 0 to FDeviceCount - 1 do
  begin
    FDevices[i].Free();
  end;
  SetLength(FDevices, 0);
  inherited Free();
end;

function TDCLPlatform.GetDevice(Index: CL_uint): TDCLDevice;
begin
  if (Index < FDeviceCount) then
    Result := FDevices[Index]
  else
    Result := nil;
end;

function TDCLPlatform.GetDeviceWithMaxClockFrequency: TDCLDevice;
  function GetParameterDevice(const Device: TDCLDevice): CL_uint;
  begin
    Result := Device.MaxClockFrequency;
  end;

var
  i: Integer;
  MaxValue: CL_uint;
  MaxValuePos: CL_uint;
begin
  if FDeviceCount = 0 then
  begin
    Result := nil;
    Exit;
  end;
  MaxValue := GetParameterDevice(FDevices[0]);
  MaxValuePos := 0;
  for i := 1 to FDeviceCount - 1 do
  begin
    if GetParameterDevice(FDevices[i]) > MaxValue then
    begin
      MaxValue := GetParameterDevice(FDevices[i]);
      MaxValuePos := i;
    end;
  end;
  Result := FDevices[MaxValuePos];
end;

function TDCLPlatform.GetDeviceWithMaxComputeUnits: TDCLDevice;
  function GetParameterDevice(const Device: TDCLDevice): CL_uint;
  begin
    Result := Device.MaxComputeUnits;
  end;

var
  i: Integer;
  MaxValue: CL_uint;
  MaxValuePos: CL_uint;
begin
  if FDeviceCount = 0 then
  begin
    Result := nil;
    Exit;
  end;
  MaxValue := GetParameterDevice(FDevices[0]);
  MaxValuePos := 0;
  for i := 1 to FDeviceCount - 1 do
  begin
    if GetParameterDevice(FDevices[i]) > MaxValue then
    begin
      MaxValue := GetParameterDevice(FDevices[i]);
      MaxValuePos := i;
    end;
  end;
  Result := FDevices[MaxValuePos];
end;

function TDCLPlatform.GetDeviceWithMaxConstantArgs: TDCLDevice;
  function GetParameterDevice(const Device: TDCLDevice): CL_uint;
  begin
    Result := Device.MaxConstantArgs;
  end;

var
  i: Integer;
  MaxValue: CL_uint;
  MaxValuePos: CL_uint;
begin
  if FDeviceCount = 0 then
  begin
    Result := nil;
    Exit;
  end;
  MaxValue := GetParameterDevice(FDevices[0]);
  MaxValuePos := 0;
  for i := 1 to FDeviceCount - 1 do
  begin
    if GetParameterDevice(FDevices[i]) > MaxValue then
    begin
      MaxValue := GetParameterDevice(FDevices[i]);
      MaxValuePos := i;
    end;
  end;
  Result := FDevices[MaxValuePos];
end;

function TDCLPlatform.GetDeviceWithMaxConstantBufferSize: TDCLDevice;
  function GetParameterDevice(const Device: TDCLDevice): CL_ulong;
  begin
    Result := Device.MaxConstantBufferSize;
  end;

var
  i: Integer;
  MaxValue: CL_ulong;
  MaxValuePos: CL_uint;
begin
  if FDeviceCount = 0 then
  begin
    Result := nil;
    Exit;
  end;
  MaxValue := GetParameterDevice(FDevices[0]);
  MaxValuePos := 0;
  for i := 1 to FDeviceCount - 1 do
  begin
    if GetParameterDevice(FDevices[i]) > MaxValue then
    begin
      MaxValue := GetParameterDevice(FDevices[i]);
      MaxValuePos := i;
    end;
  end;
  Result := FDevices[MaxValuePos];
end;

function TDCLPlatform.GetDeviceWithMaxGlobalMemCacheLineSize: TDCLDevice;
  function GetParameterDevice(const Device: TDCLDevice): CL_uint;
  begin
    Result := Device.GlobalMemCacheLineSize;
  end;

var
  i: Integer;
  MaxValue: CL_uint;
  MaxValuePos: CL_uint;
begin
  if FDeviceCount = 0 then
  begin
    Result := nil;
    Exit;
  end;
  MaxValue := GetParameterDevice(FDevices[0]);
  MaxValuePos := 0;
  for i := 1 to FDeviceCount - 1 do
  begin
    if GetParameterDevice(FDevices[i]) > MaxValue then
    begin
      MaxValue := GetParameterDevice(FDevices[i]);
      MaxValuePos := i;
    end;
  end;
  Result := FDevices[MaxValuePos];
end;

function TDCLPlatform.GetDeviceWithMaxGlobalMemCacheSize: TDCLDevice;
  function GetParameterDevice(const Device: TDCLDevice): CL_ulong;
  begin
    Result := Device.GlobalMemCacheSize;
  end;

var
  i: Integer;
  MaxValue: CL_ulong;
  MaxValuePos: CL_uint;
begin
  if FDeviceCount = 0 then
  begin
    Result := nil;
    Exit;
  end;
  MaxValue := GetParameterDevice(FDevices[0]);
  MaxValuePos := 0;
  for i := 1 to FDeviceCount - 1 do
  begin
    if GetParameterDevice(FDevices[i]) > MaxValue then
    begin
      MaxValue := GetParameterDevice(FDevices[i]);
      MaxValuePos := i;
    end;
  end;
  Result := FDevices[MaxValuePos];
end;

function TDCLPlatform.GetDeviceWithMaxGlobalMemSize: TDCLDevice;
  function GetParameterDevice(const Device: TDCLDevice): CL_ulong;
  begin
    Result := Device.GlobalMemSize;
  end;

var
  i: Integer;
  MaxValue: CL_ulong;
  MaxValuePos: CL_uint;
begin
  if FDeviceCount = 0 then
  begin
    Result := nil;
    Exit;
  end;
  MaxValue := GetParameterDevice(FDevices[0]);
  MaxValuePos := 0;
  for i := 1 to FDeviceCount - 1 do
  begin
    if GetParameterDevice(FDevices[i]) > MaxValue then
    begin
      MaxValue := GetParameterDevice(FDevices[i]);
      MaxValuePos := i;
    end;
  end;
  Result := FDevices[MaxValuePos];
end;

function TDCLPlatform.GetDeviceWithMaxImage2DHeight: TDCLDevice;
  function GetParameterDevice(const Device: TDCLDevice): Size_t;
  begin
    Result := Device.Image2DMaxHeight;
  end;

var
  i: Integer;
  MaxValue: Size_t;
  MaxValuePos: CL_uint;
begin
  if FDeviceCount = 0 then
  begin
    Result := nil;
    Exit;
  end;
  MaxValue := GetParameterDevice(FDevices[0]);
  MaxValuePos := 0;
  for i := 1 to FDeviceCount - 1 do
  begin
    if GetParameterDevice(FDevices[i]) > MaxValue then
    begin
      MaxValue := GetParameterDevice(FDevices[i]);
      MaxValuePos := i;
    end;
  end;
  Result := FDevices[MaxValuePos];
end;

function TDCLPlatform.GetDeviceWithMaxImage2DWidth: TDCLDevice;
  function GetParameterDevice(const Device: TDCLDevice): Size_t;
  begin
    Result := Device.Image2DMaxWidth;
  end;

var
  i: Integer;
  MaxValue: Size_t;
  MaxValuePos: CL_uint;
begin
  if FDeviceCount = 0 then
  begin
    Result := nil;
    Exit;
  end;
  MaxValue := GetParameterDevice(FDevices[0]);
  MaxValuePos := 0;
  for i := 1 to FDeviceCount - 1 do
  begin
    if GetParameterDevice(FDevices[i]) > MaxValue then
    begin
      MaxValue := GetParameterDevice(FDevices[i]);
      MaxValuePos := i;
    end;
  end;
  Result := FDevices[MaxValuePos];
end;

function TDCLPlatform.GetDeviceWithMaxImage3DDepth: TDCLDevice;
  function GetParameterDevice(const Device: TDCLDevice): Size_t;
  begin
    Result := Device.Image3DMaxDepth;
  end;

var
  i: Integer;
  MaxValue: Size_t;
  MaxValuePos: CL_uint;
begin
  if FDeviceCount = 0 then
  begin
    Result := nil;
    Exit;
  end;
  MaxValue := GetParameterDevice(FDevices[0]);
  MaxValuePos := 0;
  for i := 1 to FDeviceCount - 1 do
  begin
    if GetParameterDevice(FDevices[i]) > MaxValue then
    begin
      MaxValue := GetParameterDevice(FDevices[i]);
      MaxValuePos := i;
    end;
  end;
  Result := FDevices[MaxValuePos];
end;

function TDCLPlatform.GetDeviceWithMaxImage3DHeight: TDCLDevice;
  function GetParameterDevice(const Device: TDCLDevice): Size_t;
  begin
    Result := Device.Image3DMaxHeight;
  end;

var
  i: Integer;
  MaxValue: Size_t;
  MaxValuePos: CL_uint;
begin
  if FDeviceCount = 0 then
  begin
    Result := nil;
    Exit;
  end;
  MaxValue := GetParameterDevice(FDevices[0]);
  MaxValuePos := 0;
  for i := 1 to FDeviceCount - 1 do
  begin
    if GetParameterDevice(FDevices[i]) > MaxValue then
    begin
      MaxValue := GetParameterDevice(FDevices[i]);
      MaxValuePos := i;
    end;
  end;
  Result := FDevices[MaxValuePos];
end;

function TDCLPlatform.GetDeviceWithMaxImage3DWidth: TDCLDevice;
  function GetParameterDevice(const Device: TDCLDevice): Size_t;
  begin
    Result := Device.Image3DMaxWidth;
  end;

var
  i: Integer;
  MaxValue: Size_t;
  MaxValuePos: CL_uint;
begin
  if FDeviceCount = 0 then
  begin
    Result := nil;
    Exit;
  end;
  MaxValue := GetParameterDevice(FDevices[0]);
  MaxValuePos := 0;
  for i := 1 to FDeviceCount - 1 do
  begin
    if GetParameterDevice(FDevices[i]) > MaxValue then
    begin
      MaxValue := GetParameterDevice(FDevices[i]);
      MaxValuePos := i;
    end;
  end;
  Result := FDevices[MaxValuePos];
end;

function TDCLPlatform.GetDeviceWithMaxLocalMemSize: TDCLDevice;
  function GetParameterDevice(const Device: TDCLDevice): CL_ulong;
  begin
    Result := Device.LocalMemSize;
  end;

var
  i: Integer;
  MaxValue: CL_ulong;
  MaxValuePos: CL_uint;
begin
  if FDeviceCount = 0 then
  begin
    Result := nil;
    Exit;
  end;
  MaxValue := GetParameterDevice(FDevices[0]);
  MaxValuePos := 0;
  for i := 1 to FDeviceCount - 1 do
  begin
    if GetParameterDevice(FDevices[i]) > MaxValue then
    begin
      MaxValue := GetParameterDevice(FDevices[i]);
      MaxValuePos := i;
    end;
  end;
  Result := FDevices[MaxValuePos];
end;

function TDCLPlatform.GetDeviceWithMaxMemAllocSize: TDCLDevice;
  function GetParameterDevice(const Device: TDCLDevice): CL_ulong;
  begin
    Result := Device.MaxMemAllocSize;
  end;

var
  i: Integer;
  MaxValue: CL_ulong;
  MaxValuePos: CL_uint;
begin
  if FDeviceCount = 0 then
  begin
    Result := nil;
    Exit;
  end;
  MaxValue := GetParameterDevice(FDevices[0]);
  MaxValuePos := 0;
  for i := 1 to FDeviceCount - 1 do
  begin
    if GetParameterDevice(FDevices[i]) > MaxValue then
    begin
      MaxValue := GetParameterDevice(FDevices[i]);
      MaxValuePos := i;
    end;
  end;
  Result := FDevices[MaxValuePos];
end;

function TDCLPlatform.GetDeviceWithMaxParameterSize: TDCLDevice;
  function GetParameterDevice(const Device: TDCLDevice): Size_t;
  begin
    Result := Device.MaxParameterSize;
  end;

var
  i: Integer;
  MaxValue: Size_t;
  MaxValuePos: CL_uint;
begin
  if FDeviceCount = 0 then
  begin
    Result := nil;
    Exit;
  end;
  MaxValue := GetParameterDevice(FDevices[0]);
  MaxValuePos := 0;
  for i := 1 to FDeviceCount - 1 do
  begin
    if GetParameterDevice(FDevices[i]) > MaxValue then
    begin
      MaxValue := GetParameterDevice(FDevices[i]);
      MaxValuePos := i;
    end;
  end;
  Result := FDevices[MaxValuePos];
end;

function TDCLPlatform.GetDeviceWithMaxReadImageArgs: TDCLDevice;
  function GetParameterDevice(const Device: TDCLDevice): CL_uint;
  begin
    Result := Device.MaxReadImageArgs;
  end;

var
  i: Integer;
  MaxValue: CL_uint;
  MaxValuePos: CL_uint;
begin
  if FDeviceCount = 0 then
  begin
    Result := nil;
    Exit;
  end;
  MaxValue := GetParameterDevice(FDevices[0]);
  MaxValuePos := 0;
  for i := 1 to FDeviceCount - 1 do
  begin
    if GetParameterDevice(FDevices[i]) > MaxValue then
    begin
      MaxValue := GetParameterDevice(FDevices[i]);
      MaxValuePos := i;
    end;
  end;
  Result := FDevices[MaxValuePos];
end;

function TDCLPlatform.GetDeviceWithMaxSamplers: TDCLDevice;
  function GetParameterDevice(const Device: TDCLDevice): CL_uint;
  begin
    Result := Device.MaxSamplers;
  end;

var
  i: Integer;
  MaxValue: CL_uint;
  MaxValuePos: CL_uint;
begin
  if FDeviceCount = 0 then
  begin
    Result := nil;
    Exit;
  end;
  MaxValue := GetParameterDevice(FDevices[0]);
  MaxValuePos := 0;
  for i := 1 to FDeviceCount - 1 do
  begin
    if GetParameterDevice(FDevices[i]) > MaxValue then
    begin
      MaxValue := GetParameterDevice(FDevices[i]);
      MaxValuePos := i;
    end;
  end;
  Result := FDevices[MaxValuePos];
end;

function TDCLPlatform.GetDeviceWithMaxWorkGroupSize: TDCLDevice;
  function GetParameterDevice(const Device: TDCLDevice): Size_t;
  begin
    Result := Device.MaxWorkGroupSize;
  end;

var
  i: Integer;
  MaxValue: Size_t;
  MaxValuePos: CL_uint;
begin
  if FDeviceCount = 0 then
  begin
    Result := nil;
    Exit;
  end;
  MaxValue := GetParameterDevice(FDevices[0]);
  MaxValuePos := 0;
  for i := 1 to FDeviceCount - 1 do
  begin
    if GetParameterDevice(FDevices[i]) > MaxValue then
    begin
      MaxValue := GetParameterDevice(FDevices[i]);
      MaxValuePos := i;
    end;
  end;
  Result := FDevices[MaxValuePos];
end;

function TDCLPlatform.GetDeviceWithMaxWorkItemDimensions: TDCLDevice;
  function GetParameterDevice(const Device: TDCLDevice): CL_uint;
  begin
    Result := Device.MaxWorkItemDimensions;
  end;

var
  i: Integer;
  MaxValue: CL_uint;
  MaxValuePos: CL_uint;
begin
  if FDeviceCount = 0 then
  begin
    Result := nil;
    Exit;
  end;
  MaxValue := GetParameterDevice(FDevices[0]);
  MaxValuePos := 0;
  for i := 1 to FDeviceCount - 1 do
  begin
    if GetParameterDevice(FDevices[i]) > MaxValue then
    begin
      MaxValue := GetParameterDevice(FDevices[i]);
      MaxValuePos := i;
    end;
  end;
  Result := FDevices[MaxValuePos];
end;

function TDCLPlatform.GetDeviceWithMaxWriteImageArgs: TDCLDevice;
  function GetParameterDevice(const Device: TDCLDevice): CL_uint;
  begin
    Result := Device.MaxWriteImageArgs;
  end;

var
  i: Integer;
  MaxValue: CL_uint;
  MaxValuePos: CL_uint;
begin
  if FDeviceCount = 0 then
  begin
    Result := nil;
    Exit;
  end;
  MaxValue := GetParameterDevice(FDevices[0]);
  MaxValuePos := 0;
  for i := 1 to FDeviceCount - 1 do
  begin
    if GetParameterDevice(FDevices[i]) > MaxValue then
    begin
      MaxValue := GetParameterDevice(FDevices[i]);
      MaxValuePos := i;
    end;
  end;
  Result := FDevices[MaxValuePos];
end;

function TDCLPlatform.GetExtensions(Index: Size_t): AnsiString;
begin
  if Index < FExtensionsCount then
    Result := FExtensions[Index]
  else
    Result := '';
end;

function TDCLPlatform.IsPresentExtension(const ExtensionName
  : AnsiString): Boolean;
var
  i: Integer;
  UppName: AnsiString;
begin
  Result := False;
  UppName := UpperCase(ExtensionName);
  for i := 0 to High(FExtensions) do
  begin
    if FExtensions[i] = UppName then
    begin
      Result := True;
      Break;
    end;
  end;
end;

{ TDCLDevice }

constructor TDCLDevice.Create(Device_Id: PCL_device_id);
(*
  need to add
  CL_DEVICE_TYPE
  CL_DEVICE_MAX_WORK_ITEM_SIZES
  CL_DEVICE_SINGLE_FP_CONFIG
  CL_DEVICE_GLOBAL_MEM_CACHE_TYPE
  CL_DEVICE_GLOBAL_MEM_CACHE_TYPE
  CL_DEVICE_EXECUTION_CAPABILITIES
  CL_DEVICE_QUEUE_PROPERTIES
*)

var
  Size: Size_t;
  device_type: CL_device_type;
  b_bool: CL_bool;

  i, current, previous: Integer;
begin
  inherited Create();
  FDevice_id := Device_Id;

  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_NAME, 0, nil, Size);
  SetLength(FName, Size);
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_NAME, Size, @FName[1], Size);
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_VENDOR, 0, nil, Size);
  SetLength(FVendor, Size);
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_VENDOR, Size,
    @FVendor[1], Size);
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_VERSION, 0, nil, Size);
  SetLength(FVersion, Size);
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_VERSION, Size,
    @FVersion[1], Size);
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_PROFILE, 0, nil, Size);
  SetLength(FProfile, Size);
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_PROFILE, Size,
    @FProfile[1], Size);
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_TYPE_INFO,
    SizeOf(device_type), @device_type, Size);
  if (device_type and CL_DEVICE_TYPE_CPU) <> 0 then
    FIsCPU := True;
  if (device_type and CL_DEVICE_TYPE_GPU) <> 0 then
    FIsGPU := True;
  if (device_type and CL_DEVICE_TYPE_ACCELERATOR) <> 0 then
    FIsAccelerator := True;
  if (device_type and CL_DEVICE_TYPE_DEFAULT) <> 0 then
    FIsDefault := True;

  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_PREFERRED_VECTOR_WIDTH_CHAR,
    SizeOf(FMaxWorkGroupSize), @FMaxWorkGroupSize, Size);
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_PREFERRED_VECTOR_WIDTH_CHAR,
    SizeOf(FNativeVectorPreferredChar), @FNativeVectorPreferredChar, Size);
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_PREFERRED_VECTOR_WIDTH_SHORT,
    SizeOf(FNativeVectorPreferredShort), @FNativeVectorPreferredShort, Size);
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_PREFERRED_VECTOR_WIDTH_INT,
    SizeOf(FNativeVectorPreferredInt), @FNativeVectorPreferredInt, Size);
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_PREFERRED_VECTOR_WIDTH_LONG,
    SizeOf(FNativeVectorPreferredLong), @FNativeVectorPreferredLong, Size);
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_PREFERRED_VECTOR_WIDTH_FLOAT,
    SizeOf(FNativeVectorPreferredFloat), @FNativeVectorPreferredFloat, Size);
  FStatus := clGetDeviceInfo(FDevice_id,
    CL_DEVICE_PREFERRED_VECTOR_WIDTH_DOUBLE,
    SizeOf(FNativeVectorPreferredDouble), @FNativeVectorPreferredDouble, Size);
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_PREFERRED_VECTOR_WIDTH_HALF,
    SizeOf(FNativeVectorPreferredHalf), @FNativeVectorPreferredHalf, Size);
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_NATIVE_VECTOR_WIDTH_CHAR,
    SizeOf(FNativeVectorWidthChar), @FNativeVectorWidthChar, Size);
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_NATIVE_VECTOR_WIDTH_SHORT,
    SizeOf(FNativeVectorWidthShort), @FNativeVectorWidthShort, Size);
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_NATIVE_VECTOR_WIDTH_INT,
    SizeOf(FNativeVectorWidthInt), @FNativeVectorWidthInt, Size);
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_NATIVE_VECTOR_WIDTH_LONG,
    SizeOf(FNativeVectorWidthLong), @FNativeVectorWidthLong, Size);
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_NATIVE_VECTOR_WIDTH_FLOAT,
    SizeOf(FNativeVectorWidthFloat), @FNativeVectorWidthFloat, Size);
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_NATIVE_VECTOR_WIDTH_DOUBLE,
    SizeOf(FNativeVectorWidthDouble), @FNativeVectorWidthDouble, Size);
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_NATIVE_VECTOR_WIDTH_HALF,
    SizeOf(FNativeVectorWidthHalf), @FNativeVectorWidthHalf, Size);
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_MAX_CLOCK_FREQUENCY,
    SizeOf(FMaxClockFrequency), @FMaxClockFrequency, Size);
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_ADDRESS_BITS,
    SizeOf(FAddressBits), @FAddressBits, Size);
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_MAX_MEM_ALLOC_SIZE,
    SizeOf(FMaxMemAllocSize), @FMaxMemAllocSize, Size);
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_IMAGE_SUPPORT,
    SizeOf(b_bool), @b_bool, Size);
  if b_bool <> 0 then
    FIsImageSupport := True;

  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_MAX_READ_IMAGE_ARGS,
    SizeOf(FMaxReadImageArgs), @FMaxReadImageArgs, Size);
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_MAX_WRITE_IMAGE_ARGS,
    SizeOf(FMaxWriteImageArgs), @FMaxWriteImageArgs, Size);
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_IMAGE2D_MAX_WIDTH,
    SizeOf(FImage2DMaxWidth), @FImage2DMaxWidth, Size);
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_IMAGE2D_MAX_HEIGHT,
    SizeOf(FImage2DMaxHeight), @FImage2DMaxHeight, Size);
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_IMAGE3D_MAX_WIDTH,
    SizeOf(FImage3DMaxWidth), @FImage3DMaxWidth, Size);
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_IMAGE3D_MAX_HEIGHT,
    SizeOf(FImage3DMaxHeight), @FImage3DMaxHeight, Size);
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_IMAGE3D_MAX_DEPTH,
    SizeOf(FImage3DMaxDepth), @FImage3DMaxDepth, Size);
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_MAX_SAMPLERS,
    SizeOf(FMaxSamplers), @FMaxSamplers, Size);
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_MAX_PARAMETER_SIZE,
    SizeOf(FMaxParameterSize), @FMaxParameterSize, Size);
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_MEM_BASE_ADDR_ALIGN,
    SizeOf(FMemBaseAddrAlign), @FMemBaseAddrAlign, Size);
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_MIN_DATA_TYPE_ALIGN_SIZE,
    SizeOf(FMinDataTypeAlignSize), @FMinDataTypeAlignSize, Size);
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_GLOBAL_MEM_CACHELINE_SIZE,
    SizeOf(FGlobalMemCacheLineSize), @FGlobalMemCacheLineSize, Size);
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_GLOBAL_MEM_CACHE_SIZE,
    SizeOf(FGlobalMemCacheSize), @FGlobalMemCacheSize, Size);
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_GLOBAL_MEM_SIZE,
    SizeOf(FGlobalMemSize), @FGlobalMemSize, Size);
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE,
    SizeOf(FMaxConstantBufferSize), @FMaxConstantBufferSize, Size);
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_MAX_CONSTANT_ARGS,
    SizeOf(FMaxConstantArgs), @FMaxConstantArgs, Size);
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_LOCAL_MEM_SIZE,
    SizeOf(FLocalMemSize), @FLocalMemSize, Size);
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_ENDIAN_LITTLE,
    SizeOf(b_bool), @b_bool, Size);
  if b_bool <> 0 then
    FIsErrorCorrectionSupport := True;

  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_ENDIAN_LITTLE,
    SizeOf(b_bool), @b_bool, Size);
  if b_bool <> 0 then
    FIsHostUnifiedMemory := True;

  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_PROFILING_TIMER_RESOLUTION,
    SizeOf(FProfilingTimerResolution), @FProfilingTimerResolution, Size);
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_ENDIAN_LITTLE,
    SizeOf(b_bool), @b_bool, Size);
  if b_bool <> 0 then
    FIsEndianLittle := True;

  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_AVAILABLE, SizeOf(b_bool),
    @b_bool, Size);
  if b_bool <> 0 then
    FIsAvailable := True;

  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_COMPILER_AVAILABLE,
    SizeOf(b_bool), @b_bool, Size);
  if b_bool <> 0 then
    FIsCompilerAvailable := True;

  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_VENDOR_ID, SizeOf(FVendorId),
    @FVendorId, Size);
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_MAX_COMPUTE_UNITS,
    SizeOf(FMaxComputeUnits), @FMaxComputeUnits, Size);
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS,
    SizeOf(FMaxWorkItemDimensions), @FMaxWorkItemDimensions, Size);
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_EXTENSIONS, 0, nil, Size);
  SetLength(FExtensionsString, Size);
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_EXTENSIONS, Size,
    @FExtensionsString[1], Size);
  FExtensionsCount := 0;
  i := 1;
  while (i <= Length(FExtensionsString)) do
  begin
    if ((FExtensionsString[i] = ' ') or (FExtensionsString[i] = #0)) then
    begin
      if (i > 1) then
      begin
        if ((FExtensionsString[i - 1] <> ' ') and
          (FExtensionsString[i - 1] <> #0)) then
        begin
          Inc(FExtensionsCount);
        end;
      end
      else
        Inc(FExtensionsCount);
    end;
    Inc(i);
  end;
  SetLength(FExtensions, FExtensionsCount);
  previous := 1;
  current := 1;
  i := 0;
  while (current <= Length(FExtensionsString)) do
  begin
    if ((FExtensionsString[current] = AnsiString(' ')) or
      (FExtensionsString[current] = #0)) then
    begin
      if (current > previous) then
        FExtensions[i] := UpperCase(Copy(FExtensionsString, previous,
          current - previous - 1));
      previous := current + 1;
      Inc(i);
    end;
    Inc(current);
  end;

  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_OPENCL_C_VERSION, 0,
    nil, Size);
  SetLength(FOpenCLCVersion, Size);
  FStatus := clGetDeviceInfo(FDevice_id, CL_DEVICE_OPENCL_C_VERSION, Size,
    @FOpenCLCVersion[1], Size);
  FStatus := clGetDeviceInfo(FDevice_id, CL_DRIVER_VERSION, 0, nil, Size);
  SetLength(FDriverVersion, Size);
  FStatus := clGetDeviceInfo(FDevice_id, CL_DRIVER_VERSION, Size,
    @FDriverVersion[1], Size);
  FContext := TDCLContext.Create(FDevice_id);
end;

function TDCLDevice.CreateBuffer(const Size: Size_t; const Data: Pointer;
  const Flags: TDCLMemFlagsSet): TDCLBuffer;
begin
  Result := TDCLBuffer.Create(Context.FContext, Flags, Size, Data);
end;

function TDCLDevice.CreateCommandQueue(const Properties
  : TDCLCommandQueuePropertiesSet): TDCLCommandQueue;
begin
  Result := TDCLCommandQueue.Create(Device_Id, Context.FContext, Properties);
end;

function TDCLDevice.CreateContext: TDCLContext;
begin
  Result := TDCLContext.Create(FDevice_id);
end;

function TDCLDevice.CreateProgram(const Source: PPChar; const Options: PPChar)
  : TDCLProgram;
begin
  Result := TDCLProgram.Create(FContext.FContext, Source, Options);
end;

function TDCLDevice.CreateImage2D(const Format: PCL_image_format;
  const Width, Height, RowPitch: Size_t; const Data: Pointer;
  const Flags: TDCLMemFlagsSet): TDCLImage2D;
begin
  Result := TDCLImage2D.Create(Context.FContext, Flags, Format, Width, Height,
    RowPitch, Data);
end;

function TDCLDevice.CreateProgram(const FileName: String; const Options: PPChar)
  : TDCLProgram;
var
  F: TextFile;
  Source: String;
  buf: String;
begin
  AssignFile(F, FileName);
  Reset(F);
  Source := '';
  while not(EOF(F)) do
  begin
    Readln(F, buf);
    Source := Source + buf + #10 + #13;
  end;
  CloseFile(F);
  Result := CreateProgram(@PString(Source), Options);
end;

procedure TDCLDevice.Free;
begin
  FContext.Free();
  SetLength(FExtensions, 0);
  FExtensionsString := '';
  inherited Free();
end;

function TDCLDevice.GetExtensions(const Index: Size_t): AnsiString;
begin
  if Index < FExtensionsCount then
    Result := FExtensions[Index]
  else
    Result := '';
end;

function TDCLDevice.IsPresentExtension(const ExtensionName: AnsiString)
  : Boolean;
var
  i: Integer;
  UppName: AnsiString;
begin
  Result := False;
  UppName := UpperCase(ExtensionName);
  for i := 0 to High(FExtensions) do
  begin
    if FExtensions[i] = UppName then
    begin
      Result := True;
      Break;
    end;
  end;
end;

{ TDCLContext }

constructor TDCLContext.Create(Device_Id: PCL_device_id);
(*
  CL_CONTEXT_REFERENCE_COUNT
  CL_CONTEXT_DEVICES
  CL_CONTEXT_PROPERTIES
*)
var
  Size: Size_t;
begin
  inherited Create();
  FContext := clCreateContext(nil, 1, @Device_Id, nil, nil, FStatus);
  FStatus := clGetContextInfo(FContext, CL_CONTEXT_NUM_DEVICES,
    SizeOf(FNumDevices), @FNumDevices, Size);
end;

{ TDCLQueue }

constructor TDCLCommandQueue.Create(const Device_Id: PCL_device_id;
  const Context: PCL_context; const Properties: TDCLCommandQueuePropertiesSet);
var
  props: CL_command_queue_properties;
begin
  props := 0;
  if cqpOutOfOrderExecModeEnable in Properties then
    props := props or CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE;
  FCommandQueue := clCreateCommandQueue(Context, Device_Id, props, FStatus);
  FProperties := Properties;
end;

procedure TDCLContext.Free;
begin
  FStatus := clReleaseContext(FContext);
  inherited Free();
end;

{ TDCLBuffer }

constructor TDCLBuffer.Create(const Context: PCL_context;
  const Flags: TDCLMemFlagsSet; const Size: Size_t; const Data: Pointer = nil);
var
  fgs: CL_mem_flags;
begin
  inherited Create();
  fgs := 0;
  if mfReadWrite in Flags then
    fgs := fgs or CL_MEM_READ_WRITE;
  if mfWriteOnly in Flags then
    fgs := fgs or CL_MEM_WRITE_ONLY;
  if mfReadOnly in Flags then
    fgs := fgs or CL_MEM_READ_ONLY;
  if mfUseHostPtr in Flags then
    fgs := fgs or CL_MEM_USE_HOST_PTR;
  if mfAllocHostPtr in Flags then
    fgs := fgs or CL_MEM_ALLOC_HOST_PTR;
  if mfCopyHostPtr in Flags then
    fgs := fgs or CL_MEM_COPY_HOST_PTR;
  FMem := clCreateBuffer(Context, fgs, Size, Data, FStatus);
  FSize := Size;
end;

procedure TDCLBuffer.Free;
begin
  FStatus := clReleaseMemObject(FMem);
  inherited Free;
end;

procedure TDCLCommandQueue.Execute(const Kernel: TDCLKernel;
  const Size: Size_t);
begin
  FStatus := clEnqueueNDRangeKernel(FCommandQueue, Kernel.FKernel, 1, nil,
    @Size, nil, 0, nil, nil);
  FStatus := clFinish(FCommandQueue);
end;

procedure TDCLCommandQueue.Execute(const Kernel: TDCLKernel;
  // const Device: PCL_device_id;
  const Size: array of Size_t);
// var
// kernel2DWorkGroupSize: Size_t;
begin
  // FStatus := clGetKernelWorkGroupInfo(Kernel.FKernel, Device, CL_KERNEL_WORK_GROUP_SIZE, SizeOf(Size_t), @kernel2DWorkGroupSize, nil);
  FStatus := clEnqueueNDRangeKernel(FCommandQueue, Kernel.FKernel, Length(Size),
    nil, @Size[0], nil, 0, nil, nil);
  FStatus := clFinish(FCommandQueue);
end;

procedure TDCLCommandQueue.Free;
begin
  FStatus := clReleaseCommandQueue(FCommandQueue);
  inherited Free();
end;

{ TDCLProgram }

constructor TDCLProgram.Create(const Context: PCL_context; const Source: PPChar;
  const Options: PPChar);
var
  Size: Size_t;
  // FBinaries: Array of Char;
begin
  FProgram := clCreateProgramWithSource(Context, 1, Source, nil, FStatus);
  FStatus := clBuildProgram(FProgram, 0, nil, Options^, nil, nil);
  FStatus := clGetProgramInfo(FProgram, CL_PROGRAM_SOURCE, 0, nil, Size);
  FSource := GetMemory(Size);
  FStatus := clGetProgramInfo(FProgram, CL_PROGRAM_SOURCE, Size, FSource, Size);
  FStatus := clGetProgramInfo(FProgram, CL_PROGRAM_BINARY_SIZES, 0, nil,
    FBinarySizesCount);
  SetLength(FBinarySizes, FBinarySizesCount);
  FStatus := clGetProgramInfo(FProgram, CL_PROGRAM_BINARY_SIZES,
    SizeOf(FBinarySizes), @FBinarySizes[0], FBinarySizesCount);
  (* //Not yet
    FStatus := clGetProgramInfo(FProgram,CL_PROGRAM_BINARIES,0,nil,@Size);
    SetLength(FBinaries,Size);
    FStatus := clGetProgramInfo(FProgram,CL_PROGRAM_BINARIES,Size,@FBinaries[0],nil);
    Writeln(String(FBinaries));
  *)

end;

function TDCLProgram.CreateKernel(const KernelName: PPChar): TDCLKernel;
begin
  Result := TDCLKernel.Create(FProgram, KernelName);
end;

procedure TDCLProgram.Free;
begin
  FStatus := clReleaseProgram(FProgram);
  FSource := '';
  SetLength(FBinarySizes, 0);
  inherited Free;
end;

function TDCLProgram.GetBinarySizes(const Index: Size_t): Size_t;
begin
  if (Index < FBinarySizesCount) then
    Result := FBinarySizes[Index]
  else
    Result := 0;
end;

{ TDCLKernel }

constructor TDCLKernel.Create(const Program_: PCL_program;
  const KernelName: PPChar);
begin
  FKernel := clCreateKernel(Program_, KernelName^, FStatus);
end;

procedure TDCLKernel.Free;
begin
  FStatus := clReleaseKernel(FKernel);
  inherited Free();
end;

function TDCLKernel.GetFunctionName: AnsiString;
var
  Size: Size_t;
  Buffer: Array of AnsiChar;
begin
  FStatus := clGetKernelInfo(FKernel, CL_KERNEL_FUNCTION_NAME, 0, nil, Size);
  SetLength(Buffer, Size);
  FStatus := clGetKernelInfo(FKernel, CL_KERNEL_FUNCTION_NAME, Size,
    @Buffer[0], Size);
  Result := AnsiString(Buffer);
  SetLength(Buffer, 0);
end;

function TDCLKernel.GetNumArgs: CL_uint;
var
  Size: Size_t;
begin
  FStatus := clGetKernelInfo(FKernel, CL_KERNEL_NUM_ARGS, SizeOf(Result),
    @Result, Size);
end;

procedure TDCLKernel.SetArg(const Index: CL_uint; const Size: Size_t;
  const Value: Pointer);
begin
  FStatus := clSetKernelArg(FKernel, Index, Size, Value);
end;

procedure TDCLKernel.SetArg(const Index: CL_uint; const Value: TDCLBuffer);
begin
  SetArg(Index, SizeOf(@Value.FMem), @Value.FMem);
end;

procedure TDCLKernel.SetArg(const Index: CL_uint; const Value: TDCLImage2D);
begin
  SetArg(Index, SizeOf(@Value.FMem), @Value.FMem);
end;

procedure TDCLCommandQueue.ReadBuffer(const Buffer: TDCLBuffer;
  const Size: Size_t; const Data: Pointer);
begin
  FStatus := clEnqueueReadBuffer(FCommandQueue, Buffer.FMem, CL_TRUE, 0, Size,
    Data, 0, nil, nil);
  clFinish(FCommandQueue);
end;

procedure TDCLCommandQueue.ReadImage2D(const Image: TDCLImage2D;
  const Width, Height: Size_t; const Data: Pointer);
var
  origin, region: Array [0 .. 2] of Size_t;
begin
  ZeroMemory(@origin, SizeOf(origin));
  region[0] := Width;
  region[1] := Height;
  region[2] := 1; // Image 2D
  FStatus := clEnqueueReadImage(FCommandQueue, Image.FMem, CL_TRUE, @origin,
    @region, 0, 0, Data, 0, nil, nil);
  FStatus := clFinish(FCommandQueue);
end;

procedure TDCLCommandQueue.WriteImage2D(const Image: TDCLImage2D;
  const Width, Height: Size_t; const Data: Pointer);
var
  origin, region: Array [0 .. 2] of Size_t;
begin
  ZeroMemory(@origin, SizeOf(origin));
  region[0] := Width;
  region[1] := Height;
  region[2] := 1; // Image 2D
  FStatus := clEnqueueWriteImage(FCommandQueue, Image.FMem, CL_TRUE, @origin,
    @region, 0, 0, Data, 0, nil, nil);
  FStatus := clFinish(FCommandQueue);
end;

procedure TDCLCommandQueue.WriteBuffer(const Buffer: TDCLBuffer;
  const Size: Size_t; const Data: Pointer);
begin
  FStatus := clEnqueueWriteBuffer(FCommandQueue, Buffer.FMem, CL_TRUE, 0, Size,
    Data, 0, nil, nil);
end;

{ TDCLImage2D }

constructor TDCLImage2D.Create(const Context: PCL_context;
  const Flags: TDCLMemFlagsSet; const Format: PCL_image_format;
  const Width, Height, RowPitch: Size_t; const Data: Pointer);
var
  fgs: CL_mem_flags;
begin
  inherited Create();
  fgs := 0;
  if mfReadWrite in Flags then
    fgs := fgs or CL_MEM_READ_WRITE;
  if mfWriteOnly in Flags then
    fgs := fgs or CL_MEM_WRITE_ONLY;
  if mfReadOnly in Flags then
    fgs := fgs or CL_MEM_READ_ONLY;
  if mfUseHostPtr in Flags then
    fgs := fgs or CL_MEM_USE_HOST_PTR;
  if mfAllocHostPtr in Flags then
    fgs := fgs or CL_MEM_ALLOC_HOST_PTR;
  if mfCopyHostPtr in Flags then
    fgs := fgs or CL_MEM_COPY_HOST_PTR;
  FFormat := Format^;
  FMem := clCreateImage2D(Context, fgs, @FFormat, Width, Height, RowPitch,
    Data, FStatus);
end;

procedure TDCLImage2D.Free;
begin
  FStatus := clReleaseMemObject(FMem);
  inherited Free();
end;

end.
