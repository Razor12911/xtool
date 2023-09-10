unit PrecompDLL;

interface

uses
  InitCode,
  Utils, LibImport,
  PrecompUtils,
  WinAPI.Windows,
  System.SysUtils, System.Classes, System.StrUtils,
  System.Types, System.Math, System.IOUtils;

var
  Codec: TPrecompressor;

implementation

uses
  PrecompMain;

type
  LStrInfo1 = ^TStrInfo1;

  TStrInfo1 = packed record
    Position: Int64;
    OldSize, NewSize: Integer;
    Resource: Integer;
    Option: Word;
  end;

  LStrInfo2 = ^TStrInfo2;

  TStrInfo2 = packed record
    OldSize, NewSize: Integer;
    Resource: Integer;
    Option: Word;
  end;

  LStrInfo3 = ^TStrInfo3;

  TStrInfo3 = packed record
    OldSize, NewSize, ExtSize: Integer;
    Resource: Integer;
    Option: Word;
  end;

  LPrecompFuncs = PPrecompFuncs;

  TPrecompFuncs = _PrecompFuncs;

  TPrecompOutput = procedure(Instance: Integer; const Buffer: Pointer;
    Size: Integer)cdecl;
  TPrecompAdd = procedure(Instance: Integer; Info: LStrInfo1; Codec: PChar;
    Reserved: Pointer)cdecl;

  TPrecompInit = function(Command: PChar; Count: Integer; Funcs: LPrecompFuncs)
    : Boolean cdecl;
  TPrecompFree = procedure(Funcs: LPrecompFuncs)cdecl;
  TPrecompCodec = function(Index: Integer): PChar cdecl;
  TPrecompScan1 = procedure(Instance: Integer; Input: Pointer;
    Size, SizeEx: NativeInt; Output: TPrecompOutput; Add: TPrecompAdd;
    Funcs: LPrecompFuncs)cdecl;
  TPrecompScan2 = function(Instance: Integer; Input: Pointer; Size: NativeInt;
    StreamInfo: LStrInfo2; Output: TPrecompOutput; Funcs: LPrecompFuncs)
    : Boolean cdecl;
  TPrecompProcess = function(Instance: Integer; OldInput, NewInput: Pointer;
    StreamInfo: LStrInfo2; Output: TPrecompOutput; Funcs: LPrecompFuncs)
    : Boolean cdecl;
  TPrecompRestore = function(Instance: Integer; Input, InputExt: Pointer;
    StreamInfo: TStrInfo3; Output: TPrecompOutput; Funcs: LPrecompFuncs)
    : Boolean cdecl;

type
  PDLLStruct = ^TDLLStruct;

  TDLLStruct = record
    Lib: TLibImport;
    Names: TArray<String>;
    Init: TPrecompInit;
    Free: TPrecompFree;
    Codec: TPrecompCodec;
    Scan1: TPrecompScan1;
    Scan2: TPrecompScan2;
    Process: TPrecompProcess;
    Restore: TPrecompRestore;
  end;

var
  CodecIndex: TArray<Integer>;
  CodecAdd: TArray<_PrecompAdd>;

  CodecDLL: TArray<TDLLStruct>;

procedure AddStream(Instance: Integer; Info: LStrInfo1; Codec: PChar;
  Reserved: Pointer)cdecl;
var
  SI: _StrInfo1;
begin
  SI.Position := Info^.Position;
  SI.OldSize := Info^.OldSize;
  SI.NewSize := Info^.NewSize;
  SI.Resource := Info^.Resource;
  if System.Pos(SPrecompSep2, Codec) > 0 then
    SI.Status := TStreamStatus.Predicted
  else
    SI.Status := TStreamStatus.None;
  LongRec(SI.Option).Lo := Info^.Option;
  LongRec(SI.Option).Hi := CodecIndex[Instance];
  CodecAdd[Instance](Instance, @SI, Codec, nil)
end;

function DLLInit(Command: PChar; Count: Integer; Funcs: PPrecompFuncs): Boolean;
var
  I, X: Integer;
  S: String;
  Used: Boolean;
begin
  SetLength(CodecIndex, Count);
  SetLength(CodecAdd, Count);
  for I := High(CodecDLL) downto Low(CodecDLL) do
  begin
    Used := False;
    X := 0;
    while (Used = False) and (Funcs^.GetCodec(Command, X, False) <> '') do
    begin
      Used := IndexText(Funcs^.GetCodec(Command, X, False),
        CodecDLL[I].Names) >= 0;
      Inc(X);
    end;
    for X := 0 to ExternalMethods.Count - 1 do
      if not Used then
        Used := IndexText(ExternalMethods[X], CodecDLL[I].Names) >= 0;
    S := Command;
    if (Used = False) or (CodecDLL[I].Init(PChar(S), Count, @PrecompFunctions)
      = False) then
      Delete(CodecDLL, I, 1);
  end;
  Result := Length(CodecDLL) > 0;
end;

procedure DLLFree(Funcs: PPrecompFuncs);
var
  I: Integer;
begin
  for I := Low(CodecDLL) to High(CodecDLL) do
  begin
    if Assigned(CodecDLL[I].Free) then
      CodecDLL[I].Free(@PrecompFunctions);
  end;
end;

function DLLParse(Command: PChar; Option: PInteger;
  Funcs: PPrecompFuncs): Boolean;
var
  I, J: Integer;
begin
  Result := False;
  for I := Low(CodecDLL) to High(CodecDLL) do
  begin
    J := IndexText(Funcs^.GetCodec(Command, 0, False), CodecDLL[I].Names);
    if J >= 0 then
    begin
      LongRec(Option^).Lo := J;
      LongRec(Option^).Hi := I;
      Result := True;
      break;
    end;
  end;
end;

procedure DLLScan1(Instance, Depth: Integer; Input: PByte;
  Size, SizeEx: NativeInt; Output: _PrecompOutput; Add: _PrecompAdd;
  Funcs: PPrecompFuncs);
var
  I: Integer;
begin
  for I := Low(CodecDLL) to High(CodecDLL) do
  begin
    try
      CodecIndex[Instance] := I;
      CodecAdd[Instance] := Add;
      if Assigned(CodecDLL[I].Scan1) then
        CodecDLL[I].Scan1(Instance, Input, Size, SizeEx, TPrecompOutput(Output),
          @AddStream, @PrecompFunctions);
    except
    end;
  end;
end;

function DLLScan2(Instance, Depth: Integer; Input: Pointer; Size: NativeInt;
  StreamInfo: PStrInfo2; Offset: PInteger; Output: _PrecompOutput;
  Funcs: PPrecompFuncs): Boolean;
var
  SI: TStrInfo2;
begin
  Result := False;
  SI.OldSize := StreamInfo^.OldSize;
  SI.NewSize := StreamInfo^.NewSize;
  SI.Resource := StreamInfo^.Resource;
  SI.Option := LongRec(StreamInfo^.Option).Lo;
  if Assigned(CodecDLL[LongRec(StreamInfo^.Option).Hi].Scan2) then
    Result := CodecDLL[LongRec(StreamInfo^.Option).Hi].Scan2(Instance, Input,
      Size, @SI, TPrecompOutput(Output), @PrecompFunctions);
  if Result then
  begin
    StreamInfo^.OldSize := SI.OldSize;
    StreamInfo^.NewSize := SI.NewSize;
    StreamInfo^.Resource := SI.Resource;
    LongRec(StreamInfo^.Option).Lo := SI.Option;
  end;
end;

function DLLProcess(Instance, Depth: Integer; OldInput, NewInput: Pointer;
  StreamInfo: PStrInfo2; Output: _PrecompOutput; Funcs: PPrecompFuncs): Boolean;
var
  SI: TStrInfo2;
begin
  Result := False;
  SI.OldSize := StreamInfo^.OldSize;
  SI.NewSize := StreamInfo^.NewSize;
  SI.Resource := StreamInfo^.Resource;
  SI.Option := LongRec(StreamInfo^.Option).Lo;
  if Assigned(CodecDLL[LongRec(StreamInfo^.Option).Hi].Process) then
    Result := CodecDLL[LongRec(StreamInfo^.Option).Hi].Process(Instance,
      OldInput, NewInput, @SI, TPrecompOutput(Output), @PrecompFunctions);
  if Result then
  begin
    StreamInfo^.OldSize := SI.OldSize;
    StreamInfo^.NewSize := SI.NewSize;
    StreamInfo^.Resource := SI.Resource;
    LongRec(StreamInfo^.Option).Lo := SI.Option;
  end;
end;

function DLLRestore(Instance, Depth: Integer; Input, InputExt: Pointer;
  StreamInfo: _StrInfo3; Output: _PrecompOutput; Funcs: PPrecompFuncs): Boolean;
var
  SI: TStrInfo3;
begin
  Result := False;
  SI.OldSize := StreamInfo.OldSize;
  SI.NewSize := StreamInfo.NewSize;
  SI.ExtSize := StreamInfo.ExtSize;
  SI.Resource := StreamInfo.Resource;
  SI.Option := LongRec(StreamInfo.Option).Lo;
  if Assigned(CodecDLL[LongRec(StreamInfo.Option).Hi].Restore) then
    Result := CodecDLL[LongRec(StreamInfo.Option).Hi].Restore(Instance, Input,
      InputExt, SI, TPrecompOutput(Output), @PrecompFunctions);
end;

type
  PIMAGE_NT_HEADERS = ^IMAGE_NT_HEADERS;
  PIMAGE_EXPORT_DIRECTORY = ^IMAGE_EXPORT_DIRECTORY;

function ImageNtHeader(Base: Pointer): PIMAGE_NT_HEADERS; stdcall;
  external 'dbghelp.dll';
function ImageRvaToVa(NtHeaders: Pointer; Base: Pointer; Rva: ULONG;
  LastRvaSection: Pointer): Pointer; stdcall; external 'dbghelp.dll';

procedure ImageExportedFunctionNames(const ImageName: string;
  NamesList: TStrings)overload;
var
  I: Integer;
  FileHandle: THandle;
  ImageHandle: THandle;
  ImagePointer: Pointer;
  Header: PIMAGE_NT_HEADERS;
  ExportTable: PIMAGE_EXPORT_DIRECTORY;
  NamesPointer: Pointer;
  Names: PAnsiChar;
  NamesDataLeft: Integer;
begin
  NamesList.Clear;
  FileHandle := CreateFile(PChar(ImageName), GENERIC_READ, FILE_SHARE_READ, nil,
    OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if FileHandle = INVALID_HANDLE_VALUE then
    exit;
  try
    ImageHandle := CreateFileMapping(FileHandle, nil, PAGE_READONLY, 0, 0, nil);
    if ImageHandle = 0 then
      exit;
    try
      ImagePointer := MapViewOfFile(ImageHandle, FILE_MAP_READ, 0, 0, 0);
      if not Assigned(ImagePointer) then
        exit;
      try
        Header := ImageNtHeader(ImagePointer);
        if not Assigned(Header) then
          exit;
        if Header.Signature <> $00004550 then
          exit;
        ExportTable := ImageRvaToVa(Header, ImagePointer,
          Header.OptionalHeader.DataDirectory[0].VirtualAddress, nil);
        if not Assigned(ExportTable) then
          exit;
        NamesPointer := ImageRvaToVa(Header, ImagePointer,
          Cardinal(ExportTable.AddressOfNames), nil);
        if not Assigned(NamesPointer) then
          exit;
        Names := ImageRvaToVa(Header, ImagePointer,
          Cardinal(NamesPointer^), nil);
        if not Assigned(Names) then
          exit;
        NamesDataLeft := Header.OptionalHeader.DataDirectory[0].Size;
        for I := 0 to ExportTable.NumberOfNames - 1 do
        begin
          NamesList.Add(Names);
          while (Names^ <> chr(0)) and (NamesDataLeft > 0) do
          begin
            Inc(Names);
            dec(NamesDataLeft);
          end;
          Inc(Names);
        end;
      finally
        UnmapViewOfFile(ImagePointer);
      end;
    finally
      CloseHandle(ImageHandle);
    end;
  finally
    CloseHandle(FileHandle);
  end;
end;

procedure ImageExportedFunctionNames(const ImageName: TCustomMemoryStream;
  NamesList: TStrings)overload;
var
  I: Integer;
  Header: PIMAGE_NT_HEADERS;
  ExportTable: PIMAGE_EXPORT_DIRECTORY;
  NamesPointer: Pointer;
  Names: PAnsiChar;
  NamesDataLeft: Integer;
begin
  NamesList.Clear;
  Header := ImageNtHeader(ImageName.Memory);
  if not Assigned(Header) then
    exit;
  if Header.Signature <> $00004550 then
    exit;
  ExportTable := ImageRvaToVa(Header, ImageName.Memory,
    Header.OptionalHeader.DataDirectory[0].VirtualAddress, nil);
  if not Assigned(ExportTable) then
    exit;
  NamesPointer := ImageRvaToVa(Header, ImageName.Memory,
    Cardinal(ExportTable.AddressOfNames), nil);
  if not Assigned(NamesPointer) then
    exit;
  Names := ImageRvaToVa(Header, ImageName.Memory, Cardinal(NamesPointer^), nil);
  if not Assigned(Names) then
    exit;
  NamesDataLeft := Header.OptionalHeader.DataDirectory[0].Size;
  for I := 0 to ExportTable.NumberOfNames - 1 do
  begin
    NamesList.Add(Names);
    while (Names^ <> chr(0)) and (NamesDataLeft > 0) do
    begin
      Inc(Names);
      dec(NamesDataLeft);
    end;
    Inc(Names);
  end;
end;

var
  I, J, X: Integer;
  S: String;
  DLLList: TStringDynArray;
  FuncList: TStringList;
  RStream: TResourceStream;

initialization

FuncList := TStringList.Create;
FuncList.Clear;
DLLList := TDirectory.GetFiles(ExpandPath(PluginsPath, True), '*.dll',
  TSearchOption.soTopDirectoryOnly);
for I := Low(DLLList) to High(DLLList) do
begin
  ImageExportedFunctionNames(DLLList[I], FuncList);
  if (FuncList.IndexOf('PrecompInit') >= 0) and
    (FuncList.IndexOf('PrecompCodec') >= 0) then
  begin
    X := Length(CodecDLL);
    SetLength(CodecDLL, Succ(X));
    CodecDLL[X].Lib := TLibImport.Create;
    CodecDLL[X].Lib.LoadLib(PChar(DLLList[I]));
    if CodecDLL[X].Lib.Loaded then
    begin
      CodecDLL[X].Init := CodecDLL[X].Lib.GetProcAddr('PrecompInit');
      Assert(@CodecDLL[X].Init <> nil);
      @CodecDLL[X].Free := CodecDLL[X].Lib.GetProcAddr('PrecompFree');
      @CodecDLL[X].Codec := CodecDLL[X].Lib.GetProcAddr('PrecompCodec');
      @CodecDLL[X].Scan1 := CodecDLL[X].Lib.GetProcAddr('PrecompScan1');
      @CodecDLL[X].Scan2 := CodecDLL[X].Lib.GetProcAddr('PrecompScan2');
      @CodecDLL[X].Process := CodecDLL[X].Lib.GetProcAddr('PrecompProcess');
      @CodecDLL[X].Restore := CodecDLL[X].Lib.GetProcAddr('PrecompRestore');
      if InitCode.UIDLLLoaded then
        XTLAddplugin(ChangeFileExt(ExtractFileName(DLLList[I]), ''),
          PLUGIN_LIBRARY);
      J := 0;
      while Assigned(CodecDLL[X].Codec(J)) do
      begin
        S := String(CodecDLL[X].Codec(J));
        if SameText(ChangeFileExt(ExtractFileName(S), ''),
          ChangeFileExt(ExtractFileName(Utils.GetModuleName), '')) then
          FORCEDMETHOD := True;
        Insert(S, CodecDLL[X].Names, Length(CodecDLL[X].Names));
        Insert(S, Codec.Names, Length(Codec.Names));
        if not SameText(ChangeFileExt(ExtractFileName(S), ''),
          ChangeFileExt(ExtractFileName(Utils.GetModuleName), '')) then
          if InitCode.UIDLLLoaded then
            XTLAddCodec(S);
        Inc(J);
      end;
      if J = 0 then
      begin
        Insert(ChangeFileExt(ExtractFileName(DLLList[I]), ''),
          CodecDLL[X].Names, Length(CodecDLL[X].Names));
        Insert(ChangeFileExt(ExtractFileName(DLLList[I]), ''), Codec.Names,
          Length(Codec.Names));
        if InitCode.UIDLLLoaded then
          XTLAddCodec(ChangeFileExt(ExtractFileName(DLLList[I]), ''));
      end;
    end
    else
    begin
      CodecDLL[X].Lib.Free;
      SetLength(CodecDLL, X);
    end;
  end;
end;

Codec.Initialised := False;
Codec.Init := @DLLInit;
Codec.Free := @DLLFree;
Codec.Parse := @DLLParse;
Codec.Scan1 := @DLLScan1;
Codec.Scan2 := @DLLScan2;
Codec.Process := @DLLProcess;
Codec.Restore := @DLLRestore;

finalization

for I := High(CodecDLL) downto Low(CodecDLL) do
  CodecDLL[I].Lib.Free;

end.
