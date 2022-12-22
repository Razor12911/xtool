unit PrecompDLL;

interface

uses
  Utils,
  UIMain,
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
    DepthInfo: PDepthInfo)cdecl;

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
  DLLList: TStringDynArray;
  CodecDLL: TArray<TDLLStruct>;

procedure AddStream(Instance: Integer; Info: LStrInfo1; Codec: PChar;
  DepthInfo: PDepthInfo)cdecl;
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
  CodecAdd[Instance](Instance, @SI, Codec, DepthInfo)
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
  I: Integer;
begin
  Result := False;
  for I := Low(CodecDLL) to High(CodecDLL) do
  begin
    if IndexText(Funcs^.GetCodec(Command, 0, False), CodecDLL[I].Names) >= 0
    then
    begin
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
  NamesList: TStrings);
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
  // NOTE: our policy in this procedure is to exit upon any failure and return an empty list

  NamesList.Clear;

  FileHandle := CreateFile(PChar(ImageName), GENERIC_READ, FILE_SHARE_READ, nil,
    OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if FileHandle = INVALID_HANDLE_VALUE then
  begin
    exit;
  end;
  try
    ImageHandle := CreateFileMapping(FileHandle, nil, PAGE_READONLY, 0, 0, nil);
    if ImageHandle = 0 then
    begin
      exit;
    end;
    try
      ImagePointer := MapViewOfFile(ImageHandle, FILE_MAP_READ, 0, 0, 0);
      if not Assigned(ImagePointer) then
      begin
        exit;
      end;

      try
        Header := ImageNtHeader(ImagePointer);
        if not Assigned(Header) then
        begin
          exit;
        end;
        if Header.Signature <> $00004550 then
        begin // "PE\0\0" as a DWORD.
          exit;
        end;

        ExportTable := ImageRvaToVa(Header, ImagePointer,
          Header.OptionalHeader.DataDirectory[0].VirtualAddress, nil);
        if not Assigned(ExportTable) then
        begin
          exit;
        end;

        NamesPointer := ImageRvaToVa(Header, ImagePointer,
          Cardinal(ExportTable.AddressOfNames), nil);
        if not Assigned(NamesPointer) then
        begin
          exit;
        end;
        Names := ImageRvaToVa(Header, ImagePointer,
          Cardinal(NamesPointer^), nil);
        if not Assigned(Names) then
        begin
          exit;
        end;

        NamesDataLeft := Header.OptionalHeader.DataDirectory[0].Size;
        for I := 0 to ExportTable.NumberOfNames - 1 do
        begin
          NamesList.Add(Names);
          // Locate the next name
          while (Names^ <> chr(0)) and (NamesDataLeft > 0) do
          begin
            Inc(Names);
            dec(NamesDataLeft);
          end;
          Inc(Names);
        end;
      finally
        UnmapViewOfFile(ImagePointer);
        // Ignore error as there is not much we could do.
      end;
    finally
      CloseHandle(ImageHandle);
    end;
  finally
    CloseHandle(FileHandle);
  end;
end;

var
  I, J: Integer;
  S: String;
  FuncList: TStringList;
  DLLStruct: PDLLStruct;
  DLLHandle: THandle;

initialization

DLLList := TDirectory.GetFiles(ExtractFilePath(Utils.GetModuleName), '*.dll',
  TSearchOption.soTopDirectoryOnly);
FuncList := TStringList.Create;
for I := Low(DLLList) to High(DLLList) do
begin
  ImageExportedFunctionNames(DLLList[I], FuncList);
  if (FuncList.IndexOf('PrecompInit') >= 0) and
    (FuncList.IndexOf('PrecompCodec') >= 0) then
  begin
    New(DLLStruct);
    DLLHandle := LoadLibrary(PChar(DLLList[I]));
    if DLLHandle >= 32 then
    begin
      @DLLStruct^.Init := GetProcAddress(DLLHandle, 'PrecompInit');
      Assert(@DLLStruct^.Init <> nil);
      @DLLStruct^.Free := GetProcAddress(DLLHandle, 'PrecompFree');
      @DLLStruct^.Codec := GetProcAddress(DLLHandle, 'PrecompCodec');
      @DLLStruct^.Scan1 := GetProcAddress(DLLHandle, 'PrecompScan1');
      @DLLStruct^.Scan2 := GetProcAddress(DLLHandle, 'PrecompScan2');
      @DLLStruct^.Process := GetProcAddress(DLLHandle, 'PrecompProcess');
      @DLLStruct^.Restore := GetProcAddress(DLLHandle, 'PrecompRestore');
      if UIMain.DLLLoaded then
        XTLAddplugin(ChangeFileExt(ExtractFileName(DLLList[I]), ''),
          PLUGIN_LIBRARY);
      Insert(DLLStruct^, CodecDLL, Length(CodecDLL));
      J := 0;
      while Assigned(CodecDLL[Pred(Length(CodecDLL))].Codec(J)) do
      begin
        S := String(CodecDLL[Pred(Length(CodecDLL))].Codec(J));
        Insert(S, CodecDLL[Pred(Length(CodecDLL))].Names,
          Length(CodecDLL[Pred(Length(CodecDLL))].Names));
        Insert(S, Codec.Names, Length(Codec.Names));
        if UIMain.DLLLoaded then
          XTLAddCodec(S);
        Inc(J);
      end;
      if J = 0 then
      begin
        Insert(ChangeFileExt(ExtractFileName(DLLList[I]), ''),
          CodecDLL[Pred(Length(CodecDLL))].Names,
          Length(CodecDLL[Pred(Length(CodecDLL))].Names));
        Insert(ChangeFileExt(ExtractFileName(DLLList[I]), ''), Codec.Names,
          Length(Codec.Names));
        if UIMain.DLLLoaded then
          XTLAddCodec(ChangeFileExt(ExtractFileName(DLLList[I]), ''));
      end;
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

end.
