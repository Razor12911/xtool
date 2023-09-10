unit LibImport;

interface

uses
  MemoryModule,
  WinAPI.Windows,
  System.SysUtils, System.Classes, System.Character, System.RTLConsts;

type
  TLibImport = class
  private
    FIsMemoryLib: Boolean;
    FDLLLoaded: Boolean;
    FImageFileName: String;
    FDLLStream: TCustomMemoryStream;
    FDLLHandle: NativeUInt;
    FImagePtr: Pointer;
    FImageSize: NativeInt;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadLib(ALibrary: String)overload;
    procedure LoadLib(AMemory: Pointer; ASize: NativeInt)overload;
    procedure LoadLib(AStream: TStream; ASize: NativeInt)overload;
    procedure UnloadLib;
    function GetProcAddr(AProcName: PAnsiChar): Pointer;
    property Loaded: Boolean read FDLLLoaded;
    property IsMemory: Boolean read FIsMemoryLib;
    property ImageFileName: String read FImageFileName;
    property ImagePtr: Pointer read FImagePtr;
    property ImageSize: NativeInt read FImageSize;
  end;

procedure InjectLib(Source, Dest: String);

implementation

function ResourceExists(ResName: String): Boolean;
begin
  Result := FindResourceEx(HInstance, RT_RCDATA, PWideChar(ResName), 0) <> 0;
end;

function FileToResourceName(FileName: String): String;
var
  I: Integer;
begin
  Result := ExtractFileName(FileName).ToUpper;
  for I := 1 to Result.Length do
    if not Result[I].IsLetterOrDigit then
      Result[I] := '_';
end;

procedure UpdateFileResource(Source, Dest, ResName: string);
var
  Stream: TFileStream;
  hDestRes: THandle;
  lpData: Pointer;
  cbData: DWORD;
begin
  Stream := TFileStream.Create(Source, fmOpenRead or fmShareDenyNone);
  try
    Stream.Seek(0, soFromBeginning);
    cbData := Stream.Size;
    if cbData > 0 then
    begin
      GetMem(lpData, cbData);
      try
        Stream.ReadBuffer(lpData^, cbData);
        hDestRes := BeginUpdateResource(PChar(Dest), False);
        if hDestRes <> 0 then
          if UpdateResource(hDestRes, RT_RCDATA, PWideChar(ResName), 0, lpData,
            cbData) then
          begin
            if not EndUpdateResource(hDestRes, False) then
              RaiseLastOSError;
          end
          else
            RaiseLastOSError
        else
          RaiseLastOSError;
      finally
        FreeMem(lpData);
      end;
    end;
  finally
    Stream.Free;
  end;
end;

constructor TLibImport.Create;
begin
  inherited Create;
  FDLLLoaded := False;
  FImageFileName := '';
  FImagePtr := nil;
  FImageSize := 0;
end;

destructor TLibImport.Destroy;
begin
  UnloadLib;
  inherited Destroy;
end;

procedure TLibImport.LoadLib(ALibrary: String);
var
  LResName: String;
  szFileName: array [0 .. MAX_PATH] of char;
begin
  UnloadLib;
  LResName := FileToResourceName(ALibrary);
  FIsMemoryLib := ResourceExists(LResName);
  if FIsMemoryLib then
  begin
    FDLLStream := TResourceStream.Create(HInstance, LResName, RT_RCDATA);
    WriteLn(ErrOutput, FDLLStream.Size.ToString);
    FDLLHandle := NativeUInt(MemoryLoadLibary(FDLLStream.Memory));
    FDLLLoaded := Assigned(Pointer(FDLLHandle));
  end
  else
  begin
    FDLLStream := TMemoryStream.Create;
    FDLLHandle := LoadLibrary(PWideChar(ALibrary));
    FDLLLoaded := FDLLHandle >= 32;
    if FDLLLoaded then
    begin
      FillChar(szFileName, sizeof(szFileName), #0);
      GetModuleFileName(FDLLHandle, szFileName, MAX_PATH);
    end;
  end;
  FImageFileName := ALibrary;
  FImagePtr := FDLLStream.Memory;
  FImageSize := FDLLStream.Size;
end;

procedure TLibImport.LoadLib(AMemory: Pointer; ASize: NativeInt);
begin
  UnloadLib;
  FIsMemoryLib := True;
  FDLLStream := TMemoryStream.Create;
  FDLLStream.WriteBuffer(AMemory, ASize);
  FDLLHandle := NativeUInt(MemoryLoadLibary(FDLLStream.Memory));
  FDLLLoaded := Assigned(Pointer(FDLLHandle));
  FImageFileName := '';
  FImagePtr := FDLLStream.Memory;
  FImageSize := FDLLStream.Size;
end;

procedure TLibImport.LoadLib(AStream: TStream; ASize: NativeInt);
begin
  UnloadLib;
  FIsMemoryLib := True;
  FDLLStream := TMemoryStream.Create;
  FDLLStream.CopyFrom(AStream, ASize);
  FDLLHandle := NativeUInt(MemoryLoadLibary(FDLLStream.Memory));
  FDLLLoaded := Assigned(Pointer(FDLLHandle));
  FImageFileName := '';
  FImagePtr := FDLLStream.Memory;
  FImageSize := FDLLStream.Size;
end;

procedure TLibImport.UnloadLib;
begin
  if FIsMemoryLib then
  begin
    MemoryFreeLibrary(Pointer(FDLLHandle));
  end
  else if FDLLLoaded then
    FreeLibrary(FDLLHandle);
  if FDLLLoaded then
    FDLLStream.Free;
  FDLLLoaded := False;
  FImagePtr := nil;
  FImageSize := 0;
end;

function TLibImport.GetProcAddr(AProcName: PAnsiChar): Pointer;
begin
  if not FDLLLoaded then
    Result := nil
  else if FIsMemoryLib then
    Result := MemoryGetProcAddress(Pointer(FDLLHandle), AProcName)
  else
    Result := GetProcAddress(FDLLHandle, AProcName);
end;

procedure InjectLib(Source, Dest: String);
var
  LResName: String;
begin
  if not FileExists(Source) then
    raise Exception.CreateRes(@SSpecifiedFileNotFound);
  LResName := FileToResourceName(Source);
  UpdateFileResource(Source, Dest, LResName);
end;

end.
