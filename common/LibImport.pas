unit LibImport;

interface

uses
  MemoryModule,
  WinAPI.Windows,
  System.SysUtils, System.Classes, System.Character;

type
  TLibImport = class
  private
    FIsMemoryLib: Boolean;
    FDLLLoaded: Boolean;
    FDLLStream: TResourceStream;
    FDLLHandle: NativeUInt;
  public
    constructor Create(ALibrary: String);
    destructor Destroy; override;
    function GetProcAddr(AProcName: PAnsiChar): Pointer;
    property Loaded: Boolean read FDLLLoaded;
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
  Result := ChangeFileExt(ExtractFileName(FileName), '').ToUpper;
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

constructor TLibImport.Create(ALibrary: String);
var
  LResName: String;
begin
  inherited Create;
  FDLLLoaded := False;
  LResName := FileToResourceName(ALibrary);
  FIsMemoryLib := ResourceExists(LResName);
  if FIsMemoryLib then
  begin
    FDLLStream := TResourceStream.Create(HInstance, LResName, RT_RCDATA);
    FDLLHandle := NativeUInt(MemoryLoadLibary(FDLLStream.Memory));
    FDLLLoaded := Assigned(Pointer(FDLLHandle));
  end
  else
  begin
    FDLLHandle := LoadLibrary(PWideChar(ALibrary));
    FDLLLoaded := FDLLHandle >= 32;
  end;
end;

destructor TLibImport.Destroy;
begin
  if FIsMemoryLib then
  begin
    if FDLLLoaded then
      MemoryFreeLibrary(Pointer(FDLLHandle));
    FDLLStream.Free;
  end
  else if FDLLLoaded then
    FreeLibrary(FDLLHandle);
  inherited Destroy;
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
  LResName := FileToResourceName(Source);
  UpdateFileResource(Source, Dest, LResName);
end;

end.
