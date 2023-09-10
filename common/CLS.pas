unit CLS;

interface

uses
  System.SysUtils, System.Classes;

type
  CLS_CALLBACK = function(Instance: Pointer; callback_operation: Integer;
    ptr: Pointer; n: Integer): Integer; cdecl;

  CLS_MAIN = function(operation: Integer; Callback: CLS_CALLBACK;
    Instance: Pointer): Integer; cdecl;

const
  CLS_INIT = 1;
  CLS_DONE = 2;
  CLS_FLUSH = 6;

  CLS_COMPRESS = 3;
  CLS_DECOMPRESS = 4;
  CLS_PREPARE_METHOD = 5;

  CLS_FULL_READ = 4096;
  CLS_PARTIAL_READ = 5120;
  CLS_FULL_WRITE = 6144;
  CLS_PARTIAL_WRITE = 7168;

  CLS_MALLOC = 1;
  CLS_FREE = 2;
  CLS_GET_PARAMSTR = 3;
  CLS_SET_PARAMSTR = 4;
  CLS_THREADS = 5;
  CLS_MEMORY = 6;
  CLS_DECOMPRESSION_MEMORY = 7;
  CLS_DECOMPRESSOR_VERSION = 8;
  CLS_BLOCK = 9;
  CLS_EXPAND_DATA = 10;

  CLS_ID = 101;
  CLS_VERSION = 102;
  CLS_THREAD_SAFE = 103;

  CLS_OK = 0;
  CLS_ERROR_GENERAL = -1;
  CLS_ERROR_NOT_IMPLEMENTED = -2;
  CLS_ERROR_NOT_ENOUGH_MEMORY = -3;
  CLS_ERROR_READ = -4;
  CLS_ERROR_WRITE = -5;
  CLS_ERROR_ONLY_DECOMPRESS = -6;
  CLS_ERROR_INVALID_COMPRESSOR = -7;
  CLS_ERROR_BAD_COMPRESSED_DATA = -8;
  CLS_ERROR_NO_MORE_DATA_REQUIRED = -9;
  CLS_ERROR_OUTBLOCK_TOO_SMALL = -10;

  CLS_PARAM_INT = -1;
  CLS_PARAM_STRING = -2;
  CLS_PARAM_MEMORY_MB = -3;

  CLS_MAX_PARAMSTR_SIZE = 256;
  CLS_MAX_ERROR_MSG = 256;

type
  TCLSStream = class(TStream)
  protected
    FInstance: Pointer;
    FCallback: CLS_CALLBACK;
    FDone: Boolean;
  public
    constructor Create(Callback: CLS_CALLBACK; Instance: Pointer);
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

implementation

constructor TCLSStream.Create(Callback: CLS_CALLBACK; Instance: Pointer);
begin
  inherited Create;
  FCallback := Callback;
  FInstance := Instance;
  FDone := False;
end;

function TCLSStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result := FCallback(FInstance, CLS_FULL_READ, @Buffer, Count);
end;

function TCLSStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := FCallback(FInstance, CLS_PARTIAL_WRITE, @Buffer, Count);
  if (Result = CLS_ERROR_NO_MORE_DATA_REQUIRED) and (FDone = False) then
  begin
    Result := Count;
    FDone := True;
  end
  else if Result < 0 then
    Result := 0;
end;

end.
