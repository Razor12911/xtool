unit Threading;

interface

uses
  System.SysUtils, System.Classes;

type
  TThreadStatus = (tsReady, tsRunning, tsErrored, tsTerminated);

  EThreadException = class(Exception);

  TTask = class(TThread)
  private
    FSync: Boolean;
    FErrorMsg: string;
    FStatus: TThreadStatus;
    FProc0: TProc;
    FProc1: TProc<IntPtr>;
    FProc2: TProc<IntPtr, IntPtr>;
    FProc3: TProc<IntPtr, IntPtr, IntPtr>;
    FProc4: TProc<IntPtr, IntPtr, IntPtr, IntPtr>;
    FArgs: array [0 .. 3] of IntPtr;
    FStarted: Boolean;
  public
    constructor Create(Arg1: IntPtr = 0; Arg2: IntPtr = 0; Arg3: IntPtr = 0;
      Arg4: IntPtr = 0);
    destructor Destroy; override;
    procedure Update(Arg1: IntPtr = 0; Arg2: IntPtr = 0; Arg3: IntPtr = 0;
      Arg4: IntPtr = 0);
    procedure Perform(const Proc: TProc); overload;
    procedure Perform(const Proc: TProc<IntPtr>); overload;
    procedure Perform(const Proc: TProc<IntPtr, IntPtr>); overload;
    procedure Perform(const Proc: TProc<IntPtr, IntPtr, IntPtr>); overload;
    procedure Perform(const Proc: TProc<IntPtr, IntPtr, IntPtr,
      IntPtr>); overload;
    procedure Execute; override;
    procedure Start;
    procedure Wait;
    procedure RaiseLastError;
    property Status: TThreadStatus read FStatus;
    property Sync: Boolean read FSync write FSync;
  end;

procedure WaitForAll(const Tasks: array of TTask);
function WaitForAny(const Tasks: array of TTask): Integer;
function IsErrored(const Tasks: array of TTask): Boolean;

implementation

constructor TTask.Create(Arg1, Arg2, Arg3, Arg4: IntPtr);
begin
  inherited Create(True);
  FSync := False;
  FErrorMsg := '';
  FStatus := tsReady;
  FProc0 := nil;
  FProc1 := nil;
  FProc2 := nil;
  FProc3 := nil;
  FProc4 := nil;
  FArgs[0] := Arg1;
  FArgs[1] := Arg2;
  FArgs[2] := Arg3;
  FArgs[3] := Arg4;
  FStarted := False;
end;

destructor TTask.Destroy;
begin
  FStatus := tsTerminated;
  inherited Destroy;
end;

procedure TTask.Update(Arg1, Arg2, Arg3, Arg4: IntPtr);
begin
  FArgs[0] := Arg1;
  FArgs[1] := Arg2;
  FArgs[2] := Arg3;
  FArgs[3] := Arg4;
end;

procedure TTask.Perform(const Proc: TProc);
begin
  FProc0 := Proc;
  FProc1 := nil;
  FProc2 := nil;
  FProc3 := nil;
  FProc4 := nil;
end;

procedure TTask.Perform(const Proc: TProc<IntPtr>);
begin
  FProc0 := nil;
  FProc1 := Proc;
  FProc2 := nil;
  FProc3 := nil;
  FProc4 := nil;
end;

procedure TTask.Perform(const Proc: TProc<IntPtr, IntPtr>);
begin
  FProc0 := nil;
  FProc1 := nil;
  FProc2 := Proc;
  FProc3 := nil;
  FProc4 := nil;
end;

procedure TTask.Perform(const Proc: TProc<IntPtr, IntPtr, IntPtr>);
begin
  FProc0 := nil;
  FProc1 := nil;
  FProc2 := nil;
  FProc3 := Proc;
  FProc4 := nil;
end;

procedure TTask.Perform(const Proc: TProc<IntPtr, IntPtr, IntPtr, IntPtr>);
begin
  FProc0 := nil;
  FProc1 := nil;
  FProc2 := nil;
  FProc3 := nil;
  FProc4 := Proc;
end;

procedure TTask.Execute;
label
  Restart;
begin
Restart:
  while FStatus in [tsReady, tsErrored] do
    Sleep(1);
  try
    if FStatus = tsRunning then
    begin
      if Assigned(FProc0) or Assigned(FProc1) or Assigned(FProc2) or
        Assigned(FProc3) or Assigned(FProc4) then
      begin
        if FSync then
          Self.Synchronize(
            procedure
            begin
              if Assigned(FProc0) then
                FProc0
              else if Assigned(FProc1) then
                FProc1(FArgs[0])
              else if Assigned(FProc2) then
                FProc2(FArgs[0], FArgs[1])
              else if Assigned(FProc3) then
                FProc3(FArgs[0], FArgs[1], FArgs[2])
              else if Assigned(FProc4) then
                FProc4(FArgs[0], FArgs[1], FArgs[2], FArgs[3]);
            end)
        else
        begin
          if Assigned(FProc0) then
            FProc0
          else if Assigned(FProc1) then
            FProc1(FArgs[0])
          else if Assigned(FProc2) then
            FProc2(FArgs[0], FArgs[1])
          else if Assigned(FProc3) then
            FProc3(FArgs[0], FArgs[1], FArgs[2])
          else if Assigned(FProc4) then
            FProc4(FArgs[0], FArgs[1], FArgs[2], FArgs[3]);
        end;
      end;
      FStatus := tsReady;
    end;
  except
    on E: Exception do
    begin
      if E.Message <> '' then
        FErrorMsg := E.Message
      else
        FErrorMsg := 'Unknown error';
      FStatus := tsErrored;
    end;
  end;
  if FStatus <> tsTerminated then
    goto Restart;
end;

procedure TTask.Start;
begin
  if not FStarted then
  begin
    FStarted := True;
    inherited Start;
  end;
  FStatus := tsRunning;
end;

procedure TTask.Wait;
begin
  while FStatus = tsRunning do
    Sleep(1);
end;

procedure TTask.RaiseLastError;
begin
  if FErrorMsg <> '' then
  begin
    raise EThreadException.Create(FErrorMsg);
    FErrorMsg := '';
  end;
end;

procedure WaitForAll(const Tasks: array of TTask);
var
  I: Integer;
begin
  for I := Low(Tasks) to High(Tasks) do
    Tasks[I].Wait;
end;

function WaitForAny(const Tasks: array of TTask): Integer;
var
  I: Integer;
begin
  Result := -1;
  while True do
  begin
    for I := Low(Tasks) to High(Tasks) do
    begin
      if Tasks[I].Status <> tsRunning then
      begin
        Result := I;
        exit;
      end;
    end;
    Sleep(1);
  end;
end;

function IsErrored(const Tasks: array of TTask): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := Low(Tasks) to High(Tasks) do
  begin
    if Tasks[I].Status = TThreadStatus.tsErrored then
    begin
      Result := True;
      exit;
    end;
  end;
end;

end.
