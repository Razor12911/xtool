// used Critical Simulate Atomic with TMonitor.Enter(obj) and TMonitor.Exit(obj)
// CriticalSimulateAtomic defined so performance to be reduced

// used soft Simulate Critical(ring)
// SoftCritical defined so performance to be reduced

{ * object lock create by qq600585                                             * }
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

constructor TSoftCritical.Create;
begin
  inherited Create;
  L := False;
end;

procedure TSoftCritical.Acquire;
{$IFDEF ANTI_DEAD_ATOMIC_LOCK}
var
  d: TTimeTick;
{$ENDIF ANTI_DEAD_ATOMIC_LOCK}
begin
{$IFDEF ANTI_DEAD_ATOMIC_LOCK}
  d := GetTimeTick;
  while L do
    if GetTimeTick - d >= 5000 then
        RaiseInfo('dead lock');
{$ELSE ANTI_DEAD_ATOMIC_LOCK}
  while L do
      NOP;
{$ENDIF ANTI_DEAD_ATOMIC_LOCK}
  L := True;
end;

procedure TSoftCritical.Release;
begin
  L := False;
end;

procedure TSoftCritical.Enter;
begin
  Acquire;
end;

procedure TSoftCritical.Leave;
begin
  Release;
end;

constructor TCritical.Create;
begin
  inherited Create;
  LNum := TAtomInt.Create(0);
end;

destructor TCritical.Destroy;
begin
  LNum.Free;
  inherited Destroy;
end;

procedure TCritical.Acquire;
begin
  Inc(LNum.LockP^);
  LNum.UnLock;
  inherited Acquire;
end;

procedure TCritical.Release;
begin
  Dec(LNum.LockP^);
  LNum.UnLock;
  inherited Release;
end;

procedure TCritical.Enter;
begin
  Acquire();
end;

procedure TCritical.Leave;
begin
  Release();
end;

function TCritical.IsBusy: Boolean;
begin
  Result := LNum.V > 0;
end;

type
  PCritical_Struct = ^TCritical_Struct;

  TCritical_Struct = record
    Obj: TObject;
    LEnter: Integer;
    LockTick: TTimeTick;
    Critical: TCritical;
  end;

  TGetCriticalLockState = (lsSame, lsNew, lsIdle);

var
  CoreLockCritical: TCriticalSection;
  CoreComputeCritical: TCriticalSection;
  CoreTimeTickCritical: TCriticalSection;
  CriticalList: TCoreClassList;

procedure InitCriticalLock;
begin
  CoreLockCritical := TCriticalSection.Create;
  CoreComputeCritical := TCriticalSection.Create;
  CoreTimeTickCritical := TCriticalSection.Create;
  CriticalList := TCoreClassList.Create;
end;

procedure FreeCriticalLock;
var
  i: Integer;
  p: PCritical_Struct;
begin
  for i := 0 to CriticalList.Count - 1 do
    begin
      p := PCritical_Struct(CriticalList[i]);
      p^.Critical.Free;
      Dispose(p);
    end;
  CriticalList.Free;
  CriticalList := nil;

  CoreLockCritical.Free;
  CoreLockCritical := nil;

  CoreComputeCritical.Free;
  CoreComputeCritical := nil;

  CoreTimeTickCritical.Free;
  CoreTimeTickCritical := nil;
end;

procedure GetCriticalLock(const Obj: TObject; var output: PCritical_Struct; var state: TGetCriticalLockState);
var
  i, pIndex: Integer;
  p1, p2: PCritical_Struct;
begin
  output := nil;
  pIndex := -1;
  p1 := nil;
  i := 0;
  while i < CriticalList.Count do
    begin
      p2 := PCritical_Struct(CriticalList[i]);
      if p2^.Obj = Obj then
        begin
          output := p2;
          state := TGetCriticalLockState.lsSame;
          exit;
        end
      else if (p2^.Obj = nil) and (p2^.LEnter = 0) then
        begin
          p1 := p2;
          pIndex := i;
        end;
      Inc(i);
    end;

  if p1 <> nil then
    begin
      p1^.Obj := Obj;
      output := p1;
      if pIndex > 0 then
          CriticalList.Move(pIndex, 0);
      state := TGetCriticalLockState.lsIdle;
    end
  else
    begin
      new(p1);
      p1^.Obj := Obj;
      p1^.LEnter := 0;
      p1^.LockTick := GetTimeTick();
      p1^.Critical := TCritical.Create;
      CriticalList.Insert(0, p1);
      output := p1;
      state := TGetCriticalLockState.lsNew;
    end;
end;

procedure _LockCriticalObj(Obj: TObject);
var
  p: PCritical_Struct;
  ls: TGetCriticalLockState;
begin
  CoreLockCritical.Acquire;
  GetCriticalLock(Obj, p, ls);
  CoreLockCritical.Release;
  p^.Critical.Acquire;
  p^.LockTick := GetTimeTick();
  AtomInc(p^.LEnter);
end;

procedure _UnLockCriticalObj(Obj: TObject);
var
  p: PCritical_Struct;
  ls: TGetCriticalLockState;
begin
  CoreLockCritical.Acquire;
  GetCriticalLock(Obj, p, ls);
  CoreLockCritical.Release;

  AtomDec(p^.LEnter);
  if p^.LEnter < 0 then
      RaiseInfo('error: unlock failed: illegal unlock');
  p^.LockTick := GetTimeTick();
  p^.Critical.Release;
end;

procedure _RecycleLocker(const Obj: TObject);
var
  p: PCritical_Struct;
  i: Integer;
begin
  if (CoreLockCritical = nil) or (CriticalList = nil) or (CriticalList.Count = 0) then
      exit;

  CoreLockCritical.Acquire;
  i := 0;
  while i < CriticalList.Count do
    begin
      p := PCritical_Struct(CriticalList[i]);
      if p^.Obj = Obj then
        begin
          CriticalList.Delete(i);
          p^.Critical.Free;
          Dispose(p);
          break;
        end
      else
          Inc(i);
    end;
  CoreLockCritical.Release;
end;

function DeltaStep(const value_, Delta_: NativeInt): NativeInt;
begin
  if Delta_ > 0 then
      Result := (value_ + (Delta_ - 1)) and (not(Delta_ - 1))
  else
      Result := value_;
end;

procedure AtomInc(var x: Int64);
begin
{$IFDEF FPC}
  CoreComputeCritical.Acquire;
  Inc(x);
  CoreComputeCritical.Release;
{$ELSE FPC}
  System.AtomicIncrement(x);
{$ENDIF FPC}
end;

procedure AtomInc(var x: Int64; const V: Int64);
begin
{$IFDEF FPC}
  CoreComputeCritical.Acquire;
  Inc(x, V);
  CoreComputeCritical.Release;
{$ELSE FPC}
  System.AtomicIncrement(x, V);
{$ENDIF FPC}
end;

procedure AtomDec(var x: Int64);
begin
{$IFDEF FPC}
  CoreComputeCritical.Acquire;
  Dec(x);
  CoreComputeCritical.Release;
{$ELSE FPC}
  System.AtomicDecrement(x);
{$ENDIF FPC}
end;

procedure AtomDec(var x: Int64; const V: Int64);
begin
{$IFDEF FPC}
  CoreComputeCritical.Acquire;
  Dec(x, V);
  CoreComputeCritical.Release;
{$ELSE FPC}
  System.AtomicDecrement(x, V);
{$ENDIF FPC}
end;

procedure AtomInc(var x: UInt64);
begin
{$IFDEF FPC}
  CoreComputeCritical.Acquire;
  Inc(x);
  CoreComputeCritical.Release;
{$ELSE FPC}
  System.AtomicIncrement(x);
{$ENDIF FPC}
end;

procedure AtomInc(var x: UInt64; const V: UInt64);
begin
{$IFDEF FPC}
  CoreComputeCritical.Acquire;
  Inc(x, V);
  CoreComputeCritical.Release;
{$ELSE FPC}
  System.AtomicIncrement(x, V);
{$ENDIF FPC}
end;

procedure AtomDec(var x: UInt64);
begin
{$IFDEF FPC}
  CoreComputeCritical.Acquire;
  Dec(x);
  CoreComputeCritical.Release;
{$ELSE FPC}
  System.AtomicDecrement(x);
{$ENDIF FPC}
end;

procedure AtomDec(var x: UInt64; const V: UInt64);
begin
{$IFDEF FPC}
  CoreComputeCritical.Acquire;
  Dec(x, V);
  CoreComputeCritical.Release;
{$ELSE FPC}
  System.AtomicDecrement(x, V);
{$ENDIF FPC}
end;

procedure AtomInc(var x: Integer);
begin
{$IFDEF FPC}
  CoreComputeCritical.Acquire;
  Inc(x);
  CoreComputeCritical.Release;
{$ELSE FPC}
  System.AtomicIncrement(x);
{$ENDIF FPC}
end;

procedure AtomInc(var x: Integer; const V: Integer);
begin
{$IFDEF FPC}
  CoreComputeCritical.Acquire;
  Inc(x, V);
  CoreComputeCritical.Release;
{$ELSE FPC}
  System.AtomicIncrement(x, V);
{$ENDIF FPC}
end;

procedure AtomDec(var x: Integer);
begin
{$IFDEF FPC}
  CoreComputeCritical.Acquire;
  Dec(x);
  CoreComputeCritical.Release;
{$ELSE FPC}
  System.AtomicDecrement(x);
{$ENDIF FPC}
end;

procedure AtomDec(var x: Integer; const V: Integer);
begin
{$IFDEF FPC}
  CoreComputeCritical.Acquire;
  Dec(x, V);
  CoreComputeCritical.Release;
{$ELSE FPC}
  System.AtomicDecrement(x, V);
{$ENDIF FPC}
end;

procedure AtomInc(var x: Cardinal);
begin
{$IFDEF FPC}
  CoreComputeCritical.Acquire;
  Inc(x);
  CoreComputeCritical.Release;
{$ELSE FPC}
  System.AtomicIncrement(x);
{$ENDIF FPC}
end;

procedure AtomInc(var x: Cardinal; const V: Cardinal);
begin
{$IFDEF FPC}
  CoreComputeCritical.Acquire;
  Inc(x, V);
  CoreComputeCritical.Release;
{$ELSE FPC}
  System.AtomicIncrement(x, V);
{$ENDIF FPC}
end;

procedure AtomDec(var x: Cardinal);
begin
{$IFDEF FPC}
  CoreComputeCritical.Acquire;
  Dec(x);
  CoreComputeCritical.Release;
{$ELSE FPC}
  System.AtomicDecrement(x);
{$ENDIF FPC}
end;

procedure AtomDec(var x: Cardinal; const V: Cardinal);
begin
{$IFDEF FPC}
  CoreComputeCritical.Acquire;
  Dec(x, V);
  CoreComputeCritical.Release;
{$ELSE FPC}
  System.AtomicDecrement(x, V);
{$ENDIF FPC}
end;
