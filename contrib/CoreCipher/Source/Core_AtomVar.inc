function TAtomVar{$IFNDEF FPC}<T_>{$ENDIF FPC}.GetValue: T_;
begin
  Critical.Acquire;
  Result := FValue__;
  Critical.Release;
end;

procedure TAtomVar{$IFNDEF FPC}<T_>{$ENDIF FPC}.SetValue(const Value_: T_);
begin
  Critical.Acquire;
  FValue__ := Value_;
  Critical.Release;
end;

function TAtomVar{$IFNDEF FPC}<T_>{$ENDIF FPC}.GetValueP: PT_;
begin
  Result := @FValue__;
end;

constructor TAtomVar{$IFNDEF FPC}<T_>{$ENDIF FPC}.Create(Value_: T_);
begin
  inherited Create;
  FValue__ := Value_;
  Critical := TCritical_.Create;
end;

destructor TAtomVar{$IFNDEF FPC}<T_>{$ENDIF FPC}.Destroy;
begin
  Critical.Free;
  inherited Destroy;
end;

function TAtomVar{$IFNDEF FPC}<T_>{$ENDIF FPC}.Lock: T_;
begin
  Critical.Acquire;
  Result := FValue__;
end;

function TAtomVar{$IFNDEF FPC}<T_>{$ENDIF FPC}.LockP: PT_;
begin
  Critical.Acquire;
  Result := @FValue__;
end;

procedure TAtomVar{$IFNDEF FPC}<T_>{$ENDIF FPC}.UnLock(const Value_: T_);
begin
  FValue__ := Value_;
  Critical.Release;
end;

procedure TAtomVar{$IFNDEF FPC}<T_>{$ENDIF FPC}.UnLock(const Value_: PT_);
begin
  FValue__ := Value_^;
  Critical.Release;
end;

procedure TAtomVar{$IFNDEF FPC}<T_>{$ENDIF FPC}.UnLock();
begin
  Critical.Release;
end;
