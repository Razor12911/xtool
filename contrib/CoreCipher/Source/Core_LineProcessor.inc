{$IFDEF RangeCheck}{$R-}{$ENDIF}
{$IFDEF OverflowCheck}{$Q-}{$ENDIF}


procedure TLineProcessor{$IFNDEF FPC}<T_>{$ENDIF FPC}.CreateDone;
begin
end;

constructor TLineProcessor{$IFNDEF FPC}<T_>{$ENDIF FPC}.Create(const data_: Pointer; const width_, height_: NativeInt; const Value_: T_; const LineTail_: Boolean);
begin
  inherited Create;
  FData := PTArry_(data_);
  FWidth := width_;
  FHeight := height_;
  FValue := Value_;
  FLineTail := LineTail_;
  CreateDone();
end;

destructor TLineProcessor{$IFNDEF FPC}<T_>{$ENDIF FPC}.Destroy;
begin
  inherited Destroy;
end;

procedure TLineProcessor{$IFNDEF FPC}<T_>{$ENDIF FPC}.VertLine(X, y1, y2: NativeInt);
var
  i: NativeInt;
  p: PT_;
begin
  if (X < 0) or (X >= FWidth) then
      Exit;

  if y1 < 0 then
      y1 := 0;
  if y1 >= FHeight then
      y1 := FHeight - 1;

  if y2 < 0 then
      y2 := 0;
  if y2 >= FHeight then
      y2 := FHeight - 1;

  if y2 < y1 then
      Swap(y1, y2);

  p := @FData^[X + y1 * FWidth];
  for i := y1 to y2 do
    begin
      Process(p, FValue);
      inc(p, FWidth);
    end;
end;

procedure TLineProcessor{$IFNDEF FPC}<T_>{$ENDIF FPC}.HorzLine(x1, Y, x2: NativeInt);
var
  i: NativeInt;
  p: PT_;
begin
  if (Y < 0) or (Y >= FHeight) then
      Exit;

  if x1 < 0 then
      x1 := 0;
  if x1 >= FWidth then
      x1 := FWidth - 1;

  if x2 < 0 then
      x2 := 0;
  if x2 >= FWidth then
      x2 := FWidth - 1;

  if x1 > x2 then
      Swap(x1, x2);

  p := @FData^[x1 + Y * FWidth];

  for i := x1 to x2 do
    begin
      Process(p, FValue);
      inc(p);
    end;
end;

procedure TLineProcessor{$IFNDEF FPC}<T_>{$ENDIF FPC}.Line(x1, y1, x2, y2: NativeInt);
var
  dy, dx, SY, SX, i, Delta: NativeInt;
  pi, pl: NativeInt;
begin
  if (x1 = x2) and (y1 = y2) then
    begin
      Process(@FData^[x1 + y1 * FWidth], FValue);
      Exit;
    end;

  dx := x2 - x1;
  dy := y2 - y1;

  if dx > 0 then
      SX := 1
  else if dx < 0 then
    begin
      dx := -dx;
      SX := -1;
    end
  else // Dx = 0
    begin
      if dy > 0 then
          VertLine(x1, y1, y2 - 1)
      else if dy < 0 then
          VertLine(x1, y2 + 1, y1);
      if FLineTail then
          Process(@FData^[x2 + y2 * FWidth], FValue);
      Exit;
    end;

  if dy > 0 then
      SY := 1
  else if dy < 0 then
    begin
      dy := -dy;
      SY := -1;
    end
  else // Dy = 0
    begin
      if x2 > x1 then
          HorzLine(x1, y1, x2 - 1)
      else
          HorzLine(x2 + 1, y1, x1);
      if FLineTail then
          Process(@FData^[x2 + y2 * FWidth], FValue);
      Exit;
    end;

  pi := x1 + y1 * FWidth;
  SY := SY * FWidth;
  pl := FWidth * FHeight;

  if dx > dy then
    begin
      Delta := dx shr 1;
      for i := 0 to dx - 1 do
        begin
          if (pi >= 0) and (pi < pl) then
              Process(@FData^[pi], FValue);

          inc(pi, SX);
          inc(Delta, dy);
          if Delta >= dx then
            begin
              inc(pi, SY);
              dec(Delta, dx);
            end;
        end;
    end
  else // Dx < Dy
    begin
      Delta := dy shr 1;
      for i := 0 to dy - 1 do
        begin
          if (pi >= 0) and (pi < pl) then
              Process(@FData^[pi], FValue);

          inc(pi, SY);
          inc(Delta, dx);
          if Delta >= dy then
            begin
              inc(pi, SX);
              dec(Delta, dy);
            end;
        end;
    end;
  if (FLineTail) and (pi >= 0) and (pi < pl) then
      Process(@FData^[pi], FValue);
end;

procedure TLineProcessor{$IFNDEF FPC}<T_>{$ENDIF FPC}.FillBox(x1, y1, x2, y2: NativeInt);
var
  i: Integer;
begin
  if y1 > y2 then
      Swap(y1, y2);
  for i := y1 to y2 do
      HorzLine(x1, i, x2);
end;

procedure TLineProcessor{$IFNDEF FPC}<T_>{$ENDIF FPC}.Process(const vp: PT_; const v: T_);
begin
  vp^ := v;
end;
{$IFDEF RangeCheck}{$R+}{$ENDIF}
{$IFDEF OverflowCheck}{$Q+}{$ENDIF}
