type
  TFPCParallelThData32 = record
    b, e: Integer;
    Completed: ^Integer;
    OnFor: TFPCParallelForProcedure32;
    Critical: TCritical;
  end;

  PFPCParallelThData32 = ^TFPCParallelThData32;

procedure FPCParallelTh32(ThSender: TComputeThread);
var
  p: PFPCParallelThData32;
  Pass: Integer;
begin
  p := ThSender.UserData;
  Pass := p^.b;
  while Pass <= p^.e do
    begin
      p^.OnFor(Pass);
      inc(Pass);
    end;

  p^.Critical.Acquire;
  AtomInc(p^.Completed^, p^.e - p^.b + 1);
  p^.Critical.Release;
  dispose(p);
end;

procedure FPCParallelFor(parallel: Boolean; OnFor: TFPCParallelForProcedure32; b, e: Integer);
var
  Total, Depth, Completed, StepTotal, stepW, Pass, w: Integer;
  p: PFPCParallelThData32;
  i_: Integer;
  Critical: TCritical;
begin
  if b > e then
      exit;
  if (not parallel) or (not WorkInParallelCore.V) or ParallelOverflow.Busy() then
    begin
      i_ := b;
      while i_ <= e do
        begin
          try
              OnFor(i_);
          except
          end;
          inc(i_);
        end;
      exit;
    end;
  ParallelOverflow.Acquire;
  try
    Depth := ParallelGranularity;
    Total := e - b + 1;
    Critical := TCritical.Create;

    Completed := 0;

    if (Total < Depth) then
      begin
        Pass := b;
        while Pass <= e do
          begin
            new(p);
            p^.b := Pass;
            p^.e := Pass;
            p^.Completed := @Completed;
            p^.OnFor := OnFor;
            p^.Critical := Critical;
            TComputeThread.RunC(p, nil, @FPCParallelTh32);
            inc(Pass);
          end;
      end
    else
      begin
        stepW := Total div Depth;
        StepTotal := Total div stepW;
        if Total mod stepW > 0 then
            inc(StepTotal);

        Pass := 0;
        while Pass < StepTotal do
          begin
            w := stepW * Pass;
            new(p);
            if w + stepW <= Total then
              begin
                p^.b := w + b;
                p^.e := w + stepW + b - 1;
              end
            else
              begin
                p^.b := w + b;
                p^.e := Total + b - 1;
              end;
            p^.Completed := @Completed;
            p^.OnFor := OnFor;
            p^.Critical := Critical;
            TComputeThread.RunC(p, nil, @FPCParallelTh32);
            inc(Pass);
          end;
      end;

    repeat
      TThread.Sleep(1);
      Critical.Acquire;
      w := Completed;
      Critical.Release;
    until w >= Total;

    Critical.Free;
  finally
      ParallelOverflow.Release;
  end;
end;

type
  TFPCParallelThData64 = record
    b, e: Int64;
    Completed: ^Int64;
    OnFor: TFPCParallelForProcedure64;
    Critical: TCritical;
  end;

  PFPCParallelThData64 = ^TFPCParallelThData64;

procedure FPCParallelTh64(ThSender: TComputeThread);
var
  p: PFPCParallelThData64;
  Pass: Int64;
begin
  p := ThSender.UserData;
  Pass := p^.b;
  while Pass <= p^.e do
    begin
      p^.OnFor(Pass);
      inc(Pass);
    end;

  p^.Critical.Acquire;
  AtomInc(p^.Completed^, p^.e - p^.b + 1);
  p^.Critical.Release;
  dispose(p);
end;

procedure FPCParallelFor(parallel: Boolean; OnFor: TFPCParallelForProcedure64; b, e: Int64);
var
  Total, Depth, Completed, StepTotal, stepW, Pass, w: Int64;
  p: PFPCParallelThData64;
  i_: Int64;
  Critical: TCritical;
begin
  if b > e then
      exit;
  if (not parallel) or (not WorkInParallelCore.V) or ParallelOverflow.Busy() then
    begin
      i_ := b;
      while i_ <= e do
        begin
          try
              OnFor(i_);
          except
          end;
          inc(i_);
        end;
      exit;
    end;
  ParallelOverflow.Acquire;
  try
    Depth := ParallelGranularity;
    Total := e - b + 1;
    Critical := TCritical.Create;

    Completed := 0;

    if (Total < Depth) then
      begin
        Pass := b;
        while Pass <= e do
          begin
            new(p);
            p^.b := Pass;
            p^.e := Pass;
            p^.Completed := @Completed;
            p^.OnFor := OnFor;
            p^.Critical := Critical;
            TComputeThread.RunC(p, nil, @FPCParallelTh64);
            inc(Pass);
          end;
      end
    else
      begin
        stepW := Total div Depth;
        StepTotal := Total div stepW;
        if Total mod stepW > 0 then
            inc(StepTotal);

        Pass := 0;
        while Pass < StepTotal do
          begin
            w := stepW * Pass;
            new(p);
            if w + stepW <= Total then
              begin
                p^.b := w + b;
                p^.e := w + stepW + b - 1;
              end
            else
              begin
                p^.b := w + b;
                p^.e := Total + b - 1;
              end;
            p^.Completed := @Completed;
            p^.OnFor := OnFor;
            p^.Critical := Critical;
            TComputeThread.RunC(p, nil, @FPCParallelTh64);
            inc(Pass);
          end;
      end;

    repeat
      TThread.Sleep(1);
      Critical.Acquire;
      w := Completed;
      Critical.Release;
    until w >= Total;

    Critical.Free;
  finally
      ParallelOverflow.Release;
  end;
end;

procedure FPCParallelFor(OnFor: TFPCParallelForProcedure32; b, e: Integer);
begin
  FPCParallelFor(True, OnFor, b, e);
end;

procedure FPCParallelFor(OnFor: TFPCParallelForProcedure64; b, e: Int64);
begin
  FPCParallelFor(True, OnFor, b, e);
end;

procedure FPCParallelFor(b, e: Integer; OnFor: TFPCParallelForProcedure32);
begin
  FPCParallelFor(OnFor, b, e);
end;

procedure FPCParallelFor(b, e: Int64; OnFor: TFPCParallelForProcedure64);
begin
  FPCParallelFor(OnFor, b, e);
end;

procedure FPCParallelFor(parallel: Boolean; b, e: Integer; OnFor: TFPCParallelForProcedure32);
begin
  FPCParallelFor(parallel, OnFor, b, e);
end;

procedure FPCParallelFor(parallel: Boolean; b, e: Int64; OnFor: TFPCParallelForProcedure64);
begin
  FPCParallelFor(parallel, OnFor, b, e);
end;
