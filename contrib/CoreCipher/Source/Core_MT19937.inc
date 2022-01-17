(*
  paper: Mersenne Twister: A 623-dimensionallyequidistributed uniformpseudorandom number generator
  post by 2002

  reference material
  https://baike.baidu.com/item/%E6%A2%85%E6%A3%AE%E7%B4%A0%E6%95%B0
  https://baike.baidu.com/item/%E6%A2%85%E6%A3%AE%E6%97%8B%E8%BD%AC%E7%AE%97%E6%B3%95
  https://www.cnblogs.com/lfri/p/11461695.html
  https://en.wikipedia.org/wiki/Mersenne_twister
*)

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
const
  MT19937N = 624;
  MT19937M = 397;
  Mag01: array [0 .. 1] of Integer = (0, Integer($9908B0DF));
  MT19937UPPER_MASK = Integer($80000000); // most significant r/w bits
  MT19937LOWER_MASK = Integer($7FFFFFFF); // least significant r bits
  TEMPERING_MASK_B = Integer($9D2C5680);
  TEMPERING_MASK_C = Integer($EFC60000);

type
  TMTVector = array [0 .. MT19937N - 1] of Integer;

type
  TMT19937Core = record
    MT: TMTVector; // the array for the state vector
    MTI: Integer;
    InternalRndSeed, InternalOldRndSeed: Cardinal;
    Thread: TCoreClassThread;
    LastActivtedTime: TTimeTick;
    Busy: Boolean;
    Instance: Integer;
    procedure BuildMT(Seed_: Integer);
    function GenRand_MT19937(): Integer;
    procedure Init(Thread_: TCoreClassThread; LastActivtedTime_: TTimeTick);
    procedure Serialize(stream: TCoreClassStream);
    procedure Unserialize(stream: TCoreClassStream);
  end;

  PMD19937Core = ^TMT19937Core;

  TMT19937List_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<PMD19937Core>;

  TMT19937List = class(TMT19937List_Decl)
  end;

  { Initializing the array with a seed }
procedure TMT19937Core.BuildMT(Seed_: Integer);
var
  i: Integer;
begin
  MT[0] := Integer(Seed_);
  for i := 1 to MT19937N - 1 do
    begin
      MT[i] := 1812433253 * (MT[i - 1] xor (MT[i - 1] shr 30)) + i;
      { See Knuth TAOCP Vol2. 3rd Ed. P.106 for multiplier. }
      { In the previous versions, MSBs of the seed affect }
      { only MSBs of the array mt[]. }
      { 2002/01/09 modified by Makoto Matsumoto }
    end;
  MTI := MT19937N;
end;

function TMT19937Core.GenRand_MT19937(): Integer;
var
  th: TCoreClassThread;
  Y, K: Integer;
begin
  if InternalRndSeed <> InternalOldRndSeed then
      MTI := MT19937N + 1;

  { generate MT19937N longints at one time }
  if (MTI >= MT19937N) then
    begin
      { if BuildMT() has not been called }
      if MTI = (MT19937N + 1) then
        begin
          { default initial seed is used }
          BuildMT(Integer(InternalRndSeed));

          { hack: InternalRndSeed is not used more than once in this algorithm. Most }
          { user changes are re-initialising reandseed with the value it had }
          { at the start -> with the "not", we will detect this change. }
          { Detecting other changes is not useful, since the generated }
          { numbers will be different anyway. }
          InternalRndSeed := not(InternalRndSeed);
          InternalOldRndSeed := InternalRndSeed;
        end;

      for K := 0 to MT19937N - MT19937M - 1 do
        begin
          Y := (MT[K] and MT19937UPPER_MASK) or (MT[K + 1] and MT19937LOWER_MASK);
          MT[K] := MT[K + MT19937M] xor (Y shr 1) xor Mag01[Y and $00000001];
        end;

      for K := MT19937N - MT19937M to MT19937N - 2 do
        begin
          Y := (MT[K] and MT19937UPPER_MASK) or (MT[K + 1] and MT19937LOWER_MASK);
          MT[K] := MT[K + (MT19937M - MT19937N)] xor (Y shr 1) xor Mag01[Y and $00000001];
        end;

      Y := (MT[MT19937N - 1] and MT19937UPPER_MASK) or (MT[0] and MT19937LOWER_MASK);
      MT[MT19937N - 1] := MT[MT19937M - 1] xor (Y shr 1) xor Mag01[Y and $00000001];
      MTI := 0;
    end;

  Y := MT[MTI];
  inc(MTI);
  Y := Y xor (Y shr 11);
  Y := Y xor (Y shl 7) and TEMPERING_MASK_B;
  Y := Y xor (Y shl 15) and TEMPERING_MASK_C;
  Y := Y xor (Y shr 18);
  Result := Y;
end;

procedure TMT19937Core.Init(Thread_: TCoreClassThread; LastActivtedTime_: TTimeTick);
begin
  InternalRndSeed := 0;
  InternalOldRndSeed := 0;
  BuildMT(0);
  Thread := Thread_;
  LastActivtedTime := LastActivtedTime_;
  Busy := False;
  Instance := 0;
end;

procedure TMT19937Core.Serialize(stream: TCoreClassStream);
begin
  stream.WriteBuffer(MT[0], SizeOf(TMTVector));
  stream.WriteBuffer(MTI, 4);
  stream.WriteBuffer(InternalRndSeed, 4);
  stream.WriteBuffer(InternalOldRndSeed, 4);
end;

procedure TMT19937Core.Unserialize(stream: TCoreClassStream);
begin
  stream.ReadBuffer(MT[0], SizeOf(TMTVector));
  stream.ReadBuffer(MTI, 4);
  stream.ReadBuffer(InternalRndSeed, 4);
  stream.ReadBuffer(InternalOldRndSeed, 4);
end;

var
  MT19937InternalCritical: TCritical;
  MT19937POOL: TMT19937List;
  MT19937CoreToDelphi_: Boolean;

function InternalMT19937__(): PMD19937Core;
var
  th: TCoreClassThread;
  i: Integer;
  p: PMD19937Core;
begin
  th := TCoreClassThread.CurrentThread;
  Result := nil;

  MT19937InternalCritical.Acquire;

  i := 0;
  while i < MT19937POOL.Count do
    begin
      p := MT19937POOL[i];
      if p^.Thread = th then
        begin
          if i > 0 then
              MT19937POOL.Exchange(0, i);
          p^.LastActivtedTime := GetTimeTick;
          Result := p;
          inc(i);
        end
      else if (not p^.Busy) and (p^.Instance <= 0) and (GetTimeTick - p^.LastActivtedTime > MT19937LifeTime) then
        begin
          dispose(p);
          MT19937POOL.Delete(i);
        end
      else
          inc(i);
    end;

  if Result = nil then
    begin
      New(p);
      p^.Init(th, GetTimeTick);
      MT19937POOL.Add(p);
      Result := p;
    end;

  MT19937InternalCritical.Release;
end;

procedure RemoveMT19937Thread(th: TCoreClassThread);
var
  i: Integer;
  p: PMD19937Core;
begin
  MT19937InternalCritical.Acquire;
  i := 0;
  while i < MT19937POOL.Count do
    begin
      p := MT19937POOL[i];
      if (p^.Thread = th) or
        ((not p^.Busy) and (p^.Instance <= 0) and (GetTimeTick - p^.LastActivtedTime > MT19937LifeTime)) then
        begin
          dispose(p);
          MT19937POOL.Delete(i);
        end
      else
          inc(i);
    end;
  MT19937InternalCritical.Release;
end;

{$IFDEF DELPHI}

{$IFDEF InstallMT19937CoreToDelphi}


function DelphiRandom32Proc: UInt32;
begin
  Result := UInt32(InternalMT19937__()^.GenRand_MT19937());
end;

procedure DelphiRandomizeProc(NewSeed: UInt64);
begin
  InternalMT19937__()^.InternalRndSeed := Cardinal(NewSeed);
end;

procedure MT19937Install();
begin
  Random32Proc := DelphiRandom32Proc;
  RandomizeProc := DelphiRandomizeProc;
  MT19937CoreToDelphi_ := True;
end;
{$ENDIF InstallMT19937CoreToDelphi}

{$ENDIF DELPHI}


procedure InitMT19937Rand;
begin
  MT19937InternalCritical := TCritical.Create;
  MT19937POOL := TMT19937List.Create;
  MT19937CoreToDelphi_ := False;

{$IFDEF DELPHI}
{$IFDEF InstallMT19937CoreToDelphi}
  MT19937Install();
{$ENDIF InstallMT19937CoreToDelphi}
{$ENDIF DELPHI}
  MT19937LifeTime := 10 * 1000;
end;

procedure FreeMT19937Rand;
var
  i: Integer;
begin
  for i := 0 to MT19937POOL.Count - 1 do
      dispose(MT19937POOL[i]);
  DisposeObject(MT19937POOL);
  MT19937POOL := nil;
  MT19937InternalCritical.Free;
  MT19937InternalCritical := nil;
end;

function MT19937CoreToDelphi: Boolean;
begin
  Result := MT19937CoreToDelphi_;
end;

function MT19937InstanceNum(): Integer;
begin
  MT19937InternalCritical.Acquire;
  Result := MT19937POOL.Count;
  MT19937InternalCritical.Release;
end;

procedure SetMT19937Seed(seed: Integer);
begin
  with InternalMT19937__()^ do
    begin
      MT19937InternalCritical.Acquire;
      InternalRndSeed := seed;
      InternalOldRndSeed := seed;
      BuildMT(seed);
      Thread := TCoreClassThread.CurrentThread;
      LastActivtedTime := GetTimeTick();
      MT19937InternalCritical.Release;
    end;
end;

function GetMT19937Seed(): Integer;
begin
  Result := InternalMT19937__()^.InternalRndSeed;
end;

procedure MT19937Randomize();
begin
  SetMT19937Seed(Integer(GetTimeTick()));
end;

function MT19937Rand32(L: Integer): Integer;
begin
  { otherwise we can return values = L (JM) }
  if (L < 0) then
      inc(L);
  Result := Integer((Int64(Cardinal(InternalMT19937__()^.GenRand_MT19937())) * L) shr 32);
end;

procedure MT19937Rand32(L: Integer; dest: PInteger; num: NativeInt);
begin
  { otherwise we can return values = L (JM) }
  if (L < 0) then
      inc(L);

  with InternalMT19937__()^ do
    begin
      Busy := True;
      try
        while num > 0 do
          begin
            dest^ := Integer((Int64(Cardinal(GenRand_MT19937())) * L) shr 32);
            dec(num);
            inc(dest);
          end;
      finally
        LastActivtedTime := GetTimeTick;
        Busy := False;
      end;
    end;
end;

function MT19937Rand64(L: Int64): Int64;
begin
  { always call random, so the random generator cycles (TP-compatible) (JM) }
  with InternalMT19937__()^ do
      Result := Int64((UInt64(Cardinal(GenRand_MT19937())) or ((UInt64(Cardinal(GenRand_MT19937())) shl 32))) and $7FFFFFFFFFFFFFFF);
  if (L <> 0) then
      Result := Result mod L
  else
      Result := 0;
end;

procedure MT19937Rand64(L: Int64; dest: PInt64; num: NativeInt);
begin
  with InternalMT19937__()^ do
    begin
      Busy := True;
      try
        while num > 0 do
          begin
            dest^ := Int64((UInt64(Cardinal(GenRand_MT19937())) or ((UInt64(Cardinal(GenRand_MT19937())) shl 32))) and $7FFFFFFFFFFFFFFF);
            if (dest^ <> 0) then
                dest^ := dest^ mod L
            else
                dest^ := 0;
            dec(num);
            inc(dest);
          end;
      finally
        LastActivtedTime := GetTimeTick;
        Busy := False;
      end;
    end;
end;

function MT19937RandE: Extended;
const
  f = Extended(1.0) / (Int64(1) shl 32);
begin
  Result := f * Cardinal(InternalMT19937__()^.GenRand_MT19937());
end;

procedure MT19937RandE(dest: PExtended; num: NativeInt);
const
  f = Extended(1.0) / (Int64(1) shl 32);
begin
  with InternalMT19937__()^ do
    begin
      Busy := True;
      try
        while num > 0 do
          begin
            dest^ := f * Cardinal(GenRand_MT19937());
            dec(num);
            inc(dest);
          end;
      finally
        LastActivtedTime := GetTimeTick;
        Busy := False;
      end;
    end;
end;

function MT19937RandF: Single;
const
  f = Single(1.0) / (Int64(1) shl 32);
begin
  Result := f * Cardinal(InternalMT19937__()^.GenRand_MT19937());
end;

procedure MT19937RandF(dest: PSingle; num: NativeInt);
const
  f = Single(1.0) / (Int64(1) shl 32);
begin
  with InternalMT19937__()^ do
    begin
      Busy := True;
      try
        while num > 0 do
          begin
            dest^ := f * Cardinal(GenRand_MT19937());
            dec(num);
            inc(dest);
          end;
      finally
        LastActivtedTime := GetTimeTick;
        Busy := False;
      end;
    end;
end;

function MT19937RandD: Double;
const
  f = Double(1.0) / (Int64(1) shl 32);
begin
  Result := f * Cardinal(InternalMT19937__()^.GenRand_MT19937());
end;

procedure MT19937RandD(dest: PDouble; num: NativeInt);
const
  f = Double(1.0) / (Int64(1) shl 32);
begin
  with InternalMT19937__()^ do
    begin
      Busy := True;
      try
        while num > 0 do
          begin
            dest^ := f * Cardinal(GenRand_MT19937());
            dec(num);
            inc(dest);
          end;
      finally
        LastActivtedTime := GetTimeTick;
        Busy := False;
      end;
    end;
end;

procedure MT19937SaveToStream(stream: TCoreClassStream);
begin
  InternalMT19937__()^.Serialize(stream);
end;

procedure MT19937LoadFromStream(stream: TCoreClassStream);
begin
  InternalMT19937__()^.Unserialize(stream);
end;

{ ****************************************************************************** }
{ * TMT19937 classes                                                           * }
{ ****************************************************************************** }
function TMT19937Random.GetSeed: Integer;
begin
  with PMD19937Core(FRndInstance)^ do
      Result := InternalRndSeed;
end;

procedure TMT19937Random.SetSeed(const Value: Integer);
begin
  with PMD19937Core(FRndInstance)^ do
    begin
      InternalRndSeed := Value;
      InternalOldRndSeed := Value;
      BuildMT(Value);
    end;
end;

constructor TMT19937Random.Create;
begin
  inherited Create;
  FRndInstance := InternalMT19937__();
  AtomInc(PMD19937Core(FRndInstance)^.Instance);
end;

destructor TMT19937Random.Destroy;
begin
  AtomDec(PMD19937Core(FRndInstance)^.Instance);
  inherited Destroy;
end;

procedure TMT19937Random.Rndmize;
begin
  with PMD19937Core(FRndInstance)^ do
      InternalRndSeed := GetTimeTick;
end;

function TMT19937Random.Rand32(L: Integer): Integer;
begin
  { otherwise we can return values = L (JM) }
  if (L < 0) then
      inc(L);
  with PMD19937Core(FRndInstance)^ do
      Result := Integer((Int64(Cardinal(GenRand_MT19937())) * L) shr 32);
end;

procedure TMT19937Random.Rand32(L: Integer; dest: PInteger; num: NativeInt);
begin
  { otherwise we can return values = L (JM) }
  if (L < 0) then
      inc(L);

  with PMD19937Core(FRndInstance)^ do
    begin
      while num > 0 do
        begin
          dest^ := Integer((Int64(Cardinal(GenRand_MT19937())) * L) shr 32);
          dec(num);
          inc(dest);
        end;
    end;
end;

function TMT19937Random.Rand64(L: Int64): Int64;
begin
  { always call random, so the random generator cycles (TP-compatible) (JM) }
  with PMD19937Core(FRndInstance)^ do
      Result := Int64((UInt64(Cardinal(GenRand_MT19937())) or ((UInt64(Cardinal(GenRand_MT19937())) shl 32))) and $7FFFFFFFFFFFFFFF);
  if (L <> 0) then
      Result := Result mod L
  else
      Result := 0;
end;

procedure TMT19937Random.Rand64(L: Int64; dest: PInt64; num: NativeInt);
begin
  with PMD19937Core(FRndInstance)^ do
    begin
      while num > 0 do
        begin
          dest^ := Int64((UInt64(Cardinal(GenRand_MT19937())) or ((UInt64(Cardinal(GenRand_MT19937())) shl 32))) and $7FFFFFFFFFFFFFFF);
          if (dest^ <> 0) then
              dest^ := dest^ mod L
          else
              dest^ := 0;
          dec(num);
          inc(dest);
        end;
    end;
end;

function TMT19937Random.RandE: Extended;
const
  f = Extended(1.0) / (Int64(1) shl 32);
begin
  with PMD19937Core(FRndInstance)^ do
      Result := f * Cardinal(GenRand_MT19937());
end;

procedure TMT19937Random.RandE(dest: PExtended; num: NativeInt);
const
  f = Extended(1.0) / (Int64(1) shl 32);
begin
  with PMD19937Core(FRndInstance)^ do
    begin
      while num > 0 do
        begin
          dest^ := f * Cardinal(GenRand_MT19937());
          dec(num);
          inc(dest);
        end;
    end;
end;

function TMT19937Random.RandF: Single;
const
  f = Single(1.0) / (Int64(1) shl 32);
begin
  with PMD19937Core(FRndInstance)^ do
      Result := f * Cardinal(GenRand_MT19937());
end;

procedure TMT19937Random.RandF(dest: PSingle; num: NativeInt);
const
  f = Single(1.0) / (Int64(1) shl 32);
begin
  with PMD19937Core(FRndInstance)^ do
    begin
      while num > 0 do
        begin
          dest^ := f * Cardinal(GenRand_MT19937());
          dec(num);
          inc(dest);
        end;
    end;
end;

function TMT19937Random.RandD: Double;
const
  f = Double(1.0) / (Int64(1) shl 32);
begin
  with PMD19937Core(FRndInstance)^ do
      Result := f * Cardinal(GenRand_MT19937());
end;

procedure TMT19937Random.RandD(dest: PDouble; num: NativeInt);
const
  f = Double(1.0) / (Int64(1) shl 32);
begin
  with PMD19937Core(FRndInstance)^ do
    begin
      while num > 0 do
        begin
          dest^ := f * Cardinal(GenRand_MT19937());
          dec(num);
          inc(dest);
        end;
    end;
end;
