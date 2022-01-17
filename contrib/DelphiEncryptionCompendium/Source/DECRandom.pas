{*****************************************************************************
  The DEC team (see file NOTICE.txt) licenses this file
  to you under the Apache License, Version 2.0 (the
  "License"); you may not use this file except in compliance
  with the License. A copy of this licence is found in the root directory
  of this project in the file LICENCE.txt or alternatively at

    http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing,
  software distributed under the License is distributed on an
  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied.  See the License for the
  specific language governing permissions and limitations
  under the License.
*****************************************************************************}

/// <summary>
///   Secure Pseudo Random Number Generator based on Yarrow. If used without
///   doing anything special for initialization a repeatable generator will be
///   initialized always using the same start value.
/// </summary>
unit DECRandom;

interface

{$INCLUDE DECOptions.inc}

uses
  {$IFDEF FPC}
  SysUtils,
  {$ELSE}
  System.SysUtils,
  {$ENDIF}
  DECHashBase, DECHash;

/// <summary>
///   Create a seed for the random number generator from system time and
///   PerformanceCounter.
/// </summary>
/// <remarks>
///   Avoid initializing the seed using this fuction if you can as it is not
///   really secure. Use RandomBuffer instead and provide user generated input
///   as Buffer value but ensure that this is not uniform e.g. not a buffer only
///   containing $00 all over or something like this.
/// </remarks>
/// <returns>
///   Created hash value
/// </returns>
function RandomSystemTime: Int64;

/// <summary>
///   Fills the provided buffer with random values. If the DoRandomBuffer
///   variable is assigned (which is usually the case because DoBuffer is
///   assigned to it in initialization of this unit) the hash based algorithm
///   in DoBuffer will be used, otherwise the weaker one in DoRndBuffer.
/// </summary>
/// <param name="Buffer">
///   Buffer to be filled with random values
/// </param>
/// <param name="Size">
///   Size of the buffer in byte
/// </param>
procedure RandomBuffer(out Buffer; Size: Integer);

/// <summary>
///   Creates a buffer of the specified size filled with random bytes
/// </summary>
/// <param name="Size">
///   Size of the buffer to be created in bytes
/// </param>
/// <returns>
///   Buffer of the specified size in bytes filled with random data
/// </returns>
function RandomBytes(Size: Integer): TBytes;
/// <summary>
///   Creates a RawByteString of the specified length filled with random bytes.
/// </summary>
/// <remarks>
///   This function is deprecated. Better use RandomBytes where ever possible!
/// </remarks>
/// <param name="Size">
///   Length of the string to be created in bytes
/// </param>
/// <returns>
///   String of the specified length in bytes filled with random data
/// </returns>
function RandomRawByteString(Size: Integer): RawByteString; deprecated 'please use RandomBytes now';
/// <summary>
///   Creates a random UInt32 value
/// </summary>
/// <returns>
///   Random value
/// </returns>
function RandomLong: UInt32;

/// <summary>
///   If the default value of the global DoRandomSeed variable is kept, this
///   procedure initializes a repeatable or a non repeatable seed,
///   depending on the parameters specified. Otherwise the alternative DoRandomSeed
///   implementation is called. The FRndSeed variable is initialized with the
///   seed value generated.
/// </summary>
/// <param name="Buffer">
///   If a repeatable seed is to be initialized, the contents of this buffer is
///   a parameter to the seed generation and a buffer containing at least Size
///   bytes needs to be passed.
/// </param>
/// <param name="Size">
///   If Size is > 0 a repeatable seed is initialized. If Size is 0 the
///   internal seed variable FRndSeed is initialized with 0. If Size is
///   less than 0 the internal FRndSeed variable is initialized with
///   a value derrived from current system time/performance counter using
///   RandomSystemTime.
/// </param>
procedure RandomSeed(const Buffer; Size: Integer); overload;
/// <summary>
///   Creates a seed (starting) value for the random number generator. If the
///   default value of the global DoRandomSeed variable is kept, a non repeatable
///   seed based on RandomSystemTime (based on system time and potentially
///   QueryPerformanceCounter) is created and assigned to the internal FRndSeed
///   variable.
/// </summary>
procedure RandomSeed; overload;

var
  // secure PRNG initialized by this unit

  /// <summary>
  ///   This variable allows overriding the random number generation procedure
  ///   used for data buffers. By default it is initialized to point to DoBuffer,
  ///   which is a DECRandom internal procedure.
  /// </summary>
  /// <param name="Buffer">
  ///   Buffer in which the random bytes shall be written. The buffer needs to
  ///   exist and must be of at least Size bytes length.
  /// </param>
  /// <param name="Size">
  ///   Length of the buffer to be filled in Byte.
  /// </param>
  DoRandomBuffer: procedure(out Buffer; Size: Integer); register = nil;

  /// <summary>
  ///   This variable allows overriding the seed value generation procedure.
  ///   By default it is initialized with the DECRandom internal procedure DoSeed.
  /// </summary>
  DoRandomSeed: procedure(const Buffer; Size: Integer); register = nil;
  /// <summary>
  ///   Defines the hash-algorithm used for generatin seed values or hashed buffers
  /// </summary>
  RandomClass: TDECHashClass = THash_SHA256;

implementation

uses
  {$IFDEF DELPHI_2010_UP}
    System.Diagnostics
  {$ELSE}
    {$IFDEF FPC}
      {$IFDEF MSWINDOWS}
      Windows
      {$ELSE}
      LclIntf
      {$ENDIF}
    {$ELSE}
    Winapi.Windows
    {$ENDIF}
  {$ENDIF}
  ;

{$IFOPT Q+}{$DEFINE RESTORE_OVERFLOWCHECKS}{$Q-}{$ENDIF}
{$IFOPT R+}{$DEFINE RESTORE_RANGECHECKS}{$R-}{$ENDIF}

var
  /// <summary>
  ///   A sequence of values which over time will be random by replacing each
  ///   value with a derived value generated by applying the hash algorithm.
  /// </summary>
  FRegister: array[0..127] of Byte;
  /// <summary>
  ///   The hash used to generate derived values stored in FRegister is calculated
  ///   using this counter as input and this counter additionaly defines the index
  ///   in FRegister where the value will be stored. The counter can assume higher
  ///   values than the lngth of FRegister. The index calculation takes this into
  ///   account.
  /// </summary>
  FCounter: Cardinal;
  /// <summary>
  ///   Object instance for the hash generation algorithm used. The object is
  ///   created the first time it is needed and freed in finalization of this unit.
  /// </summary>
  FHash: TDECHash = nil;

  /// <summary>
  ///   Seed value, stores the last generated random number as start value for
  ///   the next randum number generation
  /// </summary>
  FRndSeed: Cardinal = 0;

function RandomSystemTime: Int64;
type
  TInt64Rec = packed record
    Lo, Hi: UInt32;
  end;
var
  {$IF defined(MSWINDOWS) and not defined(DELPHI_2010_UP)}
  SysTime: TSystemTime;
  {$ELSE}
  Hour, Minute, Second, Milliseconds: Word;
  {$IFEND}
  Counter: TInt64Rec;
  Time: Cardinal;
begin
  {$IF defined(MSWINDOWS) and not defined(DELPHI_2010_UP)}
  GetSystemTime(SysTime);
  Time := ((Cardinal(SysTime.wHour) * 60 + SysTime.wMinute) * 60 + SysTime.wSecond) * 1000 + SysTime.wMilliseconds;
  QueryPerformanceCounter(Int64(Counter));
  {$ELSE}
  DecodeTime(Now, Hour, Minute, Second, Milliseconds);
  Time := ((Cardinal(Hour) * 60 + Minute) * 60 + Second) * 1000 + Milliseconds;
    {$IFDEF DELPHI_2010_UP}
    Int64(Counter) := TStopWatch.GetTimeStamp; // uses System.Diagnostics
    {$ELSE}
      {$IFDEF FPC}
      Int64(Counter) := LclIntf.GetTickCount * 10000 {TicksPerMillisecond}; // uses LclIntf
      {$ENDIF}
    {$ENDIF}
  {$IFEND}

  Result := Time + Counter.Hi;
  Inc(Result, Ord(Result < Time)); // add "carry flag"
  Inc(Result, Counter.Lo);
end;

/// <summary>
///   Simplistic algorithm for filling a buffer with random numbers. This
///   algorithm is directly dependant on the seed passed, which by internal use
///   will normally be FRndSeed.
/// </summary>
/// <param name="Seed">
///   Seed value as starting value
/// </param>
/// <param name="Buffer">
///   Buffer which shall be filled with random bytes
/// </param>
/// <param name="Size">
///   Size of the buffer in byte
/// </param>
/// <returns>
///   New seed value after calculating the random number for the last byte in
///   the buffer.
/// </returns>
function DoRndBuffer(Seed: Cardinal; out Buffer; Size: Integer): Cardinal;
// comparable to Delphi Random() function
var
  P: PByte;
begin
  Result := Seed;
  P := @Buffer;
  if P <> nil then
  begin
    while Size > 0 do
    begin
      Result := Result * $08088405 + 1;
      P^ := Byte(Result shr 24);
      Inc(P);
      Dec(Size);
    end;
  end;
end;

procedure RandomBuffer(out Buffer; Size: Integer);
begin
  if Assigned(DoRandomBuffer) then
    DoRandomBuffer(Buffer, Size)
  else
    FRndSeed := DoRndBuffer(FRndSeed, Buffer, Size);
end;

function RandomBytes(Size: Integer): TBytes;
begin
  SetLength(Result, Size);
  RandomBuffer(Result[0], Size);
end;

function RandomRawByteString(Size: Integer): RawByteString;
begin
  SetLength(Result, Size);
  {$IF CompilerVersion >= 24.0}
  RandomBuffer(Result[Low(Result)], Size);
  {$ELSE}
  RandomBuffer(Result[1], Size);
  {$IFEND}
end;

function RandomLong: UInt32;
begin
  RandomBuffer(Result, SizeOf(Result));
end;

procedure RandomSeed(const Buffer; Size: Integer);
begin
  if Assigned(DoRandomSeed) then
    DoRandomSeed(Buffer, Size)
  else
  begin
    if Size >= 0 then
    begin
      FRndSeed := 0;
      while Size > 0 do
      begin
        Dec(Size);
        FRndSeed := (FRndSeed shl 8 + FRndSeed shr 24) xor TByteArray(Buffer)[Size]
      end;
    end
    else
      FRndSeed := RandomSystemTime;
  end;
end;

procedure RandomSeed;
begin
  RandomSeed('', -1);
end;

/// <summary>
///   Generate one random byte and modify FCounter and FRegister
/// </summary>
function DoGenerateRandomByte: Byte;
begin
  if FHash = nil then
    FHash := RandomClass.Create;

  FHash.Init;
  FHash.Calc(FCounter, SizeOf(FCounter));
  FHash.Calc(FRegister, SizeOf(FRegister));
  FHash.Done;

  FRegister[FCounter mod SizeOf(FRegister)] := FRegister[FCounter mod SizeOf(FRegister)] xor FHash.DigestAsBytes[0];
  Inc(FCounter);

  Result := FHash.DigestAsBytes[1]; // no real predictable dependency to above FHash.Digest[0] !
end;

procedure DoBuffer(out Buffer; Size: Integer);
var
  i: Integer;
begin
  for i := 0 to Size - 1 do
    TByteArray(Buffer)[i] := DoGenerateRandomByte;
end;

/// <summary>
///   Initializes a repeatable or a non repeatable seed, depending on the
///   parameters specified
/// </summary>
/// <param name="Buffer">
///   If a repeatable seed is to be initialized, the contents of this buffer is
///   a parameter to the seed generation and a buffer containing at least Size
///   bytes needs to be passed.
/// </param>
/// <param name="Size">
///   If Size is >= 0 a repeatable seed is initialized, otherwise a non repeatable
///   based on system time
/// </param>
procedure DoSeed(const Buffer; Size: Integer);
var
  i: Integer;
  t: Cardinal;
begin
  if Size >= 0 then
  begin
    // initalize a repeatable Seed
    FillChar(FRegister, SizeOf(FRegister), 0);
    FCounter := 0;
    for i := 0 to Size - 1 do
      FRegister[i mod SizeOf(FRegister)] := FRegister[i mod SizeOf(FRegister)] xor TByteArray(Buffer)[i];
  end
  else
  begin
    // ! ATTENTION !
    // Initalizes a non-repeatable Seed based on Timers, which is not secure
    // and inpredictable. The user should call RandomSeed(Data, SizeOf(Data))
    // instead, where Date contains i.e. user generated (Human) input.
    t := RandomSystemTime;
    for i := Low(FRegister) to High(FRegister) do
    begin
      FRegister[i] := FRegister[i] xor Byte(t);
      t := t shl 1 or t shr 31;
    end;
  end;
  for i := Low(FRegister) to High(FRegister) do
    DoGenerateRandomByte;
  FCounter := 0;
end;

procedure DoInit;
begin
  DoRandomBuffer := DoBuffer;
  DoRandomSeed := DoSeed;
  DoSeed('', 0);
end;

procedure DoDone;
begin
  try
    if FHash <> nil then
      FHash.Free;
  except
  end;
  FHash := nil;
  FillChar(FRegister, SizeOf(FRegister), 0);
  FCounter := 0;
end;

{$IFDEF RESTORE_RANGECHECKS}{$R+}{$ENDIF}
{$IFDEF RESTORE_OVERFLOWCHECKS}{$Q+}{$ENDIF}

initialization
  {$DEFINE AUTO_PRNG}

  DoInit;

  {$IFDEF AUTO_PRNG} // see DECOptions.inc
  RandomSeed;
  {$ENDIF AUTO_PRNG}

finalization
  DoDone;

end.
