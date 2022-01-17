{*****************************************************************************
  The DEC team (see file NOTICE.txt) licenses this file
  to you under the Apache License, Version 2.0 (the
  "License"); you may not use this file except in compliance
  with the License. A copy of this licence is found in the root directory of
  this project in the file LICENCE.txt or alternatively at

    http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing,
  software distributed under the License is distributed on an
  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied.  See the License for the
  specific language governing permissions and limitations
  under the License.
*****************************************************************************}

{$M+} // DUnitX would add it anyway
unit TestDECRandom;

interface

// Needs to be included before any other statements
{$INCLUDE TestDefines.inc}

uses
  System.SysUtils, System.Classes,
  {$IFDEF DUnitX}
  DUnitX.TestFramework,DUnitX.DUnitCompatibility,
  {$ELSE}
  TestFramework,
  {$ENDIF}
  DECRandom;

type
  /// <summary>
  ///   This can only implement a rough test of the default behavior of the PNRG.
  ///   We do not have access to the default seed value generation functions etc.
  ///   so we cannod test RandomSeed properly as we could provide our own
  ///   implementation of the default seed generators but couldn't resore the
  ///   default ones afterwards, which should be done to enable repeated test
  ///   runs in the same session.
  /// </summary>
  TTestRandom = class(TTestCase)
  private
    RandomNumbers: TBytes;
  public
  published
    procedure TestRandomLong;
    procedure TestRandomBytes;
    procedure TestRandomBuffer;
    procedure TestRandomBufferIncompletelyFilled;
  end;

implementation

{ TTestRandom }

procedure TTestRandom.TestRandomBuffer;
var
  Result : TBytes;
  Expected : TBytes;
  i : Integer;
begin
  // Set up the seed with a known value of 0 so always the same known sequence
  // results
  RandomSeed(RandomNumbers, 0);
  SetLength(Result, 5);
  FillChar(Result[0], 5, 0);

  RandomBuffer(Result[0], 5);

  Expected := TBytes.Create(208, 208, 25, 65, 118);

  for i := Low(Expected) to High(Expected) do
    CheckEquals(Expected[i], Result[i],
                'Wrong random number in known sequence at index ' + IntToStr(i));

  SetLength(Result, 5);
  FillChar(Result[0], 5, 0);

  RandomBuffer(Result[0], 5);

  Expected := TBytes.Create(107, 181, 127, 194, 179);

  for i := Low(Expected) to High(Expected) do
    CheckEquals(Expected[i], Result[i],
                'Wrong random number in known sequence at index ' + IntToStr(i));
end;

procedure TTestRandom.TestRandomBufferIncompletelyFilled;
var
  Result : TBytes;
  Expected : TBytes;
  i : Integer;
begin
  // Set up the seed with a known value of 0 so always the same known sequence
  // results
  RandomSeed(RandomNumbers, 0);
  SetLength(Result, 10);
  FillChar(Result[0], 5, 0);

  RandomBuffer(Result[0], 5);

  Expected := TBytes.Create(208, 208, 25, 65, 118, 0, 0, 0, 0, 0);

  for i := Low(Expected) to High(Expected) do
    CheckEquals(Expected[i], Result[i],
                'Wrong random number in known sequence at index ' + IntToStr(i));

  SetLength(Result, 10);
  FillChar(Result[0], 5, 0);

  RandomBuffer(Result[0], 5);

  Expected := TBytes.Create(107, 181, 127, 194, 179, 0, 0, 0, 0, 0);

  for i := Low(Expected) to High(Expected) do
    CheckEquals(Expected[i], Result[i],
                'Wrong random number in known sequence at index ' + IntToStr(i));
end;

procedure TTestRandom.TestRandomBytes;
var
  Result : TBytes;
  Expected : TBytes;
  i : Integer;
begin
  // Set up the seed with a known value of 0 so always the same known sequence
  // results
  RandomSeed(RandomNumbers, 0);
  Result := RandomBytes(5);

  Expected := TBytes.Create(208, 208, 25, 65, 118);

  for i := Low(Expected) to High(Expected) do
    CheckEquals(Expected[i], Result[i],
                'Wrong random number in known sequence at index ' + IntToStr(i));

  Result := RandomBytes(5);

  Expected := TBytes.Create(107, 181, 127, 194, 179);

  for i := Low(Expected) to High(Expected) do
    CheckEquals(Expected[i], Result[i],
                'Wrong random number in known sequence at index ' + IntToStr(i));
end;

procedure TTestRandom.TestRandomLong;
begin
  // Set up the seed with a known value of 0 so always the same known sequence
  // results
  RandomSeed(RandomNumbers, 0);
  CheckEquals(1092210896, RandomLong, 'Wrong random number from known sequence');
  CheckEquals(2142595958, RandomLong, 'Wrong random number from known sequence');
  CheckEquals(1475261378, RandomLong, 'Wrong random number from known sequence');
end;

initialization
  // Register any test cases with the test runner
  {$IFDEF DUnitX}
  TDUnitX.RegisterTestFixture(TTestRandom);
  {$ELSE}
  RegisterTests('DECRandom', [TTestRandom.Suite]);
  {$ENDIF}
end.
