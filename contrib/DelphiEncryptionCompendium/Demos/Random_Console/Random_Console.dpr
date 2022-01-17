{*****************************************************************************
  The DEC team (see file NOTICE.txt) licenses this file
  to you under the Apache License, Version 2.0 (the
  "License"); you may not use this file except in compliance
  with the license. A copy of this licence is found in the root directory of
  this project in the file LICENCE.txt or alternatively at

    http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing,
  software distributed under the License is distributed on an
  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied.  See the License for the
  specific language governing permissions and limitations
  under the License.
*****************************************************************************}

/// <summary>
///   Most simple demonstration of using the pseudo random number generator
/// </summary>
program Random_Console;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  DECRandom;

var
  RandomNumbers: TBytes;
  i, n : Integer;

begin
  try
    // Draw one random number each
    WriteLn('Random UInt32 1: ', RandomLong);
    WriteLn('Random UInt32 2: ', RandomLong);
    WriteLn;

    WriteLn('Get a buffer of random numbers');

    for n := 1 to 2 do
    begin
      // Get a buffer full of random bytes
      RandomNumbers := RandomBytes(5);

      for i := Low(RandomNumbers) to High(RandomNumbers) do
        WriteLn('Random number ', i, ' ', RandomNumbers[i]);

      WriteLn;
    end;

    WriteLn('Fill existing buffer');
    RandomBuffer(RandomNumbers[0], length(RandomNumbers));

    for i := Low(RandomNumbers) to High(RandomNumbers) do
      WriteLn('Random number ', i, ' ', RandomNumbers[i]);

    WriteLn;

    WriteLn('The way we initialize the seed now we will always get the ');
    WriteLn('same random number 1092210896 - so this is not recommended!');

    RandomSeed(RandomNumbers, 0);
    WriteLn('Random UInt32 1: ', RandomLong);

    ReadLn;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
