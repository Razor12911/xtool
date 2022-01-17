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

/// <summary>
///   Most simple demonstration of DEC hash routines
/// </summary>
program Hash_Console;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  DECFormat,
  DECHash;

var
  Hash : THash_RipeMD160;
  s    : RawByteString;

  W: THash_Whirlpool1;
begin
  Hash := THash_RipeMD160.Create;
  try
    try
      // Calculate a hash value in accordance to the original author's test data
      // http://homes.esat.kuleuven.be/~bosselae/ripemd160.html
      s := 'message digest';

      WriteLn('RipeMD160 digest (hash value) of ' + s + ' is ' + sLineBreak +
              Hash.CalcString(s, TFormat_HEX));

      ReadLn;

      W := THash_Whirlpool1.Create;

      s := 'The quick brown fox jumps over the lazy dog';

      WriteLn('RipeMD160 digest (hash value) of ' + s + ' is ' + sLineBreak +
              W.CalcString(s, TFormat_HEX));
      W.Free;
      ReadLn;
    except
      on E: Exception do
        Writeln(E.ClassName, ': ', E.Message);
    end;
  finally
    Hash.Free;
  end;
end.
