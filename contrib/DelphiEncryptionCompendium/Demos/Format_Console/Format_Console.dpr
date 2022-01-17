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
///   Most simple demonstration of DEC formatting routines
/// </summary>
program Format_Console;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  DECFormat;

var
  s, s1 : string;

begin
  try
    s  := 'Hello world!';
    // Convert the string to be encoded in a byte array
    // and te result into a string for output
    s1 := System.SysUtils.StringOf(TFormat_HEX.Encode(System.SysUtils.BytesOf(s)));
    WriteLn(s + ' encoded in hex is: ' + s1);

    // the same for decoding
    WriteLn('Hex ' + s1 + ' is ' +
      System.SysUtils.StringOf(TFormat_HEX.Decode(System.SysUtils.BytesOf(s1))) +
      ' unencoded');
    ReadLn;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
