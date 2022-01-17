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
///   Most simple demonstration of using a DEC cipher
/// </summary>
program Cipher_Console;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  DECCipherBase,
  DECCipherModes,
  DECCipherFormats,
  DECCiphers;

var
  Cipher     : TCipher_1DES;
  // We use raw byte string here since Unicode handling of Windows console
  // is not given
  SourceText : RawByteString;
  // Key for the initialization of our encryption run
  CipherKey  : RawByteString;
  IV         : RawByteString;
  Input,
  Output     : TBytes;
  i          : Integer;
begin
  Cipher := TCipher_1DES.Create;

  try
    try
      // Init our encryption
      CipherKey := 'Passwort';
      IV := #0#0#0#0#0#0#0#0;
      Cipher.Init(CipherKey, IV, 0);
      Cipher.Mode := cmCBCx;

      SourceText := 'Beispielklartext';
      WriteLn('Source text: ' + SourceText);
      Input := System.SysUtils.BytesOf(SourceText);

      // Encrypt
      Output := Cipher.EncodeBytes(Input);

      Write('Encrypted data in hex: ');
      for i := 0 to high(Output) do
        Write(IntToHex(Output[i], 2), ' ');

      WriteLn;

      // Decrypt
      Cipher.Init(CipherKey, IV, 0);
      Output := Cipher.DecodeBytes(Output);

      SourceText := RawByteString(System.SysUtils.StringOf(Output));

      WriteLn('Decrypted data: ' + SourceText);

      // Show that using a different key results in a different output
      WriteLn;

      CipherKey := 'Password';
      Cipher.Init(CipherKey, IV, 0);
      Output := Cipher.DecodeBytes(Output);

      SourceText := RawByteString(System.SysUtils.StringOf(Output));

      WriteLn('Decrypted with different key: ' + SourceText);

      ReadLn;
    except
      on E: Exception do
        Writeln(E.ClassName, ': ', E.Message);
    end;
  finally
    Cipher.Free;
  end;
end.
