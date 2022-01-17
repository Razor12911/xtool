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
unit TestDECHashMAC;

interface

// Needs to be included before any other statements
{$INCLUDE TestDefines.inc}
{$INCLUDE ..\..\Source\DECOptions.inc}

uses
  System.Classes, System.SysUtils, Generics.Collections,
  {$IFDEF DUnitX}
  DUnitX.TestFramework, DUnitX.DUnitCompatibility,
  {$ELSE}
  TestFramework,
  {$ENDIF}
  TestDECTestDataContainer, DECBaseClass, DECHash, DECHashBase;

type
  {$IFDEF DUnitX} [TestFixture] {$ENDIF}
  /// <summary>
  ///   All test cases for the HMAC class methods.
  /// </summary>
  TestTHash_HMAC = class(TTestCase)
  published
    procedure TestBytes;
    procedure TestRawByteString;
    procedure TestAAx80StringBytes;
    procedure TestAAx80StringString;
  end;

  {$IFDEF DUnitX} [TestFixture] {$ENDIF}
  /// <summary>
  ///   All test cases for the HMAC class methods.
  /// </summary>
  TestTHash_PBKDF2 = class(TTestCase)
  published
    procedure TestBytes;
    procedure TestRawByteString;
  end;

implementation

uses
  DECFormatBase, DECFormat;

{ TestTHash_HMAC }

procedure TestTHash_HMAC.TestAAx80StringBytes;
var
  AAx80String : TBytes;
  Result      : TBytes;
begin
  SetLength(AAx80String, 80);
  FillChar(AAx80String[0], 80, $AA);

  Result := THash_MD5.HMAC(AAx80String,
                           BytesOf('Test Using Larger Than Block-Size Key and Larger Than One Block-Size Data'));
  CheckEquals('6F630FAD67CDA0EE1FB1F562DB3AA53E', StringOf(ValidFormat(TFormat_HEX).Encode(Result)),
              'Test Using Larger Than Block-Size Key and Larger Than One Block-Size Data');

  Result := THash_MD5.HMAC(AAx80String, BytesOf('Test Using Larger Than Block-Size Key - Hash Key First'));
  CheckEquals('6B1AB7FE4BD7BF8F0B62E6CE61B9D0CD', StringOf(ValidFormat(TFormat_HEX).Encode(Result)),
              'MD5 Test Using Larger Than Block-Size Key - Hash Key First');

  Result := THash_SHA1.HMAC(AAx80String,
                            BytesOf('Test Using Larger Than Block-Size Key and Larger Than One Block-Size Data'));
  CheckEquals('E8E99D0F45237D786D6BBAA7965C7808BBFF1A91', StringOf(ValidFormat(TFormat_HEX).Encode(Result)),
              'SHA1 Test Using Larger Than Block-Size Key and Larger Than One Block-Size Data');

  Result := THash_SHA1.HMAC(AAx80String,
                            BytesOf('Test Using Larger Than Block-Size Key - Hash Key First'));
  CheckEquals('AA4AE5E15272D00E95705637CE8A3B55ED402112', StringOf(ValidFormat(TFormat_HEX).Encode(Result)),
              'SHA1 Test Using Larger Than Block-Size Key - Hash Key First');
end;

procedure TestTHash_HMAC.TestAAx80StringString;
var
  AAx80String : TBytes;
  Result      : TBytes;
begin
  SetLength(AAx80String, 80);
  FillChar(AAx80String[0], 80, $AA);

  Result := THash_MD5.HMAC(RawByteString(StringOf(AAx80String)),
                           RawByteString('Test Using Larger Than Block-Size Key and Larger Than One Block-Size Data'));
  CheckEquals('6F630FAD67CDA0EE1FB1F562DB3AA53E', StringOf(ValidFormat(TFormat_HEX).Encode(Result)),
              'Test Using Larger Than Block-Size Key and Larger Than One Block-Size Data');

  Result := THash_MD5.HMAC(RawByteString(StringOf(AAx80String)),
              RawByteString('Test Using Larger Than Block-Size Key - Hash Key First'));
  CheckEquals('6B1AB7FE4BD7BF8F0B62E6CE61B9D0CD', StringOf(ValidFormat(TFormat_HEX).Encode(Result)),
              'MD5 Test Using Larger Than Block-Size Key - Hash Key First');

  Result := THash_SHA1.HMAC(RawByteString(StringOf(AAx80String)),
                            RawByteString('Test Using Larger Than Block-Size Key and Larger Than One Block-Size Data'));
  CheckEquals('E8E99D0F45237D786D6BBAA7965C7808BBFF1A91', StringOf(ValidFormat(TFormat_HEX).Encode(Result)),
              'SHA1 Test Using Larger Than Block-Size Key and Larger Than One Block-Size Data');

  Result := THash_SHA1.HMAC(RawByteString(StringOf(AAx80String)),
                            RawByteString('Test Using Larger Than Block-Size Key - Hash Key First'));
  CheckEquals('AA4AE5E15272D00E95705637CE8A3B55ED402112', StringOf(ValidFormat(TFormat_HEX).Encode(Result)),
              'SHA1 Test Using Larger Than Block-Size Key - Hash Key First');
end;

procedure TestTHash_HMAC.TestBytes;
var
  Res: TBytes;
begin
  // test vectors from https://en.wikipedia.org/wiki/HMAC
  Res := THash_MD5.HMAC(BytesOf('key'), BytesOf('The quick brown fox jumps over the lazy dog'));
  CheckEquals('80070713463E7749B90C2DC24911E275', StringOf(ValidFormat(TFormat_HEX).Encode(Res)),
              'MD5 failure in The quick...');

  Res := THash_SHA1.HMAC(BytesOf('key'), BytesOf('The quick brown fox jumps over the lazy dog'));
  CheckEquals('DE7C9B85B8B78AA6BC8A7A36F70A90701C9DB4D9', StringOf(ValidFormat(TFormat_HEX).Encode(Res)),
              'SHA1 failure in The quick...');

  Res := THash_SHA256.HMAC(BytesOf('key'), BytesOf('The quick brown fox jumps over the lazy dog'));
  CheckEquals('F7BC83F430538424B13298E6AA6FB143EF4D59A14946175997479DBC2D1A3CD8',
              StringOf(ValidFormat(TFormat_HEX).Encode(Res)),
              'SHA256 failure in The quick...');
end;

procedure TestTHash_HMAC.TestRawByteString;
var
  Result: TBytes;
begin
  // test vectors from https://en.wikipedia.org/wiki/HMAC
  Result := THash_MD5.HMAC(RawByteString('key'), RawByteString('The quick brown fox jumps over the lazy dog'));
  CheckEquals('80070713463E7749B90C2DC24911E275', StringOf(ValidFormat(TFormat_HEX).Encode(Result)),
              'MD5 failure in The quick...');

  Result := THash_SHA1.HMAC(RawByteString('key'), RawByteString('The quick brown fox jumps over the lazy dog'));
  CheckEquals('DE7C9B85B8B78AA6BC8A7A36F70A90701C9DB4D9', StringOf(ValidFormat(TFormat_HEX).Encode(Result)),
              'SHA1 failure in The quick...');

  Result := THash_SHA256.HMAC(RawByteString('key'), RawByteString('The quick brown fox jumps over the lazy dog'));
  CheckEquals('F7BC83F430538424B13298E6AA6FB143EF4D59A14946175997479DBC2D1A3CD8',
              StringOf(ValidFormat(TFormat_HEX).Encode(Result)),
              'SHA256 failure in The quick...');
end;

{ TestTHash_PBKDF2 }

procedure TestTHash_PBKDF2.TestBytes;
var
  Result : string;
begin
  result := StringOf(ValidFormat(TFormat_HEX).Encode(THash_SHA1.PBKDF2(BytesOf('password'), BytesOf('salt'), 1, 20)));
  CheckEquals('0C60C80F961F0E71F3A9B524AF6012062FE037A6', result, 'SHA1 password salt 1 20');

  result := StringOf(ValidFormat(TFormat_HEX).Encode(THash_SHA256.PBKDF2(BytesOf('password'), BytesOf('salt'), 1, 32)));
  CheckEquals('120FB6CFFCF8B32C43E7225256C4F837A86548C92CCC35480805987CB70BE17B', result, 'SHA256 password salt 1 32');

  result := StringOf(ValidFormat(TFormat_HEX).Encode(THash_SHA256.PBKDF2(BytesOf('password'), BytesOf('salt'), 2, 32)));
  CheckEquals('AE4D0C95AF6B46D32D0ADFF928F06DD02A303F8EF3C251DFD6E2D85A95474C43', result, 'SHA256 password salt 2 32');

  result := StringOf(ValidFormat(TFormat_HEX).Encode(THash_SHA256.PBKDF2(BytesOf('password'), BytesOf('salt'), 4096, 32)));
  CheckEquals('C5E478D59288C841AA530DB6845C4C8D962893A001CE4E11A4963873AA98134A', result, 'SHA256 password salt 4096 32');

  // PBKDF2-HMAC-SHA512  test vectors from https://stackoverflow.com/questions/15593184/pbkdf2-hmac-sha-512-test-vectors﻿
  result := StringOf(ValidFormat(TFormat_HEX).Encode(THash_SHA512.PBKDF2(BytesOf('password'), BytesOf('salt'), 1, 64)));
  CheckEquals('867F70CF1ADE02CFF3752599A3A53DC4AF34C7A669815AE5D513554E1C8CF252C02D470A285A0501BAD999BFE943C08F050235D7D68B1DA55E63F73B60A57FCE',
              result, 'SHA512 password salt 1 64');

  result := StringOf(ValidFormat(TFormat_HEX).Encode(THash_SHA512.PBKDF2(BytesOf('password'), BytesOf('salt'), 2, 64)));
  CheckEquals('E1D9C16AA681708A45F5C7C4E215CEB66E011A2E9F0040713F18AEFDB866D53CF76CAB2868A39B9F7840EDCE4FEF5A82BE67335C77A6068E04112754F27CCF4E',
              result, 'SHA512 password salt 2 64');

  result := StringOf(ValidFormat(TFormat_HEX).Encode(THash_SHA512.PBKDF2(BytesOf('password'), BytesOf('salt'), 4096, 64)));
  CheckEquals('D197B1B33DB0143E018B12F3D1D1479E6CDEBDCC97C5C0F87F6902E072F457B5143F30602641B3D55CD335988CB36B84376060ECD532E039B742A239434AF2D5',
              result, 'SHA512 password salt 4096 64');

  result := StringOf(ValidFormat(TFormat_HEX).Encode(THash_SHA256.PBKDF2(BytesOf('password'), BytesOf('salt'), 1000000, 32)));
  CheckEquals('505112A590BE61AC9D3A235BF0A8EECEA40E54652EC0E3C257C227C9AA5E664C',
              result, 'SHA256 password salt 1000000 32');
end;

procedure TestTHash_PBKDF2.TestRawByteString;
var
  Result : string;
begin
  result := StringOf(ValidFormat(TFormat_HEX).Encode(THash_SHA1.PBKDF2('password', 'salt', 1, 20)));
  CheckEquals('0C60C80F961F0E71F3A9B524AF6012062FE037A6', result, 'SHA1 password salt 1 20');

  result := StringOf(ValidFormat(TFormat_HEX).Encode(THash_SHA256.PBKDF2('password', 'salt', 1, 32)));
  CheckEquals('120FB6CFFCF8B32C43E7225256C4F837A86548C92CCC35480805987CB70BE17B', result, 'SHA256 password salt 1 32');

  result := StringOf(ValidFormat(TFormat_HEX).Encode(THash_SHA256.PBKDF2('password', 'salt', 2, 32)));
  CheckEquals('AE4D0C95AF6B46D32D0ADFF928F06DD02A303F8EF3C251DFD6E2D85A95474C43', result, 'SHA256 password salt 2 32');

  result := StringOf(ValidFormat(TFormat_HEX).Encode(THash_SHA256.PBKDF2('password', 'salt', 4096, 32)));
  CheckEquals('C5E478D59288C841AA530DB6845C4C8D962893A001CE4E11A4963873AA98134A', result, 'SHA256 password salt 4096 32');

  // PBKDF2-HMAC-SHA512  test vectors from https://stackoverflow.com/questions/15593184/pbkdf2-hmac-sha-512-test-vectors﻿
  result := StringOf(ValidFormat(TFormat_HEX).Encode(THash_SHA512.PBKDF2('password', 'salt', 1, 64)));
  CheckEquals('867F70CF1ADE02CFF3752599A3A53DC4AF34C7A669815AE5D513554E1C8CF252C02D470A285A0501BAD999BFE943C08F050235D7D68B1DA55E63F73B60A57FCE',
              result, 'SHA512 password salt 1 64');

  result := StringOf(ValidFormat(TFormat_HEX).Encode(THash_SHA512.PBKDF2('password', 'salt', 2, 64)));
  CheckEquals('E1D9C16AA681708A45F5C7C4E215CEB66E011A2E9F0040713F18AEFDB866D53CF76CAB2868A39B9F7840EDCE4FEF5A82BE67335C77A6068E04112754F27CCF4E',
              result, 'SHA512 password salt 2 64');

  result := StringOf(ValidFormat(TFormat_HEX).Encode(THash_SHA512.PBKDF2('password', 'salt', 4096, 64)));
  CheckEquals('D197B1B33DB0143E018B12F3D1D1479E6CDEBDCC97C5C0F87F6902E072F457B5143F30602641B3D55CD335988CB36B84376060ECD532E039B742A239434AF2D5',
              result, 'SHA512 password salt 4096 64');

  result := StringOf(ValidFormat(TFormat_HEX).Encode(THash_SHA256.PBKDF2('password', 'salt', 1000000, 32)));
  CheckEquals('505112A590BE61AC9D3A235BF0A8EECEA40E54652EC0E3C257C227C9AA5E664C',
              result, 'SHA256 password salt 1000000 32');
end;

initialization
  // Register any test cases with the test runner
  {$IFDEF DUnitX}
  TDUnitX.RegisterTestFixture(TestTHash_HMAC);
  TDUnitX.RegisterTestFixture(TestTHash_PBKDF2);
  {$ELSE}
  RegisterTests('DECHashHMAC', [TestTHash_HMAC.Suite]);
  RegisterTests('DECHashHMAC', [TestTHash_PBKDF2.Suite]);
  {$ENDIF}
end.
