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
unit TestDECFormat;

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
  DECBaseClass, DECUtil, DECFormat, DECFormatBase;

type
  /// <summary>
  ///   Type needed for the EncodeBytes und DecodeBytes test data definition
  /// </summary>
  TestRecRawByteString = record
    Input, Output: RawByteString;
  end;

  /// <summary>
  ///   Type used for some IsValid test data lists
  /// </summary>
  TestRecIsValidRawByteString = record
    Input  : RawByteString;
    Result : Boolean;
  end;

  /// <summary>
  ///   Type needed for passing the right encode or decode method to the
  ///   generic encode/decode test
  /// </summary>
  TEncodeDecodeProc = function(const data: TBytes):TBytes of Object;
  /// <summary>
  ///   Type needed for passing the right encode or decode method to the
  ///   generic encode/decode test
  /// </summary>
  TEncodeDecodeProcRawByteString = function(const data: RawByteString):RawByteString of Object;
  /// <summary>
  ///   Type needed for passing the right encode or decode method to the
  ///   generic encode/decode test
  /// </summary>
  TEncodeDecodeProcTypeless = function(const Data; Size: Integer): RawByteString of Object;

  // Basic test implementations to be shared by the individual formatting class
  // test classes to enable easier DUnit and DUnitX compatibility
  TFormatTestsBase = class(TTestCase)
  strict protected
    procedure DoTestEncodeDecodeTypeless(
      EncodeDecodeProc: TEncodeDecodeProcTypeless;
      TestData: array of TestRecRawByteString);

    procedure DoTestEncodeDecode(EncodeDecodeProc: TEncodeDecodeProc;
                                 TestData: array of TestRecRawByteString);

    procedure DoTestEncodeDecodeRawByteString(
      EncodeDecodeProc: TEncodeDecodeProcRawByteString;
      TestData: array of TestRecRawByteString);
  end;

  // Test methods for class TFormat_HEX
  {$IFDEF DUnitX} [TestFixture] {$ENDIF}
  TestTFormat_HEX = class(TFormatTestsBase)
  strict private
    FFormat_HEX: TFormat_HEX;

    const
      cTestDataEncode : array[1..2] of TestRecRawByteString = (
        (Input:  RawByteString('');
         Output: ''),
        (Input:  RawByteString('Test'+#10+#9+#$AA+#$55+#$AB+#$CD+#$EF);
         Output: '546573740A09AA55ABCDEF'));

      cTestDataDecode : array[1..2] of TestRecRawByteString = (
        (Input:  RawByteString('');
         Output: ''),
        (Input:  '546573740A09AA55ABCDEF';
         Output: RawByteString('Test'+#10+#9+#$AA+#$55+#$AB+#$CD+#$EF)));
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestEncodeBytes;
    procedure TestEncodeRawByteString;
    procedure TestEncodeTypeless;
    procedure TestDecodeBytes;
    procedure TestDecodeRawByteString;
    procedure TestDecodeTypeless;
    procedure TestIsValidTypeless;
    procedure TestIsValidTBytes;
    procedure TestIsValidRawByteString;
    procedure TestClassByName;
    procedure TestIdentity;
  end;

  // Test methods for class TFormat_HEXL
  {$IFDEF DUnitX} [TestFixture] {$ENDIF}
  TestTFormat_HEXL = class(TFormatTestsBase)
  strict private
    FFormat_HEXL: TFormat_HEXL;

    const
      cTestDataEncode : array[1..2] of TestRecRawByteString = (
        (Input:  RawByteString('');
         Output: ''),
        (Input:  RawByteString('Test'+#10+#9+#$AA+#$55+#$AB+#$CD+#$EF);
         Output: '546573740a09aa55abcdef'));

      cTestDataDecode : array[1..2] of TestRecRawByteString = (
        (Input:  RawByteString('');
         Output: ''),
        (Input:  '546573740a09aa55abcdef';
         Output: RawByteString('Test'+#10+#9+#$AA+#$55+#$AB+#$CD+#$EF)));
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestEncodeBytes;
    procedure TestEncodeRawByteString;
    procedure TestEncodeTypeless;
    procedure TestDecodeBytes;
    procedure TestDecodeRawByteString;
    procedure TestDecodeTypeless;
    procedure TestIsValidTypeless;
    procedure TestIsValidTBytes;
    procedure TestIsValidRawByteString;
    procedure TestClassByName;
    procedure TestIdentity;
  end;

  // Test methods for class TFormat_DECMIME32
  {$IFDEF DUnitX} [TestFixture] {$ENDIF}
  TestTFormat_DECMIME32 = class(TFormatTestsBase)
  strict private
    FFormat_DECMIME32: TFormat_DECMIME32;

    const
      cTestDataEncode : array[1..6] of TestRecRawByteString = (
        (Input:  RawByteString('');
         Output: ''),
        (Input:  RawByteString('Test' + #10 +#9 + #$AA + #$55);
         Output: 'xk3gh4jbjsklf'),
        (Input:  RawByteString('Test' + #10 +#9 + #$AA + #$55 + #$AA);
         Output: 'xk3gh4jbjsklfyc'),
        (Input:  RawByteString('Test' + #10 +#9 + #$AA + #$55 + #$AA + #$55);
         Output: 'xk3gh4jbjsklfyzk'),
        (Input:  RawByteString('Test' + #10 +#9 + #$AA + #$55 + #$AA + #$55 + #$AA);
         Output: 'xk3gh4jbjsklfyzkkf'),
        (Input:  RawByteString('Test' + #10 +#9 + #$AA + #$55 + #$AA + #$55 + #$AA + #$55);
         Output: 'xk3gh4jbjsklfyzkkpya')); // als letzter Buchstabe kommt manchmal oft ,u statt a heraus?
                                           // scheint etwas zufällig? Was ist da faul?
                                           // lt. DECTest.vec ist a richtig

      cTestDataDecode : array[1..6] of TestRecRawByteString = (
        (Input:  '';
         Output: RawByteString('')),
        (Input:  'xk3gh4jbjsklf'; // lt. alter DECTest.vec aber xk3gh4jbjsklf f statt y ???
         Output: RawByteString('Test' + #10 +#9 + #$AA + #$55)),
        (Input:  'xk3gh4jbjsklfyc';
         Output: RawByteString('Test' + #10 +#9 + #$AA + #$55 + #$AA)),
        (Input:  'xk3gh4jbjsklfyzk';
         Output: RawByteString('Test' + #10 +#9 + #$AA + #$55 + #$AA + #$55)),
        (Input:  'xk3gh4jbjsklfyzkkf';
         Output: RawByteString('Test' + #10 +#9 + #$AA + #$55 + #$AA + #$55 + #$AA)),
        (Input:  'xk3gh4jbjsklfyzkkpya';
         Output: RawByteString('Test' + #10 +#9 + #$AA + #$55 + #$AA + #$55 + #$AA + #$55)));
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestEncodeBytes;
    procedure TestEncodeRawByteString;
    procedure TestEncodeTypeless;
    procedure TestDecodeBytes;
    procedure TestDecodeRawByteString;
    procedure TestDecodeTypeless;
    procedure TestIsValidTypeless;
    procedure TestIsValidTBytes;
    procedure TestIsValidRawByteString;
    procedure TestClassByName;
    procedure TestIdentity;
  end;

  // Test methods for class TFormat_Base64
  {$IFDEF DUnitX} [TestFixture] {$ENDIF}
  TestTFormat_Base64 = class(TFormatTestsBase)
  strict private
    FFormat_Base64: TFormat_Base64;
    const
      cTestDataEncode : array[1..12] of TestRecRawByteString = (
        (Input:  RawByteString('');
         Output: ''),
        (Input:  RawByteString('Test' + #10 +#9 + #$AA + #$55);
         Output: 'VGVzdAoJqlU='),
        (Input:  RawByteString('Test' + #10 +#9 + #$AA + #$55 + #$AA);
         Output: 'VGVzdAoJqlWq'),
        (Input:  RawByteString('Test' + #10 +#9 + #$AA + #$55 + #$AA + #$55);
         Output: 'VGVzdAoJqlWqVQ=='),
        (Input:  RawByteString('Test' + #10 +#9 + #$AA + #$55 + #$AA + #$55 + #$AA);
         Output: 'VGVzdAoJqlWqVao='),
        (Input:  RawByteString('Test' + #10 +#9 + #$AA + #$55 + #$AA + #$55 + #$AA + #$55);
         Output: 'VGVzdAoJqlWqVapV'),
        (Input:  RawByteString('f');
         Output: 'Zg=='),
        (Input:  RawByteString('fo');
         Output: 'Zm8='),
        (Input:  RawByteString('foo');
         Output: 'Zm9v'),
        (Input:  RawByteString('foob');
         Output: 'Zm9vYg=='),
        (Input:  RawByteString('fooba');
         Output: 'Zm9vYmE='),
        (Input:  RawByteString('foobar');
         Output: 'Zm9vYmFy'));

      cTestDataDecode : array[1..12] of TestRecRawByteString = (
        (Input:  '';
         Output: RawByteString('')),
        (Input:  'VGVzdAoJqlU=';
         Output: RawByteString('Test' + #10 +#9 + #$AA + #$55)),
        (Input:  'VGVzdAoJqlWq';
         Output: RawByteString('Test' + #10 +#9 + #$AA + #$55 + #$AA)),
        (Input:  'VGVzdAoJqlWqVQ==';
         Output: RawByteString('Test' + #10 +#9 + #$AA + #$55 + #$AA + #$55)),
        (Input:  'VGVzdAoJqlWqVao=';
         Output: RawByteString('Test' + #10 +#9 + #$AA + #$55 + #$AA + #$55 + #$AA)),
        (Input:  'VGVzdAoJqlWqVapV';
         Output: RawByteString('Test' + #10 +#9 + #$AA + #$55 + #$AA + #$55 + #$AA + #$55)),
        (Input:  'Zg==';
         Output: RawByteString('f')),
        (Input: 'Zm8=';
         Output:  RawByteString('fo')),
        (Input:  'Zm9v';
         Output: RawByteString('foo')),
        (Input:  'Zm9vYg==';
         Output: RawByteString('foob')),
        (Input:  'Zm9vYmE=';
         Output: RawByteString('fooba')),
        (Input:  'Zm9vYmFy';
         Output: RawByteString('foobar')));
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestEncodeBytes;
    procedure TestEncodeRawByteString;
    procedure TestEncodeTypeless;
    procedure TestDecodeBytes;
    procedure TestDecodeRawByteString;
    procedure TestDecodeTypeless;
    procedure TestIsValidTypeless;
    procedure TestIsValidTBytes;
    procedure TestIsValidRawByteString;
    procedure TestClassByName;
    procedure TestIdentity;
  end;

  // Test methods for class TFormat_Radix64
  {$IFDEF DUnitX} [TestFixture] {$ENDIF}
  TestTFormat_Radix64 = class(TFormatTestsBase)
  strict private
    FFormat_Radix64: TFormat_Radix64;

    const
      cTestDataEncode : array[1..6] of TestRecRawByteString = (
        (Input:  RawByteString('');
         Output: RawByteString('')),
        (Input:  RawByteString('Test' + #10 +#9 + #$AA + #$55);
         Output: RawByteString('VGVzdAoJqlU=' + #13 + #10 +'=XtiM')),
        (Input:  RawByteString('Test' + #10 +#9 + #$AA + #$55 + #$AA);
         Output: RawByteString('VGVzdAoJqlWq' + #13 + #10 + '=qBH3')),
        (Input:  RawByteString('Test' + #10 +#9 + #$AA + #$55 + #$AA + #$55);
         Output: RawByteString('VGVzdAoJqlWqVQ==' + #13 + #10 + '=Rqc1')),
        (Input:  RawByteString('Test' + #10 +#9 + #$AA + #$55 + #$AA + #$55 + #$AA);
         Output: RawByteString('VGVzdAoJqlWqVao=' + #13 + #10 +'=s2dH')),
        (Input:  RawByteString('Test' + #10 +#9 + #$AA + #$55 + #$AA + #$55 + #$AA + #$55);
         Output: RawByteString('VGVzdAoJqlWqVapV' + #13 + #10 +'=WEFz')));

      cTestDataDecode : array[1..6] of TestRecRawByteString = (
        (Input:  RawByteString('');
         Output: RawByteString('')),
        (Input:  RawByteString('VGVzdAoJqlU=' + #13 + #10 +'=XtiM');
         Output: RawByteString('Test' + #10 +#9 + #$AA + #$55)),
        (Input:  RawByteString('VGVzdAoJqlWq' + #13 + #10 + '=qBH3');
         Output: RawByteString('Test' + #10 +#9 + #$AA + #$55 + #$AA)),
        (Input:  RawByteString('VGVzdAoJqlWqVQ==' + #13 + #10 + '=Rqc1');
         Output: RawByteString('Test' + #10 +#9 + #$AA + #$55 + #$AA + #$55)),
        (Input:  RawByteString('VGVzdAoJqlWqVao=' + #13 + #10 +'=s2dH');
         Output: RawByteString('Test' + #10 +#9 + #$AA + #$55 + #$AA + #$55 + #$AA)),
        (Input:  RawByteString('VGVzdAoJqlWqVapV' + #13 + #10 +'=WEFz');
         Output: RawByteString('Test' + #10 +#9 + #$AA + #$55 + #$AA + #$55 + #$AA + #$55)));
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestEncodeBytes;
    procedure TestEncodeRawByteString;
    procedure TestEncodeTypeless;
    procedure TestDecodeBytes;
    procedure TestDecodeRawByteString;
    procedure TestDecodeTypeless;
    procedure TestEncodeRawByteStringWithCharsPerLine;
    procedure TestIsValidTypeless;
    procedure TestIsValidTBytes;
    procedure TestIsValidRawByteString;
    procedure TestClassByName;
    procedure TestIdentity;
  end;

  // Test methods for class TFormat_UU
  {$IFDEF DUnitX} [TestFixture] {$ENDIF}
  TestTFormat_UU = class(TFormatTestsBase)
  strict private
    FFormat_UU: TFormat_UU;

    const
      cTestDataEncode : array[1..6] of TestRecRawByteString = (
        (Input:  RawByteString('');
         Output: ''),
        (Input:  RawByteString('Test' + #10 +#9 + #$AA + #$55);
         Output: '(5&5S=`H)JE4`'),
        (Input:  RawByteString('Test' + #10 +#9 + #$AA + #$55 + #$AA);
         Output: ')5&5S=`H)JE6J'),
        (Input:  RawByteString('Test' + #10 +#9 + #$AA + #$55 + #$AA + #$55);
         Output: '*5&5S=`H)JE6J50``'),
        (Input:  RawByteString('Test' + #10 +#9 + #$AA + #$55 + #$AA + #$55 + #$AA);
         Output: '+5&5S=`H)JE6J5:H`'),
        (Input:  RawByteString('Test' + #10 +#9 + #$AA + #$55 + #$AA + #$55 + #$AA + #$55);
         Output: ',5&5S=`H)JE6J5:I5'));

      cTestDataDecode : array[1..6] of TestRecRawByteString = (
        (Input:  '';
         Output: RawByteString('')),
        (Input:  RawByteString('(5&5S=`H)JE4`');
         Output: RawByteString('Test' + #10 +#9 + #$AA + #$55)),
        (Input:  RawByteString(')5&5S=`H)JE6J');
         Output: RawByteString('Test' + #10 +#9 + #$AA + #$55 + #$AA)),
        (Input:  RawByteString('*5&5S=`H)JE6J50``');
         Output: RawByteString('Test' + #10 +#9 + #$AA + #$55 + #$AA + #$55)),
        (Input:  RawByteString('+5&5S=`H)JE6J5:H`');
         Output: RawByteString('Test' + #10 +#9 + #$AA + #$55 + #$AA + #$55 + #$AA)),
        (Input:  RawByteString(',5&5S=`H)JE6J5:I5');
         Output: RawByteString('Test' + #10 +#9 + #$AA + #$55 + #$AA + #$55 + #$AA + #$55)));
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestEncodeBytes;
    procedure TestEncodeRawByteString;
    procedure TestEncodeTypeless;
    procedure TestDecodeBytes;
    procedure TestDecodeRawByteString;
    procedure TestDecodeTypeless;
    procedure TestIsValidTypeless;
    procedure TestIsValidTBytes;
    procedure TestIsValidRawByteString;
    procedure TestClassByName;
    procedure TestIdentity;
  end;

  // Test methods for class TFormat_XX
  {$IFDEF DUnitX} [TestFixture] {$ENDIF}
  TestTFormat_XX = class(TFormatTestsBase)
  strict private
    FFormat_XX: TFormat_XX;

    const
      cTestDataEncode : array[1..6] of TestRecRawByteString = (
        (Input:  RawByteString('');
         Output: ''),
        (Input:  RawByteString('Test' + #10 +#9 + #$AA + #$55);
         Output: '6J4JnR+c7eZI+'),
        (Input:  RawByteString('Test' + #10 +#9 + #$AA + #$55 + #$AA);
         Output: '7J4JnR+c7eZKe'),
        (Input:  RawByteString('Test' + #10 +#9 + #$AA + #$55 + #$AA + #$55);
         Output: '8J4JnR+c7eZKeJE++'),
        (Input:  RawByteString('Test' + #10 +#9 + #$AA + #$55 + #$AA + #$55 + #$AA);
         Output: '9J4JnR+c7eZKeJOc+'),
        (Input:  RawByteString('Test' + #10 +#9 + #$AA + #$55 + #$AA + #$55 + #$AA + #$55);
         Output: 'AJ4JnR+c7eZKeJOdJ'));

      cTestDataDecode : array[1..6] of TestRecRawByteString = (
        (Input:  RawByteString('');
         Output: ''),
        (Input:  RawByteString('6J4JnR+c7eZI+');
         Output: RawByteString('Test' + #10 +#9 + #$AA + #$55)),
        (Input:  RawByteString('7J4JnR+c7eZKe');
         Output: RawByteString('Test' + #10 +#9 + #$AA + #$55 + #$AA)),
        (Input:  RawByteString('8J4JnR+c7eZKeJE++');
         Output: RawByteString('Test' + #10 +#9 + #$AA + #$55 + #$AA + #$55)),
        (Input:  RawByteString('9J4JnR+c7eZKeJOc+');
         Output: RawByteString('Test' + #10 +#9 + #$AA + #$55 + #$AA + #$55 + #$AA)),
        (Input:  RawByteString('AJ4JnR+c7eZKeJOdJ');
         Output: RawByteString('Test' + #10 +#9 + #$AA + #$55 + #$AA + #$55 + #$AA + #$55)));
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestEncodeBytes;
    procedure TestEncodeRawByteString;
    procedure TestEncodeTypeless;
    procedure TestDecodeBytes;
    procedure TestDecodeRawByteString;
    procedure TestDecodeTypeless;
    procedure TestIsValidTypeless;
    procedure TestIsValidTBytes;
    procedure TestIsValidRawByteString;
    procedure TestClassByName;
    procedure TestIdentity;
  end;

  // Test methods for class TFormat_ESCAPE
  {$IFDEF DUnitX} [TestFixture] {$ENDIF}
  TestTFormat_ESCAPE = class(TFormatTestsBase)
  strict private
    FFormat_ESCAPE: TFormat_ESCAPE;

    const
      cTestDataEncode : array[1..9] of TestRecRawByteString = (
        (Input:  '';
         Output: RawByteString('')),
        (Input:  RawByteString('Test' + #$AA + #$55);
         Output: RawByteString('Test\xAAU')),
        (Input:  RawByteString('Test' + #$80 + #$55);
         Output: RawByteString('Test\x80U')),
        (Input:  RawByteString('Test' + #$20 + #$55);
         Output: RawByteString('Test U')),
        (Input:  RawByteString('Test' + #$19 + #$55);
         Output: RawByteString('Test\x19U')),
        (Input:  RawByteString('Test' + #$07 + #$08 + #$09 + #$0A + #$0B + #$0C + #$0D + #$55);
         Output: RawByteString('Test\a\b\t\n\v\f\rU')),
        (Input:  RawByteString('Test\U');
         Output: RawByteString('Test\\U')),
        (Input:  RawByteString('Test\'+#$07 +'U');
         Output: RawByteString('Test\\\aU')),
        (Input:  RawByteString('Test"hello"U');
         Output: RawByteString('Test\"hello\"U')));

      cTestDataDecode : array[1..9] of TestRecRawByteString = (
        (Input:  '';
         Output: RawByteString('')),
        (Input:  RawByteString('Test\xAAU');
         Output: RawByteString('Test' + #$AA + #$55)),
        (Input:  RawByteString('Test\x80U');
         Output: RawByteString('Test' + #$80 + #$55)),
        (Input:  RawByteString('Test U');
         Output: RawByteString('Test' + #$20 + #$55)),
        (Input:  RawByteString('Test\x19U');
         Output: RawByteString('Test' + #$19 + #$55)),
        (Input:  RawByteString('Test\a\b\t\n\v\f\rU');
         Output: RawByteString('Test' + #$07 + #$08 + #$09 + #$0A + #$0B + #$0C + #$0D + #$55)),
        (Input:  RawByteString('Test\\U');
         Output: RawByteString('Test\U')),
        (Input:  RawByteString('Test\\\aU');
         Output: RawByteString('Test\'+#$07 +'U')),
        (Input:  RawByteString('Test\"hello\"U');
         Output: RawByteString('Test"hello"U')));

        cTestDataIsValid : array[1..18] of TestRecIsValidRawByteString = (
          (Input  : 'abcABC123';
           Result : true),
          (Input  : '\a';
           Result : true),
          (Input  : '\b';
           Result : true),
          (Input  : '\t';
           Result : true),
          (Input  : '\n';
           Result : true),
          (Input  : '\r';
           Result : true),
          (Input  : '\v';
           Result : true),
          (Input  : '\f';
           Result : true),
          (Input  : '\r\n';
           Result : true),
          (Input  : 'Data\tvalue';
           Result : true),
          (Input  : '\';
           Result : false),
          (Input  : '\q';
           Result : false),
          (Input  : '\X41';
           Result : true),
          (Input  : '\X';
           Result : false),
          (Input  : '\X9';
           Result : false),
          (Input  : '\XZZ';
           Result : false),
          (Input  : chr(6);
           Result : false),
          (Input  : chr(31);
           Result : false));
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestEncodeBytes;
    procedure TestEncodeRawByteString;
    procedure TestEncodeTypeless;
    procedure TestDecodeBytes;
    procedure TestDecodeRawByteString;
    procedure TestDecodeTypeless;
    procedure TestIsValidTypeless;
    procedure TestIsValidTBytes;
    procedure TestIsValidRawByteString;
    procedure TestClassByName;
    procedure TestIdentity;
  end;

  // Test methods for class TFormat_BigEndian16
  {$IFDEF DUnitX} [TestFixture] {$ENDIF}
  TestTFormat_BigEndian16 = class(TFormatTestsBase)
  strict private
    FFormat_BigEndian16: TFormat_BigEndian16;

    const
      cTestDataEncode : array[1..2] of TestRecRawByteString = (
        (Input:  RawByteString('');
         Output: ''),
        (Input:  RawByteString('Test'+#10+#9+#$AA+#$55+#$AB+#$CD+#$EF+'1');
         Output: RawByteString('eTts'+#9+#10+#$55+#$AA+#$CD+#$AB+'1'+#$EF)));
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestEncodeBytes;
    procedure TestEncodeRawByteString;
    procedure TestEncodeTypeless;
    procedure TestDecodeBytes;
    procedure TestDecodeRawByteString;
    procedure TestDecodeTypeless;
    procedure TestIsValidTypeless;
    procedure TestIsValidTBytes;
    procedure TestIsValidRawByteString;
    procedure TestClassByName;
    procedure TestIdentity;
  end;

  // Test methods for class TFormat_BigEndian32
  {$IFDEF DUnitX} [TestFixture] {$ENDIF}
  TestTFormat_BigEndian32 = class(TFormatTestsBase)
  strict private
    FFormat_BigEndian32: TFormat_BigEndian32;

    const
      cTestDataEncode : array[1..2] of TestRecRawByteString = (
        (Input:  RawByteString('');
         Output: ''),
        (Input:  RawByteString('Test'+#10+#9+#$AA+#$55+#$AB+#$CD+#$EF+'1');
         Output: RawByteString('tseT'+#$55+#$AA+#9+#10+'1'+#$EF+#$CD+#$AB)));
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestEncodeBytes;
    procedure TestEncodeRawByteString;
    procedure TestEncodeTypeless;
    procedure TestDecodeBytes;
    procedure TestDecodeRawByteString;
    procedure TestDecodeTypeless;
    procedure TestIsValidTypeless;
    procedure TestIsValidTBytes;
    procedure TestIsValidRawByteString;
    procedure TestClassByName;
    procedure TestIdentity;
  end;

  // Test methods for class TFormat_BigEndian64
  {$IFDEF DUnitX} [TestFixture] {$ENDIF}
  TestTFormat_BigEndian64 = class(TFormatTestsBase)
  strict private
    FFormat_BigEndian64: TFormat_BigEndian64;

    const
      cTestDataEncode : array[1..2] of TestRecRawByteString = (
        (Input:  RawByteString('');
         Output: ''),
        (Input:  RawByteString('Test'+#10+#9+#$AA+#$55+#$AB+#$CD+#$EF+'1ABCD');
         Output: RawByteString(#$55+#$AA+#9+#10+'tseTDCBA1'+#$EF+#$CD+#$AB)));
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestEncodeBytes;
    procedure TestEncodeRawByteString;
    procedure TestEncodeTypeless;
    procedure TestDecodeBytes;
    procedure TestDecodeRawByteString;
    procedure TestDecodeTypeless;
    procedure TestIsValidTypeless;
    procedure TestIsValidTBytes;
    procedure TestIsValidRawByteString;
    procedure TestClassByName;
    procedure TestIdentity;
  end;


implementation

procedure TestTFormat_HEX.SetUp;
begin
  FFormat_HEX := TFormat_HEX.Create;
end;

procedure TestTFormat_HEX.TearDown;
begin
  FFormat_HEX.Free;
  FFormat_HEX := nil;
end;

procedure TestTFormat_HEX.TestClassByName;
var
  ReturnValue : TDECFormatClass;
begin
  ReturnValue := FFormat_HEX.ClassByName('TFormat_HEX');
  CheckEquals(TFormat_HEX, ReturnValue, 'Class is not registered');
end;

procedure TestTFormat_HEX.TestDecodeBytes;
begin
  DoTestEncodeDecode(FFormat_HEX.Decode, cTestDataDecode);
end;

procedure TestTFormat_HEX.TestDecodeRawByteString;
begin
  DoTestEncodeDecodeRawByteString(FFormat_HEX.Decode, cTestDataDecode);
end;

procedure TestTFormat_HEX.TestDecodeTypeless;
begin
  DoTestEncodeDecodeTypeless(FFormat_HEX.Decode, cTestDataDecode);
end;

procedure TestTFormat_HEX.TestEncodeBytes;
begin
  DoTestEncodeDecode(FFormat_HEX.Encode, cTestDataEncode);
end;

procedure TestTFormat_HEX.TestEncodeRawByteString;
begin
  DoTestEncodeDecodeRawByteString(FFormat_HEX.Encode, cTestDataEncode);
end;

procedure TestTFormat_HEX.TestEncodeTypeless;
begin
  DoTestEncodeDecodeTypeless(FFormat_HEX.Encode, cTestDataEncode);
end;

procedure TestTFormat_HEX.TestIdentity;
begin
  CheckEquals($E1B35EAB, FFormat_HEX.Identity);
end;

procedure TestTFormat_HEX.TestIsValidRawByteString;
begin
  CheckEquals(true,  TFormat_HEX.IsValid(BytesOf('')));
  CheckEquals(true,  TFormat_HEX.IsValid(BytesOf('0123456789abcdefABCDEF')));
  // Invalid character: q is not a hex char
  CheckEquals(false, TFormat_HEX.IsValid(BytesOf('1q')));
  // Hex input length needs to be a multiple of 2, if input is not empty
  CheckEquals(false, TFormat_HEX.IsValid(BytesOf('6')));
end;

procedure TestTFormat_HEX.TestIsValidTBytes;
var
  SrcBuf: TBytes;
begin
  SrcBuf := BytesOf(RawByteString(''));
  CheckEquals(true, TFormat_HEX.IsValid(SrcBuf));

  SrcBuf := BytesOf(RawByteString('0123456789abcdefABCDEF'));
  CheckEquals(true, TFormat_HEX.IsValid(SrcBuf));

  SrcBuf := BytesOf(RawByteString('q'));
  CheckEquals(false, TFormat_HEX.IsValid(SrcBuf));
end;

procedure TestTFormat_HEX.TestIsValidTypeless;
var
  SrcBuf: TBytes;
  p     : Pointer;
begin
  SrcBuf := BytesOf(RawByteString(''));
  CheckEquals(true, TFormat_HEX.IsValid(SrcBuf, 0));

  SrcBuf := BytesOf(RawByteString('0123456789abcdefABCDEF'));
  p := @SrcBuf[0];
  CheckEquals(true, TFormat_HEX.IsValid(p^, length(SrcBuf)));

  SrcBuf := BytesOf(RawByteString('q'));
  p := @SrcBuf[0];
  CheckEquals(false, TFormat_HEX.IsValid(p^, length(SrcBuf)));
end;

procedure TestTFormat_HEXL.SetUp;
begin
  FFormat_HEXL := TFormat_HEXL.Create;
end;

procedure TestTFormat_HEXL.TearDown;
begin
  FFormat_HEXL.Free;
  FFormat_HEXL := nil;
end;

procedure TestTFormat_HEXL.TestClassByName;
var
  ReturnValue : TDECFormatClass;
begin
  ReturnValue := FFormat_HEXL.ClassByName('TFormat_HEXL');
  CheckEquals(TFormat_HEXL, ReturnValue, 'Class is not registered');
end;

procedure TestTFormat_HEXL.TestDecodeBytes;
begin
  DoTestEncodeDecode(FFormat_HEXL.Decode, cTestDataDecode);
end;

procedure TestTFormat_HEXL.TestDecodeRawByteString;
begin
  DoTestEncodeDecodeRawByteString(FFormat_HEXL.Decode, cTestDataDecode);
end;

procedure TestTFormat_HEXL.TestDecodeTypeless;
begin
  DoTestEncodeDecodeTypeless(FFormat_HEXL.Decode, cTestDataDecode);
end;

procedure TestTFormat_HEXL.TestEncodeBytes;
begin
  DoTestEncodeDecode(FFormat_HEXL.Encode, cTestDataEncode);
end;

procedure TestTFormat_HEXL.TestEncodeRawByteString;
begin
  DoTestEncodeDecodeRawByteString(FFormat_HEXL.Encode, cTestDataEncode);
end;

procedure TestTFormat_HEXL.TestEncodeTypeless;
begin
  DoTestEncodeDecodeTypeless(FFormat_HEXL.Encode, cTestDataEncode);
end;

procedure TestTFormat_HEXL.TestIdentity;
begin
  CheckEquals($39D2D18D, FFormat_HEXL.Identity);
end;

procedure TestTFormat_HEXL.TestIsValidRawByteString;
begin
  CheckEquals(true, TFormat_HEXL.IsValid(BytesOf('')));
  CheckEquals(true, TFormat_HEXL.IsValid(BytesOf('0123456789abcdefABCDEF')));
  // Invalid character: q is not a hex char
  CheckEquals(false, TFormat_HEX.IsValid(BytesOf('1q')));
  // Hex input length needs to be a multiple of 2, if input is not empty
  CheckEquals(false, TFormat_HEX.IsValid(BytesOf('6')));
end;

procedure TestTFormat_HEXL.TestIsValidTBytes;
var
  SrcBuf: TBytes;
begin
  SrcBuf := BytesOf(RawByteString(''));
  CheckEquals(true, TFormat_HEXL.IsValid(SrcBuf));

  SrcBuf := BytesOf(RawByteString('0123456789abcdefABCDEF'));
  CheckEquals(true, TFormat_HEXL.IsValid(SrcBuf));

  SrcBuf := BytesOf(RawByteString('q'));
  CheckEquals(false, TFormat_HEXL.IsValid(SrcBuf));
end;

procedure TestTFormat_HEXL.TestIsValidTypeless;
var
  SrcBuf: TBytes;
begin
  SrcBuf := BytesOf(RawByteString(''));
  CheckEquals(true, TFormat_HEXL.IsValid(SrcBuf, 0));

  SrcBuf := BytesOf(RawByteString('0123456789abcdefABCDEF'));
  CheckEquals(true, TFormat_HEXL.IsValid(SrcBuf[0], length(SrcBuf)));

  SrcBuf := BytesOf(RawByteString('q'));
  CheckEquals(false, TFormat_HEXL.IsValid(SrcBuf[0], length(SrcBuf)));
end;

procedure TestTFormat_DECMIME32.SetUp;
begin
  FFormat_DECMIME32 := TFormat_DECMIME32.Create;
end;

procedure TestTFormat_DECMIME32.TearDown;
begin
  FFormat_DECMIME32.Free;
  FFormat_DECMIME32 := nil;
end;

procedure TestTFormat_DECMIME32.TestClassByName;
var
  ReturnValue : TDECFormatClass;
begin
  ReturnValue := FFormat_DECMIME32.ClassByName('TFormat_DECMIME32');
  CheckEquals(TFormat_DECMIME32, ReturnValue, 'Class is not registered');
end;

procedure TestTFormat_DECMIME32.TestDecodeBytes;
begin
  DoTestEncodeDecode(FFormat_DECMIME32.Decode, cTestDataDecode);
end;

procedure TestTFormat_DECMIME32.TestDecodeRawByteString;
begin
  DoTestEncodeDecodeRawByteString(FFormat_DECMIME32.Decode, cTestDataDecode);
end;

procedure TestTFormat_DECMIME32.TestDecodeTypeless;
begin
  DoTestEncodeDecodeTypeless(FFormat_DECMIME32.Decode, cTestDataDecode);
end;

procedure TestTFormat_DECMIME32.TestEncodeBytes;
begin
  DoTestEncodeDecode(FFormat_DECMIME32.Encode, cTestDataEncode);
end;

procedure TestTFormat_DECMIME32.TestEncodeRawByteString;
begin
  DoTestEncodeDecodeRawByteString(FFormat_DECMIME32.Encode, cTestDataEncode);
end;

procedure TestTFormat_DECMIME32.TestEncodeTypeless;
begin
  DoTestEncodeDecodeTypeless(FFormat_DECMIME32.Encode, cTestDataEncode);
end;

procedure TestTFormat_DECMIME32.TestIdentity;
begin
  CheckEquals($A7072340, FFormat_DECMIME32.Identity);
end;

procedure TestTFormat_DECMIME32.TestIsValidRawByteString;
begin
  CheckEquals(true, TFormat_DECMIME32.IsValid(BytesOf('')));

  CheckEquals(true, TFormat_DECMIME32.IsValid(BytesOf('abcdefghijklnpqrstuwxyz123456789')));
  CheckEquals(false, TFormat_DECMIME32.IsValid(BytesOf('1Q')));
  CheckEquals(true, TFormat_DECMIME32.IsValid(BytesOf('6')));
end;

procedure TestTFormat_DECMIME32.TestIsValidTBytes;
var
  SrcBuf: TBytes;
begin
  SrcBuf := BytesOf(RawByteString(''));
  CheckEquals(true, TFormat_DECMIME32.IsValid(SrcBuf));

  SrcBuf := BytesOf(cTestDataEncode[3].Output);
  CheckEquals(true, TFormat_DECMIME32.IsValid(SrcBuf));

  SrcBuf := BytesOf(RawByteString('Q'));
  CheckEquals(false, TFormat_DECMIME32.IsValid(SrcBuf));
end;

procedure TestTFormat_DECMIME32.TestIsValidTypeless;
var
  SrcBuf: TBytes;
begin
  SrcBuf := BytesOf(RawByteString(''));
  CheckEquals(true, TFormat_DECMIME32.IsValid(SrcBuf, 0));

  SrcBuf := BytesOf(cTestDataEncode[3].Output);
  CheckEquals(true, TFormat_DECMIME32.IsValid(SrcBuf[0], length(SrcBuf)));

  SrcBuf := BytesOf(RawByteString('Q'));
  CheckEquals(false, TFormat_DECMIME32.IsValid(SrcBuf[0], length(SrcBuf)));
end;

procedure TestTFormat_Base64.SetUp;
begin
  FFormat_Base64 := TFormat_Base64.Create;
end;

procedure TestTFormat_Base64.TearDown;
begin
  FFormat_Base64.Free;
  FFormat_Base64 := nil;
end;

procedure TestTFormat_Base64.TestClassByName;
var
  ReturnValue : TDECFormatClass;
begin
  ReturnValue := FFormat_Base64.ClassByName('TFormat_Base64');
  CheckEquals(TFormat_Base64, ReturnValue, 'Class is not registered');
end;

procedure TestTFormat_Base64.TestDecodeBytes;
begin
  DoTestEncodeDecode(FFormat_Base64.Decode, cTestDataDecode);
end;

procedure TestTFormat_Base64.TestDecodeRawByteString;
begin
  DoTestEncodeDecodeRawByteString(FFormat_Base64.Decode, cTestDataDecode);
end;

procedure TestTFormat_Base64.TestDecodeTypeless;
begin
  DoTestEncodeDecodeTypeless(FFormat_Base64.Decode, cTestDataDecode);
end;

procedure TestTFormat_Base64.TestEncodeBytes;
begin
  DoTestEncodeDecode(FFormat_Base64.Encode, cTestDataEncode);
end;

procedure TestTFormat_Base64.TestEncodeRawByteString;
begin
  DoTestEncodeDecodeRawByteString(FFormat_Base64.Encode, cTestDataEncode);
end;

procedure TestTFormat_Base64.TestEncodeTypeless;
begin
  DoTestEncodeDecodeTypeless(FFormat_Base64.Encode, cTestDataEncode);
end;

procedure TestTFormat_Base64.TestIdentity;
begin
  CheckEquals($521BA151, FFormat_BASE64.Identity);
end;

procedure TestTFormat_Base64.TestIsValidRawByteString;
var
  i : Integer;
begin
  CheckEquals(true, TFormat_Base64.IsValid(BytesOf('')));

  CheckEquals(true, TFormat_Base64.IsValid(
    BytesOf('ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/='#$0D#$0A)));
  CheckEquals(false, TFormat_Base64.IsValid(BytesOf('abc"')));
  CheckEquals(true, TFormat_Base64.IsValid(BytesOf('6')));

  for i := low(cTestDataDecode) to high(cTestDataDecode) do
  begin
    // skip empty test data
    if (cTestDataDecode[i].Input = '') then
      Continue;

    CheckEquals(true, TFormat_Base64.IsValid(RawByteString(cTestDataDecode[i].Input)),
                'Failure on ' + string(cTestDataDecode[i].Input) + ' ');
  end;
end;

procedure TestTFormat_Base64.TestIsValidTBytes;
const
  Data = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/='#$0D#$0A;
var
  SrcBuf: TBytes;
  i     : Integer;
begin
  SrcBuf := BytesOf(RawByteString(''));
  CheckEquals(true, TFormat_Base64.IsValid(SrcBuf));

  SetLength(SrcBuf, 1);
  for i := 0 to 255 do
  begin
    SrcBuf[0] := i;

    if (pos(chr(i), Data) > 0) then
      CheckEquals(true, TFormat_Base64.IsValid(SrcBuf),
                  'Failure at char: ' + Chr(i) + ' ')
    else
      CheckEquals(false, TFormat_Base64.IsValid(SrcBuf),
                  'Failure at char nr: ' + IntToHex(i, 8) + ' ');
  end;

  SrcBuf := BytesOf('abc"');
  CheckEquals(false, TFormat_Base64.IsValid(SrcBuf), 'Data: abc" ');

  SrcBuf := BytesOf(cTestDataDecode[3].Input);
  CheckEquals(true, TFormat_Base64.IsValid(SrcBuf),
              'Data: ' + string(cTestDataDecode[3].Input));

  for i := low(cTestDataDecode) to high(cTestDataDecode) do
  begin
    // skip empty test data
    if (cTestDataDecode[i].Input = '') then
      Continue;

    SrcBuf := BytesOf(RawByteString(cTestDataDecode[i].Input));
    CheckEquals(true, TFormat_Base64.IsValid(SrcBuf),
                'Failure on ' + string(cTestDataDecode[i].Input) + ' ');
  end;
end;

procedure TestTFormat_Base64.TestIsValidTypeless;
const
  Data = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/='#$0D#$0A;
var
  SrcBuf: TBytes;
  i     : Integer;
begin
  SrcBuf := BytesOf(RawByteString(''));
  CheckEquals(true, TFormat_Base64.IsValid(SrcBuf, 0));

  SetLength(SrcBuf, 1);
  for i := 0 to 255 do
  begin
    SrcBuf[0] := i;

    if (pos(chr(i), Data) > 0) then
      CheckEquals(true, TFormat_Base64.IsValid(SrcBuf[0], length(SrcBuf)),
                  'Failure at char: ' + Chr(i) + ' ')
    else
      CheckEquals(false, TFormat_Base64.IsValid(SrcBuf[0], length(SrcBuf)),
                  'Failure at char nr: ' + IntToHex(i, 8) + ' ');
  end;


  SrcBuf := BytesOf('abc"');
  CheckEquals(false, TFormat_Base64.IsValid(SrcBuf[0], length(SrcBuf)), 'Data: abc" ');

  SrcBuf := BytesOf(cTestDataDecode[3].Input);
  CheckEquals(true, TFormat_Base64.IsValid(SrcBuf[0], length(SrcBuf)),
              'Data: ' + string(cTestDataDecode[3].Input));

  for i := low(cTestDataDecode) to high(cTestDataDecode) do
  begin
    // skip empty test data
    if (cTestDataDecode[i].Input = '') then
      Continue;

    SrcBuf := BytesOf(RawByteString(cTestDataDecode[i].Input));
    CheckEquals(true, TFormat_Base64.IsValid(SrcBuf[0], length(SrcBuf)),
                'Failure on ' + string(cTestDataDecode[i].Input) + ' ');
  end;
end;

procedure TestTFormat_Radix64.SetUp;
begin
  FFormat_Radix64 := TFormat_Radix64.Create;
end;

procedure TestTFormat_Radix64.TearDown;
begin
  FFormat_Radix64.Free;
  FFormat_Radix64 := nil;
end;

procedure TestTFormat_Radix64.TestEncodeRawByteStringWithCharsPerLine;
type
  /// <summary>
  ///   Type needed for the EncodeBytes und DecodeBytes test data definition
  /// </summary>
  TestRecCharsPerLine = record
    Input, Output: RawByteString;
    CharsPerLine : Byte;
  end;

const
  cTestDataEncode : array[1..2] of TestRecCharsPerLine = (
    (Input:  RawByteString('Test' + #10 +#9 + #$AA + #$55);
     Output: RawByteString('VGVzdAoJqlU=' + #13 + #10 +'=XtiM');
     CharsPerLine: 20),
    (Input:  RawByteString('Test' + #10 +#9 + #$AA + #$55);
     Output: RawByteString('VGVz' + #13 + #10 + 'dAoJ' + #13 + #10 + 'qlU=' +
                           #13 + #10 + '=Xti' + #13 + #10 + 'M');
     CharsPerLine: 4)); 
var
  i       : Integer;
  SrcBuf,
  DestBuf : TBytes;
  OldCharsPerLineValue : Cardinal;
begin
  OldCharsPerLineValue := FFormat_Radix64.GetCharsPerLine;
  for i := Low(cTestDataEncode) to High(cTestDataEncode) do
  begin
    SrcBuf := BytesOf(RawByteString(cTestDataEncode[i].Input));
    FFormat_Radix64.SetCharsPerLine(cTestDataEncode[i].CharsPerLine);

    DestBuf := FFormat_Radix64.Encode(SrcBuf);

    CheckEquals(cTestDataEncode[i].Output,
                BytesToRawString(DestBuf));
  end;
  FFormat_Radix64.SetCharsPerLine(OldCharsPerLineValue);
end;

procedure TestTFormat_Radix64.TestClassByName;
var
  ReturnValue : TDECFormatClass;
begin
  ReturnValue := FFormat_Radix64.ClassByName('TFormat_Radix64');
  CheckEquals(TFormat_Radix64, ReturnValue, 'Class is not registered');
end;

procedure TestTFormat_Radix64.TestDecodeBytes;
begin
  DoTestEncodeDecode(FFormat_Radix64.Decode, cTestDataDecode);
end;

procedure TestTFormat_Radix64.TestDecodeRawByteString;
begin
  DoTestEncodeDecodeRawByteString(FFormat_Radix64.Decode, cTestDataDecode);
end;

procedure TestTFormat_Radix64.TestDecodeTypeless;
begin
  DoTestEncodeDecodeTypeless(FFormat_Radix64.Decode, cTestDataDecode);
end;

procedure TestTFormat_Radix64.TestEncodeBytes;
begin
  DoTestEncodeDecode(FFormat_Radix64.Encode, cTestDataEncode);
end;

procedure TestTFormat_Radix64.TestEncodeRawByteString;
begin
  DoTestEncodeDecodeRawByteString(FFormat_Radix64.Encode, cTestDataEncode);
end;

procedure TestTFormat_Radix64.TestEncodeTypeless;
begin
  DoTestEncodeDecodeTypeless(FFormat_Radix64.Encode, cTestDataEncode);
end;

procedure TestTFormat_Radix64.TestIdentity;
begin
  CheckEquals($B5607732, FFormat_Radix64.Identity);
end;

procedure TestTFormat_Radix64.TestIsValidRawByteString;
var
  i     : Integer;
begin
  for i := low(cTestDataDecode) to high(cTestDataDecode) do
  begin
    // skip empty test data
    if (cTestDataDecode[i].Input = '') then
      Continue;

    CheckEquals(true, TFormat_Radix64.IsValid(cTestDataDecode[i].Input),
                'Failure on ' + string(cTestDataDecode[i].Input) + ' ');
  end;

  // Check if wrong CRC is not being detected
  CheckEquals(false, TFormat_Radix64.IsValid('VGVzdAoJqlU=' + #13 + #10 +'=XtiN'),
              'CRC-failure not detected on ' + 'VGVzdAoJqlU=' + #13 + #10 +'=XtiN' + ' ');

  CheckEquals(false, TFormat_Radix64.IsValid('VGVzdAoJqlU=' + #13 + #10 +'=YtiM'),
              'CRC-failure not detected on ' + 'VGVzdAoJqlU=' + #13 + #10 +'=YtiM' + ' ');

  // No CRC present due to missing =
  CheckEquals(false, TFormat_Radix64.IsValid('VGVzdAoJqlU=' + #13 + #10 +'XtiM'),
              'CRC not present not detected on ' + 'VGVzdAoJqlU=' + #13 + #10 +'XtiM' + ' ');

  // CRC has double =
  CheckEquals(false, TFormat_Radix64.IsValid('VGVzdAoJqlU=' + #13 + #10 +'XtiM'),
              'CRC not present not detected on ' + 'VGVzdAoJqlU=' + #13 + #10 +'==XtiM' + ' ');
end;

procedure TestTFormat_Radix64.TestIsValidTBytes;
var
  SrcBuf: TBytes;
  i     : Integer;
begin
  for i := low(cTestDataDecode) to high(cTestDataDecode) do
  begin
    // skip empty test data
    if (cTestDataDecode[i].Input = '') then
      Continue;

    SrcBuf := BytesOf(RawByteString(cTestDataDecode[i].Input));
    CheckEquals(true, TFormat_Radix64.IsValid(SrcBuf),
                'Failure on ' + string(cTestDataDecode[i].Input) + ' ');
  end;

  // Check if wrong CRC is not being detected
  CheckEquals(false, TFormat_Radix64.IsValid(BytesOf(RawByteString('VGVzdAoJqlU=' + #13 + #10 +'=XtiN'))),
              'CRC-failure not detected on ' + 'VGVzdAoJqlU=' + #13 + #10 +'=XtiN' + ' ');

  CheckEquals(false, TFormat_Radix64.IsValid(BytesOf(RawByteString('VGVzdAoJqlU=' + #13 + #10 +'=YtiM'))),
              'CRC-failure not detected on ' + 'VGVzdAoJqlU=' + #13 + #10 +'=YtiM' + ' ');

  // No CRC present due to missing =
  CheckEquals(false, TFormat_Radix64.IsValid(BytesOf(RawByteString('VGVzdAoJqlU=' + #13 + #10 +'XtiM'))),
              'CRC not present not detected on ' + 'VGVzdAoJqlU=' + #13 + #10 +'XtiM' + ' ');

  // CRC has double =
  CheckEquals(false, TFormat_Radix64.IsValid(BytesOf(RawByteString('VGVzdAoJqlU=' + #13 + #10 +'XtiM'))),
              'CRC not present not detected on ' + 'VGVzdAoJqlU=' + #13 + #10 +'==XtiM' + ' ');
end;

procedure TestTFormat_Radix64.TestIsValidTypeless;
var
  SrcBuf: TBytes;
  i     : Integer;
begin
  for i := low(cTestDataDecode) to high(cTestDataDecode) do
  begin
    // skip empty test data
    if (cTestDataDecode[i].Input = '') then
      Continue;

    SrcBuf := BytesOf(RawByteString(cTestDataDecode[i].Input));
    CheckEquals(true, TFormat_Radix64.IsValid(SrcBuf[0], length(SrcBuf)),
                'Failure on ' + string(cTestDataDecode[i].Input) + ' ');
  end;

  // Check if wrong CRC is not being detected
  SrcBuf := BytesOf(RawByteString('VGVzdAoJqlU=' + #13 + #10 +'=XtiN'));
  CheckEquals(false, TFormat_Radix64.IsValid(SrcBuf[0], length(SrcBuf)),
              'CRC-failure not detected on ' + 'VGVzdAoJqlU=' + #13 + #10 +'=XtiN' + ' ');

  SrcBuf := BytesOf(RawByteString('VGVzdAoJqlU=' + #13 + #10 +'=YtiM'));
  CheckEquals(false, TFormat_Radix64.IsValid(SrcBuf[0], length(SrcBuf)),
              'CRC-failure not detected on ' + 'VGVzdAoJqlU=' + #13 + #10 +'=YtiM' + ' ');

  // No CRC present due to missing =
  SrcBuf := BytesOf(RawByteString('VGVzdAoJqlU=' + #13 + #10 +'XtiM'));
  CheckEquals(false, TFormat_Radix64.IsValid(SrcBuf[0], length(SrcBuf)),
              'CRC not present not detected on ' + 'VGVzdAoJqlU=' + #13 + #10 +'XtiM' + ' ');

  // CRC has double =
  SrcBuf := BytesOf(RawByteString('VGVzdAoJqlU=' + #13 + #10 +'XtiM'));
  CheckEquals(false, TFormat_Radix64.IsValid(SrcBuf[0], length(SrcBuf)),
              'CRC not present not detected on ' + 'VGVzdAoJqlU=' + #13 + #10 +'==XtiM' + ' ');
end;

procedure TestTFormat_UU.SetUp;
begin
  FFormat_UU := TFormat_UU.Create;
end;

procedure TestTFormat_UU.TearDown;
begin
  FFormat_UU.Free;
  FFormat_UU := nil;
end;

procedure TestTFormat_UU.TestClassByName;
var
  ReturnValue : TDECFormatClass;
begin
  ReturnValue := FFormat_UU.ClassByName('TFormat_UU');
  CheckEquals(TFormat_UU, ReturnValue, 'Class is not registered');
end;

procedure TestTFormat_UU.TestDecodeBytes;
begin
  DoTestEncodeDecode(FFormat_UU.Decode, cTestDataDecode);
end;

procedure TestTFormat_UU.TestDecodeRawByteString;
begin
  DoTestEncodeDecodeRawByteString(FFormat_UU.Decode, cTestDataDecode);
end;

procedure TestTFormat_UU.TestDecodeTypeless;
begin
  DoTestEncodeDecodeTypeless(FFormat_UU.Decode, cTestDataDecode);
end;

procedure TestTFormat_UU.TestEncodeBytes;
begin
  DoTestEncodeDecode(FFormat_UU.Encode, cTestDataEncode);
end;

procedure TestTFormat_UU.TestEncodeRawByteString;
begin
  DoTestEncodeDecodeRawByteString(FFormat_UU.Encode, cTestDataEncode);
end;

procedure TestTFormat_UU.TestEncodeTypeless;
begin
  DoTestEncodeDecodeTypeless(FFormat_UU.Encode, cTestDataEncode);
end;

procedure TestTFormat_UU.TestIdentity;
begin
  CheckEquals($6FCCDE6F, FFormat_UU.Identity);
end;

procedure TestTFormat_UU.TestIsValidRawByteString;
var
  i     : Integer;
begin
  CheckEquals(true, TFormat_UU.IsValid(RawByteString('')),'Failure on empty string ');

  for i := Low(cTestDataDecode) to High(cTestDataDecode) do
  begin
    if cTestDataDecode[i].Input = '' then
      Continue;

    CheckEquals(true, TFormat_UU.IsValid(cTestDataDecode[i].Input),
                'Failure on ' + string(cTestDataDecode[i].Input) + ' ');
  end;

  CheckEquals(false, TFormat_UU.IsValid(RawByteString('#$61')), 'Failure on char $61 ');
  CheckEquals(false, TFormat_UU.IsValid('(5&5S' + #$61 + '=`H)JE4`'), 'Failure on char $61 inbetween ');
  CheckEquals(false, TFormat_UU.IsValid(#$61 + '(5&5S=`H)JE4`'), 'Failure on char $61 at the beginning ');
  CheckEquals(false, TFormat_UU.IsValid('(5&5S=`H)JE4`' + #$61), 'Failure on char $61 at the end ');

  for i := $61 to $FF do
    CheckEquals(false, TFormat_UU.IsValid(RawByteString(chr(i))),
                'Failure on char #' + IntToHex(i, 8) + ' ');
end;

procedure TestTFormat_UU.TestIsValidTBytes;
var
  SrcBuf: TBytes;
  i     : Integer;
begin
  SetLength(SrcBuf, 0);
  CheckEquals(true, TFormat_UU.IsValid(SrcBuf, length(SrcBuf)),
              'Failure on empty buffer ');

  for i := Low(cTestDataDecode) to High(cTestDataDecode) do
  begin
    if cTestDataDecode[i].Input = '' then
      Continue;

    CheckEquals(true, TFormat_UU.IsValid(BytesOf(cTestDataDecode[i].Input)),
                'Failure on ' + string(cTestDataDecode[i].Input) + ' ');
  end;

  CheckEquals(false, TFormat_UU.IsValid(BytesOf(RawByteString('#$61'))), 'Failure on char $61 ');
  CheckEquals(false, TFormat_UU.IsValid(BytesOf('(5&5S' + #$61 + '=`H)JE4`')), 'Failure on char $61 inbetween ');
  CheckEquals(false, TFormat_UU.IsValid(BytesOf(#$61 + '(5&5S=`H)JE4`')), 'Failure on char $61 at the beginning ');
  CheckEquals(false, TFormat_UU.IsValid(BytesOf('(5&5S=`H)JE4`' + #$61)), 'Failure on char $61 at the end ');

  for i := $61 to $FF do
    CheckEquals(false, TFormat_UU.IsValid(BytesOf(RawByteString(chr(i)))),
                'Failure on char #' + IntToHex(i, 8) + ' ');
end;

procedure TestTFormat_UU.TestIsValidTypeless;
var
  SrcBuf: TBytes;
  i     : Integer;
  p     : Pointer;
begin
  SetLength(SrcBuf, 0);
  p := @SrcBuf;
  CheckEquals(true, TFormat_UU.IsValid(p^, length(SrcBuf)),
              'Failure on empty buffer ');

  for i := Low(cTestDataDecode) to High(cTestDataDecode) do
  begin
    if cTestDataDecode[i].Input = '' then
      Continue;

    SrcBuf := BytesOf(cTestDataDecode[i].Input);
    p := @SrcBuf[0];
    CheckEquals(true, TFormat_UU.IsValid(p^, length(SrcBuf)),
                'Failure on ' + string(cTestDataDecode[i].Input) + ' ');
  end;

  SrcBuf := BytesOf(RawByteString('#$61'));
  p := @SrcBuf;
  CheckEquals(false, TFormat_UU.IsValid(p^, length(SrcBuf)), 'Failure on char $61 ');
  SrcBuf := BytesOf('(5&5S' + #$61 + '=`H)JE4`');
  p := @SrcBuf;
  CheckEquals(false, TFormat_UU.IsValid(p^, length(SrcBuf)), 'Failure on char $61 inbetween ');
  SrcBuf := BytesOf(#$61 + '(5&5S=`H)JE4`');
  p := @SrcBuf;
  CheckEquals(false, TFormat_UU.IsValid(p^, length(SrcBuf)), 'Failure on char $61 at the beginning ');
  SrcBuf := BytesOf('(5&5S=`H)JE4`' + #$61);
  p := @SrcBuf;
  CheckEquals(false, TFormat_UU.IsValid(p^, length(SrcBuf)), 'Failure on char $61 at the end ');

  SetLength(SrcBuf, 1);
  for i := $61 to $FF do
  begin
    SrcBuf[0] := i;
    p := @SrcBuf;
    CheckEquals(false, TFormat_UU.IsValid(p^, length(SrcBuf)),
                'Failure on char #' + IntToHex(i, 8) + ' ');
  end;
end;

procedure TestTFormat_XX.SetUp;
begin
  FFormat_XX := TFormat_XX.Create;
end;

procedure TestTFormat_XX.TearDown;
begin
  FFormat_XX.Free;
  FFormat_XX := nil;
end;

procedure TestTFormat_XX.TestClassByName;
var
  ReturnValue : TDECFormatClass;
begin
  ReturnValue := FFormat_XX.ClassByName('TFormat_XX');
  CheckEquals(TFormat_XX, ReturnValue, 'Class is not registered');
end;

procedure TestTFormat_XX.TestDecodeBytes;
begin
  DoTestEncodeDecode(FFormat_XX.Decode, cTestDataDecode);
end;

procedure TestTFormat_XX.TestDecodeRawByteString;
begin
  DoTestEncodeDecodeRawByteString(FFormat_XX.Decode, cTestDataDecode);
end;

procedure TestTFormat_XX.TestDecodeTypeless;
begin
  DoTestEncodeDecodeTypeless(FFormat_XX.Decode, cTestDataDecode);
end;

procedure TestTFormat_XX.TestEncodeBytes;
begin
  DoTestEncodeDecode(FFormat_XX.Encode, cTestDataEncode);
end;

procedure TestTFormat_XX.TestEncodeRawByteString;
begin
  DoTestEncodeDecodeRawByteString(FFormat_XX.Encode, cTestDataEncode);
end;

procedure TestTFormat_XX.TestEncodeTypeless;
begin
  DoTestEncodeDecodeTypeless(FFormat_XX.Encode, cTestDataEncode);
end;

procedure TestTFormat_XX.TestIdentity;
begin
  CheckEquals($A4D3DC9F, FFormat_XX.Identity);
end;

procedure TestTFormat_XX.TestIsValidRawByteString;
var
  i     : Integer;
begin
  CheckEquals(true, TFormat_XX.IsValid(RawByteString('')),'Failure on empty string ');

  for i := Low(cTestDataDecode) to High(cTestDataDecode) do
  begin
    if cTestDataDecode[i].Input = '' then
      Continue;

    CheckEquals(true, TFormat_XX.IsValid(cTestDataDecode[i].Input),
                'Failure on ' + string(cTestDataDecode[i].Input) + ' ');
  end;

  CheckEquals(false, TFormat_XX.IsValid(RawByteString('#$2A')), 'Failure on char $2A ');
  CheckEquals(false, TFormat_XX.IsValid('6J4Jn' + #$2A + 'R+c7eZI+'), 'Failure on char $2A inbetween ');
  CheckEquals(false, TFormat_XX.IsValid(#$2A +'6J4JnR+c7eZI+'), 'Failure on char $2A at the beginning ');
  CheckEquals(false, TFormat_XX.IsValid('6J4JnR+c7eZI+' + #$2A), 'Failure on char $2A at the end ');

  CheckEquals(false, TFormat_XX.IsValid(RawByteString('#$23')), 'Failure on char $23 ');
  CheckEquals(false, TFormat_XX.IsValid('6J4Jn' + #$23 + 'R+c7eZI+'), 'Failure on char $23 inbetween ');
  CheckEquals(false, TFormat_XX.IsValid(#$23 +'6J4JnR+c7eZI+'), 'Failure on char $23 at the beginning ');
  CheckEquals(false, TFormat_XX.IsValid('6J4JnR+c7eZI+' + #$23), 'Failure on char $23 at the end ');

  for i := $7B to $FF do
    CheckEquals(false, TFormat_XX.IsValid(RawByteString(chr(i))),
                'Failure on char #' + IntToHex(i, 8) + ' ');
end;

procedure TestTFormat_XX.TestIsValidTBytes;
var
  SrcBuf: TBytes;
  i     : Integer;
begin
  SetLength(SrcBuf, 0);
  CheckEquals(true, TFormat_XX.IsValid(SrcBuf, length(SrcBuf)),
              'Failure on empty buffer ');

  for i := Low(cTestDataDecode) to High(cTestDataDecode) do
  begin
    if cTestDataDecode[i].Input = '' then
      Continue;

    CheckEquals(true, TFormat_XX.IsValid(BytesOf(cTestDataDecode[i].Input)),
                'Failure on ' + string(cTestDataDecode[i].Input) + ' ');
  end;

  CheckEquals(false, TFormat_XX.IsValid(BytesOf(RawByteString('#$2A'))), 'Failure on char $2A ');
  CheckEquals(false, TFormat_XX.IsValid(BytesOf('6J4Jn' + #$2A + 'R+c7eZI+')), 'Failure on char $2A inbetween ');
  CheckEquals(false, TFormat_XX.IsValid(BytesOf(#$2A +'6J4JnR+c7eZI+')), 'Failure on char $2A at the beginning ');
  CheckEquals(false, TFormat_XX.IsValid(BytesOf('6J4JnR+c7eZI+' + #$2A)), 'Failure on char $2A at the end ');

  CheckEquals(false, TFormat_XX.IsValid(BytesOf(RawByteString('#$23'))), 'Failure on char $23 ');
  CheckEquals(false, TFormat_XX.IsValid(BytesOf('6J4Jn' + #$23 + 'R+c7eZI+')), 'Failure on char $23 inbetween ');
  CheckEquals(false, TFormat_XX.IsValid(BytesOf(#$23 +'6J4JnR+c7eZI+')), 'Failure on char $23 at the beginning ');
  CheckEquals(false, TFormat_XX.IsValid(BytesOf('6J4JnR+c7eZI+' + #$23)), 'Failure on char $23 at the end ');

  for i := $7B to $FF do
    CheckEquals(false, TFormat_XX.IsValid(RawByteString(chr(i))),
                'Failure on char #' + IntToHex(i, 8) + ' ');
end;

procedure TestTFormat_XX.TestIsValidTypeless;
var
  SrcBuf: TBytes;
  i     : Integer;
  p     : Pointer;
begin
  SetLength(SrcBuf, 0);
  p := @SrcBuf;
  CheckEquals(true, TFormat_XX.IsValid(p^, length(SrcBuf)),
              'Failure on empty buffer ');

  for i := Low(cTestDataDecode) to High(cTestDataDecode) do
  begin
    if cTestDataDecode[i].Input = '' then
      Continue;

    SrcBuf := BytesOf(cTestDataDecode[i].Input);
    p := @SrcBuf[0];
    CheckEquals(true, TFormat_XX.IsValid(p^, length(SrcBuf)),
                'Failure on ' + string(cTestDataDecode[i].Input) + ' ');
  end;

  Srcbuf := BytesOf(RawByteString('#$2A'));
  p := @SrcBuf;
  CheckEquals(false, TFormat_XX.IsValid(p^, length(SrcBuf)), 'Failure on char $2A ');
  Srcbuf := BytesOf('6J4Jn' + #$2A + 'R+c7eZI+');
  p := @SrcBuf;
  CheckEquals(false, TFormat_XX.IsValid(p^, length(SrcBuf)), 'Failure on char $2A inbetween ');
  Srcbuf := BytesOf(#$2A +'6J4JnR+c7eZI+');
  p := @SrcBuf;
  CheckEquals(false, TFormat_XX.IsValid(p^, length(SrcBuf)), 'Failure on char $2A at the beginning ');
  Srcbuf := BytesOf('6J4JnR+c7eZI+' + #$2A);
  p := @SrcBuf;
  CheckEquals(false, TFormat_XX.IsValid(p^, length(SrcBuf)), 'Failure on char $2A at the end ');

  Srcbuf := BytesOf(RawByteString('#$23'));
  p := @SrcBuf;
  CheckEquals(false, TFormat_XX.IsValid(p^, length(SrcBuf)), 'Failure on char $23 ');
  Srcbuf := BytesOf('6J4Jn' + #$23 + 'R+c7eZI+');
  p := @SrcBuf;
  CheckEquals(false, TFormat_XX.IsValid(p^, length(SrcBuf)), 'Failure on char $23 inbetween ');
  Srcbuf := BytesOf(#$23 +'6J4JnR+c7eZI+');
  p := @SrcBuf;
  CheckEquals(false, TFormat_XX.IsValid(p^, length(SrcBuf)), 'Failure on char $23 at the beginning ');
  Srcbuf := BytesOf('6J4JnR+c7eZI+' + #$23);
  p := @SrcBuf;
  CheckEquals(false, TFormat_XX.IsValid(p^, length(SrcBuf)), 'Failure on char $23 at the end ');

  SetLength(SrcBuf, 1);
  for i := $61 to $FF do
  begin
    SrcBuf[0] := i;
    p := @SrcBuf;
    CheckEquals(false, TFormat_XX.IsValid(p^, length(SrcBuf)),
                'Failure on char #' + IntToHex(i, 8) + ' ');
  end;
end;

procedure TestTFormat_ESCAPE.SetUp;
begin
  FFormat_ESCAPE := TFormat_ESCAPE.Create;
end;

procedure TestTFormat_ESCAPE.TearDown;
begin
  FFormat_ESCAPE.Free;
  FFormat_ESCAPE := nil;
end;

procedure TestTFormat_ESCAPE.TestClassByName;
var
  ReturnValue : TDECFormatClass;
begin
  ReturnValue := FFormat_ESCAPE.ClassByName('TFormat_ESCAPE');
  CheckEquals(TFormat_ESCAPE, ReturnValue, 'Class is not registered');
end;

procedure TestTFormat_ESCAPE.TestDecodeBytes;
begin
  DoTestEncodeDecode(FFormat_ESCAPE.Decode, cTestDataDecode);
end;

procedure TestTFormat_ESCAPE.TestDecodeRawByteString;
begin
  DoTestEncodeDecodeRawByteString(FFormat_ESCAPE.Decode, cTestDataDecode);
end;

procedure TestTFormat_ESCAPE.TestDecodeTypeless;
begin
  DoTestEncodeDecodeTypeless(FFormat_ESCAPE.Decode, cTestDataDecode);
end;

procedure TestTFormat_ESCAPE.TestEncodeBytes;
begin
  DoTestEncodeDecode(FFormat_ESCAPE.Encode, cTestDataEncode);
end;

procedure TestTFormat_ESCAPE.TestEncodeRawByteString;
begin
  DoTestEncodeDecodeRawByteString(FFormat_ESCAPE.Encode, cTestDataEncode);
end;

procedure TestTFormat_ESCAPE.TestEncodeTypeless;
begin
  DoTestEncodeDecodeTypeless(FFormat_ESCAPE.Encode, cTestDataEncode);
end;

procedure TestTFormat_ESCAPE.TestIdentity;
begin
  CheckEquals($168B27C3, FFormat_ESCAPE.Identity);
end;

procedure TestTFormat_ESCAPE.TestIsValidRawByteString;
var
  i   : Integer;
  Str : RawByteString;
  p   : ^Byte;
begin
  CheckEquals(true, TFormat_ESCAPE.IsValid(RawByteString('')),'Failure on empty string ');

  // check all chars in allowed range
  for i := 32 to 127 do
  begin
    // skip backslash
    if i = $5C then
      Continue;

    CheckEquals(true, TFormat_ESCAPE.IsValid(RawByteString(chr(i))),
                IntToStr(i) + ': Failure on ' + chr(i) + ' ');
  end;

  // check hex chars
  for i := 128 to 255 do
  begin
    CheckEquals(true, TFormat_ESCAPE.IsValid(RawByteString('\X' + IntToHex(i, 2))),
                IntToStr(i) + ': Failure on \X' + IntToHex(i, 2));
  end;

  // check hex chars
  for i := 0 to 31 do
  begin
    CheckEquals(true, TFormat_ESCAPE.IsValid(RawByteString('\X' + IntToHex(i, 2))),
                IntToStr(i) + ': Failure on \X' + IntToHex(i, 2));
  end;

  for i := Low(cTestDataIsValid) to High(cTestDataIsValid) do
  begin
    CheckEquals(cTestDataIsValid[i].Result,
                TFormat_ESCAPE.IsValid(cTestDataIsValid[i].Input),
                IntToStr(i) + ': Failure on ' + string(cTestDataIsValid[i].Input));
  end;

  Str := 'A';
  p := @Str[1];
  p^ := $80;

  CheckEquals(false,
              TFormat_ESCAPE.IsValid(Str),
              'Failure on ' + string(Str));
end;

procedure TestTFormat_ESCAPE.TestIsValidTBytes;
var
  i : Integer;
  b : TBytes;
begin
  CheckEquals(true, TFormat_ESCAPE.IsValid(BytesOf(RawByteString(''))),'Failure on empty string ');

  // check all chars in allowed range
  for i := 32 to 127 do
  begin
    // skip backslash
    if i = $5C then
      Continue;

    CheckEquals(true, TFormat_ESCAPE.IsValid(BytesOf(chr(i))),
                IntToStr(i) + ': Failure on ' + chr(i) + ' ');
  end;

  // check hex chars
  for i := 128 to 255 do
  begin
    CheckEquals(true, TFormat_ESCAPE.IsValid(BytesOf('\X' + IntToHex(i, 2))),
                IntToStr(i) + ': Failure on \X' + IntToHex(i, 2));
  end;

  // check hex chars
  for i := 0 to 31 do
  begin
    CheckEquals(true, TFormat_ESCAPE.IsValid(BytesOf('\X' + IntToHex(i, 2))),
                IntToStr(i) + ': Failure on \X' + IntToHex(i, 2));
  end;

  for i := Low(cTestDataIsValid) to High(cTestDataIsValid) do
  begin
    CheckEquals(cTestDataIsValid[i].Result,
                TFormat_ESCAPE.IsValid(BytesOf(cTestDataIsValid[i].Input)),
                IntToStr(i) + ': Failure on ' + string(cTestDataIsValid[i].Input));
  end;

  // Chars outside the range 32..$7F need to be hex encoded e.g. \X80
  SetLength(b, 1);
  b[0] := $80;
  CheckEquals(false, TFormat_ESCAPE.IsValid(b), 'Failure on: #$80 ');
end;

procedure TestTFormat_ESCAPE.TestIsValidTypeless;
var
  i     : Integer;
  Bytes : TBytes;
begin
  SetLength(Bytes, 0);
  CheckEquals(true, TFormat_ESCAPE.IsValid(Bytes, 0),'Failure on empty string ');

  // check all chars in allowed range
  for i := 32 to 127 do
  begin
    // skip backslash
    if i = $5C then
      Continue;

    {$IF CompilerVersion >= 24.0}
    CheckEquals(true, TFormat_ESCAPE.IsValid(RawByteString(chr(i))[low(RawByteString)], 1),
                'Failure on ' + chr(i) + ' ');
    {$ELSE}
    CheckEquals(true, TFormat_ESCAPE.IsValid(RawByteString(chr(i))[1], 1),
                'Failure on ' + chr(i) + ' ');
    {$IFEND}
  end;

  // check hex chars
  for i := 128 to 255 do
  begin
    Bytes := BytesOf('\X' + IntToHex(i, 2));
    CheckEquals(true, TFormat_ESCAPE.IsValid(Bytes[0], length(Bytes)),
                IntToStr(i) + ': Failure on \X' + IntToHex(i, 2));
  end;

  // check hex chars
  for i := 0 to 31 do
  begin
    Bytes := BytesOf('\X' + IntToHex(i, 2));
    CheckEquals(true, TFormat_ESCAPE.IsValid(Bytes[0], length(Bytes)),
                IntToStr(i) + ': Failure on \X' + IntToHex(i, 2));
  end;

  for i := Low(cTestDataIsValid) to High(cTestDataIsValid) do
  begin
    Bytes := BytesOf(cTestDataIsValid[i].Input);
    CheckEquals(cTestDataIsValid[i].Result,
                TFormat_ESCAPE.IsValid(Bytes[0], length(Bytes)),
                IntToStr(i) + ': Failure on ' + string(cTestDataIsValid[i].Input));
  end;

  // Chars outside the range 32..$7F need to be hex encoded e.g. \X80
  SetLength(Bytes, 1);
  Bytes[0] := $80;
  CheckEquals(false, TFormat_ESCAPE.IsValid(Bytes[0], 1), 'Failure on: #$80 ');
end;

{ TFormatTestsBase }

procedure TFormatTestsBase.DoTestEncodeDecode(EncodeDecodeProc: TEncodeDecodeProc;
                                              TestData: array of TestRecRawByteString);
var
  i       : Integer;
  SrcBuf,
  DestBuf : TBytes;
begin
  for i := Low(TestData) to High(TestData) do
  begin
    SrcBuf := BytesOf(RawByteString(TestData[i].Input));
    DestBuf := EncodeDecodeProc(SrcBuf);

    CheckEquals(TestData[i].Output,
                BytesToRawString(DestBuf));
  end;
end;

procedure TFormatTestsBase.DoTestEncodeDecodeRawByteString(EncodeDecodeProc: TEncodeDecodeProcRawByteString;
                                                           TestData: array of TestRecRawByteString);
var
  i      : Integer;
  result : RawByteString;
begin
  for i := Low(TestData) to High(TestData) do
  begin
    result := EncodeDecodeProc(TestData[i].Input);

    CheckEquals(TestData[i].Output, result);
  end;
end;

procedure TFormatTestsBase.DoTestEncodeDecodeTypeless(EncodeDecodeProc: TEncodeDecodeProcTypeless;
                                                      TestData: array of TestRecRawByteString);
var
  i      : Integer;
  result : RawByteString;
  pdata  : PByte;
  len    : Integer;
begin
  for i := Low(TestData) to High(TestData) do
  begin
    if length(TestData[i].Input) > 0 then
    begin
      {$IF CompilerVersion >= 24.0}
      pdata := @TestData[i].Input[low(TestData[i].Input)];

      len := length(TestData[i].Input) * SizeOf(TestData[i].Input[low(TestData[i].Input)]);
      {$ELSE}
      pdata := @TestData[i].Input[1];

      len := length(TestData[i].Input) * SizeOf(TestData[i].Input[1]);
      {$IFEND}
    end
    else
    begin
      pdata := nil;
      len   := 0;
    end;

    result := EncodeDecodeProc(pdata^, len);

    CheckEquals(TestData[i].Output, result);
  end;
end;

{ TestTFormat_BigEndian16 }

procedure TestTFormat_BigEndian16.SetUp;
begin
  FFormat_BigEndian16 := TFormat_BigEndian16.Create;
end;

procedure TestTFormat_BigEndian16.TearDown;
begin
  FFormat_BigEndian16.Free;
  FFormat_BigEndian16 := nil;
end;

procedure TestTFormat_BigEndian16.TestClassByName;
var
  ReturnValue : TDECFormatClass;
begin
  ReturnValue := FFormat_BigEndian16.ClassByName('TFormat_BigEndian16');
  CheckEquals(TFormat_BigEndian16, ReturnValue, 'Class is not registered');
end;

procedure TestTFormat_BigEndian16.TestDecodeBytes;
begin
  DoTestEncodeDecode(FFormat_BigEndian16.Decode, cTestDataEncode);
end;

procedure TestTFormat_BigEndian16.TestDecodeRawByteString;
begin
  DoTestEncodeDecodeRawByteString(FFormat_BigEndian16.Decode, cTestDataEncode);
end;

procedure TestTFormat_BigEndian16.TestDecodeTypeless;
begin
  DoTestEncodeDecodeTypeless(FFormat_BigEndian16.Decode, cTestDataEncode);
end;

procedure TestTFormat_BigEndian16.TestEncodeBytes;
begin
  DoTestEncodeDecode(FFormat_BigEndian16.Encode, cTestDataEncode);
end;

procedure TestTFormat_BigEndian16.TestEncodeRawByteString;
begin
  DoTestEncodeDecodeRawByteString(FFormat_BigEndian16.Encode, cTestDataEncode);
end;

procedure TestTFormat_BigEndian16.TestEncodeTypeless;
begin
  DoTestEncodeDecodeTypeless(FFormat_BigEndian16.Encode, cTestDataEncode);
end;

procedure TestTFormat_BigEndian16.TestIdentity;
begin
  CheckEquals($957DD064, FFormat_BigEndian16.Identity);
end;

procedure TestTFormat_BigEndian16.TestIsValidRawByteString;
begin
  CheckEquals(true, TFormat_BigEndian16.IsValid(RawByteString('')),'Failure on empty string');
  CheckEquals(false, TFormat_BigEndian16.IsValid(RawByteString('1')),'Failure on odd length string');
  CheckEquals(true, TFormat_BigEndian16.IsValid(RawByteString('12')),'Failure on 2-byte string');
  CheckEquals(false, TFormat_BigEndian16.IsValid(RawByteString('123')),'Failure on 1-byte string');
  CheckEquals(true, TFormat_BigEndian16.IsValid(RawByteString('1234')),'Failure on 4-byte string');
  CheckEquals(true, TFormat_BigEndian16.IsValid(RawByteString('1234abCdeFghijkl')),'Failure on 16-byte string');
end;

procedure TestTFormat_BigEndian16.TestIsValidTBytes;
begin
  CheckEquals(true, TFormat_BigEndian16.IsValid(BytesOf(RawByteString(''))),'Failure on empty string ');

  CheckEquals(false, TFormat_BigEndian16.IsValid(BytesOf(RawByteString('1'))),'Failure on 1-byte string');
  CheckEquals(true, TFormat_BigEndian16.IsValid(BytesOf(RawByteString('12'))),'Failure on 2-byte string');
  CheckEquals(false, TFormat_BigEndian16.IsValid(BytesOf(RawByteString('123'))),'Failure on 3-byte string');
  CheckEquals(true, TFormat_BigEndian16.IsValid(BytesOf(RawByteString('1234'))),'Failure on 4-byte string');
  CheckEquals(true, TFormat_BigEndian16.IsValid(BytesOf(RawByteString('1234abCdeFghijkl'))),'Failure on 16-byte string');
end;

procedure TestTFormat_BigEndian16.TestIsValidTypeless;
var
  Bytes : TBytes;
begin
  SetLength(Bytes, 0);
  CheckEquals(true, TFormat_BigEndian16.IsValid(Bytes, 0),'Failure on empty data');

  Bytes := TBytes.Create(1);
  CheckEquals(false, TFormat_BigEndian16.IsValid(Bytes, length(Bytes)),'Failure on 1-byte data');

  Bytes := TBytes.Create(254, 255);
  CheckEquals(true, TFormat_BigEndian16.IsValid(Bytes, length(Bytes)),'Failure on 2-byte data');

  Bytes := TBytes.Create(1, 2, 3);
  CheckEquals(false, TFormat_BigEndian16.IsValid(Bytes, length(Bytes)),'Failure on 3-byte data');

  Bytes := TBytes.Create(1, 2, 3, 4);
  CheckEquals(true, TFormat_BigEndian16.IsValid(Bytes, length(Bytes)),'Failure on 4-byte data');

  Bytes := TBytes.Create(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16);
  CheckEquals(true, TFormat_BigEndian16.IsValid(Bytes, length(Bytes)),'Failure on 16-byte data');
end;

{ TestTFormat_BigEndian32 }

procedure TestTFormat_BigEndian32.SetUp;
begin
  FFormat_BigEndian32 := TFormat_BigEndian32.Create;
end;

procedure TestTFormat_BigEndian32.TearDown;
begin
  FFormat_BigEndian32.Free;
  FFormat_BigEndian32 := nil;
end;

procedure TestTFormat_BigEndian32.TestClassByName;
var
  ReturnValue : TDECFormatClass;
begin
  ReturnValue := FFormat_BigEndian32.ClassByName('TFormat_BigEndian32');
  CheckEquals(TFormat_BigEndian32, ReturnValue, 'Class is not registered');
end;

procedure TestTFormat_BigEndian32.TestDecodeBytes;
begin
  DoTestEncodeDecode(FFormat_BigEndian32.Decode, cTestDataEncode);
end;

procedure TestTFormat_BigEndian32.TestDecodeRawByteString;
begin
  DoTestEncodeDecodeRawByteString(FFormat_BigEndian32.Decode, cTestDataEncode);
end;

procedure TestTFormat_BigEndian32.TestDecodeTypeless;
begin
  DoTestEncodeDecodeTypeless(FFormat_BigEndian32.Decode, cTestDataEncode);
end;

procedure TestTFormat_BigEndian32.TestEncodeBytes;
begin
  DoTestEncodeDecode(FFormat_BigEndian32.Encode, cTestDataEncode);
end;

procedure TestTFormat_BigEndian32.TestEncodeRawByteString;
begin
  DoTestEncodeDecodeRawByteString(FFormat_BigEndian32.Encode, cTestDataEncode);
end;

procedure TestTFormat_BigEndian32.TestEncodeTypeless;
begin
  DoTestEncodeDecodeTypeless(FFormat_BigEndian32.Encode, cTestDataEncode);
end;

procedure TestTFormat_BigEndian32.TestIdentity;
begin
  CheckEquals($A02676FF, FFormat_BigEndian32.Identity);
end;

procedure TestTFormat_BigEndian32.TestIsValidRawByteString;
begin
  CheckEquals(true, TFormat_BigEndian32.IsValid(RawByteString('')),'Failure on empty string');
  CheckEquals(false, TFormat_BigEndian32.IsValid(RawByteString('1')),'Failure on odd length string');
  CheckEquals(false, TFormat_BigEndian32.IsValid(RawByteString('12')),'Failure on 2-byte string');
  CheckEquals(false, TFormat_BigEndian32.IsValid(RawByteString('123')),'Failure on 1-byte string');
  CheckEquals(true, TFormat_BigEndian32.IsValid(RawByteString('1234')),'Failure on 4-byte string');
  CheckEquals(true, TFormat_BigEndian32.IsValid(RawByteString('1234abCdeFghijkl')),'Failure on 16-byte string');
end;

procedure TestTFormat_BigEndian32.TestIsValidTBytes;
begin
  CheckEquals(true, TFormat_BigEndian32.IsValid(BytesOf(RawByteString(''))),'Failure on empty string ');

  CheckEquals(false, TFormat_BigEndian32.IsValid(BytesOf(RawByteString('1'))),'Failure on 1-byte string');
  CheckEquals(false, TFormat_BigEndian32.IsValid(BytesOf(RawByteString('12'))),'Failure on 2-byte string');
  CheckEquals(false, TFormat_BigEndian32.IsValid(BytesOf(RawByteString('123'))),'Failure on 3-byte string');
  CheckEquals(true, TFormat_BigEndian32.IsValid(BytesOf(RawByteString('1234'))),'Failure on 4-byte string');
  CheckEquals(true, TFormat_BigEndian32.IsValid(BytesOf(RawByteString('1234abCdeFghijkl'))),'Failure on 16-byte string');
end;

procedure TestTFormat_BigEndian32.TestIsValidTypeless;
var
  Bytes : TBytes;
begin
  SetLength(Bytes, 0);
  CheckEquals(true, TFormat_BigEndian32.IsValid(Bytes, 0),'Failure on empty data');

  Bytes := TBytes.Create(1);
  CheckEquals(false, TFormat_BigEndian32.IsValid(Bytes, length(Bytes)),'Failure on 1-byte data');

  Bytes := TBytes.Create(254, 255);
  CheckEquals(false, TFormat_BigEndian32.IsValid(Bytes, length(Bytes)),'Failure on 2-byte data');

  Bytes := TBytes.Create(1, 2, 3);
  CheckEquals(false, TFormat_BigEndian32.IsValid(Bytes, length(Bytes)),'Failure on 3-byte data');

  Bytes := TBytes.Create(1, 2, 3, 4);
  CheckEquals(true, TFormat_BigEndian32.IsValid(Bytes, length(Bytes)),'Failure on 4-byte data');

  Bytes := TBytes.Create(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16);
  CheckEquals(true, TFormat_BigEndian32.IsValid(Bytes, length(Bytes)),'Failure on 16-byte data');
end;

{ TestTFormat_BigEndian64 }

procedure TestTFormat_BigEndian64.SetUp;
begin
  FFormat_BigEndian64 := TFormat_BigEndian64.Create;
end;

procedure TestTFormat_BigEndian64.TearDown;
begin
  FFormat_BigEndian64.Free;
  FFormat_BigEndian64 := nil;
end;

procedure TestTFormat_BigEndian64.TestClassByName;
var
  ReturnValue : TDECFormatClass;
begin
  ReturnValue := FFormat_BigEndian64.ClassByName('TFormat_BigEndian64');
  CheckEquals(TFormat_BigEndian64, ReturnValue, 'Class is not registered');
end;

procedure TestTFormat_BigEndian64.TestDecodeBytes;
begin
  DoTestEncodeDecode(FFormat_BigEndian64.Decode, cTestDataEncode);
end;

procedure TestTFormat_BigEndian64.TestDecodeRawByteString;
begin
  DoTestEncodeDecodeRawByteString(FFormat_BigEndian64.Decode, cTestDataEncode);
end;

procedure TestTFormat_BigEndian64.TestDecodeTypeless;
begin
  DoTestEncodeDecodeTypeless(FFormat_BigEndian64.Decode, cTestDataEncode);
end;

procedure TestTFormat_BigEndian64.TestEncodeBytes;
begin
  DoTestEncodeDecode(FFormat_BigEndian64.Encode, cTestDataEncode);
end;

procedure TestTFormat_BigEndian64.TestEncodeRawByteString;
begin
  DoTestEncodeDecodeRawByteString(FFormat_BigEndian64.Encode, cTestDataEncode);
end;

procedure TestTFormat_BigEndian64.TestEncodeTypeless;
begin
  DoTestEncodeDecodeTypeless(FFormat_BigEndian64.Encode, cTestDataEncode);
end;

procedure TestTFormat_BigEndian64.TestIdentity;
begin
  CheckEquals($3432278F, FFormat_BigEndian64.Identity);
end;

procedure TestTFormat_BigEndian64.TestIsValidRawByteString;
begin
  CheckEquals(true, TFormat_BigEndian64.IsValid(RawByteString('')),'Failure on empty string');
  CheckEquals(false, TFormat_BigEndian64.IsValid(RawByteString('1')),'Failure on odd length string');
  CheckEquals(false, TFormat_BigEndian64.IsValid(RawByteString('12')),'Failure on 2-byte string');
  CheckEquals(false, TFormat_BigEndian64.IsValid(RawByteString('123')),'Failure on 1-byte string');
  CheckEquals(false, TFormat_BigEndian64.IsValid(RawByteString('1234')),'Failure on 4-byte string');
  CheckEquals(true, TFormat_BigEndian64.IsValid(RawByteString('1234abCd')),'Failure on 8-byte string');
  CheckEquals(true, TFormat_BigEndian64.IsValid(RawByteString('1234abCdeFghijkl')),'Failure on 16-byte string');
end;

procedure TestTFormat_BigEndian64.TestIsValidTBytes;
begin
  CheckEquals(true, TFormat_BigEndian64.IsValid(BytesOf(RawByteString(''))),'Failure on empty string ');

  CheckEquals(false, TFormat_BigEndian64.IsValid(BytesOf(RawByteString('1'))),'Failure on 1-byte string');
  CheckEquals(false, TFormat_BigEndian64.IsValid(BytesOf(RawByteString('12'))),'Failure on 2-byte string');
  CheckEquals(false, TFormat_BigEndian64.IsValid(BytesOf(RawByteString('123'))),'Failure on 3-byte string');
  CheckEquals(false, TFormat_BigEndian64.IsValid(BytesOf(RawByteString('1234'))),'Failure on 4-byte string');
  CheckEquals(true, TFormat_BigEndian64.IsValid(BytesOf(RawByteString('1234abCd'))),'Failure on 8-byte string');
  CheckEquals(true, TFormat_BigEndian64.IsValid(BytesOf(RawByteString('1234abCdeFghijkl'))),'Failure on 16-byte string');
end;

procedure TestTFormat_BigEndian64.TestIsValidTypeless;
var
  Bytes : TBytes;
begin
  SetLength(Bytes, 0);
  CheckEquals(true, TFormat_BigEndian64.IsValid(Bytes, 0),'Failure on empty data');

  Bytes := TBytes.Create(1);
  CheckEquals(false, TFormat_BigEndian64.IsValid(Bytes, length(Bytes)),'Failure on 1-byte data');

  Bytes := TBytes.Create(254, 255);
  CheckEquals(false, TFormat_BigEndian64.IsValid(Bytes, length(Bytes)),'Failure on 2-byte data');

  Bytes := TBytes.Create(1, 2, 3);
  CheckEquals(false, TFormat_BigEndian64.IsValid(Bytes, length(Bytes)),'Failure on 3-byte data');

  Bytes := TBytes.Create(1, 2, 3, 4);
  CheckEquals(false, TFormat_BigEndian64.IsValid(Bytes, length(Bytes)),'Failure on 4-byte data');

  Bytes := TBytes.Create(1, 2, 3, 4, 5, 6, 7, 8);
  CheckEquals(true, TFormat_BigEndian64.IsValid(Bytes, length(Bytes)),'Failure on 8-byte data');

  Bytes := TBytes.Create(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16);
  CheckEquals(true, TFormat_BigEndian64.IsValid(Bytes, length(Bytes)),'Failure on 16-byte data');
end;

initialization
  // Register any test cases with the test runner
  {$IFDEF DUnitX}
//  TDUnitX.RegisterTestFixture(TestTFormat);
  TDUnitX.RegisterTestFixture(TestTFormat_HEX);
  TDUnitX.RegisterTestFixture(TestTFormat_HEXL);
  TDUnitX.RegisterTestFixture(TestTFormat_DECMIME32);
  TDUnitX.RegisterTestFixture(TestTFormat_Base64);
  TDUnitX.RegisterTestFixture(TestTFormat_Radix64);
  TDUnitX.RegisterTestFixture(TestTFormat_UU);
  TDUnitX.RegisterTestFixture(TestTFormat_XX);
  TDUnitX.RegisterTestFixture(TestTFormat_ESCAPE);
  TDUnitX.RegisterTestFixture(TestTFormat_BigEndian16);
  TDUnitX.RegisterTestFixture(TestTFormat_BigEndian32);
  TDUnitX.RegisterTestFixture(TestTFormat_BigEndian64);
  {$ELSE}
  RegisterTests('DECFormat', [//TestTFormat,
                              TestTFormat_HEX.Suite,
                              TestTFormat_HEXL.Suite,   TestTFormat_DECMIME32.Suite,
                              TestTFormat_Base64.Suite, TestTFormat_Radix64.Suite,
                              TestTFormat_UU.Suite,     TestTFormat_XX.Suite,
                              TestTFormat_ESCAPE.Suite,
                              TestTFormat_BigEndian16.Suite,
                              TestTFormat_BigEndian32.Suite,
                              TestTFormat_BigEndian64.Suite]);
  {$ENDIF}

finalization
end.


