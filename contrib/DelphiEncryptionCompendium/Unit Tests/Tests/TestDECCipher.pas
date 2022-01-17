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
unit TestDECCipher;

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
  DECBaseClass, DECCipherBase, DECCiphers, DECUtil, DECFormatBase, DECFormat;

type
  // A function with these parameters has to be passed to DoTestEncode/Decode to
  // make that one generic
  TEncodeDecodeFunc = function (const Source: RawByteString; Format: TDECFormatClass = nil): TBytes of Object;

  /// <summary>
  ///   All known testvectors use the same filler byte and the same cmCTSx mode
  /// </summary>
  TCipherTestData = record
    InputData  : RawByteString;
    OutputData : RawByteString;

    Key        : RawByteString;
    InitVector : RawByteString;
    Filler     : Byte;
    Mode       : TCipherMode;
  end;

  /// <summary>
  ///   Init method called before conducting a test. Sets up the concrete
  ///   cipher password, mode, initialization vector etc.
  /// </summary>
  /// <param name="TestData">
  ///   Record with the data for the current test, including encryption key,
  ///   initialization vector etc.
  /// </param>
  TInitProc = procedure(TestData: TCipherTestData) Of Object;
  /// <summary>
  ///   Caqllback method for cleaning up after each test
  /// </summary>
  TDoneProc = procedure of Object;

  // Basic class all Cipher test classes should inherit from
  TCipherBasis = class(TTestCase)
  strict protected
    FTestData : array of TCipherTestData;
    /// <summary>
    ///   FTestData gets put into this memory stream in tests which test the
    ///   stream oriented encryption/decryption methods
    /// </summary>
    FTestStream : TMemoryStream;

    /// <summary>
    ///   Converts a test vector with the follwing syntax to a byte array:
    ///   \x30\x31\x41 where \ is the delimiter and x means that the following
    ///   two chars are the hex ordinal number of an ANSI char
    /// </summary>
    function  ConvertHexVectorToBytes(Vector: string): TBytes;

    /// <summary>
    ///   Ensures that a given key is not longer then the KeySize passed
    /// </summary>
    /// <param name="Key">
    ///   Key to be checked. if it is longer than KeySize it will be cut off.
    /// </param>
    /// <param name="KeySize">
    ///   Maximum size of a key for the given cipher algorithm
    /// </param>
    procedure LimitKeyLength(var Key:RawByteString; KeySize: Integer);

    procedure DoTestEncode(EncodeFunc: TEncodeDecodeFunc; InitProc: TInitProc; DoneProc: TDoneProc);
    procedure DoTestDecode(DecodeFunct: TEncodeDecodeFunc; InitProc: TInitProc; DoneProc: TDoneProc);
  end;

  // Testmethods for class TDECCipher
  {$IFDEF DUnitX} [TestFixture] {$ENDIF}
  TestTDECCipher = class(TCipherBasis)
  strict private
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIsClassListCreated;
    procedure TestValidCipherSetDefaultCipherClass;
  end;

  // Testmethoden for Klasse TCipher_Null
  {$IFDEF DUnitX} [TestFixture] {$ENDIF}
  TestTCipher_Null = class(TTestCase)
  strict private
    FCipher_Null: TCipher_Null;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIdentity;
    procedure TestContext;
    procedure TestClassByName;
  end;

  // Testmethods for class TCipher_Blowfish
  {$IFDEF DUnitX} [TestFixture] {$ENDIF}
  TestTCipher_Blowfish = class(TCipherBasis)
  strict private
    FCipher_Blowfish: TCipher_Blowfish;

    procedure Init(TestData: TCipherTestData);
    procedure Done;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIdentity;
    procedure TestContext;
    procedure TestEncode;
    procedure TestDecode;
    procedure TestClassByName;
  end;

  // Testmethods for class TCipher_Twofish
  {$IFDEF DUnitX} [TestFixture] {$ENDIF}
  TestTCipher_Twofish = class(TCipherBasis)
  strict private
    FCipher_Twofish: TCipher_Twofish;

    procedure Init(TestData: TCipherTestData);
    procedure Done;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIdentity;
    procedure TestContext;
    procedure TestEncode;
    procedure TestDecode;
    procedure TestClassByName;
  end;

  // Testmethods for class TCipher_IDEA
  {$IFDEF DUnitX} [TestFixture] {$ENDIF}
  TestTCipher_IDEA = class(TCipherBasis)
  strict private
    FCipher_IDEA: TCipher_IDEA;

    procedure Init(TestData: TCipherTestData);
    procedure Done;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIdentity;
    procedure TestContext;
    procedure TestEncode;
    procedure TestDecode;
    procedure TestClassByName;
  end;

  // Testmethods for class TCipher_Cast256
  {$IFDEF DUnitX} [TestFixture] {$ENDIF}
  TestTCipher_Cast256 = class(TCipherBasis)
  strict private
    FCipher_Cast256: TCipher_Cast256;

    procedure Init(TestData: TCipherTestData);
    procedure Done;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIdentity;
    procedure TestContext;
    procedure TestEncode;
    procedure TestDecode;
    procedure TestClassByName;
  end;

  // Testmethods for class TCipher_Mars
  {$IFDEF DUnitX} [TestFixture] {$ENDIF}
  TestTCipher_Mars = class(TCipherBasis)
  strict private
    FCipher_Mars: TCipher_Mars;

    procedure Init(TestData: TCipherTestData);
    procedure Done;
  private
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIdentity;
    procedure TestContext;
    procedure TestEncode;
    procedure TestDecode;
    procedure TestClassByName;
  end;

  // Testmethods for class TCipher_RC4
  {$IFDEF DUnitX} [TestFixture] {$ENDIF}
  TestTCipher_RC4 = class(TCipherBasis)
  strict private
    FCipher_RC4: TCipher_RC4;

    procedure Init(TestData: TCipherTestData);
    procedure Done;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIdentity;
    procedure TestContext;
    procedure TestEncode;
    procedure TestDecode;
    procedure TestClassByName;
  end;

  // Testmethods for class TCipher_RC6
  {$IFDEF DUnitX} [TestFixture] {$ENDIF}
  TestTCipher_RC6 = class(TCipherBasis)
  strict private
    FCipher_RC6: TCipher_RC6;

    procedure Init(TestData: TCipherTestData);
    procedure Done;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIdentity;
    procedure TestContext;
    procedure TestEncode;
    procedure TestDecode;
    procedure TestClassByName;
  end;

  // Testmethods for class TCipher_AES
  {$IFDEF DUnitX} [TestFixture] {$ENDIF}
  TestTCipher_AES = class(TCipherBasis)
  strict private
    FCipher_AES: TCipher_AES;

    procedure Init(TestData: TCipherTestData);
    procedure Done;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIdentity;
    procedure TestContext;
    procedure TestEncode;
    procedure TestDecode;
    procedure TestClassByName;
  end;

  // Testmethods for class TCipher_Rijndael which is an alias for AES as it's
  // the original name of that algorithm
  {$IFDEF DUnitX} [TestFixture] {$ENDIF}
  TestTCipher_Rijndael = class(TCipherBasis)
  strict private
    FCipher_Rijndael: TCipher_Rijndael;

    procedure Init(TestData: TCipherTestData);
    procedure Done;

    procedure DoTestClassByName;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIdentity;
    procedure TestContext;
    procedure TestEncode;
    procedure TestDecode;
    procedure TestClassByName;
  end;

  // Testmethods for class TCipher_Square
  {$IFDEF DUnitX} [TestFixture] {$ENDIF}
  TestTCipher_Square = class(TCipherBasis)
  strict private
    FCipher_Square: TCipher_Square;

    procedure Init(TestData: TCipherTestData);
    procedure Done;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIdentity;
    procedure TestContext;
    procedure TestEncode;
    procedure TestDecode;
    procedure TestClassByName;
  end;

  // Testmethods for class TCipher_SCOP
  {$IFDEF DUnitX} [TestFixture] {$ENDIF}
  TestTCipher_SCOP = class(TCipherBasis)
  strict private
    FCipher_SCOP: TCipher_SCOP;

    procedure Init(TestData: TCipherTestData);
    procedure Done;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIdentity;
    procedure TestContext;
    procedure TestEncode;
    procedure TestDecode;
    procedure TestClassByName;
  end;

  // Testmethods for class TCipher_SCOP_DEC52
  {$IFDEF DUnitX} [TestFixture] {$ENDIF}
  TestTCipher_SCOP_DEC52 = class(TCipherBasis)
  strict private
    FCipher_SCOP_DEC52: TCipher_SCOP_DEC52;

    procedure DoTestClassByName;

    procedure Init(TestData: TCipherTestData);
    procedure Done;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIdentity;
    procedure TestContext;
    procedure TestEncode;
    procedure TestDecode;
    procedure TestClassByName;
  end;

  // Testmethods for class TCipher_Sapphire
  {$IFDEF DUnitX} [TestFixture] {$ENDIF}
  TestTCipher_Sapphire = class(TCipherBasis)
  strict private
    FCipher_Sapphire: TCipher_Sapphire;

    procedure Init(TestData: TCipherTestData);
    procedure Done;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIdentity;
    procedure TestContext;
    procedure TestEncode;
    procedure TestDecode;
    procedure TestClassByName;
  end;

  // Testmethods for class TCipher_1DES
  {$IFDEF DUnitX} [TestFixture] {$ENDIF}
  TestTCipher_1DES = class(TCipherBasis)
  strict private
    FCipher_1DES: TCipher_1DES;

    procedure Init(TestData: TCipherTestData);
    procedure Done;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIdentity;
    procedure TestContext;
    procedure TestEncode;
    procedure TestDecode;
    procedure TestClassByName;
  end;

  // Testmethods for class TCipher_2DES
  {$IFDEF DUnitX} [TestFixture] {$ENDIF}
  TestTCipher_2DES = class(TCipherBasis)
  strict private
    FCipher_2DES: TCipher_2DES;

    procedure Init(TestData: TCipherTestData);
    procedure Done;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIdentity;
    procedure TestContext;
    procedure TestEncode;
    procedure TestDecode;
    procedure TestClassByName;
  end;

  // Testmethods for class TCipher_3DES
  {$IFDEF DUnitX} [TestFixture] {$ENDIF}
  TestTCipher_3DES = class(TCipherBasis)
  strict private
    FCipher_3DES: TCipher_3DES;

    procedure Init(TestData: TCipherTestData);
    procedure Done;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIdentity;
    procedure TestContext;
    procedure TestEncode;
    procedure TestDecode;
    procedure TestClassByName;
  end;

  // Testmethods for class TCipher_2DDES
  {$IFDEF DUnitX} [TestFixture] {$ENDIF}
  TestTCipher_2DDES = class(TCipherBasis)
  strict private
    FCipher_2DDES: TCipher_2DDES;

    procedure Init(TestData: TCipherTestData);
    procedure Done;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIdentity;
    procedure TestContext;
    procedure TestEncode;
    procedure TestDecode;
    procedure TestClassByName;
  end;

  // Testmethods for class TCipher_3DDES
  {$IFDEF DUnitX} [TestFixture] {$ENDIF}
  TestTCipher_3DDES = class(TCipherBasis)
  strict private
    FCipher_3DDES: TCipher_3DDES;

    procedure Init(TestData: TCipherTestData);
    procedure Done;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIdentity;
    procedure TestContext;
    procedure TestEncode;
    procedure TestDecode;
    procedure TestClassByName;
  end;

  // Testmethods for class TCipher_3TDES
  {$IFDEF DUnitX} [TestFixture] {$ENDIF}
  TestTCipher_3TDES = class(TCipherBasis)
  strict private
    FCipher_3TDES: TCipher_3TDES;

    procedure Init(TestData: TCipherTestData);
    procedure Done;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIdentity;
    procedure TestContext;
    procedure TestEncode;
    procedure TestDecode;
    procedure TestClassByName;
  end;

  // Testmethods for class TCipher_3Way
  {$IFDEF DUnitX} [TestFixture] {$ENDIF}
  TestTCipher_3Way = class(TCipherBasis)
  strict private
    FCipher_3Way: TCipher_3Way;

    procedure Init(TestData: TCipherTestData);
    procedure Done;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIdentity;
    procedure TestContext;
    procedure TestEncode;
    procedure TestDecode;
    procedure TestClassByName;
  end;

  // Testmethods for class TCipher_Cast128
  {$IFDEF DUnitX} [TestFixture] {$ENDIF}
  TestTCipher_Cast128 = class(TCipherBasis)
  strict private
    FCipher_Cast128: TCipher_Cast128;

    procedure Init(TestData: TCipherTestData);
    procedure Done;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIdentity;
    procedure TestContext;
    procedure TestEncode;
    procedure TestDecode;
    procedure TestClassByName;
  end;

  // Testmethods for class TCipher_Gost
  {$IFDEF DUnitX} [TestFixture] {$ENDIF}
  TestTCipher_Gost = class(TCipherBasis)
  strict private
    FCipher_Gost: TCipher_Gost;

    procedure Init(TestData: TCipherTestData);
    procedure Done;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIdentity;
    procedure TestContext;
    procedure TestEncode;
    procedure TestDecode;
    procedure TestClassByName;
  end;

  // Testmethods for class TCipher_Magma, which is an alias for Ghost
  {$IFDEF DUnitX} [TestFixture] {$ENDIF}
  TestTCipher_Magma = class(TCipherBasis)
  strict private
    FCipher_Magma: TCipher_Magma;

    procedure Init(TestData: TCipherTestData);
    procedure Done;

    procedure DoTestClassByName;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIdentity;
    procedure TestContext;
    procedure TestEncode;
    procedure TestDecode;
    procedure TestClassByName;
  end;

  // Testmethods for class TCipher_Misty
  {$IFDEF DUnitX} [TestFixture] {$ENDIF}
  TestTCipher_Misty = class(TCipherBasis)
  strict private
    FCipher_Misty: TCipher_Misty;

    procedure Init(TestData: TCipherTestData);
    procedure Done;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIdentity;
    procedure TestContext;
    procedure TestEncode;
    procedure TestDecode;
    procedure TestClassByName;
  end;

  // Testmethods for class TCipher_NewDES
  {$IFDEF DUnitX} [TestFixture] {$ENDIF}
  TestTCipher_NewDES = class(TCipherBasis)
  strict private
    FCipher_NewDES: TCipher_NewDES;

    procedure Init(TestData: TCipherTestData);
    procedure Done;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIdentity;
    procedure TestContext;
    procedure TestEncode;
    procedure TestDecode;
    procedure TestClassByName;
  end;

  // Testmethods for class TCipher_Q128
  {$IFDEF DUnitX} [TestFixture] {$ENDIF}
  TestTCipher_Q128 = class(TCipherBasis)
  strict private
    FCipher_Q128: TCipher_Q128;

    procedure Init(TestData: TCipherTestData);
    procedure Done;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIdentity;
    procedure TestContext;
    procedure TestEncode;
    procedure TestDecode;
    procedure TestClassByName;
  end;

  // Testmethods for class TCipher_RC2
  {$IFDEF DUnitX} [TestFixture] {$ENDIF}
  TestTCipher_RC2 = class(TCipherBasis)
  strict private
    FCipher_RC2: TCipher_RC2;

    procedure Init(TestData: TCipherTestData);
    procedure Done;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIdentity;
    procedure TestContext;
    procedure TestEncode;
    procedure TestDecode;
    procedure TestClassByName;
  end;

  // Testmethods for class TCipher_RC5
  {$IFDEF DUnitX} [TestFixture] {$ENDIF}
  TestTCipher_RC5 = class(TCipherBasis)
  strict private
    FCipher_RC5: TCipher_RC5;

    procedure Init(TestData: TCipherTestData);
    procedure Done;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIdentity;
    procedure TestContext;
    procedure TestEncode;
    procedure TestDecode;
    procedure TestClassByName;
  end;

  // Testmethods for class TCipher_SAFER
  {$IFDEF DUnitX} [TestFixture] {$ENDIF}
  TestTCipher_SAFER = class(TCipherBasis)
  strict private
    FCipher_SAFER: TCipher_SAFER;

    procedure Init(TestData: TCipherTestData);
    procedure Done;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIdentity;
    procedure TestContext;
    procedure TestEncode;
    procedure TestDecode;
    procedure TestClassByName;
  end;

  // Testmethods for class TCipher_Shark
  {$IFDEF DUnitX} [TestFixture] {$ENDIF}
  TestTCipher_Shark = class(TCipherBasis)
  strict private
    FCipher_Shark: TCipher_Shark;

    procedure Init(TestData: TCipherTestData);
    procedure Done;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIdentity;
    procedure TestContext;
    procedure TestEncode;
    procedure TestDecode;
    procedure TestClassByName;
  end;

  // Testmethods for class TCipher_Shark_DEC52
  {$IFDEF DUnitX} [TestFixture] {$ENDIF}
  TestTCipher_Shark_DEC52 = class(TCipherBasis)
  strict private
    FCipher_Shark_DEC52: TCipher_Shark_DEC52;

    procedure DoTestClassByName;

    procedure Init(TestData: TCipherTestData);
    procedure Done;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIdentity;
    procedure TestContext;
    procedure TestEncode;
    procedure TestDecode;
    procedure TestClassByName;
  end;

  // Testmethods for class TCipher_Skipjack
  {$IFDEF DUnitX} [TestFixture] {$ENDIF}
  TestTCipher_Skipjack = class(TCipherBasis)
  strict private
    FCipher_Skipjack: TCipher_Skipjack;

    procedure Init(TestData: TCipherTestData);
    procedure Done;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIdentity;
    procedure TestContext;
    procedure TestEncode;
    procedure TestDecode;
    procedure TestClassByName;
  end;

  // Testmethods for class TCipher_TEA
  {$IFDEF DUnitX} [TestFixture] {$ENDIF}
  TestTCipher_TEA = class(TCipherBasis)
  strict private
    FCipher_TEA: TCipher_TEA;

    procedure Init(TestData: TCipherTestData);
    procedure Done;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIdentity;
    procedure TestContext;
    procedure TestEncode;
    procedure TestDecode;
    procedure TestClassByName;
  end;

  // Testmethods for class TCipher_XTEA
  {$IFDEF DUnitX} [TestFixture] {$ENDIF}
  TestTCipher_XTEA = class(TCipherBasis)
  strict private
    FCipher_XTEA: TCipher_XTEA;

    procedure Init(TestData: TCipherTestData);
    procedure Done;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIdentity;
    procedure TestContext;
    procedure TestEncode;
    procedure TestDecode;
    procedure TestClassByName;
  end;

  // Testmethods for class TCipher_XTEA_DEC52
  {$IFDEF DUnitX} [TestFixture] {$ENDIF}
  TestTCipher_XTEA_DEC52 = class(TCipherBasis)
  strict private
    FCipher_XTEA_DEC52: TCipher_XTEA_DEC52;

    procedure DoTestClassByName;

    procedure Init(TestData: TCipherTestData);
    procedure Done;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIdentity;
    procedure TestContext;
    procedure TestEncode;
    procedure TestDecode;
    procedure TestClassByName;
  end;

implementation

const
  cZeroBlock8  = #$00#$00#$00#$00#$00#$00#$00#$00;
  cFFBlock8    = 'FFFFFFFFFFFFFFFF';
  cZeroBlock16 = #$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00;
  cFFBlock16   = 'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF';

procedure TestTCipher_Null.SetUp;
begin
  FCipher_Null := TCipher_Null.Create;
end;

procedure TestTCipher_Null.TearDown;
begin
  FCipher_Null.Free;
  FCipher_Null := nil;
end;

procedure TestTCipher_Null.TestClassByName;
var
  ReturnValue : TDECCipherClass;
begin
  ReturnValue := FCipher_Null.ClassByName('TCipher_Null');
  CheckEquals(TCipher_Null, ReturnValue, 'Class is not registered');
end;

procedure TestTCipher_Null.TestContext;
var
  ReturnValue: TCipherContext;
begin
  ReturnValue := FCipher_Null.Context;

  CheckEquals(   0,  ReturnValue.KeySize);
  CheckEquals(   1,  ReturnValue.BlockSize);
  CheckEquals(   8,  ReturnValue.BufferSize);
  CheckEquals(   0,  ReturnValue.AdditionalBufferSize);
  CheckEquals(   1,  ReturnValue.MinRounds);
  CheckEquals(   1,  ReturnValue.MaxRounds);
  CheckEquals(false, ReturnValue.NeedsAdditionalBufferBackup);
  CheckEquals(true,  [ctNull, ctSymmetric] = ReturnValue.CipherType);
end;

procedure TestTCipher_Null.TestIdentity;
begin
  CheckEquals($1678C79D, FCipher_Null.Identity);
end;

procedure TestTCipher_Blowfish.Done;
begin
  FCipher_Blowfish.Done;
end;

procedure TestTCipher_Blowfish.Init(TestData: TCipherTestData);
begin
  LimitKeyLength(TestData.Key, FCipher_Blowfish.Context.KeySize);

  FCipher_Blowfish.Mode := TestData.Mode;
  FCipher_Blowfish.Init(BytesOf(TestData.Key),
                        BytesOf(TestData.InitVector),
                        TestData.Filler);
end;

procedure TestTCipher_Blowfish.SetUp;
begin
  FCipher_Blowfish      := TCipher_Blowfish.Create;

  SetLength(FTestData, 1);

  FTestData[0].OutputData  := '1971cacd2b9c8529da8147b7ebce16c6910e1dc840123e3570edbc964c13d0b8';
  FTestData[0].InputData   := TFormat_ESCAPE.Decode('\x30\x44\xED\x6E\x45\xA4' +
                                                    '\x96\xF5\xF6\x35\xA2\xEB' +
                                                    '\x3D\x1A\x5D\xD6\xCB\x1D' +
                                                    '\x09\x82\x2D\xBD\xF5\x60' +
                                                    '\xC2\xB8\x58\xA1\x91\xF9' +
                                                    '\x81\xB1');

  FTestData[0].Key        := 'TCipher_Blowfish';
  FTestData[0].InitVector := '';
  FTestData[0].Filler     := $FF;
  FTestData[0].Mode       := cmCTSx;
end;

procedure TestTCipher_Blowfish.TearDown;
begin
  FCipher_Blowfish.Free;
  FCipher_Blowfish := nil;
end;

procedure TestTCipher_Blowfish.TestContext;
var
  ReturnValue: TCipherContext;
begin
  ReturnValue := FCipher_Blowfish.Context;

  CheckEquals(  56,  ReturnValue.KeySize);
  CheckEquals(   8,  ReturnValue.BlockSize);
  CheckEquals(   8,  ReturnValue.BufferSize);
  CheckEquals(4168,  ReturnValue.AdditionalBufferSize);
  CheckEquals(   1,  ReturnValue.MinRounds);
  CheckEquals(   1,  ReturnValue.MaxRounds);
  CheckEquals(false, ReturnValue.NeedsAdditionalBufferBackup);
  CheckEquals(true,  [ctBlock, ctSymmetric] = ReturnValue.CipherType);
end;

procedure TestTCipher_Blowfish.TestDecode;
begin
  DoTestDecode(FCipher_Blowfish.DecodeStringToBytes, self.Init, self.Done);
end;

procedure TestTCipher_Blowfish.TestEncode;
begin
  DoTestEncode(FCipher_Blowfish.EncodeStringToBytes, self.Init, self.Done);
end;

procedure TestTCipher_Blowfish.TestIdentity;
begin
  CheckEquals($54E9A294, FCipher_Blowfish.Identity);
end;

procedure TestTCipher_Blowfish.TestClassByName;
var
  ReturnValue : TDECCipherClass;
begin
  ReturnValue := FCipher_Blowfish.ClassByName('TCipher_Blowfish');
  CheckEquals(TCipher_Blowfish, ReturnValue, 'Class is not registered');
end;

procedure TestTCipher_Twofish.Done;
begin
  FCipher_Twofish.Done;
end;

procedure TestTCipher_Twofish.Init(TestData: TCipherTestData);
begin
  LimitKeyLength(TestData.Key, FCipher_Twofish.Context.KeySize);

  FCipher_Twofish.Mode := TestData.Mode;
  FCipher_Twofish.Init(BytesOf(TestData.Key),
                       BytesOf(TestData.InitVector),
                       TestData.Filler);
end;

procedure TestTCipher_Twofish.SetUp;
begin
  FCipher_Twofish := TCipher_Twofish.Create;

  SetLength(FTestData, 1);

  FTestData[0].OutputData  := 'e81674f9bc69442188c949bb52e1e47874171177e99dbbe9880875094f8dfe21';
  FTestData[0].InputData   := TFormat_ESCAPE.Decode('\x30\x44\xED\x6E\x45\xA4' +
                                                    '\x96\xF5\xF6\x35\xA2\xEB' +
                                                    '\x3D\x1A\x5D\xD6\xCB\x1D' +
                                                    '\x09\x82\x2D\xBD\xF5\x60' +
                                                    '\xC2\xB8\x58\xA1\x91\xF9' +
                                                    '\x81\xB1');

  FTestData[0].Key        := 'TCipher_Twofish';
  FTestData[0].InitVector := '';
  FTestData[0].Filler     := $FF;
  FTestData[0].Mode       := cmCTSx;
end;

procedure TestTCipher_Twofish.TearDown;
begin
  FCipher_Twofish.Free;
  FCipher_Twofish := nil;
end;

procedure TestTCipher_Twofish.TestContext;
var
  ReturnValue: TCipherContext;
begin
  ReturnValue := FCipher_Twofish.Context;

  CheckEquals(  32,  ReturnValue.KeySize);
  CheckEquals(  16,  ReturnValue.BlockSize);
  CheckEquals(  16,  ReturnValue.BufferSize);
  CheckEquals(4256,  ReturnValue.AdditionalBufferSize);
  CheckEquals(   1,  ReturnValue.MinRounds);
  CheckEquals(   1,  ReturnValue.MaxRounds);
  CheckEquals(false, ReturnValue.NeedsAdditionalBufferBackup);
  CheckEquals(true,  [ctBlock, ctSymmetric] = ReturnValue.CipherType);
end;

procedure TestTCipher_Twofish.TestDecode;
begin
  DoTestDecode(FCipher_Twofish.DecodeStringToBytes, self.Init, self.Done);
end;

procedure TestTCipher_Twofish.TestEncode;
begin
  DoTestEncode(FCipher_Twofish.EncodeStringToBytes, self.Init, self.Done);
end;

procedure TestTCipher_Twofish.TestIdentity;
begin
  CheckEquals($B38AB3E6, FCipher_Twofish.Identity);
end;

procedure TestTCipher_Twofish.TestClassByName;
var
  ReturnValue : TDECCipherClass;
begin
  ReturnValue := FCipher_Twofish.ClassByName('TCipher_Twofish');
  CheckEquals(TCipher_Twofish, ReturnValue, 'Class is not registered');
end;

procedure TestTCipher_IDEA.Done;
begin
  FCipher_IDEA.Done;
end;

procedure TestTCipher_IDEA.Init(TestData: TCipherTestData);
begin
  LimitKeyLength(TestData.Key, FCipher_IDEA.Context.KeySize);

  FCipher_IDEA.Mode := TestData.Mode;
  FCipher_IDEA.Init(BytesOf(TestData.Key),
                    BytesOf(TestData.InitVector),
                    TestData.Filler);
end;

procedure TestTCipher_IDEA.SetUp;
begin
  FCipher_IDEA := TCipher_IDEA.Create;

  SetLength(FTestData, 1);

  FTestData[0].OutputData  := '8c65cad843e79993ed41ea48fd665094a2256dd7b1d09a233dd2e8ecc9457f7e';
  FTestData[0].InputData   := TFormat_ESCAPE.Decode('\x30\x44\xED\x6E\x45\xA4' +
                                                    '\x96\xF5\xF6\x35\xA2\xEB' +
                                                    '\x3D\x1A\x5D\xD6\xCB\x1D' +
                                                    '\x09\x82\x2D\xBD\xF5\x60' +
                                                    '\xC2\xB8\x58\xA1\x91\xF9'+
                                                    '\x81\xB1');

  FTestData[0].Key        := 'TCipher_IDEA';
  FTestData[0].InitVector := '';
  FTestData[0].Filler     := $FF;
  FTestData[0].Mode       := cmCTSx;
end;

procedure TestTCipher_IDEA.TearDown;
begin
  FCipher_IDEA.Free;
  FCipher_IDEA := nil;
end;

procedure TestTCipher_IDEA.TestContext;
var
  ReturnValue: TCipherContext;
begin
  ReturnValue := FCipher_IDEA.Context;

  CheckEquals(  16,  ReturnValue.KeySize);
  CheckEquals(   8,  ReturnValue.BlockSize);
  CheckEquals(   8,  ReturnValue.BufferSize);
  CheckEquals( 208,  ReturnValue.AdditionalBufferSize);
  CheckEquals(   1,  ReturnValue.MinRounds);
  CheckEquals(   1,  ReturnValue.MaxRounds);
  CheckEquals(false, ReturnValue.NeedsAdditionalBufferBackup);
  CheckEquals(true,  [ctBlock, ctSymmetric] = ReturnValue.CipherType);
end;

procedure TestTCipher_IDEA.TestDecode;
begin
  DoTestDecode(FCipher_IDEA.DecodeStringToBytes, self.Init, self.Done);
end;

procedure TestTCipher_IDEA.TestEncode;
begin
  DoTestEncode(FCipher_IDEA.EncodeStringToBytes, self.Init, self.Done);
end;

procedure TestTCipher_IDEA.TestIdentity;
begin
  CheckEquals($3938F197, FCipher_IDEA.Identity);
end;

procedure TestTCipher_IDEA.TestClassByName;
var
  ReturnValue : TDECCipherClass;
begin
  ReturnValue := FCipher_IDEA.ClassByName('TCipher_IDEA');
  CheckEquals(TCipher_IDEA, ReturnValue, 'Class is not registered');
end;

procedure TestTCipher_Cast256.Done;
begin
  FCipher_Cast256.Done;
end;

procedure TestTCipher_Cast256.Init(TestData: TCipherTestData);
begin
  LimitKeyLength(TestData.Key, FCipher_Cast256.Context.KeySize);

  FCipher_Cast256.Mode := TestData.Mode;
  FCipher_Cast256.Init(BytesOf(TestData.Key),
                    BytesOf(TestData.InitVector),
                    TestData.Filler);
end;

procedure TestTCipher_Cast256.SetUp;
begin
  FCipher_Cast256 := TCipher_Cast256.Create;

  SetLength(FTestData, 1);

  FTestData[0].OutputData  := '45820e97772071993e2945d5594feca5cd583875469ca7c5faa6339c82fb9254';
  FTestData[0].InputData   := TFormat_ESCAPE.Decode('\x30\x44\xED\x6E\x45\xA4' +
                                                    '\x96\xF5\xF6\x35\xA2\xEB' +
                                                    '\x3D\x1A\x5D\xD6\xCB\x1D' +
                                                    '\x09\x82\x2D\xBD\xF5\x60' +
                                                    '\xC2\xB8\x58\xA1\x91\xF9' +
                                                    '\x81\xB1');

  FTestData[0].Key        := 'TCipher_CAST256';
  FTestData[0].InitVector := '';
  FTestData[0].Filler     := $FF;
  FTestData[0].Mode       := cmCTSx;
end;

procedure TestTCipher_Cast256.TearDown;
begin
  FCipher_Cast256.Free;
  FCipher_Cast256 := nil;
end;

procedure TestTCipher_Cast256.TestContext;
var
  ReturnValue: TCipherContext;
begin
  ReturnValue := FCipher_Cast256.Context;

  CheckEquals(  32,  ReturnValue.KeySize);
  CheckEquals(  16,  ReturnValue.BlockSize);
  CheckEquals(  16,  ReturnValue.BufferSize);
  CheckEquals( 384,  ReturnValue.AdditionalBufferSize);
  CheckEquals(   1,  ReturnValue.MinRounds);
  CheckEquals(   1,  ReturnValue.MaxRounds);
  CheckEquals(false, ReturnValue.NeedsAdditionalBufferBackup);
  CheckEquals(true,  [ctBlock, ctSymmetric] = ReturnValue.CipherType);
end;

procedure TestTCipher_Cast256.TestDecode;
begin
  DoTestDecode(FCipher_Cast256.DecodeStringToBytes, self.Init, self.Done);
end;

procedure TestTCipher_Cast256.TestEncode;
begin
  DoTestEncode(FCipher_Cast256.EncodeStringToBytes, self.Init, self.Done);
end;

procedure TestTCipher_Cast256.TestIdentity;
begin
  CheckEquals($47C2021C, FCipher_Cast256.Identity);
end;

procedure TestTCipher_Cast256.TestClassByName;
var
  ReturnValue : TDECCipherClass;
begin
  ReturnValue := FCipher_Cast256.ClassByName('TCipher_Cast256');
  CheckEquals(TCipher_Cast256, ReturnValue, 'Class is not registered');
end;

procedure TestTCipher_Mars.Done;
begin
  FCipher_Mars.Done;
end;

procedure TestTCipher_Mars.Init(TestData: TCipherTestData);
begin
  LimitKeyLength(TestData.Key, FCipher_Mars.Context.KeySize);

  FCipher_Mars.Mode := TestData.Mode;
  FCipher_Mars.Init(BytesOf(TestData.Key),
                    BytesOf(TestData.InitVector),
                    TestData.Filler);
end;

procedure TestTCipher_Mars.SetUp;
begin
  FCipher_Mars := TCipher_Mars.Create;

  SetLength(FTestData, 1);

  FTestData[0].OutputData  := 'fda54d3c1d79739ceaf668675595e210b145bfa9e4ab65efaa68c88ea34ab09d';
  FTestData[0].InputData   := TFormat_ESCAPE.Decode('\x30\x44\xED\x6E\x45\xA4' +
                                                    '\x96\xF5\xF6\x35\xA2\xEB' +
                                                    '\x3D\x1A\x5D\xD6\xCB\x1D' +
                                                    '\x09\x82\x2D\xBD\xF5\x60' +
                                                    '\xC2\xB8\x58\xA1\x91\xF9' +
                                                    '\x81\xB1');

  FTestData[0].Key        := 'TCipher_Mars';
  FTestData[0].InitVector := '';
  FTestData[0].Filler     := $FF;
  FTestData[0].Mode       := cmCTSx;
end;

procedure TestTCipher_Mars.TearDown;
begin
  FCipher_Mars.Free;
  FCipher_Mars := nil;
end;

procedure TestTCipher_Mars.TestContext;
var
  ReturnValue: TCipherContext;
begin
  ReturnValue := FCipher_Mars.Context;

  CheckEquals(  56,  ReturnValue.KeySize);
  CheckEquals(  16,  ReturnValue.BlockSize);
  CheckEquals(  16,  ReturnValue.BufferSize);
  CheckEquals( 160,  ReturnValue.AdditionalBufferSize);
  CheckEquals(   1,  ReturnValue.MinRounds);
  CheckEquals(   1,  ReturnValue.MaxRounds);
  CheckEquals(false, ReturnValue.NeedsAdditionalBufferBackup);
  CheckEquals(true,  [ctBlock, ctSymmetric] = ReturnValue.CipherType);
end;

procedure TestTCipher_Mars.TestDecode;
begin
  DoTestDecode(FCipher_Mars.DecodeStringToBytes, self.Init, self.Done);
end;

procedure TestTCipher_Mars.TestEncode;
begin
  DoTestEncode(FCipher_Mars.EncodeStringToBytes, self.Init, self.Done);
end;

procedure TestTCipher_Mars.TestIdentity;
begin
  CheckEquals($46AB51F5, FCipher_Mars.Identity);
end;

procedure TestTCipher_Mars.TestClassByName;
// ClassByName Tests for die restlichen Ciphers umsetzen!
var
  ReturnValue : TDECCipherClass;
begin
  ReturnValue := FCipher_Mars.ClassByName('TCipher_Mars');
  CheckEquals(TCipher_Mars, ReturnValue, 'Class is not registered');
end;

procedure TestTCipher_RC4.Done;
begin
  FCipher_RC4.Done;
end;

procedure TestTCipher_RC4.Init(TestData: TCipherTestData);
begin
  LimitKeyLength(TestData.Key, FCipher_RC4.Context.KeySize);

  FCipher_RC4.Mode := TestData.Mode;
  FCipher_RC4.Init(BytesOf(TestData.Key),
                    BytesOf(TestData.InitVector),
                    TestData.Filler);
end;

procedure TestTCipher_RC4.SetUp;
begin
  FCipher_RC4 := TCipher_RC4.Create;

  SetLength(FTestData, 1);

  FTestData[0].OutputData  := 'cfbb1291ba5b690a09ca5d14c2e5a229196d183f5539a4edc56c2bfb7c12d630';
  FTestData[0].InputData   := TFormat_ESCAPE.Decode('\x30\x44\xED\x6E\x45\xA4' +
                                                    '\x96\xF5\xF6\x35\xA2\xEB' +
                                                    '\x3D\x1A\x5D\xD6\xCB\x1D' +
                                                    '\x09\x82\x2D\xBD\xF5\x60' +
                                                    '\xC2\xB8\x58\xA1\x91\xF9' +
                                                    '\x81\xB1');

  FTestData[0].Key        := 'TCipher_RC4';
  FTestData[0].InitVector := '';
  FTestData[0].Filler     := $FF;
  FTestData[0].Mode       := cmCTSx;
end;

procedure TestTCipher_RC4.TearDown;
begin
  FCipher_RC4.Free;
  FCipher_RC4 := nil;
end;

procedure TestTCipher_RC4.TestClassByName;
var
  ReturnValue : TDECCipherClass;
begin
  ReturnValue := FCipher_RC4.ClassByName('TCipher_RC4');
  CheckEquals(TCipher_RC4, ReturnValue, 'Class is not registered');
end;

procedure TestTCipher_RC4.TestContext;
var
  ReturnValue: TCipherContext;
begin
  ReturnValue := FCipher_RC4.Context;

  CheckEquals( 256,  ReturnValue.KeySize);
  CheckEquals(   1,  ReturnValue.BlockSize);
  CheckEquals(  16,  ReturnValue.BufferSize);
  CheckEquals( 258,  ReturnValue.AdditionalBufferSize);
  CheckEquals(   1,  ReturnValue.MinRounds);
  CheckEquals(   1,  ReturnValue.MaxRounds);
  CheckEquals(true,  ReturnValue.NeedsAdditionalBufferBackup);
  CheckEquals(true,  [ctStream, ctSymmetric] = ReturnValue.CipherType);
end;

procedure TestTCipher_RC4.TestDecode;
begin
  DoTestDecode(FCipher_RC4.DecodeStringToBytes, self.Init, self.Done);
end;

procedure TestTCipher_RC4.TestEncode;
begin
  DoTestEncode(FCipher_RC4.EncodeStringToBytes, self.Init, self.Done);
end;

procedure TestTCipher_RC4.TestIdentity;
begin
  CheckEquals($73A3DF5A, FCipher_RC4.Identity);
end;

procedure TestTCipher_RC6.Done;
begin
  FCipher_RC6.Done;
end;

procedure TestTCipher_RC6.Init(TestData: TCipherTestData);
begin
  LimitKeyLength(TestData.Key, FCipher_RC6.Context.KeySize);

  FCipher_RC6.Mode := TestData.Mode;
  FCipher_RC6.Init(BytesOf(TestData.Key),
                    BytesOf(TestData.InitVector),
                    TestData.Filler);
end;

procedure TestTCipher_RC6.SetUp;
begin
  FCipher_RC6 := TCipher_RC6.Create;

  SetLength(FTestData, 1);

  FTestData[0].OutputData  := '987165a110febdf907853efc21dbfca18f5f8bf74528810def9a227af0622cc6';
  FTestData[0].InputData   := TFormat_ESCAPE.Decode('\x30\x44\xED\x6E\x45\xA4' +
                                                    '\x96\xF5\xF6\x35\xA2\xEB' +
                                                    '\x3D\x1A\x5D\xD6\xCB\x1D' +
                                                    '\x09\x82\x2D\xBD\xF5\x60' +
                                                    '\xC2\xB8\x58\xA1\x91\xF9' +
                                                    '\x81\xB1');

  FTestData[0].Key        := 'TCipher_RC6';
  FTestData[0].InitVector := '';
  FTestData[0].Filler     := $FF;
  FTestData[0].Mode       := cmCTSx;
end;

procedure TestTCipher_RC6.TearDown;
begin
  FCipher_RC6.Free;
  FCipher_RC6 := nil;
end;

procedure TestTCipher_RC6.TestClassByName;
var
  ReturnValue : TDECCipherClass;
begin
  ReturnValue := FCipher_RC6.ClassByName('TCipher_RC6');
  CheckEquals(TCipher_RC6, ReturnValue, 'Class is not registered');
end;

procedure TestTCipher_RC6.TestContext;
var
  ReturnValue: TCipherContext;
begin
  ReturnValue := FCipher_RC6.Context;

  CheckEquals( 256,  ReturnValue.KeySize);
  CheckEquals(  16,  ReturnValue.BlockSize);
  CheckEquals(  16,  ReturnValue.BufferSize);
  CheckEquals( 272,  ReturnValue.AdditionalBufferSize);
  CheckEquals(  16,  ReturnValue.MinRounds);
  CheckEquals(  24,  ReturnValue.MaxRounds);
  CheckEquals(false, ReturnValue.NeedsAdditionalBufferBackup);
  CheckEquals(true,  [ctBlock, ctSymmetric] = ReturnValue.CipherType);
end;

procedure TestTCipher_RC6.TestDecode;
begin
  DoTestDecode(FCipher_RC6.DecodeStringToBytes, self.Init, self.Done);
end;

procedure TestTCipher_RC6.TestEncode;
begin
  DoTestEncode(FCipher_RC6.EncodeStringToBytes, self.Init, self.Done);
end;

procedure TestTCipher_RC6.TestIdentity;
begin
  CheckEquals($9DADBE76, FCipher_RC6.Identity);
end;

procedure TestTCipher_Square.Done;
begin
  FCipher_Square.Done;
end;

procedure TestTCipher_Square.Init(TestData: TCipherTestData);
begin
  LimitKeyLength(TestData.Key, FCipher_Square.Context.KeySize);

  FCipher_Square.Mode := TestData.Mode;
  FCipher_Square.Init(BytesOf(TestData.Key),
                    BytesOf(TestData.InitVector),
                    TestData.Filler);
end;

procedure TestTCipher_Square.SetUp;
begin
  FCipher_Square := TCipher_Square.Create;

  SetLength(FTestData, 1);

  FTestData[0].OutputData  := '439ca6c467e82e472295668506396ac9182120f74436f1617d1490b1a96856c7';
  FTestData[0].InputData   := TFormat_ESCAPE.Decode('\x30\x44\xED\x6E\x45\xA4' +
                                                    '\x96\xF5\xF6\x35\xA2\xEB' +
                                                    '\x3D\x1A\x5D\xD6\xCB\x1D' +
                                                    '\x09\x82\x2D\xBD\xF5\x60' +
                                                    '\xC2\xB8\x58\xA1\x91\xF9' +
                                                    '\x81\xB1');

  FTestData[0].Key        := 'TCipher_Square';
  FTestData[0].InitVector := '';
  FTestData[0].Filler     := $FF;
  FTestData[0].Mode       := cmCTSx;
end;

procedure TestTCipher_Square.TearDown;
begin
  FCipher_Square.Free;
  FCipher_Square := nil;
end;

procedure TestTCipher_Square.TestClassByName;
var
  ReturnValue : TDECCipherClass;
begin
  ReturnValue := FCipher_Square.ClassByName('TCipher_Square');
  CheckEquals(TCipher_Square, ReturnValue, 'Class is not registered');
end;

procedure TestTCipher_Square.TestContext;
var
  ReturnValue: TCipherContext;
begin
  ReturnValue := FCipher_Square.Context;

  CheckEquals(  16,  ReturnValue.KeySize);
  CheckEquals(  16,  ReturnValue.BlockSize);
  CheckEquals(  16,  ReturnValue.BufferSize);
  CheckEquals( 288,  ReturnValue.AdditionalBufferSize);
  CheckEquals(   1,  ReturnValue.MinRounds);
  CheckEquals(   1,  ReturnValue.MaxRounds);
  CheckEquals(false, ReturnValue.NeedsAdditionalBufferBackup);
  CheckEquals(true,  [ctBlock, ctSymmetric] = ReturnValue.CipherType);
end;

procedure TestTCipher_Square.TestDecode;
begin
  DoTestDecode(FCipher_Square.DecodeStringToBytes, self.Init, self.Done);
end;

procedure TestTCipher_Square.TestEncode;
begin
  DoTestEncode(FCipher_Square.EncodeStringToBytes, self.Init, self.Done);
end;

procedure TestTCipher_Square.TestIdentity;
begin
  CheckEquals($2954C319, FCipher_Square.Identity);
end;

procedure TestTCipher_SCOP.Done;
begin
  FCipher_SCOP.Done;
end;

procedure TestTCipher_SCOP.Init(TestData: TCipherTestData);
begin
  LimitKeyLength(TestData.Key, FCipher_SCOP.Context.KeySize);

  FCipher_SCOP.Mode := TestData.Mode;
  FCipher_SCOP.Init(BytesOf(TestData.Key),
                    BytesOf(TestData.InitVector),
                    TestData.Filler);
end;

procedure TestTCipher_SCOP.SetUp;
begin
  FCipher_SCOP := TCipher_SCOP.Create;

  SetLength(FTestData, 18);

  // Standard test vector
  FTestData[0].OutputData  := 'ca29853fb7eec7f958931ff185c0b415c944c22f13e34423aba1a84fb3101f19';
  FTestData[0].InputData   := TFormat_ESCAPE.Decode('\x30\x44\xED\x6E\x45\xA4' +
                                                    '\x96\xF5\xF6\x35\xA2\xEB' +
                                                    '\x3D\x1A\x5D\xD6\xCB\x1D' +
                                                    '\x09\x82\x2D\xBD\xF5\x60' +
                                                    '\xC2\xB8\x58\xA1\x91\xF9' +
                                                    '\x81\xB1');
  FTestData[0].Key        := 'TCipher_SCOP';
  FTestData[0].InitVector := '';
  FTestData[0].Filler     := $FF;
  FTestData[0].Mode       := cmECBx;

  // Standard test vector, key with 'odd' bit set
  FTestData[1].OutputData  := '18be1fff893de279b5768b21307c5c52436835c83ed5c96ea589884b61c69dc4';
  FTestData[1].InputData   := TFormat_ESCAPE.Decode('\x30\x44\xED\x6E\x45\xA4' +
                                                    '\x96\xF5\xF6\x35\xA2\xEB' +
                                                    '\x3D\x1A\x5D\xD6\xCB\x1D' +
                                                    '\x09\x82\x2D\xBD\xF5\x60' +
                                                    '\xC2\xB8\x58\xA1\x91\xF9' +
                                                    '\x81\xB1');
  FTestData[1].Key        := 'TCipher_SCOPa';
  FTestData[1].InitVector := '';
  FTestData[1].Filler     := $FF;
  FTestData[1].Mode       := cmECBx;

  // Full 48 bytes key length
  FTestData[2].OutputData := '8dbc6579ac264ccfbb0f7aea';
  FTestData[2].InputData  := '12bytesbytes';
  FTestData[2].Key        := 'TCipher_SCOPTCipher_SCOPTCipher_SCOPTCipher_SCOP';
  FTestData[2].InitVector := '';
  FTestData[2].Filler     := $FF;
  FTestData[2].Mode       := cmECBx;

  // Source until SourceEnd: Test data as generated from the original SCOP test
  //  program written in C
  FTestData[3].OutputData := 'ce5d5f193d3b9d41f06c6135c3a3f66dbe0d798d58200d5d21a2727ba6b998afe04bb0de8a3cbb7d';
  FTestData[3].InputData  := #$0#$0#$0#$0#$0#$0#$0#$0#$0#$0#$0#$0#$0#$0#$0#$0#$0#$0#$0#$0 +
                             #$0#$0#$0#$0#$0#$0#$0#$0#$0#$0#$0#$0#$0#$0#$0#$0#$0#$0#$0#$0;
  FTestData[3].Key        := #0#1#2#3#4#5#6#7#8#9#$A#$B#$C#$D#$E#$F;
  FTestData[3].InitVector := '';
  FTestData[3].Filler     := $FF;
  FTestData[3].Mode       := cmECBx;

  FTestData[4].OutputData := 'cf5e601a';
  FTestData[4].InputData  := #$1#$1#$1#$1;
  FTestData[4].Key        := #0#1#2#3#4#5#6#7#8#9#$A#$B#$C#$D#$E#$F;
  FTestData[4].InitVector := '';
  FTestData[4].Filler     := $FF;
  FTestData[4].Mode       := cmECBx;

  FTestData[5].OutputData := 'cf5e601b';
  FTestData[5].InputData  := #$1#$1#$1#$2;
  FTestData[5].Key        := #0#1#2#3#4#5#6#7#8#9#$A#$B#$C#$D#$E#$F;
  FTestData[5].InitVector := '';
  FTestData[5].Filler     := $FF;
  FTestData[5].Mode       := cmECBx;

  FTestData[6].OutputData := 'cf5e611b';
  FTestData[6].InputData  := #$1#$1#$2#$2;
  FTestData[6].Key        := #0#1#2#3#4#5#6#7#8#9#$A#$B#$C#$D#$E#$F;
  FTestData[6].InitVector := '';
  FTestData[6].Filler     := $FF;
  FTestData[6].Mode       := cmECBx;

  FTestData[7].OutputData := 'cf5f611b';
  FTestData[7].InputData  := #$1#$2#$2#$2;
  FTestData[7].Key        := #0#1#2#3#4#5#6#7#8#9#$A#$B#$C#$D#$E#$F;
  FTestData[7].InitVector := '';
  FTestData[7].Filler     := $FF;
  FTestData[7].Mode       := cmECBx;

  FTestData[8].OutputData := 'd05f611b';
  FTestData[8].InputData  := #$2#$2#$2#$2;
  FTestData[8].Key        := #0#1#2#3#4#5#6#7#8#9#$A#$B#$C#$D#$E#$F;
  FTestData[8].InitVector := '';
  FTestData[8].Filler     := $FF;
  FTestData[8].Mode       := cmECBx;

  FTestData[9].OutputData := 'cf5e601a3e3c9e42';
  FTestData[9].InputData  := #$1#$1#$1#$1#$1#$1#$1#$1;
  FTestData[9].Key        := #0#1#2#3#4#5#6#7#8#9#$A#$B#$C#$D#$E#$F;
  FTestData[9].InitVector := '';
  FTestData[9].Filler     := $FF;
  FTestData[9].Mode       := cmECBx;

  FTestData[10].OutputData := 'cf5e601a3e3c9e43';
  FTestData[10].InputData  := #$1#$1#$1#$1#$1#$1#$1#$2;
  FTestData[10].Key        := #0#1#2#3#4#5#6#7#8#9#$A#$B#$C#$D#$E#$F;
  FTestData[10].InitVector := '';
  FTestData[10].Filler     := $FF;
  FTestData[10].Mode       := cmECBx;

  FTestData[11].OutputData := 'cf5e601a3e3c9f43';
  FTestData[11].InputData  := #$1#$1#$1#$1#$1#$1#$2#$2;
  FTestData[11].Key        := #0#1#2#3#4#5#6#7#8#9#$A#$B#$C#$D#$E#$F;
  FTestData[11].InitVector := '';
  FTestData[11].Filler     := $FF;
  FTestData[11].Mode       := cmECBx;

  FTestData[12].OutputData := 'cf5e601a3e3d9f43';
  FTestData[12].InputData  := #$1#$1#$1#$1#$1#$2#$2#$2;
  FTestData[12].Key        := #0#1#2#3#4#5#6#7#8#9#$A#$B#$C#$D#$E#$F;
  FTestData[12].InitVector := '';
  FTestData[12].Filler     := $FF;
  FTestData[12].Mode       := cmECBx;

  FTestData[13].OutputData := 'cf5e601a3f3d9f43';
  FTestData[13].InputData  := #$1#$1#$1#$1#$2#$2#$2#$2;
  FTestData[13].Key        := #0#1#2#3#4#5#6#7#8#9#$A#$B#$C#$D#$E#$F;
  FTestData[13].InitVector := '';
  FTestData[13].Filler     := $FF;
  FTestData[13].Mode       := cmECBx;

  FTestData[14].OutputData := 'cf5e601b3f3d9f43';
  FTestData[14].InputData  := #$1#$1#$1#$2#$2#$2#$2#$2;
  FTestData[14].Key        := #0#1#2#3#4#5#6#7#8#9#$A#$B#$C#$D#$E#$F;
  FTestData[14].InitVector := '';
  FTestData[14].Filler     := $FF;
  FTestData[14].Mode       := cmECBx;

  FTestData[15].OutputData := 'cf5e611b3f3d9f43';
  FTestData[15].InputData  := #$1#$1#$2#$2#$2#$2#$2#$2;
  FTestData[15].Key        := #0#1#2#3#4#5#6#7#8#9#$A#$B#$C#$D#$E#$F;
  FTestData[15].InitVector := '';
  FTestData[15].Filler     := $FF;
  FTestData[15].Mode       := cmECBx;

  FTestData[16].OutputData := 'cf5f611b3f3d9f43';
  FTestData[16].InputData  := #$1#$2#$2#$2#$2#$2#$2#$2;
  FTestData[16].Key        := #0#1#2#3#4#5#6#7#8#9#$A#$B#$C#$D#$E#$F;
  FTestData[16].InitVector := '';
  FTestData[16].Filler     := $FF;
  FTestData[16].Mode       := cmECBx;

  FTestData[17].OutputData := 'd05f611b3f3d9f43';
  FTestData[17].InputData  := #$2#$2#$2#$2#$2#$2#$2#$2;
  FTestData[17].Key        := #0#1#2#3#4#5#6#7#8#9#$A#$B#$C#$D#$E#$F;
  FTestData[17].InitVector := '';
  FTestData[17].Filler     := $FF;
  FTestData[17].Mode       := cmECBx;
  // SourceEnd
end;

procedure TestTCipher_SCOP.TearDown;
begin
  FCipher_SCOP.Free;
  FCipher_SCOP := nil;
end;

procedure TestTCipher_SCOP.TestClassByName;
var
  ReturnValue : TDECCipherClass;
begin
  ReturnValue := FCipher_SCOP.ClassByName('TCipher_SCOP');
  CheckEquals(TCipher_SCOP, ReturnValue, 'Class is not registered');
end;

procedure TestTCipher_SCOP.TestContext;
var
  ReturnValue: TCipherContext;
begin
  ReturnValue := FCipher_SCOP.Context;

  CheckEquals(  48,  ReturnValue.KeySize);
  CheckEquals(   4,  ReturnValue.BlockSize);
  CheckEquals(  32,  ReturnValue.BufferSize);
  CheckEquals(1548,  ReturnValue.AdditionalBufferSize);
  CheckEquals(   1,  ReturnValue.MinRounds);
  CheckEquals(   1,  ReturnValue.MaxRounds);
  CheckEquals(true,  ReturnValue.NeedsAdditionalBufferBackup);
  CheckEquals(true,  [ctStream, ctSymmetric] = ReturnValue.CipherType);
end;

procedure TestTCipher_SCOP.TestDecode;
begin
  DoTestDecode(FCipher_SCOP.DecodeStringToBytes, self.Init, self.Done);
end;

procedure TestTCipher_SCOP.TestEncode;
begin
  DoTestEncode(FCipher_SCOP.EncodeStringToBytes, self.Init, self.Done);
end;

procedure TestTCipher_SCOP.TestIdentity;
begin
  CheckEquals($938C9891, FCipher_Scop.Identity);
end;

procedure TestTCipher_SCOP_DEC52.Done;
begin
  FCipher_SCOP_DEC52.Done;
end;

procedure TestTCipher_SCOP_DEC52.DoTestClassByName;
var
  ReturnValue : TDECCipherClass;
begin
  ReturnValue := FCipher_SCOP_DEC52.ClassByName('TCipher_SCOP_DEC52');
  // This line should never be executed due to ClassByName rising an exception
  // but it suppresses a ReturnValue is not being used compiler warning
  CheckEquals(TCipher_SCOP_DEC52, ReturnValue, 'Class is not registered');
end;

procedure TestTCipher_SCOP_DEC52.Init(TestData: TCipherTestData);
begin
  LimitKeyLength(TestData.Key, FCipher_SCOP_DEC52.Context.KeySize);

  FCipher_SCOP_DEC52.Mode := TestData.Mode;
  FCipher_SCOP_DEC52.Init(BytesOf(TestData.Key),
                    BytesOf(TestData.InitVector),
                    TestData.Filler);
end;

procedure TestTCipher_SCOP_DEC52.SetUp;
begin
  FCipher_SCOP_DEC52 := TCipher_SCOP_DEC52.Create;

  SetLength(FTestData, 1);

  FTestData[0].OutputData  := 'b1a7ee707aab160af9b9c3ebc2db5ee814a28995d2c1f994c53ca159ee052632';
  FTestData[0].InputData   := TFormat_ESCAPE.Decode('\x30\x44\xED\x6E\x45\xA4' +
                                                    '\x96\xF5\xF6\x35\xA2\xEB' +
                                                    '\x3D\x1A\x5D\xD6\xCB\x1D' +
                                                    '\x09\x82\x2D\xBD\xF5\x60' +
                                                    '\xC2\xB8\x58\xA1\x91\xF9' +
                                                    '\x81\xB1');

  FTestData[0].Key        := 'TCipher_SCOP';
  FTestData[0].InitVector := '';
  FTestData[0].Filler     := $FF;
  FTestData[0].Mode       := cmCTSx;
end;

procedure TestTCipher_SCOP_DEC52.TearDown;
begin
  FCipher_SCOP_DEC52.Free;
  FCipher_SCOP_DEC52 := nil;
end;

procedure TestTCipher_SCOP_DEC52.TestClassByName;
begin
  // Class shall not be registered by default
  CheckException(DoTestClassByName, EDECClassNotRegisteredException);
end;

procedure TestTCipher_SCOP_DEC52.TestContext;
var
  ReturnValue: TCipherContext;
begin
  ReturnValue := FCipher_SCOP_DEC52.Context;

  CheckEquals(  48,  ReturnValue.KeySize);
  CheckEquals(   4,  ReturnValue.BlockSize);
  CheckEquals(  32,  ReturnValue.BufferSize);
  CheckEquals(1548,  ReturnValue.AdditionalBufferSize);
  CheckEquals(   1,  ReturnValue.MinRounds);
  CheckEquals(   1,  ReturnValue.MaxRounds);
  CheckEquals(true,  ReturnValue.NeedsAdditionalBufferBackup);
  CheckEquals(true,  [ctStream, ctSymmetric] = ReturnValue.CipherType);
end;

procedure TestTCipher_SCOP_DEC52.TestDecode;
begin
  DoTestDecode(FCipher_SCOP_DEC52.DecodeStringToBytes, self.Init, self.Done);
end;

procedure TestTCipher_SCOP_DEC52.TestEncode;
begin
  DoTestEncode(FCipher_SCOP_DEC52.EncodeStringToBytes, self.Init, self.Done);
end;

procedure TestTCipher_SCOP_DEC52.TestIdentity;
begin
  CheckEquals($398EE1E3, FCipher_SCOP_DEC52.Identity);
end;

procedure TestTCipher_Sapphire.Done;
begin
  FCipher_Sapphire.Done;
end;

procedure TestTCipher_Sapphire.Init(TestData: TCipherTestData);
begin
  LimitKeyLength(TestData.Key, FCipher_Sapphire.Context.KeySize);

  FCipher_Sapphire.Mode := TestData.Mode;
  FCipher_Sapphire.Init(BytesOf(TestData.Key),
                    BytesOf(TestData.InitVector),
                    TestData.Filler);
end;

procedure TestTCipher_Sapphire.SetUp;
begin
  FCipher_Sapphire := TCipher_Sapphire.Create;

  SetLength(FTestData, 1);

  FTestData[0].OutputData  := 'cff8d04e8e0d42e6aef37afaacbe9b08850c4d0c75ac54c0b9388e54e5609650';
  FTestData[0].InputData   := TFormat_ESCAPE.Decode('\x30\x44\xED\x6E\x45\xA4' +
                                                    '\x96\xF5\xF6\x35\xA2\xEB' +
                                                    '\x3D\x1A\x5D\xD6\xCB\x1D' +
                                                    '\x09\x82\x2D\xBD\xF5\x60' +
                                                    '\xC2\xB8\x58\xA1\x91\xF9' +
                                                    '\x81\xB1');

  FTestData[0].Key        := 'TCipher_Sapphire';
  FTestData[0].InitVector := '';
  FTestData[0].Filler     := $FF;
  FTestData[0].Mode       := cmCTSx;
end;

procedure TestTCipher_Sapphire.TearDown;
begin
  FCipher_Sapphire.Free;
  FCipher_Sapphire := nil;
end;

procedure TestTCipher_Sapphire.TestClassByName;
var
  ReturnValue : TDECCipherClass;
begin
  ReturnValue := FCipher_Sapphire.ClassByName('TCipher_Sapphire');
  CheckEquals(TCipher_Sapphire, ReturnValue, 'Class is not registered');
end;

procedure TestTCipher_Sapphire.TestContext;
var
  ReturnValue: TCipherContext;
begin
  ReturnValue := FCipher_Sapphire.Context;

  CheckEquals(1024,  ReturnValue.KeySize);
  CheckEquals(   1,  ReturnValue.BlockSize);
  CheckEquals(  32,  ReturnValue.BufferSize);
  CheckEquals(1044,  ReturnValue.AdditionalBufferSize);
  CheckEquals(   1,  ReturnValue.MinRounds);
  CheckEquals(   1,  ReturnValue.MaxRounds);
  CheckEquals(true,  ReturnValue.NeedsAdditionalBufferBackup);
  CheckEquals(true,  [ctStream, ctSymmetric] = ReturnValue.CipherType);
end;

procedure TestTCipher_Sapphire.TestDecode;
begin
  DoTestDecode(FCipher_Sapphire.DecodeStringToBytes, self.Init, self.Done);
end;

procedure TestTCipher_Sapphire.TestEncode;
begin
  DoTestEncode(FCipher_Sapphire.EncodeStringToBytes, self.Init, self.Done);
end;

procedure TestTCipher_Sapphire.TestIdentity;
begin
  CheckEquals($42FAA470, FCipher_Sapphire.Identity);
end;

procedure TestTCipher_1DES.Done;
begin
  FCipher_1DES.Done;
end;

procedure TestTCipher_1DES.Init(TestData: TCipherTestData);
begin
  LimitKeyLength(TestData.Key, FCipher_1DES.Context.KeySize);

  FCipher_1DES.Mode := TestData.Mode;
  FCipher_1DES.Init(BytesOf(TestData.Key),
                    BytesOf(TestData.InitVector),
                    TestData.Filler);
end;

procedure TestTCipher_1DES.SetUp;
begin
  FCipher_1DES := TCipher_1DES.Create;

  SetLength(FTestData, 1);

  FTestData[0].OutputData  := 'ad6942bbf668204d53cdc762139398c0300d850be2aa72096fdb5f8ed3e4cf8a';
  FTestData[0].InputData   := TFormat_ESCAPE.Decode('\x30\x44\xED\x6E\x45\xA4' +
                                                    '\x96\xF5\xF6\x35\xA2\xEB' +
                                                    '\x3D\x1A\x5D\xD6\xCB\x1D' +
                                                    '\x09\x82\x2D\xBD\xF5\x60' +
                                                    '\xC2\xB8\x58\xA1\x91\xF9' +
                                                    '\x81\xB1');

  FTestData[0].Key        := 'TCipher_1DES';
  FTestData[0].InitVector := '';
  FTestData[0].Filler     := $FF;
  FTestData[0].Mode       := cmCTSx;
end;

procedure TestTCipher_1DES.TearDown;
begin
  FCipher_1DES.Free;
  FCipher_1DES := nil;
end;

procedure TestTCipher_1DES.TestClassByName;
var
  ReturnValue : TDECCipherClass;
begin
  ReturnValue := FCipher_1DES.ClassByName('TCipher_1DES');
  CheckEquals(TCipher_1DES, ReturnValue, 'Class is not registered');
end;

procedure TestTCipher_1DES.TestContext;
var
  ReturnValue: TCipherContext;
begin
  ReturnValue := FCipher_1DES.Context;

  CheckEquals(   8,  ReturnValue.KeySize);
  CheckEquals(   8,  ReturnValue.BlockSize);
  CheckEquals(   8,  ReturnValue.BufferSize);
  CheckEquals( 256,  ReturnValue.AdditionalBufferSize);
  CheckEquals(   1,  ReturnValue.MinRounds);
  CheckEquals(   1,  ReturnValue.MaxRounds);
  CheckEquals(false, ReturnValue.NeedsAdditionalBufferBackup);
  CheckEquals(true,  [ctBlock, ctSymmetric] = ReturnValue.CipherType);
end;

procedure TestTCipher_1DES.TestDecode;
begin
  DoTestDecode(FCipher_1DES.DecodeStringToBytes, self.Init, self.Done);
end;

procedure TestTCipher_1DES.TestEncode;
begin
  DoTestEncode(FCipher_1DES.EncodeStringToBytes, self.Init, self.Done);
end;

procedure TestTCipher_1DES.TestIdentity;
begin
  CheckEquals($640A08AC, FCipher_1DES.Identity);
end;

procedure TestTCipher_2DES.Done;
begin
  FCipher_2DES.Done;
end;

procedure TestTCipher_2DES.Init(TestData: TCipherTestData);
begin
  LimitKeyLength(TestData.Key, FCipher_2DES.Context.KeySize);

  FCipher_2DES.Mode := TestData.Mode;
  FCipher_2DES.Init(BytesOf(TestData.Key),
                    BytesOf(TestData.InitVector),
                    TestData.Filler);
end;

procedure TestTCipher_2DES.SetUp;
begin
  FCipher_2DES := TCipher_2DES.Create;

  SetLength(FTestData, 1);

  FTestData[0].OutputData  := '665c7927e91c8ba0a9e4995a158cbd465c9c75913c38069d75b47e68e947fdab';
  FTestData[0].InputData   := TFormat_ESCAPE.Decode('\x30\x44\xED\x6E\x45\xA4' +
                                                    '\x96\xF5\xF6\x35\xA2\xEB' +
                                                    '\x3D\x1A\x5D\xD6\xCB\x1D' +
                                                    '\x09\x82\x2D\xBD\xF5\x60' +
                                                    '\xC2\xB8\x58\xA1\x91\xF9' +
                                                    '\x81\xB1');

  FTestData[0].Key        := 'TCipher_2DES';
  FTestData[0].InitVector := '';
  FTestData[0].Filler     := $FF;
  FTestData[0].Mode       := cmCTSx;
end;

procedure TestTCipher_2DES.TearDown;
begin
  FCipher_2DES.Free;
  FCipher_2DES := nil;
end;

procedure TestTCipher_2DES.TestClassByName;
var
  ReturnValue : TDECCipherClass;
begin
  ReturnValue := FCipher_2DES.ClassByName('TCipher_2DES');
  CheckEquals(TCipher_2DES, ReturnValue, 'Class is not registered');
end;

procedure TestTCipher_2DES.TestContext;
var
  ReturnValue: TCipherContext;
begin
  ReturnValue := FCipher_2DES.Context;

  CheckEquals(  16,  ReturnValue.KeySize);
  CheckEquals(   8,  ReturnValue.BlockSize);
  CheckEquals(   8,  ReturnValue.BufferSize);
  CheckEquals( 512,  ReturnValue.AdditionalBufferSize);
  CheckEquals(   1,  ReturnValue.MinRounds);
  CheckEquals(   1,  ReturnValue.MaxRounds);
  CheckEquals(false, ReturnValue.NeedsAdditionalBufferBackup);
  CheckEquals(true,  [ctBlock, ctSymmetric] = ReturnValue.CipherType);
end;

procedure TestTCipher_2DES.TestDecode;
begin
  DoTestDecode(FCipher_2DES.DecodeStringToBytes, self.Init, self.Done);
end;

procedure TestTCipher_2DES.TestEncode;
begin
  DoTestEncode(FCipher_2DES.EncodeStringToBytes, self.Init, self.Done);
end;

procedure TestTCipher_2DES.TestIdentity;
begin
  CheckEquals($76BFA742, FCipher_2DES.Identity);
end;

procedure TestTCipher_3DES.Done;
begin
  FCipher_3DES.Done;
end;

procedure TestTCipher_3DES.Init(TestData: TCipherTestData);
begin
  LimitKeyLength(TestData.Key, FCipher_3DES.Context.KeySize);

  FCipher_3DES.Mode := TestData.Mode;
  FCipher_3DES.Init(BytesOf(TestData.Key),
                    BytesOf(TestData.InitVector),
                    TestData.Filler);
end;

procedure TestTCipher_3DES.SetUp;
begin
  FCipher_3DES := TCipher_3DES.Create;

  SetLength(FTestData, 1);

  FTestData[0].OutputData  := '074c14f3e22e08d964bf6f82b5dff0a22f2d3bdb17db25b6b51efa71372fd172';
  FTestData[0].InputData   := TFormat_ESCAPE.Decode('\x30\x44\xED\x6E\x45\xA4' +
                                                    '\x96\xF5\xF6\x35\xA2\xEB' +
                                                    '\x3D\x1A\x5D\xD6\xCB\x1D' +
                                                    '\x09\x82\x2D\xBD\xF5\x60' +
                                                    '\xC2\xB8\x58\xA1\x91\xF9' +
                                                    '\x81\xB1');

  FTestData[0].Key        := 'TCipher_3DES';
  FTestData[0].InitVector := '';
  FTestData[0].Filler     := $FF;
  FTestData[0].Mode       := cmCTSx;
end;

procedure TestTCipher_3DES.TearDown;
begin
  FCipher_3DES.Free;
  FCipher_3DES := nil;
end;

procedure TestTCipher_3DES.TestClassByName;
var
  ReturnValue : TDECCipherClass;
begin
  ReturnValue := FCipher_3DES.ClassByName('TCipher_3DES');
  CheckEquals(TCipher_3DES, ReturnValue, 'Class is not registered');
end;

procedure TestTCipher_3DES.TestContext;
var
  ReturnValue: TCipherContext;
begin
  ReturnValue := FCipher_3DES.Context;

  CheckEquals(  24,  ReturnValue.KeySize);
  CheckEquals(   8,  ReturnValue.BlockSize);
  CheckEquals(   8,  ReturnValue.BufferSize);
  CheckEquals( 768,  ReturnValue.AdditionalBufferSize);
  CheckEquals(   1,  ReturnValue.MinRounds);
  CheckEquals(   1,  ReturnValue.MaxRounds);
  CheckEquals(false, ReturnValue.NeedsAdditionalBufferBackup);
  CheckEquals(true,  [ctBlock, ctSymmetric] = ReturnValue.CipherType);
end;

procedure TestTCipher_3DES.TestDecode;
begin
  DoTestDecode(FCipher_3DES.DecodeStringToBytes, self.Init, self.Done);
end;

procedure TestTCipher_3DES.TestEncode;
begin
  DoTestEncode(FCipher_3DES.EncodeStringToBytes, self.Init, self.Done);
end;

procedure TestTCipher_3DES.TestIdentity;
begin
  CheckEquals($CE03C027, FCipher_3DES.Identity);
end;

procedure TestTCipher_2DDES.Done;
begin
  FCipher_2DDES.Done;
end;

procedure TestTCipher_2DDES.Init(TestData: TCipherTestData);
begin
  LimitKeyLength(TestData.Key, FCipher_2DDES.Context.KeySize);

  FCipher_2DDES.Mode := TestData.Mode;
  FCipher_2DDES.Init(BytesOf(TestData.Key),
                    BytesOf(TestData.InitVector),
                    TestData.Filler);
end;

procedure TestTCipher_2DDES.SetUp;
begin
  FCipher_2DDES := TCipher_2DDES.Create;

  SetLength(FTestData, 1);

  FTestData[0].OutputData  := '936cf643c6a77fed4db4704ae2a6068b751319afe182ed354e13f688a46b3326';
  FTestData[0].InputData   := TFormat_ESCAPE.Decode('\x30\x44\xED\x6E\x45\xA4' +
                                                    '\x96\xF5\xF6\x35\xA2\xEB' +
                                                    '\x3D\x1A\x5D\xD6\xCB\x1D' +
                                                    '\x09\x82\x2D\xBD\xF5\x60' +
                                                    '\xC2\xB8\x58\xA1\x91\xF9' +
                                                    '\x81\xB1');

  FTestData[0].Key        := 'TCipher_2DDES';
  FTestData[0].InitVector := '';
  FTestData[0].Filler     := $FF;
  FTestData[0].Mode       := cmCTSx;
end;

procedure TestTCipher_2DDES.TearDown;
begin
  FCipher_2DDES.Free;
  FCipher_2DDES := nil;
end;

procedure TestTCipher_2DDES.TestClassByName;
var
  ReturnValue : TDECCipherClass;
begin
  ReturnValue := FCipher_2DDES.ClassByName('TCipher_2DDES');
  CheckEquals(TCipher_2DDES, ReturnValue, 'Class is not registered');
end;

procedure TestTCipher_2DDES.TestContext;
var
  ReturnValue: TCipherContext;
begin
  ReturnValue := FCipher_2DDES.Context;

  CheckEquals(  16,  ReturnValue.KeySize);
  CheckEquals(  16,  ReturnValue.BlockSize);
  CheckEquals(  16,  ReturnValue.BufferSize);
  CheckEquals( 512,  ReturnValue.AdditionalBufferSize);
  CheckEquals(   1,  ReturnValue.MinRounds);
  CheckEquals(   1,  ReturnValue.MaxRounds);
  CheckEquals(false, ReturnValue.NeedsAdditionalBufferBackup);
  CheckEquals(true,  [ctBlock, ctSymmetric] = ReturnValue.CipherType);
end;

procedure TestTCipher_2DDES.TestDecode;
begin
  DoTestDecode(FCipher_2DDES.DecodeStringToBytes, self.Init, self.Done);
end;

procedure TestTCipher_2DDES.TestEncode;
begin
  DoTestEncode(FCipher_2DDES.EncodeStringToBytes, self.Init, self.Done);
end;

procedure TestTCipher_2DDES.TestIdentity;
begin
  CheckEquals($70C155BD, FCipher_2DDES.Identity);
end;

procedure TestTCipher_3DDES.Done;
begin
  FCipher_3DDES.Done;
end;

procedure TestTCipher_3DDES.Init(TestData: TCipherTestData);
begin
  LimitKeyLength(TestData.Key, FCipher_3DDES.Context.KeySize);

  FCipher_3DDES.Mode := TestData.Mode;
  FCipher_3DDES.Init(BytesOf(TestData.Key),
                    BytesOf(TestData.InitVector),
                    TestData.Filler);
end;

procedure TestTCipher_3DDES.SetUp;
begin
  FCipher_3DDES := TCipher_3DDES.Create;

  SetLength(FTestData, 1);

  FTestData[0].OutputData  := '2f5a5ed45e8aaa4ed26659481de195942a9fcc1f4de614f050040364669a778e';
  FTestData[0].InputData   := TFormat_ESCAPE.Decode('\x30\x44\xED\x6E\x45\xA4' +
                                                    '\x96\xF5\xF6\x35\xA2\xEB' +
                                                    '\x3D\x1A\x5D\xD6\xCB\x1D' +
                                                    '\x09\x82\x2D\xBD\xF5\x60' +
                                                    '\xC2\xB8\x58\xA1\x91\xF9' +
                                                    '\x81\xB1');

  FTestData[0].Key        := 'TCipher_3DDES';
  FTestData[0].InitVector := '';
  FTestData[0].Filler     := $FF;
  FTestData[0].Mode       := cmCTSx;
end;

procedure TestTCipher_3DDES.TearDown;
begin
  FCipher_3DDES.Free;
  FCipher_3DDES := nil;
end;

procedure TestTCipher_3DDES.TestClassByName;
var
  ReturnValue : TDECCipherClass;
begin
  ReturnValue := FCipher_3DDES.ClassByName('TCipher_3DDES');
  CheckEquals(TCipher_3DDES, ReturnValue, 'Class is not registered');
end;

procedure TestTCipher_3DDES.TestContext;
var
  ReturnValue: TCipherContext;
begin
  ReturnValue := FCipher_3DDES.Context;

  CheckEquals(  24,  ReturnValue.KeySize);
  CheckEquals(  16,  ReturnValue.BlockSize);
  CheckEquals(  16,  ReturnValue.BufferSize);
  CheckEquals( 768,  ReturnValue.AdditionalBufferSize);
  CheckEquals(   1,  ReturnValue.MinRounds);
  CheckEquals(   1,  ReturnValue.MaxRounds);
  CheckEquals(false, ReturnValue.NeedsAdditionalBufferBackup);
  CheckEquals(true,  [ctBlock, ctSymmetric] = ReturnValue.CipherType);
end;

procedure TestTCipher_3DDES.TestDecode;
begin
  DoTestDecode(FCipher_3DDES.DecodeStringToBytes, self.Init, self.Done);
end;

procedure TestTCipher_3DDES.TestEncode;
begin
  DoTestEncode(FCipher_3DDES.EncodeStringToBytes, self.Init, self.Done);
end;

procedure TestTCipher_3DDES.TestIdentity;
begin
  CheckEquals($4DA17C0D, FCipher_3DDES.Identity);
end;

procedure TestTCipher_3TDES.Done;
begin
  FCipher_3TDES.Done;
end;

procedure TestTCipher_3TDES.Init(TestData: TCipherTestData);
begin
  LimitKeyLength(TestData.Key, FCipher_3TDES.Context.KeySize);

  FCipher_3TDES.Mode := TestData.Mode;
  FCipher_3TDES.Init(BytesOf(TestData.Key),
                    BytesOf(TestData.InitVector),
                    TestData.Filler);
end;

procedure TestTCipher_3TDES.SetUp;
begin
  FCipher_3TDES := TCipher_3TDES.Create;

  SetLength(FTestData, 1);

  FTestData[0].OutputData  := '899e748e57060649fc7436b21a538bb8d64b57c6a0863bf6b5f18468c0f6466e';
  FTestData[0].InputData   := TFormat_ESCAPE.Decode('\x30\x44\xED\x6E\x45\xA4' +
                                                    '\x96\xF5\xF6\x35\xA2\xEB' +
                                                    '\x3D\x1A\x5D\xD6\xCB\x1D' +
                                                    '\x09\x82\x2D\xBD\xF5\x60' +
                                                    '\xC2\xB8\x58\xA1\x91\xF9' +
                                                    '\x81\xB1');

  FTestData[0].Key        := 'TCipher_3TDES';
  FTestData[0].InitVector := '';
  FTestData[0].Filler     := $FF;
  FTestData[0].Mode       := cmCTSx;
end;

procedure TestTCipher_3TDES.TearDown;
begin
  FCipher_3TDES.Free;
  FCipher_3TDES := nil;
end;

procedure TestTCipher_3TDES.TestClassByName;
var
  ReturnValue : TDECCipherClass;
begin
  ReturnValue := FCipher_3TDES.ClassByName('TCipher_3TDES');
  CheckEquals(TCipher_3TDES, ReturnValue, 'Class is not registered');
end;

procedure TestTCipher_3TDES.TestContext;
var
  ReturnValue: TCipherContext;
begin
  ReturnValue := FCipher_3TDES.Context;

  CheckEquals(  24,  ReturnValue.KeySize);
  CheckEquals(  24,  ReturnValue.BlockSize);
  CheckEquals(  24,  ReturnValue.BufferSize);
  CheckEquals( 768,  ReturnValue.AdditionalBufferSize);
  CheckEquals(   1,  ReturnValue.MinRounds);
  CheckEquals(   1,  ReturnValue.MaxRounds);
  CheckEquals(false, ReturnValue.NeedsAdditionalBufferBackup);
  CheckEquals(true,  [ctBlock, ctSymmetric] = ReturnValue.CipherType);
end;

procedure TestTCipher_3TDES.TestDecode;
begin
  DoTestEncode(FCipher_3TDES.EncodeStringToBytes, self.Init, self.Done);
end;

procedure TestTCipher_3TDES.TestEncode;
begin
  DoTestDecode(FCipher_3TDES.DecodeStringToBytes, self.Init, self.Done);
end;

procedure TestTCipher_3TDES.TestIdentity;
begin
  CheckEquals($1DB82B92, FCipher_3TDES.Identity);
end;

procedure TestTCipher_3Way.Done;
begin
  FCipher_3Way.Done;
end;

procedure TestTCipher_3Way.Init(TestData: TCipherTestData);
begin
  LimitKeyLength(TestData.Key, FCipher_3Way.Context.KeySize);

  FCipher_3Way.Mode := TestData.Mode;
  FCipher_3Way.Init(BytesOf(TestData.Key),
                    BytesOf(TestData.InitVector),
                    TestData.Filler);
end;

procedure TestTCipher_3Way.SetUp;
begin
  FCipher_3Way := TCipher_3Way.Create;

  SetLength(FTestData, 1);

  FTestData[0].OutputData  := '77fc77947c8fde21e981df2ab1bc7ef8a3b6444bb6fc79c49b4058cee8959e12';
  FTestData[0].InputData   := TFormat_ESCAPE.Decode('\x30\x44\xED\x6E\x45\xA4' +
                                                    '\x96\xF5\xF6\x35\xA2\xEB' +
                                                    '\x3D\x1A\x5D\xD6\xCB\x1D' +
                                                    '\x09\x82\x2D\xBD\xF5\x60' +
                                                    '\xC2\xB8\x58\xA1\x91\xF9' +
                                                    '\x81\xB1');

  FTestData[0].Key        := 'TCipher_3Way';
  FTestData[0].InitVector := '';
  FTestData[0].Filler     := $FF;
  FTestData[0].Mode       := cmCTSx;
end;

procedure TestTCipher_3Way.TearDown;
begin
  FCipher_3Way.Free;
  FCipher_3Way := nil;
end;

procedure TestTCipher_3Way.TestClassByName;
var
  ReturnValue : TDECCipherClass;
begin
  ReturnValue := FCipher_3Way.ClassByName('TCipher_3Way');
  CheckEquals(TCipher_3Way, ReturnValue, 'Class is not registered');
end;

procedure TestTCipher_3Way.TestContext;
var
  ReturnValue: TCipherContext;
begin
  ReturnValue := FCipher_3Way.Context;

  CheckEquals(  12,  ReturnValue.KeySize);
  CheckEquals(  12,  ReturnValue.BlockSize);
  CheckEquals(  12,  ReturnValue.BufferSize);
  CheckEquals( 120,  ReturnValue.AdditionalBufferSize);
  CheckEquals(   1,  ReturnValue.MinRounds);
  CheckEquals(   1,  ReturnValue.MaxRounds);
  CheckEquals(false, ReturnValue.NeedsAdditionalBufferBackup);
  CheckEquals(true,  [ctBlock, ctSymmetric] = ReturnValue.CipherType);
end;

procedure TestTCipher_3Way.TestDecode;
begin
  DoTestDecode(FCipher_3Way.DecodeStringToBytes, self.Init, self.Done);
end;

procedure TestTCipher_3Way.TestEncode;
begin
  DoTestEncode(FCipher_3Way.EncodeStringToBytes, self.Init, self.Done);
end;

procedure TestTCipher_3Way.TestIdentity;
begin
  CheckEquals($54DAF114, FCipher_3Way.Identity);
end;

procedure TestTCipher_Cast128.Done;
begin
  FCipher_Cast128.Done;
end;

procedure TestTCipher_Cast128.Init(TestData: TCipherTestData);
begin
  LimitKeyLength(TestData.Key, FCipher_Cast128.Context.KeySize);

  FCipher_Cast128.Mode := TestData.Mode;
  FCipher_Cast128.Init(BytesOf(TestData.Key),
                    BytesOf(TestData.InitVector),
                    TestData.Filler);
end;

procedure TestTCipher_Cast128.SetUp;
begin
  FCipher_Cast128 := TCipher_Cast128.Create;

  SetLength(FTestData, 1);

  FTestData[0].OutputData  := '6c27d14cf6ba76e7a4781c20188c30bcd29af62a631ffd04893fc70e07a9949b';
  FTestData[0].InputData   := TFormat_ESCAPE.Decode('\x30\x44\xED\x6E\x45\xA4' +
                                                    '\x96\xF5\xF6\x35\xA2\xEB' +
                                                    '\x3D\x1A\x5D\xD6\xCB\x1D' +
                                                    '\x09\x82\x2D\xBD\xF5\x60' +
                                                    '\xC2\xB8\x58\xA1\x91\xF9' +
                                                    '\x81\xB1');

  FTestData[0].Key        := 'TCipher_Cast128';
  FTestData[0].InitVector := '';
  FTestData[0].Filler     := $FF;
  FTestData[0].Mode       := cmCTSx;
end;

procedure TestTCipher_Cast128.TearDown;
begin
  FCipher_Cast128.Free;
  FCipher_Cast128 := nil;
end;

procedure TestTCipher_Cast128.TestClassByName;
var
  ReturnValue : TDECCipherClass;
begin
  ReturnValue := FCipher_Cast128.ClassByName('TCipher_Cast128');
  CheckEquals(TCipher_Cast128, ReturnValue, 'Class is not registered');
end;

procedure TestTCipher_Cast128.TestContext;
var
  ReturnValue: TCipherContext;
begin
  ReturnValue := FCipher_Cast128.Context;

  CheckEquals(  16,  ReturnValue.KeySize);
  CheckEquals(   8,  ReturnValue.BlockSize);
  CheckEquals(   8,  ReturnValue.BufferSize);
  CheckEquals( 128,  ReturnValue.AdditionalBufferSize);
  CheckEquals(   1,  ReturnValue.MinRounds);
  CheckEquals( 256,  ReturnValue.MaxRounds);
  CheckEquals(false, ReturnValue.NeedsAdditionalBufferBackup);
  CheckEquals(true,  [ctBlock, ctSymmetric] = ReturnValue.CipherType);
end;

procedure TestTCipher_Cast128.TestDecode;
begin
  DoTestDecode(FCipher_Cast128.DecodeStringToBytes, self.Init, self.Done);
end;

procedure TestTCipher_Cast128.TestEncode;
begin
  DoTestEncode(FCipher_Cast128.EncodeStringToBytes, self.Init, self.Done);
end;

procedure TestTCipher_Cast128.TestIdentity;
begin
  CheckEquals($ED7D0785, FCipher_Cast128.Identity);
end;

procedure TestTCipher_Gost.Done;
begin
  FCipher_Gost.Done;
end;

procedure TestTCipher_Gost.Init(TestData: TCipherTestData);
begin
  LimitKeyLength(TestData.Key, FCipher_Gost.Context.KeySize);

  FCipher_Gost.Mode := TestData.Mode;
  FCipher_Gost.Init(BytesOf(TestData.Key),
                    BytesOf(TestData.InitVector),
                    TestData.Filler);
end;

procedure TestTCipher_Gost.SetUp;
begin
  FCipher_Gost := TCipher_Gost.Create;

  SetLength(FTestData, 1);

  FTestData[0].OutputData  := 'b303a03fb57b914d97512440bdcf251534059cf8ab10869ff2804784479b1ad1';
  FTestData[0].InputData   := TFormat_ESCAPE.Decode('\x30\x44\xED\x6E\x45\xA4' +
                                                    '\x96\xF5\xF6\x35\xA2\xEB' +
                                                    '\x3D\x1A\x5D\xD6\xCB\x1D' +
                                                    '\x09\x82\x2D\xBD\xF5\x60' +
                                                    '\xC2\xB8\x58\xA1\x91\xF9' +
                                                    '\x81\xB1');

  FTestData[0].Key        := 'TCipher_Gost';
  FTestData[0].InitVector := '';
  FTestData[0].Filler     := $FF;
  FTestData[0].Mode       := cmCTSx;
end;

procedure TestTCipher_Gost.TearDown;
begin
  FCipher_Gost.Free;
  FCipher_Gost := nil;
end;

procedure TestTCipher_Gost.TestClassByName;
var
  ReturnValue : TDECCipherClass;
begin
  ReturnValue := FCipher_Gost.ClassByName('TCipher_Gost');
  CheckEquals(TCipher_Gost, ReturnValue, 'Class is not registered');
end;

procedure TestTCipher_Gost.TestContext;
var
  ReturnValue: TCipherContext;
begin
  ReturnValue := FCipher_Gost.Context;

  CheckEquals(  32,  ReturnValue.KeySize);
  CheckEquals(   8,  ReturnValue.BlockSize);
  CheckEquals(   8,  ReturnValue.BufferSize);
  CheckEquals(  32,  ReturnValue.AdditionalBufferSize);
  CheckEquals(   1,  ReturnValue.MinRounds);
  CheckEquals(   1,  ReturnValue.MaxRounds);
  CheckEquals(false, ReturnValue.NeedsAdditionalBufferBackup);
  CheckEquals(true,  [ctBlock, ctSymmetric] = ReturnValue.CipherType);
end;

procedure TestTCipher_Gost.TestDecode;
begin
  DoTestDecode(FCipher_Gost.DecodeStringToBytes, self.Init, self.Done);
end;

procedure TestTCipher_Gost.TestEncode;
begin
  DoTestEncode(FCipher_Gost.EncodeStringToBytes, self.Init, self.Done);
end;

procedure TestTCipher_Gost.TestIdentity;
begin
  CheckEquals($A4F73879, FCipher_Gost.Identity);
end;

procedure TestTCipher_Magma.Done;
begin
  FCipher_Magma.Done;
end;

procedure TestTCipher_Magma.Init(TestData: TCipherTestData);
begin
  LimitKeyLength(TestData.Key, FCipher_Magma.Context.KeySize);

  FCipher_Magma.Mode := TestData.Mode;
  FCipher_Magma.Init(BytesOf(TestData.Key),
                     BytesOf(TestData.InitVector),
                     TestData.Filler);
end;

procedure TestTCipher_Magma.SetUp;
begin
  FCipher_Magma := TCipher_Magma.Create;

  SetLength(FTestData, 1);

  FTestData[0].OutputData  := 'b303a03fb57b914d97512440bdcf251534059cf8ab10869ff2804784479b1ad1';
  FTestData[0].InputData   := TFormat_ESCAPE.Decode('\x30\x44\xED\x6E\x45\xA4' +
                                                    '\x96\xF5\xF6\x35\xA2\xEB' +
                                                    '\x3D\x1A\x5D\xD6\xCB\x1D' +
                                                    '\x09\x82\x2D\xBD\xF5\x60' +
                                                    '\xC2\xB8\x58\xA1\x91\xF9' +
                                                    '\x81\xB1');

  FTestData[0].Key        := 'TCipher_Gost';
  FTestData[0].InitVector := '';
  FTestData[0].Filler     := $FF;
  FTestData[0].Mode       := cmCTSx;
end;

procedure TestTCipher_Magma.TearDown;
begin
  FCipher_Magma.Free;
  FCipher_Magma := nil;
end;

procedure TestTCipher_Magma.DoTestClassByName;
var
  ReturnValue : TDECCipherClass;
begin
  ReturnValue := FCipher_Magma.ClassByName('TCipher_Magma');
  // This line should never be executed due to ClassByName rising an exception
  // but it suppresses a ReturnValue is not being used compiler warning
  CheckEquals(TCipher_Magma, ReturnValue, 'Class is not registered');
end;

procedure TestTCipher_Magma.TestClassByName;
begin
  CheckException(DoTestClassByName, EDECClassNotRegisteredException);
end;

procedure TestTCipher_Magma.TestContext;
var
  ReturnValue: TCipherContext;
begin
  ReturnValue := FCipher_Magma.Context;

  CheckEquals(  32,  ReturnValue.KeySize);
  CheckEquals(   8,  ReturnValue.BlockSize);
  CheckEquals(   8,  ReturnValue.BufferSize);
  CheckEquals(  32,  ReturnValue.AdditionalBufferSize);
  CheckEquals(   1,  ReturnValue.MinRounds);
  CheckEquals(   1,  ReturnValue.MaxRounds);
  CheckEquals(false, ReturnValue.NeedsAdditionalBufferBackup);
  CheckEquals(true,  [ctBlock, ctSymmetric] = ReturnValue.CipherType);
end;

procedure TestTCipher_Magma.TestDecode;
begin
  DoTestDecode(FCipher_Magma.DecodeStringToBytes, self.Init, self.Done);
end;

procedure TestTCipher_Magma.TestEncode;
begin
  DoTestEncode(FCipher_Magma.EncodeStringToBytes, self.Init, self.Done);
end;

procedure TestTCipher_Magma.TestIdentity;
begin
  CheckEquals($5BB9788, FCipher_Magma.Identity);
end;

procedure TestTCipher_Misty.Done;
begin
  FCipher_Misty.Done;
end;

procedure TestTCipher_Misty.Init(TestData: TCipherTestData);
begin
  LimitKeyLength(TestData.Key, FCipher_Misty.Context.KeySize);

  FCipher_Misty.Mode := TestData.Mode;
  FCipher_Misty.Init(BytesOf(TestData.Key),
                    BytesOf(TestData.InitVector),
                    TestData.Filler);
end;

procedure TestTCipher_Misty.SetUp;
begin
  FCipher_Misty := TCipher_Misty.Create;

  SetLength(FTestData, 1);

  FTestData[0].OutputData  := '647bc5c64945aa955d64cd567c6cb6478157fe8cf48419bc27600ca679850fc9';
  FTestData[0].InputData   := TFormat_ESCAPE.Decode('\x30\x44\xED\x6E\x45\xA4' +
                                                    '\x96\xF5\xF6\x35\xA2\xEB' +
                                                    '\x3D\x1A\x5D\xD6\xCB\x1D' +
                                                    '\x09\x82\x2D\xBD\xF5\x60' +
                                                    '\xC2\xB8\x58\xA1\x91\xF9' +
                                                    '\x81\xB1');

  FTestData[0].Key        := 'TCipher_Misty';
  FTestData[0].InitVector := '';
  FTestData[0].Filler     := $FF;
  FTestData[0].Mode       := cmCTSx;
end;

procedure TestTCipher_Misty.TearDown;
begin
  FCipher_Misty.Free;
  FCipher_Misty := nil;
end;

procedure TestTCipher_Misty.TestClassByName;
var
  ReturnValue : TDECCipherClass;
begin
  ReturnValue := FCipher_Misty.ClassByName('TCipher_Misty');
  CheckEquals(TCipher_Misty, ReturnValue, 'Class is not registered');
end;

procedure TestTCipher_Misty.TestContext;
var
  ReturnValue: TCipherContext;
begin
  ReturnValue := FCipher_Misty.Context;

  CheckEquals(  16,  ReturnValue.KeySize);
  CheckEquals(   8,  ReturnValue.BlockSize);
  CheckEquals(   8,  ReturnValue.BufferSize);
  CheckEquals( 128,  ReturnValue.AdditionalBufferSize);
  CheckEquals(   1,  ReturnValue.MinRounds);
  CheckEquals(   1,  ReturnValue.MaxRounds);
  CheckEquals(false, ReturnValue.NeedsAdditionalBufferBackup);
  CheckEquals(true,  [ctBlock, ctSymmetric] = ReturnValue.CipherType);
end;

procedure TestTCipher_Misty.TestDecode;
begin
  DoTestDecode(FCipher_Misty.DecodeStringToBytes, self.Init, self.Done);
end;

procedure TestTCipher_Misty.TestEncode;
begin
  DoTestEncode(FCipher_Misty.EncodeStringToBytes, self.Init, self.Done);
end;

procedure TestTCipher_Misty.TestIdentity;
begin
  CheckEquals($534C8585, FCipher_Misty.Identity);
end;

procedure TestTCipher_NewDES.Done;
begin
  FCipher_NewDES.Done;
end;

procedure TestTCipher_NewDES.Init(TestData: TCipherTestData);
begin
  LimitKeyLength(TestData.Key, FCipher_NewDES.Context.KeySize);

  FCipher_NewDES.Mode := TestData.Mode;
  FCipher_NewDES.Init(BytesOf(TestData.Key),
                    BytesOf(TestData.InitVector),
                    TestData.Filler);
end;

procedure TestTCipher_NewDES.SetUp;
begin
  FCipher_NewDES := TCipher_NewDES.Create;

  SetLength(FTestData, 1);

  FTestData[0].OutputData  := 'd5914f9c743546fbd5ad9131751464fea779216a29994789d20d760c739ccd17';
  FTestData[0].InputData   := TFormat_ESCAPE.Decode('\x30\x44\xED\x6E\x45\xA4' +
                                                    '\x96\xF5\xF6\x35\xA2\xEB' +
                                                    '\x3D\x1A\x5D\xD6\xCB\x1D' +
                                                    '\x09\x82\x2D\xBD\xF5\x60' +
                                                    '\xC2\xB8\x58\xA1\x91\xF9' +
                                                    '\x81\xB1');

  FTestData[0].Key        := 'TCipher_NewDES';
  FTestData[0].InitVector := '';
  FTestData[0].Filler     := $FF;
  FTestData[0].Mode       := cmCTSx;
end;

procedure TestTCipher_NewDES.TearDown;
begin
  FCipher_NewDES.Free;
  FCipher_NewDES := nil;
end;

procedure TestTCipher_NewDES.TestClassByName;
var
  ReturnValue : TDECCipherClass;
begin
  ReturnValue := FCipher_NewDES.ClassByName('TCipher_NewDES');
  CheckEquals(TCipher_NewDES, ReturnValue, 'Class is not registered');
end;

procedure TestTCipher_NewDES.TestContext;
var
  ReturnValue: TCipherContext;
begin
  ReturnValue := FCipher_NewDES.Context;

  CheckEquals(  15,  ReturnValue.KeySize);
  CheckEquals(   8,  ReturnValue.BlockSize);
  CheckEquals(   8,  ReturnValue.BufferSize);
  CheckEquals( 120,  ReturnValue.AdditionalBufferSize);
  CheckEquals(   1,  ReturnValue.MinRounds);
  CheckEquals(   1,  ReturnValue.MaxRounds);
  CheckEquals(true,  ReturnValue.NeedsAdditionalBufferBackup);
  CheckEquals(true,  [ctBlock, ctSymmetric] = ReturnValue.CipherType);
end;

procedure TestTCipher_NewDES.TestDecode;
begin
  DoTestDecode(FCipher_NewDES.DecodeStringToBytes, self.Init, self.Done);
end;

procedure TestTCipher_NewDES.TestEncode;
begin
  DoTestEncode(FCipher_NewDES.EncodeStringToBytes, self.Init, self.Done);
end;

procedure TestTCipher_NewDES.TestIdentity;
begin
  CheckEquals($5EE9D8B9, FCipher_NewDES.Identity);
end;

procedure TestTCipher_Q128.Done;
begin
  FCipher_Q128.Done;
end;

procedure TestTCipher_Q128.Init(TestData: TCipherTestData);
begin
  LimitKeyLength(TestData.Key, FCipher_Q128.Context.KeySize);

  FCipher_Q128.Mode := TestData.Mode;
  FCipher_Q128.Init(BytesOf(TestData.Key),
                    BytesOf(TestData.InitVector),
                    TestData.Filler);
end;

procedure TestTCipher_Q128.SetUp;
begin
  FCipher_Q128 := TCipher_Q128.Create;

  SetLength(FTestData, 1);

  FTestData[0].OutputData  := '99aad03dca144e2af81e01a0eaab9f48232d5954547e2b128680e833ebe15eae';
  FTestData[0].InputData   := TFormat_ESCAPE.Decode('\x30\x44\xED\x6E\x45\xA4' +
                                                    '\x96\xF5\xF6\x35\xA2\xEB' +
                                                    '\x3D\x1A\x5D\xD6\xCB\x1D' +
                                                    '\x09\x82\x2D\xBD\xF5\x60' +
                                                    '\xC2\xB8\x58\xA1\x91\xF9' +
                                                    '\x81\xB1');

  FTestData[0].Key        := 'TCipher_Q128';
  FTestData[0].InitVector := '';
  FTestData[0].Filler     := $FF;
  FTestData[0].Mode       := cmCTSx;
end;

procedure TestTCipher_Q128.TearDown;
begin
  FCipher_Q128.Free;
  FCipher_Q128 := nil;
end;

procedure TestTCipher_Q128.TestClassByName;
var
  ReturnValue : TDECCipherClass;
begin
  ReturnValue := FCipher_Q128.ClassByName('TCipher_Q128');
  CheckEquals(TCipher_Q128, ReturnValue, 'Class is not registered');
end;

procedure TestTCipher_Q128.TestContext;
var
  ReturnValue: TCipherContext;
begin
  ReturnValue := FCipher_Q128.Context;

  CheckEquals(  16,  ReturnValue.KeySize);
  CheckEquals(  16,  ReturnValue.BlockSize);
  CheckEquals(  16,  ReturnValue.BufferSize);
  CheckEquals( 256,  ReturnValue.AdditionalBufferSize);
  CheckEquals(   1,  ReturnValue.MinRounds);
  CheckEquals(   1,  ReturnValue.MaxRounds);
  CheckEquals(false, ReturnValue.NeedsAdditionalBufferBackup);
  CheckEquals(true,  [ctBlock, ctSymmetric] = ReturnValue.CipherType);
end;

procedure TestTCipher_Q128.TestDecode;
begin
  DoTestDecode(FCipher_Q128.DecodeStringToBytes, self.Init, self.Done);
end;

procedure TestTCipher_Q128.TestEncode;
begin
  DoTestEncode(FCipher_Q128.EncodeStringToBytes, self.Init, self.Done);
end;

procedure TestTCipher_Q128.TestIdentity;
begin
  CheckEquals($B70802F5, FCipher_Q128.Identity);
end;

procedure TestTCipher_RC2.Done;
begin
  FCipher_RC2.Done;
end;

procedure TestTCipher_RC2.Init(TestData: TCipherTestData);
begin
  LimitKeyLength(TestData.Key, FCipher_RC2.Context.KeySize);

  FCipher_RC2.Mode := TestData.Mode;
  FCipher_RC2.Init(BytesOf(TestData.Key),
                    BytesOf(TestData.InitVector),
                    TestData.Filler);
end;

procedure TestTCipher_RC2.SetUp;
begin
  FCipher_RC2 := TCipher_RC2.Create;

  SetLength(FTestData, 1);

  FTestData[0].OutputData  := '71a2f0fdc2f93c871064b779d3fcdd1153364fd71153775d8d53a72e8b8af9e7';
  FTestData[0].InputData   := TFormat_ESCAPE.Decode('\x30\x44\xED\x6E\x45\xA4' +
                                                    '\x96\xF5\xF6\x35\xA2\xEB' +
                                                    '\x3D\x1A\x5D\xD6\xCB\x1D' +
                                                    '\x09\x82\x2D\xBD\xF5\x60' +
                                                    '\xC2\xB8\x58\xA1\x91\xF9' +
                                                    '\x81\xB1');

  FTestData[0].Key        := 'TCipher_RC2';
  FTestData[0].InitVector := '';
  FTestData[0].Filler     := $FF;
  FTestData[0].Mode       := cmCTSx;
end;

procedure TestTCipher_RC2.TearDown;
begin
  FCipher_RC2.Free;
  FCipher_RC2 := nil;
end;

procedure TestTCipher_RC2.TestClassByName;
var
  ReturnValue : TDECCipherClass;
begin
  ReturnValue := FCipher_RC2.ClassByName('TCipher_RC2');
  CheckEquals(TCipher_RC2, ReturnValue, 'Class is not registered');
end;

procedure TestTCipher_RC2.TestContext;
var
  ReturnValue: TCipherContext;
begin
  ReturnValue := FCipher_RC2.Context;

  CheckEquals( 128,  ReturnValue.KeySize);
  CheckEquals(   8,  ReturnValue.BlockSize);
  CheckEquals(   8,  ReturnValue.BufferSize);
  CheckEquals( 128,  ReturnValue.AdditionalBufferSize);
  CheckEquals(   1,  ReturnValue.MinRounds);
  CheckEquals(   1,  ReturnValue.MaxRounds);
  CheckEquals(false, ReturnValue.NeedsAdditionalBufferBackup);
  CheckEquals(true,  [ctBlock, ctSymmetric] = ReturnValue.CipherType);
end;

procedure TestTCipher_RC2.TestDecode;
begin
  DoTestDecode(FCipher_RC2.DecodeStringToBytes, self.Init, self.Done);
end;

procedure TestTCipher_RC2.TestEncode;
begin
  DoTestEncode(FCipher_RC2.EncodeStringToBytes, self.Init, self.Done);
end;

procedure TestTCipher_RC2.TestIdentity;
begin
  CheckEquals($9AC07A6F, FCipher_RC2.Identity);
end;

procedure TestTCipher_RC5.Done;
begin
  FCipher_RC5.Done;
end;

procedure TestTCipher_RC5.Init(TestData: TCipherTestData);
begin
  LimitKeyLength(TestData.Key, FCipher_RC5.Context.KeySize);

  FCipher_RC5.Mode := TestData.Mode;
  FCipher_RC5.Init(BytesOf(TestData.Key),
                    BytesOf(TestData.InitVector),
                    TestData.Filler);
end;

procedure TestTCipher_RC5.SetUp;
begin
  FCipher_RC5 := TCipher_RC5.Create;

  SetLength(FTestData, 1);

  FTestData[0].OutputData  := '10392ce00b5f097fd6b16c0eb975d5ccfcbeb58d41ac547c8330269daccb0a69';
  FTestData[0].InputData   := TFormat_ESCAPE.Decode('\x30\x44\xED\x6E\x45\xA4' +
                                                    '\x96\xF5\xF6\x35\xA2\xEB' +
                                                    '\x3D\x1A\x5D\xD6\xCB\x1D' +
                                                    '\x09\x82\x2D\xBD\xF5\x60' +
                                                    '\xC2\xB8\x58\xA1\x91\xF9' +
                                                    '\x81\xB1');

  FTestData[0].Key        := 'TCipher_RC5';
  FTestData[0].InitVector := '';
  FTestData[0].Filler     := $FF;
  FTestData[0].Mode       := cmCTSx;
end;

procedure TestTCipher_RC5.TearDown;
begin
  FCipher_RC5.Free;
  FCipher_RC5 := nil;
end;

procedure TestTCipher_RC5.TestClassByName;
var
  ReturnValue : TDECCipherClass;
begin
  ReturnValue := FCipher_RC5.ClassByName('TCipher_RC5');
  CheckEquals(TCipher_RC5, ReturnValue, 'Class is not registered');
end;

procedure TestTCipher_RC5.TestContext;
var
  ReturnValue: TCipherContext;
begin
  ReturnValue := FCipher_RC5.Context;

  CheckEquals( 256,  ReturnValue.KeySize);
  CheckEquals(   8,  ReturnValue.BlockSize);
  CheckEquals(   8,  ReturnValue.BufferSize);
  CheckEquals( 136,  ReturnValue.AdditionalBufferSize);
  CheckEquals(   1,  ReturnValue.MinRounds);
  CheckEquals( 256,  ReturnValue.MaxRounds);
  CheckEquals(false, ReturnValue.NeedsAdditionalBufferBackup);
  CheckEquals(true,  [ctBlock, ctSymmetric] = ReturnValue.CipherType);
end;

procedure TestTCipher_RC5.TestDecode;
begin
  DoTestDecode(FCipher_RC5.DecodeStringToBytes, self.Init, self.Done);
end;

procedure TestTCipher_RC5.TestEncode;
begin
  DoTestEncode(FCipher_RC5.EncodeStringToBytes, self.Init, self.Done);
end;

procedure TestTCipher_RC5.TestIdentity;
begin
  CheckEquals($04A4EFCC, FCipher_RC5.Identity);
end;

procedure TestTCipher_SAFER.Done;
begin
  FCipher_SAFER.Done;
end;

procedure TestTCipher_SAFER.Init(TestData: TCipherTestData);
begin
  LimitKeyLength(TestData.Key, FCipher_SAFER.Context.KeySize);

  FCipher_SAFER.Mode := TestData.Mode;
  FCipher_SAFER.Init(BytesOf(TestData.Key),
                    BytesOf(TestData.InitVector),
                    TestData.Filler);
end;

procedure TestTCipher_SAFER.SetUp;
begin
  FCipher_SAFER := TCipher_SAFER.Create;

  SetLength(FTestData, 1);

  FTestData[0].OutputData  := '003d4920736385aad9c20ade7e9ee9ab24d07434477e211d55f935289884a875';
  FTestData[0].InputData   := TFormat_ESCAPE.Decode('\x30\x44\xED\x6E\x45\xA4' +
                                                    '\x96\xF5\xF6\x35\xA2\xEB' +
                                                    '\x3D\x1A\x5D\xD6\xCB\x1D' +
                                                    '\x09\x82\x2D\xBD\xF5\x60' +
                                                    '\xC2\xB8\x58\xA1\x91\xF9' +
                                                    '\x81\xB1');

  FTestData[0].Key        := 'TCipher_SAFER';
  FTestData[0].InitVector := '';
  FTestData[0].Filler     := $FF;
  FTestData[0].Mode       := cmCTSx;
end;

procedure TestTCipher_SAFER.TearDown;
begin
  FCipher_SAFER.Free;
  FCipher_SAFER := nil;
end;

procedure TestTCipher_SAFER.TestClassByName;
var
  ReturnValue : TDECCipherClass;
begin
  ReturnValue := FCipher_SAFER.ClassByName('TCipher_SAFER');
  CheckEquals(TCipher_SAFER, ReturnValue, 'Class is not registered');
end;

procedure TestTCipher_SAFER.TestContext;
var
  ReturnValue: TCipherContext;
begin
  ReturnValue := FCipher_SAFER.Context;

  CheckEquals(  16,  ReturnValue.KeySize);
  CheckEquals(   8,  ReturnValue.BlockSize);
  CheckEquals(   8,  ReturnValue.BufferSize);
  CheckEquals( 768,  ReturnValue.AdditionalBufferSize);
  CheckEquals(   4,  ReturnValue.MinRounds);
  CheckEquals(  13,  ReturnValue.MaxRounds);
  CheckEquals(false, ReturnValue.NeedsAdditionalBufferBackup);
  CheckEquals(true,  [ctBlock, ctSymmetric] = ReturnValue.CipherType);
end;

procedure TestTCipher_SAFER.TestDecode;
begin
  DoTestDecode(FCipher_SAFER.DecodeStringToBytes, self.Init, self.Done);
end;

procedure TestTCipher_SAFER.TestEncode;
begin
  DoTestEncode(FCipher_SAFER.EncodeStringToBytes, self.Init, self.Done);
end;

procedure TestTCipher_SAFER.TestIdentity;
begin
  CheckEquals($97CE1F8A, FCipher_SAFER.Identity);
end;

procedure TestTCipher_Shark.Done;
begin
  FCipher_Shark.Done;
end;

procedure TestTCipher_Shark.Init(TestData: TCipherTestData);
begin
  LimitKeyLength(TestData.Key, FCipher_Shark.Context.KeySize);

  FCipher_Shark.Mode := TestData.Mode;
  FCipher_Shark.Init(BytesOf(TestData.Key),
                    BytesOf(TestData.InitVector),
                    TestData.Filler);
end;

procedure TestTCipher_Shark.SetUp;
begin
  FCipher_Shark := TCipher_Shark.Create;

  SetLength(FTestData, 2);

  FTestData[0].OutputData := 'e97af38e7c8c56d0426597162c4e68ad867ac9540fe9a2cf7b2fd33e7df8919c';
  FTestData[0].InputData  := TFormat_ESCAPE.Decode('\x30\x44\xED\x6E\x45\xA4' +
                                                   '\x96\xF5\xF6\x35\xA2\xEB' +
                                                   '\x3D\x1A\x5D\xD6\xCB\x1D' +
                                                   '\x09\x82\x2D\xBD\xF5\x60' +
                                                   '\xC2\xB8\x58\xA1\x91\xF9' +
                                                   '\x81\xB1');
  FTestData[0].Key        := 'TCipher_Shark';
  FTestData[0].InitVector := '';
  FTestData[0].Filler     := $FF;
  FTestData[0].Mode       := cmECBx;

  FTestData[1].OutputData := '3968bf331e8ca5ed';
  FTestData[1].InputData  := #0#0#0#0#0#0#0#0;
  FTestData[1].Key        := 'TCipher_Shark';
  FTestData[1].InitVector := '';
  FTestData[1].Filler     := $FF;
  FTestData[1].Mode       := cmECBx;
end;

procedure TestTCipher_Shark.TearDown;
begin
  FCipher_Shark.Free;
  FCipher_Shark := nil;
end;

procedure TestTCipher_Shark.TestClassByName;
var
  ReturnValue : TDECCipherClass;
begin
  ReturnValue := FCipher_Shark.ClassByName('TCipher_Shark');
  CheckEquals(TCipher_Shark, ReturnValue, 'Class is not registered');
end;

procedure TestTCipher_Shark.TestContext;
var
  ReturnValue: TCipherContext;
begin
  ReturnValue := FCipher_Shark.Context;

  CheckEquals(  16,  ReturnValue.KeySize);
  CheckEquals(   8,  ReturnValue.BlockSize);
  CheckEquals(   8,  ReturnValue.BufferSize);
  CheckEquals( 112,  ReturnValue.AdditionalBufferSize);
  CheckEquals(   1,  ReturnValue.MinRounds);
  CheckEquals(   1,  ReturnValue.MaxRounds);
  CheckEquals(false, ReturnValue.NeedsAdditionalBufferBackup);
  CheckEquals(true,  [ctBlock, ctSymmetric] = ReturnValue.CipherType);
end;

procedure TestTCipher_Shark.TestDecode;
begin
  DoTestDecode(FCipher_Shark.DecodeStringToBytes, self.Init, self.Done);
end;

procedure TestTCipher_Shark.TestEncode;
begin
  DoTestEncode(FCipher_Shark.EncodeStringToBytes, self.Init, self.Done);
end;

procedure TestTCipher_Shark.TestIdentity;
begin
  CheckEquals($8E616AD3, FCipher_Shark.Identity);
end;

procedure TestTCipher_Shark_DEC52.Done;
begin
  FCipher_Shark_DEC52.Done;
end;

procedure TestTCipher_Shark_DEC52.DoTestClassByName;
var
  ReturnValue : TDECCipherClass;
begin
  ReturnValue := FCipher_Shark_DEC52.ClassByName('TCipher_Shark_DEC52');
  // This line should never be executed due to ClassByName rising an exception
  // but it suppresses a ReturnValue is not being used compiler warning
  CheckEquals(TCipher_Shark_DEC52, ReturnValue, 'Class is not registered');
end;

procedure TestTCipher_Shark_DEC52.Init(TestData: TCipherTestData);
begin
  LimitKeyLength(TestData.Key, FCipher_Shark_DEC52.Context.KeySize);

  FCipher_Shark_DEC52.Mode := TestData.Mode;
  FCipher_Shark_DEC52.Init(BytesOf(TestData.Key),
                    BytesOf(TestData.InitVector),
                    TestData.Filler);
end;

procedure TestTCipher_Shark_DEC52.SetUp;
begin
  FCipher_Shark_DEC52 := TCipher_Shark_DEC52.Create;

  SetLength(FTestData, 1);

  FTestData[0].OutputData := 'd96521aac0c384609dce1f8bfbab183fa121acf85349c06f273a8915d37ae90b';
  FTestData[0].InputData  := TFormat_ESCAPE.Decode('\x30\x44\xED\x6E\x45\xA4' +
                                                   '\x96\xF5\xF6\x35\xA2\xEB' +
                                                   '\x3D\x1A\x5D\xD6\xCB\x1D' +
                                                   '\x09\x82\x2D\xBD\xF5\x60' +
                                                   '\xC2\xB8\x58\xA1\x91\xF9' +
                                                   '\x81\xB1');
  FTestData[0].Key        := 'TCipher_Shark';
  FTestData[0].InitVector := '';
  FTestData[0].Filler     := $FF;
  FTestData[0].Mode       := cmCTSx;
end;

procedure TestTCipher_Shark_DEC52.TearDown;
begin
  FCipher_Shark_DEC52.Free;
  FCipher_Shark_DEC52 := nil;
end;

procedure TestTCipher_Shark_DEC52.TestClassByName;
begin
  // Class shall not be registered by default
  CheckException(DoTestClassByName, EDECClassNotRegisteredException);
end;

procedure TestTCipher_Shark_DEC52.TestContext;
var
  ReturnValue: TCipherContext;
begin
  ReturnValue := FCipher_Shark_DEC52.Context;

  CheckEquals(  16,  ReturnValue.KeySize);
  CheckEquals(   8,  ReturnValue.BlockSize);
  CheckEquals(   8,  ReturnValue.BufferSize);
  CheckEquals( 112,  ReturnValue.AdditionalBufferSize);
  CheckEquals(   1,  ReturnValue.MinRounds);
  CheckEquals(   1,  ReturnValue.MaxRounds);
  CheckEquals(false, ReturnValue.NeedsAdditionalBufferBackup);
  CheckEquals(true,  [ctBlock, ctSymmetric] = ReturnValue.CipherType);
end;

procedure TestTCipher_Shark_DEC52.TestDecode;
begin
  DoTestDecode(FCipher_Shark_DEC52.DecodeStringToBytes, self.Init, self.Done);
end;

procedure TestTCipher_Shark_DEC52.TestEncode;
begin
  DoTestEncode(FCipher_Shark_DEC52.EncodeStringToBytes, self.Init, self.Done);
end;

procedure TestTCipher_Shark_DEC52.TestIdentity;
begin
  CheckEquals($7901E07F, FCipher_Shark_DEC52.Identity);
end;

procedure TestTCipher_Skipjack.Done;
begin
  FCipher_Skipjack.Done;
end;

procedure TestTCipher_Skipjack.Init(TestData: TCipherTestData);
begin
  LimitKeyLength(TestData.Key, FCipher_Skipjack.Context.KeySize);

  FCipher_Skipjack.Mode := TestData.Mode;
  FCipher_Skipjack.Init(BytesOf(TestData.Key),
                    BytesOf(TestData.InitVector),
                    TestData.Filler);
end;

procedure TestTCipher_Skipjack.SetUp;
begin
  FCipher_Skipjack := TCipher_Skipjack.Create;

  SetLength(FTestData, 1);

  FTestData[0].OutputData  := 'd513a692ec2435e8174e2b555e8d27dac99aa9b9213da0011802b30eb7b551ea';
  FTestData[0].InputData   := TFormat_ESCAPE.Decode('\x30\x44\xED\x6E\x45\xA4' +
                                                    '\x96\xF5\xF6\x35\xA2\xEB' +
                                                    '\x3D\x1A\x5D\xD6\xCB\x1D' +
                                                    '\x09\x82\x2D\xBD\xF5\x60' +
                                                    '\xC2\xB8\x58\xA1\x91\xF9' +
                                                    '\x81\xB1');

  FTestData[0].Key        := 'TCipher_Skipjack';
  FTestData[0].InitVector := '';
  FTestData[0].Filler     := $FF;
  FTestData[0].Mode       := cmCTSx;
end;

procedure TestTCipher_Skipjack.TearDown;
begin
  FCipher_Skipjack.Free;
  FCipher_Skipjack := nil;
end;

procedure TestTCipher_Skipjack.TestClassByName;
var
  ReturnValue : TDECCipherClass;
begin
  ReturnValue := FCipher_Skipjack.ClassByName('TCipher_Skipjack');
  CheckEquals(TCipher_Skipjack, ReturnValue, 'Class is not registered');
end;

procedure TestTCipher_Skipjack.TestContext;
var
  ReturnValue: TCipherContext;
begin
  ReturnValue := FCipher_Skipjack.Context;

  CheckEquals(  10,  ReturnValue.KeySize);
  CheckEquals(   8,  ReturnValue.BlockSize);
  CheckEquals(   8,  ReturnValue.BufferSize);
  CheckEquals(2560,  ReturnValue.AdditionalBufferSize);
  CheckEquals(   1,  ReturnValue.MinRounds);
  CheckEquals(   1,  ReturnValue.MaxRounds);
  CheckEquals(false, ReturnValue.NeedsAdditionalBufferBackup);
  CheckEquals(true,  [ctBlock, ctSymmetric] = ReturnValue.CipherType);
end;

procedure TestTCipher_Skipjack.TestDecode;
begin
  DoTestDecode(FCipher_Skipjack.DecodeStringToBytes, self.Init, self.Done);
end;

procedure TestTCipher_Skipjack.TestEncode;
begin
  DoTestEncode(FCipher_Skipjack.EncodeStringToBytes, self.Init, self.Done);
end;

procedure TestTCipher_Skipjack.TestIdentity;
begin
  CheckEquals($D2283F49, FCipher_Skipjack.Identity);
end;

procedure TestTCipher_TEA.Done;
begin
  FCipher_TEA.Done;
end;

procedure TestTCipher_TEA.Init(TestData: TCipherTestData);
begin
  LimitKeyLength(TestData.Key, FCipher_TEA.Context.KeySize);

  FCipher_TEA.Mode := TestData.Mode;
  FCipher_TEA.Init(BytesOf(TestData.Key),
                    BytesOf(TestData.InitVector),
                    TestData.Filler);
end;

procedure TestTCipher_TEA.SetUp;
begin
  FCipher_TEA := TCipher_TEA.Create;

  SetLength(FTestData, 1);

  FTestData[0].OutputData  := 'b7b8aabb264b06f97086b0e4560429ccbf55ea4eef59261819b0037c298ce277';
  FTestData[0].InputData   := TFormat_ESCAPE.Decode('\x30\x44\xED\x6E\x45\xA4' +
                                                    '\x96\xF5\xF6\x35\xA2\xEB' +
                                                    '\x3D\x1A\x5D\xD6\xCB\x1D' +
                                                    '\x09\x82\x2D\xBD\xF5\x60' +
                                                    '\xC2\xB8\x58\xA1\x91\xF9' +
                                                    '\x81\xB1');

  FTestData[0].Key        := 'TCipher_TEA';
  FTestData[0].InitVector := '';
  FTestData[0].Filler     := $FF;
  FTestData[0].Mode       := cmCTSx;
end;

procedure TestTCipher_TEA.TearDown;
begin
  FCipher_TEA.Free;
  FCipher_TEA := nil;
end;

procedure TestTCipher_TEA.TestClassByName;
var
  ReturnValue : TDECCipherClass;
begin
  ReturnValue := FCipher_TEA.ClassByName('TCipher_TEA');
  CheckEquals(TCipher_TEA, ReturnValue, 'Class is not registered');
end;

procedure TestTCipher_TEA.TestContext;
var
  ReturnValue: TCipherContext;
begin
  ReturnValue := FCipher_TEA.Context;

  CheckEquals(  16,  ReturnValue.KeySize);
  CheckEquals(   8,  ReturnValue.BlockSize);
  CheckEquals(   8,  ReturnValue.BufferSize);
  CheckEquals(  32,  ReturnValue.AdditionalBufferSize);
  CheckEquals(  16,  ReturnValue.MinRounds);
  CheckEquals( 256,  ReturnValue.MaxRounds);
  CheckEquals(false, ReturnValue.NeedsAdditionalBufferBackup);
  CheckEquals(true,  [ctBlock, ctSymmetric] = ReturnValue.CipherType);
end;

procedure TestTCipher_TEA.TestDecode;
begin
  DoTestDecode(FCipher_TEA.DecodeStringToBytes, self.Init, self.Done);
end;

procedure TestTCipher_TEA.TestEncode;
begin
  DoTestEncode(FCipher_TEA.EncodeStringToBytes, self.Init, self.Done);
end;

procedure TestTCipher_TEA.TestIdentity;
begin
  CheckEquals($011B81DD, FCipher_TEA.Identity);
end;

procedure TestTCipher_XTEA.Done;
begin
  FCipher_XTEA.Done;
end;

procedure TestTCipher_XTEA.Init(TestData: TCipherTestData);
begin
  LimitKeyLength(TestData.Key, FCipher_XTEA.Context.KeySize);

  FCipher_XTEA.Mode := TestData.Mode;
  FCipher_XTEA.Init(BytesOf(TestData.Key),
                    BytesOf(TestData.InitVector),
                    TestData.Filler);
end;

procedure TestTCipher_XTEA.SetUp;
begin
  // Source of the test data used:
  // https://github.com/froydnj/ironclad/blob/master/testing/test-vectors/xtea.testvec
  FCipher_XTEA := TCipher_XTEA.Create;
{ TODO : Should be specified via FTestData? But how to apply? }
  FCipher_XTEA.Rounds := 32;

  SetLength(FTestData, 4);
  FTestData[0].OutputData := 'd8d4e9ded91e13f7';
  FTestData[0].InputData  := TFormat_HEX.Decode('0000000000000000');

  FTestData[0].Key        := #$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00;
  FTestData[0].InitVector := '';
  FTestData[0].Filler     := $00;
  FTestData[0].Mode       := cmECBx;

  FTestData[1].OutputData := '058c7e0537191550';
  FTestData[1].InputData  := TFormat_HEX.Decode('0000000000000000');

  FTestData[1].Key        := RawByteString(#$00#$00#$00#$80#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00);
  FTestData[1].InitVector := '';
  FTestData[1].Filler     := $00;
  FTestData[1].Mode       := cmECBx;

  FTestData[2].OutputData := 'ef175e2818e3d22f';
  FTestData[2].InputData  := TFormat_HEX.Decode('1a1a1a1a1a1a1a1a');

  FTestData[2].Key        := #$1a#$1a#$1a#$1a#$1a#$1a#$1a#$1a#$1a#$1a#$1a#$1a#$1a#$1a#$1a#$1a;
  FTestData[2].InitVector := '';
  FTestData[2].Filler     := $00;
  FTestData[2].Mode       := cmECBx;

  FTestData[3].Key        := TFormat_BigEndian32.Decode(
                               RawByteString(#$2B#$D6#$45#$9F#$82#$C5#$B3#$00 +
                                             #$95#$2C#$49#$10#$48#$81#$FF#$48));
  FTestData[3].InitVector := '';
  FTestData[3].Filler     := $00;
  FTestData[3].Mode       := cmECBx;

  FTestData[3].OutputData := '0a1eb4673a595fa0';
  FTestData[3].InputData  := TFormat_HEX.Decode('144702ea844d5cad');
end;

procedure TestTCipher_XTEA.TearDown;
begin
  FCipher_XTEA.Free;
  FCipher_XTEA := nil;
end;

procedure TestTCipher_XTEA.TestClassByName;
var
  ReturnValue : TDECCipherClass;
begin
  ReturnValue := FCipher_XTEA.ClassByName('TCipher_XTEA');
  CheckEquals(TCipher_XTEA, ReturnValue, 'Class is not registered');
end;

procedure TestTCipher_XTEA.TestContext;
var
  ReturnValue: TCipherContext;
begin
  ReturnValue := FCipher_XTEA.Context;

  CheckEquals(  16,  ReturnValue.KeySize);
  CheckEquals(   8,  ReturnValue.BlockSize);
  CheckEquals(   8,  ReturnValue.BufferSize);
  CheckEquals(  32,  ReturnValue.AdditionalBufferSize);
  CheckEquals(  16,  ReturnValue.MinRounds);
  CheckEquals( 256,  ReturnValue.MaxRounds);
  CheckEquals(false, ReturnValue.NeedsAdditionalBufferBackup);
  CheckEquals(true,  [ctBlock, ctSymmetric] = ReturnValue.CipherType);
end;

procedure TestTCipher_XTEA.TestDecode;
begin
  DoTestDecode(FCipher_XTEA.DecodeStringToBytes, self.Init, self.Done);
end;

procedure TestTCipher_XTEA.TestEncode;
begin
  DoTestEncode(FCipher_XTEA.EncodeStringToBytes, self.Init, self.Done);
end;

procedure TestTCipher_XTEA.TestIdentity;
begin
  CheckEquals($CDBB621D, FCipher_XTEA.Identity);
end;

procedure TestTCipher_XTEA_DEC52.Done;
begin
  FCipher_XTEA_DEC52.Done;
end;

procedure TestTCipher_XTEA_DEC52.DoTestClassByName;
var
  ReturnValue : TDECCipherClass;
begin
  ReturnValue := FCipher_XTEA_DEC52.ClassByName('TCipher_XTEA_DEC52');
  // This line should never be executed due to ClassByName rising an exception
  // but it suppresses a ReturnValue is not being used compiler warning
  CheckEquals(TCipher_XTEA_DEC52, ReturnValue, 'Class is not registered');
end;

procedure TestTCipher_XTEA_DEC52.Init(TestData: TCipherTestData);
begin
  LimitKeyLength(TestData.Key, FCipher_XTEA_DEC52.Context.KeySize);

  FCipher_XTEA_DEC52.Mode := TestData.Mode;
  FCipher_XTEA_DEC52.Init(BytesOf(TestData.Key),
                    BytesOf(TestData.InitVector),
                    TestData.Filler);
end;

procedure TestTCipher_XTEA_DEC52.SetUp;
begin
  // Source of the test data used: Hagen's original test vectors
  FCipher_XTEA_DEC52 := TCipher_XTEA_DEC52.Create;
  FCipher_XTEA_DEC52.Rounds := 16;

  SetLength(FTestData, 1);
  FTestData[0].OutputData  := 'cd7ebba2921a4b3be29e62cff71da5df63339429e2367c663ff81af90278bfa1';
  FTestData[0].InputData   := TFormat_ESCAPE.Decode('\x30\x44\xED\x6E\x45\xA4' +
                                                    '\x96\xF5\xF6\x35\xA2\xEB' +
                                                    '\x3D\x1A\x5D\xD6\xCB\x1D' +
                                                    '\x09\x82\x2D\xBD\xF5\x60' +
                                                    '\xC2\xB8\x58\xA1\x91\xF9' +
                                                    '\x81\xB1');

  // The key for this test vector is the old name of the class which was
  // TCipher_TEAN as the vector already existed in DEC 5.2 but the class got
  // renamed later on
  FTestData[0].Key        := 'TCipher_TEAN';
  FTestData[0].InitVector := '';
  FTestData[0].Filler     := $FF;
  FTestData[0].Mode       := cmCTSx;
end;

procedure TestTCipher_XTEA_DEC52.TearDown;
begin
  FCipher_XTEA_DEC52.Free;
  FCipher_XTEA_DEC52 := nil;
end;

procedure TestTCipher_XTEA_DEC52.TestClassByName;
begin
  // Class shall not be registered by default
  CheckException(DoTestClassByName, EDECClassNotRegisteredException);
end;

procedure TestTCipher_XTEA_DEC52.TestContext;
var
  ReturnValue: TCipherContext;
begin
  ReturnValue := FCipher_XTEA_DEC52.Context;

  CheckEquals(  16,  ReturnValue.KeySize);
  CheckEquals(   8,  ReturnValue.BlockSize);
  CheckEquals(   8,  ReturnValue.BufferSize);
  CheckEquals(  32,  ReturnValue.AdditionalBufferSize);
  CheckEquals(  16,  ReturnValue.MinRounds);
  CheckEquals( 256,  ReturnValue.MaxRounds);
  CheckEquals(false, ReturnValue.NeedsAdditionalBufferBackup);
  CheckEquals(true,  [ctBlock, ctSymmetric] = ReturnValue.CipherType);
end;

procedure TestTCipher_XTEA_DEC52.TestDecode;
begin
  DoTestDecode(FCipher_XTEA_DEC52.DecodeStringToBytes, self.Init, self.Done);
end;

procedure TestTCipher_XTEA_DEC52.TestEncode;
begin
  DoTestEncode(FCipher_XTEA_DEC52.EncodeStringToBytes, self.Init, self.Done);
end;

procedure TestTCipher_XTEA_DEC52.TestIdentity;
begin
  CheckEquals($59A6BE1E, FCipher_XTEA_DEC52.Identity);
end;

{ TCipherBasis }

function TCipherBasis.ConvertHexVectorToBytes(Vector: string): TBytes;
var
  sl: TStringList;
  i : Integer;
  s : string;
begin
  System.Assert(Length(Vector) mod 4 = 0, 'Char count of ' + Vector + ' is not integral');

  SetLength(Result, Length(Vector) div 4);

  if (Vector <> '') then
  begin
    sl := TStringList.Create;
    try
      sl.Delimiter := '\';
      sl.DelimitedText := StringReplace(Vector, 'x', '', [rfReplaceAll]);

      // first element is always empty
      sl.Delete(0);
      s := '';
      for i := 0 to sl.Count - 1 do
      begin
        sl[i] := '0x' + sl[i];
        Result[i] := StrToInt(sl[i]);
      end;
    finally
      sl.Free;
    end;
  end;
end;

procedure TCipherBasis.DoTestDecode(DecodeFunct: TEncodeDecodeFunc; InitProc: TInitProc; DoneProc: TDoneProc);
var
  Data          : TCipherTestData;
  Result        : TBytes;
  TempResultHex : RawByteString;
begin
{ TODO :
Das Problem ist hier: dass wir zu low level testen, da die bisherigen Textvektoren
ja immer von einem bestimmten CipherModus ausgehen, und nicht die
einzelnen DoEncode/DoDecode primitive. Diese sind später zu testen, wenn
wir die bisherigen Vektoren testen können. Dann können wir die nötigen
Daten synthetisieren. }
  for Data in FTestData do
  begin
    InitProc(Data);
    Result := DecodeFunct(RawByteString(Data.OutputData), TFormat_HEXL);
    DoneProc;

    TempResultHex := RawByteString(StringOf(Result));

    CheckEquals(TFormat_HEXL.Encode(Data.InputData), TFormat_HEXL.Encode(TempResultHex));
  end;
end;

procedure TCipherBasis.DoTestEncode(EncodeFunc: TEncodeDecodeFunc; InitProc: TInitProc; DoneProc: TDoneProc);
var
  Data          : TCipherTestData;
  Result        : TBytes;
  TempResultHex : RawByteString;
begin
{ TODO :
Das Problem ist hier: dass wir zu low level testen, da die bisherigen Testvektoren
ja immer von einem bestimmten CipherModus ausgehen, und nicht die
einzelnen DoEncode/DoDecode primitive. Diese sind später zu testen, wenn
wir die bisherigen Vektoren testen können. Dann können wir die nötigen
Daten synthetisieren. }
  for Data in FTestData do
  begin
    InitProc(Data);
    Result := EncodeFunc(RawByteString(Data.InputData), TFormat_COPY);
    DoneProc;

    TempResultHex := TFormat_HEXL.Encode(Result[0], length(Result));

    CheckEquals(Data.OutputData, TempResultHex);
  end;
end;

procedure TCipherBasis.LimitKeyLength(var Key: RawByteString; KeySize: Integer);
begin
  if Length(Key) > KeySize then
    Delete(Key, KeySize + 1, length(Key));
end;

{ TestTDECCipher }

procedure TestTDECCipher.SetUp;
begin
  inherited;
end;

procedure TestTDECCipher.TearDown;
begin
  inherited;
end;

procedure TestTDECCipher.TestIsClassListCreated;
begin
  CheckEquals(true, assigned(TDECCipher.ClassList), 'Class list has not been created in initialization');
end;

procedure TestTDECCipher.TestValidCipherSetDefaultCipherClass;
var
  result : Boolean;
begin
  // Asumption: nobody has called SetDefaultCipher yet
  result := ValidCipher(nil) = TCipher_Null;
  CheckEquals(true, result, 'Initial default cipher is not TCipher_Null');

  try
    SetDefaultCipherClass(TCipher_AES);
    result := ValidCipher(nil) = TCipher_AES;
    CheckEquals(true, result, 'Changed default cipher is not TCipher_AES');

    SetDefaultCipherClass(TCipher_TEA);
    result := ValidCipher(nil) = TCipher_TEA;
    CheckEquals(true, result, 'Changed default cipher is not TCipher_TEA');

    result := ValidCipher(TCipher_XTEA) = TCipher_XTEA;
    CheckEquals(true, result, 'Passed cipher is not TCipher_XTEA');
  finally
    SetDefaultCipherClass(TCipher_Null);
  end;
end;

{ TestTCipher_AES }

procedure TestTCipher_AES.Done;
begin
  FCipher_AES.Done;
end;

procedure TestTCipher_AES.Init(TestData: TCipherTestData);
begin
  LimitKeyLength(TestData.Key, FCipher_AES.Context.KeySize);

  FCipher_AES.Mode := TestData.Mode;
  FCipher_AES.Init(BytesOf(TestData.Key),
                   BytesOf(TestData.InitVector),
                   TestData.Filler);
end;

procedure TestTCipher_AES.SetUp;
begin
  FCipher_AES := TCipher_AES.Create;

  SetLength(FTestData, 5);
  FTestData[0].OutputData  := '946d2b5ee0ad1b5ca523a513958b3d2d9387f3374551f6589be7901b3687f9a9';
  FTestData[0].InputData   := TFormat_ESCAPE.Decode('\x30\x44\xED\x6E\x45\xA4' +
                                                    '\x96\xF5\xF6\x35\xA2\xEB' +
                                                    '\x3D\x1A\x5D\xD6\xCB\x1D' +
                                                    '\x09\x82\x2D\xBD\xF5\x60' +
                                                    '\xC2\xB8\x58\xA1\x91\xF9' +
                                                    '\x81\xB1');

  FTestData[0].Key        := 'TCipher_Rijndael';
  FTestData[0].InitVector := '';
  FTestData[0].Filler     := $FF;
  FTestData[0].Mode       := cmCTSx;

  // Original test vectors from Nist FIPS 197 AES standard description
  // AES 128
  FTestData[1].OutputData  := '69c4e0d86a7b0430d8cdb78070b4c55a';
  FTestData[1].InputData   := TFormat_HEXL.Decode('00112233445566778899aabbccddeeff');

  FTestData[1].Key        := TFormat_HEXL.Decode('000102030405060708090a0b0c0d0e0f');
  FTestData[1].InitVector := '';
  FTestData[1].Filler     := $FF;
  FTestData[1].Mode       := cmECBx;

  // AES 192
  FTestData[2].OutputData  := 'dda97ca4864cdfe06eaf70a0ec0d7191';
  FTestData[2].InputData   := TFormat_HEXL.Decode('00112233445566778899aabbccddeeff');

  FTestData[2].Key        := TFormat_HEXL.Decode('000102030405060708090a0b0c0d0e0f1011121314151617');
  FTestData[2].InitVector := '';
  FTestData[2].Filler     := $FF;
  FTestData[2].Mode       := cmECBx;

  // AES 256
  FTestData[3].OutputData  := '8ea2b7ca516745bfeafc49904b496089';
  FTestData[3].InputData   := TFormat_HEXL.Decode('00112233445566778899aabbccddeeff');

  FTestData[3].Key        := TFormat_HEXL.Decode('000102030405060708090a0b0c0d0e0f'+
                                                 '101112131415161718191a1b1c1d1e1f');
  FTestData[3].InitVector := '';
  FTestData[3].Filler     := $FF;
  FTestData[3].Mode       := cmECBx;

  // CBC
  FTestData[4].OutputData  := '8859653cb4c4e4ca3add490015ac8860fa59d1e233301563b184fcca95790c8c';
  FTestData[4].InputData   := 'abcdefghijklmnopqrstuv0123456789';

  FTestData[4].Key        := TFormat_HEXL.Decode('30313233343536373839303132333435');
  FTestData[4].InitVector := TFormat_HEXL.Decode('30313233343536373839303132333435');
  FTestData[4].Filler     := $FF;
  FTestData[4].Mode       := cmCBCx;
end;

procedure TestTCipher_AES.TearDown;
begin
  FCipher_AES.Free;
  FCipher_AES := nil;
end;

procedure TestTCipher_AES.TestClassByName;
var
  ReturnValue : TDECCipherClass;
begin
  ReturnValue := FCipher_AES.ClassByName('TCipher_AES');
  CheckEquals(TCipher_AES, ReturnValue, 'Class is not registered');
end;

procedure TestTCipher_AES.TestContext;
var
  ReturnValue: TCipherContext;
begin
  ReturnValue := FCipher_AES.Context;

  CheckEquals(  32,  ReturnValue.KeySize);
  CheckEquals(  16,  ReturnValue.BlockSize);
  CheckEquals(  16,  ReturnValue.BufferSize);
  CheckEquals( 480,  ReturnValue.AdditionalBufferSize);
  CheckEquals(   1,  ReturnValue.MinRounds);
  CheckEquals(   1,  ReturnValue.MaxRounds);
  CheckEquals(false, ReturnValue.NeedsAdditionalBufferBackup);
  CheckEquals(true,  [ctBlock, ctSymmetric] = ReturnValue.CipherType);
end;

procedure TestTCipher_AES.TestDecode;
begin
  DoTestDecode(FCipher_AES.DecodeStringToBytes, self.Init, self.Done);
end;

procedure TestTCipher_AES.TestEncode;
begin
  DoTestEncode(FCipher_AES.EncodeStringToBytes, self.Init, self.Done);
end;

procedure TestTCipher_AES.TestIdentity;
begin
  CheckEquals($E84F910E, FCipher_AES.Identity);
end;

{ TestTCipher_Rijndael }

procedure TestTCipher_Rijndael.Done;
begin
  FCipher_Rijndael.Done;
end;

procedure TestTCipher_Rijndael.Init(TestData: TCipherTestData);
begin
  LimitKeyLength(TestData.Key, FCipher_Rijndael.Context.KeySize);

  FCipher_Rijndael.Mode := TestData.Mode;
  FCipher_Rijndael.Init(BytesOf(TestData.Key),
                   BytesOf(TestData.InitVector),
                   TestData.Filler);
end;

procedure TestTCipher_Rijndael.SetUp;
begin
  FCipher_Rijndael := TCipher_Rijndael.Create;

  SetLength(FTestData, 1);
  FTestData[0].OutputData  := '946d2b5ee0ad1b5ca523a513958b3d2d9387f3374551f6589be7901b3687f9a9';
  FTestData[0].InputData   := TFormat_ESCAPE.Decode('\x30\x44\xED\x6E\x45\xA4' +
                                                    '\x96\xF5\xF6\x35\xA2\xEB' +
                                                    '\x3D\x1A\x5D\xD6\xCB\x1D' +
                                                    '\x09\x82\x2D\xBD\xF5\x60' +
                                                    '\xC2\xB8\x58\xA1\x91\xF9' +
                                                    '\x81\xB1');

  FTestData[0].Key        := 'TCipher_Rijndael';
  FTestData[0].InitVector := '';
  FTestData[0].Filler     := $FF;
  FTestData[0].Mode       := cmCTSx;
end;

procedure TestTCipher_Rijndael.TearDown;
begin
  FCipher_Rijndael.Free;
  FCipher_Rijndael := nil;
end;

procedure TestTCipher_Rijndael.DoTestClassByName;
var
  ReturnValue : TDECCipherClass;
begin
  ReturnValue := FCipher_Rijndael.ClassByName('TCipher_Rijndael');
  // This line should never be executed due to ClassByName rising an exception
  // but it suppresses a ReturnValue is not being used compiler warning
  CheckEquals(TCipher_Rijndael, ReturnValue, 'Class is not registered');
end;

procedure TestTCipher_Rijndael.TestClassByName;
begin
  CheckException(DoTestClassByName, EDECClassNotRegisteredException);
end;

procedure TestTCipher_Rijndael.TestContext;
var
  ReturnValue: TCipherContext;
begin
  ReturnValue := FCipher_Rijndael.Context;

  CheckEquals(  32,  ReturnValue.KeySize);
  CheckEquals(  16,  ReturnValue.BlockSize);
  CheckEquals(  16,  ReturnValue.BufferSize);
  CheckEquals( 480,  ReturnValue.AdditionalBufferSize);
  CheckEquals(   1,  ReturnValue.MinRounds);
  CheckEquals(   1,  ReturnValue.MaxRounds);
  CheckEquals(false, ReturnValue.NeedsAdditionalBufferBackup);
  CheckEquals(true,  [ctBlock, ctSymmetric] = ReturnValue.CipherType);
end;

procedure TestTCipher_Rijndael.TestDecode;
begin
  DoTestDecode(FCipher_Rijndael.DecodeStringToBytes, self.Init, self.Done);
end;

procedure TestTCipher_Rijndael.TestEncode;
begin
  DoTestEncode(FCipher_Rijndael.EncodeStringToBytes, self.Init, self.Done);
end;

procedure TestTCipher_Rijndael.TestIdentity;
begin
  CheckEquals($F8B830A5, FCipher_Rijndael.Identity);
end;

initialization
  // Register all test classes
  {$IFDEF DUnitX}
  TDUnitX.RegisterTestFixture(TestTDECCipher);
  TDUnitX.RegisterTestFixture(TestTCipher_Null);
  TDUnitX.RegisterTestFixture(TestTCipher_Blowfish);
  TDUnitX.RegisterTestFixture(TestTCipher_Twofish);
  TDUnitX.RegisterTestFixture(TestTCipher_IDEA);
  TDUnitX.RegisterTestFixture(TestTCipher_Cast256);
  TDUnitX.RegisterTestFixture(TestTCipher_Mars);
  TDUnitX.RegisterTestFixture(TestTCipher_RC4);
  TDUnitX.RegisterTestFixture(TestTCipher_RC6);
  TDUnitX.RegisterTestFixture(TestTCipher_AES);
  TDUnitX.RegisterTestFixture(TestTCipher_Rijndael);
  TDUnitX.RegisterTestFixture(TestTCipher_Square);
  TDUnitX.RegisterTestFixture(TestTCipher_SCOP);
  TDUnitX.RegisterTestFixture(TestTCipher_SCOP_DEC52);
  TDUnitX.RegisterTestFixture(TestTCipher_Sapphire);
  TDUnitX.RegisterTestFixture(TestTCipher_1DES);
  TDUnitX.RegisterTestFixture(TestTCipher_2DES);
  TDUnitX.RegisterTestFixture(TestTCipher_3DES);
  TDUnitX.RegisterTestFixture(TestTCipher_2DDES);
  TDUnitX.RegisterTestFixture(TestTCipher_3DDES);;
  TDUnitX.RegisterTestFixture(TestTCipher_3TDES);;
  TDUnitX.RegisterTestFixture(TestTCipher_3Way);
  TDUnitX.RegisterTestFixture(TestTCipher_Cast128);
  TDUnitX.RegisterTestFixture(TestTCipher_Gost);
  TDUnitX.RegisterTestFixture(TestTCipher_Magma);
  TDUnitX.RegisterTestFixture(TestTCipher_Misty);
  TDUnitX.RegisterTestFixture(TestTCipher_NewDES);
  TDUnitX.RegisterTestFixture(TestTCipher_Q128);
  TDUnitX.RegisterTestFixture(TestTCipher_RC2);
  TDUnitX.RegisterTestFixture(TestTCipher_RC5);
  TDUnitX.RegisterTestFixture(TestTCipher_SAFER);
  TDUnitX.RegisterTestFixture(TestTCipher_Shark);
  TDUnitX.RegisterTestFixture(TestTCipher_Shark_DEC52);
  TDUnitX.RegisterTestFixture(TestTCipher_Skipjack);
  TDUnitX.RegisterTestFixture(TestTCipher_TEA);
  TDUnitX.RegisterTestFixture(TestTCipher_XTEA);
  TDUnitX.RegisterTestFixture(TestTCipher_XTEA_DEC52);
  {$ELSE}
  RegisterTests('DECCipher', [TestTDECCipher.Suite,
                              TestTCipher_Null.Suite,
                              TestTCipher_Blowfish.Suite,
                              TestTCipher_Twofish.Suite,
                              TestTCipher_IDEA.Suite,
                              TestTCipher_Cast256.Suite,
                              TestTCipher_Mars.Suite,
                              TestTCipher_RC4.Suite,
                              TestTCipher_RC6.Suite,
                              TestTCipher_AES.Suite,
                              TestTCipher_Rijndael.Suite,
                              TestTCipher_Square.Suite,
                              TestTCipher_SCOP.Suite,
                              TestTCipher_SCOP_DEC52.Suite,
                              TestTCipher_Sapphire.Suite,
                              TestTCipher_1DES.Suite,
                              TestTCipher_2DES.Suite,
                              TestTCipher_3DES.Suite,
                              TestTCipher_2DDES.Suite,
                              TestTCipher_3DDES.Suite,
                              TestTCipher_3TDES.Suite,
                              TestTCipher_3Way.Suite,
                              TestTCipher_Cast128.Suite,
                              TestTCipher_Gost.Suite,
                              TestTCipher_Magma.Suite,
                              TestTCipher_Misty.Suite,
                              TestTCipher_NewDES.Suite,
                              TestTCipher_Q128.Suite,
                              TestTCipher_RC2.Suite,
                              TestTCipher_RC5.Suite,
                              TestTCipher_SAFER.Suite,
                              TestTCipher_Shark.Suite,
                              TestTCipher_Shark_DEC52.Suite,
                              TestTCipher_Skipjack.Suite,
                              TestTCipher_TEA.Suite,
                              TestTCipher_XTEA.Suite,
                              TestTCipher_XTEA_DEC52.Suite]);
  {$ENDIF}
end.

