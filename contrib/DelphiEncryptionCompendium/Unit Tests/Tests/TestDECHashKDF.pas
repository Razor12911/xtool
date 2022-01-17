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
unit TestDECHashKDF;

interface

// Needs to be included before any other statements
{$INCLUDE TestDefines.inc}
{$INCLUDE ..\..\Source\DECOptions.inc}

uses
  System.Classes, System.SysUtils, Generics.Collections,
  {$IFDEF DUnitX}
  DUnitX.TestFramework,DUnitX.DUnitCompatibility,
  {$ELSE}
  TestFramework,
  {$ENDIF}
  TestDECTestDataContainer, DECTypes, DECBaseClass, DECHash, DECHashBase,
  DECHashAuthentication, DECUtil, DECFormatBase;

type
  /// <summary>
  ///   Meta class to store class references in the list as there is no pointer
  ///   to class methods as it seems
  /// </summary>
  THashClass = class of TDECHash;

  /// <summary>
  ///   Type of the KDF or MGF variant to be tested
  /// </summary>
  TKDFMGFAlgorithm = (ktKDF1, ktKDF2, ktKDF3, ktKDFx, ktMGF1, ktMGFx);

  /// <summary>
  ///   Record containing everything necessary for a single key deviation
  ///   method test
  /// </summary>
  TKeyDeviationTestData = record
    /// <summary>
    ///   Test data as normal text
    /// </summary>
    InputData  : RawByteString;
    /// <summary>
    ///   Optional parameter for some key deviation tests.
    /// </summary>
    SeedData   : RawByteString;
    /// <summary>
    ///   Test output in hexadecimal format
    /// </summary>
    OutputData : RawByteString;
    /// <summary>
    ///   Requested output length
    /// </summary>
    MaskSize   : Integer;
    /// <summary>
    ///   If the entry is for a KDF1-KDF3 or KDFx test define which test method to call
    /// </summary>
    Algorithm  : TKDFMGFAlgorithm;
    /// <summary>
    ///   Class reference containing the class methods to be tested
    /// </summary>
    HashClass  : TDECHashAuthenticationClass;
    /// <summary>
    ///   Index, which i being used for the KDFx and MGFx variants of the
    ///   original author of DEC
    /// </summary>
    Index      : UInt32;
  end;

  /// <summary>
  ///   List of all the test cases
  /// </summary>
  TKeyDeviationTestList = class(TList<TKeyDeviationTestData>)
  public
    /// <summary>
    ///   Add one entry to the list
    /// </summary>
    /// <param name="InputData">
    ///   Test data as normal text
    /// </param>
    /// <param name="OutputData">
    ///   Test output in hexadecimal format
    /// </param>
    /// <param name="MaskSize">
    ///   Requested output length
    /// </param>
    /// <param name="HashClass">
    ///   Class reference containing the class methods to be tested
    /// </param>
    /// <param name="AlgorithmType">
    ///   Algorithm for which the test data specified is
    /// </param>
    /// <param name="Index">
    ///   Index, which i being used for the KDFx and MGFx variants of the
    ///   original author of DEC
    /// </param>
    procedure Add(const InputData, OutputData: RawByteString;
                  MaskSize : Integer; HashClass: TDECHashAuthenticationClass;
                  AlgorithmType : TKDFMGFAlgorithm = ktKDF1;
                  Index : UInt32 = 1); overload;
    /// <summary>
    ///   Add one entry to the list
    /// </summary>
    /// <param name="InputData">
    ///   Test data as normal text
    /// </param>
    /// <param name="SeedData">
    ///   Optional parameter for some tests. Given in hexadecimal format.
    /// </param>
    /// <param name="OutputData">
    ///   Test output in hexadecimal format
    /// </param>
    /// <param name="MaskSize">
    ///   Requested output length
    /// </param>
    /// <param name="HashClass">
    ///   Class reference containing the class methods to be tested
    /// </param>
    /// <param name="AlgorithmType">
    ///   Algorithm for which the test data specified is
    /// </param>
    /// <param name="Index">
    ///   Index, which i being used for the KDFx and MGFx variants of the
    ///   original author of DEC
    /// </param>
    procedure Add(const InputData, SeedData, OutputData: RawByteString;
                  MaskSize : Integer; HashClass: TDECHashAuthenticationClass;
                  AlgorithmType : TKDFMGFAlgorithm = ktKDF1;
                  Index : UInt32 = 1); overload;
  end;

  {$IFDEF DUnitX} [TestFixture] {$ENDIF}
  /// <summary>
  ///   All test cases for the MGF1 class methods. These are in this class rather
  ///   than in TestTDECHash as the MGF1 class methods can only be called on a
  ///   concrete hash class as they use the DigestSize and would otherwise fail.
  ///   We currently mostly have test data for the SHA1 (= SHA with 128 bit length)
  ///   algorithm, but the MGF1 method is universally useable so the tests got
  ///   separated here.
  /// </summary>
  TestTHash_MGF1 = class(TTestCase)
  strict protected
    // List of test cases, defined in such a way that input, length,
    // output and hash class to be used shall be spoecified.
    FTestData : TKeyDeviationTestList;
  public
    procedure SetUp; override;
    procedure TearDown; override;

    procedure TestTBytesInternal(Algorithm : TKDFMGFAlgorithm);
    procedure TestRawMemoryInternal(Algorithm : TKDFMGFAlgorithm);
  published
    procedure TestMGF1TBytes;
    procedure TestMGF1RawMemory;
    procedure TestMGFxTBytes;
    procedure TestMGFxRawMemory;
  end;

  {$IFDEF DUnitX} [TestFixture] {$ENDIF}
  /// <summary>
  ///   All test cases for the KDF class methods. These are in this class rather
  ///   than in TestTDECHash as the KDF class methods can only be called on a
  ///   concrete hash class as they use the DigestSize and would otherwise fail.
  ///   We currently mostly have test data for the SHA and SHA256 algorithms,
  ///   but the MGF1 method is universally useable so the tests got separated here.
  /// </summary>
  TestTHash_KDF = class(TTestCase)
  strict protected
    // List of test cases, defined in such a way that input, length,
    // output and hash class to be used shall be spoecified.
    FTestData : TKeyDeviationTestList;

    procedure InternalTest(KDFType: TKDFMGFAlgorithm);
    procedure InternalTestTBytes(KDFType: TKDFMGFAlgorithm);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestKDF1;
    procedure TestKDF1TBytes;
    procedure TestKDF2;
    procedure TestKDF2TBytes;
    procedure TestKDF3;
    procedure TestKDF3TBytes;
    procedure TestKDFx;
    procedure TestKDFxTBytes;
  end;

implementation

uses
  DECFormat;

{ TKeyDeviationTestList }

procedure TKeyDeviationTestList.Add(const InputData, OutputData: RawByteString;
                                    MaskSize: Integer; HashClass: TDECHashAuthenticationClass;
                                    AlgorithmType : TKDFMGFAlgorithm = ktKDF1;
                                    Index : UInt32 = 1);
var
  Data: TKeyDeviationTestData;
begin
  Data.InputData  := InputData;
  Data.OutputData := OutputData;
  Data.MaskSize   := MaskSize;
  Data.HashClass  := HashClass;
  Data.Algorithm    := AlgorithmType;
  Data.Index      := Index;

  self.Add(Data);
end;

procedure TKeyDeviationTestList.Add(const InputData, SeedData,
  OutputData: RawByteString; MaskSize: Integer; HashClass: TDECHashAuthenticationClass;
  AlgorithmType : TKDFMGFAlgorithm = ktKDF1;
  Index : UInt32 = 1);
var
  Data: TKeyDeviationTestData;
begin
  Data.InputData  := InputData;
  Data.SeedData   := SeedData; //SysUtils.BytesOf(TFormat_HexL.Decode(SeedData));
  Data.OutputData := OutputData;
  Data.MaskSize   := MaskSize;
  Data.HashClass  := HashClass;
  Data.Algorithm    := AlgorithmType;
  Data.Index      := Index;

  self.Add(Data);
end;

{ TestTHash_KDF }

procedure TestTHash_KDF.InternalTest(KDFType: TKDFMGFAlgorithm);
var
  TestData : TKeyDeviationTestData;
  Result, ExpResult, Data, Seed : TBytes;
begin
  for TestData in FTestData do
  begin
    if (TestData.Algorithm = KDFType) then
    begin
      Data := System.SysUtils.BytesOf(TestData.InputData);
      Seed := System.SysUtils.BytesOf(TestData.SeedData);
      ExpResult := System.SysUtils.BytesOf(TestData.OutputData);

      case KDFType of
        ktKDF1 : if (length(Seed) = 0) then
                   Result := TestData.HashClass.KDF1(Data[0], length(Data),
                                                     NullStr, 0, TestData.MaskSize)
                 else
                   Result := TestData.HashClass.KDF1(Data[0], length(Data),
                                                     Seed[0], length(Seed), TestData.MaskSize);
        ktKDF2 : if (length(Seed) = 0) then
                   Result := TestData.HashClass.KDF2(Data[0], length(Data),
                                                     NullStr, 0, TestData.MaskSize)
                 else
                   Result := TestData.HashClass.KDF2(Data[0], length(Data),
                                                     Seed[0], length(Seed), TestData.MaskSize);
        ktKDF3 : if (length(Seed) = 0) then
                   Result := TestData.HashClass.KDF3(Data[0], length(Data),
                                                     NullStr, 0, TestData.MaskSize)
                 else
                   Result := TestData.HashClass.KDF3(Data[0], length(Data),
                                                     Seed[0], length(Seed), TestData.MaskSize);

        ktKDFx : if (length(Seed) = 0) then
                   Result := TestData.HashClass.KDFx(Data[0], length(Data),
                                                     NullStr, 0,
                                                     TestData.MaskSize,
                                                     TestData.Index)
                 else
                   Result := TestData.HashClass.KDFx(Data[0], length(Data),
                                                     Seed[0], length(Seed),
                                                     TestData.MaskSize,
                                                     TestData.Index);
      end;

      CheckEquals(DECUtil.BytesToRawString(ExpResult),
                  DECUtil.BytesToRawString(Result));
    end;
  end;
end;

procedure TestTHash_KDF.InternalTestTBytes(KDFType: TKDFMGFAlgorithm);
var
  TestData : TKeyDeviationTestData;
  Result, ExpResult, Data, Seed : TBytes;
begin
  for TestData in FTestData do
  begin
    if (TestData.Algorithm = KDFType) then
    begin
      Data := System.SysUtils.BytesOf(TestData.InputData);
      Seed := System.SysUtils.BytesOf(TestData.SeedData);
      ExpResult := System.SysUtils.BytesOf(TestData.OutputData);

      if (KDFType = ktKDF1) then
        Result := TestData.HashClass.KDF1(Data, Seed, TestData.MaskSize);

      if (KDFType = ktKDF2) then
        Result := TestData.HashClass.KDF2(Data, Seed, TestData.MaskSize);

      if (KDFType = ktKDF3) then
        Result := TestData.HashClass.KDF3(Data, Seed, TestData.MaskSize);

      if (KDFType = ktKDFx) then
        Result := TestData.HashClass.KDFx(Data, Seed, TestData.MaskSize, TestData.Index);

      CheckEquals(DECUtil.BytesToRawString(ExpResult),
                  DECUtil.BytesToRawString(Result));
    end;
  end;
end;

procedure TestTHash_KDF.SetUp;
begin
  inherited;

  FTestData := TKeyDeviationTestList.Create;

  FTestData.Add(TFormat_HexL.Decode('0001020304'),
                TFormat_HexL.Decode('0506070809'),
                TFormat_HexL.Decode('f52b'),
                2, THash_MD2, ktKDF1);

  // Test data from Wolfgang Erhard's library
  FTestData.Add(TFormat_HexL.Decode('deadbeeffeebdaed'),
                '',
                TFormat_HexL.Decode('b0ad565b14b478cad4763856ff3016b1' +
                                    'a93d840f87261bede7ddf0f9305a6e44'),
                32, THash_SHA1, ktKDF1);

  FTestData.Add(TFormat_HexL.Decode('deadbeeffeebdaed'),
                '',
                TFormat_HexL.Decode('87261bede7ddf0f9305a6e44a74e6a08' +
                                    '46dede27f48205c6b141888742b0ce2c'),
                32, THash_SHA1, ktKDF2);

  FTestData.Add(TFormat_HexL.Decode('deadbeeffeebdaed'),
                '',
                TFormat_HexL.Decode('60cef67059af33f6aebce1e10188f434' +
                                    'f80306ac0360470aeb41f81bafb35790'),
                32, THash_SHA1, ktKDF3);

  FTestData.Add(TFormat_HexL.Decode('032e45326fa859a72ec235acff929b15' +
                                    'd1372e30b207255f0611b8f785d76437' +
                                    '4152e0ac009e509e7ba30cd2f1778e11' +
                                    '3b64e135cf4e2292c75efe5288edfda4'),
                '',
                TFormat_HexL.Decode('10a2403db42a8743cb989de86e668d16' +
                                    '8cbe6046e23ff26f741e87949a3bba13' +
                                    '11ac179f819a3d18412e9eb45668f292' +
                                    '3c087c1299005f8d5fd42ca257bc93e8' +
                                    'fee0c5a0d2a8aa70185401fbbd99379e' +
                                    'c76c663e9a29d0b70f3fe261a59cdc24' +
                                    '875a60b4aacb1319fa11c3365a8b79a4' +
                                    '4669f26fba933d012db213d7e3b16349'),
                128, THash_SHA256, ktKDF2);

  FTestData.Add(TFormat_HexL.Decode('032e45326fa859a72ec235acff929b15' +
                                    'd1372e30b207255f0611b8f785d76437' +
                                    '4152e0ac009e509e7ba30cd2f1778e11' +
                                    '3b64e135cf4e2292c75efe5288edfda4'),
                '',
                TFormat_HexL.Decode('0e6a26eb7b956ccb8b3bdc1ca975bc57' +
                                    'c3989e8fbad31a224655d800c4695484' +
                                    '0ff32052cdf0d640562bdfadfa263cfc' +
                                    'cf3c52b29f2af4a1869959bc77f854cf' +
                                    '15bd7a25192985a842dbff8e13efee5b' +
                                    '7e7e55bbe4d389647c686a9a9ab3fb88' +
                                    '9b2d7767d3837eea4e0a2f04b53ca8f5' +
                                    '0fb31225c1be2d0126c8c7a4753b0807'),
                128, THash_SHA1, ktKDF2);

  FTestData.Add(TFormat_HexL.Decode('ca7c0f8c3ffa87a96e1b74ac8e6af594' +
                                    '347bb40a'),
                '',
                TFormat_HexL.Decode('744ab703f5bc082e59185f6d049d2d36' +
                                    '7db245c2'),
                20, THash_SHA1, ktKDF2);

  FTestData.Add(TFormat_HexL.Decode('0499b502fc8b5bafb0f4047e731d1f9f' +
                                    'd8cd0d8881'),
                '',
                TFormat_HexL.Decode('03c62280c894e103c680b13cd4b4ae74' +
                                    '0a5ef0c72547292f82dc6b1777f47d63' +
                                    'ba9d1ea732dbf386'),
                40, THash_SHA1, ktKDF2);

  // Test vector #1, ANSI X9.63
  FTestData.Add(TFormat_HexL.Decode('96c05619d56c328ab95fe84b18264b08' +
                                    '725b85e33fd34f08'),
                '',
                TFormat_HexL.Decode('443024c3dae66b95e6f5670601558f71'),
                16, THash_SHA256, ktKDF2);

  // Test vector #2, ANSI X9.63
  FTestData.Add(TFormat_HexL.Decode('96f600b73ad6ac5629577eced51743dd' +
                                    '2c24c21b1ac83ee4'),
                '',
                TFormat_HexL.Decode('b6295162a7804f5667ba9070f82fa522'),
                16, THash_SHA256, ktKDF2);

  // Test vector #3, ANSI X9.63
  FTestData.Add(TFormat_HexL.Decode('22518b10e70f2a3f243810ae3254139e' +
                                    'fbee04aa57c7af7d'),
                TFormat_HexL.Decode('75eef81aa3041e33b80971203d2c0c52'),
                TFormat_HexL.Decode('c498af77161cc59f2962b9a713e2b215' +
                                    '152d139766ce34a776df11866a69bf2e' +
                                    '52a13d9c7c6fc878c50c5ea0bc7b00e0' +
                                    'da2447cfd874f6cf92f30d0097111485' +
                                    '500c90c3af8b487872d04685d14c8d1d' +
                                    'c8d7fa08beb0ce0ababc11f0bd496269' +
                                    '142d43525a78e5bc79a17f59676a5706' +
                                    'dc54d54d4d1f0bd7e386128ec26afc21'),
                128, THash_SHA256, ktKDF2);

  // Test vector #4, ANSI X9.63
  FTestData.Add(TFormat_HexL.Decode('7e335afa4b31d772c0635c7b0e06f26f' +
                                    'cd781df947d2990a'),
                TFormat_HexL.Decode('d65a4812733f8cdbcdfb4b2f4c191d87'),
                TFormat_HexL.Decode('c0bd9e38a8f9de14c2acd35b2f3410c6' +
                                    '988cf02400543631e0d6a4c1d030365a' +
                                    'cbf398115e51aaddebdc9590664210f9' +
                                    'aa9fed770d4c57edeafa0b8c14f93300' +
                                    '865251218c262d63dadc47dfa0e02848' +
                                    '26793985137e0a544ec80abf2fdf5ab9' +
                                    '0bdaea66204012efe34971dc431d625c' +
                                    'd9a329b8217cc8fd0d9f02b13f2f6b0b'),
                128, THash_SHA256, ktKDF2);

  // Test for DEC's own KDFx variant, testdata synthesised and verified against
  // DEC V5.2
  FTestData.Add(TFormat_HexL.Decode('deadbeeffeebdaed'),
                '',
                TFormat_HexL.Decode('e473e6b6065219bab4fde9113bd80301'+
                                    '6cc49979783559585b0c8bb5bbdfa4cd'),
                32, THash_SHA1, ktKDFx, 1);

  FTestData.Add(TFormat_HexL.Decode('deadbeeffeebdaed'),
                '',
                TFormat_HexL.Decode('2dfb9508c07cd1521849d8dd92585ec0'+
                                    '9499132de0f6b8d4ed2768ec1c83f0ed'),
                32, THash_SHA1, ktKDFx, 2);

  FTestData.Add(TFormat_HexL.Decode('7e335afa4b31d772c0635c7b0e06f26f' +
                                    'cd781df947d2990a'),
                TFormat_HexL.Decode('d65a4812733f8cdbcdfb4b2f4c191d87'),
                TFormat_HexL.Decode('934006d019879d1ee2787b27ed57841b' +
                                    '66433425d6a0f4ca6abb20f6967dc660' +
                                    'd04f8577b13ec8d4ec54610a78e0881f' +
                                    '2ae26f482c81053c6d8951e787b2e4a9' +
                                    '9b3c2a95bc196948e1f1819c55ba08d6' +
                                    '6a6ca395e9929eaee752b5dc324e980d' +
                                    '71f0e8c1b244bb3b0c09b903ebc446e2' +
                                    '925bf1a7041923a3910959e5dcd6afd2'),
                128, THash_SHA256, ktKDFx, 1);

  FTestData.Add(TFormat_HexL.Decode('7e335afa4b31d772c0635c7b0e06f26f' +
                                    'cd781df947d2990a'),
                TFormat_HexL.Decode('d65a4812733f8cdbcdfb4b2f4c191d87'),
                TFormat_HexL.Decode('2c26a1303d2146d61384e689debf541f' +
                                    'bcd832e9343f2771b7e28dd877f096d5' +
                                    '95e0e12c768d74d72a91176908b34842' +
                                    'a0a32d7b3f8293d169fa873a5a7e747e' +
                                    '0e6e054e73c2ca1bb89a1772e4e5b0f6' +
                                    '7cead338829bb02e7012254f4f79e63a' +
                                    '619e6f14782af946e169c2870ec6ca3a' +
                                    '68377d2ad42196987278f93674c7d861'),
                128, THash_SHA256, ktKDFx, 2);
end;

procedure TestTHash_KDF.TearDown;
begin
  FTestData.Free;

  inherited;
end;

procedure TestTHash_KDF.TestKDF1;
begin
  InternalTest(ktKDF1);
end;

procedure TestTHash_KDF.TestKDF1TBytes;
begin
  InternalTestTBytes(ktKDF1);
end;

procedure TestTHash_KDF.TestKDF2;
begin
  InternalTest(ktKDF2);
end;

procedure TestTHash_KDF.TestKDF2TBytes;
begin
  InternalTestTBytes(ktKDF2);
end;

procedure TestTHash_KDF.TestKDF3;
begin
  InternalTest(ktKDF3);
end;

procedure TestTHash_KDF.TestKDF3TBytes;
begin
  InternalTestTBytes(ktKDF3);
end;

procedure TestTHash_KDF.TestKDFx;
begin
  InternalTest(ktKDFx);
end;

procedure TestTHash_KDF.TestKDFxTBytes;
begin
  InternalTestTBytes(ktKDFx);
end;

{ THash_TestMGF1 }

procedure TestTHash_MGF1.SetUp;
begin
  inherited;

  FTestData := TKeyDeviationTestList.Create;

  FTestData.Add('foo', '1ac907', 3, THash_SHA1, ktMGF1);
  FTestData.Add('foo', '1ac9075cd4', 5, THash_SHA1, ktMGF1);
  FTestData.Add('bar', 'bc0c655e01', 5, THash_SHA1, ktMGF1);
  FTestData.Add('bar', 'bc0c655e016bc2931d85a2e675181adcef7f581f76df2739da74f' +
                       'aac41627be2f7f415c89e983fd0ce80ced9878641cb4876', 50, THash_SHA1, ktMGF1);
  FTestData.Add('bar', '382576a7841021cc28fc4c0948753fb8312090cea942ea4c4e735' +
                       'd10dc724b155f9f6069f289d61daca0cb814502ef04eae1', 50, THash_SHA256, ktMGF1);

  FTestData.Add('foo', 'b2ae0e', 3, THash_SHA1, ktMGFx, 1);
  FTestData.Add('foo', '44ffe3', 3, THash_SHA1, ktMGFx, 2);
  FTestData.Add('foo', 'b2ae0e6e26', 5, THash_SHA1, ktMGFx, 1);
  FTestData.Add('foo', '44ffe3a40b', 5, THash_SHA1, ktMGFx, 2);
  FTestData.Add('bar', '21dfca12e1', 5, THash_SHA1, ktMGFx, 1);
  FTestData.Add('bar', '596eb7fe1b', 5, THash_SHA1, ktMGFx, 2);
  FTestData.Add('bar', '21dfca12e13ebcaa77a5b6c0ff7023266e0dd54498306070fc735' +
                       'ad7b0d88e14cb1acc887f564c7adb677a1b6b4b1e6b3f07', 50, THash_SHA1, ktMGFx, 1);
  FTestData.Add('bar', '596eb7fe1ba0a133e7c445c745270bf6433dc88cef9f922840f75' +
                       'd4867a247ea37fc9458c934e4675bcd7300722ee8d07b37', 50, THash_SHA1, ktMGFx, 2);
  FTestData.Add('bar', 'e01f7238400a2f1501bbaaf937d3d081bda4fe5bccc713134f5d3' +
                       'a580bd66783d379e6d91c6e91a1072744ae42fe4e182f32', 50, THash_SHA256, ktMGFx, 1);
  FTestData.Add('bar', '816453f0040d4f0469ccfcb520bf63fbf4616e46adff6708d40b1' +
                       '63041b91a234baa7f8a1fb86b23ef81df6e1121233dd88e', 50, THash_SHA256, ktMGFx, 2);
end;

procedure TestTHash_MGF1.TearDown;
begin
  FTestData.Free;

  inherited;
end;

procedure TestTHash_MGF1.TestMGF1RawMemory;
begin
  TestRawMemoryInternal(ktMGF1)
end;

procedure TestTHash_MGF1.TestMGF1TBytes;
begin
  TestTBytesInternal(ktMGF1);
end;

procedure TestTHash_MGF1.TestMGFxRawMemory;
begin
  TestRawMemoryInternal(ktMGFx)
end;

procedure TestTHash_MGF1.TestMGFxTBytes;
begin
  TestTBytesInternal(ktMGFx);
end;

procedure TestTHash_MGF1.TestRawMemoryInternal(Algorithm : TKDFMGFAlgorithm);
var
  InputData  : TBytes;
  OutputData : TBytes;
  i          : Integer;
begin
  for i := 0 to FTestData.Count - 1 do
  begin
    InputData  := BytesOf(FTestData[i].InputData);

    // Skip tests which are not for the selected algorithm
    if FTestData[i].Algorithm <> Algorithm then
      Continue;

    case FTestData[i].Algorithm of
      ktMGF1 : OutputData := FTestData[i].HashClass.MGF1(InputData[0], length(InputData),
                                              FTestData[i].MaskSize);
      ktMGFx : OutputData := FTestData[i].HashClass.MGFx(InputData[0], length(InputData),
                                              FTestData[i].MaskSize, FTestData[i].Index);
    end;

    CheckEquals(FTestData[i].OutputData,
                DECUtil.BytesToRawString(TFormat_HEXL.Encode(OutputData)),
                'MGFT1/MGFx test failed at index ' + IntToStr(i));
  end;
end;

procedure TestTHash_MGF1.TestTBytesInternal(Algorithm : TKDFMGFAlgorithm);
var
  InputData  : TBytes;
  OutputData : TBytes;
  i          : Integer;
begin
  for i := 0 to FTestData.Count - 1 do
  begin
    InputData  := BytesOf(FTestData[i].InputData);

    // Skip tests which are not for the selected algorithm
    if FTestData[i].Algorithm <> Algorithm then
      Continue;

    case FTestData[i].Algorithm of
      ktMGF1 : OutputData := FTestData[i].HashClass.MGF1(InputData, FTestData[i].MaskSize);
      ktMGFx : OutputData := FTestData[i].HashClass.MGFx(InputData, FTestData[i].MaskSize,
                                                         FTestData[i].Index);
    end;

    CheckEquals(FTestData[i].OutputData,
                DECUtil.BytesToRawString(TFormat_HEXL.Encode(OutputData)),
                'MGF1/MGFx test failed at index ' + IntToStr(i));
  end;
end;

initialization
  // Register any test cases with the test runner
  {$IFDEF DUnitX}
  TDUnitX.RegisterTestFixture(TestTHash_MGF1);
  TDUnitX.RegisterTestFixture(TestTHash_KDF);
  {$ELSE}
  RegisterTests('DECHashKDF', [TestTHash_MGF1.Suite,
                               TestTHash_KDF.Suite]);
  {$ENDIF}
end.
