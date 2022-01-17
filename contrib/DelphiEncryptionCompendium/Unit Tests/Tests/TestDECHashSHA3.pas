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
unit TestDECHashSHA3;

interface

// Needs to be included before any other statements
{$INCLUDE TestDefines.inc}
{$INCLUDE ..\..\Source\DECOptions.inc}

{$IF CompilerVersion >= 24.0} // Too many local constants for Delphi XE2 and older
uses
  System.SysUtils, System.Classes, Generics.Collections,
  {$IFDEF DUnitX}
  DUnitX.TestFramework,DUnitX.DUnitCompatibility,
  {$ELSE}
  TestFramework,
  {$ENDIF}
  TestDECTestDataContainer,
  DECTypes, DECBaseClass, DECHash, DECHashBase, DECHashAuthentication, DECUtil,
  DECFormatBase, DECHashBitBase, TestDECHash;

type
  /// <summary>
  ///   Base class for the SHA3 tests, provides loading the test data from files
  /// </summary>
  TestTHash_SHA3_Base = class(THash_TestBase)
  strict private
    /// <summary>
    ///   Load the data of all test files specified for the test class
    /// </summary>
    procedure LoadTestFiles; inline;
    /// <summary>
    ///   Calculate the Unicode hash value of the given test data
    /// </summary>
    /// <param name="TestData">
    ///   Test data as lower case Hex-Value
    /// </param>
    /// <param name="HashInst">
    ///   Instance of the hash algorithm used
    /// </param>
    /// <returns>
    ///   Calculated lower case hex formatted hash over the test data
    /// </returns>
    function CalcUnicodeHash(TestData : string;
                             HashInst : TDECHashAuthentication): RawByteString; inline;
  strict protected
    /// <summary>
    ///   List of test data files to laod
    /// </summary>
    FTestFileNames : TStringList;

    /// <summary>
    ///   Overridden so that loading of the test data file only happens here
    ///   and not also for the metadata etc. tests as well
    /// </summary>
    procedure DoTestCalcBuffer(HashClass:TDECHash); override;
    /// <summary>
    ///   Overridden so that loading of the test data file only happens here
    ///   and not also for the metadata etc. tests as well
    /// </summary>
    procedure DoTestCalcBytes(HashClass:TDECHash); override;
    /// <summary>
    ///   Overridden so that loading of the test data file only happens here
    ///   and not also for the metadata etc. tests as well
    /// </summary>
    procedure DoTestCalcStream(HashClass:TDECHash); override;
    /// <summary>
    ///   Overridden so that loading of the test data file only happens here
    ///   and not also for the metadata etc. tests as well
    /// </summary>
    procedure DoTestCalcUnicodeString(HashClass:TDECHash); override;
    /// <summary>
    ///   Overridden so that loading of the test data file only happens here
    ///   and not also for the metadata etc. tests as well
    /// </summary>
    procedure DoTestCalcRawByteString(HashClass:TDECHash); override;

    /// <summary>
    ///   Loads data stored in the rsp files provided by NIST into the test
    ///   data lists. Any exceptions will be caught and entries with wrong format
    ///   ignored.
    /// </summary>
    /// <param name="FileName">
    ///   Namne of the file to load
    /// </param>
    /// <param name="TestData">
    ///   Interface to the test data management instance
    /// </param>
    /// <param name="HashInst">
    ///   Instance of the hashing class to be tested for calculating the UTF-string
    ///   test data.
    /// </param>
    procedure LoadTestDataFile(FileName : string;
                               TestData : IHashTestDataContainer;
                               HashInst : TDECHashAuthentication);
  public
    /// <summary>
    ///   Create test file list
    /// </summary>
    procedure SetUp; override;
    /// <summary>
    ///   Free test file lsit
    /// </summary>
    procedure TearDown; override;
  end;

  // Test methods for class THash_SHA3_224
  {$IFDEF DUnitX} [TestFixture] {$ENDIF}
  TestTHash_SHA3_224 = class(TestTHash_SHA3_Base)
  strict protected
    /// <summary>
    ///   Some tests need to set the SHA3 specific padding byte and final bit length
    ///   parameters
    /// </summary>
    procedure ConfigHashClass(HashClass: TDECHash; IdxTestData:Integer); override;
  public
    procedure SetUp; override;
  published
    procedure TestDigestSize;
    procedure TestBlockSize;
    procedure TestIsPasswordHash;
    procedure TestClassByName;
    procedure TestIdentity;
  end;

  // Test methods for class THash_SHA3_256
  {$IFDEF DUnitX} [TestFixture] {$ENDIF}
  TestTHash_SHA3_256 = class(TestTHash_SHA3_Base)
  strict protected
    /// <summary>
    ///   Some tests need to set the SHA3 specific padding byte and final bit length
    ///   parameters
    /// </summary>
    procedure ConfigHashClass(HashClass: TDECHash; IdxTestData:Integer); override;
  public
    procedure SetUp; override;
  published
    procedure TestDigestSize;
    procedure TestBlockSize;
    procedure TestIsPasswordHash;
    procedure TestClassByName;
    procedure TestIdentity;
  end;

  // Test methods for class THash_SHA3_384
  {$IFDEF DUnitX} [TestFixture] {$ENDIF}
  TestTHash_SHA3_384 = class(TestTHash_SHA3_Base)
  strict protected
    /// <summary>
    ///   Some tests need to set the SHA3 specific padding byte and final bit length
    ///   parameters
    /// </summary>
    procedure ConfigHashClass(HashClass: TDECHash; IdxTestData:Integer); override;
  public
    procedure SetUp; override;
  published
    procedure TestDigestSize;
    procedure TestBlockSize;
    procedure TestIsPasswordHash;
    procedure TestClassByName;
    procedure TestIdentity;
  end;

  // Test methods for class THash_SHA3_512
  {$IFDEF DUnitX} [TestFixture] {$ENDIF}
  TestTHash_SHA3_512 = class(TestTHash_SHA3_Base)
  strict protected
    /// <summary>
    ///   Some tests need to set the SHA3 specific padding byte and final bit length
    ///   parameters
    /// </summary>
    procedure ConfigHashClass(HashClass: TDECHash; IdxTestData:Integer); override;
  public
    procedure SetUp; override;
  published
    procedure TestDigestSize;
    procedure TestBlockSize;
    procedure TestIsPasswordHash;
    procedure TestClassByName;
    procedure TestIdentity;
  end;

implementation

uses
  DECFormat;

{ TestTHash_SHA3_Base }

procedure TestTHash_SHA3_Base.SetUp;
begin
  inherited;

  FTestFileNames := TStringList.Create;
end;

procedure TestTHash_SHA3_Base.TearDown;
begin
  inherited;

  FTestFileNames.Free;
end;

procedure TestTHash_SHA3_Base.LoadTestFiles;
var
  FileName : string;
begin
  for FIleName in FTestFIleNames do
    LoadTestDataFile(FileName, FTestData, FHash);
end;

procedure TestTHash_SHA3_Base.DoTestCalcBuffer(HashClass:TDECHash);
begin
  LoadTestFiles;
  inherited;
end;

procedure TestTHash_SHA3_Base.DoTestCalcBytes(HashClass:TDECHash);
begin
  LoadTestFiles;
  inherited;
end;

procedure TestTHash_SHA3_Base.DoTestCalcStream(HashClass:TDECHash);
begin
  LoadTestFiles;
  inherited;
end;

procedure TestTHash_SHA3_Base.DoTestCalcUnicodeString(HashClass:TDECHash);
begin
  LoadTestFiles;
  inherited;
end;

procedure TestTHash_SHA3_Base.DoTestCalcRawByteString(HashClass:TDECHash);
begin
  LoadTestFiles;
  inherited;
end;

procedure TestTHash_SHA3_Base.LoadTestDataFile(FileName : string;
                                               TestData : IHashTestDataContainer;
                                               HashInst : TDECHashAuthentication);
var
  Contents     : TStringList;
  FileRow,
  FileRowTrim,
  s1           : string;
  Len          : Int32;
  FinalByteLen : Int16;
  lDataRow     : IHashTestDataRowSetup;
begin
  Len      := 0;
  Contents := TStringList.Create;
  try
    Contents.LoadFromFile(FileName);

    for FileRow in Contents do
    begin
      FileRowTrim := LowerCase(Trim(FileRow));

      if (Pos('len', FileRowTrim) = 1) then
      begin
        lDataRow := FTestData.AddRow;

        s1  := FileRowTrim;
        Delete(s1, 1, 6);
        Len := StrToInt(s1);
        FinalByteLen := Len mod 8;
        lDataRow.FinalBitLength := FinalByteLen;
        THash_SHA3Base(HashInst).FinalByteLength := FinalByteLen;

        Continue;
      end;

      if (Pos('msg', FileRowTrim) = 1) then
      begin
        s1 := FileRowTrim;
        Delete(s1, 1, 6);

        if (Len > 0) then
        begin
          lDataRow.AddInputVector(TFormat_HexL.Decode(RawByteString(s1)));
          lDataRow.ExpectedOutputUTFStrTest := CalcUnicodeHash(s1, HashInst);
        end
        else
        begin
          lDataRow.AddInputVector('');
          lDataRow.ExpectedOutputUTFStrTest := CalcUnicodeHash('', HashInst);
        end;

        Continue;
      end;

      if (Pos('md', FileRowTrim) = 1) then
      begin
        s1 := FileRowTrim;
        Delete(s1, 1, 5);
        lDataRow.ExpectedOutput := RawByteString(s1);

        Continue;
      end;
    end;
  finally
    Contents.Free;
  end;
end;

function TestTHash_SHA3_Base.CalcUnicodeHash(TestData : string;
                                             HashInst : TDECHashAuthentication): RawByteString;
begin
  Result := BytesToRawString(TFormat_HEXL.Encode(
               System.SysUtils.BytesOf(HashInst.CalcString(
                 string(TFormat_HexL.Decode(RawByteString(TestData)))))));
end;

{ TestTHash_SHA3_224 }

procedure TestTHash_SHA3_224.ConfigHashClass(HashClass: TDECHash;
  IdxTestData: Integer);
begin
  inherited;

  THash_SHA3_224(FHash).FinalByteLength := FTestData[IdxTestData].FinalByteLength;
  THash_SHA3_224(FHash).PaddingByte     := FTestData[IdxTestData].PaddingByte;
end;

procedure TestTHash_SHA3_224.SetUp;
var
  lDataRow : IHashTestDataRowSetup;
  i        : Integer;
  s        : RawByteString;
begin
  // All specified data sources are for the non unicode expected outputs
  inherited;
  FHash := THash_SHA3_224.Create;

  //Source https://csrc.nist.gov/CSRC/media/Projects/Cryptographic-Algorithm-
  //       Validation-Program/documents/sha3/sha-3bittestvectors.zip
  FTestFileNames.Add('..\..\Unit Tests\Data\SHA3_224ShortMsg.rsp');
  FTestFileNames.Add('..\..\Unit Tests\Data\SHA3_224LongMsg.rsp');
  // SourceEnd

  // Source: https://csrc.nist.gov/CSRC/media/Projects/Cryptographic-Standards-
  //         and-Guidelines/documents/examples/SHA3-224_1600.pdf
  lDataRow := FTestData.AddRow;
  lDataRow.ExpectedOutput           := '9376816aba503f72f96ce7eb65ac095deee3be4b' +
                                       'f9bbc2a1cb7e11e0';
  lDataRow.ExpectedOutputUTFStrTest := '28a4a80fded04a676674687c8330422eedeb18c9' +
                                       'dba976234a9e007a';
  lDataRow.AddInputVector(RawByteString(#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3), 1, 20);
  lDataRow.FinalBitLength := 0;

  // Source: https://emn178.github.io/online-tools/sha3_224.html
  lDataRow := FTestData.AddRow;
  lDataRow.ExpectedOutput           := '32eb6a4121daebe223db1987740814e1dd9d9ddb' +
                                       'ddfd466feff5c9b4';
  lDataRow.ExpectedOutputUTFStrTest := '0f1ad8cd5a85fe68319b67427e1f0b685498bc24' +
                                       '6a81a1f595c89e4e';
  lDataRow.AddInputVector(RawByteString('e21et2e2et1208e7t12e07812te08127et1028e' +
                                        '7t1208e7gd81d872t178r02tr370823'), 1, 10);
  lDataRow.FinalBitLength := 0;

  lDataRow := FTestData.AddRow;
  lDataRow.ExpectedOutput           := 'f7fc914c8fe4827d866b02df2459840260f4adb0' +
                                       'db4deb9fa661756c';
  lDataRow.ExpectedOutputUTFStrTest := 'e4d44bbda0b8fc8a73b421f6795c6380c0e21d50' +
                                       '539a7b43c20a7529';

   for i := 1 to 10 do
     s := s + 'e21et2e2et1208e7t12e07812te08127et1028e7t1208e7gd81d872t178r02tr370823';
   s := s + 'TurboMagic';
   s := s + s + s;

  lDataRow.AddInputVector(s);
  lDataRow.FinalBitLength := 0;

  // Source https://csrc.nist.gov/CSRC/media/Projects/Cryptographic-Standards-
  //        and-Guidelines/documents/examples/SHA3-224_Msg5.pdf
  lDataRow := FTestData.AddRow;
  lDataRow.ExpectedOutput           := 'ffbad5da96bad71789330206dc6768ecaeb1b32d' +
                                       'ca6b3301489674ab';
  lDataRow.ExpectedOutputUTFStrTest := '3d0e88c1e4fe0f6577e921e50805155b0748b40a' +
                                       '3ab368c96b63f686';
  lDataRow.AddInputVector(#$13);
  lDataRow.FinalBitLength := 5;

  // Source https://csrc.nist.gov/CSRC/media/Projects/Cryptographic-Standards-
  //        and-Guidelines/documents/examples/SHA3-224_Msg30.pdf
  lDataRow := FTestData.AddRow;
  lDataRow.ExpectedOutput           := 'd666a514cc9dba25ac1ba69ed3930460deaac985' +
                                       '1b5f0baab007df3b';
  lDataRow.ExpectedOutputUTFStrTest := '098526f4e121e977c325078374bf13ee9b0f2ed3' +
                                       '14ce743c5641cebe';
  lDataRow.AddInputVector(#$53#$58#$7B#$19);
  lDataRow.FinalBitLength := 6;

  // Source https://csrc.nist.gov/CSRC/media/Projects/Cryptographic-Standards-
  //        and-Guidelines/documents/examples/SHA3-224_Msg1605.pdf
  lDataRow := FTestData.AddRow;
  lDataRow.ExpectedOutput           := '22d2f7bb0b173fd8c19686f9173166e3ee627380' +
                                       '47d7eadd69efb228';
  lDataRow.ExpectedOutputUTFStrTest := 'a6871aef1c16c4c0fcaef97636711fb6216b1586' +
                                       '26d4e2b7e9e7e962';
  lDataRow.AddInputVector(RawByteString(#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3));
  lDataRow.FinalBitLength := 5;

  // Source https://csrc.nist.gov/CSRC/media/Projects/Cryptographic-Standards-
  //        and-Guidelines/documents/examples/SHA3-224_1630.pdf
  lDataRow := FTestData.AddRow;
  lDataRow.ExpectedOutput           := '4e907bb1057861f200a599e9d4f85b02d88453bf' +
                                       '5b8ace9ac589134c';
  lDataRow.ExpectedOutputUTFStrTest := '30a15a07d7f0a34e5b36de3bec18c31eac2c9495' +
                                       '2b50820095c8807e';
  lDataRow.AddInputVector(RawByteString(#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3));
  lDataRow.FinalBitLength := 6;
end;

procedure TestTHash_SHA3_224.TestBlockSize;
begin
  CheckEquals(144, FHash.BlockSize);
end;

procedure TestTHash_SHA3_224.TestClassByName;
begin
  DoTestClassByName('THash_SHA3_224', THash_SHA3_224);
end;

procedure TestTHash_SHA3_224.TestDigestSize;
begin
  CheckEquals(28, FHash.DigestSize);
end;

procedure TestTHash_SHA3_224.TestIdentity;
begin
  CheckEquals($D0579DA9, FHash.Identity);
end;

procedure TestTHash_SHA3_224.TestIsPasswordHash;
begin
  CheckNotEquals(true, FHash.IsPasswordHash);
end;

{ TestTHash_SHA3_256 }

procedure TestTHash_SHA3_256.ConfigHashClass(HashClass: TDECHash;
  IdxTestData: Integer);
begin
  inherited;

  THash_SHA3_256(FHash).FinalByteLength := FTestData[IdxTestData].FinalByteLength;
  THash_SHA3_256(FHash).PaddingByte     := FTestData[IdxTestData].PaddingByte;
end;

procedure TestTHash_SHA3_256.SetUp;
var
  lDataRow:IHashTestDataRowSetup;
begin
  // All specified data sources are for the non unicode expected outputs
  inherited;
  FHash := THash_SHA3_256.Create;

  //Source https://csrc.nist.gov/CSRC/media/Projects/Cryptographic-Algorithm-
  //       Validation-Program/documents/sha3/sha-3bittestvectors.zip
  FTestFileNames.Add('..\..\Unit Tests\Data\SHA3_256ShortMsg.rsp');
  FTestFileNames.Add('..\..\Unit Tests\Data\SHA3_256LongMsg.rsp');
  // SourceEnd

  // Source: https://csrc.nist.gov/CSRC/media/Projects/Cryptographic-Standards-
  //         and-Guidelines/documents/examples/SHA3-256_Msg0.pdf
  lDataRow := FTestData.AddRow;
  lDataRow.ExpectedOutput           := 'a7ffc6f8bf1ed76651c14756a061d662f580ff4d' +
                                       'e43b49fa82d80a4b80f8434a';
  lDataRow.ExpectedOutputUTFStrTest := 'a7ffc6f8bf1ed76651c14756a061d662f580ff4d' +
                                       'e43b49fa82d80a4b80f8434a';
  lDataRow.AddInputVector('');

  // Source: https://csrc.nist.gov/CSRC/media/Projects/Cryptographic-Standards-
  //         and-Guidelines/documents/examples/SHA3-224_1600.pdf
  lDataRow := FTestData.AddRow;
  lDataRow.ExpectedOutput           := '79f38adec5c20307a98ef76e8324afbfd46cfd81' +
                                       'b22e3973c65fa1bd9de31787';
  lDataRow.ExpectedOutputUTFStrTest := '06ea5e186dab1b3f99bcf91918b53748367674c0' +
                                       '5baa627010fba06edb67f0ba';
  lDataRow.AddInputVector(RawByteString(
                          #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3), 1, 20);
  lDataRow.FinalBitLength := 0;

  // Source https://csrc.nist.gov/CSRC/media/Projects/Cryptographic-Standards-
  //        and-Guidelines/documents/examples/SHA3-256_Msg5.pdf
  lDataRow := FTestData.AddRow;
  lDataRow.ExpectedOutput           := '7b0047cf5a456882363cbf0fb05322cf65f4b705' +
                                       '9a46365e830132e3b5d957af';
  lDataRow.ExpectedOutputUTFStrTest := 'f2771aed86eba40f0f20c80aed2efb2ccdcd2a45' +
                                       '14a89642353dc4ab6f31a0a1';
  lDataRow.AddInputVector(#$13);
  lDataRow.FinalBitLength := 5;


  // Source https://csrc.nist.gov/CSRC/media/Projects/Cryptographic-Standards-
  //        and-Guidelines/documents/examples/SHA3-256_Msg30.pdf
  lDataRow := FTestData.AddRow;
  lDataRow.ExpectedOutput           := 'c8242fef409e5ae9d1f1c857ae4dc624b92b1980' +
                                       '9f62aa8c07411c54a078b1d0';
  lDataRow.ExpectedOutputUTFStrTest := '0264f8f24160a1c2336453338772637b64864ce1' +
                                       '3c3c4207c40b34d28d68cd23';
  lDataRow.AddInputVector(#$53#$58#$7B#$19);
  lDataRow.FinalBitLength := 6;

  // Source https://csrc.nist.gov/CSRC/media/Projects/Cryptographic-Standards-
  //        and-Guidelines/documents/examples/SHA3-256_Msg1605.pdf
  lDataRow := FTestData.AddRow;
  lDataRow.ExpectedOutput           := '81ee769bed0950862b1ddded2e84aaa6ab7bfdd3' +
                                       'ceaa471be31163d40336363c';
  lDataRow.ExpectedOutputUTFStrTest := '7a4185574238ca2e2550a9fa85a0c5a327811698' +
                                       'c3a05531a70e0a7ec369e2e5';
  lDataRow.AddInputVector(RawByteString(#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3));
  lDataRow.FinalBitLength := 5;

  // Source https://csrc.nist.gov/CSRC/media/Projects/Cryptographic-Standards-
  //        and-Guidelines/documents/examples/SHA3-256_1630.pdf
  lDataRow := FTestData.AddRow;
  lDataRow.ExpectedOutput           := '52860aa301214c610d922a6b6cab981ccd06012e' +
                                       '54ef689d744021e738b9ed20';
  lDataRow.ExpectedOutputUTFStrTest := '8f0bb49b3327e5a03dd69bded05a86c9e7d72a7d' +
                                       '719dd354a873cf2a70c30354';
  lDataRow.AddInputVector(RawByteString(#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3));
  lDataRow.FinalBitLength := 6;
end;

procedure TestTHash_SHA3_256.TestBlockSize;
begin
  CheckEquals(136, FHash.BlockSize);
end;

procedure TestTHash_SHA3_256.TestClassByName;
begin
  DoTestClassByName('THash_SHA3_256', THash_SHA3_256);
end;

procedure TestTHash_SHA3_256.TestDigestSize;
begin
  CheckEquals(32, FHash.DigestSize);
end;

procedure TestTHash_SHA3_256.TestIdentity;
begin
  CheckEquals($71186A42, FHash.Identity);
end;

procedure TestTHash_SHA3_256.TestIsPasswordHash;
begin
  CheckNotEquals(true, FHash.IsPasswordHash);
end;

{ TestTHash_SHA3_384 }

procedure TestTHash_SHA3_384.ConfigHashClass(HashClass: TDECHash;
  IdxTestData: Integer);
begin
  inherited;

  THash_SHA3_384(FHash).FinalByteLength := FTestData[IdxTestData].FinalByteLength;
  THash_SHA3_384(FHash).PaddingByte     := FTestData[IdxTestData].PaddingByte;
end;

procedure TestTHash_SHA3_384.SetUp;
var
  lDataRow:IHashTestDataRowSetup;
begin
  inherited;
  FHash := THash_SHA3_384.Create;

  //Source https://csrc.nist.gov/CSRC/media/Projects/Cryptographic-Algorithm-
  //       Validation-Program/documents/sha3/sha-3bittestvectors.zip
  FTestFileNames.Add('..\..\Unit Tests\Data\SHA3_384ShortMsg.rsp');
  FTestFileNames.Add('..\..\Unit Tests\Data\SHA3_384LongMsg.rsp');
  // SourceEnd

  // Source: https://csrc.nist.gov/CSRC/media/Projects/Cryptographic-Standards-
  //         and-Guidelines/documents/examples/SHA3-384_Msg0.pdf
  lDataRow := FTestData.AddRow;
  lDataRow.ExpectedOutput           := '0c63a75b845e4f7d01107d852e4c2485c51a50aa' +
                                       'aa94fc61995e71bbee983a2ac3713831264adb47' +
                                       'fb6bd1e058d5f004';
  lDataRow.ExpectedOutputUTFStrTest := '0c63a75b845e4f7d01107d852e4c2485c51a50aa' +
                                       'aa94fc61995e71bbee983a2ac3713831264adb47' +
                                       'fb6bd1e058d5f004';
  lDataRow.AddInputVector('');

  // Source: https://csrc.nist.gov/CSRC/media/Projects/Cryptographic-Standards-
  //         and-Guidelines/documents/examples/SHA3-384_1600.pdf
  lDataRow := FTestData.AddRow;
  lDataRow.ExpectedOutput           := '1881de2ca7e41ef95dc4732b8f5f002b189cc1e4' +
                                       '2b74168ed1732649ce1dbcdd76197a31fd55ee98' +
                                       '9f2d7050dd473e8f';
  lDataRow.ExpectedOutputUTFStrTest := '50dd07a64dc6ff190db60e612d8511742baa8eb5' +
                                       'a499a0a02e51cea3f4b922f8ecf72785dc0a2ef3' +
                                       '8230340378d3d104';
  lDataRow.AddInputVector(RawByteString(
                          #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3), 1, 20);
  lDataRow.FinalBitLength := 0;

  // Source https://csrc.nist.gov/CSRC/media/Projects/Cryptographic-Standards-
  //        and-Guidelines/documents/examples/SHA3-384_Msg5.pdf
  lDataRow := FTestData.AddRow;
  lDataRow.ExpectedOutput           := '737c9b491885e9bf7428e792741a7bf8dca96534' +
                                       '71c3e148473f2c236b6a0a6455eb1dce9f779b4b' +
                                       '6b237fef171b1c64';
  lDataRow.ExpectedOutputUTFStrTest := 'c2ab721500b00f70be1f5adfcbe70cdf22581c35' +
                                       'ed47d265538c1cbd939f1fe8290e58d096eb3378' +
                                       'f3a75818d623ebb8';
  lDataRow.AddInputVector(#$13);
  lDataRow.FinalBitLength := 5;

  // Source https://csrc.nist.gov/CSRC/media/Projects/Cryptographic-Standards-
  //        and-Guidelines/documents/examples/SHA3-384_Msg30.pdf
  lDataRow := FTestData.AddRow;
  lDataRow.ExpectedOutput           := '955b4dd1be03261bd76f807a7efd432435c41736' +
                                       '2811b8a50c564e7ee9585e1ac7626dde2fdc030f' +
                                       '876196ea267f08c3';
  lDataRow.ExpectedOutputUTFStrTest := '20243fac26472f874f31539f8b26c1eb226f0c02' +
                                       '3f7f3affa688a471a1943c6fb00dbcbb928b5234' +
                                       'e19423670f34c328';
  lDataRow.AddInputVector(#$53#$58#$7B#$19);
  lDataRow.FinalBitLength := 6;

  // Source https://csrc.nist.gov/CSRC/media/Projects/Cryptographic-Standards-
  //        and-Guidelines/documents/examples/SHA3-384_Msg1605.pdf
  lDataRow := FTestData.AddRow;
  lDataRow.ExpectedOutput           := 'a31fdbd8d576551c21fb1191b54bda65b6c5fe97' +
                                       'f0f4a69103424b43f7fdb835979fdbeae8b3fe16' +
                                       'cb82e587381eb624';
  lDataRow.ExpectedOutputUTFStrTest := 'f3c3516f2b9d8a07233fc3dd427f8aba27d8e2e5' +
                                       '793a6054f5bcdf005ce6de90d8dc6c69a833f5a7' +
                                       '9da35504fc3ea8bc';
  lDataRow.AddInputVector(RawByteString(#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3));
  lDataRow.FinalBitLength := 5;

  // Source https://csrc.nist.gov/CSRC/media/Projects/Cryptographic-Standards-
  //        and-Guidelines/documents/examples/SHA3-384_1630.pdf
  lDataRow := FTestData.AddRow;
  lDataRow.ExpectedOutput           := '3485d3b280bd384cf4a777844e94678173055d1c' +
                                       'bc40c7c2c3833d9ef12345172d6fcd31923bb879' +
                                       '5ac81847d3d8855c';
  lDataRow.ExpectedOutputUTFStrTest := '63f5d9652bc5a4619ea4b3baf70a43e9d43d0620' +
                                       '20109c162265f84c95eeb5768a4ce055dbe64385' +
                                       '324650c2cd9091d5';
  lDataRow.AddInputVector(RawByteString(#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3));
  lDataRow.FinalBitLength := 6;
end;

procedure TestTHash_SHA3_384.TestBlockSize;
begin
  CheckEquals(104, FHash.BlockSize);
end;

procedure TestTHash_SHA3_384.TestClassByName;
begin
  DoTestClassByName('THash_SHA3_384', THash_SHA3_384);
end;

procedure TestTHash_SHA3_384.TestDigestSize;
begin
  CheckEquals(48, FHash.DigestSize);
end;

procedure TestTHash_SHA3_384.TestIdentity;
begin
  CheckEquals($2B7A1F14, FHash.Identity);
end;

procedure TestTHash_SHA3_384.TestIsPasswordHash;
begin
  CheckNotEquals(true, FHash.IsPasswordHash);
end;

{ TestTHash_SHA3_512 }

procedure TestTHash_SHA3_512.ConfigHashClass(HashClass: TDECHash;
  IdxTestData: Integer);
begin
  inherited;

  THash_SHA3_512(FHash).FinalByteLength := FTestData[IdxTestData].FinalByteLength;
  THash_SHA3_512(FHash).PaddingByte     := FTestData[IdxTestData].PaddingByte;
end;

procedure TestTHash_SHA3_512.SetUp;
var
  lDataRow : IHashTestDataRowSetup;
begin
  inherited;
  FHash := THash_SHA3_512.Create;

  //Source https://csrc.nist.gov/CSRC/media/Projects/Cryptographic-Algorithm-
  //       Validation-Program/documents/sha3/sha-3bittestvectors.zip
  FTestFileNames.Add('..\..\Unit Tests\Data\SHA3_512ShortMsg.rsp');
  FTestFileNames.Add('..\..\Unit Tests\Data\SHA3_512LongMsg.rsp');
  // SourceEnd

  // Source: https://csrc.nist.gov/CSRC/media/Projects/Cryptographic-Standards-
  //         and-Guidelines/documents/examples/SHA3-512_1600.pdf
  lDataRow := FTestData.AddRow;
  lDataRow.ExpectedOutput           := 'e76dfad22084a8b1467fcf2ffa58361bec7628ed' +
                                       'f5f3fdc0e4805dc48caeeca81b7c13c30adf52a3' +
                                       '659584739a2df46be589c51ca1a4a8416df6545a' +
                                       '1ce8ba00';
  lDataRow.ExpectedOutputUTFStrTest := '54ab223a7cee7603f2b89596b54f8d838845e0a0' +
                                       'af2be3e9ad2cd7acb111757cb0c41b3564c07778' +
                                       '47684435da78577781eef8e6a6652c9844a85882' +
                                       'e0fa8b28';
  lDataRow.AddInputVector(RawByteString(
                          #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3), 1, 20);
  lDataRow.FinalBitLength := 0;

  // Source https://csrc.nist.gov/CSRC/media/Projects/Cryptographic-Standards-
  //        and-Guidelines/documents/examples/SHA3-512_Msg5.pdf
  lDataRow := FTestData.AddRow;
  lDataRow.ExpectedOutput           := 'a13e01494114c09800622a70288c432121ce7003' +
                                       '9d753cadd2e006e4d961cb27544c1481e5814bdc' +
                                       'eb53be6733d5e099795e5e81918addb058e22a9f' +
                                       '24883f37';
  lDataRow.ExpectedOutputUTFStrTest := '6961fe7f75e0ad35aead16be49c711b373d22226' +
                                       'ea2b7b2897df24048287a1b6d7b43bad246c8a44' +
                                       'ddccb49263a688343fa60142650a3e06af2a87c9' +
                                       '296438a8';
  lDataRow.AddInputVector(#$13);
  lDataRow.FinalBitLength := 5;

  // Source https://csrc.nist.gov/CSRC/media/Projects/Cryptographic-Standards-
  //        and-Guidelines/documents/examples/SHA3-512_Msg30.pdf
  lDataRow := FTestData.AddRow;
  lDataRow.ExpectedOutput           := '9834c05a11e1c5d3da9c740e1c106d9e590a0e53' +
                                       '0b6f6aaa7830525d075ca5db1bd8a6aa981a2861' +
                                       '3ac334934a01823cd45f45e49b6d7e6917f2f167' +
                                       '78067bab';
  lDataRow.ExpectedOutputUTFStrTest := '7e59a0c8fca17a79c748f927c85408bfda1b158d' +
                                       '7ab95df59f650a3bb773e1eb6c1112cd2c351b24' +
                                       'b99a2f8e08688ec19816bfc292fba63305307571' +
                                       'ce9360cf';
  lDataRow.AddInputVector(#$53#$58#$7B#$19);
  lDataRow.FinalBitLength := 6;

  // Source https://csrc.nist.gov/CSRC/media/Projects/Cryptographic-Standards-
  //        and-Guidelines/documents/examples/SHA3-512_Msg1605.pdf
  lDataRow := FTestData.AddRow;
  lDataRow.ExpectedOutput           := 'fc4a167ccb31a937d698fde82b04348c9539b28f' +
                                       '0c9d3b4505709c03812350e4990e9622974f6e57' +
                                       '5c47861c0d2e638ccfc2023c365bb60a93f52855' +
                                       '0698786b';
  lDataRow.ExpectedOutputUTFStrTest := '7e65803342ce242fdc8cfd27019516669615327f' +
                                       '679ced86453df7ee0a745267a453e7b94568c2ea' +
                                       'dc4ce7a7de02f48a5c2204d03418542f1a64f7db' +
                                       '78026218';
  lDataRow.AddInputVector(RawByteString(#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3));
  lDataRow.FinalBitLength := 5;

  // Source https://csrc.nist.gov/CSRC/media/Projects/Cryptographic-Standards-
  //        and-Guidelines/documents/examples/SHA3-512_1630.pdf
  lDataRow := FTestData.AddRow;
  lDataRow.ExpectedOutput           := 'cf9a30ac1f1f6ac0916f9fef1919c595debe2ee8' +
                                       '0c85421210fdf05f1c6af73aa9cac881d0f91db6' +
                                       'd034a2bbadc1cf7fbcb2ecfa9d191d3a5016fb3f' +
                                       'ad8709c9';
  lDataRow.ExpectedOutputUTFStrTest := '55971e2e5d0182f25bc453364f9d3a77fef05e7e' +
                                       '32c99e630a8586c37815aea5087a51aa3730f855' +
                                       'df963c7740f4e6bd2b73f5079cf317031ed120c3' +
                                       '3df4054d';
  lDataRow.AddInputVector(RawByteString(#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3#$A3 +
                                        #$A3#$A3#$A3#$A3));
  lDataRow.FinalBitLength := 6;
end;

procedure TestTHash_SHA3_512.TestBlockSize;
begin
  CheckEquals(72, FHash.BlockSize);
end;

procedure TestTHash_SHA3_512.TestClassByName;
begin
  DoTestClassByName('THash_SHA3_512', THash_SHA3_512);
end;

procedure TestTHash_SHA3_512.TestDigestSize;
begin
  CheckEquals(64, FHash.DigestSize);
end;

procedure TestTHash_SHA3_512.TestIdentity;
begin
  CheckEquals($17567DDA, FHash.Identity);
end;

procedure TestTHash_SHA3_512.TestIsPasswordHash;
begin
  CheckNotEquals(true, FHash.IsPasswordHash);
end;

initialization
  // Register any test cases with the test runner
  {$IFDEF DUnitX}
  TDUnitX.RegisterTestFixture(TestTHash_SHA3_224);
  TDUnitX.RegisterTestFixture(TestTHash_SHA3_256);
  TDUnitX.RegisterTestFixture(TestTHash_SHA3_384);
  TDUnitX.RegisterTestFixture(TestTHash_SHA3_512);
  {$ELSE}
  RegisterTests('DECHash', [TestTHash_SHA3_224.Suite,
                            TestTHash_SHA3_256.Suite,
                            TestTHash_SHA3_384.Suite,
                            TestTHash_SHA3_512.Suite]);
  {$ENDIF}

{$ELSE}
implementation
{$IFEND}

end.
