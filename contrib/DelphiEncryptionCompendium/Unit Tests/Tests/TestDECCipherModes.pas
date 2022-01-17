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
unit TestDECCipherModes;

interface

// Needs to be included before any other statements
{$INCLUDE TestDefines.inc}

uses
  {$IFDEF DUnitX}
  DUnitX.TestFramework,DUnitX.DUnitCompatibility,
  {$ELSE}
  TestFramework,
  {$ENDIF}
  System.SysUtils,
  DECCipherBase, DECCipherModes, DECCipherFormats, DECCiphers;

type
  /// <summary>
  ///   Class reference to be abe to specify duifferent cipher classes for
  ///   carrying out the different tests.
  /// </summary>
  TFormattedCipherClass = class of TDECFormattedCipher;

  /// <summary>
  ///   One entry in a list of tests
  /// </summary>
  TTestEntry = record
    /// <summary>
    ///   Input value, needs to be of block size length or a multiple of it
    /// </summary>
    Input      : RawByteString;
    /// <summary>
    ///   Expected output value, needs to be of block size length or a multiple of it
    /// </summary>
    Output     : RawByteString;
    /// <summary>
    ///   Expected output value which is used if Output is empty. Contains the
    ///   output in hexadecimal notation.
    /// </summary>
    OutputHex  : RawByteString;
    /// <summary>
    ///   Init Vektor für den ersten Test
    /// </summary>
    InitVector : RawByteString;
    /// <summary>
    ///   Class reference for the cipher class used for this test.
    /// </summary>
    TestClass  : TFormattedCipherClass;
    /// <summary>
    ///   Block concatenating/padding mode
    /// </summary>
    Mode       : TCipherMode;
  end;

  /// <summary>
  ///   Prototype for a function to be passed to the generic test method
  /// </summary>
  TTestFunction = procedure(Source, Dest: PByteArray; Size: Integer) of object;

  /// <summary>
  ///   Testmethoden für Klasse TDECCipherModes
  /// </summary>
  {$IFDEF DUnitX} [TestFixture] {$ENDIF}
  TestTDECCipherModes = class(TTestCase)
  strict private
    const
      Data: array[1..27] of TTestEntry = ((Input:     'ABCDEFGHIJKLMNOPQRSTUVWX';
                                          Output:    'ABCDEFGHIJKLMNOPQRSTUVWX';
                                          TestClass: TCipher_Null;
                                          Mode:      TCipherMode.cmECBx),
                                         (Input:     '000000000000000000000000';
                                          Output:    '000000000000000000000000';
                                          TestClass: TCipher_Null;
                                          Mode:      TCipherMode.cmECBx),
                                         (Input:     '12345678';
                                          Output:    '12345678';
                                          TestClass: TCipher_Null;
                                          Mode:      TCipherMode.cmECBx),
                                         (Input:     'ABCDEFGHIJKLMNOPQRSTUVWX';
                                          Output:    '';
                                          OutputHex: 'FE5A89A7A1F4BD29DFFADFCF2239E1F581106DA64C0AE704';
                                          TestClass: TCipher_1DES;
                                          Mode:      TCipherMode.cmOFB8),
                                         (Input:     '000000000000000000000000';
                                          Output:    '';
                                          OutputHex: '8F28FAD3D482CA51A680A4B35F479E95E0720EC2296C806C';
                                          TestClass: TCipher_1DES;
                                          Mode:      TCipherMode.cmOFB8),
                                         (Input:     '12345678';
                                          Output:    '';
                                          OutputHex: '8E2AF9D7D184CD59';
                                          TestClass: TCipher_1DES;
                                          Mode:      TCipherMode.cmOFB8),
                                         (Input:     'ABCDEFGHIJKLMNOPQRSTUVWX';
                                          Output:    '';
                                          OutputHex: 'FE604D3DF9C2AE3D7839AF5BDEE8FD9078544A1996EC4F1C';
                                          TestClass: TCipher_1DES;
                                          Mode:      TCipherMode.cmCFB8),
                                         (Input:     '000000000000000000000000';
                                          Output:    '';
                                          OutputHex: '8FD637FC449CF89F1E5EEBB66BED15C7F8C63B4481F74C5A';
                                          TestClass: TCipher_1DES;
                                          Mode:      TCipherMode.cmCFB8),
                                         (Input:     '12345678';
                                          Output:    '';
                                          OutputHex: '8EF08D6414063543';
                                          TestClass: TCipher_1DES;
                                          Mode:      TCipherMode.cmCFB8),
                                         (Input:     'ABCDEFGHIJKLMNOPQRSTUVWX';
                                          Output:    '';
                                          OutputHex: 'FEAB3839BBA059FC1FECBF798CEF537803F10F15967E3ABD';
                                          TestClass: TCipher_1DES;
                                          Mode:      TCipherMode.cmCFS8),
                                         (Input:     '000000000000000000000000';
                                          Output:    '';
                                          OutputHex: '8F9661B53B06D611BA916562F4420DA4B6EFD550BF01DA2C';
                                          TestClass: TCipher_1DES;
                                          Mode:      TCipherMode.cmCFS8),
                                         (Input:     '12345678';
                                          Output:    '';
                                          OutputHex: '8EEA2F2F86159953';
                                          TestClass: TCipher_1DES;
                                          Mode:      TCipherMode.cmCFS8),
                                         (Input:     'ABCDEFGHIJKLMNOPQRSTUVWX';
                                          Output:    '';
                                          OutputHex: 'FED41297FD52669B4221F913AF978D77292C958B2A9E289A';
                                          TestClass: TCipher_1DES;
                                          Mode:      TCipherMode.cmOFBx),
                                         (Input:     '000000000000000000000000';
                                          Output:    '';
                                          OutputHex: '8FA661E3882411E33B5B826FD2E9F217484EF6EF4FF84FF2';
                                          TestClass: TCipher_1DES;
                                          Mode:      TCipherMode.cmOFBx),
                                         (Input:     '12345678';
                                          Output:    '';
                                          OutputHex: '8EA462E78D2216EB';
                                          TestClass: TCipher_1DES;
                                          Mode:      TCipherMode.cmOFBx),
                                         (Input:     'ABCDEFGHIJKLMNOPQRSTUVWX';
                                          Output:    '';
                                          OutputHex: 'FED41297FD52669B0DEC818A383ADA358E469BE634B7AFBC';
                                          TestClass: TCipher_1DES;
                                          Mode:      TCipherMode.cmCFSx),
                                         (Input:     '000000000000000000000000';
                                          Output:    '';
                                          OutputHex: '8FA661E3882411E3DB7258A29424D11F2BB6B4607D24D5DB';
                                          TestClass: TCipher_1DES;
                                          Mode:      TCipherMode.cmCFSx),
                                         (Input:     '12345678';
                                          Output:    '';
                                          OutputHex: '8EA462E78D2216EB';
                                          TestClass: TCipher_1DES;
                                          Mode:      TCipherMode.cmCFSx),
                                         (Input     : 'ABCDEFGHIJKLMNOPQRSTUVWX';
                                          Output    : 'qsqwqsq'+#$7f+'89:;<=>/ikioikiw';
                                          InitVector: '01234567';
                                          TestClass : TCipher_NULL;
                                          Mode      : TCipherMode.cmCBCx),
                                         (Input     : '000000000000000000000000';
                                          Output    : '00000000' + #0#0#0#0#0#0#0#0 + '00000000';
                                          InitVector: #0#0#0#0#0#0#0#0;
                                          TestClass : TCipher_NULL;
                                          Mode      : TCipherMode.cmCBCx),
                                         (Input     : '000000000000000000000000';
                                          Output    : #0#1#2#3#4#5#6#7 + '01234567' + #0#1#2#3#4#5#6#7;
                                          InitVector: '01234567';
                                          TestClass : TCipher_NULL;
                                          Mode      : TCipherMode.cmCBCx),
                                         (Input:     'ABCDEFGHIJKLMNOPQRSTUVWX';
                                          Output:    '';
                                          OutputHex: 'FD73DA2F279926A19A65EFA8EBA5EEB67A778C6CD73294F5';
                                          TestClass: TCipher_1DES;
                                          Mode:      TCipherMode.cmCTSx),
                                         (Input:     '000000000000000000000000';
                                          Output:    '';
                                          OutputHex: '1D538CCCF38138A6BD4655272CC67443A0E32865EB422745';
                                          TestClass: TCipher_1DES;
                                          Mode:      TCipherMode.cmCTSx),
                                         (Input:     '12345678';
                                          Output:    '';
                                          OutputHex: '8EE274B893296F9E';
                                          TestClass: TCipher_1DES;
                                          Mode:      TCipherMode.cmCTSx),
                                         (Input:     'ABCDEFGHIJKLMNOPQRSTUVWX';
                                          Output:    '';
                                          OutputHex: 'FED41297FD52669BF5361295F3BD937EF0644802ED92DC21';
                                          TestClass: TCipher_1DES;
                                          Mode:      TCipherMode.cmCFBx),
                                         (Input:     '000000000000000000000000';
                                          Output:    '';
                                          OutputHex: '8FA661E3882411E35337C15BAE99B7CBDD988AC4FABB3368';
                                          TestClass: TCipher_1DES;
                                          Mode:      TCipherMode.cmCFBx),
                                         (Input:     '12345678';
                                          Output:    '';
                                          OutputHex: '8EA462E78D2216EB';
                                          TestClass: TCipher_1DES;
                                          Mode:      TCipherMode.cmCFBx));

    /// <summary>
    ///   Carries out the actual encode test
    /// </summary>
    /// <param name="Data">
    ///   Array with the data definint inputs and outputs for the tests
    /// </param>
    /// <param name="Mode">
    ///   Cipher mode which shall be tested
    /// </param>
    /// <param name="TestAllModes">
    ///   if true parameter Mode will be ignored and tests for all modes be
    ///   carried out
    /// </param>
    procedure DoTestEncode(Data: array of TTestEntry; Mode: TCipherMode; TestAllModes: Boolean = false);
    /// <summary>
    ///   Carries out the actual decode test
    /// </summary>
    /// <param name="Data">
    ///   Array with the data definint inputs and outputs for the tests
    /// </param>
    /// <param name="Mode">
    ///   Cipher mode which shall be tested
    /// </param>
    /// <param name="TestAllModes">
    ///   if true parameter Mode will be ignored and tests for all modes be
    ///   carried out
    /// </param>
    procedure DoTestDecode(Data: array of TTestEntry; Mode: TCipherMode; TestAllModes: Boolean = false);
  published
    procedure TestEncodeECBx;
    procedure TestEncodeOFB8;
    procedure TestEncodeCFB8;
    procedure TestEncodeCFS8;
    procedure TestEncodeCFBx;
    procedure TestEncodeOFBx;
    procedure TestEncodeCFSx;
    procedure TestEncodeCBCx;
    procedure TestEncodeCTSx;
    procedure TestDecodeECBx;
    procedure TestDecodeOFB8;
    procedure TestDecodeCFB8;
    procedure TestDecodeCFS8;
    procedure TestDecodeCFBx;
    procedure TestDecodeOFBx;
    procedure TestDecodeCFSx;
    procedure TestDecodeCBCx;
    procedure TestDecodeCTSx;
    procedure TestEncode;
    procedure TestDecode;
  end;

implementation

uses
  DECUtil;

procedure TestTDECCipherModes.DoTestEncode(Data: array of TTestEntry; Mode: TCipherMode; TestAllModes: Boolean = false);
var
  Dest   : TBytes;
  Source : TBytes;
  i, n   : Integer;
  Result : string;

  Cipher : TDECCipherModes;
begin
  for i := Low(Data) to High(Data) do
  begin
    if not TestAllModes then
      // Skip data for other modes
      if Data[i].Mode <> Mode then
        Continue;

    Cipher := Data[i].TestClass.Create;
    Cipher.Mode := Data[i].Mode;

    try
      Cipher.Init(BytesOf(RawByteString('ABCDEFGH')), BytesOf(Data[i].InitVector), $FF);

      SetLength(Source, Length(Data[i].Input));
      FillChar(Source[0], Length(Source), $FF);

      Move(Data[i].Input[1], Source[0], Length(Data[i].Input));

      SetLength(Dest, length(Source));
      Cipher.Encode(Source[0], Dest[0], length(Source));

      // Output is noted non hexadecimal
      if Data[i].Output <> '' then
      begin
        for n := Low(Dest) to High(Dest) do
        begin
          CheckEquals(Ord(Data[i].Output[n+1]), Dest[n],
                      IntToStr(n+1) + '. position is wrong. ' +
                      IntToStr(i) + '. test series. Expected: ' +
                      string(Data[i].Output) + ' was: ' + string(DECUtil.BytesToRawString(Dest)));
        end;
      end
      else
      begin
        // Output is noted in hex
        Result := '';
        for n := Low(Dest) to High(Dest) do
          Result := Result + IntToHex(Dest[n], 2);

        {$IF CompilerVersion >= 24.0}
        for n := Low(Result) to High(Result) do
          CheckEquals(char(Data[i].OutputHex[n]), Result[n],
                      IntToStr(n+1) + '. position is wrong. ' +
                      IntToStr(i) + '. test series. Expected: ' +
                      string(Data[i].OutputHex) + ' was: ' + Result);
        {$ELSE}
        for n := 1 to Length(Result) do
          CheckEquals(char(Data[i].OutputHex[n]), Result[n],
                      IntToStr(n+1) + '. position is wrong. ' +
                      IntToStr(i) + '. test series. Expected: ' +
                      string(Data[i].OutputHex) + ' was: ' + Result);
        {$IFEND}
      end;

    finally
      Cipher.Free;
    end;
  end;
end;

procedure TestTDECCipherModes.DoTestDecode(Data: array of TTestEntry; Mode: TCipherMode; TestAllModes: Boolean = false);
var
  Dest    : TBytes;
  Source  : TBytes;
  i, n, m : Integer;

  Cipher : TDECCipherModes;
begin
  for i := Low(Data) to High(Data) do
  begin
    if not TestAllModes then
      // Skip data for other modes
      if Data[i].Mode <> Mode then
        Continue;

    Cipher := Data[i].TestClass.Create;
    Cipher.Mode := Data[i].Mode;

    try
      Cipher.Init(BytesOf(RawByteString('ABCDEFGH')), BytesOf(Data[i].InitVector), $FF);

      if (Data[i].Output <> '') then
      begin
        SetLength(Source, Length(Data[i].Output));
        FillChar(Source[0], Length(Source), $FF);

        Move(Data[i].Output[1], Source[0], Length(Data[i].Output));
      end
      else
      begin
        SetLength(Source, Length(Data[i].OutputHex) div 2);
        FillChar(Source[0], Length(Source), $FF);

        n := 1; m := 0;

        repeat
          Source[m] := StrToInt('$' + char(Data[i].OutputHex[n]) + char(Data[i].OutputHex[n +1]));

          inc(n, 2);
          inc(m);
        until (n > Length(Data[i].OutputHex));
      end;

      SetLength(Dest, length(Source));
      Cipher.Decode(Source[0], Dest[0], length(Source));

      for n := Low(Dest) to High(Dest) do
      begin
        CheckEquals(Ord(Data[i].Input[n+1]), Dest[n],
                    IntToStr(n+1) + '. position is wrong. ' +
                    IntToStr(i) + '. test series. Expected: ' +
                    string(Data[i].Input) + ' was: ' + string(DECUtil.BytesToRawString(Dest)));
      end;

    finally
      Cipher.Free;
    end;
  end;
end;

procedure TestTDECCipherModes.TestEncodeECBx;
begin
  DoTestEncode(Data, TCipherMode.cmECBx);
end;

procedure TestTDECCipherModes.TestEncodeOFB8;
begin
  DoTestEncode(Data, TCipherMode.cmOFB8);
end;

procedure TestTDECCipherModes.TestEncodeCFB8;
begin
  DoTestEncode(Data, TCipherMode.cmCFB8);
end;

procedure TestTDECCipherModes.TestEncodeCFS8;
begin
  DoTestEncode(Data, TCipherMode.cmCFS8);
end;

procedure TestTDECCipherModes.TestEncodeCFBx;
begin
  DoTestEncode(Data, TCipherMode.cmCFBx);
end;

procedure TestTDECCipherModes.TestEncodeOFBx;
begin
  DoTestEncode(Data, TCipherMode.cmOFBx);
end;

procedure TestTDECCipherModes.TestEncodeCFSx;
begin
  DoTestEncode(Data, TCipherMode.cmCFSx);
end;

procedure TestTDECCipherModes.TestEncodeCBCx;
begin
  DoTestEncode(Data, TCipherMode.cmCBCx);
end;

procedure TestTDECCipherModes.TestEncodeCTSx;
begin
  DoTestEncode(Data, TCipherMode.cmCTSx);
end;

procedure TestTDECCipherModes.TestDecodeECBx;
begin
  DoTestDecode(Data, TCipherMode.cmECBx);
end;

procedure TestTDECCipherModes.TestDecodeOFB8;
begin
  DoTestDecode(Data, TCipherMode.cmOFB8);
end;

procedure TestTDECCipherModes.TestDecodeCFB8;
begin
  DoTestDecode(Data, TCipherMode.cmCFB8);
end;

procedure TestTDECCipherModes.TestDecodeCFS8;
begin
  DoTestDecode(Data, TCipherMode.cmCFS8);
end;

procedure TestTDECCipherModes.TestDecodeCFBx;
begin
  DoTestDecode(Data, TCipherMode.cmCFBx);
end;

procedure TestTDECCipherModes.TestDecodeOFBx;
begin
  DoTestDecode(Data, TCipherMode.cmOFBx);
end;

procedure TestTDECCipherModes.TestDecodeCFSx;
begin
  DoTestDecode(Data, TCipherMode.cmCFSx);
end;

procedure TestTDECCipherModes.TestDecodeCBCx;
begin
  DoTestDecode(Data, TCipherMode.cmCBCx);
end;

procedure TestTDECCipherModes.TestDecodeCTSx;
begin
  DoTestDecode(Data, TCipherMode.cmCTSx);
end;

procedure TestTDECCipherModes.TestEncode;
begin
  DoTestEncode(Data, TCipherMode.cmCTSx, true);
end;

procedure TestTDECCipherModes.TestDecode;
begin
  DoTestDecode(Data, TCipherMode.cmCTSx, true);
end;

initialization
  // Register all test cases to be run
  {$IFDEF DUnitX}
  TDUnitX.RegisterTestFixture(TestTDECCipherModes);
  {$ELSE}
  RegisterTest(TestTDECCipherModes.Suite);
  {$ENDIF}
end.
