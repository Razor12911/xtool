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
unit TestDECCRC;

interface

// Needs to be included before any other statements
{$INCLUDE TestDefines.inc}

uses
  System.SysUtils, System.Classes,
  {$IFDEF DUnitX}
  DUnitX.TestFramework, DUnitX.DUnitCompatibility,
  {$ELSE}
  TestFramework,
  {$ENDIF}
  DECUtil, DECCRC;

type
  /// <summary>
  ///   Entry for one single CRC test
  /// </summary>
  TCRCTest = record
    Input : RawByteString;
    CRC   : UInt32;
  end;

  // Test methods for CRC routines
  {$IFDEF DUnitX} [TestFixture] {$ENDIF}
  TestCRC = class(TTestCase)
  strict private
    FTestData      : array of TCRCTest;
    /// <summary>
    ///   Required index into FTestData for the callback based method test
    /// </summary>
    FTestDataIndex : Integer;

    /// <summary>
    ///   Buffer with the data for the CRCCode variant taking a callback to
    ///   fetch the data from. The test will fill this buffer for the callback
    ///   to fetch its data from
    /// </summary>
    FCallbackBuffer : TBytes;
  private
    procedure SetUpCRC8;
    procedure SetUpCRC8ATMHEC;
    procedure SetUpCRC8SMBus;
    procedure SetUpCRC10;
    procedure SetUpCRC12;
    procedure SetUpCRC15CAN;
    procedure SetUpCRC16;
    procedure SetUpCRC16CCITT;
    procedure SetUpCRC16XMODEM;
    procedure SetUpCRC16ZMODEM;
    procedure SetUpCRC24;
    procedure SetUpCRC32;
    procedure SetUpCRC32CCITT;
    procedure SetUpCRC32ZMODEM;

    procedure DoTest(CRCType:TCRCType);
    procedure TestCRCCodeSingleBuffer(Test: TCRCTest; CRCDef: TCRCDef);
    procedure TestCRCCodeMultiBuffer(Test: TCRCTest; CRCDef: TCRCDef);

    procedure TestCRCCodeSingleBufferCallback(Test: TCRCTest; CRCDef: TCRCDef);

    /// <summary>
    ///   Callback for the tests for the CRCCode variant requiring a callback
    ///   for delivering the data to compute the CRC over.
    /// </summary>
    function CRCCodeReadCallback(var Buffer; Count: Int64): Int64;
    function ReadMethod(var Buffer; Count: Int64): Int64;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCRCSetup;

    procedure TestCRCInitCRC8;
    procedure TestCRCInitCRC10;
    procedure TestCRCInitCRC12;
    procedure TestCRCInitCRC16;
    procedure TestCRCInitCRC16CCITT;
    procedure TestCRCInitCRC16XMODEM;
    procedure TestCRCInitCRC16ZMODEM;
    procedure TestCRCInitCRC24;
    procedure TestCRCInitCRC32;
    procedure TestCRCInitCRC32CCITT;
    procedure TestCRCInitCRC32ZMODEM;
    procedure TestCRCInitCRC8ATMHEC;
    procedure TestCRCInitCRC8SMBUS;
    procedure TestCRCInitCRC15CAN;

    procedure TestCRCCodeCRC16SingleBuffer;
    procedure TestCRCCodeCRC16MultiBuffer;
    procedure TestCRCCodeCRC32SingleBuffer;
    procedure TestCRCCodeCRC32MultiBuffer;

    procedure TestCRCCodeCallback;

    procedure TestCRCCodeCRC16SingleBufferCallback;

    procedure TestCRCDoneFinalVector0;
    procedure TestCRCDoneFinalVectorFFFFFFFF;

    procedure TestCRC8;
    procedure TestCRC8ATMHEC;
    procedure TestCRC8SMBus;
    procedure TestCRC10;
    procedure TestCRC12;
    procedure TestCRC15CAN;
    procedure TestCRC16;
    procedure TestCRC16CCITT;
    procedure TestCRC16XMODEM;
    procedure TestCRC16ZMODEM;
    procedure TestCRC24;
    procedure TestCRC32;
    procedure TestCRC32CCITT;
    procedure TestCRC32ZMODEM;

    procedure TestCRC16Standalone;
    procedure TestCRC32Standalone;
  end;

implementation

const
  /// <summary>
  ///   Test data for the callback based CRCCode variant. is the same as one CRC32
  ///   test data element (123456789)
  /// </summary>
  cCallbackTestData : array[1..9] of Byte = ($31, $32, $33, $34, $35, $36, $37, $38, $39);

procedure TestCRC.SetUp;
begin
end;

procedure TestCRC.TearDown;
begin
end;

function TestCRC.CRCCodeReadCallback(var Buffer; Count: Int64): Int64;
begin
  if (length(FCallbackBuffer) >= Count) then
  begin
    Move(FCallbackBuffer[0], Buffer, Count);
    result := Count;
  end
  else
    if (length(FCallbackBuffer) > 0) then
    begin
      Move(FCallbackBuffer[0], Buffer, length(FCallbackBuffer));
      result := length(FCallbackBuffer);
    end
    else
      result := 0;
end;

procedure TestCRC.DoTest(CRCType:TCRCType);
var
  SrcBuf : TBytes;
  CRC    : UInt32;
  i      : Integer;
begin
  for i := Low(FTestData) to High(FTestData) do
  begin
    SrcBuf := BytesOf(FTestData[i].Input);

    if Length(FTestData[i].Input) > 0 then
      CRC   := CRCCalc(CRCType, SrcBuf[0], Length(SrcBuf))
    else
      CRC   := CRCCalc(CRCType, SrcBuf, Length(SrcBuf));

    CheckEquals(FTestData[i].CRC, CRC, 'Input: ' + string(FTestData[i].Input));
  end;
end;

procedure TestCRC.SetUpCRC8;
begin
  SetLength(FTestData, 5);
  FTestData[0].Input := '';
  FTestData[0].CRC   := $00;
  FTestData[1].Input := '123456789';
  FTestData[1].CRC   := $07;
  FTestData[2].Input := '123456780';
  FTestData[2].CRC   := $DB;
  FTestData[3].Input := '0000';
  FTestData[3].CRC   := $2F;
  FTestData[4].Input := '00000';
  FTestData[4].CRC   := $5D;
end;

procedure TestCRC.TestCRC8;
begin
  SetUpCRC8;
  DoTest(CRC_8);
end;

procedure TestCRC.SetUpCRC8ATMHEC;
begin
  SetLength(FTestData, 5);
  FTestData[0].Input := '';
  FTestData[0].CRC   := $00;
  FTestData[1].Input := '123456789';
  FTestData[1].CRC   := $20;
  FTestData[2].Input := '123456780';
  FTestData[2].CRC   := $BF;
  FTestData[3].Input := '0000';
  FTestData[3].CRC   := $53;
  FTestData[4].Input := '00000';
  FTestData[4].CRC   := $3A;
end;

procedure TestCRC.TestCRC8ATMHEC;
begin
  SetUpCRC8ATMHEC;
  DoTest(CRC_8ATMHEC);
end;

procedure TestCRC.SetUpCRC8SMBus;
begin
  SetLength(FTestData, 5);
  FTestData[0].Input := '';
  FTestData[0].CRC   := $00;
  FTestData[1].Input := '123456789';
  FTestData[1].CRC   := $F4;
  FTestData[2].Input := '123456780';
  FTestData[2].CRC   := $CB;
  FTestData[3].Input := '0000';
  FTestData[3].CRC   := $21;
  FTestData[4].Input := '00000';
  FTestData[4].CRC   := $77;
end;

procedure TestCRC.TestCRC8SMBus;
begin
  SetUpCRC8SMBus;
  DoTest(CRC_8SMBus);
end;

procedure TestCRC.TestCRCCodeCRC16SingleBuffer;
var
  CRCDef : TCRCDef;
  Test   : TCRCTest;
begin
  SetUpCRC16;

  for Test in FTestData do
  begin
    CheckEquals(true,
                CRCSetup(CRCDef, $00008005, 16, $00000000, $00000000, true),
                'CRC algorithm not properly set up');

    TestCRCCodeSingleBuffer(Test, CRCDef);
  end;
end;

procedure TestCRC.TestCRCCodeCRC16SingleBufferCallback;
var
  CRCDef : TCRCDef;
  Test   : TCRCTest;
begin
  SetUpCRC16;

  for Test in FTestData do
  begin
    CheckEquals(true,
                CRCSetup(CRCDef, $00008005, 16, $00000000, $00000000, true),
                'CRC algorithm not properly set up');

    TestCRCCodeSingleBufferCallback(Test, CRCDef);
  end;
end;

procedure TestCRC.TestCRCCodeCallback;
var
  CRCDef   : TCRCDef;
  CRC      : UInt32;
  i        : Integer;
begin
  SetLength(FTestData, 3);
  FTestData[0].Input := '123456789';
  FTestData[0].CRC   := $7DC08C09;
  FTestData[1].Input := '12345678901234567890123456789012345678901234567890123'+
                        '45678901234567890123456789012345678901234567890123456'+
                        '78901234567890123456789012345678901234567890123456789'+
                        '01234567890123456789012345678901234567890123456789012'+
                        '34567890123456789012345678901234567890123456789012345'+
                        '67890123456789012345678901234567890123456789012345678'+
                        '90123456789012345678901234567890123456789012345678901'+
                        '23456789012345678901234567890123456789012345678901234'+
                        '56789012345678901234567890123456789012345678901234567'+
                        '89012345678901234567890123456789012345678901234567890'+
                        '12345678901234567890123456789012345678901234567890123'+
                        '45678901234567890123456789012345678901234567890123456'+
                        '78901234567890123456789012345678901234567890123456789'+
                        '01234567890123456789012345678901234567890123456789012'+
                        '34567890123456789012345678901234567890123456789012345'+
                        '67890123456789012345678901234567890123456789012345678'+
                        '90123456789012345678901234567890123456789012345678901'+
                        '23456789012345678901234567890123456789012345678901234'+
                        '56789012345678901234567890123456789012345678901234567'+
                        '89012345678901234567890123456789012345678901234567890'+
                        '1234567890123456789012345678901234567890';
  FTestData[1].CRC   := $BED38AF0; // Length: 1100 byte
  FTestData[2].Input := '12345678901234567890123456789012345678901234567890123'+
                        '45678901234567890123456789012345678901234567890123456'+
                        '78901234567890123456789012345678901234567890123456789'+
                        '01234567890123456789012345678901234567890123456789012'+
                        '34567890123456789012345678901234567890123456789012345'+
                        '67890123456789012345678901234567890123456789012345678'+
                        '90123456789012345678901234567890123456789012345678901'+
                        '23456789012345678901234567890123456789012345678901234'+
                        '56789012345678901234567890123456789012345678901234567'+
                        '89012345678901234567890123456789012345678901234567890'+
                        '12345678901234567890123456789012345678901234567890123'+
                        '45678901234567890123456789012345678901234567890123456'+
                        '78901234567890123456789012345678901234567890123456789'+
                        '01234567890123456789012345678901234567890123456789012'+
                        '34567890123456789012345678901234567890123456789012345'+
                        '67890123456789012345678901234567890123456789012345678'+
                        '90123456789012345678901234567890123456789012345678901'+
                        '23456789012345678901234567890123456789012345678901234'+
                        '56789012345678901234567890123456789012345678901234567'+
                        '890123456789012345';
  FTestData[2].CRC   := $9E5864CB; // length: 1025 byte, just 1 byte longer than
                                   // the buffer


  FTestDataIndex := 0;

  for i := Low(FTestData) to High(FTestData) do
  begin
    CheckEquals(true,
                DECCRC.CRCInit(CRCDef, CRC_32),
                'CRC setup failed');

    CRC := DECCRC.CRCCode(CRCDef, ReadMethod, length(FTestData[i].Input));
    CheckEquals(FTestData[i].CRC, CRC, 'Wrong callback CRC calculation result');

    inc(FTestDataIndex);
  end;
end;

function TestCRC.ReadMethod(var Buffer; Count: Int64): Int64;
var
  p : PByte;
  i : Integer;
begin
  p := @Buffer;
  for i := 0 to Count-1 do
  begin
    {$IF CompilerVersion >= 24.0}
    p^ := Ord(FTestData[FTestDataIndex].Input[low(FTestData[FTestDataIndex].Input) + i]);
    {$ELSE}
    p^ := Ord(FTestData[FTestDataIndex].Input[1 + i]);
    {$IFEND}
    inc(p);
  end;

  result := Count;
end;

procedure TestCRC.TestCRCCodeCRC16MultiBuffer;
var
  CRCDef   : TCRCDef;
  Test     : TCRCTest;
begin
  SetLength(FTestData, 2);
  FTestData[0].Input := '123456789';
  FTestData[0].CRC   := $BB3D;
  FTestData[1].Input := '123456780';
  FTestData[1].CRC   := $BDFD;

  for Test in FTestData do
  begin
    CheckEquals(true,
                CRCSetup(CRCDef, $00008005, 16, $00000000, $00000000, true),
                'CRC algorithm not properly set up');

    TestCRCCodeMultiBuffer(Test, CRCDef);
  end;
end;

procedure TestCRC.TestCRCCodeCRC32SingleBuffer;
var
  CRCDef : TCRCDef;
  Test   : TCRCTest;
begin
  SetUpCRC32;

  for Test in FTestData do
  begin
    CheckEquals(true,
                CRCSetup(CRCDef, $9DB11213, 32, $FFFFFFFF, $FFFFFFFF, true),
                'CRC algorithm not properly set up');

    TestCRCCodeSingleBuffer(Test, CRCDef);
  end;
end;

procedure TestCRC.TestCRCCodeCRC32MultiBuffer;
var
  CRCDef   : TCRCDef;
  Test     : TCRCTest;
begin
  SetLength(FTestData, 2);
  FTestData[0].Input := '123456789';
  FTestData[0].CRC   := $7DC08C09;
  FTestData[1].Input := '123456780';
  FTestData[1].CRC   := $52284990;

  for Test in FTestData do
  begin
    CheckEquals(true,
                CRCSetup(CRCDef, $9DB11213, 32, $FFFFFFFF, $FFFFFFFF, true),
                'CRC algorithm not properly set up');

    TestCRCCodeMultiBuffer(Test, CRCDef);
  end;
end;

procedure TestCRC.TestCRCSetup;
var
  CRCDef : TCRCDef;
begin
  // correct initializations
  CheckEquals(true, CRCSetup(CRCDef, 1234, 8, 5678, 9, true), '8 Bit setup failure');
  CheckEquals(1234, CRCDef.Polynomial,  '8 Bit polynome setup failure');
  CheckEquals(   8, CRCDef.Bits,        '8 Bit bit count setup failure');
  CheckEquals(5678, CRCDef.InitVector,  '8 Bit init vector setup failure');
  CheckEquals(   9, CRCDef.FinalVector, '8 Bit final vector setup failure');
  CheckEquals(true, CRCDef.Inverse,     '8 Bit inverse setup failure');

  CheckEquals(true, CRCSetup(CRCDef, 5678, 16, 1234, 99, false), '16 Bit setup failure');
  CheckEquals(5678, CRCDef.Polynomial,  '16 Bit polynome setup failure');
  CheckEquals(  16, CRCDef.Bits,        '16 Bit bit count setup failure');
  CheckEquals(1234, CRCDef.InitVector,  '16 Bit init vector setup failure');
  CheckEquals(  99, CRCDef.FinalVector, '16 Bit final vector setup failure');
  CheckEquals(false, CRCDef.Inverse,    '16 Bit inverse setup failure');

  CheckEquals(true, CRCSetup(CRCDef, 12341234, 32, 56785678, 99999999, true), '32 Bit setup failure');
  CheckEquals(12341234, CRCDef.Polynomial,  '32 Bit polynome setup failure');
  CheckEquals(      32, CRCDef.Bits,        '32 Bit bit count setup failure');
  CheckEquals(56785678, CRCDef.InitVector,  '32 Bit init vector setup failure');
  CheckEquals(99999999, CRCDef.FinalVector, '32 Bit final vector setup failure');
  CheckEquals(true, CRCDef.Inverse,         '32 Bit inverse setup failure');

  // initialization with too few bits
  CheckEquals(false, CRCSetup(CRCDef, 1234, 7, 5678, 9, true), '7 Bit setup failure');
end;

procedure TestCRC.SetUpCRC10;
begin
  SetLength(FTestData, 5);
  FTestData[0].Input := '';
  FTestData[0].CRC   := 0;
  FTestData[1].Input := '123456789';
  FTestData[1].CRC   := $391;
  FTestData[2].Input := '123456780';
  FTestData[2].CRC   := $1E7;
  FTestData[3].Input := '0000';
  FTestData[3].CRC   := $6F;
  FTestData[4].Input := '00000';
  FTestData[4].CRC   := $348;
end;

procedure TestCRC.TestCRC10;
begin
  SetUpCRC10;
  DoTest(CRC_10);
end;

procedure TestCRC.SetUpCRC12;
begin
  SetLength(FTestData, 5);
  FTestData[0].Input := '';
  FTestData[0].CRC   := 0;
  FTestData[1].Input := '123456789';
  FTestData[1].CRC   := $C61;
  FTestData[2].Input := '123456780';
  FTestData[2].CRC   := $C3B;
  FTestData[3].Input := '0000';
  FTestData[3].CRC   := $5D2;
  FTestData[4].Input := '00000';
  FTestData[4].CRC   := $6D1;
end;

procedure TestCRC.TestCRC12;
begin
  SetUpCRC12;
  DoTest(CRC_12);
end;

procedure TestCRC.SetUpCRC15CAN;
begin
  SetLength(FTestData, 5);
  FTestData[0].Input := '';
  FTestData[0].CRC   := 0;
  FTestData[1].Input := '123456789';
  FTestData[1].CRC   := $3645;
  FTestData[2].Input := '123456780';
  FTestData[2].CRC   := $26BD;
  FTestData[3].Input := '0000';
  FTestData[3].CRC   := $20AD;
  FTestData[4].Input := '00000';
  FTestData[4].CRC   := $1673;
end;

procedure TestCRC.TestCRC15CAN;
begin
  SetUpCRC15CAN;
  DoTest(CRC_15CAN);
end;

procedure TestCRC.SetUpCRC16;
begin
  SetLength(FTestData, 5);
  FTestData[0].Input := '';
  FTestData[0].CRC   := 0;
  FTestData[1].Input := '123456789';
  FTestData[1].CRC   := $BB3D;
  FTestData[2].Input := '123456780';
  FTestData[2].CRC   := $BDFD;
  FTestData[3].Input := '0000';
  FTestData[3].CRC   := $1B1B;
  FTestData[4].Input := '00000';
  FTestData[4].CRC   := $1F5B;
end;

procedure TestCRC.TestCRC16;
begin
  SetUpCRC16;
  DoTest(CRC_16);
end;

procedure TestCRC.SetUpCRC16CCITT;
begin
  SetLength(FTestData, 5);
  FTestData[0].Input := '';
  FTestData[0].CRC   := $1D0F;
  FTestData[1].Input := '123456789';
  FTestData[1].CRC   := $E5CC;
  FTestData[2].Input := '123456780';
  FTestData[2].CRC   := $74E5;
  FTestData[3].Input := '0000';
  FTestData[3].CRC   := $D49A;
  FTestData[4].Input := '00000';
  FTestData[4].CRC   := $27AA;
end;

procedure TestCRC.TestCRC16CCITT;
begin
  SetUpCRC16CCITT;
  DoTest(CRC_16CCITT);
end;

procedure TestCRC.TestCRC16Standalone;
var
  CRC : UInt16;
  i   : Integer;
begin
  SetUpCRC16;

  for i := 0 to length(FTestData)-1 do
  begin
    if FTestData[i].Input <> '' then
    begin
      {$IF CompilerVersion >= 24.0}
      CRC := CRC16(0,
                   FTestData[i].Input[low(FTestData[i].Input)],
                   length(FTestData[i].Input));
      {$ELSE}
      CRC := CRC16(0,
                   FTestData[i].Input[1],
                   length(FTestData[i].Input));
      {$IFEND}

      CheckEquals(FTestData[i].CRC, CRC,
                  'Wrong CRC16Standalone reult for iteration ' + IntToStr(i));
    end;
  end;
end;

procedure TestCRC.SetUpCRC16XMODEM;
begin
  SetLength(FTestData, 5);
  FTestData[0].Input := '';
  FTestData[0].CRC   := $0;
  FTestData[1].Input := '123456789';
  FTestData[1].CRC   := $C73;
  FTestData[2].Input := '123456780';
  FTestData[2].CRC   := $482;
  FTestData[3].Input := '0000';
  FTestData[3].CRC   := $1A1A;
  FTestData[4].Input := '00000';
  FTestData[4].CRC   := $CC3;
end;

procedure TestCRC.TestCRC16XMODEM;
begin
  SetUpCRC16XMODEM;
  DoTest(CRC_16XMODEM);
end;

procedure TestCRC.SetUpCRC16ZMODEM;
begin
  SetLength(FTestData, 5);
  FTestData[0].Input := '';
  FTestData[0].CRC   := $0;
  FTestData[1].Input := '123456789';
  FTestData[1].CRC   := $31C3;
  FTestData[2].Input := '123456780';
  FTestData[2].CRC   := $A0EA;
  FTestData[3].Input := '0000';
  FTestData[3].CRC   := $DA8A;
  FTestData[4].Input := '00000';
  FTestData[4].CRC   := $D664;
end;

procedure TestCRC.TestCRC16ZMODEM;
begin
  SetUpCRC16ZMODEM;
  DoTest(CRC_16ZMODEM);
end;

procedure TestCRC.SetUpCRC24;
begin
  SetLength(FTestData, 5);
  FTestData[0].Input := '';
  FTestData[0].CRC   := $00B704CE;
  FTestData[1].Input := '123456789';
  FTestData[1].CRC   := $21CF02;
  FTestData[2].Input := '123456780';
  FTestData[2].CRC   := $602C0;
  FTestData[3].Input := '0000';
  FTestData[3].CRC   := $2CF27D;
  FTestData[4].Input := '00000';
  FTestData[4].CRC   := $55451;
end;

procedure TestCRC.TestCRC24;
begin
  SetUpCRC24;
  DoTest(CRC_24);
end;

procedure TestCRC.SetUpCRC32;
begin
  SetLength(FTestData, 5);
  FTestData[0].Input := '';
  FTestData[0].CRC   := $FFFFFFFF;
  FTestData[1].Input := '123456789';
  FTestData[1].CRC   := $7DC08C09;
  FTestData[2].Input := '123456780';
  FTestData[2].CRC   := $52284990;
  FTestData[3].Input := '0000';
  FTestData[3].CRC   := $BD2AFE8D;
  FTestData[4].Input := '00000';
  FTestData[4].CRC   := $8B779315;
end;

procedure TestCRC.TestCRC32;
begin
  SetUpCRC32;
  DoTest(CRC_32);
end;

procedure TestCRC.SetUpCRC32CCITT;
begin
  SetLength(FTestData, 5);
  FTestData[0].Input := '';
  FTestData[0].CRC   := $FFFFFFFF;
  FTestData[1].Input := '123456789';
  FTestData[1].CRC   := $CBF43926;
  FTestData[2].Input := '123456780';
  FTestData[2].CRC   := $B2288182;
  FTestData[3].Input := '0000';
  FTestData[3].CRC   := $C9BC472;
  FTestData[4].Input := '00000';
  FTestData[4].CRC   := $4ADC54F5;
end;

procedure TestCRC.TestCRC32CCITT;
begin
  SetUpCRC32CCITT;
  DoTest(CRC_32CCITT);
end;

procedure TestCRC.TestCRC32Standalone;
var
  CRC : UInt32;
  i   : Integer;
begin
  SetUpCRC32CCITT;

  for i := 0 to length(FTestData)-1 do
  begin
    if FTestData[i].Input <> '' then
    begin
      {$IF CompilerVersion >= 24.0}
      CRC := CRC32(0,
                   FTestData[i].Input[low(FTestData[i].Input)],
                   length(FTestData[i].Input));
      {$ELSE}
      CRC := CRC32(0,
                   FTestData[i].Input[1],
                   length(FTestData[i].Input));
      {$IFEND}

      CheckEquals(FTestData[i].CRC, CRC,
                  'Wrong CRC32Standalone reult for iteration ' + IntToStr(i));
    end;
  end;
end;

procedure TestCRC.SetUpCRC32ZMODEM;
begin
  SetLength(FTestData, 5);
  FTestData[0].Input := '';
  FTestData[0].CRC   := $FFFFFFFF;
  FTestData[1].Input := '123456789';
  FTestData[1].CRC   := $340BC6D9;
  FTestData[2].Input := '123456780';
  FTestData[2].CRC   := $4DD77E7D;
  FTestData[3].Input := '0000';
  FTestData[3].CRC   := $F3643B8D;
  FTestData[4].Input := '00000';
  FTestData[4].CRC   := $B523AB0A;
end;

procedure TestCRC.TestCRC32ZMODEM;
begin
  SetUpCRC32ZMODEM;
  DoTest(CRC_32ZMODEM);
end;

procedure TestCRC.TestCRCInitCRC15CAN;
var
  CRCDef : TCRCDef;
begin
  CheckEquals(true,  CRCInit(CRCDef, CRC_15CAN), 'CRC_15CAN data not retrieved');
  CheckEquals($4599, CRCDef.Polynomial,          'CRC_15CAN polynome wrong');
  CheckEquals(15,    CRCDef.Bits,                'CRC_15CAN bit count wrong');
  CheckEquals($0,    CRCDef.InitVector,          'CRC_15CAN init vector wrong');
  CheckEquals(true,  CRCDef.Inverse,             'CRC_15CAN inverse Flag wrong');
end;

procedure TestCRC.TestCRCInitCRC8SMBUS;
var
  CRCDef : TCRCDef;
begin
  CheckEquals(true,  CRCInit(CRCDef, CRC_8SMBUS), 'CRC_8SMBUS data not retrieved');
  CheckEquals($7,    CRCDef.Polynomial,           'CRC_8SMBUS polynome wrong');
  CheckEquals(8,     CRCDef.Bits,                 'CRC_8SMBUS bit count wrong');
  CheckEquals($0,    CRCDef.InitVector,           'CRC_8SMBUS init vector wrong');
  CheckEquals(false, CRCDef.Inverse,              'CRC_8SMBUS inverse Flag wrong');
end;

procedure TestCRC.TestCRCInitCRC8ATMHEC;
var
  CRCDef : TCRCDef;
begin
  CheckEquals(true, CRCInit(CRCDef, CRC_8ATMHEC), 'CRC_8ATMHEC data not retrieved');
  CheckEquals($7,   CRCDef.Polynomial,            'CRC_8ATMHEC polynome wrong');
  CheckEquals(8,    CRCDef.Bits,                  'CRC_8ATMHEC bit count wrong');
  CheckEquals($0,   CRCDef.InitVector,            'CRC_8ATMHEC init vector wrong');
  CheckEquals(true, CRCDef.Inverse,               'CRC_8ATMHEC inverse Flag wrong');
end;

procedure TestCRC.TestCRCInitCRC32ZMODEM;
var
  CRCDef : TCRCDef;
begin
  CheckEquals(true,       CRCInit(CRCDef, CRC_32ZMODEM), 'CRC_32ZMODEM data not retrieved');
  CheckEquals($4C11DB7,   CRCDef.Polynomial,             'CRC_32ZMODEM polynome wrong');
  CheckEquals(32,         CRCDef.Bits,                   'CRC_32ZMODEM bit count wrong');
  CheckEquals(4294967295, CRCDef.InitVector,             'CRC_32ZMODEM init vector wrong');
  CheckEquals(true,       CRCDef.Inverse,                'CRC_32ZMODEM inverse Flag wrong');
end;

procedure TestCRC.TestCRCInitCRC32CCITT;
var
  CRCDef : TCRCDef;
begin
  CheckEquals(true,       CRCInit(CRCDef, CRC_32CCITT), 'CRC_32CCITT data not retrieved');
  CheckEquals($4C11DB7,   CRCDef.Polynomial,            'CRC_32CCITT polynome wrong');
  CheckEquals(32,         CRCDef.Bits,                  'CRC_32CCITT bit count wrong');
  CheckEquals(4294967295, CRCDef.InitVector,            'CRC_32CCITT init vector wrong');
  CheckEquals(true,       CRCDef.Inverse,               'CRC_32CCITT inverse Flag wrong');
end;

procedure TestCRC.TestCRCInitCRC32;
var
  CRCDef : TCRCDef;
begin
  CheckEquals(true,       CRCInit(CRCDef, CRC_32), 'CRC_32 data not retrieved');
  CheckEquals(2645627411, CRCDef.Polynomial,       'CRC_32 polynome wrong');
  CheckEquals(32,         CRCDef.Bits,             'CRC_32 bit count wrong');
  CheckEquals(4294967295, CRCDef.InitVector,       'CRC_32 init vector wrong');
  CheckEquals(true,       CRCDef.Inverse,          'CRC_32 inverse Flag wrong');
end;

procedure TestCRC.TestCRCInitCRC24;
var
  CRCDef : TCRCDef;
begin
  CheckEquals(true,    CRCInit(CRCDef, CRC_24), 'CRC_24 data not retrieved');
  CheckEquals($864CFB, CRCDef.Polynomial,       'CRC_24 polynome wrong');
  CheckEquals(24,      CRCDef.Bits,             'CRC_24 bit count wrong');
  CheckEquals($B704CE, CRCDef.InitVector,       'CRC_24 init vector wrong');
  CheckEquals(false,   CRCDef.Inverse,          'CRC_24 inverse Flag wrong');
end;

procedure TestCRC.TestCRCInitCRC16ZMODEM;
var
  CRCDef : TCRCDef;
begin
  CheckEquals(true,  CRCInit(CRCDef, CRC_16ZMODEM), 'CRC_16ZMODEM data not retrieved');
  CheckEquals($1021, CRCDef.Polynomial,             'CRC_16ZMODEM polynome wrong');
  CheckEquals(16,    CRCDef.Bits,                   'CRC_16ZMODEM bit count wrong');
  CheckEquals($0,    CRCDef.InitVector,             'CRC_16ZMODEM init vector wrong');
  CheckEquals(false, CRCDef.Inverse,                'CRC_16ZMODEM inverse Flag wrong');
end;

procedure TestCRC.TestCRCInitCRC16XMODEM;
var
  CRCDef : TCRCDef;
begin
  CheckEquals(true,  CRCInit(CRCDef, CRC_16XMODEM), 'CRC_16XMODEM data not retrieved');
  CheckEquals($8408, CRCDef.Polynomial,             'CRC_16XMODEM polynome wrong');
  CheckEquals(16,    CRCDef.Bits,                   'CRC_16XMODEM bit count wrong');
  CheckEquals($0,    CRCDef.InitVector,             'CRC_16XMODEM init vector wrong');
  CheckEquals(true,  CRCDef.Inverse,                'CRC_16XMODEM inverse Flag wrong');
end;

procedure TestCRC.TestCRCInitCRC16CCITT;
var
  CRCDef : TCRCDef;
begin
  CheckEquals(true,  CRCInit(CRCDef, CRC_16CCITT), 'CRC_16CCITT data not retrieved');
  CheckEquals($1021, CRCDef.Polynomial,            'CRC_16CCITT polynome wrong');
  CheckEquals(16,    CRCDef.Bits,                  'CRC_16CCITT bit count wrong');
  CheckEquals($1D0F, CRCDef.InitVector,            'CRC_16CCITT init vector wrong');
  CheckEquals(false, CRCDef.Inverse,               'CRC_16CCITT inverse Flag wrong');
end;

procedure TestCRC.TestCRCInitCRC16;
var
  CRCDef : TCRCDef;
begin
  // CRC_16 ARC;IBM;MODBUS RTU
  CheckEquals(true,  CRCInit(CRCDef, CRC_16), 'CRC_16 data not retrieved');
  CheckEquals($8005, CRCDef.Polynomial,       'CRC_16 polynome wrong');
  CheckEquals(16,    CRCDef.Bits,             'CRC_16 bit count wrong');
  CheckEquals($0,    CRCDef.InitVector,       'CRC_16 init vector wrong');
  CheckEquals(true,  CRCDef.Inverse,          'CRC_16 inverse Flag wrong');
end;

procedure TestCRC.TestCRCInitCRC12;
var
  CRCDef : TCRCDef;
begin
  CheckEquals(true, CRCInit(CRCDef, CRC_12), 'CRC_12 data not retrieved');
  CheckEquals($80F, CRCDef.Polynomial,       'CRC_12 polynome wrong');
  CheckEquals(12,   CRCDef.Bits,             'CRC_12 bit count wrong');
  CheckEquals($0,   CRCDef.InitVector,       'CRC_12 init vector wrong');
  CheckEquals(true, CRCDef.Inverse,          'CRC_12 inverse Flag wrong');
end;

procedure TestCRC.TestCRCInitCRC10;
var
  CRCDef : TCRCDef;
begin
  CheckEquals(true, CRCInit(CRCDef, CRC_10), 'CRC_10 data not retrieved');
  CheckEquals($233, CRCDef.Polynomial,       'CRC_10 polynome wrong');
  CheckEquals(10,   CRCDef.Bits,             'CRC_10 bit count wrong');
  CheckEquals($0,   CRCDef.InitVector,       'CRC_10 init vector wrong');
  CheckEquals(true, CRCDef.Inverse,          'CRC_10 inverse Flag wrong');
end;

procedure TestCRC.TestCRCInitCRC8;
var
  CRCDef : TCRCDef;
begin
  CheckEquals(true, CRCInit(CRCDef, CRC_8), 'CRC_8 data not retrieved');
  CheckEquals($D1,  CRCDef.Polynomial,      'CRC_8 polynome wrong');
  CheckEquals(8,    CRCDef.Bits,            'CRC_8 bit count wrong');
  CheckEquals($0,   CRCDef.InitVector,      'CRC_8 init vector wrong');
  CheckEquals(true, CRCDef.Inverse,         'CRC_8 inverse Flag wrong');
end;

procedure TestCRC.TestCRCCodeMultiBuffer(Test: TCRCTest; CRCDef: TCRCDef);
var
  InputBuf : TBytes;
  SrcBuf   : TBytes;
  StartIdx : Integer;
  res      : UInt32;
begin
  // fixed length on purpose to require multimple passes of the loop
  SetLength(InputBuf, 4);
  SrcBuf   := BytesOf(Test.Input);

  StartIdx := 0;
  res      := 0;

  while (StartIdx < length(SrcBuf)) do
  begin
    if ((length(SrcBuf) - StartIdx) < length(InputBuf)) then
      SetLength(InputBuf, length(SrcBuf) - StartIdx);

    Move(SrcBuf[StartIdx], InputBuf[0], length(InputBuf));

    res := CRCCode(CRCDef, InputBuf[0], Length(InputBuf));
    inc(StartIdx, length(InputBuf));
  end;

  CheckEquals(Test.CRC, res, 'Wrong result for input: ' + string(test.Input));
end;

procedure TestCRC.TestCRCCodeSingleBuffer(Test: TCRCTest; CRCDef: TCRCDef);
var
  SrcBuf: TArray<System.Byte>;
  res: Cardinal;
begin
  SrcBuf := BytesOf(Test.Input);
  if Length(Test.Input) > 0 then
    res := CRCCode(CRCDef, SrcBuf[0], Length(SrcBuf))
  else
    res := CRCCode(CRCDef, SrcBuf, Length(SrcBuf));
  CheckEquals(Test.CRC, res, 'Wrong result for input: ' + string(test.Input));
end;

procedure TestCRC.TestCRCCodeSingleBufferCallback(Test: TCRCTest; CRCDef: TCRCDef);
var
  res: Cardinal;
begin
  FCallbackBuffer := BytesOf(Test.Input);

  if Length(Test.Input) > 0 then
    res := CRCCode(CRCDef, CRCCodeReadCallback, Length(FCallbackBuffer))
  else
    res := 0;
//  else
//    res := CRCCode(CRCDef, CRCCodeReadCallback, Length(FCallbackBuffer));
  CheckEquals(Test.CRC, res, 'Wrong result for input: ' + string(test.Input));
end;

procedure TestCRC.TestCRCDoneFinalVector0;
var
  CRCDef : TCRCDef;
  CRC    : UInt32;
begin
  // CRCDef initialisieren
  // Poly: $00008005; Bits: 16; Init: $00000000; FInit: $00000000; Inverse: True
  CRCInit(CRCDef, CRC_16);

  // Zwischenwert der CRC Berechnung vordefinieren, da ja nur CRCDone getestet
  // werden soll. Mask ist bei CRC_16 = $FFFF
  CRCDef.CRC := $AAAA;

  CRC := CRCDone(CRCDef);

  CheckEquals($AAAA, CRC, 'falscher CRC Wert');
  CheckEquals(CRCDef.InitVector, CRCDef.CRC, 'Falscher temporärer CRC Wert');
end;

procedure TestCRC.TestCRCDoneFinalVectorFFFFFFFF;
var
  CRCDef : TCRCDef;
  CRC    : UInt32;
begin
  // CRCDef initialisieren
  // Poly: $9DB11213; Bits: 32; Init: $FFFFFFFF; FInit: $FFFFFFFF; Inverse: True
  CRCInit(CRCDef, CRC_32);

  // Zwischenwert der CRC Berechnung vordefinieren, da ja nur CRCDone getestet
  // werden soll. Mask ist bei CRC_32 = $FFFFFFFF
  CRCDef.CRC := $AAAAAAAA;

  CRC := CRCDone(CRCDef);

  // durch den FinalVector $FFFFFFFF und das XOR im CRCDone findet eine
  // Invertierung statt ($55555555 ist Invertierung von $AAAAAAAA
  CheckEquals($55555555, CRC, 'falscher CRC Wert');
  CheckEquals(CRCDef.InitVector, CRCDef.CRC, 'Falscher temporärer CRC Wert');
end;

initialization
  // Register any test cases with the test runner
  {$IFDEF DUnitX}
  TDUnitX.RegisterTestFixture(TestCRC);
  {$ELSE}
  RegisterTest(TestCRC.Suite);
  {$ENDIF}
end.
