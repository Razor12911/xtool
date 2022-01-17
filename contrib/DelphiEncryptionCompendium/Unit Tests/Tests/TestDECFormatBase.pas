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
unit TestDECFormatBase;

interface

// Needs to be included before any other statements
{$INCLUDE TestDefines.inc}

uses
  {$IFDEF DUnitX}
  DUnitX.TestFramework,DUnitX.DUnitCompatibility,
  {$ELSE}
  TestFramework,
  {$ENDIF}
  System.SysUtils, System.Classes,
  DECUtil, DECBaseClass, DECFormatBase;

type
  // Test methods for class TFormat_Copy
  {$IFDEF DUnitX} [TestFixture] {$ENDIF}
  TestTFormat = class(TTestCase)
  strict private
    /// <summary>
    ///   Method needed because CheckException only allows procedure methods and
    ///   not functions as parameter
    /// </summary>
    procedure TestClassByInvalidIdentityHelper;
    /// <summary>
    ///   Method needed because CheckException only allows procedure methods and
    ///   not functions as parameter
    /// </summary>
    procedure TestFormatByInvalidIdentityHelper;
    /// <summary>
    ///   Method needed because CheckException only allows procedure methods and
    ///   not functions as parameter
    /// </summary>
    procedure TestClassByInvalidNameHelperEmpty;
    /// <summary>
    ///   Method needed because CheckException only allows procedure methods and
    ///   not functions as parameter
    /// </summary>
    procedure TestClassByInvalidNameHelperWrong;
    /// <summary>
    ///   Method needed because CheckException only allows procedure methods and
    ///   not functions as parameter
    /// </summary>
    procedure TestFormatByInvalidNameHelperEmpty;
    /// <summary>
    ///   Method needed because CheckException only allows procedure methods and
    ///   not functions as parameter
    /// </summary>
    procedure TestFormatByInvalidNameHelperWrong;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestUpCaseBinary;
    procedure TestTableFindBinary;
    procedure TestIsClassListCreated;

    procedure TestClassByName;
    procedure TestClassByInvalidName;
    procedure TestClassByIdentity;
    procedure TestClassByInvalidIdentity;
    procedure TestValidFormat;
    procedure TestFormatByName;
    procedure TestFormatByInvalidName;
    procedure TestFormatByIdentity;
    procedure TestFormatByInvalidIdentity;
  end;

  // Test methods for class TFormat_Copy
  {$IFDEF DUnitX} [TestFixture] {$ENDIF}
  TestTFormat_Copy = class(TTestCase)
  strict private
    FFormat_Copy: TFormat_Copy;
  private
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
  end;

implementation

uses
  DECFormat;

type
  TestRecTableFindBinary = record
    Value: Byte;
    Table: RawByteString;
    Len  : Integer;
    Index: Integer;
  end;

procedure TestTFormat_Copy.SetUp;
begin
  FFormat_Copy := TFormat_Copy.Create;
end;

procedure TestTFormat_Copy.TearDown;
begin
  FFormat_Copy.Free;
  FFormat_Copy := nil;
end;

procedure TestTFormat_Copy.TestDecodeBytes;
var
  SrcBuf,
  DestBuf : TBytes;
begin
  SrcBuf  := BytesOf(RawByteString('1234567890abcdefghijklmnopqrstuvwxyz@!$'));
  DestBuf := TFormat_Copy.Decode(SrcBuf);

  CheckEquals('1234567890abcdefghijklmnopqrstuvwxyz@!$',
              string(BytesToRawString(DestBuf)));
end;

procedure TestTFormat_Copy.TestDecodeRawByteString;
var
  SrcString,
  DestString : RawByteString;
begin
  SrcString  := '1234567890abcdefghijklmnopqrstuvwxyz@!$';
  DestString := TFormat_Copy.Decode(SrcString);

  CheckEquals(RawByteString('1234567890abcdefghijklmnopqrstuvwxyz@!$'),
              DestString);
end;

procedure TestTFormat_Copy.TestDecodeTypeless;
var
  SrcBuf     : TBytes;
  DestString : RawByteString;
begin
  SrcBuf     := BytesOf(RawByteString('1234567890abcdefghijklmnopqrstuvwxyz@!$'));
  DestString := TFormat_Copy.Encode(SrcBuf[0], length(SrcBuf));

  CheckEquals(RawByteString('1234567890abcdefghijklmnopqrstuvwxyz@!$'),
              DestString);
end;

procedure TestTFormat_Copy.TestEncodeBytes;
var
  SrcBuf,
  DestBuf : TBytes;
begin
  SrcBuf  := BytesOf(RawByteString('1234567890abcdefghijklmnopqrstuvwxyz@!$'));
  DestBuf := TFormat_Copy.Encode(SrcBuf);

  CheckEquals('1234567890abcdefghijklmnopqrstuvwxyz@!$',
              string(BytesToRawString(DestBuf)));
end;

procedure TestTFormat_Copy.TestEncodeRawByteString;
var
  SrcString,
  DestString : RawByteString;
begin
  SrcString  := '1234567890abcdefghijklmnopqrstuvwxyz@!$';
  DestString := TFormat_Copy.Encode(SrcString);

  CheckEquals(RawByteString('1234567890abcdefghijklmnopqrstuvwxyz@!$'),
              DestString);
end;

procedure TestTFormat_Copy.TestEncodeTypeless;
var
  SrcBuf     : TBytes;
  DestString : RawByteString;
begin
  SrcBuf     := BytesOf(RawByteString('1234567890abcdefghijklmnopqrstuvwxyz@!$'));
  DestString := TFormat_Copy.Encode(SrcBuf[0], length(SrcBuf));

  CheckEquals(RawByteString('1234567890abcdefghijklmnopqrstuvwxyz@!$'),
              DestString);
end;

procedure TestTFormat_Copy.TestIsValidRawByteString;
begin
  CheckEquals(true, TFormat_Copy.IsValid(BytesOf('abcdefghijklmnopqrstuvwxyz')));
  CheckEquals(true, TFormat_Copy.IsValid(BytesOf('')));
end;

procedure TestTFormat_Copy.TestIsValidTBytes;
var
  SrcBuf : TBytes;
begin
  SrcBuf  := BytesOf(RawByteString('1234567890abcdefghijklmnopqrstuvwxyz@!$'));
  CheckEquals(true, TFormat_Copy.IsValid(SrcBuf));

  SetLength(SrcBuf, 0);
  CheckEquals(true, TFormat_Copy.IsValid(SrcBuf));
end;

procedure TestTFormat_Copy.TestIsValidTypeless;
var
  SrcBuf : TBytes;
  P      : ^Byte;
begin
  SrcBuf  := BytesOf(RawByteString('1234567890abcdefghijklmnopqrstuvwxyz@!$'));
  CheckEquals(true,  TFormat_Copy.IsValid(SrcBuf[0], Length(SrcBuf)));

  P := nil;
  CheckEquals(true,  TFormat_Copy.IsValid(P^, 0));
  CheckEquals(false, TFormat_Copy.IsValid(SrcBuf[0], -1));
end;

{ TestTFormat }

procedure TestTFormat.SetUp;
begin
  inherited;

end;

procedure TestTFormat.TearDown;
begin
  inherited;

end;

procedure TestTFormat.TestUpCaseBinary;
const
  InputChars  = ' !"#$%&''()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ' +
                '[\]^_`abcdefghijklmnopqrstuvwxyz{|}~';
  OutputChars = ' !"#$%&''()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ' +
                '[\]^_`ABCDEFGHIJKLMNOPQRSTUVWXYZ{|}~';

var
  i : Integer;
  b, exp, res : Byte;
begin
  {$IF CompilerVersion >= 24.0}
  for i := Low(InputChars) to High(InputChars) do
  begin
    b   := ord(InputChars[i]);
    exp := ord(OutputChars[i]);
    res := TDECFormat.UpCaseBinary(b);

    CheckEquals(exp, res);
  end;

  {$ELSE}
  for i := 1 to Length(InputChars) do
  begin
    b   := ord(InputChars[i]);
    exp := ord(OutputChars[i]);
    res := TDECFormat.UpCaseBinary(b);

    CheckEquals(exp, res);
  end;
  {$IFEND}
end;

procedure TestTFormat.TestValidFormat;
var
  result : Boolean;
begin
  result := ValidFormat(nil) = TFormat_Copy;
  CheckEquals(true, result, 'ValidFormat(nil) must be TFormat_Copy');

  result := ValidFormat(TFormat_ESCAPE) = TFormat_ESCAPE;
  CheckEquals(true, result, 'ValidFormat(TFormat_ESCAPE) must be TFormat_ESCAPE');
end;

procedure TestTFormat.TestTableFindBinary;
const
  Data : array[1..8] of TestRecTableFindBinary = (
  (Value: 0;
   Table: '';
   Len:   10;
   Index: -1),
  (Value: 0;
   Table: '';
   Len: -10;
   Index: -1),
  (Value: $31;
   Table: '12345678901';
   Len:   100;
   Index: 0),
  (Value: $32;
   Table: '12345678901';
   Len:   100;
   Index: 1),
  (Value: $30;
   Table: '12345678901';
   Len:   100;
   Index: 9),
  (Value: $29;
   Table: '12345678901';
   Len:   100;
   Index: -1),
  (Value: $30;
   Table: '12345678901';
   Len:   9;
   Index: 9),
  (Value: $30;
   Table: '12345678901';
   Len:   8;
   Index: -1)
  );

var
  i : Integer;
  Idx : Integer;
begin
  for i := Low(Data) to High(Data) do
  begin
    Idx := TDECFormat.TableFindBinary(Data[i].Value,
                                      BytesOf(RawByteString(Data[i].Table)),
                                      Data[i].Len);
    CheckEquals(Data[i].Index, Idx);
  end;
end;

procedure TestTFormat.TestFormatByIdentity;
var
  result : Boolean;
begin
  result := FormatByIdentity(1178647993) = TFormat_Copy;
  CheckEquals(true, result, 'TFormat_Copy must have Identity value 1178647993');

  result := FormatByIdentity(3786628779) = TFormat_HEX;
  CheckEquals(true, result, 'TFormat_HEX must have Identity value 3786628779');

  result := FormatByIdentity(970117517) = TFormat_HEXL;
  CheckEquals(true, result, 'TFormat_HEXL must have Identity value 970117517');
end;

procedure TestTFormat.TestFormatByInvalidIdentity;
begin
  CheckException(TestFormatByInvalidIdentityHelper, EDECClassNotRegisteredException);
end;

procedure TestTFormat.TestFormatByInvalidIdentityHelper;
begin
  FormatByIdentity(0);
end;

procedure TestTFormat.TestClassByIdentity;
var
  result : Boolean;
begin
  result := TDECFormat.ClassByIdentity(1178647993) = TFormat_Copy;
  CheckEquals(true, result, 'TFormat_Copy must have Identity value 1178647993');

  result := TDECFormat.ClassByIdentity(3786628779) = TFormat_HEX;
  CheckEquals(true, result, 'TFormat_HEX must have Identity value 3786628779');

  result := TDECFormat.ClassByIdentity(970117517) = TFormat_HEXL;
  CheckEquals(true, result, 'TFormat_HEXL must have Identity value 970117517');
end;

procedure TestTFormat.TestClassByInvalidIdentity;
begin
  CheckException(TestClassByInvalidIdentityHelper, EDECClassNotRegisteredException);
end;

procedure TestTFormat.TestClassByInvalidIdentityHelper;
begin
  TDECFormat.ClassByIdentity(0);
end;

procedure TestTFormat.TestClassByName;
var
  result : Boolean;
begin
  result := TDECFormat.ClassByName('TFormat_HEX') = TFormat_HEX;
  CheckEquals(true, result, 'Class TFormat_HEX not found');

  result := TDECFormat.ClassByName('TFormat_HEXL') = TFormat_HEXL;
  CheckEquals(true, result, 'Class TFormat_HEXL not found');
end;

procedure TestTFormat.TestClassByInvalidName;
begin
  CheckException(TestClassByInvalidNameHelperEmpty, EDECClassNotRegisteredException);
  CheckException(TestClassByInvalidNameHelperWrong, EDECClassNotRegisteredException);
end;

procedure TestTFormat.TestClassByInvalidNameHelperEmpty;
begin
  TDECFormat.ClassByName('');
end;

procedure TestTFormat.TestClassByInvalidNameHelperWrong;
begin
  TDECFormat.ClassByName('Foo');
end;

procedure TestTFormat.TestFormatByName;
var
  result : Boolean;
begin
  result := FormatByName('TFormat_HEX') = TFormat_HEX;
  CheckEquals(true, result, 'Class TFormat_HEX not found');

  result := FormatByName('TFormat_HEXL') = TFormat_HEXL;
  CheckEquals(true, result, 'Class TFormat_HEXL not found');
end;

procedure TestTFormat.TestFormatByInvalidName;
begin
  CheckException(TestFormatByInvalidNameHelperEmpty, EDECClassNotRegisteredException);
  CheckException(TestFormatByInvalidNameHelperWrong, EDECClassNotRegisteredException);
end;

procedure TestTFormat.TestFormatByInvalidNameHelperEmpty;
begin
  FormatByName('');
end;

procedure TestTFormat.TestFormatByInvalidNameHelperWrong;
begin
  FormatByName('Foo');
end;

procedure TestTFormat.TestIsClassListCreated;
begin
  CheckEquals(true, assigned(TDECFormat.ClassList), 'Class list has not been created in initialization');
end;

initialization
  // Register any test cases with the test runner
  {$IFDEF DUnitX}
  TDUnitX.RegisterTestFixture(TestTFormat);
  TDUnitX.RegisterTestFixture(TestTFormat_Copy);
  {$ELSE}
  RegisterTests('DECFormatBase', [TestTFormat.Suite, TestTFormat_Copy.Suite]);
  {$ENDIF}
end.

