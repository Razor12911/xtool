{*****************************************************************************
  The DEC team (see file NOTICE.txt) licenses this file
  to you under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  A copy of this licence is found in the root directory of
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
unit TestDECCipherFormats;

// Needs to be included before any other statements
{$INCLUDE TestDefines.inc}

interface

uses
  {$IFDEF DUnitX}
  DUnitX.TestFramework,DUnitX.DUnitCompatibility,
  {$ELSE}
  TestFramework,
  {$ENDIF}
  System.Classes, System.SysUtils,
  DECCipherBase, DECCiphers, DECCipherFormats;

type
  /// <summary>
  ///   All known testvectors use the same filler byte and the same cmCTSx mode
  /// </summary>
  TCipherTestData = record
    PlainTextData          : RawByteString;
    EncryptedTextData      : RawByteString;
    EncryptedUTF16TextData : string;

    Key                    : RawByteString;
    InitVector             : RawByteString;
    Filler                 : Byte;
    Mode                   : TCipherMode;
  end;

  // Test methods for class TDECClassList
  {$IFDEF DUnitX} [TestFixture] {$ENDIF}
  TestTDECCipherFormats = class(TTestCase)
  strict private
    FCipherTwoFish : TDECFormattedCipher;

    /// <summary>
    ///   Array with the test data
    /// </summary>
    FTestData      : array of TCipherTestData;

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

    /// <summary>
    ///   Initialization routine which sets the properties of the crypto object
    ///   as specified in the test data record with the given index.
    /// </summary>
    /// <param name="Index">
    ///   Index of the test data record to be used for this test run initialization
    /// </param>
    procedure Init(Index: Integer);


    {$IFDEF ANSISTRINGSUPPORTED}
    /// <summary>
    ///   Copies the bytes of the buffer into an AnsiString
    /// </summary>
    /// <param name="Bytes">
    ///   Byte buffer to be converted to an AnsiString
    /// </param>
    /// <returns>
    ///   AnsiString converted buffer. If the buffer passed has a length of 0
    ///   an empty string will be returned
    /// </returns>
    function AnsiStringOf(const Bytes: TBytes): AnsiString;
    {$ENDIF}
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestEncodeBytes;
    procedure TestDecodeBytes;

    procedure TestEncodeStream;
    procedure TestDecodeStream;

// procedure TestCalculateStringData;

// Currently commented out because it would require a file as external dependency
//    procedure TestEncodeFile(const SourceFileName, DestFileName: string;
//                             const Progress: IDECProgress = nil);
//    procedure TestDecodeFile(const SourceFileName, DestFileName: string;
//                             const Progress: IDECProgress = nil);

    procedure TestEncodeStringToBytes;
    procedure TestEncodeRawByteStringToBytes;

    procedure TestEncodeStringToString;
    procedure TestEncodeRawByteStringToString;

    procedure TestDecodeStringToBytes;
    procedure TestDecodeRawByteStringToBytes;

    procedure TestDecodeStringToString;
    procedure TestDecodeRawByteStringToString;


{$IFDEF ANSISTRINGSUPPORTED}
    procedure TestEncodeAnsiStringToBytes;
    procedure TestEncodeAnsiStringToString;

    procedure TestDecodeAnsiStringToBytes;
    procedure TestDecodeAnsiStringToString;
{$ENDIF}

{$IFNDEF NEXTGEN}
    procedure TestEncodeWideStringToBytes;
    procedure TestEncodeWideStringToString;

    procedure TestDecodeWideStringToBytes;
    procedure TestDecodeWideStringToString;
{$ENDIF}
  end;

implementation

uses
  DECBaseClass, DECFormat, DECUtil;

{ TestTDECCipherFormats }

procedure TestTDECCipherFormats.LimitKeyLength(var Key: RawByteString;
  KeySize: Integer);
begin
  if Length(Key) > KeySize then
    Delete(Key, KeySize + 1, length(Key));
end;

{$IFDEF ANSISTRINGSUPPORTED}
function TestTDECCipherFormats.AnsiStringOf(const Bytes: TBytes): AnsiString;
begin
  if Assigned(Bytes) then
  begin
    SetLength(Result, length(Bytes));
    {$IF CompilerVersion >= 24.0}
    Move(Bytes[0], Result[low(Result)], length(Bytes));
    {$ELSE}
    Move(Bytes[0], Result[1], length(Bytes));
    {$IFEND}
  end
  else
    Result := '';
end;
{$ENDIF}

procedure TestTDECCipherFormats.TestDecodeRawByteStringToBytes;
var
  i      : Integer;
  result : TBytes;
begin
  for i := 0 to High(FTestData) do
  begin
    Init(i);

    result := FCipherTwoFish.DecodeStringToBytes(TFormat_HexL.Decode(FTestData[i].EncryptedTextData));

    CheckEquals(FTestData[i].PlainTextData,
                RawByteString(StringOf(result)),
                'Failure in TestDecodeRawByteStringToBytes ' + IntToStr(i));
  end;
end;

procedure TestTDECCipherFormats.TestDecodeStringToBytes;
var
  i        : Integer;
  result   : TBytes;
  InputStr : string;
  ExpStr   : string;
  ResStr   : string;
begin
  for i := 0 to High(FTestData) do
  begin
    Init(i);

    result := TFormat_HexL.Decode(BytesOf(FTestData[i].EncryptedUTF16TextData));
    InputStr := StringOf(result);

    result := FCipherTwoFish.DecodeStringToBytes(InputStr);
    ResStr := WideStringOf(result);

    ExpStr := string(FTestData[i].PlainTextData);
    CheckEquals(ExpStr, ResStr, 'Failure in TestDecodeStringToBytes ' + IntToStr(i));
  end;
end;

procedure TestTDECCipherFormats.Init(Index: Integer);
begin
  LimitKeyLength(FTestData[Index].Key, FCipherTwoFish.Context.KeySize);

  FCipherTwoFish.Mode := FTestData[Index].Mode;
  FCipherTwoFish.Init(BytesOf(FTestData[Index].Key),
                      BytesOf(FTestData[Index].InitVector),
                      FTestData[Index].Filler);
end;

procedure TestTDECCipherFormats.SetUp;
begin
  FCipherTwoFish := TCipher_Twofish.Create;

  SetLength(FTestData, 1);

  FTestData[0].EncryptedTextData := 'e81674f9bc69442188c949bb52e1e47874171177e99' +
                                    'dbbe9880875094f8dfe21';
  FTestData[0].PlainTextData     := TFormat_ESCAPE.Decode('\x30\x44\xED\x6E\x45\xA4' +
                                                          '\x96\xF5\xF6\x35\xA2\xEB' +
                                                          '\x3D\x1A\x5D\xD6\xCB\x1D' +
                                                          '\x09\x82\x2D\xBD\xF5\x60' +
                                                          '\xC2\xB8\x58\xA1\x91\xF9' +
                                                          '\x81\xB1');
  // In this first test case simply the RawByteString based test data filled up
  // with a 0 in each char to form a UTF16 char
  FTestData[0].EncryptedUTF16TextData := 'ebc6a21d2a7d8341f643a0bf494057d5a5c38f' +
                                         '0ae72bd4ced90b5e6467de24c7d06b88207a41' +
                                         'f9d32126e38ab49024c98788b8619c3cbeb7fa' +
                                         'ad2cd9b7e40480';

  FTestData[0].Key        := 'TCipher_Twofish';
  FTestData[0].InitVector := '';
  FTestData[0].Filler     := $FF;
  FTestData[0].Mode       := cmCTSx;
end;

procedure TestTDECCipherFormats.TearDown;
begin
  FCipherTwoFish.Free;
end;

{$IFDEF ANSISTRINGSUPPORTED}
procedure TestTDECCipherFormats.TestDecodeAnsiStringToBytes;
var
  i      : Integer;
  result : TBytes;
begin
  for i := 0 to High(FTestData) do
  begin
    Init(i);

    result := FCipherTwoFish.DecodeStringToBytes(AnsiString(TFormat_HexL.Decode(FTestData[i].EncryptedTextData)));

    CheckEquals(FTestData[i].PlainTextData,
                AnsiStringOf(result),
                'Failure in TestDecodeAnsiStringToBytes ' + IntToStr(i));
  end;
end;

procedure TestTDECCipherFormats.TestDecodeAnsiStringToString;
var
  i        : Integer;
  result   : AnsiString;
  InputStr : AnsiString;
  StrArr   : TBytes;
begin
  for i := 0 to High(FTestData) do
  begin
    Init(i);

    StrArr := BytesOf(FTestData[i].EncryptedTextData);
    StrArr := TFormat_HexL.Decode(StrArr);
    InputStr := AnsiStringOf(StrArr);

    result := FCipherTwoFish.DecodeStringToString(InputStr);

    CheckEquals(AnsiString(FTestData[i].PlainTextData),
                result,
                'Failure in TestDecodeAnsiStringToString ' + IntToStr(i));
  end;
end;
{$ENDIF}

procedure TestTDECCipherFormats.TestDecodeBytes;
var
  i      : Integer;
  result : TBytes;
begin
  for i := 0 to High(FTestData) do
  begin
    Init(i);

    result := FCipherTwoFish.DecodeBytes(
                BytesOf(TFormat_HexL.Decode(FTestData[i].EncryptedTextData)));

    CheckEquals(FTestData[i].PlainTextData,
                RawByteString(StringOf(result)),
                'Failure in TestDecodeBytes ' + IntToStr(i));
  end;
end;

procedure TestTDECCipherFormats.TestDecodeRawByteStringToString;
var
  i        : Integer;
  result   : RawByteString;
  InputStr : RawByteString;
  StrArr   : TBytes;
begin
  for i := 0 to High(FTestData) do
  begin
    Init(i);

    StrArr := BytesOf(FTestData[i].EncryptedTextData);
    StrArr := TFormat_HexL.Decode(StrArr);
    InputStr := BytesToRawString(StrArr);

    result := FCipherTwoFish.DecodeStringToString(InputStr);

    CheckEquals(FTestData[i].PlainTextData,
                result,
                'Failure in TestDecodeRawByteStringToString ' + IntToStr(i));
  end;
end;

procedure TestTDECCipherFormats.TestDecodeStream;
var
  Src, Dest : TMemoryStream;
  SrcBuf    : TBytes;
  i         : Integer;
  result    : TBytes;
begin
  Src := TMemoryStream.Create;
  try

    Dest := TMemoryStream.Create;
    try

      for i := 0 to High(FTestData) do
      begin
        Init(i);

        SrcBuf := BytesOf(TFormat_HexL.Decode(FTestData[i].EncryptedTextData));

        Src.Clear;
        {$IF CompilerVersion >= 25.0}
        Src.WriteData(SrcBuf, length(SrcBuf));
        {$ELSE}
        Src.Write(SrcBuf[0], Length(SrcBuf));
        {$IFEND}
        Src.Seek(0, TSeekOrigin.soBeginning);

        FCipherTwoFish.DecodeStream(Src, Dest, Src.Size, nil);

        Dest.Seek(0, TSeekOrigin.soBeginning);
        SetLength(result, Dest.Size);
        {$IF CompilerVersion >= 25.0}
        Dest.Read(result, 0, Dest.Size);
        {$ELSE}
        Dest.Read(Result[0], Dest.Size);
        {$IFEND}

        CheckEquals(FTestData[i].PlainTextData,
                    RawByteString(StringOf(result)),
                    'Failure in TestDecodeStream ' + IntToStr(i));
      end;

    finally
      Dest.Free;
    end;

  finally
    Src.Free;
  end;
end;

procedure TestTDECCipherFormats.TestDecodeStringToString;
var
  i        : Integer;
  result   : string;
  InputStr : string;
  StrArr   : TBytes;
begin
  for i := 0 to High(FTestData) do
  begin
    Init(i);

    StrArr := BytesOf(FTestData[i].EncryptedUTF16TextData);
    StrArr := TFormat_HexL.Decode(StrArr);
    InputStr := StringOf(StrArr);

    result := FCipherTwoFish.DecodeStringToString(InputStr);

    CheckEquals(string(FTestData[i].PlainTextData),
                result,
                'Failure in TestDecodeStringToString ' + IntToStr(i));
  end;
end;

{$IFNDEF NEXTGEN}
procedure TestTDECCipherFormats.TestDecodeWideStringToBytes;
var
  i        : Integer;
  result   : TBytes;
  InputStr : WideString;
  ExpStr   : WideString;
  ResStr   : WideString;
begin
  for i := 0 to High(FTestData) do
  begin
    Init(i);

    result := TFormat_HexL.Decode(BytesOf(FTestData[i].EncryptedUTF16TextData));
    InputStr := StringOf(result);

    result := FCipherTwoFish.DecodeStringToBytes(InputStr);
    ResStr := WideStringOf(result);

    ExpStr := string(FTestData[i].PlainTextData);
    CheckEquals(ExpStr, ResStr, 'Failure in TestDecodeWideStringToBytes ' + IntToStr(i));
  end;
end;

procedure TestTDECCipherFormats.TestDecodeWideStringToString;
var
  i        : Integer;
  result   : WideString;
  InputStr : WideString;
  StrArr   : TBytes;
begin
  for i := 0 to High(FTestData) do
  begin
    Init(i);

    StrArr := BytesOf(FTestData[i].EncryptedUTF16TextData);
    StrArr := TFormat_HexL.Decode(StrArr);
    InputStr := StringOf(StrArr);

    result := FCipherTwoFish.DecodeStringToString(InputStr);

    CheckEquals(string(FTestData[i].PlainTextData),
                string(result),
                'Failure in TestDecodeWideStringToString ' + IntToStr(i));
  end;
end;
{$ENDIF}

{$IFDEF NEXTGEN}
procedure TestTDECCipherFormats.TestEncodeAnsiStringToBytes;
var
  i        : Integer;
  result   : TBytes;
  InputStr : AnsiString;
begin
  for i := 0 to High(FTestData) do
  begin
    Init(i);

    InputStr := AnsiString(FTestData[i].PlainTextData);
    result := FCipherTwoFish.EncodeStringToBytes(InputStr);

    CheckEquals(FTestData[i].EncryptedTextData,
                AnsiStringOf(TFormat_HexL.Encode(result)),
                'Failure in TestEncodeAnsiStringToBytes ' + IntToString(i));
  end;
end;

procedure TestTDECCipherFormats.TestEncodeAnsiStringToString;
var
  i        : Integer;
  result   : AnsiString;
  InputStr : AnsiString;
begin
  for i := 0 to High(FTestData) do
  begin
    Init(i);

    InputStr := FTestData[i].PlainTextData;
    result := FCipherTwoFish.EncodeStringToString(InputStr);

    CheckEquals(AnsiString(FTestData[i].EncryptedTextData),
                AnsiStringOf(TFormat_HexL.Encode(BytesOf(result))),
                'Failure in TestEncodeAnsiStringToString ' + IntToString(i));
  end;
end;
{$ENDIF}

procedure TestTDECCipherFormats.TestEncodeBytes;
var
  i      : Integer;
  result : TBytes;
begin
  for i := 0 to High(FTestData) do
  begin
    Init(i);

    result := FCipherTwoFish.EncodeBytes(BytesOf(FTestData[i].PlainTextData));

    CheckEquals(FTestData[i].EncryptedTextData,
                RawByteString(StringOf(TFormat_HexL.Encode(result))),
                'Failure in TestEncodeBytes ' + IntToStr(i));
  end;
end;

procedure TestTDECCipherFormats.TestEncodeRawByteStringToBytes;
var
  i      : Integer;
  result : TBytes;
begin
  for i := 0 to High(FTestData) do
  begin
    Init(i);

    result := FCipherTwoFish.EncodeStringToBytes(FTestData[i].PlainTextData);

    CheckEquals(FTestData[i].EncryptedTextData,
                RawByteString(StringOf(TFormat_HexL.Encode(result))),
                'Failure in TestEncodeRawByteStringToBytes ' + IntToStr(i));
  end;
end;

procedure TestTDECCipherFormats.TestEncodeRawByteStringToString;
var
  i        : Integer;
  result   : RawByteString;
  InputStr : RawByteString;
begin
  for i := 0 to High(FTestData) do
  begin
    Init(i);

    InputStr := FTestData[i].PlainTextData;
    result := FCipherTwoFish.EncodeStringToString(InputStr);

    CheckEquals(FTestData[i].EncryptedTextData,
                BytesToRawString(TFormat_HexL.Encode(BytesOf(result))),
                'Failure in TestEncodeRawByteStringToString ' + IntToStr(i));
  end;
end;

procedure TestTDECCipherFormats.TestEncodeStream;
var
  Src, Dest : TMemoryStream;
  SrcBuf    : TBytes;
  i         : Integer;
  result    : TBytes;
begin
  Src := TMemoryStream.Create;
  try

    Dest := TMemoryStream.Create;
    try

      for i := 0 to High(FTestData) do
      begin
        Init(i);

        SrcBuf := BytesOf(FTestData[i].PlainTextData);

        Src.Clear;
        {$IF CompilerVersion >= 25.0}
        Src.WriteData(SrcBuf, length(SrcBuf));
        {$ELSE}
        Src.Write(SrcBuf[0], Length(SrcBuf));
        {$IFEND}
        Src.Seek(0, TSeekOrigin.soBeginning);

        FCipherTwoFish.EncodeStream(Src, Dest, Src.Size, nil);

        Dest.Seek(0, TSeekOrigin.soBeginning);
        SetLength(result, Dest.Size);
        {$IF CompilerVersion >= 25.0}
        Dest.Read(result, 0, Dest.Size);
        {$ELSE}
        Dest.Read(Result[0], Dest.Size);
        {$IFEND}

        CheckEquals(FTestData[i].EncryptedTextData,
                    RawByteString(StringOf(TFormat_HexL.Encode(result))),
                    'Failure in TestEncodeStream ' + IntToStr(i));
      end;

    finally
      Dest.Free;
    end;

  finally
    Src.Free;
  end;
end;

procedure TestTDECCipherFormats.TestEncodeStringToBytes;
var
  i        : Integer;
  result   : TBytes;
  InputStr : string;
begin
  for i := 0 to High(FTestData) do
  begin
    Init(i);

    InputStr := string(FTestData[i].PlainTextData);
    result := FCipherTwoFish.EncodeStringToBytes(InputStr);

    CheckEquals(FTestData[i].EncryptedUTF16TextData,
                string(RawByteString(StringOf(TFormat_HexL.Encode(result)))),
                'Failure in TestEncodeStringToBytes ' + IntToStr(i));
  end;
end;

procedure TestTDECCipherFormats.TestEncodeStringToString;
var
  i        : Integer;
  result   : string;
  InputStr : string;
begin
  for i := 0 to High(FTestData) do
  begin
    Init(i);

    InputStr := string(FTestData[i].PlainTextData);
    result := FCipherTwoFish.EncodeStringToString(InputStr);

    CheckEquals(FTestData[i].EncryptedUTF16TextData,
                StringOf(TFormat_HexL.Encode(BytesOf(result))),
                'Failure in TestEncodeStringToString ' + IntToStr(i));
  end;
end;

{$IFNDEF NEXTGEN}
procedure TestTDECCipherFormats.TestEncodeWideStringToBytes;
var
  i        : Integer;
  result   : TBytes;
  InputStr : WideString;
begin
  for i := 0 to High(FTestData) do
  begin
    Init(i);

    InputStr := WideString(FTestData[i].PlainTextData);
    result := FCipherTwoFish.EncodeStringToBytes(InputStr);

    CheckEquals(FTestData[i].EncryptedUTF16TextData,
                string(RawByteString(StringOf(TFormat_HexL.Encode(result)))),
                'Failure in TestEncodeWideStringToBytes ' + IntToStr(i));
  end;
end;

procedure TestTDECCipherFormats.TestEncodeWideStringToString;
var
  i        : Integer;
  result   : WideString;
  InputStr : WideString;
begin
  for i := 0 to High(FTestData) do
  begin
    Init(i);

    InputStr := WideString(FTestData[i].PlainTextData);
    result := FCipherTwoFish.EncodeStringToString(InputStr);

    CheckEquals(FTestData[i].EncryptedUTF16TextData,
                StringOf(TFormat_HexL.Encode(BytesOf(result))),
                'Failure in TestEncodeWideStringToString ' + IntToStr(i));
  end;
end;
{$ENDIF}

initialization
  // Register any test cases with the test runner
  {$IFNDEF DUnitX}
  RegisterTest(TestTDECCipherFormats.Suite);
  {$ELSE}
  TDUnitX.RegisterTestFixture(TestTDECCipherFormats);
  {$ENDIF}
end.
