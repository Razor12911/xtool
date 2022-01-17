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
unit TestDECUtil;

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
  DECUtil;

type
  TTestBitTwiddling = class(TTestCase)
  published
    procedure ReverseBits32;
    procedure ReverseBits8;
    procedure SwapBytes;
    procedure SwapLong;
    procedure SwapLongBuffer;
    procedure SwapInt64;
    procedure SwapInt64Buffer;
    procedure XORBuffers;
  end;

  TTestBufferProtection = class(TTestCase)
  published
    procedure ProtectBuffer;
    procedure ProtectStream;
    procedure ProtectStreamPartial;
    procedure ProtectBytes;
    procedure ProtectString;
    {$IFDEF ANSISTRINGSUPPORTED}
    procedure ProtectStringAnsi;
    {$ENDIF}
    {$IFNDEF NextGen}
    procedure ProtectStringWide;
    {$ENDIF}
    {$IFDEF MSWINDOWS}
    procedure ProtectStringRawByteString;
    {$ENDIF}
    procedure BytesToRawString;
    procedure BytesToRawStringEmpty;
  end;

implementation

type
  TestRecUInt8 = record
    Input  : UInt8;
    Result : UInt8;
  end;

  TestRecCardinal = record
    Input: Cardinal;
    Result: Cardinal;
  end;

  TestRecInt64 = record
    Input : Int64;
    Result: Int64;
  end;

procedure TTestBitTwiddling.ReverseBits32;
const
  ReverseBitArray: array[0..4] of TestRecCardinal = (
    (Input: 0;           Result: 0),
    (Input: 256;         Result: 8388608),
    (Input: 1024;        Result: 2097152),
    (Input: 65536;       Result: 32768),
    (Input: 4294967295;  Result: 4294967295)
  );
var
  i: Integer;
begin
  for i := 0 to Length(ReverseBitArray) - 1 do
  begin
    CheckEquals(ReverseBitArray[i].Result, DECUtil.ReverseBits(ReverseBitArray[i].Input));
  end;
end;

procedure TTestBitTwiddling.ReverseBits8;
const
  ReverseBitArray: array[0..4] of TestRecUInt8 = (
    (Input: 0;           Result: 0),
    (Input: 1;           Result: 128),
    (Input: 255;         Result: 255),
    (Input: 10;          Result: $50),
    (Input: 11;          Result: $D0)
  );
var
  i: Integer;
begin
  for i := 0 to Length(ReverseBitArray) - 1 do
  begin
    CheckEquals(ReverseBitArray[i].Result, DECUtil.ReverseBits(ReverseBitArray[i].Input));
  end;
end;

procedure TTestBitTwiddling.SwapBytes;
const
  Input: RawByteString  = '0123456789';
  Output: RawByteString = '9876543210';
var
  s: RawByteString;
  c: Cardinal;
begin
  s := Input;
  {$IF CompilerVersion >= 24.0}
  DECUtil.SwapBytes(s[Low(s)], Length(s));
  {$ELSE}
  DECUtil.SwapBytes(s[1], Length(s));
  {$IFEND}
  CheckEquals(Output, s);

  {$IF CompilerVersion >= 24.0}
  DECUtil.SwapBytes(s[Low(s)], Length(s));
  {$ELSE}
  DECUtil.SwapBytes(s[1], Length(s));
  {$IFEND}
  CheckEquals(Input, s);

  c := 123456789;
  DECUtil.SwapBytes(c, SizeOf(UInt32));
  CheckEquals(365779719, c);

  c := High(Cardinal);
  DECUtil.SwapBytes(c, SizeOf(UInt32));
  CheckEquals(4294967295, c);
end;

procedure TTestBitTwiddling.SwapLong;
const
  SwapLongArray: array[0..4] of TestRecCardinal = (
    (Input: 0;           Result: 0),
    (Input: 256;         Result: 65536),
    (Input: 1024;        Result: 262144),
    (Input: 65536;       Result: 256),
    (Input: 4294967295;  Result: 4294967295)
  );
var
  i: Integer;
begin
  for i := 0 to Length(SwapLongArray) - 1 do
    CheckEquals(SwapLongArray[i].Result, DECUtil.SwapUInt32(SwapLongArray[i].Input));
end;

procedure TTestBitTwiddling.SwapLongBuffer;
const
  SwapLongArray: array[0..4] of TestRecCardinal = (
    (Input: 0;           Result: 0),
    (Input: 256;         Result: 65536),
    (Input: 1024;        Result: 262144),
    (Input: 65536;       Result: 256),
    (Input: 4294967295;  Result: 4294967295)
  );

var
  SrcBuf  : array[0..length(SwapLongArray)] of UInt32;
  DestBuf : array[0..length(SwapLongArray)] of UInt32;
  i       : Integer;
begin
  for i := Low(SwapLongArray) to High(SwapLongArray) do
    SrcBuf[i] := SwapLongArray[i].Input;

  DECUtil.SwapUInt32Buffer(SrcBuf, DestBuf, Length(SrcBuf));

  for i := Low(SwapLongArray) to High(SwapLongArray) do
    CheckEquals(SwapLongArray[i].Result, DestBuf[i]);
end;

procedure TTestBitTwiddling.SwapInt64;
const
  // Intel CPU is Little Endian
  SwapInt64Array: array[0..6] of TestRecInt64 = (
    (Input: 0;           Result: 0),
    (Input: 1;           Result: 72057594037927936),  // 2^56
    (Input: 2;           Result: 144115188075855872), // 2^57
    (Input: 256;         Result: 281474976710656),    // 2^48
    (Input: 65536;       Result: 1099511627776),      // 2^40
    (Input: 16777216;    Result: 4294967296),         // 2^32
    (Input: -1;          Result: -1)
  );

var
  i : Integer;
begin
  for i := Low(SwapInt64Array) to High(SwapInt64Array) do
    CheckEquals(SwapInt64Array[i].Result, DECUtil.SwapInt64(SwapInt64Array[i].Input));
end;

procedure TTestBitTwiddling.SwapInt64Buffer;
const
  // Intel CPU is Little Endian
  SwapInt64Array: array[0..6] of TestRecInt64 = (
    (Input: 0;           Result: 0),
    (Input: 1;           Result: 72057594037927936),  // 2^56
    (Input: 2;           Result: 144115188075855872), // 2^57
    (Input: 256;         Result: 281474976710656),    // 2^48
    (Input: 65536;       Result: 1099511627776),      // 2^40
    (Input: 16777216;    Result: 4294967296),         // 2^32
    (Input: -1;          Result: -1)
  );

var
  SrcBuf  : array[0..length(SwapInt64Array)] of Int64;
  DestBuf : array[0..length(SwapInt64Array)] of Int64;
  i       : Integer;
begin
  for i := Low(SwapInt64Array) to High(SwapInt64Array) do
    SrcBuf[i] := SwapInt64Array[i].Input;

  DECUtil.SwapInt64Buffer(SrcBuf, DestBuf, Length(SrcBuf));

  for i := Low(SwapInt64Array) to High(SwapInt64Array) do
    CheckEquals(SwapInt64Array[i].Result, DestBuf[i]);
end;

procedure TTestBitTwiddling.XORBuffers;
type
  UInt32Rec = packed record
    case Integer of
      0: (UInt32: UInt32);
      1: (Bytes: array [0..3] of Byte);
  end;

var
  LBuf, RBuf : TBytes;
  DestBuf    : TBytes;
  CheckBuf   :UInt32Rec;
  i          : Integer;
begin
  SetLength(LBuf, 4);
  SetLength(RBuf, 4);
  SetLength(DestBuf, 4);

  for i := 0 to 3 do
  begin
    LBuf[i] := i;
    RBuf[i] := i;
  end;

  DECUtil.XORBuffers(LBuf[0], RBuf[0], Length(LBuf), DestBuf[0]);

  for i := Low(DestBuf) to High(DestBuf) do
    CheckBuf.Bytes[i] := DestBuf[i];

  CheckEquals(0, CheckBuf.UInt32);

  SetLength(LBuf, 4);
  SetLength(RBuf, 4);
  SetLength(DestBuf, 4);

  for i := 0 to 3 do
  begin
    LBuf[i] := i;
    RBuf[i] := 0;
  end;

  DECUtil.XORBuffers(LBuf[0], RBuf[0], Length(LBuf), DestBuf[0]);

  for i := Low(DestBuf) to High(DestBuf) do
    CheckBuf.Bytes[i] := DestBuf[i];

  CheckEquals(50462976, CheckBuf.UInt32);
end;

procedure TTestBufferProtection.ProtectBuffer;
var
  Buf : TBytes;
  i   : Integer;
begin
  SetLength(Buf, 12);
  for i := $40 to $40 + Length(Buf) - 1 do
    Buf[i-$40] := i;

  DECUtil.ProtectBuffer(Buf[0], Length(Buf));
  CheckEquals(#$00+#$00+#$00+#$00+#$00+#$00+#$00+#$00+#$00+#$00+#$00+#$00,
              string(DECUtil.BytesToRawString(Buf)));
end;

procedure TTestBufferProtection.ProtectStream;
var
  Stream  : TMemoryStream;
  SrcBuf  : TBytes;
  DestBuf : TBytes;
  i       : Integer;
begin
  SetLength(SrcBuf, 12);
  for i := $40 to $40 + Length(SrcBuf) - 1 do
    SrcBuf[i-$40] := i;

  SetLength(DestBuf, Length(SrcBuf));

  Stream := TMemoryStream.Create;
  try
    Stream.Write(SrcBuf[0], Length(SrcBuf));
    Stream.Position := 0;
    DECUtil.ProtectStream(Stream, Stream.Size);

    Stream.Read(DestBuf[0], Stream.Size);
    CheckEquals(#$00+#$00+#$00+#$00+#$00+#$00+#$00+#$00+#$00+#$00+#$00+#$00,
                string(DECUtil.BytesToRawString(DestBuf)));
  finally
    Stream.Free;
  end;
end;

procedure TTestBufferProtection.ProtectStreamPartial;
var
  Stream  : TMemoryStream;
  SrcBuf  : TBytes;
  DestBuf : TBytes;
  i       : Integer;
begin
  SetLength(SrcBuf, 12);
  for i := $40 to $40 + Length(SrcBuf) - 1 do
    SrcBuf[i-$40] := i;

  SetLength(DestBuf, Length(SrcBuf));

  Stream := TMemoryStream.Create;
  try
    Stream.Write(SrcBuf[0], Length(SrcBuf));
    Stream.Position := 0;
    DECUtil.ProtectStream(Stream, 2);

    Stream.Read(DestBuf[0], Stream.Size);
    CheckEquals(#$42+#$43+#$44+#$45+#$46+#$47+#$48+#$49+#$4A+#$4B+#$00+#$00,
                string(DECUtil.BytesToRawString(DestBuf)));
  finally
    Stream.Free;
  end;
end;

procedure TTestBufferProtection.ProtectBytes;
var
  Buf : TBytes;
  i   : Integer;
begin
  SetLength(Buf, 12);
  for i := $40 to $40 + Length(Buf) - 1 do
    Buf[i-$40] := i;

  DecUtil.ProtectBytes(Buf);

  CheckEquals('', string(DECUtil.BytesToRawString(Buf)));
end;

procedure TTestBufferProtection.ProtectString;
var
  s : string;
begin
  s := 'Hello';
  DECUtil.ProtectString(s);

  CheckEquals('', s);
end;

{$IFDEF ANSISTRINGSUPPORTED}
procedure TTestBufferProtection.ProtectStringAnsi;
var
  s : AnsiString;
begin
  s := 'Hello';
  DECUtil.ProtectString(s);

  CheckEquals('', string(s));
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
procedure TTestBufferProtection.ProtectStringRawByteString;
var
  s : RawByteString;
begin
  s := 'Hello';
  DECUtil.ProtectString(s);

  CheckEquals('', string(s));
end;
{$ENDIF}

{$IFNDEF NextGen}
procedure TTestBufferProtection.ProtectStringWide;
var
  s : WideString;
begin
  s := 'Hello';
  DECUtil.ProtectString(s);

  CheckEquals('', string(s));
end;
{$ENDIF}

procedure TTestBufferProtection.BytesToRawString;
var
  Buf: TBytes;
  i  : Integer;
  result : RawByteString;
begin
  SetLength(Buf, 43);
  for i := 48 to 48 + length(Buf) - 1 do
    Buf[i-48] := i;

  result := DECUtil.BytesToRawString(Buf);

  CheckEquals('0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ',
              string(result));
end;

procedure TTestBufferProtection.BytesToRawStringEmpty;
var
  Buf: TBytes;
begin
  SetLength(Buf, 0);

  CheckEquals('', string(DECUtil.BytesToRawString(Buf)));
end;

initialization
  // Register any test cases with the test runner
  {$IFDEF DUnitX}
  TDUnitX.RegisterTestFixture(TTestBitTwiddling);
  TDUnitX.RegisterTestFixture(TTestBufferProtection);
  {$ELSE}
  RegisterTests('DECUtil', [TTestBitTwiddling.Suite, TTestBufferProtection.Suite]);
  {$ENDIF}
end.
