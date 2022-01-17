{ ****************************************************************************** }
{ * delphi:string fpc:UnicodeString               by QQ 600585@qq.com          * }
{ * https://zpascal.net                                                        * }
{ * https://github.com/PassByYou888/zAI                                        * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/PascalString                               * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zChinese                                   * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/FFMPEG-Header                              * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/InfiniteIoT                                * }
{ * https://github.com/PassByYou888/FastMD5                                    * }
{ ****************************************************************************** }

unit UPascalStrings;

{$INCLUDE zDefine.inc}

interface


uses CoreClasses, PascalStrings;

type
{$IFDEF FPC}
  USystemChar = UnicodeChar;
  USystemString = UnicodeString;
{$ELSE FPC}
  USystemChar = PascalStrings.SystemChar;
  USystemString = PascalStrings.SystemString;
{$ENDIF FPC}
  PUSystemString = ^USystemString;
  PUPascalString = ^TUPascalString;
  TUArrayChar = array of USystemChar;
  TUOrdChar = (uc0to9, uc1to9, uc0to32, uc0to32no10, ucLoAtoF, ucHiAtoF, ucLoAtoZ, ucHiAtoZ, ucHex, ucAtoF, ucAtoZ, ucVisibled);
  TUOrdChars = set of TUOrdChar;
  TUHash = Cardinal;
  TUHash64 = UInt64;

  TUPascalString = record
  private
    function GetText: USystemString;
    procedure SetText(const Value: USystemString);
    function GetLen: Integer;
    procedure SetLen(const Value: Integer);
    function GetChars(index: Integer): USystemChar;
    procedure SetChars(index: Integer; const Value: USystemChar);
    function GetBytes: TBytes;
    procedure SetBytes(const Value: TBytes);
    function GetPlatformBytes: TBytes;
    procedure SetPlatformBytes(const Value: TBytes);
    function GetANSI: TBytes;
    procedure SetANSI(const Value: TBytes);
    function GetLast: USystemChar;
    procedure SetLast(const Value: USystemChar);
    function GetFirst: USystemChar;
    procedure SetFirst(const Value: USystemChar);
    function GetUpperChar(index: Integer): USystemChar;
    procedure SetUpperChar(index: Integer; const Value: USystemChar);
    function GetLowerChar(index: Integer): USystemChar;
    procedure SetLowerChar(index: Integer; const Value: USystemChar);
  public
    buff: TUArrayChar;

{$IFDEF DELPHI}
    class operator Equal(const Lhs, Rhs: TUPascalString): Boolean;
    class operator NotEqual(const Lhs, Rhs: TUPascalString): Boolean;
    class operator GreaterThan(const Lhs, Rhs: TUPascalString): Boolean;
    class operator GreaterThanOrEqual(const Lhs, Rhs: TUPascalString): Boolean;
    class operator LessThan(const Lhs, Rhs: TUPascalString): Boolean;
    class operator LessThanOrEqual(const Lhs, Rhs: TUPascalString): Boolean;

    class operator Add(const Lhs, Rhs: TUPascalString): TUPascalString;
    class operator Add(const Lhs: USystemString; const Rhs: TUPascalString): TUPascalString;
    class operator Add(const Lhs: TUPascalString; const Rhs: USystemString): TUPascalString;
    class operator Add(const Lhs: USystemChar; const Rhs: TUPascalString): TUPascalString;
    class operator Add(const Lhs: TUPascalString; const Rhs: USystemChar): TUPascalString;

    class operator Implicit(Value: RawByteString): TUPascalString;
    class operator Implicit(Value: TPascalString): TUPascalString;
    class operator Implicit(Value: USystemString): TUPascalString;
    class operator Implicit(Value: USystemChar): TUPascalString;
    class operator Implicit(Value: TUPascalString): USystemString;
    class operator Implicit(Value: TUPascalString): Variant;

    class operator Explicit(Value: TUPascalString): RawByteString;
    class operator Explicit(Value: TUPascalString): TPascalString;
    class operator Explicit(Value: TUPascalString): USystemString;
    class operator Explicit(Value: TUPascalString): Variant;
    class operator Explicit(Value: USystemString): TUPascalString;
    class operator Explicit(Value: Variant): TUPascalString;
    class operator Explicit(Value: USystemChar): TUPascalString;
{$ENDIF}
    function Copy(index, Count: NativeInt): TUPascalString;
    function Same(const p: PUPascalString): Boolean; overload;
    function Same(const t: TUPascalString): Boolean; overload;
    function Same(const t1, t2: TUPascalString): Boolean; overload;
    function Same(const t1, t2, t3: TUPascalString): Boolean; overload;
    function Same(const t1, t2, t3, t4: TUPascalString): Boolean; overload;
    function Same(const t1, t2, t3, t4, t5: TUPascalString): Boolean; overload;
    function Same(const IgnoreCase: Boolean; const t: TUPascalString): Boolean; overload;
    function ComparePos(const Offset: Integer; const p: PUPascalString): Boolean; overload;
    function ComparePos(const Offset: Integer; const t: TUPascalString): Boolean; overload;
    function GetPos(const s: TUPascalString; const Offset: Integer = 1): Integer; overload;
    function GetPos(const s: PUPascalString; const Offset: Integer = 1): Integer; overload;
    function Exists(c: USystemChar): Boolean; overload;
    function Exists(c: array of USystemChar): Boolean; overload;
    function Exists(const s: TUPascalString): Boolean; overload;
    function GetCharCount(c: USystemChar): Integer;

    function hash: TUHash;
    function Hash64: TUHash64;

    property Last: USystemChar read GetLast write SetLast;
    property First: USystemChar read GetFirst write SetFirst;

    procedure DeleteLast;
    procedure DeleteFirst;
    procedure Delete(idx, cnt: Integer);
    procedure Clear;
    procedure Append(t: TUPascalString); overload;
    procedure Append(c: USystemChar); overload;
    procedure Append(const Fmt: SystemString; const Args: array of const); overload;
    function GetString(bPos, ePos: NativeInt): TUPascalString;
    procedure Insert(AText: USystemString; idx: Integer);
    procedure FastAsText(var output: USystemString);
    procedure FastGetBytes(var output: TBytes);
    property Text: USystemString read GetText write SetText;
    function LowerText: USystemString;
    function UpperText: USystemString;
    function Invert: TUPascalString;
    function TrimChar(const Chars: TUPascalString): TUPascalString;
    function DeleteChar(const Chars: TUPascalString): TUPascalString; overload;
    function DeleteChar(const Chars: TUOrdChars): TUPascalString; overload;
    function ReplaceChar(const Chars: TUPascalString; const newChar: USystemChar): TUPascalString; overload;
    function ReplaceChar(const Chars, newChar: USystemChar): TUPascalString; overload;
    function ReplaceChar(const Chars: TUOrdChars; const newChar: USystemChar): TUPascalString; overload;

    function BuildPlatformPChar: Pointer;
    class procedure FreePlatformPChar(p: Pointer); static;

    class function RandomString(L_: Integer): TUPascalString; static;

    { https://en.wikipedia.org/wiki/Smith%E2%80%93Waterman_algorithm }
    function SmithWaterman(const p: PUPascalString): Double; overload;
    function SmithWaterman(const s: TUPascalString): Double; overload;

    property Len: Integer read GetLen write SetLen;
    property L: Integer read GetLen write SetLen;
    property Chars[index: Integer]: USystemChar read GetChars write SetChars; default;
    property UpperChar[index: Integer]: USystemChar read GetUpperChar write SetUpperChar;
    property LowerChar[index: Integer]: USystemChar read GetLowerChar write SetLowerChar;
    property Bytes: TBytes read GetBytes write SetBytes;                         // UTF8
    property PlatformBytes: TBytes read GetPlatformBytes write SetPlatformBytes; // system default
    property ANSI: TBytes read GetANSI write SetANSI;                            // Ansi Bytes
    function BOMBytes: TBytes;
  end;

  TUArrayPascalString = array of TUPascalString;
  PUArrayPascalString = ^TUArrayPascalString;

  TUArrayPascalStringPtr = array of PUPascalString;
  PUArrayPascalStringPtr = ^TUArrayPascalStringPtr;

  TUPStr = TUPascalString;

function UCharIn(c: USystemChar; const SomeChars: array of USystemChar): Boolean; overload;
function UCharIn(c: USystemChar; const SomeChar: USystemChar): Boolean; overload;
function UCharIn(c: USystemChar; const s: TUPascalString): Boolean; overload;
function UCharIn(c: USystemChar; const p: PUPascalString): Boolean; overload;
function UCharIn(c: USystemChar; const SomeCharsets: TUOrdChars): Boolean; overload;
function UCharIn(c: USystemChar; const SomeCharset: TUOrdChar): Boolean; overload;
function UCharIn(c: USystemChar; const SomeCharsets: TUOrdChars; const SomeChars: TUPascalString): Boolean; overload;
function UCharIn(c: USystemChar; const SomeCharsets: TUOrdChars; const p: PUPascalString): Boolean; overload;

function UFastHashPSystemString(const s: PSystemString): TUHash; overload;
function UFastHash64PSystemString(const s: PSystemString): TUHash64; overload;

function UFastHashSystemString(const s: SystemString): TUHash; overload;
function UFastHash64SystemString(const s: SystemString): TUHash64; overload;

function UFastHashPPascalString(const s: PPascalString): TUHash;
function UFastHash64PPascalString(const s: PPascalString): TUHash64;

function UFormat(const Fmt: USystemString; const Args: array of const): USystemString;

{$IFDEF FPC}

operator := (const s: Variant)r: TUPascalString;
operator := (const s: AnsiString)r: TUPascalString;
operator := (const s: RawByteString)r: TUPascalString;
operator := (const s: UnicodeString)r: TUPascalString;
operator := (const s: WideString)r: TUPascalString;
operator := (const s: ShortString)r: TUPascalString;
operator := (const c: USystemChar)r: TUPascalString;
operator := (const c: TPascalString)r: TUPascalString;

operator := (const s: TUPascalString)r: AnsiString;
operator := (const s: TUPascalString)r: RawByteString;
operator := (const s: TUPascalString)r: UnicodeString;
operator := (const s: TUPascalString)r: WideString;
operator := (const s: TUPascalString)r: ShortString;
operator := (const s: TUPascalString)r: Variant;
operator := (const s: TUPascalString)r: TPascalString;

operator = (const a: TUPascalString; const b: TUPascalString): Boolean;
operator <> (const a: TUPascalString; const b: TUPascalString): Boolean;
operator > (const a: TUPascalString; const b: TUPascalString): Boolean;
operator >= (const a: TUPascalString; const b: TUPascalString): Boolean;
operator < (const a: TUPascalString; const b: TUPascalString): Boolean;
operator <= (const a: TUPascalString; const b: TUPascalString): Boolean;

operator + (const a: TUPascalString; const b: TUPascalString): TUPascalString;
operator + (const a: TUPascalString; const b: USystemString): TUPascalString;
operator + (const a: USystemString; const b: TUPascalString): TUPascalString;
operator + (const a: TUPascalString; const b: USystemChar): TUPascalString;
operator + (const a: USystemChar; const b: TUPascalString): TUPascalString;

{$ENDIF}

{ https://en.wikipedia.org/wiki/Smith%E2%80%93Waterman_algorithm }

// short string likeness and out diff
function USmithWatermanCompare(const seq1, seq2: PUPascalString; var diff1, diff2: TUPascalString;
  const NoDiffChar: Boolean; const diffChar: USystemChar): Double; overload;
function USmithWatermanCompare(const seq1, seq2: PUPascalString; var diff1, diff2: TUPascalString): Double; overload;
function USmithWatermanCompare(const seq1, seq2: TUPascalString; var diff1, diff2: TUPascalString;
  const NoDiffChar: Boolean; const diffChar: USystemChar): Double; overload;
function USmithWatermanCompare(const seq1, seq2: TUPascalString; var diff1, diff2: TUPascalString): Double; overload;

// short string likeness
function USmithWatermanCompare(const seq1, seq2: PUPascalString; out Same, Diff: Integer): Double; overload;
function USmithWatermanCompare(const seq1, seq2: PUPascalString): Double; overload;
function USmithWatermanCompare(const seq1, seq2: TUPascalString): Double; overload;
function USmithWatermanCompare(const seq1: TUArrayPascalString; const seq2: TUPascalString): Double; overload;

// memory likeness
function USmithWatermanCompare(const seq1: Pointer; siz1: Integer; const seq2: Pointer; siz2: Integer;
  out Same, Diff: Integer): Double; overload;
function USmithWatermanCompare(const seq1: Pointer; siz1: Integer; const seq2: Pointer; siz2: Integer): Double; overload;

// long string likeness
function USmithWatermanCompareLongString(const t1, t2: TUPascalString; const MinDiffCharWithPeerLine: Integer; out Same, Diff: Integer): Double; overload;
function USmithWatermanCompareLongString(const t1, t2: TUPascalString): Double; overload;

var
  USystemCharSize: NativeInt = SizeOf(USystemChar);
{$IFDEF CPU64}
  UMaxSmithWatermanMatrix: NativeInt = 10000 * 10;
{$ELSE}
  UMaxSmithWatermanMatrix: NativeInt = 8192;
{$ENDIF}


const
{$IFDEF FirstCharInZero}
  UFirstCharPos = 0;
{$ELSE}
  UFirstCharPos = 1;
{$ENDIF}

implementation

uses SysUtils, Variants;

procedure CombineCharsPP(const c1, c2: TUArrayChar; var output: TUArrayChar);
var
  LL, rl: Integer;
begin
  LL := length(c1);
  rl := length(c2);
  SetLength(output, LL + rl);
  if LL > 0 then
      CopyPtr(@c1[0], @output[0], LL * USystemCharSize);
  if rl > 0 then
      CopyPtr(@c2[0], @output[LL], rl * USystemCharSize);
end;

procedure CombineCharsSP(const c1: USystemString; const c2: TUArrayChar; var output: TUArrayChar);
var
  LL, rl: Integer;
begin
  LL := length(c1);
  rl := length(c2);
  SetLength(output, LL + rl);
  if LL > 0 then
      CopyPtr(@c1[UFirstCharPos], @output[0], LL * USystemCharSize);
  if rl > 0 then
      CopyPtr(@c2[0], @output[LL], rl * USystemCharSize);
end;

procedure CombineCharsPS(const c1: TUArrayChar; const c2: USystemString; var output: TUArrayChar);
var
  LL, rl: Integer;
begin
  LL := length(c1);
  rl := length(c2);
  SetLength(output, LL + rl);
  if LL > 0 then
      CopyPtr(@c1[0], @output[0], LL * USystemCharSize);
  if rl > 0 then
      CopyPtr(@c2[UFirstCharPos], @output[LL], rl * USystemCharSize);
end;

procedure CombineCharsCP(const c1: USystemChar; const c2: TUArrayChar; var output: TUArrayChar);
var
  rl: Integer;
begin
  rl := length(c2);
  SetLength(output, rl + 1);
  output[0] := c1;
  if rl > 0 then
      CopyPtr(@c2[0], @output[1], rl * USystemCharSize);
end;

procedure CombineCharsPC(const c1: TUArrayChar; const c2: USystemChar; var output: TUArrayChar);
var
  LL: Integer;
begin
  LL := length(c1);
  SetLength(output, LL + 1);
  if LL > 0 then
      CopyPtr(@c1[0], @output[0], LL * USystemCharSize);
  output[LL] := c2;
end;

function UCharIn(c: USystemChar; const SomeChars: array of USystemChar): Boolean;
var
  AChar: USystemChar;
begin
  Result := True;
  for AChar in SomeChars do
    if AChar = c then
        Exit;
  Result := False;
end;

function UCharIn(c: USystemChar; const SomeChar: USystemChar): Boolean;
begin
  Result := c = SomeChar;
end;

function UCharIn(c: USystemChar; const s: TUPascalString): Boolean;
begin
  Result := s.Exists(c);
end;

function UCharIn(c: USystemChar; const p: PUPascalString): Boolean;
begin
  Result := p^.Exists(c);
end;

function UCharIn(c: USystemChar; const SomeCharset: TUOrdChar): Boolean;
const
  ord0 = Ord('0');
  ord1 = Ord('1');
  ord9 = Ord('9');
  ordLA = Ord('a');
  ordHA = Ord('A');
  ordLF = Ord('f');
  ordHF = Ord('F');
  ordLZ = Ord('z');
  ordHZ = Ord('Z');

var
  v: Word;
begin
  v := Ord(c);
  case SomeCharset of
    uc0to9: Result := (v >= ord0) and (v <= ord9);
    uc1to9: Result := (v >= ord1) and (v <= ord9);
    uc0to32: Result := ((v >= 0) and (v <= 32));
    uc0to32no10: Result := ((v >= 0) and (v <= 32) and (v <> 10));
    ucLoAtoF: Result := (v >= ordLA) and (v <= ordLF);
    ucHiAtoF: Result := (v >= ordHA) and (v <= ordHF);
    ucLoAtoZ: Result := (v >= ordLA) and (v <= ordLZ);
    ucHiAtoZ: Result := (v >= ordHA) and (v <= ordHZ);
    ucHex: Result := ((v >= ordLA) and (v <= ordLF)) or ((v >= ordHA) and (v <= ordHF)) or ((v >= ord0) and (v <= ord9));
    ucAtoF: Result := ((v >= ordLA) and (v <= ordLF)) or ((v >= ordHA) and (v <= ordHF));
    ucAtoZ: Result := ((v >= ordLA) and (v <= ordLZ)) or ((v >= ordHA) and (v <= ordHZ));
    ucVisibled: Result := (v <= $20) and (v <= $7E);
    else Result := False;
  end;
end;

function UCharIn(c: USystemChar; const SomeCharsets: TUOrdChars): Boolean;
var
  i: TUOrdChar;
begin
  Result := True;
  for i in SomeCharsets do
    if UCharIn(c, i) then
        Exit;
  Result := False;
end;

function UCharIn(c: USystemChar; const SomeCharsets: TUOrdChars; const SomeChars: TUPascalString): Boolean;
begin
  if UCharIn(c, SomeCharsets) then
      Result := True
  else
      Result := UCharIn(c, SomeChars);
end;

function UCharIn(c: USystemChar; const SomeCharsets: TUOrdChars; const p: PUPascalString): Boolean;
begin
  if UCharIn(c, SomeCharsets) then
      Result := True
  else
      Result := UCharIn(c, p);
end;

function UFastHashPSystemString(const s: PSystemString): TUHash;
var
  i: Integer;
  c: USystemChar;
begin
  Result := 0;

{$IFDEF FirstCharInZero}
  for i := 0 to length(s^) - 1 do
{$ELSE}
  for i := 1 to length(s^) do
{$ENDIF}
    begin
      c := s^[i];
      if UCharIn(c, ucHiAtoZ) then
          inc(c, 32);
      Result := ((Result shl 7) or (Result shr 25)) + TUHash(c);
    end;
end;

function UFastHash64PSystemString(const s: PSystemString): TUHash64;
var
  i: Integer;
  c: USystemChar;
begin
  Result := 0;

{$IFDEF FirstCharInZero}
  for i := 0 to length(s^) - 1 do
{$ELSE}
  for i := 1 to length(s^) do
{$ENDIF}
    begin
      c := s^[i];
      if UCharIn(c, ucHiAtoZ) then
          inc(c, 32);
      Result := ((Result shl 7) or (Result shr 57)) + TUHash64(c);
    end;
end;

function UFastHashSystemString(const s: SystemString): TUHash;
begin
  Result := UFastHashPSystemString(@s);
end;

function UFastHash64SystemString(const s: SystemString): TUHash64;
begin
  Result := UFastHash64PSystemString(@s);
end;

function UFastHashPPascalString(const s: PPascalString): TUHash;
var
  i: Integer;
  c: USystemChar;
begin
  Result := 0;
  for i := 1 to s^.Len do
    begin
      c := s^[i];
      if UCharIn(c, ucHiAtoZ) then
          inc(c, 32);
      Result := ((Result shl 7) or (Result shr 25)) + TUHash(c);
    end;
end;

function UFastHash64PPascalString(const s: PPascalString): TUHash64;
var
  i: Integer;
  c: USystemChar;
begin
  Result := 0;
  for i := 1 to s^.Len do
    begin
      c := s^[i];
      if UCharIn(c, ucHiAtoZ) then
          inc(c, 32);
      Result := ((Result shl 7) or (Result shr 57)) + TUHash64(c);
    end;
end;

function UFormat(const Fmt: USystemString; const Args: array of const): USystemString;
begin
  try
{$IFDEF FPC}
    Result := UnicodeFormat(Fmt, Args);
{$ELSE FPC}
    Result := Format(Fmt, Args);
{$ENDIF FPC}
  except
      Result := Fmt;
  end;
end;

function BytesOfPascalString(const s: TUPascalString): TBytes;
begin
  Result := s.Bytes;
end;

function PascalStringOfBytes(const s: TBytes): TUPascalString;
begin
  Result.Bytes := s;
end;

function GetSWMVMemory(const xLen, yLen: NativeInt): Pointer; inline;
{ optimized matrix performance }
begin
  Result := System.AllocMem((xLen + 1) * (yLen + 1) * SizeOf(NativeInt));
end;

function GetSWMV(const p: Pointer; const w, x, y: NativeInt): NativeInt; inline;
{ optimized matrix performance }
begin
  Result := PNativeInt(nativeUInt(p) + ((x + y * (w + 1)) * SizeOf(NativeInt)))^;
end;

procedure SetSWMV(const p: Pointer; const w, x, y: NativeInt; const v: NativeInt); inline;
{ optimized matrix performance }
begin
  PNativeInt(nativeUInt(p) + ((x + y * (w + 1)) * SizeOf(NativeInt)))^ := v;
end;

function GetMax(const i1, i2: NativeInt): NativeInt; inline;
begin
  if i1 > i2 then
      Result := i1
  else
      Result := i2;
end;

const
  SmithWaterman_MatchOk = 1;
  mismatch_penalty = -1;
  gap_penalty = -1;

function USmithWatermanCompare(const seq1, seq2: PUPascalString; var diff1, diff2: TUPascalString;
  const NoDiffChar: Boolean; const diffChar: USystemChar): Double;

  function InlineMatch(alphaC, betaC: USystemChar; const diffC: USystemChar): Integer; inline;
  begin
    if UCharIn(alphaC, ucLoAtoZ) then
        dec(alphaC, 32);
    if UCharIn(betaC, ucLoAtoZ) then
        dec(betaC, 32);

    if alphaC = betaC then
        Result := SmithWaterman_MatchOk
    else if (alphaC = diffC) or (betaC = diffC) then
        Result := gap_penalty
    else
        Result := mismatch_penalty;
  end;

var
  swMatrixPtr: Pointer;
  i, j, L1, l2: NativeInt;
  matched, deleted, inserted: NativeInt;
  score_current, score_diagonal, score_left, score_right: NativeInt;
  identity: NativeInt;
  align1, align2: TUPascalString;
begin
  L1 := seq1^.Len;
  l2 := seq2^.Len;

  if (L1 = 0) or (l2 = 0) or (L1 > UMaxSmithWatermanMatrix) or (l2 > UMaxSmithWatermanMatrix) then
    begin
      Result := -1;
      Exit;
    end;

  { fast build matrix }
  swMatrixPtr := GetSWMVMemory(L1, l2);
  if swMatrixPtr = nil then
    begin
      diff1 := '';
      diff2 := '';
      Result := -1;
      Exit;
    end;

  i := 0;
  while i <= L1 do
    begin
      SetSWMV(swMatrixPtr, L1, i, 0, gap_penalty * i);
      inc(i);
    end;

  j := 0;
  while j <= l2 do
    begin
      SetSWMV(swMatrixPtr, L1, 0, j, gap_penalty * j);
      inc(j);
    end;

  { compute matrix }
  i := 1;
  while i <= L1 do
    begin
      j := 1;
      while j <= l2 do
        begin
          matched := GetSWMV(swMatrixPtr, L1, i - 1, j - 1) + InlineMatch(seq1^[i], seq2^[j], diffChar);
          deleted := GetSWMV(swMatrixPtr, L1, i - 1, j) + gap_penalty;
          inserted := GetSWMV(swMatrixPtr, L1, i, j - 1) + gap_penalty;
          SetSWMV(swMatrixPtr, L1, i, j, GetMax(matched, GetMax(deleted, inserted)));
          inc(j);
        end;
      inc(i);
    end;

  { compute align }
  i := L1;
  j := l2;
  align1 := '';
  align2 := '';
  identity := 0;
  while (i > 0) and (j > 0) do
    begin
      score_current := GetSWMV(swMatrixPtr, L1, i, j);
      score_diagonal := GetSWMV(swMatrixPtr, L1, i - 1, j - 1);
      score_left := GetSWMV(swMatrixPtr, L1, i - 1, j);
      score_right := GetSWMV(swMatrixPtr, L1, i, j - 1);

      matched := InlineMatch(seq1^[i], seq2^[j], diffChar);

      if score_current = score_diagonal + matched then
        begin
          if matched = SmithWaterman_MatchOk then
            begin
              inc(identity);
              align1.Append(seq1^[i]);
              align2.Append(seq2^[j]);
            end
          else if NoDiffChar then
            begin
              align1.Append(diffChar);
              align2.Append(diffChar);
            end
          else
            begin
              align1.Append(seq1^[i]);
              align2.Append(seq2^[j]);
            end;
          dec(i);
          dec(j);
        end
      else if score_current = score_left + gap_penalty then
        begin
          if NoDiffChar then
              align1.Append(diffChar)
          else
              align1.Append(seq1^[i]);
          align2.Append(diffChar);
          dec(i);
        end
      else if score_current = score_right + gap_penalty then
        begin
          if NoDiffChar then
              align2.Append(diffChar)
          else
              align2.Append(seq2^[j]);
          align1.Append(diffChar);
          dec(j);
        end
      else
          raise Exception.Create('matrix error'); // matrix debug time
    end;

  System.FreeMemory(swMatrixPtr);

  while i > 0 do
    begin
      if NoDiffChar then
          align1.Append(diffChar)
      else
          align1.Append(seq1^[i]);
      align2.Append(diffChar);
      dec(i);
    end;

  while j > 0 do
    begin
      if NoDiffChar then
          align2.Append(diffChar)
      else
          align2.Append(seq2^[j]);
      align1.Append(diffChar);
      dec(j);
    end;

  if identity > 0 then
      Result := identity / align1.Len
  else
      Result := -1;

  diff1 := align1.Invert;
  diff2 := align2.Invert;
end;

function USmithWatermanCompare(const seq1, seq2: PUPascalString; var diff1, diff2: TUPascalString): Double;
begin
  Result := USmithWatermanCompare(seq1, seq2, diff1, diff2, False, '-');
end;

function USmithWatermanCompare(const seq1, seq2: TUPascalString; var diff1, diff2: TUPascalString;
  const NoDiffChar: Boolean; const diffChar: USystemChar): Double;
begin
  Result := USmithWatermanCompare(@seq1, @seq2, diff1, diff2, NoDiffChar, diffChar);
end;

function USmithWatermanCompare(const seq1, seq2: TUPascalString; var diff1, diff2: TUPascalString): Double;
begin
  Result := USmithWatermanCompare(seq1, seq2, diff1, diff2, False, '-');
end;

function USmithWatermanCompare(const seq1, seq2: PUPascalString; out Same, Diff: Integer): Double;

  function InlineMatch(alphaC, betaC: USystemChar): NativeInt; inline;
  begin
    if UCharIn(alphaC, ucLoAtoZ) then
        dec(alphaC, 32);
    if UCharIn(betaC, ucLoAtoZ) then
        dec(betaC, 32);

    if alphaC = betaC then
        Result := SmithWaterman_MatchOk
    else
        Result := mismatch_penalty;
  end;

var
  swMatrixPtr: Pointer;
  i, j, L1, l2: NativeInt;
  matched, deleted, inserted: NativeInt;
  score_current, score_diagonal, score_left, score_right: NativeInt;
  identity, L_: NativeInt;
begin
  L1 := seq1^.Len;
  l2 := seq2^.Len;

  if (L1 = 0) or (l2 = 0) or (L1 > UMaxSmithWatermanMatrix) or (l2 > UMaxSmithWatermanMatrix) then
    begin
      Result := -1;
      Same := 0;
      Diff := L1 + l2;
      Exit;
    end;

  { fast build matrix }
  swMatrixPtr := GetSWMVMemory(L1, l2);
  if swMatrixPtr = nil then
    begin
      Result := -1;
      Exit;
    end;

  i := 0;
  while i <= L1 do
    begin
      SetSWMV(swMatrixPtr, L1, i, 0, gap_penalty * i);
      inc(i);
    end;

  j := 0;
  while j <= l2 do
    begin
      SetSWMV(swMatrixPtr, L1, 0, j, gap_penalty * j);
      inc(j);
    end;

  { compute matrix }
  i := 1;
  while i <= L1 do
    begin
      j := 1;
      while j <= l2 do
        begin
          matched := GetSWMV(swMatrixPtr, L1, i - 1, j - 1) + InlineMatch(seq1^[i], seq2^[j]);
          deleted := GetSWMV(swMatrixPtr, L1, i - 1, j) + gap_penalty;
          inserted := GetSWMV(swMatrixPtr, L1, i, j - 1) + gap_penalty;
          SetSWMV(swMatrixPtr, L1, i, j, GetMax(matched, GetMax(deleted, inserted)));
          inc(j);
        end;
      inc(i);
    end;

  { compute align }
  i := L1;
  j := l2;
  identity := 0;
  L_ := 0;
  while (i > 0) and (j > 0) do
    begin
      score_current := GetSWMV(swMatrixPtr, L1, i, j);
      score_diagonal := GetSWMV(swMatrixPtr, L1, i - 1, j - 1);
      score_left := GetSWMV(swMatrixPtr, L1, i - 1, j);
      score_right := GetSWMV(swMatrixPtr, L1, i, j - 1);
      matched := InlineMatch(seq1^[i], seq2^[j]);

      if score_current = score_diagonal + matched then
        begin
          if matched = SmithWaterman_MatchOk then
              inc(identity);

          inc(L_);
          dec(i);
          dec(j);
        end
      else if score_current = score_left + gap_penalty then
        begin
          inc(L_);
          dec(i);
        end
      else if score_current = score_right + gap_penalty then
        begin
          inc(L_);
          dec(j);
        end
      else
          raise Exception.Create('matrix error'); // matrix debug time
    end;

  System.FreeMemory(swMatrixPtr);

  if identity > 0 then
    begin
      Result := identity / (L_ + i + j);
      Same := identity;
      Diff := (L_ + i + j) - identity;
    end
  else
    begin
      Result := -1;
      Same := 0;
      Diff := L_ + i + j;
    end;
end;

function USmithWatermanCompare(const seq1, seq2: PUPascalString): Double;
var
  Same, Diff: Integer;
begin
  Result := USmithWatermanCompare(seq1, seq2, Same, Diff);
end;

function USmithWatermanCompare(const seq1, seq2: TUPascalString): Double;
begin
  Result := USmithWatermanCompare(@seq1, @seq2);
end;

function USmithWatermanCompare(const seq1: TUArrayPascalString; const seq2: TUPascalString): Double;
var
  i: Integer;
  r: Double;
begin
  Result := -1;
  for i := 0 to length(seq1) - 1 do
    begin
      r := USmithWatermanCompare(seq1[i], seq2);
      if r > Result then
          Result := r;
    end;
end;

function USmithWatermanCompare(const seq1: Pointer; siz1: Integer; const seq2: Pointer; siz2: Integer;
  out Same, Diff: Integer): Double;

  function InlineMatch(const alphaB, betaB: Byte): NativeInt; inline;
  begin
    if alphaB = betaB then
        Result := SmithWaterman_MatchOk
    else
        Result := mismatch_penalty;
  end;

var
  swMatrixPtr: Pointer;
  i, j, L1, l2: NativeInt;
  matched, deleted, inserted: NativeInt;
  score_current, score_diagonal, score_left, score_right: NativeInt;
  identity, L_: NativeInt;
begin
  L1 := siz1;
  l2 := siz2;

  if (L1 = 0) or (l2 = 0) or (L1 > UMaxSmithWatermanMatrix) or (l2 > UMaxSmithWatermanMatrix) then
    begin
      Result := -1;
      Same := 0;
      Diff := L1 + l2;
      Exit;
    end;

  { fast build matrix }
  swMatrixPtr := GetSWMVMemory(L1, l2);
  if swMatrixPtr = nil then
    begin
      Result := -1;
      Exit;
    end;

  i := 0;
  while i <= L1 do
    begin
      SetSWMV(swMatrixPtr, L1, i, 0, gap_penalty * i);
      inc(i);
    end;

  j := 0;
  while j <= l2 do
    begin
      SetSWMV(swMatrixPtr, L1, 0, j, gap_penalty * j);
      inc(j);
    end;

  { compute matrix }
  i := 1;
  while i <= L1 do
    begin
      j := 1;
      while j <= l2 do
        begin
          matched := GetSWMV(swMatrixPtr, L1, i - 1, j - 1) + InlineMatch(PByte(nativeUInt(seq1) + (i - 1))^, PByte(nativeUInt(seq2) + (j - 1))^);
          deleted := GetSWMV(swMatrixPtr, L1, i - 1, j) + gap_penalty;
          inserted := GetSWMV(swMatrixPtr, L1, i, j - 1) + gap_penalty;
          SetSWMV(swMatrixPtr, L1, i, j, GetMax(matched, GetMax(deleted, inserted)));
          inc(j);
        end;
      inc(i);
    end;

  { compute align }
  i := L1;
  j := l2;
  identity := 0;
  L_ := 0;
  while (i > 0) and (j > 0) do
    begin
      score_current := GetSWMV(swMatrixPtr, L1, i, j);
      score_diagonal := GetSWMV(swMatrixPtr, L1, i - 1, j - 1);
      score_left := GetSWMV(swMatrixPtr, L1, i - 1, j);
      score_right := GetSWMV(swMatrixPtr, L1, i, j - 1);
      matched := InlineMatch(PByte(nativeUInt(seq1) + (i - 1))^, PByte(nativeUInt(seq2) + (j - 1))^);

      if score_current = score_diagonal + matched then
        begin
          if matched = SmithWaterman_MatchOk then
              inc(identity);

          inc(L_);
          dec(i);
          dec(j);
        end
      else if score_current = score_left + gap_penalty then
        begin
          inc(L_);
          dec(i);
        end
      else if score_current = score_right + gap_penalty then
        begin
          inc(L_);
          dec(j);
        end
      else
          raise Exception.Create('matrix error'); // matrix debug time
    end;

  System.FreeMemory(swMatrixPtr);

  if identity > 0 then
    begin
      Result := identity / (L_ + i + j);
      Same := identity;
      Diff := (L_ + i + j) - identity;
    end
  else
    begin
      Result := -1;
      Same := 0;
      Diff := L_ + i + j;
    end;
end;

function USmithWatermanCompare(const seq1: Pointer; siz1: Integer; const seq2: Pointer; siz2: Integer): Double;
var
  Same, Diff: Integer;
begin
  Result := USmithWatermanCompare(seq1, siz1, seq2, siz2, Same, Diff);
end;

function USmithWatermanCompareLongString(const t1, t2: TUPascalString; const MinDiffCharWithPeerLine: Integer; out Same, Diff: Integer): Double;
type
  PSRec = ^TSRec;

  TSRec = record
    s: TUPascalString;
  end;

  procedure _FillText(psPtr: PUPascalString; outLst: TCoreClassList);
  var
    L_, i: Integer;
    n: TUPascalString;
    p: PSRec;
  begin
    L_ := psPtr^.Len;
    i := 1;
    n := '';
    while i <= L_ do
      begin
        if UCharIn(psPtr^[i], [#13, #10]) then
          begin
            n := n.DeleteChar(#32#9);
            if n.Len > 0 then
              begin
                new(p);
                p^.s := n;
                outLst.Add(p);
                n := '';
              end;
            repeat
                inc(i);
            until (i > L_) or (not UCharIn(psPtr^[i], [#13, #10, #32, #9]));
          end
        else
          begin
            n.Append(psPtr^[i]);
            inc(i);
          end;
      end;

    n := n.DeleteChar(#32#9);
    if n.Len > 0 then
      begin
        new(p);
        p^.s := n;
        outLst.Add(p);
      end;
  end;

  function InlineMatch(const alpha, beta: PSRec; const MinDiffCharWithPeerLine: Integer; var cSame, cDiff: Integer): NativeInt; inline;
  begin
    if USmithWatermanCompare(@alpha^.s, @beta^.s, cSame, cDiff) > 0 then
      begin
        if cDiff < MinDiffCharWithPeerLine then
            Result := SmithWaterman_MatchOk
        else
            Result := mismatch_penalty;
      end
    else
        Result := mismatch_penalty;
  end;

var
  lst1, lst2: TCoreClassList;

  procedure _Init;
  begin
    lst1 := TCoreClassList.Create;
    lst2 := TCoreClassList.Create;
    _FillText(@t1, lst1);
    _FillText(@t2, lst2);
  end;

  procedure _Free;
  var
    i: Integer;
  begin
    for i := 0 to lst1.Count - 1 do
        Dispose(PSRec(lst1[i]));
    for i := 0 to lst2.Count - 1 do
        Dispose(PSRec(lst2[i]));
    DisposeObject([lst1, lst2]);
  end;

var
  swMatrixPtr: Pointer;
  i, j, L1, l2: NativeInt;
  matched, deleted, inserted: NativeInt;
  score_current, score_diagonal, score_left, score_right: NativeInt;
  cSame, cDiff, TotalSame, TotalDiff: Integer;
begin
  _Init;
  L1 := lst1.Count;
  l2 := lst2.Count;

  if (L1 = 0) or (l2 = 0) or (L1 > UMaxSmithWatermanMatrix) or (l2 > UMaxSmithWatermanMatrix) then
    begin
      Result := -1;
      Same := 0;
      Diff := L1 + l2;
      _Free;
      Exit;
    end;

  { fast build matrix }
  swMatrixPtr := GetSWMVMemory(L1, l2);
  if swMatrixPtr = nil then
    begin
      Result := -1;
      _Free;
      Exit;
    end;

  i := 0;
  while i <= L1 do
    begin
      SetSWMV(swMatrixPtr, L1, i, 0, gap_penalty * i);
      inc(i);
    end;

  j := 0;
  while j <= l2 do
    begin
      SetSWMV(swMatrixPtr, L1, 0, j, gap_penalty * j);
      inc(j);
    end;

  { compute matrix }
  i := 1;
  while i <= L1 do
    begin
      j := 1;
      while j <= l2 do
        begin
          matched := GetSWMV(swMatrixPtr, L1, i - 1, j - 1) + InlineMatch(PSRec(lst1[i - 1]), PSRec(lst2[j - 1]), MinDiffCharWithPeerLine, cSame, cDiff);
          deleted := GetSWMV(swMatrixPtr, L1, i - 1, j) + gap_penalty;
          inserted := GetSWMV(swMatrixPtr, L1, i, j - 1) + gap_penalty;
          SetSWMV(swMatrixPtr, L1, i, j, GetMax(matched, GetMax(deleted, inserted)));
          inc(j);
        end;
      inc(i);
    end;

  { compute align }
  i := L1;
  j := l2;
  TotalSame := 0;
  TotalDiff := 0;
  while (i > 0) and (j > 0) do
    begin
      score_current := GetSWMV(swMatrixPtr, L1, i, j);
      score_diagonal := GetSWMV(swMatrixPtr, L1, i - 1, j - 1);
      score_left := GetSWMV(swMatrixPtr, L1, i - 1, j);
      score_right := GetSWMV(swMatrixPtr, L1, i, j - 1);
      matched := InlineMatch(PSRec(lst1[i - 1]), PSRec(lst2[j - 1]), MinDiffCharWithPeerLine, cSame, cDiff);

      inc(TotalSame, cSame);
      inc(TotalDiff, cDiff);

      if score_current = score_diagonal + matched then
        begin
          dec(i);
          dec(j);
        end
      else if score_current = score_left + gap_penalty then
        begin
          dec(i);
        end
      else if score_current = score_right + gap_penalty then
        begin
          dec(j);
        end
      else
          raise Exception.Create('matrix error'); // matrix debug time
    end;

  System.FreeMemory(swMatrixPtr);
  _Free;

  if TotalSame > 0 then
    begin
      Result := TotalSame / (TotalSame + TotalDiff);
      Same := TotalSame;
      Diff := TotalDiff;
    end
  else
    begin
      Result := -1;
      Same := 0;
      Diff := t2.Len + t1.Len;
    end;
end;

function USmithWatermanCompareLongString(const t1, t2: TUPascalString): Double;
var
  Same, Diff: Integer;
begin
  Result := USmithWatermanCompareLongString(t1, t2, 5, Same, Diff);
end;

{$IFDEF FPC}


operator := (const s: Variant)r: TUPascalString;
begin
  r.Text := s;
end;

operator := (const s: AnsiString)r: TUPascalString;
begin
  r.Text := s;
end;

operator := (const s: RawByteString)r: TUPascalString;
begin
  r.Text := s;
end;

operator := (const s: UnicodeString)r: TUPascalString;
begin
  r.Text := s;
end;

operator := (const s: WideString)r: TUPascalString;
begin
  r.Text := s;
end;

operator := (const s: ShortString)r: TUPascalString;
begin
  r.Text := s;
end;

operator := (const c: USystemChar)r: TUPascalString;
begin
  r.Text := c;
end;

operator := (const c: TPascalString)r: TUPascalString;
begin
  Result.Bytes := c.Bytes;
end;

operator := (const s: TUPascalString)r: AnsiString;
begin
  r := s.Text;
end;

operator := (const s: TUPascalString)r: RawByteString;
begin
  r := s.Text;
end;

operator := (const s: TUPascalString)r: UnicodeString;
begin
  r := s.Text;
end;

operator := (const s: TUPascalString)r: WideString;
begin
  r := s.Text;
end;

operator := (const s: TUPascalString)r: ShortString;
begin
  r := s.Text;
end;

operator := (const s: TUPascalString)r: Variant;
begin
  r := s.Text;
end;

operator := (const s: TUPascalString)r: TPascalString;
begin
  Result.Bytes := s.Bytes;
end;

operator = (const a: TUPascalString; const b: TUPascalString): Boolean;
begin
  Result := a.Text = b.Text;
end;

operator <> (const a: TUPascalString; const b: TUPascalString): Boolean;
begin
  Result := a.Text <> b.Text;
end;

operator > (const a: TUPascalString; const b: TUPascalString): Boolean;
begin
  Result := a.Text > b.Text;
end;

operator >= (const a: TUPascalString; const b: TUPascalString): Boolean;
begin
  Result := a.Text >= b.Text;
end;

operator < (const a: TUPascalString; const b: TUPascalString): Boolean;
begin
  Result := a.Text < b.Text;
end;

operator <= (const a: TUPascalString; const b: TUPascalString): Boolean;
begin
  Result := a.Text <= b.Text;
end;

operator + (const a: TUPascalString; const b: TUPascalString): TUPascalString;
begin
  CombineCharsPP(a.buff, b.buff, Result.buff);
end;

operator + (const a: TUPascalString; const b: USystemString): TUPascalString;
begin
  CombineCharsPS(a.buff, b, Result.buff);
end;

operator + (const a: USystemString; const b: TUPascalString): TUPascalString;
begin
  CombineCharsSP(a, b.buff, Result.buff);
end;

operator + (const a: TUPascalString; const b: USystemChar): TUPascalString;
begin
  CombineCharsPC(a.buff, b, Result.buff);
end;

operator + (const a: USystemChar; const b: TUPascalString): TUPascalString;
begin
  CombineCharsCP(a, b.buff, Result.buff);
end;

{$ENDIF}


function TUPascalString.GetText: USystemString;
begin
  SetLength(Result, length(buff));
  if length(buff) > 0 then
      CopyPtr(@buff[0], @Result[UFirstCharPos], length(buff) * USystemCharSize);
end;

procedure TUPascalString.SetText(const Value: USystemString);
begin
  SetLength(buff, length(Value));

  if length(buff) > 0 then
      CopyPtr(@Value[UFirstCharPos], @buff[0], length(buff) * USystemCharSize);
end;

function TUPascalString.GetLen: Integer;
begin
  Result := length(buff);
end;

procedure TUPascalString.SetLen(const Value: Integer);
begin
  SetLength(buff, Value);
end;

function TUPascalString.GetChars(index: Integer): USystemChar;
begin
  if (index > length(buff)) or (index <= 0) then
      Result := #0
  else
      Result := buff[index - 1];
end;

procedure TUPascalString.SetChars(index: Integer; const Value: USystemChar);
begin
  buff[index - 1] := Value;
end;

function TUPascalString.GetBytes: TBytes;
begin
  SetLength(Result, 0);
  if length(buff) = 0 then
      Exit;
{$IFDEF FPC}
  Result := SysUtils.TEncoding.UTF8.GetBytes(buff);
{$ELSE}
  Result := SysUtils.TEncoding.UTF8.GetBytes(buff);
{$ENDIF}
end;

procedure TUPascalString.SetBytes(const Value: TBytes);
begin
  SetLength(buff, 0);
  if length(Value) = 0 then
      Exit;
  try
      Text := SysUtils.TEncoding.UTF8.GetString(Value);
  except
      SetPlatformBytes(Value);
  end;
end;

function TUPascalString.GetPlatformBytes: TBytes;
begin
  SetLength(Result, 0);
  if length(buff) = 0 then
      Exit;
{$IFDEF FPC}
  Result := SysUtils.TEncoding.Default.GetBytes(buff);
{$ELSE}
  Result := SysUtils.TEncoding.Default.GetBytes(buff);
{$ENDIF}
end;

procedure TUPascalString.SetPlatformBytes(const Value: TBytes);
begin
  SetLength(buff, 0);
  if length(Value) = 0 then
      Exit;
  try
      Text := SysUtils.TEncoding.Default.GetString(Value);
  except
      SetLength(buff, 0);
  end;
end;

function TUPascalString.GetANSI: TBytes;
begin
  SetLength(Result, 0);
  if length(buff) = 0 then
      Exit;
{$IFDEF FPC}
  Result := SysUtils.TEncoding.ANSI.GetBytes(Text);
{$ELSE}
  Result := SysUtils.TEncoding.ANSI.GetBytes(buff);
{$ENDIF}
end;

procedure TUPascalString.SetANSI(const Value: TBytes);
begin
  SetLength(buff, 0);
  if length(Value) = 0 then
      Exit;
  try
      Text := SysUtils.TEncoding.ANSI.GetString(Value);
  except
      SetLength(buff, 0);
  end;
end;

function TUPascalString.GetLast: USystemChar;
begin
  if length(buff) > 0 then
      Result := buff[length(buff) - 1]
  else
      Result := #0;
end;

procedure TUPascalString.SetLast(const Value: USystemChar);
begin
  buff[length(buff) - 1] := Value;
end;

function TUPascalString.GetFirst: USystemChar;
begin
  if length(buff) > 0 then
      Result := buff[0]
  else
      Result := #0;
end;

procedure TUPascalString.SetFirst(const Value: USystemChar);
begin
  buff[0] := Value;
end;

function TUPascalString.GetUpperChar(index: Integer): USystemChar;
begin
  Result := GetChars(index);
  if CharIn(Result, cLoAtoZ) then
      Result := USystemChar(Word(Result) xor $0020);
end;

procedure TUPascalString.SetUpperChar(index: Integer; const Value: USystemChar);
begin
  if CharIn(Value, cLoAtoZ) then
      SetChars(index, USystemChar(Word(Value) xor $0020))
  else
      SetChars(index, Value);
end;

function TUPascalString.GetLowerChar(index: Integer): USystemChar;
begin
  Result := GetChars(index);
  if CharIn(Result, cHiAtoZ) then
      Result := USystemChar(Word(Result) or $0020);
end;

procedure TUPascalString.SetLowerChar(index: Integer; const Value: USystemChar);
begin
  if CharIn(Value, cHiAtoZ) then
      SetChars(index, USystemChar(Word(Value) or $0020))
  else
      SetChars(index, Value);
end;
{$IFDEF DELPHI}


class operator TUPascalString.Equal(const Lhs, Rhs: TUPascalString): Boolean;
begin
  Result := (Lhs.Len = Rhs.Len) and (Lhs.Text = Rhs.Text);
end;

class operator TUPascalString.NotEqual(const Lhs, Rhs: TUPascalString): Boolean;
begin
  Result := not(Lhs = Rhs);
end;

class operator TUPascalString.GreaterThan(const Lhs, Rhs: TUPascalString): Boolean;
begin
  Result := Lhs.Text > Rhs.Text;
end;

class operator TUPascalString.GreaterThanOrEqual(const Lhs, Rhs: TUPascalString): Boolean;
begin
  Result := Lhs.Text >= Rhs.Text;
end;

class operator TUPascalString.LessThan(const Lhs, Rhs: TUPascalString): Boolean;
begin
  Result := Lhs.Text < Rhs.Text;
end;

class operator TUPascalString.LessThanOrEqual(const Lhs, Rhs: TUPascalString): Boolean;
begin
  Result := Lhs.Text <= Rhs.Text;
end;

class operator TUPascalString.Add(const Lhs, Rhs: TUPascalString): TUPascalString;
begin
  CombineCharsPP(Lhs.buff, Rhs.buff, Result.buff);
end;

class operator TUPascalString.Add(const Lhs: USystemString; const Rhs: TUPascalString): TUPascalString;
begin
  CombineCharsSP(Lhs, Rhs.buff, Result.buff);
end;

class operator TUPascalString.Add(const Lhs: TUPascalString; const Rhs: USystemString): TUPascalString;
begin
  CombineCharsPS(Lhs.buff, Rhs, Result.buff);
end;

class operator TUPascalString.Add(const Lhs: USystemChar; const Rhs: TUPascalString): TUPascalString;
begin
  CombineCharsCP(Lhs, Rhs.buff, Result.buff);
end;

class operator TUPascalString.Add(const Lhs: TUPascalString; const Rhs: USystemChar): TUPascalString;
begin
  CombineCharsPC(Lhs.buff, Rhs, Result.buff);
end;

class operator TUPascalString.Implicit(Value: RawByteString): TUPascalString;
begin
  Result.Text := Value;
end;

class operator TUPascalString.Implicit(Value: TPascalString): TUPascalString;
begin
  Result.Bytes := Value.Bytes;
end;

class operator TUPascalString.Implicit(Value: USystemString): TUPascalString;
begin
  Result.Text := Value;
end;

class operator TUPascalString.Implicit(Value: USystemChar): TUPascalString;
begin
  Result.Len := 1;
  Result.buff[0] := Value;
end;

class operator TUPascalString.Implicit(Value: TUPascalString): USystemString;
begin
  Result := Value.Text;
end;

class operator TUPascalString.Implicit(Value: TUPascalString): Variant;
begin
  Result := Value.Text;
end;

class operator TUPascalString.Explicit(Value: TUPascalString): RawByteString;
begin
  Result := Value.Text;
end;

class operator TUPascalString.Explicit(Value: TUPascalString): TPascalString;
begin
  Result.Bytes := Value.Bytes;
end;

class operator TUPascalString.Explicit(Value: TUPascalString): USystemString;
begin
  Result := Value.Text;
end;

class operator TUPascalString.Explicit(Value: TUPascalString): Variant;
begin
  Result := Value.Text;
end;

class operator TUPascalString.Explicit(Value: USystemString): TUPascalString;
begin
  Result.Text := Value;
end;

class operator TUPascalString.Explicit(Value: Variant): TUPascalString;
begin
  Result.Text := VarToStr(Value);
end;

class operator TUPascalString.Explicit(Value: USystemChar): TUPascalString;
begin
  Result.Len := 1;
  Result.buff[0] := Value;
end;

{$ENDIF}


function TUPascalString.Copy(index, Count: NativeInt): TUPascalString;
var
  L_: NativeInt;
begin
  L_ := length(buff);

  if (index - 1) + Count > L_ then
      Count := L_ - (index - 1);

  SetLength(Result.buff, Count);
  if Count > 0 then
      CopyPtr(@buff[index - 1], @Result.buff[0], USystemCharSize * Count);
end;

function TUPascalString.Same(const p: PUPascalString): Boolean;
var
  i: Integer;
  s, d: USystemChar;
begin
  Result := (p^.Len = Len);
  if not Result then
      Exit;
  for i := 0 to Len - 1 do
    begin
      s := buff[i];
      if UCharIn(s, ucHiAtoZ) then
          inc(s, 32);
      d := p^.buff[i];
      if UCharIn(d, ucHiAtoZ) then
          inc(d, 32);
      if s <> d then
          Exit(False);
    end;
end;

function TUPascalString.Same(const t: TUPascalString): Boolean;
var
  i: Integer;
  s, d: USystemChar;
begin
  Result := (t.Len = Len);
  if not Result then
      Exit;
  for i := 0 to Len - 1 do
    begin
      s := buff[i];
      if UCharIn(s, ucHiAtoZ) then
          inc(s, 32);
      d := t.buff[i];
      if UCharIn(d, ucHiAtoZ) then
          inc(d, 32);
      if s <> d then
          Exit(False);
    end;
end;

function TUPascalString.Same(const t1, t2: TUPascalString): Boolean;
begin
  Result := Same(@t1) or Same(@t2);
end;

function TUPascalString.Same(const t1, t2, t3: TUPascalString): Boolean;
begin
  Result := Same(@t1) or Same(@t2) or Same(@t3);
end;

function TUPascalString.Same(const t1, t2, t3, t4: TUPascalString): Boolean;
begin
  Result := Same(@t1) or Same(@t2) or Same(@t3) or Same(@t4);
end;

function TUPascalString.Same(const t1, t2, t3, t4, t5: TUPascalString): Boolean;
begin
  Result := Same(@t1) or Same(@t2) or Same(@t3) or Same(@t4) or Same(@t5);
end;

function TUPascalString.Same(const IgnoreCase: Boolean; const t: TUPascalString): Boolean;
var
  i: Integer;
  s, d: USystemChar;
begin
  Result := (t.Len = Len);
  if not Result then
      Exit;
  for i := 0 to Len - 1 do
    begin
      s := buff[i];
      if IgnoreCase then
        if UCharIn(s, ucHiAtoZ) then
            inc(s, 32);

      d := t.buff[i];
      if IgnoreCase then
        if UCharIn(d, ucHiAtoZ) then
            inc(d, 32);

      if s <> d then
          Exit(False);
    end;
end;

function TUPascalString.ComparePos(const Offset: Integer; const p: PUPascalString): Boolean;
var
  i, L_: Integer;
  sourChar, destChar: USystemChar;
begin
  Result := False;
  i := 1;
  L_ := p^.Len;
  if (Offset + L_ - 1) > Len then
      Exit;
  while i <= L_ do
    begin
      sourChar := GetChars(Offset + i - 1);
      destChar := p^[i];

      if UCharIn(sourChar, ucLoAtoZ) then
          dec(sourChar, 32);
      if UCharIn(destChar, ucLoAtoZ) then
          dec(destChar, 32);

      if sourChar <> destChar then
          Exit;
      inc(i);
    end;
  Result := True;
end;

function TUPascalString.ComparePos(const Offset: Integer; const t: TUPascalString): Boolean;
var
  i, L_: Integer;
  sourChar, destChar: USystemChar;
begin
  Result := False;
  i := 1;
  L_ := t.Len;
  if (Offset + L_) > Len then
      Exit;
  while i <= L_ do
    begin
      sourChar := GetChars(Offset + i - 1);
      destChar := t[i];

      if UCharIn(sourChar, ucLoAtoZ) then
          dec(sourChar, 32);
      if UCharIn(destChar, ucLoAtoZ) then
          dec(destChar, 32);

      if sourChar <> destChar then
          Exit;
      inc(i);
    end;
  Result := True;
end;

function TUPascalString.GetPos(const s: TUPascalString; const Offset: Integer = 1): Integer;
var
  i: Integer;
begin
  Result := 0;
  if s.Len > 0 then
    for i := Offset to Len - s.Len + 1 do
      if ComparePos(i, @s) then
          Exit(i);
end;

function TUPascalString.GetPos(const s: PUPascalString; const Offset: Integer = 1): Integer;
var
  i: Integer;
begin
  Result := 0;
  if s^.Len > 0 then
    for i := Offset to Len - s^.Len + 1 do
      if ComparePos(i, s) then
          Exit(i);
end;

function TUPascalString.Exists(c: USystemChar): Boolean;
var
  i: Integer;
begin
  for i := low(buff) to high(buff) do
    if buff[i] = c then
        Exit(True);
  Result := False;
end;

function TUPascalString.Exists(c: array of USystemChar): Boolean;
var
  i: Integer;
begin
  for i := low(buff) to high(buff) do
    if UCharIn(buff[i], c) then
        Exit(True);
  Result := False;
end;

function TUPascalString.Exists(const s: TUPascalString): Boolean;
begin
  Result := GetPos(@s, 1) > 0;
end;

function TUPascalString.hash: TUHash;
begin
  Result := UFastHashPPascalString(@Self);
end;

function TUPascalString.Hash64: TUHash64;
begin
  Result := UFastHash64PPascalString(@Self);
end;

function TUPascalString.GetCharCount(c: USystemChar): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := low(buff) to high(buff) do
    if UCharIn(buff[i], c) then
        inc(Result);
end;

procedure TUPascalString.DeleteLast;
begin
  if Len > 0 then
      SetLength(buff, length(buff) - 1);
end;

procedure TUPascalString.DeleteFirst;
begin
  if Len > 0 then
      buff := System.Copy(buff, 1, Len);
end;

procedure TUPascalString.Delete(idx, cnt: Integer);
begin
  if (idx + cnt <= Len) then
      Text := GetString(1, idx) + GetString(idx + cnt, Len + 1)
  else
      Text := GetString(1, idx);
end;

procedure TUPascalString.Clear;
begin
  SetLength(buff, 0);
end;

procedure TUPascalString.Append(t: TUPascalString);
var
  r, L_: Integer;
begin
  L_ := length(t.buff);
  if L_ > 0 then
    begin
      r := length(buff);
      SetLength(buff, r + L_);
      CopyPtr(@t.buff[0], @buff[r], L_ * USystemCharSize);
    end;
end;

procedure TUPascalString.Append(c: USystemChar);
begin
  SetLength(buff, length(buff) + 1);
  buff[length(buff) - 1] := c;
end;

procedure TUPascalString.Append(const Fmt: SystemString; const Args: array of const);
begin
  Append(PFormat(Fmt, Args));
end;

function TUPascalString.GetString(bPos, ePos: NativeInt): TUPascalString;
begin
  if ePos > length(buff) then
      Result := Self.Copy(bPos, length(buff) - bPos + 1)
  else
      Result := Self.Copy(bPos, (ePos - bPos));
end;

procedure TUPascalString.Insert(AText: USystemString; idx: Integer);
begin
  Text := GetString(1, idx) + AText + GetString(idx + 1, Len);
end;

procedure TUPascalString.FastAsText(var output: USystemString);
begin
  SetLength(output, length(buff));
  if length(buff) > 0 then
      CopyPtr(@buff[0], @output[UFirstCharPos], length(buff) * USystemCharSize);
end;

procedure TUPascalString.FastGetBytes(var output: TBytes);
begin
{$IFDEF FPC}
  output := SysUtils.TEncoding.UTF8.GetBytes(buff);
{$ELSE}
  output := SysUtils.TEncoding.UTF8.GetBytes(buff);
{$ENDIF}
end;

function TUPascalString.LowerText: USystemString;
begin
  Result := LowerCase(Text);
end;

function TUPascalString.UpperText: USystemString;
begin
  Result := UpperCase(Text);
end;

function TUPascalString.Invert: TUPascalString;
var
  i, j: Integer;
begin
  SetLength(Result.buff, length(buff));
  j := low(Result.buff);
  for i := high(buff) downto low(buff) do
    begin
      Result.buff[j] := buff[i];
      inc(j);
    end;
end;

function TUPascalString.TrimChar(const Chars: TUPascalString): TUPascalString;
var
  L_, bp, EP: Integer;
begin
  Result := '';
  L_ := Len;
  if L_ > 0 then
    begin
      bp := 1;
      while UCharIn(GetChars(bp), @Chars) do
        begin
          inc(bp);
          if (bp > L_) then
            begin
              Result := '';
              Exit;
            end;
        end;
      if bp > L_ then
          Result := ''
      else
        begin
          EP := L_;

          while UCharIn(GetChars(EP), @Chars) do
            begin
              dec(EP);
              if (EP < 1) then
                begin
                  Result := '';
                  Exit;
                end;
            end;
          Result := GetString(bp, EP + 1);
        end;
    end;
end;

function TUPascalString.DeleteChar(const Chars: TUPascalString): TUPascalString;
var
  c: USystemChar;
begin
  Result := '';
  for c in buff do
    if not UCharIn(c, @Chars) then
        Result.Append(c);
end;

function TUPascalString.DeleteChar(const Chars: TUOrdChars): TUPascalString;
var
  c: USystemChar;
begin
  Result := '';
  for c in buff do
    if not UCharIn(c, Chars) then
        Result.Append(c);
end;

function TUPascalString.ReplaceChar(const Chars: TUPascalString; const newChar: USystemChar): TUPascalString;
var
  i: Integer;
begin
  Result.Len := Len;
  for i := low(buff) to high(buff) do
    if UCharIn(buff[i], Chars) then
        Result.buff[i] := newChar
    else
        Result.buff[i] := buff[i];
end;

function TUPascalString.ReplaceChar(const Chars, newChar: USystemChar): TUPascalString;
var
  i: Integer;
begin
  Result.Len := Len;
  for i := low(buff) to high(buff) do
    if UCharIn(buff[i], Chars) then
        Result.buff[i] := newChar
    else
        Result.buff[i] := buff[i];
end;

function TUPascalString.ReplaceChar(const Chars: TUOrdChars; const newChar: USystemChar): TUPascalString;
var
  i: Integer;
begin
  Result.Len := Len;
  for i := low(buff) to high(buff) do
    if UCharIn(buff[i], Chars) then
        Result.buff[i] := newChar
    else
        Result.buff[i] := buff[i];
end;

function TUPascalString.BuildPlatformPChar: Pointer;
type
  TAnsiChar_Buff = array [0 .. MaxInt - 1] of Byte;
  PAnsiChar_Buff = ^TAnsiChar_Buff;
var
  swap_buff: TBytes;
  buff_P: PAnsiChar_Buff;
begin
  swap_buff := PlatformBytes;
  buff_P := GetMemory(length(swap_buff) + 1);
  CopyPtr(@swap_buff[0], buff_P, length(swap_buff));
  buff_P^[length(swap_buff)] := 0;
  SetLength(swap_buff, 0);
  Result := buff_P;
end;

class procedure TUPascalString.FreePlatformPChar(p: Pointer);
begin
  FreeMemory(p);
end;

class function TUPascalString.RandomString(L_: Integer): TUPascalString;
var
  i: Integer;
  rnd: TMT19937Random;
begin
  Result.L := L_;
  rnd := TMT19937Random.Create;
  for i := 1 to L_ do
      Result[i] := USystemChar(rnd.Rand32($7E - $20) + $20);
  DisposeObject(rnd);
end;

function TUPascalString.SmithWaterman(const p: PUPascalString): Double;
begin
  Result := USmithWatermanCompare(@Self, @p);
end;

function TUPascalString.SmithWaterman(const s: TUPascalString): Double;
begin
  Result := USmithWatermanCompare(@Self, @s);
end;

function TUPascalString.BOMBytes: TBytes;
begin
{$IFDEF FPC}
  Result := GetBytes;
{$ELSE}
  Result := SysUtils.TEncoding.UTF8.GetPreamble + GetBytes;
{$ENDIF}
end;

end.
