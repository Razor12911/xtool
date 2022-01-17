{ ****************************************************************************** }
{ * delphi:string fpc:AnsiString               by QQ 600585@qq.com             * }
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

(*
  update history
  2017-11-26
  fixed UnicodeString in FPC
*)

unit PascalStrings;

{$INCLUDE zDefine.inc}

interface

uses CoreClasses;

type
  SystemChar = Char;
  SystemString = string;
  THash = Cardinal;
  THash64 = UInt64;
  PSystemString = ^SystemString;
  PPascalString = ^TPascalString;
  TArrayChar = array of SystemChar;
  TOrdChar = (c0to9, c1to9, c0to32, c0to32no10, cLoAtoF, cHiAtoF, cLoAtoZ, cHiAtoZ, cHex, cAtoF, cAtoZ, cVisibled);
  TOrdChars = set of TOrdChar;

  TPascalString = record
  private
    function GetText: SystemString;
    procedure SetText(const Value: SystemString);
    function GetLen: Integer;
    procedure SetLen(const Value: Integer);
    function GetChars(index: Integer): SystemChar;
    procedure SetChars(index: Integer; const Value: SystemChar);
    function GetBytes: TBytes;
    procedure SetBytes(const Value: TBytes);
    function GetPlatformBytes: TBytes;
    procedure SetPlatformBytes(const Value: TBytes);
    function GetANSI: TBytes;
    procedure SetANSI(const Value: TBytes);
    function GetLast: SystemChar;
    procedure SetLast(const Value: SystemChar);
    function GetFirst: SystemChar;
    procedure SetFirst(const Value: SystemChar);
    function GetUpperChar(index: Integer): SystemChar;
    procedure SetUpperChar(index: Integer; const Value: SystemChar);
    function GetLowerChar(index: Integer): SystemChar;
    procedure SetLowerChar(index: Integer; const Value: SystemChar);
  public
    buff: TArrayChar;

{$IFDEF DELPHI}
    class operator Equal(const Lhs, Rhs: TPascalString): Boolean;
    class operator NotEqual(const Lhs, Rhs: TPascalString): Boolean;
    class operator GreaterThan(const Lhs, Rhs: TPascalString): Boolean;
    class operator GreaterThanOrEqual(const Lhs, Rhs: TPascalString): Boolean;
    class operator LessThan(const Lhs, Rhs: TPascalString): Boolean;
    class operator LessThanOrEqual(const Lhs, Rhs: TPascalString): Boolean;

    class operator Add(const Lhs, Rhs: TPascalString): TPascalString;
    class operator Add(const Lhs: SystemString; const Rhs: TPascalString): TPascalString;
    class operator Add(const Lhs: TPascalString; const Rhs: SystemString): TPascalString;
    class operator Add(const Lhs: SystemChar; const Rhs: TPascalString): TPascalString;
    class operator Add(const Lhs: TPascalString; const Rhs: SystemChar): TPascalString;

    class operator Implicit(Value: RawByteString): TPascalString;
    class operator Implicit(Value: SystemString): TPascalString;
    class operator Implicit(Value: SystemChar): TPascalString;
    class operator Implicit(Value: TPascalString): SystemString;
    class operator Implicit(Value: TPascalString): Variant;

    class operator Explicit(Value: TPascalString): RawByteString;
    class operator Explicit(Value: TPascalString): SystemString;
    class operator Explicit(Value: SystemString): TPascalString;
    class operator Explicit(Value: SystemChar): TPascalString;
    class operator Explicit(Value: Variant): TPascalString;
    class operator Explicit(Value: TPascalString): Variant;
{$ENDIF}
    function Copy(index, Count: NativeInt): TPascalString;
    function Same(const p: PPascalString): Boolean; overload;
    function Same(const t: TPascalString): Boolean; overload;
    function Same(const t1, t2: TPascalString): Boolean; overload;
    function Same(const t1, t2, t3: TPascalString): Boolean; overload;
    function Same(const t1, t2, t3, t4: TPascalString): Boolean; overload;
    function Same(const t1, t2, t3, t4, t5: TPascalString): Boolean; overload;
    function Same(const IgnoreCase: Boolean; const t: TPascalString): Boolean; overload;
    function ComparePos(const Offset: Integer; const p: PPascalString): Boolean; overload;
    function ComparePos(const Offset: Integer; const t: TPascalString): Boolean; overload;
    function GetPos(const s: TPascalString; const Offset: Integer = 1): Integer; overload;
    function GetPos(const s: PPascalString; const Offset: Integer = 1): Integer; overload;
    function Exists(c: SystemChar): Boolean; overload;
    function Exists(c: array of SystemChar): Boolean; overload;
    function Exists(const s: TPascalString): Boolean; overload;
    function GetCharCount(c: SystemChar): Integer;

    function hash: THash;
    function Hash64: THash64;

    property Last: SystemChar read GetLast write SetLast;
    property First: SystemChar read GetFirst write SetFirst;

    procedure DeleteLast;
    procedure DeleteFirst;
    procedure Delete(idx, cnt: Integer);
    procedure Clear;
    procedure Append(t: TPascalString); overload;
    procedure Append(c: SystemChar); overload;
    procedure Append(const Fmt: SystemString; const Args: array of const); overload;
    function GetString(bPos, ePos: NativeInt): TPascalString;
    procedure Insert(AText: SystemString; idx: Integer);
    procedure FastAsText(var output: SystemString);
    procedure FastGetBytes(var output: TBytes);
    property Text: SystemString read GetText write SetText;
    function LowerText: SystemString;
    function UpperText: SystemString;
    function Invert: TPascalString;
    function TrimChar(const Chars: TPascalString): TPascalString;
    function DeleteChar(const Chars: TPascalString): TPascalString; overload;
    function DeleteChar(const Chars: TOrdChars): TPascalString; overload;
    function ReplaceChar(const Chars: TPascalString; const newChar: SystemChar): TPascalString; overload;
    function ReplaceChar(const Chars, newChar: SystemChar): TPascalString; overload;
    function ReplaceChar(const Chars: TOrdChars; const newChar: SystemChar): TPascalString; overload;

    function BuildPlatformPChar: Pointer;
    class procedure FreePlatformPChar(p: Pointer); static;

    class function RandomString(L_: Integer): TPascalString; static;

    { https://en.wikipedia.org/wiki/Smith%E2%80%93Waterman_algorithm }
    function SmithWaterman(const p: PPascalString): Double; overload;
    function SmithWaterman(const s: TPascalString): Double; overload;

    property Len: Integer read GetLen write SetLen;
    property L: Integer read GetLen write SetLen;
    property Chars[index: Integer]: SystemChar read GetChars write SetChars; default;
    property UpperChar[index: Integer]: SystemChar read GetUpperChar write SetUpperChar;
    property LowerChar[index: Integer]: SystemChar read GetLowerChar write SetLowerChar;
    property Bytes: TBytes read GetBytes write SetBytes;                         // UTF8
    property PlatformBytes: TBytes read GetPlatformBytes write SetPlatformBytes; // system default
    property ANSI: TBytes read GetANSI write SetANSI;                            // Ansi Bytes
    function BOMBytes: TBytes;
  end;

  TArrayPascalString = array of TPascalString;
  PArrayPascalString = ^TArrayPascalString;

  TArrayPascalStringPtr = array of PPascalString;
  PArrayPascalStringPtr = ^TArrayPascalStringPtr;

  TPStr = TPascalString;

function CharIn(c: SystemChar; const SomeChars: array of SystemChar): Boolean; overload;
function CharIn(c: SystemChar; const SomeChar: SystemChar): Boolean; overload;
function CharIn(c: SystemChar; const s: TPascalString): Boolean; overload;
function CharIn(c: SystemChar; const p: PPascalString): Boolean; overload;
function CharIn(c: SystemChar; const SomeCharsets: TOrdChars): Boolean; overload;
function CharIn(c: SystemChar; const SomeCharset: TOrdChar): Boolean; overload;
function CharIn(c: SystemChar; const SomeCharsets: TOrdChars; const SomeChars: TPascalString): Boolean; overload;
function CharIn(c: SystemChar; const SomeCharsets: TOrdChars; const p: PPascalString): Boolean; overload;

function FastHashPSystemString(const s: PSystemString): THash; overload;
function FastHash64PSystemString(const s: PSystemString): THash64; overload;

function FastHashSystemString(const s: SystemString): THash; overload;
function FastHash64SystemString(const s: SystemString): THash64; overload;

function FastHashPPascalString(const s: PPascalString): THash;
function FastHash64PPascalString(const s: PPascalString): THash64;

function PFormat(const Fmt: SystemString; const Args: array of const): SystemString;

{$IFDEF FPC}

operator := (const s: Variant)r: TPascalString;
operator := (const s: AnsiString)r: TPascalString;
operator := (const s: RawByteString)r: TPascalString;
operator := (const s: UnicodeString)r: TPascalString;
operator := (const s: WideString)r: TPascalString;
operator := (const s: ShortString)r: TPascalString;
operator := (const c: SystemChar)r: TPascalString;

operator := (const s: TPascalString)r: AnsiString;
operator := (const s: TPascalString)r: RawByteString;
operator := (const s: TPascalString)r: UnicodeString;
operator := (const s: TPascalString)r: WideString;
operator := (const s: TPascalString)r: ShortString;
operator := (const s: TPascalString)r: Variant;

operator = (const a: TPascalString; const b: TPascalString): Boolean;
operator <> (const a: TPascalString; const b: TPascalString): Boolean;
operator > (const a: TPascalString; const b: TPascalString): Boolean;
operator >= (const a: TPascalString; const b: TPascalString): Boolean;
operator < (const a: TPascalString; const b: TPascalString): Boolean;
operator <= (const a: TPascalString; const b: TPascalString): Boolean;

operator + (const a: TPascalString; const b: TPascalString): TPascalString;
operator + (const a: TPascalString; const b: SystemString): TPascalString;
operator + (const a: SystemString; const b: TPascalString): TPascalString;
operator + (const a: TPascalString; const b: SystemChar): TPascalString;
operator + (const a: SystemChar; const b: TPascalString): TPascalString;

{$ENDIF FPC}

{ https://en.wikipedia.org/wiki/Smith%E2%80%93Waterman_algorithm }

// short string likeness and out diff
function SmithWatermanCompare(const seq1, seq2: PPascalString; var diff1, diff2: TPascalString;
  const NoDiffChar: Boolean; const diffChar: SystemChar): Double; overload;
function SmithWatermanCompare(const seq1, seq2: PPascalString; var diff1, diff2: TPascalString): Double; overload;
function SmithWatermanCompare(const seq1, seq2: TPascalString; var diff1, diff2: TPascalString;
  const NoDiffChar: Boolean; const diffChar: SystemChar): Double; overload;
function SmithWatermanCompare(const seq1, seq2: TPascalString; var diff1, diff2: TPascalString): Double; overload;

// short string likeness
function SmithWatermanCompare(const seq1, seq2: PPascalString; out Same, Diff: Integer): Double; overload;
function SmithWatermanCompare(const seq1, seq2: PPascalString): Double; overload;
function SmithWatermanCompare(const seq1, seq2: TPascalString): Double; overload;
function SmithWatermanCompare(const seq1: TArrayPascalString; const seq2: TPascalString): Double; overload;

// memory likeness
function SmithWatermanCompare(const seq1: Pointer; siz1: Integer; const seq2: Pointer; siz2: Integer;
  out Same, Diff: Integer): Double; overload;
function SmithWatermanCompare(const seq1: Pointer; siz1: Integer; const seq2: Pointer; siz2: Integer): Double; overload;

// long string likeness
function SmithWatermanCompareLongString(const t1, t2: TPascalString; const MinDiffCharWithPeerLine: Integer; out Same, Diff: Integer): Double; overload;
function SmithWatermanCompareLongString(const t1, t2: TPascalString): Double; overload;

var
  SystemCharSize: NativeInt = SizeOf(SystemChar);
{$IFDEF CPU64}
  MaxSmithWatermanMatrix: NativeInt = 10000 * 10;
{$ELSE}
  MaxSmithWatermanMatrix: NativeInt = 8192;
{$ENDIF}


const
{$IFDEF FirstCharInZero}
  FirstCharPos = 0;
{$ELSE}
  FirstCharPos = 1;
{$ENDIF}

implementation

uses SysUtils, Variants;

procedure CombineCharsPP(const c1, c2: TArrayChar; var output: TArrayChar);
var
  LL, rl: Integer;
begin
  LL := length(c1);
  rl := length(c2);
  SetLength(output, LL + rl);
  if LL > 0 then
      CopyPtr(@c1[0], @output[0], LL * SystemCharSize);
  if rl > 0 then
      CopyPtr(@c2[0], @output[LL], rl * SystemCharSize);
end;

procedure CombineCharsSP(const c1: SystemString; const c2: TArrayChar; var output: TArrayChar);
var
  LL, rl: Integer;
begin
  LL := length(c1);
  rl := length(c2);
  SetLength(output, LL + rl);
  if LL > 0 then
      CopyPtr(@c1[FirstCharPos], @output[0], LL * SystemCharSize);
  if rl > 0 then
      CopyPtr(@c2[0], @output[LL], rl * SystemCharSize);
end;

procedure CombineCharsPS(const c1: TArrayChar; const c2: SystemString; var output: TArrayChar);
var
  LL, rl: Integer;
begin
  LL := length(c1);
  rl := length(c2);
  SetLength(output, LL + rl);
  if LL > 0 then
      CopyPtr(@c1[0], @output[0], LL * SystemCharSize);
  if rl > 0 then
      CopyPtr(@c2[FirstCharPos], @output[LL], rl * SystemCharSize);
end;

procedure CombineCharsCP(const c1: SystemChar; const c2: TArrayChar; var output: TArrayChar);
var
  rl: Integer;
begin
  rl := length(c2);
  SetLength(output, rl + 1);
  output[0] := c1;
  if rl > 0 then
      CopyPtr(@c2[0], @output[1], rl * SystemCharSize);
end;

procedure CombineCharsPC(const c1: TArrayChar; const c2: SystemChar; var output: TArrayChar);
var
  LL: Integer;
begin
  LL := length(c1);
  SetLength(output, LL + 1);
  if LL > 0 then
      CopyPtr(@c1[0], @output[0], LL * SystemCharSize);
  output[LL] := c2;
end;

function CharIn(c: SystemChar; const SomeChars: array of SystemChar): Boolean;
var
  AChar: SystemChar;
begin
  Result := True;
  for AChar in SomeChars do
    if AChar = c then
        Exit;
  Result := False;
end;

function CharIn(c: SystemChar; const SomeChar: SystemChar): Boolean;
begin
  Result := c = SomeChar;
end;

function CharIn(c: SystemChar; const s: TPascalString): Boolean;
begin
  Result := s.Exists(c);
end;

function CharIn(c: SystemChar; const p: PPascalString): Boolean;
begin
  Result := p^.Exists(c);
end;

function CharIn(c: SystemChar; const SomeCharset: TOrdChar): Boolean;
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
    c0to9: Result := (v >= ord0) and (v <= ord9);
    c1to9: Result := (v >= ord1) and (v <= ord9);
    c0to32: Result := ((v >= 0) and (v <= 32));
    c0to32no10: Result := ((v >= 0) and (v <= 32) and (v <> 10));
    cLoAtoF: Result := (v >= ordLA) and (v <= ordLF);
    cHiAtoF: Result := (v >= ordHA) and (v <= ordHF);
    cLoAtoZ: Result := (v >= ordLA) and (v <= ordLZ);
    cHiAtoZ: Result := (v >= ordHA) and (v <= ordHZ);
    cHex: Result := ((v >= ordLA) and (v <= ordLF)) or ((v >= ordHA) and (v <= ordHF)) or ((v >= ord0) and (v <= ord9));
    cAtoF: Result := ((v >= ordLA) and (v <= ordLF)) or ((v >= ordHA) and (v <= ordHF));
    cAtoZ: Result := ((v >= ordLA) and (v <= ordLZ)) or ((v >= ordHA) and (v <= ordHZ));
    cVisibled: Result := (v <= $20) and (v <= $7E);
    else Result := False;
  end;
end;

function CharIn(c: SystemChar; const SomeCharsets: TOrdChars): Boolean;
var
  i: TOrdChar;
begin
  Result := True;
  for i in SomeCharsets do
    if CharIn(c, i) then
        Exit;
  Result := False;
end;

function CharIn(c: SystemChar; const SomeCharsets: TOrdChars; const SomeChars: TPascalString): Boolean;
begin
  if CharIn(c, SomeCharsets) then
      Result := True
  else
      Result := CharIn(c, SomeChars);
end;

function CharIn(c: SystemChar; const SomeCharsets: TOrdChars; const p: PPascalString): Boolean;
begin
  if CharIn(c, SomeCharsets) then
      Result := True
  else
      Result := CharIn(c, p);
end;

function BytesOfPascalString(const s: TPascalString): TBytes;
begin
  Result := s.Bytes;
end;

function PascalStringOfBytes(const s: TBytes): TPascalString;
begin
  Result.Bytes := s;
end;

function FastHashPSystemString(const s: PSystemString): THash;
var
  i: Integer;
  c: SystemChar;
begin
  Result := 0;

{$IFDEF FirstCharInZero}
  for i := 0 to length(s^) - 1 do
{$ELSE}
  for i := 1 to length(s^) do
{$ENDIF}
    begin
      c := s^[i];
      if CharIn(c, cHiAtoZ) then
          inc(c, 32);
      Result := ((Result shl 7) or (Result shr 25)) + THash(c);
    end;
end;

function FastHash64PSystemString(const s: PSystemString): THash64;
var
  i: Integer;
  c: SystemChar;
begin
  Result := 0;

{$IFDEF FirstCharInZero}
  for i := 0 to length(s^) - 1 do
{$ELSE}
  for i := 1 to length(s^) do
{$ENDIF}
    begin
      c := s^[i];
      if CharIn(c, cHiAtoZ) then
          inc(c, 32);
      Result := ((Result shl 7) or (Result shr 57)) + THash64(c);
    end;
end;

function FastHashSystemString(const s: SystemString): THash;
begin
  Result := FastHashPSystemString(@s);
end;

function FastHash64SystemString(const s: SystemString): THash64;
begin
  Result := FastHash64PSystemString(@s);
end;

function FastHashPPascalString(const s: PPascalString): THash;
var
  i: Integer;
  c: SystemChar;
begin
  Result := 0;
  for i := 1 to s^.Len do
    begin
      c := s^[i];
      if CharIn(c, cHiAtoZ) then
          inc(c, 32);
      Result := ((Result shl 7) or (Result shr 25)) + THash(c);
    end;
end;

function FastHash64PPascalString(const s: PPascalString): THash64;
var
  i: Integer;
  c: SystemChar;
begin
  Result := 0;
  for i := 1 to s^.Len do
    begin
      c := s^[i];
      if CharIn(c, cHiAtoZ) then
          inc(c, 32);
      Result := ((Result shl 7) or (Result shr 57)) + THash64(c);
    end;
end;

function PFormat(const Fmt: SystemString; const Args: array of const): SystemString;
begin
  try
      Result := Format(Fmt, Args);
  except
      Result := Fmt;
  end;
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

function SmithWatermanCompare(const seq1, seq2: PPascalString; var diff1, diff2: TPascalString;
  const NoDiffChar: Boolean; const diffChar: SystemChar): Double;

  function InlineMatch(alphaC, betaC: SystemChar; const diffC: SystemChar): Integer; inline;
  begin
    if CharIn(alphaC, cLoAtoZ) then
        dec(alphaC, 32);
    if CharIn(betaC, cLoAtoZ) then
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
  align1, align2: TPascalString;
begin
  L1 := seq1^.Len;
  l2 := seq2^.Len;

  if (L1 = 0) or (l2 = 0) or (L1 > MaxSmithWatermanMatrix) or (l2 > MaxSmithWatermanMatrix) then
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

function SmithWatermanCompare(const seq1, seq2: PPascalString; var diff1, diff2: TPascalString): Double;
begin
  Result := SmithWatermanCompare(seq1, seq2, diff1, diff2, False, '-');
end;

function SmithWatermanCompare(const seq1, seq2: TPascalString; var diff1, diff2: TPascalString;
  const NoDiffChar: Boolean; const diffChar: SystemChar): Double;
begin
  Result := SmithWatermanCompare(@seq1, @seq2, diff1, diff2, NoDiffChar, diffChar);
end;

function SmithWatermanCompare(const seq1, seq2: TPascalString; var diff1, diff2: TPascalString): Double;
begin
  Result := SmithWatermanCompare(seq1, seq2, diff1, diff2, False, '-');
end;

function SmithWatermanCompare(const seq1, seq2: PPascalString; out Same, Diff: Integer): Double;

  function InlineMatch(alphaC, betaC: SystemChar): NativeInt; inline;
  begin
    if CharIn(alphaC, cLoAtoZ) then
        dec(alphaC, 32);
    if CharIn(betaC, cLoAtoZ) then
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

  if (L1 = 0) or (l2 = 0) or (L1 > MaxSmithWatermanMatrix) or (l2 > MaxSmithWatermanMatrix) then
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

function SmithWatermanCompare(const seq1, seq2: PPascalString): Double;
var
  Same, Diff: Integer;
begin
  Result := SmithWatermanCompare(seq1, seq2, Same, Diff);
end;

function SmithWatermanCompare(const seq1, seq2: TPascalString): Double;
begin
  Result := SmithWatermanCompare(@seq1, @seq2);
end;

function SmithWatermanCompare(const seq1: TArrayPascalString; const seq2: TPascalString): Double;
var
  i: Integer;
  r: Double;
begin
  Result := -1;
  for i := 0 to length(seq1) - 1 do
    begin
      r := SmithWatermanCompare(seq1[i], seq2);
      if r > Result then
          Result := r;
    end;
end;

function SmithWatermanCompare(const seq1: Pointer; siz1: Integer; const seq2: Pointer; siz2: Integer;
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

  if (L1 = 0) or (l2 = 0) or (L1 > MaxSmithWatermanMatrix) or (l2 > MaxSmithWatermanMatrix) then
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

function SmithWatermanCompare(const seq1: Pointer; siz1: Integer; const seq2: Pointer; siz2: Integer): Double;
var
  Same, Diff: Integer;
begin
  Result := SmithWatermanCompare(seq1, siz1, seq2, siz2, Same, Diff);
end;

function SmithWatermanCompareLongString(const t1, t2: TPascalString; const MinDiffCharWithPeerLine: Integer; out Same, Diff: Integer): Double;
type
  PSRec = ^TSRec;

  TSRec = record
    s: TPascalString;
  end;

  procedure _FillText(psPtr: PPascalString; outLst: TCoreClassList);
  var
    L_, i: Integer;
    n: TPascalString;
    p: PSRec;
  begin
    L_ := psPtr^.Len;
    i := 1;
    n := '';
    while i <= L_ do
      begin
        if CharIn(psPtr^[i], [#13, #10]) then
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
            until (i > L_) or (not CharIn(psPtr^[i], [#13, #10, #32, #9]));
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
    if SmithWatermanCompare(@alpha^.s, @beta^.s, cSame, cDiff) > 0 then
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

  if (L1 = 0) or (l2 = 0) or (L1 > MaxSmithWatermanMatrix) or (l2 > MaxSmithWatermanMatrix) then
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

function SmithWatermanCompareLongString(const t1, t2: TPascalString): Double;
var
  Same, Diff: Integer;
begin
  Result := SmithWatermanCompareLongString(t1, t2, 5, Same, Diff);
end;

{$IFDEF FPC}


operator := (const s: Variant)r: TPascalString;
begin
  r.Text := s;
end;

operator := (const s: AnsiString)r: TPascalString;
begin
  r.Text := s;
end;

operator := (const s: RawByteString)r: TPascalString;
begin
  r.Text := s;
end;

operator := (const s: UnicodeString)r: TPascalString;
begin
  r.Text := s;
end;

operator := (const s: WideString)r: TPascalString;
begin
  r.Text := s;
end;

operator := (const s: ShortString)r: TPascalString;
begin
  r.Text := s;
end;

operator := (const c: SystemChar)r: TPascalString;
begin
  r.Text := c;
end;

operator := (const s: TPascalString)r: AnsiString;
begin
  r := s.Text;
end;

operator := (const s: TPascalString)r: RawByteString;
begin
  r := s.Text;
end;

operator := (const s: TPascalString)r: UnicodeString;
begin
  r := s.Text;
end;

operator := (const s: TPascalString)r: WideString;
begin
  r := s.Text;
end;

operator := (const s: TPascalString)r: ShortString;
begin
  r := s.Text;
end;

operator := (const s: TPascalString)r: Variant;
begin
  r := s.Text;
end;

operator = (const a: TPascalString; const b: TPascalString): Boolean;
begin
  Result := a.Text = b.Text;
end;

operator <> (const a: TPascalString; const b: TPascalString): Boolean;
begin
  Result := a.Text <> b.Text;
end;

operator > (const a: TPascalString; const b: TPascalString): Boolean;
begin
  Result := a.Text > b.Text;
end;

operator >= (const a: TPascalString; const b: TPascalString): Boolean;
begin
  Result := a.Text >= b.Text;
end;

operator < (const a: TPascalString; const b: TPascalString): Boolean;
begin
  Result := a.Text < b.Text;
end;

operator <= (const a: TPascalString; const b: TPascalString): Boolean;
begin
  Result := a.Text <= b.Text;
end;

operator + (const a: TPascalString; const b: TPascalString): TPascalString;
begin
  CombineCharsPP(a.buff, b.buff, Result.buff);
end;

operator + (const a: TPascalString; const b: SystemString): TPascalString;
begin
  CombineCharsPS(a.buff, b, Result.buff);
end;

operator + (const a: SystemString; const b: TPascalString): TPascalString;
begin
  CombineCharsSP(a, b.buff, Result.buff);
end;

operator + (const a: TPascalString; const b: SystemChar): TPascalString;
begin
  CombineCharsPC(a.buff, b, Result.buff);
end;

operator + (const a: SystemChar; const b: TPascalString): TPascalString;
begin
  CombineCharsCP(a, b.buff, Result.buff);
end;

{$ENDIF}


function TPascalString.GetText: SystemString;
begin
  SetLength(Result, length(buff));
  if length(buff) > 0 then
      CopyPtr(@buff[0], @Result[FirstCharPos], length(buff) * SystemCharSize);
end;

procedure TPascalString.SetText(const Value: SystemString);
begin
  SetLength(buff, length(Value));

  if length(buff) > 0 then
      CopyPtr(@Value[FirstCharPos], @buff[0], length(buff) * SystemCharSize);
end;

function TPascalString.GetLen: Integer;
begin
  Result := length(buff);
end;

procedure TPascalString.SetLen(const Value: Integer);
begin
  SetLength(buff, Value);
end;

function TPascalString.GetChars(index: Integer): SystemChar;
begin
  if (index > length(buff)) or (index <= 0) then
      Result := #0
  else
      Result := buff[index - 1];
end;

procedure TPascalString.SetChars(index: Integer; const Value: SystemChar);
begin
  buff[index - 1] := Value;
end;

function TPascalString.GetBytes: TBytes;
begin
  SetLength(Result, 0);
  if length(buff) = 0 then
      Exit;
{$IFDEF FPC}
  Result := SysUtils.TEncoding.UTF8.GetBytes(Text);
{$ELSE}
  Result := SysUtils.TEncoding.UTF8.GetBytes(buff);
{$ENDIF}
end;

procedure TPascalString.SetBytes(const Value: TBytes);
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

function TPascalString.GetPlatformBytes: TBytes;
begin
  SetLength(Result, 0);
  if length(buff) = 0 then
      Exit;
{$IFDEF FPC}
  Result := SysUtils.TEncoding.Default.GetBytes(Text);
{$ELSE}
  Result := SysUtils.TEncoding.Default.GetBytes(buff);
{$ENDIF}
end;

procedure TPascalString.SetPlatformBytes(const Value: TBytes);
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

function TPascalString.GetANSI: TBytes;
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

procedure TPascalString.SetANSI(const Value: TBytes);
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

function TPascalString.GetLast: SystemChar;
begin
  if length(buff) > 0 then
      Result := buff[length(buff) - 1]
  else
      Result := #0;
end;

procedure TPascalString.SetLast(const Value: SystemChar);
begin
  buff[length(buff) - 1] := Value;
end;

function TPascalString.GetFirst: SystemChar;
begin
  if length(buff) > 0 then
      Result := buff[0]
  else
      Result := #0;
end;

procedure TPascalString.SetFirst(const Value: SystemChar);
begin
  buff[0] := Value;
end;

function TPascalString.GetUpperChar(index: Integer): SystemChar;
begin
  Result := GetChars(index);
  if CharIn(Result, cLoAtoZ) then
      Result := SystemChar(Word(Result) xor $0020);
end;

procedure TPascalString.SetUpperChar(index: Integer; const Value: SystemChar);
begin
  if CharIn(Value, cLoAtoZ) then
      SetChars(index, SystemChar(Word(Value) xor $0020))
  else
      SetChars(index, Value);
end;

function TPascalString.GetLowerChar(index: Integer): SystemChar;
begin
  Result := GetChars(index);
  if CharIn(Result, cHiAtoZ) then
      Result := SystemChar(Word(Result) or $0020);
end;

procedure TPascalString.SetLowerChar(index: Integer; const Value: SystemChar);
begin
  if CharIn(Value, cHiAtoZ) then
      SetChars(index, SystemChar(Word(Value) or $0020))
  else
      SetChars(index, Value);
end;

{$IFDEF DELPHI}


class operator TPascalString.Equal(const Lhs, Rhs: TPascalString): Boolean;
begin
  Result := (Lhs.Len = Rhs.Len) and (Lhs.Text = Rhs.Text);
end;

class operator TPascalString.NotEqual(const Lhs, Rhs: TPascalString): Boolean;
begin
  Result := not(Lhs = Rhs);
end;

class operator TPascalString.GreaterThan(const Lhs, Rhs: TPascalString): Boolean;
begin
  Result := Lhs.Text > Rhs.Text;
end;

class operator TPascalString.GreaterThanOrEqual(const Lhs, Rhs: TPascalString): Boolean;
begin
  Result := Lhs.Text >= Rhs.Text;
end;

class operator TPascalString.LessThan(const Lhs, Rhs: TPascalString): Boolean;
begin
  Result := Lhs.Text < Rhs.Text;
end;

class operator TPascalString.LessThanOrEqual(const Lhs, Rhs: TPascalString): Boolean;
begin
  Result := Lhs.Text <= Rhs.Text;
end;

class operator TPascalString.Add(const Lhs, Rhs: TPascalString): TPascalString;
begin
  CombineCharsPP(Lhs.buff, Rhs.buff, Result.buff);
end;

class operator TPascalString.Add(const Lhs: SystemString; const Rhs: TPascalString): TPascalString;
begin
  CombineCharsSP(Lhs, Rhs.buff, Result.buff);
end;

class operator TPascalString.Add(const Lhs: TPascalString; const Rhs: SystemString): TPascalString;
begin
  CombineCharsPS(Lhs.buff, Rhs, Result.buff);
end;

class operator TPascalString.Add(const Lhs: SystemChar; const Rhs: TPascalString): TPascalString;
begin
  CombineCharsCP(Lhs, Rhs.buff, Result.buff);
end;

class operator TPascalString.Add(const Lhs: TPascalString; const Rhs: SystemChar): TPascalString;
begin
  CombineCharsPC(Lhs.buff, Rhs, Result.buff);
end;

class operator TPascalString.Implicit(Value: RawByteString): TPascalString;
begin
  Result.Text := Value;
end;

class operator TPascalString.Implicit(Value: SystemString): TPascalString;
begin
  Result.Text := Value;
end;

class operator TPascalString.Implicit(Value: SystemChar): TPascalString;
begin
  Result.Len := 1;
  Result.buff[0] := Value;
end;

class operator TPascalString.Implicit(Value: TPascalString): SystemString;
begin
  Result := Value.Text;
end;

class operator TPascalString.Implicit(Value: TPascalString): Variant;
begin
  Result := Value.Text;
end;

class operator TPascalString.Explicit(Value: TPascalString): RawByteString;
begin
  Result := Value.Text;
end;

class operator TPascalString.Explicit(Value: TPascalString): SystemString;
begin
  Result := Value.Text;
end;

class operator TPascalString.Explicit(Value: TPascalString): Variant;
begin
  Result := Value.Text;
end;

class operator TPascalString.Explicit(Value: SystemString): TPascalString;
begin
  Result.Text := Value;
end;

class operator TPascalString.Explicit(Value: Variant): TPascalString;
begin
  Result.Text := VarToStr(Value);
end;

class operator TPascalString.Explicit(Value: SystemChar): TPascalString;
begin
  Result.Len := 1;
  Result.buff[0] := Value;
end;

{$ENDIF}


function TPascalString.Copy(index, Count: NativeInt): TPascalString;
var
  L_: NativeInt;
begin
  L_ := length(buff);

  if (index - 1) + Count > L_ then
      Count := L_ - (index - 1);

  SetLength(Result.buff, Count);
  if Count > 0 then
      CopyPtr(@buff[index - 1], @Result.buff[0], SystemCharSize * Count);
end;

function TPascalString.Same(const p: PPascalString): Boolean;
var
  i: Integer;
  s, d: SystemChar;
begin
  Result := (p^.Len = Len);
  if not Result then
      Exit;
  for i := 0 to Len - 1 do
    begin
      s := buff[i];
      if CharIn(s, cHiAtoZ) then
          inc(s, 32);
      d := p^.buff[i];
      if CharIn(d, cHiAtoZ) then
          inc(d, 32);
      if s <> d then
          Exit(False);
    end;
end;

function TPascalString.Same(const t: TPascalString): Boolean;
var
  i: Integer;
  s, d: SystemChar;
begin
  Result := (t.Len = Len);
  if not Result then
      Exit;
  for i := 0 to Len - 1 do
    begin
      s := buff[i];
      if CharIn(s, cHiAtoZ) then
          inc(s, 32);
      d := t.buff[i];
      if CharIn(d, cHiAtoZ) then
          inc(d, 32);
      if s <> d then
          Exit(False);
    end;
end;

function TPascalString.Same(const t1, t2: TPascalString): Boolean;
begin
  Result := Same(@t1) or Same(@t2);
end;

function TPascalString.Same(const t1, t2, t3: TPascalString): Boolean;
begin
  Result := Same(@t1) or Same(@t2) or Same(@t3);
end;

function TPascalString.Same(const t1, t2, t3, t4: TPascalString): Boolean;
begin
  Result := Same(@t1) or Same(@t2) or Same(@t3) or Same(@t4);
end;

function TPascalString.Same(const t1, t2, t3, t4, t5: TPascalString): Boolean;
begin
  Result := Same(@t1) or Same(@t2) or Same(@t3) or Same(@t4) or Same(@t5);
end;

function TPascalString.Same(const IgnoreCase: Boolean; const t: TPascalString): Boolean;
var
  i: Integer;
  s, d: SystemChar;
begin
  Result := (t.Len = Len);
  if not Result then
      Exit;
  for i := 0 to Len - 1 do
    begin
      s := buff[i];
      if IgnoreCase then
        if CharIn(s, cHiAtoZ) then
            inc(s, 32);

      d := t.buff[i];
      if IgnoreCase then
        if CharIn(d, cHiAtoZ) then
            inc(d, 32);

      if s <> d then
          Exit(False);
    end;
end;

function TPascalString.ComparePos(const Offset: Integer; const p: PPascalString): Boolean;
var
  i, L_: Integer;
  sourChar, destChar: SystemChar;
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

      if CharIn(sourChar, cLoAtoZ) then
          dec(sourChar, 32);
      if CharIn(destChar, cLoAtoZ) then
          dec(destChar, 32);

      if sourChar <> destChar then
          Exit;
      inc(i);
    end;
  Result := True;
end;

function TPascalString.ComparePos(const Offset: Integer; const t: TPascalString): Boolean;
var
  i, L_: Integer;
  sourChar, destChar: SystemChar;
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

      if CharIn(sourChar, cLoAtoZ) then
          dec(sourChar, 32);
      if CharIn(destChar, cLoAtoZ) then
          dec(destChar, 32);

      if sourChar <> destChar then
          Exit;
      inc(i);
    end;
  Result := True;
end;

function TPascalString.GetPos(const s: TPascalString; const Offset: Integer = 1): Integer;
var
  i: Integer;
begin
  Result := 0;
  if s.Len > 0 then
    for i := Offset to Len - s.Len + 1 do
      if ComparePos(i, @s) then
          Exit(i);
end;

function TPascalString.GetPos(const s: PPascalString; const Offset: Integer = 1): Integer;
var
  i: Integer;
begin
  Result := 0;
  if s^.Len > 0 then
    for i := Offset to Len - s^.Len + 1 do
      if ComparePos(i, s) then
          Exit(i);
end;

function TPascalString.Exists(c: SystemChar): Boolean;
var
  i: Integer;
begin
  for i := low(buff) to high(buff) do
    if buff[i] = c then
        Exit(True);
  Result := False;
end;

function TPascalString.Exists(c: array of SystemChar): Boolean;
var
  i: Integer;
begin
  for i := low(buff) to high(buff) do
    if CharIn(buff[i], c) then
        Exit(True);
  Result := False;
end;

function TPascalString.Exists(const s: TPascalString): Boolean;
begin
  Result := GetPos(@s, 1) > 0;
end;

function TPascalString.GetCharCount(c: SystemChar): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := low(buff) to high(buff) do
    if CharIn(buff[i], c) then
        inc(Result);
end;

function TPascalString.hash: THash;
begin
  Result := FastHashPPascalString(@Self);
end;

function TPascalString.Hash64: THash64;
begin
  Result := FastHash64PPascalString(@Self);
end;

procedure TPascalString.DeleteLast;
begin
  if Len > 0 then
      SetLength(buff, length(buff) - 1);
end;

procedure TPascalString.DeleteFirst;
begin
  if Len > 0 then
      buff := System.Copy(buff, 1, Len);
end;

procedure TPascalString.Delete(idx, cnt: Integer);
begin
  if (idx + cnt <= Len) then
      Text := GetString(1, idx) + GetString(idx + cnt, Len + 1)
  else
      Text := GetString(1, idx);
end;

procedure TPascalString.Clear;
begin
  SetLength(buff, 0);
end;

procedure TPascalString.Append(t: TPascalString);
var
  r, L_: Integer;
begin
  L_ := length(t.buff);
  if L_ > 0 then
    begin
      r := length(buff);
      SetLength(buff, r + L_);
      CopyPtr(@t.buff[0], @buff[r], L_ * SystemCharSize);
    end;
end;

procedure TPascalString.Append(c: SystemChar);
begin
  SetLength(buff, length(buff) + 1);
  buff[length(buff) - 1] := c;
end;

procedure TPascalString.Append(const Fmt: SystemString; const Args: array of const);
begin
  Append(PFormat(Fmt, Args));
end;

function TPascalString.GetString(bPos, ePos: NativeInt): TPascalString;
begin
  if ePos > length(buff) then
      Result := Self.Copy(bPos, length(buff) - bPos + 1)
  else
      Result := Self.Copy(bPos, (ePos - bPos));
end;

procedure TPascalString.Insert(AText: SystemString; idx: Integer);
begin
  Text := GetString(1, idx) + AText + GetString(idx + 1, Len);
end;

procedure TPascalString.FastAsText(var output: SystemString);
begin
  SetLength(output, length(buff));
  if length(buff) > 0 then
      CopyPtr(@buff[0], @output[FirstCharPos], length(buff) * SystemCharSize);
end;

procedure TPascalString.FastGetBytes(var output: TBytes);
begin
{$IFDEF FPC}
  output := SysUtils.TEncoding.UTF8.GetBytes(Text);
{$ELSE}
  output := SysUtils.TEncoding.UTF8.GetBytes(buff);
{$ENDIF}
end;

function TPascalString.LowerText: SystemString;
begin
  Result := LowerCase(Text);
end;

function TPascalString.UpperText: SystemString;
begin
  Result := UpperCase(Text);
end;

function TPascalString.Invert: TPascalString;
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

function TPascalString.TrimChar(const Chars: TPascalString): TPascalString;
var
  L_, bp, EP: Integer;
begin
  Result := '';
  L_ := Len;
  if L_ > 0 then
    begin
      bp := 1;
      while CharIn(GetChars(bp), @Chars) do
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

          while CharIn(GetChars(EP), @Chars) do
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

function TPascalString.DeleteChar(const Chars: TPascalString): TPascalString;
var
  c: SystemChar;
begin
  Result := '';
  for c in buff do
    if not CharIn(c, @Chars) then
        Result.Append(c);
end;

function TPascalString.DeleteChar(const Chars: TOrdChars): TPascalString;
var
  c: SystemChar;
begin
  Result := '';
  for c in buff do
    if not CharIn(c, Chars) then
        Result.Append(c);
end;

function TPascalString.ReplaceChar(const Chars: TPascalString; const newChar: SystemChar): TPascalString;
var
  i: Integer;
begin
  Result.Len := Len;
  for i := low(buff) to high(buff) do
    if CharIn(buff[i], Chars) then
        Result.buff[i] := newChar
    else
        Result.buff[i] := buff[i];
end;

function TPascalString.ReplaceChar(const Chars, newChar: SystemChar): TPascalString;
var
  i: Integer;
begin
  Result.Len := Len;
  for i := low(buff) to high(buff) do
    if CharIn(buff[i], Chars) then
        Result.buff[i] := newChar
    else
        Result.buff[i] := buff[i];
end;

function TPascalString.ReplaceChar(const Chars: TOrdChars; const newChar: SystemChar): TPascalString;
var
  i: Integer;
begin
  Result.Len := Len;
  for i := low(buff) to high(buff) do
    if CharIn(buff[i], Chars) then
        Result.buff[i] := newChar
    else
        Result.buff[i] := buff[i];
end;

function TPascalString.BuildPlatformPChar: Pointer;
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

class procedure TPascalString.FreePlatformPChar(p: Pointer);
begin
  FreeMemory(p);
end;

class function TPascalString.RandomString(L_: Integer): TPascalString;
var
  i: Integer;
  rnd: TMT19937Random;
begin
  Result.L := L_;
  rnd := TMT19937Random.Create;
  for i := 1 to L_ do
      Result[i] := SystemChar(rnd.Rand32($7E - $20) + $20);
  DisposeObject(rnd);
end;

function TPascalString.SmithWaterman(const p: PPascalString): Double;
begin
  Result := SmithWatermanCompare(@Self, @p);
end;

function TPascalString.SmithWaterman(const s: TPascalString): Double;
begin
  Result := SmithWatermanCompare(@Self, @s);
end;

function TPascalString.BOMBytes: TBytes;
begin
{$IFDEF FPC}
  Result := GetBytes;
{$ELSE}
  Result := SysUtils.TEncoding.UTF8.GetPreamble + GetBytes;
{$ENDIF}
end;

end.
