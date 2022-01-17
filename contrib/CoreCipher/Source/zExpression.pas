{ * zExpression                                                                * }
{ ****************************************************************************** }
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
unit zExpression;

{$INCLUDE zDefine.inc}

interface

uses SysUtils, Variants, CoreClasses, TypInfo, TextParsing, PascalStrings, UnicodeMixedLib,
  DoStatusIO, ListEngine, OpCode;

type
{$REGION 'internal define'}
  TSymbolOperation = (soAdd, soSub, soMul, soDiv, soMod, soIntDiv, soPow, soOr, soAnd, soXor, // math
    soEqual, soLessThan, soEqualOrLessThan, soGreaterThan, soEqualOrGreaterThan, soNotEqual,  // logic
    soShl, soShr,                                                                             // bit
    soBlockIndentBegin, soBlockIndentEnd,                                                     // block indent
    soPropIndentBegin, soPropIndentEnd,                                                       // property indent
    soDotSymbol, soCommaSymbol,                                                               // dot and comma
    soEolSymbol,                                                                              // eol
    soProc, soParameter,                                                                      // proc
    soUnknow);
  TSymbolOperations = set of TSymbolOperation;

  TExpressionDeclType = (
    edtSymbol,                                                                                 // symbol
    edtBool, edtInt, edtInt64, edtUInt64, edtWord, edtByte, edtSmallInt, edtShortInt, edtUInt, // inbuild byte type
    edtSingle, edtDouble, edtCurrency,                                                         // inbuild float type
    edtString,                                                                                 // string
    edtProcExp,                                                                                // proc
    edtExpressionAsValue,                                                                      // expression
    edtUnknow);

  TExpressionDeclTypes = set of TExpressionDeclType;

  TSymbolExpression = class;

  TExpressionListData = record
    DeclType: TExpressionDeclType; // declaration
    charPos: Integer;              // char pos
    Symbol: TSymbolOperation;      // symbol
    Value: Variant;                // value
    Expression: TSymbolExpression; // expression
    ExpressionAutoFree: Boolean;   // autofree
  end;

  PExpressionListData = ^TExpressionListData;

  TNumTextType = (nttBool, nttInt, nttInt64, nttUInt64, nttWord, nttByte,
    nttSmallInt, nttShortInt, nttUInt,
    nttSingle, nttDouble, nttCurrency,
    nttUnknow);

  TSymbolExpression = class sealed(TCoreClassObject)
  protected
    FList: TCoreClassList;
    FTextStyle: TTextStyle;
  public
    constructor Create(const TextStyle_: TTextStyle);
    destructor Destroy; override;

    property TextStyle: TTextStyle read FTextStyle;
    procedure Clear;
    procedure PrintDebug(const detail: Boolean; const prefix: SystemString); overload;
    procedure PrintDebug(const detail: Boolean); overload;
    function Decl(): SystemString;

    function GetCount(t: TExpressionDeclTypes): Integer;
    function GetSymbolCount(Operations: TSymbolOperations): Integer;
    function AvailValueCount: Integer;
    function Count: Integer;

    function InsertSymbol(const idx: Integer; v: TSymbolOperation; charPos: Integer): PExpressionListData;
    function Insert(const idx: Integer; v: TExpressionListData): PExpressionListData;
    procedure InsertExpression(const idx: Integer; E: TSymbolExpression);
    procedure AddExpression(const E: TSymbolExpression);
    function AddSymbol(const v: TSymbolOperation; charPos: Integer): PExpressionListData;
    function AddBool(const v: Boolean; charPos: Integer): PExpressionListData;
    function AddInt(const v: Integer; charPos: Integer): PExpressionListData;
    function AddUInt(const v: Cardinal; charPos: Integer): PExpressionListData;
    function AddInt64(const v: Int64; charPos: Integer): PExpressionListData;
    function AddUInt64(const v: UInt64; charPos: Integer): PExpressionListData;
    function AddWord(const v: Word; charPos: Integer): PExpressionListData;
    function AddByte(const v: Byte; charPos: Integer): PExpressionListData;
    function AddSmallInt(const v: SmallInt; charPos: Integer): PExpressionListData;
    function AddShortInt(const v: ShortInt; charPos: Integer): PExpressionListData;
    function AddSingle(const v: Single; charPos: Integer): PExpressionListData;
    function AddDouble(const v: Double; charPos: Integer): PExpressionListData;
    function AddCurrency(const v: Currency; charPos: Integer): PExpressionListData;
    function AddString(const v: SystemString; charPos: Integer): PExpressionListData;
    function AddFunc(const v: SystemString; charPos: Integer): PExpressionListData;
    function AddExpressionAsValue(AutoFree: Boolean; Expression: TSymbolExpression; Symbol: TSymbolOperation; Value: Variant; charPos: Integer): PExpressionListData;
    function Add(const v: TExpressionListData): PExpressionListData;
    function AddCopy(const v: TExpressionListData): PExpressionListData;

    procedure Delete(const idx: Integer);
    procedure DeleteLast;

    function Last: PExpressionListData;
    function First: PExpressionListData;

    function IndexOf(p: PExpressionListData): Integer;

    function GetItems(index: Integer): PExpressionListData;
    property Items[index: Integer]: PExpressionListData read GetItems; default;
  end;

  TOnDeclValueCall = procedure(const Decl: SystemString; var ValType: TExpressionDeclType; var Value: Variant);
  TOnDeclValueMethod = procedure(const Decl: SystemString; var ValType: TExpressionDeclType; var Value: Variant) of object;
{$IFDEF FPC}
  TOnDeclValueProc = procedure(const Decl: SystemString; var ValType: TExpressionDeclType; var Value: Variant) is nested;
{$ELSE FPC}
  TOnDeclValueProc = reference to procedure(const Decl: SystemString; var ValType: TExpressionDeclType; var Value: Variant);
{$ENDIF FPC}
  //
  { text parse support }
  TExpressionParsingState = set of (esFirst, esWaitOp, esWaitIndentEnd, esWaitPropParamIndentEnd, esWaitValue);
  PExpressionParsingState = ^TExpressionParsingState;

  { variant array vector }
  TExpressionValueVector = array of Variant;
  PExpressionValueVector = ^TExpressionValueVector;

  { aligned variant matrix }
  TExpressionValueMatrix = array of TExpressionValueVector;
  PExpressionValueMatrix = ^TExpressionValueMatrix;

  // other
function NumTextType(s: TPascalString): TNumTextType;
procedure InitExp(var v: TExpressionListData);
function dt2op(const v: TExpressionDeclType): TOpValueType;
function VariantToExpressionDeclType(var v: Variant): TExpressionDeclType;

function ParseOperationState(ParsingEng: TTextParsing;
  var cPos, bPos, ePos, BlockIndent, PropIndent: Integer; var pStates: TExpressionParsingState): TSymbolOperation;

function ParseSymbol(ParsingEng: TTextParsing; WorkSym: TSymbolExpression;
  var cPos, bPos, ePos, BlockIndent, PropIndent: Integer; pStates: PExpressionParsingState): Boolean;

function __ParseTextExpressionAsSymbol(ParsingEng: TTextParsing; const uName: SystemString;
  const OnDeclValueCall: TOnDeclValueCall; const OnDeclValueMethod: TOnDeclValueMethod; const OnDeclValueProc: TOnDeclValueProc;
  RefrenceOpRT: TOpCustomRunTime): TSymbolExpression;

// parsing text as expression structor, backcall is TOnDeclValueCall
function ParseTextExpressionAsSymbol_C(ParsingEng: TTextParsing; const uName: SystemString;
  const OnGetValue: TOnDeclValueCall; RefrenceOpRT: TOpCustomRunTime): TSymbolExpression; overload;

// parsing text as expression structor, backcall is TOnDeclValueMethod
function ParseTextExpressionAsSymbol_M(ParsingEng: TTextParsing; const uName: SystemString;
  const OnGetValue: TOnDeclValueMethod; RefrenceOpRT: TOpCustomRunTime): TSymbolExpression; overload;

// parsing text as expression structor, backcall is TOnDeclValueProc
function ParseTextExpressionAsSymbol_P(ParsingEng: TTextParsing; const uName: SystemString;
  const OnGetValue: TOnDeclValueProc; RefrenceOpRT: TOpCustomRunTime): TSymbolExpression; overload;

// parsing text as expression structor
function ParseTextExpressionAsSymbol(SpecialAsciiToken: TListPascalString;
  TextStyle: TTextStyle; const uName, ExpressionText: SystemString;
  const OnGetValue: TOnDeclValueMethod; RefrenceOpRT: TOpCustomRunTime): TSymbolExpression; overload;

// parsing text as expression structor
function ParseTextExpressionAsSymbol(TextStyle: TTextStyle; const uName, ExpressionText: SystemString;
  const OnGetValue: TOnDeclValueMethod; RefrenceOpRT: TOpCustomRunTime): TSymbolExpression; overload;

// parsing text as expression structor
function ParseTextExpressionAsSymbol(SpecialAsciiToken: TListPascalString; ExpressionText: SystemString; RefrenceOpRT: TOpCustomRunTime): TSymbolExpression; overload;
function ParseTextExpressionAsSymbol(ExpressionText: SystemString; RefrenceOpRT: TOpCustomRunTime): TSymbolExpression; overload;

// parsing text as expression structor
function ParseTextExpressionAsSymbol(SpecialAsciiToken: TListPascalString; ExpressionText: SystemString): TSymbolExpression; overload;
function ParseTextExpressionAsSymbol(ExpressionText: SystemString): TSymbolExpression; overload;

// parsing text as expression structor
function ParseTextExpressionAsSymbol_M(SpecialAsciiToken: TListPascalString;
  TextEngClass: TTextParsingClass; TextStyle: TTextStyle; const uName, ExpressionText: SystemString;
  const OnGetValue: TOnDeclValueMethod; RefrenceOpRT: TOpCustomRunTime): TSymbolExpression; overload;
// parsing text as expression structor
function ParseTextExpressionAsSymbol_M(TextEngClass: TTextParsingClass; TextStyle: TTextStyle; const uName, ExpressionText: SystemString;
  const OnGetValue: TOnDeclValueMethod; RefrenceOpRT: TOpCustomRunTime): TSymbolExpression; overload;

// parsing text as expression structor
function ParseTextExpressionAsSymbol_C(SpecialAsciiToken: TListPascalString;
  TextEngClass: TTextParsingClass; TextStyle: TTextStyle; const uName, ExpressionText: SystemString;
  const OnGetValue: TOnDeclValueCall; RefrenceOpRT: TOpCustomRunTime): TSymbolExpression; overload;
// parsing text as expression structor
function ParseTextExpressionAsSymbol_C(TextEngClass: TTextParsingClass; TextStyle: TTextStyle; const uName, ExpressionText: SystemString;
  const OnGetValue: TOnDeclValueCall; RefrenceOpRT: TOpCustomRunTime): TSymbolExpression; overload;

// parsing text as expression structor
function ParseTextExpressionAsSymbol_P(SpecialAsciiToken: TListPascalString;
  TextEngClass: TTextParsingClass; TextStyle: TTextStyle; const uName, ExpressionText: SystemString;
  const OnGetValue: TOnDeclValueProc; RefrenceOpRT: TOpCustomRunTime): TSymbolExpression; overload;
// parsing text as expression structor
function ParseTextExpressionAsSymbol_P(TextEngClass: TTextParsingClass; TextStyle: TTextStyle; const uName, ExpressionText: SystemString;
  const OnGetValue: TOnDeclValueProc; RefrenceOpRT: TOpCustomRunTime): TSymbolExpression; overload;

// symbol priority
function RebuildLogicalPrioritySymbol(Exps: TSymbolExpression): TSymbolExpression;

// format symbol
function RebuildAllSymbol(Exps: TSymbolExpression): TSymbolExpression;

// build opCode
function BuildAsOpCode(DebugMode: Boolean; SymbExps: TSymbolExpression; const uName: SystemString; LineNo: Integer): TOpCode; overload;
function BuildAsOpCode(SymbExps: TSymbolExpression): TOpCode; overload;
function BuildAsOpCode(DebugMode: Boolean; SymbExps: TSymbolExpression): TOpCode; overload;
function BuildAsOpCode(DebugMode: Boolean; TextStyle: TTextStyle; ExpressionText: SystemString): TOpCode; overload;
function BuildAsOpCode(TextStyle: TTextStyle; ExpressionText: SystemString): TOpCode; overload;
function BuildAsOpCode(ExpressionText: SystemString): TOpCode; overload;
function BuildAsOpCode(DebugMode: Boolean; TextStyle: TTextStyle; ExpressionText: SystemString; RefrenceOpRT: TOpCustomRunTime): TOpCode; overload;
function BuildAsOpCode(TextStyle: TTextStyle; ExpressionText: SystemString; RefrenceOpRT: TOpCustomRunTime): TOpCode; overload;
function BuildAsOpCode(ExpressionText: SystemString; RefrenceOpRT: TOpCustomRunTime): TOpCode; overload;

// Evaluate Expression
function EvaluateExpressionValue_M(UsedCache: Boolean; SpecialAsciiToken: TListPascalString;
  TextEngClass: TTextParsingClass; TextStyle: TTextStyle; ExpressionText: SystemString; const OnGetValue: TOnDeclValueMethod): Variant;
function EvaluateExpressionValue_C(UsedCache: Boolean; SpecialAsciiToken: TListPascalString;
  TextEngClass: TTextParsingClass; TextStyle: TTextStyle; ExpressionText: SystemString; const OnGetValue: TOnDeclValueCall): Variant;
function EvaluateExpressionValue_P(UsedCache: Boolean; SpecialAsciiToken: TListPascalString;
  TextEngClass: TTextParsingClass; TextStyle: TTextStyle; ExpressionText: SystemString; const OnGetValue: TOnDeclValueProc): Variant;
{$ENDREGION 'internal define'}

procedure CleanOpCache();

{ prototype: EvaluateExpressionValue }
function IsSymbolVectorExpression(ExpressionText: SystemString; TextStyle: TTextStyle; SpecialAsciiToken: TListPascalString): Boolean;
function EvaluateExpressionValue(UsedCache: Boolean;
  SpecialAsciiToken: TListPascalString; DebugMode: Boolean; TextStyle: TTextStyle; ExpressionText: SystemString;
  opRT: TOpCustomRunTime; const_vl: THashVariantList): Variant; overload;
function EvaluateExpressionValue(UsedCache: Boolean; SpecialAsciiToken: TListPascalString; DebugMode: Boolean; TextStyle: TTextStyle; ExpressionText: SystemString; opRT: TOpCustomRunTime): Variant; overload;

// select used Cache
function EvaluateExpressionValue(UsedCache: Boolean; ExpressionText: SystemString; opRT: TOpCustomRunTime): Variant; overload;
function EvaluateExpressionValue(UsedCache: Boolean; ExpressionText: SystemString): Variant; overload;
function EvaluateExpressionValue(UsedCache: Boolean; TextStyle: TTextStyle; ExpressionText: SystemString): Variant; overload;
function EvaluateExpressionValue(UsedCache: Boolean; TextStyle: TTextStyle; ExpressionText: SystemString; opRT: TOpCustomRunTime): Variant; overload;
function EvaluateExpressionValue(UsedCache: Boolean; SpecialAsciiToken: TListPascalString; DebugMode: Boolean; ExpressionText: SystemString; opRT: TOpCustomRunTime): Variant; overload;
function EvaluateExpressionValue(UsedCache: Boolean; SpecialAsciiToken: TListPascalString; ExpressionText: SystemString; opRT: TOpCustomRunTime): Variant; overload;
function EvaluateExpressionValue(UsedCache: Boolean; SpecialAsciiToken: TListPascalString; DebugMode: Boolean; ExpressionText: SystemString): Variant; overload;
function EvaluateExpressionValue(UsedCache: Boolean; SpecialAsciiToken: TListPascalString; ExpressionText: SystemString): Variant; overload;
function EvaluateExpressionValue(UsedCache: Boolean; SpecialAsciiToken: TListPascalString; TextStyle: TTextStyle; ExpressionText: SystemString; opRT: TOpCustomRunTime): Variant; overload;

// used Cache
function EvaluateExpressionValue(ExpressionText: SystemString; opRT: TOpCustomRunTime): Variant; overload;
function EvaluateExpressionValue(ExpressionText: SystemString): Variant; overload;
function EvaluateExpressionValue(TextStyle: TTextStyle; ExpressionText: SystemString): Variant; overload;
function EvaluateExpressionValue(TextStyle: TTextStyle; ExpressionText: SystemString; opRT: TOpCustomRunTime): Variant; overload;
function EvaluateExpressionValue(SpecialAsciiToken: TListPascalString; DebugMode: Boolean; ExpressionText: SystemString; opRT: TOpCustomRunTime): Variant; overload;
function EvaluateExpressionValue(SpecialAsciiToken: TListPascalString; ExpressionText: SystemString; opRT: TOpCustomRunTime): Variant; overload;
function EvaluateExpressionValue(SpecialAsciiToken: TListPascalString; DebugMode: Boolean; ExpressionText: SystemString): Variant; overload;
function EvaluateExpressionValue(SpecialAsciiToken: TListPascalString; ExpressionText: SystemString): Variant; overload;
function EvaluateExpressionValue(SpecialAsciiToken: TListPascalString; TextStyle: TTextStyle; ExpressionText: SystemString; opRT: TOpCustomRunTime): Variant; overload;

// Evaluate multi Expression as variant Vector
function EvaluateExpressionVector(DebugMode, UsedCache: Boolean; SpecialAsciiToken: TListPascalString; TextStyle: TTextStyle; ExpressionText: SystemString;
  opRT: TOpCustomRunTime; const_vl: THashVariantList): TExpressionValueVector; overload;
function EvaluateExpressionVector(UsedCache: Boolean; SpecialAsciiToken: TListPascalString; TextStyle: TTextStyle; ExpressionText: SystemString;
  opRT: TOpCustomRunTime; const_vl: THashVariantList): TExpressionValueVector; overload;
function EvaluateExpressionVector(SpecialAsciiToken: TListPascalString; TextStyle: TTextStyle; ExpressionText: SystemString;
  opRT: TOpCustomRunTime; const_vl: THashVariantList): TExpressionValueVector; overload;
function EvaluateExpressionVector(ExpressionText: SystemString; opRT: TOpCustomRunTime; const_vl: THashVariantList): TExpressionValueVector; overload;
function EvaluateExpressionVector(ExpressionText: SystemString; const_vl: THashVariantList): TExpressionValueVector; overload;
function EvaluateExpressionVector(ExpressionText: SystemString; TextStyle: TTextStyle): TExpressionValueVector; overload;
function EvaluateExpressionVector(ExpressionText: SystemString): TExpressionValueVector; overload;

// Evaluate multi Expression as variant matrix
function EvaluateExpressionMatrix(W, H: Integer; SpecialAsciiToken: TListPascalString; TextStyle: TTextStyle; ExpressionText: SystemString;
  opRT: TOpCustomRunTime; const_vl: THashVariantList): TExpressionValueMatrix; overload;
function EvaluateExpressionMatrix(W, H: Integer; ExpressionText: SystemString; opRT: TOpCustomRunTime; const_vl: THashVariantList): TExpressionValueMatrix; overload;
function EvaluateExpressionMatrix(W, H: Integer; ExpressionText: SystemString; const_vl: THashVariantList): TExpressionValueMatrix; overload;
function EvaluateExpressionMatrix(W, H: Integer; ExpressionText: SystemString; TextStyle: TTextStyle): TExpressionValueMatrix; overload;
function EvaluateExpressionMatrix(W, H: Integer; ExpressionText: SystemString): TExpressionValueMatrix; overload;

// easy API
function EStr(s: U_String): U_String;
function EStrToInt(s: U_String; default: Integer): Integer;
function EStrToInt64(s: U_String; default: Int64): Int64;
function EStrToFloat(s: U_String; default: Double): Double;
function EStrToSingle(s: U_String; default: Single): Single;
function EStrToDouble(s: U_String; default: Double): Double;

// print
function ExpressionValueVectorToStr(v: TExpressionValueVector): TPascalString;
procedure DoStatus(v: TExpressionValueVector); overload;
procedure DoStatus(v: TExpressionValueMatrix); overload;

// test
procedure EvaluateExpressionVectorAndMatrix_test_;

implementation

var
  OpCache: THashObjectList;

{$REGION 'internal imp'}


type
  TSymbolOperationType = record
    State: TSymbolOperation;
    Decl: SystemString;
  end;

const
  MethodToken: TExpressionDeclTypes = ([edtProcExp]);

  AllExpressionValueType: TExpressionDeclTypes = ([
    edtBool, edtInt, edtInt64, edtUInt64, edtWord, edtByte, edtSmallInt, edtShortInt, edtUInt,
    edtSingle, edtDouble, edtCurrency,
    edtString, edtProcExp,
    edtExpressionAsValue]);

  SymbolOperationPriority: array [0 .. 3] of TSymbolOperations = (
    ([soOr, soAnd, soXor]),
    ([soEqual, soLessThan, soEqualOrLessThan, soGreaterThan, soEqualOrGreaterThan, soNotEqual]),
    ([soAdd, soSub]),
    ([soMul, soDiv, soMod, soIntDiv, soShl, soShr, soPow]));

  AllowPrioritySymbol: TSymbolOperations = ([
    soAdd, soSub, soMul, soDiv, soMod, soIntDiv, soPow, soOr, soAnd, soXor,
    soEqual, soLessThan, soEqualOrLessThan, soGreaterThan, soEqualOrGreaterThan, soNotEqual,
    soShl, soShr,
    soDotSymbol, soCommaSymbol]);

  OpLogicalSymbol: TSymbolOperations = ([
    soAdd, soSub, soMul, soDiv, soMod, soIntDiv, soPow, soOr, soAnd, soXor,
    soEqual, soLessThan, soEqualOrLessThan, soGreaterThan, soEqualOrGreaterThan, soNotEqual,
    soShl, soShr]);

  SymbolOperationTextDecl: array [TSymbolOperation] of TSymbolOperationType = (
    (State: soAdd; Decl: '+'),
    (State: soSub; Decl: '-'),
    (State: soMul; Decl: '*'),
    (State: soDiv; Decl: '/'),
    (State: soMod; Decl: ' mod '),
    (State: soIntDiv; Decl: ' div '),
    (State: soPow; Decl: '^'),
    (State: soOr; Decl: ' or '),
    (State: soAnd; Decl: ' and '),
    (State: soXor; Decl: ' xor '),
    (State: soEqual; Decl: ' = '),
    (State: soLessThan; Decl: ' < '),
    (State: soEqualOrLessThan; Decl: ' <= '),
    (State: soGreaterThan; Decl: ' > '),
    (State: soEqualOrGreaterThan; Decl: ' => '),
    (State: soNotEqual; Decl: ' <> '),
    (State: soShl; Decl: ' shl '),
    (State: soShr; Decl: ' shr '),
    (State: soBlockIndentBegin; Decl: '('),
    (State: soBlockIndentEnd; Decl: ')'),
    (State: soPropIndentBegin; Decl: '['),
    (State: soPropIndentEnd; Decl: ']'),
    (State: soDotSymbol; Decl: '.'),
    (State: soCommaSymbol; Decl: ','),
    (State: soEolSymbol; Decl: ';'),
    (State: soProc; Decl: '|Proc|'),
    (State: soParameter; Decl: ','),
    (State: soUnknow; Decl: '?')
    );

function NumTextType(s: TPascalString): TNumTextType;
type
  TValSym = (vsSymSub, vsSymAdd, vsSymAddSub, vsSymDollar, vsDot, vsDotBeforNum, vsDotAfterNum, vsNum, vsAtoF, vsE, vsUnknow);
var
  cnt: array [TValSym] of Integer;
  v: TValSym;
  c: SystemChar;
  i: Integer;
begin
  if s.Same('true') or s.Same('false') then
      Exit(nttBool);

  for v := low(TValSym) to high(TValSym) do
      cnt[v] := 0;

  for i := 1 to s.Len do
    begin
      c := s[i];
      if CharIn(c, [c0to9]) then
        begin
          inc(cnt[vsNum]);
          if cnt[vsDot] > 0 then
              inc(cnt[vsDotAfterNum]);
        end
      else if CharIn(c, [cLoAtoF, cHiAtoF]) then
        begin
          inc(cnt[vsAtoF]);
          if CharIn(c, 'eE') then
              inc(cnt[vsE]);
        end
      else if c = '.' then
        begin
          inc(cnt[vsDot]);
          cnt[vsDotBeforNum] := cnt[vsNum];
        end
      else if CharIn(c, '-') then
        begin
          inc(cnt[vsSymSub]);
          inc(cnt[vsSymAddSub]);
        end
      else if CharIn(c, '+') then
        begin
          inc(cnt[vsSymAdd]);
          inc(cnt[vsSymAddSub]);
        end
      else if CharIn(c, '$') and (i = 1) then
        begin
          inc(cnt[vsSymDollar]);
          if i <> 1 then
              Exit(nttUnknow);
        end
      else
          Exit(nttUnknow);
    end;

  if cnt[vsDot] > 1 then
      Exit(nttUnknow);
  if cnt[vsSymDollar] > 1 then
      Exit(nttUnknow);
  if (cnt[vsSymDollar] = 0) and (cnt[vsNum] = 0) then
      Exit(nttUnknow);
  if (cnt[vsSymAdd] > 1) and (cnt[vsE] = 0) and (cnt[vsSymDollar] = 0) then
      Exit(nttUnknow);

  if (cnt[vsSymDollar] = 0) and
    ((cnt[vsDot] = 1) or ((cnt[vsE] = 1) and ((cnt[vsSymAddSub] >= 1) and (cnt[vsSymDollar] = 0)))) then
    begin
      if cnt[vsSymDollar] > 0 then
          Exit(nttUnknow);
      if (cnt[vsAtoF] <> cnt[vsE]) then
          Exit(nttUnknow);

      if cnt[vsE] = 1 then
        begin
          Result := nttDouble
        end
      else if ((cnt[vsDotBeforNum] > 0)) and (cnt[vsDotAfterNum] > 0) then
        begin
          if cnt[vsDotAfterNum] < 5 then
              Result := nttCurrency
          else if cnt[vsNum] > 7 then
              Result := nttDouble
          else
              Result := nttSingle;
        end
      else
          Exit(nttUnknow);
    end
  else
    begin
      if cnt[vsSymDollar] = 1 then
        begin
          if cnt[vsSymSub] > 0 then
            begin
              if cnt[vsNum] + cnt[vsAtoF] = 0 then
                  Result := nttUnknow
              else if cnt[vsNum] + cnt[vsAtoF] < 2 then
                  Result := nttShortInt
              else if cnt[vsNum] + cnt[vsAtoF] < 4 then
                  Result := nttSmallInt
              else if cnt[vsNum] + cnt[vsAtoF] < 7 then
                  Result := nttInt
              else if cnt[vsNum] + cnt[vsAtoF] < 13 then
                  Result := nttInt64
              else
                  Result := nttUnknow;
            end
          else
            begin
              if cnt[vsNum] + cnt[vsAtoF] = 0 then
                  Result := nttUnknow
              else if cnt[vsNum] + cnt[vsAtoF] < 3 then
                  Result := nttByte
              else if cnt[vsNum] + cnt[vsAtoF] < 5 then
                  Result := nttWord
              else if cnt[vsNum] + cnt[vsAtoF] < 8 then
                  Result := nttUInt
              else if cnt[vsNum] + cnt[vsAtoF] < 14 then
                  Result := nttUInt64
              else
                  Result := nttUnknow;
            end;
        end
      else if cnt[vsAtoF] > 0 then
          Exit(nttUnknow)
      else if cnt[vsSymSub] > 0 then
        begin
          if cnt[vsNum] = 0 then
              Result := nttUnknow
          else if cnt[vsNum] < 3 then
              Result := nttShortInt
          else if cnt[vsNum] < 5 then
              Result := nttSmallInt
          else if cnt[vsNum] < 8 then
              Result := nttInt
          else if cnt[vsNum] < 15 then
              Result := nttInt64
          else
              Result := nttUnknow;
        end
      else
        begin
          if cnt[vsNum] = 0 then
              Result := nttUnknow
          else if cnt[vsNum] < 3 then
              Result := nttByte
          else if cnt[vsNum] < 5 then
              Result := nttWord
          else if cnt[vsNum] < 8 then
              Result := nttUInt
          else if cnt[vsNum] < 16 then
              Result := nttUInt64
          else
              Result := nttUnknow;
        end;
    end;
end;

procedure InitExp(var v: TExpressionListData);
begin
  v.DeclType := edtUnknow;
  v.charPos := -1;
  v.Symbol := soUnknow;
  v.Value := NULL;
  v.Expression := nil;
  v.ExpressionAutoFree := False;
end;

function dt2op(const v: TExpressionDeclType): TOpValueType;
begin
  case v of
    edtBool: Result := ovtBool;
    edtInt: Result := ovtInt;
    edtInt64: Result := ovtInt64;
    edtUInt64: Result := ovtUInt64;
    edtWord: Result := ovtWord;
    edtByte: Result := ovtByte;
    edtSmallInt: Result := ovtSmallInt;
    edtShortInt: Result := ovtShortInt;
    edtUInt: Result := ovtUInt;
    edtSingle: Result := ovtSingle;
    edtDouble: Result := ovtDouble;
    edtCurrency: Result := ovtCurrency;
    edtString: Result := ovtString;
    edtProcExp: Result := ovtProc;
    else Result := ovtUnknow;
  end;
end;

function VariantToExpressionDeclType(var v: Variant): TExpressionDeclType;
begin
  case VarType(v) of
    varSmallInt: Result := edtSmallInt;
    varInteger: Result := edtInt;
    varSingle: Result := edtSingle;
    varDouble: Result := edtDouble;
    varCurrency: Result := edtCurrency;
    varBoolean: Result := edtBool;
    varShortInt: Result := edtShortInt;
    varByte: Result := edtByte;
    varWord: Result := edtWord;
    varLongWord: Result := edtUInt;
    varInt64: Result := edtInt64;
    varUInt64: Result := edtUInt64;
    else
      begin
        if VarIsStr(v) then
            Result := edtString
        else
            Result := edtUnknow;
      end;
  end;
end;

constructor TSymbolExpression.Create(const TextStyle_: TTextStyle);
begin
  inherited Create;
  FList := TCoreClassList.Create;
  FTextStyle := TextStyle_;
end;

destructor TSymbolExpression.Destroy;
begin
  Clear;
  DisposeObject(FList);
  inherited Destroy;
end;

procedure TSymbolExpression.Clear;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
    begin
      if (PExpressionListData(FList[i])^.ExpressionAutoFree) and (PExpressionListData(FList[i])^.Expression <> nil) then
          DisposeObject(PExpressionListData(FList[i])^.Expression);

      Dispose(PExpressionListData(FList[i]));
    end;

  FList.Clear;
end;

procedure TSymbolExpression.PrintDebug(const detail: Boolean; const prefix: SystemString);
var
  i: Integer;
  p: PExpressionListData;
begin
  DoStatus(prefix + ' decl: ' + Decl());

  if detail then
    begin
      for i := 0 to Count - 1 do
        begin
          p := GetItems(i);

          DoStatus(prefix + ' id:%d exp:%s symbol:%s val:%s', [i,
            GetEnumName(TypeInfo(TExpressionDeclType), Ord(p^.DeclType)),
            GetEnumName(TypeInfo(TSymbolOperation), Ord(p^.Symbol)),
            VarToStr(p^.Value)]);

        end;

      DoStatus('');

      for i := 0 to Count - 1 do
        begin
          p := GetItems(i);
          if p^.Expression <> nil then
            if p^.Expression.Count > 0 then
                p^.Expression.PrintDebug(detail, prefix + ' -> ' + VarToStr(p^.Value));
        end;
    end;
end;

procedure TSymbolExpression.PrintDebug(const detail: Boolean);
begin
  PrintDebug(detail, '');
end;

function TSymbolExpression.Decl(): SystemString;
var
  i, j: Integer;
  p: PExpressionListData;
begin
  Result := '';
  for i := 0 to FList.Count - 1 do
    begin
      p := FList[i];
      case p^.DeclType of
        edtSymbol:
          Result := Result + SymbolOperationTextDecl[p^.Symbol].Decl;
        edtSingle, edtDouble, edtCurrency:
          Result := Result + FloatToStr(p^.Value);
        edtProcExp:
          begin
            Result := Result + VarToStr(p^.Value) + '(';
            for j := 0 to p^.Expression.Count - 1 do
              begin
                if j = 0 then
                    Result := Result + p^.Expression[j]^.Expression.Decl
                else
                    Result := Result + ',' + p^.Expression[j]^.Expression.Decl;
              end;
            Result := Result + ')';
          end;
        edtString:
          begin
            case FTextStyle of
              tsPascal: Result := Result + TTextParsing.TranslateTextToPascalDecl(VarToStr(p^.Value));
              tsC: Result := Result + TTextParsing.TranslateTextToC_Decl(VarToStr(p^.Value));
              else Result := Result + VarToStr(p^.Value);
            end;
          end;
        edtExpressionAsValue:
          begin
            case p^.Symbol of
              soBlockIndentBegin:
                Result := Format('%s%s%s%s',
                  [Result,
                  SymbolOperationTextDecl[soBlockIndentBegin].Decl,
                  p^.Expression.Decl,
                  SymbolOperationTextDecl[soBlockIndentEnd].Decl
                  ]);
              soPropIndentBegin:
                Result := Format('%s%s%s%s',
                  [Result,
                  SymbolOperationTextDecl[soPropIndentBegin].Decl,
                  p^.Expression.Decl,
                  SymbolOperationTextDecl[soPropIndentEnd].Decl
                  ]);
              soParameter:
                begin
                  Result := Format('%s%s%s%s',
                    [Result,
                    SymbolOperationTextDecl[soBlockIndentBegin].Decl,
                    p^.Expression.Decl,
                    SymbolOperationTextDecl[soBlockIndentEnd].Decl
                    ]);
                end;
              else
                Result := Result + ' !error! ';
            end;
          end;
        edtUnknow: Result := Result + ' !error! ';
        else
          Result := Result + VarToStr(p^.Value);
      end;
    end;
end;

function TSymbolExpression.GetCount(t: TExpressionDeclTypes): Integer;
var
  i: Integer;
  p: PExpressionListData;
begin
  Result := 0;
  for i := 0 to FList.Count - 1 do
    begin
      p := FList[i];
      if p^.DeclType in t then
          inc(Result);
    end;
end;

function TSymbolExpression.GetSymbolCount(Operations: TSymbolOperations): Integer;
var
  i: Integer;
  p: PExpressionListData;
begin
  Result := 0;
  for i := 0 to FList.Count - 1 do
    begin
      p := FList[i];
      if p^.DeclType = edtSymbol then
        begin
          if p^.Symbol in Operations then
              inc(Result);
        end;
    end;
end;

function TSymbolExpression.AvailValueCount: Integer;
begin
  Result := GetCount(AllExpressionValueType);
end;

function TSymbolExpression.Count: Integer;
begin
  Result := FList.Count;
end;

function TSymbolExpression.InsertSymbol(const idx: Integer; v: TSymbolOperation; charPos: Integer): PExpressionListData;
var
  p: PExpressionListData;
begin
  new(p);
  InitExp(p^);
  p^.DeclType := edtSymbol;
  p^.charPos := charPos;
  p^.Symbol := v;
  p^.Value := v;
  FList.Insert(idx, p);
  Result := p;
end;

function TSymbolExpression.Insert(const idx: Integer; v: TExpressionListData): PExpressionListData;
var
  p: PExpressionListData;
begin
  new(p);
  p^ := v;
  FList.Insert(idx, p);
  Result := p;
end;

procedure TSymbolExpression.InsertExpression(const idx: Integer; E: TSymbolExpression);
var
  NewList: TCoreClassList;
  i: Integer;
  p: PExpressionListData;
begin
  NewList := TCoreClassList.Create;
  NewList.Capacity := E.FList.Count + FList.Count;

  for i := 0 to idx do
      NewList.Add(FList[i]);

  for i := 0 to E.FList.Count - 1 do
    begin
      new(p);
      p^ := PExpressionListData(E.FList[i])^;
      NewList.Add(p);
    end;

  for i := idx to FList.Count - 1 do
      NewList.Add(FList[i]);

  DisposeObject(FList);
  FList := NewList;
end;

procedure TSymbolExpression.AddExpression(const E: TSymbolExpression);
var
  i: Integer;
begin
  for i := 0 to E.Count - 1 do
      AddCopy(E[i]^);
end;

function TSymbolExpression.AddSymbol(const v: TSymbolOperation; charPos: Integer): PExpressionListData;
var
  p: PExpressionListData;
begin
  new(p);
  InitExp(p^);
  p^.DeclType := edtSymbol;
  p^.charPos := charPos;
  p^.Symbol := v;
  p^.Value := SymbolOperationTextDecl[v].Decl;
  FList.Add(p);
  Result := p;
end;

function TSymbolExpression.AddBool(const v: Boolean; charPos: Integer): PExpressionListData;
var
  p: PExpressionListData;
begin
  new(p);
  InitExp(p^);
  p^.DeclType := edtBool;
  p^.charPos := charPos;
  p^.Value := v;
  FList.Add(p);
  Result := p;
end;

function TSymbolExpression.AddInt(const v: Integer; charPos: Integer): PExpressionListData;
var
  p: PExpressionListData;
begin
  new(p);
  InitExp(p^);
  p^.DeclType := edtInt;
  p^.charPos := charPos;
  p^.Value := v;
  FList.Add(p);
  Result := p;
end;

function TSymbolExpression.AddUInt(const v: Cardinal; charPos: Integer): PExpressionListData;
var
  p: PExpressionListData;
begin
  new(p);
  InitExp(p^);
  p^.DeclType := edtUInt;
  p^.charPos := charPos;
  p^.Value := v;
  FList.Add(p);
  Result := p;
end;

function TSymbolExpression.AddInt64(const v: Int64; charPos: Integer): PExpressionListData;
var
  p: PExpressionListData;
begin
  new(p);
  InitExp(p^);
  p^.DeclType := edtInt64;
  p^.charPos := charPos;
  p^.Value := v;
  FList.Add(p);
  Result := p;
end;

function TSymbolExpression.AddUInt64(const v: UInt64; charPos: Integer): PExpressionListData;
var
  p: PExpressionListData;
begin
  new(p);
  InitExp(p^);
  p^.DeclType := edtUInt64;
  p^.charPos := charPos;
  p^.Value := v;
  FList.Add(p);
  Result := p;
end;

function TSymbolExpression.AddWord(const v: Word; charPos: Integer): PExpressionListData;
var
  p: PExpressionListData;
begin
  new(p);
  InitExp(p^);
  p^.DeclType := edtWord;
  p^.charPos := charPos;
  p^.Value := v;
  FList.Add(p);
  Result := p;
end;

function TSymbolExpression.AddByte(const v: Byte; charPos: Integer): PExpressionListData;
var
  p: PExpressionListData;
begin
  new(p);
  InitExp(p^);
  p^.DeclType := edtByte;
  p^.charPos := charPos;
  p^.Value := v;
  FList.Add(p);
  Result := p;
end;

function TSymbolExpression.AddSmallInt(const v: SmallInt; charPos: Integer): PExpressionListData;
var
  p: PExpressionListData;
begin
  new(p);
  InitExp(p^);
  p^.DeclType := edtSmallInt;
  p^.charPos := charPos;
  p^.Value := v;
  FList.Add(p);
  Result := p;
end;

function TSymbolExpression.AddShortInt(const v: ShortInt; charPos: Integer): PExpressionListData;
var
  p: PExpressionListData;
begin
  new(p);
  InitExp(p^);
  p^.DeclType := edtShortInt;
  p^.charPos := charPos;
  p^.Value := v;
  FList.Add(p);
  Result := p;
end;

function TSymbolExpression.AddSingle(const v: Single; charPos: Integer): PExpressionListData;
var
  p: PExpressionListData;
begin
  new(p);
  InitExp(p^);
  p^.DeclType := edtSingle;
  p^.charPos := charPos;
  p^.Value := v;
  FList.Add(p);
  Result := p;
end;

function TSymbolExpression.AddDouble(const v: Double; charPos: Integer): PExpressionListData;
var
  p: PExpressionListData;
begin
  new(p);
  InitExp(p^);
  p^.DeclType := edtDouble;
  p^.charPos := charPos;
  p^.Value := v;
  FList.Add(p);
  Result := p;
end;

function TSymbolExpression.AddCurrency(const v: Currency; charPos: Integer): PExpressionListData;
var
  p: PExpressionListData;
begin
  new(p);
  InitExp(p^);
  p^.DeclType := edtCurrency;
  p^.charPos := charPos;
  p^.Value := v;
  FList.Add(p);
  Result := p;
end;

function TSymbolExpression.AddString(const v: SystemString; charPos: Integer): PExpressionListData;
var
  p: PExpressionListData;
begin
  new(p);
  InitExp(p^);
  p^.DeclType := edtString;
  p^.charPos := charPos;
  p^.Value := v;
  FList.Add(p);
  Result := p;
end;

function TSymbolExpression.AddFunc(const v: SystemString; charPos: Integer): PExpressionListData;
var
  p: PExpressionListData;
begin
  new(p);
  InitExp(p^);
  p^.DeclType := edtProcExp;
  p^.charPos := charPos;
  p^.Symbol := soProc;
  p^.Value := v;
  p^.Expression := TSymbolExpression.Create(FTextStyle);
  p^.ExpressionAutoFree := True;
  FList.Add(p);
  Result := p;
end;

function TSymbolExpression.AddExpressionAsValue(AutoFree: Boolean; Expression: TSymbolExpression; Symbol: TSymbolOperation; Value: Variant; charPos: Integer): PExpressionListData;
var
  p: PExpressionListData;
begin
  new(p);
  InitExp(p^);
  p^.DeclType := edtExpressionAsValue;
  p^.charPos := charPos;
  p^.Symbol := Symbol;
  p^.Value := Value;
  p^.Expression := Expression;
  p^.ExpressionAutoFree := AutoFree;
  FList.Add(p);
  Result := p;
end;

function TSymbolExpression.Add(const v: TExpressionListData): PExpressionListData;
var
  p: PExpressionListData;
begin
  new(p);
  p^ := v;
  p^.ExpressionAutoFree := False;
  FList.Add(p);
  Result := p;
end;

function TSymbolExpression.AddCopy(const v: TExpressionListData): PExpressionListData;
var
  p: PExpressionListData;
  i: Integer;
begin
  new(p);
  p^ := v;
  p^.ExpressionAutoFree := False;
  if v.Expression <> nil then
    begin
      p^.Expression := TSymbolExpression.Create(FTextStyle);
      p^.ExpressionAutoFree := True;
      for i := 0 to v.Expression.Count - 1 do
          p^.Expression.AddCopy(v.Expression[i]^)
    end;
  FList.Add(p);
  Result := p;
end;

procedure TSymbolExpression.Delete(const idx: Integer);
var
  p: PExpressionListData;
begin
  p := FList[idx];
  if (p^.ExpressionAutoFree) and (p^.Expression <> nil) then
      DisposeObject(p^.Expression);
  Dispose(p);
  FList.Delete(idx);
end;

procedure TSymbolExpression.DeleteLast;
begin
  Delete(Count - 1);
end;

function TSymbolExpression.Last: PExpressionListData;
begin
  Result := FList.Last;
end;

function TSymbolExpression.First: PExpressionListData;
begin
  Result := FList.First;
end;

function TSymbolExpression.IndexOf(p: PExpressionListData): Integer;
var
  i: Integer;
begin
  for i := FList.Count - 1 downto 0 do
    if FList[i] = p then
        Exit(i);
  Exit(-1);
end;

function TSymbolExpression.GetItems(index: Integer): PExpressionListData;
begin
  Result := FList[index];
end;

function ParseOperationState(ParsingEng: TTextParsing;
  var cPos, bPos, ePos, BlockIndent, PropIndent: Integer; var pStates: TExpressionParsingState): TSymbolOperation;

var
  c: SystemChar;
  Decl: TPascalString;
  p: PExpressionListData;
begin
  Result := soUnknow;
  if not(esWaitOp in pStates) then
      Exit;

  while cPos <= ParsingEng.Len do
    begin
      if ParsingEng.isComment(cPos) then
        begin
          cPos := ParsingEng.GetCommentEndPos(cPos);
          Continue;
        end;

      c := ParsingEng.ParsingData.Text[cPos];
      bPos := cPos;

      if (CharIn(c, ';')) then
        begin
          inc(cPos);
          Result := soEolSymbol;
          Exit;
        end;

      if (CharIn(c, ',')) then
        begin
          inc(cPos);
          pStates := pStates - [esWaitOp] + [esWaitValue];
          Result := soCommaSymbol;
          Exit;
        end;

      if CharIn(c, ')') then
        begin
          inc(cPos);
          if (esWaitIndentEnd in pStates) then
            begin
              dec(BlockIndent);
              if BlockIndent < 0 then
                begin
                  pStates := pStates - [esWaitOp, esWaitIndentEnd];
                  Result := soBlockIndentEnd;
                  Exit;
                end
              else if BlockIndent = 0 then
                  pStates := pStates - [esWaitIndentEnd];

              pStates := pStates + [esWaitOp];
              Result := soBlockIndentEnd;
              Exit;
            end
          else
            begin
              pStates := pStates - [esWaitOp, esWaitIndentEnd];
              Result := soBlockIndentEnd;
              Exit;
            end;
        end;

      if CharIn(c, ']') then
        begin
          inc(cPos);
          if (esWaitPropParamIndentEnd in pStates) then
            begin
              dec(PropIndent);
              if PropIndent < 0 then
                begin
                  pStates := pStates - [esWaitOp, esWaitPropParamIndentEnd];
                  Result := soPropIndentEnd;
                  Exit;
                end
              else if PropIndent = 0 then
                  pStates := pStates - [esWaitPropParamIndentEnd];

              pStates := pStates + [esWaitOp];
              Result := soPropIndentEnd;
              Exit;
            end
          else
            begin
              pStates := pStates - [esWaitOp, esWaitPropParamIndentEnd];
              Result := soPropIndentEnd;
              Exit;
            end;
        end;

      if CharIn(c, '(') then
        begin
          inc(cPos);
          inc(BlockIndent);
          pStates := pStates - [esWaitOp];
          pStates := pStates + [esWaitValue, esWaitIndentEnd];

          Result := soBlockIndentBegin;
          Exit;
        end;

      if CharIn(c, '[') then
        begin
          inc(cPos);
          inc(PropIndent);
          Result := soPropIndentBegin;
          pStates := pStates - [esWaitOp];
          pStates := pStates + [esWaitValue, esWaitPropParamIndentEnd];
          Exit;
        end;

      if (ParsingEng.ComparePosStr(cPos, '>=')) or (ParsingEng.ComparePosStr(cPos, '=>')) then
        begin
          inc(cPos, 2);
          Result := soEqualOrGreaterThan;
          pStates := pStates - [esWaitOp];
          pStates := pStates + [esWaitValue];
          Exit;
        end;
      if (ParsingEng.ComparePosStr(cPos, '<=')) or (ParsingEng.ComparePosStr(cPos, '=<')) then
        begin
          inc(cPos, 2);
          Result := soEqualOrLessThan;
          pStates := pStates - [esWaitOp];
          pStates := pStates + [esWaitValue];
          Exit;
        end;
      if (ParsingEng.ComparePosStr(cPos, '<>')) or (ParsingEng.ComparePosStr(cPos, '><')) or (ParsingEng.ComparePosStr(cPos, '!=')) then
        begin
          inc(cPos, 2);
          Result := soNotEqual;
          pStates := pStates - [esWaitOp];
          pStates := pStates + [esWaitValue];
          Exit;
        end;
      if (ParsingEng.ComparePosStr(cPos, '==')) then
        begin
          inc(cPos, 2);
          Result := soEqual;
          pStates := pStates - [esWaitOp];
          pStates := pStates + [esWaitValue];
          Exit;
        end;
      if (ParsingEng.ComparePosStr(cPos, '&&')) then
        begin
          inc(cPos, 2);
          Result := soAnd;
          pStates := pStates - [esWaitOp];
          pStates := pStates + [esWaitValue];
          Exit;
        end;
      if (ParsingEng.ComparePosStr(cPos, '||')) then
        begin
          inc(cPos, 2);
          Result := soOr;
          pStates := pStates - [esWaitOp];
          pStates := pStates + [esWaitValue];
          Exit;
        end;
      if (ParsingEng.ComparePosStr(cPos, '<<')) then
        begin
          inc(cPos, 2);
          Result := soShl;
          pStates := pStates - [esWaitOp];
          pStates := pStates + [esWaitValue];
          Exit;
        end;
      if (ParsingEng.ComparePosStr(cPos, '>>')) then
        begin
          inc(cPos, 2);
          Result := soShr;
          pStates := pStates - [esWaitOp];
          pStates := pStates + [esWaitValue];
          Exit;
        end;

      if CharIn(c, '+-*/^=><.,&|%') then
        begin
          if c = '+' then
              Result := soAdd
          else if c = '-' then
              Result := soSub
          else if c = '*' then
              Result := soMul
          else if c = '/' then
              Result := soDiv
          else if c = '^' then
              Result := soPow
          else if c = '=' then
              Result := soEqual
          else if c = '>' then
              Result := soGreaterThan
          else if c = '<' then
              Result := soLessThan
          else if c = '.' then
              Result := soDotSymbol
          else if c = ',' then
              Result := soCommaSymbol
          else if c = '&' then
              Result := soAnd
          else if c = '|' then
              Result := soOr
          else if c = '%' then
              Result := soMod;
          inc(cPos);
          pStates := pStates - [esWaitOp];
          pStates := pStates + [esWaitValue];
          Exit;
        end;

      if (ParsingEng.isAscii(cPos)) then
        begin
          bPos := cPos;
          ePos := ParsingEng.GetAsciiEndPos(cPos);
          Decl := ParsingEng.GetStr(bPos, ePos);

          if Decl.Same('or') then
              Result := soOr
          else if Decl.Same('and') then
              Result := soAnd
          else if Decl.Same('xor') then
              Result := soXor
          else if Decl.Same('div', 'idiv', 'intdiv') then
              Result := soIntDiv
          else if Decl.Same('fdiv', 'floatdiv') then
              Result := soDiv
          else if Decl.Same('mod') then
              Result := soMod
          else if Decl.Same('shl') then
              Result := soShl
          else if Decl.Same('shr') then
              Result := soShr
          else
            begin
              Result := soUnknow;
              Exit;
            end;

          cPos := ePos;
          pStates := pStates - [esWaitOp];
          pStates := pStates + [esWaitValue];
          Exit;
        end;

      if ParsingEng.isNumber(cPos) then
        begin
          Result := soUnknow;
          Exit;
        end;

      inc(cPos);
    end;
  pStates := [];
  Result := soEolSymbol;
end;

function ParseSymbol(ParsingEng: TTextParsing; WorkSym: TSymbolExpression;
  var cPos, bPos, ePos, BlockIndent, PropIndent: Integer; pStates: PExpressionParsingState): Boolean;
var
  bak_cPos: Integer;
  Decl: SystemString;
  OpState: TSymbolOperation;
  RV: Variant;
  robj: TCoreClassObject;
  p: PExpressionListData;
begin
  while cPos <= ParsingEng.Len do
    begin
      pStates^ := pStates^ - [esWaitValue, esFirst];
      pStates^ := pStates^ + [esWaitOp];

      bak_cPos := cPos;
      OpState := ParseOperationState(ParsingEng, cPos, bPos, ePos, BlockIndent, PropIndent, pStates^);

      case OpState of
        soUnknow, soEolSymbol:
          begin
            Result := False;
            Exit;
          end;
        soDotSymbol:
          begin
            Result := False;
            Exit;
          end;
        soCommaSymbol:
          begin
            WorkSym.AddSymbol(OpState, bak_cPos);
            Result := True;
            Exit;
          end;
        soPropIndentBegin:
          begin
            WorkSym.AddSymbol(OpState, bak_cPos);
            Result := True;
            Exit;
          end;
        soPropIndentEnd:
          begin
            WorkSym.AddSymbol(OpState, bak_cPos);
            Result := True;
            Exit;
          end;
        soBlockIndentEnd:
          begin
            WorkSym.AddSymbol(OpState, bak_cPos);
            Result := True;
            Exit;
          end;
        soBlockIndentBegin:
          begin
            WorkSym.AddSymbol(OpState, bak_cPos);
            Result := True;
            Exit;
          end;
        else
          begin
            WorkSym.AddSymbol(OpState, bak_cPos);
            Result := True;
            Exit;
          end;
      end;
    end;
  Result := False;
end;

function __ParseTextExpressionAsSymbol(ParsingEng: TTextParsing; const uName: SystemString;
  const OnDeclValueCall: TOnDeclValueCall; const OnDeclValueMethod: TOnDeclValueMethod;
  const OnDeclValueProc: TOnDeclValueProc;
  RefrenceOpRT: TOpCustomRunTime): TSymbolExpression;

  procedure PrintError(const s: SystemString);
  begin
    if s = '' then
        DoStatus('declaration error "%s"', [ParsingEng.Text.Text])
    else
        DoStatus('declaration error "%s" -> [%s]', [ParsingEng.Text.Text, s]);
    DoStatus('');
  end;

  function GetDeclValue(const Decl: SystemString; var v: Variant): TExpressionDeclType;
  begin
    v := Decl;
    Result := edtProcExp;

    if Assigned(OnDeclValueCall) then
        OnDeclValueCall(Decl, Result, v);
    if Assigned(OnDeclValueMethod) then
        OnDeclValueMethod(Decl, Result, v);
    if Assigned(OnDeclValueProc) then
        OnDeclValueProc(Decl, Result, v);
  end;

  function FillProc(var ExpIndex: Integer; const Exps, procExp: TSymbolExpression): TSymbolExpression;
  var
    WasProc: Boolean;
    LocalExp, ResExp: TSymbolExpression;
    p1, p2, p: PExpressionListData;
  begin
    if ExpIndex >= Exps.Count then
      begin
        Result := nil;
        Exit;
      end;

    WasProc := procExp <> nil;

    if WasProc then
        LocalExp := procExp.AddExpressionAsValue(
        True, TSymbolExpression.Create(ParsingEng.TextStyle), soParameter, 'param_1', Exps[ExpIndex]^.charPos)^.Expression
    else
        LocalExp := TSymbolExpression.Create(ParsingEng.TextStyle);

    Result := LocalExp;

    while ExpIndex < Exps.Count do
      begin
        p1 := Exps[ExpIndex];

        if ExpIndex + 1 < Exps.Count then
            p2 := Exps[ExpIndex + 1]
        else
            p2 := nil;

        if (p1^.DeclType = edtProcExp) then
          begin
            if p2 <> nil then
              begin
                if (p2^.DeclType = edtSymbol) and (p2^.Symbol in [soBlockIndentBegin, soPropIndentBegin]) then
                  begin
                    inc(ExpIndex, 2);
                    p := LocalExp.AddFunc(p1^.Value, p1^.charPos);
                    FillProc(ExpIndex, Exps, p^.Expression);
                    Continue;
                  end;
              end
            else
              begin
                Result.AddFunc(p1^.Value, p1^.charPos);
                inc(ExpIndex);
                Continue;
              end;
          end;

        if (p1^.DeclType = edtSymbol) then
          begin
            if p1^.Symbol in [soBlockIndentBegin, soPropIndentBegin] then
              begin
                inc(ExpIndex);
                ResExp := FillProc(ExpIndex, Exps, nil);
                if ResExp <> nil then
                    LocalExp.AddExpressionAsValue(True, ResExp, soBlockIndentBegin, p1^.Symbol, p1^.charPos);
                Continue;
              end;
            if p1^.Symbol in [soBlockIndentEnd, soPropIndentEnd] then
              begin
                inc(ExpIndex);
                Exit;
              end;
            if (p1^.Symbol in [soCommaSymbol]) then
              begin
                if not WasProc then
                  begin
                    PrintError('comma Illegal');
                    Exit;
                  end;

                LocalExp := procExp.AddExpressionAsValue(True,
                  TSymbolExpression.Create(ParsingEng.TextStyle), soParameter, 'param_' + IntToStr(procExp.Count + 1),
                  Exps[ExpIndex]^.charPos)^.Expression;
                inc(ExpIndex);
                Continue;
              end;
          end;

        LocalExp.AddCopy(p1^);
        inc(ExpIndex);
      end;
  end;

var
  cPos, bPos, ePos, i: Integer;
  td: PTokenData;
  State: TExpressionParsingState;
  BlockIndent, PropIndent: Integer;
  Container: TSymbolExpression;
  te: TTextParsing;
  Decl: TPascalString;
  OpState: TSymbolOperation;
  isNumber, isSpecialSymbol, isAscii, isTextDecl, isSymbol: Boolean;
  RV: Variant;
  p: PExpressionListData;
begin
  Result := nil;

  if ParsingEng.ParsingData.Len < 1 then
      Exit;

  cPos := 1;
  BlockIndent := 0;
  PropIndent := 0;
  State := [esFirst];
  Container := TSymbolExpression.Create(ParsingEng.TextStyle);

  while cPos <= ParsingEng.Len do
    begin
      if ParsingEng.isComment(cPos) then
        begin
          cPos := ParsingEng.GetCommentEndPos(cPos) + 1;
          Continue;
        end;

      // check esWaitOp state
      if (esWaitOp in State) and (CharIn(ParsingEng.GetChar(cPos), ParsingEng.SymbolTable)) then
        begin
          isNumber := False;
          isTextDecl := False;
          isAscii := False;
          isSymbol := True;
          bPos := cPos;
          ePos := bPos + 1;
        end
      else
        begin
          td := ParsingEng.TokenPos[cPos];
          isSpecialSymbol := td^.tokenType = ttSpecialSymbol;
          if isSpecialSymbol then
            begin
              isNumber := False;
              isTextDecl := False;
              isAscii := False;
              isSymbol := False;
            end
          else if (td^.tokenType = ttAscii) and
            (
            td^.Text.Same('and', 'or', 'xor', 'shl', 'shr')
            or
            td^.Text.Same('div', 'idiv', 'intdiv', 'fdiv', 'floatdiv')
            or
            td^.Text.Same('mod')
            ) then
            begin
              isSymbol := True;
              isNumber := False;
              isTextDecl := False;
              isAscii := False;
            end
          else
            begin
              isNumber := td^.tokenType = ttNumber;
              isTextDecl := td^.tokenType = ttTextDecl;
              isAscii := td^.tokenType = ttAscii;
              isSymbol := td^.tokenType = ttSymbol;
            end;
        end;

      if (not(esWaitOp in State)) and (isSpecialSymbol or isNumber or isTextDecl or isAscii) then
        begin
          if not((esWaitValue in State) or (esFirst in State)) then
            begin
              PrintError('');
              Break;
            end;

          bPos := cPos;
          ePos := td^.ePos;
          if (isSpecialSymbol) and (ParsingEng.GetAsciiBeginPos(ePos) <= ePos) then
              ePos := ParsingEng.GetSpecialSymbolEndPos(ParsingEng.GetAsciiEndPos(ePos));
          cPos := ePos;

          Decl := ParsingEng.GetStr(bPos, ePos);
          if isNumber then
            begin
              if Decl.ComparePos(1, '0x') then
                begin
                  Decl.DeleteFirst;
                  Decl[1] := '$';
                end;
              case NumTextType(Decl) of
                nttBool: Container.AddBool(StrToBool(Decl), bPos);
                nttInt: Container.AddInt(StrToInt(Decl), bPos);
                nttInt64: Container.AddInt64(StrToInt64(Decl), bPos);
{$IFDEF FPC}
                nttUInt64: Container.AddUInt64(StrToQWord(Decl), bPos);
{$ELSE}
                nttUInt64: Container.AddUInt64(StrToUInt64(Decl), bPos);
{$ENDIF}
                nttWord: Container.AddWord(StrToInt(Decl), bPos);
                nttByte: Container.AddByte(StrToInt(Decl), bPos);
                nttSmallInt: Container.AddSmallInt(StrToInt(Decl), bPos);
                nttShortInt: Container.AddShortInt(StrToInt(Decl), bPos);
                nttUInt: Container.AddUInt(StrToInt(Decl), bPos);
                nttSingle: Container.AddSingle(StrToFloat(Decl), bPos);
                nttDouble: Container.AddDouble(StrToFloat(Decl), bPos);
                nttCurrency: Container.AddCurrency(StrToFloat(Decl), bPos);
                else
                  begin
                    PrintError(Format('number expression "%s" Illegal', [Decl.Text]));
                    Break;
                  end;
              end;
            end
          else if isTextDecl then
            begin
              Container.AddString(ParsingEng.GetTextBody(Decl), bPos);
            end
          else
            case NumTextType(Decl) of
              nttBool: Container.AddBool(StrToBool(Decl), bPos);
              nttInt: Container.AddInt(StrToInt(Decl), bPos);
              nttInt64: Container.AddInt64(StrToInt64(Decl), bPos);
{$IFDEF FPC}
              nttUInt64: Container.AddUInt64(StrToQWord(Decl), bPos);
{$ELSE}
              nttUInt64: Container.AddUInt64(StrToUInt64(Decl), bPos);
{$ENDIF}
              nttWord: Container.AddWord(StrToInt(Decl), bPos);
              nttByte: Container.AddByte(StrToInt(Decl), bPos);
              nttSmallInt: Container.AddSmallInt(StrToInt(Decl), bPos);
              nttShortInt: Container.AddShortInt(StrToInt(Decl), bPos);
              nttUInt: Container.AddUInt(StrToInt(Decl), bPos);
              nttSingle: Container.AddSingle(StrToFloat(Decl), bPos);
              nttDouble: Container.AddDouble(StrToFloat(Decl), bPos);
              nttCurrency: Container.AddCurrency(StrToFloat(Decl), bPos);
              else
                begin
                  case GetDeclValue(Decl, RV) of
                    edtBool: Container.AddBool(RV, bPos);
                    edtInt: Container.AddInt(RV, bPos);
                    edtInt64: Container.AddInt64(RV, bPos);
                    edtUInt64: Container.AddUInt64(RV, bPos);
                    edtWord: Container.AddWord(RV, bPos);
                    edtByte: Container.AddByte(RV, bPos);
                    edtSmallInt: Container.AddSmallInt(RV, bPos);
                    edtShortInt: Container.AddShortInt(RV, bPos);
                    edtUInt: Container.AddUInt(RV, bPos);
                    edtSingle: Container.AddSingle(RV, bPos);
                    edtDouble: Container.AddDouble(RV, bPos);
                    edtCurrency: Container.AddCurrency(RV, bPos);
                    edtString: Container.AddString(RV, bPos);
                    edtProcExp:
                      begin
                        if (RefrenceOpRT <> nil) and (not RefrenceOpRT.ProcList.Exists(RV)) then
                          if (DefaultOpRT <> RefrenceOpRT) and (not DefaultOpRT.ProcList.Exists(RV)) then
                            begin
                              PrintError(Format('function "%s" Illegal', [RV]));
                              Break;
                            end;
                        Container.AddFunc(RV, bPos);
                      end;
                    else
                      begin
                        PrintError(Format('define "%s" Illegal', [Decl.Text]));
                        Break;
                      end;
                  end;
                end;
            end;
          if not ParseSymbol(ParsingEng, Container, cPos, bPos, ePos, BlockIndent, PropIndent, @State) then
              Break
          else
              Continue;
        end;

      if (isSymbol) then
        begin
          if not ParseSymbol(ParsingEng, Container, cPos, bPos, ePos, BlockIndent, PropIndent, @State) then
              Break
          else
              Continue;
        end;

      inc(cPos);
    end;

  if (BlockIndent + PropIndent = 0) then
    begin
      i := 0;
      Result := FillProc(i, Container, nil);
      if Result = nil then
          PrintError('indent error');
    end
  else
      PrintError('indent error');

  DisposeObject(Container);
end;

function ParseTextExpressionAsSymbol_C(ParsingEng: TTextParsing; const uName: SystemString;
  const OnGetValue: TOnDeclValueCall; RefrenceOpRT: TOpCustomRunTime): TSymbolExpression;
begin
  Result := __ParseTextExpressionAsSymbol(ParsingEng, uName, OnGetValue, nil, nil, RefrenceOpRT);
end;

function ParseTextExpressionAsSymbol_M(ParsingEng: TTextParsing; const uName: SystemString;
  const OnGetValue: TOnDeclValueMethod; RefrenceOpRT: TOpCustomRunTime): TSymbolExpression;
begin
  Result := __ParseTextExpressionAsSymbol(ParsingEng, uName, nil, OnGetValue, nil, RefrenceOpRT);
end;

function ParseTextExpressionAsSymbol_P(ParsingEng: TTextParsing; const uName: SystemString;
  const OnGetValue: TOnDeclValueProc; RefrenceOpRT: TOpCustomRunTime): TSymbolExpression;
begin
  Result := __ParseTextExpressionAsSymbol(ParsingEng, uName, nil, nil, OnGetValue, RefrenceOpRT);
end;

function ParseTextExpressionAsSymbol(SpecialAsciiToken: TListPascalString;
  TextStyle: TTextStyle; const uName, ExpressionText: SystemString;
  const OnGetValue: TOnDeclValueMethod; RefrenceOpRT: TOpCustomRunTime): TSymbolExpression;
var
  ParsingEng: TTextParsing;
begin
  ParsingEng := TTextParsing.Create(ExpressionText, TextStyle, SpecialAsciiToken);
  Result := ParseTextExpressionAsSymbol_M(ParsingEng, uName, OnGetValue, RefrenceOpRT);
  DisposeObject(ParsingEng);
end;

function ParseTextExpressionAsSymbol(TextStyle: TTextStyle; const uName, ExpressionText: SystemString;
  const OnGetValue: TOnDeclValueMethod; RefrenceOpRT: TOpCustomRunTime): TSymbolExpression;
begin
  Result := ParseTextExpressionAsSymbol(nil, TextStyle, uName, ExpressionText, OnGetValue, RefrenceOpRT);
end;

function ParseTextExpressionAsSymbol(SpecialAsciiToken: TListPascalString;
  ExpressionText: SystemString; RefrenceOpRT: TOpCustomRunTime): TSymbolExpression;
var
  ParsingEng: TTextParsing;
begin
  ParsingEng := TTextParsing.Create(ExpressionText, tsPascal, SpecialAsciiToken);
  Result := ParseTextExpressionAsSymbol_M(ParsingEng, '', nil, RefrenceOpRT);
  DisposeObject(ParsingEng);
end;

function ParseTextExpressionAsSymbol(ExpressionText: SystemString; RefrenceOpRT: TOpCustomRunTime): TSymbolExpression;
begin
  Result := ParseTextExpressionAsSymbol(nil, ExpressionText, RefrenceOpRT);
end;

function ParseTextExpressionAsSymbol(SpecialAsciiToken: TListPascalString; ExpressionText: SystemString): TSymbolExpression;
var
  ParsingEng: TTextParsing;
begin
  ParsingEng := TTextParsing.Create(ExpressionText, tsPascal, SpecialAsciiToken);
  Result := ParseTextExpressionAsSymbol_M(ParsingEng, '', nil, DefaultOpRT);
  DisposeObject(ParsingEng);
end;

function ParseTextExpressionAsSymbol(ExpressionText: SystemString): TSymbolExpression;
begin
  Result := ParseTextExpressionAsSymbol(nil, ExpressionText);
end;

function ParseTextExpressionAsSymbol_M(SpecialAsciiToken: TListPascalString;
  TextEngClass: TTextParsingClass; TextStyle: TTextStyle; const uName, ExpressionText: SystemString;
  const OnGetValue: TOnDeclValueMethod; RefrenceOpRT: TOpCustomRunTime): TSymbolExpression;
var
  ParsingEng: TTextParsing;
begin
  ParsingEng := TextEngClass.Create(ExpressionText, TextStyle, SpecialAsciiToken);
  Result := ParseTextExpressionAsSymbol_M(ParsingEng, '', OnGetValue, RefrenceOpRT);
  DisposeObject(ParsingEng);
end;

function ParseTextExpressionAsSymbol_M(TextEngClass: TTextParsingClass; TextStyle: TTextStyle; const uName, ExpressionText: SystemString;
  const OnGetValue: TOnDeclValueMethod; RefrenceOpRT: TOpCustomRunTime): TSymbolExpression;
begin
  Result := ParseTextExpressionAsSymbol_M(nil, TextEngClass, TextStyle, uName, ExpressionText, OnGetValue, RefrenceOpRT);
end;

function ParseTextExpressionAsSymbol_C(SpecialAsciiToken: TListPascalString;
  TextEngClass: TTextParsingClass; TextStyle: TTextStyle; const uName, ExpressionText: SystemString;
  const OnGetValue: TOnDeclValueCall; RefrenceOpRT: TOpCustomRunTime): TSymbolExpression;
var
  ParsingEng: TTextParsing;
begin
  ParsingEng := TextEngClass.Create(ExpressionText, TextStyle, SpecialAsciiToken);
  Result := ParseTextExpressionAsSymbol_C(ParsingEng, '', OnGetValue, RefrenceOpRT);
  DisposeObject(ParsingEng);
end;

function ParseTextExpressionAsSymbol_C(TextEngClass: TTextParsingClass; TextStyle: TTextStyle; const uName, ExpressionText: SystemString;
  const OnGetValue: TOnDeclValueCall; RefrenceOpRT: TOpCustomRunTime): TSymbolExpression;
begin
  Result := ParseTextExpressionAsSymbol_C(nil, TextEngClass, TextStyle, uName, ExpressionText, OnGetValue, RefrenceOpRT);
end;

function ParseTextExpressionAsSymbol_P(SpecialAsciiToken: TListPascalString;
  TextEngClass: TTextParsingClass; TextStyle: TTextStyle; const uName, ExpressionText: SystemString;
  const OnGetValue: TOnDeclValueProc; RefrenceOpRT: TOpCustomRunTime): TSymbolExpression;
var
  ParsingEng: TTextParsing;
begin
  ParsingEng := TextEngClass.Create(ExpressionText, TextStyle, SpecialAsciiToken);
  Result := ParseTextExpressionAsSymbol_P(ParsingEng, '', OnGetValue, RefrenceOpRT);
  DisposeObject(ParsingEng);
end;

function ParseTextExpressionAsSymbol_P(TextEngClass: TTextParsingClass; TextStyle: TTextStyle; const uName, ExpressionText: SystemString;
  const OnGetValue: TOnDeclValueProc; RefrenceOpRT: TOpCustomRunTime): TSymbolExpression;
begin
  Result := ParseTextExpressionAsSymbol_P(nil, TextEngClass, TextStyle, uName, ExpressionText, OnGetValue, RefrenceOpRT);
end;

function RebuildLogicalPrioritySymbol(Exps: TSymbolExpression): TSymbolExpression;
  function SymbolPriority(s1, s2: TSymbolOperation): Integer;

    function FindSymbol(s: TSymbolOperation): Integer;
    var
      i: Integer;
    begin
      for i := low(SymbolOperationPriority) to high(SymbolOperationPriority) do
        if s in SymbolOperationPriority[i] then
            Exit(i);
      raise Exception.Create('no define symbol');
    end;

  begin
    if (s1 in [soUnknow, soCommaSymbol]) or (s2 in [soUnknow, soCommaSymbol]) then
        Exit(0);
    Result := FindSymbol(s2) - FindSymbol(s1);
  end;

var
  SymbolIndex: Integer;
  newExpression: TSymbolExpression;
  ParseAborted: Boolean;

  procedure PrintError(const s: SystemString);
  begin
    ParseAborted := True;
    if s <> '' then
        DoStatus(Format('Priority symbol failed : %s', [s]))
    else
        DoStatus('Priority symbol failed');
  end;

  procedure ProcessSymbol(OwnerSym: TSymbolOperation);
  var
    p1, p2, startIndent, lastIndent: PExpressionListData;
    LastSym, lastIndentSym: TSymbolOperation;
    LastSymbolPriority, LastOwnerSymbolPriority: Integer;
  begin
    if ParseAborted then
        Exit;
    if SymbolIndex >= Exps.Count then
        Exit;

    if newExpression.Count > 0 then
        startIndent := newExpression.Last
    else
        startIndent := nil;

    LastSym := OwnerSym;
    lastIndent := nil;
    lastIndentSym := OwnerSym;

    while True do
      begin
        if ParseAborted then
            Break;

        if SymbolIndex >= Exps.Count then
            Break;

        p1 := Exps[SymbolIndex];

        if (p1^.DeclType in AllExpressionValueType) then
          begin
            inc(SymbolIndex);
            if SymbolIndex >= Exps.Count then
              begin
                newExpression.Add(p1^);
                Break;
              end;

            p2 := Exps[SymbolIndex];

            if (p1^.DeclType in MethodToken) and (p2^.DeclType = edtExpressionAsValue) then
              begin
                newExpression.Add(p1^);
                newExpression.Add(p2^);
              end
            else if p2^.DeclType = edtSymbol then
              begin
                if p2^.Symbol in AllowPrioritySymbol then
                  begin
                    LastOwnerSymbolPriority := SymbolPriority(p2^.Symbol, OwnerSym);
                    LastSymbolPriority := SymbolPriority(p2^.Symbol, LastSym);

                    if LastOwnerSymbolPriority > 0 then
                      begin
                        newExpression.Add(p1^);
                        Break;
                      end;

                    if LastSymbolPriority < 0 then
                      begin
                        lastIndent := newExpression.AddSymbol(soBlockIndentBegin, p1^.charPos);
                        lastIndentSym := LastSym;
                        newExpression.Add(p1^);
                        newExpression.Add(p2^);

                        inc(SymbolIndex);
                        ProcessSymbol(p2^.Symbol);
                        newExpression.AddSymbol(soBlockIndentEnd, p2^.charPos);

                        Continue;
                      end
                    else if LastSymbolPriority > 0 then
                      begin
                        if startIndent = nil then
                            startIndent := newExpression.First;

                        newExpression.InsertSymbol(newExpression.IndexOf(startIndent), soBlockIndentBegin, startIndent^.charPos);
                        newExpression.Add(p1^);
                        newExpression.AddSymbol(soBlockIndentEnd, p2^.charPos);
                        newExpression.Add(p2^);
                      end
                    else
                      begin
                        newExpression.Add(p1^);
                        newExpression.Add(p2^);
                      end;
                    LastSym := p2^.Symbol;
                  end
                else
                  begin
                    PrintError(SymbolOperationTextDecl[p2^.Symbol].Decl);
                    Exit;
                  end;
              end;
          end
        else if (p1^.DeclType = edtSymbol) then
          begin
            inc(SymbolIndex);
            if SymbolIndex >= Exps.Count then
              begin
                newExpression.Add(p1^);
                Break;
              end;

            p2 := Exps[SymbolIndex];

            if (p2^.DeclType in AllExpressionValueType) then
              begin
                if p1^.Symbol in AllowPrioritySymbol then
                  begin
                    LastSymbolPriority := SymbolPriority(p1^.Symbol, lastIndentSym);

                    if LastSymbolPriority < 0 then
                      begin
                        newExpression.InsertSymbol(newExpression.IndexOf(lastIndent), soBlockIndentBegin, lastIndent^.charPos);
                        newExpression.Add(p1^);
                        LastSym := p1^.Symbol;
                        ProcessSymbol(p1^.Symbol);
                        newExpression.AddSymbol(soBlockIndentEnd, p2^.charPos);
                        Continue;
                      end
                    else
                      begin
                        newExpression.Add(p1^);
                        Continue;
                      end;
                  end
                else
                  begin
                    PrintError(SymbolOperationTextDecl[p1^.Symbol].Decl);
                    Exit;
                  end;
              end
            else
              begin
                PrintError('expression structor Illegal');
                Exit;
              end;
          end;

        inc(SymbolIndex);
      end;
  end;

begin
  Result := nil;
  if Exps.AvailValueCount = 0 then
      Exit;

  if Exps.GetSymbolCount([
    soBlockIndentBegin, soBlockIndentEnd,
    soPropIndentBegin, soPropIndentEnd,
    soEolSymbol, soUnknow]) > 0 then
    begin
      PrintError('Illegal symbol');
      Exit;
    end;

  SymbolIndex := 0;
  newExpression := TSymbolExpression.Create(Exps.FTextStyle);
  ParseAborted := False;

  ProcessSymbol(soUnknow);

  if ParseAborted then
    begin
      newExpression.Free;
      PrintError('Illegal');
    end
  else
      Result := newExpression;
end;

function RebuildAllSymbol(Exps: TSymbolExpression): TSymbolExpression;
var
  SymbolIndex: Integer;
  ParseAborted: Boolean;

  procedure PrintError(const s: SystemString);
  begin
    ParseAborted := True;
    if s <> '' then
        DoStatus(Format('indent symbol failed : %s', [s]))
    else
        DoStatus('indent symbol failed');
  end;

  function ProcessIndent(OwnerIndentSym: TSymbolOperation): TSymbolExpression;
  var
    p1, p2: PExpressionListData;
    LocalExp, ResExp: TSymbolExpression;
  begin
    LocalExp := TSymbolExpression.Create(Exps.FTextStyle);
    Result := LocalExp;
    while True do
      begin
        if SymbolIndex >= Exps.Count then
            Break;

        p1 := Exps[SymbolIndex];

        if (p1^.DeclType in [edtSymbol]) then
          begin
            if p1^.Symbol in [soBlockIndentBegin, soPropIndentBegin] then
              begin
                inc(SymbolIndex);

                ResExp := ProcessIndent(p1^.Symbol);
                LocalExp.AddExpressionAsValue(True, ResExp, p1^.Symbol, SymbolOperationTextDecl[p1^.Symbol].Decl, p1^.charPos);

                if SymbolIndex >= Exps.Count then
                  begin
                    PrintError('indent Illegal');
                    Exit;
                  end;
              end
            else if ((OwnerIndentSym = soBlockIndentBegin) and (p1^.Symbol = soBlockIndentEnd)) or
              ((OwnerIndentSym = soPropIndentBegin) and (p1^.Symbol = soPropIndentEnd)) then
              begin
                Exit;
              end
            else if p1^.Symbol in [soCommaSymbol] then
              begin
                LocalExp.Add(p1^);
              end
            else
              begin
                LocalExp.Add(p1^);
              end;
          end
        else if (p1^.DeclType in AllExpressionValueType) then
          begin
            if p1^.DeclType = edtProcExp then
              begin
                LocalExp.Add(p1^);
                inc(SymbolIndex);
                Continue;
              end;

            inc(SymbolIndex);
            if SymbolIndex >= Exps.Count then
              begin
                LocalExp.Add(p1^);
                Break;
              end;

            p2 := Exps[SymbolIndex];

            if p2^.DeclType = edtSymbol then
              begin
                if (p2^.Symbol in [soBlockIndentBegin, soPropIndentBegin]) then
                  begin
                    if (p1^.DeclType in MethodToken) then
                      begin
                        PrintError('method Illegal');
                        Exit;
                      end;

                    LocalExp.Add(p1^);
                    inc(SymbolIndex);

                    ResExp := ProcessIndent(p2^.Symbol);
                    LocalExp.AddExpressionAsValue(True, ResExp, p2^.Symbol, SymbolOperationTextDecl[p2^.Symbol].Decl, p2^.charPos);

                    if SymbolIndex >= Exps.Count then
                      begin
                        PrintError('indent Illegal');
                        Exit;
                      end;

                  end
                else if ((OwnerIndentSym = soBlockIndentBegin) and (p2^.Symbol = soBlockIndentEnd)) or
                  ((OwnerIndentSym = soPropIndentBegin) and (p2^.Symbol = soPropIndentEnd)) then
                  begin
                    LocalExp.Add(p1^);
                    Exit;
                  end
                else if p2^.Symbol = soCommaSymbol then
                  begin
                    PrintError('Comma Illegal');
                    Exit;
                  end
                else
                  begin
                    LocalExp.Add(p1^);
                    LocalExp.Add(p2^);
                  end;
              end
            else
              begin
                PrintError('expression structor Illegal');
                Exit;
              end;
          end;

        inc(SymbolIndex);
      end;
  end;

  function ProcessPriority(_e: TSymbolExpression): TSymbolExpression;
  var
    i, j: Integer;
    E, ResExp: TSymbolExpression;
    p, funcP: PExpressionListData;
  begin
    E := RebuildLogicalPrioritySymbol(_e);
    if E = nil then
      begin
        Result := nil;
        PrintError('parse priority failed');
        Exit;
      end;

    Result := TSymbolExpression.Create(E.FTextStyle);

    for i := 0 to E.Count - 1 do
      begin
        p := E[i];
        if p^.DeclType = edtExpressionAsValue then
          begin
            case p^.Symbol of
              soBlockIndentBegin:
                begin
                  Result.AddSymbol(soBlockIndentBegin, p^.charPos);
                  ResExp := ProcessPriority(p^.Expression);
                  if ResExp <> nil then
                    begin
                      Result.AddExpression(ResExp);
                      DisposeObject(ResExp);
                    end;
                  Result.AddSymbol(soBlockIndentEnd, p^.charPos);
                end;
              soPropIndentBegin:
                begin
                  Result.AddSymbol(soPropIndentBegin, p^.charPos);
                  ResExp := ProcessPriority(p^.Expression);
                  if ResExp <> nil then
                    begin
                      Result.AddExpression(ResExp);
                      DisposeObject(ResExp);
                    end;
                  Result.AddSymbol(soPropIndentEnd, p^.charPos);
                end;
              else
                begin
                  Break;
                end;
            end;
          end
        else if p^.DeclType = edtProcExp then
          begin
            funcP := Result.AddFunc(VarToStr(p^.Value), p^.charPos);
            if (p^.Expression.Count > 0) and (p^.Expression.First^.Expression.Count > 0) then
              for j := 0 to p^.Expression.Count - 1 do
                begin
                  ResExp := RebuildAllSymbol(p^.Expression[j]^.Expression);
                  if ResExp <> nil then
                      funcP^.Expression.AddExpressionAsValue(True, ResExp, soParameter, VarToStr(p^.Expression[j]^.Value), p^.Expression[j]^.charPos);
                end;
          end
        else
          begin
            Result.Add(p^);
          end;
      end;
    DisposeObject([E]);
  end;

var
  rse: TSymbolExpression;
begin
  Result := nil;
  SymbolIndex := 0;
  ParseAborted := False;

  rse := ProcessIndent(soUnknow);
  Result := ProcessPriority(rse);
  DisposeObject(rse);
end;

function BuildAsOpCode(DebugMode: Boolean; SymbExps: TSymbolExpression; const uName: SystemString; LineNo: Integer): TOpCode;
var
  NewSymbExps: TSymbolExpression;
  SymbolIndex: Integer;
  BuildAborted: Boolean;
  OpContainer: TCoreClassListForObj;

  procedure PrintError(const s: SystemString);
  begin
    BuildAborted := True;
    if s <> '' then
        DoStatus(Format('build op failed : %s', [s]))
    else
        DoStatus('build op failed');
  end;

  function NewOpValue(uName: SystemString): TOpCode;
  begin
    Result := op_Value.Create(False);
    Result.ParsedInfo := uName;
    Result.ParsedLineNo := LineNo;
    OpContainer.Add(Result);
  end;

  function NewOpProc(uName: SystemString): TOpCode;
  begin
    Result := op_Proc.Create(False);
    Result.ParsedInfo := uName;
    Result.ParsedLineNo := LineNo;
    OpContainer.Add(Result);
  end;

  function NewOpPrefixFromSym(sym: TSymbolOperation; const uName: SystemString): TOpCode;
  begin
    case sym of
      soAdd: Result := op_Add_Prefix.Create(False);
      soSub: Result := op_Sub_Prefix.Create(False);
      else
        Result := nil;
    end;
    if Result <> nil then
      begin
        Result.ParsedInfo := uName;
        Result.ParsedLineNo := LineNo;
        OpContainer.Add(Result);
      end;
  end;

  function NewOpFromSym(sym: TSymbolOperation; const uName: SystemString): TOpCode;
  begin
    case sym of
      soAdd: Result := op_Add.Create(False);
      soSub: Result := op_Sub.Create(False);
      soMul: Result := op_Mul.Create(False);
      soDiv: Result := op_Div.Create(False);
      soMod: Result := op_Mod.Create(False);
      soIntDiv: Result := op_IntDiv.Create(False);
      soPow: Result := op_Pow.Create(False);
      soOr: Result := op_Or.Create(False);
      soAnd: Result := op_And.Create(False);
      soXor: Result := op_Xor.Create(False);
      soEqual: Result := op_Equal.Create(False);
      soLessThan: Result := op_LessThan.Create(False);
      soEqualOrLessThan: Result := op_EqualOrLessThan.Create(False);
      soGreaterThan: Result := op_GreaterThan.Create(False);
      soEqualOrGreaterThan: Result := op_EqualOrGreaterThan.Create(False);
      soNotEqual: Result := op_NotEqual.Create(False);
      soShl: Result := op_Shl.Create(False);
      soShr: Result := op_Shr.Create(False);
      else
        Result := nil;
    end;
    if Result <> nil then
      begin
        Result.ParsedInfo := uName;
        Result.ParsedLineNo := LineNo;

        OpContainer.Add(Result);
      end;
  end;

  function ProcessIndent(OwnerIndentSym: TSymbolOperation): TOpCode;
  var
    i: Integer;
    p1, p2: PExpressionListData;
    LocalOp, OldOp, ResOp, ProcOp: TOpCode;
  begin
    LocalOp := nil;
    OldOp := nil;
    ResOp := nil;
    Result := nil;

    while True do
      begin
        if SymbolIndex >= NewSymbExps.Count then
          begin
            if LocalOp <> nil then
                Result := LocalOp;
            Break;
          end;

        p1 := NewSymbExps[SymbolIndex];

        if (p1^.DeclType in [edtSymbol]) then
          begin
            if p1^.Symbol in [soBlockIndentBegin, soPropIndentBegin] then
              begin
                inc(SymbolIndex);
                ResOp := ProcessIndent(p1^.Symbol);
                if ResOp <> nil then
                  begin
                    if LocalOp <> nil then
                      begin
                        LocalOp.AddLink(ResOp);
                      end
                    else
                      begin
                        LocalOp := NewOpValue(uName);
                        LocalOp.AddLink(ResOp);
                      end;
                  end
                else
                  begin
                    PrintError('logical operotion Illegal');
                    Break;
                  end;
              end
            else if ((OwnerIndentSym = soBlockIndentBegin) and (p1^.Symbol = soBlockIndentEnd)) or
              ((OwnerIndentSym = soPropIndentBegin) and (p1^.Symbol = soPropIndentEnd)) then
              begin
                Result := LocalOp;
                Break;
              end
            else if p1^.Symbol in OpLogicalSymbol then
              begin
                if LocalOp <> nil then
                  begin
                    OldOp := LocalOp;
                    LocalOp := NewOpFromSym(p1^.Symbol, uName);
                    if LocalOp = nil then
                      begin
                        PrintError('prefix symbol Illegal');
                        Break;
                      end;
                    LocalOp.AddLink(OldOp);
                  end
                else
                  begin
                    // fixed symbol prefix, -(operation), -proc(xx)...
                    if (SymbolIndex + 1 < NewSymbExps.Count) then
                      begin
                        p2 := NewSymbExps[SymbolIndex + 1];
                        if (p1^.Symbol in [soAdd, soSub]) then
                          begin
                            if (p2^.DeclType = edtSymbol) and (p2^.Symbol in [soBlockIndentBegin, soPropIndentBegin]) then
                              begin
                                inc(SymbolIndex);
                                ResOp := ProcessIndent(p2^.Symbol);
                                if ResOp <> nil then
                                  begin
                                    LocalOp := NewOpPrefixFromSym(p1^.Symbol, uName);
                                    if LocalOp = nil then
                                      begin
                                        PrintError('prefix symbol Illegal');
                                        Break;
                                      end;
                                    LocalOp.AddLink(ResOp);
                                  end
                                else
                                  begin
                                    PrintError('logical operotion Illegal');
                                    Break;
                                  end;
                                Continue;
                              end
                            else if (p2^.DeclType = edtProcExp) and (p2^.Symbol = soProc) then
                              begin
                                ProcOp := NewOpProc(uName);
                                ProcOp.AddValue(p2^.Value);
                                for i := 0 to p2^.Expression.Count - 1 do
                                  begin
                                    ResOp := BuildAsOpCode(False, p2^.Expression[i]^.Expression, uName, LineNo);
                                    if ResOp <> nil then
                                        ProcOp.AddLink(ResOp)
                                    else
                                      begin
                                        PrintError('method Illegal');
                                        Break;
                                      end;
                                  end;

                                LocalOp := NewOpPrefixFromSym(p1^.Symbol, uName);
                                LocalOp.AddLink(ProcOp);

                                inc(SymbolIndex, 2);
                                Continue;
                              end;
                          end;
                      end;
                    PrintError('logical operotion Illegal');
                    Break;
                  end;
              end
            else
              begin
                PrintError('logical operotion Illegal');
                Break;
              end;
          end
        else if (p1^.DeclType in AllExpressionValueType) then
          begin
            if p1^.DeclType = edtProcExp then
              begin
                ProcOp := NewOpProc(uName);
                ProcOp.AddValue(p1^.Value);
                for i := 0 to p1^.Expression.Count - 1 do
                  begin
                    ResOp := BuildAsOpCode(False, p1^.Expression[i]^.Expression, uName, LineNo);
                    if ResOp <> nil then
                        ProcOp.AddLink(ResOp)
                    else
                      begin
                        PrintError('method Illegal');
                        Break;
                      end;
                  end;

                if LocalOp <> nil then
                  begin
                    LocalOp.AddLink(ProcOp);
                  end
                else
                  begin
                    LocalOp := NewOpValue(uName);
                    LocalOp.AddLink(ProcOp);
                  end;
                inc(SymbolIndex);
                Continue;
              end;

            inc(SymbolIndex);
            if SymbolIndex >= NewSymbExps.Count then
              begin
                if LocalOp <> nil then
                  begin
                    LocalOp.AddValueT(p1^.Value, dt2op(p1^.DeclType));
                  end
                else
                  begin
                    LocalOp := NewOpValue(uName);
                    LocalOp.AddValueT(p1^.Value, dt2op(p1^.DeclType));
                  end;
                Result := LocalOp;
                Break;
              end;

            p2 := NewSymbExps[SymbolIndex];

            if p2^.DeclType = edtSymbol then
              begin
                if (p2^.Symbol in [soBlockIndentBegin, soPropIndentBegin]) then
                  begin
                    // function call
                    if not(p1^.DeclType in MethodToken) then
                      begin
                        PrintError('method Illegal');
                        Break;
                      end
                    else
                      begin
                      end;

                    inc(SymbolIndex);
                    ResOp := ProcessIndent(p2^.Symbol);

                  end
                else if ((OwnerIndentSym = soBlockIndentBegin) and (p2^.Symbol = soBlockIndentEnd)) or
                  ((OwnerIndentSym = soPropIndentBegin) and (p2^.Symbol = soPropIndentEnd)) then
                  begin
                    if LocalOp <> nil then
                      begin
                        LocalOp.AddValueT(p1^.Value, dt2op(p1^.DeclType));
                      end
                    else
                      begin
                        LocalOp := NewOpValue(uName);
                        LocalOp.AddValueT(p1^.Value, dt2op(p1^.DeclType));
                      end;
                    Result := LocalOp;
                    Break;
                  end
                else if p2^.Symbol in OpLogicalSymbol then
                  begin
                    if LocalOp <> nil then
                      begin
                        OldOp := LocalOp;
                        OldOp.AddValueT(p1^.Value, dt2op(p1^.DeclType));
                        LocalOp := NewOpFromSym(p2^.Symbol, uName);
                        LocalOp.AddLink(OldOp);
                      end
                    else
                      begin
                        LocalOp := NewOpFromSym(p2^.Symbol, uName);
                        LocalOp.AddValueT(p1^.Value, dt2op(p1^.DeclType));
                      end;
                  end
                else
                  begin
                    PrintError('Illegal');
                    Break;
                  end;
              end
            else
              begin
                PrintError('Illegal');
                Break;
              end;
          end;

        inc(SymbolIndex);
      end;
  end;

  procedure ProcessOpContainer(Successed: Boolean);
  var
    i: Integer;
  begin
    for i := 0 to OpContainer.Count - 1 do
      if Successed then
          TOpCode(OpContainer[i]).AutoFreeLink := True
      else
          DisposeObject(TOpCode(OpContainer[i]));
    OpContainer.Clear;
  end;

begin
  Result := nil;
  if SymbExps <> nil then
    begin
      NewSymbExps := RebuildAllSymbol(SymbExps);
      if NewSymbExps <> nil then
        begin
          if DebugMode then
              NewSymbExps.PrintDebug(True);

          if NewSymbExps.GetSymbolCount([soBlockIndentBegin, soPropIndentBegin]) =
            NewSymbExps.GetSymbolCount([soBlockIndentEnd, soPropIndentEnd]) then
            begin
              OpContainer := TCoreClassListForObj.Create;

              SymbolIndex := 0;
              BuildAborted := False;
              Result := ProcessIndent(soUnknow);
              ProcessOpContainer(Result <> nil);
              DisposeObject(OpContainer);
            end;
          DisposeObject(NewSymbExps);
        end;
    end;
end;

function BuildAsOpCode(SymbExps: TSymbolExpression): TOpCode;
begin
  Result := BuildAsOpCode(False, SymbExps, '', 0);
end;

function BuildAsOpCode(DebugMode: Boolean; SymbExps: TSymbolExpression): TOpCode;
begin
  Result := BuildAsOpCode(DebugMode, SymbExps, '', 0);
end;

function BuildAsOpCode(DebugMode: Boolean; TextStyle: TTextStyle; ExpressionText: SystemString): TOpCode;
var
  sym: TSymbolExpression;
begin
  sym := ParseTextExpressionAsSymbol(TextStyle, '', ExpressionText, nil, DefaultOpRT);
  Result := BuildAsOpCode(DebugMode, sym, '', 0);
  DisposeObject(sym);
end;

function BuildAsOpCode(TextStyle: TTextStyle; ExpressionText: SystemString): TOpCode;
var
  sym: TSymbolExpression;
begin
  sym := ParseTextExpressionAsSymbol(TextStyle, '', ExpressionText, nil, DefaultOpRT);
  Result := BuildAsOpCode(False, sym, '', 0);
  DisposeObject(sym);
end;

function BuildAsOpCode(ExpressionText: SystemString): TOpCode;
var
  sym: TSymbolExpression;
begin
  sym := ParseTextExpressionAsSymbol(ExpressionText);
  Result := BuildAsOpCode(False, sym, '', 0);
  DisposeObject(sym);
end;

function BuildAsOpCode(DebugMode: Boolean; TextStyle: TTextStyle; ExpressionText: SystemString; RefrenceOpRT: TOpCustomRunTime): TOpCode;
var
  sym: TSymbolExpression;
begin
  sym := ParseTextExpressionAsSymbol(TextStyle, '', ExpressionText, nil, RefrenceOpRT);
  Result := BuildAsOpCode(DebugMode, sym, '', 0);
  DisposeObject(sym);
end;

function BuildAsOpCode(TextStyle: TTextStyle; ExpressionText: SystemString; RefrenceOpRT: TOpCustomRunTime): TOpCode;
var
  sym: TSymbolExpression;
begin
  sym := ParseTextExpressionAsSymbol(TextStyle, '', ExpressionText, nil, RefrenceOpRT);
  Result := BuildAsOpCode(False, sym, '', 0);
  DisposeObject(sym);
end;

function BuildAsOpCode(ExpressionText: SystemString; RefrenceOpRT: TOpCustomRunTime): TOpCode;
var
  sym: TSymbolExpression;
begin
  sym := ParseTextExpressionAsSymbol(ExpressionText, RefrenceOpRT);
  Result := BuildAsOpCode(False, sym, '', 0);
  DisposeObject(sym);
end;

function EvaluateExpressionValue_M(UsedCache: Boolean; SpecialAsciiToken: TListPascalString;
  TextEngClass: TTextParsingClass; TextStyle: TTextStyle; ExpressionText: SystemString; const OnGetValue: TOnDeclValueMethod): Variant;
var
  sym: TSymbolExpression;
  Op: TOpCode;
  i: Integer;
begin
  if UsedCache then
    begin
      LockObject(OpCache);
      Op := TOpCode(OpCache[ExpressionText]);
      UnLockObject(OpCache);
    end;

  if (Op <> nil) and (UsedCache) then
    begin
      try
          Result := Op.Execute(DefaultOpRT);
      except
          Result := NULL;
      end;
    end
  else
    begin
      Result := NULL;
      sym := ParseTextExpressionAsSymbol_M(SpecialAsciiToken, TextEngClass, TextStyle, '', ExpressionText, OnGetValue, DefaultOpRT);

      if sym <> nil then
        begin
          Op := BuildAsOpCode(False, sym, 'Main', -1);
          if Op <> nil then
            begin
              try
                Result := Op.Execute(DefaultOpRT);
                if UsedCache then
                  begin
                    LockObject(OpCache);
                    OpCache.Add(ExpressionText, Op);
                    UnLockObject(OpCache);
                  end
                else
                    DisposeObject(Op);
              except
                  Result := NULL;
              end;
            end;
          DisposeObject(sym);
        end;
    end;
end;

function EvaluateExpressionValue_C(UsedCache: Boolean; SpecialAsciiToken: TListPascalString;
  TextEngClass: TTextParsingClass; TextStyle: TTextStyle; ExpressionText: SystemString; const OnGetValue: TOnDeclValueCall): Variant;
var
  sym: TSymbolExpression;
  Op: TOpCode;
  i: Integer;
begin
  if UsedCache then
    begin
      LockObject(OpCache);
      Op := TOpCode(OpCache[ExpressionText]);
      UnLockObject(OpCache);
    end;

  if (Op <> nil) and (UsedCache) then
    begin
      try
          Result := Op.Execute(DefaultOpRT);
      except
          Result := NULL;
      end;
    end
  else
    begin
      Result := NULL;
      sym := ParseTextExpressionAsSymbol_C(SpecialAsciiToken, TextEngClass, TextStyle, '', ExpressionText, OnGetValue, DefaultOpRT);

      if sym <> nil then
        begin
          Op := BuildAsOpCode(False, sym, 'Main', -1);
          if Op <> nil then
            begin
              try
                Result := Op.Execute(DefaultOpRT);
                if UsedCache then
                  begin
                    LockObject(OpCache);
                    OpCache.Add(ExpressionText, Op);
                    UnLockObject(OpCache);
                  end
                else
                    DisposeObject(Op);
              except
                  Result := NULL;
              end;
            end;
          DisposeObject(sym);
        end;
    end;
end;

function EvaluateExpressionValue_P(UsedCache: Boolean; SpecialAsciiToken: TListPascalString;
  TextEngClass: TTextParsingClass; TextStyle: TTextStyle; ExpressionText: SystemString; const OnGetValue: TOnDeclValueProc): Variant;
var
  sym: TSymbolExpression;
  Op: TOpCode;
  i: Integer;
begin
  if UsedCache then
    begin
      LockObject(OpCache);
      Op := TOpCode(OpCache[ExpressionText]);
      UnLockObject(OpCache);
    end;

  if (Op <> nil) and (UsedCache) then
    begin
      try
          Result := Op.Execute(DefaultOpRT);
      except
          Result := NULL;
      end;
    end
  else
    begin
      Result := NULL;
      sym := ParseTextExpressionAsSymbol_P(SpecialAsciiToken, TextEngClass, TextStyle, '', ExpressionText, OnGetValue, DefaultOpRT);

      if sym <> nil then
        begin
          Op := BuildAsOpCode(False, sym, 'Main', -1);
          if Op <> nil then
            begin
              try
                Result := Op.Execute(DefaultOpRT);
                if UsedCache then
                  begin
                    LockObject(OpCache);
                    OpCache.Add(ExpressionText, Op);
                    UnLockObject(OpCache);
                  end
                else
                    DisposeObject(Op);
              except
                  Result := NULL;
              end;
            end;
          DisposeObject(sym);
        end;
    end;
end;

{$ENDREGION 'internal imp'}


procedure CleanOpCache();
begin
  LockObject(OpCache);
  OpCache.Clear;
  UnLockObject(OpCache);
end;

type
  TExpression_ConstVL = class
    VL: THashVariantList;
    procedure GetValue(const Decl: SystemString; var ValType: TExpressionDeclType; var Value: Variant);
  end;

procedure TExpression_ConstVL.GetValue(const Decl: SystemString; var ValType: TExpressionDeclType; var Value: Variant);
begin
  if (VL <> nil) and (VL.Exists(Decl)) then
    begin
      Value := VL[Decl];
      ValType := VariantToExpressionDeclType(Value);
    end
end;

function IsSymbolVectorExpression(ExpressionText: SystemString; TextStyle: TTextStyle; SpecialAsciiToken: TListPascalString): Boolean;
var
  t: TTextParsing;
  L: TPascalStringList;
begin
  t := TTextParsing.Create(umlDeleteChar(ExpressionText, #13#10#32#9), TextStyle, SpecialAsciiToken, SpacerSymbol.v);
  L := TPascalStringList.Create;
  if t.FillSymbolVector(L) then
    begin
      if (L.Count = 2) and (L[1].L = 0) then
          Result := False
      else
          Result := L.Count > 1;
    end;
  DisposeObject(t);
  DisposeObject(L);
end;

function EvaluateExpressionValue(UsedCache: Boolean;
  SpecialAsciiToken: TListPascalString; DebugMode: Boolean; TextStyle: TTextStyle; ExpressionText: SystemString;
  opRT: TOpCustomRunTime; const_vl: THashVariantList): Variant;
var
  v: TExpressionValueVector;
  sym: TSymbolExpression;
  Op: TOpCode;
  i: Integer;
  exp_const_vl: TExpression_ConstVL;
begin
  if IsSymbolVectorExpression(ExpressionText, TextStyle, SpecialAsciiToken) then
    begin
      v := EvaluateExpressionVector(DebugMode, UsedCache, SpecialAsciiToken, TextStyle, ExpressionText, opRT, const_vl);
      Result := ExpressionValueVectorToStr(v).Text;
      SetLength(v, 0);
      Exit;
    end;

  if (UsedCache) and (const_vl = nil) then
    begin
      LockObject(OpCache);
      Op := TOpCode(OpCache[ExpressionText]);
      UnLockObject(OpCache);
    end;

  if (Op <> nil) and (UsedCache) and (const_vl = nil) then
    begin
      try
          Result := Op.Execute(opRT);
      except
          Result := NULL;
      end;
    end
  else
    begin
      exp_const_vl := TExpression_ConstVL.Create;
      exp_const_vl.VL := const_vl;

      Result := NULL;
      sym := ParseTextExpressionAsSymbol(SpecialAsciiToken, TextStyle, '', ExpressionText, {$IFDEF FPC}@{$ENDIF FPC}exp_const_vl.GetValue, opRT);

      if sym <> nil then
        begin
          Op := BuildAsOpCode(DebugMode, sym, 'Main', -1);
          if Op <> nil then
            begin
              try
                Result := Op.Execute(opRT);

                if (UsedCache) and (const_vl = nil) then
                  begin
                    LockObject(OpCache);
                    OpCache.Add(ExpressionText, Op);
                    UnLockObject(OpCache);
                  end
                else
                    DisposeObject(Op);
              except
                  Result := NULL;
              end;
            end;
          DisposeObject(sym);
        end
      else
        begin
        end;
      DisposeObject(exp_const_vl);
    end;
end;

function EvaluateExpressionValue(UsedCache: Boolean; SpecialAsciiToken: TListPascalString;
  DebugMode: Boolean; TextStyle: TTextStyle; ExpressionText: SystemString; opRT: TOpCustomRunTime): Variant;
begin
  Result := EvaluateExpressionValue(UsedCache, SpecialAsciiToken, DebugMode, TextStyle, ExpressionText, opRT, nil);
end;

function EvaluateExpressionValue(UsedCache: Boolean; ExpressionText: SystemString; opRT: TOpCustomRunTime): Variant;
begin
  Result := EvaluateExpressionValue(UsedCache, nil, False, tsPascal, ExpressionText, opRT, nil);
end;

function EvaluateExpressionValue(UsedCache: Boolean; ExpressionText: SystemString): Variant;
begin
  Result := EvaluateExpressionValue(UsedCache, nil, False, tsPascal, ExpressionText, DefaultOpRT, nil);
end;

function EvaluateExpressionValue(UsedCache: Boolean; TextStyle: TTextStyle; ExpressionText: SystemString): Variant;
begin
  Result := EvaluateExpressionValue(UsedCache, nil, False, TextStyle, ExpressionText, DefaultOpRT, nil);
end;

function EvaluateExpressionValue(UsedCache: Boolean; TextStyle: TTextStyle; ExpressionText: SystemString; opRT: TOpCustomRunTime): Variant;
begin
  Result := EvaluateExpressionValue(UsedCache, nil, False, TextStyle, ExpressionText, opRT, nil);
end;

function EvaluateExpressionValue(UsedCache: Boolean; SpecialAsciiToken: TListPascalString; DebugMode: Boolean; ExpressionText: SystemString; opRT: TOpCustomRunTime): Variant;
begin
  Result := EvaluateExpressionValue(UsedCache, SpecialAsciiToken, DebugMode, tsPascal, ExpressionText, opRT, nil);
end;

function EvaluateExpressionValue(UsedCache: Boolean; SpecialAsciiToken: TListPascalString; ExpressionText: SystemString; opRT: TOpCustomRunTime): Variant;
begin
  Result := EvaluateExpressionValue(UsedCache, SpecialAsciiToken, False, tsPascal, ExpressionText, opRT, nil);
end;

function EvaluateExpressionValue(UsedCache: Boolean; SpecialAsciiToken: TListPascalString; DebugMode: Boolean; ExpressionText: SystemString): Variant;
begin
  Result := EvaluateExpressionValue(UsedCache, SpecialAsciiToken, DebugMode, tsPascal, ExpressionText, DefaultOpRT, nil);
end;

function EvaluateExpressionValue(UsedCache: Boolean; SpecialAsciiToken: TListPascalString; ExpressionText: SystemString): Variant;
begin
  Result := EvaluateExpressionValue(UsedCache, SpecialAsciiToken, False, tsPascal, ExpressionText, DefaultOpRT, nil);
end;

function EvaluateExpressionValue(UsedCache: Boolean; SpecialAsciiToken: TListPascalString; TextStyle: TTextStyle; ExpressionText: SystemString; opRT: TOpCustomRunTime): Variant;
begin
  Result := EvaluateExpressionValue(UsedCache, SpecialAsciiToken, False, TextStyle, ExpressionText, opRT, nil);
end;

function EvaluateExpressionValue(ExpressionText: SystemString; opRT: TOpCustomRunTime): Variant;
begin
  Result := EvaluateExpressionValue(True, ExpressionText, opRT);
end;

function EvaluateExpressionValue(ExpressionText: SystemString): Variant;
begin
  Result := EvaluateExpressionValue(True, ExpressionText);
end;

function EvaluateExpressionValue(TextStyle: TTextStyle; ExpressionText: SystemString): Variant;
begin
  Result := EvaluateExpressionValue(True, nil, False, TextStyle, ExpressionText, DefaultOpRT);
end;

function EvaluateExpressionValue(TextStyle: TTextStyle; ExpressionText: SystemString; opRT: TOpCustomRunTime): Variant;
begin
  Result := EvaluateExpressionValue(True, nil, False, TextStyle, ExpressionText, opRT);
end;

function EvaluateExpressionValue(SpecialAsciiToken: TListPascalString; DebugMode: Boolean; ExpressionText: SystemString; opRT: TOpCustomRunTime): Variant;
begin
  Result := EvaluateExpressionValue(True, SpecialAsciiToken, DebugMode, tsPascal, ExpressionText, opRT);
end;

function EvaluateExpressionValue(SpecialAsciiToken: TListPascalString; ExpressionText: SystemString; opRT: TOpCustomRunTime): Variant;
begin
  Result := EvaluateExpressionValue(True, SpecialAsciiToken, False, tsPascal, ExpressionText, opRT);
end;

function EvaluateExpressionValue(SpecialAsciiToken: TListPascalString; DebugMode: Boolean; ExpressionText: SystemString): Variant;
begin
  Result := EvaluateExpressionValue(True, SpecialAsciiToken, DebugMode, tsPascal, ExpressionText, DefaultOpRT);
end;

function EvaluateExpressionValue(SpecialAsciiToken: TListPascalString; ExpressionText: SystemString): Variant;
begin
  Result := EvaluateExpressionValue(True, SpecialAsciiToken, False, tsPascal, ExpressionText, DefaultOpRT);
end;

function EvaluateExpressionValue(SpecialAsciiToken: TListPascalString; TextStyle: TTextStyle; ExpressionText: SystemString; opRT: TOpCustomRunTime): Variant;
begin
  Result := EvaluateExpressionValue(True, SpecialAsciiToken, False, TextStyle, ExpressionText, opRT);
end;

function EvaluateExpressionVector(DebugMode, UsedCache: Boolean; SpecialAsciiToken: TListPascalString; TextStyle: TTextStyle; ExpressionText: SystemString;
  opRT: TOpCustomRunTime; const_vl: THashVariantList): TExpressionValueVector;
var
  t: TTextParsing;
  L: TPascalStringList;
  i: Integer;
begin
  SetLength(Result, 0);
  t := TTextParsing.Create(ExpressionText, TextStyle, SpecialAsciiToken, SpacerSymbol.v);
  L := TPascalStringList.Create;
  if t.FillSymbolVector(L) then
    begin
      SetLength(Result, L.Count);
      for i := 0 to L.Count - 1 do
        begin
          try
              Result[i] := EvaluateExpressionValue(UsedCache, SpecialAsciiToken, DebugMode, TextStyle, L[i], opRT, const_vl);
          except
              Result[i] := NULL;
          end;
        end;
    end;
  DisposeObject(L);
  DisposeObject(t);
end;

function EvaluateExpressionVector(UsedCache: Boolean; SpecialAsciiToken: TListPascalString; TextStyle: TTextStyle; ExpressionText: SystemString;
  opRT: TOpCustomRunTime; const_vl: THashVariantList): TExpressionValueVector;
begin
  Result := EvaluateExpressionVector(False, UsedCache, SpecialAsciiToken, TextStyle, ExpressionText, opRT, const_vl);
end;

function EvaluateExpressionVector(SpecialAsciiToken: TListPascalString; TextStyle: TTextStyle; ExpressionText: SystemString;
  opRT: TOpCustomRunTime; const_vl: THashVariantList): TExpressionValueVector;
begin
  Result := EvaluateExpressionVector(False, False, SpecialAsciiToken, TextStyle, ExpressionText, opRT, const_vl);
end;

function EvaluateExpressionVector(ExpressionText: SystemString; opRT: TOpCustomRunTime; const_vl: THashVariantList): TExpressionValueVector;
begin
  Result := EvaluateExpressionVector(nil, tsPascal, ExpressionText, opRT, const_vl);
end;

function EvaluateExpressionVector(ExpressionText: SystemString; const_vl: THashVariantList): TExpressionValueVector;
begin
  Result := EvaluateExpressionVector(ExpressionText, DefaultOpRT, const_vl);
end;

function EvaluateExpressionVector(ExpressionText: SystemString; TextStyle: TTextStyle): TExpressionValueVector;
begin
  Result := EvaluateExpressionVector(nil, TextStyle, ExpressionText, nil, nil);
end;

function EvaluateExpressionVector(ExpressionText: SystemString): TExpressionValueVector;
begin
  Result := EvaluateExpressionVector(ExpressionText, nil);
end;

function EvaluateExpressionMatrix(W, H: Integer;
  SpecialAsciiToken: TListPascalString; TextStyle: TTextStyle; ExpressionText: SystemString;
  opRT: TOpCustomRunTime; const_vl: THashVariantList): TExpressionValueMatrix; overload;
var
  buff: TExpressionValueVector;
  i, j, k: Integer;
begin
  SetLength(Result, 0, 0);
  buff := EvaluateExpressionVector(SpecialAsciiToken, TextStyle, ExpressionText, opRT, const_vl);
  if length(buff) >= W * H then
    begin
      SetLength(Result, H, W);
      k := 0;
      for j := 0 to H - 1 do
        for i := 0 to W - 1 do
          begin
            Result[j, i] := buff[k];
            inc(k);
          end;
    end;
end;

function EvaluateExpressionMatrix(W, H: Integer; ExpressionText: SystemString; opRT: TOpCustomRunTime; const_vl: THashVariantList): TExpressionValueMatrix;
begin
  Result := EvaluateExpressionMatrix(W, H, nil, tsPascal, ExpressionText, opRT, const_vl);
end;

function EvaluateExpressionMatrix(W, H: Integer; ExpressionText: SystemString; const_vl: THashVariantList): TExpressionValueMatrix;
begin
  Result := EvaluateExpressionMatrix(W, H, ExpressionText, DefaultOpRT, const_vl);
end;

function EvaluateExpressionMatrix(W, H: Integer; ExpressionText: SystemString; TextStyle: TTextStyle): TExpressionValueMatrix;
begin
  Result := EvaluateExpressionMatrix(W, H, nil, TextStyle, ExpressionText, nil, nil);
end;

function EvaluateExpressionMatrix(W, H: Integer; ExpressionText: SystemString): TExpressionValueMatrix;
begin
  Result := EvaluateExpressionMatrix(W, H, ExpressionText, DefaultOpRT, nil);
end;

function EStr(s: U_String): U_String;
begin
  Result := umlVarToStr(EvaluateExpressionValue(s), False);
end;

function EStrToInt(s: U_String; default: Integer): Integer;
var
  v: Variant;
begin
  v := EvaluateExpressionValue(s);
  if VarIsNumeric(v) then
      Result := v
  else
      Result := default;
end;

function EStrToInt64(s: U_String; default: Int64): Int64;
var
  v: Variant;
begin
  v := EvaluateExpressionValue(s);
  if VarIsNumeric(v) then
      Result := v
  else
      Result := default;
end;

function EStrToFloat(s: U_String; default: Double): Double;
begin
  Result := EStrToDouble(s, default);
end;

function EStrToSingle(s: U_String; default: Single): Single;
var
  v: Variant;
begin
  v := EvaluateExpressionValue(s);
  if VarIsNumeric(v) then
      Result := v
  else
      Result := default;
end;

function EStrToDouble(s: U_String; default: Double): Double;
var
  v: Variant;
begin
  v := EvaluateExpressionValue(s);
  if VarIsNumeric(v) then
      Result := v
  else
      Result := default;
end;

function ExpressionValueVectorToStr(v: TExpressionValueVector): TPascalString;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to length(v) - 1 do
    begin
      if VarIsNull(v[i]) then
          Result.Append('error, ')
      else if VarIsStr(v[i]) then
          Result.Append(VarToStr(v[i]) + ', ')
      else
          Result.Append(VarToStr(v[i]) + ', ');
    end;
  Result := Result.TrimChar(', ');
end;

procedure DoStatus(v: TExpressionValueVector);
var
  i: Integer;
begin
  for i := 0 to length(v) - 1 do
      DoStatusNoLn(umlVarToStr(v[i]) + ' ');
  DoStatusNoLn;
end;

procedure DoStatus(v: TExpressionValueMatrix);
var
  i: Integer;
begin
  for i := 0 to high(v) do
      DoStatus(v[i]);
end;

procedure EvaluateExpressionVectorAndMatrix_test_;
var
  VL: THashVariantList;
  buff: TExpressionValueVector;
  EM: TExpressionValueMatrix;
begin
  VL := THashVariantList.Create;
  VL['a1'] := 10;
  VL['a2'] := 20;
  VL['a3'] := 30;
  buff := EvaluateExpressionVector('a1,a2,a3,a1*a2,a1+a2+a3,min(a1,a2,a3)*a3', VL);
  EM := EvaluateExpressionMatrix(3, 2, 'a1,a2,a3,a1*a2,a1+a2+a3,min(a1,a2,a3)*a3', VL);
  DisposeObject(VL);
  SetLength(buff, 0);
  SetLength(EM, 0, 0);
end;

initialization

OpCache := THashObjectList.CustomCreate(True, $FFFF);

finalization

DisposeObject(OpCache);

end.
