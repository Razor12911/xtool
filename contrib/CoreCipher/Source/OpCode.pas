{ * opCode                                                                     * }
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
unit OpCode;

{$INCLUDE zDefine.inc}

interface

uses SysUtils, Variants, Math, CoreClasses, PascalStrings, DoStatusIO, ListEngine, UnicodeMixedLib, DataFrameEngine;

type
  TOpValueType = (
    ovtBool, ovtInt, ovtInt64, ovtUInt64, ovtWord, ovtByte, ovtSmallInt, ovtShortInt, ovtUInt,
    ovtSingle, ovtDouble, ovtCurrency,
    ovtString, ovtProc,
    ovtUnknow);

  TOpCode = class;
  TOpCustomRunTime = class;

  TOpParam = array of Variant;

  TOnOpCall = function(var Param: TOpParam): Variant;
  TOnOpMethod = function(var Param: TOpParam): Variant of object;
  TOnObjectOpCall = function(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
  TOnObjectOpMethod = function(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant of object;

{$IFDEF FPC}
  TOnOpProc = function(var Param: TOpParam): Variant is nested;
  TOnObjectOpProc = function(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant is nested;
{$ELSE FPC}
  TOnOpProc = reference to function(var Param: TOpParam): Variant;
  TOnObjectOpProc = reference to function(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
{$ENDIF FPC}
  POpRTData = ^TOpRTData;

  TOpRTData = record
    Param: TOpParam;
    Name, Description, Category: SystemString;
    OnOpCall: TOnOpCall;
    OnOpMethod: TOnOpMethod;
    OnObjectOpCall: TOnObjectOpCall;
    OnObjectOpMethod: TOnObjectOpMethod;
    OnOpProc: TOnOpProc;
    OnObjectOpProc: TOnObjectOpProc;
    procedure Init;
  end;

  TOpCustomRunTime = class(TCoreClassObject)
  protected
    procedure FreeNotifyProc(p: Pointer);

    function DoInt(var Param: TOpParam): Variant;
    function DoFrac(var Param: TOpParam): Variant;
    function DoExp(var Param: TOpParam): Variant;
    function DoCos(var Param: TOpParam): Variant;
    function DoSin(var Param: TOpParam): Variant;
    function DoLn(var Param: TOpParam): Variant;
    function DoArcTan(var Param: TOpParam): Variant;
    function DoSqrt(var Param: TOpParam): Variant;
    function DoSqr(var Param: TOpParam): Variant;
    function DoTan(var Param: TOpParam): Variant;
    function DoRound(var Param: TOpParam): Variant;
    function DoTrunc(var Param: TOpParam): Variant;
    function DoDeg(var Param: TOpParam): Variant;
    function DoPower(var Param: TOpParam): Variant;

    function DoPI(var Param: TOpParam): Variant;
    function DoBool(var Param: TOpParam): Variant;
    function DoTrue(var Param: TOpParam): Variant;
    function DoFalse(var Param: TOpParam): Variant;
    function DoRColor(var Param: TOpParam): Variant;
    function DoVec2(var Param: TOpParam): Variant;
    function DoVec3(var Param: TOpParam): Variant;
    function DoVec4(var Param: TOpParam): Variant;

    function DoRandom(var Param: TOpParam): Variant;
    function DoRandomFloat(var Param: TOpParam): Variant;

    function DoMax(var Param: TOpParam): Variant;
    function DoMin(var Param: TOpParam): Variant;
    function DoClamp(var Param: TOpParam): Variant;
    function DoIfThen(var Param: TOpParam): Variant;

    function DoStr(var Param: TOpParam): Variant;

    function DoGetFirst(var Param: TOpParam): Variant;
    function DoDeleteFirst(var Param: TOpParam): Variant;
    function DoGetLast(var Param: TOpParam): Variant;
    function DoDeleteLast(var Param: TOpParam): Variant;

    function DoMultiple(var Param: TOpParam): Variant;
    function DoPrint(var Param: TOpParam): Variant;

    procedure InternalReg; virtual;
  public
    ProcList: THashList;
    Trigger: POpRTData;

    UserObject: TCoreClassObject;
    UserData: Pointer;

    constructor Create;
    constructor CustomCreate(maxHashLen: Integer); virtual;
    destructor Destroy; override;

    function GetProcDescription(ProcName: SystemString): SystemString; overload;
    function GetAllProcDescription(): TPascalStringList; overload;
    function GetAllProcDescription(Category: U_String): TPascalStringList; overload;

    function RegOpC(ProcName: SystemString; OnProc: TOnOpCall): POpRTData; overload;
    function RegOpC(ProcName, ProcDescription: SystemString; OnProc: TOnOpCall): POpRTData; overload;
    function RegOpM(ProcName: SystemString; OnProc: TOnOpMethod): POpRTData; overload;
    function RegOpM(ProcName, ProcDescription: SystemString; OnProc: TOnOpMethod): POpRTData; overload;
    function RegObjectOpC(ProcName: SystemString; OnProc: TOnObjectOpCall): POpRTData; overload;
    function RegObjectOpC(ProcName, ProcDescription: SystemString; OnProc: TOnObjectOpCall): POpRTData; overload;
    function RegObjectOpM(ProcName: SystemString; OnProc: TOnObjectOpMethod): POpRTData; overload;
    function RegObjectOpM(ProcName, ProcDescription: SystemString; OnProc: TOnObjectOpMethod): POpRTData; overload;
    function RegOpP(ProcName: SystemString; OnProc: TOnOpProc): POpRTData; overload;
    function RegOpP(ProcName, ProcDescription: SystemString; OnProc: TOnOpProc): POpRTData; overload;
    function RegObjectOpP(ProcName: SystemString; OnProc: TOnObjectOpProc): POpRTData; overload;
    function RegObjectOpP(ProcName, ProcDescription: SystemString; OnProc: TOnObjectOpProc): POpRTData; overload;
  end;

  opClass = class of TOpCode;

  TOpCode = class(TCoreClassObject)
  private type
    POpData = ^opData;

    opData = record
      Op: TOpCode;
      Value: Variant;
      ValueType: TOpValueType;
    end;
  protected
    FParam: TCoreClassList;
    FAutoFreeLink: Boolean;
    function DoExecute(opRT: TOpCustomRunTime): Variant; virtual;
    function GetParam(index: Integer): POpData;
    procedure EvaluateParam(opRT: TOpCustomRunTime); overload;
    procedure EvaluateParam(printLog: Boolean; opRT: TOpCustomRunTime); overload;
  public
    Owner: TOpCode;
    ParsedInfo: SystemString;
    ParsedLineNo: Integer;

    constructor Create(AFreeLink: Boolean);
    destructor Destroy; override;

    procedure SaveToStream(stream: TCoreClassStream);
    class function LoadFromStream(stream: TCoreClassStream; out LoadedOp: TOpCode): Boolean;

    function AddValue(v: Variant): Integer; overload;
    function AddValueT(v: Variant; VT: TOpValueType): Integer; overload;
    function AddLink(Obj: TOpCode): Integer;

    function CloneNewSelf: TOpCode;

    property Param[index: Integer]: POpData read GetParam; default;
    function Count: Integer;

    function Execute: Variant; overload;
    function Execute(opRT: TOpCustomRunTime): Variant; overload;

    function OwnerRoot: TOpCode;

    property AutoFreeLink: Boolean read FAutoFreeLink write FAutoFreeLink;
  end;

  op_Value = class sealed(TOpCode)
  private
    // a
    function DoExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_Proc = class sealed(TOpCode)
  private
    // proc(a,b,c...)
    function DoExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_Add_Prefix = class sealed(TOpCode)
  private
    // +proc
    function DoExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_Sub_Prefix = class sealed(TOpCode)
  private
    // -proc
    function DoExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_Add = class sealed(TOpCode)
  private
    // a + b + n...
    function DoExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_Sub = class sealed(TOpCode)
  private
    // a - b - n...
    function DoExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_Mul = class sealed(TOpCode)
  private
    // a * b * n...
    function DoExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_Div = class sealed(TOpCode)
  private
    // a / b / n...
    function DoExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_IntDiv = class sealed(TOpCode)
  private
    // a div b div n...
    function DoExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_Pow = class sealed(TOpCode)
  private
    // a pow b
    function DoExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_Mod = class sealed(TOpCode)
  private
    // a mod b mod n...
    function DoExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_Or = class sealed(TOpCode)
  private
    // a or b or n...
    function DoExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_And = class sealed(TOpCode)
  private
    // a and b and n...
    function DoExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_Xor = class sealed(TOpCode)
  private
    // a xor b xor n...
    function DoExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_Shl = class sealed(TOpCode)
  private
    // a shl b shl n...
    function DoExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_Shr = class sealed(TOpCode)
  private
    // a shr b shr n...
    function DoExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_Equal = class sealed(TOpCode)
  private
    // a = b
    function DoExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_LessThan = class sealed(TOpCode)
  private
    // a < b
    function DoExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_EqualOrLessThan = class sealed(TOpCode)
  private
    // a <= b
    function DoExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_GreaterThan = class sealed(TOpCode)
  private
    // a > b
    function DoExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_EqualOrGreaterThan = class sealed(TOpCode)
  private
    // a >= b
    function DoExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_NotEqual = class sealed(TOpCode)
  private
    // a <> b
    function DoExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_Symbol_Sub = class sealed(TOpCode)
  private
    // -a
    function DoExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_Symbol_Add = class sealed(TOpCode)
  private
    // +a
    function DoExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

function LoadOpFromStream(stream: TCoreClassStream; out LoadedOp: TOpCode): Boolean;

var
  DefaultOpRT: TOpCustomRunTime;

implementation

uses Geometry2DUnit, Geometry3DUnit;

type
  opRegData = record
    opClass: opClass;
    OpName: TPascalString;
    hash: Cardinal;
  end;

  POpRegData = ^opRegData;

var
  OpList: TCoreClassList;

procedure TOpRTData.Init;
begin
  SetLength(Param, 0);
  Name := '';
  Description := '';
  Category := '';
  OnOpCall := nil;
  OnOpMethod := nil;
  OnObjectOpCall := nil;
  OnObjectOpMethod := nil;
  OnOpProc := nil;
  OnObjectOpProc := nil;
end;

function GetRegistedOp(Name: TPascalString): POpRegData;
var
  i: Integer;
  p: POpRegData;
  hash: Cardinal;
begin
  Result := nil;
  hash := FastHashPPascalString(@Name);
  for i := 0 to OpList.Count - 1 do
    begin
      p := OpList[i];
      if (p^.hash = hash) and (SameText(Name, p^.OpName)) then
          Exit(p);
    end;
end;

procedure RegisterOp(c: opClass);
var
  p: POpRegData;
begin
  if GetRegistedOp(c.ClassName) <> nil then
      raise Exception.Create('same op ' + c.ClassName);
  new(p);
  p^.opClass := c;
  p^.OpName := p^.opClass.ClassName;
  p^.hash := FastHashPPascalString(@p^.OpName);
  OpList.Add(p);
end;

procedure _FreeOp;
var
  i: Integer;
  p: POpRegData;
begin
  for i := 0 to OpList.Count - 1 do
    begin
      p := OpList[i];
      Dispose(p);
    end;
  DisposeObject(OpList);
end;

function LoadOpFromStream(stream: TCoreClassStream; out LoadedOp: TOpCode): Boolean;

  function LoadFromDataFrame_(CurDataEng: TDataFrameEngine): TOpCode;
  var
    AName: SystemString;
    RegPtr: POpRegData;
    i, cnt: Integer;
    NeedNewOp: Boolean;
    newDataEng: TDataFrameEngine;
    v: Variant;
    VT: TOpValueType;
  begin
    AName := CurDataEng.Reader.ReadString;
    RegPtr := GetRegistedOp(AName);
    if RegPtr <> nil then
      begin
        Result := RegPtr^.opClass.Create(True);
        Result.ParsedInfo := CurDataEng.Reader.ReadString;
        Result.ParsedLineNo := CurDataEng.Reader.ReadInteger;
        cnt := CurDataEng.Reader.ReadInteger;
        for i := 0 to cnt - 1 do
          begin
            NeedNewOp := CurDataEng.Reader.ReadBool;

            if NeedNewOp then
              begin
                // create new TOpCode
                newDataEng := TDataFrameEngine.Create;
                CurDataEng.Reader.ReadDataFrame(newDataEng);
                Result.AddLink(LoadFromDataFrame_(newDataEng));
                DisposeObject(newDataEng);
              end
            else
              begin
                v := CurDataEng.Reader.ReadVariant;
                VT := TOpValueType(CurDataEng.Reader.ReadInteger);
                Result.AddValueT(v, VT);
              end;
          end;
      end
    else
        raise Exception.Create('opCode failed');
  end;

var
  DataEng: TDataFrameEngine;
  DataEdition: Integer;
begin
  Result := False;
  DataEng := TDataFrameEngine.Create;
  try
    DataEng.DecodeFrom(stream, True);
    DataEdition := DataEng.Reader.ReadInteger;
    if DataEdition = 1 then
      begin
        LoadedOp := LoadFromDataFrame_(DataEng);
        Result := True;
      end
    else
        LoadedOp := nil;
  except
  end;
  DisposeObject(DataEng);
end;

procedure TOpCustomRunTime.FreeNotifyProc(p: Pointer);
begin
  SetLength(POpRTData(p)^.Param, 0);
  Dispose(POpRTData(p));
end;

function TOpCustomRunTime.DoInt(var Param: TOpParam): Variant;
var
  v: Variant;
  i: Integer;
begin
  v := 0;
  for i := low(Param) to high(Param) do
      v := v + Param[i];
  Result := Int(v);
end;

function TOpCustomRunTime.DoFrac(var Param: TOpParam): Variant;
var
  v: Variant;
  i: Integer;
begin
  v := 0;
  for i := low(Param) to high(Param) do
      v := v + Param[i];
  Result := Frac(v);
end;

function TOpCustomRunTime.DoExp(var Param: TOpParam): Variant;
var
  v: Variant;
  i: Integer;
begin
  v := 0;
  for i := low(Param) to high(Param) do
      v := v + Param[i];
  Result := Exp(v);
end;

function TOpCustomRunTime.DoCos(var Param: TOpParam): Variant;
var
  v: Variant;
  i: Integer;
begin
  v := 0;
  for i := low(Param) to high(Param) do
      v := v + Param[i];
  Result := Cos(v);
end;

function TOpCustomRunTime.DoSin(var Param: TOpParam): Variant;
var
  v: Variant;
  i: Integer;
begin
  v := 0;
  for i := low(Param) to high(Param) do
      v := v + Param[i];
  Result := Sin(v);
end;

function TOpCustomRunTime.DoLn(var Param: TOpParam): Variant;
var
  v: Variant;
  i: Integer;
begin
  v := 0;
  for i := low(Param) to high(Param) do
      v := v + Param[i];
  Result := ln(v);
end;

function TOpCustomRunTime.DoArcTan(var Param: TOpParam): Variant;
var
  v: Variant;
  i: Integer;
begin
  v := 0;
  for i := low(Param) to high(Param) do
      v := v + Param[i];
  Result := ArcTan(v);
end;

function TOpCustomRunTime.DoSqrt(var Param: TOpParam): Variant;
var
  v: Variant;
  i: Integer;
begin
  v := 0;
  for i := low(Param) to high(Param) do
      v := v + Param[i];
  Result := Sqrt(v);
end;

function TOpCustomRunTime.DoSqr(var Param: TOpParam): Variant;
var
  v: Variant;
  i: Integer;
begin
  v := 0;
  for i := low(Param) to high(Param) do
      v := v + Param[i];
  Result := Sqr(v);
end;

function TOpCustomRunTime.DoTan(var Param: TOpParam): Variant;
var
  v: Variant;
  i: Integer;
begin
  v := 0;
  for i := low(Param) to high(Param) do
      v := v + Param[i];
  Result := Tan(v);
end;

function TOpCustomRunTime.DoRound(var Param: TOpParam): Variant;
var
  v: Variant;
  i: Integer;
begin
  v := 0;
  for i := low(Param) to high(Param) do
      v := v + Param[i];
  Result := Round(Double(v));
end;

function TOpCustomRunTime.DoTrunc(var Param: TOpParam): Variant;
var
  v: Variant;
  i: Integer;
begin
  v := 0;
  for i := low(Param) to high(Param) do
      v := v + Param[i];
  Result := Trunc(Double(v));
end;

function TOpCustomRunTime.DoDeg(var Param: TOpParam): Variant;
var
  v: Variant;
  i: Integer;
begin
  v := 0;
  for i := low(Param) to high(Param) do
      v := v + Param[i];
  Result := NormalizeDegAngle(TGeoFloat(v));
end;

function TOpCustomRunTime.DoPower(var Param: TOpParam): Variant;
var
  v: Variant;
  i: Integer;
begin
  if length(Param) = 2 then
      Result := Power(Param[0], Param[1])
  else
      Result := 0;
end;

function TOpCustomRunTime.DoPI(var Param: TOpParam): Variant;
begin
  Result := PI;
end;

function TOpCustomRunTime.DoBool(var Param: TOpParam): Variant;
  function v2b(const v: Variant): Boolean;
  var
    n: TPascalString;
  begin
    if VarIsStr(v) then
      begin
        n := VarToStr(v);
        n := n.DeleteChar(#32#9);
        if n.Same('True') or n.Same('Yes') or n.Same('1') then
            Result := True
        else
            Result := False;
      end
    else if VarIsOrdinal(v) then
        Result := Boolean(v)
    else if VarIsFloat(v) then
        Result := Boolean(Round(Double(v)))
    else
        Result := Boolean(v);
  end;

var
  n: Boolean;
  i: Integer;
begin
  n := True;
  for i := low(Param) to high(Param) do
      n := n and v2b(Param[i]);
  Result := n;
end;

function TOpCustomRunTime.DoTrue(var Param: TOpParam): Variant;
begin
  Result := True;
end;

function TOpCustomRunTime.DoFalse(var Param: TOpParam): Variant;
begin
  Result := False;
end;

function TOpCustomRunTime.DoRColor(var Param: TOpParam): Variant;
var
  buff: array [0 .. 3] of SystemString;
  i: Integer;
begin
  for i := 0 to 2 do
      buff[i] := '0.0';
  buff[3] := '1.0';

  for i := Low(Param) to high(Param) do
      buff[i] := VarToStr(Param[i]);

  Result := Format('RColor(%s,%s,%s,%s)', [buff[0], buff[1], buff[2], buff[3]]);
end;

function TOpCustomRunTime.DoVec2(var Param: TOpParam): Variant;
var
  buff: array [0 .. 1] of SystemString;
  i: Integer;
begin
  for i := Low(buff) to high(buff) do
      buff[i] := '0.0';

  for i := Low(Param) to high(Param) do
      buff[i] := VarToStr(Param[i]);

  Result := Format('Vec2(%s,%s)', [buff[0], buff[1]]);
end;

function TOpCustomRunTime.DoVec3(var Param: TOpParam): Variant;
var
  buff: array [0 .. 2] of SystemString;
  i: Integer;
begin
  for i := Low(buff) to high(buff) do
      buff[i] := '0.0';

  for i := Low(Param) to high(Param) do
      buff[i] := VarToStr(Param[i]);

  Result := Format('Vec3(%s,%s,%s)', [buff[0], buff[1], buff[2]]);
end;

function TOpCustomRunTime.DoVec4(var Param: TOpParam): Variant;
var
  buff: array [0 .. 3] of SystemString;
  i: Integer;
begin
  for i := Low(buff) to high(buff) do
      buff[i] := '0.0';

  for i := Low(Param) to high(Param) do
      buff[i] := VarToStr(Param[i]);

  Result := Format('Vec4(%s,%s,%s,%s)', [buff[0], buff[1], buff[2], buff[3]]);
end;

function TOpCustomRunTime.DoRandom(var Param: TOpParam): Variant;
var
  v: Integer;
  i: Integer;
begin
  v := 0;
  for i := low(Param) to high(Param) do
      v := v + Param[i];

  if v <> 0 then
      Result := MT19937Rand32(v)
  else
      Result := MT19937Rand32(MaxInt);
end;

function TOpCustomRunTime.DoRandomFloat(var Param: TOpParam): Variant;
begin
  Result := MT19937RandF;
end;

function TOpCustomRunTime.DoMax(var Param: TOpParam): Variant;
var
  i: Integer;
begin
  if length(Param) = 0 then
    begin
      Result := NULL;
      Exit;
    end;
  Result := Param[0];
  for i := 1 to length(Param) - 1 do
    if Param[i] > Result then
        Result := Param[i];
end;

function TOpCustomRunTime.DoMin(var Param: TOpParam): Variant;
var
  i: Integer;
begin
  if length(Param) = 0 then
    begin
      Result := NULL;
      Exit;
    end;
  Result := Param[0];
  for i := 1 to length(Param) - 1 do
    if Param[i] < Result then
        Result := Param[i];
end;

function TOpCustomRunTime.DoClamp(var Param: TOpParam): Variant;
var
  minv_, maxv_: Variant;
begin
  if length(Param) <> 3 then
    begin
      if length(Param) > 0 then
          Result := Param[0]
      else
          Result := NULL;
      Exit;
    end;

  if Param[1] > Param[2] then
    begin
      minv_ := Param[2];
      maxv_ := Param[1];
    end
  else
    begin
      minv_ := Param[1];
      maxv_ := Param[2];
    end;

  if Param[0] < minv_ then
      Result := minv_
  else if Param[0] > maxv_ then
      Result := maxv_
  else
      Result := Param[0];
end;

function TOpCustomRunTime.DoIfThen(var Param: TOpParam): Variant;
begin
  if length(Param) <> 3 then
    begin
      Result := NULL;
      Exit;
    end;
  if Boolean(Param[0]) = True then
      Result := Param[1]
  else
      Result := Param[2];
end;

function TOpCustomRunTime.DoStr(var Param: TOpParam): Variant;
var
  n: TPascalString;
  i: Integer;
begin
  n := '';
  for i := low(Param) to high(Param) do
      n.Append(VarToStr(Param[i]));
  Result := n;
end;

function TOpCustomRunTime.DoGetFirst(var Param: TOpParam): Variant;
begin
  if length(Param) = 2 then
      Result := umlGetFirstStr(VarToStr(Param[0]), VarToStr(Param[1])).Text
  else
      Result := '';
end;

function TOpCustomRunTime.DoDeleteFirst(var Param: TOpParam): Variant;
begin
  if length(Param) = 2 then
      Result := umlDeleteFirstStr(VarToStr(Param[0]), VarToStr(Param[1])).Text
  else
      Result := '';
end;

function TOpCustomRunTime.DoGetLast(var Param: TOpParam): Variant;
begin
  if length(Param) = 2 then
      Result := umlGetLastStr(VarToStr(Param[0]), VarToStr(Param[1])).Text
  else
      Result := '';
end;

function TOpCustomRunTime.DoDeleteLast(var Param: TOpParam): Variant;
begin
  if length(Param) = 2 then
      Result := umlDeleteLastStr(VarToStr(Param[0]), VarToStr(Param[1])).Text
  else
      Result := '';
end;

function TOpCustomRunTime.DoMultiple(var Param: TOpParam): Variant;
var
  i: Integer;
begin
  if length(Param) >= 2 then
    begin
      Result := True;
      for i := 1 to length(Param) - 1 do
          Result := Result and umlMultipleMatch(VarToStr(Param[0]), VarToStr(Param[i]));
    end
  else
      Result := True;
end;

function TOpCustomRunTime.DoPrint(var Param: TOpParam): Variant;
var
  i: Integer;
begin
  for i := low(Param) to high(Param) do
    begin
      DoStatusNoLn(Param[i]);
      if i < high(Param) then
          DoStatusNoLn(#32);
    end;

  DoStatusNoLn;
  Result := True;
end;

procedure TOpCustomRunTime.InternalReg;
begin
  ProcList.OnFreePtr := {$IFDEF FPC}@{$ENDIF FPC}FreeNotifyProc;
  RegOpM('Int', 'Int(0..n): math function', {$IFDEF FPC}@{$ENDIF FPC}DoInt)^.Category := 'Base Math';
  RegOpM('Frac', 'Frac(0..n): math function', {$IFDEF FPC}@{$ENDIF FPC}DoFrac)^.Category := 'Base Math';
  RegOpM('Exp', 'Exp(0..n): math function', {$IFDEF FPC}@{$ENDIF FPC}DoExp)^.Category := 'Base Math';
  RegOpM('Cos', 'Cos(0..n): math function', {$IFDEF FPC}@{$ENDIF FPC}DoCos)^.Category := 'Base Math';
  RegOpM('Sin', 'Sin(0..n): math function', {$IFDEF FPC}@{$ENDIF FPC}DoSin)^.Category := 'Base Math';
  RegOpM('Ln', 'Ln(0..n): math function', {$IFDEF FPC}@{$ENDIF FPC}DoLn)^.Category := 'Base Math';
  RegOpM('ArcTan', 'ArcTan(0..n): math function', {$IFDEF FPC}@{$ENDIF FPC}DoArcTan)^.Category := 'Base Math';
  RegOpM('Sqrt', 'Sqrt(0..n): math function', {$IFDEF FPC}@{$ENDIF FPC}DoSqrt)^.Category := 'Base Math';
  RegOpM('Sqr', 'Sqr(0..n): math function', {$IFDEF FPC}@{$ENDIF FPC}DoSqr)^.Category := 'Base Math';
  RegOpM('Tan', 'Tan(0..n): math function', {$IFDEF FPC}@{$ENDIF FPC}DoTan)^.Category := 'Base Math';
  RegOpM('Round', 'Round(0..n): math function', {$IFDEF FPC}@{$ENDIF FPC}DoRound)^.Category := 'Base Math';
  RegOpM('Trunc', 'Trunc(0..n): math function', {$IFDEF FPC}@{$ENDIF FPC}DoTrunc)^.Category := 'Base Math';
  RegOpM('Deg', 'Deg(0..n): NormalizeDegAngle function', {$IFDEF FPC}@{$ENDIF FPC}DoDeg)^.Category := 'Base Math';
  RegOpM('Power', 'Power(float,float): Power: Raise base to any power function', {$IFDEF FPC}@{$ENDIF FPC}DoPower)^.Category := 'Base Math';

  RegOpM('PI', 'PI(): return PI', {$IFDEF FPC}@{$ENDIF FPC}DoPI)^.Category := 'Base Math';

  RegOpM('Bool', 'Bool(n..n): convert any variant as bool', {$IFDEF FPC}@{$ENDIF FPC}DoBool)^.Category := 'Base Math';
  RegOpM('Boolean', 'Boolean(n..n): convert any variant as bool', {$IFDEF FPC}@{$ENDIF FPC}DoBool)^.Category := 'Base Math';

  RegOpM('True', 'True(): return true', {$IFDEF FPC}@{$ENDIF FPC}DoTrue)^.Category := 'Base Math';
  RegOpM('False', 'False(): return false', {$IFDEF FPC}@{$ENDIF FPC}DoFalse)^.Category := 'Base Math';

  RegOpM('RColor', 'RColor(R,G,B,A): return RColor string', {$IFDEF FPC}@{$ENDIF FPC}DoRColor)^.Category := 'Base Math';
  RegOpM('Vec2', 'Vec2(X,Y): return Vec2 string', {$IFDEF FPC}@{$ENDIF FPC}DoVec2)^.Category := 'Base Math';
  RegOpM('Vec3', 'Vec3(X,Y,Z): return Vec3 string', {$IFDEF FPC}@{$ENDIF FPC}DoVec3)^.Category := 'Base Math';
  RegOpM('Vec4', 'Vec4(X,Y,Z,W): return Vec4 string', {$IFDEF FPC}@{$ENDIF FPC}DoVec4)^.Category := 'Base Math';

  RegOpM('Random', 'Random(0..n): return number', {$IFDEF FPC}@{$ENDIF FPC}DoRandom)^.Category := 'Base Math';
  RegOpM('RandomFloat', 'RandomFloat(): return float', {$IFDEF FPC}@{$ENDIF FPC}DoRandomFloat)^.Category := 'Base Math';
  RegOpM('RandomF', 'RandomF(): return float', {$IFDEF FPC}@{$ENDIF FPC}DoRandomFloat)^.Category := 'Base Math';

  RegOpM('Max', 'Max(0..n): return max value', {$IFDEF FPC}@{$ENDIF FPC}DoMax)^.Category := 'Base Math';
  RegOpM('Min', 'Min(0..n): return min value', {$IFDEF FPC}@{$ENDIF FPC}DoMin)^.Category := 'Base Math';
  RegOpM('Clamp', 'Clamp(value, min, max): return clamp value', {$IFDEF FPC}@{$ENDIF FPC}DoClamp)^.Category := 'Base Math';
  RegOpM('IfThen', 'IfThen(bool, if true then of value, if false then of value): return if value', {$IFDEF FPC}@{$ENDIF FPC}DoIfThen)^.Category := 'Base Math';

  RegOpM('Str', 'Str(n..n): convert any variant as string', {$IFDEF FPC}@{$ENDIF FPC}DoStr)^.Category := 'Base String';
  RegOpM('Base String', 'String(n..n): convert any variant as string', {$IFDEF FPC}@{$ENDIF FPC}DoStr)^.Category := 'Base String';
  RegOpM('Text', 'Text(n..n): convert any variant as string', {$IFDEF FPC}@{$ENDIF FPC}DoStr)^.Category := 'Base String';

  RegOpM('GetFirst', 'GetFirst(string, split Char): return first split segment', {$IFDEF FPC}@{$ENDIF FPC}DoGetFirst)^.Category := 'Base String';
  RegOpM('First', 'First(string, split Char): return first split segment', {$IFDEF FPC}@{$ENDIF FPC}DoGetFirst)^.Category := 'Base String';
  RegOpM('DeleteFirst', 'DeleteFirst(string, split Char): return removed at after first split segment of value', {$IFDEF FPC}@{$ENDIF FPC}DoDeleteFirst)^.Category := 'Base String';
  RegOpM('GetLast', 'GetLast(string, split Char): return last split segment', {$IFDEF FPC}@{$ENDIF FPC}DoGetLast)^.Category := 'Base String';
  RegOpM('Last', 'Last(string, split Char): return last split segment', {$IFDEF FPC}@{$ENDIF FPC}DoGetLast)^.Category := 'Base String';
  RegOpM('DeleteLast', 'DeleteLast(string, split Char): return removed at after last split segment of value', {$IFDEF FPC}@{$ENDIF FPC}DoDeleteLast)^.Category := 'Base String';

  RegOpM('MultipleMatch', 'MultipleMatch(multile exp, n..n): return bool', {$IFDEF FPC}@{$ENDIF FPC}DoMultiple)^.Category := 'Base String';
  RegOpM('Multiple', 'MultipleMatch(multile exp, n..n): return bool', {$IFDEF FPC}@{$ENDIF FPC}DoMultiple)^.Category := 'Base String';

  RegOpM('Print', 'Print(n..n): output to console', {$IFDEF FPC}@{$ENDIF FPC}DoPrint)^.Category := 'Base String';
  RegOpM('DoStatus', 'DoStatus(n..n): output to console', {$IFDEF FPC}@{$ENDIF FPC}DoPrint)^.Category := 'Base String';
end;

constructor TOpCustomRunTime.Create;
begin
  CustomCreate(512);
end;

constructor TOpCustomRunTime.CustomCreate(maxHashLen: Integer);
begin
  inherited Create;
  ProcList := THashList.CustomCreate(maxHashLen);
  ProcList.AutoFreeData := True;

  Trigger := nil;

  UserObject := nil;
  UserData := nil;

  InternalReg;
end;

destructor TOpCustomRunTime.Destroy;
begin
  DisposeObject(ProcList);
  inherited Destroy;
end;

function TOpCustomRunTime.GetProcDescription(ProcName: SystemString): SystemString;
var
  p: POpRTData;
begin
  Result := ProcName + '(): no Descripion';
  p := ProcList[ProcName];
  if p <> nil then
    if p^.Description <> '' then
        Result := p^.Description;
end;

function TOpCustomRunTime.GetAllProcDescription(): TPascalStringList;
begin
  Result := GetAllProcDescription('*');
end;

function TOpCustomRunTime.GetAllProcDescription(Category: U_String): TPascalStringList;
var
  arry: THashDataArray;
  hl: THashObjectList;
  ns, tmp: TPascalStringList;

  i, j: Integer;
  p: POpRTData;
  n: TPascalString;
begin
  Result := TPascalStringList.Create;
  arry := ProcList.GetHashDataArray();

  hl := THashObjectList.CustomCreate(True, 256);
  for i := Low(arry) to High(arry) do
    begin
      p := arry[i]^.Data;
      if not hl.Exists(p^.Category) then
          hl.FastAdd(p^.Category, TPascalStringList.Create);
      tmp := hl[p^.Category] as TPascalStringList;

      if p^.Description <> '' then
          n := p^.Description
      else
          n := p^.Name + '(): no Descripion';

      tmp.Add(n);
    end;
  SetLength(arry, 0);

  ns := TPascalStringList.Create;
  hl.GetListData(ns);
  for i := 0 to ns.Count - 1 do
    if umlMultipleMatch(Category, ns[i]) then
      begin
        Result.Add(PFormat('%s:', [ns[i].Text]));
        tmp := ns.Objects[i] as TPascalStringList;
        for j := 0 to tmp.Count - 1 do
            Result.Add('  ' + tmp[j]);
        Result.Add('');
      end;
  n := '';
  DisposeObject(ns);
  DisposeObject(hl);
end;

function TOpCustomRunTime.RegOpC(ProcName: SystemString; OnProc: TOnOpCall): POpRTData;
var
  p: POpRTData;
begin
  new(p);
  p^.Init;
  p^.Name := ProcName;
  p^.OnOpCall := OnProc;
  ProcList.Add(ProcName, p, True);
  Result := p;
end;

function TOpCustomRunTime.RegOpC(ProcName, ProcDescription: SystemString; OnProc: TOnOpCall): POpRTData;
var
  p: POpRTData;
begin
  new(p);
  p^.Init;
  p^.Name := ProcName;
  p^.Description := ProcDescription;
  p^.OnOpCall := OnProc;
  ProcList.Add(ProcName, p, True);
  Result := p;
end;

function TOpCustomRunTime.RegOpM(ProcName: SystemString; OnProc: TOnOpMethod): POpRTData;
var
  p: POpRTData;
begin
  new(p);
  p^.Init;
  p^.Name := ProcName;
  p^.OnOpMethod := OnProc;
  ProcList.Add(ProcName, p, True);
  Result := p;
end;

function TOpCustomRunTime.RegOpM(ProcName, ProcDescription: SystemString; OnProc: TOnOpMethod): POpRTData;
var
  p: POpRTData;
begin
  new(p);
  p^.Init;
  p^.Name := ProcName;
  p^.Description := ProcDescription;
  p^.OnOpMethod := OnProc;
  ProcList.Add(ProcName, p, True);
  Result := p;
end;

function TOpCustomRunTime.RegObjectOpC(ProcName: SystemString; OnProc: TOnObjectOpCall): POpRTData;
var
  p: POpRTData;
begin
  new(p);
  p^.Init;
  p^.Name := ProcName;
  p^.OnObjectOpCall := OnProc;
  ProcList.Add(ProcName, p, True);
  Result := p;
end;

function TOpCustomRunTime.RegObjectOpC(ProcName, ProcDescription: SystemString; OnProc: TOnObjectOpCall): POpRTData;
var
  p: POpRTData;
begin
  new(p);
  p^.Init;
  p^.Name := ProcName;
  p^.Description := ProcDescription;
  p^.OnObjectOpCall := OnProc;
  ProcList.Add(ProcName, p, True);
  Result := p;
end;

function TOpCustomRunTime.RegObjectOpM(ProcName: SystemString; OnProc: TOnObjectOpMethod): POpRTData;
var
  p: POpRTData;
begin
  new(p);
  p^.Init;
  p^.Name := ProcName;
  p^.OnObjectOpMethod := OnProc;
  ProcList.Add(ProcName, p, True);
  Result := p;
end;

function TOpCustomRunTime.RegObjectOpM(ProcName, ProcDescription: SystemString; OnProc: TOnObjectOpMethod): POpRTData;
var
  p: POpRTData;
begin
  new(p);
  p^.Init;
  p^.Name := ProcName;
  p^.Description := ProcDescription;
  p^.OnObjectOpMethod := OnProc;
  ProcList.Add(ProcName, p, True);
  Result := p;
end;

function TOpCustomRunTime.RegOpP(ProcName: SystemString; OnProc: TOnOpProc): POpRTData;
var
  p: POpRTData;
begin
  new(p);
  p^.Init;
  p^.Name := ProcName;
  p^.OnOpProc := OnProc;
  ProcList.Add(ProcName, p, True);
  Result := p;
end;

function TOpCustomRunTime.RegOpP(ProcName, ProcDescription: SystemString; OnProc: TOnOpProc): POpRTData;
var
  p: POpRTData;
begin
  new(p);
  p^.Init;
  p^.Name := ProcName;
  p^.Description := ProcDescription;
  p^.OnOpProc := OnProc;
  ProcList.Add(ProcName, p, True);
  Result := p;
end;

function TOpCustomRunTime.RegObjectOpP(ProcName: SystemString; OnProc: TOnObjectOpProc): POpRTData;
var
  p: POpRTData;
begin
  new(p);
  p^.Init;
  p^.Name := ProcName;
  p^.OnObjectOpProc := OnProc;
  ProcList.Add(ProcName, p, True);
  Result := p;
end;

function TOpCustomRunTime.RegObjectOpP(ProcName, ProcDescription: SystemString; OnProc: TOnObjectOpProc): POpRTData;
var
  p: POpRTData;
begin
  new(p);
  p^.Init;
  p^.Name := ProcName;
  p^.Description := ProcDescription;
  p^.OnObjectOpProc := OnProc;
  ProcList.Add(ProcName, p, True);
  Result := p;
end;

function TOpCode.DoExecute(opRT: TOpCustomRunTime): Variant;
begin
  Result := NULL;
end;

function TOpCode.GetParam(index: Integer): POpData;
begin
  Result := FParam[index];
end;

procedure TOpCode.EvaluateParam(opRT: TOpCustomRunTime);
begin
  EvaluateParam(False, opRT);
end;

procedure TOpCode.EvaluateParam(printLog: Boolean; opRT: TOpCustomRunTime);
var
  i: Integer;
  p: POpData;
begin
  for i := 0 to FParam.Count - 1 do
    begin
      p := FParam[i];
      if p^.Op <> nil then
        begin
          try
              p^.Op.EvaluateParam(printLog, opRT);
          except
          end;

          try
            p^.Value := p^.Op.DoExecute(opRT);

            if printLog then
                DoStatus('%s value:%s', [ClassName, VarToStr(p^.Value)]);
          except
          end;
        end;
    end;
end;

constructor TOpCode.Create(AFreeLink: Boolean);
begin
  inherited Create;
  Owner := nil;
  FParam := TCoreClassList.Create;
  FAutoFreeLink := AFreeLink;
  ParsedInfo := '';
  ParsedLineNo := 0;
end;

destructor TOpCode.Destroy;
var
  i: Integer;
  p: POpData;
begin
  if FParam <> nil then
    begin
      for i := 0 to FParam.Count - 1 do
        begin
          p := FParam[i];
          if (FAutoFreeLink) and (p^.Op <> nil) then
              DisposeObject(p^.Op);
          Dispose(p);
        end;
      FParam.Clear;
      DisposeObject(FParam);
    end;
  inherited Destroy;
end;

procedure TOpCode.SaveToStream(stream: TCoreClassStream);
  procedure SaveToDataFrame(Op: TOpCode; CurDataEng: TDataFrameEngine);
  var
    i: Integer;
    p: POpData;
    newDataEng: TDataFrameEngine;
  begin
    CurDataEng.WriteString(Op.ClassName);
    CurDataEng.WriteString(Op.ParsedInfo);
    CurDataEng.WriteInteger(Op.ParsedLineNo);
    CurDataEng.WriteInteger(Op.Count);
    for i := 0 to Op.Count - 1 do
      begin
        p := Op[i];
        if p^.Op <> nil then
          begin
            CurDataEng.WriteBool(True);
            newDataEng := TDataFrameEngine.Create;
            SaveToDataFrame(p^.Op, newDataEng);
            CurDataEng.WriteDataFrame(newDataEng);
            DisposeObject(newDataEng);
          end
        else
          begin
            CurDataEng.WriteBool(False);
            CurDataEng.WriteVariant(p^.Value);
            CurDataEng.WriteInteger(Integer(p^.ValueType));
          end;
      end;
  end;

var
  DataEng: TDataFrameEngine;
begin
  DataEng := TDataFrameEngine.Create;
  DataEng.WriteInteger(1);
  SaveToDataFrame(Self, DataEng);
  DataEng.EncodeTo(stream, True);
  DisposeObject(DataEng);
end;

class function TOpCode.LoadFromStream(stream: TCoreClassStream; out LoadedOp: TOpCode): Boolean;
begin
  Result := LoadOpFromStream(stream, LoadedOp);
end;

function TOpCode.AddValue(v: Variant): Integer;
var
  p: POpData;
begin
  new(p);
  p^.Op := nil;

  p^.Value := v;

  case VarType(v) of
    varSmallInt: p^.ValueType := ovtSmallInt;
    varInteger: p^.ValueType := ovtInt;
    varSingle: p^.ValueType := ovtSingle;
    varDouble: p^.ValueType := ovtDouble;
    varCurrency: p^.ValueType := ovtCurrency;
    varBoolean: p^.ValueType := ovtBool;
    varShortInt: p^.ValueType := ovtShortInt;
    varByte: p^.ValueType := ovtByte;
    varWord: p^.ValueType := ovtWord;
    varLongWord: p^.ValueType := ovtUInt;
    varInt64: p^.ValueType := ovtInt64;
    varUInt64: p^.ValueType := ovtUInt64;
    else
      begin
        if VarIsStr(v) then
            p^.ValueType := ovtString
        else
            p^.ValueType := ovtUnknow;
      end;
  end;

  Result := FParam.Add(p);
end;

function TOpCode.AddValueT(v: Variant; VT: TOpValueType): Integer;
var
  p: POpData;
begin
  new(p);
  p^.Op := nil;
  p^.Value := v;
  p^.ValueType := VT;
  Result := FParam.Add(p);
end;

function TOpCode.AddLink(Obj: TOpCode): Integer;
var
  p: POpData;
begin
  new(p);

  if Obj.Owner <> nil then
      p^.Op := Obj.CloneNewSelf
  else
      p^.Op := Obj;

  p^.Op.Owner := Self;

  p^.Value := NULL;
  p^.ValueType := ovtUnknow;
  Result := FParam.Add(p);
end;

function TOpCode.CloneNewSelf: TOpCode;
var
  i: Integer;
  p: POpData;
begin
  Result := opClass(Self.ClassType).Create(True);
  Result.ParsedInfo := Self.ParsedInfo;
  Result.ParsedLineNo := Self.ParsedLineNo;

  for i := 0 to FParam.Count - 1 do
    begin
      p := FParam[i];
      if p^.Op <> nil then
          Result.AddLink(p^.Op.CloneNewSelf)
      else
          Result.AddValueT(p^.Value, p^.ValueType);
    end;
end;

function TOpCode.Count: Integer;
begin
  Result := FParam.Count;
end;

function TOpCode.Execute: Variant;
begin
  Result := Execute(DefaultOpRT);
end;

function TOpCode.Execute(opRT: TOpCustomRunTime): Variant;
begin
  try
      EvaluateParam(opRT);
  except
    Result := NULL;
    Exit;
  end;

  try
      Result := DoExecute(opRT);
  except
      Result := NULL;
  end;
end;

function TOpCode.OwnerRoot: TOpCode;
begin
  if Owner = nil then
      Result := Self
  else
      Result := Owner.OwnerRoot;
end;

{ op_Value }

function op_Value.DoExecute(opRT: TOpCustomRunTime): Variant;
begin
  Result := Param[0]^.Value;
end;

{ op_Proc }
function op_Proc.DoExecute(opRT: TOpCustomRunTime): Variant;
var
  p: POpRTData;
  i: Integer;
begin
  Result := NULL;
  if (opRT = nil) then
      opRT := DefaultOpRT;

  p := opRT.ProcList[VarToStr(Param[0]^.Value)];
  if p = nil then
    begin
      if opRT = DefaultOpRT then
          Exit;
      p := DefaultOpRT.ProcList[VarToStr(Param[0]^.Value)];
      if p = nil then
          Exit;
    end;

  if length(p^.Param) <> Count - 1 then
      SetLength(p^.Param, Count - 1);

  for i := 1 to Count - 1 do
      p^.Param[i - 1] := Param[i]^.Value;

  opRT.Trigger := p;

  if Assigned(p^.OnOpCall) then
      Result := p^.OnOpCall(p^.Param);
  if Assigned(p^.OnOpMethod) then
      Result := p^.OnOpMethod(p^.Param);
  if Assigned(p^.OnObjectOpCall) then
      Result := p^.OnObjectOpCall(opRT, p^.Param);
  if Assigned(p^.OnObjectOpMethod) then
      Result := p^.OnObjectOpMethod(opRT, p^.Param);
  if Assigned(p^.OnOpProc) then
      Result := p^.OnOpProc(p^.Param);
  if Assigned(p^.OnObjectOpProc) then
      Result := p^.OnObjectOpProc(opRT, p^.Param);
end;

{ op_Add_Prefix }

function op_Add_Prefix.DoExecute(opRT: TOpCustomRunTime): Variant;
var
  i: Integer;
begin
  if Count = 0 then
      Exit(NULL);
  Result := Param[0]^.Value;
  for i := 1 to Count - 1 do
      Result := Result + Param[i]^.Value;
  Result := - - Result;
end;

{ op_Sub_Prefix }

function op_Sub_Prefix.DoExecute(opRT: TOpCustomRunTime): Variant;
var
  i: Integer;
begin
  if Count = 0 then
      Exit(NULL);
  Result := Param[0]^.Value;
  for i := 1 to Count - 1 do
      Result := Result + Param[i]^.Value;
  Result := -Result;
end;

{ op_Add }

function op_Add.DoExecute(opRT: TOpCustomRunTime): Variant;

  function Fast_VarIsStr(var v: Variant): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
  var
    p: pVarData;
  begin
    // optimized
    p := @TVarData(v);
    while p^.VType = varByRef or varVariant do
        p := pVarData(p^.VPointer);
    Result := (p^.VType = varOleStr) or (p^.VType = varString) or (p^.VType = varUString);
  end;

var
  i: Integer;
  n1, n2: TPascalString;
begin
  if Count = 0 then
      Exit(NULL);
  Result := Param[0]^.Value;

  if Fast_VarIsStr(Result) then
    begin
      // optimized
      n1 := VarToStr(Result);
      for i := 1 to Count - 1 do
        begin
          try
              n1.Append(VarToStr(Param[i]^.Value));
          except
          end;
        end;
      Result := n1.Text;
    end
  else
    begin
      for i := 1 to Count - 1 do
        begin
          try
            if Fast_VarIsStr(Result) then
              begin
                // SystemString combine
                n1 := VarToStr(Result);
                if not umlIsNumber(n1) then
                  begin
                    Result := n1 + VarToStr(Param[i]^.Value);
                    Continue;
                  end
              end;

            if Fast_VarIsStr(Param[i]^.Value) then
              begin
                // SystemString combine
                n2 := VarToStr(Param[i]^.Value);
                if not umlIsNumber(n2) then
                  begin
                    Result := VarToStr(Result) + n2;
                    Continue;
                  end
              end;

            // logic compute
            Result := Result + Param[i]^.Value;
          except
          end;
        end;
    end;
end;

{ op_Sub }

function op_Sub.DoExecute(opRT: TOpCustomRunTime): Variant;
var
  i: Integer;
begin
  if Count = 0 then
      Exit(NULL);
  Result := Param[0]^.Value;
  for i := 1 to Count - 1 do
      Result := Result - Param[i]^.Value;
end;

{ op_Mul }

function op_Mul.DoExecute(opRT: TOpCustomRunTime): Variant;
var
  i: Integer;
begin
  if Count = 0 then
      Exit(NULL);
  Result := Param[0]^.Value;
  for i := 1 to Count - 1 do
      Result := Result * Param[i]^.Value;
end;

{ op_Div }

function op_Div.DoExecute(opRT: TOpCustomRunTime): Variant;
var
  i: Integer;
begin
  if Count = 0 then
      Exit(NULL);
  Result := Param[0]^.Value;
  for i := 1 to Count - 1 do
      Result := Result / Param[i]^.Value;
end;

{ op_IntDiv }

function op_IntDiv.DoExecute(opRT: TOpCustomRunTime): Variant;
var
  i: Integer;
begin
  if Count = 0 then
      Exit(NULL);
  Result := Param[0]^.Value;
  for i := 1 to Count - 1 do
      Result := Result div Param[i]^.Value;
end;

{ op_Pow }
function op_Pow.DoExecute(opRT: TOpCustomRunTime): Variant;
var
  i: Integer;
begin
  if Count = 0 then
      Exit(NULL);
  Result := Param[0]^.Value;
  for i := 1 to Count - 1 do
      Result := Power(Result, Param[i]^.Value);
end;

{ op_Mod }

function op_Mod.DoExecute(opRT: TOpCustomRunTime): Variant;
var
  i: Integer;
begin
  if Count = 0 then
      Exit(NULL);
  Result := Param[0]^.Value;
  for i := 1 to Count - 1 do
      Result := Result mod Param[i]^.Value;
end;

{ op_Or }

function op_Or.DoExecute(opRT: TOpCustomRunTime): Variant;
var
  i: Integer;
begin
  if Count = 0 then
      Exit(NULL);
  Result := Param[0]^.Value;
  for i := 1 to Count - 1 do
      Result := Result or Param[i]^.Value;
end;

{ op_And }

function op_And.DoExecute(opRT: TOpCustomRunTime): Variant;
var
  i: Integer;
begin
  if Count = 0 then
      Exit(NULL);
  Result := Param[0]^.Value;
  for i := 1 to Count - 1 do
      Result := Result and Param[i]^.Value;
end;

{ op_Xor }

function op_Xor.DoExecute(opRT: TOpCustomRunTime): Variant;
var
  i: Integer;
begin
  if Count = 0 then
      Exit(NULL);
  Result := Param[0]^.Value;
  for i := 1 to Count - 1 do
      Result := Result xor Param[i]^.Value;
end;

{ op_shl }

function op_Shl.DoExecute(opRT: TOpCustomRunTime): Variant;
var
  i: Integer;
begin
  if Count = 0 then
      Exit(NULL);
  Result := Param[0]^.Value;
  for i := 1 to Count - 1 do
      Result := UInt64(Result) shl UInt64(Param[i]^.Value);
end;

{ op_shr }

function op_Shr.DoExecute(opRT: TOpCustomRunTime): Variant;
var
  i: Integer;
begin
  if Count = 0 then
      Exit(NULL);
  Result := Param[0]^.Value;
  for i := 1 to Count - 1 do
      Result := UInt64(Result) shr UInt64(Param[i]^.Value);
end;

{ op_Equal }

function op_Equal.DoExecute(opRT: TOpCustomRunTime): Variant;
begin
  Result := Param[0]^.Value = Param[1]^.Value;
end;

{ op_LessThan }

function op_LessThan.DoExecute(opRT: TOpCustomRunTime): Variant;
begin
  Result := Param[0]^.Value < Param[1]^.Value;
end;

{ op_EqualOrLessThan }

function op_EqualOrLessThan.DoExecute(opRT: TOpCustomRunTime): Variant;
begin
  Result := Param[0]^.Value <= Param[1]^.Value;
end;

{ op_GreaterThan }

function op_GreaterThan.DoExecute(opRT: TOpCustomRunTime): Variant;
begin
  Result := Param[0]^.Value > Param[1]^.Value;
end;

{ op_EqualOrGreaterThan }

function op_EqualOrGreaterThan.DoExecute(opRT: TOpCustomRunTime): Variant;
begin
  Result := Param[0]^.Value >= Param[1]^.Value;
end;

{ op_NotEqual }

function op_NotEqual.DoExecute(opRT: TOpCustomRunTime): Variant;
begin
  Result := Param[0]^.Value <> Param[1]^.Value;
end;

{ op_Symbol_Sub }

function op_Symbol_Sub.DoExecute(opRT: TOpCustomRunTime): Variant;
begin
  Result := -Param[0]^.Value;
end;

{ op_Symbol_Add }

function op_Symbol_Add.DoExecute(opRT: TOpCustomRunTime): Variant;
begin
  Result := Param[0]^.Value;
end;

initialization

DefaultOpRT := TOpCustomRunTime.Create;
OleVariantInt64AsDouble := True;

OpList := TCoreClassList.Create;

RegisterOp(op_Value);
RegisterOp(op_Proc);
RegisterOp(op_Add_Prefix);
RegisterOp(op_Sub_Prefix);
RegisterOp(op_Add);
RegisterOp(op_Sub);
RegisterOp(op_Mul);
RegisterOp(op_Div);
RegisterOp(op_IntDiv);
RegisterOp(op_Mod);
RegisterOp(op_Or);
RegisterOp(op_And);
RegisterOp(op_Xor);
RegisterOp(op_Shl);
RegisterOp(op_Shr);
RegisterOp(op_Equal);
RegisterOp(op_LessThan);
RegisterOp(op_EqualOrLessThan);
RegisterOp(op_GreaterThan);
RegisterOp(op_EqualOrGreaterThan);
RegisterOp(op_NotEqual);
RegisterOp(op_Symbol_Sub);
RegisterOp(op_Symbol_Add);

finalization

DisposeObject(DefaultOpRT);
_FreeOp;

end.
