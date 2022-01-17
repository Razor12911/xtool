unit FormExpr;
{--------------------------------------------------------------
| TFormulaParser
|  Multi formula support
|  Extension by Xavier Mor-Mur
|  xmormur@telepolis.com
|  xmormur@teleline.es
|
|---------------------------------------------------------------}
interface
uses Dialogs, OObjects, SysUtils, Classes, ParseExpr, ParseClass;
type

  TFormulaParser = class(TExpressionParser)
  private
    FormulaNames: TStringList;
    tslTmp: TStringList;
    bTrace: Boolean;
    ExprResult: double;
  protected
    procedure FillExpressList; override;
  public
    constructor Create;
    destructor Destroy; override;
    function Formula(FName, FExpr: string): Integer;
    function Eval(FIndex: Integer): double; overload;
    function Eval(FName: string): double; overload;
    function Index(FName: string): Integer;
    function Name(FIndex: Integer): string;
    function Text(FName: string): string;
    property Trace: Boolean read bTrace write bTrace;
  end;
implementation
uses Math;

var
  sExpr: string; // needed for display(...) function

{ TFormulaParser }

constructor TFormulaParser.Create;
begin
  inherited;
  FormulaNames := TStringList.Create;
  tslTmp := TStringList.Create;
  DefineVariable('#', @ExprResult);
end;

destructor TFormulaParser.Destroy;
begin
  inherited;
  FormulaNames.Free;
  tslTmp.Free;
end;

//---> Added start
// set formula string

function TFormulaParser.Formula(FName, FExpr: string): Integer;
var
  i, j: Integer;
  expr: TStringList;
begin
  tslTmp.Clear;
  tslTmp.SetText(PChar(FExpr));
  expr := TStringList.Create;
  Result := -1;
  i := FormulaNames.IndexOf(FName);
  if i >= 0 then
    FormulaNames.Delete(i);
  if tslTmp.Count > 0 then
  begin
    for i := 0 to tslTmp.Count - 1 do
    begin
      if tslTmp.Strings[i] <> '' then
      begin
        j := AddExpression(tslTmp.Strings[i]);
        if j < 0 then
          break;
        if (copy(Expression[j], 1, 5) = 'goto(') or
          (copy(Expression[j], 1, 7) = 'ifgoto(') or
          (copy(Expression[j], 1, 8) = 'display(') or
          (copy(Expression[j], 1, 4) = 'stop') then
          expr.Add(IntToStr(-(j + 1)))
        else
          expr.Add(IntToStr(j + 1));
      end;
    end;
    if expr.Count > 0 then
      Result := FormulaNames.AddObject(FName, TObject(expr));
  end;
end;

// evaluate formula by name

function TFormulaParser.Eval(FName: string): double;
var
  i: Integer;
begin
  Result := 0;
  i := FormulaNames.IndexOf(FName);
  if i >= 0 then
    Result := Eval(i);
end;

// evaluate formula by index

function TFormulaParser.Eval(FIndex: Integer): double;
var
  i, j, k: Integer;
  expr: TStringList;
begin
  ExprResult := 0;
  if (FIndex >= 0) and (FIndex < FormulaNames.Count) then
  begin
    expr := TStringList(FormulaNames.Objects[FIndex]);
    i := 0;
    while i < expr.Count do
    begin
      Val(expr.Strings[i], j, k);
      sExpr := '(' + IntToStr(i + 1) + ') ' + Expression[abs(j) - 1] +
        char($0D0A) + char($0D0A);
      if j < 0 then
      begin
        k := trunc(AsFloat[abs(j) - 1]);
        if k > 0 then
          i := k - 1 // goto(...), ifgoto(...), stop
        else
        begin
          i := i + 1; // display(...)
        end;
      end
      else
      begin
        ExprResult := AsFloat[abs(j) - 1];
        i := i + 1;
      end;
      if Trace then
        ShowMessage(sExpr + FloatToStr(ExprResult));
    end;
  end;
  Result := ExprResult;
end;

// get formula index from its name

function TFormulaParser.Index(FName: string): Integer;
begin
  Result := FormulaNames.IndexOf(FName);
end;

// get formula name from its index

function TFormulaParser.Name(FIndex: Integer): string;
begin
  if (FIndex >= 0) and (FIndex < FormulaNames.Count) then
    Result := FormulaNames.Strings[FIndex]
  else
    Result := '';
end;

// get formula string

function TFormulaParser.Text(FName: string): string;
var
  i, j, k: Integer;
  expr: TStringList;
begin
  tslTmp.Clear;
  i := FormulaNames.IndexOf(FName);
  if i >= 0 then
  begin
    expr := TStringList(FormulaNames.Objects[i]);
    for i := 0 to expr.Count - 1 do
    begin
      Val(expr.Strings[i], j, k);
      tslTmp.Add(Expression[abs(j)-1]);
    end;
  end;
  Result := tslTmp.Text;
end;

procedure _stop(Param: PExpressionRec);
begin
  with Param^ do
  begin
    Res := 999999;
  end;
end;

procedure _goto(Param: PExpressionRec);
begin
  with Param^ do
  begin
    Res := trunc(Args[0]^);
  end;
end;

procedure _ifgoto(Param: PExpressionRec);
begin
  with Param^ do
  begin
    if Args[0]^ < 0 then
      Res := Args[1]^
    else if Args[0]^ = 0 then
      Res := Args[2]^
    else if Args[0]^ > 0 then
      Res := Args[3]^;
  end;
end;

procedure _display(Param: PExpressionRec);
begin
  with Param^ do
  begin
    sExpr := sExpr + char($0D0A) + char($0D0A) + FloatToStr(Args[0]^);
    ShowMessage(sExpr);
    Res := 0;
  end;
end;

procedure TFormulaParser.FillExpressList;
begin
  inherited;
  with WordsList do
  begin
    Add(TFunction.Create('stop', 'stop execution formula', _stop, 0));
    Add(TFunction.Create('goto', 'goto line number', _goto, 1));
    Add(TFunction.Create('ifgoto', 'conditional goto', _ifgoto, 4));
    Add(TFunction.Create('display', 'display result', _display, 1));
  end;
end;

end.

