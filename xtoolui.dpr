library xtoolui;

{$R *.res}

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Types,
  FMX.Controls,
  FMX.StdCtrls,
  WinAPI.Windows,
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  Unit1 in 'Unit1.pas' {Form1} ,
  Unit2 in 'Unit2.pas' {Form2};

const
  PLUGIN_DATABASE = 0;
  PLUGIN_CONFIG = 1;
  PLUGIN_LIBRARY = 2;

type
  PUIFuncs = ^TUIFuncs;

  TUIFuncs = record
    IsZlibLoaded: Boolean;
    IsReflateLoaded: Boolean;
    IsPreflateLoaded: Boolean;
    IsLZ4Loaded: Boolean;
    IsLZOLoaded: Boolean;
    IsZSTDLoaded: Boolean;
    IsOodleLoaded: Boolean;
    IsFLACLoaded: Boolean;
    IsBrunsliLoaded: Boolean;
    IsPackJPGLoaded: Boolean;
    IsJoJpegLoaded: Boolean;
    IsXDeltaLoaded: Boolean;
    IsLZMALoaded: Boolean;
    IsSrepAvailable: Boolean;
  end;

var
  UIInitialised: Boolean = False;

procedure XTLUI1;
begin
  Form1 := TForm1.Create(nil);
  Form2 := TForm2.Create(Form1);
end;

function XTLUI2(Funcs: PUIFuncs; var Params: TArray<String>;
  out LibType: Integer; out LibPath: String): Boolean;
var
  I, J, X: Integer;
  Expander: TExpander;
begin
  if not UIInitialised then
  begin
    UIInitialised := True;
    Form1.Edit6.Text := GetIniString('UI', 'Plugins', '',
      ChangeFileExt(GetModuleName, '.ini'));
    Form1.Edit6.OnChange := Form1.Edit6Change;
    { Form2.CheckBox3.Enabled := Funcs^.IsZlibLoaded;
      Form2.CheckBox1.Enabled := Funcs^.IsReflateLoaded;
      Form2.CheckBox2.Enabled := Funcs^.IsPreflateLoaded;
      Form2.CheckBox4.Enabled := Funcs^.IsLZ4Loaded;
      Form2.CheckBox5.Enabled := Funcs^.IsLZ4Loaded;
      Form2.CheckBox6.Enabled := Funcs^.IsLZ4Loaded;
      Form2.CheckBox7.Enabled := Funcs^.IsLZOLoaded;
      Form2.CheckBox8.Enabled := Funcs^.IsZSTDLoaded;
      Form2.CheckBox10.Enabled := Funcs^.IsOodleLoaded;
      Form2.CheckBox12.Enabled := Funcs^.IsOodleLoaded;
      Form2.CheckBox14.Enabled := Funcs^.IsOodleLoaded;
      Form2.CheckBox16.Enabled := Funcs^.IsOodleLoaded;
      Form2.CheckBox18.Enabled := Funcs^.IsOodleLoaded;
      Form2.CheckBox21.Enabled := Funcs^.IsFLACLoaded;
      Form2.RadioButton4.Enabled := Funcs^.IsFLACLoaded;
      Form2.CheckBox26.Enabled := Funcs^.IsBrunsliLoaded or
      Funcs^.IsPackJPGLoaded or Funcs^.IsJoJpegLoaded;
      Form2.RadioButton1.Enabled := Funcs^.IsBrunsliLoaded;
      Form2.RadioButton2.Enabled := Funcs^.IsPackJPGLoaded;
      Form2.RadioButton3.Enabled := Funcs^.IsJoJpegLoaded;
      Form1.GroupBox5.Enabled := Funcs^.IsLZMALoaded; }
    Form1.SpinBox4.Enabled := Funcs^.IsSrepAvailable;
    for I := Low(Methods) to High(Methods) do
    begin
      case Methods[I].FType of
        PLUGIN_DATABASE:
          Form2.ListBox2.Items.Add(Methods[I].FName);
        PLUGIN_CONFIG:
          begin
            J := Length(CfgCtrls);
            Insert(TConfigCtrl.Create(Form2, Form2.VertScrollBox3), CfgCtrls,
              Length(CfgCtrls));
            CfgCtrls[J].SetText(Methods[I].FName);
            for X := Low(Methods[I].Codecs) to High(Methods[I].Codecs) do
              CfgCtrls[J].AddCodec(Methods[I].Codecs[X]);
          end;
        PLUGIN_LIBRARY:
          begin
            J := Length(LibCtrls);
            Insert(TLibraryCtrl.Create(Form2, Form2.VertScrollBox3), LibCtrls,
              Length(LibCtrls));
            LibCtrls[J].SetText(Methods[I].FName);
            for X := Low(Methods[I].Codecs) to High(Methods[I].Codecs) do
              LibCtrls[J].AddCodec(Methods[I].Codecs[X]);
          end;
      end;
    end;
    for I := 0 to Form2.ComponentCount - 1 do
    begin
      if Form2.Components[I] is TExpander then
        TExpander(Form2.Components[I]).IsExpanded := False;
    end;
    if Form2.ListBox2.Items.Count = 0 then
      Form2.Expander10.Visible := False;
  end;
  Result := Form1.ShowModal = mrOk;
  LibType := Form1.ComboBox4.ItemIndex;
  LibPath := Form1.Edit5.Text;
  if Result then
  begin
    SetLength(Params, Length(CmdStr));
    for I := Low(CmdStr) to High(CmdStr) do
      Params[I] := CmdStr[I];
  end;
end;

procedure XTLAddPlugin(S: String; I: Integer);
var
  J: Integer;
begin
  J := Length(Methods);
  SetLength(Methods, Succ(J));
  Methods[J].FName := S;
  Methods[J].FType := I;
  SetLength(Methods[J].Codecs, 0);
end;

procedure XTLAddCodec(S: String);
var
  J: Integer;
begin
  J := Pred(Length(Methods));
  Insert(S, Methods[J].Codecs, Length(Methods[J].Codecs));
end;

exports XTLUI1, XTLUI2, XTLAddPlugin, XTLAddCodec;

begin

end.
