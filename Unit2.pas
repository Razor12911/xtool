unit Unit2;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, System.Math, System.StrUtils,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.TabControl,
  FMX.StdCtrls, FMX.Edit, FMX.EditBox, FMX.SpinBox, FMX.Controls.Presentation,
  FMX.Layouts, FMX.ListBox;

type
  TForm2 = class(TForm)
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    Expander1: TExpander;
    Layout6: TLayout;
    CheckBox3: TCheckBox;
    Layout1: TLayout;
    Label4: TLabel;
    SpinBox4: TSpinBox;
    Layout2: TLayout;
    CheckBox1: TCheckBox;
    Layout3: TLayout;
    Label1: TLabel;
    SpinBox1: TSpinBox;
    Layout4: TLayout;
    CheckBox2: TCheckBox;
    Expander2: TExpander;
    Layout5: TLayout;
    CheckBox4: TCheckBox;
    Layout7: TLayout;
    Label2: TLabel;
    SpinBox2: TSpinBox;
    Layout8: TLayout;
    CheckBox5: TCheckBox;
    Layout9: TLayout;
    SpinBox3: TSpinBox;
    Layout10: TLayout;
    CheckBox6: TCheckBox;
    VertScrollBox1: TVertScrollBox;
    Layout11: TLayout;
    SpinBox5: TSpinBox;
    Layout12: TLayout;
    Label6: TLabel;
    SpinBox6: TSpinBox;
    Expander3: TExpander;
    Layout13: TLayout;
    CheckBox7: TCheckBox;
    Layout14: TLayout;
    Label7: TLabel;
    Layout15: TLayout;
    SpinBox8: TSpinBox;
    ComboBox1: TComboBox;
    Expander4: TExpander;
    Layout16: TLayout;
    CheckBox8: TCheckBox;
    Layout18: TLayout;
    SpinBox7: TSpinBox;
    CheckBox9: TCheckBox;
    Expander5: TExpander;
    Layout17: TLayout;
    CheckBox10: TCheckBox;
    Layout19: TLayout;
    SpinBox9: TSpinBox;
    CheckBox11: TCheckBox;
    Layout20: TLayout;
    CheckBox12: TCheckBox;
    Layout21: TLayout;
    SpinBox10: TSpinBox;
    CheckBox13: TCheckBox;
    Layout22: TLayout;
    CheckBox14: TCheckBox;
    Layout23: TLayout;
    SpinBox11: TSpinBox;
    CheckBox15: TCheckBox;
    Layout24: TLayout;
    CheckBox16: TCheckBox;
    Layout25: TLayout;
    SpinBox12: TSpinBox;
    CheckBox17: TCheckBox;
    Layout26: TLayout;
    CheckBox18: TCheckBox;
    Layout27: TLayout;
    SpinBox13: TSpinBox;
    CheckBox19: TCheckBox;
    Layout28: TLayout;
    CheckBox20: TCheckBox;
    Expander6: TExpander;
    Layout29: TLayout;
    CheckBox21: TCheckBox;
    Layout30: TLayout;
    SpinBox14: TSpinBox;
    CheckBox23: TCheckBox;
    CheckBox24: TCheckBox;
    CheckBox25: TCheckBox;
    Layout31: TLayout;
    Label3: TLabel;
    SpinBox15: TSpinBox;
    Layout32: TLayout;
    CheckBox26: TCheckBox;
    Layout33: TLayout;
    RadioButton1: TRadioButton;
    Layout34: TLayout;
    RadioButton2: TRadioButton;
    Layout35: TLayout;
    RadioButton3: TRadioButton;
    Layout36: TLayout;
    Label5: TLabel;
    SpinBox16: TSpinBox;
    Layout37: TLayout;
    Button1: TButton;
    RadioButton4: TRadioButton;
    VertScrollBox3: TVertScrollBox;
    Expander10: TExpander;
    ListBox2: TListBox;
    procedure FormShow(Sender: TObject);
    procedure CheckBox3Change(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure CheckBox4Change(Sender: TObject);
    procedure CheckBox5Change(Sender: TObject);
    procedure CheckBox6Change(Sender: TObject);
    procedure CheckBox7Change(Sender: TObject);
    procedure CheckBox10Change(Sender: TObject);
    procedure CheckBox8Change(Sender: TObject);
    procedure CheckBox12Change(Sender: TObject);
    procedure CheckBox14Change(Sender: TObject);
    procedure CheckBox16Change(Sender: TObject);
    procedure CheckBox18Change(Sender: TObject);
    procedure CheckBox21Change(Sender: TObject);
    procedure CheckBox26Change(Sender: TObject);
    procedure CheckBox24Change(Sender: TObject);
    procedure CheckBox23Change(Sender: TObject);
    procedure CheckBox25Change(Sender: TObject);
    procedure CheckBox9Change(Sender: TObject);
    procedure CheckBox11Change(Sender: TObject);
    procedure CheckBox13Change(Sender: TObject);
    procedure CheckBox15Change(Sender: TObject);
    procedure CheckBox17Change(Sender: TObject);
    procedure CheckBox19Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure RadioButton4Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TMethod = record
    FName: String;
    FType: Integer;
    Codecs: TArray<String>;
  end;

  TConfigCtrl = class(TObject)
  private
    FOwner, FParent: TFmxObject;
    FExpander: TExpander;
    FListBox: TListBox;
  public
    constructor Create(AOwner, AParent: TFmxObject);
    destructor Destroy; override;
    procedure SetText(AText: String);
    procedure AddCodec(ACodec: String);
    function GetCodec(out Codec: String): Boolean;
  end;

  TLibraryCtrl = class(TObject)
  private
    FOwner, FParent: TFmxObject;
    FExpander: TExpander;
    FCheckBox: TArray<TCheckBox>;
    FLayout1, FLayout2: TArray<TLayout>;
    FLabel: TArray<TLabel>;
    FEdit: TArray<TEdit>;
    procedure OnCheckBoxChange(Sender: TObject);
  public
    constructor Create(AOwner, AParent: TFmxObject);
    destructor Destroy; override;
    procedure SetText(AText: String);
    procedure AddCodec(ACodec: String);
    function GetCodec(out Codec: String): Boolean;
  end;

var
  Form2: TForm2;
  Methods: TArray<TMethod>;
  CfgCtrls: TArray<TConfigCtrl>;
  LibCtrls: TArray<TLibraryCtrl>;

implementation

{$R *.fmx}

uses Unit1;

procedure AddMethod(const SubStr: String; var ResStr: String;
  ParamStr: TArray<String>; ParamBool: TArray<Boolean>);
var
  I: Integer;
  S: String;
begin
  if ResStr = '' then
    ResStr := SubStr
  else
    ResStr := ResStr + '+' + SubStr;
  S := '';
  for I := Low(ParamStr) to High(ParamStr) do
    if ParamBool[I] then
      S := S + ':' + ParamStr[I];
  ResStr := ResStr + S;
end;

constructor TConfigCtrl.Create(AOwner, AParent: TFmxObject);
begin
  inherited Create;
  FOwner := AOwner;
  FParent := AParent;
  FExpander := TExpander.Create(FOwner);
  FExpander.Parent := FParent;
  FExpander.Height := 130;
  FExpander.Margins.Top := 8;
  FExpander.Margins.Left := 8;
  FExpander.Margins.Right := 8;
  FExpander.Align := TAlignLayout.Top;
  FListBox := TListBox.Create(FExpander);
  FListBox.Parent := FExpander;
  FListBox.Margins.Bottom := 8;
  FListBox.Margins.Left := 8;
  FListBox.Margins.Right := 8;
  FListBox.Align := TAlignLayout.Client;
  FListBox.ShowCheckboxes := True;
end;

destructor TConfigCtrl.Destroy;
begin
  FListBox.Free;
  FExpander.Free;
  inherited Destroy;
end;

procedure TConfigCtrl.SetText(AText: String);
begin
  FExpander.Text := AText;
end;

procedure TConfigCtrl.AddCodec(ACodec: String);
begin
  FListBox.Items.Add(ACodec);
end;

function TConfigCtrl.GetCodec(out Codec: String): Boolean;
var
  I: Integer;
  S: String;
begin
  Result := False;
  S := '';
  for I := 0 to FListBox.Items.Count - 1 do
    if FListBox.ItemByIndex(I).IsChecked then
    begin
      S := S + ':' + FListBox.Items[I];
      Result := True;
    end;
  if Result then
    Codec := FExpander.Text + S;
end;

constructor TLibraryCtrl.Create(AOwner, AParent: TFmxObject);
begin
  inherited Create;
  FOwner := AOwner;
  FParent := AParent;
  FExpander := TExpander.Create(FOwner);
  FExpander.Parent := FParent;
  FExpander.Height := 25;
  FExpander.Margins.Top := 8;
  FExpander.Margins.Left := 8;
  FExpander.Margins.Right := 8;
  FExpander.Align := TAlignLayout.Top;
end;

destructor TLibraryCtrl.Destroy;
begin
  FExpander.Free;
  inherited Destroy;
end;

procedure TLibraryCtrl.OnCheckBoxChange(Sender: TObject);
var
  I: Integer;
begin
  for I := Low(FCheckBox) to High(FCheckBox) do
    if Sender = FCheckBox[I] then
      FEdit[I].Enabled := FCheckBox[I].IsChecked;
end;

procedure TLibraryCtrl.SetText(AText: String);
begin
  FExpander.Text := AText;
end;

procedure TLibraryCtrl.AddCodec(ACodec: String);
var
  I: Integer;
begin
  I := Length(FLayout1);
  Insert(TLayout.Create(FExpander), FLayout1, Length(FLayout1));
  FLayout1[I].Parent := FExpander;
  FLayout1[I].Height := 64;
  FLayout1[I].Align := TAlignLayout.Top;
  Insert(TCheckBox.Create(FLayout1[I]), FCheckBox, Length(FCheckBox));
  FCheckBox[I].Parent := FLayout1[I];
  FCheckBox[I].Height := 20;
  FCheckBox[I].Margins.Top := 8;
  FCheckBox[I].Margins.Left := 8;
  FCheckBox[I].Margins.Right := 8;
  FCheckBox[I].Align := TAlignLayout.Top;
  FCheckBox[I].Text := ACodec;
  FCheckBox[I].OnChange := OnCheckBoxChange;
  Insert(TLayout.Create(FLayout1[I]), FLayout2, Length(FLayout2));
  FLayout2[I].Parent := FLayout1[I];
  FLayout2[I].Align := TAlignLayout.Client;
  Insert(TLabel.Create(FLayout2[I]), FLabel, Length(FLabel));
  FLabel[I].Parent := FLayout2[I];
  FLabel[I].Width := 40;
  FLabel[I].Margins.Top := 8;
  FLabel[I].Margins.Bottom := 8;
  FLabel[I].Margins.Left := 8;
  FLabel[I].Align := TAlignLayout.Left;
  FLabel[I].Text := 'Param';
  Insert(TEdit.Create(FLayout2[I]), FEdit, Length(FEdit));
  FEdit[I].Parent := FLayout2[I];
  FEdit[I].Enabled := False;
  FEdit[I].Margins.Top := 8;
  FEdit[I].Margins.Bottom := 8;
  FEdit[I].Margins.Left := 8;
  FEdit[I].Margins.Right := 8;
  FEdit[I].Align := TAlignLayout.Client;
  FExpander.Height := FExpander.Height + 64;
end;

function TLibraryCtrl.GetCodec(out Codec: String): Boolean;
var
  I: Integer;
  S: String;
begin
  Result := False;
  S := '';
  for I := Low(FCheckBox) to High(FCheckBox) do
    if FCheckBox[I].IsChecked then
    begin
      AddMethod(FCheckBox[I].Text, S, [FEdit[I].Text], [FEdit[I].Text <> '']);
      Result := True;
    end;
  if Result then
    Codec := S;
end;

procedure TForm2.Button1Click(Sender: TObject);
var
  I: Integer;
  S, Res: String;
begin
  Res := '';
  if CheckBox3.IsChecked then
    AddMethod('zlib', Res, ['w' + SpinBox4.Text], [True]);
  if CheckBox1.IsChecked then
    AddMethod('reflate', Res, ['l' + SpinBox1.Text], [True]);
  if CheckBox2.IsChecked then
    AddMethod('preflate', Res, [], []);
  if CheckBox20.IsChecked then
    AddMethod('png', Res, [], []);
  if CheckBox4.IsChecked then
    AddMethod('lz4', Res, ['a' + SpinBox2.Text], [True]);
  if CheckBox5.IsChecked then
    AddMethod('lz4hc', Res, ['l' + SpinBox3.Text], [CheckBox24.IsChecked]);
  if CheckBox6.IsChecked then
    AddMethod('lz4f', Res, ['l' + SpinBox5.Text, 'd' + SpinBox6.Text],
      [CheckBox23.IsChecked, True]);
  if CheckBox7.IsChecked then
    AddMethod('lzo1x', Res, ['l' + SpinBox8.Text], [CheckBox25.IsChecked]);
  if CheckBox8.IsChecked then
    AddMethod('zstd', Res, ['l' + SpinBox7.Text], [CheckBox9.IsChecked]);
  if CheckBox10.IsChecked then
    AddMethod('kraken', Res, ['l' + SpinBox9.Text, 'n' + SpinBox15.Text,
      't' + SpinBox16.Text], [CheckBox11.IsChecked, True, True]);
  if CheckBox12.IsChecked then
    AddMethod('mermaid', Res, ['l' + SpinBox10.Text, 'n' + SpinBox15.Text,
      't' + SpinBox16.Text], [CheckBox13.IsChecked, True, True]);
  if CheckBox14.IsChecked then
    AddMethod('selkie', Res, ['l' + SpinBox11.Text, 'n' + SpinBox15.Text,
      't' + SpinBox16.Text], [CheckBox15.IsChecked, True, True]);
  if CheckBox16.IsChecked then
    AddMethod('hydra', Res, ['l' + SpinBox12.Text, 'n' + SpinBox15.Text,
      't' + SpinBox16.Text], [CheckBox17.IsChecked, True, True]);
  if CheckBox18.IsChecked then
    AddMethod('leviathan', Res, ['l' + SpinBox13.Text, 'n' + SpinBox15.Text,
      't' + SpinBox16.Text], [CheckBox19.IsChecked, True, True]);
  if CheckBox21.IsChecked then
    AddMethod('flac', Res, ['l' + SpinBox14.Text], [RadioButton4.IsChecked]);
  if CheckBox26.IsChecked and RadioButton1.IsChecked then
    AddMethod('brunsli', Res, [], []);
  if CheckBox26.IsChecked and RadioButton2.IsChecked then
    AddMethod('packjpg', Res, [], []);
  if CheckBox26.IsChecked and RadioButton3.IsChecked then
    AddMethod('jojpeg', Res, [], []);
  for I := 0 to ListBox2.Items.Count - 1 do
    if ListBox2.ItemByIndex(I).IsChecked then
      AddMethod(ListBox2.Items[I], Res, [], []);
  for I := Low(CfgCtrls) to High(CfgCtrls) do
    if CfgCtrls[I].GetCodec(S) then
      AddMethod(S, Res, [], []);
  for I := Low(LibCtrls) to High(LibCtrls) do
    if LibCtrls[I].GetCodec(S) then
      AddMethod(S, Res, [], []);
  Form1.Edit2.Text := Res;
end;

procedure TForm2.CheckBox10Change(Sender: TObject);
begin
  Layout19.Enabled := CheckBox10.IsChecked;
end;

procedure TForm2.CheckBox11Change(Sender: TObject);
begin
  SpinBox9.Enabled := CheckBox11.IsChecked;
end;

procedure TForm2.CheckBox12Change(Sender: TObject);
begin
  Layout21.Enabled := CheckBox12.IsChecked;
end;

procedure TForm2.CheckBox13Change(Sender: TObject);
begin
  SpinBox10.Enabled := CheckBox13.IsChecked;
end;

procedure TForm2.CheckBox14Change(Sender: TObject);
begin
  Layout23.Enabled := CheckBox14.IsChecked;
end;

procedure TForm2.CheckBox15Change(Sender: TObject);
begin
  SpinBox11.Enabled := CheckBox15.IsChecked;
end;

procedure TForm2.CheckBox16Change(Sender: TObject);
begin
  Layout25.Enabled := CheckBox16.IsChecked;
end;

procedure TForm2.CheckBox17Change(Sender: TObject);
begin
  SpinBox12.Enabled := CheckBox17.IsChecked;
end;

procedure TForm2.CheckBox18Change(Sender: TObject);
begin
  Layout27.Enabled := CheckBox18.IsChecked;
end;

procedure TForm2.CheckBox19Change(Sender: TObject);
begin
  SpinBox13.Enabled := CheckBox19.IsChecked;
end;

procedure TForm2.CheckBox1Change(Sender: TObject);
begin
  Layout3.Enabled := CheckBox1.IsChecked;
end;

procedure TForm2.CheckBox21Change(Sender: TObject);
begin
  Layout30.Enabled := CheckBox21.IsChecked;
end;

procedure TForm2.CheckBox23Change(Sender: TObject);
begin
  SpinBox5.Enabled := CheckBox23.IsChecked;
end;

procedure TForm2.CheckBox24Change(Sender: TObject);
begin
  SpinBox3.Enabled := CheckBox24.IsChecked;
end;

procedure TForm2.CheckBox25Change(Sender: TObject);
begin
  SpinBox8.Enabled := CheckBox25.IsChecked;
end;

procedure TForm2.CheckBox26Change(Sender: TObject);
begin
  Layout33.Enabled := CheckBox26.IsChecked;
  Layout34.Enabled := CheckBox26.IsChecked;
  Layout35.Enabled := CheckBox26.IsChecked;
end;

procedure TForm2.CheckBox3Change(Sender: TObject);
begin
  Layout1.Enabled := CheckBox3.IsChecked;
end;

procedure TForm2.CheckBox4Change(Sender: TObject);
begin
  Layout7.Enabled := CheckBox4.IsChecked;
end;

procedure TForm2.CheckBox5Change(Sender: TObject);
begin
  Layout9.Enabled := CheckBox5.IsChecked;
end;

procedure TForm2.CheckBox6Change(Sender: TObject);
begin
  Layout11.Enabled := CheckBox6.IsChecked;
  Layout12.Enabled := CheckBox6.IsChecked;
end;

procedure TForm2.CheckBox7Change(Sender: TObject);
begin
  Layout14.Enabled := CheckBox7.IsChecked;
  Layout15.Enabled := CheckBox7.IsChecked;
end;

procedure TForm2.CheckBox8Change(Sender: TObject);
begin
  Layout18.Enabled := CheckBox8.IsChecked;
end;

procedure TForm2.CheckBox9Change(Sender: TObject);
begin
  SpinBox7.Enabled := CheckBox9.IsChecked;
end;

procedure TForm2.FormShow(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to ComponentCount - 1 do
  begin
    if Components[I] is TSpinBox then
      TSpinBox(Components[I]).Cursor := crDefault;
    if Components[I] is TCheckBox then
      if Assigned(TCheckBox(Components[I]).OnChange) then
        TCheckBox(Components[I]).OnChange(nil);
    if Components[I] is TRadioButton then
      if Assigned(TRadioButton(Components[I]).OnChange) then
        TRadioButton(Components[I]).OnChange(nil);
  end;
end;

procedure TForm2.RadioButton4Change(Sender: TObject);
begin
  SpinBox14.Enabled := RadioButton4.IsChecked;
end;

end.
