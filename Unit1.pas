unit Unit1;

interface

uses
  WinAPI.Windows,
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, System.Math, System.StrUtils, System.IniFiles,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.TabControl,
  FMX.Layouts, FMX.ListBox, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Edit,
  FMX.EditBox, FMX.SpinBox, FMX.Menus, FMX.ComboEdit, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo;

type
  TForm1 = class(TForm)
    StyleBook1: TStyleBook;
    SaveDialog1: TSaveDialog;
    OpenDialog1: TOpenDialog;
    GroupBox1: TGroupBox;
    ComboBox2: TComboBox;
    Edit1: TEdit;
    SearchEditButton1: TSearchEditButton;
    Layout1: TLayout;
    GroupBox2: TGroupBox;
    Layout3: TLayout;
    VertScrollBox1: TVertScrollBox;
    GroupBox3: TGroupBox;
    Layout7: TLayout;
    Edit3: TEdit;
    SearchEditButton3: TSearchEditButton;
    ComboBox3: TComboBox;
    Label1: TLabel;
    SpinBox1: TSpinBox;
    Label2: TLabel;
    SpinBox2: TSpinBox;
    Layout2: TLayout;
    GroupBox4: TGroupBox;
    Layout6: TLayout;
    GroupBox5: TGroupBox;
    Layout8: TLayout;
    Label7: TLabel;
    SpinBox7: TSpinBox;
    Label4: TLabel;
    SpinBox4: TSpinBox;
    CheckBox4: TCheckBox;
    GroupBox6: TGroupBox;
    Layout5: TLayout;
    Edit2: TEdit;
    Button2: TButton;
    CheckBox2: TCheckBox;
    GroupBox8: TGroupBox;
    Layout10: TLayout;
    Edit5: TEdit;
    SearchEditButton4: TSearchEditButton;
    ComboBox4: TComboBox;
    ComboBox5: TComboBox;
    ComboEdit1: TComboEdit;
    Label6: TLabel;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    TabItem3: TTabItem;
    GroupBox9: TGroupBox;
    Layout11: TLayout;
    Edit7: TEdit;
    SearchEditButton6: TSearchEditButton;
    ComboBox6: TComboBox;
    GroupBox10: TGroupBox;
    Layout12: TLayout;
    Edit8: TEdit;
    SearchEditButton7: TSearchEditButton;
    ComboBox7: TComboBox;
    GroupBox11: TGroupBox;
    Layout13: TLayout;
    Edit9: TEdit;
    SearchEditButton8: TSearchEditButton;
    ComboBox8: TComboBox;
    GroupBox12: TGroupBox;
    Layout14: TLayout;
    Label9: TLabel;
    SpinBox6: TSpinBox;
    Label10: TLabel;
    SpinBox8: TSpinBox;
    VertScrollBox2: TVertScrollBox;
    TabControl2: TTabControl;
    TabItem4: TTabItem;
    TabItem5: TTabItem;
    TabItem6: TTabItem;
    TabItem7: TTabItem;
    TabItem8: TTabItem;
    GroupBox13: TGroupBox;
    Layout15: TLayout;
    GroupBox14: TGroupBox;
    Layout16: TLayout;
    Edit11: TEdit;
    SearchEditButton10: TSearchEditButton;
    Memo1: TMemo;
    Layout17: TLayout;
    Button3: TButton;
    Button4: TButton;
    OpenDialog2: TOpenDialog;
    Label11: TLabel;
    Layout4: TLayout;
    Button1: TButton;
    Label5: TLabel;
    Edit6: TEdit;
    SearchEditButton5: TSearchEditButton;
    Layout18: TLayout;
    Button5: TButton;
    Layout19: TLayout;
    Button6: TButton;
    VertScrollBox3: TVertScrollBox;
    TabItem9: TTabItem;
    VertScrollBox4: TVertScrollBox;
    GroupBox15: TGroupBox;
    Layout20: TLayout;
    Edit10: TEdit;
    SearchEditButton9: TSearchEditButton;
    ComboBox9: TComboBox;
    GroupBox16: TGroupBox;
    Layout21: TLayout;
    Edit12: TEdit;
    SearchEditButton11: TSearchEditButton;
    ComboBox10: TComboBox;
    GroupBox17: TGroupBox;
    Layout22: TLayout;
    Label12: TLabel;
    SpinBox9: TSpinBox;
    Label13: TLabel;
    SpinBox10: TSpinBox;
    GroupBox18: TGroupBox;
    Layout23: TLayout;
    Edit13: TEdit;
    SearchEditButton12: TSearchEditButton;
    ComboBox11: TComboBox;
    Layout24: TLayout;
    Button7: TButton;
    VertScrollBox5: TVertScrollBox;
    GroupBox19: TGroupBox;
    Layout25: TLayout;
    Edit14: TEdit;
    SearchEditButton13: TSearchEditButton;
    ComboBox12: TComboBox;
    GroupBox20: TGroupBox;
    Layout26: TLayout;
    Edit15: TEdit;
    SearchEditButton14: TSearchEditButton;
    ComboBox13: TComboBox;
    GroupBox21: TGroupBox;
    Layout27: TLayout;
    Label14: TLabel;
    SpinBox11: TSpinBox;
    Label15: TLabel;
    SpinBox12: TSpinBox;
    GroupBox22: TGroupBox;
    Layout28: TLayout;
    Edit16: TEdit;
    SearchEditButton15: TSearchEditButton;
    ComboBox14: TComboBox;
    Layout29: TLayout;
    Button8: TButton;
    GroupBox23: TGroupBox;
    Layout30: TLayout;
    Edit17: TEdit;
    SearchEditButton16: TSearchEditButton;
    ComboBox15: TComboBox;
    VertScrollBox6: TVertScrollBox;
    GroupBox24: TGroupBox;
    Layout31: TLayout;
    Edit18: TEdit;
    SearchEditButton17: TSearchEditButton;
    ComboBox16: TComboBox;
    GroupBox25: TGroupBox;
    Layout32: TLayout;
    Edit19: TEdit;
    SearchEditButton18: TSearchEditButton;
    GroupBox26: TGroupBox;
    Layout33: TLayout;
    Label17: TLabel;
    SpinBox14: TSpinBox;
    GroupBox27: TGroupBox;
    Layout34: TLayout;
    Edit20: TEdit;
    SearchEditButton19: TSearchEditButton;
    Layout35: TLayout;
    Button9: TButton;
    VertScrollBox7: TVertScrollBox;
    GroupBox28: TGroupBox;
    Layout36: TLayout;
    Edit21: TEdit;
    SearchEditButton20: TSearchEditButton;
    ComboBox19: TComboBox;
    GroupBox29: TGroupBox;
    Layout37: TLayout;
    Edit22: TEdit;
    SearchEditButton21: TSearchEditButton;
    GroupBox30: TGroupBox;
    Layout38: TLayout;
    Label18: TLabel;
    SpinBox15: TSpinBox;
    Label19: TLabel;
    SpinBox16: TSpinBox;
    GroupBox31: TGroupBox;
    Layout39: TLayout;
    Edit23: TEdit;
    SearchEditButton22: TSearchEditButton;
    ComboBox21: TComboBox;
    GroupBox32: TGroupBox;
    Layout40: TLayout;
    Edit24: TEdit;
    Layout41: TLayout;
    Button10: TButton;
    SaveDialog2: TSaveDialog;
    VertScrollBox8: TVertScrollBox;
    GroupBox33: TGroupBox;
    Layout42: TLayout;
    Edit25: TEdit;
    SearchEditButton23: TSearchEditButton;
    GroupBox34: TGroupBox;
    Layout43: TLayout;
    Edit26: TEdit;
    SearchEditButton24: TSearchEditButton;
    GroupBox35: TGroupBox;
    Layout44: TLayout;
    Label16: TLabel;
    SpinBox13: TSpinBox;
    GroupBox36: TGroupBox;
    Layout45: TLayout;
    Edit27: TEdit;
    SearchEditButton25: TSearchEditButton;
    Layout46: TLayout;
    Button11: TButton;
    ComboBox17: TComboBox;
    ComboBox18: TComboBox;
    CheckBox7: TCheckBox;
    Label20: TLabel;
    Edit28: TEdit;
    SearchEditButton26: TSearchEditButton;
    Label21: TLabel;
    ComboEdit2: TComboEdit;
    CheckBox1: TCheckBox;
    Label3: TLabel;
    SpinBox3: TSpinBox;
    CheckBox6: TCheckBox;
    Label8: TLabel;
    ComboEdit3: TComboEdit;
    Label22: TLabel;
    ComboEdit4: TComboEdit;
    procedure FormShow(Sender: TObject);
    procedure SearchEditButton1Click(Sender: TObject);
    procedure SearchEditButton3Click(Sender: TObject);
    procedure ComboBox3Change(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ComboBox4Change(Sender: TObject);
    procedure SearchEditButton4Click(Sender: TObject);
    procedure Edit6Change(Sender: TObject);
    procedure SearchEditButton5Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ComboBox5Change(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure SearchEditButton10Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure ComboBox7Change(Sender: TObject);
    procedure ComboBox6Change(Sender: TObject);
    procedure ComboBox8Change(Sender: TObject);
    procedure SearchEditButton7Click(Sender: TObject);
    procedure SearchEditButton6Click(Sender: TObject);
    procedure SearchEditButton8Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure ComboBox9Change(Sender: TObject);
    procedure ComboBox11Change(Sender: TObject);
    procedure ComboBox10Change(Sender: TObject);
    procedure SearchEditButton21Click(Sender: TObject);
    procedure ComboBox19Change(Sender: TObject);
    procedure ComboBox21Change(Sender: TObject);
    procedure ComboBox12Change(Sender: TObject);
    procedure ComboBox15Change(Sender: TObject);
    procedure ComboBox14Change(Sender: TObject);
    procedure ComboBox13Change(Sender: TObject);
    procedure SearchEditButton20Click(Sender: TObject);
    procedure SearchEditButton22Click(Sender: TObject);
    procedure SearchEditButton9Click(Sender: TObject);
    procedure SearchEditButton12Click(Sender: TObject);
    procedure SearchEditButton11Click(Sender: TObject);
    procedure SearchEditButton13Click(Sender: TObject);
    procedure SearchEditButton16Click(Sender: TObject);
    procedure SearchEditButton15Click(Sender: TObject);
    procedure SearchEditButton14Click(Sender: TObject);
    procedure ComboBox16Change(Sender: TObject);
    procedure SearchEditButton17Click(Sender: TObject);
    procedure SearchEditButton19Click(Sender: TObject);
    procedure SearchEditButton18Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure SearchEditButton23Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure ComboBox18Change(Sender: TObject);
    procedure ComboBox17Change(Sender: TObject);
    procedure SearchEditButton25Click(Sender: TObject);
    procedure SearchEditButton24Click(Sender: TObject);
    procedure CheckBox4Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  CmdStr: TArray<String>;
  Init: Boolean = False;
  DecodeMode: Integer = -1;

function GetModuleName: string;
function GetIniString(Section, Key, Default, FileName: string): string;
procedure SetIniString(Section, Key, Value, FileName: string);

implementation

{$R *.fmx}

uses
  Unit2;

function GetModuleName: string;
var
  szFileName: array [0 .. MAX_PATH] of char;
begin
  FillChar(szFileName, sizeof(szFileName), #0);
  GetModuleFileName(hInstance, szFileName, MAX_PATH);
  Result := szFileName;
end;

function GetIniString(Section, Key, Default, FileName: string): string;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(FileName);
  with Ini do
    try
      Result := Ini.ReadString(Section, Key, Default);
    finally
      Free;
    end;
end;

procedure SetIniString(Section, Key, Value, FileName: string);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(FileName);
  with Ini do
    try
      Ini.WriteString(Section, Key, Value);
    finally
      Free;
    end;
end;

procedure TForm1.Button10Click(Sender: TObject);
begin
  SetLength(CmdStr, 0);
  Insert(ParamStr(0), CmdStr, Length(CmdStr));
  Insert('generate', CmdStr, Length(CmdStr));
  Insert('-c' + SpinBox15.Text + 'mb', CmdStr, Length(CmdStr));
  Insert('-t' + SpinBox16.Text, CmdStr, Length(CmdStr));
  Insert('-m' + Edit24.Text, CmdStr, Length(CmdStr));
  Insert(Edit21.Text, CmdStr, Length(CmdStr));
  Insert(Edit23.Text, CmdStr, Length(CmdStr));
  Insert(Edit22.Text, CmdStr, Length(CmdStr));
end;

procedure TForm1.Button11Click(Sender: TObject);
begin
  SetLength(CmdStr, 0);
  Insert(ParamStr(0), CmdStr, Length(CmdStr));
  Insert('decode', CmdStr, Length(CmdStr));
  Insert('-t' + SpinBox13.Text, CmdStr, Length(CmdStr));
  if CheckBox7.IsChecked then
    Insert('-v', CmdStr, Length(CmdStr));
  Insert('-g' + ReplaceText(ReplaceText(ComboEdit4.Text, '%', 'p'), ' ', ''),
    CmdStr, Length(CmdStr));
  Insert('-bd' + Edit6.Text, CmdStr, Length(CmdStr));
  Insert(Edit25.Text, CmdStr, Length(CmdStr));
  case DecodeMode of
    0:
      Insert(Edit26.Text, CmdStr, Length(CmdStr));
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  S: String;
begin
  SetLength(CmdStr, 0);
  Insert(ParamStr(0), CmdStr, Length(CmdStr));
  Insert('precomp', CmdStr, Length(CmdStr));
  Insert('-c' + SpinBox1.Text + 'mb', CmdStr, Length(CmdStr));
  Insert('-t' + SpinBox2.Text, CmdStr, Length(CmdStr));
  if CheckBox1.IsChecked then
    Insert('-lm', CmdStr, Length(CmdStr));
  Insert('-d' + SpinBox3.Text, CmdStr, Length(CmdStr));
  if CheckBox2.IsChecked then
    Insert('-s', CmdStr, Length(CmdStr));
  if CheckBox6.IsChecked then
    Insert('-v', CmdStr, Length(CmdStr));
  Insert('-m' + Edit2.Text, CmdStr, Length(CmdStr));
  if ComboBox5.ItemIndex > 0 then
    Insert('-dd' + IfThen(SpinBox4.Enabled, SpinBox4.Text, ''), CmdStr,
      Length(CmdStr));
  if ComboEdit1.Enabled then
    Insert('-SI' + ReplaceText(ComboEdit1.Text, ' ', ''), CmdStr,
      Length(CmdStr));
  if CheckBox4.IsChecked then
  begin
    S := '';
    if not ComboEdit2.Text.StartsWith('Auto', False) then
      S := S + ':d' + ReplaceText(ComboEdit2.Text, ' ', '');
    // S := S + ':o8';
    Insert('-l' + SpinBox7.Text + IfThen(SameText(ComboEdit2.Text, 'Auto'), '',
      'x') + S, CmdStr, Length(CmdStr));
  end;
  Insert('-g' + ReplaceText(ReplaceText(ComboEdit3.Text, '%', 'p'), ' ', ''),
    CmdStr, Length(CmdStr));
  Insert('-bd' + Edit6.Text, CmdStr, Length(CmdStr));
  Insert(Edit1.Text, CmdStr, Length(CmdStr));
  if ComboBox3.ItemIndex = 1 then
    Insert(Edit3.Text, CmdStr, Length(CmdStr));
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Form2.Left := Form1.Left + (Form1.Width div 2) - (Form2.Width div 2);
  Form2.Top := Form1.Top + (Form1.Height div 2) - (Form2.Height div 2);
  Form2.ShowModal;
  Form2.Close;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  OpenDialog2.FileName := '';
  if OpenDialog2.Execute then
    Memo1.Lines.AddStrings(OpenDialog2.Files);
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  Dir: string;
begin
  if SelectDirectory('', '', Dir) then
    Memo1.Lines.Add(Dir);
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  SetLength(CmdStr, 0);
  Insert(ParamStr(0), CmdStr, Length(CmdStr));
  Insert('find', CmdStr, Length(CmdStr));
  Insert('-c' + SpinBox6.Text + 'mb', CmdStr, Length(CmdStr));
  Insert('-t' + SpinBox8.Text, CmdStr, Length(CmdStr));
  Insert(Edit8.Text, CmdStr, Length(CmdStr));
  Insert(Edit7.Text, CmdStr, Length(CmdStr));
  if ComboBox8.ItemIndex = 1 then
    Insert(Edit9.Text, CmdStr, Length(CmdStr));
end;

procedure TForm1.Button6Click(Sender: TObject);
var
  I: Integer;
begin
  SetLength(CmdStr, 0);
  Insert(ParamStr(0), CmdStr, Length(CmdStr));
  Insert('archive', CmdStr, Length(CmdStr));
  for I := 0 to Memo1.Lines.Count - 1 do
    Insert(Memo1.Lines[I], CmdStr, Length(CmdStr));
  Insert(Edit11.Text, CmdStr, Length(CmdStr));
end;

procedure TForm1.Button7Click(Sender: TObject);
begin
  SetLength(CmdStr, 0);
  Insert(ParamStr(0), CmdStr, Length(CmdStr));
  Insert('erase', CmdStr, Length(CmdStr));
  Insert('-c' + SpinBox9.Text + 'mb', CmdStr, Length(CmdStr));
  Insert('-t' + SpinBox10.Text, CmdStr, Length(CmdStr));
  Insert(Edit10.Text, CmdStr, Length(CmdStr));
  Insert(Edit13.Text, CmdStr, Length(CmdStr));
  if ComboBox10.ItemIndex = 1 then
    Insert(Edit12.Text, CmdStr, Length(CmdStr));
end;

procedure TForm1.Button8Click(Sender: TObject);
begin
  SetLength(CmdStr, 0);
  Insert(ParamStr(0), CmdStr, Length(CmdStr));
  Insert('replace', CmdStr, Length(CmdStr));
  Insert('-c' + SpinBox11.Text + 'mb', CmdStr, Length(CmdStr));
  Insert('-t' + SpinBox12.Text, CmdStr, Length(CmdStr));
  Insert(Edit14.Text, CmdStr, Length(CmdStr));
  Insert(Edit17.Text, CmdStr, Length(CmdStr));
  Insert(Edit16.Text, CmdStr, Length(CmdStr));
  if ComboBox13.ItemIndex = 1 then
    Insert(Edit15.Text, CmdStr, Length(CmdStr));
end;

procedure TForm1.Button9Click(Sender: TObject);
begin
  SetLength(CmdStr, 0);
  Insert(ParamStr(0), CmdStr, Length(CmdStr));
  Insert('patch', CmdStr, Length(CmdStr));
  Insert('-t' + SpinBox14.Text, CmdStr, Length(CmdStr));
  Insert(Edit18.Text, CmdStr, Length(CmdStr));
  Insert(Edit20.Text, CmdStr, Length(CmdStr));
  Insert(Edit19.Text, CmdStr, Length(CmdStr));
end;

procedure TForm1.CheckBox4Change(Sender: TObject);
begin
  SpinBox7.Enabled := CheckBox4.IsChecked;
  ComboEdit2.Enabled := CheckBox4.IsChecked;
end;

procedure TForm1.ComboBox10Change(Sender: TObject);
begin
  Edit12.Enabled := ComboBox10.ItemIndex <> 0;
end;

procedure TForm1.ComboBox11Change(Sender: TObject);
begin
  Edit13.Text := '';
end;

procedure TForm1.ComboBox12Change(Sender: TObject);
begin
  Edit14.Text := '';
end;

procedure TForm1.ComboBox13Change(Sender: TObject);
begin
  Edit15.Enabled := ComboBox13.ItemIndex <> 0;
end;

procedure TForm1.ComboBox14Change(Sender: TObject);
begin
  Edit16.Text := '';
end;

procedure TForm1.ComboBox15Change(Sender: TObject);
begin
  Edit17.Text := '';
end;

procedure TForm1.ComboBox16Change(Sender: TObject);
begin
  Edit18.Text := '';
  Edit20.Text := '';
end;

procedure TForm1.ComboBox17Change(Sender: TObject);
begin
  Edit27.Text := '';
end;

procedure TForm1.ComboBox18Change(Sender: TObject);
begin
  Edit26.Text := '';
end;

procedure TForm1.ComboBox19Change(Sender: TObject);
begin
  Edit21.Text := '';
end;

procedure TForm1.ComboBox21Change(Sender: TObject);
begin
  Edit23.Text := '';
end;

procedure TForm1.ComboBox2Change(Sender: TObject);
begin
  SearchEditButton1.Enabled := ComboBox2.ItemIndex <> 2;
  Edit1.ReadOnly := ComboBox2.ItemIndex <> 2;
  if ComboBox2.ItemIndex = 2 then
    Edit1.Text := 'http://mattmahoney.net/dc/silesia.zip'
  else
    Edit1.Text := '';
end;

procedure TForm1.ComboBox3Change(Sender: TObject);
begin
  Edit3.Enabled := ComboBox3.ItemIndex <> 0;
  ComboEdit1.Enabled := (ComboBox5.ItemIndex = 2) and (ComboBox3.ItemIndex = 0);
end;

procedure TForm1.ComboBox4Change(Sender: TObject);
begin
  Edit5.Enabled := ComboBox4.ItemIndex > 0;
  Edit5.Text := '';
end;

procedure TForm1.ComboBox5Change(Sender: TObject);
begin
  SpinBox4.Enabled := ComboBox5.ItemIndex = 2;
  ComboEdit1.Enabled := (ComboBox5.ItemIndex = 2) and (ComboBox3.ItemIndex = 0);
end;

procedure TForm1.ComboBox6Change(Sender: TObject);
begin
  Edit7.Text := '';
end;

procedure TForm1.ComboBox7Change(Sender: TObject);
begin
  Edit8.Text := '';
end;

procedure TForm1.ComboBox8Change(Sender: TObject);
begin
  Edit9.Enabled := ComboBox8.ItemIndex <> 0;
end;

procedure TForm1.ComboBox9Change(Sender: TObject);
begin
  Edit10.Text := '';
end;

procedure TForm1.Edit6Change(Sender: TObject);
begin
  if Sender = Edit6 then
  begin
    Edit28.Text := Edit6.Text;
    ShowMessage('Restart required to reload new plugins folder.');
  end
  else
    Edit6.Text := Edit28.Text;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Edit6.Text <> GetIniString('UI', 'Plugins', '',
    ChangeFileExt(GetModuleName, '.ini')) then
    SetIniString('UI', 'Plugins', Edit6.Text,
      ChangeFileExt(GetModuleName, '.ini'));
end;

procedure TForm1.FormShow(Sender: TObject);
var
  I: Integer;
begin
  if not Init then
  begin
    Init := True;
    for I := 0 to ComponentCount - 1 do
    begin
      if Components[I] is TSpinBox then
        TSpinBox(Components[I]).Cursor := crDefault;
      if Components[I] is TComboBox then
        TComboBox(Components[I]).OnChange(nil);
    end;
    SpinBox2.Max := CPUCount * 2;
    SpinBox2.Value := Max(1, CPUCount div 2);
    SpinBox8.Max := CPUCount * 2;
    SpinBox8.Value := Max(1, CPUCount div 2);
    SpinBox10.Max := CPUCount * 2;
    SpinBox10.Value := Max(1, CPUCount div 2);
    SpinBox12.Max := CPUCount * 2;
    SpinBox12.Value := Max(1, CPUCount div 2);
    SpinBox14.Max := CPUCount * 2;
    SpinBox14.Value := Max(1, CPUCount div 2);
    SpinBox16.Max := CPUCount * 2;
    SpinBox16.Value := Max(1, CPUCount div 2);
    SpinBox13.Max := CPUCount * 2;
    SpinBox13.Value := Max(1, CPUCount div 2);
  end;
end;

procedure TForm1.SearchEditButton10Click(Sender: TObject);
begin
  SaveDialog1.FileName := '';
  if SaveDialog1.Execute then
    Edit11.Text := SaveDialog1.FileName;
end;

procedure TForm1.SearchEditButton11Click(Sender: TObject);
begin
  SaveDialog1.FileName := '';
  if SaveDialog1.Execute then
    Edit12.Text := SaveDialog1.FileName;
end;

procedure TForm1.SearchEditButton12Click(Sender: TObject);
var
  Dir: string;
begin
  case ComboBox11.ItemIndex of
    0:
      begin
        OpenDialog1.FileName := '';
        if OpenDialog1.Execute then
          Edit13.Text := OpenDialog1.FileName;
      end;
    1:
      if SelectDirectory('', '', Dir) then
        Edit13.Text := Dir;
  end;
end;

procedure TForm1.SearchEditButton13Click(Sender: TObject);
var
  Dir: string;
begin
  case ComboBox12.ItemIndex of
    0:
      begin
        OpenDialog1.FileName := '';
        if OpenDialog1.Execute then
          Edit14.Text := OpenDialog1.FileName;
      end;
    1:
      if SelectDirectory('', '', Dir) then
        Edit14.Text := Dir;
  end;
end;

procedure TForm1.SearchEditButton14Click(Sender: TObject);
begin
  SaveDialog1.FileName := '';
  if SaveDialog1.Execute then
    Edit15.Text := SaveDialog1.FileName;
end;

procedure TForm1.SearchEditButton15Click(Sender: TObject);
var
  Dir: string;
begin
  case ComboBox14.ItemIndex of
    0:
      begin
        OpenDialog1.FileName := '';
        if OpenDialog1.Execute then
          Edit16.Text := OpenDialog1.FileName;
      end;
    1:
      if SelectDirectory('', '', Dir) then
        Edit16.Text := Dir;
  end;
end;

procedure TForm1.SearchEditButton16Click(Sender: TObject);
var
  Dir: string;
begin
  case ComboBox15.ItemIndex of
    0:
      begin
        OpenDialog1.FileName := '';
        if OpenDialog1.Execute then
          Edit17.Text := OpenDialog1.FileName;
      end;
    1:
      if SelectDirectory('', '', Dir) then
        Edit17.Text := Dir;
  end;
end;

procedure TForm1.SearchEditButton17Click(Sender: TObject);
var
  Dir: string;
begin
  case ComboBox16.ItemIndex of
    0:
      begin
        OpenDialog1.FileName := '';
        if OpenDialog1.Execute then
          Edit18.Text := OpenDialog1.FileName;
      end;
    1:
      if SelectDirectory('', '', Dir) then
        Edit18.Text := Dir;
  end;
end;

procedure TForm1.SearchEditButton18Click(Sender: TObject);
begin
  SaveDialog1.FileName := '';
  if SaveDialog1.Execute then
    Edit19.Text := SaveDialog1.FileName;
end;

procedure TForm1.SearchEditButton19Click(Sender: TObject);
var
  Dir: string;
begin
  case ComboBox16.ItemIndex of
    0:
      begin
        OpenDialog1.FileName := '';
        if OpenDialog1.Execute then
          Edit20.Text := OpenDialog1.FileName;
      end;
    1:
      if SelectDirectory('', '', Dir) then
        Edit20.Text := Dir;
  end;
end;

procedure TForm1.SearchEditButton1Click(Sender: TObject);
var
  Dir: string;
begin
  case ComboBox2.ItemIndex of
    0:
      begin
        OpenDialog1.FileName := '';
        if OpenDialog1.Execute then
          Edit1.Text := OpenDialog1.FileName;
      end;
    1:
      if SelectDirectory('', '', Dir) then
        Edit1.Text := Dir;
  end;
end;

procedure TForm1.SearchEditButton20Click(Sender: TObject);
var
  Dir: string;
begin
  case ComboBox19.ItemIndex of
    0:
      begin
        OpenDialog1.FileName := '';
        if OpenDialog1.Execute then
          Edit21.Text := OpenDialog1.FileName;
      end;
    1:
      if SelectDirectory('', '', Dir) then
        Edit21.Text := Dir;
  end;
end;

procedure TForm1.SearchEditButton21Click(Sender: TObject);
begin
  SaveDialog2.FileName := '';
  if SaveDialog2.Execute then
    Edit22.Text := SaveDialog2.FileName;
end;

procedure TForm1.SearchEditButton22Click(Sender: TObject);
var
  Dir: string;
begin
  case ComboBox21.ItemIndex of
    0:
      begin
        OpenDialog1.FileName := '';
        if OpenDialog1.Execute then
          Edit23.Text := OpenDialog1.FileName;
      end;
    1:
      if SelectDirectory('', '', Dir) then
        Edit23.Text := Dir;
  end;
end;

procedure TForm1.SearchEditButton23Click(Sender: TObject);
  function IndexInt(AInteger: Integer; const AValues: array of Integer)
    : Integer;
  var
    I: Integer;
  begin
    Result := -1;
    for I := Low(AValues) to High(AValues) do
      if AInteger = AValues[I] then
      begin
        Result := I;
        break;
      end;
  end;

const
  XTOOL_PRECOMP = $304C5458;
  XTOOL_IODEC = $314C5458;
  XTOOL_PATCH = $324C5458;
  XTOOL_ARCH = $334C5458;
  XTOOL_EXEC = $344C5458;
var
  I: Integer;
begin
  OpenDialog1.FileName := '';
  if OpenDialog1.Execute then
  begin
    with TFileStream.Create(OpenDialog1.FileName, fmShareDenyNone) do
      try
        ReadBuffer(I, I.Size);
      finally
        Free;
      end;
    DecodeMode := IndexInt(I, [XTOOL_PRECOMP, XTOOL_IODEC, XTOOL_PATCH,
      XTOOL_ARCH]);
    if DecodeMode < 0 then
      raise Exception.Create('Unsupported input');
    SpinBox13.Enabled := DecodeMode in [0];
    GroupBox36.Enabled := DecodeMode in [1, 2];
    Edit25.Text := OpenDialog1.FileName;
  end;
end;

procedure TForm1.SearchEditButton24Click(Sender: TObject);
var
  Dir: string;
begin
  case ComboBox18.ItemIndex of
    0:
      begin
        SaveDialog1.FileName := '';
        if SaveDialog1.Execute then
          Edit26.Text := SaveDialog1.FileName;
      end;
    1:
      if SelectDirectory('', '', Dir) then
        Edit26.Text := Dir;
  end;
end;

procedure TForm1.SearchEditButton25Click(Sender: TObject);
var
  Dir: string;
begin
  case ComboBox17.ItemIndex of
    0:
      begin
        OpenDialog1.FileName := '';
        if OpenDialog1.Execute then
          Edit27.Text := OpenDialog1.FileName;
      end;
    1:
      if SelectDirectory('', '', Dir) then
        Edit27.Text := Dir;
  end;
end;

procedure TForm1.SearchEditButton3Click(Sender: TObject);
begin
  SaveDialog1.FileName := '';
  if SaveDialog1.Execute then
    Edit3.Text := SaveDialog1.FileName;
end;

procedure TForm1.SearchEditButton4Click(Sender: TObject);
var
  Dir: string;
begin
  if SelectDirectory('', '', Dir) then
    Edit5.Text := Dir;
end;

procedure TForm1.SearchEditButton5Click(Sender: TObject);
var
  Dir: string;
begin
  if SelectDirectory('', '', Dir) then
  begin
    Edit6.Text := Dir;
    Edit28.Text := Dir;
  end;
end;

procedure TForm1.SearchEditButton6Click(Sender: TObject);
var
  Dir: string;
begin
  case ComboBox6.ItemIndex of
    0:
      begin
        OpenDialog1.FileName := '';
        if OpenDialog1.Execute then
          Edit7.Text := OpenDialog1.FileName;
      end;
    1:
      if SelectDirectory('', '', Dir) then
        Edit7.Text := Dir;
  end;
end;

procedure TForm1.SearchEditButton7Click(Sender: TObject);
var
  Dir: string;
begin
  case ComboBox7.ItemIndex of
    0:
      begin
        OpenDialog1.FileName := '';
        if OpenDialog1.Execute then
          Edit8.Text := OpenDialog1.FileName;
      end;
    1:
      if SelectDirectory('', '', Dir) then
        Edit8.Text := Dir;
  end;
end;

procedure TForm1.SearchEditButton8Click(Sender: TObject);
begin
  SaveDialog1.FileName := '';
  if SaveDialog1.Execute then
    Edit9.Text := SaveDialog1.FileName;
end;

procedure TForm1.SearchEditButton9Click(Sender: TObject);
var
  Dir: string;
begin
  case ComboBox9.ItemIndex of
    0:
      begin
        OpenDialog1.FileName := '';
        if OpenDialog1.Execute then
          Edit10.Text := OpenDialog1.FileName;
      end;
    1:
      if SelectDirectory('', '', Dir) then
        Edit10.Text := Dir;
  end;
end;

end.
