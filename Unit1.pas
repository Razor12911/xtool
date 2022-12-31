unit Unit1;

interface

uses
  WinAPI.Windows,
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, System.Math, System.StrUtils, System.IniFiles,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.TabControl,
  FMX.Layouts, FMX.ListBox, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Edit,
  FMX.EditBox, FMX.SpinBox, FMX.Menus;

type
  TForm1 = class(TForm)
    StyleBook1: TStyleBook;
    SaveDialog1: TSaveDialog;
    OpenDialog1: TOpenDialog;
    Layout4: TLayout;
    Button1: TButton;
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
    CheckBox1: TCheckBox;
    Layout2: TLayout;
    Label3: TLabel;
    SpinBox3: TSpinBox;
    GroupBox4: TGroupBox;
    Layout6: TLayout;
    GroupBox5: TGroupBox;
    Layout8: TLayout;
    Label7: TLabel;
    SpinBox7: TSpinBox;
    CheckBox5: TCheckBox;
    CheckBox3: TCheckBox;
    Label4: TLabel;
    SpinBox4: TSpinBox;
    CheckBox4: TCheckBox;
    PopupMenu1: TPopupMenu;
    MenuItem1: TMenuItem;
    GroupBox6: TGroupBox;
    Layout5: TLayout;
    Edit2: TEdit;
    Button2: TButton;
    GroupBox7: TGroupBox;
    Layout9: TLayout;
    Edit4: TEdit;
    SearchEditButton2: TSearchEditButton;
    ComboBox1: TComboBox;
    CheckBox2: TCheckBox;
    CheckBox6: TCheckBox;
    GroupBox8: TGroupBox;
    Layout10: TLayout;
    Edit5: TEdit;
    SearchEditButton4: TSearchEditButton;
    ComboBox4: TComboBox;
    Label5: TLabel;
    Edit6: TEdit;
    SearchEditButton5: TSearchEditButton;
    procedure FormShow(Sender: TObject);
    procedure SearchEditButton1Click(Sender: TObject);
    procedure SearchEditButton3Click(Sender: TObject);
    procedure ComboBox3Change(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure SearchEditButton2Click(Sender: TObject);
    procedure ComboBox4Change(Sender: TObject);
    procedure SearchEditButton4Click(Sender: TObject);
    procedure Edit6Change(Sender: TObject);
    procedure SearchEditButton5Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  CmdStr: TArray<String>;

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

procedure TForm1.Button1Click(Sender: TObject);
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
  if ComboBox1.ItemIndex = 1 then
    Insert('-db', CmdStr, Length(CmdStr))
  else if ComboBox1.ItemIndex > 1 then
    Insert('-db' + Edit4.Text, CmdStr, Length(CmdStr));
  if CheckBox3.IsChecked then
    Insert('-dd' + IfThen(SpinBox4.Enabled, SpinBox4.Text, ''), CmdStr,
      Length(CmdStr));
  if CheckBox4.IsChecked then
    Insert('--compress=' + 't' + SpinBox2.Text + ':l' + SpinBox7.Text + ':hi' +
      IfThen(CheckBox5.IsChecked, '1', '0'), CmdStr, Length(CmdStr));
  Insert('--basedir=' + Edit6.Text, CmdStr, Length(CmdStr));
  Insert(Edit1.Text, CmdStr, Length(CmdStr));
  if ComboBox3.ItemIndex = 0 then
    Insert(Edit3.Text, CmdStr, Length(CmdStr));
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Form2.Left := Form1.Left + (Form1.Width div 2) - (Form2.Width div 2);
  Form2.Top := Form1.Top + (Form1.Height div 2) - (Form2.Height div 2);
  Form2.ShowModal;
  Form2.Close;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  Edit4.Enabled := ComboBox1.ItemIndex in [2, 3];
  Edit4.Text := '';
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
  Edit3.Enabled := ComboBox3.ItemIndex <> 1;
end;

procedure TForm1.ComboBox4Change(Sender: TObject);
begin
  Edit5.Enabled := ComboBox4.ItemIndex > 0;
  Edit5.Text := '';
end;

procedure TForm1.Edit6Change(Sender: TObject);
begin
  ShowMessage('Restart required to reload new plugins folder.');
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
  for I := 0 to ComponentCount - 1 do
    if Components[I] is TSpinBox then
      TSpinBox(Components[I]).Cursor := crDefault;
  SpinBox2.Max := CPUCount * 2;
  SpinBox2.Value := Max(1, CPUCount div 2);
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

procedure TForm1.SearchEditButton2Click(Sender: TObject);
begin
  if ComboBox1.ItemIndex = 2 then
  begin
    SaveDialog1.FileName := '';
    if SaveDialog1.Execute then
      Edit4.Text := SaveDialog1.FileName;
  end
  else if ComboBox1.ItemIndex = 3 then
  begin
    OpenDialog1.FileName := '';
    if OpenDialog1.Execute then
      Edit4.Text := OpenDialog1.FileName;
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
    Edit6.Text := Dir;
end;

end.
