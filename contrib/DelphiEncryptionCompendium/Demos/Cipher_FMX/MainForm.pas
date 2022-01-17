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

unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.StdCtrls, FMX.ListBox, FMX.Controls.Presentation, FMX.Edit, System.Rtti,
  {$IF RTLVersion < 31}
  {$ELSE}
  FMX.Grid.Style,
  {$ENDIF}
  FMX.Grid, FMX.ScrollBox, DECCipherBase, DECFormatBase;

type
  /// <summary>
  ///   Form of the cross platform FMX Cipher demo
  /// </summary>
  TFormMain = class(TForm)
    VertScrollBox1: TVertScrollBox;
    LayoutTop: TLayout;
    Label2: TLabel;
    ComboBoxCipherAlgorithm: TComboBox;
    Label5: TLabel;
    ComboBoxInputFormatting: TComboBox;
    Label6: TLabel;
    ComboBoxOutputFormatting: TComboBox;
    Label1: TLabel;
    EditKey: TEdit;
    Label3: TLabel;
    EditInitVector: TEdit;
    Label4: TLabel;
    EditFiller: TEdit;
    Label7: TLabel;
    ComboBoxChainingMethod: TComboBox;
    CheckBoxLiveCalc: TCheckBox;
    Label8: TLabel;
    StringGridContext: TStringGrid;
    StringColumn1: TStringColumn;
    StringColumn2: TStringColumn;
    Label9: TLabel;
    Label10: TLabel;
    EditPlainText: TEdit;
    EditCipherText: TEdit;
    ButtonEncrypt: TButton;
    ButtonDecrypt: TButton;
    LabelVersion: TLabel;
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ComboBoxCipherAlgorithmChange(Sender: TObject);
    procedure ButtonEncryptClick(Sender: TObject);
    procedure EditPlainTextChangeTracking(Sender: TObject);
    procedure ButtonDecryptClick(Sender: TObject);
  private
    procedure InitFormatCombos;
    procedure InitCipherCombo;
    procedure InitCipherModes;
    procedure ShowErrorMessage(ErrorMsg: string);
    function GetSelectedCipherMode: TCipherMode;
    function GetSettings(var InputFormatting  : TDECFormatClass;
                         var OutputFormatting : TDECFormatClass): Boolean;
    function GetCipherAlgorithm(var Cipher: TDECCipher): Boolean;
  public
  end;

var
  FormMain: TFormMain;

implementation

uses
  System.TypInfo, Generics.Collections, FMX.Platform,
  DECBaseClass, DECFormat, DECCipherModes,
  DECCipherFormats, DECCiphers, DECUtil
  {$IFDEF Android}
  ,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.Helpers,
  Androidapi.JNI.App
  {$ENDIF};

{$R *.fmx}

procedure TFormMain.ButtonDecryptClick(Sender: TObject);
var
  Cipher           : TDECCipher;
  InputFormatting  : TDECFormatClass;
  OutputFormatting : TDECFormatClass;
  InputBuffer      : TBytes;
  OutputBuffer     : TBytes;
begin
  if not GetSettings(InputFormatting, OutputFormatting) then
    exit;

  if ComboBoxCipherAlgorithm.ItemIndex >= 0 then
  begin
    if not GetCipherAlgorithm(Cipher) then
      exit;

    try
      InputBuffer  := System.SysUtils.BytesOf(EditCipherText.Text);

      if InputFormatting.IsValid(InputBuffer) then
      begin
        OutputBuffer := (Cipher as TDECFormattedCipher).DecodeBytes(InputFormatting.Decode(InputBuffer));

        EditPlainText.Text := string(DECUtil.BytesToRawString(OutputFormatting.Encode(OutputBuffer)));
      end
      else
        ShowErrorMessage('Input has wrong format');
    finally
      Cipher.Free;
    end;
  end
  else
    ShowErrorMessage('No cipher algorithm selected');
end;

procedure TFormMain.ButtonEncryptClick(Sender: TObject);
var
  Cipher           : TDECCipher;
  InputFormatting  : TDECFormatClass;
  OutputFormatting : TDECFormatClass;
  InputBuffer      : TBytes;
  OutputBuffer     : TBytes;
begin
  if not GetSettings(InputFormatting, OutputFormatting) then
    exit;

  if ComboBoxCipherAlgorithm.ItemIndex >= 0 then
  begin
    if not GetCipherAlgorithm(Cipher) then
      exit;

    try
      InputBuffer  := System.SysUtils.BytesOf(EditPlainText.Text);

      if InputFormatting.IsValid(InputBuffer) then
      begin
        OutputBuffer := (Cipher as TDECFormattedCipher).EncodeBytes(InputFormatting.Decode(InputBuffer));

        EditCipherText.Text := string(DECUtil.BytesToRawString(OutputFormatting.Encode(OutputBuffer)));
      end
      else
        ShowErrorMessage('Input has wrong format');
    finally
      Cipher.Free;
    end;
  end
  else
    ShowErrorMessage('No cipher algorithm selected');
end;

function TFormMain.GetSettings(var InputFormatting  : TDECFormatClass;
                               var OutputFormatting : TDECFormatClass): Boolean;
begin
  result := false;

  if ComboBoxInputFormatting.ItemIndex >= 0 then
  begin
    // Find the class type of the selected formatting class and create an instance of it
    InputFormatting := TDECFormat.ClassByName(
      ComboBoxInputFormatting.Items[ComboBoxInputFormatting.ItemIndex]);
  end
  else
  begin
    ShowErrorMessage('No input format selected');
    exit;
  end;

  if ComboBoxOutputFormatting.ItemIndex >= 0 then
  begin
    // Find the class type of the selected formatting class and create an instance of it
    OutputFormatting := TDECFormat.ClassByName(
      ComboBoxOutputFormatting.Items[ComboBoxOutputFormatting.ItemIndex]);
  end
  else
  begin
    ShowErrorMessage('No output format selected');
    exit;
  end;

  if EditKey.Text.IsEmpty or EditInitVector.Text.IsEmpty or EditFiller.Text.IsEmpty then
  begin
    ShowErrorMessage('No key, initialization vector or filler byte given');
    exit;
  end;

  result := true;
end;

function TFormMain.GetCipherAlgorithm(var Cipher : TDECCipher):Boolean;
begin
  result := false;

  // Find the class type of the selected cipher class and create an instance of it
  Cipher := TDECCipher.ClassByName(
    ComboBoxCipherAlgorithm.Items[ComboBoxCipherAlgorithm.ItemIndex]).Create;

  if TFormat_HEX.IsValid(RawByteString(EditInitVector.Text)) and
     TFormat_HEX.IsValid(RawByteString(EditFiller.Text)) then
  begin
    Cipher.Init(RawByteString(EditKey.Text),
                TFormat_HEX.Decode(RawByteString(EditInitVector.Text)),
                StrToInt('0x' + EditFiller.Text));

    Cipher.Mode := GetSelectedCipherMode;
  end
  else
  begin
    ShowErrorMessage('Init vector or filler byte  not given in hexadecimal representation');
    exit;
  end;

  result := true;
end;

function TFormMain.GetSelectedCipherMode:TCipherMode;
begin
  // Determine selected block chaining method via RTTI (runtime type information)
  result := TCipherMode(System.TypInfo.GetEnumValue(
              TypeInfo(TCipherMode),
              ComboBoxChainingMethod.Items[ComboBoxChainingMethod.ItemIndex]));
end;

procedure TFormMain.ShowErrorMessage(ErrorMsg: string);
{$IF RTLVersion > 30}
var
  AsyncDlg : IFMXDialogServiceASync;
{$ENDIF}
begin
  {$IF RTLVersion > 30}
  if TPlatformServices.Current.SupportsPlatformService(IFMXDialogServiceAsync,
                                                       IInterface(AsyncDlg)) then
    AsyncDlg.MessageDialogAsync(Translate(ErrorMsg),
             TMsgDlgType.mtError, [TMsgDlgBtn.mbOk], TMsgDlgBtn.mbOk, 0,
    procedure (const AResult: TModalResult)
    begin
    end);
  {$ELSE}
  MessageDlg(Translate(ErrorMsg),
             TMsgDlgType.mtError, [TMsgDlgBtn.mbOk], 0);
  {$ENDIF}
end;

procedure TFormMain.ComboBoxCipherAlgorithmChange(Sender: TObject);
var
  Context : TCipherContext;
begin
  Context := TDECCipher.ClassByName(
    ComboBoxCipherAlgorithm.Items[ComboBoxCipherAlgorithm.ItemIndex]).Context;

  StringGridContext.RowCount := 7;
  StringGridContext.Cells[0, 0] := 'Key size (bit)';
  StringGridContext.Cells[0, 1] := 'Block size (bit)';
  StringGridContext.Cells[0, 2] := 'Buffer size (bit)';
  StringGridContext.Cells[0, 3] := 'User size (bit)';
  StringGridContext.Cells[0, 4] := 'User save';
  StringGridContext.Cells[0, 5] := 'Cipher mode';
  StringGridContext.Cells[0, 6] := 'Cipher key';

  StringGridContext.Cells[1, 0] :=  IntToStr(Context.KeySize*8);
  StringGridContext.Cells[1, 1] :=  IntToStr(Context.BlockSize*8);
  StringGridContext.Cells[1, 2] :=  IntToStr(Context.BufferSize*8);
  StringGridContext.Cells[1, 3] :=  IntToStr(Context.AdditionalBufferSize*8);
  StringGridContext.Cells[1, 4] :=  BoolToStr(Context.NeedsAdditionalBufferBackup, true);

  if ctBlock in Context.CipherType then
    StringGridContext.Cells[1, 5] := 'block cipher'
  else
    StringGridContext.Cells[1, 5] := 'stream cipher';

  if ctSymmetric in Context.CipherType then
    StringGridContext.Cells[1, 6] := 'symmetric'
  else
    StringGridContext.Cells[1, 6] := 'asymmetric';
end;

procedure TFormMain.EditPlainTextChangeTracking(Sender: TObject);
begin
  if CheckBoxLiveCalc.IsChecked then
    ButtonEncryptClick(self)
end;

procedure TFormMain.FormCreate(Sender: TObject);
var
  AppService : IFMXApplicationService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXApplicationService,
                                                       IInterface(AppService)) then
    LabelVersion.Text := format(LabelVersion.Text, [AppService.AppVersion])
  else
    LabelVersion.Text := format(LabelVersion.Text, ['']);

  InitFormatCombos;
  InitCipherCombo;
  InitCipherModes;
end;

procedure TFormMain.FormResize(Sender: TObject);
begin
  LayoutTop.Width    := VertScrollBox1.Width;
end;

procedure TFormMain.InitFormatCombos;
var
  MyClass : TPair<Int64, TDECClass>;
  Formats : TStringList;
  CopyIdx : Integer;
begin
  Formats := TStringList.Create;

  try
    for MyClass in TDECFormat.ClassList do
      Formats.Add(MyClass.Value.ClassName);

    Formats.Sort;
    ComboBoxInputFormatting.Items.AddStrings(Formats);
    ComboBoxOutputFormatting.Items.AddStrings(Formats);

    if Formats.Count > 0 then
    begin
      if Formats.Find('TFormat_Copy', CopyIdx) then
      begin
        ComboBoxInputFormatting.ItemIndex  := CopyIdx;
        ComboBoxOutputFormatting.ItemIndex := CopyIdx;
      end
      else
      begin
        ComboBoxInputFormatting.ItemIndex  := 0;
        ComboBoxOutputFormatting.ItemIndex := 0;
      end;
    end;
  finally
    Formats.Free;
  end;
end;

procedure TFormMain.InitCipherCombo;
var
  MyClass : TPair<Int64, TDECClass>;
  Ciphers : TStringList;
begin
  Ciphers := TStringList.Create;

  try
    // Alternatively you can use TDECCipher.ClassList.GetClassList(Ciphers); but
    // then it's harder to remove TCipher_Null from the list
    for MyClass in TDECCipher.ClassList do
    begin
      if (MyClass.Value <> TCipher_Null) then
        Ciphers.Add(MyClass.Value.ClassName);
    end;

    Ciphers.Sort;
    ComboBoxCipherAlgorithm.Items.AddStrings(Ciphers);

    if Ciphers.Count > 0 then
      ComboBoxCipherAlgorithm.ItemIndex  := 0;
  finally
    Ciphers.Free;
  end;
end;

procedure TFormMain.InitCipherModes;
var
  Mode : TCipherMode;
begin
  for Mode := low(TCipherMode) to high(TCipherMode) do
  begin
    ComboBoxChainingMethod.Items.Add(System.TypInfo.GetEnumName(
                                       TypeInfo(TCipherMode),
                                       Integer(Mode)));
  end;

  if ComboBoxChainingMethod.Items.Count > 0 then
    ComboBoxChainingMethod.ItemIndex := 0;
end;

end.
