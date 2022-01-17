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

/// <summary>
///   Simple demonstration of using the IDECProgress interface for displaying
///   progress of an operation
/// </summary>
unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, DECUtil;

type
  TFormMain = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    ProgressBar1: TProgressBar;
    RadioButtonMethod: TRadioButton;
    RadioButtonProcedure: TRadioButton;
    RadioButtonAnonMethod: TRadioButton;
    procedure Button1Click(Sender: TObject);
  public
    procedure OnProgress(Size, Pos: Int64; State: TDECProgressState);
  end;

var
  FormMain: TFormMain;

implementation

uses
  System.UITypes, DECCiphers, DECCipherBase;

{$R *.dfm}

resourcestring
  rFileNameEmptyFailure = 'No input file specified!';

procedure OnProgressProc(Size, Pos: Int64; State: TDECProgressState);
begin
  FormMain.ProgressBar1.Min := 0;
  FormMain.ProgressBar1.Max := Size;

  if (State = Finished) then
    FormMain.ProgressBar1.Position := FormMain.ProgressBar1.Max
  else
    FormMain.ProgressBar1.Position := Pos;
end;

procedure TFormMain.Button1Click(Sender: TObject);
var
  Cipher     : TCipher_AES;
  TargetFile : string;
begin
  if Edit1.Text = '' then
  begin
    MessageDlg(rFileNameEmptyFailure, mtError, [mbOK], -1);
    exit;
  end;

  Cipher := TCipher_AES.Create;

  try
    try
      // Init encryption
      Cipher.Init(RawByteString('Passwort1234567890'), RawByteString(#1#2#3#4#5#6#7#99), 0);
      Cipher.Mode := cmCBCx;

      // replace file extension of input file
      TargetFile := Edit1.Text;
      Delete(TargetFile, pos('.', TargetFile), length(TargetFile));
      TargetFile := TargetFile + '.enc';

      // depending on selected radio button demo a different progress event technique
      if RadioButtonMethod.Checked then
        Cipher.EncodeFile(Edit1.Text, TargetFile, OnProgress)
      else
        if RadioButtonProcedure.Checked then
          Cipher.EncodeFile(Edit1.Text, TargetFile, OnProgressProc)
        else
          if RadioButtonAnonMethod.Checked then
            Cipher.EncodeFile(Edit1.Text, TargetFile,
                              procedure(Size, Pos: Int64; State: TDECProgressState)
                              begin
                                ProgressBar1.Min := 0;
                                ProgressBar1.Max := Size;

                                if (State = Finished) then
                                  ProgressBar1.Position := ProgressBar1.Max
                                else
                                  ProgressBar1.Position := Pos;
                              end);
    except
      on E: Exception do
        MessageDlg(E.Message, mtError, [mbOK], -1);
    end;
  finally
    Cipher.Free;
  end;
end;

procedure TFormMain.OnProgress(Size, Pos: Int64; State: TDECProgressState);
begin
  ProgressBar1.Min := 0;
  ProgressBar1.Max := Size;

  if (State = Finished) then
    ProgressBar1.Position := ProgressBar1.Max
  else
    ProgressBar1.Position := Pos;
end;

end.
