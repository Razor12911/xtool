unit Unit1;

{$mode objfpc}{$H+}
{$MODESWITCH AdvancedRecords}


interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  CoreCipher, DoStatusIO, PascalStrings;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure DoStatusNear(AText: Systemstring; const ID: Integer=0);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }
procedure TForm1.DoStatusNear(AText: Systemstring; const ID: Integer=0);
begin
  Form1.Memo1.Lines.Add(AText);
  Application.ProcessMessages;
end;


procedure TForm1.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(Self, @DoStatusNear);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Button1.Visible:=False;
  TestCoreCipher;
  Button1.Visible:=True;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  DeleteDoStatusHook(Self);
end;

end.

