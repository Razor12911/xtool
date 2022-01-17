program ProgressDemoVCL;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {Form1},
  DECBaseClass in '..\..\Source\DECBaseClass.pas',
  DECCipherBase in '..\..\Source\DECCipherBase.pas',
  DECCipherFormats in '..\..\Source\DECCipherFormats.pas',
  DECCipherInterface in '..\..\Source\DECCipherInterface.pas',
  DECCipherModes in '..\..\Source\DECCipherModes.pas',
  DECCiphers in '..\..\Source\DECCiphers.pas',
  DECDataCipher in '..\..\Source\DECDataCipher.pas',
  DECCRC in '..\..\Source\DECCRC.pas',
  DECUtil in '..\..\Source\DECUtil.pas',
  DECTypes in '..\..\Source\DECTypes.pas',
  DECUtilRawByteStringHelper in '..\..\Source\DECUtilRawByteStringHelper.pas',
  DECFormatBase in '..\..\Source\DECFormatBase.pas',
  DECData in '..\..\Source\DECData.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
