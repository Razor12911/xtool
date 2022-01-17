{*****************************************************************************
  The DEC team (see file NOTICE.txt) licenses this file
  to you under the Apache License, Version 2.0 (the
  "License"); you may not use this file except in compliance
  with the License. A copy of this licence is found in the root directory
  of this project in the file LICENCE.txt or alternatively at

    http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing,
  software distributed under the License is distributed on an
  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied.  See the License for the
  specific language governing permissions and limitations
  under the License.
*****************************************************************************}

// Simple project group for easier DEC development
program DEC60;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  DECBaseClass in 'DECBaseClass.pas',
  DECCipherBase in 'DECCipherBase.pas',
  DECCipherFormats in 'DECCipherFormats.pas',
  DECCipherModes in 'DECCipherModes.pas',
  DECCipherInterface in 'DECCipherInterface.pas',
  DECCiphers in 'DECCiphers.pas',
  DECCRC in 'DECCRC.pas',
  DECData in 'DECData.pas',
  DECDataCipher in 'DECDataCipher.pas',
  DECDataHash in 'DECDataHash.pas',
  DECFormat in 'DECFormat.pas',
  DECFormatBase in 'DECFormatBase.pas',
  DECHash in 'DECHash.pas',
  DECHashBase in 'DECHashBase.pas',
  DECHashInterface in 'DECHashInterface.pas',
  DECRandom in 'DECRandom.pas',
  DECTypes in 'DECTypes.pas',
  DECUtil in 'DECUtil.pas',
  DECUtilRawByteStringHelper in 'DECUtilRawByteStringHelper.pas',
  DECHashAuthentication in 'DECHashAuthentication.pas',
  DECHashBitBase in 'DECHashBitBase.pas';

begin
  try
    { TODO -oUser -cConsole Main : Insert code here }
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;
end.
