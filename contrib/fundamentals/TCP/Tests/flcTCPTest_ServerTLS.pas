{   2020/05/11  5.01  Move tests from unit flcTests into seperate units.       }

{$INCLUDE flcTCPTest.inc}

unit flcTCPTest_ServerTLS;

interface



{$IFDEF TCPSERVER_TEST_TLS}
{                                                                              }
{ Test                                                                         }
{                                                                              }
procedure Test;
{$ENDIF}



implementation

{$IFDEF TCPSERVER_TEST_TLS}
uses
  SysUtils,
  flcSocketLib,
  flcTLSTestCertificates,
  flcTCPServer;
{$ENDIF}



{                                                                              }
{ Test                                                                         }
{                                                                              }
{$IFDEF TCPSERVER_TEST_TLS}
{$ASSERTIONS ON}
procedure Test_Server_TLS;
var
  S : TF5TCPServer;
begin
  S := TF5TCPServer.Create(nil);
  try
    // init
    S.AddressFamily := iaIP4;
    S.ServerPort := 12845;
    S.TLSEnabled := True;
    S.TLSServer.PrivateKeyRSAPEM := RSA_STunnel_PrivateKeyRSAPEM;
    S.Active := True;
  finally
    S.Finalise;
    FreeAndNil(S);
  end;
end;

procedure Test;
begin
  Test_Server_TLS;
end;
{$ENDIF}



end.
