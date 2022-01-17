{******************************************************************************}
{                                                                              }
{   2011/06/11  0.01  Initial version.                                         }
{   2011/06/23  0.02  Simple test case.                                        }
{   2016/01/09  5.03  Revised for Fundamentals 5.                              }
{                                                                              }
{ Supported compilers:                                                         }
{                                                                              }
{   Delphi 7 Win32                      5.03  2019/02/24                       }
{   Delphi XE7 Win32                    5.03  2016/01/09                       }
{   Delphi XE7 Win64                    5.03  2016/01/09                       }
{                                                                              }
{******************************************************************************}

{$INCLUDE flcHTTP.inc}

{$IFDEF HTTP_TEST}
  {$DEFINE HTTP_TEST_LOG_TO_CONSOLE}
  {$DEFINE HTTPSERVER_TEST}
  {$DEFINE HTTPCLIENT_TEST}
  {.DEFINE HTTPCLIENT_TEST_WEB1}
  {.DEFINE HTTPCLIENT_TEST_WEB2}
  {$DEFINE HTTPCLIENTSERVER_TEST}
  {$IFDEF HTTP_TLS}
    {$DEFINE HTTPCLIENTSERVER_TEST_HTTPS}
  {$ENDIF}
{$ENDIF}

unit flcHTTPTests;

interface

uses
  flcHTTPUtils,
  flcHTTPClient,
  flcHTTPServer;



{                                                                              }
{ Test cases                                                                   }
{                                                                              }
{$IFDEF HTTP_TEST}
procedure Test;
{$ENDIF}



implementation

uses
  SysUtils,
  SyncObjs,
  flcUtils,
  flcBase64,
  flcSocketLib
  {$IFDEF HTTP_TLS},
  flcTLSCertificate,
  flcTLSHandshake
  {$ENDIF};



{$IFDEF HTTP_TEST}
{$ASSERTIONS ON}

{                                                                              }
{ Test cases - Server                                                          }
{                                                                              }
{$IFDEF HTTPSERVER_TEST}
type
  THTTPServerTestObj = class
    Lock : TCriticalSection;
    constructor Create;
    destructor Destroy; override;
    procedure HTTPServerLog(const Server: TF5HTTPServer; const LogType: THTTPServerLogType; const Msg: String; const LogLevel: Integer);
  end;

constructor THTTPServerTestObj.Create;
begin
  inherited Create;
  Lock := TCriticalSection.Create;
end;

destructor THTTPServerTestObj.Destroy;
begin
  FreeAndNil(Lock);
  inherited Destroy;
end;

procedure THTTPServerTestObj.HTTPServerLog(const Server: TF5HTTPServer; const LogType: THTTPServerLogType; const Msg: String; const LogLevel: Integer);
begin
  {$IFDEF HTTP_TEST_LOG_TO_CONSOLE}
  Lock.Acquire;
  try
    Writeln(Msg);
  finally
    Lock.Release;
  end;
  {$ENDIF}
end;

procedure Test_Server;
var
  Srv : TF5HTTPServer;
  Tst : THTTPServerTestObj;
begin
  Tst := THTTPServerTestObj.Create;
  Srv := TF5HTTPServer.Create(nil);
  try
    Srv.OnLog := Tst.HTTPServerLog;
    Srv.AddressFamily := safIP4;
    Srv.ServerPort := 8088;
    Assert(not Srv.Active);
    Srv.Active := True;
    Assert(Srv.Active);
    Sleep(100);
    Assert(Srv.Active);
    Srv.Active := False;
    Assert(not Srv.Active);
  finally
    Srv.Free;
    Tst.Free;
  end;
end;
{$ENDIF}




{                                                                              }
{ Test cases - Client                                                          }
{                                                                              }
{$IFDEF HTTPCLIENT_TEST}
type
  THTTPClientTestObj = class
    Lock : TCriticalSection;
    constructor Create;
    destructor Destroy; override;
    procedure HTTPClientLog(Client: TF5HTTPClient; LogType: THTTPClientLogType; Msg: String; Level: Integer);
  end;

constructor THTTPClientTestObj.Create;
begin
  inherited Create;
  Lock := TCriticalSection.Create;
end;

destructor THTTPClientTestObj.Destroy;
begin
  FreeAndNil(Lock);
  inherited Destroy;
end;

procedure THTTPClientTestObj.HTTPClientLog(Client: TF5HTTPClient; LogType: THTTPClientLogType; Msg: String; Level: Integer);
begin
  {$IFDEF HTTP_TEST_LOG_TO_CONSOLE}
  Lock.Acquire;
  try
    Writeln(Msg);
  finally
    Lock.Release;
  end;
  {$ENDIF}
end;

procedure Test_Client;
var H : TF5HTTPClient;
    T : THTTPClientTestObj;

  {$IFDEF HTTPCLIENT_TEST_WEB1}
  // Test simple connect and request sequence
  procedure TestWeb1;
  begin
    H.Host := 'www.google.com';
    H.Port := '80';
    H.Method := cmGET;
    H.URI := '/';
    Assert(not H.Active);
    H.Active := True;
    Assert(H.Active);
    repeat
      Sleep(10);
    until H.State in [hcsStopping, hcsConnectFailed, hcsConnected_Ready];
    Assert(H.State = hcsConnected_Ready);
    Sleep(100);
    H.Request;
    repeat
      Sleep(10);
    until H.State in [hcsStopping, hcsConnectFailed, hcsResponseComplete, hcsResponseCompleteAndClosing, hcsConnected_Ready];
    Assert(H.State = hcsResponseComplete);
    H.Active := False;
    Assert(not H.Active);
  end;
  {$ENDIF}

  {$IFDEF HTTPCLIENT_TEST_WEB2}
  // Test multiple requests
  procedure TestWeb2;
  begin
    H.Host := 'www.google.com';
    // H.Host := 'www.cnn.com';
    H.Port := '80';
    H.Method := cmGET;
    H.URI := '/';
    Assert(not H.Active);
    H.Request;
    repeat
      Sleep(10);
    until H.State in [hcsStopping, hcsConnectFailed, hcsResponseComplete, hcsResponseCompleteAndClosing];
    Assert(H.State = hcsResponseComplete);
    Sleep(500);
    H.Request;
    repeat
      Sleep(10);
    until H.State in [hcsStopping, hcsConnectFailed, hcsResponseComplete, hcsResponseCompleteAndClosing];
    Assert(H.State = hcsResponseComplete);
    H.Active := False;
    Assert(not H.Active);
  end;
  {$ENDIF}

begin
  T := THTTPClientTestObj.Create;
  H := TF5HTTPClient.Create(nil);
  try
    H.OnLog := T.HTTPClientLog;
    H.UserAgent := 'Experimental';
    {$IFDEF HTTPCLIENT_TEST_WEB1}
    TestWeb1;
    {$ENDIF}
    {$IFDEF HTTPCLIENT_TEST_WEB2}
    TestWeb2;
    {$ENDIF}
  finally
    H.Free;
    T.Free;
  end;
end;
{$ENDIF}



{                                                                              }
{ Test cases - Client/Server                                                   }
{                                                                              }
{$IFDEF HTTPCLIENTSERVER_TEST}
type
  THTTPClientServerTestObj = class
    Lock : TCriticalSection;
    constructor Create;
    destructor Destroy; override;
    procedure Log(Msg: String);
    procedure HTTPClientLog(Client: TF5HTTPClient; LogType: THTTPClientLogType; Msg: String; Level: Integer);
    procedure HTTPClientResponseHeader(Client: TF5HTTPClient);
    procedure HTTPClientResponseComplete(Client: TF5HTTPClient);
    procedure HTTPServerLog(const Server: TF5HTTPServer; const LogType: THTTPServerLogType; const Msg: String; const LogLevel: Integer);
    procedure HTTPServerPrepareResponse(const Server: TF5HTTPServer; const Client: THTTPServerClient);
    procedure HTTPServerRequestComplete(const Server: TF5HTTPServer; const Client: THTTPServerClient);
  end;

constructor THTTPClientServerTestObj.Create;
begin
  inherited Create;
  Lock := TCriticalSection.Create;
end;

destructor THTTPClientServerTestObj.Destroy;
begin
  FreeAndNil(Lock);
  inherited Destroy;
end;

procedure THTTPClientServerTestObj.Log(Msg: String);
begin
  {$IFDEF HTTP_TEST_LOG_TO_CONSOLE}
  Lock.Acquire;
  try
    Writeln(Msg);
  finally
    Lock.Release;
  end;
  {$ENDIF}
end;

procedure THTTPClientServerTestObj.HTTPClientLog(Client: TF5HTTPClient; LogType: THTTPClientLogType; Msg: String; Level: Integer);
begin
  Log('C:' + IntToStr(Level) + ':' + Msg);
end;

procedure THTTPClientServerTestObj.HTTPClientResponseHeader(Client: TF5HTTPClient);
begin
  Assert(Client.ResponseCode = 200);
  Assert(Client.ResponseRecord.Header.CommonHeaders.ContentType.Value = hctTextHtml);
end;

procedure THTTPClientServerTestObj.HTTPClientResponseComplete(Client: TF5HTTPClient);
begin
  Assert(Client.ResponseContentStr = '<HTML>Test</HTML>');
end;

procedure THTTPClientServerTestObj.HTTPServerLog(const Server: TF5HTTPServer; const LogType: THTTPServerLogType; const Msg: String; const LogLevel: Integer);
begin
  Log('S:' + IntToStr(LogLevel) + ':' + Msg);
end;

procedure THTTPClientServerTestObj.HTTPServerPrepareResponse(const Server: TF5HTTPServer; const Client: THTTPServerClient);
begin
  Client.ResponseCode := 200;
  Client.ResponseMsg := 'OK';
  Client.ResponseContentType := 'text/html';
  Client.ResponseContentMechanism := hctmString;
  Client.ResponseContentStr := '<HTML>Test</HTML>';
  Client.ResponseReady := True;
end;

procedure THTTPClientServerTestObj.HTTPServerRequestComplete(const Server: TF5HTTPServer; const Client: THTTPServerClient);
begin
end;

procedure Test_ClientServer_Simple(const HTTPS: Boolean);
var
  Srv : TF5HTTPServer;
  Cln : TF5HTTPClient;
  Tst : THTTPClientServerTestObj;
  T : Integer;
  {$IFDEF HTTPCLIENTSERVER_TEST_HTTPS}
  CtL : TTLSCertificateList;
  {$ENDIF}
begin
  Tst := THTTPClientServerTestObj.Create;
  Srv := TF5HTTPServer.Create(nil);
  Cln := TF5HTTPClient.Create(nil);
  try
    Srv.OnLog := Tst.HTTPServerLog;
    Cln.OnLog := Tst.HTTPClientLog;
    //
    Srv.OnPrepareResponse := Tst.HTTPServerPrepareResponse;
    Srv.OnRequestComplete := Tst.HTTPServerRequestComplete;
    Srv.AddressFamily := safIP4;
    Srv.ServerPort := 8795;
    {$IFDEF HTTPCLIENTSERVER_TEST_HTTPS}
    if HTTPS then
      begin
        Srv.TCPServer.TLSServer.PrivateKeyRSAPEM := // from stunnel pem file
            'MIICXAIBAAKBgQCxUFMuqJJbI9KnB8VtwSbcvwNOltWBtWyaSmp7yEnqwWel5TFf' +
            'cOObCuLZ69sFi1ELi5C91qRaDMow7k5Gj05DZtLDFfICD0W1S+n2Kql2o8f2RSvZ' +
            'qD2W9l8i59XbCz1oS4l9S09L+3RTZV9oer/Unby/QmicFLNM0WgrVNiKywIDAQAB' +
            'AoGAKX4KeRipZvpzCPMgmBZi6bUpKPLS849o4pIXaO/tnCm1/3QqoZLhMB7UBvrS' +
            'PfHj/Tejn0jjHM9xYRHi71AJmAgzI+gcN1XQpHiW6kATNDz1r3yftpjwvLhuOcp9' +
            'tAOblojtImV8KrAlVH/21rTYQI+Q0m9qnWKKCoUsX9Yu8UECQQDlbHL38rqBvIMk' +
            'zK2wWJAbRvVf4Fs47qUSef9pOo+p7jrrtaTqd99irNbVRe8EWKbSnAod/B04d+cQ' +
            'ci8W+nVtAkEAxdqPOnCISW4MeS+qHSVtaGv2kwvfxqfsQw+zkwwHYqa+ueg4wHtG' +
            '/9+UgxcXyCXrj0ciYCqURkYhQoPbWP82FwJAWWkjgTgqsYcLQRs3kaNiPg8wb7Yb' +
            'NxviX0oGXTdCaAJ9GgGHjQ08lNMxQprnpLT8BtZjJv5rUOeBuKoXagggHQJAaUAF' +
            '91GLvnwzWHg5p32UgPsF1V14siX8MgR1Q6EfgKQxS5Y0Mnih4VXfnAi51vgNIk/2' +
            'AnBEJkoCQW8BTYueCwJBALvz2JkaUfCJc18E7jCP7qLY4+6qqsq+wr0t18+ogOM9' +
            'JIY9r6e1qwNxQ/j1Mud6gn6cRrObpRtEad5z2FtcnwY=';
        TLSCertificateListAppend(CtL,
          MIMEBase64Decode( // from stunnel pem file
            'MIICDzCCAXigAwIBAgIBADANBgkqhkiG9w0BAQQFADBCMQswCQYDVQQGEwJQTDEf' +
            'MB0GA1UEChMWU3R1bm5lbCBEZXZlbG9wZXJzIEx0ZDESMBAGA1UEAxMJbG9jYWxo' +
            'b3N0MB4XDTk5MDQwODE1MDkwOFoXDTAwMDQwNzE1MDkwOFowQjELMAkGA1UEBhMC' +
            'UEwxHzAdBgNVBAoTFlN0dW5uZWwgRGV2ZWxvcGVycyBMdGQxEjAQBgNVBAMTCWxv' +
            'Y2FsaG9zdDCBnzANBgkqhkiG9w0BAQEFAAOBjQAwgYkCgYEAsVBTLqiSWyPSpwfF' +
            'bcEm3L8DTpbVgbVsmkpqe8hJ6sFnpeUxX3Djmwri2evbBYtRC4uQvdakWgzKMO5O' +
            'Ro9OQ2bSwxXyAg9FtUvp9iqpdqPH9kUr2ag9lvZfIufV2ws9aEuJfUtPS/t0U2Vf' +
            'aHq/1J28v0JonBSzTNFoK1TYissCAwEAAaMVMBMwEQYJYIZIAYb4QgEBBAQDAgZA' +
            'MA0GCSqGSIb3DQEBBAUAA4GBAAhYFTngWc3tuMjVFhS4HbfFF/vlOgTu44/rv2F+' +
            'ya1mEB93htfNxx3ofRxcjCdorqONZFwEba6xZ8/UujYfVmIGCBy4X8+aXd83TJ9A' +
            'eSjTzV9UayOoGtmg8Dv2aj/5iabNeK1Qf35ouvlcTezVZt2ZeJRhqUHcGaE+apCN' +
            'TC9Y'));
        Srv.TCPServer.TLSServer.CertificateList := CtL;
      end;
    Srv.HTTPSEnabled := HTTPS;
    {$ENDIF}
    Srv.Active := True;
    Assert(Srv.Active);
    //
    Cln.OnResponseHeader := Tst.HTTPClientResponseHeader;
    Cln.OnResponseComplete := Tst.HTTPClientResponseComplete;
    Cln.ResponseContentMechanism := hcrmString;
    Cln.AddressFamily := cafIP4;
    Cln.Port := '8795';
    Cln.Host := '127.0.0.1';
    Cln.URI := '/';
    Cln.Method := cmGET;
    {$IFDEF HTTPCLIENTSERVER_TEST_HTTPS}
    Cln.UseHTTPS := HTTPS;
    {$ENDIF}
    Cln.Active := True;
    Cln.Request;

    T := 0;
    repeat
      Sleep(1);
      Inc(T);
    until (T > 2000) or (Srv.ClientCount = 1);
    Assert(Srv.ClientCount = 1);

    T := 0;
    repeat
      Sleep(1);
      Inc(T);
    until (T > 2000) or (Cln.State in [hcsResponseComplete, hcsResponseCompleteAndClosed]);
    Assert(Cln.State in [hcsResponseComplete, hcsResponseCompleteAndClosed]);

    Cln.Active := False;
    Assert(not Cln.Active);

    T := 0;
    repeat
      Sleep(1);
      Inc(T);
    until (T > 2000) or (Srv.ClientCount = 0);
    Assert(Srv.ClientCount = 0);

    Srv.Active := False;
    Assert(not Srv.Active);
  finally
    Cln.Free;
    Srv.Free;
    Tst.Free;
  end;
end;

procedure Test_ClientServer;
begin
  Test_ClientServer_Simple(False);
  {$IFDEF HTTPCLIENTSERVER_TEST_HTTPS}
  Test_ClientServer_Simple(True);
  {$ENDIF}
end;
{$ENDIF}

procedure Test;
begin
  {$IFDEF HTTPSERVER_TEST}
  Test_Server;
  {$ENDIF}
  {$IFDEF HTTPCLIENT_TEST}
  Test_Client;
  {$ENDIF}
  {$IFDEF HTTPCLIENTSERVER_TEST}
  Test_ClientServer;
  {$ENDIF}
end;
{$ENDIF}



end.

