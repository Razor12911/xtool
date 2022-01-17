{   2020/05/11  5.01  Move tests from unit flcTests into seperate units.       }

{$INCLUDE flcTCPTest.inc}

{$IFDEF TCPCLIENT_TEST}
  {$DEFINE TCPCLIENT_TEST_WEB}
{$ENDIF}

unit flcTCPTest_Client;

interface

{$IFDEF TCPCLIENT_TEST}
uses
  SysUtils,
  SyncObjs,
  flcStdTypes,
  flcTCPClient;
{$ENDIF}



{$IFDEF TCPCLIENT_TEST}
{                                                                              }
{ TCP Client Test Object                                                       }
{                                                                              }
type
  TTCPClientTestObj = class
    States  : array[TTCPClientState] of Boolean;
    Connect : Boolean;
    LogMsg  : String;
    Lock    : TCriticalSection;
    constructor Create;
    destructor Destroy; override;
    procedure ClientLog(Sender: TF5TCPClient; LogType: TTCPClientLogType; LogMsg: String; LogLevel: Integer);
    procedure ClientConnect(Client: TF5TCPClient);
    procedure ClientStateChanged(Client: TF5TCPClient; State: TTCPClientState);
  end;



{                                                                              }
{ Test                                                                         }
{                                                                              }
procedure Test;
{$ENDIF}



implementation

{$IFDEF TCPCLIENT_TEST}
uses
  flcTCPConnection;
{$ENDIF}




{$IFDEF TCPCLIENT_TEST}
{$ASSERTIONS ON}
{                                                                              }
{ TCP Client Test Object                                                       }
{                                                                              }
constructor TTCPClientTestObj.Create;
begin
  inherited Create;
  Lock := TCriticalSection.Create;
end;

destructor TTCPClientTestObj.Destroy;
begin
  FreeAndNil(Lock);
  inherited Destroy;
end;

procedure TTCPClientTestObj.ClientLog(Sender: TF5TCPClient; LogType: TTCPClientLogType; LogMsg: String; LogLevel: Integer);
begin
  {$IFDEF TCP_TEST_LOG_TO_CONSOLE}
  Lock.Acquire;
  try
    Writeln(LogLevel:2, ' ', LogMsg);
  finally
    Lock.Release;
  end;
  {$ENDIF}
end;

procedure TTCPClientTestObj.ClientConnect(Client: TF5TCPClient);
begin
  Connect := True;
end;

procedure TTCPClientTestObj.ClientStateChanged(Client: TF5TCPClient; State: TTCPClientState);
begin
  States[State] := True;
end;



{                                                                              }
{ Test                                                                         }
{                                                                              }
{$IFDEF TCPCLIENT_TEST_WEB}
procedure Test_Client_Web;
var
  C : TF5TCPClient;
  S : RawByteString;
  A : TTCPClientTestObj;
begin
  A := TTCPClientTestObj.Create;
  C := TF5TCPClient.Create(nil);
  try
    // init
    C.OnLog := A.ClientLog;
    C.LocalHost := '0.0.0.0';
    C.Host := 'www.google.com';
    C.Port := '80';
    C.OnStateChanged := A.ClientStateChanged;
    C.OnConnected := A.ClientConnect;
    C.WaitForStartup := True;
    Assert(not C.Active);
    Assert(C.State = csInit);
    Assert(C.IsConnectionClosed);
    Assert(not A.Connect);
    // start
    C.Active := True;
    Assert(C.Active);
    Assert(C.State <> csInit);
    Assert(A.States[csStarting]);
    Assert(C.IsConnectingOrConnected);
    Assert(not C.IsConnectionClosed);
    // wait connect
    C.WaitForConnect(8000);
    Assert(C.IsConnected);
    Assert(C.State = csReady);
    Assert(C.Connection.State = cnsConnected);
    Assert(A.Connect);
    Assert(A.States[csConnecting]);
    Assert(A.States[csConnected]);
    Assert(A.States[csReady]);
    // send request
    C.Connection.WriteByteString(
        'GET / HTTP/1.1'#13#10 +
        'Host: www.google.com'#13#10 +
        'Date: 7 Nov 2013 12:34:56 GMT'#13#10 +
        #13#10);
    // wait response
    C.BlockingConnection.WaitForReceiveData(1, 5000);
    // read response
    S := C.Connection.ReadByteString(C.Connection.ReadBufferUsed);
    Assert(S <> '');
    // close
    C.Connection.Close;
    C.WaitForClose(2000);
    Assert(not C.IsConnected);
    Assert(C.IsConnectionClosed);
    Assert(C.Connection.State = cnsClosed);
    // stop
    C.Active := False;
    Assert(not C.Active);
    Assert(C.IsConnectionClosed);
  finally
    C.Finalise;
    FreeAndNil(C);
    A.Free;
  end;
end;
{$ENDIF}

procedure Test;
begin
  {$IFDEF TCPCLIENT_TEST_WEB}
  Test_Client_Web;
  {$ENDIF}
end;
{$ENDIF}



end.

