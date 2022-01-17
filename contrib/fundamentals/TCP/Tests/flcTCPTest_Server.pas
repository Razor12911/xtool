{   2020/05/11  5.01  Move tests from unit flcTests into seperate units.       }

{$INCLUDE flcTCPTest.inc}

unit flcTCPTest_Server;

interface



{                                                                              }
{ Test                                                                         }
{                                                                              }
{$IFDEF TCPSERVER_TEST}
procedure Test;
{$ENDIF}



implementation

{$IFDEF TCPSERVER_TEST}
uses
  SysUtils,
  flcStdTypes,
  flcSocketLib,
  flcSocket,
  flcTCPUtils,
  flcTCPConnection,
  flcTCPServer;
{$ENDIF}



{                                                                              }
{ Test                                                                         }
{                                                                              }
{$IFDEF TCPSERVER_TEST}
{$ASSERTIONS ON}
procedure Test_Server_Simple;
var S : TF5TCPServer;
    I : Integer;
begin
  S := TF5TCPServer.Create(nil);
  try
    // init
    S.AddressFamily := iaIP4;
    S.ServerPort := 12745;
    S.MaxClients := -1;
    Assert(S.State = ssInit);
    Assert(not S.Active);
    // activate
    S.Active := True;
    Assert(S.Active);
    I := 0;
    repeat
      Inc(I);
      Sleep(1);
    until (S.State <> ssStarting) or (I >= 5000);
    Assert(S.State = ssReady);
    Assert(S.ClientCount = 0);
    // shut down
    S.Active := False;
    Assert(not S.Active);
    Assert(S.State = ssClosed);
  finally
    S.Finalise;
    FreeAndNil(S);
  end;

  S := TF5TCPServer.Create(nil);
  try
    // init
    S.AddressFamily := iaIP4;
    S.ServerPort := 12745;
    S.MaxClients := -1;
    Assert(S.State = ssInit);
    for I := 1 to 10 do
      begin
        // activate
        Assert(not S.Active);
        S.Active := True;
        Assert(S.Active);
        // deactivate
        S.Active := False;
        Assert(not S.Active);
        Assert(S.State = ssClosed);
      end;
  finally
    S.Finalise;
    FreeAndNil(S);
  end;
end;

procedure Test_Server_Connections;
const
  MaxConns = 10;
var
  T : Word32;
  S : TF5TCPServer;
  C : array[1..MaxConns] of TSysSocket;
  I : Integer;
begin
  S := TF5TCPServer.Create(nil);
  S.AddressFamily := iaIP4;
  S.BindAddress := '127.0.0.1';
  S.ServerPort := 12249;
  S.MaxClients := -1;
  S.Active := True;

  for I := 1 to MaxConns do
    begin
      C[I] := TSysSocket.Create(iaIP4, ipTCP, False);
      C[I].Bind('127.0.0.1', 0);
      C[I].SetBlocking(False);
    end;
  T := TCPGetTick;
  for I := 1 to MaxConns do
    begin
      C[I].Connect('127.0.0.1', '12249');
      Sleep(5);
      if I mod 100 = 0 then
        Writeln(I, ' ', Word32(TCPGetTick - T) / I:0:2);
    end;
  I := 0;
  repeat
    Sleep(10);
    Inc(I, 10);
  until (S.ClientCount = MaxConns) or (I > 4000);
  Assert(S.ClientCount = MaxConns);
  T := Word32(TCPGetTick - T);
  Writeln(T / MaxConns:0:2);

  for I := 1 to MaxConns do
    C[I].Close;
  for I := 1 to MaxConns do
    FreeAndNil(C[I]);

  S.Active := False;
  Sleep(100);

  S.Finalise;
  S.Free;
end;

procedure Test_Server_MultiServers_Connections;
const
  MaxSrvrs = 20;
  MaxConns = 10;
var
  T : Word32;
  S : array[1..MaxSrvrs] of TF5TCPServer;
  C : array[1..MaxSrvrs] of array[1..MaxConns] of TSysSocket;
  SySo : TSysSocket;
  I, J : Integer;
begin
  for I := 1 to MaxSrvrs do
    begin
      S[I] := TF5TCPServer.Create(nil);
      S[I].AddressFamily := iaIP4;
      S[I].BindAddress := '127.0.0.1';
      S[I].ServerPort := 12300 + I;
      S[I].MaxClients := -1;
      S[I].Active := True;
    end;

  for J := 1 to MaxSrvrs do
    for I := 1 to MaxConns do
      begin
        SySo := TSysSocket.Create(iaIP4, ipTCP, False);
        C[J][I] := SySo;
        SySo.Bind('127.0.0.1', 0);
        SySo.SetBlocking(False);
      end;
  T := TCPGetTick;
  for J := 1 to MaxSrvrs do
    for I := 1 to MaxConns do
      begin
        C[J][I].Connect('127.0.0.1', RawByteString(IntToStr(12300 + J)));
        if I mod 2 = 0 then
          Sleep(1);
        if I mod 100 = 0 then
          Writeln(J, ' ', I, ' ', Word32(TCPGetTick - T) / (I + (J - 1) * MaxConns):0:2);
      end;
  I := 0;
  Sleep(1000);
  repeat
    Sleep(1);
    Inc(I);
  until (S[MaxSrvrs].ClientCount = MaxConns) or (I > 10000);
  Assert(S[MaxSrvrs].ClientCount = MaxConns);
  T := Word32(TCPGetTick - T);
  Writeln(T / (MaxConns * MaxSrvrs):0:2);
  Sleep(1000);

  for J := 1 to MaxSrvrs do
    for I := 1 to MaxConns do
      begin
        C[J][I].Close;
        C[J][I].Free;
      end;

  for I := 1 to MaxSrvrs do
    S[I].Active := False;

  for I := 1 to MaxSrvrs do
    begin
      S[I].Finalise;
      S[I].Free;
    end;
end;

procedure Test;
begin
  Test_Server_Simple;
  Test_Server_Connections;
  Test_Server_MultiServers_Connections;
end;
{$ENDIF}



end.
