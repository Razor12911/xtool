{   2020/05/11  5.01  Move tests from unit flcTests into seperate units.       }

{$INCLUDE flcTCPTest.inc}

unit flcTCPTest_ClientServer;

interface

{$IFDEF TCPCLIENTSERVER_TEST}
uses
  {$IFDEF OS_MSWIN}
  Windows,
  {$ENDIF}
  SysUtils,
  SyncObjs,
  Classes,

  flcStdTypes,
  flcTCPConnection,
  flcTCPClient,
  flcTCPServer;
{$ENDIF}



{$IFDEF TCPCLIENTSERVER_TEST}
{                                                                              }
{ TCP ClientServer Test Object                                                 }
{                                                                              }
type
  TTCPClientServerTestObj = class
    Lock : TCriticalSection;
    constructor Create;
    destructor Destroy; override;
    procedure Log(Msg: String);
    procedure ClientLog(Client: TF5TCPClient; LogType: TTCPClientLogType; Msg: String; LogLevel: Integer);
    procedure ServerLog(Sender: TF5TCPServer; LogType: TTCPLogType; Msg: String; LogLevel: Integer);
  end;

  TTCPClientServerBlockTestObj = class
    FinC, FinS : Boolean;
    procedure ClientExec(Client: TF5TCPClient; Connection: TTCPBlockingConnection; var CloseOnExit: Boolean);
    procedure ServerExec(Sender: TTCPServerClient; Connection: TTCPBlockingConnection; var CloseOnExit: Boolean);
  end;



{                                                                              }
{ Test functions                                                               }
{                                                                              }
type
  TF5TCPClientArray = array of TF5TCPClient;
  TTCPServerClientArray = array of TTCPServerClient;

procedure TestClientServer_ReadWrite_Blocks(
          const TestClientCount: Integer;
          const TestLargeBlock: Boolean;
          const DebugObj : TTCPClientServerTestObj;
          const C: TF5TCPClientArray;
          const T: TTCPServerClientArray);



{                                                                              }
{ Test                                                                         }
{                                                                              }
procedure Test;
{$ENDIF}



implementation

{$IFDEF TCPCLIENTSERVER_TEST}
uses
  flcSocketLib,
  flcTimers;
{$ENDIF}



{$IFDEF TCPCLIENTSERVER_TEST}
{$ASSERTIONS ON}
{                                                                              }
{ TCP ClientServer Test Object                                                 }
{                                                                              }

{ TTCPClientServerTestObj }

constructor TTCPClientServerTestObj.Create;
begin
  inherited Create;
  Lock := TCriticalSection.Create;
end;

destructor TTCPClientServerTestObj.Destroy;
begin
  FreeAndNil(Lock);
  inherited Destroy;
end;

{$IFDEF TCP_TEST_LOG_TO_CONSOLE}
procedure TTCPClientServerTestObj.Log(Msg: String);
var S : String;
begin
  S := FormatDateTime('hh:nn:ss.zzz', Now) + ' ' + Msg;
  Lock.Acquire;
  try
    Writeln(S);
  finally
    Lock.Release;
  end;
end;
{$ELSE}
procedure TTCPClientServerTestObj.Log(Msg: String);
begin
end;
{$ENDIF}

procedure TTCPClientServerTestObj.ClientLog(Client: TF5TCPClient; LogType: TTCPClientLogType; Msg: String; LogLevel: Integer);
begin
  Log('C[' + IntToStr(Client.Tag) + ']:' + Msg);
end;

procedure TTCPClientServerTestObj.ServerLog(Sender: TF5TCPServer; LogType: TTCPLogType; Msg: String; LogLevel: Integer);
begin
  Log('S:' + Msg);
end;



{ TTCPClientServerBlockTestObj }

procedure TTCPClientServerBlockTestObj.ClientExec(Client: TF5TCPClient;Connection: TTCPBlockingConnection; var CloseOnExit: Boolean);
var
  B1 : LongWord;
begin
  Sleep(200);

  B1 := 1234;
  Connection.Write(B1, 4, 10000);

  B1 := 0;
  Connection.Read(B1, 4, 10000);
  Assert(B1 = 22222);

  FinC := True;
end;

procedure TTCPClientServerBlockTestObj.ServerExec(Sender: TTCPServerClient; Connection: TTCPBlockingConnection; var CloseOnExit: Boolean);
var
  B1 : LongWord;
begin
  B1 := 0;
  Connection.Read(B1, 4, 10000);
  Assert(B1 = 1234);

  B1 := 22222;
  Connection.Write(B1, 4, 10000);

  FinS := True;
end;



{                                                                              }
{ Test                                                                         }
{                                                                              }
procedure TestClientServer_ReadWrite_Blocks(
          const TestClientCount: Integer;
          const TestLargeBlock: Boolean;
          const DebugObj : TTCPClientServerTestObj;
          const C: TF5TCPClientArray;
          const T: TTCPServerClientArray);
const
  LargeBlockSize = 256 * 1024;
var
  K, I, J : Integer;
  F : RawByteString;
  B : Byte;
begin
  // read & write (small block): client to server
  for K := 0 to TestClientCount - 1 do
    C[K].Connection.WriteByteString('Fundamentals');
  for K := 0 to TestClientCount - 1 do
    begin
      DebugObj.Log('{RWS:' + IntToStr(K + 1) + ':A}');
      I := 0;
      repeat
        Inc(I);
        Sleep(1);
      until (T[K].Connection.ReadBufferUsed >= 12) or (I >= 5000);
      DebugObj.Log('{RWS:' + IntToStr(K + 1) + ':B}');
      Assert(T[K].Connection.ReadBufferUsed = 12);
      F := T[K].Connection.PeekByteString(3);
      Assert(F = 'Fun');
      Assert(T[K].Connection.PeekByte(B));
      Assert(B = Ord('F'));
      F := T[K].Connection.ReadByteString(12);
      Assert(F = 'Fundamentals');
      DebugObj.Log('{RWS:' + IntToStr(K + 1) + ':Z}');
    end;
  // read & write (small block): server to client
  for K := 0 to TestClientCount - 1 do
    T[K].Connection.WriteByteString('123');
  for K := 0 to TestClientCount - 1 do
    begin
      C[K].BlockingConnection.WaitForReceiveData(3, 5000);
      F := C[K].Connection.ReadByteString(3);
      Assert(F = '123');
    end;
  if TestLargeBlock then
    begin
      // read & write (large block): client to server
      F := '';
      for I := 1 to LargeBlockSize do
        F := F + #1;
      for K := 0 to TestClientCount - 1 do
        C[K].Connection.WriteByteString(F);
      for K := 0 to TestClientCount - 1 do
        begin
          J := LargeBlockSize;
          repeat
            I := 0;
            repeat
              Inc(I);
              Sleep(1);
              Assert(C[K].State = csReady);
              Assert(T[K].State = scsReady);
            until (T[K].Connection.ReadBufferUsed > 0) or (I >= 5000);
            Assert(T[K].Connection.ReadBufferUsed > 0);
            F := T[K].Connection.ReadByteString(T[K].Connection.ReadBufferUsed);
            Assert(Length(F) > 0);
            for I := 1 to Length(F) do
              Assert(F[I] = #1);
            Dec(J, Length(F));
          until J <= 0;
          Assert(J = 0);
          Sleep(2);
          Assert(T[K].Connection.ReadBufferUsed = 0);
        end;
      // read & write (large block): server to client
      F := '';
      for I := 1 to LargeBlockSize do
        F := F + #1;
      for K := 0 to TestClientCount - 1 do
        T[K].Connection.WriteByteString(F);
      for K := 0 to TestClientCount - 1 do
        begin
          J := LargeBlockSize;
          repeat
            C[K].BlockingConnection.WaitForReceiveData(1, 5000);
            Assert(C[K].State = csReady);
            Assert(T[K].State = scsReady);
            Assert(C[K].Connection.ReadBufferUsed > 0);
            F := C[K].Connection.ReadByteString(C[K].Connection.ReadBufferUsed);
            Assert(Length(F) > 0);
            for I := 1 to Length(F) do
              Assert(F[I] = #1);
            Dec(J, Length(F));
          until J <= 0;
          Assert(J = 0);
          Sleep(2);
          Assert(C[K].Connection.ReadBufferUsed = 0);
        end;
    end;
end;

procedure TestClientServer_ReadWrite(
          const TestClientCount: Integer;
          const TestLargeBlock: Boolean
          );

  procedure WaitClientConnected(const Client: TF5TCPClient);
  var I : Integer;
  begin
    I := 0;
    repeat
      Inc(I);
      Sleep(1);
    until (I >= 8000) or
          (Client.State in [csReady, csClosed]);
    Assert(Client.State = csReady);
    Assert(Client.Connection.State = cnsConnected);
  end;

var C : TF5TCPClientArray;
    S : TF5TCPServer;
    T : TTCPServerClientArray;
    TSC : TTCPServerClient;
    I, K : Integer;
    DebugObj : TTCPClientServerTestObj;
begin
  DebugObj := TTCPClientServerTestObj.Create;
  S := TF5TCPServer.Create(nil);
  SetLength(C, TestClientCount);
  SetLength(T, TestClientCount);
  for K := 0 to TestClientCount - 1 do
    begin
      C[K] := TF5TCPClient.Create(nil);
      // init client
      C[K].Tag := K + 1;
      C[K].OnLog := DebugObj.ClientLog;
    end;
  try
    // init server
    S.OnLog := DebugObj.ServerLog;
    S.AddressFamily := iaIP4;
    S.BindAddress := '127.0.0.1';
    S.ServerPort := 12545;
    S.MaxClients := -1;
    Assert(S.State = ssInit);
    Assert(not S.Active);
    // start server
    S.Start;
    Assert(S.Active);
    I := 0;
    repeat
      Inc(I);
      Sleep(1);
    until (S.State <> ssStarting) or (I >= 5000);
    Sleep(100);
    Assert(S.State = ssReady);
    Assert(S.ClientCount = 0);
    for K := 0 to TestClientCount - 1 do
      begin
        // init client
        C[K].AddressFamily := cafIP4;
        C[K].Host := '127.0.0.1';
        C[K].Port := '12545';
        Assert(C[K].State = csInit);
        Assert(not C[K].Active);
        // start client
        C[K].WaitForStartup := True;
        C[K].Start;
        Assert(Assigned(C[K].Connection));
        Assert(C[K].Active);
      end;
    for K := 0 to TestClientCount - 1 do
      // wait for client to connect
      WaitClientConnected(C[K]);
    // wait for server connections
    I := 0;
    repeat
      Inc(I);
      Sleep(1);
    until (S.ClientCount >= TestClientCount) or (I >= 5000);
    Assert(S.ClientCount = TestClientCount);
    // wait for server clients
    TSC := S.ClientIterateFirst;
    for K := 0 to TestClientCount - 1 do
      begin
        T[K] := TSC;
        Assert(Assigned(T[K]));
        Assert(T[K].State in [scsStarting, scsNegotiating, scsReady]);
        Assert(T[K].Connection.State in [cnsProxyNegotiation, cnsConnected]);
        TSC.ReleaseReference;
        TSC := S.ClientIterateNext(TSC);
      end;
    // test read/write
    TestClientServer_ReadWrite_Blocks(
        TestClientCount,
        TestLargeBlock,
        DebugObj,
        C, T);
    // release reference
    for K := 0 to TestClientCount - 1 do
      T[K].ReleaseReference;
    // stop clients
    for K := TestClientCount - 1 downto 0 do
      begin
        C[K].Stop;
        Assert(C[K].State = csStopped);
      end;
    I := 0;
    repeat
      Inc(I);
      Sleep(1);
    until (S.ClientCount = 0) or (I >= 5000);
    Assert(S.ClientCount = 0);
    // stop server
    S.Stop;
    Assert(not S.Active);
  finally
    for K := TestClientCount - 1 downto 0 do
      begin
        C[K].Finalise;
        FreeAndNil(C[K]);
      end;
    S.Finalise;
    FreeAndNil(S);
    DebugObj.Free;
  end;
end;

procedure Test_ClientServer_StopStart;
const
  TestClientCount = 10;
  TestRepeatCount = 3;
var C : array of TF5TCPClient;
    S : TF5TCPServer;
    I, K : Integer;
    J : Integer;
    DebugObj : TTCPClientServerTestObj;
begin
  DebugObj := TTCPClientServerTestObj.Create;
  S := TF5TCPServer.Create(nil);
  SetLength(C, TestClientCount);
  for K := 0 to TestClientCount - 1 do
    begin
      C[K] := TF5TCPClient.Create(nil);
      // init client
      C[K].Tag := K + 1;
      C[K].OnLog := DebugObj.ClientLog;
    end;
  try
    // init server
    S.OnLog := DebugObj.ServerLog;
    S.AddressFamily := iaIP4;
    S.BindAddress := '127.0.0.1';
    S.ServerPort := 12645;
    S.MaxClients := -1;
    Assert(S.State = ssInit);
    Assert(not S.Active);
    // start server
    S.Start;
    Assert(S.Active);
    I := 0;
    repeat
      Inc(I);
      Sleep(1);
    until (S.State <> ssStarting) or (I >= 5000);
    Assert(S.State = ssReady);
    Assert(S.ClientCount = 0);
    // init clients
    for K := 0 to TestClientCount - 1 do
      begin
        C[K].AddressFamily := cafIP4;
        C[K].Host := '127.0.0.1';
        C[K].Port := '12645';
        C[K].WaitForStartup := True;
        Assert(C[K].State = csInit);
        Assert(not C[K].Active);
      end;
    // test quickly starting and stopping clients
    for J := 0 to TestRepeatCount - 1 do
      begin
        // start client
        for K := 0 to TestClientCount - 1 do
          begin
            C[K].Start;
            // connection must exist when Start exits
            Assert(Assigned(C[K].Connection));
            Assert(C[K].Active);
          end;
        // delay
        if J > 0 then
          Sleep(J - 1);
        // stop clients
        for K := TestClientCount - 1 downto 0 do
          begin
            C[K].Stop;
            Assert(C[K].State = csStopped);
            Assert(not Assigned(C[K].Connection));
          end;
        // delay
        if J > 0 then
          Sleep(J - 1);
        // re-start client
        for K := 0 to TestClientCount - 1 do
          begin
            C[K].Start;
            // connection must exist when Start exits
            Assert(Assigned(C[K].Connection));
            Assert(C[K].Active);
          end;
        // delay
        if J > 0 then
          Sleep(J - 1);
        // re-stop clients
        for K := TestClientCount - 1 downto 0 do
          begin
            C[K].Stop;
            Assert(C[K].State = csStopped);
            Assert(not Assigned(C[K].Connection));
          end;
      end;
    // stop server
    S.Stop;
    Assert(not S.Active);
  finally
    for K := TestClientCount - 1 downto 0 do
      begin
        C[K].Finalise;
        FreeAndNil(C[K]);
      end;
    S.Finalise;
    FreeAndNil(S);
    DebugObj.Free;
  end;
end;

procedure Test_ClientServer_ReadWrite;
begin
  TestClientServer_ReadWrite(1,  True);
  TestClientServer_ReadWrite(2,  True);
  TestClientServer_ReadWrite(5,  True);
  {$IFNDEF LINUX} // FAILS
  TestClientServer_ReadWrite(30, False);
  {$ENDIF}
end;

procedure Test_ClientServer_Shutdown;
var
  S : TF5TCPServer;
  C : TF5TCPClient;
  T : TTCPServerClient;
  B, X : RawByteString;
  I, L, N, M : Integer;
begin
  S := TF5TCPServer.Create(nil);
  S.AddressFamily := iaIP4;
  S.BindAddress := '127.0.0.1';
  S.ServerPort := 12213;
  S.MaxClients := -1;
  S.Active := True;
  Sleep(100);

  C := TF5TCPClient.Create(nil);
  C.LocalHost := '0.0.0.0';
  C.Host := '127.0.0.1';
  C.Port := '12213';
  C.WaitForStartup := True;
  C.UseWorkerThread := False;
  C.Active := True;
  Sleep(100);

  Assert(C.State = csReady);

  SetLength(B, 1024 * 1024);
  for I := 1 to Length(B) do
    B[I] := #1;
  L := C.Connection.WriteByteString(B);
  Assert(L > 1024);
  Sleep(1);

  Assert(not C.IsShutdownComplete);
  C.Shutdown;
  for I := 1 to 10 do
    begin
      if C.IsShutdownComplete then
        break;
      Sleep(50);
    end;
  Assert(C.IsShutdownComplete);

  T := S.ClientIterateFirst;
  Assert(Assigned(T));
  SetLength(X, L);
  M := 0;
  repeat
    N := T.Connection.Read(X[1], L);
    Inc(M, N);
    Sleep(10);
  until N <= 0;
  Assert(M = L);
  Sleep(100);

  Assert(C.State = csClosed);
  Assert(T.State = scsClosed);

  T.ReleaseReference;

  C.Close;
  C.Finalise;
  C.Free;

  S.Active := False;
  S.Finalise;
  S.Free;
end;

procedure Test_ClientServer_Block;
var
  S : TF5TCPServer;
  C : TF5TCPClient;
  DebugObj : TTCPClientServerTestObj;
  TestObj : TTCPClientServerBlockTestObj;
begin
  DebugObj := TTCPClientServerTestObj.Create;
  TestObj := TTCPClientServerBlockTestObj.Create;

  S := TF5TCPServer.Create(nil);
  S.OnLog := DebugObj.ServerLog;
  S.AddressFamily := iaIP4;
  S.BindAddress := '127.0.0.1';
  S.ServerPort := 12145;
  S.MaxClients := -1;
  S.UseWorkerThread := True;
  S.OnClientWorkerExecute := TestObj.ServerExec;
  S.Active := True;

  Sleep(50);

  C := TF5TCPClient.Create(nil);
  C.OnLog := DebugObj.ClientLog;
  C.LocalHost := '0.0.0.0';
  C.Host := '127.0.0.1';
  C.Port := '12145';
  C.WaitForStartup := True;
  C.UseWorkerThread := True;
  C.OnWorkerExecute := TestObj.ClientExec;
  C.Active := True;

  repeat
    Sleep(1);
  until TestObj.FinC and TestObj.FinS;

  C.Active := False;
  S.Active := False;

  C.Finalise;
  FreeAndNil(C);
  S.Finalise;
  FreeAndNil(S);

  TestObj.Free;
  DebugObj.Free;
end;

procedure Test_ClientServer_RetryConnect;
var
  S : TF5TCPServer;
  C : TF5TCPClient;
  DebugObj : TTCPClientServerTestObj;
  TestObj : TTCPClientServerBlockTestObj;
  I : Integer;
begin
  DebugObj := TTCPClientServerTestObj.Create;
  TestObj := TTCPClientServerBlockTestObj.Create;

  S := TF5TCPServer.Create(nil);
  S.OnLog := DebugObj.ServerLog;
  S.AddressFamily := iaIP4;
  S.BindAddress := '127.0.0.1';
  S.ServerPort := 12045;
  S.MaxClients := -1;
  S.UseWorkerThread := True;
  S.OnClientWorkerExecute := TestObj.ServerExec;

  C := TF5TCPClient.Create(nil);
  C.OnLog := DebugObj.ClientLog;
  C.LocalHost := '0.0.0.0';
  C.Host := '127.0.0.1';
  C.Port := '12045';
  C.WaitForStartup := True;
  C.UseWorkerThread := True;
  C.OnWorkerExecute := TestObj.ClientExec;
  C.RetryFailedConnect := True;
  C.RetryFailedConnectDelaySec := 2;
  C.RetryFailedConnectMaxAttempts := 3;
  C.ReconnectOnDisconnect := False;
  C.Active := True;

  Sleep(2000);

  S.Active := True;

  I := 0;
  repeat
    Sleep(1);
    Inc(I);
  until (TestObj.FinC and TestObj.FinS) or (I > 4000);
  Assert(TestObj.FinC and TestObj.FinS);

  //S.Active := False;
  //Sleep(100000);

  C.Active := False;
  S.Active := False;

  C.Finalise;
  FreeAndNil(C);

  S.Finalise;
  FreeAndNil(S);

  TestObj.Free;
  DebugObj.Free;
end;

procedure Test_ClientServer_Latency;

  procedure DoYield;
  begin
    {$IFDEF DELPHI}
    {$IFDEF OS_MSWIN}
    Yield;
    {$ELSE}
    TThread.Yield;
    {$ENDIF}
    {$ELSE}
    TThread.Yield;
    {$ENDIF}
  end;

  procedure WaitClientConnected(const Client: TF5TCPClient);
  var I : Integer;
  begin
    // wait for client to connect
    I := 0;
    repeat
      Inc(I);
      Sleep(1);
    until (I >= 8000) or
          (Client.State in [csReady, csClosed]);
    Assert(Client.State = csReady);
    Assert(Client.Connection.State = cnsConnected);
  end;

var C : TF5TCPClient;
    S : TF5TCPServer;
    TSC : TTCPServerClient;
    I, Nr : Integer;
    F : RawByteString;
    DebugObj : TTCPClientServerTestObj;
    T : Word64;
    Buf : array[0..15] of Byte;
const
  Test1N = 5;
begin
  DebugObj := TTCPClientServerTestObj.Create;
  DebugObj.Log('');
  DebugObj.Log('Test_ClientServer_Latency:');

  S := TF5TCPServer.Create(nil);
  C := TF5TCPClient.Create(nil);
  // init client
  C.Tag := 1;
  C.OnLog := DebugObj.ClientLog;
  try
    // init server
    S.OnLog := DebugObj.ServerLog;
    S.AddressFamily := iaIP4;
    S.BindAddress := '127.0.0.1';
    Randomize;
    S.ServerPort := 12513 + Random(1000);
    S.MaxClients := -1;
    Assert(S.State = ssInit);
    Assert(not S.Active);
    // start server
    S.Start;
    Assert(S.Active);
    I := 0;
    repeat
      Inc(I);
      Sleep(10);
    until (S.State <> ssStarting) or (I >= 400);
    Sleep(100);
    Assert(S.State = ssReady);
    Assert(S.ClientCount = 0);
    // init client
    C.AddressFamily := cafIP4;
    C.Host := '127.0.0.1';
    C.Port := IntToStr(S.ServerPort);
    Assert(C.State = csInit);
    Assert(not C.Active);
    // start client
    C.WaitForStartup := True;
    C.Start;
    Assert(Assigned(C.Connection));
    Assert(C.Active);
    // wait for client to connect
    WaitClientConnected(C);
    // wait for server connections
    I := 0;
    repeat
      Inc(I);
      Sleep(10);
    until (S.ClientCount >= 1) or (I >= 10000);
    Assert(S.ClientCount = 1);
    // wait for server clients
    TSC := S.ClientIterateFirst;
    Assert(Assigned(TSC));
    Assert(TSC.State in [scsStarting, scsNegotiating, scsReady]);
    Assert(TSC.Connection.State in [cnsProxyNegotiation, cnsConnected]);
    for Nr := 1 to Test1N do
      begin
        // read & write (small block): client to server
        // read directly from connection before buffered
        T := GetMicroTick;
        C.Connection.WriteByteString('Fundamentals');
        repeat
          DoYield;
          F := TSC.Connection.ReadByteString(12);
          if F = 'Fundamentals' then
            break;
        until MicroTickDelta(T, GetMicroTick) >= 5000000; // 5s
        T := MicroTickDelta(T, GetMicroTick);
        DebugObj.Log('  Latency1_ClientToServer:' + IntToStr(T));
        Assert(F = 'Fundamentals');
        // read & write (small block): server to client
        // read directly from connection before buffered
        FillChar(Buf, SizeOf(Buf), 0);
        T := GetMicroTick;
        TSC.Connection.WriteByteString('123');
        repeat
          DoYield;
          if C.Connection.Read(Buf[0], 3) = 3 then
            if (Buf[0] = Ord('1')) and (Buf[1] = Ord('2')) and (Buf[2] = Ord('3')) then
              break;
        until MicroTickDelta(T, GetMicroTick) >= 5000000; // 5s
        T := MicroTickDelta(T, GetMicroTick);
        DebugObj.Log('  Latency2_ServerToClient:' + IntToStr(T));
        Assert((Buf[0] = Ord('1')) and (Buf[1] = Ord('2')) and (Buf[2] = Ord('3')));
        // read & write (small block): client to server
        T := GetMicroTick;
        C.Connection.WriteByteString('Fundamentals');
        repeat
          DoYield;
        until (TSC.Connection.ReadBufferUsed >= 12) or (MicroTickDelta(T, GetMicroTick) >= 5000000);
        T := MicroTickDelta(T, GetMicroTick);
        DebugObj.Log('  Latency3_ClientToServer:' + IntToStr(T));
        Assert(TSC.Connection.ReadBufferUsed = 12);
        F := TSC.Connection.ReadByteString(12);
        Assert(F = 'Fundamentals');
        // read & write (small block): server to client
        T := GetMicroTick;
        TSC.Connection.WriteByteString('123');
        repeat
          DoYield;
        until (C.Connection.ReadBufferUsed >= 3) or (MicroTickDelta(T, GetMicroTick) >= 5000000);
        T := MicroTickDelta(T, GetMicroTick);
        DebugObj.Log('  Latency4_ServerToClient:' + IntToStr(T));
        F := C.Connection.ReadByteString(3);
        Assert(F = '123');
      end;
    TSC.ReleaseReference;
  finally
    C.Free;
    S.Free;
  end;

  FreeAndNil(DebugObj);
end;

procedure Test;
begin
  Test_ClientServer_ReadWrite;
  Test_ClientServer_StopStart;
  Test_ClientServer_Shutdown;
  Test_ClientServer_Block;
  Test_ClientServer_RetryConnect;
  Test_ClientServer_Block;
  Test_ClientServer_ReadWrite;
  Test_ClientServer_Latency;
end;
{$ENDIF}



end.

