{   2020/05/11  5.01  Move tests from unit flcTests into seperate units.       }

{$INCLUDE flcTCPTest.inc}

{$IFDEF TCPCLIENT_TEST_TLS}
  {$DEFINE TCPCLIENT_TEST_TLS_WEB}
{$ENDIF}

unit flcTCPTest_ClientTLS;

interface



{                                                                              }
{ Test                                                                         }
{                                                                              }
{$IFDEF TCPCLIENT_TEST_TLS}
procedure Test;
{$ENDIF}



implementation

{$IFDEF TCPCLIENT_TEST_TLS}
uses
  SysUtils,

  flcTLSTransportTypes,
  flcTLSTransportConnection,
  flcTLSTransportClient,

  flcTCPConnection,
  flcTCPClient,
  flcTCPTest_Client;
{$ENDIF}



{                                                                              }
{ Test                                                                         }
{                                                                              }
{$IFDEF TCPCLIENT_TEST_TLS}
{$ASSERTIONS ON}

{$IFDEF TCPCLIENT_TEST_TLS_WEB}
const
  TLSWebTestHost = 'www.rfc.org'; // DHE_RSA_WITH_AES_256_CBC_SHA

procedure Test_Client_TLS_Web(
          const TLSOptions: TTCPClientTLSOptions                       = [];
          const TLSClientOptions: TTCPClientTLSClientOptions           = DefaultTLSClientOptions;
          const TLSVersionOptions: TTCPClientTLSVersionOptions         = DefaultTLSClientVersionOptions;
          const TLSKeyExchangeOptions: TTCPClientTLSKeyExchangeOptions = DefaultTLSClientKeyExchangeOptions;
          const TLSCipherOptions: TTCPClientTLSCipherOptions           = DefaultTLSClientCipherOptions;
          const TLSHashOptions: TTCPClientTLSHashOptions               = DefaultTLSClientHashOptions
          );
var
  C : TF5TCPClient;
  I, L : Integer;
  S : RawByteString;
  A : TTCPClientTestObj;
begin
  A := TTCPClientTestObj.Create;
  C := TF5TCPClient.Create(nil);
  try
    // init
    C.OnLog := A.ClientLog;
    C.TLSEnabled := True;
    C.TLSOptions := TLSOptions;
    C.TLSClientOptions := TLSClientOptions;
    C.TLSVersionOptions := TLSVersionOptions;
    C.TLSKeyExchangeOptions := TLSKeyExchangeOptions;
    C.TLSCipherOptions := TLSCipherOptions;
    C.TLSHashOptions := TLSHashOptions;
    C.LocalHost := '0.0.0.0';
    C.Host := TLSWebTestHost;
    C.Port := '443';
    // start
    C.Active := True;
    Assert(C.Active);
    // wait connect
    I := 0;
    repeat
      Sleep(1);
      Inc(I);
    until (
            (C.State in [csReady, csClosed]) and
            (C.TLSClient.ConnectionState in [tlscoApplicationData, tlscoErrorBadProtocol, tlscoCancelled, tlscoClosed]) and
            (C.Connection.State = cnsConnected)
          ) or
          (I = 5000);
    Assert(C.State = csReady);
    Assert(C.Connection.State = cnsConnected);
    Assert(C.TLSClient.ConnectionState = tlscoApplicationData);
    // send
    S :=
        'GET / HTTP/1.1'#13#10 +
        'Host: ' + TLSWebTestHost + #13#10 +
        'Date: 11 Oct 2011 12:34:56 GMT'#13#10 +
        #13#10;
    C.Connection.Write(S[1], Length(S));
    C.BlockingConnection.WaitForTransmitFin(5000);
    // read
    C.BlockingConnection.WaitForReceiveData(1, 5000);
    L := C.Connection.ReadBufferUsed;
    Assert(L > 0);
    SetLength(S, L);
    Assert(C.Connection.Read(S[1], L) = L);
    Assert(Copy(S, 1, 6) = 'HTTP/1');
    // close
    C.BlockingConnection.Shutdown(2000, 2000, 5000);
    Assert(C.Connection.State = cnsClosed);
    // stop
    C.Active := False;
    Assert(not C.Active);
  finally
    C.Finalise;
    FreeAndNil(C);
    A.Free;
  end;
end;

procedure TestClientTLSWeb;
begin
  //Test_Client_TLS_Web([ctoDisableTLS10, ctoDisableTLS11, ctoDisableTLS12]); // SSL 3
  //Test_Client_TLS_Web([ctoDisableSSL3,  ctoDisableTLS11, ctoDisableTLS12]); // TLS 1.0
  //Test_Client_TLS_Web([ctoDisableSSL3,  ctoDisableTLS10, ctoDisableTLS12]); // TLS 1.1

  // TLS 1.2
  {Test_Client_TLS_Web(
      DefaultTCPClientTLSOptions,
      DefaultTLSClientOptions,
      [tlsvoTLS12],
      DefaultTLSClientKeyExchangeOptions,
      DefaultTLSClientCipherOptions,
      DefaultTLSClientHashOptions);}

  // TLS 1.2
  Test_Client_TLS_Web(
      DefaultTCPClientTLSOptions,
      DefaultTLSClientOptions,
      [tlsvoTLS12],
      [tlskeoDHE_RSA],
      DefaultTLSClientCipherOptions,
      DefaultTLSClientHashOptions);

{  Test_Client_TLS_Web(
      DefaultTCPClientTLSOptions,
      DefaultTLSClientOptions,
      [tlsvoTLS12],
      [tlskeoRSA],
      [tlsco3DES],
      DefaultTLSClientHashOptions); }

  //Test_Client_TLS_Web([]);
end;
{$ENDIF}

procedure Test;
begin
  {$IFDEF TCPCLIENT_TEST_TLS_WEB}
  TestClientTLSWeb;
  {$ENDIF}
end;
{$ENDIF}



end.

