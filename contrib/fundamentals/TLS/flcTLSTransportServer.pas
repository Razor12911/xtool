{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals TLS                                         }
{   File name:        flcTLSTransportServer.pas                                }
{   File version:     5.04                                                     }
{   Description:      TLS Transport Server                                     }
{                                                                              }
{   Copyright:        Copyright (c) 2008-2020, David J Butler                  }
{                     All rights reserved.                                     }
{                     Redistribution and use in source and binary forms, with  }
{                     or without modification, are permitted provided that     }
{                     the following conditions are met:                        }
{                     Redistributions of source code must retain the above     }
{                     copyright notice, this list of conditions and the        }
{                     following disclaimer.                                    }
{                     THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND   }
{                     CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED          }
{                     WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED   }
{                     WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A          }
{                     PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL     }
{                     THE REGENTS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,    }
{                     INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR             }
{                     CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,    }
{                     PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF     }
{                     USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)         }
{                     HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER   }
{                     IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING        }
{                     NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE   }
{                     USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE             }
{                     POSSIBILITY OF SUCH DAMAGE.                              }
{                                                                              }
{   Github:           https://github.com/fundamentalslib                       }
{   E-mail:           fundamentals.library at gmail.com                        }
{                                                                              }
{ Revision history:                                                            }
{                                                                              }
{   2010/12/02  0.01  Initial development.                                     }
{   2010/12/15  0.02  Development. Simple client server test case.             }
{   2016/01/08  0.03  String changes.                                          }
{   2018/07/17  5.04  Revised for Fundamentals 5.                              }
{   2020/05/19  5.08  Sign RSA authentication signature for DHE_RSA.           }
{                                                                              }
{******************************************************************************}

{$INCLUDE flcTLS.inc}

unit flcTLSTransportServer;

interface

uses
  { System }

  SyncObjs,

  { Utils }

  flcStdTypes,

  { Cipher }

  flcCipherRSA,
  flcCipherDH,

  { X509 }

  flcX509Certificate,

  { PEM }

  flcPEM,

  { TLS }

  flcTLSProtocolVersion,
  flcTLSAlgorithmTypes,
  flcTLSRandom,
  flcTLSCipherSuite,
  flcTLSAlert,
  flcTLSKeyExchangeParams,
  flcTLSCertificate,
  flcTLSKeys,
  flcTLSHandshake,
  flcTLSTransportTypes,
  flcTLSTransportConnection;



{                                                                              }
{ TLS Server                                                                   }
{                                                                              }
const
  DefaultTLSServerOptions            = [];
  DefaultTLSServerVersionOptions     = AllTLSVersionOptions - [tlsvoSSL3];
  DefaultTLSServerKeyExchangeOptions = AllTLSKeyExchangeOptions;
  DefaultTLSServerCipherOptions      = AllTLSCipherOptions;
  DefaultTLSServerHashOptions        = AllTLSHashOptions;



type
  TTLSServer = class;

  TTLSServerClientState = (
    tlsscInit,
    tlsscHandshakeAwaitingClientHello,
    tlsscHandshakeAwaitingClientKeyExchange,
    tlsscHandshakeAwaitingFinish,
    tlsscConnection
    );

  TTLSServerClient = class(TTLSConnection)
  protected
    FServer   : TTLSServer;
    FUserObj  : TObject;
    FClientId : Integer;

    FClientState          : TTLSServerClientState;
    FSessionID            : RawByteString;
    FCipherSuite          : TTLSCipherSuite;
    FCompression          : TTLSCompressionMethod;
    FClientHello          : TTLSClientHello;
    FClientHelloRandomStr : RawByteString;
    FServerHello          : TTLSServerHello;
    FServerHelloRandomStr : RawByteString;
    FServerKeyExchange    : TTLSServerKeyExchange;
    FClientKeyExchange    : TTLSClientKeyExchange;
    FPreMasterSecret      : RawByteString;
    FMasterSecret         : RawByteString;
    FDHState              : PDHState;

    procedure TriggerLog(const LogType: TTLSLogType; const LogMsg: String; const LogLevel: Integer); override;
    procedure TriggerConnectionStateChange; override;
    procedure TriggerAlert(const Level: TTLSAlertLevel; const Description: TTLSAlertDescription); override;
    procedure TriggerHandshakeFinished; override;

    procedure SetClientState(const State: TTLSServerClientState);

    procedure TransportLayerSendProc(const Sender: TTLSConnection; const Buffer; const Size: Integer);

    procedure SelectCompression(var Compression: TTLSCompressionMethod);
    procedure SelectCipherSuite(var CipherSuite: TTLSCipherSuite);

    procedure InitDHState;

    procedure InitProtocolVersion;
    procedure InitHandshakeServerHello;
    procedure InitHandshakeServerKeyExchange;

    procedure SendHandshakeHelloRequest;
    procedure SendHandshakeServerHello;
    procedure SendHandshakeCertificate;
    procedure SendHandshakeServerKeyExchange;
    procedure SendHandshakeCertificateRequest;
    procedure SendHandshakeServerHelloDone;
    procedure SendHandshakeFinished;

    procedure HandleHandshakeClientHello(const Buffer; const Size: Integer);
    procedure HandleHandshakeCertificateVerify(const Buffer; const Size: Integer);
    procedure HandleHandshakeClientKeyExchange(const Buffer; const Size: Integer);
    procedure HandleHandshakeFinished(const Buffer; const Size: Integer);
    procedure HandleHandshakeMessage(const MsgType: TTLSHandshakeType; const Buffer; const Size: Integer); override;

    procedure InitCipherSpecNone;
    procedure DoStart;

  public
    constructor Create(const AServer: TTLSServer; const AUserObj: TObject);
    destructor Destroy; override;

    property  UserObj: TObject read FUserObj;
    procedure Start;
  end;

  TTLSServerOptions = set of (
    tlssoNone
    );

  TTLSServerState = (
    tlssInit,
    tlssActive,
    tlssStopped
    );

  TTLSServerTransportLayerSendProc = procedure (Server: TTLSServer; Client: TTLSServerClient; const Buffer; const Size: Integer) of object;
  TTLSServerNotifyEvent = procedure (Sender: TTLSServer) of object;
  TTLSServerLogEvent = procedure (Sender: TTLSServer; LogType: TTLSLogType; LogMsg: String; LogLevel: Integer) of object;
  TTLSServerClientEvent = procedure (Sender: TTLSServer; Client: TTLSServerClient) of object;
  TTLSServerClientAlertEvent = procedure (Sender: TTLSServer; Client: TTLSServerClient; Level: TTLSAlertLevel; Description: TTLSAlertDescription) of object;

  TTLSServer = class
  protected
    FOnLog                     : TTLSServerLogEvent;
    FOnClientStateChange       : TTLSServerClientEvent;
    FOnClientAlert             : TTLSServerClientAlertEvent;
    FOnClientHandshakeFinished : TTLSServerClientEvent;
    FTransportLayerSendProc    : TTLSServerTransportLayerSendProc;
    FServerOptions             : TTLSServerOptions;
    FVersionOptions            : TTLSVersionOptions;
    FKeyExchangeOptions        : TTLSKeyExchangeOptions;
    FCipherOptions             : TTLSCipherOptions;
    FHashOptions               : TTLSHashOptions;
    FCertificateList           : TTLSCertificateList;
    FPrivateKeyRSA             : RawByteString;
    FPEMFileName               : String;
    FPEMText                   : RawByteString;
    FDHKeySize                 : Integer;

    FLock              : TCriticalSection;
    FState             : TTLSServerState;
    FClients           : array of TTLSServerClient;
    FClientNr          : Integer;
    FX509RSAPrivateKey : TX509RSAPrivateKey;
    FRSAPrivateKey     : TRSAPrivateKey;

    procedure Init; virtual;

    procedure Lock;
    procedure Unlock;

    procedure Log(const LogType: TTLSLogType; const LogMsg: String; const LogLevel: Integer = 0); overload; virtual;
    procedure Log(const LogType: TTLSLogType; const LogMsg: String; const Args: array of const; const LogLevel: Integer = 0); overload;

    procedure CheckNotActive;
    procedure CheckActive;

    procedure SetServerOptions(const AServerOptions: TTLSServerOptions);
    procedure SetVersionOptions(const AVersionOptions: TTLSVersionOptions);
    procedure SetCipherOptions(const ACipherOptions: TTLSCipherOptions);
    procedure SetKeyExchangeOptions(const AKeyExchangeOptions: TTLSKeyExchangeOptions);
    procedure SetHashOptions(const AHashOptions: TTLSHashOptions);

    procedure SetCertificateList(const List: TTLSCertificateList);
    procedure SetPrivateKeyRSA(const APrivateKeyRSA: RawByteString);
    function  GetPrivateKeyRSAPEM: RawByteString;
    procedure SetPrivateKeyRSAPEM(const APrivateKeyRSAPEM: RawByteString);
    procedure SetPEMFileName(const APEMFileName: String);
    procedure SetPEMText(const APEMText: RawByteString);
    procedure SetDHKeySize(const ADHKeySize: Integer);

    procedure ClientLog(const Client: TTLSServerClient; const LogType: TTLSLogType; const LogMsg: String; const LogLevel: Integer);
    procedure ClientStateChange(const Client: TTLSServerClient);
    procedure ClientAlert(const Client: TTLSServerClient; const Level: TTLSAlertLevel; const Description: TTLSAlertDescription);
    procedure ClientHandshakeFinished(const Client: TTLSServerClient);

    function  CreateClient(const UserObj: TObject): TTLSServerClient; virtual;
    function  GetClientCount: Integer;
    function  GetClient(const Idx: Integer): TTLSServerClient;
    function  GetClientIndex(const Client: TTLSServerClient): Integer;

    procedure ClientTransportLayerSend(const Sender: TTLSServerClient; const Buffer; const Size: Integer);

    procedure InitFromPEM;
    procedure InitPrivateKey;
    procedure AllocateSessionID(var SessionID: RawByteString);

    procedure DoStart;
    procedure DoStop;

  public
    constructor Create(const ATransportLayerSendProc: TTLSServerTransportLayerSendProc);
    destructor Destroy; override;

    property  OnLog: TTLSServerLogEvent read FOnLog write FOnLog;
    property  OnClientAlert: TTLSServerClientAlertEvent read FOnClientAlert write FOnClientAlert;
    property  OnClientStateChange: TTLSServerClientEvent read FOnClientStateChange write FOnClientStateChange;
    property  OnClientHandshakeFinished: TTLSServerClientEvent read FOnClientHandshakeFinished write FOnClientHandshakeFinished;

    property  ServerOptions: TTLSServerOptions read FServerOptions write SetServerOptions default DefaultTLSServerOptions;
    property  VersionOptions: TTLSVersionOptions read FVersionOptions write SetVersionOptions default DefaultTLSServerVersionOptions;
    property  KeyExchangeOptions: TTLSKeyExchangeOptions read FKeyExchangeOptions write SetKeyExchangeOptions default DefaultTLSServerKeyExchangeOptions;
    property  CipherOptions: TTLSCipherOptions read FCipherOptions write SetCipherOptions default DefaultTLSServerCipherOptions;
    property  HashOptions: TTLSHashOptions read FHashOptions write SetHashOptions default DefaultTLSServerHashOptions;

    property  CertificateList: TTLSCertificateList read FCertificateList write SetCertificateList;
    property  PrivateKeyRSA: RawByteString read FPrivateKeyRSA write SetPrivateKeyRSA;
    property  PrivateKeyRSAPEM: RawByteString read GetPrivateKeyRSAPEM write SetPrivateKeyRSAPEM;
    property  PEMFileName: String read FPEMFileName write SetPEMFileName;
    property  PEMText: RawByteString read FPEMText write SetPEMText;
    property  DHKeySize: Integer read FDHKeySize write SetDHKeySize;

    property  State: TTLSServerState read FState;
    procedure Start;
    procedure Stop;

    property  ClientCount: Integer read GetClientCount;
    property  Client[const Idx: Integer]: TTLSServerClient read GetClient;
    function  AddClient(const UserObj: TObject): TTLSServerClient;
    procedure RemoveClient(const Client: TTLSServerClient);

    procedure ProcessTransportLayerReceivedData(
              const AClient: TTLSServerClient;
              const Buffer; const Size: Integer);
  end;



implementation

uses
  { System }
  SysUtils,

  { Utils }
  flcBase64,
  flcHugeInt,

  { Cipher }
  flcCipherUtils,
  flcCipherRandom,

  { TLS }
  flcTLSErrors,
  flcTLSSessionID,
  flcTLSCipher;



{                                                                              }
{ TLS Server Client                                                            }
{                                                                              }
constructor TTLSServerClient.Create(const AServer: TTLSServer; const AUserObj: TObject);
begin
  Assert(Assigned(AServer));
  inherited Create(TransportLayerSendProc);
  FServer := AServer;
  FUserObj := AUserObj;
end;

destructor TTLSServerClient.Destroy;
begin
  if Assigned(FDHState) then
    begin
      DHStateFinalise(FDHState^);
      Dispose(FDHState);
      FDHState := nil;
    end;
  inherited Destroy;
end;

procedure TTLSServerClient.TriggerLog(const LogType: TTLSLogType; const LogMsg: String; const LogLevel: Integer);
begin
  inherited;
  FServer.ClientLog(self, LogType, LogMsg, LogLevel);
end;

procedure TTLSServerClient.TriggerConnectionStateChange;
begin
  inherited;
  FServer.ClientStateChange(self);
end;

procedure TTLSServerClient.TriggerAlert(const Level: TTLSAlertLevel; const Description: TTLSAlertDescription);
begin
  inherited;
  FServer.ClientAlert(self, Level, Description);
end;

procedure TTLSServerClient.TriggerHandshakeFinished;
begin
  inherited;
  FServer.ClientHandshakeFinished(self);
end;

procedure TTLSServerClient.SetClientState(const State: TTLSServerClientState);
begin
  FClientState := State;
end;

procedure TTLSServerClient.TransportLayerSendProc(const Sender: TTLSConnection; const Buffer; const Size: Integer);
begin
  FServer.ClientTransportLayerSend(self, Buffer, Size);
end;

procedure TTLSServerClient.SelectCompression(var Compression: TTLSCompressionMethod);
begin
  Compression := tlscmNull;
end;

procedure TTLSServerClient.SelectCipherSuite(var CipherSuite: TTLSCipherSuite);
var
  I : TTLSCipherSuite;
  C : PTLSCipherSuiteInfo;
begin
  for I := High(TTLSCipherSuite) downto Low(TTLSCipherSuite) do
    if (I <> tlscsNone) and (I in FClientHello.CipherSuites) then
      begin
        C := @TLSCipherSuiteInfo[I];
        if C^.ServerSupport then
          begin
            CipherSuite := I;
            exit;
          end;
      end;
  CipherSuite := tlscsNone;
end;

procedure TTLSServerClient.InitDHState;
begin
  New(FDHState);
  DHStateInit(FDHState^);

  (*
  P = 'FFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD129024E088A67CC74020BBEA63B139B22514A08798E3404DDEF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245E485B576625E7EC6F44C42E9A63A3620FFFFFFFFFFFFFFFF'
  G = '00000002'
  *)

{  HugeWordToHexB(FDHState^.Q);
  HugeWordToHexB(FDHState^.X);
  HugeWordToHexB(FDHState^.Y);
}

(*
  DHInitHashAlgorithm(FDHState^, dhhSHA1);

  HexToHugeWordB('FFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD129024E088A67CC74020BBEA63B139B22514A08798E3404DDEF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245E485B576625E7EC6F44C42E9A63A3620FFFFFFFFFFFFFFFF',
      FDHState^.P);
  HexToHugeWordB('00000002', FDHState^.G);

  HexToHugeWordB('AF2FCC0B5ADD8F623266378ADBF72664D5913FFF', FDHState^.Q);
  HexToHugeWordB('8E62C571E6ECCC720CF65E432FA779BE56EFAB46', FDHState^.X);
  HexToHugeWordB('A37CCB97CAB7AC5C3C2B59FC7C33007B2CDD9FA6C616D8BA4A202EC901FE5F37F790295D5DAA990E22121C154B78E2201B36AB4A8944E61D34ADA541221653B49E432B782A5A59806E87ADBCC57033385FB9197CEC137170DD80466D82F431BA',
      FDHState^.Y);

  FDHState^.PrimePBitCount := DHWellKnownGroup[0].PBitCount;
  FDHState^.PrimeQBitCount := DHWellKnownGroup[0].QBitCount;
*)

  //DHGenerateKeys(FDHState^, dhhSHA1, DHQBitCount(FServer.FDHKeySize), FServer.FDHKeySize);
  //// Use well known pairs
  //// x/prikey is a random nr

  HexToHugeWordB(DHWellKnownGroup[0].P_Hex, FDHState^.P);
  HexToHugeWordB(DHWellKnownGroup[0].G_Hex, FDHState^.G);
  DHDeriveKeysFromGroupParametersPG(
      FDHState^, dhhSHA1, DHWellKnownGroup[0].QBitCount, DHWellKnownGroup[0].PBitCount,
      FDHState^.P, FDHState^.G);
end;

procedure TTLSServerClient.InitProtocolVersion;
begin
  FProtocolVersion := FClientHello.ProtocolVersion;
  if IsSSL2(FProtocolVersion) then
    raise ETLSError.CreateAlertBadProtocolVersion; // SSL2 not supported
  if IsPostTLS12(FProtocolVersion) then
    FProtocolVersion := TLSProtocolVersion12;
  if not IsKnownTLSVersion(FProtocolVersion) then
    raise ETLSError.CreateAlertBadProtocolVersion; // unknown SSL version
end;

procedure TTLSServerClient.InitHandshakeServerHello;
begin
  InitTLSServerHello(FServerHello,
      FProtocolVersion,
      FSessionID,
      FCipherSpecNew.CipherSuiteDetails.CipherSuiteInfo^.Rec,
      FCompression);

  {$IFDEF TLS_TEST_NO_RANDOM_HELLO}
  FClientHello.Random.gmt_unix_time := 123;
  FillChar(FServerHello.Random.random_bytes, 28, 117);
  {$ENDIF}

  FServerHelloRandomStr := TLSRandomToStr(FServerHello.Random);
end;

procedure TTLSServerClient.InitHandshakeServerKeyExchange;
begin
  InitTLSServerKeyExchange(FServerKeyExchange);

  case FCipherSpecNew.KeyExchangeAlgorithm of
    tlskeaDHE_DSS,
    tlskeaDHE_RSA,
    tlskeaDH_Anon :
      begin
        InitDHState;

        AssignTLSServerKeyExchangeDHParams(
            FServerKeyExchange,
            DHHugeWordKeyEncodeBytes(FDHState^.P),
            DHHugeWordKeyEncodeBytes(FDHState^.G),
            DHHugeWordKeyEncodeBytes(FDHState^.Y));  /////////

        SignTLSServerKeyExchangeDH_RSA(
            FServerKeyExchange,
            PTLSClientServerRandom(@FClientHello.Random)^,
            PTLSClientServerRandom(@FServerHello.Random)^,
            FServer.FRSAPrivateKey);
      end;
  end;
end;

const
  MaxHandshakeHelloRequestSize       = 2048;
  MaxHandshakeServerHelloSize        = 2048;
  MaxHandshakeCertificateSize        = 65536;
  MaxHandshakeServerKeyExchangeSize  = 16384;
  MaxHandshakeCertificateRequestSize = 16384;
  MaxHandshakeServerHelloDoneSize    = 16384;
  MaxHandshakeFinishedSize           = 2048;

procedure TTLSServerClient.SendHandshakeHelloRequest;
var
  Buf  : array[0..MaxHandshakeHelloRequestSize - 1] of Byte;
  Size : Integer;
begin
  Size := EncodeTLSHandshakeHelloRequest(Buf, SizeOf(Buf));
  SendHandshake(Buf, Size);
end;

procedure TTLSServerClient.SendHandshakeServerHello;
var
  Buf  : array[0..MaxHandshakeServerHelloSize - 1] of Byte;
  Size : Integer;
begin
  InitHandshakeServerHello;
  Size := EncodeTLSHandshakeServerHello(Buf, SizeOf(Buf), FServerHello);
  SendHandshake(Buf, Size);
end;

procedure TTLSServerClient.SendHandshakeCertificate;
var
  Buf  : array[0..MaxHandshakeCertificateSize - 1] of Byte;
  Size : Integer;
begin
  Size := EncodeTLSHandshakeCertificate(Buf, SizeOf(Buf), FServer.FCertificateList);
  SendHandshake(Buf, Size);
end;

procedure TTLSServerClient.SendHandshakeServerKeyExchange;
var
  Buf  : array[0..MaxHandshakeServerKeyExchangeSize - 1] of Byte;
  Size : Integer;
begin
  InitHandshakeServerKeyExchange;
  Size := EncodeTLSHandshakeServerKeyExchange(Buf, SizeOf(Buf),
      FCipherSpecNew.KeyExchangeAlgorithm, FServerKeyExchange);
  SendHandshake(Buf, Size);
end;

procedure TTLSServerClient.SendHandshakeCertificateRequest;
var
  Buf  : array[0..MaxHandshakeCertificateRequestSize - 1] of Byte;
  Size : Integer;
  CReq : TTLSCertificateRequest;
begin
  Size := EncodeTLSHandshakeCertificateRequest(Buf, SizeOf(Buf), CReq);
  SendHandshake(Buf, Size);
end;

procedure TTLSServerClient.SendHandshakeServerHelloDone;
var
  Buf  : array[0..MaxHandshakeServerHelloDoneSize - 1] of Byte;
  Size : Integer;
begin
  Size := EncodeTLSHandshakeServerHelloDone(Buf, SizeOf(Buf));
  SendHandshake(Buf, Size);
end;

procedure TTLSServerClient.SendHandshakeFinished;
var
  Buf  : array[0..MaxHandshakeFinishedSize - 1] of Byte;
  Size : Integer;
begin
  Size := EncodeTLSHandshakeFinished(Buf, SizeOf(Buf), FMasterSecret, FProtocolVersion, FVerifyHandshakeData, False);
  SendHandshake(Buf, Size);
end;

procedure TTLSServerClient.HandleHandshakeClientHello(const Buffer; const Size: Integer);
begin
  if FClientState <> tlsscHandshakeAwaitingClientHello then
    raise ETLSError.CreateAlertUnexpectedMessage;

  DecodeTLSClientHello(Buffer, Size, FClientHello);
  FClientHelloRandomStr := TLSRandomToStr(FClientHello.Random);
  {$IFDEF TLS_DEBUG}
  Log(tlsltDebug, 'ClientHello:%s', [TLSProtocolVersionName(FClientHello.ProtocolVersion)]);
  {$ENDIF}

  InitProtocolVersion;
  {$IFDEF TLS_DEBUG}
  Log(tlsltDebug, 'ProtocolVersion:%s', [TLSProtocolVersionName(FProtocolVersion)]);
  {$ENDIF}

  SelectCompression(FCompression);
  SelectCipherSuite(FCipherSuite);
  {$IFDEF TLS_DEBUG}
  Log(tlsltDebug, 'CipherSuite:%s', [TLSCipherSuiteInfo[FCipherSuite].Name]);
  {$ENDIF}
  if FCipherSuite = tlscsNone then
    raise ETLSError.CreateAlert(TLSError_BadProtocol, tlsadHandshake_failure);
  InitTLSSecurityParameters(FCipherSpecNew, FCompression, FCipherSuite);

  SetClientState(tlsscHandshakeAwaitingClientKeyExchange);

  SendHandshakeServerHello;
  SendHandshakeCertificate;

  { The ServerKeyExchange message is sent by the server only when the ServerCertificate message (if sent) }
  { does not contain enough data to allow the user-agent to exchange a premaster secret. }
  { This is true for the following key exchange methods: }
  { RSA_EXPORT (if the public key in the server certificate is longer than 512 bits) }
  { DHE_DSS, DHE_DSS_EXPORT, DHE_RSA, DHE_RSA_EXPORT, DH_anon }
  { It is not legal to send the server key exchange message for the following key exchange methods: }
  { RSA, DH_DSS, DH_RSA, }
  { RSA_EXPORT (when the public key in the server certificate is less than or equal to 512 bits in length) }
  { Additionally, a ServerKeyExchange message may be sent, if it is required (e.g., if the server has no }
  { certificate, or if its certificate is for signing only).  If the server is authenticated, it may }
  { request a certificate from the client. }

  if FCipherSpecNew.KeyExchangeAlgorithm in [tlskeaDHE_DSS, tlskeaDHE_RSA, tlskeaDH_Anon] then
    SendHandshakeServerKeyExchange;

  SendHandshakeServerHelloDone;
end;

procedure TTLSServerClient.HandleHandshakeCertificateVerify(const Buffer; const Size: Integer);
begin
  if FClientState <> tlsscHandshakeAwaitingClientKeyExchange then
    raise ETLSError.CreateAlertUnexpectedMessage;
end;

procedure TTLSServerClient.HandleHandshakeClientKeyExchange(const Buffer; const Size: Integer);
var
  Yc : HugeWord;
  ZZ : HugeWord;
begin
  if FClientState <> tlsscHandshakeAwaitingClientKeyExchange then
    raise ETLSError.CreateAlertUnexpectedMessage;

  DecodeTLSClientKeyExchange(Buffer, Size,
      FCipherSpecNew.KeyExchangeAlgorithm,
      True, FClientKeyExchange);

  // pre-master-secret
  case FCipherSpecNew.KeyExchangeAlgorithm of
    tlskeaNone : ;
    tlskeaNULL : ;
    tlskeaRSA  :
      begin
        { RFC 5246: When RSA is used for server authentication and key exchange, a 48- }
        { byte pre_master_secret is generated by the client, encrypted under }
        { the server's public key, and sent to the server.  The server uses its }
        { private key to decrypt the pre_master_secret.  Both parties then }
        { convert the pre_master_secret into the master_secret, as specified above. }

        FPreMasterSecret := RSADecryptStr(rsaetRSAES_PKCS1, FServer.FRSAPrivateKey,
            FClientKeyExchange.EncryptedPreMasterSecret);
      end;
    tlskeaDHE_DSS,
    tlskeaDHE_RSA,
    tlskeaDH_Anon :
      begin
        Assert(Assigned(FDHState));

        { RFC 5246: A conventional Diffie-Hellman computation is performed.  The }
        { negotiated key (Z) is used as the pre_master_secret, and is converted }
        { into the master_secret, as specified above.  Leading bytes of Z that }
        { contain all zero bits are stripped before it is used as the }
        { pre_master_secret }

        HugeWordInit(Yc);
        HugeWordInit(ZZ);
        DHHugeWordKeyDecodeBytes(Yc, FClientKeyExchange.ClientDiffieHellmanPublic.dh_Yc);
        HugeWordPowerAndMod(ZZ, Yc, FDHState^.X, FDHState^.P);

        FPreMasterSecret := DHHugeWordKeyEncodeBytes(ZZ);

        HugeWordFinalise(ZZ);
        HugeWordFinalise(Yc);
      end;
  else
    raise ETLSError.CreateAlert(TLSError_BadProtocol, tlsadHandshake_failure);
  end;

  case FCipherSpecNew.KeyExchangeAlgorithm of
    tlskeaRSA,
    tlskeaDHE_DSS,
    tlskeaDHE_RSA,
    tlskeaDH_Anon :
      begin
        Assert(FPreMasterSecret <> '');
        FMasterSecret := TLSMasterSecret(FProtocolVersion, FPreMasterSecret,
            FClientHelloRandomStr, FServerHelloRandomStr);
        SecureClearStr(FPreMasterSecret);

        GenerateTLSKeys(FProtocolVersion,
            FCipherSpecNew.CipherSuiteDetails.HashInfo^.KeyLength,
            FCipherSpecNew.CipherSuiteDetails.CipherInfo^.KeyBits,
            FCipherSpecNew.CipherSuiteDetails.CipherInfo^.IVSize * 8,
            FMasterSecret,
            FServerHelloRandomStr,
            FClientHelloRandomStr,
            FKeys);
        GenerateFinalTLSKeys(FProtocolVersion,
            FCipherSpecNew.CipherSuiteDetails.CipherInfo^.Exportable,
            FCipherSpecNew.CipherSuiteDetails.CipherInfo^.ExpKeyMat * 8,
            FServerHelloRandomStr,
            FClientHelloRandomStr,
            FKeys);

        SetEncodeKeys(FKeys.ServerMACKey, FKeys.ServerEncKey, FKeys.ServerIV);
        SetDecodeKeys(FKeys.ClientMACKey, FKeys.ClientEncKey, FKeys.ClientIV);
      end;
  end;

  SetClientState(tlsscHandshakeAwaitingFinish);
end;

procedure TTLSServerClient.HandleHandshakeFinished(const Buffer; const Size: Integer);
begin
  if FClientState <> tlsscHandshakeAwaitingFinish then
    raise ETLSError.CreateAlertUnexpectedMessage;

  SendChangeCipherSpec;
  ChangeEncryptCipherSpec;
  SetClientState(tlsscConnection);
  SetConnectionState(tlscoApplicationData);
  SendHandshakeFinished;
  TriggerHandshakeFinished;
end;

procedure TTLSServerClient.HandleHandshakeMessage(const MsgType: TTLSHandshakeType; const Buffer; const Size: Integer);
begin
  {$IFDEF TLS_DEBUG}
  Log(tlsltDebug, 'R:Handshake:%s:%db', [TLSHandshakeTypeToStr(MsgType), Size]);
  {$ENDIF}

  case MsgType of
    tlshtHello_request       : ShutdownBadProtocol(tlsadUnexpected_message);
    tlshtClient_hello        : HandleHandshakeClientHello(Buffer, Size);
    tlshtServer_hello        : ShutdownBadProtocol(tlsadUnexpected_message);
    tlshtCertificate         : ShutdownBadProtocol(tlsadUnexpected_message);
    tlshtServer_key_exchange : ShutdownBadProtocol(tlsadUnexpected_message);
    tlshtCertificate_request : ShutdownBadProtocol(tlsadUnexpected_message);
    tlshtServer_hello_done   : ShutdownBadProtocol(tlsadUnexpected_message);
    tlshtCertificate_verify  : HandleHandshakeCertificateVerify(Buffer, Size);
    tlshtClient_key_exchange : HandleHandshakeClientKeyExchange(Buffer, Size);
    tlshtFinished            : HandleHandshakeFinished(Buffer, Size);
  else
    ShutdownBadProtocol(tlsadUnexpected_message);
  end;
end;

procedure TTLSServerClient.InitCipherSpecNone;
begin
  InitTLSSecurityParametersNone(FCipherEncryptSpec);
  InitTLSSecurityParametersNone(FCipherDecryptSpec);
  TLSCipherInitNone(FCipherEncryptState, tlscoEncrypt);
  TLSCipherInitNone(FCipherDecryptState, tlscoDecrypt);
end;

procedure TTLSServerClient.DoStart;
begin
  SetConnectionState(tlscoStart);
  InitCipherSpecNone;
  SetConnectionState(tlscoHandshaking);
  SetClientState(tlsscHandshakeAwaitingClientHello);
  FServer.AllocateSessionID(FSessionID);
end;

procedure TTLSServerClient.Start;
begin
  Assert(FConnectionState = tlscoInit);
  Assert(FClientState = tlsscInit);
  DoStart;
end;



{                                                                              }
{ TLS Server                                                                   }
{                                                                              }
constructor TTLSServer.Create(const ATransportLayerSendProc: TTLSServerTransportLayerSendProc);
begin
  inherited Create;
  Init;
  if not Assigned(ATransportLayerSendProc) then
    raise ETLSError.Create(TLSError_InvalidParameter);
  FTransportLayerSendProc := ATransportLayerSendProc;
end;

procedure TTLSServer.Init;
begin
  FServerOptions      := DefaultTLSServerOptions;
  FVersionOptions     := DefaultTLSServerVersionOptions;
  FKeyExchangeOptions := DefaultTLSServerKeyExchangeOptions;
  FCipherOptions      := DefaultTLSServerCipherOptions;
  FHashOptions        := DefaultTLSServerHashOptions;

  FState := tlssInit;
  FLock := TCriticalSection.Create;
  RSAPrivateKeyInit(FRSAPrivateKey);
  FDHKeySize := 1024;
end;

destructor TTLSServer.Destroy;
var I : Integer;
begin
  for I := Length(FClients) - 1 downto 0 do
    FreeAndNil(FClients[I]);
  RSAPrivateKeyFinalise(FRSAPrivateKey);
  FreeAndNil(FLock);
  inherited Destroy;
end;

procedure TTLSServer.Lock;
begin
  Assert(Assigned(FLock));
  FLock.Acquire;
end;

procedure TTLSServer.Unlock;
begin
  FLock.Release;
end;

procedure TTLSServer.Log(const LogType: TTLSLogType; const LogMsg: String; const LogLevel: Integer);
begin
  if Assigned(FOnLog) then
    FOnLog(self, LogType, LogMsg, LogLevel);
end;

procedure TTLSServer.Log(const LogType: TTLSLogType; const LogMsg: String; const Args: array of const; const LogLevel: Integer);
begin
  Log(LogType, Format(LogMsg, Args), LogLevel);
end;

procedure TTLSServer.CheckNotActive;
begin
  if FState = tlssActive then
    raise ETLSError.Create(TLSError_InvalidState, 'Operation not allowed while active');
end;

procedure TTLSServer.CheckActive;
begin
  if FState <> tlssActive then
    raise ETLSError.Create(TLSError_InvalidState, 'Operation not allowed while not active');
end;

procedure TTLSServer.SetServerOptions(const AServerOptions: TTLSServerOptions);
begin
  if AServerOptions = FServerOptions then
    exit;
  CheckNotActive;
  FServerOptions := AServerOptions;
end;

procedure TTLSServer.SetVersionOptions(const AVersionOptions: TTLSVersionOptions);
begin
  if AVersionOptions = FVersionOptions then
    exit;
  CheckNotActive;
  if AVersionOptions = [] then
    raise ETLSError.Create(TLSError_InvalidParameter, 'Invalid version options');
  FVersionOptions := AVersionOptions;
end;

procedure TTLSServer.SetCipherOptions(const ACipherOptions: TTLSCipherOptions);
begin
  if ACipherOptions = FCipherOptions then
    exit;
  CheckNotActive;
  if ACipherOptions = [] then
    raise ETLSError.Create(TLSError_InvalidParameter, 'Invalid cipher options');
  FCipherOptions := ACipherOptions;
end;

procedure TTLSServer.SetKeyExchangeOptions(const AKeyExchangeOptions: TTLSKeyExchangeOptions);
begin
  if AKeyExchangeOptions = FKeyExchangeOptions then
    exit;
  CheckNotActive;
  if AKeyExchangeOptions = [] then
    raise ETLSError.Create(TLSError_InvalidParameter, 'Invalid key exchange options');
  FKeyExchangeOptions := AKeyExchangeOptions;
end;

procedure TTLSServer.SetHashOptions(const AHashOptions: TTLSHashOptions);
begin
  if AHashOptions = FHashOptions then
    exit;
  CheckNotActive;
  if AHashOptions = [] then
    raise ETLSError.Create(TLSError_InvalidParameter, 'Invalid hash options');
  FHashOptions := AHashOptions;
end;

procedure TTLSServer.SetCertificateList(const List: TTLSCertificateList);
begin
  CheckNotActive;
  FCertificateList := Copy(List);
end;

procedure TTLSServer.SetPrivateKeyRSA(const APrivateKeyRSA: RawByteString);
begin
  if APrivateKeyRSA = FPrivateKeyRSA then
    exit;
  CheckNotActive;
  FPrivateKeyRSA := APrivateKeyRSA;
end;

function TTLSServer.GetPrivateKeyRSAPEM: RawByteString;
begin
  Result := MIMEBase64Encode(PrivateKeyRSA);
end;

procedure TTLSServer.SetPrivateKeyRSAPEM(const APrivateKeyRSAPEM: RawByteString);
begin
  SetPrivateKeyRSA(MIMEBase64Decode(APrivateKeyRSAPEM));
end;

procedure TTLSServer.SetPEMFileName(const APEMFileName: String);
begin
  if APEMFileName = FPEMFileName then
    exit;
  CheckNotActive;
  FPEMFileName := APEMFileName;
end;

procedure TTLSServer.SetPEMText(const APEMText: RawByteString);
begin
  if APEMText = FPEMText then
    exit;
  CheckNotActive;
  FPEMText := APEMText;
end;

procedure TTLSServer.SetDHKeySize(const ADHKeySize: Integer);
begin
  if ADHKeySize = FDHKeySize then
    exit;
  CheckNotActive;
  FDHKeySize := ADHKeySize;
end;

procedure TTLSServer.ClientLog(const Client: TTLSServerClient; const LogType: TTLSLogType; const LogMsg: String; const LogLevel: Integer);
begin
  Log(LogType, 'C[%d]:%s', [Client.FClientId, LogMsg], LogLevel + 1);
end;

procedure TTLSServer.ClientStateChange(const Client: TTLSServerClient);
begin
  if Assigned(FOnClientStateChange) then
    FOnClientStateChange(self, Client);
end;

procedure TTLSServer.ClientAlert(const Client: TTLSServerClient; const Level: TTLSAlertLevel; const Description: TTLSAlertDescription);
begin
  if Assigned(FOnClientAlert) then
    FOnClientAlert(self, Client, Level, Description);
end;

procedure TTLSServer.ClientHandshakeFinished(const Client: TTLSServerClient);
begin
  if Assigned(FOnClientHandshakeFinished) then
    FOnClientHandshakeFinished(self, Client);
end;

function TTLSServer.CreateClient(const UserObj: TObject): TTLSServerClient;
begin
  Result := TTLSServerClient.Create(self, UserObj);
end;

function TTLSServer.GetClientCount: Integer;
begin
  Result := Length(FClients);
end;

function TTLSServer.GetClient(const Idx: Integer): TTLSServerClient;
begin
  Assert(Idx >= 0);
  Assert(Idx < Length(FClients));

  Result := FClients[Idx];
end;

function TTLSServer.GetClientIndex(const Client: TTLSServerClient): Integer;
var I : Integer;
begin
  for I := 0 to Length(FClients) - 1 do
    if FClients[I] = Client then
      begin
        Result := I;
        exit;
      end;
  Result := -1;
end;

function TTLSServer.AddClient(const UserObj: TObject): TTLSServerClient;
var L : Integer;
    C : TTLSServerClient;
begin
  CheckActive;
  C := CreateClient(UserObj);
  Lock;
  try
    Inc(FClientNr);
    C.FClientId := FClientNr;

    L := Length(FClients);
    SetLength(FClients, L + 1);
    FClients[L] := C;
  finally
    Unlock;
  end;
  Result := C;
end;

procedure TTLSServer.RemoveClient(const Client: TTLSServerClient);
var I, J, L : Integer;
begin
  Lock;
  try
    I := GetClientIndex(Client);
    if I < 0 then
      raise ETLSError.Create(TLSError_InvalidParameter);
    L := Length(FClients);
    for J := I to L - 2 do
      FClients[J] := FClients[J + 1];
    SetLength(FClients, L - 1);
  finally
    Unlock;
  end;
  Client.Free;
end;

procedure TTLSServer.ClientTransportLayerSend(const Sender: TTLSServerClient; const Buffer; const Size: Integer);
begin
  Assert(Assigned(FTransportLayerSendProc));
  Assert(Size > 0);
  FTransportLayerSendProc(self, Sender, Buffer, Size);
end;

procedure TTLSServer.ProcessTransportLayerReceivedData(const AClient: TTLSServerClient; const Buffer; const Size: Integer);
begin
  if not Assigned(AClient) then
    raise ETLSError.Create(TLSError_InvalidParameter);
  AClient.ProcessTransportLayerReceivedData(Buffer, Size);
end;

procedure TTLSServer.InitFromPEM;
var P : TPEMFile;
    L, I : Integer;
begin
  if (FPEMFileName = '') and (FPEMText = '') then
    exit;
  P := TPEMFile.Create;
  try
    if FPEMFileName <> '' then
      P.LoadFromFile(FPEMFileName)
    else
      P.LoadFromText(FPEMText);
    FPrivateKeyRSA := P.RSAPrivateKey;
    L := P.CertificateCount;
    SetLength(FCertificateList, L);
    for I := 0 to L - 1 do
      FCertificateList[I] := P.Certificate[I];
  finally
    P.Free;
  end;
end;

procedure TTLSServer.InitPrivateKey;
var L1, L2 : Integer;
begin
  if FPrivateKeyRSA = '' then
    raise ETLSError.Create(TLSError_InvalidCertificate, 'No private key');
  ParseX509RSAPrivateKeyStr(FPrivateKeyRSA, FX509RSAPrivateKey);
  L1 := NormaliseX509IntKeyBuf(FX509RSAPrivateKey.Modulus);
  L2 := NormaliseX509IntKeyBuf(FX509RSAPrivateKey.PrivateExponent);
  if L2 > L1 then
    L1 := L2;
  RSAPrivateKeyAssignBufStr(FRSAPrivateKey, L1 * 8,
      FX509RSAPrivateKey.Modulus,
      FX509RSAPrivateKey.PrivateExponent);
end;

procedure TTLSServer.AllocateSessionID(var SessionID: RawByteString);
begin
  SessionID := SecureRandomStrA(TLSSessionIDMaxLen);
end;

procedure TTLSServer.DoStart;
begin
  Assert(FState <> tlssActive);

  {$IFDEF TLS_DEBUG}
  Log(tlsltDebug, 'Start');
  {$ENDIF}

  FClientNr := 0;
  InitFromPEM;
  InitPrivateKey;
  FState := tlssActive;
end;

procedure TTLSServer.DoStop;
begin
  Assert(FState = tlssActive);

  {$IFDEF TLS_DEBUG}
  Log(tlsltDebug, 'Stop');
  {$ENDIF}

  FState := tlssStopped;
end;

procedure TTLSServer.Start;
begin
  if FState = tlssActive then
    exit;
  DoStart;
end;

procedure TTLSServer.Stop;
begin
  if FState <> tlssActive then
    exit;
  DoStop;
end;



end.

