{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals TLS                                         }
{   File name:        flcTLSTransportClient.pas                                }
{   File version:     5.08                                                     }
{   Description:      TLS Transport Client                                     }
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
{   2008/01/18  0.01  Initial development.                                     }
{   2010/11/26  0.02  Protocol messages.                                       }
{   2010/11/30  0.03  Encrypted messages.                                      }
{   2010/12/03  0.04  Connection base class.                                   }
{   2016/01/08  0.05  String changes.                                          }
{   2018/07/17  5.06  Revised for Fundamentals 5.                              }
{   2020/05/11  5.07  VersionOptions, KeyExchangeOptions, CipherOptions and    }
{                     HashOptions.                                             }
{   2020/05/19  5.08  Verify RSA authentication signature for DHE_RSA.         }
{                                                                              }
{******************************************************************************}

{$INCLUDE flcTLS.inc}

unit flcTLSTransportClient;

interface

uses
  SysUtils,

  { Utils }

  flcStdTypes,

  { Cipher }

  flcCipherRSA,
  flcCipherDH,

  { X509 }

  flcX509Certificate,

  { TLS }

  flcTLSProtocolVersion,
  flcTLSAlgorithmTypes,
  flcTLSRandom,
  flcTLSCipherSuite,
  flcTLSRecord,
  flcTLSAlert,
  flcTLSKeyExchangeParams,
  flcTLSCertificate,
  flcTLSKeys,
  flcTLSHandshake,
  flcTLSTransportTypes,
  flcTLSTransportConnection;



{                                                                              }
{ TLS Client                                                                   }
{                                                                              }
type
  TTLSClientOption = (
      tlscloNone
    );

  TTLSClientOptions = set of TTLSClientOption;

const
  DefaultTLSClientOptions            = [];
  DefaultTLSClientVersionOptions     = AllTLSVersionOptions - [tlsvoSSL3];
  DefaultTLSClientKeyExchangeOptions = AllTLSKeyExchangeOptions;
  DefaultTLSClientCipherOptions      = AllTLSCipherOptions;
  DefaultTLSClientHashOptions        = AllTLSHashOptions;

type
  TTLSClient = class;

  TTLSClientNotifyEvent = procedure (Sender: TTLSClient) of object;

  TTLSClientState = (
      tlsclInit,
      tlsclHandshakeAwaitingServerHello,
      tlsclHandshakeAwaitingServerHelloDone,
      tlsclHandshakeClientKeyExchange,
      tlsclConnection
    );

  TTLSClient = class(TTLSConnection)
  protected
    FClientOptions         : TTLSClientOptions;
    FVersionOptions        : TTLSVersionOptions;
    FKeyExchangeOptions    : TTLSKeyExchangeOptions;
    FCipherOptions         : TTLSCipherOptions;
    FHashOptions           : TTLSHashOptions;
    FResumeSessionID       : RawByteString;

    FClientState           : TTLSClientState;
    FClientProtocolVersion : TTLSProtocolVersion;
    FServerProtocolVersion : TTLSProtocolVersion;
    FClientHello           : TTLSClientHello;
    FClientHelloRandomStr  : RawByteString;
    FServerHello           : TTLSServerHello;
    FServerHelloRandomStr  : RawByteString;
    FServerCertificateList : TTLSCertificateList;
    FServerX509Certs       : TX509CertificateArray;
    FServerKeyExchange     : TTLSServerKeyExchange;
    FCertificateRequest    : TTLSCertificateRequest;
    FCertificateRequested  : Boolean;
    FClientKeyExchange     : TTLSClientKeyExchange;
    FServerRSAPublicKey    : TRSAPublicKey;
    FDHState               : PDHState;
    FPreMasterSecretStr    : RawByteString;
    FMasterSecret          : RawByteString;

    procedure Init; override;

    procedure SetClientState(const AState: TTLSClientState);
    procedure CheckNotActive;

    procedure SetClientOptions(const AClientOptions: TTLSClientOptions);
    procedure SetVersionOptions(const AVersionOptions: TTLSVersionOptions);
    procedure SetCipherOptions(const ACipherOptions: TTLSCipherOptions);
    procedure SetKeyExchangeOptions(const AKeyExchangeOptions: TTLSKeyExchangeOptions);
    procedure SetHashOptions(const AHashOptions: TTLSHashOptions);

    procedure InitInitialProtocolVersion;
    procedure InitSessionProtocolVersion;
    procedure InitClientHelloCipherSuites;
    procedure InitDHState;

    procedure InitHandshakeClientHello;
    procedure InitServerPublicKey_RSA;
    procedure InitHandshakeClientKeyExchange;

    procedure SendHandshakeClientHello;
    procedure SendHandshakeCertificate;
    procedure SendHandshakeClientKeyExchange;
    procedure SendHandshakeCertificateVerify;
    procedure SendHandshakeFinished;

    procedure HandleHandshakeHelloRequest(const Buffer; const Size: Integer);
    procedure HandleHandshakeServerHello(const Buffer; const Size: Integer);
    procedure HandleHandshakeCertificate(const Buffer; const Size: Integer);
    procedure HandleHandshakeServerKeyExchange(const Buffer; const Size: Integer);
    procedure HandleHandshakeCertificateRequest(const Buffer; const Size: Integer);
    procedure HandleHandshakeServerHelloDone(const Buffer; const Size: Integer);
    procedure HandleHandshakeFinished(const Buffer; const Size: Integer);
    procedure HandleHandshakeMessage(const MsgType: TTLSHandshakeType; const Buffer; const Size: Integer); override;

    procedure InitCipherSpecNone;
    procedure InitCipherSpecNewFromServerHello;

    procedure DoStart;

  public
    constructor Create(const ATransportLayerSendProc: TTLSConnectionTransportLayerSendProc);
    destructor Destroy; override;

    property  ClientOptions: TTLSClientOptions read FClientOptions write SetClientOptions default DefaultTLSClientOptions;
    property  VersionOptions: TTLSVersionOptions read FVersionOptions write SetVersionOptions default DefaultTLSClientVersionOptions;
    property  KeyExchangeOptions: TTLSKeyExchangeOptions read FKeyExchangeOptions write SetKeyExchangeOptions default DefaultTLSClientKeyExchangeOptions;
    property  CipherOptions: TTLSCipherOptions read FCipherOptions write SetCipherOptions default DefaultTLSClientCipherOptions;
    property  HashOptions: TTLSHashOptions read FHashOptions write SetHashOptions default DefaultTLSClientHashOptions;

    property  ResumeSessionID: RawByteString read FResumeSessionID write FResumeSessionID;

    // property OnValidateCertificate

    property  ClientState: TTLSClientState read FClientState;

    procedure Start;
  end;



implementation

uses
  { Utils }

  flcHugeInt,
  flcASN1,

  { Cipher }

  flcCipherUtils,

  { TLS }

  flcTLSErrors,
  flcTLSCompress,
  flcTLSCipher;



{                                                                              }
{ TLS Client                                                                   }
{                                                                              }
const
  STLSClientState: array[TTLSClientState] of String = (
    'Init',
    'HandshakeAwaitingServerHello',
    'HandshakeAwaitingServerHelloDone',
    'HandshakeClientKeyExchange',
    'Connection');

constructor TTLSClient.Create(const ATransportLayerSendProc: TTLSConnectionTransportLayerSendProc);
begin
  inherited Create(ATransportLayerSendProc);
end;

destructor TTLSClient.Destroy;
begin
  if Assigned(FDHState) then
    begin
      DHStateFinalise(FDHState^);
      Dispose(FDHState);
      FDHState := nil;
    end;
  RSAPublicKeyFinalise(FServerRSAPublicKey);
  SecureClearStr(FMasterSecret);
  inherited Destroy;
end;

procedure TTLSClient.Init;
begin
  inherited Init;

  RSAPublicKeyInit(FServerRSAPublicKey);

  FClientOptions      := DefaultTLSClientOptions;
  FVersionOptions     := DefaultTLSClientVersionOptions;
  FKeyExchangeOptions := DefaultTLSClientKeyExchangeOptions;
  FCipherOptions      := DefaultTLSClientCipherOptions;
  FHashOptions        := DefaultTLSClientHashOptions;

  FClientState := tlsclInit;
end;

procedure TTLSClient.SetClientState(const AState: TTLSClientState);
begin
  {$IFDEF TLS_DEBUG}
  Log(tlsltDebug, 'State:%s', [STLSClientState[AState]]);
  {$ENDIF}

  FClientState := AState;
end;

procedure TTLSClient.CheckNotActive;
begin
  if FClientState <> tlsclInit then
    raise ETLSError.Create(TLSError_InvalidState, 'Operation not allowed while active');
end;

procedure TTLSClient.SetClientOptions(const AClientOptions: TTLSClientOptions);
begin
  if AClientOptions = FClientOptions then
    exit;
  CheckNotActive;
  FClientOptions := AClientOptions;
end;

procedure TTLSClient.SetVersionOptions(const AVersionOptions: TTLSVersionOptions);
begin
  if AVersionOptions = FVersionOptions then
    exit;
  CheckNotActive;
  if AVersionOptions = [] then
    raise ETLSError.Create(TLSError_InvalidParameter, 'Invalid version options');
  FVersionOptions := AVersionOptions;
end;

procedure TTLSClient.SetCipherOptions(const ACipherOptions: TTLSCipherOptions);
begin
  if ACipherOptions = FCipherOptions then
    exit;
  CheckNotActive;
  if ACipherOptions = [] then
    raise ETLSError.Create(TLSError_InvalidParameter, 'Invalid cipher options');
  FCipherOptions := ACipherOptions;
end;

procedure TTLSClient.SetKeyExchangeOptions(const AKeyExchangeOptions: TTLSKeyExchangeOptions);
begin
  if AKeyExchangeOptions = FKeyExchangeOptions then
    exit;
  CheckNotActive;
  if AKeyExchangeOptions = [] then
    raise ETLSError.Create(TLSError_InvalidParameter, 'Invalid key exchange options');
  FKeyExchangeOptions := AKeyExchangeOptions;
end;

procedure TTLSClient.SetHashOptions(const AHashOptions: TTLSHashOptions);
begin
  if AHashOptions = FHashOptions then
    exit;
  CheckNotActive;
  if AHashOptions = [] then
    raise ETLSError.Create(TLSError_InvalidParameter, 'Invalid hash options');
  FHashOptions := AHashOptions;
end;

procedure TTLSClient.InitInitialProtocolVersion;
begin
  // set highest allowable protocol version
  if tlsvoTLS12 in FVersionOptions then
    InitTLSProtocolVersion12(FProtocolVersion) else
  if tlsvoTLS11 in FVersionOptions then
    InitTLSProtocolVersion11(FProtocolVersion) else
  if tlsvoTLS10 in FVersionOptions then
    InitTLSProtocolVersion10(FProtocolVersion) else
  if tlsvoSSL3 in FVersionOptions then
    InitSSLProtocolVersion30(FProtocolVersion)
  else
    raise ETLSError.Create(TLSError_InvalidParameter, 'Invalid version options');
  FClientProtocolVersion := FProtocolVersion;

  {$IFDEF TLS_DEBUG}
  Log(tlsltDebug, 'InitialProtocolVersion:%s', [TLSProtocolVersionName(FProtocolVersion)]);
  {$ENDIF}
end;

procedure TTLSClient.InitSessionProtocolVersion;
begin
  FProtocolVersion := FServerProtocolVersion;
  if IsTLS12(FProtocolVersion) and not (tlsvoTLS12 in FVersionOptions) then
    InitTLSProtocolVersion11(FProtocolVersion);
  if IsTLS11(FProtocolVersion) and not (tlsvoTLS11 in FVersionOptions) then
    InitTLSProtocolVersion10(FProtocolVersion);
  if IsTLS10(FProtocolVersion) and not (tlsvoTLS10 in FVersionOptions) then
    InitSSLProtocolVersion30(FProtocolVersion);
  if IsSSL3(FProtocolVersion) and not (tlsvoSSL3 in FVersionOptions) then
    raise ETLSError.CreateAlertBadProtocolVersion; // no allowable protocol version

  {$IFDEF TLS_DEBUG}
  Log(tlsltDebug, 'SessionProtocolVersion:%s', [TLSProtocolVersionName(FProtocolVersion)]);
  {$ENDIF}
end;

procedure TTLSClient.InitClientHelloCipherSuites;
var
  C : TTLSCipherSuites;
  I : TTLSCipherSuite;
  P : PTLSCipherSuiteInfo;
  R : Boolean;
begin
  C := [];
  for I := Low(I) to High(I) do
    begin
      P := @TLSCipherSuiteInfo[I];
      R := P^.ClientSupport;
      if R then
        if not (tlscoRC4 in FCipherOptions) then
          if P^.Cipher in [tlscscRC4_40, tlscscRC4_56, tlscscRC4_128] then
            R := False;
      if R then
        if not (tlscoDES in FCipherOptions) then
          if P^.Cipher in [tlscscDES_CBC] then
            R := False;
      if R then
        if not (tlsco3DES in FCipherOptions) then
          if P^.Cipher in [tlscsc3DES_EDE_CBC] then
            R := False;
      if R then
        if not (tlscoAES128 in FCipherOptions) then
          if P^.Cipher in [tlscscAES_128_CBC] then
            R := False;
      if R then
        if not (tlscoAES256 in FCipherOptions) then
          if P^.Cipher in [tlscscAES_256_CBC] then
            R := False;
      if R then
        if not (tlshoMD5 in FHashOptions) then
          if P^.Hash in [tlscshMD5] then
            R := False;
      if R then
        if not (tlshoSHA1 in FHashOptions) then
          if P^.Hash in [tlscshSHA] then
            R := False;
      if R then
        if not (tlshoSHA256 in FHashOptions) then
          if P^.Hash in [tlscshSHA256] then
            R := False;
      if R then
        if not (tlshoSHA384 in FHashOptions) then
          if P^.Hash in [tlscshSHA384] then
            R := False;
      if R then
        if not (tlskeoRSA in FKeyExchangeOptions) then
          if P^.KeyExchange in [tlscskeRSA] then
            R := False;
      if R then
        if not (tlskeoDH_Anon in FKeyExchangeOptions) then
          if P^.KeyExchange in [tlscskeDH_anon] then
            R := False;
      if R then
        if not (tlskeoDH_RSA in FKeyExchangeOptions) then
          if P^.KeyExchange in [tlscskeDH_RSA] then
            R := False;
      if R then
        if not (tlskeoDHE_RSA in FKeyExchangeOptions) then
          if P^.KeyExchange in [tlscskeDHE_RSA] then
            R := False;
      if R then
        if not (tlskeoECDH_RSA in FKeyExchangeOptions) then
          if P^.KeyExchange in [tlscskeECDH_RSA] then
            R := False;
      if R then
        if not (tlskeoECDHE_RSA in FKeyExchangeOptions) then
          if P^.KeyExchange in [tlscskeECDHE_RSA] then
            R := False;
      if R then
        Include(C, I);
    end;
  if C = [] then
    raise ETLSError.Create(TLSError_InvalidParameter, 'No allowable cipher suite');
  FClientHello.CipherSuites := C;
end;

procedure TTLSClient.InitDHState;
begin
  New(FDHState);
  DHStateInit(FDHState^);
  DHInitHashAlgorithm(FDHState^, dhhSHA1);
end;

procedure TTLSClient.InitHandshakeClientHello;
begin
  InitTLSClientHello(FClientHello,
      FClientProtocolVersion,
      FResumeSessionID);

  {$IFDEF TLS_TEST_NO_RANDOM_HELLO}
  FClientHello.Random.gmt_unix_time := 123;
  FillChar(FClientHello.Random.random_bytes, 28, 117);
  {$ENDIF}

  InitClientHelloCipherSuites;
  FClientHello.CompressionMethods := [tlscmNull];
  FClientHelloRandomStr := TLSRandomToStr(FClientHello.Random);
end;

procedure TTLSClient.InitServerPublicKey_RSA;
begin
  GetCertificateRSAPublicKey(FServerX509Certs,
      FServerRSAPublicKey);
end;

procedure TTLSClient.InitHandshakeClientKeyExchange;
var
  S : RawByteString;
  PMS : TTLSPreMasterSecret;
  DHXC : HugeWord;
  DHYC : HugeWord;
  DHP : HugeWord;
  DHG : HugeWord;
  DHYS : HugeWord;
  ZZ : HugeWord;
begin
  {$IFDEF TLS_DEBUG}
  Log(tlsltDebug, 'InitHandshakeClientKeyExchange:%s', [
      TLSKeyExchangeAlgorithmInfo[FCipherSpecNew.KeyExchangeAlgorithm].Name]);
  {$ENDIF}

  case FCipherSpecNew.KeyExchangeAlgorithm of
    tlskeaRSA :
      begin
        { RFC 5246: When RSA is used for server authentication and key exchange, a 48- }
        { byte pre_master_secret is generated by the client, encrypted under }
        { the server's public key, and sent to the server.  The server uses its }
        { private key to decrypt the pre_master_secret.  Both parties then }
        { convert the pre_master_secret into the master_secret, as specified above. }

        InitTLSPreMasterSecret_Random(PMS, FClientHello.ProtocolVersion);
        FPreMasterSecretStr := TLSPreMasterSecretToStr(PMS);

        InitServerPublicKey_RSA;

        InitTLSEncryptedPreMasterSecret_RSA(S, PMS, FServerRSAPublicKey);
        FClientKeyExchange.EncryptedPreMasterSecret := S;
      end;
    tlskeaDHE_DSS,
    tlskeaDHE_RSA,
    tlskeaDH_Anon :
      begin
        //Assert(Assigned(FDHState));

        { RFC 5246: A conventional Diffie-Hellman computation is performed.  The }
        { negotiated key (Z) is used as the pre_master_secret, and is converted }
        { into the master_secret, as specified above.  Leading bytes of Z that }
        { contain all zero bits are stripped before it is used as the }
        { pre_master_secret }

        InitTLSPreMasterSecret_Random(PMS, FClientHello.ProtocolVersion);

        HugeWordInit(DHXC);
        HugeWordInit(DHYC);
        HugeWordInit(DHP);
        HugeWordInit(DHG);
        HugeWordInit(DHYS);
        HugeWordInit(ZZ);
        try
          DHHugeWordKeyDecodeBytes(DHP, FServerKeyExchange.DHParams.dh_p);
          DHHugeWordKeyDecodeBytes(DHG, FServerKeyExchange.DHParams.dh_g);
          DHHugeWordKeyDecodeBytes(DHYS, FServerKeyExchange.DHParams.dh_Ys);

          //HugeWordSetSize(DHXC, (SizeOf(PMS) + 3) div 4);   //// size? 384 bits in SBA. use q size.
          //Move(PMS, DHXC.Data^, SizeOf(PMS));

          repeat
            HugeWordRandom(DHXC, HugeWordGetSize(DHP)); /////
            if SizeOf(PMS) <= DHXC.Used * 4 then
              Move(PMS, DHXC.Data^, SizeOf(PMS))
            else
              Move(PMS, DHXC.Data^, DHXC.Used * 4);
          until HugeWordCompare(DHXC, DHP) < 0;

          HugeWordPowerAndMod(DHYC, DHG, DHXC, DHP);  // yc = (g ^ xc) mod p

          FClientKeyExchange.ClientDiffieHellmanPublic.PublicValueEncodingExplicit := True;
          FClientKeyExchange.ClientDiffieHellmanPublic.dh_Yc := DHHugeWordKeyEncodeBytes(DHYC);

          HugeWordPowerAndMod(ZZ, DHYs, DHXC, DHP);  // ZZ = (ys ^ xc) mod p

          FPreMasterSecretStr := DHHugeWordKeyEncodeBytes(ZZ);
        finally
          //// Secure clear
          HugeWordFinalise(ZZ);
          HugeWordFinalise(DHYS);
          HugeWordFinalise(DHG);
          HugeWordFinalise(DHP);
          HugeWordFinalise(DHYC);
          HugeWordFinalise(DHXC);
        end;
      end;
    tlskeaDH_DSS,
    tlskeaDH_RSA :
      begin
        FClientKeyExchange.ClientDiffieHellmanPublic.PublicValueEncodingExplicit := False;
        FClientKeyExchange.ClientDiffieHellmanPublic.dh_Yc := '';
      end;
  end;

  case FCipherSpecNew.KeyExchangeAlgorithm of
    tlskeaRSA,
    tlskeaDHE_DSS,
    tlskeaDHE_RSA,
    tlskeaDH_Anon :
      begin
        Assert(FPreMasterSecretStr <> '');
        FMasterSecret := TLSMasterSecret(FProtocolVersion, FPreMasterSecretStr,
            FClientHelloRandomStr, FServerHelloRandomStr);
        SecureClearStr(FPreMasterSecretStr);

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

        SetEncodeKeys(FKeys.ClientMACKey, FKeys.ClientEncKey, FKeys.ClientIV);
        SetDecodeKeys(FKeys.ServerMACKey, FKeys.ServerEncKey, FKeys.ServerIV);
      end;
  end;
end;

const
  MaxHandshakeClientHelloSize       = 16384;
  MaxHandshakeCertificateSize       = 65536;
  MaxHandshakeClientKeyExchangeSize = 2048;
  MaxHandshakeCertificateVerifySize = 16384;
  MaxHandshakeFinishedSize          = 2048;

procedure TTLSClient.SendHandshakeClientHello;
var
  Buf  : array[0..MaxHandshakeClientHelloSize - 1] of Byte;
  Size : Integer;
begin
  {$IFDEF TLS_DEBUG}
  Log(tlsltDebug, 'T:Handshake:ClientHello');
  {$ENDIF}

  InitHandshakeClientHello;
  Size := EncodeTLSHandshakeClientHello(Buf, SizeOf(Buf), FClientHello);
  SendHandshake(Buf, Size);
end;

procedure TTLSClient.SendHandshakeCertificate;
var
  Buf  : array[0..MaxHandshakeCertificateSize - 1] of Byte;
  Size : Integer;
begin
  {$IFDEF TLS_DEBUG}
  Log(tlsltDebug, 'T:Handshake:Certificate');
  {$ENDIF}

  Size := EncodeTLSHandshakeCertificate(Buf, SizeOf(Buf), nil);
  SendHandshake(Buf, Size);
end;

procedure TTLSClient.SendHandshakeClientKeyExchange;
var
  Buf  : array[0..MaxHandshakeClientKeyExchangeSize - 1] of Byte;
  Size : Integer;
begin
  {$IFDEF TLS_DEBUG}
  Log(tlsltDebug, 'T:Handshake:ClientKeyExchange');
  {$ENDIF}

  InitHandshakeClientKeyExchange;
  Size := EncodeTLSHandshakeClientKeyExchange(
      Buf, SizeOf(Buf),
      FCipherSpecNew.KeyExchangeAlgorithm,
      FClientKeyExchange);
  SendHandshake(Buf, Size);
end;

procedure TTLSClient.SendHandshakeCertificateVerify;
var
  Buf  : array[0..MaxHandshakeCertificateVerifySize - 1] of Byte;
  Size : Integer;
begin
  Size := EncodeTLSHandshakeCertificateVerify(Buf, SizeOf(Buf));
  SendHandshake(Buf, Size);
end;

procedure TTLSClient.SendHandshakeFinished;
var
  Buf  : array[0..MaxHandshakeFinishedSize - 1] of Byte;
  Size : Integer;
begin
  {$IFDEF TLS_DEBUG}
  Log(tlsltDebug, 'T:Handshake:Finished:%s', [TLSProtocolVersionName(FProtocolVersion)]);
  {$ENDIF}

  Size := EncodeTLSHandshakeFinished(Buf, SizeOf(Buf), FMasterSecret, FProtocolVersion, FVerifyHandshakeData, True);
  SendHandshake(Buf, Size);
end;

procedure TTLSClient.HandleHandshakeHelloRequest(const Buffer; const Size: Integer);
begin
  if IsNegotiatingState then
    exit; // ignore while negotiating
  if FConnectionState = tlscoApplicationData then
    SendAlert(tlsalWarning, tlsadNo_renegotiation); // client does not support renegotiation, notify server
end;

procedure TTLSClient.HandleHandshakeServerHello(const Buffer; const Size: Integer);
begin
  if not (FClientState in [tlsclHandshakeAwaitingServerHello]) or
     not (FConnectionState in [tlscoStart, tlscoHandshaking]) then
    raise ETLSError.CreateAlertUnexpectedMessage;

  DecodeTLSServerHello(Buffer, Size, FServerHello);
  FServerProtocolVersion := FServerHello.ProtocolVersion;

  {$IFDEF TLS_DEBUG}
  Log(tlsltDebug, 'ServerProtocolVersion:%s', [TLSProtocolVersionName(FServerProtocolVersion)]);
  {$ENDIF}

  FServerHelloRandomStr := TLSRandomToStr(FServerHello.Random);
  if not IsTLSProtocolVersion(FServerProtocolVersion, FProtocolVersion) then // different protocol version
    begin
      if IsPostTLS12(FServerProtocolVersion) then
        raise ETLSError.CreateAlert(TLSError_BadProtocol, tlsadProtocol_version); // unsupported future version of TLS
      if not IsKnownTLSVersion(FServerProtocolVersion) then
        raise ETLSError.CreateAlert(TLSError_BadProtocol, tlsadProtocol_version); // unknown past TLS version
    end;
  InitSessionProtocolVersion;
  InitCipherSpecNewFromServerHello;
  SetClientState(tlsclHandshakeAwaitingServerHelloDone);
end;

procedure TTLSClient.HandleHandshakeCertificate(const Buffer; const Size: Integer);
begin
  if FClientState <> tlsclHandshakeAwaitingServerHelloDone then
    raise ETLSError.CreateAlertUnexpectedMessage;

  DecodeTLSCertificate(Buffer, Size, FServerCertificateList);
  ParseX509Certificates(FServerCertificateList, FServerX509Certs);

  {$IFDEF TLS_DEBUG}
  Log(tlsltDebug, 'R:Handshake:Certificate:Count=%d', [Length(FServerX509Certs)]);
  {$ENDIF}
end;

procedure TTLSClient.HandleHandshakeServerKeyExchange(const Buffer; const Size: Integer);
//var
//  R : HugeWord;
begin
  if FClientState <> tlsclHandshakeAwaitingServerHelloDone then
    raise ETLSError.CreateAlertUnexpectedMessage;

  DecodeTLSServerKeyExchange(Buffer, Size, FCipherSpecNew.KeyExchangeAlgorithm,
      FServerKeyExchange);

  case FCipherSpecNew.KeyExchangeAlgorithm of
    tlskeaDHE_DSS,
    tlskeaDHE_RSA,
    tlskeaDH_Anon :
      begin
        {
        //// Validate
        //DHHugeWordKeyDecodeBytes(DHP, FServerKeyExchange.DHParams.dh_p);
        //DHHugeWordKeyDecodeBytes(DHG, FServerKeyExchange.DHParams.dh_g);
        //DHHugeWordKeyDecodeBytes(DHYS, FServerKeyExchange.DHParams.dh_Ys);

        InitDHState;
        DHHugeWordKeyDecodeBytes(FDHState^.P, FServerKeyExchange.DHParams.dh_p);
        DHHugeWordKeyDecodeBytes(FDHState^.G, FServerKeyExchange.DHParams.dh_g);
        FDHState^.PrimePBitCount := HugeWordGetSizeInBits(FDHState^.P);
        FDHState^.PrimeQBitCount := DHQBitCount(FDHState^.PrimePBitCount);

        DHDeriveKeysFromGroupParametersPG(FDHState^, dhhSHA1,
            FDHState^.PrimeQBitCount,
            FDHState^.PrimePBitCount,
            FDHState^.P,
            FDHState^.G);

        HugeWordInit(R);
        DHHugeWordKeyDecodeBytes(R, FServerKeyExchange.DHParams.dh_Ys);

        DHGenerateSharedSecretZZ(FDHState^, HugeWordGetSizeInBits(R), R);
        HugeWordFinalise(R);
        }

        if FCipherSpecNew.KeyExchangeAlgorithm = tlskeaDHE_RSA then
          begin
            InitServerPublicKey_RSA;
            if not VerifyTLSServerKeyExchangeDH_RSA(FServerKeyExchange,
                      PTLSClientServerRandom(@FClientHello.Random)^,
                      PTLSClientServerRandom(@FServerHello.Random)^,
                      FServerRSAPublicKey) then
              raise ETLSError.CreateAlert(TLSError_BadProtocol, tlsadHandshake_failure);
          end;
      end;
  end;
end;

procedure TTLSClient.HandleHandshakeCertificateRequest(const Buffer; const Size: Integer);
begin
  if FClientState <> tlsclHandshakeAwaitingServerHelloDone then
    raise ETLSError.CreateAlertUnexpectedMessage;

  DecodeTLSCertificateRequest(Buffer, Size, FCertificateRequest);
  FCertificateRequested := True;
end;

procedure TTLSClient.HandleHandshakeServerHelloDone(const Buffer; const Size: Integer);
begin
  if FClientState <> tlsclHandshakeAwaitingServerHelloDone then
    raise ETLSError.CreateAlertUnexpectedMessage;

  SetClientState(tlsclHandshakeClientKeyExchange);
  if FCertificateRequested then
    SendHandshakeCertificate;
  SendHandshakeClientKeyExchange;
  // TODO SendHandshakeCertificateVerify;
  SendChangeCipherSpec;
  ChangeEncryptCipherSpec;
  SendHandshakeFinished;
end;

procedure TTLSClient.HandleHandshakeFinished(const Buffer; const Size: Integer);
begin
  if FClientState <> tlsclHandshakeClientKeyExchange then
    raise ETLSError.CreateAlertUnexpectedMessage;

  SetClientState(tlsclConnection);
  SetConnectionState(tlscoApplicationData);
  TriggerHandshakeFinished;
end;

procedure TTLSClient.HandleHandshakeMessage(const MsgType: TTLSHandshakeType; const Buffer; const Size: Integer);
begin
  {$IFDEF TLS_DEBUG}
  Log(tlsltDebug, 'R:Handshake:%s:%db', [TLSHandshakeTypeToStr(MsgType), Size]);
  {$ENDIF}

  case MsgType of
    tlshtHello_request       : HandleHandshakeHelloRequest(Buffer, Size);
    tlshtClient_hello        : ShutdownBadProtocol(tlsadUnexpected_message);
    tlshtServer_hello        : HandleHandshakeServerHello(Buffer, Size);
    tlshtCertificate         : HandleHandshakeCertificate(Buffer, Size);
    tlshtServer_key_exchange : HandleHandshakeServerKeyExchange(Buffer, Size);
    tlshtCertificate_request : HandleHandshakeCertificateRequest(Buffer, Size);
    tlshtServer_hello_done   : HandleHandshakeServerHelloDone(Buffer, Size);
    tlshtCertificate_verify  : ShutdownBadProtocol(tlsadUnexpected_message);
    tlshtClient_key_exchange : ShutdownBadProtocol(tlsadUnexpected_message);
    tlshtFinished            : HandleHandshakeFinished(Buffer, Size);
  else
    ShutdownBadProtocol(tlsadUnexpected_message);
  end;
end;

procedure TTLSClient.InitCipherSpecNone;
begin
  InitTLSSecurityParametersNone(FCipherEncryptSpec);
  InitTLSSecurityParametersNone(FCipherDecryptSpec);
  TLSCipherInitNone(FCipherEncryptState, tlscoEncrypt);
  TLSCipherInitNone(FCipherDecryptState, tlscoDecrypt);
end;

procedure TTLSClient.InitCipherSpecNewFromServerHello;
begin
  InitTLSSecurityParameters(
      FCipherSpecNew,
      FServerHello.CompressionMethod,
      GetCipherSuiteByRec(FServerHello.CipherSuite.B1, FServerHello.CipherSuite.B2));

  {$IFDEF TLS_DEBUG}
  Log(tlsltDebug, 'CipherSpec:%s', [FCipherSpecNew.CipherSuiteDetails.CipherSuiteInfo^.Name]);
  {$ENDIF}
end;

procedure TTLSClient.DoStart;
begin
  SetConnectionState(tlscoStart);
  InitInitialProtocolVersion;
  InitCipherSpecNone;
  SetConnectionState(tlscoHandshaking);
  SetClientState(tlsclHandshakeAwaitingServerHello);
  SendHandshakeClientHello;
end;

procedure TTLSClient.Start;
begin
  Assert(FConnectionState = tlscoInit);
  Assert(FClientState = tlsclInit);
  DoStart;
end;



end.

