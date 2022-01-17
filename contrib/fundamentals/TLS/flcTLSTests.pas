{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals TLS                                         }
{   File name:        flcTLSTests.pas                                          }
{   File version:     5.05                                                     }
{   Description:      TLS tests                                                }
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
{   2010/12/15  0.02  Client/Server test case.                                 }
{   2010/12/17  0.03  Client/Server test cases for TLS 1.0, 1.1 and 1.2.       }
{   2018/07/17  5.04  Revised for Fundamentals 5.                              }
{   2020/05/11  5.05  Use client options.                                      }
{                                                                              }
{ References:                                                                  }
{                                                                              }
{   SSL 3 - www.mozilla.org/projects/security/pki/nss/ssl/draft302.txt         }
{   RFC 2246 - The TLS Protocol Version 1.0                                    }
{   RFC 4346 - The TLS Protocol Version 1.1                                    }
{   RFC 5246 - The TLS Protocol Version 1.2                                    }
{   RFC 4366 - Transport Layer Security (TLS) Extensions                       }
{   www.mozilla.org/projects/security/pki/nss/ssl/traces/trc-clnt-ex.html      }
{   RFC 4492 - Elliptic Curve Cryptography (ECC) Cipher Suites                 }
{                                                                              }
{******************************************************************************}

//  -----------------------------------------------------  -------------------------------------------
//  CIPHER SUITE                                           ClientServer Test
//  -----------------------------------------------------  -------------------------------------------
//  RSA_WITH_RC4_128_MD5                                   TESTED OK
//  RSA_WITH_RC4_128_SHA                                   TESTED OK
//  RSA_WITH_DES_CBC_SHA                                   TESTED OK
//  RSA_WITH_AES_128_CBC_SHA                               TESTED OK
//  RSA_WITH_AES_256_CBC_SHA                               TESTED OK
//  RSA_WITH_AES_128_CBC_SHA256                            TESTED OK TLS 1.2 only
//  RSA_WITH_AES_256_CBC_SHA256                            TESTED OK TLS 1.2 only
//  NULL_WITH_NULL_NULL                                    UNTESTED
//  RSA_WITH_NULL_MD5                                      UNTESTED
//  RSA_WITH_NULL_SHA                                      UNTESTED
//  RSA_WITH_IDEA_CBC_SHA                                  UNTESTED
//  RSA_WITH_3DES_EDE_CBC_SHA                              ERROR: SERVER DECRYPTION FAILED
//  RSA_WITH_NULL_SHA256                                   ERROR
//  DHE_RSA_WITH_AES_256_CBC_SHA                           TESTED OK TLS 1.2
//  -----------------------------------------------------  -------------------------------------------

{$INCLUDE flcTLS.inc}

{$DEFINE TLS_TEST_LOG_TO_CONSOLE}

{$DEFINE Cipher_SupportEC}

unit flcTLSTests;

interface



{                                                                              }
{ Test                                                                         }
{                                                                              }
{$IFDEF TLS_TEST}
procedure Test;
{$ENDIF}



implementation

uses
  { System }

  SysUtils,
  SyncObjs,

  { Utils }

  flcStdTypes,
  flcBase64,
  flcStrings,
  flcPEM,
  flcASN1,
  flcX509Certificate,
  flcHugeInt,

  { Cipher }

  flcCipherAES,
  flcCipherRSA,
  flcCipherDH,
  {$IFDEF Cipher_SupportEC}
  flcCipherEllipticCurve,
  {$ENDIF}

  { TLS }

  flcTLSConsts,
  flcTLSProtocolVersion,
  flcTLSRecord,
  flcTLSAlert,
  flcTLSAlgorithmTypes,
  flcTLSRandom,
  flcTLSCertificate,
  flcTLSHandshake,
  flcTLSCipher,
  flcTLSPRF,
  flcTLSKeys,
  flcTLSTransportTypes,
  flcTLSTransportConnection,
  flcTLSTransportClient,
  flcTLSTransportServer,
  flcTLSTestCertificates;



{                                                                              }
{ Test                                                                         }
{                                                                              }
{$IFDEF TLS_TEST}
type
  TTLSClientServerTester = class
    Lock : TCriticalSection;
    Sr : TTLSServer;
    Cl : TTLSClient;
    SCl : TTLSServerClient;
    constructor Create;
    destructor Destroy; override;
    procedure ServerTLSendProc(Server: TTLSServer; Client: TTLSServerClient; const Buffer; const Size: Integer);
    procedure ClientTLSendProc(const Sender: TTLSConnection; const Buffer; const Size: Integer);
    procedure Log(Msg: String);
    procedure ClientLog(Sender: TTLSConnection; LogType: TTLSLogType; LogMsg: String; LogLevel: Integer);
    procedure ServerLog(Sender: TTLSServer; LogType: TTLSLogType; LogMsg: String; LogLevel: Integer);
  end;

constructor TTLSClientServerTester.Create;
begin
  inherited Create;
  Lock := TCriticalSection.Create;
  Sr := TTLSServer.Create(ServerTLSendProc);
  Sr.OnLog := ServerLog;
  Cl := TTLSClient.Create(ClientTLSendProc);
  Cl.OnLog := ClientLog;
end;

destructor TTLSClientServerTester.Destroy;
begin
  FreeAndNil(Cl);
  FreeAndNil(Sr);
  FreeAndNil(Lock);
  inherited Destroy;
end;

procedure TTLSClientServerTester.ServerTLSendProc(Server: TTLSServer; Client: TTLSServerClient; const Buffer; const Size: Integer);
begin
  Assert(Assigned(Cl));
  Cl.ProcessTransportLayerReceivedData(Buffer, Size);
end;

procedure TTLSClientServerTester.ClientTLSendProc(const Sender: TTLSConnection; const Buffer; const Size: Integer);
begin
  Assert(Assigned(SCl));
  SCl.ProcessTransportLayerReceivedData(Buffer, Size);
end;

{$IFDEF TLS_TEST_LOG_TO_CONSOLE}
procedure TTLSClientServerTester.Log(Msg: String);
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
procedure TTLSClientServerTester.Log(Msg: String);
begin
end;
{$ENDIF}

procedure TTLSClientServerTester.ClientLog(Sender: TTLSConnection; LogType: TTLSLogType; LogMsg: String; LogLevel: Integer);
begin
  Log(IntToStr(LogLevel) + ' C:' + LogMsg);
end;

procedure TTLSClientServerTester.ServerLog(Sender: TTLSServer; LogType: TTLSLogType; LogMsg: String; LogLevel: Integer);
begin
  Log(IntToStr(LogLevel) + ' S:' + LogMsg);
end;

procedure TestClientServer(
          const ClientOptions      : TTLSClientOptions      = [];
          const VersionOptions     : TTLSVersionOptions     = DefaultTLSClientVersionOptions;
          const KeyExchangeOptions : TTLSKeyExchangeOptions = DefaultTLSClientKeyExchangeOptions;
          const CipherOptions      : TTLSCipherOptions      = DefaultTLSClientCipherOptions;
          const HashOptions        : TTLSHashOptions        = DefaultTLSClientHashOptions
          );
const
  LargeBlockSize = TLS_PLAINTEXT_FRAGMENT_MAXSIZE * 8;
var CS : TTLSClientServerTester;
    CtL : TTLSCertificateList;
    S : RawByteString;
    I, L : Integer;
begin
  CS := TTLSClientServerTester.Create;
  try
    // initialise client
    CS.Cl.ClientOptions := ClientOptions;
    // initialise server
    CS.Sr.PrivateKeyRSAPEM := RSA_STunnel_PrivateKeyRSAPEM;
    TLSCertificateListAppend(CtL,
        MIMEBase64Decode(RSA_STunnel_CertificatePEM));
    CS.Sr.CertificateList := CtL;
    CS.Sr.DHKeySize := 512;
    // start server
    CS.Sr.Start;
    Assert(CS.Sr.State = tlssActive);
    // start connection
    CS.SCl := CS.Sr.AddClient(nil);
    CS.SCl.Start;
    CS.Cl.Start;
    // negotiated
    Assert(CS.Cl.IsReadyState);
    Assert(CS.SCl.IsReadyState);
    // application data (small block)
    S := 'Fundamentals';
    CS.Cl.Write(S[1], Length(S));
    Assert(CS.SCl.AvailableToRead = 12);
    S := '1234567890';
    Assert(CS.SCl.Read(S[1], 3) = 3);
    Assert(CS.SCl.AvailableToRead = 9);
    Assert(S = 'Fun4567890');
    Assert(CS.SCl.Read(S[1], 9) = 9);
    Assert(CS.SCl.AvailableToRead = 0);
    Assert(S = 'damentals0');
    S := 'Fundamentals';
    CS.SCl.Write(S[1], Length(S));
    Assert(CS.Cl.AvailableToRead = 12);
    S := '123456789012';
    Assert(CS.Cl.Read(S[1], 12) = 12);
    Assert(CS.Cl.AvailableToRead = 0);
    Assert(S = 'Fundamentals');
    // application data (large blocks)
    for L := LargeBlockSize - 1 to LargeBlockSize + 1 do
      begin
        SetLength(S, L);
        FillChar(S[1], L, #1);
        CS.Cl.Write(S[1], L);
        Assert(CS.SCl.AvailableToRead = L);
        FillChar(S[1], L, #0);
        CS.SCl.Read(S[1], L);
        for I := 1 to L do
          Assert(S[I] = #1);
        Assert(CS.SCl.AvailableToRead = 0);
        CS.SCl.Write(S[1], L);
        Assert(CS.Cl.AvailableToRead = L);
        FillChar(S[1], L, #0);
        Assert(CS.Cl.Read(S[1], L) = L);
        for I := 1 to L do
          Assert(S[I] = #1);
        Assert(CS.Cl.AvailableToRead = 0);
      end;
    // close
    CS.Cl.Close;
    Assert(CS.Cl.IsFinishedState);
    Assert(CS.SCl.IsFinishedState);
    // stop
    CS.Sr.RemoveClient(CS.SCl);
    CS.Sr.Stop;
  finally
    FreeAndNil(CS);
  end;
end;

procedure Test_Units_Dependencies;
begin
  flcPEM.Test;
  flcASN1.Test;
  flcX509Certificate.Test;

  flcHugeInt.Test;

  flcCipherAES.Test;
  flcCipherRSA.Test;
  flcCipherDH.Test;
  {$IFDEF Cipher_SupportEC}
  flcCipherEllipticCurve.Test;
  {$ENDIF}
end;

procedure Test_Units_TLS;
begin
  flcTLSAlert.Test;
  flcTLSAlgorithmTypes.Test;
  flcTLSRandom.Test;
  flcTLSCipher.Test;
  flcTLSHandshake.Test;
  flcTLSKeys.Test;
  flcTLSProtocolVersion.Test;
  flcTLSPRF.Test;
  flcTLSRecord.Test;
end;

procedure Test_ClientServer_TLSVersions;
begin
  // TLS 1.2
  TestClientServer([], [tlsvoTLS12]);
  // TLS 1.1
  TestClientServer([], [tlsvoTLS11]);
  // TLS 1.0
  TestClientServer([], [tlsvoTLS10]);
  Sleep(100);
  // SSL 3
  // Fails with invalid parameter
  //SelfTestClientServer([], [tlsvoSSL3]);
end;

procedure Test_ClientServer_KeyExchangeAlgos;
begin
  // TLS 1.2 RSA
  TestClientServer([], [tlsvoTLS12], [tlskeoRSA]);
  // TLS 1.2 DHE_RSA
  TestClientServer([], [tlsvoTLS12], [tlskeoDHE_RSA]);
  // TLS 1.2 DH_anon
  // Under development/testing
  //SelfTestClientServer([], [tlsvoTLS12], [tlskeoDH_Anon]);
end;

procedure Test_ClientServer_CipherAlgos;
begin
  // TLS 1.2 RC4
  TestClientServer([], [tlsvoTLS12], DefaultTLSClientKeyExchangeOptions,
      [tlscoRC4]);
  // TLS 1.2 AES128
  TestClientServer([], [tlsvoTLS12], DefaultTLSClientKeyExchangeOptions,
      [tlscoAES128]);
  // TLS 1.2 AES256
  TestClientServer([], [tlsvoTLS12], DefaultTLSClientKeyExchangeOptions,
      [tlscoAES256]);
  // TLS 1.2 DES
  TestClientServer([], [tlsvoTLS12], DefaultTLSClientKeyExchangeOptions,
      [tlscoDES]);
  // TLS 1.2 3DES
  // No Cipher Suite
  {SelfTestClientServer([], [tlsvoTLS12], DefaultTLSClientKeyExchangeOptions,
      [tlsco3DES]);}
end;

procedure Test_ClientServer_HashAlgos;
begin
  // TLS 1.2 SHA256
  TestClientServer(
      [], [tlsvoTLS12], DefaultTLSClientKeyExchangeOptions, DefaultTLSClientCipherOptions,
      [tlshoSHA256]);
  // TLS 1.2 SHA1
  TestClientServer(
      [], [tlsvoTLS12], DefaultTLSClientKeyExchangeOptions, DefaultTLSClientCipherOptions,
      [tlshoSHA1]);
  // TLS 1.2 MD5
  TestClientServer(
      [], [tlsvoTLS12], DefaultTLSClientKeyExchangeOptions, DefaultTLSClientCipherOptions,
      [tlshoMD5]);
end;

procedure Test_ClientServer;
begin
  Test_ClientServer_TLSVersions;
  Test_ClientServer_KeyExchangeAlgos;
  Test_ClientServer_CipherAlgos;
  Test_ClientServer_HashAlgos;
end;

procedure Test;
begin
  Test_Units_Dependencies;
  Test_Units_TLS;
  Test_ClientServer;
end;
{$ENDIF}



end.

