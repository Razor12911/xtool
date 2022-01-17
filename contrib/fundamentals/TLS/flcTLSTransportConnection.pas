{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals TLS                                         }
{   File name:        flcTLSTransportConnection.pas                            }
{   File version:     5.06                                                     }
{   Description:      TLS Transport Connection                                 }
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
{   2010/12/03  0.04  Revision.                                                }
{   2011/06/18  0.05  Allow multiple handshakes in a record.                   }
{   2018/07/17  5.06  Revised for Fundamentals 5.                              }
{                                                                              }
{******************************************************************************}

{$INCLUDE flcTLS.inc}

unit flcTLSTransportConnection;

interface

uses
  { Utils }

  flcStdTypes,

  { TLS }

  flcTLSErrors,
  flcTLSProtocolVersion,
  flcTLSAlgorithmTypes,
  flcTLSPRF,
  flcTLSCipherSuite,
  flcTLSCipher,
  flcTLSRecord,
  flcTLSAlert,
  flcTLSHandshake,
  flcTLSBuffer,
  flcTLSKeys,
  flcTLSTransportTypes;



{                                                                              }
{ TLS security parameters                                                      }
{                                                                              }
type
  TTLSSecurityParameters = record
    PrfAlgorithm                : TTLSPRFAlgorithm;
    CipherSuite                 : TTLSCipherSuite;
    CipherSuiteDetails          : TTLSCipherSuiteDetails;
    CipherSuiteCipherCipherInfo : PTLSCipherSuiteCipherCipherInfo;
    Compression                 : TTLSCompressionMethod;
    KeyExchangeAlgorithm        : TTLSKeyExchangeAlgorithm;
    KeyExchangeAlgorithmInfo    : PTLSKeyExchangeAlgorithmInfo;
  end;
  PTLSSecurityParameters = ^TTLSSecurityParameters;

procedure InitTLSSecurityParameters(var A: TTLSSecurityParameters;
          const CompressionMethod: TTLSCompressionMethod;
          const CipherSuite: TTLSCipherSuite);
procedure InitTLSSecurityParametersNone(var A: TTLSSecurityParameters);
procedure InitTLSSecurityParametersNULL(var A: TTLSSecurityParameters);



{                                                                              }
{ TLS connection                                                               }
{                                                                              }
type
  TTLSConnection = class;

  TTLSConnectionTransportLayerSendProc =
    procedure (const Sender: TTLSConnection; const Buffer; const Size: Integer) of object;

  TTLSConnectionState = (
      tlscoInit,
      tlscoStart,
      tlscoHandshaking,
      tlscoApplicationData,
      tlscoErrorBadProtocol,
      tlscoCancelled,
      tlscoClosed
    );

  TTLSConnectionLogEvent = procedure (Sender: TTLSConnection; LogType: TTLSLogType; LogMsg: String; LogLevel: Integer) of object;
  TTLSConnectionStateChangeEvent = procedure (Sender: TTLSConnection; State: TTLSConnectionState) of object;
  TTLSConnectionAlertEvent = procedure (Sender: TTLSConnection; Level: TTLSAlertLevel; Description: TTLSAlertDescription) of object;
  TTLSConnectionNotifyEvent = procedure (Sender: TTLSConnection) of object;

  TTLSConnection = class
  protected
    FTransportLayerSendProc : TTLSConnectionTransportLayerSendProc;
    FOnLog                  : TTLSConnectionLogEvent;
    FOnStateChange          : TTLSConnectionStateChangeEvent;
    FOnAlert                : TTLSConnectionAlertEvent;
    FOnHandshakeFinished    : TTLSConnectionNotifyEvent;
    FConnectionState        : TTLSConnectionState;
    FConnectionErrorMessage : String;
    FInBuf                  : TTLSBuffer;
    FOutBuf                 : TTLSBuffer;
    FProtocolVersion        : TTLSProtocolVersion;
    FReadSeqNo              : Int64;
    FWriteSeqNo             : Int64;
    FKeys                   : TTLSKeys;
    FEncMACKey              : RawByteString;
    FEncCipherKey           : RawByteString;
    FEncIV                  : RawByteString;
    FDecMACKey              : RawByteString;
    FDecCipherKey           : RawByteString;
    FDecIV                  : RawByteString;
    FVerifyHandshakeData    : RawByteString;
    FCipherEncryptSpec      : TTLSSecurityParameters;
    FCipherEncryptState     : TTLSCipherState;
    FCipherDecryptSpec      : TTLSSecurityParameters;
    FCipherDecryptState     : TTLSCipherState;
    FCipherSpecNew          : TTLSSecurityParameters;

    procedure Init; virtual;

    procedure Log(const LogType: TTLSLogType; const LogMsg: String; const LogLevel: Integer = 0); overload;
    procedure Log(const LogType: TTLSLogType; const LogMsg: String; const LogArgs: array of const; const LogLevel: Integer = 0); overload;

    procedure TriggerLog(const LogType: TTLSLogType; const LogMsg: String; const LogLevel: Integer); virtual;
    procedure TriggerConnectionStateChange; virtual;
    procedure TriggerAlert(const Level: TTLSAlertLevel; const Description: TTLSAlertDescription); virtual;
    procedure TriggerHandshakeFinished; virtual;

    procedure SetConnectionState(const State: TTLSConnectionState);
    procedure SetClosed;
    procedure SetErrorBadProtocol;

    procedure SetEncodeKeys(const MACKey, CipherKey, IV: RawByteString);
    procedure SetDecodeKeys(const MACKey, CipherKey, IV: RawByteString);

    procedure TransportLayerSend(const Buffer; const Size: Integer);
    procedure SendContent(const ContentType: TTLSContentType; const Buffer; const Size: Integer);

    procedure SendAlert(const Level: TTLSAlertLevel; const Description: TTLSAlertDescription);
    procedure SendAlertCloseNotify;
    procedure SendAlertUnexpectedMessage;
    procedure SendAlertIllegalParameter;
    procedure SendAlertDecodeError;
    procedure SendAlertProtocolVersion;
    procedure SendAlertInternalError;

    procedure SendApplicationData(const Buffer; const Size: Integer);
    procedure SendChangeCipherSpec;
    procedure SendHandshake(const Buf; const Size: Integer);

    procedure ShutdownBadProtocol(const AlertDescription: TTLSAlertDescription);
    procedure AddVerifyHandshakeData(const Buffer; const Size: Integer);
    procedure DoClose;
    
    procedure ChangeEncryptCipherSpec; 
    procedure ChangeDecryptCipherSpec;

    procedure HandleAlertCloseNotify;
    procedure HandleAlertProtocolVersion;
    procedure HandleAlertProtocolFailure(const Alert: TTLSAlert);
    procedure HandleAlertCertificateError(const Alert: TTLSAlert);
    procedure HandleAlertSecurityError(const Alert: TTLSAlert);
    procedure HandleAlertUserCancelled;
    procedure HandleAlertNoRenegotiation;
    procedure HandleAlertUnknown(const Alert: TTLSAlert);
    procedure HandleProtocolAlert(const Buffer; const Size: Integer);

    procedure HandleProtocolChangeCipherSpec(const Buffer; const Size: Integer);
    procedure HandleProtocolApplicationData(const Buffer; const Size: Integer);

    procedure HandleHandshakeMessage(const MsgType: TTLSHandshakeType; const Buffer; const Size: Integer); virtual; abstract;
    procedure HandleProtocolHandshake(const Buffer; const Size: Integer);

    procedure ProcessTransportLayerData;

  public
    constructor Create(const ATransportLayerSendProc: TTLSConnectionTransportLayerSendProc);
    destructor Destroy; override;

    property  OnLog: TTLSConnectionLogEvent read FOnLog write FOnLog;
    property  OnStateChange: TTLSConnectionStateChangeEvent read FOnStateChange write FOnStateChange;
    property  OnAlert: TTLSConnectionAlertEvent read FOnAlert write FOnAlert;
    property  OnHandshakeFinished: TTLSConnectionNotifyEvent read FOnHandshakeFinished write FOnHandshakeFinished;

    property  ConnectionState: TTLSConnectionState read FConnectionState;
    property  ConnectionErrorMessage: String read FConnectionErrorMessage;

    function  IsNegotiatingState: Boolean;
    function  IsReadyState: Boolean;
    function  IsFinishedState: Boolean;

    procedure ProcessTransportLayerReceivedData(const Buffer; const Size: Integer);

    function  AvailableToRead: Integer;
    function  Read(var Buffer; const Size: Integer): Integer;
    procedure Write(const Buffer; const Size: Integer);
    procedure Close;
  end;



implementation

uses
  { System }

  SysUtils,

  { Cipher }

  flcCipherUtils,

  { TLS }

  flcTLSConsts;



{                                                                              }
{ Security Parameters                                                          }
{                                                                              }
procedure InitTLSSecurityParameters(var A: TTLSSecurityParameters;
          const CompressionMethod: TTLSCompressionMethod;
          const CipherSuite: TTLSCipherSuite);
var C : PTLSCipherSuiteInfo;
begin
  C := @TLSCipherSuiteInfo[CipherSuite];
  A.Compression := CompressionMethod;
  A.CipherSuite := CipherSuite;
  InitTLSCipherSuiteDetails(A.CipherSuiteDetails, CipherSuite);
  A.KeyExchangeAlgorithm := TLSCipherSuiteKeyExchangeInfo[C^.KeyExchange].Algorithm;
  A.KeyExchangeAlgorithmInfo := @TLSKeyExchangeAlgorithmInfo[A.KeyExchangeAlgorithm];
end;

procedure InitTLSSecurityParametersNone(var A: TTLSSecurityParameters);
begin
  InitTLSSecurityParameters(A, tlscmNull, tlscsNone);
end;

procedure InitTLSSecurityParametersNULL(var A: TTLSSecurityParameters);
begin
  InitTLSSecurityParameters(A, tlscmNull, tlscsNULL_WITH_NULL_NULL);
end;



{                                                                              }
{ TLS connection                                                               }
{                                                                              }
constructor TTLSConnection.Create(const ATransportLayerSendProc: TTLSConnectionTransportLayerSendProc);
begin
  inherited Create;
  Init;
  if not Assigned(ATransportLayerSendProc) then
    raise ETLSError.Create(TLSError_InvalidParameter);
  FTransportLayerSendProc := ATransportLayerSendProc;
end;

procedure TTLSConnection.Init;
begin
  FConnectionState := tlscoInit;
  TLSBufferInitialise(FInBuf);
  TLSBufferInitialise(FOutBuf);
end;

destructor TTLSConnection.Destroy;
begin
  SecureClearStr(FEncMACKey);
  SecureClearStr(FEncCipherKey);
  SecureClearStr(FEncIV);
  SecureClearStr(FDecMACKey);
  SecureClearStr(FDecCipherKey);
  SecureClearStr(FDecIV);
  SecureClearStr(FVerifyHandshakeData);
  TLSBufferFinalise(FOutBuf);
  TLSBufferFinalise(FInBuf);
  inherited Destroy;
end;

procedure TTLSConnection.Log(const LogType: TTLSLogType; const LogMsg: String; const LogLevel: Integer);
begin
  TriggerLog(LogType, LogMsg, LogLevel);
end;

procedure TTLSConnection.Log(const LogType: TTLSLogType; const LogMsg: String; const LogArgs: array of const; const LogLevel: Integer);
begin
  Log(LogType, Format(LogMsg, LogArgs), LogLevel);
end;

procedure TTLSConnection.TriggerLog(const LogType: TTLSLogType; const LogMsg: String; const LogLevel: Integer);
begin
  if Assigned(FOnLog) then
    FOnLog(self, LogType, LogMsg, LogLevel);
end;

procedure TTLSConnection.TriggerConnectionStateChange;
begin
  if Assigned(FOnStateChange) then
    FOnStateChange(self, FConnectionState);
end;

procedure TTLSConnection.TriggerAlert(const Level: TTLSAlertLevel; const Description: TTLSAlertDescription);
begin
  if Assigned(FOnAlert) then
    FOnAlert(self, Level, Description);
end;

procedure TTLSConnection.TriggerHandshakeFinished;
begin
  if Assigned(FOnHandshakeFinished) then
    FOnHandshakeFinished(self);
end;

const
  TLSConnectionStateStr : array[TTLSConnectionState] of String = (
    'Init',
    'Start',
    'Handshaking',
    'ApplicationData',
    'ErrorBadProtocol',
    'Cancelled',
    'Closed');

procedure TTLSConnection.SetConnectionState(const State: TTLSConnectionState);
begin
  FConnectionState := State;

  {$IFDEF TLS_DEBUG}
  Log(tlsltDebug, 'ConnectionState:%s', [TLSConnectionStateStr[State]]);
  {$ENDIF}

  TriggerConnectionStateChange;
end;

procedure TTLSConnection.SetClosed;
begin
  SetConnectionState(tlscoClosed);
end;

procedure TTLSConnection.SetErrorBadProtocol;
begin
  SetConnectionState(tlscoErrorBadProtocol);
end;

function TTLSConnection.IsNegotiatingState: Boolean;
begin
  Result := FConnectionState in [
      tlscoStart,
      tlscoHandshaking];
end;

function TTLSConnection.IsReadyState: Boolean;
begin
  Result := FConnectionState = tlscoApplicationData;
end;

function TTLSConnection.IsFinishedState: Boolean;
begin
  Result := FConnectionState in [
      tlscoErrorBadProtocol,
      tlscoCancelled,
      tlscoClosed];
end;

procedure TTLSConnection.SetEncodeKeys(const MACKey, CipherKey, IV: RawByteString);
begin
  FEncMACKey := MACKey;
  FEncCipherKey := CipherKey;
  FEncIV := IV;
end;

procedure TTLSConnection.SetDecodeKeys(const MACKey, CipherKey, IV: RawByteString);
begin
  FDecMACKey := MACKey;
  FDecCipherKey := CipherKey;
  FDecIV := IV;
end;

procedure TTLSConnection.TransportLayerSend(const Buffer; const Size: Integer);
begin
  Assert(Assigned(FTransportLayerSendProc));
  Assert(Assigned(@Buffer));
  Assert(Size >= 0);

  FTransportLayerSendProc(self, Buffer, Size);
end;

const
  TLS_CLIENT_RECORDBUF_MAXSIZE = TLS_PLAINTEXT_FRAGMENT_MAXSIZE * 2;

procedure TTLSConnection.SendContent(
          const ContentType: TTLSContentType;
          const Buffer; const Size: Integer);
var P : PByte;
    L : Integer;
    BufMsg : array[0..TLS_CLIENT_RECORDBUF_MAXSIZE - 1] of Byte;
    M, RecSize : Integer;
begin
  {$IFDEF TLS_DEBUG}
  Log(tlsltDebug, 'T:%s:%db', [TLSContentTypeToStr(ContentType), Size]);
  {$ENDIF}

  P := @Buffer;
  L := Size;
  while L > 0 do
    begin
      M := L;
      if M > TLS_PLAINTEXT_FRAGMENT_MAXSIZE then
        M := TLS_PLAINTEXT_FRAGMENT_MAXSIZE;

      RecSize := EncodeTLSRecord(
          BufMsg, SizeOf(BufMsg),
          FProtocolVersion,
          ContentType,
          P^, M,
          FCipherEncryptSpec.Compression,
          FCipherEncryptSpec.CipherSuiteDetails,
          FWriteSeqNo,
          Pointer(FEncMACKey)^, Length(FEncMACKey),
          FCipherEncryptState,
          Pointer(FEncIV), Length(FEncIV));

      Inc(FWriteSeqNo);
      TransportLayerSend(BufMsg, RecSize);

      Dec(L, M);
      Inc(P, M);
    end;
end;

procedure TTLSConnection.SendAlert(const Level: TTLSAlertLevel; const Description: TTLSAlertDescription);
var B : TTLSAlert;
begin
  {$IFDEF TLS_DEBUG}
  Log(tlsltDebug, 'T:Alert:%s:%s', [TLSAlertLevelToStr(Level), TLSAlertDescriptionToStr(Description)]);
  {$ENDIF}

  InitTLSAlert(B, Level, Description);
  SendContent(tlsctAlert, B, TLSAlertSize);

  if Level = tlsalFatal then
    FConnectionErrorMessage :=
        TLSAlertLevelToStr(Level) + ':' +
        TLSAlertDescriptionToStr(Description);
end;

procedure TTLSConnection.SendAlertCloseNotify;
begin
  SendAlert(tlsalWarning, tlsadClose_notify);
end;

procedure TTLSConnection.SendAlertUnexpectedMessage;
begin
  SendAlert(tlsalFatal, tlsadUnexpected_message);
end;

procedure TTLSConnection.SendAlertIllegalParameter;
begin
  SendAlert(tlsalFatal, tlsadIllegal_parameter);
end;

procedure TTLSConnection.SendAlertDecodeError;
begin
  SendAlert(tlsalFatal, tlsadDecode_error);
end;

procedure TTLSConnection.SendAlertProtocolVersion;
begin
  SendAlert(tlsalFatal, tlsadDecode_error);
end;

procedure TTLSConnection.SendAlertInternalError;
begin
  SendAlert(tlsalFatal, tlsadInternal_error);
end;

procedure TTLSConnection.SendApplicationData(const Buffer; const Size: Integer);
begin
  Assert(FConnectionState = tlscoApplicationData);
  SendContent(tlsctApplication_data, Buffer, Size);
end;

procedure TTLSConnection.SendHandshake(const Buf; const Size: Integer);
begin
  SendContent(tlsctHandshake, Buf, Size);
  AddVerifyHandshakeData(Buf, Size);
end;

procedure TTLSConnection.SendChangeCipherSpec;
var B : TTLSChangeCipherSpec;
begin
  {$IFDEF TLS_DEBUG}
  Log(tlsltDebug, 'T:ChangeCipherSpec');
  {$ENDIF}

  InitTLSChangeCipherSpec(B);
  SendContent(tlsctChange_cipher_spec, B, TLSChangeCipherSpecSize);
end;

procedure TTLSConnection.ShutdownBadProtocol(const AlertDescription: TTLSAlertDescription);
begin
  {$IFDEF TLS_DEBUG}
  Log(tlsltDebug, 'ShutdownBadProtocol:%s', [TLSAlertDescriptionToStr(AlertDescription)]);
  {$ENDIF}

  SendAlert(tlsalFatal, AlertDescription);
  SetErrorBadProtocol;
end;

procedure TTLSConnection.AddVerifyHandshakeData(const Buffer; const Size: Integer);
var S : RawByteString;
begin
  Assert(Size > 0);
  SetLength(S, Size);
  Move(Buffer, S[1], Size);
  FVerifyHandshakeData := FVerifyHandshakeData + S;
end;

procedure TTLSConnection.DoClose;
begin
  if FConnectionState = tlscoApplicationData then
    SendAlertCloseNotify;
  SetClosed;
end;

procedure TTLSConnection.ChangeEncryptCipherSpec;
begin
  FCipherEncryptSpec := FCipherSpecNew;

  {$IFDEF TLS_DEBUG}
  Log(tlsltDebug, 'EncryptCipherSpec:%s', [FCipherEncryptSpec.CipherSuiteDetails.CipherSuiteInfo^.Name]);
  {$ENDIF}

  FWriteSeqNo := 0;
  TLSCipherFinalise(FCipherEncryptState);
  TLSCipherInit(
      FCipherEncryptState,
      tlscoEncrypt,
      FCipherEncryptSpec.CipherSuiteDetails.CipherSuiteInfo^.Cipher,
      FEncCipherKey[1], Length(FEncCipherKey));
end;

procedure TTLSConnection.ChangeDecryptCipherSpec;
begin
  FCipherDecryptSpec := FCipherSpecNew;

  {$IFDEF TLS_DEBUG}
  Log(tlsltDebug, 'DecryptCipherSpec:%s', [
                  FCipherDecryptSpec.CipherSuiteDetails.CipherSuiteInfo^.Name]);
  {$ENDIF}

  FReadSeqNo := 0;
  TLSCipherFinalise(FCipherDecryptState);
  TLSCipherInit(
      FCipherDecryptState,
      tlscoDecrypt,
      FCipherDecryptSpec.CipherSuiteDetails.CipherSuiteInfo^.Cipher,
      FDecCipherKey[1], Length(FDecCipherKey));
end;

procedure TTLSConnection.HandleAlertCloseNotify;
begin
  SetClosed;
end;

procedure TTLSConnection.HandleAlertProtocolVersion;
begin
  SetErrorBadProtocol;
end;

procedure TTLSConnection.HandleAlertProtocolFailure(const Alert: TTLSAlert);
begin
  SetErrorBadProtocol;
end;

procedure TTLSConnection.HandleAlertCertificateError(const Alert: TTLSAlert);
begin
  SetErrorBadProtocol;
end;

procedure TTLSConnection.HandleAlertSecurityError(const Alert: TTLSAlert);
begin
  SetErrorBadProtocol;
end;

procedure TTLSConnection.HandleAlertUserCancelled;
begin
  SetConnectionState(tlscoCancelled);
end;

procedure TTLSConnection.HandleAlertNoRenegotiation;
begin
  SetErrorBadProtocol;
end;

procedure TTLSConnection.HandleAlertUnknown(const Alert: TTLSAlert);
begin
  if Alert.level = tlsalFatal then
    SetErrorBadProtocol;
end;

procedure TTLSConnection.HandleProtocolAlert(const Buffer; const Size: Integer);
var Alert : PTLSAlert;
begin
  Alert := @Buffer;

  {$IFDEF TLS_DEBUG}
  Log(tlsltDebug, 'R:Alert:%s:%s', [TLSAlertLevelToStr(Alert^.level),
                                    TLSAlertDescriptionToStr(Alert^.description)]);
  {$ENDIF}

  if Alert^.level = tlsalFatal then
    FConnectionErrorMessage :=
        TLSAlertLevelToStr(Alert^.level) + ':' +
        TLSAlertDescriptionToStr(Alert^.description);

  case Alert^.description of
    tlsadClose_notify            : HandleAlertCloseNotify;
    tlsadProtocol_version        : HandleAlertProtocolVersion;
    tlsadUnexpected_message,
    tlsadBad_record_mac,
    tlsadDecryption_failed,
    tlsadRecord_overflow,
    tlsadDecompression_failure,
    tlsadHandshake_failure,
    tlsadInternal_error,
    tlsadIllegal_parameter,
    tlsadDecode_error,
    tlsadDecrypt_error,
    tlsadUnsupported_extention   : HandleAlertProtocolFailure(Alert^);
    tlsadNo_certificate,
    tlsadBad_certificate,
    tlsadUnsupported_certificate,
    tlsadCertificate_revoked,
    tlsadCertificate_expired,
    tlsadCertificate_unknown,
    tlsadUnknown_ca              : HandleAlertCertificateError(Alert^);
    tlsadAccess_denied,
    tlsadExport_restriction,
    tlsadInsufficient_security   : HandleAlertSecurityError(Alert^);
    tlsadUser_canceled           : HandleAlertUserCancelled;
    tlsadNo_renegotiation        : HandleAlertNoRenegotiation;
    /////
  else
    HandleAlertUnknown(Alert^);
  end;

  TriggerAlert(Alert^.level, Alert^.description);
end;

procedure TTLSConnection.HandleProtocolChangeCipherSpec(const Buffer; const Size: Integer);
begin
  {$IFDEF TLS_DEBUG}
  Log(tlsltDebug, 'R:ChangeCipherSpec');
  {$ENDIF}

  ChangeDecryptCipherSpec;
end;

procedure TTLSConnection.HandleProtocolApplicationData(const Buffer; const Size: Integer);
begin
  TLSBufferAddBuf(FOutBuf, Buffer, Size);
end;

procedure TTLSConnection.HandleProtocolHandshake(const Buffer; const Size: Integer);
var P : PByte;
    N : Integer;
    MsgType : TTLSHandshakeType;
    Len : Integer;
begin
  try
    P := @Buffer;
    N := Size;
    repeat
      DecodeTLSHandshakeHeader(PTLSHandshakeHeader(P)^, MsgType, Len);
      if MsgType <> tlshtHello_request then
        AddVerifyHandshakeData(P^, TLSHandshakeHeaderSize + Len);
      Inc(P, TLSHandshakeHeaderSize);
      Dec(N, TLSHandshakeHeaderSize);

      {$IFDEF TLS_DEBUG}
      Log(tlsltDebug, 'R:Handshake:[%s]:%db', [TLSHandshakeTypeToStr(MsgType), Len]);
      {$ENDIF}

      HandleHandshakeMessage(MsgType, P^, Len);
      Inc(P, Len);
      Dec(N, Len);
    until N <= 0;
  except
    on E : ETLSError do
      ShutdownBadProtocol(E.AlertDescription)
    else
      ShutdownBadProtocol(tlsadHandshake_failure);
  end;
end;

procedure TTLSConnection.ProcessTransportLayerData;
var P, Q            : PByte;
    RecHeader       : PTLSRecordHeader;
    ContentType     : TTLSContentType;
    ProtocolVersion : TTLSProtocolVersion;
    RecLength       : Word;
    PlainSize       : Integer;
    PlainBuf        : array[0..TLS_CLIENT_RECORDBUF_MAXSIZE - 1] of Byte;
begin
  while TLSBufferUsed(FInBuf) >= TLSRecordHeaderSize do
    begin
      P := TLSBufferPtr(FInBuf);

      // decode header
      RecHeader := PTLSRecordHeader(P);
      DecodeTLSRecordHeader(RecHeader^, ContentType, ProtocolVersion, RecLength);

      // validate header
      if not (ContentType in [
          tlsctHandshake,
          tlsctAlert,
          tlsctApplication_data,
          tlsctChange_cipher_spec]) then
        ShutdownBadProtocol(tlsadUnexpected_message);

      // wait for complete record
      if TLSBufferUsed(FInBuf) < TLSRecordHeaderSize + RecLength then
        exit;

      // record received
      {$IFDEF TLS_DEBUG}
      Log(tlsltDebug, 'R:Record:[%s]:%db', [TLSContentTypeToStr(ContentType), RecLength]);
      {$ENDIF}

      try
        Inc(P, TLSRecordHeaderSize);
        DecodeTLSRecord(
           RecHeader,
           P^, RecLength,
           FProtocolVersion,
           FCipherDecryptSpec.Compression,
           FCipherDecryptSpec.CipherSuiteDetails,
           FReadSeqNo,
           Pointer(FDecMACKey)^, Length(FDecMACKey),
           FCipherDecryptState,
           Pointer(FDecIV), Length(FDecIV),
           PlainBuf, SizeOf(PlainBuf), PlainSize);
        TLSBufferDiscard(FInBuf, TLSRecordHeaderSize + RecLength);
        Inc(FReadSeqNo);

        // process
        Q := @PlainBuf;
        case ContentType of
          tlsctHandshake          : HandleProtocolHandshake(Q^, PlainSize);
          tlsctAlert              : HandleProtocolAlert(Q^, PlainSize);
          tlsctApplication_data   : HandleProtocolApplicationData(Q^, PlainSize);
          tlsctChange_cipher_spec : HandleProtocolChangeCipherSpec(Q^, PlainSize);
        else
          ShutdownBadProtocol(tlsadUnexpected_message);
        end;
      except
        on E : ETLSError do
          if E.AlertDescription = tlsadMax then
            ShutdownBadProtocol(tlsadDecode_error)
          else
            ShutdownBadProtocol(E.AlertDescription);
        else
          ShutdownBadProtocol(tlsadDecode_error);
      end;
      if IsFinishedState then
         exit;
    end;
  SecureClear(PlainBuf, SizeOf(PlainBuf));
end;

procedure TTLSConnection.ProcessTransportLayerReceivedData(const Buffer; const Size: Integer);
begin
  TLSBufferAddBuf(FInBuf, Buffer, Size);
  if IsFinishedState then
    raise ETLSError.Create(TLSError_InvalidState); // tls session finished

  ProcessTransportLayerData;
end;

function TTLSConnection.AvailableToRead: Integer;
begin
  Result := TLSBufferUsed(FOutBuf);
end;

function TTLSConnection.Read(var Buffer; const Size: Integer): Integer;
var L, N : Integer;
begin
  if Size <= 0 then
    begin
      Result := 0;
      exit;
    end;
  N := TLSBufferUsed(FOutBuf);
  if N = 0 then
    begin
      Result := 0;
      exit;
    end;
  if Size > N then
    L := N
  else
    L := Size;
  Result := TLSBufferRemove(FOutBuf, Buffer, L);
end;

procedure TTLSConnection.Write(const Buffer; const Size: Integer);
begin
  if Size <= 0 then
    exit;
  if IsFinishedState then
    raise ETLSError.Create(TLSError_InvalidState); // tls session finished

  if FConnectionState <> tlscoApplicationData then
    raise ETLSError.Create(TLSError_InvalidState); // cannot accept application data yet.. todo: buffer until negotiation finished?

  SendApplicationData(Buffer, Size);
end;

procedure TTLSConnection.Close;
begin
  if IsFinishedState then
    raise ETLSError.Create(TLSError_InvalidState); // not open

  DoClose;
end;



end.

