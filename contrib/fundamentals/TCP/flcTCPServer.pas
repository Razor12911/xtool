{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals 5.00                                        }
{   File name:        flcTCPServer.pas                                         }
{   File version:     5.27                                                     }
{   Description:      TCP server.                                              }
{                                                                              }
{   Copyright:        Copyright (c) 2007-2020, David J Butler                  }
{                     All rights reserved.                                     }
{                     This file is licensed under the BSD License.             }
{                     See http://www.opensource.org/licenses/bsd-license.php   }
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
{   2007/12/01  0.01  Initial development.                                     }
{   2010/11/07  0.02  Development.                                             }
{   2010/11/12  0.03  Refactor for asynchronous operation.                     }
{   2010/12/15  0.04  TLS support.                                             }
{   2010/12/20  0.05  Option to limit the number of clients.                   }
{   2010/12/29  0.06  Indicate when client is in the negotiating state.        }
{   2010/12/30  0.07  Separate control and process threads.                    }
{   2011/06/25  0.08  Improved logging.                                        }
{   2011/07/26  0.09  Improvements.                                            }
{   2011/09/03  4.10  Revise for Fundamentals 4.                               }
{   2013/01/28  4.11  Fix for restarting server.                               }
{   2015/04/25  4.12  OnReady event.                                           }
{   2015/04/26  4.13  Blocking interface and worker thread.                    }
{   2015/04/27  4.14  Whitelist/Blacklist.                                     }
{   2016/01/09  5.15  Revised for Fundamentals 5.                              }
{   2018/08/30  5.16  Trigger Close event when ready client is terminated.     }
{   2018/09/07  5.17  Implement ClientList as linked list.                     }
{   2018/09/07  5.18  Improve latency for large number of clients.             }
{   2018/09/10  5.19  Change polling to use Sockets Poll function.             }
{   2018/12/31  5.20  OnActivity events.                                       }
{   2019/04/10  5.21  String changes.                                          }
{   2019/04/16  5.22  Client shutdown events.                                  }
{   2019/05/19  5.23  Multiple processing threads.                             }
{   2019/10/06  5.24  Use TSimpleEvents to wait on process and controller      }
{                     threads. Improved latency.                               }
{   2019/12/30  5.25  MinReadBufferSize, MinWriteBuffersize,                   }
{                     SocketReadBufferSize and SocketWriteBufferSize.          }
{   2020/03/21  5.26  Remove address whitelist/blacklist.                      }
{   2020/05/02  5.27  Log exceptions raised in event handlers.                 }
{                                                                              }
{ Supported compilers:                                                         }
{                                                                              }
{   Delphi 2010-10.4 Win32/Win64        5.27  2020/06/02                       }
{   Delphi 10.2-10.4 Linux64            5.27  2020/06/02                       }
{   Delphi 10.2-10.4 iOS32/64           5.27  2020/06/02                       }
{   Delphi 10.2-10.4 OSX32/64           5.27  2020/06/02                       }
{   Delphi 10.2-10.4 Android32/64       5.27  2020/06/02                       }
{   FreePascal 3.0.4 Win64              5.27  2020/06/02                       }
{                                                                              }
{******************************************************************************}

{$INCLUDE ../flcInclude.inc}
{$INCLUDE flcTCP.inc}

unit flcTCPServer;

interface

uses
  { System }

  {$IFDEF DELPHI5}
  Windows,
  {$ENDIF}
  SysUtils,
  SyncObjs,
  Classes,

  { Utils }

  flcStdTypes,

  { Sockets }

  flcSocketLib,
  flcSocketLibSys,
  flcSocket,

  { TCP }

  flcTCPBuffer,
  flcTCPConnection

  { TLS }

  {$IFDEF TCPSERVER_TLS},
  flcTLSTransportTypes,
  flcTLSTransportConnection,
  flcTLSTransportServer
  {$ENDIF}
  ;



const
  TCP_SERVER_DEFAULT_MaxBacklog = 64;
  TCP_SERVER_DEFAULT_MaxClients = -1;
  TCP_SERVER_DEFAULT_ProcessThreadCount = 1;

  TCP_SERVER_DEFAULT_MinBufferSize = ETHERNET_MTU;     // 1500 bytes
  TCP_SERVER_DEFAULT_MaxBufferSize = ETHERNET_MTU * 6; // 9000 bytes

  TCP_SERVER_DEFAULT_SocketBufferSize = 0; // if 0 the default socket buffer size is not modified



{$IFDEF TCPSERVER_TLS}
type
  TTCPServerTLSOption = (
    stoNone
    );

  TTCPServerTLSOptions = set of TTCPServerTLSOption;

const
  DefaultTCPServerTLSOptions = [];
{$ENDIF}



type
  ETCPServer = class(Exception);

  TF5TCPServer = class;

  { TCP Server Client                                                          }

  TTCPServerClientState = (
      scsInit,
      scsStarting,
      scsNegotiating,
      scsReady,
      scsClosed
    );

  TTCPServerClient = class
  protected
    FServer         : TF5TCPServer;
    FPrev           : TTCPServerClient;
    FNext           : TTCPServerClient;
    FState          : TTCPServerClientState;
    FTerminated     : Boolean;
    FRemoteAddr     : TSocketAddr;
    FSocket         : TSysSocket;
    FConnection     : TTCPConnection;
    FReferenceCount : Integer;
    FOrphanClient   : Boolean;
    FClientID       : Int64;
    FPollIndex      : Integer;
    //// FPolEvents : Word16;
    FUserTag        : NativeInt;
    FUserObject     : TObject;

    {$IFDEF TCPSERVER_TLS}
    FTLSClient      : TTLSServerClient;
    FTLSProxy       : TTCPConnectionProxy;
    {$ENDIF}

    procedure Log(const LogType: TTCPLogType; const LogMsg: String; const LogLevel: Integer = 0); overload;
    procedure Log(const LogType: TTCPLogType; const LogMsg: String; const LogArgs: array of const; const LogLevel: Integer = 0); overload;

    function  GetState: TTCPServerClientState;
    function  GetStateStr: String;
    procedure SetState(const AState: TTCPServerClientState);

    procedure SetNegotiating;
    procedure SetReady;

    function  GetRemoteAddrStr: String;

    function  GetBlockingConnection: TTCPBlockingConnection;

    {$IFDEF TCPSERVER_TLS}
    procedure InstallTLSProxy;
    {$ENDIF}

    procedure ConnectionLog(Sender: TTCPConnection; LogType: TTCPLogType; LogMsg: String; LogLevel: Integer);

    procedure ConnectionStateChange(Sender: TTCPConnection; State: TTCPConnectionState);
    procedure ConnectionReady(Sender: TTCPConnection);
    procedure ConnectionReadShutdown(Sender: TTCPConnection);
    procedure ConnectionShutdown(Sender: TTCPConnection);
    procedure ConnectionClose(Sender: TTCPConnection);

    procedure ConnectionRead(Sender: TTCPConnection);
    procedure ConnectionWrite(Sender: TTCPConnection);
    procedure ConnectionReadActivity(Sender: TTCPConnection);
    procedure ConnectionWriteActivity(Sender: TTCPConnection);

    procedure ConnectionWorkerExecute(Sender: TTCPConnection;
              Connection: TTCPBlockingConnection; var CloseOnExit: Boolean);

    procedure TriggerStateChange;
    procedure TriggerNegotiating;
    procedure TriggerConnected;
    procedure TriggerReady;
    procedure TriggerReadShutdown;
    procedure TriggerShutdown;
    procedure TriggerClose;
    procedure TriggerRead;
    procedure TriggerWrite;
    procedure TriggerReadActivity;
    procedure TriggerWriteActivity;

    procedure Start;
    procedure Process(const ProcessRead, ProcessWrite: Boolean;
              const ActivityTime: TDateTime;
              var Idle, Terminated: Boolean);
    procedure AddReference;
    procedure SetClientOrphaned;

  public
    constructor Create(
                const AServer: TF5TCPServer;
                const ASocketHandle: TSocketHandle;
                const AClientID: Int64;
                const ARemoteAddr: TSocketAddr);
    destructor Destroy; override;
    procedure Finalise;

    property  State: TTCPServerClientState read GetState;
    property  StateStr: String read GetStateStr;
    property  Terminated: Boolean read FTerminated;

    // Connection has a non-blocking interface.
    // BlockingConnection has a blocking interface. It can be used from a
    // worker thread, it should not be used from an event handler.
    property  Connection: TTCPConnection read FConnection;
    property  BlockingConnection: TTCPBlockingConnection read GetBlockingConnection;

    procedure Close;
    procedure ReleaseReference;

    {$IFDEF TCPSERVER_TLS}
    property  TLSClient: TTLSServerClient read FTLSClient;
    procedure StartTLS;
    {$ENDIF}

    property  RemoteAddr: TSocketAddr read FRemoteAddr;
    property  RemoteAddrStr: String read GetRemoteAddrStr;

    property  ClientID: Int64 read FClientID;

    // Worker thread
    procedure TerminateWorkerThread;

    // User defined values
    property  UserTag: NativeInt read FUserTag write FUserTag;
    property  UserObject: TObject read FUserObject write FUserObject;
  end;

  TTCPServerClientClass = class of TTCPServerClient;



  { TCP Server Client List                                                     }

  TTCPServerClientList = class
  private
    FCount : Integer;
    FFirst : TTCPServerClient;
    FLast  : TTCPServerClient;

  public
    destructor Destroy; override;
    procedure Finalise;

    procedure Add(const Client: TTCPServerClient);
    procedure Remove(const Client: TTCPServerClient);
    property  First: TTCPServerClient read FFirst;
    property  Count: Integer read FCount;
  end;



  { TCP Server Poll List                                                       }
  { Poll list maintains poll buffer used in call to Poll.                      }

  TTCPServerPollList = class
  private
    FListLen     : Integer;
    FListUsed    : Integer;
    FClientCount : Integer;
    FFDList      : packed array of TPollfd;
    FClientList  : array of TTCPServerClient;

  public
    constructor Create;
    destructor Destroy; override;
    procedure Finalise;
    procedure AddPollEvent(const ASocket: TSysSocket);
    function  Add(const Client: TTCPServerClient): Integer;
    procedure Remove(const Idx: Integer);
    property  ClientCount: Integer read FClientCount;
    procedure GetPollBuffer(out P: Pointer; out ItemCount: Integer);
    function  GetClientByIndex(const Idx: Integer): TTCPServerClient; {$IFDEF UseInline}inline;{$ENDIF}
  end;

  { TCP Server Thread                                                          }

  TTCPServerThreadTask = (
      sttControl,
      sttProcess
    );

  TTCPServerThread = class(TThread)
  protected
    FServer : TF5TCPServer;
    FTask   : TTCPServerThreadTask;
    procedure Execute; override;
  public
    constructor Create(const AServer: TF5TCPServer; const ATask: TTCPServerThreadTask);
    procedure Finalise;
    property Terminated;
  end;

  { TCP Server                                                                 }

  TTCPServerState = (
      ssInit,
      ssStarting,
      ssReady,
      ssFailure,
      ssClosed
    );

  TTCPServerControlThreadState = (
      sctsInit,
      sctsPollReady,
      sctsPollProcess
    );

  {$IFDEF TCPSERVER_TLS}
  TTCPServerTLSServerOptions = TTLSServerOptions;
  TTCPServerTLSVersionOptions = TTLSVersionOptions;
  TTCPServerTLSKeyExchangeOptions = TTLSKeyExchangeOptions;
  TTCPServerTLSCipherOptions = TTLSCipherOptions;
  TTCPServerTLSHashOptions = TTLSHashOptions;
  {$ENDIF}

  TTCPServerNotifyEvent = procedure (AServer: TF5TCPServer) of object;
  TTCPServerLogEvent = procedure (AServer: TF5TCPServer; LogType: TTCPLogType;
      Msg: String; LogLevel: Integer) of object;
  TTCPServerStateEvent = procedure (AServer: TF5TCPServer; AState: TTCPServerState) of object;
  TTCPServerClientEvent = procedure (AClient: TTCPServerClient) of object;
  TTCPServerIdleEvent = procedure (AServer: TF5TCPServer; AThread: TTCPServerThread) of object;
  TTCPServerAcceptEvent = procedure (AServer: TF5TCPServer; AAddress: TSocketAddr;
      var AAcceptClient: Boolean) of object;
  TTCPServerClientWorkerExecuteEvent = procedure (AClient: TTCPServerClient;
      AConnection: TTCPBlockingConnection; var CloseOnExit: Boolean) of object;

  TF5TCPServer = class(TComponent)
  private
    // parameters
    FAddressFamily         : TIPAddressFamily;
    FBindAddressStr        : String;
    FServerPort            : Integer;
    FMaxBacklog            : Integer;
    FMaxClients            : Integer;
    FMinReadBufferSize     : Integer;
    FMaxReadBufferSize     : Integer;
    FMinWriteBufferSize    : Integer;
    FMaxWriteBufferSize    : Integer;
    FSocketReadBufferSize  : Integer;
    FSocketWriteBufferSize : Integer;
    FTrackLastActivityTime : Boolean;
    FProcessThreadCount    : Integer;
    {$IFDEF TCPSERVER_TLS}
    FTLSEnabled            : Boolean;
    FTLSOptions            : TTCPServerTLSOptions;
    FTLSServerOptions      : TTLSServerOptions;
    FTLSVersionOptions     : TTCPServerTLSVersionOptions;
    FTLSKeyExchangeOptions : TTCPServerTLSKeyExchangeOptions;
    FTLSCipherOptions      : TTCPServerTLSCipherOptions;
    FTLSHashOptions        : TTCPServerTLSHashOptions;
    {$ENDIF}
    FUseWorkerThread       : Boolean;
    FUserTag               : NativeInt;
    FUserObject            : TObject;

    FProcessProcessEvent   : TSimpleEvent;
    FProcessReadyEvent     : TSimpleEvent;
    FControlReadyEvent     : TSimpleEvent;

    // event handlers
    FOnLog                 : TTCPServerLogEvent;
    FOnStateChanged        : TTCPServerStateEvent;
    FOnStart               : TTCPServerNotifyEvent;
    FOnStop                : TTCPServerNotifyEvent;
    FOnThreadIdle          : TTCPServerIdleEvent;

    FOnClientAccept        : TTCPServerAcceptEvent;
    FOnClientCreate        : TTCPServerClientEvent;
    FOnClientAdd           : TTCPServerClientEvent;
    FOnClientRemove        : TTCPServerClientEvent;
    FOnClientDestroy       : TTCPServerClientEvent;

    FOnClientStateChange   : TTCPServerClientEvent;
    FOnClientNegotiating   : TTCPServerClientEvent;
    FOnClientConnected     : TTCPServerClientEvent;
    FOnClientReady         : TTCPServerClientEvent;
    FOnClientReadShutdown  : TTCPServerClientEvent;
    FOnClientShutdown      : TTCPServerClientEvent;
    FOnClientClose         : TTCPServerClientEvent;

    FOnClientRead          : TTCPServerClientEvent;
    FOnClientWrite         : TTCPServerClientEvent;
    FOnClientReadActivity  : TTCPServerClientEvent;
    FOnClientWriteActivity : TTCPServerClientEvent;

    FOnClientWorkerExecute : TTCPServerClientWorkerExecuteEvent;

    // state
    FLock                 : TCriticalSection;
    FActive               : Boolean;
    FActiveOnLoaded       : Boolean;
    FState                : TTCPServerState;
    FControlThread        : TTCPServerThread;
    FControlState         : TTCPServerControlThreadState;
    FProcessThreads       : array of TTCPServerThread;
    FProcessThreadsRun    : Integer;
    FProcessThreadsReady  : Integer;
    FServerSocket         : TSysSocket;
    FBindAddress          : TSocketAddr;
    FClientList           : TTCPServerClientList;
    FClientAcceptedList   : TTCPServerClientList;
    FClientTerminatedList : TTCPServerClientList;
    FPollList             : TTCPServerPollList;
    FPollTime             : TDateTime;
    FPollEntBuf           : Pointer;
    FPollEntCount         : Integer;
    FPollProcessIdx       : Integer;
    FClientIDCounter      : Int64;
    {$IFDEF TCPSERVER_TLS}
    FTLSServer            : TTLSServer;
    {$ENDIF}

  protected
    procedure Init; virtual;
    procedure InitDefaults; virtual;

    procedure Lock;
    procedure Unlock;

    procedure Log(const LogType: TTCPLogType; const Msg: String; const LogLevel: Integer = 0); overload;
    procedure Log(const LogType: TTCPLogType; const Msg: String; const Args: array of const; const LogLevel: Integer = 0); overload;
    procedure LogException(const Msg: String; const E: Exception);

    function  GetState: TTCPServerState;
    function  GetStateStr: String;
    procedure SetState(const AState: TTCPServerState);
    procedure CheckNotActive;

    procedure SetActive(const AActive: Boolean);
    procedure Loaded; override;

    procedure SetAddressFamily(const AAddressFamily: TIPAddressFamily);
    procedure SetBindAddress(const ABindAddressStr: String);
    procedure SetServerPort(const AServerPort: Integer);
    procedure SetMaxBacklog(const AMaxBacklog: Integer);
    procedure SetMaxClients(const AMaxClients: Integer);

    procedure SetMinReadBufferSize(const AMinReadBufferSize: Integer);
    procedure SetMaxReadBufferSize(const AMaxReadBufferSize: Integer);
    procedure SetMinWriteBufferSize(const AMinWriteBufferSize: Integer);
    procedure SetMaxWriteBufferSize(const AMaxWriteBufferSize: Integer);
    procedure SetSocketReadBufferSize(const ASocketReadBufferSize: Integer);
    procedure SetSocketWriteBufferSize(const ASocketWriteBufferSize: Integer);

    procedure SetTrackLastActivityTime(const Track: Boolean);

    procedure SetProcessThreadCount(const ThreadCount: Integer);

    {$IFDEF TCPSERVER_TLS}
    procedure SetTLSEnabled(const TLSEnabled: Boolean);
    procedure SetTLSOptions(const ATLSOptions: TTCPServerTLSOptions);
    procedure SetTLSServerOptions(const ATLSServerOptions: TTCPServerTLSServerOptions);
    procedure SetTLSVersionOptions(const ATLSVersionOptions: TTCPServerTLSVersionOptions);
    procedure SetTLSKeyExchangeOptions(const ATLSKeyExchangeOptions: TTCPServerTLSKeyExchangeOptions);
    procedure SetTLSCipherOptions(const ATLSCipherOptions: TTCPServerTLSCipherOptions);
    procedure SetTLSHashOptions(const ATLSHashOptions: TTCPServerTLSHashOptions);
    {$ENDIF}

    procedure SetUseWorkerThread(const UseWorkerThread: Boolean);

    procedure TriggerStart; virtual;
    procedure TriggerStop; virtual;

    procedure TriggerThreadIdle(const AThread: TTCPServerThread); virtual;

    procedure ServerSocketLog(Sender: TSysSocket; LogType: TSysSocketLogType; Msg: String);

    procedure ClientLog(const AClient: TTCPServerClient; const LogType: TTCPLogType; const LogMsg: String; const LogLevel: Integer);

    procedure TriggerClientAccept(const Address: TSocketAddr; var AcceptClient: Boolean); virtual;
    procedure TriggerClientCreate(const Client: TTCPServerClient); virtual;
    procedure TriggerClientAdd(const Client: TTCPServerClient); virtual;
    procedure TriggerClientRemove(const Client: TTCPServerClient); virtual;
    procedure TriggerClientDestroy(const Client: TTCPServerClient); virtual;
    procedure TriggerClientStateChange(const Client: TTCPServerClient); virtual;
    procedure TriggerClientNegotiating(const Client: TTCPServerClient); virtual;
    procedure TriggerClientConnected(const Client: TTCPServerClient); virtual;
    procedure TriggerClientReady(const Client: TTCPServerClient); virtual;
    procedure TriggerClientReadShutdown(const Client: TTCPServerClient); virtual;
    procedure TriggerClientShutdown(const Client: TTCPServerClient); virtual;
    procedure TriggerClientClose(const Client: TTCPServerClient); virtual;
    procedure TriggerClientRead(const Client: TTCPServerClient); virtual;
    procedure TriggerClientWrite(const Client: TTCPServerClient); virtual;
    procedure TriggerClientReadActivity(const Client: TTCPServerClient); virtual;
    procedure TriggerClientWriteActivity(const Client: TTCPServerClient); virtual;
    procedure TriggerClientWorkerExecute(const Client: TTCPServerClient;
              const Connection: TTCPBlockingConnection; var CloseOnExit: Boolean); virtual;

    procedure SetReady; virtual;
    procedure SetClosed; virtual;

    procedure DoCloseClients;
    procedure DoCloseServer;
    procedure DoClose;

    {$IFDEF TCPSERVER_TLS}
    procedure TLSServerTransportLayerSendProc(
              AServer: TTLSServer; AClient: TTLSServerClient;
              const Buffer; const Size: Integer);
    {$ENDIF}

    procedure StartControlThread;
    procedure StartProcessThreads;
    procedure StopServerThreads;

    procedure DoSetActive;
    procedure DoSetInactive;

    function  CreateClient(const ASocketHandle: TSocketHandle; const ASocketAddr: TSocketAddr): TTCPServerClient; virtual;

    function  CanAcceptClient: Boolean;
    function  ServerAcceptClient: Boolean;
    function  ServerDropClient: Boolean;
    procedure ProcessClient(
              const AClient: TTCPServerClient;
              const ProcessRead, ProcessWrite: Boolean;
              const ActivityTime: TDateTime;
              out ClientIdle, ClientTerminated: Boolean);
    function  ServerProcessClient: Boolean;
    procedure ServerPoll(out Idle: Boolean; out ProcessPending: Boolean);

    procedure ControlThreadExecute(const Thread: TTCPServerThread);
    procedure ProcessThreadExecute(const Thread: TTCPServerThread);

    procedure ThreadError(const Thread: TTCPServerThread; const Error: Exception);
    procedure ThreadTerminate(const Thread: TTCPServerThread);

    function  GetActiveClientCount: Integer;
    function  GetClientCount: Integer;

    function  GetReadRate: Int64;
    function  GetWriteRate: Int64;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Finalise; virtual;

    // Parameters
    property  AddressFamily: TIPAddressFamily read FAddressFamily write SetAddressFamily default iaIP4;
    property  BindAddress: String read FBindAddressStr write SetBindAddress;
    property  ServerPort: Integer read FServerPort write SetServerPort;
    property  MaxBacklog: Integer read FMaxBacklog write SetMaxBacklog default TCP_SERVER_DEFAULT_MaxBacklog;
    property  MaxClients: Integer read FMaxClients write SetMaxClients default TCP_SERVER_DEFAULT_MaxClients;

    property  MinReadBufferSize: Integer read FMinReadBufferSize write SetMinReadBufferSize default TCP_SERVER_DEFAULT_MinBufferSize;
    property  MaxReadBufferSize: Integer read FMaxReadBufferSize write SetMaxReadBufferSize default TCP_SERVER_DEFAULT_MaxBufferSize;
    property  MinWriteBufferSize: Integer read FMinWriteBufferSize write SetMinWriteBufferSize default TCP_SERVER_DEFAULT_MinBufferSize;
    property  MaxWriteBufferSize: Integer read FMaxWriteBufferSize write SetMaxWriteBufferSize default TCP_SERVER_DEFAULT_MaxBufferSize;

    property  SocketReadBufferSize: Integer read FSocketReadBufferSize write SetSocketReadBufferSize default TCP_SERVER_DEFAULT_SocketBufferSize;
    property  SocketWriteBufferSize: Integer read FSocketWriteBufferSize write SetSocketWriteBufferSize default TCP_SERVER_DEFAULT_SocketBufferSize;

    property  TrackLastActivityTime: Boolean read FTrackLastActivityTime write SetTrackLastActivityTime default True;
    property  ProcessThreadCount: Integer read FProcessThreadCount write SetProcessThreadCount default TCP_SERVER_DEFAULT_ProcessThreadCount;

    // TLS
    {$IFDEF TCPSERVER_TLS}
    property  TLSEnabled: Boolean read FTLSEnabled write SetTLSEnabled default False;
    property  TLSOptions: TTCPServerTLSOptions read FTLSOptions write SetTLSOptions default DefaultTCPServerTLSOptions;
    property  TLSServerOptions: TTCPServerTLSServerOptions read FTLSServerOptions write SetTLSServerOptions default DefaultTLSServerOptions;
    property  TLSVersionOptions: TTCPServerTLSVersionOptions read FTLSVersionOptions write SetTLSVersionOptions default DefaultTLSServerVersionOptions;
    property  TLSKeyExchangeOptions: TTCPServerTLSKeyExchangeOptions read FTLSKeyExchangeOptions write SetTLSKeyExchangeOptions default DefaultTLSServerKeyExchangeOptions;
    property  TLSCipherOptions: TTCPServerTLSCipherOptions read FTLSCipherOptions write SetTLSCipherOptions default DefaultTLSServerCipherOptions;
    property  TLSHashOptions: TTCPServerTLSHashOptions read FTLSHashOptions write SetTLSHashOptions default DefaultTLSServerHashOptions;
    property  TLSServer: TTLSServer read FTLSServer;
    {$ENDIF}

    // Event handlers may be triggered from any number of external threads.
    // Event handlers should do their own synchronisation if required.
    property  OnLog: TTCPServerLogEvent read FOnLog write FOnLog;

    property  OnStateChanged: TTCPServerStateEvent read FOnStateChanged write FOnStateChanged;
    property  OnStart: TTCPServerNotifyEvent read FOnStart write FOnStart;
    property  OnStop: TTCPServerNotifyEvent read FOnStop write FOnStop;
    property  OnThreadIdle: TTCPServerIdleEvent read FOnThreadIdle write FOnThreadIdle;

    property  OnClientAccept: TTCPServerAcceptEvent read FOnClientAccept write FOnClientAccept;
    property  OnClientCreate: TTCPServerClientEvent read FOnClientCreate write FOnClientCreate;
    property  OnClientAdd: TTCPServerClientEvent read FOnClientAdd write FOnClientAdd;
    property  OnClientRemove: TTCPServerClientEvent read FOnClientRemove write FOnClientRemove;
    property  OnClientDestroy: TTCPServerClientEvent read FOnClientDestroy write FOnClientDestroy;

    property  OnClientStateChange: TTCPServerClientEvent read FOnClientStateChange write FOnClientStateChange;
    property  OnClientNegotiating: TTCPServerClientEvent read FOnClientNegotiating write FOnClientNegotiating;
    property  OnClientConnected: TTCPServerClientEvent read FOnClientConnected write FOnClientConnected;
    property  OnClientReady: TTCPServerClientEvent read FOnClientReady write FOnClientReady;
    property  OnClientReadShutdown: TTCPServerClientEvent read FOnClientReadShutdown write FOnClientReadShutdown;
    property  OnClientShutdown: TTCPServerClientEvent read FOnClientShutdown write FOnClientShutdown;
    property  OnClientClose: TTCPServerClientEvent read FOnClientClose write FOnClientClose;

    property  OnClientRead: TTCPServerClientEvent read FOnClientRead write FOnClientRead;
    property  OnClientWrite: TTCPServerClientEvent read FOnClientWrite write FOnClientWrite;
    property  OnClientReadActivity: TTCPServerClientEvent read FOnClientReadActivity write FOnClientReadActivity;
    property  OnClientWriteActivity: TTCPServerClientEvent read FOnClientWriteActivity write FOnClientWriteActivity;

    // State
    property  State: TTCPServerState read GetState;
    property  StateStr: String read GetStateStr;
    property  Active: Boolean read FActive write SetActive default False;
    procedure Start;
    procedure Stop;

    property  ActiveClientCount: Integer read GetActiveClientCount;
    property  ClientCount: Integer read GetClientCount;
    function  ClientIterateFirst: TTCPServerClient;
    function  ClientIterateNext(const C: TTCPServerClient): TTCPServerClient;

    property  ReadRate: Int64 read GetReadRate;
    property  WriteRate: Int64 read GetWriteRate;

    // Worker thread
    // When UseWorkerThread is True, each client will have a worker thread
    // created when it is in the Ready state. OnClientWorkerExecute will
    // be called where the client can use the blocking connection interface.
    property  UseWorkerThread: Boolean read FUseWorkerThread write SetUseWorkerThread default False;
    property  OnClientWorkerExecute: TTCPServerClientWorkerExecuteEvent read FOnClientWorkerExecute write FOnClientWorkerExecute;

    // User defined values
    property  UserTag: NativeInt read FUserTag write FUserTag;
    property  UserObject: TObject read FUserObject write FUserObject;
  end;



{                                                                              }
{ Fundamentals Code Library TCP Server component                               }
{                                                                              }
type
  TfclTCPServer = class(TF5TCPServer)
  published
    property  Active;
    property  AddressFamily;
    property  BindAddress;
    property  ServerPort;
    property  MaxBacklog;
    property  MinReadBufferSize;
    property  MaxReadBufferSize;
    property  MinWriteBufferSize;
    property  MaxWriteBufferSize;

    {$IFDEF TCPSERVER_TLS}
    property  TLSEnabled;
    property  TLSOptions;
    property  TLSServerOptions;
    property  TLSVersionOptions;
    property  TLSKeyExchangeOptions;
    property  TLSCipherOptions;
    property  TLSHashOptions;
    {$ENDIF}

    property  OnLog;
    property  OnStateChanged;
    property  OnStart;
    property  OnStop;
    property  OnThreadIdle;

    property  OnClientAccept;
    property  OnClientCreate;
    property  OnClientAdd;
    property  OnClientRemove;
    property  OnClientDestroy;

    property  OnClientStateChange;
    property  OnClientNegotiating;
    property  OnClientReady;
    property  OnClientReadShutdown;
    property  OnClientShutdown;
    property  OnClientClose;

    property  OnClientRead;
    property  OnClientWrite;
    property  OnClientReadActivity;
    property  OnClientWriteActivity;

    property  UseWorkerThread;
    property  OnClientWorkerExecute;
  end;



implementation

{$IFDEF TCPSERVER_TLS}
uses
  { TLS }

  flcTLSConsts;
{$ENDIF}



{                                                                              }
{ Error and debug strings                                                      }
{                                                                              }
const
  SError_NotAllowedWhileActive = 'Operation not allowed while server is active';
  SError_InvalidServerPort     = 'Invalid server port';

  STCPServerState : array[TTCPServerState] of String = (
      'Initialise',
      'Starting',
      'Ready',
      'Failure',
      'Closed');

  STCPServerClientState : array[TTCPServerClientState] of String = (
      'Initialise',
      'Starting',
      'Negotiating',
      'Ready',
      'Closed');

  LogLevel_Client       = 2;
  LogLevel_Connection   = 2;
  LogLevel_ServerSocket = 5;



{$IFDEF TCPSERVER_TLS}
{                                                                              }
{ TCP Server Client TLS Connection Proxy                                       }
{                                                                              }
type
  TTCPServerClientTLSConnectionProxy = class(TTCPConnectionProxy)
  private
    FTLSServer : TTLSServer;
    FTLSClient : TTLSServerClient;

    procedure TLSClientTransportLayerSendProc(const Sender: TTLSConnection; const Buffer; const Size: Integer);
    procedure TLSClientLog(Sender: TTLSConnection; LogType: TTLSLogType; LogMsg: String; LogLevel: Integer);
    procedure TLSClientStateChange(Sender: TTLSConnection; State: TTLSConnectionState);

  public
    class function ProxyName: String; override;

    constructor Create(const ATLSServer: TTLSServer; const ATCPConnection: TTCPConnection);
    destructor Destroy; override;

    procedure ProxyStart; override;
    procedure ProcessReadData(const Buf; const BufSize: Integer); override;
    procedure ProcessWriteData(const Buf; const BufSize: Integer); override;
  end;

class function TTCPServerClientTLSConnectionProxy.ProxyName: String;
begin
  Result := 'TLSServerClient';
end;

constructor TTCPServerClientTLSConnectionProxy.Create(
            const ATLSServer: TTLSServer;
            const ATCPConnection: TTCPConnection);
begin
  Assert(Assigned(ATLSServer));
  Assert(Assigned(ATCPConnection));

  inherited Create;

  FTLSServer := ATLSServer;
  FTLSClient := ATLSServer.AddClient(self);
  {$IFDEF TCP_DEBUG_TLS}
  FTLSClient.OnLog := TLSClientLog;
  {$ENDIF}
  FTLSClient.OnStateChange := TLSClientStateChange;
end;

destructor TTCPServerClientTLSConnectionProxy.Destroy;
begin
  if Assigned(FTLSServer) and Assigned(FTLSClient) then
    FTLSServer.RemoveClient(FTLSClient);
  inherited Destroy;
end;

procedure TTCPServerClientTLSConnectionProxy.ProxyStart;
begin
  SetState(prsNegotiating);
  FTLSClient.Start;
end;

procedure TTCPServerClientTLSConnectionProxy.TLSClientTransportLayerSendProc(const Sender: TTLSConnection; const Buffer; const Size: Integer);
begin
  ConnectionPutWriteData(Buffer, Size);
end;

procedure TTCPServerClientTLSConnectionProxy.TLSClientLog(Sender: TTLSConnection; LogType: TTLSLogType; LogMsg: String; LogLevel: Integer);
begin
  {$IFDEF TCP_DEBUG_TLS}
  Log(tlDebug, Format('TLS:%s', [LogMsg]), LogLevel + 1);
  {$ENDIF}
end;

procedure TTCPServerClientTLSConnectionProxy.TLSClientStateChange(Sender: TTLSConnection; State: TTLSConnectionState);
begin
  case State of
    tlscoApplicationData : SetState(prsFiltering);
    tlscoCancelled,
    tlscoErrorBadProtocol :
      begin
        ConnectionClose;
        SetState(prsError);
      end;
    tlscoClosed :
      begin
        ConnectionClose;
        SetState(prsClosed);
      end;
  end;
end;

procedure TTCPServerClientTLSConnectionProxy.ProcessReadData(const Buf; const BufSize: Integer);
const
  ReadBufSize = TLS_PLAINTEXT_FRAGMENT_MAXSIZE * 2;
var
  ReadBuf : array[0..ReadBufSize - 1] of Byte;
  L : Integer;
begin
  {$IFDEF TCP_DEBUG_DATA}
  Log(tlDebug, 'ProcessReadData:%db', [BufSize]);
  {$ENDIF}

  FTLSClient.ProcessTransportLayerReceivedData(Buf, BufSize);
  repeat
    L := FTLSClient.AvailableToRead;
    if L > ReadBufSize then
      L := ReadBufSize;
    if L > 0 then
      begin
        L := FTLSClient.Read(ReadBuf, L);
        if L > 0 then
          ConnectionPutReadData(ReadBuf, L);
      end;
  until L <= 0;
end;

procedure TTCPServerClientTLSConnectionProxy.ProcessWriteData(const Buf; const BufSize: Integer);
begin
  {$IFDEF TCP_DEBUG_DATA}
  Log(tlDebug, 'ProcessWriteData:%db', [BufSize]);
  {$ENDIF}

  FTLSClient.Write(Buf, BufSize);
end;
{$ENDIF}



{                                                                              }
{ TCP Server Client                                                            }
{                                                                              }
constructor TTCPServerClient.Create(
            const AServer: TF5TCPServer;
            const ASocketHandle: TSocketHandle;
            const AClientID: Int64;
            const ARemoteAddr: TSocketAddr);
begin
  Assert(Assigned(AServer));
  Assert(ASocketHandle <> INVALID_SOCKETHANDLE);

  inherited Create;

  FState := scsInit;
  FServer := AServer;
  FClientID := AClientID;
  FSocket := TSysSocket.Create(AServer.FAddressFamily, ipTCP, False, ASocketHandle);
  FRemoteAddr := ARemoteAddr;

  FConnection := TTCPConnection.Create(
      FSocket,
      AServer.FMinReadBufferSize,
      AServer.FMaxReadBufferSize,
      AServer.FMinWriteBufferSize,
      AServer.FMaxWriteBufferSize);
  if FServer.FSocketReadBufferSize > 0 then
    FConnection.SocketReadBufferSize := FServer.FSocketReadBufferSize;
  if FServer.FSocketWriteBufferSize > 0 then
    FConnection.SocketWriteBufferSize := FServer.FSocketWriteBufferSize;
  FConnection.TrackLastActivityTime := AServer.FTrackLastActivityTime;
  if Assigned(FServer.FOnLog) then
    FConnection.OnLog := ConnectionLog;
  FConnection.OnStateChange    := ConnectionStateChange;
  FConnection.OnReady          := ConnectionReady;
  FConnection.OnReadShutdown   := ConnectionReadShutdown;
  FConnection.OnShutdown       := ConnectionShutdown;
  FConnection.OnClose          := ConnectionClose;
  FConnection.OnWorkerExecute  := ConnectionWorkerExecute;
  if Assigned(FServer.FOnClientRead) then
    FConnection.OnRead := ConnectionRead;
  if Assigned(FServer.FOnClientWrite) then
    FConnection.OnWrite := ConnectionWrite;
  if Assigned(FServer.FOnClientReadActivity) then
    FConnection.OnReadActivity := ConnectionReadActivity;
  if Assigned(FServer.FOnClientWriteActivity) then
    FConnection.OnWriteActivity := ConnectionWriteActivity;

  {$IFDEF TCPSERVER_TLS}
  if FServer.FTLSEnabled then
    InstallTLSProxy;
  {$ENDIF}
end;

destructor TTCPServerClient.Destroy;
begin
  if Assigned(FConnection) then
    FConnection.Finalise;
  FreeAndNil(FConnection);
  FreeAndNil(FSocket);
  inherited Destroy;
end;

procedure TTCPServerClient.Finalise;
begin
  FUserObject := nil;
  FNext := nil;
  FPrev := nil;
  FServer := nil;
end;

procedure TTCPServerClient.Log(const LogType: TTCPLogType; const LogMsg: String; const LogLevel: Integer);
begin
  if Assigned(FServer) then
    FServer.ClientLog(self, LogType, LogMsg, LogLevel);
end;

procedure TTCPServerClient.Log(const LogType: TTCPLogType; const LogMsg: String; const LogArgs: array of const; const LogLevel: Integer);
begin
  Log(LogType, Format(LogMsg, LogArgs), LogLevel);
end;

function TTCPServerClient.GetState: TTCPServerClientState;
begin
  Result := FState;
end;

function TTCPServerClient.GetStateStr: String;
begin
  Result := STCPServerClientState[GetState];
end;

procedure TTCPServerClient.SetState(const AState: TTCPServerClientState);
begin
  Assert(FState <> AState);
  FState := AState;

  {$IFDEF TCP_DEBUG}
  Log(tlDebug, 'State:%s', [STCPServerClientState[AState]]);
  {$ENDIF}
end;

procedure TTCPServerClient.SetNegotiating;
begin
  SetState(scsNegotiating);
  TriggerNegotiating;
end;

procedure TTCPServerClient.SetReady;
begin
  SetState(scsReady);
  TriggerReady;
end;

function TTCPServerClient.GetRemoteAddrStr: String;
begin
  Result := SocketAddrStr(FRemoteAddr);
end;

function TTCPServerClient.GetBlockingConnection: TTCPBlockingConnection;
begin
  Assert(Assigned(FConnection));
  Result := FConnection.BlockingConnection;
end;

{$IFDEF TCPSERVER_TLS}
procedure TTCPServerClient.InstallTLSProxy;
var
  Proxy : TTCPServerClientTLSConnectionProxy;
begin
  Assert(Assigned(FServer));

  {$IFDEF TCP_DEBUG_TLS}
  Log(tlDebug, 'InstallTLSProxy');
  {$ENDIF}

  Proxy := TTCPServerClientTLSConnectionProxy.Create(FServer.FTLSServer, FConnection);
  FTLSClient := Proxy.FTLSClient;
  FTLSProxy := Proxy;
  FConnection.AddProxy(Proxy);
end;
{$ENDIF}

procedure TTCPServerClient.ConnectionLog(Sender: TTCPConnection; LogType: TTCPLogType; LogMsg: String; LogLevel: Integer);
begin
  {$IFDEF TCP_DEBUG_CONNECTION}
  Log(LogType, 'Connection:%s', [LogMsg], LogLevel_Connection + LogLevel);
  {$ELSE}
  if LogType = tlError then //// 2020-05-05
    Log(LogType, 'Connection:%s', [LogMsg], LogLevel_Connection + LogLevel);
  {$ENDIF}
end;

procedure TTCPServerClient.ConnectionStateChange(Sender: TTCPConnection; State: TTCPConnectionState);
begin
  {$IFDEF TCP_DEBUG_CONNECTION}
  Log(tlDebug, 'Connection_StateChange:%s', [Sender.StateStr]);
  {$ENDIF}
  case State of
    cnsProxyNegotiation : SetNegotiating;
    cnsConnected        : SetReady;
  end;
  TriggerStateChange;
end;

procedure TTCPServerClient.ConnectionReady(Sender: TTCPConnection);
begin
  TriggerConnected;
end;

procedure TTCPServerClient.ConnectionReadShutdown(Sender: TTCPConnection);
begin
  TriggerReadShutdown;
end;

procedure TTCPServerClient.ConnectionShutdown(Sender: TTCPConnection);
begin
  TriggerShutdown;
end;

procedure TTCPServerClient.ConnectionClose(Sender: TTCPConnection);
begin
  {$IFDEF TCP_DEBUG_CONNECTION}
  Log(tlDebug, 'Connection_Close');
  {$ENDIF}
  if FState = scsClosed then
    exit;
  SetState(scsClosed);
  TriggerClose;
end;

procedure TTCPServerClient.ConnectionRead(Sender: TTCPConnection);
begin
  TriggerRead;
end;

procedure TTCPServerClient.ConnectionWrite(Sender: TTCPConnection);
begin
  TriggerWrite;
end;

procedure TTCPServerClient.ConnectionReadActivity(Sender: TTCPConnection);
begin
  TriggerReadActivity;
end;

procedure TTCPServerClient.ConnectionWriteActivity(Sender: TTCPConnection);
begin
  TriggerWriteActivity;
end;

procedure TTCPServerClient.ConnectionWorkerExecute(Sender: TTCPConnection;
          Connection: TTCPBlockingConnection;
          var CloseOnExit: Boolean);
begin
  if Assigned(FServer) then
    FServer.TriggerClientWorkerExecute(self, Connection, CloseOnExit);
end;

procedure TTCPServerClient.TriggerStateChange;
begin
  if Assigned(FServer) then
    FServer.TriggerClientStateChange(self);
end;

procedure TTCPServerClient.TriggerNegotiating;
begin
  if Assigned(FServer) then
    FServer.TriggerClientNegotiating(self);
end;

procedure TTCPServerClient.TriggerConnected;
begin
  if Assigned(FServer) then
    FServer.TriggerClientConnected(self);
end;

procedure TTCPServerClient.TriggerReady;
begin
  if Assigned(FServer) then
    FServer.TriggerClientReady(self);
end;

procedure TTCPServerClient.TriggerReadShutdown;
begin
  if Assigned(FServer) then
    FServer.TriggerClientReadShutdown(self);
end;

procedure TTCPServerClient.TriggerShutdown;
begin
  if Assigned(FServer) then
    FServer.TriggerClientShutdown(self);
end;

procedure TTCPServerClient.TriggerClose;
begin
  if Assigned(FServer) then
    FServer.TriggerClientClose(self);
end;

procedure TTCPServerClient.TriggerRead;
begin
  if Assigned(FServer) then
    FServer.TriggerClientRead(self);
end;

procedure TTCPServerClient.TriggerWrite;
begin
  if Assigned(FServer) then
    FServer.TriggerClientWrite(self);
end;

procedure TTCPServerClient.TriggerReadActivity;
begin
  if Assigned(FServer) then
    FServer.TriggerClientReadActivity(self);
end;

procedure TTCPServerClient.TriggerWriteActivity;
begin
  if Assigned(FServer) then
    FServer.TriggerClientWriteActivity(self);
end;

procedure TTCPServerClient.Start;
begin
  {$IFDEF TCP_DEBUG}
  Log(tlDebug, 'Start');
  {$ENDIF}
  SetState(scsStarting);
  FConnection.Start;
end;

procedure TTCPServerClient.Process(
          const ProcessRead, ProcessWrite: Boolean;
          const ActivityTime: TDateTime;
          var Idle, Terminated: Boolean);
begin
  //FServer.Lock; ////
  //try
    FConnection.ProcessSocket(ProcessRead, ProcessWrite, ActivityTime, Idle, Terminated);
  //finally
    //FServer.Unlock; ////
  //end;
  if Terminated then
    FTerminated := True;
end;

procedure TTCPServerClient.AddReference;
begin
  FServer.Lock;
  try
    Inc(FReferenceCount);
  finally
    FServer.Unlock;
  end;
end;

procedure TTCPServerClient.SetClientOrphaned;
begin
  Assert(not FOrphanClient);
  Assert(Assigned(FServer));

  FOrphanClient := True;
  FServer := nil;
end;

procedure TTCPServerClient.ReleaseReference;
begin
  if FOrphanClient then
    begin
      Dec(FReferenceCount);
      if FReferenceCount = 0 then
        begin
          Finalise;
          {$IFNDEF NEXTGEN}
          Free;
          {$ENDIF}
        end;
    end
  else
    begin
      Assert(Assigned(FServer));
      FServer.Lock;
      try
        if FReferenceCount = 0 then
          exit;
        Dec(FReferenceCount);
      finally
        FServer.Unlock;
      end;
    end;
end;

procedure TTCPServerClient.Close;
begin
  if FState = scsClosed then
    exit;
  {$IFDEF TCP_DEBUG}
  Log(tlDebug, 'Close');
  {$ENDIF}
  FSocket.Close;
  SetState(scsClosed);
  TriggerClose;
end;

{$IFDEF TCPSERVER_TLS}
procedure TTCPServerClient.StartTLS;
begin
  Assert(Assigned(FServer));

  if FServer.FTLSEnabled then
    exit;
  InstallTLSProxy;
end;
{$ENDIF}

procedure TTCPServerClient.TerminateWorkerThread;
begin
  if Assigned(FConnection) then
    FConnection.TerminateWorkerThread;
end;



{                                                                              }
{ TCP Server Client List                                                       }
{                                                                              }
{ This implementation uses a linked list to avoid any heap operations.         }
{                                                                              }
destructor TTCPServerClientList.Destroy;
var
  Iter, Next : TTCPServerClient;
begin
  Iter := First;
  FFirst := nil;
  FLast := nil;
  while Assigned(Iter) do
    begin
      Next := Iter.FNext;
      Iter.FPrev := nil;
      Iter.FNext := nil;
      if Iter.FReferenceCount = 0 then
        Iter.Free
      else
        Iter.SetClientOrphaned;
      Iter := Next;
    end;
  inherited Destroy;
end;

procedure TTCPServerClientList.Finalise;
begin
end;

procedure TTCPServerClientList.Add(const Client: TTCPServerClient);
var
  Last : TTCPServerClient;
begin
  Assert(Assigned(Client));
  Last := FLast;
  Client.FNext := nil;
  Client.FPrev := Last;
  if Assigned(Last) then
    Last.FNext := Client
  else
    FFirst := Client;
  FLast := Client;
  Inc(FCount);
end;

procedure TTCPServerClientList.Remove(const Client: TTCPServerClient);
var
  Prev, Next : TTCPServerClient;
begin
  Assert(Assigned(Client));
  Assert(FCount > 0);
  Prev := Client.FPrev;
  Next := Client.FNext;
  if Assigned(Prev) then
    begin
      Prev.FNext := Next;
      Client.FPrev := nil;
    end
  else
    begin
      Assert(FFirst = Client);
      FFirst := Next;
    end;
  if Assigned(Next) then
    begin
      Next.FPrev := Prev;
      Client.FNext := nil;
    end
  else
    begin
      Assert(FLast = Client);
      FLast := Prev;
    end;
  Dec(FCount);
end;



{                                                                              }
{ TCP Server Poll List                                                         }
{                                                                              }
{ This implementation aims to:                                                 }
{   - Keep a populated buffer ready for use in calls to Poll (one entry for    }
{     every active client).                                                    }
{   - Avoid heap operations for calls to frequently used operations Add        }
{     and Remove.                                                              }
{                                                                              }
constructor TTCPServerPollList.Create;
begin
  inherited Create;
end;

destructor TTCPServerPollList.Destroy;
begin
  Finalise;
  inherited Destroy;
end;

procedure TTCPServerPollList.Finalise;
begin
  FFDList := nil;
  FClientList := nil;
end;

procedure TTCPServerPollList.AddPollEvent(const ASocket: TSysSocket);
var
  SocketHandle : TSocket;
begin
  SocketHandle := ASocket.SocketHandle;
  SetLength(FFDList, 1);
  SetLength(FClientList, 1);
  FFDList[0].fd := SocketHandle;
  FFDList[0].events := POLLIN;
  FFDList[0].revents := 0;
  FClientList[0] := nil;
  FClientCount := 1;
  FListLen := 1;
  FListUsed := 1;
end;

function TTCPServerPollList.Add(const Client: TTCPServerClient): Integer;
var
  SocketHandle : TSocket;
  Idx, I, N, L : Integer;
begin
  SocketHandle := Client.FSocket.SocketHandle;
  if FClientCount < FListUsed then
    begin
      Idx := -1;
      for I := 0 to FListUsed - 1 do
        if not Assigned(FClientList[I]) then
          begin
            Idx := I;
            break;
          end;
      if Idx < 0 then
        raise ETCPServer.Create('Internal error');
    end
  else
  if FListUsed < FListLen then
    begin
      Idx := FListUsed;
      Inc(FListUsed);
    end
  else
    begin
      N := FListLen;
      L := N;
      if L < 16 then
        L := 16
      else
        L := L * 2;
      SetLength(FFDList, L);
      SetLength(FClientList, L);
      for I := N to L - 1 do
        FClientList[I] := nil;
      FListLen := L;
      Idx := FListUsed;
      Inc(FListUsed);
    end;
  FClientList[Idx] := Client;
  FFDList[Idx].fd := SocketHandle;
  FFDList[Idx].events := POLLIN or POLLOUT;
  FFDList[Idx].revents := 0;
  Inc(FClientCount);
  Result := Idx;
end;

procedure TTCPServerPollList.Remove(const Idx: Integer);
begin
  if (Idx < 0) or (Idx >= FListUsed) or not Assigned(FClientList[Idx]) then
    raise ETCPServer.Create('Invalid index');
  FClientList[Idx] := nil;
  FFDList[Idx].fd := INVALID_SOCKET;
  FFDList[Idx].events := 0;
  FFDList[Idx].revents := 0;
  Dec(FClientCount);
  if Idx = FListUsed - 1 then
    while (FListUsed > 0) and not Assigned(FClientList[FListUsed - 1]) do
      Dec(FListUsed);
end;

// Returns buffer to be passed to Poll in P
procedure TTCPServerPollList.GetPollBuffer(out P: Pointer; out ItemCount: Integer);
begin
  P := Pointer(FFDList);
  ItemCount := FListUsed;
end;

function TTCPServerPollList.GetClientByIndex(const Idx: Integer): TTCPServerClient;
begin
  Assert(Idx >= 0);
  Assert(Idx < FListUsed);
  Result := FClientList[Idx];
end;



{                                                                              }
{ TCP Server Thread                                                            }
{                                                                              }
constructor TTCPServerThread.Create(const AServer: TF5TCPServer; const ATask: TTCPServerThreadTask);
begin
  Assert(Assigned(AServer));
  FServer := AServer;
  FTask := ATask;
  FreeOnTerminate := False;
  inherited Create(False);
end;

procedure TTCPServerThread.Finalise;
begin
  FServer := nil;
end;

procedure TTCPServerThread.Execute;
begin
  Assert(Assigned(FServer));
  try
    try
      case FTask of
        sttControl : FServer.ControlThreadExecute(self);
        sttProcess : FServer.ProcessThreadExecute(self);
      end;
    except
      on E : Exception do
        if not Terminated then
          FServer.ThreadError(self, E);
    end;
  finally
    if not Terminated then
      FServer.ThreadTerminate(self);
    FServer := nil;
  end;
end;



{                                                                              }
{ TCP Server                                                                   }
{                                                                              }
constructor TF5TCPServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Init;
end;

procedure TF5TCPServer.Init;
begin
  FState := ssInit;
  FActiveOnLoaded := False;
  FLock := TCriticalSection.Create;
  FClientList := TTCPServerClientList.Create;
  FClientAcceptedList := TTCPServerClientList.Create;
  FClientTerminatedList := TTCPServerClientList.Create;
  FPollList := TTCPServerPollList.Create;
  {$IFDEF TCPSERVER_TLS}
  FTLSServer := TTLSServer.Create(TLSServerTransportLayerSendProc);
  {$ENDIF}
  FProcessProcessEvent := TSimpleEvent.Create;
  FProcessReadyEvent := TSimpleEvent.Create;
  FControlReadyEvent := TSimpleEvent.Create;
  InitDefaults;
end;

procedure TF5TCPServer.InitDefaults;
begin
  FActive := False;
  FAddressFamily := iaIP4;
  FBindAddressStr := '0.0.0.0';
  FMaxBacklog := TCP_SERVER_DEFAULT_MaxBacklog;
  FMaxClients := TCP_SERVER_DEFAULT_MaxClients;
  FMinReadBufferSize := TCP_SERVER_DEFAULT_MinBufferSize;
  FMaxReadBufferSize := TCP_SERVER_DEFAULT_MaxBufferSize;
  FMinWriteBufferSize := TCP_SERVER_DEFAULT_MinBufferSize;
  FMaxWriteBufferSize := TCP_SERVER_DEFAULT_MaxBufferSize;
  FSocketReadBufferSize := TCP_SERVER_DEFAULT_SocketBufferSize;
  FSocketWriteBufferSize := TCP_SERVER_DEFAULT_SocketBufferSize;
  FTrackLastActivityTime := True;
  FProcessThreadCount := TCP_SERVER_DEFAULT_ProcessThreadCount;
  {$IFDEF TCPSERVER_TLS}
  FTLSEnabled := False;
  FTLSOptions := DefaultTCPServerTLSOptions;
  FTLSServerOptions := DefaultTLSServerOptions;
  FTLSVersionOptions := DefaultTLSServerVersionOptions;
  FTLSKeyExchangeOptions := DefaultTLSServerKeyExchangeOptions;
  FTLSCipherOptions := DefaultTLSServerCipherOptions;
  FTLSHashOptions := DefaultTLSServerHashOptions;
  {$ENDIF}
end;

destructor TF5TCPServer.Destroy;
begin
  Finalise;
  FreeAndNil(FClientTerminatedList);
  FreeAndNil(FClientAcceptedList);
  FreeAndNil(FClientList);
  FreeAndNil(FServerSocket);
  FreeAndNil(FLock);
  inherited Destroy;
end;

procedure TF5TCPServer.Finalise;
var
  Iter : TTCPServerClient;
begin
  try
    StopServerThreads;
    if Assigned(FClientList) then
      begin
        Iter := FClientList.First;
        while Assigned(Iter) do
          begin
            Iter.TerminateWorkerThread;
            Iter := Iter.FNext;
          end;
      end;
    FreeAndNil(FProcessProcessEvent);
    FreeAndNil(FProcessReadyEvent);
    FreeAndNil(FControlReadyEvent);
  except
    {$IFDEF TCP_DEBUG} raise; {$ELSE}
    on E : Exception do
       LogException('Error stopping threads: %s', E); {$ENDIF}
  end;
  {$IFDEF TCPSERVER_TLS}
  FreeAndNil(FTLSServer);
  {$ENDIF}
  if Assigned(FPollList) then
    begin
      FPollList.Finalise;
      FreeAndNil(FPollList);
    end;
  FUserObject := nil;
end;

procedure TF5TCPServer.Lock;
begin
  Assert(Assigned(FLock));
  FLock.Acquire;
end;

procedure TF5TCPServer.Unlock;
begin
  Assert(Assigned(FLock));
  FLock.Release;
end;

procedure TF5TCPServer.Log(const LogType: TTCPLogType; const Msg: String; const LogLevel: Integer);
begin
  if Assigned(FOnLog) then
    FOnLog(self, LogType, Msg, LogLevel);
end;

procedure TF5TCPServer.Log(const LogType: TTCPLogType; const Msg: String; const Args: array of const; const LogLevel: Integer);
begin
  Log(LogType, Format(Msg, Args), LogLevel);
end;

procedure TF5TCPServer.LogException(const Msg: String; const E: Exception);
begin
  Log(tlError, Msg, [E.Message]);
end;

function TF5TCPServer.GetState: TTCPServerState;
begin
  Lock;
  try
    Result := FState;
  finally
    Unlock;
  end;
end;

function TF5TCPServer.GetStateStr: String;
begin
  Result := STCPServerState[GetState];
end;

procedure TF5TCPServer.SetState(const AState: TTCPServerState);
begin
  Lock;
  try
    Assert(FState <> AState);
    FState := AState;
  finally
    Unlock;
  end;
  {$IFDEF TCP_LOG_SERVERSTATE}
  Log(tlInfo, 'State=%s', [GetStateStr]);
  {$ENDIF}
  if Assigned(FOnStateChanged) then
    FOnStateChanged(self, AState);
end;

procedure TF5TCPServer.CheckNotActive;
begin
  if not (csDesigning in ComponentState) then
    if FActive then
      raise ETCPServer.Create(SError_NotAllowedWhileActive);
end;

procedure TF5TCPServer.SetActive(const AActive: Boolean);
begin
  if AActive = FActive then
    exit;
  if csDesigning in ComponentState then
    FActive := AActive else
  if csLoading in ComponentState then
    FActiveOnLoaded := AActive
  else
    if AActive then
      DoSetActive
    else
      DoSetInactive;
end;

procedure TF5TCPServer.Loaded;
begin
  inherited Loaded;
  if FActiveOnLoaded then
    DoSetActive;
end;

procedure TF5TCPServer.SetAddressFamily(const AAddressFamily: TIPAddressFamily);
begin
  if AAddressFamily = FAddressFamily then
    exit;
  CheckNotActive;
  FAddressFamily := AAddressFamily;
  {$IFDEF TCP_LOG_PARAMETERS}
  Log(tlParameter, 'AddressFamily:%s', [IPAddressFamilyStr[AddressFamily]]);
  {$ENDIF}
end;

procedure TF5TCPServer.SetBindAddress(const ABindAddressStr: String);
begin
  if ABindAddressStr = FBindAddressStr then
    exit;
  CheckNotActive;
  FBindAddressStr := ABindAddressStr;
  {$IFDEF TCP_LOG_PARAMETERS}
  Log(tlParameter, 'BindAddress:%s', [BindAddressStr]);
  {$ENDIF}
end;

procedure TF5TCPServer.SetServerPort(const AServerPort: Integer);
begin
  if AServerPort = FServerPort then
    exit;
  CheckNotActive;
  if (AServerPort <= 0) or (AServerPort > $FFFF) then
    raise ETCPServer.Create(SError_InvalidServerPort);
  FServerPort := AServerPort;
  {$IFDEF TCP_LOG_PARAMETERS}
  Log(tlParameter, 'ServerPort:%d', [ServerPort]);
  {$ENDIF}
end;

procedure TF5TCPServer.SetMaxBacklog(const AMaxBacklog: Integer);
begin
  if AMaxBacklog = FMaxBacklog then
    exit;
  CheckNotActive;
  FMaxBacklog := AMaxBacklog;
  {$IFDEF TCP_LOG_PARAMETERS}
  Log(tlParameter, 'MaxBacklog:%d', [MaxBacklog]);
  {$ENDIF}
end;

procedure TF5TCPServer.SetMaxClients(const AMaxClients: Integer);
begin
  if AMaxClients = FMaxClients then
    exit;
  Lock;
  try
    FMaxClients := AMaxClients;
  finally
    Unlock;
  end;
  {$IFDEF TCP_LOG_PARAMETERS}
  Log(tlParameter, 'MaxClients:%d', [MaxClients]);
  {$ENDIF}
end;

procedure TF5TCPServer.SetMinReadBufferSize(const AMinReadBufferSize: Integer);
begin
  if AMinReadBufferSize = FMinReadBufferSize then
    exit;
  CheckNotActive;
  FMinReadBufferSize := AMinReadBufferSize;
  {$IFDEF TCP_LOG_PARAMETERS}
  Log(tlParameter, 'MinReadBufferSize:%d', [MinReadBufferSize]);
  {$ENDIF}
end;

procedure TF5TCPServer.SetMaxReadBufferSize(const AMaxReadBufferSize: Integer);
begin
  if AMaxReadBufferSize = FMaxReadBufferSize then
    exit;
  CheckNotActive;
  FMaxReadBufferSize := AMaxReadBufferSize;
  {$IFDEF TCP_LOG_PARAMETERS}
  Log(tlParameter, 'MaxReadBufferSize:%d', [MaxReadBufferSize]);
  {$ENDIF}
end;

procedure TF5TCPServer.SetMinWriteBufferSize(const AMinWriteBufferSize: Integer);
begin
  if AMinWriteBufferSize = FMinWriteBufferSize then
    exit;
  CheckNotActive;
  FMinWriteBufferSize := AMinWriteBufferSize;
  {$IFDEF TCP_LOG_PARAMETERS}
  Log(tlParameter, 'MinWriteBufferSize:%d', [MinWriteBufferSize]);
  {$ENDIF}
end;

procedure TF5TCPServer.SetMaxWriteBufferSize(const AMaxWriteBufferSize: Integer);
begin
  if AMaxWriteBufferSize = FMaxWriteBufferSize then
    exit;
  CheckNotActive;
  FMaxWriteBufferSize := AMaxWriteBufferSize;
  {$IFDEF TCP_LOG_PARAMETERS}
  Log(tlParameter, 'MaxWriteBufferSize:%d', [MaxWriteBufferSize]);
  {$ENDIF}
end;

procedure TF5TCPServer.SetSocketReadBufferSize(const ASocketReadBufferSize: Integer);
begin
  if ASocketReadBufferSize = FSocketReadBufferSize then
    exit;
  CheckNotActive;
  FSocketReadBufferSize := ASocketReadBufferSize;
  {$IFDEF TCP_LOG_PARAMETERS}
  Log(tlParameter, 'SocketReadBufferSize:%d', [SocketReadBufferSize]);
  {$ENDIF}
end;

procedure TF5TCPServer.SetSocketWriteBufferSize(const ASocketWriteBufferSize: Integer);
begin
  if ASocketWriteBufferSize = FSocketWriteBufferSize then
    exit;
  CheckNotActive;
  FSocketWriteBufferSize := ASocketWriteBufferSize;
  {$IFDEF TCP_LOG_PARAMETERS}
  Log(tlParameter, 'SocketWriteBufferSize:%d', [SocketWriteBufferSize]);
  {$ENDIF}
end;

procedure TF5TCPServer.SetTrackLastActivityTime(const Track: Boolean);
begin
  if Track = FTrackLastActivityTime then
    exit;
  CheckNotActive;
  FTrackLastActivityTime := Track;
  {$IFDEF TCP_LOG_PARAMETERS}
  Log(tlParameter, 'TrackLastActivityTime:%d', [Ord(Track)]);
  {$ENDIF}
end;

procedure TF5TCPServer.SetProcessThreadCount(const ThreadCount: Integer);
begin
  if ThreadCount = FProcessThreadCount then
    exit;
  CheckNotActive;
  FProcessThreadCount := ThreadCount;
  {$IFDEF TCP_LOG_PARAMETERS}
  Log(tlParameter, 'ProcessThreadCount:%d', [ThreadCount]);
  {$ENDIF}
end;

{$IFDEF TCPSERVER_TLS}
procedure TF5TCPServer.SetTLSEnabled(const TLSEnabled: Boolean);
begin
  if TLSEnabled = FTLSEnabled then
    exit;
  CheckNotActive;
  FTLSEnabled := TLSEnabled;
  {$IFDEF TCP_LOG_PARAMETERS}
  Log(tlParameter, 'TLSEnabled:%d', [Ord(TLSEnabled)]);
  {$ENDIF}
end;

procedure TF5TCPServer.SetTLSOptions(const ATLSOptions: TTCPServerTLSOptions);
begin
  if ATLSOptions = FTLSOptions then
    exit;
  CheckNotActive;
  FTLSOptions := ATLSOptions;
end;

procedure TF5TCPServer.SetTLSServerOptions(const ATLSServerOptions: TTCPServerTLSServerOptions);
begin
  if ATLSServerOptions = FTLSServerOptions then
    exit;
  CheckNotActive;
  FTLSServerOptions := ATLSServerOptions;
end;

procedure TF5TCPServer.SetTLSVersionOptions(const ATLSVersionOptions: TTCPServerTLSVersionOptions);
begin
  if ATLSVersionOptions = FTLSVersionOptions then
    exit;
  CheckNotActive;
  FTLSVersionOptions := ATLSVersionOptions;
end;

procedure TF5TCPServer.SetTLSKeyExchangeOptions(const ATLSKeyExchangeOptions: TTCPServerTLSKeyExchangeOptions);
begin
  if ATLSKeyExchangeOptions = FTLSKeyExchangeOptions then
    exit;
  CheckNotActive;
  FTLSKeyExchangeOptions := ATLSKeyExchangeOptions;
end;

procedure TF5TCPServer.SetTLSCipherOptions(const ATLSCipherOptions: TTCPServerTLSCipherOptions);
begin
  if ATLSCipherOptions = FTLSCipherOptions then
    exit;
  CheckNotActive;
  FTLSCipherOptions := ATLSCipherOptions;
end;

procedure TF5TCPServer.SetTLSHashOptions(const ATLSHashOptions: TTCPServerTLSHashOptions);
begin
  if ATLSHashOptions = FTLSHashOptions then
    exit;
  CheckNotActive;
  FTLSHashOptions := ATLSHashOptions;
end;
{$ENDIF}

procedure TF5TCPServer.SetUseWorkerThread(const UseWorkerThread: Boolean);
begin
  if UseWorkerThread = FUseWorkerThread then
    exit;
  CheckNotActive;
  FUseWorkerThread := UseWorkerThread;
  {$IFDEF TCP_LOG_PARAMETERS}
  Log(tlParameter, 'UseWorkerThread:%d', [UseWorkerThread]);
  {$ENDIF}
end;

procedure TF5TCPServer.TriggerStart;
begin
  if Assigned(FOnStart) then
    FOnStart(self);
end;

procedure TF5TCPServer.TriggerStop;
begin
  if Assigned(FOnStop) then
    FOnStop(self);
end;

procedure TF5TCPServer.TriggerThreadIdle(const AThread: TTCPServerThread);
begin
  if Assigned(FOnThreadIdle) then
    FOnThreadIdle(self, AThread)
  else
    Sleep(1);
end;

procedure TF5TCPServer.ServerSocketLog(Sender: TSysSocket; LogType: TSysSocketLogType; Msg: String);
begin
  {$IFDEF TCP_DEBUG_SOCKET}
  Log(tlDebug, 'ServerSocket:%s', [Msg], LogLevel_Socket);
  {$ENDIF}
end;

procedure TF5TCPServer.ClientLog(const AClient: TTCPServerClient; const LogType: TTCPLogType; const LogMsg: String; const LogLevel: Integer);
begin
  Assert(Assigned(AClient));
  {$IFDEF TCP_DEBUG}
  Log(LogType, 'Client[%d]:%s', [Client.ClientID, LogMsg], LogLevel_Client + LogLevel);
  {$ENDIF}
end;

procedure TF5TCPServer.TriggerClientAccept(const Address: TSocketAddr; var AcceptClient: Boolean);
begin
  if Assigned(FOnClientAccept) then
    try
      FOnClientAccept(self, Address, AcceptClient);
    except
      {$IFDEF TCP_DEBUG} raise; {$ELSE}
      on E : Exception do
        Log(tlError, 'TriggerClientAccept.Error:Address=%s,Error=%s[%s]',
            [SocketAddrStr(Address), E.ClassName, E.Message]);
      {$ENDIF}
    end;
end;

procedure TF5TCPServer.TriggerClientCreate(const Client: TTCPServerClient);
begin
  if Assigned(FOnClientCreate) then
    try
      FOnClientCreate(Client);
    except
      {$IFDEF TCP_DEBUG} raise; {$ELSE}
      on E : Exception do
        Log(tlError, 'TriggerClientCreate.Error:Client=%d,Error=%s[%s]',
            [Client.ClientID, E.ClassName, E.Message]);
      {$ENDIF}
    end;
end;

procedure TF5TCPServer.TriggerClientAdd(const Client: TTCPServerClient);
begin
  if Assigned(FOnClientAdd) then
    try
      FOnClientAdd(Client);
    except
      {$IFDEF TCP_DEBUG} raise; {$ELSE}
      on E : Exception do
        Log(tlError, 'TriggerClientAdd.Error:Client=%d,Error=%s[%s]',
            [Client.ClientID, E.ClassName, E.Message]);
      {$ENDIF}
    end;
end;

procedure TF5TCPServer.TriggerClientRemove(const Client: TTCPServerClient);
begin
  if Assigned(FOnClientRemove) then
    try
      FOnClientRemove(Client);
    except
      {$IFDEF TCP_DEBUG} raise; {$ELSE}
      on E : Exception do
        Log(tlError, 'TriggerClientRemove.Error:Client=%d,Error=%s[%s]',
            [Client.ClientID, E.ClassName, E.Message]);
      {$ENDIF}
    end;
end;

procedure TF5TCPServer.TriggerClientDestroy(const Client: TTCPServerClient);
begin
  if Assigned(FOnClientDestroy) then
    try
      FOnClientDestroy(Client);
    except
      {$IFDEF TCP_DEBUG} raise; {$ELSE}
      on E : Exception do
        Log(tlError, 'TriggerClientDestroy.Error:Client=%d,Error=%s[%s]',
            [Client.ClientID, E.ClassName, E.Message]);
      {$ENDIF}
    end;
end;

procedure TF5TCPServer.TriggerClientStateChange(const Client: TTCPServerClient);
begin
  if Assigned(FOnClientStateChange) then
    try
      FOnClientStateChange(Client);
    except
      {$IFDEF TCP_DEBUG} raise; {$ELSE}
      on E : Exception do
        Log(tlError, 'TriggerClientStateChange.Error:Client=%d,Error=%s[%s]',
            [Client.ClientID, E.ClassName, E.Message]);
      {$ENDIF}
    end;
end;

procedure TF5TCPServer.TriggerClientNegotiating(const Client: TTCPServerClient);
begin
  if Assigned(FOnClientNegotiating) then
    FOnClientNegotiating(Client);
end;

procedure TF5TCPServer.TriggerClientConnected(const Client: TTCPServerClient);
begin
  if Assigned(FOnClientConnected) then
    try
      FOnClientConnected(Client);
    except
      {$IFDEF TCP_DEBUG} raise; {$ELSE}
      on E : Exception do
        Log(tlError, 'TriggerClientConnected.Error:Client=%d,Error=%s[%s]',
            [Client.ClientID, E.ClassName, E.Message]);
      {$ENDIF}
    end;
end;

procedure TF5TCPServer.TriggerClientReady(const Client: TTCPServerClient);
begin
  if Assigned(FOnClientReady) then
    try
      FOnClientReady(Client);
    except
      {$IFDEF TCP_DEBUG} raise; {$ELSE}
      on E : Exception do
        Log(tlError, 'TriggerClientReady.Error:Client=%d,Error=%s[%s]',
            [Client.ClientID, E.ClassName, E.Message]);
      {$ENDIF}
    end;
end;

procedure TF5TCPServer.TriggerClientReadShutdown(const Client: TTCPServerClient);
begin
  if Assigned(FOnClientReadShutdown) then
    try
      FOnClientReadShutdown(Client);
    except
      {$IFDEF TCP_DEBUG} raise; {$ELSE}
      on E : Exception do
        Log(tlError, 'TriggerClientReadShutdown.Error:Client=%d,Error=%s[%s]',
            [Client.ClientID, E.ClassName, E.Message]);
      {$ENDIF}
    end;
end;

procedure TF5TCPServer.TriggerClientShutdown(const Client: TTCPServerClient);
begin
  if Assigned(FOnClientShutdown) then
    try
      FOnClientShutdown(Client);
    except
      {$IFDEF TCP_DEBUG} raise; {$ELSE}
      on E : Exception do
        Log(tlError, 'TriggerClientShutdown.Error:Client=%d,Error=%s[%s]',
            [Client.ClientID, E.ClassName, E.Message]);
      {$ENDIF}
    end;
end;

procedure TF5TCPServer.TriggerClientClose(const Client: TTCPServerClient);
begin
  if Assigned(FOnClientClose) then
    try
      FOnClientClose(Client);
    except
      {$IFDEF TCP_DEBUG} raise; {$ELSE}
      on E : Exception do
        Log(tlError, 'TriggerClientClose.Error:Client=%d,Error=%s[%s]',
            [Client.ClientID, E.ClassName, E.Message]);
      {$ENDIF}
    end;
end;

procedure TF5TCPServer.TriggerClientRead(const Client: TTCPServerClient);
begin
  if Assigned(FOnClientRead) then
    try
      FOnClientRead(Client);
    except
      {$IFDEF TCP_DEBUG} raise; {$ELSE}
      on E : Exception do
        Log(tlError, 'TriggerClientRead.Error:Client=%d,Error=%s[%s]',
            [Client.ClientID, E.ClassName, E.Message]);
      {$ENDIF}
    end;
end;

procedure TF5TCPServer.TriggerClientWrite(const Client: TTCPServerClient);
begin
  if Assigned(FOnClientWrite) then
    try
      FOnClientWrite(Client);
    except
      {$IFDEF TCP_DEBUG} raise; {$ELSE}
      on E : Exception do
        Log(tlError, 'TriggerClientWrite.Error:Client=%d,Error=%s[%s]',
            [Client.ClientID, E.ClassName, E.Message]);
      {$ENDIF}
    end;
end;

procedure TF5TCPServer.TriggerClientReadActivity(const Client: TTCPServerClient);
begin
  if Assigned(FOnClientReadActivity) then
    try
      FOnClientReadActivity(Client);
    except
      {$IFDEF TCP_DEBUG} raise; {$ELSE}
      on E : Exception do
        Log(tlError, 'TriggerClientReadActivity.Error:Client=%d,Error=%s[%s]',
            [Client.ClientID, E.ClassName, E.Message]);
      {$ENDIF}
    end;
end;

procedure TF5TCPServer.TriggerClientWriteActivity(const Client: TTCPServerClient);
begin
  if Assigned(FOnClientWriteActivity) then
    try
      FOnClientWriteActivity(Client);
    except
      {$IFDEF TCP_DEBUG} raise; {$ELSE}
      on E : Exception do
        Log(tlError, 'TriggerClientWriteActivity.Error:Client=%d,Error=%s[%s]',
            [Client.ClientID, E.ClassName, E.Message]);
      {$ENDIF}
    end;
end;

procedure TF5TCPServer.TriggerClientWorkerExecute(const Client: TTCPServerClient;
          const Connection: TTCPBlockingConnection; var CloseOnExit: Boolean);
begin
  if Assigned(FOnClientWorkerExecute) then
    FOnClientWorkerExecute(Client, Connection, CloseOnExit);
end;

procedure TF5TCPServer.SetReady;
begin
  SetState(ssReady);
end;

procedure TF5TCPServer.SetClosed;
begin
  SetState(ssClosed);
end;

procedure TF5TCPServer.DoCloseClients;
var
  C : TTCPServerClient;
begin
  C := FClientList.FFirst;
  while Assigned(C) do
    begin
      C.Close;
      C := C.FNext;
    end;
end;

procedure TF5TCPServer.DoCloseServer;
begin
  if Assigned(FServerSocket) then
    FServerSocket.CloseSocket;
end;

procedure TF5TCPServer.DoClose;
begin
  DoCloseServer;
  DoCloseClients;
  SetClosed;
end;

{$IFDEF TCPSERVER_TLS}
procedure TF5TCPServer.TLSServerTransportLayerSendProc(AServer: TTLSServer; AClient: TTLSServerClient; const Buffer; const Size: Integer);
var Proxy : TTCPServerClientTLSConnectionProxy;
begin
  Assert(Assigned(AClient.UserObj));
  Assert(AClient.UserObj is TTCPServerClientTLSConnectionProxy);

  Proxy := TTCPServerClientTLSConnectionProxy(AClient.UserObj);
  Proxy.TLSClientTransportLayerSendProc(AClient, Buffer, Size);
end;
{$ENDIF}

procedure TF5TCPServer.StartControlThread;
begin
  Assert(not Assigned(FControlThread));
  FControlState := sctsInit;
  FControlThread := TTCPServerThread.Create(self, sttControl);
end;

procedure TF5TCPServer.StartProcessThreads;
var
  L, I : Integer;
begin
  Assert(FProcessThreads = nil);
  L := FProcessThreadCount;
  if L <= 0 then
    L := TCP_SERVER_DEFAULT_ProcessThreadCount;
  FProcessThreadsRun := L;
  SetLength(FProcessThreads, L);
  for I := 0 to L - 1 do
    FProcessThreads[I] := nil;
  for I := 0 to L - 1 do
    FProcessThreads[I] := TTCPServerThread.Create(self, sttProcess);
end;

procedure TF5TCPServer.StopServerThreads;
var
  C : TTCPServerThread;
  T : TTCPServerThread;
  L, I : Integer;
begin
  C := FControlThread;
  if Assigned(C) then
    C.Terminate;
  L := Length(FProcessThreads);
  for I := 0 to L - 1 do
    begin
      T := FProcessThreads[I];
      if Assigned(T) then
        T.Terminate;
    end;
  if Assigned(C) then
    begin
      try
        C.WaitFor;
      except
      end;
      C.Finalise;
    end;
  for I := 0 to L - 1 do
    begin
      T := FProcessThreads[I];
      if Assigned(T) then
        begin
          FProcessThreads[I] := nil;
          try
            T.WaitFor;
          except
          end;
          T.Finalise;
          FreeAndNil(T);
        end;
    end;
  FProcessThreads := nil;
  if Assigned(C) then
    begin
      FControlThread := nil;
      FreeAndNil(C);
    end;
end;

procedure TF5TCPServer.DoSetActive;
begin
  Assert(not FActive);
  {$IFDEF TCP_DEBUG}
  Log(tlDebug, 'Starting');
  {$ENDIF}
  TriggerStart;
  FActive := True;
  SetState(ssStarting);
  {$IFDEF TCPSERVER_TLS}
  if FTLSEnabled then
    begin
      FTLSServer.ServerOptions := FTLSServerOptions;
      FTLSServer.VersionOptions := FTLSVersionOptions;
      FTLSServer.KeyExchangeOptions := FTLSKeyExchangeOptions;
      FTLSServer.CipherOptions := FTLSCipherOptions;
      FTLSServer.HashOptions := FTLSHashOptions;
      FTLSServer.Start;
    end;
  {$ENDIF}
  FProcessThreadsReady := 0;
  StartControlThread;
  StartProcessThreads;
  {$IFDEF TCP_DEBUG}
  Log(tlDebug, 'Started');
  {$ENDIF}
end;

procedure TF5TCPServer.DoSetInactive;

  procedure RemoveAllClients(const List: TTCPServerClientList);
  var
    Iter, Next : TTCPServerClient;
  begin
    Iter := List.First;
    while Assigned(Iter) do
      begin
        Next := Iter.FNext;
        TriggerClientRemove(Iter);
        List.Remove(Iter);
        if Iter.FReferenceCount = 0 then
          begin
            TriggerClientDestroy(Iter);
            Iter.Free;
          end
        else
          Iter.SetClientOrphaned;
        Iter := Next;
      end;
  end;

var
  Iter : TTCPServerClient;
begin
  Assert(FActive);
  {$IFDEF TCP_DEBUG}
  Log(tlDebug, 'Stopping');
  {$ENDIF}
  TriggerStop;
  StopServerThreads;
  Iter := FClientList.First;
  while Assigned(Iter) do
    begin
      Iter.TerminateWorkerThread;
      Iter := Iter.FNext;
    end;
  DoClose;
  {$IFDEF TCPSERVER_TLS}
  if FTLSEnabled then
    FTLSServer.Stop;
  {$ENDIF}
  RemoveAllClients(FClientTerminatedList);
  RemoveAllClients(FClientAcceptedList);
  RemoveAllClients(FClientList);
  FreeAndNil(FServerSocket);
  {$IFDEF TCP_DEBUG}
  Log(tlDebug, 'Stopped');
  {$ENDIF}
  FActive := False;
end;

function TF5TCPServer.CreateClient(const ASocketHandle: TSocketHandle; const ASocketAddr: TSocketAddr): TTCPServerClient;
var
  ClientId : Int64;
begin
  Inc(FClientIDCounter);
  ClientId := FClientIDCounter;
  {$IFDEF TCP_DEBUG}
  Log(tlDebug, 'CreateClient(ID:%d,Handle:%d)', [ClientId, Ord(SocketHandle)]);
  {$ENDIF}
  Result := TTCPServerClient.Create(self, ASocketHandle, ClientId, ASocketAddr);
end;

function TF5TCPServer.CanAcceptClient: Boolean;
var M : Integer;
begin
  Lock;
  try
    M := FMaxClients;
    if M < 0 then // no limit
      Result := True else
    if M = 0 then // paused
      Result := False
    else
      Result := FClientList.Count + FClientAcceptedList.Count < M;
  finally
    Unlock;
  end;
end;

function TF5TCPServer.ServerAcceptClient: Boolean;
var AcceptAddr   : TSocketAddr;
    AcceptSocket : TSocketHandle;
    AcceptClient : Boolean;
    Client       : TTCPServerClient;
begin
  // accept socket
  AcceptSocket := FServerSocket.Accept(AcceptAddr);
  if AcceptSocket = INVALID_SOCKETHANDLE then
    begin
      Result := False;
      exit;
    end;
  {$IFDEF TCP_DEBUG}
  Log(tlDebug, Format('IncommingConnection(%s:%d)', [
      SocketAddrIPStrA(AcceptAddr),
      AcceptAddr.Port]));
  {$ENDIF}
  AcceptClient := True;
  if (AcceptAddr.AddrFamily = iaNone) or
     (AcceptAddr.AddrFamily <> FAddressFamily) then //// 2020/05/05
    begin
      Log(tlError, 'Accept: Invalid address family: Closing'); //// 2020/05/05
      AcceptClient := False;
    end;
  if AcceptClient then
    TriggerClientAccept(AcceptAddr, AcceptClient);
  if not AcceptClient then
    begin
      SocketClose(AcceptSocket);
      Result := False;
      exit;
    end;
  // create, add and start new client
  Lock;
  try
    Client := CreateClient(AcceptSocket, AcceptAddr);
    Client.Connection.UseWorkerThread := FUseWorkerThread;
  finally
    Unlock;
  end;
  // set socket TcpNoDelay option
  try
    Client.Connection.Socket.TcpNoDelayEnabled := True;
  except
  end;
  // set socket non-blocking for processing
  try
    Client.Connection.Socket.SetBlocking(False);
  except
    Client.Free;
    raise;
  end;
  TriggerClientCreate(Client);
  Lock;
  try
    FClientAcceptedList.Add(Client);
    Client.Start;
  finally
    Unlock;
  end;
  {$IFDEF TCP_DEBUG}
  Log(tlDebug, 'ClientAdded');
  {$ENDIF}
  TriggerClientAdd(Client);
  Result := True;
end;

// Find a terminated client without any references to it, if found
// remove from client list and free client object
// Returns True if client found and dropped
function TF5TCPServer.ServerDropClient: Boolean;
var
  ItCnt, ClCnt : Integer;
  Iter : TTCPServerClient;
  DropCl : TTCPServerClient;
begin
  // find terminated client to free
  Lock;
  try
    DropCl := nil;
    ClCnt := FClientTerminatedList.Count;
    Iter := FClientTerminatedList.First;
    for ItCnt := 0 to ClCnt - 1 do
      begin
        if Iter.FReferenceCount = 0 then
          begin
            DropCl := Iter;
            FClientTerminatedList.Remove(DropCl);
            break;
          end;
        Iter := Iter.FNext;
      end;
  finally
    Unlock;
  end;
  if not Assigned(DropCl) then
    begin
      // no client to drop
      Result := False;
      exit;
    end;
  // notify and free client
  {$IFDEF TCP_DEBUG}
  Log(tlDebug, 'ClientDestroy');
  {$ENDIF}
  TriggerClientDestroy(DropCl);
  DropCl.Finalise;
  {$IFNDEF NEXTGEN}
  DropCl.Free;
  {$ENDIF}
  Result := True;
end;

// Process a client (read from socket, write to socket, handle socket errors)
procedure TF5TCPServer.ProcessClient(
          const AClient: TTCPServerClient;
          const ProcessRead, ProcessWrite: Boolean;
          const ActivityTime: TDateTime;
          out ClientIdle, ClientTerminated: Boolean);
var
  ClSt : TTCPServerClientState;
  ClFr : Boolean;
begin
  AClient.Process(ProcessRead, ProcessWrite, ActivityTime, ClientIdle, ClientTerminated);
  if ClientTerminated then
    begin
      AClient.TerminateWorkerThread;
      Lock;
      try
        ClSt := AClient.State;
        ClFr := AClient.FReferenceCount = 0;
        FPollList.Remove(AClient.FPollIndex);
        FClientList.Remove(AClient);
      finally
        Unlock;
      end;
      if ClSt = scsReady then
        begin
          AClient.SetState(scsClosed);
          TriggerClientClose(AClient);
        end;
      TriggerClientRemove(AClient);
      if ClFr then
        begin
          {$IFDEF TCP_DEBUG}
          Log(tlDebug, 'ClientDestroy');
          {$ENDIF}
          TriggerClientDestroy(AClient);
          AClient.Finalise;
          {$IFNDEF NEXTGEN}
          AClient.Free;
          {$ENDIF}
        end
      else
        begin
          Lock;
          try
            FClientTerminatedList.Add(AClient);
          finally
            Unlock;
          end;
        end;
    end;
end;

function TF5TCPServer.ServerProcessClient: Boolean;
var
  IdxStart, Cnt, Idx : Integer;
  ItemP : PPollfd;
  Ev : Int16;
  Cl : TTCPServerClient;
  ClientIdle, ClientTerminated : Boolean;
begin
  Ev := 0;
  Cl := nil;
  Lock;
  try
    IdxStart := FPollProcessIdx;
    Cnt := FPollEntCount;
    if IdxStart >= Cnt then
      begin
        Result := False;
        exit;
      end;
    ItemP := FPollEntBuf;
    Inc(ItemP, IdxStart);
    for Idx := IdxStart to Cnt - 1 do
      begin
        Ev := ItemP^.revents;
        if (ItemP^.fd <> INVALID_SOCKET) and (Ev <> 0) then
          begin
            Cl := FPollList.GetClientByIndex(Idx);
            Assert(Assigned(Cl));
            ItemP^.revents := 0;
            FPollProcessIdx := Idx + 1;
            break;
          end;
        Inc(ItemP);
      end;
  finally
    Unlock;
  end;
  if not Assigned(Cl) then
    begin
      Result := False;
      exit;
    end;
  ProcessClient(Cl,
      Ev and (POLLIN or POLLHUP or POLLERR) <> 0,
      Ev and (POLLOUT or POLLHUP or POLLERR) <> 0,
      FPollTime,
      ClientIdle, ClientTerminated);
  Result := True;
end;

// Add newly accepted clients to poll list
// Poll to determine which clients to process
procedure TF5TCPServer.ServerPoll(out Idle: Boolean; out ProcessPending: Boolean);
var
  Cl, Nx : TTCPServerClient;
  FdPtr : Pointer;
  FdCnt : Integer;
  ItemP : PPollfd;
  Idx : Integer;
  WritePoll : Boolean;
  Ev : Int16;
  PollRes : Integer;
  {$IFDEF OS_WIN32}
  PollCnt : Integer;
  {$ENDIF}
begin
  Lock;
  try
    Cl := FClientAcceptedList.First;
    while Assigned(Cl) do
      begin
        Nx := Cl.FNext;
        FClientAcceptedList.Remove(Cl);
        FClientList.Add(Cl);
        Cl.FPollIndex := FPollList.Add(Cl);
        Cl := Nx;
      end;
  finally
    Unlock;
  end;
  if FPollList.ClientCount = 0 then
    begin
      Idle := True;
      ProcessPending := False;
      exit;
    end;
  FPollList.GetPollBuffer(FdPtr, FdCnt);
  ItemP := FdPtr;
  for Idx := 0 to FdCnt - 1 do
    begin
      Cl := FPollList.GetClientByIndex(Idx);
      if Assigned(Cl) then
        begin
          Cl.Connection.GetEventsToPoll(WritePoll);
          Ev := POLLIN;
          if WritePoll then
            Ev := Ev or POLLOUT;
          ItemP^.events := Ev;
        end
      else
        ItemP^.events := 0;
      ItemP^.revents := 0;
      Inc(ItemP);
    end;
  Assert(FdCnt > 0);
  {$IFDEF OS_WIN32}
  // under Win32, WinSock blocks Socket.Write() if Socket.Poll() is active
  // use loop to reduce write latency
  for PollCnt := 1 to 10 do
    begin
      PollRes := SocketsPoll(FdPtr, FdCnt, 10); // 10 milliseconds
      if PollRes <> 0 then
        break;
    end;
  {$ELSE}
  PollRes := SocketsPoll(FdPtr, FdCnt, 100); // 100 milliseconds
  {$ENDIF}
  if PollRes < 0 then
    begin
      Idle := True;
      ProcessPending := False;
      //// Check error: log error/warn/alter/critial
      exit;
    end;
  if PollRes = 0 then
    begin
      Idle := False;
      ProcessPending := False;
      exit;
    end;
  FPollEntBuf := FdPtr;
  FPollEntCount := FdCnt;
  FPollTime := Now;
  FPollProcessIdx := 0;
  Idle := False;
  ProcessPending := True;
end;

// The control thread handles accepting new clients and removing deleted client
// A single instance of the control thread executes
procedure TF5TCPServer.ControlThreadExecute(const Thread: TTCPServerThread);

  function IsTerminated: Boolean;
  begin
    Result := Thread.Terminated;
  end;

var
  IsIdle : Boolean;
  DoPoll : Boolean;
  PollIdle, PollProcess : Boolean;
begin
  {$IFDEF TCP_DEBUG_THREAD}
  Log(tlDebug, 'ControlThreadExecute');
  {$ENDIF}
  Assert(FControlState = sctsInit);
  Assert(FState = ssStarting);
  Assert(not Assigned(FServerSocket));
  Assert(Assigned(Thread));
  if IsTerminated then
    exit;
  // initialise server socket
  FBindAddress := ResolveHost(FBindAddressStr, FAddressFamily);
  SetSocketAddrPort(FBindAddress, FServerPort);
  if IsTerminated then
    exit;
  FServerSocket := TSysSocket.Create(FAddressFamily, ipTCP, False, INVALID_SOCKETHANDLE);
  try
    {$IFDEF TCP_DEBUG}
    FServerSocket.OnLog := ServerSocketLog;
    {$ENDIF}
    FServerSocket.SetBlocking(True);
    FServerSocket.Bind(FBindAddress);
    FServerSocket.Listen(FMaxBacklog);
  except
    FreeAndNil(FServerSocket);
    SetState(ssFailure);
    raise; ////// retry in loop, log alert/critical
  end;
  if IsTerminated then
    exit;

  // server socket ready
  FServerSocket.SetBlocking(False);
  SetReady;
  if IsTerminated then
    exit;
  Lock;
  try
    FControlState := sctsPollReady;
    FProcessProcessEvent.ResetEvent;
    FProcessReadyEvent.ResetEvent;
  finally
    Unlock;
  end;
  // loop until thread termination
  while not IsTerminated do
    begin
      IsIdle := True;
      // drop terminated clients
      while ServerDropClient do
        begin
          IsIdle := False;
          if IsTerminated then
            exit;
        end;
      // accept new clients
      if IsTerminated then
        break;
      while CanAcceptClient do
        if ServerAcceptClient then
          begin
            IsIdle := False;
            if IsTerminated then
              exit;
          end
        else
          break;
      // poll / managed process threads
      if IsTerminated then
        break;
      if FControlReadyEvent.WaitFor(100) = wrTimeout then
        IsIdle := False;
      Lock;
      try
        // wait process threads ready to poll
        DoPoll :=
            (FControlState = sctsPollReady) and
            (FProcessThreadsReady = FProcessThreadsRun);
        // wait process theads complete
        if (FControlState = sctsPollProcess) and
           (FProcessThreadsReady = FProcessThreadsRun) then
          begin
            // start next poll
            FProcessThreadsReady := 0;
            FControlState := sctsPollReady;
            FProcessReadyEvent.SetEvent;
            FControlReadyEvent.ResetEvent;
            FProcessProcessEvent.ResetEvent;
            IsIdle := False;
          end;
      finally
        Unlock;
      end;
      if DoPoll then
        begin
          ServerPoll(PollIdle, PollProcess);
          if IsTerminated then
            break;
          if PollIdle and not PollProcess and IsIdle then
            Sleep(50); // No clients to poll
          if not PollIdle then
            IsIdle := False;
          if PollProcess then
            begin
              Lock;
              try
                // start clients process
                FProcessThreadsReady := 0;
                FControlState := sctsPollProcess;
                FProcessProcessEvent.SetEvent;
                FControlReadyEvent.ResetEvent;
                FProcessReadyEvent.ResetEvent;
              finally
                Unlock;
              end;
            end;
        end;
      // sleep if idle
      if IsTerminated then
        break;
      if IsIdle then
        TriggerThreadIdle(Thread);
    end;
end;

// The processing thread handles processing of client sockets
// Event handlers are called from this thread
// A single instance of the processing thread executes
procedure TF5TCPServer.ProcessThreadExecute(const Thread: TTCPServerThread);

  function IsTerminated: Boolean;
  begin
    Result := Thread.Terminated;
  end;

  procedure SetThreadReady;
  begin
    Lock;
    try
      Inc(FProcessThreadsReady);
      if FProcessThreadsReady >= FProcessThreadsRun then
        FControlReadyEvent.SetEvent;
    finally
      Unlock;
    end;
  end;

  function WaitState(const AState: TTCPServerControlThreadState): Boolean;
  var
    WaitFin : Boolean;
  begin
    repeat
      if IsTerminated then
        begin
          Result := False;
          exit;
        end;
      Lock;
      try
        WaitFin := FControlState = AState;
      finally
        Unlock;
      end;
      if IsTerminated then
        begin
          Result := False;
          exit;
        end;
      if WaitFin then
        begin
          Result := True;
          exit;
        end;
      if AState = sctsPollProcess then
        FProcessProcessEvent.WaitFor(100)
      else
      if AState = sctsPollReady then
        FProcessReadyEvent.WaitFor(100)
      else
        TriggerThreadIdle(Thread);
    until False;
  end;

begin
  Assert(Assigned(Thread));

  {$IFDEF TCP_DEBUG_THREAD}
  Log(tlDebug, 'ProcessThreadExecute');
  {$ENDIF}

  // loop until thread termination
  while not IsTerminated do
    begin
      // wait to process
      SetThreadReady;
      if not WaitState(sctsPollProcess) then
        break;
      // process clients
      repeat
        if not ServerProcessClient then
          break;
        if IsTerminated then
          exit;
      until False;
      // wait for next poll
      if IsTerminated then
        break;
      SetThreadReady;
      if not WaitState(sctsPollReady) then
        break;
    end;
end;

procedure TF5TCPServer.ThreadError(const Thread: TTCPServerThread; const Error: Exception);
begin
  Log(tlError, Format('ThreadError(Task:%d,%s,%s)', [Ord(Thread.FTask), Error.ClassName, Error.Message]));
end;

procedure TF5TCPServer.ThreadTerminate(const Thread: TTCPServerThread);
begin
  {$IFDEF TCP_DEBUG_THREAD}
  Log(tlDebug, Format('ThreadTerminate(Task:%d)', [Ord(Thread.FTask)]));
  {$ENDIF}
end;

procedure TF5TCPServer.Start;
begin
  if FActive then
    exit;
  DoSetActive;
end;

procedure TF5TCPServer.Stop;
begin
  if not FActive then
    exit;
  DoSetInactive;
end;

function TF5TCPServer.GetActiveClientCount: Integer;
var
  N : Integer;
  C : TTCPServerClient;
begin
  Lock;
  try
    N := 0;
    C := FClientList.FFirst;
    while Assigned(C) do
      begin
        if not C.FTerminated and (C.FState in [scsNegotiating, scsReady]) then
          Inc(N);
        C := C.FNext;
      end;
  finally
    Unlock;
  end;
  Result := N;
end;

function TF5TCPServer.GetClientCount: Integer;
begin
  Lock;
  try
    Result := FClientList.Count;
  finally
    Unlock;
  end;
end;

function TF5TCPServer.ClientIterateFirst: TTCPServerClient;
var
  C : TTCPServerClient;
begin
  Lock;
  try
    C := FClientList.FFirst;
    // add reference to prevent removal of client
    // caller must call C.ReleaseReference
    C.AddReference;
  finally
    Unlock;
  end;
  Result := C;
end;

function TF5TCPServer.ClientIterateNext(const C: TTCPServerClient): TTCPServerClient;
var
  N : TTCPServerClient;
begin
  Lock;
  try
    N := C.FNext;
    if Assigned(N) then
      // add reference to prevent removal of client
      // caller must call C.ReleaseReference
      N.AddReference;
  finally
    Unlock;
  end;
  Result := N;
end;

function TF5TCPServer.GetReadRate: Int64;
var
  R : Int64;
  C : TTCPServerClient;
begin
  Lock;
  try
    R := 0;
    C := FClientList.FFirst;
    while Assigned(C) do
      begin
        if not C.FTerminated and (C.FState = scsReady) then
          Inc(R, C.Connection.ReadRate);
        C := C.FNext;
      end;
  finally
    Unlock;
  end;
  Result := R;
end;

function TF5TCPServer.GetWriteRate: Int64;
var
  R : Int64;
  C : TTCPServerClient;
begin
  Lock;
  try
    R := 0;
    C := FClientList.FFirst;
    while Assigned(C) do
      begin
        if not C.FTerminated and (C.FState = scsReady) then
          Inc(R, C.Connection.WriteRate);
        C := C.FNext;
      end;
  finally
    Unlock;
  end;
  Result := R;
end;



end.

