{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals 5.00                                        }
{   File name:        flcTCPConnection.pas                                     }
{   File version:     5.34                                                     }
{   Description:      TCP connection.                                          }
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
{   2008/12/23  0.01  Initial development.                                     }
{   2010/11/07  0.02  Revision.                                                }
{   2010/11/12  0.03  Refactor for asynchronous operation.                     }
{   2010/12/03  0.04  Connection proxies.                                      }
{   2010/12/17  0.05  Throttling.                                              }
{   2010/12/19  0.06  Multiple connection proxies.                             }
{   2010/12/29  0.07  Report read/write rates.                                 }
{   2011/01/02  0.08  Bug fix in PollSocket routine.                           }
{   2011/06/16  0.09  Fix TriggerRead with proxies.                            }
{   2011/06/18  0.10  Fix Read error in PollSocket when closed by proxies.     }
{   2011/06/25  0.11  Improve Write notifications.                             }
{   2011/06/26  0.12  Implement defered shutdown if write buffer not empty.    }
{   2011/07/03  0.13  WriteBufferEmpty event.                                  }
{   2011/07/04  0.14  Trigger read events outside lock.                        }
{   2011/07/24  0.15  Discard method.                                          }
{   2011/07/31  0.16  Defer close from proxy until after read.                 }
{   2011/09/03  4.17  Revise for Fundamentals 4.                               }
{   2011/09/10  4.18  Improve locking granularity.                             }
{   2011/09/15  4.19  Improve polling efficiency.                              }
{   2011/10/06  4.20  Fix TCPTick frequency.                                   }
{   2015/03/14  4.21  RawByteString changes.                                   }
{   2015/04/09  4.22  TBytes functions.                                        }
{   2015/04/26  4.23  Blocking interface and worker thread.                    }
{   2016/01/09  5.24  Revised for Fundamentals 5.                              }
{   2018/08/30  5.25  PollSocket returns Terminated on exception.              }
{   2018/09/08  5.26  PollSocket remove use of Select.                         }
{   2018/09/10  5.27  PollSocket change to ProcessSocket.                      }
{   2018/12/31  5.28  CreationTime and LastActivityTime properties.            }
{   2019/04/10  5.29  Locking changes.                                         }
{   2019/04/11  5.30  Shutdown send handling.                                  }
{   2019/04/13  5.31  Shutdown receive handling.                               }
{   2019/12/30  5.32  Initialise buffer sizes on creation.                     }
{   2020/03/28  5.33  Update LastReadActivityTime when read is from socket.    }
{   2020/05/02  5.34  Log exceptions raised in event handlers.                 }
{                                                                              }
{******************************************************************************}

{$INCLUDE ../flcInclude.inc}
{$INCLUDE flcTCP.inc}

unit flcTCPConnection;

interface

uses
  { System }

  SysUtils,
  {$IFDEF WindowsPlatform}
  Windows,
  {$ENDIF}
  SyncObjs,
  Classes,

  { Utils }

  flcStdTypes,

  { Socket }

  flcSocket,

  { TCP }

  flcTCPBuffer;



{                                                                              }
{ TCP Connection                                                               }
{                                                                              }
type
  ETCPConnection = class(Exception);



  TTCPLogType = (
      tlDebug,
      tlParameter,
      tlInfo,
      tlWarning,
      tlError
    );
    //tlCritical


  TTCPConnectionProxyState = (
      prsInit,         // proxy is initialising
      prsNegotiating,  // proxy is negotiating, connection data is not yet being transferred
      prsFiltering,    // proxy has successfully completed negotiation and is actively filtering connection data
      prsFinished,     // proxy has successfully completed operation and can be bypassed
      prsError,        // proxy has failed and connection is invalid
      prsClosed        // proxy has closed the connection
    );

  TTCPConnectionProxy = class;

  TTCPConnectionProxyEvent = procedure (const AProxy: TTCPConnectionProxy) of object;
  TTCPConnectionProxyLogEvent = procedure (
      const AProxy: TTCPConnectionProxy;
      const LogType: TTCPLogType; const LogMsg: String; const LogLevel: Integer) of object;
  TTCPConnectionProxyStateEvent = procedure (
      const AProxy: TTCPConnectionProxy;
      const AState: TTCPConnectionProxyState) of object;
  TTCPConnectionProxyDataEvent = procedure (
      const AProxy: TTCPConnectionProxy; const Buf; const BufSize: Integer) of object;

  TTCPConnectionProxy = class
  private
    FOnLog                    : TTCPConnectionProxyLogEvent;
    FOnStateChange            : TTCPConnectionProxyStateEvent;
    FOnConnectionClose        : TTCPConnectionProxyEvent;
    FOnConnectionPutReadData  : TTCPConnectionProxyDataEvent;
    FOnConnectionPutWriteData : TTCPConnectionProxyDataEvent;

    FNextProxy    : TTCPConnectionProxy;
    FState        : TTCPConnectionProxyState;

  protected
    FErrorMessage : String;

    procedure Log(const LogType: TTCPLogType; const LogMsg: String; const LogLevel: Integer = 0); overload;
    procedure Log(const LogType: TTCPLogType; const LogMsg: String; const LogArgs: array of const; const LogLevel: Integer = 0); overload;

    function  GetStateStr: String;

    procedure SetState(const AState: TTCPConnectionProxyState);
    procedure SetStateError(const AErrorMessage: String);

    procedure ConnectionClose;
    procedure ConnectionPutReadData(const Buf; const BufSize: Integer);
    procedure ConnectionPutWriteData(const Buf; const BufSize: Integer);
    procedure ProxyStart; virtual; abstract;

  public
    class function ProxyName: String; virtual;

    constructor Create;
    procedure Finalise;

    property  OnLog: TTCPConnectionProxyLogEvent read FOnLog write FOnLog;
    property  OnStateChange: TTCPConnectionProxyStateEvent read FOnStateChange write FOnStateChange;

    property  OnConnectionClose: TTCPConnectionProxyEvent read FOnConnectionClose write FOnConnectionClose;
    property  OnConnectionPutReadData: TTCPConnectionProxyDataEvent read FOnConnectionPutReadData write FOnConnectionPutReadData;
    property  OnConnectionPutWriteData: TTCPConnectionProxyDataEvent read FOnConnectionPutWriteData write FOnConnectionPutWriteData;

    property  State: TTCPConnectionProxyState read FState;
    property  StateStr: String read GetStateStr;
    property  ErrorMessage: String read FErrorMessage;

    procedure Start;

    procedure ProcessReadData(const Buf; const BufSize: Integer); virtual; abstract;
    procedure ProcessWriteData(const Buf; const BufSize: Integer); virtual; abstract;
  end;



  TTCPConnectionProxyList = class
  private
    FList : array of TTCPConnectionProxy;

    function  GetCount: Integer;
    function  GetItem(const AIdx: Integer): TTCPConnectionProxy;
    function  GetLastItem: TTCPConnectionProxy;

  public
    destructor Destroy; override;
    procedure Finalise;

    property  Count: Integer read GetCount;
    property  Item[const AIdx: Integer]: TTCPConnectionProxy read GetItem; default;
    procedure Add(const AProxy: TTCPConnectionProxy);
    property  LastItem: TTCPConnectionProxy read GetLastItem;
  end;


  TTCPConnectionState = (
      cnsInit,               // Initialising/Connecting
      cnsProxyNegotiation,   // Proxy busy with negotiation
      cnsConnected,          // Connected
      cnsClosed              // Closed
    );
  TTCPConnectionStates = set of TTCPConnectionState;



  TTCPConnectionTransferState = record
    LastUpdate    : Word64;
    TransferBytes : Int64;
    TransferRate  : Word64;
  end;



  TRawByteCharSet = set of AnsiChar;

  TTCPBlockingConnection = class;

  TTCPConnection = class;

  TTCPConnectionNotifyEvent = procedure (AConnection: TTCPConnection) of object;
  TTCPConnectionStateChangeEvent = procedure (AConnection: TTCPConnection; AState: TTCPConnectionState) of object;
  TTCPConnectionLogEvent = procedure (AConnection: TTCPConnection; LogType: TTCPLogType; LogMsg: String; LogLevel: Integer) of object;
  TTCPConnectionWorkerExecuteEvent = procedure (AConnection: TTCPConnection;
      ABlockingConnection: TTCPBlockingConnection; var CloseOnExit: Boolean) of object;

  TTCPConnection = class
  protected
    // parameters
    FSocket                : TSysSocket;

    FReadThrottle          : Boolean;
    FReadThrottleRate      : Integer;
    FWriteThrottle         : Boolean;
    FWriteThrottleRate     : Integer;

    FTrackLastActivityTime : Boolean;

    FUseWorkerThread       : Boolean;

    FUserTag               : NativeInt;
    FUserObject            : TObject;

    // events
    FOnLog              : TTCPConnectionLogEvent;

    FOnStateChange      : TTCPConnectionStateChangeEvent;
    FOnReady            : TTCPConnectionNotifyEvent;
    FOnClose            : TTCPConnectionNotifyEvent;
    FOnReadShutdown     : TTCPConnectionNotifyEvent;
    FOnShutdown         : TTCPConnectionNotifyEvent;

    FOnRead             : TTCPConnectionNotifyEvent;
    FOnWrite            : TTCPConnectionNotifyEvent;
    FOnReadActivity     : TTCPConnectionNotifyEvent;
    FOnWriteActivity    : TTCPConnectionNotifyEvent;
    FOnReadBufferFull   : TTCPConnectionNotifyEvent;
    FOnWriteBufferEmpty : TTCPConnectionNotifyEvent;

    FOnWait             : TTCPConnectionNotifyEvent;

    FOnWorkerExecute    : TTCPConnectionWorkerExecuteEvent;
    FOnWorkerFinished   : TTCPConnectionNotifyEvent;

    // state
    FLock                  : TCriticalSection;

    FState                 : TTCPConnectionState;
    FErrorMessage          : String;

    FCreationTime          : TDateTime; //// InitialiseTime

    FReadBuffer            : TTCPBuffer;
    FWriteBuffer           : TTCPBuffer;

    FReadTransferState     : TTCPConnectionTransferState;
    FWriteTransferState    : TTCPConnectionTransferState;

    FProxyList             : TTCPConnectionProxyList;
    FProxyConnection       : Boolean;

    FReadyNotified         : Boolean;
    FReadEventPending      : Boolean;
    FReadBufferFull        : Boolean;
    FReadProcessPending    : Boolean;
    FReadActivityPending   : Boolean;
    FWriteEventPending     : Boolean;
    FShutdownSendPending   : Boolean;
    FShutdownSent          : Boolean;
    FShutdownRecv          : Boolean;
    FShutdownComplete      : Boolean;
    FClosePending          : Boolean;

    FLastReadActivityTime  : TDateTime;
    FLastWriteActivityTime : TDateTime;

    FBlockingConnection    : TTCPBlockingConnection;

    FWorkerThread          : TThread;
    FWorkerErrorMsg        : String;
    FWorkerErrorClass      : String;

    procedure Init; virtual;
    procedure InitBuffers(
              const AReadBufferMinSize: Int32;
              const AReadBufferMaxSize: Int32;
              const AWriteBufferMinSize: Int32;
              const AWriteBufferMaxSize: Int32);

    procedure Lock;
    procedure Unlock;

    procedure Log(const LogType: TTCPLogType; const LogMsg: String; const LogLevel: Integer = 0); overload;
    procedure Log(const LogType: TTCPLogType; const LogMsg: String; const LogArgs: array of const; const LogLevel: Integer = 0); overload;

    function  GetState: TTCPConnectionState;
    function  GetStateStr: String;

    procedure SetStateProxyNegotiation;
    procedure SetStateFailed;
    procedure SetStateConnected;
    function  SetStateClosed: Boolean;

    function  GetReadBufferMinSize: Int32;
    function  GetReadBufferMaxSize: Int32;
    function  GetWriteBufferMinSize: Int32;
    function  GetWriteBufferMaxSize: Int32;

    procedure SetReadBufferMinSize(const AReadBufferMinSize: Int32);
    procedure SetReadBufferMaxSize(const AReadBufferMaxSize: Int32);
    procedure SetWriteBufferMinSize(const AWriteBufferMinSize: Int32);
    procedure SetWriteBufferMaxSize(const AWriteBufferMaxSize: Int32);

    function  GetSocketReadBufferSize: Integer;
    function  GetSocketWriteBufferSize: Integer;
    procedure SetSocketReadBufferSize(const Size: Integer);
    procedure SetSocketWriteBufferSize(const Size: Integer);

    function  GetReadBufferUsed: Integer;
    function  GetWriteBufferUsed: Integer;
    function  GetReadBufferAvailable: Integer;
    function  GetWriteBufferAvailable: Integer;

    procedure SetReadThrottle(const AReadThrottle: Boolean);
    procedure SetWriteThrottle(const AWriteThrottle: Boolean);

    function  GetReadRate: Integer;
    function  GetWriteRate: Integer;

    procedure TriggerStateChange;
    procedure TriggerReady;
    procedure TriggerReadShutdown;
    procedure TriggerShutdown;
    procedure TriggerClose;
    procedure TriggerRead;
    procedure TriggerWrite;
    procedure TriggerReadActivity;
    procedure TriggerWriteActivity;
    procedure TriggerReadBufferFull;
    procedure TriggerWriteBufferEmpty;

    procedure TriggerWait;
    procedure TriggerWorkerFinished;

    function  GetFirstActiveProxy: TTCPConnectionProxy;

    procedure ProxyProcessReadData(const Buf; const BufSize: Integer; out ReadEventPending: Boolean);
    procedure ProxyProcessWriteData(const Buf; const BufSize: Integer);

    procedure ProxyLog(const AProxy: TTCPConnectionProxy; const LogType: TTCPLogType; const LogMsg: String; const LogLevel: Integer);

    procedure ProxyConnectionClose(const AProxy: TTCPConnectionProxy);
    procedure ProxyStateChange(const AProxy: TTCPConnectionProxy; const AState: TTCPConnectionProxyState);

    procedure ProxyConnectionPutReadData(const AProxy: TTCPConnectionProxy; const Buf; const BufSize: Integer);
    procedure ProxyConnectionPutWriteData(const AProxy: TTCPConnectionProxy; const Buf; const BufSize: Integer);

    procedure StartProxies(out AProxiesFinished: Boolean);

    function  FillBufferFromSocket(out RecvShutdown, RecvClosed, ReadEventPending, ReadBufFullEventPending: Boolean): Integer;
    function  WriteBufferToSocket(out BufferEmptyBefore, BufferEmptied: Boolean): Integer;

    function  GetLastReadActivityTime: TDateTime;
    function  GetLastWriteActivityTime: TDateTime;
    function  GetLastActivityTime: TDateTime;

    function  LocateByteCharInBuffer(const ADelimiter: ByteCharSet; const AMaxSize: Integer): Integer;
    function  LocateByteStrInBuffer(const ADelimiter: RawByteString; const AMaxSize: Integer): Integer;

    function  WriteToTransport(const Buf; const BufSize: Integer): Integer;

    procedure DoShutdown(out AShutdownComplete: Boolean);

    procedure Wait;

    function  GetBlockingConnection: TTCPBlockingConnection;

    procedure StartWorkerThread;
    procedure WorkerThreadExecute(const AThread: TThread);

  public
    constructor Create(
                const ASocket: TSysSocket;
                const AReadBufferMinSize: Int32 = TCP_BUFFER_DEFAULTMINSIZE;
                const AReadBufferMaxSize: Int32 = TCP_BUFFER_DEFAULTMAXSIZE;
                const AWriteBufferMinSize: Int32 = TCP_BUFFER_DEFAULTMINSIZE;
                const AWriteBufferMaxSize: Int32 = TCP_BUFFER_DEFAULTMAXSIZE
                );

    destructor Destroy; override;
    procedure Finalise;

    // Events
    property  OnLog: TTCPConnectionLogEvent read FOnLog write FOnLog;

    property  OnStateChange: TTCPConnectionStateChangeEvent read FOnStateChange write FOnStateChange;
    property  OnReady: TTCPConnectionNotifyEvent read FOnReady write FOnReady;
    property  OnReadShutdown: TTCPConnectionNotifyEvent read FOnReadShutdown write FOnReadShutdown;
    property  OnShutdown: TTCPConnectionNotifyEvent read FOnShutdown write FOnShutdown;
    property  OnClose: TTCPConnectionNotifyEvent read FOnClose write FOnClose;

    property  OnRead: TTCPConnectionNotifyEvent read FOnRead write FOnRead;
    property  OnWrite: TTCPConnectionNotifyEvent read FOnWrite write FOnWrite;
    property  OnReadBufferFull: TTCPConnectionNotifyEvent read FOnReadBufferFull write FOnReadBufferFull;
    property  OnWriteBufferEmpty: TTCPConnectionNotifyEvent read FOnWriteBufferEmpty write FOnWriteBufferEmpty;

    property  OnReadActivity: TTCPConnectionNotifyEvent read FOnReadActivity write FOnReadActivity; //// OnSocketReadActivity
    property  OnWriteActivity: TTCPConnectionNotifyEvent read FOnWriteActivity write FOnWriteActivity; ////

    // Parameters
    property  Socket: TSysSocket read FSocket;

    // Buffers sizes
    property  ReadBufferMinSize: Int32 read GetReadBufferMinSize write SetReadBufferMinSize;
    property  ReadBufferMaxSize: Int32 read GetReadBufferMaxSize write SetReadBufferMaxSize;
    property  WriteBufferMinSize: Int32 read GetWriteBufferMinSize write SetWriteBufferMinSize;
    property  WriteBufferMaxSize: Int32 read GetWriteBufferMaxSize write SetWriteBufferMaxSize;

    // Socket buffer sizes
    property  SocketReadBufferSize: Integer read GetSocketReadBufferSize write SetSocketReadBufferSize;
    property  SocketWriteBufferSize: Integer read GetSocketWriteBufferSize write SetSocketWriteBufferSize;

    // Proxies
    procedure AddProxy(const AProxy: TTCPConnectionProxy);

    // State
    property  State: TTCPConnectionState read GetState;
    property  StateStr: String read GetStateStr;
    property  ErrorMessage: String read FErrorMessage;

    property  CreationTime: TDateTime read FCreationTime;

    procedure Start;

    // Throttling
    property  ReadThrottle: Boolean read FReadThrottle write SetReadThrottle;
    property  ReadThrottleRate: Integer read FReadThrottleRate write FReadThrottleRate;
    property  WriteThrottle: Boolean read FWriteThrottle write SetWriteThrottle;
    property  WriteThrottleRate: Integer read FWriteThrottleRate write FWriteThrottleRate;

    property  ReadRate: Integer read GetReadRate;
    property  WriteRate: Integer read GetWriteRate;

    // Poll routines
    procedure GetEventsToPoll(out WritePoll: Boolean);
    procedure ProcessSocket(
              const ProcessRead, ProcessWrite: Boolean;
              const ActivityTime: TDateTime;
              out Idle, Terminated: Boolean);

    // Last activity times
    property  TrackLastActivityTime: Boolean read FTrackLastActivityTime write FTrackLastActivityTime;
    property  LastReadActivityTime: TDateTime read GetLastReadActivityTime;
    property  LastWriteActivityTime: TDateTime read GetLastWriteActivityTime;
    property  LastActivityTime: TDateTime read GetLastActivityTime;

    // Buffer status
    property  ReadBufferUsed: Integer read GetReadBufferUsed;
    property  WriteBufferUsed: Integer read GetWriteBufferUsed;
    property  ReadBufferAvailable: Integer read GetReadBufferAvailable;
    property  WriteBufferAvailable: Integer read GetWriteBufferAvailable;

    // Read
    function  Read(var Buf; const BufSize: Integer): Integer;
    function  ReadByteString(const AStrLen: Integer): RawByteString;
    function  ReadBytes(const ASize: Integer): TBytes;

    // Discard
    function  Discard(const ASize: Integer): Integer;

    // Peek
    function  Peek(var Buf; const BufSize: Int32): Integer;
    function  PeekByte(out B: Byte): Boolean;
    function  PeekByteString(const AStrLen: Integer): RawByteString;
    function  PeekBytes(const ASize: Integer): TBytes;

    function  PeekDelimited(var Buf; const BufSize: Integer;
              const ADelimiter: TRawByteCharSet;
              const AMaxSize: Integer = -1): Integer; overload;
    function  PeekDelimited(var Buf; const BufSize: Integer;
              const ADelimiter: RawByteString;
              const AMaxSize: Integer = -1): Integer; overload;

    // ReadLine
    function  ReadLine(var Line: RawByteString;
              const ADelimiter: RawByteString;
              const AMaxLineLength: Integer = -1): Boolean;

    // Write
    function  Write(const Buf; const BufSize: Integer): Integer;
    function  WriteByteString(const AStr: RawByteString): Integer;
    function  WriteBytes(const B: TBytes): Integer;

    // Shutdown/Close
    // Shutdown initiates a graceful shutdown.
    // Close closes connection immediately.
    procedure Shutdown;
    function  IsShutdownComplete: Boolean;
    procedure Close;

    // Worker thread
    // Set UseWorkerThread to True and set OnWorkerExecute to execute
    // OnWorkerExecute in a separate thread.
    // The thread is launched when the connection state is Connected.
    // WorkerErrorClass and WorkerErrorMsg is set to the exception details if an
    // unhandled exception is raised in OnWorkerExecute.
    property  UseWorkerThread: Boolean read FUseWorkerThread write FUseWorkerThread;
    property  OnWorkerExecute: TTCPConnectionWorkerExecuteEvent read FOnWorkerExecute write FOnWorkerExecute;
    property  WorkerErrorClass: String read FWorkerErrorClass;
    property  WorkerErrorMsg: String read FWorkerErrorMsg;
    procedure TerminateWorkerThread;
    procedure WaitForWorkerThread;

    // Blocking
    // The blocking interface is available from the worker thread.
    // OnWait is called when blocking occurs.
    property  BlockingConnection: TTCPBlockingConnection read GetBlockingConnection;
    property  OnWait: TTCPConnectionNotifyEvent read FOnWait write FOnWait;

    // User defined values
    property  UserTag: NativeInt read FUserTag write FUserTag;
    property  UserObject: TObject read FUserObject write FUserObject;
  end;

  TTCPConnectionClass = class of TTCPConnection;

  // Blocking connection
  // These methods will block until a result is available or timeout expires.
  // If TimeOut is set to -1 then method may wait indefinetely for a result.
  // Note: These functions should not be called from this object's event handlers
  // or from the main thread.
  // These functions can only be called from the worker thread or any other user thread.
  TTCPBlockingConnection = class
  private
    FConnection : TTCPConnection;

    procedure Wait;

  public
    constructor Create(const AConnection : TTCPConnection);
    destructor Destroy; override;
    procedure Finalise;

    property  Connection: TTCPConnection read FConnection;

    function  WaitForState(const AStates: TTCPConnectionStates; const ATimeOutMs: Integer): TTCPConnectionState;
    function  WaitForReceiveData(const ABufferSize: Integer; const ATimeOutMs: Integer): Boolean;
    function  WaitForTransmitFin(const ATimeOutMs: Integer): Boolean;
    function  WaitForClose(const ATimeOutMs: Integer): Boolean;

    function  Read(var Buf; const BufferSize: Integer; const ATimeOutMs: Integer = -1): Integer;
    function  Write(const Buf; const BufferSize: Integer; const ATimeOutMs: Integer = -1): Integer;
    procedure Shutdown(
              const SettleTimeMs: Integer = 2000;
              const TransmitTimeOutMs: Integer = 15000;
              const CloseTimeOutMs: Integer = 45000);
    procedure Close(const ATimeOutMs: Integer = 10000);
  end;



implementation

uses
  { System }

  {$IFDEF POSIX}
  {$IFDEF DELPHI}
  Posix.Time,
  {$ENDIF}
  {$ENDIF}

  {$IFDEF POSIX}
  {$IFDEF FREEPASCAL}
  BaseUnix,
  Unix,
  {$ENDIF}
  {$ENDIF}

  { Sockets }

  flcSocketLib,

  { TCP }

  flcTCPUtils;



{                                                                              }
{ TCP Connection Transfer Statistics                                           }
{                                                                              }

// Reset transfer statistics
procedure TCPConnectionTransferReset(var State: TTCPConnectionTransferState);
begin
  State.LastUpdate := TCPGetTick;
  State.TransferBytes := 0;
  State.TransferRate := 0;
end;

// Update the transfer's internal state for elapsed time
procedure TCPConnectionTransferUpdate(
          var State: TTCPConnectionTransferState;
          const CurrentTick: Word64;
          out Elapsed: Int64);
begin
  Elapsed := TCPTickDelta(State.LastUpdate, CurrentTick);
  Assert(Elapsed >= 0);
  // wait at least 1000ms between updates
  if Elapsed < 1000 then
    exit;
  // update transfer rate
  State.TransferRate := (State.TransferBytes * 1000) div Elapsed; // bytes per second
  // scale down
  while Elapsed > 60 do
    begin
      Elapsed := Elapsed div 2;
      State.TransferBytes := State.TransferBytes div 2;
    end;
  State.LastUpdate := TCPTickDeltaU(Word64(Elapsed), CurrentTick);
end;

// Returns the number of bytes that can be transferred with this throttle in place
function TCPConnectionTransferThrottledSize(
         var State: TTCPConnectionTransferState;
         const CurrentTick: Word32;
         const MaxTransferRate: Integer;
         const BufferSize: Integer): Integer;
var
  Elapsed : Int64;
  Quota : Int64;
  QuotaFree : Int64;
begin
  Assert(MaxTransferRate > 0);
  TCPConnectionTransferUpdate(State, CurrentTick, Elapsed);
  Quota := ((Elapsed + 30) * MaxTransferRate) div 1000; // quota allowed over Elapsed period
  QuotaFree := Quota - State.TransferBytes;             // quota remaining
  if QuotaFree >= BufferSize then
    Result := BufferSize else
  if QuotaFree <= 0 then
    Result := 0
  else
    Result := QuotaFree;
end;

// Updates transfer statistics for a number of bytes transferred
procedure TCPConnectionTransferredBytes(
          var State: TTCPConnectionTransferState;
          const ByteCount: Integer); {$IFDEF UseInline}inline;{$ENDIF}
begin
  Inc(State.TransferBytes, ByteCount);
end;



{                                                                              }
{ Error and debug strings                                                      }
{                                                                              }
const
  SError_InvalidParameter = 'Invalid parameter';
  SError_TimedOut         = 'Timed out';
  SError_ConnectionClosed = 'Connection closed';

  SConnectionProxyState : array[TTCPConnectionProxyState] of String = (
    'Init',
    'Negotiating',
    'Filtering',
    'Finished',
    'Error',
    'Closed'
    );

  SConnectionState : array[TTCPConnectionState] of String = (
    'Init',
    'ProxyNegotiation',
    ////'Failed',
    'Connected',
    'Closed'
    );



{                                                                              }
{ TCP Connection Proxy                                                         }
{                                                                              }
class function TTCPConnectionProxy.ProxyName: String;
begin
  Result := ClassName;
end;

constructor TTCPConnectionProxy.Create;
begin
  inherited Create;
  FState := prsInit;
end;

procedure TTCPConnectionProxy.Finalise;
begin
end;

procedure TTCPConnectionProxy.Log(const LogType: TTCPLogType; const LogMsg: String; const LogLevel: Integer);
begin
  if Assigned(FOnLog) then
    FOnLog(Self, LogType, LogMsg, LogLevel);
end;

procedure TTCPConnectionProxy.Log(const LogType: TTCPLogType; const LogMsg: String; const LogArgs: array of const; const LogLevel: Integer);
begin
  Log(LogType, Format(LogMsg, LogArgs), LogLevel);
end;

function TTCPConnectionProxy.GetStateStr: String;
begin
  Result := SConnectionProxyState[FState];
end;

procedure TTCPConnectionProxy.SetState(const AState: TTCPConnectionProxyState);
begin
  if AState = FState then
    exit;

  {$IFDEF TCP_DEBUG}
  Log(tlDebug, 'State:%s', [SConnectionProxyState[AState]]);
  {$ENDIF}

  FState := AState;
  if Assigned(FOnStateChange) then
    FOnStateChange(Self, AState);
end;

procedure TTCPConnectionProxy.SetStateError(const AErrorMessage: String);
begin
  if FState = prsError then
    exit;

  FErrorMessage := AErrorMessage;
  SetState(prsError);
end;

procedure TTCPConnectionProxy.ConnectionClose;
begin
  {$IFDEF TCP_DEBUG}
  Log(tlDebug, 'Close');
  {$ENDIF}

  if Assigned(FOnConnectionClose) then
    FOnConnectionClose(Self);
end;

procedure TTCPConnectionProxy.ConnectionPutReadData(const Buf; const BufSize: Integer);
begin
  {$IFDEF TCP_DEBUG_DATA}
  Log(tlDebug, 'PutRead:%db', [BufSize]);
  {$ENDIF}

  if Assigned(FOnConnectionPutReadData) then
    FOnConnectionPutReadData(Self, Buf, BufSize);
end;

procedure TTCPConnectionProxy.ConnectionPutWriteData(const Buf; const BufSize: Integer);
begin
  {$IFDEF TCP_DEBUG_DATA}
  Log(tlDebug, 'PutWrite:%db', [BufSize]);
  {$ENDIF}

  if Assigned(FOnConnectionPutWriteData) then
    FOnConnectionPutWriteData(Self, Buf, BufSize);
end;

procedure TTCPConnectionProxy.Start;
begin
  {$IFDEF TCP_DEBUG}
  Log(tlDebug, 'Start');
  {$ENDIF}

  ProxyStart;
end;



{                                                                              }
{ TCP Connection Proxy List                                                    }
{                                                                              }
destructor TTCPConnectionProxyList.Destroy;
var
  I : Integer;
begin
  for I := Length(FList) - 1 downto 0 do
    FreeAndNil(FList[I]);
  FList := nil;
  inherited Destroy;
end;

procedure TTCPConnectionProxyList.Finalise;
var
  I : Integer;
begin
  for I := Length(FList) - 1 downto 0 do
    FList[I].Finalise;
end;

function TTCPConnectionProxyList.GetCount: Integer;
begin
  Result := Length(FList);
end;

function TTCPConnectionProxyList.GetItem(const AIdx: Integer): TTCPConnectionProxy;
begin
  Assert((AIdx >= 0) and (AIdx < Length(FList)));

  Result := FList[AIdx];
end;

function TTCPConnectionProxyList.GetLastItem: TTCPConnectionProxy;
var
  L : Integer;
begin
  L := Length(FList);
  if L = 0 then
    Result := nil
  else
    Result := FList[L - 1];
end;

procedure TTCPConnectionProxyList.Add(const AProxy: TTCPConnectionProxy);
var
  L : Integer;
begin
  Assert(Assigned(AProxy));

  L := Length(FList);
  SetLength(FList, L + 1);
  FList[L] := AProxy;
end;



{                                                                              }
{ TCP Connection Worker Thread                                                 }
{                                                                              }
type
  TTCPConnectionWorkerThread = class(TThread)
  private
    FConnection: TTCPConnection;
  protected
    procedure Execute; override;
  public
    constructor Create(const Connection: TTCPConnection);
    property Terminated;
  end;

constructor TTCPConnectionWorkerThread.Create(const Connection: TTCPConnection);
begin
  Assert(Assigned(Connection));
  FConnection := Connection;
  FreeOnTerminate := False;
  inherited Create(False);
end;

procedure TTCPConnectionWorkerThread.Execute;
begin
  FConnection.WorkerThreadExecute(self);
end;



{                                                                              }
{ TCP Connection                                                               }
{                                                                              }
constructor TTCPConnection.Create(
            const ASocket: TSysSocket;
            const AReadBufferMinSize: Int32;
            const AReadBufferMaxSize: Int32;
            const AWriteBufferMinSize: Int32;
            const AWriteBufferMaxSize: Int32);
begin
  Assert(Assigned(ASocket));

  inherited Create;

  FSocket := ASocket;

  Init;
  InitBuffers(
      AReadBufferMinSize,
      AReadBufferMaxSize,
      AWriteBufferMinSize,
      AWriteBufferMaxSize);
end;

destructor TTCPConnection.Destroy;
begin
  if Assigned(FWorkerThread) then
    try
      FWorkerThread.Terminate;
      FWorkerThread.WaitFor;
      FreeAndNil(FWorkerThread);
    except
    end;

  FreeAndNil(FBlockingConnection);
  TCPBufferFinalise(FWriteBuffer);
  TCPBufferFinalise(FReadBuffer);
  FreeAndNil(FProxyList);

  FreeAndNil(FLock);

  inherited Destroy;
end;

procedure TTCPConnection.Finalise;
begin
  if Assigned(FBlockingConnection) then
    FBlockingConnection.Finalise;
  if Assigned(FProxyList) then
    FProxyList.Finalise;
  FUserObject := nil;
end;

procedure TTCPConnection.Init;
var
  N : TDateTime;
begin
  FState := cnsInit;
  FReadThrottle  := False;
  FWriteThrottle := False;
  FLock := TCriticalSection.Create;
  FProxyList := TTCPConnectionProxyList.Create;
  FProxyConnection := False;
  N := Now;
  FCreationTime := N;
  FTrackLastActivityTime := False;
  FLastReadActivityTime := 0.0;
  FLastWriteActivityTime := 0.0;
end;

procedure TTCPConnection.InitBuffers(
          const AReadBufferMinSize: Int32;
          const AReadBufferMaxSize: Int32;
          const AWriteBufferMinSize: Int32;
          const AWriteBufferMaxSize: Int32);
begin
  TCPBufferInitialise(FReadBuffer,  AReadBufferMaxSize,  AReadBufferMinSize);
  TCPBufferInitialise(FWriteBuffer, AWriteBufferMaxSize, AWriteBufferMinSize);
end;

procedure TTCPConnection.Lock;
begin
  if Assigned(FLock) then
    FLock.Acquire;
end;

procedure TTCPConnection.Unlock;
begin
  if Assigned(FLock) then
    FLock.Release;
end;

procedure TTCPConnection.Log(const LogType: TTCPLogType; const LogMsg: String; const LogLevel: Integer);
begin
  if Assigned(FOnLog) then
    FOnLog(self, LogType, LogMsg, LogLevel);
end;

procedure TTCPConnection.Log(const LogType: TTCPLogType; const LogMsg: String; const LogArgs: array of const; const LogLevel: Integer);
begin
  Log(LogType, Format(LogMsg, LogArgs), LogLevel);
end;

function TTCPConnection.GetReadBufferMinSize: Int32;
begin
  Lock;
  try
    Result := TCPBufferGetMinSize(FReadBuffer);
  finally
    Unlock;
  end;
end;

function TTCPConnection.GetReadBufferMaxSize: Int32;
begin
  Lock;
  try
    Result := TCPBufferGetMaxSize(FReadBuffer);
  finally
    Unlock;
  end;
end;

function TTCPConnection.GetWriteBufferMinSize: Int32;
begin
  Lock;
  try
    Result := TCPBufferGetMinSize(FWriteBuffer);
  finally
    Unlock;
  end;
end;

function TTCPConnection.GetWriteBufferMaxSize: Int32;
begin
  Lock;
  try
    Result := TCPBufferGetMaxSize(FWriteBuffer);
  finally
    Unlock;
  end;
end;

procedure TTCPConnection.SetReadBufferMinSize(const AReadBufferMinSize: Int32);
begin
  Lock;
  try
    TCPBufferSetMinSize(FReadBuffer, AReadBufferMinSize);
  finally
    Unlock;
  end;
end;

procedure TTCPConnection.SetReadBufferMaxSize(const AReadBufferMaxSize: Int32);
begin
  Lock;
  try
    TCPBufferSetMaxSize(FReadBuffer, AReadBufferMaxSize);
  finally
    Unlock;
  end;
end;

procedure TTCPConnection.SetWriteBufferMinSize(const AWriteBufferMinSize: Int32);
begin
  Lock;
  try
    TCPBufferSetMinSize(FWriteBuffer, AWriteBufferMinSize);
  finally
    Unlock;
  end;
end;

procedure TTCPConnection.SetWriteBufferMaxSize(const AWriteBufferMaxSize: Int32);
begin
  Lock;
  try
    TCPBufferSetMaxSize(FWriteBuffer, AWriteBufferMaxSize);
  finally
    Unlock;
  end;
end;

function TTCPConnection.GetSocketReadBufferSize: Integer;
begin
  Lock;
  try
    Result := FSocket.ReceiveBufferSize;
  finally
    Unlock;
  end;
end;

function TTCPConnection.GetSocketWriteBufferSize: Integer;
begin
  Lock;
  try
    Result := FSocket.SendBufferSize;
  finally
    Unlock;
  end;
end;

procedure TTCPConnection.SetSocketReadBufferSize(const Size: Integer);
begin
  Lock;
  try
    FSocket.ReceiveBufferSize := Size;
  finally
    Unlock;
  end;
end;

procedure TTCPConnection.SetSocketWriteBufferSize(const Size: Integer);
begin
  Lock;
  try
    FSocket.SendBufferSize := Size;
  finally
    Unlock;
  end;
end;

function TTCPConnection.GetReadBufferUsed: Integer;
begin
  Lock;
  try
    Result := FReadBuffer.Used;
  finally
    Unlock;
  end;
end;

function TTCPConnection.GetWriteBufferUsed: Integer;
begin
  Lock;
  try
    Result := FWriteBuffer.Used;
  finally
    Unlock;
  end;
end;

function TTCPConnection.GetReadBufferAvailable: Integer;
begin
  Lock;
  try
    Result := TCPBufferAvailable(FReadBuffer);
  finally
    Unlock;
  end;
end;

function TTCPConnection.GetWriteBufferAvailable: Integer;
begin
  Lock;
  try
    Result := TCPBufferAvailable(FWriteBuffer);
  finally
    Unlock;
  end;
end;

function TTCPConnection.GetState: TTCPConnectionState;
begin
  Lock;
  try
    Result := FState;
  finally
    Unlock;
  end;
end;

function TTCPConnection.GetStateStr: String;
begin
  Result := SConnectionState[GetState];
end;

procedure TTCPConnection.SetStateProxyNegotiation;
begin
  Lock;
  try
    FState := cnsProxyNegotiation;
  finally
    Unlock;
  end;

  {$IFDEF TCP_DEBUG}
  Log(tlDebug, 'State:ProxyNegotiation');
  {$ENDIF}
  TriggerStateChange;
end;

procedure TTCPConnection.SetStateFailed;
begin
  Lock;
  try
    if not (FState in [cnsInit, cnsProxyNegotiation]) then
      exit;
    ////FState := cnsFailed;
  finally
    Unlock;
  end;

  {$IFDEF TCP_DEBUG}
  Log(tlDebug, 'State:Failed');
  {$ENDIF}
  TriggerStateChange;
end;

procedure TTCPConnection.SetStateConnected;
var
  NotifyReady : Boolean;
begin
  Lock;
  try
    Assert(FState in [cnsInit, cnsProxyNegotiation, cnsClosed]);
    FState := cnsConnected;
    // Connection state can change from 'connected' back to 'proxy negotiation'
    // and then back to 'connected' again.
    // Trigger the ready event on the first change to 'connected'.
    if not FReadyNotified then
      begin
        FReadyNotified := True;
        NotifyReady := True;
      end
    else
      NotifyReady := False;
  finally
    Unlock;
  end;

  TriggerStateChange;

  if NotifyReady then
    begin
      Lock;
      try
        if FUseWorkerThread and Assigned(FOnWorkerExecute) then
          StartWorkerThread;
      finally
        Unlock;
      end;
      TriggerReady;
    end;
end;

function TTCPConnection.SetStateClosed: Boolean;
begin
  Lock;
  try
    if FState = cnsClosed then
      begin
        Result := False;
        exit;
      end;
    FState := cnsClosed;
  finally
    Unlock;
  end;
  TriggerStateChange;
  TriggerClose;
  Result := True;
end;

procedure TTCPConnection.AddProxy(const AProxy: TTCPConnectionProxy);
var
  P : TTCPConnectionProxy;
  DoNeg : Boolean;
begin
  if not Assigned(AProxy) then
    raise ETCPConnection.Create(SError_InvalidParameter);

  {$IFDEF TCP_DEBUG}
  Log(tlDebug, 'AddProxy:%s', [AProxy.ProxyName]);
  {$ENDIF}

  Lock;
  try
    // add to list
    P := FProxyList.LastItem;
    FProxyList.Add(AProxy);
    if Assigned(P) then
      P.FNextProxy := AProxy;
    AProxy.FNextProxy := nil;
    FProxyConnection := True;
    // restart negotiation if connected
    DoNeg := (FState = cnsConnected);

    AProxy.OnLog := ProxyLog;
    AProxy.OnStateChange := ProxyStateChange;
    AProxy.OnConnectionClose := ProxyConnectionClose;
    AProxy.OnConnectionPutReadData := ProxyConnectionPutReadData;
    AProxy.OnConnectionPutWriteData := ProxyConnectionPutWriteData;
  finally
    Unlock;
  end;
  if DoNeg then
    begin
      SetStateProxyNegotiation;
      AProxy.Start;
    end;
end;

procedure TTCPConnection.SetReadThrottle(const AReadThrottle: Boolean);
begin
  Lock;
  try
    FReadThrottle := AReadThrottle;
  finally
    Unlock;
  end;
end;

procedure TTCPConnection.SetWriteThrottle(const AWriteThrottle: Boolean);
begin
  Lock;
  try
    FWriteThrottle := AWriteThrottle;
  finally
    Unlock;
  end;
end;

function TTCPConnection.GetReadRate: Integer;
var
  E : Int64;
begin
  Lock;
  try
    if not FReadThrottle then
      TCPConnectionTransferUpdate(FReadTransferState, TCPGetTick, E);
    Result := FReadTransferState.TransferRate;
  finally
    Unlock;
  end;
end;

function TTCPConnection.GetWriteRate: Integer;
var
  E : Int64;
begin
  Lock;
  try
    if not FWriteThrottle then
      TCPConnectionTransferUpdate(FWriteTransferState, TCPGetTick, E);
    Result := FWriteTransferState.TransferRate;
  finally
    Unlock;
  end;
end;

procedure TTCPConnection.TriggerStateChange;
begin
  if Assigned(FOnStateChange) then
    try
      FOnStateChange(self, FState);
    except
      on E : Exception do
        Log(tlError, 'TriggerStateChange.Error:Error=%s[%s]', [E.ClassName, E.Message]);
    end;
end;

procedure TTCPConnection.TriggerReady;
begin
  if Assigned(FOnReady) then
    try
      FOnReady(self);
    except
      on E : Exception do
        Log(tlError, 'TriggerReady.Error:Error=%s[%s]', [E.ClassName, E.Message]);
    end;
end;

procedure TTCPConnection.TriggerReadShutdown;
begin
  if Assigned(FOnReadShutdown) then
    try
      FOnReadShutdown(self);
    except
      on E : Exception do
        Log(tlError, 'TriggerReadShutdown.Error:Error=%s[%s]', [E.ClassName, E.Message]);
    end;
end;

procedure TTCPConnection.TriggerShutdown;
begin
  if Assigned(FOnShutdown) then
    try
      FOnShutdown(self);
    except
      on E : Exception do
        Log(tlError, 'TriggerShutdown.Error:Error=%s[%s]', [E.ClassName, E.Message]);
    end;
end;

procedure TTCPConnection.TriggerClose;
begin
  if Assigned(FOnClose) then
    try
      FOnClose(self);
    except
      on E : Exception do
        Log(tlError, 'TriggerClose.Error:Error=%s[%s]', [E.ClassName, E.Message]);
    end;
end;

procedure TTCPConnection.TriggerRead;
begin
  if Assigned(FOnRead) then
    try
      FOnRead(self);
    except
      on E : Exception do
        Log(tlError, 'TriggerRead.Error:Error=%s[%s]', [E.ClassName, E.Message]);
    end;
end;

procedure TTCPConnection.TriggerWrite;
begin
  if Assigned(FOnWrite) then
    try
      FOnWrite(self);
    except
      on E : Exception do
        Log(tlError, 'TriggerWrite.Error:Error=%s[%s]', [E.ClassName, E.Message]);
    end;
end;

procedure TTCPConnection.TriggerReadActivity;
begin
  if Assigned(FOnReadActivity) then
    try
      FOnReadActivity(self);
    except
      on E : Exception do
        Log(tlError, 'TriggerReadActivity.Error:Error=%s[%s]', [E.ClassName, E.Message]);
    end;
end;

procedure TTCPConnection.TriggerWriteActivity;
begin
  if Assigned(FOnWriteActivity) then
    try
      FOnWriteActivity(self);
    except
      on E : Exception do
        Log(tlError, 'TriggerWriteActivity.Error:Error=%s[%s]', [E.ClassName, E.Message]);
    end;
end;

procedure TTCPConnection.TriggerReadBufferFull;
begin
  if Assigned(FOnReadBufferFull) then
    try
      FOnReadBufferFull(self);
    except
      on E : Exception do
        Log(tlError, 'TriggerReadBufferFull.Error:Error=%s[%s]', [E.ClassName, E.Message]);
    end;
end;

procedure TTCPConnection.TriggerWriteBufferEmpty;
begin
  if Assigned(FOnWriteBufferEmpty) then
    try
      FOnWriteBufferEmpty(self);
    except
      on E : Exception do
        Log(tlError, 'TriggerWriteBufferEmpty.Error:Error=%s[%s]', [E.ClassName, E.Message]);
    end;
end;

procedure TTCPConnection.TriggerWait;
begin
  if Assigned(FOnWait) then
    FOnWait(self);
end;

procedure TTCPConnection.TriggerWorkerFinished;
begin
  if Assigned(FOnWorkerFinished) then
    FOnWorkerFinished(self);
end;

function TTCPConnection.GetFirstActiveProxy: TTCPConnectionProxy;
var P : TTCPConnectionProxy;
begin
  Lock;
  try
    if FProxyList.Count = 0 then
      P := nil
    else
      P := FProxyList[0];
    while Assigned(P) and not (P.State in [prsNegotiating, prsFiltering]) do
      P := P.FNextProxy;
  finally
    Unlock;
  end;
  Result := P;
end;

procedure TTCPConnection.ProxyProcessReadData(const Buf; const BufSize: Integer; out ReadEventPending: Boolean);
var P : TTCPConnectionProxy;
begin
  Assert(FProxyConnection);
  Assert(FProxyList.Count > 0);

  ReadEventPending := False;
  P := GetFirstActiveProxy;
  if Assigned(P) then
    // pass to first active proxy
    P.ProcessReadData(Buf, BufSize)
  else
    begin
      // no active proxies, add data to read buffer
      Lock;
      try
        FProxyConnection := False;
        TCPBufferAddBuf(FReadBuffer, Buf, BufSize);
      finally
        Unlock;
      end;
      // allow user to read buffered data; flag pending event
      ReadEventPending := True;
    end;
end;

procedure TTCPConnection.ProxyProcessWriteData(const Buf; const BufSize: Integer);
var P : TTCPConnectionProxy;
begin
  Assert(BufSize > 0);
  Assert(FProxyConnection);
  Assert(FProxyList.Count > 0);

  P := GetFirstActiveProxy;
  if Assigned(P) then
    // pass to first active proxy
    P.ProcessWriteData(Buf, BufSize)
  else
    begin
      // no active proxies, send data
      Lock;
      try
        FProxyConnection := False;
        WriteToTransport(Buf, BufSize);
      finally
        Unlock;
      end;
    end;
end;

procedure TTCPConnection.ProxyLog(const AProxy: TTCPConnectionProxy; const LogType: TTCPLogType; const LogMsg: String; const LogLevel: Integer);
begin
  Assert(Assigned(AProxy));

  {$IFDEF TCP_DEBUG_PROXY}
  Log(LogType, 'Proxy[%s]:%s', [AProxy.ProxyName, LogMsg], LogLevel + 1);
  {$ENDIF}
end;

function GetNextFilteringProxy(const Proxy: TTCPConnectionProxy): TTCPConnectionProxy;
var P : TTCPConnectionProxy;
begin
  Assert(Assigned(Proxy));

  P := Proxy.FNextProxy;
  while Assigned(P) and not (P.State = prsFiltering) do
    P := P.FNextProxy;
  Result := P;
end;

procedure TTCPConnection.ProxyConnectionPutReadData(const AProxy: TTCPConnectionProxy; const Buf; const BufSize: Integer);
var P : TTCPConnectionProxy;
begin
  P := GetNextFilteringProxy(AProxy);
  if Assigned(P) then
    // pass along proxy chain
    P.ProcessReadData(Buf, BufSize)
  else
    // last proxy, add to read buffer, regardless of available size
    // reading from socket is throttled in FillBufferFromSocket when read buffer fills up
    begin
      Lock;
      try
        TCPBufferAddBuf(FReadBuffer, Buf, BufSize);
        // allow user to read buffered data; flag pending event
        FReadEventPending := True;
      finally
        Unlock;
      end;
    end;
end;

procedure TTCPConnection.ProxyConnectionPutWriteData(const AProxy: TTCPConnectionProxy; const Buf; const BufSize: Integer);
var P : TTCPConnectionProxy;
begin
  P := GetNextFilteringProxy(AProxy);
  if Assigned(P) then
    // pass along proxy chain
    P.ProcessWriteData(Buf, BufSize)
  else
    // last proxy, add to write buffer, regardless of available size
    begin
      Lock;
      try
        WriteToTransport(Buf, BufSize);
      finally
        Unlock;
      end;
    end;
end;

procedure TTCPConnection.ProxyConnectionClose(const AProxy: TTCPConnectionProxy);
begin
  Assert(FProxyConnection);
  // flag close pending; handled outside lock, after read pending
  Lock;
  try
    FClosePending := True;
  finally
    Unlock;
  end;
end;

// called when a proxy changes state
procedure TTCPConnection.ProxyStateChange(const AProxy: TTCPConnectionProxy; const AState: TTCPConnectionProxyState);
var P : TTCPConnectionProxy;
begin
  case AState of
    prsFiltering,
    prsFinished :
      begin
        Assert(FState = cnsProxyNegotiation);
        Lock;
        try
          P := AProxy.FNextProxy;
        finally
          Unlock;
        end;
        if Assigned(P) then
          P.Start
        else
          SetStateConnected;
      end;
    prsNegotiating : ;
    prsError  :
      begin
        ////
        FErrorMessage := AProxy.ErrorMessage;
      end;
    prsClosed : ;
  end;
end;

procedure TTCPConnection.StartProxies(out AProxiesFinished: Boolean);
var L : Integer;
    P : TTCPConnectionProxy;
begin
  Lock;
  try
    L := FProxyList.Count;
    if L = 0 then
      begin
        // no proxies
        AProxiesFinished := True;
        exit;
      end;
    P := FProxyList.Item[0];
  finally
    Unlock;
  end;
  // start proxy negotiation
  SetStateProxyNegotiation;
  P.Start;
  AProxiesFinished := False;
end;

procedure TTCPConnection.Start;
var ProxiesFin : Boolean;
begin
  Assert(FState in [cnsInit, cnsClosed]);
  {$IFDEF TCP_DEBUG}
  Log(tlDebug, 'Start');
  {$ENDIF}
  Lock;
  try
    TCPBufferClear(FReadBuffer);
    TCPBufferClear(FWriteBuffer);
    TCPConnectionTransferReset(FReadTransferState);
    TCPConnectionTransferReset(FWriteTransferState);
    FReadyNotified := False;
    FReadEventPending := False;
    FReadBufferFull := False;
    FReadProcessPending := False;
    FReadActivityPending := False;
    FWriteEventPending := False;
    FShutdownSendPending := False;
    FShutdownSent := False;
    FShutdownRecv := False;
    FShutdownComplete := False;
    FClosePending := False;
  finally
    Unlock;
  end;
  StartProxies(ProxiesFin);
  if ProxiesFin then
    SetStateConnected;
end;

// Pre: Socket is non-blocking
function TTCPConnection.FillBufferFromSocket(
         out RecvShutdown, RecvClosed, ReadEventPending, ReadBufFullEventPending: Boolean): Integer;
const
  BufferSize = TCP_BUFFER_DEFAULTMAXSIZE;
var
  Buffer : array[0..BufferSize - 1] of Byte;
  Avail, Size : Integer;
  IsHandleInvalid : Boolean;
  IsProxyConn : Boolean;
  {$IFDEF TCP_DEBUG}
  Unlocked : Boolean;
  {$ENDIF}
begin
  Result := 0;
  RecvShutdown := False;
  RecvClosed := False;
  ReadEventPending := False;
  ReadBufFullEventPending := False;
  {$IFDEF TCP_DEBUG}
  Unlocked := False;
  {$ENDIF}
  Lock;
  try
    // check space available in read buffer
    Avail := TCPBufferAvailable(FReadBuffer);
    if Avail <= 0 then
      // no space in buffer, don't read any more from socket
      // this will eventually throttle the actual TCP connection when the system's TCP receive buffer fills up
      // Set FReadBufferFull flag, since read event won't trigger again, this function must be called manually
      // next time there's space in the receive buffer
      begin
        if not FReadBufferFull then
          begin
            ReadBufFullEventPending := True;
            FReadBufferFull := True;
          end;
        exit;
      end;
    IsHandleInvalid := FSocket.IsSocketHandleInvalid;
    // receive from socket into local buffer
    if IsHandleInvalid then // socket may have been closed by a proxy
      begin
        {$IFDEF TCP_DEBUG}
        Unlock;
        Unlocked := True;
        Log(tlDebug, 'FillBufferFromSocket:SocketHandleInvalid');
        {$ENDIF}
        RecvClosed := True;
        exit;
      end;
    if Avail > BufferSize then
      Avail := BufferSize;
    Size := FSocket.Recv(Buffer, Avail);
    if Size = 0 then
      begin
        // socket shutdown
        if not FShutdownRecv then
          begin
            FShutdownRecv := True;
            RecvShutdown := True;
          end;
        ////RecvClosed := True;
        {$IFDEF TCP_DEBUG}
        Unlock;
        Unlocked := True;
        Log(tlDebug, 'FillBufferFromSocket:GracefulClose');
        {$ENDIF}
        exit;
      end;
    if Size < 0 then
      // nothing more to receive from socket
      exit;
    IsProxyConn := FProxyConnection;
    if not IsProxyConn then
      // move from local buffer to read buffer
      begin
        TCPBufferAddBuf(FReadBuffer, Buffer, Size);
        // allow user to read buffered data; flag pending event
        ReadEventPending := True;
      end;
  finally
    {$IFDEF TCP_DEBUG}
    if not Unlocked then
    {$ENDIF}
    Unlock;
  end;
  if IsProxyConn then
    // pass local buffer to proxies to process
    ProxyProcessReadData(Buffer, Size, ReadEventPending);
  // data received
  {$IFDEF TCP_DEBUG_DATA}
  Log(tlDebug, 'FillBufferFromSocket:Received:%db', [Size]);
  {$ENDIF}
  Result := Size;
end;

// Returns number of bytes written to socket
// Pre: Socket is non-blocking
function TTCPConnection.WriteBufferToSocket(out BufferEmptyBefore, BufferEmptied: Boolean): Integer;
var P : Pointer;
    SizeBuf, SizeWrite, SizeWritten : Integer;
    E : Boolean;
begin
  BufferEmptied := False;
  Lock;
  try
    SizeBuf := TCPBufferUsed(FWriteBuffer);
    // get write size
    E := SizeBuf <= 0;
    BufferEmptyBefore := E;
    if E then
      SizeWrite := 0
    else
    if FWriteThrottle and (FWriteThrottleRate > 0) then // throttled writing
      SizeWrite := TCPConnectionTransferThrottledSize(FWriteTransferState, TCPGetTick, FWriteThrottleRate, SizeBuf)
    else
      SizeWrite := SizeBuf;
    // handle nothing to send
    if SizeWrite = 0 then
      begin
        Result := 0;
        exit;
      end;
    // get buffer pointer
    P := TCPBufferPtr(FWriteBuffer);
    // send to socket
    Assert(Assigned(P));
    SizeWritten := FSocket.Send(P^, SizeWrite);
    // handle nothing sent
    if SizeWritten = 0 then
      begin
        Result := 0;
        exit;
      end;
    Assert(SizeWritten >= 0);
    // update transfer statistics
    TCPConnectionTransferredBytes(FWriteTransferState, SizeWritten);
    // discard successfully sent bytes from connection buffer
    TCPBufferDiscard(FWriteBuffer, SizeWritten);
  finally
    Unlock;
  end;
  // check if buffer emptied
  if SizeWritten = SizeBuf then
    BufferEmptied := True;
  {$IFDEF TCP_DEBUG_DATA}
  Log(tlDebug, 'WriteBufferToSocket:Fin:%d:%db:%db:%db',
      [Ord(BufferEmptied), SizeBuf, SizeWrite, SizeWritten]);
  {$ENDIF}
  Result := SizeWritten;
end;

procedure TTCPConnection.GetEventsToPoll(out WritePoll: Boolean); // out ReadPoll; out ProcessSocket)
begin
  Lock;
  try
    WritePoll :=
        FWriteEventPending or
        FShutdownSendPending or
        not TCPBufferEmpty(FWriteBuffer);
  finally
    Unlock;
  end;
end;

// Processes socket by reading/writing
// Pre: Socket is non-blocking
procedure TTCPConnection.ProcessSocket(
          const ProcessRead, ProcessWrite: Boolean;
          const ActivityTime: TDateTime;
          out Idle, Terminated: Boolean);
var
  Len : Integer;
  Error, Fin : Boolean;
  ShutdownReadWaiting : Boolean;
  ReadProcessPending : Boolean;
  ReadActivityPending : Boolean;
  ReadEventPending : Boolean;
  ReadDoProcess : Boolean;
  RecvShutdown : Boolean;
  RecvClosed : Boolean;
  RecvReadEvent : Boolean;
  RecvReadBufFullEvent : Boolean;
  RecvCloseNow : Boolean;
  RecvData : Boolean;
  RecvActivity : Boolean;
  RecvActivityNotified : Boolean;
  ReadDirect : Boolean;
  WriteBufEmptyBefore : Boolean;
  WriteBufEmptied : Boolean;
  WriteEventPending : Boolean;
  WriteDoProcess : Boolean;
  WriteEvent : Boolean;
  WriteBufEmptyEvent : Boolean;
  WriteShutdownNow : Boolean;
  WriteData : Boolean;
  WriteActivity : Boolean;
  ShutdownSendPending : Boolean;
  ShutdownComplete : Boolean;
  ClosePending : Boolean;
  TrackLastActivityTime : Boolean;
begin
  try
    Idle := True;
    Lock;
    try
      // handle closed socket
      if FSocket.IsSocketHandleInvalid then
        begin
          Terminated := True;
          exit;
        end;
      // check if read/write should process
      Assert(FState <> cnsInit);
      //read
      ReadProcessPending := FReadProcessPending;
      FReadProcessPending := False;
      ReadActivityPending := FReadActivityPending;
      FReadActivityPending := False;
      ReadEventPending := FReadEventPending;
      FReadEventPending := False;
      ClosePending := FClosePending;
      FClosePending := False;
      ShutdownReadWaiting := FShutdownSent and not FShutdownComplete;
      ReadDoProcess :=
          ProcessRead or
          ReadProcessPending or
          ReadActivityPending or
          ReadEventPending or
          ClosePending or
          ShutdownReadWaiting;
      ReadDirect := TCPBufferEmpty(FReadBuffer) and not FProxyConnection;
      // write
      WriteEventPending := FWriteEventPending;
      FWriteEventPending := False;
      ShutdownSendPending := FShutdownSendPending;
      FShutdownSendPending := False;
      WriteDoProcess :=
          WriteEventPending or
          ShutdownSendPending or
          (ProcessWrite and not TCPBufferEmpty(FWriteBuffer));
      // last activity times
      TrackLastActivityTime := FTrackLastActivityTime;
    finally
      Unlock;
    end;
    Terminated := False;
    // process read
    RecvActivity := False;
    if ReadDoProcess then
      begin
        RecvActivityNotified := False;
        if ReadActivityPending then
          RecvActivity := True;

        //// 2019/12/30 - allow event handler to read directly from socket
        //// 2020/05/20 - only trigger when connection buf empty and not proxy
        if ReadDirect then
          TriggerRead;

        Fin := False;
        repeat
          // receive data from socket into buffer
          try
            Len := FillBufferFromSocket(RecvShutdown, RecvClosed, RecvReadEvent, RecvReadBufFullEvent);
          except
            Len := 0;
            RecvShutdown := False;
            RecvClosed := True;
            RecvReadEvent := False;
            RecvReadBufFullEvent := False;
          end;

          // check pending flags
          if ReadEventPending then
            begin
              RecvReadEvent := True;
              ReadEventPending := False;
            end;
          RecvCloseNow := ClosePending;
          if RecvCloseNow then
            ClosePending := False;
          // check received data
          RecvData := Len > 0;
          if RecvData then
            begin
              RecvReadEvent := True; //// 2020/05/20 added to trigger TLS proxy recvd data
              RecvActivity := True;
              Idle := False;
            end
          else
            Fin := True;
          // perform pending actions
          if RecvActivity then
            if not RecvActivityNotified then
              begin
                RecvActivityNotified := True;
                TriggerReadActivity;
              end;
          if RecvReadBufFullEvent then
            TriggerReadBufferFull;
          if RecvReadEvent then
            TriggerRead;
          if RecvShutdown then
            begin
              TriggerReadShutdown;
              Lock;
              try
                if not FShutdownSendPending and not FShutdownSent then
                  begin
                    // send shutdown pending
                    ShutdownSendPending := True;
                    WriteDoProcess := True;
                  end;
              finally
                Unlock;
              end;
            end;
          if ShutdownReadWaiting and RecvShutdown then
            begin
              Lock;
              try
                FShutdownComplete := True;
              finally
                Unlock;
              end;
              TriggerShutdown;
              Close;
              Terminated := True;
              Fin := True;
            end
          else
          if RecvCloseNow then
            begin
              Close;
              Fin := True;
            end
          else
            if RecvClosed then
              begin
                // socket closed
                SetStateClosed;
                Terminated := True;
                Fin := True;
              end;
        until Fin;
      end;
    // process write
    WriteActivity := False;
    if WriteDoProcess then
      begin
        WriteEvent := False;
        WriteBufEmptyEvent := False;
        WriteShutdownNow := False;
        Error := False;
        try
          Len := WriteBufferToSocket(WriteBufEmptyBefore, WriteBufEmptied);
        except
          Len := 0;
          Error := True;
        end;
        // check write activity
        WriteData := Len > 0;
        if WriteEventPending then
          begin
            WriteActivity := True;
            WriteEvent := True;
            WriteBufEmptyEvent := True;
          end;
        // check write state
        if WriteBufEmptied then
          WriteBufEmptyEvent := True;
        if WriteBufEmptyBefore and ShutdownSendPending then
          WriteShutdownNow := True;
        if WriteData then
          begin
            // data sent
            Idle := False;
            WriteEvent := True;
            WriteActivity := True;
          end
        else
          begin
            if Error then
              // socket send error
              Terminated := True;
            // nothing sent
          end;
        // trigger write
        if WriteActivity then
          TriggerWriteActivity;
        if WriteEvent then
          TriggerWrite;
        // triger write buffer empty
        if WriteBufEmptyEvent then
          TriggerWriteBufferEmpty;
        // pending shutdown
        if WriteShutdownNow then
          begin
            DoShutdown(ShutdownComplete);
            if ShutdownComplete then
              begin
                TriggerShutdown;
                Close;
                Terminated := True;
              end;
          end;
      end;
      // set last activity time
      if RecvActivity or WriteActivity then
        if TrackLastActivityTime then
          begin
            Lock;
            try
              if RecvActivity then
                FLastReadActivityTime := ActivityTime;
              if WriteActivity then
                FLastWriteActivityTime := ActivityTime;
            finally
              Unlock;
            end;
          end;
  except
    on E : Exception do
      begin
        Idle := False;
        Terminated := True;
        {.IFDEF TCP_DEBUG}
        Log(tlError, 'ProcessSocket:Terminated:%s', [E.Message]);
        {.ENDIF}
      end;
  end;
end;

function TTCPConnection.GetLastReadActivityTime: TDateTime;
begin
  Lock;
  try
    Result := FLastReadActivityTime;
  finally
    Unlock;
  end;
end;

function TTCPConnection.GetLastWriteActivityTime: TDateTime;
begin
  Lock;
  try
    Result := FLastWriteActivityTime;
  finally
    Unlock;
  end;
end;

function TTCPConnection.GetLastActivityTime: TDateTime;
begin
  Lock;
  try
    if FLastReadActivityTime > FLastWriteActivityTime then
      Result := FLastReadActivityTime
    else
    if FLastWriteActivityTime > FCreationTime then
      Result := FLastWriteActivityTime
    else
      Result := FCreationTime;
  finally
    Unlock;
  end;
end;

// LocateByteCharInBuffer
// Returns position of Delimiter in buffer
// Returns >= 0 if found in buffer
// Returns -1 if not found in buffer
// MaxSize specifies maximum bytes before delimiter, of -1 for no limit
function TTCPConnection.LocateByteCharInBuffer(const ADelimiter: ByteCharSet; const AMaxSize: Integer): Integer;
begin
  Result := TCPBufferLocateByteChar(FReadBuffer, ADelimiter, AMaxSize);
end;

// LocateByteStrInBuffer
// Returns position of Delimiter in buffer
// Returns >= 0 if found in buffer
// Returns -1 if not found in buffer
// MaxSize specifies maximum bytes before delimiter, of -1 for no limit
function TTCPConnection.LocateByteStrInBuffer(const ADelimiter: RawByteString; const AMaxSize: Integer): Integer;
var DelLen  : Integer;
    BufSize : Integer;
    LocLen  : Integer;
    BufPtr  : PByteChar;
    DelPtr  : PByteChar;
    I       : Integer;
begin
  if AMaxSize = 0 then
    begin
      Result := -1;
      exit;
    end;
  DelLen := Length(ADelimiter);
  if DelLen = 0 then
    begin
      Result := -1;
      exit;
    end;
  BufSize := FReadBuffer.Used;
  if BufSize < DelLen then
    begin
      Result := -1;
      exit;
    end;
  if AMaxSize < 0 then
    LocLen := BufSize
  else
    if BufSize < AMaxSize then
      LocLen := BufSize
    else
      LocLen := AMaxSize;
  BufPtr := TCPBufferPtr(FReadBuffer);
  DelPtr := PByteChar(ADelimiter);
  for I := 0 to LocLen - DelLen do
    if TCPCompareMem(BufPtr^, DelPtr^, DelLen) then
      begin
        Result := I;
        exit;
      end
    else
      Inc(BufPtr);
  Result := -1;
end;

// PeekDelimited
// Returns number of bytes transferred to buffer, including delimiter
// Returns -1 if not found in buffer
// Returns >= 0 if found.
// MaxSize specifies maximum bytes before delimiter, of -1 for no limit
function TTCPConnection.PeekDelimited(var Buf; const BufSize: Integer;
         const ADelimiter: TRawByteCharSet; const AMaxSize: Integer): Integer;
var DelPos : Integer;
    BufPtr : PByteChar;
    BufLen : Integer;
begin
  Lock;
  try
    DelPos := LocateByteCharInBuffer(ADelimiter, AMaxSize);
    if DelPos >= 0 then
      begin
        // found
        BufPtr := TCPBufferPtr(FReadBuffer);
        BufLen := DelPos + 1;
        if BufLen > BufSize then
          BufLen := BufSize;
        Move(BufPtr^, Buf, BufLen);
        Result := BufLen;
      end
    else
      Result := -1;
  finally
    Unlock;
  end;
end;

// PeekDelimited
// Returns number of bytes transferred to buffer, including delimiter
// Returns -1 if not found in buffer
// Returns >= 0 if found.
// MaxSize specifies maximum bytes before delimiter, of -1 for no limit
function TTCPConnection.PeekDelimited(var Buf; const BufSize: Integer;
         const ADelimiter: RawByteString; const AMaxSize: Integer): Integer;
var DelPos : Integer;
    BufPtr : PByteChar;
    BufLen : Integer;
begin
  Assert(ADelimiter <> '');
  Lock;
  try
    DelPos := LocateByteStrInBuffer(ADelimiter, AMaxSize);
    if DelPos >= 0 then
      begin
        // found
        BufPtr := TCPBufferPtr(FReadBuffer);
        BufLen := DelPos + Length(ADelimiter);
        if BufLen > BufSize then
          BufLen := BufSize;
        Move(BufPtr^, Buf, BufLen);
        Result := BufLen;
      end
    else
      Result := -1;
  finally
    Unlock;
  end;
end;

// Read a number of bytes from read buffer and transport.
// Return the number of bytes actually read.
// Throttles reading.
function TTCPConnection.Read(var Buf; const BufSize: Integer): Integer;
var
  BufPtr : PByteChar;
  SizeRead, SizeReadBuf, SizeReadSocket, SizeRecv, SizeRemain, SizeTotal : Integer;
begin
  if BufSize <= 0 then
    begin
      Result := 0;
      exit;
    end;
  Lock;
  try
    // get read size
    if FReadThrottle then // throttled reading
      begin
        SizeRead := TCPConnectionTransferThrottledSize(FReadTransferState, TCPGetTick, FReadThrottleRate, BufSize);
        // handle nothing to read
        if SizeRead <= 0 then
          begin
            Result := 0;
            exit;
          end;
      end
    else
      SizeRead := BufSize;
    // read from buffer
    SizeReadBuf := TCPBufferRemove(FReadBuffer, Buf, SizeRead);
    if SizeReadBuf > 0 then
      if FReadBufferFull then
        begin
          FReadBufferFull := False;
          FReadProcessPending := True;
        end;
    if SizeReadBuf = SizeRead then
      // required number of bytes was in buffer
      SizeReadSocket := 0
    else
    if FProxyConnection then
      // don't read directly from socket if this connection has proxies
      SizeReadSocket := 0
    else
    if FSocket.IsSocketHandleInvalid then
      SizeReadSocket := 0
    else
      begin
        // calculate remaining bytes to read
        SizeRemain := SizeRead - SizeReadBuf;
        // read from socket
        BufPtr := @Buf;
        Inc(BufPtr, SizeReadBuf);
        try
          SizeRecv := FSocket.Recv(BufPtr^, SizeRemain);
        except
          SizeRecv := -1;
        end;
        if SizeRecv = 0 then
          begin
            // Graceful shutdown
            ////SetStateClosed;
            FReadProcessPending := True;
            SizeReadSocket := 0;
          end
        else
        if SizeRecv < 0 then
          SizeReadSocket := 0
        else
          SizeReadSocket := SizeRecv;
        if SizeReadSocket > 0 then
          begin
            FReadActivityPending := True;
            //// 2020/03/28 - Direct reading from socket in ProcessSocket needs updating last read activity
            if FTrackLastActivityTime then
              FLastReadActivityTime := Now;
          end;
      end;
    SizeTotal := SizeReadBuf + SizeReadSocket;
    // update transfer statistics
    TCPConnectionTransferredBytes(FReadTransferState, SizeTotal);
  finally
    Unlock;
  end;
  {$IFDEF TCP_DEBUG_DATA}
  Log(tlDebug, 'Read:Fin:%db:%db:%db:%db',
      [BufSize, SizeRead, SizeReadBuf, SizeReadSocket]);
  {$ENDIF}
  // return number of bytes read
  Result := SizeTotal;
end;

function TTCPConnection.ReadByteString(const AStrLen: Integer): RawByteString;
var ReadLen : Integer;
begin
  if AStrLen <= 0 then
    begin
      Result := '';
      exit;
    end;
  SetLength(Result, AStrLen);
  ReadLen := Read(Pointer(Result)^, AStrLen);
  if ReadLen < AStrLen then
    SetLength(Result, ReadLen);
end;

function TTCPConnection.ReadBytes(const ASize: Integer): TBytes;
var ReadLen : Integer;
begin
  if ASize <= 0 then
    begin
      Result := nil;
      exit;
    end;
  SetLength(Result, ASize);
  ReadLen := Read(Pointer(Result)^, ASize);
  if ReadLen < ASize then
    SetLength(Result, ReadLen);
end;

// Discard a number of bytes from the read buffer.
// Returns the number of bytes actually discarded.
// No throttling and no reading from transport.
function TTCPConnection.Discard(const ASize: Integer): Integer;
var SizeDiscarded : Integer;
begin
  // handle nothing to discard
  if ASize <= 0 then
    begin
      Result := 0;
      exit;
    end;
  Lock;
  try
    // discard from buffer
    SizeDiscarded := TCPBufferDiscard(FReadBuffer, ASize);
    if SizeDiscarded > 0 then
      if FReadBufferFull then
        begin
          FReadBufferFull := False;
          FReadProcessPending := True;
        end;
    // update transfer statistics
    TCPConnectionTransferredBytes(FReadTransferState, SizeDiscarded);
  finally
    Unlock;
  end;
  {$IFDEF TCP_DEBUG_DATA}
  Log(tlDebug, 'DiscardedFromBuffer:%db:%db', [Size, SizeDiscarded]);
  {$ENDIF}
  // return number of bytes discarded
  Result := SizeDiscarded;
end;

function TTCPConnection.WriteToTransport(const Buf; const BufSize: Integer): Integer;
var
  BufP : PByteChar;
  UseBuf : Boolean;
  SizeToBuf : Integer;
  SizeToSocket : Integer;
begin
  Assert(BufSize > 0);
  // if there is already data in the write buffer then add to the write buffer; or
  // if write is being throttled then add to the write buffer
  UseBuf := (TCPBufferUsed(FWriteBuffer) > 0) or FWriteThrottle;
  if UseBuf then
    begin
      TCPBufferAddBuf(FWriteBuffer, Buf, BufSize);
      SizeToBuf := BufSize;
      SizeToSocket := 0;
    end
  else
    begin
      // send the data directly to the socket
      SizeToSocket := FSocket.Send(Buf, BufSize);
      // update transfer statistics
      TCPConnectionTransferredBytes(FWriteTransferState, SizeToSocket);
      if SizeToSocket < BufSize then
        begin
          // add the data not sent to the socket to the write buffer
          BufP := @Buf;
          Inc(BufP, SizeToSocket);
          SizeToBuf := BufSize - SizeToSocket;
          TCPBufferAddBuf(FWriteBuffer, BufP^, SizeToBuf);
        end
      else
        begin
          FWriteEventPending := True; // all data sent directly to socket
          SizeToBuf := 0;
        end;
    end;
  {$IFDEF TCP_DEBUG_DATA}
  ////Log(tlDebug, 'WriteToTransport:BufSize=%db:ToSocket=%db:ToBuf=%db', [BufSize, SizeToSocket, SizeToBuf]);
  {$ENDIF}
  Result := SizeToSocket + SizeToBuf;
end;

// Write a number of bytes to transport
// No throtling
function TTCPConnection.Write(const Buf; const BufSize: Integer): Integer;
var IsProxy : Boolean;
begin
  Result := 0;
  if BufSize <= 0 then
    exit;
  Lock;
  try
    if FState = cnsClosed then
      raise ETCPConnection.Create(SError_ConnectionClosed);
    // if this connection has proxies then pass the data to the proxies
    IsProxy := FProxyConnection;
    if not IsProxy then
      // send data to buffer/socket
      Result := WriteToTransport(Buf, BufSize);
  finally
    Unlock;
  end;
  if IsProxy then
    begin
      ProxyProcessWriteData(Buf, BufSize);
      Result := BufSize;
    end;
  Assert(Result = BufSize);
end;

function TTCPConnection.WriteByteString(const AStr: RawByteString): Integer;
var Len : Integer;
begin
  Len := Length(AStr);
  if Len <= 0 then
    begin
      Result := 0;
      exit;
    end;
  Result := Write(Pointer(AStr)^, Len);
end;

function TTCPConnection.WriteBytes(const B: TBytes): Integer;
var Len : Integer;
begin
  Len := Length(B);
  if Len <= 0 then
    begin
      Result := 0;
      exit;
    end;
  Result := Write(Pointer(B)^, Len);
end;

// Peek a number of bytes from buffer.
// No throttling; no reading from transport
function TTCPConnection.Peek(var Buf; const BufSize: Int32): Integer;
begin
  Lock;
  try
    Result := TCPBufferPeek(FReadBuffer, Buf, BufSize);
  finally
    Unlock;
  end;
end;

function TTCPConnection.PeekByte(out B: Byte): Boolean;
begin
  Lock;
  try
    Result := TCPBufferPeekByte(FReadBuffer, B);
  finally
    Unlock;
  end;
end;

function TTCPConnection.PeekByteString(const AStrLen: Integer): RawByteString;
var PeekLen : Integer;
begin
  if AStrLen <= 0 then
    begin
      Result := '';
      exit;
    end;
  SetLength(Result, AStrLen);
  PeekLen := Peek(Pointer(Result)^, AStrLen);
  if PeekLen < AStrLen then
    SetLength(Result, PeekLen);
end;

function TTCPConnection.PeekBytes(const ASize: Integer): TBytes;
var PeekLen : Integer;
begin
  if ASize <= 0 then
    begin
      Result := nil;
      exit;
    end;
  SetLength(Result, ASize);
  PeekLen := Peek(Pointer(Result)^, ASize);
  if PeekLen < ASize then
    SetLength(Result, PeekLen);
end;

// Reads a line delimited by specified Delimiter
// MaxLineLength is maximum line length excluding the delimiter
// Returns False if line not available
// Returns True if line read
function TTCPConnection.ReadLine(var Line: RawByteString; const ADelimiter: RawByteString; const AMaxLineLength: Integer): Boolean;
var
  DelPos : Integer;
  DelLen : Integer;
begin
  Assert(ADelimiter <> '');
  Lock;
  try
    DelPos := LocateByteStrInBuffer(ADelimiter, AMaxLineLength);
    Result := DelPos >= 0;
    if not Result then
      exit;
    SetLength(Line, DelPos);
    if DelPos > 0 then
      Read(PByteChar(Line)^, DelPos);
    DelLen := Length(ADelimiter);
    Discard(DelLen);
  finally
    Unlock;
  end;
end;

procedure TTCPConnection.DoShutdown(out AShutdownComplete: Boolean);
begin
  {$IFDEF TCP_DEBUG}
  Log(tlDebug, 'DoShutDown:%db:%db', [GetReadBufferUsed, GetWriteBufferUsed]);
  {$ENDIF}
  AShutdownComplete := False;
  // Sends TCP FIN message to close connection and
  // disallow any further sending on the socket
  Lock;
  try
    if FShutdownSent then
      exit;
    FShutdownSent := True;
    FSocket.Shutdown(ssSend);
    AShutdownComplete := FShutdownSent;
    if AShutdownComplete then
      FShutdownComplete := True;
  finally
    Unlock;
  end;
end;

procedure TTCPConnection.Shutdown;
var
  ShutdownNow : Boolean;
  ShutdownComplete : Boolean;
begin
  {$IFDEF TCP_DEBUG}
  Log(tlDebug, 'ShutDown:%db:%db', [GetReadBufferUsed, GetWriteBufferUsed]);
  {$ENDIF}
  Lock;
  try
    if FState = cnsClosed then
      exit;
    ShutdownNow := False;
    if TCPBufferUsed(FWriteBuffer) > 0 then
      // defer shutdown until write buffer is emptied to socket
      FShutdownSendPending := True
    else
      ShutdownNow := True;
  finally
    Unlock;
  end;
  if ShutdownNow then
    begin
      DoShutDown(ShutdownComplete);
      if ShutdownComplete then
        begin
          TriggerShutdown;
          Close;
        end;
    end;
end;

function TTCPConnection.IsShutdownComplete: Boolean;
begin
  Lock;
  try
    Result := (FState = cnsClosed) or FShutdownComplete;
  finally
    Unlock;
  end;
end;

procedure TTCPConnection.Close;
begin
  {$IFDEF TCP_DEBUG}
  Log(tlDebug, 'Close:%db:%db', [GetReadBufferUsed, GetWriteBufferUsed]);
  {$ENDIF}
  if not SetStateClosed then
    exit;
  Lock;
  try
    FSocket.Close;
  finally
    Unlock;
  end;
end;

procedure TTCPConnection.TerminateWorkerThread;
begin
  Lock;
  try
    if Assigned(FWorkerThread) then
      FWorkerThread.Terminate;
  finally
    Unlock;
  end;
end;

procedure TTCPConnection.WaitForWorkerThread;
begin
  Lock;
  try
    if Assigned(FWorkerThread) then
      try
        FWorkerThread.WaitFor;
      except
      end;
  finally
    Unlock;
  end;
end;

procedure TTCPConnection.Wait;
begin
  TriggerWait;
  Sleep(5);
end;

function TTCPConnection.GetBlockingConnection: TTCPBlockingConnection;
begin
  Lock;
  try
    if not Assigned(FBlockingConnection) then
      FBlockingConnection := TTCPBlockingConnection.Create(self);
    Result := FBlockingConnection;
  finally
    Unlock;
  end;
end;

procedure TTCPConnection.StartWorkerThread;
begin
  Assert(not Assigned(FWorkerThread));

  FWorkerThread := TTCPConnectionWorkerThread.Create(self);
end;

procedure TTCPConnection.WorkerThreadExecute(const AThread: TThread);
var
  WorkerThread : TTCPConnectionWorkerThread;
  Event : TTCPConnectionWorkerExecuteEvent;
  CloseOnExit : Boolean;

  function IsTerminated: Boolean;
  begin
    Result := WorkerThread.Terminated;
  end;

begin
  try
    WorkerThread := AThread as TTCPConnectionWorkerThread;
    CloseOnExit := False;
    try
      try
        Event := FOnWorkerExecute;
        if Assigned(Event) then
          Event(self, GetBlockingConnection, CloseOnExit);
      finally
        TriggerWorkerFinished;
      end;
    finally
      if not IsTerminated then
        if CloseOnExit then
          Close;
    end;
  except
    on E : Exception do
      if not IsTerminated then
        begin
          Lock;
          try
            FWorkerErrorMsg := E.Message;
            FWorkerErrorClass := E.ClassName;
          finally
            Unlock;
          end;
        end;
  end;
end;



{                                                                              }
{ TCP Blocking Connection                                                      }
{                                                                              }
constructor TTCPBlockingConnection.Create(const AConnection: TTCPConnection);
begin
  Assert(Assigned(AConnection));
  inherited Create;
  FConnection := AConnection;
end;

destructor TTCPBlockingConnection.Destroy;
begin
  inherited Destroy;
end;

procedure TTCPBlockingConnection.Finalise;
begin
  FConnection := nil;
end;

procedure TTCPBlockingConnection.Wait;
begin
  FConnection.Wait;
end;

// Wait until one of the States or time out.
function TTCPBlockingConnection.WaitForState(const AStates: TTCPConnectionStates; const ATimeOutMs: Integer): TTCPConnectionState;
var T : Word32;
    S : TTCPConnectionState;
    C : TTCPConnection;
begin
  T := TCPGetTick;
  C := FConnection;
  repeat
    S := C.GetState;
    if S in AStates then
      break;
    if ATimeOutMs >= 0 then
      if TCPTickDelta(T, TCPGetTick) >= ATimeOutMs then
        break;
    Wait;
  until False;
  Result := S;
end;

// Wait until amount of data received, closed or time out.
function TTCPBlockingConnection.WaitForReceiveData(const ABufferSize: Integer; const ATimeOutMs: Integer): Boolean;
var T : Word32;
    L : Integer;
    C : TTCPConnection;
begin
  Assert(Assigned(FConnection));
  T := TCPGetTick;
  C := FConnection;
  repeat
    L := C.ReadBufferUsed;
    if L >= ABufferSize then
      break;
    if ATimeOutMs >= 0 then
      if TCPTickDelta(T, TCPGetTick) >= ATimeOutMs then
        break;
    if C.GetState = cnsClosed then
      break;
    Wait;
  until False;
  Result := L >= ABufferSize;
end;

// Wait until send buffer is cleared to socket, closed or time out.
function TTCPBlockingConnection.WaitForTransmitFin(const ATimeOutMs: Integer): Boolean;
var T : Word32;
    L : Integer;
    C : TTCPConnection;
begin
  Assert(Assigned(FConnection));
  T := TCPGetTick;
  C := FConnection;
  repeat
    L := C.WriteBufferUsed;
    if L = 0 then
      break;
    if ATimeOutMs >= 0 then
      if TCPTickDelta(T, TCPGetTick) >= ATimeOutMs then
        break;
    if C.GetState = cnsClosed then
      break;
    Wait;
  until False;
  Result := L = 0;
end;

// Wait until socket is closed or timeout.
function TTCPBlockingConnection.WaitForClose(const ATimeOutMs: Integer): Boolean;
begin
  Result := WaitForState([cnsClosed], ATimeOutMs) = cnsClosed;
end;

// Wait for read data until required size is available or timeout.
function TTCPBlockingConnection.Read(var Buf; const BufferSize: Integer; const ATimeOutMs: Integer): Integer;
begin
  if not WaitForReceiveData(BufferSize, ATimeOutMs) then
    raise ETCPConnection.Create(SError_TimedOut);
  Result := FConnection.Read(Buf, BufferSize);
end;

// Write and wait for write buffers to empty or timeout.
function TTCPBlockingConnection.Write(const Buf; const BufferSize: Integer; const ATimeOutMs: Integer): Integer;
begin
  Assert(Assigned(FConnection));
  Result := FConnection.Write(Buf, BufferSize);
  if Result > 0 then
    WaitForTransmitFin(ATimeOutMs);
end;

// Does a graceful shutdown and waits for connection to close or timeout.
// Data received during shutdown is available after connection close.
//   SettleTimeMs is the maximum time to wait for output buffer to clear before initiating Shutdown.
//   TransmitTimeOutMs is the maximum time to wait for output buffers to clear after Shutdown.
//   CloseTimeOutMs is the maximum time to wait for the graceful close after output buffers cleared.
procedure TTCPBlockingConnection.Shutdown(
          const SettleTimeMs: Integer;
          const TransmitTimeOutMs: Integer;
          const CloseTimeOutMs: Integer);
begin
  Assert(Assigned(FConnection));
  if SettleTimeMs > 0 then
    WaitForTransmitFin(SettleTimeMs);
  FConnection.Shutdown;
  if not WaitForTransmitFin(TransmitTimeOutMs) then
    raise ETCPConnection.Create(SError_TimedOut);
  if not WaitForClose(CloseTimeOutMs) then
    raise ETCPConnection.Create(SError_TimedOut);
end;

// Closes immediately and waits for connection to close or timeout.
procedure TTCPBlockingConnection.Close(const ATimeOutMs: Integer);
begin
  Assert(Assigned(FConnection));
  FConnection.Close;
  if not WaitForClose(ATimeOutMs) then
    raise ETCPConnection.Create(SError_TimedOut);
end;



end.

