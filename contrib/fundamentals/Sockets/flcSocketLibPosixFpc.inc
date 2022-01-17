{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals 5.00                                        }
{   File name:        flcSocketLibPosixFpc.inc                                 }
{   File version:     5.09                                                     }
{   Description:      FreePascal Posix socket library                          }
{                                                                              }
{   Copyright:        Copyright © 2001-2020, David J Butler                    }
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
{   2005/07/13  4.01  Initial Unix support.                                    }
{   2005/07/17  4.02  Minor improvements.                                      }
{   2005/12/06  4.03  Compilable with FreePascal 2.0.1 Linux i386.             }
{   2005/12/10  4.04  Revised for Fundamentals 4.                              }
{   2006/12/14  4.05  IP6 support.                                             }
{   2007/12/29  4.06  Revision.                                                }
{   2014/04/23  4.07  Revision.                                                }
{   2016/01/09  5.08  Revised for Fundamentals 5.                              }
{   2018/09/09  5.09  Poll function.                                           }
{                                                                              }
{ Supported compilers:                                                         }
{                                                                              }
{   FreePascal 3.0.4 Win64              5.09  2020/06/02                       }
{                                                                              }
{ References:                                                                  }
{                                                                              }
{   http://www.die.net/doc/linux/man/man7/socket.7.html                        }
{                                                                              }
{******************************************************************************}

interface

uses
  { System }
  SysUtils,
  BaseUnix,
  UnixType,
  Sockets,
  DynLibs,

  { Fundamentals }
  flcStdTypes;



{                                                                              }
{ Unix socket types                                                            }
{                                                                              }
type
  TSocket = Word32;

const
  INVALID_SOCKET = TSocket(not 0);

type
  SunB = packed record
    s_b1, s_b2, s_b3, s_b4 : Byte;
  end;
  SunW = packed record
    s_w1, s_w2 : Word;
  end;
  PInAddr = ^TInAddr;
  in_addr = record
    case Integer of
      0 : (S_un_b : SunB);
      1 : (S_un_w : SunW);
      2 : (S_addr : Word32);
  end;
  TInAddr = in_addr;

  PSockAddrIn = ^TSockAddrIn;
  sockaddr_in = record
    case Integer of
      0 : (sin_family : Word;
           sin_port   : Word;
           sin_addr   : TInAddr;
           sin_zero   : array[0..7] of Byte);
      1 : (sa_family  : Word;
           sa_data    : array[0..13] of Byte);
  end;
  TSockAddrIn = sockaddr_in;

  PIn6Addr = ^TIn6Addr;
  in6_addr = packed record
    case Integer of
      0 : (u6_addr8  : packed array[0..15] of Byte);
      1 : (u6_addr16 : packed array[0..7] of Word);
      2 : (u6_addr32 : packed array[0..3] of Word32);
      3 : (s6_addr   : packed array[0..15] of ShortInt);
      4 : (s6_addr8  : packed array[0..15] of ShortInt);
      5 : (s6_addr16 : packed array[0..7] of SmallInt);
      6 : (s6_addr32 : packed array[0..3] of Int32);
  end;
  TIn6Addr = in6_addr;



{                                                                              }
{ Unix socket constants                                                        }
{                                                                              }
const
  // Address family
  {$IFDEF FREEPASCAL}
  AF_UNSPEC = Sockets.AF_UNSPEC;
  AF_INET   = Sockets.AF_INET;
  AF_INET6  = Sockets.AF_INET6;
  AF_MAX    = Sockets.AF_MAX;
  {$ELSE}
  AF_UNSPEC = 0;
  AF_INET   = 2;
  AF_INET6  = 10;
  AF_MAX    = 24;
  {$ENDIF}

  // Protocol family
  PF_UNSPEC = AF_UNSPEC;
  PF_INET   = AF_INET;
  PF_INET6  = AF_INET6;
  PF_MAX    = AF_MAX;

  // Socket type
  SOCK_STREAM    = 1;
  SOCK_DGRAM     = 2;
  SOCK_RAW       = 3;
  SOCK_RDM       = 4;
  SOCK_SEQPACKET = 5;

  // IP protocol
  {$IFDEF FREEPASCAL}
  IPPROTO_IP     = Sockets.IPPROTO_IP;
  IPPROTO_ICMP   = Sockets.IPPROTO_ICMP;
  IPPROTO_IGMP   = Sockets.IPPROTO_IGMP;
  IPPROTO_TCP    = Sockets.IPPROTO_TCP;
  IPPROTO_UDP    = Sockets.IPPROTO_UDP;
  IPPROTO_IPV6   = Sockets.IPPROTO_IPV6;
  IPPROTO_ICMPV6 = Sockets.IPPROTO_ICMPV6;
  IPPROTO_RAW    = Sockets.IPPROTO_RAW;
  IPPROTO_MAX    = Sockets.IPPROTO_MAX;
  {$ELSE}
  IPPROTO_IP     = 0;
  IPPROTO_ICMP   = 1;
  IPPROTO_IGMP   = 2;
  IPPROTO_TCP    = 6;
  IPPROTO_UDP    = 17;
  IPPROTO_IPV6   = 41;
  IPPROTO_ICMPV6 = 58;
  IPPROTO_RAW    = 255;
  IPPROTO_MAX    = 256;
  {$ENDIF}

  // Socket level
  SOL_SOCKET    = 1;

  // Socket options
  {$IFDEF FREEPASCAL}
  SO_DEBUG     = Sockets.SO_DEBUG;
  SO_REUSEADDR = Sockets.SO_REUSEADDR;
  SO_TYPE      = Sockets.SO_TYPE;
  SO_ERROR     = Sockets.SO_ERROR;
  SO_DONTROUTE = Sockets.SO_DONTROUTE;
  SO_BROADCAST = Sockets.SO_BROADCAST;
  SO_SNDBUF    = Sockets.SO_SNDBUF;
  SO_RCVBUF    = Sockets.SO_RCVBUF;
  SO_KEEPALIVE = Sockets.SO_KEEPALIVE;
  SO_OOBINLINE = Sockets.SO_OOBINLINE;
  SO_NO_CHECK  = Sockets.SO_NO_CHECK;
  SO_PRIORITY  = Sockets.SO_PRIORITY;
  SO_LINGER    = Sockets.SO_LINGER;
  SO_BSDCOMPAT = Sockets.SO_BSDCOMPAT;
  //SO_REUSEPORT = Sockets.SO_REUSEPORT;
  SO_PASSCRED  = Sockets.SO_PASSCRED;
  SO_PEERCRED  = Sockets.SO_PEERCRED;
  SO_RCVLOWAT  = Sockets.SO_RCVLOWAT;
  SO_SNDLOWAT  = Sockets.SO_SNDLOWAT;
  SO_RCVTIMEO  = Sockets.SO_RCVTIMEO;
  SO_SNDTIMEO  = Sockets.SO_SNDTIMEO;
  {$ELSE}
  SO_DEBUG     = 1;
  SO_REUSEADDR = 2;
  SO_TYPE      = 3;
  SO_ERROR     = 4;
  SO_DONTROUTE = 5;
  SO_BROADCAST = 6;
  SO_SNDBUF    = 7;
  SO_RCVBUF    = 8;
  SO_KEEPALIVE = 9;
  SO_OOBINLINE = 10;
  SO_NO_CHECK  = 11;
  SO_PRIORITY  = 12;
  SO_LINGER    = 13;
  SO_BSDCOMPAT = 14;
  SO_REUSEPORT = 15;
  SO_PASSCRED  = 16;
  SO_PEERCRED  = 17;
  SO_RCVLOWAT  = 18;
  SO_SNDLOWAT  = 19;
  SO_RCVTIMEO  = 20;
  SO_SNDTIMEO  = 21;
  {$ENDIF}

  // TCP options
  TCP_NODELAY = $0001;

  // Send/Recv options
  {$IFDEF FREEPASCAL}
  MSG_OOB       = Sockets.MSG_OOB;
  MSG_PEEK      = Sockets.MSG_PEEK;
  MSG_DONTROUTE = Sockets.MSG_DONTROUTE;
  MSG_TRYHARD   = Sockets.MSG_TRYHARD;
  MSG_CTRUNC    = Sockets.MSG_CTRUNC;
  MSG_PROXY     = Sockets.MSG_PROXY;
  MSG_TRUNC     = Sockets.MSG_TRUNC;
  MSG_DONTWAIT  = Sockets.MSG_DONTWAIT;
  MSG_EOR       = Sockets.MSG_EOR;
  MSG_WAITALL   = Sockets.MSG_WAITALL;
  MSG_FIN       = Sockets.MSG_FIN;
  MSG_SYN       = Sockets.MSG_SYN;
  MSG_CONFIRM   = Sockets.MSG_CONFIRM;
  MSG_RST       = Sockets.MSG_RST;
  MSG_ERRQUERE  = Sockets.MSG_ERRQUERE;
  MSG_NOSIGNAL  = Sockets.MSG_NOSIGNAL;
  MSG_MORE      = Sockets.MSG_MORE;
  {$ELSE}
  MSG_OOB       = $0001;
  MSG_PEEK      = $0002;
  MSG_DONTROUTE = $0004;
  MSG_TRYHARD   = MSG_DONTROUTE;
  MSG_CTRUNC    = $0008;
  MSG_PROXY     = $0010;
  MSG_TRUNC     = $0020;
  MSG_DONTWAIT  = $0040;
  MSG_EOR       = $0080;
  MSG_WAITALL   = $0100;
  MSG_FIN       = $0200;
  MSG_SYN       = $0400;
  MSG_CONFIRM   = $0800;
  MSG_RST       = $1000;
  MSG_ERRQUERE  = $2000;
  MSG_NOSIGNAL  = $4000;
  MSG_MORE      = $8000;
  {$ENDIF}

  // Ioctl functions
  {$IFDEF OS_OSX}
  FIONREAD = $4004667F;
  FIONBIO	 = $8004667E;
  FIOASYNC = $8004667D;
  {$ELSE}
  FIONREAD = $541B;
  FIONBIO  = $5421;
  FIOASYNC = $5452;
  {$ENDIF}

  // IP4 addresses
  INADDR_ANY       = Word32($00000000);
  INADDR_LOOPBACK  = Word32($7F000001);
  INADDR_BROADCAST = Word32(not 0);
  INADDR_NONE      = Word32(not 0);



{                                                                              }
{ Unix socket types                                                            }
{                                                                              }
type
  PLinger = ^TLinger;
  linger = record
    l_onoff  : LongBool;
    l_linger : Int32;
  end;
  TLinger = linger;

type
  TFD_MASK = Word32;

const
  FD_SETSIZE   = 1024;
  NFDBITS      = 8 * Sizeof(TFD_MASK);
  FD_ARRAYSIZE = FD_SETSIZE div NFDBITS;

type
  PFDSet = ^TFDSet;
  TFDSet = record
    fds_bits : packed array[0..FD_ARRAYSIZE - 1] of TFD_MASK;
  end;

function  FD_ISSET(fd: Int32; const fdset: TFDSet): Boolean;
procedure FD_SET(const fd: Int32; var fdset: TFDSet);
procedure FD_CLR(const fd: Int32; var fdset: TFDSet);
procedure FD_ZERO(out fdset: TFDSet);
function  FD_COUNT(const fdset: TFDSet): Integer;

type
  PSockAddrIn6 = ^TSockAddrIn6;
  sockaddr_in6 = packed record
		sin6_family   : Word;
		sin6_port     : Word;
		sin6_flowinfo : Word32;
		sin6_addr     : TIn6Addr;
		sin6_scope_id : Word32;
  end;
  TSockAddrIn6 = sockaddr_in6;

  PSockAddr = ^TSockAddr;
  sockaddr = packed record
      case sa_family : Word of
        AF_INET : (
          sin_port      : Word;
          sin_addr      : TInAddr;
          sin_zero      : array[0..7] of Byte );
        AF_INET6 : (
          sin6_port     : Word;
          sin6_flowinfo : Word32;
          sin6_addr     : TIn6Addr;
          sin6_scope_id : Word32; );
  end;
  TSockAddr = sockaddr;
  TSockAddrArray = Array of TSockAddr;

type
  PAddrInfo = ^TAddrInfo;
  addrinfo = record
                ai_flags     : Int32;
                ai_family    : Int32;
                ai_socktype  : Int32;
                ai_protocol  : Int32;
                ai_addrlen   : Int32;
                ai_addr      : PSockAddr;
                ai_canonname : PAnsiChar;
                ai_next      : PAddrInfo;
              end;
  TAddrInfo = addrinfo;

const
  // ai_flags constants
  AI_PASSIVE     = $0001;
  AI_CANONNAME   = $0002;
  AI_NUMERICHOST = $0004;

type
  PHostEnt = ^THostEnt;
  hostent = record
    h_name     : PAnsiChar;
    h_aliases  : ^PAnsiChar;
    h_addrtype : Int32;
    h_length   : Int32;
    case Byte of
      0 : (h_addr_list : ^PAnsiChar);
      1 : (h_addr      : ^PAnsiChar);
  end;
  THostEnt = hostent;

  PNetEnt = ^TNetEnt;
  netent = packed record
    n_name     : PAnsiChar;
    n_aliases  : ^PAnsiChar;
    n_addrtype : Int32;
    n_net      : Word32;
  end;
  TNetEnt = netent;

  PServEnt = ^TServEnt;
  servent = record
    s_name    : PAnsiChar;
    s_aliases : ^PAnsiChar;
    s_port    : Word;
    s_proto   : PAnsiChar;
  end;
  TServEnt = servent;

  PProtoEnt = ^TProtoEnt;
  protoent = record
    p_name    : PAnsiChar;
    p_aliases : ^PAnsiChar;
    p_proto   : SmallInt;
  end;
  TProtoEnt = protoent;

type
  TPollfd = record
              fd      : TSocket;
              events  : Int16;
              revents : Int16;
             end;
  PPollfd = ^TPollfd;

const
  POLLIN   = $0001;
  POLLPRI  = $0002;
  POLLOUT  = $0004;
  POLLERR  = $0008;
  POLLHUP  = $0010;
  POLLNVAL = $0020;



{                                                                              }
{ Other structures and constants                                               }
{                                                                              }
type
  PTimeVal = ^TTimeVal;
  timeval = record
    {$IFDEF CPU_X86_64}
    tv_sec  : Int64;
    tv_usec : Int64;
    {$ELSE}
    tv_sec  : Int32;
    tv_usec : Int32;
    {$ENDIF}
  end;
  TTimeVal = timeval;

const
   NI_MAXHOST = 1025;
   NI_MAXSERV = 32;

   NI_NOFQDN      = 1;
   NI_NUMERICHOST = 2;
   NI_NAMERQD     = 4;
   NI_NUMERICSERV = 8;
   NI_DGRAM       = 16;

   IP_MULTICAST_TTL = 33;



{                                                                              }
{ UnixSock error code constants                                                }
{                                                                              }
const
  EINTR           = ESysEINTR;
  EBADF           = ESysEBADF;
  EACCES          = ESysEACCES;
  EFAULT          = ESysEFAULT;
  EINVAL          = ESysEINVAL;
  EMFILE          = ESysEMFILE;
  EWOULDBLOCK     = ESysEWOULDBLOCK;
  EINPROGRESS     = ESysEINPROGRESS;
  EALREADY        = ESysEALREADY;
  ENOTSOCK        = ESysENOTSOCK;
  EDESTADDRREQ    = ESysEDESTADDRREQ;
  EMSGSIZE        = ESysEMSGSIZE;
  EPROTOTYPE      = ESysEPROTOTYPE;
  ENOPROTOOPT     = ESysENOPROTOOPT;
  EPROTONOSUPPORT = ESysEPROTONOSUPPORT;
  ESOCKTNOSUPPORT = ESysESOCKTNOSUPPORT;
  EOPNOTSUPP      = ESysEOPNOTSUPP;
  EPFNOSUPPORT    = ESysEPFNOSUPPORT;
  EAFNOSUPPORT    = ESysEAFNOSUPPORT;
  EADDRINUSE      = ESysEADDRINUSE;
  EADDRNOTAVAIL   = ESysEADDRNOTAVAIL;
  ENETDOWN        = ESysENETDOWN;
  ENETUNREACH     = ESysENETUNREACH;
  ENETRESET       = ESysENETRESET;
  ECONNABORTED    = ESysECONNABORTED;
  ECONNRESET      = ESysECONNRESET;
  ENOBUFS	        = ESysENOBUFS;
  EISCONN         = ESysEISCONN;
  ENOTCONN        = ESysENOTCONN;
  ESHUTDOWN       = ESysESHUTDOWN;
  ETOOMANYREFS    = ESysETOOMANYREFS;
  ETIMEDOUT       = ESysETIMEDOUT;
  ECONNREFUSED    = ESysECONNREFUSED;
  ENAMETOOLONG    = ESysENAMETOOLONG;
  EHOSTDOWN       = ESysEHOSTDOWN;
  EHOSTUNREACH    = ESysEHOSTUNREACH;



{                                                                              }
{ UnixSock errors                                                              }
{                                                                              }
type
  EUnixSock = class(Exception)
  private
    FErrorCode : Integer;
  public
    constructor Create(const Msg: String; const ErrorCode: Integer = -1);
    property ErrorCode: Integer read FErrorCode;
  end;

function  UnixSockErrorMessage(const ErrorCode: Integer): String;



{                                                                              }
{ Socket library functions                                                     }
{                                                                              }
type
  TSockLen = Int32;

{ Berkeley socket interface                                                    }
function  Accept(const S: TSocket; const Addr: PSockAddr; var AddrLen: TSockLen): TSocket;
function  Bind(const S: TSocket; const Name: TSockAddr; const NameLen: Integer): Integer;
function  CloseSocket(const S: TSocket): Integer;
function  Connect(const S: TSocket; const Name: PSockAddr; const NameLen: Integer): Integer;
procedure FreeAddrInfo(const AddrInfo: PAddrInfo);
function  GetAddrInfo(const NodeName: PAnsiChar; const ServName: PAnsiChar;
          const Hints: PAddrInfo; var AddrInfo: PAddrInfo): Integer;
function  GetHostByAddr(const Addr: Pointer; const Len: Integer; const AF: Integer): PHostEnt;
function  GetHostByName(const Name: PAnsiChar): PHostEnt;
function  GetHostName(const Name: PAnsiChar; const Len: Integer): Integer;
function  GetNameInfo(const Addr: PSockAddr; const NameLen: Integer;
          const Host: PAnsiChar; const HostLen: Word32;
          const Serv: PAnsiChar; const ServLen: Word32; const Flags: Integer): Integer;
function  GetPeerName(const S: TSocket; var Name: TSockAddr; var NameLen: Integer): Integer;
function  GetProtoByName(const Name: PAnsiChar): PProtoEnt;
function  GetProtoByNumber(const Proto: Integer): PProtoEnt;
function  GetServByName(const Name, Proto: PAnsiChar): PServEnt;
function  GetServByPort(const Port: Integer; const Proto: PAnsiChar): PServEnt;
function  GetSockName(const S: TSocket; var Name: TSockAddr; var NameLen: Integer): Integer;
function  GetSockOpt(const S: TSocket; const Level, OptName: Integer;
          const OptVal: Pointer; var OptLen: Integer): Integer;
function  htons(const HostShort: Word): Word;
function  htonl(const HostLong: Word32): Word32;
function  inet_ntoa(const InAddr: TInAddr): PAnsiChar;
function  inet_addr(const P: PAnsiChar): Word32;
function  IoctlSocket(const S: TSocket; const Cmd: Word32; var Arg: Word32): Integer;
function  Listen(const S: TSocket; const Backlog: Integer): Integer;
function  ntohs(const NetShort: Word): Word;
function  ntohl(const NetLong: Word32): Word32;
function  Recv(const S: TSocket; var Buf; const Len, Flags: Integer): Integer;
function  RecvFrom(const S: TSocket; var Buf; const Len, Flags: Integer;
          var From: TSockAddr; var FromLen: Integer): Integer;
function  Poll(const fds: Pointer; const nfds: Integer; const timeout: Integer): Integer;
function  Select(const nfds: Word32; const ReadFDS, WriteFDS, ExceptFDS: PFDSet;
          const TimeOut: PTimeVal): Integer;
function  Send(const S: TSocket; const Buf; const Len, Flags: Integer): Integer;
function  SendTo(const S: TSocket; const Buf; const Len, Flags: Integer;
          const AddrTo: PSockAddr; const ToLen: Integer): Integer;
function  SetSockOpt(const S: TSocket; const Level, OptName: Integer;
          const OptVal: Pointer; const OptLen: Integer): Integer;
function  Shutdown(const S: TSocket; const How: Integer): Integer;
function  Socket(const AF, Struct, Protocol: Integer): TSocket;

{ Socket helpers                                                               }
function  SockGetLastError: Integer;
function  SockAvailableToRecv(const S: TSocket): Integer;
procedure SetSockBlocking(const S: TSocket; const Block: Boolean);



implementation

uses
  SyncObjs;



{                                                                              }
{ Socket library lock                                                          }
{                                                                              }
var
  SocketLibLock : TCriticalSection = nil;

procedure InitializeLibLock;
begin
  SocketLibLock := TCriticalSection.Create;
end;

procedure FinalizeLibLock;
begin
  FreeAndNil(SocketLibLock);
end;

procedure LibLock;
begin
  if Assigned(SocketLibLock) then
    SocketLibLock.Acquire;
end;

procedure LibUnlock;
begin
  if Assigned(SocketLibLock) then
    SocketLibLock.Release;
end;



{                                                                              }
{ Unix socket types                                                            }
{                                                                              }
function FDMASK(const d: Int32): TFD_MASK;
begin
  Result := 1 shl (d mod NFDBITS);
end;

function FD_ISSET(fd: Int32; const fdset: TFDSet): Boolean;
begin
  Result := (fdset.fds_bits[fd div NFDBITS] and FDMASK(fd)) <> 0;
end;

procedure FD_SET(const fd: Int32; var fdset: TFDSet);
var I : Integer;
begin
  I := fd div NFDBITS;
  Assert(I < FD_ARRAYSIZE);
  fdset.fds_bits[I] := fdset.fds_bits[I] or FDMASK(fd);
end;

procedure FD_CLR(const fd: Int32; var fdset: TFDSet);
var I : Integer;
begin
  I := fd div NFDBITS;
  Assert(I < FD_ARRAYSIZE);
  fdset.fds_bits[I] := fdset.fds_bits[I] and not FDMASK(fd);
end;

procedure FD_ZERO(out fdset: TFDSet);
var I : Integer;
begin
  for I := 0 to FD_ARRAYSIZE - 1 do
    fdset.fds_bits[I] := 0;
end;

function FD_COUNT(const fdset: TFDSet): Integer;
var C, I, J : Integer;
    F : Int32;
begin
  C := 0;
  for I := 0 to FD_ARRAYSIZE - 1 do
    if fdset.fds_bits[I] <> 0 then
      begin
        F := I * NFDBITS;
        for J := 0 to NFDBITS - 1 do
          begin
            if FD_ISSET(F, FDSet) then
              Inc(C);
            Inc(F);
          end;
      end;
  Result := C;
end;



{                                                                              }
{ Socket library function types                                                }
{                                                                              }
type
  { Unix                                                                       }
  TGetServByNameProc =
      function (name, proto: PAnsiChar): PServEnt; cdecl;
  TGetServByPortProc =
      function (port: Int32; proto: PAnsiChar): PServEnt; cdecl;
  TGetProtoByNameProc =
      function (name: PAnsiChar): PProtoEnt; cdecl;
  TGetProtoByNumberProc =
      function (proto: Int32): PProtoEnt; cdecl;
  TGetHostByNameProc =
      function (name: PAnsiChar): PHostEnt; cdecl;
  TGetHostByAddrProc =
      function (addr: Pointer; len, Struct: Int32): PHostEnt; cdecl;
  TGetHostNameProc =
      function (name: PAnsiChar; len: Int32): Int32; cdecl;
  TSocketProc =
      function (af, Struct, Protocol: Int32): TSocket; cdecl;
  TShutdownProc =
      function (s: TSocket; how: Int32): Int32; cdecl;
  TSetSockOptProc =
      function (s: TSocket; level, optname: Int32; optval: PAnsiChar;
                optlen: Int32): Int32; cdecl;
  TGetSockOptProc =
      function (s: TSocket; level, optname: Int32; optval: PAnsiChar;
                var optlen: Int32): Int32; cdecl;
  TSendToProc =
      function (s: TSocket; const Buf; len, flags: Int32;
                const addrto: PSockAddr; tolen: Int32): Int32; cdecl;
  TSendProc =
      function (s: TSocket; const Buf; len, flags: Int32): Int32; cdecl;
  TRecvProc =
      function (s: TSocket; var Buf; len, flags: Int32): Int32; cdecl;
  TRecvFromProc =
      function (s: TSocket; var Buf; len, flags: Int32;
                var from: TSockAddr; var fromlen: Int32): Int32; cdecl;
  TntohsProc =
      function (netshort: Word): Word; cdecl;
  TntohlProc =
      function (netlong: Word32): Word32; cdecl;
  TListenProc =
      function (s: TSocket; backlog: Int32): Int32; cdecl;
  TIoctlSocketProc =
      function (s: TSocket; cmd: Word32; var arg: Word32): Int32; cdecl;
  TInet_ntoaProc =
      function (inaddr: TInAddr): PAnsiChar; cdecl;
  TInet_addrProc =
      function (cp: PAnsiChar): Word32; cdecl;
  ThtonsProc =
      function (hostshort: Word): Word; cdecl;
  ThtonlProc =
      function (hostlong: Word32): Word32; cdecl;
  TGetSockNameProc =
      function (s: TSocket; var name: TSockAddr; var namelen: Int32): Int32; cdecl;
  TGetPeerNameProc =
      function (s: TSocket; var name: TSockAddr; var namelen: Int32): Int32; cdecl;
  TConnectProc =
      function (s: TSocket; name: PSockAddr; namelen: Int32): Int32; cdecl;
  TCloseSocketProc =
      function (s: TSocket): Int32; cdecl;
  TBindProc =
      function (s: TSocket; name: PSockAddr; namelen: Int32): Int32; cdecl;
  TAcceptProc =
      function (s: TSocket; addr: PSockAddr; var addrlen: Int32): TSocket; cdecl;
  TSelectProc =
      function (nfds: Word32; readfds, writefds, exceptfds: PFDSet;
                timeout: PTimeVal): Int32; cdecl;
  TPollProc =
      function (fds: Pointer; nfds: Int32; timeout: Int32): Int32; cdecl;
  TGetAddrInfoProc =
      function (NodeName: PAnsiChar; ServName: PAnsiChar; Hints: PAddrInfo;
                var Addrinfo: PAddrInfo): Int32; cdecl;
  TFreeAddrInfoProc =
      procedure (ai: PAddrInfo); cdecl;
  TGetNameInfoProc =
      function (addr: PSockAddr; namelen: Int32; host: PAnsiChar;
                hostlen: Word32; serv: PAnsiChar; servlen: Word32;
                flags: Int32): Int32; cdecl;
  Terrno_locationProc =
      function: PInt32; cdecl;



{                                                                              }
{ Socket library function variables                                            }
{                                                                              }
var
  { Sockets                                                                    }
  AcceptProc                           : TAcceptProc = nil;
  BindProc                             : TBindProc = nil;
  CloseSocketProc                      : TCloseSocketProc = nil;
  ConnectProc                          : TConnectProc = nil;
  FreeAddrInfoProc                     : TFreeAddrInfoProc = nil;
  GetAddrInfoProc                      : TGetAddrInfoProc = nil;
  GetHostByAddrProc                    : TGetHostByAddrProc = nil;
  GetHostByNameProc                    : TGetHostByNameProc = nil;
  GetHostNameProc                      : TGetHostNameProc = nil;
  GetNameInfoProc                      : TGetNameInfoProc = nil;
  GetPeerNameProc                      : TGetPeerNameProc = nil;
  GetProtoByNameProc                   : TGetProtoByNameProc = nil;
  GetProtoByNumberProc                 : TGetProtoByNumberProc = nil;
  GetServByNameProc                    : TGetServByNameProc = nil;
  GetServByPortProc                    : TGetServByPortProc = nil;
  GetSockNameProc                      : TGetSockNameProc = nil;
  GetSockOptProc                       : TGetSockOptProc = nil;
  htonsProc                            : ThtonsProc = nil;
  htonlProc                            : ThtonlProc = nil;
  inet_ntoaProc                        : TInet_ntoaProc = nil;
  inet_addrProc                        : TInet_addrProc = nil;
  IoctlSocketProc                      : TIoctlSocketProc = nil;
  ListenProc                           : TListenProc = nil;
  ntohsProc                            : TntohsProc = nil;
  ntohlProc                            : TntohlProc = nil;
  RecvProc                             : TRecvProc = nil;
  RecvFromProc                         : TRecvFromProc = nil;
  PollProc                             : TPollProc = nil;
  SelectProc                           : TSelectProc = nil;
  SendProc                             : TSendProc = nil;
  SendToProc                           : TSendToProc = nil;
  SetSockOptProc                       : TSetSockOptProc = nil;
  ShutdownProc                         : TShutdownProc = nil;
  SocketProc                           : TSocketProc = nil;

  { Unix                                                                       }
  errno_locationProc                   : Terrno_locationProc = nil;



{                                                                              }
{ Socket library loading / unloading                                           }
{                                                                              }
type
  TSocketLibraryHandle = TLibHandle;

var
  // System handle to dynamically linked library
  SocketLibraryHandle    : TSocketLibraryHandle = TSocketLibraryHandle(0);
  SocketLibraryFinalized : Boolean = False;  // True = Library finalised, cannot be loaded anymore
  SocketLibraryLoaded    : Integer = 0;      // 0 = Not loaded, 1 = SocketLibraryName1, 2 = SocketLibraryName2

const
  SocketLibraryName1 = 'libc.so.6';
  SocketLibraryName2 = 'libc.so';

procedure LoadSocketLibrary;

  // Use the system to load the dynamically linked socket library file.
  // Returns True on success.
  function LoadLibrary(const LibraryName: AnsiString): Boolean;
  begin
    SocketLibraryHandle := dynlibs.LoadLibrary(LibraryName);
    Result := (SocketLibraryHandle <> 0);
  end;

begin
  // Ignore if already loaded
  if Word32(SocketLibraryHandle) <> 0 then
    exit;
  // Raise an exception if an attempt is made to reload the library after
  // unit has finalized
  if SocketLibraryFinalized then
    raise EUnixSock.Create('Socket library finalized');
  // Load socket library
  if LoadLibrary(SocketLibraryName1) then
    SocketLibraryLoaded := 1
  else if LoadLibrary(SocketLibraryName2) then
    SocketLibraryLoaded := 2
  else
    begin
      // Failure
      SocketLibraryHandle := TSocketLibraryHandle(0);
      SocketLibraryLoaded := 0;
      raise EUnixSock.Create('Failed to load socket library');
    end;
end;

procedure UnloadSocketLibrary;
var H : TSocketLibraryHandle;
begin
  // Ignore if not loaded
  H := SocketLibraryHandle;
  if Word32(H) = 0 then
    exit;
  // Set state unloaded
  SocketLibraryHandle := TSocketLibraryHandle(0);
  SocketLibraryLoaded := 0;
  // Clear function references
  AcceptProc := nil;
  BindProc := nil;
  CloseSocketProc := nil;
  ConnectProc := nil;
  FreeAddrInfoProc := nil;
  GetAddrInfoProc := nil;
  GetHostByAddrProc := nil;
  GetHostByNameProc := nil;
  GetHostNameProc := nil;
  GetNameInfoProc := nil;
  GetPeerNameProc := nil;
  GetProtoByNameProc := nil;
  GetProtoByNumberProc := nil;
  GetServByNameProc := nil;
  GetServByPortProc := nil;
  GetSockNameProc := nil;
  GetSockOptProc := nil;
  htonsProc := nil;
  htonlProc := nil;
  IoctlSocketProc := nil;
  inet_ntoaProc := nil;
  inet_addrProc := nil;
  ListenProc := nil;
  ntohsProc := nil;
  ntohlProc := nil;
  RecvProc := nil;
  RecvFromProc := nil;
  PollProc := nil;
  SelectProc := nil;
  SendProc := nil;
  SendToProc := nil;
  SetSockOptProc := nil;
  ShutdownProc := nil;
  SocketProc := nil;
  errno_locationProc := nil;
  // Unload socket library
  dynlibs.UnloadLibrary(H);
end;

procedure GetSocketProc(const ProcName: AnsiString; var Proc: Pointer);
begin
  LibLock;
  try
    // Check if already linked
    if Assigned(Proc) then
      exit;
    // Load socket library
    if SocketLibraryHandle = 0 then
      LoadSocketLibrary;
    Assert(SocketLibraryHandle <> 0);
    // Get socket procedure
    Proc := dynlibs.GetProcedureAddress(SocketLibraryHandle, PAnsiChar(ProcName));
    // Check success
    if not Assigned(Proc) then
      raise EUnixSock.Create('Failed to link socket library function: ' + ProcName);
  finally
    LibUnlock;
  end;
end;



{                                                                              }
{ Socket library functions                                                     }
{                                                                              }
function Accept(const S: TSocket; const Addr: PSockAddr; var AddrLen: TSockLen): TSocket;
begin
  if not Assigned(AcceptProc) then
    GetSocketProc('accept', @AcceptProc);
  Result := AcceptProc(S, Addr, AddrLen);
end;

function Bind(const S: TSocket; const Name: TSockAddr; const NameLen: Integer): Integer;
begin
  if not Assigned(BindProc) then
    GetSocketProc('bind', @BindProc);
  Result := BindProc(S, @Name, NameLen);
end;

function CloseSocket(const S: TSocket): Integer;
begin
  if not Assigned(CloseSocketProc) then
    GetSocketProc('close', @CloseSocketProc);
  Result := CloseSocketProc(S);
end;

function Connect(const S: TSocket; const Name: PSockAddr; const NameLen: Integer): Integer;
begin
  if not Assigned(ConnectProc) then
    GetSocketProc('connect', @ConnectProc);
  Result := ConnectProc(S, Name, NameLen);
end;

procedure FreeAddrInfo(const AddrInfo: PAddrInfo);
begin
  if not Assigned(FreeAddrInfoProc) then
    GetSocketProc('freeaddrinfo', @FreeAddrInfoProc);
  FreeAddrInfoProc(AddrInfo);
end;

function GetAddrInfo(const NodeName: PAnsiChar; const ServName: PAnsiChar;
    const Hints: PAddrInfo; var AddrInfo: PAddrInfo): Integer;
begin
  if not Assigned(GetAddrInfoProc) then
    GetSocketProc('getaddrinfo', @GetAddrInfoProc);
  Result := GetAddrInfoProc(NodeName, ServName, Hints, AddrInfo);
end;

function GetHostByAddr(const Addr: Pointer; const Len: Integer; const AF: Integer): PHostEnt;
begin
  if not Assigned(GetHostByAddrProc) then
    GetSocketProc('gethostbyaddr', @GetHostByAddrProc);
  Result := GetHostByAddrProc(Addr, Len, AF);
end;

function GetHostByName(const Name: PAnsiChar): PHostEnt;
begin
  if not Assigned(GetHostByNameProc) then
    GetSocketProc('gethostbyname', @GetHostByNameProc);
  Result := GetHostByNameProc(Name);
end;

function GetHostName(const Name: PAnsiChar; const Len: Integer): Integer;
begin
  if not Assigned(GetHostNameProc) then
    GetSocketProc('gethostname', @GetHostNameProc);
  Result := GetHostNameProc(Name, Len);
end;

function GetNameInfo(const Addr: PSockAddr; const NameLen: Integer;
    const Host: PAnsiChar; const HostLen: Word32; const Serv: PAnsiChar;
    const ServLen: Word32; const Flags: Integer): Integer;
begin
  if not Assigned(GetNameInfoProc) then
    GetSocketProc('getnameinfo', @GetNameInfoProc);
  Result := GetNameInfoProc(Addr, NameLen, Host, HostLen, Serv, ServLen, Flags);
end;

function GetPeerName(const S: TSocket; var Name: TSockAddr; var NameLen: Integer): Integer;
begin
  if not Assigned(GetPeerNameProc) then
    GetSocketProc('getpeername', @GetPeerNameProc);
  Result := GetPeerNameProc(S, Name, NameLen);
end;

function GetProtoByName(const Name: PAnsiChar): PProtoEnt;
begin
  if not Assigned(GetProtoByNameProc) then
    GetSocketProc('getprotobyname', @GetProtoByNameProc);
  Result := GetProtoByNameProc(Name);
end;

function GetProtoByNumber(const Proto: Integer): PProtoEnt;
begin
  if not Assigned(GetProtoByNumberProc) then
    GetSocketProc('getprotobynumber', @GetProtoByNumberProc);
  Result := GetProtoByNumberProc(Proto);
end;

function GetServByName(const Name, Proto: PAnsiChar): PServEnt;
begin
  if not Assigned(GetServByNameProc) then
    GetSocketProc('getservbyname', @GetServByNameProc);
  Result := GetServByNameProc(Name, Proto);
end;

function GetServByPort(const Port: Integer; const Proto: PAnsiChar): PServEnt;
begin
  if not Assigned(GetServByPortProc) then
    GetSocketProc('getservbyport', @GetServByPortProc);
  Result := GetServByPortProc(Port, Proto);
end;

function GetSockName(const S: TSocket; var Name: TSockAddr; var NameLen: Integer): Integer;
begin
  if not Assigned(GetSockNameProc) then
    GetSocketProc('getsockname', @GetSockNameProc);
  Result := GetSockNameProc(S, Name, NameLen);
end;

function GetSockOpt(const S: TSocket; const Level, OptName: Integer;
    const OptVal: Pointer; var OptLen: Integer): Integer;
begin
  if not Assigned(GetSockOptProc) then
    GetSocketProc('getsockopt', @GetSockOptProc);
  Result := GetSockOptProc(S, Level, OptName, OptVal, OptLen);
end;

function htons(const HostShort: Word): Word;
begin
  if not Assigned(htonsProc) then
    GetSocketProc('htons', @htonsProc);
  Result := htonsProc(HostShort);
end;

function htonl(const HostLong: Word32): Word32;
begin
  if not Assigned(htonlProc) then
    GetSocketProc('htonl', @htonlProc);
  Result := htonlProc(HostLong);
end;

function inet_ntoa(const InAddr: TInAddr): PAnsiChar;
begin
  if not Assigned(Inet_ntoaProc) then
    GetSocketProc('inet_ntoa', @Inet_ntoaProc);
  Result := inet_ntoaProc(InAddr);
end;

function inet_addr(const P: PAnsiChar): Word32;
begin
  if not Assigned(Inet_addrProc) then
    GetSocketProc('inet_addr', @Inet_addrProc);
  Result := inet_addrProc(P);
end;

function IoctlSocket(const S: TSocket; const Cmd: Word32; var Arg: Word32): Integer;
begin
  if not Assigned(IoctlSocketProc) then
    GetSocketProc('ioctlsocket', @IoctlSocketProc);
  Result := IoctlSocketProc(S, Cmd, Arg);
end;

function Listen(const S: TSocket; const Backlog: Integer): Integer;
begin
  if not Assigned(ListenProc) then
    GetSocketProc('listen', @ListenProc);
  Result := ListenProc(S, Backlog);
end;

function ntohs(const NetShort: Word): Word;
begin
  if not Assigned(ntohsProc) then
    GetSocketProc('ntohs', @ntohsProc);
  Result := ntohsProc(NetShort);
end;

function ntohl(const NetLong: Word32): Word32;
begin
  if not Assigned(ntohlProc) then
    GetSocketProc('ntohl', @ntohlProc);
  Result := ntohlProc(NetLong);
end;

function Recv(const S: TSocket; var Buf; const Len, Flags: Integer): Integer;
begin
  if not Assigned(RecvProc) then
    GetSocketProc('recv', @RecvProc);
  Result := RecvProc(S, Buf, Len, Flags);
end;

function RecvFrom(const S: TSocket; var Buf; const Len, Flags: Integer;
    var From: TSockAddr; var FromLen: Integer): Integer;
begin
  if not Assigned(RecvFromProc) then
    GetSocketProc('recvfrom', @RecvFromProc);
  Result := RecvFromProc(S, Buf, Len, Flags, From, FromLen);
end;

function Poll(const fds: Pointer; const nfds: Integer; const timeout: Integer): Integer;
begin
  if not Assigned(PollProc) then
    GetSocketProc('poll', @PollProc);
  Result := PollProc(fds, nfds, timeout);
end;

function Select(const nfds: Word32; const ReadFDS, WriteFDS, ExceptFDS: PFDSet;
    const TimeOut: PTimeVal): Integer;
begin
  if not Assigned(SelectProc) then
    GetSocketProc('select', @SelectProc);
  Result := SelectProc(nfds, ReadFDS, WriteFDS, ExceptFDS, TimeOut);
end;

function Send(const S: TSocket; const Buf; const Len, Flags: Integer): Integer;
begin
  if not Assigned(SendProc) then
    GetSocketProc('send', @SendProc);
  Result := SendProc(S, Buf, Len, Flags);
end;

function SendTo(const S: TSocket; const Buf; const Len, Flags: Integer;
    const AddrTo: PSockAddr; const ToLen: Integer): Integer;
begin
  if not Assigned(SendToProc) then
    GetSocketProc('sendto', @SendToProc);
  Result := SendToProc(S, Buf, Len, Flags, AddrTo, ToLen);
end;

function SetSockOpt(const S: TSocket; const Level, OptName: Integer;
    const OptVal: Pointer; const OptLen: Integer): Integer;
begin
  if not Assigned(SetSockOptProc) then
    GetSocketProc('setsockopt', @SetSockOptProc);
  Result := SetSockOptProc(S, Level, OptName, OptVal, OptLen);
end;

function Shutdown(const S: TSocket; const How: Integer): Integer;
begin
  if not Assigned(ShutdownProc) then
    GetSocketProc('shutdown', @ShutdownProc);
  Result := ShutdownProc(S, How);
end;

function Socket(const AF, Struct, Protocol: Integer): TSocket;
begin
  if not Assigned(SocketProc) then
    GetSocketProc('socket', @SocketProc);
  Result := SocketProc(AF, Struct, Protocol);
end;

function SockGetLastError: Integer;
var P : PInteger;
begin
  if not Assigned(errno_locationProc) then
    GetSocketProc('__errno_location', @errno_locationProc);
  P := errno_locationProc;
  Result := P^;
end;

function SockAvailableToRecv(const S: TSocket): Integer;
var L : Word32;
begin
  if FpIoctl(S, FIONREAD, @L) <> 0 then
    Result := 0
  else
    Result := L;
end;

procedure SetSockBlocking(const S: TSocket; const Block: Boolean);
var A : Int32;
begin
  if S = INVALID_SOCKET then
    raise EUnixSock.Create('Invalid socket handle');
  // Set non-blocking flag on file handle
  A := FpFcntl(S, F_GETFL, 0);
  if A < 0 then
    raise EUnixSock.Create('Failed to set blocking mode', SockGetLastError);
  if Block then
    A := A and not O_NONBLOCK
  else
    A := A or O_NONBLOCK;
  if FpFcntl(S, F_SETFL, A) < 0 then
    raise EUnixSock.Create('Failed to set blocking mode', SockGetLastError);
end;



{                                                                              }
{ UnixSock errors                                                              }
{                                                                              }
constructor EUnixSock.Create(const Msg: String; const ErrorCode: Integer);
begin
  inherited Create(Msg);
  FErrorCode := ErrorCode;
end;

function UnixSockErrorMessage(const ErrorCode: Integer): String;
begin
  case ErrorCode of
    0, -1               : Result := '';
    EINTR               : Result := 'Operation interrupted';
    EBADF               : Result := 'Invalid handle';
    EACCES              : Result := 'Permission denied';
    EFAULT              : Result := 'Invalid pointer';
    EINVAL              : Result := 'Invalid argument';
    EMFILE              : Result := 'Too many open handles';
    EWOULDBLOCK         : Result := 'Blocking operation';
    EINPROGRESS         : Result := 'Operation in progress';
    EALREADY            : Result := 'Operation already performed';
    ENOTSOCK            : Result := 'Socket operation on non-socket or not connected';
    EDESTADDRREQ        : Result := 'Destination address required';
    EMSGSIZE            : Result := 'Invalid message size';
    EPROTOTYPE          : Result := 'Invalid protocol type';
    ENOPROTOOPT         : Result := 'Protocol not available';
    EPROTONOSUPPORT     : Result := 'Protocol not supported';
    ESOCKTNOSUPPORT     : Result := 'Socket type not supported';
    EOPNOTSUPP          : Result := 'Socket operation not supported';
    EPFNOSUPPORT        : Result := 'Protocol family not supported';
    EAFNOSUPPORT        : Result := 'Address family not supported by protocol family';
    EADDRINUSE          : Result := 'Address in use';
    EADDRNOTAVAIL       : Result := 'Address not available';
    ENETDOWN	          : Result := 'The network is down';
    ENETUNREACH         : Result := 'The network is unreachable';
    ENETRESET           : Result := 'Network connection reset';
    ECONNABORTED        : Result := 'Connection aborted';
    ECONNRESET          : Result := 'Connection reset by peer';
    ENOBUFS	            : Result := 'No buffer space available';
    EISCONN             : Result := 'Socket connected';
    ENOTCONN            : Result := 'Socket not connected';
    ESHUTDOWN           : Result := 'Socket shutdown';
    ETOOMANYREFS        : Result := 'Too many references';
    ETIMEDOUT           : Result := 'Connection timed out';
    ECONNREFUSED        : Result := 'Connection refused';
    ENAMETOOLONG        : Result := 'Name too long';
    EHOSTDOWN           : Result := 'Host is unavailable';
    EHOSTUNREACH        : Result := 'Host is unreachable';
  else
    Result := 'System error #' + IntToStr(ErrorCode);
  end;
end;



end.

