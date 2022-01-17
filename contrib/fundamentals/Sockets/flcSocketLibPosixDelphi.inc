{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals 5.00                                        }
{   File name:        flcSocketLibPosixDelphi.inc                              }
{   File version:     5.03                                                     }
{   Description:      Delphi Posix socket library                              }
{                                                                              }
{   Copyright:        Copyright © 2018-2020, David J Butler                    }
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
{   2018/07/12  5.01  Initial implementation.                                  }
{   2018/07/16  5.02  Fixes.                                                   }
{   2018/09/09  5.03  Poll function.                                           }
{                                                                              }
{ Supported compilers:                                                         }
{                                                                              }
{   Delphi 10.2-10.4 Linux64            5.03  2020/06/02                       }
{   Delphi 10.2-10.4 iOS32/64           5.03  2020/06/02                       }
{   Delphi 10.2-10.4 OSX32/64           5.03  2020/06/02                       }
{   Delphi 10.2-10.4 Android32/64       5.03  2020/06/02                       }
{                                                                              }
{******************************************************************************}

{$WARN SYMBOL_PLATFORM OFF}
{$WARN UNIT_PLATFORM OFF}

interface

uses
  { System }
  Posix.Errno,
  Posix.SysTime,
  Posix.SysSelect,
  Posix.SysSocket,
  Posix.NetinetIn,
  Posix.NetDB,

  { Fundamentals }
  flcStdTypes;



type
  TTimeVal = timeval;
  PTimeVal = Posix.SysTime.Ptimeval;

type
  TSocket = Integer;

const
  INVALID_SOCKET = TSocket(not 0);

type
  TInAddr = in_addr;
  PInAddr = Pin_addr;
  TIn6Addr = in6_addr;
  PIn6Addr = Pin6_addr;

const
  INADDR_NONE = Posix.NetinetIn.INADDR_NONE;
  INADDR_ANY = Posix.NetinetIn.INADDR_ANY;
  INADDR_BROADCAST = Posix.NetinetIn.INADDR_BROADCAST;
  INADDR_LOOPBACK = Posix.NetinetIn.INADDR_LOOPBACK;

type
  TSockAddr = sockaddr_storage;
  PTSockAddr = ^TSockAddr;
  PSockAddr = Posix.SysSocket.psockaddr;

  TSockAddrIn = sockaddr_in;
  PSockAddrIn = ^TSockAddrIn;

  TSockAddrIn6 = sockaddr_in6;
  PSockAddrIn6 = ^TSockAddrIn6;

  TFDSet = fd_set;

  THostEnt = hostent;
  PHostEnt = Posix.NetDB.phostent;

  TServEnt = servent;
  PServEnt = Posix.NetDB.Pservent;

  TProtoEnt = protoent;
  PProtoEnt = Posix.NetDB.Pprotoent;

  TAddrInfo = addrinfo;
  PAddrInfo = Posix.NetDB.paddrinfo;

const
  AF_INET = Posix.SysSocket.AF_INET;
  AF_INET6 = Posix.SysSocket.AF_INET6;
  AF_UNSPEC = Posix.SysSocket.AF_UNSPEC;

  IPPROTO_IP = Posix.NetinetIn.IPPROTO_IP;
  IPPROTO_ICMP = Posix.NetinetIn.IPPROTO_ICMP;
  IPPROTO_TCP = Posix.NetinetIn.IPPROTO_TCP;
  IPPROTO_UDP = Posix.NetinetIn.IPPROTO_UDP;
  IPPROTO_RAW = Posix.NetinetIn.IPPROTO_RAW;

  SOCK_STREAM = Posix.SysSocket.SOCK_STREAM;
  SOCK_DGRAM = Posix.SysSocket.SOCK_DGRAM;
  SOCK_RAW = Posix.SysSocket.SOCK_RAW;

  MSG_OOB = Posix.SysSocket.MSG_OOB;
  MSG_PEEK = Posix.SysSocket.MSG_PEEK;

  SOL_SOCKET = Posix.SysSocket.SOL_SOCKET;
  {$IFNDEF ANDROID}
  {$IFNDEF OSX}
  {$IFNDEF IOS}
  SOL_RAW = Posix.SysSocket.SOL_RAW;
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}

  SO_RCVTIMEO = Posix.SysSocket.SO_RCVTIMEO;
  SO_SNDTIMEO = Posix.SysSocket.SO_SNDTIMEO;
  SO_BROADCAST = Posix.SysSocket.SO_BROADCAST;
  SO_LINGER = Posix.SysSocket.SO_LINGER;
  SO_RCVBUF = Posix.SysSocket.SO_RCVBUF;
  SO_SNDBUF = Posix.SysSocket.SO_SNDBUF;
  TCP_NODELAY = 1; //Posix.SysSocket.TCP_NODELAY;

type
  TFD_MASK = fd_mask;
  TFD_Set = fd_set;
  PFDSet = Pfd_set;

const
  FD_SETSIZE = Posix.SysSelect.FD_SETSIZE;
  NFDBITS = Posix.SysSelect.NFDBITS;

function  FDMASK(const d: Int32): TFD_MASK;
function  FD_ISSET(fd: Int32; const fdset: TFDSet): Boolean;
procedure FD_SET(const fd: Int32; var fdset: TFDSet);
procedure FD_CLR(const fd: Int32; var fdset: TFDSet);
procedure FD_ZERO(out fdset: TFDSet);
function  FD_COUNT(const fdset: TFDSet): Integer;

const
  NI_MAXHOST = Posix.NetDB.NI_MAXHOST;
  NI_MAXSERV = Posix.NetDB.NI_MAXSERV;
  NI_NUMERICSERV = Posix.NetDB.NI_NUMERICSERV;
  AI_NUMERICHOST = Posix.NetDB.AI_NUMERICHOST;

{$IFDEF OS_ANDROID32}
{$IFDEF DELPHI10_4_UP}
  {$DEFINE SockLen_Int32}
{$ENDIF}
{$ENDIF}

{$IFDEF SockLen_Int32}
type
  TSockLen = Int32;
{$ELSE}
type
  TSockLen = FixedUInt;
{$ENDIF}

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

function  Accept(const socket: TSocket; const address: PSockAddr; var address_len: TSockLen): Integer;
function  Bind(const socket: TSocket; const address: TSockAddr; const address_len: TSockLen): Integer;
function  CloseSocket(const socket: TSocket): Integer;
function  Connect(const socket: TSocket; const address: PSockAddr; const address_len: TSockLen): Integer;
procedure FreeAddrInfo(const ai: PAddrInfo);
function  GetAddrInfo(const HostName: Pointer; const ServName: Pointer;
          const Hints: PAddrInfo; var AddrInfo: PAddrInfo): Integer;
function  GetHostByAddr(const Addr: Pointer; const Len: Integer;
          const AF: Integer): PHostEnt;
function  GetHostByName(const Name: Pointer): PHostEnt;
function  GetHostName(const Name: Pointer; const Len: Integer): Integer;
function  GetNameInfo(const Addr: PSockAddr; const NameLen: Integer;
          const Host: Pointer; const HostLen: Word32;
          const Serv: Pointer; const ServLen: Word32; const Flags: Integer): Integer;
function  GetProtoByName(const Name: Pointer): PProtoEnt;
function  GetProtoByNumber(const Proto: Integer): PProtoEnt;
function  GetPeerName(const S: TSocket; var Name: TSockAddr; var NameLen: TSockLen): Integer;
function  GetServByName(const Name, Proto: Pointer): PServEnt;
function  GetServByPort(const Port: Integer; const Proto: Pointer): PServEnt;
function  GetSockName(const S: TSocket; var Name: TSockAddr; var NameLen: TSockLen): Integer;
function  GetSockOpt(const S: TSocket; const Level, OptName: Integer;
          const OptVal: Pointer; var OptLen: TSockLen): Integer;
function  htons(const HostShort: Word): Word;
function  htonl(const HostLong: Word32): Word32;
function  inet_ntoa(const InAddr: TInAddr): Pointer;
function  inet_addr(const P: Pointer): Word32;
function  Listen(const S: TSocket; const Backlog: Integer): Integer;
function  ntohs(const NetShort: Word): Word;
function  ntohl(const NetLong: Word32): Word32;
function  Poll(fds: Pointer; nfds: Integer; timeout: Integer): Integer;
function  Recv(const S: TSocket; var Buf; const Len, Flags: Integer): Integer;
function  RecvFrom(const S: TSocket; var Buf; const Len, Flags: Integer;
          var From: TSockAddr; var FromLen: Integer): Integer;
function  Select(const nfds: Word32; const ReadFDS, WriteFDS, ExceptFDS: PFDSet;
          const TimeOut: PTimeVal): Integer;
function  Send(const S: TSocket; const Buf; const Len, Flags: Integer): Integer;
function  SendTo(const S: TSocket; const Buf; const Len, Flags: Integer;
          const AddrTo: PSockAddr; const ToLen: Integer): Integer;
function  SetSockOpt(const S: TSocket; const Level, OptName: Integer;
          const OptVal: Pointer; const OptLen: Integer): Integer;
function  Shutdown(const S: TSocket; const How: Integer): Integer;
function  Socket(const AF, Struct, Protocol: Integer): TSocket;

const
  EINTR = Posix.Errno.EINTR;
  EBADF = Posix.Errno.EBADF;
  EACCES = Posix.Errno.EACCES;
  EFAULT = Posix.Errno.EFAULT;
  EINVAL = Posix.Errno.EINVAL;
  EMFILE = Posix.Errno.EMFILE;
  EWOULDBLOCK = Posix.Errno.EWOULDBLOCK;
  EINPROGRESS = Posix.Errno.EINPROGRESS;
  EALREADY = Posix.Errno.EALREADY;
  ENOTSOCK = Posix.Errno.ENOTSOCK;
  EDESTADDRREQ = Posix.Errno.EDESTADDRREQ;
  EMSGSIZE = Posix.Errno.EMSGSIZE;
  EPROTOTYPE = Posix.Errno.EPROTOTYPE;
  ENOPROTOOPT = Posix.Errno.ENOPROTOOPT;
  EPROTONOSUPPORT = Posix.Errno.EPROTONOSUPPORT;
  ESOCKTNOSUPPORT = Posix.Errno.ESOCKTNOSUPPORT;
  EOPNOTSUPP = Posix.Errno.EOPNOTSUPP;
  EPFNOSUPPORT = Posix.Errno.EPFNOSUPPORT;
  EAFNOSUPPORT = Posix.Errno.EAFNOSUPPORT;
  EADDRINUSE = Posix.Errno.EADDRINUSE;
  EADDRNOTAVAIL = Posix.Errno.EADDRNOTAVAIL;
  ENETDOWN = Posix.Errno.ENETDOWN;
  ENETUNREACH = Posix.Errno.ENETUNREACH;
  ENETRESET = Posix.Errno.ENETRESET;
  ECONNABORTED = Posix.Errno.ECONNABORTED;
  ECONNRESET = Posix.Errno.ECONNRESET;
  ENOBUFS	= Posix.Errno.ENOBUFS;
  EISCONN = Posix.Errno.EISCONN;
  ENOTCONN = Posix.Errno.ENOTCONN;
  ESHUTDOWN = Posix.Errno.ESHUTDOWN;
  ETOOMANYREFS = Posix.Errno.ETOOMANYREFS;
  ETIMEDOUT = Posix.Errno.ETIMEDOUT;
  ECONNREFUSED = Posix.Errno.ECONNREFUSED;
  ENAMETOOLONG = Posix.Errno.ENAMETOOLONG;
  EHOSTDOWN = Posix.Errno.EHOSTDOWN;
  EHOSTUNREACH = Posix.Errno.EHOSTUNREACH;

function  GetLastSocketError: Integer;
procedure SetSockBlocking(const S: TSocket; const Blocking: Boolean);
function  SockAvailableToRecv(const S: TSocket): Integer;



implementation

uses
  Posix.Unistd,
  Posix.ArpaInet,
  Posix.Fcntl,
  Posix.StrOpts,
  Posix.Base;



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
  Assert(I < FD_SETSIZE);
  fdset.fds_bits[I] := fdset.fds_bits[I] or FDMASK(fd);
end;

procedure FD_CLR(const fd: Int32; var fdset: TFDSet);
var I : Integer;
begin
  I := fd div NFDBITS;
  Assert(I < FD_SETSIZE);
  fdset.fds_bits[I] := fdset.fds_bits[I] and not FDMASK(fd);
end;

procedure FD_ZERO(out fdset: TFDSet);
var I : Integer;
begin
  for I := 0 to FD_SETSIZE div NFDBITS - 1 do
    fdset.fds_bits[I] := 0;
end;

function FD_COUNT(const fdset: TFDSet): Integer;
var C, I, J : Integer;
    F : Int32;
begin
  C := 0;
  for I := 0 to FD_SETSIZE div NFDBITS - 1 do
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

function Accept(const socket: TSocket; const address: PSockAddr; var address_len: TSockLen): Integer;
begin
  Result := Posix.SysSocket.accept(socket, address^, address_len);
end;

function Bind(const socket: TSocket; const address: TSockAddr; const address_len: TSockLen): Integer;
begin
  Result := Posix.SysSocket.bind(socket, Psockaddr(@address)^, address_len);
end;

function CloseSocket(const socket: TSocket): Integer;
begin
  Result := __close(socket);
end;

function Connect(const socket: TSocket; const address: PSockAddr; const address_len: TSockLen): Integer;
begin
  Result := Posix.SysSocket.connect(socket, address^, address_len);
end;

procedure FreeAddrInfo(const ai: PAddrInfo);
begin
  Posix.NetDB.freeaddrinfo(ai^);
end;

function GetAddrInfo(const HostName: Pointer; const ServName: Pointer;
         const Hints: PAddrInfo; var AddrInfo: PAddrInfo): Integer;
begin
  Result := Posix.NetDB.getaddrinfo(
      MarshaledAString(HostName),
      MarshaledAString(ServName),
      Hints^,
      AddrInfo);
end;

function GetHostByAddr(const Addr: Pointer; const Len: Integer; const AF: Integer): PHostEnt;
begin
  Result := Posix.NetDB.gethostbyaddr(Addr^, Len, AF);
end;

function GetHostByName(const Name: Pointer): PHostEnt;
begin
  Result := Posix.NetDB.gethostbyname(MarshaledAString(Name));
end;

function GetHostName(const Name: Pointer; const Len: Integer): Integer;
begin
  Result := Posix.Unistd.gethostname(MarshaledAString(Name), Len);
end;

function GetNameInfo(const Addr: PSockAddr; const NameLen: Integer;
         const Host: Pointer; const HostLen: Word32;
         const Serv: Pointer; const ServLen: Word32; const Flags: Integer): Integer;
begin
  Result := Posix.NetDB.getnameinfo(Addr^, NameLen,
      MarshaledAString(Host), HostLen,
      MarshaledAString(Serv), ServLen, Flags);
end;

function GetProtoByName(const Name: Pointer): PProtoEnt;
begin
  Result := Posix.NetDB.getprotobyname(Name);
end;

function GetProtoByNumber(const Proto: Integer): PProtoEnt;
begin
  Result := Posix.NetDB.getprotobynumber(Proto);
end;

function GetPeerName(const S: TSocket; var Name: TSockAddr; var NameLen: TSockLen): Integer;
begin
  Result := Posix.SysSocket.getpeername(S, Psockaddr(@Name)^, NameLen);
end;

function GetServByName(const Name, Proto: Pointer): PServEnt;
begin
  Result := Posix.NetDB.getservbyname(Name, Proto);
end;

function GetServByPort(const Port: Integer; const Proto: Pointer): PServEnt;
begin
  Result := Posix.NetDB.getservbyport(Port, Proto);
end;

function GetSockName(const S: TSocket; var Name: TSockAddr; var NameLen: TSockLen): Integer;
begin
  Result := Posix.SysSocket.getsockname(S, Psockaddr(@Name)^, NameLen);
end;

function GetSockOpt(const S: TSocket; const Level, OptName: Integer;
         const OptVal: Pointer; var OptLen: TSockLen): Integer;
begin
  Result := Posix.SysSocket.getsockopt(S, Level, OptName,
      OptVal^, OptLen);
end;

function htons(const HostShort: Word): Word;
begin
  Result := Posix.ArpaInet.htons(HostShort);
end;

function htonl(const HostLong: Word32): Word32;
begin
  Result := Posix.ArpaInet.htonl(HostLong);
end;

function inet_ntoa(const InAddr: TInAddr): Pointer;
begin
  Result := Posix.ArpaInet.inet_ntoa(InAddr);
end;

function inet_addr(const P: Pointer): Word32;
begin
  Result := Posix.ArpaInet.inet_addr(P);
end;

function Listen(const S: TSocket; const Backlog: Integer): Integer;
begin
  Result := Posix.SysSocket.listen(S, Backlog);
end;

function ntohs(const NetShort: Word): Word;
begin
  Result := Posix.ArpaInet.ntohs(NetShort);
end;

function ntohl(const NetLong: Word32): Word32;
begin
  Result := Posix.ArpaInet.ntohl(NetLong);
end;

function libcpoll(fds: Pointer; nfds: Int32; timeout: Int32): Int32; cdecl;
  external libc name _PU + 'poll';
  {$EXTERNALSYM libcpoll}

function Poll(fds: Pointer; nfds: Integer; timeout: Integer): Integer;
begin
  Result := libcpoll(fds, nfds, timeout);
end;

function Recv(const S: TSocket; var Buf; const Len, Flags: Integer): Integer;
begin
  Result := Posix.SysSocket.recv(S, Buf, Len, Flags);
end;

function RecvFrom(const S: TSocket; var Buf; const Len, Flags: Integer;
         var From: TSockAddr; var FromLen: Integer): Integer;
var
  FromLenT : TSockLen;
begin
  FromLenT := FromLen;
  Result := Posix.SysSocket.recvfrom(S, Buf, Len, Flags, Psockaddr(@From)^, FromLenT);
  FromLen := FromLenT;
end;

function Select(const nfds: Word32; const ReadFDS, WriteFDS, ExceptFDS: PFDSet;
         const TimeOut: PTimeVal): Integer;
begin
  Result := Posix.SysSelect.select(nfds, ReadFDS, WriteFDS, ExceptFDS, TimeOut);
end;

function Send(const S: TSocket; const Buf; const Len, Flags: Integer): Integer;
begin
  Result := Posix.SysSocket.send(S, Buf, Len, Flags);
end;

function SendTo(const S: TSocket; const Buf; const Len, Flags: Integer;
         const AddrTo: PSockAddr; const ToLen: Integer): Integer;
begin
  Result := Posix.SysSocket.sendto(S, Buf, Len, Flags, AddrTo^, ToLen);
end;

function SetSockOpt(const S: TSocket; const Level, OptName: Integer;
         const OptVal: Pointer; const OptLen: Integer): Integer;
begin
  Result := Posix.SysSocket.setsockopt(S, Level, OptName, OptVal^, OptLen);
end;

function Shutdown(const S: TSocket; const How: Integer): Integer;
begin
  Result := Posix.SysSocket.shutdown(S, How);
end;

function Socket(const AF, Struct, Protocol: Integer): TSocket;
begin
  Result := Posix.SysSocket.socket(AF, Struct, Protocol);
end;

function GetLastSocketError: Integer;
begin
  Result := errno;
end;

procedure SetSockBlocking(const S: TSocket; const Blocking: Boolean);
var
  Flags : Integer;
begin
  Flags := Posix.Fcntl.fcntl(Integer(S), F_GETFL, 0);
  if Blocking then
    Flags := Flags and not O_NONBLOCK
  else
    Flags := Flags or O_NONBLOCK;
  Posix.Fcntl.fcntl(Integer(S), F_SETFL, Flags);
end;

{$IFDEF ANDROID}
function SockAvailableToRecv(const S: TSocket): Integer;
begin
  Result := 0;
end;
{$ELSE}{$IFDEF OSX}
function SockAvailableToRecv(const S: TSocket): Integer;
begin
  Result := 0;
end;
{$ELSE}{$IFDEF IOS}
function SockAvailableToRecv(const S: TSocket): Integer;
begin
  Result := 0;
end;
{$ELSE}
function SockAvailableToRecv(const S: TSocket): Integer;
var L : Word32;
begin
  if Ioctl(S, I_NREAD, @L) <> 0 then
    Result := 0
  else
    Result := L;
end;
{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}



end.

