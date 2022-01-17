{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals 5.00                                        }
{   File name:        flcSocketLibWindows.inc                                  }
{   File version:     5.13                                                     }
{   Description:      WinSock API                                              }
{                                                                              }
{   Copyright:        Copyright (c) 2001-2020, David J Butler                  }
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
{   2001/12/11  0.01  Initial version.                                         }
{   2002/07/01  3.02  Revised for Fundamentals 3.                              }
{   2004/04/01  3.03  Change to dynamically load Winsock library.              }
{   2005/07/14  4.04  Compilable with FreePascal 2 Win32 i386.                 }
{   2005/12/10  4.05  Revised for Fundamentals 4.                              }
{   2006/12/04  4.06  Improved Winsock 2 support.                              }
{   2006/12/14  4.07  IP6 support.                                             }
{   2010/06/28  4.08  Revisions for FreePascal 2.4.0.                          }
{   2010/07/21  4.09  Moved to cWinSock unit.                                  }
{   2011/09/27  4.10  Added GetAddrInfoW, FreeAddrInfoW.                       }
{   2016/01/09  5.11  Revised for Fundamentals 5.                              }
{   2018/07/11  5.12  Word32 type changes.                                     }
{   2018/09/09  5.13  WSAPoll function.                                        }
{                                                                              }
{ Supported compilers:                                                         }
{                                                                              }
{   Delphi 2010-10.4 Win32/Win64        5.13  2020/06/02                       }
{   FreePascal 3.0.4 Win64              5.13  2020/06/02                       }
{                                                                              }
{ References:                                                                  }
{                                                                              }
{   Microsoft Platform SDK: Windows Sockets                                    }
{                                                                              }
{******************************************************************************}

{$IFDEF DEBUG}
{$IFDEF TEST}
  {$DEFINE WINSOCK_TEST}
{$ENDIF}
{$ENDIF}

interface

uses
  { System }
  Windows,
  SysUtils,

  { Fundamentals }
  flcStdTypes;



{                                                                              }
{ WinSock constants                                                            }
{                                                                              }
const
  // Address family
  AF_UNSPEC    = 0;
  AF_UNIX      = 1;
  AF_INET      = 2;
  AF_IMPLINK   = 3;
  AF_PUP       = 4;
  AF_CHAOS     = 5;
  AF_IPX       = 6;
  AF_NS        = 6;
  AF_ISO       = 7;
  AF_OSI       = AF_ISO;
  AF_ECMA      = 8;
  AF_DATAKIT   = 9;
  AF_CCITT     = 10;
  AF_SNA       = 11;
  AF_DECnet    = 12;
  AF_DLI       = 13;
  AF_LAT       = 14;
  AF_HYLINK    = 15;
  AF_APPLETALK = 16;
  AF_NETBIOS   = 17;
  AF_VOICEVIEW = 18;
  AF_FIREFOX   = 19;
  AF_UNKNOWN1  = 20;
  AF_BAN       = 21;
  AF_ATM       = 22;
  AF_INET6     = 23;
  AF_MAX       = 24;

  // Protocol family
  PF_UNSPEC    = AF_UNSPEC;
  PF_UNIX      = AF_UNIX;
  PF_INET      = AF_INET;
  PF_IMPLINK   = AF_IMPLINK;
  PF_PUP       = AF_PUP;
  PF_CHAOS     = AF_CHAOS;
  PF_NS        = AF_NS;
  PF_IPX       = AF_IPX;
  PF_ISO       = AF_ISO;
  PF_OSI       = AF_OSI;
  PF_ECMA      = AF_ECMA;
  PF_DATAKIT   = AF_DATAKIT;
  PF_CCITT     = AF_CCITT;
  PF_SNA       = AF_SNA;
  PF_DECnet    = AF_DECnet;
  PF_DLI       = AF_DLI;
  PF_LAT       = AF_LAT;
  PF_HYLINK    = AF_HYLINK;
  PF_APPLETALK = AF_APPLETALK;
  PF_VOICEVIEW = AF_VOICEVIEW;
  PF_FIREFOX   = AF_FIREFOX;
  PF_UNKNOWN1  = AF_UNKNOWN1;
  PF_BAN       = AF_BAN;
  PF_ATM       = AF_ATM;
  PF_INET6     = AF_INET6;
  PF_MAX       = AF_MAX;

  // Socket type
  SOCK_STREAM    = 1;
  SOCK_DGRAM     = 2;
  SOCK_RAW       = 3;
  SOCK_RDM       = 4;
  SOCK_SEQPACKET = 5;

  // IP Protocol
  IPPROTO_IP     = 0;
  IPPROTO_ICMP   = 1;
  IPPROTO_IGMP   = 2;
  IPPROTO_GGP    = 3;
  IPPROTO_TCP    = 6;
  IPPROTO_PUP    = 12;
  IPPROTO_UDP    = 17;
  IPPROTO_IDP    = 22;
  IPPROTO_IPV6   = 41;
  IPPROTO_ICMPV6 = 58;
  IPPROTO_ND     = 77;
  IPPROTO_RAW    = 255;
  IPPROTO_MAX    = 256;

  // Events
  FD_READ      = $01;
  FD_WRITE     = $02;
  FD_OOB       = $04;
  FD_ACCEPT    = $08;
  FD_CONNECT   = $10;
  FD_CLOSE     = $20;
  FD_QOS       = $40; // WinSock 2
  FD_GROUP_QOS = $80; // WinSock 2

  FD_READ_BIT      = 0;
  FD_WRITE_BIT     = 1;
  FD_OOB_BIT       = 2;
  FD_ACCEPT_BIT    = 3;
  FD_CONNECT_BIT   = 4;
  FD_CLOSE_BIT     = 5;
  FD_QOS_BIT       = 6; // WinSock 2
  FD_GROUP_QOS_BIT = 7; // WinSock 2

  // Socket level
  SOL_SOCKET               = $FFFF;

  // WinSock socket options
  SO_DEBUG                 = $0001;
  SO_ACCEPTCONN            = $0002;
  SO_REUSEADDR             = $0004;
  SO_KEEPALIVE             = $0008;
  SO_DONTROUTE             = $0010;
  SO_BROADCAST             = $0020;
  SO_USELOOPBACK           = $0040;
  SO_LINGER                = $0080;
  SO_OOBINLINE             = $0100;
  SO_SNDBUF                = $1001;
  SO_RCVBUF                = $1002;
  SO_SNDLOWAT              = $1003;
  SO_RCVLOWAT              = $1004;
  SO_SNDTIMEO              = $1005;
  SO_RCVTIMEO              = $1006;
  SO_ERROR                 = $1007;
  SO_TYPE                  = $1008;
  SO_GROUP_ID              = $2001;
  SO_GROUP_PRIORITY        = $2002;
  SO_MAX_MSG_SIZE          = $2003;
  SO_CONNDATA              = $7000;
  SO_CONNOPT               = $7001;
  SO_DISCDATA              = $7002;
  SO_DISCOPT               = $7003;
  SO_CONNDATALEN           = $7004;
  SO_CONNOPTLEN            = $7005;
  SO_DISCDATALEN           = $7006;
  SO_DISCOPTLEN            = $7007;
  SO_OPENTYPE              = $7008;
  SO_MAXDG                 = $7009;
  SO_MAXPATHDG             = $700A;
  SO_UPDATE_ACCEPT_CONTEXT = $700B;
  SO_CONNECT_TIME          = $700C;

  // WinSock TCP options
  TCP_NODELAY   = $0001;
  TCP_BSDURGENT = $7000;

  // WinSock 1 IP_ values
  WS1_IP_OPTIONS          = 1;
  WS1_IP_MULTICAST_IF     = 2;
  WS1_IP_MULTICAST_TTL    = 3;
  WS1_IP_MULTICAST_LOOP   = 4;
  WS1_IP_ADD_MEMBERSHIP   = 5;
  WS1_IP_DROP_MEMBERSHIP  = 6;
  WS1_IP_TTL              = 7;
  WS1_IP_TOS              = 8;
  WS1_IP_DONTFRAGMENT     = 9;

  // WinSock 2 IP_ values
  WS2_IP_OPTIONS          = 1;
  WS2_IP_HDRINCL          = 2;
  WS2_IP_TOS              = 3;
  WS2_IP_TTL              = 4;
  WS2_IP_MULTICAST_IF     = 9;
  WS2_IP_MULTICAST_TTL    = 10;
  WS2_IP_MULTICAST_LOOP   = 11;
  WS2_IP_ADD_MEMBERSHIP   = 12;
  WS2_IP_DROP_MEMBERSHIP  = 13;
  WS2_IP_DONTFRAGMENT     = 14;

var
  // IPPROTO_IP level options
  IP_OPTIONS          : ShortInt = WS2_IP_OPTIONS;
  IP_MULTICAST_IF     : ShortInt = WS2_IP_MULTICAST_IF;
  IP_MULTICAST_TTL    : ShortInt = WS2_IP_MULTICAST_TTL;
  IP_MULTICAST_LOOP   : ShortInt = WS2_IP_MULTICAST_LOOP;
  IP_ADD_MEMBERSHIP   : ShortInt = WS2_IP_ADD_MEMBERSHIP;
  IP_DROP_MEMBERSHIP  : ShortInt = WS2_IP_DROP_MEMBERSHIP;
  IP_TTL              : ShortInt = WS2_IP_TTL;
  IP_TOS              : ShortInt = WS2_IP_TOS;
  IP_DONTFRAGMENT     : ShortInt = WS2_IP_DONTFRAGMENT;
  IP_HDRINCL          : ShortInt = WS2_IP_HDRINCL;

const
  // ShutDown options
  SD_RECEIVE = 0;
  SD_SEND    = 1;
  SD_BOTH    = 2;

  // WSASend/WSASendTo/WSARecv/WSARecvFrom options
  MSG_OOB       = $0001;
  MSG_PEEK      = $0002;
  MSG_DONTROUTE = $0004;
  MSG_INTERRUPT = $0010;
  MSG_PARTIAL   = $8000;

  // WSASocket options
  WSA_FLAG_OVERLAPPED        = $01;
  WSA_FLAG_MULTIPOINT_C_ROOT = $02;
  WSA_FLAG_MULTIPOINT_C_LEAF = $04;
  WSA_FLAG_MULTIPOINT_D_ROOT = $08;
  WSA_FLAG_MULTIPOINT_D_LEAF = $10;

  // Condition function return values
  CF_ACCEPT = 0;
  CF_REJECT = 1;
  CF_DEFER  = 2;

  // Socket group
  SG_UNCONSTRAINED_GROUP = 1;
  SG_CONSTRAINED_GROUP   = 2;

  // WSAJoinLeaf
  JL_SENDER_ONLY   = 1;
  JL_RECEIVER_ONLY = 2;
  JL_BOTH          = 4;

  // WSAIoctl
  IOC_VOID     = $20000000;
  IOC_OUT      = $40000000;
  IOC_IN       = $80000000;
  IOC_INOUT    = (IOC_IN or IOC_OUT);

  IOC_UNIX     = $00000000;
  IOC_WS2      = $08000000;
  IOC_PROTOCOL = $10000000;
  IOC_VENDOR   = $18000000;

  SIO_ASSOCIATE_HANDLE               = IOC_IN    or IOC_WS2 or 1;
  SIO_ENABLE_CIRCULAR_QUEUEING       = IOC_WS2              or 2;
  SIO_FIND_ROUTE                     = IOC_OUT   or IOC_WS2 or 3;
  SIO_FLUSH                          = IOC_WS2              or 4;
  SIO_GET_BROADCAST_ADDRESS          = IOC_OUT   or IOC_WS2 or 5;
  SIO_GET_EXTENSION_FUNCTION_POINTER = IOC_INOUT or IOC_WS2 or 6;
  SIO_GET_QOS                        = IOC_INOUT or IOC_WS2 or 7;
  SIO_GET_GROUP_QOS                  = IOC_INOUT or IOC_WS2 or 8;
  SIO_MULTIPOINT_LOOPBACK            = IOC_IN    or IOC_WS2 or 9;
  SIO_MULTICAST_SCOPE                = IOC_IN    or IOC_WS2 or 10;
  SIO_SET_QOS                        = IOC_IN    or IOC_WS2 or 11;
  SIO_SET_GROUP_QOS                  = IOC_IN    or IOC_WS2 or 12;
  SIO_TRANSLATE_HANDLE               = IOC_INOUT or IOC_WS2 or 13;

  // Namespaces
  NS_ALL         = 0;
  NS_SAP         = 1;
  NS_NDS         = 2;
  NS_PEER_BROWSE = 3;
  NS_TCPIP_LOCAL = 10;
  NS_TCPIP_HOSTS = 11;
  NS_DNS         = 12;
  NS_NETBT       = 13;
  NS_WINS        = 14;
  NS_NBP         = 20;
  NS_MS          = 30;
  NS_STDA        = 31;
  NS_NTDS        = 32;
  NS_X500        = 40;
  NS_NIS         = 41;
  NS_NISPLUS     = 42;
  NS_WRQ         = 50;

  // GetNameInfo flags
  NI_NOFQDN 	   = $01;
  NI_NUMERICHOST = $02;
  NI_NAMEREQD	   = $04;
  NI_NUMERICSERV = $08;
  NI_DGRAM       = $10;

  // GetNameInfo limits
  NI_MAXHOST	   = 1025;
  NI_MAXSERV	   = 32;

  // Resolution flags for WSAGetAddressByName
  RES_UNUSED_1    = $00000001;
  RES_FLUSH_CACHE = $00000002;
  RES_SERVICE     = $00000004;

  // Well known service type names
  SERVICE_TYPE_VALUE_TCPPORTA                 = 'TcpPort';
  SERVICE_TYPE_VALUE_TCPPORTW : UnicodeString = 'TcpPort';
  SERVICE_TYPE_VALUE_UDPPORTA                 = 'UdpPort';
  SERVICE_TYPE_VALUE_UDPPORTW : UnicodeString = 'UdpPort';

  // Limits
  MAXGETHOSTSTRUCT = 1024;

  // Ioctl functions
  FIONREAD = $4004667F;
  FIONBIO  = $8004667E;
  FIOASYNC = $8004667D;

  // IP4 addresses
  INADDR_ANY       = Word32($00000000);
  INADDR_LOOPBACK  = Word32($7F000001);
  INADDR_BROADCAST = Word32(not 0);
  INADDR_NONE      = Word32(not 0);

  // WSAPoll
  POLLERR    = $0001;
  POLLHUP    = $0002;
  POLLNVAL   = $0004;
  POLLWRNORM = $0010;
  POLLWRBAND = $0020;
  POLLRDNORM = $0100;
  POLLRDBAND = $0200;
  POLLPRI    = $0400;
  POLLIN     = POLLRDNORM or POLLRDBAND;
  POLLOUT    = POLLWRNORM;



{                                                                              }
{ WinSock errors                                                               }
{                                                                              }
type
  EWinSock = class(Exception)
  private
    FErrorCode : Integer;
  public
    constructor Create(const Msg: String; const ErrorCode: Integer = -1);
    property ErrorCode: Integer read FErrorCode;
  end;

function  WinSockErrorMessage(const ErrorCode: Integer): String;
procedure RaiseWinSockError(const Msg: String; const ErrorCode: Integer);



{                                                                              }
{ WinSock types                                                                }
{                                                                              }
type
  {$IFDEF OS_WIN64}
  TSocket = UInt64;
  {$ELSE}
  TSocket = UInt32;
  {$ENDIF}

const
  INVALID_SOCKET = TSocket(not 0);

type
  PHostEnt = ^THostEnt;
  hostent = record
    h_name     : PAnsiChar;
    h_aliases  : ^PAnsiChar;
    h_addrtype : SmallInt;
    h_length   : SmallInt;
    case Byte of
      0 : (h_addr_list : ^PAnsiChar);
      1 : (h_addr      : ^PAnsiChar);
  end;
  THostEnt = hostent;

  PNetEnt = ^TNetEnt;
  netent = packed record
    n_name     : PAnsiChar;
    n_aliases  : ^PAnsiChar;
    n_addrtype : SmallInt;
    n_net      : Word32;
  end;
  TNetEnt = netent;

  PServEnt = ^TServEnt;
  servent = record
    s_name    : PAnsiChar;
    s_aliases : ^PAnsiChar;
    {$IFDEF OS_WIN64}
    s_proto   : PAnsiChar;
    s_port    : Word;
    {$ENDIF}
    {$IFDEF OS_WIN32}
    s_port    : Word;
    s_proto   : PAnsiChar;
    {$ENDIF}
  end;
  TServEnt = servent;

  PProtoEnt = ^TProtoEnt;
  protoent = record
    p_name    : PAnsiChar;
    p_aliases : ^PAnsiChar;
    p_proto   : SmallInt;
  end;
  TProtoEnt = protoent;

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
  TInAddrArray = array of TInAddr;
  TInAddrArrayArray = array of TInAddrArray;

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
  TIn6AddrArray = array of TIn6Addr;
  TIn6AddrArrayArray = array of array of TIn6AddrArray;

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
  TSockAddrArray = array of TSockAddr;

  PLinger = ^TLinger;
  linger = record
    l_onoff  : Word;
    l_linger : Word;
  end;
  TLinger = linger;

const
  FD_SETSIZE = 64;

type
  PFDSet = ^TFDSet;
  TFDSet = record
    fd_count : Int32;
    fd_array : array[0..FD_SETSIZE-1] of TSocket;
  end;

function  FD_ISSET(const fd: TSocket; const fdset: TFDSet): Boolean;
procedure FD_SET(const fd: TSocket; var fdset: TFDSet);
procedure FD_CLR(const fd: TSocket; var fdset: TFDSet);
procedure FD_ZERO(var fdset: TFDSet);
function  FD_COUNT(const fdset: TFDSet): Integer;

type
  PTimeVal = ^TTimeVal;
  timeval = record
    tv_sec  : Int32;
    tv_usec : Int32;
  end;
  TTimeVal = timeval;

const
  WSADESCRIPTION_LEN = 256;
  WSASYS_STATUS_LEN  = 128;

type
  PWSAData = ^TWSAData;
  WSAData = record
    Version      : Word;
    HighVersion  : Word;
    {$IFDEF OS_WIN64}
    MaxSockets   : Word;
    MaxUdpDg     : Word;
    VendorInfo   : Pointer;
    Description  : array[0..WSADESCRIPTION_LEN+1-1] of AnsiChar;
    SystemStatus : array[0..WSASYS_STATUS_LEN+1-1] of AnsiChar;
    {$ENDIF}
    {$IFDEF OS_WIN32}
    Description  : array[0..WSADESCRIPTION_LEN+1-1] of AnsiChar;
    SystemStatus : array[0..WSASYS_STATUS_LEN+1-1] of AnsiChar;
    MaxSockets   : Word;
    MaxUdpDg     : Word;
    VendorInfo   : Pointer;
    {$ENDIF}
  end;
  TWSAData = WSAData;

type
  PAddrInfo = ^TAddrInfo;
  addrinfo = record
               ai_flags     : Int32;
               ai_family    : Int32;
               ai_socktype  : Int32;
               ai_protocol  : Int32;
               ai_addrlen   : Int32;
               ai_canonname : PAnsiChar;
               ai_addr      : PSockAddr;
               ai_next      : PAddrInfo;
             end;
  TAddrInfo = addrinfo;

  PAddrInfoW = ^TAddrInfoW;
  addrinfoW = record
                ai_flags     : Int32;
                ai_family    : Int32;
                ai_socktype  : Int32;
                ai_protocol  : Int32;
                ai_addrlen   : Int32;
                ai_canonname : PWideChar;
                ai_addr      : PSockAddr;
                ai_next      : PAddrInfoW;
              end;
  TAddrInfoW = addrinfoW;

const
  // ai_flags constants
  AI_PASSIVE     = $0001;
  AI_CANONNAME   = $0002;
  AI_NUMERICHOST = $0004;



{                                                                              }
{ WinSock 2 types                                                              }
{                                                                              }
type
  PWSABuf = ^TWSABuf;
  WSABUF = record
    Len : Word32;
    Buf : PAnsiChar;
  end;
  TWSABuf = WSABUF;

  TWSABufArray = packed array[0..1023] of TWSABuf;
  PWSABufArray = ^TWSABufArray;

  GROUP = Word;

  PWSAOverlapped = ^TWSAOverlapped;
  WSAOVERLAPPED = TOverlapped;
  TWSAOverlapped = WSAOverlapped;

  PWSAEvent = ^TWSAEvent;
  WSAEVENT = THandle;
  TWSAEvent = WSAEvent;

const
  MAX_PROTOCOL_CHAIN = 7;

type
  TWSAProtocolChain = record
    ChainLen     : Int32;
    ChainEntries : array[0..MAX_PROTOCOL_CHAIN-1] of Word32;
  end;

const
  WSAPROTOCOL_LEN = 255;

type
  PWSAProtocol_InfoA = ^TWSAProtocol_InfoA;
  TWSAProtocol_InfoA = record
    ServiceFlags1     : Word32;
    ServiceFlags2     : Word32;
    ServiceFlags3     : Word32;
    ServiceFlags4     : Word32;
    ProviderFlags     : Word32;
    ProviderId        : TGUID;
    CatalogEntryId    : Word32;
    ProtocolChain     : TWSAProtocolChain;
    Version           : Int32;
    AddressFamily     : Int32;
    MaxSockAddr       : Int32;
    MinSockAddr       : Int32;
    SocketType        : Int32;
    iProtocol         : Int32;
    ProtocolMaxOffset : Int32;
    NetworkByteOrder  : Int32;
    SecurityScheme    : Int32;
    MessageSize       : Word32;
    ProviderReserved  : Word32;
    szProtocol        : array[0..WSAPROTOCOL_LEN+1-1] of AnsiChar;
  end;

  PWSAProtocol_InfoW = ^TWSAProtocol_InfoW;
  TWSAProtocol_InfoW = record
    ServiceFlags1     : Word32;
    ServiceFlags2     : Word32;
    ServiceFlags3     : Word32;
    ServiceFlags4     : Word32;
    ProviderFlags     : Word32;
    ProviderId        : TGUID;
    CatalogEntryId    : Word32;
    ProtocolChain     : TWSAProtocolChain;
    Version           : Int32;
    AddressFamily     : Int32;
    MaxSockAddr       : Int32;
    MinSockAddr       : Int32;
    SocketType        : Int32;
    iProtocol         : Int32;
    ProtocolMaxOffset : Int32;
    NetworkByteOrder  : Int32;
    SecurityScheme    : Int32;
    MessageSize       : Word32;
    ProviderReserved  : Word32;
    szProtocol        : array[0..WSAPROTOCOL_LEN+1-1] of WideChar;
  end;

const
  FD_MAX_EVENTS = 8;

type
  PWSANetworkEvents = ^TWSANetworkEvents;
  TWSANetworkEvents = record
    NetworkEvents : Int32;
    ErrorCode     : array[0..FD_MAX_EVENTS-1] of Int32;
  end;

  TServiceType = Int32;

  PFlowSpec = ^TFlowSpec;
  TFlowSpec = record
    TokenRate          : Int32;
    TokenBucketSize    : Int32;
    PeakBandwidth      : Int32;
    Latency            : Int32;
    DelayVariation     : Int32;
    ServiceType        : TServiceType;
    MaxSduSize         : Int32;
    MinimumPolicedSize : Int32;
  end;

  TQualityOfService = record
    SendingFlowspec   : TFlowSpec;
    ReceivingFlowspec : TFlowSpec;
    ProviderSpecific  : TWSABuf;
  end;
  PQualityOfService = ^TQualityOfService;

  PWSANameSpace_InfoA = ^TWSANameSpace_InfoA;
  TWSANameSpace_InfoA = record
    NSProviderId : TGUID;
    NameSpace    : Word32;
    fActive      : LongBool;
    Version      : Word32;
    Identifier   : PAnsiChar;
  end;
  PWSANameSpace_InfoW = ^TWSANameSpace_InfoW;
  TWSANameSpace_InfoW = record
    NSProviderId : TGUID;
    NameSpace    : Word32;
    fActive      : LongBool;
    Version      : Word32;
    Identifier   : PWideChar;
  end;

  PWSANSClassInfoA = ^TWSANSClassInfoA;
  TWSANSClassInfoA = record
    Name      : PAnsiChar;
    NameSpace : Word32;
    ValueType : Word32;
    ValueSize : Word32;
    Value     : Pointer;
  end;
  PWSANSClassInfoW = ^TWSANSClassInfoW;
  TWSANSClassInfoW = record
    Name      : PWideChar;
    NameSpace : Word32;
    ValueType : Word32;
    ValueSize : Word32;
    Value     : Pointer;
  end;

  TWSAServiceClassInfoA = record
    ServiceClassId   : PGUID;
    ServiceClassName : PAnsiChar;
    Count            : Word32;
    ClassInfos       : PWSANSClassInfoA;
  end;
  PWSAServiceClassInfoA = ^TWSAServiceClassInfoA;
  TWSAServiceClassInfoW = record
    ServiceClassId   : PGUID;
    ServiceClassName : PWideChar;
    Count            : Word32;
    ClassInfos       : PWSANSClassInfoW;
  end;
  PWSAServiceClassInfoW = ^TWSAServiceClassInfoW;

  TWSAEComparator = (COMP_EQUAL, COMP_NOTLESS);
  PWSAVersion = ^TWSAVersion;
  TWSAVersion = record
    Version : Word32;
    How     : TWSAEComparator;
  end;

  PAFProtocols = ^TAFProtocols;
  TAFProtocols = record
    AddressFamily : Int32;
    Protocol      : Int32;
  end;

  PSOCKET_ADDRESS = ^SOCKET_ADDRESS;
  SOCKET_ADDRESS = record
    Sockaddr       : PSockAddr;
    SockaddrLength : Int32;
  end;

  PCSADDR_INFO = ^CSADDR_INFO;
  CSADDR_INFO = record
    LocalAddr  : SOCKET_ADDRESS;
    RemoteAddr : SOCKET_ADDRESS;
    SocketType : Int32;
    Protocol   : Int32;
  end;

  PBLOB = ^TBLOB;
  TBLOB = record
    Size     : Word32;
    BlobData : PByte;
  end;

  TWSAeSetServiceOp = (
      RNRSERVICE_REGISTER,
      RNRSERVICE_DEREGISTER,
      RNRSERVICE_DELETE);

  PWSAQuerySetA = ^TWSAQuerySetA;
  TWSAQuerySetA = record
    Size                : Word32;
    ServiceInstanceName : PAnsiChar;
    ServiceClassId      : PGUID;
    Version             : PWSAVersion;
    Comment             : PAnsiChar;
    NameSpace           : Word32;
    NSProviderId        : PGUID;
    Context             : PAnsiChar;
    NumberOfProtocols   : Word32;
    Protocols           : PAFProtocols;
    QueryString         : PAnsiChar;
    NumberOfCsAddrs     : Word32;
    Buffer              : PCSADDR_INFO;
    OutputFlags         : Word32;
    Blob                : PBLOB;
  end;
  PWSAQuerySetW = ^TWSAQuerySetW;
  TWSAQuerySetW = record
    Size                : Word32;
    ServiceInstanceName : PWideChar;
    ServiceClassId      : PGUID;
    Version             : PWSAVersion;
    Comment             : PWideChar;
    NameSpace           : Word32;
    NSProviderId        : PGUID;
    Context             : PWideChar;
    NumberOfProtocols   : Word32;
    Protocols           : PAFProtocols;
    QueryString         : PWideChar;
    NumberOfCsAddrs     : Word32;
    Buffer              : PCSADDR_INFO;
    OutputFlags         : Word32;
    lpBlob              : PBLOB;
  end;

  LPCONDITIONPROC = function (
      const CallerId: PWSABuf;
      const CallerData: PWSABuf;
      const SQOS, GQOS: PQualityOfService;
      const CalleeId, CalleeData: PWSABuf;
      const g: GROUP;
      {$IFDEF OS_WIN64}
      const CallbackData: UInt64
      {$ENDIF}
      {$IFDEF OS_WIN32}
      const CallbackData: UInt32
      {$ENDIF}
      ): Int32; stdcall;
  TConditionProc = LPCONDITIONPROC;

  LPWSAOVERLAPPED_COMPLETION_ROUTINE = procedure (
      const Error, Transferred: Word32;
      const Overlapped: PWSAOverlapped;
      const Flags: Word32); stdcall;
  TWSAOverlappedCompletionRoutine = LPWSAOVERLAPPED_COMPLETION_ROUTINE;

  TWSAPOLLFD = record
    fd      : TSocket;
    events  : Int16;
    revents : Int16;
  end;
  PWSAPOLLFD = ^TWSAPOLLFD;

  TPollfd = TWSAPOLLFD;
  PPollfd = ^TPollfd;



{                                                                              }
{ WinSock error code constants                                                 }
{                                                                              }
const
  WSABASEERR             = 10000;
  WSAEINTR               = WSABASEERR + 4;
  WSAEBADF               = WSABASEERR + 9;
  WSAEACCES              = WSABASEERR + 13;
  WSAEFAULT              = WSABASEERR + 14;
  WSAEINVAL              = WSABASEERR + 22;
  WSAEMFILE              = WSABASEERR + 24;
  WSAEWOULDBLOCK         = WSABASEERR + 35;
  WSAEINPROGRESS         = WSABASEERR + 36;
  WSAEALREADY            = WSABASEERR + 37;
  WSAENOTSOCK            = WSABASEERR + 38;
  WSAEDESTADDRREQ        = WSABASEERR + 39;
  WSAEMSGSIZE            = WSABASEERR + 40;
  WSAEPROTOTYPE          = WSABASEERR + 41;
  WSAENOPROTOOPT         = WSABASEERR + 42;
  WSAEPROTONOSUPPORT     = WSABASEERR + 43;
  WSAESOCKTNOSUPPORT     = WSABASEERR + 44;
  WSAEOPNOTSUPP          = WSABASEERR + 45;
  WSAEPFNOSUPPORT        = WSABASEERR + 46;
  WSAEAFNOSUPPORT        = WSABASEERR + 47;
  WSAEADDRINUSE          = WSABASEERR + 48;
  WSAEADDRNOTAVAIL       = WSABASEERR + 49;
  WSAENETDOWN            = WSABASEERR + 50;
  WSAENETUNREACH         = WSABASEERR + 51;
  WSAENETRESET           = WSABASEERR + 52;
  WSAECONNABORTED        = WSABASEERR + 53;
  WSAECONNRESET          = WSABASEERR + 54;
  WSAENOBUFS             = WSABASEERR + 55;
  WSAEISCONN             = WSABASEERR + 56;
  WSAENOTCONN            = WSABASEERR + 57;
  WSAESHUTDOWN           = WSABASEERR + 58;
  WSAETOOMANYREFS        = WSABASEERR + 59;
  WSAETIMEDOUT           = WSABASEERR + 60;
  WSAECONNREFUSED        = WSABASEERR + 61;
  WSAELOOP               = WSABASEERR + 62;
  WSAENAMETOOLONG        = WSABASEERR + 63;
  WSAEHOSTDOWN           = WSABASEERR + 64;
  WSAEHOSTUNREACH        = WSABASEERR + 65;
  WSAENOTEMPTY           = WSABASEERR + 66;
  WSAEPROCLIM            = WSABASEERR + 67;
  WSAEUSERS              = WSABASEERR + 68;
  WSAEDQUOT              = WSABASEERR + 69;
  WSAESTALE              = WSABASEERR + 70;
  WSAEREMOTE             = WSABASEERR + 71;
  WSASYSNOTREADY         = WSABASEERR + 91;
  WSAVERNOTSUPPORTED     = WSABASEERR + 92;
  WSANOTINITIALISED      = WSABASEERR + 93;
  WSAEDISCON             = WSABASEERR + 101;
  WSAENOMORE             = WSABASEERR + 102;
  WSAECANCELLED          = WSABASEERR + 103;
  WSAEINVALIDPROCTABLE   = WSABASEERR + 104;
  WSAEINVALIDPROVIDER    = WSABASEERR + 105;
  WSAEPROVIDERFAILEDINIT = WSABASEERR + 106;
  WSASYSCALLFAILURE      = WSABASEERR + 107;
  WSASERVICE_NOT_FOUND   = WSABASEERR + 108;
  WSATYPE_NOT_FOUND      = WSABASEERR + 109;
  WSA_E_NO_MORE          = WSABASEERR + 110;
  WSA_E_CANCELLED        = WSABASEERR + 111;
  WSAEREFUSED            = WSABASEERR + 112;
  WSAHOST_NOT_FOUND      = WSABASEERR + 1001;
  WSATRY_AGAIN           = WSABASEERR + 1002;
  WSANO_RECOVERY         = WSABASEERR + 1003;
  WSANO_DATA             = WSABASEERR + 1004;
  WSANO_ADDRESS          = WSANO_DATA;

  // Define WinSock error identifiers from standard Windows errors
  WSA_INVALID_HANDLE     = ERROR_INVALID_HANDLE;
  WSA_INVALID_PARAMETER  = ERROR_INVALID_PARAMETER;
  WSA_IO_INCOMPLETE      = ERROR_IO_INCOMPLETE;
  WSA_IO_PENDING         = ERROR_IO_PENDING;
  WSA_NOT_ENOUGH_MEMORY  = ERROR_NOT_ENOUGH_MEMORY;
  WSA_OPERATION_ABORTED  = ERROR_OPERATION_ABORTED;



{                                                                              }
{ WinSockStartup / WinSockCleanup                                              }
{                                                                              }
var
  WinSockStarted : Boolean = False;
  WinSockVersion : Word = 0;
  WinSock2API    : Boolean = False;

procedure WinSockStartup(const WinSock2Required: Boolean = False);
function  IsWinSock2API: Boolean;



{ Berkeley socket interface (WinSock)                                          }
type
  TSockLen = Int32;

function  Accept(const S: TSocket; const Addr: PSockAddr; var AddrLen: TSockLen): TSocket;
function  Bind(const S: TSocket; const Name: TSockAddr; const NameLen: Integer): Integer;
function  CloseSocket(const S: TSocket): Integer;
function  Connect(const S: TSocket; const Name: PSockAddr; const NameLen: Integer): Integer;
procedure FreeAddrInfo(const AddrInfo: PAddrInfo);
procedure FreeAddrInfoW(const AddrInfo: PAddrInfoW);
function  GetAddrInfo(const NodeName: PAnsiChar; const ServName: PAnsiChar;
          const Hints: PAddrInfo; var AddrInfo: PAddrInfo): Integer;
function  GetAddrInfoW(const NodeName: PWideChar; const ServName: PWideChar;
          const Hints: PAddrInfoW; var AddrInfo: PAddrInfoW): Integer;
function  GetHostByAddr(const Addr: Pointer; const Len: Integer; const AF: Integer): PHostEnt;
function  GetHostByName(const Name: PAnsiChar): PHostEnt;
function  GetHostName(const Name: PAnsiChar; const Len: Integer): Integer;
function  GetNameInfo(const Addr: PSockAddr; const NameLen: Integer;
          const Host: PAnsiChar; const HostLen: Word32;
          const Serv: PAnsiChar; const ServLen: Word32; const Flags: Integer): Integer;
function  GetPeerName(const S: TSocket; var Name: TSockAddr; var NameLen: TSockLen): Integer;
function  GetProtoByName(const Name: PAnsiChar): PProtoEnt;
function  GetProtoByNumber(const Proto: Integer): PProtoEnt;
function  GetServByName(const Name, Proto: PAnsiChar): PServEnt;
function  GetServByPort(const Port: Integer; const Proto: PAnsiChar): PServEnt;
function  GetSockName(const S: TSocket; var Name: TSockAddr; var NameLen: TSockLen): Integer;
function  GetSockOpt(const S: TSocket; const Level, OptName: Integer;
          const OptVal: Pointer; var OptLen: Integer): Integer;
function  htons(const HostShort: Word): Word;
function  htonl(const HostLong: Word32): Word32;
function  inet_ntoa(const InAddr: TInAddr): PAnsiChar;
function  inet_addr(const P: PAnsiChar): Word32;
function  IoctlSocket(const S: TSocket; const Cmd: Word32; var Arg: Word32): Int32;
function  Listen(const S: TSocket; const Backlog: Integer): Integer;
function  ntohs(const NetShort: Word): Word;
function  ntohl(const NetLong: Word32): Word32;
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



{ WinSock 1 interface                                                          }
function  WSAAsyncGetHostByAddr(const HWindow: HWND; const wMsg: Int32;
          const Addr: PAnsiChar; const Len, Struct: Integer; const Buf: PAnsiChar;
          const BufLen: Integer): THandle;
function  WSAAsyncGetHostByName(const HWindow: HWND; const wMsg: Int32;
          const Name, Buf: PAnsiChar; const BufLen: Integer): THandle;
function  WSAAsyncSelect(const S: TSocket; const HWindow: HWND;
          const wMsg: Int32; const lEvent: Int32): Integer;
function  WSACancelAsyncRequest(const AsyncTaskHandle: THandle): Integer;
function  WSAGetLastError: Integer;
procedure WSASetLastError(const Error: Integer);



{ WinSock 2 interface                                                          }
function  WSAAccept(const S: TSocket;
          var Addr: TSockAddr; var AddrLen: Integer;
          const Condition: TConditionProc;
          {$IFDEF OS_WIN64}
          const CallbackData: UInt64
          {$ENDIF}
          {$IFDEF OS_WIN32}
          const CallbackData: Word32
          {$ENDIF}): TSocket;
function  WSAAddressToStringA(var Address: TSockAddr;
          const AddressLength: Word32;
          const ProtocolInfo: PWSAProtocol_InfoA;
          const AddressString: PAnsiChar; var AddressStringLength: Word32): Integer;
function  WSAAddressToStringW(var Address: TSockAddr;
          const AddressLength: Word32;
          const ProtocolInfo: PWSAProtocol_InfoW;
          const AddressString: PWideChar; var AddressStringLength: Word32): Integer;
function  WSACloseEvent(const Event: WSAEVENT): WordBool;
function  WSAConnect(const S: TSocket;
          const Name: TSockAddr; const NameLen: Integer;
          const CallerData, CalleeData: PWSABuf;
          const SQOS, GQOS: PQualityOfService): Integer;
function  WSACreateEvent: WSAEVENT;
function  WSADuplicateSocketA(const S: TSocket;
          const ProcessId: Word32;
          const ProtocolInfo: PWSAProtocol_InfoA): Integer;
function  WSADuplicateSocketW(const S: TSocket;
          const ProcessId: Word32;
          const ProtocolInfo: PWSAProtocol_InfoW) : Integer;
function  WSAEnumNameSpaceProvidersA(var BufferLength: Word32;
          const Buffer: PWSANameSpace_InfoA): Integer;
function  WSAEnumNameSpaceProvidersW(var BufferLength: Word32;
          const Buffer: PWSANameSpace_InfoW): Integer;
function  WSAEnumNetworkEvents(const S: TSocket;
          const EventObject: WSAEVENT;
          const NetworkEvents: PWSANetworkEvents): Integer;
function  WSAEnumProtocolsA(const lpiProtocols: PInt32;
          const ProtocolBuffer: PWSAProtocol_InfoA;
          var BufferLength: Word32): Integer;
function  WSAEnumProtocolsW(const lpiProtocols: PInt32;
          const ProtocolBuffer: PWSAProtocol_InfoW;
          var BufferLength: Word32): Integer;
function  WSAEventSelect(const S: TSocket; const EventObject: WSAEVENT;
          const NetworkEvents: Int32): Integer;
function  WSAGetOverlappedResult(const S: TSocket; const Overlapped: PWSAOverlapped;
          const lpcbTransfer: LPDWORD; const Wait: BOOL;
          var Flags: Word32): WordBool;
function  WSAGetQosByName(const S: TSocket; const QOSName: PWSABuf;
          const QOS: PQualityOfService): WordBool;
function  WSAGetServiceClassInfoA(const ProviderId: PGUID;
          const ServiceClassId: PGUID; var BufSize: Word32;
          ServiceClassInfo: PWSAServiceClassInfoA): Integer;
function  WSAGetServiceClassInfoW(const ProviderId: PGUID;
          const ServiceClassId: PGUID; var BufSize: Word32;
          ServiceClassInfo: PWSAServiceClassInfoW): Integer;
function  WSAGetServiceClassNameByClassIdA(const ServiceClassId: PGUID;
          ServiceClassName: PAnsiChar; var BufferLength: Word32): Integer;
function  WSAGetServiceClassNameByClassIdW(const ServiceClassId: PGUID;
          ServiceClassName: PWideChar; var BufferLength: Word32 ): Integer;
function  WSAHtonl(const S: TSocket; const HostLong: Word32;
          var NetLong: Word32): Integer;
function  WSAHtons(const S: TSocket; const HostShort: Word;
          var NetShort: Word): Integer;
function  WSAInstallServiceClassA(const ServiceClassInfo: PWSAServiceClassInfoA): Integer;
function  WSAInstallServiceClassW(const ServiceClassInfo: PWSAServiceClassInfoW): Integer;
function  WSAIoctl(const S: TSocket; const IoControlCode: Word32;
          const InBuffer: Pointer; const InBufferSize: Word32;
          const OutBuffer: Pointer; const OutBufferSize: Word32;
          var BytesReturned: Word32;
          const Overlapped: PWSAOverlapped;
          const CompletionRoutine: TWSAOverlappedCompletionRoutine): Integer;
function  WSAJoinLeaf(const S: TSocket; const Name: PSockAddr;
          const NameLen: Integer; const CallerData, CalleeData: PWSABuf;
          const SQOS, GQOS: PQualityOfService;
          const Flags: Word32): TSocket;
function  WSALookupServiceBeginA(const Restrictions: PWSAQuerySetA;
          const ControlFlags: Word32; Lookup: PHANDLE): Integer;
function  WSALookupServiceBeginW(const Restrictions: PWSAQuerySetW;
          const ControlFlags: Word32; Lookup: PHANDLE): Integer;
function  WSALookupServiceEnd(const Lookup: THandle): Integer;
function  WSALookupServiceNextA(const Lookup: THandle; const ControlFlags: Word32;
          var BufferLength: Word32; Results: PWSAQuerySetA): Integer;
function  WSALookupServiceNextW(const Lookup: THandle; const ControlFlags: Word32;
          var BufferLength: Word32; Results: PWSAQuerySetW): Integer;
function  WSANtohl(const S: TSocket; const NetLong: Word32;
          var HostLong: Word32): Integer;
function  WSANtohs(const S: TSocket; const NetShort: Word;
          var HostShort: Word): Integer;
function  WSAPoll(const fdArray: Pointer; const fds: Integer; const Timeout: Integer): Integer;
function  WSARecv(const S: TSocket;
          const Buffers: PWSABuf; const BufferCount: Word32;
          var NumberOfBytesRecvd: Word32; var Flags: Word32;
          const Overlapped: PWSAOverlapped;
          const CompletionRoutine: TWSAOverlappedCompletionRoutine): Integer;
function  WSARecvDisconnect(const S: TSocket; const lpInboundDisconnectData: PWSABuf): Integer;
function  WSARecvFrom(const S: TSocket;
          const Buffers: PWSABuf; const BufferCount: Word32;
          var NumberOfBytesRecvd: Word32; var Flags: Word32;
          const lpFrom: PSockAddr; const lpFromlen: PInt32;
          const Overlapped: PWSAOverlapped;
          const CompletionRoutine: TWSAOverlappedCompletionRoutine): Integer;
function  WSARemoveServiceClass(const ServiceClassId: PGUID): Integer;
function  WSAResetEvent(const Event: WSAEVENT): WordBool;
function  WSASend(const S: TSocket;
          const Buffers: PWSABuf; const BufferCount: Word32;
          var NumberOfBytesSent: Word32;
          const Flags: Word32;
          const Overlapped: PWSAOverlapped;
          const CompletionRoutine: TWSAOverlappedCompletionRoutine): Integer;
function  WSASendDisconnect(const S: TSocket; const OutboundDisconnectData: PWSABuf): Integer;
function  WSASendTo(const S: TSocket;
          const Buffers: PWSABuf; const BufferCount: Word32;
          var NumberOfBytesSent: Word32; const Flags: Word32;
          const AddrTo: PSockAddr; const ToLen: Integer;
          const Overlapped: PWSAOverlapped;
          const CompletionRoutine: TWSAOverlappedCompletionRoutine): Integer;
function  WSASetEvent(const Event: WSAEVENT): WordBool;
function  WSASetServiceA(const RegInfo: PWSAQuerySetA;
          const essoperation: TWSAeSetServiceOp;
          const ControlFlags: Word32): Integer;
function  WSASetServiceW(const RegInfo: PWSAQuerySetW;
          const essoperation: TWSAeSetServiceOp;
          const ControlFlags: Word32): Integer;
function  WSASocketA(const AF, iType, Protocol: Integer;
          const ProtocolInfo: PWSAProtocol_InfoA;
          const G: GROUP; const Flags: Word32): TSocket;
function  WSASocketW(const AF, iType, Protocol: Integer;
          const ProtocolInfo: PWSAProtocol_InfoW;
          const G: GROUP; const Flags: Word32): TSocket;
function  WSAStringToAddressA(const AddressString: PAnsiChar;
          const AddressFamily: Integer; const ProtocolInfo: PWSAProtocol_InfoA;
          var Address: TSockAddr; var AddressLength: Integer): Integer;
function  WSAStringToAddressW(const AddressString: PWideChar;
          const AddressFamily: Integer; const ProtocolInfo: PWSAProtocol_InfoA;
          var Address: TSockAddr; var AddressLength: Integer): Integer;
function  WSAWaitForMultipleEvents(const Events: Word32;
          const lphEvents: PWSAEVENT; const WaitAll: LongBool;
          const Timeout: Word32; const Alertable: LongBool): Word32;



{                                                                              }
{ Socket helpers                                                               }
{                                                                              }
function  SockAvailableToRecv(const S: TSocket): Integer;
procedure SetSockBlocking(const S: TSocket; const Block: Boolean);



{                                                                              }
{ Test cases                                                                   }
{                                                                              }
{$IFDEF WINSOCK_TEST}
procedure Test;
{$ENDIF}



implementation

uses
  { System }
  SyncObjs;



{                                                                              }
{ WinSock errors                                                               }
{                                                                              }
constructor EWinSock.Create(const Msg: String; const ErrorCode: Integer);
begin
  inherited Create(Msg);
  FErrorCode := ErrorCode;
end;

function WinSockErrorMessage(const ErrorCode: Integer): String;
begin
  case ErrorCode of
    0, -1                  : Result := '';
    WSAEINTR               : Result := 'Operation interrupted';
    WSAEBADF               : Result := 'Invalid handle';
    WSAEACCES              : Result := 'Permission denied';
    WSAEFAULT              : Result := 'Invalid pointer';
    WSAEINVAL              : Result := 'Invalid argument';
    WSAEMFILE              : Result := 'Too many open handles';
    WSAEWOULDBLOCK         : Result := 'Blocking operation';
    WSAEINPROGRESS         : Result := 'Operation in progress';
    WSAEALREADY            : Result := 'Operation already performed';
    WSAENOTSOCK            : Result := 'Socket operation on non-socket or not connected';
    WSAEDESTADDRREQ        : Result := 'Destination address required';
    WSAEMSGSIZE            : Result := 'Invalid message size';
    WSAEPROTOTYPE          : Result := 'Invalid protocol type';
    WSAENOPROTOOPT         : Result := 'Protocol not available';
    WSAEPROTONOSUPPORT     : Result := 'Protocol not supported';
    WSAESOCKTNOSUPPORT     : Result := 'Socket type not supported';
    WSAEOPNOTSUPP          : Result := 'Socket operation not supported';
    WSAEPFNOSUPPORT        : Result := 'Protocol family not supported';
    WSAEAFNOSUPPORT        : Result := 'Address family not supported by protocol family';
    WSAEADDRINUSE          : Result := 'Address in use';
    WSAEADDRNOTAVAIL       : Result := 'Address not available';
    WSAENETDOWN	           : Result := 'The network is down';
    WSAENETUNREACH         : Result := 'The network is unreachable';
    WSAENETRESET           : Result := 'Network connection reset';
    WSAECONNABORTED        : Result := 'Connection aborted';
    WSAECONNRESET          : Result := 'Connection reset by peer';
    WSAENOBUFS	           : Result := 'No buffer space available';
    WSAEISCONN             : Result := 'Socket connected';
    WSAENOTCONN            : Result := 'Socket not connected';
    WSAESHUTDOWN           : Result := 'Socket shutdown';
    WSAETOOMANYREFS        : Result := 'Too many references';
    WSAETIMEDOUT           : Result := 'Connection timed out';
    WSAECONNREFUSED        : Result := 'Connection refused';
    WSAENAMETOOLONG        : Result := 'Name too long';
    WSAEHOSTDOWN           : Result := 'Host is unavailable';
    WSAEHOSTUNREACH        : Result := 'Host is unreachable';
    WSAHOST_NOT_FOUND      : Result := 'Host not found';
    WSATRY_AGAIN           : Result := 'Try again';
    WSANO_RECOVERY         : Result := 'Nonrecoverable error';
    WSA_NOT_ENOUGH_MEMORY  : Result := 'Insufficient memory';
    WSAEPROCLIM	           : Result := 'Process limit reached';
    WSASYSNOTREADY         : Result := 'Network subsystem is unavailable';
    WSAVERNOTSUPPORTED     : Result := 'Winsock version not supported';
    WSANOTINITIALISED      : Result := 'Winsock not initialized';
    WSANO_DATA             : Result := 'No data';
    WSAEDISCON             : Result := 'Disconnected';
    WSAENOMORE             : Result := 'No more';
    WSAECANCELLED          : Result := 'Cancelled';
    WSAEINVALIDPROCTABLE   : Result := 'Invalid procedure table from service provider';
    WSAEINVALIDPROVIDER    : Result := 'Invalid service provider version number';
    WSAEPROVIDERFAILEDINIT : Result := 'Unable to initialize a service provider';
    WSASYSCALLFAILURE      : Result := 'System call failure';
    WSASERVICE_NOT_FOUND   : Result := 'Service not found';
    WSATYPE_NOT_FOUND      : Result := 'Type not found';
    WSA_E_NO_MORE          : Result := 'No more';
    WSA_E_CANCELLED        : Result := 'Cancelled';
    WSAEREFUSED            : Result := 'Refused';
    // Windows errors
    WSA_INVALID_HANDLE     : Result := 'Invalid handle';
    WSA_INVALID_PARAMETER  : Result := 'Invalid parameter';
    WSA_IO_INCOMPLETE      : Result := 'Operation incomplete';
    WSA_IO_PENDING         : Result := 'Operation pending';
    WSA_OPERATION_ABORTED  : Result := 'Operation aborted';
  else
    Result := 'System error #' + IntToStr(ErrorCode);
  end;
end;

procedure RaiseWinSockError(const Msg: String; const ErrorCode: Integer);
var ErrMsg : String;
begin
  ErrMsg := WinSockErrorMessage(ErrorCode);
  if ErrMsg = '' then
    ErrMsg := Msg
  else
    ErrMsg := Format('%s: %s', [Msg, ErrMsg]);
  raise EWinSock.Create(ErrMsg, ErrorCode);
end;



{                                                                              }
{ WinSock types                                                                }
{                                                                              }
function FD_ISSET(const fd: TSocket; const fdset: TFDSet): Boolean;
var I : Integer;
begin
  for I := 0 to fdset.fd_count - 1 do
    if fdset.fd_array[I] = fd then
      begin
        Result := True;
        exit;
      end;
  Result := False;
end;

procedure FD_SET(const fd: TSocket; var fdset: TFDSet);
var C : Integer;
begin
  C := fdset.fd_count;
  Assert(C < FD_SETSIZE - 1);
  fdset.fd_array[C] := fd;
  fdset.fd_count := C + 1;
end;

procedure FD_CLR(const fd: TSocket; var fdset: TFDSet);
var I, J : Integer;
begin
  for I := 0 to fdset.fd_count - 1 do
    if fdset.fd_array[I] = fd then
      begin
        for J := I to fdset.fd_count - 2 do
          fdset.fd_array[J] := fdset.fd_array[J + 1];
        Dec(fdset.fd_count);
        exit;
      end;
end;

procedure FD_ZERO(var fdset: TFDSet);
begin
  fdset.fd_count := 0;
end;

function FD_COUNT(const fdset: TFDSet): Integer;
begin
  Result := fdset.fd_count;
end;



{                                                                              }
{ Socket library function types                                                }
{                                                                              }
type
  { WinSock                                                                    }
  TGetServByNameProc =
      function (name, proto: PAnsiChar): PServEnt; stdcall;
  TGetServByPortProc =
      function (port: Int32; proto: PAnsiChar): PServEnt; stdcall;
  TGetProtoByNameProc =
      function (name: PAnsiChar): PProtoEnt; stdcall;
  TGetProtoByNumberProc =
      function (proto: Int32): PProtoEnt; stdcall;
  TGetHostByNameProc =
      function (name: PAnsiChar): PHostEnt; stdcall;
  TGetHostByAddrProc =
      function (addr: Pointer; len, Struct: Int32): PHostEnt; stdcall;
  TGetHostNameProc =
      function (name: PAnsiChar; len: Int32): Int32; stdcall;
  TSocketProc =
      function (af, Struct, protocol: Int32): TSocket; stdcall;
  TShutdownProc =
      function (s: TSocket; how: Int32): Int32; stdcall;
  TSetSockOptProc =
      function (s: TSocket; level, optname: Int32;
                optval: PAnsiChar; optlen: Int32): Int32; stdcall;
  TGetSockOptProc =
      function (s: TSocket; level, optname: Int32;
                optval: PAnsiChar; var optlen: Int32): Int32; stdcall;
  TSendToProc =
      function (s: TSocket; const Buf; len, flags: Int32;
                const addrto: PSockAddr; tolen: Int32): Int32; stdcall;
  TSendProc =
      function (s: TSocket; const Buf; len, flags: Int32): Int32; stdcall;
  TRecvProc =
      function (s: TSocket; var Buf; len, flags: Int32): Int32; stdcall;
  TRecvFromProc =
      function (s: TSocket; var Buf; len, flags: Int32;
                var from: TSockAddr; var fromlen: Int32): Int32; stdcall;
  TntohsProc =
      function (netshort: Word): Word; stdcall;
  TntohlProc =
      function (netlong: Word32): Word32; stdcall;
  TListenProc =
      function (s: TSocket; backlog: Int32): Int32; stdcall;
  TIoctlSocketProc =
      function (s: TSocket; cmd: Word32; var arg: Word32): Int32; stdcall;
  Tinet_ntoaProc =
      function (inaddr: TInAddr): PAnsiChar; stdcall;
  Tinet_addrProc =
      function (cp: PAnsiChar): Word32; stdcall;
  ThtonsProc =
      function (hostshort: Word): Word; stdcall;
  ThtonlProc =
      function (hostlong: Word32): Word32; stdcall;
  TGetSockNameProc =
      function (s: TSocket; var name: TSockAddr; var namelen: Int32): Int32; stdcall;
  TGetPeerNameProc =
      function (s: TSocket; var name: TSockAddr; var namelen: Int32): Int32; stdcall;
  TConnectProc =
      function (s: TSocket; name: PSockAddr; namelen: Int32): Int32; stdcall;
  TCloseSocketProc =
      function (s: TSocket): Int32; stdcall;
  TBindProc =
      function (s: TSocket; name: PSockAddr; namelen: Int32): Int32; stdcall;
  TAcceptProc =
      function (s: TSocket; addr: PSockAddr; var addrlen: Int32): TSocket; stdcall;
  TSelectProc =
      function (nfds: Word32; readfds, writefds, exceptfds: PFDSet;
                timeout: PTimeVal): Int32; stdcall;
  TGetAddrInfoProc =
      function (NodeName: PAnsiChar; ServName: PAnsiChar; Hints: PAddrInfo;
                var Addrinfo: PAddrInfo): Int32; stdcall;
  TGetAddrInfoWProc =
      function (NodeName: PWideChar; ServName: PWideChar; Hints: PAddrInfoW;
                var Addrinfo: PAddrInfoW): Int32; stdcall;
  TFreeAddrInfoProc =
      procedure (ai: PAddrInfo); stdcall;
  TFreeAddrInfoWProc =
      procedure (ai: PAddrInfoW); stdcall;
  TGetNameInfoProc =
      function (addr: PSockAddr; namelen: Int32; host: PAnsiChar;
                hostlen: Word32; serv: PAnsiChar; servlen: Word32;
                flags: Int32): Int32; stdcall;
  TWSAAsyncGetHostByAddrProc =
      function (HWindow: HWND; wMsg: Int32; addr: PAnsiChar; len, Struct: Int32;
                buf: PAnsiChar; buflen: Int32): THandle; stdcall;
  TWSAAsyncGetHostByNameProc =
      function (HWindow: HWND; wMsg: Int32; name, buf: PAnsiChar; buflen: Int32): THandle; stdcall;
  TWSAAsyncSelectProc =
      function (s: TSocket; HWindow: HWND; wMsg: Int32; lEvent: Int32): Int32; stdcall;
  TWSACancelAsyncRequestProc =
      function (hAsyncTaskHandle: THandle): Int32; stdcall;
  TWSACleanupProc =
      function : Int32; stdcall;
  TWSAGetLastErrorProc =
      function : Int32; stdcall;
  TWSASetLastErrorProc =
      procedure (Error: Int32); stdcall;
  TWSAStartupProc =
      function (wVersionRequired: Word; var WSData: TWSAData): Int32; stdcall;

  { WinSock 2                                                                  }
  TWSAAcceptProc =
      function (s: TSocket; addr: PSockAddr; addrlen: PInt32;
                Condition: LPCONDITIONPROC;
                {$IFDEF OS_WIN64}
                const CallbackData: UInt64
                {$ENDIF}
                {$IFDEF OS_WIN32}
                const CallbackData: UInt32
                {$ENDIF}): TSocket; stdcall;
  TWSAAddressToStringAProc =
      function (var Address: TSockAddr; AddressLength: Word32;
                ProtocolInfo: PWSAProtocol_InfoA;
                AddressString: PAnsiChar;
                var AddressStringLength: Word32): Int32; stdcall;
  TWSAAddressToStringWProc =
      function (var Address: TSockAddr; AddressLength: Word32;
                ProtocolInfo: PWSAProtocol_InfoW;
                AddressString: PWideChar;
                var AddressStringLength: Word32): Int32; stdcall;
  TWSACloseEventProc =
      function (Event: WSAEVENT) : WordBool; stdcall;
  TWSAConnectProc =
      function (s: TSocket; name: PSockAddr; namelen: Int32;
                CallerData, CalleeData: PWSABuf;
                SQOS, GQOS: PQualityOfService): Int32; stdcall;
  TWSACreateEventProc =
      function : WSAEVENT; stdcall;
  TWSADuplicateSocketAProc =
      function (s: TSocket; ProcessId: Word32;
                ProtocolInfo: PWSAProtocol_InfoA): Int32; stdcall;
  TWSADuplicateSocketWProc =
      function (s: TSocket; ProcessId: Word32;
                ProtocolInfo: PWSAProtocol_InfoW): Int32; stdcall;
  TWSAEnumNameSpaceProvidersAProc =
      function (var BufferLength: Word32; const Buffer: PWSANameSpace_InfoA): Int32; stdcall;
  TWSAEnumNameSpaceProvidersWProc =
      function (var BufferLength: Word32; const Buffer: PWSANameSpace_InfoW): Int32; stdcall;
  TWSAEnumNetworkEventsProc =
      function (s: TSocket; EventObject: WSAEVENT;
                NetworkEvents: PWSANetworkEvents): Int32; stdcall;
  TWSAEnumProtocolsAProc =
      function (Protocols: PInt32; ProtocolBuffer: PWSAProtocol_InfoA;
                var BufferLength: Word32): Int32; stdcall;
  TWSAEnumProtocolsWProc =
      function (Protocols: PInt32; ProtocolBuffer: PWSAProtocol_InfoW;
                var BufferLength: Word32): Int32; stdcall;
  TWSAEventSelectProc =
      function (s: TSocket; EventObject: WSAEVENT;
                NetworkEvents: Int32): Int32; stdcall;
  TWSAGetOverlappedResultProc =
      function (s: TSocket; Overlapped: PWSAOverlapped;
                lpcbTransfer: LPDWORD; fWait: BOOL;
                var Flags: Word32): WordBool; stdcall;
  TWSAGetQosByNameProc =
      function (s: TSocket; QOSName: PWSABuf; QOS: PQualityOfService): WordBool; stdcall;
  TWSAGetServiceClassInfoAProc =
      function (ProviderId: PGUID; ServiceClassId: PGUID;
                var BufSize: Word32; ServiceClassInfo: PWSAServiceClassInfoA): Int32; stdcall;
  TWSAGetServiceClassInfoWProc =
      function (ProviderId: PGUID; ServiceClassId: PGUID;
                var BufSize: Word32; ServiceClassInfo: PWSAServiceClassInfoW): Int32; stdcall;
  TWSAGetServiceClassNameByClassIdAProc =
      function (ServiceClassId: PGUID; ServiceClassName: PAnsiChar;
                var BufferLength: Word32): Int32; stdcall;
  TWSAGetServiceClassNameByClassIdWProc =
      function (ServiceClassId: PGUID; ServiceClassName: PWideChar;
                var BufferLength: Word32): Int32; stdcall;
  TWSAhtonlProc =
      function (s: TSocket; hostlong: Word32; var netlong: Word32): Int32; stdcall;
  TWSAhtonsProc =
      function (s: TSocket; hostshort: Word; var netshort: Word): Int32; stdcall;
  TWSAInstallServiceClassAProc =
      function (ServiceClassInfo: PWSAServiceClassInfoA): Int32; stdcall;
  TWSAInstallServiceClassWProc =
      function (ServiceClassInfo: PWSAServiceClassInfoW): Int32; stdcall;
  TWSAIoctlProc =
      function (s: TSocket; IoControlCode: Word32;
                InBuffer: Pointer; InBufferLen: Word32;
                OutBuffer: Pointer; OutBufferLen: Word32;
                lpcbBytesReturned: LPDWORD;
                Overlapped: PWSAOverlapped;
                CompletionRoutine: LPWSAOVERLAPPED_COMPLETION_ROUTINE): Int32; stdcall;
  TWSAJoinLeafProc =
      function (s: TSocket; name: PSockAddr; namelen: Int32;
                CallerData, CalleeData: PWSABuf;
                SQOS, GQOS: PQualityOfService; Flags: Word32): TSocket; stdcall;
  TWSALookupServiceBeginAProc =
      function (Restrictions: PWSAQuerySetA; ControlFlags: Word32;
                lphLookup: PHANDLE): Int32; stdcall;
  TWSALookupServiceBeginWProc =
      function (Restrictions: PWSAQuerySetW; ControlFlags: Word32;
                lphLookup: PHANDLE): Int32; stdcall;
  TWSALookupServiceEndProc =
      function (Lookup: THandle): Int32; stdcall;
  TWSALookupServiceNextAProc =
      function (Lookup: THandle; ControlFlags: Word32;
                var BufferLength: Word32; Results: PWSAQuerySetA): Int32; stdcall;
  TWSALookupServiceNextWProc =
      function (Lookup: THandle; ControlFlags: Word32;
                var BufferLength: Word32; Results: PWSAQuerySetW): Int32; stdcall;
  TWSANtohlProc =
      function (s: TSocket; netlong: Word32; var hostlong: Word32): Int32; stdcall;
  TWSANtohsProc =
      function (s: TSocket; netshort: Word; var hostshort: Word): Int32; stdcall;
  TWSAPollProc =
      function (fdArray: Pointer; fds: Word32; timeout: Int32): Int32; stdcall;
  TWSARecvProc =
      function (s: TSocket; Buffers: PWSABuf; BufferCount: Word32;
                var NumberOfBytesRecvd: Word32; var Flags: Word32;
                Overlapped: PWSAOverlapped;
                CompletionRoutine: LPWSAOVERLAPPED_COMPLETION_ROUTINE): Int32; stdcall;
  TWSARecvDisconnectProc =
      function (s: TSocket; InboundDisconnectData: PWSABuf): Int32; stdcall;
  TWSARecvFromProc =
      function (s: TSocket; Buffers: PWSABuf; BufferCount: Word32;
                var NumberOfBytesRecvd: Word32; var Flags: Word32;
                From: PSockAddr; lpFromlen: PInt32;
                Overlapped: PWSAOverlapped;
                CompletionRoutine: LPWSAOVERLAPPED_COMPLETION_ROUTINE): Int32; stdcall;
  TWSARemoveServiceClassProc =
      function (ServiceClassId: PGUID): Int32; stdcall;
  TWSAResetEventProc =
      function (Event: WSAEVENT): WordBool; stdcall;
  TWSASendProc =
      function (s: TSocket; Buffers: PWSABuf; BufferCount: Word32;
                var NumberOfBytesSent: Word32;
                Flags: Word32;
                Overlapped: PWSAOverlapped;
                CompletionRoutine: LPWSAOVERLAPPED_COMPLETION_ROUTINE): Int32; stdcall;
  TWSASendDisconnectProc =
      function (s: TSocket; OutboundDisconnectData: PWSABuf): Int32; stdcall;
  TWSASendToProc =
      function (s: TSocket; Buffers: PWSABuf; BufferCount: Word32;
                var NumberOfBytesSent: Word32;
                Flags: Word32;
                AddrTo: PSockAddr; ToLen: Int32;
                Overlapped: PWSAOverlapped;
                CompletionRoutine: LPWSAOVERLAPPED_COMPLETION_ROUTINE): Int32; stdcall;
  TWSASetEventProc =
      function (Event: WSAEVENT): WordBool; stdcall;
  TWSASetServiceAProc =
      function (RegInfo: PWSAQuerySetA; essoperation: TWSAeSetServiceOp;
                ControlFlags: Word32): Int32; stdcall;
  TWSASetServiceWProc =
      function (RegInfo: PWSAQuerySetW; essoperation: TWSAeSetServiceOp;
                ControlFlags: Word32): Int32; stdcall;
  TWSASocketAProc =
      function (af, iType, protocol: Int32; ProtocolInfo: PWSAProtocol_InfoA;
                g: GROUP; Flags: Word32): TSocket; stdcall;
  TWSASocketWProc =
      function (af, iType, protocol: Int32; ProtocolInfo: PWSAProtocol_InfoW;
                g: GROUP; Flags: Word32): TSocket; stdcall;
  TWSAStringToAddressAProc =
      function (AddressString: PAnsiChar; AddressFamily: Int32;
                ProtocolInfo: PWSAProtocol_InfoA;
                var Address: TSockAddr; var AddressLength: Int32): Int32; stdcall;
  TWSAStringToAddressWProc =
      function (AddressString: PWideChar; AddressFamily: Int32;
                ProtocolInfo: PWSAProtocol_InfoA;
                var Address: TSockAddr; var AddressLength: Int32): Int32; stdcall;
  TWSAWaitForMultipleEventsProc =
      function (cEvents: Word32; lphEvents: PWSAEVENT;
                WaitAll: LongBool; Timeout: Word32; Alertable: LongBool): Word32; stdcall;



{                                                                              }
{ Socket library function variables                                            }
{                                                                              }
var
  AcceptProc                           : TAcceptProc = nil;
  BindProc                             : TBindProc = nil;
  CloseSocketProc                      : TCloseSocketProc = nil;
  ConnectProc                          : TConnectProc = nil;
  FreeAddrInfoProc                     : TFreeAddrInfoProc = nil;
  FreeAddrInfoWProc                    : TFreeAddrInfoWProc = nil;
  GetAddrInfoProc                      : TGetAddrInfoProc = nil;
  GetAddrInfoWProc                     : TGetAddrInfoWProc = nil;
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
  SelectProc                           : TSelectProc = nil;
  SendProc                             : TSendProc = nil;
  SendToProc                           : TSendToProc = nil;
  SetSockOptProc                       : TSetSockOptProc = nil;
  ShutdownProc                         : TShutdownProc = nil;
  SocketProc                           : TSocketProc = nil;

  { WinSock 1                                                                  }
  WSAAsyncGetHostByAddrProc            : TWSAAsyncGetHostByAddrProc = nil;
  WSAAsyncGetHostByNameProc            : TWSAAsyncGetHostByNameProc = nil;
  WSAAsyncSelectProc                   : TWSAAsyncSelectProc = nil;
  WSACancelAsyncRequestProc            : TWSACancelAsyncRequestProc = nil;
  WSACleanupProc                       : TWSACleanupProc = nil;
  WSAGetLastErrorProc                  : TWSAGetLastErrorProc = nil;
  WSASetLastErrorProc                  : TWSASetLastErrorProc = nil;
  WSAStartupProc                       : TWSAStartupProc = nil;

  { WinSock 2                                                                  }
  WSAAcceptProc                        : TWSAAcceptProc = nil;
  WSAAddressToStringAProc              : TWSAAddressToStringAProc = nil;
  WSAAddressToStringWProc              : TWSAAddressToStringWProc = nil;
  WSACloseEventProc                    : TWSACloseEventProc = nil;
  WSAConnectProc                       : TWSAConnectProc = nil;
  WSACreateEventProc                   : TWSACreateEventProc = nil;
  WSADuplicateSocketAProc              : TWSADuplicateSocketAProc = nil;
  WSADuplicateSocketWProc              : TWSADuplicateSocketWProc = nil;
  WSAEnumNameSpaceProvidersAProc       : TWSAEnumNameSpaceProvidersAProc = nil;
  WSAEnumNameSpaceProvidersWProc       : TWSAEnumNameSpaceProvidersWProc = nil;
  WSAEnumNetworkEventsProc             : TWSAEnumNetworkEventsProc = nil;
  WSAEnumProtocolsAProc                : TWSAEnumProtocolsAProc = nil;
  WSAEnumProtocolsWProc                : TWSAEnumProtocolsWProc = nil;
  WSAEventSelectProc                   : TWSAEventSelectProc = nil;
  WSAGetOverlappedResultProc           : TWSAGetOverlappedResultProc = nil;
  WSAGetQosByNameProc                  : TWSAGetQosByNameProc = nil;
  WSAGetServiceClassInfoAProc          : TWSAGetServiceClassInfoAProc = nil;
  WSAGetServiceClassInfoWProc          : TWSAGetServiceClassInfoWProc = nil;
  WSAGetServiceClassNameByClassIdAProc : TWSAGetServiceClassNameByClassIdAProc = nil;
  WSAGetServiceClassNameByClassIdWProc : TWSAGetServiceClassNameByClassIdWProc = nil;
  WSAHtonlProc                         : TWSAhtonlProc = nil;
  WSAHtonsProc                         : TWSAhtonsProc = nil;
  WSAInstallServiceClassAProc          : TWSAInstallServiceClassAProc = nil;
  WSAInstallServiceClassWProc          : TWSAInstallServiceClassWProc = nil;
  WSAIoctlProc                         : TWSAIoctlProc = nil;
  WSAJoinLeafProc                      : TWSAJoinLeafProc = nil;
  WSALookupServiceBeginAProc           : TWSALookupServiceBeginAProc = nil;
  WSALookupServiceBeginWProc           : TWSALookupServiceBeginWProc = nil;
  WSALookupServiceEndProc              : TWSALookupServiceEndProc = nil;
  WSALookupServiceNextAProc            : TWSALookupServiceNextAProc = nil;
  WSALookupServiceNextWProc            : TWSALookupServiceNextWProc = nil;
  WSANtohlProc                         : TWSANtohlProc = nil;
  WSANtohsProc                         : TWSANtohsProc = nil;
  WSAPollProc                          : TWSAPollProc = nil;
  WSARecvProc                          : TWSARecvProc = nil;
  WSARecvDisconnectProc                : TWSARecvDisconnectProc = nil;
  WSARecvFromProc                      : TWSARecvFromProc = nil;
  WSARemoveServiceClassProc            : TWSARemoveServiceClassProc = nil;
  WSAResetEventProc                    : TWSAResetEventProc = nil;
  WSASendProc                          : TWSASendProc = nil;
  WSASendDisconnectProc                : TWSASendDisconnectProc = nil;
  WSASendToProc                        : TWSASendToProc = nil;
  WSASetEventProc                      : TWSASetEventProc = nil;
  WSASetServiceAProc                   : TWSASetServiceAProc = nil;
  WSASetServiceWProc                   : TWSASetServiceWProc = nil;
  WSASocketAProc                       : TWSASocketAProc = nil;
  WSASocketWProc                       : TWSASocketWProc = nil;
  WSAStringToAddressAProc              : TWSAStringToAddressAProc = nil;
  WSAStringToAddressWProc              : TWSAStringToAddressWProc = nil;
  WSAWaitForMultipleEventsProc         : TWSAWaitForMultipleEventsProc = nil;



{                                                                              }
{ WinSock library lock                                                         }
{                                                                              }
var
  WinSockLibLock : TCriticalSection = nil;

procedure InitializeLibLock;
begin
  WinSockLibLock := TCriticalSection.Create;
end;

procedure FinalizeLibLock;
begin
  FreeAndNil(WinSockLibLock);
end;

procedure LibLock;
begin
  if Assigned(WinSockLibLock) then
    WinSockLibLock.Acquire;
end;

procedure LibUnlock;
begin
  if Assigned(WinSockLibLock) then
    WinSockLibLock.Release;
end;



{                                                                              }
{ WinSock library loading / unloading                                          }
{                                                                              }
type
  TWinSockLibraryHandle = HMODULE;

var
  // System handle to dynamically linked library
  SocketLibraryHandle    : TWinSockLibraryHandle = TWinSockLibraryHandle(0);
  SocketLibraryFinalized : Boolean = False;  // True = Library finalised, cannot be loaded anymore
  SocketLibraryLoaded    : Integer = 0;      // 0 = Not loaded, 1 = SocketLibraryName1, 2 = SocketLibraryName2

const
  // The WinSock 2 library is first attempted before falling back
  // to the WinSock 1 library.
  SocketLibraryName1 = 'ws2_32.dll';   // WinSock 2
  SocketLibraryName2 = 'wsock32.dll';  // WinSock 1

procedure LoadSocketLibrary;

  // Use the system to load the dynamically linked socket library file.
  // Returns True on success.
  function LoadLibrary(const LibraryName: AnsiString): Boolean;
  begin
    SocketLibraryHandle := Windows.LoadLibraryA(PAnsiChar(LibraryName));
    Result := (SocketLibraryHandle > HINSTANCE_ERROR);
  end;

begin
  // Ignore if already loaded
  if Word32(SocketLibraryHandle) <> 0 then
    exit;
  // Raise an exception if an attempt is made to reload the library after
  // unit has finalized
  if SocketLibraryFinalized then
    raise EWinSock.Create('Socket library finalized');
  // Load socket library
  if LoadLibrary(SocketLibraryName1) then
    SocketLibraryLoaded := 1
  else if LoadLibrary(SocketLibraryName2) then
    SocketLibraryLoaded := 2
  else
    begin
      // Failure
      SocketLibraryHandle := TWinSockLibraryHandle(0);
      SocketLibraryLoaded := 0;
      raise EWinSock.Create('Failed to load socket library');
    end;
end;

procedure UnloadSocketLibrary;
var H : TWinSockLibraryHandle;
begin
  // Ignore if not loaded
  H := SocketLibraryHandle;
  if Word32(H) = 0 then
    exit;
  // Set state unloaded
  SocketLibraryHandle := TWinSockLibraryHandle(0);
  SocketLibraryLoaded := 0;
  // Clear function references
  AcceptProc := nil;
  BindProc := nil;
  CloseSocketProc := nil;
  ConnectProc := nil;
  FreeAddrInfoProc := nil;
  FreeAddrInfoWProc := nil;
  GetAddrInfoProc := nil;
  GetAddrInfoWProc := nil;
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
  SelectProc := nil;
  SendProc := nil;
  SendToProc := nil;
  SetSockOptProc := nil;
  ShutdownProc := nil;
  SocketProc := nil;
  // WinSock 1
  WSAAsyncGetHostByAddrProc := nil;
  WSAAsyncGetHostByNameProc := nil;
  WSAAsyncSelectProc := nil;
  WSACancelAsyncRequestProc := nil;
  WSACleanupProc := nil;
  WSAGetLastErrorProc := nil;
  WSASetLastErrorProc := nil;
  WSAStartupProc := nil;
  // WinSock 2
  WSAAcceptProc := nil;
  WSAAddressToStringAProc := nil;
  WSAAddressToStringWProc := nil;
  WSACloseEventProc := nil;
  WSAConnectProc := nil;
  WSACreateEventProc := nil;
  WSADuplicateSocketAProc := nil;
  WSADuplicateSocketWProc := nil;
  WSAEnumNameSpaceProvidersAProc := nil;
  WSAEnumNameSpaceProvidersWProc := nil;
  WSAEnumNetworkEventsProc := nil;
  WSAEnumProtocolsAProc := nil;
  WSAEnumProtocolsWProc := nil;
  WSAEventSelectProc := nil;
  WSAGetOverlappedResultProc := nil;
  WSAGetQosByNameProc := nil;
  WSAGetServiceClassInfoAProc := nil;
  WSAGetServiceClassInfoWProc := nil;
  WSAGetServiceClassNameByClassIdAProc := nil;
  WSAGetServiceClassNameByClassIdWProc := nil;
  WSAHtonlProc := nil;
  WSAHtonsProc := nil;
  WSAInstallServiceClassAProc := nil;
  WSAInstallServiceClassWProc := nil;
  WSAIoctlProc := nil;
  WSAJoinLeafProc := nil;
  WSALookupServiceBeginAProc := nil;
  WSALookupServiceBeginWProc := nil;
  WSALookupServiceEndProc := nil;
  WSALookupServiceNextAProc := nil;
  WSALookupServiceNextWProc := nil;
  WSANtohlProc := nil;
  WSANtohsProc := nil;
  WSAPollProc := nil;
  WSARecvProc := nil;
  WSARecvDisconnectProc := nil;
  WSARecvFromProc := nil;
  WSARemoveServiceClassProc := nil;
  WSAResetEventProc := nil;
  WSASendProc := nil;
  WSASendDisconnectProc := nil;
  WSASendToProc := nil;
  WSASetEventProc := nil;
  WSASetServiceAProc := nil;
  WSASetServiceWProc := nil;
  WSASocketAProc := nil;
  WSASocketWProc := nil;
  WSAStringToAddressAProc := nil;
  WSAStringToAddressWProc := nil;
  WSAWaitForMultipleEventsProc := nil;
  // Unload socket library
  Windows.FreeLibrary(H);
end;

procedure GetSocketProc(const ProcName: AnsiString; var Proc: Pointer);
begin
  LibLock;
  try
    // Check if already linked
    if Assigned(Proc) then
      exit;
    // Load socket library
    if Word32(SocketLibraryHandle) = 0 then
      LoadSocketLibrary;
    Assert(Word32(SocketLibraryHandle) <> 0);
    // Get socket procedure
    Proc := Windows.GetProcAddress(SocketLibraryHandle, PAnsiChar(ProcName));
    // Check success
    if not Assigned(Proc) then
      raise EWinSock.CreateFmt('Failed to link socket library function: %s', [ProcName]);
  finally
    LibUnlock;
  end;
end;



{                                                                              }
{ Socket library functions                                                     }
{                                                                              }
function Accept(const S: TSocket; const Addr: PSockAddr; var AddrLen: Integer): TSocket;
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
    GetSocketProc('closesocket', @CloseSocketProc);
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

procedure FreeAddrInfoW(const AddrInfo: PAddrInfoW);
begin
  if not Assigned(FreeAddrInfoWProc) then
    GetSocketProc('FreeAddrInfoW', @FreeAddrInfoWProc);
  FreeAddrInfoWProc(AddrInfo);
end;

function GetAddrInfo(const NodeName: PAnsiChar; const ServName: PAnsiChar;
    const Hints: PAddrInfo; var AddrInfo: PAddrInfo): Integer;
begin
  if not Assigned(GetAddrInfoProc) then
    GetSocketProc('getaddrinfo', @GetAddrInfoProc);
  Result := GetAddrInfoProc(NodeName, ServName, Hints, AddrInfo);
end;

function GetAddrInfoW(const NodeName: PWideChar; const ServName: PWideChar;
    const Hints: PAddrInfoW; var AddrInfo: PAddrInfoW): Integer;
begin
  if not Assigned(GetAddrInfoWProc) then
    GetSocketProc('GetAddrInfoW', @GetAddrInfoWProc);
  Result := GetAddrInfoWProc(NodeName, ServName, Hints, AddrInfo);
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

function IoctlSocket(const S: TSocket; const Cmd: Word32; var Arg: Word32): Int32;
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

{ WinSock 1                                                                    }
function WSAAsyncGetHostByAddr(const HWindow: HWND; const wMsg: Int32;
    const Addr: PAnsiChar; const Len, Struct: Integer; const Buf: PAnsiChar;
    const BufLen: Integer): THandle;
begin
  if not Assigned(WSAAsyncGetHostByAddrProc) then
    GetSocketProc('WSAAsyncGetHostByAddr', @WSAAsyncGetHostByAddrProc);
  Result := WSAAsyncGetHostByAddrProc(HWindow, wMsg, Addr, Len, Struct, Buf, BufLen);
end;

function WSAAsyncGetHostByName(const HWindow: HWND; const wMsg: Int32;
    const Name, Buf: PAnsiChar; const BufLen: Integer): THandle;
begin
  if not Assigned(WSAAsyncGetHostByNameProc) then
    GetSocketProc('WSAAsyncGetHostByName', @WSAAsyncGetHostByNameProc);
  Result := WSAAsyncGetHostByNameProc(HWindow, wMsg, Name, Buf, BufLen);
end;

function WSAAsyncSelect(const S: TSocket; const HWindow: HWND;
    const wMsg: Int32; const lEvent: Int32): Integer;
begin
  if not Assigned(WSAAsyncSelectProc) then
    GetSocketProc('WSAAsyncSelect', @WSAAsyncSelectProc);
  Result := WSAAsyncSelectProc(S, HWindow, wMsg, lEvent);
end;

function WSACancelAsyncRequest(const AsyncTaskHandle: THandle): Integer;
begin
  if not Assigned(WSACancelAsyncRequestProc) then
    GetSocketProc('WSACancelAsyncRequest', @WSACancelAsyncRequestProc);
  Result := WSACancelAsyncRequestProc(AsyncTaskHandle);
end;

function WSACleanup: Integer;
begin
  if not Assigned(WSACleanupProc) then
    GetSocketProc('WSACleanup', @WSACleanupProc);
  Result := WSACleanupProc;
end;

function WSAGetLastError: Integer;
begin
  if not Assigned(WSAGetLastErrorProc) then
    GetSocketProc('WSAGetLastError', @WSAGetLastErrorProc);
  Result := WSAGetLastErrorProc;
end;

procedure WSASetLastError(const Error: Integer);
begin
  if not Assigned(WSASetLastErrorProc) then
    GetSocketProc('WSASetLastError', @WSASetLastErrorProc);
  WSASetLastErrorProc(Error);
end;

function WSAStartup(wVersionRequired: Word; var WSData: TWSAData): Integer;
begin
  if not Assigned(WSAStartupProc) then
    GetSocketProc('WSAStartup', @WSAStartupProc);
  Result := WSAStartupProc(wVersionRequired, WSData);
end;

{ WinSock 2                                                                    }
function WSAAccept(const S: TSocket; var Addr: TSockAddr; var AddrLen: Integer;
    const Condition: TConditionProc;
    {$IFDEF OS_WIN64}
    const CallbackData: UInt64
    {$ENDIF}
    {$IFDEF OS_WIN32}
    const CallbackData: UInt32
    {$ENDIF}): TSocket;
begin
  if not Assigned(WSAAcceptProc) then
    GetSocketProc('WSAAccept', @WSAAcceptProc);
  Result := WSAAcceptProc(S, @addr, @addrlen, Condition, CallbackData);
end;

function WSAAddressToStringA(var Address: TSockAddr;
    const AddressLength: Word32;
    const ProtocolInfo: PWSAProtocol_InfoA;
    const AddressString: PAnsiChar; var AddressStringLength: Word32): Integer;
begin
  if not Assigned(WSAAddressToStringAProc) then
    GetSocketProc('WSAAddressToStringA', @WSAAddressToStringAProc);
  Result := WSAAddressToStringAProc(Address, AddressLength,
      ProtocolInfo, AddressString, AddressStringLength);
end;

function WSAAddressToStringW(var Address: TSockAddr;
    const AddressLength: Word32;
    const ProtocolInfo: PWSAProtocol_InfoW;
    const AddressString: PWideChar; var AddressStringLength: Word32): Integer;
begin
  if not Assigned(WSAAddressToStringWProc) then
    GetSocketProc('WSAAddressToStringW', @WSAAddressToStringWProc);
  Result := WSAAddressToStringWProc(Address, AddressLength,
      ProtocolInfo, AddressString, AddressStringLength);
end;

function WSACloseEvent(const Event: WSAEVENT): WordBool;
begin
  if not Assigned(WSACloseEventProc) then
    GetSocketProc('WSACloseEvent', @WSACloseEventProc);
  Result := WSACloseEventProc(Event);
end;

function WSAConnect(const S: TSocket;
    const Name: TSockAddr; const NameLen: Integer;
    const CallerData, CalleeData: PWSABuf;
    const SQOS, GQOS: PQualityOfService): Integer;
begin
  if not Assigned(WSAConnectProc) then
    GetSocketProc('WSAConnect', @WSAConnectProc);
  Result := WSAConnectProc(S, @name, namelen, CallerData, CalleeData,
      SQOS, GQOS);
end;

function WSACreateEvent: WSAEVENT;
begin
  if not Assigned(WSACreateEventProc) then
    GetSocketProc('WSACreateEvent', @WSACreateEventProc);
  Result := WSACreateEventProc;
end;

function WSADuplicateSocketA(const S: TSocket; const ProcessId: Word32;
    const ProtocolInfo: PWSAProtocol_InfoA): Integer;
begin
  if not Assigned(WSADuplicateSocketAProc) then
    GetSocketProc('WSADuplicateSocketA', @WSADuplicateSocketAProc);
  Result := WSADuplicateSocketAProc(S, ProcessId, ProtocolInfo);
end;

function WSADuplicateSocketW(const S: TSocket; const ProcessId: Word32;
    const ProtocolInfo: PWSAProtocol_InfoW) : Integer;
begin
  if not Assigned(WSADuplicateSocketWProc) then
    GetSocketProc('WSADuplicateSocketW', @WSADuplicateSocketWProc);
  Result := WSADuplicateSocketWProc(S, ProcessId, ProtocolInfo);
end;

function WSAEnumNetworkEvents(const S: TSocket; const EventObject: WSAEVENT;
    const NetworkEvents: PWSANetworkEvents): Integer;
begin
  if not Assigned(WSAEnumNetworkEventsProc) then
    GetSocketProc('WSAEnumNetworkEvents', @WSAEnumNetworkEventsProc);
  Result := WSAEnumNetworkEventsProc(S, EventObject, NetworkEvents);
end;

function WSAEnumProtocolsA(const lpiProtocols: PInt32;
    const ProtocolBuffer: PWSAProtocol_InfoA;
    var BufferLength: Word32): Integer;
begin
  if not Assigned(WSAEnumProtocolsAProc) then
    GetSocketProc('WSAEnumProtocolsA', @WSAEnumProtocolsAProc);
  Result := WSAEnumProtocolsAProc(lpiProtocols, ProtocolBuffer, BufferLength);
end;

function WSAEnumProtocolsW(const lpiProtocols: PInt32;
    const ProtocolBuffer: PWSAProtocol_InfoW;
    var BufferLength: Word32): Integer;
begin
  if not Assigned(WSAEnumProtocolsWProc) then
    GetSocketProc('WSAEnumProtocolsW', @WSAEnumProtocolsWProc);
  Result := WSAEnumProtocolsWProc(lpiProtocols, ProtocolBuffer, BufferLength);
end;

function WSAEventSelect(const S: TSocket; const EventObject: WSAEVENT;
    const NetworkEvents: Int32): Integer;
begin
  if not Assigned(WSAEventSelectProc) then
    GetSocketProc('WSAEventSelect', @WSAEventSelectProc);
  Result := WSAEventSelectProc(S, EventObject, NetworkEvents);
end;

function WSAGetOverlappedResult(const S: TSocket; const Overlapped: PWSAOverlapped;
    const lpcbTransfer: LPDWORD; const Wait: BOOL;
    var Flags: Word32): WordBool;
begin
  if not Assigned(WSAGetOverlappedResultProc) then
    GetSocketProc('WSAGetOverlappedResult', @WSAGetOverlappedResultProc);
  Result := WSAGetOverlappedResultProc(S, Overlapped, lpcbTransfer, Wait, Flags);
end;

function WSAGetQosByName(const S: TSocket; const QOSName: PWSABuf;
    const QOS: PQualityOfService): WordBool;
begin
  if not Assigned(WSAGetQosByNameProc) then
    GetSocketProc('WSAGetQosByName', @WSAGetQosByNameProc);
  Result := WSAGetQosByNameProc(S, QOSName, QOS);
end;

function WSAHtonl(const S: TSocket; const HostLong: Word32;
    var NetLong: Word32): Integer;
begin
  if not Assigned(WSAHtonlProc) then
    GetSocketProc('WSAHtonl', @WSAHtonlProc);
  Result := WSAHtonlProc(S, HostLong, NetLong);
end;

function WSAHtons(const S: TSocket; const HostShort: Word;
    var NetShort: Word): Integer;
begin
  if not Assigned(WSAHtonsProc) then
    GetSocketProc('WSAHtons', @WSAHtonsProc);
  Result := WSAHtonsProc(S, HostShort, NetShort);
end;

function WSAIoctl(const S: TSocket; const IoControlCode: Word32;
    const InBuffer: Pointer; const InBufferSize: Word32;
    const OutBuffer: Pointer; const OutBufferSize: Word32;
    var BytesReturned: Word32;
    const Overlapped: PWSAOverlapped;
    const CompletionRoutine: TWSAOverlappedCompletionRoutine): Integer;
begin
  if not Assigned(WSAIoctlProc) then
    GetSocketProc('WSAIoctl', @WSAIoctlProc);
  Result := WSAIoctlProc(S, IoControlCode, InBuffer, InBufferSize,
      OutBuffer, OutBufferSize, @BytesReturned, Overlapped,
      CompletionRoutine);
end;

function WSAJoinLeaf(const S: TSocket; const Name: PSockAddr;
    const NameLen: Integer; const CallerData, CalleeData: PWSABuf;
    const SQOS, GQOS: PQualityOfService;
    const Flags: Word32): TSocket;
begin
  if not Assigned(WSAJoinLeafProc) then
    GetSocketProc('WSAJoinLeaf', @WSAJoinLeafProc);
  Result := WSAJoinLeafProc(S, Name, NameLen, CallerData, CalleeData, SQOS, GQOS, Flags);
end;

function WSANtohl(const S: TSocket; const NetLong: Word32;
    var HostLong: Word32): Integer;
begin
  if not Assigned(WSANtohlProc) then
    GetSocketProc('WSANtohl', @WSANtohlProc);
  Result := WSANtohlProc(S, NetLong, HostLong);
end;

function WSANtohs(const S: TSocket; const NetShort: Word;
    var HostShort: Word): Integer;
begin
  if not Assigned(WSANtohsProc) then
    GetSocketProc('WSANtohs', @WSANtohsProc);
  Result := WSANtohsProc(S, NetShort, HostShort);
end;

function WSAPoll(const fdArray: Pointer; const fds: Integer; const Timeout: Integer): Integer;
begin
  if not Assigned(WSAPollProc) then
    GetSocketProc('WSAPoll', @WSAPollProc);
  Result := WSAPollProc(fdArray, fds, Timeout);
end;

function WSARecv(const S: TSocket; const Buffers: PWSABuf;
    const BufferCount: Word32;
    var NumberOfBytesRecvd: Word32; var Flags: Word32;
    const Overlapped: PWSAOverlapped;
    const CompletionRoutine: TWSAOverlappedCompletionRoutine): Integer;
begin
  if not Assigned(WSARecvProc) then
    GetSocketProc('WSARecv', @WSARecvProc);
  Result := WSARecvProc(S, Buffers, BufferCount, NumberOfBytesRecvd, Flags,
      Overlapped, CompletionRoutine);
end;

function WSARecvDisconnect(const S: TSocket; const lpInboundDisconnectData: PWSABuf): Integer;
begin
  if not Assigned(WSARecvDisconnectProc) then
    GetSocketProc('WSARecvDisconnect', @WSARecvDisconnectProc);
  Result := WSARecvDisconnectProc(S, lpInboundDisconnectData);
end;

function WSARecvFrom(const S: TSocket; const Buffers: PWSABuf;
    const BufferCount: Word32;
    var NumberOfBytesRecvd: Word32; var Flags: Word32;
    const lpFrom: PSockAddr; const lpFromlen: PInt32;
    const Overlapped: PWSAOverlapped;
    const CompletionRoutine: TWSAOverlappedCompletionRoutine): Integer;
begin
  if not Assigned(WSARecvFromProc) then
    GetSocketProc('WSARecvFrom', @WSARecvFromProc);
  Result := WSARecvFromProc(S, Buffers, BufferCount, NumberOfBytesRecvd, Flags,
      lpFrom, lpFromlen, Overlapped, CompletionRoutine);
end;

function WSAResetEvent(const Event: WSAEVENT): WordBool;
begin
  if not Assigned(WSAResetEventProc) then
    GetSocketProc('WSAResetEvent', @WSAResetEventProc);
  Result := WSAResetEventProc(Event);
end;

function WSASend(const S: TSocket;
    const Buffers: PWSABuf; const BufferCount: Word32;
    var NumberOfBytesSent: Word32;
    const Flags: Word32;
    const Overlapped: PWSAOverlapped;
    const CompletionRoutine: TWSAOverlappedCompletionRoutine): Integer;
begin
  if not Assigned(WSASendProc) then
    GetSocketProc('WSASend', @WSASendProc);
  Result := WSASendProc(S, Buffers, BufferCount, NumberOfBytesSent, Flags,
      Overlapped, CompletionRoutine);
end;

function WSASendDisconnect(const S: TSocket; const OutboundDisconnectData: PWSABuf): Integer;
begin
  if not Assigned(WSASendDisconnectProc) then
    GetSocketProc('WSASendDisconnect', @WSASendDisconnectProc);
  Result := WSASendDisconnectProc(S, OutboundDisconnectData);
end;

function WSASendTo(const S: TSocket; const Buffers: PWSABuf;
    const BufferCount: Word32; var NumberOfBytesSent: Word32;
    const Flags: Word32;
    const AddrTo: PSockAddr; const ToLen : Integer;
    const Overlapped: PWSAOverlapped;
    const CompletionRoutine: TWSAOverlappedCompletionRoutine): Integer;
begin
  if not Assigned(WSASendToProc) then
    GetSocketProc('WSASendTo', @WSASendToProc);
  Result := WSASendToProc(S, Buffers, BufferCount, NumberOfBytesSent,
      Flags, AddrTo, ToLen, Overlapped, CompletionRoutine);
end;

function WSASetEvent(const Event: WSAEVENT): WordBool;
begin
  if not Assigned(WSASetEventProc) then
    GetSocketProc('WSASetEvent', @WSASetEventProc);
  Result := WSASetEventProc(Event);
end;

function WSASocketA(const AF, iType, Protocol: Integer;
    const ProtocolInfo: PWSAProtocol_InfoA;
    const G: GROUP; const Flags: Word32): TSocket;
begin
  if not Assigned(WSASocketAProc) then
    GetSocketProc('WSASocketA', @WSASocketAProc);
  Result := WSASocketAProc(AF, iType, Protocol, ProtocolInfo, G, Flags);
end;

function WSASocketW(const AF, iType, Protocol: Integer;
    const ProtocolInfo: PWSAProtocol_InfoW;
    const G: GROUP; const Flags: Word32): TSocket;
begin
  if not Assigned(WSASocketWProc) then
    GetSocketProc('WSASocketW', @WSASocketWProc);
  Result := WSASocketWProc(AF, iType, Protocol, ProtocolInfo, G, Flags);
end;

function WSAStringToAddressA(const AddressString: PAnsiChar;
    const AddressFamily: Integer; const ProtocolInfo: PWSAProtocol_InfoA;
    var Address: TSockAddr; var AddressLength: Integer): Integer;
begin
  if not Assigned(WSAStringToAddressAProc) then
    GetSocketProc('WSAStringToAddressA', @WSAStringToAddressAProc);
  Result := WSAStringToAddressAProc(AddressString, AddressFamily,
      ProtocolInfo, Address, AddressLength);
end;

function WSAStringToAddressW(const AddressString: PWideChar;
    const AddressFamily: Integer; const ProtocolInfo: PWSAProtocol_InfoA;
    var Address: TSockAddr; var AddressLength: Integer): Integer;
begin
  if not Assigned(WSAStringToAddressWProc) then
    GetSocketProc('WSAStringToAddressW', @WSAStringToAddressWProc);
  Result := WSAStringToAddressWProc(AddressString, AddressFamily,
      ProtocolInfo, Address, AddressLength);
end;

function WSAWaitForMultipleEvents(const Events: Word32;
    const lphEvents: PWSAEVENT; const WaitAll: LongBool;
    const Timeout: Word32; const Alertable: LongBool): Word32;
begin
  if not Assigned(WSAWaitForMultipleEventsProc) then
    GetSocketProc('WSAWaitForMultipleEvents', @WSAWaitForMultipleEventsProc);
  Result := WSAWaitForMultipleEventsProc(Events, lphEvents, WaitAll, TimeOut, Alertable);
end;

function WSAEnumNameSpaceProvidersA(var BufferLength: Word32;
    const Buffer: PWSANameSpace_InfoA): Integer;
begin
  if not Assigned(WSAEnumNameSpaceProvidersAProc) then
    GetSocketProc('WSAEnumNameSpaceProvidersA', @WSAEnumNameSpaceProvidersAProc);
  Result := WSAEnumNameSpaceProvidersAProc(BufferLength, Buffer);
end;

function WSAEnumNameSpaceProvidersW(var BufferLength: Word32;
    const Buffer: PWSANameSpace_InfoW): Integer;
begin
  if not Assigned(WSAEnumNameSpaceProvidersWProc) then
    GetSocketProc('WSAEnumNameSpaceProvidersW', @WSAEnumNameSpaceProvidersWProc);
  Result := WSAEnumNameSpaceProvidersWProc(BufferLength, Buffer);
end;

function WSAGetServiceClassInfoA(const ProviderId: PGUID;
    const ServiceClassId: PGUID; var BufSize: Word32;
    ServiceClassInfo: PWSAServiceClassInfoA): Integer;
begin
  if not Assigned(WSAGetServiceClassInfoAProc) then
    GetSocketProc('WSAGetServiceClassInfoA', @WSAGetServiceClassInfoAProc);
  Result := WSAGetServiceClassInfoAProc(ProviderId, ServiceClassId,
      BufSize, ServiceClassInfo);
end;

function WSAGetServiceClassInfoW(const ProviderId: PGUID;
    const ServiceClassId: PGUID; var BufSize: Word32;
    ServiceClassInfo: PWSAServiceClassInfoW): Integer;
begin
  if not Assigned(WSAGetServiceClassInfoWProc) then
    GetSocketProc('WSAGetServiceClassInfoW', @WSAGetServiceClassInfoWProc);
  Result := WSAGetServiceClassInfoWProc(ProviderId, ServiceClassId,
      BufSize, ServiceClassInfo);
end;

function WSAGetServiceClassNameByClassIdA(const ServiceClassId: PGUID;
    ServiceClassName: PAnsiChar; var BufferLength: Word32): Integer;
begin
  if not Assigned(WSAGetServiceClassNameByClassIdAProc) then
    GetSocketProc('WSAGetServiceClassNameByClassIdA', @WSAGetServiceClassNameByClassIdAProc);
  Result := WSAGetServiceClassNameByClassIdAProc(ServiceClassId,
      ServiceClassName, BufferLength);
end;

function WSAGetServiceClassNameByClassIdW(const ServiceClassId: PGUID;
    ServiceClassName: PWideChar; var BufferLength: Word32): Integer;
begin
  if not Assigned(WSAGetServiceClassNameByClassIdWProc) then
    GetSocketProc('WSAGetServiceClassNameByClassIdW', @WSAGetServiceClassNameByClassIdWProc);
  Result := WSAGetServiceClassNameByClassIdWProc(ServiceClassId,
      ServiceClassName, BufferLength);
end;

function WSAInstallServiceClassA(const ServiceClassInfo: PWSAServiceClassInfoA): Integer;
begin
  if not Assigned(WSAInstallServiceClassAProc) then
    GetSocketProc('WSAInstallServiceClassA', @WSAInstallServiceClassAProc);
  Result := WSAInstallServiceClassAProc(ServiceClassInfo);
end;

function WSAInstallServiceClassW(const ServiceClassInfo: PWSAServiceClassInfoW): Integer;
begin
  if not Assigned(WSAInstallServiceClassWProc) then
    GetSocketProc('WSAInstallServiceClassW', @WSAInstallServiceClassWProc);
  Result := WSAInstallServiceClassWProc(ServiceClassInfo);
end;

function WSALookupServiceBeginA(const Restrictions: PWSAQuerySetA;
    const ControlFlags: Word32; Lookup: PHANDLE): Integer;
begin
  if not Assigned(WSALookupServiceBeginAProc) then
    GetSocketProc('WSALookupServiceBeginA', @WSALookupServiceBeginAProc);
  Result := WSALookupServiceBeginAProc(Restrictions, ControlFlags, Lookup);
end;

function WSALookupServiceBeginW(const Restrictions: PWSAQuerySetW;
    const ControlFlags: Word32; Lookup: PHANDLE): Integer;
begin
  if not Assigned(WSALookupServiceBeginWProc) then
    GetSocketProc('WSALookupServiceBeginW', @WSALookupServiceBeginWProc);
  Result := WSALookupServiceBeginWProc(Restrictions, ControlFlags, Lookup);
end;

function WSALookupServiceEnd(const Lookup: THandle): Integer;
begin
  if not Assigned(WSALookupServiceEndProc) then
    GetSocketProc('WSALookupServiceEnd', @WSALookupServiceEndProc);
  Result := WSALookupServiceEndProc(Lookup);
end;

function WSALookupServiceNextA(const Lookup: THandle;
    const ControlFlags: Word32; var BufferLength: Word32;
    Results: PWSAQuerySetA): Integer;
begin
  if not Assigned(WSALookupServiceNextAProc) then
    GetSocketProc('WSALookupServiceNextA', @WSALookupServiceNextAProc);
  Result := WSALookupServiceNextAProc(Lookup, ControlFlags, BufferLength,
      Results);
end;

function WSALookupServiceNextW(const Lookup: THandle;
    const ControlFlags: Word32; var BufferLength: Word32;
    Results: PWSAQuerySetW): Integer;
begin
  if not Assigned(WSALookupServiceNextWProc) then
    GetSocketProc('WSALookupServiceNextW', @WSALookupServiceNextWProc);
  Result := WSALookupServiceNextWProc(Lookup, ControlFlags, BufferLength,
      Results);
end;

function WSARemoveServiceClass(const ServiceClassId: PGUID): Integer;
begin
  if not Assigned(WSARemoveServiceClassProc) then
    GetSocketProc('WSARemoveServiceClass', @WSARemoveServiceClassProc);
  Result := WSARemoveServiceClassProc(ServiceClassId);
end;

function WSASetServiceA(const RegInfo: PWSAQuerySetA;
    const essoperation: TWSAeSetServiceOp; const ControlFlags: Word32): Integer;
begin
  if not Assigned(WSASetServiceAProc) then
    GetSocketProc('WSASetServiceA', @WSASetServiceAProc);
  Result := WSASetServiceAProc(RegInfo, essoperation, ControlFlags);
end;

function WSASetServiceW(const RegInfo: PWSAQuerySetW;
    const essoperation: TWSAeSetServiceOp; const ControlFlags: Word32): Integer;
begin
  if not Assigned(WSASetServiceWProc) then
    GetSocketProc('WSASetServiceW', @WSASetServiceWProc);
  Result := WSASetServiceWProc(RegInfo, essoperation, ControlFlags);
end;



{                                                                              }
{ Socket helpers                                                               }
{                                                                              }
function SockAvailableToRecv(const S: TSocket): Integer;
var L : Word32;
begin
  if ioctlsocket(S, FIONREAD, L) <> 0 then
    Result := 0
  else
    Result := L;
end;

procedure SetSockBlocking(const S: TSocket; const Block: Boolean);
var Mode : Word32;
begin
  if S = INVALID_SOCKET then
    raise EWinSock.Create('Invalid socket handle');
  // Set non-blocking flag on socket
  if Block then
    Mode := 0
  else
    Mode := 1;
  if IoctlSocket(S, FIONBIO, Mode) <> 0 then
    RaiseWinSockError('Blocking mode not set', WSAGetLastError);
end;



{                                                                              }
{ WinSockStartup / WinSockCleanup                                              }
{                                                                              }
const
  WinSockVer10 = $0001; // WinSock 1.0
  WinSockVer11 = $0101; // WinSock 1.1
  WinSockVer20 = $0002; // WinSock 2.0
  WinSockVer22 = $0202; // WinSock 2.2

procedure InitializeWinSock1;
begin
  IP_OPTIONS         := WS1_IP_OPTIONS;
  IP_MULTICAST_IF    := WS1_IP_MULTICAST_IF;
  IP_MULTICAST_TTL   := WS1_IP_MULTICAST_TTL;
  IP_MULTICAST_LOOP  := WS1_IP_MULTICAST_LOOP;
  IP_ADD_MEMBERSHIP  := WS1_IP_ADD_MEMBERSHIP;
  IP_DROP_MEMBERSHIP := WS1_IP_DROP_MEMBERSHIP;
  IP_TTL             := WS1_IP_TTL;
  IP_TOS             := WS1_IP_TOS;
  IP_DONTFRAGMENT    := WS1_IP_DONTFRAGMENT;
  IP_HDRINCL         := -1;
end;

procedure InitializeWinSock2;
begin
  IP_OPTIONS         := WS2_IP_OPTIONS;
  IP_MULTICAST_IF    := WS2_IP_MULTICAST_IF;
  IP_MULTICAST_TTL   := WS2_IP_MULTICAST_TTL;
  IP_MULTICAST_LOOP  := WS2_IP_MULTICAST_LOOP;
  IP_ADD_MEMBERSHIP  := WS2_IP_ADD_MEMBERSHIP;
  IP_DROP_MEMBERSHIP := WS2_IP_DROP_MEMBERSHIP;
  IP_TTL             := WS2_IP_TTL;
  IP_TOS             := WS2_IP_TOS;
  IP_DONTFRAGMENT    := WS2_IP_DONTFRAGMENT;
  IP_HDRINCL         := WS2_IP_HDRINCL;
end;

procedure WinSockStartup(const WinSock2Required: Boolean);
var ErrorCode   : Integer;
    WinSockData : WSAData;
begin
  LibLock;
  try
    if WinSockStarted then
      exit;
    FillChar(WinSockData, Sizeof(WSAData), 0);
    // Attempt WinSock 2 startup
    ErrorCode := WSAStartup(WinSockVer22, WinSockData);   // Request WinSock 2.2
    if ErrorCode = WSAVERNOTSUPPORTED then
      ErrorCode := WSAStartup(WinSockVer20, WinSockData); // Request WinSock 2.0
    // Attempt WinSock 1 startup
    if (ErrorCode = WSAVERNOTSUPPORTED) and not WinSock2Required then
      begin
        ErrorCode := WSAStartup(WinSockVer11, WinSockData);   // Request WinSock 1.1
        if ErrorCode = WSAVERNOTSUPPORTED then
          ErrorCode := WSAStartup(WinSockVer10, WinSockData); // Request WinSock 1.0
      end;
    if ErrorCode <> 0 then
      RaiseWinSockError('Winsock startup failed', ErrorCode);
    // Success
    WinSockStarted := True;
    WinSockVersion := WinSockData.Version;
    WinSock2API := Lo(WinSockVersion) >= 2;
    // WinSock version-specific initialization
    if Lo(WinSockVersion) = 1 then
      InitializeWinSock1
    else
      InitializeWinSock2;
  finally
    LibUnlock;
  end;
end;

procedure WinSockCleanup;
begin
  if not WinSockStarted then
    exit;
  WSACleanup;
  WinSockStarted := False;
end;

function IsWinSock2API: Boolean;
begin
  if not WinSockStarted then
    WinSockStartup(False);
  Result := WinSock2API;
end;



{                                                                              }
{ Test cases                                                                   }
{                                                                              }
{$IFDEF WINSOCK_TEST}
{$ASSERTIONS ON}
procedure Test;
var S : TSocket;
    U, V : Word32;
    A : sockaddr;
    AIA : PAddrInfo;
    AIW : PAddrInfoW;
begin
  Assert(Sizeof(TInAddr) = 4);
  Assert(Sizeof(TIn6Addr) = 16);
  // Start
  WinSockStartup;
  Assert(WinSockStarted);
  // ErrorMessage
  Assert(WinSockErrorMessage(0) = '');
  Assert(WinSockErrorMessage(WSAECONNRESET) <> '');
  // Socket
  S := Socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  Assert(S <> INVALID_SOCKET);
  Assert(WSAGetLastError = 0);
  // htonl, ntohl
  WSAHtonl(S, 123456, U);
  V := 0;
  WSANtohl(S, U, V);
  Assert(V = 123456);
  Assert(htonl(123456) = U);
  Assert(ntohl(U) = 123456);
  // Bind
  FillChar(A, SizeOf(A), 0);
  A.sa_family := AF_INET;
  Assert(Bind(S, A, SizeOf(A)) = 0);
  // CloseSocket
  CloseSocket(S);
  // GetAddrInfo
  AIA := nil;
  GetAddrInfo('localhost', nil, nil, AIA);
  Assert(Assigned(AIA));
  FreeAddrInfo(AIA);
  // GetAddrInfoW
  AIW := nil;
  GetAddrInfoW('localhost', nil, nil, AIW);
  Assert(Assigned(AIW));
  FreeAddrInfoW(AIW);
end;
{$ENDIF}



{                                                                              }
{ Unit initialization and finalization                                         }
{                                                                              }
initialization
  InitializeLibLock;
finalization
  WinSockCleanup;
  SocketLibraryFinalized := True;
  UnloadSocketLibrary;
  FinalizeLibLock;
end.

