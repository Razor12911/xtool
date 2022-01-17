{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals 5.00                                        }
{   File name:        flcSocketLib.pas                                         }
{   File version:     5.22                                                     }
{   Description:      Platform independent socket library.                     }
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
{   2001/12/12  0.02  Added LocalHost functions.                               }
{   2002/07/01  3.03  Revised for Fundamentals 3.                              }
{   2003/08/19  3.04  Added IP4AddressType function.                           }
{   2005/07/01  4.05  Renamed to cSocketsLib.                                  }
{   2005/07/13  4.06  Initial Unix support.                                    }
{   2005/07/14  4.07  Compilable with FreePascal 2 Win32 i386.                 }
{   2005/07/17  4.08  Minor improvements.                                      }
{   2005/12/06  4.09  Compilable with FreePascal 2.0.1 Linux i386.             }
{   2005/12/10  4.10  Revised for Fundamentals 4.                              }
{   2006/12/04  4.11  Improved Winsock 2 support.                              }
{   2006/12/14  4.12  IP6 support.                                             }
{   2007/12/29  4.13  Revision.                                                }
{   2010/09/12  4.14  Revision.                                                }
{   2014/04/23  4.15  Revision.                                                }
{   2015/04/24  4.16  SocketAddrArray help functions.                          }
{   2015/05/06  4.17  Rename IP4/IP6 address functions.                        }
{   2016/01/09  5.18  Revised for Fundamentals 5.                              }
{   2018/07/17  5.19  Type changes.                                            }
{   2018/09/09  5.20  Poll function.                                           }
{   2018/09/24  5.21  OSX changes.                                             }
{   2019/12/26  5.22  Socket TcpNoDelay option.                                }
{                                                                              }
{ Supported compilers:                                                         }
{                                                                              }
{   Delphi 2010-10.4 Win32/Win64        5.22  2020/06/02                       }
{   Delphi 10.2-10.4 Linux64            5.22  2020/06/02                       }
{   Delphi 10.2-10.4 iOS32/64           5.22  2020/06/02                       }
{   Delphi 10.2-10.4 OSX32/64           5.22  2020/06/02                       }
{   Delphi 10.2-10.4 Android32/64       5.22  2020/06/02                       }
{   FreePascal 3.0.4 Win64              5.22  2020/06/02                       }
{                                                                              }
{ References:                                                                  }
{                                                                              }
{   Microsoft Platform SDK: Windows Sockets                                    }
{   http://www.die.net/doc/linux/man/man7/socket.7.html                        }
{                                                                              }
{******************************************************************************}

{$INCLUDE ..\flcInclude.inc}

{$IFDEF DEBUG}
{$IFDEF TEST}
  {$DEFINE SOCKETLIB_TEST}
  {.DEFINE SOCKETLIB_TEST_IP6}
  {.DEFINE SOCKETLIB_TEST_IP4_INTERNET}
  {.DEFINE SOCKETLIB_TEST_OUTPUT}
{$ENDIF}
{$ENDIF}

// Socket system type
{$IFDEF MSWIN}
  {$DEFINE SOCKETLIB_WIN}
{$ELSE}
  {$IFDEF FREEPASCAL}
    {$DEFINE SOCKETLIB_POSIX_FPC}
  {$ELSE}
    {$DEFINE SOCKETLIB_POSIX_DELPHI}
  {$ENDIF}
{$ENDIF}

unit flcSocketLib;

interface

uses
  { System }
  {$IFDEF SOCKETLIB_WIN}
  Windows,
  {$ENDIF}
  SysUtils,
  { Fundamentals }
  flcStdTypes,
  flcSocketLibSys;



{                                                                              }
{ Socket structures                                                            }
{                                                                              }
type
  TIPAddressFamily = (
      iaNone,
      iaIP4,
      iaIP6);

const
  IPAddressFamilyStr: array[TIPAddressFamily] of String = (
      '',
      'IP4',
      'IP6');

function IPAddressFamilyToAF(const AddressFamily: TIPAddressFamily): Int32; {$IFDEF UseInline}inline;{$ENDIF}
function AFToIPAddressFamily(const AF: Int32): TIPAddressFamily; {$IFDEF UseInline}inline;{$ENDIF}

type
  TIPProtocol = (
      ipNone,
      ipIP,
      ipICMP,
      ipTCP,
      ipUDP,
      ipRaw);

function IPProtocolToIPPROTO(const Protocol: TIPProtocol): Int32;

type
  TIP4Addr = packed record
    case Integer of
      0 : (Addr8  : array[0..3] of Byte);
      1 : (Addr16 : array[0..1] of Word);
      2 : (Addr32 : Word32);
  end;
  PIP4Addr = ^TIP4Addr;
  TIP4AddrArray = array of TIP4Addr;
  TIP4AddrArrayArray = array of TIP4AddrArray;

const
  IP4AddrZero      : TIP4Addr = (Addr32: $00000000);
  IP4AddrLoopback  : TIP4Addr = (Addr32: $7F000001);
  IP4AddrBroadcast : TIP4Addr = (Addr32: $FFFFFFFF);
  IP4AddrNone      : TIP4Addr = (Addr32: $FFFFFFFF);

  IP4AddrStrAny       = '0.0.0.0';
  IP4AddrStrLoopback  = '127.0.0.1';
  IP4AddrStrBroadcast = '255.255.255.255';

function IP4AddrIsZero(const A: TIP4Addr): Boolean;
function IP4AddrIsNone(const A: TIP4Addr): Boolean;

type
  TIP6Addr = packed record
    case Integer of
      0 : (Addr8  : array[0..15] of Byte);
      1 : (Addr16 : array[0..7] of Word);
      2 : (Addr32 : array[0..3] of Word32);
  end;
  PIP6Addr = ^TIP6Addr;
  TIP6AddrArray = array of TIP6Addr;
  TIP6AddrArrayArray = array of TIP6AddrArray;

const
  IP6AddrStrUnspecified = '::';
  IP6AddrStrAnyHost     = '::0';
  IP6AddrStrLocalHost   = '::1';
  IP6AddrStrBroadcast   = 'ffff::1';

  IP6AddrZero : TIP6Addr = (Addr32: (0, 0, 0, 0));

function  IP6AddrIsZero(const A: TIP6Addr): Boolean;
function  IP6AddrIsLocalHost(const A: TIP6Addr): Boolean;
function  IP6AddrIsBroadcast(const A: TIP6Addr): Boolean;
function  IP6AddrIsEqual(const A, B: TIP6Addr): Boolean;
procedure IP6AddrSetZero(out A: TIP6Addr); {$IFDEF UseInline}inline;{$ENDIF}
procedure IP6AddrSetLocalHost(var A: TIP6Addr);
procedure IP6AddrSetBroadcast(var A: TIP6Addr);
procedure IP6AddrAssign(var A: TIP6Addr; const B: TIP6Addr); {$IFDEF UseInline}inline;{$ENDIF}

type
  TSocketAddr = packed record
    Port : Word; // port in host endian (not network endian)
    case AddrFamily : TIPAddressFamily of
      iaIP4 : (AddrIP4  : TIP4Addr);
      iaIP6 : (AddrIP6  : TIP6Addr;
               FlowInfo : Word32;
               ScopeID  : Word32);
  end;
  PSocketAddr = ^TSocketAddr;
  TSocketAddrArray = array of TSocketAddr;
  TSocketAddrArrayArray = array of TSocketAddrArray;

procedure InitSocketAddrNone(out Addr: TSocketAddr); {$IFDEF UseInline}inline;{$ENDIF}
function  InitSocketAddr(out SocketAddr: TSocketAddr; const Addr: TIP4Addr; const Port: Word): Integer; overload;
function  InitSocketAddr(out SocketAddr: TSocketAddr; const Addr: TIP6Addr; const Port: Word): Integer; overload;
procedure SetSocketAddrPort(var SocketAddr: TSocketAddr; const Port: Word);

function  SockAddrLen(const SockAddr: TSockAddr): Integer; {$IFDEF UseInline}inline;{$ENDIF}
function  SockAddrToSocketAddr(const Addr: TSockAddr): TSocketAddr;
function  SocketAddrToSockAddr(const Addr: TSocketAddr; out SockAddr: TSockAddr): Integer;

function  SocketAddrIPStrA(const Addr: TSocketAddr): RawByteString;
function  SocketAddrIPStr(const Addr: TSocketAddr): String;

function  SocketAddrStrA(const Addr: TSocketAddr): RawByteString;
function  SocketAddrStr(const Addr: TSocketAddr): String;

function  SocketAddrEqual(const Addr1, Addr2: TSocketAddr): Boolean;

procedure SocketAddrArrayAppend(var AddrArray: TSocketAddrArray; const Addr: TSocketAddr);
function  SocketAddrArrayGetAddrIndex(const AddrArray: TSocketAddrArray; const Addr: TSocketAddr): Integer;
function  SocketAddrArrayHasAddr(const AddrArray: TSocketAddrArray; const Addr: TSocketAddr): Boolean; {$IFDEF UseInline}inline;{$ENDIF}

type
  TSocketHost = record
    Used  : Boolean;
    Host  : RawByteString;
    Alias : array of RawByteString;
    Addr  : TSocketAddrArray;
  end;
  PSocketHost = ^TSocketHost;
  TSocketHostArray = array of TSocketHost;
  TSocketHostArrayArray = array of TSocketHostArray;

function  HostEntToSocketHost(const HostEnt: PHostEnt): TSocketHost;

type
  {$IFDEF CPU_X86_64}
  TSocketHandle = UInt64;
  {$ELSE}
  TSocketHandle = UInt32;
  {$ENDIF}
  TSocketHandleArray = array of TSocketHandle;

function  SocketHandleArrayToFDSet(const Handles: TSocketHandleArray): TFDSet;
procedure SocketHandleArrayAppend(var Handles: TSocketHandleArray; const Handle: TSocketHandle);
function  SocketHandleArrayLocate(var Handles: TSocketHandleArray; const Handle: TSocketHandle): Integer;

const
  INVALID_SOCKETHANDLE = TSocketHandle(-1);

function  AddrInfoCount(const AddrInfo: PAddrInfo; const Family: Word): Integer;



{                                                                              }
{ Socket library functions                                                     }
{                                                                              }
type
  TSocketShutdown = (ssBoth, ssSend, ssRecv);
  TSocketRecvFlag = (srfOOB, srfPeek);
  TSocketRecvFlags = set of TSocketRecvFlag;

function  SocketAccept(const S: TSocketHandle; out Addr: TSocketAddr): TSocketHandle;
function  SocketBind(const S: TSocketHandle; const Addr: TSocketAddr): Integer;
function  SocketClose(const S: TSocketHandle): Integer;
function  SocketConnect(const S: TSocketHandle; const Addr: TSocketAddr): Integer;
procedure SocketGetAddrInfo(
          const AddressFamily: TIPAddressFamily;
          const Protocol: TIPProtocol;
          const Host, Port: RawByteString;
          out Addresses: TSocketAddrArray);
function  SocketGetHostByAddr(const Addr: Pointer; const Len: Integer; const AF: Integer): TSocketHost;
function  SocketGetHostByName(const Name: Pointer): TSocketHost;
function  SocketGetHostName(const Name: PByteChar; const Len: Integer): Integer;
function  SocketGetNameInfo(const Address: TSocketAddr): RawByteString;
function  SocketGetPeerName(const S: TSocketHandle; out Name: TSocketAddr): Integer;
function  SocketGetServByName(const Name, Proto: Pointer): PServEnt;
function  SocketGetServByPort(const Port: Integer; const Proto: Pointer): PServEnt;
function  SocketGetSockName(const S: TSocketHandle; out Name: TSocketAddr): Integer;
function  SocketGetSockOpt(const S: TSocketHandle; const Level, OptName: Integer;
          const OptVal: Pointer; var OptLen: Integer): Integer;
function  Sockethtons(const HostShort: Word): Word; {$IFDEF UseInline}inline;{$ENDIF}
function  Sockethtonl(const HostLong: Word32): Word32; {$IFDEF UseInline}inline;{$ENDIF}
function  Socketinet_ntoa(const InAddr: TIP4Addr): RawByteString;
function  Socketinet_addr(const P: Pointer): TIP4Addr;
function  SocketListen(const S: TSocketHandle; const Backlog: Integer): Integer;
function  Socketntohs(const NetShort: Word): Word; {$IFDEF UseInline}inline;{$ENDIF}
function  Socketntohl(const NetLong: Word32): Word32; {$IFDEF UseInline}inline;{$ENDIF}
function  SocketsPoll(const Fd: Pointer; const FdCount: Integer; const Timeout: Integer): Integer;
function  SocketRecv(const S: TSocketHandle; var Buf; const Len: Integer; const Flags: TSocketRecvFlags): Integer; {$IFDEF UseInline}inline;{$ENDIF}
function  SocketRecvFrom(const S: TSocketHandle; var Buf; const Len: Integer; const Flags: TSocketRecvFlags;
          out From: TSocketAddr): Integer; {$IFDEF UseInline}inline;{$ENDIF}
function  SocketSelect(const nfds: Word32;
          var ReadFDS, WriteFDS, ExceptFDS: TSocketHandleArray;
          const TimeOutMicroseconds: Int64): Integer; overload;
function  SocketSelect(const S: TSocketHandle;
          var ReadSelect, WriteSelect, ExceptSelect: Boolean;
          const TimeOutMicroseconds: Int64): Integer; overload;
function  SocketSend(const S: TSocketHandle; const Buf; const Len, Flags: Integer): Integer; {$IFDEF UseInline}inline;{$ENDIF}
function  SocketSendTo(const S: TSocketHandle; const Buf; const Len, Flags: Integer;
          const AddrTo: TSocketAddr): Integer; {$IFDEF UseInline}inline;{$ENDIF}
function  SocketSetSockOpt(const S: TSocketHandle; const Level, OptName: Integer;
          const OptVal: Pointer; const OptLen: Integer): Integer;
function  SocketShutdown(const S: TSocketHandle; const How: TSocketShutdown): Integer;
function  SocketSocket(const Family: TIPAddressFamily; const Struct: Integer;
          const Protocol: TIPProtocol): TSocketHandle;



{                                                                              }
{ Socket library errors                                                        }
{                                                                              }
const
  // Error result for socket library functions
  SOCKET_ERROR = -1;

{$IFDEF SOCKETLIB_WIN}
const
  // Define Berkeley/Posix error identifiers for equivalent Windows error codes
  EINTR              = WSAEINTR;
  EBADF              = WSAEBADF;
  EACCES             = WSAEACCES;
  EFAULT             = WSAEFAULT;
  EINVAL             = WSAEINVAL;
  EMFILE             = WSAEMFILE;
  EWOULDBLOCK        = WSAEWOULDBLOCK;
  EAGAIN             = WSAEWOULDBLOCK;
  EINPROGRESS        = WSAEINPROGRESS;
  EALREADY           = WSAEALREADY;
  ENOTSOCK           = WSAENOTSOCK;
  EDESTADDRREQ       = WSAEDESTADDRREQ;
  EMSGSIZE           = WSAEMSGSIZE;
  EPROTOTYPE         = WSAEPROTOTYPE;
  ENOPROTOOPT        = WSAENOPROTOOPT;
  EPROTONOSUPPORT    = WSAEPROTONOSUPPORT;
  ESOCKTNOSUPPORT    = WSAESOCKTNOSUPPORT;
  EOPNOTSUPP         = WSAEOPNOTSUPP;
  EPFNOSUPPORT       = WSAEPFNOSUPPORT;
  EAFNOSUPPORT       = WSAEAFNOSUPPORT;
  EADDRINUSE         = WSAEADDRINUSE;
  EADDRNOTAVAIL      = WSAEADDRNOTAVAIL;
  ENETDOWN           = WSAENETDOWN;
  ENETUNREACH        = WSAENETUNREACH;
  ENETRESET          = WSAENETRESET;
  ECONNABORTED       = WSAECONNABORTED;
  ECONNRESET         = WSAECONNRESET;
  ENOBUFS            = WSAENOBUFS;
  EISCONN            = WSAEISCONN;
  ENOTCONN           = WSAENOTCONN;
  ESHUTDOWN          = WSAESHUTDOWN;
  ETOOMANYREFS       = WSAETOOMANYREFS;
  ETIMEDOUT          = WSAETIMEDOUT;
  ECONNREFUSED       = WSAECONNREFUSED;
  ELOOP              = WSAELOOP;
  ENAMETOOLONG       = WSAENAMETOOLONG;
  EHOSTDOWN          = WSAEHOSTDOWN;
  EHOSTUNREACH       = WSAEHOSTUNREACH;
  ENOTEMPTY          = WSAENOTEMPTY;
  EUSERS             = WSAEUSERS;
  EDQUOT             = WSAEDQUOT;
  ESTALE             = WSAESTALE;
  EREMOTE            = WSAEREMOTE;
  HOST_NOT_FOUND     = WSAHOST_NOT_FOUND;
  TRY_AGAIN          = WSATRY_AGAIN;
  NO_RECOVERY        = WSANO_RECOVERY;
  ENOMEM             = WSA_NOT_ENOUGH_MEMORY;
{$ENDIF}
{$IFDEF SOCKETLIB_POSIX_FPC}
const
  // Define Berkeley/Posix error identifiers for equivalent Unix error codes
  EINTR              = flcSocketLibSys.EINTR;
  EBADF              = flcSocketLibSys.EBADF;
  EACCES             = flcSocketLibSys.EACCES;
  EFAULT             = flcSocketLibSys.EFAULT;
  EINVAL             = flcSocketLibSys.EINVAL;
  EMFILE             = flcSocketLibSys.EMFILE;
  EWOULDBLOCK        = flcSocketLibSys.EWOULDBLOCK;
  EAGAIN             = flcSocketLibSys.EWOULDBLOCK;
  EINPROGRESS        = flcSocketLibSys.EINPROGRESS;
  EALREADY           = flcSocketLibSys.EALREADY;
  ENOTSOCK           = flcSocketLibSys.ENOTSOCK;
  EDESTADDRREQ       = flcSocketLibSys.EDESTADDRREQ;
  EMSGSIZE           = flcSocketLibSys.EMSGSIZE;
  EPROTOTYPE         = flcSocketLibSys.EPROTOTYPE;
  ENOPROTOOPT        = flcSocketLibSys.ENOPROTOOPT;
  EPROTONOSUPPORT    = flcSocketLibSys.EPROTONOSUPPORT;
  ESOCKTNOSUPPORT    = flcSocketLibSys.ESOCKTNOSUPPORT;
  EOPNOTSUPP         = flcSocketLibSys.EOPNOTSUPP;
  EPFNOSUPPORT       = flcSocketLibSys.EPFNOSUPPORT;
  EAFNOSUPPORT       = flcSocketLibSys.EAFNOSUPPORT;
  EADDRINUSE         = flcSocketLibSys.EADDRINUSE;
  EADDRNOTAVAIL      = flcSocketLibSys.EADDRNOTAVAIL;
  ENETDOWN           = flcSocketLibSys.ENETDOWN;
  ENETUNREACH        = flcSocketLibSys.ENETUNREACH;
  ENETRESET          = flcSocketLibSys.ENETRESET;
  ECONNABORTED       = flcSocketLibSys.ECONNABORTED;
  ECONNRESET         = flcSocketLibSys.ECONNRESET;
  ENOBUFS            = flcSocketLibSys.ENOBUFS;
  EISCONN            = flcSocketLibSys.EISCONN;
  ENOTCONN           = flcSocketLibSys.ENOTCONN;
  ESHUTDOWN          = flcSocketLibSys.ESHUTDOWN;
  ETOOMANYREFS       = flcSocketLibSys.ETOOMANYREFS;
  ETIMEDOUT          = flcSocketLibSys.ETIMEDOUT;
  ECONNREFUSED       = flcSocketLibSys.ECONNREFUSED;
  //ELOOP              = flcSocketLibSys.ELOOP;
  ENAMETOOLONG       = flcSocketLibSys.ENAMETOOLONG;
  EHOSTDOWN          = flcSocketLibSys.EHOSTDOWN;
  EHOSTUNREACH       = flcSocketLibSys.EHOSTUNREACH;
  //ENOTEMPTY          = flcSocketLibSys.ENOTEMPTY;
  //EUSERS             = flcSocketLibSys.EUSERS;
  //EDQUOT             = flcSocketLibSys.EDQUOT;
  //ESTALE             = flcSocketLibSys.ESTALE;
  //EREMOTE            = flcSocketLibSys.EREMOTE;
  //HOST_NOT_FOUND     = flcSocketLibSys.HOST_NOT_FOUND;
  //TRY_AGAIN          = flcSocketLibSys.TRY_AGAIN;
  //NO_RECOVERY        = flcSocketLibSys.NO_RECOVERY;
  //ENOMEM             = flcSocketLibSys._NOT_ENOUGH_MEMORY;
{$ENDIF}

type
  ESocketLib = class(Exception)
  protected
    FErrorCode : Integer;

  public
    constructor Create(const Msg: String; const ErrorCode: Integer = 0);
    constructor CreateFmt(const Msg: String; const Args: array of const;
                const ErrorCode: Integer = 0);

    property ErrorCode: Integer read FErrorCode;
  end;

function  SocketGetLastError: Integer;
function  SocketGetErrorMessage(const ErrorCode: Integer): String;



{                                                                              }
{ IP addresses                                                                 }
{   IsIPAddress returns True if Address is a valid IP address. NetAddress      }
{   contains the address in network byte order.                                }
{   IsInternetIP returns True if Address appears to be an Internet IP.         }
{                                                                              }
type
  TIP4AddressType = (
      inaPublic,
      inaPrivate,
      inaNone,
      inaReserved,
      inaLoopback,
      inaLinkLocalNetwork,
      inaTestNetwork,
      inaMulticast,
      inaBroadcast);

function  IsIP4AddressB(const Address: RawByteString; out NetAddress: TIP4Addr): Boolean;
function  IsIP6AddressB(const Address: RawByteString; out NetAddress: TIP6Addr): Boolean;

function  IsIP4AddressU(const Address: UnicodeString; out NetAddress: TIP4Addr): Boolean;
{$IFDEF SOCKETLIB_WIN}
function  IsIP6AddressU(const Address: UnicodeString; out NetAddress: TIP6Addr): Boolean;
{$ENDIF}

function  IsIP4Address(const Address: String; out NetAddress: TIP4Addr): Boolean;
{$IFDEF SOCKETLIB_WIN}
function  IsIP6Address(const Address: String; out NetAddress: TIP6Addr): Boolean;
{$ENDIF}

function  IP4AddressStrB(const Address: TIP4Addr): RawByteString;
function  IP6AddressStrB(const Address: TIP6Addr): RawByteString;

function  IP4AddressStr(const Address: TIP4Addr): String;
function  IP6AddressStr(const Address: TIP6Addr): String;

function  IP4AddressType(const Address: TIP4Addr): TIP4AddressType;
function  IsPrivateIP4Address(const Address: TIP4Addr): Boolean;
function  IsInternetIP4Address(const Address: TIP4Addr): Boolean;
procedure SwapIP4Endian(var Address: TIP4Addr);



{                                                                              }
{ Port constants                                                               }
{                                                                              }
const
  // IP ports
  IPPORT_ECHO       = 7;
  IPPORT_DISCARD    = 9;
  IPPORT_DAYTIME    = 13;
  IPPORT_QOTD       = 17;
  IPPORT_FTPDATA    = 20;
  IPPORT_FTP        = 21;
  IPPORT_SSH        = 22;
  IPPORT_TELNET     = 23;
  IPPORT_SMTP       = 25;
  IPPORT_TIMESERVER = 37;
  IPPORT_NAMESERVER = 42;
  IPPORT_WHOIS      = 43;
  IPPORT_GOPHER     = 70;
  IPPORT_FINGER     = 79;
  IPPORT_HTTP       = 80;
  IPPORT_POP3       = 110;
  IPPORT_IDENT      = 113;
  IPPORT_NNTP       = 119;
  IPPORT_NTP        = 123;
  IPPORT_HTTPS      = 443;
  IPPORT_SSMTP      = 465;
  IPPORT_SNNTP      = 563;

  // IP port names
  IPPORTSTR_FTP    = 'ftp';
  IPPORTSTR_SSH    = 'ssh';
  IPPORTSTR_TELNET = 'telnet';
  IPPORTSTR_SMTP   = 'smtp';
  IPPORTSTR_HTTP   = 'http';
  IPPORTSTR_POP3   = 'pop3';
  IPPORTSTR_NNTP   = 'nntp';



{                                                                              }
{ HostEnt decoding                                                             }
{                                                                              }
function  HostEntAddressesCount(const HostEnt: PHostEnt): Integer;
function  HostEntAddresses(const HostEnt: PHostEnt): TIP4AddrArray;
function  HostEntAddress(const HostEnt: PHostEnt; const Index: Integer): TSocketAddr;
function  HostEntAddressIP4(const HostEnt: PHostEnt; const Index: Integer = 0): TIP4Addr;
function  HostEntAddressStr(const HostEnt: PHostEnt; const Index: Integer = 0): RawByteString;
function  HostEntName(const HostEnt: PHostEnt): RawByteString;



{                                                                              }
{ IP protocol                                                                  }
{   Enumeration of IP protocols.                                               }
{                                                                              }
function  IPProtocolToStrB(const Protocol: TIPProtocol): RawByteString;
function  StrToIPProtocolB(const Protocol: RawByteString): TIPProtocol;

function  IPProtocolToStr(const Protocol: TIPProtocol): String;
function  StrToIPProtocol(const Protocol: String): TIPProtocol;



{                                                                              }
{ Local host                                                                   }
{                                                                              }
type
  AddressStrArrayB = Array of RawByteString;
  AddressStrArray = Array of String;

function  LocalHostNameB: RawByteString;
function  LocalHostName: String;

function  LocalIPAddresses: TIP4AddrArray;
function  LocalIP6Addresses: TIP6AddrArray;

function  LocalIP4AddressesStrB: AddressStrArrayB;
function  LocalIP6AddressesStrB: AddressStrArrayB;

function  LocalIP4AddressesStr: AddressStrArray;
function  LocalIP6AddressesStr: AddressStrArray;

function  GuessInternetIP4: TIP4Addr;
function  GuessInternetIP4StrB: RawByteString;
function  GuessInternetIP4Str: String;



{                                                                              }
{ Remote host                                                                  }
{   Reverse name lookup (domain name from IP address).                         }
{   Blocks. Raises an exception if unsuccessful.                               }
{                                                                              }
function  GetRemoteHostNameB(const Address: TSocketAddr): RawByteString; overload;
function  GetRemoteHostNameB(const Address: TIP4Addr): RawByteString; overload;
function  GetRemoteHostNameB(const Address: TIP6Addr): RawByteString; overload;

function  GetRemoteHostName(const Address: TSocketAddr): String; overload;
function  GetRemoteHostName(const Address: TIP4Addr): String; overload;
function  GetRemoteHostName(const Address: TIP6Addr): String; overload;



{                                                                              }
{ Resolve host                                                                 }
{   Resolves Host (IP or domain name).                                         }
{   Blocks. Raises an exception if unsuccessful.                               }
{                                                                              }
function  ResolveHostExB(const Host: RawByteString; const AddressFamily: TIPAddressFamily): TSocketAddrArray;
function  ResolveHostB(const Host: RawByteString; const AddressFamily: TIPAddressFamily): TSocketAddr;

function  ResolveHostEx(const Host: String; const AddressFamily: TIPAddressFamily): TSocketAddrArray;
function  ResolveHost(const Host: String; const AddressFamily: TIPAddressFamily): TSocketAddr;

function  ResolveHostIP4ExB(const Host: RawByteString): TIP4AddrArray;
function  ResolveHostIP4B(const Host: RawByteString): TIP4Addr;

function  ResolveHostIP4Ex(const Host: String): TIP4AddrArray;
function  ResolveHostIP4(const Host: String): TIP4Addr;

function  ResolveHostIP6ExB(const Host: RawByteString): TIP6AddrArray;
function  ResolveHostIP6B(const Host: RawByteString): TIP6Addr;

function  ResolveHostIP6Ex(const Host: String): TIP6AddrArray;
function  ResolveHostIP6(const Host: String): TIP6Addr;



{                                                                              }
{ Port                                                                         }
{   NetPort is the Port value in network byte order.                           }
{   ResolvePort returns the NetPort.                                           }
{                                                                              }
function  ResolvePortB(const Port: RawByteString; const Protocol: TIPProtocol): Word;
function  ResolvePort(const Port: String; const Protocol: TIPProtocol): Word;

function  NetPortToPort(const NetPort: Word): Word;
function  NetPortToPortStr(const NetPort: Word): String;
function  NetPortToPortStrB(const NetPort: Word): RawByteString;

function  PortToNetPort(const Port: Word): Word;



{                                                                              }
{ Resolve host and port                                                        }
{                                                                              }
function  ResolveB(
          const Host: RawByteString; const Port: Integer;
          const AddressFamily: TIPAddressFamily = iaIP4;
          const Protocol: TIPProtocol = ipTCP): TSocketAddr; overload;
function  ResolveB(
          const Host, Port: RawByteString;
          const AddressFamily: TIPAddressFamily = iaIP4;
          const Protocol: TIPProtocol = ipTCP): TSocketAddr; overload;

function  Resolve(
          const Host: String; const Port: Integer;
          const AddressFamily: TIPAddressFamily = iaIP4;
          const Protocol: TIPProtocol = ipTCP): TSocketAddr; overload;
function  Resolve(
          const Host, Port: String;
          const AddressFamily: TIPAddressFamily = iaIP4;
          const Protocol: TIPProtocol = ipTCP): TSocketAddr; overload;



{                                                                              }
{ Socket handle                                                                }
{   AllocateSocketHandle returns a handle to a new socket.                     }
{   Raises an exception if allocation failed.                                  }
{                                                                              }
function  AllocateSocketHandle(
          const AddressFamily: TIPAddressFamily;
          const Protocol: TIPProtocol;
          const Overlapped: Boolean = False): TSocketHandle;



{                                                                              }
{ Socket options                                                               }
{                                                                              }
function  GetSocketReceiveTimeout(const SocketHandle: TSocketHandle): Integer;
procedure SetSocketReceiveTimeout(const SocketHandle: TSocketHandle;
          const TimeoutUs: Integer);
function  GetSocketSendTimeOut(const SocketHandle: TSocketHandle): Integer;
procedure SetSocketSendTimeout(const SocketHandle: TSocketHandle;
          const TimeoutUs: Integer);
function  GetSocketReceiveBufferSize(const SocketHandle: TSocketHandle): Integer;
procedure SetSocketReceiveBufferSize(const SocketHandle: TSocketHandle; const BufferSize: Integer);
function  GetSocketSendBufferSize(const SocketHandle: TSocketHandle): Integer;
procedure SetSocketSendBufferSize(const SocketHandle: TSocketHandle; const BufferSize: Integer);
{$IFDEF SOCKETLIB_WIN}
procedure GetSocketLinger(const SocketHandle: TSocketHandle;
          var Linger: Boolean; var LingerTimeSec: Integer);
procedure SetSocketLinger(const SocketHandle: TSocketHandle;
          const Linger: Boolean; const LingerTimeSec: Integer = 0);
{$ENDIF}
function  GetSocketBroadcast(const SocketHandle: TSocketHandle): Boolean;
procedure SetSocketBroadcast(const SocketHandle: TSocketHandle;
          const Broadcast: Boolean);
{$IFDEF SOCKETLIB_WIN}
function  GetSocketMulticastTTL(const SocketHandle: TSocketHandle): Integer;
procedure SetSocketMulticastTTL(const SocketHandle: TSocketHandle; const TTL: Integer);
{$ENDIF}
function  GetSocketTcpNoDelay(const SocketHandle: TSocketHandle): Boolean;
procedure SetSocketTcpNoDelay(const SocketHandle: TSocketHandle; const TcpNoDelay: Boolean);



{                                                                              }
{ Socket mode                                                                  }
{                                                                              }
procedure SetSocketBlocking(const SocketHandle: TSocketHandle; const Block: Boolean);

{$IFDEF SOCKETLIB_WIN}
type
  TSocketAsynchronousEvent = (
    saeConnect,
    saeClose,
    saeRead,
    saeWrite,
    saeAccept);
  TSocketAsynchronousEvents = set of TSocketAsynchronousEvent;

function  SocketAsynchronousEventsToEvents(const Events: TSocketAsynchronousEvents): Int32;
function  EventsToSocketAsynchronousEvents(const Events: Int32): TSocketAsynchronousEvents;

procedure SetSocketAsynchronous(
          const SocketHandle: TSocketHandle;
          const WindowHandle: HWND;
          const Msg: Integer;
          const Events: TSocketAsynchronousEvents);
{$ENDIF}



{                                                                              }
{ Socket helpers                                                               }
{                                                                              }
function  GetSocketAvailableToRecv(const SocketHandle: TSocketHandle): Integer;



{                                                                              }
{ Test cases                                                                   }
{                                                                              }
{$IFDEF SOCKETLIB_TEST}
procedure Test;
{$ENDIF}



implementation

uses
  { System }
  {$IFDEF SOCKETLIB_POSIX_FPC}
  dynlibs,
  {$ENDIF}
  SyncObjs,
  { Fundamentals }
  flcUtils;



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
{ Helper functions                                                             }
{                                                                              }
function StrZLenB(const S: Pointer): Integer;
var P : PByteChar;
begin
  if not Assigned(S) then
    Result := 0
  else
    begin
      Result := 0;
      P := S;
      while Ord(P^) <> 0 do
        begin
          Inc(Result);
          Inc(P);
        end;
    end;
end;

function StrZPasB(const A: Pointer): RawByteString;
var I, L : Integer;
    P : PByteChar;
begin
  L := StrZLenB(A);
  SetLength(Result, L);
  if L = 0 then
    exit;
  I := 0;
  P := A;
  while I < L do
    begin
      Result[I + 1] := P^;
      Inc(I);
      Inc(P);
    end;
end;



{                                                                              }
{ Socket structure routines                                                    }
{                                                                              }
function IPAddressFamilyToAF(const AddressFamily: TIPAddressFamily): Int32;
begin
  case AddressFamily of
    iaIP4 : Result := AF_INET;
    iaIP6 : Result := AF_INET6;
  else
    Result := AF_UNSPEC;
  end;
end;

function AFToIPAddressFamily(const AF: Int32): TIPAddressFamily;
begin
  case AF of
    AF_INET  : Result := iaIP4;
    AF_INET6 : Result := iaIP6;
  else
    Result := iaNone;
  end;
end;

function IPProtocolToIPPROTO(const Protocol: TIPProtocol): Int32;
begin
  case Protocol of
    ipIP   : Result := IPPROTO_IP;
    ipICMP : Result := IPPROTO_ICMP;
    ipTCP  : Result := IPPROTO_TCP;
    ipUDP  : Result := IPPROTO_UDP;
    ipRaw  : Result := IPPROTO_RAW;
  else
    Result := -1;
  end;
end;

function IP4AddrIsZero(const A: TIP4Addr): Boolean;
begin
  Result := A.Addr32 = IP4AddrZero.Addr32;
end;

function IP4AddrIsNone(const A: TIP4Addr): Boolean;
begin
  Result := A.Addr32 = IP4AddrNone.Addr32;
end;

function IP6AddrIsZero(const A: TIP6Addr): Boolean;
begin
  Result := (A.Addr32[0] = $00000000) and
            (A.Addr32[1] = $00000000) and
            (A.Addr32[2] = $00000000) and
            (A.Addr32[3] = $00000000);
end;

function IP6AddrIsLocalHost(const A: TIP6Addr): Boolean;
begin
  Result := (A.Addr32[0] = $00000000) and
            (A.Addr32[1] = $00000000) and
            (A.Addr32[2] = $00000000) and
            (A.Addr32[3] = $01000000);
end;

function IP6AddrIsBroadcast(const A: TIP6Addr): Boolean;
begin
  Result := (A.Addr32[0] = $0000FFFF) and
            (A.Addr32[1] = $00000000) and
            (A.Addr32[2] = $00000000) and
            (A.Addr32[3] = $01000000);
end;

function IP6AddrIsEqual(const A, B: TIP6Addr): Boolean;
begin
  Result := (A.Addr32[0] = B.Addr32[0]) and
            (A.Addr32[1] = B.Addr32[1]) and
            (A.Addr32[2] = B.Addr32[2]) and
            (A.Addr32[3] = B.Addr32[3]);
end;

procedure IP6AddrSetZero(out A: TIP6Addr);
begin
  A.Addr32[0] := 0;
  A.Addr32[1] := 0;
  A.Addr32[2] := 0;
  A.Addr32[3] := 0;
end;

procedure IP6AddrSetLocalHost(var A: TIP6Addr);
begin
  A.Addr32[0] := $00000000;
  A.Addr32[1] := $00000000;
  A.Addr32[2] := $00000000;
  A.Addr32[3] := $01000000;
end;

procedure IP6AddrSetBroadcast(var A: TIP6Addr);
begin
  A.Addr32[0] := $0000FFFF;
  A.Addr32[1] := $00000000;
  A.Addr32[2] := $00000000;
  A.Addr32[3] := $01000000;
end;

procedure IP6AddrAssign(var A: TIP6Addr; const B: TIP6Addr);
begin
  Move(B, A, Sizeof(TIP6Addr));
end;

procedure InitSocketAddrNone(out Addr: TSocketAddr);
begin
  FillChar(Addr, SizeOf(TSocketAddr), 0);
  Addr.AddrFamily := iaNone;
end;

function InitSocketAddr(out SocketAddr: TSocketAddr; const Addr: TIP4Addr; const Port: Word): Integer;
begin
  InitSocketAddrNone(SocketAddr);
  SocketAddr.AddrFamily := iaIP4;
  SocketAddr.Port       := Port;
  SocketAddr.AddrIP4    := Addr;
  Result := Sizeof(TSocketAddr);
end;

function InitSocketAddr(out SocketAddr: TSocketAddr; const Addr: TIP6Addr; const Port: Word): Integer;
begin
  InitSocketAddrNone(SocketAddr);
  SocketAddr.AddrFamily := iaIP6;
  SocketAddr.Port       := Port;
  IP6AddrAssign(SocketAddr.AddrIP6, Addr);
  Result := Sizeof(TSocketAddr);
end;

procedure SetSocketAddrPort(var SocketAddr: TSocketAddr; const Port: Word);
begin
  SocketAddr.Port := Port;
end;

function SockAddrLen(const SockAddr: TSockAddr): Integer;
begin
  {$IFDEF SOCKETLIB_POSIX_DELPHI}
  case SockAddr.ss_family of
  {$ELSE}
  case SockAddr.sa_family of
  {$ENDIF}
    AF_INET  : Result := Sizeof(TSockAddrIn);
    AF_INET6 : Result := Sizeof(TSockAddrIn6);
  else
    Result := 0;
  end;
end;

function SockAddrToSocketAddr(const Addr: TSockAddr): TSocketAddr;
var
  AddrIn : PSockAddrIn;
  AddrIn6 : PSockAddrIn6;
begin
  {$IFDEF SOCKETLIB_POSIX_DELPHI}
  case Addr.ss_family of
  {$ELSE}
  case Addr.sa_family of
  {$ENDIF}
    AF_INET :
    begin
      AddrIn := @Addr;
      Result.AddrFamily := iaIP4;
      Result.Port := NetPortToPort(AddrIn^.sin_port);
      Result.AddrIP4.Addr32 := AddrIn^.sin_addr.S_addr;
    end;
    AF_INET6 :
    begin
      AddrIn6 := @Addr;
      Result.AddrFamily := iaIP6;
      Result.Port := NetPortToPort(AddrIn6^.sin6_port);
      Move(AddrIn6.sin6_addr, Result.AddrIP6, SizeOf(TIP6Addr));
    end;
  else
    // raise ESocketLib.Create('Address family not supported', -1);   //// 2020/05/05
    FillChar(Result, SizeOf(Result), 0);
    Result.AddrFamily := iaNone;
  end;
end;

// Returns size used in SockAddr structure
function SocketAddrToSockAddr(const Addr: TSocketAddr; out SockAddr: TSockAddr): Integer;
var
  AddrIn : PSockAddrIn;
  AddrIn6 : PSockAddrIn6;
begin
  case Addr.AddrFamily of
    iaIP4 :
    begin
      AddrIn := @SockAddr;
      FillChar(AddrIn^.sin_zero, SizeOf(AddrIn^.sin_zero), 0);
      {$IFDEF OSX}
      AddrIn^.sin_len := SizeOf(TSockAddrIn);
      {$ENDIF}
      {$IFDEF SOCKETLIB_POSIX_DELPHI}
      AddrIn^.sin_family := AF_INET;
      {$ELSE}
      AddrIn^.sa_family := AF_INET;
      {$ENDIF}
      AddrIn^.sin_port := PortToNetPort(Addr.Port);
      AddrIn^.sin_addr.S_addr := Addr.AddrIP4.Addr32;
      Result := SizeOf(TSockAddrIn);
    end;
    iaIP6 :
    begin
      AddrIn6 := @SockAddr;
      FillChar(AddrIn6^, SizeOf(TSockAddrIn6), 0);
      {$IFDEF OSX}
      AddrIn6^.sin6_len := SizeOf(TSockAddrIn6);
      {$ENDIF}
      AddrIn6^.sin6_family := AF_INET6;
      AddrIn6^.sin6_port := PortToNetPort(Addr.Port);
      Move(Addr.AddrIP6.Addr32[0], AddrIn6^.sin6_addr, 16);
      Result := SizeOf(TSockAddrIn6);
    end;
  else
    begin
      {$IFDEF SOCKETLIB_POSIX_DELPHI}
      SockAddr.ss_family := AF_UNSPEC;
      {$ELSE}
      SockAddr.sa_family := AF_UNSPEC;
      {$ENDIF}
      Result := 0;
    end;
  end;
end;

function SocketAddrIPStrA(const Addr: TSocketAddr): RawByteString;
begin
  case Addr.AddrFamily of
    iaIP4 : Result := IP4AddressStrB(Addr.AddrIP4);
    iaIP6 : Result := IP6AddressStrB(Addr.AddrIP6);
  else
    Result := '';
  end;
end;

function SocketAddrIPStr(const Addr: TSocketAddr): String;
begin
  case Addr.AddrFamily of
    iaIP4 : Result := IP4AddressStr(Addr.AddrIP4);
    iaIP6 : Result := IP6AddressStr(Addr.AddrIP6);
  else
    Result := '';
  end;
end;

function SocketAddrStrA(const Addr: TSocketAddr): RawByteString;
begin
  Result := SocketAddrIPStrA(Addr) + ':' + RawByteString(IntToStr(Addr.Port));
end;

function SocketAddrStr(const Addr: TSocketAddr): String;
begin
  Result := Format('%s:%d', [SocketAddrIPStr(Addr), Addr.Port]);
end;

function SocketAddrEqual(const Addr1, Addr2: TSocketAddr): Boolean;
begin
  if Addr1.AddrFamily <> Addr2.AddrFamily then
    Result := False
  else
  if Addr1.Port <> Addr2.Port then
    Result := False
  else
  case Addr1.AddrFamily of
    iaIP4 : Result := Addr1.AddrIP4.Addr32 = Addr2.AddrIP4.Addr32;
    iaIP6 : Result := IP6AddrIsEqual(Addr1.AddrIP6, Addr2.AddrIP6)
  else
    Result := False;
  end;
end;

procedure SocketAddrArrayAppend(var AddrArray: TSocketAddrArray; const Addr: TSocketAddr);
var L : Integer;
begin
  L := Length(AddrArray);
  SetLength(AddrArray, L + 1);
  AddrArray[L] := Addr;
end;

function SocketAddrArrayGetAddrIndex(const AddrArray: TSocketAddrArray; const Addr: TSocketAddr): Integer;
var I : Integer;
begin
  for I := 0 to Length(AddrArray) - 1 do
    if SocketAddrEqual(AddrArray[I], Addr) then
      begin
        Result := I;
        exit;
      end;
  Result := -1;
end;

function SocketAddrArrayHasAddr(const AddrArray: TSocketAddrArray; const Addr: TSocketAddr): Boolean;
begin
  Result := SocketAddrArrayGetAddrIndex(AddrArray, Addr) >= 0;
end;

function HostEntToSocketHost(const HostEnt: PHostEnt): TSocketHost;
var C, I : Integer;
begin
  if not Assigned(HostEnt) then
    begin
      Result.Used := False;
      Result.Host := '';
      Result.Alias := nil;
      Result.Addr := nil;
      exit;
    end;
  Result.Used := True;
  Result.Host := HostEntName(HostEnt);
  C := HostEntAddressesCount(HostEnt);
  SetLength(Result.Addr, C);
  for I := 0 to C - 1 do
    Result.Addr[I] := HostEntAddress(HostEnt, I);
end;

function SocketHandleArrayToFDSet(const Handles: TSocketHandleArray): TFDSet;
var I : Integer;
begin
  FD_ZERO(Result);
  for I := 0 to Length(Handles) - 1 do
    FD_SET(Handles[I], Result);
end;

{$IFDEF SOCKETLIB_WIN}
function FDSetToSocketHandleArray(const FDSet: TFDSet): TSocketHandleArray;
var I, L : Integer;
begin
  Result := nil;
  L := FD_COUNT(FDSet);
  SetLength(Result, L);
  for I := 0 to L - 1 do
    Result[I] := FDSet.fd_array[I];
end;
{$ENDIF}
{$IFDEF SOCKETLIB_POSIX_FPC}
function FDSetToSocketHandleArray(const FDSet: TFDSet): TSocketHandleArray;
var I, L, J : Integer;
    F : Int32;
begin
  Result := nil;
  L := FD_COUNT(FDSet);
  SetLength(Result, L);
  for I := 0 to FD_ARRAYSIZE - 1 do
    if FDSet.fds_bits[I] <> 0 then
      begin
        F := I * NFDBITS;
        for J := 0 to NFDBITS - 1 do
          begin
            if FD_ISSET(F, FDSet) then
              SocketHandleArrayAppend(Result, F);
            Inc(F);
          end;
      end;
end;
{$ENDIF}
{$IFDEF SOCKETLIB_POSIX_DELPHI}
function FDSetToSocketHandleArray(const FDSet: TFDSet): TSocketHandleArray;
var I, L, J : Integer;
    F : Int32;
begin
  Result := nil;
  L := FD_COUNT(FDSet);
  SetLength(Result, L);
  for I := 0 to FD_SETSIZE - 1 do
    if FDSet.fds_bits[I] <> 0 then
      begin
        F := I * NFDBITS;
        for J := 0 to NFDBITS - 1 do
          begin
            if FD_ISSET(F, FDSet) then
              SocketHandleArrayAppend(Result, F);
            Inc(F);
          end;
      end;
end;
{$ENDIF}

procedure SocketHandleArrayAppend(var Handles: TSocketHandleArray; const Handle: TSocketHandle);
var L : Integer;
begin
  L := Length(Handles);
  SetLength(Handles, L + 1);
  Handles[L] := Handle;
end;

function SocketHandleArrayLocate(var Handles: TSocketHandleArray; const Handle: TSocketHandle): Integer;
var I : Integer;
begin
  for I := 0 to Length(Handles) - 1 do
    if Handles[I] = Handle then
      begin
        Result := I;
        exit;
      end;
  Result := -1;
end;

function AddrInfoCount(const AddrInfo: PAddrInfo; const Family: Word): Integer;
var CurrAddr : PAddrInfo;
    Found : Integer;
    SockAddr : PSockAddr;
begin
  CurrAddr := AddrInfo;
  Found := 0;
  while Assigned(CurrAddr) do
    begin
      SockAddr := CurrAddr^.ai_addr;
      if Assigned(SockAddr) and (SockAddr^.sa_family = Family) then
        Inc(Found);
      CurrAddr := CurrAddr^.ai_next;
    end;
  Result := Found;
end;



{                                                                              }
{ Socket library functions                                                     }
{                                                                              }
function SocketAccept(const S: TSocketHandle; out Addr: TSocketAddr): TSocketHandle;
var AAddrLen : TSockLen;
    AAddr    : TSockAddr;
    ASocket  : TSocket;
begin
  AAddrLen := SizeOf(TSockAddr);
  FillChar(AAddr, SizeOf(TSockAddr), 0);
  ASocket := Accept(TSocket(S), @AAddr, AAddrLen);
  if (ASocket <> INVALID_SOCKET) and (AAddrLen > 0) then
    begin
      Addr := SockAddrToSocketAddr(AAddr);
      Result := TSocketHandle(ASocket);
    end
  else
    begin
      InitSocketAddrNone(Addr);
      Result := INVALID_SOCKETHANDLE;
    end;
end;

function SocketBind(const S: TSocketHandle; const Addr: TSocketAddr): Integer;
var SockAddr : TSockAddr;
    SockAddrLen : Integer;
begin
  SockAddrLen := SocketAddrToSockAddr(Addr, SockAddr);
  Result := Bind(TSocket(S), SockAddr, SockAddrLen);
end;

function SocketClose(const S: TSocketHandle): Integer;
begin
  Result := CloseSocket(TSocket(S));
end;

function SocketConnect(const S: TSocketHandle; const Addr: TSocketAddr): Integer;
var SockAddr : TSockAddr;
    SockAddrLen : Integer;
begin
  SockAddrLen := SocketAddrToSockAddr(Addr, SockAddr);
  Result := Connect(TSocket(S), @SockAddr, SockAddrLen);
end;

procedure SocketGetAddrInfo(
          const AddressFamily: TIPAddressFamily;
          const Protocol: TIPProtocol;
          const Host, Port: RawByteString;
          out Addresses: TSocketAddrArray);
var Hints    : TAddrInfo;
    AddrInfo : PAddrInfo;
    Error    : Integer;
    CurrAddr : PAddrInfo;
    Found    : Integer;
    AddrIdx  : Integer;
    SockAddr : PSockAddr;
    QHost    : PByteChar;
    QPort    : PByteChar;
begin
  // Initialize Hints for GetAddrInfo
  FillChar(Hints, Sizeof(TAddrInfo), 0);
  Hints.ai_family := IPAddressFamilyToAF(AddressFamily);
  if Hints.ai_family = AF_UNSPEC then
    raise ESocketLib.Create('Invalid address family');
  Hints.ai_protocol := IPProtocolToIPPROTO(Protocol);
  // GetAddrInfo
  AddrInfo := nil;
  if Host = '' then
    QHost := nil
  else
    QHost := PByteChar(Host);
  if Port = '' then
    QPort := nil
  else
    QPort := PByteChar(Port);
  {$IFDEF SOCKETLIB_WIN}
  if not WinSockStarted then
    WinSockStartup;
  {$ENDIF}
  Error := GetAddrInfo(Pointer(QHost), Pointer(QPort), @Hints, AddrInfo);
  if Error <> 0 then
    raise ESocketLib.Create('Lookup failed', SocketGetLastError);
  try
    // Count number of results
    Found := AddrInfoCount(AddrInfo, Hints.ai_family);
    if Found = 0 then
      // No results returned
      Addresses := nil
    else
      begin
        // Populate results
        SetLength(Addresses, Found);
        for AddrIdx := 0 to Found - 1 do
          InitSocketAddrNone(Addresses[AddrIdx]);
        CurrAddr := AddrInfo;
        AddrIdx := 0;
        while Assigned(CurrAddr) do
          begin
            SockAddr := CurrAddr^.ai_addr;
            if Assigned(SockAddr) and (SockAddr^.sa_family = Hints.ai_family) then
              begin
                {$IFDEF SOCKETLIB_POSIX_DELPHI}
                Addresses[AddrIdx] := SockAddrToSocketAddr(PTSockAddr(SockAddr)^);
                {$ELSE}
                Addresses[AddrIdx] := SockAddrToSocketAddr(SockAddr^);
                {$ENDIF}
                Inc(AddrIdx);
                if AddrIdx = Found then // last result
                  break;
              end;
            CurrAddr := CurrAddr^.ai_next;
          end;
      end;
  finally
    // Release resources allocated by GetAddrInfo
    FreeAddrInfo(AddrInfo);
  end;
end;

function SocketGetHostByAddr(const Addr: Pointer; const Len: Integer; const AF: Integer): TSocketHost;
var HostEnt : PHostEnt;
begin
  HostEnt := GetHostByAddr(Addr, Len, AF);
  Result := HostEntToSocketHost(HostEnt);
end;

function SocketGetHostByName(const Name: Pointer): TSocketHost;
var HostEnt : PHostEnt;
begin
  HostEnt := GetHostByName(Name);
  Result := HostEntToSocketHost(HostEnt);
end;

function SocketGetHostName(const Name: PByteChar; const Len: Integer): Integer;
begin
  Result := GetHostName(Pointer(Name), Len);
end;

function SocketGetNameInfo(const Address: TSocketAddr): RawByteString;
var Hints   : TAddrInfo;
    Host    : Array[0..NI_MAXHOST] of AnsiChar;
    Serv    : Array[0..NI_MAXSERV] of AnsiChar;
    Error   : Integer;
    Addr    : TSockAddr;
    AddrLen : Integer;
begin
  AddrLen := SocketAddrToSockAddr(Address, Addr);
  FillChar(Hints, Sizeof(TAddrInfo), 0);
  {$IFDEF SOCKETLIB_POSIX_DELPHI}
  Hints.ai_family := Addr.ss_family;
  {$ELSE}
  Hints.ai_family := Addr.sa_family;
  {$ENDIF}
  FillChar(Host, Sizeof(Host), 0);
  FillChar(Serv, Sizeof(Serv), 0);
  {$IFDEF SOCKETLIB_WIN}
  if not WinSockStarted then
    WinSockStartup;
  {$ENDIF}
  Error := GetNameInfo(@Addr, AddrLen, @Host, NI_MAXHOST,
      @Serv, NI_MAXSERV, NI_NUMERICSERV);
  if Error <> 0 then
    raise ESocketLib.Create('Reverse lookup failed', SocketGetLastError);
  Result := StrZPasB(PByteChar(@Host));
end;

function SocketGetPeerName(const S: TSocketHandle; out Name: TSocketAddr): Integer;
var Addr : TSockAddr;
    L : TSockLen;
begin
  L := SizeOf(Addr);
  Result := GetPeerName(TSocket(S), Addr, L);
  Name := SockAddrToSocketAddr(Addr);
end;

function SocketGetServByName(const Name, Proto: Pointer): PServEnt;
begin
  Result := GetServByName(Name, Proto);
end;

function SocketGetServByPort(const Port: Integer; const Proto: Pointer): PServEnt;
begin
  Result := GetServByPort(Port, Proto);
end;

function SocketGetSockName(const S: TSocketHandle; out Name: TSocketAddr): Integer;
var Addr : TSockAddr;
    L : TSockLen;
begin
  L := SizeOf(Addr);
  Result := GetSockName(S, Addr, L);
  Name := SockAddrToSocketAddr(Addr);
end;

function SocketGetSockOpt(const S: TSocketHandle; const Level, OptName: Integer;
         const OptVal: Pointer; var OptLen: Integer): Integer;
var
  OptLenT : TSockLen;
begin
  FillChar(OptVal^, OptLen, 0);
  OptLenT := OptLen;
  Result := GetSockOpt(S, Level, OptName, OptVal, OptLenT);
  OptLen := OptLenT;
end;

function Sockethtons(const HostShort: Word): Word;
begin
  Result := htons(HostShort);
end;

function Sockethtonl(const HostLong: Word32): Word32;
begin
  Result := htonl(HostLong);
end;

function Socketinet_ntoa(const InAddr: TIP4Addr): RawByteString;
var A : TInAddr;
begin
  A.S_addr := InAddr.Addr32;
  Result := StrZPasB(Pointer(inet_ntoa(A)));
end;

function Socketinet_addr(const P: Pointer): TIP4Addr;
begin
  Result.Addr32 := inet_addr(P);
end;

function SocketListen(const S: TSocketHandle; const Backlog: Integer): Integer;
begin
  Result := Listen(S, Backlog);
end;

function Socketntohs(const NetShort: Word): Word;
begin
  Result := ntohs(NetShort);
end;

function Socketntohl(const NetLong: Word32): Word32;
begin
  Result := ntohl(NetLong);
end;

function SocketsPoll(const Fd: Pointer; const FdCount: Integer; const Timeout: Integer): Integer;
begin
  {$IFDEF SOCKETLIB_WIN}
  Result := WSAPoll(Fd, FdCount, Timeout);
  {$ELSE}
  Result := Poll(Fd, FdCount, Timeout);
  {$ENDIF}
end;

function SocketRecvFlagsToFlags(const Flags: TSocketRecvFlags): Int32; {$IFDEF UseInline}inline;{$ENDIF}
var F : Int32;
begin
  F := 0;
  if srfOOB in Flags then
    F := F or MSG_OOB;
  if srfPeek in Flags then
    F := F or MSG_PEEK;
  Result := F;
end;

function SocketRecv(const S: TSocketHandle; var Buf; const Len: Integer; const Flags: TSocketRecvFlags): Integer;
begin
  Result := Recv(TSocket(S), Buf, Len, SocketRecvFlagsToFlags(Flags));
end;

function SocketRecvFrom(const S: TSocketHandle; var Buf; const Len: Integer; const Flags: TSocketRecvFlags;
         out From: TSocketAddr): Integer;
var Addr : TSockAddr;
    L : Integer;
begin
  L := SizeOf(Addr);
  Result := RecvFrom(TSocket(S), Buf, Len, SocketRecvFlagsToFlags(Flags), Addr, L);
  if Result <> SOCKET_ERROR then
    From := SockAddrToSocketAddr(Addr);
end;

function SocketSelect(const nfds: Word32;
         var ReadFDS, WriteFDS, ExceptFDS: TSocketHandleArray;
         const TimeOutMicroseconds: Int64): Integer;
var R, W, E : TFDSet;
    T       : TTimeVal;
    P       : PTimeVal;
begin
  R := SocketHandleArrayToFDSet(ReadFDS);
  W := SocketHandleArrayToFDSet(WriteFDS);
  E := SocketHandleArrayToFDSet(ExceptFDS);
  if TimeOutMicroseconds >= 0 then
    begin
      FillChar(T, Sizeof(TTimeVal), 0);
      T.tv_sec  := TimeOutMicroseconds div 1000000;
      T.tv_usec := TimeOutMicroseconds mod 1000000;
      P := @T;
    end
  else
    P := nil;
  Result := Select(nfds, @R, @W, @E, P);
  if Result >= 0 then
    begin
      ReadFDS := FDSetToSocketHandleArray(R);
      WriteFDS := FDSetToSocketHandleArray(W);
      ExceptFDS := FDSetToSocketHandleArray(E);
    end;
end;

function SocketSelect(const S: TSocketHandle;
         var ReadSelect, WriteSelect, ExceptSelect: Boolean;
         const TimeOutMicroseconds: Int64): Integer;
var R, W, E : TFDSet;
    T       : TTimeVal;
    P       : PTimeVal;
begin
  FD_ZERO(R);
  FD_ZERO(W);
  FD_ZERO(E);
  if ReadSelect then
    FD_SET(S, R);
  if WriteSelect then
    FD_SET(S, W);
  if ExceptSelect then
    FD_SET(S, E);
  if TimeOutMicroseconds >= 0 then
    begin
      FillChar(T, Sizeof(TTimeVal), 0);
      T.tv_sec  := TimeOutMicroseconds div 1000000;
      T.tv_usec := TimeOutMicroseconds mod 1000000;
      P := @T;
    end
  else
    P := nil;
  Result := Select(S + 1, @R, @W, @E, P);
  if Result >= 0 then
    begin
      if ReadSelect then
        ReadSelect := FD_ISSET(S, R);
      if WriteSelect then
        WriteSelect := FD_ISSET(S, W);
      if ExceptSelect then
        ExceptSelect := FD_ISSET(S, E);
    end;
end;

function SocketSend(const S: TSocketHandle; const Buf; const Len, Flags: Integer): Integer;
begin
  Result := Send(TSocket(S), Buf, Len, Flags);
end;

function SocketSendTo(const S: TSocketHandle; const Buf; const Len, Flags: Integer;
         const AddrTo: TSocketAddr): Integer;
var Addr : TSockAddr;
    AddrLen : Integer;
begin
  AddrLen := SocketAddrToSockAddr(AddrTo, Addr);
  Result := SendTo(TSocket(S), Buf, Len, Flags, @Addr, AddrLen);
end;

function SocketSetSockOpt(const S: TSocketHandle; const Level, OptName: Integer;
         const OptVal: Pointer; const OptLen: Integer): Integer;
begin
  Result := SetSockOpt(TSocket(S), Level, OptName, OptVal, OptLen);
end;

{$IFDEF SOCKETLIB_POSIX_FPC}
function SocketShutdown(const S: TSocketHandle; const How: TSocketShutdown): Integer;
begin
  Result := 0;
end;
{$ENDIF}

{$IFDEF SOCKETLIB_WIN}
function SocketShutdown(const S: TSocketHandle; const How: TSocketShutdown): Integer;
var H : Integer;
begin
  case How of
    ssBoth : H := SD_BOTH;
    ssSend : H := SD_SEND;
    ssRecv : H := SD_RECEIVE;
  else
    H := SD_BOTH;
  end;
  Result := Shutdown(TSocket(S), H);
end;
{$ENDIF}

{$IFDEF SOCKETLIB_POSIX_DELPHI}
function SocketShutdown(const S: TSocketHandle; const How: TSocketShutdown): Integer;
begin
  Result := Shutdown(TSocket(S), 0);
end;
{$ENDIF}

function SocketSocket(const Family: TIPAddressFamily; const Struct: Integer; const Protocol: TIPProtocol): TSocketHandle;
var AF, Pr : Integer;
begin
  AF := IPAddressFamilyToAF(Family);
  Pr := IPProtocolToIPPROTO(Protocol);
  Result := TSocketHandle(Socket(AF, Struct, Pr));
end;



{                                                                              }
{ Socket library errors                                                        }
{                                                                              }
function ESocketLibErrorMsg(const Msg: String; const ErrorCode: Integer): String;
var S : String;
begin
  if ErrorCode <> 0 then
    begin
      S := SocketGetErrorMessage(ErrorCode);
      if Msg <> '' then
        S := Format('%s: %s', [Msg, S]);
    end
  else
    S := Msg;
  Result := S;
end;

constructor ESocketLib.Create(const Msg: String; const ErrorCode: Integer);
begin
  inherited Create(ESocketLibErrorMsg(Msg, ErrorCode));
  FErrorCode := ErrorCode;
end;

constructor ESocketLib.CreateFmt(const Msg: String; const Args: array of const;
    const ErrorCode: Integer);
begin
  inherited CreateFmt(ESocketLibErrorMsg(Msg, ErrorCode), Args);
  FErrorCode := ErrorCode;
end;

{$IFDEF SOCKETLIB_POSIX_FPC}
function SocketGetLastError: Integer;
begin
  Result := flcSocketLibSys.SockGetLastError;
end;
{$ENDIF}
{$IFDEF SOCKETLIB_POSIX_DELPHI}
function SocketGetLastError: Integer;
begin
  Result := flcSocketLibSys.GetLastSocketError;
end;
{$ENDIF}
{$IFDEF SOCKETLIB_WIN}
function SocketGetLastError: Integer;
begin
  Result := flcSocketLibSys.WSAGetLastError;
end;
{$ENDIF}

function SocketGetErrorMessage(const ErrorCode: Integer): String;
begin
  case ErrorCode of
    0                      : Result := '';
    EINTR                  : Result := 'Operation interrupted';
    EBADF                  : Result := 'Invalid handle';
    EACCES                 : Result := 'Permission denied';
    EFAULT                 : Result := 'Invalid pointer';
    EINVAL                 : Result := 'Invalid argument';
    EMFILE                 : Result := 'Too many open handles';
    EWOULDBLOCK            : Result := 'Blocking operation';
    EINPROGRESS            : Result := 'Operation in progress';
    EALREADY               : Result := 'Operation already performed';
    ENOTSOCK               : Result := 'Socket operation on non-socket or not connected';
    EDESTADDRREQ           : Result := 'Destination address required';
    EMSGSIZE               : Result := 'Invalid message size';
    EPROTOTYPE             : Result := 'Invalid protocol type';
    ENOPROTOOPT            : Result := 'Protocol not available';
    EPROTONOSUPPORT        : Result := 'Protocol not supported';
    ESOCKTNOSUPPORT        : Result := 'Socket type not supported';
    EOPNOTSUPP             : Result := 'Socket operation not supported';
    EPFNOSUPPORT           : Result := 'Protocol family not supported';
    EAFNOSUPPORT           : Result := 'Address family not supported by protocol family';
    EADDRINUSE             : Result := 'Address in use';
    EADDRNOTAVAIL          : Result := 'Address not available';
    ENETDOWN	             : Result := 'The network is down';
    ENETUNREACH            : Result := 'The network is unreachable';
    ENETRESET              : Result := 'Network connection reset';
    ECONNABORTED           : Result := 'Connection aborted';
    ECONNRESET             : Result := 'Connection reset by peer';
    ENOBUFS	               : Result := 'No buffer space available';
    EISCONN                : Result := 'Socket connected';
    ENOTCONN               : Result := 'Socket not connected';
    ESHUTDOWN              : Result := 'Socket shutdown';
    ETOOMANYREFS           : Result := 'Too many references';
    ETIMEDOUT              : Result := 'Connection timed out';
    ECONNREFUSED           : Result := 'Connection refused';
    ENAMETOOLONG           : Result := 'Name too long';
    EHOSTDOWN              : Result := 'Host is unavailable';
    EHOSTUNREACH           : Result := 'Host is unreachable';
    {$IFDEF SOCKETLIB_WIN}
    HOST_NOT_FOUND         : Result := 'Host not found';
    TRY_AGAIN              : Result := 'Try again';
    NO_RECOVERY            : Result := 'Nonrecoverable error';
    ENOMEM                 : Result := 'Insufficient memory';
    {$ENDIF}
  else
    {$IFDEF SOCKETLIB_WIN}
    Result := flcSocketLibSys.WinSockErrorMessage(ErrorCode);
    {$ELSE}
    {$IFDEF SOCKETLIB_POSIX_FPC}
    Result := flcSocketLibSys.UnixSockErrorMessage(ErrorCode);
    {$ELSE}
    Result := Format('System error #%d', [ErrorCode]);
    {$ENDIF}
    {$ENDIF}
  end;
end;



{                                                                              }
{ IP Addresses                                                                 }
{                                                                              }
function IsIP4AddressB(const Address: RawByteString; out NetAddress: TIP4Addr): Boolean;
var I, L, N : Integer;
begin
  // Validate length: shortest full IP address is 7 characters: #.#.#.#
  L := Length(Address);
  if L < 7 then
    begin
      NetAddress := IP4AddrNone;
      Result := False;
      exit;
    end;
  // Validate number of '.' characters: full IP address must have 3 dots
  N := 0;
  for I := 1 to L do
    if Address[I] = '.' then
      Inc(N);
  if N <> 3 then
    begin
      NetAddress := IP4AddrNone;
      Result := False;
      exit;
    end;
  // Use system to resolve IP
  {$IFDEF SOCKETLIB_WIN}
  if not WinSockStarted then
    WinSockStartup;
  {$ENDIF}
  NetAddress := Socketinet_addr(Pointer(Address));
  if NetAddress.Addr32 <> Word32(INADDR_NONE) then
    Result := True
  else
    // Check for broadcast IP (INADDR_NONE = INADDR_BROADCAST)
    if Address = IP4AddrStrBroadcast then
      begin
        NetAddress := IP4AddrBroadcast;
        Result := True;
      end
    else
      // Unable to resolve IP
      Result := False;
end;

function IsIP6AddressB(const Address: RawByteString; out NetAddress: TIP6Addr): Boolean;
var Hints    : TAddrInfo;
    AddrInfo : PAddrInfo;
    CurrAddr : PAddrInfo;
    Error    : Integer;
    SockAddr : PSockAddr;
    SockAddr6 : PSockAddrIn6;
begin
  // Check length
  if Length(Address) <= 1 then
    begin
      IP6AddrSetZero(NetAddress);
      Result := False;
      exit;
    end;
  // Check special addresses
  if (Address = IP6AddrStrUnspecified) or (Address = IP6AddrStrAnyHost) then
    begin
      IP6AddrSetZero(NetAddress);
      Result := True;
      exit;
    end;
  // Use system to resolve IP
  {$IFDEF SOCKETLIB_WIN}
  if not WinSockStarted then
    WinSockStartup;
  {$ENDIF}
  // Call GetAddrInfo with IP6 address family hint
  FillChar(Hints, Sizeof(TAddrInfo), 0);
  Hints.ai_flags := AI_NUMERICHOST;
  Hints.ai_family := AF_INET6;
  AddrInfo := nil;
  Error := GetAddrInfo(Pointer(Address), nil, @Hints, AddrInfo);
  if (Error = 0) and Assigned(AddrInfo) then
    try
      // Iterate through list of returned addresses until IP6 address is found
      CurrAddr := AddrInfo;
      Result := False;
      repeat
        SockAddr := CurrAddr^.ai_addr;
        if Assigned(SockAddr) and (SockAddr^.sa_family = AF_INET6) then
          begin
            // Found
            SockAddr6 := Pointer(SockAddr);
            Move(SockAddr6^.sin6_addr, NetAddress.Addr32, SizeOf(TIP6Addr));
            Result := True;
            break;
          end;
        CurrAddr := CurrAddr^.ai_next;
      until not Assigned(CurrAddr);
      if not Result then
        IP6AddrSetZero(NetAddress);
    finally
      // Release resources allocated by GetAddrInfo
      FreeAddrInfo(AddrInfo);
    end
  else
    begin
      // Failure
      IP6AddrSetZero(NetAddress);
      Result := False;
    end;
end;

function IsIP4AddressU(const Address: UnicodeString; out NetAddress: TIP4Addr): Boolean;
begin
  Result := IsIP4AddressB(UTF8Encode(Address), NetAddress);
end;

{$IFDEF SOCKETLIB_WIN}
function IsIP6AddressU(const Address: UnicodeString; out NetAddress: TIP6Addr): Boolean;
var Hints    : TAddrInfoW;
    AddrInfo : PAddrInfoW;
    CurrAddr : PAddrInfoW;
    Error    : Integer;
    SockAddr : PSockAddr;
begin
  // Check length
  if Length(Address) <= 1 then
    begin
      IP6AddrSetZero(NetAddress);
      Result := False;
      exit;
    end;
  // Check special addresses
  if (Address = IP6AddrStrUnspecified) or (Address = IP6AddrStrAnyHost) then
    begin
      IP6AddrSetZero(NetAddress);
      Result := True;
      exit;
    end;
  // Use system to resolve IP
  {$IFDEF SOCKETLIB_WIN}
  if not WinSockStarted then
    WinSockStartup;
  {$ENDIF}
  // Call GetAddrInfo with IP6 address family hint
  FillChar(Hints, Sizeof(TAddrInfoW), 0);
  Hints.ai_flags := AI_NUMERICHOST;
  Hints.ai_family := AF_INET6;
  AddrInfo := nil;
  Error := GetAddrInfoW(PWideChar(Address), nil, @Hints, AddrInfo);
  if (Error = 0) and Assigned(AddrInfo) then
    try
      // Iterate through list of returned addresses until IP6 address is found
      CurrAddr := AddrInfo;
      Result := False;
      repeat
        SockAddr := CurrAddr^.ai_addr;
        if Assigned(SockAddr) and (SockAddr^.sa_family = AF_INET6) then
          begin
            // Found
            Move(SockAddr^.sin6_addr.u6_addr32, NetAddress.Addr32, SizeOf(TIP6Addr));
            Result := True;
            break;
          end;
        CurrAddr := CurrAddr^.ai_next;
      until not Assigned(CurrAddr);
      if not Result then
        IP6AddrSetZero(NetAddress);
    finally
      // Release resources allocated by GetAddrInfo
      FreeAddrInfoW(AddrInfo);
    end
  else
    begin
      // Failure
      IP6AddrSetZero(NetAddress);
      Result := False;
    end;
end;
{$ENDIF}

function IsIP4Address(const Address: String; out NetAddress: TIP4Addr): Boolean;
begin
  {$IFDEF StringIsUnicode}
  Result := IsIP4AddressU(Address, NetAddress);
  {$ELSE}
  Result := IsIP4AddressB(Address, NetAddress);
  {$ENDIF}
end;

{$IFDEF SOCKETLIB_WIN}
function IsIP6Address(const Address: String; out NetAddress: TIP6Addr): Boolean;
begin
  {$IFDEF StringIsUnicode}
  Result := IsIP6AddressU(Address, NetAddress);
  {$ELSE}
  Result := IsIP6AddressB(Address, NetAddress);
  {$ENDIF}
end;
{$ENDIF}

function IP4AddressStrB(const Address: TIP4Addr): RawByteString;
begin
  Result := Socketinet_ntoa(Address);
end;

function IP6AddressStrB(const Address: TIP6Addr): RawByteString;
var I : Integer;
begin
  // Handle special addresses
  if IP6AddrIsZero(Address) then
    begin
      Result := IP6AddrStrUnspecified;
      exit;
    end;
  if IP6AddrIsLocalHost(Address) then
    begin
      Result := IP6AddrStrLocalHost;
      exit;
    end;
  // Return full IP6 address
  Result := '';
  for I := 0 to 7 do
    begin
      Result := Result + Word32ToHexB(ntohs(Address.Addr16[I]), 0, True);
      if I < 7 then
        Result := Result + ':';
    end;
end;

function IP4AddressStr(const Address: TIP4Addr): String;
begin
  {$IFDEF StringIsUnicode}
  Result := String(IP4AddressStrB(Address));
  {$ELSE}
  Result := IP4AddressStrB(Address);
  {$ENDIF}
end;

function IP6AddressStr(const Address: TIP6Addr): String;
begin
  {$IFDEF StringIsUnicode}
  Result := String(IP6AddressStrB(Address));
  {$ELSE}
  Result := IP6AddressStrB(Address);
  {$ENDIF}
end;

function IP4AddressType(const Address: TIP4Addr): TIP4AddressType;
begin
  Result := inaPublic;
  case Address.Addr8[0] of
    0        : if Address.Addr32 = 0 then
                 Result := inaNone
               else
                 Result := inaReserved;
    10       : Result := inaPrivate;
    127      : Result := inaLoopback;
    169      : if Address.Addr8[1] = 254 then
                 Result := inaLinkLocalNetwork;
    172      : if Address.Addr8[1] and $F0 = $10 then
                 Result := inaPrivate;
    192      : case Address.Addr8[1] of
                 0   : if Address.Addr8[2] = 2 then
                         Result := inaTestNetwork;
                 168 : Result := inaPrivate;
               end;
    224..239 : Result := inaMulticast;
    240..254 : Result := inaReserved;
    255      : if Address.Addr32 = $FFFFFFFF then
                 Result := inaBroadcast
               else
                 Result := inaReserved;
  end;
end;

function IsPrivateIP4Address(const Address: TIP4Addr): Boolean;
begin
  Result := IP4AddressType(Address) = inaPrivate;
end;

function IsInternetIP4Address(const Address: TIP4Addr): Boolean;
begin
  Result := IP4AddressType(Address) = inaPublic;
end;

procedure SwapIP4Endian(var Address: TIP4Addr);
var A : Byte;
begin
  A := Address.Addr8[0];
  Address.Addr8[0] := Address.Addr8[3];
  Address.Addr8[3] := A;
  A := Address.Addr8[1];
  Address.Addr8[1] := Address.Addr8[2];
  Address.Addr8[2] := A;
end;



{                                                                              }
{ HostEnt functions                                                            }
{                                                                              }
function HostEntAddressesCount(const HostEnt: PHostEnt): Integer;
var P : ^PInAddr;
    Q : PInAddr;
begin
  Result := 0;
  if not Assigned(HostEnt) then
    exit;
  Assert(HostEnt^.h_addrtype = AF_INET);
  Assert(HostEnt^.h_length = Sizeof(TInAddr));
  P := Pointer(HostEnt^.h_addr_list);
  if not Assigned(P) then
    exit;
  Q := P^;
  while Assigned(Q) do
    begin
      Inc(P);
      Inc(Result);
      Q := P^
    end;
end;

function HostEntAddresses(const HostEnt: PHostEnt): TIP4AddrArray;
var P : ^PInAddr;
    I, L : Integer;
begin
  L := HostEntAddressesCount(HostEnt);
  SetLength(Result, L);
  if L = 0 then
    exit;
  P := Pointer(HostEnt^.h_addr_list);
  for I := 0 to L - 1 do
    begin
      Result[I].Addr32 := P^^.S_addr;
      Inc(P);
    end;
end;

function HostEntAddress(const HostEnt: PHostEnt; const Index: Integer): TSocketAddr;
var A : TIPAddressFamily;
    L : Integer;
    P : ^Pointer;
    Q : Pointer;
    I : Integer;
begin
  InitSocketAddrNone(Result);
  if not Assigned(HostEnt) then
    exit;
  A := AFToIPAddressFamily(HostEnt^.h_addrtype);
  if A = iaNone then
    raise ESocketLib.Create('Invalid address family');
  L := HostEnt^.h_length;
  Assert( ((A = iaIP4) and (L = Sizeof(TInAddr))) or
          ((A = iaIP6) and (L = Sizeof(TIn6Addr))) );
  P := Pointer(HostEnt^.h_addr_list);
  if not Assigned(P) then
    exit;
  Q := P^;
  I := 0;
  while Assigned(Q) and (I < Index) do
    begin
      Inc(P);
      Inc(I);
      Q := P^
    end;
  if not Assigned(Q) then
    exit;
  Result.AddrFamily := A;
  case A of
    iaIP4 : Result.AddrIP4.Addr32 := PInAddr(Q)^.S_addr;
    iaIP6 : Move(PIn6Addr(Q)^, Result.AddrIP6, SizeOf(TIP6Addr));
  end;
end;

function HostEntAddressIP4(const HostEnt: PHostEnt; const Index: Integer): TIP4Addr;
var P : ^PInAddr;
    Q : PInAddr;
    I : Integer;
begin
  Result := IP4AddrNone;
  if not Assigned(HostEnt) then
    exit;
  Assert(HostEnt^.h_addrtype = AF_INET);
  Assert(HostEnt^.h_length = Sizeof(TInAddr));
  P := Pointer(HostEnt^.h_addr_list);
  if not Assigned(P) then
    exit;
  Q := P^;
  I := 0;
  while Assigned(Q) and (I < Index) do
    begin
      Inc(P);
      Inc(I);
      Q := P^
    end;
  if Assigned(Q) then
    Result.Addr32 := Q^.S_addr;
end;

function HostEntAddressStr(const HostEnt: PHostEnt; const Index: Integer): RawByteString;
begin
  Result := IP4AddressStrB(HostEntAddressIP4(HostEnt, Index));
end;

function HostEntName(const HostEnt: PHostEnt): RawByteString;
begin
  Result := StrZPasB(Pointer(HostEnt.h_name));
end;



{                                                                              }
{ SocketProtocolAsString                                                       }
{                                                                              }
const
  ProtocolStr: Array[TIPProtocol] of RawByteString =
      ('', 'ip', 'icmp', 'tcp', 'udp', 'raw');

function IPProtocolToStrB(const Protocol: TIPProtocol): RawByteString;
var ProtoNum : Integer;
    PEnt     : PProtoEnt;
begin
  case Protocol of
    ipTCP : ProtoNum := IPPROTO_TCP;
    ipUDP : ProtoNum := IPPROTO_UDP;
    ipRaw : ProtoNum := IPPROTO_RAW;
  else
    ProtoNum := -1;
  end;
  if ProtoNum >= 0 then
    begin
      {$IFDEF SOCKETLIB_WIN}
      if not WinSockStarted then
        WinSockStartup;
      {$ENDIF}
      PEnt := GetProtoByNumber(ProtoNum);
      if Assigned(PEnt) then
        Result := StrZPasB(Pointer(PEnt^.p_name))
      else
        Result := ProtocolStr[Protocol];
    end
  else
    Result := '';
end;

function StrToIPProtocolB(const Protocol: RawByteString): TIPProtocol;
var I    : TIPProtocol;
    PEnt : PProtoEnt;
begin
  PEnt := GetProtoByName(Pointer(Protocol));
  if Assigned(PEnt) then
    case PEnt^.p_proto of
      IPPROTO_TCP : Result := ipTCP;
      IPPROTO_UDP : Result := ipUDP;
      IPPROTO_RAW : Result := ipRaw;
    else
      Result := ipNone;
    end
  else
    begin
      for I := Low(TIPProtocol) to High(TIPProtocol) do
        if Protocol = ProtocolStr[I] then
          begin
            Result := I;
            exit;
          end;
      Result := ipNone;
    end;
end;

function IPProtocolToStr(const Protocol: TIPProtocol): String;
begin
  {$IFDEF StringIsUnicode}
  Result := String(IPProtocolToStrB(Protocol));
  {$ELSE}
  Result := IPProtocolToStrB(Protocol);
  {$ENDIF}
end;

function StrToIPProtocol(const Protocol: String): TIPProtocol;
begin
  {$IFDEF StringIsUnicode}
  Result := StrToIPProtocolB(RawByteString(Protocol));
  {$ELSE}
  Result := StrToIPProtocolB(Protocol);
  {$ENDIF}
end;


{                                                                              }
{ Local Host                                                                   }
{                                                                              }
function LocalHostNameB: RawByteString;
var Buf : Array[0..1024] of AnsiChar;
    Err : Integer;
begin
  {$IFDEF SOCKETLIB_WIN}
  if not WinSockStarted then
    WinSockStartup;
  {$ENDIF}
  FillChar(Buf, Sizeof(Buf), 0);
  Err := SocketGetHostName(@Buf, Sizeof(Buf) - 1);
  if Err <> 0 then
    raise ESocketLib.Create('Local host name not available', Err);
  Result := StrZPasB(@Buf);
end;

function LocalHostName: String;
begin
  {$IFDEF StringIsUnicode}
  Result := String(LocalHostNameB);
  {$ELSE}
  Result := LocalHostNameB;
  {$ENDIF}
end;

function LocalIPAddresses: TIP4AddrArray;
begin
  {$IFDEF SOCKETLIB_WIN}
  if not WinSockStarted then
    WinSockStartup;
  {$ENDIF}
  Result := HostEntAddresses(GetHostByName(Pointer(LocalHostNameB)));
end;

function LocalIP6Addresses: TIP6AddrArray;
var Addr : TSocketAddrArray;
    L, I : Integer;
begin
  SocketGetAddrInfo(iaIP6, ipNone, LocalHostNameB, '', Addr);
  L := Length(Addr);
  SetLength(Result, L);
  for I := 0 to L - 1 do
    IP6AddrAssign(Result[I], Addr[I].AddrIP6);
end;

function LocalIP4AddressesStrB: AddressStrArrayB;
var V : TIP4AddrArray;
    I, L : Integer;
begin
  V := LocalIPAddresses;
  L := Length(V);
  SetLength(Result, L);
  for I := 0 to L - 1 do
    Result[I] := IP4AddressStrB(V[I]);
end;

function LocalIP6AddressesStrB: AddressStrArrayB;
var V : TIP6AddrArray;
    I, L : Integer;
begin
  V := LocalIP6Addresses;
  L := Length(V);
  SetLength(Result, L);
  for I := 0 to L - 1 do
    Result[I] := IP6AddressStrB(V[I]);
end;

function LocalIP4AddressesStr: AddressStrArray;
var V : TIP4AddrArray;
    I, L : Integer;
begin
  V := LocalIPAddresses;
  L := Length(V);
  SetLength(Result, L);
  for I := 0 to L - 1 do
    Result[I] := IP4AddressStr(V[I]);
end;

function LocalIP6AddressesStr: AddressStrArray;
var V : TIP6AddrArray;
    I, L : Integer;
begin
  V := LocalIP6Addresses;
  L := Length(V);
  SetLength(Result, L);
  for I := 0 to L - 1 do
    Result[I] := IP6AddressStr(V[I]);
end;

function GuessInternetIP4: TIP4Addr;
var A : TIP4AddrArray;
    I : Integer;
begin
  A := LocalIPAddresses;
  for I := 0 to Length(A) - 1 do
    if IsInternetIP4Address(A[I]) then
      begin
        Result.Addr32 := A[I].Addr32;
        exit;
      end;
  Result := IP4AddrNone;
end;

function GuessInternetIP4StrB: RawByteString;
var A : TIP4Addr;
begin
  A := GuessInternetIP4;
  if Int32(A.Addr32) = Int32(INADDR_NONE) then
    Result := ''
  else
    Result := IP4AddressStrB(A);
end;

function GuessInternetIP4Str: String;
begin
  {$IFDEF StringIsUnicode}
  Result := String(GuessInternetIP4StrB);
  {$ELSE}
  Result := GuessInternetIP4StrB;
  {$ENDIF}
end;



{                                                                              }
{ Remote host name                                                             }
{                                                                              }
function GetRemoteHostNameB(const Address: TSocketAddr): RawByteString;
var NewAPI  : Boolean;
    HostEnt : TSocketHost;
begin
  {$IFDEF SOCKETLIB_WIN}
  if not WinSockStarted then
    WinSockStartup;
  {$ENDIF}
  NewAPI := {$IFDEF SOCKETLIB_WIN}WinSock2API or{$ENDIF}
            (Address.AddrFamily = iaIP6);
  if NewAPI then
    begin
      Result := SocketGetNameInfo(Address);
      exit;
    end;
  case Address.AddrFamily of
    iaIP4 : HostEnt := SocketGetHostByAddr(@Address.AddrIP4, Sizeof(TInAddr), AF_INET);
    iaIP6 : HostEnt := SocketGetHostByAddr(@Address.AddrIP6, Sizeof(TIn6Addr), AF_INET6);
  else
    raise ESocketLib.Create('Invalid address family');
  end;
  if not HostEnt.Used then
    raise ESocketLib.Create('Reverse lookup failed', SocketGetLastError);
  Result := HostEnt.Host;
end;

function GetRemoteHostNameB(const Address: TIP4Addr): RawByteString;
var S : TSocketAddr;
begin
  InitSocketAddrNone(S);
  S.AddrFamily := iaIP4;
  S.AddrIP4 := Address;
  Result := GetRemoteHostNameB(S);
end;

function GetRemoteHostNameB(const Address: TIP6Addr): RawByteString;
var S : TSocketAddr;
begin
  InitSocketAddrNone(S);
  S.AddrFamily := iaIP6;
  IP6AddrAssign(S.AddrIP6, Address);
  Result := GetRemoteHostNameB(S);
end;

function GetRemoteHostName(const Address: TSocketAddr): String;
begin
  {$IFDEF StringIsUnicode}
  Result := String(GetRemoteHostNameB(Address));
  {$ELSE}
  Result := GetRemoteHostNameB(Address);
  {$ENDIF}
end;

function GetRemoteHostName(const Address: TIP4Addr): String;
begin
  {$IFDEF StringIsUnicode}
  Result := String(GetRemoteHostNameB(Address));
  {$ELSE}
  Result := GetRemoteHostNameB(Address);
  {$ENDIF}
end;

function GetRemoteHostName(const Address: TIP6Addr): String;
begin
  {$IFDEF StringIsUnicode}
  Result := String(GetRemoteHostNameB(Address));
  {$ELSE}
  Result := GetRemoteHostNameB(Address);
  {$ENDIF}
end;



{                                                                              }
{ Resolve host                                                                 }
{                                                                              }
function ResolveHostExB(const Host: RawByteString;
    const AddressFamily: TIPAddressFamily): TSocketAddrArray;
var NewAPI  : Boolean;
    HostEnt : PHostEnt;
    InAddr  : TIP4Addr;
    In6Addr : TIP6Addr;
    InAddrs : TIP4AddrArray;
    L, I    : Integer;
begin
  {$IFDEF DELPHI7_DOWN}
  InAddrs := nil;
  {$ENDIF}
  if Host = '' then
    raise ESocketLib.Create('Host not specified');
  if AddressFamily = iaIP4 then
    if IsIP4AddressB(Host, InAddr) then
      begin
        SetLength(Result, 1);
        InitSocketAddr(Result[0], InAddr, 0);
        exit;
      end;
  if AddressFamily = iaIP6 then
    if IsIP6AddressB(Host, In6Addr) then
      begin
        SetLength(Result, 1);
        InitSocketAddr(Result[0], In6Addr, 0);
        exit;
      end;
  {$IFDEF SOCKETLIB_WIN}
  if not WinSockStarted then
    WinSockStartup;
  {$ENDIF}
  NewAPI :=
      {$IFDEF SOCKETLIB_WIN}WinSock2API or{$ENDIF}
      (AddressFamily = iaIP6);
  if NewAPI then
    begin
      SocketGetAddrInfo(AddressFamily, ipNone, Host, '', Result);
      exit;
    end;
  HostEnt := GetHostByName(Pointer(Host));
  if Assigned(HostEnt) then
    begin
      InAddrs := HostEntAddresses(HostEnt);
      L := Length(InAddrs);
      SetLength(Result, L);
      for I := 0 to L - 1 do
        InitSocketAddr(Result[I], InAddrs[I], 0);
    end
  else
    raise ESocketLib.Create('Failed to resolve host', SocketGetLastError);
end;

function ResolveHostB(const Host: RawByteString;
    const AddressFamily: TIPAddressFamily): TSocketAddr;
var A : TSocketAddrArray;
begin
  A := ResolveHostExB(Host, AddressFamily);
  if Length(A) = 0 then
    raise ESocketLib.Create('Failed to resolve host');
  Result := A[0];
end;

function ResolveHostEx(const Host: String; const AddressFamily: TIPAddressFamily): TSocketAddrArray;
begin
  {$IFDEF StringIsUnicode}
  Result := ResolveHostExB(RawByteString(Host), AddressFamily);
  {$ELSE}
  Result := ResolveHostExB(Host, AddressFamily);
  {$ENDIF}
end;

function ResolveHost(const Host: String; const AddressFamily: TIPAddressFamily): TSocketAddr;
begin
  {$IFDEF StringIsUnicode}
  Result := ResolveHostB(RawByteString(Host), AddressFamily);
  {$ELSE}
  Result := ResolveHostB(Host, AddressFamily);
  {$ENDIF}
end;

function ResolveHostIP4ExB(const Host: RawByteString): TIP4AddrArray;
var HostEnt : PHostEnt;
    InAddr  : TIP4Addr;
    {$IFDEF SOCKETLIB_WIN}
    Addrs   : TSocketAddrArray;
    I, L    : Integer;
    {$ENDIF}
begin
  if Host = '' then
    raise ESocketLib.Create('Host not specified');
  if IsIP4AddressB(Host, InAddr) then
    begin
      SetLength(Result, 1);
      Result[0] := InAddr;
      exit;
    end;
  {$IFDEF SOCKETLIB_WIN}
  if not WinSockStarted then
    WinSockStartup;
  if WinSock2API then
    begin
      SocketGetAddrInfo(iaIP4, ipNone, Host, '', Addrs);
      L := Length(Addrs);
      if L = 0 then
        raise ESocketLib.Create('Failed to resolve host', SocketGetLastError);
      SetLength(Result, L);
      for I := 0 to L - 1 do
        Result[I] := Addrs[I].AddrIP4;
      exit;
    end;
  {$ENDIF}
  HostEnt := GetHostByName(Pointer(Host));
  if Assigned(HostEnt) then
    Result := HostEntAddresses(HostEnt)
  else
    raise ESocketLib.Create('Failed to resolve host', SocketGetLastError);
end;

function ResolveHostIP4B(const Host: RawByteString): TIP4Addr;
var A : TIP4AddrArray;
begin
  A := ResolveHostIP4ExB(Host);
  if Length(A) = 0 then
    raise ESocketLib.Create('Failed to resolve host');
  Result.Addr32 := A[0].Addr32;
end;

function ResolveHostIP4Ex(const Host: String): TIP4AddrArray;
begin
  {$IFDEF StringIsUnicode}
  Result := ResolveHostIP4ExB(RawByteString(Host));
  {$ELSE}
  Result := ResolveHostIP4ExB(Host);
  {$ENDIF}
end;

function ResolveHostIP4(const Host: String): TIP4Addr;
begin
  {$IFDEF StringIsUnicode}
  Result := ResolveHostIP4B(RawByteString(Host));
  {$ELSE}
  Result := ResolveHostIP4B(Host);
  {$ENDIF}
end;

function ResolveHostIP6ExB(const Host: RawByteString): TIP6AddrArray;
var In6Addr : TIP6Addr;
    Addrs   : TSocketAddrArray;
    L, I    : Integer;
begin
  if Host = '' then
    raise ESocketLib.Create('Host not specified');
  if IsIP6AddressB(Host, In6Addr) then
    begin
      SetLength(Result, 1);
      Result[0] := In6Addr;
      exit;
    end;
  SocketGetAddrInfo(iaIP6, ipNone, Host, '', Addrs);
  L := Length(Addrs);
  if L = 0 then
    raise ESocketLib.Create('Failed to resolve host', SocketGetLastError);
  SetLength(Result, L);
  for I := 0 to L - 1 do
    Result[I] := Addrs[I].AddrIP6;
end;

function ResolveHostIP6B(const Host: RawByteString): TIP6Addr;
var Addrs : TSocketAddrArray;
begin
  if Host = '' then
    raise ESocketLib.Create('Host not specified');
  if IsIP6AddressB(Host, Result) then
    exit;
  SocketGetAddrInfo(iaIP6, ipNone, Host, '', Addrs);
  if Length(Addrs) = 0 then
    raise ESocketLib.Create('Failed to resolve host', SocketGetLastError);
  Assert(Addrs[0].AddrFamily = iaIP6);
  Result := Addrs[0].AddrIP6;
end;

function ResolveHostIP6Ex(const Host: String): TIP6AddrArray;
begin
  {$IFDEF StringIsUnicode}
  Result := ResolveHostIP6ExB(RawByteString(Host));
  {$ELSE}
  Result := ResolveHostIP6ExB(Host);
  {$ENDIF}
end;

function ResolveHostIP6(const Host: String): TIP6Addr;
begin
  {$IFDEF StringIsUnicode}
  Result := ResolveHostIP6B(RawByteString(Host));
  {$ELSE}
  Result := ResolveHostIP6B(Host);
  {$ENDIF}
end;



{                                                                              }
{ Port                                                                         }
{                                                                              }
{$IFDEF DELPHI5}
function TryStrToInt(const S: AnsiString; var I: Integer): Boolean;
var Error : Integer;
begin
  Val(S, I, Error);
  Result := Error = 0;
end;
{$ENDIF}

function ResolvePortB(const Port: RawByteString; const Protocol: TIPProtocol): Word;
var PortInt  : Integer;
    PortPtr  : PByte;
    ProtoEnt : PProtoEnt;
    ServEnt  : PServEnt;
begin
  if Port = '' then
    raise ESocketLib.Create('Port not specified');
  // Resolve numeric port value
  if TryStrToInt(String(Port), PortInt) then
    begin
      if (PortInt < 0) or (PortInt > $FFFF) then
        raise ESocketLib.Create('Port number out of range');
      Result := PortToNetPort(Word(PortInt));
      exit;
    end;
  // Resolve port using system
  {$IFDEF SOCKETLIB_WIN}
  if not WinSockStarted then
    WinSockStartup;
  {$ENDIF}
  case Protocol of
    ipTCP : ProtoEnt := GetProtoByNumber(IPPROTO_TCP);
    ipUDP : ProtoEnt := GetProtoByNumber(IPPROTO_UDP);
  else
    ProtoEnt := nil;
  end;
  PortPtr := Pointer(Port);
  while PortPtr^ = Ord(' ') do
    Inc(PortPtr);
  if Assigned(ProtoEnt) and Assigned(ProtoEnt^.p_name) then
    ServEnt := GetServByName(Pointer(PortPtr), ProtoEnt^.p_name)
  else
    ServEnt := GetServByName(Pointer(PortPtr), nil);
  if not Assigned(ServEnt) then
    raise ESocketLib.Create('Failed to resolve port', SocketGetLastError);
  Result := ServEnt^.s_port;
end;

function ResolvePort(const Port: String; const Protocol: TIPProtocol): Word;
begin
  {$IFDEF StringIsUnicode}
  Result := ResolvePortB(RawByteString(Port), Protocol);
  {$ELSE}
  Result := ResolvePortB(Port, Protocol);
  {$ENDIF}
end;

function NetPortToPort(const NetPort: Word): Word;
begin
  Result := Socketntohs(NetPort);
end;

function NetPortToPortStr(const NetPort: Word): String;
begin
  Result := IntToStr(NetPortToPort(NetPort));
end;

function NetPortToPortStrB(const NetPort: Word): RawByteString;
begin
  Result := RawByteString(IntToStr(NetPortToPort(NetPort)));
end;

function PortToNetPort(const Port: Word): Word;
begin
  Result := Sockethtons(Port);
end;



{                                                                              }
{ Resolve host and port                                                        }
{                                                                              }
function ResolveB(
         const Host: RawByteString; const Port: Integer;
         const AddressFamily: TIPAddressFamily = iaIP4;
         const Protocol: TIPProtocol = ipTCP): TSocketAddr;
begin
  InitSocketAddrNone(Result);
  case AddressFamily of
    iaIP4 :
      begin
        Result.AddrFamily := iaIP4;
        if Host <> '' then
          Result.AddrIP4 := ResolveHostIP4B(Host)
        else
          Result.AddrIP4.Addr32 := Word32(INADDR_ANY);
        Result.Port := Port;
      end;
    iaIP6 :
      begin
        Result.AddrFamily := iaIP6;
        if Host <> '' then
          Result.AddrIP6 := ResolveHostIP6B(Host)
        else
          IP6AddrSetZero(Result.AddrIP6);
        Result.Port := Port;
      end;
  end;
end;

function ResolveB(
         const Host, Port: RawByteString;
         const AddressFamily: TIPAddressFamily;
         const Protocol: TIPProtocol): TSocketAddr;
begin
  InitSocketAddrNone(Result);
  case AddressFamily of
    iaIP4 :
      begin
        Result.AddrFamily := iaIP4;
        if Host <> '' then
          Result.AddrIP4 := ResolveHostIP4B(Host)
        else
          Result.AddrIP4.Addr32 := Word32(INADDR_ANY);
        if Port <> '' then
          Result.Port := NetPortToPort(ResolvePortB(Port, Protocol));
      end;
    iaIP6 :
      begin
        Result.AddrFamily :=  iaIP6;
        if Host <> '' then
          Result.AddrIP6 := ResolveHostIP6B(Host)
        else
          IP6AddrSetZero(Result.AddrIP6);
        if Port <> '' then
          Result.Port := NetPortToPort(ResolvePortB(Port, Protocol));
      end;
  end;
end;

function Resolve(
         const Host: String; const Port: Integer;
         const AddressFamily: TIPAddressFamily = iaIP4;
         const Protocol: TIPProtocol = ipTCP): TSocketAddr;
begin
  {$IFDEF StringIsUnicode}
  {$IFDEF POSIX}
  Result := ResolveB(UTF8Encode(Host), Port, AddressFamily, Protocol);
  {$ELSE}
  Result := ResolveB(RawByteString(Host), Port, AddressFamily, Protocol);
  {$ENDIF}
  {$ELSE}
  Result := ResolveB(Host, Port, AddressFamily, Protocol);
  {$ENDIF}
end;

function Resolve(
         const Host, Port: String;
         const AddressFamily: TIPAddressFamily = iaIP4;
         const Protocol: TIPProtocol = ipTCP): TSocketAddr;
begin
  {$IFDEF StringIsUnicode}
  {$IFDEF POSIX}
  Result := ResolveB(UTF8Encode(Host), UTF8Encode(Port), AddressFamily, Protocol);
  {$ELSE}
  Result := ResolveB(RawByteString(Host), RawByteString(Port), AddressFamily, Protocol);
  {$ENDIF}
  {$ELSE}
  Result := ResolveB(Host, Port, AddressFamily, Protocol);
  {$ENDIF}
end;



{                                                                              }
{ Socket handle                                                                }
{                                                                              }
function AllocateSocketHandle(const AddressFamily: TIPAddressFamily;
    const Protocol: TIPProtocol; const Overlapped: Boolean): TSocketHandle;
var AF, ST, PR : Int32;
    {$IFDEF SOCKETLIB_WIN}
    NewAPI     : Boolean;
    FL         : Word32;
    {$ENDIF}
    Res        : TSocket;
begin
  AF := IPAddressFamilyToAF(AddressFamily);
  if AF = AF_UNSPEC then
    raise ESocketLib.Create('Invalid address family', EINVAL);
  PR := IPProtocolToIPPROTO(Protocol);
  if PR < 0 then
    raise ESocketLib.Create('Invalid protocol', EINVAL);
  case Protocol of
    ipTCP : ST := SOCK_STREAM;
    ipUDP : ST := SOCK_DGRAM;
    ipRaw : ST := SOCK_RAW;
  else
    raise ESocketLib.Create('Invalid protocol', EINVAL);
  end;
  {$IFDEF SOCKETLIB_WIN}
  if not WinSockStarted then
    WinSockStartup;
  {$IFDEF OS_WIN64}
  NewAPI := False;
  {$ELSE}
  NewAPI := WinSock2API or Overlapped;
  {$ENDIF}
  if NewAPI then
    begin
      if Overlapped then
        FL := WSA_FLAG_OVERLAPPED
      else
        FL := 0;
      Result := WSASocketA(AF, ST, PR, nil, 0, FL);
      if Result = INVALID_SOCKET then
        raise ESocketLib.Create('Failed to allocate socket handle', SocketGetLastError);
      exit;
    end;
  {$ENDIF}
  if Overlapped then
    raise ESocketLib.Create('Overlapped sockets not supported');
  Res := Socket(AF, ST, PR);
  if Res = INVALID_SOCKET then
    raise ESocketLib.Create('Failed to allocate socket handle', SocketGetLastError);
  Result := TSocketHandle(Res);
end;



{                                                                              }
{ Socket options                                                               }
{                                                                              }

// if TimeoutUs = 0 operation doesn't time out
function GetSocketReceiveTimeout(const SocketHandle: TSocketHandle): Integer;
var Opt : TTimeVal;
    OptLen : Integer;
begin
  OptLen := Sizeof(Opt);
  if SocketGetSockOpt(SocketHandle, SOL_SOCKET, SO_RCVTIMEO, @Opt, OptLen) < 0 then
    raise ESocketLib.Create('Socket receive timeout not available', SocketGetLastError);
  Result := Opt.tv_sec * 1000000 + Opt.tv_usec;
end;

procedure SetSocketReceiveTimeout(const SocketHandle: TSocketHandle;
    const TimeoutUs: Integer);
var Opt : TTimeVal;
begin
  Opt.tv_sec := TimeoutUs div 1000000;
  Opt.tv_usec := TimeoutUs mod 1000000;
  if SocketSetSockOpt(SocketHandle, SOL_SOCKET, SO_RCVTIMEO, @Opt, Sizeof(Opt)) < 0 then
    raise ESocketLib.Create('Socket receive timeout not set', SocketGetLastError);
end;

// if TimeoutUs = 0 operation doesn't time out
function GetSocketSendTimeOut(const SocketHandle: TSocketHandle): Integer;
var Opt : TTimeVal;
    OptLen : Integer;
begin
  OptLen := Sizeof(Opt);
  if SocketGetSockOpt(SocketHandle, SOL_SOCKET, SO_SNDTIMEO, @Opt, OptLen) < 0 then
    raise ESocketLib.Create('Socket send timeout not available', SocketGetLastError);
  Result := Opt.tv_sec * 1000000 + Opt.tv_usec;
end;

procedure SetSocketSendTimeout(const SocketHandle: TSocketHandle;
    const TimeoutUs: Integer);
var Opt : TTimeVal;
begin
  Opt.tv_sec := TimeoutUs div 1000000;
  Opt.tv_usec := TimeoutUs mod 1000000;
  if SocketSetSockOpt(SocketHandle, SOL_SOCKET, SO_SNDTIMEO, @Opt, Sizeof(Opt)) < 0 then
    raise ESocketLib.Create('Socket send timeout not set', SocketGetLastError);
end;

function GetSocketReceiveBufferSize(const SocketHandle: TSocketHandle): Integer;
var BufferSize : Int32;
    OptLen : Integer;
begin
  OptLen := Sizeof(BufferSize);
  if SocketGetSockOpt(SocketHandle, SOL_SOCKET, SO_RCVBUF, @BufferSize, OptLen) < 0 then
    raise ESocketLib.Create('Receive buffer size not available', SocketGetLastError);
  Result := BufferSize;
end;

procedure SetSocketReceiveBufferSize(const SocketHandle: TSocketHandle; const BufferSize: Integer);
begin
  if SocketSetSockOpt(SocketHandle, SOL_SOCKET, SO_RCVBUF, @BufferSize, Sizeof(BufferSize)) < 0 then
    raise ESocketLib.Create('Receive buffer size not set', SocketGetLastError);
end;

function GetSocketSendBufferSize(const SocketHandle: TSocketHandle): Integer;
var BufferSize : Int32;
    OptLen : Integer;
begin
  OptLen := Sizeof(BufferSize);
  if SocketGetSockOpt(SocketHandle, SOL_SOCKET, SO_SNDBUF, @BufferSize, OptLen) < 0 then
    raise ESocketLib.Create('Send buffer size not available', SocketGetLastError);
  Result := BufferSize;
end;

procedure SetSocketSendBufferSize(const SocketHandle: TSocketHandle; const BufferSize: Integer);
begin
  if SocketSetSockOpt(SocketHandle, SOL_SOCKET, SO_SNDBUF, @BufferSize, Sizeof(BufferSize)) < 0 then
    raise ESocketLib.Create('Send buffer size not set', SocketGetLastError);
end;

{$IFDEF SOCKETLIB_WIN}
procedure GetSocketLinger(const SocketHandle: TSocketHandle;
    var Linger: Boolean; var LingerTimeSec: Integer);
var Opt : TLinger;
    OptLen : Int32;
begin
  OptLen := Sizeof(Opt);
  if SocketGetSockOpt(SocketHandle, SOL_SOCKET, SO_LINGER, @Opt, OptLen) < 0 then
    raise ESocketLib.Create('Socket linger option not available', SocketGetLastError);
  Linger := Opt.l_onoff <> 0;
  LingerTimeSec := Opt.l_linger;
end;

procedure SetSocketLinger(const SocketHandle: TSocketHandle;
    const Linger: Boolean; const LingerTimeSec: Integer);
var Opt : TLinger;
begin
  if Linger then
    Opt.l_onoff := 1
  else
    Opt.l_onoff := 0;
  Opt.l_linger := LingerTimeSec;
  if SocketSetSockOpt(SocketHandle, SOL_SOCKET, SO_LINGER, @Opt, Sizeof(Opt)) < 0 then
    raise ESocketLib.Create('Socket linger option not set', SocketGetLastError);
end;
{$ENDIF}

{$IFDEF SOCKETLIB_POSIX_FPC}
procedure GetSocketLinger(const SocketHandle: TSocketHandle;
    var Linger: Boolean; var LingerTimeSec: Integer);
var Opt : TLinger;
    OptLen : Int32;
begin
  OptLen := Sizeof(Opt);
  if SocketGetSockOpt(SocketHandle, SOL_SOCKET, SO_LINGER, @Opt, OptLen) < 0 then
    raise ESocketLib.Create('Socket linger option not available', SocketGetLastError);
  Linger := Opt.l_onoff;
  LingerTimeSec := Opt.l_linger;
end;

procedure SetSocketLinger(const SocketHandle: TSocketHandle;
    const Linger: Boolean; const LingerTimeSec: Integer);
var Opt : TLinger;
begin
  Opt.l_onoff := Linger;
  Opt.l_linger := LingerTimeSec;
  if SocketSetSockOpt(SocketHandle, SOL_SOCKET, SO_LINGER, @Opt, Sizeof(Opt)) < 0 then
    raise ESocketLib.Create('Socket linger option not set', SocketGetLastError);
end;
{$ENDIF}

function GetSocketBroadcast(const SocketHandle: TSocketHandle): Boolean;
var Opt : LongBool;
    OptLen : Int32;
begin
  OptLen := Sizeof(Opt);
  if SocketGetSockOpt(SocketHandle, SOL_SOCKET, SO_BROADCAST, @Opt, OptLen) < 0 then
    raise ESocketLib.Create('Socket broadcast option not available', SocketGetLastError);
  Result := Opt;
end;

procedure SetSocketBroadcast(const SocketHandle: TSocketHandle;
    const Broadcast: Boolean);
var Opt : LongBool;
begin
  Opt := Broadcast;
  if SocketSetSockOpt(SocketHandle, SOL_SOCKET, SO_BROADCAST, @Opt, Sizeof(Opt)) < 0 then
    raise ESocketLib.Create('Socket broadcast option not set', SocketGetLastError);
end;

{$IFDEF SOCKETLIB_WIN}
function GetSocketMulticastTTL(const SocketHandle: TSocketHandle): Integer;
var Opt : Int32;
    OptLen : Integer;
begin
  Opt := -1;
  OptLen := Sizeof(Opt);
  if SocketGetSockOpt(SocketHandle, IPPROTO_IP, IP_MULTICAST_TTL, @Opt, OptLen) < 0 then
    raise ESocketLib.Create('Socket multicast TTL option not available', SocketGetLastError);
  Result := Opt;
end;

procedure SetSocketMulticastTTL(const SocketHandle: TSocketHandle; const TTL: Integer);
var Opt : Int32;
begin
  Opt := TTL;
  if SocketSetSockOpt(SocketHandle, IPPROTO_IP, IP_MULTICAST_TTL, @Opt, Sizeof(Opt)) < 0 then
    raise ESocketLib.Create('Socket multicast TTL option not set', SocketGetLastError);
end;
{$ENDIF}

function GetSocketTcpNoDelay(const SocketHandle: TSocketHandle): Boolean;
var Opt : LongBool;
    OptLen : Int32;
begin
  OptLen := Sizeof(Opt);
  if SocketGetSockOpt(SocketHandle, IPPROTO_TCP, TCP_NODELAY, @Opt, OptLen) < 0 then
    raise ESocketLib.Create('Socket TCP-NoDelay option not available', SocketGetLastError);
  Result := Opt;
end;

procedure SetSocketTcpNoDelay(const SocketHandle: TSocketHandle; const TcpNoDelay: Boolean);
var Opt : LongBool;
begin
  Opt := TcpNoDelay;
  if SocketSetSockOpt(SocketHandle, IPPROTO_TCP, TCP_NODELAY, @Opt, Sizeof(Opt)) < 0 then
    raise ESocketLib.Create('Socket TCP-NoDelay option not set', SocketGetLastError);
end;



{                                                                              }
{ Socket mode                                                                  }
{                                                                              }
procedure SetSocketBlocking(const SocketHandle: TSocketHandle; const Block: Boolean);
begin
  SetSockBlocking(TSocket(SocketHandle), Block);
end;

{$IFDEF SOCKETLIB_WIN}
function SocketAsynchronousEventsToEvents(const Events: TSocketAsynchronousEvents): Int32;
var E : Int32;
begin
  E := 0;
  if saeConnect in Events then
    E := E or FD_CONNECT;
  if saeClose in Events then
    E := E or FD_CLOSE;
  if saeRead in Events then
    E := E or FD_READ;
  if saeWrite in Events then
    E := E or FD_WRITE;
  if saeAccept in Events then
    E := E or FD_ACCEPT;
  Result := E;
end;

function EventsToSocketAsynchronousEvents(const Events: Int32): TSocketAsynchronousEvents;
var E : TSocketAsynchronousEvents;
begin
  E := [];
  if Events and FD_CONNECT <> 0 then
    Include(E, saeConnect);
  if Events and FD_CLOSE <> 0 then
    Include(E, saeClose);
  if Events and FD_READ <> 0 then
    Include(E, saeRead);
  if Events and FD_WRITE <> 0 then
    Include(E, saeWrite);
  if Events and FD_ACCEPT <> 0 then
    Include(E, saeAccept);
  Result := E;
end;

procedure SetSocketAsynchronous(
          const SocketHandle: TSocketHandle;
          const WindowHandle: HWND; const Msg: Integer;
          const Events: TSocketAsynchronousEvents);
var E : Int32;
begin
  E := SocketAsynchronousEventsToEvents(Events);
  if WSAAsyncSelect(SocketHandle, WindowHandle, Msg, E) < 0 then
    raise ESocketLib.Create('Asynchronous mode not set', SocketGetLastError);
end;
{$ENDIF}

function GetSocketAvailableToRecv(const SocketHandle: TSocketHandle): Integer;
begin
  Result := SockAvailableToRecv(SocketHandle);
end;



{                                                                              }
{ Test cases                                                                   }
{                                                                              }
{$IFDEF SOCKETLIB_TEST}
{$ASSERTIONS ON}
procedure Test;
var S : RawByteString;
    W : AddressStrArrayB;
    A : TIP4Addr;
    L : TIP4AddrArray;
    H : TSocket;
    P : Word;
    D : TSocketAddr;
    E : TSocketAddrArray;
    {$IFDEF SOCKETLIB_TEST_IP6}
    B : TIP6Addr;
    C : TIP6AddrArray;
    {$ENDIF}
begin
  Assert(Sizeof(TInAddr) = 4,                     'TInAddr');
  Assert(Sizeof(TIn6Addr) = 16,                   'TIn6Addr');

  Assert(IPAddressFamilyToAF(iaIP4) = AF_INET);
  Assert(AFToIPAddressFamily(AF_INET) = iaIP4);

  // IsIPAddress
  Assert(IsIP4AddressB('192.168.0.1', A),          'IsIPAddress');
  Assert((A.Addr8[0] = 192) and
         (A.Addr8[1] = 168) and
         (A.Addr8[2] = 0)   and
         (A.Addr8[3] = 1),                         'IsIPAddress');
  Assert(IP4AddressType(A) = inaPrivate,           'IPAddressType');
  Assert(IP4AddressStrB(A) = '192.168.0.1',        'IPAddressStr');
  Assert(IP4AddressStr(A) = '192.168.0.1',         'IPAddressStr');
  Assert(IsIP4AddressB('0.0.0.0', A),              'IsIPAddress');
  Assert(A.Addr32 = 0,                             'IsIPAddress');
  Assert(IsIP4Address('0.0.0.0', A),               'IsIPAddress');
  Assert(A.Addr32 = 0,                             'IsIPAddress');
  Assert(IsIP4AddressB('255.255.255.255', A),      'IsIPAddress');
  Assert(A.Addr32 = INADDR_BROADCAST,              'IsIPAddress');
  Assert(IP4AddressStrB(A) = '255.255.255.255',    'IPAddressStr');
  Assert(not IsIP4AddressB('', A),                 'IsIPAddress');
  Assert(not IsIP4AddressB('192.168.0.', A),       'IsIPAddress');
  Assert(not IsIP4AddressB('192.168.0', A),        'IsIPAddress');
  Assert(not IsIP4AddressB('192.168.0.256', A),    'IsIPAddress');
  {$IFNDEF SOCKETLIB_POSIX_DELPHI}
  Assert(SocketGetLastError = 0,                   'IsIPAddress');
  {$ENDIF}
  Assert(IsIP4AddressB('192.168.0.255', A),        'IsIPAddress');
  Assert(IP4AddressStrB(A) = '192.168.0.255',      'IPAddressStr');
  {$IFNDEF SOCKETLIB_POSIX_DELPHI}
  Assert(SocketGetLastError = 0,                   'IsIPAddress');
  {$ENDIF}

  // ResolveHost IP
  A := ResolveHostIP4B('192.168.0.1');
  Assert(IP4AddressStrB(A) = '192.168.0.1',        'ResolveHostIP4');
  Assert((A.Addr8[0] = 192) and
         (A.Addr8[1] = 168) and
         (A.Addr8[2] = 0)   and
         (A.Addr8[3] = 1),                        'ResolveHostIP4');
  InitSocketAddr(D, A, 80);
  Assert(D.AddrFamily = iaIP4,                    'PopulateSockAddr');
  Assert(D.Port = 80,                             'PopulateSockAddr');
  Assert(D.AddrIP4.Addr32 = A.Addr32,             'PopulateSockAddr');

  // ResolveHost: local
  S := LocalHostNameB;
  Assert(S <> '',                                 'LocalHostName');
  A := ResolveHostIP4B(S);
  Assert(A.Addr32 <> 0,                           'ResolveHostIP4');
  L := ResolveHostIP4ExB(S);
  Assert(Length(L) > 0,                           'ResolveHostIP4Ex');
  Assert(L[0].Addr32 <> INADDR_ANY,               'ResolveHostIP4Ex');
  {$IFDEF SOCKETLIB_TEST_IP6}
  B := ResolveHostIP6A(S);
  Assert(not IP6AddrIsZero(B),                    'ResolveHostIP6');
  C := ResolveHostIP6ExA(S);
  Assert(Length(C) > 0,                           'ResolveHostIP6Ex');
  {$ENDIF}
  E := ResolveHostExB(S, iaIP4);
  Assert(Length(E) > 0,                           'ResolveHost');
  Assert(E[0].AddrFamily = iaIP4,                 'ResolveHost');
  Assert(E[0].AddrIP4.Addr32 <> INADDR_ANY,       'ResolveHost');
  {$IFDEF SOCKETLIB_TEST_IP6}
  E := ResolveHostExA(S, iaIP6);
  Assert(Length(E) > 0,                           'ResolveHost');
  Assert(E[0].AddrFamily = iaIP6,                 'ResolveHost');
  Assert(not IP6AddrIsZero(E[0].AddrIP6),         'ResolveHost');
  {$ENDIF}
  S := GetRemoteHostNameB(A);
  Assert(S <> '',                                 'GetRemoteHostName');

  {$IFDEF SOCKETLIB_TEST_IP4_INTERNET}
  // ResolveHost: internet
  S := 'www.google.com';
  A := ResolveHostIP4A(S);
  Assert(A.Addr32 <> 0,                           'ResolveHostIP4');
  L := ResolveHostIP4ExA(S);
  Assert(Length(L) > 0,                           'ResolveHostIP4Ex');
  Assert(L[0].Addr32 <> INADDR_ANY,               'ResolveHostIP4Ex');
  {$IFDEF SOCKETLIB_TEST_OUTPUT}
  Write('{google:n=', Length(L), ':', IPAddressStr(L[0]), '}');
  {$ENDIF}
  {$ENDIF}

  // ResolvePort
  P := ResolvePortB('25', ipTCP);
  Assert(ntohs(P) = 25,                           'ResolvePort');
  P := ResolvePortB('http', ipTCP);
  Assert(ntohs(P) = 80,                           'ResolvePort');
  P := ResolvePort('http', ipTCP);
  Assert(ntohs(P) = 80,                           'ResolvePort');

  // LocalIPAddresses
  W := LocalIP4AddressesStrB;
  Assert(Length(W) > 0,                           'LocalIPAddresses');

  {$IFDEF SOCKETLIB_TEST_IP6}
  // IP6 addresses
  Assert(not IsIPAddress('', B),                  'IsIP6Address');
  Assert(IsIPAddress('::1', B),                   'IsIP6Address');
  Assert(IN6ADDR_IsLocalHost(B),                  'IN6ADDR_IsLocalHost');
  IN6ADDR_SetLocalHost(B);
  Assert(IN6ADDR_IsLocalHost(B),                  'IN6ADDR_SetLocalHost');
  Assert(IPAddressStr(B) = '::1',                 'IP6AddressStr');
  Assert(IsIPAddress('ffff::1', B),               'IsIP6Address');
  Assert(IPAddressStr(B) = 'ffff:0:0:0:0:0:0:1',  'IP6AddressStr');
  Assert(IsIPAddress('::', B),                    'IsIP6Address');
  Assert(IPAddressStr(B) = '::',                  'IP6AddressStr');
  Assert(IN6ADDR_IsZero(B),                       'IN6ADDR_IsZero');
  IN6ADDR_SetZero(B);
  Assert(IPAddressStr(B) = '::',                  'IN6ADDR_SetZero');
  Assert(IN6ADDR_IsZero(B),                       'IN6ADDR_IsZero');
  IN6ADDR_SetBroadcast(B);
  Assert(IPAddressStr(B) = 'ffff:0:0:0:0:0:0:1',  'IN6ADDR_SetBroadcast');
  Assert(IN6ADDR_IsBroadcast(B),                  'IN6ADDR_IsBroadcast');

  C := LocalIP6Addresses;
  Assert(Length(C) > 0,                           'LocalIP6Addresses');
  Assert(IPAddressStr(C[0]) = '::1',              'LocalIP6Addresses');
  S := GetRemoteHostName(C[0]);
  Assert(S <> '',                                 'GetRemoteHostName');

  B := ResolveHostIP6('ffff::1');
  Assert(IPAddressStr(B) = 'ffff:0:0:0:0:0:0:1',  'ResolveHostIP6');
  B := ResolveHostIP6(LocalHostName);
  Assert(IN6ADDR_IsLocalHost(B),                  'ResolveHostIP6');
  {$ENDIF}

  // Sockets
  H := AllocateSocketHandle(iaIP4, ipTCP);
  Assert(H <> 0,                                  'AllocateSocketHandle');
  Assert(SocketGetLastError = 0,                  'AllocateSocketHandle');
  SetSocketBlocking(H, True);
  Assert(SocketGetLastError = 0,                  'SetSocketBlocking');
  SetSocketBlocking(H, False);
  Assert(SocketGetLastError = 0,                  'SetSocketBlocking');
  Assert(SocketClose(H) = 0,                      'SocketClose');
  Assert(SocketGetLastError = 0,                  'SocketClose');

  H := AllocateSocketHandle(iaIP4, ipUDP);
  Assert(H <> 0,                                  'AllocateSocketHandle');
  Assert(SocketGetLastError = 0,                  'AllocateSocketHandle');
  Assert(SocketClose(H) = 0,                      'SocketClose');
  Assert(SocketGetLastError = 0,                  'SocketClose');

  {$IFDEF SOCKETLIB_TEST_IP6}
  H := AllocateSocketHandle(iaIP6, ipTCP);
  Assert(H <> 0,                                  'AllocateSocketHandle');
  Assert(SocketGetLastError = 0,                  'AllocateSocketHandle');
  Assert(SocketClose(H) = 0,                      'SocketClose');
  Assert(SocketGetLastError = 0,                  'SocketClose');

  H := AllocateSocketHandle(iaIP6, ipUDP);
  Assert(H <> 0,                                  'AllocateSocketHandle');
  Assert(SocketGetLastError = 0,                  'AllocateSocketHandle');
  Assert(SocketClose(H) = 0,                      'SocketClose');
  Assert(SocketGetLastError = 0,                  'SocketClose');
  {$ENDIF}

  {$IFDEF SOCKETLIB_WIN}
  {$IFNDEF OS_WIN64}
  // overlapped socket
  H := AllocateSocketHandle(iaIP4, ipTCP, True);
  Assert(H <> 0,                                  'AllocateSocketHandle');
  Assert(SocketGetLastError = 0,                  'AllocateSocketHandle');
  Assert(SocketClose(H) = 0,                      'SocketClose');
  Assert(SocketGetLastError = 0,                  'SocketClose');
  {$ENDIF}
  {$ENDIF}
end;
{$ENDIF}



{                                                                              }
{ Unit initialization and finalization                                         }
{                                                                              }
initialization
  InitializeLibLock;
finalization
  FinalizeLibLock;
end.

