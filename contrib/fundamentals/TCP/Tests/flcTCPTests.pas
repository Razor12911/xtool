{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals 5.00                                        }
{   File name:        flcTCPTests.pas                                          }
{   File version:     5.11                                                     }
{   Description:      TCP tests.                                               }
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
{   2010/12/15  0.01  Test for TLS client/server.                              }
{   2011/01/02  0.02  Test for large buffers.                                  }
{   2011/04/22  0.03  Simple buffer tests.                                     }
{   2011/04/22  4.04  Test for multiple connections.                           }
{   2011/10/13  4.05  SSL3 tests.                                              }
{   2012/04/19  4.06  Test for stopping and restarting client.                 }
{   2015/04/26  4.07  Test for worker thread and blocking interface.           }
{   2016/01/08  5.08  Update for Fundamentals 5.                               }
{   2018/09/08  5.09  Server tests with high connection count.                 }
{   2019/04/16  5.10  Shutdown test.                                           }
{   2020/05/11  5.11  Move tests out into seperate units.                      }
{                                                                              }
{ Todo:                                                                        }
{ - Test case socks proxy                                                      }
{ - Test case buffer full/empty events                                         }
{ - Test case deferred shutdown                                                }
{ - Test case throttling                                                       }
{ - Test case read/write rate reporting                                        }
{ - Test case multiple proxies                                                 }
{ - Test case writing large chunks                                             }
{ - Test case performance                                                      }
{ - Test case stress test (throughput and number of connections)               }
{ - See SSL3 test case                                                         }
{******************************************************************************}

{$INCLUDE flcTCPTest.inc}

unit flcTCPTests;

interface



{                                                                              }
{ Test                                                                         }
{                                                                              }
{$IFDEF TCP_TEST}
procedure Test;
{$ENDIF}



implementation

uses
  flcTCPTest_Buffer
  {$IFDEF TCPCLIENT_TEST},
  flcTCPTest_Client
  {$ENDIF}
  {$IFDEF TCPCLIENT_TEST_TLS},
  flcTCPTest_ClientTLS
  {$ENDIF}
  {$IFDEF TCPSERVER_TEST},
  flcTCPTest_Server
  {$ENDIF}
  {$IFDEF TCPSERVER_TEST_TLS},
  flcTCPTest_ServerTLS
  {$ENDIF}
  {$IFDEF TCPCLIENTSERVER_TEST},
  flcTCPTest_ClientServer
  {$ENDIF}
  {$IFDEF TCPCLIENTSERVER_TEST_TLS},
  flcTCPTest_ClientServerTLS
  {$ENDIF}
  ;



{$IFDEF TCP_TEST}
{$ASSERTIONS ON}
procedure Test_Buffer;
begin
  flcTCPTest_Buffer.Test;
end;

{$IFDEF TCPCLIENT_TEST}
procedure Test_Client;
begin
  flcTCPTest_Client.Test;
  {$IFDEF TCPCLIENT_TEST_TLS}
  flcTCPTest_ClientTLS.Test;
  {$ENDIF}
end;
{$ENDIF}

{$IFDEF TCPSERVER_TEST}
procedure Test_Server;
begin
  flcTCPTest_Server.Test;
  {$IFDEF TCPSERVER_TEST_TLS}
  flcTCPTest_ServerTLS.Test;
  {$ENDIF}
end;
{$ENDIF}

{$IFDEF TCPCLIENTSERVER_TEST}
procedure Test_ClientServer;
begin
  flcTCPTest_ClientServer.Test;
  {$IFDEF TCPCLIENTSERVER_TEST_TLS}
  flcTCPTest_ClientServerTLS.Test;
  {$ENDIF}
end;
{$ENDIF}

procedure Test;
begin
  Test_Buffer;
  {$IFDEF TCPSERVER_TEST}
  Test_Server;
  {$ENDIF}
  {$IFDEF TCPCLIENT_TEST}
  Test_Client;
  {$ENDIF}
  {$IFDEF TCPCLIENTSERVER_TEST}
  Test_ClientServer;
  {$ENDIF}
end;
{$ENDIF}



end.

