{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals TLS                                         }
{   File name:        flcTLSRandom.pas                                         }
{   File version:     5.02                                                     }
{   Description:      TLS Random                                               }
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
{   2020/05/09  5.02  Create flcTLSRandom unit from flcTLSUtils unit.          }
{                                                                              }
{******************************************************************************}

{$INCLUDE flcTLS.inc}

unit flcTLSRandom;

interface

uses
  { Utils }

  flcStdTypes;



{                                                                              }
{ Random                                                                       }
{                                                                              }
type
  TTLSRandom = packed record
    gmt_unix_time : Word32;
    random_bytes  : array[0..27] of Byte;
  end;
  PTLSRandom = ^TTLSRandom;

const
  TLSRandomSize = Sizeof(TTLSRandom);

procedure InitTLSRandom(var Random: TTLSRandom);
function  TLSRandomToStr(const Random: TTLSRandom): RawByteString;



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

  { Cipher }

  flcCipherRandom;



{                                                                              }
{ Random                                                                       }
{   gmt_unix_time     The current time and date in standard UNIX               }
{                     32-bit format according to the sender's                  }
{                     internal clock.  Clocks are not required to be           }
{                     set correctly by the basic SSL Protocol; higher          }
{                     level or application protocols may define                }
{                     additional requirements.                                 }
{   random_bytes      28 bytes generated by a secure random number             }
{                     generator.                                               }
{                                                                              }
procedure InitTLSRandom(var Random: TTLSRandom);
begin
  Random.gmt_unix_time := Word32(DateTimeToFileDate(Now));
  SecureRandomBuf(Random.random_bytes, SizeOf(Random.random_bytes));
end;

function TLSRandomToStr(const Random: TTLSRandom): RawByteString;
begin
  SetLength(Result, TLSRandomSize);
  Move(Random, Result[1], TLSRandomSize);
end;



{                                                                              }
{ Test                                                                         }
{                                                                              }
{$IFDEF TLS_TEST}
{$ASSERTIONS ON}
procedure Test;
begin
  Assert(TLSRandomSize = 32);
end;
{$ENDIF}



end.
