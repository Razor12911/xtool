{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals 5.00                                        }
{   File name:        flcTCPUtils.pas                                          }
{   File version:     5.01                                                     }
{   Description:      TCP utilities.                                           }
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
{   2020/05/20  0.01  Initial version from unit flcTCPConnection.              }
{                     TCP timers helpers. TCP CompareMem helper.               }
{                                                                              }
{******************************************************************************}

{$INCLUDE ../flcInclude.inc}
{$INCLUDE flcTCP.inc}

unit flcTCPUtils;

interface

uses
  { Fundamentals }
  flcStdTypes;



{                                                                              }
{ TCP timer helpers                                                            }
{                                                                              }
function  TCPGetTick: Word64;
function  TCPTickDelta(const D1, D2: Word64): Int64;
function  TCPTickDeltaU(const D1, D2: Word64): Word64;


{                                                                              }
{ TCP CompareMem helper                                                        }
{                                                                              }
function  TCPCompareMem(const Buf1; const Buf2; const Count: Integer): Boolean;



implementation

uses
  { Fundamentals }
  flcTimers;



{                                                                              }
{ TCP timer helpers                                                            }
{                                                                              }
function TCPGetTick: Word64;
begin
  Result := GetMilliTick;
end;

function TCPTickDelta(const D1, D2: Word64): Int64;
begin
  Result := MilliTickDelta(D1, D2);
end;

function TCPTickDeltaU(const D1, D2: Word64): Word64;
begin
  Result := MilliTickDeltaU(D1, D2);
end;



{                                                                              }
{ TCP CompareMem helper                                                        }
{                                                                              }
{$IFDEF ASM386_DELPHI}
function TCPCompareMem(const Buf1; const Buf2; const Count: Integer): Boolean;
asm
      // EAX = Buf1, EDX = Buf2, ECX = Count
      OR      ECX, ECX
      JLE     @Fin1
      CMP     EAX, EDX
      JE      @Fin1
      PUSH    ESI
      PUSH    EDI
      MOV     ESI, EAX
      MOV     EDI, EDX
      MOV     EDX, ECX
      SHR     ECX, 2
      XOR     EAX, EAX
      REPE    CMPSD
      JNE     @Fin0
      MOV     ECX, EDX
      AND     ECX, 3
      REPE    CMPSB
      JNE     @Fin0
      INC     EAX
@Fin0:
      POP     EDI
      POP     ESI
      RET
@Fin1:
      MOV     AL, 1
end;
{$ELSE}
function TCPCompareMem(const Buf1; const Buf2; const Count: Integer): Boolean;
var P, Q : Pointer;
    D, I : Integer;
begin
  P := @Buf1;
  Q := @Buf2;
  if (Count <= 0) or (P = Q) then
    begin
      Result := True;
      exit;
    end;
  D := Word32(Count) div 4;
  for I := 1 to D do
    if PWord32(P)^ = PWord32(Q)^ then
      begin
        Inc(PWord32(P));
        Inc(PWord32(Q));
      end
    else
      begin
        Result := False;
        exit;
      end;
  D := Word32(Count) and 3;
  for I := 1 to D do
    if PByte(P)^ = PByte(Q)^ then
      begin
        Inc(PByte(P));
        Inc(PByte(Q));
      end
    else
      begin
        Result := False;
        exit;
      end;
  Result := True;
end;
{$ENDIF}



end.

