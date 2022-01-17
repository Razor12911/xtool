{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals 5.00                                        }
{   File name:        flcTCPBuffer.pas                                         }
{   File version:     5.08                                                     }
{   Description:      TCP buffer.                                              }
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
{   2010/12/02  0.02  Revision.                                                }
{   2011/04/22  0.03  Simple test cases.                                       }
{   2011/06/16  0.04  Minor change in PeekPtr routine.                         }
{   2011/09/03  4.05  Revised for Fundamentals 4.                              }
{   2016/01/09  5.06  Revised for Fundamentals 5.                              }
{   2019/04/10  5.07  Change default buffer size.                              }
{   2019/12/29  5.08  Minimum buffer size.                                     }
{                                                                              }
{******************************************************************************}

{$INCLUDE ../flcInclude.inc}
{$INCLUDE flcTCP.inc}

unit flcTCPBuffer;

interface

uses
  { System }

  SysUtils,

  { Utils }

  flcStdTypes;



{                                                                              }
{ TCP Buffer                                                                   }
{                                                                              }
type
  ETCPBuffer = class(Exception);

  TTCPBuffer = record
    Ptr  : Pointer;
    Size : Integer;
    Min  : Int32;
    Max  : Int32;
    Head : Int32;
    Used : Int32;
  end;

const
  ETHERNET_MTU       = 1500;
  ETHERNET_MTU_JUMBO = 9000;

  TCP_BUFFER_DEFAULTMAXSIZE = ETHERNET_MTU_JUMBO * 8; // 72,000 bytes
  TCP_BUFFER_DEFAULTMINSIZE = ETHERNET_MTU * 6;       // 9,000 bytes

procedure TCPBufferInitialise(
          var TCPBuf: TTCPBuffer;
          const TCPBufMaxSize: Int32 = TCP_BUFFER_DEFAULTMAXSIZE;
          const TCPBufMinSize: Int32 = TCP_BUFFER_DEFAULTMINSIZE);
procedure TCPBufferFinalise(var TCPBuf: TTCPBuffer);

function  TCPBufferGetMaxSize(const TCPBuf: TTCPBuffer): Int32;  {$IFDEF UseInline}inline;{$ENDIF}
procedure TCPBufferSetMaxSize(
          var TCPBuf: TTCPBuffer;
          const MaxSize: Int32);

function  TCPBufferGetMinSize(const TCPBuf: TTCPBuffer): Int32;  {$IFDEF UseInline}inline;{$ENDIF}
procedure TCPBufferSetMinSize(
          var TCPBuf: TTCPBuffer;
          const MinSize: Int32);

procedure TCPBufferPack(var TCPBuf: TTCPBuffer);
procedure TCPBufferResize(
          var TCPBuf: TTCPBuffer;
          const TCPBufSize: Int32);
procedure TCPBufferExpand(
          var TCPBuf: TTCPBuffer;
          const Size: Int32);
procedure TCPBufferShrink(var TCPBuf: TTCPBuffer);
procedure TCPBufferMinimize(var TCPBuf: TTCPBuffer);
procedure TCPBufferClear(var TCPBuf: TTCPBuffer);

function  TCPBufferAddPtr(
          var TCPBuf: TTCPBuffer;
          const Size: Int32): Pointer;
procedure TCPBufferAdded(
          var TCPBuf: TTCPBuffer;
          const Size: Int32);
procedure TCPBufferAddBuf(
          var TCPBuf: TTCPBuffer;
          const Buf; const Size: Int32);

function  TCPBufferPeekPtr(
          const TCPBuf: TTCPBuffer;
          var BufPtr: Pointer): Int32; {$IFDEF UseInline}inline;{$ENDIF}
function  TCPBufferPeek(
          var TCPBuf: TTCPBuffer;
          var Buf; const Size: Int32): Int32;
function  TCPBufferPeekByte(
          var TCPBuf: TTCPBuffer;
          out B: Byte): Boolean;

function  TCPBufferRemove(
          var TCPBuf: TTCPBuffer;
          var Buf; const Size: Int32): Int32;
function  TCPBufferRemoveBuf(
          var TCPBuf: TTCPBuffer;
          var Buf; const Size: Int32): Boolean;

function  TCPBufferDiscard(
          var TCPBuf: TTCPBuffer;
          const Size: Int32): Int32;

function  TCPBufferUsed(const TCPBuf: TTCPBuffer): Int32;        {$IFDEF UseInline}inline;{$ENDIF}
function  TCPBufferEmpty(const TCPBuf: TTCPBuffer): Boolean;     {$IFDEF UseInline}inline;{$ENDIF}
function  TCPBufferAvailable(const TCPBuf: TTCPBuffer): Int32;   {$IFDEF UseInline}inline;{$ENDIF}

function  TCPBufferPtr(const TCPBuf: TTCPBuffer): Pointer;       {$IFDEF UseInline}inline;{$ENDIF}

function  TCPBufferLocateByteChar(const TCPBuf: TTCPBuffer;
          const Delimiter: ByteCharSet; const MaxSize: Integer): Int32;



implementation



{                                                                              }
{ Resource strings                                                             }
{                                                                              }
const
  SBufferOverflow = 'Buffer overflow';



{                                                                              }
{ TCP Buffer                                                                   }
{                                                                              }

// Initialise a TCP buffer
procedure TCPBufferInitialise(
          var TCPBuf: TTCPBuffer;
          const TCPBufMaxSize: Int32;
          const TCPBufMinSize: Int32);
var
  L, M : Int32;
begin
  TCPBuf.Ptr := nil;
  TCPBuf.Size := 0;
  TCPBuf.Head := 0;
  TCPBuf.Used := 0;
  L := TCPBufMinSize;
  if L < 0 then
    L := TCP_BUFFER_DEFAULTMINSIZE;
  M := TCPBufMaxSize;
  if M < 0 then
    M := TCP_BUFFER_DEFAULTMAXSIZE;
  if L > M then
    L := M;
  TCPBuf.Min := L;
  TCPBuf.Max := M;
  if L > 0 then
    GetMem(TCPBuf.Ptr, L);
  TCPBuf.Size := L;
end;

// Finalise a TCP buffer
procedure TCPBufferFinalise(var TCPBuf: TTCPBuffer);
var
  P : Pointer;
begin
  P := TCPBuf.Ptr;
  if Assigned(P) then
    begin
      TCPBuf.Ptr := nil;
      FreeMem(P);
    end;
  TCPBuf.Size := 0;
end;

// Gets maximum buffer size
function TCPBufferGetMaxSize(const TCPBuf: TTCPBuffer): Int32;
begin
  Result := TCPBuf.Max;
end;

// Sets maximum buffer size
// Note: This limit is not enforced. It is used by TCPBufferAvailable.
procedure TCPBufferSetMaxSize(
          var TCPBuf: TTCPBuffer;
          const MaxSize: Int32);
var
  L : Int32;
begin
  L := MaxSize;
  if L < 0 then
    L := TCP_BUFFER_DEFAULTMAXSIZE;
  TCPBuf.Max := L;
end;

// Gets minimum buffer size
function TCPBufferGetMinSize(const TCPBuf: TTCPBuffer): Int32;
begin
  Result := TCPBuf.Min;
end;

procedure TCPBufferSetMinSize(
          var TCPBuf: TTCPBuffer;
          const MinSize: Int32);
var
  L : Int32;
begin
  L := MinSize;
  if L < 0 then
    L := TCP_BUFFER_DEFAULTMINSIZE;
  TCPBuf.Min := L;
end;

// Pack a TCP buffer
// Moves data to front of buffer
// Post: TCPBuf.Head = 0
procedure TCPBufferPack(var TCPBuf: TTCPBuffer);
var
  P, Q : PByte;
  U, H : Int32;
begin
  H := TCPBuf.Head;
  if H <= 0 then
    exit;
  U := TCPBuf.Used;
  if U <= 0 then
    begin
      TCPBuf.Head := 0;
      exit;
    end;
  Assert(Assigned(TCPBuf.Ptr));
  P := TCPBuf.Ptr;
  Q := P;
  Inc(P, H);
  Move(P^, Q^, U);
  TCPBuf.Head := 0;
end;

// Resize a TCP buffer
// New buffer size must be large enough to hold existing data
// Post: TCPBuf.Size = TCPBufSize
procedure TCPBufferResize(
          var TCPBuf: TTCPBuffer;
          const TCPBufSize: Int32);
var
  U, L : Int32;
begin
  L := TCPBufSize;
  U := TCPBuf.Used;
  // treat negative TCPBufSize parameter as zero
  if L < 0 then
    L := 0;
  // check if shrinking buffer to less than used size
  if U > L then
    raise ETCPBuffer.Create(SBufferOverflow);
  // check if packing required to fit buffer
  if U + TCPBuf.Head > L then
    TCPBufferPack(TCPBuf);
  Assert(U + TCPBuf.Head <= L);
  // resize
  ReallocMem(TCPBuf.Ptr, L);
  TCPBuf.Size := L;
end;

// Expand a TCP buffer
// Expands the size of the TCP buffer to at least Size
procedure TCPBufferExpand(
          var TCPBuf: TTCPBuffer;
          const Size: Int32);
var
  S : Int32;
  N : Int64;
  I : Int64;
begin
  S := TCPBuf.Size;
  N := Size;
  // check if expansion not required
  if N <= S then
    exit;
  // scale up new size proportional to current size
  // increase by at least quarter of current size
  // this reduces the number of resizes in growing buffers
  I := S + (S div 4);
  if N < I then
    N := I;
  // ensure new size is multiple of MTU size
  I := N mod ETHERNET_MTU;
  if I > 0 then
    Inc(N, ETHERNET_MTU - I);
  // resize buffer
  Assert(N >= Size);
  TCPBufferResize(TCPBuf, N);
end;

// Shrink the size of a TCP buffer to release all unused memory
// Post: TCPBuf.Used = TCPBuf.Size and TCPBuf.Head = 0
procedure TCPBufferShrink(var TCPBuf: TTCPBuffer);
var
  S, U : Int32;
begin
  S := TCPBuf.Size;
  if S <= 0 then
    exit;
  U := TCPBuf.Used;
  if U = 0 then
    begin
      TCPBufferResize(TCPBuf, 0);
      TCPBuf.Head := 0;
      exit;
    end;
  if U = S then
    exit;
  TCPBufferPack(TCPBuf);        // move data to front of buffer
  TCPBufferResize(TCPBuf, U);   // set size equal to used bytes
  Assert(TCPBuf.Used = TCPBuf.Size);
end;

// Applies Min parameter to allocated memory
procedure TCPBufferMinimize(var TCPBuf: TTCPBuffer); {$IFDEF UseInline}inline;{$ENDIF}
var
  Mi : Int32;
begin
  Mi := TCPBuf.Min;
  if Mi >= 0 then
    if TCPBuf.Used <= Mi then
      if TCPBuf.Size > Mi then
        TCPBufferResize(TCPBuf, Mi);
end;

// Clear the data from a TCP buffer
procedure TCPBufferClear(var TCPBuf: TTCPBuffer); {$IFDEF UseInline}inline;{$ENDIF}
begin
  TCPBuf.Used := 0;
  TCPBuf.Head := 0;
  TCPBufferMinimize(TCPBuf);
end;

// Returns a pointer to position in buffer to add new data of Size
// Handles resizing and packing of buffer to fit new data
function TCPBufferAddPtr(
         var TCPBuf: TTCPBuffer;
         const Size: Int32): Pointer; {$IFDEF UseInline}inline;{$ENDIF}
var
  P : PByte;
  U : Int32;
  L : Int64;
  H : Int32;
begin
  // return nil if nothing to add
  if Size <= 0 then
    begin
      Result := nil;
      exit;
    end;
  U := TCPBuf.Used;
  L := U + Size;
  // resize if necessary
  if L > TCPBuf.Size then
    TCPBufferExpand(TCPBuf, L);
  // pack if necessary
  if TCPBuf.Head + L > TCPBuf.Size then
    TCPBufferPack(TCPBuf);
  // buffer should now be large enough for new data
  H := TCPBuf.Head;
  Assert(TCPBuf.Size > 0);
  Assert(H + L <= TCPBuf.Size);
  // get buffer pointer
  Assert(Assigned(TCPBuf.Ptr));
  P := TCPBuf.Ptr;
  Inc(P, H);
  Inc(P, U);
  Result := P;
end;

// Increases data used in buffer by Size.
// TCPBufferAdded should only be called in conjuction with TCPBufferAddPtr.
procedure TCPBufferAdded(
          var TCPBuf: TTCPBuffer;
          const Size: Int32);
begin
  if Size <= 0 then
    exit;
  Assert(TCPBuf.Head + TCPBuf.Used + Size <= TCPBuf.Size);
  Inc(TCPBuf.Used, Size);
end;

// Adds new data from a buffer to a TCP buffer
procedure TCPBufferAddBuf(
          var TCPBuf: TTCPBuffer;
          const Buf; const Size: Int32); {$IFDEF UseInline}inline;{$ENDIF}
var
  P : PByte;
begin
  if Size <= 0 then
    exit;
  // get TCP buffer pointer
  P := TCPBufferAddPtr(TCPBuf, Size);
  // move user buffer to TCP buffer
  Assert(Assigned(P));
  Move(Buf, P^, Size);
  Inc(TCPBuf.Used, Size);
  Assert(TCPBuf.Head + TCPBuf.Used <= TCPBuf.Size);
end;

// Peek TCP buffer
// Returns the number of bytes available to peek
function TCPBufferPeekPtr(
         const TCPBuf: TTCPBuffer;
         var BufPtr: Pointer): Int32; {$IFDEF UseInline}inline;{$ENDIF}
var
  P : PByte;
  L : Int32;
begin
  // handle empty TCP buffer
  L := TCPBuf.Used;
  if L <= 0 then
    begin
      BufPtr := nil;
      Result := 0;
      exit;
    end;
  // get buffer pointer
  Assert(TCPBuf.Head + L <= TCPBuf.Size);
  Assert(Assigned(TCPBuf.Ptr));
  P := TCPBuf.Ptr;
  Inc(P, TCPBuf.Head);
  BufPtr := P;
  // return size
  Result := L;
end;

// Peek data from a TCP buffer
// Returns the number of bytes actually available and copied into the buffer
function TCPBufferPeek(
         var TCPBuf: TTCPBuffer;
         var Buf; const Size: Int32): Int32; {$IFDEF UseInline}inline;{$ENDIF}
var
  P : Pointer;
  L : Int32;
begin
  // handle peeking zero bytes
  if Size <= 0 then
    begin
      Result := 0;
      exit;
    end;
  L := TCPBufferPeekPtr(TCPBuf, P);
  // peek from TCP buffer
  if L > Size then
    L := Size;
  Move(P^, Buf, L);
  Result := L;
end;

// Peek byte from a TCP buffer
// Returns True if a byte is available
function TCPBufferPeekByte(
         var TCPBuf: TTCPBuffer;
         out B: Byte): Boolean;
var
  P : Pointer;
  L : Int32;
begin
  L := TCPBufferPeekPtr(TCPBuf, P);
  // peek from TCP buffer
  if L = 0 then
    Result := False
  else
    begin
      B := PByte(P)^;
      Result := True;
    end;
end;

// Remove data from a TCP buffer
// Returns the number of bytes actually available and copied into the user buffer
function TCPBufferRemove(
         var TCPBuf: TTCPBuffer;
         var Buf; const Size: Int32): Int32; {$IFDEF UseInline}inline;{$ENDIF}
var
  L, H, U : Int32;
begin
  // peek data from buffer
  L := TCPBufferPeek(TCPBuf, Buf, Size);
  if L = 0 then
    begin
      Result := 0;
      exit;
    end;
  // remove from TCP buffer
  H := TCPBuf.Head;
  U := TCPBuf.Used;
  Dec(U, L);
  if U = 0 then
    H := 0
  else
    Inc(H, L);
  TCPBuf.Head := H;
  TCPBuf.Used := U;
  TCPBufferMinimize(TCPBuf);
  Result := L;
end;

// Remove data from a TCP buffer
// Returns True if Size bytes were available and copied into the user buffer
// Returns False if Size bytes were not available
function TCPBufferRemoveBuf(
         var TCPBuf: TTCPBuffer;
         var Buf; const Size: Int32): Boolean; {$IFDEF UseInline}inline;{$ENDIF}
var
  H, U : Int32;
  P : PByte;
begin
  // handle invalid size
  if Size <= 0 then
    begin
      Result := False;
      exit;
    end;
  // check if enough data available
  U := TCPBuf.Used;
  if U < Size then
    begin
      Result := False;
      exit;
    end;
  // get buffer
  H := TCPBuf.Head;
  Assert(H + Size <= TCPBuf.Size);
  P := TCPBuf.Ptr;
  Assert(Assigned(P));
  Inc(P, H);
  Move(P^, Buf, Size);
  // remove from TCP buffer
  Dec(U, Size);
  if U = 0 then
    H := 0
  else
    Inc(H, Size);
  TCPBuf.Head := H;
  TCPBuf.Used := U;
  TCPBufferMinimize(TCPBuf);
  Result := True;
end;

// Discard a number of bytes from the TCP buffer
// Returns the number of bytes actually discarded from buffer
function TCPBufferDiscard(
         var TCPBuf: TTCPBuffer;
         const Size: Int32): Int32; {$IFDEF UseInline}inline;{$ENDIF}
var
  L, U : Int32;
begin
  // handle discarding zero bytes from buffer
  L := Size;
  if L <= 0 then
    begin
      Result := 0;
      exit;
    end;
  // handle discarding the complete buffer
  U := TCPBuf.Used;
  if L >= U then
    begin
      TCPBuf.Used := 0;
      TCPBuf.Head := 0;
      TCPBufferMinimize(TCPBuf);
      Result := U;
      exit;
    end;
  // discard partial buffer
  Inc(TCPBuf.Head, L);
  Dec(U, L);
  TCPBuf.Used := U;
  TCPBufferMinimize(TCPBuf);
  Result := L;
end;

// Returns number of bytes used in TCP buffer
function TCPBufferUsed(const TCPBuf: TTCPBuffer): Int32; {$IFDEF UseInline}inline;{$ENDIF}
begin
  Result := TCPBuf.Used;
end;

function TCPBufferEmpty(const TCPBuf: TTCPBuffer): Boolean; {$IFDEF UseInline}inline;{$ENDIF}
begin
  Result := TCPBuf.Used = 0;
end;

// Returns number of bytes available in TCP buffer
// Note: this function can return a negative number if the TCP buffer uses more bytes than set in Max
function TCPBufferAvailable(const TCPBuf: TTCPBuffer): Int32; {$IFDEF UseInline}inline;{$ENDIF}
begin
  Result := TCPBuf.Max - TCPBuf.Used;
end;

// Returns pointer to TCP buffer head
function TCPBufferPtr(const TCPBuf: TTCPBuffer): Pointer; {$IFDEF UseInline}inline;{$ENDIF}
var
  P : PByte;
begin
  Assert(Assigned(TCPBuf.Ptr));
  P := TCPBuf.Ptr;
  Inc(P, TCPBuf.Head);
  Result := P;
end;

// LocateByteChar
// Returns position of Delimiter in buffer
// Returns >= 0 if found in buffer
// Returns -1 if not found in buffer
// MaxSize specifies maximum bytes before delimiter, of -1 for no limit
function TCPBufferLocateByteChar(const TCPBuf: TTCPBuffer;
         const Delimiter: ByteCharSet; const MaxSize: Integer): Int32;
var
  BufSize : Int32;
  LocLen  : Int32;
  BufPtr  : PByteChar;
  I       : Int32;
begin
  if MaxSize = 0 then
    begin
      Result := -1;
      exit;
    end;
  BufSize := TCPBuf.Used;
  if BufSize <= 0 then
    begin
      Result := -1;
      exit;
    end;
  if MaxSize < 0 then
    LocLen := BufSize
  else
    if BufSize < MaxSize then
      LocLen := BufSize
    else
      LocLen := MaxSize;
  BufPtr := TCPBufferPtr(TCPBuf);
  for I := 0 to LocLen - 1 do
    if BufPtr^ in Delimiter then
      begin
        Result := I;
        exit;
      end
    else
      Inc(BufPtr);
  Result := -1;
end;



end.

