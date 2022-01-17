{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals 5.00                                        }
{   File name:        flcProtoBufUtils.pas                                     }
{   File version:     5.04                                                     }
{   Description:      Protocol Buffer utilities.                               }
{                                                                              }
{   Copyright:        Copyright (c) 2012-2018, David J Butler                  }
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
{   2012/04/14  0.01  VarInt encoding and decoding                             }
{   2012/04/15  0.02  SInt32 encoding and decoding                             }
{   2012/04/25  0.03  Improvements                                             }
{   2016/01/14  5.04  Revised for Fundamentals 5.                              }
{   2018/08/13  5.05  String type changes.                                     }
{                                                                              }
{ Supported compilers:                                                         }
{                                                                              }
{   Delphi XE7 Win32                    5.04  2016/01/14                       }
{   Delphi XE7 Win64                    5.04  2016/01/14                       }
{                                                                              }
{******************************************************************************}

{$INCLUDE flcProtoBuf.inc}

unit flcProtoBufUtils;

interface

uses
  { System }
  SysUtils,

  { Fundamentals }
  flcStdTypes;



{ SInt32 / SInt64 }

function  pbSInt32ToUInt32(const A: Int32): Word32;
function  pbUInt32ToSInt32(const A: Word32): Int32;
function  pbSInt64ToUInt64(const A: Int64): UInt64;
function  pbUInt64ToSInt64(const A: UInt64): Int64;



{ VarInt }

const
  pbMaxVarIntSizeEnc = 10;

type
  EpbVarIntError = class(Exception);

  TpbVarInt = record
    EncSize : Byte;
    EncData : array[0..pbMaxVarIntSizeEnc - 1] of Byte;
  end;

function  pbVarIntEquals(const A, B: TpbVarInt): Boolean;
function  pbVarIntIsZero(var A: TpbVarInt): Boolean;

procedure pbVarIntInitZero(var A: TpbVarInt);
procedure pbVarIntInitVarInt(var A: TpbVarInt; const B: TpbVarInt);

function  pbVarIntInitEncBuf(var A: TpbVarInt; const Buf; const BufSize: Integer): Integer;
procedure pbVarIntInitBinBuf(var A: TpbVarInt; const Buf; const BufSize: Integer);
procedure pbVarIntInitUInt32(var A: TpbVarInt; const B: Word32);
procedure pbVarIntInitSInt32(var A: TpbVarInt; const B: Int32);
procedure pbVarIntInitUInt64(var A: TpbVarInt; const B: UInt64);
procedure pbVarIntInitInt64(var A: TpbVarInt; const B: Int64);
procedure pbVarIntInitSInt64(var A: TpbVarInt; const B: Int64);

function  pbVarIntToEncBuf(const A: TpbVarInt; var Buf; const BufSize: Integer): Integer;
procedure pbVarIntToBinBuf(const A: TpbVarInt; var Buf; const BufSize: Integer);
function  pbVarIntToUInt32(const A: TpbVarInt): Word32;
function  pbVarIntToSInt32(const A: TpbVarInt): Int32;
function  pbVarIntToUInt64(const A: TpbVarInt): UInt64;
function  pbVarIntToInt64(const A: TpbVarInt): Int64;
function  pbVarIntToSInt64(const A: TpbVarInt): Int64;



{ Wire type }

type
  TpbWireType = (
    pwtVarInt     = 0,
    pwt64Bit      = 1,
    pwtVarBytes   = 2,
    pwtStartGroup = 3, // deprecated
    pwtEndGroup   = 4, // deprecated
    pwt32Bit      = 5,
    pwtNotUsed6   = 6,
    pwtNotUsed7   = 7
  );



{ Field key }

function  pbFieldKeyToUInt32(const FieldNumber: Int32; const WireType: TpbWireType): Word32;
procedure pbUInt32ToFieldKey(const A: Word32; var FieldNumber: Int32; var WireType: TpbWireType);



{ Encode }

type
  EpbEncodeError = class(Exception);

function  pbEncodeValueVarInt(var Buf; const BufSize: Integer; const Value: TpbVarInt): Integer;
function  pbEncodeValueInt32(var Buf; const BufSize: Integer; const Value: Int32): Integer;
function  pbEncodeValueInt64(var Buf; const BufSize: Integer; const Value: Int64): Integer;
function  pbEncodeValueUInt32(var Buf; const BufSize: Integer; const Value: Word32): Integer;
function  pbEncodeValueUInt64(var Buf; const BufSize: Integer; const Value: UInt64): Integer;
function  pbEncodeValueSInt32(var Buf; const BufSize: Integer; const Value: Int32): Integer;
function  pbEncodeValueSInt64(var Buf; const BufSize: Integer; const Value: Int64): Integer;
function  pbEncodeValueBool(var Buf; const BufSize: Integer; const Value: Boolean): Integer;
function  pbEncodeValueDouble(var Buf; const BufSize: Integer; const Value: Double): Integer;
function  pbEncodeValueFloat(var Buf; const BufSize: Integer; const Value: Single): Integer;
function  pbEncodeValueFixed32(var Buf; const BufSize: Integer; const Value: Word32): Integer;
function  pbEncodeValueFixed64(var Buf; const BufSize: Integer; const Value: UInt64): Integer;
function  pbEncodeValueSFixed32(var Buf; const BufSize: Integer; const Value: Int32): Integer;
function  pbEncodeValueSFixed64(var Buf; const BufSize: Integer; const Value: Int64): Integer;
function  pbEncodeValueString(var Buf; const BufSize: Integer; const Value: RawByteString): Integer;
function  pbEncodeValueBytes(var Buf; const BufSize: Integer; const ValueBuf; const ValueBufSize: Integer): Integer;

function  pbEncodeFieldKey(var Buf; const BufSize: Integer; const FieldNumber: Int32; const WireType: TpbWireType): Integer;
function  pbEncodeFieldVarBytesHdr(var Buf; const BufSize: Integer; const FieldNumber: Int32; const VarBytesSize: Integer): Integer;

function  pbEncodeFieldVarInt(var Buf; const BufSize: Integer; const FieldNumber: Int32; const Value: TpbVarInt): Integer;
function  pbEncodeFieldInt32(var Buf; const BufSize: Integer; const FieldNumber: Int32; const Value: Int32): Integer;
function  pbEncodeFieldInt64(var Buf; const BufSize: Integer; const FieldNumber: Int32; const Value: Int64): Integer;
function  pbEncodeFieldUInt32(var Buf; const BufSize: Integer; const FieldNumber: Int32; const Value: Word32): Integer;
function  pbEncodeFieldUInt64(var Buf; const BufSize: Integer; const FieldNumber: Int32; const Value: UInt64): Integer;
function  pbEncodeFieldSInt32(var Buf; const BufSize: Integer; const FieldNumber: Int32; const Value: Int32): Integer;
function  pbEncodeFieldSInt64(var Buf; const BufSize: Integer; const FieldNumber: Int32; const Value: Int64): Integer;
function  pbEncodeFieldBool(var Buf; const BufSize: Integer; const FieldNumber: Int32; const Value: Boolean): Integer;
function  pbEncodeFieldDouble(var Buf; const BufSize: Integer; const FieldNumber: Int32; const Value: Double): Integer;
function  pbEncodeFieldFloat(var Buf; const BufSize: Integer; const FieldNumber: Int32; const Value: Single): Integer;
function  pbEncodeFieldFixed32(var Buf; const BufSize: Integer; const FieldNumber: Int32; const Value: Word32): Integer;
function  pbEncodeFieldFixed64(var Buf; const BufSize: Integer; const FieldNumber: Int32; const Value: UInt64): Integer;
function  pbEncodeFieldSFixed32(var Buf; const BufSize: Integer; const FieldNumber: Int32; const Value: Int32): Integer;
function  pbEncodeFieldSFixed64(var Buf; const BufSize: Integer; const FieldNumber: Int32; const Value: Int64): Integer;
function  pbEncodeFieldString(var Buf; const BufSize: Integer; const FieldNumber: Int32; const Value: RawByteString): Integer;
function  pbEncodeFieldBytes(var Buf; const BufSize: Integer; const FieldNumber: Int32; const ValueBuf; const ValueBufSize: Integer): Integer;



{ Decode }

type
  EpbDecodeError = class(Exception);

function  pbDecodeValueInt32(const Buf; const BufSize: Integer; var Value: Int32): Integer;
function  pbDecodeValueInt64(const Buf; const BufSize: Integer; var Value: Int64): Integer;
function  pbDecodeValueUInt32(const Buf; const BufSize: Integer; var Value: Word32): Integer;
function  pbDecodeValueUInt64(const Buf; const BufSize: Integer; var Value: UInt64): Integer;
function  pbDecodeValueSInt32(const Buf; const BufSize: Integer; var Value: Int32): Integer;
function  pbDecodeValueSInt64(const Buf; const BufSize: Integer; var Value: Int64): Integer;
function  pbDecodeValueDouble(const Buf; const BufSize: Integer; var Value: Double): Integer;
function  pbDecodeValueFloat(const Buf; const BufSize: Integer; var Value: Single): Integer;
function  pbDecodeValueFixed32(const Buf; const BufSize: Integer; var Value: Word32): Integer;
function  pbDecodeValueFixed64(const Buf; const BufSize: Integer; var Value: UInt64): Integer;
function  pbDecodeValueSFixed32(const Buf; const BufSize: Integer; var Value: Int32): Integer;
function  pbDecodeValueSFixed64(const Buf; const BufSize: Integer; var Value: Int64): Integer;
function  pbDecodeValueBool(const Buf; const BufSize: Integer; var Value: Boolean): Integer;
function  pbDecodeValueString(const Buf; const BufSize: Integer; var Value: RawByteString): Integer;
function  pbDecodeValueBytes(const Buf; const BufSize: Integer; var ValueBuf; const ValueBufSize: Integer): Integer;

type
  TpbProtoBufDecodeField = record
    FieldNum         : Int32;
    WireType         : TpbWireType;
    WireDataPtr      : Pointer;
    WireDataSize     : Int32;
    ValueVarInt      : TpbVarInt;
    ValueVarBytesPtr : Pointer;
    ValueVarBytesLen : Int32;
  end;
  PpbProtoBufDecodeField = ^TpbProtoBufDecodeField;

  TpbProtoBufDecodeFieldCallbackProc =
      procedure (const Field: TpbProtoBufDecodeField;
                 const Data: Pointer);

procedure pbDecodeProtoBuf(
          const Buf; const BufSize: Integer;
          const CallbackProc: TpbProtoBufDecodeFieldCallbackProc;
          const CallbackData: Pointer);

procedure pbDecodeFieldInt32(const Field: TpbProtoBufDecodeField; var Value: Int32);
procedure pbDecodeFieldInt64(const Field: TpbProtoBufDecodeField; var Value: Int64);
procedure pbDecodeFieldUInt32(const Field: TpbProtoBufDecodeField; var Value: Word32);
procedure pbDecodeFieldUInt64(const Field: TpbProtoBufDecodeField; var Value: UInt64);
procedure pbDecodeFieldSInt32(const Field: TpbProtoBufDecodeField; var Value: Int32);
procedure pbDecodeFieldSInt64(const Field: TpbProtoBufDecodeField; var Value: Int64);
procedure pbDecodeFieldDouble(const Field: TpbProtoBufDecodeField; var Value: Double);
procedure pbDecodeFieldFloat(const Field: TpbProtoBufDecodeField; var Value: Single);
procedure pbDecodeFieldFixed32(const Field: TpbProtoBufDecodeField; var Value: Word32);
procedure pbDecodeFieldFixed64(const Field: TpbProtoBufDecodeField; var Value: UInt64);
procedure pbDecodeFieldSFixed32(const Field: TpbProtoBufDecodeField; var Value: Int32);
procedure pbDecodeFieldSFixed64(const Field: TpbProtoBufDecodeField; var Value: Int64);
procedure pbDecodeFieldBool(const Field: TpbProtoBufDecodeField; var Value: Boolean);
function  pbDecodeFieldString(const Field: TpbProtoBufDecodeField; var Value: RawByteString): Integer;
function  pbDecodeFieldBytes(const Field: TpbProtoBufDecodeField; var ValueBuf; const ValueBufSize: Integer): Integer;



{                                                                              }
{ Test cases                                                                   }
{                                                                              }
{$IFDEF PROTOBUF_TEST}
procedure Test;
{$ENDIF}



implementation



{ Strings }

const
  SErr_BufferTooSmall  = 'Buffer too small';
  SErr_Overflow        = 'Overflow';
  SErr_InvalidVarInt   = 'Invalid VarInt';
  SErr_InvalidBuffer   = 'Invalid buffer';
  SErr_InvalidWireType = 'Invalid wire type';



{ SInt32 }

// returns SInt32 encoded to Word32 using 'ZigZag' encoding
function pbSInt32ToUInt32(const A: Int32): Word32;
var I : Int64;
begin
  if A < 0 then
    begin
      // use Int64 value to negate A without overflow
      I := A;
      I := -I;
      // encode ZigZag
      Result := (Word32(I) - 1) * 2 + 1
    end
  else
    Result := Word32(A) * 2;
end;

// returns SInt32 decoded from Word32 using 'ZigZag' encoding
function pbUInt32ToSInt32(const A: Word32): Int32;
begin
  if A and 1 <> 0 then
    Result := -(A div 2) - 1
  else
    Result := A div 2;
end;

// returns SInt64 encoded to UInt64 using 'ZigZag' encoding
function pbSInt64ToUInt64(const A: Int64): UInt64;
var I : UInt64;
begin
  if A < 0 then
    begin
      // use two's complement to negate A without overflow
      I := not A;
      Inc(I);
      // encode ZigZag
      Dec(I);
      I := I * 2;
      Inc(I);
      Result := I;
    end
  else
    Result := UInt64(A) * 2;
end;

// returns SInt64 decoded from UInt64 using 'ZigZag' encoding
function pbUInt64ToSInt64(const A: UInt64): Int64;
var I : Int64;
begin
  if A and 1 <> 0 then
    begin
      I := A shr 1;
      I := -I;
      Dec(I);
      Result := I;
    end
  else
    begin
      I := A shr 1;
      Result := I;
    end;
end;



{ VarInt }

// returns true if two VarInts have equal values
function pbVarIntEquals(const A, B: TpbVarInt): Boolean;
var I, L : Integer;
begin
  L := A.EncSize;
  Result := L = B.EncSize;
  if not Result then
    exit;
  for I := 0 to L - 1 do
    if A.EncData[I] <> B.EncData[I] then
      begin
        Result := False;
        exit;
      end;
end;

function pbVarIntIsZero(var A: TpbVarInt): Boolean;
begin
  Result :=
      (A.EncSize = 1) and
      (A.EncData[0] = 0);
end;

// initialises VarInt to zero
procedure pbVarIntInitZero(var A: TpbVarInt);
begin
  A.EncSize := 1;
  A.EncData[0] := 0;
end;

// initialises VarInt from another VarInt
procedure pbVarIntInitVarInt(var A: TpbVarInt; const B: TpbVarInt);
begin
  A.EncSize := B.EncSize;
  A.EncData := B.EncData;
end;

// initialises VarInt from a VarInt encoded buffer
// encoded buffer may be larger than the VarInt encoded in it
// returns the size of the VarInt in the encoded buffer
function pbVarIntInitEncBuf(var A: TpbVarInt; const Buf; const BufSize: Integer): Integer;
var L, I : Integer;
    P : PByte;
    C : Byte;
begin
  L := BufSize;
  if L <= 0 then
    begin
      // no buffer
      pbVarIntInitZero(A);
      Result := 0;
      exit;
    end;
  // copy and find size
  I := 0;
  P := @Buf;
  while I < L do
    begin
      if I + 1 > pbMaxVarIntSizeEnc then
        raise EpbVarIntError.Create(SErr_Overflow);
      C := P^;
      A.EncData[I] := C;
      if C and $80 = 0 then
        break;
      Inc(I);
      Inc(P);
    end;
  if I >= L then
    raise EpbVarIntError.Create(SErr_InvalidBuffer);
  Inc(I);
  A.EncSize := I;
  Result := I;
end;

// initialises VarInt from integer buffer
procedure pbVarIntInitBinBuf(var A: TpbVarInt; const Buf; const BufSize: Integer);
var L : Integer;
    P : PByte;
    I : Integer;
    BinBuf : Word;
    BinBufBits : ShortInt;
    BinByte : Byte;
    EncSize : Byte;

  procedure EncodeFromBuffer;
  var EncByte : Byte;
  begin
    // encode 7 bits
    EncByte := Byte(BinBuf and $7F);
    if EncSize < pbMaxVarIntSizeEnc then
      A.EncData[EncSize] := EncByte
    else
      if EncByte <> 0 then
        raise EpbVarIntError.Create(SErr_Overflow);
    Inc(EncSize);
    // remove 7 bits from buffer
    BinBuf := BinBuf shr 7;
    Dec(BinBufBits, 7);
  end;

begin
  L := BufSize;
  if L <= 0 then
    begin
      // no buffer
      pbVarIntInitZero(A);
      exit;
    end;
  // skip most significant zero bytes
  P := @Buf;
  Inc(P, L - 1);
  while (L > 0) and (P^ = 0) do
    begin
      Dec(P);
      Dec(L);
    end;
  if L <= 0 then
    begin
      // all zero values
      pbVarIntInitZero(A);
      exit;
    end;
  // encode
  P := @Buf;
  EncSize := 0;
  BinBuf := 0;
  BinBufBits := 0;
  for I := 0 to L - 1 do
    begin
      // buffer 8 bits
      BinByte := P^;
      BinBuf := BinBuf or (Word(BinByte) shl BinBufBits);
      Inc(BinBufBits, 8);
      Inc(P);
      // encode 7 bits
      EncodeFromBuffer;
      // encode further 7 bits if available
      if BinBufBits >= 7 then
        EncodeFromBuffer;
    end;
  // encode last partial byte
  if BinBufBits > 0 then
    EncodeFromBuffer;
  // find final encoding byte
  while (EncSize > 1) and (A.EncData[EncSize - 1] = 0) do
    Dec(EncSize);
  A.EncSize := EncSize;
  // mark non-final bytes
  if EncSize > 1 then
    begin
      P := @A.EncData[0];
      for I := 0 to EncSize - 2 do
        begin
          P^ := P^ or $80;
          Inc(P);
        end;
    end;
end;

// initialises VarInt from Word32
procedure pbVarIntInitUInt32(var A: TpbVarInt; const B: Word32);
begin
  if B < $80 then
    begin
      A.EncSize := 1;
      A.EncData[0] := Byte(B);
    end
  else
  if B < $4000 then
    begin
      A.EncSize := 2;
      A.EncData[0] := Byte(B and $7F) or $80;
      A.EncData[1] := Byte(B shr 7);
    end
  else
  if B < $200000 then
    begin
      A.EncSize := 3;
      A.EncData[0] := Byte(B          and $7F) or $80;
      A.EncData[1] := Byte((B shr 7)  and $7F) or $80;
      A.EncData[2] := Byte( B shr 14         );
    end
  else
  if B < $10000000 then
    begin
      A.EncSize := 4;
      A.EncData[0] := Byte(B          and $7F) or $80;
      A.EncData[1] := Byte((B shr 7)  and $7F) or $80;
      A.EncData[2] := Byte((B shr 14) and $7F) or $80;
      A.EncData[3] := Byte( B shr 21         );
    end
  else
    begin
      A.EncSize := 5;
      A.EncData[0] := Byte(B          and $7F) or $80;
      A.EncData[1] := Byte((B shr 7)  and $7F) or $80;
      A.EncData[2] := Byte((B shr 14) and $7F) or $80;
      A.EncData[3] := Byte((B shr 21) and $7F) or $80;
      A.EncData[4] := Byte( B shr 28         );
    end;
end;

procedure pbVarIntInitSInt32(var A: TpbVarInt; const B: Int32);
begin
  pbVarIntInitUInt32(A, pbSInt32ToUInt32(B));
end;

procedure pbVarIntInitUInt64(var A: TpbVarInt; const B: UInt64);
begin
  pbVarIntInitBinBuf(A, B, SizeOf(B));
end;

procedure pbVarIntInitInt64(var A: TpbVarInt; const B: Int64);
begin
  pbVarIntInitBinBuf(A, B, SizeOf(B));
end;

procedure pbVarIntInitSInt64(var A: TpbVarInt; const B: Int64);
begin
  pbVarIntInitUInt64(A, pbSInt64ToUInt64(B));
end;

// converts VarInt to encoded buffer
// returns size of the encoded VarInt
function pbVarIntToEncBuf(const A: TpbVarInt; var Buf; const BufSize: Integer): Integer;
var L : Integer;
begin
  L := A.EncSize;
  if BufSize >= L then
    Move(A.EncData[0], Buf, L);
  Result := L;
end;

// converts VarInt to integer buffer
procedure pbVarIntToBinBuf(const A: TpbVarInt; var Buf; const BufSize: Integer);
var BinBuf : Word;
    BinBufBits : ShortInt;
    BinBufP : PByte;
    BinBufLeft : Integer;
    I : Integer;
    EncByte : Byte;
    EncByteFin : Boolean;
    EncFin : Boolean;

  procedure EncodeToBuffer;
  var BinBufByte : Byte;
  begin
    BinBufByte := Byte(BinBuf and $FF);
    if BinBufLeft <= 0 then
      if BinBufByte = 0 then
        exit
      else
        raise EpbVarIntError.Create(SErr_Overflow);
    BinBufP^ := BinBufByte;
    Inc(BinBufP);
    Dec(BinBufLeft);
    BinBuf := BinBuf shr 8;
    Dec(BinBufBits, 8);
  end;

begin
  if BufSize <= 0 then
    raise EpbVarIntError.Create(SErr_Overflow);
  BinBufP := @Buf;
  BinBufLeft := BufSize;
  BinBuf := 0;
  BinBufBits := 0;
  for I := 0 to A.EncSize - 1 do
    begin
      // decode 7 bits from encoded buffer
      EncByte := A.EncData[I];
      EncFin := (I = A.EncSize - 1);
      EncByteFin := (EncByte and $80 = 0);
      if (EncFin and not EncByteFin) or
         (not EncFin and EncByteFin) then
        raise EpbVarIntError.Create(SErr_InvalidVarInt);
      BinBuf := BinBuf or (Word(EncByte and $7F) shl BinBufBits);
      Inc(BinBufBits, 7);
      // encode 8 bits to binary buffer if available
      if BinBufBits >= 8 then
        EncodeToBuffer;
    end;
  // encode final partial byte
  if BinBufBits > 0 then
    EncodeToBuffer;
  // fill rest of binary buffer with zero
  while BinBufLeft > 0 do
    begin
      BinBufP^ := 0;
      Inc(BinBufP);
      Dec(BinBufLeft);
    end;
end;

// returns VarInt value as Word32
function pbVarIntToUInt32(const A: TpbVarInt): Word32;
var B : Word32;
    I : Byte;
    EncByte : Byte;
begin
  Assert(A.EncSize > 0);
  Assert(A.EncSize <= pbMaxVarIntSizeEnc);
  // decode
  B := 0;
  I := 0;
  while I < A.EncSize do
    begin
      EncByte := A.EncData[I];
      if I = 4 then
        if EncByte and $F0 <> 0 then
          raise EpbVarIntError.Create(SErr_Overflow);
      B := B or (Word32(EncByte and $7F) shl (I * 7));
      Inc(I);
      if EncByte and $80 = 0 then // final byte marker
        begin
          if I < A.EncSize then // final byte marker before final byte reached
            raise EpbVarIntError.Create(SErr_InvalidVarInt);
          // finished
          Result := B;
          exit;
        end;
    end;
  // final byte reached without final byte marker
  raise EpbVarIntError.Create(SErr_InvalidVarInt);
end;

function pbVarIntToSInt32(const A: TpbVarInt): Int32;
begin
  Result := pbUInt32ToSInt32(pbVarIntToUInt32(A));
end;

function pbVarIntToUInt64(const A: TpbVarInt): UInt64;
begin
  pbVarIntToBinBuf(A, Result, SizeOf(Result));
end;

function pbVarIntToInt64(const A: TpbVarInt): Int64;
begin
  pbVarIntToBinBuf(A, Result, SizeOf(Result));
end;

function pbVarIntToSInt64(const A: TpbVarInt): Int64;
begin
  Result := pbUInt64ToSInt64(pbVarIntToUInt64(A));
end;



{ Field key }

function pbFieldKeyToUInt32(const FieldNumber: Int32; const WireType: TpbWireType): Word32;
begin
  Result := Ord(WireType) or (FieldNumber shl 3);
end;

procedure pbUInt32ToFieldKey(const A: Word32; var FieldNumber: Int32; var WireType: TpbWireType);
begin
  WireType := TpbWireType(A and 7);
  FieldNumber := A shr 3;
end;



{ Encode }

function pbEncodeValueVarInt(var Buf; const BufSize: Integer; const Value: TpbVarInt): Integer;
begin
  Result := pbVarIntToEncBuf(Value, Buf, BufSize);
end;

function pbEncodeValueInt32(var Buf; const BufSize: Integer; const Value: Int32): Integer;
var
  A : TpbVarInt;
begin
  pbVarIntInitUInt32(A, Word32(Value));
  Result := pbVarIntToEncBuf(A, Buf, BufSize);
end;

function pbEncodeValueInt64(var Buf; const BufSize: Integer; const Value: Int64): Integer;
var
  A : TpbVarInt;
begin
  pbVarIntInitBinBuf(A, Value, SizeOf(Value));
  Result := pbVarIntToEncBuf(A, Buf, BufSize);
end;

function pbEncodeValueUInt32(var Buf; const BufSize: Integer; const Value: Word32): Integer;
var
  A : TpbVarInt;
begin
  pbVarIntInitUInt32(A, Value);
  Result := pbVarIntToEncBuf(A, Buf, BufSize);
end;

function pbEncodeValueUInt64(var Buf; const BufSize: Integer; const Value: UInt64): Integer;
var
  A : TpbVarInt;
begin
  pbVarIntInitBinBuf(A, Value, SizeOf(Value));
  Result := pbVarIntToEncBuf(A, Buf, BufSize);
end;

function pbEncodeValueSInt32(var Buf; const BufSize: Integer; const Value: Int32): Integer;
var
  A : TpbVarInt;
begin
  pbVarIntInitSInt32(A, Value);
  Result := pbVarIntToEncBuf(A, Buf, BufSize);
end;

function pbEncodeValueSInt64(var Buf; const BufSize: Integer; const Value: Int64): Integer;
var
  A : TpbVarInt;
begin
  pbVarIntInitSInt64(A, Value);
  Result := pbVarIntToEncBuf(A, Buf, BufSize);
end;

function pbEncodeValueBool(var Buf; const BufSize: Integer; const Value: Boolean): Integer;
var
  A : TpbVarInt;
begin
  pbVarIntInitUInt32(A, Ord(Value));
  Result := pbVarIntToEncBuf(A, Buf, BufSize);
end;

function pbEncodeValueDouble(var Buf; const BufSize: Integer; const Value: Double): Integer;
begin
  if BufSize >= SizeOf(Value) then
    PDouble(@Buf)^ := Value;
  Result := SizeOf(Value);
end;

function pbEncodeValueFloat(var Buf; const BufSize: Integer; const Value: Single): Integer;
begin
  if BufSize >= SizeOf(Value) then
    PSingle(@Buf)^ := Value;
  Result := SizeOf(Value);
end;

function pbEncodeValueFixed32(var Buf; const BufSize: Integer; const Value: Word32): Integer;
begin
  if BufSize >= SizeOf(Value) then
    PWord32(@Buf)^ := Value;
  Result := SizeOf(Value);
end;

function pbEncodeValueFixed64(var Buf; const BufSize: Integer; const Value: UInt64): Integer;
begin
  if BufSize >= SizeOf(Value) then
    PUInt64(@Buf)^ := Value;
  Result := SizeOf(Value);
end;

function pbEncodeValueSFixed32(var Buf; const BufSize: Integer; const Value: Int32): Integer;
begin
  if BufSize >= SizeOf(Value) then
    PInt32(@Buf)^ := Value;
  Result := SizeOf(Value);
end;

function pbEncodeValueSFixed64(var Buf; const BufSize: Integer; const Value: Int64): Integer;
begin
  if BufSize >= SizeOf(Value) then
    PInt64(@Buf)^ := Value;
  Result := SizeOf(Value);
end;

function pbEncodeValueString(var Buf; const BufSize: Integer; const Value: RawByteString): Integer;
var
  P : PByte;
  L, N, J : Integer;
  A : TpbVarInt;
begin
  P := @Buf;
  L := BufSize;
  // string length as VarInt
  N := Length(Value);
  pbVarIntInitUInt32(A, Word32(N));
  J := pbVarIntToEncBuf(A, P^, L);
  Inc(P, J);
  Dec(L, J);
  // string content
  if N > 0 then
    begin
      if L >= N then
        Move(Pointer(Value)^, P^, N);
      Dec(L, N);
    end;
  Result := BufSize - L;
end;

function pbEncodeValueBytes(var Buf; const BufSize: Integer; const ValueBuf; const ValueBufSize: Integer): Integer;
var
  P : PByte;
  L, N, J : Integer;
  A : TpbVarInt;
begin
  P := @Buf;
  L := BufSize;
  // buffer size as VarInt
  N := ValueBufSize;
  pbVarIntInitUInt32(A, Word32(N));
  J := pbVarIntToEncBuf(A, P^, L);
  Inc(P, J);
  Dec(L, J);
  // buffer content
  if N > 0 then
    begin
      if L >= N then
        Move(ValueBuf, P^, N);
      Dec(L, N);
    end;
  Result := BufSize - L;
end;

function pbEncodeFieldKey(var Buf; const BufSize: Integer;
         const FieldNumber: Int32; const WireType: TpbWireType): Integer;
var
  FieldKey : Word32;
  A : TpbVarInt;
begin
  FieldKey := pbFieldKeyToUInt32(FieldNumber, WireType);
  pbVarIntInitUInt32(A, FieldKey);
  Result := pbVarIntToEncBuf(A, Buf, BufSize);
end;

function pbEncodeFieldVarBytesHdr(var Buf; const BufSize: Integer;
         const FieldNumber: Int32; const VarBytesSize: Integer): Integer;
var
  P : PByte;
  L : Integer;
  I : Integer;
  A : TpbVarInt;
begin
  if VarBytesSize < 0 then
    raise EpbEncodeError.Create(SErr_InvalidBuffer);
  P := @Buf;
  L := BufSize;
  I := pbEncodeFieldKey(P^, L, FieldNumber, pwtVarBytes);
  Dec(L, I);
  Inc(P, I);
  pbVarIntInitUInt32(A, Word32(VarBytesSize));
  I := pbVarIntToEncBuf(A, P^, L);
  Dec(L, I);
  Result := BufSize - L;
end;

function pbEncodeFieldVarInt(var Buf; const BufSize: Integer;
         const FieldNumber: Int32; const Value: TpbVarInt): Integer;
var
  P : PByte;
  L : Integer;
  I : Integer;
begin
  P := @Buf;
  L := BufSize;
  I := pbEncodeFieldKey(P^, L, FieldNumber, pwtVarInt);
  Inc(P, I);
  Dec(L, I);
  I := pbVarIntToEncBuf(Value, P^, L);
  Dec(L, I);
  Result := BufSize - L;
end;

function pbEncodeFieldInt32(var Buf; const BufSize: Integer;
         const FieldNumber: Int32; const Value: Int32): Integer;
var
  A : TpbVarInt;
begin
  pbVarIntInitUInt32(A, Word32(Value));
  Result := pbEncodeFieldVarInt(Buf, BufSize, FieldNumber, A);
end;

function pbEncodeFieldInt64(var Buf; const BufSize: Integer;
         const FieldNumber: Int32; const Value: Int64): Integer;
var
  A : TpbVarInt;
begin
  pbVarIntInitBinBuf(A, Value, SizeOf(Value));
  Result := pbEncodeFieldVarInt(Buf, BufSize, FieldNumber, A);
end;

function pbEncodeFieldUInt32(var Buf; const BufSize: Integer;
         const FieldNumber: Int32; const Value: Word32): Integer;
var
  A : TpbVarInt;
begin
  pbVarIntInitUInt32(A, Value);
  Result := pbEncodeFieldVarInt(Buf, BufSize, FieldNumber, A);
end;

function pbEncodeFieldUInt64(var Buf; const BufSize: Integer;
         const FieldNumber: Int32; const Value: UInt64): Integer;
var
  A : TpbVarInt;
begin
  pbVarIntInitBinBuf(A, Value, SizeOf(Value));
  Result := pbEncodeFieldVarInt(Buf, BufSize, FieldNumber, A);
end;

function pbEncodeFieldSInt32(var Buf; const BufSize: Integer;
         const FieldNumber: Int32; const Value: Int32): Integer;
var
  A : TpbVarInt;
begin
  pbVarIntInitSInt32(A, Value);
  Result := pbEncodeFieldVarInt(Buf, BufSize, FieldNumber, A);
end;

function pbEncodeFieldSInt64(var Buf; const BufSize: Integer;
         const FieldNumber: Int32; const Value: Int64): Integer;
var
  A : TpbVarInt;
begin
  pbVarIntInitSInt64(A, Value);
  Result := pbEncodeFieldVarInt(Buf, BufSize, FieldNumber, A);
end;

function pbEncodeFieldBool(var Buf; const BufSize: Integer;
         const FieldNumber: Int32; const Value: Boolean): Integer;
var
  A : TpbVarInt;
begin
  pbVarIntInitUInt32(A, Ord(Value));
  Result := pbEncodeFieldVarInt(Buf, BufSize, FieldNumber, A);
end;

function pbEncodeFieldDouble(var Buf; const BufSize: Integer;
         const FieldNumber: Int32; const Value: Double): Integer;
var
  P : PByte;
  L : Integer;
  I : Integer;
begin
  P := @Buf;
  L := BufSize;
  I := pbEncodeFieldKey(P^, L, FieldNumber, pwt64Bit);
  Inc(P, I);
  Dec(L, I);
  I := pbEncodeValueDouble(P^, L, Value);
  Dec(L, I);
  Result := BufSize - L;
end;

function pbEncodeFieldFloat(var Buf; const BufSize: Integer;
         const FieldNumber: Int32; const Value: Single): Integer;
var
  P : PByte;
  L : Integer;
  I : Integer;
begin
  P := @Buf;
  L := BufSize;
  I := pbEncodeFieldKey(P^, L, FieldNumber, pwt32Bit);
  Inc(P, I);
  Dec(L, I);
  I := pbEncodeValueFloat(P^, L, Value);
  Dec(L, I);
  Result := BufSize - L;
end;

function pbEncodeFieldFixed32(var Buf; const BufSize: Integer;
         const FieldNumber: Int32; const Value: Word32): Integer;
var
  P : PByte;
  L : Integer;
  I : Integer;
begin
  P := @Buf;
  L := BufSize;
  I := pbEncodeFieldKey(P^, L, FieldNumber, pwt32Bit);
  Inc(P, I);
  Dec(L, I);
  I := pbEncodeValueFixed32(P^, L, Value);
  Dec(L, I);
  Result := BufSize - L;
end;

function pbEncodeFieldFixed64(var Buf; const BufSize: Integer;
         const FieldNumber: Int32; const Value: UInt64): Integer;
var
  P : PByte;
  L : Integer;
  I : Integer;
begin
  P := @Buf;
  L := BufSize;
  I := pbEncodeFieldKey(P^, L, FieldNumber, pwt64Bit);
  Inc(P, I);
  Dec(L, I);
  I := pbEncodeValueFixed64(P^, L, Value);
  Dec(L, I);
  Result := BufSize - L;
end;

function pbEncodeFieldSFixed32(var Buf; const BufSize: Integer;
         const FieldNumber: Int32; const Value: Int32): Integer;
var
  P : PByte;
  L : Integer;
  I : Integer;
begin
  P := @Buf;
  L := BufSize;
  I := pbEncodeFieldKey(P^, L, FieldNumber, pwt32Bit);
  Inc(P, I);
  Dec(L, I);
  I := pbEncodeValueSFixed32(P^, L, Value);
  Dec(L, I);
  Result := BufSize - L;
end;

function pbEncodeFieldSFixed64(var Buf; const BufSize: Integer;
         const FieldNumber: Int32; const Value: Int64): Integer;
var
  P : PByte;
  L : Integer;
  I : Integer;
begin
  P := @Buf;
  L := BufSize;
  I := pbEncodeFieldKey(P^, L, FieldNumber, pwt64Bit);
  Inc(P, I);
  Dec(L, I);
  I := pbEncodeValueSFixed64(P^, L, Value);
  Dec(L, I);
  Result := BufSize - L;
end;

function pbEncodeFieldString(var Buf; const BufSize: Integer;
         const FieldNumber: Int32; const Value: RawByteString): Integer;
var
  P : PByte;
  L : Integer;
  I : Integer;
begin
  P := @Buf;
  L := BufSize;
  I := pbEncodeFieldKey(P^, L, FieldNumber, pwtVarBytes);
  Inc(P, I);
  Dec(L, I);
  I := pbEncodeValueString(P^, L, Value);
  Dec(L, I);
  Result := BufSize - L;
end;

function pbEncodeFieldBytes(var Buf; const BufSize: Integer;
         const FieldNumber: Int32; const ValueBuf; const ValueBufSize: Integer): Integer;
var
  P : PByte;
  L : Integer;
  I : Integer;
begin
  if ValueBufSize < 0 then
    raise EpbEncodeError.Create(SErr_InvalidBuffer);
  P := @Buf;
  L := BufSize;
  I := pbEncodeFieldKey(P^, L, FieldNumber, pwtVarBytes);
  Inc(P, I);
  Dec(L, I);
  I := pbEncodeValueBytes(P^, L, ValueBuf, ValueBufSize);
  Dec(L, I);
  Result := BufSize - L;
end;



{ Decode }

function pbDecodeValueInt32(const Buf; const BufSize: Integer; var Value: Int32): Integer;
var
  A : TpbVarInt;
begin
  Result := pbVarIntInitEncBuf(A, Buf, BufSize);
  pbVarIntToBinBuf(A, Value, SizeOf(Value));
end;

function pbDecodeValueInt64(const Buf; const BufSize: Integer; var Value: Int64): Integer;
var
  A : TpbVarInt;
begin
  Result := pbVarIntInitEncBuf(A, Buf, BufSize);
  pbVarIntToBinBuf(A, Value, SizeOf(Value));
end;

function pbDecodeValueUInt32(const Buf; const BufSize: Integer; var Value: Word32): Integer;
var
  A : TpbVarInt;
begin
  Result := pbVarIntInitEncBuf(A, Buf, BufSize);
  pbVarIntToBinBuf(A, Value, SizeOf(Value));
end;

function pbDecodeValueUInt64(const Buf; const BufSize: Integer; var Value: UInt64): Integer;
var
  A : TpbVarInt;
begin
  Result := pbVarIntInitEncBuf(A, Buf, BufSize);
  pbVarIntToBinBuf(A, Value, SizeOf(Value));
end;

function pbDecodeValueSInt32(const Buf; const BufSize: Integer; var Value: Int32): Integer;
var
  A : TpbVarInt;
begin
  Result := pbVarIntInitEncBuf(A, Buf, BufSize);
  Value := pbVarIntToSInt32(A);
end;

function pbDecodeValueSInt64(const Buf; const BufSize: Integer; var Value: Int64): Integer;
var
  A : TpbVarInt;
begin
  Result := pbVarIntInitEncBuf(A, Buf, BufSize);
  Value := pbVarIntToSInt64(A);
end;

function pbDecodeValueDouble(const Buf; const BufSize: Integer; var Value: Double): Integer;
begin
  if BufSize < SizeOf(Value) then
    raise EpbDecodeError.Create(SErr_InvalidBuffer);
  Value := PDouble(@Buf)^;
  Result := SizeOf(Value);
end;

function pbDecodeValueFloat(const Buf; const BufSize: Integer; var Value: Single): Integer;
begin
  if BufSize < SizeOf(Value) then
    raise EpbDecodeError.Create(SErr_InvalidBuffer);
  Value := PSingle(@Buf)^;
  Result := SizeOf(Value);
end;

function pbDecodeValueFixed32(const Buf; const BufSize: Integer; var Value: Word32): Integer;
begin
  if BufSize < SizeOf(Value) then
    raise EpbDecodeError.Create(SErr_InvalidBuffer);
  Value := PWord32(@Buf)^;
  Result := SizeOf(Value);
end;

function pbDecodeValueFixed64(const Buf; const BufSize: Integer; var Value: UInt64): Integer;
begin
  if BufSize < SizeOf(Value) then
    raise EpbDecodeError.Create(SErr_InvalidBuffer);
  Value := PUInt64(@Buf)^;
  Result := SizeOf(Value);
end;

function pbDecodeValueSFixed32(const Buf; const BufSize: Integer; var Value: Int32): Integer;
begin
  if BufSize < SizeOf(Value) then
    raise EpbDecodeError.Create(SErr_InvalidBuffer);
  Value := PInt32(@Buf)^;
  Result := SizeOf(Value);
end;

function pbDecodeValueSFixed64(const Buf; const BufSize: Integer; var Value: Int64): Integer;
begin
  if BufSize < SizeOf(Value) then
    raise EpbDecodeError.Create(SErr_InvalidBuffer);
  Value := PInt64(@Buf)^;
  Result := SizeOf(Value);
end;

function pbDecodeValueBool(const Buf; const BufSize: Integer; var Value: Boolean): Integer;
var
  A : TpbVarInt;
begin
  Result := pbVarIntInitEncBuf(A, Buf, BufSize);
  Value := not pbVarIntIsZero(A);
end;

function pbDecodeValueString(const Buf; const BufSize: Integer; var Value: RawByteString): Integer;
var
  P : PByte;
  L, I, N : Integer;
  S : RawByteString;
begin
  P := @Buf;
  L := BufSize;
  I := pbDecodeValueInt32(P^, L, N);
  Dec(L, I);
  Inc(P, I);
  if L < N then
    raise EpbDecodeError.Create(SErr_InvalidBuffer);
  SetLength(S, N);
  if N > 0 then
    begin
      Move(P^, Pointer(S)^, N);
      Dec(L, N);
    end;
  Value := S;
  Result:= BufSize - L;
end;

function pbDecodeValueBytes(const Buf; const BufSize: Integer; var ValueBuf; const ValueBufSize: Integer): Integer;
var
  P : PByte;
  L, I, N : Integer;
begin
  P := @Buf;
  L := BufSize;
  I := pbDecodeValueInt32(P^, L, N);
  Dec(L, I);
  Inc(P, I);
  if L < N then
    raise EpbDecodeError.Create(SErr_InvalidBuffer);
  if N > 0 then
    begin
      if ValueBufSize < N then
        raise EpbDecodeError.Create(SErr_InvalidBuffer);
      Move(P^, ValueBuf, N);
      Dec(L, N);
    end;
  Result:= BufSize - L;
end;

procedure pbDecodeProtoBuf(
          const Buf; const BufSize: Integer;
          const CallbackProc: TpbProtoBufDecodeFieldCallbackProc;
          const CallbackData: Pointer);
var
  P : PByte;
  L : Integer;
  I : Integer;
  A : TpbVarInt;
  Q : PByte;
  WireSize : Integer;
  Key32 : Word32;
  Field : TpbProtoBufDecodeField;
begin
  P := @Buf;
  L := BufSize;
  while L > 0 do
    begin
      // decode field key
      I := pbVarIntInitEncBuf(A, P^, L);
      Assert(I > 0);
      Inc(P, I);
      Dec(L, I);
      Key32 := pbVarIntToUInt32(A);
      pbUInt32ToFieldKey(Key32, Field.FieldNum, Field.WireType);
      // decode wire data
      Field.WireDataPtr := P;
      case Field.WireType of
        pwt32Bit    : WireSize := 4;
        pwt64Bit    : WireSize := 8;
        pwtVarInt   : WireSize := pbVarIntInitEncBuf(Field.ValueVarInt, P^, L);
        pwtVarBytes :
          begin
            // VarBytes size
            I := pbVarIntInitEncBuf(A, P^, L);
            Field.ValueVarBytesLen := pbVarIntToUInt32(A);
            // VarBytes data
            Q := P;
            Inc(Q, I);
            Field.ValueVarBytesPtr := Q;
            // wire size
            WireSize := I + Field.ValueVarBytesLen;
          end;
      else
        raise EpbDecodeError.Create(SErr_InvalidBuffer); // unrecognised wire type
      end;
      Field.WireDataSize := WireSize;
      if WireSize > L then
        raise EpbDecodeError.Create(SErr_InvalidBuffer); // incomplete/bad buffer
      Inc(P, WireSize);
      Dec(L, WireSize);
      // field callback
      if Assigned(CallbackProc) then
        CallbackProc(Field, CallbackData);
    end;
end;

procedure pbDecodeFieldInt32(const Field: TpbProtoBufDecodeField; var Value: Int32);
begin
  case Field.WireType of
    pwt32Bit  : Value := PInt32(Field.WireDataPtr)^;
    pwtVarInt : pbVarIntToBinBuf(Field.ValueVarInt, Value, SizeOf(Value));
  else
    raise EpbDecodeError.Create(SErr_InvalidWireType);
  end;
end;

procedure pbDecodeFieldInt64(const Field: TpbProtoBufDecodeField; var Value: Int64);
begin
  case Field.WireType of
    pwt32Bit  : Value := PInt32(Field.WireDataPtr)^;
    pwt64Bit  : Value := PInt64(Field.WireDataPtr)^;
    pwtVarInt : pbVarIntToBinBuf(Field.ValueVarInt, Value, SizeOf(Value));
  else
    raise EpbDecodeError.Create(SErr_InvalidWireType);
  end;
end;

procedure pbDecodeFieldUInt32(const Field: TpbProtoBufDecodeField; var Value: Word32);
begin
  case Field.WireType of
    pwt32Bit  : Value := PWord32(Field.WireDataPtr)^;
    pwtVarInt : Value := pbVarIntToUInt32(Field.ValueVarInt);
  else
    raise EpbDecodeError.Create(SErr_InvalidWireType);
  end;
end;

procedure pbDecodeFieldUInt64(const Field: TpbProtoBufDecodeField; var Value: UInt64);
begin
  case Field.WireType of
    pwt32Bit  : Value := PWord32(Field.WireDataPtr)^;
    pwt64Bit  : Value := PUInt64(Field.WireDataPtr)^;
    pwtVarInt : pbVarIntToBinBuf(Field.ValueVarInt, Value, SizeOf(Value));
  else
    raise EpbDecodeError.Create(SErr_InvalidWireType);
  end;
end;

procedure pbDecodeFieldSInt32(const Field: TpbProtoBufDecodeField; var Value: Int32);
begin
  case Field.WireType of
    pwt32Bit  : Value := PInt32(Field.WireDataPtr)^;
    pwtVarInt : Value := pbVarIntToSInt32(Field.ValueVarInt);
  else
    raise EpbDecodeError.Create(SErr_InvalidWireType);
  end;
end;

procedure pbDecodeFieldSInt64(const Field: TpbProtoBufDecodeField; var Value: Int64);
begin
  case Field.WireType of
    pwt32Bit  : Value := PInt32(Field.WireDataPtr)^;
    pwt64Bit  : Value := PInt64(Field.WireDataPtr)^;
    pwtVarInt : Value := pbVarIntToSInt64(Field.ValueVarInt);
  else
    raise EpbDecodeError.Create(SErr_InvalidWireType);
  end;
end;

procedure pbDecodeFieldDouble(const Field: TpbProtoBufDecodeField; var Value: Double);
begin
  case Field.WireType of
    pwt32Bit : Value := PSingle(Field.WireDataPtr)^;
    pwt64Bit : Value := PDouble(Field.WireDataPtr)^;
  else
    raise EpbDecodeError.Create(SErr_InvalidWireType);
  end;
end;

procedure pbDecodeFieldFloat(const Field: TpbProtoBufDecodeField; var Value: Single);
begin
  case Field.WireType of
    pwt32Bit : Value := PSingle(Field.WireDataPtr)^;
  else
    raise EpbDecodeError.Create(SErr_InvalidWireType);
  end;
end;

procedure pbDecodeFieldFixed32(const Field: TpbProtoBufDecodeField; var Value: Word32);
begin
  case Field.WireType of
    pwt32Bit  : Value := PWord32(Field.WireDataPtr)^;
    pwtVarInt : Value := pbVarIntToUInt32(Field.ValueVarInt);
  else
    raise EpbDecodeError.Create(SErr_InvalidWireType);
  end;
end;

procedure pbDecodeFieldFixed64(const Field: TpbProtoBufDecodeField; var Value: UInt64);
begin
  case Field.WireType of
    pwt32Bit  : Value := PWord32(Field.WireDataPtr)^;
    pwt64Bit  : Value := PUInt64(Field.WireDataPtr)^;
  else
    raise EpbDecodeError.Create(SErr_InvalidWireType);
  end;
end;

procedure pbDecodeFieldSFixed32(const Field: TpbProtoBufDecodeField; var Value: Int32);
begin
  case Field.WireType of
    pwt32Bit  : Value := PInt32(Field.WireDataPtr)^;
  else
    raise EpbDecodeError.Create(SErr_InvalidWireType);
  end;
end;

procedure pbDecodeFieldSFixed64(const Field: TpbProtoBufDecodeField; var Value: Int64);
begin
  case Field.WireType of
    pwt32Bit  : Value := PInt32(Field.WireDataPtr)^;
    pwt64Bit  : Value := PInt64(Field.WireDataPtr)^;
  else
    raise EpbDecodeError.Create(SErr_InvalidWireType);
  end;
end;

procedure pbDecodeFieldBool(const Field: TpbProtoBufDecodeField; var Value: Boolean);
begin
  case Field.WireType of
    pwt32Bit  : Value := PInt32(Field.WireDataPtr)^ <> 0;
    pwt64Bit  : Value := PInt64(Field.WireDataPtr)^ <> 0;
    pwtVarInt : Value := pbVarIntToUInt32(Field.ValueVarInt) <> 0;
  else
    raise EpbDecodeError.Create(SErr_InvalidWireType);
  end;
end;

function pbDecodeFieldString(const Field: TpbProtoBufDecodeField; var Value: RawByteString): Integer;
var
  S : RawByteString;
  L : Integer;
begin
  case Field.WireType of
    pwtVarBytes :
      begin
        L := Field.ValueVarBytesLen;
        SetLength(S, L);
        if L > 0 then
          Move(Field.ValueVarBytesPtr^, Pointer(S)^, L);
        Value := S;
        Result := L;
      end;
  else
    raise EpbDecodeError.Create(SErr_InvalidWireType);
  end;
end;

function pbDecodeFieldBytes(const Field: TpbProtoBufDecodeField; var ValueBuf; const ValueBufSize: Integer): Integer;
var
  L : Integer;
begin
  case Field.WireType of
    pwtVarBytes :
      begin
        L := Field.ValueVarBytesLen;
        if ValueBufSize < L then
          raise EpbDecodeError.Create(SErr_BufferTooSmall);
        if L > 0 then
          Move(Field.ValueVarBytesPtr^, ValueBuf, L);
        Result := L;
      end;
  else
    raise EpbDecodeError.Create(SErr_InvalidWireType);
  end;
end;



{                                                                              }
{ Test cases                                                                   }
{                                                                              }
{$IFDEF PROTOBUF_TEST}
{$ASSERTIONS ON}
{$IFNDEF SupportsUInt64}{$WARNINGS OFF}{$ENDIF}
procedure Test_VarInt;
var I, J : TpbVarInt;
    A : Word32;
begin
  // InitZero
  pbVarIntInitZero(I);
  Assert(I.EncSize = 1);
  Assert(I.EncData[0] = 0);
  Assert(pbVarIntToUInt32(I) = 0);

  // pbVarIntEquals
  pbVarIntInitUInt32(I, 0);
  pbVarIntInitZero(J);
  Assert(pbVarIntEquals(I, J));

  pbVarIntInitUInt32(I, 0);
  pbVarIntInitUInt32(J, 1);
  Assert(not pbVarIntEquals(I, J));

  // pbVarIntInitWord32
  pbVarIntInitUInt32(I, 0);
  Assert(I.EncSize = 1);
  Assert(I.EncData[0] = 0);
  Assert(pbVarIntToUInt32(I) = 0);

  pbVarIntInitUInt32(I, 1);
  Assert(I.EncSize = 1);
  Assert(I.EncData[0] = 1);
  Assert(pbVarIntToUInt32(I) = 1);

  pbVarIntInitUInt32(I, $7F);
  Assert(I.EncSize = 1);
  Assert(I.EncData[0] = $7F);
  Assert(pbVarIntToUInt32(I) = $7F);

  pbVarIntInitUInt32(I, $80);
  Assert(I.EncSize = 2);
  Assert(I.EncData[0] = $80);
  Assert(I.EncData[1] = $01);
  Assert(pbVarIntToUInt32(I) = $80);

  pbVarIntInitUInt32(I, $81);
  Assert(I.EncSize = 2);
  Assert(I.EncData[0] = $81);
  Assert(I.EncData[1] = $01);
  Assert(pbVarIntToUInt32(I) = $81);

  pbVarIntInitUInt32(I, $100);
  Assert(I.EncSize = 2);
  Assert(I.EncData[0] = $80);
  Assert(I.EncData[1] = $02);
  Assert(pbVarIntToUInt32(I) = $100);

  pbVarIntInitUInt32(I, $3FFF);
  Assert(I.EncSize = 2);
  Assert(I.EncData[0] = $FF);
  Assert(I.EncData[1] = $7F);
  Assert(pbVarIntToUInt32(I) = $3FFF);

  pbVarIntInitUInt32(I, $4000);
  Assert(I.EncSize = 3);
  Assert(I.EncData[0] = $80);
  Assert(I.EncData[1] = $80);
  Assert(I.EncData[2] = $01);
  Assert(pbVarIntToUInt32(I) = $4000);

  pbVarIntInitUInt32(I, $4001);
  Assert(I.EncSize = 3);
  Assert(I.EncData[0] = $81);
  Assert(I.EncData[1] = $80);
  Assert(I.EncData[2] = $01);
  Assert(pbVarIntToUInt32(I) = $4001);

  pbVarIntInitUInt32(I, $1FFFFF);
  Assert(I.EncSize = 3);
  Assert(I.EncData[0] = $FF);
  Assert(I.EncData[1] = $FF);
  Assert(I.EncData[2] = $7F);
  Assert(pbVarIntToUInt32(I) = $1FFFFF);

  pbVarIntInitUInt32(I, $200000);
  Assert(I.EncSize = 4);
  Assert(I.EncData[0] = $80);
  Assert(I.EncData[1] = $80);
  Assert(I.EncData[2] = $80);
  Assert(I.EncData[3] = $01);
  Assert(pbVarIntToUInt32(I) = $200000);

  pbVarIntInitUInt32(I, $FFFFFFF);
  Assert(I.EncSize = 4);
  Assert(I.EncData[0] = $FF);
  Assert(I.EncData[1] = $FF);
  Assert(I.EncData[2] = $FF);
  Assert(I.EncData[3] = $7F);
  Assert(pbVarIntToUInt32(I) = $FFFFFFF);

  pbVarIntInitUInt32(I, $10000000);
  Assert(I.EncSize = 5);
  Assert(I.EncData[0] = $80);
  Assert(I.EncData[1] = $80);
  Assert(I.EncData[2] = $80);
  Assert(I.EncData[3] = $80);
  Assert(I.EncData[4] = $01);
  Assert(pbVarIntToUInt32(I) = $10000000);

  pbVarIntInitUInt32(I, $12345678);
  Assert(I.EncSize = 5);
  Assert(I.EncData[0] = $F8);
  Assert(I.EncData[1] = $AC);
  Assert(I.EncData[2] = $D1);
  Assert(I.EncData[3] = $91);
  Assert(I.EncData[4] = $01);
  Assert(pbVarIntToUInt32(I) = $12345678);

  pbVarIntInitUInt32(I, $FFFFFFFF);
  Assert(I.EncSize = 5);
  Assert(I.EncData[0] = $FF);
  Assert(I.EncData[1] = $FF);
  Assert(I.EncData[2] = $FF);
  Assert(I.EncData[3] = $FF);
  Assert(I.EncData[4] = $0F);
  Assert(pbVarIntToUInt32(I) = $FFFFFFFF);

  // pbVarIntInitBinBuf
  A := 0;
  pbVarIntInitBinBuf(I, A, SizeOf(A));
  Assert(I.EncSize = 1);
  Assert(I.EncData[0] = 0);
  Assert(pbVarIntToUInt32(I) = 0);

  A := 1;
  pbVarIntInitBinBuf(I, A, SizeOf(A));
  Assert(I.EncSize = 1);
  Assert(I.EncData[0] = 1);
  Assert(pbVarIntToUInt32(I) = 1);

  A := $7F;
  pbVarIntInitBinBuf(I, A, SizeOf(A));
  Assert(I.EncSize = 1);
  Assert(I.EncData[0] = $7F);
  Assert(pbVarIntToUInt32(I) = $7F);

  A := $80;
  pbVarIntInitBinBuf(I, A, SizeOf(A));
  Assert(I.EncSize = 2);
  Assert(I.EncData[0] = $80);
  Assert(I.EncData[1] = $01);
  Assert(pbVarIntToUInt32(I) = $80);

  A := $81;
  pbVarIntInitBinBuf(I, A, SizeOf(A));
  Assert(I.EncSize = 2);
  Assert(I.EncData[0] = $81);
  Assert(I.EncData[1] = $01);
  Assert(pbVarIntToUInt32(I) = $81);

  A := $100;
  pbVarIntInitBinBuf(I, A, SizeOf(A));
  Assert(I.EncSize = 2);
  Assert(I.EncData[0] = $80);
  Assert(I.EncData[1] = $02);
  Assert(pbVarIntToUInt32(I) = $100);

  A := $3FFF;
  pbVarIntInitBinBuf(I, A, SizeOf(A));
  Assert(I.EncSize = 2);
  Assert(I.EncData[0] = $FF);
  Assert(I.EncData[1] = $7F);
  Assert(pbVarIntToUInt32(I) = $3FFF);

  A := $4000;
  pbVarIntInitBinBuf(I, A, SizeOf(A));
  Assert(I.EncSize = 3);
  Assert(I.EncData[0] = $80);
  Assert(I.EncData[1] = $80);
  Assert(I.EncData[2] = $01);
  Assert(pbVarIntToUInt32(I) = $4000);

  A := $4001;
  pbVarIntInitBinBuf(I, A, SizeOf(A));
  Assert(I.EncSize = 3);
  Assert(I.EncData[0] = $81);
  Assert(I.EncData[1] = $80);
  Assert(I.EncData[2] = $01);
  Assert(pbVarIntToUInt32(I) = $4001);

  A := $200000;
  pbVarIntInitBinBuf(I, A, SizeOf(A));
  Assert(I.EncSize = 4);
  Assert(I.EncData[0] = $80);
  Assert(I.EncData[1] = $80);
  Assert(I.EncData[2] = $80);
  Assert(I.EncData[3] = $01);
  Assert(pbVarIntToUInt32(I) = $200000);

  A := $10000000;
  pbVarIntInitBinBuf(I, A, SizeOf(A));
  Assert(I.EncSize = 5);
  Assert(I.EncData[0] = $80);
  Assert(I.EncData[1] = $80);
  Assert(I.EncData[2] = $80);
  Assert(I.EncData[3] = $80);
  Assert(I.EncData[4] = $01);
  Assert(pbVarIntToUInt32(I) = $10000000);

  A := $12345678;
  pbVarIntInitBinBuf(I, A, SizeOf(A));
  Assert(I.EncSize = 5);
  Assert(I.EncData[0] = $F8);
  Assert(I.EncData[1] = $AC);
  Assert(I.EncData[2] = $D1);
  Assert(I.EncData[3] = $91);
  Assert(I.EncData[4] = $01);
  Assert(pbVarIntToUInt32(I) = $12345678);

  A := $FFFFFFFF;
  pbVarIntInitBinBuf(I, A, SizeOf(A));
  Assert(I.EncSize = 5);
  Assert(I.EncData[0] = $FF);
  Assert(I.EncData[1] = $FF);
  Assert(I.EncData[2] = $FF);
  Assert(I.EncData[3] = $FF);
  Assert(I.EncData[4] = $0F);
  Assert(pbVarIntToUInt32(I) = $FFFFFFFF);

  // pbVarIntInitEncBuf
  pbVarIntInitUInt32(I, 0);
  Assert(pbVarIntInitEncBuf(J, I.EncData[0], SizeOf(I.EncData)) = I.EncSize);
  Assert(pbVarIntToUInt32(J) = 0);

  pbVarIntInitUInt32(I, 1);
  Assert(pbVarIntInitEncBuf(J, I.EncData[0], SizeOf(I.EncData)) = I.EncSize);
  Assert(pbVarIntToUInt32(J) = 1);

  pbVarIntInitUInt32(I, $80);
  Assert(pbVarIntInitEncBuf(J, I.EncData[0], SizeOf(I.EncData)) = I.EncSize);
  Assert(pbVarIntToUInt32(J) = $80);

  pbVarIntInitUInt32(I, $3FFF);
  Assert(pbVarIntInitEncBuf(J, I.EncData[0], SizeOf(I.EncData)) = I.EncSize);
  Assert(pbVarIntToUInt32(J) = $3FFF);

  pbVarIntInitUInt32(I, $4000);
  Assert(pbVarIntInitEncBuf(J, I.EncData[0], SizeOf(I.EncData)) = I.EncSize);
  Assert(pbVarIntToUInt32(J) = $4000);

  pbVarIntInitUInt32(I, $4001);
  Assert(pbVarIntInitEncBuf(J, I.EncData[0], SizeOf(I.EncData)) = I.EncSize);
  Assert(pbVarIntToUInt32(J) = $4001);

  pbVarIntInitUInt32(I, $12345678);
  Assert(pbVarIntInitEncBuf(J, I.EncData[0], SizeOf(I.EncData)) = I.EncSize);
  Assert(pbVarIntToUInt32(J) = $12345678);

  pbVarIntInitUInt32(I, $FFFFFFFF);
  Assert(pbVarIntInitEncBuf(J, I.EncData[0], SizeOf(I.EncData)) = I.EncSize);
  Assert(pbVarIntToUInt32(J) = $FFFFFFFF);

  // pbVarIntToBinBuf
  pbVarIntInitUInt32(I, 0);
  pbVarIntToBinBuf(I, A, SizeOf(A));
  Assert(A = 0);

  pbVarIntInitUInt32(I, 1);
  pbVarIntToBinBuf(I, A, SizeOf(A));
  Assert(A = 1);

  pbVarIntInitUInt32(I, $7F);
  pbVarIntToBinBuf(I, A, SizeOf(A));
  Assert(A = $7F);

  pbVarIntInitUInt32(I, $80);
  pbVarIntToBinBuf(I, A, SizeOf(A));
  Assert(A = $80);

  pbVarIntInitUInt32(I, $81);
  pbVarIntToBinBuf(I, A, SizeOf(A));
  Assert(A = $81);

  pbVarIntInitUInt32(I, $3FFF);
  pbVarIntToBinBuf(I, A, SizeOf(A));
  Assert(A = $3FFF);

  pbVarIntInitUInt32(I, $4000);
  pbVarIntToBinBuf(I, A, SizeOf(A));
  Assert(A = $4000);

  pbVarIntInitUInt32(I, $4001);
  pbVarIntToBinBuf(I, A, SizeOf(A));
  Assert(A = $4001);

  pbVarIntInitUInt32(I, $200000);
  pbVarIntToBinBuf(I, A, SizeOf(A));
  Assert(A = $200000);

  pbVarIntInitUInt32(I, $10000000);
  pbVarIntToBinBuf(I, A, SizeOf(A));
  Assert(A = $10000000);

  pbVarIntInitUInt32(I, $12345678);
  pbVarIntToBinBuf(I, A, SizeOf(A));
  Assert(A = $12345678);

  pbVarIntInitUInt32(I, $FFFFFFFF);
  pbVarIntToBinBuf(I, A, SizeOf(A));
  Assert(A = $FFFFFFFF);

  // pbSInt32ToUInt32
  Assert(pbSInt32ToUInt32(0) = 0);
  Assert(pbSInt32ToUInt32(-1) = 1);
  Assert(pbSInt32ToUInt32(1) = 2);
  Assert(pbSInt32ToUInt32(-2) = 3);
  Assert(pbSInt32ToUInt32(2) = 4);
  Assert(pbSInt32ToUInt32(-3) = 5);
  Assert(pbSInt32ToUInt32(2147483647) = $FFFFFFFE);
  Assert(pbSInt32ToUInt32(-2147483648) = $FFFFFFFF);

  // pbUInt32ToSInt32
  Assert(pbUInt32ToSInt32(0) = 0);
  Assert(pbUInt32ToSInt32(1) = -1);
  Assert(pbUInt32ToSInt32(2) = 1);
  Assert(pbUInt32ToSInt32(3) = -2);
  Assert(pbUInt32ToSInt32(4) = 2);
  Assert(pbUInt32ToSInt32(5) = -3);
  Assert(pbUInt32ToSInt32($FFFFFFFE) = 2147483647);
  Assert(pbUInt32ToSInt32($FFFFFFFF) = -2147483648);

  // pbVarIntInitSInt32
  pbVarIntInitSInt32(I, 0);
  Assert(pbVarIntToSInt32(I) = 0);

  pbVarIntInitSInt32(I, -1);
  Assert(pbVarIntToSInt32(I) = -1);

  pbVarIntInitSInt32(I, 1);
  Assert(pbVarIntToSInt32(I) = 1);

  pbVarIntInitSInt32(I, 2147483647);
  Assert(pbVarIntToSInt32(I) = 2147483647);

  pbVarIntInitSInt32(I, -2147483648);
  Assert(pbVarIntToSInt32(I) = -2147483648);

  // pbSInt64ToUInt64
  Assert(pbSInt64ToUInt64(0) = 0);
  Assert(pbSInt64ToUInt64(-1) = 1);
  Assert(pbSInt64ToUInt64(1) = 2);
  Assert(pbSInt64ToUInt64(-2) = 3);
  Assert(pbSInt64ToUInt64(2) = 4);
  Assert(pbSInt64ToUInt64(-3) = 5);
  Assert(pbSInt64ToUInt64(2147483647)  = $FFFFFFFE);
  Assert(pbSInt64ToUInt64(-2147483648) = $FFFFFFFF);
  Assert(pbSInt64ToUInt64(2147483648)  = $100000000);
  Assert(pbSInt64ToUInt64(-2147483649) = $100000001);
  Assert(pbSInt64ToUInt64(4611686018427387903)  = $7FFFFFFFFFFFFFFE);
  Assert(pbSInt64ToUInt64(-4611686018427387904) = $7FFFFFFFFFFFFFFF);
  Assert(pbSInt64ToUInt64(4611686018427387904)  = $8000000000000000);
  Assert(pbSInt64ToUInt64(-4611686018427387905) = $8000000000000001);
  Assert(pbSInt64ToUInt64(9223372036854775807)  = $FFFFFFFFFFFFFFFE);
  Assert(pbSInt64ToUInt64(-9223372036854775808) = $FFFFFFFFFFFFFFFF);

  // pbUInt64ToSInt64
  Assert(pbUInt64ToSInt64(0) = 0);
  Assert(pbUInt64ToSInt64(1) = -1);
  Assert(pbUInt64ToSInt64(2) = 1);
  Assert(pbUInt64ToSInt64(3) = -2);
  Assert(pbUInt64ToSInt64(4) = 2);
  Assert(pbUInt64ToSInt64(5) = -3);
  Assert(pbUInt64ToSInt64($FFFFFFFE) = 2147483647);
  Assert(pbUInt64ToSInt64($FFFFFFFF) = -2147483648);
  Assert(pbUInt64ToSInt64($100000000) = 2147483648);
  Assert(pbUInt64ToSInt64($100000001) = -2147483649);
  Assert(pbUInt64ToSInt64($7FFFFFFFFFFFFFFE) = 4611686018427387903);
  Assert(pbUInt64ToSInt64($7FFFFFFFFFFFFFFF) = -4611686018427387904);
  Assert(pbUInt64ToSInt64($8000000000000000) = 4611686018427387904);
  Assert(pbUInt64ToSInt64($8000000000000001) = -4611686018427387905);
  Assert(pbUInt64ToSInt64($FFFFFFFFFFFFFFFE) = 9223372036854775807);
  Assert(pbUInt64ToSInt64($FFFFFFFFFFFFFFFF) = -9223372036854775808);

  // pbVarIntInitSInt64
  pbVarIntInitSInt64(I, 0);
  Assert(pbVarIntToSInt64(I) = 0);

  pbVarIntInitSInt64(I, -1);
  Assert(pbVarIntToSInt64(I) = -1);

  pbVarIntInitSInt64(I, 1);
  Assert(pbVarIntToSInt64(I) = 1);

  pbVarIntInitSInt64(I, 2147483647);
  Assert(pbVarIntToSInt64(I) = 2147483647);

  pbVarIntInitSInt64(I, -2147483648);
  Assert(pbVarIntToSInt64(I) = -2147483648);

  pbVarIntInitSInt64(I, 2147483648);
  Assert(pbVarIntToSInt64(I) = 2147483648);

  pbVarIntInitSInt64(I, -2147483649);
  Assert(pbVarIntToSInt64(I) = -2147483649);

  pbVarIntInitSInt64(I, 9223372036854775807);
  Assert(pbVarIntToSInt64(I) = 9223372036854775807);

  pbVarIntInitSInt64(I, -9223372036854775808);
  Assert(pbVarIntToSInt64(I) = -9223372036854775808);
end;

procedure Test_Encode;
var
  B : array[0..1023] of Byte;
begin
  FillChar(B, SizeOf(B), 0);

  Assert(pbEncodeFieldKey(B[0], SizeOf(B), 3, pwtVarBytes) = 1);
  Assert(B[0] = $1A);

  Assert(pbEncodeFieldString(B[0], SizeOf(B), 2, 'testing') = 9);
  Assert(B[0] = $12);
  Assert(B[1] = $07);
  Assert(B[2] = $74);
  Assert(B[8] = $67);

  Assert(pbEncodeFieldUInt32(B[0], SizeOf(B), 2, 12) = 2);
  Assert(B[0] = $10);
  Assert(B[1] = $0C);

  FillChar(B, SizeOf(B), 0);

  Assert(pbEncodeFieldString(B[0], 0, 2, 'testing') = 9);
  Assert(B[0] = 0);
  Assert(B[1] = 0);
  Assert(B[2] = 0);
  Assert(B[8] = 0);

  Assert(pbEncodeFieldUInt32(B[0], 0, 2, 12) = 2);
  Assert(B[0] = 0);
  Assert(B[1] = 0);
end;

procedure Test_Decode_CallbackProc(const Field: TpbProtoBufDecodeField; const Data: Pointer);
var
  A : Word32;
  B : Int64;
  C : RawByteString;
begin
  case Integer(Data) of
    1 : case Field.FieldNum of
          4 : begin
                Assert(Field.WireType = pwtVarInt);
                Assert(pbVarIntToUInt32(Field.ValueVarInt) = 13);
                A := 0;
                pbDecodeFieldUInt32(Field, A);
                Assert(A = 13);
              end;
          5 : begin
                Assert(Field.WireType = pwtVarInt);
                Assert(pbVarIntToSInt64(Field.ValueVarInt) = -1);
                B := 0;
                pbDecodeFieldSInt64(Field, B);
                Assert(B = -1);
              end;
          6 : begin
                Assert(Field.WireType = pwtVarBytes);
                pbDecodeFieldString(Field, C);
                Assert(C = 'ABC');
              end;
        else
          Assert(False);
        end;
  else
    Assert(False);
  end;
end;

procedure Test_Decode;
var
  B : array[0..1023] of Byte;
  P : PByte;
  L, N : Integer;
begin
  P := @B[0];
  L := SizeOf(B);
  N := pbEncodeFieldInt32(P^, L, 4, 13);
  Dec(L, N);
  Inc(P, N);
  N := pbEncodeFieldSInt64(P^, L, 5, -1);
  Dec(L, N);
  Inc(P, N);
  N := pbEncodeFieldString(P^, L, 6, 'ABC');
  Dec(L, N);

  pbDecodeProtoBuf(B[0], SizeOf(B) - L, Test_Decode_CallbackProc, Ptr(1));
end;

procedure Test;
begin
  Test_VarInt;
  Test_Encode;
  Test_Decode;
end;
{$ENDIF}



end.

