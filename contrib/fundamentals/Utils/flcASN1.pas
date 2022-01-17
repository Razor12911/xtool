{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals 5.00                                        }
{   File name:        cASN1.pas                                                }
{   File version:     5.05                                                     }
{   Description:      Abstract Syntax Notation One (ASN.1)                     }
{                     BER (Basic Encoding Routines)                            }
{                                                                              }
{   Copyright:        Copyright (c) 2010-2020, David J Butler                  }
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
{ References:                                                                  }
{                                                                              }
{   ITU-T Rec. X.690 (07/2002)                                                 }
{   http://www.itu.int/ITU-T/studygroups/com17/languages/X.690-0207.pdf        }
{   http://en.wikipedia.org/wiki/Abstract_Syntax_Notation_One                  }
{   http://en.wikipedia.org/wiki/Basic_Encoding_Rules                          }
{   http://luca.ntop.org/Teaching/Appunti/asn1.html                            }
{   http://www.obj-sys.com/asn1tutorial/node10.html                            }
{   [OID registry] http://www.alvestrand.no/objectid/top.html                  }
{   [OID registry] http://oid-info.com/get/1                                   }
{                                                                              }
{ Revision history:                                                            }
{                                                                              }
{   2010/11/08  0.01  Initial development: encoding routines.                  }
{   2010/11/23  0.02  Initial development: decoding routines.                  }
{   2011/04/02  0.03  Compilable with Delphi 5.                                }
{   2018/07/17  5.04  Revised for Fundamentals 5.                              }
{   2018/08/12  5.05  String type changes.                                     }
{                                                                              }
{ Supported compilers:                                                         }
{                                                                              }
{   Delphi 2010-10.4 Win32/Win64        5.05  2020/06/02                       }
{   Delphi 10.2-10.4 Linux64            5.05  2020/06/02                       }
{   FreePascal 3.0.4 Win64              5.05  2020/06/02                       }
{                                                                              }
{ Todo:                                                                        }
{ - Parser to str repr                                                         }
{ - Parser to dom                                                              }
{******************************************************************************}

{$INCLUDE ..\flcInclude.inc}

{$IFDEF DEBUG}
{$IFDEF TEST}
  {$DEFINE ASN1_TEST}
{$ENDIF}
{$ENDIF}

unit flcASN1;

interface

uses
  { System }
  SysUtils,

  { Fundamentals }
  flcStdTypes;



{ ASN.1 errors                                                                 }
type
  EASN1 = class(Exception);



{ Type identifier octet                    }
{                                          }
{   Class	            Bit 8   Bit 7        }
{   universal	            0	    0          }
{   application	          0	    1          }
{   context-specific      1	    0          }
{   private	              1	    1          }
{                                          }
{   Bit 6 = P/C (Primitive/Constructed)    }
const
  ASN1_ID_END_OF_CONTENT    = $00;
  ASN1_ID_BOOLEAN           = $01;
  ASN1_ID_INTEGER           = $02;
  ASN1_ID_BIT_STRING        = $03;
  ASN1_ID_OCTET_STRING      = $04;
  ASN1_ID_NULL              = $05;
  ASN1_ID_OBJECT_IDENTIFIER = $06;
  ASN1_ID_OBJECT_DESCRIPTOR = $07;
  ASN1_ID_EXTERNAL          = $08;
  ASN1_ID_REAL              = $09;
  ASN1_ID_ENUMERATED        = $0A;
  ASN1_ID_EMBEDDED_PDV      = $0B;
  ASN1_ID_UTF8STRING        = $0C; // ISO10646-1 (Unicode, UTF8)
  ASN1_ID_RELATIVE_OID      = $0D;
  ASN1_ID_NUMERICSTRING     = $12; // ASCII
  ASN1_ID_PRINTABLESTRING   = $13; // ASCII (A..Z, a..z, 0..9, ' = ( ) + , - . / : ?) (Excludes @ & _)
  ASN1_ID_T61STRING         = $14; // CCITT T.61 (Teletex) (ASCII, 8 bit, escape sequences)
  ASN1_ID_VIDEOTEXSTRING    = $15; // CCITT T.100/T.101
  ASN1_ID_IA5STRING         = $16; // International Alphabet 5 (ASCII)
  ASN1_ID_UTCTIME           = $17;
  ASN1_ID_GENERALIZEDTIME   = $18;
  ASN1_ID_GRAPHICSTRING     = $19;
  ASN1_ID_VISIBLESTRING     = $1A; // ISO646 (ASCII)
  ASN1_ID_GENERALSTRING     = $1B;
  ASN1_ID_UNIVERSALSTRING   = $1C; // ISO10646-1 (Unicode, UCS4)
  ASN1_ID_CHARACTERSTRING   = $1D;
  ASN1_ID_BMPSTRING         = $1E; // ISO10646-1 (Unicode, UCS2)
  ASN1_ID_SEQUENCE          = $30;
  ASN1_ID_SET               = $31;

  ASN1_ID_CONSTRUCTED       = $20;
  ASN1_ID_APPLICATION       = $40;
  ASN1_ID_CONTEXT_SPECIFIC  = $80;
  ASN1_ID_PRIVATE           = $C0;

  ASN1_ID_CONSTR_APPLICATION      = ASN1_ID_CONSTRUCTED or ASN1_ID_APPLICATION;
  ASN1_ID_CONSTR_CONTEXT_SPECIFIC = ASN1_ID_CONSTRUCTED or ASN1_ID_CONTEXT_SPECIFIC;
  ASN1_ID_CONSTR_PRIVATE          = ASN1_ID_CONSTRUCTED or ASN1_ID_PRIVATE;



{ Object identifiers  }

type
  TASN1ObjectIdentifier = array of Integer;

procedure ASN1OIDInit(var A: TASN1ObjectIdentifier; const B: array of Integer);
function  ASN1OIDToStrB(const A: TASN1ObjectIdentifier): RawByteString;
function  ASN1OIDToStr(const A: TASN1ObjectIdentifier): String;
function  ASN1OIDEqual(const A: TASN1ObjectIdentifier; const B: array of Integer): Boolean;



{ OIDs for common hash and cipher algorithms }

const
  // PKCS-1
  OID_PKCS_1         : array[0..5] of Integer = (1, 2,  840, 113549, 1,   1);
  OID_RSA            : array[0..6] of Integer = (1, 2,  840, 113549, 1,   1,  1);
  OID_RSA_PKCS1_MD2  : array[0..6] of Integer = (1, 2,  840, 113549, 1,   1,  2);
  OID_RSA_PKCS1_MD5  : array[0..6] of Integer = (1, 2,  840, 113549, 1,   1,  4);
  OID_RSA_PKCS1_SHA1 : array[0..6] of Integer = (1, 2,  840, 113549, 1,   1,  5);
  OID_RSA_OAEP       : array[0..6] of Integer = (1, 2,  840, 113549, 1,   1,  7);
  OID_RSA_MGF1       : array[0..6] of Integer = (1, 2,  840, 113549, 1,   1,  8);

  // PKCS#3
  OID_PKCS_3         : array[0..5] of Integer = (1, 2,  840, 113549, 1,   3);
  OID_DHKeyAgreement : array[0..6] of Integer = (1, 2,  840, 113549, 1,   3,  1);

  // PKCS-9/Signatures/SMIME/alg
  OID_PKCS_9         : array[0..5] of Integer = (1, 2,  840, 113549, 1,   9);
  OID_3DES_wrap      : array[0..8] of Integer = (1, 2,  840, 113549, 1,   9,  16, 3, 6);
  OID_RC2_wrap       : array[0..8] of Integer = (1, 2,  840, 113549, 1,   9,  16, 3, 7);
  OID_HMAC_3DES_wrap : array[0..8] of Integer = (1, 2,  840, 113549, 1,   9,  16, 3, 11);
  OID_HMAC_AES_wrap  : array[0..8] of Integer = (1, 2,  840, 113549, 1,   9,  16, 3, 12);

  // PKCS#2
  OID_MD2            : array[0..5] of Integer = (1, 2,  840, 113549, 2,   2);
  OID_MD4            : array[0..5] of Integer = (1, 2,  840, 113549, 2,   4);
  OID_MD5            : array[0..5] of Integer = (1, 2,  840, 113549, 2,   5);
  OID_HMAC_SHA1      : array[0..5] of Integer = (1, 2,  840, 113549, 2,   7);
  OID_HMAC_SHA224    : array[0..5] of Integer = (1, 2,  840, 113549, 2,   8);
  OID_HMAC_SHA256    : array[0..5] of Integer = (1, 2,  840, 113549, 2,   9);
  OID_HMAC_SHA384    : array[0..5] of Integer = (1, 2,  840, 113549, 2,   10);
  OID_HMAC_SHA512    : array[0..5] of Integer = (1, 2,  840, 113549, 2,   11);

  // rsadsi/encryptionalgorithm
  OID_RC2_CBC        : array[0..5] of Integer = (1, 2,  840, 113549, 3,   2);
  OID_RC2_ECB        : array[0..5] of Integer = (1, 2,  840, 113549, 3,   3);
  OID_RC4            : array[0..5] of Integer = (1, 2,  840, 113549, 3,   4);
  OID_RC4_MAC        : array[0..5] of Integer = (1, 2,  840, 113549, 3,   5);
  OID_DES_CBC        : array[0..5] of Integer = (1, 2,  840, 113549, 3,   6);
  OID_DES_EDE3_CBC   : array[0..5] of Integer = (1, 2,  840, 113549, 3,   7);
  OID_RC5_CBC        : array[0..5] of Integer = (1, 2,  840, 113549, 3,   8);
  OID_RC5_CBCPad     : array[0..5] of Integer = (1, 2,  840, 113549, 3,   9);

  // X9-57
  OID_DSA            : array[0..5] of Integer = (1, 2,  840, 10040,  4,   1);
  OID_DSA_SHA1       : array[0..5] of Integer = (1, 2,  840, 10040,  4,   3);

  // secsig
  OID_SHA1           : array[0..5] of Integer = (1, 3,  14,  3,      2,   26);



{ Encode }

function ASN1EncodeLength(const Len: Integer): RawByteString;
function ASN1EncodeObj(const TypeID: Byte; const Data: RawByteString): RawByteString;

function ANS1EncodeEndOfContent: RawByteString;
function ASN1EncodeNull: RawByteString;
function ASN1EncodeBoolean(const A: Boolean): RawByteString;
function ASN1EncodeDataInteger8(const A: ShortInt): RawByteString;
function ASN1EncodeDataInteger16(const A: SmallInt): RawByteString;
function ASN1EncodeDataInteger24(const A: Int32): RawByteString;
function ASN1EncodeDataInteger32(const A: Int32): RawByteString;
function ASN1EncodeDataInteger64(const A: Int64): RawByteString;
function ASN1EncodeInteger8(const A: ShortInt): RawByteString;
function ASN1EncodeInteger16(const A: SmallInt): RawByteString;
function ASN1EncodeInteger24(const A: Int32): RawByteString;
function ASN1EncodeInteger32(const A: Int32): RawByteString;
function ASN1EncodeInteger64(const A: Int64): RawByteString;
function ASN1EncodeIntegerBuf(const A; const Size: Integer): RawByteString;
function ASN1EncodeIntegerBufStr(const A: RawByteString): RawByteString;
function ASN1EncodeEnumerated(const A: Int64): RawByteString;
function ASN1EncodeBitString(const A: RawByteString; const UnusedBits: Byte): RawByteString;
function ASN1EncodeOctetString(const A: RawByteString): RawByteString;
function ASN1EncodeInt32AsOctetString(const A: Int32): RawByteString;
function ASN1EncodeUTF8String(const A: RawByteString): RawByteString;
function ASN1EncodeIA5String(const A: RawByteString): RawByteString;
function ASN1EncodeVisibleString(const A: RawByteString): RawByteString;
function ASN1EncodeNumericString(const A: RawByteString): RawByteString;
function ASN1EncodePrintableString(const A: RawByteString): RawByteString;
function ASN1EncodeTeletexString(const A: RawByteString): RawByteString;
function ASN1EncodeUniversalString(const A: UnicodeString): RawByteString;
function ASN1EncodeBMPString(const A: UnicodeString): RawByteString;
function ASN1EncodeUTCTime(const A: TDateTime): RawByteString;
function ASN1EncodeGeneralizedTime(const A: TDateTime): RawByteString;
function ASN1EncodeOID(const OID: array of Integer): RawByteString;
function ASN1EncodeSequence(const A: RawByteString): RawByteString;
function ASN1EncodeSet(const A: RawByteString): RawByteString;
function ASN1EncodeContextSpecific(const I: Integer; const A: RawByteString): RawByteString;



{ Decode }

function ASN1DecodeLength(const Buf; const Size: Integer; var Len: Integer): Integer;
function ASN1DecodeObjHeader(const Buf; const Size: Integer;
         var TypeID: Byte; var Len: Integer; var Data: Pointer): Integer;
function ASN1TypeIsConstructedType(const TypeID: Byte): Boolean;
function ASN1TypeIsContextSpecific(const TypeID: Byte; var Idx: Integer): Boolean;

function ASN1DecodeDataBoolean(const Buf; const Size: Integer; var A: Boolean): Integer;
function ASN1DecodeDataInteger32(const Buf; const Size: Integer; var A: Int32): Integer;
function ASN1DecodeDataInteger64(const Buf; const Size: Integer; var A: Int64): Integer;
function ASN1DecodeDataIntegerBuf(const Buf; const Size: Integer; var A: RawByteString): Integer;
function ASN1DecodeDataBitString(const Buf; const Size: Integer; var A: RawByteString; var UnusedBits: Byte): Integer;
function ASN1DecodeDataRawByteString(const Buf; const Size: Integer; var A: RawByteString): Integer;
function ASN1DecodeDataOctetString(const Buf; const Size: Integer; var A: RawByteString): Integer;
function ASN1DecodeDataIA5String(const Buf; const Size: Integer; var A: RawByteString): Integer;
function ASN1DecodeDataVisibleString(const Buf; const Size: Integer; var A: RawByteString): Integer;
function ASN1DecodeDataNumericString(const Buf; const Size: Integer; var A: RawByteString): Integer;
function ASN1DecodeDataPrintableString(const Buf; const Size: Integer; var A: RawByteString): Integer;
function ASN1DecodeDataTeletexString(const Buf; const Size: Integer; var A: RawByteString): Integer;
function ASN1DecodeDataUTF8String(const Buf; const Size: Integer; var A: RawByteString): Integer;
function ASN1DecodeDataUniversalString(const Buf; const Size: Integer; var A: RawByteString): Integer;
function ASN1DecodeDataBMPString(const Buf; const Size: Integer; var A: RawByteString): Integer;
function ASN1DecodeDataOID(const Buf; const Size: Integer; var A: TASN1ObjectIdentifier): Integer;
function ASN1DecodeDataUTCTime(const Buf; const Size: Integer; var A: TDateTime): Integer;
function ASN1DecodeDataGeneralizedTime(const Buf; const Size: Integer; var A: TDateTime): Integer;

function ASN1DecodeBoolean(const TypeID: Byte; const DataBuf; const DataSize: Integer; var A: Boolean): Integer;
function ASN1DecodeInteger32(const TypeID: Byte; const DataBuf; const DataSize: Integer; var A: Int32): Integer;
function ASN1DecodeInteger64(const TypeID: Byte; const DataBuf; const DataSize: Integer; var A: Int64): Integer;
function ASN1DecodeIntegerBuf(const TypeID: Byte; const DataBuf; const DataSize: Integer; var A: RawByteString): Integer;
function ASN1DecodeBitString(const TypeID: Byte; const DataBuf; const DataSize: Integer; var A: RawByteString; var UnusedBits: Byte): Integer;
function ASN1DecodeString(const TypeID: Byte; const DataBuf; const DataSize: Integer; var A: RawByteString): Integer;
function ASN1DecodeOID(const TypeID: Byte; const DataBuf; const DataSize: Integer; var A: TASN1ObjectIdentifier): Integer;
function ASN1DecodeTime(const TypeID: Byte; const DataBuf; const DataSize: Integer; var A: TDateTime): Integer;

type
  TASN1ParseProc =
      procedure (const TypeID: Byte; const DataBuf; const DataSize: Integer;
                 const ObjectIdx: Integer; const CallerData: NativeInt);

function ASN1Parse(
         const Buf; const Size: Integer;
         const ParseProc: TASN1ParseProc;
         const CallerData: NativeInt): Integer;



{                                                                              }
{ Test                                                                         }
{                                                                              }
{$IFDEF ASN1_TEST}
procedure Test;
{$ENDIF}



implementation

uses
  { Fundamentals }
  flcUtils,
  flcUTF,
  flcUnicodeCodecs;



{ Errors }

const
  SErr_EncodeError = 'Encode error';

  SErr_DecodeError           = 'Decode error';
  SErr_DecodeInvalidType     = 'Decode error: Invalid type';
  SErr_DecodeConversionError = 'Decode error: Invalid type conversion';



{ Constants }

const
  MinInt8  = -128;
  MaxInt8  = 127;
  MinInt16 = -32768;
  MaxInt16 = 32767;
  MinInt24 = -8388608;
  MaxInt24 = 8388607;
  MinInt32 = Low(Int32); // -2147483648;
  MaxInt32 = 2147483647;



{ Types }

{$IFDEF DELPHI5}
type
  PByte = ^Byte;
{$ENDIF}



{ Utilities }

{$IFDEF DELPHI5}
function TryStrToInt(const S: AnsiString; var I: Integer): Boolean;
var Error : Integer;
begin
  Val(S, I, Error);
  Result := Error = 0;
end;

function TryStrToInt64(const S: AnsiString; var I: Int64): Boolean;
var Error : Integer;
begin
  Val(S, I, Error);
  Result := Error = 0;
end;
{$ENDIF}



{ Object identifiers  }

procedure ASN1OIDInit(var A: TASN1ObjectIdentifier; const B: array of Integer);
var L, I : Integer;
begin
  L := Length(B);
  SetLength(A, L);
  for I := 0 to L - 1 do
    A[I] := B[I];
end;

function ASN1OIDToStrB(const A: TASN1ObjectIdentifier): RawByteString;
var I : Integer;
    S : RawByteString;
begin
  SetLength(S, 0);
  for I := 0 to Length(A) - 1 do
    begin
      if I > 0 then
        S := S + '.';
      S := S + IntToStringB(A[I]);
    end;
  Result := S;
end;

function ASN1OIDToStr(const A: TASN1ObjectIdentifier): String;
var I : Integer;
    S : String;
begin
  SetLength(S, 0);
  for I := 0 to Length(A) - 1 do
    begin
      if I > 0 then
        S := S + '.';
      S := S + IntToStr(A[I]);
    end;
  Result := S;
end;

function ASN1OIDEqual(const A: TASN1ObjectIdentifier; const B: array of Integer): Boolean;
var L, M, I : Integer;
begin
  L := Length(A);
  M := Length(B);
  Result := False;
  if L <> M then
    exit;
  for I := 0 to L - 1 do
    if A[I] <> B[I] then
      exit;
  Result := True;
end;



{ Encode }

function ASN1EncodeLength_Short(const Len: Integer): RawByteString;
begin
  Assert(Len >= 0);
  Assert(Len <= 127);

  SetLength(Result, 1);
  Result := ByteChar(Len);
end;

function ASN1EncodeLength_Long(const Len: Integer): RawByteString;
var N, I : Integer;
    B : array[0..3] of Byte;
begin
  Assert(Len >= 0);

  // count bytes required to represent Len
  N := 0;
  I := Len;
  repeat
    B[N] := I mod $100;
    I := I div $100;
    Inc(N);
  until I = 0;
  // set result
  SetLength(Result, N + 1);
  Result[1] := AnsiChar(N or $80);
  for I := 0 to N - 1 do
    Result[2 + I] := AnsiChar(B[N - I - 1]);
end;

function ASN1EncodeLength(const Len: Integer): RawByteString;
begin
  if Len < $80 then
    Result := ASN1EncodeLength_Short(Len)
  else
    Result := ASN1EncodeLength_Long(Len);
end;

function ASN1EncodeObj(const TypeID: Byte; const Data: RawByteString): RawByteString;
begin
  Result :=
      AnsiChar(TypeID) +
      ASN1EncodeLength(Length(Data)) +
      Data;
end;

function ANS1EncodeEndOfContent: RawByteString;
begin
  Result := ASN1EncodeObj(ASN1_ID_END_OF_CONTENT, '');
end;

function ASN1EncodeNull: RawByteString;
begin
  Result := ASN1EncodeObj(ASN1_ID_NULL, '');
end;

function ASN1EncodeBoolean(const A: Boolean): RawByteString;
begin
  if A then
    Result := ASN1EncodeObj(ASN1_ID_BOOLEAN, #$FF)
  else
    Result := ASN1EncodeObj(ASN1_ID_BOOLEAN, #$00);
end;

function ASN1EncodeDataInteger8(const A: ShortInt): RawByteString;
begin
  Result := AnsiChar(Byte(A));
end;

function ASN1EncodeDataInteger16(const A: SmallInt): RawByteString;
var D : RawByteString;
begin
  if (A >= MinInt8) and (A <= MaxInt8) then
    Result := ASN1EncodeDataInteger8(A)
  else
    begin
      SetLength(D, 2);
      Move(A, D[1], 2);
      Result := D;
    end;
end;

function ASN1EncodeDataInteger24(const A: Int32): RawByteString;
var D : RawByteString;
begin
  if (A >= MinInt16) and (A <= MaxInt16) then
    Result := ASN1EncodeDataInteger16(A)
  else
    begin
      SetLength(D, 3);
      Move(A, D[1], 3);
      Result := D;
    end;
end;

function ASN1EncodeDataInteger32(const A: Int32): RawByteString;
var D : RawByteString;
begin
  if (A >= MinInt24) and (A <= MaxInt24) then
    Result := ASN1EncodeDataInteger24(A)
  else
    begin
      SetLength(D, 4);
      Move(A, D[1], 4);
      Result := D;
    end;
end;

function ASN1EncodeDataInteger64(const A: Int64): RawByteString;
var D : RawByteString;
    F : Byte;
    B : array[0..7] of Byte;
    I, L : Integer;
begin
  Move(A, B[0], 8);
  if B[7] and $80 <> 0 then
    F := $FF
  else
    F := $00;
  L := 0;
  for I := 7 downto 1 do
    if B[I] <> F then
      begin
        L := I;
        break;
      end;
  if ((F = $00) and (B[L] and $80 <> 0)) or
     ((F = $FF) and (B[L] and $80 = 0)) then
    Inc(L);
  Inc(L);
  SetLength(D, L);
  Move(B[0], D[1], L);
  Result := D;
end;

function ASN1EncodeInteger8(const A: ShortInt): RawByteString;
begin
  Result := ASN1EncodeObj(ASN1_ID_INTEGER, ASN1EncodeDataInteger8(A));
end;

function ASN1EncodeInteger16(const A: SmallInt): RawByteString;
begin
  Result := ASN1EncodeObj(ASN1_ID_INTEGER, ASN1EncodeDataInteger16(A));
end;

function ASN1EncodeInteger24(const A: Int32): RawByteString;
begin
  Result := ASN1EncodeObj(ASN1_ID_INTEGER, ASN1EncodeDataInteger24(A));
end;

function ASN1EncodeInteger32(const A: Int32): RawByteString;
begin
  Result := ASN1EncodeObj(ASN1_ID_INTEGER, ASN1EncodeDataInteger32(A));
end;

function ASN1EncodeInteger64(const A: Int64): RawByteString;
begin
  Result := ASN1EncodeObj(ASN1_ID_INTEGER, ASN1EncodeDataInteger64(A));
end;

function ASN1EncodeIntegerBuf(const A; const Size: Integer): RawByteString;
var D : RawByteString;
begin
  Assert(Size > 0);

  SetLength(D, Size);
  Move(A, D[1], Size);
  Result := ASN1EncodeObj(ASN1_ID_INTEGER, D);
end;

function ASN1EncodeIntegerBufStr(const A: RawByteString): RawByteString;
begin
  Assert(A <> '');
  
  Result := ASN1EncodeObj(ASN1_ID_INTEGER, A);
end;

function ASN1EncodeEnumerated(const A: Int64): RawByteString;
begin
  Result := ASN1EncodeObj(ASN1_ID_ENUMERATED, ASN1EncodeDataInteger64(A));
end;

function ASN1EncodeBitString(const A: RawByteString; const UnusedBits: Byte): RawByteString;
begin
  if A = '' then
    Result := ASN1EncodeObj(ASN1_ID_BIT_STRING, AnsiChar(#0))
  else
    Result := ASN1EncodeObj(ASN1_ID_BIT_STRING, AnsiChar(UnusedBits) + A);
end;

function ASN1EncodeOctetString(const A: RawByteString): RawByteString;
begin
  Result := ASN1EncodeObj(ASN1_ID_OCTET_STRING, A);
end;

function ASN1EncodeInt32AsOctetString(const A: Int32): RawByteString;
var S : RawByteString;
    I : Integer;
    F : Int32;
begin
  SetLength(S, 4);
  F := A;
  for I := 0 to 3 do
    begin
      S[4 - I] := AnsiChar(F mod 256);
      F := F div 256;
    end;
  Result := ASN1EncodeOctetString(S);
end;

function ASN1EncodeUTF8String(const A: RawByteString): RawByteString;
begin
  Result := ASN1EncodeObj(ASN1_ID_UTF8STRING, A);
end;

function ASN1EncodeIA5String(const A: RawByteString): RawByteString;
begin
  Result := ASN1EncodeObj(ASN1_ID_IA5STRING, A);
end;

function ASN1EncodeVisibleString(const A: RawByteString): RawByteString;
begin
  Result := ASN1EncodeObj(ASN1_ID_VISIBLESTRING, A);
end;

function ASN1EncodeNumericString(const A: RawByteString): RawByteString;
begin
  Result := ASN1EncodeObj(ASN1_ID_NUMERICSTRING, A);
end;

function ASN1EncodePrintableString(const A: RawByteString): RawByteString;
begin
  Result := ASN1EncodeObj(ASN1_ID_PRINTABLESTRING, A);
end;

function ASN1EncodeTeletexString(const A: RawByteString): RawByteString;
begin
  Result := ASN1EncodeObj(ASN1_ID_T61STRING, A);
end;

function ASN1EncodeUniversalString(const A: UnicodeString): RawByteString;
var S : RawByteString;
begin
  S := UTF16ToEncodingU(TUCS4LECodec, A);
  Result := ASN1EncodeObj(ASN1_ID_UNIVERSALSTRING, S);
end;

function ASN1EncodeBMPString(const A: UnicodeString): RawByteString;
var S : RawByteString;
begin
  S := UTF16ToEncodingU(TUCS2Codec, A);
  Result := ASN1EncodeObj(ASN1_ID_UNIVERSALSTRING, S);
end;

function ASN1EncodeUTCTime(const A: TDateTime): RawByteString;
begin
  {$IFDEF StringIsUnicode}
  Result := ASN1EncodeObj(ASN1_ID_UTCTIME, RawByteString(FormatDateTime('YYMMDDHHNNSS', A)) + 'Z');
  {$ELSE}
  Result := ASN1EncodeObj(ASN1_ID_UTCTIME, FormatDateTime('YYMMDDHHNNSS', A) + 'Z');
  {$ENDIF}
end;

function ASN1EncodeGeneralizedTime(const A: TDateTime): RawByteString;
begin
  {$IFDEF StringIsUnicode}
  Result := ASN1EncodeObj(ASN1_ID_GENERALIZEDTIME, RawByteString(FormatDateTime('YYYYMMDDHHNNSS', A)) + 'Z');
  {$ELSE}
  Result := ASN1EncodeObj(ASN1_ID_GENERALIZEDTIME, FormatDateTime('YYYYMMDDHHNNSS', A) + 'Z');
  {$ENDIF}
end;

// OID implementation limits
const
  MAX_BER_OIDPart_Len = 6;   // 6 = max 42-bit part, 10 = max 70-bit part
  MAX_BER_OID_Length  = 512;
  MAX_OID_Parts       = 64;

type
  TOIDPartEnc = record
    Len : Byte;
    Enc : array[0..MAX_BER_OIDPart_Len - 1] of Byte;
  end;

procedure ASN1BEREncodeOIDPart(const OIDPart: Integer; var Enc: TOIDPartEnc);
var N, A, B, C : Integer;
begin
  N := 0;
  A := OIDPart;
  repeat
    B := A div 128;
    C := A mod 128;
    if N > 0 then
      C := C or 128;
    if N >= MAX_BER_OIDPart_Len then
      raise EASN1.Create(SErr_EncodeError);
    Enc.Enc[N] := Byte(C);
    Inc(N);
    A := B;
  until A = 0;
  Enc.Len := N;
end;

// BER Encode OID
function ASN1EncodeOID(const OID: array of Integer): RawByteString;
var L, I, N, J, K : Integer;
    A, B, C : Integer;
    E : TOIDPartEnc;
    Buf : array[0..MAX_BER_OID_Length - 1] of Byte;
    S : RawByteString;
begin
  Result := '';
  L := Length(OID);
  if L < 2 then
    raise EASN1.Create(SErr_EncodeError);
  A := OID[0];
  B := OID[1];
  C := A * 40 + B;
  if C > $FF then
    raise EASN1.Create(SErr_EncodeError);
  N := 1;
  Buf[0] := C;
  for I := 2 to L - 1 do
    begin
      A := OID[I];
      ASN1BEREncodeOIDPart(A, E);
      K := E.Len;
      if N + K - 1 >= MAX_BER_OID_Length then
        raise EASN1.Create(SErr_EncodeError);
      for J := 0 to K - 1 do
        Buf[N + J] := E.Enc[K - J - 1];
      Inc(N, K);
    end;
  SetLength(S, N);
  Move(Buf[0], S[1], N);
  Result := ASN1EncodeObj(ASN1_ID_OBJECT_IDENTIFIER, S);
end;

function ASN1EncodeSequence(const A: RawByteString): RawByteString;
begin
  Result := ASN1EncodeObj(ASN1_ID_SEQUENCE, A);
end;

function ASN1EncodeSet(const A: RawByteString): RawByteString;
begin
  Result := ASN1EncodeObj(ASN1_ID_SET, A);
end;

function ASN1EncodeContextSpecific(const I: Integer; const A: RawByteString): RawByteString;
begin
  Assert(I >= 0);
  Assert(I <= 31);
  Result := ASN1EncodeObj(ASN1_ID_CONSTR_CONTEXT_SPECIFIC + I, A);
end;



{ Decode }

function ASN1DecodeLength(const Buf; const Size: Integer; var Len: Integer): Integer;
var P : PByte;
    L, I : Byte;
begin
  if Size <= 0 then
    raise EASN1.Create(SErr_DecodeError); // buffer too small
  P := @Buf;
  L := P^;
  if L < $80 then
    begin
      Len := L;
      Result := 1;
      exit;
    end;
  L := L and $7F;
  if Size < L + 1 then
    raise EASN1.Create(SErr_DecodeError); // buffer too small
  if L > 3 then
    raise EASN1.Create(SErr_DecodeError); // size too big
  Len := 0;
  for I := 0 to L - 1 do
    begin
      Inc(P);
      Len := (Len shl 8) or P^;
    end;
  Result := L + 1;
end;

function ASN1DecodeObjHeader(const Buf; const Size: Integer;
         var TypeID: Byte; var Len: Integer; var Data: Pointer): Integer;
var P : PByte;
    L : Integer;
begin
  Assert(Assigned(@Buf));
  if Size < 2 then
    raise EASN1.Create(SErr_DecodeError); // buffer too small
  P := @Buf;
  TypeID := P^;
  Inc(P);
  L := ASN1DecodeLength(P^, Size - 1, Len);
  Inc(P, L);
  Data := P;
  Result := L + 1;
end;

function ASN1TypeIsConstructedType(const TypeID: Byte): Boolean;
begin
  Result := TypeID and ASN1_ID_CONSTRUCTED <> 0;
end;

function ASN1TypeIsContextSpecific(const TypeID: Byte; var Idx: Integer): Boolean;
begin
  if TypeID and ASN1_ID_CONTEXT_SPECIFIC <> 0 then
    begin
      Idx := TypeID and $1F;
      Result := True;
    end
  else
    begin
      Idx := -1;
      Result := False;
    end;
end;

function ASN1DecodeDataBoolean(const Buf; const Size: Integer; var A: Boolean): Integer;
begin
  if Size <> 1 then
    raise EASN1.Create(SErr_DecodeError);
  A := PByte(@Buf)^ <> 0;
  Result := 1;
end;

function ASN1DecodeDataInteger32(const Buf; const Size: Integer; var A: Int32): Integer;
var P : PByte;
    L, I : Integer;
    B : array[0..3] of Byte;
    E : Byte;
begin
  P := @Buf;
  L := Size;
  if (L <= 0) or (L > 4) then
    raise EASN1.Create(SErr_DecodeError); // integer too big for Int32
  Move(P^, B[0], L);
  if L < 4 then
    begin
      // extend sign
      if B[L - 1] >= $80 then
        E := $FF
      else
        E := $00;
      for I := L to 3 do
        B[I] := E;
    end;
  Move(B[0], A, 4);
  Result := L;
end;

function ASN1DecodeDataInteger64(const Buf; const Size: Integer; var A: Int64): Integer;
var P : PByte;
    L, I : Integer;
    B : array[0..7] of Byte;
    E : Byte;
begin
  P := @Buf;
  L := Size;
  if (L <= 0) or (L > 8) then
    raise EASN1.Create(SErr_DecodeError); // integer too big for Int64
  Move(P^, B[0], L);
  if L < 8 then
    begin
      // extend sign
      if B[L - 1] >= $80 then
        E := $FF
      else
        E := $00;
      for I := L to 7 do
        B[I] := E;
    end;
  Move(B[0], A, 8);
  Result := L;
end;

function ASN1DecodeDataIntegerBuf(const Buf; const Size: Integer; var A: RawByteString): Integer;
var L : Integer;
begin
  L := Size;
  SetLength(A, L);
  if L > 0 then
    Move(Buf, A[1], L);
  Result := L;
end;

function ASN1DecodeDataBitString(const Buf; const Size: Integer; var A: RawByteString; var UnusedBits: Byte): Integer;
var P : PByte;
    L : Integer;
    F : Byte;
begin
  P := @Buf;
  L := Size;
  if L <= 0 then
    raise EASN1.Create(SErr_DecodeError); // invalid size
  F := P^;
  if F > 7 then
    raise EASN1.Create(SErr_DecodeError); // invalid UnusedBits
  if L = 1 then
    begin
      if F <> 0 then
        raise EASN1.Create(SErr_DecodeError); // invalid size
      A := '';
      UnusedBits := 0;
      Result := 1;
      exit;
    end;
  Inc(P);
  Dec(L);
  SetLength(A, L);
  if L > 0 then
    Move(P^, A[1], L);
  UnusedBits := F;
  Result := L + 1;
end;

function ASN1DecodeDataRawByteString(const Buf; const Size: Integer; var A: RawByteString): Integer;
var L : Integer;
begin
  L := Size;
  SetLength(A, L);
  if L > 0 then
    Move(Buf, A[1], L);
  Result := L;
end;

function ASN1DecodeDataOctetString(const Buf; const Size: Integer; var A: RawByteString): Integer;
begin
  Result := ASN1DecodeDataRawByteString(Buf, Size, A);
end;

function ASN1DecodeDataIA5String(const Buf; const Size: Integer; var A: RawByteString): Integer;
begin
  Result := ASN1DecodeDataRawByteString(Buf, Size, A);
end;

function ASN1DecodeDataVisibleString(const Buf; const Size: Integer; var A: RawByteString): Integer;
begin
  Result := ASN1DecodeDataRawByteString(Buf, Size, A);
end;

function ASN1DecodeDataNumericString(const Buf; const Size: Integer; var A: RawByteString): Integer;
begin
  Result := ASN1DecodeDataRawByteString(Buf, Size, A);
end;

function ASN1DecodeDataPrintableString(const Buf; const Size: Integer; var A: RawByteString): Integer;
begin
  Result := ASN1DecodeDataRawByteString(Buf, Size, A);
end;

function ASN1DecodeDataTeletexString(const Buf; const Size: Integer; var A: RawByteString): Integer;
begin
  Result := ASN1DecodeDataRawByteString(Buf, Size, A);
end;

function ASN1DecodeDataUTF8String(const Buf; const Size: Integer; var A: RawByteString): Integer;
begin
  Result := ASN1DecodeDataRawByteString(Buf, Size, A);
end;

function ASN1DecodeDataUniversalString(const Buf; const Size: Integer; var A: RawByteString): Integer;
begin
  A := UnicodeStringToUTF8String(EncodingToUTF16U(TUCS4LECodec, @Buf, Size));
  Result := Size;
end;

function ASN1DecodeDataBMPString(const Buf; const Size: Integer; var A: RawByteString): Integer;
begin
  A := UnicodeStringToUTF8String(EncodingToUTF16U(TUCS2Codec, @Buf, Size));
  Result := Size;
end;

function ASN1DecodeOIDPart(const Buf; const Size: Integer; var Part: Integer): Integer;
var P : PByte;
    L, C : Integer;
    A : Byte;
    R : Boolean;
    V : Int64;
begin
  P := @Buf;
  L := Size;
  C := 1;
  V := 0;
  repeat
    if L < 0 then
      raise EASN1.Create(SErr_DecodeError);
    if C > MAX_BER_OIDPart_Len then
      raise EASN1.Create(SErr_DecodeError); // too many bytes in this OID part
    A := P^;
    R := A and 128 = 0;
    V := (V shl 7) or (A and 127);
    Inc(P);
    Dec(L);
    Inc(C);
  until R;
  if V > MaxInt32 then
    raise EASN1.Create(SErr_DecodeError); // value too large
  Part := V;
  Result := Size - L;
end;

function ASN1DecodeDataOID(const Buf; const Size: Integer; var A: TASN1ObjectIdentifier): Integer;
var P : PByte;
    L, N, C, I : Integer;
    F : Byte;
    Parts : array[0..MAX_OID_Parts - 1] of Integer;
begin
  P := @Buf;
  L := Size;
  if L < 1 then
    raise EASN1.Create(SErr_DecodeError);
  F := P^;
  Parts[0] := F div 40;
  Parts[1] := F mod 40;
  Inc(P);
  Dec(L);
  N := 2;
  while L > 0 do
    begin
      if N >= MAX_OID_Parts then
        raise EASN1.Create(SErr_DecodeError);
      C := ASN1DecodeOIDPart(P^, L, Parts[N]);
      Inc(P, C);
      Dec(L, C);
      Inc(N);
    end;
  SetLength(A, N);
  for I := 0 to N - 1 do
    A[I] := Parts[I];
  Result := Size - L;
end;

function ASN1DecodeDataUTCTime(const Buf; const Size: Integer; var A: TDateTime): Integer;
var D : RawByteString;
    YYYY, YY, MM, DD, HH, NN, SS : Integer;
begin
  if Size < 6 then
    raise EASN1.Create(SErr_DecodeError);
  SetLength(D, Size);
  Move(Buf, D[1], Size);
  if not TryStringToIntB(Copy(D, 1, 2), YY) then
    raise EASN1.Create(SErr_DecodeError);
  if not TryStringToIntB(Copy(D, 3, 2), MM) then
    raise EASN1.Create(SErr_DecodeError);
  if not TryStringToIntB(Copy(D, 5, 2), DD) then
    raise EASN1.Create(SErr_DecodeError);
  HH := 0;
  NN := 0;
  SS := 0;
  if Size >= 10 then
    begin
      if not TryStringToIntB(Copy(D, 7, 2), HH) then
        raise EASN1.Create(SErr_DecodeError);
      if not TryStringToIntB(Copy(D, 9, 2), NN) then
        raise EASN1.Create(SErr_DecodeError);
      if Size >= 12 then
        if not TryStringToIntB(Copy(D, 11, 2), SS) then
          raise EASN1.Create(SErr_DecodeError);
    end;
  if YY <= 49 then
    YYYY := 2000 + YY
  else
    YYYY := 1900 + YY;
  A := EncodeDate(YYYY, MM, DD) +
       EncodeTime(HH, NN, SS, 0);
  Result := Size;
end;

function ASN1DecodeDataGeneralizedTime(const Buf; const Size: Integer; var A: TDateTime): Integer;
var D : RawByteString;
    YYYY, MM, DD, HH, NN, SS : Integer;
begin
  if Size < 8 then
    raise EASN1.Create(SErr_DecodeError);
  SetLength(D, Size);
  Move(Buf, D[1], Size);
  if not TryStringToIntB(Copy(D, 1, 4), YYYY) then
    raise EASN1.Create(SErr_DecodeError);
  if not TryStringToIntB(Copy(D, 5, 2), MM) then
    raise EASN1.Create(SErr_DecodeError);
  if not TryStringToIntB(Copy(D, 7, 2), DD) then
    raise EASN1.Create(SErr_DecodeError);
  HH := 0;
  NN := 0;
  SS := 0;
  if Size >= 12 then
    begin
      if not TryStringToIntB(Copy(D, 9, 2), HH) then
        raise EASN1.Create(SErr_DecodeError);
      if not TryStringToIntB(Copy(D, 11, 2), NN) then
        raise EASN1.Create(SErr_DecodeError);
      if Size >= 14 then
        if not TryStringToIntB(Copy(D, 13, 2), SS) then
          raise EASN1.Create(SErr_DecodeError);
    end;
  A := EncodeDate(YYYY, MM, DD) +
       EncodeTime(HH, NN, SS, 0);
  Result := Size;
end;

function ASN1DecodeBoolean(const TypeID: Byte; const DataBuf; const DataSize: Integer; var A: Boolean): Integer;
var I : Int64;
begin
  case TypeID of
    ASN1_ID_BOOLEAN : Result := ASN1DecodeDataBoolean(DataBuf, DataSize, A);
    ASN1_ID_INTEGER :
      begin
        Result := ASN1DecodeDataInteger64(DataBuf, DataSize, I);
        A := I <> 0;
      end;
  else
    raise EASN1.Create(SErr_DecodeInvalidType);
  end;
end;

function ASN1DecodeInteger32(const TypeID: Byte; const DataBuf; const DataSize: Integer;
         var A: Int32): Integer;
var I : Int64;
begin
  Result := ASN1DecodeInteger64(TypeID, DataBuf, DataSize, I);
  if (I > High(Int32)) or (I < Low(Int32)) then
    raise EASN1.Create(SErr_DecodeError);
  A := I;
end;

function ASN1DecodeInteger64(const TypeID: Byte; const DataBuf; const DataSize: Integer;
         var A: Int64): Integer;
var S : RawByteString;
begin
  case TypeID of
    ASN1_ID_INTEGER      : Result := ASN1DecodeDataInteger64(DataBuf, DataSize, A);
    ASN1_ID_OCTET_STRING,
    ASN1_ID_UTF8STRING,
    ASN1_ID_NUMERICSTRING,
    ASN1_ID_PRINTABLESTRING,
    ASN1_ID_VISIBLESTRING,
    ASN1_ID_T61STRING,
    ASN1_ID_UNIVERSALSTRING,
    ASN1_ID_BMPSTRING :
      begin
        Result := ASN1DecodeString(TypeID, DataBuf, DataSize, S);
        if not TryStringToInt64B(S, A) then
          raise EASN1.Create(SErr_DecodeConversionError);
      end;
  else
    raise EASN1.Create(SErr_DecodeInvalidType);
  end;
end;

function ASN1DecodeIntegerBuf(const TypeID: Byte; const DataBuf; const DataSize: Integer;
         var A: RawByteString): Integer;
begin
  case TypeID of
    ASN1_ID_INTEGER : Result := ASN1DecodeDataIntegerBuf(DataBuf, DataSize, A);
  else
    raise EASN1.Create(SErr_DecodeInvalidType);
  end;
end;

function ASN1DecodeBitString(const TypeID: Byte; const DataBuf; const DataSize: Integer; var A: RawByteString; var UnusedBits: Byte): Integer;
begin
  case TypeID of
    ASN1_ID_BIT_STRING : Result := ASN1DecodeDataBitString(DataBuf, DataSize, A, UnusedBits);
  else
    raise EASN1.Create(SErr_DecodeInvalidType);
  end;
end;

type
  TStringParseData = record
    Delim : RawByteString;
    Str   : RawByteString;
  end;
  PStringParseData = ^TStringParseData;

procedure StringParseProc(const TypeID: Byte; const DataBuf; const DataSize: Integer;
          const ObjectIdx: Integer; const CallerData: NativeInt);
var S : RawByteString;
    D : PStringParseData;
begin
  ASN1DecodeString(TypeID, DataBuf, DataSize, S);
  D := Pointer(CallerData);
  if (ObjectIdx > 0) and (D^.Delim <> '') then
    D^.Str := D^.Str + D^.Delim;
  D^.Str := D^.Str + S;
end;

function ASN1DecodeString(const TypeID: Byte; const DataBuf; const DataSize: Integer;
         var A: RawByteString): Integer;
var I : Int64;
    B : TASN1ObjectIdentifier;
    D : TStringParseData;
    K : Byte;
begin
  if ASN1TypeIsConstructedType(TypeID) then
    begin
      D.Delim := ' ';
      Result := ASN1Parse(DataBuf, DataSize, StringParseProc, NativeInt(@D));
      A := D.Str;
    end
  else
    case TypeID of
      ASN1_ID_OCTET_STRING    : Result := ASN1DecodeDataOctetString(DataBuf, DataSize, A);
      ASN1_ID_UTF8STRING      : Result := ASN1DecodeDataUTF8String(DataBuf, DataSize, A);
      ASN1_ID_NUMERICSTRING   : Result := ASN1DecodeDataNumericString(DataBuf, DataSize, A);
      ASN1_ID_PRINTABLESTRING : Result := ASN1DecodeDataPrintableString(DataBuf, DataSize, A);
      ASN1_ID_VISIBLESTRING   : Result := ASN1DecodeDataVisibleString(DataBuf, DataSize, A);
      ASN1_ID_T61STRING       : Result := ASN1DecodeDataTeletexString(DataBuf, DataSize, A);
      ASN1_ID_UNIVERSALSTRING : Result := ASN1DecodeDataUniversalString(DataBuf, DataSize, A);
      ASN1_ID_BMPSTRING       : Result := ASN1DecodeDataBMPString(DataBuf, DataSize, A);
      ASN1_ID_BIT_STRING      : Result := ASN1DecodeDataBitString(DataBuf, DataSize, A, K);
      ASN1_ID_IA5STRING       : Result := ASN1DecodeDataIA5String(DataBuf, DataSize, A);
      ASN1_ID_INTEGER :
        begin
          Result := ASN1DecodeDataInteger64(DataBuf, DataSize, I);
          A := IntToStringB(I);
        end;
      ASN1_ID_OBJECT_IDENTIFIER :
        begin
          Result := ASN1DecodeDataOID(DataBuf, DataSize, B);
          A := ASN1OIDToStrB(B);
        end;
    else
      raise EASN1.Create(SErr_DecodeInvalidType);
    end;
end;

function ASN1DecodeOID(const TypeID: Byte; const DataBuf; const DataSize: Integer;
         var A: TASN1ObjectIdentifier): Integer;
begin
  case TypeID of
    ASN1_ID_OBJECT_IDENTIFIER : Result := ASN1DecodeDataOID(DataBuf, DataSize, A);
  else
    raise EASN1.Create(SErr_DecodeInvalidType);
  end;
end;

function ASN1DecodeTime(const TypeID: Byte; const DataBuf; const DataSize: Integer; var A: TDateTime): Integer;
begin
  case TypeID of
    ASN1_ID_UTCTIME         : Result := ASN1DecodeDataUTCTime(DataBuf, DataSize, A);
    ASN1_ID_GENERALIZEDTIME : Result := ASN1DecodeDataGeneralizedTime(DataBuf, DataSize, A);
  else
    raise EASN1.Create(SErr_DecodeInvalidType);
  end;
end;

function ASN1Parse(const Buf; const Size: Integer; const ParseProc: TASN1ParseProc; const CallerData: NativeInt): Integer;
var P : PByte;
    L, N, T : Integer;
    ObjIdx : Integer;
    TypeID : Byte;
    Len : Integer;
    Data : Pointer;
begin
  P := @Buf;
  L := Size;
  if (L < 0) or not Assigned(P) then
    raise EASN1.Create(SErr_DecodeError); // invalid buffer
  if L = 0 then
    begin
      // nothing to parse
      Result := 0;
      exit;
    end;
  // iterate top level ASN.1 objects in buffer
  ObjIdx := 0;
  while L > 0 do
    begin
      N := ASN1DecodeObjHeader(P^, L, TypeID, Len, Data);
      T := N + Len;
      if T > L then
        raise EASN1.Create(SErr_DecodeError); // invalid length encoded in header
      ParseProc(TypeID, Data^, Len, ObjIdx, CallerData);
      Inc(P, T);
      Dec(L, T);
      Inc(ObjIdx);
    end;
  Result := Size - L;
end;



{                                                                              }
{ Test                                                                         }
{                                                                              }
{$IFDEF ASN1_TEST}
{$ASSERTIONS ON}
procedure TestParseProc(
          const TypeID: Byte; const DataBuf; const DataSize: Integer;
          const ObjectIdx: Integer; const CallerData: NativeInt);
var I : Int64;
    S : RawByteString;
begin
  case CallerData of
    0 : case ObjectIdx of
          0 : begin
                Assert(TypeID = ASN1_ID_SEQUENCE);
                Assert(ASN1Parse(DataBuf, DataSize, TestParseProc, 1) = DataSize);
              end;
        else
          Assert(False);
        end;
    1 : case ObjectIdx of
          0 : begin
                Assert(TypeID = ASN1_ID_INTEGER);
                ASN1DecodeInteger64(TypeID, DataBuf, DataSize, I);
                Assert(I = 123);
              end;
          1 : begin
                Assert(TypeID = ASN1_ID_PRINTABLESTRING);
                ASN1DecodeString(TypeID, DataBuf, DataSize, S);
                Assert(S = 'ABC');
              end;
        else
          Assert(False);
        end;
  else
    Assert(False);
  end;
end;

procedure Test;
var S : RawByteString;
    L, I, J : Integer;
    D : TASN1ObjectIdentifier;
begin
  Assert(ASN1EncodeLength(0) = #$00);
  Assert(ASN1EncodeLength(1) = #$01);
  Assert(ASN1EncodeLength($7F) = #$7F);
  Assert(ASN1EncodeLength($80) = #$81#$80);
  Assert(ASN1EncodeLength($FF) = #$81#$FF);
  Assert(ASN1EncodeLength($100) = #$82#$01#$00);

  Assert(ASN1EncodeOID(OID_3DES_wrap) = #$06#$0b#$2a#$86#$48#$86#$f7#$0d#$01#$09#$10#$03#$06);
  Assert(ASN1EncodeOID(OID_RC2_wrap)  = #$06#$0b#$2a#$86#$48#$86#$f7#$0d#$01#$09#$10#$03#$07);

  S := RawByteString(#$2a#$86#$48#$86#$f7#$0d#$01#$09#$10#$03#$06);
  L := Length(S);
  Assert(ASN1DecodeDataOID(S[1], L, D) = L);
  Assert(Length(D) = 9);
  Assert((D[0] = 1) and (D[1] = 2) and (D[2] = 840) and (D[3] = 113549) and
         (D[4] = 1) and (D[5] = 9) and (D[6] = 16) and (D[7] = 3) and (D[8] = 6));
  Assert(ASN1OIDToStrB(D) = '1.2.840.113549.1.9.16.3.6');

  S := RawByteString(#$2a#$86#$48#$86#$f7#$0d#$03#$06);
  L := Length(S);
  Assert(ASN1DecodeDataOID(S[1], L, D) = L);
  Assert(Length(D) = 6);
  Assert((D[0] = 1) and (D[1] = 2) and (D[2] = 840) and (D[3] = 113549) and
         (D[4] = 3) and (D[5] = 6));

  Assert(ASN1EncodeInteger32(0) = #$02#$01#$00);
  Assert(ASN1EncodeInteger32(1) = #$02#$01#$01);
  Assert(ASN1EncodeInteger32(-1) = #$02#$01#$FF);
  Assert(ASN1EncodeInteger32(-$80) = #$02#$01#$80);
  Assert(ASN1EncodeInteger32(-$81) = #$02#$02#$7F#$FF);
  Assert(ASN1EncodeInteger32(-$FF) = #$02#$02#$01#$FF);
  Assert(ASN1EncodeInteger32($7F) = #$02#$01#$7F);
  Assert(ASN1EncodeInteger32($80) = #$02#$02#$80#$00);
  Assert(ASN1EncodeInteger32($FF) = #$02#$02#$FF#$00);

  for I := -512 to 512 do
    begin
      S := ASN1EncodeInteger32(I);
      Assert(S = ASN1EncodeInteger64(I));
      L := Length(S);
      Assert(ASN1DecodeDataInteger32(S[3], L - 2, J) = L - 2);
      Assert(J = I);
    end;

  S :=
    ASN1EncodeSequence(
        ASN1EncodeInteger32(123) +
        ASN1EncodePrintableString('ABC')
        );
  L := Length(S);
  Assert(L > 0);
  Assert(ASN1Parse(S[1], L, @TestParseProc, 0) = L);
end;
{$ENDIF}



end.

