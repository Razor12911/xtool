{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals TLS                                         }
{   File name:        flcTLSPRF.pas                                            }
{   File version:     5.02                                                     }
{   Description:      TLS PRF (Pseudo Random Function)                         }
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
{   2020/05/09  5.02  Create flcTLSPRF unit from flcTLSUtils unit.             }
{                                                                              }
{******************************************************************************}

{$INCLUDE flcTLS.inc}

unit flcTLSPRF;

interface

uses
  { Utils }

  flcStdTypes,

  { TLS }

  flcTLSProtocolVersion;



{                                                                              }
{ PRFAlgorithm                                                                 }
{                                                                              }
type
  TTLSPRFAlgorithm = (
    tlspaSHA256
    );



{                                                                              }
{ PRF (Pseudo-Random Function)                                                 }
{                                                                              }
function  tlsP_MD5(const Secret, Seed: RawByteString; const Size: Integer): RawByteString;
function  tlsP_SHA1(const Secret, Seed: RawByteString; const Size: Integer): RawByteString;
function  tlsP_SHA256(const Secret, Seed: RawByteString; const Size: Integer): RawByteString;
function  tlsP_SHA512(const Secret, Seed: RawByteString; const Size: Integer): RawByteString;

function  tls10PRF(const Secret, ALabel, Seed: RawByteString; const Size: Integer): RawByteString;
function  tls12PRF_SHA256(const Secret, ALabel, Seed: RawByteString; const Size: Integer): RawByteString;
function  tls12PRF_SHA512(const Secret, ALabel, Seed: RawByteString; const Size: Integer): RawByteString;

function  TLSPRF(const ProtocolVersion: TTLSProtocolVersion;
          const Secret, ALabel, Seed: RawByteString; const Size: Integer): RawByteString;



{                                                                              }
{ Test                                                                         }
{                                                                              }
{$IFDEF TLS_TEST}
procedure Test;
{$ENDIF}



implementation

uses
  { Utils }

  flcHash,

  { TLS }

  flcTLSErrors;



{                                                                              }
{ P_hash                                                                       }
{ P_hash(secret, seed) = HMAC_hash(secret, A(1) + seed) +                      }
{                        HMAC_hash(secret, A(2) + seed) +                      }
{                        HMAC_hash(secret, A(3) + seed) + ...                  }
{ Where + indicates concatenation.                                             }
{ A() is defined as:                                                           }
{     A(0) = seed                                                              }
{     A(i) = HMAC_hash(secret, A(i-1))                                         }
{                                                                              }
function tlsP_MD5(const Secret, Seed: RawByteString; const Size: Integer): RawByteString;
var A, P : RawByteString;
    L : Integer;
begin
  P := '';
  L := 0;
  A := Seed;
  repeat
    A := MD5DigestToStrA(CalcHMAC_MD5(Secret, A));
    P := P + MD5DigestToStrA(CalcHMAC_MD5(Secret, A + Seed));
    Inc(L, 16);
  until L >= Size;
  if L > Size then
    SetLength(P, Size);
  Result := P;
end;

function tlsP_SHA1(const Secret, Seed: RawByteString; const Size: Integer): RawByteString;
var A, P : RawByteString;
    L : Integer;
begin
  P := '';
  L := 0;
  A := Seed;
  repeat
    A := SHA1DigestToStrA(CalcHMAC_SHA1(Secret, A));
    P := P + SHA1DigestToStrA(CalcHMAC_SHA1(Secret, A + Seed));
    Inc(L, 20);
  until L >= Size;
  if L > Size then
    SetLength(P, Size);
  Result := P;
end;

function tlsP_SHA256(const Secret, Seed: RawByteString; const Size: Integer): RawByteString;
var A, P : RawByteString;
    L : Integer;
begin
  P := '';
  L := 0;
  A := Seed;
  repeat
    A := SHA256DigestToStrA(CalcHMAC_SHA256(Secret, A));
    P := P + SHA256DigestToStrA(CalcHMAC_SHA256(Secret, A + Seed));
    Inc(L, 32);
  until L >= Size;
  if L > Size then
    SetLength(P, Size);
  Result := P;
end;

function tlsP_SHA512(const Secret, Seed: RawByteString; const Size: Integer): RawByteString;
var A, P : RawByteString;
    L : Integer;
begin
  P := '';
  L := 0;
  A := Seed;
  repeat
    A := SHA512DigestToStrA(CalcHMAC_SHA512(Secret, A));
    P := P + SHA512DigestToStrA(CalcHMAC_SHA512(Secret, A + Seed));
    Inc(L, 64);
  until L >= Size;
  if L > Size then
    SetLength(P, Size);
  Result := P;
end;



{                                                                              }
{ PRF                                                                          }
{ TLS 1.0:                                                                     }
{ PRF(secret, label, seed) = P_MD5(S1, label + seed) XOR                       }
{                            P_SHA-1(S2, label + seed);                        }
{ S1 and S2 are the two halves of the secret and each is the same length.      }
{ S1 is taken from the first half of the secret, S2 from the second half.      }
{ Their length is created by rounding up the length of the overall secret      }
{ divided by two; thus, if the original secret is an odd number of bytes       }
{ long, the last byte of S1 will be the same as the first byte of S2.          }
{                                                                              }
{ TLS 1.2:                                                                     }
{ PRF(secret, label, seed) = P_<hash>(secret, label + seed)                    }
{ P_SHA-256                                                                    }
{                                                                              }
procedure tls10PRFSplitSecret(const Secret: RawByteString; var S1, S2: RawByteString);
var L, N : Integer;
begin
  N := Length(Secret);
  L := N;
  if L mod 2 = 1 then
    Inc(L);
  L := L div 2;
  S1 := Copy(Secret, 1, L);
  S2 := Copy(Secret, N - L + 1, L);
end;

function tls10PRF(const Secret, ALabel, Seed: RawByteString; const Size: Integer): RawByteString;
var S1, S2 : RawByteString;
    P1, P2 : RawByteString;
    R      : RawByteString;
    I      : Integer;
begin
  tls10PRFSplitSecret(Secret, S1, S2);
  P1 := tlsP_MD5(S1, ALabel + Seed, Size);
  P2 := tlsP_SHA1(S2, ALabel + Seed, Size);
  SetLength(R, Size);
  for I := 1 to Size do
    R[I] := AnsiChar(Byte(P1[I]) xor Byte(P2[I]));
  Result := R;
end;

function tls12PRF_SHA256(const Secret, ALabel, Seed: RawByteString; const Size: Integer): RawByteString;
begin
  Result := tlsP_SHA256(Secret, ALabel + Seed, Size);
end;

function tls12PRF_SHA512(const Secret, ALabel, Seed: RawByteString; const Size: Integer): RawByteString;
begin
  Result := tlsP_SHA512(Secret, ALabel + Seed, Size);
end;

function TLSPRF(const ProtocolVersion: TTLSProtocolVersion;
         const Secret, ALabel, Seed: RawByteString; const Size: Integer): RawByteString;
begin
  if IsTLS12OrLater(ProtocolVersion) then
    Result := tls12PRF_SHA256(Secret, ALabel, Seed, Size) else
  if IsTLS10OrLater(ProtocolVersion) then
    Result := tls10PRF(Secret, ALabel, Seed, Size)
  else
    raise ETLSError.Create(TLSError_InvalidParameter);
end;



{                                                                              }
{ Test                                                                         }
{                                                                              }
{$IFDEF TLS_TEST}
{$ASSERTIONS ON}
procedure Test;
begin
  //                                                                                   //
  // Test vectors from http://www6.ietf.org/mail-archive/web/tls/current/msg03416.html //
  //                                                                                   //
  Assert(tls12PRF_SHA256(
      RawByteString(#$9b#$be#$43#$6b#$a9#$40#$f0#$17#$b1#$76#$52#$84#$9a#$71#$db#$35),
      'test label',
      RawByteString(#$a0#$ba#$9f#$93#$6c#$da#$31#$18#$27#$a6#$f7#$96#$ff#$d5#$19#$8c), 100) =
      #$e3#$f2#$29#$ba#$72#$7b#$e1#$7b +
      #$8d#$12#$26#$20#$55#$7c#$d4#$53 +
      #$c2#$aa#$b2#$1d#$07#$c3#$d4#$95 +
      #$32#$9b#$52#$d4#$e6#$1e#$db#$5a +
      #$6b#$30#$17#$91#$e9#$0d#$35#$c9 +
      #$c9#$a4#$6b#$4e#$14#$ba#$f9#$af +
      #$0f#$a0#$22#$f7#$07#$7d#$ef#$17 +
      #$ab#$fd#$37#$97#$c0#$56#$4b#$ab +
      #$4f#$bc#$91#$66#$6e#$9d#$ef#$9b +
      #$97#$fc#$e3#$4f#$79#$67#$89#$ba +
      #$a4#$80#$82#$d1#$22#$ee#$42#$c5 +
      #$a7#$2e#$5a#$51#$10#$ff#$f7#$01 +
      #$87#$34#$7b#$66);
  Assert(tls12PRF_SHA512(
      RawByteString(#$b0#$32#$35#$23#$c1#$85#$35#$99#$58#$4d#$88#$56#$8b#$bb#$05#$eb),
      'test label',
      RawByteString(#$d4#$64#$0e#$12#$e4#$bc#$db#$fb#$43#$7f#$03#$e6#$ae#$41#$8e#$e5), 196) =
      #$12#$61#$f5#$88#$c7#$98#$c5#$c2 +
      #$01#$ff#$03#$6e#$7a#$9c#$b5#$ed +
      #$cd#$7f#$e3#$f9#$4c#$66#$9a#$12 +
      #$2a#$46#$38#$d7#$d5#$08#$b2#$83 +
      #$04#$2d#$f6#$78#$98#$75#$c7#$14 +
      #$7e#$90#$6d#$86#$8b#$c7#$5c#$45 +
      #$e2#$0e#$b4#$0c#$1c#$f4#$a1#$71 +
      #$3b#$27#$37#$1f#$68#$43#$25#$92 +
      #$f7#$dc#$8e#$a8#$ef#$22#$3e#$12 +
      #$ea#$85#$07#$84#$13#$11#$bf#$68 +
      #$65#$3d#$0c#$fc#$40#$56#$d8#$11 +
      #$f0#$25#$c4#$5d#$df#$a6#$e6#$fe +
      #$c7#$02#$f0#$54#$b4#$09#$d6#$f2 +
      #$8d#$d0#$a3#$23#$3e#$49#$8d#$a4 +
      #$1a#$3e#$75#$c5#$63#$0e#$ed#$be +
      #$22#$fe#$25#$4e#$33#$a1#$b0#$e9 +
      #$f6#$b9#$82#$66#$75#$be#$c7#$d0 +
      #$1a#$84#$56#$58#$dc#$9c#$39#$75 +
      #$45#$40#$1d#$40#$b9#$f4#$6c#$7a +
      #$40#$0e#$e1#$b8#$f8#$1c#$a0#$a6 +
      #$0d#$1a#$39#$7a#$10#$28#$bf#$f5 +
      #$d2#$ef#$50#$66#$12#$68#$42#$fb +
      #$8d#$a4#$19#$76#$32#$bd#$b5#$4f +
      #$f6#$63#$3f#$86#$bb#$c8#$36#$e6 +
      #$40#$d4#$d8#$98);
end;
{$ENDIF}



end.

