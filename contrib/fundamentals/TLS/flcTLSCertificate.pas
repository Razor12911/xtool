{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals TLS                                         }
{   File name:        flcTLSCertificate.pas                                    }
{   File version:     5.02                                                     }
{   Description:      TLS Certificate                                          }
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
{   2020/05/11  5.02  Create unit flcTLSCertificate from units                 }
{                     flcTLSHandshake and flcTLSClient.                        }
{                                                                              }
{******************************************************************************}

{$INCLUDE flcTLS.inc}

unit flcTLSCertificate;

interface

uses
  { Utils }

  flcX509Certificate,

  { Cipher }

  flcCipherRSA;



{                                                                              }
{ Certificate                                                                  }
{                                                                              }
type
  TTLSCertificateList = array of RawByteString;

procedure TLSCertificateListAppend(var List: TTLSCertificateList; const A: RawByteString);

function  EncodeTLSCertificate(
          var Buffer; const Size: Integer;
          const CertificateList: TTLSCertificateList): Integer;

function  DecodeTLSCertificate(
          const Buffer; const Size: Integer;
          var CertificateList: TTLSCertificateList): Integer;

procedure ParseX509Certificates(
          const CertificateList: TTLSCertificateList;
          var X509Certificates: TX509CertificateArray);

function  GetCertificateRSAPublicKey(
          const X509Certificates: TX509CertificateArray;
          var RSAPublicKey: TRSAPublicKey): Boolean;



implementation

uses
  { Utils }

  flcASN1,

  { TLS }

  flcTLSErrors,
  flcTLSOpaqueEncoding;



{                                                                              }
{ Certificate                                                                  }
{   certificate_list  : <0..2^24-1> ASN.1Cert;                                 }
{                                                                              }
{ ASN.1Cert = <1..2^24-1> opaque;                                              }
{                                                                              }
procedure TLSCertificateListAppend(var List: TTLSCertificateList; const A: RawByteString);
var L : Integer;
begin
  L := Length(List);
  SetLength(List, L + 1);
  List[L] := A;
end;

function EncodeTLSCertificate(
         var Buffer; const Size: Integer;
         const CertificateList: TTLSCertificateList): Integer;
var P : PByte;
    N, L, I, M, T : Integer;
    C : RawByteString;
begin
  Assert(Size >= 0);
  N := Size;
  P := @Buffer;
  // certificate_list
  L := Length(CertificateList);
  T := 0;
  for I := 0 to L - 1 do
    Inc(T, 3 + Length(CertificateList[I]));
  EncodeTLSLen24(P^, N, T);
  Dec(N, 3);
  Inc(P, 3);
  for I := 0 to L - 1 do
    begin
      // ASN.1Cert
      C := CertificateList[I];
      if C = '' then
        raise ETLSError.Create(TLSError_InvalidCertificate);
      M := EncodeTLSOpaque24(P^, N, C);
      Dec(N, M);
      Inc(P, M);
    end;

  Result := Size - N;
end;

function DecodeTLSCertificate(
         const Buffer; const Size: Integer;
         var CertificateList: TTLSCertificateList): Integer;
var P : PByte;
    N, L, M, F : Integer;
    C : RawByteString;
begin
  Assert(Size >= 0);
  N := Size;
  P := @Buffer;
  // certificate_list
  DecodeTLSLen24(P^, N, L);
  Dec(N, 3);
  Inc(P, 3);
  SetLength(CertificateList, 0);
  F := 0;
  while L > 0 do
    begin
      // ASN.1Cert
      M := DecodeTLSOpaque24(P^, N, C);
      Dec(N, M);
      Inc(P, M);
      Dec(L, M);
      Inc(F);
      SetLength(CertificateList, F);
      CertificateList[F - 1] := C;
    end;
  Result := Size - N;
end;

procedure ParseX509Certificates(
          const CertificateList: TTLSCertificateList;
          var X509Certificates: TX509CertificateArray);
var
  L : Integer;
  I : Integer;
  C : RawByteString;
begin
  L := Length(CertificateList);
  SetLength(X509Certificates, L);
  for I := 0 to L - 1 do
    begin
      C := CertificateList[I];
      InitX509Certificate(X509Certificates[I]);
      if C <> '' then
        try
          ParseX509Certificate(C[1], Length(C), X509Certificates[I])
        except
          raise ETLSError.Create(TLSError_InvalidCertificate);
        end;
    end;
end;

function GetCertificateRSAPublicKey(
         const X509Certificates: TX509CertificateArray;
         var RSAPublicKey: TRSAPublicKey): Boolean;
var
  I, L, N1, N2 : Integer;
  C : PX509Certificate;
  S : RawByteString;
  PKR : TX509RSAPublicKey;
  R : Boolean;
begin
  // find RSA public key from certificates
  R := False;
  L := Length(X509Certificates);
  for I := 0 to L - 1 do
    begin
      C := @X509Certificates[I];
      if ASN1OIDEqual(C^.TBSCertificate.SubjectPublicKeyInfo.Algorithm.Algorithm, OID_RSA) then
        begin
          S := C^.TBSCertificate.SubjectPublicKeyInfo.SubjectPublicKey;
          Assert(S <> '');
          ParseX509RSAPublicKey(S[1], Length(S), PKR);
          R := True;
          break;
        end;
    end;
  if not R then
    begin
      Result := False;
      exit;
    end;
  N1 := NormaliseX509IntKeyBuf(PKR.Modulus);
  N2 := NormaliseX509IntKeyBuf(PKR.PublicExponent);
  if N2 > N1 then
    N1 := N2;
  // initialise RSA public key
  RSAPublicKeyAssignBuf(RSAPublicKey, N1 * 8,
      PKR.Modulus[1], Length(PKR.Modulus),
      PKR.PublicExponent[1], Length(PKR.PublicExponent), True);
  Result := True;
end;




end.

