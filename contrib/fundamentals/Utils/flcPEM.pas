{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals 5.00                                        }
{   File name:        flcPEM.pas                                               }
{   File version:     5.03                                                     }
{   Description:      PEM file parsing                                         }
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
{ Revision history:                                                            }
{                                                                              }
{   2011/10/18  0.01  Initial development.                                     }
{   2016/01/10  0.02  String changes.                                          }
{   2018/07/17  5.03  Revised for Fundamentals 5.                              }
{                                                                              }
{******************************************************************************}

{$INCLUDE ..\flcInclude.inc}

{$IFDEF DEBUG}
{$IFDEF TEST}
  {$DEFINE PEM_TEST}
{$ENDIF}
{$ENDIF}

unit flcPEM;

interface

uses
  { System }

  SysUtils,

  { Fundamentals }

  flcStdTypes;



{ TPEMFile }

type
  TPEMFile = class
  private
    FCertificates  : array of RawByteString;
    FRSAPrivateKey : RawByteString;

    procedure Clear;
    procedure AddCertificate(const CertificatePEM: RawByteString);
    procedure SetRSAPrivateKey(const RSAPrivateKeyPEM: RawByteString);
    procedure ParsePEMContent(const Content: RawByteString);

    function  GetCertificateCount: Integer;
    function  GetCertificate(const Idx: Integer): RawByteString;

  public
    procedure LoadFromText(const Txt: RawByteString);
    procedure LoadFromFile(const FileName: String);

    property  CertificateCount: Integer read GetCertificateCount;
    property  Certificate[const Idx: Integer]: RawByteString read GetCertificate;
    property  RSAPrivateKey: RawByteString read FRSAPrivateKey;
  end;

  EPEMFile = class(Exception);



{$IFDEF PEM_TEST}
procedure Test;
{$ENDIF}



implementation

uses
  { System }

  Classes,

  { Fundamentals }

  flcStrings,
  flcBase64;



{ TPEMFile }

procedure TPEMFile.Clear;
begin
  FCertificates := nil;
  FRSAPrivateKey := '';
end;

procedure TPEMFile.AddCertificate(const CertificatePEM: RawByteString);
var
  L : Integer;
  C : RawByteString;
begin
  C := MIMEBase64Decode(CertificatePEM);
  L := Length(FCertificates);
  SetLength(FCertificates, L + 1);
  FCertificates[L] := C;
end;

procedure TPEMFile.SetRSAPrivateKey(const RSAPrivateKeyPEM: RawByteString);
begin
  FRSAPrivateKey := MIMEBase64Decode(RSAPrivateKeyPEM);
end;

procedure TPEMFile.ParsePEMContent(const Content: RawByteString);
var
  S : RawByteString;

  function GetTextBetween(const Start, Stop: RawByteString; var Between: RawByteString): Boolean;
  var I, J : Integer;
  begin
    I := PosStrB(Start, S, 1, False);
    if I > 0 then
      begin
        J := PosStrB(Stop, S, 1, False);
        if J = 0 then
          J := Length(S) + 1;
        Between := CopyRangeB(S, I + Length(Start), J - 1);
        Delete(S, I, J + Length(Stop) - I);
        Between := StrRemoveCharSetB(Between, [#0..#32]);
        Result := True;
      end
    else
      Result := False;
  end;

var
  Found : Boolean;
  Cert : RawByteString;
  RSAPriv : RawByteString;

begin
  S := Content;
  repeat
    Found := GetTextBetween('-----BEGIN CERTIFICATE-----', '-----END CERTIFICATE-----', Cert);
    if Found then
      AddCertificate(Cert);
  until not Found;
  Found := GetTextBetween('-----BEGIN RSA PRIVATE KEY-----', '-----END RSA PRIVATE KEY-----', RSAPriv);
  if Found then
    SetRSAPrivateKey(RSAPriv);
end;

procedure TPEMFile.LoadFromText(const Txt: RawByteString);
begin
  Clear;
  ParsePEMContent(Txt);
end;

procedure TPEMFile.LoadFromFile(const FileName: String);
var
  F : TFileStream;
  B : RawByteString;
  L : Int64;
  N : Integer;
begin
  try
    F := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    try
      L := F.Size;
      if L > 16 * 1024 * 1024 then
        raise EPEMFile.Create('File too large');
      N := L;
      if N = 0 then
        B := ''
      else
        begin
          SetLength(B, N);
          F.ReadBuffer(B[1], N);
        end;
    finally
      F.Free;
    end;
  except
    on E : Exception do
      raise EPEMFile.CreateFmt('Error loading PEM file: %s: %s', [E.ClassName, E.Message]);
  end;
  LoadFromText(B);
end;

function TPEMFile.GetCertificateCount: Integer;
begin
  Result := Length(FCertificates);
end;

function TPEMFile.GetCertificate(const Idx: Integer): RawByteString;
begin
  Result := FCertificates[Idx];
end;



{$IFDEF PEM_TEST}
{$ASSERTIONS ON}
const
  TestPEMText1 = // stunnel.pem
    '-----BEGIN RSA PRIVATE KEY-----'#13#10 +
    'MIICXAIBAAKBgQCxUFMuqJJbI9KnB8VtwSbcvwNOltWBtWyaSmp7yEnqwWel5TFf'#13#10 +
    'cOObCuLZ69sFi1ELi5C91qRaDMow7k5Gj05DZtLDFfICD0W1S+n2Kql2o8f2RSvZ'#13#10 +
    'qD2W9l8i59XbCz1oS4l9S09L+3RTZV9oer/Unby/QmicFLNM0WgrVNiKywIDAQAB'#13#10 +
    'AoGAKX4KeRipZvpzCPMgmBZi6bUpKPLS849o4pIXaO/tnCm1/3QqoZLhMB7UBvrS'#13#10 +
    'PfHj/Tejn0jjHM9xYRHi71AJmAgzI+gcN1XQpHiW6kATNDz1r3yftpjwvLhuOcp9'#13#10 +
    'tAOblojtImV8KrAlVH/21rTYQI+Q0m9qnWKKCoUsX9Yu8UECQQDlbHL38rqBvIMk'#13#10 +
    'zK2wWJAbRvVf4Fs47qUSef9pOo+p7jrrtaTqd99irNbVRe8EWKbSnAod/B04d+cQ'#13#10 +
    'ci8W+nVtAkEAxdqPOnCISW4MeS+qHSVtaGv2kwvfxqfsQw+zkwwHYqa+ueg4wHtG'#13#10 +
    '/9+UgxcXyCXrj0ciYCqURkYhQoPbWP82FwJAWWkjgTgqsYcLQRs3kaNiPg8wb7Yb'#13#10 +
    'NxviX0oGXTdCaAJ9GgGHjQ08lNMxQprnpLT8BtZjJv5rUOeBuKoXagggHQJAaUAF'#13#10 +
    '91GLvnwzWHg5p32UgPsF1V14siX8MgR1Q6EfgKQxS5Y0Mnih4VXfnAi51vgNIk/2'#13#10 +
    'AnBEJkoCQW8BTYueCwJBALvz2JkaUfCJc18E7jCP7qLY4+6qqsq+wr0t18+ogOM9'#13#10 +
    'JIY9r6e1qwNxQ/j1Mud6gn6cRrObpRtEad5z2FtcnwY='#13#10 +
    '-----END RSA PRIVATE KEY-----'#13#10 +
    '-----BEGIN CERTIFICATE-----'#13#10 +
    'MIICDzCCAXigAwIBAgIBADANBgkqhkiG9w0BAQQFADBCMQswCQYDVQQGEwJQTDEf'#13#10 +
    'MB0GA1UEChMWU3R1bm5lbCBEZXZlbG9wZXJzIEx0ZDESMBAGA1UEAxMJbG9jYWxo'#13#10 +
    'b3N0MB4XDTk5MDQwODE1MDkwOFoXDTAwMDQwNzE1MDkwOFowQjELMAkGA1UEBhMC'#13#10 +
    'UEwxHzAdBgNVBAoTFlN0dW5uZWwgRGV2ZWxvcGVycyBMdGQxEjAQBgNVBAMTCWxv'#13#10 +
    'Y2FsaG9zdDCBnzANBgkqhkiG9w0BAQEFAAOBjQAwgYkCgYEAsVBTLqiSWyPSpwfF'#13#10 +
    'bcEm3L8DTpbVgbVsmkpqe8hJ6sFnpeUxX3Djmwri2evbBYtRC4uQvdakWgzKMO5O'#13#10 +
    'Ro9OQ2bSwxXyAg9FtUvp9iqpdqPH9kUr2ag9lvZfIufV2ws9aEuJfUtPS/t0U2Vf'#13#10 +
    'aHq/1J28v0JonBSzTNFoK1TYissCAwEAAaMVMBMwEQYJYIZIAYb4QgEBBAQDAgZA'#13#10 +
    'MA0GCSqGSIb3DQEBBAUAA4GBAAhYFTngWc3tuMjVFhS4HbfFF/vlOgTu44/rv2F+'#13#10 +
    'ya1mEB93htfNxx3ofRxcjCdorqONZFwEba6xZ8/UujYfVmIGCBy4X8+aXd83TJ9A'#13#10 +
    'eSjTzV9UayOoGtmg8Dv2aj/5iabNeK1Qf35ouvlcTezVZt2ZeJRhqUHcGaE+apCN'#13#10 +
    'TC9Y'#13#10 +
    '-----END CERTIFICATE-----'#13#10;

  TestPEMText2 =
    '-----BEGIN CERTIFICATE-----' +
    'MIIDQjCCAiqgAwIBAgIJAKDslQh3d8kdMA0GCSqGSIb3DQEBBQUAMB8xHTAbBgNV' +
    'BAMTFHd3dy5ldGVybmFsbGluZXMuY29tMB4XDTExMTAxODEwMzYwOVoXDTIxMTAx' +
    'NTEwMzYwOVowHzEdMBsGA1UEAxMUd3d3LmV0ZXJuYWxsaW5lcy5jb20wggEiMA0G' +
    'CSqGSIb3DQEBAQUAA4IBDwAwggEKAoIBAQCw/7d6zyehR69DaJCGbk3oMP7pSWya' +
    'U1tDMG+CdqikLqHoo3SBshbvquOVFcy9yY8fECTbNXfOjhV0M6SJgGQ/SP/nfZgx' +
    'MHAK9sWc5G6V5sqPqrTRgkv0Wu25mdO6FRh8DIxOMY0Ppqno5hHZ0emSj1amvtWX' +
    'zBD6pXNGgrFln6HL2eyCwqlL0wTXWO/YrvblF/83Ln9i6luVQ9NtACQBiPcYqoNM' +
    '1OG142xYNpRNp7zrHkNCQeXVxmC6goCgj0BmcSqrUPayLdgkgv8hniUwLYQIt91r' +
    'cxJwGNWxlbLgqQqTdhecKp01JVgO8jy3yFpMEoqCj9+BuuxVqDfvHK1tAgMBAAGj' +
    'gYAwfjAdBgNVHQ4EFgQUbLgD+S3ZSNlU1nxTsjTmAQIfpCQwTwYDVR0jBEgwRoAU' +
    'bLgD+S3ZSNlU1nxTsjTmAQIfpCShI6QhMB8xHTAbBgNVBAMTFHd3dy5ldGVybmFs' +
    'bGluZXMuY29tggkAoOyVCHd3yR0wDAYDVR0TBAUwAwEB/zANBgkqhkiG9w0BAQUF' +
    'AAOCAQEACSQTcPC8ga5C/PysnoTNAk4OB+hdgMoS3Fv7ROUV9GqgYED6rJo0+CxD' +
    'g19GLlKt/aBlglh4Ddc7X84dWtftS4JIjjVVkWevt8/sDoZ+ISd/tC9aDX3gOAlW' +
    'RORhfp3Qtyy0AjZcIOAGNkzkotuMG/uOVifPFhTNXwa8hHOGN60riGXEj5sNFFop' +
    'EaxplTfakVq8TxlQivnIETjrEbVX8XkOl4nlsHevC2suXE1ZkQIbQoaAy0WzGGUR' +
    '54GBIzXf32t80S71w5rs/mzVaGOeTZYcHtv5Epd9CNVrEle6w0NW9R7Ov4gXI9n8' +
    'GV9jITGfsOdqu7j9Iaf7MVj+JRE7Dw==' +
    '-----END CERTIFICATE-----' +
    '-----BEGIN RSA PRIVATE KEY-----' +
    'MIIEpQIBAAKCAQEAsP+3es8noUevQ2iQhm5N6DD+6UlsmlNbQzBvgnaopC6h6KN0' +
    'gbIW76rjlRXMvcmPHxAk2zV3zo4VdDOkiYBkP0j/532YMTBwCvbFnORulebKj6q0' +
    '0YJL9FrtuZnTuhUYfAyMTjGND6ap6OYR2dHpko9Wpr7Vl8wQ+qVzRoKxZZ+hy9ns' +
    'gsKpS9ME11jv2K725Rf/Ny5/YupblUPTbQAkAYj3GKqDTNThteNsWDaUTae86x5D' +
    'QkHl1cZguoKAoI9AZnEqq1D2si3YJIL/IZ4lMC2ECLfda3MScBjVsZWy4KkKk3YX' +
    'nCqdNSVYDvI8t8haTBKKgo/fgbrsVag37xytbQIDAQABAoIBAQCdnZnOCtrHjAZO' +
    'iLbqfx9xPPBC3deQNdp3IpKqIvBaBAy6FZSSSfySwCKZiCgieXKxvraTXjGqBmyk' +
    'ZbiHmYWrtV3szrLQWsnreYTQCbtQUYzgEquiRd1NZAt907XvZwm+rY3js8xhu5Bi' +
    'jT4oMf1FPc9z/UxHOLmF+f+FMqy2SM2Fxh3jAsxJBaMVEJXpqdQDI86CATgYrqVY' +
    'mlAWQcQ8pL0wwRctZ+XgjQH52V3sk4cIzqIBTO+MN6emmxDl9JdrGZKRei9YEIhG' +
    'mFeXH7rsGg+TZtfvu1M9Kfy2fdgNwTUoTTn93v8gcrwCbyvl5JCzKy07Om/aOXFr' +
    'I8bSWXIhAoGBANu07hegU99zIhvTWmh2Fuml0Lr+cHcZTObh+oeZg1xaDUrlnFOY' +
    '3fyA5x5Jxib3V7OOAeIz/AsmcYq/649nR8NfeiizY5or84Fy1mazRR8diGDV3nUG' +
    'ZATv6yaOY/z31FOLaxT95tDvqWK+Qr5cykq4e6XDDp9P8odCIjJmUdt7AoGBAM48' +
    'vCjtGQ99BVwkcFIj0IacRj3YKzsp06W6V2Z+czlKctJAMAQN8hu9IcXMEIUsi9GD' +
    'MkyzzxjvGRdmIuS58IFqRbr/fIAQLVpY9SPAL771ZCFHmIrKrCYiLYAcg/BSoR29' +
    'me6aFaEcLBFvzHPFNymdyMsaOHSRMZYUlq6VUbI3AoGBAINJeMURf00VRZqPD4VA' +
    'm6x+813qUVY5/iQxgT2qVD7JaQwKbQHfZTdP58vHlesO/o9DGokLO1+GV27sBF0r' +
    'AE0VLrBHkgs8nEQMVWYFVhaj1SzYYBhZ+0af/0qI5+LwTSanNxPSLS1JKVTiEIwk' +
    'cpV37Bs/letJIMoGkNzBG8UlAoGBAKrSfZt8f3RnvmfKusoeZhsJF9kj0vMHOwob' +
    'ZUc8152Nf7uMdPj2wCGfr3iRBOH5urnH7ILBsHjbmjHaZG6FYKMg7i7sbSf5vkcG' +
    'Rc3d4u5NfSlfjwbuxlYzmvJxLAuDtXXX1MdgEyhGGG485uDBamZrDaTEzBwpIyRH' +
    'W2OxxGBTAoGAZHJQKTajcqQQoRSgPPWWU3X8zdlu5hCgNU54bXaPAfJ6IBWvicMZ' +
    'QLw+9mtshtz+Xy0aBbkxUeUlwwzexb9rg1KZppTq/yRqkOlEkI3ZdqiclTK13BCh' +
    '6r6dC2qqq+DVm9Nlm/S9Gab9YSIA0g5MFg5WLwu1KNwuOODE4Le/91c=' +
    '-----END RSA PRIVATE KEY-----';

procedure Test;
var P : TPEMFile;
begin
  P := TPEMFile.Create;
  try
    P.LoadFromText(TestPEMText1);
    P.LoadFromText(TestPEMText2);
  finally
    P.Free;
  end;
end;
{$ENDIF}



end.

