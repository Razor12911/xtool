{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals TLS                                         }
{   File name:        flcTLSTestCertificates.pas                               }
{   File version:     5.05                                                     }
{   Description:      TLS Test Certificates                                    }
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
{   2020/05/11  5.01  Initial certificates: RSA768, RSA2048, ECDSA-Secp256k1,  }
{                     DSA512, DSA2048 and RSA-STunnel.                         }
{                                                                              }
{******************************************************************************}

{$INCLUDE flcTLS.inc}

unit flcTLSTestCertificates;

interface



{$IFDEF TLS_TEST}
const
  // rsa768-example.com-1
  RSA768_ExampleCom_1_PrivateKeyRSAPEM =
    'MIIB5QIBADANBgkqhkiG9w0BAQEFAASCAc8wggHLAgEAAmEAoO3RSQnutJfvT56L' +
    '85saXYZQi5zCzt5YUb75z3T/WsN1oaWt7NR1Bd1+xBxdmHOSRfSm0CsBw/gkt9YI' +
    'Vyjl6IbclBbhgIGzS3bkoVCvbG6SKtxGJPJcf9BOV8J9DqqXAgMBAAECYQCJz14b' +
    'h+/soveCXSlH8ZjAYlbzV8jTUkCbsElIyM4rsZo4VSL93mpgHW+DDS9xb/WFPtdk' +
    'CadzB+mnK8/Hul0OteOFFT5gn9vRzlFPS6WzPTeoeaKtqrwX8RyU+xLgREECMQDQ' +
    '+yAK8LF/v9yJ55A7Tfcq9XDr+eEjZ2DkaytvRX4HlMV9uCccCkISxxdBIbZMeIcC' +
    'MQDFIv8ikcJ5VjYaEr83Jof5bcH7qHeHV1WY2017cV/7ISVxwPL7rACKer+RWhnv' +
    'kXECMHzYiZv/jwqypB3+qLvFKBQR7RQMg+OSrt/G5nvjGBePWSxyB2tI9ZAiQFI4' +
    'wZ+NoQIwBhaWmpK11tl6wkNh9GoUOPfSzdreFif0VMwxEGbn9/GGHoU++9bMDXrM' +
    '/8gwlN2BAjEAsBKVdwN9HHE6uu8YoSxudy5Tcwa+bj8yxLiS6EwRs5WwzvaLv88p' +
    'ES7nd5PO2txL';

  RSA768_ExampleCom_1_CertificatePEM =
    'MIICtzCCAkGgAwIBAgIUZQggmdgFQ6iq5DQGqcgDcL7jj4EwDQYJKoZIhvcNAQEL' +
    'BQAwgY8xCzAJBgNVBAYTAkFVMRMwEQYDVQQIDApTb21lLVN0YXRlMREwDwYDVQQH' +
    'DAhMb2NhbGl0eTEhMB8GA1UECgwYSW50ZXJuZXQgV2lkZ2l0cyBQdHkgTHRkMRQw' +
    'EgYDVQQDDAtleGFtcGxlLmNvbTEfMB0GCSqGSIb3DQEJARYQaW5mb0BleGFtcGxl' +
    'LmNvbTAeFw0yMDA1MTExODI5NTlaFw0zMDA1MDkxODI5NTlaMIGPMQswCQYDVQQG' +
    'EwJBVTETMBEGA1UECAwKU29tZS1TdGF0ZTERMA8GA1UEBwwITG9jYWxpdHkxITAf' +
    'BgNVBAoMGEludGVybmV0IFdpZGdpdHMgUHR5IEx0ZDEUMBIGA1UEAwwLZXhhbXBs' +
    'ZS5jb20xHzAdBgkqhkiG9w0BCQEWEGluZm9AZXhhbXBsZS5jb20wfDANBgkqhkiG' +
    '9w0BAQEFAANrADBoAmEAoO3RSQnutJfvT56L85saXYZQi5zCzt5YUb75z3T/WsN1' +
    'oaWt7NR1Bd1+xBxdmHOSRfSm0CsBw/gkt9YIVyjl6IbclBbhgIGzS3bkoVCvbG6S' +
    'KtxGJPJcf9BOV8J9DqqXAgMBAAGjUzBRMB0GA1UdDgQWBBRKImmf1879CQy/aFl/' +
    'miDQp00BJDAfBgNVHSMEGDAWgBRKImmf1879CQy/aFl/miDQp00BJDAPBgNVHRMB' +
    'Af8EBTADAQH/MA0GCSqGSIb3DQEBCwUAA2EAZ2mFxf4DNHrmVMbvyGVXzzPmmGoz' +
    'vNdPeSQH16cgl1Q/Ass+52pSLjeIgiSpJcQtG5YzZY/t1m8LPcGqASYg9azce8fo' +
    'b34G260PvXX4FUrOSBZ2Owx59R5Cl1r238oj';



  // rsa2048-example.com-1
  RSA2048_ExampleCom_1_PrivateKeyRSAPEM =
    'MIIEvQIBADANBgkqhkiG9w0BAQEFAASCBKcwggSjAgEAAoIBAQDe6cFTj4ciladG' +
    'pHE9r0ytDfmWKVnnJZYLR2DLPVg6LVkEeykusfUSpf4pYwKOuToLD24iZpK5x3CY' +
    'UR348OXkFT2ytQwIcyRhK91N5x06WbmtCSl2Fx0GVBaKwfgx73LNfwtGvcOQVOQy' +
    'hozHm0TkhBHVplJ57hD3YGTH98WSiRP0fS1yV595wP72nYAvPAp61bVYpRNJDjFZ' +
    'QESubigYB5ufLUfGntlOasNd1CuICFFkSpRi1cd/r6qvq4ehhH7ypGTBjbPQ3Zg9' +
    'vpjR5B3NCKtH2n90fKy+Fuxfcm73GUinjR1C66Ve0CHovoskEROCcbCoi1VT2+oy' +
    '+cqOExfFAgMBAAECggEBANzZNyK0lqwbHOmOTmtQ3GSv7dFqEppB0NBH3Yw+sMSi' +
    '3QjlhL2wrh/VuWQDpisFNI50sSb//Op2wAUIiOt0sC8zJDeDy/IrMaXcMZvXGEwR' +
    'TTY0V5GaALWeZd7/ogjHNTSHZAKoS7MZiCTOzXeNS8ojVxAXgqsuxDxykibUQjiU' +
    'IVdKTN5KVANu1iL6FX9cXjbTncYmvMdtVqLccs4uFaazhWw756or1rk2rQQIEdXX' +
    'HisZ0iQeOmZILXy8HZI3EH/Vz8CDC9T3xJG76PiDP6I5wbM2WcOLAZcop+hWi/+U' +
    'KtTlIMFLYHSOwKjJmc4xkLRKRtoHUUtofAHSvh7CgZkCgYEA9dhhtfCskvw6mZcx' +
    'BuPNoN6Ge9+m56Z2dhSRVs5mzNSDjuVa+HFXAAramrJ6avc7fDm8S8j/AD9b2XnW' +
    'x4z4SFn9tTZyuUPEPAQAmwxFS2QHUA1fxDDvDh407iiSzXBO9ehke3b65YM6zjPl' +
    'WK72OG11dbFfctD4MIjK/0kFo9sCgYEA6B7iZkI/uiLQ0fqz07oOasvsGjtFja6F' +
    '2uNVZkZo0iYnXzdWGWeLlLQ3bme4kylpIyj2hh2EHQzrjJ6NCgU/W/QvDMFuyJbQ' +
    'wtUhWEFjlgqgjigRGDm3kMj0+hYbnS5g3rVpmfDsKT8fwCoMXwDCEgMpDe5CnwJf' +
    'ucgm/Njo1N8CgYAIoLttIzErR2bXFRNHZp9E0gpuNn8pChKGOlqPbVb2QU8MqMf0' +
    'iCXBfqAFZdYeAuc3iN8u2bL5Uz/p9fivsCbWgzIANhT4o4QzhwBucJPN/Yi0KoP9' +
    '4qnBGRZKdWoRg6uBvdIo8xgDDgP2UKPv5NQHTvAcXUk4QlUzftmA9BMamQKBgDFa' +
    'D6zKPR5oNJnQgddsYZBXVxWksH8VMiR93TRnl/XGYuydqVKxbz3oqzhwGRBA57ew' +
    'B+ov8Fz02EgHldkhkH0Oh8pgfhtr5WrnQbWwAWpvS/+tiSTrcJn6AAwEE07yA2qW' +
    'i6NNVAjZAPksd4Djel+2CE6L7+I68PthENkFjUtlAoGAX+TvpFV9b7O+HxpKtljz' +
    'DrCoN54myazIAbkEkQTHoCQcqdnvS4pnCTi+j7Gcvncbw1DHqCUadV4MG+9BNLjp' +
    'ZuKNEqVcjdKyVC1ZO8o58/PC/jKf46BZf0KPZ2njYN/HhTsYKW+cS8BqQ03z05Vx' +
    'mIRN6htJ2rDNYLnPKuFxW1s=';

  RSA2048_ExampleCom_1_CertificatePEM =
    'MIID2TCCAsGgAwIBAgIUDjajQu/92DMG5roEQ0G4WkQ49pQwDQYJKoZIhvcNAQEL' +
    'BQAwfDELMAkGA1UEBhMCQVUxEzARBgNVBAgMClNvbWUtU3RhdGUxITAfBgNVBAoM' +
    'GEludGVybmV0IFdpZGdpdHMgUHR5IEx0ZDEUMBIGA1UEAwwLZXhhbXBsZS5jb20x' +
    'HzAdBgkqhkiG9w0BCQEWEGluZm9AZXhhbXBsZS5jb20wHhcNMjAwNTExMTgyNzE2' +
    'WhcNMzAwNTA5MTgyNzE2WjB8MQswCQYDVQQGEwJBVTETMBEGA1UECAwKU29tZS1T' +
    'dGF0ZTEhMB8GA1UECgwYSW50ZXJuZXQgV2lkZ2l0cyBQdHkgTHRkMRQwEgYDVQQD' +
    'DAtleGFtcGxlLmNvbTEfMB0GCSqGSIb3DQEJARYQaW5mb0BleGFtcGxlLmNvbTCC' +
    'ASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBAN7pwVOPhyKVp0akcT2vTK0N' +
    '+ZYpWecllgtHYMs9WDotWQR7KS6x9RKl/iljAo65OgsPbiJmkrnHcJhRHfjw5eQV' +
    'PbK1DAhzJGEr3U3nHTpZua0JKXYXHQZUForB+DHvcs1/C0a9w5BU5DKGjMebROSE' +
    'EdWmUnnuEPdgZMf3xZKJE/R9LXJXn3nA/vadgC88CnrVtVilE0kOMVlARK5uKBgH' +
    'm58tR8ae2U5qw13UK4gIUWRKlGLVx3+vqq+rh6GEfvKkZMGNs9DdmD2+mNHkHc0I' +
    'q0faf3R8rL4W7F9ybvcZSKeNHULrpV7QIei+iyQRE4JxsKiLVVPb6jL5yo4TF8UC' +
    'AwEAAaNTMFEwHQYDVR0OBBYEFBPFygrc+d52rGccjTlh0lRXhn7VMB8GA1UdIwQY' +
    'MBaAFBPFygrc+d52rGccjTlh0lRXhn7VMA8GA1UdEwEB/wQFMAMBAf8wDQYJKoZI' +
    'hvcNAQELBQADggEBAJqY4EARd0fHRYTtQA7antfIGSSDYH0S5E3mVSQEKKNR029h' +
    'XJNUKi93kheAwMAQlgKqFWWIPRGvEg8rVux3T6tSm14v7aEYmC6z/XdidYo1hA6F' +
    'f4hAC2eBkhzEcql1FAbt+s1crBmLkfRWxeI8E7/xYmFYP99QJdv+BkxD48Rf+bE3' +
    'hRIq2X2yOq1oC1zGR75fWcL7+6418bwYQfeOrn4bSHuWiOq19kELlzUQbSmjPnmF' +
    'zSErKEU8hezaVX5Z6mwYgV1P4Y4CPHaoK/Tl55Lr0KSLEc2U5Ff3SBKvkgkW968q' +
    'JYTt4/thw9MVT9/AO63iAIiYF79e8C5ZTYa6J/s=';



  // ecdsa-secp256k1-example.com-1
  ECDSA_Secp256k1_ExampleCom_1_PrivateKeyPEM =
    'MIGEAgEAMBAGByqGSM49AgEGBSuBBAAKBG0wawIBAQQgH1iT7iOnhczYKDnUnd0T' +
    '01m31q2wwDouonax6KF04r2hRANCAATQcPezFewJYlOYPDSYuy46Bu9SAfU8qo7a' +
    'OYCFgN/WmiFckB/oi+sEqnYxsgPMN77+c9EjSjvl1zKCQhEP4nf1';

  ECDSA_Secp256k1_ExampleCom_1_CertificatePEM =
    'MIICSzCCAfCgAwIBAgIUMknQUnv2p2k+F2ATArxlu9STJuwwCgYIKoZIzj0EAwIw' +
    'fDELMAkGA1UEBhMCQVUxEzARBgNVBAgMClNvbWUtU3RhdGUxITAfBgNVBAoMGElu' +
    'dGVybmV0IFdpZGdpdHMgUHR5IEx0ZDEUMBIGA1UEAwwLZXhhbXBsZS5jb20xHzAd' +
    'BgkqhkiG9w0BCQEWEGluZm9AZXhhbXBsZS5jb20wHhcNMjAwNTExMTg0MTE4WhcN' +
    'MzAwNTA5MTg0MTE4WjB8MQswCQYDVQQGEwJBVTETMBEGA1UECAwKU29tZS1TdGF0' +
    'ZTEhMB8GA1UECgwYSW50ZXJuZXQgV2lkZ2l0cyBQdHkgTHRkMRQwEgYDVQQDDAtl' +
    'eGFtcGxlLmNvbTEfMB0GCSqGSIb3DQEJARYQaW5mb0BleGFtcGxlLmNvbTBWMBAG' +
    'ByqGSM49AgEGBSuBBAAKA0IABNBw97MV7AliU5g8NJi7LjoG71IB9Tyqjto5gIWA' +
    '39aaIVyQH+iL6wSqdjGyA8w3vv5z0SNKO+XXMoJCEQ/id/WjUzBRMB0GA1UdDgQW' +
    'BBRa68KZXd4PMT2ujfRcGelXVYT5RTAfBgNVHSMEGDAWgBRa68KZXd4PMT2ujfRc' +
    'GelXVYT5RTAPBgNVHRMBAf8EBTADAQH/MAoGCCqGSM49BAMCA0kAMEYCIQDjOYWa' +
    'geEf616bxFlOq6GTqzHNdAsoYcllkvdTH7oGrAIhAMx/D4pikimc/8PKt2RROqU2' +
    '1pYBYY4q+WNHi/FkP1Rb';



  // dsa-512-example.com-1
  DSA512_ExampleCom_1_DSAParamsPEM =
    'MIGdAkEAy7pVdOq2xqDKhJVOK3yjEcBdngaCsng6nahCzR/YkOY1OS+Eb4ED+vv7' +
    'bjWxjZzQxknRzcgMqAwLEabjoxNIGwIVAOveBUI43583wCYz0nUtdEtRAaEpAkEA' +
    'uDkymWK9Bcy+YF+x4+xOyM1XGvyXb43TjkvwH+lTrH8jyeNpOfID07cW7xIBR/pz' +
    'r7sf6aFr0VqBWli+T+5KZQ==';

  DSA512_ExampleCom_1_PrivateKeyPEM =
    'MIH5AgEAAkEAy7pVdOq2xqDKhJVOK3yjEcBdngaCsng6nahCzR/YkOY1OS+Eb4ED' +
    '+vv7bjWxjZzQxknRzcgMqAwLEabjoxNIGwIVAOveBUI43583wCYz0nUtdEtRAaEp' +
    'AkEAuDkymWK9Bcy+YF+x4+xOyM1XGvyXb43TjkvwH+lTrH8jyeNpOfID07cW7xIB' +
    'R/pzr7sf6aFr0VqBWli+T+5KZQJBAIVwoK42Ydy2m7mtij2NLZ0Bom8l65+XQ62+' +
    'BmV2+2SPZaLOP3R+6p3voWbimxLr1dHqnaPLxcoNvtLk0QVsAqECFETKnDMulFrC' +
    'pWeRddos5U5WJNzC';

  DSA512_ExampleCom_1_CertificatePEM =
    'MIIC0TCCAo6gAwIBAgIUfVSUaT4wpDU2h5Czdf+zJPqtVQYwCwYJYIZIAWUDBAMC' +
    'MHwxCzAJBgNVBAYTAkFVMRMwEQYDVQQIDApTb21lLVN0YXRlMSEwHwYDVQQKDBhJ' +
    'bnRlcm5ldCBXaWRnaXRzIFB0eSBMdGQxFDASBgNVBAMMC2V4YW1wbGUuY29tMR8w' +
    'HQYJKoZIhvcNAQkBFhBpbmZvQGV4YW1wbGUuY29tMB4XDTIwMDUxMTE5NTEyOFoX' +
    'DTMwMDUwOTE5NTEyOFowfDELMAkGA1UEBhMCQVUxEzARBgNVBAgMClNvbWUtU3Rh' +
    'dGUxITAfBgNVBAoMGEludGVybmV0IFdpZGdpdHMgUHR5IEx0ZDEUMBIGA1UEAwwL' +
    'ZXhhbXBsZS5jb20xHzAdBgkqhkiG9w0BCQEWEGluZm9AZXhhbXBsZS5jb20wgfIw' +
    'gakGByqGSM44BAEwgZ0CQQDLulV06rbGoMqElU4rfKMRwF2eBoKyeDqdqELNH9iQ' +
    '5jU5L4RvgQP6+/tuNbGNnNDGSdHNyAyoDAsRpuOjE0gbAhUA694FQjjfnzfAJjPS' +
    'dS10S1EBoSkCQQC4OTKZYr0FzL5gX7Hj7E7IzVca/JdvjdOOS/Af6VOsfyPJ42k5' +
    '8gPTtxbvEgFH+nOvux/poWvRWoFaWL5P7kplA0QAAkEAvsRAbx3gJSKdOAmD6922' +
    'L/akoJiyT6hkz/9/D+oHvfdD6C5djg9E7ldBxu2y4G0RT6hrBgucnaY5EKRQ4Qa3' +
    'aqNTMFEwHQYDVR0OBBYEFN8uO0Y+73h43EIYn10VZvHY28/iMB8GA1UdIwQYMBaA' +
    'FN8uO0Y+73h43EIYn10VZvHY28/iMA8GA1UdEwEB/wQFMAMBAf8wCwYJYIZIAWUD' +
    'BAMCAzAAMC0CFQDJuGO19W778E8nVcowcu6INt4++gIUURUMEWm/dRO+4ti8GoHD' +
    'X4xdmkI=';



  // dsa-2048-example.com-1
  DSA2048_ExampleCom_1_DSAParamsPEM =
    'MIICLAKCAQEA0SWakCcmMvfxKFGQZkRCYDvaCIVu1PVuDTzclYdvWMLRajxpO3jK' +
    'paekYV9kh/beE6aRtyhEvNbEo9YmWPM8PSYwN5OBp8mW+gxaO7yrr/SVNXrF+xsK' +
    'dXle7UE4gP5JXtY4Tnl0u0BBGyrhl5rnBKfxJllhC4c+YEoEb+IWuJcal9Qhv1S7' +
    'dxVqB7KJ1PG8IOe4ainWjM7bnqjoHd5JpqfNami9CKKoV5EAdiQcVPEVcf1uKU+F' +
    'E6TzAqep60MTdi6K7l+xfTHKt15aZlcAS/93rkA6BiguUK/JV8JFGUvY2Zu2hNh0' +
    'FlJfGIShRbScrf8jIelzFeCdZc6EujSrQQIhAOTD9z+JcF8PGCRmyHDAdUEDnlXw' +
    'lobq80CtiTeabSwLAoIBABQTvKuqZNNbhMw6jwjwrVLhbRxVcaLxRHKVGS4b7+zN' +
    'j6zBANZ+OD0ds9XFpy5XIAkwgOiFCK3A/QC+1/eSRqSpzaBb6NwN9poAsxh5TzQe' +
    '0j1FmvUGDzjHgPSCZXCaEqgFtwqzn188oeOAWsGDNHfEeEIzM735AEyOfIEtDNwE' +
    'r5xaUpJa0m0q1TRule90VySR+1JMOJBdl6rDXeUe5YgdjW0oXBoZ7Ejlbzz8bjwk' +
    'LZaxipq4iScHar6gNIXSH5NRmhKRANg9/IRdzdduC0iyCwvANlQXjNqv6qxONvnK' +
    'Eeud+U0qeOzXQpFunX7jk0pWhXJsrHYoYE8JEF+t2TE=';

  DSA2048_ExampleCom_1_PrivateKeyPEM =
    'MIIDVgIBAAKCAQEA0SWakCcmMvfxKFGQZkRCYDvaCIVu1PVuDTzclYdvWMLRajxp' +
    'O3jKpaekYV9kh/beE6aRtyhEvNbEo9YmWPM8PSYwN5OBp8mW+gxaO7yrr/SVNXrF' +
    '+xsKdXle7UE4gP5JXtY4Tnl0u0BBGyrhl5rnBKfxJllhC4c+YEoEb+IWuJcal9Qh' +
    'v1S7dxVqB7KJ1PG8IOe4ainWjM7bnqjoHd5JpqfNami9CKKoV5EAdiQcVPEVcf1u' +
    'KU+FE6TzAqep60MTdi6K7l+xfTHKt15aZlcAS/93rkA6BiguUK/JV8JFGUvY2Zu2' +
    'hNh0FlJfGIShRbScrf8jIelzFeCdZc6EujSrQQIhAOTD9z+JcF8PGCRmyHDAdUED' +
    'nlXwlobq80CtiTeabSwLAoIBABQTvKuqZNNbhMw6jwjwrVLhbRxVcaLxRHKVGS4b' +
    '7+zNj6zBANZ+OD0ds9XFpy5XIAkwgOiFCK3A/QC+1/eSRqSpzaBb6NwN9poAsxh5' +
    'TzQe0j1FmvUGDzjHgPSCZXCaEqgFtwqzn188oeOAWsGDNHfEeEIzM735AEyOfIEt' +
    'DNwEr5xaUpJa0m0q1TRule90VySR+1JMOJBdl6rDXeUe5YgdjW0oXBoZ7Ejlbzz8' +
    'bjwkLZaxipq4iScHar6gNIXSH5NRmhKRANg9/IRdzdduC0iyCwvANlQXjNqv6qxO' +
    'NvnKEeud+U0qeOzXQpFunX7jk0pWhXJsrHYoYE8JEF+t2TECggEAbafGARPAM0KD' +
    '9DNoJTKn94GXQPJcHMqE8i8IZtDTtn2jFNcz5p+Ee9n7Tz0fPeNQuaQLUF3GK7wT' +
    '82D1JxEyFmWXysIyDa45iwXfFUMb44DxOZh6PNcH7pn/ZTQg9LeXFOSqpIUdEzAC' +
    '0feCbfaWT081vnJFEq6ZZ4TKNTiYo/pFRs6d6KxlJGHtuR1FvFcR2HmK9wff0W7t' +
    'wA3+D7rqE1eJd6GiAf2WGYkdwt/Jbyqtn3hQyO6++23qsE9yStaweL5lGxsyzpZ1' +
    'dyaKg9yQNZZGGIey7EPTZ02bpceADxx0beWg3QhSXMzk35+AA2o4wyL6jWujy7zR' +
    'xIDYYdGsCwIhAINGDcB6W3IQCCe/XSHrJ9uXEQ7zcytzmBbPBJW7L2Gr';

  DSA2048_ExampleCom_1_CertificatePEM =
    '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' +
    'ezo=';


  // rsa-stunnel
  // from stunnel pem file
  RSA_STunnel_PrivateKeyRSAPEM =
    'MIICXAIBAAKBgQCxUFMuqJJbI9KnB8VtwSbcvwNOltWBtWyaSmp7yEnqwWel5TFf' +
    'cOObCuLZ69sFi1ELi5C91qRaDMow7k5Gj05DZtLDFfICD0W1S+n2Kql2o8f2RSvZ' +
    'qD2W9l8i59XbCz1oS4l9S09L+3RTZV9oer/Unby/QmicFLNM0WgrVNiKywIDAQAB' +
    'AoGAKX4KeRipZvpzCPMgmBZi6bUpKPLS849o4pIXaO/tnCm1/3QqoZLhMB7UBvrS' +
    'PfHj/Tejn0jjHM9xYRHi71AJmAgzI+gcN1XQpHiW6kATNDz1r3yftpjwvLhuOcp9' +
    'tAOblojtImV8KrAlVH/21rTYQI+Q0m9qnWKKCoUsX9Yu8UECQQDlbHL38rqBvIMk' +
    'zK2wWJAbRvVf4Fs47qUSef9pOo+p7jrrtaTqd99irNbVRe8EWKbSnAod/B04d+cQ' +
    'ci8W+nVtAkEAxdqPOnCISW4MeS+qHSVtaGv2kwvfxqfsQw+zkwwHYqa+ueg4wHtG' +
    '/9+UgxcXyCXrj0ciYCqURkYhQoPbWP82FwJAWWkjgTgqsYcLQRs3kaNiPg8wb7Yb' +
    'NxviX0oGXTdCaAJ9GgGHjQ08lNMxQprnpLT8BtZjJv5rUOeBuKoXagggHQJAaUAF' +
    '91GLvnwzWHg5p32UgPsF1V14siX8MgR1Q6EfgKQxS5Y0Mnih4VXfnAi51vgNIk/2' +
    'AnBEJkoCQW8BTYueCwJBALvz2JkaUfCJc18E7jCP7qLY4+6qqsq+wr0t18+ogOM9' +
    'JIY9r6e1qwNxQ/j1Mud6gn6cRrObpRtEad5z2FtcnwY=';

  RSA_STunnel_CertificatePEM =
    'MIICDzCCAXigAwIBAgIBADANBgkqhkiG9w0BAQQFADBCMQswCQYDVQQGEwJQTDEf' +
    'MB0GA1UEChMWU3R1bm5lbCBEZXZlbG9wZXJzIEx0ZDESMBAGA1UEAxMJbG9jYWxo' +
    'b3N0MB4XDTk5MDQwODE1MDkwOFoXDTAwMDQwNzE1MDkwOFowQjELMAkGA1UEBhMC' +
    'UEwxHzAdBgNVBAoTFlN0dW5uZWwgRGV2ZWxvcGVycyBMdGQxEjAQBgNVBAMTCWxv' +
    'Y2FsaG9zdDCBnzANBgkqhkiG9w0BAQEFAAOBjQAwgYkCgYEAsVBTLqiSWyPSpwfF' +
    'bcEm3L8DTpbVgbVsmkpqe8hJ6sFnpeUxX3Djmwri2evbBYtRC4uQvdakWgzKMO5O' +
    'Ro9OQ2bSwxXyAg9FtUvp9iqpdqPH9kUr2ag9lvZfIufV2ws9aEuJfUtPS/t0U2Vf' +
    'aHq/1J28v0JonBSzTNFoK1TYissCAwEAAaMVMBMwEQYJYIZIAYb4QgEBBAQDAgZA' +
    'MA0GCSqGSIb3DQEBBAUAA4GBAAhYFTngWc3tuMjVFhS4HbfFF/vlOgTu44/rv2F+' +
    'ya1mEB93htfNxx3ofRxcjCdorqONZFwEba6xZ8/UujYfVmIGCBy4X8+aXd83TJ9A' +
    'eSjTzV9UayOoGtmg8Dv2aj/5iabNeK1Qf35ouvlcTezVZt2ZeJRhqUHcGaE+apCN' +
    'TC9Y';
{$ENDIF}



implementation



end.

