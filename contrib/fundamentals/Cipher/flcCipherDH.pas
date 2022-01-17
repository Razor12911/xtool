{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals 5.00                                        }
{   File name:        flcCipherDH.pas                                          }
{   File version:     5.08                                                     }
{   Description:      Diffie-Hellman (DH) cipher routines                      }
{                                                                              }
{   Copyright:        Copyright (c) 2010-2020, David J Butler                  }
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
{ References:                                                                  }
{                                                                              }
{   RFC 2631 - Diffie-Hellman Key Agreement Method                             }
{   RFC 5114 - Additional Diffie-Hellman Groups                                }
{   RFC 3526 - MODP Diffie-Hellman groups for IKE                              }
{   RFC 2409 - The Internet Key Exchange (IKE)                                 }
{   http://www.faqs.org/rfcs/rfc2539.html                                      }
{   https://weakdh.org/                                                        }
{   http://www.floatingdoghead.net/bigprimes.html                              }
{   https://www.cryptopp.com/wiki/Diffie-Hellman                               }
{   http://csrc.nist.gov/publications/fips/fips186-3/fips_186-3.pdf            }
{                                                                              }
{ Revision history:                                                            }
{                                                                              }
{   2010/11/07  0.01  Initial development (Primes, OtherInfo)                  }
{   2010/11/10  0.02  Further development (KEK)                                }
{   2010/12/03  0.03  Changes to prime generation based on RFC errata.         }
{   2016/01/07  0.04  Improvements and tests (KM, KEK and generation).         }
{   2016/01/08  0.05  Secure random.                                           }
{   2016/01/09  5.06  Revised for Fundamentals 5.                              }
{   2016/01/29  5.07  Well known groups, tests and fixes.                      }
{   2018/07/17  5.08  Types changes.                                           }
{                                                                              }
{ Todo:                                                                        }
{   - SHA256 Hash algorithm support                                            }
{   - Error codes                                                              }
{   - Split up TDHState                                                        }
{   - Callbacks for slow operations                                            }
{******************************************************************************}

{$INCLUDE flcCipher.inc}

unit flcCipherDH;

interface

uses
  { System }
  SysUtils,
  { Fundamentals }
  flcStdTypes,
  flcHugeInt;



type
  EDH = class(Exception);

  TDHHashAlgorithm = (
      dhhSHA1,
      dhhSHA256);

  TDHState = record
    HashAlgorithm  : TDHHashAlgorithm;
    HashBitCount   : Integer;
    PrimeQBitCount : Integer;          // Bits in Q = m
    PrimePBitCount : Integer;          // Bits in P = L
    P              : HugeWord;         // Group param: Prime P
    Q              : HugeWord;         // Group param: Prime Q
    Seed           : HugeWord;         // Group param: Seed
    Counter        : Integer;          // Group param: P counter
    J              : HugeWord;         // Group param: Generator validation parameter
    G              : HugeWord;         // Generator
    X              : HugeWord;         // Private key (Xa)
    Y              : HugeWord;         // Public key (Ya)
    ZZ             : HugeWord;         // Shared secret
    RemoteKeySize  : Integer;          // Remote public key (Yb)
    RemoteKey      : HugeWord;         // Remote public key (Yb)
    KEK            : RawByteString;
  end;
  PDHState = ^TDHState;

procedure DHStateInit(var State: TDHState);
procedure DHStateFinalise(var State: TDHState);

procedure DHInitHashAlgorithm(var State: TDHState;
          const HashAlgorithm: TDHHashAlgorithm);

procedure DHGeneratePrimeQ(var State: TDHState; const Bits: Integer;
          const FixedSeed: Boolean);
procedure DHGeneratePrimeP(var State: TDHState; const BitsP: Integer);

procedure DHGeneratePrimesPQ(
          var State: TDHState;
          const PrimeQBitCount, PrimePBitCount: Integer;
          const Validating: Boolean);
procedure DHGenerateG(var State: TDHState);

function  DHIsGeneratedGroupParameterValid(const State: TDHState): Boolean;

procedure DHGeneratePrivateKeyX(var State: TDHState);
procedure DHGeneratePublicKeyY(var State: TDHState);

function  DHQBitCount(const PBitCount: Integer): Integer;

procedure DHGenerateKeys(var State: TDHState;
          const HashAlgorithm: TDHHashAlgorithm;
          const PrimeQBitCount, PrimePBitCount: Integer);

procedure DHDeriveKeysFromGroupParametersPGQ(var State: TDHState;
          const HashAlgorithm: TDHHashAlgorithm;
          const PrimeQBitCount, PrimePBitCount: Integer;
          const P, G, Q: HugeWord);
procedure DHDeriveKeysFromGroupParametersPG(var State: TDHState;
          const HashAlgorithm: TDHHashAlgorithm;
          const PrimeQBitCount, PrimePBitCount: Integer;
          const P, G: HugeWord);
procedure DHDeriveKeysFromGroupParameterP1(var State: TDHState;
          const HashAlgorithm: TDHHashAlgorithm;
          const PrimeQBitCount, PrimePBitCount: Integer;
          const P: HugeWord);
procedure DHDeriveKeysFromGroupParameterP2(var State: TDHState);

function  DHIsPublicKeyValid(const State: TDHState; const Y: HugeWord): Boolean;

function  DHHugeWordKeyEncodeBytes(const A: HugeWord): RawByteString;
procedure DHHugeWordKeyDecodeBytes(var A: HugeWord; const B: RawByteString);

procedure DHGenerateSharedSecretZZ(
          var State: TDHState;
          const RemotePublicKeySize: Integer;
          const RemotePublicKey: HugeWord);

procedure DHGenerateKEK(
          var State: TDHState;
          const CipherOID: array of Integer;
          const CipherKeyBits: Integer;
          const PartyAInfo: RawByteString);

type
  TDHWellKnownGroup = record
    Description : String;
    Source      : String;
    IKEGroupId  : Integer;
    PBitCount   : Integer;
    QBitCount   : Integer;
    P_Hex       : RawByteString;
    G_Hex       : RawByteString;
  end;
  PDHWellKnownGroup = ^TDHWellKnownGroup;

const
  DHWellKnownGroups = 12;
  DHWellKnownGroup: array[0..DHWellKnownGroups - 1] of TDHWellKnownGroup = (
      (
      Description: 'RFC 2409 - 6.1 - First Oakley Default Group - 2^768 - 2 ^704 - 1 + 2^64 * ( [2^638 pi] + 149686 )';
      IKEGroupId: 1;
      PBitCount: 768;
      QBitCount: 160;
      P_Hex: 'FFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD1' +
             '29024E088A67CC74020BBEA63B139B22514A08798E3404DD' +
             'EF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245' +
             'E485B576625E7EC6F44C42E9A63A3620FFFFFFFFFFFFFFFF';
      G_Hex: '00000002'
      ),
      (
      Description: 'RFC 2409 - 6.2 - Second Oakley Group - 2^1024 - 2^960 - 1 + 2^64 * ( [2^894 pi] + 129093 )';
      IKEGroupId: 2;
      PBitCount: 1024;
      QBitCount: 160;
      P_Hex: 'FFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD1' +
             '29024E088A67CC74020BBEA63B139B22514A08798E3404DD' +
             'EF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245' +
             'E485B576625E7EC6F44C42E9A637ED6B0BFF5CB6F406B7ED' +
             'EE386BFB5A899FA5AE9F24117C4B1FE649286651ECE65381' +
             'FFFFFFFFFFFFFFFF';
      G_Hex: '00000002'
      ),
      (
      Description: 'RFC 3526 - 2 - 1536-bit MODP Group - 2^1536 - 2^1472 - 1 + 2^64 * ( [2^1406 pi] + 741804 )';
      IKEGroupId: 5;
      PBitCount: 1536;
      QBitCount: 160;
      P_Hex: 'FFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD1' +
             '29024E088A67CC74020BBEA63B139B22514A08798E3404DD' +
             'EF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245' +
             'E485B576625E7EC6F44C42E9A637ED6B0BFF5CB6F406B7ED' +
             'EE386BFB5A899FA5AE9F24117C4B1FE649286651ECE45B3D' +
             'C2007CB8A163BF0598DA48361C55D39A69163FA8FD24CF5F' +
             '83655D23DCA3AD961C62F356208552BB9ED529077096966D' +
             '670C354E4ABC9804F1746C08CA237327FFFFFFFFFFFFFFFF';
      G_Hex: '00000002'
      ),
      (
      Description: 'RFC 3526 - 3 - 2048-bit MODP Group - 2^2048 - 2^1984 - 1 + 2^64 * ( [2^1918 pi] + 124476 )';
      IKEGroupId: 14;
      PBitCount: 2048;
      QBitCount: 256;
      P_Hex: 'FFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD1' +
             '29024E088A67CC74020BBEA63B139B22514A08798E3404DD' +
             'EF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245' +
             'E485B576625E7EC6F44C42E9A637ED6B0BFF5CB6F406B7ED' +
             'EE386BFB5A899FA5AE9F24117C4B1FE649286651ECE45B3D' +
             'C2007CB8A163BF0598DA48361C55D39A69163FA8FD24CF5F' +
             '83655D23DCA3AD961C62F356208552BB9ED529077096966D' +
             '670C354E4ABC9804F1746C08CA18217C32905E462E36CE3B' +
             'E39E772C180E86039B2783A2EC07A28FB5C55DF06F4C52C9' +
             'DE2BCBF6955817183995497CEA956AE515D2261898FA0510' +
             '15728E5A8AACAA68FFFFFFFFFFFFFFFF';
      G_Hex: '00000002'
      ),
      (
      Description: 'RFC 3526 - 4 - 3072-bit MODP Group - 2^3072 - 2^3008 - 1 + 2^64 * ( [2^2942 pi] + 1690314 )';
      IKEGroupId: 15;
      PBitCount: 3072;
      QBitCount: 256;
      P_Hex: 'FFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD1' +
             '29024E088A67CC74020BBEA63B139B22514A08798E3404DD' +
             'EF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245' +
             'E485B576625E7EC6F44C42E9A637ED6B0BFF5CB6F406B7ED' +
             'EE386BFB5A899FA5AE9F24117C4B1FE649286651ECE45B3D' +
             'C2007CB8A163BF0598DA48361C55D39A69163FA8FD24CF5F' +
             '83655D23DCA3AD961C62F356208552BB9ED529077096966D' +
             '670C354E4ABC9804F1746C08CA18217C32905E462E36CE3B' +
             'E39E772C180E86039B2783A2EC07A28FB5C55DF06F4C52C9' +
             'DE2BCBF6955817183995497CEA956AE515D2261898FA0510' +
             '15728E5A8AAAC42DAD33170D04507A33A85521ABDF1CBA64' +
             'ECFB850458DBEF0A8AEA71575D060C7DB3970F85A6E1E4C7' +
             'ABF5AE8CDB0933D71E8C94E04A25619DCEE3D2261AD2EE6B' +
             'F12FFA06D98A0864D87602733EC86A64521F2B18177B200C' +
             'BBE117577A615D6C770988C0BAD946E208E24FA074E5AB31' +
             '43DB5BFCE0FD108E4B82D120A93AD2CAFFFFFFFFFFFFFFFF';
      G_Hex: '00000002'
      ),
      (
      Description: 'RFC 3526 - 5 - 4096-bit MODP Group - 2^4096 - 2^4032 - 1 + 2^64 * ( [2^3966 pi] + 240904 )';
      IKEGroupId: 16;
      PBitCount: 4096;
      QBitCount: 256;
      P_Hex: 'FFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD1' +
             '29024E088A67CC74020BBEA63B139B22514A08798E3404DD' +
             'EF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245' +
             'E485B576625E7EC6F44C42E9A637ED6B0BFF5CB6F406B7ED' +
             'EE386BFB5A899FA5AE9F24117C4B1FE649286651ECE45B3D' +
             'C2007CB8A163BF0598DA48361C55D39A69163FA8FD24CF5F' +
             '83655D23DCA3AD961C62F356208552BB9ED529077096966D' +
             '670C354E4ABC9804F1746C08CA18217C32905E462E36CE3B' +
             'E39E772C180E86039B2783A2EC07A28FB5C55DF06F4C52C9' +
             'DE2BCBF6955817183995497CEA956AE515D2261898FA0510' +
             '15728E5A8AAAC42DAD33170D04507A33A85521ABDF1CBA64' +
             'ECFB850458DBEF0A8AEA71575D060C7DB3970F85A6E1E4C7' +
             'ABF5AE8CDB0933D71E8C94E04A25619DCEE3D2261AD2EE6B' +
             'F12FFA06D98A0864D87602733EC86A64521F2B18177B200C' +
             'BBE117577A615D6C770988C0BAD946E208E24FA074E5AB31' +
             '43DB5BFCE0FD108E4B82D120A92108011A723C12A787E6D7' +
             '88719A10BDBA5B2699C327186AF4E23C1A946834B6150BDA' +
             '2583E9CA2AD44CE8DBBBC2DB04DE8EF92E8EFC141FBECAA6' +
             '287C59474E6BC05D99B2964FA090C3A2233BA186515BE7ED' +
             '1F612970CEE2D7AFB81BDD762170481CD0069127D5B05AA9' +
             '93B4EA988D8FDDC186FFB7DC90A6C08F4DF435C934063199' +
             'FFFFFFFFFFFFFFFF';
      G_Hex: '00000002'
      ),
      (
      Description: 'RFC 3526 - 6 - 6144-bit MODP Group - 2^6144 - 2^6080 - 1 + 2^64 * ( [2^6014 pi] + 929484 )';
      IKEGroupId: 17;
      PBitCount: 6144;
      QBitCount: 256;
      P_Hex: 'FFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD129024E08' +
             '8A67CC74020BBEA63B139B22514A08798E3404DDEF9519B3CD3A431B' +
             '302B0A6DF25F14374FE1356D6D51C245E485B576625E7EC6F44C42E9' +
             'A637ED6B0BFF5CB6F406B7EDEE386BFB5A899FA5AE9F24117C4B1FE6' +
             '49286651ECE45B3DC2007CB8A163BF0598DA48361C55D39A69163FA8' +
             'FD24CF5F83655D23DCA3AD961C62F356208552BB9ED529077096966D' +
             '670C354E4ABC9804F1746C08CA18217C32905E462E36CE3BE39E772C' +
             '180E86039B2783A2EC07A28FB5C55DF06F4C52C9DE2BCBF695581718' +
             '3995497CEA956AE515D2261898FA051015728E5A8AAAC42DAD33170D' +
             '04507A33A85521ABDF1CBA64ECFB850458DBEF0A8AEA71575D060C7D' +
             'B3970F85A6E1E4C7ABF5AE8CDB0933D71E8C94E04A25619DCEE3D226' +
             '1AD2EE6BF12FFA06D98A0864D87602733EC86A64521F2B18177B200C' +
             'BBE117577A615D6C770988C0BAD946E208E24FA074E5AB3143DB5BFC' +
             'E0FD108E4B82D120A92108011A723C12A787E6D788719A10BDBA5B26' +
             '99C327186AF4E23C1A946834B6150BDA2583E9CA2AD44CE8DBBBC2DB' +
             '04DE8EF92E8EFC141FBECAA6287C59474E6BC05D99B2964FA090C3A2' +
             '233BA186515BE7ED1F612970CEE2D7AFB81BDD762170481CD0069127' +
             'D5B05AA993B4EA988D8FDDC186FFB7DC90A6C08F4DF435C934028492' +
             '36C3FAB4D27C7026C1D4DCB2602646DEC9751E763DBA37BDF8FF9406' +
             'AD9E530EE5DB382F413001AEB06A53ED9027D831179727B0865A8918' +
             'DA3EDBEBCF9B14ED44CE6CBACED4BB1BDB7F1447E6CC254B33205151' +
             '2BD7AF426FB8F401378CD2BF5983CA01C64B92ECF032EA15D1721D03' +
             'F482D7CE6E74FEF6D55E702F46980C82B5A84031900B1C9E59E7C97F' +
             'BEC7E8F323A97A7E36CC88BE0F1D45B7FF585AC54BD407B22B4154AA' +
             'CC8F6D7EBF48E1D814CC5ED20F8037E0A79715EEF29BE32806A1D58B' +
             'B7C5DA76F550AA3D8A1FBFF0EB19CCB1A313D55CDA56C9EC2EF29632' +
             '387FE8D76E3C0468043E8F663F4860EE12BF2D5B0B7474D6E694F91E' +
             '6DCC4024FFFFFFFFFFFFFFFF';
      G_Hex: '00000002'
      ),
      (
      Description: 'RFC 3526 - 7 - 8192-bit MODP Group - 2^8192 - 2^8128 - 1 + 2^64 * ( [2^8062 pi] + 4743158 )';
      IKEGroupId: 18;
      PBitCount: 8192;
      QBitCount: 256;
      P_Hex: 'FFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD1' +
             '29024E088A67CC74020BBEA63B139B22514A08798E3404DD' +
             'EF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245' +
             'E485B576625E7EC6F44C42E9A637ED6B0BFF5CB6F406B7ED' +
             'EE386BFB5A899FA5AE9F24117C4B1FE649286651ECE45B3D' +
             'C2007CB8A163BF0598DA48361C55D39A69163FA8FD24CF5F' +
             '83655D23DCA3AD961C62F356208552BB9ED529077096966D' +
             '670C354E4ABC9804F1746C08CA18217C32905E462E36CE3B' +
             'E39E772C180E86039B2783A2EC07A28FB5C55DF06F4C52C9' +
             'DE2BCBF6955817183995497CEA956AE515D2261898FA0510' +
             '15728E5A8AAAC42DAD33170D04507A33A85521ABDF1CBA64' +
             'ECFB850458DBEF0A8AEA71575D060C7DB3970F85A6E1E4C7' +
             'ABF5AE8CDB0933D71E8C94E04A25619DCEE3D2261AD2EE6B' +
             'F12FFA06D98A0864D87602733EC86A64521F2B18177B200C' +
             'BBE117577A615D6C770988C0BAD946E208E24FA074E5AB31' +
             '43DB5BFCE0FD108E4B82D120A92108011A723C12A787E6D7' +
             '88719A10BDBA5B2699C327186AF4E23C1A946834B6150BDA' +
             '2583E9CA2AD44CE8DBBBC2DB04DE8EF92E8EFC141FBECAA6' +
             '287C59474E6BC05D99B2964FA090C3A2233BA186515BE7ED' +
             '1F612970CEE2D7AFB81BDD762170481CD0069127D5B05AA9' +
             '93B4EA988D8FDDC186FFB7DC90A6C08F4DF435C934028492' +
             '36C3FAB4D27C7026C1D4DCB2602646DEC9751E763DBA37BD' +
             'F8FF9406AD9E530EE5DB382F413001AEB06A53ED9027D831' +
             '179727B0865A8918DA3EDBEBCF9B14ED44CE6CBACED4BB1B' +
             'DB7F1447E6CC254B332051512BD7AF426FB8F401378CD2BF' +
             '5983CA01C64B92ECF032EA15D1721D03F482D7CE6E74FEF6' +
             'D55E702F46980C82B5A84031900B1C9E59E7C97FBEC7E8F3' +
             '23A97A7E36CC88BE0F1D45B7FF585AC54BD407B22B4154AA' +
             'CC8F6D7EBF48E1D814CC5ED20F8037E0A79715EEF29BE328' +
             '06A1D58BB7C5DA76F550AA3D8A1FBFF0EB19CCB1A313D55C' +
             'DA56C9EC2EF29632387FE8D76E3C0468043E8F663F4860EE' +
             '12BF2D5B0B7474D6E694F91E6DBE115974A3926F12FEE5E4' +
             '38777CB6A932DF8CD8BEC4D073B931BA3BC832B68D9DD300' +
             '741FA7BF8AFC47ED2576F6936BA424663AAB639C5AE4F568' +
             '3423B4742BF1C978238F16CBE39D652DE3FDB8BEFC848AD9' +
             '22222E04A4037C0713EB57A81A23F0C73473FC646CEA306B' +
             '4BCBC8862F8385DDFA9D4B7FA2C087E879683303ED5BDD3A' +
             '062B3CF5B3A278A66D2A13F83F44F82DDF310EE074AB6A36' +
             '4597E899A0255DC164F31CC50846851DF9AB48195DED7EA1' +
             'B1D510BD7EE74D73FAF36BC31ECFA268359046F4EB879F92' +
             '4009438B481C6CD7889A002ED5EE382BC9190DA6FC026E47' +
             '9558E4475677E9AA9E3050E2765694DFC81F56E880B96E71' +
             '60C980DD98EDD3DFFFFFFFFFFFFFFFFF';
      G_Hex: '00000002'
      ),
      (
      Description: 'RFC 5114 - 2.1 - 1024-bit MODP Group with 160-bit Prime Order Subgroup';
      IKEGroupId: 22;
      PBitCount: 1024;
      QBitCount: 160;
      P_Hex: 'B10B8F96A080E01DDE92DE5EAE5D54EC52C99FBCFB06A3C6' +
             '9A6A9DCA52D23B616073E28675A23D189838EF1E2EE652C0' +
             '13ECB4AEA906112324975C3CD49B83BFACCBDD7D90C4BD70' +
             '98488E9C219A73724EFFD6FAE5644738FAA31A4FF55BCCC0' +
             'A151AF5F0DC8B4BD45BF37DF365C1A65E68CFDA76D4DA708' +
             'DF1FB2BC2E4A4371';
      G_Hex: 'A4D1CBD5C3FD34126765A442EFB99905F8104DD258AC507F' +
             'D6406CFF14266D31266FEA1E5C41564B777E690F5504F213' +
             '160217B4B01B886A5E91547F9E2749F4D7FBD7D3B9A92EE1' +
             '909D0D2263F80A76A6A24C087A091F531DBF0A0169B6A28A' +
             'D662A4D18E73AFA32D779D5918D08BC8858F4DCEF97C2A24' +
             '855E6EEB22B3B2E5'
      ),
      (
      Description: 'RFC 5114 - 2.2 - 2048-bit MODP Group with 224-bit Prime Order Subgroup';
      IKEGroupId: 23;
      PBitCount: 2048;
      QBitCount: 224;
      P_Hex: 'AD107E1E9123A9D0D660FAA79559C51FA20D64E5683B9FD1' +
             'B54B1597B61D0A75E6FA141DF95A56DBAF9A3C407BA1DF15' +
             'EB3D688A309C180E1DE6B85A1274A0A66D3F8152AD6AC212' +
             '9037C9EDEFDA4DF8D91E8FEF55B7394B7AD5B7D0B6C12207' +
             'C9F98D11ED34DBF6C6BA0B2C8BBC27BE6A00E0A0B9C49708' +
             'B3BF8A317091883681286130BC8985DB1602E714415D9330' +
             '278273C7DE31EFDC7310F7121FD5A07415987D9ADC0A486D' +
             'CDF93ACC44328387315D75E198C641A480CD86A1B9E587E8' +
             'BE60E69CC928B2B9C52172E413042E9B23F10B0E16E79763' +
             'C9B53DCF4BA80A29E3FB73C16B8E75B97EF363E2FFA31F71' +
             'CF9DE5384E71B81C0AC4DFFE0C10E64F';
      G_Hex: 'AC4032EF4F2D9AE39DF30B5C8FFDAC506CDEBE7B89998CAF' +
             '74866A08CFE4FFE3A6824A4E10B9A6F0DD921F01A70C4AFA' +
             'AB739D7700C29F52C57DB17C620A8652BE5E9001A8D66AD7' +
             'C17669101999024AF4D027275AC1348BB8A762D0521BC98A' +
             'E247150422EA1ED409939D54DA7460CDB5F6C6B250717CBE' +
             'F180EB34118E98D119529A45D6F834566E3025E316A330EF' +
             'BB77A86F0C1AB15B051AE3D428C8F8ACB70A8137150B8EEB' +
             '10E183EDD19963DDD9E263E4770589EF6AA21E7F5F2FF381' +
             'B539CCE3409D13CD566AFBB48D6C019181E1BCFE94B30269' +
             'EDFE72FE9B6AA4BD7B5A0F1C71CFFF4C19C418E1F6EC0179' +
             '81BC087F2A7065B384B890D3191F2BFA';
      ),
      (
      Description: 'RFC 5114 - 2.3 - 2048-bit MODP Group with 256-bit Prime Order Subgroup';
      IKEGroupId: 24;
      PBitCount: 2048;
      QBitCount: 256;
      P_Hex: '87A8E61DB4B6663CFFBBD19C651959998CEEF608660DD0F2' +
             '5D2CEED4435E3B00E00DF8F1D61957D4FAF7DF4561B2AA30' +
             '16C3D91134096FAA3BF4296D830E9A7C209E0C6497517ABD' +
             '5A8A9D306BCF67ED91F9E6725B4758C022E0B1EF4275BF7B' +
             '6C5BFC11D45F9088B941F54EB1E59BB8BC39A0BF12307F5C' +
             '4FDB70C581B23F76B63ACAE1CAA6B7902D52526735488A0E' +
             'F13C6D9A51BFA4AB3AD8347796524D8EF6A167B5A41825D9' +
             '67E144E5140564251CCACB83E6B486F6B3CA3F7971506026' +
             'C0B857F689962856DED4010ABD0BE621C3A3960A54E710C3' +
             '75F26375D7014103A4B54330C198AF126116D2276E11715F' +
             '693877FAD7EF09CADB094AE91E1A1597';
      G_Hex: '3FB32C9B73134D0B2E77506660EDBD484CA7B18F21EF2054' +
             '07F4793A1A0BA12510DBC15077BE463FFF4FED4AAC0BB555' +
             'BE3A6C1B0C6B47B1BC3773BF7E8C6F62901228F8C28CBB18' +
             'A55AE31341000A650196F931C77A57F2DDF463E5E9EC144B' +
             '777DE62AAAB8A8628AC376D282D6ED3864E67982428EBC83' +
             '1D14348F6F2F9193B5045AF2767164E1DFC967C1FB3F2E55' +
             'A4BD1BFFE83B9C80D052B985D182EA0ADB2A3B7313D3FE14' +
             'C8484B1E052588B9B7D2BBD2DF016199ECD06E1557CD0915' +
             'B3353BBB64E0EC377FD028370DF92B52C7891428CDC67EB6' +
             '184B523D1DB246C32F63078490F00EF8D647D148D4795451' +
             '5E2327CFEF98C582664B4C0F6CC41659'
      ),
      (
      Description: 'http://www.floatingdoghead.net/bigprimes.html - 8192-bit secure Diffie-Hellman modulus #1';
      IKEGroupId: -1;
      PBitCount: 8192;
      QBitCount: 192;
      P_Hex: '99134f625a7e28b61a2e610e1d55c5b1c01dc37f718c16116e482f500a046cca' +
             '650df4bea06121e842c16e3208548c51dbfe07a34fba510d61c5961d8920c41b' +
             '516e47ead343d0152f71ff692ccfd078b90323f0405ab12a0f5b1717236242b5' +
             'b4382b65bfeee7a8ed102a9fd8870b42a6b54a7667cfb0ac6a58de6b7ef1d7ca' +
             'db892d859ec9cd5feef5ca54127aa75aa84d20ea88d3085d3fab4ea71a82b4c0' +
             'ad0b9139d68d9521ca9ca4bef9f2221f96c7b3d93564e7735ea1514e318baa24' +
             '72e46a0f474fcfcafab662843d172ab0a5021e27fd8dd3c077f876350029679a' +
             '25de16eb0df0371d889f48d709ae68638dc2c82e797dccec72f8dec9842ba8a7' +
             'a9b9a047afc98769252b1c3db5805b7e7a91b4860eb785cd0eaa575f9313a876' +
             '2eca9929aed02f5fbd660a11c2a3bb70e2836a471928d283ab40fc470cf71331' +
             'eb51e6a09ad94b8f50f33af6c8af995fc943616467a6815e5d32cfb25a084ac6' +
             '386746dd5ab339283e643bf3ef7d488838769b2990776b728b1de7e605c8294f' +
             'cc7ecd9092a357d3286f7827bf921e4fb7239db2f5aa415ec53be0a5b26f7063' +
             '2191929b7dc9a8e17917b308884bb0745e8420fc6639a570ef45e3eae39a9444' +
             'cdcfc4509775d32f297a47025fd706fd6854a6a327981c4e4e56280ac590e63b' +
             'cd6dea98682a4c655de964ae0de75922b0f88a071950029de9f6ce673599ebb7' +
             '4bb122969b7c065a10ee2e6e60c41e8acae2ea6da0097810192d17198756dd22' +
             'bc1b533602e60dab38c0146b02394c7ba727da464bd7ab5bbd0b3c287522caf5' +
             '1f69db508fc5c603a7832bfbdba41f7467e3a565984368421e489cf04a44a0d4' +
             'fb5bd58237773506284131ea99c0c55545729d1b56473e2ee83e73c74fd19b5f' +
             '076115a85042370028e899c6cdaac1be1e9668518be1e48aecf40de573da2241' +
             '3e20a2d64d6ff935c5899f1c3c40eec8c33f9389aaff8ea6cab749199e09084f' +
             '94aa7dfe5257c09c601198ae84225cf1f995ce8610a1c21f79d767f177c75f6b' +
             '3a5575e3880b021e67f9c8bca52a6b8ce4ceec522fae129a08a23288f570403e' +
             '8acd8973b490fae5a6e669a5291afee18550b3d6e96100ee41b32b92e48056ad' +
             '626451d132c4676496776951eff8c672a480d297fb9d12aaa3faee456ea93953' +
             'b1ca94ceeaeb6e4091fd820df5d6bfc2fed4a2587a096f8b5267600a97c4b9eb' +
             'bfa91b5579dbe1cfaf7c6018bc4a1d679204c2e729c492eba06bd506f77b2403' +
             'd57730d1c73dcc861aabb2f3e90ffdb9a12bf3298eb34924c2be999bbcc6e4d7' +
             'dea62e98a1ea40943c998142a9947a13eef3e1531506a30b369b399bf22868cf' +
             'c79684e07e8dad0a936d8db81e876f3711dc1ea0c53fc19e234f00f826ccc2f8' +
             '8aa0e2ae60eebaff8a98977924c2054429454ca000c5c2aa22fccfc5e2fdd553';
      G_Hex: ''
      )
      );



{                                                                              }
{ Test cases                                                                   }
{                                                                              }
{$IFDEF CIPHER_TEST}
procedure Test;
{$ENDIF}



implementation

uses
  { Fundamentals }
  flcHash,
  flcRandom,
  flcASN1,
  flcCipherRandom;



{                                                                              }
{ SecureClear                                                                  }
{                                                                              }
procedure SecureClearHugeWord(var A: HugeWord);
begin
  if (A.Alloc = 0) or not Assigned(A.Data) then
    exit;
  SecureClear(A.Data^, A.Alloc * HugeWordElementSize);
end;

procedure SecureHugeWordFinalise(var A: HugeWord);
begin
  SecureClearHugeWord(A);
  HugeWordFinalise(A);
end;



{                                                                              }
{ SecureHugeWordRandom                                                         }
{                                                                              }
procedure SecureHugeWordRandom(var A: HugeWord; const Size: Integer);
begin
  HugeWordSetSize(A, Size);
  if Size <= 0 then
    exit;
  SecureRandomBuf(A.Data^, Size * HugeWordElementSize);
end;



{                                                                              }
{ DH                                                                           }
{                                                                              }
const
  SPrimeGenerationFailure = 'Failed to generate prime';
  SInvalidParameter = 'Invalid parameter';

procedure DHStateInit(var State: TDHState);
begin
  FillChar(State, SizeOf(TDHState), 0);
  HugeWordInit(State.P);
  HugeWordInit(State.Q);
  HugeWordInit(State.Seed);
  HugeWordInit(State.J);
  HugeWordInit(State.G);
  HugeWordInit(State.X);
  HugeWordInit(State.Y);
  HugeWordInit(State.ZZ);
  HugeWordInit(State.RemoteKey);
end;

procedure DHStateFinalise(var State: TDHState);
begin
  SecureHugeWordFinalise(State.RemoteKey);
  SecureHugeWordFinalise(State.ZZ);
  SecureHugeWordFinalise(State.Y);
  SecureHugeWordFinalise(State.X);
  SecureHugeWordFinalise(State.G);
  SecureHugeWordFinalise(State.J);
  SecureHugeWordFinalise(State.Seed);
  SecureHugeWordFinalise(State.Q);
  SecureHugeWordFinalise(State.P);
end;

const
  HashAlgorithmBitCount : array[TDHHashAlgorithm] of Integer = (160, 256);

procedure DHInitHashAlgorithm(var State: TDHState;
          const HashAlgorithm: TDHHashAlgorithm);
begin
  State.HashAlgorithm := HashAlgorithm;
  State.HashBitCount := HashAlgorithmBitCount[HashAlgorithm];
end;

procedure DHHugeWordSHA1(const A: HugeWord; var B: HugeWord);
var H : T160BitDigest;
begin
  H := CalcSHA1(A.Data^, A.Used * HugeWordElementSize);
  HugeWordSetSize(B, SHA1DigestBits div HugeWordElementBits);
  Move(H.Word32s[0], B.Data^, SHA1DigestSize);
end;

procedure DHRandomSeed(const Bits: Integer; var Seed: HugeWord);
var
  N, I : Integer;
begin
  Assert(Bits >= HugeWordElementBits);

  N := (Bits + HugeWordElementBits - 1) div HugeWordElementBits;
  SecureHugeWordRandom(Seed, N);
  for I := Bits to N * HugeWordElementBits - 1 do
    HugeWordClearBit(Seed, I);
end;

procedure DHGeneratePrimeQ_CalcU(var U: HugeWord; const State: TDHState; const M, MP, HashBitCount: Integer);
var I, F : Integer;
    T1, T2 : HugeWord;
begin
  Assert(MP > 0);
  Assert(HashBitCount > 0);
  Assert(not HugeWordIsZero(State.Seed));

  Assert(State.HashAlgorithm = dhhSHA1);
  Assert(State.HashBitCount = 160);
  Assert(HashBitCount = State.HashBitCount);

  HugeWordInit(T1);
  HugeWordInit(T2);
  try
    // set U = 0
    HugeWordAssignZero(U);
    // for i = 0 to m' - 1
    for I := 0 to MP - 1 do
      begin
        // http://www.rfc-editor.org/errata_search.php?rfc=2631
        // Modified version from errata for RFC 2631:
        // U = U + (
        //          SHA1[SEED + i] XOR
        //          SHA1[(SEED + m' + i) mod (2^seedlen)]
        //         )
        //         * 2^(160 * i)
        //
        //     T1 = SHA1[SEED + i]
        HugeWordAssign(T1, State.Seed);
        HugeWordAddWord32(T1, I);
        DHHugeWordSHA1(T1, T1);
        //     T2 = SEED + m' + i
        HugeWordAssign(T2, State.Seed);
        HugeWordAddWord32(T2, MP + I);
        //     T2 = T2 mod (2^seedlen)
        for F := M to HugeWordGetBitCount(T2) - 1 do
          HugeWordClearBit(T2, F);
        HugeWordNormalise(T2);
        //     T2 = SHA1[T2]
        DHHugeWordSHA1(T2, T2);
        //     T1 = T1 XOR T2
        HugeWordXorHugeWord(T1, T2);
        //     T1 = T1 * 2^(160 * i)
        HugeWordShl(T1, HashBitCount * I);
        //     U = U + T1
        HugeWordAdd(U, T1);
      end;
  finally
    SecureHugeWordFinalise(T2);
    SecureHugeWordFinalise(T1);
  end;
end;

procedure DHGeneratePrimeQ(var State: TDHState; const Bits: Integer;
    const FixedSeed: Boolean);
var
  M, MP, H : Integer;
  Q, U : HugeWord;
  R : Boolean;
  I : Integer;
begin
  Assert(Bits > 0);
  Assert(State.HashBitCount > 0);

  HugeWordInit(Q);
  HugeWordInit(U);
  try
    M := Bits;
    H := State.HashBitCount;
    // set m' = m / 160 where / represents integer division with rounding upwards i.e. 200/160 = 2
    MP := (M + H - 1) div H;
    repeat
      // select an arbitrary bit string SEED such that the length of SEED >= m
      if not FixedSeed then
        DHRandomSeed(M, State.Seed);
      // calculate U
      DHGeneratePrimeQ_CalcU(U, State, M, MP, H);
      // form q from U by computing U mod (2^m) and setting the most significant
      // bit (the 2^(m-1) bit) and the least significant bit to 1. In terms of
      // boolean operations, q = U OR 2^(m-1) OR 1. Note that 2^(m-1) < q < 2^m
      HugeWordAssign(Q, U);
      for I := M to HugeWordGetSizeInBits(Q) - 1 do
        HugeWordClearBit(Q, I);
      HugeWordSetBit(Q, 0);
      HugeWordSetBit(Q, M - 1);
      HugeWordNormalise(Q);
      // use a robust primality algorithm to test whether q is prime
      R := HugeWordIsPrime(Q) <> pNotPrime;
      if FixedSeed and not R then
        raise EDH.Create(SPrimeGenerationFailure);
    until R;
    State.PrimeQBitCount := M;
    HugeWordAssign(State.Q, Q);
  finally
    SecureHugeWordFinalise(U);
    SecureHugeWordFinalise(Q);
  end;
end;

procedure DHGeneratePrimeP(var State: TDHState; const BitsP: Integer);
var L, M, LP, NP, MP, C, I, H, IZ : Integer;
    P, R, T1, T2, V, W, X : HugeWord;
    PR : Boolean;
begin
  Assert(BitsP > 0);
  Assert(State.HashBitCount > 0);
  Assert(State.PrimeQBitCount > 0);
  Assert(not HugeWordIsZero(State.Q));

  Assert(State.HashAlgorithm = dhhSHA1);
  Assert(State.HashBitCount = 160);

  HugeWordInit(P);
  HugeWordInit(R);
  HugeWordInit(T1);
  HugeWordInit(T2);
  HugeWordInit(V);
  HugeWordInit(W);
  HugeWordInit(X);
  try
    H := State.HashBitCount;
    // set L' = L/160, set N' = L/1024
    L := BitsP;
    M := State.PrimeQBitCount;
    LP := (L + H - 1) div H;
    NP := (L + 1023) div 1024;
    MP := (M + H - 1) div H;
    // let counter = 0
    C := 0;
    repeat
      // set R = Seed + 2*m' + (L' * counter)
      HugeWordAssign(R, State.Seed);
      HugeWordAddWord32(R, 2 * MP + LP * C);
      // set V = 0
      HugeWordAssignZero(V);
      // for i = 0 to L' - 1 do
      for I := 0 to LP - 1 do
        begin
          // V = V + SHA1(R + i) * 2^(160 * i)
          HugeWordAssign(T1, R);
          HugeWordAddWord32(T1, I);
          DHHugeWordSHA1(T1, T1);
          if I > 0 then
            HugeWordShl(T1, H * I);
          HugeWordAdd(V, T1);
        end;
      // set W = V mod 2^L
      HugeWordAssign(W, V);
      for IZ := L to HugeWordGetBitCount(W) - 1 do
        HugeWordClearBit(W, IZ);
      // set X = W OR 2^(L-1)
      // note that 0 <= W < 2^(L-1) and hence X >= 2^(L-1)
      HugeWordAssign(X, W);
      HugeWordSetBit(X, L - 1);
      HugeWordNormalise(X);
      // set p = X - (X mod (2*q)) + 1
      HugeWordAssign(P, X);
      //     T1 = 2 * Q
      HugeWordAssign(T1, State.Q);
      HugeWordShl(T1, 1);
      //     T2 = X mod (2*q)
      HugeWordMod(X, T1, T2);
      HugeWordSubtract(P, T2);
      HugeWordAddWord32(P, 1);
      // if p > 2^(L-1) use a robust primality test to test whether p is prime
      if not HugeWordIsBitSet(P, L - 1) then
        PR := False
      else
        PR := HugeWordIsPrime(P) <> pNotPrime;
      // if p is prime output p, q, seed, counter and stop, else
      // set counter = counter + 1
      if not PR then
        Inc(C);
      // if counter < (4096 * N) then repeat
    until PR or (C >= 4096 * NP);
    if not PR then
      raise EDH.Create(SPrimeGenerationFailure);
    State.PrimePBitCount := L;
    State.Counter := C;
    HugeWordAssign(State.P, P);
  finally
    SecureHugeWordFinalise(X);
    SecureHugeWordFinalise(W);
    SecureHugeWordFinalise(V);
    SecureHugeWordFinalise(T2);
    SecureHugeWordFinalise(T1);
    SecureHugeWordFinalise(R);
    SecureHugeWordFinalise(P);
  end;
end;

procedure DHGeneratePrimesPQ(
          var State: TDHState;
          const PrimeQBitCount, PrimePBitCount: Integer;
          const Validating: Boolean);
begin
  Assert(PrimeQBitCount >= 160);
  Assert(PrimePBitCount > PrimeQBitCount);

  DHGeneratePrimeQ(State, PrimeQBitCount, Validating);
  DHGeneratePrimeP(State, PrimePBitCount);
end;

{ g = h^[(p-1)/q] mod p, where
  h is any integer with 1 < h < p-1 such that h[(p-1)/q] mod p > 1
  (g has order q mod p; i.e. g^q mod p = 1 if g!=1)
  j a large integer such that p=qj + 1 }
procedure DHGenerateG(var State: TDHState);
var
  J, T1, T2, H, G : HugeWord;
begin
  Assert(not HugeWordIsZero(State.P));
  Assert(not HugeWordIsZero(State.Q));

  HugeWordInit(J);
  HugeWordInit(T1);
  HugeWordInit(T2);
  HugeWordInit(H);
  HugeWordInit(G);
  try
    // 1. Let j = (p - 1)/q.
    // T1 = p - 1
    HugeWordAssign(T1, State.P);
    HugeWordSubtractWord32(T1, 1);
    // J = T1 / q
    HugeWordDivide(T1, State.Q, J, T2);
    repeat
      // 2. Set h = any integer, where 1 < h < p - 1 and h differs from any value previously tried.
      repeat
        SecureHugeWordRandom(H, State.P.Used);
      until (HugeWordCompare(H, T1) < 0) and
            (HugeWordCompareWord32(H, 1) > 0);
      // 3. Set g = h^j mod p
      HugeWordPowerAndMod(G, H, J, State.P);
      // 4. If g = 1 go to step 2
    until not HugeWordIsOne(G);
    HugeWordAssign(State.J, J);
    HugeWordAssign(State.G, G);
  finally
    SecureHugeWordFinalise(G);
    SecureHugeWordFinalise(H);
    SecureHugeWordFinalise(T2);
    SecureHugeWordFinalise(T1);
    SecureHugeWordFinalise(J);
  end;
end;

{
2.2.2.  Group Parameter Validation
   The ASN.1 for DH keys in [PKIX] includes elements j and validation-
   Parms which MAY be used by recipients of a key to verify that the
   group parameters were correctly generated. Two checks are possible:
     1. Verify that p=qj + 1. This demonstrates that the parameters meet
        the X9.42 parameter criteria.
     2. Verify that when the p,q generation procedure of [FIPS-186]
        Appendix 2 is followed with seed 'seed', that p is found when
        'counter' = pgenCounter.
     This demonstrates that the parameters were randomly chosen and
     do not have a special form.
   Whether agents provide validation information in their certificates
   is a local matter between the agents and their CA.
}
function DHIsGeneratedGroupParameterValid(const State: TDHState): Boolean;
var T : HugeWord;
    D : TDHState;
begin
  Assert(not HugeWordIsZero(State.P));
  Assert(not HugeWordIsZero(State.Q));
  Assert(not HugeWordIsZero(State.J));
  Assert(not HugeWordIsZero(State.Seed));

  HugeWordInit(T);
  try
    HugeWordMultiply(T, State.Q, State.J);
    HugeWordInc(T);
    // validation 1
    if not HugeWordEquals(T, State.P) then
      Result := False
    else
      begin
        // validation 2
        DHStateInit(D);
        try
          DHInitHashAlgorithm(D, State.HashAlgorithm);
          HugeWordAssign(D.Seed, State.Seed);
          DHGeneratePrimesPQ(D, State.PrimeQBitCount, State.PrimePBitCount, True);
          if (D.Counter <> State.Counter) then
            Result := False
          else
            if not HugeWordEquals(D.P, State.P) then
              Result := False
            else
              Result := True;
        finally
          DHStateFinalise(D);
        end;
      end;
  finally
    SecureHugeWordFinalise(T);
  end;
end;

{ X9.42 requires that the private key x be in the interval [2, (q - 2)].
  x should be randomly generated in this interval. y is then computed by
  calculating g^x mod p.

  To comply with this memo, m MUST
  be >=160 bits in length, (consequently, q MUST be at least 160 bits
  long). When symmetric ciphers stronger than DES are to be used, a
  larger m may be advisable. p must be a minimum of 512 bits long.
}
procedure DHGeneratePrivateKeyX(var State: TDHState);
var
  X, M : HugeWord;
  L, N, I : Integer;
begin
  Assert(State.PrimeQBitCount >= 160);
  Assert(State.PrimePBitCount >= 512);
  Assert(not HugeWordIsZero(State.Q));

  HugeWordInit(X);
  HugeWordInit(M);
  try
    HugeWordAssign(M, State.Q);  //// Use State.PrimeQBitCount instead
    HugeWordSubtractWord32(M, 2);
    N := HugeWordGetSizeInBits(M);
    while (N > 0) and not HugeWordIsBitSet(M, N - 1) do
      Dec(N);
    repeat
      L := (N + HugeWordElementBits - 1) div HugeWordElementBits;
      SecureHugeWordRandom(X, L);
      for I := N to L * HugeWordElementBits - 1 do
        HugeWordClearBit(X, I);
    until (HugeWordCompareWord32(X, 2) > 0) and
          (HugeWordCompare(X, M) < 0);
    HugeWordAssign(State.X, X);
  finally
    SecureHugeWordFinalise(M);
    SecureHugeWordFinalise(X);
  end;
end;

{ ya is party a's public key; ya = g^xa mod p }
procedure DHGeneratePublicKeyY(var State: TDHState);
begin
  Assert(not HugeWordIsZero(State.P));
  Assert(not HugeWordIsZero(State.G));
  Assert(not HugeWordIsZero(State.X));

  HugeWordPowerAndMod(State.Y, State.G, State.X, State.P);
end;

{ Recommended maximum QBitCount for given PBitCount }
function DHQBitCount(const PBitCount: Integer): Integer;
begin
  if PBitCount < 512 then
    raise EDH.Create(SInvalidParameter);
  if PBitCount <= 2048 then
    Result := 256
  else
  if PBitCount <= 4096 then
    Result := 512
  else
    Result := 768;
end;

{ Generate private / public key pair }
procedure DHGenerateKeys(var State: TDHState;
          const HashAlgorithm: TDHHashAlgorithm;
          const PrimeQBitCount, PrimePBitCount: Integer);
begin
  DHInitHashAlgorithm(State, HashAlgorithm);
  DHGeneratePrimesPQ(State, PrimeQBitCount, PrimePBitCount, False);
  DHGenerateG(State);
  DHGeneratePrivateKeyX(State);
  DHGeneratePublicKeyY(State);
end;

{ Derive keys from parameters }
procedure DHDeriveKeysFromGroupParametersPGQ(var State: TDHState;
          const HashAlgorithm: TDHHashAlgorithm;
          const PrimeQBitCount, PrimePBitCount: Integer;
          const P, G, Q: HugeWord);
begin
  DHInitHashAlgorithm(State, HashAlgorithm);

  State.PrimeQBitCount := PrimeQBitCount;
  State.PrimePBitCount := PrimePBitCount;

  HugeWordAssign(State.P, P);
  HugeWordAssign(State.G, G);
  HugeWordAssign(State.Q, Q);

  DHGeneratePrivateKeyX(State);
  DHGeneratePublicKeyY(State);
end;

procedure DHDeriveKeysFromGroupParametersPG(var State: TDHState;
          const HashAlgorithm: TDHHashAlgorithm;
          const PrimeQBitCount, PrimePBitCount: Integer;
          const P, G: HugeWord);
begin
  Assert(PrimeQBitCount >= 160);
  Assert(PrimePBitCount > PrimeQBitCount);

  DHInitHashAlgorithm(State, HashAlgorithm);

  State.PrimePBitCount := PrimePBitCount;
  DHGeneratePrimeQ(State, PrimeQBitCount, False);

  HugeWordAssign(State.P, P);
  HugeWordAssign(State.G, G);

  DHGeneratePrivateKeyX(State);
  DHGeneratePublicKeyY(State);
end;

procedure DHDeriveKeysFromGroupParameterP1(var State: TDHState;
          const HashAlgorithm: TDHHashAlgorithm;
          const PrimeQBitCount, PrimePBitCount: Integer;
          const P: HugeWord);
begin
  Assert(PrimeQBitCount >= 160);
  Assert(PrimePBitCount > PrimeQBitCount);

  DHInitHashAlgorithm(State, HashAlgorithm);

  State.PrimePBitCount := PrimePBitCount;
  DHGeneratePrimeQ(State, PrimeQBitCount, False);

  HugeWordAssign(State.P, P);
end;

procedure DHDeriveKeysFromGroupParameterP2(var State: TDHState);
begin
  DHGeneratePrivateKeyX(State);
  DHGeneratePublicKeyY(State);
end;

{ X9.42 defines that the shared secret ZZ is generated as follows:
  ZZ = g ^ (xb * xa) mod p
  Note that the individual parties actually perform the computations:
      ZZ = (yb ^ xa)  mod p  = (ya ^ xb)  mod p
  where ^ denotes exponentiation
      ya is party a's public key; ya = g ^ xa mod p
      yb is party b's public key; yb = g ^ xb mod p
      xa is party a's private key
      xb is party b's private key }
procedure DHGenerateSharedSecretZZ(
          var State: TDHState;
          const RemotePublicKeySize: Integer;
          const RemotePublicKey: HugeWord);
begin
  Assert(RemotePublicKeySize > 0);
  Assert(not HugeWordIsZero(State.P));
  Assert(not HugeWordIsZero(State.X));
  Assert(not HugeWordIsZero(RemotePublicKey));

  State.RemoteKeySize := RemotePublicKeySize;
  HugeWordAssign(State.RemoteKey, RemotePublicKey);
  HugeWordPowerAndMod(State.ZZ, State.RemoteKey, State.X, State.P);
end;

{ Verify that y lies within the interval [2,p-1]. If it does not, the key is invalid.
  Compute y^q mod p. If the result == 1, the key is valid. Otherwise the key is invalid. }
function DHIsPublicKeyValid(const State: TDHState; const Y: HugeWord): Boolean;
var T : HugeWord;
begin
  Assert(not HugeWordIsZero(State.P));
  Assert(not HugeWordIsZero(State.Q));

  if HugeWordCompareWord32(Y, 2) <= 0 then
    Result := False
  else
  if HugeWordCompare(Y, State.P) >= 0 then
    Result := False
  else
    begin
      HugeWordInit(T);
      try
        HugeWordPowerAndMod(T, Y, State.Q, State.P);
        Result := HugeWordIsOne(T);
      finally
        HugeWordFinalise(T);
      end;
    end;
end;

function DHHugeWordKeyEncodeBytes(const A: HugeWord): RawByteString;
var L, N, I : Integer;
    S : RawByteString;
    P, Q : PByte;
begin
  L := A.Used;
  if L = 0 then
    begin
      Result := '';
      exit;
    end;
  N := L * HugeWordElementSize;
  SetLength(S, N);
  P := A.Data;
  Inc(P, N - 1);
  Q := @S[1];
  for I := 0 to N - 1 do
    begin
      Q^ := P^;
      Inc(Q);
      Dec(P);
    end;
  I := 1;
  while (I <= N) and (S[I] = #0) do
    Inc(I);
  Dec(I);
  if I > 0 then
    if I = N then
      S := #0
    else
      Delete(S, 1, I);
  Result := S;
end;

procedure DHHugeWordKeyDecodeBytes(var A: HugeWord; const B: RawByteString);
var L, N, I : Integer;
    P, Q : PByte;
begin
  L := Length(B);
  if L = 0 then
    begin
      HugeWordAssignZero(A);
      exit;
    end;
  N := (L + HugeWordElementSize - 1) div HugeWordElementSize;
  HugeWordSetSize(A, N);
  P := @B[1];
  Inc(P, L - 1);
  Q := A.Data;
  for I := 0 to L - 1 do
    begin
      Q^ := P^;
      Inc(Q);
      Dec(P);
    end;
  for I := L to N * HugeWordElementSize - 1 do
    begin
      Q^ := 0;
      Inc(Q);
    end;
end;

{ OtherInfo ::= SEQUENCE [
      keyInfo KeySpecificInfo,
      partyAInfo [0] OCTET STRING OPTIONAL,
      suppPubInfo [2] OCTET STRING ]
  KeySpecificInfo ::= SEQUENCE [
      algorithm OBJECT IDENTIFIER,
      counter OCTET STRING SIZE (4..4) ]
  algorithm is the ASN.1 algorithm OID of the CEK wrapping algorithm
      with which this KEK will be used. Note that this is NOT an
      AlgorithmIdentifier, but simply the OBJECT IDENTIFIER. No parameters
      are used.
  counter is a 32 bit number, represented in network byte order. Its
      initial value is 1 for any ZZ, i.e. the byte sequence 00 00 00 01
      (hex), and it is incremented by one every time the above key
      generation function is run for a given KEK.
  partyAInfo is a random string provided by the sender. In CMS, it is
      provided as a parameter in the UserKeyingMaterial field (encoded as
      an OCTET STRING). If provided, partyAInfo MUST contain 512 bits.
  suppPubInfo is the length of the generated KEK, in bits, represented
      as a 32 bit number in network byte order. E.g. for 3DES it would be
      the byte sequence 00 00 00 C0. }
function DHGenerateOtherInfo(
         const CipherOID: array of Integer;
         const CipherKeyBits: Integer;
         const KEKCounter: Integer;
         const PartyAInfo: RawByteString): RawByteString;
var S : RawByteString;
begin
  Assert(CipherKeyBits > 0);

  S := ASN1EncodeSequence(
         ASN1EncodeOID(CipherOID) +
         ASN1EncodeInt32AsOctetString(KEKCounter)
       );
  if PartyAInfo <> '' then
    S := S +
      ASN1EncodeObj($A0,
        ASN1EncodeOctetString(PartyAInfo)
      );
  S := S +
    ASN1EncodeObj($A2,
      ASN1EncodeInt32AsOctetString(CipherKeyBits)
    );
  Result := ASN1EncodeSequence(S);
end;

{  KM = H ( ZZ || OtherInfo)
   H is the message digest function SHA-1 [FIPS-180] ZZ is the shared
   secret value computed in Section 2.1.1. Leading zeros MUST be
   preserved, so that ZZ occupies as many octets as p. For instance, if
   p is 1024 bits, ZZ should be 128 bytes long. }
procedure DHGenerateKM(
          var State: TDHState;
          const CipherOID: array of Integer;
          const CipherKeyBits: Integer;
          const KEKCounter: Integer;
          const PartyAInfo: RawByteString;
          var KM: T160BitDigest);
var S, T : RawByteString;
    L : Integer;
begin
  Assert(CipherKeyBits > 0);
  Assert(State.PrimePBitCount > 0);
  Assert(not HugeWordIsZero(State.ZZ));

  // ZZ
  L := State.PrimePBitCount div 8;
  SetLength(S, L);
  HugeWordSetSize(State.ZZ, L div 4);
  Move(State.ZZ.Data^, S[1], L);
  // OtherInfo
  T := S +
       DHGenerateOtherInfo(CipherOID, CipherKeyBits, KEKCounter, PartyAInfo);
  // KM
  KM := CalcSHA1(T);
end;

{  Each key encryption algorithm requires a specific size key (n). The
   KEK is generated by mapping the left n-most bytes of KM onto the key.
   For 3DES, which requires 192 bits of keying material, the algorithm
   must be run twice, once with a counter value of 1 (to generate K1',
   K2', and the first 32 bits of K3') and once with a counter value of 2
   (to generate the last 32 bits of K3). K1',K2' and K3' are then parity
   adjusted to generate the 3 DES keys K1,K2 and K3.  For RC2-128, which
   requires 128 bits of keying material, the algorithm is run once, with
   a counter value of 1, and the left-most 128 bits are directly
   converted to an RC2 key. Similarly, for RC2-40, which requires 40
   bits of keying material, the algorithm is run once, with a counter
   value of 1, and the leftmost 40 bits are used as the key. }
procedure DHGenerateKEK(
          var State: TDHState;
          const CipherOID: array of Integer;
          const CipherKeyBits: Integer;
          const PartyAInfo: RawByteString);
var N, I, L : Integer;
    KEK : RawByteString;
    KMS : RawByteString;
    KM : T160BitDigest;
begin
  Assert(CipherKeyBits > 0);

  Assert(State.HashAlgorithm = dhhSHA1);
  Assert(State.HashBitCount = 160);

  KEK := '';
  L := CipherKeyBits;
  N := (L + 159) div 160;
  for I := 0 to N - 1 do
    begin
      DHGenerateKM(State, CipherOID, CipherKeyBits, I + 1, PartyAInfo, KM);
      SetLength(KMS, 20);
      Move(KM, KMS[1], 20);
      if L < 160 then
        SetLength(KMS, L div 8);
      KEK := KEK + KMS;
      Dec(L, 160);
    end;
  State.KEK := KEK;
end;



{                                                                              }
{ Test cases                                                                   }
{                                                                              }
{$IFDEF CIPHER_TEST}
{$ASSERTIONS ON}
procedure Test_OtherInfo;
var S : RawByteString;
    PaI, PaE : RawByteString;
begin
  // RFC 2631 - 2.1.6. Example 1 - OtherInfo encoding
  S := DHGenerateOtherInfo(OID_3DES_wrap, 192, 1, '');
  Assert(S =
    #$30#$1d +
        #$30#$13 +
            #$06#$0b#$2a#$86#$48#$86#$f7#$0d#$01#$09#$10#$03#$06 +
            #$04#$04 +
                #$00#$00#$00#$01 +
        #$a2#$06 +
            #$04#$04 +
                #$00#$00#$00#$c0);

  // RFC 2631 - 2.1.7. Example 2 - OtherInfo encoding
  PaI := RawByteString(
      #$01#$23#$45#$67#$89#$ab#$cd#$ef#$fe#$dc#$ba#$98#$76#$54#$32#$01 +
      #$01#$23#$45#$67#$89#$ab#$cd#$ef#$fe#$dc#$ba#$98#$76#$54#$32#$01 +
      #$01#$23#$45#$67#$89#$ab#$cd#$ef#$fe#$dc#$ba#$98#$76#$54#$32#$01 +
      #$01#$23#$45#$67#$89#$ab#$cd#$ef#$fe#$dc#$ba#$98#$76#$54#$32#$01);
  S := DHGenerateOtherInfo(OID_RC2_wrap, 128, 1, PaI);
  PaE := RawByteString(
      #$30#$61 +
          #$30#$13 +
              #$06#$0b#$2a#$86#$48#$86#$f7#$0d#$01#$09#$10#$03#$07 +
                  #$04#$04 +
                      #$00#$00#$00#$01 +
      #$a0#$42 +
          #$04#$40 +
              #$01#$23#$45#$67#$89#$ab#$cd#$ef#$fe#$dc#$ba#$98#$76#$54#$32#$01 +
              #$01#$23#$45#$67#$89#$ab#$cd#$ef#$fe#$dc#$ba#$98#$76#$54#$32#$01 +
              #$01#$23#$45#$67#$89#$ab#$cd#$ef#$fe#$dc#$ba#$98#$76#$54#$32#$01 +
              #$01#$23#$45#$67#$89#$ab#$cd#$ef#$fe#$dc#$ba#$98#$76#$54#$32#$01 +
      #$a2#$06 +
          #$04#$04 +
              #$00#$00#$00#$80);
  Assert(S = PaE);
end;

procedure Test_KM;
var Da : TDHState;
    S : RawByteString;
    ZZ : RawByteString;
    PaI : RawByteString;
    KM : T160BitDigest;
begin
  // RFC 2631 - 2.1.7. Example 2 - KM calculation
  DHStateInit(Da);
  try
    DHInitHashAlgorithm(Da, dhhSHA1);
    Da.PrimePBitCount := 160;
    PaI := RawByteString(
        #$01#$23#$45#$67#$89#$ab#$cd#$ef#$fe#$dc#$ba#$98#$76#$54#$32#$01 +
        #$01#$23#$45#$67#$89#$ab#$cd#$ef#$fe#$dc#$ba#$98#$76#$54#$32#$01 +
        #$01#$23#$45#$67#$89#$ab#$cd#$ef#$fe#$dc#$ba#$98#$76#$54#$32#$01 +
        #$01#$23#$45#$67#$89#$ab#$cd#$ef#$fe#$dc#$ba#$98#$76#$54#$32#$01);
    ZZ := #$00#$01#$02#$03#$04#$05#$06#$07#$08#$09#$0a#$0b#$0c#$0d#$0e#$0f#$10#$11#$12#$13;
    HugeWordAssignBufStrB(Da.ZZ, ZZ, False);
    DHGenerateKM(Da, OID_RC2_wrap, 128, 1, PaI, KM);
    S := DigestToBufB(KM, 20);
    Assert(S = #$48#$95#$0c#$46#$e0#$53#$00#$75#$40#$3c#$ce#$72#$88#$96#$04#$e0#$3e#$7b#$5d#$e9);
  finally
    DHStateFinalise(Da);
  end;
end;

procedure Test_1;
const
  QC = 180;
  PC = 512;
var Da, Db : TDHState;
    Za, Zb : RawByteString;
begin
  // Test: key generation, shared secret, KEK
  DHStateInit(Da);
  DHStateInit(Db);
  try
    // generate group parameters and keys for A
    DHGenerateKeys(Da, dhhSHA1, QC, PC);

    // validate generated group parameters
    Assert(DHIsGeneratedGroupParameterValid(Da));

    // generate keys for B
    DHDeriveKeysFromGroupParametersPGQ(Db, dhhSHA1, Da.PrimeQBitCount, Da.PrimePBitCount,
        Da.P, Da.G, Da.Q);

    // A validates public keys of A and B
    Assert(DHIsPublicKeyValid(Da, Da.Y));
    Assert(DHIsPublicKeyValid(Da, Db.Y));

    // B validates public keys of A and B
    Assert(DHIsPublicKeyValid(Db, Da.Y));
    Assert(DHIsPublicKeyValid(Db, Db.Y));

    // A & B generate shared secret from remote public key
    DHGenerateSharedSecretZZ(Da, PC, Db.Y);
    DHGenerateSharedSecretZZ(Db, PC, Da.Y);
    Za := HugeWordToStrB(Da.ZZ);
    Zb := HugeWordToStrB(Db.ZZ);
    Assert(Za = Zb);

    // A & B generate KEK
    DHGenerateKEK(Da, OID_RC2_wrap, PC, '');
    DHGenerateKEK(Db, OID_RC2_wrap, PC, '');
    Assert(Length(Da.KEK) = PC div 8);
    Assert(Length(Db.KEK) = PC div 8);
    Assert(Da.KEK = Db.KEK);
  finally
    DHStateFinalise(Db);
    DHStateFinalise(Da);
  end;
end;

procedure Test_2;
var WK : PDHWellKnownGroup;
    Da, Db : TDHState;
    Za, Zb : RawByteString;
    I, PC, QC : Integer;
    ValP, ValG : HugeWord;
begin
  // Test well known DH groups
  for I := 0 to 1 do // DHWellKnownGroups - 1 do
    begin
      WK := @DHWellKnownGroup[I];

      HugeWordInit(ValP);
      HugeWordInit(ValG);
      try
        HexToHugeWordB(WK^.P_Hex, ValP);
        Assert((HugeWordToHexB(ValP, False) = WK^.P_Hex) or
               (HugeWordToHexB(ValP, True) = WK^.P_Hex));

        Assert(HugeWordIsPrime(ValP) <> pNotPrime);

        if WK^.G_Hex <> '' then
          begin
            HexToHugeWordB(WK^.G_Hex, ValG);
            Assert((HugeWordToHexB(ValG, False) = WK^.G_Hex) or
                   (HugeWordToHexB(ValG, True) = WK^.G_Hex));
          end;

        PC := WK^.PBitCount;
        QC := WK^.QBitCount;

        // Test key derivation and shared secret generation from group params
        DHStateInit(Da);
        DHStateInit(Db);
        try
          if WK^.G_Hex = '' then
            begin
              DHDeriveKeysFromGroupParameterP1(Da, dhhSHA1, QC, PC, ValP);

              DHInitHashAlgorithm(Db, dhhSHA1);
              Db.PrimePBitCount := Da.PrimePBitCount;
              Db.PrimeQBitCount := Da.PrimeQBitCount;
              HugeWordAssign(Db.Q, Da.Q);
              HugeWordAssign(Db.P, Da.P);

              DHGenerateG(Da);
              HugeWordAssign(Db.G, Da.G);

              DHDeriveKeysFromGroupParameterP2(Da);
              DHDeriveKeysFromGroupParameterP2(Db);
            end
          else
            begin
              DHDeriveKeysFromGroupParametersPG(Da, dhhSHA1, QC, PC, ValP, ValG);

              DHInitHashAlgorithm(Db, dhhSHA1);
              Db.PrimePBitCount := Da.PrimePBitCount;
              Db.PrimeQBitCount := Da.PrimeQBitCount;
              HugeWordAssign(Db.P, Da.P);
              HugeWordAssign(Db.G, Da.G);
              HugeWordAssign(Db.Q, Da.Q);

              DHGeneratePrivateKeyX(Db);
              DHGeneratePublicKeyY(Db);
            end;

          Assert(not HugeWordEquals(Da.X, Db.X));
          Assert(not HugeWordEquals(Da.Y, Db.Y));

          //Assert(DHIsPublicKeyValid(Da, Da.Y));
          //Assert(DHIsPublicKeyValid(Da, Db.Y));
          //Assert(DHIsPublicKeyValid(Db, Da.Y));
          //Assert(DHIsPublicKeyValid(Db, Db.Y));

          DHGenerateSharedSecretZZ(Da, PC, Db.Y);
          DHGenerateSharedSecretZZ(Db, PC, Da.Y);

          Za := HugeWordToStrB(Da.ZZ);
          Zb := HugeWordToStrB(Db.ZZ);
          Assert(Za = Zb);

          DHGenerateKEK(Da, OID_RC2_wrap, QC, '');
          DHGenerateKEK(Db, OID_RC2_wrap, QC, '');
          Assert(Length(Da.KEK) = QC div 8);
          Assert(Length(Db.KEK) = QC div 8);
          Assert(Da.KEK = Db.KEK);
        finally
          DHStateFinalise(Db);
          DHStateFinalise(Da);
        end;
      finally
        HugeWordFinalise(ValG);
        HugeWordFinalise(ValP);
      end;
    end;
end;

type
  TTest3Case = record
    WellKnownIdx : Integer;
    XA, YA, XB, YB, Z : RawByteString;
  end;
  PTest3Case = ^TTest3Case;

const
  Test3Cases = 3;
  Test3Case: array[0..Test3Cases - 1] of TTest3Case = (
      ( // RFC 5114 A.1.
      WellKnownIdx: 8;
      XA: 'B9A3B3AE8FEFC1A2930496507086F8455D48943E';
      YA: '2A853B3D92197501' +
          'B9015B2DEB3ED84F5E021DCC3E52F109D3273D2B7521281C' +
          'BABE0E76FF5727FA8ACCE26956BA9A1FCA26F20228D8693F' +
          'EB10841D84A7360054ECE5A7F5B7A61AD3DFB3C60D2E4310' +
          '6D8727DA37DF9CCE95B478755D06BCEA8F9D45965F75A5F3' +
          'D1DF3701165FC9E50C4279CEB07F989540AE96D5D88ED776';
      XB: '9392C9F9EB6A7A6A9022F7D83E7223C6835BBDDA';
      YB: '717A6CB053371FF4' +
          'A3B932941C1E5663F861A1D6AD34AE66576DFB98F6C6CBF9' +
          'DDD5A56C7833F6BCFDFF095582AD868E440E8D09FD769E3C' +
          'ECCDC3D3B1E4CFA057776CAAF9739B6A9FEE8E7411F8D6DA' +
          'C09D6A4EDB46CC2B5D5203090EAE6126311E53FD2C14B574' +
          'E6A3109A3DA1BE41BDCEAA186F5CE06716A2B6A07B3C33FE';
      Z : '5C804F454D30D9C4' +
          'DF85271F93528C91DF6B48AB5F80B3B59CAAC1B28F8ACBA9' +
          'CD3E39F3CB614525D9521D2E644C53B807B810F340062F25' +
          '7D7D6FBFE8D5E8F072E9B6E9AFDA9413EAFB2E8B0699B1FB' +
          '5A0CACEDDEAEAD7E9CFBB36AE2B420835BD83A19FB0B5E96' +
          'BF8FA4D09E345525167ECD9155416F46F408ED31B63C6E6D'
      ),
      ( // RFC 5114 A.2.
      WellKnownIdx: 9;
      XA: '22E62601' +
          'DBFFD06708A680F747F361F76D8F4F721A0548E483294B0C';
      YA: '1B3A63451BD886E699E67B494E288BD7' +
          'F8E0D370BADDA7A0EFD2FDE7D8F66145CC9F280419975EB8' +
          '08877C8A4C0C8E0BD48D4A5401EB1E8776BFEEE134C03831' +
          'AC273CD9D635AB0CE006A42A887E3F52FB8766B650F38078' +
          'BC8EE8580CEFE243968CFC4F8DC3DB084554171D41BF2E86' +
          '1B7BB4D69DD0E01EA387CBAA5CA672AFCBE8BDB9D62D4CE1' +
          '5F17DD36F91ED1EEDD65CA4A06455CB94CD40A52EC360E84' +
          'B3C926E22C4380A3BF309D56849768B7F52CFDF655FD053A' +
          '7EF706979E7E5806B17DFAE53AD2A5BC568EBB529A7A61D6' +
          '8D256F8FC97C074A861D827E2EBC8C6134553115B70E7103' +
          '920AA16D85E52BCBAB8D786A68178FA8FF7C2F5C71648D6F';
      XB: '4FF3BC96' +
          'C7FC6A6D71D3B363800A7CDFEF6FC41B4417EA15353B7590';
      YB: '4DCEE992A9762A13F2F83844AD3D77EE' +
          '0E31C9718B3DB6C2035D3961182C3E0BA247EC4182D760CD' +
          '48D99599970622A1881BBA2DC822939C78C3912C6661FA54' +
          '38B20766222B75E24C2E3AD0C7287236129525EE15B5DD79' +
          '98AA04C4A9696CACD7172083A97A81664EAD2C479E444E4C' +
          '0654CC19E28D7703CEE8DACD6126F5D665EC52C67255DB92' +
          '014B037EB621A2AC8E365DE071FFC1400ACF077A12913DD8' +
          'DE89473437AB7BA346743C1B215DD9C12164A7E4053118D1' +
          '99BEC8EF6FC561170C84C87D10EE9A674A1FA8FFE13BDFBA' +
          '1D44DE48946D68DC0CDD777635A7AB5BFB1E4BB7B856F968' +
          '27734C184138E915D9C3002EBCE53120546A7E2002142B6C';
      Z : '34D9BDDC1B42176C313FEA034C21034D' +
          '074A6313BB4ECDB3703FFF424567A46BDF75530EDE0A9DA5' +
          '229DE7D76732286CBC0F91DA4C3C852FC099C679531D94C7' +
          '8AB03D9DECB0A4E4CA8B2BB4591C4021CF8CE3A20A541D33' +
          '994017D0200AE2C9516E2FF5145779269E862B0FB474A2D5' +
          '6DC31ED569A7700B4C4AB16B22A45513531EF523D7121207' +
          '7B5A169BDEFFAD7AD9608284C7795B6D5A5183B87066DE17' +
          'D8D671C9EBD8EC89544D45EC061593D442C62AB9CE3B1CB9' +
          '943A1D23A5EA3BCF21A01471E67E003E7F8A69C728BE490B' +
          '2FC88CFEB92DB6A215E5D03C17C464C9AC1A46E203E13F95' +
          '2995FB03C69D3CC47FCB510B6998FFD3AA6DE73CF9F63869';
      ),
      ( // RFC 5114 A.3.
      WellKnownIdx: 10;
      XA: '0881382CDB87660C' +
          '6DC13E614938D5B9C8B2F248581CC5E31B35454397FCE50E';
      YA: '2E9380C8323AF97545BC4941DEB0EC37' +
          '42C62FE0ECE824A6ABDBE66C59BEE0242911BFB967235CEB' +
          'A35AE13E4EC752BE630B92DC4BDE2847A9C62CB815274542' +
          '1FB7EB60A63C0FE9159FCCE726CE7CD8523D7450667EF840' +
          'E4919121EB5F01C8C9B0D3D648A93BFB75689E8244AC134A' +
          'F544711CE79A02DCC34226684780DDDCB498594106C37F5B' +
          'C79856487AF5AB022A2E5E42F09897C1A85A11EA0212AF04' +
          'D9B4CEBC937C3C1A3E15A8A0342E337615C84E7FE3B8B9B8' +
          '7FB1E73A15AF12A30D746E06DFC34F290D797CE51AA13AA7' +
          '85BF6658AFF5E4B093003CBEAF665B3C2E113A3A4E905269' +
          '341DC0711426685F4EF37E868A8126FF3F2279B57CA67E29';
      XB: '7D62A7E3EF36DE61' +
          '7B13D1AFB82C780D83A23BD4EE6705645121F371F546A53D';
      YB: '575F0351BD2B1B817448BDF87A6C362C' +
          '1E289D3903A30B9832C5741FA250363E7ACBC7F77F3DACBC' +
          '1F131ADD8E03367EFF8FBBB3E1C5784424809B25AFE4D226' +
          '2A1A6FD2FAB64105CA30A674E07F7809852088632FC04923' +
          '3791AD4EDD083A978B883EE618BC5E0DD047415F2D95E683' +
          'CF14826B5FBE10D3CE41C6C120C78AB20008C698BF7F0BCA' +
          'B9D7F407BED0F43AFB2970F57F8D12043963E66DDD320D59' +
          '9AD9936C8F44137C08B180EC5E985CEBE186F3D549677E80' +
          '607331EE17AF3380A725B0782317D7DD43F59D7AF9568A9B' +
          'B63A84D365F92244ED120988219302F42924C7CA90B89D24' +
          'F71B0AB697823D7DEB1AFF5B0E8E4A45D49F7F53757E1913';
      Z : '86C70BF8D0BB81BB01078A17219CB7D2' +
          '7203DB2A19C877F1D1F19FD7D77EF22546A68F005AD52DC8' +
          '4553B78FC60330BE51EA7C0672CAC1515E4B35C047B9A551' +
          'B88F39DC26DA14A09EF74774D47C762DD177F9ED5BC2F11E' +
          '52C879BD95098504CD9EECD8A8F9B3EFBD1F008AC5853097' +
          'D9D1837F2B18F77CD7BE01AF80A7C7B5EA3CA54CC02D0C11' +
          '6FEE3F95BB87399385875D7E86747E676E728938ACBFF709' +
          '8E05BE4DCFB24052B83AEFFB14783F029ADBDE7F53FAE920' +
          '84224090E007CEE94D4BF2BACE9FFD4B57D2AF7C724D0CAA' +
          '19BF0501F6F17B4AA10F425E3EA76080B4B9D6B3CEFEA115' +
          'B2CEB8789BB8A3B0EA87FEBE63B6C8F846EC6DB0C26C5D7C';
      )
      );

procedure Test_3;
var Tst : PTest3Case;
    WK : PDHWellKnownGroup;
    Da, Db : TDHState;
    Za, Zb, Zz : RawByteString;
    I, PC, QC : Integer;
    ValP, ValG : HugeWord;
begin
  for I := 0 to Test3Cases - 1 do
    begin
      Tst := @Test3Case[I];
      WK := @DHWellKnownGroup[Tst^.WellKnownIdx];

      HugeWordInit(ValP);
      HugeWordInit(ValG);
      try
        HexToHugeWordB(WK^.P_Hex, ValP);
        HexToHugeWordB(WK^.G_Hex, ValG);

        PC := WK^.PBitCount;
        QC := WK^.QBitCount;

        // Test shared secret generation using test keys
        DHStateInit(Da);
        DHStateInit(Db);
        try
          DHInitHashAlgorithm(Da, dhhSHA1);
          HugeWordAssign(Da.P, ValP);
          HugeWordAssign(Da.G, ValG);
          Da.PrimePBitCount := PC;
          Da.PrimeQBitCount := QC;
          HexToHugeWordB(Tst^.XA, Da.X);
          HexToHugeWordB(Tst^.YA, Da.Y);

          DHInitHashAlgorithm(Db, dhhSHA1);
          HugeWordAssign(Db.P, ValP);
          HugeWordAssign(Db.G, ValG);
          Db.PrimePBitCount := PC;
          Db.PrimeQBitCount := QC;
          HexToHugeWordB(Tst^.XB, Db.X);
          HexToHugeWordB(Tst^.YB, Db.Y);

          DHGenerateSharedSecretZZ(Da, PC, Db.Y);
          DHGenerateSharedSecretZZ(Db, PC, Da.Y);

          Za := HugeWordToStrB(Da.ZZ);
          Zb := HugeWordToStrB(Db.ZZ);
          Assert(Za = Zb);

          Zz := HugeWordToHexB(Da.ZZ, False);
          Assert(Zz = Tst^.Z);

          DHGenerateKEK(Da, OID_RC2_wrap, QC, '');
          DHGenerateKEK(Db, OID_RC2_wrap, QC, '');
          Assert(Length(Da.KEK) = QC div 8);
          Assert(Length(Db.KEK) = QC div 8);
          Assert(Da.KEK = Db.KEK);
        finally
          DHStateFinalise(Db);
          DHStateFinalise(Da);
        end;

      finally
        HugeWordFinalise(ValG);
        HugeWordFinalise(ValP);
      end;
    end;
end;

procedure Test;
begin
  Test_OtherInfo;
  Test_KM;
  Test_1;
  Test_2;
  Test_3;
end;
{$ENDIF}



end.

