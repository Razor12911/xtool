{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals 5.00                                        }
{   File name:        flcCipherTest.pas                                        }
{   File version:     5.08                                                     }
{   Description:      Cipher Test                                              }
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
{   2007/01/05  0.01  Initial version.                                         }
{   2007/01/07  0.02  ECB and padding support.                                 }
{   2008/06/15  0.03  CBC mode.                                                }
{   2008/06/17  0.04  CFB and OFB modes.                                       }
{   2010/12/16  4.05  AES cipher.                                              }
{   2016/01/09  5.06  Revised for Fundamentals 5.                              }
{   2019/06/09  5.07  Tests for Triple-DES-EDE-3.                              }
{   2020/05/20  5.08  Create flcCipherTest unit from flcCipher unit.           }
{                                                                              }
{******************************************************************************}

{$INCLUDE flcCipher.inc}

{$DEFINE Cipher_SupportEC}

unit flcCipherTest;

interface



{                                                                              }
{ Test                                                                         }
{                                                                              }
{$IFDEF CIPHER_TEST}
procedure Test;
{$ENDIF}
{$IFDEF CIPHER_PROFILE}
procedure Profile;
{$ENDIF}



implementation

uses
  SysUtils,

  flcStdTypes,
  flcUtils,
  flcCipherRandom,
  flcCipherRC2,
  flcCipherAES,
  flcCipherDES,
  flcCipherDH,
  flcCipherRSA,
  {$IFDEF Cipher_SupportEC}
  flcCipherEllipticCurve,
  {$ENDIF}
  flcCipher;



{                                                                              }
{ Test                                                                         }
{                                                                              }
{$IFDEF CIPHER_TEST}
{$ASSERTIONS ON}
type
  TCipherTestCase = record
    Cipher     : TCipherType;
    Mode       : TCipherMode;
    KeyBits    : Integer;      // Effective key length (bits)
    Key        : RawByteString;
    InitVector : RawByteString;
    PlainText  : RawByteString;
    CipherText : RawByteString;
  end;

const
  CipherTestCaseCount = 36;

var
  CipherTestCases : array[0..CipherTestCaseCount - 1] of TCipherTestCase = (
    // RC2 test vectors from RFC 2268
    (Cipher:     ctRC2;
     Mode:       cmECB;
     KeyBits:    63;
     Key:        RawByteString(#$00#$00#$00#$00#$00#$00#$00#$00);
     PlainText:  RawByteString(#$00#$00#$00#$00#$00#$00#$00#$00);
     CipherText: RawByteString(#$eb#$b7#$73#$f9#$93#$27#$8e#$ff)),
    (Cipher:     ctRC2;
     Mode:       cmECB;
     KeyBits:    64;
     Key:        RawByteString(#$ff#$ff#$ff#$ff#$ff#$ff#$ff#$ff);
     PlainText:  RawByteString(#$ff#$ff#$ff#$ff#$ff#$ff#$ff#$ff);
     CipherText: RawByteString(#$27#$8b#$27#$e4#$2e#$2f#$0d#$49)),
    (Cipher:     ctRC2;
     Mode:       cmECB;
     KeyBits:    64;
     Key:        RawByteString(#$30#$00#$00#$00#$00#$00#$00#$00);
     PlainText:  RawByteString(#$10#$00#$00#$00#$00#$00#$00#$01);
     CipherText: RawByteString(#$30#$64#$9e#$df#$9b#$e7#$d2#$c2)),
    (Cipher:     ctRC2;
     Mode:       cmECB;
     KeyBits:    64;
     Key:        RawByteString(#$88);
     PlainText:  RawByteString(#$00#$00#$00#$00#$00#$00#$00#$00);
     CipherText: RawByteString(#$61#$a8#$a2#$44#$ad#$ac#$cc#$f0)),
    (Cipher:     ctRC2;
     Mode:       cmECB;
     KeyBits:    64;
     Key:        RawByteString(#$88#$bc#$a9#$0e#$90#$87#$5a);
     PlainText:  RawByteString(#$00#$00#$00#$00#$00#$00#$00#$00);
     CipherText: RawByteString(#$6c#$cf#$43#$08#$97#$4c#$26#$7f)),
    (Cipher:     ctRC2;
     Mode:       cmECB;
     KeyBits:    64;
     Key:        RawByteString(#$88#$bc#$a9#$0e#$90#$87#$5a#$7f#$0f#$79#$c3#$84#$62#$7b#$af#$b2);
     PlainText:  RawByteString(#$00#$00#$00#$00#$00#$00#$00#$00);
     CipherText: RawByteString(#$1a#$80#$7d#$27#$2b#$be#$5d#$b1)),
    (Cipher:     ctRC2;
     Mode:       cmECB;
     KeyBits:    128;
     Key:        RawByteString(#$88#$bc#$a9#$0e#$90#$87#$5a#$7f#$0f#$79#$c3#$84#$62#$7b#$af#$b2);
     PlainText:  RawByteString(#$00#$00#$00#$00#$00#$00#$00#$00);
     CipherText: RawByteString(#$22#$69#$55#$2a#$b0#$f8#$5c#$a6)),
    (Cipher:     ctRC2;
     Mode:       cmECB;
     KeyBits:    129;
     Key:        RawByteString(#$88#$bc#$a9#$0e#$90#$87#$5a#$7f#$0f#$79#$c3#$84#$62#$7b#$af#$b2#$16#$f8#$0a#$6f#$85#$92#$05#$84#$c4#$2f#$ce#$b0#$be#$25#$5d#$af#$1e);
     PlainText:  RawByteString(#$00#$00#$00#$00#$00#$00#$00#$00);
     CipherText: RawByteString(#$5b#$78#$d3#$a4#$3d#$ff#$f1#$f1)),
    // RC4 test vectors from http://en.wikipedia.org/wiki/RC4
    (Cipher:     ctRC4;
     Mode:       cmECB;
     KeyBits:    24;
     Key:        'Key';
     PlainText:  'Plaintext';
     CipherText: RawByteString(#$BB#$F3#$16#$E8#$D9#$40#$AF#$0A#$D3)),
    (Cipher:     ctRC4;
     Mode:       cmECB;
     KeyBits:    32;
     Key:        'Wiki';
     PlainText:  'pedia';
     CipherText: RawByteString(#$10#$21#$BF#$04#$20)),
    (Cipher:     ctRC4;
     Mode:       cmECB;
     KeyBits:    48;
     Key:        'Secret';
     PlainText:  'Attack at dawn';
     CipherText: RawByteString(#$45#$A0#$1F#$64#$5F#$C3#$5B#$38#$35#$52#$54#$4B#$9B#$F5)),
    // RC4 test vectors from Internet Draft on ARCFOUR
    (Cipher:     ctRC4;
     Mode:       cmECB;
     KeyBits:    64;
     Key:        RawByteString(#$01#$23#$45#$67#$89#$AB#$CD#$EF);
     PlainText:  RawByteString(#$00#$00#$00#$00#$00#$00#$00#$00);
     CipherText: RawByteString(#$74#$94#$C2#$E7#$10#$4B#$08#$79)),
    (Cipher:     ctRC4;
     Mode:       cmECB;
     KeyBits:    40;
     Key:        RawByteString(#$61#$8a#$63#$d2#$fb);
     PlainText:  RawByteString(#$dc#$ee#$4c#$f9#$2c);
     CipherText: RawByteString(#$f1#$38#$29#$c9#$de)),
    (Cipher:     ctRC4;
     Mode:       cmECB;
     KeyBits:    128;
     Key:        RawByteString(#$29#$04#$19#$72#$fb#$42#$ba#$5f#$c7#$12#$77#$12#$f1#$38#$29#$c9);
     PlainText:  RawByteString(#$52#$75#$69#$73#$6c#$69#$6e#$6e#$75#$6e#$20#$6c#$61#$75#$6c#$75 +
                 #$20#$6b#$6f#$72#$76#$69#$73#$73#$73#$61#$6e#$69#$2c#$20#$74#$e4 +
                 #$68#$6b#$e4#$70#$e4#$69#$64#$65#$6e#$20#$70#$e4#$e4#$6c#$6c#$e4 +
                 #$20#$74#$e4#$79#$73#$69#$6b#$75#$75#$2e#$20#$4b#$65#$73#$e4#$79 +
                 #$f6#$6e#$20#$6f#$6e#$20#$6f#$6e#$6e#$69#$20#$6f#$6d#$61#$6e#$61 +
                 #$6e#$69#$2c#$20#$6b#$61#$73#$6b#$69#$73#$61#$76#$75#$75#$6e#$20 +
                 #$6c#$61#$61#$6b#$73#$6f#$74#$20#$76#$65#$72#$68#$6f#$75#$75#$2e +
                 #$20#$45#$6e#$20#$6d#$61#$20#$69#$6c#$6f#$69#$74#$73#$65#$2c#$20 +
                 #$73#$75#$72#$65#$20#$68#$75#$6f#$6b#$61#$61#$2c#$20#$6d#$75#$74 +
                 #$74#$61#$20#$6d#$65#$74#$73#$e4#$6e#$20#$74#$75#$6d#$6d#$75#$75 +
                 #$73#$20#$6d#$75#$6c#$6c#$65#$20#$74#$75#$6f#$6b#$61#$61#$2e#$20 +
                 #$50#$75#$75#$6e#$74#$6f#$20#$70#$69#$6c#$76#$65#$6e#$2c#$20#$6d +
                 #$69#$20#$68#$75#$6b#$6b#$75#$75#$2c#$20#$73#$69#$69#$6e#$74#$6f +
                 #$20#$76#$61#$72#$61#$6e#$20#$74#$75#$75#$6c#$69#$73#$65#$6e#$2c +
                 #$20#$6d#$69#$20#$6e#$75#$6b#$6b#$75#$75#$2e#$20#$54#$75#$6f#$6b +
                 #$73#$75#$74#$20#$76#$61#$6e#$61#$6d#$6f#$6e#$20#$6a#$61#$20#$76 +
                 #$61#$72#$6a#$6f#$74#$20#$76#$65#$65#$6e#$2c#$20#$6e#$69#$69#$73 +
                 #$74#$e4#$20#$73#$79#$64#$e4#$6d#$65#$6e#$69#$20#$6c#$61#$75#$6c +
                 #$75#$6e#$20#$74#$65#$65#$6e#$2e#$20#$2d#$20#$45#$69#$6e#$6f#$20 +
                 #$4c#$65#$69#$6e#$6f);
     CipherText: RawByteString(#$35#$81#$86#$99#$90#$01#$e6#$b5#$da#$f0#$5e#$ce#$eb#$7e#$ee#$21 +
                 #$e0#$68#$9c#$1f#$00#$ee#$a8#$1f#$7d#$d2#$ca#$ae#$e1#$d2#$76#$3e +
                 #$68#$af#$0e#$ad#$33#$d6#$6c#$26#$8b#$c9#$46#$c4#$84#$fb#$e9#$4c +
                 #$5f#$5e#$0b#$86#$a5#$92#$79#$e4#$f8#$24#$e7#$a6#$40#$bd#$22#$32 +
                 #$10#$b0#$a6#$11#$60#$b7#$bc#$e9#$86#$ea#$65#$68#$80#$03#$59#$6b +
                 #$63#$0a#$6b#$90#$f8#$e0#$ca#$f6#$91#$2a#$98#$eb#$87#$21#$76#$e8 +
                 #$3c#$20#$2c#$aa#$64#$16#$6d#$2c#$ce#$57#$ff#$1b#$ca#$57#$b2#$13 +
                 #$f0#$ed#$1a#$a7#$2f#$b8#$ea#$52#$b0#$be#$01#$cd#$1e#$41#$28#$67 +
                 #$72#$0b#$32#$6e#$b3#$89#$d0#$11#$bd#$70#$d8#$af#$03#$5f#$b0#$d8 +
                 #$58#$9d#$bc#$e3#$c6#$66#$f5#$ea#$8d#$4c#$79#$54#$c5#$0c#$3f#$34 +
                 #$0b#$04#$67#$f8#$1b#$42#$59#$61#$c1#$18#$43#$07#$4d#$f6#$20#$f2 +
                 #$08#$40#$4b#$39#$4c#$f9#$d3#$7f#$f5#$4b#$5f#$1a#$d8#$f6#$ea#$7d +
                 #$a3#$c5#$61#$df#$a7#$28#$1f#$96#$44#$63#$d2#$cc#$35#$a4#$d1#$b0 +
                 #$34#$90#$de#$c5#$1b#$07#$11#$fb#$d6#$f5#$5f#$79#$23#$4d#$5b#$7c +
                 #$76#$66#$22#$a6#$6d#$e9#$2b#$e9#$96#$46#$1d#$5e#$4d#$c8#$78#$ef +
                 #$9b#$ca#$03#$05#$21#$e8#$35#$1e#$4b#$ae#$d2#$fd#$04#$f9#$46#$73 +
                 #$68#$c4#$ad#$6a#$c1#$86#$d0#$82#$45#$b2#$63#$a2#$66#$6d#$1f#$6c +
                 #$54#$20#$f1#$59#$9d#$fd#$9f#$43#$89#$21#$c2#$f5#$a4#$63#$93#$8c +
                 #$e0#$98#$22#$65#$ee#$f7#$01#$79#$bc#$55#$3f#$33#$9e#$b1#$a4#$c1 +
                 #$af#$5f#$6a#$54#$7f)),
    // AES test vectors generated from online AES calculator at http://www.unsw.adfa.edu.au/~lpb/src/AEScalc/AEScalc.html
    (Cipher:     ctAES;
     Mode:       cmECB;
     KeyBits:    128;
     Key:        RawByteString(#$0f#$15#$71#$c9#$47#$d9#$e8#$59#$0c#$b7#$ad#$d6#$af#$7f#$67#$98);
     PlainText:  '1234567890123456';
     CipherText: RawByteString(#$2f#$7d#$76#$42#$5e#$bb#$85#$e4#$f2#$e7#$b0#$08#$68#$bf#$0f#$ce)),
    (Cipher:     ctAES;
     Mode:       cmECB;
     KeyBits:    128;
     Key:        RawByteString(#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00);
     PlainText:  RawByteString(#$14#$0f#$0f#$10#$11#$b5#$22#$3d#$79#$58#$77#$17#$ff#$d9#$ec#$3a);
     CipherText: RawByteString(#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00)),
    (Cipher:     ctAES;
     Mode:       cmECB;
     KeyBits:    192;
     Key:        RawByteString(#$96#$43#$D8#$33#$4A#$63#$DF#$4D#$48#$E3#$1E#$9E#$25#$67#$18#$F2#$92#$29#$31#$9C#$19#$F1#$5B#$A4);
     PlainText:  RawByteString(#$23#$00#$ea#$46#$3f#$43#$72#$64#$12#$75#$5f#$4c#$83#$e2#$cb#$78);
     CipherText: RawByteString(#$48#$E3#$1E#$9E#$25#$67#$18#$F2#$92#$29#$31#$9C#$19#$F1#$5B#$A4)),
    (Cipher:     ctAES;
     Mode:       cmECB;
     KeyBits:    256;
     Key:        RawByteString(#$85#$C6#$B2#$BB#$23#$00#$14#$8F#$94#$5A#$EB#$F1#$F0#$21#$CF#$79#$05#$8C#$CF#$FD#$BB#$CB#$38#$2D#$1F#$6F#$56#$58#$5D#$8A#$4A#$DE);
     PlainText:  RawByteString(#$e7#$0f#$9e#$09#$08#$87#$0a#$1d#$cf#$09#$60#$ae#$13#$d0#$7c#$68);
     CipherText: RawByteString(#$05#$8C#$CF#$FD#$BB#$CB#$38#$2D#$1F#$6F#$56#$58#$5D#$8A#$4A#$DE)),
    // AES test vectors generated from online AES calculator at http://www.riscure.com/tech-corner/online-crypto-tools/aes.html
    (Cipher:     ctAES;
     Mode:       cmCBC;
     KeyBits:    128;
     Key:        RawByteString(#$84#$52#$35#$BA#$BE#$BD#$14#$84#$63#$E9#$DB#$46#$74#$77#$F9#$D2);
     InitVector: RawByteString(#$01#$02#$03#$04#$05#$06#$07#$08#$09#$10#$11#$12#$13#$14#$15#$16);
     PlainText:  RawByteString(#$8F#$98#$3F#$D0#$99#$A3#$6D#$1E#$2F#$A5#$B3#$86#$31#$14#$42#$08);
     CipherText: RawByteString(#$7E#$50#$7D#$C5#$D8#$ED#$3B#$A9#$F4#$C9#$30#$C8#$13#$D4#$A7#$BC)),
    // DES test vectors from http://www.aci.net/Kalliste/des.htm
    (Cipher:     ctDES;
     Mode:       cmECB;
     KeyBits:    64;
     Key:        RawByteString(#$13#$34#$57#$79#$9B#$BC#$DF#$F1);
     PlainText:  RawByteString(#$01#$23#$45#$67#$89#$AB#$CD#$EF);
     CipherText: RawByteString(#$85#$E8#$13#$54#$0F#$0A#$B4#$05)),
    (Cipher:     ctDES;
     Mode:       cmECB;
     KeyBits:    64;
     Key:        RawByteString(#$0E#$32#$92#$32#$EA#$6D#$0D#$73);
     PlainText:  RawByteString(#$87#$87#$87#$87#$87#$87#$87#$87);
     CipherText: RawByteString(#$00#$00#$00#$00#$00#$00#$00#$00)),
    // DES test vectors from http://groups.google.com/group/sci.crypt/msg/1e08a60f44daa890?&hl=en
    (Cipher:     ctDES;
     Mode:       cmECB;
     KeyBits:    64;
     Key:        RawByteString(#$01#$01#$01#$01#$01#$01#$01#$01);
     PlainText:  RawByteString(#$95#$F8#$A5#$E5#$DD#$31#$D9#$00);
     CipherText: RawByteString(#$80#$00#$00#$00#$00#$00#$00#$00)),
    (Cipher:     ctDES;
     Mode:       cmECB;
     KeyBits:    64;
     Key:        RawByteString(#$7C#$A1#$10#$45#$4A#$1A#$6E#$57);
     PlainText:  RawByteString(#$01#$A1#$D6#$D0#$39#$77#$67#$42);
     CipherText: RawByteString(#$69#$0F#$5B#$0D#$9A#$26#$93#$9B)),
    (Cipher:     ctDES;
     Mode:       cmECB;
     KeyBits:    64;
     Key:        RawByteString(#$80#$01#$01#$01#$01#$01#$01#$01);
     PlainText:  RawByteString(#$00#$00#$00#$00#$00#$00#$00#$00);
     CipherText: RawByteString(#$95#$A8#$D7#$28#$13#$DA#$A9#$4D)),
    // DES test vectors from http://tero.co.uk/des/show.php
    (Cipher:     ctDES;
     Mode:       cmECB;
     KeyBits:    64;
     Key:        '12345678';
     PlainText:  'This is the message to encrypt!!';
     CipherText: RawByteString(#$05#$c9#$c4#$ca#$fb#$99#$37#$d9#$5b#$bf#$be#$df#$c5#$d7#$7f#$19 +
                 #$a6#$cd#$5a#$5d#$ab#$18#$8a#$33#$df#$d8#$97#$9f#$c4#$b7#$b2#$be)),
    (Cipher:     ctDES;
     Mode:       cmCBC;
     KeyBits:    64;
     Key:        '12345678';
     InitVector: 'abcdefgh';
     PlainText:  'This is the message to encrypt!!';
     CipherText: RawByteString(#$6c#$a9#$47#$0c#$84#$9d#$1c#$c1#$a5#$9f#$fc#$14#$8f#$1c#$b5#$e9 +
                 #$cf#$1f#$5c#$03#$28#$a7#$e8#$75#$63#$87#$ff#$4d#$0f#$e4#$60#$50)),
    (Cipher:     ctDES;
     Mode:       cmECB;
     KeyBits:    64;
     Key:        RawByteString(#$01#$23#$45#$67#$89#$ab#$cd#$ef);
     PlainText:  'Now is the time for all ';
     CipherText: RawByteString(#$3f#$a4#$0e#$8a#$98#$4d#$48#$15#$6a#$27#$17#$87#$ab#$88#$83#$f9 +
                 #$89#$3d#$51#$ec#$4b#$56#$3b#$53)),
    // DES test vectors from http://www.herongyang.com/crypto/des_php_implementation_mcrypt_2.html
    (Cipher:     ctDES;
     Mode:       cmCBC;
     KeyBits:    64;
     Key:        RawByteString(#$01#$23#$45#$67#$89#$ab#$cd#$ef);
     InitVector: RawByteString(#$12#$34#$56#$78#$90#$ab#$cd#$ef);
     PlainText:  RawByteString(#$4e#$6f#$77#$20#$69#$73#$20#$74#$68#$65#$20#$74#$69#$6d#$65#$20 +
                 #$66#$6f#$72#$20#$61#$6c#$6c#$20);
     CipherText: RawByteString(#$e5#$c7#$cd#$de#$87#$2b#$f2#$7c#$43#$e9#$34#$00#$8c#$38#$9c#$0f +
                 #$68#$37#$88#$49#$9a#$7c#$05#$f6)),
    (Cipher:     ctDES;
     Mode:       cmCFB;
     KeyBits:    64;
     Key:        RawByteString(#$01#$23#$45#$67#$89#$ab#$cd#$ef);
     InitVector: RawByteString(#$12#$34#$56#$78#$90#$ab#$cd#$ef);
     PlainText:  RawByteString(#$4e#$6f#$77#$20#$69#$73#$20#$74#$68#$65#$20#$74#$69#$6d#$65#$20 +
                 #$66#$6f#$72#$20#$61#$6c#$6c#$20);
     CipherText: RawByteString(#$f3#$1f#$da#$07#$01#$14#$62#$ee#$18#$7f#$43#$d8#$0a#$7c#$d9#$b5 +
                 #$b0#$d2#$90#$da#$6e#$5b#$9a#$87)),
    (Cipher:     ctDES;
     Mode:       cmOFB;
     KeyBits:    64;
     Key:        RawByteString(#$01#$23#$45#$67#$89#$ab#$cd#$ef);
     InitVector: RawByteString(#$12#$34#$56#$78#$90#$ab#$cd#$ef);
     PlainText:  RawByteString(#$4e#$6f#$77#$20#$69#$73#$20#$74#$68#$65#$20#$74#$69#$6d#$65#$20 +
                 #$66#$6f#$72#$20#$61#$6c#$6c#$20);
     CipherText: RawByteString(#$f3#$4a#$28#$50#$c9#$c6#$49#$85#$d6#$84#$ad#$96#$d7#$72#$e2#$f2 +
                 #$43#$ea#$49#$9a#$be#$e8#$ae#$95)),
    // Triple-DES test vectors generated from online DES calculator at http://www.riscure.com/tech-corner/online-crypto-tools/des.html
    (Cipher:     ctTripleDESEDE;
     Mode:       cmECB;
     KeyBits:    128;
     Key:        '1234567890123456';
     PlainText:  '1234567890123456';
     CipherText: RawByteString(#$BC#$57#$08#$BC#$02#$FE#$BF#$2F#$F6#$AD#$24#$D2#$1E#$FB#$70#$3A)),
    (Cipher:     ctTripleDESEDE;
     Mode:       cmECB;
     KeyBits:    128;
     Key:        RawByteString(#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00);
     PlainText:  '1234567890123456';
     CipherText: RawByteString(#$62#$DD#$8E#$4A#$61#$4E#$1A#$F9#$BE#$3D#$31#$47#$71#$1F#$A2#$77)),
    (Cipher:     ctTripleDESEDE;
     Mode:       cmCBC;
     KeyBits:    128;
     Key:        RawByteString(#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00#$00);
     InitVector: '12345678';
     PlainText:  '1234567890123456';
     CipherText: RawByteString(#$8C#$A6#$4D#$E9#$C1#$B1#$23#$A7#$97#$8D#$A5#$4E#$AE#$E5#$7B#$46)),
    // Triple-DES-3 from https://www.cosic.esat.kuleuven.be/nessie/testvectors/bc/des/Triple-Des-3-Key-192-64.unverified.test-vectors
    (Cipher:     ctTripleDES3EDE;
     Mode:       cmECB;
     KeyBits:    192;
     Key:        RawByteString(#$F1#$F1#$F1#$F1#$F1#$F1#$F1#$F1#$F1#$F1#$F1#$F1#$F1#$F1#$F1#$F1#$F1#$F1#$F1#$F1#$F1#$F1#$F1#$F1);
     InitVector: '';
     PlainText:  RawByteString(#$F1#$F1#$F1#$F1#$F1#$F1#$F1#$F1);
     CipherText: RawByteString(#$5D#$1B#$8F#$AF#$78#$39#$49#$4B)),
    (Cipher:     ctTripleDES3EDE;
     Mode:       cmECB;
     KeyBits:    192;
     Key:        RawByteString(#$00#$01#$02#$03#$04#$05#$06#$07#$08#$09#$0A#$0B#$0C#$0D#$0E#$0F#$10#$11#$12#$13#$14#$15#$16#$17);
     InitVector: '';
     PlainText:  RawByteString(#$00#$11#$22#$33#$44#$55#$66#$77);
     CipherText: RawByteString(#$97#$A2#$5B#$A8#$2B#$56#$4F#$4C)),
    (Cipher:     ctTripleDES3EDE;
     Mode:       cmECB;
     KeyBits:    192;
     Key:        RawByteString(#$2B#$D6#$45#$9F#$82#$C5#$B3#$00#$95#$2C#$49#$10#$48#$81#$FF#$48#$2B#$D6#$45#$9F#$82#$C5#$B3#$00);
     InitVector: '';
     PlainText:  RawByteString(#$EA#$02#$47#$14#$AD#$5C#$4D#$84);
     CipherText: RawByteString(#$C6#$16#$AC#$E8#$43#$95#$82#$47))
   );

procedure Test_TestCases;
var I : Integer;
    B : array[0..1023] of AnsiChar;
    L : Integer;
    C : RawByteString;
    M : RawByteString;
    X : Integer;
begin
  for I := 0 to CipherTestCaseCount - 1 do
    with CipherTestCases[I] do
      try
        if Assigned(GetCipherInfo(Cipher)) then
          begin
            M := IntToStringB(I);
            L := Length(PlainText);
            Move(Pointer(PlainText)^, B[0], L);
            L := Encrypt(Cipher, Mode, cpNone, KeyBits, Pointer(Key), Length(Key),
                @B[0], L, @B[0], Sizeof(B), Pointer(InitVector), Length(InitVector));
            C := '';
            SetLength(C, L);
            Move(B[0], Pointer(C)^, L);
            if C <> CipherText then
              begin
                for X := 1 to L do
                  if C[X] <> CipherText[X] then Writeln(X, '!', Ord(C[X]), '<>', Ord(CipherText[X]), ' L=', L);
              end;
            { Freepascal issue with RawByteString constant conversion }
            {
            if I = 13 then
              begin
                T := CipherText;
                for X := 1 to L do
                  Write(IntToHex(Ord(T[X]), 2));
                Writeln;
              end;
            }
            Assert(C = CipherText, M);
            L := Decrypt(Cipher, Mode, cpNone, KeyBits, Pointer(Key), Length(Key),
                @B[0], L, Pointer(InitVector), Length(InitVector));
            Move(B[0], PByteChar(C)^, L);
            Assert(C = PlainText, M);
            Assert(Encrypt(Cipher, Mode, cpNone, KeyBits, Key, PlainText, InitVector) = CipherText, M);
            Assert(Decrypt(Cipher, Mode, cpNone, KeyBits, Key, CipherText, InitVector) = PlainText, M);
          end;
      except
        on E : Exception do
          raise Exception.Create('Test case ' + IntToStr(I) + ': ' + E.Message);
      end;
end;

procedure Test_CipherRandom;
begin
  Assert(Length(SecureRandomHexStrB(0)) = 0);
  Assert(Length(SecureRandomHexStrB(1)) = 1);
  Assert(Length(SecureRandomHexStrB(511)) = 511);
  Assert(Length(SecureRandomHexStrB(512)) = 512);
  Assert(Length(SecureRandomHexStrB(513)) = 513);

  Assert(Length(SecureRandomHexStr(513)) = 513);
  Assert(Length(SecureRandomHexStrU(513)) = 513);

  Assert(Length(SecureRandomStrA(0)) = 0);
  Assert(Length(SecureRandomStrA(1)) = 1);
  Assert(Length(SecureRandomStrA(1023)) = 1023);
  Assert(Length(SecureRandomStrA(1024)) = 1024);
  Assert(Length(SecureRandomStrA(1025)) = 1025);
end;

procedure Test;
begin
  Assert(RC2BlockSize = 8);
  Assert(RC2BlockSize = Sizeof(TRC2Block));
  Assert(DESBlockSize = 8);
  Assert(DESBlockSize = Sizeof(TDESBlock));

  flcCipherAES.Test;
  flcCipherDH.Test;
  flcCipherRSA.Test;
  {$IFDEF Cipher_SupportEC}
  flcCipherEllipticCurve.Test;
  {$ENDIF}

  flcCipher.Test;

  Test_CipherRandom;
  Test_TestCases;
end;
{$ENDIF}

{$IFDEF CIPHER_PROFILE}
procedure Profile;
begin
end;
{$ENDIF}



end.
