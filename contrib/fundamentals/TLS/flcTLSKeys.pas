{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals TLS                                         }
{   File name:        flcTLSKeys.pas                                           }
{   File version:     5.02                                                     }
{   Description:      TLS Keys                                                 }
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
{   2020/05/09  5.02  Create flcTLSKeys unit from flcTLSUtils unit.            }
{                                                                              }
{******************************************************************************}

{$INCLUDE flcTLS.inc}

unit flcTLSKeys;

interface

uses
  { Utils }

  flcStdTypes,

  { TLS }

  flcTLSProtocolVersion;



{                                                                              }
{ Key block                                                                    }
{                                                                              }
function  tls10KeyBlock(const MasterSecret, ServerRandom, ClientRandom: RawByteString; const Size: Integer): RawByteString;
function  tls12SHA256KeyBlock(const MasterSecret, ServerRandom, ClientRandom: RawByteString; const Size: Integer): RawByteString;
function  tls12SHA512KeyBlock(const MasterSecret, ServerRandom, ClientRandom: RawByteString; const Size: Integer): RawByteString;

function  TLSKeyBlock(const ProtocolVersion: TTLSProtocolVersion;
          const MasterSecret, ServerRandom, ClientRandom: RawByteString; const Size: Integer): RawByteString;



{                                                                              }
{ Master secret                                                                }
{                                                                              }
function  tls10MasterSecret(const PreMasterSecret, ClientRandom, ServerRandom: RawByteString): RawByteString;
function  tls12SHA256MasterSecret(const PreMasterSecret, ClientRandom, ServerRandom: RawByteString): RawByteString;
function  tls12SHA512MasterSecret(const PreMasterSecret, ClientRandom, ServerRandom: RawByteString): RawByteString;

function  TLSMasterSecret(const ProtocolVersion: TTLSProtocolVersion;
          const PreMasterSecret, ClientRandom, ServerRandom: RawByteString): RawByteString;



{                                                                              }
{ TLS Keys                                                                     }
{                                                                              }
type
  TTLSKeys = record
    KeyBlock     : RawByteString;
    ClientMACKey : RawByteString;
    ServerMACKey : RawByteString;
    ClientEncKey : RawByteString;
    ServerEncKey : RawByteString;
    ClientIV     : RawByteString;
    ServerIV     : RawByteString;
  end;

procedure GenerateTLSKeys(
          const ProtocolVersion: TTLSProtocolVersion;
          const MACKeyBits, CipherKeyBits, IVBits: Integer;
          const MasterSecret, ServerRandom, ClientRandom: RawByteString;
          var TLSKeys: TTLSKeys);

procedure GenerateFinalTLSKeys(
          const ProtocolVersion: TTLSProtocolVersion;
          const IsExportable: Boolean;
          const ExpandedKeyBits: Integer;
          const ServerRandom, ClientRandom: RawByteString;
          var TLSKeys: TTLSKeys);



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
  flcStrings,

  { TLS }

  flcTLSErrors,
  flcTLSPRF;



{                                                                              }
{ Key block                                                                    }
{                                                                              }
{ SSL 3.0:                                                                     }
{ key_block =                                                                  }
{      MD5(master_secret + SHA('A' + master_secret +                           }
{                              ServerHello.random + ClientHello.random)) +     }
{      MD5(master_secret + SHA('BB' + master_secret +                          }
{                              ServerHello.random + ClientHello.random)) +     }
{      MD5(master_secret + SHA('CCC' + master_secret +                         }
{                              ServerHello.random + ClientHello.random)) +     }
{                              [...];                                          }
{                                                                              }
{ TLS 1.0 / 1.1 / 1.2:                                                         }
{ key_block = PRF(SecurityParameters.master_secret,                            }
{                 "key expansion",                                             }
{                 SecurityParameters.server_random +                           }
{                 SecurityParameters.client_random);                           }
{                                                                              }
function ssl30KeyBlockP(const Prefix, MasterSecret, ServerRandom, ClientRandom: RawByteString): RawByteString;
begin
  Result :=
      MD5DigestToStrA(
          CalcMD5(MasterSecret +
              SHA1DigestToStrA(
                  CalcSHA1(Prefix + MasterSecret + ServerRandom + ClientRandom))));
end;

function ssl30KeyBlockPF(const MasterSecret, ServerRandom, ClientRandom: RawByteString; const Size: Integer): RawByteString;
var Salt : RawByteString;
    I : Integer;
begin
  Result := '';
  I := 1;
  while Length(Result) < Size do
    begin
      if I > 26 then
        raise ETLSError.Create(TLSError_InvalidParameter);
      Salt := DupCharB(ByteChar(Ord('A') + I - 1), I);
      Result := Result +
          ssl30KeyBlockP(Salt, MasterSecret, ServerRandom, ClientRandom);
      Inc(I);
    end;
  SetLength(Result, Size);
end;

function ssl30KeyBlock(const MasterSecret, ServerRandom, ClientRandom: RawByteString; const Size: Integer): RawByteString;
begin
  Result := ssl30KeyBlockPF(MasterSecret, ServerRandom, ClientRandom, Size);
end;

const
  LabelKeyExpansion = 'key expansion';

function tls10KeyBlock(const MasterSecret, ServerRandom, ClientRandom: RawByteString; const Size: Integer): RawByteString;
var S : RawByteString;
begin
  S := ServerRandom + ClientRandom;
  Result := tls10PRF(MasterSecret, LabelKeyExpansion, S, Size);
end;

function tls12SHA256KeyBlock(const MasterSecret, ServerRandom, ClientRandom: RawByteString; const Size: Integer): RawByteString;
var S : RawByteString;
begin
  S := ServerRandom + ClientRandom;
  Result := tls12PRF_SHA256(MasterSecret, LabelKeyExpansion, S, Size);
end;

function tls12SHA512KeyBlock(const MasterSecret, ServerRandom, ClientRandom: RawByteString; const Size: Integer): RawByteString;
var S : RawByteString;
begin
  S := ServerRandom + ClientRandom;
  Result := tls12PRF_SHA512(MasterSecret, LabelKeyExpansion, S, Size);
end;

function TLSKeyBlock(const ProtocolVersion: TTLSProtocolVersion;
         const MasterSecret, ServerRandom, ClientRandom: RawByteString; const Size: Integer): RawByteString;
begin
  if IsTLS12OrLater(ProtocolVersion) then
    Result := tls12SHA256KeyBlock(MasterSecret, ServerRandom, ClientRandom, Size) else
  if IsTLS10OrLater(ProtocolVersion) then
    Result := tls10KeyBlock(MasterSecret, ServerRandom, ClientRandom, Size) else
  if IsSSL3(ProtocolVersion) then
    Result := ssl30KeyBlock(MasterSecret, ServerRandom, ClientRandom, Size)
  else
    raise ETLSError.Create(TLSError_InvalidParameter);
end;



{                                                                              }
{ Master secret                                                                }
{                                                                              }
{ SSL 3:                                                                       }
{ master_secret =                                                              }
{      MD5(pre_master_secret + SHA('A' + pre_master_secret +                   }
{          ClientHello.random + ServerHello.random)) +                         }
{      MD5(pre_master_secret + SHA('BB' + pre_master_secret +                  }
{          ClientHello.random + ServerHello.random)) +                         }
{      MD5(pre_master_secret + SHA('CCC' + pre_master_secret +                 }
{          ClientHello.random + ServerHello.random));                          }
{                                                                              }
{ TLS 1.0 1.1 1.2:                                                             }
{ master_secret = PRF(pre_master_secret,                                       }
{                     "master secret",                                         }
{                     ClientHello.random + ServerHello.random)                 }
{                                                                              }
{ The master secret is always exactly 48 bytes in length. The length of        }
{ the premaster secret will vary depending on key exchange method.             }
{                                                                              }
const
  LabelMasterSecret = 'master secret';
  MasterSecretSize = 48;

function ssl30MasterSecretP(const Prefix, PreMasterSecret, ClientRandom, ServerRandom: RawByteString): RawByteString;
begin
  Result :=
      MD5DigestToStrA(
          CalcMD5(PreMasterSecret +
              SHA1DigestToStrA(
                  CalcSHA1(Prefix + PreMasterSecret + ClientRandom + ServerRandom))));
end;

function ssl30MasterSecret(const PreMasterSecret, ClientRandom, ServerRandom: RawByteString): RawByteString;
begin
  Result :=
      ssl30MasterSecretP('A', PreMasterSecret, ClientRandom, ServerRandom) +
      ssl30MasterSecretP('BB', PreMasterSecret, ClientRandom, ServerRandom) +
      ssl30MasterSecretP('CCC', PreMasterSecret, ClientRandom, ServerRandom);
end;

function tls10MasterSecret(const PreMasterSecret, ClientRandom, ServerRandom: RawByteString): RawByteString;
var S : RawByteString;
begin
  S := ClientRandom + ServerRandom;
  Result := tls10PRF(PreMasterSecret, LabelMasterSecret, S, MasterSecretSize);
end;

function tls12SHA256MasterSecret(const PreMasterSecret, ClientRandom, ServerRandom: RawByteString): RawByteString;
var S : RawByteString;
begin
  S := ClientRandom + ServerRandom;
  Result := tls12PRF_SHA256(PreMasterSecret, LabelMasterSecret, S, MasterSecretSize);
end;

function tls12SHA512MasterSecret(const PreMasterSecret, ClientRandom, ServerRandom: RawByteString): RawByteString;
var S : RawByteString;
begin
  S := ClientRandom + ServerRandom;
  Result := tls12PRF_SHA512(PreMasterSecret, LabelMasterSecret, S, MasterSecretSize);
end;

function TLSMasterSecret(const ProtocolVersion: TTLSProtocolVersion;
         const PreMasterSecret, ClientRandom, ServerRandom: RawByteString): RawByteString;
begin
  if IsTLS12OrLater(ProtocolVersion) then
    Result := tls12SHA256MasterSecret(PreMasterSecret, ClientRandom, ServerRandom) else
  if IsTLS10OrLater(ProtocolVersion) then
    Result := tls10MasterSecret(PreMasterSecret, ClientRandom, ServerRandom) else
  if IsSSL3(ProtocolVersion) then
    Result := ssl30MasterSecret(PreMasterSecret, ClientRandom, ServerRandom)
  else
    raise ETLSError.Create(TLSError_InvalidParameter);
end;



{                                                                              }
{ TLS Keys                                                                     }
{                                                                              }
procedure GenerateTLSKeys(
          const ProtocolVersion: TTLSProtocolVersion;
          const MACKeyBits, CipherKeyBits, IVBits: Integer;
          const MasterSecret, ServerRandom, ClientRandom: RawByteString;
          var TLSKeys: TTLSKeys);
var L, I, N : Integer;
    S : RawByteString;
begin
  Assert(MACKeyBits mod 8 = 0);
  Assert(CipherKeyBits mod 8 = 0);
  Assert(IVBits mod 8 = 0);

  L := MACKeyBits * 2 + CipherKeyBits * 2 + IVBits * 2;
  L := L div 8;
  S := TLSKeyBlock(ProtocolVersion, MasterSecret, ServerRandom, ClientRandom, L);
  TLSKeys.KeyBlock := S;
  I := 1;
  N := MACKeyBits div 8;
  TLSKeys.ClientMACKey := Copy(S, I, N);
  TLSKeys.ServerMACKey := Copy(S, I + N, N);
  Inc(I, N * 2);
  N := CipherKeyBits div 8;
  TLSKeys.ClientEncKey := Copy(S, I, N);
  TLSKeys.ServerEncKey := Copy(S, I + N, N);
  Inc(I, N * 2);
  N := IVBits div 8;
  TLSKeys.ClientIV := Copy(S, I, N);
  TLSKeys.ServerIV := Copy(S, I + N, N);
end;

{ TLS 1.0:                                                                     }
{ final_client_write_key = PRF(SecurityParameters.client_write_key,            }
{   "client write key",                                                        }
{   SecurityParameters.client_random + SecurityParameters.server_random);      }
{ final_server_write_key = PRF(SecurityParameters.server_write_key,            }
{   "server write key",                                                        }
{   SecurityParameters.client_random + SecurityParameters.server_random);      }
{ iv_block = PRF("", "IV block",                                               }
{   SecurityParameters.client_random + SecurityParameters.server_random);      }
const
  LabelClientWriteKey = 'client write key';
  LabelServerWriteKey = 'server write key';
  LabelIVBlock        = 'IV block';

procedure GenerateFinalTLSKeys(
          const ProtocolVersion: TTLSProtocolVersion;
          const IsExportable: Boolean;
          const ExpandedKeyBits: Integer;
          const ServerRandom, ClientRandom: RawByteString;
          var TLSKeys: TTLSKeys);
var S : RawByteString;
    L : Integer;
    V : RawByteString;
begin
  if IsTLS11OrLater(ProtocolVersion) then
    exit;
  if not IsExportable then
    exit;
  if IsSSL2(ProtocolVersion) or IsSSL3(ProtocolVersion) then
    raise ETLSError.Create(TLSError_InvalidParameter, 'Unsupported version');
  S := ClientRandom + ServerRandom;
  Assert(ExpandedKeyBits mod 8 = 0);
  L := ExpandedKeyBits div 8;
  TLSKeys.ClientEncKey := tls10PRF(TLSKeys.ClientEncKey, LabelClientWriteKey, S, L);
  TLSKeys.ServerEncKey := tls10PRF(TLSKeys.ServerEncKey, LabelServerWriteKey, S, L);
  L := Length(TLSKeys.ClientIV);
  if L > 0 then
    begin
      V := tls10PRF('', LabelIVBlock, S, L * 2);
      TLSKeys.ClientIV := Copy(V, 1, L);
      TLSKeys.ServerIV := Copy(V, L + 1, L);
    end;
end;



{                                                                              }
{ Test                                                                         }
{                                                                              }
{$IFDEF TLS_TEST}
{$ASSERTIONS ON}
const
  PreMasterSecret = RawByteString(
      #$03#$01#$84#$54#$F5#$D6#$EB#$F5#$A8#$08#$BA#$FA#$7A#$22#$61#$2D +
      #$75#$DC#$40#$E8#$98#$F9#$0E#$B2#$87#$80#$B8#$1A#$8F#$68#$25#$B8 +
      #$51#$D0#$54#$45#$61#$8A#$50#$C9#$BB#$0E#$39#$53#$45#$78#$BE#$79);
  ClientRandom = RawByteString(
      #$40#$FC#$30#$AE#$2D#$63#$84#$BB#$C5#$4B#$27#$FD#$58#$21#$CA#$90 +
      #$05#$F6#$A7#$7B#$37#$BB#$72#$E1#$FC#$1D#$1B#$6A#$F5#$1C#$C8#$9F);
  ServerRandom = RawByteString(
      #$40#$FC#$31#$10#$79#$AB#$17#$66#$FA#$8B#$3F#$AA#$FD#$5E#$48#$23 +
      #$FA#$90#$31#$D8#$3C#$B9#$A3#$2C#$8C#$F5#$E9#$81#$9B#$A2#$63#$6C);
  MasterSecret = RawByteString(
      #$B0#$00#$22#$34#$59#$03#$16#$B7#$7A#$6C#$56#$9B#$89#$D2#$7A#$CC +
      #$F3#$85#$55#$59#$3A#$14#$76#$3D#$54#$BF#$EB#$3F#$E0#$2F#$B1#$4B +
      #$79#$8C#$75#$A9#$78#$55#$6C#$8E#$A2#$14#$60#$B7#$45#$EB#$77#$B2);
  MACWriteKey = RawByteString(
      #$85#$F0#$56#$F8#$07#$1D#$B1#$89#$89#$D0#$E1#$33#$3C#$CA#$63#$F9);

procedure TestKeyBlock;
var S : RawByteString;
begin
  //                                                                                              //
  // Example from http://download.oracle.com/javase/1.5.0/docs/guide/security/jsse/ReadDebug.html //
  //                                                                                              //
  Assert(tls10MasterSecret(PreMasterSecret, ClientRandom, ServerRandom) = MasterSecret);
  S := tls10KeyBlock(MasterSecret, ServerRandom, ClientRandom, 64);
  Assert(Copy(S, 1, 48) =
      MACWriteKey +
      #$1E#$4D#$D1#$D3#$0A#$78#$EE#$B7#$4F#$EC#$15#$79#$B2#$59#$18#$40 +
      #$10#$D0#$D6#$C2#$D9#$B7#$62#$CB#$2C#$74#$BF#$5F#$85#$3C#$6F#$E7);
end;

procedure Test;
begin
  TestKeyBlock;
end;
{$ENDIF}



end.
