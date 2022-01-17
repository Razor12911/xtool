{ ****************************************************************************** }
{ * Core cipher Library ,writen by QQ 600585@qq.com                            * }
{ * https://zpascal.net                                                        * }
{ * https://github.com/PassByYou888/zAI                                        * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/PascalString                               * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zChinese                                   * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/FFMPEG-Header                              * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/InfiniteIoT                                * }
{ * https://github.com/PassByYou888/FastMD5                                    * }
{ ****************************************************************************** }

(*
  update history
  2017-11-26
  fixed fastMD5,THashMD5 calculate x64 and x86,ARM platform more than 4G memory Support QQ600585
  change name TMD5Class as THashMD5
  Added global DefaultParallelDepth

  2017-12-6
  added supported hash elf64

  2017-12-7
  added System default key

  2018-5-16
  remove pasmp

  2018-9
  fixed rc6 with arm Linux
*)

unit CoreCipher;

{ core cipher engine. create by qq600585 }
{$INCLUDE zDefine.inc}

// debug used
// {$UNDEF RangeCheck}
// {$UNDEF OverflowCheck}

{$O+}

interface

uses
  Types, SysUtils, Math, TypInfo,
{$IFDEF FastMD5}
  Fast_MD5,
{$ENDIF}
  CoreClasses, UnicodeMixedLib, MemoryStream64, PascalStrings, ListEngine;

{$REGION 'BaseDefine'}


const
  { largest structure that can be created }
  MaxStructSize = MaxInt; { 2G }
  cIntSize = 4;
  cKeyDWORDSize = 4;
  cKey2DWORDSize = 8;
  cKey64Size = 8;
  cKey128Size = 16;
  cKey192Size = 24;
  cKey256Size = 32;

type
  PDWORD = ^DWORD;

  { general structures }
  PDWordArray = ^TDWordArray;
  TDWordArray = array [0 .. MaxStructSize div SizeOf(DWORD) - 1] of DWORD;

  TCCByteArray = array [0 .. MaxStructSize div SizeOf(Byte) - 1] of Byte;
  PCCByteArray = ^TCCByteArray;

  TInt32 = packed record
    case Byte of
      1: (Lo: Word;
          Hi: Word);
      2: (LoLo: Byte;
          LoHi: Byte;
          HiLo: Byte;
          HiHi: Byte);
      3: (i: Integer);
      4: (u: DWORD);
  end;

  TInt64 = packed record
    case Byte of
      0: (Lo: Integer;
          Hi: Integer);
      1: (LoLo: Word;
          LoHi: Word;
          HiLo: Word;
          HiHi: Word);
      2: (LoLoLo: Byte;
          LoLoHi: Byte;
          LoHiLo: Byte;
          LoHiHi: Byte;
          HiLoLo: Byte;
          HiLoHi: Byte;
          HiHiLo: Byte;
          HiHiHi: Byte);
      3: (i: Int64);
      4: (u: UInt64);
  end;

  { encryption key types }
type
  PKey64 = ^TKey64; { !!.03 }
  TKey64 = array [0 .. 7] of Byte;

  PKey128 = ^TKey128; { !!.03 }
  TKey128 = array [0 .. 15] of Byte;

  PKey256 = ^TKey256; { !!.03 }
  TKey256 = array [0 .. 31] of Byte;

  { encryption block types }
  PLBCBlock = ^TLBCBlock;
  TLBCBlock = array [0 .. 3] of DWORD; { LBC block }

  PDESBlock = ^TDESBlock;
  TDESBlock = array [0 .. 7] of Byte; { DES block }

  PLQCBlock = ^TLQCBlock;
  TLQCBlock = array [0 .. 1] of DWORD; { Quick Cipher,no LBC key generate }

  PBFBlock = ^TBFBlock;
  TBFBlock = array [0 .. 1] of DWORD; { BlowFish }

  PXXTEABlock = ^TXXTEABlock;
  TXXTEABlock = array [0 .. 63] of Byte; { XXTEA }

  TDesConverter = packed record
    case Byte of
      0: (Bytes: array [0 .. 7] of Byte);
      1: (DWords: array [0 .. 1] of DWORD)
  end;

  P128Bit = ^T128Bit;
  T128Bit = array [0 .. 3] of DWORD;

  P256Bit = ^T256Bit;
  T256Bit = array [0 .. 7] of DWORD;

  TTransformOutput = array [0 .. 3] of DWORD;
  PTransformInput = ^TTransformInput;
  TTransformInput = array [0 .. 15] of DWORD;

  { context type constants }
const
  BFRounds = 16; { 16 blowfish rounds }

  { block cipher context types }
type
  { Blowfish }
  PBFContext = ^TBFContext;

  TBFContext = packed record
    PBox: array [0 .. (BFRounds + 1)] of DWORD;
    SBox: array [0 .. 3, 0 .. 255] of DWORD;
  end;

  { DES }
  PDESContext = ^TDESContext;

  TDESContext = packed record
    TransformedKey: array [0 .. 31] of DWORD;
    Encrypt: Boolean;
  end;

  { 3 DES }
  PTripleDESContext = ^TTripleDESContext;
  TTripleDESContext = array [0 .. 1] of TDESContext;

  PTripleDESContext3Key = ^TTripleDESContext3Key;
  TTripleDESContext3Key = array [0 .. 2] of TDESContext; { !!.01 }

  { LBC Cipher context }
  PLBCContext = ^TLBCContext;

  TLBCContext = packed record
    Encrypt: Boolean;
    Dummy: array [0 .. 2] of Byte; { filler }
    Rounds: Integer;
    case Byte of
      0: (SubKeys64: array [0 .. 15] of TKey64);
      1: (SubKeysInts: array [0 .. 3, 0 .. 7] of DWORD);
  end;

  { LSC stream cipher }
  PLSCContext = ^TLSCContext;

  TLSCContext = packed record
    index: Integer;
    Accumulator: Integer;
    SBox: array [0 .. 255] of Byte;
  end;

  { random number stream ciphers }
  PRNG32Context = ^TRNG32Context;
  TRNG32Context = array [0 .. 3] of Byte;

  PRNG64Context = ^TRNG64Context;
  TRNG64Context = array [0 .. 7] of Byte;

  { message digest blocks }
  PMD5Digest = ^TMD5Digest;
  TMD5Digest = TMD5; { 128 bits - MD5 }
  TMD5Key = TMD5Digest;

  PSHA1Digest = ^TSHA1Digest;
  TSHA1Digest = array [0 .. 19] of Byte; { 160 bits - SHA-1 }
  TSHA1Key = TSHA1Digest;

  PSHA256Digest = ^TSHA256Digest;
  TSHA256Digest = array [0 .. 31] of Byte; { 256 bits - SHA-256 }
  TSHA256Key = TSHA256Digest;

  PSHA512Digest = ^TSHA512Digest;
  TSHA512Digest = array [0 .. 63] of Byte; { 512 bits - SHA-512 }
  TSHA512Key = TSHA512Digest;

  PSHA3_224_Digest = ^TSHA3_224_Digest;
  PSHA3_256_Digest = ^TSHA3_256_Digest;
  PSHA3_384_Digest = ^TSHA3_384_Digest;
  PSHA3_512_Digest = ^TSHA3_512_Digest;

  TSHA3_224_Digest = array [0 .. 224 div 8 - 1] of Byte;
  TSHA3_256_Digest = array [0 .. 256 div 8 - 1] of Byte;
  TSHA3_384_Digest = array [0 .. 384 div 8 - 1] of Byte;
  TSHA3_512_Digest = array [0 .. 512 div 8 - 1] of Byte;

  { message digest context types }
  TLMDContext = packed record
    DigestIndex: Integer;
    Digest: array [0 .. 255] of Byte;
    KeyIndex: Integer;
    case Byte of
      0: (KeyInts: array [0 .. 3] of DWORD);
      1: (key: TKey128);
  end;

  PMD5Context = ^TMD5Context;

  TMD5Context = packed record       { MD5 }
    Count: array [0 .. 1] of DWORD; { number of bits handled mod 2^64 }
    State: TTransformOutput;        { scratch buffer }
    Buf: array [0 .. 63] of Byte;   { input buffer }
  end;

  TSHA1Context = packed record { SHA-1 }
    sdHi: DWORD;
    sdLo: DWORD;
    sdIndex: nativeUInt;
    sdHash: array [0 .. 4] of DWORD;
    sdBuf: array [0 .. 63] of Byte;
  end;
{$ENDREGION 'BaseDefine'}


type
  { key style and auto Encrypt }
  TCipherSecurity = (csNone,
    csDES64, csDES128, csDES192,
    csBlowfish, csLBC, csLQC, csRNG32, csRNG64, csLSC,
    // mini cipher
    csXXTea512,
    // NIST cipher
    csRC6, csSerpent, csMars, csRijndael, csTwoFish);

  TCipherSecuritys = set of TCipherSecurity;

  TCipherSecurityArray = array of TCipherSecurity;

  TCipherKeyStyle = (cksNone, cksKey64, cks3Key64, cksKey128, cksKey256, cks2IntKey, cksIntKey, ckyDynamicKey);
  PCipherKeyBuffer = ^TCipherKeyBuffer;
  TCipherKeyBuffer = TBytes;

  THashSecurity = (
    hsNone,
    hsFastMD5, hsMD5, hsSHA1, hsSHA256, hsSHA512,
    hsSHA3_224, hsSHA3_256, hsSHA3_384, hsSHA3_512,
    hs256, hs128, hs64, hs32, hs16, hsELF, hsELF64, hsMix128, hsCRC16, hsCRC32);

  THashSecuritys = set of THashSecurity;

  TCipher = class(TCoreClassObject)
  public const
    CAllHash: THashSecuritys = [
      hsNone,
      hsFastMD5, hsMD5, hsSHA1, hsSHA256, hsSHA512,
      hsSHA3_224, hsSHA3_256, hsSHA3_384, hsSHA3_512,
      hs256, hs128, hs64, hs32, hs16, hsELF, hsELF64, hsMix128, hsCRC16, hsCRC32];

    CHashName: array [THashSecurity] of SystemString = (
      'None',
      'FastMD5', 'MD5', 'SHA1', 'SHA256', 'SHA512',
      'SHA3_224', 'SHA3_256', 'SHA3_384', 'SHA3_512',
      '256', '128', '64', '32', '16', 'ELF', 'ELF64', 'Mix128', 'CRC16', 'CRC32');

    CCipherSecurityName: array [TCipherSecurity] of SystemString =
      ('None',
      'DES64', 'DES128', 'DES192',
      'Blowfish', 'LBC', 'LQC', 'RNG32', 'RNG64', 'LSC',
      'XXTea512',
      'RC6', 'Serpent', 'Mars', 'Rijndael', 'TwoFish');

    cCipherKeyStyle: array [TCipherSecurity] of TCipherKeyStyle =
      (
      cksNone,       // csNone
      cksKey64,      // csDES64
      cksKey128,     // csDES128
      cks3Key64,     // csDES192
      cksKey128,     // csBlowfish
      cksKey128,     // csLBC
      cksKey128,     // csLQC
      cksIntKey,     // csRNG32
      cks2IntKey,    // csRNG64
      ckyDynamicKey, // csLSC
      cksKey128,     // csXXTea512
      ckyDynamicKey, // csRC6
      ckyDynamicKey, // csSerpent
      ckyDynamicKey, // csMars
      ckyDynamicKey, // csRijndael
      ckyDynamicKey  // csTwoFish
      );
  public
    class function AllCipher: TCipherSecurityArray;

    class function NameToHashSecurity(n: SystemString; var hash: THashSecurity): Boolean;

    class function BuffToString(buff: Pointer; Size: NativeInt): TPascalString; overload;
    class function StringToBuff(const Hex: TPascalString; var Buf; BufSize: Cardinal): Boolean; overload;

    class procedure HashToString(hash: Pointer; Size: NativeInt; var output: TPascalString); overload;

    class procedure HashToString(hash: TSHA3_224_Digest; var output: TPascalString); overload;
    class procedure HashToString(hash: TSHA3_256_Digest; var output: TPascalString); overload;
    class procedure HashToString(hash: TSHA3_384_Digest; var output: TPascalString); overload;
    class procedure HashToString(hash: TSHA3_512_Digest; var output: TPascalString); overload;
    class procedure HashToString(hash: TSHA512Digest; var output: TPascalString); overload;
    class procedure HashToString(hash: TSHA256Digest; var output: TPascalString); overload;
    class procedure HashToString(hash: TSHA1Digest; var output: TPascalString); overload;
    class procedure HashToString(hash: TMD5Digest; var output: TPascalString); overload;
    class procedure HashToString(hash: TBytes; var output: TPascalString); overload;
    class procedure HashToString(hash: TBytes; var output: SystemString); overload;

    class function CompareHash(h1, h2: TSHA3_224_Digest): Boolean; overload;
    class function CompareHash(h1, h2: TSHA3_256_Digest): Boolean; overload;
    class function CompareHash(h1, h2: TSHA3_384_Digest): Boolean; overload;
    class function CompareHash(h1, h2: TSHA3_512_Digest): Boolean; overload;
    class function CompareHash(h1, h2: TSHA512Digest): Boolean; overload;
    class function CompareHash(h1, h2: TSHA256Digest): Boolean; overload;
    class function CompareHash(h1, h2: TSHA1Digest): Boolean; overload;
    class function CompareHash(h1, h2: TMD5Digest): Boolean; overload;
    class function CompareHash(h1, h2: Pointer; Size: NativeInt): Boolean; overload;
    class function CompareHash(h1, h2: TBytes): Boolean; overload;

    class function CompareKey(k1, k2: TCipherKeyBuffer): Boolean; overload;

    class function GenerateSHA3_224Hash(sour: Pointer; Size: NativeInt): TSHA3_224_Digest;
    class function GenerateSHA3_256Hash(sour: Pointer; Size: NativeInt): TSHA3_256_Digest;
    class function GenerateSHA3_384Hash(sour: Pointer; Size: NativeInt): TSHA3_384_Digest;
    class function GenerateSHA3_512Hash(sour: Pointer; Size: NativeInt): TSHA3_512_Digest;
    class function GenerateSHA512Hash(sour: Pointer; Size: NativeInt): TSHA512Digest;
    class function GenerateSHA256Hash(sour: Pointer; Size: NativeInt): TSHA256Digest;
    class function GenerateSHA1Hash(sour: Pointer; Size: NativeInt): TSHA1Digest;
    class function GenerateMD5Hash(sour: Pointer; Size: NativeInt): TMD5Digest;
    class procedure GenerateMDHash(sour: Pointer; Size: NativeInt; OutHash: Pointer; HashSize: NativeInt);

    class procedure GenerateHashByte(hs: THashSecurity; sour: Pointer; Size: NativeInt; var output: TBytes);
    class function GenerateHashString(hs: THashSecurity; sour: Pointer; Size: NativeInt): TPascalString;

    class function BufferToHex(const Buf; BufSize: Cardinal): TPascalString;
    class function HexToBuffer(const Hex: TPascalString; var Buf; BufSize: Cardinal): Boolean;

    class function CopyKey(const k: TCipherKeyBuffer): TCipherKeyBuffer;

    class procedure GenerateNoneKey(var output: TCipherKeyBuffer);
    class procedure GenerateKey64(const s: TPascalString; var output: TCipherKeyBuffer); overload;
    class procedure GenerateKey64(sour: Pointer; Size: NativeInt; var output: TCipherKeyBuffer); overload;
    class procedure GenerateKey128(const s: TPascalString; var output: TCipherKeyBuffer); overload;
    class procedure GenerateKey128(sour: Pointer; Size: NativeInt; var output: TCipherKeyBuffer); overload;
    class procedure GenerateKey256(const s: TPascalString; var output: TCipherKeyBuffer); overload;
    class procedure GenerateKey256(sour: Pointer; Size: NativeInt; var output: TCipherKeyBuffer); overload;
    class procedure Generate3Key64(const s: TPascalString; var output: TCipherKeyBuffer); overload;
    class procedure Generate3Key64(sour: Pointer; Size: NativeInt; var output: TCipherKeyBuffer); overload;
    class procedure Generate2IntKey(const s: TPascalString; var output: TCipherKeyBuffer); overload;
    class procedure Generate2IntKey(sour: Pointer; Size: NativeInt; var output: TCipherKeyBuffer); overload;
    class procedure GenerateIntKey(const s: TPascalString; var output: TCipherKeyBuffer); overload;
    class procedure GenerateIntKey(sour: Pointer; Size: NativeInt; var output: TCipherKeyBuffer); overload;
    class procedure GenerateBytesKey(const s: TPascalString; KeySize: DWORD; var output: TCipherKeyBuffer); overload;
    class procedure GenerateBytesKey(sour: Pointer; Size, KeySize: DWORD; var output: TCipherKeyBuffer); overload;
    class procedure GenerateKey64(const k: TKey64; var output: TCipherKeyBuffer); overload;
    class procedure GenerateKey128(const k1, k2: TKey64; var output: TCipherKeyBuffer); overload;

    class procedure GenerateKey(const k: TKey64; var output: TCipherKeyBuffer); overload;
    class procedure GenerateKey(const k1, k2, k3: TKey64; var output: TCipherKeyBuffer); overload;
    class procedure GenerateKey(const k: TKey128; var output: TCipherKeyBuffer); overload;
    class procedure GenerateKey(const k: TKey256; var output: TCipherKeyBuffer); overload;
    class procedure GenerateKey(const k1, k2: DWORD; var output: TCipherKeyBuffer); overload;
    class procedure GenerateKey(const k: DWORD; var output: TCipherKeyBuffer); overload;
    class procedure GenerateKey(const key: PByte; Size: DWORD; var output: TCipherKeyBuffer); overload;
    class procedure GenerateKey(cs: TCipherSecurity; buffPtr: Pointer; Size: NativeInt; var output: TCipherKeyBuffer); overload;
    class procedure GenerateKey(cs: TCipherSecurity; s: TPascalString; var output: TCipherKeyBuffer); overload;

    class function GetKeyStyle(const p: PCipherKeyBuffer): TCipherKeyStyle; overload;

    class function GetKey(const KeyBuffPtr: PCipherKeyBuffer; var k: TKey64): Boolean; overload;
    class function GetKey(const KeyBuffPtr: PCipherKeyBuffer; var k1, k2, k3: TKey64): Boolean; overload;
    class function GetKey(const KeyBuffPtr: PCipherKeyBuffer; var k: TKey128): Boolean; overload;
    class function GetKey(const KeyBuffPtr: PCipherKeyBuffer; var k: TKey256): Boolean; overload;
    class function GetKey(const KeyBuffPtr: PCipherKeyBuffer; var k1, k2: DWORD): Boolean; overload;
    class function GetKey(const KeyBuffPtr: PCipherKeyBuffer; var k: DWORD): Boolean; overload;
    class function GetKey(const KeyBuffPtr: PCipherKeyBuffer; var key: TBytes): Boolean; overload;

    class procedure EncryptTail(TailPtr: Pointer; TailSize: NativeInt);
    class function DES64(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
    class function DES128(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
    class function DES192(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
    class function Blowfish(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
    class function LBC(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
    class function LQC(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
    class function RNG32(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer): Boolean;
    class function RNG64(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer): Boolean;
    class function LSC(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer): Boolean;
    class function TwoFish(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
    class function XXTea512(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
    class function RC6(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
    class function Serpent(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
    class function Mars(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
    class function Rijndael(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
    class procedure BlockCBC(sour: Pointer; Size: NativeInt; boxBuff: Pointer; boxSiz: NativeInt);

    class function EncryptBuffer(cs: TCipherSecurity; sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
    class function EncryptBufferCBC(cs: TCipherSecurity; sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
  end;

{$IFDEF Parallel}

  TParallelCipher = class(TCoreClassObject)
  private type
    TParallelCipherFunc = procedure(Job, buff, key: Pointer; Size: NativeInt) of object;

    PParallelCipherJobData = ^TParallelCipherJobData;

    TParallelCipherJobData = packed record
      cipherFunc: TParallelCipherFunc;
      KeyBuffer: Pointer;
      OriginBuffer: Pointer;
      BlockLen: NativeInt;
      TotalBlock: NativeInt;
      CompletedBlock: NativeInt;
      Encrypt: Boolean;
    end;
  protected
    procedure DES64_Parallel(Job, buff, key: Pointer; Size: NativeInt);
    procedure DES128_Parallel(Job, buff, key: Pointer; Size: NativeInt);
    procedure DES192_Parallel(Job, buff, key: Pointer; Size: NativeInt);
    procedure Blowfish_Parallel(Job, buff, key: Pointer; Size: NativeInt);
    procedure LBC_Parallel(Job, buff, key: Pointer; Size: NativeInt);
    procedure LQC_Parallel(Job, buff, key: Pointer; Size: NativeInt);
    procedure TwoFish_Parallel(Job, buff, key: Pointer; Size: NativeInt);
    procedure XXTea512_Parallel(Job, buff, key: Pointer; Size: NativeInt);
    procedure RC6_Parallel(Job, buff, key: Pointer; Size: NativeInt);
    procedure Serpent_Parallel(Job, buff, key: Pointer; Size: NativeInt);
    procedure Mars_Parallel(Job, buff, key: Pointer; Size: NativeInt);
    procedure Rijndael_Parallel(Job, buff, key: Pointer; Size: NativeInt);
    procedure BlockCBC_Parallel(Job, buff, key: Pointer; Size: NativeInt);
    procedure ParallelCipherCall(const JobData: PParallelCipherJobData; const FromIndex, ToIndex: Integer);
    procedure RunParallel(const JobData: PParallelCipherJobData; const Total, Depth: Integer);
  public
    BlockDepth: Integer;

    constructor Create;
    destructor Destroy; override;

    function DES64(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
    function DES128(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
    function DES192(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
    function Blowfish(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
    function LBC(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
    function LQC(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
    function TwoFish(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
    function XXTea512(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
    function RC6(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
    function Serpent(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
    function Mars(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
    function Rijndael(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
    procedure BlockCBC(sour: Pointer; Size: NativeInt; boxBuff: Pointer; boxSiz: NativeInt);

    function EncryptBuffer(cs: TCipherSecurity; sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
    function EncryptBufferCBC(cs: TCipherSecurity; sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
  end;

{$ENDIF}


var
  { system default cbc refrence }
  SystemCBC: TBytes;
{$IFDEF Parallel}
  { system default Parallel depth }
  DefaultParallelDepth: Integer;     // default cpucount * 2
  ParallelTriggerCondition: Integer; // default 1024
{$ENDIF}

procedure InitSysCBCAndDefaultKey(rand: Int64);

function SequEncryptWithDirect(const cs: TCipherSecurity; sour: Pointer; Size: NativeInt; const key: TCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean; overload;
function SequEncryptWithDirect(const ca: TCipherSecurityArray; sour: Pointer; Size: NativeInt; const key: TCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean; overload;

{$IFDEF Parallel}
function SequEncryptWithParallel(const cs: TCipherSecurity; sour: Pointer; Size: NativeInt; const key: TCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean; overload;
function SequEncryptWithParallel(const ca: TCipherSecurityArray; sour: Pointer; Size: NativeInt; const key: TCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean; overload;
{$ENDIF}

function SequEncrypt(const ca: TCipherSecurityArray; sour: Pointer; Size: NativeInt; const key: TCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean; overload;
function SequEncrypt(const cs: TCipherSecurity; sour: Pointer; Size: NativeInt; const key: TCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean; overload;

function SequEncryptCBCWithDirect(const cs: TCipherSecurity; sour: Pointer; Size: NativeInt; const key: TCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean; overload;
function SequEncryptCBCWithDirect(const ca: TCipherSecurityArray; sour: Pointer; Size: NativeInt; const key: TCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean; overload;

{$IFDEF Parallel}
function SequEncryptCBCWithParallel(const cs: TCipherSecurity; sour: Pointer; Size: NativeInt; const key: TCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean; overload;
function SequEncryptCBCWithParallel(const ca: TCipherSecurityArray; sour: Pointer; Size: NativeInt; const key: TCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean; overload;
{$ENDIF}

function SequEncryptCBC(const ca: TCipherSecurityArray; sour: Pointer; Size: NativeInt; const key: TCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean; overload;
function SequEncryptCBC(const cs: TCipherSecurity; sour: Pointer; Size: NativeInt; const key: TCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean; overload;

function GenerateSequHash(hssArry: THashSecuritys; sour: Pointer; Size: NativeInt): TPascalString; overload;
procedure GenerateSequHash(hssArry: THashSecuritys; sour: Pointer; Size: NativeInt; output: TListPascalString); overload;
procedure GenerateSequHash(hssArry: THashSecuritys; sour: Pointer; Size: NativeInt; output: TCoreClassStream); overload;

function CompareSequHash(HashVL: THashStringList; sour: Pointer; Size: NativeInt): Boolean; overload;
function CompareSequHash(hashData: TPascalString; sour: Pointer; Size: NativeInt): Boolean; overload;
function CompareSequHash(hashData: TListPascalString; sour: Pointer; Size: NativeInt): Boolean; overload;
function CompareSequHash(hashData: TCoreClassStream; sour: Pointer; Size: NativeInt): Boolean; overload;

function GenerateMemoryHash(hssArry: THashSecuritys; sour: Pointer; Size: NativeInt): TPascalString;
function CompareMemoryHash(sour: Pointer; Size: NativeInt; const hashBuff: TPascalString): Boolean;

function GeneratePasswordHash(hssArry: THashSecuritys; const passwd: TPascalString): TPascalString;
function ComparePasswordHash(const passwd, hashBuff: TPascalString): Boolean;

function GeneratePassword(const ca: TCipherSecurityArray; const passwd: TPascalString): TPascalString; overload;
function ComparePassword(const ca: TCipherSecurityArray; const passwd, passwdDataSource: TPascalString): Boolean; overload;

function GeneratePassword(const cs: TCipherSecurity; const passwd: TPascalString): TPascalString; overload;
function ComparePassword(const cs: TCipherSecurity; const passwd, passwdDataSource: TPascalString): Boolean; overload;

{
  QuantumCryptographyPassword: used sha-3 shake256 cryptography as 512 bits password

  SHA-3 (Secure Hash Algorithm 3) is the latest member of the Secure Hash Algorithm family of standards,
  released by NIST on August 5, 2015.[4][5] Although part of the same series of standards,
  SHA-3 is internally quite different from the MD5-like structure of SHA-1 and SHA-2.

  Keccak is based on a novel approach called sponge construction.
  Sponge construction is based on a wide random function or random permutation, and allows inputting ("absorbing" in sponge terminology) any amount of data,
  and outputting ("squeezing") any amount of data,
  while acting as a pseudorandom function with regard to all previous inputs. This leads to great flexibility.

  NIST does not currently plan to withdraw SHA-2 or remove it from the revised Secure Hash Standard.
  The purpose of SHA-3 is that it can be directly substituted for SHA-2 in current applications if necessary,
  and to significantly improve the robustness of NIST's overall hash algorithm toolkit

  ref wiki
  https://en.wikipedia.org/wiki/SHA-3
}
function GenerateQuantumCryptographyPassword(const passwd: TPascalString): TPascalString;
function CompareQuantumCryptographyPassword(const passwd, passwdDataSource: TPascalString): Boolean;

// QuantumCryptography for Stream support: used sha-3-512 cryptography as 512 bits password
procedure QuantumEncrypt(input, output: TCoreClassStream; SecurityLevel: Integer; key: TCipherKeyBuffer);
function QuantumDecrypt(input, output: TCoreClassStream; key: TCipherKeyBuffer): Boolean;

procedure TestCoreCipher;

{$REGION 'cryptAndHash'}


type
  { Blowfish Cipher }
  TBlowfish = class(TCoreClassObject)
  public
    class procedure EncryptBF(const Context: TBFContext; var Block: TBFBlock; Encrypt: Boolean);
    class procedure InitEncryptBF(key: TKey128; var Context: TBFContext);
  end;

  { DES Cipher }
  TDES = class(TCoreClassObject)
  strict private
    class procedure JoinBlock(const l, r: DWORD; var Block: TDESBlock);
    class procedure SplitBlock(const Block: TDESBlock; var l, r: DWORD);
  private
  public
    class procedure EncryptDES(const Context: TDESContext; var Block: TDESBlock);
    class procedure EncryptTripleDES(const Context: TTripleDESContext; var Block: TDESBlock);
    class procedure EncryptTripleDES3Key(const Context: TTripleDESContext3Key; var Block: TDESBlock);
    class procedure InitEncryptDES(const key: TKey64; var Context: TDESContext; Encrypt: Boolean);
    class procedure InitEncryptTripleDES(const key: TKey128; var Context: TTripleDESContext; Encrypt: Boolean);
    class procedure InitEncryptTripleDES3Key(const Key1, Key2, Key3: TKey64; var Context: TTripleDESContext3Key; Encrypt: Boolean);
    class procedure ShrinkDESKey(var key: TKey64);
  end;

  { SHA1 }
  TSHA1 = class(TCoreClassObject)
  strict private
    class procedure SHA1Clear(var Context: TSHA1Context);
    class procedure SHA1Hash(var Context: TSHA1Context);
    class function SHA1SwapByteOrder(n: DWORD): DWORD;
    class procedure SHA1UpdateLen(var Context: TSHA1Context; Len: DWORD);
  public
    class procedure FinalizeSHA1(var Context: TSHA1Context; var Digest: TSHA1Digest);
    class procedure SHA1(var Digest: TSHA1Digest; const Buf; BufSize: nativeUInt);
    class procedure InitSHA1(var Context: TSHA1Context);
    class procedure ByteBuffSHA1(var Digest: TSHA1Digest; const ABytes: TBytes);
    class procedure UpdateSHA1(var Context: TSHA1Context; const Buf; BufSize: nativeUInt);
  end;

  { SHA-2-SHA256 }
  TSHA256 = class(TCoreClassObject)
  private
    class procedure SwapDWORD(var a: DWORD);
    class procedure Compute(var Digest: TSHA256Digest; const buff: Pointer);
  public
    class procedure SHA256(var Digest: TSHA256Digest; const Buf; BufSize: nativeUInt);
  end;

  { SHA-2-SHA512 }
  TSHA512 = class(TCoreClassObject)
  private
    class procedure SwapQWORD(var a: UInt64);
    class procedure Compute(var Digest: TSHA512Digest; const buff: Pointer);
  public
    class procedure SHA512(var Digest: TSHA512Digest; const Buf; BufSize: UInt64);
  end;

  { SHA-3:SHA224,SHA256,SHA384,SHA512,SHAKE128,SHAKE256 }
  TSHA3 = class(TCoreClassObject)
  private type
    TSHA3Context = record
      HashLength: DWORD;
      BlockLen: DWORD;
      Buffer: array of Byte;
      BufSize: DWORD;
      a, b: array [0 .. 24] of UInt64;
      c, d: array [0 .. 4] of UInt64;
    end;
  private const
    // Round constants
    RC: array [0 .. 23] of UInt64 = (
      $0000000000000001, $0000000000008082, $800000000000808A, $8000000080008000,
      $000000000000808B, $0000000080000001, $8000000080008081, $8000000000008009,
      $000000000000008A, $0000000000000088, $0000000080008009, $000000008000000A,
      $000000008000808B, $800000000000008B, $8000000000008089, $8000000000008003,
      $8000000000008002, $8000000000000080, $000000000000800A, $800000008000000A,
      $8000000080008081, $8000000000008080, $0000000080000001, $8000000080008008);

    // Rotation offsets
    RO: array [0 .. 24] of Integer = (0, 1, 62, 28, 27, 36, 44, 6, 55, 20, 3, 10, 43, 25, 39, 41, 45, 15, 21, 8, 18, 2, 61, 56, 14);
  private
    class function ComputeX(const x: Integer): Integer; inline;
    class function ComputeXY(const x, y: Integer): Integer; inline;
    class procedure BlockSHA3(var Context: TSHA3Context);
    class procedure InitializeSHA3(var Context: TSHA3Context; HashLength: Integer);
    class procedure SHA3(var Context: TSHA3Context; Chunk: PByte; Size: NativeInt);
    class procedure FinalizeSHA3(var Context: TSHA3Context; const output: PCCByteArray);
    class procedure FinalizeSHAKE(var Context: TSHA3Context; Limit: Integer; const output: PCCByteArray);
  public
    class procedure SHA224(var Digest: TSHA3_224_Digest; Buf: PByte; BufSize: NativeInt);
    class procedure SHA256(var Digest: TSHA3_256_Digest; Buf: PByte; BufSize: NativeInt);
    class procedure SHA384(var Digest: TSHA3_384_Digest; Buf: PByte; BufSize: NativeInt);
    class procedure SHA512(var Digest: TSHA3_512_Digest; Buf: PByte; BufSize: NativeInt);
    class procedure SHAKE128(const Digest: PCCByteArray; Buf: PByte; BufSize: NativeInt; Limit: Integer);
    class procedure SHAKE256(const Digest: PCCByteArray; Buf: PByte; BufSize: NativeInt; Limit: Integer);
  end;

  { LBC Cipher }
  TLBC = class(TCoreClassObject)
  public
    class procedure EncryptLBC(const Context: TLBCContext; var Block: TLBCBlock);
    class procedure EncryptLQC(const key: TKey128; var Block: TLQCBlock; Encrypt: Boolean);
    class procedure InitEncryptLBC(const key: TKey128; var Context: TLBCContext; Rounds: Integer; Encrypt: Boolean);
  end;

  { MD5 }
  THashMD5 = class(TCoreClassObject)
  public
    class procedure FinalizeMD5(var Context: TMD5Context; var Digest: TMD5Digest);
    class procedure GenerateMD5Key(var key: TKey128; const ABytes: TBytes);
    class procedure HashMD5(var Digest: TMD5Digest; const Buf; BufSize: NativeInt);
    class procedure InitMD5(var Context: TMD5Context);
    class procedure ByteBuffHashMD5(var Digest: TMD5Digest; const ABytes: TBytes);
    class procedure UpdateMD5(var Context: TMD5Context; const Buf; BufSize: NativeInt);
  end;

  { message digest }
  THashMD = class(TCoreClassObject)
  public
    class procedure FinalizeLMD(var Context: TLMDContext; var Digest; DigestSize: Integer);
    class procedure GenerateLMDKey(var key; KeySize: Integer; const ABytes: TBytes);
    class procedure HashLMD(var Digest; DigestSize: Integer; const Buf; BufSize: NativeInt);
    class procedure InitLMD(var Context: TLMDContext);
    class procedure ByteBuffHashLMD(var Digest; DigestSize: Integer; const ABytes: TBytes);
    class procedure UpdateLMD(var Context: TLMDContext; const Buf; BufSize: NativeInt);
  end;

  { Random Number Cipher }
  TRNG = class(TCoreClassObject)
  public
    class procedure EncryptRNG32(var Context: TRNG32Context; var Buf; BufSize: Integer);
    class procedure EncryptRNG64(var Context: TRNG64Context; var Buf; BufSize: Integer);
    class procedure InitEncryptRNG32(key: DWORD; var Context: TRNG32Context);
    class procedure InitEncryptRNG64(KeyHi, KeyLo: DWORD; var Context: TRNG64Context);
  end;

  { LSC Stream Cipher }
  TLSC = class(TCoreClassObject)
  public
    class procedure EncryptLSC(var Context: TLSCContext; var Buf; BufSize: Integer);
    class procedure InitEncryptLSC(const key; KeySize: Integer; var Context: TLSCContext);
  end;

  { Miscellaneous algorithms }
  { Misc public utilities }
  TMISC = class(TCoreClassObject)
  public
    class procedure Mix128(var x: T128Bit); static;
    class function Ran0Prim(var Seed: Integer; IA, IQ, IR: Integer): Integer; static;
    class function Random64(var Seed: TInt64): Integer; static;
    class procedure Transform(var OutputBuffer: TTransformOutput; var InBuf: TTransformInput); static;
    class procedure GenerateRandomKey(var key; KeySize: Integer); static;
    class procedure HashELF(var Digest: DWORD; const Buf; BufSize: nativeUInt); static;
    class procedure HashELF64(var Digest: Int64; const Buf; BufSize: nativeUInt); static;
    class procedure HashMix128(var Digest: DWORD; const Buf; BufSize: NativeInt); static;
    class function Ran01(var Seed: Integer): Integer; static;
    class function Ran02(var Seed: Integer): Integer; static;
    class function Ran03(var Seed: Integer): Integer; static;
    class function Random32Byte(var Seed: Integer): Byte; static;
    class function Random64Byte(var Seed: TInt64): Byte; static;
    class function RolX(i, c: DWORD): DWORD; static;
    class procedure ByteBuffHashELF(var Digest: DWORD; const ABytes: TBytes); static;
    class procedure ByteBuffHashMix128(var Digest: DWORD; const ABytes: TBytes); static;
    class procedure XorMem(var Mem1; const Mem2; Count: NativeInt); static;
  end;

  { TEA }
procedure XXTEAEncrypt(var key: TKey128; var Block: TXXTEABlock);
procedure XXTEADecrypt(var key: TKey128; var Block: TXXTEABlock);

const
  { RC6 }
  cRC6_NumRounds = 20; { number of rounds must be between 16-24 }

type
  PRC6Key = ^TRC6Key;
  TRC6Key = array [0 .. ((cRC6_NumRounds * 2) + 3)] of DWORD;

  PRC6Block = ^TRC6Block;
  TRC6Block = array [0 .. 15] of Byte;

  TRC6 = class(TCoreClassObject)
  public
    class function LRot32(x, c: DWORD): DWORD;
    class function RRot32(x, c: DWORD): DWORD;
    class procedure InitKey(buff: Pointer; Size: Integer; var KeyContext: TRC6Key);
    class procedure Encrypt(var KeyContext: TRC6Key; var Data: TRC6Block);
    class procedure Decrypt(var KeyContext: TRC6Key; var Data: TRC6Block);
  end;

type
  { Serpent }
  PSerpentkey = ^TSerpentkey;
  TSerpentkey = array [0 .. 131] of DWORD;
  PSerpentBlock = ^TSerpentBlock;
  TSerpentBlock = array [0 .. 15] of Byte;

  TSerpent = class(TCoreClassObject)
  public
    class procedure InitKey(buff: Pointer; Size: Integer; var KeyContext: TSerpentkey);
    class procedure Encrypt(var KeyContext: TSerpentkey; var Data: TSerpentBlock);
    class procedure Decrypt(var KeyContext: TSerpentkey; var Data: TSerpentBlock);
  end;

type
  { Mars }
  PMarskey = ^TMarskey;
  TMarskey = array [0 .. 39] of DWORD;
  PMarsBlock = ^TMarsBlock;
  TMarsBlock = array [0 .. 15] of Byte;

  TMars = class(TCoreClassObject)
  public
    class procedure gen_mask(var x, m: DWORD);
    class procedure InitKey(buff: Pointer; Size: Integer; var KeyContext: TMarskey);
    class procedure Encrypt(var KeyContext: TMarskey; var Data: TMarsBlock);
    class procedure Decrypt(var KeyContext: TMarskey; var Data: TMarsBlock);
  end;

type
  { Rijndael }
  PRijndaelkey = ^TRijndaelkey;

  TRijndaelkey = packed record
    NumRounds: DWORD;
    rk, drk: array [0 .. 14, 0 .. 7] of DWORD;
  end;

  PRijndaelBlock = ^TRijndaelBlock;
  TRijndaelBlock = array [0 .. 15] of Byte;

  TRijndael = class(TCoreClassObject)
  private const
{$REGION 'RijndaelDefine'}
    T1: array [0 .. 255, 0 .. 3] of Byte = (
      ($C6, $63, $63, $A5), ($F8, $7C, $7C, $84), ($EE, $77, $77, $99), ($F6, $7B, $7B, $8D),
      ($FF, $F2, $F2, $0D), ($D6, $6B, $6B, $BD), ($DE, $6F, $6F, $B1), ($91, $C5, $C5, $54),
      ($60, $30, $30, $50), ($02, $01, $01, $03), ($CE, $67, $67, $A9), ($56, $2B, $2B, $7D),
      ($E7, $FE, $FE, $19), ($B5, $D7, $D7, $62), ($4D, $AB, $AB, $E6), ($EC, $76, $76, $9A),
      ($8F, $CA, $CA, $45), ($1F, $82, $82, $9D), ($89, $C9, $C9, $40), ($FA, $7D, $7D, $87),
      ($EF, $FA, $FA, $15), ($B2, $59, $59, $EB), ($8E, $47, $47, $C9), ($FB, $F0, $F0, $0B),
      ($41, $AD, $AD, $EC), ($B3, $D4, $D4, $67), ($5F, $A2, $A2, $FD), ($45, $AF, $AF, $EA),
      ($23, $9C, $9C, $BF), ($53, $A4, $A4, $F7), ($E4, $72, $72, $96), ($9B, $C0, $C0, $5B),
      ($75, $B7, $B7, $C2), ($E1, $FD, $FD, $1C), ($3D, $93, $93, $AE), ($4C, $26, $26, $6A),
      ($6C, $36, $36, $5A), ($7E, $3F, $3F, $41), ($F5, $F7, $F7, $02), ($83, $CC, $CC, $4F),
      ($68, $34, $34, $5C), ($51, $A5, $A5, $F4), ($D1, $E5, $E5, $34), ($F9, $F1, $F1, $08),
      ($E2, $71, $71, $93), ($AB, $D8, $D8, $73), ($62, $31, $31, $53), ($2A, $15, $15, $3F),
      ($08, $04, $04, $0C), ($95, $C7, $C7, $52), ($46, $23, $23, $65), ($9D, $C3, $C3, $5E),
      ($30, $18, $18, $28), ($37, $96, $96, $A1), ($0A, $05, $05, $0F), ($2F, $9A, $9A, $B5),
      ($0E, $07, $07, $09), ($24, $12, $12, $36), ($1B, $80, $80, $9B), ($DF, $E2, $E2, $3D),
      ($CD, $EB, $EB, $26), ($4E, $27, $27, $69), ($7F, $B2, $B2, $CD), ($EA, $75, $75, $9F),
      ($12, $09, $09, $1B), ($1D, $83, $83, $9E), ($58, $2C, $2C, $74), ($34, $1A, $1A, $2E),
      ($36, $1B, $1B, $2D), ($DC, $6E, $6E, $B2), ($B4, $5A, $5A, $EE), ($5B, $A0, $A0, $FB),
      ($A4, $52, $52, $F6), ($76, $3B, $3B, $4D), ($B7, $D6, $D6, $61), ($7D, $B3, $B3, $CE),
      ($52, $29, $29, $7B), ($DD, $E3, $E3, $3E), ($5E, $2F, $2F, $71), ($13, $84, $84, $97),
      ($A6, $53, $53, $F5), ($B9, $D1, $D1, $68), ($00, $00, $00, $00), ($C1, $ED, $ED, $2C),
      ($40, $20, $20, $60), ($E3, $FC, $FC, $1F), ($79, $B1, $B1, $C8), ($B6, $5B, $5B, $ED),
      ($D4, $6A, $6A, $BE), ($8D, $CB, $CB, $46), ($67, $BE, $BE, $D9), ($72, $39, $39, $4B),
      ($94, $4A, $4A, $DE), ($98, $4C, $4C, $D4), ($B0, $58, $58, $E8), ($85, $CF, $CF, $4A),
      ($BB, $D0, $D0, $6B), ($C5, $EF, $EF, $2A), ($4F, $AA, $AA, $E5), ($ED, $FB, $FB, $16),
      ($86, $43, $43, $C5), ($9A, $4D, $4D, $D7), ($66, $33, $33, $55), ($11, $85, $85, $94),
      ($8A, $45, $45, $CF), ($E9, $F9, $F9, $10), ($04, $02, $02, $06), ($FE, $7F, $7F, $81),
      ($A0, $50, $50, $F0), ($78, $3C, $3C, $44), ($25, $9F, $9F, $BA), ($4B, $A8, $A8, $E3),
      ($A2, $51, $51, $F3), ($5D, $A3, $A3, $FE), ($80, $40, $40, $C0), ($05, $8F, $8F, $8A),
      ($3F, $92, $92, $AD), ($21, $9D, $9D, $BC), ($70, $38, $38, $48), ($F1, $F5, $F5, $04),
      ($63, $BC, $BC, $DF), ($77, $B6, $B6, $C1), ($AF, $DA, $DA, $75), ($42, $21, $21, $63),
      ($20, $10, $10, $30), ($E5, $FF, $FF, $1A), ($FD, $F3, $F3, $0E), ($BF, $D2, $D2, $6D),
      ($81, $CD, $CD, $4C), ($18, $0C, $0C, $14), ($26, $13, $13, $35), ($C3, $EC, $EC, $2F),
      ($BE, $5F, $5F, $E1), ($35, $97, $97, $A2), ($88, $44, $44, $CC), ($2E, $17, $17, $39),
      ($93, $C4, $C4, $57), ($55, $A7, $A7, $F2), ($FC, $7E, $7E, $82), ($7A, $3D, $3D, $47),
      ($C8, $64, $64, $AC), ($BA, $5D, $5D, $E7), ($32, $19, $19, $2B), ($E6, $73, $73, $95),
      ($C0, $60, $60, $A0), ($19, $81, $81, $98), ($9E, $4F, $4F, $D1), ($A3, $DC, $DC, $7F),
      ($44, $22, $22, $66), ($54, $2A, $2A, $7E), ($3B, $90, $90, $AB), ($0B, $88, $88, $83),
      ($8C, $46, $46, $CA), ($C7, $EE, $EE, $29), ($6B, $B8, $B8, $D3), ($28, $14, $14, $3C),
      ($A7, $DE, $DE, $79), ($BC, $5E, $5E, $E2), ($16, $0B, $0B, $1D), ($AD, $DB, $DB, $76),
      ($DB, $E0, $E0, $3B), ($64, $32, $32, $56), ($74, $3A, $3A, $4E), ($14, $0A, $0A, $1E),
      ($92, $49, $49, $DB), ($0C, $06, $06, $0A), ($48, $24, $24, $6C), ($B8, $5C, $5C, $E4),
      ($9F, $C2, $C2, $5D), ($BD, $D3, $D3, $6E), ($43, $AC, $AC, $EF), ($C4, $62, $62, $A6),
      ($39, $91, $91, $A8), ($31, $95, $95, $A4), ($D3, $E4, $E4, $37), ($F2, $79, $79, $8B),
      ($D5, $E7, $E7, $32), ($8B, $C8, $C8, $43), ($6E, $37, $37, $59), ($DA, $6D, $6D, $B7),
      ($01, $8D, $8D, $8C), ($B1, $D5, $D5, $64), ($9C, $4E, $4E, $D2), ($49, $A9, $A9, $E0),
      ($D8, $6C, $6C, $B4), ($AC, $56, $56, $FA), ($F3, $F4, $F4, $07), ($CF, $EA, $EA, $25),
      ($CA, $65, $65, $AF), ($F4, $7A, $7A, $8E), ($47, $AE, $AE, $E9), ($10, $08, $08, $18),
      ($6F, $BA, $BA, $D5), ($F0, $78, $78, $88), ($4A, $25, $25, $6F), ($5C, $2E, $2E, $72),
      ($38, $1C, $1C, $24), ($57, $A6, $A6, $F1), ($73, $B4, $B4, $C7), ($97, $C6, $C6, $51),
      ($CB, $E8, $E8, $23), ($A1, $DD, $DD, $7C), ($E8, $74, $74, $9C), ($3E, $1F, $1F, $21),
      ($96, $4B, $4B, $DD), ($61, $BD, $BD, $DC), ($0D, $8B, $8B, $86), ($0F, $8A, $8A, $85),
      ($E0, $70, $70, $90), ($7C, $3E, $3E, $42), ($71, $B5, $B5, $C4), ($CC, $66, $66, $AA),
      ($90, $48, $48, $D8), ($06, $03, $03, $05), ($F7, $F6, $F6, $01), ($1C, $0E, $0E, $12),
      ($C2, $61, $61, $A3), ($6A, $35, $35, $5F), ($AE, $57, $57, $F9), ($69, $B9, $B9, $D0),
      ($17, $86, $86, $91), ($99, $C1, $C1, $58), ($3A, $1D, $1D, $27), ($27, $9E, $9E, $B9),
      ($D9, $E1, $E1, $38), ($EB, $F8, $F8, $13), ($2B, $98, $98, $B3), ($22, $11, $11, $33),
      ($D2, $69, $69, $BB), ($A9, $D9, $D9, $70), ($07, $8E, $8E, $89), ($33, $94, $94, $A7),
      ($2D, $9B, $9B, $B6), ($3C, $1E, $1E, $22), ($15, $87, $87, $92), ($C9, $E9, $E9, $20),
      ($87, $CE, $CE, $49), ($AA, $55, $55, $FF), ($50, $28, $28, $78), ($A5, $DF, $DF, $7A),
      ($03, $8C, $8C, $8F), ($59, $A1, $A1, $F8), ($09, $89, $89, $80), ($1A, $0D, $0D, $17),
      ($65, $BF, $BF, $DA), ($D7, $E6, $E6, $31), ($84, $42, $42, $C6), ($D0, $68, $68, $B8),
      ($82, $41, $41, $C3), ($29, $99, $99, $B0), ($5A, $2D, $2D, $77), ($1E, $0F, $0F, $11),
      ($7B, $B0, $B0, $CB), ($A8, $54, $54, $FC), ($6D, $BB, $BB, $D6), ($2C, $16, $16, $3A));
    T2: array [0 .. 255, 0 .. 3] of Byte = (
      ($A5, $C6, $63, $63), ($84, $F8, $7C, $7C), ($99, $EE, $77, $77), ($8D, $F6, $7B, $7B),
      ($0D, $FF, $F2, $F2), ($BD, $D6, $6B, $6B), ($B1, $DE, $6F, $6F), ($54, $91, $C5, $C5),
      ($50, $60, $30, $30), ($03, $02, $01, $01), ($A9, $CE, $67, $67), ($7D, $56, $2B, $2B),
      ($19, $E7, $FE, $FE), ($62, $B5, $D7, $D7), ($E6, $4D, $AB, $AB), ($9A, $EC, $76, $76),
      ($45, $8F, $CA, $CA), ($9D, $1F, $82, $82), ($40, $89, $C9, $C9), ($87, $FA, $7D, $7D),
      ($15, $EF, $FA, $FA), ($EB, $B2, $59, $59), ($C9, $8E, $47, $47), ($0B, $FB, $F0, $F0),
      ($EC, $41, $AD, $AD), ($67, $B3, $D4, $D4), ($FD, $5F, $A2, $A2), ($EA, $45, $AF, $AF),
      ($BF, $23, $9C, $9C), ($F7, $53, $A4, $A4), ($96, $E4, $72, $72), ($5B, $9B, $C0, $C0),
      ($C2, $75, $B7, $B7), ($1C, $E1, $FD, $FD), ($AE, $3D, $93, $93), ($6A, $4C, $26, $26),
      ($5A, $6C, $36, $36), ($41, $7E, $3F, $3F), ($02, $F5, $F7, $F7), ($4F, $83, $CC, $CC),
      ($5C, $68, $34, $34), ($F4, $51, $A5, $A5), ($34, $D1, $E5, $E5), ($08, $F9, $F1, $F1),
      ($93, $E2, $71, $71), ($73, $AB, $D8, $D8), ($53, $62, $31, $31), ($3F, $2A, $15, $15),
      ($0C, $08, $04, $04), ($52, $95, $C7, $C7), ($65, $46, $23, $23), ($5E, $9D, $C3, $C3),
      ($28, $30, $18, $18), ($A1, $37, $96, $96), ($0F, $0A, $05, $05), ($B5, $2F, $9A, $9A),
      ($09, $0E, $07, $07), ($36, $24, $12, $12), ($9B, $1B, $80, $80), ($3D, $DF, $E2, $E2),
      ($26, $CD, $EB, $EB), ($69, $4E, $27, $27), ($CD, $7F, $B2, $B2), ($9F, $EA, $75, $75),
      ($1B, $12, $09, $09), ($9E, $1D, $83, $83), ($74, $58, $2C, $2C), ($2E, $34, $1A, $1A),
      ($2D, $36, $1B, $1B), ($B2, $DC, $6E, $6E), ($EE, $B4, $5A, $5A), ($FB, $5B, $A0, $A0),
      ($F6, $A4, $52, $52), ($4D, $76, $3B, $3B), ($61, $B7, $D6, $D6), ($CE, $7D, $B3, $B3),
      ($7B, $52, $29, $29), ($3E, $DD, $E3, $E3), ($71, $5E, $2F, $2F), ($97, $13, $84, $84),
      ($F5, $A6, $53, $53), ($68, $B9, $D1, $D1), ($00, $00, $00, $00), ($2C, $C1, $ED, $ED),
      ($60, $40, $20, $20), ($1F, $E3, $FC, $FC), ($C8, $79, $B1, $B1), ($ED, $B6, $5B, $5B),
      ($BE, $D4, $6A, $6A), ($46, $8D, $CB, $CB), ($D9, $67, $BE, $BE), ($4B, $72, $39, $39),
      ($DE, $94, $4A, $4A), ($D4, $98, $4C, $4C), ($E8, $B0, $58, $58), ($4A, $85, $CF, $CF),
      ($6B, $BB, $D0, $D0), ($2A, $C5, $EF, $EF), ($E5, $4F, $AA, $AA), ($16, $ED, $FB, $FB),
      ($C5, $86, $43, $43), ($D7, $9A, $4D, $4D), ($55, $66, $33, $33), ($94, $11, $85, $85),
      ($CF, $8A, $45, $45), ($10, $E9, $F9, $F9), ($06, $04, $02, $02), ($81, $FE, $7F, $7F),
      ($F0, $A0, $50, $50), ($44, $78, $3C, $3C), ($BA, $25, $9F, $9F), ($E3, $4B, $A8, $A8),
      ($F3, $A2, $51, $51), ($FE, $5D, $A3, $A3), ($C0, $80, $40, $40), ($8A, $05, $8F, $8F),
      ($AD, $3F, $92, $92), ($BC, $21, $9D, $9D), ($48, $70, $38, $38), ($04, $F1, $F5, $F5),
      ($DF, $63, $BC, $BC), ($C1, $77, $B6, $B6), ($75, $AF, $DA, $DA), ($63, $42, $21, $21),
      ($30, $20, $10, $10), ($1A, $E5, $FF, $FF), ($0E, $FD, $F3, $F3), ($6D, $BF, $D2, $D2),
      ($4C, $81, $CD, $CD), ($14, $18, $0C, $0C), ($35, $26, $13, $13), ($2F, $C3, $EC, $EC),
      ($E1, $BE, $5F, $5F), ($A2, $35, $97, $97), ($CC, $88, $44, $44), ($39, $2E, $17, $17),
      ($57, $93, $C4, $C4), ($F2, $55, $A7, $A7), ($82, $FC, $7E, $7E), ($47, $7A, $3D, $3D),
      ($AC, $C8, $64, $64), ($E7, $BA, $5D, $5D), ($2B, $32, $19, $19), ($95, $E6, $73, $73),
      ($A0, $C0, $60, $60), ($98, $19, $81, $81), ($D1, $9E, $4F, $4F), ($7F, $A3, $DC, $DC),
      ($66, $44, $22, $22), ($7E, $54, $2A, $2A), ($AB, $3B, $90, $90), ($83, $0B, $88, $88),
      ($CA, $8C, $46, $46), ($29, $C7, $EE, $EE), ($D3, $6B, $B8, $B8), ($3C, $28, $14, $14),
      ($79, $A7, $DE, $DE), ($E2, $BC, $5E, $5E), ($1D, $16, $0B, $0B), ($76, $AD, $DB, $DB),
      ($3B, $DB, $E0, $E0), ($56, $64, $32, $32), ($4E, $74, $3A, $3A), ($1E, $14, $0A, $0A),
      ($DB, $92, $49, $49), ($0A, $0C, $06, $06), ($6C, $48, $24, $24), ($E4, $B8, $5C, $5C),
      ($5D, $9F, $C2, $C2), ($6E, $BD, $D3, $D3), ($EF, $43, $AC, $AC), ($A6, $C4, $62, $62),
      ($A8, $39, $91, $91), ($A4, $31, $95, $95), ($37, $D3, $E4, $E4), ($8B, $F2, $79, $79),
      ($32, $D5, $E7, $E7), ($43, $8B, $C8, $C8), ($59, $6E, $37, $37), ($B7, $DA, $6D, $6D),
      ($8C, $01, $8D, $8D), ($64, $B1, $D5, $D5), ($D2, $9C, $4E, $4E), ($E0, $49, $A9, $A9),
      ($B4, $D8, $6C, $6C), ($FA, $AC, $56, $56), ($07, $F3, $F4, $F4), ($25, $CF, $EA, $EA),
      ($AF, $CA, $65, $65), ($8E, $F4, $7A, $7A), ($E9, $47, $AE, $AE), ($18, $10, $08, $08),
      ($D5, $6F, $BA, $BA), ($88, $F0, $78, $78), ($6F, $4A, $25, $25), ($72, $5C, $2E, $2E),
      ($24, $38, $1C, $1C), ($F1, $57, $A6, $A6), ($C7, $73, $B4, $B4), ($51, $97, $C6, $C6),
      ($23, $CB, $E8, $E8), ($7C, $A1, $DD, $DD), ($9C, $E8, $74, $74), ($21, $3E, $1F, $1F),
      ($DD, $96, $4B, $4B), ($DC, $61, $BD, $BD), ($86, $0D, $8B, $8B), ($85, $0F, $8A, $8A),
      ($90, $E0, $70, $70), ($42, $7C, $3E, $3E), ($C4, $71, $B5, $B5), ($AA, $CC, $66, $66),
      ($D8, $90, $48, $48), ($05, $06, $03, $03), ($01, $F7, $F6, $F6), ($12, $1C, $0E, $0E),
      ($A3, $C2, $61, $61), ($5F, $6A, $35, $35), ($F9, $AE, $57, $57), ($D0, $69, $B9, $B9),
      ($91, $17, $86, $86), ($58, $99, $C1, $C1), ($27, $3A, $1D, $1D), ($B9, $27, $9E, $9E),
      ($38, $D9, $E1, $E1), ($13, $EB, $F8, $F8), ($B3, $2B, $98, $98), ($33, $22, $11, $11),
      ($BB, $D2, $69, $69), ($70, $A9, $D9, $D9), ($89, $07, $8E, $8E), ($A7, $33, $94, $94),
      ($B6, $2D, $9B, $9B), ($22, $3C, $1E, $1E), ($92, $15, $87, $87), ($20, $C9, $E9, $E9),
      ($49, $87, $CE, $CE), ($FF, $AA, $55, $55), ($78, $50, $28, $28), ($7A, $A5, $DF, $DF),
      ($8F, $03, $8C, $8C), ($F8, $59, $A1, $A1), ($80, $09, $89, $89), ($17, $1A, $0D, $0D),
      ($DA, $65, $BF, $BF), ($31, $D7, $E6, $E6), ($C6, $84, $42, $42), ($B8, $D0, $68, $68),
      ($C3, $82, $41, $41), ($B0, $29, $99, $99), ($77, $5A, $2D, $2D), ($11, $1E, $0F, $0F),
      ($CB, $7B, $B0, $B0), ($FC, $A8, $54, $54), ($D6, $6D, $BB, $BB), ($3A, $2C, $16, $16));
    T3: array [0 .. 255, 0 .. 3] of Byte = (
      ($63, $A5, $C6, $63), ($7C, $84, $F8, $7C), ($77, $99, $EE, $77), ($7B, $8D, $F6, $7B),
      ($F2, $0D, $FF, $F2), ($6B, $BD, $D6, $6B), ($6F, $B1, $DE, $6F), ($C5, $54, $91, $C5),
      ($30, $50, $60, $30), ($01, $03, $02, $01), ($67, $A9, $CE, $67), ($2B, $7D, $56, $2B),
      ($FE, $19, $E7, $FE), ($D7, $62, $B5, $D7), ($AB, $E6, $4D, $AB), ($76, $9A, $EC, $76),
      ($CA, $45, $8F, $CA), ($82, $9D, $1F, $82), ($C9, $40, $89, $C9), ($7D, $87, $FA, $7D),
      ($FA, $15, $EF, $FA), ($59, $EB, $B2, $59), ($47, $C9, $8E, $47), ($F0, $0B, $FB, $F0),
      ($AD, $EC, $41, $AD), ($D4, $67, $B3, $D4), ($A2, $FD, $5F, $A2), ($AF, $EA, $45, $AF),
      ($9C, $BF, $23, $9C), ($A4, $F7, $53, $A4), ($72, $96, $E4, $72), ($C0, $5B, $9B, $C0),
      ($B7, $C2, $75, $B7), ($FD, $1C, $E1, $FD), ($93, $AE, $3D, $93), ($26, $6A, $4C, $26),
      ($36, $5A, $6C, $36), ($3F, $41, $7E, $3F), ($F7, $02, $F5, $F7), ($CC, $4F, $83, $CC),
      ($34, $5C, $68, $34), ($A5, $F4, $51, $A5), ($E5, $34, $D1, $E5), ($F1, $08, $F9, $F1),
      ($71, $93, $E2, $71), ($D8, $73, $AB, $D8), ($31, $53, $62, $31), ($15, $3F, $2A, $15),
      ($04, $0C, $08, $04), ($C7, $52, $95, $C7), ($23, $65, $46, $23), ($C3, $5E, $9D, $C3),
      ($18, $28, $30, $18), ($96, $A1, $37, $96), ($05, $0F, $0A, $05), ($9A, $B5, $2F, $9A),
      ($07, $09, $0E, $07), ($12, $36, $24, $12), ($80, $9B, $1B, $80), ($E2, $3D, $DF, $E2),
      ($EB, $26, $CD, $EB), ($27, $69, $4E, $27), ($B2, $CD, $7F, $B2), ($75, $9F, $EA, $75),
      ($09, $1B, $12, $09), ($83, $9E, $1D, $83), ($2C, $74, $58, $2C), ($1A, $2E, $34, $1A),
      ($1B, $2D, $36, $1B), ($6E, $B2, $DC, $6E), ($5A, $EE, $B4, $5A), ($A0, $FB, $5B, $A0),
      ($52, $F6, $A4, $52), ($3B, $4D, $76, $3B), ($D6, $61, $B7, $D6), ($B3, $CE, $7D, $B3),
      ($29, $7B, $52, $29), ($E3, $3E, $DD, $E3), ($2F, $71, $5E, $2F), ($84, $97, $13, $84),
      ($53, $F5, $A6, $53), ($D1, $68, $B9, $D1), ($00, $00, $00, $00), ($ED, $2C, $C1, $ED),
      ($20, $60, $40, $20), ($FC, $1F, $E3, $FC), ($B1, $C8, $79, $B1), ($5B, $ED, $B6, $5B),
      ($6A, $BE, $D4, $6A), ($CB, $46, $8D, $CB), ($BE, $D9, $67, $BE), ($39, $4B, $72, $39),
      ($4A, $DE, $94, $4A), ($4C, $D4, $98, $4C), ($58, $E8, $B0, $58), ($CF, $4A, $85, $CF),
      ($D0, $6B, $BB, $D0), ($EF, $2A, $C5, $EF), ($AA, $E5, $4F, $AA), ($FB, $16, $ED, $FB),
      ($43, $C5, $86, $43), ($4D, $D7, $9A, $4D), ($33, $55, $66, $33), ($85, $94, $11, $85),
      ($45, $CF, $8A, $45), ($F9, $10, $E9, $F9), ($02, $06, $04, $02), ($7F, $81, $FE, $7F),
      ($50, $F0, $A0, $50), ($3C, $44, $78, $3C), ($9F, $BA, $25, $9F), ($A8, $E3, $4B, $A8),
      ($51, $F3, $A2, $51), ($A3, $FE, $5D, $A3), ($40, $C0, $80, $40), ($8F, $8A, $05, $8F),
      ($92, $AD, $3F, $92), ($9D, $BC, $21, $9D), ($38, $48, $70, $38), ($F5, $04, $F1, $F5),
      ($BC, $DF, $63, $BC), ($B6, $C1, $77, $B6), ($DA, $75, $AF, $DA), ($21, $63, $42, $21),
      ($10, $30, $20, $10), ($FF, $1A, $E5, $FF), ($F3, $0E, $FD, $F3), ($D2, $6D, $BF, $D2),
      ($CD, $4C, $81, $CD), ($0C, $14, $18, $0C), ($13, $35, $26, $13), ($EC, $2F, $C3, $EC),
      ($5F, $E1, $BE, $5F), ($97, $A2, $35, $97), ($44, $CC, $88, $44), ($17, $39, $2E, $17),
      ($C4, $57, $93, $C4), ($A7, $F2, $55, $A7), ($7E, $82, $FC, $7E), ($3D, $47, $7A, $3D),
      ($64, $AC, $C8, $64), ($5D, $E7, $BA, $5D), ($19, $2B, $32, $19), ($73, $95, $E6, $73),
      ($60, $A0, $C0, $60), ($81, $98, $19, $81), ($4F, $D1, $9E, $4F), ($DC, $7F, $A3, $DC),
      ($22, $66, $44, $22), ($2A, $7E, $54, $2A), ($90, $AB, $3B, $90), ($88, $83, $0B, $88),
      ($46, $CA, $8C, $46), ($EE, $29, $C7, $EE), ($B8, $D3, $6B, $B8), ($14, $3C, $28, $14),
      ($DE, $79, $A7, $DE), ($5E, $E2, $BC, $5E), ($0B, $1D, $16, $0B), ($DB, $76, $AD, $DB),
      ($E0, $3B, $DB, $E0), ($32, $56, $64, $32), ($3A, $4E, $74, $3A), ($0A, $1E, $14, $0A),
      ($49, $DB, $92, $49), ($06, $0A, $0C, $06), ($24, $6C, $48, $24), ($5C, $E4, $B8, $5C),
      ($C2, $5D, $9F, $C2), ($D3, $6E, $BD, $D3), ($AC, $EF, $43, $AC), ($62, $A6, $C4, $62),
      ($91, $A8, $39, $91), ($95, $A4, $31, $95), ($E4, $37, $D3, $E4), ($79, $8B, $F2, $79),
      ($E7, $32, $D5, $E7), ($C8, $43, $8B, $C8), ($37, $59, $6E, $37), ($6D, $B7, $DA, $6D),
      ($8D, $8C, $01, $8D), ($D5, $64, $B1, $D5), ($4E, $D2, $9C, $4E), ($A9, $E0, $49, $A9),
      ($6C, $B4, $D8, $6C), ($56, $FA, $AC, $56), ($F4, $07, $F3, $F4), ($EA, $25, $CF, $EA),
      ($65, $AF, $CA, $65), ($7A, $8E, $F4, $7A), ($AE, $E9, $47, $AE), ($08, $18, $10, $08),
      ($BA, $D5, $6F, $BA), ($78, $88, $F0, $78), ($25, $6F, $4A, $25), ($2E, $72, $5C, $2E),
      ($1C, $24, $38, $1C), ($A6, $F1, $57, $A6), ($B4, $C7, $73, $B4), ($C6, $51, $97, $C6),
      ($E8, $23, $CB, $E8), ($DD, $7C, $A1, $DD), ($74, $9C, $E8, $74), ($1F, $21, $3E, $1F),
      ($4B, $DD, $96, $4B), ($BD, $DC, $61, $BD), ($8B, $86, $0D, $8B), ($8A, $85, $0F, $8A),
      ($70, $90, $E0, $70), ($3E, $42, $7C, $3E), ($B5, $C4, $71, $B5), ($66, $AA, $CC, $66),
      ($48, $D8, $90, $48), ($03, $05, $06, $03), ($F6, $01, $F7, $F6), ($0E, $12, $1C, $0E),
      ($61, $A3, $C2, $61), ($35, $5F, $6A, $35), ($57, $F9, $AE, $57), ($B9, $D0, $69, $B9),
      ($86, $91, $17, $86), ($C1, $58, $99, $C1), ($1D, $27, $3A, $1D), ($9E, $B9, $27, $9E),
      ($E1, $38, $D9, $E1), ($F8, $13, $EB, $F8), ($98, $B3, $2B, $98), ($11, $33, $22, $11),
      ($69, $BB, $D2, $69), ($D9, $70, $A9, $D9), ($8E, $89, $07, $8E), ($94, $A7, $33, $94),
      ($9B, $B6, $2D, $9B), ($1E, $22, $3C, $1E), ($87, $92, $15, $87), ($E9, $20, $C9, $E9),
      ($CE, $49, $87, $CE), ($55, $FF, $AA, $55), ($28, $78, $50, $28), ($DF, $7A, $A5, $DF),
      ($8C, $8F, $03, $8C), ($A1, $F8, $59, $A1), ($89, $80, $09, $89), ($0D, $17, $1A, $0D),
      ($BF, $DA, $65, $BF), ($E6, $31, $D7, $E6), ($42, $C6, $84, $42), ($68, $B8, $D0, $68),
      ($41, $C3, $82, $41), ($99, $B0, $29, $99), ($2D, $77, $5A, $2D), ($0F, $11, $1E, $0F),
      ($B0, $CB, $7B, $B0), ($54, $FC, $A8, $54), ($BB, $D6, $6D, $BB), ($16, $3A, $2C, $16));
    T4: array [0 .. 255, 0 .. 3] of Byte = (
      ($63, $63, $A5, $C6), ($7C, $7C, $84, $F8), ($77, $77, $99, $EE), ($7B, $7B, $8D, $F6),
      ($F2, $F2, $0D, $FF), ($6B, $6B, $BD, $D6), ($6F, $6F, $B1, $DE), ($C5, $C5, $54, $91),
      ($30, $30, $50, $60), ($01, $01, $03, $02), ($67, $67, $A9, $CE), ($2B, $2B, $7D, $56),
      ($FE, $FE, $19, $E7), ($D7, $D7, $62, $B5), ($AB, $AB, $E6, $4D), ($76, $76, $9A, $EC),
      ($CA, $CA, $45, $8F), ($82, $82, $9D, $1F), ($C9, $C9, $40, $89), ($7D, $7D, $87, $FA),
      ($FA, $FA, $15, $EF), ($59, $59, $EB, $B2), ($47, $47, $C9, $8E), ($F0, $F0, $0B, $FB),
      ($AD, $AD, $EC, $41), ($D4, $D4, $67, $B3), ($A2, $A2, $FD, $5F), ($AF, $AF, $EA, $45),
      ($9C, $9C, $BF, $23), ($A4, $A4, $F7, $53), ($72, $72, $96, $E4), ($C0, $C0, $5B, $9B),
      ($B7, $B7, $C2, $75), ($FD, $FD, $1C, $E1), ($93, $93, $AE, $3D), ($26, $26, $6A, $4C),
      ($36, $36, $5A, $6C), ($3F, $3F, $41, $7E), ($F7, $F7, $02, $F5), ($CC, $CC, $4F, $83),
      ($34, $34, $5C, $68), ($A5, $A5, $F4, $51), ($E5, $E5, $34, $D1), ($F1, $F1, $08, $F9),
      ($71, $71, $93, $E2), ($D8, $D8, $73, $AB), ($31, $31, $53, $62), ($15, $15, $3F, $2A),
      ($04, $04, $0C, $08), ($C7, $C7, $52, $95), ($23, $23, $65, $46), ($C3, $C3, $5E, $9D),
      ($18, $18, $28, $30), ($96, $96, $A1, $37), ($05, $05, $0F, $0A), ($9A, $9A, $B5, $2F),
      ($07, $07, $09, $0E), ($12, $12, $36, $24), ($80, $80, $9B, $1B), ($E2, $E2, $3D, $DF),
      ($EB, $EB, $26, $CD), ($27, $27, $69, $4E), ($B2, $B2, $CD, $7F), ($75, $75, $9F, $EA),
      ($09, $09, $1B, $12), ($83, $83, $9E, $1D), ($2C, $2C, $74, $58), ($1A, $1A, $2E, $34),
      ($1B, $1B, $2D, $36), ($6E, $6E, $B2, $DC), ($5A, $5A, $EE, $B4), ($A0, $A0, $FB, $5B),
      ($52, $52, $F6, $A4), ($3B, $3B, $4D, $76), ($D6, $D6, $61, $B7), ($B3, $B3, $CE, $7D),
      ($29, $29, $7B, $52), ($E3, $E3, $3E, $DD), ($2F, $2F, $71, $5E), ($84, $84, $97, $13),
      ($53, $53, $F5, $A6), ($D1, $D1, $68, $B9), ($00, $00, $00, $00), ($ED, $ED, $2C, $C1),
      ($20, $20, $60, $40), ($FC, $FC, $1F, $E3), ($B1, $B1, $C8, $79), ($5B, $5B, $ED, $B6),
      ($6A, $6A, $BE, $D4), ($CB, $CB, $46, $8D), ($BE, $BE, $D9, $67), ($39, $39, $4B, $72),
      ($4A, $4A, $DE, $94), ($4C, $4C, $D4, $98), ($58, $58, $E8, $B0), ($CF, $CF, $4A, $85),
      ($D0, $D0, $6B, $BB), ($EF, $EF, $2A, $C5), ($AA, $AA, $E5, $4F), ($FB, $FB, $16, $ED),
      ($43, $43, $C5, $86), ($4D, $4D, $D7, $9A), ($33, $33, $55, $66), ($85, $85, $94, $11),
      ($45, $45, $CF, $8A), ($F9, $F9, $10, $E9), ($02, $02, $06, $04), ($7F, $7F, $81, $FE),
      ($50, $50, $F0, $A0), ($3C, $3C, $44, $78), ($9F, $9F, $BA, $25), ($A8, $A8, $E3, $4B),
      ($51, $51, $F3, $A2), ($A3, $A3, $FE, $5D), ($40, $40, $C0, $80), ($8F, $8F, $8A, $05),
      ($92, $92, $AD, $3F), ($9D, $9D, $BC, $21), ($38, $38, $48, $70), ($F5, $F5, $04, $F1),
      ($BC, $BC, $DF, $63), ($B6, $B6, $C1, $77), ($DA, $DA, $75, $AF), ($21, $21, $63, $42),
      ($10, $10, $30, $20), ($FF, $FF, $1A, $E5), ($F3, $F3, $0E, $FD), ($D2, $D2, $6D, $BF),
      ($CD, $CD, $4C, $81), ($0C, $0C, $14, $18), ($13, $13, $35, $26), ($EC, $EC, $2F, $C3),
      ($5F, $5F, $E1, $BE), ($97, $97, $A2, $35), ($44, $44, $CC, $88), ($17, $17, $39, $2E),
      ($C4, $C4, $57, $93), ($A7, $A7, $F2, $55), ($7E, $7E, $82, $FC), ($3D, $3D, $47, $7A),
      ($64, $64, $AC, $C8), ($5D, $5D, $E7, $BA), ($19, $19, $2B, $32), ($73, $73, $95, $E6),
      ($60, $60, $A0, $C0), ($81, $81, $98, $19), ($4F, $4F, $D1, $9E), ($DC, $DC, $7F, $A3),
      ($22, $22, $66, $44), ($2A, $2A, $7E, $54), ($90, $90, $AB, $3B), ($88, $88, $83, $0B),
      ($46, $46, $CA, $8C), ($EE, $EE, $29, $C7), ($B8, $B8, $D3, $6B), ($14, $14, $3C, $28),
      ($DE, $DE, $79, $A7), ($5E, $5E, $E2, $BC), ($0B, $0B, $1D, $16), ($DB, $DB, $76, $AD),
      ($E0, $E0, $3B, $DB), ($32, $32, $56, $64), ($3A, $3A, $4E, $74), ($0A, $0A, $1E, $14),
      ($49, $49, $DB, $92), ($06, $06, $0A, $0C), ($24, $24, $6C, $48), ($5C, $5C, $E4, $B8),
      ($C2, $C2, $5D, $9F), ($D3, $D3, $6E, $BD), ($AC, $AC, $EF, $43), ($62, $62, $A6, $C4),
      ($91, $91, $A8, $39), ($95, $95, $A4, $31), ($E4, $E4, $37, $D3), ($79, $79, $8B, $F2),
      ($E7, $E7, $32, $D5), ($C8, $C8, $43, $8B), ($37, $37, $59, $6E), ($6D, $6D, $B7, $DA),
      ($8D, $8D, $8C, $01), ($D5, $D5, $64, $B1), ($4E, $4E, $D2, $9C), ($A9, $A9, $E0, $49),
      ($6C, $6C, $B4, $D8), ($56, $56, $FA, $AC), ($F4, $F4, $07, $F3), ($EA, $EA, $25, $CF),
      ($65, $65, $AF, $CA), ($7A, $7A, $8E, $F4), ($AE, $AE, $E9, $47), ($08, $08, $18, $10),
      ($BA, $BA, $D5, $6F), ($78, $78, $88, $F0), ($25, $25, $6F, $4A), ($2E, $2E, $72, $5C),
      ($1C, $1C, $24, $38), ($A6, $A6, $F1, $57), ($B4, $B4, $C7, $73), ($C6, $C6, $51, $97),
      ($E8, $E8, $23, $CB), ($DD, $DD, $7C, $A1), ($74, $74, $9C, $E8), ($1F, $1F, $21, $3E),
      ($4B, $4B, $DD, $96), ($BD, $BD, $DC, $61), ($8B, $8B, $86, $0D), ($8A, $8A, $85, $0F),
      ($70, $70, $90, $E0), ($3E, $3E, $42, $7C), ($B5, $B5, $C4, $71), ($66, $66, $AA, $CC),
      ($48, $48, $D8, $90), ($03, $03, $05, $06), ($F6, $F6, $01, $F7), ($0E, $0E, $12, $1C),
      ($61, $61, $A3, $C2), ($35, $35, $5F, $6A), ($57, $57, $F9, $AE), ($B9, $B9, $D0, $69),
      ($86, $86, $91, $17), ($C1, $C1, $58, $99), ($1D, $1D, $27, $3A), ($9E, $9E, $B9, $27),
      ($E1, $E1, $38, $D9), ($F8, $F8, $13, $EB), ($98, $98, $B3, $2B), ($11, $11, $33, $22),
      ($69, $69, $BB, $D2), ($D9, $D9, $70, $A9), ($8E, $8E, $89, $07), ($94, $94, $A7, $33),
      ($9B, $9B, $B6, $2D), ($1E, $1E, $22, $3C), ($87, $87, $92, $15), ($E9, $E9, $20, $C9),
      ($CE, $CE, $49, $87), ($55, $55, $FF, $AA), ($28, $28, $78, $50), ($DF, $DF, $7A, $A5),
      ($8C, $8C, $8F, $03), ($A1, $A1, $F8, $59), ($89, $89, $80, $09), ($0D, $0D, $17, $1A),
      ($BF, $BF, $DA, $65), ($E6, $E6, $31, $D7), ($42, $42, $C6, $84), ($68, $68, $B8, $D0),
      ($41, $41, $C3, $82), ($99, $99, $B0, $29), ($2D, $2D, $77, $5A), ($0F, $0F, $11, $1E),
      ($B0, $B0, $CB, $7B), ($54, $54, $FC, $A8), ($BB, $BB, $D6, $6D), ($16, $16, $3A, $2C));
    T5: array [0 .. 255, 0 .. 3] of Byte = (
      ($51, $F4, $A7, $50), ($7E, $41, $65, $53), ($1A, $17, $A4, $C3), ($3A, $27, $5E, $96),
      ($3B, $AB, $6B, $CB), ($1F, $9D, $45, $F1), ($AC, $FA, $58, $AB), ($4B, $E3, $03, $93),
      ($20, $30, $FA, $55), ($AD, $76, $6D, $F6), ($88, $CC, $76, $91), ($F5, $02, $4C, $25),
      ($4F, $E5, $D7, $FC), ($C5, $2A, $CB, $D7), ($26, $35, $44, $80), ($B5, $62, $A3, $8F),
      ($DE, $B1, $5A, $49), ($25, $BA, $1B, $67), ($45, $EA, $0E, $98), ($5D, $FE, $C0, $E1),
      ($C3, $2F, $75, $02), ($81, $4C, $F0, $12), ($8D, $46, $97, $A3), ($6B, $D3, $F9, $C6),
      ($03, $8F, $5F, $E7), ($15, $92, $9C, $95), ($BF, $6D, $7A, $EB), ($95, $52, $59, $DA),
      ($D4, $BE, $83, $2D), ($58, $74, $21, $D3), ($49, $E0, $69, $29), ($8E, $C9, $C8, $44),
      ($75, $C2, $89, $6A), ($F4, $8E, $79, $78), ($99, $58, $3E, $6B), ($27, $B9, $71, $DD),
      ($BE, $E1, $4F, $B6), ($F0, $88, $AD, $17), ($C9, $20, $AC, $66), ($7D, $CE, $3A, $B4),
      ($63, $DF, $4A, $18), ($E5, $1A, $31, $82), ($97, $51, $33, $60), ($62, $53, $7F, $45),
      ($B1, $64, $77, $E0), ($BB, $6B, $AE, $84), ($FE, $81, $A0, $1C), ($F9, $08, $2B, $94),
      ($70, $48, $68, $58), ($8F, $45, $FD, $19), ($94, $DE, $6C, $87), ($52, $7B, $F8, $B7),
      ($AB, $73, $D3, $23), ($72, $4B, $02, $E2), ($E3, $1F, $8F, $57), ($66, $55, $AB, $2A),
      ($B2, $EB, $28, $07), ($2F, $B5, $C2, $03), ($86, $C5, $7B, $9A), ($D3, $37, $08, $A5),
      ($30, $28, $87, $F2), ($23, $BF, $A5, $B2), ($02, $03, $6A, $BA), ($ED, $16, $82, $5C),
      ($8A, $CF, $1C, $2B), ($A7, $79, $B4, $92), ($F3, $07, $F2, $F0), ($4E, $69, $E2, $A1),
      ($65, $DA, $F4, $CD), ($06, $05, $BE, $D5), ($D1, $34, $62, $1F), ($C4, $A6, $FE, $8A),
      ($34, $2E, $53, $9D), ($A2, $F3, $55, $A0), ($05, $8A, $E1, $32), ($A4, $F6, $EB, $75),
      ($0B, $83, $EC, $39), ($40, $60, $EF, $AA), ($5E, $71, $9F, $06), ($BD, $6E, $10, $51),
      ($3E, $21, $8A, $F9), ($96, $DD, $06, $3D), ($DD, $3E, $05, $AE), ($4D, $E6, $BD, $46),
      ($91, $54, $8D, $B5), ($71, $C4, $5D, $05), ($04, $06, $D4, $6F), ($60, $50, $15, $FF),
      ($19, $98, $FB, $24), ($D6, $BD, $E9, $97), ($89, $40, $43, $CC), ($67, $D9, $9E, $77),
      ($B0, $E8, $42, $BD), ($07, $89, $8B, $88), ($E7, $19, $5B, $38), ($79, $C8, $EE, $DB),
      ($A1, $7C, $0A, $47), ($7C, $42, $0F, $E9), ($F8, $84, $1E, $C9), ($00, $00, $00, $00),
      ($09, $80, $86, $83), ($32, $2B, $ED, $48), ($1E, $11, $70, $AC), ($6C, $5A, $72, $4E),
      ($FD, $0E, $FF, $FB), ($0F, $85, $38, $56), ($3D, $AE, $D5, $1E), ($36, $2D, $39, $27),
      ($0A, $0F, $D9, $64), ($68, $5C, $A6, $21), ($9B, $5B, $54, $D1), ($24, $36, $2E, $3A),
      ($0C, $0A, $67, $B1), ($93, $57, $E7, $0F), ($B4, $EE, $96, $D2), ($1B, $9B, $91, $9E),
      ($80, $C0, $C5, $4F), ($61, $DC, $20, $A2), ($5A, $77, $4B, $69), ($1C, $12, $1A, $16),
      ($E2, $93, $BA, $0A), ($C0, $A0, $2A, $E5), ($3C, $22, $E0, $43), ($12, $1B, $17, $1D),
      ($0E, $09, $0D, $0B), ($F2, $8B, $C7, $AD), ($2D, $B6, $A8, $B9), ($14, $1E, $A9, $C8),
      ($57, $F1, $19, $85), ($AF, $75, $07, $4C), ($EE, $99, $DD, $BB), ($A3, $7F, $60, $FD),
      ($F7, $01, $26, $9F), ($5C, $72, $F5, $BC), ($44, $66, $3B, $C5), ($5B, $FB, $7E, $34),
      ($8B, $43, $29, $76), ($CB, $23, $C6, $DC), ($B6, $ED, $FC, $68), ($B8, $E4, $F1, $63),
      ($D7, $31, $DC, $CA), ($42, $63, $85, $10), ($13, $97, $22, $40), ($84, $C6, $11, $20),
      ($85, $4A, $24, $7D), ($D2, $BB, $3D, $F8), ($AE, $F9, $32, $11), ($C7, $29, $A1, $6D),
      ($1D, $9E, $2F, $4B), ($DC, $B2, $30, $F3), ($0D, $86, $52, $EC), ($77, $C1, $E3, $D0),
      ($2B, $B3, $16, $6C), ($A9, $70, $B9, $99), ($11, $94, $48, $FA), ($47, $E9, $64, $22),
      ($A8, $FC, $8C, $C4), ($A0, $F0, $3F, $1A), ($56, $7D, $2C, $D8), ($22, $33, $90, $EF),
      ($87, $49, $4E, $C7), ($D9, $38, $D1, $C1), ($8C, $CA, $A2, $FE), ($98, $D4, $0B, $36),
      ($A6, $F5, $81, $CF), ($A5, $7A, $DE, $28), ($DA, $B7, $8E, $26), ($3F, $AD, $BF, $A4),
      ($2C, $3A, $9D, $E4), ($50, $78, $92, $0D), ($6A, $5F, $CC, $9B), ($54, $7E, $46, $62),
      ($F6, $8D, $13, $C2), ($90, $D8, $B8, $E8), ($2E, $39, $F7, $5E), ($82, $C3, $AF, $F5),
      ($9F, $5D, $80, $BE), ($69, $D0, $93, $7C), ($6F, $D5, $2D, $A9), ($CF, $25, $12, $B3),
      ($C8, $AC, $99, $3B), ($10, $18, $7D, $A7), ($E8, $9C, $63, $6E), ($DB, $3B, $BB, $7B),
      ($CD, $26, $78, $09), ($6E, $59, $18, $F4), ($EC, $9A, $B7, $01), ($83, $4F, $9A, $A8),
      ($E6, $95, $6E, $65), ($AA, $FF, $E6, $7E), ($21, $BC, $CF, $08), ($EF, $15, $E8, $E6),
      ($BA, $E7, $9B, $D9), ($4A, $6F, $36, $CE), ($EA, $9F, $09, $D4), ($29, $B0, $7C, $D6),
      ($31, $A4, $B2, $AF), ($2A, $3F, $23, $31), ($C6, $A5, $94, $30), ($35, $A2, $66, $C0),
      ($74, $4E, $BC, $37), ($FC, $82, $CA, $A6), ($E0, $90, $D0, $B0), ($33, $A7, $D8, $15),
      ($F1, $04, $98, $4A), ($41, $EC, $DA, $F7), ($7F, $CD, $50, $0E), ($17, $91, $F6, $2F),
      ($76, $4D, $D6, $8D), ($43, $EF, $B0, $4D), ($CC, $AA, $4D, $54), ($E4, $96, $04, $DF),
      ($9E, $D1, $B5, $E3), ($4C, $6A, $88, $1B), ($C1, $2C, $1F, $B8), ($46, $65, $51, $7F),
      ($9D, $5E, $EA, $04), ($01, $8C, $35, $5D), ($FA, $87, $74, $73), ($FB, $0B, $41, $2E),
      ($B3, $67, $1D, $5A), ($92, $DB, $D2, $52), ($E9, $10, $56, $33), ($6D, $D6, $47, $13),
      ($9A, $D7, $61, $8C), ($37, $A1, $0C, $7A), ($59, $F8, $14, $8E), ($EB, $13, $3C, $89),
      ($CE, $A9, $27, $EE), ($B7, $61, $C9, $35), ($E1, $1C, $E5, $ED), ($7A, $47, $B1, $3C),
      ($9C, $D2, $DF, $59), ($55, $F2, $73, $3F), ($18, $14, $CE, $79), ($73, $C7, $37, $BF),
      ($53, $F7, $CD, $EA), ($5F, $FD, $AA, $5B), ($DF, $3D, $6F, $14), ($78, $44, $DB, $86),
      ($CA, $AF, $F3, $81), ($B9, $68, $C4, $3E), ($38, $24, $34, $2C), ($C2, $A3, $40, $5F),
      ($16, $1D, $C3, $72), ($BC, $E2, $25, $0C), ($28, $3C, $49, $8B), ($FF, $0D, $95, $41),
      ($39, $A8, $01, $71), ($08, $0C, $B3, $DE), ($D8, $B4, $E4, $9C), ($64, $56, $C1, $90),
      ($7B, $CB, $84, $61), ($D5, $32, $B6, $70), ($48, $6C, $5C, $74), ($D0, $B8, $57, $42));
    T6: array [0 .. 255, 0 .. 3] of Byte = (
      ($50, $51, $F4, $A7), ($53, $7E, $41, $65), ($C3, $1A, $17, $A4), ($96, $3A, $27, $5E),
      ($CB, $3B, $AB, $6B), ($F1, $1F, $9D, $45), ($AB, $AC, $FA, $58), ($93, $4B, $E3, $03),
      ($55, $20, $30, $FA), ($F6, $AD, $76, $6D), ($91, $88, $CC, $76), ($25, $F5, $02, $4C),
      ($FC, $4F, $E5, $D7), ($D7, $C5, $2A, $CB), ($80, $26, $35, $44), ($8F, $B5, $62, $A3),
      ($49, $DE, $B1, $5A), ($67, $25, $BA, $1B), ($98, $45, $EA, $0E), ($E1, $5D, $FE, $C0),
      ($02, $C3, $2F, $75), ($12, $81, $4C, $F0), ($A3, $8D, $46, $97), ($C6, $6B, $D3, $F9),
      ($E7, $03, $8F, $5F), ($95, $15, $92, $9C), ($EB, $BF, $6D, $7A), ($DA, $95, $52, $59),
      ($2D, $D4, $BE, $83), ($D3, $58, $74, $21), ($29, $49, $E0, $69), ($44, $8E, $C9, $C8),
      ($6A, $75, $C2, $89), ($78, $F4, $8E, $79), ($6B, $99, $58, $3E), ($DD, $27, $B9, $71),
      ($B6, $BE, $E1, $4F), ($17, $F0, $88, $AD), ($66, $C9, $20, $AC), ($B4, $7D, $CE, $3A),
      ($18, $63, $DF, $4A), ($82, $E5, $1A, $31), ($60, $97, $51, $33), ($45, $62, $53, $7F),
      ($E0, $B1, $64, $77), ($84, $BB, $6B, $AE), ($1C, $FE, $81, $A0), ($94, $F9, $08, $2B),
      ($58, $70, $48, $68), ($19, $8F, $45, $FD), ($87, $94, $DE, $6C), ($B7, $52, $7B, $F8),
      ($23, $AB, $73, $D3), ($E2, $72, $4B, $02), ($57, $E3, $1F, $8F), ($2A, $66, $55, $AB),
      ($07, $B2, $EB, $28), ($03, $2F, $B5, $C2), ($9A, $86, $C5, $7B), ($A5, $D3, $37, $08),
      ($F2, $30, $28, $87), ($B2, $23, $BF, $A5), ($BA, $02, $03, $6A), ($5C, $ED, $16, $82),
      ($2B, $8A, $CF, $1C), ($92, $A7, $79, $B4), ($F0, $F3, $07, $F2), ($A1, $4E, $69, $E2),
      ($CD, $65, $DA, $F4), ($D5, $06, $05, $BE), ($1F, $D1, $34, $62), ($8A, $C4, $A6, $FE),
      ($9D, $34, $2E, $53), ($A0, $A2, $F3, $55), ($32, $05, $8A, $E1), ($75, $A4, $F6, $EB),
      ($39, $0B, $83, $EC), ($AA, $40, $60, $EF), ($06, $5E, $71, $9F), ($51, $BD, $6E, $10),
      ($F9, $3E, $21, $8A), ($3D, $96, $DD, $06), ($AE, $DD, $3E, $05), ($46, $4D, $E6, $BD),
      ($B5, $91, $54, $8D), ($05, $71, $C4, $5D), ($6F, $04, $06, $D4), ($FF, $60, $50, $15),
      ($24, $19, $98, $FB), ($97, $D6, $BD, $E9), ($CC, $89, $40, $43), ($77, $67, $D9, $9E),
      ($BD, $B0, $E8, $42), ($88, $07, $89, $8B), ($38, $E7, $19, $5B), ($DB, $79, $C8, $EE),
      ($47, $A1, $7C, $0A), ($E9, $7C, $42, $0F), ($C9, $F8, $84, $1E), ($00, $00, $00, $00),
      ($83, $09, $80, $86), ($48, $32, $2B, $ED), ($AC, $1E, $11, $70), ($4E, $6C, $5A, $72),
      ($FB, $FD, $0E, $FF), ($56, $0F, $85, $38), ($1E, $3D, $AE, $D5), ($27, $36, $2D, $39),
      ($64, $0A, $0F, $D9), ($21, $68, $5C, $A6), ($D1, $9B, $5B, $54), ($3A, $24, $36, $2E),
      ($B1, $0C, $0A, $67), ($0F, $93, $57, $E7), ($D2, $B4, $EE, $96), ($9E, $1B, $9B, $91),
      ($4F, $80, $C0, $C5), ($A2, $61, $DC, $20), ($69, $5A, $77, $4B), ($16, $1C, $12, $1A),
      ($0A, $E2, $93, $BA), ($E5, $C0, $A0, $2A), ($43, $3C, $22, $E0), ($1D, $12, $1B, $17),
      ($0B, $0E, $09, $0D), ($AD, $F2, $8B, $C7), ($B9, $2D, $B6, $A8), ($C8, $14, $1E, $A9),
      ($85, $57, $F1, $19), ($4C, $AF, $75, $07), ($BB, $EE, $99, $DD), ($FD, $A3, $7F, $60),
      ($9F, $F7, $01, $26), ($BC, $5C, $72, $F5), ($C5, $44, $66, $3B), ($34, $5B, $FB, $7E),
      ($76, $8B, $43, $29), ($DC, $CB, $23, $C6), ($68, $B6, $ED, $FC), ($63, $B8, $E4, $F1),
      ($CA, $D7, $31, $DC), ($10, $42, $63, $85), ($40, $13, $97, $22), ($20, $84, $C6, $11),
      ($7D, $85, $4A, $24), ($F8, $D2, $BB, $3D), ($11, $AE, $F9, $32), ($6D, $C7, $29, $A1),
      ($4B, $1D, $9E, $2F), ($F3, $DC, $B2, $30), ($EC, $0D, $86, $52), ($D0, $77, $C1, $E3),
      ($6C, $2B, $B3, $16), ($99, $A9, $70, $B9), ($FA, $11, $94, $48), ($22, $47, $E9, $64),
      ($C4, $A8, $FC, $8C), ($1A, $A0, $F0, $3F), ($D8, $56, $7D, $2C), ($EF, $22, $33, $90),
      ($C7, $87, $49, $4E), ($C1, $D9, $38, $D1), ($FE, $8C, $CA, $A2), ($36, $98, $D4, $0B),
      ($CF, $A6, $F5, $81), ($28, $A5, $7A, $DE), ($26, $DA, $B7, $8E), ($A4, $3F, $AD, $BF),
      ($E4, $2C, $3A, $9D), ($0D, $50, $78, $92), ($9B, $6A, $5F, $CC), ($62, $54, $7E, $46),
      ($C2, $F6, $8D, $13), ($E8, $90, $D8, $B8), ($5E, $2E, $39, $F7), ($F5, $82, $C3, $AF),
      ($BE, $9F, $5D, $80), ($7C, $69, $D0, $93), ($A9, $6F, $D5, $2D), ($B3, $CF, $25, $12),
      ($3B, $C8, $AC, $99), ($A7, $10, $18, $7D), ($6E, $E8, $9C, $63), ($7B, $DB, $3B, $BB),
      ($09, $CD, $26, $78), ($F4, $6E, $59, $18), ($01, $EC, $9A, $B7), ($A8, $83, $4F, $9A),
      ($65, $E6, $95, $6E), ($7E, $AA, $FF, $E6), ($08, $21, $BC, $CF), ($E6, $EF, $15, $E8),
      ($D9, $BA, $E7, $9B), ($CE, $4A, $6F, $36), ($D4, $EA, $9F, $09), ($D6, $29, $B0, $7C),
      ($AF, $31, $A4, $B2), ($31, $2A, $3F, $23), ($30, $C6, $A5, $94), ($C0, $35, $A2, $66),
      ($37, $74, $4E, $BC), ($A6, $FC, $82, $CA), ($B0, $E0, $90, $D0), ($15, $33, $A7, $D8),
      ($4A, $F1, $04, $98), ($F7, $41, $EC, $DA), ($0E, $7F, $CD, $50), ($2F, $17, $91, $F6),
      ($8D, $76, $4D, $D6), ($4D, $43, $EF, $B0), ($54, $CC, $AA, $4D), ($DF, $E4, $96, $04),
      ($E3, $9E, $D1, $B5), ($1B, $4C, $6A, $88), ($B8, $C1, $2C, $1F), ($7F, $46, $65, $51),
      ($04, $9D, $5E, $EA), ($5D, $01, $8C, $35), ($73, $FA, $87, $74), ($2E, $FB, $0B, $41),
      ($5A, $B3, $67, $1D), ($52, $92, $DB, $D2), ($33, $E9, $10, $56), ($13, $6D, $D6, $47),
      ($8C, $9A, $D7, $61), ($7A, $37, $A1, $0C), ($8E, $59, $F8, $14), ($89, $EB, $13, $3C),
      ($EE, $CE, $A9, $27), ($35, $B7, $61, $C9), ($ED, $E1, $1C, $E5), ($3C, $7A, $47, $B1),
      ($59, $9C, $D2, $DF), ($3F, $55, $F2, $73), ($79, $18, $14, $CE), ($BF, $73, $C7, $37),
      ($EA, $53, $F7, $CD), ($5B, $5F, $FD, $AA), ($14, $DF, $3D, $6F), ($86, $78, $44, $DB),
      ($81, $CA, $AF, $F3), ($3E, $B9, $68, $C4), ($2C, $38, $24, $34), ($5F, $C2, $A3, $40),
      ($72, $16, $1D, $C3), ($0C, $BC, $E2, $25), ($8B, $28, $3C, $49), ($41, $FF, $0D, $95),
      ($71, $39, $A8, $01), ($DE, $08, $0C, $B3), ($9C, $D8, $B4, $E4), ($90, $64, $56, $C1),
      ($61, $7B, $CB, $84), ($70, $D5, $32, $B6), ($74, $48, $6C, $5C), ($42, $D0, $B8, $57));
    T7: array [0 .. 255, 0 .. 3] of Byte = (
      ($A7, $50, $51, $F4), ($65, $53, $7E, $41), ($A4, $C3, $1A, $17), ($5E, $96, $3A, $27),
      ($6B, $CB, $3B, $AB), ($45, $F1, $1F, $9D), ($58, $AB, $AC, $FA), ($03, $93, $4B, $E3),
      ($FA, $55, $20, $30), ($6D, $F6, $AD, $76), ($76, $91, $88, $CC), ($4C, $25, $F5, $02),
      ($D7, $FC, $4F, $E5), ($CB, $D7, $C5, $2A), ($44, $80, $26, $35), ($A3, $8F, $B5, $62),
      ($5A, $49, $DE, $B1), ($1B, $67, $25, $BA), ($0E, $98, $45, $EA), ($C0, $E1, $5D, $FE),
      ($75, $02, $C3, $2F), ($F0, $12, $81, $4C), ($97, $A3, $8D, $46), ($F9, $C6, $6B, $D3),
      ($5F, $E7, $03, $8F), ($9C, $95, $15, $92), ($7A, $EB, $BF, $6D), ($59, $DA, $95, $52),
      ($83, $2D, $D4, $BE), ($21, $D3, $58, $74), ($69, $29, $49, $E0), ($C8, $44, $8E, $C9),
      ($89, $6A, $75, $C2), ($79, $78, $F4, $8E), ($3E, $6B, $99, $58), ($71, $DD, $27, $B9),
      ($4F, $B6, $BE, $E1), ($AD, $17, $F0, $88), ($AC, $66, $C9, $20), ($3A, $B4, $7D, $CE),
      ($4A, $18, $63, $DF), ($31, $82, $E5, $1A), ($33, $60, $97, $51), ($7F, $45, $62, $53),
      ($77, $E0, $B1, $64), ($AE, $84, $BB, $6B), ($A0, $1C, $FE, $81), ($2B, $94, $F9, $08),
      ($68, $58, $70, $48), ($FD, $19, $8F, $45), ($6C, $87, $94, $DE), ($F8, $B7, $52, $7B),
      ($D3, $23, $AB, $73), ($02, $E2, $72, $4B), ($8F, $57, $E3, $1F), ($AB, $2A, $66, $55),
      ($28, $07, $B2, $EB), ($C2, $03, $2F, $B5), ($7B, $9A, $86, $C5), ($08, $A5, $D3, $37),
      ($87, $F2, $30, $28), ($A5, $B2, $23, $BF), ($6A, $BA, $02, $03), ($82, $5C, $ED, $16),
      ($1C, $2B, $8A, $CF), ($B4, $92, $A7, $79), ($F2, $F0, $F3, $07), ($E2, $A1, $4E, $69),
      ($F4, $CD, $65, $DA), ($BE, $D5, $06, $05), ($62, $1F, $D1, $34), ($FE, $8A, $C4, $A6),
      ($53, $9D, $34, $2E), ($55, $A0, $A2, $F3), ($E1, $32, $05, $8A), ($EB, $75, $A4, $F6),
      ($EC, $39, $0B, $83), ($EF, $AA, $40, $60), ($9F, $06, $5E, $71), ($10, $51, $BD, $6E),
      ($8A, $F9, $3E, $21), ($06, $3D, $96, $DD), ($05, $AE, $DD, $3E), ($BD, $46, $4D, $E6),
      ($8D, $B5, $91, $54), ($5D, $05, $71, $C4), ($D4, $6F, $04, $06), ($15, $FF, $60, $50),
      ($FB, $24, $19, $98), ($E9, $97, $D6, $BD), ($43, $CC, $89, $40), ($9E, $77, $67, $D9),
      ($42, $BD, $B0, $E8), ($8B, $88, $07, $89), ($5B, $38, $E7, $19), ($EE, $DB, $79, $C8),
      ($0A, $47, $A1, $7C), ($0F, $E9, $7C, $42), ($1E, $C9, $F8, $84), ($00, $00, $00, $00),
      ($86, $83, $09, $80), ($ED, $48, $32, $2B), ($70, $AC, $1E, $11), ($72, $4E, $6C, $5A),
      ($FF, $FB, $FD, $0E), ($38, $56, $0F, $85), ($D5, $1E, $3D, $AE), ($39, $27, $36, $2D),
      ($D9, $64, $0A, $0F), ($A6, $21, $68, $5C), ($54, $D1, $9B, $5B), ($2E, $3A, $24, $36),
      ($67, $B1, $0C, $0A), ($E7, $0F, $93, $57), ($96, $D2, $B4, $EE), ($91, $9E, $1B, $9B),
      ($C5, $4F, $80, $C0), ($20, $A2, $61, $DC), ($4B, $69, $5A, $77), ($1A, $16, $1C, $12),
      ($BA, $0A, $E2, $93), ($2A, $E5, $C0, $A0), ($E0, $43, $3C, $22), ($17, $1D, $12, $1B),
      ($0D, $0B, $0E, $09), ($C7, $AD, $F2, $8B), ($A8, $B9, $2D, $B6), ($A9, $C8, $14, $1E),
      ($19, $85, $57, $F1), ($07, $4C, $AF, $75), ($DD, $BB, $EE, $99), ($60, $FD, $A3, $7F),
      ($26, $9F, $F7, $01), ($F5, $BC, $5C, $72), ($3B, $C5, $44, $66), ($7E, $34, $5B, $FB),
      ($29, $76, $8B, $43), ($C6, $DC, $CB, $23), ($FC, $68, $B6, $ED), ($F1, $63, $B8, $E4),
      ($DC, $CA, $D7, $31), ($85, $10, $42, $63), ($22, $40, $13, $97), ($11, $20, $84, $C6),
      ($24, $7D, $85, $4A), ($3D, $F8, $D2, $BB), ($32, $11, $AE, $F9), ($A1, $6D, $C7, $29),
      ($2F, $4B, $1D, $9E), ($30, $F3, $DC, $B2), ($52, $EC, $0D, $86), ($E3, $D0, $77, $C1),
      ($16, $6C, $2B, $B3), ($B9, $99, $A9, $70), ($48, $FA, $11, $94), ($64, $22, $47, $E9),
      ($8C, $C4, $A8, $FC), ($3F, $1A, $A0, $F0), ($2C, $D8, $56, $7D), ($90, $EF, $22, $33),
      ($4E, $C7, $87, $49), ($D1, $C1, $D9, $38), ($A2, $FE, $8C, $CA), ($0B, $36, $98, $D4),
      ($81, $CF, $A6, $F5), ($DE, $28, $A5, $7A), ($8E, $26, $DA, $B7), ($BF, $A4, $3F, $AD),
      ($9D, $E4, $2C, $3A), ($92, $0D, $50, $78), ($CC, $9B, $6A, $5F), ($46, $62, $54, $7E),
      ($13, $C2, $F6, $8D), ($B8, $E8, $90, $D8), ($F7, $5E, $2E, $39), ($AF, $F5, $82, $C3),
      ($80, $BE, $9F, $5D), ($93, $7C, $69, $D0), ($2D, $A9, $6F, $D5), ($12, $B3, $CF, $25),
      ($99, $3B, $C8, $AC), ($7D, $A7, $10, $18), ($63, $6E, $E8, $9C), ($BB, $7B, $DB, $3B),
      ($78, $09, $CD, $26), ($18, $F4, $6E, $59), ($B7, $01, $EC, $9A), ($9A, $A8, $83, $4F),
      ($6E, $65, $E6, $95), ($E6, $7E, $AA, $FF), ($CF, $08, $21, $BC), ($E8, $E6, $EF, $15),
      ($9B, $D9, $BA, $E7), ($36, $CE, $4A, $6F), ($09, $D4, $EA, $9F), ($7C, $D6, $29, $B0),
      ($B2, $AF, $31, $A4), ($23, $31, $2A, $3F), ($94, $30, $C6, $A5), ($66, $C0, $35, $A2),
      ($BC, $37, $74, $4E), ($CA, $A6, $FC, $82), ($D0, $B0, $E0, $90), ($D8, $15, $33, $A7),
      ($98, $4A, $F1, $04), ($DA, $F7, $41, $EC), ($50, $0E, $7F, $CD), ($F6, $2F, $17, $91),
      ($D6, $8D, $76, $4D), ($B0, $4D, $43, $EF), ($4D, $54, $CC, $AA), ($04, $DF, $E4, $96),
      ($B5, $E3, $9E, $D1), ($88, $1B, $4C, $6A), ($1F, $B8, $C1, $2C), ($51, $7F, $46, $65),
      ($EA, $04, $9D, $5E), ($35, $5D, $01, $8C), ($74, $73, $FA, $87), ($41, $2E, $FB, $0B),
      ($1D, $5A, $B3, $67), ($D2, $52, $92, $DB), ($56, $33, $E9, $10), ($47, $13, $6D, $D6),
      ($61, $8C, $9A, $D7), ($0C, $7A, $37, $A1), ($14, $8E, $59, $F8), ($3C, $89, $EB, $13),
      ($27, $EE, $CE, $A9), ($C9, $35, $B7, $61), ($E5, $ED, $E1, $1C), ($B1, $3C, $7A, $47),
      ($DF, $59, $9C, $D2), ($73, $3F, $55, $F2), ($CE, $79, $18, $14), ($37, $BF, $73, $C7),
      ($CD, $EA, $53, $F7), ($AA, $5B, $5F, $FD), ($6F, $14, $DF, $3D), ($DB, $86, $78, $44),
      ($F3, $81, $CA, $AF), ($C4, $3E, $B9, $68), ($34, $2C, $38, $24), ($40, $5F, $C2, $A3),
      ($C3, $72, $16, $1D), ($25, $0C, $BC, $E2), ($49, $8B, $28, $3C), ($95, $41, $FF, $0D),
      ($01, $71, $39, $A8), ($B3, $DE, $08, $0C), ($E4, $9C, $D8, $B4), ($C1, $90, $64, $56),
      ($84, $61, $7B, $CB), ($B6, $70, $D5, $32), ($5C, $74, $48, $6C), ($57, $42, $D0, $B8));
    T8: array [0 .. 255, 0 .. 3] of Byte = (
      ($F4, $A7, $50, $51), ($41, $65, $53, $7E), ($17, $A4, $C3, $1A), ($27, $5E, $96, $3A),
      ($AB, $6B, $CB, $3B), ($9D, $45, $F1, $1F), ($FA, $58, $AB, $AC), ($E3, $03, $93, $4B),
      ($30, $FA, $55, $20), ($76, $6D, $F6, $AD), ($CC, $76, $91, $88), ($02, $4C, $25, $F5),
      ($E5, $D7, $FC, $4F), ($2A, $CB, $D7, $C5), ($35, $44, $80, $26), ($62, $A3, $8F, $B5),
      ($B1, $5A, $49, $DE), ($BA, $1B, $67, $25), ($EA, $0E, $98, $45), ($FE, $C0, $E1, $5D),
      ($2F, $75, $02, $C3), ($4C, $F0, $12, $81), ($46, $97, $A3, $8D), ($D3, $F9, $C6, $6B),
      ($8F, $5F, $E7, $03), ($92, $9C, $95, $15), ($6D, $7A, $EB, $BF), ($52, $59, $DA, $95),
      ($BE, $83, $2D, $D4), ($74, $21, $D3, $58), ($E0, $69, $29, $49), ($C9, $C8, $44, $8E),
      ($C2, $89, $6A, $75), ($8E, $79, $78, $F4), ($58, $3E, $6B, $99), ($B9, $71, $DD, $27),
      ($E1, $4F, $B6, $BE), ($88, $AD, $17, $F0), ($20, $AC, $66, $C9), ($CE, $3A, $B4, $7D),
      ($DF, $4A, $18, $63), ($1A, $31, $82, $E5), ($51, $33, $60, $97), ($53, $7F, $45, $62),
      ($64, $77, $E0, $B1), ($6B, $AE, $84, $BB), ($81, $A0, $1C, $FE), ($08, $2B, $94, $F9),
      ($48, $68, $58, $70), ($45, $FD, $19, $8F), ($DE, $6C, $87, $94), ($7B, $F8, $B7, $52),
      ($73, $D3, $23, $AB), ($4B, $02, $E2, $72), ($1F, $8F, $57, $E3), ($55, $AB, $2A, $66),
      ($EB, $28, $07, $B2), ($B5, $C2, $03, $2F), ($C5, $7B, $9A, $86), ($37, $08, $A5, $D3),
      ($28, $87, $F2, $30), ($BF, $A5, $B2, $23), ($03, $6A, $BA, $02), ($16, $82, $5C, $ED),
      ($CF, $1C, $2B, $8A), ($79, $B4, $92, $A7), ($07, $F2, $F0, $F3), ($69, $E2, $A1, $4E),
      ($DA, $F4, $CD, $65), ($05, $BE, $D5, $06), ($34, $62, $1F, $D1), ($A6, $FE, $8A, $C4),
      ($2E, $53, $9D, $34), ($F3, $55, $A0, $A2), ($8A, $E1, $32, $05), ($F6, $EB, $75, $A4),
      ($83, $EC, $39, $0B), ($60, $EF, $AA, $40), ($71, $9F, $06, $5E), ($6E, $10, $51, $BD),
      ($21, $8A, $F9, $3E), ($DD, $06, $3D, $96), ($3E, $05, $AE, $DD), ($E6, $BD, $46, $4D),
      ($54, $8D, $B5, $91), ($C4, $5D, $05, $71), ($06, $D4, $6F, $04), ($50, $15, $FF, $60),
      ($98, $FB, $24, $19), ($BD, $E9, $97, $D6), ($40, $43, $CC, $89), ($D9, $9E, $77, $67),
      ($E8, $42, $BD, $B0), ($89, $8B, $88, $07), ($19, $5B, $38, $E7), ($C8, $EE, $DB, $79),
      ($7C, $0A, $47, $A1), ($42, $0F, $E9, $7C), ($84, $1E, $C9, $F8), ($00, $00, $00, $00),
      ($80, $86, $83, $09), ($2B, $ED, $48, $32), ($11, $70, $AC, $1E), ($5A, $72, $4E, $6C),
      ($0E, $FF, $FB, $FD), ($85, $38, $56, $0F), ($AE, $D5, $1E, $3D), ($2D, $39, $27, $36),
      ($0F, $D9, $64, $0A), ($5C, $A6, $21, $68), ($5B, $54, $D1, $9B), ($36, $2E, $3A, $24),
      ($0A, $67, $B1, $0C), ($57, $E7, $0F, $93), ($EE, $96, $D2, $B4), ($9B, $91, $9E, $1B),
      ($C0, $C5, $4F, $80), ($DC, $20, $A2, $61), ($77, $4B, $69, $5A), ($12, $1A, $16, $1C),
      ($93, $BA, $0A, $E2), ($A0, $2A, $E5, $C0), ($22, $E0, $43, $3C), ($1B, $17, $1D, $12),
      ($09, $0D, $0B, $0E), ($8B, $C7, $AD, $F2), ($B6, $A8, $B9, $2D), ($1E, $A9, $C8, $14),
      ($F1, $19, $85, $57), ($75, $07, $4C, $AF), ($99, $DD, $BB, $EE), ($7F, $60, $FD, $A3),
      ($01, $26, $9F, $F7), ($72, $F5, $BC, $5C), ($66, $3B, $C5, $44), ($FB, $7E, $34, $5B),
      ($43, $29, $76, $8B), ($23, $C6, $DC, $CB), ($ED, $FC, $68, $B6), ($E4, $F1, $63, $B8),
      ($31, $DC, $CA, $D7), ($63, $85, $10, $42), ($97, $22, $40, $13), ($C6, $11, $20, $84),
      ($4A, $24, $7D, $85), ($BB, $3D, $F8, $D2), ($F9, $32, $11, $AE), ($29, $A1, $6D, $C7),
      ($9E, $2F, $4B, $1D), ($B2, $30, $F3, $DC), ($86, $52, $EC, $0D), ($C1, $E3, $D0, $77),
      ($B3, $16, $6C, $2B), ($70, $B9, $99, $A9), ($94, $48, $FA, $11), ($E9, $64, $22, $47),
      ($FC, $8C, $C4, $A8), ($F0, $3F, $1A, $A0), ($7D, $2C, $D8, $56), ($33, $90, $EF, $22),
      ($49, $4E, $C7, $87), ($38, $D1, $C1, $D9), ($CA, $A2, $FE, $8C), ($D4, $0B, $36, $98),
      ($F5, $81, $CF, $A6), ($7A, $DE, $28, $A5), ($B7, $8E, $26, $DA), ($AD, $BF, $A4, $3F),
      ($3A, $9D, $E4, $2C), ($78, $92, $0D, $50), ($5F, $CC, $9B, $6A), ($7E, $46, $62, $54),
      ($8D, $13, $C2, $F6), ($D8, $B8, $E8, $90), ($39, $F7, $5E, $2E), ($C3, $AF, $F5, $82),
      ($5D, $80, $BE, $9F), ($D0, $93, $7C, $69), ($D5, $2D, $A9, $6F), ($25, $12, $B3, $CF),
      ($AC, $99, $3B, $C8), ($18, $7D, $A7, $10), ($9C, $63, $6E, $E8), ($3B, $BB, $7B, $DB),
      ($26, $78, $09, $CD), ($59, $18, $F4, $6E), ($9A, $B7, $01, $EC), ($4F, $9A, $A8, $83),
      ($95, $6E, $65, $E6), ($FF, $E6, $7E, $AA), ($BC, $CF, $08, $21), ($15, $E8, $E6, $EF),
      ($E7, $9B, $D9, $BA), ($6F, $36, $CE, $4A), ($9F, $09, $D4, $EA), ($B0, $7C, $D6, $29),
      ($A4, $B2, $AF, $31), ($3F, $23, $31, $2A), ($A5, $94, $30, $C6), ($A2, $66, $C0, $35),
      ($4E, $BC, $37, $74), ($82, $CA, $A6, $FC), ($90, $D0, $B0, $E0), ($A7, $D8, $15, $33),
      ($04, $98, $4A, $F1), ($EC, $DA, $F7, $41), ($CD, $50, $0E, $7F), ($91, $F6, $2F, $17),
      ($4D, $D6, $8D, $76), ($EF, $B0, $4D, $43), ($AA, $4D, $54, $CC), ($96, $04, $DF, $E4),
      ($D1, $B5, $E3, $9E), ($6A, $88, $1B, $4C), ($2C, $1F, $B8, $C1), ($65, $51, $7F, $46),
      ($5E, $EA, $04, $9D), ($8C, $35, $5D, $01), ($87, $74, $73, $FA), ($0B, $41, $2E, $FB),
      ($67, $1D, $5A, $B3), ($DB, $D2, $52, $92), ($10, $56, $33, $E9), ($D6, $47, $13, $6D),
      ($D7, $61, $8C, $9A), ($A1, $0C, $7A, $37), ($F8, $14, $8E, $59), ($13, $3C, $89, $EB),
      ($A9, $27, $EE, $CE), ($61, $C9, $35, $B7), ($1C, $E5, $ED, $E1), ($47, $B1, $3C, $7A),
      ($D2, $DF, $59, $9C), ($F2, $73, $3F, $55), ($14, $CE, $79, $18), ($C7, $37, $BF, $73),
      ($F7, $CD, $EA, $53), ($FD, $AA, $5B, $5F), ($3D, $6F, $14, $DF), ($44, $DB, $86, $78),
      ($AF, $F3, $81, $CA), ($68, $C4, $3E, $B9), ($24, $34, $2C, $38), ($A3, $40, $5F, $C2),
      ($1D, $C3, $72, $16), ($E2, $25, $0C, $BC), ($3C, $49, $8B, $28), ($0D, $95, $41, $FF),
      ($A8, $01, $71, $39), ($0C, $B3, $DE, $08), ($B4, $E4, $9C, $D8), ($56, $C1, $90, $64),
      ($CB, $84, $61, $7B), ($32, $B6, $70, $D5), ($6C, $5C, $74, $48), ($B8, $57, $42, $D0));
    S5: array [0 .. 255] of Byte = (
      $52, $09, $6A, $D5, $30, $36, $A5, $38, $BF, $40, $A3, $9E, $81, $F3, $D7, $FB, $7C, $E3, $39, $82,
      $9B, $2F, $FF, $87, $34, $8E, $43, $44, $C4, $DE, $E9, $CB, $54, $7B, $94, $32, $A6, $C2, $23, $3D,
      $EE, $4C, $95, $0B, $42, $FA, $C3, $4E, $08, $2E, $A1, $66, $28, $D9, $24, $B2, $76, $5B, $A2, $49,
      $6D, $8B, $D1, $25, $72, $F8, $F6, $64, $86, $68, $98, $16, $D4, $A4, $5C, $CC, $5D, $65, $B6, $92,
      $6C, $70, $48, $50, $FD, $ED, $B9, $DA, $5E, $15, $46, $57, $A7, $8D, $9D, $84, $90, $D8, $AB, $00,
      $8C, $BC, $D3, $0A, $F7, $E4, $58, $05, $B8, $B3, $45, $06, $D0, $2C, $1E, $8F, $CA, $3F, $0F, $02,
      $C1, $AF, $BD, $03, $01, $13, $8A, $6B, $3A, $91, $11, $41, $4F, $67, $DC, $EA, $97, $F2, $CF, $CE,
      $F0, $B4, $E6, $73, $96, $AC, $74, $22, $E7, $AD, $35, $85, $E2, $F9, $37, $E8, $1C, $75, $DF, $6E,
      $47, $F1, $1A, $71, $1D, $29, $C5, $89, $6F, $B7, $62, $0E, $AA, $18, $BE, $1B, $FC, $56, $3E, $4B,
      $C6, $D2, $79, $20, $9A, $DB, $C0, $FE, $78, $CD, $5A, $F4, $1F, $DD, $A8, $33, $88, $07, $C7, $31,
      $B1, $12, $10, $59, $27, $80, $EC, $5F, $60, $51, $7F, $A9, $19, $B5, $4A, $0D, $2D, $E5, $7A, $9F,
      $93, $C9, $9C, $EF, $A0, $E0, $3B, $4D, $AE, $2A, $F5, $B0, $C8, $EB, $BB, $3C, $83, $53, $99, $61,
      $17, $2B, $04, $7E, $BA, $77, $D6, $26, $E1, $69, $14, $63, $55, $21, $0C, $7D);
    U1: array [0 .. 255, 0 .. 3] of Byte = (
      ($00, $00, $00, $00), ($0E, $09, $0D, $0B), ($1C, $12, $1A, $16), ($12, $1B, $17, $1D),
      ($38, $24, $34, $2C), ($36, $2D, $39, $27), ($24, $36, $2E, $3A), ($2A, $3F, $23, $31),
      ($70, $48, $68, $58), ($7E, $41, $65, $53), ($6C, $5A, $72, $4E), ($62, $53, $7F, $45),
      ($48, $6C, $5C, $74), ($46, $65, $51, $7F), ($54, $7E, $46, $62), ($5A, $77, $4B, $69),
      ($E0, $90, $D0, $B0), ($EE, $99, $DD, $BB), ($FC, $82, $CA, $A6), ($F2, $8B, $C7, $AD),
      ($D8, $B4, $E4, $9C), ($D6, $BD, $E9, $97), ($C4, $A6, $FE, $8A), ($CA, $AF, $F3, $81),
      ($90, $D8, $B8, $E8), ($9E, $D1, $B5, $E3), ($8C, $CA, $A2, $FE), ($82, $C3, $AF, $F5),
      ($A8, $FC, $8C, $C4), ($A6, $F5, $81, $CF), ($B4, $EE, $96, $D2), ($BA, $E7, $9B, $D9),
      ($DB, $3B, $BB, $7B), ($D5, $32, $B6, $70), ($C7, $29, $A1, $6D), ($C9, $20, $AC, $66),
      ($E3, $1F, $8F, $57), ($ED, $16, $82, $5C), ($FF, $0D, $95, $41), ($F1, $04, $98, $4A),
      ($AB, $73, $D3, $23), ($A5, $7A, $DE, $28), ($B7, $61, $C9, $35), ($B9, $68, $C4, $3E),
      ($93, $57, $E7, $0F), ($9D, $5E, $EA, $04), ($8F, $45, $FD, $19), ($81, $4C, $F0, $12),
      ($3B, $AB, $6B, $CB), ($35, $A2, $66, $C0), ($27, $B9, $71, $DD), ($29, $B0, $7C, $D6),
      ($03, $8F, $5F, $E7), ($0D, $86, $52, $EC), ($1F, $9D, $45, $F1), ($11, $94, $48, $FA),
      ($4B, $E3, $03, $93), ($45, $EA, $0E, $98), ($57, $F1, $19, $85), ($59, $F8, $14, $8E),
      ($73, $C7, $37, $BF), ($7D, $CE, $3A, $B4), ($6F, $D5, $2D, $A9), ($61, $DC, $20, $A2),
      ($AD, $76, $6D, $F6), ($A3, $7F, $60, $FD), ($B1, $64, $77, $E0), ($BF, $6D, $7A, $EB),
      ($95, $52, $59, $DA), ($9B, $5B, $54, $D1), ($89, $40, $43, $CC), ($87, $49, $4E, $C7),
      ($DD, $3E, $05, $AE), ($D3, $37, $08, $A5), ($C1, $2C, $1F, $B8), ($CF, $25, $12, $B3),
      ($E5, $1A, $31, $82), ($EB, $13, $3C, $89), ($F9, $08, $2B, $94), ($F7, $01, $26, $9F),
      ($4D, $E6, $BD, $46), ($43, $EF, $B0, $4D), ($51, $F4, $A7, $50), ($5F, $FD, $AA, $5B),
      ($75, $C2, $89, $6A), ($7B, $CB, $84, $61), ($69, $D0, $93, $7C), ($67, $D9, $9E, $77),
      ($3D, $AE, $D5, $1E), ($33, $A7, $D8, $15), ($21, $BC, $CF, $08), ($2F, $B5, $C2, $03),
      ($05, $8A, $E1, $32), ($0B, $83, $EC, $39), ($19, $98, $FB, $24), ($17, $91, $F6, $2F),
      ($76, $4D, $D6, $8D), ($78, $44, $DB, $86), ($6A, $5F, $CC, $9B), ($64, $56, $C1, $90),
      ($4E, $69, $E2, $A1), ($40, $60, $EF, $AA), ($52, $7B, $F8, $B7), ($5C, $72, $F5, $BC),
      ($06, $05, $BE, $D5), ($08, $0C, $B3, $DE), ($1A, $17, $A4, $C3), ($14, $1E, $A9, $C8),
      ($3E, $21, $8A, $F9), ($30, $28, $87, $F2), ($22, $33, $90, $EF), ($2C, $3A, $9D, $E4),
      ($96, $DD, $06, $3D), ($98, $D4, $0B, $36), ($8A, $CF, $1C, $2B), ($84, $C6, $11, $20),
      ($AE, $F9, $32, $11), ($A0, $F0, $3F, $1A), ($B2, $EB, $28, $07), ($BC, $E2, $25, $0C),
      ($E6, $95, $6E, $65), ($E8, $9C, $63, $6E), ($FA, $87, $74, $73), ($F4, $8E, $79, $78),
      ($DE, $B1, $5A, $49), ($D0, $B8, $57, $42), ($C2, $A3, $40, $5F), ($CC, $AA, $4D, $54),
      ($41, $EC, $DA, $F7), ($4F, $E5, $D7, $FC), ($5D, $FE, $C0, $E1), ($53, $F7, $CD, $EA),
      ($79, $C8, $EE, $DB), ($77, $C1, $E3, $D0), ($65, $DA, $F4, $CD), ($6B, $D3, $F9, $C6),
      ($31, $A4, $B2, $AF), ($3F, $AD, $BF, $A4), ($2D, $B6, $A8, $B9), ($23, $BF, $A5, $B2),
      ($09, $80, $86, $83), ($07, $89, $8B, $88), ($15, $92, $9C, $95), ($1B, $9B, $91, $9E),
      ($A1, $7C, $0A, $47), ($AF, $75, $07, $4C), ($BD, $6E, $10, $51), ($B3, $67, $1D, $5A),
      ($99, $58, $3E, $6B), ($97, $51, $33, $60), ($85, $4A, $24, $7D), ($8B, $43, $29, $76),
      ($D1, $34, $62, $1F), ($DF, $3D, $6F, $14), ($CD, $26, $78, $09), ($C3, $2F, $75, $02),
      ($E9, $10, $56, $33), ($E7, $19, $5B, $38), ($F5, $02, $4C, $25), ($FB, $0B, $41, $2E),
      ($9A, $D7, $61, $8C), ($94, $DE, $6C, $87), ($86, $C5, $7B, $9A), ($88, $CC, $76, $91),
      ($A2, $F3, $55, $A0), ($AC, $FA, $58, $AB), ($BE, $E1, $4F, $B6), ($B0, $E8, $42, $BD),
      ($EA, $9F, $09, $D4), ($E4, $96, $04, $DF), ($F6, $8D, $13, $C2), ($F8, $84, $1E, $C9),
      ($D2, $BB, $3D, $F8), ($DC, $B2, $30, $F3), ($CE, $A9, $27, $EE), ($C0, $A0, $2A, $E5),
      ($7A, $47, $B1, $3C), ($74, $4E, $BC, $37), ($66, $55, $AB, $2A), ($68, $5C, $A6, $21),
      ($42, $63, $85, $10), ($4C, $6A, $88, $1B), ($5E, $71, $9F, $06), ($50, $78, $92, $0D),
      ($0A, $0F, $D9, $64), ($04, $06, $D4, $6F), ($16, $1D, $C3, $72), ($18, $14, $CE, $79),
      ($32, $2B, $ED, $48), ($3C, $22, $E0, $43), ($2E, $39, $F7, $5E), ($20, $30, $FA, $55),
      ($EC, $9A, $B7, $01), ($E2, $93, $BA, $0A), ($F0, $88, $AD, $17), ($FE, $81, $A0, $1C),
      ($D4, $BE, $83, $2D), ($DA, $B7, $8E, $26), ($C8, $AC, $99, $3B), ($C6, $A5, $94, $30),
      ($9C, $D2, $DF, $59), ($92, $DB, $D2, $52), ($80, $C0, $C5, $4F), ($8E, $C9, $C8, $44),
      ($A4, $F6, $EB, $75), ($AA, $FF, $E6, $7E), ($B8, $E4, $F1, $63), ($B6, $ED, $FC, $68),
      ($0C, $0A, $67, $B1), ($02, $03, $6A, $BA), ($10, $18, $7D, $A7), ($1E, $11, $70, $AC),
      ($34, $2E, $53, $9D), ($3A, $27, $5E, $96), ($28, $3C, $49, $8B), ($26, $35, $44, $80),
      ($7C, $42, $0F, $E9), ($72, $4B, $02, $E2), ($60, $50, $15, $FF), ($6E, $59, $18, $F4),
      ($44, $66, $3B, $C5), ($4A, $6F, $36, $CE), ($58, $74, $21, $D3), ($56, $7D, $2C, $D8),
      ($37, $A1, $0C, $7A), ($39, $A8, $01, $71), ($2B, $B3, $16, $6C), ($25, $BA, $1B, $67),
      ($0F, $85, $38, $56), ($01, $8C, $35, $5D), ($13, $97, $22, $40), ($1D, $9E, $2F, $4B),
      ($47, $E9, $64, $22), ($49, $E0, $69, $29), ($5B, $FB, $7E, $34), ($55, $F2, $73, $3F),
      ($7F, $CD, $50, $0E), ($71, $C4, $5D, $05), ($63, $DF, $4A, $18), ($6D, $D6, $47, $13),
      ($D7, $31, $DC, $CA), ($D9, $38, $D1, $C1), ($CB, $23, $C6, $DC), ($C5, $2A, $CB, $D7),
      ($EF, $15, $E8, $E6), ($E1, $1C, $E5, $ED), ($F3, $07, $F2, $F0), ($FD, $0E, $FF, $FB),
      ($A7, $79, $B4, $92), ($A9, $70, $B9, $99), ($BB, $6B, $AE, $84), ($B5, $62, $A3, $8F),
      ($9F, $5D, $80, $BE), ($91, $54, $8D, $B5), ($83, $4F, $9A, $A8), ($8D, $46, $97, $A3));
    U2: array [0 .. 255, 0 .. 3] of Byte = (
      ($00, $00, $00, $00), ($0B, $0E, $09, $0D), ($16, $1C, $12, $1A), ($1D, $12, $1B, $17),
      ($2C, $38, $24, $34), ($27, $36, $2D, $39), ($3A, $24, $36, $2E), ($31, $2A, $3F, $23),
      ($58, $70, $48, $68), ($53, $7E, $41, $65), ($4E, $6C, $5A, $72), ($45, $62, $53, $7F),
      ($74, $48, $6C, $5C), ($7F, $46, $65, $51), ($62, $54, $7E, $46), ($69, $5A, $77, $4B),
      ($B0, $E0, $90, $D0), ($BB, $EE, $99, $DD), ($A6, $FC, $82, $CA), ($AD, $F2, $8B, $C7),
      ($9C, $D8, $B4, $E4), ($97, $D6, $BD, $E9), ($8A, $C4, $A6, $FE), ($81, $CA, $AF, $F3),
      ($E8, $90, $D8, $B8), ($E3, $9E, $D1, $B5), ($FE, $8C, $CA, $A2), ($F5, $82, $C3, $AF),
      ($C4, $A8, $FC, $8C), ($CF, $A6, $F5, $81), ($D2, $B4, $EE, $96), ($D9, $BA, $E7, $9B),
      ($7B, $DB, $3B, $BB), ($70, $D5, $32, $B6), ($6D, $C7, $29, $A1), ($66, $C9, $20, $AC),
      ($57, $E3, $1F, $8F), ($5C, $ED, $16, $82), ($41, $FF, $0D, $95), ($4A, $F1, $04, $98),
      ($23, $AB, $73, $D3), ($28, $A5, $7A, $DE), ($35, $B7, $61, $C9), ($3E, $B9, $68, $C4),
      ($0F, $93, $57, $E7), ($04, $9D, $5E, $EA), ($19, $8F, $45, $FD), ($12, $81, $4C, $F0),
      ($CB, $3B, $AB, $6B), ($C0, $35, $A2, $66), ($DD, $27, $B9, $71), ($D6, $29, $B0, $7C),
      ($E7, $03, $8F, $5F), ($EC, $0D, $86, $52), ($F1, $1F, $9D, $45), ($FA, $11, $94, $48),
      ($93, $4B, $E3, $03), ($98, $45, $EA, $0E), ($85, $57, $F1, $19), ($8E, $59, $F8, $14),
      ($BF, $73, $C7, $37), ($B4, $7D, $CE, $3A), ($A9, $6F, $D5, $2D), ($A2, $61, $DC, $20),
      ($F6, $AD, $76, $6D), ($FD, $A3, $7F, $60), ($E0, $B1, $64, $77), ($EB, $BF, $6D, $7A),
      ($DA, $95, $52, $59), ($D1, $9B, $5B, $54), ($CC, $89, $40, $43), ($C7, $87, $49, $4E),
      ($AE, $DD, $3E, $05), ($A5, $D3, $37, $08), ($B8, $C1, $2C, $1F), ($B3, $CF, $25, $12),
      ($82, $E5, $1A, $31), ($89, $EB, $13, $3C), ($94, $F9, $08, $2B), ($9F, $F7, $01, $26),
      ($46, $4D, $E6, $BD), ($4D, $43, $EF, $B0), ($50, $51, $F4, $A7), ($5B, $5F, $FD, $AA),
      ($6A, $75, $C2, $89), ($61, $7B, $CB, $84), ($7C, $69, $D0, $93), ($77, $67, $D9, $9E),
      ($1E, $3D, $AE, $D5), ($15, $33, $A7, $D8), ($08, $21, $BC, $CF), ($03, $2F, $B5, $C2),
      ($32, $05, $8A, $E1), ($39, $0B, $83, $EC), ($24, $19, $98, $FB), ($2F, $17, $91, $F6),
      ($8D, $76, $4D, $D6), ($86, $78, $44, $DB), ($9B, $6A, $5F, $CC), ($90, $64, $56, $C1),
      ($A1, $4E, $69, $E2), ($AA, $40, $60, $EF), ($B7, $52, $7B, $F8), ($BC, $5C, $72, $F5),
      ($D5, $06, $05, $BE), ($DE, $08, $0C, $B3), ($C3, $1A, $17, $A4), ($C8, $14, $1E, $A9),
      ($F9, $3E, $21, $8A), ($F2, $30, $28, $87), ($EF, $22, $33, $90), ($E4, $2C, $3A, $9D),
      ($3D, $96, $DD, $06), ($36, $98, $D4, $0B), ($2B, $8A, $CF, $1C), ($20, $84, $C6, $11),
      ($11, $AE, $F9, $32), ($1A, $A0, $F0, $3F), ($07, $B2, $EB, $28), ($0C, $BC, $E2, $25),
      ($65, $E6, $95, $6E), ($6E, $E8, $9C, $63), ($73, $FA, $87, $74), ($78, $F4, $8E, $79),
      ($49, $DE, $B1, $5A), ($42, $D0, $B8, $57), ($5F, $C2, $A3, $40), ($54, $CC, $AA, $4D),
      ($F7, $41, $EC, $DA), ($FC, $4F, $E5, $D7), ($E1, $5D, $FE, $C0), ($EA, $53, $F7, $CD),
      ($DB, $79, $C8, $EE), ($D0, $77, $C1, $E3), ($CD, $65, $DA, $F4), ($C6, $6B, $D3, $F9),
      ($AF, $31, $A4, $B2), ($A4, $3F, $AD, $BF), ($B9, $2D, $B6, $A8), ($B2, $23, $BF, $A5),
      ($83, $09, $80, $86), ($88, $07, $89, $8B), ($95, $15, $92, $9C), ($9E, $1B, $9B, $91),
      ($47, $A1, $7C, $0A), ($4C, $AF, $75, $07), ($51, $BD, $6E, $10), ($5A, $B3, $67, $1D),
      ($6B, $99, $58, $3E), ($60, $97, $51, $33), ($7D, $85, $4A, $24), ($76, $8B, $43, $29),
      ($1F, $D1, $34, $62), ($14, $DF, $3D, $6F), ($09, $CD, $26, $78), ($02, $C3, $2F, $75),
      ($33, $E9, $10, $56), ($38, $E7, $19, $5B), ($25, $F5, $02, $4C), ($2E, $FB, $0B, $41),
      ($8C, $9A, $D7, $61), ($87, $94, $DE, $6C), ($9A, $86, $C5, $7B), ($91, $88, $CC, $76),
      ($A0, $A2, $F3, $55), ($AB, $AC, $FA, $58), ($B6, $BE, $E1, $4F), ($BD, $B0, $E8, $42),
      ($D4, $EA, $9F, $09), ($DF, $E4, $96, $04), ($C2, $F6, $8D, $13), ($C9, $F8, $84, $1E),
      ($F8, $D2, $BB, $3D), ($F3, $DC, $B2, $30), ($EE, $CE, $A9, $27), ($E5, $C0, $A0, $2A),
      ($3C, $7A, $47, $B1), ($37, $74, $4E, $BC), ($2A, $66, $55, $AB), ($21, $68, $5C, $A6),
      ($10, $42, $63, $85), ($1B, $4C, $6A, $88), ($06, $5E, $71, $9F), ($0D, $50, $78, $92),
      ($64, $0A, $0F, $D9), ($6F, $04, $06, $D4), ($72, $16, $1D, $C3), ($79, $18, $14, $CE),
      ($48, $32, $2B, $ED), ($43, $3C, $22, $E0), ($5E, $2E, $39, $F7), ($55, $20, $30, $FA),
      ($01, $EC, $9A, $B7), ($0A, $E2, $93, $BA), ($17, $F0, $88, $AD), ($1C, $FE, $81, $A0),
      ($2D, $D4, $BE, $83), ($26, $DA, $B7, $8E), ($3B, $C8, $AC, $99), ($30, $C6, $A5, $94),
      ($59, $9C, $D2, $DF), ($52, $92, $DB, $D2), ($4F, $80, $C0, $C5), ($44, $8E, $C9, $C8),
      ($75, $A4, $F6, $EB), ($7E, $AA, $FF, $E6), ($63, $B8, $E4, $F1), ($68, $B6, $ED, $FC),
      ($B1, $0C, $0A, $67), ($BA, $02, $03, $6A), ($A7, $10, $18, $7D), ($AC, $1E, $11, $70),
      ($9D, $34, $2E, $53), ($96, $3A, $27, $5E), ($8B, $28, $3C, $49), ($80, $26, $35, $44),
      ($E9, $7C, $42, $0F), ($E2, $72, $4B, $02), ($FF, $60, $50, $15), ($F4, $6E, $59, $18),
      ($C5, $44, $66, $3B), ($CE, $4A, $6F, $36), ($D3, $58, $74, $21), ($D8, $56, $7D, $2C),
      ($7A, $37, $A1, $0C), ($71, $39, $A8, $01), ($6C, $2B, $B3, $16), ($67, $25, $BA, $1B),
      ($56, $0F, $85, $38), ($5D, $01, $8C, $35), ($40, $13, $97, $22), ($4B, $1D, $9E, $2F),
      ($22, $47, $E9, $64), ($29, $49, $E0, $69), ($34, $5B, $FB, $7E), ($3F, $55, $F2, $73),
      ($0E, $7F, $CD, $50), ($05, $71, $C4, $5D), ($18, $63, $DF, $4A), ($13, $6D, $D6, $47),
      ($CA, $D7, $31, $DC), ($C1, $D9, $38, $D1), ($DC, $CB, $23, $C6), ($D7, $C5, $2A, $CB),
      ($E6, $EF, $15, $E8), ($ED, $E1, $1C, $E5), ($F0, $F3, $07, $F2), ($FB, $FD, $0E, $FF),
      ($92, $A7, $79, $B4), ($99, $A9, $70, $B9), ($84, $BB, $6B, $AE), ($8F, $B5, $62, $A3),
      ($BE, $9F, $5D, $80), ($B5, $91, $54, $8D), ($A8, $83, $4F, $9A), ($A3, $8D, $46, $97));
    U3: array [0 .. 255, 0 .. 3] of Byte = (
      ($00, $00, $00, $00), ($0D, $0B, $0E, $09), ($1A, $16, $1C, $12), ($17, $1D, $12, $1B),
      ($34, $2C, $38, $24), ($39, $27, $36, $2D), ($2E, $3A, $24, $36), ($23, $31, $2A, $3F),
      ($68, $58, $70, $48), ($65, $53, $7E, $41), ($72, $4E, $6C, $5A), ($7F, $45, $62, $53),
      ($5C, $74, $48, $6C), ($51, $7F, $46, $65), ($46, $62, $54, $7E), ($4B, $69, $5A, $77),
      ($D0, $B0, $E0, $90), ($DD, $BB, $EE, $99), ($CA, $A6, $FC, $82), ($C7, $AD, $F2, $8B),
      ($E4, $9C, $D8, $B4), ($E9, $97, $D6, $BD), ($FE, $8A, $C4, $A6), ($F3, $81, $CA, $AF),
      ($B8, $E8, $90, $D8), ($B5, $E3, $9E, $D1), ($A2, $FE, $8C, $CA), ($AF, $F5, $82, $C3),
      ($8C, $C4, $A8, $FC), ($81, $CF, $A6, $F5), ($96, $D2, $B4, $EE), ($9B, $D9, $BA, $E7),
      ($BB, $7B, $DB, $3B), ($B6, $70, $D5, $32), ($A1, $6D, $C7, $29), ($AC, $66, $C9, $20),
      ($8F, $57, $E3, $1F), ($82, $5C, $ED, $16), ($95, $41, $FF, $0D), ($98, $4A, $F1, $04),
      ($D3, $23, $AB, $73), ($DE, $28, $A5, $7A), ($C9, $35, $B7, $61), ($C4, $3E, $B9, $68),
      ($E7, $0F, $93, $57), ($EA, $04, $9D, $5E), ($FD, $19, $8F, $45), ($F0, $12, $81, $4C),
      ($6B, $CB, $3B, $AB), ($66, $C0, $35, $A2), ($71, $DD, $27, $B9), ($7C, $D6, $29, $B0),
      ($5F, $E7, $03, $8F), ($52, $EC, $0D, $86), ($45, $F1, $1F, $9D), ($48, $FA, $11, $94),
      ($03, $93, $4B, $E3), ($0E, $98, $45, $EA), ($19, $85, $57, $F1), ($14, $8E, $59, $F8),
      ($37, $BF, $73, $C7), ($3A, $B4, $7D, $CE), ($2D, $A9, $6F, $D5), ($20, $A2, $61, $DC),
      ($6D, $F6, $AD, $76), ($60, $FD, $A3, $7F), ($77, $E0, $B1, $64), ($7A, $EB, $BF, $6D),
      ($59, $DA, $95, $52), ($54, $D1, $9B, $5B), ($43, $CC, $89, $40), ($4E, $C7, $87, $49),
      ($05, $AE, $DD, $3E), ($08, $A5, $D3, $37), ($1F, $B8, $C1, $2C), ($12, $B3, $CF, $25),
      ($31, $82, $E5, $1A), ($3C, $89, $EB, $13), ($2B, $94, $F9, $08), ($26, $9F, $F7, $01),
      ($BD, $46, $4D, $E6), ($B0, $4D, $43, $EF), ($A7, $50, $51, $F4), ($AA, $5B, $5F, $FD),
      ($89, $6A, $75, $C2), ($84, $61, $7B, $CB), ($93, $7C, $69, $D0), ($9E, $77, $67, $D9),
      ($D5, $1E, $3D, $AE), ($D8, $15, $33, $A7), ($CF, $08, $21, $BC), ($C2, $03, $2F, $B5),
      ($E1, $32, $05, $8A), ($EC, $39, $0B, $83), ($FB, $24, $19, $98), ($F6, $2F, $17, $91),
      ($D6, $8D, $76, $4D), ($DB, $86, $78, $44), ($CC, $9B, $6A, $5F), ($C1, $90, $64, $56),
      ($E2, $A1, $4E, $69), ($EF, $AA, $40, $60), ($F8, $B7, $52, $7B), ($F5, $BC, $5C, $72),
      ($BE, $D5, $06, $05), ($B3, $DE, $08, $0C), ($A4, $C3, $1A, $17), ($A9, $C8, $14, $1E),
      ($8A, $F9, $3E, $21), ($87, $F2, $30, $28), ($90, $EF, $22, $33), ($9D, $E4, $2C, $3A),
      ($06, $3D, $96, $DD), ($0B, $36, $98, $D4), ($1C, $2B, $8A, $CF), ($11, $20, $84, $C6),
      ($32, $11, $AE, $F9), ($3F, $1A, $A0, $F0), ($28, $07, $B2, $EB), ($25, $0C, $BC, $E2),
      ($6E, $65, $E6, $95), ($63, $6E, $E8, $9C), ($74, $73, $FA, $87), ($79, $78, $F4, $8E),
      ($5A, $49, $DE, $B1), ($57, $42, $D0, $B8), ($40, $5F, $C2, $A3), ($4D, $54, $CC, $AA),
      ($DA, $F7, $41, $EC), ($D7, $FC, $4F, $E5), ($C0, $E1, $5D, $FE), ($CD, $EA, $53, $F7),
      ($EE, $DB, $79, $C8), ($E3, $D0, $77, $C1), ($F4, $CD, $65, $DA), ($F9, $C6, $6B, $D3),
      ($B2, $AF, $31, $A4), ($BF, $A4, $3F, $AD), ($A8, $B9, $2D, $B6), ($A5, $B2, $23, $BF),
      ($86, $83, $09, $80), ($8B, $88, $07, $89), ($9C, $95, $15, $92), ($91, $9E, $1B, $9B),
      ($0A, $47, $A1, $7C), ($07, $4C, $AF, $75), ($10, $51, $BD, $6E), ($1D, $5A, $B3, $67),
      ($3E, $6B, $99, $58), ($33, $60, $97, $51), ($24, $7D, $85, $4A), ($29, $76, $8B, $43),
      ($62, $1F, $D1, $34), ($6F, $14, $DF, $3D), ($78, $09, $CD, $26), ($75, $02, $C3, $2F),
      ($56, $33, $E9, $10), ($5B, $38, $E7, $19), ($4C, $25, $F5, $02), ($41, $2E, $FB, $0B),
      ($61, $8C, $9A, $D7), ($6C, $87, $94, $DE), ($7B, $9A, $86, $C5), ($76, $91, $88, $CC),
      ($55, $A0, $A2, $F3), ($58, $AB, $AC, $FA), ($4F, $B6, $BE, $E1), ($42, $BD, $B0, $E8),
      ($09, $D4, $EA, $9F), ($04, $DF, $E4, $96), ($13, $C2, $F6, $8D), ($1E, $C9, $F8, $84),
      ($3D, $F8, $D2, $BB), ($30, $F3, $DC, $B2), ($27, $EE, $CE, $A9), ($2A, $E5, $C0, $A0),
      ($B1, $3C, $7A, $47), ($BC, $37, $74, $4E), ($AB, $2A, $66, $55), ($A6, $21, $68, $5C),
      ($85, $10, $42, $63), ($88, $1B, $4C, $6A), ($9F, $06, $5E, $71), ($92, $0D, $50, $78),
      ($D9, $64, $0A, $0F), ($D4, $6F, $04, $06), ($C3, $72, $16, $1D), ($CE, $79, $18, $14),
      ($ED, $48, $32, $2B), ($E0, $43, $3C, $22), ($F7, $5E, $2E, $39), ($FA, $55, $20, $30),
      ($B7, $01, $EC, $9A), ($BA, $0A, $E2, $93), ($AD, $17, $F0, $88), ($A0, $1C, $FE, $81),
      ($83, $2D, $D4, $BE), ($8E, $26, $DA, $B7), ($99, $3B, $C8, $AC), ($94, $30, $C6, $A5),
      ($DF, $59, $9C, $D2), ($D2, $52, $92, $DB), ($C5, $4F, $80, $C0), ($C8, $44, $8E, $C9),
      ($EB, $75, $A4, $F6), ($E6, $7E, $AA, $FF), ($F1, $63, $B8, $E4), ($FC, $68, $B6, $ED),
      ($67, $B1, $0C, $0A), ($6A, $BA, $02, $03), ($7D, $A7, $10, $18), ($70, $AC, $1E, $11),
      ($53, $9D, $34, $2E), ($5E, $96, $3A, $27), ($49, $8B, $28, $3C), ($44, $80, $26, $35),
      ($0F, $E9, $7C, $42), ($02, $E2, $72, $4B), ($15, $FF, $60, $50), ($18, $F4, $6E, $59),
      ($3B, $C5, $44, $66), ($36, $CE, $4A, $6F), ($21, $D3, $58, $74), ($2C, $D8, $56, $7D),
      ($0C, $7A, $37, $A1), ($01, $71, $39, $A8), ($16, $6C, $2B, $B3), ($1B, $67, $25, $BA),
      ($38, $56, $0F, $85), ($35, $5D, $01, $8C), ($22, $40, $13, $97), ($2F, $4B, $1D, $9E),
      ($64, $22, $47, $E9), ($69, $29, $49, $E0), ($7E, $34, $5B, $FB), ($73, $3F, $55, $F2),
      ($50, $0E, $7F, $CD), ($5D, $05, $71, $C4), ($4A, $18, $63, $DF), ($47, $13, $6D, $D6),
      ($DC, $CA, $D7, $31), ($D1, $C1, $D9, $38), ($C6, $DC, $CB, $23), ($CB, $D7, $C5, $2A),
      ($E8, $E6, $EF, $15), ($E5, $ED, $E1, $1C), ($F2, $F0, $F3, $07), ($FF, $FB, $FD, $0E),
      ($B4, $92, $A7, $79), ($B9, $99, $A9, $70), ($AE, $84, $BB, $6B), ($A3, $8F, $B5, $62),
      ($80, $BE, $9F, $5D), ($8D, $B5, $91, $54), ($9A, $A8, $83, $4F), ($97, $A3, $8D, $46));
    U4: array [0 .. 255, 0 .. 3] of Byte = (
      ($00, $00, $00, $00), ($09, $0D, $0B, $0E), ($12, $1A, $16, $1C), ($1B, $17, $1D, $12),
      ($24, $34, $2C, $38), ($2D, $39, $27, $36), ($36, $2E, $3A, $24), ($3F, $23, $31, $2A),
      ($48, $68, $58, $70), ($41, $65, $53, $7E), ($5A, $72, $4E, $6C), ($53, $7F, $45, $62),
      ($6C, $5C, $74, $48), ($65, $51, $7F, $46), ($7E, $46, $62, $54), ($77, $4B, $69, $5A),
      ($90, $D0, $B0, $E0), ($99, $DD, $BB, $EE), ($82, $CA, $A6, $FC), ($8B, $C7, $AD, $F2),
      ($B4, $E4, $9C, $D8), ($BD, $E9, $97, $D6), ($A6, $FE, $8A, $C4), ($AF, $F3, $81, $CA),
      ($D8, $B8, $E8, $90), ($D1, $B5, $E3, $9E), ($CA, $A2, $FE, $8C), ($C3, $AF, $F5, $82),
      ($FC, $8C, $C4, $A8), ($F5, $81, $CF, $A6), ($EE, $96, $D2, $B4), ($E7, $9B, $D9, $BA),
      ($3B, $BB, $7B, $DB), ($32, $B6, $70, $D5), ($29, $A1, $6D, $C7), ($20, $AC, $66, $C9),
      ($1F, $8F, $57, $E3), ($16, $82, $5C, $ED), ($0D, $95, $41, $FF), ($04, $98, $4A, $F1),
      ($73, $D3, $23, $AB), ($7A, $DE, $28, $A5), ($61, $C9, $35, $B7), ($68, $C4, $3E, $B9),
      ($57, $E7, $0F, $93), ($5E, $EA, $04, $9D), ($45, $FD, $19, $8F), ($4C, $F0, $12, $81),
      ($AB, $6B, $CB, $3B), ($A2, $66, $C0, $35), ($B9, $71, $DD, $27), ($B0, $7C, $D6, $29),
      ($8F, $5F, $E7, $03), ($86, $52, $EC, $0D), ($9D, $45, $F1, $1F), ($94, $48, $FA, $11),
      ($E3, $03, $93, $4B), ($EA, $0E, $98, $45), ($F1, $19, $85, $57), ($F8, $14, $8E, $59),
      ($C7, $37, $BF, $73), ($CE, $3A, $B4, $7D), ($D5, $2D, $A9, $6F), ($DC, $20, $A2, $61),
      ($76, $6D, $F6, $AD), ($7F, $60, $FD, $A3), ($64, $77, $E0, $B1), ($6D, $7A, $EB, $BF),
      ($52, $59, $DA, $95), ($5B, $54, $D1, $9B), ($40, $43, $CC, $89), ($49, $4E, $C7, $87),
      ($3E, $05, $AE, $DD), ($37, $08, $A5, $D3), ($2C, $1F, $B8, $C1), ($25, $12, $B3, $CF),
      ($1A, $31, $82, $E5), ($13, $3C, $89, $EB), ($08, $2B, $94, $F9), ($01, $26, $9F, $F7),
      ($E6, $BD, $46, $4D), ($EF, $B0, $4D, $43), ($F4, $A7, $50, $51), ($FD, $AA, $5B, $5F),
      ($C2, $89, $6A, $75), ($CB, $84, $61, $7B), ($D0, $93, $7C, $69), ($D9, $9E, $77, $67),
      ($AE, $D5, $1E, $3D), ($A7, $D8, $15, $33), ($BC, $CF, $08, $21), ($B5, $C2, $03, $2F),
      ($8A, $E1, $32, $05), ($83, $EC, $39, $0B), ($98, $FB, $24, $19), ($91, $F6, $2F, $17),
      ($4D, $D6, $8D, $76), ($44, $DB, $86, $78), ($5F, $CC, $9B, $6A), ($56, $C1, $90, $64),
      ($69, $E2, $A1, $4E), ($60, $EF, $AA, $40), ($7B, $F8, $B7, $52), ($72, $F5, $BC, $5C),
      ($05, $BE, $D5, $06), ($0C, $B3, $DE, $08), ($17, $A4, $C3, $1A), ($1E, $A9, $C8, $14),
      ($21, $8A, $F9, $3E), ($28, $87, $F2, $30), ($33, $90, $EF, $22), ($3A, $9D, $E4, $2C),
      ($DD, $06, $3D, $96), ($D4, $0B, $36, $98), ($CF, $1C, $2B, $8A), ($C6, $11, $20, $84),
      ($F9, $32, $11, $AE), ($F0, $3F, $1A, $A0), ($EB, $28, $07, $B2), ($E2, $25, $0C, $BC),
      ($95, $6E, $65, $E6), ($9C, $63, $6E, $E8), ($87, $74, $73, $FA), ($8E, $79, $78, $F4),
      ($B1, $5A, $49, $DE), ($B8, $57, $42, $D0), ($A3, $40, $5F, $C2), ($AA, $4D, $54, $CC),
      ($EC, $DA, $F7, $41), ($E5, $D7, $FC, $4F), ($FE, $C0, $E1, $5D), ($F7, $CD, $EA, $53),
      ($C8, $EE, $DB, $79), ($C1, $E3, $D0, $77), ($DA, $F4, $CD, $65), ($D3, $F9, $C6, $6B),
      ($A4, $B2, $AF, $31), ($AD, $BF, $A4, $3F), ($B6, $A8, $B9, $2D), ($BF, $A5, $B2, $23),
      ($80, $86, $83, $09), ($89, $8B, $88, $07), ($92, $9C, $95, $15), ($9B, $91, $9E, $1B),
      ($7C, $0A, $47, $A1), ($75, $07, $4C, $AF), ($6E, $10, $51, $BD), ($67, $1D, $5A, $B3),
      ($58, $3E, $6B, $99), ($51, $33, $60, $97), ($4A, $24, $7D, $85), ($43, $29, $76, $8B),
      ($34, $62, $1F, $D1), ($3D, $6F, $14, $DF), ($26, $78, $09, $CD), ($2F, $75, $02, $C3),
      ($10, $56, $33, $E9), ($19, $5B, $38, $E7), ($02, $4C, $25, $F5), ($0B, $41, $2E, $FB),
      ($D7, $61, $8C, $9A), ($DE, $6C, $87, $94), ($C5, $7B, $9A, $86), ($CC, $76, $91, $88),
      ($F3, $55, $A0, $A2), ($FA, $58, $AB, $AC), ($E1, $4F, $B6, $BE), ($E8, $42, $BD, $B0),
      ($9F, $09, $D4, $EA), ($96, $04, $DF, $E4), ($8D, $13, $C2, $F6), ($84, $1E, $C9, $F8),
      ($BB, $3D, $F8, $D2), ($B2, $30, $F3, $DC), ($A9, $27, $EE, $CE), ($A0, $2A, $E5, $C0),
      ($47, $B1, $3C, $7A), ($4E, $BC, $37, $74), ($55, $AB, $2A, $66), ($5C, $A6, $21, $68),
      ($63, $85, $10, $42), ($6A, $88, $1B, $4C), ($71, $9F, $06, $5E), ($78, $92, $0D, $50),
      ($0F, $D9, $64, $0A), ($06, $D4, $6F, $04), ($1D, $C3, $72, $16), ($14, $CE, $79, $18),
      ($2B, $ED, $48, $32), ($22, $E0, $43, $3C), ($39, $F7, $5E, $2E), ($30, $FA, $55, $20),
      ($9A, $B7, $01, $EC), ($93, $BA, $0A, $E2), ($88, $AD, $17, $F0), ($81, $A0, $1C, $FE),
      ($BE, $83, $2D, $D4), ($B7, $8E, $26, $DA), ($AC, $99, $3B, $C8), ($A5, $94, $30, $C6),
      ($D2, $DF, $59, $9C), ($DB, $D2, $52, $92), ($C0, $C5, $4F, $80), ($C9, $C8, $44, $8E),
      ($F6, $EB, $75, $A4), ($FF, $E6, $7E, $AA), ($E4, $F1, $63, $B8), ($ED, $FC, $68, $B6),
      ($0A, $67, $B1, $0C), ($03, $6A, $BA, $02), ($18, $7D, $A7, $10), ($11, $70, $AC, $1E),
      ($2E, $53, $9D, $34), ($27, $5E, $96, $3A), ($3C, $49, $8B, $28), ($35, $44, $80, $26),
      ($42, $0F, $E9, $7C), ($4B, $02, $E2, $72), ($50, $15, $FF, $60), ($59, $18, $F4, $6E),
      ($66, $3B, $C5, $44), ($6F, $36, $CE, $4A), ($74, $21, $D3, $58), ($7D, $2C, $D8, $56),
      ($A1, $0C, $7A, $37), ($A8, $01, $71, $39), ($B3, $16, $6C, $2B), ($BA, $1B, $67, $25),
      ($85, $38, $56, $0F), ($8C, $35, $5D, $01), ($97, $22, $40, $13), ($9E, $2F, $4B, $1D),
      ($E9, $64, $22, $47), ($E0, $69, $29, $49), ($FB, $7E, $34, $5B), ($F2, $73, $3F, $55),
      ($CD, $50, $0E, $7F), ($C4, $5D, $05, $71), ($DF, $4A, $18, $63), ($D6, $47, $13, $6D),
      ($31, $DC, $CA, $D7), ($38, $D1, $C1, $D9), ($23, $C6, $DC, $CB), ($2A, $CB, $D7, $C5),
      ($15, $E8, $E6, $EF), ($1C, $E5, $ED, $E1), ($07, $F2, $F0, $F3), ($0E, $FF, $FB, $FD),
      ($79, $B4, $92, $A7), ($70, $B9, $99, $A9), ($6B, $AE, $84, $BB), ($62, $A3, $8F, $B5),
      ($5D, $80, $BE, $9F), ($54, $8D, $B5, $91), ($4F, $9A, $A8, $83), ($46, $97, $A3, $8D));

    rcon: array [0 .. 29] of Cardinal = (
      $01, $02, $04, $08, $10, $20, $40, $80, $1B, $36, $6C, $D8, $AB, $4D, $9A,
      $2F, $5E, $BC, $63, $C6, $97, $35, $6A, $D4, $B3, $7D, $FA, $EF, $C5, $91);

{$ENDREGION 'RijndaelDefine'}
    class procedure InvMixColumn(const a: PByteArray; const BC: Byte);
  public
    class procedure InitKey(buff: Pointer; Size: Integer; var KeyContext: TRijndaelkey);
    class procedure Encrypt(var KeyContext: TRijndaelkey; var Data: TRijndaelBlock); overload;
    class procedure Encrypt(var KeyContext: TRijndaelkey; var B1, B2, B3, B4: DWORD); overload;
    class procedure Decrypt(var KeyContext: TRijndaelkey; var Data: TRijndaelBlock); overload;
    class procedure Decrypt(var KeyContext: TRijndaelkey; var B1, B2, B3, B4: DWORD); overload;
  end;

type
  { twofish }
  PTwofishKey = ^TTwofishKey;

  TTwofishKey = record
    ExpandedKey: array [0 .. 39] of DWORD;
    SBoxKey: array [0 .. 3] of DWORD;
    SBox0: array [0 .. 255] of Byte;
    SBox1: array [0 .. 255] of Byte;
    SBox2: array [0 .. 255] of Byte;
    SBox3: array [0 .. 255] of Byte;
    KeyLen: Integer;
  end;

  PTwofishBlock = ^TTwofishBlock;
  TTwofishBlock = array [0 .. 15] of Byte;

  TTwofish = class(TCoreClassObject)
  private const
{$REGION 'TwofishDefine'}
    P8x8: array [0 .. 1, 0 .. 255] of Byte =
      (($A9, $67, $B3, $E8, $04, $FD, $A3, $76,
      $9A, $92, $80, $78, $E4, $DD, $D1, $38,
      $0D, $C6, $35, $98, $18, $F7, $EC, $6C,
      $43, $75, $37, $26, $FA, $13, $94, $48,
      $F2, $D0, $8B, $30, $84, $54, $DF, $23,
      $19, $5B, $3D, $59, $F3, $AE, $A2, $82,
      $63, $01, $83, $2E, $D9, $51, $9B, $7C,
      $A6, $EB, $A5, $BE, $16, $0C, $E3, $61,
      $C0, $8C, $3A, $F5, $73, $2C, $25, $0B,
      $BB, $4E, $89, $6B, $53, $6A, $B4, $F1,
      $E1, $E6, $BD, $45, $E2, $F4, $B6, $66,
      $CC, $95, $03, $56, $D4, $1C, $1E, $D7,
      $FB, $C3, $8E, $B5, $E9, $CF, $BF, $BA,
      $EA, $77, $39, $AF, $33, $C9, $62, $71,
      $81, $79, $09, $AD, $24, $CD, $F9, $D8,
      $E5, $C5, $B9, $4D, $44, $08, $86, $E7,
      $A1, $1D, $AA, $ED, $06, $70, $B2, $D2,
      $41, $7B, $A0, $11, $31, $C2, $27, $90,
      $20, $F6, $60, $FF, $96, $5C, $B1, $AB,
      $9E, $9C, $52, $1B, $5F, $93, $0A, $EF,
      $91, $85, $49, $EE, $2D, $4F, $8F, $3B,
      $47, $87, $6D, $46, $D6, $3E, $69, $64,
      $2A, $CE, $CB, $2F, $FC, $97, $05, $7A,
      $AC, $7F, $D5, $1A, $4B, $0E, $A7, $5A,
      $28, $14, $3F, $29, $88, $3C, $4C, $02,
      $B8, $DA, $B0, $17, $55, $1F, $8A, $7D,
      $57, $C7, $8D, $74, $B7, $C4, $9F, $72,
      $7E, $15, $22, $12, $58, $07, $99, $34,
      $6E, $50, $DE, $68, $65, $BC, $DB, $F8,
      $C8, $A8, $2B, $40, $DC, $FE, $32, $A4,
      $CA, $10, $21, $F0, $D3, $5D, $0F, $00,
      $6F, $9D, $36, $42, $4A, $5E, $C1, $E0),

      ($75, $F3, $C6, $F4, $DB, $7B, $FB, $C8,
      $4A, $D3, $E6, $6B, $45, $7D, $E8, $4B,
      $D6, $32, $D8, $FD, $37, $71, $F1, $E1,
      $30, $0F, $F8, $1B, $87, $FA, $06, $3F,
      $5E, $BA, $AE, $5B, $8A, $00, $BC, $9D,
      $6D, $C1, $B1, $0E, $80, $5D, $D2, $D5,
      $A0, $84, $07, $14, $B5, $90, $2C, $A3,
      $B2, $73, $4C, $54, $92, $74, $36, $51,
      $38, $B0, $BD, $5A, $FC, $60, $62, $96,
      $6C, $42, $F7, $10, $7C, $28, $27, $8C,
      $13, $95, $9C, $C7, $24, $46, $3B, $70,
      $CA, $E3, $85, $CB, $11, $D0, $93, $B8,
      $A6, $83, $20, $FF, $9F, $77, $C3, $CC,
      $03, $6F, $08, $BF, $40, $E7, $2B, $E2,
      $79, $0C, $AA, $82, $41, $3A, $EA, $B9,
      $E4, $9A, $A4, $97, $7E, $DA, $7A, $17,
      $66, $94, $A1, $1D, $3D, $F0, $DE, $B3,
      $0B, $72, $A7, $1C, $EF, $D1, $53, $3E,
      $8F, $33, $26, $5F, $EC, $76, $2A, $49,
      $81, $88, $EE, $21, $C4, $1A, $EB, $D9,
      $C5, $39, $99, $CD, $AD, $31, $8B, $01,
      $18, $23, $DD, $1F, $4E, $2D, $F9, $48,
      $4F, $F2, $65, $8E, $78, $5C, $58, $19,
      $8D, $E5, $98, $57, $67, $7F, $05, $64,
      $AF, $63, $B6, $FE, $F5, $B7, $3C, $A5,
      $CE, $E9, $68, $44, $E0, $4D, $43, $69,
      $29, $2E, $AC, $15, $59, $A8, $0A, $9E,
      $6E, $47, $DF, $34, $35, $6A, $CF, $DC,
      $22, $C9, $C0, $9B, $89, $D4, $ED, $AB,
      $12, $A2, $0D, $52, $BB, $02, $2F, $A9,
      $D7, $61, $1E, $B4, $50, $04, $F6, $C2,
      $16, $25, $86, $56, $55, $09, $BE, $91));

    MDS: array [0 .. 3, 0 .. 7] of Byte = (($01, $A4, $55, $87, $5A, $58, $DB, $9E),
      ($A4, $56, $82, $F3, $1E, $C6, $68, $E5),
      ($02, $A1, $FC, $C1, $47, $AE, $3D, $19),
      ($A4, $55, $87, $5A, $58, $DB, $9E, $03));

    Arr5B: array [0 .. 255] of DWORD = (
      $00, $5B, $B6, $ED, $05, $5E, $B3, $E8, $0A, $51, $BC, $E7, $0F, $54, $B9, $E2,
      $14, $4F, $A2, $F9, $11, $4A, $A7, $FC, $1E, $45, $A8, $F3, $1B, $40, $AD, $F6,
      $28, $73, $9E, $C5, $2D, $76, $9B, $C0, $22, $79, $94, $CF, $27, $7C, $91, $CA,
      $3C, $67, $8A, $D1, $39, $62, $8F, $D4, $36, $6D, $80, $DB, $33, $68, $85, $DE,
      $50, $0B, $E6, $BD, $55, $0E, $E3, $B8, $5A, $01, $EC, $B7, $5F, $04, $E9, $B2,
      $44, $1F, $F2, $A9, $41, $1A, $F7, $AC, $4E, $15, $F8, $A3, $4B, $10, $FD, $A6,
      $78, $23, $CE, $95, $7D, $26, $CB, $90, $72, $29, $C4, $9F, $77, $2C, $C1, $9A,
      $6C, $37, $DA, $81, $69, $32, $DF, $84, $66, $3D, $D0, $8B, $63, $38, $D5, $8E,
      $A0, $FB, $16, $4D, $A5, $FE, $13, $48, $AA, $F1, $1C, $47, $AF, $F4, $19, $42,
      $B4, $EF, $02, $59, $B1, $EA, $07, $5C, $BE, $E5, $08, $53, $BB, $E0, $0D, $56,
      $88, $D3, $3E, $65, $8D, $D6, $3B, $60, $82, $D9, $34, $6F, $87, $DC, $31, $6A,
      $9C, $C7, $2A, $71, $99, $C2, $2F, $74, $96, $CD, $20, $7B, $93, $C8, $25, $7E,
      $F0, $AB, $46, $1D, $F5, $AE, $43, $18, $FA, $A1, $4C, $17, $FF, $A4, $49, $12,
      $E4, $BF, $52, $09, $E1, $BA, $57, $0C, $EE, $B5, $58, $03, $EB, $B0, $5D, $06,
      $D8, $83, $6E, $35, $DD, $86, $6B, $30, $D2, $89, $64, $3F, $D7, $8C, $61, $3A,
      $CC, $97, $7A, $21, $C9, $92, $7F, $24, $C6, $9D, $70, $2B, $C3, $98, $75, $2E);

    ArrEF: array [0 .. 255] of Byte = (
      $00, $EF, $B7, $58, $07, $E8, $B0, $5F, $0E, $E1, $B9, $56, $09, $E6, $BE, $51,
      $1C, $F3, $AB, $44, $1B, $F4, $AC, $43, $12, $FD, $A5, $4A, $15, $FA, $A2, $4D,
      $38, $D7, $8F, $60, $3F, $D0, $88, $67, $36, $D9, $81, $6E, $31, $DE, $86, $69,
      $24, $CB, $93, $7C, $23, $CC, $94, $7B, $2A, $C5, $9D, $72, $2D, $C2, $9A, $75,
      $70, $9F, $C7, $28, $77, $98, $C0, $2F, $7E, $91, $C9, $26, $79, $96, $CE, $21,
      $6C, $83, $DB, $34, $6B, $84, $DC, $33, $62, $8D, $D5, $3A, $65, $8A, $D2, $3D,
      $48, $A7, $FF, $10, $4F, $A0, $F8, $17, $46, $A9, $F1, $1E, $41, $AE, $F6, $19,
      $54, $BB, $E3, $0C, $53, $BC, $E4, $0B, $5A, $B5, $ED, $02, $5D, $B2, $EA, $05,
      $E0, $0F, $57, $B8, $E7, $08, $50, $BF, $EE, $01, $59, $B6, $E9, $06, $5E, $B1,
      $FC, $13, $4B, $A4, $FB, $14, $4C, $A3, $F2, $1D, $45, $AA, $F5, $1A, $42, $AD,
      $D8, $37, $6F, $80, $DF, $30, $68, $87, $D6, $39, $61, $8E, $D1, $3E, $66, $89,
      $C4, $2B, $73, $9C, $C3, $2C, $74, $9B, $CA, $25, $7D, $92, $CD, $22, $7A, $95,
      $90, $7F, $27, $C8, $97, $78, $20, $CF, $9E, $71, $29, $C6, $99, $76, $2E, $C1,
      $8C, $63, $3B, $D4, $8B, $64, $3C, $D3, $82, $6D, $35, $DA, $85, $6A, $32, $DD,
      $A8, $47, $1F, $F0, $AF, $40, $18, $F7, $A6, $49, $11, $FE, $A1, $4E, $16, $F9,
      $B4, $5B, $03, $EC, $B3, $5C, $04, $EB, $BA, $55, $0D, $E2, $BD, $52, $0A, $E5);
{$ENDREGION 'TwofishDefine'}
    class function TwofishCalculateSBoxes(x: DWORD; l: Pointer; KeySize: DWORD): DWORD;
    class function TwofishH(x: DWORD; l: Pointer; KeySize: DWORD): DWORD; overload;
    class function TwofishH(const x: DWORD; const key: TTwofishKey): DWORD; overload;
    class function RSMDSMul(const x, y: Byte): Byte;
    class function MultiplyMDS(const E, O: DWORD): DWORD;
  public
    class procedure InitKey(buff: PCCByteArray; Size: Integer; var KeyContext: TTwofishKey);
    class procedure Encrypt(var KeyContext: TTwofishKey; var Data: TTwofishBlock);
    class procedure Decrypt(var KeyContext: TTwofishKey; var Data: TTwofishBlock);
  end;
{$ENDREGION 'cryptAndHash'}

implementation

uses DoStatusIO;

const
  { Blowfish lookup tables }
  bf_P: array [0 .. (BFRounds + 1)] of DWORD = (
    $243F6A88, $85A308D3, $13198A2E, $03707344,
    $A4093822, $299F31D0, $082EFA98, $EC4E6C89,
    $452821E6, $38D01377, $BE5466CF, $34E90C6C,
    $C0AC29B7, $C97C50DD, $3F84D5B5, $B5470917,
    $9216D5D9, $8979FB1B);

  bf_S: array [0 .. 3, 0 .. 255] of DWORD =
    (
    ($D1310BA6, $98DFB5AC, $2FFD72DB, $D01ADFB7,
    $B8E1AFED, $6A267E96, $BA7C9045, $F12C7F99,
    $24A19947, $B3916CF7, $0801F2E2, $858EFC16,
    $636920D8, $71574E69, $A458FEA3, $F4933D7E,

    $0D95748F, $728EB658, $718BCD58, $82154AEE,
    $7B54A41D, $C25A59B5, $9C30D539, $2AF26013,
    $C5D1B023, $286085F0, $CA417918, $B8DB38EF,
    $8E79DCB0, $603A180E, $6C9E0E8B, $B01E8A3E,

    $D71577C1, $BD314B27, $78AF2FDA, $55605C60,
    $E65525F3, $AA55AB94, $57489862, $63E81440,
    $55CA396A, $2AAB10B6, $B4CC5C34, $1141E8CE,
    $A15486AF, $7C72E993, $B3EE1411, $636FBC2A,

    $2BA9C55D, $741831F6, $CE5C3E16, $9B87931E,
    $AFD6BA33, $6C24CF5C, $7A325381, $28958677,
    $3B8F4898, $6B4BB9AF, $C4BFE81B, $66282193,
    $61D809CC, $FB21A991, $487CAC60, $5DEC8032,

    $EF845D5D, $E98575B1, $DC262302, $EB651B88,
    $23893E81, $D396ACC5, $0F6D6FF3, $83F44239,
    $2E0B4482, $A4842004, $69C8F04A, $9E1F9B5E,
    $21C66842, $F6E96C9A, $670C9C61, $ABD388F0,

    $6A51A0D2, $D8542F68, $960FA728, $AB5133A3,
    $6EEF0B6C, $137A3BE4, $BA3BF050, $7EFB2A98,
    $A1F1651D, $39AF0176, $66CA593E, $82430E88,
    $8CEE8619, $456F9FB4, $7D84A5C3, $3B8B5EBE,

    $E06F75D8, $85C12073, $401A449F, $56C16AA6,
    $4ED3AA62, $363F7706, $1BFEDF72, $429B023D,
    $37D0D724, $D00A1248, $DB0FEAD3, $49F1C09B,
    $075372C9, $80991B7B, $25D479D8, $F6E8DEF7,

    $E3FE501A, $B6794C3B, $976CE0BD, $04C006BA,
    $C1A94FB6, $409F60C4, $5E5C9EC2, $196A2463,
    $68FB6FAF, $3E6C53B5, $1339B2EB, $3B52EC6F,
    $6DFC511F, $9B30952C, $CC814544, $AF5EBD09,

    $BEE3D004, $DE334AFD, $660F2807, $192E4BB3,
    $C0CBA857, $45C8740F, $D20B5F39, $B9D3FBDB,
    $5579C0BD, $1A60320A, $D6A100C6, $402C7279,
    $679F25FE, $FB1FA3CC, $8EA5E9F8, $DB3222F8,

    $3C7516DF, $FD616B15, $2F501EC8, $AD0552AB,
    $323DB5FA, $FD238760, $53317B48, $3E00DF82,
    $9E5C57BB, $CA6F8CA0, $1A87562E, $DF1769DB,
    $D542A8F6, $287EFFC3, $AC6732C6, $8C4F5573,

    $695B27B0, $BBCA58C8, $E1FFA35D, $B8F011A0,
    $10FA3D98, $FD2183B8, $4AFCB56C, $2DD1D35B,
    $9A53E479, $B6F84565, $D28E49BC, $4BFB9790,
    $E1DDF2DA, $A4CB7E33, $62FB1341, $CEE4C6E8,

    $EF20CADA, $36774C01, $D07E9EFE, $2BF11FB4,
    $95DBDA4D, $AE909198, $EAAD8E71, $6B93D5A0,
    $D08ED1D0, $AFC725E0, $8E3C5B2F, $8E7594B7,
    $8FF6E2FB, $F2122B64, $8888B812, $900DF01C,

    $4FAD5EA0, $688FC31C, $D1CFF191, $B3A8C1AD,
    $2F2F2218, $BE0E1777, $EA752DFE, $8B021FA1,
    $E5A0CC0F, $B56F74E8, $18ACF3D6, $CE89E299,
    $B4A84FE0, $FD13E0B7, $7CC43B81, $D2ADA8D9,

    $165FA266, $80957705, $93CC7314, $211A1477,
    $E6AD2065, $77B5FA86, $C75442F5, $FB9D35CF,
    $EBCDAF0C, $7B3E89A0, $D6411BD3, $AE1E7E49,
    $00250E2D, $2071B35E, $226800BB, $57B8E0AF,

    $2464369B, $F009B91E, $5563911D, $59DFA6AA,
    $78C14389, $D95A537F, $207D5BA2, $02E5B9C5,
    $83260376, $6295CFA9, $11C81968, $4E734A41,
    $B3472DCA, $7B14A94A, $1B510052, $9A532915,

    $D60F573F, $BC9BC6E4, $2B60A476, $81E67400,
    $08BA6FB5, $571BE91F, $F296EC6B, $2A0DD915,
    $B6636521, $E7B9F9B6, $FF34052E, $C5855664,
    $53B02D5D, $A99F8FA1, $08BA4799, $6E85076A),
    { SECOND 256 }
    ($4B7A70E9, $B5B32944, $DB75092E, $C4192623,
    $AD6EA6B0, $49A7DF7D, $9CEE60B8, $8FEDB266,
    $ECAA8C71, $699A17FF, $5664526C, $C2B19EE1,
    $193602A5, $75094C29, $A0591340, $E4183A3E,

    $3F54989A, $5B429D65, $6B8FE4D6, $99F73FD6,
    $A1D29C07, $EFE830F5, $4D2D38E6, $F0255DC1,
    $4CDD2086, $8470EB26, $6382E9C6, $021ECC5E,
    $09686B3F, $3EBAEFC9, $3C971814, $6B6A70A1,

    $687F3584, $52A0E286, $B79C5305, $AA500737,
    $3E07841C, $7FDEAE5C, $8E7D44EC, $5716F2B8,
    $B03ADA37, $F0500C0D, $F01C1F04, $0200B3FF,
    $AE0CF51A, $3CB574B2, $25837A58, $DC0921BD,

    $D19113F9, $7CA92FF6, $94324773, $22F54701,
    $3AE5E581, $37C2DADC, $C8B57634, $9AF3DDA7,
    $A9446146, $0FD0030E, $ECC8C73E, $A4751E41,
    $E238CD99, $3BEA0E2F, $3280BBA1, $183EB331,

    $4E548B38, $4F6DB908, $6F420D03, $F60A04BF,
    $2CB81290, $24977C79, $5679B072, $BCAF89AF,
    $DE9A771F, $D9930810, $B38BAE12, $DCCF3F2E,
    $5512721F, $2E6B7124, $501ADDE6, $9F84CD87,

    $7A584718, $7408DA17, $BC9F9ABC, $E94B7D8C,
    $EC7AEC3A, $DB851DFA, $63094366, $C464C3D2,
    $EF1C1847, $3215D908, $DD433B37, $24C2BA16,
    $12A14D43, $2A65C451, $50940002, $133AE4DD,

    $71DFF89E, $10314E55, $81AC77D6, $5F11199B,
    $043556F1, $D7A3C76B, $3C11183B, $5924A509,
    $F28FE6ED, $97F1FBFA, $9EBABF2C, $1E153C6E,
    $86E34570, $EAE96FB1, $860E5E0A, $5A3E2AB3,

    $771FE71C, $4E3D06FA, $2965DCB9, $99E71D0F,
    $803E89D6, $5266C825, $2E4CC978, $9C10B36A,
    $C6150EBA, $94E2EA78, $A5FC3C53, $1E0A2DF4,
    $F2F74EA7, $361D2B3D, $1939260F, $19C27960,

    $5223A708, $F71312B6, $EBADFE6E, $EAC31F66,
    $E3BC4595, $A67BC883, $B17F37D1, $018CFF28,
    $C332DDEF, $BE6C5AA5, $65582185, $68AB9802,
    $EECEA50F, $DB2F953B, $2AEF7DAD, $5B6E2F84,

    $1521B628, $29076170, $ECDD4775, $619F1510,
    $13CCA830, $EB61BD96, $0334FE1E, $AA0363CF,
    $B5735C90, $4C70A239, $D59E9E0B, $CBAADE14,
    $EECC86BC, $60622CA7, $9CAB5CAB, $B2F3846E,

    $648B1EAF, $19BDF0CA, $A02369B9, $655ABB50,
    $40685A32, $3C2AB4B3, $319EE9D5, $C021B8F7,
    $9B540B19, $875FA099, $95F7997E, $623D7DA8,
    $F837889A, $97E32D77, $11ED935F, $16681281,

    $0E358829, $C7E61FD6, $96DEDFA1, $7858BA99,
    $57F584A5, $1B227263, $9B83C3FF, $1AC24696,
    $CDB30AEB, $532E3054, $8FD948E4, $6DBC3128,
    $58EBF2EF, $34C6FFEA, $FE28ED61, $EE7C3C73,

    $5D4A14D9, $E864B7E3, $42105D14, $203E13E0,
    $45EEE2B6, $A3AAABEA, $DB6C4F15, $FACB4FD0,
    $C742F442, $EF6ABBB5, $654F3B1D, $41CD2105,
    $D81E799E, $86854DC7, $E44B476A, $3D816250,

    $CF62A1F2, $5B8D2646, $FC8883A0, $C1C7B6A3,
    $7F1524C3, $69CB7492, $47848A0B, $5692B285,
    $095BBF00, $AD19489D, $1462B174, $23820E00,
    $58428D2A, $0C55F5EA, $1DADF43E, $233F7061,

    $3372F092, $8D937E41, $D65FECF1, $6C223BDB,
    $7CDE3759, $CBEE7460, $4085F2A7, $CE77326E,
    $A6078084, $19F8509E, $E8EFD855, $61D99735,
    $A969A7AA, $C50C06C2, $5A04ABFC, $800BCADC,

    $9E447A2E, $C3453484, $FDD56705, $0E1E9EC9,
    $DB73DBD3, $105588CD, $675FDA79, $E3674340,
    $C5C43465, $713E38D8, $3D28F89E, $F16DFF20,
    $153E21E7, $8FB03D4A, $E6E39F2B, $DB83ADF7),
    { THIRD 256 }
    ($E93D5A68, $948140F7, $F64C261C, $94692934,
    $411520F7, $7602D4F7, $BCF46B2E, $D4A20068,
    $D4082471, $3320F46A, $43B7D4B7, $500061AF,
    $1E39F62E, $97244546, $14214F74, $BF8B8840,

    $4D95FC1D, $96B591AF, $70F4DDD3, $66A02F45,
    $BFBC09EC, $03BD9785, $7FAC6DD0, $31CB8504,
    $96EB27B3, $55FD3941, $DA2547E6, $ABCA0A9A,
    $28507825, $530429F4, $0A2C86DA, $E9B66DFB,

    $68DC1462, $D7486900, $680EC0A4, $27A18DEE,
    $4F3FFEA2, $E887AD8C, $B58CE006, $7AF4D6B6,
    $AACE1E7C, $D3375FEC, $CE78A399, $406B2A42,
    $20FE9E35, $D9F385B9, $EE39D7AB, $3B124E8B,

    $1DC9FAF7, $4B6D1856, $26A36631, $EAE397B2,
    $3A6EFA74, $DD5B4332, $6841E7F7, $CA7820FB,
    $FB0AF54E, $D8FEB397, $454056AC, $BA489527,
    $55533A3A, $20838D87, $FE6BA9B7, $D096954B,

    $55A867BC, $A1159A58, $CCA92963, $99E1DB33,
    $A62A4A56, $3F3125F9, $5EF47E1C, $9029317C,
    $FDF8E802, $04272F70, $80BB155C, $05282CE3,
    $95C11548, $E4C66D22, $48C1133F, $C70F86DC,

    $07F9C9EE, $41041F0F, $404779A4, $5D886E17,
    $325F51EB, $D59BC0D1, $F2BCC18F, $41113564,
    $257B7834, $602A9C60, $DFF8E8A3, $1F636C1B,
    $0E12B4C2, $02E1329E, $AF664FD1, $CAD18115,

    $6B2395E0, $333E92E1, $3B240B62, $EEBEB922,
    $85B2A20E, $E6BA0D99, $DE720C8C, $2DA2F728,
    $D0127845, $95B794FD, $647D0862, $E7CCF5F0,
    $5449A36F, $877D48FA, $C39DFD27, $F33E8D1E,

    $0A476341, $992EFF74, $3A6F6EAB, $F4F8FD37,
    $A812DC60, $A1EBDDF8, $991BE14C, $DB6E6B0D,
    $C67B5510, $6D672C37, $2765D43B, $DCD0E804,
    $F1290DC7, $CC00FFA3, $B5390F92, $690FED0B,

    $667B9FFB, $CEDB7D9C, $A091CF0B, $D9155EA3,
    $BB132F88, $515BAD24, $7B9479BF, $763BD6EB,
    $37392EB3, $CC115979, $8026E297, $F42E312D,
    $6842ADA7, $C66A2B3B, $12754CCC, $782EF11C,

    $6A124237, $B79251E7, $06A1BBE6, $4BFB6350,
    $1A6B1018, $11CAEDFA, $3D25BDD8, $E2E1C3C9,
    $44421659, $0A121386, $D90CEC6E, $D5ABEA2A,
    $64AF674E, $DA86A85F, $BEBFE988, $64E4C3FE,

    $9DBC8057, $F0F7C086, $60787BF8, $6003604D,
    $D1FD8346, $F6381FB0, $7745AE04, $D736FCCC,
    $83426B33, $F01EAB71, $B0804187, $3C005E5F,
    $77A057BE, $BDE8AE24, $55464299, $BF582E61,

    $4E58F48F, $F2DDFDA2, $F474EF38, $8789BDC2,
    $5366F9C3, $C8B38E74, $B475F255, $46FCD9B9,
    $7AEB2661, $8B1DDF84, $846A0E79, $915F95E2,
    $466E598E, $20B45770, $8CD55591, $C902DE4C,

    $B90BACE1, $BB8205D0, $11A86248, $7574A99E,
    $B77F19B6, $E0A9DC09, $662D09A1, $C4324633,
    $E85A1F02, $09F0BE8C, $4A99A025, $1D6EFE10,
    $1AB93D1D, $0BA5A4DF, $A186F20F, $2868F169,

    $DCB7DA83, $573906FE, $A1E2CE9B, $4FCD7F52,
    $50115E01, $A70683FA, $A002B5C4, $0DE6D027,
    $9AF88C27, $773F8641, $C3604C06, $61A806B5,
    $F0177A28, $C0F586E0, $006058AA, $30DC7D62,

    $11E69ED7, $2338EA63, $53C2DD94, $C2C21634,
    $BBCBEE56, $90BCB6DE, $EBFC7DA1, $CE591D76,
    $6F05E409, $4B7C0188, $39720A3D, $7C927C24,
    $86E3725F, $724D9DB9, $1AC15BB4, $D39EB8FC,

    $ED545578, $08FCA5B5, $D83D7CD3, $4DAD0FC4,
    $1E50EF5E, $B161E6F8, $A28514D9, $6C51133C,
    $6FD5C7E7, $56E14EC4, $362ABFCE, $DDC6C837,
    $D79A3234, $92638212, $670EFA8E, $406000E0),
    { FOURTH 256 }
    ($3A39CE37, $D3FAF5CF, $ABC27737, $5AC52D1B,
    $5CB0679E, $4FA33742, $D3822740, $99BC9BBE,
    $D5118E9D, $BF0F7315, $D62D1C7E, $C700C47B,
    $B78C1B6B, $21A19045, $B26EB1BE, $6A366EB4,

    $5748AB2F, $BC946E79, $C6A376D2, $6549C2C8,
    $530FF8EE, $468DDE7D, $D5730A1D, $4CD04DC6,
    $2939BBDB, $A9BA4650, $AC9526E8, $BE5EE304,
    $A1FAD5F0, $6A2D519A, $63EF8CE2, $9A86EE22,

    $C089C2B8, $43242EF6, $A51E03AA, $9CF2D0A4,
    $83C061BA, $9BE96A4D, $8FE51550, $BA645BD6,
    $2826A2F9, $A73A3AE1, $4BA99586, $EF5562E9,
    $C72FEFD3, $F752F7DA, $3F046F69, $77FA0A59,

    $80E4A915, $87B08601, $9B09E6AD, $3B3EE593,
    $E990FD5A, $9E34D797, $2CF0B7D9, $022B8B51,
    $96D5AC3A, $017DA67D, $D1CF3ED6, $7C7D2D28,
    $1F9F25CF, $ADF2B89B, $5AD6B472, $5A88F54C,

    $E029AC71, $E019A5E6, $47B0ACFD, $ED93FA9B,
    $E8D3C48D, $283B57CC, $F8D56629, $79132E28,
    $785F0191, $ED756055, $F7960E44, $E3D35E8C,
    $15056DD4, $88F46DBA, $03A16125, $0564F0BD,

    $C3EB9E15, $3C9057A2, $97271AEC, $A93A072A,
    $1B3F6D9B, $1E6321F5, $F59C66FB, $26DCF319,
    $7533D928, $B155FDF5, $03563482, $8ABA3CBB,
    $28517711, $C20AD9F8, $ABCC5167, $CCAD925F,

    $4DE81751, $3830DC8E, $379D5862, $9320F991,
    $EA7A90C2, $FB3E7BCE, $5121CE64, $774FBE32,
    $A8B6E37E, $C3293D46, $48DE5369, $6413E680,
    $A2AE0810, $DD6DB224, $69852DFD, $09072166,

    $B39A460A, $6445C0DD, $586CDECF, $1C20C8AE,
    $5BBEF7DD, $1B588D40, $CCD2017F, $6BB4E3BB,
    $DDA26A7E, $3A59FF45, $3E350A44, $BCB4CDD5,
    $72EACEA8, $FA6484BB, $8D6612AE, $BF3C6F47,

    $D29BE463, $542F5D9E, $AEC2771B, $F64E6370,
    $740E0D8D, $E75B1357, $F8721671, $AF537D5D,
    $4040CB08, $4EB4E2CC, $34D2466A, $0115AF84,
    $E1B00428, $95983A1D, $06B89FB4, $CE6EA048,

    $6F3F3B82, $3520AB82, $011A1D4B, $277227F8,
    $611560B1, $E7933FDC, $BB3A792B, $344525BD,
    $A08839E1, $51CE794B, $2F32C9B7, $A01FBAC9,
    $E01CC87E, $BCC7D1F6, $CF0111C3, $A1E8AAC7,

    $1A908749, $D44FBD9A, $D0DADECB, $D50ADA38,
    $0339C32A, $C6913667, $8DF9317C, $E0B12B4F,
    $F79E59B7, $43F5BB3A, $F2D519FF, $27D9459C,
    $BF97222C, $15E6FC2A, $0F91FC71, $9B941525,

    $FAE59361, $CEB69CEB, $C2A86459, $12BAA8D1,
    $B6C1075E, $E3056A0C, $10D25065, $CB03A442,
    $E0EC6E0E, $1698DB3B, $4C98A0BE, $3278E964,
    $9F1F9532, $E0D392DF, $D3A0342B, $8971F21E,

    $1B0A7441, $4BA3348C, $C5BE7120, $C37632D8,
    $DF359F8D, $9B992F2E, $E60B6F47, $0FE3F11D,
    $E54CDA54, $1EDAD891, $CE6279CF, $CD3E7E6F,
    $1618B166, $FD2C1D05, $848FD2C5, $F6FB2299,

    $F523F357, $A6327623, $93A83531, $56CCCD02,
    $ACF08162, $5A75EBB5, $6E163697, $88D273CC,
    $DE966292, $81B949D0, $4C50901B, $71C65614,
    $E6C6C7BD, $327A140A, $45E1D006, $C3F27B9A,

    $C9AA53FD, $62A80F00, $BB25BFE2, $35BDD2F6,
    $71126905, $B2040222, $B6CBCF7C, $CD769C2B,
    $53113EC0, $1640E3D3, $38ABBD60, $2547ADF0,
    $BA38209C, $F746CE76, $77AFA1C5, $20756060,

    $85CBFE4E, $8AE88DD8, $7AAAF9B0, $4CF9AA7E,
    $1948C25C, $02FB8A8C, $01C36AE4, $D6EBE1F9,
    $90D4F869, $A65CDEA0, $3F09252D, $C208E69F,
    $B74E6132, $CE77E25B, $578FDFE3, $3AC372E6)
    );

  { first 2048 bits of Pi in hexadecimal, low to high, without the leading "3" }
  Pi2048: array [0 .. 255] of Byte = (
    $24, $3F, $6A, $88, $85, $A3, $08, $D3, $13, $19, $8A, $2E, $03, $70, $73, $44,
    $A4, $09, $38, $22, $29, $9F, $31, $D0, $08, $2E, $FA, $98, $EC, $4E, $6C, $89,
    $45, $28, $21, $E6, $38, $D0, $13, $77, $BE, $54, $66, $CF, $34, $E9, $0C, $6C,
    $C0, $AC, $29, $B7, $C9, $7C, $50, $DD, $3F, $84, $D5, $B5, $B5, $47, $09, $17,
    $92, $16, $D5, $D9, $89, $79, $FB, $1B, $D1, $31, $0B, $A6, $98, $DF, $B5, $AC,
    $2F, $FD, $72, $DB, $D0, $1A, $DF, $B7, $B8, $E1, $AF, $ED, $6A, $26, $7E, $96,
    $BA, $7C, $90, $45, $F1, $2C, $7F, $99, $24, $A1, $99, $47, $B3, $91, $6C, $F7,
    $08, $01, $F2, $E2, $85, $8E, $FC, $16, $63, $69, $20, $D8, $71, $57, $4E, $69,
    $A4, $58, $FE, $A3, $F4, $93, $3D, $7E, $0D, $95, $74, $8F, $72, $8E, $B6, $58,
    $71, $8B, $CD, $58, $82, $15, $4A, $EE, $7B, $54, $A4, $1D, $C2, $5A, $59, $B5,
    $9C, $30, $D5, $39, $2A, $F2, $60, $13, $C5, $D1, $B0, $23, $28, $60, $85, $F0,
    $CA, $41, $79, $18, $B8, $DB, $38, $EF, $8E, $79, $DC, $B0, $60, $3A, $18, $0E,
    $6C, $9E, $0E, $8B, $B0, $1E, $8A, $3E, $D7, $15, $77, $C1, $BD, $31, $4B, $27,
    $78, $AF, $2F, $DA, $55, $60, $5C, $60, $E6, $55, $25, $F3, $AA, $55, $AB, $94,
    $57, $48, $98, $62, $63, $E8, $14, $40, $55, $CA, $39, $6A, $2A, $AB, $10, $B6,
    $B4, $CC, $5C, $34, $11, $41, $E8, $CE, $A1, $54, $86, $AF, $7C, $72, $E9, $93);

  BCSalts: array [0 .. 3] of DWORD =
    ($55555555, $AAAAAAAA, $33333333, $CCCCCCCC);

  { SHA-1 constants }
  { 5 magic numbers }
  SHA1_A = DWORD($67452301);
  SHA1_B = DWORD($EFCDAB89);
  SHA1_C = DWORD($98BADCFE);
  SHA1_D = DWORD($10325476);
  SHA1_E = DWORD($C3D2E1F0);
  { four rounds consts }
  SHA1_K1 = DWORD($5A827999);
  SHA1_K2 = DWORD($6ED9EBA1);
  SHA1_K3 = DWORD($8F1BBCDC);
  SHA1_K4 = DWORD($CA62C1D6);
  { Maskes used in byte swap }
  LBMASK_HI = DWORD($FF0000);
  LBMASK_LO = DWORD($FF00);

  INPUTWHITEN = 0;
  RS_GF_FDBK = $14D;
  MDS_GF_FDBK = $169;
  SK_STEP = $02020202;
  SK_BUMP = $01010101;
  SK_ROTL = 9;

  Mars_SBox: array [0 .. 511] of DWORD = (
    $09D0C479, $28C8FFE0, $84AA6C39, $9DAD7287,
    $7DFF9BE3, $D4268361, $C96DA1D4, $7974CC93,
    $85D0582E, $2A4B5705, $1CA16A62, $C3BD279D,
    $0F1F25E5, $5160372F, $C695C1FB, $4D7FF1E4,
    $AE5F6BF4, $0D72EE46, $FF23DE8A, $B1CF8E83,
    $F14902E2, $3E981E42, $8BF53EB6, $7F4BF8AC,
    $83631F83, $25970205, $76AFE784, $3A7931D4,
    $4F846450, $5C64C3F6, $210A5F18, $C6986A26,
    $28F4E826, $3A60A81C, $D340A664, $7EA820C4,
    $526687C5, $7EDDD12B, $32A11D1D, $9C9EF086,
    $80F6E831, $AB6F04AD, $56FB9B53, $8B2E095C,
    $B68556AE, $D2250B0D, $294A7721, $E21FB253,
    $AE136749, $E82AAE86, $93365104, $99404A66,
    $78A784DC, $B69BA84B, $04046793, $23DB5C1E,
    $46CAE1D6, $2FE28134, $5A223942, $1863CD5B,
    $C190C6E3, $07DFB846, $6EB88816, $2D0DCC4A,
    $A4CCAE59, $3798670D, $CBFA9493, $4F481D45,
    $EAFC8CA8, $DB1129D6, $B0449E20, $0F5407FB,
    $6167D9A8, $D1F45763, $4DAA96C3, $3BEC5958,
    $ABABA014, $B6CCD201, $38D6279F, $02682215,
    $8F376CD5, $092C237E, $BFC56593, $32889D2C,
    $854B3E95, $05BB9B43, $7DCD5DCD, $A02E926C,
    $FAE527E5, $36A1C330, $3412E1AE, $F257F462,
    $3C4F1D71, $30A2E809, $68E5F551, $9C61BA44,
    $5DED0AB8, $75CE09C8, $9654F93E, $698C0CCA,
    $243CB3E4, $2B062B97, $0F3B8D9E, $00E050DF,
    $FC5D6166, $E35F9288, $C079550D, $0591AEE8,
    $8E531E74, $75FE3578, $2F6D829A, $F60B21AE,
    $95E8EB8D, $6699486B, $901D7D9B, $FD6D6E31,
    $1090ACEF, $E0670DD8, $DAB2E692, $CD6D4365,
    $E5393514, $3AF345F0, $6241FC4D, $460DA3A3,
    $7BCF3729, $8BF1D1E0, $14AAC070, $1587ED55,
    $3AFD7D3E, $D2F29E01, $29A9D1F6, $EFB10C53,
    $CF3B870F, $B414935C, $664465ED, $024ACAC7,
    $59A744C1, $1D2936A7, $DC580AA6, $CF574CA8,
    $040A7A10, $6CD81807, $8A98BE4C, $ACCEA063,
    $C33E92B5, $D1E0E03D, $B322517E, $2092BD13,
    $386B2C4A, $52E8DD58, $58656DFB, $50820371,
    $41811896, $E337EF7E, $D39FB119, $C97F0DF6,
    $68FEA01B, $A150A6E5, $55258962, $EB6FF41B,
    $D7C9CD7A, $A619CD9E, $BCF09576, $2672C073,
    $F003FB3C, $4AB7A50B, $1484126A, $487BA9B1,
    $A64FC9C6, $F6957D49, $38B06A75, $DD805FCD,
    $63D094CF, $F51C999E, $1AA4D343, $B8495294,
    $CE9F8E99, $BFFCD770, $C7C275CC, $378453A7,
    $7B21BE33, $397F41BD, $4E94D131, $92CC1F98,
    $5915EA51, $99F861B7, $C9980A88, $1D74FD5F,
    $B0A495F8, $614DEED0, $B5778EEA, $5941792D,
    $FA90C1F8, $33F824B4, $C4965372, $3FF6D550,
    $4CA5FEC0, $8630E964, $5B3FBBD6, $7DA26A48,
    $B203231A, $04297514, $2D639306, $2EB13149,
    $16A45272, $532459A0, $8E5F4872, $F966C7D9,
    $07128DC0, $0D44DB62, $AFC8D52D, $06316131,
    $D838E7CE, $1BC41D00, $3A2E8C0F, $EA83837E,
    $B984737D, $13BA4891, $C4F8B949, $A6D6ACB3,
    $A215CDCE, $8359838B, $6BD1AA31, $F579DD52,
    $21B93F93, $F5176781, $187DFDDE, $E94AEB76,
    $2B38FD54, $431DE1DA, $AB394825, $9AD3048F,
    $DFEA32AA, $659473E3, $623F7863, $F3346C59,
    $AB3AB685, $3346A90B, $6B56443E, $C6DE01F8,
    $8D421FC0, $9B0ED10C, $88F1A1E9, $54C1F029,
    $7DEAD57B, $8D7BA426, $4CF5178A, $551A7CCA,
    $1A9A5F08, $FCD651B9, $25605182, $E11FC6C3,
    $B6FD9676, $337B3027, $B7C8EB14, $9E5FD030,
    $6B57E354, $AD913CF7, $7E16688D, $58872A69,
    $2C2FC7DF, $E389CCC6, $30738DF1, $0824A734,
    $E1797A8B, $A4A8D57B, $5B5D193B, $C8A8309B,
    $73F9A978, $73398D32, $0F59573E, $E9DF2B03,
    $E8A5B6C8, $848D0704, $98DF93C2, $720A1DC3,
    $684F259A, $943BA848, $A6370152, $863B5EA3,
    $D17B978B, $6D9B58EF, $0A700DD4, $A73D36BF,
    $8E6A0829, $8695BC14, $E35B3447, $933AC568,
    $8894B022, $2F511C27, $DDFBCC3C, $006662B6,
    $117C83FE, $4E12B414, $C2BCA766, $3A2FEC10,
    $F4562420, $55792E2A, $46F5D857, $CEDA25CE,
    $C3601D3B, $6C00AB46, $EFAC9C28, $B3C35047,
    $611DFEE3, $257C3207, $FDD58482, $3B14D84F,
    $23BECB64, $A075F3A3, $088F8EAD, $07ADF158,
    $7796943C, $FACABF3D, $C09730CD, $F7679969,
    $DA44E9ED, $2C854C12, $35935FA3, $2F057D9F,
    $690624F8, $1CB0BAFD, $7B0DBDC6, $810F23BB,
    $FA929A1A, $6D969A17, $6742979B, $74AC7D05,
    $010E65C4, $86A3D963, $F907B5A0, $D0042BD3,
    $158D7D03, $287A8255, $BBA8366F, $096EDC33,
    $21916A7B, $77B56B86, $951622F9, $A6C5E650,
    $8CEA17D1, $CD8C62BC, $A3D63433, $358A68FD,
    $0F9B9D3C, $D6AA295B, $FE33384A, $C000738E,
    $CD67EB2F, $E2EB6DC2, $97338B02, $06C9F246,
    $419CF1AD, $2B83C045, $3723F18A, $CB5B3089,
    $160BEAD7, $5D494656, $35F8A74B, $1E4E6C9E,
    $000399BD, $67466880, $B4174831, $ACF423B2,
    $CA815AB3, $5A6395E7, $302A67C5, $8BDB446B,
    $108F8FA4, $10223EDA, $92B8B48B, $7F38D0EE,
    $AB2701D4, $0262D415, $AF224A30, $B3D88ABA,
    $F8B2C3AF, $DAF7EF70, $CC97D3B7, $E9614B6C,
    $2BAEBFF4, $70F687CF, $386C9156, $CE092EE5,
    $01E87DA6, $6CE91E6A, $BB7BCC84, $C7922C20,
    $9D3B71FD, $060E41C6, $D7590F15, $4E03BB47,
    $183C198E, $63EEB240, $2DDBF49A, $6D5CBA54,
    $923750AF, $F9E14236, $7838162B, $59726C72,
    $81B66760, $BB2926C1, $48A0CE0D, $A6C0496D,
    $AD43507B, $718D496A, $9DF057AF, $44B1BDE6,
    $054356DC, $DE7CED35, $D51A138B, $62088CC9,
    $35830311, $C96EFCA2, $686F86EC, $8E77CB68,
    $63E1D6B8, $C80F9778, $79C491FD, $1B4C67F2,
    $72698D7D, $5E368C31, $F7D95E2E, $A1D3493F,
    $DCD9433E, $896F1552, $4BC4CA7A, $A6D1BAF4,
    $A5A96DCC, $0BEF8B46, $A169FDA7, $74DF40B7,
    $4E208804, $9A756607, $038E87C8, $20211E44,
    $8B7AD4BF, $C6403F35, $1848E36D, $80BDB038,
    $1E62891C, $643D2107, $BF04D6F8, $21092C8C,
    $F644F389, $0778404E, $7B78ADB8, $A2C52D53,
    $42157ABE, $A2253E2E, $7BF3F4AE, $80F594F9,
    $953194E7, $77EB92ED, $B3816930, $DA8D9336,
    $BF447469, $F26D9483, $EE6FAED5, $71371235,
    $DE425F73, $B4E59F43, $7DBE2D4E, $2D37B185,
    $49DC9A63, $98C39D98, $1301C9A2, $389B1BBF,
    $0C18588D, $A421C1BA, $7AA3865C, $71E08558,
    $3C5CFCAA, $7D239CA4, $0297D9DD, $D7DC2830,
    $4B37802B, $7428AB54, $AEEE0347, $4B3FBB85,
    $692F2F08, $134E578E, $36D9E0BF, $AE8B5FCF,
    $EDB93ECF, $2B27248E, $170EB1EF, $7DC57FD6,
    $1E760F16, $B1136601, $864E1B9B, $D7EA7319,
    $3AB871BD, $CFA4D76F, $E31BD782, $0DBEB469,
    $ABB96061, $5370F85D, $FFB07E37, $DA30D0FB,
    $EBC977B6, $0B98B40F, $3A4D0FE6, $DF4FC26B,
    $159CF22A, $C298D6E2, $2B78EF6A, $61A94AC0,
    $AB561187, $14EEA0F0, $DF0D4164, $19AF70EE);

type
  TBlock2048 = array [0 .. 255] of Byte;

  TBCHalfBlock = array [0 .. 1] of Integer;

  TBFBlockEx = packed record
    Xl: array [0 .. 3] of Byte;
    Xr: array [0 .. 3] of Byte;
  end;

class function TCipher.AllCipher: TCipherSecurityArray;
var
  cs: TCipherSecurity;
begin
  SetLength(Result, Integer(high(TCipherSecurity)) + 1);
  for cs := low(TCipherSecurity) to high(TCipherSecurity) do
      Result[Integer(cs)] := cs;
end;

class function TCipher.NameToHashSecurity(n: SystemString; var hash: THashSecurity): Boolean;
var
  h: THashSecurity;
begin
  Result := False;
  for h := low(THashSecurity) to high(THashSecurity) do
    if SameText(CHashName[h], n) then
      begin
        hash := h;
        Result := True;
        Exit;
      end;
end;

class function TCipher.BuffToString(buff: Pointer; Size: NativeInt): TPascalString;
begin
  HashToString(buff, Size, Result);
end;

class function TCipher.StringToBuff(const Hex: TPascalString; var Buf; BufSize: Cardinal): Boolean;
begin
  Result := HexToBuffer(Hex, Buf, BufSize);
end;

class procedure TCipher.HashToString(hash: Pointer; Size: NativeInt; var output: TPascalString);
const
  HexArr: array [0 .. 15] of U_Char = ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f');
var
  i: Integer;
begin
  output.Len := Size * 2;
  for i := 0 to Size - 1 do
    begin
      output.buff[i * 2] := HexArr[(PByte(nativeUInt(hash) + i)^ shr 4) and $0F];
      output.buff[i * 2 + 1] := HexArr[PByte(nativeUInt(hash) + i)^ and $0F];
    end;
end;

class procedure TCipher.HashToString(hash: TSHA3_224_Digest; var output: TPascalString);
begin
  HashToString(@hash[0], SizeOf(hash), output);
end;

class procedure TCipher.HashToString(hash: TSHA3_256_Digest; var output: TPascalString);
begin
  HashToString(@hash[0], SizeOf(hash), output);
end;

class procedure TCipher.HashToString(hash: TSHA3_384_Digest; var output: TPascalString);
begin
  HashToString(@hash[0], SizeOf(hash), output);
end;

class procedure TCipher.HashToString(hash: TSHA3_512_Digest; var output: TPascalString);
begin
  HashToString(@hash[0], SizeOf(hash), output);
end;

class procedure TCipher.HashToString(hash: TSHA512Digest; var output: TPascalString);
begin
  HashToString(@hash[0], SizeOf(hash), output);
end;

class procedure TCipher.HashToString(hash: TSHA256Digest; var output: TPascalString);
begin
  HashToString(@hash[0], SizeOf(hash), output);
end;

class procedure TCipher.HashToString(hash: TSHA1Digest; var output: TPascalString);
begin
  HashToString(@hash[0], SizeOf(hash), output);
end;

class procedure TCipher.HashToString(hash: TMD5Digest; var output: TPascalString);
begin
  HashToString(@hash[0], SizeOf(hash), output);
end;

class procedure TCipher.HashToString(hash: TBytes; var output: TPascalString);
begin
  HashToString(@hash[0], length(hash), output);
end;

class procedure TCipher.HashToString(hash: TBytes; var output: SystemString);
var
  s: TPascalString;
begin
  if length(hash) > 0 then
    begin
      HashToString(@hash[0], length(hash), s);
      output := s.Text;
    end
  else
      output := '';
end;

class function TCipher.CompareHash(h1, h2: TSHA3_224_Digest): Boolean;
begin
  Result := CompareMemory(@h1[0], @h2[0], SizeOf(h1));
end;

class function TCipher.CompareHash(h1, h2: TSHA3_256_Digest): Boolean;
begin
  Result := CompareMemory(@h1[0], @h2[0], SizeOf(h1));
end;

class function TCipher.CompareHash(h1, h2: TSHA3_384_Digest): Boolean;
begin
  Result := CompareMemory(@h1[0], @h2[0], SizeOf(h1));
end;

class function TCipher.CompareHash(h1, h2: TSHA3_512_Digest): Boolean;
begin
  Result := CompareMemory(@h1[0], @h2[0], SizeOf(h1));
end;

class function TCipher.CompareHash(h1, h2: TSHA512Digest): Boolean;
begin
  Result := CompareMemory(@h1[0], @h2[0], SizeOf(h1));
end;

class function TCipher.CompareHash(h1, h2: TSHA256Digest): Boolean;
begin
  Result := CompareMemory(@h1[0], @h2[0], SizeOf(h1));
end;

class function TCipher.CompareHash(h1, h2: TSHA1Digest): Boolean;
begin
  Result := CompareMemory(@h1[0], @h2[0], SizeOf(h1));
end;

class function TCipher.CompareHash(h1, h2: TMD5Digest): Boolean;
begin
  Result := CompareMemory(@h1[0], @h2[0], SizeOf(h1));
end;

class function TCipher.CompareHash(h1, h2: Pointer; Size: NativeInt): Boolean;
begin
  Result := CompareMemory(h1, h2, Size);
end;

class function TCipher.CompareHash(h1, h2: TBytes): Boolean;
begin
  if length(h1) = 0 then
      Result := length(h2) = 0
  else
      Result := (length(h1) = length(h2)) and (CompareMemory(@h1[0], @h2[0], length(h1)));
end;

class function TCipher.CompareKey(k1, k2: TCipherKeyBuffer): Boolean;
begin
  if length(k1) = 0 then
      Result := length(k2) = 0
  else
      Result := (length(k1) = length(k2)) and (CompareMemory(@k1[0], @k2[0], length(k1)));
end;

class function TCipher.GenerateSHA3_224Hash(sour: Pointer; Size: NativeInt): TSHA3_224_Digest;
begin
  TSHA3.SHA224(Result, sour, Size);
end;

class function TCipher.GenerateSHA3_256Hash(sour: Pointer; Size: NativeInt): TSHA3_256_Digest;
begin
  TSHA3.SHA256(Result, sour, Size);
end;

class function TCipher.GenerateSHA3_384Hash(sour: Pointer; Size: NativeInt): TSHA3_384_Digest;
begin
  TSHA3.SHA384(Result, sour, Size);
end;

class function TCipher.GenerateSHA3_512Hash(sour: Pointer; Size: NativeInt): TSHA3_512_Digest;
begin
  TSHA3.SHA512(Result, sour, Size);
end;

class function TCipher.GenerateSHA512Hash(sour: Pointer; Size: NativeInt): TSHA512Digest;
begin
  TSHA512.SHA512(Result, sour^, Size);
end;

class function TCipher.GenerateSHA256Hash(sour: Pointer; Size: NativeInt): TSHA256Digest;
begin
  TSHA256.SHA256(Result, sour^, Size);
end;

class function TCipher.GenerateSHA1Hash(sour: Pointer; Size: NativeInt): TSHA1Digest;
begin
  TSHA1.SHA1(Result, sour^, Size);
end;

class function TCipher.GenerateMD5Hash(sour: Pointer; Size: NativeInt): TMD5Digest;
begin
{$IF Defined(FastMD5) and (Defined(WIN32) or Defined(WIN64))}
  Result := FastMD5(sour, Size);
{$ELSE}
  THashMD5.HashMD5(Result, sour^, Size);
{$IFEND}
end;

class procedure TCipher.GenerateMDHash(sour: Pointer; Size: NativeInt; OutHash: Pointer; HashSize: NativeInt);
begin
  THashMD.HashLMD(OutHash^, HashSize, sour^, Size);
end;

class procedure TCipher.GenerateHashByte(hs: THashSecurity; sour: Pointer; Size: NativeInt; var output: TBytes);
var
  swBuff: TBytes;
begin
  if Size <= 0 then
    begin
      SetLength(output, 0);
      Exit;
    end;

  if Size < 6 then
    begin
      SetLength(output, 16);
      PMD5(@output[0])^ := umlMD5(PByte(sour), Size);
      Exit;
    end;

  case hs of
    hsNone:
      begin
        SetLength(output, 0);
      end;
    hsFastMD5:
      begin
        SetLength(output, 16);
        PMD5(@output[0])^ := umlMD5(PByte(sour), DWORD(Size));
      end;
    hsMD5:
      begin
        SetLength(output, 16);
        THashMD5.HashMD5(PMD5Digest(@output[0])^, sour^, Size);
      end;
    hsSHA1:
      begin
        SetLength(output, 20);
        TSHA1.SHA1(PSHA1Digest(@output[0])^, sour^, Size);
      end;
    hsSHA256:
      begin
        SetLength(output, 32);
        TSHA256.SHA256(PSHA256Digest(@output[0])^, sour^, Size);
      end;
    hsSHA512:
      begin
        SetLength(output, 64);
        TSHA512.SHA512(PSHA512Digest(@output[0])^, sour^, Size);
      end;
    hsSHA3_224:
      begin
        SetLength(output, SizeOf(TSHA3_224_Digest));
        TSHA3.SHA224(PSHA3_224_Digest(@output[0])^, sour, Size);
      end;
    hsSHA3_256:
      begin
        SetLength(output, SizeOf(TSHA3_256_Digest));
        TSHA3.SHA256(PSHA3_256_Digest(@output[0])^, sour, Size);
      end;
    hsSHA3_384:
      begin
        SetLength(output, SizeOf(TSHA3_384_Digest));
        TSHA3.SHA384(PSHA3_384_Digest(@output[0])^, sour, Size);
      end;
    hsSHA3_512:
      begin
        SetLength(output, SizeOf(TSHA3_512_Digest));
        TSHA3.SHA512(PSHA3_512_Digest(@output[0])^, sour, Size);
      end;
    hs256:
      begin
        SetLength(output, 256);
        THashMD.HashLMD((@output[0])^, 256, sour^, Size);
      end;
    hs128:
      begin
        SetLength(output, 128);
        THashMD.HashLMD((@output[0])^, 128, sour^, Size);
      end;
    hs64:
      begin
        SetLength(output, 64);
        THashMD.HashLMD((@output[0])^, 64, sour^, Size);
      end;
    hs32:
      begin
        SetLength(output, 32);
        THashMD.HashLMD((@output[0])^, 32, sour^, Size);
      end;
    hs16:
      begin
        SetLength(output, 16);
        THashMD.HashLMD((@output[0])^, 16, sour^, Size);
      end;
    hsELF:
      begin
        SetLength(output, 4);
        TMISC.HashELF(PDWORD(@output[0])^, sour^, Size);
      end;
    hsELF64:
      begin
        SetLength(output, 8);
        TMISC.HashELF64(PInt64(@output[0])^, sour^, Size);
      end;
    hsMix128:
      begin
        SetLength(output, 4);

        if Size < 16 then
          begin
            SetLength(swBuff, 16);
            THashMD.HashLMD(swBuff[0], 16, sour^, Size);
            TMISC.HashMix128(PDWORD(@output[0])^, swBuff[0], 16);
          end
        else
            TMISC.HashMix128(PDWORD(@output[0])^, sour^, Size);
      end;
    hsCRC16:
      begin
        SetLength(output, 2);
        PWORD(@output[0])^ := umlCRC16(PByte(sour), Size);
      end;
    hsCRC32:
      begin
        SetLength(output, 4);
        PCardinal(@output[0])^ := umlCRC32(PByte(sour), Size);
      end;
  end;
end;

class function TCipher.GenerateHashString(hs: THashSecurity; sour: Pointer; Size: NativeInt): TPascalString;
var
  h: TBytes;
begin
  GenerateHashByte(hs, sour, Size, h);
  HashToString(h, Result);
  SetLength(h, 0);
end;

class function TCipher.BufferToHex(const Buf; BufSize: Cardinal): TPascalString;
begin
  Result := BuffToString(@Buf, BufSize);
end;

class function TCipher.HexToBuffer(const Hex: TPascalString; var Buf; BufSize: Cardinal): Boolean;
var
  i, c: Integer;
  filStr: TPascalString;
  Count: Integer;
  cChar: Char;
begin
  Result := False;
  filStr.Text := '';
  for cChar in Hex.buff do
    if CharIn(cChar, [c0to9, cLoAtoF, cHiAtoF]) then
        filStr.Append(cChar);

  FillPtrByte(@Buf, BufSize, 0);
  Count := Min(filStr.Len div 2, BufSize);

  for i := 0 to Count - 1 do
    begin
      val('$' + filStr[i * 2 + 1] + filStr[i * 2 + 2], TCCByteArray(Buf)[i], c);
      if c <> 0 then
          Exit;
    end;

  Result := True;
end;

class function TCipher.CopyKey(const k: TCipherKeyBuffer): TCipherKeyBuffer;
begin
  SetLength(Result, length(k));
  CopyPtr(@k[0], @Result[0], length(k));
end;

class procedure TCipher.GenerateNoneKey(var output: TCipherKeyBuffer);
begin
  SetLength(output, C_Byte_Size);
  output[0] := Byte(TCipherKeyStyle.cksNone);
end;

class procedure TCipher.GenerateKey64(const s: TPascalString; var output: TCipherKeyBuffer);
begin
  SetLength(output, C_Byte_Size + cKey64Size);
  output[0] := Byte(TCipherKeyStyle.cksKey64);
  THashMD.GenerateLMDKey((@output[1])^, cKey64Size, s.Bytes);
end;

class procedure TCipher.GenerateKey64(sour: Pointer; Size: NativeInt; var output: TCipherKeyBuffer);
begin
  SetLength(output, C_Byte_Size + cKey64Size);
  output[0] := Byte(TCipherKeyStyle.cksKey64);
  THashMD.HashLMD((@output[1])^, cKey64Size, sour^, Size);
end;

class procedure TCipher.GenerateKey128(const s: TPascalString; var output: TCipherKeyBuffer);
begin
  SetLength(output, C_Byte_Size + cKey128Size);
  output[0] := Byte(TCipherKeyStyle.cksKey128);
  THashMD.GenerateLMDKey((@output[1])^, cKey128Size, s.Bytes);
end;

class procedure TCipher.GenerateKey128(sour: Pointer; Size: NativeInt; var output: TCipherKeyBuffer);
begin
  SetLength(output, C_Byte_Size + cKey128Size);
  output[0] := Byte(TCipherKeyStyle.cksKey128);
  THashMD.HashLMD((@output[1])^, cKey128Size, sour^, Size);
end;

class procedure TCipher.GenerateKey256(const s: TPascalString; var output: TCipherKeyBuffer);
begin
  SetLength(output, C_Byte_Size + cKey256Size);
  output[0] := Byte(TCipherKeyStyle.cksKey256);
  THashMD.GenerateLMDKey((@output[1])^, cKey256Size, s.Bytes);
end;

class procedure TCipher.GenerateKey256(sour: Pointer; Size: NativeInt; var output: TCipherKeyBuffer);
begin
  SetLength(output, C_Byte_Size + cKey256Size);
  output[0] := Byte(TCipherKeyStyle.cksKey256);
  THashMD.HashLMD((@output[1])^, cKey256Size, sour^, Size);
end;

class procedure TCipher.Generate3Key64(const s: TPascalString; var output: TCipherKeyBuffer);
begin
  SetLength(output, C_Byte_Size + cKey192Size);
  output[0] := Byte(TCipherKeyStyle.cks3Key64);
  THashMD.GenerateLMDKey((@output[1])^, cKey192Size, s.Bytes);
end;

class procedure TCipher.Generate3Key64(sour: Pointer; Size: NativeInt; var output: TCipherKeyBuffer);
begin
  SetLength(output, C_Byte_Size + cKey192Size);
  output[0] := Byte(TCipherKeyStyle.cks3Key64);
  THashMD.HashLMD((@output[1])^, cKey192Size, sour^, Size);
end;

class procedure TCipher.Generate2IntKey(const s: TPascalString; var output: TCipherKeyBuffer);
begin
  SetLength(output, C_Byte_Size + cKey2DWORDSize);
  output[0] := Byte(TCipherKeyStyle.cks2IntKey);
  THashMD.GenerateLMDKey((@output[1])^, cKey2DWORDSize, s.Bytes);
end;

class procedure TCipher.Generate2IntKey(sour: Pointer; Size: NativeInt; var output: TCipherKeyBuffer);
begin
  SetLength(output, C_Byte_Size + cKey2DWORDSize);
  output[0] := Byte(TCipherKeyStyle.cks2IntKey);
  THashMD.HashLMD((@output[1])^, cKey2DWORDSize, sour^, Size);
end;

class procedure TCipher.GenerateIntKey(const s: TPascalString; var output: TCipherKeyBuffer);
begin
  SetLength(output, C_Byte_Size + cKeyDWORDSize);
  output[0] := Byte(TCipherKeyStyle.cksIntKey);
  THashMD.GenerateLMDKey((@output[1])^, cKeyDWORDSize, s.Bytes);
end;

class procedure TCipher.GenerateIntKey(sour: Pointer; Size: NativeInt; var output: TCipherKeyBuffer);
begin
  SetLength(output, C_Byte_Size + cKeyDWORDSize);
  output[0] := Byte(TCipherKeyStyle.cksIntKey);
  THashMD.HashLMD((@output[1])^, cKeyDWORDSize, sour^, Size);
end;

class procedure TCipher.GenerateBytesKey(const s: TPascalString; KeySize: DWORD; var output: TCipherKeyBuffer);
begin
  SetLength(output, C_Byte_Size + cIntSize + KeySize);
  output[0] := Byte(TCipherKeyStyle.ckyDynamicKey);
  PDWORD(@output[1])^ := KeySize;
  THashMD.GenerateLMDKey((@output[1 + cIntSize])^, KeySize, s.Bytes);
end;

class procedure TCipher.GenerateBytesKey(sour: Pointer; Size, KeySize: DWORD; var output: TCipherKeyBuffer);
begin
  SetLength(output, C_Byte_Size + cIntSize + KeySize);
  output[0] := Byte(TCipherKeyStyle.ckyDynamicKey);
  PDWORD(@output[1])^ := KeySize;
  THashMD.HashLMD((@output[1 + cIntSize])^, KeySize, sour^, Size);
end;

class procedure TCipher.GenerateKey64(const k: TKey64; var output: TCipherKeyBuffer);
begin
  GenerateKey(k, output);
end;

class procedure TCipher.GenerateKey128(const k1, k2: TKey64; var output: TCipherKeyBuffer);
begin
  SetLength(output, C_Byte_Size + cKey128Size);
  output[0] := Byte(TCipherKeyStyle.cksKey128);
  PKey64(@output[1])^ := k1;
  PKey64(@output[1 + cKey64Size])^ := k2;
end;

class procedure TCipher.GenerateKey(const k: TKey64; var output: TCipherKeyBuffer);
begin
  SetLength(output, C_Byte_Size + cKey64Size);
  output[0] := Byte(TCipherKeyStyle.cksKey64);
  PKey64(@output[1])^ := k;
end;

class procedure TCipher.GenerateKey(const k1, k2, k3: TKey64; var output: TCipherKeyBuffer);
begin
  SetLength(output, C_Byte_Size + cKey192Size);
  output[0] := Byte(TCipherKeyStyle.cks3Key64);
  PKey64(@output[1])^ := k1;
  PKey64(@output[1 + cKey64Size])^ := k2;
  PKey64(@output[1 + cKey64Size + cKey64Size])^ := k3;
end;

class procedure TCipher.GenerateKey(const k: TKey128; var output: TCipherKeyBuffer);
begin
  SetLength(output, C_Byte_Size + cKey128Size);
  output[0] := Byte(TCipherKeyStyle.cksKey128);
  PKey128(@output[1])^ := k;
end;

class procedure TCipher.GenerateKey(const k: TKey256; var output: TCipherKeyBuffer);
begin
  SetLength(output, C_Byte_Size + cKey256Size);
  output[0] := Byte(TCipherKeyStyle.cksKey256);
  PKey256(@output[1])^ := k;
end;

class procedure TCipher.GenerateKey(const k1, k2: DWORD; var output: TCipherKeyBuffer);
begin
  SetLength(output, C_Byte_Size + cKey2DWORDSize);
  output[0] := Byte(TCipherKeyStyle.cks2IntKey);
  PInteger(@output[1])^ := k1;
  PInteger(@output[1 + cKeyDWORDSize])^ := k2;
end;

class procedure TCipher.GenerateKey(const k: DWORD; var output: TCipherKeyBuffer);
begin
  SetLength(output, C_Byte_Size + cKeyDWORDSize);
  output[0] := Byte(TCipherKeyStyle.cksIntKey);
  PInteger(@output[1])^ := k;
end;

class procedure TCipher.GenerateKey(const key: PByte; Size: DWORD; var output: TCipherKeyBuffer);
begin
  SetLength(output, C_Byte_Size + cIntSize + Size);
  output[0] := Byte(TCipherKeyStyle.ckyDynamicKey);
  PInteger(@output[1])^ := Size;
  CopyPtr(key, @output[1 + cIntSize], Size);
end;

class procedure TCipher.GenerateKey(cs: TCipherSecurity; buffPtr: Pointer; Size: NativeInt; var output: TCipherKeyBuffer);
begin
  case cCipherKeyStyle[cs] of
    cksNone: GenerateNoneKey(output);
    cksKey64: GenerateKey64(buffPtr, Size, output);
    cks3Key64: Generate3Key64(buffPtr, Size, output);
    cksKey128: GenerateKey128(buffPtr, Size, output);
    cksKey256: GenerateKey256(buffPtr, Size, output);
    cks2IntKey: Generate2IntKey(buffPtr, Size, output);
    cksIntKey: GenerateIntKey(buffPtr, Size, output);
    ckyDynamicKey: GenerateBytesKey(buffPtr, Size, 32, output);
  end;
end;

class procedure TCipher.GenerateKey(cs: TCipherSecurity; s: TPascalString; var output: TCipherKeyBuffer);
var
  buff: TBytes;
  key: TBytes;
begin
  buff := s.Bytes;
  SetLength(key, 128);
  TCipher.GenerateMDHash(@buff[0], SizeOf(buff), @key[0], 128);
  GenerateKey(cs, @key[0], 128, output);
end;

class function TCipher.GetKeyStyle(const p: PCipherKeyBuffer): TCipherKeyStyle;
begin
  Result := TCipherKeyStyle(p^[0]);
end;

class function TCipher.GetKey(const KeyBuffPtr: PCipherKeyBuffer; var k: TKey64): Boolean;
begin
  Result := GetKeyStyle(KeyBuffPtr) = TCipherKeyStyle.cksKey64;
  if not Result then
      Exit;
  k := PKey64(@KeyBuffPtr^[1])^
end;

class function TCipher.GetKey(const KeyBuffPtr: PCipherKeyBuffer; var k1, k2, k3: TKey64): Boolean;
begin
  Result := GetKeyStyle(KeyBuffPtr) = TCipherKeyStyle.cks3Key64;
  if not Result then
      Exit;
  k1 := PKey64(@KeyBuffPtr^[1])^;
  k2 := PKey64(@KeyBuffPtr^[1 + cKey64Size])^;
  k3 := PKey64(@KeyBuffPtr^[1 + cKey64Size + cKey64Size])^;
end;

class function TCipher.GetKey(const KeyBuffPtr: PCipherKeyBuffer; var k: TKey128): Boolean;
begin
  Result := GetKeyStyle(KeyBuffPtr) = TCipherKeyStyle.cksKey128;
  if not Result then
      Exit;
  k := PKey128(@KeyBuffPtr^[1])^
end;

class function TCipher.GetKey(const KeyBuffPtr: PCipherKeyBuffer; var k: TKey256): Boolean;
begin
  Result := GetKeyStyle(KeyBuffPtr) = TCipherKeyStyle.cksKey256;
  if not Result then
      Exit;
  k := PKey256(@KeyBuffPtr^[1])^
end;

class function TCipher.GetKey(const KeyBuffPtr: PCipherKeyBuffer; var k1, k2: DWORD): Boolean;
begin
  Result := GetKeyStyle(KeyBuffPtr) = TCipherKeyStyle.cks2IntKey;
  if not Result then
      Exit;
  k1 := PDWORD(@KeyBuffPtr^[1])^;
  k2 := PDWORD(@KeyBuffPtr^[1 + cKeyDWORDSize])^;
end;

class function TCipher.GetKey(const KeyBuffPtr: PCipherKeyBuffer; var k: DWORD): Boolean;
begin
  Result := GetKeyStyle(KeyBuffPtr) = TCipherKeyStyle.cksIntKey;
  if not Result then
      Exit;
  k := PDWORD(@KeyBuffPtr^[1])^;
end;

class function TCipher.GetKey(const KeyBuffPtr: PCipherKeyBuffer; var key: TBytes): Boolean;
var
  siz: Integer;
begin
  Result := GetKeyStyle(KeyBuffPtr) = TCipherKeyStyle.ckyDynamicKey;
  if not Result then
      Exit;
  siz := PInteger(@KeyBuffPtr^[1])^;
  SetLength(key, siz);
  CopyPtr(@KeyBuffPtr^[1 + cIntSize], @key[0], siz);
end;

class procedure TCipher.EncryptTail(TailPtr: Pointer; TailSize: NativeInt);
begin
  BlockCBC(TailPtr, TailSize, @SystemCBC[0], length(SystemCBC));
end;

class function TCipher.DES64(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
var
  k: TKey64;
  d: TDESContext;
  p: nativeUInt;
begin
  Result := False;
  if Size <= 0 then
      Exit;

  if Size >= 8 then
    begin
      if not GetKey(KeyBuff, k) then
          Exit;
      TDES.InitEncryptDES(k, d, Encrypt);

      p := 0;
      repeat
        TDES.EncryptDES(d, PDESBlock(nativeUInt(sour) + p)^);
        p := p + 8;
      until p + 8 > Size;
    end
  else
      p := 0;

  if (ProcessTail) and (Size - p > 0) then
      EncryptTail(Pointer(nativeUInt(sour) + p), Size - p);

  Result := True;
end;

class function TCipher.DES128(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
var
  k: TKey128;
  d: TTripleDESContext;
  p: nativeUInt;
begin
  Result := False;
  if Size <= 0 then
      Exit;

  if Size >= 8 then
    begin
      if not GetKey(KeyBuff, k) then
          Exit;
      TDES.InitEncryptTripleDES(k, d, Encrypt);

      p := 0;
      repeat
        TDES.EncryptTripleDES(d, PDESBlock(nativeUInt(sour) + p)^);
        p := p + 8;
      until p + 8 > Size;
    end
  else
      p := 0;

  if (ProcessTail) and (Size - p > 0) then
      EncryptTail(Pointer(nativeUInt(sour) + p), Size - p);

  Result := True;
end;

class function TCipher.DES192(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
var
  k1, k2, k3: TKey64;
  d: TTripleDESContext3Key;
  p: nativeUInt;
begin
  Result := False;
  if Size <= 0 then
      Exit;

  if Size >= 8 then
    begin
      if not GetKey(KeyBuff, k1, k2, k3) then
          Exit;
      TDES.InitEncryptTripleDES3Key(k1, k2, k3, d, Encrypt);

      p := 0;
      repeat
        TDES.EncryptTripleDES3Key(d, PDESBlock(nativeUInt(sour) + p)^);
        p := p + 8;
      until p + 8 > Size;
    end
  else
      p := 0;

  if (ProcessTail) and (Size - p > 0) then
      EncryptTail(Pointer(nativeUInt(sour) + p), Size - p);
  Result := True;
end;

class function TCipher.Blowfish(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
var
  k: TKey128;
  d: TBFContext;
  p: nativeUInt;
begin
  Result := False;
  if Size <= 0 then
      Exit;

  if Size >= 8 then
    begin
      if not GetKey(KeyBuff, k) then
          Exit;
      TBlowfish.InitEncryptBF(k, d);

      p := 0;
      repeat
        TBlowfish.EncryptBF(d, PBFBlock(nativeUInt(sour) + p)^, Encrypt);
        p := p + 8;
      until p + 8 > Size;
    end
  else
      p := 0;

  if (ProcessTail) and (Size - p > 0) then
      EncryptTail(Pointer(nativeUInt(sour) + p), Size - p);

  Result := True;
end;

class function TCipher.LBC(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
var
  k: TKey128;
  d: TLBCContext;
  p: nativeUInt;
begin
  Result := False;
  if Size <= 0 then
      Exit;

  if Size >= 16 then
    begin
      if not GetKey(KeyBuff, k) then
          Exit;
      TLBC.InitEncryptLBC(k, d, 16, Encrypt);

      p := 0;
      repeat
        TLBC.EncryptLBC(d, PLBCBlock(nativeUInt(sour) + p)^);
        p := p + 16;
      until p + 16 > Size;
    end
  else
      p := 0;

  if (ProcessTail) and (Size - p > 0) then
      EncryptTail(Pointer(nativeUInt(sour) + p), Size - p);
  Result := True;
end;

class function TCipher.LQC(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
var
  k: TKey128;
  p: nativeUInt;
begin
  Result := False;
  if Size <= 0 then
      Exit;

  if Size >= 8 then
    begin
      if not GetKey(KeyBuff, k) then
          Exit;

      p := 0;
      repeat
        TLBC.EncryptLQC(k, PLQCBlock(nativeUInt(sour) + p)^, Encrypt);
        p := p + 8;
      until p + 8 > Size;
    end
  else
      p := 0;

  if (ProcessTail) and (Size - p > 0) then
      EncryptTail(Pointer(nativeUInt(sour) + p), Size - p);
  Result := True;
end;

class function TCipher.RNG32(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer): Boolean;
var
  k: DWORD;
  d: TRNG32Context;
begin
  Result := False;
  if Size <= 0 then
      Exit;
  if not GetKey(KeyBuff, k) then
      Exit;

  TRNG.InitEncryptRNG32(k, d);

  TRNG.EncryptRNG32(d, sour^, Size);

  Result := True;
end;

class function TCipher.RNG64(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer): Boolean;
var
  k1, k2: DWORD;
  d: TRNG64Context;
begin
  Result := False;
  if Size <= 0 then
      Exit;
  if not GetKey(KeyBuff, k1, k2) then
      Exit;

  TRNG.InitEncryptRNG64(k1, k2, d);

  TRNG.EncryptRNG64(d, sour^, Size);

  Result := True;
end;

class function TCipher.LSC(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer): Boolean;
var
  k: TBytes;
  k255: TBytes;
  d: TLSCContext;
begin
  Result := False;
  if Size <= 0 then
      Exit;
  if not GetKey(KeyBuff, k) then
      Exit;

  if length(k) > 255 then
    begin
      SetLength(k255, 255);
      THashMD.GenerateLMDKey((@k255[0])^, 255, k);
    end
  else
      k255 := k;

  TLSC.InitEncryptLSC((@k255[0])^, length(k255), d);

  TLSC.EncryptLSC(d, sour^, Size);

  Result := True;
end;

class function TCipher.TwoFish(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
var
  k, k256: TBytes;
  d: TTwofishKey;
  p: nativeUInt;
begin
  Result := False;
  if Size <= 0 then
      Exit;
  if Size >= 16 then
    begin
      if not GetKey(KeyBuff, k) then
          Exit;

      SetLength(k256, 32);
      THashMD.GenerateLMDKey((@k256[0])^, 32, k);

      TTwofish.InitKey(@k256[0], 32, d);

      if Encrypt then
        begin
          p := 0;
          repeat
            TTwofish.Encrypt(d, PTwofishBlock(nativeUInt(sour) + p)^);
            p := p + 16;
          until p + 16 > Size;
        end
      else
        begin
          p := 0;
          repeat
            TTwofish.Decrypt(d, PTwofishBlock(nativeUInt(sour) + p)^);
            p := p + 16;
          until p + 16 > Size;
        end;
    end
  else
      p := 0;

  if (ProcessTail) and (Size - p > 0) then
      EncryptTail(Pointer(nativeUInt(sour) + p), Size - p);
  Result := True;
end;

class function TCipher.XXTea512(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
var
  k: TKey128;
  p: nativeUInt;
begin
  Result := False;
  if Size <= 0 then
      Exit;

  if Size >= 64 then
    begin
      if not GetKey(KeyBuff, k) then
          Exit;

      if Encrypt then
        begin
          p := 0;
          repeat
            XXTEAEncrypt(k, PXXTEABlock(nativeUInt(sour) + p)^);
            p := p + 64;
          until p + 64 > Size;
        end
      else
        begin
          p := 0;
          repeat
            XXTEADecrypt(k, PXXTEABlock(nativeUInt(sour) + p)^);
            p := p + 64;
          until p + 64 > Size;
        end;
    end
  else
      p := 0;

  if (ProcessTail) and (Size - p > 0) then
      EncryptTail(Pointer(nativeUInt(sour) + p), Size - p);
  Result := True;
end;

class function TCipher.RC6(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
var
  k, k256: TBytes;
  d: TRC6Key;
  p: nativeUInt;
begin
  Result := False;
  if Size <= 0 then
      Exit;
  if Size >= 16 then
    begin
      if not GetKey(KeyBuff, k) then
          Exit;

      SetLength(k256, 32);
      THashMD.GenerateLMDKey((@k256[0])^, 32, k);

      TRC6.InitKey(@k256[0], 32, d);

      if Encrypt then
        begin
          p := 0;
          repeat
            TRC6.Encrypt(d, PRC6Block(nativeUInt(sour) + p)^);
            p := p + 16;
          until p + 16 > Size;
        end
      else
        begin
          p := 0;
          repeat
            TRC6.Decrypt(d, PRC6Block(nativeUInt(sour) + p)^);
            p := p + 16;
          until p + 16 > Size;
        end;
    end
  else
      p := 0;

  if (ProcessTail) and (Size - p > 0) then
      EncryptTail(Pointer(nativeUInt(sour) + p), Size - p);
  Result := True;
end;

class function TCipher.Serpent(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
var
  k, k256: TBytes;
  d: TSerpentkey;
  p: nativeUInt;
begin
  Result := False;
  if Size <= 0 then
      Exit;
  if Size >= 16 then
    begin
      if not GetKey(KeyBuff, k) then
          Exit;

      SetLength(k256, 32);
      THashMD.GenerateLMDKey((@k256[0])^, 32, k);

      TSerpent.InitKey(@k256[0], 32, d);

      if Encrypt then
        begin
          p := 0;
          repeat
            TSerpent.Encrypt(d, PSerpentBlock(nativeUInt(sour) + p)^);
            p := p + 16;
          until p + 16 > Size;
        end
      else
        begin
          p := 0;
          repeat
            TSerpent.Decrypt(d, PSerpentBlock(nativeUInt(sour) + p)^);
            p := p + 16;
          until p + 16 > Size;
        end;
    end
  else
      p := 0;

  if (ProcessTail) and (Size - p > 0) then
      EncryptTail(Pointer(nativeUInt(sour) + p), Size - p);
  Result := True;
end;

class function TCipher.Mars(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
var
  k, k256: TBytes;
  d: TMarskey;
  p: nativeUInt;
begin
  Result := False;
  if Size <= 0 then
      Exit;
  if Size >= 16 then
    begin
      if not GetKey(KeyBuff, k) then
          Exit;

      SetLength(k256, 32);
      THashMD.GenerateLMDKey((@k256[0])^, 32, k);

      TMars.InitKey(@k256[0], 32, d);

      if Encrypt then
        begin
          p := 0;
          repeat
            TMars.Encrypt(d, PMarsBlock(nativeUInt(sour) + p)^);
            p := p + 16;
          until p + 16 > Size;
        end
      else
        begin
          p := 0;
          repeat
            TMars.Decrypt(d, PMarsBlock(nativeUInt(sour) + p)^);
            p := p + 16;
          until p + 16 > Size;
        end;
    end
  else
      p := 0;

  if (ProcessTail) and (Size - p > 0) then
      EncryptTail(Pointer(nativeUInt(sour) + p), Size - p);
  Result := True;
end;

class function TCipher.Rijndael(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
var
  k, k256: TBytes;
  d: TRijndaelkey;
  p: nativeUInt;
begin
  Result := False;
  if Size <= 0 then
      Exit;
  if Size >= 16 then
    begin
      if not GetKey(KeyBuff, k) then
          Exit;

      SetLength(k256, 32);
      THashMD.GenerateLMDKey((@k256[0])^, 32, k);

      TRijndael.InitKey(@k256[0], 32, d);

      if Encrypt then
        begin
          p := 0;
          repeat
            TRijndael.Encrypt(d, PRijndaelBlock(nativeUInt(sour) + p)^);
            p := p + 16;
          until p + 16 > Size;
        end
      else
        begin
          p := 0;
          repeat
            TRijndael.Decrypt(d, PRijndaelBlock(nativeUInt(sour) + p)^);
            p := p + 16;
          until p + 16 > Size;
        end;
    end
  else
      p := 0;

  if (ProcessTail) and (Size - p > 0) then
      EncryptTail(Pointer(nativeUInt(sour) + p), Size - p);
  Result := True;
end;

class procedure TCipher.BlockCBC(sour: Pointer; Size: NativeInt; boxBuff: Pointer; boxSiz: NativeInt);
var
  p: nativeUInt;
begin
  if Size = 0 then
      Exit;
  if boxSiz >= Size then
    begin
      TMISC.XorMem(sour^, boxBuff^, Size);
    end
  else
    begin
      p := 0;
      repeat
        TMISC.XorMem(Pointer(nativeUInt(sour) + p)^, boxBuff^, boxSiz);
        p := p + boxSiz;
      until p + boxSiz > Size;

      BlockCBC(Pointer(nativeUInt(sour) + p), Size - p, boxBuff, boxSiz);
    end;
end;

class function TCipher.EncryptBuffer(cs: TCipherSecurity; sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
begin
  Result := False;
  case cs of
    csNone: Result := True;
    csDES64: Result := DES64(sour, Size, KeyBuff, Encrypt, ProcessTail);
    csDES128: Result := DES128(sour, Size, KeyBuff, Encrypt, ProcessTail);
    csDES192: Result := DES192(sour, Size, KeyBuff, Encrypt, ProcessTail);
    csBlowfish: Result := Blowfish(sour, Size, KeyBuff, Encrypt, ProcessTail);
    csLBC: Result := LBC(sour, Size, KeyBuff, Encrypt, ProcessTail);
    csLQC: Result := LQC(sour, Size, KeyBuff, Encrypt, ProcessTail);
    csRNG32: Result := RNG32(sour, Size, KeyBuff);
    csRNG64: Result := RNG64(sour, Size, KeyBuff);
    csLSC: Result := LSC(sour, Size, KeyBuff);
    csTwoFish: Result := TwoFish(sour, Size, KeyBuff, Encrypt, ProcessTail);
    csXXTea512: Result := XXTea512(sour, Size, KeyBuff, Encrypt, ProcessTail);
    csRC6: Result := RC6(sour, Size, KeyBuff, Encrypt, ProcessTail);
    csSerpent: Result := Serpent(sour, Size, KeyBuff, Encrypt, ProcessTail);
    csMars: Result := Mars(sour, Size, KeyBuff, Encrypt, ProcessTail);
    csRijndael: Result := Rijndael(sour, Size, KeyBuff, Encrypt, ProcessTail);
  end;
end;

class function TCipher.EncryptBufferCBC(cs: TCipherSecurity; sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
begin
  if cs = TCipherSecurity.csNone then
    begin
      Result := True;
      Exit;
    end;

  if Encrypt then
    begin
      Result := EncryptBuffer(cs, sour, Size, KeyBuff, Encrypt, ProcessTail);
      if Result then
          BlockCBC(sour, Size, @SystemCBC[0], length(SystemCBC));
    end
  else
    begin
      BlockCBC(sour, Size, @SystemCBC[0], length(SystemCBC));
      Result := EncryptBuffer(cs, sour, Size, KeyBuff, Encrypt, ProcessTail);
    end;
end;

{$IFDEF Parallel}


procedure TParallelCipher.DES64_Parallel(Job, buff, key: Pointer; Size: NativeInt);
var
  p: nativeUInt;
begin
  p := 0;
  repeat
    TDES.EncryptDES(PDESContext(key)^, PDESBlock(nativeUInt(buff) + p)^);
    p := p + PParallelCipherJobData(Job)^.BlockLen;
  until p + PParallelCipherJobData(Job)^.BlockLen > Size;
end;

procedure TParallelCipher.DES128_Parallel(Job, buff, key: Pointer; Size: NativeInt);
var
  p: nativeUInt;
begin
  p := 0;
  repeat
    TDES.EncryptTripleDES(PTripleDESContext(key)^, PDESBlock(nativeUInt(buff) + p)^);
    p := p + PParallelCipherJobData(Job)^.BlockLen;
  until p + PParallelCipherJobData(Job)^.BlockLen > Size;
end;

procedure TParallelCipher.DES192_Parallel(Job, buff, key: Pointer; Size: NativeInt);
var
  p: nativeUInt;
begin
  p := 0;
  repeat
    TDES.EncryptTripleDES3Key(PTripleDESContext3Key(key)^, PDESBlock(nativeUInt(buff) + p)^);
    p := p + PParallelCipherJobData(Job)^.BlockLen;
  until p + PParallelCipherJobData(Job)^.BlockLen > Size;
end;

procedure TParallelCipher.Blowfish_Parallel(Job, buff, key: Pointer; Size: NativeInt);
var
  p: nativeUInt;
begin
  p := 0;
  repeat
    TBlowfish.EncryptBF(PBFContext(key)^, PBFBlock(nativeUInt(buff) + p)^, PParallelCipherJobData(Job)^.Encrypt);
    p := p + PParallelCipherJobData(Job)^.BlockLen;
  until p + PParallelCipherJobData(Job)^.BlockLen > Size;
end;

procedure TParallelCipher.LBC_Parallel(Job, buff, key: Pointer; Size: NativeInt);
var
  p: nativeUInt;
begin
  p := 0;
  repeat
    TLBC.EncryptLBC(PLBCContext(key)^, PLBCBlock(nativeUInt(buff) + p)^);
    p := p + PParallelCipherJobData(Job)^.BlockLen;
  until p + PParallelCipherJobData(Job)^.BlockLen > Size;
end;

procedure TParallelCipher.LQC_Parallel(Job, buff, key: Pointer; Size: NativeInt);
var
  p: nativeUInt;
begin
  p := 0;
  repeat
    TLBC.EncryptLQC(PKey128(key)^, PLQCBlock(nativeUInt(buff) + p)^, PParallelCipherJobData(Job)^.Encrypt);
    p := p + PParallelCipherJobData(Job)^.BlockLen;
  until p + PParallelCipherJobData(Job)^.BlockLen > Size;
end;

procedure TParallelCipher.TwoFish_Parallel(Job, buff, key: Pointer; Size: NativeInt);
var
  p: nativeUInt;
begin
  if PParallelCipherJobData(Job)^.Encrypt then
    begin
      p := 0;
      repeat
        TTwofish.Encrypt(PTwofishKey(key)^, PTwofishBlock(nativeUInt(buff) + p)^);
        p := p + PParallelCipherJobData(Job)^.BlockLen;
      until p + PParallelCipherJobData(Job)^.BlockLen > Size;
    end
  else
    begin
      p := 0;
      repeat
        TTwofish.Decrypt(PTwofishKey(key)^, PTwofishBlock(nativeUInt(buff) + p)^);
        p := p + PParallelCipherJobData(Job)^.BlockLen;
      until p + PParallelCipherJobData(Job)^.BlockLen > Size;
    end;
end;

procedure TParallelCipher.XXTea512_Parallel(Job, buff, key: Pointer; Size: NativeInt);
var
  p: nativeUInt;
begin
  if PParallelCipherJobData(Job)^.Encrypt then
    begin
      p := 0;
      repeat
        XXTEAEncrypt(PKey128(key)^, PXXTEABlock(nativeUInt(buff) + p)^);
        p := p + PParallelCipherJobData(Job)^.BlockLen;
      until p + PParallelCipherJobData(Job)^.BlockLen > Size;
    end
  else
    begin
      p := 0;
      repeat
        XXTEADecrypt(PKey128(key)^, PXXTEABlock(nativeUInt(buff) + p)^);
        p := p + PParallelCipherJobData(Job)^.BlockLen;
      until p + PParallelCipherJobData(Job)^.BlockLen > Size;
    end;
end;

procedure TParallelCipher.RC6_Parallel(Job, buff, key: Pointer; Size: NativeInt);
var
  p: nativeUInt;
begin
  if PParallelCipherJobData(Job)^.Encrypt then
    begin
      p := 0;
      repeat
        TRC6.Encrypt(PRC6Key(key)^, PRC6Block(nativeUInt(buff) + p)^);
        p := p + PParallelCipherJobData(Job)^.BlockLen;
      until p + PParallelCipherJobData(Job)^.BlockLen > Size;
    end
  else
    begin
      p := 0;
      repeat
        TRC6.Decrypt(PRC6Key(key)^, PRC6Block(nativeUInt(buff) + p)^);
        p := p + PParallelCipherJobData(Job)^.BlockLen;
      until p + PParallelCipherJobData(Job)^.BlockLen > Size;
    end;
end;

procedure TParallelCipher.Serpent_Parallel(Job, buff, key: Pointer; Size: NativeInt);
var
  p: nativeUInt;
begin
  if PParallelCipherJobData(Job)^.Encrypt then
    begin
      p := 0;
      repeat
        TSerpent.Encrypt(PSerpentkey(key)^, PSerpentBlock(nativeUInt(buff) + p)^);
        p := p + PParallelCipherJobData(Job)^.BlockLen;
      until p + PParallelCipherJobData(Job)^.BlockLen > Size;
    end
  else
    begin
      p := 0;
      repeat
        TSerpent.Decrypt(PSerpentkey(key)^, PSerpentBlock(nativeUInt(buff) + p)^);
        p := p + PParallelCipherJobData(Job)^.BlockLen;
      until p + PParallelCipherJobData(Job)^.BlockLen > Size;
    end;
end;

procedure TParallelCipher.Mars_Parallel(Job, buff, key: Pointer; Size: NativeInt);
var
  p: nativeUInt;
begin
  if PParallelCipherJobData(Job)^.Encrypt then
    begin
      p := 0;
      repeat
        TMars.Encrypt(PMarskey(key)^, PMarsBlock(nativeUInt(buff) + p)^);
        p := p + PParallelCipherJobData(Job)^.BlockLen;
      until p + PParallelCipherJobData(Job)^.BlockLen > Size;
    end
  else
    begin
      p := 0;
      repeat
        TMars.Decrypt(PMarskey(key)^, PMarsBlock(nativeUInt(buff) + p)^);
        p := p + PParallelCipherJobData(Job)^.BlockLen;
      until p + PParallelCipherJobData(Job)^.BlockLen > Size;
    end;
end;

procedure TParallelCipher.Rijndael_Parallel(Job, buff, key: Pointer; Size: NativeInt);
var
  p: nativeUInt;
begin
  if PParallelCipherJobData(Job)^.Encrypt then
    begin
      p := 0;
      repeat
        TRijndael.Encrypt(PRijndaelkey(key)^, PRijndaelBlock(nativeUInt(buff) + p)^);
        p := p + PParallelCipherJobData(Job)^.BlockLen;
      until p + PParallelCipherJobData(Job)^.BlockLen > Size;
    end
  else
    begin
      p := 0;
      repeat
        TRijndael.Decrypt(PRijndaelkey(key)^, PRijndaelBlock(nativeUInt(buff) + p)^);
        p := p + PParallelCipherJobData(Job)^.BlockLen;
      until p + PParallelCipherJobData(Job)^.BlockLen > Size;
    end;
end;

procedure TParallelCipher.BlockCBC_Parallel(Job, buff, key: Pointer; Size: NativeInt);
begin
  TCipher.BlockCBC(buff, Size, key, PParallelCipherJobData(Job)^.BlockLen);
end;

procedure TParallelCipher.ParallelCipherCall(const JobData: PParallelCipherJobData; const FromIndex, ToIndex: Integer);
var
  newBuffPtr: Pointer;
  newBuffSiz: nativeUInt;
begin
  newBuffPtr := Pointer(nativeUInt(JobData^.OriginBuffer) + (FromIndex * JobData^.BlockLen));
  newBuffSiz := (ToIndex - FromIndex) * JobData^.BlockLen;

  try
      JobData^.cipherFunc(JobData, newBuffPtr, JobData^.KeyBuffer, newBuffSiz);
  except
  end;

  inc(JobData^.CompletedBlock, (ToIndex - FromIndex));
end;

procedure TParallelCipher.RunParallel(const JobData: PParallelCipherJobData; const Total, Depth: Integer);
var
  StepTotal, stepW: Integer;
{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: Integer);
  var
    w: Integer;
  begin
    w := stepW * pass;
    if w + stepW <= Total then
        ParallelCipherCall(JobData, w, w + stepW)
    else
        ParallelCipherCall(JobData, w, Total);
  end;
{$ENDIF FPC}


begin
  if Total <= 0 then
      Exit;
  if (Depth <= 0) or (Total < Depth) then
    begin
      ParallelCipherCall(JobData, 0, Total);
      Exit;
    end;
  stepW := Total div Depth;
  StepTotal := Total div stepW;
  if Total mod stepW > 0 then
      inc(StepTotal);

{$IFDEF FPC}
  FPCParallelFor(@Nested_ParallelFor, 0, StepTotal - 1);
{$ELSE FPC}
  DelphiParallelFor(0, StepTotal - 1, procedure(pass: Integer)
    var
      w: Integer;
    begin
      w := stepW * pass;
      if w + stepW <= Total then
          ParallelCipherCall(JobData, w, w + stepW)
      else
          ParallelCipherCall(JobData, w, Total);
    end);
{$ENDIF FPC}
end;

constructor TParallelCipher.Create;
begin
  inherited Create;
  BlockDepth := DefaultParallelDepth;
end;

destructor TParallelCipher.Destroy;
begin
  inherited Destroy;
end;

function TParallelCipher.DES64(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
var
  JobData: TParallelCipherJobData;
  k: TKey64;
  Context: TDESContext;
  tailSiz: NativeInt;
begin
  Result := False;
  if Size <= 0 then
      Exit;

  if Size >= 8 then
    begin
      if not TCipher.GetKey(KeyBuff, k) then
          Exit;

      TDES.InitEncryptDES(k, Context, Encrypt);
      JobData.cipherFunc := {$IFDEF FPC}@{$ENDIF FPC}DES64_Parallel;
      JobData.KeyBuffer := @Context;
      JobData.OriginBuffer := sour;
      JobData.BlockLen := 8;
      JobData.TotalBlock := Size div JobData.BlockLen;
      JobData.CompletedBlock := 0;
      JobData.Encrypt := Encrypt;

      RunParallel(@JobData, JobData.TotalBlock, BlockDepth);
    end;

  if ProcessTail then
    begin
      tailSiz := Size mod JobData.BlockLen;

      if tailSiz > 0 then
          TCipher.EncryptTail(Pointer(nativeUInt(sour) + Size - tailSiz), tailSiz);
    end;

  Result := True;
end;

function TParallelCipher.DES128(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
var
  JobData: TParallelCipherJobData;
  k: TKey128;
  Context: TTripleDESContext;
  tailSiz: NativeInt;
begin
  Result := False;
  if Size <= 0 then
      Exit;

  if Size >= 8 then
    begin
      if not TCipher.GetKey(KeyBuff, k) then
          Exit;

      TDES.InitEncryptTripleDES(k, Context, Encrypt);
      JobData.cipherFunc := {$IFDEF FPC}@{$ENDIF FPC}DES128_Parallel;
      JobData.KeyBuffer := @Context;
      JobData.OriginBuffer := sour;
      JobData.BlockLen := 8;
      JobData.TotalBlock := Size div JobData.BlockLen;
      JobData.CompletedBlock := 0;
      JobData.Encrypt := Encrypt;

      RunParallel(@JobData, JobData.TotalBlock, BlockDepth);
    end;

  if ProcessTail then
    begin
      tailSiz := Size mod JobData.BlockLen;

      if tailSiz > 0 then
          TCipher.EncryptTail(Pointer(nativeUInt(sour) + Size - tailSiz), tailSiz);
    end;

  Result := True;
end;

function TParallelCipher.DES192(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
var
  JobData: TParallelCipherJobData;
  k1, k2, k3: TKey64;
  Context: TTripleDESContext3Key;
  tailSiz: NativeInt;
begin
  Result := False;
  if Size <= 0 then
      Exit;

  if Size >= 8 then
    begin
      if not TCipher.GetKey(KeyBuff, k1, k2, k3) then
          Exit;

      TDES.InitEncryptTripleDES3Key(k1, k2, k3, Context, Encrypt);
      JobData.cipherFunc := {$IFDEF FPC}@{$ENDIF FPC}DES192_Parallel;
      JobData.KeyBuffer := @Context;
      JobData.OriginBuffer := sour;
      JobData.BlockLen := 8;
      JobData.TotalBlock := Size div JobData.BlockLen;
      JobData.CompletedBlock := 0;
      JobData.Encrypt := Encrypt;

      RunParallel(@JobData, JobData.TotalBlock, BlockDepth);
    end;

  if ProcessTail then
    begin
      tailSiz := Size mod JobData.BlockLen;

      if tailSiz > 0 then
          TCipher.EncryptTail(Pointer(nativeUInt(sour) + Size - tailSiz), tailSiz);
    end;

  Result := True;
end;

function TParallelCipher.Blowfish(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
var
  JobData: TParallelCipherJobData;
  k: TKey128;
  Context: TBFContext;
  tailSiz: NativeInt;
begin
  Result := False;
  if Size <= 0 then
      Exit;

  if Size >= 8 then
    begin
      if not TCipher.GetKey(KeyBuff, k) then
          Exit;

      TBlowfish.InitEncryptBF(k, Context);
      JobData.cipherFunc := {$IFDEF FPC}@{$ENDIF FPC}Blowfish_Parallel;
      JobData.KeyBuffer := @Context;
      JobData.OriginBuffer := sour;
      JobData.BlockLen := 8;
      JobData.TotalBlock := Size div JobData.BlockLen;
      JobData.CompletedBlock := 0;
      JobData.Encrypt := Encrypt;

      RunParallel(@JobData, JobData.TotalBlock, BlockDepth);
    end;

  if ProcessTail then
    begin
      tailSiz := Size mod JobData.BlockLen;

      if tailSiz > 0 then
          TCipher.EncryptTail(Pointer(nativeUInt(sour) + Size - tailSiz), tailSiz);
    end;

  Result := True;
end;

function TParallelCipher.LBC(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
var
  JobData: TParallelCipherJobData;
  k: TKey128;
  Context: TLBCContext;
  tailSiz: NativeInt;
begin
  Result := False;
  if Size <= 0 then
      Exit;

  if Size >= 16 then
    begin
      if not TCipher.GetKey(KeyBuff, k) then
          Exit;

      TLBC.InitEncryptLBC(k, Context, 16, Encrypt);
      JobData.cipherFunc := {$IFDEF FPC}@{$ENDIF FPC}LBC_Parallel;
      JobData.KeyBuffer := @Context;
      JobData.OriginBuffer := sour;
      JobData.BlockLen := 16;
      JobData.TotalBlock := Size div JobData.BlockLen;
      JobData.CompletedBlock := 0;
      JobData.Encrypt := Encrypt;

      RunParallel(@JobData, JobData.TotalBlock, BlockDepth);
    end;

  if ProcessTail then
    begin
      tailSiz := Size mod JobData.BlockLen;

      if tailSiz > 0 then
          TCipher.EncryptTail(Pointer(nativeUInt(sour) + Size - tailSiz), tailSiz);
    end;

  Result := True;
end;

function TParallelCipher.LQC(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
var
  JobData: TParallelCipherJobData;
  k: TKey128;
  tailSiz: NativeInt;
begin
  Result := False;
  if Size <= 0 then
      Exit;

  if Size >= 8 then
    begin
      if not TCipher.GetKey(KeyBuff, k) then
          Exit;

      JobData.cipherFunc := {$IFDEF FPC}@{$ENDIF FPC}LQC_Parallel;
      JobData.KeyBuffer := @k;
      JobData.OriginBuffer := sour;
      JobData.BlockLen := 8;
      JobData.TotalBlock := Size div JobData.BlockLen;
      JobData.CompletedBlock := 0;
      JobData.Encrypt := Encrypt;

      RunParallel(@JobData, JobData.TotalBlock, BlockDepth);
    end;

  if ProcessTail then
    begin
      tailSiz := Size mod JobData.BlockLen;

      if tailSiz > 0 then
          TCipher.EncryptTail(Pointer(nativeUInt(sour) + Size - tailSiz), tailSiz);
    end;

  Result := True;
end;

function TParallelCipher.TwoFish(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
var
  JobData: TParallelCipherJobData;
  k, k256: TBytes;
  Context: TTwofishKey;
  tailSiz: NativeInt;
begin
  Result := False;
  if Size <= 0 then
      Exit;

  if Size >= 16 then
    begin
      if not TCipher.GetKey(KeyBuff, k) then
          Exit;

      SetLength(k256, 32);
      THashMD.GenerateLMDKey((@k256[0])^, 32, k);

      TTwofish.InitKey(@k256[0], 32, Context);
      JobData.cipherFunc := {$IFDEF FPC}@{$ENDIF FPC}TwoFish_Parallel;
      JobData.KeyBuffer := @Context;
      JobData.OriginBuffer := sour;
      JobData.BlockLen := 16;
      JobData.TotalBlock := Size div JobData.BlockLen;
      JobData.CompletedBlock := 0;
      JobData.Encrypt := Encrypt;

      RunParallel(@JobData, JobData.TotalBlock, BlockDepth);
    end;

  if ProcessTail then
    begin
      tailSiz := Size mod JobData.BlockLen;

      if tailSiz > 0 then
          TCipher.EncryptTail(Pointer(nativeUInt(sour) + Size - tailSiz), tailSiz);
    end;

  Result := True;
end;

function TParallelCipher.XXTea512(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
var
  JobData: TParallelCipherJobData;
  k: TKey128;
  tailSiz: NativeInt;
begin
  Result := False;
  if Size <= 0 then
      Exit;

  if Size >= 64 then
    begin
      if not TCipher.GetKey(KeyBuff, k) then
          Exit;

      JobData.cipherFunc := {$IFDEF FPC}@{$ENDIF FPC}XXTea512_Parallel;
      JobData.KeyBuffer := @k;
      JobData.OriginBuffer := sour;
      JobData.BlockLen := 64;
      JobData.TotalBlock := Size div JobData.BlockLen;
      JobData.CompletedBlock := 0;
      JobData.Encrypt := Encrypt;

      RunParallel(@JobData, JobData.TotalBlock, BlockDepth);
    end;

  if ProcessTail then
    begin
      tailSiz := Size mod JobData.BlockLen;

      if tailSiz > 0 then
          TCipher.EncryptTail(Pointer(nativeUInt(sour) + Size - tailSiz), tailSiz);
    end;

  Result := True;
end;

function TParallelCipher.RC6(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
var
  JobData: TParallelCipherJobData;
  k, k256: TBytes;
  Context: TRC6Key;
  tailSiz: NativeInt;
begin
  Result := False;
  if Size <= 0 then
      Exit;

  if Size >= 16 then
    begin
      if not TCipher.GetKey(KeyBuff, k) then
          Exit;

      SetLength(k256, 32);
      THashMD.GenerateLMDKey((@k256[0])^, 32, k);

      TRC6.InitKey(@k256[0], 32, Context);
      JobData.cipherFunc := {$IFDEF FPC}@{$ENDIF FPC}RC6_Parallel;
      JobData.KeyBuffer := @Context;
      JobData.OriginBuffer := sour;
      JobData.BlockLen := 16;
      JobData.TotalBlock := Size div JobData.BlockLen;
      JobData.CompletedBlock := 0;
      JobData.Encrypt := Encrypt;

      RunParallel(@JobData, JobData.TotalBlock, BlockDepth);
    end;

  if ProcessTail then
    begin
      tailSiz := Size mod JobData.BlockLen;

      if tailSiz > 0 then
          TCipher.EncryptTail(Pointer(nativeUInt(sour) + Size - tailSiz), tailSiz);
    end;

  Result := True;
end;

function TParallelCipher.Serpent(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
var
  JobData: TParallelCipherJobData;
  k, k256: TBytes;
  Context: TSerpentkey;
  tailSiz: NativeInt;
begin
  Result := False;
  if Size <= 0 then
      Exit;

  if Size >= 16 then
    begin
      if not TCipher.GetKey(KeyBuff, k) then
          Exit;

      SetLength(k256, 32);
      THashMD.GenerateLMDKey((@k256[0])^, 32, k);

      TSerpent.InitKey(@k256[0], 32, Context);

      JobData.cipherFunc := {$IFDEF FPC}@{$ENDIF FPC}Serpent_Parallel;
      JobData.KeyBuffer := @Context;
      JobData.OriginBuffer := sour;
      JobData.BlockLen := 16;
      JobData.TotalBlock := Size div JobData.BlockLen;
      JobData.CompletedBlock := 0;
      JobData.Encrypt := Encrypt;

      RunParallel(@JobData, JobData.TotalBlock, BlockDepth);
    end;

  if ProcessTail then
    begin
      tailSiz := Size mod JobData.BlockLen;

      if tailSiz > 0 then
          TCipher.EncryptTail(Pointer(nativeUInt(sour) + Size - tailSiz), tailSiz);
    end;

  Result := True;
end;

function TParallelCipher.Mars(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
var
  JobData: TParallelCipherJobData;
  k, k256: TBytes;
  Context: TMarskey;
  tailSiz: NativeInt;
begin
  Result := False;
  if Size <= 0 then
      Exit;

  if Size >= 16 then
    begin
      if not TCipher.GetKey(KeyBuff, k) then
          Exit;

      SetLength(k256, 32);
      THashMD.GenerateLMDKey((@k256[0])^, 32, k);

      TMars.InitKey(@k256[0], 32, Context);

      JobData.cipherFunc := {$IFDEF FPC}@{$ENDIF FPC}Mars_Parallel;
      JobData.KeyBuffer := @Context;
      JobData.OriginBuffer := sour;
      JobData.BlockLen := 16;
      JobData.TotalBlock := Size div JobData.BlockLen;
      JobData.CompletedBlock := 0;
      JobData.Encrypt := Encrypt;

      RunParallel(@JobData, JobData.TotalBlock, BlockDepth);
    end;

  if ProcessTail then
    begin
      tailSiz := Size mod JobData.BlockLen;

      if tailSiz > 0 then
          TCipher.EncryptTail(Pointer(nativeUInt(sour) + Size - tailSiz), tailSiz);
    end;

  Result := True;
end;

function TParallelCipher.Rijndael(sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
var
  JobData: TParallelCipherJobData;
  k, k256: TBytes;
  Context: TRijndaelkey;
  tailSiz: NativeInt;
begin
  Result := False;
  if Size <= 0 then
      Exit;

  if Size >= 16 then
    begin
      if not TCipher.GetKey(KeyBuff, k) then
          Exit;

      SetLength(k256, 32);
      THashMD.GenerateLMDKey((@k256[0])^, 32, k);

      TRijndael.InitKey(@k256[0], 32, Context);

      JobData.cipherFunc := {$IFDEF FPC}@{$ENDIF FPC}Rijndael_Parallel;
      JobData.KeyBuffer := @Context;
      JobData.OriginBuffer := sour;
      JobData.BlockLen := 16;
      JobData.TotalBlock := Size div JobData.BlockLen;
      JobData.CompletedBlock := 0;
      JobData.Encrypt := Encrypt;

      RunParallel(@JobData, JobData.TotalBlock, BlockDepth);
    end;

  if ProcessTail then
    begin
      tailSiz := Size mod JobData.BlockLen;

      if tailSiz > 0 then
          TCipher.EncryptTail(Pointer(nativeUInt(sour) + Size - tailSiz), tailSiz);
    end;

  Result := True;
end;

procedure TParallelCipher.BlockCBC(sour: Pointer; Size: NativeInt; boxBuff: Pointer; boxSiz: NativeInt);
var
  JobData: TParallelCipherJobData;
  tailSiz: NativeInt;
begin
  if Size <= 0 then
      Exit;
  if boxSiz <= 0 then
      Exit;

  if boxSiz >= Size then
    begin
      TCipher.BlockCBC(sour, Size, boxBuff, boxSiz);
      Exit;
    end;

  JobData.cipherFunc := {$IFDEF FPC}@{$ENDIF FPC}BlockCBC_Parallel;
  JobData.KeyBuffer := boxBuff;
  JobData.OriginBuffer := sour;
  JobData.BlockLen := boxSiz;
  JobData.TotalBlock := Size div JobData.BlockLen;
  JobData.CompletedBlock := 0;
  JobData.Encrypt := True;

  RunParallel(@JobData, JobData.TotalBlock, BlockDepth);

  tailSiz := Size mod JobData.BlockLen;

  if tailSiz > 0 then
      TCipher.BlockCBC(Pointer(nativeUInt(sour) + Size - tailSiz), tailSiz, boxBuff, boxSiz);
end;

function TParallelCipher.EncryptBuffer(cs: TCipherSecurity; sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
begin
  Result := False;
  case cs of
    csNone: Result := True;
    csDES64: Result := DES64(sour, Size, KeyBuff, Encrypt, ProcessTail);
    csDES128: Result := DES128(sour, Size, KeyBuff, Encrypt, ProcessTail);
    csDES192: Result := DES192(sour, Size, KeyBuff, Encrypt, ProcessTail);
    csBlowfish: Result := Blowfish(sour, Size, KeyBuff, Encrypt, ProcessTail);
    csLBC: Result := LBC(sour, Size, KeyBuff, Encrypt, ProcessTail);
    csLQC: Result := LQC(sour, Size, KeyBuff, Encrypt, ProcessTail);
    csRNG32: Result := TCipher.RNG32(sour, Size, KeyBuff);
    csRNG64: Result := TCipher.RNG64(sour, Size, KeyBuff);
    csLSC: Result := TCipher.LSC(sour, Size, KeyBuff);
    csTwoFish: Result := TwoFish(sour, Size, KeyBuff, Encrypt, ProcessTail);
    csXXTea512: Result := XXTea512(sour, Size, KeyBuff, Encrypt, ProcessTail);
    csRC6: Result := RC6(sour, Size, KeyBuff, Encrypt, ProcessTail);
    csSerpent: Result := Serpent(sour, Size, KeyBuff, Encrypt, ProcessTail);
    csMars: Result := Mars(sour, Size, KeyBuff, Encrypt, ProcessTail);
    csRijndael: Result := Rijndael(sour, Size, KeyBuff, Encrypt, ProcessTail);
  end;
end;

function TParallelCipher.EncryptBufferCBC(cs: TCipherSecurity; sour: Pointer; Size: NativeInt; KeyBuff: PCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
begin
  if cs = TCipherSecurity.csNone then
    begin
      Result := True;
      Exit;
    end;

  if Encrypt then
    begin
      Result := EncryptBuffer(cs, sour, Size, KeyBuff, Encrypt, ProcessTail);
      if Result then
          BlockCBC(sour, Size, @SystemCBC[0], length(SystemCBC));
    end
  else
    begin
      BlockCBC(sour, Size, @SystemCBC[0], length(SystemCBC));
      Result := EncryptBuffer(cs, sour, Size, KeyBuff, Encrypt, ProcessTail);
    end;
end;

{$ENDIF}


procedure InitSysCBCAndDefaultKey(rand: Int64);
var
  i: Integer;
  Seed: TInt64;
begin
{$IFDEF Parallel}
  { system default Parallel depth }
  DefaultParallelDepth := CPUCount * 2;
  ParallelTriggerCondition := 1024;
{$ENDIF}
  SetLength(SystemCBC, 64 * 1024);
  Seed.i := rand;
  for i := 0 to (length(SystemCBC) div 4) - 1 do
      PInteger(@SystemCBC[i * 4])^ := TMISC.Random64(Seed);
end;

function SequEncryptWithDirect(const cs: TCipherSecurity; sour: Pointer; Size: NativeInt; const key: TCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
var
  k: TCipherKeyBuffer;
begin
  TCipher.GenerateKey(cs, @key[0], length(key), k);
  Result := TCipher.EncryptBuffer(cs, sour, Size, @k, Encrypt, ProcessTail);
end;

function SequEncryptWithDirect(const ca: TCipherSecurityArray; sour: Pointer; Size: NativeInt; const key: TCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
var
  i: Integer;
begin
  Result := True;

  if Encrypt then
    begin
      for i := low(ca) to high(ca) do
          Result := Result and SequEncryptWithDirect(ca[i], sour, Size, key, Encrypt, ProcessTail);
    end
  else
    begin
      for i := high(ca) downto low(ca) do
          Result := Result and SequEncryptWithDirect(ca[i], sour, Size, key, Encrypt, ProcessTail);
    end;
end;

{$IFDEF Parallel}


function SequEncryptWithParallel(const cs: TCipherSecurity; sour: Pointer; Size: NativeInt; const key: TCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
var
  k: TCipherKeyBuffer;
  Parallel: TParallelCipher;
begin
  TCipher.GenerateKey(cs, @key[0], length(key), k);

  Parallel := TParallelCipher.Create;
  Parallel.BlockDepth := DefaultParallelDepth;
  Result := Parallel.EncryptBuffer(cs, sour, Size, @k, Encrypt, ProcessTail);
  DisposeObject(Parallel);
end;

function SequEncryptWithParallel(const ca: TCipherSecurityArray; sour: Pointer; Size: NativeInt; const key: TCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
var
  i: Integer;
begin
  Result := True;

  if Encrypt then
    begin
      for i := low(ca) to high(ca) do
          Result := Result and SequEncryptWithParallel(ca[i], sour, Size, key, Encrypt, ProcessTail);
    end
  else
    begin
      for i := high(ca) downto low(ca) do
          Result := Result and SequEncryptWithParallel(ca[i], sour, Size, key, Encrypt, ProcessTail);
    end;
end;
{$ENDIF}


function SequEncrypt(const ca: TCipherSecurityArray; sour: Pointer; Size: NativeInt; const key: TCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
begin
{$IFDEF Parallel}
  if Size >= ParallelTriggerCondition then
      Result := SequEncryptWithParallel(ca, sour, Size, key, Encrypt, ProcessTail)
  else
{$ENDIF}
      Result := SequEncryptWithDirect(ca, sour, Size, key, Encrypt, ProcessTail);
end;

function SequEncrypt(const cs: TCipherSecurity; sour: Pointer; Size: NativeInt; const key: TCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
begin
{$IFDEF Parallel}
  if Size >= ParallelTriggerCondition then
      Result := SequEncryptWithParallel(cs, sour, Size, key, Encrypt, ProcessTail)
  else
{$ENDIF}
      Result := SequEncryptWithDirect(cs, sour, Size, key, Encrypt, ProcessTail);
end;

function SequEncryptCBCWithDirect(const cs: TCipherSecurity; sour: Pointer; Size: NativeInt; const key: TCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
var
  k: TCipherKeyBuffer;
begin
  TCipher.GenerateKey(cs, @key[0], length(key), k);
  Result := TCipher.EncryptBufferCBC(cs, sour, Size, @k, Encrypt, ProcessTail);
end;

function SequEncryptCBCWithDirect(const ca: TCipherSecurityArray; sour: Pointer; Size: NativeInt; const key: TCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
var
  i: Integer;
begin
  Result := True;

  if Encrypt then
    begin
      for i := low(ca) to high(ca) do
          Result := Result and SequEncryptCBCWithDirect(ca[i], sour, Size, key, Encrypt, ProcessTail);
    end
  else
    begin
      for i := high(ca) downto low(ca) do
          Result := Result and SequEncryptCBCWithDirect(ca[i], sour, Size, key, Encrypt, ProcessTail);
    end;
end;

{$IFDEF Parallel}


function SequEncryptCBCWithParallel(const cs: TCipherSecurity; sour: Pointer; Size: NativeInt; const key: TCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
var
  k: TCipherKeyBuffer;
  Parallel: TParallelCipher;
begin
  TCipher.GenerateKey(cs, @key[0], length(key), k);

  Parallel := TParallelCipher.Create;
  Parallel.BlockDepth := DefaultParallelDepth;
  Result := Parallel.EncryptBufferCBC(cs, sour, Size, @k, Encrypt, ProcessTail);
  DisposeObject(Parallel);
end;

function SequEncryptCBCWithParallel(const ca: TCipherSecurityArray; sour: Pointer; Size: NativeInt; const key: TCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
var
  i: Integer;
begin
  Result := True;

  if Encrypt then
    begin
      for i := low(ca) to high(ca) do
          Result := Result and SequEncryptCBCWithParallel(ca[i], sour, Size, key, Encrypt, ProcessTail);
    end
  else
    begin
      for i := high(ca) downto low(ca) do
          Result := Result and SequEncryptCBCWithParallel(ca[i], sour, Size, key, Encrypt, ProcessTail);
    end;
end;
{$ENDIF}


function SequEncryptCBC(const ca: TCipherSecurityArray; sour: Pointer; Size: NativeInt; const key: TCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
begin
{$IFDEF Parallel}
  if Size >= ParallelTriggerCondition then
      Result := SequEncryptCBCWithParallel(ca, sour, Size, key, Encrypt, ProcessTail)
  else
{$ENDIF}
      Result := SequEncryptCBCWithDirect(ca, sour, Size, key, Encrypt, ProcessTail);
end;

function SequEncryptCBC(const cs: TCipherSecurity; sour: Pointer; Size: NativeInt; const key: TCipherKeyBuffer; Encrypt, ProcessTail: Boolean): Boolean;
begin
{$IFDEF Parallel}
  if Size >= ParallelTriggerCondition then
      Result := SequEncryptCBCWithParallel(cs, sour, Size, key, Encrypt, ProcessTail)
  else
{$ENDIF}
      Result := SequEncryptCBCWithDirect(cs, sour, Size, key, Encrypt, ProcessTail);
end;

function GenerateSequHash(hssArry: THashSecuritys; sour: Pointer; Size: NativeInt): TPascalString;
var
  h: THashSecurity;
  vl: THashStringList;
  hBuff: TBytes;
  n: SystemString;
begin
  vl := THashStringList.Create;
  for h in hssArry do
    begin
      TCipher.GenerateHashByte(h, sour, Size, hBuff);
      n := '';
      TCipher.HashToString(hBuff, n);
      vl[TCipher.CHashName[h]] := '(' + n + ')';
    end;

  Result.Text := vl.AsText;

  DisposeObject([vl]);
end;

procedure GenerateSequHash(hssArry: THashSecuritys; sour: Pointer; Size: NativeInt; output: TListPascalString);
var
  h: THashSecurity;
  vl: THashStringList;
  hBuff: TBytes;
  n: SystemString;
begin
  vl := THashStringList.Create;
  for h in hssArry do
    begin
      TCipher.GenerateHashByte(h, sour, Size, hBuff);
      n := '';
      TCipher.HashToString(hBuff, n);
      vl[TCipher.CHashName[h]] := '(' + n + ')';
    end;

  vl.ExportAsStrings(output);

  DisposeObject([vl]);
end;

procedure GenerateSequHash(hssArry: THashSecuritys; sour: Pointer; Size: NativeInt; output: TCoreClassStream);
var
  h: THashSecurity;
  vl: THashStringList;
  hBuff: TBytes;
  n: SystemString;
begin
  vl := THashStringList.Create;
  for h in hssArry do
    begin
      TCipher.GenerateHashByte(h, sour, Size, hBuff);
      n := '';
      TCipher.HashToString(hBuff, n);
      vl[TCipher.CHashName[h]] := '(' + n + ')';
    end;

  vl.SaveToStream(output);

  DisposeObject([vl]);
end;

function CompareSequHash(HashVL: THashStringList; sour: Pointer; Size: NativeInt): Boolean;
var
  ns: TListString;
  i: Integer;
  sourHash, destHash: TBytes;
  hName: SystemString;
  hValue: TPascalString;
  h: THashSecurity;
begin
  Result := True;

  ns := TListString.Create;
  HashVL.GetNameList(ns);

  for i := 0 to ns.Count - 1 do
    begin
      hName := ns[i];
      hValue := umlTrimSpace(HashVL.GetDefaultValue(hName, ''));
      if TCipher.NameToHashSecurity(hName, h) and ((hValue.Len >= 2) and (hValue.First = '(') and (hValue.Last = ')')) then
        begin
          hValue.DeleteFirst;
          hValue.DeleteLast;

          TCipher.GenerateHashByte(h, sour, Size, sourHash);
          SetLength(destHash, length(sourHash));
          if length(destHash) > 0 then
            begin
              Result := Result and TCipher.HexToBuffer(hValue, destHash[0], length(destHash));
              Result := Result and TCipher.CompareHash(sourHash, destHash);
            end;
        end;
      if not Result then
          Break;
    end;

  DisposeObject([ns]);
end;

function CompareSequHash(hashData: TPascalString; sour: Pointer; Size: NativeInt): Boolean;
var
  vl: THashStringList;
begin
  vl := THashStringList.Create;
  vl.AsText := hashData.Text;
  Result := CompareSequHash(vl, sour, Size);
  DisposeObject(vl);
end;

function CompareSequHash(hashData: TListPascalString; sour: Pointer; Size: NativeInt): Boolean;
var
  vl: THashStringList;
begin
  vl := THashStringList.Create;
  vl.ImportFromStrings(hashData);
  Result := CompareSequHash(vl, sour, Size);
  DisposeObject(vl);
end;

function CompareSequHash(hashData: TCoreClassStream; sour: Pointer; Size: NativeInt): Boolean;
var
  vl: THashStringList;
begin
  vl := THashStringList.Create;
  vl.LoadFromStream(hashData);
  Result := CompareSequHash(vl, sour, Size);
  DisposeObject(vl);
end;

function GenerateMemoryHash(hssArry: THashSecuritys; sour: Pointer; Size: NativeInt): TPascalString;
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  GenerateSequHash(hssArry, sour, Size, m64);
  umlEncodeStreamBASE64(m64, Result);
  DisposeObject(m64);
  Result := '(' + Result + ')';
end;

function CompareMemoryHash(sour: Pointer; Size: NativeInt; const hashBuff: TPascalString): Boolean;
var
  n: TPascalString;
  m64: TMemoryStream64;
begin
  Result := False;
  n := hashBuff.TrimChar(#32);
  if (n.Len > 2) and (n.First = '(') and (n.Last = ')') then
    begin
      n.DeleteFirst;
      n.DeleteLast;
      m64 := TMemoryStream64.Create;
      umlDecodeStreamBASE64(n, m64);
      Result := CompareSequHash(m64, sour, Size);
      DisposeObject(m64);
    end;
end;

function GeneratePasswordHash(hssArry: THashSecuritys; const passwd: TPascalString): TPascalString;
var
  buff: TBytes;
  m64: TMemoryStream64;
begin
  buff := passwd.Bytes;
  m64 := TMemoryStream64.Create;
  GenerateSequHash(hssArry, @buff[0], length(buff), m64);
  umlEncodeStreamBASE64(m64, Result);
  DisposeObject(m64);
  Result := '(' + Result + ')';
  SetLength(buff, 0);
end;

function ComparePasswordHash(const passwd, hashBuff: TPascalString): Boolean;
var
  n: TPascalString;
  buff: TBytes;
  m64: TMemoryStream64;
begin
  Result := False;
  n := hashBuff.TrimChar(#32);
  if (n.Len > 2) and (n.First = '(') and (n.Last = ')') then
    begin
      n.DeleteFirst;
      n.DeleteLast;
      buff := passwd.Bytes;
      m64 := TMemoryStream64.Create;
      umlDecodeStreamBASE64(n, m64);
      Result := CompareSequHash(m64, @buff[0], length(buff));
      DisposeObject(m64);
      SetLength(buff, 0);
    end;
end;

function GeneratePassword(const ca: TCipherSecurityArray; const passwd: TPascalString): TPascalString;
var
  KeyBuff: TBytes;
  buff: TBytes;
begin
  KeyBuff := passwd.Bytes;
  buff := passwd.Bytes;
  SequEncryptCBC(ca, @buff[0], length(buff), KeyBuff, True, False);
  umlBase64EncodeBytes(buff, Result);
  Result := '(' + Result + ')';
  SetLength(buff, 0);
  SetLength(KeyBuff, 0);
end;

function ComparePassword(const ca: TCipherSecurityArray; const passwd, passwdDataSource: TPascalString): Boolean;
var
  sour: TPascalString;
  refBuff: TBytes;
  KeyBuff: TBytes;
begin
  Result := False;
  sour := umlTrimSpace(passwdDataSource);
  if (sour.Len > 2) and (sour.First = '(') and (sour.Last = ')') then
    begin
      sour.DeleteFirst;
      sour.DeleteLast;
      umlBase64DecodeBytes(sour, refBuff);
      KeyBuff := passwd.Bytes;
      SequEncryptCBC(ca, @refBuff[0], length(refBuff), KeyBuff, False, False);
      Result := TCipher.CompareKey(refBuff, KeyBuff);
      SetLength(refBuff, 0);
      SetLength(KeyBuff, 0);
    end;
end;

function GeneratePassword(const cs: TCipherSecurity; const passwd: TPascalString): TPascalString; overload;
var
  KeyBuff: TBytes;
  buff: TBytes;
begin
  KeyBuff := passwd.Bytes;
  buff := passwd.Bytes;
  SequEncryptCBC(cs, @buff[0], length(buff), KeyBuff, True, False);
  umlBase64EncodeBytes(buff, Result);
  Result := '(' + Result + ')';
  SetLength(buff, 0);
  SetLength(KeyBuff, 0);
end;

function ComparePassword(const cs: TCipherSecurity; const passwd, passwdDataSource: TPascalString): Boolean; overload;
var
  sour: TPascalString;
  refBuff: TBytes;
  KeyBuff: TBytes;
begin
  Result := False;
  sour := umlTrimSpace(passwdDataSource);
  if (sour.Len > 2) and (sour.First = '(') and (sour.Last = ')') then
    begin
      sour.DeleteFirst;
      sour.DeleteLast;
      umlBase64DecodeBytes(sour, refBuff);
      KeyBuff := passwd.Bytes;
      SequEncryptCBC(cs, @refBuff[0], length(refBuff), KeyBuff, False, False);
      Result := TCipher.CompareKey(refBuff, KeyBuff);
      SetLength(refBuff, 0);
      SetLength(KeyBuff, 0);
    end;
end;

function GenerateQuantumCryptographyPassword(const passwd: TPascalString): TPascalString;
var
  buff, cryptBuff: TBytes;
begin
  buff := passwd.Bytes;
  SetLength(cryptBuff, 512 div 8);
  TSHA3.SHAKE256(@cryptBuff[0], @buff[0], length(buff), 512);
  umlBase64EncodeBytes(cryptBuff, Result);
  Result := '(' + Result + ')';
  SetLength(buff, 0);
  SetLength(cryptBuff, 0);
end;

function CompareQuantumCryptographyPassword(const passwd, passwdDataSource: TPascalString): Boolean;
var
  sour: TPascalString;
  refBuff, buff, cryptBuff: TBytes;
begin
  Result := False;
  sour := umlTrimSpace(passwdDataSource);
  if (sour.Len > 2) and (sour.First = '(') and (sour.Last = ')') then
    begin
      sour.DeleteFirst;
      sour.DeleteLast;
      umlBase64DecodeBytes(sour, refBuff);
      buff := passwd.Bytes;
      SetLength(cryptBuff, 512 div 8);
      TSHA3.SHAKE256(@cryptBuff[0], @buff[0], length(buff), 512);
      Result := TCipher.CompareKey(refBuff, cryptBuff);
      SetLength(refBuff, 0);
      SetLength(buff, 0);
      SetLength(cryptBuff, 0);
    end;
end;

type
  TQuantumEncryptHead = packed record
    CipherSecurity: Byte;
    Level: Word;
    Size: Int64;
    hash: TSHA3_512_Digest;
  end;

procedure QuantumEncrypt(input, output: TCoreClassStream; SecurityLevel: Integer; key: TCipherKeyBuffer);
var
  m64: TMemoryStream64;
  head: TQuantumEncryptHead;
  i: Integer;
  hh: TSHA3_512_Digest;
begin
  m64 := TMemoryStream64.Create;
  input.Position := 0;
  m64.CopyFrom(input, input.Size);

  head.CipherSecurity := Byte(TCipherSecurity.csRijndael);
  head.Level := SecurityLevel;
  head.Size := m64.Size;

  // 2x-sha3-512 Secure Hash
  TSHA3.SHA512(hh, m64.Memory, head.Size);
  TSHA3.SHA512(head.hash, @hh[0], 64);

  // infinition encrypt
  for i := 0 to head.Level - 1 do
      SequEncryptCBC(TCipherSecurity(head.CipherSecurity), m64.Memory, m64.Size, key, True, True);

  output.write(head, SizeOf(head));
  output.write(m64.Memory^, m64.Size);
  DisposeObject(m64);
end;

function QuantumDecrypt(input, output: TCoreClassStream; key: TCipherKeyBuffer): Boolean;
var
  head: TQuantumEncryptHead;
  m64: TMemoryStream64;
  i: Integer;
  hh, h: TSHA3_512_Digest;
begin
  Result := False;
  input.read(head, SizeOf(head));
  m64 := TMemoryStream64.Create;
  if m64.CopyFrom(input, head.Size) <> head.Size then
    begin
      DisposeObject(m64);
      Exit;
    end;

  // infinition encrypt
  for i := 0 to head.Level - 1 do
      SequEncryptCBC(TCipherSecurity(head.CipherSecurity), m64.Memory, m64.Size, key, False, True);

  // 2x-sha3-512 Secure Hash
  TSHA3.SHA512(hh, m64.Memory, m64.Size);
  TSHA3.SHA512(h, @hh[0], 64);

  // compare
  if not TCipher.CompareHash(h, head.hash) then
    begin
      DisposeObject(m64);
      Exit;
    end;

  m64.Position := 0;
  output.CopyFrom(m64, m64.Size);
  DisposeObject(m64);

  Result := True;
end;

procedure TestCoreCipher;
var
  Buffer: TBytes;
  sour, Dest: TMemoryStream64;
  k: TCipherKeyBuffer;
  cs: TCipherSecurity;
  sourHash: TSHA1Digest;
  d: TTimeTick;

  hs: THashSecurity;
  hByte: TBytes;

{$IFDEF Parallel}
  Parallel: TParallelCipher;
{$ENDIF}
  ps: TListPascalString;

  s: TPascalString;
begin
  sour := TMemoryStream64.Create;
  sour.Size := Int64(10 * 1024 * 1024 + 9);

  FillPtrByte(sour.Memory, sour.Size, $7F);
  DoStatus('stream mode md5 :' + umlStreamMD5String(sour).Text);
  DoStatus('pointer mode md5:' + umlMD5String(sour.Memory, sour.Size).Text);

  DisposeObject(sour);

  DoStatus('Generate and verify QuantumCryptographyPassword test');
  s := GenerateQuantumCryptographyPassword('123456');
  if not CompareQuantumCryptographyPassword('123456', s) then
      DoStatus('QuantumCryptographyPassword failed!');
  if CompareQuantumCryptographyPassword('1234560', s) then
      DoStatus('QuantumCryptographyPassword failed!');

  DoStatus('Generate and verify password test');
  DoStatus('verify short password');
  s := GeneratePasswordHash(TCipher.CAllHash, '1');
  if not ComparePasswordHash('1', s) then
      DoStatus('PasswordHash failed!');
  if ComparePasswordHash('11', s) then
      DoStatus('PasswordHash failed!');

  DoStatus('verify long password');
  s := GeneratePasswordHash(TCipher.CAllHash, 'hello world 123456');
  if not ComparePasswordHash('hello world 123456', s) then
      DoStatus('PasswordHash failed!');
  if ComparePasswordHash('111 hello world 123456', s) then
      DoStatus('PasswordHash failed!');

  DoStatus('verify full chiher style password');
  s := GeneratePassword(TCipher.AllCipher, 'hello world');
  if not ComparePassword(TCipher.AllCipher, 'hello world', s) then
      DoStatus('Password cipher test failed! cipher: %s', ['']);
  if ComparePassword(TCipher.AllCipher, 'hello_world', s) then
      DoStatus('Password cipher test failed! cipher: %s', ['']);

  for cs in TCipher.AllCipher do
    begin
      DoStatus('verify %s chiher style password', [GetEnumName(TypeInfo(TCipherSecurity), Integer(cs))]);
      s := GeneratePassword(TCipher.AllCipher, 'hello world');
      if not ComparePassword(TCipher.AllCipher, 'hello world', s) then
          DoStatus('Password cipher test failed! cipher: %s', [GetEnumName(TypeInfo(TCipherSecurity), Integer(cs))]);
      if ComparePassword(TCipher.AllCipher, 'hello_world', s) then
          DoStatus('Password cipher test failed! cipher: %s', [GetEnumName(TypeInfo(TCipherSecurity), Integer(cs))]);
    end;

  // hash and Sequence Encrypt
  SetLength(Buffer, 128 * 1024);
  FillPtrByte(@Buffer[0], length(Buffer), 99);

  ps := TListPascalString.Create;

  DoStatus('Generate Sequence Hash');
  GenerateSequHash(TCipher.CAllHash, @Buffer[0], length(Buffer), ps);
  // DoStatus(ps.Text);

  if not CompareSequHash(ps, @Buffer[0], length(Buffer)) then
      DoStatus('hash compare failed!');

  DoStatus('test Sequence Encrypt');
  k := TPascalString('hello world').Bytes;
  if not SequEncryptWithDirect(TCipher.AllCipher, @Buffer[0], length(Buffer), k, True, True) then
      DoStatus('SequEncrypt failed!');
  if not SequEncryptWithDirect(TCipher.AllCipher, @Buffer[0], length(Buffer), k, False, True) then
      DoStatus('SequEncrypt failed!');

  DoStatus('verify Sequence Encrypt');
  if not CompareSequHash(ps, @Buffer[0], length(Buffer)) then
      DoStatus('hash compare failed!');

  // cipher Encrypt performance
  SetLength(Buffer, 1024 * 1024 * 1 + 99);
  FillPtrByte(@Buffer[0], length(Buffer), $7F);

  sour := TMemoryStream64.Create;
  Dest := TMemoryStream64.Create;
  sour.write(Buffer[0], high(Buffer));

  Dest.Clear;
  sour.Position := 0;
  Dest.CopyFrom(sour, sour.Size);
  sour.Position := 0;
  Dest.Position := 0;

  sourHash := TCipher.GenerateSHA1Hash(sour.Memory, sour.Size);

{$IFDEF Parallel}
  DoStatus(#13#10'Parallel cipher performance test');

  for cs in TCipher.AllCipher do
    begin
      TCipher.GenerateKey(cs, 'hello world', k);
      Parallel := TParallelCipher.Create;

      Dest.Clear;
      sour.Position := 0;
      Dest.CopyFrom(sour, sour.Size);
      sour.Position := 0;
      Dest.Position := 0;

      d := GetTimeTick;

      if not Parallel.EncryptBufferCBC(cs, Dest.Memory, Dest.Size, @k, True, True) then
          DoStatus('%s: Parallel encode failed', [GetEnumName(TypeInfo(TCipherSecurity), Integer(cs))]);
      if not Parallel.EncryptBufferCBC(cs, Dest.Memory, Dest.Size, @k, False, True) then
          DoStatus('%s: Parallel decode failed', [GetEnumName(TypeInfo(TCipherSecurity), Integer(cs))]);
      DoStatus('%s - Parallel performance:%dms', [GetEnumName(TypeInfo(TCipherSecurity), Integer(cs)), GetTimeTick - d]);

      if not TCipher.CompareHash(TCipher.GenerateSHA1Hash(Dest.Memory, Dest.Size), sourHash) then
          DoStatus('%s Parallel hash error!', [GetEnumName(TypeInfo(TCipherSecurity), Integer(cs))]);

      DisposeObject(Parallel);
    end;

  for cs in TCipher.AllCipher do
    begin
      TCipher.GenerateKey(cs, 'hello world', k);
      Parallel := TParallelCipher.Create;

      Dest.Clear;
      sour.Position := 0;
      Dest.CopyFrom(sour, sour.Size);
      sour.Position := 0;
      Dest.Position := 0;

      d := GetTimeTick;

      if not TCipher.EncryptBufferCBC(cs, Dest.Memory, Dest.Size, @k, True, True) then
          DoStatus('%s: normal 2 Parallel encode failed', [GetEnumName(TypeInfo(TCipherSecurity), Integer(cs))]);
      if not Parallel.EncryptBufferCBC(cs, Dest.Memory, Dest.Size, @k, False, True) then
          DoStatus('%s: normal 2 Parallel decode failed', [GetEnumName(TypeInfo(TCipherSecurity), Integer(cs))]);
      DoStatus('%s - normal 2 Parallel performance:%dms', [GetEnumName(TypeInfo(TCipherSecurity), Integer(cs)), GetTimeTick - d]);

      if not TCipher.CompareHash(TCipher.GenerateSHA1Hash(Dest.Memory, Dest.Size), sourHash) then
          DoStatus('%s normal 2 Parallel hash error!', [GetEnumName(TypeInfo(TCipherSecurity), Integer(cs))]);

      DisposeObject(Parallel);
    end;

  for cs in TCipher.AllCipher do
    begin
      TCipher.GenerateKey(cs, 'hello world', k);
      Parallel := TParallelCipher.Create;

      Dest.Clear;
      sour.Position := 0;
      Dest.CopyFrom(sour, sour.Size);
      sour.Position := 0;
      Dest.Position := 0;

      d := GetTimeTick;

      if not Parallel.EncryptBufferCBC(cs, Dest.Memory, Dest.Size, @k, True, True) then
          DoStatus('%s: Parallel 2 normal encode failed', [GetEnumName(TypeInfo(TCipherSecurity), Integer(cs))]);
      if not TCipher.EncryptBufferCBC(cs, Dest.Memory, Dest.Size, @k, False, True) then
          DoStatus('%s: Parallel 2 normal decode failed', [GetEnumName(TypeInfo(TCipherSecurity), Integer(cs))]);
      DoStatus('%s - Parallel 2 normal performance:%dms', [GetEnumName(TypeInfo(TCipherSecurity), Integer(cs)), GetTimeTick - d]);

      if not TCipher.CompareHash(TCipher.GenerateSHA1Hash(Dest.Memory, Dest.Size), sourHash) then
          DoStatus('%s Parallel 2 normal hash error!', [GetEnumName(TypeInfo(TCipherSecurity), Integer(cs))]);

      DisposeObject(Parallel);
    end;
{$ENDIF}
  DoStatus(#13#10'normal cipher performance test');

  for cs in TCipher.AllCipher do
    begin
      TCipher.GenerateKey(cs, 'hello world', k);

      Dest.Clear;
      sour.Position := 0;
      Dest.CopyFrom(sour, sour.Size);
      sour.Position := 0;
      Dest.Position := 0;

      d := GetTimeTick;
      if not TCipher.EncryptBufferCBC(cs, Dest.Memory, Dest.Size, @k, True, True) then
          DoStatus('%s: encode failed', [GetEnumName(TypeInfo(TCipherSecurity), Integer(cs))]);
      if not TCipher.EncryptBufferCBC(cs, Dest.Memory, Dest.Size, @k, False, True) then
          DoStatus('%s: decode failed', [GetEnumName(TypeInfo(TCipherSecurity), Integer(cs))]);
      DoStatus('%s - normal performance:%dms', [GetEnumName(TypeInfo(TCipherSecurity), Integer(cs)), GetTimeTick - d]);
      if not TCipher.CompareHash(TCipher.GenerateSHA1Hash(Dest.Memory, Dest.Size), sourHash) then
          DoStatus('%s hash error!', [GetEnumName(TypeInfo(TCipherSecurity), Integer(cs))]);
    end;

  // hash performance
  DoStatus(#13#10'hash performance test');
  Dest.Clear;
  sour.Position := 0;
  Dest.CopyFrom(sour, sour.Size);
  sour.Position := 0;
  Dest.Position := 0;

  for hs := low(THashSecurity) to high(THashSecurity) do
    begin
      d := GetTimeTick;
      TCipher.GenerateHashByte(hs, Dest.Memory, Dest.Size, hByte);
      DoStatus('%s - performance:%dms', [GetEnumName(TypeInfo(THashSecurity), Integer(hs)), (GetTimeTick - d)]);
    end;

  DoStatus(#13#10'Cipher test done!');
  DisposeObject([ps, sour, Dest]);
end;

{ TBlowfish }

class procedure TBlowfish.EncryptBF(const Context: TBFContext; var Block: TBFBlock; Encrypt: Boolean);
var
  i: Integer;
  TmpBlock: TBFBlockEx; { !!.01 }
begin
  CopyPtr(@Block, @TmpBlock, SizeOf(TmpBlock)); { !!.01 }
  if Encrypt then begin
      Block[0] := Block[0] xor Context.PBox[0];

      { 16 Rounds to go (8 double rounds to avoid swaps) }
      i := 1;
      repeat
        { first half round }
        Block[1] := Block[1] xor Context.PBox[i] xor (((
          Context.SBox[0, TmpBlock.Xl[3]] + Context.SBox[1, TmpBlock.Xl[2]])
          xor Context.SBox[2, TmpBlock.Xl[1]]) + Context.SBox[3, TmpBlock.Xl[0]]);
        { second half round }
        Block[0] := Block[0] xor Context.PBox[i + 1] xor (((
          Context.SBox[0, TmpBlock.Xr[3]] + Context.SBox[1, TmpBlock.Xr[2]])
          xor Context.SBox[2, TmpBlock.Xr[1]]) + Context.SBox[3, TmpBlock.Xr[0]]);
        inc(i, 2);
      until i > BFRounds;
      Block[1] := Block[1] xor Context.PBox[(BFRounds + 1)];
    end
  else begin
      Block[1] := Block[1] xor Context.PBox[(BFRounds + 1)];

      { 16 Rounds to go (8 double rounds to avoid swaps) }
      i := BFRounds;
      repeat
        { first half round }
        Block[0] := Block[0] xor Context.PBox[i] xor (((
          Context.SBox[0, TmpBlock.Xr[3]] + Context.SBox[1, TmpBlock.Xr[2]])
          xor Context.SBox[2, TmpBlock.Xr[1]]) + Context.SBox[3, TmpBlock.Xr[0]]);
        { second half round }
        Block[1] := Block[1] xor Context.PBox[i - 1] xor (((
          Context.SBox[0, TmpBlock.Xl[3]] + Context.SBox[1, TmpBlock.Xl[2]])
          xor Context.SBox[2, TmpBlock.Xl[1]]) + Context.SBox[3, TmpBlock.Xl[0]]);
        dec(i, 2);
      until i < 1;
      Block[0] := Block[0] xor Context.PBox[0];
    end;
end;

class procedure TBlowfish.InitEncryptBF(key: TKey128; var Context: TBFContext);
var
  i: Integer;
  j: Integer;
  k: Integer;
  Data: DWORD;
  Block: TBFBlock;
begin
  { initialize PArray }
  CopyPtr(@bf_P, @Context.PBox, SizeOf(Context.PBox));
  { initialize SBox }
  CopyPtr(@bf_S, @Context.SBox, SizeOf(Context.SBox));

  { update PArray with the key bits }
  j := 0;
  for i := 0 to (BFRounds + 1) do begin
      Data := 0;
      for k := 0 to 3 do begin
          Data := (Data shl 8) or key[j];
          inc(j);
          if j >= SizeOf(key) then
              j := 0;
        end;
      Context.PBox[i] := Context.PBox[i] xor Data;
    end;

  { encrypt an all-zero SystemString using the Blowfish algorithm and }
  { replace the elements of the P-array with the output of this process }

  Block[0] := 0;
  Block[1] := 0;
  i := 0;
  repeat
    EncryptBF(Context, Block, True);
    Context.PBox[i] := Block[0];
    Context.PBox[i + 1] := Block[1];
    inc(i, 2);
  until i > BFRounds + 1;

  { continue the process, replacing the elements of the four S-boxes in }
  { order, with the output of the continuously changing Blowfish algorithm }

  for j := 0 to 3 do begin
      i := 0;
      repeat
        EncryptBF(Context, Block, True);
        Context.SBox[j, i] := Block[0];
        Context.SBox[j, i + 1] := Block[1];
        inc(i, 2);
      until i > 255;
    end;

  { in total, 521 iterations are required to generate all required subkeys. }
end;

{ TDES }

class procedure TDES.EncryptDES(const Context: TDESContext; var Block: TDESBlock);
const
  SPBox: array [0 .. 7, 0 .. 63] of DWORD =
    (($01010400, $00000000, $00010000, $01010404, $01010004, $00010404, $00000004, $00010000,
    $00000400, $01010400, $01010404, $00000400, $01000404, $01010004, $01000000, $00000004,
    $00000404, $01000400, $01000400, $00010400, $00010400, $01010000, $01010000, $01000404,
    $00010004, $01000004, $01000004, $00010004, $00000000, $00000404, $00010404, $01000000,
    $00010000, $01010404, $00000004, $01010000, $01010400, $01000000, $01000000, $00000400,
    $01010004, $00010000, $00010400, $01000004, $00000400, $00000004, $01000404, $00010404,
    $01010404, $00010004, $01010000, $01000404, $01000004, $00000404, $00010404, $01010400,
    $00000404, $01000400, $01000400, $00000000, $00010004, $00010400, $00000000, $01010004),
    ($80108020, $80008000, $00008000, $00108020, $00100000, $00000020, $80100020, $80008020,
    $80000020, $80108020, $80108000, $80000000, $80008000, $00100000, $00000020, $80100020,
    $00108000, $00100020, $80008020, $00000000, $80000000, $00008000, $00108020, $80100000,
    $00100020, $80000020, $00000000, $00108000, $00008020, $80108000, $80100000, $00008020,
    $00000000, $00108020, $80100020, $00100000, $80008020, $80100000, $80108000, $00008000,
    $80100000, $80008000, $00000020, $80108020, $00108020, $00000020, $00008000, $80000000,
    $00008020, $80108000, $00100000, $80000020, $00100020, $80008020, $80000020, $00100020,
    $00108000, $00000000, $80008000, $00008020, $80000000, $80100020, $80108020, $00108000),
    ($00000208, $08020200, $00000000, $08020008, $08000200, $00000000, $00020208, $08000200,
    $00020008, $08000008, $08000008, $00020000, $08020208, $00020008, $08020000, $00000208,
    $08000000, $00000008, $08020200, $00000200, $00020200, $08020000, $08020008, $00020208,
    $08000208, $00020200, $00020000, $08000208, $00000008, $08020208, $00000200, $08000000,
    $08020200, $08000000, $00020008, $00000208, $00020000, $08020200, $08000200, $00000000,
    $00000200, $00020008, $08020208, $08000200, $08000008, $00000200, $00000000, $08020008,
    $08000208, $00020000, $08000000, $08020208, $00000008, $00020208, $00020200, $08000008,
    $08020000, $08000208, $00000208, $08020000, $00020208, $00000008, $08020008, $00020200),
    ($00802001, $00002081, $00002081, $00000080, $00802080, $00800081, $00800001, $00002001,
    $00000000, $00802000, $00802000, $00802081, $00000081, $00000000, $00800080, $00800001,
    $00000001, $00002000, $00800000, $00802001, $00000080, $00800000, $00002001, $00002080,
    $00800081, $00000001, $00002080, $00800080, $00002000, $00802080, $00802081, $00000081,
    $00800080, $00800001, $00802000, $00802081, $00000081, $00000000, $00000000, $00802000,
    $00002080, $00800080, $00800081, $00000001, $00802001, $00002081, $00002081, $00000080,
    $00802081, $00000081, $00000001, $00002000, $00800001, $00002001, $00802080, $00800081,
    $00002001, $00002080, $00800000, $00802001, $00000080, $00800000, $00002000, $00802080),
    ($00000100, $02080100, $02080000, $42000100, $00080000, $00000100, $40000000, $02080000,
    $40080100, $00080000, $02000100, $40080100, $42000100, $42080000, $00080100, $40000000,
    $02000000, $40080000, $40080000, $00000000, $40000100, $42080100, $42080100, $02000100,
    $42080000, $40000100, $00000000, $42000000, $02080100, $02000000, $42000000, $00080100,
    $00080000, $42000100, $00000100, $02000000, $40000000, $02080000, $42000100, $40080100,
    $02000100, $40000000, $42080000, $02080100, $40080100, $00000100, $02000000, $42080000,
    $42080100, $00080100, $42000000, $42080100, $02080000, $00000000, $40080000, $42000000,
    $00080100, $02000100, $40000100, $00080000, $00000000, $40080000, $02080100, $40000100),
    ($20000010, $20400000, $00004000, $20404010, $20400000, $00000010, $20404010, $00400000,
    $20004000, $00404010, $00400000, $20000010, $00400010, $20004000, $20000000, $00004010,
    $00000000, $00400010, $20004010, $00004000, $00404000, $20004010, $00000010, $20400010,
    $20400010, $00000000, $00404010, $20404000, $00004010, $00404000, $20404000, $20000000,
    $20004000, $00000010, $20400010, $00404000, $20404010, $00400000, $00004010, $20000010,
    $00400000, $20004000, $20000000, $00004010, $20000010, $20404010, $00404000, $20400000,
    $00404010, $20404000, $00000000, $20400010, $00000010, $00004000, $20400000, $00404010,
    $00004000, $00400010, $20004010, $00000000, $20404000, $20000000, $00400010, $20004010),
    ($00200000, $04200002, $04000802, $00000000, $00000800, $04000802, $00200802, $04200800,
    $04200802, $00200000, $00000000, $04000002, $00000002, $04000000, $04200002, $00000802,
    $04000800, $00200802, $00200002, $04000800, $04000002, $04200000, $04200800, $00200002,
    $04200000, $00000800, $00000802, $04200802, $00200800, $00000002, $04000000, $00200800,
    $04000000, $00200800, $00200000, $04000802, $04000802, $04200002, $04200002, $00000002,
    $00200002, $04000000, $04000800, $00200000, $04200800, $00000802, $00200802, $04200800,
    $00000802, $04000002, $04200802, $04200000, $00200800, $00000000, $00000002, $04200802,
    $00000000, $00200802, $04200000, $00000800, $04000002, $04000800, $00000800, $00200002),
    ($10001040, $00001000, $00040000, $10041040, $10000000, $10001040, $00000040, $10000000,
    $00040040, $10040000, $10041040, $00041000, $10041000, $00041040, $00001000, $00000040,
    $10040000, $10000040, $10001000, $00001040, $00041000, $00040040, $10040040, $10041000,
    $00001040, $00000000, $00000000, $10040040, $10000040, $10001000, $00041040, $00040000,
    $00041040, $00040000, $10041000, $00001000, $00000040, $10040040, $00001000, $00041040,
    $10001000, $00000040, $10000040, $10040000, $10040040, $10000000, $00040000, $10001040,
    $00000000, $10041040, $00040040, $10000040, $10040000, $10001000, $10001040, $00000000,
    $10041040, $00041000, $00041000, $00001040, $00001040, $00040040, $10000000, $10041000));
var
  i, l, r, Work: DWORD;
  CPtr: PDWORD;

  procedure IPerm(var l, r: DWORD);
  var
    Work: DWORD;
  begin
    Work := ((l shr 4) xor r) and $0F0F0F0F;
    r := r xor Work;
    l := l xor Work shl 4;

    Work := ((l shr 16) xor r) and $0000FFFF;
    r := r xor Work;
    l := l xor Work shl 16;

    Work := ((r shr 2) xor l) and $33333333;
    l := l xor Work;
    r := r xor Work shl 2;

    Work := ((r shr 8) xor l) and $00FF00FF;
    l := l xor Work;
    r := r xor Work shl 8;

    r := (r shl 1) or (r shr 31);
    Work := (l xor r) and $AAAAAAAA;
    l := l xor Work;
    r := r xor Work;
    l := (l shl 1) or (l shr 31);
  end;

  procedure FPerm(var l, r: DWORD);
  var
    Work: DWORD;
  begin
    l := l;

    r := (r shl 31) or (r shr 1);
    Work := (l xor r) and $AAAAAAAA;
    l := l xor Work;
    r := r xor Work;
    l := (l shr 1) or (l shl 31);

    Work := ((l shr 8) xor r) and $00FF00FF;
    r := r xor Work;
    l := l xor Work shl 8;

    Work := ((l shr 2) xor r) and $33333333;
    r := r xor Work;
    l := l xor Work shl 2;

    Work := ((r shr 16) xor l) and $0000FFFF;
    l := l xor Work;
    r := r xor Work shl 16;

    Work := ((r shr 4) xor l) and $0F0F0F0F;
    l := l xor Work;
    r := r xor Work shl 4;
  end;

begin
  SplitBlock(Block, l, r);
  IPerm(l, r);

  CPtr := @Context;
  for i := 0 to 7 do begin
      Work := (((r shr 4) or (r shl 28)) xor CPtr^);
      inc(CPtr);
      l := l xor SPBox[6, Work and $3F];
      l := l xor SPBox[4, Work shr 8 and $3F];
      l := l xor SPBox[2, Work shr 16 and $3F];
      l := l xor SPBox[0, Work shr 24 and $3F];

      Work := (r xor CPtr^);
      inc(CPtr);
      l := l xor SPBox[7, Work and $3F];
      l := l xor SPBox[5, Work shr 8 and $3F];
      l := l xor SPBox[3, Work shr 16 and $3F];
      l := l xor SPBox[1, Work shr 24 and $3F];

      Work := (((l shr 4) or (l shl 28)) xor CPtr^);
      inc(CPtr);
      r := r xor SPBox[6, Work and $3F];
      r := r xor SPBox[4, Work shr 8 and $3F];
      r := r xor SPBox[2, Work shr 16 and $3F];
      r := r xor SPBox[0, Work shr 24 and $3F];

      Work := (l xor CPtr^);
      inc(CPtr);
      r := r xor SPBox[7, Work and $3F];
      r := r xor SPBox[5, Work shr 8 and $3F];
      r := r xor SPBox[3, Work shr 16 and $3F];
      r := r xor SPBox[1, Work shr 24 and $3F];
    end;

  FPerm(l, r);
  JoinBlock(l, r, Block);
end;

class procedure TDES.EncryptTripleDES(const Context: TTripleDESContext; var Block: TDESBlock);
begin
  EncryptDES(Context[0], Block);
  EncryptDES(Context[1], Block);
  EncryptDES(Context[0], Block);
end;

{ !!.01 }
class procedure TDES.EncryptTripleDES3Key(const Context: TTripleDESContext3Key; var Block: TDESBlock);
begin
  EncryptDES(Context[2], Block);
  EncryptDES(Context[1], Block);
  EncryptDES(Context[0], Block);
end;

class procedure TDES.InitEncryptDES(const key: TKey64; var Context: TDESContext; Encrypt: Boolean);
const
  PC1: array [0 .. 55] of Byte =
    (56, 48, 40, 32, 24, 16, 8, 0, 57, 49, 41, 33, 25, 17, 9, 1, 58, 50, 42, 34, 26,
    18, 10, 2, 59, 51, 43, 35, 62, 54, 46, 38, 30, 22, 14, 6, 61, 53, 45, 37, 29, 21,
    13, 5, 60, 52, 44, 36, 28, 20, 12, 4, 27, 19, 11, 3);
  PC2: array [0 .. 47] of Byte =
    (13, 16, 10, 23, 0, 4, 2, 27, 14, 5, 20, 9, 22, 18, 11, 3, 25, 7,
    15, 6, 26, 19, 12, 1, 40, 51, 30, 36, 46, 54, 29, 39, 50, 44, 32, 47,
    43, 48, 38, 55, 33, 52, 45, 41, 49, 35, 28, 31);
  CTotRot: array [0 .. 15] of Byte = (1, 2, 4, 6, 8, 10, 12, 14, 15, 17, 19, 21, 23, 25, 27, 28);
  CBitMask: array [0 .. 7] of Byte = (128, 64, 32, 16, 8, 4, 2, 1);
var
  PC1M: array [0 .. 55] of Byte;
  PC1R: array [0 .. 55] of Byte;
  KS: array [0 .. 7] of Byte;
  i, j, l, m: Integer;
begin
  { convert PC1 to bits of key }
  for j := 0 to 55 do begin
      l := PC1[j];
      m := l mod 8;
      PC1M[j] := Ord((key[l div 8] and CBitMask[m]) <> 0);
    end;

  { key chunk for each iteration }
  for i := 0 to 15 do begin
      { rotate PC1 the right amount }
      for j := 0 to 27 do begin
          l := j + CTotRot[i];
          if (l < 28) then begin
              PC1R[j] := PC1M[l];
              PC1R[j + 28] := PC1M[l + 28];
            end
          else begin
              PC1R[j] := PC1M[l - 28];
              PC1R[j + 28] := PC1M[l];
            end;
        end;

      { select bits individually }
      FillPtrByte(@KS, SizeOf(KS), 0);
      for j := 0 to 47 do
        if Boolean(PC1R[PC2[j]]) then begin
            l := j div 6;
            KS[l] := KS[l] or CBitMask[j mod 6] shr 2;
          end;

      { now convert to odd/even interleaved form for use in F }
      if Encrypt then begin
          Context.TransformedKey[i * 2] := (Integer(KS[0]) shl 24) or (Integer(KS[2]) shl 16) or
            (Integer(KS[4]) shl 8) or (Integer(KS[6]));
          Context.TransformedKey[i * 2 + 1] := (Integer(KS[1]) shl 24) or (Integer(KS[3]) shl 16) or
            (Integer(KS[5]) shl 8) or (Integer(KS[7]));
        end
      else begin
          Context.TransformedKey[31 - (i * 2 + 1)] := (Integer(KS[0]) shl 24) or (Integer(KS[2]) shl 16) or
            (Integer(KS[4]) shl 8) or (Integer(KS[6]));
          Context.TransformedKey[31 - (i * 2)] := (Integer(KS[1]) shl 24) or (Integer(KS[3]) shl 16) or
            (Integer(KS[5]) shl 8) or (Integer(KS[7]));
        end;
    end;

  Context.Encrypt := Encrypt;
end;

class procedure TDES.InitEncryptTripleDES(const key: TKey128; var Context: TTripleDESContext; Encrypt: Boolean);
var
  KeyArray: array [0 .. 1] of TKey64;
begin
  CopyPtr(@key, @KeyArray, SizeOf(KeyArray)); { !!.01 }
  if Encrypt then begin
      InitEncryptDES(KeyArray[0], Context[0], True);
      InitEncryptDES(KeyArray[1], Context[1], False);
    end
  else begin
      InitEncryptDES(KeyArray[0], Context[0], False);
      InitEncryptDES(KeyArray[1], Context[1], True);
    end;
end;

{ !!.01 }
class procedure TDES.InitEncryptTripleDES3Key(const Key1, Key2, Key3: TKey64; var Context: TTripleDESContext3Key; Encrypt: Boolean);
begin
  if Encrypt then begin
      InitEncryptDES(Key1, Context[0], True);
      InitEncryptDES(Key2, Context[1], False);
      InitEncryptDES(Key3, Context[2], True);
    end
  else begin
      InitEncryptDES(Key1, Context[2], False);
      InitEncryptDES(Key2, Context[1], True);
      InitEncryptDES(Key3, Context[0], False);
    end;
end;

class procedure TDES.JoinBlock(const l, r: DWORD; var Block: TDESBlock);
var
  Temp: TDesConverter;
  i: Integer;
begin
  Temp.DWords[0] := DWORD(l);
  Temp.DWords[1] := DWORD(r);
  for i := low(Block) to high(Block) do
      Block[i] := Temp.Bytes[7 - i];
end;

class procedure TDES.ShrinkDESKey(var key: TKey64);
const
  SK1: TKey64 = ($C4, $08, $B0, $54, $0B, $A1, $E0, $AE);
  SK2: TKey64 = ($EF, $2C, $04, $1C, $E6, $38, $2F, $E6);
var
  i: Integer;
  Work1: TKey64;
  Work2: TKey64;
  Context: TDESContext;
begin
  { step #1 zero the parity bits - 8, 16, 24, ..., 64 }
  for i := 0 to 7 do
      Work1[i] := key[i] and $FE;

  { step #2 encrypt output of #1 with SK1 and xor with output of #1 }
  InitEncryptDES(SK1, Context, True);
  Work2 := Work1; { make copy }
  EncryptDES(Context, TDESBlock(Work2));
  for i := 0 to 7 do
      Work1[i] := Work1[i] xor Work2[i];

  { step #3 zero bits 1,2,3,4,8,16,17,18,19,20,24,32,33,34,35,36,40,48,49,50,51,52,56,64 }
  TInt64(Work1).Lo := TInt64(Work1).Lo and $F101F101;
  TInt64(Work1).Hi := TInt64(Work1).Hi and $F101F101;

  { step #4 encrypt output of #3 with SK2 }
  InitEncryptDES(SK2, Context, True);
  EncryptDES(Context, TDESBlock(Work1));

  key := Work1;
end;

class procedure TDES.SplitBlock(const Block: TDESBlock; var l, r: DWORD);
var
  Temp: TDesConverter;
  i: Integer;
begin
  for i := low(Block) to high(Block) do
      Temp.Bytes[7 - i] := Block[i];
  l := Temp.DWords[1];
  r := Temp.DWords[0];
end;

{ TSHA1 }

class procedure TSHA1.FinalizeSHA1(var Context: TSHA1Context; var Digest: TSHA1Digest);
begin
  with Context do begin
      sdBuf[sdIndex] := $80;

      if sdIndex >= 56 then
          SHA1Hash(Context);

      PDWORD(@sdBuf[56])^ := SHA1SwapByteOrder(sdHi);
      PDWORD(@sdBuf[60])^ := SHA1SwapByteOrder(sdLo);

      SHA1Hash(Context);

      sdHash[0] := SHA1SwapByteOrder(sdHash[0]);
      sdHash[1] := SHA1SwapByteOrder(sdHash[1]);
      sdHash[2] := SHA1SwapByteOrder(sdHash[2]);
      sdHash[3] := SHA1SwapByteOrder(sdHash[3]);
      sdHash[4] := SHA1SwapByteOrder(sdHash[4]);

      CopyPtr(@sdHash, @Digest, SizeOf(Digest));
      SHA1Clear(Context);
    end;
end;

class procedure TSHA1.SHA1(var Digest: TSHA1Digest; const Buf; BufSize: nativeUInt);
var
  Context: TSHA1Context;
begin
  InitSHA1(Context);
  UpdateSHA1(Context, Buf, BufSize);
  FinalizeSHA1(Context, Digest);
end;

class procedure TSHA1.InitSHA1(var Context: TSHA1Context);
begin
  SHA1Clear(Context);
  Context.sdHash[0] := SHA1_A;
  Context.sdHash[1] := SHA1_B;
  Context.sdHash[2] := SHA1_C;
  Context.sdHash[3] := SHA1_D;
  Context.sdHash[4] := SHA1_E;
end;

class procedure TSHA1.SHA1Clear(var Context: TSHA1Context);
begin
  FillPtrByte(@Context, SizeOf(Context), $00);
end;

class procedure TSHA1.SHA1Hash(var Context: TSHA1Context);
var
  a: DWORD;
  b: DWORD;
  c: DWORD;
  d: DWORD;
  E: DWORD;

  x: DWORD;
  w: array [0 .. 79] of DWORD;

  i: Integer;
begin
  with Context do begin
      sdIndex := 0;
      CopyPtr(@sdBuf, @w, SizeOf(w));

      // W := Mt, for t = 0 to 15 : Mt is M sub t
      for i := 0 to 15 do
          w[i] := SHA1SwapByteOrder(w[i]);

      // Transform Message block from 16 32 bit words to 80 32 bit words
      // Wt, = ( Wt-3 xor Wt-8 xor Wt-13 xor Wt-16 ) rolL 1 : Wt is W sub t
      for i := 16 to 79 do
          w[i] := TMISC.RolX(w[i - 3] xor w[i - 8] xor w[i - 14] xor w[i - 16], 1);

      a := sdHash[0];
      b := sdHash[1];
      c := sdHash[2];
      d := sdHash[3];
      E := sdHash[4];

      // the four rounds
      for i := 0 to 19 do begin
          x := TMISC.RolX(a, 5) + (d xor (b and (c xor d))) + E + w[i] + SHA1_K1;
          E := d;
          d := c;
          c := TMISC.RolX(b, 30);
          b := a;
          a := x;
        end;

      for i := 20 to 39 do begin
          x := TMISC.RolX(a, 5) + (b xor c xor d) + E + w[i] + SHA1_K2;
          E := d;
          d := c;
          c := TMISC.RolX(b, 30);
          b := a;
          a := x;
        end;

      for i := 40 to 59 do begin
          x := TMISC.RolX(a, 5) + ((b and c) or (d and (b or c))) + E + w[i] + SHA1_K3;
          E := d;
          d := c;
          c := TMISC.RolX(b, 30);
          b := a;
          a := x;
        end;

      for i := 60 to 79 do
        begin
          x := TMISC.RolX(a, 5) + (b xor c xor d) + E + w[i] + SHA1_K4;
          E := d;
          d := c;
          c := TMISC.RolX(b, 30);
          b := a;
          a := x;
        end;

      sdHash[0] := sdHash[0] + a;
      sdHash[1] := sdHash[1] + b;
      sdHash[2] := sdHash[2] + c;
      sdHash[3] := sdHash[3] + d;
      sdHash[4] := sdHash[4] + E;

      FillPtrByte(@w, SizeOf(w), $00);
      FillPtrByte(@sdBuf, SizeOf(sdBuf), $00);
    end;
end;

class function TSHA1.SHA1SwapByteOrder(n: DWORD): DWORD;
begin
  n := (n shr 24) or ((n shr 8) and LBMASK_LO) or ((n shl 8) and LBMASK_HI) or (n shl 24);
  Result := n;
end;

class procedure TSHA1.SHA1UpdateLen(var Context: TSHA1Context; Len: DWORD);
begin
  inc(Context.sdLo, (Len shl 3));
  if Context.sdLo < (Len shl 3) then
      inc(Context.sdHi);
  inc(Context.sdHi, Len shr 29);
end;

class procedure TSHA1.ByteBuffSHA1(var Digest: TSHA1Digest; const ABytes: TBytes);
begin
  SHA1(Digest, ABytes[0], length(ABytes));
end;

class procedure TSHA1.UpdateSHA1(var Context: TSHA1Context; const Buf; BufSize: nativeUInt);
var
  PBuf: PByte;
begin
  with Context do begin
      SHA1UpdateLen(Context, BufSize);
      PBuf := @Buf;
      while BufSize > 0 do begin
          if (SizeOf(sdBuf) - sdIndex) <= DWORD(BufSize) then begin
              CopyPtr(PBuf, @sdBuf[sdIndex], SizeOf(sdBuf) - sdIndex);
              dec(BufSize, SizeOf(sdBuf) - sdIndex);
              inc(PBuf, SizeOf(sdBuf) - sdIndex);
              SHA1Hash(Context);
            end
          else begin
              CopyPtr(PBuf, @sdBuf[sdIndex], BufSize);
              inc(sdIndex, BufSize);
              BufSize := 0;
            end;
        end;
    end;
end;

class procedure TSHA256.SwapDWORD(var a: DWORD);
begin
  a := Endian(a);
end;

class procedure TSHA256.Compute(var Digest: TSHA256Digest; const buff: Pointer);
var
  a, b, c, d, E, f, g, h, tmp1, tmp2: DWORD;
  w: array [0 .. 63] of DWORD;
  i: ShortInt;
begin
  a := PDWORD(@Digest[0])^;
  b := PDWORD(@Digest[4])^;
  c := PDWORD(@Digest[8])^;
  d := PDWORD(@Digest[12])^;
  E := PDWORD(@Digest[16])^;
  f := PDWORD(@Digest[20])^;
  g := PDWORD(@Digest[24])^;
  h := PDWORD(@Digest[28])^;

  CopyPtr(buff, @w[0], 64);

  for i := 0 to 15 do
      SwapDWORD(w[i]);

  for i := 16 to 63 do
      w[i] := (((w[i - 2] shr 17) or (w[i - 2] shl 15)) xor ((w[i - 2] shr 19) or (w[i - 2] shl 13)) xor
      (w[i - 2] shr 10)) + w[i - 7] + (((w[i - 15] shr 7) or (w[i - 15] shl 25)) xor
      ((w[i - 15] shr 18) or (w[i - 15] shl 14)) xor (w[i - 15] shr 3)) + w[i - 16];

  tmp1 := h + (((E shr 6) or (E shl 26)) xor ((E shr 11) or (E shl 21)) xor ((E shr 25) or (E shl 7))) + ((E and f) xor (not E and g)) + $428A2F98 + w[0];
  tmp2 := (((a shr 2) or (a shl 30)) xor ((a shr 13) or (a shl 19)) xor ((a shr 22) xor (a shl 10))) + ((a and b) xor (a and c) xor (b and c));
  h := tmp1 + tmp2;
  d := d + tmp1;
  tmp1 := g + (((d shr 6) or (d shl 26)) xor ((d shr 11) or (d shl 21)) xor ((d shr 25) or (d shl 7))) + ((d and E) xor (not d and f)) + $71374491 + w[1];
  tmp2 := (((h shr 2) or (h shl 30)) xor ((h shr 13) or (h shl 19)) xor ((h shr 22) xor (h shl 10))) + ((h and a) xor (h and b) xor (a and b));
  g := tmp1 + tmp2;
  c := c + tmp1;
  tmp1 := f + (((c shr 6) or (c shl 26)) xor ((c shr 11) or (c shl 21)) xor ((c shr 25) or (c shl 7))) + ((c and d) xor (not c and E)) + $B5C0FBCF + w[2];
  tmp2 := (((g shr 2) or (g shl 30)) xor ((g shr 13) or (g shl 19)) xor ((g shr 22) xor (g shl 10))) + ((g and h) xor (g and a) xor (h and a));
  f := tmp1 + tmp2;
  b := b + tmp1;
  tmp1 := E + (((b shr 6) or (b shl 26)) xor ((b shr 11) or (b shl 21)) xor ((b shr 25) or (b shl 7))) + ((b and c) xor (not b and d)) + $E9B5DBA5 + w[3];
  tmp2 := (((f shr 2) or (f shl 30)) xor ((f shr 13) or (f shl 19)) xor ((f shr 22) xor (f shl 10))) + ((f and g) xor (f and h) xor (g and h));
  E := tmp1 + tmp2;
  a := a + tmp1;
  tmp1 := d + (((a shr 6) or (a shl 26)) xor ((a shr 11) or (a shl 21)) xor ((a shr 25) or (a shl 7))) + ((a and b) xor (not a and c)) + $3956C25B + w[4];
  tmp2 := (((E shr 2) or (E shl 30)) xor ((E shr 13) or (E shl 19)) xor ((E shr 22) xor (E shl 10))) + ((E and f) xor (E and g) xor (f and g));
  d := tmp1 + tmp2;
  h := h + tmp1;
  tmp1 := c + (((h shr 6) or (h shl 26)) xor ((h shr 11) or (h shl 21)) xor ((h shr 25) or (h shl 7))) + ((h and a) xor (not h and b)) + $59F111F1 + w[5];
  tmp2 := (((d shr 2) or (d shl 30)) xor ((d shr 13) or (d shl 19)) xor ((d shr 22) xor (d shl 10))) + ((d and E) xor (d and f) xor (E and f));
  c := tmp1 + tmp2;
  g := g + tmp1;
  tmp1 := b + (((g shr 6) or (g shl 26)) xor ((g shr 11) or (g shl 21)) xor ((g shr 25) or (g shl 7))) + ((g and h) xor (not g and a)) + $923F82A4 + w[6];
  tmp2 := (((c shr 2) or (c shl 30)) xor ((c shr 13) or (c shl 19)) xor ((c shr 22) xor (c shl 10))) + ((c and d) xor (c and E) xor (d and E));
  b := tmp1 + tmp2;
  f := f + tmp1;
  tmp1 := a + (((f shr 6) or (f shl 26)) xor ((f shr 11) or (f shl 21)) xor ((f shr 25) or (f shl 7))) + ((f and g) xor (not f and h)) + $AB1C5ED5 + w[7];
  tmp2 := (((b shr 2) or (b shl 30)) xor ((b shr 13) or (b shl 19)) xor ((b shr 22) xor (b shl 10))) + ((b and c) xor (b and d) xor (c and d));
  a := tmp1 + tmp2;
  E := E + tmp1;
  tmp1 := h + (((E shr 6) or (E shl 26)) xor ((E shr 11) or (E shl 21)) xor ((E shr 25) or (E shl 7))) + ((E and f) xor (not E and g)) + $D807AA98 + w[8];
  tmp2 := (((a shr 2) or (a shl 30)) xor ((a shr 13) or (a shl 19)) xor ((a shr 22) xor (a shl 10))) + ((a and b) xor (a and c) xor (b and c));
  h := tmp1 + tmp2;
  d := d + tmp1;
  tmp1 := g + (((d shr 6) or (d shl 26)) xor ((d shr 11) or (d shl 21)) xor ((d shr 25) or (d shl 7))) + ((d and E) xor (not d and f)) + $12835B01 + w[9];
  tmp2 := (((h shr 2) or (h shl 30)) xor ((h shr 13) or (h shl 19)) xor ((h shr 22) xor (h shl 10))) + ((h and a) xor (h and b) xor (a and b));
  g := tmp1 + tmp2;
  c := c + tmp1;
  tmp1 := f + (((c shr 6) or (c shl 26)) xor ((c shr 11) or (c shl 21)) xor ((c shr 25) or (c shl 7))) + ((c and d) xor (not c and E)) + $243185BE + w[10];
  tmp2 := (((g shr 2) or (g shl 30)) xor ((g shr 13) or (g shl 19)) xor ((g shr 22) xor (g shl 10))) + ((g and h) xor (g and a) xor (h and a));
  f := tmp1 + tmp2;
  b := b + tmp1;
  tmp1 := E + (((b shr 6) or (b shl 26)) xor ((b shr 11) or (b shl 21)) xor ((b shr 25) or (b shl 7))) + ((b and c) xor (not b and d)) + $550C7DC3 + w[11];
  tmp2 := (((f shr 2) or (f shl 30)) xor ((f shr 13) or (f shl 19)) xor ((f shr 22) xor (f shl 10))) + ((f and g) xor (f and h) xor (g and h));
  E := tmp1 + tmp2;
  a := a + tmp1;
  tmp1 := d + (((a shr 6) or (a shl 26)) xor ((a shr 11) or (a shl 21)) xor ((a shr 25) or (a shl 7))) + ((a and b) xor (not a and c)) + $72BE5D74 + w[12];
  tmp2 := (((E shr 2) or (E shl 30)) xor ((E shr 13) or (E shl 19)) xor ((E shr 22) xor (E shl 10))) + ((E and f) xor (E and g) xor (f and g));
  d := tmp1 + tmp2;
  h := h + tmp1;
  tmp1 := c + (((h shr 6) or (h shl 26)) xor ((h shr 11) or (h shl 21)) xor ((h shr 25) or (h shl 7))) + ((h and a) xor (not h and b)) + $80DEB1FE + w[13];
  tmp2 := (((d shr 2) or (d shl 30)) xor ((d shr 13) or (d shl 19)) xor ((d shr 22) xor (d shl 10))) + ((d and E) xor (d and f) xor (E and f));
  c := tmp1 + tmp2;
  g := g + tmp1;
  tmp1 := b + (((g shr 6) or (g shl 26)) xor ((g shr 11) or (g shl 21)) xor ((g shr 25) or (g shl 7))) + ((g and h) xor (not g and a)) + $9BDC06A7 + w[14];
  tmp2 := (((c shr 2) or (c shl 30)) xor ((c shr 13) or (c shl 19)) xor ((c shr 22) xor (c shl 10))) + ((c and d) xor (c and E) xor (d and E));
  b := tmp1 + tmp2;
  f := f + tmp1;
  tmp1 := a + (((f shr 6) or (f shl 26)) xor ((f shr 11) or (f shl 21)) xor ((f shr 25) or (f shl 7))) + ((f and g) xor (not f and h)) + $C19BF174 + w[15];
  tmp2 := (((b shr 2) or (b shl 30)) xor ((b shr 13) or (b shl 19)) xor ((b shr 22) xor (b shl 10))) + ((b and c) xor (b and d) xor (c and d));
  a := tmp1 + tmp2;
  E := E + tmp1;
  tmp1 := h + (((E shr 6) or (E shl 26)) xor ((E shr 11) or (E shl 21)) xor ((E shr 25) or (E shl 7))) + ((E and f) xor (not E and g)) + $E49B69C1 + w[16];
  tmp2 := (((a shr 2) or (a shl 30)) xor ((a shr 13) or (a shl 19)) xor ((a shr 22) xor (a shl 10))) + ((a and b) xor (a and c) xor (b and c));
  h := tmp1 + tmp2;
  d := d + tmp1;
  tmp1 := g + (((d shr 6) or (d shl 26)) xor ((d shr 11) or (d shl 21)) xor ((d shr 25) or (d shl 7))) + ((d and E) xor (not d and f)) + $EFBE4786 + w[17];
  tmp2 := (((h shr 2) or (h shl 30)) xor ((h shr 13) or (h shl 19)) xor ((h shr 22) xor (h shl 10))) + ((h and a) xor (h and b) xor (a and b));
  g := tmp1 + tmp2;
  c := c + tmp1;
  tmp1 := f + (((c shr 6) or (c shl 26)) xor ((c shr 11) or (c shl 21)) xor ((c shr 25) or (c shl 7))) + ((c and d) xor (not c and E)) + $0FC19DC6 + w[18];
  tmp2 := (((g shr 2) or (g shl 30)) xor ((g shr 13) or (g shl 19)) xor ((g shr 22) xor (g shl 10))) + ((g and h) xor (g and a) xor (h and a));
  f := tmp1 + tmp2;
  b := b + tmp1;
  tmp1 := E + (((b shr 6) or (b shl 26)) xor ((b shr 11) or (b shl 21)) xor ((b shr 25) or (b shl 7))) + ((b and c) xor (not b and d)) + $240CA1CC + w[19];
  tmp2 := (((f shr 2) or (f shl 30)) xor ((f shr 13) or (f shl 19)) xor ((f shr 22) xor (f shl 10))) + ((f and g) xor (f and h) xor (g and h));
  E := tmp1 + tmp2;
  a := a + tmp1;
  tmp1 := d + (((a shr 6) or (a shl 26)) xor ((a shr 11) or (a shl 21)) xor ((a shr 25) or (a shl 7))) + ((a and b) xor (not a and c)) + $2DE92C6F + w[20];
  tmp2 := (((E shr 2) or (E shl 30)) xor ((E shr 13) or (E shl 19)) xor ((E shr 22) xor (E shl 10))) + ((E and f) xor (E and g) xor (f and g));
  d := tmp1 + tmp2;
  h := h + tmp1;
  tmp1 := c + (((h shr 6) or (h shl 26)) xor ((h shr 11) or (h shl 21)) xor ((h shr 25) or (h shl 7))) + ((h and a) xor (not h and b)) + $4A7484AA + w[21];
  tmp2 := (((d shr 2) or (d shl 30)) xor ((d shr 13) or (d shl 19)) xor ((d shr 22) xor (d shl 10))) + ((d and E) xor (d and f) xor (E and f));
  c := tmp1 + tmp2;
  g := g + tmp1;
  tmp1 := b + (((g shr 6) or (g shl 26)) xor ((g shr 11) or (g shl 21)) xor ((g shr 25) or (g shl 7))) + ((g and h) xor (not g and a)) + $5CB0A9DC + w[22];
  tmp2 := (((c shr 2) or (c shl 30)) xor ((c shr 13) or (c shl 19)) xor ((c shr 22) xor (c shl 10))) + ((c and d) xor (c and E) xor (d and E));
  b := tmp1 + tmp2;
  f := f + tmp1;
  tmp1 := a + (((f shr 6) or (f shl 26)) xor ((f shr 11) or (f shl 21)) xor ((f shr 25) or (f shl 7))) + ((f and g) xor (not f and h)) + $76F988DA + w[23];
  tmp2 := (((b shr 2) or (b shl 30)) xor ((b shr 13) or (b shl 19)) xor ((b shr 22) xor (b shl 10))) + ((b and c) xor (b and d) xor (c and d));
  a := tmp1 + tmp2;
  E := E + tmp1;
  tmp1 := h + (((E shr 6) or (E shl 26)) xor ((E shr 11) or (E shl 21)) xor ((E shr 25) or (E shl 7))) + ((E and f) xor (not E and g)) + $983E5152 + w[24];
  tmp2 := (((a shr 2) or (a shl 30)) xor ((a shr 13) or (a shl 19)) xor ((a shr 22) xor (a shl 10))) + ((a and b) xor (a and c) xor (b and c));
  h := tmp1 + tmp2;
  d := d + tmp1;
  tmp1 := g + (((d shr 6) or (d shl 26)) xor ((d shr 11) or (d shl 21)) xor ((d shr 25) or (d shl 7))) + ((d and E) xor (not d and f)) + $A831C66D + w[25];
  tmp2 := (((h shr 2) or (h shl 30)) xor ((h shr 13) or (h shl 19)) xor ((h shr 22) xor (h shl 10))) + ((h and a) xor (h and b) xor (a and b));
  g := tmp1 + tmp2;
  c := c + tmp1;
  tmp1 := f + (((c shr 6) or (c shl 26)) xor ((c shr 11) or (c shl 21)) xor ((c shr 25) or (c shl 7))) + ((c and d) xor (not c and E)) + $B00327C8 + w[26];
  tmp2 := (((g shr 2) or (g shl 30)) xor ((g shr 13) or (g shl 19)) xor ((g shr 22) xor (g shl 10))) + ((g and h) xor (g and a) xor (h and a));
  f := tmp1 + tmp2;
  b := b + tmp1;
  tmp1 := E + (((b shr 6) or (b shl 26)) xor ((b shr 11) or (b shl 21)) xor ((b shr 25) or (b shl 7))) + ((b and c) xor (not b and d)) + $BF597FC7 + w[27];
  tmp2 := (((f shr 2) or (f shl 30)) xor ((f shr 13) or (f shl 19)) xor ((f shr 22) xor (f shl 10))) + ((f and g) xor (f and h) xor (g and h));
  E := tmp1 + tmp2;
  a := a + tmp1;
  tmp1 := d + (((a shr 6) or (a shl 26)) xor ((a shr 11) or (a shl 21)) xor ((a shr 25) or (a shl 7))) + ((a and b) xor (not a and c)) + $C6E00BF3 + w[28];
  tmp2 := (((E shr 2) or (E shl 30)) xor ((E shr 13) or (E shl 19)) xor ((E shr 22) xor (E shl 10))) + ((E and f) xor (E and g) xor (f and g));
  d := tmp1 + tmp2;
  h := h + tmp1;
  tmp1 := c + (((h shr 6) or (h shl 26)) xor ((h shr 11) or (h shl 21)) xor ((h shr 25) or (h shl 7))) + ((h and a) xor (not h and b)) + $D5A79147 + w[29];
  tmp2 := (((d shr 2) or (d shl 30)) xor ((d shr 13) or (d shl 19)) xor ((d shr 22) xor (d shl 10))) + ((d and E) xor (d and f) xor (E and f));
  c := tmp1 + tmp2;
  g := g + tmp1;
  tmp1 := b + (((g shr 6) or (g shl 26)) xor ((g shr 11) or (g shl 21)) xor ((g shr 25) or (g shl 7))) + ((g and h) xor (not g and a)) + $06CA6351 + w[30];
  tmp2 := (((c shr 2) or (c shl 30)) xor ((c shr 13) or (c shl 19)) xor ((c shr 22) xor (c shl 10))) + ((c and d) xor (c and E) xor (d and E));
  b := tmp1 + tmp2;
  f := f + tmp1;
  tmp1 := a + (((f shr 6) or (f shl 26)) xor ((f shr 11) or (f shl 21)) xor ((f shr 25) or (f shl 7))) + ((f and g) xor (not f and h)) + $14292967 + w[31];
  tmp2 := (((b shr 2) or (b shl 30)) xor ((b shr 13) or (b shl 19)) xor ((b shr 22) xor (b shl 10))) + ((b and c) xor (b and d) xor (c and d));
  a := tmp1 + tmp2;
  E := E + tmp1;
  tmp1 := h + (((E shr 6) or (E shl 26)) xor ((E shr 11) or (E shl 21)) xor ((E shr 25) or (E shl 7))) + ((E and f) xor (not E and g)) + $27B70A85 + w[32];
  tmp2 := (((a shr 2) or (a shl 30)) xor ((a shr 13) or (a shl 19)) xor ((a shr 22) xor (a shl 10))) + ((a and b) xor (a and c) xor (b and c));
  h := tmp1 + tmp2;
  d := d + tmp1;
  tmp1 := g + (((d shr 6) or (d shl 26)) xor ((d shr 11) or (d shl 21)) xor ((d shr 25) or (d shl 7))) + ((d and E) xor (not d and f)) + $2E1B2138 + w[33];
  tmp2 := (((h shr 2) or (h shl 30)) xor ((h shr 13) or (h shl 19)) xor ((h shr 22) xor (h shl 10))) + ((h and a) xor (h and b) xor (a and b));
  g := tmp1 + tmp2;
  c := c + tmp1;
  tmp1 := f + (((c shr 6) or (c shl 26)) xor ((c shr 11) or (c shl 21)) xor ((c shr 25) or (c shl 7))) + ((c and d) xor (not c and E)) + $4D2C6DFC + w[34];
  tmp2 := (((g shr 2) or (g shl 30)) xor ((g shr 13) or (g shl 19)) xor ((g shr 22) xor (g shl 10))) + ((g and h) xor (g and a) xor (h and a));
  f := tmp1 + tmp2;
  b := b + tmp1;
  tmp1 := E + (((b shr 6) or (b shl 26)) xor ((b shr 11) or (b shl 21)) xor ((b shr 25) or (b shl 7))) + ((b and c) xor (not b and d)) + $53380D13 + w[35];
  tmp2 := (((f shr 2) or (f shl 30)) xor ((f shr 13) or (f shl 19)) xor ((f shr 22) xor (f shl 10))) + ((f and g) xor (f and h) xor (g and h));
  E := tmp1 + tmp2;
  a := a + tmp1;
  tmp1 := d + (((a shr 6) or (a shl 26)) xor ((a shr 11) or (a shl 21)) xor ((a shr 25) or (a shl 7))) + ((a and b) xor (not a and c)) + $650A7354 + w[36];
  tmp2 := (((E shr 2) or (E shl 30)) xor ((E shr 13) or (E shl 19)) xor ((E shr 22) xor (E shl 10))) + ((E and f) xor (E and g) xor (f and g));
  d := tmp1 + tmp2;
  h := h + tmp1;
  tmp1 := c + (((h shr 6) or (h shl 26)) xor ((h shr 11) or (h shl 21)) xor ((h shr 25) or (h shl 7))) + ((h and a) xor (not h and b)) + $766A0ABB + w[37];
  tmp2 := (((d shr 2) or (d shl 30)) xor ((d shr 13) or (d shl 19)) xor ((d shr 22) xor (d shl 10))) + ((d and E) xor (d and f) xor (E and f));
  c := tmp1 + tmp2;
  g := g + tmp1;
  tmp1 := b + (((g shr 6) or (g shl 26)) xor ((g shr 11) or (g shl 21)) xor ((g shr 25) or (g shl 7))) + ((g and h) xor (not g and a)) + $81C2C92E + w[38];
  tmp2 := (((c shr 2) or (c shl 30)) xor ((c shr 13) or (c shl 19)) xor ((c shr 22) xor (c shl 10))) + ((c and d) xor (c and E) xor (d and E));
  b := tmp1 + tmp2;
  f := f + tmp1;
  tmp1 := a + (((f shr 6) or (f shl 26)) xor ((f shr 11) or (f shl 21)) xor ((f shr 25) or (f shl 7))) + ((f and g) xor (not f and h)) + $92722C85 + w[39];
  tmp2 := (((b shr 2) or (b shl 30)) xor ((b shr 13) or (b shl 19)) xor ((b shr 22) xor (b shl 10))) + ((b and c) xor (b and d) xor (c and d));
  a := tmp1 + tmp2;
  E := E + tmp1;
  tmp1 := h + (((E shr 6) or (E shl 26)) xor ((E shr 11) or (E shl 21)) xor ((E shr 25) or (E shl 7))) + ((E and f) xor (not E and g)) + $A2BFE8A1 + w[40];
  tmp2 := (((a shr 2) or (a shl 30)) xor ((a shr 13) or (a shl 19)) xor ((a shr 22) xor (a shl 10))) + ((a and b) xor (a and c) xor (b and c));
  h := tmp1 + tmp2;
  d := d + tmp1;
  tmp1 := g + (((d shr 6) or (d shl 26)) xor ((d shr 11) or (d shl 21)) xor ((d shr 25) or (d shl 7))) + ((d and E) xor (not d and f)) + $A81A664B + w[41];
  tmp2 := (((h shr 2) or (h shl 30)) xor ((h shr 13) or (h shl 19)) xor ((h shr 22) xor (h shl 10))) + ((h and a) xor (h and b) xor (a and b));
  g := tmp1 + tmp2;
  c := c + tmp1;
  tmp1 := f + (((c shr 6) or (c shl 26)) xor ((c shr 11) or (c shl 21)) xor ((c shr 25) or (c shl 7))) + ((c and d) xor (not c and E)) + $C24B8B70 + w[42];
  tmp2 := (((g shr 2) or (g shl 30)) xor ((g shr 13) or (g shl 19)) xor ((g shr 22) xor (g shl 10))) + ((g and h) xor (g and a) xor (h and a));
  f := tmp1 + tmp2;
  b := b + tmp1;
  tmp1 := E + (((b shr 6) or (b shl 26)) xor ((b shr 11) or (b shl 21)) xor ((b shr 25) or (b shl 7))) + ((b and c) xor (not b and d)) + $C76C51A3 + w[43];
  tmp2 := (((f shr 2) or (f shl 30)) xor ((f shr 13) or (f shl 19)) xor ((f shr 22) xor (f shl 10))) + ((f and g) xor (f and h) xor (g and h));
  E := tmp1 + tmp2;
  a := a + tmp1;
  tmp1 := d + (((a shr 6) or (a shl 26)) xor ((a shr 11) or (a shl 21)) xor ((a shr 25) or (a shl 7))) + ((a and b) xor (not a and c)) + $D192E819 + w[44];
  tmp2 := (((E shr 2) or (E shl 30)) xor ((E shr 13) or (E shl 19)) xor ((E shr 22) xor (E shl 10))) + ((E and f) xor (E and g) xor (f and g));
  d := tmp1 + tmp2;
  h := h + tmp1;
  tmp1 := c + (((h shr 6) or (h shl 26)) xor ((h shr 11) or (h shl 21)) xor ((h shr 25) or (h shl 7))) + ((h and a) xor (not h and b)) + $D6990624 + w[45];
  tmp2 := (((d shr 2) or (d shl 30)) xor ((d shr 13) or (d shl 19)) xor ((d shr 22) xor (d shl 10))) + ((d and E) xor (d and f) xor (E and f));
  c := tmp1 + tmp2;
  g := g + tmp1;
  tmp1 := b + (((g shr 6) or (g shl 26)) xor ((g shr 11) or (g shl 21)) xor ((g shr 25) or (g shl 7))) + ((g and h) xor (not g and a)) + $F40E3585 + w[46];
  tmp2 := (((c shr 2) or (c shl 30)) xor ((c shr 13) or (c shl 19)) xor ((c shr 22) xor (c shl 10))) + ((c and d) xor (c and E) xor (d and E));
  b := tmp1 + tmp2;
  f := f + tmp1;
  tmp1 := a + (((f shr 6) or (f shl 26)) xor ((f shr 11) or (f shl 21)) xor ((f shr 25) or (f shl 7))) + ((f and g) xor (not f and h)) + $106AA070 + w[47];
  tmp2 := (((b shr 2) or (b shl 30)) xor ((b shr 13) or (b shl 19)) xor ((b shr 22) xor (b shl 10))) + ((b and c) xor (b and d) xor (c and d));
  a := tmp1 + tmp2;
  E := E + tmp1;
  tmp1 := h + (((E shr 6) or (E shl 26)) xor ((E shr 11) or (E shl 21)) xor ((E shr 25) or (E shl 7))) + ((E and f) xor (not E and g)) + $19A4C116 + w[48];
  tmp2 := (((a shr 2) or (a shl 30)) xor ((a shr 13) or (a shl 19)) xor ((a shr 22) xor (a shl 10))) + ((a and b) xor (a and c) xor (b and c));
  h := tmp1 + tmp2;
  d := d + tmp1;
  tmp1 := g + (((d shr 6) or (d shl 26)) xor ((d shr 11) or (d shl 21)) xor ((d shr 25) or (d shl 7))) + ((d and E) xor (not d and f)) + $1E376C08 + w[49];
  tmp2 := (((h shr 2) or (h shl 30)) xor ((h shr 13) or (h shl 19)) xor ((h shr 22) xor (h shl 10))) + ((h and a) xor (h and b) xor (a and b));
  g := tmp1 + tmp2;
  c := c + tmp1;
  tmp1 := f + (((c shr 6) or (c shl 26)) xor ((c shr 11) or (c shl 21)) xor ((c shr 25) or (c shl 7))) + ((c and d) xor (not c and E)) + $2748774C + w[50];
  tmp2 := (((g shr 2) or (g shl 30)) xor ((g shr 13) or (g shl 19)) xor ((g shr 22) xor (g shl 10))) + ((g and h) xor (g and a) xor (h and a));
  f := tmp1 + tmp2;
  b := b + tmp1;
  tmp1 := E + (((b shr 6) or (b shl 26)) xor ((b shr 11) or (b shl 21)) xor ((b shr 25) or (b shl 7))) + ((b and c) xor (not b and d)) + $34B0BCB5 + w[51];
  tmp2 := (((f shr 2) or (f shl 30)) xor ((f shr 13) or (f shl 19)) xor ((f shr 22) xor (f shl 10))) + ((f and g) xor (f and h) xor (g and h));
  E := tmp1 + tmp2;
  a := a + tmp1;
  tmp1 := d + (((a shr 6) or (a shl 26)) xor ((a shr 11) or (a shl 21)) xor ((a shr 25) or (a shl 7))) + ((a and b) xor (not a and c)) + $391C0CB3 + w[52];
  tmp2 := (((E shr 2) or (E shl 30)) xor ((E shr 13) or (E shl 19)) xor ((E shr 22) xor (E shl 10))) + ((E and f) xor (E and g) xor (f and g));
  d := tmp1 + tmp2;
  h := h + tmp1;
  tmp1 := c + (((h shr 6) or (h shl 26)) xor ((h shr 11) or (h shl 21)) xor ((h shr 25) or (h shl 7))) + ((h and a) xor (not h and b)) + $4ED8AA4A + w[53];
  tmp2 := (((d shr 2) or (d shl 30)) xor ((d shr 13) or (d shl 19)) xor ((d shr 22) xor (d shl 10))) + ((d and E) xor (d and f) xor (E and f));
  c := tmp1 + tmp2;
  g := g + tmp1;
  tmp1 := b + (((g shr 6) or (g shl 26)) xor ((g shr 11) or (g shl 21)) xor ((g shr 25) or (g shl 7))) + ((g and h) xor (not g and a)) + $5B9CCA4F + w[54];
  tmp2 := (((c shr 2) or (c shl 30)) xor ((c shr 13) or (c shl 19)) xor ((c shr 22) xor (c shl 10))) + ((c and d) xor (c and E) xor (d and E));
  b := tmp1 + tmp2;
  f := f + tmp1;
  tmp1 := a + (((f shr 6) or (f shl 26)) xor ((f shr 11) or (f shl 21)) xor ((f shr 25) or (f shl 7))) + ((f and g) xor (not f and h)) + $682E6FF3 + w[55];
  tmp2 := (((b shr 2) or (b shl 30)) xor ((b shr 13) or (b shl 19)) xor ((b shr 22) xor (b shl 10))) + ((b and c) xor (b and d) xor (c and d));
  a := tmp1 + tmp2;
  E := E + tmp1;
  tmp1 := h + (((E shr 6) or (E shl 26)) xor ((E shr 11) or (E shl 21)) xor ((E shr 25) or (E shl 7))) + ((E and f) xor (not E and g)) + $748F82EE + w[56];
  tmp2 := (((a shr 2) or (a shl 30)) xor ((a shr 13) or (a shl 19)) xor ((a shr 22) xor (a shl 10))) + ((a and b) xor (a and c) xor (b and c));
  h := tmp1 + tmp2;
  d := d + tmp1;
  tmp1 := g + (((d shr 6) or (d shl 26)) xor ((d shr 11) or (d shl 21)) xor ((d shr 25) or (d shl 7))) + ((d and E) xor (not d and f)) + $78A5636F + w[57];
  tmp2 := (((h shr 2) or (h shl 30)) xor ((h shr 13) or (h shl 19)) xor ((h shr 22) xor (h shl 10))) + ((h and a) xor (h and b) xor (a and b));
  g := tmp1 + tmp2;
  c := c + tmp1;
  tmp1 := f + (((c shr 6) or (c shl 26)) xor ((c shr 11) or (c shl 21)) xor ((c shr 25) or (c shl 7))) + ((c and d) xor (not c and E)) + $84C87814 + w[58];
  tmp2 := (((g shr 2) or (g shl 30)) xor ((g shr 13) or (g shl 19)) xor ((g shr 22) xor (g shl 10))) + ((g and h) xor (g and a) xor (h and a));
  f := tmp1 + tmp2;
  b := b + tmp1;
  tmp1 := E + (((b shr 6) or (b shl 26)) xor ((b shr 11) or (b shl 21)) xor ((b shr 25) or (b shl 7))) + ((b and c) xor (not b and d)) + $8CC70208 + w[59];
  tmp2 := (((f shr 2) or (f shl 30)) xor ((f shr 13) or (f shl 19)) xor ((f shr 22) xor (f shl 10))) + ((f and g) xor (f and h) xor (g and h));
  E := tmp1 + tmp2;
  a := a + tmp1;
  tmp1 := d + (((a shr 6) or (a shl 26)) xor ((a shr 11) or (a shl 21)) xor ((a shr 25) or (a shl 7))) + ((a and b) xor (not a and c)) + $90BEFFFA + w[60];
  tmp2 := (((E shr 2) or (E shl 30)) xor ((E shr 13) or (E shl 19)) xor ((E shr 22) xor (E shl 10))) + ((E and f) xor (E and g) xor (f and g));
  d := tmp1 + tmp2;
  h := h + tmp1;
  tmp1 := c + (((h shr 6) or (h shl 26)) xor ((h shr 11) or (h shl 21)) xor ((h shr 25) or (h shl 7))) + ((h and a) xor (not h and b)) + $A4506CEB + w[61];
  tmp2 := (((d shr 2) or (d shl 30)) xor ((d shr 13) or (d shl 19)) xor ((d shr 22) xor (d shl 10))) + ((d and E) xor (d and f) xor (E and f));
  c := tmp1 + tmp2;
  g := g + tmp1;
  tmp1 := b + (((g shr 6) or (g shl 26)) xor ((g shr 11) or (g shl 21)) xor ((g shr 25) or (g shl 7))) + ((g and h) xor (not g and a)) + $BEF9A3F7 + w[62];
  tmp2 := (((c shr 2) or (c shl 30)) xor ((c shr 13) or (c shl 19)) xor ((c shr 22) xor (c shl 10))) + ((c and d) xor (c and E) xor (d and E));
  b := tmp1 + tmp2;
  f := f + tmp1;
  tmp1 := a + (((f shr 6) or (f shl 26)) xor ((f shr 11) or (f shl 21)) xor ((f shr 25) or (f shl 7))) + ((f and g) xor (not f and h)) + $C67178F2 + w[63];
  tmp2 := (((b shr 2) or (b shl 30)) xor ((b shr 13) or (b shl 19)) xor ((b shr 22) xor (b shl 10))) + ((b and c) xor (b and d) xor (c and d));
  a := tmp1 + tmp2;
  E := E + tmp1;

  inc(PDWORD(@Digest[0])^, a);
  inc(PDWORD(@Digest[4])^, b);
  inc(PDWORD(@Digest[8])^, c);
  inc(PDWORD(@Digest[12])^, d);
  inc(PDWORD(@Digest[16])^, E);
  inc(PDWORD(@Digest[20])^, f);
  inc(PDWORD(@Digest[24])^, g);
  inc(PDWORD(@Digest[28])^, h);
end;

class procedure TSHA256.SHA256(var Digest: TSHA256Digest; const Buf; BufSize: nativeUInt);
var
  Lo, Hi: DWORD;
  p: PByte;
  ChunkIndex: Byte;
  ChunkBuf: array [0 .. 63] of Byte;
begin
  PDWORD(@Digest[0])^ := $6A09E667;
  PDWORD(@Digest[4])^ := $BB67AE85;
  PDWORD(@Digest[8])^ := $3C6EF372;
  PDWORD(@Digest[12])^ := $A54FF53A;
  PDWORD(@Digest[16])^ := $510E527F;
  PDWORD(@Digest[20])^ := $9B05688C;
  PDWORD(@Digest[24])^ := $1F83D9AB;
  PDWORD(@Digest[28])^ := $5BE0CD19;

  Lo := 0;
  inc(Lo, BufSize shl 3);
  Hi := 0;
  inc(Hi, BufSize shr 29);

  SwapDWORD(Lo);
  SwapDWORD(Hi);

  p := @Buf;

  while BufSize >= 64 do
    begin
      Compute(Digest, p);
      inc(p, 64);
      dec(BufSize, 64);
    end;
  if BufSize > 0 then
      CopyPtr(p, @ChunkBuf[0], BufSize);

  ChunkBuf[BufSize] := $80;
  ChunkIndex := BufSize + 1;
  if ChunkIndex > 56 then
    begin
      if ChunkIndex < 64 then
          FillPtrByte(@ChunkBuf[ChunkIndex], 64 - ChunkIndex, 0);
      Compute(Digest, @ChunkBuf);
      ChunkIndex := 0
    end;
  FillPtrByte(@ChunkBuf[ChunkIndex], 56 - ChunkIndex, 0);
  PDWORD(@ChunkBuf[56])^ := Hi;
  PDWORD(@ChunkBuf[60])^ := Lo;
  Compute(Digest, @ChunkBuf);

  SwapDWORD(PDWORD(@Digest[0])^);
  SwapDWORD(PDWORD(@Digest[4])^);
  SwapDWORD(PDWORD(@Digest[8])^);
  SwapDWORD(PDWORD(@Digest[12])^);
  SwapDWORD(PDWORD(@Digest[16])^);
  SwapDWORD(PDWORD(@Digest[20])^);
  SwapDWORD(PDWORD(@Digest[24])^);
  SwapDWORD(PDWORD(@Digest[28])^);
end;

class procedure TSHA512.SwapQWORD(var a: UInt64);
begin
  a := Endian(a);
end;

class procedure TSHA512.Compute(var Digest: TSHA512Digest; const buff: Pointer);
var
  a, b, c, d, E, f, g, h, T1, T2: UInt64;
  w: array [0 .. 79] of UInt64;
  i: ShortInt;
begin
{$IFDEF RangeCheck}{$R-}{$ENDIF}
  a := PUInt64(@Digest[0])^;
  b := PUInt64(@Digest[8])^;
  c := PUInt64(@Digest[16])^;
  d := PUInt64(@Digest[24])^;
  E := PUInt64(@Digest[32])^;
  f := PUInt64(@Digest[40])^;
  g := PUInt64(@Digest[48])^;
  h := PUInt64(@Digest[56])^;

  CopyPtr(buff, @w[0], 128);
  for i := 0 to 15 do
      SwapQWORD(w[i]);

  for i := 16 to 79 do
      w[i] := (((w[i - 2] shr 19) or (w[i - 2] shl 45)) xor ((w[i - 2] shr 61) or (w[i - 2] shl 3)) xor (w[i - 2] shr 6)) +
      w[i - 7] + (((w[i - 15] shr 1) or (w[i - 15] shl 63)) xor ((w[i - 15] shr 8) or (w[i - 15] shl 56)) xor (w[i - 15] shr 7)) + w[i - 16];

  T1 := h + (((E shr 14) or (E shl 50)) xor ((E shr 18) or (E shl 46)) xor ((E shr 41) or (E shl 23))) + ((E and f) xor (not E and g)) + $428A2F98D728AE22 + w[0];
  T2 := (((a shr 28) or (a shl 36)) xor ((a shr 34) or (a shl 30)) xor ((a shr 39) or (a shl 25))) + ((a and b) xor (a and c) xor (b and c));
  d := d + T1;
  h := T1 + T2;
  T1 := g + (((d shr 14) or (d shl 50)) xor ((d shr 18) or (d shl 46)) xor ((d shr 41) or (d shl 23))) + ((d and E) xor (not d and f)) + $7137449123EF65CD + w[1];
  T2 := (((h shr 28) or (h shl 36)) xor ((h shr 34) or (h shl 30)) xor ((h shr 39) or (h shl 25))) + ((h and a) xor (h and b) xor (a and b));
  c := c + T1;
  g := T1 + T2;
  T1 := f + (((c shr 14) or (c shl 50)) xor ((c shr 18) or (c shl 46)) xor ((c shr 41) or (c shl 23))) + ((c and d) xor (not c and E)) + $B5C0FBCFEC4D3B2F + w[2];
  T2 := (((g shr 28) or (g shl 36)) xor ((g shr 34) or (g shl 30)) xor ((g shr 39) or (g shl 25))) + ((g and h) xor (g and a) xor (h and a));
  b := b + T1;
  f := T1 + T2;
  T1 := E + (((b shr 14) or (b shl 50)) xor ((b shr 18) or (b shl 46)) xor ((b shr 41) or (b shl 23))) + ((b and c) xor (not b and d)) + $E9B5DBA58189DBBC + w[3];
  T2 := (((f shr 28) or (f shl 36)) xor ((f shr 34) or (f shl 30)) xor ((f shr 39) or (f shl 25))) + ((f and g) xor (f and h) xor (g and h));
  a := a + T1;
  E := T1 + T2;
  T1 := d + (((a shr 14) or (a shl 50)) xor ((a shr 18) or (a shl 46)) xor ((a shr 41) or (a shl 23))) + ((a and b) xor (not a and c)) + $3956C25BF348B538 + w[4];
  T2 := (((E shr 28) or (E shl 36)) xor ((E shr 34) or (E shl 30)) xor ((E shr 39) or (E shl 25))) + ((E and f) xor (E and g) xor (f and g));
  h := h + T1;
  d := T1 + T2;
  T1 := c + (((h shr 14) or (h shl 50)) xor ((h shr 18) or (h shl 46)) xor ((h shr 41) or (h shl 23))) + ((h and a) xor (not h and b)) + $59F111F1B605D019 + w[5];
  T2 := (((d shr 28) or (d shl 36)) xor ((d shr 34) or (d shl 30)) xor ((d shr 39) or (d shl 25))) + ((d and E) xor (d and f) xor (E and f));
  g := g + T1;
  c := T1 + T2;
  T1 := b + (((g shr 14) or (g shl 50)) xor ((g shr 18) or (g shl 46)) xor ((g shr 41) or (g shl 23))) + ((g and h) xor (not g and a)) + $923F82A4AF194F9B + w[6];
  T2 := (((c shr 28) or (c shl 36)) xor ((c shr 34) or (c shl 30)) xor ((c shr 39) or (c shl 25))) + ((c and d) xor (c and E) xor (d and E));
  f := f + T1;
  b := T1 + T2;
  T1 := a + (((f shr 14) or (f shl 50)) xor ((f shr 18) or (f shl 46)) xor ((f shr 41) or (f shl 23))) + ((f and g) xor (not f and h)) + $AB1C5ED5DA6D8118 + w[7];
  T2 := (((b shr 28) or (b shl 36)) xor ((b shr 34) or (b shl 30)) xor ((b shr 39) or (b shl 25))) + ((b and c) xor (b and d) xor (c and d));
  E := E + T1;
  a := T1 + T2;
  T1 := h + (((E shr 14) or (E shl 50)) xor ((E shr 18) or (E shl 46)) xor ((E shr 41) or (E shl 23))) + ((E and f) xor (not E and g)) + $D807AA98A3030242 + w[8];
  T2 := (((a shr 28) or (a shl 36)) xor ((a shr 34) or (a shl 30)) xor ((a shr 39) or (a shl 25))) + ((a and b) xor (a and c) xor (b and c));
  d := d + T1;
  h := T1 + T2;
  T1 := g + (((d shr 14) or (d shl 50)) xor ((d shr 18) or (d shl 46)) xor ((d shr 41) or (d shl 23))) + ((d and E) xor (not d and f)) + $12835B0145706FBE + w[9];
  T2 := (((h shr 28) or (h shl 36)) xor ((h shr 34) or (h shl 30)) xor ((h shr 39) or (h shl 25))) + ((h and a) xor (h and b) xor (a and b));
  c := c + T1;
  g := T1 + T2;
  T1 := f + (((c shr 14) or (c shl 50)) xor ((c shr 18) or (c shl 46)) xor ((c shr 41) or (c shl 23))) + ((c and d) xor (not c and E)) + $243185BE4EE4B28C + w[10];
  T2 := (((g shr 28) or (g shl 36)) xor ((g shr 34) or (g shl 30)) xor ((g shr 39) or (g shl 25))) + ((g and h) xor (g and a) xor (h and a));
  b := b + T1;
  f := T1 + T2;
  T1 := E + (((b shr 14) or (b shl 50)) xor ((b shr 18) or (b shl 46)) xor ((b shr 41) or (b shl 23))) + ((b and c) xor (not b and d)) + $550C7DC3D5FFB4E2 + w[11];
  T2 := (((f shr 28) or (f shl 36)) xor ((f shr 34) or (f shl 30)) xor ((f shr 39) or (f shl 25))) + ((f and g) xor (f and h) xor (g and h));
  a := a + T1;
  E := T1 + T2;
  T1 := d + (((a shr 14) or (a shl 50)) xor ((a shr 18) or (a shl 46)) xor ((a shr 41) or (a shl 23))) + ((a and b) xor (not a and c)) + $72BE5D74F27B896F + w[12];
  T2 := (((E shr 28) or (E shl 36)) xor ((E shr 34) or (E shl 30)) xor ((E shr 39) or (E shl 25))) + ((E and f) xor (E and g) xor (f and g));
  h := h + T1;
  d := T1 + T2;
  T1 := c + (((h shr 14) or (h shl 50)) xor ((h shr 18) or (h shl 46)) xor ((h shr 41) or (h shl 23))) + ((h and a) xor (not h and b)) + $80DEB1FE3B1696B1 + w[13];
  T2 := (((d shr 28) or (d shl 36)) xor ((d shr 34) or (d shl 30)) xor ((d shr 39) or (d shl 25))) + ((d and E) xor (d and f) xor (E and f));
  g := g + T1;
  c := T1 + T2;
  T1 := b + (((g shr 14) or (g shl 50)) xor ((g shr 18) or (g shl 46)) xor ((g shr 41) or (g shl 23))) + ((g and h) xor (not g and a)) + $9BDC06A725C71235 + w[14];
  T2 := (((c shr 28) or (c shl 36)) xor ((c shr 34) or (c shl 30)) xor ((c shr 39) or (c shl 25))) + ((c and d) xor (c and E) xor (d and E));
  f := f + T1;
  b := T1 + T2;
  T1 := a + (((f shr 14) or (f shl 50)) xor ((f shr 18) or (f shl 46)) xor ((f shr 41) or (f shl 23))) + ((f and g) xor (not f and h)) + $C19BF174CF692694 + w[15];
  T2 := (((b shr 28) or (b shl 36)) xor ((b shr 34) or (b shl 30)) xor ((b shr 39) or (b shl 25))) + ((b and c) xor (b and d) xor (c and d));
  E := E + T1;
  a := T1 + T2;
  T1 := h + (((E shr 14) or (E shl 50)) xor ((E shr 18) or (E shl 46)) xor ((E shr 41) or (E shl 23))) + ((E and f) xor (not E and g)) + $E49B69C19EF14AD2 + w[16];
  T2 := (((a shr 28) or (a shl 36)) xor ((a shr 34) or (a shl 30)) xor ((a shr 39) or (a shl 25))) + ((a and b) xor (a and c) xor (b and c));
  d := d + T1;
  h := T1 + T2;
  T1 := g + (((d shr 14) or (d shl 50)) xor ((d shr 18) or (d shl 46)) xor ((d shr 41) or (d shl 23))) + ((d and E) xor (not d and f)) + $EFBE4786384F25E3 + w[17];
  T2 := (((h shr 28) or (h shl 36)) xor ((h shr 34) or (h shl 30)) xor ((h shr 39) or (h shl 25))) + ((h and a) xor (h and b) xor (a and b));
  c := c + T1;
  g := T1 + T2;
  T1 := f + (((c shr 14) or (c shl 50)) xor ((c shr 18) or (c shl 46)) xor ((c shr 41) or (c shl 23))) + ((c and d) xor (not c and E)) + $0FC19DC68B8CD5B5 + w[18];
  T2 := (((g shr 28) or (g shl 36)) xor ((g shr 34) or (g shl 30)) xor ((g shr 39) or (g shl 25))) + ((g and h) xor (g and a) xor (h and a));
  b := b + T1;
  f := T1 + T2;
  T1 := E + (((b shr 14) or (b shl 50)) xor ((b shr 18) or (b shl 46)) xor ((b shr 41) or (b shl 23))) + ((b and c) xor (not b and d)) + $240CA1CC77AC9C65 + w[19];
  T2 := (((f shr 28) or (f shl 36)) xor ((f shr 34) or (f shl 30)) xor ((f shr 39) or (f shl 25))) + ((f and g) xor (f and h) xor (g and h));
  a := a + T1;
  E := T1 + T2;
  T1 := d + (((a shr 14) or (a shl 50)) xor ((a shr 18) or (a shl 46)) xor ((a shr 41) or (a shl 23))) + ((a and b) xor (not a and c)) + $2DE92C6F592B0275 + w[20];
  T2 := (((E shr 28) or (E shl 36)) xor ((E shr 34) or (E shl 30)) xor ((E shr 39) or (E shl 25))) + ((E and f) xor (E and g) xor (f and g));
  h := h + T1;
  d := T1 + T2;
  T1 := c + (((h shr 14) or (h shl 50)) xor ((h shr 18) or (h shl 46)) xor ((h shr 41) or (h shl 23))) + ((h and a) xor (not h and b)) + $4A7484AA6EA6E483 + w[21];
  T2 := (((d shr 28) or (d shl 36)) xor ((d shr 34) or (d shl 30)) xor ((d shr 39) or (d shl 25))) + ((d and E) xor (d and f) xor (E and f));
  g := g + T1;
  c := T1 + T2;
  T1 := b + (((g shr 14) or (g shl 50)) xor ((g shr 18) or (g shl 46)) xor ((g shr 41) or (g shl 23))) + ((g and h) xor (not g and a)) + $5CB0A9DCBD41FBD4 + w[22];
  T2 := (((c shr 28) or (c shl 36)) xor ((c shr 34) or (c shl 30)) xor ((c shr 39) or (c shl 25))) + ((c and d) xor (c and E) xor (d and E));
  f := f + T1;
  b := T1 + T2;
  T1 := a + (((f shr 14) or (f shl 50)) xor ((f shr 18) or (f shl 46)) xor ((f shr 41) or (f shl 23))) + ((f and g) xor (not f and h)) + $76F988DA831153B5 + w[23];
  T2 := (((b shr 28) or (b shl 36)) xor ((b shr 34) or (b shl 30)) xor ((b shr 39) or (b shl 25))) + ((b and c) xor (b and d) xor (c and d));
  E := E + T1;
  a := T1 + T2;
  T1 := h + (((E shr 14) or (E shl 50)) xor ((E shr 18) or (E shl 46)) xor ((E shr 41) or (E shl 23))) + ((E and f) xor (not E and g)) + $983E5152EE66DFAB + w[24];
  T2 := (((a shr 28) or (a shl 36)) xor ((a shr 34) or (a shl 30)) xor ((a shr 39) or (a shl 25))) + ((a and b) xor (a and c) xor (b and c));
  d := d + T1;
  h := T1 + T2;
  T1 := g + (((d shr 14) or (d shl 50)) xor ((d shr 18) or (d shl 46)) xor ((d shr 41) or (d shl 23))) + ((d and E) xor (not d and f)) + $A831C66D2DB43210 + w[25];
  T2 := (((h shr 28) or (h shl 36)) xor ((h shr 34) or (h shl 30)) xor ((h shr 39) or (h shl 25))) + ((h and a) xor (h and b) xor (a and b));
  c := c + T1;
  g := T1 + T2;
  T1 := f + (((c shr 14) or (c shl 50)) xor ((c shr 18) or (c shl 46)) xor ((c shr 41) or (c shl 23))) + ((c and d) xor (not c and E)) + $B00327C898FB213F + w[26];
  T2 := (((g shr 28) or (g shl 36)) xor ((g shr 34) or (g shl 30)) xor ((g shr 39) or (g shl 25))) + ((g and h) xor (g and a) xor (h and a));
  b := b + T1;
  f := T1 + T2;
  T1 := E + (((b shr 14) or (b shl 50)) xor ((b shr 18) or (b shl 46)) xor ((b shr 41) or (b shl 23))) + ((b and c) xor (not b and d)) + $BF597FC7BEEF0EE4 + w[27];
  T2 := (((f shr 28) or (f shl 36)) xor ((f shr 34) or (f shl 30)) xor ((f shr 39) or (f shl 25))) + ((f and g) xor (f and h) xor (g and h));
  a := a + T1;
  E := T1 + T2;
  T1 := d + (((a shr 14) or (a shl 50)) xor ((a shr 18) or (a shl 46)) xor ((a shr 41) or (a shl 23))) + ((a and b) xor (not a and c)) + $C6E00BF33DA88FC2 + w[28];
  T2 := (((E shr 28) or (E shl 36)) xor ((E shr 34) or (E shl 30)) xor ((E shr 39) or (E shl 25))) + ((E and f) xor (E and g) xor (f and g));
  h := h + T1;
  d := T1 + T2;
  T1 := c + (((h shr 14) or (h shl 50)) xor ((h shr 18) or (h shl 46)) xor ((h shr 41) or (h shl 23))) + ((h and a) xor (not h and b)) + $D5A79147930AA725 + w[29];
  T2 := (((d shr 28) or (d shl 36)) xor ((d shr 34) or (d shl 30)) xor ((d shr 39) or (d shl 25))) + ((d and E) xor (d and f) xor (E and f));
  g := g + T1;
  c := T1 + T2;
  T1 := b + (((g shr 14) or (g shl 50)) xor ((g shr 18) or (g shl 46)) xor ((g shr 41) or (g shl 23))) + ((g and h) xor (not g and a)) + $06CA6351E003826F + w[30];
  T2 := (((c shr 28) or (c shl 36)) xor ((c shr 34) or (c shl 30)) xor ((c shr 39) or (c shl 25))) + ((c and d) xor (c and E) xor (d and E));
  f := f + T1;
  b := T1 + T2;
  T1 := a + (((f shr 14) or (f shl 50)) xor ((f shr 18) or (f shl 46)) xor ((f shr 41) or (f shl 23))) + ((f and g) xor (not f and h)) + $142929670A0E6E70 + w[31];
  T2 := (((b shr 28) or (b shl 36)) xor ((b shr 34) or (b shl 30)) xor ((b shr 39) or (b shl 25))) + ((b and c) xor (b and d) xor (c and d));
  E := E + T1;
  a := T1 + T2;
  T1 := h + (((E shr 14) or (E shl 50)) xor ((E shr 18) or (E shl 46)) xor ((E shr 41) or (E shl 23))) + ((E and f) xor (not E and g)) + $27B70A8546D22FFC + w[32];
  T2 := (((a shr 28) or (a shl 36)) xor ((a shr 34) or (a shl 30)) xor ((a shr 39) or (a shl 25))) + ((a and b) xor (a and c) xor (b and c));
  d := d + T1;
  h := T1 + T2;
  T1 := g + (((d shr 14) or (d shl 50)) xor ((d shr 18) or (d shl 46)) xor ((d shr 41) or (d shl 23))) + ((d and E) xor (not d and f)) + $2E1B21385C26C926 + w[33];
  T2 := (((h shr 28) or (h shl 36)) xor ((h shr 34) or (h shl 30)) xor ((h shr 39) or (h shl 25))) + ((h and a) xor (h and b) xor (a and b));
  c := c + T1;
  g := T1 + T2;
  T1 := f + (((c shr 14) or (c shl 50)) xor ((c shr 18) or (c shl 46)) xor ((c shr 41) or (c shl 23))) + ((c and d) xor (not c and E)) + $4D2C6DFC5AC42AED + w[34];
  T2 := (((g shr 28) or (g shl 36)) xor ((g shr 34) or (g shl 30)) xor ((g shr 39) or (g shl 25))) + ((g and h) xor (g and a) xor (h and a));
  b := b + T1;
  f := T1 + T2;
  T1 := E + (((b shr 14) or (b shl 50)) xor ((b shr 18) or (b shl 46)) xor ((b shr 41) or (b shl 23))) + ((b and c) xor (not b and d)) + $53380D139D95B3DF + w[35];
  T2 := (((f shr 28) or (f shl 36)) xor ((f shr 34) or (f shl 30)) xor ((f shr 39) or (f shl 25))) + ((f and g) xor (f and h) xor (g and h));
  a := a + T1;
  E := T1 + T2;
  T1 := d + (((a shr 14) or (a shl 50)) xor ((a shr 18) or (a shl 46)) xor ((a shr 41) or (a shl 23))) + ((a and b) xor (not a and c)) + $650A73548BAF63DE + w[36];
  T2 := (((E shr 28) or (E shl 36)) xor ((E shr 34) or (E shl 30)) xor ((E shr 39) or (E shl 25))) + ((E and f) xor (E and g) xor (f and g));
  h := h + T1;
  d := T1 + T2;
  T1 := c + (((h shr 14) or (h shl 50)) xor ((h shr 18) or (h shl 46)) xor ((h shr 41) or (h shl 23))) + ((h and a) xor (not h and b)) + $766A0ABB3C77B2A8 + w[37];
  T2 := (((d shr 28) or (d shl 36)) xor ((d shr 34) or (d shl 30)) xor ((d shr 39) or (d shl 25))) + ((d and E) xor (d and f) xor (E and f));
  g := g + T1;
  c := T1 + T2;
  T1 := b + (((g shr 14) or (g shl 50)) xor ((g shr 18) or (g shl 46)) xor ((g shr 41) or (g shl 23))) + ((g and h) xor (not g and a)) + $81C2C92E47EDAEE6 + w[38];
  T2 := (((c shr 28) or (c shl 36)) xor ((c shr 34) or (c shl 30)) xor ((c shr 39) or (c shl 25))) + ((c and d) xor (c and E) xor (d and E));
  f := f + T1;
  b := T1 + T2;
  T1 := a + (((f shr 14) or (f shl 50)) xor ((f shr 18) or (f shl 46)) xor ((f shr 41) or (f shl 23))) + ((f and g) xor (not f and h)) + $92722C851482353B + w[39];
  T2 := (((b shr 28) or (b shl 36)) xor ((b shr 34) or (b shl 30)) xor ((b shr 39) or (b shl 25))) + ((b and c) xor (b and d) xor (c and d));
  E := E + T1;
  a := T1 + T2;
  T1 := h + (((E shr 14) or (E shl 50)) xor ((E shr 18) or (E shl 46)) xor ((E shr 41) or (E shl 23))) + ((E and f) xor (not E and g)) + $A2BFE8A14CF10364 + w[40];
  T2 := (((a shr 28) or (a shl 36)) xor ((a shr 34) or (a shl 30)) xor ((a shr 39) or (a shl 25))) + ((a and b) xor (a and c) xor (b and c));
  d := d + T1;
  h := T1 + T2;
  T1 := g + (((d shr 14) or (d shl 50)) xor ((d shr 18) or (d shl 46)) xor ((d shr 41) or (d shl 23))) + ((d and E) xor (not d and f)) + $A81A664BBC423001 + w[41];
  T2 := (((h shr 28) or (h shl 36)) xor ((h shr 34) or (h shl 30)) xor ((h shr 39) or (h shl 25))) + ((h and a) xor (h and b) xor (a and b));
  c := c + T1;
  g := T1 + T2;
  T1 := f + (((c shr 14) or (c shl 50)) xor ((c shr 18) or (c shl 46)) xor ((c shr 41) or (c shl 23))) + ((c and d) xor (not c and E)) + $C24B8B70D0F89791 + w[42];
  T2 := (((g shr 28) or (g shl 36)) xor ((g shr 34) or (g shl 30)) xor ((g shr 39) or (g shl 25))) + ((g and h) xor (g and a) xor (h and a));
  b := b + T1;
  f := T1 + T2;
  T1 := E + (((b shr 14) or (b shl 50)) xor ((b shr 18) or (b shl 46)) xor ((b shr 41) or (b shl 23))) + ((b and c) xor (not b and d)) + $C76C51A30654BE30 + w[43];
  T2 := (((f shr 28) or (f shl 36)) xor ((f shr 34) or (f shl 30)) xor ((f shr 39) or (f shl 25))) + ((f and g) xor (f and h) xor (g and h));
  a := a + T1;
  E := T1 + T2;
  T1 := d + (((a shr 14) or (a shl 50)) xor ((a shr 18) or (a shl 46)) xor ((a shr 41) or (a shl 23))) + ((a and b) xor (not a and c)) + $D192E819D6EF5218 + w[44];
  T2 := (((E shr 28) or (E shl 36)) xor ((E shr 34) or (E shl 30)) xor ((E shr 39) or (E shl 25))) + ((E and f) xor (E and g) xor (f and g));
  h := h + T1;
  d := T1 + T2;
  T1 := c + (((h shr 14) or (h shl 50)) xor ((h shr 18) or (h shl 46)) xor ((h shr 41) or (h shl 23))) + ((h and a) xor (not h and b)) + $D69906245565A910 + w[45];
  T2 := (((d shr 28) or (d shl 36)) xor ((d shr 34) or (d shl 30)) xor ((d shr 39) or (d shl 25))) + ((d and E) xor (d and f) xor (E and f));
  g := g + T1;
  c := T1 + T2;
  T1 := b + (((g shr 14) or (g shl 50)) xor ((g shr 18) or (g shl 46)) xor ((g shr 41) or (g shl 23))) + ((g and h) xor (not g and a)) + $F40E35855771202A + w[46];
  T2 := (((c shr 28) or (c shl 36)) xor ((c shr 34) or (c shl 30)) xor ((c shr 39) or (c shl 25))) + ((c and d) xor (c and E) xor (d and E));
  f := f + T1;
  b := T1 + T2;
  T1 := a + (((f shr 14) or (f shl 50)) xor ((f shr 18) or (f shl 46)) xor ((f shr 41) or (f shl 23))) + ((f and g) xor (not f and h)) + $106AA07032BBD1B8 + w[47];
  T2 := (((b shr 28) or (b shl 36)) xor ((b shr 34) or (b shl 30)) xor ((b shr 39) or (b shl 25))) + ((b and c) xor (b and d) xor (c and d));
  E := E + T1;
  a := T1 + T2;
  T1 := h + (((E shr 14) or (E shl 50)) xor ((E shr 18) or (E shl 46)) xor ((E shr 41) or (E shl 23))) + ((E and f) xor (not E and g)) + $19A4C116B8D2D0C8 + w[48];
  T2 := (((a shr 28) or (a shl 36)) xor ((a shr 34) or (a shl 30)) xor ((a shr 39) or (a shl 25))) + ((a and b) xor (a and c) xor (b and c));
  d := d + T1;
  h := T1 + T2;
  T1 := g + (((d shr 14) or (d shl 50)) xor ((d shr 18) or (d shl 46)) xor ((d shr 41) or (d shl 23))) + ((d and E) xor (not d and f)) + $1E376C085141AB53 + w[49];
  T2 := (((h shr 28) or (h shl 36)) xor ((h shr 34) or (h shl 30)) xor ((h shr 39) or (h shl 25))) + ((h and a) xor (h and b) xor (a and b));
  c := c + T1;
  g := T1 + T2;
  T1 := f + (((c shr 14) or (c shl 50)) xor ((c shr 18) or (c shl 46)) xor ((c shr 41) or (c shl 23))) + ((c and d) xor (not c and E)) + $2748774CDF8EEB99 + w[50];
  T2 := (((g shr 28) or (g shl 36)) xor ((g shr 34) or (g shl 30)) xor ((g shr 39) or (g shl 25))) + ((g and h) xor (g and a) xor (h and a));
  b := b + T1;
  f := T1 + T2;
  T1 := E + (((b shr 14) or (b shl 50)) xor ((b shr 18) or (b shl 46)) xor ((b shr 41) or (b shl 23))) + ((b and c) xor (not b and d)) + $34B0BCB5E19B48A8 + w[51];
  T2 := (((f shr 28) or (f shl 36)) xor ((f shr 34) or (f shl 30)) xor ((f shr 39) or (f shl 25))) + ((f and g) xor (f and h) xor (g and h));
  a := a + T1;
  E := T1 + T2;
  T1 := d + (((a shr 14) or (a shl 50)) xor ((a shr 18) or (a shl 46)) xor ((a shr 41) or (a shl 23))) + ((a and b) xor (not a and c)) + $391C0CB3C5C95A63 + w[52];
  T2 := (((E shr 28) or (E shl 36)) xor ((E shr 34) or (E shl 30)) xor ((E shr 39) or (E shl 25))) + ((E and f) xor (E and g) xor (f and g));
  h := h + T1;
  d := T1 + T2;
  T1 := c + (((h shr 14) or (h shl 50)) xor ((h shr 18) or (h shl 46)) xor ((h shr 41) or (h shl 23))) + ((h and a) xor (not h and b)) + $4ED8AA4AE3418ACB + w[53];
  T2 := (((d shr 28) or (d shl 36)) xor ((d shr 34) or (d shl 30)) xor ((d shr 39) or (d shl 25))) + ((d and E) xor (d and f) xor (E and f));
  g := g + T1;
  c := T1 + T2;
  T1 := b + (((g shr 14) or (g shl 50)) xor ((g shr 18) or (g shl 46)) xor ((g shr 41) or (g shl 23))) + ((g and h) xor (not g and a)) + $5B9CCA4F7763E373 + w[54];
  T2 := (((c shr 28) or (c shl 36)) xor ((c shr 34) or (c shl 30)) xor ((c shr 39) or (c shl 25))) + ((c and d) xor (c and E) xor (d and E));
  f := f + T1;
  b := T1 + T2;
  T1 := a + (((f shr 14) or (f shl 50)) xor ((f shr 18) or (f shl 46)) xor ((f shr 41) or (f shl 23))) + ((f and g) xor (not f and h)) + $682E6FF3D6B2B8A3 + w[55];
  T2 := (((b shr 28) or (b shl 36)) xor ((b shr 34) or (b shl 30)) xor ((b shr 39) or (b shl 25))) + ((b and c) xor (b and d) xor (c and d));
  E := E + T1;
  a := T1 + T2;
  T1 := h + (((E shr 14) or (E shl 50)) xor ((E shr 18) or (E shl 46)) xor ((E shr 41) or (E shl 23))) + ((E and f) xor (not E and g)) + $748F82EE5DEFB2FC + w[56];
  T2 := (((a shr 28) or (a shl 36)) xor ((a shr 34) or (a shl 30)) xor ((a shr 39) or (a shl 25))) + ((a and b) xor (a and c) xor (b and c));
  d := d + T1;
  h := T1 + T2;
  T1 := g + (((d shr 14) or (d shl 50)) xor ((d shr 18) or (d shl 46)) xor ((d shr 41) or (d shl 23))) + ((d and E) xor (not d and f)) + $78A5636F43172F60 + w[57];
  T2 := (((h shr 28) or (h shl 36)) xor ((h shr 34) or (h shl 30)) xor ((h shr 39) or (h shl 25))) + ((h and a) xor (h and b) xor (a and b));
  c := c + T1;
  g := T1 + T2;
  T1 := f + (((c shr 14) or (c shl 50)) xor ((c shr 18) or (c shl 46)) xor ((c shr 41) or (c shl 23))) + ((c and d) xor (not c and E)) + $84C87814A1F0AB72 + w[58];
  T2 := (((g shr 28) or (g shl 36)) xor ((g shr 34) or (g shl 30)) xor ((g shr 39) or (g shl 25))) + ((g and h) xor (g and a) xor (h and a));
  b := b + T1;
  f := T1 + T2;
  T1 := E + (((b shr 14) or (b shl 50)) xor ((b shr 18) or (b shl 46)) xor ((b shr 41) or (b shl 23))) + ((b and c) xor (not b and d)) + $8CC702081A6439EC + w[59];
  T2 := (((f shr 28) or (f shl 36)) xor ((f shr 34) or (f shl 30)) xor ((f shr 39) or (f shl 25))) + ((f and g) xor (f and h) xor (g and h));
  a := a + T1;
  E := T1 + T2;
  T1 := d + (((a shr 14) or (a shl 50)) xor ((a shr 18) or (a shl 46)) xor ((a shr 41) or (a shl 23))) + ((a and b) xor (not a and c)) + $90BEFFFA23631E28 + w[60];
  T2 := (((E shr 28) or (E shl 36)) xor ((E shr 34) or (E shl 30)) xor ((E shr 39) or (E shl 25))) + ((E and f) xor (E and g) xor (f and g));
  h := h + T1;
  d := T1 + T2;
  T1 := c + (((h shr 14) or (h shl 50)) xor ((h shr 18) or (h shl 46)) xor ((h shr 41) or (h shl 23))) + ((h and a) xor (not h and b)) + $A4506CEBDE82BDE9 + w[61];
  T2 := (((d shr 28) or (d shl 36)) xor ((d shr 34) or (d shl 30)) xor ((d shr 39) or (d shl 25))) + ((d and E) xor (d and f) xor (E and f));
  g := g + T1;
  c := T1 + T2;
  T1 := b + (((g shr 14) or (g shl 50)) xor ((g shr 18) or (g shl 46)) xor ((g shr 41) or (g shl 23))) + ((g and h) xor (not g and a)) + $BEF9A3F7B2C67915 + w[62];
  T2 := (((c shr 28) or (c shl 36)) xor ((c shr 34) or (c shl 30)) xor ((c shr 39) or (c shl 25))) + ((c and d) xor (c and E) xor (d and E));
  f := f + T1;
  b := T1 + T2;
  T1 := a + (((f shr 14) or (f shl 50)) xor ((f shr 18) or (f shl 46)) xor ((f shr 41) or (f shl 23))) + ((f and g) xor (not f and h)) + $C67178F2E372532B + w[63];
  T2 := (((b shr 28) or (b shl 36)) xor ((b shr 34) or (b shl 30)) xor ((b shr 39) or (b shl 25))) + ((b and c) xor (b and d) xor (c and d));
  E := E + T1;
  a := T1 + T2;
  T1 := h + (((E shr 14) or (E shl 50)) xor ((E shr 18) or (E shl 46)) xor ((E shr 41) or (E shl 23))) + ((E and f) xor (not E and g)) + $CA273ECEEA26619C + w[64];
  T2 := (((a shr 28) or (a shl 36)) xor ((a shr 34) or (a shl 30)) xor ((a shr 39) or (a shl 25))) + ((a and b) xor (a and c) xor (b and c));
  d := d + T1;
  h := T1 + T2;
  T1 := g + (((d shr 14) or (d shl 50)) xor ((d shr 18) or (d shl 46)) xor ((d shr 41) or (d shl 23))) + ((d and E) xor (not d and f)) + $D186B8C721C0C207 + w[65];
  T2 := (((h shr 28) or (h shl 36)) xor ((h shr 34) or (h shl 30)) xor ((h shr 39) or (h shl 25))) + ((h and a) xor (h and b) xor (a and b));
  c := c + T1;
  g := T1 + T2;
  T1 := f + (((c shr 14) or (c shl 50)) xor ((c shr 18) or (c shl 46)) xor ((c shr 41) or (c shl 23))) + ((c and d) xor (not c and E)) + $EADA7DD6CDE0EB1E + w[66];
  T2 := (((g shr 28) or (g shl 36)) xor ((g shr 34) or (g shl 30)) xor ((g shr 39) or (g shl 25))) + ((g and h) xor (g and a) xor (h and a));
  b := b + T1;
  f := T1 + T2;
  T1 := E + (((b shr 14) or (b shl 50)) xor ((b shr 18) or (b shl 46)) xor ((b shr 41) or (b shl 23))) + ((b and c) xor (not b and d)) + $F57D4F7FEE6ED178 + w[67];
  T2 := (((f shr 28) or (f shl 36)) xor ((f shr 34) or (f shl 30)) xor ((f shr 39) or (f shl 25))) + ((f and g) xor (f and h) xor (g and h));
  a := a + T1;
  E := T1 + T2;
  T1 := d + (((a shr 14) or (a shl 50)) xor ((a shr 18) or (a shl 46)) xor ((a shr 41) or (a shl 23))) + ((a and b) xor (not a and c)) + $06F067AA72176FBA + w[68];
  T2 := (((E shr 28) or (E shl 36)) xor ((E shr 34) or (E shl 30)) xor ((E shr 39) or (E shl 25))) + ((E and f) xor (E and g) xor (f and g));
  h := h + T1;
  d := T1 + T2;
  T1 := c + (((h shr 14) or (h shl 50)) xor ((h shr 18) or (h shl 46)) xor ((h shr 41) or (h shl 23))) + ((h and a) xor (not h and b)) + $0A637DC5A2C898A6 + w[69];
  T2 := (((d shr 28) or (d shl 36)) xor ((d shr 34) or (d shl 30)) xor ((d shr 39) or (d shl 25))) + ((d and E) xor (d and f) xor (E and f));
  g := g + T1;
  c := T1 + T2;
  T1 := b + (((g shr 14) or (g shl 50)) xor ((g shr 18) or (g shl 46)) xor ((g shr 41) or (g shl 23))) + ((g and h) xor (not g and a)) + $113F9804BEF90DAE + w[70];
  T2 := (((c shr 28) or (c shl 36)) xor ((c shr 34) or (c shl 30)) xor ((c shr 39) or (c shl 25))) + ((c and d) xor (c and E) xor (d and E));
  f := f + T1;
  b := T1 + T2;
  T1 := a + (((f shr 14) or (f shl 50)) xor ((f shr 18) or (f shl 46)) xor ((f shr 41) or (f shl 23))) + ((f and g) xor (not f and h)) + $1B710B35131C471B + w[71];
  T2 := (((b shr 28) or (b shl 36)) xor ((b shr 34) or (b shl 30)) xor ((b shr 39) or (b shl 25))) + ((b and c) xor (b and d) xor (c and d));
  E := E + T1;
  a := T1 + T2;
  T1 := h + (((E shr 14) or (E shl 50)) xor ((E shr 18) or (E shl 46)) xor ((E shr 41) or (E shl 23))) + ((E and f) xor (not E and g)) + $28DB77F523047D84 + w[72];
  T2 := (((a shr 28) or (a shl 36)) xor ((a shr 34) or (a shl 30)) xor ((a shr 39) or (a shl 25))) + ((a and b) xor (a and c) xor (b and c));
  d := d + T1;
  h := T1 + T2;
  T1 := g + (((d shr 14) or (d shl 50)) xor ((d shr 18) or (d shl 46)) xor ((d shr 41) or (d shl 23))) + ((d and E) xor (not d and f)) + $32CAAB7B40C72493 + w[73];
  T2 := (((h shr 28) or (h shl 36)) xor ((h shr 34) or (h shl 30)) xor ((h shr 39) or (h shl 25))) + ((h and a) xor (h and b) xor (a and b));
  c := c + T1;
  g := T1 + T2;
  T1 := f + (((c shr 14) or (c shl 50)) xor ((c shr 18) or (c shl 46)) xor ((c shr 41) or (c shl 23))) + ((c and d) xor (not c and E)) + $3C9EBE0A15C9BEBC + w[74];
  T2 := (((g shr 28) or (g shl 36)) xor ((g shr 34) or (g shl 30)) xor ((g shr 39) or (g shl 25))) + ((g and h) xor (g and a) xor (h and a));
  b := b + T1;
  f := T1 + T2;
  T1 := E + (((b shr 14) or (b shl 50)) xor ((b shr 18) or (b shl 46)) xor ((b shr 41) or (b shl 23))) + ((b and c) xor (not b and d)) + $431D67C49C100D4C + w[75];
  T2 := (((f shr 28) or (f shl 36)) xor ((f shr 34) or (f shl 30)) xor ((f shr 39) or (f shl 25))) + ((f and g) xor (f and h) xor (g and h));
  a := a + T1;
  E := T1 + T2;
  T1 := d + (((a shr 14) or (a shl 50)) xor ((a shr 18) or (a shl 46)) xor ((a shr 41) or (a shl 23))) + ((a and b) xor (not a and c)) + $4CC5D4BECB3E42B6 + w[76];
  T2 := (((E shr 28) or (E shl 36)) xor ((E shr 34) or (E shl 30)) xor ((E shr 39) or (E shl 25))) + ((E and f) xor (E and g) xor (f and g));
  h := h + T1;
  d := T1 + T2;
  T1 := c + (((h shr 14) or (h shl 50)) xor ((h shr 18) or (h shl 46)) xor ((h shr 41) or (h shl 23))) + ((h and a) xor (not h and b)) + $597F299CFC657E2A + w[77];
  T2 := (((d shr 28) or (d shl 36)) xor ((d shr 34) or (d shl 30)) xor ((d shr 39) or (d shl 25))) + ((d and E) xor (d and f) xor (E and f));
  g := g + T1;
  c := T1 + T2;
  T1 := b + (((g shr 14) or (g shl 50)) xor ((g shr 18) or (g shl 46)) xor ((g shr 41) or (g shl 23))) + ((g and h) xor (not g and a)) + $5FCB6FAB3AD6FAEC + w[78];
  T2 := (((c shr 28) or (c shl 36)) xor ((c shr 34) or (c shl 30)) xor ((c shr 39) or (c shl 25))) + ((c and d) xor (c and E) xor (d and E));
  f := f + T1;
  b := T1 + T2;
  T1 := a + (((f shr 14) or (f shl 50)) xor ((f shr 18) or (f shl 46)) xor ((f shr 41) or (f shl 23))) + ((f and g) xor (not f and h)) + $6C44198C4A475817 + w[79];
  T2 := (((b shr 28) or (b shl 36)) xor ((b shr 34) or (b shl 30)) xor ((b shr 39) or (b shl 25))) + ((b and c) xor (b and d) xor (c and d));
  E := E + T1;
  a := T1 + T2;

  inc(PUInt64(@Digest[0])^, a);
  inc(PUInt64(@Digest[8])^, b);
  inc(PUInt64(@Digest[16])^, c);
  inc(PUInt64(@Digest[24])^, d);
  inc(PUInt64(@Digest[32])^, E);
  inc(PUInt64(@Digest[40])^, f);
  inc(PUInt64(@Digest[48])^, g);
  inc(PUInt64(@Digest[56])^, h);
{$IFDEF RangeCheck}{$R+}{$ENDIF}
end;

class procedure TSHA512.SHA512(var Digest: TSHA512Digest; const Buf; BufSize: UInt64);
var
  Lo, Hi: UInt64;
  p: PByte;
  ChunkIndex: Byte;
  ChunkBuf: array [0 .. 127] of Byte;
begin
{$IFDEF RangeCheck}{$R-}{$ENDIF}
{$IFDEF OverflowCheck}{$Q-}{$ENDIF}
  PUInt64(@Digest[0])^ := $6A09E667F3BCC908;
  PUInt64(@Digest[8])^ := $BB67AE8584CAA73B;
  PUInt64(@Digest[16])^ := $3C6EF372FE94F82B;
  PUInt64(@Digest[24])^ := $A54FF53A5F1D36F1;
  PUInt64(@Digest[32])^ := $510E527FADE682D1;
  PUInt64(@Digest[40])^ := $9B05688C2B3E6C1F;
  PUInt64(@Digest[48])^ := $1F83D9ABFB41BD6B;
  PUInt64(@Digest[56])^ := $5BE0CD19137E2179;

  Lo := 0;
  Hi := 0;

  inc(Lo, BufSize shl 3);
  inc(Hi, BufSize shr 61);

  SwapQWORD(Lo);
  SwapQWORD(Hi);

  p := @Buf;

  while BufSize >= 128 do
    begin
      Compute(Digest, p);
      inc(p, 128);
      dec(BufSize, 128);
    end;
  if BufSize > 0 then
      CopyPtr(p, @ChunkBuf[0], BufSize);

  ChunkBuf[BufSize] := $80;
  ChunkIndex := BufSize + 1;
  if ChunkIndex > 112 then
    begin
      if ChunkIndex < 128 then
          FillPtrByte(@ChunkBuf[ChunkIndex], 128 - ChunkIndex, 0);
      Compute(Digest, @ChunkBuf);
      ChunkIndex := 0
    end;
  FillPtrByte(@ChunkBuf[ChunkIndex], 112 - ChunkIndex, 0);
  PUInt64(@ChunkBuf[112])^ := Hi;
  PUInt64(@ChunkBuf[120])^ := Lo;
  Compute(Digest, @ChunkBuf);

  SwapQWORD(PUInt64(@Digest[0])^);
  SwapQWORD(PUInt64(@Digest[8])^);
  SwapQWORD(PUInt64(@Digest[16])^);
  SwapQWORD(PUInt64(@Digest[24])^);
  SwapQWORD(PUInt64(@Digest[32])^);
  SwapQWORD(PUInt64(@Digest[40])^);
  SwapQWORD(PUInt64(@Digest[48])^);
  SwapQWORD(PUInt64(@Digest[56])^);
{$IFDEF RangeCheck}{$R+}{$ENDIF}
{$IFDEF OverflowCheck}{$Q+}{$ENDIF}
end;

{$IFDEF RangeCheck}{$R-}{$ENDIF}
{$IFDEF OverflowCheck}{$Q-}{$ENDIF}


class function TSHA3.ComputeX(const x: Integer): Integer;
begin
  if x < 0 then
      Result := ComputeX(x + 5)
  else
      Result := x mod 5;
end;

class function TSHA3.ComputeXY(const x, y: Integer): Integer;
begin
  Result := ComputeX(x) + 5 * ComputeX(y);
end;

class procedure TSHA3.BlockSHA3(var Context: TSHA3Context);
var
  i, x, y: Integer;
begin
  i := 0;
  while i < Context.BlockLen do
    begin
      Context.a[i shr 3] := Context.a[i shr 3] Xor PUInt64(@Context.Buffer[i])^;
      inc(i, 8);
    end;

  // keccakf
  for i := 0 to 23 do
    begin
      for x := 0 to 4 do
          Context.c[x] :=
          Context.a[ComputeXY(x, 0)] Xor
          Context.a[ComputeXY(x, 1)] Xor
          Context.a[ComputeXY(x, 2)] Xor
          Context.a[ComputeXY(x, 3)] Xor
          Context.a[ComputeXY(x, 4)];

      for x := 0 to 4 do
        begin
          Context.d[x] := Context.c[ComputeX(x - 1)] Xor ROL64(Context.c[ComputeX(x + 1)], 1);
          for y := 0 to 4 do
              Context.a[ComputeXY(x, y)] := Context.a[ComputeXY(x, y)] Xor Context.d[x];
        end;

      for x := 0 to 4 do
        for y := 0 to 4 do
            Context.b[ComputeXY(y, x * 2 + 3 * y)] := ROL64(Context.a[ComputeXY(x, y)], RO[ComputeXY(x, y)]);

      for x := 0 to 4 do
        for y := 0 to 4 do
            Context.a[ComputeXY(x, y)] := Context.b[ComputeXY(x, y)] Xor ((not Context.b[ComputeXY(x + 1, y)]) And Context.b[ComputeXY(x + 2, y)]);

      Context.a[0] := Context.a[0] Xor RC[i];
    end;

  Context.BufSize := 0;
end;
{$IFDEF RangeCheck}{$R+}{$ENDIF}
{$IFDEF OverflowCheck}{$Q+}{$ENDIF}


class procedure TSHA3.InitializeSHA3(var Context: TSHA3Context; HashLength: Integer);
var
  i: Integer;
begin
  Context.HashLength := HashLength;
  Context.BlockLen := 200 - 2 * HashLength;
  SetLength(Context.Buffer, Context.BlockLen);
  Context.BufSize := 0;

  for i := 0 to 24 do
    begin
      Context.a[i] := 0;
      Context.b[i] := 0;
    end;

  for i := 0 to 4 do
    begin
      Context.c[i] := 0;
      Context.d[i] := 0;
    end;
end;

class procedure TSHA3.SHA3(var Context: TSHA3Context; Chunk: PByte; Size: NativeInt);
var
  Needed: NativeInt;
begin
  while Size > 0 do
    begin
      Needed := Min(NativeInt(Context.BlockLen - Context.BufSize), Size);
      CopyPtr(Chunk, @Context.Buffer[Context.BufSize], Needed);
      inc(Context.BufSize, Needed);
      dec(Size, Needed);

      if Context.BufSize = Context.BlockLen then
        begin
          BlockSHA3(Context);
          Context.BufSize := 0;
          inc(Chunk, Needed);
        end;
    end;
end;

class procedure TSHA3.FinalizeSHA3(var Context: TSHA3Context; const output: PCCByteArray);
var
  i: Integer;
begin
  if (Context.BufSize = Context.BlockLen - 1) then
      Context.Buffer[Context.BufSize] := Byte($81)
  else
    begin
      Context.Buffer[Context.BufSize] := Byte($06);
      FillPtrByte(@Context.Buffer[Context.BufSize + 1], (Context.BlockLen - 2) - (Context.BufSize + 1), 0);
      Context.Buffer[Context.BlockLen - 1] := Byte($80);
    end;

  BlockSHA3(Context);

  i := 0;
  while i < Context.HashLength do
    begin
      PUInt64(@output^[i])^ := Context.a[i shr 3];
      inc(i, 8);
    end;

  for i := 0 to 24 do
      Context.a[i] := 0;
  Context.BufSize := 0;
  SetLength(Context.Buffer, 0);
end;

class procedure TSHA3.FinalizeSHAKE(var Context: TSHA3Context; Limit: Integer; const output: PCCByteArray);
var
  tmp: array of Byte;
  i, k, Size: Integer;
begin
  if Context.BufSize = Context.BlockLen - 1 then
      Context.Buffer[Context.BufSize] := Byte($9F)
  else
    begin
      Context.Buffer[Context.BufSize] := Byte($1F);
      FillPtrByte(@Context.Buffer[Context.BufSize + 1], (Context.BlockLen - 2) - (Context.BufSize + 1), 0);
      Context.Buffer[Context.BlockLen - 1] := Byte($80);
    end;

  BlockSHA3(Context);

  FillPtrByte(@Context.Buffer[0], length(Context.Buffer), 0);

  SetLength(tmp, Context.BlockLen);
  k := 0;
  while True do
    begin
      i := 0;
      while i < Context.BlockLen do
        begin
          PUInt64(@tmp[i])^ := Context.a[i shr 3];
          inc(i, 8);
        end;

      Size := Min(Limit, Context.BlockLen);
      CopyPtr(@tmp[0], @output^[k], Min(Size, Limit - k));
      dec(Limit, Size);
      inc(k, Size);

      if Limit <= 0 then
          Break;

      BlockSHA3(Context);
    end;

  for i := 0 to 24 do
      Context.a[i] := 0;

  Context.BufSize := 0;
  SetLength(Context.Buffer, 0);
end;

class procedure TSHA3.SHA224(var Digest: TSHA3_224_Digest; Buf: PByte; BufSize: NativeInt);
var
  Ctx: TSHA3Context;
begin
  InitializeSHA3(Ctx, 224 div 8);
  SHA3(Ctx, Buf, BufSize);
  FinalizeSHA3(Ctx, @Digest[0]);
end;

class procedure TSHA3.SHA256(var Digest: TSHA3_256_Digest; Buf: PByte; BufSize: NativeInt);
var
  Ctx: TSHA3Context;
begin
  InitializeSHA3(Ctx, 256 div 8);
  SHA3(Ctx, Buf, BufSize);
  FinalizeSHA3(Ctx, @Digest[0]);
end;

class procedure TSHA3.SHA384(var Digest: TSHA3_384_Digest; Buf: PByte; BufSize: NativeInt);
var
  Ctx: TSHA3Context;
begin
  InitializeSHA3(Ctx, 384 div 8);
  SHA3(Ctx, Buf, BufSize);
  FinalizeSHA3(Ctx, @Digest[0]);
end;

class procedure TSHA3.SHA512(var Digest: TSHA3_512_Digest; Buf: PByte; BufSize: NativeInt);
var
  Ctx: TSHA3Context;
begin
  InitializeSHA3(Ctx, 512 div 8);
  SHA3(Ctx, Buf, BufSize);
  FinalizeSHA3(Ctx, @Digest[0]);
end;

class procedure TSHA3.SHAKE128(const Digest: PCCByteArray; Buf: PByte; BufSize: NativeInt; Limit: Integer);
var
  Ctx: TSHA3Context;
begin
  InitializeSHA3(Ctx, 128 div 8);
  SHA3(Ctx, Buf, BufSize);
  FinalizeSHAKE(Ctx, Limit div 8, Digest);
end;

class procedure TSHA3.SHAKE256(const Digest: PCCByteArray; Buf: PByte; BufSize: NativeInt; Limit: Integer);
var
  Ctx: TSHA3Context;
begin
  InitializeSHA3(Ctx, 256 div 8);
  SHA3(Ctx, Buf, BufSize);
  FinalizeSHAKE(Ctx, Limit div 8, Digest);
end;

{ TLBC }

class procedure TLBC.EncryptLBC(const Context: TLBCContext; var Block: TLBCBlock);
var
  Blocks: array [0 .. 1] of TBCHalfBlock; { !!.01 }
  Work: TBCHalfBlock;
  Right: TBCHalfBlock;
  Left: TBCHalfBlock;
  AA, BB: Integer;
  CC, DD: Integer;
  r, t: Integer;
begin
  CopyPtr(@Block, @Blocks, SizeOf(Blocks)); { !!.01 }
  Right := Blocks[0];
  Left := Blocks[1];

  for r := 0 to Context.Rounds - 1 do begin
      { transform the right side }
      AA := Right[0];
      BB := TBCHalfBlock(Context.SubKeys64[r])[0];
      CC := Right[1];
      DD := TBCHalfBlock(Context.SubKeys64[r])[1];

      { mix it once... }
      AA := AA + DD;
      DD := DD + AA;
      AA := AA xor (AA shr 7);
      BB := BB + AA;
      AA := AA + BB;
      BB := BB xor (BB shl 13);
      CC := CC + BB;
      BB := BB + CC;
      CC := CC xor (CC shr 17);
      DD := DD + CC;
      CC := CC + DD;
      DD := DD xor (DD shl 9);
      AA := AA + DD;
      DD := DD + AA;
      AA := AA xor (AA shr 3);
      BB := BB + AA;
      AA := AA + BB;
      BB := BB xor (BB shl 7);
      CC := CC + BB;
      BB := BB + CC;
      CC := CC xor (DD shr 15);
      DD := DD + CC;
      CC := CC + DD;
      DD := DD xor (DD shl 11);

      { swap sets... }
      t := AA;
      AA := CC;
      CC := t;
      t := BB;
      BB := DD;
      DD := t;

      { mix it twice }
      AA := AA + DD;
      DD := DD + AA;
      AA := AA xor (AA shr 7);
      BB := BB + AA;
      AA := AA + BB;
      BB := BB xor (BB shl 13);
      CC := CC + BB;
      BB := BB + CC;
      CC := CC xor (CC shr 17);
      DD := DD + CC;
      CC := CC + DD;
      DD := DD xor (DD shl 9);
      AA := AA + DD;
      DD := DD + AA;
      AA := AA xor (AA shr 3);
      BB := BB + AA;
      AA := AA + BB;
      BB := BB xor (BB shl 7);
      CC := CC + BB;
      BB := BB + CC;
      CC := CC xor (DD shr 15);
      DD := DD + CC;
      CC := CC + DD;
      DD := DD xor (DD shl 11);

      Work[0] := Left[0] xor AA xor BB;
      Work[1] := Left[1] xor CC xor DD;

      Left := Right;
      Right := Work;
    end;

  Blocks[0] := Left;
  Blocks[1] := Right;
  CopyPtr(@Blocks, @Block, SizeOf(Block)); { !!.01 }
end;

class procedure TLBC.EncryptLQC(const key: TKey128; var Block: TLQCBlock; Encrypt: Boolean);
const
  CKeyBox: array [False .. True, 0 .. 3, 0 .. 2] of Integer =
    (((0, 3, 1), (2, 1, 3), (1, 0, 2), (3, 2, 0)),
    ((3, 2, 0), (1, 0, 2), (2, 1, 3), (0, 3, 1)));
var
  KeyInts: array [0 .. 3] of Integer; { !!.01 }
  Blocks: array [0 .. 1] of Integer;  { !!.01 }
  Work: Integer;
  Right: Integer;
  Left: Integer;
  r: Integer;
  AA, BB: Integer;
  CC, DD: Integer;
begin
  CopyPtr(@key, @KeyInts, SizeOf(KeyInts)); { !!.01 }
  CopyPtr(@Block, @Blocks, SizeOf(Blocks)); { !!.01 }
  Right := Blocks[0];
  Left := Blocks[1];

  for r := 0 to 3 do begin
      { transform the right side }
      AA := Right;
      BB := KeyInts[CKeyBox[Encrypt, r, 0]];
      CC := KeyInts[CKeyBox[Encrypt, r, 1]];
      DD := KeyInts[CKeyBox[Encrypt, r, 2]];

      { commented code does not affect results - removed for speed }
      AA := AA + DD;
      DD := DD + AA;
      AA := AA xor (AA shr 7);
      BB := BB + AA;
      AA := AA + BB;
      BB := BB xor (BB shl 13);
      CC := CC + BB;
      BB := BB + CC;
      CC := CC xor (CC shr 17);
      DD := DD + CC;
      CC := CC + DD;
      DD := DD xor (DD shl 9);
      AA := AA + DD;
      DD := DD + AA;
      AA := AA xor (AA shr 3);
      BB := BB + AA; { AA := AA + BB; }
      BB := BB xor (BB shl 7);
      CC := CC + BB; { BB := BB + CC; }
      CC := CC xor (DD shr 15);
      DD := DD + CC; { CC := CC + DD; }
      DD := DD xor (DD shl 11);

      Work := Left xor DD;
      Left := Right;
      Right := Work;
    end;

  Blocks[0] := Left;
  Blocks[1] := Right;
  CopyPtr(@Blocks, @Block, SizeOf(Block)); { !!.01 }
end;

class procedure TLBC.InitEncryptLBC(const key: TKey128; var Context: TLBCContext; Rounds: Integer; Encrypt: Boolean);
type
  TDCPTFSubKeys = packed record
    case Byte of
      0: (SubKeys64: array [0 .. 15] of TKey64);
      1: (SubKeysInts: array [0 .. 3, 0 .. 7] of DWORD);
  end;
var
  KeyArray: PDWordArray;
  AA, BB: DWORD;
  CC, DD: DWORD;
  EE, FF: DWORD;
  GG, hh: DWORD;
  i, r: Integer;
  Temp: TDCPTFSubKeys;
begin
  KeyArray := @key;
  Context.Encrypt := Encrypt;
  Context.Rounds := Max(4, Min(16, Rounds));

  { fill subkeys by propagating seed }
  for i := 0 to 3 do begin
      { interleave the key with the salt }

      AA := KeyArray^[0];
      BB := BCSalts[i];
      CC := KeyArray^[1];
      DD := BCSalts[i];
      EE := KeyArray^[2];
      FF := BCSalts[i];
      GG := KeyArray^[3];
      hh := BCSalts[i];

      { mix all the bits around for 8 rounds }
      { achieves avalanche and eliminates funnels }
      for r := 0 to 7 do begin
          AA := AA xor (BB shl 11);
          DD := DD + AA;
          BB := BB + CC;
          BB := BB xor (CC shr 2);
          EE := EE + BB;
          CC := CC + DD;
          CC := CC xor (DD shl 8);
          FF := FF + CC;
          DD := DD + EE;
          DD := DD xor (EE shr 16);
          GG := GG + DD;
          EE := EE + FF;
          EE := EE xor (FF shl 10);
          hh := hh + EE;
          FF := FF + GG;
          FF := FF xor (GG shr 4);
          AA := AA + FF;
          GG := GG + hh;
          GG := GG xor (hh shl 8);
          BB := BB + GG;
          hh := hh + AA;
          hh := hh xor (AA shr 9);
          CC := CC + hh;
          AA := AA + BB;
        end;

      { assign value to subkey }
      Context.SubKeysInts[i, 0] := AA;
      Context.SubKeysInts[i, 1] := BB;
      Context.SubKeysInts[i, 2] := CC;
      Context.SubKeysInts[i, 3] := DD;
      Context.SubKeysInts[i, 4] := EE;
      Context.SubKeysInts[i, 5] := FF;
      Context.SubKeysInts[i, 6] := GG;
      Context.SubKeysInts[i, 7] := hh;
    end;

  { reverse subkeys if decrypting - easier for EncryptLBC routine }
  if not Encrypt then begin
      for i := 0 to Context.Rounds - 1 do
          Temp.SubKeys64[(Context.Rounds - 1) - i] := Context.SubKeys64[i];
      for i := 0 to Context.Rounds - 1 do
          Context.SubKeys64[i] := Temp.SubKeys64[i];
    end;
end;

{ THashMD5 }

class procedure THashMD5.FinalizeMD5(var Context: TMD5Context; var Digest: TMD5Digest);
const
  Padding: array [0 .. 63] of Byte = (
    $80, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00);
var
  InBuf: TTransformInput;
  MDI: Integer;
  i: Word;
  II: Word;
  PadLen: Word;
begin
  { save number of bits }
  InBuf[14] := Context.Count[0];
  InBuf[15] := Context.Count[1];
  { compute number of bytes mod 64 }
  MDI := (Context.Count[0] shr 3) and $3F;
  { pad out to 56 mod 64 }
  if (MDI < 56) then
      PadLen := 56 - MDI
  else
      PadLen := 120 - MDI;
  UpdateMD5(Context, Padding, PadLen);

  CopyPtr(@Context, @Context, SizeOf(Context)); { !!.01 }

  { append length in bits and transform }
  II := 0;
  for i := 0 to 13 do
    begin
      InBuf[i] := (DWORD(Context.Buf[II + 3]) shl 24) or (DWORD(Context.Buf[II + 2]) shl 16) or (DWORD(Context.Buf[II + 1]) shl 8) or DWORD(Context.Buf[II]);
      inc(II, 4);
    end;
  TMISC.Transform(Context.State, InBuf);
  { store buffer in digest }
  II := 0;
  for i := 0 to 3 do begin
      Digest[II] := Byte(Context.State[i] and $FF);
      Digest[II + 1] := Byte((Context.State[i] shr 8) and $FF);
      Digest[II + 2] := Byte((Context.State[i] shr 16) and $FF);
      Digest[II + 3] := Byte((Context.State[i] shr 24) and $FF);
      inc(II, 4);
    end;
end;

class procedure THashMD5.GenerateMD5Key(var key: TKey128; const ABytes: TBytes);
var
  d: TMD5Digest;
begin
  HashMD5(d, ABytes[0], length(ABytes));
end;

class procedure THashMD5.HashMD5(var Digest: TMD5Digest; const Buf; BufSize: NativeInt);
var
  Context: TMD5Context;
begin
  FillPtrByte(@Context, SizeOf(Context), $00);
  InitMD5(Context);
  UpdateMD5(Context, Buf, BufSize);
  FinalizeMD5(Context, Digest);
end;

class procedure THashMD5.InitMD5(var Context: TMD5Context);
begin
  Context.Count[0] := 0;
  Context.Count[1] := 0;

  { load magic initialization constants }
  Context.State[0] := $67452301;
  Context.State[1] := $EFCDAB89;
  Context.State[2] := $98BADCFE;
  Context.State[3] := $10325476;
end;

class procedure THashMD5.ByteBuffHashMD5(var Digest: TMD5Digest; const ABytes: TBytes);
begin
  HashMD5(Digest, ABytes[0], length(ABytes));
end;

class procedure THashMD5.UpdateMD5(var Context: TMD5Context; const Buf; BufSize: NativeInt);
var
  InBuf: TTransformInput;
  BufOfs: DWORD;
  MDI: DWORD;
  i: DWORD;
begin
  // { compute number of bytes mod 64 }
  MDI := (Context.Count[0] shr 3) and $3F;

  // { update number of bits }
  if BufSize shl 3 < 0 then
      inc(Context.Count[1]);

  inc(Context.Count[0], BufSize shl 3);
  inc(Context.Count[1], BufSize shr 29);

  { add new byte acters to buffer }
  BufOfs := 0;
  while (BufSize > 0) do
    begin
      dec(BufSize);
      Context.Buf[MDI] := TCCByteArray(Buf)[BufOfs]; { !!.01 }
      inc(MDI);
      inc(BufOfs);
      if (MDI = $40) then
        begin
          for i := 0 to 15 do
              InBuf[i] := PDWORD(@Context.Buf[i * 4])^;
          TMISC.Transform(Context.State, InBuf);
          MDI := 0;
        end;
    end;
end;

{ TRNG }

class procedure TRNG.EncryptRNG32(var Context: TRNG32Context; var Buf; BufSize: Integer);
var
  i: Integer;
begin
  for i := 0 to BufSize - 1 do
      TCCByteArray(Buf)[i] := TCCByteArray(Buf)[i] xor { !!.01 }
      TMISC.Random32Byte(Integer(Context));
end;

class procedure TRNG.EncryptRNG64(var Context: TRNG64Context; var Buf; BufSize: Integer);
var
  i: Integer;
begin
  for i := 0 to BufSize - 1 do
      TCCByteArray(Buf)[i] := TCCByteArray(Buf)[i] xor { !!.01 }
      TMISC.Random64Byte(TInt64(Context));
end;

class procedure TRNG.InitEncryptRNG32(key: DWORD; var Context: TRNG32Context);
begin
  DWORD(Context) := key;
end;

class procedure TRNG.InitEncryptRNG64(KeyHi, KeyLo: DWORD; var Context: TRNG64Context);
begin
  TInt64(Context).Lo := Integer(KeyLo);
  TInt64(Context).Hi := Integer(KeyHi);
end;

{ THashMD }

class procedure THashMD.FinalizeLMD(var Context: TLMDContext; var Digest; DigestSize: Integer);
const
  Padding: array [0 .. 7] of Byte = (1, 0, 0, 0, 0, 0, 0, 0);
var
  BCContext: TLBCContext;
  i: Integer;
begin
  { pad with "1", followed by as many "0"s as needed to fill the block }
  UpdateLMD(Context, Padding, SizeOf(Padding) - Context.KeyIndex);

  { mix context using block cipher }
  TLBC.InitEncryptLBC(Context.key, BCContext, 8, True);
  for i := 0 to (SizeOf(Context.Digest) div SizeOf(TLBCBlock)) - 1 do
      TLBC.EncryptLBC(BCContext, PLBCBlock(@Context.Digest[i * SizeOf(TLBCBlock)])^);

  { return Digest of requested DigestSize }
  { max digest is 2048-bit, although it could be greater if Pi2048 was larger }
  if DigestSize > SizeOf(Context.Digest) then
      FillPtrByte(@Digest, DigestSize, 0);
  CopyPtr(@Context.Digest, @Digest, Min(SizeOf(Context.Digest), DigestSize));
end;

class procedure THashMD.GenerateLMDKey(var key; KeySize: Integer; const ABytes: TBytes);
begin
  HashLMD(key, KeySize, ABytes[0], length(ABytes));
end;

class procedure THashMD.HashLMD(var Digest; DigestSize: Integer; const Buf; BufSize: NativeInt);
var
  Context: TLMDContext;
begin
  InitLMD(Context);
  UpdateLMD(Context, Buf, BufSize);
  FinalizeLMD(Context, Digest, DigestSize);
end;

class procedure THashMD.InitLMD(var Context: TLMDContext);
begin
  Context.DigestIndex := 0;
  TBlock2048(Context.Digest) := TBlock2048(Pi2048);

  Context.KeyIndex := 0;
  Context.KeyInts[0] := $55555555;
  Context.KeyInts[1] := $55555555;
  Context.KeyInts[2] := $55555555;
  Context.KeyInts[3] := $55555555;
end;

class procedure THashMD.ByteBuffHashLMD(var Digest; DigestSize: Integer; const ABytes: TBytes);
begin
  HashLMD(Digest, DigestSize, ABytes[0], length(ABytes));
end;

class procedure THashMD.UpdateLMD(var Context: TLMDContext; const Buf; BufSize: NativeInt);
var
  AA, BB, CC, DD: DWORD;
  i, r: NativeInt;
begin
  for i := 0 to BufSize - 1 do
    with Context do begin
        { update Digest }
        Digest[DigestIndex] := Digest[DigestIndex] xor
          TCCByteArray(Buf)[i]; { !!.01 }
        DigestIndex := DigestIndex + 1;
        if (DigestIndex = SizeOf(Digest)) then
            DigestIndex := 0;

        { update BlockKey }
        key[KeyIndex] := key[KeyIndex] xor TCCByteArray(Buf)[i]; { !!.01 }
        KeyIndex := KeyIndex + 1;
        if (KeyIndex = SizeOf(key) div 2) then begin
            AA := KeyInts[3];
            BB := KeyInts[2];
            CC := KeyInts[1];
            DD := KeyInts[0];

            { mix all the bits around for 4 rounds }
            { achieves avalanche and eliminates funnels }
            for r := 0 to 3 do
              begin
                inc(AA, DD);
                inc(DD, AA);
                AA := AA xor (AA shr 7);
                inc(BB, AA);
                inc(AA, BB);
                BB := BB xor (BB shl 13);
                inc(CC, BB);
                inc(BB, CC);
                CC := CC xor (CC shr 17);
                inc(DD, CC);
                inc(CC, DD);
                DD := DD xor (DD shl 9);
                inc(AA, DD);
                inc(DD, AA);
                AA := AA xor (AA shr 3);
                inc(BB, AA);
                inc(AA, BB);
                BB := BB xor (BB shl 7);
                inc(CC, BB);
                inc(BB, CC);
                CC := CC xor (DD shr 15);
                inc(DD, CC);
                inc(CC, DD);
                DD := DD xor (DD shl 11);
              end;

            KeyInts[0] := AA;
            KeyInts[1] := BB;
            KeyInts[2] := CC;
            KeyInts[3] := DD;

            KeyIndex := 0;
          end;
      end;
end;

{ TLSC }

class procedure TLSC.EncryptLSC(var Context: TLSCContext; var Buf; BufSize: Integer);
var
  l, y, x: Integer;
  i, a: Integer;
begin
  i := Context.index;
  a := Context.Accumulator;

  for l := 0 to BufSize - 1 do begin
      i := i + 1;

      x := Context.SBox[Byte(i)];
      y := Context.SBox[Byte(x)] + x;
      Context.SBox[Byte(i)] := Context.SBox[Byte(y)];
      Context.SBox[Byte(y)] := x;

      a := a + Context.SBox[Byte(Byte(y shr 8) + Byte(y))];
      TCCByteArray(Buf)[l] := TCCByteArray(Buf)[l] xor Byte(a); { !!.01 }
    end;

  Context.index := i;
  Context.Accumulator := a;
end;

class procedure TLSC.InitEncryptLSC(const key; KeySize: Integer; var Context: TLSCContext);
var
  r, i, a: Integer;
  x: Byte;
begin
  { initialize SBox }
  for i := 0 to 255 do
      Context.SBox[i] := i;

  a := 0;
  for r := 0 to 2 do { 3 rounds - "A" accumulates }
    for i := 0 to 255 do
      begin
        a := a + Context.SBox[i] + TCCByteArray(key)[i mod KeySize]; { !!.01 }
        x := Context.SBox[i];
        Context.SBox[i] := Context.SBox[Byte(a)];
        Context.SBox[Byte(a)] := x;
      end;

  Context.index := 0;
  Context.Accumulator := a;
end;

{ TMISC }

class procedure TMISC.GenerateRandomKey(var key; KeySize: Integer);
var
  i: Integer;
begin
  MT19937Randomize;
  for i := 0 to KeySize - 1 do
      TCCByteArray(key)[i] := MT19937Rand32(256); { !!.01 }
end;

class procedure TMISC.HashELF(var Digest: DWORD; const Buf; BufSize: nativeUInt);
var
  i: nativeUInt;
  x: DWORD;
begin
  Digest := 0;
  for i := 0 to BufSize - 1 do begin
      Digest := (Digest shl 4) + TCCByteArray(Buf)[i]; { !!.01 }
      x := Digest and $F0000000;
      if (x <> 0) then
          Digest := Digest xor (x shr 24);
      Digest := Digest and (not x);
    end;
end;

class procedure TMISC.HashELF64(var Digest: Int64; const Buf; BufSize: nativeUInt);
var
  i: nativeUInt;
  x: Int64;
begin
  Digest := 0;
  for i := 0 to BufSize - 1 do begin
      Digest := (Digest shl 4) + TCCByteArray(Buf)[i]; { !!.01 }
      x := Digest and Int64($F000000000000000);
      if (x <> 0) then
          Digest := Digest xor (x shr 56);
      Digest := Digest and (not x);
    end;
end;

class procedure TMISC.HashMix128(var Digest: DWORD; const Buf; BufSize: NativeInt);
type
  T128BitArray = array [0 .. MaxStructSize div SizeOf(T128Bit) - 1] of T128Bit;
var
  Temp: T128Bit;
  PTemp: PCCByteArray;
  i, l: NativeInt;
begin
{$IFDEF RangeCheck}{$R-}{$ENDIF}
  Temp[0] := $243F6A88; { first 16 bytes of Pi in binary }
  Temp[1] := $93F40317;
  Temp[2] := $0C110496;
  Temp[3] := $C709C289;

  l := BufSize div SizeOf(T128Bit);
  for i := 0 to l - 1 do begin
      Temp[0] := Temp[0] + T128BitArray(Buf)[i][0]; { !!.01 }
      Temp[1] := Temp[1] + T128BitArray(Buf)[i][1]; { !!.01 }
      Temp[2] := Temp[2] + T128BitArray(Buf)[i][2]; { !!.01 }
      Temp[3] := Temp[3] + T128BitArray(Buf)[i][3]; { !!.01 }
      Mix128(Temp);
    end;

  PTemp := @Temp;
  if (BufSize > l * SizeOf(T128Bit)) then
    begin
      for i := 0 to (BufSize - l * SizeOf(T128Bit)) - 1 do
          inc(PTemp^[i], PCCByteArray(@Buf)^[(l * SizeOf(T128Bit)) + i]); { !!.01 }
      Mix128(Temp);
    end;

  Digest := Temp[3];
{$IFDEF RangeCheck}{$R+}{$ENDIF}
end;

class procedure TMISC.Mix128(var x: T128Bit);
var
  AA, BB, CC, DD: DWORD;
begin
  AA := x[0];
  BB := x[1];
  CC := x[2];
  DD := x[3];

  AA := AA + DD;
  DD := DD + AA;
  AA := AA xor (AA shr 7);
  BB := BB + AA;
  AA := AA + BB;
  BB := BB xor (BB shl 13);
  CC := CC + BB;
  BB := BB + CC;
  CC := CC xor (CC shr 17);
  DD := DD + CC;
  CC := CC + DD;
  DD := DD xor (DD shl 9);
  AA := AA + DD;
  DD := DD + AA;
  AA := AA xor (AA shr 3);
  BB := BB + AA;
  AA := AA + BB;
  BB := BB xor (BB shl 7);
  CC := CC + BB;
  BB := BB + CC;
  CC := CC xor (DD shr 15);
  DD := DD + CC;
  CC := CC + DD;
  DD := DD xor (DD shl 11);

  x[0] := AA;
  x[1] := BB;
  x[2] := CC;
  x[3] := DD;
end;

class function TMISC.Ran01(var Seed: Integer): Integer;
begin
  Result := Ran0Prim(Seed, 16807, 127773, 2836);
end;

class function TMISC.Ran02(var Seed: Integer): Integer;
begin
  Result := Ran0Prim(Seed, 48271, 44488, 3399);
end;

class function TMISC.Ran03(var Seed: Integer): Integer;
begin
  Result := Ran0Prim(Seed, 69621, 30845, 23902);
end;

class function TMISC.Ran0Prim(var Seed: Integer; IA, IQ, IR: Integer): Integer;
const
  IM = 2147483647;
  MA = 123459876;
var
  i, k: Integer;
begin
  { XORing with mask avoids seeds of zero }
  i := Seed xor MA;
  k := i div IQ;
  i := (IA * (i - (k * IQ))) - (IR * k);
  if i < 0 then
      i := i + IM;
  Result := i xor MA;
  Seed := Result;
end;

class function TMISC.Random32Byte(var Seed: Integer): Byte;
var
  l: Integer;
  r: TInt32;
begin
  l := Ran01(Seed);
  r := TInt32(l);
  Result := r.LoLo xor r.LoHi xor r.HiLo xor r.HiHi;
end;

class function TMISC.Random64(var Seed: TInt64): Integer;
begin
  Ran01(Seed.Lo);
  Ran01(Seed.Hi);
  Result := Seed.Lo xor Seed.Hi;
end;

class function TMISC.Random64Byte(var Seed: TInt64): Byte;
var
  l: Integer;
  r: TInt32;
begin
  l := Random64(Seed);
  r := TInt32(l);
  Result := r.LoLo xor r.LoHi xor r.HiLo xor r.HiHi;
end;

class function TMISC.RolX(i, c: DWORD): DWORD;
begin
  Result := (i shl (c and 31)) or (i shr (32 - (c and 31)));
end;

class procedure TMISC.ByteBuffHashELF(var Digest: DWORD; const ABytes: TBytes);
begin
  HashELF(Digest, ABytes[0], length(ABytes));
end;

class procedure TMISC.ByteBuffHashMix128(var Digest: DWORD; const ABytes: TBytes);
begin
  HashMix128(Digest, ABytes[0], length(ABytes));
end;

class procedure TMISC.Transform(var OutputBuffer: TTransformOutput; var InBuf: TTransformInput);
const
  S11 = 7;
  S12 = 12;
  S13 = 17;
  S14 = 22;
  S21 = 5;
  S22 = 9;
  S23 = 14;
  S24 = 20;
  S31 = 4;
  S32 = 11;
  S33 = 16;
  S34 = 23;
  S41 = 6;
  S42 = 10;
  S43 = 15;
  S44 = 21;
var
  a: DWORD;
  b: DWORD;
  c: DWORD;
  d: DWORD;

  procedure FF(var a: DWORD; const b, c, d, x, s, AC: DWORD);
  begin
    a := RolX(a + ((b and c) or (not b and d)) + x + AC, s) + b;
  end;

  procedure GG(var a: DWORD; const b, c, d, x, s, AC: DWORD);
  begin
    a := RolX(a + ((b and d) or (c and not d)) + x + AC, s) + b;
  end;

  procedure hh(var a: DWORD; const b, c, d, x, s, AC: DWORD);
  begin
    a := RolX(a + (b xor c xor d) + x + AC, s) + b;
  end;

  procedure II(var a: DWORD; const b, c, d, x, s, AC: DWORD);
  begin
    a := RolX(a + (c xor (b or not d)) + x + AC, s) + b;
  end;

begin
  a := OutputBuffer[0];
  b := OutputBuffer[1];
  c := OutputBuffer[2];
  d := OutputBuffer[3];

  { round 1 }
  FF(a, b, c, d, InBuf[0], S11, $D76AA478);  { 1 }
  FF(d, a, b, c, InBuf[1], S12, $E8C7B756);  { 2 }
  FF(c, d, a, b, InBuf[2], S13, $242070DB);  { 3 }
  FF(b, c, d, a, InBuf[3], S14, $C1BDCEEE);  { 4 }
  FF(a, b, c, d, InBuf[4], S11, $F57C0FAF);  { 5 }
  FF(d, a, b, c, InBuf[5], S12, $4787C62A);  { 6 }
  FF(c, d, a, b, InBuf[6], S13, $A8304613);  { 7 }
  FF(b, c, d, a, InBuf[7], S14, $FD469501);  { 8 }
  FF(a, b, c, d, InBuf[8], S11, $698098D8);  { 9 }
  FF(d, a, b, c, InBuf[9], S12, $8B44F7AF);  { 10 }
  FF(c, d, a, b, InBuf[10], S13, $FFFF5BB1); { 11 }
  FF(b, c, d, a, InBuf[11], S14, $895CD7BE); { 12 }
  FF(a, b, c, d, InBuf[12], S11, $6B901122); { 13 }
  FF(d, a, b, c, InBuf[13], S12, $FD987193); { 14 }
  FF(c, d, a, b, InBuf[14], S13, $A679438E); { 15 }
  FF(b, c, d, a, InBuf[15], S14, $49B40821); { 16 }

  { round 2 }
  GG(a, b, c, d, InBuf[1], S21, $F61E2562);  { 17 }
  GG(d, a, b, c, InBuf[6], S22, $C040B340);  { 18 }
  GG(c, d, a, b, InBuf[11], S23, $265E5A51); { 19 }
  GG(b, c, d, a, InBuf[0], S24, $E9B6C7AA);  { 20 }
  GG(a, b, c, d, InBuf[5], S21, $D62F105D);  { 21 }
  GG(d, a, b, c, InBuf[10], S22, $02441453); { 22 }
  GG(c, d, a, b, InBuf[15], S23, $D8A1E681); { 23 }
  GG(b, c, d, a, InBuf[4], S24, $E7D3FBC8);  { 24 }
  GG(a, b, c, d, InBuf[9], S21, $21E1CDE6);  { 25 }
  GG(d, a, b, c, InBuf[14], S22, $C33707D6); { 26 }
  GG(c, d, a, b, InBuf[3], S23, $F4D50D87);  { 27 }
  GG(b, c, d, a, InBuf[8], S24, $455A14ED);  { 28 }
  GG(a, b, c, d, InBuf[13], S21, $A9E3E905); { 29 }
  GG(d, a, b, c, InBuf[2], S22, $FCEFA3F8);  { 30 }
  GG(c, d, a, b, InBuf[7], S23, $676F02D9);  { 31 }
  GG(b, c, d, a, InBuf[12], S24, $8D2A4C8A); { 32 }

  { round 3 }
  hh(a, b, c, d, InBuf[5], S31, $FFFA3942);  { 33 }
  hh(d, a, b, c, InBuf[8], S32, $8771F681);  { 34 }
  hh(c, d, a, b, InBuf[11], S33, $6D9D6122); { 35 }
  hh(b, c, d, a, InBuf[14], S34, $FDE5380C); { 36 }
  hh(a, b, c, d, InBuf[1], S31, $A4BEEA44);  { 37 }
  hh(d, a, b, c, InBuf[4], S32, $4BDECFA9);  { 38 }
  hh(c, d, a, b, InBuf[7], S33, $F6BB4B60);  { 39 }
  hh(b, c, d, a, InBuf[10], S34, $BEBFBC70); { 40 }
  hh(a, b, c, d, InBuf[13], S31, $289B7EC6); { 41 }
  hh(d, a, b, c, InBuf[0], S32, $EAA127FA);  { 42 }
  hh(c, d, a, b, InBuf[3], S33, $D4EF3085);  { 43 }
  hh(b, c, d, a, InBuf[6], S34, $4881D05);   { 44 }
  hh(a, b, c, d, InBuf[9], S31, $D9D4D039);  { 45 }
  hh(d, a, b, c, InBuf[12], S32, $E6DB99E5); { 46 }
  hh(c, d, a, b, InBuf[15], S33, $1FA27CF8); { 47 }
  hh(b, c, d, a, InBuf[2], S34, $C4AC5665);  { 48 }

  { round 4 }
  II(a, b, c, d, InBuf[0], S41, $F4292244);  { 49 }
  II(d, a, b, c, InBuf[7], S42, $432AFF97);  { 50 }
  II(c, d, a, b, InBuf[14], S43, $AB9423A7); { 51 }
  II(b, c, d, a, InBuf[5], S44, $FC93A039);  { 52 }
  II(a, b, c, d, InBuf[12], S41, $655B59C3); { 53 }
  II(d, a, b, c, InBuf[3], S42, $8F0CCC92);  { 54 }
  II(c, d, a, b, InBuf[10], S43, $FFEFF47D); { 55 }
  II(b, c, d, a, InBuf[1], S44, $85845DD1);  { 56 }
  II(a, b, c, d, InBuf[8], S41, $6FA87E4F);  { 57 }
  II(d, a, b, c, InBuf[15], S42, $FE2CE6E0); { 58 }
  II(c, d, a, b, InBuf[6], S43, $A3014314);  { 59 }
  II(b, c, d, a, InBuf[13], S44, $4E0811A1); { 60 }
  II(a, b, c, d, InBuf[4], S41, $F7537E82);  { 61 }
  II(d, a, b, c, InBuf[11], S42, $BD3AF235); { 62 }
  II(c, d, a, b, InBuf[2], S43, $2AD7D2BB);  { 63 }
  II(b, c, d, a, InBuf[9], S44, $EB86D391);  { 64 }

  inc(OutputBuffer[0], a);
  inc(OutputBuffer[1], b);
  inc(OutputBuffer[2], c);
  inc(OutputBuffer[3], d);
end;

class procedure TMISC.XorMem(var Mem1; const Mem2; Count: NativeInt);
var
  siz: Byte;
  i: Integer;
  p1, p2: nativeUInt;
begin
  p1 := nativeUInt(@Mem1);
  p2 := nativeUInt(@Mem2);
  siz := SizeOf(nativeUInt);

  for i := 1 to Count div siz do
    begin
      PNativeUInt(p1)^ := PNativeUInt(p1)^ xor PNativeUInt(p2)^;
      p1 := p1 + siz;
      p2 := p2 + siz;
    end;
  for i := 1 to Count mod siz do
    begin
      PByte(p1)^ := PByte(p1)^ xor PByte(p2)^;
      p1 := p1 + 1;
      p2 := p2 + 1;
    end;
end;

function XXTeaMX(Sum, y, z, p, E: DWORD; const k: PDWordArray): DWORD;
begin
{$IFDEF OverflowCheck}{$Q-}{$ENDIF}
{$IFDEF RangeCheck}{$R-}{$ENDIF}
  Result := (((z shr 5) xor (y shl 2)) + ((y shr 3) xor (z shl 4))) xor ((Sum xor y) + (k^[p and 3 xor E] xor z));
{$IFDEF RangeCheck}{$R+}{$ENDIF}
{$IFDEF OverflowCheck}{$Q+}{$ENDIF}
end;

procedure XXTEAEncrypt(var key: TKey128; var Block: TXXTEABlock);
const
  XXTeaDelta = $9E3779B9;
var
  k, v: PDWordArray;
  n, z, y, Sum, E, p, q: DWORD;
begin
{$IFDEF RangeCheck}{$R-}{$ENDIF}
  n := 64 div 4 - 1;
  k := PDWordArray(@key[0]);
  v := PDWordArray(@Block[0]);
  z := v^[n];
  Sum := 0;
  q := 6 + 52 div (n + 1);
  repeat
    inc(Sum, XXTeaDelta);
    E := (Sum shr 2) and 3;
    for p := 0 to n - 1 do begin
        y := v^[p + 1];
        inc(v^[p], XXTeaMX(Sum, y, z, p, E, k));
        z := v^[p];
      end;
    p := n;
    y := v^[0];
    inc(v^[p], XXTeaMX(Sum, y, z, p, E, k));
    z := v^[p];
    dec(q);
  until q = 0;
{$IFDEF RangeCheck}{$R+}{$ENDIF}
end;

procedure XXTEADecrypt(var key: TKey128; var Block: TXXTEABlock);
const
  XXTeaDelta = $9E3779B9;
var
  k, v: PDWordArray;
  n, z, y, Sum, E, p, q: DWORD;
begin
{$IFDEF RangeCheck}{$R-}{$ENDIF}
  n := 64 div 4 - 1;
  k := PDWordArray(@key[0]);
  v := PDWordArray(@Block[0]);

  y := v^[0];
  q := 6 + 52 div (n + 1);
  Sum := q * XXTeaDelta;
  while (Sum <> 0) do
    begin
      E := (Sum shr 2) and 3;
      for p := n downto 1 do begin
          z := v^[p - 1];
          dec(v^[p], XXTeaMX(Sum, y, z, p, E, k));
          y := v^[p];
        end;
      p := 0;
      z := v^[n];
      dec(v^[0], XXTeaMX(Sum, y, z, p, E, k));
      y := v^[0];
      dec(Sum, XXTeaDelta);
    end;
{$IFDEF RangeCheck}{$R+}{$ENDIF}
end;

class function TRC6.LRot32(x, c: DWORD): DWORD;
begin
  LRot32 := ROL32(x, Byte(c));
end;

class function TRC6.RRot32(x, c: DWORD): DWORD;
begin
  RRot32 := ROR32(x, Byte(c))
end;

class procedure TRC6.InitKey(buff: Pointer; Size: Integer; var KeyContext: TRC6Key);
const
  cRC6_sBox: array [0 .. 51] of DWORD = (
    $B7E15163, $5618CB1C, $F45044D5, $9287BE8E, $30BF3847, $CEF6B200,
    $6D2E2BB9, $0B65A572, $A99D1F2B, $47D498E4, $E60C129D, $84438C56,
    $227B060F, $C0B27FC8, $5EE9F981, $FD21733A, $9B58ECF3, $399066AC,
    $D7C7E065, $75FF5A1E, $1436D3D7, $B26E4D90, $50A5C749, $EEDD4102,
    $8D14BABB, $2B4C3474, $C983AE2D, $67BB27E6, $05F2A19F, $A42A1B58,
    $42619511, $E0990ECA, $7ED08883, $1D08023C, $BB3F7BF5, $5976F5AE,
    $F7AE6F67, $95E5E920, $341D62D9, $D254DC92, $708C564B, $0EC3D004,
    $ACFB49BD, $4B32C376, $E96A3D2F, $87A1B6E8, $25D930A1, $C410AA5A,
    $62482413, $007F9DCC, $9EB71785, $3CEE913E);
var
  xKeyD: array [0 .. 63] of DWORD;
  i, j, k, xKeyLen, a, b: DWORD;
begin
  Size := Size div 8;
  CopyPtr(buff, @xKeyD, Size);
  xKeyLen := Size div 4;
  if (Size mod 4) <> 0 then
      inc(xKeyLen);
  CopyPtr(@cRC6_sBox, @KeyContext, ((cRC6_NumRounds * 2) + 4) * 4);
  i := 0;
  j := 0;
  a := 0;
  b := 0;
  if xKeyLen > ((cRC6_NumRounds * 2) + 4) then
      k := xKeyLen * 3
  else
      k := ((cRC6_NumRounds * 2) + 4) * 3;

  for k := 1 to k do
    begin
      a := LRot32(KeyContext[i] + a + b, 3);
      KeyContext[i] := a;
      b := LRot32(xKeyD[j] + a + b, a + b);
      xKeyD[j] := b;
      i := (i + 1) mod ((cRC6_NumRounds * 2) + 4);
      j := (j + 1) mod xKeyLen;
    end;
end;

class procedure TRC6.Encrypt(var KeyContext: TRC6Key; var Data: TRC6Block);
var
  x0, x1, x2, x3: PDWORD;
  u, t, i: DWORD;
begin
{$IFDEF RangeCheck}{$R-}{$ENDIF}
  x0 := PDWORD(@Data[0]);
  x1 := PDWORD(@Data[4]);
  x2 := PDWORD(@Data[8]);
  x3 := PDWORD(@Data[12]);

  x1^ := x1^ + KeyContext[0];
  x3^ := x3^ + KeyContext[1];
  for i := 1 to cRC6_NumRounds do
    begin
      t := LRot32(x1^ * (2 * x1^ + 1), 5);
      u := LRot32(x3^ * (2 * x3^ + 1), 5);
      x0^ := LRot32(x0^ xor t, u) + KeyContext[2 * i];
      x2^ := LRot32(x2^ xor u, t) + KeyContext[2 * i + 1];
      t := x0^;
      x0^ := x1^;
      x1^ := x2^;
      x2^ := x3^;
      x3^ := t;
    end;
  x0^ := x0^ + KeyContext[(2 * cRC6_NumRounds) + 2];
  x2^ := x2^ + KeyContext[(2 * cRC6_NumRounds) + 3];
{$IFDEF RangeCheck}{$R+}{$ENDIF}
end;

class procedure TRC6.Decrypt(var KeyContext: TRC6Key; var Data: TRC6Block);
var
  x0, x1, x2, x3: PDWORD;
  u, t, i: DWORD;
begin
{$IFDEF RangeCheck}{$R-}{$ENDIF}
  x0 := PDWORD(@Data[0]);
  x1 := PDWORD(@Data[4]);
  x2 := PDWORD(@Data[8]);
  x3 := PDWORD(@Data[12]);

  x2^ := x2^ - KeyContext[(2 * cRC6_NumRounds) + 3];
  x0^ := x0^ - KeyContext[(2 * cRC6_NumRounds) + 2];
  for i := cRC6_NumRounds downto 1 do
    begin
      t := x0^;
      x0^ := x3^;
      x3^ := x2^;
      x2^ := x1^;
      x1^ := t;
      u := LRot32(x3^ * (2 * x3^ + 1), 5);
      t := LRot32(x1^ * (2 * x1^ + 1), 5);
      x2^ := RRot32(x2^ - KeyContext[2 * i + 1], t) xor u;
      x0^ := RRot32(x0^ - KeyContext[2 * i], u) xor t;
    end;
  x3^ := x3^ - KeyContext[1];
  x1^ := x1^ - KeyContext[0];
{$IFDEF RangeCheck}{$R+}{$ENDIF}
end;

class procedure TSerpent.InitKey(buff: Pointer; Size: Integer; var KeyContext: TSerpentkey);
var
  kp: array [0 .. 139] of DWORD;
  i, n: DWORD;
  t0, T1, T2, T3, T4, T5, T6, T7, T8, t9, t10, t11, t12, t13, t14, t15, t16, t17: DWORD;
  a, b, c, d: DWORD;
begin
  FillPtrByte(@kp[0], SizeOf(kp), 0);
  CopyPtr(buff, @kp[0], Size);
  for i := 8 to 139 do
    begin
      t0 := kp[i - 8] xor kp[i - 5] xor kp[i - 3] xor kp[i - 1] xor $9E3779B9 xor DWORD(i - 8);
      kp[i] := (t0 shl 11) or (t0 shr 21);
    end;
  for i := 0 to 3 do
    begin
      n := i * 32;
      a := kp[n + 4 * 0 + 8];
      b := kp[n + 4 * 0 + 9];
      c := kp[n + 4 * 0 + 10];
      d := kp[n + 4 * 0 + 11];
      T1 := a xor c;
      T2 := a or d;
      T3 := a and b;
      T4 := a and d;
      T5 := b or T4;
      T6 := T1 and T2;
      kp[9 + n] := T5 xor T6;
      T8 := b xor d;
      t9 := c or T3;
      t10 := T6 xor T8;
      kp[11 + n] := t9 xor t10;
      t12 := c xor T3;
      t13 := T2 and kp[11 + n];
      kp[10 + n] := t12 xor t13;
      t15 := not kp[10 + n];
      t16 := T2 xor T3;
      t17 := kp[9 + n] and t15;
      kp[8 + n] := t16 xor t17;
      a := kp[n + 4 * 1 + 8];
      b := kp[n + 4 * 1 + 9];
      c := kp[n + 4 * 1 + 10];
      d := kp[n + 4 * 1 + 11];
      T1 := not a;
      T2 := b xor d;
      T3 := c and T1;
      kp[12 + n] := T2 xor T3;
      T5 := c xor T1;
      T6 := c xor kp[12 + n];
      T7 := b and T6;
      kp[15 + n] := T5 xor T7;
      t9 := d or T7;
      t10 := kp[12 + n] or T5;
      t11 := t9 and t10;
      kp[14 + n] := a xor t11;
      t13 := d or T1;
      t14 := T2 xor kp[15 + n];
      t15 := kp[14 + n] xor t13;
      kp[13 + n] := t14 xor t15;
      a := kp[n + 4 * 2 + 8];
      b := kp[n + 4 * 2 + 9];
      c := kp[n + 4 * 2 + 10];
      d := kp[n + 4 * 2 + 11];
      T1 := a xor d;
      T2 := b xor d;
      T3 := a and b;
      T4 := not c;
      T5 := T2 xor T3;
      kp[18 + n] := T4 xor T5;
      T7 := a xor T2;
      T8 := b or T4;
      t9 := d or kp[18 + n];
      t10 := T7 and t9;
      kp[17 + n] := T8 xor t10;
      t12 := c xor d;
      t13 := T1 or T2;
      t14 := kp[17 + n] xor t12;
      kp[19 + n] := t13 xor t14;
      t16 := T1 or kp[18 + n];
      t17 := T8 xor t14;
      kp[16 + n] := t16 xor t17;
      a := kp[n + 4 * 3 + 8];
      b := kp[n + 4 * 3 + 9];
      c := kp[n + 4 * 3 + 10];
      d := kp[n + 4 * 3 + 11];
      T1 := b xor d;
      T2 := not T1;
      T3 := a or d;
      T4 := b xor c;
      kp[23 + n] := T3 xor T4;
      T6 := a xor b;
      T7 := a or T4;
      T8 := c and T6;
      t9 := T2 or T8;
      kp[20 + n] := T7 xor t9;
      t11 := a xor kp[23 + n];
      t12 := T1 and T6;
      t13 := kp[20 + n] xor t11;
      kp[21 + n] := t12 xor t13;
      t15 := kp[20 + n] or kp[21 + n];
      t16 := T3 and t15;
      kp[22 + n] := b xor t16;
      a := kp[n + 4 * 4 + 8];
      b := kp[n + 4 * 4 + 9];
      c := kp[n + 4 * 4 + 10];
      d := kp[n + 4 * 4 + 11];
      T1 := not c;
      T2 := b xor c;
      T3 := b or T1;
      T4 := d xor T3;
      T5 := a and T4;
      kp[27 + n] := T2 xor T5;
      T7 := a xor d;
      T8 := b xor T5;
      t9 := T2 or T8;
      kp[25 + n] := T7 xor t9;
      t11 := d and T3;
      t12 := T5 xor kp[25 + n];
      t13 := kp[27 + n] and t12;
      kp[26 + n] := t11 xor t13;
      t15 := T1 or T4;
      t16 := t12 xor kp[26 + n];
      kp[24 + n] := t15 xor t16;
      a := kp[n + 4 * 5 + 8];
      b := kp[n + 4 * 5 + 9];
      c := kp[n + 4 * 5 + 10];
      d := kp[n + 4 * 5 + 11];
      T1 := a xor c;
      T2 := b or d;
      T3 := b xor c;
      T4 := not T3;
      T5 := a and d;
      kp[29 + n] := T4 xor T5;
      T7 := b or c;
      T8 := d xor T1;
      t9 := T7 and T8;
      kp[31 + n] := T2 xor t9;
      t11 := T1 and T7;
      t12 := T4 xor T8;
      t13 := kp[31 + n] and t11;
      kp[28 + n] := t12 xor t13;
      t15 := T3 xor t11;
      t16 := kp[31 + n] or t15;
      kp[30 + n] := t12 xor t16;
      a := kp[n + 4 * 6 + 8];
      b := kp[n + 4 * 6 + 9];
      c := kp[n + 4 * 6 + 10];
      d := kp[n + 4 * 6 + 11];
      T1 := not a;
      T2 := a xor b;
      T3 := a xor d;
      T4 := c xor T1;
      T5 := T2 or T3;
      kp[32 + n] := T4 xor T5;
      T7 := not d;
      T8 := kp[32 + n] and T7;
      kp[33 + n] := T2 xor T8;
      t10 := b or kp[33 + n];
      t11 := c or kp[32 + n];
      t12 := T7 xor t10;
      kp[35 + n] := t11 xor t12;
      t14 := d or kp[33 + n];
      t15 := T1 xor t14;
      t16 := kp[32 + n] or kp[35 + n];
      kp[34 + n] := t15 xor t16;
      a := kp[n + 4 * 7 + 8];
      b := kp[n + 4 * 7 + 9];
      c := kp[n + 4 * 7 + 10];
      d := kp[n + 4 * 7 + 11];
      T1 := not a;
      T2 := a xor d;
      T3 := a xor b;
      T4 := c xor T1;
      T5 := T2 or T3;
      kp[36 + n] := T4 xor T5;
      T7 := not kp[36 + n];
      T8 := b or T7;
      kp[39 + n] := T2 xor T8;
      t10 := a and kp[36 + n];
      t11 := b xor kp[39 + n];
      t12 := T8 and t11;
      kp[38 + n] := t10 xor t12;
      t14 := a or T7;
      t15 := T3 xor t14;
      t16 := kp[39 + n] and kp[38 + n];
      kp[37 + n] := t15 xor t16;
    end;
  a := kp[136];
  b := kp[137];
  c := kp[138];
  d := kp[139];
  T1 := a xor c;
  T2 := a or d;
  T3 := a and b;
  T4 := a and d;
  T5 := b or T4;
  T6 := T1 and T2;
  kp[137] := T5 xor T6;
  T8 := b xor d;
  t9 := c or T3;
  t10 := T6 xor T8;
  kp[139] := t9 xor t10;
  t12 := c xor T3;
  t13 := T2 and kp[139];
  kp[138] := t12 xor t13;
  t15 := not kp[138];
  t16 := T2 xor T3;
  t17 := kp[137] and t15;
  kp[136] := t16 xor t17;
  CopyPtr(@kp, @KeyContext, SizeOf(KeyContext));
end;

class procedure TSerpent.Encrypt(var KeyContext: TSerpentkey; var Data: TSerpentBlock);
var
  i: DWORD;
  a, b, c, d, E, f, g, h: DWORD;
  T1, T2, T3, T4, T5, T6, T7, T8, t9, t10, t11, t12, t13, t14, t15, t16, t17: DWORD;
begin
  a := PDWORD(@Data[0])^;
  b := PDWORD(@Data[4])^;
  c := PDWORD(@Data[8])^;
  d := PDWORD(@Data[12])^;

  i := 0;
  while i < 32 do
    begin
      a := a xor KeyContext[4 * (i)];
      b := b xor KeyContext[4 * (i) + 1];
      c := c xor KeyContext[4 * (i) + 2];
      d := d xor KeyContext[4 * (i) + 3];
      T1 := b xor d;
      T2 := not T1;
      T3 := a or d;
      T4 := b xor c;
      h := T3 xor T4;
      T6 := a xor b;
      T7 := a or T4;
      T8 := c and T6;
      t9 := T2 or T8;
      E := T7 xor t9;
      t11 := a xor h;
      t12 := T1 and T6;
      t13 := E xor t11;
      f := t12 xor t13;
      t15 := E or f;
      t16 := T3 and t15;
      g := b xor t16;
      E := (E shl 13) or (E shr 19);
      g := (g shl 3) or (g shr 29);
      f := f xor E xor g;
      h := h xor g xor (E shl 3);
      f := (f shl 1) or (f shr 31);
      h := (h shl 7) or (h shr 25);
      E := E xor f xor h;
      g := g xor h xor (f shl 7);
      E := (E shl 5) or (E shr 27);
      g := (g shl 22) or (g shr 10);
      E := E xor KeyContext[4 * (i + 1)];
      f := f xor KeyContext[4 * (i + 1) + 1];
      g := g xor KeyContext[4 * (i + 1) + 2];
      h := h xor KeyContext[4 * (i + 1) + 3];
      T1 := E xor h;
      T2 := f xor h;
      T3 := E and f;
      T4 := not g;
      T5 := T2 xor T3;
      c := T4 xor T5;
      T7 := E xor T2;
      T8 := f or T4;
      t9 := h or c;
      t10 := T7 and t9;
      b := T8 xor t10;
      t12 := g xor h;
      t13 := T1 or T2;
      t14 := b xor t12;
      d := t13 xor t14;
      t16 := T1 or c;
      t17 := T8 xor t14;
      a := t16 xor t17;
      a := (a shl 13) or (a shr 19);
      c := (c shl 3) or (c shr 29);
      b := b xor a xor c;
      d := d xor c xor (a shl 3);
      b := (b shl 1) or (b shr 31);
      d := (d shl 7) or (d shr 25);
      a := a xor b xor d;
      c := c xor d xor (b shl 7);
      a := (a shl 5) or (a shr 27);
      c := (c shl 22) or (c shr 10);
      a := a xor KeyContext[4 * (i + 2)];
      b := b xor KeyContext[4 * (i + 2) + 1];
      c := c xor KeyContext[4 * (i + 2) + 2];
      d := d xor KeyContext[4 * (i + 2) + 3];
      T1 := not a;
      T2 := b xor d;
      T3 := c and T1;
      E := T2 xor T3;
      T5 := c xor T1;
      T6 := c xor E;
      T7 := b and T6;
      h := T5 xor T7;
      t9 := d or T7;
      t10 := E or T5;
      t11 := t9 and t10;
      g := a xor t11;
      t13 := d or T1;
      t14 := T2 xor h;
      t15 := g xor t13;
      f := t14 xor t15;
      E := (E shl 13) or (E shr 19);
      g := (g shl 3) or (g shr 29);
      f := f xor E xor g;
      h := h xor g xor (E shl 3);
      f := (f shl 1) or (f shr 31);
      h := (h shl 7) or (h shr 25);
      E := E xor f xor h;
      g := g xor h xor (f shl 7);
      E := (E shl 5) or (E shr 27);
      g := (g shl 22) or (g shr 10);
      E := E xor KeyContext[4 * (i + 3)];
      f := f xor KeyContext[4 * (i + 3) + 1];
      g := g xor KeyContext[4 * (i + 3) + 2];
      h := h xor KeyContext[4 * (i + 3) + 3];
      T1 := E xor g;
      T2 := E or h;
      T3 := E and f;
      T4 := E and h;
      T5 := f or T4;
      T6 := T1 and T2;
      b := T5 xor T6;
      T8 := f xor h;
      t9 := g or T3;
      t10 := T6 xor T8;
      d := t9 xor t10;
      t12 := g xor T3;
      t13 := T2 and d;
      c := t12 xor t13;
      t15 := not c;
      t16 := T2 xor T3;
      t17 := b and t15;
      a := t16 xor t17;
      a := (a shl 13) or (a shr 19);
      c := (c shl 3) or (c shr 29);
      b := b xor a xor c;
      d := d xor c xor (a shl 3);
      b := (b shl 1) or (b shr 31);
      d := (d shl 7) or (d shr 25);
      a := a xor b xor d;
      c := c xor d xor (b shl 7);
      a := (a shl 5) or (a shr 27);
      c := (c shl 22) or (c shr 10);
      a := a xor KeyContext[4 * (i + 4)];
      b := b xor KeyContext[4 * (i + 4) + 1];
      c := c xor KeyContext[4 * (i + 4) + 2];
      d := d xor KeyContext[4 * (i + 4) + 3];
      T1 := not a;
      T2 := a xor d;
      T3 := a xor b;
      T4 := c xor T1;
      T5 := T2 or T3;
      E := T4 xor T5;
      T7 := not E;
      T8 := b or T7;
      h := T2 xor T8;
      t10 := a and E;
      t11 := b xor h;
      t12 := T8 and t11;
      g := t10 xor t12;
      t14 := a or T7;
      t15 := T3 xor t14;
      t16 := h and g;
      f := t15 xor t16;
      E := (E shl 13) or (E shr 19);
      g := (g shl 3) or (g shr 29);
      f := f xor E xor g;
      h := h xor g xor (E shl 3);
      f := (f shl 1) or (f shr 31);
      h := (h shl 7) or (h shr 25);
      E := E xor f xor h;
      g := g xor h xor (f shl 7);
      E := (E shl 5) or (E shr 27);
      g := (g shl 22) or (g shr 10);
      E := E xor KeyContext[4 * (i + 5)];
      f := f xor KeyContext[4 * (i + 5) + 1];
      g := g xor KeyContext[4 * (i + 5) + 2];
      h := h xor KeyContext[4 * (i + 5) + 3];
      T1 := not E;
      T2 := E xor f;
      T3 := E xor h;
      T4 := g xor T1;
      T5 := T2 or T3;
      a := T4 xor T5;
      T7 := not h;
      T8 := a and T7;
      b := T2 xor T8;
      t10 := f or b;
      t11 := g or a;
      t12 := T7 xor t10;
      d := t11 xor t12;
      t14 := h or b;
      t15 := T1 xor t14;
      t16 := a or d;
      c := t15 xor t16;
      a := (a shl 13) or (a shr 19);
      c := (c shl 3) or (c shr 29);
      b := b xor a xor c;
      d := d xor c xor (a shl 3);
      b := (b shl 1) or (b shr 31);
      d := (d shl 7) or (d shr 25);
      a := a xor b xor d;
      c := c xor d xor (b shl 7);
      a := (a shl 5) or (a shr 27);
      c := (c shl 22) or (c shr 10);
      a := a xor KeyContext[4 * (i + 6)];
      b := b xor KeyContext[4 * (i + 6) + 1];
      c := c xor KeyContext[4 * (i + 6) + 2];
      d := d xor KeyContext[4 * (i + 6) + 3];
      T1 := a xor c;
      T2 := b or d;
      T3 := b xor c;
      T4 := not T3;
      T5 := a and d;
      f := T4 xor T5;
      T7 := b or c;
      T8 := d xor T1;
      t9 := T7 and T8;
      h := T2 xor t9;
      t11 := T1 and T7;
      t12 := T4 xor T8;
      t13 := h and t11;
      E := t12 xor t13;
      t15 := T3 xor t11;
      t16 := h or t15;
      g := t12 xor t16;
      E := (E shl 13) or (E shr 19);
      g := (g shl 3) or (g shr 29);
      f := f xor E xor g;
      h := h xor g xor (E shl 3);
      f := (f shl 1) or (f shr 31);
      h := (h shl 7) or (h shr 25);
      E := E xor f xor h;
      g := g xor h xor (f shl 7);
      E := (E shl 5) or (E shr 27);
      g := (g shl 22) or (g shr 10);
      E := E xor KeyContext[4 * (i + 7)];
      f := f xor KeyContext[4 * (i + 7) + 1];
      g := g xor KeyContext[4 * (i + 7) + 2];
      h := h xor KeyContext[4 * (i + 7) + 3];
      T1 := not g;
      T2 := f xor g;
      T3 := f or T1;
      T4 := h xor T3;
      T5 := E and T4;
      d := T2 xor T5;
      T7 := E xor h;
      T8 := f xor T5;
      t9 := T2 or T8;
      b := T7 xor t9;
      t11 := h and T3;
      t12 := T5 xor b;
      t13 := d and t12;
      c := t11 xor t13;
      t15 := T1 or T4;
      t16 := t12 xor c;
      a := t15 xor t16;

      inc(i, 8);
      if i < 32 then
        begin
          a := (a shl 13) or (a shr 19);
          c := (c shl 3) or (c shr 29);
          b := b xor a xor c;
          d := d xor c xor (a shl 3);
          b := (b shl 1) or (b shr 31);
          d := (d shl 7) or (d shr 25);
          a := a xor b xor d;
          c := c xor d xor (b shl 7);
          a := (a shl 5) or (a shr 27);
          c := (c shl 22) or (c shr 10);
        end;
    end;
  a := a xor KeyContext[128];
  b := b xor KeyContext[128 + 1];
  c := c xor KeyContext[128 + 2];
  d := d xor KeyContext[128 + 3];

  PDWORD(@Data[0])^ := a;
  PDWORD(@Data[4])^ := b;
  PDWORD(@Data[8])^ := c;
  PDWORD(@Data[12])^ := d;
end;

class procedure TSerpent.Decrypt(var KeyContext: TSerpentkey; var Data: TSerpentBlock);
var
  i: DWORD;
  a, b, c, d, E, f, g, h: DWORD;
  T1, T2, T3, T4, T5, T6, T7, T8, t9, t10, t11, t12, t13, t14, t15, t16: DWORD;
begin
  a := PDWORD(@Data[0])^;
  b := PDWORD(@Data[4])^;
  c := PDWORD(@Data[8])^;
  d := PDWORD(@Data[12])^;

  i := 32;
  a := a xor KeyContext[4 * 32];
  b := b xor KeyContext[4 * 32 + 1];
  c := c xor KeyContext[4 * 32 + 2];
  d := d xor KeyContext[4 * 32 + 3];
  while i > 0 do
    begin
      if i < 32 then
        begin
          c := (c shr 22) or (c shl 10);
          a := (a shr 5) or (a shl 27);
          c := c xor d xor (b shl 7);
          a := a xor b xor d;
          d := (d shr 7) or (d shl 25);
          b := (b shr 1) or (b shl 31);
          d := d xor c xor (a shl 3);
          b := b xor a xor c;
          c := (c shr 3) or (c shl 29);
          a := (a shr 13) or (a shl 19);
        end;

      T1 := a and b;
      T2 := a or b;
      T3 := c or T1;
      T4 := d and T2;
      h := T3 xor T4;
      T6 := not d;
      T7 := b xor T4;
      T8 := h xor T6;
      t9 := T7 or T8;
      f := a xor t9;
      t11 := c xor T7;
      t12 := d or f;
      E := t11 xor t12;
      t14 := a and h;
      t15 := T3 xor f;
      t16 := E xor t14;
      g := t15 xor t16;
      E := E xor KeyContext[4 * (i - 1)];
      f := f xor KeyContext[4 * (i - 1) + 1];
      g := g xor KeyContext[4 * (i - 1) + 2];
      h := h xor KeyContext[4 * (i - 1) + 3];
      g := (g shr 22) or (g shl 10);
      E := (E shr 5) or (E shl 27);
      g := g xor h xor (f shl 7);
      E := E xor f xor h;
      h := (h shr 7) or (h shl 25);
      f := (f shr 1) or (f shl 31);
      h := h xor g xor (E shl 3);
      f := f xor E xor g;
      g := (g shr 3) or (g shl 29);
      E := (E shr 13) or (E shl 19);
      T1 := not g;
      T2 := E xor g;
      T3 := f xor h;
      T4 := E or T1;
      b := T3 xor T4;
      T6 := E or f;
      T7 := f and T2;
      T8 := b xor T6;
      t9 := T7 or T8;
      a := g xor t9;
      t11 := not b;
      t12 := h or T2;
      t13 := t9 xor t11;
      d := t12 xor t13;
      t15 := f xor t11;
      t16 := a and d;
      c := t15 xor t16;
      a := a xor KeyContext[4 * (i - 2)];
      b := b xor KeyContext[4 * (i - 2) + 1];
      c := c xor KeyContext[4 * (i - 2) + 2];
      d := d xor KeyContext[4 * (i - 2) + 3];
      c := (c shr 22) or (c shl 10);
      a := (a shr 5) or (a shl 27);
      c := c xor d xor (b shl 7);
      a := a xor b xor d;
      d := (d shr 7) or (d shl 25);
      b := (b shr 1) or (b shl 31);
      d := d xor c xor (a shl 3);
      b := b xor a xor c;
      c := (c shr 3) or (c shl 29);
      a := (a shr 13) or (a shl 19);
      T1 := not c;
      T2 := b and T1;
      T3 := d xor T2;
      T4 := a and T3;
      T5 := b xor T1;
      h := T4 xor T5;
      T7 := b or h;
      T8 := a and T7;
      f := T3 xor T8;
      t10 := a or d;
      t11 := T1 xor T7;
      E := t10 xor t11;
      t13 := a xor c;
      t14 := b and t10;
      t15 := T4 or t13;
      g := t14 xor t15;
      E := E xor KeyContext[4 * (i - 3)];
      f := f xor KeyContext[4 * (i - 3) + 1];
      g := g xor KeyContext[4 * (i - 3) + 2];
      h := h xor KeyContext[4 * (i - 3) + 3];
      g := (g shr 22) or (g shl 10);
      E := (E shr 5) or (E shl 27);
      g := g xor h xor (f shl 7);
      E := E xor f xor h;
      h := (h shr 7) or (h shl 25);
      f := (f shr 1) or (f shl 31);
      h := h xor g xor (E shl 3);
      f := f xor E xor g;
      g := (g shr 3) or (g shl 29);
      E := (E shr 13) or (E shl 19);
      T1 := g xor h;
      T2 := g or h;
      T3 := f xor T2;
      T4 := E and T3;
      b := T1 xor T4;
      T6 := E xor h;
      T7 := f or h;
      T8 := T6 and T7;
      d := T3 xor T8;
      t10 := not E;
      t11 := g xor d;
      t12 := t10 or t11;
      a := T3 xor t12;
      t14 := g or T4;
      t15 := T7 xor t14;
      t16 := d or t10;
      c := t15 xor t16;
      a := a xor KeyContext[4 * (i - 4)];
      b := b xor KeyContext[4 * (i - 4) + 1];
      c := c xor KeyContext[4 * (i - 4) + 2];
      d := d xor KeyContext[4 * (i - 4) + 3];
      c := (c shr 22) or (c shl 10);
      a := (a shr 5) or (a shl 27);
      c := c xor d xor (b shl 7);
      a := a xor b xor d;
      d := (d shr 7) or (d shl 25);
      b := (b shr 1) or (b shl 31);
      d := d xor c xor (a shl 3);
      b := b xor a xor c;
      c := (c shr 3) or (c shl 29);
      a := (a shr 13) or (a shl 19);
      T1 := b xor c;
      T2 := b or c;
      T3 := a xor c;
      T4 := T2 xor T3;
      T5 := d or T4;
      E := T1 xor T5;
      T7 := a xor d;
      T8 := T1 or T5;
      t9 := T2 xor T7;
      g := T8 xor t9;
      t11 := a and T4;
      t12 := E or t9;
      f := t11 xor t12;
      t14 := a and g;
      t15 := T2 xor t14;
      t16 := E and t15;
      h := T4 xor t16;
      E := E xor KeyContext[4 * (i - 5)];
      f := f xor KeyContext[4 * (i - 5) + 1];
      g := g xor KeyContext[4 * (i - 5) + 2];
      h := h xor KeyContext[4 * (i - 5) + 3];
      g := (g shr 22) or (g shl 10);
      E := (E shr 5) or (E shl 27);
      g := g xor h xor (f shl 7);
      E := E xor f xor h;
      h := (h shr 7) or (h shl 25);
      f := (f shr 1) or (f shl 31);
      h := h xor g xor (E shl 3);
      f := f xor E xor g;
      g := (g shr 3) or (g shl 29);
      E := (E shr 13) or (E shl 19);
      T1 := f xor h;
      T2 := not T1;
      T3 := E xor g;
      T4 := g xor T1;
      T5 := f and T4;
      a := T3 xor T5;
      T7 := E or T2;
      T8 := h xor T7;
      t9 := T3 or T8;
      d := T1 xor t9;
      t11 := not T4;
      t12 := a or d;
      b := t11 xor t12;
      t14 := h and t11;
      t15 := T3 xor t12;
      c := t14 xor t15;
      a := a xor KeyContext[4 * (i - 6)];
      b := b xor KeyContext[4 * (i - 6) + 1];
      c := c xor KeyContext[4 * (i - 6) + 2];
      d := d xor KeyContext[4 * (i - 6) + 3];
      c := (c shr 22) or (c shl 10);
      a := (a shr 5) or (a shl 27);
      c := c xor d xor (b shl 7);
      a := a xor b xor d;
      d := (d shr 7) or (d shl 25);
      b := (b shr 1) or (b shl 31);
      d := d xor c xor (a shl 3);
      b := b xor a xor c;
      c := (c shr 3) or (c shl 29);
      a := (a shr 13) or (a shl 19);
      T1 := a xor d;
      T2 := a and b;
      T3 := b xor c;
      T4 := a xor T3;
      T5 := b or d;
      h := T4 xor T5;
      T7 := c or T1;
      T8 := b xor T7;
      t9 := T4 and T8;
      f := T1 xor t9;
      t11 := not T2;
      t12 := h and f;
      t13 := t9 xor t11;
      g := t12 xor t13;
      t15 := a and d;
      t16 := c xor t13;
      E := t15 xor t16;
      E := E xor KeyContext[4 * (i - 7)];
      f := f xor KeyContext[4 * (i - 7) + 1];
      g := g xor KeyContext[4 * (i - 7) + 2];
      h := h xor KeyContext[4 * (i - 7) + 3];
      g := (g shr 22) or (g shl 10);
      E := (E shr 5) or (E shl 27);
      g := g xor h xor (f shl 7);
      E := E xor f xor h;
      h := (h shr 7) or (h shl 25);
      f := (f shr 1) or (f shl 31);
      h := h xor g xor (E shl 3);
      f := f xor E xor g;
      g := (g shr 3) or (g shl 29);
      E := (E shr 13) or (E shl 19);
      T1 := E xor h;
      T2 := g xor h;
      T3 := not T2;
      T4 := E or f;
      c := T3 xor T4;
      T6 := f xor T1;
      T7 := g or T6;
      T8 := E xor T7;
      t9 := T2 and T8;
      b := T6 xor t9;
      t11 := not T8;
      t12 := f and h;
      t13 := b or t12;
      d := t11 xor t13;
      t15 := T2 xor t12;
      t16 := b or d;
      a := t15 xor t16;
      a := a xor KeyContext[4 * (i - 8)];
      b := b xor KeyContext[4 * (i - 8) + 1];
      c := c xor KeyContext[4 * (i - 8) + 2];
      d := d xor KeyContext[4 * (i - 8) + 3];
      dec(i, 8);
    end;

  PDWORD(@Data[0])^ := a;
  PDWORD(@Data[4])^ := b;
  PDWORD(@Data[8])^ := c;
  PDWORD(@Data[12])^ := d;
end;

class procedure TMars.gen_mask(var x, m: DWORD);
var
  u: DWORD;
begin
  u := x and (x shr 1);
  u := u and (u shr 2);
  u := u and (u shr 4);
  u := u and (u shr 1) and (u shr 2);
  m := u;
  u := (x xor $FFFFFFFF) and ((x xor $FFFFFFFF) shr 1);
  u := u and (u shr 2);
  u := u and (u shr 4);
  u := u and (u shr 1) and (u shr 2);
  u := u or m;
  m := (u shl 1) or (u shl 2) or (u shl 3) or (u shl 4) or (u shl 5) or (u shl 6) or (u shl 7) or (u shl 8);
  m := (m or u or (u shl 9)) and ((x xor $FFFFFFFF) xor (x shl 1)) and ((x xor $FFFFFFFF) xor (x shr 1));
  m := m and $FFFFFFFC;
end;

class procedure TMars.InitKey(buff: Pointer; Size: Integer; var KeyContext: TMarskey);
const
  vk: array [0 .. 6] of DWORD = ($09D0C479, $28C8FFE0, $84AA6C39, $9DAD7287, $7DFF9BE3, $D4268361, $C96DA1D4);
var
  i, j: Integer;
  m, u, w, siz4: DWORD;
  t: array [-7 .. 39] of DWORD;
  KeyB: array [0 .. 39] of DWORD;
begin
  m := 0;
  FillPtrByte(@KeyB, SizeOf(KeyB), 0);
  FillPtrByte(@t, SizeOf(t), 0);

  CopyPtr(buff, @KeyB, Size);
  siz4 := Size div 4;
  CopyPtr(@vk, @t, SizeOf(vk));
  for i := 0 to 38 do
    begin
      u := t[i - 7] xor t[i - 2];
      t[i] := TRC6.LRot32(u, 3) xor KeyB[i mod siz4] xor i;
    end;
  t[39] := siz4;
  for j := 0 to 6 do
    begin
      for i := 1 to 39 do
        begin
          u := t[i] + Mars_SBox[t[i - 1] and $1FF];
          t[i] := TRC6.LRot32(u, 9);
        end;
      u := t[0] + Mars_SBox[t[39] and $1FF];
      t[0] := TRC6.LRot32(u, 9);
    end;
  for i := 0 to 39 do
      KeyContext[(7 * i) mod 40] := t[i];
  i := 5;
  repeat
    u := Mars_SBox[265 + (KeyContext[i] and $3)];
    j := KeyContext[i + 3] and $1F;
    w := KeyContext[i] or $3;
    gen_mask(w, m);
    KeyContext[i] := w xor (TRC6.LRot32(u, j) and m);
    inc(i, 2);
  until i >= 37;
end;

class procedure TMars.Encrypt(var KeyContext: TMarskey; var Data: TMarsBlock);
var
  l, m, r, t, a, b, c, d: DWORD;
begin
{$IFDEF RangeCheck}{$R-}{$ENDIF}
  a := PDWORD(@Data[0])^;
  b := PDWORD(@Data[4])^;
  c := PDWORD(@Data[8])^;
  d := PDWORD(@Data[12])^;

  a := a + KeyContext[0];
  b := b + KeyContext[1];
  c := c + KeyContext[2];
  d := d + KeyContext[3];
  b := b xor Mars_SBox[a and $FF];
  b := b + Mars_SBox[((a shr 8) and $FF) + 256];
  c := c + Mars_SBox[(a shr 16) and $FF];
  d := d xor Mars_SBox[((a shr 24) and $FF) + 256];
  a := TRC6.RRot32(a, 24);
  a := a + d;
  c := c xor Mars_SBox[b and $FF];
  c := c + Mars_SBox[((b shr 8) and $FF) + 256];
  d := d + Mars_SBox[(b shr 16) and $FF];
  a := a xor Mars_SBox[((b shr 24) and $FF) + 256];
  b := TRC6.RRot32(b, 24);
  b := b + c;
  d := d xor Mars_SBox[c and $FF];
  d := d + Mars_SBox[((c shr 8) and $FF) + 256];
  a := a + Mars_SBox[(c shr 16) and $FF];
  b := b xor Mars_SBox[((c shr 24) and $FF) + 256];
  c := TRC6.RRot32(c, 24);
  a := a xor Mars_SBox[d and $FF];
  a := a + Mars_SBox[((d shr 8) and $FF) + 256];
  b := b + Mars_SBox[(d shr 16) and $FF];
  c := c xor Mars_SBox[((d shr 24) and $FF) + 256];
  d := TRC6.RRot32(d, 24);
  b := b xor Mars_SBox[a and $FF];
  b := b + Mars_SBox[((a shr 8) and $FF) + 256];
  c := c + Mars_SBox[(a shr 16) and $FF];
  d := d xor Mars_SBox[((a shr 24) and $FF) + 256];
  a := TRC6.RRot32(a, 24);
  a := a + d;
  c := c xor Mars_SBox[b and $FF];
  c := c + Mars_SBox[((b shr 8) and $FF) + 256];
  d := d + Mars_SBox[(b shr 16) and $FF];
  a := a xor Mars_SBox[((b shr 24) and $FF) + 256];
  b := TRC6.RRot32(b, 24);
  b := b + c;
  d := d xor Mars_SBox[c and $FF];
  d := d + Mars_SBox[((c shr 8) and $FF) + 256];
  a := a + Mars_SBox[(c shr 16) and $FF];
  b := b xor Mars_SBox[((c shr 24) and $FF) + 256];
  c := TRC6.RRot32(c, 24);
  a := a xor Mars_SBox[d and $FF];
  a := a + Mars_SBox[((d shr 8) and $FF) + 256];
  b := b + Mars_SBox[(d shr 16) and $FF];
  c := c xor Mars_SBox[((d shr 24) and $FF) + 256];
  d := TRC6.RRot32(d, 24);
  m := a + KeyContext[4];
  r := TRC6.LRot32(a, 13) * KeyContext[5];
  l := Mars_SBox[m and $1FF];
  r := TRC6.LRot32(r, 5);
  t := r and $1F;
  m := TRC6.LRot32(m, t);
  l := l xor r;
  r := TRC6.LRot32(r, 5);
  l := l xor r;
  t := r and $1F;
  l := TRC6.LRot32(l, t);
  a := TRC6.LRot32(a, 13);
  b := b + l;
  c := c + m;
  d := d xor r;
  m := b + KeyContext[6];
  r := TRC6.LRot32(b, 13) * KeyContext[7];
  l := Mars_SBox[m and $1FF];
  r := TRC6.LRot32(r, 5);
  t := r and $1F;
  m := TRC6.LRot32(m, t);
  l := l xor r;
  r := TRC6.LRot32(r, 5);
  l := l xor r;
  t := r and $1F;
  l := TRC6.LRot32(l, t);
  b := TRC6.LRot32(b, 13);
  c := c + l;
  d := d + m;
  a := a xor r;
  m := c + KeyContext[8];
  r := TRC6.LRot32(c, 13) * KeyContext[9];
  l := Mars_SBox[m and $1FF];
  r := TRC6.LRot32(r, 5);
  t := r and $1F;
  m := TRC6.LRot32(m, t);
  l := l xor r;
  r := TRC6.LRot32(r, 5);
  l := l xor r;
  t := r and $1F;
  l := TRC6.LRot32(l, t);
  c := TRC6.LRot32(c, 13);
  d := d + l;
  a := a + m;
  b := b xor r;
  m := d + KeyContext[10];
  r := TRC6.LRot32(d, 13) * KeyContext[11];
  l := Mars_SBox[m and $1FF];
  r := TRC6.LRot32(r, 5);
  t := r and $1F;
  m := TRC6.LRot32(m, t);
  l := l xor r;
  r := TRC6.LRot32(r, 5);
  l := l xor r;
  t := r and $1F;
  l := TRC6.LRot32(l, t);
  d := TRC6.LRot32(d, 13);
  a := a + l;
  b := b + m;
  c := c xor r;
  m := a + KeyContext[12];
  r := TRC6.LRot32(a, 13) * KeyContext[13];
  l := Mars_SBox[m and $1FF];
  r := TRC6.LRot32(r, 5);
  t := r and $1F;
  m := TRC6.LRot32(m, t);
  l := l xor r;
  r := TRC6.LRot32(r, 5);
  l := l xor r;
  t := r and $1F;
  l := TRC6.LRot32(l, t);
  a := TRC6.LRot32(a, 13);
  b := b + l;
  c := c + m;
  d := d xor r;
  m := b + KeyContext[14];
  r := TRC6.LRot32(b, 13) * KeyContext[15];
  l := Mars_SBox[m and $1FF];
  r := TRC6.LRot32(r, 5);
  t := r and $1F;
  m := TRC6.LRot32(m, t);
  l := l xor r;
  r := TRC6.LRot32(r, 5);
  l := l xor r;
  t := r and $1F;
  l := TRC6.LRot32(l, t);
  b := TRC6.LRot32(b, 13);
  c := c + l;
  d := d + m;
  a := a xor r;
  m := c + KeyContext[16];
  r := TRC6.LRot32(c, 13) * KeyContext[17];
  l := Mars_SBox[m and $1FF];
  r := TRC6.LRot32(r, 5);
  t := r and $1F;
  m := TRC6.LRot32(m, t);
  l := l xor r;
  r := TRC6.LRot32(r, 5);
  l := l xor r;
  t := r and $1F;
  l := TRC6.LRot32(l, t);
  c := TRC6.LRot32(c, 13);
  d := d + l;
  a := a + m;
  b := b xor r;
  m := d + KeyContext[18];
  r := TRC6.LRot32(d, 13) * KeyContext[19];
  l := Mars_SBox[m and $1FF];
  r := TRC6.LRot32(r, 5);
  t := r and $1F;
  m := TRC6.LRot32(m, t);
  l := l xor r;
  r := TRC6.LRot32(r, 5);
  l := l xor r;
  t := r and $1F;
  l := TRC6.LRot32(l, t);
  d := TRC6.LRot32(d, 13);
  a := a + l;
  b := b + m;
  c := c xor r;
  m := a + KeyContext[20];
  r := TRC6.LRot32(a, 13) * KeyContext[21];
  l := Mars_SBox[m and $1FF];
  r := TRC6.LRot32(r, 5);
  t := r and $1F;
  m := TRC6.LRot32(m, t);
  l := l xor r;
  r := TRC6.LRot32(r, 5);
  l := l xor r;
  t := r and $1F;
  l := TRC6.LRot32(l, t);
  a := TRC6.LRot32(a, 13);
  d := d + l;
  c := c + m;
  b := b xor r;
  m := b + KeyContext[22];
  r := TRC6.LRot32(b, 13) * KeyContext[23];
  l := Mars_SBox[m and $1FF];
  r := TRC6.LRot32(r, 5);
  t := r and $1F;
  m := TRC6.LRot32(m, t);
  l := l xor r;
  r := TRC6.LRot32(r, 5);
  l := l xor r;
  t := r and $1F;
  l := TRC6.LRot32(l, t);
  b := TRC6.LRot32(b, 13);
  a := a + l;
  d := d + m;
  c := c xor r;
  m := c + KeyContext[24];
  r := TRC6.LRot32(c, 13) * KeyContext[25];
  l := Mars_SBox[m and $1FF];
  r := TRC6.LRot32(r, 5);
  t := r and $1F;
  m := TRC6.LRot32(m, t);
  l := l xor r;
  r := TRC6.LRot32(r, 5);
  l := l xor r;
  t := r and $1F;
  l := TRC6.LRot32(l, t);
  c := TRC6.LRot32(c, 13);
  b := b + l;
  a := a + m;
  d := d xor r;
  m := d + KeyContext[26];
  r := TRC6.LRot32(d, 13) * KeyContext[27];
  l := Mars_SBox[m and $1FF];
  r := TRC6.LRot32(r, 5);
  t := r and $1F;
  m := TRC6.LRot32(m, t);
  l := l xor r;
  r := TRC6.LRot32(r, 5);
  l := l xor r;
  t := r and $1F;
  l := TRC6.LRot32(l, t);
  d := TRC6.LRot32(d, 13);
  c := c + l;
  b := b + m;
  a := a xor r;
  m := a + KeyContext[28];
  r := TRC6.LRot32(a, 13) * KeyContext[29];
  l := Mars_SBox[m and $1FF];
  r := TRC6.LRot32(r, 5);
  t := r and $1F;
  m := TRC6.LRot32(m, t);
  l := l xor r;
  r := TRC6.LRot32(r, 5);
  l := l xor r;
  t := r and $1F;
  l := TRC6.LRot32(l, t);
  a := TRC6.LRot32(a, 13);
  d := d + l;
  c := c + m;
  b := b xor r;
  m := b + KeyContext[30];
  r := TRC6.LRot32(b, 13) * KeyContext[31];
  l := Mars_SBox[m and $1FF];
  r := TRC6.LRot32(r, 5);
  t := r and $1F;
  m := TRC6.LRot32(m, t);
  l := l xor r;
  r := TRC6.LRot32(r, 5);
  l := l xor r;
  t := r and $1F;
  l := TRC6.LRot32(l, t);
  b := TRC6.LRot32(b, 13);
  a := a + l;
  d := d + m;
  c := c xor r;
  m := c + KeyContext[32];
  r := TRC6.LRot32(c, 13) * KeyContext[33];
  l := Mars_SBox[m and $1FF];
  r := TRC6.LRot32(r, 5);
  t := r and $1F;
  m := TRC6.LRot32(m, t);
  l := l xor r;
  r := TRC6.LRot32(r, 5);
  l := l xor r;
  t := r and $1F;
  l := TRC6.LRot32(l, t);
  c := TRC6.LRot32(c, 13);
  b := b + l;
  a := a + m;
  d := d xor r;
  m := d + KeyContext[34];
  r := TRC6.LRot32(d, 13) * KeyContext[35];
  l := Mars_SBox[m and $1FF];
  r := TRC6.LRot32(r, 5);
  t := r and $1F;
  m := TRC6.LRot32(m, t);
  l := l xor r;
  r := TRC6.LRot32(r, 5);
  l := l xor r;
  t := r and $1F;
  l := TRC6.LRot32(l, t);
  d := TRC6.LRot32(d, 13);
  c := c + l;
  b := b + m;
  a := a xor r;
  b := b xor Mars_SBox[(a and $FF) + 256];
  c := c - Mars_SBox[(a shr 24) and $FF];
  d := d - Mars_SBox[((a shr 16) and $FF) + 256];
  d := d xor Mars_SBox[(a shr 8) and $FF];
  a := TRC6.LRot32(a, 24);
  c := c xor Mars_SBox[(b and $FF) + 256];
  d := d - Mars_SBox[(b shr 24) and $FF];
  a := a - Mars_SBox[((b shr 16) and $FF) + 256];
  a := a xor Mars_SBox[(b shr 8) and $FF];
  b := TRC6.LRot32(b, 24);
  c := c - b;
  d := d xor Mars_SBox[(c and $FF) + 256];
  a := a - Mars_SBox[(c shr 24) and $FF];
  b := b - Mars_SBox[((c shr 16) and $FF) + 256];
  b := b xor Mars_SBox[(c shr 8) and $FF];
  c := TRC6.LRot32(c, 24);
  d := d - a;
  a := a xor Mars_SBox[(d and $FF) + 256];
  b := b - Mars_SBox[(d shr 24) and $FF];
  c := c - Mars_SBox[((d shr 16) and $FF) + 256];
  c := c xor Mars_SBox[(d shr 8) and $FF];
  d := TRC6.LRot32(d, 24);
  b := b xor Mars_SBox[(a and $FF) + 256];
  c := c - Mars_SBox[(a shr 24) and $FF];
  d := d - Mars_SBox[((a shr 16) and $FF) + 256];
  d := d xor Mars_SBox[(a shr 8) and $FF];
  a := TRC6.LRot32(a, 24);
  c := c xor Mars_SBox[(b and $FF) + 256];
  d := d - Mars_SBox[(b shr 24) and $FF];
  a := a - Mars_SBox[((b shr 16) and $FF) + 256];
  a := a xor Mars_SBox[(b shr 8) and $FF];
  b := TRC6.LRot32(b, 24);
  c := c - b;
  d := d xor Mars_SBox[(c and $FF) + 256];
  a := a - Mars_SBox[(c shr 24) and $FF];
  b := b - Mars_SBox[((c shr 16) and $FF) + 256];
  b := b xor Mars_SBox[(c shr 8) and $FF];
  c := TRC6.LRot32(c, 24);
  d := d - a;
  a := a xor Mars_SBox[(d and $FF) + 256];
  b := b - Mars_SBox[(d shr 24) and $FF];
  c := c - Mars_SBox[((d shr 16) and $FF) + 256];
  c := c xor Mars_SBox[(d shr 8) and $FF];
  d := TRC6.LRot32(d, 24);
  a := a - KeyContext[36];
  b := b - KeyContext[37];
  c := c - KeyContext[38];
  d := d - KeyContext[39];

  PDWORD(@Data[0])^ := a;
  PDWORD(@Data[4])^ := b;
  PDWORD(@Data[8])^ := c;
  PDWORD(@Data[12])^ := d;
{$IFDEF RangeCheck}{$R+}{$ENDIF}
end;

class procedure TMars.Decrypt(var KeyContext: TMarskey; var Data: TMarsBlock);
var
  l, m, r, t, a, b, c, d: DWORD;
begin
{$IFDEF RangeCheck}{$R-}{$ENDIF}
  a := PDWORD(@Data[0])^;
  b := PDWORD(@Data[4])^;
  c := PDWORD(@Data[8])^;
  d := PDWORD(@Data[12])^;

  a := a + KeyContext[36];
  b := b + KeyContext[37];
  c := c + KeyContext[38];
  d := d + KeyContext[39];
  d := TRC6.RRot32(d, 24);
  c := c xor Mars_SBox[(d shr 8) and $FF];
  c := c + Mars_SBox[((d shr 16) and $FF) + 256];
  b := b + Mars_SBox[(d shr 24) and $FF];
  a := a xor Mars_SBox[(d and $FF) + 256];
  d := d + a;
  c := TRC6.RRot32(c, 24);
  b := b xor Mars_SBox[(c shr 8) and $FF];
  b := b + Mars_SBox[((c shr 16) and $FF) + 256];
  a := a + Mars_SBox[(c shr 24) and $FF];
  d := d xor Mars_SBox[(c and $FF) + 256];
  c := c + b;
  b := TRC6.RRot32(b, 24);
  a := a xor Mars_SBox[(b shr 8) and $FF];
  a := a + Mars_SBox[((b shr 16) and $FF) + 256];
  d := d + Mars_SBox[(b shr 24) and $FF];
  c := c xor Mars_SBox[(b and $FF) + 256];
  a := TRC6.RRot32(a, 24);
  d := d xor Mars_SBox[(a shr 8) and $FF];
  d := d + Mars_SBox[((a shr 16) and $FF) + 256];
  c := c + Mars_SBox[(a shr 24) and $FF];
  b := b xor Mars_SBox[(a and $FF) + 256];
  d := TRC6.RRot32(d, 24);
  c := c xor Mars_SBox[(d shr 8) and $FF];
  c := c + Mars_SBox[((d shr 16) and $FF) + 256];
  b := b + Mars_SBox[(d shr 24) and $FF];
  a := a xor Mars_SBox[(d and $FF) + 256];
  d := d + a;
  c := TRC6.RRot32(c, 24);
  b := b xor Mars_SBox[(c shr 8) and $FF];
  b := b + Mars_SBox[((c shr 16) and $FF) + 256];
  a := a + Mars_SBox[(c shr 24) and $FF];
  d := d xor Mars_SBox[(c and $FF) + 256];
  c := c + b;
  b := TRC6.RRot32(b, 24);
  a := a xor Mars_SBox[(b shr 8) and $FF];
  a := a + Mars_SBox[((b shr 16) and $FF) + 256];
  d := d + Mars_SBox[(b shr 24) and $FF];
  c := c xor Mars_SBox[(b and $FF) + 256];
  a := TRC6.RRot32(a, 24);
  d := d xor Mars_SBox[(a shr 8) and $FF];
  d := d + Mars_SBox[((a shr 16) and $FF) + 256];
  c := c + Mars_SBox[(a shr 24) and $FF];
  b := b xor Mars_SBox[(a and $FF) + 256];
  d := TRC6.RRot32(d, 13);
  m := d + KeyContext[34];
  r := TRC6.LRot32(d, 13) * KeyContext[35];
  l := Mars_SBox[m and $1FF];
  r := TRC6.LRot32(r, 5);
  t := r and $1F;
  m := TRC6.LRot32(m, t);
  l := l xor r;
  r := TRC6.LRot32(r, 5);
  l := l xor r;
  t := r and $1F;
  l := TRC6.LRot32(l, t);
  c := c - l;
  b := b - m;
  a := a xor r;
  c := TRC6.RRot32(c, 13);
  m := c + KeyContext[32];
  r := TRC6.LRot32(c, 13) * KeyContext[33];
  l := Mars_SBox[m and $1FF];
  r := TRC6.LRot32(r, 5);
  t := r and $1F;
  m := TRC6.LRot32(m, t);
  l := l xor r;
  r := TRC6.LRot32(r, 5);
  l := l xor r;
  t := r and $1F;
  l := TRC6.LRot32(l, t);
  b := b - l;
  a := a - m;
  d := d xor r;
  b := TRC6.RRot32(b, 13);
  m := b + KeyContext[30];
  r := TRC6.LRot32(b, 13) * KeyContext[31];
  l := Mars_SBox[m and $1FF];
  r := TRC6.LRot32(r, 5);
  t := r and $1F;
  m := TRC6.LRot32(m, t);
  l := l xor r;
  r := TRC6.LRot32(r, 5);
  l := l xor r;
  t := r and $1F;
  l := TRC6.LRot32(l, t);
  a := a - l;
  d := d - m;
  c := c xor r;
  a := TRC6.RRot32(a, 13);
  m := a + KeyContext[28];
  r := TRC6.LRot32(a, 13) * KeyContext[29];
  l := Mars_SBox[m and $1FF];
  r := TRC6.LRot32(r, 5);
  t := r and $1F;
  m := TRC6.LRot32(m, t);
  l := l xor r;
  r := TRC6.LRot32(r, 5);
  l := l xor r;
  t := r and $1F;
  l := TRC6.LRot32(l, t);
  d := d - l;
  c := c - m;
  b := b xor r;
  d := TRC6.RRot32(d, 13);
  m := d + KeyContext[26];
  r := TRC6.LRot32(d, 13) * KeyContext[27];
  l := Mars_SBox[m and $1FF];
  r := TRC6.LRot32(r, 5);
  t := r and $1F;
  m := TRC6.LRot32(m, t);
  l := l xor r;
  r := TRC6.LRot32(r, 5);
  l := l xor r;
  t := r and $1F;
  l := TRC6.LRot32(l, t);
  c := c - l;
  b := b - m;
  a := a xor r;
  c := TRC6.RRot32(c, 13);
  m := c + KeyContext[24];
  r := TRC6.LRot32(c, 13) * KeyContext[25];
  l := Mars_SBox[m and $1FF];
  r := TRC6.LRot32(r, 5);
  t := r and $1F;
  m := TRC6.LRot32(m, t);
  l := l xor r;
  r := TRC6.LRot32(r, 5);
  l := l xor r;
  t := r and $1F;
  l := TRC6.LRot32(l, t);
  b := b - l;
  a := a - m;
  d := d xor r;
  b := TRC6.RRot32(b, 13);
  m := b + KeyContext[22];
  r := TRC6.LRot32(b, 13) * KeyContext[23];
  l := Mars_SBox[m and $1FF];
  r := TRC6.LRot32(r, 5);
  t := r and $1F;
  m := TRC6.LRot32(m, t);
  l := l xor r;
  r := TRC6.LRot32(r, 5);
  l := l xor r;
  t := r and $1F;
  l := TRC6.LRot32(l, t);
  a := a - l;
  d := d - m;
  c := c xor r;
  a := TRC6.RRot32(a, 13);
  m := a + KeyContext[20];
  r := TRC6.LRot32(a, 13) * KeyContext[21];
  l := Mars_SBox[m and $1FF];
  r := TRC6.LRot32(r, 5);
  t := r and $1F;
  m := TRC6.LRot32(m, t);
  l := l xor r;
  r := TRC6.LRot32(r, 5);
  l := l xor r;
  t := r and $1F;
  l := TRC6.LRot32(l, t);
  d := d - l;
  c := c - m;
  b := b xor r;
  d := TRC6.RRot32(d, 13);
  m := d + KeyContext[18];
  r := TRC6.LRot32(d, 13) * KeyContext[19];
  l := Mars_SBox[m and $1FF];
  r := TRC6.LRot32(r, 5);
  t := r and $1F;
  m := TRC6.LRot32(m, t);
  l := l xor r;
  r := TRC6.LRot32(r, 5);
  l := l xor r;
  t := r and $1F;
  l := TRC6.LRot32(l, t);
  a := a - l;
  b := b - m;
  c := c xor r;
  c := TRC6.RRot32(c, 13);
  m := c + KeyContext[16];
  r := TRC6.LRot32(c, 13) * KeyContext[17];
  l := Mars_SBox[m and $1FF];
  r := TRC6.LRot32(r, 5);
  t := r and $1F;
  m := TRC6.LRot32(m, t);
  l := l xor r;
  r := TRC6.LRot32(r, 5);
  l := l xor r;
  t := r and $1F;
  l := TRC6.LRot32(l, t);
  d := d - l;
  a := a - m;
  b := b xor r;
  b := TRC6.RRot32(b, 13);
  m := b + KeyContext[14];
  r := TRC6.LRot32(b, 13) * KeyContext[15];
  l := Mars_SBox[m and $1FF];
  r := TRC6.LRot32(r, 5);
  t := r and $1F;
  m := TRC6.LRot32(m, t);
  l := l xor r;
  r := TRC6.LRot32(r, 5);
  l := l xor r;
  t := r and $1F;
  l := TRC6.LRot32(l, t);
  c := c - l;
  d := d - m;
  a := a xor r;
  a := TRC6.RRot32(a, 13);
  m := a + KeyContext[12];
  r := TRC6.LRot32(a, 13) * KeyContext[13];
  l := Mars_SBox[m and $1FF];
  r := TRC6.LRot32(r, 5);
  t := r and $1F;
  m := TRC6.LRot32(m, t);
  l := l xor r;
  r := TRC6.LRot32(r, 5);
  l := l xor r;
  t := r and $1F;
  l := TRC6.LRot32(l, t);
  b := b - l;
  c := c - m;
  d := d xor r;
  d := TRC6.RRot32(d, 13);
  m := d + KeyContext[10];
  r := TRC6.LRot32(d, 13) * KeyContext[11];
  l := Mars_SBox[m and $1FF];
  r := TRC6.LRot32(r, 5);
  t := r and $1F;
  m := TRC6.LRot32(m, t);
  l := l xor r;
  r := TRC6.LRot32(r, 5);
  l := l xor r;
  t := r and $1F;
  l := TRC6.LRot32(l, t);
  a := a - l;
  b := b - m;
  c := c xor r;
  c := TRC6.RRot32(c, 13);
  m := c + KeyContext[8];
  r := TRC6.LRot32(c, 13) * KeyContext[9];
  l := Mars_SBox[m and $1FF];
  r := TRC6.LRot32(r, 5);
  t := r and $1F;
  m := TRC6.LRot32(m, t);
  l := l xor r;
  r := TRC6.LRot32(r, 5);
  l := l xor r;
  t := r and $1F;
  l := TRC6.LRot32(l, t);
  d := d - l;
  a := a - m;
  b := b xor r;
  b := TRC6.RRot32(b, 13);
  m := b + KeyContext[6];
  r := TRC6.LRot32(b, 13) * KeyContext[7];
  l := Mars_SBox[m and $1FF];
  r := TRC6.LRot32(r, 5);
  t := r and $1F;
  m := TRC6.LRot32(m, t);
  l := l xor r;
  r := TRC6.LRot32(r, 5);
  l := l xor r;
  t := r and $1F;
  l := TRC6.LRot32(l, t);
  c := c - l;
  d := d - m;
  a := a xor r;
  a := TRC6.RRot32(a, 13);
  m := a + KeyContext[4];
  r := TRC6.LRot32(a, 13) * KeyContext[5];
  l := Mars_SBox[m and $1FF];
  r := TRC6.LRot32(r, 5);
  t := r and $1F;
  m := TRC6.LRot32(m, t);
  l := l xor r;
  r := TRC6.LRot32(r, 5);
  l := l xor r;
  t := r and $1F;
  l := TRC6.LRot32(l, t);
  b := b - l;
  c := c - m;
  d := d xor r;
  d := TRC6.LRot32(d, 24);
  c := c xor Mars_SBox[((d shr 24) and $FF) + 256];
  b := b - Mars_SBox[(d shr 16) and $FF];
  a := a - Mars_SBox[((d shr 8) and $FF) + 256];
  a := a xor Mars_SBox[d and $FF];
  c := TRC6.LRot32(c, 24);
  b := b xor Mars_SBox[((c shr 24) and $FF) + 256];
  a := a - Mars_SBox[(c shr 16) and $FF];
  d := d - Mars_SBox[((c shr 8) and $FF) + 256];
  d := d xor Mars_SBox[c and $FF];
  b := b - c;
  b := TRC6.LRot32(b, 24);
  a := a xor Mars_SBox[((b shr 24) and $FF) + 256];
  d := d - Mars_SBox[(b shr 16) and $FF];
  c := c - Mars_SBox[((b shr 8) and $FF) + 256];
  c := c xor Mars_SBox[b and $FF];
  a := a - d;
  a := TRC6.LRot32(a, 24);
  d := d xor Mars_SBox[((a shr 24) and $FF) + 256];
  c := c - Mars_SBox[(a shr 16) and $FF];
  b := b - Mars_SBox[((a shr 8) and $FF) + 256];
  b := b xor Mars_SBox[a and $FF];
  d := TRC6.LRot32(d, 24);
  c := c xor Mars_SBox[((d shr 24) and $FF) + 256];
  b := b - Mars_SBox[(d shr 16) and $FF];
  a := a - Mars_SBox[((d shr 8) and $FF) + 256];
  a := a xor Mars_SBox[d and $FF];
  c := TRC6.LRot32(c, 24);
  b := b xor Mars_SBox[((c shr 24) and $FF) + 256];
  a := a - Mars_SBox[(c shr 16) and $FF];
  d := d - Mars_SBox[((c shr 8) and $FF) + 256];
  d := d xor Mars_SBox[c and $FF];
  b := b - c;
  b := TRC6.LRot32(b, 24);
  a := a xor Mars_SBox[((b shr 24) and $FF) + 256];
  d := d - Mars_SBox[(b shr 16) and $FF];
  c := c - Mars_SBox[((b shr 8) and $FF) + 256];
  c := c xor Mars_SBox[b and $FF];
  a := a - d;
  a := TRC6.LRot32(a, 24);
  d := d xor Mars_SBox[((a shr 24) and $FF) + 256];
  c := c - Mars_SBox[(a shr 16) and $FF];
  b := b - Mars_SBox[((a shr 8) and $FF) + 256];
  b := b xor Mars_SBox[a and $FF];
  a := a - KeyContext[0];
  b := b - KeyContext[1];
  c := c - KeyContext[2];
  d := d - KeyContext[3];

  PDWORD(@Data[0])^ := a;
  PDWORD(@Data[4])^ := b;
  PDWORD(@Data[8])^ := c;
  PDWORD(@Data[12])^ := d;
{$IFDEF RangeCheck}{$R+}{$ENDIF}
end;

class procedure TRijndael.InvMixColumn(const a: PByteArray; const BC: Byte);
var
  j: Integer;
begin
  for j := 0 to (BC - 1) do
      PDWORD(@(a^[j * 4]))^ := PDWORD(@U1[a^[j * 4 + 0]])^ xor PDWORD(@U2[a^[j * 4 + 1]])^ xor PDWORD(@U3[a^[j * 4 + 2]])^ xor PDWORD(@U4[a^[j * 4 + 3]])^;
end;

class procedure TRijndael.InitKey(buff: Pointer; Size: Integer; var KeyContext: TRijndaelkey);
const
  s_box: array [0 .. 255] of Byte = (
    99, 124, 119, 123, 242, 107, 111, 197, 48, 1, 103, 43, 254, 215, 171, 118,
    202, 130, 201, 125, 250, 89, 71, 240, 173, 212, 162, 175, 156, 164, 114, 192,
    183, 253, 147, 38, 54, 63, 247, 204, 52, 165, 229, 241, 113, 216, 49, 21,
    4, 199, 35, 195, 24, 150, 5, 154, 7, 18, 128, 226, 235, 39, 178, 117,
    9, 131, 44, 26, 27, 110, 90, 160, 82, 59, 214, 179, 41, 227, 47, 132,
    83, 209, 0, 237, 32, 252, 177, 91, 106, 203, 190, 57, 74, 76, 88, 207,
    208, 239, 170, 251, 67, 77, 51, 133, 69, 249, 2, 127, 80, 60, 159, 168,
    81, 163, 64, 143, 146, 157, 56, 245, 188, 182, 218, 33, 16, 255, 243, 210,
    205, 12, 19, 236, 95, 151, 68, 23, 196, 167, 126, 61, 100, 93, 25, 115,
    96, 129, 79, 220, 34, 42, 144, 136, 70, 238, 184, 20, 222, 94, 11, 219,
    224, 50, 58, 10, 73, 6, 36, 92, 194, 211, 172, 98, 145, 149, 228, 121,
    231, 200, 55, 109, 141, 213, 78, 169, 108, 86, 244, 234, 101, 122, 174, 8,
    186, 120, 37, 46, 28, 166, 180, 198, 232, 221, 116, 31, 75, 189, 139, 138,
    112, 62, 181, 102, 72, 3, 246, 14, 97, 53, 87, 185, 134, 193, 29, 158,
    225, 248, 152, 17, 105, 217, 142, 148, 155, 30, 135, 233, 206, 85, 40, 223,
    140, 161, 137, 13, 191, 230, 66, 104, 65, 153, 45, 15, 176, 84, 187, 22);
var
  KC, Rounds: DWORD;
  j, r, t, rconpointer: Integer;
  tk: array [0 .. 8 - 1, 0 .. 3] of Byte;
begin
  FillPtrByte(@tk, SizeOf(tk), 0);
  CopyPtr(buff, @tk, Size);
  if Size <= 16 then
    begin
      KC := 4;
      Rounds := 10;
    end
  else if Size <= 24 then
    begin
      KC := 6;
      Rounds := 12;
    end
  else
    begin
      KC := 8;
      Rounds := 14;
    end;
  KeyContext.NumRounds := Rounds;
  r := 0;
  t := 0;
  j := 0;
  while (j < KC) and (r < (Rounds + 1)) do
    begin
      while (j < KC) and (t < 4) do
        begin
          KeyContext.rk[r, t] := PDWORD(@tk[j])^;
          inc(j);
          inc(t);
        end;
      if t = 4 then
        begin
          t := 0;
          inc(r);
        end;
    end;
  rconpointer := 0;
  while (r < (Rounds + 1)) do
    begin
      tk[0, 0] := tk[0, 0] xor s_box[tk[KC - 1, 1]];
      tk[0, 1] := tk[0, 1] xor s_box[tk[KC - 1, 2]];
      tk[0, 2] := tk[0, 2] xor s_box[tk[KC - 1, 3]];
      tk[0, 3] := tk[0, 3] xor s_box[tk[KC - 1, 0]];
      tk[0, 0] := tk[0, 0] xor rcon[rconpointer];
      inc(rconpointer);
      if KC <> 8 then
        begin
          for j := 1 to (KC - 1) do
              PDWORD(@tk[j])^ := PDWORD(@tk[j])^ xor PDWORD(@tk[j - 1])^;
        end
      else
        begin
          for j := 1 to ((KC div 2) - 1) do
              PDWORD(@tk[j])^ := PDWORD(@tk[j])^ xor PDWORD(@tk[j - 1])^;

          tk[KC div 2, 0] := tk[KC div 2, 0] xor s_box[tk[KC div 2 - 1, 0]];
          tk[KC div 2, 1] := tk[KC div 2, 1] xor s_box[tk[KC div 2 - 1, 1]];
          tk[KC div 2, 2] := tk[KC div 2, 2] xor s_box[tk[KC div 2 - 1, 2]];
          tk[KC div 2, 3] := tk[KC div 2, 3] xor s_box[tk[KC div 2 - 1, 3]];

          for j := ((KC div 2) + 1) to (KC - 1) do
              PDWORD(@tk[j])^ := PDWORD(@tk[j])^ xor PDWORD(@tk[j - 1])^;
        end;
      j := 0;
      while (j < KC) and (r < (Rounds + 1)) do
        begin
          while (j < KC) and (t < 4) do
            begin
              KeyContext.rk[r, t] := PDWORD(@tk[j])^;
              inc(j);
              inc(t);
            end;
          if t = 4 then
            begin
              inc(r);
              t := 0;
            end;
        end;
    end;
  CopyPtr(@KeyContext.rk, @KeyContext.drk, SizeOf(KeyContext.rk));
  for r := 1 to (Rounds - 1) do
      InvMixColumn(@KeyContext.drk[r], 4);
end;

class procedure TRijndael.Encrypt(var KeyContext: TRijndaelkey; var Data: TRijndaelBlock);
begin
  Encrypt(KeyContext, PDWORD(@Data[0])^, PDWORD(@Data[4])^, PDWORD(@Data[8])^, PDWORD(@Data[12])^);
end;

class procedure TRijndael.Encrypt(var KeyContext: TRijndaelkey; var B1, B2, B3, B4: DWORD);
var
  r: DWORD;
  tempb: array [0 .. 3, 0 .. 3] of Byte;
  a: array [0 .. 3, 0 .. 3] of Byte;
begin
  PDWORD(@a[0, 0])^ := B1;
  PDWORD(@a[1, 0])^ := B2;
  PDWORD(@a[2, 0])^ := B3;
  PDWORD(@a[3, 0])^ := B4;
  for r := 0 to (KeyContext.NumRounds - 2) do
    begin
      PDWORD(@tempb[0])^ := PDWORD(@a[0])^ xor KeyContext.rk[r, 0];
      PDWORD(@tempb[1])^ := PDWORD(@a[1])^ xor KeyContext.rk[r, 1];
      PDWORD(@tempb[2])^ := PDWORD(@a[2])^ xor KeyContext.rk[r, 2];
      PDWORD(@tempb[3])^ := PDWORD(@a[3])^ xor KeyContext.rk[r, 3];
      PDWORD(@a[0])^ := PDWORD(@T1[tempb[0, 0]])^ xor PDWORD(@T2[tempb[1, 1]])^ xor PDWORD(@T3[tempb[2, 2]])^ xor PDWORD(@T4[tempb[3, 3]])^;
      PDWORD(@a[1])^ := PDWORD(@T1[tempb[1, 0]])^ xor PDWORD(@T2[tempb[2, 1]])^ xor PDWORD(@T3[tempb[3, 2]])^ xor PDWORD(@T4[tempb[0, 3]])^;
      PDWORD(@a[2])^ := PDWORD(@T1[tempb[2, 0]])^ xor PDWORD(@T2[tempb[3, 1]])^ xor PDWORD(@T3[tempb[0, 2]])^ xor PDWORD(@T4[tempb[1, 3]])^;
      PDWORD(@a[3])^ := PDWORD(@T1[tempb[3, 0]])^ xor PDWORD(@T2[tempb[0, 1]])^ xor PDWORD(@T3[tempb[1, 2]])^ xor PDWORD(@T4[tempb[2, 3]])^;
    end;
  PDWORD(@tempb[0])^ := PDWORD(@a[0])^ xor KeyContext.rk[KeyContext.NumRounds - 1, 0];
  PDWORD(@tempb[1])^ := PDWORD(@a[1])^ xor KeyContext.rk[KeyContext.NumRounds - 1, 1];
  PDWORD(@tempb[2])^ := PDWORD(@a[2])^ xor KeyContext.rk[KeyContext.NumRounds - 1, 2];
  PDWORD(@tempb[3])^ := PDWORD(@a[3])^ xor KeyContext.rk[KeyContext.NumRounds - 1, 3];
  a[0, 0] := T1[tempb[0, 0], 1];
  a[0, 1] := T1[tempb[1, 1], 1];
  a[0, 2] := T1[tempb[2, 2], 1];
  a[0, 3] := T1[tempb[3, 3], 1];
  a[1, 0] := T1[tempb[1, 0], 1];
  a[1, 1] := T1[tempb[2, 1], 1];
  a[1, 2] := T1[tempb[3, 2], 1];
  a[1, 3] := T1[tempb[0, 3], 1];
  a[2, 0] := T1[tempb[2, 0], 1];
  a[2, 1] := T1[tempb[3, 1], 1];
  a[2, 2] := T1[tempb[0, 2], 1];
  a[2, 3] := T1[tempb[1, 3], 1];
  a[3, 0] := T1[tempb[3, 0], 1];
  a[3, 1] := T1[tempb[0, 1], 1];
  a[3, 2] := T1[tempb[1, 2], 1];
  a[3, 3] := T1[tempb[2, 3], 1];

  B1 := PDWORD(@a[0])^ xor KeyContext.rk[KeyContext.NumRounds, 0];
  B2 := PDWORD(@a[1])^ xor KeyContext.rk[KeyContext.NumRounds, 1];
  B3 := PDWORD(@a[2])^ xor KeyContext.rk[KeyContext.NumRounds, 2];
  B4 := PDWORD(@a[3])^ xor KeyContext.rk[KeyContext.NumRounds, 3];
end;

class procedure TRijndael.Decrypt(var KeyContext: TRijndaelkey; var Data: TRijndaelBlock);
begin
  Decrypt(KeyContext, PDWORD(@Data[0])^, PDWORD(@Data[4])^, PDWORD(@Data[8])^, PDWORD(@Data[12])^);
end;

class procedure TRijndael.Decrypt(var KeyContext: TRijndaelkey; var B1, B2, B3, B4: DWORD);
var
  r: Integer;
  tempb: array [0 .. 3, 0 .. 3] of Byte;
  a: array [0 .. 3, 0 .. 3] of Byte;
begin
  PDWORD(@a[0, 0])^ := B1;
  PDWORD(@a[1, 0])^ := B2;
  PDWORD(@a[2, 0])^ := B3;
  PDWORD(@a[3, 0])^ := B4;

  for r := KeyContext.NumRounds downto 2 do
    begin
      PDWORD(@tempb[0])^ := PDWORD(@a[0])^ xor KeyContext.drk[r, 0];
      PDWORD(@tempb[1])^ := PDWORD(@a[1])^ xor KeyContext.drk[r, 1];
      PDWORD(@tempb[2])^ := PDWORD(@a[2])^ xor KeyContext.drk[r, 2];
      PDWORD(@tempb[3])^ := PDWORD(@a[3])^ xor KeyContext.drk[r, 3];

      PDWORD(@a[0])^ := PDWORD(@T5[tempb[0, 0]])^ xor PDWORD(@T6[tempb[3, 1]])^ xor PDWORD(@T7[tempb[2, 2]])^ xor PDWORD(@T8[tempb[1, 3]])^;
      PDWORD(@a[1])^ := PDWORD(@T5[tempb[1, 0]])^ xor PDWORD(@T6[tempb[0, 1]])^ xor PDWORD(@T7[tempb[3, 2]])^ xor PDWORD(@T8[tempb[2, 3]])^;
      PDWORD(@a[2])^ := PDWORD(@T5[tempb[2, 0]])^ xor PDWORD(@T6[tempb[1, 1]])^ xor PDWORD(@T7[tempb[0, 2]])^ xor PDWORD(@T8[tempb[3, 3]])^;
      PDWORD(@a[3])^ := PDWORD(@T5[tempb[3, 0]])^ xor PDWORD(@T6[tempb[2, 1]])^ xor PDWORD(@T7[tempb[1, 2]])^ xor PDWORD(@T8[tempb[0, 3]])^;
    end;

  PDWORD(@tempb[0])^ := PDWORD(@a[0])^ xor KeyContext.drk[1, 0];
  PDWORD(@tempb[1])^ := PDWORD(@a[1])^ xor KeyContext.drk[1, 1];
  PDWORD(@tempb[2])^ := PDWORD(@a[2])^ xor KeyContext.drk[1, 2];
  PDWORD(@tempb[3])^ := PDWORD(@a[3])^ xor KeyContext.drk[1, 3];

  a[0, 0] := S5[tempb[0, 0]];
  a[0, 1] := S5[tempb[3, 1]];
  a[0, 2] := S5[tempb[2, 2]];
  a[0, 3] := S5[tempb[1, 3]];
  a[1, 0] := S5[tempb[1, 0]];
  a[1, 1] := S5[tempb[0, 1]];
  a[1, 2] := S5[tempb[3, 2]];
  a[1, 3] := S5[tempb[2, 3]];
  a[2, 0] := S5[tempb[2, 0]];
  a[2, 1] := S5[tempb[1, 1]];
  a[2, 2] := S5[tempb[0, 2]];
  a[2, 3] := S5[tempb[3, 3]];
  a[3, 0] := S5[tempb[3, 0]];
  a[3, 1] := S5[tempb[2, 1]];
  a[3, 2] := S5[tempb[1, 2]];
  a[3, 3] := S5[tempb[0, 3]];

  B1 := PDWORD(@a[0])^ xor KeyContext.drk[0, 0];
  B2 := PDWORD(@a[1])^ xor KeyContext.drk[0, 1];
  B3 := PDWORD(@a[2])^ xor KeyContext.drk[0, 2];
  B4 := PDWORD(@a[3])^ xor KeyContext.drk[0, 3];
end;

class function TTwofish.TwofishCalculateSBoxes(x: DWORD; l: Pointer; KeySize: DWORD): DWORD;
var
  b0, B1, B2, B3: Byte;
begin
  { precalculating permutations for H function }

  b0 := x and $FF;
  B1 := (x shr 8) and $FF;
  B2 := (x shr 16) and $FF;
  B3 := x shr 24;

  if KeySize > 192 then
    begin
      b0 := P8x8[1, b0] xor PByte(nativeUInt(l) + 12)^;
      B1 := P8x8[0, B1] xor PByte(nativeUInt(l) + 13)^;
      B2 := P8x8[0, B2] xor PByte(nativeUInt(l) + 14)^;
      B3 := P8x8[1, B3] xor PByte(nativeUInt(l) + 15)^;
    end;
  if KeySize > 128 then
    begin
      b0 := P8x8[1, b0] xor PByte(nativeUInt(l) + 8)^;
      B1 := P8x8[1, B1] xor PByte(nativeUInt(l) + 9)^;
      B2 := P8x8[0, B2] xor PByte(nativeUInt(l) + 10)^;
      B3 := P8x8[0, B3] xor PByte(nativeUInt(l) + 11)^;
    end;

  b0 := P8x8[0, b0] xor PByte(nativeUInt(l) + 4)^;
  B1 := P8x8[1, B1] xor PByte(nativeUInt(l) + 5)^;
  B2 := P8x8[0, B2] xor PByte(nativeUInt(l) + 6)^;
  B3 := P8x8[1, B3] xor PByte(nativeUInt(l) + 7)^;

  b0 := P8x8[1, P8x8[0, b0] xor PByte(l)^];
  B1 := P8x8[0, P8x8[0, B1] xor PByte(nativeUInt(l) + 1)^];
  B2 := P8x8[1, P8x8[1, B2] xor PByte(nativeUInt(l) + 2)^];
  B3 := P8x8[0, P8x8[1, B3] xor PByte(nativeUInt(l) + 3)^];

  Result := DWORD(b0) or (DWORD(B1) shl 8) or (DWORD(B2) shl 16) or (DWORD(B3) shl 24);
end;

class function TTwofish.TwofishH(x: DWORD; l: Pointer; KeySize: DWORD): DWORD;
var
  b0, B1, B2, B3, z0, z1, z2, z3: Byte;
begin
  b0 := x and $FF;
  B1 := (x shr 8) and $FF;
  B2 := (x shr 16) and $FF;
  B3 := x shr 24;

  if KeySize > 192 then
    begin
      b0 := P8x8[1, b0] xor PByte(nativeUInt(l) + 12)^;
      B1 := P8x8[0, B1] xor PByte(nativeUInt(l) + 13)^;
      B2 := P8x8[0, B2] xor PByte(nativeUInt(l) + 14)^;
      B3 := P8x8[1, B3] xor PByte(nativeUInt(l) + 15)^;
    end;
  if KeySize > 128 then
    begin
      b0 := P8x8[1, b0] xor PByte(nativeUInt(l) + 8)^;
      B1 := P8x8[1, B1] xor PByte(nativeUInt(l) + 9)^;
      B2 := P8x8[0, B2] xor PByte(nativeUInt(l) + 10)^;
      B3 := P8x8[0, B3] xor PByte(nativeUInt(l) + 11)^;
    end;

  b0 := P8x8[0, b0] xor PByte(nativeUInt(l) + 4)^;
  B1 := P8x8[1, B1] xor PByte(nativeUInt(l) + 5)^;
  B2 := P8x8[0, B2] xor PByte(nativeUInt(l) + 6)^;
  B3 := P8x8[1, B3] xor PByte(nativeUInt(l) + 7)^;

  b0 := P8x8[1, P8x8[0, b0] xor PByte(l)^];
  B1 := P8x8[0, P8x8[0, B1] xor PByte(nativeUInt(l) + 1)^];
  B2 := P8x8[1, P8x8[1, B2] xor PByte(nativeUInt(l) + 2)^];
  B3 := P8x8[0, P8x8[1, B3] xor PByte(nativeUInt(l) + 3)^];

  z0 := b0 xor ArrEF[B1] xor Arr5B[B2] xor Arr5B[B3];
  z1 := Arr5B[b0] xor ArrEF[B1] xor ArrEF[B2] xor B3;
  z2 := ArrEF[b0] xor Arr5B[B1] xor B2 xor ArrEF[B3];
  z3 := ArrEF[b0] xor B1 xor ArrEF[B2] xor Arr5B[B3];

  Result := DWORD(z0) or (DWORD(z1) shl 8) or (DWORD(z2) shl 16) or (DWORD(z3) shl 24);
end;

class function TTwofish.TwofishH(const x: DWORD; const key: TTwofishKey): DWORD;
var
  b0, B1, B2, B3, z0, z1, z2, z3: Byte;
begin
  b0 := key.SBox0[x and $FF];
  B1 := key.SBox1[(x shr 8) and $FF];
  B2 := key.SBox2[(x shr 16) and $FF];
  B3 := key.SBox3[x shr 24];

  { P8x8 permutations are precalculated and stored in Key.SBoxes }

  z0 := b0 xor ArrEF[B1] xor Arr5B[B2] xor Arr5B[B3];
  z1 := Arr5B[b0] xor ArrEF[B1] xor ArrEF[B2] xor B3;
  z2 := ArrEF[b0] xor Arr5B[B1] xor B2 xor ArrEF[B3];
  z3 := ArrEF[b0] xor B1 xor ArrEF[B2] xor Arr5B[B3];

  Result := DWORD(z0) or (DWORD(z1) shl 8) or (DWORD(z2) shl 16) or (DWORD(z3) shl 24);
end;

class function TTwofish.RSMDSMul(const x, y: Byte): Byte;
var
  Res: DWORD;
  index: Byte;
begin
  Res := 0;

  for Index := 7 downto 0 do
    begin
      if (y and (1 shl Index)) <> 0 then
          Res := Res xor (DWORD(x) shl Index);

      if (Res and (1 shl (8 + Index))) <> 0 then
          Res := Res xor ($14D shl Index);
    end;

  Result := Byte(Res);
end;

class function TTwofish.MultiplyMDS(const E, O: DWORD): DWORD;
var
  E0, E1, E2, E3, O0, O1, O2, O3, R0, R1, R2, R3: Byte;
begin
  E0 := E and $FF;
  E1 := (E shr 8) and $FF;
  E2 := (E shr 16) and $FF;
  E3 := (E shr 24) and $FF;
  O0 := O and $FF;
  O1 := (O shr 8) and $FF;
  O2 := (O shr 16) and $FF;
  O3 := (O shr 24) and $FF;

  R0 :=
    RSMDSMul(E0, MDS[0, 0]) xor RSMDSMul(E1, MDS[0, 1]) xor
    RSMDSMul(E2, MDS[0, 2]) xor RSMDSMul(E3, MDS[0, 3]) xor
    RSMDSMul(O0, MDS[0, 4]) xor RSMDSMul(O1, MDS[0, 5]) xor
    RSMDSMul(O2, MDS[0, 6]) xor RSMDSMul(O3, MDS[0, 7]);

  R1 :=
    RSMDSMul(E0, MDS[1, 0]) xor RSMDSMul(E1, MDS[1, 1]) xor
    RSMDSMul(E2, MDS[1, 2]) xor RSMDSMul(E3, MDS[1, 3]) xor
    RSMDSMul(O0, MDS[1, 4]) xor RSMDSMul(O1, MDS[1, 5]) xor
    RSMDSMul(O2, MDS[1, 6]) xor RSMDSMul(O3, MDS[1, 7]);

  R2 :=
    RSMDSMul(E0, MDS[2, 0]) xor RSMDSMul(E1, MDS[2, 1]) xor
    RSMDSMul(E2, MDS[2, 2]) xor RSMDSMul(E3, MDS[2, 3]) xor
    RSMDSMul(O0, MDS[2, 4]) xor RSMDSMul(O1, MDS[2, 5]) xor
    RSMDSMul(O2, MDS[2, 6]) xor RSMDSMul(O3, MDS[2, 7]);

  R3 :=
    RSMDSMul(E0, MDS[3, 0]) xor RSMDSMul(E1, MDS[3, 1]) xor
    RSMDSMul(E2, MDS[3, 2]) xor RSMDSMul(E3, MDS[3, 3]) xor
    RSMDSMul(O0, MDS[3, 4]) xor RSMDSMul(O1, MDS[3, 5]) xor
    RSMDSMul(O2, MDS[3, 6]) xor RSMDSMul(O3, MDS[3, 7]);

  Result := R0 or (R1 shl 8) or (R2 shl 16) or (R3 shl 24);
end;

class procedure TTwofish.InitKey(buff: PCCByteArray; Size: Integer; var KeyContext: TTwofishKey);
var
  i: DWORD;
  Cnt: DWORD;
  KE: array [0 .. 3] of DWORD;
  KO: array [0 .. 3] of DWORD;
  a, b: DWORD;
begin
  Cnt := (Size shl 3 + 63) shr 6;
  KeyContext.KeyLen := Size shl 3;
  for i := 0 to Cnt - 1 do
    begin
      KE[i] := buff^[i shl 3] + buff^[i shl 3 + 1] shl 8 + buff^[i shl 3 + 2] shl 16 + buff^[i shl 3 + 3] shl 24;
      KO[i] := buff^[i shl 3 + 4] + buff^[i shl 3 + 5] shl 8 + buff^[i shl 3 + 6] shl 16 + buff^[i shl 3 + 7] shl 24;

      KeyContext.SBoxKey[Cnt - i - 1] := MultiplyMDS(KE[i], KO[i]);
    end;
  for i := 0 to 19 do
    begin
      a := TwofishH(i * $02020202, @KE, KeyContext.KeyLen);
      b := TwofishH(i * $02020202 + $01010101, @KO, KeyContext.KeyLen);
      b := (b shl 8) or (b shr 24);
      KeyContext.ExpandedKey[i shl 1] := a + b;
      b := a + b shl 1;
      KeyContext.ExpandedKey[i shl 1 + 1] := (b shl 9) or (b shr 23);
    end;

  for i := 0 to 255 do
    begin
      a := (i and $FF) or ((i and $FF) shl 8) or ((i and $FF) shl 16) or ((i and $FF) shl 24);
      a := TwofishCalculateSBoxes(a, @KeyContext.SBoxKey, KeyContext.KeyLen);
      KeyContext.SBox0[i] := a and $FF;
      KeyContext.SBox1[i] := (a shr 8) and $FF;
      KeyContext.SBox2[i] := (a shr 16) and $FF;
      KeyContext.SBox3[i] := (a shr 24) and $FF;
    end;
end;

class procedure TTwofish.Encrypt(var KeyContext: TTwofishKey; var Data: TTwofishBlock);
var
  R0, R1, R2, R3, t0, T1, F0, F1: DWORD;
begin
  { prewhitening }
  R0 := PDWORD(@Data[0])^ xor KeyContext.ExpandedKey[0];
  R1 := PDWORD(@Data[4])^ xor KeyContext.ExpandedKey[1];
  R2 := PDWORD(@Data[8])^ xor KeyContext.ExpandedKey[2];
  R3 := PDWORD(@Data[12])^ xor KeyContext.ExpandedKey[3];

  { 0 round }
  t0 := TwofishH(R0, KeyContext);
  T1 := (R1 shl 8) or (R1 shr 24);
  T1 := TwofishH(T1, KeyContext);
  F0 := (t0 + T1 + KeyContext.ExpandedKey[8]);
  F1 := (t0 + T1 shl 1 + KeyContext.ExpandedKey[9]);

  R2 := R2 xor F0;
  R2 := (R2 shr 1) or (R2 shl 31);
  R3 := (R3 shl 1) or (R3 shr 31) xor F1;

  { 1 round }
  t0 := TwofishH(R2, KeyContext);
  T1 := DWORD(R3 shl 8) or (R3 shr 24);
  T1 := TwofishH(T1, KeyContext);;
  F0 := DWORD(t0 + T1 + KeyContext.ExpandedKey[10]);
  F1 := DWORD(t0 + T1 shl 1 + KeyContext.ExpandedKey[11]);

  R0 := R0 xor F0;
  R0 := (R0 shr 1) or DWORD(R0 shl 31);
  R1 := DWORD(R1 shl 1) or (R1 shr 31) xor F1;

  { 2 round }
  t0 := TwofishH(R0, KeyContext);
  T1 := DWORD(R1 shl 8) or (R1 shr 24);
  T1 := TwofishH(T1, KeyContext);;
  F0 := DWORD(t0 + T1 + KeyContext.ExpandedKey[12]);
  F1 := DWORD(t0 + T1 shl 1 + KeyContext.ExpandedKey[13]);

  R2 := R2 xor F0;
  R2 := (R2 shr 1) or DWORD(R2 shl 31);
  R3 := DWORD(R3 shl 1) or (R3 shr 31) xor F1;

  { 3 round }
  t0 := TwofishH(R2, KeyContext);
  T1 := DWORD(R3 shl 8) or (R3 shr 24);
  T1 := TwofishH(T1, KeyContext);;
  F0 := DWORD(t0 + T1 + KeyContext.ExpandedKey[14]);
  F1 := DWORD(t0 + T1 shl 1 + KeyContext.ExpandedKey[15]);

  R0 := R0 xor F0;
  R0 := (R0 shr 1) or DWORD(R0 shl 31);
  R1 := DWORD(R1 shl 1) or (R1 shr 31) xor F1;

  { 4 round }
  t0 := TwofishH(R0, KeyContext);
  T1 := DWORD(R1 shl 8) or (R1 shr 24);
  T1 := TwofishH(T1, KeyContext);;
  F0 := DWORD(t0 + T1 + KeyContext.ExpandedKey[16]);
  F1 := DWORD(t0 + T1 shl 1 + KeyContext.ExpandedKey[17]);

  R2 := R2 xor F0;
  R2 := (R2 shr 1) or DWORD(R2 shl 31);
  R3 := DWORD(R3 shl 1) or (R3 shr 31) xor F1;

  { 5 round }
  t0 := TwofishH(R2, KeyContext);
  T1 := DWORD(R3 shl 8) or (R3 shr 24);
  T1 := TwofishH(T1, KeyContext);;
  F0 := DWORD(t0 + T1 + KeyContext.ExpandedKey[18]);
  F1 := DWORD(t0 + T1 shl 1 + KeyContext.ExpandedKey[19]);

  R0 := R0 xor F0;
  R0 := (R0 shr 1) or DWORD(R0 shl 31);
  R1 := DWORD(R1 shl 1) or (R1 shr 31) xor F1;

  { 6 round }
  t0 := TwofishH(R0, KeyContext);
  T1 := DWORD(R1 shl 8) or (R1 shr 24);
  T1 := TwofishH(T1, KeyContext);;
  F0 := DWORD(t0 + T1 + KeyContext.ExpandedKey[20]);
  F1 := DWORD(t0 + T1 shl 1 + KeyContext.ExpandedKey[21]);

  R2 := R2 xor F0;
  R2 := (R2 shr 1) or DWORD(R2 shl 31);
  R3 := DWORD(R3 shl 1) or (R3 shr 31) xor F1;

  { 7 round }
  t0 := TwofishH(R2, KeyContext);
  T1 := DWORD(R3 shl 8) or (R3 shr 24);
  T1 := TwofishH(T1, KeyContext);;
  F0 := DWORD(t0 + T1 + KeyContext.ExpandedKey[22]);
  F1 := DWORD(t0 + T1 shl 1 + KeyContext.ExpandedKey[23]);

  R0 := R0 xor F0;
  R0 := (R0 shr 1) or DWORD(R0 shl 31);
  R1 := DWORD(R1 shl 1) or (R1 shr 31) xor F1;

  { 8 round }
  t0 := TwofishH(R0, KeyContext);
  T1 := DWORD(R1 shl 8) or (R1 shr 24);
  T1 := TwofishH(T1, KeyContext);;
  F0 := DWORD(t0 + T1 + KeyContext.ExpandedKey[24]);
  F1 := DWORD(t0 + T1 shl 1 + KeyContext.ExpandedKey[25]);

  R2 := R2 xor F0;
  R2 := (R2 shr 1) or DWORD(R2 shl 31);
  R3 := DWORD(R3 shl 1) or (R3 shr 31) xor F1;

  { 9 round }
  t0 := TwofishH(R2, KeyContext);
  T1 := DWORD(R3 shl 8) or (R3 shr 24);
  T1 := TwofishH(T1, KeyContext);;
  F0 := DWORD(t0 + T1 + KeyContext.ExpandedKey[26]);
  F1 := DWORD(t0 + T1 shl 1 + KeyContext.ExpandedKey[27]);

  R0 := R0 xor F0;
  R0 := (R0 shr 1) or DWORD(R0 shl 31);
  R1 := DWORD(R1 shl 1) or (R1 shr 31) xor F1;

  { 10 round }
  t0 := TwofishH(R0, KeyContext);
  T1 := DWORD(R1 shl 8) or (R1 shr 24);
  T1 := TwofishH(T1, KeyContext);;
  F0 := DWORD(t0 + T1 + KeyContext.ExpandedKey[28]);
  F1 := DWORD(t0 + T1 shl 1 + KeyContext.ExpandedKey[29]);

  R2 := R2 xor F0;
  R2 := (R2 shr 1) or DWORD(R2 shl 31);
  R3 := DWORD(R3 shl 1) or (R3 shr 31) xor F1;

  { 11 round }
  t0 := TwofishH(R2, KeyContext);
  T1 := DWORD(R3 shl 8) or (R3 shr 24);
  T1 := TwofishH(T1, KeyContext);;
  F0 := DWORD(t0 + T1 + KeyContext.ExpandedKey[30]);
  F1 := DWORD(t0 + T1 shl 1 + KeyContext.ExpandedKey[31]);

  R0 := R0 xor F0;
  R0 := (R0 shr 1) or DWORD(R0 shl 31);
  R1 := DWORD(R1 shl 1) or (R1 shr 31) xor F1;

  { 12 round }
  t0 := TwofishH(R0, KeyContext);
  T1 := DWORD(R1 shl 8) or (R1 shr 24);
  T1 := TwofishH(T1, KeyContext);;
  F0 := DWORD(t0 + T1 + KeyContext.ExpandedKey[32]);
  F1 := DWORD(t0 + T1 shl 1 + KeyContext.ExpandedKey[33]);

  R2 := R2 xor F0;
  R2 := (R2 shr 1) or DWORD(R2 shl 31);
  R3 := DWORD(R3 shl 1) or (R3 shr 31) xor F1;

  { 13 round }
  t0 := TwofishH(R2, KeyContext);
  T1 := DWORD(R3 shl 8) or (R3 shr 24);
  T1 := TwofishH(T1, KeyContext);;
  F0 := DWORD(t0 + T1 + KeyContext.ExpandedKey[34]);
  F1 := DWORD(t0 + T1 shl 1 + KeyContext.ExpandedKey[35]);

  R0 := R0 xor F0;
  R0 := (R0 shr 1) or DWORD(R0 shl 31);
  R1 := DWORD(R1 shl 1) or (R1 shr 31) xor F1;

  { 14 round }
  t0 := TwofishH(R0, KeyContext);
  T1 := DWORD(R1 shl 8) or (R1 shr 24);
  T1 := TwofishH(T1, KeyContext);;
  F0 := DWORD(t0 + T1 + KeyContext.ExpandedKey[36]);
  F1 := DWORD(t0 + T1 shl 1 + KeyContext.ExpandedKey[37]);

  R2 := R2 xor F0;
  R2 := (R2 shr 1) or DWORD(R2 shl 31);
  R3 := DWORD(R3 shl 1) or (R3 shr 31) xor F1;

  { 15 round }
  t0 := TwofishH(R2, KeyContext);
  T1 := DWORD(R3 shl 8) or (R3 shr 24);
  T1 := TwofishH(T1, KeyContext);;
  F0 := DWORD(t0 + T1 + KeyContext.ExpandedKey[38]);
  F1 := DWORD(t0 + T1 shl 1 + KeyContext.ExpandedKey[39]);

  R0 := R0 xor F0;
  R0 := (R0 shr 1) or DWORD(R0 shl 31);
  R1 := DWORD(R1 shl 1) or (R1 shr 31) xor F1;

  PDWORD(@Data[0])^ := R2 xor KeyContext.ExpandedKey[4];
  PDWORD(@Data[4])^ := R3 xor KeyContext.ExpandedKey[5];
  PDWORD(@Data[8])^ := R0 xor KeyContext.ExpandedKey[6];
  PDWORD(@Data[12])^ := R1 xor KeyContext.ExpandedKey[7];
end;

class procedure TTwofish.Decrypt(var KeyContext: TTwofishKey; var Data: TTwofishBlock);
var
  R0, R1, R2, R3, t0, T1, F0, F1: DWORD;
begin

  { prewhitening }
  R0 := PDWORD(@Data[0])^ xor KeyContext.ExpandedKey[4];
  R1 := PDWORD(@Data[4])^ xor KeyContext.ExpandedKey[5];
  R2 := PDWORD(@Data[8])^ xor KeyContext.ExpandedKey[6];
  R3 := PDWORD(@Data[12])^ xor KeyContext.ExpandedKey[7];

  { R0,R1 and R2,R3 are replaced from round to round - small optimization }

  { 15 round }
  t0 := TwofishH(R0, KeyContext);;
  T1 := DWORD(R1 shl 8) or (R1 shr 24);
  T1 := TwofishH(T1, KeyContext);;
  F0 := DWORD(t0 + T1 + KeyContext.ExpandedKey[38]);
  F1 := DWORD(t0 + T1 shl 1 + KeyContext.ExpandedKey[39]);

  R2 := (R2 shl 1) or DWORD(R2 shr 31) xor F0;
  R3 := R3 xor F1;
  R3 := DWORD(R3 shr 1) or (R3 shl 31);

  { 14 round }
  t0 := TwofishH(R2, KeyContext);;
  T1 := DWORD(R3 shl 8) or (R3 shr 24);
  T1 := TwofishH(T1, KeyContext);;
  F0 := DWORD(t0 + T1 + KeyContext.ExpandedKey[36]);
  F1 := DWORD(t0 + T1 shl 1 + KeyContext.ExpandedKey[37]);

  R0 := (R0 shl 1) or DWORD(R0 shr 31) xor F0;
  R1 := R1 xor F1;
  R1 := DWORD(R1 shr 1) or (R1 shl 31);

  { 13 round }
  t0 := TwofishH(R0, KeyContext);;
  T1 := DWORD(R1 shl 8) or (R1 shr 24);
  T1 := TwofishH(T1, KeyContext);;
  F0 := DWORD(t0 + T1 + KeyContext.ExpandedKey[34]);
  F1 := DWORD(t0 + T1 shl 1 + KeyContext.ExpandedKey[35]);

  R2 := (R2 shl 1) or DWORD(R2 shr 31) xor F0;
  R3 := R3 xor F1;
  R3 := DWORD(R3 shr 1) or (R3 shl 31);

  { 12 round }
  t0 := TwofishH(R2, KeyContext);;
  T1 := DWORD(R3 shl 8) or (R3 shr 24);
  T1 := TwofishH(T1, KeyContext);;
  F0 := DWORD(t0 + T1 + KeyContext.ExpandedKey[32]);
  F1 := DWORD(t0 + T1 shl 1 + KeyContext.ExpandedKey[33]);

  R0 := (R0 shl 1) or DWORD(R0 shr 31) xor F0;
  R1 := R1 xor F1;
  R1 := DWORD(R1 shr 1) or (R1 shl 31);

  { 11 round }
  t0 := TwofishH(R0, KeyContext);;
  T1 := DWORD(R1 shl 8) or (R1 shr 24);
  T1 := TwofishH(T1, KeyContext);;
  F0 := DWORD(t0 + T1 + KeyContext.ExpandedKey[30]);
  F1 := DWORD(t0 + T1 shl 1 + KeyContext.ExpandedKey[31]);

  R2 := (R2 shl 1) or DWORD(R2 shr 31) xor F0;
  R3 := R3 xor F1;
  R3 := DWORD(R3 shr 1) or (R3 shl 31);

  { 10 round }
  t0 := TwofishH(R2, KeyContext);;
  T1 := DWORD(R3 shl 8) or (R3 shr 24);
  T1 := TwofishH(T1, KeyContext);;
  F0 := DWORD(t0 + T1 + KeyContext.ExpandedKey[28]);
  F1 := DWORD(t0 + T1 shl 1 + KeyContext.ExpandedKey[29]);

  R0 := (R0 shl 1) or DWORD(R0 shr 31) xor F0;
  R1 := R1 xor F1;
  R1 := DWORD(R1 shr 1) or (R1 shl 31);

  { 9 round }
  t0 := TwofishH(R0, KeyContext);;
  T1 := DWORD(R1 shl 8) or (R1 shr 24);
  T1 := TwofishH(T1, KeyContext);;
  F0 := DWORD(t0 + T1 + KeyContext.ExpandedKey[26]);
  F1 := DWORD(t0 + T1 shl 1 + KeyContext.ExpandedKey[27]);

  R2 := (R2 shl 1) or DWORD(R2 shr 31) xor F0;
  R3 := R3 xor F1;
  R3 := DWORD(R3 shr 1) or (R3 shl 31);

  { 8 round }
  t0 := TwofishH(R2, KeyContext);;
  T1 := DWORD(R3 shl 8) or (R3 shr 24);
  T1 := TwofishH(T1, KeyContext);;
  F0 := DWORD(t0 + T1 + KeyContext.ExpandedKey[24]);
  F1 := DWORD(t0 + T1 shl 1 + KeyContext.ExpandedKey[25]);

  R0 := (R0 shl 1) or DWORD(R0 shr 31) xor F0;
  R1 := R1 xor F1;
  R1 := DWORD(R1 shr 1) or (R1 shl 31);

  { 7 round }
  t0 := TwofishH(R0, KeyContext);
  T1 := DWORD(R1 shl 8) or (R1 shr 24);
  T1 := TwofishH(T1, KeyContext);
  F0 := DWORD(t0 + T1 + KeyContext.ExpandedKey[22]);
  F1 := DWORD(t0 + T1 shl 1 + KeyContext.ExpandedKey[23]);

  R2 := (R2 shl 1) or DWORD(R2 shr 31) xor F0;
  R3 := R3 xor F1;
  R3 := DWORD(R3 shr 1) or (R3 shl 31);

  { 6 round }
  t0 := TwofishH(R2, KeyContext);;
  T1 := DWORD(R3 shl 8) or (R3 shr 24);
  T1 := TwofishH(T1, KeyContext);;
  F0 := DWORD(t0 + T1 + KeyContext.ExpandedKey[20]);
  F1 := DWORD(t0 + T1 shl 1 + KeyContext.ExpandedKey[21]);

  R0 := (R0 shl 1) or DWORD(R0 shr 31) xor F0;
  R1 := R1 xor F1;
  R1 := DWORD(R1 shr 1) or (R1 shl 31);

  { 5 round }
  t0 := TwofishH(R0, KeyContext);;
  T1 := DWORD(R1 shl 8) or (R1 shr 24);
  T1 := TwofishH(T1, KeyContext);;
  F0 := DWORD(t0 + T1 + KeyContext.ExpandedKey[18]);
  F1 := DWORD(t0 + T1 shl 1 + KeyContext.ExpandedKey[19]);

  R2 := (R2 shl 1) or DWORD(R2 shr 31) xor F0;
  R3 := R3 xor F1;
  R3 := DWORD(R3 shr 1) or (R3 shl 31);

  { 4 round }
  t0 := TwofishH(R2, KeyContext);;
  T1 := DWORD(R3 shl 8) or (R3 shr 24);
  T1 := TwofishH(T1, KeyContext);;
  F0 := DWORD(t0 + T1 + KeyContext.ExpandedKey[16]);
  F1 := DWORD(t0 + T1 shl 1 + KeyContext.ExpandedKey[17]);

  R0 := (R0 shl 1) or DWORD(R0 shr 31) xor F0;
  R1 := R1 xor F1;
  R1 := DWORD(R1 shr 1) or (R1 shl 31);

  { 3 round }
  t0 := TwofishH(R0, KeyContext);;
  T1 := DWORD(R1 shl 8) or (R1 shr 24);
  T1 := TwofishH(T1, KeyContext);;
  F0 := DWORD(t0 + T1 + KeyContext.ExpandedKey[14]);
  F1 := DWORD(t0 + T1 shl 1 + KeyContext.ExpandedKey[15]);

  R2 := (R2 shl 1) or DWORD(R2 shr 31) xor F0;
  R3 := R3 xor F1;
  R3 := DWORD(R3 shr 1) or (R3 shl 31);

  { 2 round }
  t0 := TwofishH(R2, KeyContext);;
  T1 := DWORD(R3 shl 8) or (R3 shr 24);
  T1 := TwofishH(T1, KeyContext);;
  F0 := DWORD(t0 + T1 + KeyContext.ExpandedKey[12]);
  F1 := DWORD(t0 + T1 shl 1 + KeyContext.ExpandedKey[13]);

  R0 := (R0 shl 1) or DWORD(R0 shr 31) xor F0;
  R1 := R1 xor F1;
  R1 := DWORD(R1 shr 1) or (R1 shl 31);

  { 1 round }
  t0 := TwofishH(R0, KeyContext);;
  T1 := DWORD(R1 shl 8) or (R1 shr 24);
  T1 := TwofishH(T1, KeyContext);;
  F0 := DWORD(t0 + T1 + KeyContext.ExpandedKey[10]);
  F1 := DWORD(t0 + T1 shl 1 + KeyContext.ExpandedKey[11]);

  R2 := (R2 shl 1) or DWORD(R2 shr 31) xor F0;
  R3 := R3 xor F1;
  R3 := DWORD(R3 shr 1) or (R3 shl 31);

  { 0 round }
  t0 := TwofishH(R2, KeyContext);;
  T1 := DWORD(R3 shl 8) or (R3 shr 24);
  T1 := TwofishH(T1, KeyContext);;
  F0 := DWORD(t0 + T1 + KeyContext.ExpandedKey[8]);
  F1 := DWORD(t0 + T1 shl 1 + KeyContext.ExpandedKey[9]);

  R0 := (R0 shl 1) or DWORD(R0 shr 31) xor F0;
  R1 := R1 xor F1;
  R1 := DWORD(R1 shr 1) or (R1 shl 31);

  PDWORD(@Data[0])^ := R2 xor KeyContext.ExpandedKey[0];
  PDWORD(@Data[4])^ := R3 xor KeyContext.ExpandedKey[1];
  PDWORD(@Data[8])^ := R0 xor KeyContext.ExpandedKey[2];
  PDWORD(@Data[12])^ := R1 xor KeyContext.ExpandedKey[3];
end;

initialization

InitSysCBCAndDefaultKey(Int64($F0F0F0F0F0F00F0F));

finalization

SetLength(SystemCBC, 0);

end.
