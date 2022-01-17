{*****************************************************************************
  The DEC team (see file NOTICE.txt) licenses this file
  to you under the Apache License, Version 2.0 (the
  "License"); you may not use this file except in compliance
  with the License. A copy of this licence is found in the root directory
  of this project in the file LICENCE.txt or alternatively at

    http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing,
  software distributed under the License is distributed on an
  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied.  See the License for the
  specific language governing permissions and limitations
  under the License.
*****************************************************************************}

/// <summary>
///   Hash functions. Be aware that the x86 ASM implementations, if activated
///   by the define, are provided by DECHash.asm86.inc!
/// </summary>
unit DECHash;

interface

{$INCLUDE DECOptions.inc}

uses
  {$IFDEF FPC}
  SysUtils, Classes,
  {$ELSE}
  System.SysUtils, System.Classes,
  {$ENDIF}
  DECBaseClass, DECFormatBase, DECUtil, DECHashBase, DECHashAUthentication,
  DECHashBitBase, DECTypes;

type
  // Hash Classes
  THash_MD2         = class;
  THash_MD4         = class;
  THash_MD5         = class;
  THash_RipeMD128   = class;
  THash_RipeMD160   = class;
  THash_RipeMD256   = class;
  THash_RipeMD320   = class;
  THash_SHA0        = class;  // SHA-0
  THash_SHA1        = class;  // SHA-1
  THash_SHA224      = class;  // SHA-2, SHA-224
  THash_SHA256      = class;  // SHA-2, SHA-256
  THash_SHA384      = class;  // SHA-2, SHA-384
  THash_SHA512      = class;  // SHA-2, SHA-512
  THash_SHA3_224    = class;
  THash_SHA3_256    = class;
  THash_SHA3_384    = class;
  THash_SHA3_512    = class;
  THash_Haval128    = class;
  THash_Haval160    = class;  // Haval 160, 3 Rounds
  THash_Haval192    = class;  // Haval 192, 4 Rounds
  THash_Haval224    = class;  // Haval 224, 4 Rounds
  THash_Haval256    = class;  // Haval 256, 5 Rounds
  THash_Tiger       = class;
  THash_Panama      = class;
  {$IFDEF OLD_WHIRLPOOL_NAMES}
  THash_Whirlpool   = class;
  THash_Whirlpool1New = class;
  {$ENDIF}

  THash_Whirlpool0  = class;
  THash_Whirlpool1  = class; // differs, depending on OLD_WHIRLPOOL_NAMES define
  THash_WhirlpoolT  = class;

  THash_Square      = class;
  THash_Snefru128   = class;  // derived from the Xerox Secure Hash Function
  THash_Snefru256   = class;  // " - "
  THash_Sapphire    = class;

  /// <summary>
  ///   Implementation of the MD2 hash algorithm. Considered to be broken,
  ///   at least on paper.
  /// </summary>
  THash_MD2 = class(TDECHashAuthentication)
  private
    FDigest: array[0..63] of Byte;
  protected
    procedure DoInit; override;
    procedure DoTransform(Buffer: PUInt32Array); override;
    procedure DoDone; override;
  public
    function Digest: PByteArray; override;
    class function DigestSize: UInt32; override;
    class function BlockSize: UInt32; override;
  end;

  /// <summary>
  ///   Base class for the MD4 hash alrogithm and for other hash-algorithms which
  ///   are close relatives to the MD4 algorithm like the RipeMD ones.
  /// </summary>
  THashBaseMD4 = class(TDECHashAuthentication)
  private
    FDigest: array[0..9] of UInt32;
  protected
    procedure DoInit; override;
    procedure DoDone; override;
  public
    function Digest: PByteArray; override;
    class function DigestSize: UInt32; override;
    class function BlockSize: UInt32; override;
  end;

  /// <summary>
  ///   The MD4 algorithm is considered to be broken, at least on paper.
  /// </summary>
  THash_MD4 = class(THashBaseMD4)
  protected
    procedure DoTransform(Buffer: PUInt32Array); override;
  end;

  /// <summary>
  ///   The MD5 algorithm is considered to be broken. Using it in HMAC algorithms
  ///   is still ok.
  /// </summary>
  THash_MD5 = class(THashBaseMD4)
  protected
    procedure DoTransform(Buffer: PUInt32Array); override;
  end;

  /// <summary>
  ///   Do not confuse with the original RipeMD algorithm which ís being
  ///   considered to be unsafe anyway. Considered to be broken due to the only
  ///   128 Bit long message digest result.
  /// </summary>
  THash_RipeMD128 = class(THashBaseMD4)
  protected
    procedure DoTransform(Buffer: PUInt32Array); override;
  end;

  THash_RipeMD160 = class(THashBaseMD4)
  protected
    procedure DoTransform(Buffer: PUInt32Array); override;
  public
    class function DigestSize: UInt32; override;
  end;

  THash_RipeMD256 = class(THashBaseMD4)
  protected
    procedure DoInit; override;
    procedure DoTransform(Buffer: PUInt32Array); override;
  public
    class function DigestSize: UInt32; override;
  end;

  THash_RipeMD320 = class(THashBaseMD4)
  protected
    procedure DoTransform(Buffer: PUInt32Array); override;
  public
    class function DigestSize: UInt32; override;
  end;

  /// <summary>
  ///   Implementation of the SHA0 hash algorithm. This is the original version
  ///   of the SHA algorithm released in 1993. In 1995 some security issues have
  ///   been identified in this algorithm so he got replaced by the slightly
  ///   modified SHA1 algorithm. The recommendation is to not use this SHA0
  ///   algorithm at all. It is only being provided for scenarios where
  ///   compatibility with this algorithm is required.
  /// </summary>
  THash_SHA0 = class(THashBaseMD4)
  protected
    procedure DoTransform(Buffer: PUInt32Array); override;
    procedure DoDone; override;
  public
    class function DigestSize: UInt32; override;
  end;

  {$IFDEF OLD_SHA_NAME}
  /// <summary>
  ///   Implementation of the SHA0 hash algorithm. This is the original version
  ///   of the SHA algorithm released in 1993. In 1995 some security issues have
  ///   been identified in this algorithm so he got replaced by the slightly
  ///   modified SHA1 algorithm. The recommendation is to not use this SHA0
  ///   algorithm at all. It is only being provided for scenarios where
  ///   compatibility with this algorithm is required.
  /// </summary>
  THash_SHA = class(THash_SHA0)
  {$IFDEF X86ASM}
  protected
    procedure DoTransform(Buffer: PUInt32Array); override;
  end
  {$ENDIF};

  {$ENDIF}

  /// <summary>
  ///   Implementation of the SHA1 hash algorithm. At least since February 2017
  ///   collisions have been found for this algorithm so it's now completely
  ///   clear that it should not be used if possible! Use SHA256 or SHA512
  ///   instead!
  /// </summary>
  THash_SHA1 = class(THash_SHA0);

  /// <summary>
  ///   This algorithm is part of the SHA2 series of hash algorithms.
  /// </summary>
  THash_SHA256 = class(THash_SHA0)
  protected
    procedure DoInit; override;
    procedure DoTransform(Buffer: PUInt32Array); override;
  public
    class function DigestSize: UInt32; override;
  end;

  /// <summary>
  ///   This algorithm is part of the SHA2 series of hash algorithms.
  ///   German BSI recommends not to use this algorithm, they recommend SHA256
  ///   or higher instead.
  /// </summary>
  THash_SHA224 = class(THash_SHA256)
  protected
    procedure DoInit; override;
  public
    class function DigestSize: UInt32; override;
    class function BlockSize: UInt32; override;
  end;

  /// <summary>
  ///   This algorithm is part of the SHA2 series of hash algorithms.
  /// </summary>
  THash_SHA384 = class(TDECHashAuthentication)
  private
    FDigest: array[0..7] of Int64;
  protected
    procedure DoInit; override;
    procedure DoTransform(Buffer: PUInt32Array); override;
    procedure DoDone; override;
  public
    function Digest: PByteArray; override;
    class function DigestSize: UInt32; override;
    class function BlockSize: UInt32; override;
  end;

  /// <summary>
  ///   This algorithm is part of the SHA2 series of hash algorithms.
  /// </summary>
  THash_SHA512 = class(THash_SHA384)
  protected
    procedure DoInit; override;
  public
    class function DigestSize: UInt32; override;
  end;

  /// <summary>
  ///   Base class for tall SHA3 implementations
  /// </summary>
  THash_SHA3Base = class(TDECHashBit)
  strict private
    // Declarations for SHA3. Must be declared here to allow private methods
    // to use these types as well.
    const
      KeccakPermutationSize        = 1600;
      /// <summary>
      ///   Maximum bitrate? If yes this would be higher than any value listed here:
      ///   https://keccak.team/keccak.html
      /// </summary>
      KeccakMaximumRate            = 1536;
      /// <summary>
      ///   KeccakPermutationSize converted into bytes instead of bits
      /// </summary>
      KeccakPermutationSizeInBytes = KeccakPermutationSize div 8;
      /// <summary>
      ///   KeccakMaximumRate converted into bytes instead of bits
      /// </summary>
      KeccakMaximumRateInBytes     = KeccakMaximumRate div 8;

      /// <summary>
      ///   Number of times to run the algorithm on the data
      /// </summary>
      cKeccakNumberOfRounds        = 24;

      /// <summary>
      ///   Precalculated values for the 24 rounds of the algorithm
      /// </summary>
      cRoundConstants : array[0..23] of UInt64 = (
        UInt64($0000000000000001), UInt64($0000000000008082),
        UInt64($800000000000808A), UInt64($8000000080008000),
        UInt64($000000000000808B), UInt64($0000000080000001),
        UInt64($8000000080008081), UInt64($8000000000008009),
        UInt64($000000000000008A), UInt64($0000000000000088),
        UInt64($0000000080008009), UInt64($000000008000000A),
        UInt64($000000008000808B), UInt64($800000000000008B),
        UInt64($8000000000008089), UInt64($8000000000008003),
        UInt64($8000000000008002), UInt64($8000000000000080),
        UInt64($000000000000800A), UInt64($800000008000000A),
        UInt64($8000000080008081), UInt64($8000000000008080),
        UInt64($0000000080000001), UInt64($8000000080008008)
      );
    type
      TState_B = packed array[0..KeccakPermutationSizeInBytes-1] of UInt8;
      TState_L = packed array[0..(KeccakPermutationSizeInBytes) div 4 - 1] of Int32;
      TKDQueue = packed array[0..KeccakMaximumRateInBytes-1] of UInt8;

      /// <summary>
      ///   Calculation status of the algorithm
      /// </summary>
      TSpongeState = packed record
                       State                     : TState_B;
                       /// <summary>
                       ///   Data of the queue to be processed
                       /// </summary>
                       DataQueue                 : TKDQueue;
                       /// <summary>
                       ///   Bitrate r of Keccak
                       /// </summary>
                       Rate                      : UInt16;
                       /// <summary>
                       ///   Capacity c of Keccak
                       /// </summary>
                       Capacity                  : UInt16;
                       /// <summary>
                       ///   How many bits are in the queue
                       /// </summary>
                       BitsInQueue               : UInt16;
                       /// <summary>
                       ///   Length of the hash value to generate in bit
                       /// </summary>
                       FixedOutputLength         : UInt16;
                       /// <summary>
                       ///   Number of bits which can be squeezed
                       /// </summary>
                       bitsAvailableForSqueezing : UInt16;
                       /// <summary>
                       ///   Flag which is set to true when entering the
                       ///   squeezing state. Suppresses further absorb calls.
                       /// </summary>
                       SqueezeActive             : Boolean;
                       /// <summary>
                       ///   If an operation fails it sets this error code
                       /// </summary>
    //                   Fill3: packed array[405..HASHCTXSIZE] of byte;
                     end;

      /// <summary>
      ///   Buffer type
      /// </summary>
      TBABytes = array[0..65535] of UInt8;
      /// <summary>
      ///   Pointer to a buffer
      /// </summary>
      PBABytes = ^TBABytes;
      /// <summary>
      ///   Type for the generated hash value
      /// </summary>
      TSHA3Digest = array[0..63] of UInt8;

    var
      /// <summary>
      ///   The generated hash value is stored here
      /// </summary>
      FDigest      : TSHA3Digest;

    /// <summary>
    ///   Function to give input data for the sponge function to absorb
    /// </summary>
    /// <param name="Data">
    ///   Pointer to the data to work on
    /// </param>
    /// <param name="DatabitLen">
    ///   Length of the data passed via the pointer in bit
    /// </param>
    /// <remarks>
    ///   Raises an EDECHashEception when DataBit len not divideable by 8 without
    ///   reminder or when already in squeezin state.
    /// </remarks>
    procedure Absorb(Data: PBABytes; DatabitLen: Int32);

    /// <summary>
    ///   Absorb remaining bits from queue
    /// </summary>
    procedure AbsorbQueue;

    {$IFDEF PUREPASCAL}
    /// <summary>
    ///   Circular left shift
    /// </summary>
    /// <param name="x">
    ///   Value to be shifted
    /// </param>
    /// <param name="c">
    ///   Number of bits the value will be shifted
    /// </param>
    /// <returns>
    ///   Shifted value
    /// </returns>
    function RotL(const x: UInt64; c: Integer): UInt64; inline;

    /// <summary>
    ///   Circular left shift by 1
    /// </summary>
    /// <param name="x">
    ///   Value to be shifted
    /// </param>
    /// <returns>
    ///   Shifted value
    /// </returns>
    function RotL1(var x: UInt64): UInt64; inline;
    {$ENDIF}
    /// <summary>
    ///   Permutates the values in the passed state
    /// </summary>
    /// <param name="State">
    ///   State to permutate
    /// </param>
    procedure KeccakPermutation(var State: TState_L);

    /// <summary>
    ///   Carries out the XorIntoState and the permutation
    /// </summary>
    /// <param name="State">
    ///   State of the algorithm which gets modified by the permutation in this method
    /// </param>
    /// <param name="Data">
    ///   Pointer to the data to operate on
    /// </param>
    /// <param name="LaneCount">
    ///   Number of times the loop in this algorithm has tpo be carried out
    /// </param>
    procedure KeccakAbsorb(var state: TState_B; data: PUInt64; laneCount: Integer);

    /// <summary>
    ///   Include input message data bits into the sponge state
    /// </summary>
    procedure XORIntoState(var state: TState_L; pI: PUInt64; laneCount: Integer);

    /// <summary>
    ///   Update state with DataBitLen bits from data. May be called multiple
    ///   times, only the last DataBitLen may be a non-multiple of 8
    ///   (the corresponding byte) must be MSB aligned, i.e. in the
    ///   (databitlen and 7) most significant bits.
    /// </summary>
    /// <param name="data">
    ///   Data to work on
    /// </param>
    /// <param name="DataBitLen">
    ///   Length of the data in bits
    /// </param>
    procedure DoUpdate(Data: Pointer; DataBitLen: Int32);

    /// <summary>
    ///   Squeeze output data from the sponge function. If the sponge function
    ///   was in the absorbing phase, this function switches it to the squeezing
    ///   phase.
    /// </summary>
    /// <param name="Output">
    ///   pointer to the buffer where to store the output data
    /// </param>
    /// <param name="OutputLength">
    ///   number of output bits desired, must be a multiple of 8.
    /// </param>
    /// <returns>
    ///   0 if successful, 1 otherwise.
    /// </returns>
    procedure Squeeze(var Output: TSHA3Digest; OutputLength: Int32);
    /// <summary>
    ///   The algorithm starts in the absorb phase (one puts data into the sponge)
    ///   and ends with the squeze phase (one squeezes the sponge) and this method
    ///   does everything needed at the transition point between these two phases
    /// </summary>
    procedure PadAndSwitchToSqueezingPhase;

    /// <summary>
    ///   ???
    /// </summary>
    /// <param name="Outp">
    ///   Pointer where the output will be stored in
    /// </param>
    /// <param name="State">
    ///   State to work on
    /// </param>
    /// <param name="LaneCount">
    ///   Number of iterations
    /// </param>
    procedure ExtractFromState(Outp: Pointer; const State: TState_L; LaneCount: Integer);

    /// <summary>
    ///   Update final bits in LSB format, pad them, and compute the hash value
    /// </summary>
    /// <param name="Bits">
    ///   Value used for padding if the length of the message to be hashed
    ///   is not a multiple of 8 bit bytes.
    /// </param>
    /// <param name="BitLen">
    ///   Number of needed padding bits?
    /// </param>
    /// <param name="HashValue">
    ///   The hash value which shall be updated by this method
    /// </param>
    procedure FinalBit_LSB(Bits: Byte; Bitlen: UInt16;
                            var HashValue: TSHA3Digest);
  strict protected
    /// <summary>
    ///   Contains the current state of the algorithms sponge part
    /// </summary>
    FSpongeState : TSpongeState;

    /// <summary>
    ///   Initializes the state of the Keccak/SHA3 sponge function. It is set to
    ///   the absorbing phase by this. If invalid parameter values are specified
    ///   a EDECHashException will be raised
    /// </summary>
    /// <param name="rate">
    ///   Block length of the message to be processed, depends directly on the
    ///   SHA3 variant (224, 256...) to be used
    /// </param>
    /// <param name="capacity">
    ///   Capacity c (it could directly be calculated from the rate as
    ///   c = 1600 - r but the original author Wolfgang Erhardt decided against
    ///   this.
    ///   The capacity is the size of that part of the state vector which, when
    ///   xored with the message blocks and when extracting the resulting hash,
    ///   stays untouched.
    /// </param>
    procedure InitSponge(Rate, Capacity: UInt16);

    /// <summary>
    ///   Init internal data
    /// </summary>
    procedure DoInit; override;
    /// <summary>
    ///   Dummy method to avoid the compiler warning about a class with abstract method
    /// </summary>
    procedure DoTransform(Buffer: PUInt32Array); override;
    /// <summary>
    ///   Final step of the calculation
    /// </summary>
    procedure DoDone; override;
  public
    /// <summary>
    ///   Processes one chunk of data to be hashed.
    /// </summary>
    /// <param name="Data">
    ///   Data on which the hash value shall be calculated on
    /// </param>
    /// <param name="DataSize">
    ///   Size of the data in bytes
    /// </param>
    procedure Calc(const Data; DataSize: Integer); override;
    /// <summary>
    ///   Returns the calculated hash value
    /// </summary>
    /// <returns>
    ///   Hash value calculated
    /// </returns>
    function Digest: PByteArray; override;
  end;

  /// <summary>
  ///   224 bit SHA3 variant
  /// </summary>
  THash_SHA3_224 = class(THash_SHA3Base)
  protected
    procedure DoInit; override;
  public
    class function BlockSize: UInt32; override;
    class function DigestSize: UInt32; override;
  end;

  /// <summary>
  ///   256 bit SHA3 variant
  /// </summary>
  THash_SHA3_256 = class(THash_SHA3Base)
  protected
    procedure DoInit; override;
  public
    class function BlockSize: UInt32; override;
    class function DigestSize: UInt32; override;
  end;

  /// <summary>
  ///   384 bit SHA3 variant
  /// </summary>
  THash_SHA3_384 = class(THash_SHA3Base)
  protected
    procedure DoInit; override;
  public
    class function BlockSize: UInt32; override;
    class function DigestSize: UInt32; override;
  end;

  /// <summary>
  ///   512 bit SHA3 variant
  /// </summary>
  THash_SHA3_512 = class(THash_SHA3Base)
  protected
    procedure DoInit; override;
  public
    class function BlockSize: UInt32; override;
    class function DigestSize: UInt32; override;
  end;

  THavalBaseTransformMethod = procedure(Buffer: PUInt32Array) of object;

  THashBaseHaval = class(TDECHashAuthentication)
  private
    FDigest: array[0..7] of UInt32;
    FRounds: UInt32;
    FTransform: THavalBaseTransformMethod;
    procedure SetRounds(Value: UInt32);
  protected
    procedure DoInit; override;
    procedure DoTransform(Buffer: PUInt32Array); override;
    procedure DoTransform3(Buffer: PUInt32Array);
    procedure DoTransform4(Buffer: PUInt32Array);
    procedure DoTransform5(Buffer: PUInt32Array);
    procedure DoDone; override;
  public
    function Digest: PByteArray; override;
    class function BlockSize: UInt32; override;
    ///   Returns the minimum possible number for the rounds parameter.
    ///   Value depends on Digest size which depends on concrete implementation
    /// </summary>
    class function GetMinRounds: UInt8;
    /// <summary>
    ///   Returns the maximum possible number for the rounds parameter.
    ///   Value depends on Digest size which depends on concrete implementation
    /// </summary>
    class function GetMaxRounds: UInt8;

    /// <summary>
    ///   Defines the number of rounds the algorithm performs on the input data.
    ///   The range for this parameter is 3-5 rounds. If a value outside this
    ///   range is assigned, the value used depends on the DigestSize. For
    ///   DigestSizes <= 20 it will be set to 3, for values <= 28 to 4 and for
    ///   bigger values to 5. For 3 rounds the algorithm is considered unsafe,
    ///   as in 2003 collisions could be found with a setting of 3 rounds only.
    /// </summary>
    property Rounds: UInt32 read FRounds write SetRounds default 3;
  end;

  /// <summary>
  ///   In 2004 collisions for this one were found, so this one should be
  ///   considered to be unsafe.
  /// </summary>
  THash_Haval128 = class(THashBaseHaval)
  public
    class function DigestSize: UInt32; override;
  end;

  THash_Haval160 = class(THashBaseHaval)
  public
    class function DigestSize: UInt32; override;
  end;

  THash_Haval192 = class(THashBaseHaval)
  public
    class function DigestSize: UInt32; override;
  end;

  THash_Haval224 = class(THashBaseHaval)
  public
    class function DigestSize: UInt32; override;
  end;

  THash_Haval256 = class(THashBaseHaval)
  public
    class function DigestSize: UInt32; override;
  end;

  /// <summary>
  ///   This is actually an implementation of the 192 bit variant of the Tiger
  ///   hash algorithm with 3 rounds, unless a different value is assigned
  ///   to the rounds property. It is considered to be unsafe at least in the
  ///   192 Bit variant!
  /// </summary>
  THash_Tiger = class(THashBaseMD4)
  private
    const
      /// <summary>
      ///   Minimum number of rounds for the Tigher hash function. Trying to set a
      ///   lower one sets the rounds to this value.
      /// </summary>
      cTigerMinRounds = 3;
      /// <summary>
      ///   Maximum number of rounds for the Tigher hash function. Trying to set a
      ///   higher one sets the rounds to this value.
      /// </summary>
      cTigerMaxRounds = 32;
    var
      FRounds: Integer;
      procedure SetRounds(Value: Integer);
  protected
    procedure DoInit; override;
    procedure DoTransform(Buffer: PUInt32Array); override;
  public
    class function DigestSize: UInt32; override;
    /// <summary>
    ///   Returns the minimum possible number for the rounds parameter
    /// </summary>
    class function GetMinRounds: UInt8;
    /// <summary>
    ///   Returns the maximum possible number for the rounds parameter
    /// </summary>
    class function GetMaxRounds: UInt8;

    /// <summary>
    ///   Defines the number of rounds the algorithm will perform on the data
    ///   passed. Valid values are in the range from 3-32 rounds and values
    ///   outside this range will lead to a rounds value of 3 or 32 to be used,
    ///   depending on whether a lower or higher value has been given.
    /// </summary>
    property Rounds: Integer read FRounds write SetRounds default 3;
  end;

  /// <summary>
  ///   As there seem to exist 128 and 160 bit variants of Tiger, which seem to
  ///   be truncated variants of Tiger 192, but we want to keep compatibility
  ///   with old code we introduce an alias for the time being.
  ///   It is considered to be unsafe at least in the 192 Bit variant!
  /// </summary>
  THash_Tiger192 = THash_Tiger;

  /// <summary>
  ///   The Panama algorithm is being considered to be unsafe. Support is only
  ///   being provided for backward compatibility.
  /// </summary>
  THash_Panama = class(TDECHashAuthentication)
  private
    FLFSRBuffer: array[0..31, 0..7] of UInt32;
    FDigest: array[0..16] of UInt32;
    FTap: UInt32;
  protected
    procedure DoInit; override;
    procedure DoTransform(Buffer: PUInt32Array); override;
    procedure DoDone; override;
    procedure DoPull;
  public
    function Digest: PByteArray; override;
    class function DigestSize: UInt32; override;
    class function BlockSize: UInt32; override; // 32
  end;

  THashBaseWhirlpool = class(TDECHashAuthentication)
  private
    FDigest: array[0..15] of UInt32;
    FTableC: Pointer;
    FTableR: Pointer;
  protected
    procedure DoTransform(Buffer: PUInt32Array); override;
    procedure DoDone; override;
  public
    function Digest: PByteArray; override;
    class function DigestSize: UInt32; override;
    class function BlockSize: UInt32; override;
  end;

  /// <summary>
  ///   This is the original variant of the algorithmus. Do not use it as some
  ///   security flaw has been detected early on by its inventors. DEC contains
  ///   it for backwards compatibility and completeness.
  /// </summary>
  THash_Whirlpool0 = class(THashBaseWhirlpool)
  protected
    procedure DoInit; override;
  end;

  /// <summary>
  ///   This is variant of the algorithmus fixing the security flaw of the
  ///   original version Whirlpool0. Do not use it in new code as it has been
  ///   superseeded by the optimized Whirlpool1 (THash_Whirlpool1 class in DEC)
  ///   variant which is additionally more safe as well! It is there for
  ///   backwards compatibility and completeness only.
  /// </summary>
  THash_WhirlpoolT = class(THashBaseWhirlpool)
  protected
    procedure DoInit; override;
  end;

  /// <summary>
  ///   The current version of Whirlpool but not the one used in code developed
  ///   against the older DEC 5.x versions. The name of the one used in your
  ///   code differs, depending whether you opt tu use the old DEC 5.2 compatible
  ///   class names where the name Whirlpool1 was already taken by the variant
  ///   nowadays known as Whirlpool-T.
  /// </summary>
  THash_Whirlpool1_ = class(THashBaseWhirlpool)
  protected
    procedure DoInit; override;
  end;

  {$IFDEF OLD_WHIRLPOOL_NAMES}
  /// <summary>
  ///   This is the original variant of the algorithmus. Do not use it as some
  ///   security flaw has been detected early on by its inventors. DEC contains
  ///   it for backwards compatibility and completeness.
  /// </summary>
  THash_Whirlpool = class(THash_Whirlpool0);
  /// <summary>
  ///   This is variant of the algorithmus fixing the security flaw of the
  ///   original version Whirlpool0. Do not use it in new code as it has been
  ///   superseeded by the optimized Whirlpool1 (THash_Whirlpool1 class in DEC)
  ///   variant which is additionally more safe as well! It is there for
  ///   backwards compatibility and completeness only.
  /// </summary>
  THash_Whirlpool1 = class(THash_WhirlpoolT);
  /// <summary>
  ///   The current version of Whirlpool but not the one used in code developed
  ///   against the older DEC 5.x versions. The name of the one used in your
  ///   code differs, depending whether you opt tu use the old DEC 5.2 compatible
  ///   class names where the name Whirlpool1 was already taken by the variant
  ///   nowadays known as Whirlpool-T.
  /// </summary>
  THash_Whirlpool1New = class(THash_Whirlpool1_);
  {$ELSE}
  /// <summary>
  ///   The current version of Whirlpool but not the one used in code developed
  ///   against the older DEC 5.x versions. The name of the one used in your
  ///   code differs, depending whether you opt tu use the old DEC 5.2 compatible
  ///   class names where the name Whirlpool1 was already taken by the variant
  ///   nowadays known as Whirlpool-T.
  /// </summary>
  THash_Whirlpool1 = class(THash_Whirlpool1_);
  {$ENDIF}

  THash_Square = class(TDECHashAuthentication)
  private
    FDigest: array[0..3] of UInt32;
  protected
    procedure DoInit; override;
    procedure DoTransform(Buffer: PUInt32Array); override;
    procedure DoDone; override;
  public
    function Digest: PByteArray; override;
    class function DigestSize: UInt32; override;
    class function BlockSize: UInt32; override;
  end;

  /// <summary>
  ///   This 1990 developed hash function was named after the Egyptian Pharaoh
  ///   Sneferu. Be sure to set SecurityLevel to at least 8. See remark there.
  /// </summary>
  THashBaseSnefru = class(TDECHashAuthentication)
  private
    FDigest: array[0..23] of UInt32;
    /// <summary>
    ///   Number of rounds the loop will do on the data
    /// </summary>
    FRounds: Integer;
    /// <summary>
    ///   Sets the number of rounds for the looping over the data
    /// </summary>
    procedure SetRounds(Value: Integer);
  protected
    procedure DoInit; override;
    procedure DoDone; override;
  public
    function Digest: PByteArray; override;
    ///   Returns the minimum possible number for the rounds parameter.
    ///   Value depends on Digest size which depends on concrete implementation
    /// </summary>
    class function GetMinRounds: UInt8;
    ///   Returns the maximum possible number for the rounds parameter.
    ///   Value depends on Digest size which depends on concrete implementation
    /// </summary>
    class function GetMaxRounds: UInt8;

    /// <summary>
    ///   Can be set from 2 to 8, default is 8. This is the number of rounds the
    ///   algorithm will use. With the default of 8 rounds it is being considered
    ///   as safe as of spring 2016, with less rounds this algorithm is considered
    ///   to be unsafe and even with 8 rounds it is not really strong.
    /// </summary>
    property Rounds: Integer
      read   FRounds
      write  SetRounds;
  end;

  /// <summary>
  ///   This 1990 developed hash function was named after the Egyptian Pharaoh
  ///   Sneferu. Be sure to set SecurityLevel to at least 8. See remark for
  ///   THashBaseSnefru.SecurityLevel.
  /// </summary>
  THash_Snefru128 = class(THashBaseSnefru)
  protected
    procedure DoTransform(Buffer: PUInt32Array); override;
  public
    class function DigestSize: UInt32; override;
    class function BlockSize: UInt32; override; // 48
  end;

  /// <summary>
  ///   This 1990 developed hash function was named after the Egyptian Pharaoh
  ///   Sneferu. Be sure to set SecurityLevel to at least 8. See remark
  ///   THashBaseSnefru.SecurityLevel.
  /// </summary>
  THash_Snefru256 = class(THashBaseSnefru)
  protected
    procedure DoTransform(Buffer: PUInt32Array); override;
  public
    class function DigestSize: UInt32; override;
    class function BlockSize: UInt32; override; // 32
  end;

  THash_Sapphire = class(TDECHashAuthentication)
  private
    FCards: array[0..255] of UInt32;
    FDigest: array[0..15] of UInt32;
    FRotor: UInt32;
    FRatchet: UInt32;
    FAvalanche: UInt32;
    FPlain: UInt32;
    FCipher: UInt32;
    FDigestSize: UInt8;

    /// <summary>
    ///   Set the length of the output hash value in byte.
    /// </summary>
    /// <param name="Value">
    ///   Minimum value is 1 byte, maximum value is 64 byte = 512 bit.
    ///   Sets the size to the default size returned by DigestSize otherwise.
    ///   is specified.
    /// </param>
    procedure SetDigestSize(Value: UInt8);
  protected
    procedure DoInit; override;
    procedure DoDone; override;
    procedure DoTransform(Buffer: PUInt32Array); override;
  public
    function Digest: PByteArray; override;
    function DigestAsBytes: TBytes; override;
    /// <summary>
    ///   Returns the default digest/hash size in bit. If RequestedDigestSize is
    ///   not set, the defauilt size returned here is being used.
    /// </summary>
    class function DigestSize: UInt32; override;
    /// <summary>
    ///   Returns on which block size this algorithm operates. Since the Sapphire
    ///   hash originates from a Sapphire stream cipher algorithm this is always 1.
    /// </summary>
    class function BlockSize: UInt32; override;
    procedure Calc(const Data; DataSize: Integer); override;

    /// <summary>
    ///   This property defines the length of the output from the hash calculation
    ///   in byte. The maximum value is 64 byte = 512 bit. Values bigger 64 byte
    ///   and a value of 0 lead to the default size returned by DigestSize otherwise.
    ///   This setting is only respected by the DigestAsBytes method and all other
    ///   convenience methods using that one like CalcStream, CalcString,
    ///   DigestAsString or DigestAsRawString.
    /// </summary>
    property RequestedDigestSize: UInt8
      read   FDigestSize
      write  SetDigestSize;
  end;

implementation

uses
  DECData, DECDataHash;

{$IFOPT Q+}{$DEFINE RESTORE_OVERFLOWCHECKS}{$Q-}{$ENDIF}
{$IFOPT R+}{$DEFINE RESTORE_RANGECHECKS}{$R-}{$ENDIF}

{$IFDEF X86ASM}
  {$DEFINE INCLUDED} // allows having the DECHash.inc in the IDE's project manager
  {$INCLUDE DECHash.asm86.inc}
{$ENDIF !X86ASM}

{ Speed comparison of ASM vs. PurePascal Implementation. Valid only for Win32 compiler
  and this was for DEC 5.1 and thus compiler versions < D2009!

                                           assembler                             pascal

  THash_SHA512    :       85.1 cycles/byte      17.62 Mb/sec      220.9 cycles/byte       6.79 Mb/sec  159%
  THash_SHA384    :       85.2 cycles/byte      17.61 Mb/sec      220.0 cycles/byte       6.82 Mb/sec  158%
  THash_Tiger     :       24.6 cycles/byte      60.98 Mb/sec       60.7 cycles/byte      24.69 Mb/sec  147%
  THash_Haval128  :       13.3 cycles/byte     112.55 Mb/sec       26.0 cycles/byte      57.77 Mb/sec   95%
  THash_SHA1      :       20.1 cycles/byte      74.80 Mb/sec       36.1 cycles/byte      41.51 Mb/sec   80%
  THash_SHA       :       20.0 cycles/byte      75.03 Mb/sec       35.5 cycles/byte      42.21 Mb/sec   78%
  THash_Haval160  :       13.2 cycles/byte     113.30 Mb/sec       22.7 cycles/byte      66.12 Mb/sec   71%
  THash_Haval256  :       25.9 cycles/byte      57.84 Mb/sec       40.5 cycles/byte      37.07 Mb/sec   56%
  THash_Snefru128 :      159.7 cycles/byte       9.39 Mb/sec      248.2 cycles/byte       6.04 Mb/sec   55%
  THash_Snefru256 :      239.3 cycles/byte       6.27 Mb/sec      367.9 cycles/byte       4.08 Mb/sec   54%
  THash_RipeMD256 :       14.5 cycles/byte     103.16 Mb/sec       21.4 cycles/byte      70.08 Mb/sec   47%
  THash_MD4       :        5.8 cycles/byte     256.73 Mb/sec        8.5 cycles/byte     176.92 Mb/sec   45%

  THash_MD2       :      251.6 cycles/byte       5.96 Mb/sec      366.1 cycles/byte       4.10 Mb/sec   45%
  THash_RipeMD128 :       15.2 cycles/byte      98.89 Mb/sec       21.2 cycles/byte      70.61 Mb/sec   40%
  THash_RipeMD320 :       25.5 cycles/byte      58.73 Mb/sec       35.8 cycles/byte      41.87 Mb/sec   40%
  THash_MD5       :        8.9 cycles/byte     169.43 Mb/sec       11.4 cycles/byte     131.01 Mb/sec   29%
  THash_RipeMD160 :       26.5 cycles/byte      56.66 Mb/sec       31.4 cycles/byte      47.79 Mb/sec   19%
  THash_Square    :       44.7 cycles/byte      33.58 Mb/sec       53.1 cycles/byte      28.23 Mb/sec   19%
  THash_Haval192  :       32.5 cycles/byte      46.17 Mb/sec       37.6 cycles/byte      39.87 Mb/sec   18%
  THash_WhirlpoolT:      104.9 cycles/byte      14.30 Mb/sec      122.8 cycles/byte      12.22 Mb/sec   17%
  THash_Whirlpool0:      104.7 cycles/byte      14.33 Mb/sec      119.9 cycles/byte      12.51 Mb/sec   15%
  THash_Sapphire  :       52.9 cycles/byte      28.35 Mb/sec       53.8 cycles/byte      27.86 Mb/sec    2%
  THash_Haval224  :       32.0 cycles/byte      46.82 Mb/sec       32.3 cycles/byte      46.46 Mb/sec    1%
  THash_SHA256    :       47.8 cycles/byte      31.35 Mb/sec       47.8 cycles/byte      31.39 Mb/sec    0%
  THash_Panama    :        8.9 cycles/byte     169.01 Mb/sec        7.3 cycles/byte     206.55 Mb/sec  -18%
}

resourcestring
  /// <summary>
  ///   Failure message when a hash algorithm is initialized with wrong parameters
  /// </summary>
  sHashInitFailure   = 'Invalid %0:s algorithm initialization parameters specified: %1:s';
  /// <summary>
  ///   Failure message when absorb is callt with a bitlength not divideable by 8
  ///   without reminder or when it is called while already in squeezing state
  /// </summary>
  aSHA3AbsorbFailure = 'Absorb: number of bits mod 8 <> 0 or squeezing active. Bits: %0:d, '+
                       'Squeezing: %1:s';

{ THash_MD2 }

{$IFNDEF THash_MD2_asm}
procedure THash_MD2.DoTransform(Buffer: PUInt32Array);
var
  I, J, T: UInt32;
begin
  for I := 0 to 3 do
  begin
    PUInt32Array(@FDigest[16])[I] := Buffer[I];
    PUInt32Array(@FDigest[32])[I] := PUInt32Array(@FDigest[0])[I] xor PUInt32Array(@FDigest[16])[I];
  end;
  T := FDigest[63];
  for I := 0 to 15 do
  begin
    T := FDigest[I + 48] xor MD2_PISubst[FDigest[I + 16] xor Byte(T)];
    FDigest[I + 48] := Byte(T);
  end;
  T := 0;
  for I := 0 to 17 do
  begin
    for J := 0 to 47 do
    begin
      T := FDigest[J] xor MD2_PISubst[T];
      FDigest[J] := Byte(T);
    end;
    T := (T + I) and $FF;
  end;
end;
{$ENDIF !THash_MD2_asm}

procedure THash_MD2.DoInit;
begin
  FillChar(FDigest, SizeOf(FDigest), 0);
end;

procedure THash_MD2.DoDone;
var
  Remain: Integer;
begin
  Remain := FBufferSize - FBufferIndex;
  FillChar(FBuffer[FBufferIndex], Remain, Remain);
  DoTransform(Pointer(FBuffer));
  Move(FDigest[48], FBuffer^, FBufferSize);
  DoTransform(Pointer(FBuffer));
end;

function THash_MD2.Digest: PByteArray;
begin
  Result := @FDigest;
end;

class function THash_MD2.DigestSize: UInt32;
begin
  Result := 16;
end;

class function THash_MD2.BlockSize: UInt32;
begin
  Result := 16;
end;

{ THashBaseMD4 }

procedure THashBaseMD4.DoInit;
begin
  FDigest[0] := $67452301;
  FDigest[1] := $EFCDAB89;
  FDigest[2] := $98BADCFE;
  FDigest[3] := $10325476;
  FDigest[4] := $C3D2E1F0;
  FDigest[5] := $76543210;
  FDigest[6] := $FEDCBA98;
  FDigest[7] := $89ABCDEF;
  FDigest[8] := $01234567;
  FDigest[9] := $3C2D1E0F;
end;

procedure THashBaseMD4.DoDone;
begin
  if FCount[2] or FCount[3] <> 0 then
    RaiseHashOverflowError;
  if FPaddingByte = 0 then
    FPaddingByte := $80;
  FBuffer[FBufferIndex] := FPaddingByte;
  Inc(FBufferIndex);
  if FBufferIndex > FBufferSize - 8 then
  begin
    FillChar(FBuffer[FBufferIndex], FBufferSize - FBufferIndex, 0);
    DoTransform(Pointer(FBuffer));
    FBufferIndex := 0;
  end;
  FillChar(FBuffer[FBufferIndex], FBufferSize - FBufferIndex, 0);
  Move(FCount, FBuffer[FBufferSize - 8], 8);
  DoTransform(Pointer(FBuffer));
end;

function THashBaseMD4.Digest: PByteArray;
begin
  Result := @FDigest;
end;

class function THashBaseMD4.DigestSize: UInt32;
begin
  Result := 16;
end;

class function THashBaseMD4.BlockSize: UInt32;
begin
  Result := 64;
end;

{ THash_MD4 }

{$IFNDEF THash_MD4_asm}
procedure THash_MD4.DoTransform(Buffer: PUInt32Array);
const
  S1 = $5A827999;
  S2 = $6ED9EBA1;
var
  A, B, C, D: UInt32;
begin
  A := FDigest[0];
  B := FDigest[1];
  C := FDigest[2];
  D := FDigest[3];

  Inc(A, B and C or not B and D + Buffer[ 0]); A := A shl  3 or A shr 29;
  Inc(D, A and B or not A and C + Buffer[ 1]); D := D shl  7 or D shr 25;
  Inc(C, D and A or not D and B + Buffer[ 2]); C := C shl 11 or C shr 21;
  Inc(B, C and D or not C and A + Buffer[ 3]); B := B shl 19 or B shr 13;
  Inc(A, B and C or not B and D + Buffer[ 4]); A := A shl  3 or A shr 29;
  Inc(D, A and B or not A and C + Buffer[ 5]); D := D shl  7 or D shr 25;
  Inc(C, D and A or not D and B + Buffer[ 6]); C := C shl 11 or C shr 21;
  Inc(B, C and D or not C and A + Buffer[ 7]); B := B shl 19 or B shr 13;
  Inc(A, B and C or not B and D + Buffer[ 8]); A := A shl  3 or A shr 29;
  Inc(D, A and B or not A and C + Buffer[ 9]); D := D shl  7 or D shr 25;
  Inc(C, D and A or not D and B + Buffer[10]); C := C shl 11 or C shr 21;
  Inc(B, C and D or not C and A + Buffer[11]); B := B shl 19 or B shr 13;
  Inc(A, B and C or not B and D + Buffer[12]); A := A shl  3 or A shr 29;
  Inc(D, A and B or not A and C + Buffer[13]); D := D shl  7 or D shr 25;
  Inc(C, D and A or not D and B + Buffer[14]); C := C shl 11 or C shr 21;
  Inc(B, C and D or not C and A + Buffer[15]); B := B shl 19 or B shr 13;

  Inc(A, B and C or B and D or C and D + Buffer[ 0] + S1); A := A shl  3 or A shr 29;
  Inc(D, A and B or A and C or B and C + Buffer[ 4] + S1); D := D shl  5 or D shr 27;
  Inc(C, D and A or D and B or A and B + Buffer[ 8] + S1); C := C shl  9 or C shr 23;
  Inc(B, C and D or C and A or D and A + Buffer[12] + S1); B := B shl 13 or B shr 19;
  Inc(A, B and C or B and D or C and D + Buffer[ 1] + S1); A := A shl  3 or A shr 29;
  Inc(D, A and B or A and C or B and C + Buffer[ 5] + S1); D := D shl  5 or D shr 27;
  Inc(C, D and A or D and B or A and B + Buffer[ 9] + S1); C := C shl  9 or C shr 23;
  Inc(B, C and D or C and A or D and A + Buffer[13] + S1); B := B shl 13 or B shr 19;
  Inc(A, B and C or B and D or C and D + Buffer[ 2] + S1); A := A shl  3 or A shr 29;
  Inc(D, A and B or A and C or B and C + Buffer[ 6] + S1); D := D shl  5 or D shr 27;
  Inc(C, D and A or D and B or A and B + Buffer[10] + S1); C := C shl  9 or C shr 23;
  Inc(B, C and D or C and A or D and A + Buffer[14] + S1); B := B shl 13 or B shr 19;
  Inc(A, B and C or B and D or C and D + Buffer[ 3] + S1); A := A shl  3 or A shr 29;
  Inc(D, A and B or A and C or B and C + Buffer[ 7] + S1); D := D shl  5 or D shr 27;
  Inc(C, D and A or D and B or A and B + Buffer[11] + S1); C := C shl  9 or C shr 23;
  Inc(B, C and D or C and A or D and A + Buffer[15] + S1); B := B shl 13 or B shr 19;

  Inc(A, B xor C xor D + Buffer[ 0] + S2); A := A shl  3 or A shr 29;
  Inc(D, A xor B xor C + Buffer[ 8] + S2); D := D shl  9 or D shr 23;
  Inc(C, D xor A xor B + Buffer[ 4] + S2); C := C shl 11 or C shr 21;
  Inc(B, C xor D xor A + Buffer[12] + S2); B := B shl 15 or B shr 17;
  Inc(A, B xor C xor D + Buffer[ 2] + S2); A := A shl  3 or A shr 29;
  Inc(D, A xor B xor C + Buffer[10] + S2); D := D shl  9 or D shr 23;
  Inc(C, D xor A xor B + Buffer[ 6] + S2); C := C shl 11 or C shr 21;
  Inc(B, C xor D xor A + Buffer[14] + S2); B := B shl 15 or B shr 17;
  Inc(A, B xor C xor D + Buffer[ 1] + S2); A := A shl  3 or A shr 29;
  Inc(D, A xor B xor C + Buffer[ 9] + S2); D := D shl  9 or D shr 23;
  Inc(C, D xor A xor B + Buffer[ 5] + S2); C := C shl 11 or C shr 21;
  Inc(B, C xor D xor A + Buffer[13] + S2); B := B shl 15 or B shr 17;
  Inc(A, B xor C xor D + Buffer[ 3] + S2); A := A shl  3 or A shr 29;
  Inc(D, A xor B xor C + Buffer[11] + S2); D := D shl  9 or D shr 23;
  Inc(C, D xor A xor B + Buffer[ 7] + S2); C := C shl 11 or C shr 21;
  Inc(B, C xor D xor A + Buffer[15] + S2); B := B shl 15 or B shr 17;

  Inc(FDigest[0], A);
  Inc(FDigest[1], B);
  Inc(FDigest[2], C);
  Inc(FDigest[3], D);
end;
{$ENDIF}

{ THash_MD5 }

{$IFNDEF THash_MD5_asm}
procedure THash_MD5.DoTransform(Buffer: PUInt32Array);
var
  A, B, C, D: UInt32;
begin
  A := FDigest[0];
  B := FDigest[1];
  C := FDigest[2];
  D := FDigest[3];

  Inc(A, Buffer[ 0] + $D76AA478 + (D xor (B and (C xor D)))); A := A shl  7 or A shr 25 + B;
  Inc(D, Buffer[ 1] + $E8C7B756 + (C xor (A and (B xor C)))); D := D shl 12 or D shr 20 + A;
  Inc(C, Buffer[ 2] + $242070DB + (B xor (D and (A xor B)))); C := C shl 17 or C shr 15 + D;
  Inc(B, Buffer[ 3] + $C1BDCEEE + (A xor (C and (D xor A)))); B := B shl 22 or B shr 10 + C;
  Inc(A, Buffer[ 4] + $F57C0FAF + (D xor (B and (C xor D)))); A := A shl  7 or A shr 25 + B;
  Inc(D, Buffer[ 5] + $4787C62A + (C xor (A and (B xor C)))); D := D shl 12 or D shr 20 + A;
  Inc(C, Buffer[ 6] + $A8304613 + (B xor (D and (A xor B)))); C := C shl 17 or C shr 15 + D;
  Inc(B, Buffer[ 7] + $FD469501 + (A xor (C and (D xor A)))); B := B shl 22 or B shr 10 + C;
  Inc(A, Buffer[ 8] + $698098D8 + (D xor (B and (C xor D)))); A := A shl  7 or A shr 25 + B;
  Inc(D, Buffer[ 9] + $8B44F7AF + (C xor (A and (B xor C)))); D := D shl 12 or D shr 20 + A;
  Inc(C, Buffer[10] + $FFFF5BB1 + (B xor (D and (A xor B)))); C := C shl 17 or C shr 15 + D;
  Inc(B, Buffer[11] + $895CD7BE + (A xor (C and (D xor A)))); B := B shl 22 or B shr 10 + C;
  Inc(A, Buffer[12] + $6B901122 + (D xor (B and (C xor D)))); A := A shl  7 or A shr 25 + B;
  Inc(D, Buffer[13] + $FD987193 + (C xor (A and (B xor C)))); D := D shl 12 or D shr 20 + A;
  Inc(C, Buffer[14] + $A679438E + (B xor (D and (A xor B)))); C := C shl 17 or C shr 15 + D;
  Inc(B, Buffer[15] + $49B40821 + (A xor (C and (D xor A)))); B := B shl 22 or B shr 10 + C;

  Inc(A, Buffer[ 1] + $F61E2562 + (C xor (D and (B xor C)))); A := A shl  5 or A shr 27 + B;
  Inc(D, Buffer[ 6] + $C040B340 + (B xor (C and (A xor B)))); D := D shl  9 or D shr 23 + A;
  Inc(C, Buffer[11] + $265E5A51 + (A xor (B and (D xor A)))); C := C shl 14 or C shr 18 + D;
  Inc(B, Buffer[ 0] + $E9B6C7AA + (D xor (A and (C xor D)))); B := B shl 20 or B shr 12 + C;
  Inc(A, Buffer[ 5] + $D62F105D + (C xor (D and (B xor C)))); A := A shl  5 or A shr 27 + B;
  Inc(D, Buffer[10] + $02441453 + (B xor (C and (A xor B)))); D := D shl  9 or D shr 23 + A;
  Inc(C, Buffer[15] + $D8A1E681 + (A xor (B and (D xor A)))); C := C shl 14 or C shr 18 + D;
  Inc(B, Buffer[ 4] + $E7D3FBC8 + (D xor (A and (C xor D)))); B := B shl 20 or B shr 12 + C;
  Inc(A, Buffer[ 9] + $21E1CDE6 + (C xor (D and (B xor C)))); A := A shl  5 or A shr 27 + B;
  Inc(D, Buffer[14] + $C33707D6 + (B xor (C and (A xor B)))); D := D shl  9 or D shr 23 + A;
  Inc(C, Buffer[ 3] + $F4D50D87 + (A xor (B and (D xor A)))); C := C shl 14 or C shr 18 + D;
  Inc(B, Buffer[ 8] + $455A14ED + (D xor (A and (C xor D)))); B := B shl 20 or B shr 12 + C;
  Inc(A, Buffer[13] + $A9E3E905 + (C xor (D and (B xor C)))); A := A shl  5 or A shr 27 + B;
  Inc(D, Buffer[ 2] + $FCEFA3F8 + (B xor (C and (A xor B)))); D := D shl  9 or D shr 23 + A;
  Inc(C, Buffer[ 7] + $676F02D9 + (A xor (B and (D xor A)))); C := C shl 14 or C shr 18 + D;
  Inc(B, Buffer[12] + $8D2A4C8A + (D xor (A and (C xor D)))); B := B shl 20 or B shr 12 + C;

  Inc(A, Buffer[ 5] + $FFFA3942 + (B xor C xor D)); A := A shl  4 or A shr 28 + B;
  Inc(D, Buffer[ 8] + $8771F681 + (A xor B xor C)); D := D shl 11 or D shr 21 + A;
  Inc(C, Buffer[11] + $6D9D6122 + (D xor A xor B)); C := C shl 16 or C shr 16 + D;
  Inc(B, Buffer[14] + $FDE5380C + (C xor D xor A)); B := B shl 23 or B shr  9 + C;
  Inc(A, Buffer[ 1] + $A4BEEA44 + (B xor C xor D)); A := A shl  4 or A shr 28 + B;
  Inc(D, Buffer[ 4] + $4BDECFA9 + (A xor B xor C)); D := D shl 11 or D shr 21 + A;
  Inc(C, Buffer[ 7] + $F6BB4B60 + (D xor A xor B)); C := C shl 16 or C shr 16 + D;
  Inc(B, Buffer[10] + $BEBFBC70 + (C xor D xor A)); B := B shl 23 or B shr  9 + C;
  Inc(A, Buffer[13] + $289B7EC6 + (B xor C xor D)); A := A shl  4 or A shr 28 + B;
  Inc(D, Buffer[ 0] + $EAA127FA + (A xor B xor C)); D := D shl 11 or D shr 21 + A;
  Inc(C, Buffer[ 3] + $D4EF3085 + (D xor A xor B)); C := C shl 16 or C shr 16 + D;
  Inc(B, Buffer[ 6] + $04881D05 + (C xor D xor A)); B := B shl 23 or B shr  9 + C;
  Inc(A, Buffer[ 9] + $D9D4D039 + (B xor C xor D)); A := A shl  4 or A shr 28 + B;
  Inc(D, Buffer[12] + $E6DB99E5 + (A xor B xor C)); D := D shl 11 or D shr 21 + A;
  Inc(C, Buffer[15] + $1FA27CF8 + (D xor A xor B)); C := C shl 16 or C shr 16 + D;
  Inc(B, Buffer[ 2] + $C4AC5665 + (C xor D xor A)); B := B shl 23 or B shr  9 + C;

  Inc(A, Buffer[ 0] + $F4292244 + (C xor (B or not D))); A := A shl  6 or A shr 26 + B;
  Inc(D, Buffer[ 7] + $432AFF97 + (B xor (A or not C))); D := D shl 10 or D shr 22 + A;
  Inc(C, Buffer[14] + $AB9423A7 + (A xor (D or not B))); C := C shl 15 or C shr 17 + D;
  Inc(B, Buffer[ 5] + $FC93A039 + (D xor (C or not A))); B := B shl 21 or B shr 11 + C;
  Inc(A, Buffer[12] + $655B59C3 + (C xor (B or not D))); A := A shl  6 or A shr 26 + B;
  Inc(D, Buffer[ 3] + $8F0CCC92 + (B xor (A or not C))); D := D shl 10 or D shr 22 + A;
  Inc(C, Buffer[10] + $FFEFF47D + (A xor (D or not B))); C := C shl 15 or C shr 17 + D;
  Inc(B, Buffer[ 1] + $85845DD1 + (D xor (C or not A))); B := B shl 21 or B shr 11 + C;
  Inc(A, Buffer[ 8] + $6FA87E4F + (C xor (B or not D))); A := A shl  6 or A shr 26 + B;
  Inc(D, Buffer[15] + $FE2CE6E0 + (B xor (A or not C))); D := D shl 10 or D shr 22 + A;
  Inc(C, Buffer[ 6] + $A3014314 + (A xor (D or not B))); C := C shl 15 or C shr 17 + D;
  Inc(B, Buffer[13] + $4E0811A1 + (D xor (C or not A))); B := B shl 21 or B shr 11 + C;
  Inc(A, Buffer[ 4] + $F7537E82 + (C xor (B or not D))); A := A shl  6 or A shr 26 + B;
  Inc(D, Buffer[11] + $BD3AF235 + (B xor (A or not C))); D := D shl 10 or D shr 22 + A;
  Inc(C, Buffer[ 2] + $2AD7D2BB + (A xor (D or not B))); C := C shl 15 or C shr 17 + D;
  Inc(B, Buffer[ 9] + $EB86D391 + (D xor (C or not A))); B := B shl 21 or B shr 11 + C;

  Inc(FDigest[0], A);
  Inc(FDigest[1], B);
  Inc(FDigest[2], C);
  Inc(FDigest[3], D);
end;
{$ENDIF}

{ THash_RipeMD128 }

{$IFNDEF X86ASM}
const
  RipeS1 = $5A827999;
  RipeS2 = $6ED9EBA1;
  RipeS3 = $8F1BBCDC;
  RipeS4 = $A953FD4E;
  RipeS5 = $50A28BE6;
  RipeS6 = $5C4DD124;
  RipeS7 = $6D703EF3;
  RipeS8 = $7A6D76E9;
{$ENDIF !X86ASM}

{$IFNDEF THash_RipeMD128_asm}
procedure THash_RipeMD128.DoTransform(Buffer: PUInt32Array);
var
  A1, B1, C1, D1: UInt32;
  A2, B2, C2, D2: UInt32;
  T: UInt32;
begin
  A1 := FDigest[0];
  B1 := FDigest[1];
  C1 := FDigest[2];
  D1 := FDigest[3];
  A2 := FDigest[0];
  B2 := FDigest[1];
  C2 := FDigest[2];
  D2 := FDigest[3];

  Inc(A1, B1 xor C1 xor D1 + Buffer[ 0]); A1 := A1 shl 11 or A1 shr 21;
  Inc(D1, A1 xor B1 xor C1 + Buffer[ 1]); D1 := D1 shl 14 or D1 shr 18;
  Inc(C1, D1 xor A1 xor B1 + Buffer[ 2]); C1 := C1 shl 15 or C1 shr 17;
  Inc(B1, C1 xor D1 xor A1 + Buffer[ 3]); B1 := B1 shl 12 or B1 shr 20;
  Inc(A1, B1 xor C1 xor D1 + Buffer[ 4]); A1 := A1 shl  5 or A1 shr 27;
  Inc(D1, A1 xor B1 xor C1 + Buffer[ 5]); D1 := D1 shl  8 or D1 shr 24;
  Inc(C1, D1 xor A1 xor B1 + Buffer[ 6]); C1 := C1 shl  7 or C1 shr 25;
  Inc(B1, C1 xor D1 xor A1 + Buffer[ 7]); B1 := B1 shl  9 or B1 shr 23;
  Inc(A1, B1 xor C1 xor D1 + Buffer[ 8]); A1 := A1 shl 11 or A1 shr 21;
  Inc(D1, A1 xor B1 xor C1 + Buffer[ 9]); D1 := D1 shl 13 or D1 shr 19;
  Inc(C1, D1 xor A1 xor B1 + Buffer[10]); C1 := C1 shl 14 or C1 shr 18;
  Inc(B1, C1 xor D1 xor A1 + Buffer[11]); B1 := B1 shl 15 or B1 shr 17;
  Inc(A1, B1 xor C1 xor D1 + Buffer[12]); A1 := A1 shl  6 or A1 shr 26;
  Inc(D1, A1 xor B1 xor C1 + Buffer[13]); D1 := D1 shl  7 or D1 shr 25;
  Inc(C1, D1 xor A1 xor B1 + Buffer[14]); C1 := C1 shl  9 or C1 shr 23;
  Inc(B1, C1 xor D1 xor A1 + Buffer[15]); B1 := B1 shl  8 or B1 shr 24;

  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[ 7] + RipeS1); A1 := A1 shl  7 or A1 shr 25;
  Inc(D1, A1 and B1 or not A1 and C1 + Buffer[ 4] + RipeS1); D1 := D1 shl  6 or D1 shr 26;
  Inc(C1, D1 and A1 or not D1 and B1 + Buffer[13] + RipeS1); C1 := C1 shl  8 or C1 shr 24;
  Inc(B1, C1 and D1 or not C1 and A1 + Buffer[ 1] + RipeS1); B1 := B1 shl 13 or B1 shr 19;
  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[10] + RipeS1); A1 := A1 shl 11 or A1 shr 21;
  Inc(D1, A1 and B1 or not A1 and C1 + Buffer[ 6] + RipeS1); D1 := D1 shl  9 or D1 shr 23;
  Inc(C1, D1 and A1 or not D1 and B1 + Buffer[15] + RipeS1); C1 := C1 shl  7 or C1 shr 25;
  Inc(B1, C1 and D1 or not C1 and A1 + Buffer[ 3] + RipeS1); B1 := B1 shl 15 or B1 shr 17;
  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[12] + RipeS1); A1 := A1 shl  7 or A1 shr 25;
  Inc(D1, A1 and B1 or not A1 and C1 + Buffer[ 0] + RipeS1); D1 := D1 shl 12 or D1 shr 20;
  Inc(C1, D1 and A1 or not D1 and B1 + Buffer[ 9] + RipeS1); C1 := C1 shl 15 or C1 shr 17;
  Inc(B1, C1 and D1 or not C1 and A1 + Buffer[ 5] + RipeS1); B1 := B1 shl  9 or B1 shr 23;
  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[ 2] + RipeS1); A1 := A1 shl 11 or A1 shr 21;
  Inc(D1, A1 and B1 or not A1 and C1 + Buffer[14] + RipeS1); D1 := D1 shl  7 or D1 shr 25;
  Inc(C1, D1 and A1 or not D1 and B1 + Buffer[11] + RipeS1); C1 := C1 shl 13 or C1 shr 19;
  Inc(B1, C1 and D1 or not C1 and A1 + Buffer[ 8] + RipeS1); B1 := B1 shl 12 or B1 shr 20;

  Inc(A1, B1 or not C1 xor D1 + Buffer[ 3] + RipeS2); A1 := A1 shl 11 or A1 shr 21;
  Inc(D1, A1 or not B1 xor C1 + Buffer[10] + RipeS2); D1 := D1 shl 13 or D1 shr 19;
  Inc(C1, D1 or not A1 xor B1 + Buffer[14] + RipeS2); C1 := C1 shl  6 or C1 shr 26;
  Inc(B1, C1 or not D1 xor A1 + Buffer[ 4] + RipeS2); B1 := B1 shl  7 or B1 shr 25;
  Inc(A1, B1 or not C1 xor D1 + Buffer[ 9] + RipeS2); A1 := A1 shl 14 or A1 shr 18;
  Inc(D1, A1 or not B1 xor C1 + Buffer[15] + RipeS2); D1 := D1 shl  9 or D1 shr 23;
  Inc(C1, D1 or not A1 xor B1 + Buffer[ 8] + RipeS2); C1 := C1 shl 13 or C1 shr 19;
  Inc(B1, C1 or not D1 xor A1 + Buffer[ 1] + RipeS2); B1 := B1 shl 15 or B1 shr 17;
  Inc(A1, B1 or not C1 xor D1 + Buffer[ 2] + RipeS2); A1 := A1 shl 14 or A1 shr 18;
  Inc(D1, A1 or not B1 xor C1 + Buffer[ 7] + RipeS2); D1 := D1 shl  8 or D1 shr 24;
  Inc(C1, D1 or not A1 xor B1 + Buffer[ 0] + RipeS2); C1 := C1 shl 13 or C1 shr 19;
  Inc(B1, C1 or not D1 xor A1 + Buffer[ 6] + RipeS2); B1 := B1 shl  6 or B1 shr 26;
  Inc(A1, B1 or not C1 xor D1 + Buffer[13] + RipeS2); A1 := A1 shl  5 or A1 shr 27;
  Inc(D1, A1 or not B1 xor C1 + Buffer[11] + RipeS2); D1 := D1 shl 12 or D1 shr 20;
  Inc(C1, D1 or not A1 xor B1 + Buffer[ 5] + RipeS2); C1 := C1 shl  7 or C1 shr 25;
  Inc(B1, C1 or not D1 xor A1 + Buffer[12] + RipeS2); B1 := B1 shl  5 or B1 shr 27;

  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[ 1] + RipeS3); A1 := A1 shl 11 or A1 shr 21;
  Inc(D1, A1 and C1 or B1 and not C1 + Buffer[ 9] + RipeS3); D1 := D1 shl 12 or D1 shr 20;
  Inc(C1, D1 and B1 or A1 and not B1 + Buffer[11] + RipeS3); C1 := C1 shl 14 or C1 shr 18;
  Inc(B1, C1 and A1 or D1 and not A1 + Buffer[10] + RipeS3); B1 := B1 shl 15 or B1 shr 17;
  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[ 0] + RipeS3); A1 := A1 shl 14 or A1 shr 18;
  Inc(D1, A1 and C1 or B1 and not C1 + Buffer[ 8] + RipeS3); D1 := D1 shl 15 or D1 shr 17;
  Inc(C1, D1 and B1 or A1 and not B1 + Buffer[12] + RipeS3); C1 := C1 shl  9 or C1 shr 23;
  Inc(B1, C1 and A1 or D1 and not A1 + Buffer[ 4] + RipeS3); B1 := B1 shl  8 or B1 shr 24;
  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[13] + RipeS3); A1 := A1 shl  9 or A1 shr 23;
  Inc(D1, A1 and C1 or B1 and not C1 + Buffer[ 3] + RipeS3); D1 := D1 shl 14 or D1 shr 18;
  Inc(C1, D1 and B1 or A1 and not B1 + Buffer[ 7] + RipeS3); C1 := C1 shl  5 or C1 shr 27;
  Inc(B1, C1 and A1 or D1 and not A1 + Buffer[15] + RipeS3); B1 := B1 shl  6 or B1 shr 26;
  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[14] + RipeS3); A1 := A1 shl  8 or A1 shr 24;
  Inc(D1, A1 and C1 or B1 and not C1 + Buffer[ 5] + RipeS3); D1 := D1 shl  6 or D1 shr 26;
  Inc(C1, D1 and B1 or A1 and not B1 + Buffer[ 6] + RipeS3); C1 := C1 shl  5 or C1 shr 27;
  Inc(B1, C1 and A1 or D1 and not A1 + Buffer[ 2] + RipeS3); B1 := B1 shl 12 or B1 shr 20;

  T := A1; A1 := A2; A2 := T;
  T := B1; B1 := B2; B2 := T;
  T := C1; C1 := C2; C2 := T;
  T := D1; D1 := D2; D2 := T;

  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[ 5] + RipeS5); A1 := A1 shl  8 or A1 shr 24;
  Inc(D1, A1 and C1 or B1 and not C1 + Buffer[14] + RipeS5); D1 := D1 shl  9 or D1 shr 23;
  Inc(C1, D1 and B1 or A1 and not B1 + Buffer[ 7] + RipeS5); C1 := C1 shl  9 or C1 shr 23;
  Inc(B1, C1 and A1 or D1 and not A1 + Buffer[ 0] + RipeS5); B1 := B1 shl 11 or B1 shr 21;
  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[ 9] + RipeS5); A1 := A1 shl 13 or A1 shr 19;
  Inc(D1, A1 and C1 or B1 and not C1 + Buffer[ 2] + RipeS5); D1 := D1 shl 15 or D1 shr 17;
  Inc(C1, D1 and B1 or A1 and not B1 + Buffer[11] + RipeS5); C1 := C1 shl 15 or C1 shr 17;
  Inc(B1, C1 and A1 or D1 and not A1 + Buffer[ 4] + RipeS5); B1 := B1 shl  5 or B1 shr 27;
  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[13] + RipeS5); A1 := A1 shl  7 or A1 shr 25;
  Inc(D1, A1 and C1 or B1 and not C1 + Buffer[ 6] + RipeS5); D1 := D1 shl  7 or D1 shr 25;
  Inc(C1, D1 and B1 or A1 and not B1 + Buffer[15] + RipeS5); C1 := C1 shl  8 or C1 shr 24;
  Inc(B1, C1 and A1 or D1 and not A1 + Buffer[ 8] + RipeS5); B1 := B1 shl 11 or B1 shr 21;
  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[ 1] + RipeS5); A1 := A1 shl 14 or A1 shr 18;
  Inc(D1, A1 and C1 or B1 and not C1 + Buffer[10] + RipeS5); D1 := D1 shl 14 or D1 shr 18;
  Inc(C1, D1 and B1 or A1 and not B1 + Buffer[ 3] + RipeS5); C1 := C1 shl 12 or C1 shr 20;
  Inc(B1, C1 and A1 or D1 and not A1 + Buffer[12] + RipeS5); B1 := B1 shl  6 or B1 shr 26;

  Inc(A1, B1 or not C1 xor D1 + Buffer[ 6] + RipeS6); A1 := A1 shl  9 or A1 shr 23;
  Inc(D1, A1 or not B1 xor C1 + Buffer[11] + RipeS6); D1 := D1 shl 13 or D1 shr 19;
  Inc(C1, D1 or not A1 xor B1 + Buffer[ 3] + RipeS6); C1 := C1 shl 15 or C1 shr 17;
  Inc(B1, C1 or not D1 xor A1 + Buffer[ 7] + RipeS6); B1 := B1 shl  7 or B1 shr 25;
  Inc(A1, B1 or not C1 xor D1 + Buffer[ 0] + RipeS6); A1 := A1 shl 12 or A1 shr 20;
  Inc(D1, A1 or not B1 xor C1 + Buffer[13] + RipeS6); D1 := D1 shl  8 or D1 shr 24;
  Inc(C1, D1 or not A1 xor B1 + Buffer[ 5] + RipeS6); C1 := C1 shl  9 or C1 shr 23;
  Inc(B1, C1 or not D1 xor A1 + Buffer[10] + RipeS6); B1 := B1 shl 11 or B1 shr 21;
  Inc(A1, B1 or not C1 xor D1 + Buffer[14] + RipeS6); A1 := A1 shl  7 or A1 shr 25;
  Inc(D1, A1 or not B1 xor C1 + Buffer[15] + RipeS6); D1 := D1 shl  7 or D1 shr 25;
  Inc(C1, D1 or not A1 xor B1 + Buffer[ 8] + RipeS6); C1 := C1 shl 12 or C1 shr 20;
  Inc(B1, C1 or not D1 xor A1 + Buffer[12] + RipeS6); B1 := B1 shl  7 or B1 shr 25;
  Inc(A1, B1 or not C1 xor D1 + Buffer[ 4] + RipeS6); A1 := A1 shl  6 or A1 shr 26;
  Inc(D1, A1 or not B1 xor C1 + Buffer[ 9] + RipeS6); D1 := D1 shl 15 or D1 shr 17;
  Inc(C1, D1 or not A1 xor B1 + Buffer[ 1] + RipeS6); C1 := C1 shl 13 or C1 shr 19;
  Inc(B1, C1 or not D1 xor A1 + Buffer[ 2] + RipeS6); B1 := B1 shl 11 or B1 shr 21;

  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[15] + RipeS7); A1 := A1 shl  9 or A1 shr 23;
  Inc(D1, A1 and B1 or not A1 and C1 + Buffer[ 5] + RipeS7); D1 := D1 shl  7 or D1 shr 25;
  Inc(C1, D1 and A1 or not D1 and B1 + Buffer[ 1] + RipeS7); C1 := C1 shl 15 or C1 shr 17;
  Inc(B1, C1 and D1 or not C1 and A1 + Buffer[ 3] + RipeS7); B1 := B1 shl 11 or B1 shr 21;
  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[ 7] + RipeS7); A1 := A1 shl  8 or A1 shr 24;
  Inc(D1, A1 and B1 or not A1 and C1 + Buffer[14] + RipeS7); D1 := D1 shl  6 or D1 shr 26;
  Inc(C1, D1 and A1 or not D1 and B1 + Buffer[ 6] + RipeS7); C1 := C1 shl  6 or C1 shr 26;
  Inc(B1, C1 and D1 or not C1 and A1 + Buffer[ 9] + RipeS7); B1 := B1 shl 14 or B1 shr 18;
  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[11] + RipeS7); A1 := A1 shl 12 or A1 shr 20;
  Inc(D1, A1 and B1 or not A1 and C1 + Buffer[ 8] + RipeS7); D1 := D1 shl 13 or D1 shr 19;
  Inc(C1, D1 and A1 or not D1 and B1 + Buffer[12] + RipeS7); C1 := C1 shl  5 or C1 shr 27;
  Inc(B1, C1 and D1 or not C1 and A1 + Buffer[ 2] + RipeS7); B1 := B1 shl 14 or B1 shr 18;
  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[10] + RipeS7); A1 := A1 shl 13 or A1 shr 19;
  Inc(D1, A1 and B1 or not A1 and C1 + Buffer[ 0] + RipeS7); D1 := D1 shl 13 or D1 shr 19;
  Inc(C1, D1 and A1 or not D1 and B1 + Buffer[ 4] + RipeS7); C1 := C1 shl  7 or C1 shr 25;
  Inc(B1, C1 and D1 or not C1 and A1 + Buffer[13] + RipeS7); B1 := B1 shl  5 or B1 shr 27;

  Inc(A1, B1 xor C1 xor D1 + Buffer[ 8]); A1 := A1 shl 15 or A1 shr 17;
  Inc(D1, A1 xor B1 xor C1 + Buffer[ 6]); D1 := D1 shl  5 or D1 shr 27;
  Inc(C1, D1 xor A1 xor B1 + Buffer[ 4]); C1 := C1 shl  8 or C1 shr 24;
  Inc(B1, C1 xor D1 xor A1 + Buffer[ 1]); B1 := B1 shl 11 or B1 shr 21;
  Inc(A1, B1 xor C1 xor D1 + Buffer[ 3]); A1 := A1 shl 14 or A1 shr 18;
  Inc(D1, A1 xor B1 xor C1 + Buffer[11]); D1 := D1 shl 14 or D1 shr 18;
  Inc(C1, D1 xor A1 xor B1 + Buffer[15]); C1 := C1 shl  6 or C1 shr 26;
  Inc(B1, C1 xor D1 xor A1 + Buffer[ 0]); B1 := B1 shl 14 or B1 shr 18;
  Inc(A1, B1 xor C1 xor D1 + Buffer[ 5]); A1 := A1 shl  6 or A1 shr 26;
  Inc(D1, A1 xor B1 xor C1 + Buffer[12]); D1 := D1 shl  9 or D1 shr 23;
  Inc(C1, D1 xor A1 xor B1 + Buffer[ 2]); C1 := C1 shl 12 or C1 shr 20;
  Inc(B1, C1 xor D1 xor A1 + Buffer[13]); B1 := B1 shl  9 or B1 shr 23;
  Inc(A1, B1 xor C1 xor D1 + Buffer[ 9]); A1 := A1 shl 12 or A1 shr 20;
  Inc(D1, A1 xor B1 xor C1 + Buffer[ 7]); D1 := D1 shl  5 or D1 shr 27;
  Inc(C1, D1 xor A1 xor B1 + Buffer[10]); C1 := C1 shl 15 or C1 shr 17;
  Inc(B1, C1 xor D1 xor A1 + Buffer[14]); B1 := B1 shl  8 or B1 shr 24;

  Inc(D1, C2 + FDigest[1]);
  FDigest[1] := FDigest[2] + D2 + A1;
  FDigest[2] := FDigest[3] + A2 + B1;
  FDigest[3] := FDIgest[0] + B2 + C1;
  FDigest[0] := D1;
end;
{$ENDIF !THash_RipeMD128_asm}

{ THash_RipeMD160 }

{$IFNDEF THash_RipeMD160_asm}
procedure THash_RipeMD160.DoTransform(Buffer: PUInt32Array);
var
  A1, B1, C1, D1, E1: UInt32;
  A2, B2, C2, D2, E2: UInt32;
  T: UInt32;
begin
  A1 := FDigest[0];
  B1 := FDigest[1];
  C1 := FDigest[2];
  D1 := FDigest[3];
  E1 := FDigest[4];

  A2 := FDigest[0];
  B2 := FDigest[1];
  C2 := FDigest[2];
  D2 := FDigest[3];
  E2 := FDigest[4];

  Inc(A1, B1 xor C1 xor D1 + Buffer[ 0]); A1 := A1 shl 11 or A1 shr 21 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 xor B1 xor C1 + Buffer[ 1]); E1 := E1 shl 14 or E1 shr 18 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 xor A1 xor B1 + Buffer[ 2]); D1 := D1 shl 15 or D1 shr 17 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 xor E1 xor A1 + Buffer[ 3]); C1 := C1 shl 12 or C1 shr 20 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 xor D1 xor E1 + Buffer[ 4]); B1 := B1 shl  5 or B1 shr 27 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 xor C1 xor D1 + Buffer[ 5]); A1 := A1 shl  8 or A1 shr 24 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 xor B1 xor C1 + Buffer[ 6]); E1 := E1 shl  7 or E1 shr 25 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 xor A1 xor B1 + Buffer[ 7]); D1 := D1 shl  9 or D1 shr 23 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 xor E1 xor A1 + Buffer[ 8]); C1 := C1 shl 11 or C1 shr 21 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 xor D1 xor E1 + Buffer[ 9]); B1 := B1 shl 13 or B1 shr 19 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 xor C1 xor D1 + Buffer[10]); A1 := A1 shl 14 or A1 shr 18 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 xor B1 xor C1 + Buffer[11]); E1 := E1 shl 15 or E1 shr 17 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 xor A1 xor B1 + Buffer[12]); D1 := D1 shl  6 or D1 shr 26 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 xor E1 xor A1 + Buffer[13]); C1 := C1 shl  7 or C1 shr 25 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 xor D1 xor E1 + Buffer[14]); B1 := B1 shl  9 or B1 shr 23 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 xor C1 xor D1 + Buffer[15]); A1 := A1 shl  8 or A1 shr 24 + E1; C1 := C1 shl 10 or C1 shr 22;

  Inc(E1, A1 and B1 or not A1 and C1 + Buffer[ 7] + RipeS1); E1 := E1 shl  7 or E1 shr 25 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 and A1 or not E1 and B1 + Buffer[ 4] + RipeS1); D1 := D1 shl  6 or D1 shr 26 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 and E1 or not D1 and A1 + Buffer[13] + RipeS1); C1 := C1 shl  8 or C1 shr 24 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 and D1 or not C1 and E1 + Buffer[ 1] + RipeS1); B1 := B1 shl 13 or B1 shr 19 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[10] + RipeS1); A1 := A1 shl 11 or A1 shr 21 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 and B1 or not A1 and C1 + Buffer[ 6] + RipeS1); E1 := E1 shl  9 or E1 shr 23 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 and A1 or not E1 and B1 + Buffer[15] + RipeS1); D1 := D1 shl  7 or D1 shr 25 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 and E1 or not D1 and A1 + Buffer[ 3] + RipeS1); C1 := C1 shl 15 or C1 shr 17 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 and D1 or not C1 and E1 + Buffer[12] + RipeS1); B1 := B1 shl  7 or B1 shr 25 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[ 0] + RipeS1); A1 := A1 shl 12 or A1 shr 20 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 and B1 or not A1 and C1 + Buffer[ 9] + RipeS1); E1 := E1 shl 15 or E1 shr 17 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 and A1 or not E1 and B1 + Buffer[ 5] + RipeS1); D1 := D1 shl  9 or D1 shr 23 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 and E1 or not D1 and A1 + Buffer[ 2] + RipeS1); C1 := C1 shl 11 or C1 shr 21 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 and D1 or not C1 and E1 + Buffer[14] + RipeS1); B1 := B1 shl  7 or B1 shr 25 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[11] + RipeS1); A1 := A1 shl 13 or A1 shr 19 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 and B1 or not A1 and C1 + Buffer[ 8] + RipeS1); E1 := E1 shl 12 or E1 shr 20 + D1; B1 := B1 shl 10 or B1 shr 22;

  Inc(D1, E1 or not A1 xor B1 + Buffer[ 3] + RipeS2); D1 := D1 shl 11 or D1 shr 21 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 or not E1 xor A1 + Buffer[10] + RipeS2); C1 := C1 shl 13 or C1 shr 19 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 or not D1 xor E1 + Buffer[14] + RipeS2); B1 := B1 shl  6 or B1 shr 26 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 or not C1 xor D1 + Buffer[ 4] + RipeS2); A1 := A1 shl  7 or A1 shr 25 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 or not B1 xor C1 + Buffer[ 9] + RipeS2); E1 := E1 shl 14 or E1 shr 18 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 or not A1 xor B1 + Buffer[15] + RipeS2); D1 := D1 shl  9 or D1 shr 23 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 or not E1 xor A1 + Buffer[ 8] + RipeS2); C1 := C1 shl 13 or C1 shr 19 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 or not D1 xor E1 + Buffer[ 1] + RipeS2); B1 := B1 shl 15 or B1 shr 17 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 or not C1 xor D1 + Buffer[ 2] + RipeS2); A1 := A1 shl 14 or A1 shr 18 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 or not B1 xor C1 + Buffer[ 7] + RipeS2); E1 := E1 shl  8 or E1 shr 24 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 or not A1 xor B1 + Buffer[ 0] + RipeS2); D1 := D1 shl 13 or D1 shr 19 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 or not E1 xor A1 + Buffer[ 6] + RipeS2); C1 := C1 shl  6 or C1 shr 26 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 or not D1 xor E1 + Buffer[13] + RipeS2); B1 := B1 shl  5 or B1 shr 27 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 or not C1 xor D1 + Buffer[11] + RipeS2); A1 := A1 shl 12 or A1 shr 20 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 or not B1 xor C1 + Buffer[ 5] + RipeS2); E1 := E1 shl  7 or E1 shr 25 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 or not A1 xor B1 + Buffer[12] + RipeS2); D1 := D1 shl  5 or D1 shr 27 + C1; A1 := A1 shl 10 or A1 shr 22;

  Inc(C1, D1 and A1 or E1 and not A1 + Buffer[ 1] + RipeS3); C1 := C1 shl 11 or C1 shr 21 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 and E1 or D1 and not E1 + Buffer[ 9] + RipeS3); B1 := B1 shl 12 or B1 shr 20 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[11] + RipeS3); A1 := A1 shl 14 or A1 shr 18 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 and C1 or B1 and not C1 + Buffer[10] + RipeS3); E1 := E1 shl 15 or E1 shr 17 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 and B1 or A1 and not B1 + Buffer[ 0] + RipeS3); D1 := D1 shl 14 or D1 shr 18 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 and A1 or E1 and not A1 + Buffer[ 8] + RipeS3); C1 := C1 shl 15 or C1 shr 17 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 and E1 or D1 and not E1 + Buffer[12] + RipeS3); B1 := B1 shl  9 or B1 shr 23 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[ 4] + RipeS3); A1 := A1 shl  8 or A1 shr 24 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 and C1 or B1 and not C1 + Buffer[13] + RipeS3); E1 := E1 shl  9 or E1 shr 23 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 and B1 or A1 and not B1 + Buffer[ 3] + RipeS3); D1 := D1 shl 14 or D1 shr 18 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 and A1 or E1 and not A1 + Buffer[ 7] + RipeS3); C1 := C1 shl  5 or C1 shr 27 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 and E1 or D1 and not E1 + Buffer[15] + RipeS3); B1 := B1 shl  6 or B1 shr 26 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[14] + RipeS3); A1 := A1 shl  8 or A1 shr 24 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 and C1 or B1 and not C1 + Buffer[ 5] + RipeS3); E1 := E1 shl  6 or E1 shr 26 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 and B1 or A1 and not B1 + Buffer[ 6] + RipeS3); D1 := D1 shl  5 or D1 shr 27 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 and A1 or E1 and not A1 + Buffer[ 2] + RipeS3); C1 := C1 shl 12 or C1 shr 20 + B1; E1 := E1 shl 10 or E1 shr 22;

  Inc(B1, D1 or not E1 xor C1 + Buffer[ 4] + RipeS4); B1 := B1 shl  9 or B1 shr 23 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, C1 or not D1 xor B1 + Buffer[ 0] + RipeS4); A1 := A1 shl 15 or A1 shr 17 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, B1 or not C1 xor A1 + Buffer[ 5] + RipeS4); E1 := E1 shl  5 or E1 shr 27 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, A1 or not B1 xor E1 + Buffer[ 9] + RipeS4); D1 := D1 shl 11 or D1 shr 21 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, E1 or not A1 xor D1 + Buffer[ 7] + RipeS4); C1 := C1 shl  6 or C1 shr 26 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, D1 or not E1 xor C1 + Buffer[12] + RipeS4); B1 := B1 shl  8 or B1 shr 24 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, C1 or not D1 xor B1 + Buffer[ 2] + RipeS4); A1 := A1 shl 13 or A1 shr 19 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, B1 or not C1 xor A1 + Buffer[10] + RipeS4); E1 := E1 shl 12 or E1 shr 20 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, A1 or not B1 xor E1 + Buffer[14] + RipeS4); D1 := D1 shl  5 or D1 shr 27 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, E1 or not A1 xor D1 + Buffer[ 1] + RipeS4); C1 := C1 shl 12 or C1 shr 20 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, D1 or not E1 xor C1 + Buffer[ 3] + RipeS4); B1 := B1 shl 13 or B1 shr 19 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, C1 or not D1 xor B1 + Buffer[ 8] + RipeS4); A1 := A1 shl 14 or A1 shr 18 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, B1 or not C1 xor A1 + Buffer[11] + RipeS4); E1 := E1 shl 11 or E1 shr 21 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, A1 or not B1 xor E1 + Buffer[ 6] + RipeS4); D1 := D1 shl  8 or D1 shr 24 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, E1 or not A1 xor D1 + Buffer[15] + RipeS4); C1 := C1 shl  5 or C1 shr 27 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, D1 or not E1 xor C1 + Buffer[13] + RipeS4); B1 := B1 shl  6 or B1 shr 26 + A1; D1 := D1 shl 10 or D1 shr 22;

  T := A1; A1 := A2; A2 := T;
  T := B1; B1 := B2; B2 := T;
  T := C1; C1 := C2; C2 := T;
  T := D1; D1 := D2; D2 := T;
  T := E1; E1 := E2; E2 := T;

  Inc(A1, C1 or not D1 xor B1 + Buffer[ 5] + RipeS5); A1 := A1 shl  8 or A1 shr 24 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, B1 or not C1 xor A1 + Buffer[14] + RipeS5); E1 := E1 shl  9 or E1 shr 23 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, A1 or not B1 xor E1 + Buffer[ 7] + RipeS5); D1 := D1 shl  9 or D1 shr 23 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, E1 or not A1 xor D1 + Buffer[ 0] + RipeS5); C1 := C1 shl 11 or C1 shr 21 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, D1 or not E1 xor C1 + Buffer[ 9] + RipeS5); B1 := B1 shl 13 or B1 shr 19 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, C1 or not D1 xor B1 + Buffer[ 2] + RipeS5); A1 := A1 shl 15 or A1 shr 17 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, B1 or not C1 xor A1 + Buffer[11] + RipeS5); E1 := E1 shl 15 or E1 shr 17 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, A1 or not B1 xor E1 + Buffer[ 4] + RipeS5); D1 := D1 shl  5 or D1 shr 27 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, E1 or not A1 xor D1 + Buffer[13] + RipeS5); C1 := C1 shl  7 or C1 shr 25 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, D1 or not E1 xor C1 + Buffer[ 6] + RipeS5); B1 := B1 shl  7 or B1 shr 25 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, C1 or not D1 xor B1 + Buffer[15] + RipeS5); A1 := A1 shl  8 or A1 shr 24 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, B1 or not C1 xor A1 + Buffer[ 8] + RipeS5); E1 := E1 shl 11 or E1 shr 21 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, A1 or not B1 xor E1 + Buffer[ 1] + RipeS5); D1 := D1 shl 14 or D1 shr 18 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, E1 or not A1 xor D1 + Buffer[10] + RipeS5); C1 := C1 shl 14 or C1 shr 18 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, D1 or not E1 xor C1 + Buffer[ 3] + RipeS5); B1 := B1 shl 12 or B1 shr 20 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, C1 or not D1 xor B1 + Buffer[12] + RipeS5); A1 := A1 shl  6 or A1 shr 26 + E1; C1 := C1 shl 10 or C1 shr 22;

  Inc(E1, A1 and C1 or B1 and not C1 + Buffer[ 6] + RipeS6); E1 := E1 shl  9 or E1 shr 23 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 and B1 or A1 and not B1 + Buffer[11] + RipeS6); D1 := D1 shl 13 or D1 shr 19 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 and A1 or E1 and not A1 + Buffer[ 3] + RipeS6); C1 := C1 shl 15 or C1 shr 17 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 and E1 or D1 and not E1 + Buffer[ 7] + RipeS6); B1 := B1 shl  7 or B1 shr 25 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[ 0] + RipeS6); A1 := A1 shl 12 or A1 shr 20 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 and C1 or B1 and not C1 + Buffer[13] + RipeS6); E1 := E1 shl  8 or E1 shr 24 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 and B1 or A1 and not B1 + Buffer[ 5] + RipeS6); D1 := D1 shl  9 or D1 shr 23 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 and A1 or E1 and not A1 + Buffer[10] + RipeS6); C1 := C1 shl 11 or C1 shr 21 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 and E1 or D1 and not E1 + Buffer[14] + RipeS6); B1 := B1 shl  7 or B1 shr 25 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[15] + RipeS6); A1 := A1 shl  7 or A1 shr 25 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 and C1 or B1 and not C1 + Buffer[ 8] + RipeS6); E1 := E1 shl 12 or E1 shr 20 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 and B1 or A1 and not B1 + Buffer[12] + RipeS6); D1 := D1 shl  7 or D1 shr 25 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 and A1 or E1 and not A1 + Buffer[ 4] + RipeS6); C1 := C1 shl  6 or C1 shr 26 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 and E1 or D1 and not E1 + Buffer[ 9] + RipeS6); B1 := B1 shl 15 or B1 shr 17 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[ 1] + RipeS6); A1 := A1 shl 13 or A1 shr 19 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 and C1 or B1 and not C1 + Buffer[ 2] + RipeS6); E1 := E1 shl 11 or E1 shr 21 + D1; B1 := B1 shl 10 or B1 shr 22;

  Inc(D1, E1 or not A1 xor B1 + Buffer[15] + RipeS7); D1 := D1 shl  9 or D1 shr 23 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 or not E1 xor A1 + Buffer[ 5] + RipeS7); C1 := C1 shl  7 or C1 shr 25 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 or not D1 xor E1 + Buffer[ 1] + RipeS7); B1 := B1 shl 15 or B1 shr 17 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 or not C1 xor D1 + Buffer[ 3] + RipeS7); A1 := A1 shl 11 or A1 shr 21 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 or not B1 xor C1 + Buffer[ 7] + RipeS7); E1 := E1 shl  8 or E1 shr 24 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 or not A1 xor B1 + Buffer[14] + RipeS7); D1 := D1 shl  6 or D1 shr 26 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 or not E1 xor A1 + Buffer[ 6] + RipeS7); C1 := C1 shl  6 or C1 shr 26 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 or not D1 xor E1 + Buffer[ 9] + RipeS7); B1 := B1 shl 14 or B1 shr 18 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 or not C1 xor D1 + Buffer[11] + RipeS7); A1 := A1 shl 12 or A1 shr 20 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 or not B1 xor C1 + Buffer[ 8] + RipeS7); E1 := E1 shl 13 or E1 shr 19 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 or not A1 xor B1 + Buffer[12] + RipeS7); D1 := D1 shl  5 or D1 shr 27 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 or not E1 xor A1 + Buffer[ 2] + RipeS7); C1 := C1 shl 14 or C1 shr 18 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 or not D1 xor E1 + Buffer[10] + RipeS7); B1 := B1 shl 13 or B1 shr 19 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 or not C1 xor D1 + Buffer[ 0] + RipeS7); A1 := A1 shl 13 or A1 shr 19 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 or not B1 xor C1 + Buffer[ 4] + RipeS7); E1 := E1 shl  7 or E1 shr 25 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 or not A1 xor B1 + Buffer[13] + RipeS7); D1 := D1 shl  5 or D1 shr 27 + C1; A1 := A1 shl 10 or A1 shr 22;

  Inc(C1, D1 and E1 or not D1 and A1 + Buffer[ 8] + RipeS8); C1 := C1 shl 15 or C1 shr 17 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 and D1 or not C1 and E1 + Buffer[ 6] + RipeS8); B1 := B1 shl  5 or B1 shr 27 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[ 4] + RipeS8); A1 := A1 shl  8 or A1 shr 24 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 and B1 or not A1 and C1 + Buffer[ 1] + RipeS8); E1 := E1 shl 11 or E1 shr 21 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 and A1 or not E1 and B1 + Buffer[ 3] + RipeS8); D1 := D1 shl 14 or D1 shr 18 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 and E1 or not D1 and A1 + Buffer[11] + RipeS8); C1 := C1 shl 14 or C1 shr 18 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 and D1 or not C1 and E1 + Buffer[15] + RipeS8); B1 := B1 shl  6 or B1 shr 26 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[ 0] + RipeS8); A1 := A1 shl 14 or A1 shr 18 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 and B1 or not A1 and C1 + Buffer[ 5] + RipeS8); E1 := E1 shl  6 or E1 shr 26 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 and A1 or not E1 and B1 + Buffer[12] + RipeS8); D1 := D1 shl  9 or D1 shr 23 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 and E1 or not D1 and A1 + Buffer[ 2] + RipeS8); C1 := C1 shl 12 or C1 shr 20 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 and D1 or not C1 and E1 + Buffer[13] + RipeS8); B1 := B1 shl  9 or B1 shr 23 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[ 9] + RipeS8); A1 := A1 shl 12 or A1 shr 20 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 and B1 or not A1 and C1 + Buffer[ 7] + RipeS8); E1 := E1 shl  5 or E1 shr 27 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 and A1 or not E1 and B1 + Buffer[10] + RipeS8); D1 := D1 shl 15 or D1 shr 17 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 and E1 or not D1 and A1 + Buffer[14] + RipeS8); C1 := C1 shl  8 or C1 shr 24 + B1; E1 := E1 shl 10 or E1 shr 22;

  Inc(B1, C1 xor D1 xor E1 + Buffer[12]); B1 := B1 shl  8 or B1 shr 24 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 xor C1 xor D1 + Buffer[15]); A1 := A1 shl  5 or A1 shr 27 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 xor B1 xor C1 + Buffer[10]); E1 := E1 shl 12 or E1 shr 20 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 xor A1 xor B1 + Buffer[ 4]); D1 := D1 shl  9 or D1 shr 23 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 xor E1 xor A1 + Buffer[ 1]); C1 := C1 shl 12 or C1 shr 20 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 xor D1 xor E1 + Buffer[ 5]); B1 := B1 shl  5 or B1 shr 27 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 xor C1 xor D1 + Buffer[ 8]); A1 := A1 shl 14 or A1 shr 18 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 xor B1 xor C1 + Buffer[ 7]); E1 := E1 shl  6 or E1 shr 26 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 xor A1 xor B1 + Buffer[ 6]); D1 := D1 shl  8 or D1 shr 24 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 xor E1 xor A1 + Buffer[ 2]); C1 := C1 shl 13 or C1 shr 19 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 xor D1 xor E1 + Buffer[13]); B1 := B1 shl  6 or B1 shr 26 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 xor C1 xor D1 + Buffer[14]); A1 := A1 shl  5 or A1 shr 27 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 xor B1 xor C1 + Buffer[ 0]); E1 := E1 shl 15 or E1 shr 17 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 xor A1 xor B1 + Buffer[ 3]); D1 := D1 shl 13 or D1 shr 19 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 xor E1 xor A1 + Buffer[ 9]); C1 := C1 shl 11 or C1 shr 21 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 xor D1 xor E1 + Buffer[11]); B1 := B1 shl 11 or B1 shr 21 + A1; D1 := D1 shl 10 or D1 shr 22;

  Inc(D1, C2 + FDigest[1]);
  FDigest[1] := FDigest[2] + D2 + E1;
  FDigest[2] := FDigest[3] + E2 + A1;
  FDigest[3] := FDigest[4] + A2 + B1;
  FDigest[4] := FDigest[0] + B2 + C1;
  FDigest[0] := D1;
end;
{$ENDIF !THash_RipeMD160_asm}

class function THash_RipeMD160.DigestSize: UInt32;
begin
  Result := 20;
end;

{ THash_RipeMD256 }

{$IFNDEF THash_RipeMD256_asm}
procedure THash_RipeMD256.DoTransform(Buffer: PUInt32Array);
var
  A1, B1, C1, D1: UInt32;
  A2, B2, C2, D2: UInt32;
  T: UInt32;
begin
  A1 := FDigest[0];
  B1 := FDigest[1];
  C1 := FDigest[2];
  D1 := FDigest[3];

  A2 := FDigest[4];
  B2 := FDigest[5];
  C2 := FDigest[6];
  D2 := FDigest[7];

  Inc(A1, B1 xor C1 xor D1 + Buffer[ 0]); A1 := A1 shl 11 or A1 shr 21;
  Inc(D1, A1 xor B1 xor C1 + Buffer[ 1]); D1 := D1 shl 14 or D1 shr 18;
  Inc(C1, D1 xor A1 xor B1 + Buffer[ 2]); C1 := C1 shl 15 or C1 shr 17;
  Inc(B1, C1 xor D1 xor A1 + Buffer[ 3]); B1 := B1 shl 12 or B1 shr 20;
  Inc(A1, B1 xor C1 xor D1 + Buffer[ 4]); A1 := A1 shl  5 or A1 shr 27;
  Inc(D1, A1 xor B1 xor C1 + Buffer[ 5]); D1 := D1 shl  8 or D1 shr 24;
  Inc(C1, D1 xor A1 xor B1 + Buffer[ 6]); C1 := C1 shl  7 or C1 shr 25;
  Inc(B1, C1 xor D1 xor A1 + Buffer[ 7]); B1 := B1 shl  9 or B1 shr 23;
  Inc(A1, B1 xor C1 xor D1 + Buffer[ 8]); A1 := A1 shl 11 or A1 shr 21;
  Inc(D1, A1 xor B1 xor C1 + Buffer[ 9]); D1 := D1 shl 13 or D1 shr 19;
  Inc(C1, D1 xor A1 xor B1 + Buffer[10]); C1 := C1 shl 14 or C1 shr 18;
  Inc(B1, C1 xor D1 xor A1 + Buffer[11]); B1 := B1 shl 15 or B1 shr 17;
  Inc(A1, B1 xor C1 xor D1 + Buffer[12]); A1 := A1 shl  6 or A1 shr 26;
  Inc(D1, A1 xor B1 xor C1 + Buffer[13]); D1 := D1 shl  7 or D1 shr 25;
  Inc(C1, D1 xor A1 xor B1 + Buffer[14]); C1 := C1 shl  9 or C1 shr 23;
  Inc(B1, C1 xor D1 xor A1 + Buffer[15]); B1 := B1 shl  8 or B1 shr 24;

  T := A1; A1 := A2; A2 := T;
  T := B1; B1 := B2; B2 := T;
  T := C1; C1 := C2; C2 := T;
  T := D1; D1 := D2; D2 := T;

  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[ 5] + RipeS5); A1 := A1 shl  8 or A1 shr 24;
  Inc(D1, A1 and C1 or B1 and not C1 + Buffer[14] + RipeS5); D1 := D1 shl  9 or D1 shr 23;
  Inc(C1, D1 and B1 or A1 and not B1 + Buffer[ 7] + RipeS5); C1 := C1 shl  9 or C1 shr 23;
  Inc(B1, C1 and A1 or D1 and not A1 + Buffer[ 0] + RipeS5); B1 := B1 shl 11 or B1 shr 21;
  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[ 9] + RipeS5); A1 := A1 shl 13 or A1 shr 19;
  Inc(D1, A1 and C1 or B1 and not C1 + Buffer[ 2] + RipeS5); D1 := D1 shl 15 or D1 shr 17;
  Inc(C1, D1 and B1 or A1 and not B1 + Buffer[11] + RipeS5); C1 := C1 shl 15 or C1 shr 17;
  Inc(B1, C1 and A1 or D1 and not A1 + Buffer[ 4] + RipeS5); B1 := B1 shl  5 or B1 shr 27;
  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[13] + RipeS5); A1 := A1 shl  7 or A1 shr 25;
  Inc(D1, A1 and C1 or B1 and not C1 + Buffer[ 6] + RipeS5); D1 := D1 shl  7 or D1 shr 25;
  Inc(C1, D1 and B1 or A1 and not B1 + Buffer[15] + RipeS5); C1 := C1 shl  8 or C1 shr 24;
  Inc(B1, C1 and A1 or D1 and not A1 + Buffer[ 8] + RipeS5); B1 := B1 shl 11 or B1 shr 21;
  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[ 1] + RipeS5); A1 := A1 shl 14 or A1 shr 18;
  Inc(D1, A1 and C1 or B1 and not C1 + Buffer[10] + RipeS5); D1 := D1 shl 14 or D1 shr 18;
  Inc(C1, D1 and B1 or A1 and not B1 + Buffer[ 3] + RipeS5); C1 := C1 shl 12 or C1 shr 20;
  Inc(B1, C1 and A1 or D1 and not A1 + Buffer[12] + RipeS5); B1 := B1 shl  6 or B1 shr 26;

  T := B1; B1 := B2; B2 := T;
  T := C1; C1 := C2; C2 := T;
  T := D1; D1 := D2; D2 := T;

  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[ 7] + RipeS1); A1 := A1 shl  7 or A1 shr 25;
  Inc(D1, A1 and B1 or not A1 and C1 + Buffer[ 4] + RipeS1); D1 := D1 shl  6 or D1 shr 26;
  Inc(C1, D1 and A1 or not D1 and B1 + Buffer[13] + RipeS1); C1 := C1 shl  8 or C1 shr 24;
  Inc(B1, C1 and D1 or not C1 and A1 + Buffer[ 1] + RipeS1); B1 := B1 shl 13 or B1 shr 19;
  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[10] + RipeS1); A1 := A1 shl 11 or A1 shr 21;
  Inc(D1, A1 and B1 or not A1 and C1 + Buffer[ 6] + RipeS1); D1 := D1 shl  9 or D1 shr 23;
  Inc(C1, D1 and A1 or not D1 and B1 + Buffer[15] + RipeS1); C1 := C1 shl  7 or C1 shr 25;
  Inc(B1, C1 and D1 or not C1 and A1 + Buffer[ 3] + RipeS1); B1 := B1 shl 15 or B1 shr 17;
  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[12] + RipeS1); A1 := A1 shl  7 or A1 shr 25;
  Inc(D1, A1 and B1 or not A1 and C1 + Buffer[ 0] + RipeS1); D1 := D1 shl 12 or D1 shr 20;
  Inc(C1, D1 and A1 or not D1 and B1 + Buffer[ 9] + RipeS1); C1 := C1 shl 15 or C1 shr 17;
  Inc(B1, C1 and D1 or not C1 and A1 + Buffer[ 5] + RipeS1); B1 := B1 shl  9 or B1 shr 23;
  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[ 2] + RipeS1); A1 := A1 shl 11 or A1 shr 21;
  Inc(D1, A1 and B1 or not A1 and C1 + Buffer[14] + RipeS1); D1 := D1 shl  7 or D1 shr 25;
  Inc(C1, D1 and A1 or not D1 and B1 + Buffer[11] + RipeS1); C1 := C1 shl 13 or C1 shr 19;
  Inc(B1, C1 and D1 or not C1 and A1 + Buffer[ 8] + RipeS1); B1 := B1 shl 12 or B1 shr 20;

  T := A1; A1 := A2; A2 := T;
  T := B1; B1 := B2; B2 := T;
  T := C1; C1 := C2; C2 := T;
  T := D1; D1 := D2; D2 := T;

  Inc(A1, B1 or not C1 xor D1 + Buffer[ 6] + RipeS6); A1 := A1 shl  9 or A1 shr 23;
  Inc(D1, A1 or not B1 xor C1 + Buffer[11] + RipeS6); D1 := D1 shl 13 or D1 shr 19;
  Inc(C1, D1 or not A1 xor B1 + Buffer[ 3] + RipeS6); C1 := C1 shl 15 or C1 shr 17;
  Inc(B1, C1 or not D1 xor A1 + Buffer[ 7] + RipeS6); B1 := B1 shl  7 or B1 shr 25;
  Inc(A1, B1 or not C1 xor D1 + Buffer[ 0] + RipeS6); A1 := A1 shl 12 or A1 shr 20;
  Inc(D1, A1 or not B1 xor C1 + Buffer[13] + RipeS6); D1 := D1 shl  8 or D1 shr 24;
  Inc(C1, D1 or not A1 xor B1 + Buffer[ 5] + RipeS6); C1 := C1 shl  9 or C1 shr 23;
  Inc(B1, C1 or not D1 xor A1 + Buffer[10] + RipeS6); B1 := B1 shl 11 or B1 shr 21;
  Inc(A1, B1 or not C1 xor D1 + Buffer[14] + RipeS6); A1 := A1 shl  7 or A1 shr 25;
  Inc(D1, A1 or not B1 xor C1 + Buffer[15] + RipeS6); D1 := D1 shl  7 or D1 shr 25;
  Inc(C1, D1 or not A1 xor B1 + Buffer[ 8] + RipeS6); C1 := C1 shl 12 or C1 shr 20;
  Inc(B1, C1 or not D1 xor A1 + Buffer[12] + RipeS6); B1 := B1 shl  7 or B1 shr 25;
  Inc(A1, B1 or not C1 xor D1 + Buffer[ 4] + RipeS6); A1 := A1 shl  6 or A1 shr 26;
  Inc(D1, A1 or not B1 xor C1 + Buffer[ 9] + RipeS6); D1 := D1 shl 15 or D1 shr 17;
  Inc(C1, D1 or not A1 xor B1 + Buffer[ 1] + RipeS6); C1 := C1 shl 13 or C1 shr 19;
  Inc(B1, C1 or not D1 xor A1 + Buffer[ 2] + RipeS6); B1 := B1 shl 11 or B1 shr 21;

  T := A1; A1 := A2; A2 := T;
  T := C1; C1 := C2; C2 := T;
  T := D1; D1 := D2; D2 := T;

  Inc(A1, B1 or not C1 xor D1 + Buffer[ 3] + RipeS2); A1 := A1 shl 11 or A1 shr 21;
  Inc(D1, A1 or not B1 xor C1 + Buffer[10] + RipeS2); D1 := D1 shl 13 or D1 shr 19;
  Inc(C1, D1 or not A1 xor B1 + Buffer[14] + RipeS2); C1 := C1 shl  6 or C1 shr 26;
  Inc(B1, C1 or not D1 xor A1 + Buffer[ 4] + RipeS2); B1 := B1 shl  7 or B1 shr 25;
  Inc(A1, B1 or not C1 xor D1 + Buffer[ 9] + RipeS2); A1 := A1 shl 14 or A1 shr 18;
  Inc(D1, A1 or not B1 xor C1 + Buffer[15] + RipeS2); D1 := D1 shl  9 or D1 shr 23;
  Inc(C1, D1 or not A1 xor B1 + Buffer[ 8] + RipeS2); C1 := C1 shl 13 or C1 shr 19;
  Inc(B1, C1 or not D1 xor A1 + Buffer[ 1] + RipeS2); B1 := B1 shl 15 or B1 shr 17;
  Inc(A1, B1 or not C1 xor D1 + Buffer[ 2] + RipeS2); A1 := A1 shl 14 or A1 shr 18;
  Inc(D1, A1 or not B1 xor C1 + Buffer[ 7] + RipeS2); D1 := D1 shl  8 or D1 shr 24;
  Inc(C1, D1 or not A1 xor B1 + Buffer[ 0] + RipeS2); C1 := C1 shl 13 or C1 shr 19;
  Inc(B1, C1 or not D1 xor A1 + Buffer[ 6] + RipeS2); B1 := B1 shl  6 or B1 shr 26;
  Inc(A1, B1 or not C1 xor D1 + Buffer[13] + RipeS2); A1 := A1 shl  5 or A1 shr 27;
  Inc(D1, A1 or not B1 xor C1 + Buffer[11] + RipeS2); D1 := D1 shl 12 or D1 shr 20;
  Inc(C1, D1 or not A1 xor B1 + Buffer[ 5] + RipeS2); C1 := C1 shl  7 or C1 shr 25;
  Inc(B1, C1 or not D1 xor A1 + Buffer[12] + RipeS2); B1 := B1 shl  5 or B1 shr 27;

  T := A1; A1 := A2; A2 := T;
  T := B1; B1 := B2; B2 := T;
  T := C1; C1 := C2; C2 := T;
  T := D1; D1 := D2; D2 := T;

  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[15] + RipeS7); A1 := A1 shl  9 or A1 shr 23;
  Inc(D1, A1 and B1 or not A1 and C1 + Buffer[ 5] + RipeS7); D1 := D1 shl  7 or D1 shr 25;
  Inc(C1, D1 and A1 or not D1 and B1 + Buffer[ 1] + RipeS7); C1 := C1 shl 15 or C1 shr 17;
  Inc(B1, C1 and D1 or not C1 and A1 + Buffer[ 3] + RipeS7); B1 := B1 shl 11 or B1 shr 21;
  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[ 7] + RipeS7); A1 := A1 shl  8 or A1 shr 24;
  Inc(D1, A1 and B1 or not A1 and C1 + Buffer[14] + RipeS7); D1 := D1 shl  6 or D1 shr 26;
  Inc(C1, D1 and A1 or not D1 and B1 + Buffer[ 6] + RipeS7); C1 := C1 shl  6 or C1 shr 26;
  Inc(B1, C1 and D1 or not C1 and A1 + Buffer[ 9] + RipeS7); B1 := B1 shl 14 or B1 shr 18;
  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[11] + RipeS7); A1 := A1 shl 12 or A1 shr 20;
  Inc(D1, A1 and B1 or not A1 and C1 + Buffer[ 8] + RipeS7); D1 := D1 shl 13 or D1 shr 19;
  Inc(C1, D1 and A1 or not D1 and B1 + Buffer[12] + RipeS7); C1 := C1 shl  5 or C1 shr 27;
  Inc(B1, C1 and D1 or not C1 and A1 + Buffer[ 2] + RipeS7); B1 := B1 shl 14 or B1 shr 18;
  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[10] + RipeS7); A1 := A1 shl 13 or A1 shr 19;
  Inc(D1, A1 and B1 or not A1 and C1 + Buffer[ 0] + RipeS7); D1 := D1 shl 13 or D1 shr 19;
  Inc(C1, D1 and A1 or not D1 and B1 + Buffer[ 4] + RipeS7); C1 := C1 shl  7 or C1 shr 25;
  Inc(B1, C1 and D1 or not C1 and A1 + Buffer[13] + RipeS7); B1 := B1 shl  5 or B1 shr 27;

  T := A1; A1 := A2; A2 := T;
  T := B1; B1 := B2; B2 := T;
  T := D1; D1 := D2; D2 := T;

  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[ 1] + RipeS3); A1 := A1 shl 11 or A1 shr 21;
  Inc(D1, A1 and C1 or B1 and not C1 + Buffer[ 9] + RipeS3); D1 := D1 shl 12 or D1 shr 20;
  Inc(C1, D1 and B1 or A1 and not B1 + Buffer[11] + RipeS3); C1 := C1 shl 14 or C1 shr 18;
  Inc(B1, C1 and A1 or D1 and not A1 + Buffer[10] + RipeS3); B1 := B1 shl 15 or B1 shr 17;
  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[ 0] + RipeS3); A1 := A1 shl 14 or A1 shr 18;
  Inc(D1, A1 and C1 or B1 and not C1 + Buffer[ 8] + RipeS3); D1 := D1 shl 15 or D1 shr 17;
  Inc(C1, D1 and B1 or A1 and not B1 + Buffer[12] + RipeS3); C1 := C1 shl  9 or C1 shr 23;
  Inc(B1, C1 and A1 or D1 and not A1 + Buffer[ 4] + RipeS3); B1 := B1 shl  8 or B1 shr 24;
  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[13] + RipeS3); A1 := A1 shl  9 or A1 shr 23;
  Inc(D1, A1 and C1 or B1 and not C1 + Buffer[ 3] + RipeS3); D1 := D1 shl 14 or D1 shr 18;
  Inc(C1, D1 and B1 or A1 and not B1 + Buffer[ 7] + RipeS3); C1 := C1 shl  5 or C1 shr 27;
  Inc(B1, C1 and A1 or D1 and not A1 + Buffer[15] + RipeS3); B1 := B1 shl  6 or B1 shr 26;
  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[14] + RipeS3); A1 := A1 shl  8 or A1 shr 24;
  Inc(D1, A1 and C1 or B1 and not C1 + Buffer[ 5] + RipeS3); D1 := D1 shl  6 or D1 shr 26;
  Inc(C1, D1 and B1 or A1 and not B1 + Buffer[ 6] + RipeS3); C1 := C1 shl  5 or C1 shr 27;
  Inc(B1, C1 and A1 or D1 and not A1 + Buffer[ 2] + RipeS3); B1 := B1 shl 12 or B1 shr 20;

  T := A1; A1 := A2; A2 := T;
  T := B1; B1 := B2; B2 := T;
  T := C1; C1 := C2; C2 := T;
  T := D1; D1 := D2; D2 := T;

  Inc(A1, B1 xor C1 xor D1 + Buffer[ 8]); A1 := A1 shl 15 or A1 shr 17;
  Inc(D1, A1 xor B1 xor C1 + Buffer[ 6]); D1 := D1 shl  5 or D1 shr 27;
  Inc(C1, D1 xor A1 xor B1 + Buffer[ 4]); C1 := C1 shl  8 or C1 shr 24;
  Inc(B1, C1 xor D1 xor A1 + Buffer[ 1]); B1 := B1 shl 11 or B1 shr 21;
  Inc(A1, B1 xor C1 xor D1 + Buffer[ 3]); A1 := A1 shl 14 or A1 shr 18;
  Inc(D1, A1 xor B1 xor C1 + Buffer[11]); D1 := D1 shl 14 or D1 shr 18;
  Inc(C1, D1 xor A1 xor B1 + Buffer[15]); C1 := C1 shl  6 or C1 shr 26;
  Inc(B1, C1 xor D1 xor A1 + Buffer[ 0]); B1 := B1 shl 14 or B1 shr 18;
  Inc(A1, B1 xor C1 xor D1 + Buffer[ 5]); A1 := A1 shl  6 or A1 shr 26;
  Inc(D1, A1 xor B1 xor C1 + Buffer[12]); D1 := D1 shl  9 or D1 shr 23;
  Inc(C1, D1 xor A1 xor B1 + Buffer[ 2]); C1 := C1 shl 12 or C1 shr 20;
  Inc(B1, C1 xor D1 xor A1 + Buffer[13]); B1 := B1 shl  9 or B1 shr 23;
  Inc(A1, B1 xor C1 xor D1 + Buffer[ 9]); A1 := A1 shl 12 or A1 shr 20;
  Inc(D1, A1 xor B1 xor C1 + Buffer[ 7]); D1 := D1 shl  5 or D1 shr 27;
  Inc(C1, D1 xor A1 xor B1 + Buffer[10]); C1 := C1 shl 15 or C1 shr 17;
  Inc(B1, C1 xor D1 xor A1 + Buffer[14]); B1 := B1 shl  8 or B1 shr 24;

  Inc(FDigest[0], A2);
  Inc(FDigest[1], B2);
  Inc(FDigest[2], C2);
  Inc(FDigest[3], D1);

  Inc(FDigest[4], A1);
  Inc(FDigest[5], B1);
  Inc(FDigest[6], C1);
  Inc(FDigest[7], D2);
end;
{$ENDIF !THash_RipeMD256_asm}

procedure THash_RipeMD256.DoInit;
begin
  FDigest[0] := $67452301;
  FDigest[1] := $EFCDAB89;
  FDigest[2] := $98BADCFE;
  FDigest[3] := $10325476;
  FDigest[4] := $76543210;
  FDigest[5] := $FEDCBA98;
  FDigest[6] := $89ABCDEF;
  FDigest[7] := $01234567;
  FDigest[8] := $01234567;
  FDigest[9] := $3C2D1E0F;
end;

class function THash_RipeMD256.DigestSize: UInt32;
begin
  Result := 32;
end;

{ THash_RipeMD320 }

{$IFNDEF THash_RipeMD320_asm}
procedure THash_RipeMD320.DoTransform(Buffer: PUInt32Array);
var
  A1, B1, C1, D1, E1: UInt32;
  A2, B2, C2, D2, E2: UInt32;
  T: UInt32;
begin
  A1 := FDigest[0];
  B1 := FDigest[1];
  C1 := FDigest[2];
  D1 := FDigest[3];
  E1 := FDigest[4];
  A2 := FDigest[5];
  B2 := FDigest[6];
  C2 := FDigest[7];
  D2 := FDigest[8];
  E2 := FDigest[9];

  Inc(A1, B1 xor C1 xor D1 + Buffer[ 0]); A1 := A1 shl 11 or A1 shr 21 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 xor B1 xor C1 + Buffer[ 1]); E1 := E1 shl 14 or E1 shr 18 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 xor A1 xor B1 + Buffer[ 2]); D1 := D1 shl 15 or D1 shr 17 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 xor E1 xor A1 + Buffer[ 3]); C1 := C1 shl 12 or C1 shr 20 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 xor D1 xor E1 + Buffer[ 4]); B1 := B1 shl  5 or B1 shr 27 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 xor C1 xor D1 + Buffer[ 5]); A1 := A1 shl  8 or A1 shr 24 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 xor B1 xor C1 + Buffer[ 6]); E1 := E1 shl  7 or E1 shr 25 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 xor A1 xor B1 + Buffer[ 7]); D1 := D1 shl  9 or D1 shr 23 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 xor E1 xor A1 + Buffer[ 8]); C1 := C1 shl 11 or C1 shr 21 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 xor D1 xor E1 + Buffer[ 9]); B1 := B1 shl 13 or B1 shr 19 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 xor C1 xor D1 + Buffer[10]); A1 := A1 shl 14 or A1 shr 18 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 xor B1 xor C1 + Buffer[11]); E1 := E1 shl 15 or E1 shr 17 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 xor A1 xor B1 + Buffer[12]); D1 := D1 shl  6 or D1 shr 26 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 xor E1 xor A1 + Buffer[13]); C1 := C1 shl  7 or C1 shr 25 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 xor D1 xor E1 + Buffer[14]); B1 := B1 shl  9 or B1 shr 23 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 xor C1 xor D1 + Buffer[15]); A1 := A1 shl  8 or A1 shr 24 + E1; C1 := C1 shl 10 or C1 shr 22;

  T := A1; A1 := A2; A2 := T;
  T := B1; B1 := B2; B2 := T;
  T := C1; C1 := C2; C2 := T;
  T := D1; D1 := D2; D2 := T;
  T := E1; E1 := E2; E2 := T;

  Inc(A1, C1 or not D1 xor B1 + Buffer[ 5] + RipeS5); A1 := A1 shl  8 or A1 shr 24 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, B1 or not C1 xor A1 + Buffer[14] + RipeS5); E1 := E1 shl  9 or E1 shr 23 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, A1 or not B1 xor E1 + Buffer[ 7] + RipeS5); D1 := D1 shl  9 or D1 shr 23 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, E1 or not A1 xor D1 + Buffer[ 0] + RipeS5); C1 := C1 shl 11 or C1 shr 21 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, D1 or not E1 xor C1 + Buffer[ 9] + RipeS5); B1 := B1 shl 13 or B1 shr 19 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, C1 or not D1 xor B1 + Buffer[ 2] + RipeS5); A1 := A1 shl 15 or A1 shr 17 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, B1 or not C1 xor A1 + Buffer[11] + RipeS5); E1 := E1 shl 15 or E1 shr 17 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, A1 or not B1 xor E1 + Buffer[ 4] + RipeS5); D1 := D1 shl  5 or D1 shr 27 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, E1 or not A1 xor D1 + Buffer[13] + RipeS5); C1 := C1 shl  7 or C1 shr 25 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, D1 or not E1 xor C1 + Buffer[ 6] + RipeS5); B1 := B1 shl  7 or B1 shr 25 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, C1 or not D1 xor B1 + Buffer[15] + RipeS5); A1 := A1 shl  8 or A1 shr 24 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, B1 or not C1 xor A1 + Buffer[ 8] + RipeS5); E1 := E1 shl 11 or E1 shr 21 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, A1 or not B1 xor E1 + Buffer[ 1] + RipeS5); D1 := D1 shl 14 or D1 shr 18 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, E1 or not A1 xor D1 + Buffer[10] + RipeS5); C1 := C1 shl 14 or C1 shr 18 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, D1 or not E1 xor C1 + Buffer[ 3] + RipeS5); B1 := B1 shl 12 or B1 shr 20 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, C1 or not D1 xor B1 + Buffer[12] + RipeS5); A1 := A1 shl  6 or A1 shr 26 + E1; C1 := C1 shl 10 or C1 shr 22;

  T := B1; B1 := B2; B2 := T;
  T := C1; C1 := C2; C2 := T;
  T := D1; D1 := D2; D2 := T;
  T := E1; E1 := E2; E2 := T;

  Inc(E1, A1 and B1 or not A1 and C1 + Buffer[ 7] + RipeS1); E1 := E1 shl  7 or E1 shr 25 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 and A1 or not E1 and B1 + Buffer[ 4] + RipeS1); D1 := D1 shl  6 or D1 shr 26 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 and E1 or not D1 and A1 + Buffer[13] + RipeS1); C1 := C1 shl  8 or C1 shr 24 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 and D1 or not C1 and E1 + Buffer[ 1] + RipeS1); B1 := B1 shl 13 or B1 shr 19 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[10] + RipeS1); A1 := A1 shl 11 or A1 shr 21 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 and B1 or not A1 and C1 + Buffer[ 6] + RipeS1); E1 := E1 shl  9 or E1 shr 23 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 and A1 or not E1 and B1 + Buffer[15] + RipeS1); D1 := D1 shl  7 or D1 shr 25 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 and E1 or not D1 and A1 + Buffer[ 3] + RipeS1); C1 := C1 shl 15 or C1 shr 17 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 and D1 or not C1 and E1 + Buffer[12] + RipeS1); B1 := B1 shl  7 or B1 shr 25 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[ 0] + RipeS1); A1 := A1 shl 12 or A1 shr 20 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 and B1 or not A1 and C1 + Buffer[ 9] + RipeS1); E1 := E1 shl 15 or E1 shr 17 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 and A1 or not E1 and B1 + Buffer[ 5] + RipeS1); D1 := D1 shl  9 or D1 shr 23 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 and E1 or not D1 and A1 + Buffer[ 2] + RipeS1); C1 := C1 shl 11 or C1 shr 21 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 and D1 or not C1 and E1 + Buffer[14] + RipeS1); B1 := B1 shl  7 or B1 shr 25 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[11] + RipeS1); A1 := A1 shl 13 or A1 shr 19 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 and B1 or not A1 and C1 + Buffer[ 8] + RipeS1); E1 := E1 shl 12 or E1 shr 20 + D1; B1 := B1 shl 10 or B1 shr 22;

  T := A1; A1 := A2; A2 := T;
  T := B1; B1 := B2; B2 := T;
  T := C1; C1 := C2; C2 := T;
  T := D1; D1 := D2; D2 := T;
  T := E1; E1 := E2; E2 := T;

  Inc(E1, A1 and C1 or B1 and not C1 + Buffer[ 6] + RipeS6); E1 := E1 shl  9 or E1 shr 23 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 and B1 or A1 and not B1 + Buffer[11] + RipeS6); D1 := D1 shl 13 or D1 shr 19 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 and A1 or E1 and not A1 + Buffer[ 3] + RipeS6); C1 := C1 shl 15 or C1 shr 17 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 and E1 or D1 and not E1 + Buffer[ 7] + RipeS6); B1 := B1 shl  7 or B1 shr 25 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[ 0] + RipeS6); A1 := A1 shl 12 or A1 shr 20 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 and C1 or B1 and not C1 + Buffer[13] + RipeS6); E1 := E1 shl  8 or E1 shr 24 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 and B1 or A1 and not B1 + Buffer[ 5] + RipeS6); D1 := D1 shl  9 or D1 shr 23 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 and A1 or E1 and not A1 + Buffer[10] + RipeS6); C1 := C1 shl 11 or C1 shr 21 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 and E1 or D1 and not E1 + Buffer[14] + RipeS6); B1 := B1 shl  7 or B1 shr 25 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[15] + RipeS6); A1 := A1 shl  7 or A1 shr 25 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 and C1 or B1 and not C1 + Buffer[ 8] + RipeS6); E1 := E1 shl 12 or E1 shr 20 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 and B1 or A1 and not B1 + Buffer[12] + RipeS6); D1 := D1 shl  7 or D1 shr 25 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 and A1 or E1 and not A1 + Buffer[ 4] + RipeS6); C1 := C1 shl  6 or C1 shr 26 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 and E1 or D1 and not E1 + Buffer[ 9] + RipeS6); B1 := B1 shl 15 or B1 shr 17 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[ 1] + RipeS6); A1 := A1 shl 13 or A1 shr 19 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 and C1 or B1 and not C1 + Buffer[ 2] + RipeS6); E1 := E1 shl 11 or E1 shr 21 + D1; B1 := B1 shl 10 or B1 shr 22;

  T := A1; A1 := A2; A2 := T;
  T := C1; C1 := C2; C2 := T;
  T := D1; D1 := D2; D2 := T;
  T := E1; E1 := E2; E2 := T;

  Inc(D1, E1 or not A1 xor B1 + Buffer[ 3] + RipeS2); D1 := D1 shl 11 or D1 shr 21 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 or not E1 xor A1 + Buffer[10] + RipeS2); C1 := C1 shl 13 or C1 shr 19 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 or not D1 xor E1 + Buffer[14] + RipeS2); B1 := B1 shl  6 or B1 shr 26 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 or not C1 xor D1 + Buffer[ 4] + RipeS2); A1 := A1 shl  7 or A1 shr 25 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 or not B1 xor C1 + Buffer[ 9] + RipeS2); E1 := E1 shl 14 or E1 shr 18 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 or not A1 xor B1 + Buffer[15] + RipeS2); D1 := D1 shl  9 or D1 shr 23 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 or not E1 xor A1 + Buffer[ 8] + RipeS2); C1 := C1 shl 13 or C1 shr 19 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 or not D1 xor E1 + Buffer[ 1] + RipeS2); B1 := B1 shl 15 or B1 shr 17 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 or not C1 xor D1 + Buffer[ 2] + RipeS2); A1 := A1 shl 14 or A1 shr 18 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 or not B1 xor C1 + Buffer[ 7] + RipeS2); E1 := E1 shl  8 or E1 shr 24 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 or not A1 xor B1 + Buffer[ 0] + RipeS2); D1 := D1 shl 13 or D1 shr 19 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 or not E1 xor A1 + Buffer[ 6] + RipeS2); C1 := C1 shl  6 or C1 shr 26 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 or not D1 xor E1 + Buffer[13] + RipeS2); B1 := B1 shl  5 or B1 shr 27 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 or not C1 xor D1 + Buffer[11] + RipeS2); A1 := A1 shl 12 or A1 shr 20 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 or not B1 xor C1 + Buffer[ 5] + RipeS2); E1 := E1 shl  7 or E1 shr 25 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 or not A1 xor B1 + Buffer[12] + RipeS2); D1 := D1 shl  5 or D1 shr 27 + C1; A1 := A1 shl 10 or A1 shr 22;

  T := A1; A1 := A2; A2 := T;
  T := B1; B1 := B2; B2 := T;
  T := C1; C1 := C2; C2 := T;
  T := D1; D1 := D2; D2 := T;
  T := E1; E1 := E2; E2 := T;

  Inc(D1, E1 or not A1 xor B1 + Buffer[15] + RipeS7); D1 := D1 shl  9 or D1 shr 23 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 or not E1 xor A1 + Buffer[ 5] + RipeS7); C1 := C1 shl  7 or C1 shr 25 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 or not D1 xor E1 + Buffer[ 1] + RipeS7); B1 := B1 shl 15 or B1 shr 17 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 or not C1 xor D1 + Buffer[ 3] + RipeS7); A1 := A1 shl 11 or A1 shr 21 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 or not B1 xor C1 + Buffer[ 7] + RipeS7); E1 := E1 shl  8 or E1 shr 24 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 or not A1 xor B1 + Buffer[14] + RipeS7); D1 := D1 shl  6 or D1 shr 26 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 or not E1 xor A1 + Buffer[ 6] + RipeS7); C1 := C1 shl  6 or C1 shr 26 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 or not D1 xor E1 + Buffer[ 9] + RipeS7); B1 := B1 shl 14 or B1 shr 18 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 or not C1 xor D1 + Buffer[11] + RipeS7); A1 := A1 shl 12 or A1 shr 20 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 or not B1 xor C1 + Buffer[ 8] + RipeS7); E1 := E1 shl 13 or E1 shr 19 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 or not A1 xor B1 + Buffer[12] + RipeS7); D1 := D1 shl  5 or D1 shr 27 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 or not E1 xor A1 + Buffer[ 2] + RipeS7); C1 := C1 shl 14 or C1 shr 18 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 or not D1 xor E1 + Buffer[10] + RipeS7); B1 := B1 shl 13 or B1 shr 19 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 or not C1 xor D1 + Buffer[ 0] + RipeS7); A1 := A1 shl 13 or A1 shr 19 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 or not B1 xor C1 + Buffer[ 4] + RipeS7); E1 := E1 shl  7 or E1 shr 25 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 or not A1 xor B1 + Buffer[13] + RipeS7); D1 := D1 shl  5 or D1 shr 27 + C1; A1 := A1 shl 10 or A1 shr 22;

  T := A1; A1 := A2; A2 := T;
  T := B1; B1 := B2; B2 := T;
  T := D1; D1 := D2; D2 := T;
  T := E1; E1 := E2; E2 := T;

  Inc(C1, D1 and A1 or E1 and not A1 + Buffer[ 1] + RipeS3); C1 := C1 shl 11 or C1 shr 21 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 and E1 or D1 and not E1 + Buffer[ 9] + RipeS3); B1 := B1 shl 12 or B1 shr 20 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[11] + RipeS3); A1 := A1 shl 14 or A1 shr 18 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 and C1 or B1 and not C1 + Buffer[10] + RipeS3); E1 := E1 shl 15 or E1 shr 17 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 and B1 or A1 and not B1 + Buffer[ 0] + RipeS3); D1 := D1 shl 14 or D1 shr 18 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 and A1 or E1 and not A1 + Buffer[ 8] + RipeS3); C1 := C1 shl 15 or C1 shr 17 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 and E1 or D1 and not E1 + Buffer[12] + RipeS3); B1 := B1 shl  9 or B1 shr 23 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[ 4] + RipeS3); A1 := A1 shl  8 or A1 shr 24 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 and C1 or B1 and not C1 + Buffer[13] + RipeS3); E1 := E1 shl  9 or E1 shr 23 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 and B1 or A1 and not B1 + Buffer[ 3] + RipeS3); D1 := D1 shl 14 or D1 shr 18 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 and A1 or E1 and not A1 + Buffer[ 7] + RipeS3); C1 := C1 shl  5 or C1 shr 27 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 and E1 or D1 and not E1 + Buffer[15] + RipeS3); B1 := B1 shl  6 or B1 shr 26 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[14] + RipeS3); A1 := A1 shl  8 or A1 shr 24 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 and C1 or B1 and not C1 + Buffer[ 5] + RipeS3); E1 := E1 shl  6 or E1 shr 26 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 and B1 or A1 and not B1 + Buffer[ 6] + RipeS3); D1 := D1 shl  5 or D1 shr 27 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 and A1 or E1 and not A1 + Buffer[ 2] + RipeS3); C1 := C1 shl 12 or C1 shr 20 + B1; E1 := E1 shl 10 or E1 shr 22;

  T := A1; A1 := A2; A2 := T;
  T := B1; B1 := B2; B2 := T;
  T := C1; C1 := C2; C2 := T;
  T := D1; D1 := D2; D2 := T;
  T := E1; E1 := E2; E2 := T;

  Inc(C1, D1 and E1 or not D1 and A1 + Buffer[ 8] + RipeS8); C1 := C1 shl 15 or C1 shr 17 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 and D1 or not C1 and E1 + Buffer[ 6] + RipeS8); B1 := B1 shl  5 or B1 shr 27 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[ 4] + RipeS8); A1 := A1 shl  8 or A1 shr 24 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 and B1 or not A1 and C1 + Buffer[ 1] + RipeS8); E1 := E1 shl 11 or E1 shr 21 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 and A1 or not E1 and B1 + Buffer[ 3] + RipeS8); D1 := D1 shl 14 or D1 shr 18 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 and E1 or not D1 and A1 + Buffer[11] + RipeS8); C1 := C1 shl 14 or C1 shr 18 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 and D1 or not C1 and E1 + Buffer[15] + RipeS8); B1 := B1 shl  6 or B1 shr 26 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[ 0] + RipeS8); A1 := A1 shl 14 or A1 shr 18 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 and B1 or not A1 and C1 + Buffer[ 5] + RipeS8); E1 := E1 shl  6 or E1 shr 26 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 and A1 or not E1 and B1 + Buffer[12] + RipeS8); D1 := D1 shl  9 or D1 shr 23 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 and E1 or not D1 and A1 + Buffer[ 2] + RipeS8); C1 := C1 shl 12 or C1 shr 20 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 and D1 or not C1 and E1 + Buffer[13] + RipeS8); B1 := B1 shl  9 or B1 shr 23 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[ 9] + RipeS8); A1 := A1 shl 12 or A1 shr 20 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 and B1 or not A1 and C1 + Buffer[ 7] + RipeS8); E1 := E1 shl  5 or E1 shr 27 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 and A1 or not E1 and B1 + Buffer[10] + RipeS8); D1 := D1 shl 15 or D1 shr 17 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 and E1 or not D1 and A1 + Buffer[14] + RipeS8); C1 := C1 shl  8 or C1 shr 24 + B1; E1 := E1 shl 10 or E1 shr 22;

  T := A1; A1 := A2; A2 := T;
  T := B1; B1 := B2; B2 := T;
  T := C1; C1 := C2; C2 := T;
  T := E1; E1 := E2; E2 := T;

  Inc(B1, D1 or not E1 xor C1 + Buffer[ 4] + RipeS4); B1 := B1 shl  9 or B1 shr 23 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, C1 or not D1 xor B1 + Buffer[ 0] + RipeS4); A1 := A1 shl 15 or A1 shr 17 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, B1 or not C1 xor A1 + Buffer[ 5] + RipeS4); E1 := E1 shl  5 or E1 shr 27 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, A1 or not B1 xor E1 + Buffer[ 9] + RipeS4); D1 := D1 shl 11 or D1 shr 21 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, E1 or not A1 xor D1 + Buffer[ 7] + RipeS4); C1 := C1 shl  6 or C1 shr 26 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, D1 or not E1 xor C1 + Buffer[12] + RipeS4); B1 := B1 shl  8 or B1 shr 24 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, C1 or not D1 xor B1 + Buffer[ 2] + RipeS4); A1 := A1 shl 13 or A1 shr 19 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, B1 or not C1 xor A1 + Buffer[10] + RipeS4); E1 := E1 shl 12 or E1 shr 20 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, A1 or not B1 xor E1 + Buffer[14] + RipeS4); D1 := D1 shl  5 or D1 shr 27 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, E1 or not A1 xor D1 + Buffer[ 1] + RipeS4); C1 := C1 shl 12 or C1 shr 20 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, D1 or not E1 xor C1 + Buffer[ 3] + RipeS4); B1 := B1 shl 13 or B1 shr 19 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, C1 or not D1 xor B1 + Buffer[ 8] + RipeS4); A1 := A1 shl 14 or A1 shr 18 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, B1 or not C1 xor A1 + Buffer[11] + RipeS4); E1 := E1 shl 11 or E1 shr 21 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, A1 or not B1 xor E1 + Buffer[ 6] + RipeS4); D1 := D1 shl  8 or D1 shr 24 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, E1 or not A1 xor D1 + Buffer[15] + RipeS4); C1 := C1 shl  5 or C1 shr 27 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, D1 or not E1 xor C1 + Buffer[13] + RipeS4); B1 := B1 shl  6 or B1 shr 26 + A1; D1 := D1 shl 10 or D1 shr 22;

  T := A1; A1 := A2; A2 := T;
  T := B1; B1 := B2; B2 := T;
  T := C1; C1 := C2; C2 := T;
  T := D1; D1 := D2; D2 := T;
  T := E1; E1 := E2; E2 := T;

  Inc(B1, C1 xor D1 xor E1 + Buffer[12]); B1 := B1 shl  8 or B1 shr 24 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 xor C1 xor D1 + Buffer[15]); A1 := A1 shl  5 or A1 shr 27 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 xor B1 xor C1 + Buffer[10]); E1 := E1 shl 12 or E1 shr 20 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 xor A1 xor B1 + Buffer[ 4]); D1 := D1 shl  9 or D1 shr 23 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 xor E1 xor A1 + Buffer[ 1]); C1 := C1 shl 12 or C1 shr 20 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 xor D1 xor E1 + Buffer[ 5]); B1 := B1 shl  5 or B1 shr 27 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 xor C1 xor D1 + Buffer[ 8]); A1 := A1 shl 14 or A1 shr 18 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 xor B1 xor C1 + Buffer[ 7]); E1 := E1 shl  6 or E1 shr 26 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 xor A1 xor B1 + Buffer[ 6]); D1 := D1 shl  8 or D1 shr 24 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 xor E1 xor A1 + Buffer[ 2]); C1 := C1 shl 13 or C1 shr 19 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 xor D1 xor E1 + Buffer[13]); B1 := B1 shl  6 or B1 shr 26 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 xor C1 xor D1 + Buffer[14]); A1 := A1 shl  5 or A1 shr 27 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 xor B1 xor C1 + Buffer[ 0]); E1 := E1 shl 15 or E1 shr 17 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 xor A1 xor B1 + Buffer[ 3]); D1 := D1 shl 13 or D1 shr 19 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 xor E1 xor A1 + Buffer[ 9]); C1 := C1 shl 11 or C1 shr 21 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 xor D1 xor E1 + Buffer[11]); B1 := B1 shl 11 or B1 shr 21 + A1; D1 := D1 shl 10 or D1 shr 22;

  Inc(FDigest[0], A2);
  Inc(FDigest[1], B2);
  Inc(FDigest[2], C2);
  Inc(FDigest[3], D2);
  Inc(FDigest[4], E1);
  Inc(FDigest[5], A1);
  Inc(FDigest[6], B1);
  Inc(FDigest[7], C1);
  Inc(FDigest[8], D1);
  Inc(FDigest[9], E2);
end;
{$ENDIF !THash_RipeMD320_asm}

class function THash_RipeMD320.DigestSize: UInt32;
begin
  Result := 40;
end;

{ THash_SHA }

{$IFNDEF THash_SHA_asm}
procedure THash_SHA0.DoTransform(Buffer: PUInt32Array);
var
  A, B, C, D, E, T: UInt32;
  W: array[0..79] of UInt32;
  I: Integer;
begin
  SwapUInt32Buffer(Buffer[0], W, 16);
  if ClassType <> THash_SHA1 then
  begin
    for I := 16 to 79 do
    begin
      T := W[I - 3] xor W[I - 8] xor W[I - 14] xor W[I - 16];
      W[I] := T;
    end;
  end
  else
  begin
    for I := 16 to 79 do
    begin
      T := W[I - 3] xor W[I - 8] xor W[I - 14] xor W[I - 16];
      W[I] := T shl 1 or T shr 31;
    end;
  end;

  A := FDigest[0];
  B := FDigest[1];
  C := FDigest[2];
  D := FDigest[3];
  E := FDigest[4];

  Inc(E, (A shl 5 or A shr 27) + (D xor (B and (C xor D))) + W[ 0] + $5A827999); B := B shr 2 or B shl 30;
  Inc(D, (E shl 5 or E shr 27) + (C xor (A and (B xor C))) + W[ 1] + $5A827999); A := A shr 2 or A shl 30;
  Inc(C, (D shl 5 or D shr 27) + (B xor (E and (A xor B))) + W[ 2] + $5A827999); E := E shr 2 or E shl 30;
  Inc(B, (C shl 5 or C shr 27) + (A xor (D and (E xor A))) + W[ 3] + $5A827999); D := D shr 2 or D shl 30;
  Inc(A, (B shl 5 or B shr 27) + (E xor (C and (D xor E))) + W[ 4] + $5A827999); C := C shr 2 or C shl 30;
  Inc(E, (A shl 5 or A shr 27) + (D xor (B and (C xor D))) + W[ 5] + $5A827999); B := B shr 2 or B shl 30;
  Inc(D, (E shl 5 or E shr 27) + (C xor (A and (B xor C))) + W[ 6] + $5A827999); A := A shr 2 or A shl 30;
  Inc(C, (D shl 5 or D shr 27) + (B xor (E and (A xor B))) + W[ 7] + $5A827999); E := E shr 2 or E shl 30;
  Inc(B, (C shl 5 or C shr 27) + (A xor (D and (E xor A))) + W[ 8] + $5A827999); D := D shr 2 or D shl 30;
  Inc(A, (B shl 5 or B shr 27) + (E xor (C and (D xor E))) + W[ 9] + $5A827999); C := C shr 2 or C shl 30;
  Inc(E, (A shl 5 or A shr 27) + (D xor (B and (C xor D))) + W[10] + $5A827999); B := B shr 2 or B shl 30;
  Inc(D, (E shl 5 or E shr 27) + (C xor (A and (B xor C))) + W[11] + $5A827999); A := A shr 2 or A shl 30;
  Inc(C, (D shl 5 or D shr 27) + (B xor (E and (A xor B))) + W[12] + $5A827999); E := E shr 2 or E shl 30;
  Inc(B, (C shl 5 or C shr 27) + (A xor (D and (E xor A))) + W[13] + $5A827999); D := D shr 2 or D shl 30;
  Inc(A, (B shl 5 or B shr 27) + (E xor (C and (D xor E))) + W[14] + $5A827999); C := C shr 2 or C shl 30;
  Inc(E, (A shl 5 or A shr 27) + (D xor (B and (C xor D))) + W[15] + $5A827999); B := B shr 2 or B shl 30;
  Inc(D, (E shl 5 or E shr 27) + (C xor (A and (B xor C))) + W[16] + $5A827999); A := A shr 2 or A shl 30;
  Inc(C, (D shl 5 or D shr 27) + (B xor (E and (A xor B))) + W[17] + $5A827999); E := E shr 2 or E shl 30;
  Inc(B, (C shl 5 or C shr 27) + (A xor (D and (E xor A))) + W[18] + $5A827999); D := D shr 2 or D shl 30;
  Inc(A, (B shl 5 or B shr 27) + (E xor (C and (D xor E))) + W[19] + $5A827999); C := C shr 2 or C shl 30;

  Inc(E, (A shl 5 or A shr 27) + (D xor B xor C) + W[20] + $6ED9EBA1); B := B shr 2 or B shl 30;
  Inc(D, (E shl 5 or E shr 27) + (C xor A xor B) + W[21] + $6ED9EBA1); A := A shr 2 or A shl 30;
  Inc(C, (D shl 5 or D shr 27) + (B xor E xor A) + W[22] + $6ED9EBA1); E := E shr 2 or E shl 30;
  Inc(B, (C shl 5 or C shr 27) + (A xor D xor E) + W[23] + $6ED9EBA1); D := D shr 2 or D shl 30;
  Inc(A, (B shl 5 or B shr 27) + (E xor C xor D) + W[24] + $6ED9EBA1); C := C shr 2 or C shl 30;
  Inc(E, (A shl 5 or A shr 27) + (D xor B xor C) + W[25] + $6ED9EBA1); B := B shr 2 or B shl 30;
  Inc(D, (E shl 5 or E shr 27) + (C xor A xor B) + W[26] + $6ED9EBA1); A := A shr 2 or A shl 30;
  Inc(C, (D shl 5 or D shr 27) + (B xor E xor A) + W[27] + $6ED9EBA1); E := E shr 2 or E shl 30;
  Inc(B, (C shl 5 or C shr 27) + (A xor D xor E) + W[28] + $6ED9EBA1); D := D shr 2 or D shl 30;
  Inc(A, (B shl 5 or B shr 27) + (E xor C xor D) + W[29] + $6ED9EBA1); C := C shr 2 or C shl 30;
  Inc(E, (A shl 5 or A shr 27) + (D xor B xor C) + W[30] + $6ED9EBA1); B := B shr 2 or B shl 30;
  Inc(D, (E shl 5 or E shr 27) + (C xor A xor B) + W[31] + $6ED9EBA1); A := A shr 2 or A shl 30;
  Inc(C, (D shl 5 or D shr 27) + (B xor E xor A) + W[32] + $6ED9EBA1); E := E shr 2 or E shl 30;
  Inc(B, (C shl 5 or C shr 27) + (A xor D xor E) + W[33] + $6ED9EBA1); D := D shr 2 or D shl 30;
  Inc(A, (B shl 5 or B shr 27) + (E xor C xor D) + W[34] + $6ED9EBA1); C := C shr 2 or C shl 30;
  Inc(E, (A shl 5 or A shr 27) + (D xor B xor C) + W[35] + $6ED9EBA1); B := B shr 2 or B shl 30;
  Inc(D, (E shl 5 or E shr 27) + (C xor A xor B) + W[36] + $6ED9EBA1); A := A shr 2 or A shl 30;
  Inc(C, (D shl 5 or D shr 27) + (B xor E xor A) + W[37] + $6ED9EBA1); E := E shr 2 or E shl 30;
  Inc(B, (C shl 5 or C shr 27) + (A xor D xor E) + W[38] + $6ED9EBA1); D := D shr 2 or D shl 30;
  Inc(A, (B shl 5 or B shr 27) + (E xor C xor D) + W[39] + $6ED9EBA1); C := C shr 2 or C shl 30;

  Inc(E, (A shl 5 or A shr 27) + ((B and C) or (D and (B or C))) + W[40] + $8F1BBCDC); B := B shr 2 or B shl 30;
  Inc(D, (E shl 5 or E shr 27) + ((A and B) or (C and (A or B))) + W[41] + $8F1BBCDC); A := A shr 2 or A shl 30;
  Inc(C, (D shl 5 or D shr 27) + ((E and A) or (B and (E or A))) + W[42] + $8F1BBCDC); E := E shr 2 or E shl 30;
  Inc(B, (C shl 5 or C shr 27) + ((D and E) or (A and (D or E))) + W[43] + $8F1BBCDC); D := D shr 2 or D shl 30;
  Inc(A, (B shl 5 or B shr 27) + ((C and D) or (E and (C or D))) + W[44] + $8F1BBCDC); C := C shr 2 or C shl 30;
  Inc(E, (A shl 5 or A shr 27) + ((B and C) or (D and (B or C))) + W[45] + $8F1BBCDC); B := B shr 2 or B shl 30;
  Inc(D, (E shl 5 or E shr 27) + ((A and B) or (C and (A or B))) + W[46] + $8F1BBCDC); A := A shr 2 or A shl 30;
  Inc(C, (D shl 5 or D shr 27) + ((E and A) or (B and (E or A))) + W[47] + $8F1BBCDC); E := E shr 2 or E shl 30;
  Inc(B, (C shl 5 or C shr 27) + ((D and E) or (A and (D or E))) + W[48] + $8F1BBCDC); D := D shr 2 or D shl 30;
  Inc(A, (B shl 5 or B shr 27) + ((C and D) or (E and (C or D))) + W[49] + $8F1BBCDC); C := C shr 2 or C shl 30;
  Inc(E, (A shl 5 or A shr 27) + ((B and C) or (D and (B or C))) + W[50] + $8F1BBCDC); B := B shr 2 or B shl 30;
  Inc(D, (E shl 5 or E shr 27) + ((A and B) or (C and (A or B))) + W[51] + $8F1BBCDC); A := A shr 2 or A shl 30;
  Inc(C, (D shl 5 or D shr 27) + ((E and A) or (B and (E or A))) + W[52] + $8F1BBCDC); E := E shr 2 or E shl 30;
  Inc(B, (C shl 5 or C shr 27) + ((D and E) or (A and (D or E))) + W[53] + $8F1BBCDC); D := D shr 2 or D shl 30;
  Inc(A, (B shl 5 or B shr 27) + ((C and D) or (E and (C or D))) + W[54] + $8F1BBCDC); C := C shr 2 or C shl 30;
  Inc(E, (A shl 5 or A shr 27) + ((B and C) or (D and (B or C))) + W[55] + $8F1BBCDC); B := B shr 2 or B shl 30;
  Inc(D, (E shl 5 or E shr 27) + ((A and B) or (C and (A or B))) + W[56] + $8F1BBCDC); A := A shr 2 or A shl 30;
  Inc(C, (D shl 5 or D shr 27) + ((E and A) or (B and (E or A))) + W[57] + $8F1BBCDC); E := E shr 2 or E shl 30;
  Inc(B, (C shl 5 or C shr 27) + ((D and E) or (A and (D or E))) + W[58] + $8F1BBCDC); D := D shr 2 or D shl 30;
  Inc(A, (B shl 5 or B shr 27) + ((C and D) or (E and (C or D))) + W[59] + $8F1BBCDC); C := C shr 2 or C shl 30;

  Inc(E, (A shl 5 or A shr 27) + (D xor B xor C) + W[60] + $CA62C1D6); B := B shr 2 or B shl 30;
  Inc(D, (E shl 5 or E shr 27) + (C xor A xor B) + W[61] + $CA62C1D6); A := A shr 2 or A shl 30;
  Inc(C, (D shl 5 or D shr 27) + (B xor E xor A) + W[62] + $CA62C1D6); E := E shr 2 or E shl 30;
  Inc(B, (C shl 5 or C shr 27) + (A xor D xor E) + W[63] + $CA62C1D6); D := D shr 2 or D shl 30;
  Inc(A, (B shl 5 or B shr 27) + (E xor C xor D) + W[64] + $CA62C1D6); C := C shr 2 or C shl 30;
  Inc(E, (A shl 5 or A shr 27) + (D xor B xor C) + W[65] + $CA62C1D6); B := B shr 2 or B shl 30;
  Inc(D, (E shl 5 or E shr 27) + (C xor A xor B) + W[66] + $CA62C1D6); A := A shr 2 or A shl 30;
  Inc(C, (D shl 5 or D shr 27) + (B xor E xor A) + W[67] + $CA62C1D6); E := E shr 2 or E shl 30;
  Inc(B, (C shl 5 or C shr 27) + (A xor D xor E) + W[68] + $CA62C1D6); D := D shr 2 or D shl 30;
  Inc(A, (B shl 5 or B shr 27) + (E xor C xor D) + W[69] + $CA62C1D6); C := C shr 2 or C shl 30;
  Inc(E, (A shl 5 or A shr 27) + (D xor B xor C) + W[70] + $CA62C1D6); B := B shr 2 or B shl 30;
  Inc(D, (E shl 5 or E shr 27) + (C xor A xor B) + W[71] + $CA62C1D6); A := A shr 2 or A shl 30;
  Inc(C, (D shl 5 or D shr 27) + (B xor E xor A) + W[72] + $CA62C1D6); E := E shr 2 or E shl 30;
  Inc(B, (C shl 5 or C shr 27) + (A xor D xor E) + W[73] + $CA62C1D6); D := D shr 2 or D shl 30;
  Inc(A, (B shl 5 or B shr 27) + (E xor C xor D) + W[74] + $CA62C1D6); C := C shr 2 or C shl 30;
  Inc(E, (A shl 5 or A shr 27) + (D xor B xor C) + W[75] + $CA62C1D6); B := B shr 2 or B shl 30;
  Inc(D, (E shl 5 or E shr 27) + (C xor A xor B) + W[76] + $CA62C1D6); A := A shr 2 or A shl 30;
  Inc(C, (D shl 5 or D shr 27) + (B xor E xor A) + W[77] + $CA62C1D6); E := E shr 2 or E shl 30;
  Inc(B, (C shl 5 or C shr 27) + (A xor D xor E) + W[78] + $CA62C1D6); D := D shr 2 or D shl 30;
  Inc(A, (B shl 5 or B shr 27) + (E xor C xor D) + W[79] + $CA62C1D6); C := C shr 2 or C shl 30;

  Inc(FDigest[0], A);
  Inc(FDigest[1], B);
  Inc(FDigest[2], C);
  Inc(FDigest[3], D);
  Inc(FDigest[4], E);
end;
{$ENDIF !THash_SHA_asm}

procedure THash_SHA0.DoDone;
begin
  if FCount[2] or FCount[3] <> 0 then
    RaiseHashOverflowError;
  if FPaddingByte = 0 then
    FPaddingByte := $80;
  FBuffer[FBufferIndex] := FPaddingByte;
  Inc(FBufferIndex);
  if FBufferIndex > FBufferSize - 8 then
  begin
    FillChar(FBuffer[FBufferIndex], FBufferSize - FBufferIndex, 0);
    DoTransform(Pointer(FBuffer));
    FBufferIndex := 0;
  end;
  FillChar(FBuffer[FBufferIndex], FBufferSize - FBufferIndex, 0);
  PUInt32(@FBuffer[FBufferSize - 8])^ := SwapUInt32(FCount[1]);
  PUInt32(@FBuffer[FBufferSize - 4])^ := SwapUInt32(FCount[0]);
  DoTransform(Pointer(FBuffer));
  SwapUInt32Buffer(FDigest, FDigest, SizeOf(FDigest) div 4);
end;

class function THash_SHA0.DigestSize: UInt32;
begin
  Result := 20;
end;

{ THash_SHA256 }

procedure THash_SHA256.DoInit;
begin
  FDigest[0]:= $6A09E667;
  FDigest[1]:= $BB67AE85;
  FDigest[2]:= $3C6EF372;
  FDigest[3]:= $A54FF53A;
  FDigest[4]:= $510E527F;
  FDigest[5]:= $9B05688C;
  FDigest[6]:= $1F83D9AB;
  FDigest[7]:= $5BE0CD19;
end;

{$IFNDEF THash_SHA256_asm}
procedure THash_SHA256.DoTransform(Buffer: PUInt32Array);
var
  I: Integer;
  A, B, C, D, E, F, G, H: UInt32;
  T1, T2: UInt32;
  W: array[0..63] of UInt32;
begin
  SwapUInt32Buffer(Buffer[0], W, 16);

  for I := 16 to 63 do
  begin
    T1 := W[I - 15];
    T2 := W[I - 2];
    W[I] := W[I - 16] + W[I - 7] +
            ((T1 shr  7 or T1 shl 25) xor (T1 shr 18 or T1 shl 14) xor (T1 shr  3)) +
            ((T2 shr 17 or T2 shl 15) xor (T2 shr 19 or T2 shl 13) xor (T2 shr 10));
  end;

  // calculate new hash values
  A := FDigest[0];
  B := FDigest[1];
  C := FDigest[2];
  D := FDigest[3];
  E := FDigest[4];
  F := FDigest[5];
  G := FDigest[6];
  H := FDigest[7];

  for I := 0 to 63 do
  begin
    T1 := ((E shr 6 or E shl 26) xor (E shr 11 or E shl 21) xor
           (E shr 25 or E shl 7)) + H + (((F xor G) and E) xor G) + SHA_256K[I] + W[I];
    T2 := ((A shr 2 or A shl 30) xor (A shr 13 or A shl 19) xor
           (A shr 22 or A shl 10)) + (((B or C) and A) or (B and C));
    H := G; G := F; F := E; E := D + T1; D := C; C := B; B := A; A := T1 + T2;
  end;

  Inc(FDigest[0], A);
  Inc(FDigest[1], B);
  Inc(FDigest[2], C);
  Inc(FDigest[3], D);
  Inc(FDigest[4], E);
  Inc(FDigest[5], F);
  Inc(FDigest[6], G);
  Inc(FDigest[7], H);
end;
{$ENDIF !THash_SHA256_asm}

class function THash_SHA256.DigestSize: UInt32;
begin
  Result := 32;
end;

{ THash_SHA224 }

class function THash_SHA224.BlockSize: UInt32;
begin
  Result := 64;
end;

class function THash_SHA224.DigestSize: UInt32;
begin
  Result := 28;
end;

procedure THash_SHA224.DoInit;
begin
  FDigest[0]:= $C1059ED8;
  FDigest[1]:= $367CD507;
  FDigest[2]:= $3070DD17;
  FDigest[3]:= $F70E5939;
  FDigest[4]:= $FFC00B31;
  FDigest[5]:= $68581511;
  FDigest[6]:= $64F98FA7;
  FDigest[7]:= $BEFA4FA4;
end;

{ THash_SHA384 }

procedure THash_SHA384.DoInit;
begin
  FDigest[0] := Int64($CBBB9D5DC1059ED8);
  FDigest[1] := Int64($629A292A367CD507);
  FDigest[2] := Int64($9159015A3070DD17);
  FDigest[3] := Int64($152FECD8F70E5939);
  FDigest[4] := Int64($67332667FFC00B31);
  FDigest[5] := Int64($8EB44A8768581511);
  FDigest[6] := Int64($DB0C2E0D64F98FA7);
  FDigest[7] := Int64($47B5481DBEFA4FA4);
end;

{$IFNDEF THash_SHA384_asm}
procedure THash_SHA384.DoTransform(Buffer: PUInt32Array);
var
  A, B, C, D, E, F, G, H: UInt64;
  T1, T2: UInt64;
  I: Integer;
  W: array [0..79] of UInt64;
begin
{ TODO : The array passed is a UInt32 array, which doesn't fit with the name of this method!}
  SwapInt64Buffer(Buffer[0], W, 16);

  // calculate other 64 uint64
  for I := 16 to 79 do
  begin
    T1 := W[I - 15];
    T2 := W[I - 2];
    W[I] := W[I - 16] + W[I - 7] +
            ((T1 shr  1 or T1 shl 63) xor (T1 shr  8 or T1 shl 56) xor (T1 shr  7)) +
            ((T2 shr 19 or T2 shl 45) xor (T2 shr 61 or T2 shl  3) xor (T2 shr  6));
  end;

  // calculate new hash values
  A := FDigest[0];
  B := FDigest[1];
  C := FDigest[2];
  D := FDigest[3];
  E := FDigest[4];
  F := FDigest[5];
  G := FDigest[6];
  H := FDigest[7];

  for I := 0 to 79 do
  begin
    T1 := ((E shr 14 or E shl 50) xor (E shr 18 or E shl 46) xor
           (E shr 41 or E shl 23)) + H + (((F xor G) and E) xor G) + SHA_512K[I] + W[I];
    T2 := ((A shr 28 or A shl 36) xor (A shr 34 or A shl 30) xor
           (A shr 39 or A shl 25)) + (((B or C) and A) or (B and C));
    H := G;
    G := F;
    F := E;
    E := D + T1;
    D := C;
    C := B;
    B := A;
    A := T1 + T2;
  end;

  Inc(FDigest[0], A);
  Inc(FDigest[1], B);
  Inc(FDigest[2], C);
  Inc(FDigest[3], D);
  Inc(FDigest[4], E);
  Inc(FDigest[5], F);
  Inc(FDigest[6], G);
  Inc(FDigest[7], H);
end;
{$ENDIF !THash_SHA384_asm}

procedure THash_SHA384.DoDone;
begin
  if FPaddingByte = 0 then
    FPaddingByte := $80;
  FBuffer[FBufferIndex] := FPaddingByte;
  Inc(FBufferIndex);
  if FBufferIndex > FBufferSize - 16 then
  begin
    FillChar(FBuffer[FBufferIndex], FBufferSize - FBufferIndex, 0);
    DoTransform(Pointer(FBuffer));
    FBufferIndex := 0;
  end;
  FillChar(FBuffer[FBufferIndex], FBufferSize - FBufferIndex, 0);
  SwapUInt32Buffer(FCount, FCount, 4);
  PUInt32(@FBuffer[FBufferSize - 16])^ := FCount[3];
  PUInt32(@FBuffer[FBufferSize - 12])^ := FCount[2];
  PUInt32(@FBuffer[FBufferSize -  8])^ := FCount[1];
  PUInt32(@FBuffer[FBufferSize -  4])^ := FCount[0];
  DoTransform(Pointer(FBuffer));
  SwapInt64Buffer(FDigest, FDigest, SizeOf(FDigest) div 8);
end;

function THash_SHA384.Digest: PByteArray;
begin
  Result := @FDigest;
end;

class function THash_SHA384.DigestSize: UInt32;
begin
  Result := 48;
end;

class function THash_SHA384.BlockSize: UInt32;
begin
  Result := 128;
end;

{ THash_SHA512 }

procedure THash_SHA512.DoInit;
begin
  FDigest[0] := Int64($6A09E667F3BCC908);
  FDigest[1] := Int64($BB67AE8584CAA73B);
  FDigest[2] := Int64($3C6EF372FE94F82B);
  FDigest[3] := Int64($A54FF53A5F1D36F1);
  FDigest[4] := Int64($510E527FADE682D1);
  FDigest[5] := Int64($9B05688C2B3E6C1F);
  FDigest[6] := Int64($1F83D9ABFB41BD6B);
  FDigest[7] := Int64($5BE0CD19137E2179);
end;

class function THash_SHA512.DigestSize: UInt32;
begin
  Result := 64;
end;

{ THashBaseHaval }

procedure THashBaseHaval.SetRounds(Value: UInt32);
begin
  if (Value < 3) or (Value > 5) then
  begin
    if DigestSize <= 20 then
      Value := 3
    else
    begin
      if DigestSize <= 28 then
        Value := 4
      else
        Value := 5;
    end;
  end;
  FRounds := Value;
  case FRounds of
    3: FTransform := DoTransform3;
    4: FTransform := DoTransform4;
    5: FTransform := DoTransform5;
  end;
end;

procedure THashBaseHaval.DoInit;
begin
  SetRounds(FRounds);
  FDigest[0] := $243F6A88;
  FDigest[1] := $85A308D3;
  FDigest[2] := $13198A2E;
  FDigest[3] := $03707344;
  FDigest[4] := $A4093822;
  FDigest[5] := $299F31D0;
  FDigest[6] := $082EFA98;
  FDigest[7] := $EC4E6C89;
end;

procedure THashBaseHaval.DoTransform(Buffer: PUInt32Array);
begin
  FTransform(Buffer);
end;

class function THashBaseHaval.GetMaxRounds: UInt8;
begin
  Result := 5;
end;

class function THashBaseHaval.GetMinRounds: UInt8;
begin
  if DigestSize <= 20 then
    Result := 3
  else
  begin
    if DigestSize <= 28 then
      Result := 4
    else
      Result := 5;
  end;
end;

{$IFNDEF THashBaseHaval_asm}
procedure THashBaseHaval.DoTransform3(Buffer: PUInt32Array);
var
  A, B, C, D, E, F, G, H, I, T: UInt32;
  Data: PUInt32;
  Offset: PByte;
begin
  Offset := @Haval_Offset;
  Data   := @Haval_Data;

  A := FDigest[0];
  B := FDigest[1];
  C := FDigest[2];
  D := FDigest[3];
  E := FDigest[4];
  F := FDigest[5];
  G := FDigest[6];
  H := FDigest[7];

  for I := 0 to 31 do
  begin
    T := C and (E xor D) xor G and A xor F and B xor E;
    T := (T shr 7 or T shl 25) + (H shr 11 or H shl 21) + Buffer[I];
	H := G; G := F; F := E; E := D; D := C; C := B; B := A; A := T;
  end;

  for I := 0 to 31 do
  begin
    T := F and (D and not A xor B and C xor E xor G) xor B and (D xor C) xor A and C xor G;
    T := (T shr 7 or T shl 25) + (H shr 11 or H shl 21) + Buffer[Offset^] + Data^;
    Inc(Offset);
    Inc(Data);
    H := G; G := F; F := E; E := D; D := C; C := B; B := A; A := T;
  end;

  for I := 0 to 31 do
  begin
    T := D and (F and E xor G xor A) xor F and C xor E and B xor A;
    T := (T shr 7 or T shl 25) + (H shr 11 or H shl 21) + Buffer[Offset^] + Data^;
    Inc(Offset);
    Inc(Data);
    H := G; G := F; F := E; E := D; D := C; C := B; B := A; A := T;
  end;

  Inc(FDigest[0], A);
  Inc(FDigest[1], B);
  Inc(FDigest[2], C);
  Inc(FDigest[3], D);
  Inc(FDigest[4], E);
  Inc(FDigest[5], F);
  Inc(FDigest[6], G);
  Inc(FDigest[7], H);
end;

procedure THashBaseHaval.DoTransform4(Buffer: PUInt32Array);
var
  A, B, C, D, E, F, G, H, I, T: UInt32;
  Data: PUInt32;
  Offset: PByte;
begin
  Offset := @Haval_Offset;
  Data := @Haval_Data;

  A := FDigest[0];
  B := FDigest[1];
  C := FDigest[2];
  D := FDigest[3];
  E := FDigest[4];
  F := FDigest[5];
  G := FDigest[6];
  H := FDigest[7];

  for I := 0 to 31 do
  begin
    T := D and (A xor B) xor F and G xor E and C xor A;
    T := (T shr 7 or T shl 25) + (H shr 11 or H shl 21) + Buffer[I];
    H := G; G := F; F := E; E := D; D := C; C := B; B := A; A := T;
  end;

  for I := 0 to 31 do
  begin
    T := B and (G and not A xor C and F xor D xor E) xor C and (G xor F) xor A and F xor E;
    T := (T shr 7 or T shl 25) + (H shr 11 or H shl 21) + Buffer[Offset^] + Data^;
    Inc(Offset);
    Inc(Data);
    H := G; G := F; F := E; E := D; D := C; C := B; B := A; A := T;
  end;

  for I := 0 to 31 do
  begin
    T := G and (C and A xor B xor F) xor C and D xor A and E xor F;
    T := (T shr 7 or T shl 25) + (H shr 11 or H shl 21) + Buffer[Offset^] + Data^;
    Inc(Offset);
    Inc(Data);
    H := G; G := F; F := E; E := D; D := C; C := B; B := A; A := T;
  end;

  for I := 0 to 31 do
  begin
    T := A and (E and not C xor F and not G xor B xor G xor D) xor F and
        (B and C xor E xor G) xor C and G xor D;
    T := (T shr 7 or T shl 25) + (H shr 11 or H shl 21) + Buffer[Offset^] + Data^;
    Inc(Offset);
    Inc(Data);
    H := G; G := F; F := E; E := D; D := C; C := B; B := A; A := T;
  end;

  Inc(FDigest[0], A);
  Inc(FDigest[1], B);
  Inc(FDigest[2], C);
  Inc(FDigest[3], D);
  Inc(FDigest[4], E);
  Inc(FDigest[5], F);
  Inc(FDigest[6], G);
  Inc(FDigest[7], H);
end;

procedure THashBaseHaval.DoTransform5(Buffer: PUInt32Array);
var
  A, B, C, D, E, F, G, H, I, T: UInt32;
  Data: PUInt32;
  Offset: PByte;
begin
  Offset := @Haval_Offset;
  Data := @Haval_Data;

  A := FDigest[0];
  B := FDigest[1];
  C := FDigest[2];
  D := FDigest[3];
  E := FDigest[4];
  F := FDigest[5];
  G := FDigest[6];
  H := FDigest[7];

  for I := 0 to 31 do
  begin
    T := C and (G xor B) xor F and E xor A and D xor G;
    T := (T shr 7 or T shl 25) + (H shr 11 or H shl 21) + Buffer[I];
    H := G; G := F; F := E; E := D; D := C; C := B; B := A; A := T;
  end;

  for I := 0 to 31 do
  begin
    T := D and (E and not A xor B and C xor G xor F) xor B and (E xor C) xor A and C xor F;
    T := (T shr 7 or T shl 25) + (H shr 11 or H shl 21) + Buffer[Offset^] + Data^;
    Inc(Offset);
    Inc(Data);
    H := G; G := F; F := E; E := D; D := C; C := B; B := A; A := T;
  end;

  for I := 0 to 31 do
  begin
    T := E and (B and D xor C xor F) xor B and A xor D and G xor F;
    T := (T shr 7 or T shl 25) + (H shr 11 or H shl 21) + Buffer[Offset^] + Data^;
    Inc(Offset);
    Inc(Data);
    H := G; G := F; F := E; E := D; D := C; C := B; B := A; A := T;
  end;

  for I := 0 to 31 do
  begin
    T := D and (F and not A xor C and not B xor E xor B xor G) xor C and
        (E and A xor F xor B) xor A and B xor G;
    T := (T shr 7 or T shl 25) + (H shr 11 or H shl 21) + Buffer[Offset^] + Data^;
    Inc(Offset);
    Inc(Data);
    H := G; G := F; F := E; E := D; D := C; C := B; B := A; A := T;
  end;

  for I := 0 to 31 do
  begin
    T := B and (D and E and G xor not F) xor D and A xor E and F xor G and C;
    T := (T shr 7 or T shl 25) + (H shr 11 or H shl 21) + Buffer[Offset^] + Data^;
    Inc(Offset); Inc(Data);
    H := G; G := F; F := E; E := D; D := C; C := B; B := A; A := T;
  end;

  Inc(FDigest[0], A);
  Inc(FDigest[1], B);
  Inc(FDigest[2], C);
  Inc(FDigest[3], D);
  Inc(FDigest[4], E);
  Inc(FDigest[5], F);
  Inc(FDigest[6], G);
  Inc(FDigest[7], H);
end;

{$ENDIF !THashBaseHaval_asm}

procedure THashBaseHaval.DoDone;

  function ROR(Value, Count: UInt32): UInt32;
  {$IFDEF X86ASM}
  asm
        MOV     ECX,EDX
        ROR     EAX,CL
  end;
  {$ELSE !X86ASM}
  begin
    Result := (Value shr Count) or (Value shl (32 - Count));
  end;
  {$ENDIF !X86ASM}

var
  T: Word;
begin
  if FCount[2] or FCount[3] <> 0 then
    RaiseHashOverflowError;
  if FPaddingByte = 0 then
    FPaddingByte := $01;
  FBuffer[FBufferIndex] := FPaddingByte;
  Inc(FBufferIndex);
  if FBufferIndex > FBufferSize - 10 then
  begin
    FillChar(FBuffer[FBufferIndex], FBufferSize - FBufferIndex - 10, 0);
    DoTransform(Pointer(FBuffer));
    FBufferIndex := 0;
  end;
  FillChar(FBuffer[FBufferIndex], FBufferSize - FBufferIndex - 10, 0);
  T := DigestSize shl 9 or FRounds shl 3 or 1;
  Move(T, FBuffer[FBufferSize - 10], SizeOf(T));
  Move(FCount, FBuffer[FBufferSize - 8], 8);
  DoTransform(Pointer(FBuffer));

  case DigestSize of
    16: begin
          Inc(FDigest[0], ROR(FDigest[7] and $000000FF or
                              FDigest[6] and $FF000000 or
                              FDigest[5] and $00FF0000 or
                              FDigest[4] and $0000FF00, 8));
          Inc(FDigest[1], ROR(FDigest[7] and $0000FF00 or
                              FDigest[6] and $000000FF or
                              FDigest[5] and $FF000000 or
                              FDigest[4] and $00FF0000, 16));
          Inc(FDigest[2], ROR(FDigest[7] and $00FF0000 or
                              FDigest[6] and $0000FF00 or
                              FDigest[5] and $000000FF or
                              FDigest[4] and $FF000000, 24));
          Inc(FDigest[3],     FDigest[7] and $FF000000 or
                              FDigest[6] and $00FF0000 or
                              FDigest[5] and $0000FF00 or
                              FDigest[4] and $000000FF);
        end;
    20: begin
          Inc(FDigest[0], ROR(FDigest[7] and ($3F) or
                              FDigest[6] and ($7F shl 25) or
                              FDigest[5] and ($3F shl 19), 19));
          Inc(FDigest[1], ROR(FDigest[7] and ($3F shl 6) or
                              FDigest[6] and ($3F) or
                              FDigest[5] and ($7F shl 25), 25));
          Inc(FDigest[2],     FDigest[7] and ($7F shl 12) or
                              FDigest[6] and ($3F shl  6) or
                              FDigest[5] and ($3F));
          Inc(FDigest[3],    (FDigest[7] and ($3F shl 19) or
                              FDigest[6] and ($7F shl 12) or
                              FDigest[5] and ($3F shl  6)) shr 6);
          Inc(FDigest[4],    (FDigest[7] and ($7F shl 25) or
                              FDigest[6] and ($3F shl 19) or
                              FDigest[5] and ($7F shl 12)) shr 12);
        end;
    24: begin
          Inc(FDigest[0], ROR(FDigest[7] and ($1F) or
                              FDigest[6] and ($3F shl 26), 26));
          Inc(FDigest[1],     FDigest[7] and ($1F shl 5) or
                              FDigest[6] and ($1F));
          Inc(FDigest[2],    (FDigest[7] and ($3F shl 10) or
                              FDigest[6] and ($1F shl  5)) shr 5);
          Inc(FDigest[3],    (FDigest[7] and ($1F shl 16) or
                              FDigest[6] and ($3F shl 10)) shr 10);
          Inc(FDigest[4],    (FDigest[7] and ($1F shl 21) or
                              FDigest[6] and ($1F shl 16)) shr 16);
          Inc(FDigest[5],    (FDigest[7] and ($3F shl 26) or
                              FDigest[6] and ($1F shl 21)) shr 21);
        end;
    28: begin
          Inc(FDigest[0], FDigest[7] shr 27 and $1F);
          Inc(FDigest[1], FDigest[7] shr 22 and $1F);
          Inc(FDigest[2], FDigest[7] shr 18 and $0F);
          Inc(FDigest[3], FDigest[7] shr 13 and $1F);
          Inc(FDigest[4], FDigest[7] shr  9 and $0F);
          Inc(FDigest[5], FDigest[7] shr  4 and $1F);
          Inc(FDigest[6], FDigest[7]        and $0F);
        end;
  end;
end;

function THashBaseHaval.Digest: PByteArray;
begin
  Result := @FDigest;
end;

class function THashBaseHaval.BlockSize: UInt32;
begin
  Result := 128;
end;

{ THash_Haval128 }

class function THash_Haval128.DigestSize: UInt32;
begin
  Result := 16;
end;

{ THash_Haval160 }

class function THash_Haval160.DigestSize: UInt32;
begin
  Result := 20;
end;

{ THash_Haval192 }

class function THash_Haval192.DigestSize: UInt32;
begin
  Result := 24;
end;

{ THash_Haval224 }

class function THash_Haval224.DigestSize: UInt32;
begin
  Result := 28;
end;

{ THash_Haval256 }

class function THash_Haval256.DigestSize: UInt32;
begin
  Result := 32;
end;

{ THash_Tiger }

procedure THash_Tiger.SetRounds(Value: Integer);
begin
  if (Value < cTigerMinRounds) then
    Value := cTigerMinRounds;
    
  if (Value > cTigerMaxRounds) then
    Value := cTigerMaxRounds;
    
  FRounds := Value;
end;

procedure THash_Tiger.DoInit;
begin
  SetRounds(FRounds);
  if FPaddingByte = 0 then
    FPaddingByte := $01;
  FDigest[0] := $89ABCDEF;
  FDigest[1] := $01234567;
  FDigest[2] := $76543210;
  FDigest[3] := $FEDCBA98;
  FDigest[4] := $C3B2E187;
  FDigest[5] := $F096A5B4;
end;

procedure THash_Tiger.DoTransform(Buffer: PUInt32Array);
type
  PTiger_Data = ^TTiger_Data;
  TTiger_Data = array[0..3, 0..255] of Int64;

  PInt64Array = ^TInt64Array;
  TInt64Array = array[0..7] of Int64;

var
  A, B, C, T: Int64;
  x0, x1, x2, x3, x4, x5, x6, x7: UInt64;
  I: Integer;
begin
  A  := PInt64Array(@FDigest)[0];
  B  := PInt64Array(@FDigest)[1];
  C  := PInt64Array(@FDigest)[2];
  x0 := PInt64Array(Buffer)[0];
  x1 := PInt64Array(Buffer)[1];
  x2 := PInt64Array(Buffer)[2];
  x3 := PInt64Array(Buffer)[3];
  x4 := PInt64Array(Buffer)[4];
  x5 := PInt64Array(Buffer)[5];
  x6 := PInt64Array(Buffer)[6];
  x7 := PInt64Array(Buffer)[7];

  for I := 1 to FRounds do // a Loop is faster for PC with small Cache
  begin
    if I > 1 then // Key Schedule
    begin
      Dec(x0, x7 xor $A5A5A5A5A5A5A5A5);
      x1 := x1 xor x0;
      Inc(x2, x1);
      Dec(x3, x2 xor (not x1 shl 19));
      x4 := x4 xor x3;
      Inc(x5, x4);
      Dec(x6, x5 xor (not x4 shr 23));
      x7 := x7 xor x6;
      Inc(x0, x7);
      Dec(x1, x0 xor (not x7 shl 19));
      x2 := x2 xor x1;
      Inc(x3, x2);
      Dec(x4, x3 xor (not x2 shr 23));
      x5 := x5 xor x4;
      Inc(x6, x5);
      Dec(x7, x6 xor $0123456789ABCDEF);
    end;

    C := C xor x0;
    Dec(A, TTiger_Data(Tiger_Data)[0, UInt32(C)        and $FF] xor
           TTiger_Data(Tiger_Data)[1, UInt32(C) shr 16 and $FF] xor
           TTiger_Data(Tiger_Data)[2,          C  shr 32 and $FF] xor
           TTiger_Data(Tiger_Data)[3, UInt32(C shr 32) shr 16 and $FF]);
    Inc(B, TTiger_Data(Tiger_Data)[3, UInt32(C) shr  8 and $FF] xor
           TTiger_Data(Tiger_Data)[2, UInt32(C) shr 24] xor
           TTiger_Data(Tiger_Data)[1, UInt32(C shr 32) shr 8 and $FF] xor
           TTiger_Data(Tiger_Data)[0, UInt32(C shr 32) shr 24]);

    if I = 1 then
      B := B shl 2 + B
    else
    begin
      if I = 2 then
        B := B shl 3 - B
      else
        B := B shl 3 + B;
    end;

    A := A xor x1;
    Dec(B, TTiger_Data(Tiger_Data)[0, UInt32(A)        and $FF] xor
           TTiger_Data(Tiger_Data)[1, UInt32(A) shr 16 and $FF] xor
           TTiger_Data(Tiger_Data)[2,          A  shr 32 and $FF] xor
           TTiger_Data(Tiger_Data)[3, UInt32(A shr 32) shr 16 and $FF]);
    Inc(C, TTiger_Data(Tiger_Data)[3, UInt32(A) shr  8 and $FF] xor
           TTiger_Data(Tiger_Data)[2, UInt32(A) shr 24] xor
           TTiger_Data(Tiger_Data)[1, UInt32(A shr 32) shr 8 and $FF] xor
           TTiger_Data(Tiger_Data)[0, UInt32(A shr 32) shr 24]);

    if I = 1 then
      C := C shl 2 + C
    else
    begin
      if I = 2 then
        C := C shl 3 - C
      else
        C := C shl 3 + C;
    end;

    B := B xor x2;
    Dec(C, TTiger_Data(Tiger_Data)[0, UInt32(B)        and $FF] xor
           TTiger_Data(Tiger_Data)[1, UInt32(B) shr 16 and $FF] xor
           TTiger_Data(Tiger_Data)[2,          B  shr 32 and $FF] xor
           TTiger_Data(Tiger_Data)[3, UInt32(B shr 32) shr 16 and $FF]);
    Inc(A, TTiger_Data(Tiger_Data)[3, UInt32(B) shr  8 and $FF] xor
           TTiger_Data(Tiger_Data)[2, UInt32(B) shr 24] xor
           TTiger_Data(Tiger_Data)[1, UInt32(B shr 32) shr 8 and $FF] xor
           TTiger_Data(Tiger_Data)[0, UInt32(B shr 32) shr 24]);

    if I = 1 then
      A := A shl 2 + A
    else
    begin
      if I = 2 then
        A := A shl 3 - A
      else
        A := A shl 3 + A;
    end;

    C := C xor x3;
    Dec(A, TTiger_Data(Tiger_Data)[0, UInt32(C)        and $FF] xor
           TTiger_Data(Tiger_Data)[1, UInt32(C) shr 16 and $FF] xor
           TTiger_Data(Tiger_Data)[2,          C  shr 32 and $FF] xor
           TTiger_Data(Tiger_Data)[3, UInt32(C shr 32) shr 16 and $FF]);
    Inc(B, TTiger_Data(Tiger_Data)[3, UInt32(C) shr  8 and $FF] xor
           TTiger_Data(Tiger_Data)[2, UInt32(C) shr 24] xor
           TTiger_Data(Tiger_Data)[1, UInt32(C shr 32) shr 8 and $FF] xor
           TTiger_Data(Tiger_Data)[0, UInt32(C shr 32) shr 24]);

    if I = 1 then
      B := B shl 2 + B
    else
    begin
      if I = 2 then
        B := B shl 3 - B
      else
        B := B shl 3 + B;
    end;

    A := A xor x4;
    Dec(B, TTiger_Data(Tiger_Data)[0, UInt32(A)        and $FF] xor
           TTiger_Data(Tiger_Data)[1, UInt32(A) shr 16 and $FF] xor
           TTiger_Data(Tiger_Data)[2,          A  shr 32 and $FF] xor
           TTiger_Data(Tiger_Data)[3, UInt32(A shr 32) shr 16 and $FF]);
    Inc(C, TTiger_Data(Tiger_Data)[3, UInt32(A) shr  8 and $FF] xor
           TTiger_Data(Tiger_Data)[2, UInt32(A) shr 24] xor
           TTiger_Data(Tiger_Data)[1, UInt32(A shr 32) shr 8 and $FF] xor
           TTiger_Data(Tiger_Data)[0, UInt32(A shr 32) shr 24]);

    if I = 1 then
      C := C shl 2 + C
    else
    begin
      if I = 2 then
        C := C shl 3 - C
      else
        C := C shl 3 + C;
    end;

    B := B xor x5;
    Dec(C, TTiger_Data(Tiger_Data)[0, UInt32(B)        and $FF] xor
           TTiger_Data(Tiger_Data)[1, UInt32(B) shr 16 and $FF] xor
           TTiger_Data(Tiger_Data)[2,          B  shr 32 and $FF] xor
           TTiger_Data(Tiger_Data)[3, UInt32(B shr 32) shr 16 and $FF]);
    Inc(A, TTiger_Data(Tiger_Data)[3, UInt32(B) shr  8 and $FF] xor
           TTiger_Data(Tiger_Data)[2, UInt32(B) shr 24] xor
           TTiger_Data(Tiger_Data)[1, UInt32(B shr 32) shr 8 and $FF] xor
           TTiger_Data(Tiger_Data)[0, UInt32(B shr 32) shr 24]);

    if I = 1 then
      A := A shl 2 + A
    else
    begin
      if I = 2 then
        A := A shl 3 - A
      else
        A := A shl 3 + A;
    end;

    C := C xor x6;
    Dec(A, TTiger_Data(Tiger_Data)[0, UInt32(C)        and $FF] xor
           TTiger_Data(Tiger_Data)[1, UInt32(C) shr 16 and $FF] xor
           TTiger_Data(Tiger_Data)[2,          C  shr 32 and $FF] xor
           TTiger_Data(Tiger_Data)[3, UInt32(C shr 32) shr 16 and $FF]);
    Inc(B, TTiger_Data(Tiger_Data)[3, UInt32(C) shr  8 and $FF] xor
           TTiger_Data(Tiger_Data)[2, UInt32(C) shr 24] xor
           TTiger_Data(Tiger_Data)[1, UInt32(C shr 32) shr 8 and $FF] xor
           TTiger_Data(Tiger_Data)[0, UInt32(C shr 32) shr 24]);

    if I = 1 then
      B := B shl 2 + B
    else
    begin
      if I = 2 then
        B := B shl 3 - B
      else
        B := B shl 3 + B;
    end;

    A := A xor x7;
    Dec(B, TTiger_Data(Tiger_Data)[0, UInt32(A)        and $FF] xor
           TTiger_Data(Tiger_Data)[1, UInt32(A) shr 16 and $FF] xor
           TTiger_Data(Tiger_Data)[2,          A  shr 32 and $FF] xor
           TTiger_Data(Tiger_Data)[3, UInt32(A shr 32) shr 16 and $FF]);
    Inc(C, TTiger_Data(Tiger_Data)[3, UInt32(A) shr  8 and $FF] xor
           TTiger_Data(Tiger_Data)[2, UInt32(A) shr 24] xor
           TTiger_Data(Tiger_Data)[1, UInt32(A shr 32) shr 8 and $FF] xor
           TTiger_Data(Tiger_Data)[0, UInt32(A shr 32) shr 24]);

    if I = 1 then
      C := C shl 2 + C
    else
    begin
      if I = 2 then
        C := C shl 3 - C
      else
        C := C shl 3 + C;
    end;

    T := A; A := C; C := B; B := T;
  end;

  PInt64Array(@FDigest)[0] := A xor PInt64Array(@FDigest)[0];
  PInt64Array(@FDigest)[1] := B  -  PInt64Array(@FDigest)[1];
  PInt64Array(@FDigest)[2] := C  +  PInt64Array(@FDigest)[2];
end;

class function THash_Tiger.GetMaxRounds: UInt8;
begin
  Result := cTigerMaxRounds;
end;

class function THash_Tiger.GetMinRounds: UInt8;
begin
  Result := cTigerMinRounds;
end;

class function THash_Tiger.DigestSize: UInt32;
begin
  Result := 24;
end;

{ THash_Panama }

procedure THash_Panama.DoInit;
begin
  FillChar(FLFSRBuffer, SizeOf(FLFSRBuffer), 0);
  FillChar(FDigest, SizeOf(FDigest), 0);
  FTap := 0;
end;

{$IFNDEF THash_Panama_asm}
procedure THash_Panama.DoTransform(Buffer: PUInt32Array);
var
  T0, T1, T2, T3: UInt32;
  PBufB, PTap0, PTap25: PUInt32Array;
begin
  // perform non-linearity stage (GAMMA)
  T0 := FDigest[ 0];
  T1 := FDigest[ 1];
  FDigest[ 0] := FDigest[ 0] xor (FDigest[ 1] or not FDigest[ 2]);
  FDigest[ 1] := FDigest[ 1] xor (FDigest[ 2] or not FDigest[ 3]);
  FDigest[ 2] := FDigest[ 2] xor (FDigest[ 3] or not FDigest[ 4]);
  FDigest[ 3] := FDigest[ 3] xor (FDigest[ 4] or not FDigest[ 5]);
  FDigest[ 4] := FDigest[ 4] xor (FDigest[ 5] or not FDigest[ 6]);
  FDigest[ 5] := FDigest[ 5] xor (FDigest[ 6] or not FDigest[ 7]);
  FDigest[ 6] := FDigest[ 6] xor (FDigest[ 7] or not FDigest[ 8]);
  FDigest[ 7] := FDigest[ 7] xor (FDigest[ 8] or not FDigest[ 9]);
  FDigest[ 8] := FDigest[ 8] xor (FDigest[ 9] or not FDigest[10]);
  FDigest[ 9] := FDigest[ 9] xor (FDigest[10] or not FDigest[11]);
  FDigest[10] := FDigest[10] xor (FDigest[11] or not FDigest[12]);
  FDigest[11] := FDigest[11] xor (FDigest[12] or not FDigest[13]);
  FDigest[12] := FDigest[12] xor (FDigest[13] or not FDigest[14]);
  FDigest[13] := FDigest[13] xor (FDigest[14] or not FDigest[15]);
  FDigest[14] := FDigest[14] xor (FDigest[15] or not FDigest[16]);
  FDigest[15] := FDigest[15] xor (FDigest[16] or not T0);
  FDigest[16] := FDigest[16] xor (T0 or not T1);

  // perform bit-dispersion stage (PI)
  T0 := FDigest[ 1];
  T1 := FDigest[ 7]; FDigest[ 1] := (T1 shl  1) or (T1 shr 31);
  T1 := FDigest[ 5]; FDigest[ 5] := (T0 shl 15) or (T0 shr 17);
  T0 := FDigest[ 8]; FDigest[ 8] := (T1 shl  4) or (T1 shr 28);
  T1 := FDigest[ 6]; FDigest[ 6] := (T0 shl 21) or (T0 shr 11);
  T0 := FDigest[13]; FDigest[13] := (T1 shl 27) or (T1 shr  5);
  T1 := FDigest[14]; FDigest[14] := (T0 shl  9) or (T0 shr 23);
  T0 := FDigest[ 2]; FDigest[ 2] := (T1 shl  3) or (T1 shr 29);
  T1 := FDigest[10]; FDigest[10] := (T0 shl 23) or (T0 shr  9);
  T0 := FDigest[16]; FDigest[16] := (T1 shl  8) or (T1 shr 24);
  T1 := FDigest[12]; FDigest[12] := (T0 shl 14) or (T0 shr 18);
  T0 := FDigest[ 9]; FDigest[ 9] := (T1 shl 13) or (T1 shr 19);
  T1 := FDigest[11]; FDigest[11] := (T0 shl  2) or (T0 shr 30);
  T0 := FDigest[ 4]; FDigest[ 4] := (T1 shl 10) or (T1 shr 22);
  T1 := FDigest[ 3]; FDigest[ 3] := (T0 shl  6) or (T0 shr 26);
  T0 := FDigest[15]; FDigest[15] := (T1 shl 24) or (T1 shr  8);
  FDigest[ 7] := (T0 shl 28) or (T0 shr  4);

  // LFSR emulation
  PBufB  := @FLFSRBuffer[(FTap + 16) and 31];
  FTap   := (FTap - 1) and 31;
  PTap0  := @FLFSRBuffer[FTap];
  PTap25 := @FLFSRBuffer[(FTap + 25) and 31];

  // update the LFSR buffer (LAMBDA_PUSH)
  PTap25[ 0] := PTap25[ 0] xor PTap0[ 2];
  PTap25[ 1] := PTap25[ 1] xor PTap0[ 3];
  PTap25[ 2] := PTap25[ 2] xor PTap0[ 4];
  PTap25[ 3] := PTap25[ 3] xor PTap0[ 5];
  PTap25[ 4] := PTap25[ 4] xor PTap0[ 6];
  PTap25[ 5] := PTap25[ 5] xor PTap0[ 7];
  PTap25[ 6] := PTap25[ 6] xor PTap0[ 0];
  PTap25[ 7] := PTap25[ 7] xor PTap0[ 1];
  PTap0[ 0] := PTap0[ 0] xor Buffer[ 0];
  PTap0[ 1] := PTap0[ 1] xor Buffer[ 1];
  PTap0[ 2] := PTap0[ 2] xor Buffer[ 2];
  PTap0[ 3] := PTap0[ 3] xor Buffer[ 3];
  PTap0[ 4] := PTap0[ 4] xor Buffer[ 4];
  PTap0[ 5] := PTap0[ 5] xor Buffer[ 5];
  PTap0[ 6] := PTap0[ 6] xor Buffer[ 6];
  PTap0[ 7] := PTap0[ 7] xor Buffer[ 7];

  // perform diffusion stage (THETA) + buffer injection stage (SIGMA)
  T0 := FDigest[ 0];
  T1 := FDigest[ 1];
  T2 := FDigest[ 2];
  T3 := FDigest[ 3];

  FDigest[ 0] := FDigest[ 0] xor FDigest[ 1] xor FDigest[ 4] xor 1;
  FDigest[ 1] := FDigest[ 1] xor FDigest[ 2] xor FDigest[ 5] xor Buffer[ 0];
  FDigest[ 2] := FDigest[ 2] xor FDigest[ 3] xor FDigest[ 6] xor Buffer[ 1];
  FDigest[ 3] := FDigest[ 3] xor FDigest[ 4] xor FDigest[ 7] xor Buffer[ 2];
  FDigest[ 4] := FDigest[ 4] xor FDigest[ 5] xor FDigest[ 8] xor Buffer[ 3];
  FDigest[ 5] := FDigest[ 5] xor FDigest[ 6] xor FDigest[ 9] xor Buffer[ 4];
  FDigest[ 6] := FDigest[ 6] xor FDigest[ 7] xor FDigest[10] xor Buffer[ 5];
  FDigest[ 7] := FDigest[ 7] xor FDigest[ 8] xor FDigest[11] xor Buffer[ 6];
  FDigest[ 8] := FDigest[ 8] xor FDigest[ 9] xor FDigest[12] xor Buffer[ 7];

  FDigest[ 9] := FDigest[ 9] xor FDigest[10] xor FDigest[13] xor PBufB[ 0];
  FDigest[10] := FDigest[10] xor FDigest[11] xor FDigest[14] xor PBufB[ 1];
  FDigest[11] := FDigest[11] xor FDigest[12] xor FDigest[15] xor PBufB[ 2];
  FDigest[12] := FDigest[12] xor FDigest[13] xor FDigest[16] xor PBufB[ 3];
  FDigest[13] := FDigest[13] xor FDigest[14] xor T0          xor PBufB[ 4];
  FDigest[14] := FDigest[14] xor FDigest[15] xor T1          xor PBufB[ 5];
  FDigest[15] := FDigest[15] xor FDigest[16] xor T2          xor PBufB[ 6];
  FDigest[16] := FDigest[16] xor T0          xor T3          xor PBufB[ 7];
end;
{$ENDIF !THash_Panama_asm}

procedure THash_Panama.DoDone;
begin
  if FPaddingByte = 0 then
    FPaddingByte := $01;
  FBuffer[FBufferIndex] := FPaddingByte;
  Inc(FBufferIndex);
  FillChar(FBuffer[FBufferIndex], FBufferSize - FBufferIndex, 0);
  DoTransform(Pointer(FBuffer));
  DoPull;
  FillChar(FLFSRBuffer, SizeOf(FLFSRBuffer), 0);
  FTap := 0;
end;

{$IFNDEF THash_Panama_asm}
procedure THash_Panama.DoPull;
var
  PBufL, PBufB, PTap0, PTap25: PUInt32Array;
  T0, T1, T2, T3: UInt32;
  I: Integer;
begin
  for I := 0 to 31 do
  begin
    // LFSR emulation
    PBufL := @FLFSRBuffer[(FTap +  4) and 31];
    PBufB := @FLFSRBuffer[(FTap + 16) and 31];
    FTap := (FTap - 1) and 31;
    PTap0  := @FLFSRBuffer[FTap];
    PTap25 := @FLFSRBuffer[(FTap + 25) and 31];

    // update the LFSR buffer (LAMBDA_PULL)
    PTap25[ 0] := PTap25[ 0] xor PTap0[ 2];
    PTap25[ 1] := PTap25[ 1] xor PTap0[ 3];
    PTap25[ 2] := PTap25[ 2] xor PTap0[ 4];
    PTap25[ 3] := PTap25[ 3] xor PTap0[ 5];
    PTap25[ 4] := PTap25[ 4] xor PTap0[ 6];
    PTap25[ 5] := PTap25[ 5] xor PTap0[ 7];
    PTap25[ 6] := PTap25[ 6] xor PTap0[ 0];
    PTap25[ 7] := PTap25[ 7] xor PTap0[ 1];
    PTap0[ 0] := PTap0[ 0] xor FDigest[ 1];
    PTap0[ 1] := PTap0[ 1] xor FDigest[ 2];
    PTap0[ 2] := PTap0[ 2] xor FDigest[ 3];
    PTap0[ 3] := PTap0[ 3] xor FDigest[ 4];
    PTap0[ 4] := PTap0[ 4] xor FDigest[ 5];
    PTap0[ 5] := PTap0[ 5] xor FDigest[ 6];
    PTap0[ 6] := PTap0[ 6] xor FDigest[ 7];
    PTap0[ 7] := PTap0[ 7] xor FDigest[ 8];

    // perform non-linearity stage (GAMMA)
    T0 := FDigest[ 0];
    T1 := FDigest[ 1];
    FDigest[ 0] := FDigest[ 0] xor (FDigest[ 1] or not FDigest[ 2]);
    FDigest[ 1] := FDigest[ 1] xor (FDigest[ 2] or not FDigest[ 3]);
    FDigest[ 2] := FDigest[ 2] xor (FDigest[ 3] or not FDigest[ 4]);
    FDigest[ 3] := FDigest[ 3] xor (FDigest[ 4] or not FDigest[ 5]);
    FDigest[ 4] := FDigest[ 4] xor (FDigest[ 5] or not FDigest[ 6]);
    FDigest[ 5] := FDigest[ 5] xor (FDigest[ 6] or not FDigest[ 7]);
    FDigest[ 6] := FDigest[ 6] xor (FDigest[ 7] or not FDigest[ 8]);
    FDigest[ 7] := FDigest[ 7] xor (FDigest[ 8] or not FDigest[ 9]);
    FDigest[ 8] := FDigest[ 8] xor (FDigest[ 9] or not FDigest[10]);
    FDigest[ 9] := FDigest[ 9] xor (FDigest[10] or not FDigest[11]);
    FDigest[10] := FDigest[10] xor (FDigest[11] or not FDigest[12]);
    FDigest[11] := FDigest[11] xor (FDigest[12] or not FDigest[13]);
    FDigest[12] := FDigest[12] xor (FDigest[13] or not FDigest[14]);
    FDigest[13] := FDigest[13] xor (FDigest[14] or not FDigest[15]);
    FDigest[14] := FDigest[14] xor (FDigest[15] or not FDigest[16]);
    FDigest[15] := FDigest[15] xor (FDigest[16] or not T0);
    FDigest[16] := FDigest[16] xor (T0 or not T1);

    // perform bit-dispersion stage (PI)
    T0 := FDigest[ 1];
    T1 := FDigest[ 7]; FDigest[ 1] := (T1 shl  1) or (T1 shr 31);
    T1 := FDigest[ 5]; FDigest[ 5] := (T0 shl 15) or (T0 shr 17);
    T0 := FDigest[ 8]; FDigest[ 8] := (T1 shl  4) or (T1 shr 28);
    T1 := FDigest[ 6]; FDigest[ 6] := (T0 shl 21) or (T0 shr 11);
    T0 := FDigest[13]; FDigest[13] := (T1 shl 27) or (T1 shr  5);
    T1 := FDigest[14]; FDigest[14] := (T0 shl  9) or (T0 shr 23);
    T0 := FDigest[ 2]; FDigest[ 2] := (T1 shl  3) or (T1 shr 29);
    T1 := FDigest[10]; FDigest[10] := (T0 shl 23) or (T0 shr  9);
    T0 := FDigest[16]; FDigest[16] := (T1 shl  8) or (T1 shr 24);
    T1 := FDigest[12]; FDigest[12] := (T0 shl 14) or (T0 shr 18);
    T0 := FDigest[ 9]; FDigest[ 9] := (T1 shl 13) or (T1 shr 19);
    T1 := FDigest[11]; FDigest[11] := (T0 shl  2) or (T0 shr 30);
    T0 := FDigest[ 4]; FDigest[ 4] := (T1 shl 10) or (T1 shr 22);
    T1 := FDigest[ 3]; FDigest[ 3] := (T0 shl  6) or (T0 shr 26);
    T0 := FDigest[15]; FDigest[15] := (T1 shl 24) or (T1 shr  8);
    FDigest[ 7] := (T0 shl 28) or (T0 shr  4);

    // perform diffusion stage (THETA) + buffer injection stage (SIGMA)
    T0 := FDigest[ 0];
    T1 := FDigest[ 1];
    T2 := FDigest[ 2];
    T3 := FDigest[ 3];
    FDigest[ 0] := FDigest[ 0] xor FDigest[ 1] xor FDigest[ 4] xor 1;
    FDigest[ 1] := FDigest[ 1] xor FDigest[ 2] xor FDigest[ 5] xor PBufL[ 0];
    FDigest[ 2] := FDigest[ 2] xor FDigest[ 3] xor FDigest[ 6] xor PBufL[ 1];
    FDigest[ 3] := FDigest[ 3] xor FDigest[ 4] xor FDigest[ 7] xor PBufL[ 2];
    FDigest[ 4] := FDigest[ 4] xor FDigest[ 5] xor FDigest[ 8] xor PBufL[ 3];
    FDigest[ 5] := FDigest[ 5] xor FDigest[ 6] xor FDigest[ 9] xor PBufL[ 4];
    FDigest[ 6] := FDigest[ 6] xor FDigest[ 7] xor FDigest[10] xor PBufL[ 5];
    FDigest[ 7] := FDigest[ 7] xor FDigest[ 8] xor FDigest[11] xor PBufL[ 6];
    FDigest[ 8] := FDigest[ 8] xor FDigest[ 9] xor FDigest[12] xor PBufL[ 7];
    FDigest[ 9] := FDigest[ 9] xor FDigest[10] xor FDigest[13] xor PBufB[ 0];
    FDigest[10] := FDigest[10] xor FDigest[11] xor FDigest[14] xor PBufB[ 1];
    FDigest[11] := FDigest[11] xor FDigest[12] xor FDigest[15] xor PBufB[ 2];
    FDigest[12] := FDigest[12] xor FDigest[13] xor FDigest[16] xor PBufB[ 3];
    FDigest[13] := FDigest[13] xor FDigest[14] xor T0          xor PBufB[ 4];
    FDigest[14] := FDigest[14] xor FDigest[15] xor T1          xor PBufB[ 5];
    FDigest[15] := FDigest[15] xor FDigest[16] xor T2          xor PBufB[ 6];
    FDigest[16] := FDigest[16] xor T0 xor T3                   xor PBufB[ 7];
  end;

  // move state to Digest buffer
  FDigest[0] := FDigest[ 9];
  FDigest[1] := FDigest[10];
  FDigest[2] := FDigest[11];
  FDigest[3] := FDigest[12];
  FDigest[4] := FDigest[13];
  FDigest[5] := FDigest[14];
  FDigest[6] := FDigest[15];
  FDigest[7] := FDigest[16];
end;
{$ENDIF !THash_Panama_asm}

function THash_Panama.Digest: PByteArray;
begin
  Result := @FDigest;
end;

class function THash_Panama.DigestSize: UInt32;
begin
  Result := 32;
end;

class function THash_Panama.BlockSize: UInt32;
begin
  Result := 32
end;

{ THashBaseWhirlpool }

{$IFNDEF THashBaseWhirlpool_asm}
procedure THashBaseWhirlpool.DoTransform(Buffer: PUInt32Array);
type
  PWhirlData = ^TWhirlData;
  TWhirlData = array[0..15] of UInt32;
  PWhirlTable = ^TWhirlTable;
  TWhirlTable = array[0..7, 0..511] of UInt32;

  procedure Whirl(var L: TWhirlData; const K: TWhirlData; const T: PWhirlTable);
  begin
    L[0*2+0] := T[0, ((K[ 0] shl  1) and $1fe)] xor
                T[1, ((K[14] shr  7) and $1fe)] xor
                T[2, ((K[12] shr 15) and $1fe)] xor
                T[3, ((K[10] shr 23) and $1fe)] xor
                T[4, ((K[ 9] shl  1) and $1fe)] xor
                T[5, ((K[ 7] shr  7) and $1fe)] xor
                T[6, ((K[ 5] shr 15) and $1fe)] xor
                T[7, ((K[ 3] shr 23) and $1fe)];
    L[0*2+1] := T[0, ((K[ 0] shl  1) and $1fe)+1] xor
                T[1, ((K[14] shr  7) and $1fe)+1] xor
                T[2, ((K[12] shr 15) and $1fe)+1] xor
                T[3, ((K[10] shr 23) and $1fe)+1] xor
                T[4, ((K[ 9] shl  1) and $1fe)+1] xor
                T[5, ((K[ 7] shr  7) and $1fe)+1] xor
                T[6, ((K[ 5] shr 15) and $1fe)+1] xor
                T[7, ((K[ 3] shr 23) and $1fe)+1];
    L[1*2+0] := T[0, ((K[ 2] shl  1) and $1fe)] xor
                T[1, ((K[ 0] shr  7) and $1fe)] xor
                T[2, ((K[14] shr 15) and $1fe)] xor
                T[3, ((K[12] shr 23) and $1fe)] xor
                T[4, ((K[11] shl  1) and $1fe)] xor
                T[5, ((K[ 9] shr  7) and $1fe)] xor
                T[6, ((K[ 7] shr 15) and $1fe)] xor
                T[7, ((K[ 5] shr 23) and $1fe)];
    L[1*2+1] := T[0, ((K[ 2] shl  1) and $1fe)+1] xor
                T[1, ((K[ 0] shr  7) and $1fe)+1] xor
                T[2, ((K[14] shr 15) and $1fe)+1] xor
                T[3, ((K[12] shr 23) and $1fe)+1] xor
                T[4, ((K[11] shl  1) and $1fe)+1] xor
                T[5, ((K[ 9] shr  7) and $1fe)+1] xor
                T[6, ((K[ 7] shr 15) and $1fe)+1] xor
                T[7, ((K[ 5] shr 23) and $1fe)+1];
    L[2*2+0] := T[0, ((K[ 4] shl  1) and $1fe)] xor
                T[1, ((K[ 2] shr  7) and $1fe)] xor
                T[2, ((K[ 0] shr 15) and $1fe)] xor
                T[3, ((K[14] shr 23) and $1fe)] xor
                T[4, ((K[13] shl  1) and $1fe)] xor
                T[5, ((K[11] shr  7) and $1fe)] xor
                T[6, ((K[ 9] shr 15) and $1fe)] xor
                T[7, ((K[ 7] shr 23) and $1fe)];
    L[2*2+1] := T[0, ((K[ 4] shl  1) and $1fe)+1] xor
                T[1, ((K[ 2] shr  7) and $1fe)+1] xor
                T[2, ((K[ 0] shr 15) and $1fe)+1] xor
                T[3, ((K[14] shr 23) and $1fe)+1] xor
                T[4, ((K[13] shl  1) and $1fe)+1] xor
                T[5, ((K[11] shr  7) and $1fe)+1] xor
                T[6, ((K[ 9] shr 15) and $1fe)+1] xor
                T[7, ((K[ 7] shr 23) and $1fe)+1];
    L[3*2+0] := T[0, ((K[ 6] shl  1) and $1fe)] xor
                T[1, ((K[ 4] shr  7) and $1fe)] xor
                T[2, ((K[ 2] shr 15) and $1fe)] xor
                T[3, ((K[ 0] shr 23) and $1fe)] xor
                T[4, ((K[15] shl  1) and $1fe)] xor
                T[5, ((K[13] shr  7) and $1fe)] xor
                T[6, ((K[11] shr 15) and $1fe)] xor
                T[7, ((K[ 9] shr 23) and $1fe)];
    L[3*2+1] := T[0, ((K[ 6] shl  1) and $1fe)+1] xor
                T[1, ((K[ 4] shr  7) and $1fe)+1] xor
                T[2, ((K[ 2] shr 15) and $1fe)+1] xor
                T[3, ((K[ 0] shr 23) and $1fe)+1] xor
                T[4, ((K[15] shl  1) and $1fe)+1] xor
                T[5, ((K[13] shr  7) and $1fe)+1] xor
                T[6, ((K[11] shr 15) and $1fe)+1] xor
                T[7, ((K[ 9] shr 23) and $1fe)+1];
    L[4*2+0] := T[0, ((K[ 8] shl  1) and $1fe)] xor
                T[1, ((K[ 6] shr  7) and $1fe)] xor
                T[2, ((K[ 4] shr 15) and $1fe)] xor
                T[3, ((K[ 2] shr 23) and $1fe)] xor
                T[4, ((K[ 1] shl  1) and $1fe)] xor
                T[5, ((K[15] shr  7) and $1fe)] xor
                T[6, ((K[13] shr 15) and $1fe)] xor
                T[7, ((K[11] shr 23) and $1fe)];
    L[4*2+1] := T[0, ((K[ 8] shl  1) and $1fe)+1] xor
                T[1, ((K[ 6] shr  7) and $1fe)+1] xor
                T[2, ((K[ 4] shr 15) and $1fe)+1] xor
                T[3, ((K[ 2] shr 23) and $1fe)+1] xor
                T[4, ((K[ 1] shl  1) and $1fe)+1] xor
                T[5, ((K[15] shr  7) and $1fe)+1] xor
                T[6, ((K[13] shr 15) and $1fe)+1] xor
                T[7, ((K[11] shr 23) and $1fe)+1];
    L[5*2+0] := T[0, ((K[10] shl  1) and $1fe)] xor
                T[1, ((K[ 8] shr  7) and $1fe)] xor
                T[2, ((K[ 6] shr 15) and $1fe)] xor
                T[3, ((K[ 4] shr 23) and $1fe)] xor
                T[4, ((K[ 3] shl  1) and $1fe)] xor
                T[5, ((K[ 1] shr  7) and $1fe)] xor
                T[6, ((K[15] shr 15) and $1fe)] xor
                T[7, ((K[13] shr 23) and $1fe)];
    L[5*2+1] := T[0, ((K[10] shl  1) and $1fe)+1] xor
                T[1, ((K[ 8] shr  7) and $1fe)+1] xor
                T[2, ((K[ 6] shr 15) and $1fe)+1] xor
                T[3, ((K[ 4] shr 23) and $1fe)+1] xor
                T[4, ((K[ 3] shl  1) and $1fe)+1] xor
                T[5, ((K[ 1] shr  7) and $1fe)+1] xor
                T[6, ((K[15] shr 15) and $1fe)+1] xor
                T[7, ((K[13] shr 23) and $1fe)+1];
    L[6*2+0] := T[0, ((K[12] shl  1) and $1fe)] xor
                T[1, ((K[10] shr  7) and $1fe)] xor
                T[2, ((K[ 8] shr 15) and $1fe)] xor
                T[3, ((K[ 6] shr 23) and $1fe)] xor
                T[4, ((K[ 5] shl  1) and $1fe)] xor
                T[5, ((K[ 3] shr  7) and $1fe)] xor
                T[6, ((K[ 1] shr 15) and $1fe)] xor
                T[7, ((K[15] shr 23) and $1fe)];
    L[6*2+1] := T[0, ((K[12] shl  1) and $1fe)+1] xor
                T[1, ((K[10] shr  7) and $1fe)+1] xor
                T[2, ((K[ 8] shr 15) and $1fe)+1] xor
                T[3, ((K[ 6] shr 23) and $1fe)+1] xor
                T[4, ((K[ 5] shl  1) and $1fe)+1] xor
                T[5, ((K[ 3] shr  7) and $1fe)+1] xor
                T[6, ((K[ 1] shr 15) and $1fe)+1] xor
                T[7, ((K[15] shr 23) and $1fe)+1];
    L[7*2+0] := T[0, ((K[14] shl  1) and $1fe)] xor
                T[1, ((K[12] shr  7) and $1fe)] xor
                T[2, ((K[10] shr 15) and $1fe)] xor
                T[3, ((K[ 8] shr 23) and $1fe)] xor
                T[4, ((K[ 7] shl  1) and $1fe)] xor
                T[5, ((K[ 5] shr  7) and $1fe)] xor
                T[6, ((K[ 3] shr 15) and $1fe)] xor
                T[7, ((K[ 1] shr 23) and $1fe)];
    L[7*2+1] := T[0, ((K[14] shl  1) and $1fe)+1] xor
                T[1, ((K[12] shr  7) and $1fe)+1] xor
                T[2, ((K[10] shr 15) and $1fe)+1] xor
                T[3, ((K[ 8] shr 23) and $1fe)+1] xor
                T[4, ((K[ 7] shl  1) and $1fe)+1] xor
                T[5, ((K[ 5] shr  7) and $1fe)+1] xor
                T[6, ((K[ 3] shr 15) and $1fe)+1] xor
                T[7, ((K[ 1] shr 23) and $1fe)+1];
  end;

var
  S, L, K: TWhirlData;
  I: Integer;
begin
  Assert(not Odd(Whirlpool_Rounds));

  Move(FDigest, K, SizeOf(FDigest));
  XORBuffers(FDigest, Buffer[0], SizeOf(FDigest), S);

  // iterate over all rounds
  for I := 0 to Whirlpool_Rounds div 2 - 1 do
  begin
    Whirl(L, K, FTableC);
    L[0] := L[0] xor PUInt32Array(FTableR)[I*4+0];
    L[1] := L[1] xor PUInt32Array(FTableR)[I*4+1];
    Whirl(K, S, FTableC);
    XORBuffers(L, K, SizeOf(S), S);

    Whirl(K, L, FTableC);
    K[0] := K[0] xor PUInt32Array(FTableR)[I*4+2];
    K[1] := K[1] xor PUInt32Array(FTableR)[I*4+3];
    Whirl(L, S, FTableC);
    XORBuffers(K, L, SizeOf(S), S);
  end;

  XORBuffers(S, Buffer[0], SizeOf(FDigest), S);
  XORBuffers(S, FDigest, SizeOf(FDigest), FDigest);
end;
{$ENDIF !THashBaseWhirlpool_asm}

procedure THashBaseWhirlpool.DoDone;
var
  I: Integer;
begin
  if FPaddingByte = 0 then
    FPaddingByte := $80;
  FBuffer[FBufferIndex] := FPaddingByte;
  Inc(FBufferIndex);
  if FBufferIndex > FBufferSize - 32 then
  begin
    FillChar(FBuffer[FBufferIndex], FBufferSize - FBufferIndex, 0);
    DoTransform(Pointer(FBuffer));
    FBufferIndex := 0;
  end;
  FillChar(FBuffer[FBufferIndex], FBufferSize - FBufferIndex, 0);
  for I := 31 downto 0 do
    FBuffer[63 - I] := PByteArray(@FCount)[I];
  DoTransform(Pointer(FBuffer));
end;

function THashBaseWhirlpool.Digest: PByteArray;
begin
  Result := @FDigest;
end;

class function THashBaseWhirlpool.DigestSize: UInt32;
begin
  Result := 64;
end;

class function THashBaseWhirlpool.BlockSize: UInt32;
begin
  Result := 64;
end;

{ THash_Whirlpool0 }

procedure THash_Whirlpool0.DoInit;
begin
  FillChar(FDigest, SizeOf(FDigest), 0);
  FTableC := @Whirlpool_C_U;
  FTableR := @Whirlpool_RC_U
end;

{ THash_WhirlpoolT }

procedure THash_WhirlpoolT.DoInit;
begin
  FillChar(FDigest, SizeOf(FDigest), 0);
  FTableC := @Whirlpool_C_T;
  FTableR := @Whirlpool_RC_T;
end;

{ THash_Whirlpool1_ }

procedure THash_Whirlpool1_.DoInit;
begin
  FillChar(FDigest, SizeOf(FDigest), 0);
  FTableC := @Whirlpool_C_1;
  FTableR := @Whirlpool_RC_1;
end;

{ THash_Square }

procedure THash_Square.DoInit;
begin
  FillChar(FDigest, SizeOf(FDigest), 0);
end;

{$IFNDEF THash_Square_asm}
procedure THash_Square.DoTransform(Buffer: PUInt32Array);
var
  Key: array[0..8, 0..3] of UInt32;
  A, B, C, D: UInt32;
  AA, BB, CC, DD: UInt32;
  I: Integer;
begin
  // Build and expand the Key, Digest include the Key
  Key[0, 0] := FDigest[0];
  Key[0, 1] := FDigest[1];
  Key[0, 2] := FDigest[2];
  Key[0, 3] := FDigest[3];

  for I := 1 to 8 do
  begin
    Key[I, 0] := Key[I - 1, 0] xor Key[I - 1, 3] shr 8 xor Key[I - 1, 3] shl 24 xor 1 shl (I - 1);
    Key[I, 1] := Key[I - 1, 1] xor Key[I, 0];
    Key[I, 2] := Key[I - 1, 2] xor Key[I, 1];
    Key[I, 3] := Key[I - 1, 3] xor Key[I, 2];

    Key[I - 1, 0] := Square_PHIr[0, Key[I - 1, 0]        and $FF] xor
                     Square_PHIr[1, Key[I - 1, 0] shr  8 and $FF] xor
                     Square_PHIr[2, Key[I - 1, 0] shr 16 and $FF] xor
                     Square_PHIr[3, Key[I - 1, 0] shr 24        ];
    Key[I - 1, 1] := Square_PHIr[0, Key[I - 1, 1]        and $FF] xor
                     Square_PHIr[1, Key[I - 1, 1] shr  8 and $FF] xor
                     Square_PHIr[2, Key[I - 1, 1] shr 16 and $FF] xor
                     Square_PHIr[3, Key[I - 1, 1] shr 24        ];
    Key[I - 1, 2] := Square_PHIr[0, Key[I - 1, 2]        and $FF] xor
                     Square_PHIr[1, Key[I - 1, 2] shr  8 and $FF] xor
                     Square_PHIr[2, Key[I - 1, 2] shr 16 and $FF] xor
                     Square_PHIr[3, Key[I - 1, 2] shr 24        ];
    Key[I - 1, 3] := Square_PHIr[0, Key[I - 1, 3]        and $FF] xor
                     Square_PHIr[1, Key[I - 1, 3] shr  8 and $FF] xor
                     Square_PHIr[2, Key[I - 1, 3] shr 16 and $FF] xor
                     Square_PHIr[3, Key[I - 1, 3] shr 24        ];
  end;

  // Encrypt begin here, same TCipher_Square.Encode
  A := Buffer[0] xor Key[0, 0];
  B := Buffer[1] xor Key[0, 1];
  C := Buffer[2] xor Key[0, 2];
  D := Buffer[3] xor Key[0, 3];

  for I := 0 to 6 do
  begin
    AA := Square_TE[0, A        and $FF] xor
          Square_TE[1, B        and $FF] xor
          Square_TE[2, C        and $FF] xor
          Square_TE[3, D        and $FF] xor Key[I + 1, 0];
    BB := Square_TE[0, A shr  8 and $FF] xor
          Square_TE[1, B shr  8 and $FF] xor
          Square_TE[2, C shr  8 and $FF] xor
          Square_TE[3, D shr  8 and $FF] xor Key[I + 1, 1];
    CC := Square_TE[0, A shr 16 and $FF] xor
          Square_TE[1, B shr 16 and $FF] xor
          Square_TE[2, C shr 16 and $FF] xor
          Square_TE[3, D shr 16 and $FF] xor Key[I + 1, 2];
    DD := Square_TE[0, A shr 24        ] xor
          Square_TE[1, B shr 24        ] xor
          Square_TE[2, C shr 24        ] xor
          Square_TE[3, D shr 24        ] xor Key[I + 1, 3];

    A := AA; B := BB; C := CC; D := DD;
  end;

  FDigest[0] := Buffer[0] xor
                Square_SEint[A        and $FF]        xor
                Square_SEint[B        and $FF] shl  8 xor
                Square_SEint[C        and $FF] shl 16 xor
                Square_SEint[D        and $FF] shl 24 xor Key[8, 0];
  FDigest[1] := Buffer[1] xor
                Square_SEint[A shr  8 and $FF]        xor
                Square_SEint[B shr  8 and $FF] shl  8 xor
                Square_SEint[C shr  8 and $FF] shl 16 xor
                Square_SEint[D shr  8 and $FF] shl 24 xor Key[8, 1];
  FDigest[2] := Buffer[2] xor
                Square_SEint[A shr 16 and $FF]        xor
                Square_SEint[B shr 16 and $FF] shl  8 xor
                Square_SEint[C shr 16 and $FF] shl 16 xor
                Square_SEint[D shr 16 and $FF] shl 24 xor Key[8, 2];
  FDigest[3] := Buffer[3] xor
                Square_SEint[A shr 24        ]        xor
                Square_SEint[B shr 24        ] shl  8 xor
                Square_SEint[C shr 24        ] shl 16 xor
                Square_SEint[D shr 24        ] shl 24 xor Key[8, 3];
end;
{$ENDIF !THash_Square_asm}

procedure THash_Square.DoDone;
var
  I: Integer;
begin
  if FPaddingByte = 0 then
    FPaddingByte := $80;
  FBuffer[FBufferIndex] := FPaddingByte;
  Inc(FBufferIndex);
  if FBufferIndex > FBufferSize - 8 then
  begin
    FillChar(FBuffer[FBufferIndex], FBufferSize - FBufferIndex, 0);
    DoTransform(Pointer(FBuffer));
    FBufferIndex := 0;
  end;
  FillChar(FBuffer[FBufferIndex], FBufferSize - FBufferIndex, 0);
  for I := 7 downto 0 do
    FBuffer[15 - I] := PByteArray(@FCount[0])[I];
  DoTransform(Pointer(FBuffer));
end;

function THash_Square.Digest: PByteArray;
begin
  Result := @FDigest;
end;

class function THash_Square.DigestSize: UInt32;
begin
  Result := 16;
end;

class function THash_Square.BlockSize: UInt32;
begin
  Result := 16;
end;

{ THashBaseSnefru }

procedure THashBaseSnefru.SetRounds(Value: Integer);
begin
  if (Value < 2) or (Value > 8) then
    Value := 8;
  FRounds := Value;
end;

procedure THashBaseSnefru.DoInit;
begin
  FillChar(FDigest, SizeOf(FDigest), 0);
  SetRounds(FRounds);
end;

class function THashBaseSnefru.GetMaxRounds: UInt8;
begin
  Result := 8;
end;

class function THashBaseSnefru.GetMinRounds: UInt8;
begin
  Result := 2;
end;

procedure THashBaseSnefru.DoDone;
begin
  if FBufferIndex > 0 then
  begin
    FillChar(FBuffer[FBufferIndex], FBufferSize - FBufferIndex, 0);
    DoTransform(Pointer(FBuffer));
    FBufferIndex := 0;
  end;
  FillChar(FBuffer[FBufferIndex], FBufferSize - FBufferIndex, 0);
  PUInt32(@FBuffer[FBufferSize - 8])^ := SwapUInt32(FCount[1]);
  PUInt32(@FBuffer[FBufferSize - 4])^ := SwapUInt32(FCount[0]);
  DoTransform(Pointer(FBuffer));
  SwapUInt32Buffer(FDigest, FDigest, 8);
end;

function THashBaseSnefru.Digest: PByteArray;
begin
  Result := @FDigest;
end;

{ THash_Snefru128 }

{$IFNDEF THash_Snefru128_asm}
procedure THash_Snefru128.DoTransform(Buffer: PUInt32Array);
const
  ShiftTable: array[0..3] of Integer = (16, 8, 16, 24);
var
  I, Index, ByteInWord, T, N, S, S0, S1: UInt32;
  D, Box0, Box1: PUInt32Array;
begin
  D := @FDigest;
  SwapUInt32Buffer(Buffer[0], D[4], 12);
  Move(D[0], D[16], 16);
  Box0 := @Snefru_Data[0];
  Box1 := @Snefru_Data[1];
  for Index := 0 to FRounds - 1 do
  begin
    for ByteInWord := 0 to 3 do
    begin
      I := 0;
      N := D[0];
      while I < 16 do
      begin
        S0 := Box0[N and $FF];
        T := (I +  1) and 15;    N := D[T] xor S0; D[T] := N;
        T := (I + 15) and 15; D[T] := D[T] xor S0;
        S1 := Box0[N and $FF];
        T := (I +  2) and 15;    N := D[T] xor S1; D[T] := N;
        T := (I + 16) and 15; D[T] := D[T] xor S1;
        S0 := Box1[N and $FF];
        T := (I +  3) and 15;    N := D[T] xor S0; D[T] := N;
        T := (I + 17) and 15; D[T] := D[T] xor S0;
        S1 := Box1[N and $FF];
        T := (I +  4) and 15;    N := D[T] xor S1; D[T] := N;
        T := (I + 18) and 15; D[T] := D[T] xor S1;
        Inc(I, 4);
      end;
      T := ShiftTable[ByteInWord];
      S := 32 - T;
      for I := 0 to 15 do
        D[I] := D[I] shr T or D[I] shl S;
    end;
    Box0 := @Box0[512];
    Box1 := @Box1[512];
  end;
  for I := 0 to 3 do
    D[I] := D[I + 16] xor D[15 - I];
end;
{$ENDIF !THash_Snefru128_asm}

class function THash_Snefru128.DigestSize: UInt32;
begin
  Result := 16;
end;

class function THash_Snefru128.BlockSize: UInt32;
begin
  Result := 48
end;

{ THash_Snefru256 }

{$IFNDEF THash_Snefru256_asm}
procedure THash_Snefru256.DoTransform(Buffer: PUInt32Array);
const
  ShiftTable: array[0..3] of Integer = (16, 8, 16, 24);
var
  I, Index, ByteInWord, T, N, S, S0, S1: UInt32;
  D, Box0, Box1: PUInt32Array;
begin
  D := @FDigest;
  SwapUInt32Buffer(Buffer[0], D[8], 8);
  Move(D[0], D[16], 32);
  Box0 := @Snefru_Data[0];
  Box1 := @Snefru_Data[1];
  for Index := 0 to FRounds - 1 do
  begin
    for ByteInWord := 0 to 3 do
    begin
      I := 0;
      N := D[0];
      while I < 16 do
      begin
        S0 := Box0[N and $FF];
        T := (I +  1) and 15;    N := D[T] xor S0; D[T] := N;
        T := (I + 15) and 15; D[T] := D[T] xor S0;
        S1 := Box0[N and $FF];
        T := (I +  2) and 15;    N := D[T] xor S1; D[T] := N;
        T := (I + 16) and 15; D[T] := D[T] xor S1;
        S0 := Box1[N and $FF];
        T := (I +  3) and 15;    N := D[T] xor S0; D[T] := N;
        T := (I + 17) and 15; D[T] := D[T] xor S0;
        S1 := Box1[N and $FF];
        T := (I +  4) and 15;    N := D[T] xor S1; D[T] := N;
        T := (I + 18) and 15; D[T] := D[T] xor S1;
        Inc(I, 4);
      end;
      T := ShiftTable[ByteInWord];
      S := 32 - T;
      for I := 0 to 15 do
        D[I] := D[I] shr T or D[I] shl S;
    end;
    Box0 := @Box0[512];
    Box1 := @Box1[512];
  end;
  for I := 0 to 7 do
    D[I] := D[I + 16] xor D[15 - I];
end;
{$ENDIF !THash_Snefru256_asm}

class function THash_Snefru256.DigestSize: UInt32;
begin
  Result := 32;
end;

class function THash_Snefru256.BlockSize: UInt32;
begin
  Result := 32
end;

{ THash_Sapphire }

procedure THash_Sapphire.DoInit;
var
  I: Integer;
begin
  FillChar(FDigest, SizeOf(FDigest), 0);
  FRotor := 1;
  FRatchet := 3;
  FAvalanche := 5;
  FPlain := 7;
  FCipher := 11;
  for I := 0 to 255 do
    FCards[I] := 255 - I;
end;

procedure THash_Sapphire.DoTransform(Buffer: PUInt32Array);
begin
  // Empty on purpose: the base class for the hashes declares an abstract
  // DoTransform method and not providing an override for it would cause a
  // compiler warning
end;

procedure THash_Sapphire.SetDigestSize(Value: UInt8);
begin
  if (Value >= 1) and (Value <= 64) then
    FDigestSize := Value
  else
    FDigestSize := DigestSize;
end;

procedure THash_Sapphire.DoDone;
var
  I: Integer;
begin
  for I := 255 downto 0 do
    Calc(I, 1);
  for I := 0 to DigestSize - 1 do
  begin
    Calc(#0#0, 1);
    PByteArray(@FDigest)[I] := FCipher;
  end;
end;

{$IFNDEF THash_Sapphire_asm}
procedure THash_Sapphire.Calc(const Data; DataSize: Integer);
var
  Cipher, Ratchet, Rotor, Plain, Avalanche, T: UInt32;
  D: PByte;
begin
  D         := @Data;
  Cipher    := FCipher;
  Ratchet   := FRatchet;
  Rotor     := FRotor;
  Plain     := FPlain;
  Avalanche := FAvalanche;

  while DataSize > 0 do
  begin
    Dec(DataSize);
    Ratchet := (Ratchet + FCards[Rotor]) and $FF;
    Rotor := (Rotor + 1) and $FF;
    T := FCards[Cipher];
    FCards[Cipher] := FCards[Ratchet];
    FCards[Ratchet] := FCards[Plain];
    FCards[Plain] := FCards[Rotor];
    FCards[Rotor] := T;
    Avalanche := (Avalanche + FCards[T]) and $FF;
    T := (FCards[Plain] + FCards[Cipher] + FCards[Avalanche]) and $FF;
    Plain := D^; Inc(D);
    Cipher := Plain xor FCards[FCards[T]] xor FCards[(FCards[Ratchet] + FCards[Rotor]) and $FF];
  end;

  FCipher    := Cipher;
  FRatchet   := Ratchet;
  FRotor     := Rotor;
  FPlain     := Plain;
  FAvalanche := Avalanche;
end;
{$ENDIF !THash_Sapphire_asm}

function THash_Sapphire.Digest: PByteArray;
begin
  Result := @FDigest;
end;

function THash_Sapphire.DigestAsBytes: TBytes;
var
  Size: Integer;
begin
  if FDigestSize > 0 then
    Size := FDigestSize
  else
    Size := DigestSize;

  SetLength(Result, Size);
  if Size <> 0 then
    Move(FDigest, Result[0], Size);
end;

class function THash_Sapphire.DigestSize: UInt32;
begin
  Result := 64;
end;

class function THash_Sapphire.BlockSize: UInt32;
begin
  Result := 1;
end;

{$IFDEF RESTORE_RANGECHECKS}{$R+}{$ENDIF}
{$IFDEF RESTORE_OVERFLOWCHECKS}{$Q+}{$ENDIF}

{ THash_SHA3_224 }

class function THash_SHA3_224.BlockSize: UInt32;
begin
  Result := 144;
end;

class function THash_SHA3_224.DigestSize: UInt32;
begin
  Result := 28;
end;

procedure THash_SHA3_224.DoInit;
begin
  inherited;

  InitSponge(1152,  448);
  FSpongeState.FixedOutputLength := 224;
end;

{ THash_SHA3_256 }

class function THash_SHA3_256.BlockSize: UInt32;
begin
  Result := 136;
end;

class function THash_SHA3_256.DigestSize: UInt32;
begin
  Result := 32;
end;

procedure THash_SHA3_256.DoInit;
begin
  inherited;

  InitSponge(1088,  512);
  FSpongeState.fixedOutputLength := 256;
end;

{ THash_SHA3_384 }

class function THash_SHA3_384.BlockSize: UInt32;
begin
  Result := 104;
end;

class function THash_SHA3_384.DigestSize: UInt32;
begin
  Result := 48;
end;

procedure THash_SHA3_384.DoInit;
begin
  inherited;

  InitSponge(832,  768);
  FSpongeState.fixedOutputLength := 384;
end;

{ THash_SHA3_512 }

class function THash_SHA3_512.BlockSize: UInt32;
begin
  Result := 72;
end;

class function THash_SHA3_512.DigestSize: UInt32;
begin
  Result := 64;
end;

procedure THash_SHA3_512.DoInit;
begin
  inherited;

  InitSponge(576, 1024);
  FSpongeState.fixedOutputLength := 512;
end;

{ THash_SHA3Base }

procedure THash_SHA3Base.InitSponge(Rate, Capacity: UInt16);
begin
  // This is the only place where state.error is reset to 0 = SUCCESS
  FillChar(FSpongeState, SizeOf(FSpongeState), 0);

  if (Rate + Capacity <> 1600) or (Rate = 0) or (Rate >= 1600) or
     ((Rate and 63) <> 0) then
    raise EDECHashException.CreateFmt(sHashInitFailure, ['SHA3',
                                                         'rate: ' + IntToStr(Rate) +
                                                         ' capacity: ' + IntToStr(Capacity)]);

  FSpongeState.Rate     := Rate;
  FSpongeState.Capacity := Capacity;
end;

procedure THash_SHA3Base.KeccakAbsorb(var state: TState_B; data: PUInt64; laneCount: Integer);
begin
   XORIntoState(TState_L(state), data, laneCount);
   KeccakPermutation(TState_L(state));
end;

{$IFDEF PUREPASCAL}
function THash_SHA3Base.RotL(const x: UInt64; c: Integer): UInt64;
begin
  Result := (x shl c) or (x shr (64-c));
end;

function THash_SHA3Base.RotL1(var x: UInt64): UInt64;
begin
  Result := (x shl 1) or (x shr (64-1));
end;

procedure THash_SHA3Base.KeccakPermutation(var state: TState_L);
var
   A : PUInt64Array;
   B : array[0..24] of UInt64;
   C0, C1, C2, C3, C4, D0, D1, D2, D3, D4: UInt64;
   i : Integer;

begin
   A := PUInt64Array(@state);
   for i := 0 to 23 do begin
      C0 := A[00] xor A[05] xor A[10] xor A[15] xor A[20];
      C1 := A[01] xor A[06] xor A[11] xor A[16] xor A[21];
      C2 := A[02] xor A[07] xor A[12] xor A[17] xor A[22];
      C3 := A[03] xor A[08] xor A[13] xor A[18] xor A[23];
      C4 := A[04] xor A[09] xor A[14] xor A[19] xor A[24];

      D0 := RotL1(C0) xor C3;
      D1 := RotL1(C1) xor C4;
      D2 := RotL1(C2) xor C0;
      D3 := RotL1(C3) xor C1;
      D4 := RotL1(C4) xor C2;

      B[00] := A[00] xor D1;
      B[01] := RotL(A[06] xor D2, 44);
      B[02] := RotL(A[12] xor D3, 43);
      B[03] := RotL(A[18] xor D4, 21);
      B[04] := RotL(A[24] xor D0, 14);
      B[05] := RotL(A[03] xor D4, 28);
      B[06] := RotL(A[09] xor D0, 20);
      B[07] := RotL(A[10] xor D1, 3);
      B[08] := RotL(A[16] xor D2, 45);
      B[09] := RotL(A[22] xor D3, 61);
      B[10] := RotL(A[01] xor D2, 1);
      B[11] := RotL(A[07] xor D3, 6);
      B[12] := RotL(A[13] xor D4, 25);
      B[13] := RotL(A[19] xor D0, 8);
      B[14] := RotL(A[20] xor D1, 18);
      B[15] := RotL(A[04] xor D0, 27);
      B[16] := RotL(A[05] xor D1, 36);
      B[17] := RotL(A[11] xor D2, 10);
      B[18] := RotL(A[17] xor D3, 15);
      B[19] := RotL(A[23] xor D4, 56);
      B[20] := RotL(A[02] xor D3, 62);
      B[21] := RotL(A[08] xor D4, 55);
      B[22] := RotL(A[14] xor D0, 39);
      B[23] := RotL(A[15] xor D1, 41);
      B[24] := RotL(A[21] xor D2, 2);

      A[00] := B[00] xor ((not B[01]) and B[02]);
      A[01] := B[01] xor ((not B[02]) and B[03]);
      A[02] := B[02] xor ((not B[03]) and B[04]);
      A[03] := B[03] xor ((not B[04]) and B[00]);
      A[04] := B[04] xor ((not B[00]) and B[01]);
      A[05] := B[05] xor ((not B[06]) and B[07]);
      A[06] := B[06] xor ((not B[07]) and B[08]);
      A[07] := B[07] xor ((not B[08]) and B[09]);
      A[08] := B[08] xor ((not B[09]) and B[05]);
      A[09] := B[09] xor ((not B[05]) and B[06]);
      A[10] := B[10] xor ((not B[11]) and B[12]);
      A[11] := B[11] xor ((not B[12]) and B[13]);
      A[12] := B[12] xor ((not B[13]) and B[14]);
      A[13] := B[13] xor ((not B[14]) and B[10]);
      A[14] := B[14] xor ((not B[10]) and B[11]);
      A[15] := B[15] xor ((not B[16]) and B[17]);
      A[16] := B[16] xor ((not B[17]) and B[18]);
      A[17] := B[17] xor ((not B[18]) and B[19]);
      A[18] := B[18] xor ((not B[19]) and B[15]);
      A[19] := B[19] xor ((not B[15]) and B[16]);
      A[20] := B[20] xor ((not B[21]) and B[22]);
      A[21] := B[21] xor ((not B[22]) and B[23]);
      A[22] := B[22] xor ((not B[23]) and B[24]);
      A[23] := B[23] xor ((not B[24]) and B[20]);
      A[24] := B[24] xor ((not B[20]) and B[21]);

      A[00] := A[00] xor cRoundConstants[i];
   end;
end;
{$ELSE}
// Must be procedural as otherwise the parameters get passed in different
// CPU registers and the complete ASM code would have needed to be rewritten.
procedure KeccakPermutationKernel(B, A, C : Pointer);
asm
  {$IFDEF X86ASM}
    {$INCLUDE DECHash.sha3_mmx.inc}
  {$ELSE}
    {$INCLUDE DECHash.sha3_x64.inc}
  {$ENDIF}
end;

procedure THash_SHA3Base.KeccakPermutation(var state: TState_L);
var
   A : PUInt64Array;
   B : array [0..24] of UInt64;
   C : array [0..4] of UInt64;
   i : Integer;

  {$IFDEF X86ASM}
  procedure EMMS;
  asm
    // This operation marks the x87 FPU data registers (which are aliased to the
    // MMX technology registers) as available for use by x87 FPU floating-point
    // instructions.
    emms
  end;
  {$ENDIF}

begin
   A := PUInt64Array(@state);
   for i:=0 to 23 do begin
      KeccakPermutationKernel(@B, A, @C);
      A[00] := A[00] xor cRoundConstants[i];
   end;

   {$IFDEF X86ASM}
   EMMS;
   {$ENDIF}
end;
{$ENDIF}

procedure THash_SHA3Base.PadAndSwitchToSqueezingPhase;
var
  i: integer;
begin
  // Note: the bits are numbered from 0 = LSB to 7 = MSB
  if (FSpongeState.BitsInQueue + 1 = FSpongeState.Rate) then
  begin
    i := FSpongeState.BitsInQueue div 8;
    FSpongeState.DataQueue[i] := FSpongeState.DataQueue[i] or
                                 (1 shl (FSpongeState.BitsInQueue and 7));
    AbsorbQueue;
    FillChar(FSpongeState.DataQueue, FSpongeState.Rate div 8, 0);
  end
  else
  begin
    i := FSpongeState.BitsInQueue div 8;
    FillChar(FSpongeState.DataQueue[(FSpongeState.BitsInQueue+7) div 8],
             FSpongeState.Rate div 8 - (FSpongeState.BitsInQueue+7) div 8, 0);
    FSpongeState.DataQueue[i] := FSpongeState.DataQueue[i] or
                                 (1 shl (FSpongeState.BitsInQueue and 7));
  end;

  i := (FSpongeState.Rate-1) div 8;
  FSpongeState.DataQueue[i] := FSpongeState.DataQueue[i] or
                               (1 shl ((FSpongeState.Rate-1) and 7));
  AbsorbQueue;
  ExtractFromState(@FSpongeState.DataQueue,
                   TState_L(FSpongeState.State),
                   FSpongeState.Rate div 64);
  FSpongeState.bitsAvailableForSqueezing := FSpongeState.Rate;
  FSpongeState.SqueezeActive := true;
end;

procedure THash_SHA3Base.Squeeze(var Output: TSHA3Digest; OutputLength: Int32);
var
  i            : Int32;
  PartialBlock : Int16;
begin
  if not FSpongeState.SqueezeActive then
    PadAndSwitchToSqueezingPhase;

  // Only multiple of 8 bits are allowed, truncation must be done at user level
  if OutputLength and 7 <> 0 then
    raise EDECHashException.CreateFmt(aSHA3AbsorbFailure,
                                 [OutputLength, 'true']);

  i := 0;
  while i < OutputLength do
  begin
    if FSpongeState.bitsAvailableForSqueezing = 0 then
    begin
      KeccakPermutation(TState_L(FSpongeState.State));
      ExtractFromState(@FSpongeState.DataQueue, TState_L(FSpongeState.State),
                       FSpongeState.Rate div 64);
      FSpongeState.bitsAvailableForSqueezing := FSpongeState.Rate;
    end;

    PartialBlock := FSpongeState.bitsAvailableForSqueezing;
    if PartialBlock > OutputLength - i then
      PartialBlock := OutputLength - i;

    move(FSpongeState.DataQueue[(FSpongeState.Rate - FSpongeState.bitsAvailableForSqueezing) div 8],
         output[i div 8], PartialBlock div 8);
    dec(FSpongeState.bitsAvailableForSqueezing, PartialBlock);
    inc(i, PartialBlock);
  end;
end;

procedure THash_SHA3Base.XORIntoState(var state: TState_L; pI: PUInt64; laneCount: Integer);
var
   pS: PUInt64;
   i: Integer;
begin
   pS := @state[0];
   for i:=laneCount-1 downto 0 do begin
      pS^ := pS^ xor pI^;
      Inc(pI);
      Inc(pS);
   end;
end;


procedure THash_SHA3Base.Absorb(Data: PBABytes; DatabitLen: Int32);
var
  i, j, wholeBlocks, partialBlock: Integer;
  partialByte: Integer;
  curData: PUInt64;
begin
  // if a number of bits which cannot be divided by 8 without reminder is in the
  // queue or algorithm is already in squeezing state
  if (FSpongeState.BitsInQueue and 7 <> 0) or FSpongeState.SqueezeActive then
  begin
    raise EDECHashException.CreateFmt(aSHA3AbsorbFailure,
                                     [FSpongeState.BitsInQueue,
                                      BoolToStr(FSpongeState.SqueezeActive, true)]);
  end;

  i := 0;

  while i < databitlen do
  begin
     if ((FSpongeState.BitsInQueue = 0) and (databitlen >= FSpongeState.Rate) and
        (i <= (databitlen - FSpongeState.Rate))) then
     begin
       wholeBlocks := (databitlen-i) div FSpongeState.Rate;
       curData := @data^[i div 8];
       j := 0;
       while j < wholeBlocks do
       begin
         KeccakAbsorb(FSpongeState.State, curData, FSpongeState.Rate div 64);
         Inc(j);
         Inc(PByte(curData), FSpongeState.Rate div 8);
       end;
       Inc(i, wholeBlocks * FSpongeState.Rate);
     end
     else
     begin
       partialBlock := databitlen - i;
       if partialBlock + FSpongeState.BitsInQueue > FSpongeState.Rate then
         partialBlock := FSpongeState.Rate - FSpongeState.BitsInQueue;

       partialByte := partialBlock and 7;
       Dec(partialBlock, partialByte);
       Move(data^[i div 8], FSpongeState.DataQueue[FSpongeState.BitsInQueue div 8], partialBlock div 8);
       Inc(FSpongeState.BitsInQueue, partialBlock);
       Inc(i, partialBlock);
       if FSpongeState.BitsInQueue=FSpongeState.Rate then
          AbsorbQueue;

       if partialByte > 0 then
       begin
         FSpongeState.DataQueue[FSpongeState.BitsInQueue div 8] :=
           data^[i div 8] and ((1 shl partialByte)-1);

         Inc(FSpongeState.BitsInQueue, partialByte);
         Inc(i, partialByte);
       end;
     end;
  end;
end;

procedure THash_SHA3Base.AbsorbQueue;
begin
  // state.bitsInQueue is assumed to be equal to state.rat
  KeccakAbsorb(FSpongeState.State, @FSpongeState.DataQueue, FSpongeState.Rate div 64);
  FSpongeState.BitsInQueue := 0;
end;

procedure THash_SHA3Base.Calc(const Data; DataSize: Integer);
var
  DataPtr   : PBABytes;
  RoundSize : UInt32;
const
  // Maximum number of bytes one can process in one round
  MaxRoundSize = MaxInt div 8;
begin
  // due to the way the inherited calc is constructed it must not be called here!
  if (DataSize > 0) then
  begin
    DataPtr := PBABytes(@Data);

    while (UInt32(DataSize) > 0) do
    begin
      RoundSize := DataSize;
      if (RoundSize > MaxRoundSize) then
        RoundSize := MaxRoundSize;

      Absorb(DataPtr, RoundSize * 8);
      Dec(DataSize, RoundSize);
      Inc(DataPtr, RoundSize);
    end;

  end;
end;

function THash_SHA3Base.Digest: PByteArray;
begin
  Result := @FDigest;
end;

procedure THash_SHA3Base.DoDone;
begin
  FinalBit_LSB(FFinalByte, FFinalByteLength, FDigest);
end;

procedure THash_SHA3Base.DoInit;
begin
  inherited;

  FillChar(FDIgest[0], Length(FDigest), 0);
end;

procedure THash_SHA3Base.DoUpdate(Data: Pointer; DataBitLen: Int32);
var
  LastByte: Byte;
begin
  // No partial byte
  if DataBitLen and 7 = 0 then
    Absorb(Data, DataBitLen)
  else
  begin
    // Data contains a partial byte. Calculate the whole bytes first then the
    // partial one.
    Absorb(Data, DataBitLen - (DataBitLen and 7));

    // Align the last partial byte to the least significant bits
    LastByte := PBABytes(Data)^[DataBitLen div 8] shr (8 - (DataBitLen and 7));
    Absorb(@LastByte, DataBitLen and 7);
  end;
end;

procedure THash_SHA3Base.ExtractFromState(outp: Pointer; const state: TState_L; laneCount: Integer);
var
   pI, pS: PUInt64;
   i: Integer;
begin
   pI := outp;
   pS := @state[0];
   for i := laneCount - 1 downto 0 do
   begin
      pI^ := pS^;
      Inc(pI);
      Inc(pS);
   end;
end;

procedure THash_SHA3Base.FinalBit_LSB(Bits: Byte; Bitlen: UInt16;
                                     var Hashvalue: TSHA3Digest);
var
  WorkingBitLen : Int16;
  lw : UInt16;
begin
  // normalize Bitlen and Bits (zero high bits)
  Bitlen := Bitlen and 7;
  if Bitlen = 0 then
    lw := 0
  else
    lw := Bits and pred(word(1) shl Bitlen);

  // 'append' (in LSB language) the domain separation bits
  if (FSpongeState.FixedOutputLength = 0) then
  begin
    lw := lw or (word($F) shl Bitlen);
    WorkingBitLen := Bitlen+4;
  end
  else
  begin
    // SHA3: append two bits 01
    lw := lw or (word($2) shl Bitlen);
    WorkingBitLen := Bitlen+2;
  end;

  // update state with final bits
  if WorkingBitLen < 9 then
  begin
    // 0..8 bits, one call to update
    lw := lw shl (8-WorkingBitLen);
    DoUpdate(@lw, WorkingBitLen);
    // squeeze the digits from the sponge
    Squeeze(Hashvalue, FSpongeState.FixedOutputLength);
  end
  else
  begin
    // More than 8 bits, first a regular update with low byte
    DoUpdate(@lw, 8);

    // Finally update remaining last bits
    dec(WorkingBitLen,8);
    lw := lw shr WorkingBitLen;
    DoUpdate(@lw, WorkingBitLen);

    Squeeze(Hashvalue, FSpongeState.FixedOutputLength);
  end;
end;

procedure THash_SHA3Base.DoTransform(Buffer: PUInt32Array);
begin
// Empty on purpose as calculation is implemented differently for SHA3. Needed
// to suppress the compiler warning that a class with an abstract method is created
end;

initialization
  // Define the has returned by ValidHash if passing nil as parameter
  SetDefaultHashClass(THash_SHA256);

  {$IFNDEF ManualRegisterClasses}
  THash_MD2.RegisterClass(TDECHash.ClassList);
  THash_MD4.RegisterClass(TDECHash.ClassList);
  THash_MD5.RegisterClass(TDECHash.ClassList);
  THash_RipeMD128.RegisterClass(TDECHash.ClassList);
  THash_RipeMD160.RegisterClass(TDECHash.ClassList);
  THash_RipeMD256.RegisterClass(TDECHash.ClassList);
  THash_RipeMD320.RegisterClass(TDECHash.ClassList);
  THash_SHA0.RegisterClass(TDECHash.ClassList);
  THash_SHA1.RegisterClass(TDECHash.ClassList);
  THash_SHA224.RegisterClass(TDECHash.ClassList);
  THash_SHA256.RegisterClass(TDECHash.ClassList);
  THash_SHA384.RegisterClass(TDECHash.ClassList);
  THash_SHA512.RegisterClass(TDECHash.ClassList);
  THash_SHA3_224.RegisterClass(TDECHash.ClassList);
  THash_SHA3_256.RegisterClass(TDECHash.ClassList);
  THash_SHA3_384.RegisterClass(TDECHash.ClassList);
  THash_SHA3_512.RegisterClass(TDECHash.ClassList);
  THash_Haval128.RegisterClass(TDECHash.ClassList);
  THash_Haval160.RegisterClass(TDECHash.ClassList);
  THash_Haval192.RegisterClass(TDECHash.ClassList);
  THash_Haval224.RegisterClass(TDECHash.ClassList);
  THash_Haval256.RegisterClass(TDECHash.ClassList);
  THash_Tiger.RegisterClass(TDECHash.ClassList);
  THash_Panama.RegisterClass(TDECHash.ClassList);

  {$IFDEF OLD_WHIRLPOOL_NAMES}
  THash_Whirlpool.RegisterClass(TDECHash.ClassList);
  THash_Whirlpool1.RegisterClass(TDECHash.ClassList);
  THash_Whirlpool1New.RegisterClass(TDECHash.ClassList);
  {$ELSE}
  THash_Whirlpool1.RegisterClass(TDECHash.ClassList);
  {$ENDIF}

  THash_Whirlpool0.RegisterClass(TDECHash.ClassList);
  THash_WhirlpoolT.RegisterClass(TDECHash.ClassList);

  THash_Square.RegisterClass(TDECHash.ClassList);
  THash_Snefru128.RegisterClass(TDECHash.ClassList);
  THash_Snefru256.RegisterClass(TDECHash.ClassList);
  THash_Sapphire.RegisterClass(TDECHash.ClassList);

  {$IFDEF OLD_SHA_NAME}
  THash_SHA.RegisterClass(TDECHash.ClassList);
  {$ENDIF}

  {$ENDIF}

finalization
  // No need to unregister the hash classes, as the list is being freed
  // in finalization of DECBaseClass unit

end.
