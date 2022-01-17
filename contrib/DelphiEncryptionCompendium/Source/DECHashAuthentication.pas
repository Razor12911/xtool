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
///   Unit containing all the KDF, MGF, HMAC and PBKDF2 algorithms
/// </summary>
unit DECHashAuthentication;

interface

uses
  System.SysUtils, DECHashBase;

type
  /// <summary>
  ///   Meta class for all the hashing classes in order to support the
  ///   registration mechanism
  /// </summary>
  TDECHashAuthenticationClass = class of TDECHashAuthentication;

  /// <summary>
  ///   Type of the KDF variant
  /// </summary>
  TKDFType = (ktKDF1, ktKDF2, ktKDF3);

  /// <summary>
  ///   Class containing all the KDF, MGF, HMAC and PBKDF2 algorithms
  /// </summary>
  TDECHashAuthentication = class(TDECHash)
  strict private
    /// <summary>
    ///   Key deviation algorithm to derrive keys from other keys.
    ///   IEEE P1363 Working Group, ISO 18033-2:2004
    ///   This is either KDF1 or KDF2 depending on KDFType
    /// </summary>
    /// <param name="Data">
    ///   Source data from which the new key shall be derrived.
    /// </param>
    /// <param name="DataSize">
    ///   Size in bytes of the source data passed.
    /// </param>
    /// <param name="Seed">
    ///   Start value for pseudo random number generator
    /// </param>
    /// <param name="SeedSize">
    ///   Size of the seed in byte.
    /// </param>
    /// <param name="MaskSize">
    ///   Size of the generated output in byte
    /// </param>
    /// <param name="KDFType">
    ///   Type of the algorithm: 1 = KDF1, 2 = KDF2 and 3 = KDF 3
    /// </param>
    /// <returns>
    ///   Returns the new derrived key.
    /// </returns>
    class function KDFInternal(const Data; DataSize: Integer; const Seed;
                               SeedSize, MaskSize: Integer; KDFType: TKDFType): TBytes; inline;
  public
    /// <summary>
    ///   Detects whether the given hash class is one particularily suited
    ///   for storing hashes of passwords
    /// </summary>
    /// <returns>
    ///   true if it's a hash class specifically designed to store password
    ///   hashes, false for ordinary hash algorithms.
    /// </returns>
    class function IsPasswordHash: Boolean; override;

    // mask generation

    /// <summary>
    ///   Mask generation: generates an output based on the data given which is
    ///   similar to a hash function but in contrast does not have a fixed output
    ///   length. Use of a MGF is desirable in cases where a fixed-size hash
    ///   would be inadequate. Examples include generating padding, producing
    ///   one time pads or keystreams in symmetric key encryption, and yielding
    ///   outputs for pseudorandom number generators.
    ///   Indexed Mask generation function, IEEE P1363 working group
    ///   equal to KDF1 except without seed. RFC 2437 PKCS #1
    /// </summary>
    /// <param name="Data">
    ///   Data from which to generate a mask from
    /// </param>
    /// <param name="DataSize">
    ///   Size of the input data in bytes
    /// </param>
    /// <param name="MaskSize">
    ///   Size of the returned mask in bytes
    /// </param>
    /// <returns>
    ///   Mask such that one cannot determine the data which had been given to
    ///   generate this mask from.
    /// </returns>
    class function MGF1(const Data; DataSize, MaskSize: Integer): TBytes; overload;
    /// <summary>
    ///   Mask generation: generates an output based on the data given which is
    ///   similar to a hash function but incontrast does not have a fixed output
    ///   length. Use of a MGF is desirable in cases where a fixed-size hash
    ///   would be inadequate. Examples include generating padding, producing
    ///   one time pads or keystreams in symmetric key encryption, and yielding
    ///   outputs for pseudorandom number generators
    /// </summary>
    /// <param name="Data">
    ///   Data from which to generate a mask from
    /// </param>
    /// <param name="MaskSize">
    ///   Size of the returned mask in bytes
    /// </param>
    /// <returns>
    ///   Mask such that one cannot determine the data which had been given to
    ///   generate this mask from.
    /// </returns>
    class function MGF1(const Data: TBytes; MaskSize: Integer): TBytes; overload;

    /// <summary>
    ///   Key deviation algorithm to derrive keys from other keys.
    ///   IEEE P1363 Working Group, ISO 18033-2:2004
    /// </summary>
    /// <param name="Data">
    ///   Source data from which the new key shall be derrived.
    /// </param>
    /// <param name="DataSize">
    ///   Size in bytes of the source data passed.
    /// </param>
    /// <param name="Seed">
    ///   Salt value
    /// </param>
    /// <param name="SeedSize">
    ///   Size of the seed/salt in byte.
    /// </param>
    /// <param name="MaskSize">
    ///   Size of the generated output in byte
    /// </param>
    /// <returns>
    ///   Returns the new derrived key with the length specified in MaskSize.
    /// </returns>
    /// <remarks>
    ///   In earlier versions there was an optional format parameter. This has
    ///   been removed as this is a base class. The method might not have
    ///   returned a result with the MaskSize specified, as the formatting might
    ///   have had to alter this. This would have been illogical.
    /// </remarks>
    class function KDF1(const Data; DataSize: Integer; const Seed;
                        SeedSize, MaskSize: Integer): TBytes; overload;

    /// <summary>
    ///   Key deviation algorithm to derrive keys from other keys.
    ///   IEEE P1363 Working Group, ISO 18033-2:2004
    /// </summary>
    /// <param name="Data">
    ///   Source data from which the new key shall be derrived.
    /// </param>
    /// <param name="Seed">
    ///   Salt value
    /// </param>
    /// <param name="MaskSize">
    ///   Size of the generated output in byte
    /// </param>
    /// <returns>
    ///   Returns the new derrived key with the length specified in MaskSize.
    /// </returns>
    class function KDF1(const Data, Seed: TBytes; MaskSize: Integer): TBytes; overload;

    /// <summary>
    ///   Key deviation algorithm to derrive keys from other keys.
    ///   IEEE P1363 Working Group, ISO 18033-2:2004
    /// </summary>
    /// <param name="Data">
    ///   Source data from which the new key shall be derrived.
    /// </param>
    /// <param name="DataSize">
    ///   Size in bytes of the source data passed.
    /// </param>
    /// <param name="Seed">
    ///   Salt value
    /// </param>
    /// <param name="SeedSize">
    ///   Size of the seed/salt in byte.
    /// </param>
    /// <param name="MaskSize">
    ///   Size of the generated output in byte
    /// </param>
    /// <returns>
    ///   Returns the new derrived key with the length specified in MaskSize.
    /// </returns>
    /// <remarks>
    ///   In earlier versions there was an optional format parameter. This has
    ///   been removed as this is a base class. The method might not have
    ///   returned a result with the MaskSize specified, as the formatting might
    ///   have had to alter this. This would have been illogical.
    /// </remarks>
    class function KDF2(const Data; DataSize: Integer; const Seed;
                        SeedSize, MaskSize: Integer): TBytes; overload;

    /// <summary>
    ///   Key deviation algorithm to derrive keys from other keys.
    ///   IEEE P1363 Working Group, ISO 18033-2:2004
    /// </summary>
    /// <param name="Data">
    ///   Source data from which the new key shall be derrived.
    /// </param>
    /// <param name="Seed">
    ///   Start value for pseudo random number generator
    /// </param>
    /// <param name="MaskSize">
    ///   Size of the generated output in byte
    /// </param>
    /// <returns>
    ///   Returns the new derrived key with the length specified in MaskSize.
    /// </returns>
    class function KDF2(const Data, Seed: TBytes; MaskSize: Integer): TBytes; overload;

    /// <summary>
    ///   Key deviation algorithm to derrive keys from other keys.
    /// </summary>
    /// <param name="Data">
    ///   Source data from which the new key shall be derrived.
    /// </param>
    /// <param name="DataSize">
    ///   Size in bytes of the source data passed.
    /// </param>
    /// <param name="Seed">
    ///   Salt value
    /// </param>
    /// <param name="SeedSize">
    ///   Size of the seed/salt in byte.
    /// </param>
    /// <param name="MaskSize">
    ///   Size of the generated output in byte
    /// </param>
    /// <returns>
    ///   Returns the new derrived key with the length specified in MaskSize.
    /// </returns>
    /// <remarks>
    ///   In earlier versions there was an optional format parameter. This has
    ///   been removed as this is a base class. The method might not have
    ///   returned a result with the MaskSize specified, as the formatting might
    ///   have had to alter this. This would have been illogical.
    /// </remarks>
    class function KDF3(const Data; DataSize: Integer; const Seed;
                        SeedSize, MaskSize: Integer): TBytes; overload;

    /// <summary>
    ///   Key deviation algorithm to derrive keys from other keys.
    /// </summary>
    /// <param name="Data">
    ///   Source data from which the new key shall be derrived.
    /// </param>
    /// <param name="Seed">
    ///   Salt value
    /// </param>
    /// <param name="MaskSize">
    ///   Size of the generated output in byte
    /// </param>
    /// <returns>
    ///   Returns the new derrived key with the length specified in MaskSize.
    /// </returns>
    class function KDF3(const Data, Seed: TBytes; MaskSize: Integer): TBytes; overload;

    // DEC's own KDF + MGF

    /// <summary>
    ///   Key deviation algorithm to derrive keys from other keys. The alrorithm
    ///   implemented by this method does not follow any official standard.
    /// </summary>
    /// <param name="Data">
    ///   Source data from which the new key shall be derrived.
    /// </param>
    /// <param name="DataSize">
    ///   Size in bytes of the source data passed.
    /// </param>
    /// <param name="Seed">
    ///   Salt value
    /// </param>
    /// <param name="SeedSize">
    ///   Size of the seed/salt in byte.
    /// </param>
    /// <param name="MaskSize">
    ///   Size of the generated output in byte
    /// </param>
    /// <param name="Index">
    ///   Optional parameter: can be used to specify a different default value
    ///   for the index variable used in the algorithm.
    /// </param>
    /// <returns>
    ///   Returns the new derrived key with the length specified in MaskSize.
    /// </returns>
    class function KDFx(const Data; DataSize: Integer; const Seed; SeedSize, MaskSize: Integer; Index: UInt32 = 1): TBytes; overload;
    /// <summary>
    ///   Key deviation algorithm to derrive keys from other keys.
    /// </summary>
    /// <remarks>
    ///   This variant of the algorithm does not follow an official standard.
    ///   It has been created by the original author of DEC.
    /// </remarks>
    /// <param name="Data">
    ///   Source data from which the new key shall be derrived.
    /// </param>
    /// <param name="Seed">
    ///   Salt value
    /// </param>
    /// <param name="MaskSize">
    ///   Size of the generated output in byte
    /// </param>
    /// <param name="Index">
    ///   Optional parameter: can be used to specify a different default value
    ///   for the index variable used in the algorithm.
    /// </param>
    /// <returns>
    ///   Returns the new derrived key with the length specified in MaskSize.
    /// </returns>
    class function KDFx(const Data, Seed: TBytes; MaskSize: Integer; Index: UInt32 = 1): TBytes; overload;

    /// <summary>
    ///   Mask generation: generates an output based on the data given which is
    ///   similar to a hash function but incontrast does not have a fixed output
    ///   length. Use of a MGF is desirable in cases where a fixed-size hash
    ///   would be inadequate. Examples include generating padding, producing
    ///   one time pads or keystreams in symmetric key encryption, and yielding
    ///   outputs for pseudorandom number generators.
    /// </summary>
    /// <remarks>
    ///   This variant of the algorithm does not follow an official standard.
    ///   It has been created by the original author of DEC.
    /// </remarks>
    /// <param name="Data">
    ///   Data from which to generate a mask from
    /// </param>
    /// <param name="DataSize">
    ///   Size of the passed data in bytes
    /// </param>
    /// <param name="MaskSize">
    ///   Size of the returned mask in bytes
    /// </param>
    /// <param name="Index">
    ///   Looks like this is a salt applied to each byte of output data?
{ TODO : Clarify this parameter }
    /// </param>
    /// <returns>
    ///   Mask such that one cannot determine the data which had been given to
    ///   generate this mask from.
    /// </returns>
    class function MGFx(const Data; DataSize, MaskSize: Integer; Index: UInt32 = 1): TBytes; overload;
    /// <summary>
    ///   Mask generation: generates an output based on the data given which is
    ///   similar to a hash function but incontrast does not have a fixed output
    ///   length. Use of a MGF is desirable in cases where a fixed-size hash
    ///   would be inadequate. Examples include generating padding, producing
    ///   one time pads or keystreams in symmetric key encryption, and yielding
    ///   outputs for pseudorandom number generators.
    /// </summary>
    /// <remarks>
    ///   This variant of the algorithm does not follow an official standard.
    ///   It has been created by the original author of DEC.
    /// </remarks>
    /// <param name="Data">
    ///   Data from which to generate a mask from
    /// </param>
    /// <param name="MaskSize">
    ///   Size of the returned mask in bytes
    /// </param>
    /// <param name="Index">
    ///   Looks like this is a salt applied to each byte of output data?
{ TODO : Clarify this parameter }
    /// </param>
    /// <returns>
    ///   Mask such that one cannot determine the data which had been given to
    ///   generate this mask from.
    /// </returns>
    class function MGFx(const Data: TBytes; MaskSize: Integer; Index: UInt32 = 1): TBytes; overload;

    /// <summary>
    ///   HMAC according to rfc2202: hash message authentication code allow to
    ///   verify both the data integrity and the authenticity of a message.
    /// </summary>
    /// <param name="Key">
    ///   This is the secret key which shall not be transmitted over the line.
    ///   The sender uses this key to create the resulting HMAC, transmits the
    ///   text and the HMAC over the line and the receiver recalculates the HMAC
    ///   based on his copy of the secret key. If his calculated HMAC equals the
    ///   transfered HMAC value the message has not been tampered.
    /// </param>
    /// <param name="Text">
    ///   Text over which to calculate the HMAC
    /// </param>
    /// <returns>
    ///   Calculated HMAC
    /// </returns>
    class function HMAC(const Key, Text: TBytes): TBytes; overload;

    /// <summary>
    ///   HMAC according to rfc2202: hash message authentication code allow to
    ///   verify both the data integrity and the authenticity of a message.
    /// </summary>
    /// <param name="Key">
    ///   This is the secret key which shall not be transmitted over the line.
    ///   The sender uses this key to create the resulting HMAC, transmits the
    ///   text and the HMAC over the line and the receiver recalculates the HMAC
    ///   based on his copy of the secret key. If his calculated HMAC equals the
    ///   transfered HMAC value the message has not been tampered.
    /// </param>
    /// <param name="Text">
    ///   Text over which to calculate the HMAC
    /// </param>
    /// <returns>
    ///   Calculated HMAC
    /// </returns>
    class function HMAC(const Key, Text: RawByteString): TBytes; overload;

    /// <summary>
    ///   Password based key deviation function 2
    ///   RFC 2898, PKCS #5.
    ///   This can be used to create a login sheme by storing the output,
    ///   number of iterations and the salt. When the user enters a password
    ///   this calculation is done using the same parameters as stored for his
    ///   user account and comparing the output.
    /// </summary>
    /// <param name="Password">
    ///   Password to create the deviation from
    /// </param>
    /// <param name="Salt">
    ///   Salt used to modify the password
    /// </param>
    /// <param name="Iterations">
    ///   Number of iterations to perform
    /// </param>
    /// <param name="KeyLength">
    ///   Length of the resulting key in byte
    /// </param>
    class function PBKDF2(const Password, Salt: TBytes; Iterations: Integer; KeyLength: Integer): TBytes; overload;

    /// <summary>
    ///   Password based key deviation function 2
    ///   RFC 2898, PKCS #5.
    ///   This can be used to create a login sheme by storing the output,
    ///   number of iterations and the salt. When the user enters a password
    ///   this calculation is done using the same parameters as stored for his
    ///   user account and comparing the output.
    /// </summary>
    /// <param name="Password">
    ///   Password to create the deviation from
    /// </param>
    /// <param name="Salt">
    ///   Salt used to modify the password
    /// </param>
    /// <param name="Iterations">
    ///   Number of iterations to perform
    /// </param>
    /// <param name="KeyLength">
    ///   Length of the resulting key in byte
    /// </param>
    class function PBKDF2(const Password, Salt: RawByteString; Iterations: Integer; KeyLength: Integer): TBytes; overload;
  end;

  /// <summary>
  ///   All hash classes with hash algorithms specially developed for password
  ///   hashing should inherit from this class in order to be able to distinguish
  ///   those from normal hash algorithms not really meant to be used for password
  ///   hashing.
  /// </summary>
  TDECPasswordHash = class(TDECHashAuthentication);

  {$IF CompilerVersion < 28.0}
  /// <summary>
  ///   Class helper for implementing array concatenation which is not available
  ///   in Delphi XE6 or lower.
  /// </summary>
  /// <remarks>
  ///   SHall be removed as soon as the minimum supported version is XE7 or higher.
  /// </remarks>
  TArrHelper = class
    class procedure AppendArrays<T>(var A: TArray<T>; const B: TArray<T>);
  end;
  {$IFEND}

implementation

uses
  DECUtil;

class function TDECHashAuthentication.IsPasswordHash: Boolean;
begin
  Result := self.InheritsFrom(TDECPasswordHash);
end;

class function TDECHashAuthentication.KDFInternal(const Data; DataSize: Integer; const Seed;
                             SeedSize, MaskSize: Integer; KDFType: TKDFType): TBytes;
var
  I, n,
  Rounds, DigestBytes : Integer;
  Dest                : PByteArray;
  Count               : UInt32;
  HashInstance        : TDECHashAuthentication;
begin
  SetLength(Result, 0);
  DigestBytes := DigestSize;
  Assert(MaskSize >= 0);
  Assert(DataSize >= 0);
  Assert(SeedSize >= 0);
  Assert(DigestBytes >= 0);

  HashInstance := TDECHashAuthenticationClass(self).Create;
  try
    Rounds := (MaskSize + DigestBytes - 1) div DigestBytes;
    SetLength(Result, Rounds * DigestBytes);
    Dest := @Result[0];


    if (KDFType = ktKDF2) then
      n := 1
    else
      n := 0;

    for I := 0 to Rounds-1 do
    begin
      Count := SwapUInt32(n);
      HashInstance.Init;

      if (KDFType = ktKDF3) then
      begin
        HashInstance.Calc(Count, SizeOf(Count));
        HashInstance.Calc(Data, DataSize);
      end
      else
      begin
        HashInstance.Calc(Data, DataSize);
        HashInstance.Calc(Count, SizeOf(Count));
      end;

      HashInstance.Calc(Seed, SeedSize);
      HashInstance.Done;
      Move(HashInstance.Digest[0], Dest[(I) * DigestBytes], DigestBytes);

      inc(n);
    end;

    SetLength(Result, MaskSize);
  finally
    HashInstance.Free;
  end;
end;

class function TDECHashAuthentication.MGF1(const Data; DataSize, MaskSize: Integer): TBytes;
begin
  Result := KDF1(Data, DataSize, NullStr, 0, MaskSize);
end;

class function TDECHashAuthentication.MGF1(const Data: TBytes; MaskSize: Integer): TBytes;
begin
  Result := KDFInternal(Data[0], Length(Data), NullStr, 0, MaskSize, ktKDF1);
end;

class function TDECHashAuthentication.KDF1(const Data; DataSize: Integer; const Seed;
  SeedSize, MaskSize: Integer): TBytes;
begin
  Result := KDFInternal(Data, DataSize, Seed, SeedSize, MaskSize, ktKDF1);
end;

class function TDECHashAuthentication.KDF1(const Data, Seed: TBytes;
  MaskSize: Integer): TBytes;
begin
  if (length(Seed) > 0) then
    Result := KDFInternal(Data[0], length(Data), Seed[0], length(Seed), MaskSize, ktKDF1)
  else
    Result := KDFInternal(Data[0], length(Data), NullStr, 0, MaskSize, ktKDF1);
end;

class function TDECHashAuthentication.KDF2(const Data; DataSize: Integer; const Seed;
                             SeedSize, MaskSize: Integer): TBytes;
begin
  Result := KDFInternal(Data, DataSize, Seed, SeedSize, MaskSize, ktKDF2);
end;

class function TDECHashAuthentication.KDF2(const Data, Seed: TBytes; MaskSize: Integer): TBytes;
begin
  if (length(Seed) > 0) then
    Result := KDFInternal(Data[0], Length(Data), Seed[0], Length(Seed), MaskSize, ktKDF2)
  else
    Result := KDFInternal(Data[0], Length(Data), NullStr, 0, MaskSize, ktKDF2);
end;

class function TDECHashAuthentication.KDF3(const Data; DataSize: Integer; const Seed;
                             SeedSize, MaskSize: Integer): TBytes;
begin
  Result := KDFInternal(Data, DataSize, Seed, SeedSize, MaskSize, ktKDF3);
end;

class function TDECHashAuthentication.KDF3(const Data, Seed: TBytes; MaskSize: Integer): TBytes;
begin
  if (length(Seed) > 0) then
    Result := KDFInternal(Data[0], Length(Data), Seed[0], Length(Seed), MaskSize, ktKDF3)
  else
    Result := KDFInternal(Data[0], Length(Data), NullStr, 0, MaskSize, ktKDF3);
end;

class function TDECHashAuthentication.KDFx(const Data; DataSize: Integer; const Seed; SeedSize, MaskSize: Integer; Index: UInt32 = 1): TBytes;
// DEC's own KDF, even stronger
var
  I, J         : Integer;
  Count        : UInt32;
  R            : Byte;
  HashInstance : TDECHashAuthentication;
begin
  Assert(MaskSize >= 0);
  Assert(DataSize >= 0);
  Assert(SeedSize >= 0);
  Assert(DigestSize > 0);

  SetLength(Result, MaskSize);
  Index := SwapUInt32(Index);

  HashInstance := TDECHashAuthenticationClass(self).Create;
  try
    for I := 0 to MaskSize - 1 do
    begin
      HashInstance.Init;

      Count := SwapUInt32(I);
      HashInstance.Calc(Count, SizeOf(Count));
      HashInstance.Calc(Result[0], I);

      HashInstance.Calc(Index, SizeOf(Index));

      Count := SwapUInt32(SeedSize);
      HashInstance.Calc(Count, SizeOf(Count));
      HashInstance.Calc(Seed, SeedSize);

      Count := SwapUInt32(DataSize);
      HashInstance.Calc(Count, SizeOf(Count));
      HashInstance.Calc(Data, DataSize);

      HashInstance.Done;

      R := 0;

      for J := 0 to DigestSize - 1 do
        R := R xor HashInstance.Digest[J];

      Result[I] := R;
    end;
  finally
    HashInstance.Free;
  end;
end;

class function TDECHashAuthentication.KDFx(const Data, Seed: TBytes; MaskSize: Integer; Index: UInt32 = 1): TBytes;
begin
  if (length(Seed) > 0) then
    Result := KDFx(Data[0], Length(Data), Seed[0], Length(Seed), MaskSize, Index)
  else
    Result := KDFx(Data[0], Length(Data), NullStr, Length(Seed), MaskSize, Index)
end;

class function TDECHashAuthentication.MGFx(const Data; DataSize, MaskSize: Integer; Index: UInt32 = 1): TBytes;
begin
  Result := KDFx(Data, DataSize, NullStr, 0, MaskSize, Index);
end;

class function TDECHashAuthentication.MGFx(const Data: TBytes; MaskSize: Integer; Index: UInt32 = 1): TBytes;
begin
  Result := KDFx(Data[0], Length(Data), NullStr, 0, MaskSize, Index);
end;

class function TDECHashAuthentication.HMAC(const Key, Text: RawByteString): TBytes;
begin
  result := HMAC(BytesOf(Key), BytesOf(Text));
end;

class function TDECHashAuthentication.HMAC(const Key, Text: TBytes): TBytes;
const
  CONST_UINT_OF_0x36 = $3636363636363636;
  CONST_UINT_OF_0x5C = $5C5C5C5C5C5C5C5C;
var
  HashInstance: TDECHashAuthentication;
  InnerKeyPad, OuterKeyPad: array of Byte;
  I, KeyLength, BlockSize, DigestLength: Integer;
begin
  HashInstance := TDECHashAuthenticationClass(self).Create;
  try
    BlockSize    := HashInstance.BlockSize; // 64 for sha1, ...
    DigestLength := HashInstance.DigestSize;
    KeyLength    := Length(Key);

    SetLength(InnerKeyPad, BlockSize);
    SetLength(OuterKeyPad, BlockSize);

    I := 0;

    if KeyLength > BlockSize then
    begin
      Result    := HashInstance.CalcBytes(Key);
      KeyLength := DigestLength;
    end
    else
      Result := Key;

    while I <= KeyLength - SizeOf(NativeUInt) do
    begin
      PNativeUInt(@InnerKeyPad[I])^ := PNativeUInt(@Result[I])^ xor NativeUInt(CONST_UINT_OF_0x36);
      PNativeUInt(@OuterKeyPad[I])^ := PNativeUInt(@Result[I])^ xor NativeUInt(CONST_UINT_OF_0x5C);
      Inc(I, SizeOf(NativeUInt));
    end;

    while I < KeyLength do
    begin
      InnerKeyPad[I] := Result[I] xor $36;
      OuterKeyPad[I] := Result[I] xor $5C;
      Inc(I);
    end;

    while I <= BlockSize - SizeOf(NativeUInt) do
    begin
      PNativeUInt(@InnerKeyPad[I])^ := NativeUInt(CONST_UINT_OF_0x36);
      PNativeUInt(@OuterKeyPad[I])^ := NativeUInt(CONST_UINT_OF_0x5C);
      Inc(I, SizeOf(NativeUInt));
    end;

    while I < BlockSize do
    begin
      InnerKeyPad[I] := $36;
      OuterKeyPad[I] := $5C;
      Inc(I);
    end;

    HashInstance.Init;
    HashInstance.Calc(InnerKeyPad[0], BlockSize);
    if Length(Text) > 0 then
      HashInstance.Calc(Text[0], Length(Text));
    HashInstance.Done;
    Result := HashInstance.DigestAsBytes;

    HashInstance.Init;
    HashInstance.Calc(OuterKeyPad[0], BlockSize);
    HashInstance.Calc(Result[0], DigestLength);
    HashInstance.Done;

    Result := HashInstance.DigestAsBytes;
  finally
    HashInstance.Free;
  end;
end;

class function TDECHashAuthentication.PBKDF2(const Password, Salt: TBytes; Iterations: Integer; KeyLength: Integer): TBytes;
const
  CONST_UINT_OF_0x36 = $3636363636363636;
  CONST_UINT_OF_0x5C = $5C5C5C5C5C5C5C5C;
var
  Hash: TDECHashAuthentication;
  I, J, C: Integer;
  BlockCount, HashLengthRounded, SaltLength: Integer;
  PassLength, DigestLength, BlockSize: Integer;
  InnerKeyPad, OuterKeyPad: TBytes;
  SaltEx, T, U, TrimmedKey: TBytes;
begin
  Hash := TDECHashAuthenticationClass(self).Create;
  try
    // Setup needed parameters
    DigestLength      := Hash.DigestSize;
    HashLengthRounded := DigestLength - SizeOf(NativeUInt) + 1;
    BlockCount        := Trunc((KeyLength + DigestLength - 1) / DigestLength);
    BlockSize         := Hash.BlockSize;
    PassLength        := Length(Password);
    SaltLength        := Length(Salt);
    SaltEx            := Salt;
    SetLength(SaltEx, SaltLength + 4);  // reserve 4 bytes for INT_32_BE(i)
    SetLength(T, DigestLength);

    // Prepare Key for HMAC calculation
    // PrepareKeyForHMAC;
    I := 0;
    if PassLength > BlockSize then
    begin
      TrimmedKey := Hash.CalcBytes(Password);
      PassLength := DigestLength;
    end
    else
      TrimmedKey := Password;

    SetLength(InnerKeyPad, BlockSize);
    SetLength(OuterKeyPad, BlockSize);
    while I < PassLength do
    begin
      InnerKeyPad[I] := TrimmedKey[I] xor $36;
      OuterKeyPad[I] := TrimmedKey[I] xor $5C;
      Inc(I);
    end;
    while I < BlockSize do
    begin
      InnerKeyPad[I] := $36;
      OuterKeyPad[I] := $5C;
      Inc(I);
    end;

    // Calculate DK
    for I := 1 to BlockCount do
    begin
      SaltEx[SaltLength + 0] := Byte(I shr 24);   // INT_32_BE(i)
      SaltEx[SaltLength + 1] := Byte(I shr 16);
      SaltEx[SaltLength + 2] := Byte(I shr 8);
      SaltEx[SaltLength + 3] := Byte(I shr 0);
      FillChar(T[0], DigestLength, 0);            // reset Ti / F
      U := SaltEx;                                // initialize U to U1 = Salt + INT_32_BE(i)
      // Calculate F(Password, Salt, c, i) = U1 ^ U2 ^ ... ^ Uc
      for C := 1 to Iterations do
      begin
        Hash.Init;
        Hash.Calc(InnerKeyPad[0], BlockSize);
        Hash.Calc(U[0], Length(U));
        Hash.Done;
        U := Hash.DigestAsBytes;

        Hash.Init;
        Hash.Calc(OuterKeyPad[0], BlockSize);
        Hash.Calc(U[0], DigestLength);
        Hash.Done;
        U := Hash.DigestAsBytes;                  // Ui
        // F = U1 ^ U2 ^ ... ^ Uc
        J := 0;
        while J < HashLengthRounded do
        begin
          PNativeUInt(@T[J])^ := PNativeUInt(@T[J])^ xor PNativeUInt(@U[J])^;
          Inc(J, SizeOf(NativeUInt));
        end;
        while J < DigestLength do
        begin
          T[J] := T[J] xor U[J];
          Inc(J);
        end;
      end;

      {$IF CompilerVersion >= 28.0}
      Result := Result + T;                       // DK += F    , DK = DK || Ti
      {$ELSE}
      TArrHelper.AppendArrays<Byte>(Result, T);
      {$IFEND}
    end;
  finally
    Hash.Free;
  end;

  // Trim to the needed key length
  SetLength(Result, KeyLength);
end;

class function TDECHashAuthentication.PBKDF2(const Password, Salt: RawByteString; Iterations: Integer; KeyLength: Integer): TBytes;
begin
  result := PBKDF2(BytesOf(Password), BytesOf(Salt), Iterations, KeyLength);
end;

{ TArrHelper }

{$IF CompilerVersion < 28.0}
class procedure TArrHelper.AppendArrays<T>(var A: TArray<T>; const B: TArray<T>);
var
  i, L: Integer;
begin
  L := Length(A);
  SetLength(A, L + Length(B));
  for i := 0 to High(B) do
    A[L + i] := B[i];
end;
{$IFEND}

end.
