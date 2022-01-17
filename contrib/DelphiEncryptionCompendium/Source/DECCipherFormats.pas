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
unit DECCipherFormats;

interface

uses
  {$IFDEF FPC}
  SysUtils, Classes,
  {$ELSE}
  System.SysUtils, System.Classes,
  {$ENDIF}
  DECCipherBase, DECCipherModes, DECUtil, DECFormatBase, DECCipherInterface;

type
  /// <summary>
  ///   Class in which the various encode/decode variants provided have been
  ///   moved in order to keep the base cipher class small and clean.
  /// </summary>
  TDECFormattedCipher = class(TDECCipherModes, IDECCipher)
  private
    /// <summary>
    ///   Encrypts or decrypts the data contained in a given stream
    /// </summary>
    /// <param name="Source">
    ///   Source stream containing the data to encrypt or to decrypt
    /// </param>
    /// <param name="Dest">
    ///   Destination stream, where the encrypted or decrypted data shall be put in
    /// </param>
    /// <param name="DataSize">
    ///   Number of bytes of Source to be encrypted or decrypted
    /// </param>
    /// <param name="CipherProc">
    ///   Callback which either encrypts or decrypts the stream, depending on
    ///   which one is being passed
    /// </param>
    /// <param name="OnProgress">
    ///   optional callback for reporting progress of the operation
    /// </param>
    procedure DoEncodeDecodeStream(const Source, Dest: TStream; DataSize: Int64;
                                   const CipherProc: TDECCipherCodeEvent;
                                   const OnProgress: TDECProgressEvent);

    /// <summary>
    ///   Encrypts or decrypts a file and stores the result in another file
    /// </summary>
    /// <param name="SourceFileName">
    ///   Path and name of the file to encrypt
    /// </param>
    /// <param name="DestFileName">
    ///   Path and name of the file the encrypted data shall be stored in
    /// </param>
    /// <param name="Proc">
    ///   This method does the actual encrypting or decrypting of the data.
    ///   Usually the Encode or Decode method is being passed here which is
    ///   declared in TDECCipherBase as virtual abstract method and
    ///   implemented in the individual cipher class inheriting from this one
    /// </param>
    /// <param name="OnProgress">
    ///   Optional event which can be passed to get information about the
    ///   progress of the encryption operation
    /// </param>
    procedure DoEncodeDecodeFile(const SourceFileName, DestFileName: string;
                                 const Proc: TDECCipherCodeEvent;
                                 const OnProgress: TDECProgressEvent);
  public
    /// <summary>
    ///   Encrypts the contents of a given byte array
    /// </summary>
    /// <param name="Source">
    ///   Byte array with data to be encrypted. When block chaining mode ECBx
    ///   is used (not recommended!), the size of the data passed via this
    ///   parameter needs to be a multiple of the block size of the algorithm used,
    ///   otherwise a EDECCipherException exception will be raised!
    /// </param>
    /// <returns>
    ///   Byte array with encrypted data
    /// </returns>
    function EncodeBytes(const Source: TBytes): TBytes;

    /// <summary>
    ///   Decrypts the contents of a given byte array
    /// </summary>
    /// <param name="Source">
    ///   Byte array with data to be decrypted. When block chaining mode ECBx
    ///   is used (not recommended!), the size of the data passed via this
    ///   parameter needs to be a multiple of the block size of the algorithm used,
    ///   otherwise a EDECCipherException exception will be raised!
    /// </param>
    /// <returns>
    ///   Byte array with decrypted data
    /// </returns>
    function DecodeBytes(const Source: TBytes): TBytes;

    /// <summary>
    ///   Encrypts the data contained in a given stream
    /// </summary>
    /// <param name="Source">
    ///   Source stream containing the data to encrypt. When block chaining mode ECBx
    ///   is used (not recommended!), the size of the data passed via this
    ///   parameter needs to be a multiple of the block size of the algorithm used,
    ///   otherwise a EDECCipherException exception will be raised!
    /// </param>
    /// <param name="Dest">
    ///   Destination stream, where the encrypted data shall be put in
    /// </param>
    /// <param name="DataSize">
    ///   Number of bytes of Source to be encrypted
    /// </param>
    /// <param name="OnProgress">
    ///   optional callback for reporting progress of the operation
    /// </param>
    procedure EncodeStream(const Source, Dest: TStream; DataSize: Int64;
                           const OnProgress: TDECProgressEvent = nil);

    /// <summary>
    ///   Decrypts the data contained in a given stream
    /// </summary>
    /// <param name="Source">
    ///   Source stream containing the data to decrypt. When block chaining mode ECBx
    ///   is used (not recommended!), the size of the data passed via this
    ///   parameter needs to be a multiple of the block size of the algorithm used,
    ///   otherwise a EDECCipherException exception will be raised!
    /// </param>
    /// <param name="Dest">
    ///   Destination stream, where the decrypted data shall be put in
    /// </param>
    /// <param name="DataSize">
    ///   Number of bytes of Source to be decrypted
    /// </param>
    /// <param name="OnProgress">
    ///   optional callback for reporting progress of the operation
    /// </param>
    procedure DecodeStream(const Source, Dest: TStream; DataSize: Int64;
                           const OnProgress: TDECProgressEvent = nil);

    /// <summary>
    ///   Reads the contents of one file, encrypts it and stores it in another file
    /// </summary>
    /// <param name="SourceFileName">
    ///   Path and name of the file to encrypt. When block chaining mode ECBx
    ///   is used (not recommended!), the size of the data passed via this
    ///   parameter needs to be a multiple of the block size of the algorithm
    ///   used, otherwise a EDECCipherException exception will be raised!
    /// </param>
    /// <param name="DestFileName">
    ///   Path and name of the file the encrypted data shall be stored in
    /// </param>
    /// <param name="OnProgress">
    ///   Optional event which can be passed to get information about the
    ///   progress of the encryption operation
    /// </param>
    procedure EncodeFile(const SourceFileName, DestFileName: string;
                         const OnProgress: TDECProgressEvent = nil);

    /// <summary>
    ///   Reads the contents of one file, decrypts it and stores it in another file
    /// </summary>
    /// <param name="SourceFileName">
    ///   Path and name of the file to decrypt. When block chaining mode ECBx
    ///   is used (not recommended!), the size of the data passed via this
    ///   parameter needs to be a multiple of the block size of the algorithm
    ///   used, otherwise a EDECCipherException exception will be raised!
    /// </param>
    /// <param name="DestFileName">
    ///   Path and name of the file the decrypted data shall be stored in
    /// </param>
    /// <param name="OnProgress">
    ///   Optional event which can be passed to get information about the
    ///   progress of the decryption operation
    /// </param>
    procedure DecodeFile(const SourceFileName, DestFileName: string;
                         const OnProgress: TDECProgressEvent = nil);

    /// <summary>
    ///   Encrypts the contents of the passed unicode string
    /// </summary>
    /// <param name="Source">
    ///   String to encrypt. When block chaining mode ECBx
    ///   is used (not recommended!), the size of the data passed via this
    ///   parameter needs to be a multiple of the block size of the algorithm
    ///   used, otherwise a EDECCipherException exception will be raised!
    /// </param>
    /// <param name="Format">
    ///   Optional parameter. One can pass a class reference of one of the
    ///   concrete data formatting classes here which will be internally used
    ///   to convert the data. Encoded will be the encrypted data, not the
    ///   source data. Formattings can be used to convert data into a format
    ///   suitable for the transport medium the data shall be transported with.
    /// </param>
    /// <returns>
    ///   Encrypted string as a byte array
    /// </returns>
    function EncodeStringToBytes(const Source: string;
                                 Format: TDECFormatClass = nil): TBytes; overload;

    /// <summary>
    ///   Encrypts the contents of the passed RawByteString
    /// </summary>
    /// <param name="Source">
    ///   String to encrypt. When block chaining mode ECBx
    ///   is used (not recommended!), the size of the data passed via this
    ///   parameter needs to be a multiple of the block size of the algorithm
    ///   used, otherwise a EDECCipherException exception will be raised!
    /// </param>
    /// <param name="Format">
    ///   Optional parameter. One can pass a class reference of one of the
    ///   concrete data formatting classes here which will be internally used
    ///   to convert the data. Encoded will be the encrypted data, not the
    ///   source data. Formattings can be used to convert data into a format
    ///   suitable for the transport medium the data shall be transported with.
    /// </param>
    /// <returns>
    ///   Encrypted string as a byte array
    /// </returns>
    function EncodeStringToBytes(const Source: RawByteString;
                                 Format: TDECFormatClass = nil): TBytes; overload;

    /// <summary>
    ///   Encrypts the contents of the passed unicode string
    /// </summary>
    /// <param name="Source">
    ///   String to encrypt. When block chaining mode ECBx
    ///   is used (not recommended!), the size of the data passed via this
    ///   parameter needs to be a multiple of the block size of the algorithm
    ///   used, otherwise a EDECCipherException exception will be raised!
    /// </param>
    /// <param name="Format">
    ///   Optional parameter. One can pass a class reference of one of the
    ///   concrete data formatting classes here which will be internally used
    ///   to convert the data. Encoded will be the encrypted data, not the
    ///   source data. Formattings can be used to convert data into a format
    ///   suitable for the transport medium the data shall be transported with.
    /// </param>
    /// <returns>
    ///   Encrypted string
    /// </returns>
    /// <remarks>
    ///   The use of this method is only recommended if a formatting is passed
    ///   which will result in an 7-bit ASCII compatible string as we cannot
    ///   ensure that Unicode string processing will not alter/interpret some
    ///   byte combinations in a destructive way, making the encrypted string
    ///   un-decryptable.
    /// </remarks>
    function EncodeStringToString(const Source: string;
                                  Format: TDECFormatClass = nil): string; overload;

    /// <summary>
    ///   Encrypts the contents of the passed unicode string
    /// </summary>
    /// <param name="Source">
    ///   String to encrypt. When block chaining mode ECBx
    ///   is used (not recommended!), the size of the data passed via this
    ///   parameter needs to be a multiple of the block size of the algorithm
    ///   used, otherwise a EDECCipherException exception will be raised!
    /// </param>
    /// <param name="Format">
    ///   Optional parameter. One can pass a class reference of one of the
    ///   concrete data formatting classes here which will be internally used
    ///   to convert the data. Encoded will be the encrypted data, not the
    ///   source data. Formattings can be used to convert data into a format
    ///   suitable for the transport medium the data shall be transported with.
    /// </param>
    /// <returns>
    ///   Encrypted string
    /// </returns>
    /// <remarks>
    ///   The use of this method is only recommended if a formatting is passed
    ///   which will result in an 7-bit ASCII compatible string as we cannot
    ///   ensure that string processing will not alter/interpret some
    ///   byte combinations in a destructive way, making the encrypted string
    ///   un-decryptable.
    /// </remarks>
    function EncodeStringToString(const Source: RawByteString;
                                  Format: TDECFormatClass = nil): RawByteString; overload;

    /// <summary>
    ///   Decrypts the contents of the passed encrypted unicode string
    /// </summary>
    /// <param name="Source">
    ///   String to decrypt. When block chaining mode ECBx
    ///   is used (not recommended!), the size of the data passed via this
    ///   parameter needs to be a multiple of the block size of the algorithm
    ///   used, otherwise a EDECCipherException exception will be raised!
    /// </param>
    /// <param name="Format">
    ///   Optional parameter. One can pass a class reference of one of the
    ///   concrete data formatting classes here which will be internally used
    ///   to convert the data. Decoded will be the still encrypted data, not the
    ///   encrypted data. Formattings can be used to convert data into a format
    ///   suitable for the transport medium the data shall be transported with.
    /// </param>
    /// <returns>
    ///   Decrypted string as a byte array
    /// </returns>
    function DecodeStringToBytes(const Source: string;
                                 Format: TDECFormatClass = nil): TBytes; overload;

    /// <summary>
    ///   Decrypts the contents of the passed encrypted RawByteString
    /// </summary>
    /// <param name="Source">
    ///   String to decrypt. When block chaining mode ECBx
    ///   is used (not recommended!), the size of the data passed via this
    ///   parameter needs to be a multiple of the block size of the algorithm
    ///   used, otherwise a EDECCipherException exception will be raised!
    /// </param>
    /// <param name="Format">
    ///   Optional parameter. One can pass a class reference of one of the
    ///   concrete data formatting classes here which will be internally used
    ///   to convert the data. Decoded will be the still encrypted data, not the
    ///   encrypted data. Formattings can be used to convert data into a format
    ///   suitable for the transport medium the data shall be transported with.
    /// </param>
    /// <returns>
    ///   Decrypted string as a byte array
    /// </returns>
    function DecodeStringToBytes(const Source: RawByteString;
                                 Format: TDECFormatClass = nil): TBytes; overload;

    /// <summary>
    ///   Decrypts the contents of the passed Unicode string
    /// </summary>
    /// <param name="Source">
    ///   String to decrypt. When block chaining mode ECBx
    ///   is used (not recommended!), the size of the data passed via this
    ///   parameter needs to be a multiple of the block size of the algorithm
    ///   used, otherwise a EDECCipherException exception will be raised!
    /// </param>
    /// <param name="Format">
    ///   Optional parameter. One can pass a class reference of one of the
    ///   concrete data formatting classes here which will be internally used
    ///   to convert the data. Decoded will be the encrypted data, not the
    ///   decrypted data. Formattings can be used to convert data into a format
    ///   suitable for the transport medium the data shall be transported with.
    /// </param>
    /// <returns>
    ///   Decrypted string
    /// </returns>
    /// <remarks>
    ///   The use of this method is only recommended if a formatting is passed
    ///   which uses an 7-bit ASCII compatible string as input so that it
    ///   didn't get altered by Unicode string processing in some hafrmful way
    /// </remarks>
    function DecodeStringToString(const Source: string;
                                  Format: TDECFormatClass = nil): string; overload;

    /// <summary>
    ///   Decrypts the contents of the passed RawByteString string
    /// </summary>
    /// <param name="Source">
    ///   String to decrypt. When block chaining mode ECBx
    ///   is used (not recommended!), the size of the data passed via this
    ///   parameter needs to be a multiple of the block size of the algorithm
    ///   used, otherwise a EDECCipherException exception will be raised!
    /// </param>
    /// <param name="Format">
    ///   Optional parameter. One can pass a class reference of one of the
    ///   concrete data formatting classes here which will be internally used
    ///   to convert the data. Decoded will be the encrypted data, not the
    ///   decrypted data. Formattings can be used to convert data into a format
    ///   suitable for the transport medium the data shall be transported with.
    /// </param>
    /// <returns>
    ///   Decrypted string
    /// </returns>
    /// <remarks>
    ///   The use of this method is only recommended if a formatting is passed
    ///   which uses an 7-bit ASCII compatible string as input so that it
    ///   didn't get altered by string processing in some hafrmful way
    /// </remarks>
    function DecodeStringToString(const Source: RawByteString;
                                  Format: TDECFormatClass = nil): RawByteString; overload;

{$IFDEF ANSISTRINGSUPPORTED}
    /// <summary>
    ///   Encrypts the contents of the passed Ansistring
    /// </summary>
    /// <param name="Source">
    ///   String to encrypt. When block chaining mode ECBx
    ///   is used (not recommended!), the size of the data passed via this
    ///   parameter needs to be a multiple of the block size of the algorithm
    ///   used, otherwise a EDECCipherException exception will be raised!
    /// </param>
    /// <param name="Format">
    ///   Optional parameter. One can pass a class reference of one of the
    ///   concrete data formatting classes here which will be internally used
    ///   to convert the data. Encoded will be the encrypted data, not the
    ///   source data. Formattings can be used to convert data into a format
    ///   suitable for the transport medium the data shall be transported with.
    /// </param>
    /// <returns>
    ///   Encrypted string as a byte array
    /// </returns>
    function EncodeStringToBytes(const Source: AnsiString;
                                 Format: TDECFormatClass = nil): TBytes; overload;

    /// <summary>
    ///   Encrypts the contents of the passed Ansistring
    /// </summary>
    /// <param name="Source">
    ///   String to encrypt. When block chaining mode ECBx
    ///   is used (not recommended!), the size of the data passed via this
    ///   parameter needs to be a multiple of the block size of the algorithm
    ///   used, otherwise a EDECCipherException exception will be raised!
    /// </param>
    /// <param name="Format">
    ///   Optional parameter. One can pass a class reference of one of the
    ///   concrete data formatting classes here which will be internally used
    ///   to convert the data. Encoded will be the encrypted data, not the
    ///   source data. Formattings can be used to convert data into a format
    ///   suitable for the transport medium the data shall be transported with.
    /// </param>
    /// <returns>
    ///   Encrypted string as an AnsiString
    /// </returns>
    /// <remarks>
    ///   The use of this method is only recommended if a formatting is passed
    ///   which will result in an 7-bit ASCII compatible string as we cannot
    ///   ensure that string processing will not alter/interpret some
    ///   byte combinations in a destructive way, making the encrypted string
    ///   un-decryptable.
    /// </remarks>
    function EncodeStringToString(const Source: AnsiString;
                                  Format: TDECFormatClass = nil): AnsiString; overload;

    /// <summary>
    ///   Decrypts the contents of the passed encrypted Ansistring
    /// </summary>
    /// <param name="Source">
    ///   String to decrypt. When block chaining mode ECBx
    ///   is used (not recommended!), the size of the data passed via this
    ///   parameter needs to be a multiple of the block size of the algorithm
    ///   used, otherwise a EDECCipherException exception will be raised!
    /// </param>
    /// <param name="Format">
    ///   Optional parameter. One can pass a class reference of one of the
    ///   concrete data formatting classes here which will be internally used
    ///   to convert the data. Decoded will be the still encrypted data, not the
    ///   encrypted data. Formattings can be used to convert data into a format
    ///   suitable for the transport medium the data shall be transported with.
    /// </param>
    /// <returns>
    ///   Decrypted string as a byte array
    /// </returns>
    function DecodeStringToBytes(const Source: AnsiString;
                                 Format: TDECFormatClass = nil): TBytes; overload;

    /// <summary>
    ///   Decrypts the contents of the passed AnsiString string
    /// </summary>
    /// <param name="Source">
    ///   String to decrypt. When block chaining mode ECBx
    ///   is used (not recommended!), the size of the data passed via this
    ///   parameter needs to be a multiple of the block size of the algorithm
    ///   used, otherwise a EDECCipherException exception will be raised!
    /// </param>
    /// <param name="Format">
    ///   Optional parameter. One can pass a class reference of one of the
    ///   concrete data formatting classes here which will be internally used
    ///   to convert the data. Decoded will be the encrypted data, not the
    ///   decrypted data. Formattings can be used to convert data into a format
    ///   suitable for the transport medium the data shall be transported with.
    /// </param>
    /// <returns>
    ///   Decrypted string
    /// </returns>
    /// <remarks>
    ///   The use of this method is only recommended if a formatting is passed
    ///   which uses an 7-bit ASCII compatible string as input so that it
    ///   didn't get altered by string processing in some hafrmful way
    /// </remarks>
    function DecodeStringToString(const Source: AnsiString;
                                  Format: TDECFormatClass = nil): AnsiString; overload;
{$ENDIF}

{$IFNDEF NEXTGEN}
    /// <summary>
    ///   Encrypts the contents of the passed Widestring
    /// </summary>
    /// <param name="Source">
    ///   String to encrypt. When block chaining mode ECBx
    ///   is used (not recommended!), the size of the data passed via this
    ///   parameter needs to be a multiple of the block size of the algorithm
    ///   used, otherwise a EDECCipherException exception will be raised!
    /// </param>
    /// <param name="Format">
    ///   Optional parameter. One can pass a class reference of one of the
    ///   concrete data formatting classes here which will be internally used
    ///   to convert the data. Encoded will be the encrypted data, not the
    ///   source data. Formattings can be used to convert data into a format
    ///   suitable for the transport medium the data shall be transported with.
    /// </param>
    /// <returns>
    ///   Encrypted string as a byte array
    /// </returns>
    function EncodeStringToBytes(const Source: WideString;
                                 Format: TDECFormatClass = nil): TBytes; overload;

    /// <summary>
    ///   Encrypts the contents of the passed Widestring
    /// </summary>
    /// <param name="Source">
    ///   String to encrypt. When block chaining mode ECBx
    ///   is used (not recommended!), the size of the data passed via this
    ///   parameter needs to be a multiple of the block size of the algorithm
    ///   used, otherwise a EDECCipherException exception will be raised!
    /// </param>
    /// <param name="Format">
    ///   Optional parameter. One can pass a class reference of one of the
    ///   concrete data formatting classes here which will be internally used
    ///   to convert the data. Encoded will be the encrypted data, not the
    ///   source data. Formattings can be used to convert data into a format
    ///   suitable for the transport medium the data shall be transported with.
    /// </param>
    /// <returns>
    ///   Encrypted string as an WideString
    /// </returns>
    /// <remarks>
    ///   The use of this method is only recommended if a formatting is passed
    ///   which will result in an 7-bit ASCII compatible string as we cannot
    ///   ensure that string processing will not alter/interpret some
    ///   byte combinations in a destructive way, making the encrypted string
    ///   un-decryptable.
    /// </remarks>
    function EncodeStringToString(const Source: WideString;
                                  Format: TDECFormatClass = nil): WideString; overload;

    /// <summary>
    ///   Decrypts the contents of the passed encrypted Widestring
    /// </summary>
    /// <param name="Source">
    ///   String to decrypt. When block chaining mode ECBx
    ///   is used (not recommended!), the size of the data passed via this
    ///   parameter needs to be a multiple of the block size of the algorithm
    ///   used, otherwise a EDECCipherException exception will be raised!
    /// </param>
    /// <param name="Format">
    ///   Optional parameter. One can pass a class reference of one of the
    ///   concrete data formatting classes here which will be internally used
    ///   to convert the data. Decoded will be the still encrypted data, not the
    ///   encrypted data. Formattings can be used to convert data into a format
    ///   suitable for the transport medium the data shall be transported with.
    /// </param>
    /// <returns>
    ///   Decrypted string as a byte array
    /// </returns>
    function DecodeStringToBytes(const Source: WideString;
                                 Format: TDECFormatClass = nil): TBytes; overload;

    /// <summary>
    ///   Decrypts the contents of the passed WideString string
    /// </summary>
    /// <param name="Source">
    ///   String to decrypt. When block chaining mode ECBx
    ///   is used (not recommended!), the size of the data passed via this
    ///   parameter needs to be a multiple of the block size of the algorithm
    ///   used, otherwise a EDECCipherException exception will be raised!
    /// </param>
    /// <param name="Format">
    ///   Optional parameter. One can pass a class reference of one of the
    ///   concrete data formatting classes here which will be internally used
    ///   to convert the data. Decoded will be the encrypted data, not the
    ///   decrypted data. Formattings can be used to convert data into a format
    ///   suitable for the transport medium the data shall be transported with.
    /// </param>
    /// <returns>
    ///   Decrypted string
    /// </returns>
    /// <remarks>
    ///   The use of this method is only recommended if a formatting is passed
    ///   which uses an 7-bit ASCII compatible string as input so that it
    ///   didn't get altered by string processing in some hafrmful way
    /// </remarks>
    function DecodeStringToString(const Source: WideString;
                                  Format: TDECFormatClass = nil): WideString; overload;
{$ENDIF}
  end;

implementation

uses
  DECBaseClass;

function TDECFormattedCipher.EncodeBytes(const Source: TBytes): TBytes;
begin
  SetLength(Result, Length(Source));
  if Length(Result) > 0 then
    Encode(Source[0], Result[0], Length(Source));
end;

function TDECFormattedCipher.DecodeBytes(const Source: TBytes): TBytes;
begin
  Result := Source;
  if Length(Result) > 0 then
    Decode(Result[0], Result[0], Length(Source));
end;

procedure TDECFormattedCipher.DoEncodeDecodeStream(const Source, Dest: TStream;
                                                   DataSize: Int64;
                                                   const CipherProc: TDECCipherCodeEvent;
                                                   const OnProgress: TDECProgressEvent);
var
  Buffer: TBytes;
  BufferSize, Bytes: Integer;
  Max, StartPos, Pos: Int64;
begin
  Pos := Source.Position;
  if DataSize < 0 then
    DataSize := Source.Size - Pos;

  Max      := Pos + DataSize;
  StartPos := Pos;

  if DataSize > 0 then
    try
      if Assigned(OnProgress) then
        OnProgress(Max, 0, Started);

      if StreamBufferSize <= 0 then
        StreamBufferSize := 8192;
      BufferSize := StreamBufferSize mod Context.BlockSize;
      if BufferSize = 0 then
        BufferSize := StreamBufferSize
      else
        BufferSize := StreamBufferSize + Context.BlockSize - BufferSize;
      if DataSize > BufferSize then
        SetLength(Buffer, BufferSize)
      else
        SetLength(Buffer, DataSize);
      while DataSize > 0 do
      begin
        Bytes := BufferSize;
        if Bytes > DataSize then
          Bytes := DataSize;
        Source.ReadBuffer(Buffer[0], Bytes);

        // The real encryption or decryption routine
        CipherProc(Buffer[0], Buffer[0], Bytes);
        Dest.WriteBuffer(Buffer[0], Bytes);
        Dec(DataSize, Bytes);
        Inc(Pos, Bytes);

        if Assigned(OnProgress) then
          OnProgress(Max, Pos - StartPos, Processing);
      end;
    finally
      ProtectBytes(Buffer);
      if Assigned(OnProgress) then
        OnProgress(Max, Max, Finished);
    end;
end;

procedure TDECFormattedCipher.EncodeStream(const Source, Dest: TStream; DataSize: Int64;
                                           const OnProgress: TDECProgressEvent);
begin
  DoEncodeDecodeStream(Source, Dest, DataSize,
                       Encode, OnProgress);
end;

procedure TDECFormattedCipher.DecodeStream(const Source, Dest: TStream; DataSize: Int64;
                                           const OnProgress: TDECProgressEvent);
begin
  DoEncodeDecodeStream(Source, Dest, DataSize,
                       Decode, OnProgress);
end;

procedure TDECFormattedCipher.DoEncodeDecodeFile(const SourceFileName, DestFileName: string;
                                                 const Proc: TDECCipherCodeEvent;
                                                 const OnProgress: TDECProgressEvent);
var
  S, D: TStream;
begin
  Assert(SourceFileName <> DestFileName, 'Source and Dest file name may not be equal');

  S := TFileStream.Create(SourceFileName, fmOpenRead or fmShareDenyNone);
  try
    D := TFileStream.Create(DestFileName, fmCreate);
    try
      DoEncodeDecodeStream(S, D, S.Size, Proc, OnProgress);
    finally
      D.Free;
    end;
  finally
    S.Free;
  end;
end;

procedure TDECFormattedCipher.EncodeFile(const SourceFileName, DestFileName: string;
                                         const OnProgress: TDECProgressEvent);
begin
  DoEncodeDecodeFile(SourceFileName, DestFileName, Encode, OnProgress);
end;

procedure TDECFormattedCipher.DecodeFile(const SourceFileName, DestFileName: string;
                                         const OnProgress: TDECProgressEvent);
begin
  DoEncodeDecodeFile(SourceFileName, DestFileName, Decode, OnProgress);
end;

function TDECFormattedCipher.EncodeStringToBytes(const Source: string;
                                                 Format: TDECFormatClass = nil): TBytes;
var
  Len: Integer;
begin
  if Length(Source) > 0 then
  begin
    {$IF CompilerVersion >= 24.0}
    Len := Length(Source) * SizeOf(Source[low(Source)]);
    SetLength(Result, Len);
    Encode(Source[low(Source)], Result[0], Len);
    {$ELSE}
    Len := Length(Source) * SizeOf(Source[1]);
    SetLength(Result, Len);
    Encode(Source[1], Result[0], Len);
    {$IFEND}

    Result := ValidFormat(Format).Encode(Result);
  end
  else
    SetLength(Result, 0);
end;

function TDECFormattedCipher.EncodeStringToBytes(const Source: RawByteString; Format: TDECFormatClass): TBytes;
var
  Len: Integer;
begin
  if Length(Source) > 0 then
  begin
    {$IF CompilerVersion >= 24.0}
    Len := Length(Source) * SizeOf(Source[low(Source)]);
    SetLength(Result, Len);
    Encode(Source[low(Source)], Result[0], Len);
    {$ELSE}
    Len := Length(Source) * SizeOf(Source[1]);
    SetLength(Result, Len);
    Encode(Source[1], Result[0], Len);
    {$IFEND}

    Result := ValidFormat(Format).Encode(Result);
  end
  else
    SetLength(Result, 0);
end;

function TDECFormattedCipher.DecodeStringToBytes(const Source: string; Format: TDECFormatClass): TBytes;
var
  Len: Integer;
  Src: TBytes;
begin
  if Length(Source) > 0 then
  begin
    Src := ValidFormat(Format).Decode(BytesOf(Source));

    Len := Length(Src);
    Result := Src;
    Decode(Result[0], Result[0], Len);
  end
  else
    SetLength(Result, 0);
end;

function TDECFormattedCipher.DecodeStringToBytes(const Source: RawByteString; Format: TDECFormatClass): TBytes;
var
  Len: Integer;
  Src: TBytes;
begin
  if Length(Source) > 0 then
  begin
    Src := ValidFormat(Format).Decode(BytesOf(Source));

    Len := Length(Src);
    Result := Src;
    Decode(Result[0], Result[0], Len);
  end
  else
    SetLength(Result, 0);
end;

{$IFDEF ANSISTRINGSUPPORTED}
function TDECFormattedCipher.EncodeStringToBytes(const Source: AnsiString; Format: TDECFormatClass): TBytes;
var
  Len: Integer;
begin
  if Length(Source) > 0 then
  begin
    Len := Length(Source) * SizeOf(Source[1]);
    SetLength(Result, Len);
    Encode(Source[1], Result[0], Len);

    Result := ValidFormat(Format).Encode(Result);
  end
  else
    SetLength(Result, 0);
end;
{$ENDIF}

{$IFDEF ANSISTRINGSUPPORTED}
function TDECFormattedCipher.DecodeStringToBytes(const Source: AnsiString; Format: TDECFormatClass): TBytes;
var
  Len: Integer;
  Src: TBytes;
begin
  if Length(Source) > 0 then
  begin
    Src := ValidFormat(Format).Decode(SysUtils.BytesOf(Source));

    Len := Length(Src);
    SetLength(Result, Len);
    Decode(Src[0], Result[0], Len);
  end
  else
    SetLength(Result, 0);
end;
{$ENDIF}

{$IFNDEF NEXTGEN}
function TDECFormattedCipher.EncodeStringToBytes(const Source: WideString; Format: TDECFormatClass): TBytes;
var
  Len: Integer;
begin
  if Length(Source) > 0 then
  begin
    Len := Length(Source) * SizeOf(Source[1]);
    SetLength(Result, Len);
    Encode(Source[1], Result[0], Len);

    Result := ValidFormat(Format).Encode(Result);
  end
  else
    SetLength(Result, 0);
end;

function TDECFormattedCipher.EncodeStringToString(const Source: WideString;
  Format: TDECFormatClass): WideString;
begin
  result := WideString(EncodeStringToString(string(Source), Format));
end;
{$ENDIF}

{$IFDEF ANSISTRINGSUPPORTED}
function TDECFormattedCipher.EncodeStringToString(const Source: AnsiString;
  Format: TDECFormatClass): AnsiString;
var
  Len             : Integer;
  EncryptedBuffer : TBytes;
  Temp            : TBytes;
begin
  if Length(Source) > 0 then
  begin
    Len := Length(Source) * SizeOf(Source[1]);
    SetLength(EncryptedBuffer, Len);
    Encode(Source[1], EncryptedBuffer[0], Len);

    Temp := ValidFormat(Format).Encode(EncryptedBuffer);
    SetLength(Result, length(Temp));
    Move(Temp[0], Result[1], length(Temp));
  end
  else
    SetLength(Result, 0);
end;
{$ENDIF}

function TDECFormattedCipher.EncodeStringToString(const Source: string;
  Format: TDECFormatClass): string;
var
  SourceSize      : Integer;
  EncryptedBuffer : TBytes;
begin
  if Length(Source) > 0 then
  begin
    {$IF CompilerVersion >= 24.0}
    SourceSize := Length(Source) * SizeOf(Source[low(Source)]);
    SetLength(EncryptedBuffer, SourceSize);
    Encode(Source[low(Source)], EncryptedBuffer[0], SourceSize);
    {$ELSE}
    SourceSize := Length(Source) * SizeOf(Source[1]);
    SetLength(EncryptedBuffer, SourceSize);
    Encode(Source[1], EncryptedBuffer[0], SourceSize);
    {$IFEND}

    Result := StringOf(ValidFormat(Format).Encode(EncryptedBuffer));
  end
  else
    Result := '';
end;

function TDECFormattedCipher.EncodeStringToString(const Source: RawByteString;
  Format: TDECFormatClass): RawByteString;
var
  SourceSize      : Integer;
  EncryptedBuffer : TBytes;
  Temp            : TBytes;
begin
  if Length(Source) > 0 then
  begin
    {$IF CompilerVersion >= 24.0}
    SourceSize := Length(Source) * SizeOf(Source[low(Source)]);
    SetLength(EncryptedBuffer, SourceSize);
    Encode(Source[low(Source)], EncryptedBuffer[0], SourceSize);
    {$ELSE}
    SourceSize := Length(Source) * SizeOf(Source[1]);
    SetLength(EncryptedBuffer, SourceSize);
    Encode(Source[1], EncryptedBuffer[0], SourceSize);
    {$IFEND}

    Temp   := ValidFormat(Format).Encode(EncryptedBuffer);
    SetLength(Result, length(Temp));
    {$IF CompilerVersion >= 24.0}
    Move(Temp[0], Result[low(Result)], length(Temp))
    {$ELSE}
    Move(Temp[0], Result[1], length(Temp))
    {$IFEND}
  end
  else
    Result := '';
end;

{$IFNDEF NEXTGEN}
function TDECFormattedCipher.DecodeStringToBytes(const Source: WideString; Format: TDECFormatClass): TBytes;
var
  Len: Integer;
  Src: TBytes;
begin
  if Length(Source) > 0 then
  begin
    Src := ValidFormat(Format).Decode(BytesOf(Source));

    Len := Length(Src);
    SetLength(Result, Len);
    Decode(Src[0], Result[0], Len);
  end
  else
    SetLength(Result, 0);
end;
{$ENDIF}

{$IFDEF ANSISTRINGSUPPORTED}
function TDECFormattedCipher.DecodeStringToString(const Source: AnsiString;
  Format: TDECFormatClass): AnsiString;
var
  Len : Integer;
  Src : TBytes;
  Tmp : TBytes;
begin
  if Length(Source) > 0 then
  begin
    Src := ValidFormat(Format).Decode(SysUtils.BytesOf(Source));

    Len := Length(Src);
    SetLength(Tmp, Len);
    Decode(Src[0], Tmp[0], Len);

    SetLength(Result, length(Tmp));

    {$IF CompilerVersion >= 24.0}
    Move(Tmp[0], Result[low(Result)], length(Tmp))
    {$ELSE}
    Move(Tmp[0], Result[1], length(Tmp))
    {$IFEND}
  end
  else
    SetLength(Result, 0);
end;
{$ENDIF}

{$IFNDEF NEXTGEN}
function TDECFormattedCipher.DecodeStringToString(const Source: WideString;
  Format: TDECFormatClass): WideString;
begin
  Result := WideString(DecodeStringToString(string(Source), Format));
end;
{$ENDIF}

function TDECFormattedCipher.DecodeStringToString(const Source: RawByteString;
  Format: TDECFormatClass): RawByteString;
var
  Len : Integer;
  Src : TBytes;
  Tmp : TBytes;
begin
  if Length(Source) > 0 then
  begin
    Src := ValidFormat(Format).Decode(BytesOf(Source));

    Len := Length(Src);
    SetLength(Tmp, Len);
    Decode(Src[0], Tmp[0], Len);

    SetLength(Result, length(Tmp));

    {$IF CompilerVersion >= 24.0}
    Move(Tmp[0], Result[low(Result)], length(Tmp))
    {$ELSE}
    Move(Tmp[0], Result[1], length(Tmp))
    {$IFEND}
  end
  else
    SetLength(Result, 0);
end;

function TDECFormattedCipher.DecodeStringToString(const Source: string;
  Format: TDECFormatClass): string;
var
  Len : Integer;
  Src : TBytes;
  Tmp : TBytes;
begin
  if Length(Source) > 0 then
  begin
    Src := ValidFormat(Format).Decode(BytesOf(Source));

    Len := Length(Src);
    SetLength(Tmp, Len);
    Decode(Src[0], Tmp[0], Len);
    Result := WideStringOf(Tmp);
  end
  else
    SetLength(Result, 0);
end;

end.
