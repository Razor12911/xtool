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
unit DECCipherModes;

interface

{$INCLUDE DECOptions.inc}

uses
  {$IFDEF FPC}
  SysUtils,
  {$ELSE}
  System.SysUtils,
  {$ENDIF}
  DECCipherBase;

{$I DECOptions.inc}

type
  /// <summary>
  ///   Most ciphers are block oriented and thus work on blocks of a fixed size.
  ///   In order to not encrypt each block separately without any link to his
  ///   predecessor and sucessor, which would make attacks on the encrypted data
  ///   easier, each block should be linked with his predecessor (or the
  ///   initialization vector). This class implements the various supported
  ///   algorithms for linking blocks.
  /// </summary>
  TDECCipherModes = class(TDECCipher)
  strict protected
    /// <summary>
    ///   Raises an EDECCipherException exception and provides the correct values
    ///   for message lenght and block size
    /// </summary>
    procedure ReportInvalidMessageLength(Cipher: TDECCipher);
    /// <summary>
    ///   Electronic Code Book
    ///   Mode cmECBx needs message padding to be a multiple of Cipher.BlockSize
    ///   and should be used only in 1-byte Streamciphers.
    ///   This one works on Blocks of Cipher.BufferSize bytes, when using a
    ///   Blockcipher that's equal to Cipher.BlockSize.
    /// </summary>
    /// <remarks>
    ///   This mode should not be used in practice, as it makes the encrypted
    ///   message vulnerable to certain attacks without knowing the encryption key
    /// </remarks>
    procedure EncodeECBx(Source, Dest: PByteArray; Size: Integer); virtual;
    /// <summary>
    ///   8bit Output Feedback mode, needs no padding
    /// </summary>
    procedure EncodeOFB8(Source, Dest: PByteArray; Size: Integer); virtual;
    /// <summary>
    ///   8 bit Cipher Feedback mode, needs no padding and works on 8 bit
    ///   Feedback Shift Registers.
    /// </summary>
    procedure EncodeCFB8(Source, Dest: PByteArray; Size: Integer); virtual;
    /// <summary>
    ///   8Bit CFS, double Cipher Feedback mode (CFB), needs no padding and
    ///   works on 8 bit Feedback Shift Registers.
    ///   This one is a proprietary mode developed by Hagen Reddmann. This mode
    ///   works as cmCBCx, cmCFBx, cmCFB8 but with double XOR'ing of the
    ///   inputstream into Feedback register.
    /// </summary>
    procedure EncodeCFS8(Source, Dest: PByteArray; Size: Integer); virtual;
    /// <summary>
    ///   Cipher Feedback mode (CFB) on Blocksize of Cipher, needs no padding
    ///   This one works on Blocks of Cipher.BufferSize bytes, when using a
    ///   Blockcipher that's equal to Cipher.BlockSize.
    /// </summary>
    procedure EncodeCFBx(Source, Dest: PByteArray; Size: Integer); virtual;
    /// <summary>
    ///   Output Feedback mode on Blocksize of Cipher, needs no padding and
    ///   works on 8 bit Feedback Shift Registers.
    ///   This one works on Blocks of Cipher.BufferSize bytes, when using a
    ///   Blockcipher that's equal to Cipher.BlockSize.
    /// </summary>
    procedure EncodeOFBx(Source, Dest: PByteArray; Size: Integer); virtual;
    /// <summary>
    ///   double Cipher Feedback mode (CFB) on Blocksize of Cipher, needs no padding.
    ///   This one works on Blocks of Cipher.BufferSize bytes, when using a
    ///   Blockcipher that's equal to Cipher.BlockSize.
    ///   This one is a proprietary mode developed by Hagen Reddmann. This mode
    ///   works as cmCBCx, cmCFBx, cmCFB8 but with double XOR'ing of the
    ///   inputstream into Feedback register.
    /// </summary>
    procedure EncodeCFSx(Source, Dest: PByteArray; Size: Integer); virtual;
    /// <summary>
    ///   Cipher Block Chaining, with CFB8 padding of truncated final block
    ///   It needs no external padding, because internally the last
    ///   truncated block is padded by cmCFS8 or cmCFB8. After padding these Modes
    ///   cannot be used to process any more data. If needed to process chunks of
    ///   data then each chunk must be aligned to Cipher.BufferSize bytes.
    ///   This one works on Blocks of Cipher.BufferSize bytes, when using a
    ///   Blockcipher that's equal to Cipher.BlockSize.
    /// </summary>
    procedure EncodeCBCx(Source, Dest: PByteArray; Size: Integer); virtual;
    /// <summary>
    ///   double CBC, with CFS8 padding of truncated final block
    ///   It needs no external padding, because internally the last
    ///   truncated block is padded by cmCFS8 or cmCFB8. After padding these Modes
    ///   cannot be used to process any more data. If needed to process chunks of
    ///   data then each chunk must be aligned to Cipher.BufferSize bytes.
    ///   This one works on Blocks of Cipher.BufferSize bytes, when using a
    ///   Blockcipher that's equal to Cipher.BlockSize.
    ///   This one is a proprietary mode developed by Hagen Reddmann. This mode
    ///   works as cmCBCx, cmCFBx, cmCFB8 but with double XOR'ing of the
    ///   inputstream into Feedback register.
    /// </summary>
    procedure EncodeCTSx(Source, Dest: PByteArray; Size: Integer); virtual;
    {$IFDEF DEC3_CMCTS}
    /// <summary>
    ///   double CBC, with
    ///   for DEC 3.0 compatibility only
    ///   This is a proprietary mode developed by Frederik Winkelsdorf. It
    ///   replaces the CFS8 padding of the truncated final block with a CFSx padding.
    ///   Useful when converting projects that previously used the old DEC v3.0. It
    ///   has the same restrictions for external padding and chunk processing as
    ///   cmCTSx has. It has a less secure padding of the truncated final block.
    ///   (to enable it see DECOptions.inc)
    /// </summary>
    procedure EncodeCTS3(Source, Dest: PByteArray; Size: Integer); virtual;
    {$ENDIF}
    /// <summary>
    ///   Electronic Code Book
    ///   Mode cmECBx needs message padding to be a multiple of Cipher.BlockSize
    ///   and should be used only in 1-byte Streamciphers.
    ///   This one works on blocks of Cipher.BufferSize bytes, when using a
    ///   blockcipher that's equal to Cipher.BlockSize.
    /// </summary>
    procedure DecodeECBx(Source, Dest: PByteArray; Size: Integer); virtual;
    /// <summary>
    ///   8 bit Output Feedback mode, needs no padding
    /// </summary>
    procedure DecodeOFB8(Source, Dest: PByteArray; Size: Integer); virtual;
    /// <summary>
    ///   8 bit Cipher Feedback mode, needs no padding and works on 8 bit
    ///   Feedback Shift Registers.
    /// </summary>
    procedure DecodeCFB8(Source, Dest: PByteArray; Size: Integer); virtual;
    /// <summary>
    ///   8 Bit CFS, double Cipher Feedback mode (CFB), needs no padding and
    ///   works on 8 bit Feedback Shift Registers.
    ///   This one is a proprietary mode developed by Hagen Reddmann. This mode
    ///   works as cmCBCx, cmCFBx, cmCFB8 but with double XOR'ing of the
    ///   inputstream into Feedback register.
    /// </summary>
    procedure DecodeCFS8(Source, Dest: PByteArray; Size: Integer); virtual;
    /// <summary>
    ///   Cipher Feedback mode (CFB) on Blocksize of Cipher, needs no padding
    ///   This one works on blocks of Cipher.BufferSize bytes, when using a
    ///   blockcipher that's equal to Cipher.BlockSize.
    /// </summary>
    procedure DecodeCFBx(Source, Dest: PByteArray; Size: Integer); virtual;
    /// <summary>
    ///   Output Feedback mode on Blocksize of Cipher, needs no padding and
    ///   works on 8 bit Feedback Shift Registers.
    ///   This one works on blocks of Cipher.BufferSize bytes, when using a
    ///   blockcipher that's equal to Cipher.BlockSize.
    /// </summary>
    procedure DecodeOFBx(Source, Dest: PByteArray; Size: Integer); virtual;
    /// <summary>
    ///   double Cipher Feedback mode (CFB) on Blocksize of Cipher, needs no padding.
    ///   This one works on blocks of Cipher.BufferSize bytes, when using a
    ///   blockcipher that's equal to Cipher.BlockSize.
    ///   This one is a proprietary mode developed by Hagen Reddmann. This mode
    ///   works as cmCBCx, cmCFBx, cmCFB8 but with double XOR'ing of the
    ///   inputstream into Feedback register.
    /// </summary>
    procedure DecodeCFSx(Source, Dest: PByteArray; Size: Integer); virtual;
    /// <summary>
    ///   Cipher Block Chaining, with CFB8 padding of truncated final block.
    ///   It needs no external padding, because internally the last
    ///   truncated block is padded by cmCFS8 or cmCFB8. After padding these modes
    ///   cannot be used to process any more data. If needed to process chunks of
    ///   data then each chunk must be algined to Cipher.BufferSize bytes.
    ///   This one works on blocks of Cipher.BufferSize bytes, when using a
    ///   blockcipher that's equal to Cipher.BlockSize.
    /// </summary>
    procedure DecodeCBCx(Source, Dest: PByteArray; Size: Integer); virtual;
    /// <summary>
    ///   double CBC, with CFS8 padding of truncated final block
    ///   It needs no external padding, because internally the last
    ///   truncated block is padded by cmCFS8 or cmCFB8. After padding these Modes
    ///   cannot be used to process any more data. If needed to process chunks of
    ///   data then each chunk must be algined to Cipher.BufferSize bytes.
    ///   This one works on blocks of Cipher.BufferSize bytes, when using a
    ///   blockcipher that's equal to Cipher.BlockSize.
    ///   This one is a proprietary mode developed by Hagen Reddmann. This mode
    ///   works as cmCBCx, cmCFBx, cmCFB8 but with double XOR'ing of the
    ///   inputstream into feedback register.
    /// </summary>
    procedure DecodeCTSx(Source, Dest: PByteArray; Size: Integer); virtual;
    {$IFDEF DEC3_CMCTS}
    /// <summary>
    ///   double CBC
    ///   This is a proprietary mode developed by Frederik Winkelsdorf. It
    ///   replaces the CFS8 padding of the truncated final block with a CFSx padding.
    ///   Useful when converting projects that previously used the old DEC v3.0. It
    ///   has the same restrictions for external padding and chunk processing as
    ///   cmCTSx has. It has a less secure padding of the truncated final block.
    ///   (to enable it see DECOptions.inc)
    /// </summary>
    /// <remarks>
    ///   For DEC 3.0 compatibility only
    /// </remarks>
    procedure DecodeCTS3(Source, Dest: PByteArray; Size: Integer); virtual;
    {$ENDIF}
  public
    /// <summary>
    ///   Encrypts a given block of data
    /// </summary>
    /// <param name="Source">
    ///   Data to be encrypted
    /// </param>
    /// <param name="Dest">
    ///   Data after encryption
    /// </param>
    /// <param name="DataSize">
    ///   Size of the data the Source parameter points to in byte
    /// </param>
    procedure Encode(const Source; var Dest; DataSize: Integer);
    /// <summary>
    ///   Decrypts a given block of data
    /// </summary>
    /// <param name="Source">
    ///   Data to be Decrypted
    /// </param>
    /// <param name="Dest">
    ///   Data after decryption
    /// </param>
    /// <param name="DataSize">
    ///   Size of the data the Source parameter points to in byte
    /// </param>
    procedure Decode(const Source; var Dest; DataSize: Integer);
  end;

implementation

uses
  {$IFDEF FPC}
  TypInfo,
  {$ELSE}
  System.TypInfo,
  {$ENDIF}
  DECUtil;

resourcestring
  sInvalidMessageLength = 'Message length for mode %0:s must be a multiple of %1:d bytes';

procedure TDECCipherModes.ReportInvalidMessageLength(Cipher: TDECCipher);
begin
  raise EDECCipherException.CreateResFmt(@sInvalidMessageLength,
                                         [System.TypInfo.GetEnumName(TypeInfo(TCipherMode),
                                         Integer(Cipher.Mode)),
                                         Cipher.Context.BlockSize]);
end;

procedure TDECCipherModes.Encode(const Source; var Dest; DataSize: Integer);
begin
  CheckState([csInitialized, csEncode, csDone]);

  case FMode of
    cmECBx:   EncodeECBx(@Source, @Dest, DataSize);
    cmCBCx:   EncodeCBCx(@Source, @Dest, DataSize);
    cmCTSx:   EncodeCTSx(@Source, @Dest, DataSize);
    {$IFDEF DEC3_CMCTS}
    cmCTS3:   EncodeCTS3(@Source, @Dest, DataSize);
    {$ENDIF DEC3_CMCTS}
    cmCFB8:   EncodeCFB8(@Source, @Dest, DataSize);
    cmCFBx:   EncodeCFBx(@Source, @Dest, DataSize);
    cmOFB8:   EncodeOFB8(@Source, @Dest, DataSize);
    cmOFBx:   EncodeOFBx(@Source, @Dest, DataSize);
    cmCFS8:   EncodeCFS8(@Source, @Dest, DataSize);
    cmCFSx:   EncodeCFSx(@Source, @Dest, DataSize);
  end;
end;

procedure TDECCipherModes.EncodeECBx(Source, Dest: PByteArray; Size: Integer);
var
  I: Integer;
begin
  if Context.BlockSize = 1 then
  begin
    DoEncode(Source, Dest, Size);
    FState := csEncode;
  end
  else
  begin
    Dec(Size, FBufferSize);
    I := 0;
    while I <= Size do
    begin
      DoEncode(@Source[I], @Dest[I], FBufferSize);
      Inc(I, FBufferSize);
    end;
    Dec(Size, I - FBufferSize);
    if Size > 0 then
    begin
      if Size mod Context.BlockSize = 0 then
      begin
        DoEncode(@Source[I], @Dest[I], Size);
        FState := csEncode;
      end
      else
      begin
        FState := csPadded;
        ReportInvalidMessageLength(Self);
      end;
    end;
  end;
end;

procedure TDECCipherModes.EncodeOFB8(Source, Dest: PByteArray; Size: Integer);
var
  I: Integer;
begin
  I := 0;
  while I < Size do
  begin
    DoEncode(FFeedback, FBuffer, FBufferSize);
    Move(FFeedback[1], FFeedback[0], FBufferSize - 1);
    FFeedback[FBufferSize - 1] := FBuffer[0];
    Dest[I] := Source[I] xor FBuffer[0];
    Inc(I);
  end;
  FState := csEncode;
end;

procedure TDECCipherModes.EncodeCFB8(Source, Dest: PByteArray; Size: Integer);
// CFB-8
var
  I: Integer;
begin
  I := 0;
  while I < Size do
  begin
    DoEncode(FFeedback, FBuffer, FBufferSize);
    Move(FFeedback[1], FFeedback[0], FBufferSize - 1);
    Dest[I] := Source[I] xor FBuffer[0];
    FFeedback[FBufferSize - 1] := Dest[I];
    Inc(I);
  end;
  FState := csEncode;
end;

procedure TDECCipherModes.EncodeCFS8(Source, Dest: PByteArray; Size: Integer);
// CFS-8, CTS as CFB
var
  I: Integer;
begin
  I := 0;
  while I < Size do
  begin
    DoEncode(FFeedback, FBuffer, FBufferSize);
    Dest[I] := Source[I] xor FBuffer[0];
    Move(FFeedback[1], FFeedback[0], FBufferSize - 1);
    FFeedback[FBufferSize - 1] := FFeedback[FBufferSize - 1] xor Dest[I];
    Inc(I);
  end;
  FState := csEncode;
end;

procedure TDECCipherModes.EncodeCFBx(Source, Dest: PByteArray; Size: Integer);
// CFB-BlockSize
var
  I: Integer;
  F: PByteArray;
begin
  FState := csEncode;
  if FBufferIndex > 0 then
  begin
    I := FBufferSize - FBufferIndex;
    if I > Size then
      I := Size;
    XORBuffers(Source[0], FBuffer[FBufferIndex], I, Dest[0]);
    Move(Dest[0], FFeedback[FBufferIndex], I);
    Inc(FBufferIndex, I);
    if FBufferIndex < FBufferSize then
      Exit;
    Dec(Size, I);
    Source := @Source[I];
    Dest := @Dest[I];
    FBufferIndex := 0
  end;
  Dec(Size, FBufferSize);
  F := FFeedback;
  I := 0;
  while I < Size do
  begin
    DoEncode(F, FBuffer, FBufferSize);
    XORBuffers(Source[I], FBuffer[0], FBufferSize, Dest[I]);
    F := @Dest[I];
    Inc(I, FBufferSize);
  end;
  if F <> FFeedback then
    Move(F^, FFeedback^, FBufferSize);
  Dec(Size, I - FBufferSize);
  if Size > 0 then
  begin
    DoEncode(FFeedback, FBuffer, FBufferSize);
    XORBuffers(Source[I], FBuffer[0], Size, Dest[I]);
    Move(Dest[I], FFeedback[0], Size);
    FBufferIndex := Size;
  end;
end;

procedure TDECCipherModes.EncodeOFBx(Source, Dest: PByteArray; Size: Integer);
// OFB-BlockSize
var
  I: Integer;
begin
  FState := csEncode;
  if FBufferIndex > 0 then
  begin
    I := FBufferSize - FBufferIndex;
    if I > Size then
      I := Size;
    XORBuffers(Source[0], FFeedback[FBufferIndex], I, Dest[0]);
    Inc(FBufferIndex, I);
    if FBufferIndex < FBufferSize then
      Exit;
    Dec(Size, I);
    Source := @Source[I];
    Dest := @Dest[I];
    FBufferIndex := 0
  end;
  Dec(Size, FBufferSize);
  I := 0;
  while I < Size do
  begin
    DoEncode(FFeedback, FFeedback, FBufferSize);
    XORBuffers(Source[I], FFeedback[0], FBufferSize, Dest[I]);
    Inc(I, FBufferSize);
  end;
  Dec(Size, I - FBufferSize);
  if Size > 0 then
  begin
    DoEncode(FFeedback, FFeedback, FBufferSize);
    XORBuffers(Source[I], FFeedback[0], Size, Dest[I]);
    FBufferIndex := Size;
  end;
end;

procedure TDECCipherModes.EncodeCFSx(Source, Dest: PByteArray; Size: Integer);
// CFS-BlockSize
var
  I: Integer;
begin
  FState := csEncode;
  if FBufferIndex > 0 then
  begin
    I := FBufferSize - FBufferIndex;
    if I > Size then
      I := Size;
    XORBuffers(Source[0], FBuffer[FBufferIndex], I, Dest[0]);
    XORBuffers(Dest[0], FFeedback[FBufferIndex], I, FFeedback[FBufferIndex]);
    Inc(FBufferIndex, I);
    if FBufferIndex < FBufferSize then
      Exit;
    Dec(Size, I);
    Source := @Source[I];
    Dest := @Dest[I];
    FBufferIndex := 0
  end;
  Dec(Size, FBufferSize);
  I := 0;
  while I < Size do
  begin
    DoEncode(FFeedback, FBuffer, FBufferSize);
    XORBuffers(Source[I], FBuffer[0], FBufferSize, Dest[I]);
    XORBuffers(Dest[I], FFeedback[0], FBufferSize, FFeedback[0]);
    Inc(I, FBufferSize);
  end;
  Dec(Size, I - FBufferSize);
  if Size > 0 then
  begin
    DoEncode(FFeedback, FBuffer, FBufferSize);
    XORBuffers(Source[I], FBuffer[0], Size, Dest[I]);
    XORBuffers(Dest[I], FFeedback[0], Size, FFeedback[0]);
    FBufferIndex := Size;
  end;
end;

procedure TDECCipherModes.EncodeCBCx(Source, Dest: PByteArray; Size: Integer);
var
  F: PByteArray;
  I: Integer;
begin
  Dec(Size, FBufferSize);
  F := FFeedback;
  I := 0;
  while I <= Size do
  begin
    XORBuffers(Source[I], F[0], FBufferSize, Dest[I]);
    F := @Dest[I];
    DoEncode(F, F, FBufferSize);
    Inc(I, FBufferSize);
  end;
  if F <> FFeedback then
    Move(F[0], FFeedback[0], FBufferSize);
  Dec(Size, I - FBufferSize);
  if Size > 0 then
  begin  // padding
    EncodeCFB8(@Source[I], @Dest[I], Size);
    FState := csPadded;
  end
  else
    FState := csEncode;
end;

procedure TDECCipherModes.EncodeCTSx(Source, Dest: PByteArray; Size: Integer);
var
  I: Integer;
begin
  Dec(Size, FBufferSize);
  I := 0;
  while I <= Size do
  begin
    XORBuffers(Source[I], FFeedback[0], FBufferSize, Dest[I]);
    DoEncode(@Dest[I], @Dest[I], FBufferSize);
    XORBuffers(Dest[I], FFeedback[0], FBufferSize, FFeedback[0]);
    Inc(I, FBufferSize);
  end;
  Dec(Size, I - FBufferSize);
  if Size > 0 then
  begin // padding
    EncodeCFS8(@Source[I], @Dest[I], Size);
    FState := csPadded;
  end
  else
    FState := csEncode;
end;

{$IFDEF DEC3_CMCTS}
procedure TDECCipherModes.EncodeCTS3(Source, Dest: PByteArray; Size: Integer);
var
  I: Integer;
begin
  Dec(Size, FBufferSize);
  I := 0;
  while I <= Size do
  begin
    XORBuffers(Source[I], FFeedback[0], FBufferSize, Dest[I]);
    DoEncode(@Dest[I], @Dest[I], FBufferSize);
    XORBuffers(Dest[I], FFeedback[0], FBufferSize, FFeedback[0]);
    Inc(I, FBufferSize);
  end;
  Dec(Size, I - FBufferSize);
  if Size > 0 then
  begin // padding
    EncodeCFSx(@Source[I], @Dest[I], Size); // use the padding implemented in CFSx
    FState := csPadded;
  end
  else
    FState := csEncode;
end;
{$ENDIF DEC3_CMCTS}

procedure TDECCipherModes.Decode(const Source; var Dest; DataSize: Integer);
begin
  CheckState([csInitialized, csDecode, csDone]);

  case FMode of
    cmECBx:   DecodeECBx(@Source, @Dest, DataSize);
    cmCBCx:   DecodeCBCx(@Source, @Dest, DataSize);
    cmCTSx:   DecodeCTSx(@Source, @Dest, DataSize);
    {$IFDEF DEC3_CMCTS}
    cmCTS3:   DecodeCTS3(@Source, @Dest, DataSize);
    {$ENDIF DEC3_CMCTS}
    cmCFB8:   DecodeCFB8(@Source, @Dest, DataSize);
    cmCFBx:   DecodeCFBx(@Source, @Dest, DataSize);
    cmOFB8:   DecodeOFB8(@Source, @Dest, DataSize);
    cmOFBx:   DecodeOFBx(@Source, @Dest, DataSize);
    cmCFS8:   DecodeCFS8(@Source, @Dest, DataSize);
    cmCFSx:   DecodeCFSx(@Source, @Dest, DataSize);
  end;
end;

procedure TDECCipherModes.DecodeECBx(Source, Dest: PByteArray; Size: Integer);
var
  I: Integer;
begin
  if Context.BlockSize = 1 then
  begin
    DoDecode(Source, Dest, Size);
    FState := csDecode;
  end
  else
  begin
    Dec(Size, FBufferSize);
    I := 0;
    while I <= Size do
    begin
      DoDecode(@Source[I], @Dest[I], FBufferSize);
      Inc(I, FBufferSize);
    end;
    Dec(Size, I - FBufferSize);
    if Size > 0 then
    begin
      if Size mod Context.BlockSize = 0 then
      begin
        DoDecode(@Source[I], @Dest[I], Size);
        FState := csDecode;
      end
      else
      begin
        FState := csPadded;
        ReportInvalidMessageLength(Self);
      end;
    end;
  end;
end;

procedure TDECCipherModes.DecodeCFB8(Source, Dest: PByteArray; Size: Integer);
// CFB-8
var
  I: Integer;
begin
  I := 0;
  while I < Size do
  begin
    DoEncode(FFeedback, FBuffer, FBufferSize);
    Move(FFeedback[1], FFeedback[0], FBufferSize - 1);
    FFeedback[FBufferSize - 1] := Source[I];
    Dest[I] := Source[I] xor FBuffer[0];
    Inc(I);
  end;
  FState := csDecode;
end;

procedure TDECCipherModes.DecodeOFB8(Source, Dest: PByteArray; Size: Integer);
// same as EncodeOFB
var
  I: Integer;
begin
  I := 0;
  while I < Size do
  begin
    DoEncode(FFeedback, FBuffer, FBufferSize);
    Move(FFeedback[1], FFeedback[0], FBufferSize - 1);
    FFeedback[FBufferSize - 1] := FBuffer[0];
    Dest[I] := Source[I] xor FBuffer[0];
    Inc(I);
  end;
  FState := csDecode;
end;

procedure TDECCipherModes.DecodeCFS8(Source, Dest: PByteArray; Size: Integer);
var
  I: Integer;
begin
  I := 0;
  while I < Size do
  begin
    DoEncode(FFeedback, FBuffer, FBufferSize);
    Move(FFeedback[1], FFeedback[0], FBufferSize - 1);
    FFeedback[FBufferSize - 1] := FFeedback[FBufferSize - 1] xor Source[I];
    Dest[I] := Source[I] xor FBuffer[0];
    Inc(I);
  end;
  FState := csDecode;
end;

procedure TDECCipherModes.DecodeCFBx(Source, Dest: PByteArray; Size: Integer);
// CFB-BlockSize
var
  I: Integer;
  F: PByteArray;
begin
  FState := csDecode;
  if FBufferIndex > 0 then
  begin // remaining bytes of last decode
    I := FBufferSize - FBufferIndex;
    if I > Size then
      I := Size;
    Move(Source[0], FFeedback[FBufferIndex], I);
    XORBuffers(Source[0], FBuffer[FBufferIndex], I, Dest[0]);
    Inc(FBufferIndex, I);
    if FBufferIndex < FBufferSize then
      Exit;
    Dec(Size, I);
    Source := @Source[I];
    Dest := @Dest[I];
    FBufferIndex := 0
  end;
  // process chunks of FBufferSize bytes
  Dec(Size, FBufferSize);
  I := 0;
  if Source <> Dest then
  begin
    F := FFeedback;
    while I < Size do
    begin
      DoEncode(F, FBuffer, FBufferSize);
      XORBuffers(Source[I], FBuffer[0], FBufferSize, Dest[I]);
      F := @Source[I];
      Inc(I, FBufferSize);
    end;
    if F <> FFeedback then
      Move(F^, FFeedback^, FBufferSize);
  end
  else
    while I < Size do
    begin
      DoEncode(FFeedback, FBuffer, FBufferSize);
      Move(Source[I], FFeedback[0], FBufferSize);
      XORBuffers(Source[I], FBuffer[0], FBufferSize, Dest[I]);
      Inc(I, FBufferSize);
    end;
  Dec(Size, I - FBufferSize);
  if Size > 0 then
  begin // remaining bytes
    DoEncode(FFeedback, FBuffer, FBufferSize);
    Move(Source[I], FFeedback[0], Size);
    XORBuffers(Source[I], FBuffer[0], Size, Dest[I]);
    FBufferIndex := Size;
  end;
end;

procedure TDECCipherModes.DecodeOFBx(Source, Dest: PByteArray; Size: Integer);
// OFB-BlockSize, same as EncodeOFBx
var
  I: Integer;
begin
  FState := csDecode;
  if FBufferIndex > 0 then
  begin
    I := FBufferSize - FBufferIndex;
    if I > Size then
      I := Size;
    XORBuffers(Source[0], FFeedback[FBufferIndex], I, Dest[0]);
    Inc(FBufferIndex, I);
    if FBufferIndex < FBufferSize then
      Exit;
    Dec(Size, I);
    Source := @Source[I];
    Dest := @Dest[I];
    FBufferIndex := 0
  end;
  Dec(Size, FBufferSize);
  I := 0;
  while I < Size do
  begin
    DoEncode(FFeedback, FFeedback, FBufferSize);
    XORBuffers(Source[I], FFeedback[0], FBufferSize, Dest[I]);
    Inc(I, FBufferSize);
  end;
  Dec(Size, I - FBufferSize);
  if Size > 0 then
  begin
    DoEncode(FFeedback, FFeedback, FBufferSize);
    XORBuffers(Source[I], FFeedback[0], Size, Dest[I]);
    FBufferIndex := Size;
  end;
end;

procedure TDECCipherModes.DecodeCFSx(Source, Dest: PByteArray; Size: Integer);
// CFS-BlockSize
var
  I: Integer;
begin
  FState := csDecode;
  if FBufferIndex > 0 then
  begin // remaining bytes of last decode
    I := FBufferSize - FBufferIndex;
    if I > Size then
      I := Size;
    XORBuffers(Source[0], FFeedback[FBufferIndex], I, FFeedback[FBufferIndex]);
    XORBuffers(Source[0], FBuffer[FBufferIndex], I, Dest[0]);
    Inc(FBufferIndex, I);
    if FBufferIndex < FBufferSize then
      Exit;
    Dec(Size, I);
    Source := @Source[I];
    Dest := @Dest[I];
    FBufferIndex := 0
  end;
  // process chunks of FBufferSize bytes
  Dec(Size, FBufferSize);
  I := 0;
  while I < Size do
  begin
    DoEncode(FFeedback, FBuffer, FBufferSize);
    XORBuffers(Source[I], FFeedback[0], FBufferSize, FFeedback[0]);
    XORBuffers(Source[I], FBuffer[0], FBufferSize, Dest[I]);
    Inc(I, FBufferSize);
  end;
  Dec(Size, I - FBufferSize);
  if Size > 0 then
  begin // remaining bytes
    DoEncode(FFeedback, FBuffer, FBufferSize);
    XORBuffers(Source[I], FFeedback[0], Size, FFeedback[0]);
    XORBuffers(Source[I], FBuffer[0], Size, Dest[I]);
    FBufferIndex := Size;
  end;
end;

procedure TDECCipherModes.DecodeCBCx(Source, Dest: PByteArray; Size: Integer);
var
  I: Integer;
  F, B, T: PByteArray;
begin
  Dec(Size, FBufferSize);
  F := FFeedback;
  I := 0;
  if Source = Dest then
  begin
    B := FBuffer;
    while I <= Size do
    begin
      Move(Source[I], B[0], FBufferSize);
      DoDecode(@Source[I], @Source[I], FBufferSize);
      XORBuffers(Source[I], F[0], FBufferSize, Source[I]);
      T := F;
      F := B;
      B := T;
      Inc(I, FBufferSize);
    end;
  end
  else
  begin
    while I <= Size do
    begin
      DoDecode(@Source[I], @Dest[I], FBufferSize);
      XORBuffers(F[0], Dest[I], FBufferSize, Dest[I]);
      F := @Source[I];
      Inc(I, FBufferSize);
    end;
  end;
  if F <> FFeedback then
    Move(F[0], FFeedback[0], FBufferSize);
  Dec(Size, I - FBufferSize);
  if Size > 0 then
  begin
    DecodeCFB8(@Source[I], @Dest[I], Size);
    FState := csPadded;
  end
  else
    FState := csDecode;
end;

procedure TDECCipherModes.DecodeCTSx(Source, Dest: PByteArray; Size: Integer);
var
  I: Integer;
  F, B, T: PByteArray;
begin
  Dec(Size, FBufferSize);
  F := FFeedback;
  B := FBuffer;
  I := 0;
  while I <= Size do
  begin
    XORBuffers(Source[I], F[0], FBufferSize, B[0]);
    DoDecode(@Source[I], @Dest[I], FBufferSize);
    XORBuffers(Dest[I], F[0], FBufferSize, Dest[I]);
    T := B;
    B := F;
    F := T;
    Inc(I, FBufferSize);
  end;
  if F <> FFeedback then
    Move(F[0], FFeedback[0], FBufferSize);
  Dec(Size, I - FBufferSize);
  if Size > 0 then
  begin
    DecodeCFS8(@Source[I], @Dest[I], Size);
    FState := csPadded;
  end
  else
    FState := csDecode;
end;

{$IFDEF DEC3_CMCTS}
procedure DecodeCTS3(Source, Dest: PByteArray; Size: Integer);
var
  I: Integer;
  F, B, T: PByteArray;
begin
  Dec(Size, FBufferSize);
  F := FFeedback;
  B := FBuffer;
  I := 0;
  while I <= Size do
  begin
    XORBuffers(Source[I], F[0], FBufferSize, B[0]);
    DoDecode(@Source[I], @Dest[I], FBufferSize);
    XORBuffers(Dest[I], F[0], FBufferSize, Dest[I]);
    T := B;
    B := F;
    F := T;
    Inc(I, FBufferSize);
  end;
  if F <> FFeedback then
    Move(F[0], FFeedback[0], FBufferSize);
  Dec(Size, I - FBufferSize);
  if Size > 0 then
  begin
    DecodeCFSx(@Source[I], @Dest[I], Size); // use the padding implemented in CFSx
    FState := csPadded;
  end
  else
    FState := csDecode;
end;
{$ENDIF DEC3_CMCTS}

end.
