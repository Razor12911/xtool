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
///   This unit provides a standardisized way for applying format conversions
///   to data
/// </summary>
unit DECFormat;

interface

{$INCLUDE DECOptions.inc}

uses
  {$IFDEF FPC}
  SysUtils, Classes,
  {$ELSE}
  System.SysUtils, System.Classes,
  {$ENDIF}
  DECBaseClass, DECFormatBase, DECUtil;

type
  /// <summary>
  ///   wrapper (allows omitting DECFormatBase in user code)
  /// </summary>
  TDECFormat          = DECFormatBase.TDECFormat;
  /// <summary>
  ///   wrapper (allows omitting DECFormatBase in user code)
  /// </summary>
  TDECFormatClass     = DECFormatBase.TDECFormatClass;
  /// <summary>
  ///   wrapper (allows omitting DECFormatBase in user code)
  /// </summary>
  TFormat_Copy        = DECFormatBase.TFormat_Copy;

  TFormat_HEX         = class;
  TFormat_HEXL        = class;

  TFormat_Base16      = class;
  TFormat_Base16L     = class;

  TFormat_DECMIME32   = class;

  TFormat_Base64      = class;
  TFormat_MIME64      = class;

  TFormat_Radix64     = class;
  TFormat_PGP         = class;

  TFormat_UU          = class;
  TFormat_XX          = class;
  TFormat_ESCAPE      = class;

  TFormat_BigEndian16 = class;
  TFormat_BigEndian32 = class;

  /// <summary>
  ///   Hexadecimal in Uppercase, Base16, see http://tools.ietf.org/html/rfc4648
  /// </summary>
  TFormat_HEX = class(TDECFormat)
  protected
    class procedure DoEncode(const Source; var Dest: TBytes; Size: Integer); override;
    class procedure DoDecode(const Source; var Dest: TBytes; Size: Integer); override;
    class function DoIsValid(const Data; Size: Integer): Boolean; override;
  public
    class function CharTableBinary: TBytes; virtual;
  end;

  /// <summary>
  ///   Hexadecimal in lowercase, Base16, see http://tools.ietf.org/html/rfc4648
  /// </summary>
  TFormat_HEXL = class(TFormat_HEX)
  public
    class function CharTableBinary: TBytes; override;
  end;

  /// <summary>
  ///   Same as TFormat_HEX, use TFormat_HEX instead
  /// </summary>
  TFormat_Base16 = class(TFormat_HEX)
  end deprecated 'Use TFormat_HEX instead';

  /// <summary>
  ///   Same as TFormat_HEXL, use TFormat_HEXL instead
  /// </summary>
  TFormat_Base16L = class(TFormat_HEXL)
  end deprecated 'Use TFormat_HEXL instead';

  /// <summary>
  ///   Proprietary variant of MINE32, kept for backwards compatibility with old
  ///   DEC versions
  /// </summary>
  /// <remarks>
  ///   This formatting should only be used for supporting legacy projects which
  ///   already use this format. It is being considered to be more or less deprecated.
  /// </remarks>
  TFormat_DECMIME32 = class(TFormat_HEX)
  protected
    /// <summary>
    ///   Encodes data passed to this method in the proprietary DECMIME32 format.
    /// </summary>
    /// <remarks>
    ///   This formatting should only be used for supporting legacy projects which
    ///   already use this format. It is being considered to be more or less deprecated.
    /// </remarks>
    class procedure DoEncode(const Source; var Dest: TBytes; Size: Integer); override;
    /// <summary>
    ///   Decodes data passed to this method in the proprietary DECMIME32 format
    ///   into an array of normal bytes.
    /// </summary>
    /// <remarks>
    ///   This formatting should only be used for supporting legacy projects which
    ///   already use this format. It is being considered to be more or less deprecated.
    /// </remarks>
    class procedure DoDecode(const Source; var Dest: TBytes; Size: Integer); override;
    /// <summary>
    ///   Checks if certain data adheres to the rules for this formatting.
    /// </summary>
    /// <remarks>
    ///   This formatting should only be used for supporting legacy projects which
    ///   already use this format. It is being considered to be more or less deprecated.
    /// </remarks>
    class function DoIsValid(const Data; Size: Integer): Boolean; override;

  public
    class function CharTableBinary: TBytes; override;
  end;

  /// <summary>
  ///   Same as DECMIME32, which itsself should only be used for legacy projects
  /// </summary>
  TFormat_MIME32 = class(TFormat_DECMIME32)
  end deprecated 'Use TFormat_DECMIME32 instead';

  /// <summary>
  ///   Base64 (without soft wraps), see http://tools.ietf.org/html/rfc4648
  /// </summary>
  TFormat_Base64 = class(TFormat_HEX)
  protected
    class procedure DoEncode(const Source; var Dest: TBytes; Size: Integer); override;
    class procedure DoDecode(const Source; var Dest: TBytes; Size: Integer); override;
    class function  DoIsValid(const Data; Size: Integer): Boolean; override;
  public
    class function CharTableBinary: TBytes; override;
  end;

  /// <summary>
  ///   Same as TFormat_Base64, use TFormat_Base64 instead, kept for backwards
  ///   compatibility only
  /// </summary>
  TFormat_MIME64 = class(TFormat_Base64)
  end deprecated 'Use TFormat_Base64 instead';

  /// <summary>
  ///   OpenPGP/PGP Base64 with 24-bit Checksums, see http://tools.ietf.org/html/rfc4880
  /// </summary>
  TFormat_Radix64 = class(TFormat_Base64)
  /// <summary>
  ///   Here the section needs to be private so that the variable can be accessed
  ///   for initialization in the initialization section, which is needed since
  ///   all functionality of the class is implemented as class methods
  /// </summary>
  private
    /// <summary>
    ///   Maximum number of chars for one line of message text
    /// </summary>
    class var FCharsPerLine : UInt32;
  protected
    /// <summary>
    ///   Extracts the CRC24 checksum from Radix64 encoded data
    /// </summary>
    /// <param name="Data">
    ///   Data to extract the checksum from
    /// </param>
    /// <param name="Size">
    ///   Size of the data in byte
    /// </param>
    /// <returns>
    ///   CRC24 checksum if present, otherwise $FFFFFFFF
    /// </returns>
    class function DoExtractCRC(const Data; var Size: Integer): UInt32;
    /// <summary>
    ///   If the data given exceeds FCharsPerLine, means the maximum allowed
    ///   line lenth, a CR/LF pair needs to be inserted at that position.
    /// </summary>
    /// <param name="Source">
    ///   Data to insert a CR/LF into if necessary
    /// </param>
    /// <param name="Dest">
    ///   In this byte array the processed data will be returned
    /// </param>
    /// <param name="LineLength">
    ///   Maximum length of a line in byte. At this position the CR/LF will be
    ///   inserted if the source passed in exceeds this length.
    /// </param>
    class procedure InsertCRLF(const Source: TBytes; var Dest: TBytes; LineLength: Integer);
    class procedure DoEncode(const Source; var Dest: TBytes; Size: Integer); override;
    class procedure DoDecode(const Source; var Dest: TBytes; Size: Integer); override;

    class function  DoIsValid(const Data; Size: Integer): Boolean; override;
  public
    /// <summary>
    ///   Changes the number of chars after which a line break is being added
    /// </summary>
    /// <param name="Value">
    ///   Maximum number of chars for a single line. Values < 1 result in an
    ///   EArgumentOutOfRangeException being raised
    /// </param>
    class procedure SetCharsPerLine(const Value: UInt32);
    /// <summary>
    ///   Returns the number of chars after which a line break will be introduced
    /// </summary>
    /// <returns>
    ///   Maximum number of chars per line
    /// </returns>
    /// <remarks>
    ///   Cannot be a property, as properties cannot access class vars
    /// </remarks>
    class function GetCharsPerLine: UInt32;
  end;

  /// <summary>
  ///   Same as TFormat_Radix64, use TFormat_Radix64 instead, kept for backwards
  ///   compatibility only
  /// </summary>
  TFormat_PGP = class(TFormat_Radix64)
  end deprecated 'Use TFormat_Radix64 instead';

  /// <summary>
  ///   Unix UU format
  /// </summary>
  TFormat_UU = class(TDECFormat)
  protected
    class procedure DoEncode(const Source; var Dest: TBytes; Size: Integer); override;
    class procedure DoDecode(const Source; var Dest: TBytes; Size: Integer); override;
    class function  DoIsValid(const Data; Size: Integer): Boolean; override;
  public
    class function CharTableBinary: TBytes; virtual;
  end;

  /// <summary>
  ///   Unix XX format
  /// </summary>
  TFormat_XX = class(TFormat_UU)
  public
    class function CharTableBinary: TBytes; override;
  end;

  /// <summary>
  ///   Escaped format
  /// </summary>
  TFormat_ESCAPE = class(TDECFormat)
  protected
    class procedure DoEncode(const Source; var Dest: TBytes; Size: Integer); override;
    class procedure DoDecode(const Source; var Dest: TBytes; Size: Integer); override;
    class function  DoIsValid(const Data; Size: Integer): Boolean; override;
  public
    class function CharTableBinary: TBytes; virtual;
  end;

  /// <summary>
  ///   Conversion from/to 16 bit big endian
  /// </summary>
  TFormat_BigEndian16 = class(TDECFormat)
  private
    class procedure DoSawp(const Source; var Dest: TBytes; Size: Integer); inline;
  protected
    class procedure DoEncode(const Source; var Dest: TBytes; Size: Integer); override;
    class procedure DoDecode(const Source; var Dest: TBytes; Size: Integer); override;
    class function  DoIsValid(const Data; Size: Integer): Boolean; override;
  public
  end;

  /// <summary>
  ///   Conversion from/to 32 bit big endian
  /// </summary>
  TFormat_BigEndian32 = class(TDECFormat)
  private
    class procedure DoSawp(const Source; var Dest: TBytes; Size: Integer); inline;
  protected
    class procedure DoEncode(const Source; var Dest: TBytes; Size: Integer); override;
    class procedure DoDecode(const Source; var Dest: TBytes; Size: Integer); override;
    class function  DoIsValid(const Data; Size: Integer): Boolean; override;
  public
  end;

  /// <summary>
  ///   Conversion from/to 64 bit big endian
  /// </summary>
  TFormat_BigEndian64 = class(TDECFormat)
  private
    class procedure DoSawp(const Source; var Dest: TBytes; Size: Integer); inline;
  protected
    class procedure DoEncode(const Source; var Dest: TBytes; Size: Integer); override;
    class procedure DoDecode(const Source; var Dest: TBytes; Size: Integer); override;
    class function  DoIsValid(const Data; Size: Integer): Boolean; override;
  public
  end;

implementation

uses
  DECCRC; // needed by TFormat_Radix64

resourcestring
  sInvalidStringFormat  = 'Input is not an valid %s format';

class function TFormat_HEX.CharTableBinary: TBytes;
begin
  SetLength(result, 48);
  // special and skipped chars
  // '0123456789ABCDEFX$ abcdefhHx()[]{},;:-_/\*+"'''+CHR(9)+CHR(10)+CHR(13);

  {$IF CompilerVersion >= 28.0}
  result := [$30, $31, $32, $33, $34, $35, $36, $37, $38, $39, $41, $42, $43,
             $44, $45, $46, $58, $24, $20, $61, $62, $63, $64, $65, $66, $68,
             $48, $78, $28, $29, $5B, $5D, $7B, $7D, $2C, $3B, $3A, $2D, $5F,
             $2F, $5C, $2A, $2B, $22, $27, $09, $0A, $0D];
  {$ELSE}
  // Remove this initialisation variant as soon as XE7+ is the new minimum
  // supported Delphi version
  result[ 0]:=$30;
  result[ 1]:=$31;
  result[ 2]:=$32;
  result[ 3]:=$33;
  result[ 4]:=$34;
  result[ 5]:=$35;
  result[ 6]:=$36;
  result[ 7]:=$37;
  result[ 8]:=$38;
  result[ 9]:=$39;
  result[10]:=$41;
  result[11]:=$42;
  result[12]:=$43;
  result[13]:=$44;
  result[14]:=$45;
  result[15]:=$46;
  result[16]:=$58;
  result[17]:=$24;
  result[18]:=$20;
  result[19]:=$61;
  result[20]:=$62;
  result[21]:=$63;
  result[22]:=$64;
  result[23]:=$65;
  result[24]:=$66;
  result[25]:=$68;
  result[26]:=$48;
  result[27]:=$78;
  result[28]:=$28;
  result[29]:=$29;
  result[30]:=$5B;
  result[31]:=$5D;
  result[32]:=$7B;
  result[33]:=$7D;
  result[34]:=$2C;
  result[35]:=$3B;
  result[36]:=$3A;
  result[37]:=$2D;
  result[38]:=$5F;
  result[39]:=$2F;
  result[40]:=$5C;
  result[41]:=$2A;
  result[42]:=$2B;
  result[43]:=$22;
  result[44]:=$27;
  result[45]:=$09;
  result[46]:=$0A;
  result[47]:=$0D;
  {$IFEND}
end;

class procedure TFormat_HEX.DoEncode(const Source; var Dest: TBytes; Size: Integer);
var
  S     : PByte;
  Table : TBytes;
  i     : Integer;
begin
  if Size <= 0 then Exit;
  SetLength(Dest, Size * 2);

  Table := CharTableBinary;

  S     := PByte(@Source);
  i     := 0;

  while Size > 0 do
  begin
    Dest[i]     := Table[S^ shr  4];
    Dest[i + 1] := Table[S^ and $F];

    Inc(S);
    Dec(Size);
    Inc(i, 2);
  end;
end;

class procedure TFormat_HEX.DoDecode(const Source; var Dest: TBytes; Size: Integer);
var
  S: PByte;
  D: PByte;
  T: TBytes;
  I,P: Integer;

  HasIdent: Boolean;
begin
  SetLength(Dest, 0);
  if Size <= 0 then Exit;
  SetLength(Dest, Size div 2); //  + 1);

  T := CharTableBinary;

  D := PByte(Dest);
  S := PByte(@Source);
  I := 0;
  HasIdent := False;
  while Size > 0 do
  begin
    P := TableFindBinary(S^, T, 18);
    if P < 0 then P := TableFindBinary(UpCaseBinary(S^), T, 16);
    if P < 0 then
      raise EDECException.CreateResFmt(@sInvalidStringFormat, [self.GetShortClassName]);
    Inc(S);
    if P >= 0 then
      if P > 16 then
      begin
        if not HasIdent then
        begin
          HasIdent := True;
          I := 0;
          D := PByte(Dest);
        end;
      end
      else
      begin
        if Odd(I) then
        begin
          D^ := D^ or P;
          Inc(D);
        end
        else
          D^ := P shl 4;
        Inc(I);
      end;
    Dec(Size);
  end;
end;

class function TFormat_HEX.DoIsValid(const Data; Size: Integer): Boolean;
var
  T: TBytes;
  S: PByte;
begin
  if not odd(Size) then
  begin
    Result := True;
    T := CharTableBinary;
    S := @Data;
    while Result and (Size > 0) do
    begin
      if TableFindBinary(S^, T, length(T)) >= 0 then
      begin
        Inc(S);
        Dec(Size);
      end
      else
        Result := False;
    end;
  end
  else
    result := false;
end;

class function TFormat_HEXL.CharTableBinary: TBytes;
begin
  SetLength(result, 48);
  // special and skipped chars
  // '0123456789abcdefX$ ABCDEFhHx()[]{},;:-_/\*+"'''+CHR(9)+CHR(10)+CHR(13);

  {$IF CompilerVersion >= 28.0}
  result := [$30, $31, $32, $33, $34, $35, $36, $37, $38, $39, $61, $62, $63,
             $64, $65, $66, $68, $58, $24, $20, $41, $42, $43, $44, $45, $46,
             $48, $78, $28, $29, $5B, $5D, $7B, $7D, $2C, $3B, $3A, $2D, $5F,
             $2F, $5C, $2A, $2B, $22, $27, $09, $0A, $0D];
  {$ELSE}
  // Remove this initialisation variant as soon as XE7+ is the new minimum
  // supported Delphi version
  result[ 0]:=$30;
  result[ 1]:=$31;
  result[ 2]:=$32;
  result[ 3]:=$33;
  result[ 4]:=$34;
  result[ 5]:=$35;
  result[ 6]:=$36;
  result[ 7]:=$37;
  result[ 8]:=$38;
  result[ 9]:=$39;
  result[10]:=$61;
  result[11]:=$62;
  result[12]:=$63;
  result[13]:=$64;
  result[14]:=$65;
  result[15]:=$66;
  result[16]:=$68;
  result[17]:=$58;
  result[18]:=$24;
  result[19]:=$20;
  result[20]:=$41;
  result[21]:=$42;
  result[22]:=$43;
  result[23]:=$44;
  result[24]:=$45;
  result[25]:=$46;
  result[26]:=$48;
  result[27]:=$78;
  result[28]:=$28;
  result[29]:=$29;
  result[30]:=$5B;
  result[31]:=$5D;
  result[32]:=$7B;
  result[33]:=$7D;
  result[34]:=$2C;
  result[35]:=$3B;
  result[36]:=$3A;
  result[37]:=$2D;
  result[38]:=$5F;
  result[39]:=$2F;
  result[40]:=$5C;
  result[41]:=$2A;
  result[42]:=$2B;
  result[43]:=$22;
  result[44]:=$27;
  result[45]:=$09;
  result[46]:=$0A;
  result[47]:=$0D;
  {$IFEND}
end;

class function TFormat_DECMIME32.CharTableBinary: TBytes;
begin
  // special and skipped chars
  // 'abcdefghijklnpqrstuwxyz123456789 =$()[]{},;:-_\*"'''+CHR(9)+CHR(10)+CHR(13);
  SetLength(result, 53);
  {$IF CompilerVersion >= 28.0}
  result := [$61, $62, $63, $64, $65, $66, $67, $68, $69, $6A, $6B, $6C, $6E, $70,
             $71, $72, $73, $74, $75, $77, $78, $79, $7A, $31, $32, $33, $34, $35,
             $36, $37, $38, $39, $20, $3D, $24, $28, $29, $5B, $5D, $7B, $7D, $2C,
             $3B, $3A, $2D, $5F, $5C, $2A, $22, $27, $09, $0A, $0D];
  {$ELSE}
  // Remove this initialisation variant as soon as XE7+ is the new minimum
  // supported Delphi version
  result[ 0]:=$61;
  result[ 1]:=$62;
  result[ 2]:=$63;
  result[ 3]:=$64;
  result[ 4]:=$65;
  result[ 5]:=$66;
  result[ 6]:=$67;
  result[ 7]:=$68;
  result[ 8]:=$69;
  result[ 9]:=$6A;
  result[10]:=$6B;
  result[11]:=$6C;
  result[12]:=$6E;
  result[13]:=$70;

  result[14]:=$71;
  result[15]:=$72;
  result[16]:=$73;
  result[17]:=$74;
  result[18]:=$75;
  result[19]:=$77;
  result[20]:=$78;
  result[21]:=$79;
  result[22]:=$7A;
  result[23]:=$31;
  result[24]:=$32;
  result[25]:=$33;
  result[26]:=$34;
  result[27]:=$35;

  result[28]:=$36;
  result[29]:=$37;
  result[30]:=$38;
  result[31]:=$39;
  result[32]:=$20;
  result[33]:=$3D;
  result[34]:=$24;
  result[35]:=$28;
  result[36]:=$29;
  result[37]:=$5B;
  result[38]:=$5D;
  result[39]:=$7B;
  result[40]:=$7D;
  result[41]:=$2C;

  result[42]:=$3B;
  result[43]:=$3A;
  result[44]:=$2D;
  result[45]:=$5F;
  result[46]:=$5C;
  result[47]:=$2A;
  result[48]:=$22;
  result[49]:=$27;
  result[50]:=$09;
  result[51]:=$0A;
  result[52]:=$0D;
  {$IFEND}
end;

class procedure TFormat_DECMIME32.DoEncode(const Source; var Dest: TBytes; Size: Integer);
var
  T   : TBytes;
  Src : TBytes;
  S, D: PByte;
  i, n: Integer;
begin
  SetLength(Dest, 0);
  if Size <= 0 then
    Exit;

  // The passed in source parameter has to be converted into an array with
  // added additional 0 value. This is because in the original form a string was
  // being passed in as source parameter, which automatically contained a #00 at
  // the end and depending on length of the data passed in, the @S[i shr 3] index
  // calculation can result in an index which represents the last byte of the
  // source parameter. That one is accessed as PWord then which results in
  // reading the first byte behind that source parameter as well! This led to
  // wrong data errors in the unit tests.
  SetLength(Src, Size + 1);
  Move(Source, Src[0], Size);
  Src[length(Src)-1] := 0;

  Size := Size * 8;
  SetLength(Dest, Size div 5 + 5);

  D := @Dest[0];
  T := CharTableBinary;
  S := @Src[0];

  i := 0; n := 0;
  while i < Size do
  begin
    D^ := T[PWord(@S[i shr 3])^ shr (i and $7) and $1F];

    Inc(D);
    Inc(i, 5);

    Inc(n);
  end;

  SetLength(Dest, n);
  SetLength(Src, 0);
end;

class function TFormat_DECMIME32.DoIsValid(const Data; Size: Integer): Boolean;
var
  T: TBytes;
  S: PByte;
begin
  Result := True;
  T := CharTableBinary;
  S := @Data;
  while Result and (Size > 0) do
  begin
    if TableFindBinary(S^, T, length(T)) >= 0 then
    begin
      Inc(S);
      Dec(Size);
    end
    else
      Result := False;
  end;
end;

class procedure TFormat_DECMIME32.DoDecode(const Source; var Dest: TBytes; Size: Integer);
var
  T: TBytes;
  S: PByte;
  D: PByte;
  i, V: Integer;
begin
  SetLength(Dest, 0);
  if Size <= 0 then
    Exit;

  Size := Size * 5;
  SetLength(Dest, Size div 8);

  T := CharTableBinary;
  S := @Source;
  D := @Dest[0];

  FillChar(D^, Length(Dest), 0);
  i := 0;

  while i < Size do
  begin
    V := TableFindBinary(S^, T, 32);
    if V < 0 then
      V := TableFindBinary(UpCaseBinary(S^), T, 32);
    if V >= 0 then
    begin
      PWord(@D[i shr 3])^ := PWord(@D[i shr 3])^ or (V shl (i and $7));
      Inc(i, 5);
    end
    else
      Dec(Size, 5);
    Inc(S);
  end;

  SetLength(Dest, Size div 8);
end;

class function TFormat_Base64.CharTableBinary: TBytes;
begin
  //  'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=' +
  //  ' $()[]{},;:-_\*"'''+CHR(9)+CHR(10)+CHR(13); // special and skipped chars
  SetLength(result, 85);

  {$IF CompilerVersion >= 28.0}
  result := [$41, $42, $43, $44, $45, $46, $47, $48, $49, $4A, $4B, $4C, $4D,
             $4E, $4F, $50, $51, $52, $53, $54, $55, $56, $57, $58, $59, $5A,
             $61, $62, $63, $64, $65, $66, $67, $68, $69, $6A, $6B, $6C, $6D,
             $6E, $6F, $70, $71, $72, $73, $74, $75, $76, $77, $78, $79, $7A,
             $30, $31, $32, $33, $34, $35, $36, $37, $38, $39, $2B, $2F, $3D,
             $20, $24, $28, $29, $5B, $5D, $7B, $7D, $2C, $3B, $3A, $2D, $5F,
             $5C, $2A, $22, $27, $09, $0A, $0D];
  {$ELSE}
  // Remove this initialisation variant as soon as XE7+ is the new minimum
  // supported Delphi version
  result[0 ]:=$41;
  result[1 ]:=$42;
  result[2 ]:=$43;
  result[3 ]:=$44;
  result[4 ]:=$45;
  result[5 ]:=$46;
  result[6 ]:=$47;
  result[7 ]:=$48;
  result[8 ]:=$49;
  result[9 ]:=$4A;
  result[10]:=$4B;
  result[11]:=$4C;
  result[12]:=$4D;

  result[13]:=$4E;
  result[14]:=$4F;
  result[15]:=$50;
  result[16]:=$51;
  result[17]:=$52;
  result[18]:=$53;
  result[19]:=$54;
  result[20]:=$55;
  result[21]:=$56;
  result[22]:=$57;
  result[23]:=$58;
  result[24]:=$59;
  result[25]:=$5A;

  result[26]:=$61;
  result[27]:=$62;
  result[28]:=$63;
  result[29]:=$64;
  result[30]:=$65;
  result[31]:=$66;
  result[32]:=$67;
  result[33]:=$68;
  result[34]:=$69;
  result[35]:=$6A;
  result[36]:=$6B;
  result[37]:=$6C;
  result[38]:=$6D;

  result[39]:=$6E;
  result[40]:=$6F;
  result[41]:=$70;
  result[42]:=$71;
  result[43]:=$72;
  result[44]:=$73;
  result[45]:=$74;
  result[46]:=$75;
  result[47]:=$76;
  result[48]:=$77;
  result[49]:=$78;
  result[50]:=$79;
  result[51]:=$7A;

  result[52]:=$30;
  result[53]:=$31;
  result[54]:=$32;
  result[55]:=$33;
  result[56]:=$34;
  result[57]:=$35;
  result[58]:=$36;
  result[59]:=$37;
  result[60]:=$38;
  result[61]:=$39;
  result[62]:=$2B;
  result[63]:=$2F;
  result[64]:=$3D;

  result[65]:=$20;
  result[66]:=$24;
  result[67]:=$28;
  result[68]:=$29;
  result[69]:=$5B;
  result[70]:=$5D;
  result[71]:=$7B;
  result[72]:=$7D;
  result[73]:=$2C;
  result[74]:=$3B;
  result[75]:=$3A;
  result[76]:=$2D;
  result[77]:=$5F;

  result[78]:=$5C;
  result[79]:=$2A;
  result[80]:=$22;
  result[81]:=$27;
  result[82]:=$09;
  result[83]:=$0A;
  result[84]:=$0D;
  {$IFEND}
end;

class procedure TFormat_Base64.DoEncode(const Source; var Dest: TBytes; Size: Integer);
var
  T: TBytes;
  S: PByte;
  D: PByte;
  B: UInt32;
  i: Integer;
  n: Integer;
begin
  SetLength(Dest, 0);
  if Size <= 0 then
    Exit;

  SetLength(Dest, Size * 4 div 3 + 4);

  T := CharTableBinary;
  S := @Source;
  D := @Dest[0];

  n := 0;
  while Size >= 3 do
  begin
    Dec(Size, 3);
    B := Byte(S[0]) shl 16 or Byte(S[1]) shl 8 or Byte(S[2]);
    D[0] := T[B shr 18 and $3F];
    D[1] := T[B shr 12 and $3F];
    D[2] := T[B shr  6 and $3F];
    D[3] := T[B        and $3F];
    Inc(D, 4);
    S := @S[3];
    Inc(n, 4);
  end;

  while Size > 0 do
  begin
    B := 0;
    for i := 0 to 2 do
    begin
      B := B shl 8;
      if Size > 0 then
      begin
        B := B or Byte(S[0]);
        S := @S[1];
      end;
      Dec(Size);
    end;
    for i := 3 downto 0 do
    begin
      if Size < 0 then
      begin
        D[i] := T[64];
        Inc(Size);
      end
      else
        D[i] := T[B and $3F];
      B := B shr 6;
    end;
    Inc(D, 4);
    Inc(n, 4);
  end;

  // original calculation was substract dest ptr d - start of dest
  SetLength(Dest, n);
end;

class procedure TFormat_Base64.DoDecode(const Source; var Dest: TBytes; Size: Integer);
var
  T: TBytes;
  S: PByte;
  D, L: PByte; // 1) to make pointer arithmetic work 2) P/TByteArray is limited to 32768 bytes
  B: UInt32;
  i, j, n: Integer;
begin
  SetLength(Dest, 0);
  if Size <= 0 then
    Exit;

  SetLength(Dest, Size);

  T := CharTableBinary;
  S := @Source;
  D := @Dest[0];

  Move(Source, Dest[0], Size);

  L := S + Size;
  j := 0;
  n := 0;
  while S < L do
  begin
    B := 0;
    j := 4;
    while (j > 0) and (S < L) do
    begin
      i := TableFindBinary(S^, T, 65);
      Inc(S);
      if i >= 0 then
      begin
        if i < 64 then
        begin
          B := B shl 6 or Byte(i);
          Dec(j);
        end
        else
          L := S;
      end;
    end;
    if j > 0 then
    begin
      if j >= 4 then
      begin
        j := 0;
        Break;
      end
      else
        B := B shl (6 * j);
    end;
    i := 2;
    while i >= 0 do
    begin
      D[i] := Byte(B);
      B := B shr 8;
      Dec(i);
    end;
    Inc(D, 3);
    Inc(n, 3);
  end;

  SetLength(Dest, n-j);
end;

class function TFormat_Base64.DoIsValid(const Data; Size: Integer): Boolean;
var
  T: TBytes;
  S: PByte;
begin
  Result := True;
  T := CharTableBinary;
  S := @Data;
  while Result and (Size > 0) do
  begin
    // A-Z, a-z, 0-9, + and / and CR/LF
    if S^ in [$41..$5A, $61..$7A, $2B, $2F..$39, $3D, $0D, $0A] then
    begin
      Inc(S);
      Dec(Size);
    end
    else
      Result := False;
  end;
end;

class function TFormat_Radix64.DoExtractCRC(const Data; var Size: Integer): UInt32;
var
  L: PByte; // 1) to make pointer arithmetic work 2) P/TByteArray is limited to 32768 bytes
  C: Byte;
  R: TBytes;
begin
  Result := $FFFFFFFF;
  C := CharTableBinary[64]; // get padding char, per default '='
  L := PByte(@Data) + Size;
  while L <> @Data do
  begin
    if L^ = C then
      Break
    else
      Dec(L); // scan reverse for padding char
  end;
  if L - PByte(@Data) >= Size - 5 then // remaining chars must be > 4, e.g. '=XQRT'
  try
    Inc(L);
    inherited DoDecode(L^, R, Size - (L - PByte(@Data)));
    if Length(R) >= 3 then
    begin
      Result := 0;
      Move(R[0], Result, 3);
      Size := L - PByte(@Data);
    end;
  except
  end;
end;

class function TFormat_Radix64.DoIsValid(const Data; Size: Integer): Boolean;
var
  crc24 : UInt32;
  Dest  : TBytes;
begin
  // Radix64 is like Base64 but with additional CRC24 checksum
  result := TFormat_Base64.IsValid(Data, Size);

  // Check contained checksum as well
  if result then
  begin
    crc24 := DoExtractCRC(Data, Size);
    // we need to decode, because it removes the CR/LF linebreaks which would
    // invalidate the checksum
    inherited DoDecode(Data, Dest, Size);

    if crc24 <> $FFFFFFFF then
    begin
      // recalc CRC and compare
      SwapBytes(crc24, 3);
      result := crc24 = CRCCalc(CRC_24, Dest[0], Length(Dest));
    end
    else
      result := false;
  end;
end;

class function TFormat_Radix64.GetCharsPerLine: UInt32;
begin
  result := FCharsPerLine;
end;

class procedure TFormat_Radix64.InsertCRLF(const Source: TBytes; var Dest: TBytes; LineLength: Integer);
var
  S, D: PByte;
  i: Integer;
begin
  i := Length(Source);
  if (LineLength <= 0) or (i <= LineLength) then
  begin
    SetLength(Dest, i);
    Move(Source[0], Dest[0], i);
    Exit;
  end;

  SetLength(Dest, i + i * 2 div LineLength + 2);

  S := @Source[0];
  D := @Dest[0];

  repeat
    Move(S^, D^, LineLength);
    Inc(S, LineLength);
    Inc(D, LineLength);
    D^ := Ord(#13);
    Inc(D);
    D^ := Ord(#10);
    Inc(D);
    Dec(i, LineLength);
  until i < LineLength;

  Move(S^, D^, i);
  Inc(D, i);

  SetLength(Dest, PByte(D) - PByte(Dest));
end;

class procedure TFormat_Radix64.SetCharsPerLine(const Value: UInt32);
begin
  Assert(Value > 0, 'Invalid number of chars per line: ' + IntToStr(Value));

  if (Value > 0) then
    FCharsPerLine := Value
  else
    raise EArgumentOutOfRangeException.Create('Invalid number of chars per line: ' +
                                              IntToStr(Value));
end;

class procedure TFormat_Radix64.DoEncode(const Source; var Dest: TBytes; Size: Integer);
var
  TempData : TBytes;
  CRC      : UInt32;
  CRCData  : TBytes;
  Position : Integer;
begin
  SetLength(Dest, 0);
  if Size <= 0 then
    Exit;

  // use Base64
  inherited DoEncode(Source, TempData, Size);

  // split lines
  InsertCRLF(TempData, Dest, FCharsPerLine);
  SetLength(TempData, 0);

  CRC := CRCCalc(CRC_24, Source, Size); // calculate 24-bit Checksum
  SwapBytes(CRC, 3); // PGP use Big Endian

  // check and insert LF if needed
  Position := Length(Dest) - 1; // last char
  if Dest[Position] <> $0A then
  begin
    // insert CR needed, CRC must be in the next line
    Position := Length(Dest);
    SetLength(Dest, Position + 2);
    Dest[Position]   := $0D; // append CR
    Dest[Position+1] := $0A; // append LF
  end;

  // encode CRC with Base64 too
  inherited DoEncode(CRC, CRCData, 3);

  // if CRC is too long insert CRLF. -1 to compensate the later added = char
  InsertCRLF(CRCData, TempData, FCharsPerLine - 1);
  CRCData := TempData;

  // append encoded CRC
  Position := Length(Dest);
  SetLength(Dest, Position + 1 + Length(CRCData));
  Dest[Position] := Ord('=');
  Move(CRCData[0], Dest[Position + 1], Length(CRCData));
end;

class procedure TFormat_Radix64.DoDecode(const Source; var Dest: TBytes; Size: Integer);
var
  CRC: UInt32;
begin
  SetLength(Dest, 0);
  if Size <= 0 then
    Exit;

  CRC := DoExtractCRC(Source, Size);
  inherited DoDecode(Source, Dest, Size);

  if CRC <> $FFFFFFFF then // check CRC if found
  begin
    SwapBytes(CRC, 3);
    if CRC <> CRCCalc(CRC_24, Dest[0], Length(Dest)) then
      raise EDECFormatException.CreateResFmt(@sInvalidStringFormat, [self.GetShortClassName]);
  end;
end;

class function TFormat_UU.CharTableBinary: TBytes;
begin
  // '`!"#$%&''()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_' +
  // ' '+CHR(9)+CHR(10)+CHR(13);

  SetLength(result, 68);
  {$IF CompilerVersion >= 28.0}
  result := [$60, $21, $22, $23, $24, $25, $26, $27, $28, $29, $2A, $2B, $2C,
             $2D, $2E, $2F, $30, $31, $32, $33, $34, $35, $36, $37, $38, $39,
             $3A, $3B, $3C, $3D, $3E, $3F, $40, $41, $42, $43, $44, $45, $46,
             $47, $48, $49, $4A, $4B, $4C, $4D, $4E, $4F, $50, $51, $52, $53,
             $54, $55, $56, $57, $58, $59, $5A, $5B, $5C, $5D, $5E, $5F, $20,
             $09, $0A, $0D];
  {$ELSE}
  // Remove this initialisation variant as soon as XE7+ is the new minimum
  // supported Delphi version
  result[ 0]:=$60;
  result[ 1]:=$21;
  result[ 2]:=$22;
  result[ 3]:=$23;
  result[ 4]:=$24;
  result[ 5]:=$25;
  result[ 6]:=$26;
  result[ 7]:=$27;
  result[ 8]:=$28;
  result[ 9]:=$29;
  result[10]:=$2A;
  result[11]:=$2B;
  result[12]:=$2C;

  result[13]:=$2D;
  result[14]:=$2E;
  result[15]:=$2F;
  result[16]:=$30;
  result[17]:=$31;
  result[18]:=$32;
  result[19]:=$33;
  result[20]:=$34;
  result[21]:=$35;
  result[22]:=$36;
  result[23]:=$37;
  result[24]:=$38;
  result[25]:=$39;

  result[26]:=$3A;
  result[27]:=$3B;
  result[28]:=$3C;
  result[29]:=$3D;
  result[30]:=$3E;
  result[31]:=$3F;
  result[32]:=$40;
  result[33]:=$41;
  result[34]:=$42;
  result[35]:=$43;
  result[36]:=$44;
  result[37]:=$45;
  result[38]:=$46;

  result[39]:=$47;
  result[40]:=$48;
  result[41]:=$49;
  result[42]:=$4A;
  result[43]:=$4B;
  result[44]:=$4C;
  result[45]:=$4D;
  result[46]:=$4E;
  result[47]:=$4F;
  result[48]:=$50;
  result[49]:=$51;
  result[50]:=$52;
  result[51]:=$53;

  result[52]:=$54;
  result[53]:=$55;
  result[54]:=$56;
  result[55]:=$57;
  result[56]:=$58;
  result[57]:=$59;
  result[58]:=$5A;
  result[59]:=$5B;
  result[60]:=$5C;
  result[61]:=$5D;
  result[62]:=$5E;
  result[63]:=$5F;
  result[64]:=$20;

  result[65]:=$09;
  result[66]:=$0A;
  result[67]:=$0D;
  {$IFEND}
end;

class procedure TFormat_UU.DoEncode(const Source; var Dest: TBytes; Size: Integer);
var
  T: TBytes;
  S: PByte;
  D: PByte; // 1) to make pointer arithmetic work 2) P/TByteArray is limited to 32768 bytes
  L, i: Integer;
  B: Cardinal;
begin
  SetLength(Dest, 0);
  if Size <= 0 then
    Exit;

  SetLength(Dest, Size * 4 div 3 + Size div 45 + 10);

  T := CharTableBinary;
  S := @Source;
  D := @Dest[0];

  while Size > 0 do
  begin
    L := Size;
    if L > 45 then
      L := 45;
    Dec(Size, L);
    D^ := T[L];
    while L > 0 do
    begin
      B := 0;
      for i := 0 to 2 do
      begin
        B := B shl 8;
        if L > 0 then
        begin
          B := B or S^;
          Inc(S);
        end;
        Dec(L);
      end;
      for i := 4 downto 1 do
      begin
        D[i] := T[B and $3F];
        B := B shr 6;
      end;
      Inc(D, 4);
    end;
    Inc(D);
  end;

  SetLength(Dest, PByte(D) - PByte(Dest));
end;

class procedure TFormat_UU.DoDecode(const Source; var Dest: TBytes; Size: Integer);
var
  T: TBytes;
  S: PByte;
  D, L: PByte; // 1) to make pointer arithmetic work 2) P/TByteArray is limited to 32768 bytes
  i, E: Integer;
  B: UInt32;
begin
  SetLength(Dest, 0);
  if Size <= 0 then
    Exit;

  SetLength(Dest, Size);

  T := CharTableBinary;
  S := @Source;
  D := @Dest[0];

  L := PByte(S) + Size;

  repeat
    Size := TableFindBinary(S^, T, 64);
    if (Size < 0) or (Size > 45) then
      raise EDECException.CreateResFmt(@sInvalidStringFormat, [self.GetShortClassName]);
    Inc(S);
    while Size > 0 do
    begin
      B := 0;
      i := 4;
      while (i > 0) and (S <= L) do
      begin
        E := TableFindBinary(S^, T, 64);
        if E >= 0 then
        begin
          B := B shl 6 or Byte(E);
          Dec(i);
        end;
        Inc(S);
      end;
      i := 2;
      repeat
        D[i] := Byte(B);
        B    := B shr 8;
        Dec(i);
      until i < 0;
      if Size > 3 then
        Inc(D, 3)
      else
        Inc(D, Size);
      Dec(Size, 3);
    end;
  until S >= L;

  SetLength(Dest, PByte(D) - PByte(Dest));
end;

class function TFormat_UU.DoIsValid(const Data; Size: Integer): Boolean;
var
  T: TBytes;
  S: PByte;
  Len, P, i: Integer;
begin
  Result := False;
  T := CharTableBinary;
  Len := Length(T);
  S := @Data;
  P := 0;

  while Size > 0 do
  begin
    i := TableFindBinary(S^, T, Len);
    if i >= 0 then
    begin
      Dec(Size);
      Inc(S);
      if P = 0 then
      begin
        if i > 45 then
          Exit;
        P := (i * 4 + 2) div 3;
      end
      else
        if i < 64 then
          Dec(P);
    end
    else
      Exit;
  end;

  if P <> 0 then
    Exit;

  Result := True;
end;

class function TFormat_XX.CharTableBinary: TBytes;
begin
  // '+-0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz' +
  // ' "()[]'''+CHR(9)+CHR(10)+CHR(13);
  SetLength(result, 74);
  {$IF CompilerVersion >= 28.0}
  result := [$2B, $2D, $30, $31, $32, $33, $34, $35, $36, $37, $38, $39, $41,
             $42, $43, $44, $45, $46, $47, $48, $49, $4A, $4B, $4C, $4D, $4E,
             $4F, $50, $51, $52, $53, $54, $55, $56, $57, $58, $59, $5A, $61,
             $62, $63, $64, $65, $66, $67, $68, $69, $6A, $6B, $6C, $6D, $6E,
             $6F, $70, $71, $72, $73, $74, $75, $76, $77, $78, $79, $7A, $20,
             $22, $28, $29, $5B, $5D, $27, $09, $0A, $0D];
  {$ELSE}
  // Remove this initialisation variant as soon as XE7+ is the new minimum
  // supported Delphi version
  result[ 0]:=$2B;
  result[ 1]:=$2D;
  result[ 2]:=$30;
  result[ 3]:=$31;
  result[ 4]:=$32;
  result[ 5]:=$33;
  result[ 6]:=$34;
  result[ 7]:=$35;
  result[ 8]:=$36;
  result[ 9]:=$37;
  result[10]:=$38;
  result[11]:=$39;
  result[12]:=$41;

  result[13]:=$42;
  result[14]:=$43;
  result[15]:=$44;
  result[16]:=$45;
  result[17]:=$46;
  result[18]:=$47;
  result[19]:=$48;
  result[20]:=$49;
  result[21]:=$4A;
  result[22]:=$4B;
  result[23]:=$4C;
  result[24]:=$4D;
  result[25]:=$4E;

  result[26]:=$4F;
  result[27]:=$50;
  result[28]:=$51;
  result[29]:=$52;
  result[30]:=$53;
  result[31]:=$54;
  result[32]:=$55;
  result[33]:=$56;
  result[34]:=$57;
  result[35]:=$58;
  result[36]:=$59;
  result[37]:=$5A;
  result[38]:=$61;

  result[39]:=$62;
  result[40]:=$63;
  result[41]:=$64;
  result[42]:=$65;
  result[43]:=$66;
  result[44]:=$67;
  result[45]:=$68;
  result[46]:=$69;
  result[47]:=$6A;
  result[48]:=$6B;
  result[49]:=$6C;
  result[50]:=$6D;
  result[51]:=$6E;

  result[52]:=$6F;
  result[53]:=$70;
  result[54]:=$71;
  result[55]:=$72;
  result[56]:=$73;
  result[57]:=$74;
  result[58]:=$75;
  result[59]:=$76;
  result[60]:=$77;
  result[61]:=$78;
  result[62]:=$79;
  result[63]:=$7A;
  result[64]:=$20;

  result[65]:=$22;
  result[66]:=$28;
  result[67]:=$29;
  result[68]:=$5B;
  result[69]:=$5D;
  result[70]:=$27;
  result[71]:=$09;
  result[72]:=$0A;
  result[73]:=$0D;
  {$IFEND}
end;

var
  // Initlialized in initialization section, cannot be const because of the
  // TBytes requirement
  ESCAPE_CodesL: TBytes; //array[0..6] of Byte = ($61, $62, $74, $6E, $76, $66, $72);
  ESCAPE_CodesU: TBytes; //array[0..6] of Byte = ($41, $42, $54, $4E, $56, $46, $52);

class function TFormat_ESCAPE.CharTableBinary: TBytes;
begin
  Result := TFormat_HEX.CharTableBinary;
end;

class procedure TFormat_ESCAPE.DoEncode(const Source; var Dest: TBytes; Size: Integer);
var
  T: TBytes;
  S: PByte;
  D: PByte; // 1) to make pointer arithmetic work 2) P/TByteArray is limited to 32768 bytes
  i: Integer;
begin
  SetLength(Dest, 0);
  if Size <= 0 then
    Exit;

  SetLength(Dest, Size + 8);

  T := CharTableBinary;
  S := @Source;
  D := @Dest[0];

  i := Size;

  while Size > 0 do
  begin
    if i <= 0 then
    begin
      i := D - PByte(Dest);
      SetLength(Dest, i + Size + 8);
      D := PByte(Dest) + i;
      i := Size;
    end;
    if (S^ < 32) or (S^ > $7F) then
    begin
      if (S^ >= 7) and (S^ <= 13) then
      begin
        D^ := $5C; // \ char
        Inc(D);
        D^ := ESCAPE_CodesL[S^ - 7];
        Inc(D);
        Dec(i, 2);
      end
      else
      begin
        D^ := $5C; // \ char
        Inc(D);
        D^ := $78; // x
        Inc(D);
        D^ := T[S^ shr 4];
        Inc(D);
        D^ := T[S^ and $F];
        Inc(D);
        Dec(i, 4);
      end
    end
    else
    begin
      // S^is \ char?
      if S^ = $5C then
      begin
        D^ := $5C; // \ char
        Inc(D);
        D^ := $5C; // \ char
        Inc(D);
        Dec(i, 2);
      end
      else
      // S^ is " char?
      if S^ = $22 then
      begin
        D^ := $5C; // \ char
        Inc(D);
        D^ := $22; // " char
        Inc(D);
        Dec(i, 2);
      end
      else
      begin
        D^ := S^;
        Inc(D);
        Dec(i);
      end;
    end;
    Dec(Size);
    Inc(S);
  end;

  SetLength(Dest, PByte(D) - PByte(Dest));
end;

class function TFormat_ESCAPE.DoIsValid(const Data; Size: Integer): Boolean;
var
  T: TBytes;
  S: PByte;
begin
  Result := False;
  T := CharTableBinary;
  S := @Data;

  while Size > 0 do
  begin
    if (S^ > $7F) or (S^ < 32) then
      Exit;

    // start of an escape sequence
    if S^ = $5C then
    begin
      Dec(Size);
      Inc(S);

      // \ at the end
      if Size <= 0 then
        Exit;

      // X for hex notation
      if UpCaseBinary(S^) = $58 then
      begin
        Inc(S);
        Dec(Size);

        // incomplete hex notation follows?
        if (Size < 2) or (TableFindBinary(UpCaseBinary(S^), T, 16) < 0) then
          Exit;

        Inc(S);
        Dec(Size);

        if (TableFindBinary(UpCaseBinary(S^), T, 16) < 0) then
          Exit;

        Inc(S);
        Dec(Size);
      end
      else
      begin
        // \ with invalid following char?
        if TableFindBinary(UpCaseBinary(S^), TBytes(ESCAPE_CodesU), 7) < 0 then
          Exit;

        Dec(Size);
        Inc(S);
      end;
    end
    else
    begin
      Dec(Size);
      Inc(S);
    end;
  end;

  Result := True;
end;

class procedure TFormat_ESCAPE.DoDecode(const Source; var Dest: TBytes; Size: Integer);
var
  T: TBytes;
  S: PByte; // 1) to make pointer arithmetic work 2) P/TByteArray is limited to 32768 bytes
  D: PByte;
  L: PByte; // 1) to make pointer arithmetic work 2) P/TByteArray is limited to 32768 bytes
  i: Integer;
begin
  if Size <= 0 then
    Exit;
  SetLength(Dest, Size);

  T := CharTableBinary;
  S := @Source;
  D := @Dest[0];

  L := S + Size;

  while S < L do
  begin
    // S^ is \ char?
    if S^ = $5C then
    begin
      Inc(S);
      if S > L then Break;
      // S^ is X char?
      if UpCaseBinary(S^) = $58 then
      begin
        if S + 2 > L then
          raise EDECFormatException.CreateResFmt(@sInvalidStringFormat, [self.GetShortClassName]);
        Inc(S);
        i := TableFindBinary(UpCaseBinary(S^), T, 16);
        if i < 0 then
          raise EDECFormatException.CreateResFmt(@sInvalidStringFormat, [self.GetShortClassName]);
        D^ := i shl 4;
        Inc(S);
        i := TableFindBinary(UpCaseBinary(S^), T, 16);
        if i < 0 then
          raise EDECFormatException.CreateResFmt(@sInvalidStringFormat, [self.GetShortClassName]);
        D^ := D^ or i;
      end
      else
      begin
        i := TableFindBinary(UpCaseBinary(S^), TBytes(ESCAPE_CodesU), 7);
        if i >= 0 then
          D^ := i + 7
        else
          D^ := S^;
      end;
    end
    else
      D^ := S^;
    Inc(D);
    Inc(S);
  end;

  SetLength(Dest, PByte(D) - PByte(Dest));
end;

{ TFormat_BigEndian16 }

class procedure TFormat_BigEndian16.DoDecode(const Source; var Dest: TBytes;
  Size: Integer);
begin
  DoSawp(Source, Dest, Size);
end;

class procedure TFormat_BigEndian16.DoEncode(const Source; var Dest: TBytes;
  Size: Integer);
begin
  DoSawp(Source, Dest, Size);
end;

class function TFormat_BigEndian16.DoIsValid(const Data;
  Size: Integer): Boolean;
begin
  // swapping bytes in 16 bit mode requires even number of bytes
  result := not Odd(Size);
end;

class procedure TFormat_BigEndian16.DoSawp(const Source; var Dest: TBytes;
  Size: Integer);
var
  i : Integer;
begin
  if (Size < 0) or Odd(Size) then
    Exit;
  SetLength(Dest, Size);

  if (Size > 0) then
  begin
    Move(Source, Dest[0], Size);

    i := 0;
    while (i < length(Dest)) do
    begin
      DECUtil.SwapBytes(Dest[i], 2);
      inc(i, 2);
    end;
  end;
end;

{ TFormat_BigEndian32 }

class procedure TFormat_BigEndian32.DoDecode(const Source; var Dest: TBytes;
  Size: Integer);
begin
  DoSawp(Source, Dest, Size);
end;

class procedure TFormat_BigEndian32.DoEncode(const Source; var Dest: TBytes;
  Size: Integer);
begin
  DoSawp(Source, Dest, Size);
end;

class function TFormat_BigEndian32.DoIsValid(const Data;
  Size: Integer): Boolean;
begin
  result := (Size mod 4) = 0;
end;

class procedure TFormat_BigEndian32.DoSawp(const Source; var Dest: TBytes;
  Size: Integer);
var
  i       : Integer;
  SwapRes : UInt32;
begin
  if (Size < 0) or ((Size mod 4) <> 0) then
    Exit;
  SetLength(Dest, Size);

  if (Size > 0) then
  begin
    Move(Source, Dest[0], Size);

    i := 0;
    while (i < length(Dest)) do
    begin
      Move(Dest[i], SwapRes, 4);
      SwapRes := DECUtil.SwapUInt32(SwapRes);
      Move(SwapRes, Dest[i], 4);
      inc(i, 4);
    end;
  end;
end;

{ TFormat_BigEndian64 }

class procedure TFormat_BigEndian64.DoDecode(const Source; var Dest: TBytes;
  Size: Integer);
begin
  DoSawp(Source, Dest, Size);
end;

class procedure TFormat_BigEndian64.DoEncode(const Source; var Dest: TBytes;
  Size: Integer);
begin
  DoSawp(Source, Dest, Size);
end;

class function TFormat_BigEndian64.DoIsValid(const Data;
  Size: Integer): Boolean;
begin
  result := (Size mod 8) = 0;
end;

class procedure TFormat_BigEndian64.DoSawp(const Source; var Dest: TBytes;
  Size: Integer);
var
  i       : Integer;
  SwapRes : Int64;
begin
  if (Size < 0) or ((Size mod 8) <> 0) then
    Exit;
  SetLength(Dest, Size);

  if (Size > 0) then
  begin
    Move(Source, Dest[0], Size);

    i := 0;
    while (i < length(Dest)) do
    begin
      Move(Dest[i], SwapRes, 8);
      SwapRes := DECUtil.SwapInt64(SwapRes);
      Move(SwapRes, Dest[i], 8);
      inc(i, 8);
    end;
  end;
end;

initialization
  SetLength(ESCAPE_CodesL, 7);
  ESCAPE_CodesL[0] := $61;
  ESCAPE_CodesL[1] := $62;
  ESCAPE_CodesL[2] := $74;
  ESCAPE_CodesL[3] := $6E;
  ESCAPE_CodesL[4] := $76;
  ESCAPE_CodesL[5] := $66;
  ESCAPE_CodesL[6] := $72;

  SetLength(ESCAPE_CodesU, 7);
  ESCAPE_CodesU[0] := $41;
  ESCAPE_CodesU[1] := $42;
  ESCAPE_CodesU[2] := $54;
  ESCAPE_CodesU[3] := $4E;
  ESCAPE_CodesU[4] := $56;
  ESCAPE_CodesU[5] := $46;
  ESCAPE_CodesU[6] := $52;

  {$IFNDEF ManualRegisterClasses}
  TFormat_HEX.RegisterClass(TDECFormat.ClassList);
  TFormat_HEXL.RegisterClass(TDECFormat.ClassList);
  TFormat_DECMIME32.RegisterClass(TDECFormat.ClassList);
  TFormat_Base64.RegisterClass(TDECFormat.ClassList);
  TFormat_Radix64.RegisterClass(TDECFormat.ClassList);
  TFormat_UU.RegisterClass(TDECFormat.ClassList);
  TFormat_XX.RegisterClass(TDECFormat.ClassList);
  TFormat_ESCAPE.RegisterClass(TDECFormat.ClassList);
  TFormat_BigEndian16.RegisterClass(TDECFormat.ClassList);
  TFormat_BigEndian32.RegisterClass(TDECFormat.ClassList);
  TFormat_BigEndian64.RegisterClass(TDECFormat.ClassList);
  {$ENDIF}

  // Init the number of chars per line as per RFC 4880 to 76 chars
  TFormat_Radix64.FCharsPerLine := 76;

finalization

end.
