{ *****************************************************************************
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
  ***************************************************************************** }

/// <summary>
/// Contains the base class for all the formatting classes
/// </summary>
unit DECFormatBase;

interface

{$INCLUDE DECOptions.inc}

uses
{$IFDEF FPC}
  SysUtils, Classes,
{$ELSE}
  System.SysUtils, System.Classes,
{$ENDIF}
  DECBaseClass, DECUtil;

type
  /// <summary>
  /// Class reference type of the TDECFormat base class. This is used for
  /// passing formatting classes as parameters or returning those. This is
  /// especially useful for the formatting classes, as they only contain
  /// class functions.
  /// </summary>
  TDECFormatClass = class of TDECFormat;

  /// <summary>
  /// copy input to output (default format)
  /// </summary>
  TFormat_Copy = class;

  /// <summary>
  /// Basis for all formatting classes. Not to be instantiated directly.
  /// </summary>
  TDECFormat = class(TDECObject)
  protected
    /// <summary>
    /// Internal method for the actual format conversion. This method needs to
    /// be overridden in all the child classes. Converts into the format.
    /// </summary>
    /// <param name="Source">
    /// Data to be converted
    /// </param>
    /// <param name="Dest">
    /// Into this parameter the converted data will be written into.
    /// </param>
    /// <param name="Size">
    /// Number of bytes from source which will get converted.
    /// </param>
    class procedure DoEncode(const Source; var Dest: TBytes;
      Size: Integer); virtual;
    /// <summary>
    /// Internal method for the actual format conversion. This method needs to
    /// be overridden in all the child classes. Converts from the format into
    /// the format the data had before encoding it.
    /// </summary>
    /// <param name="Source">
    /// Data to be converted
    /// </param>
    /// <param name="Dest">
    /// Into this parameter the converted data will be written into.
    /// </param>
    /// <param name="Size">
    /// Number of bytes from source which will get converted.
    /// </param>
    class procedure DoDecode(const Source; var Dest: TBytes;
      Size: Integer); virtual;
    /// <summary>
    /// Internal method for checking whether all bytes of the data to be
    /// processed are valid for this particular formatting. This method needs
    /// to be overridden in all the child classes.
    /// </summary>
    /// <param name="Data">
    /// Data to be checked
    /// </param>
    /// <param name="Size">
    /// Number of bytes from data which will get checked.
    /// </param>
    class function DoIsValid(const Data; Size: Integer): Boolean; virtual;
  public
    /// <summary>
    /// List of registered DEC classes. Key is the Identity of the class.
    /// </summary>
    class var ClassList: TDECClassList;

    /// <summary>
    /// Tries to find a class type by its name in the list of registered
    /// formatting classes
    /// </summary>
    /// <param name="Name">
    /// Name to look for in the list
    /// </param>
    /// <returns>
    /// Returns the class type if found. if it could not be found a
    /// EDECClassNotRegisteredException will be thrown
    /// </returns>
    class function ClassByName(const Name: string): TDECFormatClass;

    /// <summary>
    /// Tries to find a class type by its numeric identity DEC assigned to it.
    /// Useful for file headers, so they can easily encode numerically which
    /// cipher class was being used.
    /// </summary>
    /// <param name="Identity">
    /// Identity to look for
    /// </param>
    /// <returns>
    /// Returns the class type of the class with the specified identity value
    /// or throws an EDECClassNotRegisteredException exception if no class
    /// with the given identity has been found
    /// </returns>
    class function ClassByIdentity(Identity: Int64): TDECFormatClass;

    /// <summary>
    /// Calls the internal method which actually does the format conversion.
    /// </summary>
    /// <param name="Data">
    /// Source data to be converted into the format of this class as
    /// RawByteString. Empty strings are allowed. They will simply lead to
    // empty return arrays as well.
    /// </param>
    /// <returns>
    /// Data in the format of this formatting algorithm as RawByteString
    /// </returns>
    class function Encode(const Data: RawByteString): RawByteString; overload;

    /// <summary>
    /// Calls the internal method which actually does the format conversion.
    /// </summary>
    /// <param name="Data">
    /// Source data to be converted into the format of this class as untyped
    /// parameter. Empty data is allowed. It will simply lead to empty return
    // values as well.
    /// </param>
    /// <param name="Size">
    /// Size of the data passed via data in bytes.
    /// </param>
    /// <returns>
    /// Data in the format of this formatting algorithm as RawByteString
    /// </returns>
    class function Encode(const Data; Size: Integer): RawByteString; overload;

    /// <summary>
    /// Calls the internal method which actually does the format conversion.
    /// </summary>
    /// <param name="Data">
    /// Source data to be converted into the format of this class as Byte Array.
    /// Empty arrays of size 0 are allowed. They will simply lead to empty return
    // arrays as well.
    /// </param>
    /// <returns>
    /// Data in the format of this formatting algorithm as byte array.
    /// </returns>
    class function Encode(const Data: TBytes): TBytes; overload;

    /// <summary>
    /// Calls the internal method which actually does the format conversion.
    /// </summary>
    /// <param name="Data">
    /// Source data to be converted from the format of this class as byte array
    /// into the original byte representation. Empty arrays of size 0 are allowed.
    // They will simply lead to empty return arrays as well.
    /// </param>
    /// <returns>
    /// Data in the original byte format it had before getting encoded with
    /// this formatting.
    /// </returns>
    class function Decode(const Data: TBytes): TBytes; overload;

    /// <summary>
    /// Calls the internal method which actually does the format conversion.
    /// </summary>
    /// <param name="Data">
    /// Source data to be converted from the format of this class as
    /// RawByteString into the original representation. Empty strings are allowed.
    /// They will simply lead to empty return arrays as well.
    /// </param>
    /// <returns>
    /// Data in the format of this formatting algorithm as RawByteString
    /// </returns>
    class function Decode(const Data: RawByteString): RawByteString; overload;

    /// <summary>
    /// Calls the internal method which actually does the format conversion.
    /// </summary>
    /// <param name="Data">
    /// Source data to be converted from the format of this class as untyped
    /// parameter into the original representation. Empty data is allowed.
    /// It will simply lead to empty return values as well.
    /// </param>
    /// <param name="Size">
    /// Size of the data passed via data in bytes.
    /// </param>
    /// <returns>
    /// Data in the format of this formatting algorithm as RawByteString
    /// </returns>
    class function Decode(const Data; Size: Integer): RawByteString; overload;

    /// <summary>
    /// Checks whether the data passed to this method only contains chars
    /// valid for this specific formatting.
    /// </summary>
    /// <param name="Data">
    /// Untyped parameter with the data to be checked
    /// </param>
    /// <param name="Size">
    /// Size of the data to be checked in bytes
    /// </param>
    /// <returns>
    /// true, if the input data contains only characters valid for this format
    /// </returns>
    class function IsValid(const Data; Size: Integer): Boolean; overload;

    /// <summary>
    /// Checks whether the data passed to this method only contains chars
    /// valid for this specific formatting.
    /// </summary>
    /// <param name="Data">
    /// Byte array with the data to be checked
    /// </param>
    /// <returns>
    /// true, if the input data contains only characters valid for this format
    /// </returns>
    class function IsValid(const Data: TBytes): Boolean; overload;

    /// <summary>
    /// Checks whether the data passed to this method only contains chars
    /// valid for this specific formatting.
    /// </summary>
    /// <param name="Text">
    /// RawByteString with the data to be checked
    /// </param>
    /// <returns>
    /// true, if the input data contains only characters valid for this format
    /// </returns>
    class function IsValid(const Text: RawByteString): Boolean; overload;

    /// <summary>
    /// Converts the ordinal number of an ASCII char given as byte into the
    /// ordinal number of the corresponding upper case ASCII char. Works only
    /// on a-z and works like the System.Pas variant just on bytes instead of chars
    /// </summary>
    /// <param name="b">
    /// Ordinal ASCII char value to be converted to upper case
    /// </param>
    /// <returns>
    /// Uppercase ordinal number if the number passed in as parameter belongs to
    /// a char in the a-z range. Otherwise the number passed in will be returned.
    /// </returns>
    class function UpCaseBinary(b: Byte): Byte;

    /// <summary>
    /// Looks for the index of a given byte in a byte-array.
    /// </summary>
    /// <param name="Value">
    /// Byte value to be searched in the array
    /// </param>
    /// <param name="Table">
    /// Byte-array where the value is searched in
    /// </param>
    /// <param name="Len">
    /// Maximum index until which the search will be performed. If Len is higher
    /// than length(Table) the latter will be used as maximum
    /// </param>
    /// <returns>
    /// Index of the first appearance of the searched value. If it cannot be found
    /// the result will be -1. The index is 0 based.
    /// </returns>
    class function TableFindBinary(Value: Byte; Table: TBytes;
      Len: Integer): Integer;
  end;

  /// <summary>
  /// Formatting class which doesn't apply any transformation to the data
  /// passed in. It simply copies it from Source to Dest.
  /// </summary>
  TFormat_Copy = class(TDECFormat)
  protected
    /// <summary>
    /// Copies the data contained in Source into Dest without any conversion
    /// </summary>
    /// <param name="Source">
    /// Variable from which Size bytes will be copied to Dest
    /// </param>
    /// <param name="Dest">
    /// Byte-array where Source will be copied into. It will be dimensioned
    /// to a length of Size internally.
    /// </param>
    /// <param name="Size">
    /// Number of bytes to copy from Soruce to Dest
    /// </param>
    class procedure DoEncode(const Source; var Dest: TBytes;
      Size: Integer); override;
    /// <summary>
    /// Copies the data contained in Source into Dest without any conversion
    /// </summary>
    /// <param name="Source">
    /// Variable from which Size bytes will be copied to Dest
    /// </param>
    /// <param name="Dest">
    /// Byte-array where Source will be copied into. It will be dimensioned
    /// to a length of Size internally.
    /// </param>
    /// <param name="Size">
    /// Number of bytes to copy from Soruce to Dest
    /// </param>
    class procedure DoDecode(const Source; var Dest: TBytes;
      Size: Integer); override;
    /// <summary>
    /// Dummy function to check if Source is valid for this particular format
    /// </summary>
    /// <param name="Data">
    /// Data to be checked for validity. In this dummy case it will only be
    /// checked for Size >= 0
    /// </param>
    /// <param name="Size">
    /// Number of bytes the Source to be checked contains
    /// </param>
    /// <returns>
    /// true if Size >= 0
    /// </returns>
    class function DoIsValid(const Data; Size: Integer): Boolean; override;
  public
  end;

  /// <summary>
  /// Returns the passed class type if it is not nil. Otherwise the class type
  /// of the TFormat_Copy class is being returned.
  /// </summary>
  /// <param name="FormatClass">
  /// Class type of a formatting class like TFormat_HEX or nil, if no formatting
  /// is desired.
  /// </param>
  /// <returns>
  /// Passed class type or TFormat_Copy class type, depending on FormatClass
  /// parameter value.
  /// </returns>
function ValidFormat(FormatClass: TDECFormatClass = nil): TDECFormatClass;

/// <summary>
/// Searches a registered formatting class by name.
/// </summary>
/// <param name="Name">
/// Unique long (TFormat_HEXL) or short (HEXL) name of the class to be searched.
/// </param>
/// <returns>
/// Class type, which can be used to create an object isntance from. Raises an
/// EDECClassNotRegisteredException exception if the class cannot be found in
/// the list of registered format classes.
/// </returns>
function FormatByName(const Name: string): TDECFormatClass;

/// <summary>
/// Searches a registered formatting class by identity. The identity is some
/// integer value calculated on the basis of the class name, the length of the
/// name and a fixed prefix and by calculating a CRC32 checksum of this.
/// </summary>
/// <param name="Identity">
/// Unique identity of the class to be searched.
/// </param>
/// <returns>
/// Class type, which can be used to create an object isntance from. Raises an
/// EDECClassNotRegisteredException exception if the class cannot be found in
/// the list of registered format classes.
/// </returns>
function FormatByIdentity(Identity: Int64): TDECFormatClass;

implementation

function ValidFormat(FormatClass: TDECFormatClass = nil): TDECFormatClass;
begin
  if FormatClass <> nil then
    Result := FormatClass
  else
    Result := TFormat_Copy;
end;

function FormatByName(const Name: string): TDECFormatClass;
begin
  Result := TDECFormatClass(TDECFormat.ClassList.ClassByName(Name));
end;

function FormatByIdentity(Identity: Int64): TDECFormatClass;
begin
  Result := TDECFormatClass(TDECFormat.ClassList.ClassByIdentity(Identity));
end;

{ TDECFormat }

class procedure TDECFormat.DoEncode(const Source; var Dest: TBytes;
  Size: Integer);
begin
  // C++ does not support virtual static functions thus the base cannot be
  // marked 'abstract'. This is our workaround:
  raise EDECAbstractError.Create(GetShortClassName);
end;

class procedure TDECFormat.DoDecode(const Source; var Dest: TBytes;
  Size: Integer);
begin
  // C++ does not support virtual static functions thus the base cannot be
  // marked 'abstract'. This is our workaround:
  raise EDECAbstractError.Create(GetShortClassName);
end;

class function TDECFormat.DoIsValid(const Data; Size: Integer): Boolean;
begin
{$IFDEF FPC}
  Result := False; // suppress FPC compiler warning
{$ENDIF FPC}
  // C++ does not support virtual static functions thus the base cannot be
  // marked 'abstract'. This is our workaround:
  raise EDECAbstractError.Create(GetShortClassName);
end;

class function TDECFormat.Encode(const Data: RawByteString): RawByteString;
var
  b: TBytes;
begin
  if Length(Data) > 0 then
  begin
    {$IF CompilerVersion >= 24.0}
    DoEncode(Data[Low(Data)], b, Length(Data) * SizeOf(Data[Low(Data)]));
    {$ELSE}
    DoEncode(Data[1], b, Length(Data) * SizeOf(Data[1]));
    {$IFEND}
    Result := BytesToRawString(b);
  end
  else
    SetLength(Result, 0);
end;

class function TDECFormat.Encode(const Data: TBytes): TBytes;
var
  b: TBytes;
begin
  if Length(Data) > 0 then
  begin
    DoEncode(Data[0], b, Length(Data));
    Result := b;
  end
  else
    SetLength(Result, 0);
end;

class function TDECFormat.ClassByIdentity(Identity: Int64): TDECFormatClass;
begin
  Result := TDECFormatClass(ClassList.ClassByIdentity(Identity));
end;

class function TDECFormat.ClassByName(const Name: string): TDECFormatClass;
begin
  Result := TDECFormatClass(ClassList.ClassByName(Name));
end;

class function TDECFormat.Decode(const Data: TBytes): TBytes;
var
  b: TBytes;
begin
  if Length(Data) > 0 then
  begin
    DoDecode(Data[0], b, Length(Data));
    Result := b;
  end
  else
    SetLength(Result, 0);
end;

class function TDECFormat.Decode(const Data: RawByteString): RawByteString;
var
  b: TBytes;
begin
  if Length(Data) > 0 then
  begin
    {$IF CompilerVersion >= 24.0}
    DoDecode(Data[Low(Data)], b, Length(Data) * SizeOf(Data[Low(Data)]));
    {$ELSE}
    DoDecode(Data[1], b, Length(Data) * SizeOf(Data[1]));
    {$IFEND}
    Result := BytesToRawString(b);
  end
  else
    SetLength(Result, 0);
end;

class function TDECFormat.Decode(const Data; Size: Integer): RawByteString;
var
  b: TBytes;
begin
  if Size > 0 then
  begin
    DoDecode(Data, b, Size);
    Result := BytesToRawString(b);
  end
  else
    SetLength(Result, 0);
end;

class function TDECFormat.Encode(const Data; Size: Integer): RawByteString;
var
  b: TBytes;
begin
  if Size > 0 then
  begin
    DoEncode(Data, b, Size);
    Result := BytesToRawString(b);
  end
  else
    SetLength(Result, 0);
end;

class function TDECFormat.IsValid(const Data; Size: Integer): Boolean;
begin
  Result := DoIsValid(Data, Size);
end;

class function TDECFormat.IsValid(const Data: TBytes): Boolean;
begin
  Result := (Length(Data) = 0) or (DoIsValid(Data[0], Length(Data)));
end;

class function TDECFormat.IsValid(const Text: RawByteString): Boolean;
begin
  {$IF CompilerVersion >= 24.0}
  Result := (Length(Text) = 0) or
    (DoIsValid(Text[Low(Text)], Length(Text) * SizeOf(Text[Low(Text)])));
  {$ELSE}
  Result := (Length(Text) = 0) or
    (DoIsValid(Text[1], Length(Text) * SizeOf(Text[1])));
  {$IFEND}
end;

class function TDECFormat.UpCaseBinary(b: Byte): Byte;
begin
  Result := b;
  if Result in [$61 .. $7A] then
    Dec(Result, $61 - $41);
end;

class function TDECFormat.TableFindBinary(Value: Byte; Table: TBytes;
  Len: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  i := 0;
  while (i <= Len) and (i < Length(Table)) do
  begin
    if (Table[i] = Value) then
    begin
      Result := i;
      break;
    end;

    inc(i);
  end;
end;

{ TFormat_Copy }

class procedure TFormat_Copy.DoEncode(const Source; var Dest: TBytes;
  Size: Integer);
begin
  SetLength(Dest, Size);
  if Size <> 0 then
    Move(Source, Dest[0], Size);
end;

class procedure TFormat_Copy.DoDecode(const Source; var Dest: TBytes;
  Size: Integer);
begin
  SetLength(Dest, Size);
  if Size <> 0 then
    Move(Source, Dest[0], Size);
end;

class function TFormat_Copy.DoIsValid(const Data; Size: Integer): Boolean;
begin
  Result := Size >= 0;
end;

{$IFDEF DELPHIORBCB}

procedure ModuleUnload(Instance: NativeInt);
var
  i: Integer;
begin
  if TDECFormat.ClassList <> nil then
  begin
    for i := TDECFormat.ClassList.Count - 1 downto 0 do
    begin
      if NativeInt(FindClassHInstance(TClass(TDECFormat.ClassList[i]))) = Instance
      then
        TDECFormat.ClassList.Remove(TDECFormat.ClassList[i].Identity);
    end;
  end;
end;
{$ENDIF DELPHIORBCB}

initialization

// Code for packages and dynamic extension of the class registration list
{$IFDEF DELPHIORBCB}
  AddModuleUnloadProc(ModuleUnload);
{$ENDIF DELPHIORBCB}
TDECFormat.ClassList := TDECClassList.Create;

TFormat_Copy.RegisterClass(TDECFormat.ClassList);

finalization

// Ensure no further instances of classes registered in the registration list
// are possible through the list after this unit has been unloaded by unloding
// the package this unit is in
{$IFDEF DELPHIORBCB}
  RemoveModuleUnloadProc(ModuleUnload);
{$ENDIF DELPHIORBCB}
TDECFormat.ClassList.Free;

end.
