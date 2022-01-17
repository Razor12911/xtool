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

unit DECBaseClass;

interface

{$INCLUDE DECOptions.inc}

uses
  {$IFDEF FPC}
  SysUtils, Classes,
  {$ELSE}
  System.SysUtils, System.Classes,
  {$ENDIF}
  Generics.Collections;

type
  /// <summary>
  ///   Class type for the base class from which all other DEC classes inherit
  ///   in order to be able to create lists of classes, pick an entry of such a
  ///   list and construct an object out of it
  /// </summary>
  TDECClass = class of TDECObject;

  /// <summary>
  ///   Generic list of DEC classes with the identity as key
  /// </summary>
  TDECClassList = class(TDictionary<Int64, TDECClass>)
  strict private
    /// <summary>
    ///   Checks if a given class type has the same short class name as given
    /// </summary>
    /// <param name="Name">
    ///   Short class name, e.g. HEXL
    /// </param>
    /// <param name="ClassType">
    ///   Class reference to check against
    /// </param>
    /// <returns>
    ///   true if the class reference is for the given short name
    /// </returns>
    function DoFindNameShort(const Name: string; const ClassType: TDECClass): Boolean;
    /// <summary>
    ///   Checks if a given class type has the same long class name as given
    /// </summary>
    /// <param name="Name">
    ///   Long class name, e.g. TFormat_HEXL
    /// </param>
    /// <param name="ClassType">
    ///   Class reference to check against
    /// </param>
    /// <returns>
    ///   true if the class reference is for the given long name
    /// </returns>
    function DoFindNameLong(const Name: string; const ClassType: TClass): Boolean;
  public
    /// <summary>
    ///   Tries to find a class type by its name
    /// </summary>
    /// <param name="Name">
    ///   Name to look for in the list
    /// </param>
    /// <returns>
    ///   Returns the class type if found. if it could not be found a
    ///   EDECClassNotRegisteredException will be thrown
    /// </returns>
    function ClassByName(const Name: string): TDECClass;

    /// <summary>
    ///   Tries to find a class type by its numeric identity DEC assigned to it.
    ///   Useful for file headers, so they can easily encode numerically which
    ///   cipher class was being used.
    /// </summary>
    /// <param name="Identity">
    ///   Identity to look for
    /// </param>
    /// <returns>
    ///   Returns the class type of the class with the specified identity value
    ///   or throws an EDECClassNotRegisteredException exception if no class
    ///   with the given identity has been found
    /// </returns>
    function ClassByIdentity(Identity: Int64): TDECClass;
    /// <summary>
    ///   Returns a list of all classes registered in this list
    /// </summary>
    /// <param name="List">
    ///   List where the registered classes shall be added to. The string is the
    ///   long class name, the object the class reference. The list is being
    ///   cleared first and when an uncreated list is given nothing is being done
    /// </param>
    procedure GetClassList(List: TStrings);
  end;

  /// <summary>
  ///   Parent class of all cryptography and hash implementations
  /// </summary>
  TDECObject = class(TInterfacedObject)
  public
    /// <summary>
    ///   Overrideable but otherwise empty constructor (calls his parent
    ///   constructor or course)
    /// </summary>
    constructor Create; virtual;
    /// <summary>
    ///   This function creates a unique Signature for each class using the
    ///   following naming scheme:
    ///
    ///   'Z' repeated n times (to fill space of 256 chars) + DEC ClassName
    ///
    ///   The CRC32 of the generated Signature is used as our unique Identity
    ///
    ///   Important Note:
    ///   DEC 5.2 introduced a bug which breaks backward compatibility with
    ///   DEC 5.1 by using String instead of AnsiString. This leads to different
    ///   Identities when using Unicode capable Delphi Versions (Delphi 2009+).
    ///
    ///   To restore the *wrong* behavior of DEC 5.2 enable the DEC52_IDENTITY option
    ///   in the configuration file DECOptions.inc.
    ///
    ///   With this and all future versions we will keep backward compatibility.
    /// </summary>
    class function Identity: Int64;
    {$IFDEF X86ASM}
    /// <summary>
    ///   Override FreeInstance to fill allocated Object with zeros, that is
    ///   safer for any access to invalid Pointers of released Objects.
    /// </summary>
    procedure FreeInstance; override;
    {$ENDIF X86ASM}

    /// <summary>
    ///   Registers this class type in the list of DEC classes (ClassList).
    ///   Trying to register an already registered class will raise an exception.
    /// </summary>
    /// <param name="ClassList">
    ///   List to which the own class type shall be added. This allows subclasses
    ///   to have their own lists
    /// </param>
    class procedure RegisterClass(ClassList : TDECClassList);
    /// <summary>
    ///   Removes tthis class type from the list of registered DEC classes
    ///   (ClassList). Trying to unregister a non registered class is a do nothing
    ///   operation.
    /// </summary>
    /// <param name="ClassList">
    ///   List from which the own class type shall be removed. This allows
    ///   subclasses to have their own lists
    /// </param>
    class procedure UnregisterClass(ClassList : TDECClassList);

    /// <summary>
    ///   Returns short Classname of any DEC derrived class. This is the part
    ///   of the class name after the _ so for THash_RipeMD160 it will be RipeMD160.
    /// </summary>
    /// <param name="ClassName">
    ///   Complete class name
    /// </param>
    /// <returns>
    ///   Short class name
    /// </returns>
    class function GetShortClassNameFromName(const ClassName: string): string;

    /// <summary>
    ///   Returns short Classname of any DEC derrived class type. This is the part
    ///   of the class name after the _ so for THash_RipeMD160 it will be RipeMD160.
    /// </summary>
    /// <returns>
    ///   Short class name or empty string if ClassType is nil.
    /// </returns>
    class function GetShortClassName: string;
  end;

var
  /// <summary>
  ///   default used for generating class identities
  /// </summary>
  IdentityBase: Int64 = $25844852;

  /// <summary>
  ///   Size in bytes used for buffering data read from or written to a stream
  /// </summary>
  StreamBufferSize: Integer = 8192;

  {$IFDEF NEXTGEN}
  EmptyStr: string = '';
  /// <summary>
  ///   Pointer to an empty string. For non Nextgen platforms declared in SysUtils
  ///   for backwards compatibility only. Here declared for NextGen only and
  ///   should get replaced
  /// </summary>
  NullStr: PString = @EmptyStr;
  {$ENDIF}

implementation

uses
  DECUtil, DECCRC;

resourcestring
  sClassNotRegistered = 'Class %s is not registered';
  sWrongIdentity      = 'Another class "%s" with the same identity as "%s" has already been registered';

constructor TDECObject.Create;
begin
  inherited Create;
end;

class function TDECObject.Identity: Int64;
var
  Signature: {$IFDEF DEC52_IDENTITY}string{$ELSE !DEC52_IDENTITY}RawByteString{$ENDIF !DEC52_IDENTITY};
begin
  {$IFDEF DEC52_IDENTITY}
  Signature := StringOfChar(#$5A, 256 - Length(ClassName)) + UpperCase(ClassName);
    {$IF CompilerVersion >= 24.0}
    Result := CRC32(IdentityBase, Signature[Low(Signature)],
                                  Length(Signature) * SizeOf(Signature[Low(Signature)]));
    {$ELSE}
    Result := CRC32(IdentityBase, Signature[Low(Signature)],
                                  Length(Signature) * SizeOf(Signature[1]));
    {$IFEND}
  {$ELSE !DEC52_IDENTITY}
  Signature := RawByteString(StringOfChar(#$5A, 256 - Length(ClassName)) + UpperCase(ClassName));
    {$IF CompilerVersion >= 24.0}
    Result := CRC32(IdentityBase, Signature[Low(Signature)],
                                  Length(Signature) * SizeOf(Signature[Low(Signature)]));
    {$ELSE}
    Result := CRC32(IdentityBase, Signature[1],
                                  Length(Signature) * SizeOf(Signature[1]));
    {$IFEND}
  {$ENDIF !DEC52_IDENTITY}
end;

class procedure TDECObject.RegisterClass(ClassList : TDECClassList);
begin
  ClassList.Add(Identity, self);
end;

{$IFDEF X86ASM}
procedure TDECObject.FreeInstance;
// Override FreeInstance to fill allocated Object with zeros, that is
// safer for any access to invalid Pointers of released Objects
asm
      PUSH    EBX
      PUSH    EDI
      MOV     EBX,EAX
      CALL    TObject.CleanupInstance
      MOV     EAX,[EBX]
      CALL    TObject.InstanceSize
      MOV     ECX,EAX
      MOV     EDI,EBX
      XOR     EAX,EAX
      REP     STOSB
      MOV     EAX,EBX
      CALL    System.@FreeMem
      POP     EDI
      POP     EBX
end;
{$ENDIF X86ASM}

class procedure TDECObject.UnregisterClass(ClassList : TDECClassList);
begin
  ClassList.Remove(Identity);
end;

class function TDECObject.GetShortClassName: string;
begin
  Result := GetShortClassNameFromName(self.ClassName);
end;

class function TDECObject.GetShortClassNameFromName(const ClassName: string): string;
var
  i: Integer;
begin
  Result := ClassName;
  i := Pos('_', Result);
  if i > 0 then
    Delete(Result, 1, i);
end;

{ TDECClassList }

function TDECClassList.DoFindNameShort(const Name: string; const ClassType: TDECClass): Boolean;
begin
  Result := CompareText(ClassType.GetShortClassName, Name) = 0;
end;

function TDECClassList.DoFindNameLong(const Name: string; const ClassType: TClass): Boolean;
var
  s: string;
begin
  s := Name;
  Result := CompareText(ClassType.ClassName, Name) = 0;
end;

function TDECClassList.ClassByIdentity(Identity: Int64): TDECClass;
begin
  try
    Result := Items[Identity];
  except
    On EListError do
      raise EDECClassNotRegisteredException.CreateResFmt(@sClassNotRegistered,
                                                         [IntToHEX(Identity, 8)]);
  end;
end;

function TDECClassList.ClassByName(const Name: string): TDECClass;
var
  FindNameShort : Boolean;
  Pair          : TPair<Int64, TDECCLass>;
begin
  Result := nil;

  if Length(Name) > 0 then
  begin
    FindNameShort := TDECClass.GetShortClassNameFromName(Name) = Name;

    for Pair in self do
    begin
      if FindNameShort then
      begin
        if DoFindNameShort(Name, Pair.Value) then
        begin
          result := Pair.Value;
          break;
        end;
      end
      else
        if DoFindNameLong(Name, Pair.Value) then
        begin
          result := Pair.Value;
          break;
        end;
    end;
  end;

  if Result = nil then
    raise EDECClassNotRegisteredException.CreateResFmt(@sClassNotRegistered, [Name]);
end;

procedure TDECClassList.GetClassList(List: TStrings);
var
  Pair : TPair<Int64, TDECCLass>;
begin
  if List <> nil then
  try
    List.BeginUpdate;
    List.Clear;

    for Pair in self do
      List.AddObject(Pair.Value.ClassName, TObject(Pair.Value));

  finally
    List.EndUpdate;
  end;
end;

initialization

finalization

end.
