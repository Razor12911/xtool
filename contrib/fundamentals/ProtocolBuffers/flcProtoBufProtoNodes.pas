{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals 5.00                                        }
{   File name:        flcProtoBufProtoNodes.pas                                }
{   File version:     5.04                                                     }
{   Description:      Protocol Buffer proto file nodes.                        }
{                                                                              }
{   Copyright:        Copyright (c) 2012-2016, David J Butler                  }
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
{   2012/04/15  0.01  Initial development: Proto nodes.                        }
{   2012/04/26  0.02  Resolve delimited identifiers.                           }
{   2016/01/14  0.03  RawByteString changes.                                   }
{   2016/01/14  5.04  Revised for Fundamentals 5.                              }
{                                                                              }
{******************************************************************************}

{$INCLUDE flcProtoBuf.inc}

unit flcProtoBufProtoNodes;

interface

uses
  { System }
  SysUtils,
  { Fundamentals }
  flcUtils;



type
  TpbProtoNodeFactory = class;

  TpbProtoNode = class
  protected

  public
    function  ResolveValue(const AIdentifier: RawByteString; const AChildrenOnly: Boolean): TpbProtoNode; virtual;
    function  ResolveType(const AIdentifier: RawByteString; const AChildrenOnly: Boolean): TpbProtoNode; virtual;

    function  ExpectResolvedValue(const AIdentifier: RawByteString): TpbProtoNode;
    function  ExpectResolvedType(const AIdentifier: RawByteString): TpbProtoNode;

    function  GetAsProtoString: RawByteString; virtual;
  end;

  EpbProtoNode = class(Exception);

  TpbProtoEnum = class; // forward

  TpbProtoEnumValue = class(TpbProtoNode)
  protected
    FParentEnum : TpbProtoEnum;
    FName       : RawByteString;
    FValue      : LongInt;

  public
    constructor Create(const AParentEnum: TpbProtoEnum);

    property  Name: RawByteString read FName write FName;
    property  Value: LongInt read FValue write FValue;

    function  GetAsProtoString: RawByteString; override;
  end;

  TpbProtoMessage = class; // forward

  TpbProtoEnum = class(TpbProtoNode)
  protected
    FName   : RawByteString;
    FValues : array of TpbProtoEnumValue;

  public
    constructor Create(const AParentNode: TpbProtoNode);

    property  Name: RawByteString read FName write FName;

    procedure Add(const V: TpbProtoEnumValue);
    function  GetValueCount: Integer;
    function  GetValue(const Idx: Integer): TpbProtoEnumValue;
    function  GetValueByName(const AName: RawByteString): TpbProtoEnumValue;

    function  ResolveValue(const AIdentifier: RawByteString; const AChildrenOnly: Boolean): TpbProtoNode; override;
    function  GetAsProtoString: RawByteString; override;
  end;

  TpbProtoLiteralType = (
    pltNone,
    pltInteger,
    pltFloat,
    pltString,
    pltBoolean,
    pltIdentifier
  );

  TpbProtoLiteral = class(TpbProtoNode)
  protected
    FParentNode   : TpbProtoNode;
    FLiteralType  : TpbProtoLiteralType;
    FLiteralInt   : Int64;
    FLiteralFloat : Extended;
    FLiteralStr   : RawByteString;
    FLiteralBool  : Boolean;
    FLiteralIden  : RawByteString;

  public
    constructor Create(const AParentNode: TpbProtoNode);

    property  LiteralType: TpbProtoLiteralType read FLiteralType write FLiteralType;
    property  LiteralInt: Int64 read FLiteralInt write FLiteralInt;
    property  LiteralFloat: Extended read FLiteralFloat write FLiteralFloat;
    property  LiteralStr: RawByteString read FLiteralStr write FLiteralStr;
    property  LiteralBool: Boolean read FLiteralBool write FLiteralBool;
    property  LiteralIden: RawByteString read FLiteralIden write FLiteralIden;

    function  ResolveValue(const AIdentifier: RawByteString; const AChildrenOnly: Boolean): TpbProtoNode; override;
    function  GetAsProtoString: RawByteString; override;

    function  LiteralIdenValue: TpbProtoNode;
  end;

  TpbProtoOption = class(TpbProtoNode)
  protected
    FCustom : Boolean;
    FName   : RawByteString;
    FValue  : TpbProtoLiteral;

    procedure SetValue(const Value: TpbProtoLiteral);

  public
    constructor Create(const AFactory: TpbProtoNodeFactory);
    destructor Destroy; override;

    property  Custom: Boolean read FCustom write FCustom;
    property  Name: RawByteString read FName write FName;
    property  Value: TpbProtoLiteral read FValue write FValue;
  end;

  TpbProtoFieldCardinality = (
    pfcNone,
    pfcRequired,
    pfcOptional,
    pfcRepeated
  );

  TpbProtoFieldBaseType = (
    pftNone,

    pftDouble,
    pftFloat,
    pftInt32,
    pftInt64,
    pftUInt32,
    pftUInt64,
    pftSInt32,
    pftSInt64,
    pftFixed32,
    pftFixed64,
    pftSFixed32,
    pftSFixed64,
    pftBool,
    pftString,
    pftBytes,

    pftIdentifier
  );

  TpbProtoField = class; // forward

  TpbProtoFieldType = class(TpbProtoNode)
  protected
    FParentField : TpbProtoField;
    FBaseType    : TpbProtoFieldBaseType;
    FIdenStr     : RawByteString;

  public
    constructor Create(const AParentField: TpbProtoField);

    property  ParentField: TpbProtoField read FParentField;
    property  BaseType: TpbProtoFieldBaseType read FBaseType write FBaseType;
    property  IdenStr: RawByteString read FIdenStr write FIdenStr;

    function  ResolveType(const AIdentifier: RawByteString; const AChildrenOnly: Boolean): TpbProtoNode; override;
    function  GetAsProtoString: RawByteString; override;

    function  IsSimpleType: Boolean;
    function  IsIdenType: Boolean;
    function  IdenType: TpbProtoNode;
  end;

  TpbProtoField = class(TpbProtoNode)
  protected
    FParentMessage : TpbProtoMessage;
    FName          : RawByteString;
    FFieldType     : TpbProtoFieldType;
    FCardinality   : TpbProtoFieldCardinality;
    FTagID         : LongInt;
    FDefaultValue  : TpbProtoLiteral;
    FOptionPacked  : Boolean;
    FOptions       : array of TpbProtoOption;

    procedure SetFieldType(const Value: TpbProtoFieldType);
    procedure SetDefaultValue(const Value: TpbProtoLiteral);

  public
    constructor Create(const AParentMessage: TpbProtoMessage; const AFactory: TpbProtoNodeFactory);
    destructor Destroy; override;

    property  ParentMessage: TpbProtoMessage read FParentMessage;
    property  Name: RawByteString read FName write FName;
    property  FieldType: TpbProtoFieldType read FFieldType write SetFieldType;
    property  Cardinality: TpbProtoFieldCardinality read FCardinality write FCardinality;
    property  TagID: LongInt read FTagID write FTagID;
    property  DefaultValue: TpbProtoLiteral read FDefaultValue write SetDefaultValue;
    property  OptionPacked: Boolean read FOptionPacked write FOptionPacked;

    procedure AddOption(const Option: TpbProtoOption);
    function  GetOptionCount: Integer;
    function  GetOption(const Idx: Integer): TpbProtoOption;

    function  ResolveValue(const AIdentifier: RawByteString; const AChildrenOnly: Boolean): TpbProtoNode; override;
    function  ResolveType(const AIdentifier: RawByteString; const AChildrenOnly: Boolean): TpbProtoNode; override;

    function  GetAsProtoString: RawByteString; override;
  end;

  TpbProtoMessage = class(TpbProtoNode)
  protected
    FParentNode    : TpbProtoNode;
    FName          : RawByteString;
    FMessages      : array of TpbProtoMessage;
    FFields        : array of TpbProtoField;
    FEnums         : array of TpbProtoEnum;
    FExtensionsMin : LongInt;
    FExtensionsMax : LongInt;

    function  GetFieldCount: Integer;
    function  GetField(const Idx: Integer): TpbProtoField;

    function  GetEnumCount: Integer;
    function  GetEnum(const Idx: Integer): TpbProtoEnum;
    function  GetEnumByName(const AName: RawByteString): TpbProtoEnum;

    function  GetMessageCount: Integer;
    function  GetMessage(const Idx: Integer): TpbProtoMessage;
    function  GetMessageByName(const AName: RawByteString): TpbProtoMessage;

  public
    constructor Create(const AParentNode: TpbProtoNode);
    destructor Destroy; override;

    property  ParentNode: TpbProtoNode read FParentNode;

    property  Name: RawByteString read FName write FName;

    procedure AddField(const F: TpbProtoField);
    property  FieldCount: Integer read GetFieldCount;
    property  Field[const Idx: Integer]: TpbProtoField read GetField;
    function  GetFieldByTagID(const ATagID: Integer): TpbProtoField;

    procedure AddEnum(const E: TpbProtoEnum);
    property  EnumCount: Integer read GetEnumCount;
    property  Enum[const Idx: Integer]: TpbProtoEnum read GetEnum;

    procedure AddMessage(const M: TpbProtoMessage);
    property  MessageCount: Integer read GetMessageCount;
    property  Msg[const Idx: Integer]: TpbProtoMessage read GetMessage;

    property  ExtensionsMin: LongInt read FExtensionsMin write FExtensionsMin;
    property  ExtensionsMax: LongInt read FExtensionsMax write FExtensionsMax;

    function  ResolveValue(const AIdentifier: RawByteString; const AChildrenOnly: Boolean): TpbProtoNode; override;
    function  ResolveType(const AIdentifier: RawByteString; const AChildrenOnly: Boolean): TpbProtoNode; override;

    function  GetAsProtoString: RawByteString; override;
  end;

  TpbProtoPackage = class(TpbProtoNode)
  protected
    FFileName         : RawByteString;
    FName             : RawByteString;
    FMessages         : array of TpbProtoMessage;
    FImports          : array of RawByteString;
    FImportedPackages : array of TpbProtoPackage;
    FOptions          : array of TpbProtoOption;
    FEnums            : array of TpbProtoEnum;

    function  GetEnumCount: Integer;
    function  GetEnum(const Idx: Integer): TpbProtoEnum;
    function  GetEnumByName(const AName: RawByteString): TpbProtoEnum;

  public
    constructor Create;
    destructor Destroy; override;

    property  FileName: RawByteString read FFileName write FFileName;
    property  Name: RawByteString read FName write FName;

    procedure AddMessage(const M: TpbProtoMessage);
    function  GetMessageCount: Integer;
    function  GetMessage(const Idx: Integer): TpbProtoMessage;
    function  GetMessageByName(const AName: RawByteString): TpbProtoMessage;

    procedure AddImport(const Path: RawByteString);
    function  GetImportCount: Integer;
    function  GetImport(const Idx: Integer): RawByteString;

    procedure AddImportedPackage(const Package: TpbProtoPackage);
    function  GetImportedPackageCount: Integer;
    function  GetImportedPackage(const Idx: Integer): TpbProtoPackage;
    function  GetImportedPackageByName(const AName: RawByteString): TpbProtoPackage;

    procedure AddOption(const Option: TpbProtoOption);
    function  GetOptionCount: Integer;
    function  GetOption(const Idx: Integer): TpbProtoOption;

    procedure AddEnum(const E: TpbProtoEnum);
    property  EnumCount: Integer read GetEnumCount;
    property  Enum[const Idx: Integer]: TpbProtoEnum read GetEnum;

    function  ResolveValue(const AIdentifier: RawByteString; const AChildrenOnly: Boolean): TpbProtoNode; override;
    function  ResolveType(const AIdentifier: RawByteString; const AChildrenOnly: Boolean): TpbProtoNode; override;

    function  GetAsProtoString: RawByteString; override;
  end;

  TpbProtoNodeFactory = class
  public
    function  CreatePackage: TpbProtoPackage; virtual;
    function  CreateMessage(const AParentNode: TpbProtoNode): TpbProtoMessage; virtual;
    function  CreateField(const AParentMessage: TpbProtoMessage): TpbProtoField; virtual;
    function  CreateFieldType(const AParentField: TpbProtoField): TpbProtoFieldType; virtual;
    function  CreateLiteral(const AParentNode: TpbProtoNode): TpbProtoLiteral; virtual;
    function  CreateEnum(const AParentNode: TpbProtoNode): TpbProtoEnum; virtual;
    function  CreateEnumValue(const AParentEnum: TpbProtoEnum): TpbProtoEnumValue; virtual;
  end;



{ Default ProtoNode factory }

function GetDefaultProtoNodeFactory: TpbProtoNodeFactory;



implementation

uses
  { Fundamentals }
  flcStrings,
  flcFloats;



const
  pbCRLF = RawByteString(#13#10);

  ProtoFieldCardinalityStr : array[TpbProtoFieldCardinality] of RawByteString = (
    '',
    'required',
    'optional',
    'repeated'
  );

  ProtoFieldTypeStr : array[TpbProtoFieldBaseType] of RawByteString = (
    '',
    'double',
    'float',
    'int32',
    'int64',
    'uint32',
    'uint64',
    'sint32',
    'sint64',
    'fixed32',
    'fixed64',
    'sfixed32',
    'sfixed64',
    'bool',
    'string',
    'bytes',
    ''
  );

  SErr_IdentifierNotDefined = 'Identifier not defined (%s)';
  SErr_IdentifierNotAValue  = 'Identifier not a value (%s)';



{ Helper functions }

procedure pbSplitIdentifier(const AIdentifier: RawByteString;
          var ALeftName, ARightIdentifier: RawByteString);
begin
  StrSplitAtCharB(AIdentifier, '.', ALeftName, ARightIdentifier, True);
end;



{ TpbProtoNode }

function TpbProtoNode.ResolveValue(const AIdentifier: RawByteString; const AChildrenOnly: Boolean): TpbProtoNode;
begin
  raise EpbProtoNode.CreateFmt('%s.ResolveValue not implemented', [ClassName]);
end;

function TpbProtoNode.ResolveType(const AIdentifier: RawByteString; const AChildrenOnly: Boolean): TpbProtoNode;
begin
  raise EpbProtoNode.CreateFmt('%s.ResolveType not implemented', [ClassName]);
end;

function TpbProtoNode.ExpectResolvedValue(const AIdentifier: RawByteString): TpbProtoNode;
begin
  Result := ResolveValue(AIdentifier, False);
  if not Assigned(Result) then
    raise EpbProtoNode.CreateFmt(SErr_IdentifierNotDefined, [AIdentifier]);
end;

function TpbProtoNode.ExpectResolvedType(const AIdentifier: RawByteString): TpbProtoNode;
begin
  Result := ResolveType(AIdentifier, False);
  if not Assigned(Result) then
    raise EpbProtoNode.CreateFmt(SErr_IdentifierNotDefined, [AIdentifier]);
end;

function TpbProtoNode.GetAsProtoString: RawByteString;
begin
  raise EpbProtoNode.CreateFmt('%s.GetAsProtoString not implemented', [ClassName]);
end;



{ TpbProtoEnumValue }

constructor TpbProtoEnumValue.Create(const AParentEnum: TpbProtoEnum);
begin
  Assert(Assigned(AParentEnum));
  inherited Create;
  FParentEnum := AParentEnum;
end;

function TpbProtoEnumValue.GetAsProtoString: RawByteString;
begin
  Result := FName + ' = ' + IntToStringB(FValue) + ';';
end;



{ TpbProtoEnum }

constructor TpbProtoEnum.Create(const AParentNode: TpbProtoNode);
begin
  Assert(Assigned(AParentNode));
  inherited Create;
end;

procedure TpbProtoEnum.Add(const V: TpbProtoEnumValue);
var L : Integer;
begin
  L := Length(FValues);
  SetLength(FValues, L + 1);
  FValues[L] := V;
end;

function TpbProtoEnum.GetValueCount: Integer;
begin
  Result := Length(FValues);
end;

function TpbProtoEnum.GetValue(const Idx: Integer): TpbProtoEnumValue;
begin
  Assert((Idx >= 0) and (Idx < Length(FValues)));
  Result := FValues[Idx];
end;

function TpbProtoEnum.GetValueByName(const AName: RawByteString): TpbProtoEnumValue;
var
  I : Integer;
  V : TpbProtoEnumValue;
begin
  for I := 0 to Length(FValues) - 1 do
    begin
      V := FValues[I];
      if V.FName = AName then
        begin
          Result := V;
          exit;
        end;
    end;
  Result := nil;
end;

function TpbProtoEnum.ResolveValue(const AIdentifier: RawByteString; const AChildrenOnly: Boolean): TpbProtoNode;
begin
  Result := GetValueByName(AIdentifier);
end;

function TpbProtoEnum.GetAsProtoString: RawByteString;
var I, L : Integer;
begin
  Result := 'enum ' + FName + ' {' + pbCRLF;
  L := Length(FValues);
  for I := 0 to L - 1 do
    Result := Result + FValues[I].GetAsProtoString + pbCRLF;
  Result := Result + '}';
end;



{ TpbProtoLiteral }

constructor TpbProtoLiteral.Create(const AParentNode: TpbProtoNode);
begin
  Assert(Assigned(AParentNode));
  inherited Create;
  FParentNode := AParentNode;
end;

function TpbProtoLiteral.ResolveValue(const AIdentifier: RawByteString; const AChildrenOnly: Boolean): TpbProtoNode;
begin
  if AChildrenOnly then
    Result := nil
  else
    Result := FParentNode.ResolveValue(AIdentifier, False);
end;

function TpbProtoLiteral.GetAsProtoString: RawByteString;
begin
  case FLiteralType of
    pltNone       : Result := '';
    pltInteger    : Result := IntToStringB(FLiteralInt);
    pltFloat      : Result := FloatToStringB(FLiteralFloat);
    pltString     : Result := StrQuoteB(FLiteralStr, '"');
    pltBoolean    : Result := iifB(FLiteralBool, 'true', 'false');
    pltIdentifier : Result := FLiteralIden;
  else
    raise EpbProtoNode.Create('Invalid literal type');
  end;
end;

function TpbProtoLiteral.LiteralIdenValue: TpbProtoNode;
begin
  if FLiteralType <> pltIdentifier then
    raise EpbProtoNode.Create('Not a identifier type');
  Result := ResolveValue(FLiteralIden, False);
end;



{ TpbProtoFieldType }

constructor TpbProtoFieldType.Create(const AParentField: TpbProtoField);
begin
  Assert(Assigned(AParentField));
  inherited Create;
  FParentField := AParentField;
end;

function TpbProtoFieldType.ResolveType(const AIdentifier: RawByteString; const AChildrenOnly: Boolean): TpbProtoNode;
begin
  if AChildrenOnly then
    Result := nil
  else
    Result := FParentField.ResolveType(AIdentifier, False);
end;

function TpbProtoFieldType.GetAsProtoString: RawByteString;
begin
  if FBaseType = pftIdentifier then
    Result := FIdenStr
  else
    Result := ProtoFieldTypeStr[FBaseType];
end;

function TpbProtoFieldType.IsSimpleType: Boolean;
begin
  Result := not (FBaseType in [pftNone, pftIdentifier]);
end;

function TpbProtoFieldType.IsIdenType: Boolean;
begin
  Result := FBaseType = pftIdentifier;
end;

function TpbProtoFieldType.IdenType: TpbProtoNode;
begin
  if not IsIdenType then
    raise EpbProtoNode.Create('Not an identifier type');
  Result := ExpectResolvedType(FIdenStr);
end;



{ TpbProtoField }

constructor TpbProtoField.Create(const AParentMessage: TpbProtoMessage; const AFactory: TpbProtoNodeFactory);
begin
  Assert(Assigned(AParentMessage));
  Assert(Assigned(AFactory));
  inherited Create;
  FParentMessage := AParentMessage;
  FDefaultValue := AFactory.CreateLiteral(self);
  FFieldType := AFactory.CreateFieldType(self);
end;

destructor TpbProtoField.Destroy;
var I : Integer;
begin
  for I := Length(FOptions) - 1 downto 0 do
    FreeAndNil(FOptions);
  FreeAndNil(FFieldType);
  FreeAndNil(FDefaultValue);
  inherited Destroy;
end;

procedure TpbProtoField.SetFieldType(const Value: TpbProtoFieldType);
begin
  FreeAndNil(FFieldType);
  FFieldType := Value;
end;

procedure TpbProtoField.SetDefaultValue(const Value: TpbProtoLiteral);
begin
  FreeAndNil(FDefaultValue);
  FDefaultValue := Value;
end;

procedure TpbProtoField.AddOption(const Option: TpbProtoOption);
var
  L : Integer;
begin
  L := Length(FOptions);
  SetLength(FOptions, L + 1);
  FOptions[L] := Option;
end;

function TpbProtoField.GetOptionCount: Integer;
begin
  Result := Length(FOptions);
end;

function TpbProtoField.GetOption(const Idx: Integer): TpbProtoOption;
begin
  Assert((Idx >= 0) and (Idx < Length(FOptions)));

  Result := FOptions[Idx];
end;

function TpbProtoField.ResolveValue(const AIdentifier: RawByteString; const AChildrenOnly: Boolean): TpbProtoNode;
begin
  if AChildrenOnly then
    Result := nil
  else
    Result := FParentMessage.ResolveValue(AIdentifier, False);
end;

function TpbProtoField.ResolveType(const AIdentifier: RawByteString; const AChildrenOnly: Boolean): TpbProtoNode;
begin
  if AChildrenOnly then
    Result := nil
  else
    Result := FParentMessage.ResolveType(AIdentifier, False);
end;

function TpbProtoField.GetAsProtoString: RawByteString;
begin
  Result :=
      ProtoFieldCardinalityStr[FCardinality] + ' ' +
      FFieldType.GetAsProtoString + ' ' +
      FName + ' = ' +
      IntToStringB(FTagID) + ';';
end;



{ TpbProtoMessage }

constructor TpbProtoMessage.Create(const AParentNode: TpbProtoNode);
begin
  Assert(Assigned(AParentNode));
  inherited Create;
  FParentNode := AParentNode;
end;

destructor TpbProtoMessage.Destroy;
var I : Integer;
begin
  for I := Length(FEnums) - 1 downto 0 do
    FreeAndNil(FEnums[I]);
  for I := Length(FFields) - 1 downto 0 do
    FreeAndNil(FFields[I]);
  for I := Length(FMessages) - 1 downto 0 do
    FreeAndNil(FMessages[I]);
  inherited Destroy;
end;

procedure TpbProtoMessage.AddField(const F: TpbProtoField);
var L : Integer;
begin
  L := Length(FFields);
  SetLength(FFields, L + 1);
  FFields[L] := F;
end;

function TpbProtoMessage.GetFieldCount: Integer;
begin
  Result := Length(FFields);
end;

function TpbProtoMessage.GetField(const Idx: Integer): TpbProtoField;
begin
  Assert((Idx >= 0) and (Idx < Length(FFields)));
  Result := FFields[Idx];
end;

function TpbProtoMessage.GetFieldByTagID(const ATagID: Integer): TpbProtoField;
var
  I : Integer;
  F : TpbProtoField;
begin
  for I := 0 to Length(FFields) - 1 do
    begin
      F := GetField(I);
      if F.FTagID = ATagID then
        begin
          Result := F;
          exit;
        end;
    end;
  Result := nil;
end;

procedure TpbProtoMessage.AddEnum(const E: TpbProtoEnum);
var L : Integer;
begin
  L := Length(FEnums);
  SetLength(FEnums, L + 1);
  FEnums[L] := E;
end;

function TpbProtoMessage.GetEnumCount: Integer;
begin
  Result := Length(FEnums);
end;

function TpbProtoMessage.GetEnum(const Idx: Integer): TpbProtoEnum;
begin
  Assert((Idx >= 0) and (Idx < Length(FEnums)));
  Result := FEnums[Idx];
end;

function TpbProtoMessage.GetEnumByName(const AName: RawByteString): TpbProtoEnum;
var
  I : Integer;
  E : TpbProtoEnum;
begin
  for I := 0 to GetEnumCount - 1 do
    begin
      E := GetEnum(I);
      if E.FName = AName then
        begin
          Result := E;
          exit;
        end;
    end;
  Result := nil;
end;

procedure TpbProtoMessage.AddMessage(const M: TpbProtoMessage);
var L : Integer;
begin
  L := Length(FMessages);
  SetLength(FMessages, L + 1);
  FMessages[L] := M;
end;

function TpbProtoMessage.GetMessageCount: Integer;
begin
  Result := Length(FMessages);
end;

function TpbProtoMessage.GetMessage(const Idx: Integer): TpbProtoMessage;
begin
  Assert((Idx >= 0) and (Idx < Length(FMessages)));
  Result := FMessages[Idx];
end;

function TpbProtoMessage.GetMessageByName(const AName: RawByteString): TpbProtoMessage;
var
  I : Integer;
  M : TpbProtoMessage;
begin
  for I := 0 to GetMessageCount - 1 do
    begin
      M := GetMessage(I);
      if M.FName = AName then
        begin
          Result := M;
          exit;
        end;
    end;
  Result := nil;
end;

function TpbProtoMessage.ResolveValue(const AIdentifier: RawByteString; const AChildrenOnly: Boolean): TpbProtoNode;
var
  I : Integer;
  E : TpbProtoEnum;
  M : TpbProtoMessage;
  N, S : RawByteString;
begin
  for I := 0 to GetEnumCount - 1 do
    begin
      E := GetEnum(I);
      Result := E.ResolveValue(AIdentifier, True);
      if Assigned(Result) then
        exit;
    end;
  pbSplitIdentifier(AIdentifier, N, S);
  M := GetMessageByName(N);
  if Assigned(M) then
    begin
      if S = '' then
        raise EpbProtoNode.CreateFmt(SErr_IdentifierNotAValue, [AIdentifier]);
      Result := M.ResolveValue(S, True);
      if Assigned(Result) then
        exit
      else
        raise EpbProtoNode.CreateFmt(SErr_IdentifierNotDefined, [AIdentifier]);
    end;
  if AChildrenOnly then
    Result := nil
  else
    Result := FParentNode.ResolveValue(AIdentifier, False);
end;

function TpbProtoMessage.ResolveType(const AIdentifier: RawByteString; const AChildrenOnly: Boolean): TpbProtoNode;
var
  M : TpbProtoMessage;
  E : TpbProtoEnum;
  N, S : RawByteString;
begin
  pbSplitIdentifier(AIdentifier, N, S);
  M := GetMessageByName(N);
  if Assigned(M) then
    begin
      if S = '' then
        Result := M
      else
        begin
          Result := M.ResolveType(S, True);
          if not Assigned(Result) then
            raise EpbProtoNode.CreateFmt(SErr_IdentifierNotDefined, [AIdentifier]);
        end;
      exit;
    end;
  E := GetEnumByName(AIdentifier);
  if Assigned(E) then
    begin
      Result := E;
      exit;
    end;
  if AChildrenOnly then
    Result := nil
  else
    Result := FParentNode.ResolveType(AIdentifier, False);
end;

function TpbProtoMessage.GetAsProtoString: RawByteString;
var
  S : RawByteString;
  I : Integer;
begin
  S := 'message ' + FName + ' {' + pbCRLF;
  for I := 0 to GetMessageCount - 1 do
    S := S + GetMessage(I).GetAsProtoString;
  for I := 0 to GetEnumCount - 1 do
    S := S + GetEnum(I).GetAsProtoString;
  for I := 0 to GetFieldCount - 1 do
    S := S + GetField(I).GetAsProtoString;
  S := S + '}' + pbCRLF;
  Result := S;
end;



{ TpbProtoOption }

constructor TpbProtoOption.Create(const AFactory: TpbProtoNodeFactory);
begin
  Assert(Assigned(AFactory));
  inherited Create;
  FValue := AFactory.CreateLiteral(self);
end;

destructor TpbProtoOption.Destroy;
begin
  FreeAndNil(FValue);
  inherited Destroy;
end;

procedure TpbProtoOption.SetValue(const Value: TpbProtoLiteral);
begin
  FreeAndNil(FValue);
  FValue := Value;
end;



{ TpbProtoPackage }

constructor TpbProtoPackage.Create;
begin
  inherited Create;
end;

destructor TpbProtoPackage.Destroy;
var I : Integer;
begin
  for I := Length(FImportedPackages) - 1 downto 0 do
    FreeAndNil(FImportedPackages[I]);
  for I := Length(FOptions) - 1 downto 0 do
    FreeAndNil(FOptions[I]);
  for I := Length(FMessages) - 1 downto 0 do
    FreeAndNil(FMessages[I]);
  inherited Destroy;
end;

procedure TpbProtoPackage.AddMessage(const M: TpbProtoMessage);
var
  L : Integer;
begin
  L := Length(FMessages);
  SetLength(FMessages, L + 1);
  FMessages[L] := M;
end;

function TpbProtoPackage.GetMessageCount: Integer;
begin
  Result := Length(FMessages);
end;

function TpbProtoPackage.GetMessage(const Idx: Integer): TpbProtoMessage;
begin
  Assert((Idx >= 0) and (Idx < Length(FMessages)));
  Result := FMessages[Idx];
end;

function TpbProtoPackage.GetMessageByName(const AName: RawByteString): TpbProtoMessage;
var
  I : Integer;
  M : TpbProtoMessage;
begin
  for I := 0 to GetMessageCount - 1 do
    begin
      M := GetMessage(I);
      if M.FName = AName then
        begin
          Result := M;
          exit;
        end;
    end;
  Result := nil;
end;

procedure TpbProtoPackage.AddImport(const Path: RawByteString);
var
  L : Integer;
begin
  L := Length(FImports);
  SetLength(FImports, L + 1);
  FImports[L] := Path;
end;

function TpbProtoPackage.GetImportCount: Integer;
begin
  Result := Length(FImports);
end;

function TpbProtoPackage.GetImport(const Idx: Integer): RawByteString;
begin
  Assert((Idx >= 0) and (Idx < Length(FImports)));
  Result := FImports[Idx];
end;

procedure TpbProtoPackage.AddImportedPackage(const Package: TpbProtoPackage);
var
  L : Integer;
begin
  L := Length(FImportedPackages);
  SetLength(FImportedPackages, L + 1);
  FImportedPackages[L] := Package;
end;

function TpbProtoPackage.GetImportedPackageCount: Integer;
begin
  Result := Length(FImportedPackages);
end;

function TpbProtoPackage.GetImportedPackage(const Idx: Integer): TpbProtoPackage;
begin
  Assert((Idx >= 0) and (Idx < Length(FImportedPackages)));
  Result := FImportedPackages[Idx];
end;

function TpbProtoPackage.GetImportedPackageByName(const AName: RawByteString): TpbProtoPackage;
var
  I : Integer;
  P : TpbProtoPackage;
begin
  for I := 0 to GetImportedPackageCount - 1 do
    begin
      P := GetImportedPackage(I);
      if P.FName = AName then
        begin
          Result := P;
          exit;
        end;
    end;
  Result := nil;
end;

procedure TpbProtoPackage.AddOption(const Option: TpbProtoOption);
var
  L : Integer;
begin
  L := Length(FOptions);
  SetLength(FOptions, L + 1);
  FOptions[L] := Option;
end;

function TpbProtoPackage.GetOptionCount: Integer;
begin
  Result := Length(FOptions);
end;

function TpbProtoPackage.GetOption(const Idx: Integer): TpbProtoOption;
begin
  Assert((Idx >= 0) and (Idx < Length(FOptions)));
  Result := FOptions[Idx];
end;

procedure TpbProtoPackage.AddEnum(const E: TpbProtoEnum);
var L : Integer;
begin
  L := Length(FEnums);
  SetLength(FEnums, L + 1);
  FEnums[L] := E;
end;

function TpbProtoPackage.GetEnumCount: Integer;
begin
  Result := Length(FEnums);
end;

function TpbProtoPackage.GetEnum(const Idx: Integer): TpbProtoEnum;
begin
  Assert((Idx >= 0) and (Idx < Length(FEnums)));
  Result := FEnums[Idx];
end;

function TpbProtoPackage.GetEnumByName(const AName: RawByteString): TpbProtoEnum;
var
  I : Integer;
  E : TpbProtoEnum;
begin
  for I := 0 to GetEnumCount - 1 do
    begin
      E := GetEnum(I);
      if E.FName = AName then
        begin
          Result := E;
          exit;
        end;
    end;
  Result := nil;
end;

function TpbProtoPackage.ResolveValue(const AIdentifier: RawByteString; const AChildrenOnly: Boolean): TpbProtoNode;
var
  I : Integer;
  E : TpbProtoEnum;
  M : TpbProtoMessage;
  P : TpbProtoPackage;
  N, S : RawByteString;
begin
  for I := 0 to GetEnumCount - 1 do
    begin
      E := GetEnum(I);
      Result := E.ResolveValue(AIdentifier, True);
      if Assigned(Result) then
        exit;
    end;
  pbSplitIdentifier(AIdentifier, N, S);
  M := GetMessageByName(N);
  if Assigned(M) then
    begin
      if S = '' then
        raise EpbProtoNode.CreateFmt(SErr_IdentifierNotAValue, [AIdentifier]);
      Result := M.ResolveValue(S, True);
      if Assigned(Result) then
        exit
      else
        raise EpbProtoNode.CreateFmt(SErr_IdentifierNotDefined, [AIdentifier]);
    end;
  for I := 0 to GetMessageCount - 1 do
    begin
      M := GetMessage(I);
      Result := M.ResolveValue(AIdentifier, True);
      if Assigned(Result) then
        exit;
    end;
  P := GetImportedPackageByName(N);
  if Assigned(P) then
    begin
      if S = '' then
        raise EpbProtoNode.CreateFmt(SErr_IdentifierNotAValue, [AIdentifier]);
      Result := P.ResolveValue(S, True);
      if Assigned(Result) then
        exit
      else
        raise EpbProtoNode.CreateFmt(SErr_IdentifierNotDefined, [AIdentifier]);
    end;
  for I := 0 to GetImportedPackageCount - 1 do
    begin
      P := GetImportedPackage(I);
      Result := P.ResolveValue(AIdentifier, True);
      if Assigned(Result) then
        exit;
    end;
  Result := nil;
end;

function TpbProtoPackage.ResolveType(const AIdentifier: RawByteString; const AChildrenOnly: Boolean): TpbProtoNode;
var
  M : TpbProtoMessage;
  E : TpbProtoEnum;
  P : TpbProtoPackage;
  N, S : RawByteString;
  I : Integer;
begin
  pbSplitIdentifier(AIdentifier, N, S);
  M := GetMessageByName(N);
  if Assigned(M) then
    begin
      if S = '' then
        Result := M
      else
        begin
          Result := M.ResolveType(S, True);
          if not Assigned(Result) then
            raise EpbProtoNode.CreateFmt(SErr_IdentifierNotDefined, [AIdentifier]);
        end;
      exit;
    end;
  E := GetEnumByName(AIdentifier);
  if Assigned(E) then
    begin
      Result := E;
      exit;
    end;
  P := GetImportedPackageByName(N);
  if Assigned(P) then
    begin
      if S = '' then
        Result := P
      else
        begin
          Result := P.ResolveType(S, True);
          if not Assigned(Result) then
            raise EpbProtoNode.CreateFmt(SErr_IdentifierNotDefined, [AIdentifier]);
        end;
      exit;
    end;
  for I := 0 to GetImportedPackageCount - 1 do
    begin
      Result := GetImportedPackage(I).ResolveType(AIdentifier, True);
      if Assigned(Result) then
        exit;
    end;
  Result := nil;
end;

function TpbProtoPackage.GetAsProtoString: RawByteString;
var
  I : Integer;
  S : RawByteString;
begin
  S := '';
  if FName <> '' then
    S := S + 'package ' + FName + ';' + pbCRLF + pbCRLF;
  for I := 0 to GetMessageCount - 1 do
    S := S + GetMessage(I).GetAsProtoString;
  Result := S;
end;



{ TpbProtoNodeFactory }

function TpbProtoNodeFactory.CreatePackage: TpbProtoPackage;
begin
  Result := TpbProtoPackage.Create;
end;

function TpbProtoNodeFactory.CreateMessage(const AParentNode: TpbProtoNode): TpbProtoMessage;
begin
  Result := TpbProtoMessage.Create(AParentNode);
end;

function TpbProtoNodeFactory.CreateField(const AParentMessage: TpbProtoMessage): TpbProtoField;
begin
  Result := TpbProtoField.Create(AParentMessage, self);
end;

function TpbProtoNodeFactory.CreateFieldType(const AParentField: TpbProtoField): TpbProtoFieldType;
begin
  Result := TpbProtoFieldType.Create(AParentField);
end;

function TpbProtoNodeFactory.CreateLiteral(const AParentNode: TpbProtoNode): TpbProtoLiteral;
begin
  Result := TpbProtoLiteral.Create(AParentNode);
end;

function TpbProtoNodeFactory.CreateEnum(const AParentNode: TpbProtoNode): TpbProtoEnum;
begin
  Result := TpbProtoEnum.Create(AParentNode);
end;

function TpbProtoNodeFactory.CreateEnumValue(const AParentEnum: TpbProtoEnum): TpbProtoEnumValue;
begin
  Result := TpbProtoEnumValue.Create(AParentEnum);
end;



{ Default ProtoNode factory }

var
  DefaultProtoNodeFactory: TpbProtoNodeFactory = nil;

function GetDefaultProtoNodeFactory: TpbProtoNodeFactory;
begin
  if not Assigned(DefaultProtoNodeFactory) then
    DefaultProtoNodeFactory := TpbProtoNodeFactory.Create;
  Result := DefaultProtoNodeFactory;
end;



initialization
finalization
  FreeAndNil(DefaultProtoNodeFactory);
end.

