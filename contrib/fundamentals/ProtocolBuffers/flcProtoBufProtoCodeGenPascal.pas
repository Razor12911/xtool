{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals 5.00                                        }
{   File name:        flcProtoBufProtoCodeGenPascal.pas                        }
{   File version:     5.06                                                     }
{   Description:      Protocol Buffer code generator for Pascal.               }
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
{   2012/04/15  0.01  Initial version: Framework                               }
{   2012/04/16  0.02  Generates unit with record definitions.                  }
{   2012/04/17  0.03  Refactoring.                                             }
{   2012/04/26  0.04  Imports.                                                 }
{   2013/04/17  0.05  Fix with enumeration code generation.                    }
{   2016/01/15  5.06  Revised for Fundamentals 5.                              }
{                                                                              }
{******************************************************************************}

{$INCLUDE flcProtoBuf.inc}

unit flcProtoBufProtoCodeGenPascal;

interface

uses
  { Fundamentals }
  flcStdTypes,
  flcUtils,
  flcDynArrays,
  flcStrings,
  flcStringBuilder,
  flcProtoBufProtoNodes;



type
  { CodeGenPascal }

  TCodeGenSupportVersion = (cgsvLessXE, cgsvXE, cgsvAll);

  TCodeGenPascalUnitUsesList = class
  protected
    FList : RawByteStringArray;

  public
    procedure Add(const Name: RawByteString);
    function  GetAsPascal: RawByteString;
  end;

  TCodeGenPascalIntfDefinitions = class
  protected
    FList : RawByteStringArray;

  public
    function  HasDef(const Name: RawByteString): Boolean;
    function  Add(const Name: RawByteString): Boolean;
  end;

  TCodeGenPascalUnitSection = class(TRawByteStringBuilder)
  end;

  TCodeGenPascalUnit = class
  protected
    FName         : RawByteString;
    FUnitComments : RawByteString;
    FIntfUsesList : TCodeGenPascalUnitUsesList;
    FIntfSection  : TCodeGenPascalUnitSection;
    FIntfDefs     : TCodeGenPascalIntfDefinitions;
    FImplUsesList : TCodeGenPascalUnitUsesList;
    FImplSection  : TCodeGenPascalUnitSection;

  public
    constructor Create;
    destructor Destroy; override;

    property  Name: RawByteString read FName write FName;
    property  UnitComments: RawByteString read FUnitComments write FUnitComments;

    property  Intf: TCodeGenPascalUnitSection read FIntfSection;
    property  IntfUses: TCodeGenPascalUnitUsesList read FIntfUsesList;
    property  IntfDefs: TCodeGenPascalIntfDefinitions read FIntfDefs;

    property  Impl: TCodeGenPascalUnitSection read FImplSection;
    property  ImplUses: TCodeGenPascalUnitUsesList read FImplUsesList;

    function  GetAsPascal: RawByteString;
    procedure Save(const Path: String);
  end;



  { ProtoPascal }

  TpbProtoPascalPackage = class; // forward
  TpbProtoPascalMessage = class; // forward
  TpbProtoPascalField = class; // forward
  TpbProtoPascalFieldType = class; // forward
  TpbProtoPascalEnum = class; // forward;



  { TpbProtoPascalEnumValue }

  TpbProtoPascalEnumValue = class(TpbProtoEnumValue)
  protected
    FPascalProtoName : RawByteString;
    FPascalName      : RawByteString;

    function GetPascalParentEnum: TpbProtoPascalEnum;

  public
    procedure CodeGenInit;
    function GetPascalDeclaration: RawByteString;
  end;



  { TpbProtoPascalEnum }

  TpbProtoPascalEnum = class(TpbProtoEnum)
  protected
    FPascalProtoName : RawByteString;
    FPascalName : RawByteString;
    FPascalEnumValuePrefix : RawByteString;

    function  GetPascalValue(const Idx: Integer): TpbProtoPascalEnumValue;
    procedure GenerateDeclaration(const AUnit: TCodeGenPascalUnit);
    procedure GenerateHelpers(const AUnit: TCodeGenPascalUnit);
    function  GetPascalZeroValueName: RawByteString;

  public
    procedure CodeGenInit;
    procedure GenerateMessageUnit(const AUnit: TCodeGenPascalUnit);
  end;



  { TpbProtoPascalLiteral }

  TpbProtoPascalLiteral = class(TpbProtoLiteral)
  protected
  public
    procedure CodeGenInit;
    function  GetPascalValueStr: RawByteString;
  end;



  { TpbProtoPascalFieldType }

  TpbProtoPascalFieldBaseKind = (
      bkNone,
      bkEnum,
      bkMsg,
      bkSimple
      );

  TpbProtoPascalFieldBaseType = class
  protected
    FParentFieldType : TpbProtoPascalFieldType;
    FBaseKind        : TpbProtoPascalFieldBaseKind;
    FEnum            : TpbProtoPascalEnum;
    FMsg             : TpbProtoPascalMessage;

    FPascalTypeStr      : RawByteString;
    FPascalProtoStr     : RawByteString;
    FPascalZeroValueStr : RawByteString;

  public
    constructor Create(const AParentFieldType: TpbProtoPascalFieldType);

    procedure CodeGenInit;

    function  GetPascalEncodeFieldCall(const ParBuf, ParBufSize, ParTagID, ParValue: RawByteString): RawByteString;
    function  GetPascalEncodeValueCall(const ParBuf, ParBufSize, ParValue: RawByteString): RawByteString;
    function  GetPascalDecodeFieldCall(const ParField, ParValue: RawByteString): RawByteString;
    function  GetPascalDecodeValueCall(const ParBuf, ParBufSize, ParValue: RawByteString): RawByteString;
    function  GetPascalInitInstanceCall(const ParInstance: RawByteString): RawByteString;
  end;

  TpbProtoPascalFieldType = class(TpbProtoFieldType)
  protected
    FIsArray : Boolean;

    FPascalBaseType : TpbProtoPascalFieldBaseType;

    FPascalTypeStr         : RawByteString;
    FPascalProtoStr        : RawByteString;
    FPascalZeroValueStr    : RawByteString;
    FPascalDefaultValueStr : RawByteString;

    FPascalArrayEncodeFuncName : RawByteString;
    FPascalArrayDecodeFuncName : RawByteString;

    FPascalEncodeFuncName : RawByteString;
    FPascalDecodeFuncName : RawByteString;

    function  GetPascalParentField: TpbProtoPascalField;
    procedure GenerateArrayHelpers(const AUnit: TCodeGenPascalUnit; const PasVersion: TCodeGenSupportVersion);

  public
    constructor Create(const AParentField: TpbProtoField);
    destructor Destroy; override;

    procedure CodeGenInit;
    procedure GenerateMessageUnit(const AUnit: TCodeGenPascalUnit; const PasVersion: TCodeGenSupportVersion);
  end;



  { TpbProtoPascalField }

  TpbProtoPascalField = class(TpbProtoField)
  protected
    FPascalProtoName    : RawByteString;
    FPascalName         : RawByteString;
    FPascalHasValueName : RawByteString;

    FPascalRecordDefinition : RawByteString;
    FPascalRecordHasValueDefinition : RawByteString;

    FPascalRecordInitStatement : RawByteString;
    FPascalRecordInitHasValueStatement : RawByteString;

    FPascalRecordFinaliseStatement : RawByteString;

    function  GetPascalFieldType: TpbProtoPascalFieldType;
    function  GetPascalParentMessage: TpbProtoPascalMessage;
    function  GetPascalDefaultValue: TpbProtoPascalLiteral;

    function  IsArray: Boolean;

    function  GetPascalEncodeFieldTypeCall(const ParBuf, ParBufSize, ParValue: RawByteString): RawByteString;
    function  GetPascalDecodeFieldTypeCall(const ParField, ParValue: RawByteString; const PascalFieldPrefix: RawByteString): RawByteString;

  public
    constructor Create(const AParentMessage: TpbProtoMessage; const AFactory: TpbProtoNodeFactory);
    destructor Destroy; override;

    procedure CodeGenInit;
    procedure GenerateMessageUnit(const AUnit: TCodeGenPascalUnit; const PasVersion: TCodeGenSupportVersion);
  end;



  { TpbProtoPascalMessage }

  TpbProtoPascalMessage = class(TpbProtoMessage)
  protected
    FPascalProtoName : RawByteString;
    FPascalName      : RawByteString;

    function  GetPascalPackage: TpbProtoPascalPackage;
    function  GetPascalField(const Idx: Integer): TpbProtoPascalField;
    function  GetPascalEnum(const Idx: Integer): TpbProtoPascalEnum;
    function  GetPascalMessage(const Idx: Integer): TpbProtoPascalMessage;

    procedure GenerateRecordDeclaration(const AUnit: TCodeGenPascalUnit; const PasVersion: TCodeGenSupportVersion);
    procedure GenerateRecordInitProc(const AUnit: TCodeGenPascalUnit; const PasVersion: TCodeGenSupportVersion);
    procedure GenerateRecordEncodeProc(const AUnit: TCodeGenPascalUnit; const PasVersion: TCodeGenSupportVersion);
    procedure GenerateRecordDecodeProc(const AUnit: TCodeGenPascalUnit; const PasVersion: TCodeGenSupportVersion);

  public
    constructor Create(const AParentNode: TpbProtoNode);
    destructor Destroy; override;

    procedure CodeGenInit;
    procedure GenerateMessageUnit(const AUnit: TCodeGenPascalUnit; const PasVersion: TCodeGenSupportVersion);
  end;



  { TpbProtoPascalPackage }

  TpbProtoPascalPackage = class(TpbProtoPackage)
  protected
    FPascalProtoName : RawByteString;
    FPascalBaseName  : RawByteString;
    FMessageUnit     : TCodeGenPascalUnit;

    function GetPascalMessage(const Idx: Integer): TpbProtoPascalMessage;
    function GetPascalEnum(const Idx: Integer): TpbProtoPascalEnum;
    function GetPascalImportedPackage(const Idx: Integer): TpbProtoPascalPackage;

  public
    constructor Create;
    destructor Destroy; override;

    property  MessageUnit: TCodeGenPascalUnit read FMessageUnit;

    procedure CodeGenInit;
    procedure GenerateMessageUnit(const PasVersion: TCodeGenSupportVersion);
    procedure Save(const OutputPath: String);
  end;



  { TpbProtoCodeGenPascal }

  TpbProtoCodeGenPascal = class
  protected
    FOutputPath : String;

  public
    constructor Create;
    destructor Destroy; override;

    property  OutputPath: String read FOutputPath write FOutputPath;
    procedure GenerateCode(const APackage: TpbProtoPackage; const PasVersion: TCodeGenSupportVersion);
  end;



  { TpbProtoPascalNodeFactory }

  TpbProtoPascalNodeFactory = class(TpbProtoNodeFactory)
  public
    function  CreatePackage: TpbProtoPackage; override;
    function  CreateMessage(const AParentNode: TpbProtoNode): TpbProtoMessage; override;
    function  CreateField(const AParentMessage: TpbProtoMessage): TpbProtoField; override;
    function  CreateFieldType(const AParentField: TpbProtoField): TpbProtoFieldType; override;
    function  CreateLiteral(const AParentNode: TpbProtoNode): TpbProtoLiteral; override;
    function  CreateEnum(const AParentNode: TpbProtoNode): TpbProtoEnum; override;
    function  CreateEnumValue(const AParentEnum: TpbProtoEnum): TpbProtoEnumValue; override;
  end;



{ GetPascalProtoNodeFactory }

function GetPascalProtoNodeFactory: TpbProtoPascalNodeFactory;



implementation

uses
  { System }
  SysUtils,
  Classes,

  { Fundamentals }
  flcFloats,
  flcASCII;



const
  CRLF = RawByteString(#13#10);



{ TCodeGenPascalUnitUsesList }

procedure TCodeGenPascalUnitUsesList.Add(const Name: RawByteString);
begin
  if DynArrayPosNextB(Name, FList) >= 0 then
    exit;
  DynArrayAppendB(FList, Name);
end;

function TCodeGenPascalUnitUsesList.GetAsPascal: RawByteString;
var L, I : Integer;
begin
  L := Length(FList);
  if L = 0 then
    begin
      Result := CRLF + CRLF;
      exit;
    end;
  Result :=
    'uses' + CRLF;
  for I := 0 to L - 1 do
    begin
      Result := Result + '  ' + FList[I];
      if I < L - 1 then
        Result := Result + ',' + CRLF;
    end;
  Result := Result + ';' + CRLF +
      CRLF +
      CRLF +
      CRLF;
end;



{ TCodeGenPascalIntfDefinitions }

function TCodeGenPascalIntfDefinitions.HasDef(const Name: RawByteString): Boolean;
begin
  Result := DynArrayPosNextB(Name, FList) >= 0;
end;

function TCodeGenPascalIntfDefinitions.Add(const Name: RawByteString): Boolean;
begin
  Result := DynArrayPosNextB(Name, FList) < 0;
  if not Result then
    exit;
  DynArrayAppendB(FList, Name);
end;



{ TCodeGenPascalUnit }

constructor TCodeGenPascalUnit.Create;
begin
  inherited Create;
  FIntfUsesList := TCodeGenPascalUnitUsesList.Create;
  FIntfSection := TCodeGenPascalUnitSection.Create;
  FIntfDefs := TCodeGenPascalIntfDefinitions.Create;
  FImplUsesList := TCodeGenPascalUnitUsesList.Create;
  FImplSection := TCodeGenPascalUnitSection.Create;
end;

destructor TCodeGenPascalUnit.Destroy;
begin
  FreeAndNil(FImplSection);
  FreeAndNil(FImplUsesList);
  FreeAndNil(FIntfDefs);
  FreeAndNil(FIntfSection);
  FreeAndNil(FIntfUsesList);
  inherited Destroy;
end;

function TCodeGenPascalUnit.GetAsPascal: RawByteString;
begin
  Result :=
      FUnitComments + iifB(FUnitComments <> '', CRLF, '') +
      'unit ' + FName + ';' + CRLF +
      CRLF +
      'interface' + CRLF +
      CRLF +
      FIntfUsesList.GetAsPascal +
      FIntfSection.AsRawByteString +
      'implementation' + CRLF +
      CRLF +
      FImplUsesList.GetAsPascal +
      FImplSection.AsRawByteString +
      'end.' + CRLF +
      CRLF;
end;

procedure TCodeGenPascalUnit.Save(const Path: String);
var
  FileName : String;
  FileData : RawByteString;
  FileStream : TFileStream;
begin
  FileName := Path + String(FName) + '.pas';
  FileData := GetAsPascal;
  FileStream := TFileStream.Create(FileName, fmCreate);
  try
    FileStream.WriteBuffer(Pointer(FileData)^, Length(FileData));
  finally
    FileStream.Free;
  end;
end;



{ ProtoPascal }

const
  ProtoFieldBaseTypeToPascalBaseTypeStr: array[TpbProtoFieldBaseType] of RawByteString = (
    '',
    'Double',
    'Single',
    'LongInt',
    'Int64',
    'LongWord',
    'UInt64',
    'LongInt',
    'Int64',
    'LongWord',
    'UInt64',
    'LongInt',
    'Int64',
    'Boolean',
    'RawByteString',
    'RawByteString',
    ''
  );

  ProtoFieldBaseTypeToPascalZeroValueStr: array[TpbProtoFieldBaseType] of RawByteString = (
    '',
    '0.0',
    '0.0',
    '0',
    '0',
    '0',
    '0',
    '0',
    '0',
    '0',
    '0',
    '0',
    '0',
    'False',
    '''''',
    '''''',
    ''
  );

  ProtoFieldTypeToPascalStr : array[TpbProtoFieldBaseType] of RawByteString = (
    '',
    'Double',
    'Float',
    'Int32',
    'Int64',
    'UInt32',
    'UInt64',
    'SInt32',
    'SInt64',
    'Fixed32',
    'Fixed64',
    'SFixed32',
    'SFixed64',
    'Bool',
    'String',
    'Bytes',
    ''
  );



// converts a name from the .proto file to a name that follows Pascal
// conventions, i.e. camel case, no underscores
function ProtoNameToPascalProtoName(const AName: RawByteString): RawByteString;
var S : RawByteString;
    I : Integer;
begin
  S := AName;
  // replace _xxx with _Xxx
  repeat
    I := PosStrB('_', S);
    if I > 0 then
      begin
        Delete(S, I, 1);
        if I <= Length(S) then
          S[I] := AsciiUpCaseB(S[I]);
      end;
  until I = 0;
  // first character upper case
  S := AsciiFirstUpB(S);
  // return Pascal name
  Result := S;
end;



{ TpbProtoPascalEnumValue }

function TpbProtoPascalEnumValue.GetPascalParentEnum: TpbProtoPascalEnum;
begin
  Result := FParentEnum as TpbProtoPascalEnum;
end;

procedure TpbProtoPascalEnumValue.CodeGenInit;
begin
  FPascalProtoName := ProtoNameToPascalProtoName(FName);
  FPascalName := GetPascalParentEnum.FPascalEnumValuePrefix + FPascalProtoName;
end;

function TpbProtoPascalEnumValue.GetPascalDeclaration: RawByteString;
begin
  Result := FPascalName + ' = ' + IntToStringB(FValue);
end;



{ TpbProtoPascalEnum }

function TpbProtoPascalEnum.GetPascalValue(const Idx: Integer): TpbProtoPascalEnumValue;
begin
  Result := GetValue(Idx) as TpbProtoPascalEnumValue;
end;

function TpbProtoPascalEnum.GetPascalZeroValueName: RawByteString;
begin
  if GetValueCount = 0 then
    Result := ''
  else
    Result := GetPascalValue(0).FPascalName;
end;

procedure TpbProtoPascalEnum.CodeGenInit;
var I : Integer;
begin
  FPascalProtoName := ProtoNameToPascalProtoName(FName);
  FPascalName := 'T' + FPascalProtoName;

  FPascalEnumValuePrefix := FName;
  AsciiConvertLowerB(FPascalEnumValuePrefix);

  for I := 0 to GetValueCount - 1 do
    GetPascalValue(I).CodeGenInit;
end;

procedure TpbProtoPascalEnum.GenerateDeclaration(const AUnit: TCodeGenPascalUnit);
var
  I, L : Integer;
begin
  with AUnit do
  begin
    Intf.AppendLn('{ ' + FPascalName + ' }');
    Intf.AppendLn;
    Intf.AppendLn('type');
    Intf.AppendLn('  ' + FPascalName + ' = (');
    L := GetValueCount;
    for I := 0 to L - 1 do
      begin
        Intf.Append('    ' + GetPascalValue(I).GetPascalDeclaration);
        if I < L - 1 then
          Intf.AppendCh(',');
        Intf.AppendLn;
      end;
    Intf.AppendLn('  );');
    Intf.AppendLn;
  end;
end;

procedure TpbProtoPascalEnum.GenerateHelpers(const AUnit: TCodeGenPascalUnit);
var
  Proto : RawByteString;
begin
  with AUnit do
    begin
      Impl.AppendLn('{ ' + FPascalName + ' }');
      Impl.AppendLn;

      Proto := 'function  pbEncodeValue' + FPascalProtoName + '(var Buf; const BufSize: Integer; const Value: ' + FPascalName + '): Integer;';
      Intf.AppendLn(Proto);
      Proto := 'function pbEncodeValue' + FPascalProtoName + '(var Buf; const BufSize: Integer; const Value: ' + FPascalName + '): Integer;';
      Impl.AppendLn(Proto);
      Impl.AppendLn('begin');
      Impl.AppendLn('  Result := pbEncodeValueInt32(Buf, BufSize, Ord(Value));');
      Impl.AppendLn('end;');
      Impl.AppendLn;

      Proto := 'function  pbEncodeField' + FPascalProtoName + '(var Buf; const BufSize: Integer; const FieldNum: Integer; const Value: ' + FPascalName + '): Integer;';
      Intf.AppendLn(Proto);
      Proto := 'function pbEncodeField' + FPascalProtoName + '(var Buf; const BufSize: Integer; const FieldNum: Integer; const Value: ' + FPascalName + '): Integer;';
      Impl.AppendLn(Proto);
      Impl.AppendLn('begin');
      Impl.AppendLn('  Result := pbEncodeFieldInt32(Buf, BufSize, FieldNum, Ord(Value));');
      Impl.AppendLn('end;');
      Impl.AppendLn;

      Proto := 'function  pbDecodeValue' + FPascalProtoName + '(const Buf; const BufSize: Integer; var Value: ' + FPascalName + '): Integer;';
      Intf.AppendLn(Proto);
      Proto := 'function pbDecodeValue' + FPascalProtoName + '(const Buf; const BufSize: Integer; var Value: ' + FPascalName + '): Integer;';
      Impl.AppendLn(Proto);
      Impl.AppendLn('var I : LongInt;');
      Impl.AppendLn('begin');
      Impl.AppendLn('  Result := pbDecodeValueInt32(Buf, BufSize, I);');
      Impl.AppendLn('  Value := ' + FPascalName + '(I);');
      Impl.AppendLn('end;');
      Impl.AppendLn;

      Proto := 'procedure pbDecodeField' + FPascalProtoName + '(const Field: TpbProtoBufDecodeField; var Value: ' + FPascalName + ');';
      Intf.AppendLn(Proto);
      Impl.AppendLn(Proto);
      Impl.AppendLn('var I : LongInt;');
      Impl.AppendLn('begin');
      Impl.AppendLn('  pbDecodeFieldInt32(Field, I);');
      Impl.AppendLn('  Value := ' + FPascalName + '(I);');
      Impl.AppendLn('end;');
      Impl.AppendLn;
    end;
end;

procedure TpbProtoPascalEnum.GenerateMessageUnit(const AUnit: TCodeGenPascalUnit);
begin
  GenerateDeclaration(AUnit);
  GenerateHelpers(AUnit);
  AUnit.Intf.AppendLn;
  AUnit.Intf.AppendLn;
  AUnit.Intf.AppendLn;
  AUnit.Impl.AppendLn;
  AUnit.Impl.AppendLn;
end;



{ TpbProtoPascalLiteral }

procedure TpbProtoPascalLiteral.CodeGenInit;
begin
end;

function TpbProtoPascalLiteral.GetPascalValueStr: RawByteString;
var
  V : TpbProtoNode;
begin
  case FLiteralType of
    pltInteger : Result := IntToStringB(FLiteralInt);
    pltFloat   : Result := FloatToStringB(FLiteralFloat);
    pltString  : Result := StrQuoteB(FLiteralStr, '''');
    pltBoolean : Result := iifB(FLiteralBool, 'True', 'False');
    pltIdentifier :
      begin
        V := LiteralIdenValue;
        if V is TpbProtoPascalEnumValue then
          Result := TpbProtoPascalEnumValue(V).FPascalName
        else
          Result := '';
      end;
  else
    raise EpbProtoNode.Create('Literal type not supported');
  end;
end;



{ TpbProtoPascalFieldBaseType }

constructor TpbProtoPascalFieldBaseType.Create(const AParentFieldType: TpbProtoPascalFieldType);
begin
  inherited Create;
  FParentFieldType := AParentFieldType;
  FBaseKind := bkNone;
end;

procedure TpbProtoPascalFieldBaseType.CodeGenInit;
var T : TpbProtoNode;
    B : TpbProtoFieldBaseType;
begin
  if FParentFieldType.IsIdenType then
    begin
      T := FParentFieldType.IdenType;
      if T is TpbProtoPascalEnum then
        begin
          FBaseKind := bkEnum;
          FEnum := TpbProtoPascalEnum(T);
          FPascalTypeStr      := FEnum.FPascalName;
          FPascalProtoStr     := FEnum.FPascalProtoName;
          FPascalZeroValueStr := FEnum.GetPascalZeroValueName;
        end
      else
      if T is TpbProtoPascalMessage then
        begin
          FBaseKind := bkMsg;
          FMsg := TpbProtoPascalMessage(T);
          FPascalTypeStr      := FMsg.FPascalName;
          FPascalProtoStr     := FMsg.FPascalProtoName;
          FPascalZeroValueStr := '';
        end
      else
        raise EpbProtoNode.CreateFmt('Unresolved identifier: %s', [FParentFieldType.IdenStr]);
    end
  else
    begin
      FBaseKind := bkSimple;
      B := FParentFieldType.FBaseType;
      FPascalTypeStr      := ProtoFieldBaseTypeToPascalBaseTypeStr[B];
      FPascalProtoStr     := ProtoFieldTypeToPascalStr[B];
      FPascalZeroValueStr := ProtoFieldBaseTypeToPascalZeroValueStr[B];
    end;
end;

function TpbProtoPascalFieldBaseType.GetPascalEncodeFieldCall(const ParBuf, ParBufSize, ParTagID, ParValue: RawByteString): RawByteString;
begin
  case FBaseKind of
    bkSimple :
      Result := 'pbEncodeField' + FPascalProtoStr +
          '(' + ParBuf + ', ' + ParBufSize + ', ' + ParTagID + ', ' + ParValue + ')';
    bkEnum :
      Result := 'pbEncodeField' + FEnum.FPascalProtoName +
          '(' + ParBuf + ', ' + ParBufSize + ', ' + ParTagID + ', ' + ParValue + ')';
    bkMsg  :
      Result := 'pbEncodeField' + FMsg.FPascalProtoName +
          '(' + ParBuf + ', ' + ParBufSize + ', ' + ParTagID + ', ' + ParValue + ')';
  else
    Result := '';
  end;
end;

function TpbProtoPascalFieldBaseType.GetPascalEncodeValueCall(const ParBuf, ParBufSize, ParValue: RawByteString): RawByteString;
begin
  case FBaseKind of
    bkSimple :
      Result := 'pbEncodeValue' + FPascalProtoStr +
          '(' + ParBuf + ', ' + ParBufSize + ', ' + ParValue + ')';
    bkEnum :
      Result := 'pbEncodeValue' + FEnum.FPascalProtoName +
          '(' + ParBuf + ', ' + ParBufSize + ', ' + ParValue + ')';
    bkMsg  :
      Result := 'pbEncodeValue' + FMsg.FPascalProtoName +
          '(' + ParBuf + ', ' + ParBufSize + ', ' + ParValue + ')';
  else
    Result := '';
  end;
end;

function TpbProtoPascalFieldBaseType.GetPascalDecodeFieldCall(const ParField, ParValue: RawByteString): RawByteString;
begin
  case FBaseKind of
    bkSimple :
      Result := 'pbDecodeField' + FPascalProtoStr +
          '(' + ParField + ', ' + ParValue + ')';
    bkEnum :
      Result := 'pbDecodeField' + FEnum.FPascalProtoName +
          '(' + ParField + ', ' + ParValue + ')';
    bkMsg :
      Result := 'pbDecodeField' + FMsg.FPascalProtoName +
          '(' + ParField + ', ' + ParValue + ')';
  else
    Result := '';
  end;
end;

function TpbProtoPascalFieldBaseType.GetPascalDecodeValueCall(const ParBuf, ParBufSize, ParValue: RawByteString): RawByteString;
begin
  case FBaseKind of
    bkSimple :
      Result := 'pbDecodeValue' + FPascalProtoStr +
          '(' + ParBuf + ', ' + ParBufSize + ', ' + ParValue + ')';
    bkEnum :
      Result := 'pbDecodeValue' + FEnum.FPascalProtoName +
          '(' + ParBuf + ', ' + ParBufSize + ', ' + ParValue + ')';
    bkMsg :
      Result := 'pbDecodeValue' + FMsg.FPascalProtoName +
          '(' + ParBuf + ', ' + ParBufSize + ', ' + ParValue + ')';
  else
    Result := '';
  end;
end;

function TpbProtoPascalFieldBaseType.GetPascalInitInstanceCall(const ParInstance: RawByteString): RawByteString;
begin
  case FBaseKind of
    bkMsg : Result := FMsg.FPascalProtoName + 'Init(' + ParInstance + ')';
  else
    Result := '';
  end;
end;



{ TpbProtoPascalFieldType }

constructor TpbProtoPascalFieldType.Create(const AParentField: TpbProtoField);
begin
  inherited Create(AParentField);
  FPascalBaseType := TpbProtoPascalFieldBaseType.Create(self);
end;

destructor TpbProtoPascalFieldType.Destroy;
begin
  FreeAndNil(FPascalBaseType);
  inherited Destroy;
end;

function TpbProtoPascalFieldType.GetPascalParentField: TpbProtoPascalField;
begin
  Result := FParentField as TpbProtoPascalField;
end;

procedure TpbProtoPascalFieldType.CodeGenInit;
begin
  FPascalBaseType.CodeGenInit;

  FIsArray := FParentField.Cardinality = pfcRepeated;
  if FIsArray then
    begin
      FPascalProtoStr := 'DynArray' + FPascalBaseType.FPascalProtoStr;
      FPascalTypeStr := 'T' + FPascalProtoStr;
      FPascalZeroValueStr := 'nil';
      FPascalDefaultValueStr := 'nil';

      FPascalArrayEncodeFuncName := 'pbEncodeField' + FPascalProtoStr;
      FPascalArrayDecodeFuncName := 'pbDecodeField' + FPascalProtoStr;

      if FParentField.OptionPacked then
        begin
          FPascalEncodeFuncName := FPascalArrayEncodeFuncName + '_Packed';
          FPascalDecodeFuncName := FPascalArrayDecodeFuncName + '_Packed';
        end
      else
        begin
          FPascalEncodeFuncName := FPascalArrayEncodeFuncName;
          FPascalDecodeFuncName := FPascalArrayDecodeFuncName;
        end;
    end
  else
    begin
      FPascalTypeStr := FPascalBaseType.FPascalTypeStr;
      FPascalZeroValueStr := FPascalBaseType.FPascalZeroValueStr;
      if FParentField.DefaultValue.LiteralType = pltNone then
        FPascalDefaultValueStr := FPascalZeroValueStr
      else
        FPascalDefaultValueStr := GetPascalParentField.GetPascalDefaultValue.GetPascalValueStr;

      FPascalArrayEncodeFuncName := '';
      FPascalArrayDecodeFuncName := '';
      FPascalEncodeFuncName := '';
    end;
end;

procedure TpbProtoPascalFieldType.GenerateArrayHelpers(const AUnit: TCodeGenPascalUnit;
  const PasVersion: TCodeGenSupportVersion);
var
  Proto : RawByteString;
  CommentLine : RawByteString;
  S : RawByteString;
begin
  with AUnit do
    if IntfDefs.Add(FPascalTypeStr) then
    begin
      CommentLine := '{ ' + FPascalTypeStr + ' }';

      Intf.AppendLn(CommentLine);
      Intf.AppendLn;

      Impl.AppendLn(CommentLine);
      Impl.AppendLn;

      Intf.AppendLn('type');
      Intf.AppendLn('  ' + FPascalTypeStr + ' = array of ' + FPascalBaseType.FPascalTypeStr + ';');
      Intf.AppendLn;

      if PasVersion = cgsvAll then
        Intf.AppendLn('{$IFDEF VER_XE}');

      if PasVersion in [cgsvXE, cgsvAll] then
      begin
        Intf.AppendLn('  ' + FPascalTypeStr + 'Helper = record helper for ' + FPascalTypeStr);
        Intf.AppendLn('  public');
        Proto := '    function EncodeField(var Buf; const BufSize: Integer; const FieldNum: Integer): Integer;';
        Intf.AppendLn(Proto);

        Proto := '    function EncodeField_Packed(var Buf; const BufSize: Integer; const FieldNum: Integer): Integer;';
        Intf.AppendLn(Proto);

        Proto := '    procedure DecodeField(const Field: TpbProtoBufDecodeField);';
        Intf.AppendLn(Proto);

        Proto := '    procedure DecodeField_Packed(const Field: TpbProtoBufDecodeField);';
        Intf.AppendLn(Proto);
        Intf.AppendLn('  end;');
      end;

      if PasVersion = cgsvAll then
        Intf.AppendLn('{$ELSE}');

      if PasVersion in [cgsvLessXE, cgsvAll] then
      begin
        Proto :=
            'function ' + FPascalArrayEncodeFuncName +
                '(var Buf; const BufSize: Integer; const FieldNum: Integer; const Value: ' + FPascalTypeStr + '): Integer;';
        Intf.AppendLn(Proto);

        Proto :=
            'function ' + FPascalArrayEncodeFuncName + '_Packed' +
                '(var Buf; const BufSize: Integer; const FieldNum: Integer; const Value: ' + FPascalTypeStr + '): Integer;';
        Intf.AppendLn(Proto);

        Proto :=
            'procedure ' + FPascalArrayDecodeFuncName +
                '(const Field: TpbProtoBufDecodeField; var Value: ' + FPascalTypeStr + ');';
        Intf.AppendLn(Proto);

        Proto :=
            'procedure ' + FPascalArrayDecodeFuncName + '_Packed' +
                '(const Field: TpbProtoBufDecodeField; var Value: ' + FPascalTypeStr + ');';
        Intf.AppendLn(Proto);
      end;

      if PasVersion = cgsvAll then
        Intf.AppendLn('{$ENDIF}');


      if PasVersion = cgsvAll then
        Impl.AppendLn('{$IFDEF VER_XE}');

      if PasVersion in [cgsvXE, cgsvAll] then
      begin
        Proto :=
            'function ' + FPascalTypeStr + 'Helper.EncodeField' +
                '(var Buf; const BufSize: Integer; const FieldNum: Integer): Integer;';
        Impl.AppendLn(Proto);
      end;

      if PasVersion = cgsvAll then
        Impl.AppendLn('{$ELSE}');

      if PasVersion in [cgsvLessXE, cgsvAll] then
      begin
        Proto :=
            'function ' + FPascalArrayEncodeFuncName +
                '(var Buf; const BufSize: Integer; const FieldNum: Integer; const Value: ' + FPascalTypeStr + '): Integer;';
        Impl.AppendLn(Proto);
      end;

      if PasVersion = cgsvAll then
        Impl.AppendLn('{$ENDIF}');


      Impl.AppendLn('var');
      Impl.AppendLn('  P : PByte;');
      Impl.AppendLn('  I, L, N : Integer;');
      Impl.AppendLn('begin');
      Impl.AppendLn('  P := @Buf;');
      Impl.AppendLn('  L := BufSize;');

      case PasVersion of
        cgsvLessXE:  Impl.AppendLn('  for I := 0 to Length(Value) - 1 do');
        cgsvXE:      Impl.AppendLn('  for I := 0 to Length(Self) - 1 do');
        cgsvAll:     Impl.AppendLn('  for I := 0 to Length({$IFDEF VER_XE}Self{$ELSE}Value{$ENDIF}) - 1 do');
      end;

      Impl.AppendLn('    begin');

      if PasVersion = cgsvAll then
        Impl.AppendLn('{$IFDEF VER_XE}');

      case PasVersion of
        cgsvXE:      Impl.AppendLn('      N := Self[I].EncodeField(P^, L, FieldNum);');
        cgsvAll:     Impl.AppendLn('      N := {$IFDEF VER_XE}Self[I]{$ELSE}Value[I]{$ENDIF}.EncodeField(P^, L, FieldNum);');
      end;

      if PasVersion = cgsvAll then
        Impl.AppendLn('{$ELSE}');

      if PasVersion in [cgsvLessXE, cgsvAll] then
        Impl.AppendLn('      N := ' + FPascalBaseType.GetPascalEncodeFieldCall('P^', 'L', 'FieldNum', 'Value[I]') + ';');

      if PasVersion = cgsvAll then
        Impl.AppendLn('{$ENDIF}');


      Impl.AppendLn('      Inc(P, N);');
      Impl.AppendLn('      Dec(L, N);');
      Impl.AppendLn('    end;');
      Impl.AppendLn('  Result := BufSize - L;');
      Impl.AppendLn('end;');
      Impl.AppendLn;


      if PasVersion = cgsvAll then
        Impl.AppendLn('{$IFDEF VER_XE}');

      if PasVersion in [cgsvXE, cgsvAll] then
      begin
        Proto :=
            'function ' + FPascalTypeStr + 'Helper.EncodeField_Packed' +
                '(var Buf; const BufSize: Integer; const FieldNum: Integer): Integer;';
        Impl.AppendLn(Proto);
      end;

      if PasVersion = cgsvAll then
        Impl.AppendLn('{$ELSE}');

      if PasVersion in [cgsvLessXE, cgsvAll] then
      begin
        Proto :=
            'function ' + FPascalArrayEncodeFuncName + '_Packed' +
                '(var Buf; const BufSize: Integer; const FieldNum: Integer; const Value: ' + FPascalTypeStr + '): Integer;';
        Impl.AppendLn(Proto);
      end;

      if PasVersion = cgsvAll then
        Impl.AppendLn('{$ENDIF}');

      Impl.AppendLn('var');
      Impl.AppendLn('  P : PByte;');
      Impl.AppendLn('  I, T, L, N : Integer;');
      Impl.AppendLn('begin');
      Impl.AppendLn('  P := @Buf;');
      Impl.AppendLn('  T := 0;');

      case PasVersion of
        cgsvLessXE:  Impl.AppendLn('  for I := 0 to Length(Value) - 1 do');
        cgsvXE:      Impl.AppendLn('  for I := 0 to Length(Self) - 1 do');
        cgsvAll:     Impl.AppendLn('  for I := 0 to Length({$IFDEF VER_XE}Self{$ELSE}Value{$ENDIF}) - 1 do');
      end;

      Impl.AppendLn('  begin');

      if PasVersion = cgsvAll then
        Impl.AppendLn('{$IFDEF VER_XE}');

      if PasVersion in [cgsvXE, cgsvAll] then
        Impl.AppendLn('    Inc(T, Self[I].EncodeValue(P^, 0));');

      if PasVersion = cgsvAll then
        Impl.AppendLn('{$ELSE}');

      if PasVersion in [cgsvLessXE, cgsvAll] then
        Impl.AppendLn('    Inc(T, ' + FPascalBaseType.GetPascalEncodeValueCall('P^', '0', 'Value[I]') + ');');

      if PasVersion = cgsvAll then
        Impl.AppendLn('{$ENDIF}');

      Impl.AppendLn('  end;');

      Impl.AppendLn('  L := BufSize;');
      Impl.AppendLn('  N := pbEncodeFieldVarBytesHdr(P^, L, FieldNum, T);');
      Impl.AppendLn('  Inc(P, N);');
      Impl.AppendLn('  Dec(L, N);');

      case PasVersion of
        cgsvLessXE:  Impl.AppendLn('  for I := 0 to Length(Value) - 1 do');
        cgsvXE:      Impl.AppendLn('  for I := 0 to Length(Self) - 1 do');
        cgsvAll:     Impl.AppendLn('  for I := 0 to Length({$IFDEF VER_XE}Self{$ELSE}Value{$ENDIF}) - 1 do');
      end;

      Impl.AppendLn('    begin');

      if PasVersion = cgsvAll then
        Impl.AppendLn('{$IFDEF VER_XE}');

      if PasVersion in [cgsvXE, cgsvAll] then
        Impl.AppendLn('      N := Self[I].EncodeValue(P^, L);');

      if PasVersion = cgsvAll then
        Impl.AppendLn('{$ELSE}');

      if PasVersion in [cgsvLessXE, cgsvAll] then
        Impl.AppendLn('      N := ' + FPascalBaseType.GetPascalEncodeValueCall('P^', 'L', 'Value[I]') + ';');

      if PasVersion = cgsvAll then
        Impl.AppendLn('{$ENDIF}');


      Impl.AppendLn('      Inc(P, N);');
      Impl.AppendLn('      Dec(L, N);');
      Impl.AppendLn('    end;');
      Impl.AppendLn('  Result := BufSize - L;');
      Impl.AppendLn('end;');
      Impl.AppendLn;

      if PasVersion = cgsvAll then
        Impl.AppendLn('{$IFDEF VER_XE}');

      if PasVersion in [cgsvXE, cgsvAll] then
      begin
        Proto :=
            'procedure ' + FPascalTypeStr + 'Helper.DecodeField' +
                '(const Field: TpbProtoBufDecodeField);';
        Impl.AppendLn(Proto);
      end;

      if PasVersion = cgsvAll then
        Impl.AppendLn('{$ELSE}');

      if PasVersion in [cgsvLessXE, cgsvAll] then
      begin
        Proto :=
            'procedure ' + FPascalArrayDecodeFuncName +
                '(const Field: TpbProtoBufDecodeField; var Value: ' + FPascalTypeStr + ');';
        Impl.AppendLn(Proto);
      end;

      if PasVersion = cgsvAll then
        Impl.AppendLn('{$ENDIF}');


      Impl.AppendLn('var');
      Impl.AppendLn('  L : Integer;');
      Impl.AppendLn('begin');

      if PasVersion = cgsvAll then
        Impl.AppendLn('{$IFDEF VER_XE}');

      if PasVersion in [cgsvXE, cgsvAll] then
      begin
        Impl.AppendLn('  L := Length(Self);');
        Impl.AppendLn('  SetLength(Self, L + 1);');
        Impl.AppendLn('  Self[L].Init;');
        Impl.AppendLn('  Self[L].DecodeField(Field);');
      end;

      if PasVersion = cgsvAll then
        Impl.AppendLn('{$ELSE}');

      if PasVersion in [cgsvLessXE, cgsvAll] then
      begin
        Impl.AppendLn('  L := Length(Value);');
        Impl.AppendLn('  SetLength(Value, L + 1);');
        S := FPascalBaseType.GetPascalInitInstanceCall('Value[L]');
        if S <> '' then
          Impl.AppendLn('  ' + S + ';');
        Impl.AppendLn('  ' + FPascalBaseType.GetPascalDecodeFieldCall('Field', 'Value[L]') + ';');
      end;

      if PasVersion = cgsvAll then
        Impl.AppendLn('{$ENDIF}');

      Impl.AppendLn('end;');
      Impl.AppendLn;

      if PasVersion = cgsvAll then
        Impl.AppendLn('{$IFDEF VER_XE}');

      if PasVersion in [cgsvXE, cgsvAll] then
      begin
        Proto :=
            'procedure ' + FPascalTypeStr + 'Helper.DecodeField_Packed' +
                '(const Field: TpbProtoBufDecodeField);';
        Impl.AppendLn(Proto);
      end;

      if PasVersion = cgsvAll then
        Impl.AppendLn('{$ELSE}');

      if PasVersion in [cgsvLessXE, cgsvAll] then
      begin
        Proto :=
            'procedure ' + FPascalArrayDecodeFuncName + '_Packed' +
                '(const Field: TpbProtoBufDecodeField; var Value: ' + FPascalTypeStr + ');';
        Impl.AppendLn(Proto);
      end;

      if PasVersion = cgsvAll then
        Impl.AppendLn('{$ENDIF}');


      Impl.AppendLn('var');
      Impl.AppendLn('  P : PByte;');
      Impl.AppendLn('  L, N, I : Integer;');
      Impl.AppendLn('begin');
      Impl.AppendLn('  P := Field.ValueVarBytesPtr;');
      Impl.AppendLn('  L := 0;');
      Impl.AppendLn('  N := Field.ValueVarBytesLen;');
      Impl.AppendLn('  while N > 0 do');
      Impl.AppendLn('    begin');

      if PasVersion = cgsvAll then
        Impl.AppendLn('{$IFDEF VER_XE}');

      if PasVersion in [cgsvXE, cgsvAll] then
      begin
        Impl.AppendLn('      SetLength(Self, L + 1);');
        Impl.AppendLn('      Self[L].Init;');
        Impl.AppendLn('      I := Self[L].DecodeValue(P^, N);');
      end;

      if PasVersion = cgsvAll then
        Impl.AppendLn('{$ELSE}');

      if PasVersion in [cgsvLessXE, cgsvAll] then
      begin
        Impl.AppendLn('      SetLength(Value, L + 1);');
        S := FPascalBaseType.GetPascalInitInstanceCall('Value[L]');
        if S <> '' then
          Impl.AppendLn('      ' + S + ';');
        Impl.AppendLn('      I := ' + FPascalBaseType.GetPascalDecodeValueCall('P^', 'N', 'Value[L]') + ';');
      end;

      if PasVersion = cgsvAll then
        Impl.AppendLn('{$ENDIF}');

      Impl.AppendLn('      Inc(L);');
      Impl.AppendLn('      Inc(P, I);');
      Impl.AppendLn('      Dec(N, I);');
      Impl.AppendLn('    end;');
      Impl.AppendLn('end;');
      Impl.AppendLn;

      Impl.AppendLn;
      Impl.AppendLn;

      Intf.AppendLn;
      Intf.AppendLn;
      Intf.AppendLn;
    end;
end;

procedure TpbProtoPascalFieldType.GenerateMessageUnit(const AUnit: TCodeGenPascalUnit;
  const PasVersion: TCodeGenSupportVersion);
begin
  if FIsArray then
    GenerateArrayHelpers(AUnit, PasVersion);
end;



{ TpbProtoPascalField }

constructor TpbProtoPascalField.Create(const AParentMessage: TpbProtoMessage; const AFactory: TpbProtoNodeFactory);
begin
  inherited Create(AParentMessage, AFactory);
end;

destructor TpbProtoPascalField.Destroy;
begin
  inherited Destroy;
end;

function TpbProtoPascalField.GetPascalFieldType: TpbProtoPascalFieldType;
begin
  Result := FFieldType as TpbProtoPascalFieldType;
end;

function TpbProtoPascalField.GetPascalParentMessage: TpbProtoPascalMessage;
begin
  Result := FParentMessage as TpbProtoPascalMessage;
end;

function TpbProtoPascalField.GetPascalDefaultValue: TpbProtoPascalLiteral;
begin
  Result := FDefaultValue as TpbProtoPascalLiteral;
end;

function TpbProtoPascalField.IsArray: Boolean;
begin
  Result := FCardinality = pfcRepeated;
end;

procedure TpbProtoPascalField.CodeGenInit;
begin
  FPascalProtoName := ProtoNameToPascalProtoName(FName);
  FPascalName := FPascalProtoName;
  if FCardinality = pfcOptional then
    FPascalHasValueName := FPascalName + '_HasValue'
  else
    FPascalHasValueName := '';

  GetPascalFieldType.CodeGenInit;

  FPascalRecordDefinition :=
      FPascalName + ' : ' + GetPascalFieldType.FPascalTypeStr + ';';

  FPascalRecordHasValueDefinition := '';
  if FCardinality = pfcOptional then
    FPascalRecordHasValueDefinition :=
        FPascalHasValueName + ' : Boolean;';

  FPascalRecordInitHasValueStatement := '';
  if not GetPascalFieldType.FIsArray and (GetPascalFieldType.FPascalBaseType.FBaseKind = bkMsg) then
    begin
      FPascalRecordInitStatement :=
          GetPascalFieldType.FPascalBaseType.FMsg.FPascalProtoName + 'Init(' + FPascalName + ');';
      FPascalRecordFinaliseStatement :=
          GetPascalFieldType.FPascalBaseType.FMsg.FPascalProtoName + 'Finalise(' + FPascalName + ');';
    end
  else
    begin
      FPascalRecordInitStatement :=
          FPascalName + ' := ' + GetPascalFieldType.FPascalDefaultValueStr + ';';
      if FCardinality = pfcOptional then
        FPascalRecordInitHasValueStatement :=
            FPascalHasValueName + ' := False;';
      FPascalRecordFinaliseStatement := '';
    end;
end;

procedure TpbProtoPascalField.GenerateMessageUnit(const AUnit: TCodeGenPascalUnit;
  const PasVersion: TCodeGenSupportVersion);
begin
  GetPascalFieldType.GenerateMessageUnit(AUnit, PasVersion);
end;

function TpbProtoPascalField.GetPascalEncodeFieldTypeCall(const ParBuf, ParBufSize, ParValue: RawByteString): RawByteString;
begin
  if IsArray then
    Result := GetPascalFieldType.FPascalEncodeFuncName +
        '(' + ParBuf + ', ' + ParBufSize + ', ' + IntToStringB(FTagID) + ', ' + ParValue + ')'
  else
    Result := GetPascalFieldType.FPascalBaseType.GetPascalEncodeFieldCall(
        ParBuf, ParBufSize, IntToStringB(FTagID), ParValue);
end;

function TpbProtoPascalField.GetPascalDecodeFieldTypeCall(const ParField, ParValue: RawByteString; const PascalFieldPrefix: RawByteString): RawByteString;
begin
  if IsArray then
    Result := GetPascalFieldType.FPascalDecodeFuncName + '(' + ParField + ', ' + ParValue + ')'
  else
  if FCardinality = pfcOptional then
    Result :=
        'begin ' +
        GetPascalFieldType.FPascalBaseType.GetPascalDecodeFieldCall(ParField, ParValue) + '; ' +
        PascalFieldPrefix + FPascalHasValueName + ' := True; ' +
        'end'
  else
    Result := GetPascalFieldType.FPascalBaseType.GetPascalDecodeFieldCall(ParField, ParValue);
end;



{ TpbProtoPascalMessage }

constructor TpbProtoPascalMessage.Create(const AParentNode: TpbProtoNode);
begin
  inherited Create(AParentNode);
end;

destructor TpbProtoPascalMessage.Destroy;
begin
  inherited Destroy;
end;

function TpbProtoPascalMessage.GetPascalPackage: TpbProtoPascalPackage;
begin
  Result := FParentNode as TpbProtoPascalPackage;
end;

function TpbProtoPascalMessage.GetPascalField(const Idx: Integer): TpbProtoPascalField;
begin
  Result := GetField(Idx) as TpbProtoPascalField;
end;

function TpbProtoPascalMessage.GetPascalEnum(const Idx: Integer): TpbProtoPascalEnum;
begin
  Result := GetEnum(Idx) as TpbProtoPascalEnum;
end;

function TpbProtoPascalMessage.GetPascalMessage(const Idx: Integer): TpbProtoPascalMessage;
begin
  Result := GetMessage(Idx) as TpbProtoPascalMessage;
end;

procedure TpbProtoPascalMessage.CodeGenInit;
var I : Integer;
begin
  FPascalProtoName := ProtoNameToPascalProtoName(FName) + 'Record';
  FPascalName := 'T' + FPascalProtoName;

  for I := 0 to GetEnumCount - 1 do
    GetPascalEnum(I).CodeGenInit;
  for I := 0 to GetMessageCount - 1 do
    GetPascalMessage(I).CodeGenInit;

  for I := 0 to GetFieldCount - 1 do
    GetPascalField(I).CodeGenInit;
end;

procedure TpbProtoPascalMessage.GenerateRecordDeclaration(const AUnit: TCodeGenPascalUnit;
  const PasVersion: TCodeGenSupportVersion);
var
  I : Integer;
  F : TpbProtoPascalField;
begin
  with AUnit do
    begin
      Intf.AppendLn('type');
      Intf.AppendLn('  ' + FPascalName + ' = record');

      if PasVersion = cgsvAll then
        Intf.AppendLn('{$IFDEF VER_XE}');

      if PasVersion in [cgsvXE, cgsvAll] then
        Intf.AppendLn('  public');

      if PasVersion = cgsvAll then
        Intf.AppendLn('{$ENDIF}');

      for I := 0 to GetFieldCount - 1 do
        begin
          F := GetPascalField(I);
          if F.FPascalRecordHasValueDefinition <> '' then
            Intf.AppendLn('    ' + F.FPascalRecordHasValueDefinition);
          Intf.AppendLn('    ' + F.FPascalRecordDefinition);
        end;

      if PasVersion = cgsvAll then
        Intf.AppendLn('  {$IFDEF VER_XE}');

      if PasVersion in [cgsvXE, cgsvAll] then
      begin
        Intf.AppendLn('    procedure Init;');
        Intf.AppendLn('    procedure Finalise;');

        Intf.AppendLn('    function  EncodeData(var Buf; const BufSize: Integer): Integer;');
        Intf.AppendLn('    function  EncodeValue(var Buf; const BufSize: Integer): Integer;');
        Intf.AppendLn('    function  EncodeField(var Buf; const BufSize: Integer; const FieldNum: Integer): Integer;');

        Intf.AppendLn('    function  DecodeValue(const Buf; const BufSize: Integer): Integer;');
        Intf.AppendLn('    procedure DecodeField(const Field: TpbProtoBufDecodeField);');
      end;

      if PasVersion = cgsvAll then
        Intf.AppendLn('  {$ENDIF}');

      Intf.AppendLn('  end;');
      Intf.AppendLn('  P' + FPascalProtoName + ' = ^T' + FPascalProtoName + ';');
      Intf.AppendLn;
  end;

end;

procedure TpbProtoPascalMessage.GenerateRecordInitProc(const AUnit: TCodeGenPascalUnit;
  const PasVersion: TCodeGenSupportVersion);
var
  I : Integer;
  Proto, S : RawByteString;
  F : Boolean;
  Field : TpbProtoPascalField;
begin
  with AUnit do
    begin
      if PasVersion in [cgsvLessXE, cgsvAll] then
      begin
        Proto := 'procedure ' + FPascalProtoName + 'Init(var A: ' + FPascalName + ');';
        Intf.AppendLn(Proto);
      end;

      if PasVersion = cgsvAll then
        Impl.AppendLn('{$IFDEF VER_XE}');

      if PasVersion in [cgsvXE, cgsvAll] then
        Impl.AppendLn('procedure T' + FPascalProtoName + '.Init;');

      if PasVersion = cgsvAll then
        Impl.AppendLn('{$ELSE}');

      if PasVersion in [cgsvLessXE, cgsvAll] then
      begin
        Proto := 'procedure ' + FPascalProtoName + 'Init(var A: ' + FPascalName + ');';
        Impl.AppendLn(Proto);
      end;

      if PasVersion = cgsvAll then
        Impl.AppendLn('{$ENDIF}');

      Impl.AppendLn('begin');

      if PasVersion = cgsvAll then
        Impl.AppendLn('{$IFDEF VER_XE}');

      if PasVersion in [cgsvXE, cgsvAll] then
      Impl.AppendLn('  with Self do');

      if PasVersion = cgsvAll then
        Impl.AppendLn('{$ELSE}');

      if PasVersion in [cgsvLessXE, cgsvAll] then
      Impl.AppendLn('  with A do');

      if PasVersion = cgsvAll then
        Impl.AppendLn('{$ENDIF}');

      Impl.AppendLn('  begin');
      for I := 0 to GetFieldCount - 1 do
        begin
          Field := GetPascalField(I);
          if Field.FPascalRecordInitHasValueStatement <> '' then
            Impl.AppendLn('    ' + GetPascalField(I).FPascalRecordInitHasValueStatement);
          Impl.AppendLn('    ' + GetPascalField(I).FPascalRecordInitStatement);
        end;
      Impl.AppendLn('  end;');
      Impl.AppendLn('end;');
      Impl.AppendLn;

      if PasVersion in [cgsvLessXE, cgsvAll] then
      begin
        Proto := 'procedure ' + FPascalProtoName + 'Finalise(var A: ' + FPascalName + ');';
        Intf.AppendLn(Proto);
      end;

      if PasVersion = cgsvAll then
        Impl.AppendLn('{$IFDEF VER_XE}');

      if PasVersion in [cgsvXE, cgsvAll] then
      begin
        Impl.AppendLn('procedure T' + FPascalProtoName + '.Finalise;');
      end;

      if PasVersion = cgsvAll then
        Impl.AppendLn('{$ELSE}');

      if PasVersion in [cgsvLessXE, cgsvAll] then
      begin
        Proto := 'procedure ' + FPascalProtoName + 'Finalise(var A: ' + FPascalName + ');';
        Impl.AppendLn(Proto);
      end;

      if PasVersion = cgsvAll then
        Impl.AppendLn('{$ENDIF}');

      F := False;
      for I := GetFieldCount - 1 downto 0 do
         if GetPascalField(I).FPascalRecordFinaliseStatement <> '' then
           begin
             F := True;
             break;
           end;
      Impl.AppendLn('begin');
      if F then
        begin
          if PasVersion = cgsvAll then
            Impl.AppendLn('{$IFDEF VER_XE}');

          Impl.AppendLn('  with Self do');

          if PasVersion = cgsvAll then
            Impl.AppendLn('{$ELSE}');

          Impl.AppendLn('  with A do');

          if PasVersion = cgsvAll then
            Impl.AppendLn('{$ENDIF}');

          Impl.AppendLn('  begin');
          for I := GetFieldCount - 1 downto 0 do
            begin
              S := GetPascalField(I).FPascalRecordFinaliseStatement;
              if S <> '' then
                Impl.AppendLn('    ' + S);
            end;
          Impl.AppendLn('  end;');
        end;
      Impl.AppendLn('end;');
      Impl.AppendLn;
    end;
end;

procedure TpbProtoPascalMessage.GenerateRecordEncodeProc(const AUnit: TCodeGenPascalUnit;
  const PasVersion: TCodeGenSupportVersion);
var
  I, L : Integer;
  F : TpbProtoPascalField;
  Ind : RawByteString;
  Proto : RawByteString;
  EncodeDataProcName : RawByteString;
  EncodeValueProcName : RawByteString;
  EncodeFieldProcName : RawByteString;
begin
  with AUnit do
    begin
      EncodeDataProcName := 'pbEncodeData' + FPascalProtoName;

      if PasVersion in [cgsvLessXE, cgsvAll] then
      begin
        Proto := 'function  ' + EncodeDataProcName + '(var Buf; const BufSize: Integer; const A: ' + FPascalName + '): Integer;';
        Intf.AppendLn(Proto);
      end;

      if PasVersion = cgsvAll then
        Impl.AppendLn('{$IFDEF VER_XE}');

      if PasVersion in [cgsvXE, cgsvAll] then
        Impl.AppendLn('function T' + FPascalProtoName + '.EncodeData(var Buf; const BufSize: Integer): Integer;');

      if PasVersion = cgsvAll then
        Impl.AppendLn('{$ELSE}');

      if PasVersion in [cgsvLessXE, cgsvAll] then
      begin
        Proto := 'function ' + EncodeDataProcName + '(var Buf; const BufSize: Integer; const A: ' + FPascalName + '): Integer;';
        Impl.AppendLn(Proto);
      end;

      if PasVersion = cgsvAll then
        Impl.AppendLn('{$ENDIF}');

      Impl.AppendLn('var');
      Impl.AppendLn('  P : PByte;');
      Impl.AppendLn('  L : Integer;');
      Impl.AppendLn('  I : Integer;');
      Impl.AppendLn('begin');
      Impl.AppendLn('  P := @Buf;');
      Impl.AppendLn('  L := BufSize;');
      L := GetFieldCount;
      for I := 0 to L - 1 do
        begin
          F := GetPascalField(I);
          Ind := '  ';
          if F.Cardinality = pfcOptional then
            begin
              case PasVersion of
                cgsvLessXE:  Impl.AppendLn(Ind + 'if A.' + F.FPascalHasValueName + ' then');
                cgsvXE:      Impl.AppendLn(Ind + 'if ' + F.FPascalHasValueName + ' then');
                cgsvAll:     Impl.AppendLn(Ind + 'if {$IFNDEF VER_XE}A.{$IFEND}' + F.FPascalHasValueName + ' then');
              end;

              Ind := Ind + '  ';
              Impl.AppendLn(Ind + 'begin');
              Ind := Ind + '  ';
            end;

          if F.IsArray then
          begin
            if PasVersion = cgsvAll then
              Impl.AppendLn('{$IFDEF VER_XE}');

            if PasVersion in [cgsvXE, cgsvAll] then
              Impl.AppendLn(Ind + 'I := ' + F.FPascalName + '.EncodeField(P^, L, ' + IntToStringB(F.FTagID) + ');');

            if PasVersion = cgsvAll then
              Impl.AppendLn('{$ELSE}');

            if PasVersion in [cgsvLessXE, cgsvAll] then
              Impl.AppendLn(Ind + 'I := ' + F.GetPascalEncodeFieldTypeCall('P^', 'L', 'A.' + F.FPascalName) + ';');

            if PasVersion = cgsvAll then
              Impl.AppendLn('{$ENDIF}');
          end
          else
          begin
            case PasVersion of
              cgsvLessXE:  Impl.AppendLn(Ind + 'I := ' + F.GetPascalEncodeFieldTypeCall('P^', 'L', 'A.' + F.FPascalName) + ';');
              cgsvXE:      Impl.AppendLn(Ind + 'I := ' + F.GetPascalEncodeFieldTypeCall('P^', 'L', '' + F.FPascalName) + ';');
              cgsvAll:     Impl.AppendLn(Ind + 'I := ' + F.GetPascalEncodeFieldTypeCall('P^', 'L', '{$IFNDEF VER_XE}A.{$IFEND}' + F.FPascalName) + ';');
            end;
          end;

          Impl.AppendLn(Ind + 'Dec(L, I);');
          if I < L - 1 then
            Impl.AppendLn(Ind + 'Inc(P, I);');
          if F.Cardinality = pfcOptional then
            begin
              SetLength(Ind, Length(Ind) - 2);
              Impl.AppendLn(Ind + 'end;');
            end;
        end;
      Impl.AppendLn('  Result := BufSize - L;');
      Impl.AppendLn('end;');
      Impl.AppendLn;

      EncodeValueProcName := 'pbEncodeValue' + FPascalProtoName;
      if PasVersion in [cgsvLessXE, cgsvAll] then
      begin
        Proto := 'function  ' + EncodeValueProcName + '(var Buf; const BufSize: Integer; const A: ' + FPascalName + '): Integer;';
        Intf.AppendLn(Proto);
      end;

      if PasVersion = cgsvAll then
        Impl.AppendLn('{$IFDEF VER_XE}');

      if PasVersion in [cgsvXE, cgsvAll] then
        Impl.AppendLn('function T' + FPascalProtoName + '.EncodeValue(var Buf; const BufSize: Integer): Integer;');

      if PasVersion = cgsvAll then
        Impl.AppendLn('{$ELSE}');

      if PasVersion in [cgsvLessXE, cgsvAll] then
      begin
        Proto := 'function ' + EncodeValueProcName + '(var Buf; const BufSize: Integer; const A: ' + FPascalName + '): Integer;';
        Impl.AppendLn(Proto);
      end;

      if PasVersion = cgsvAll then
        Impl.AppendLn('{$ENDIF}');


      Impl.AppendLn('var');
      Impl.AppendLn('  P : PByte;');
      Impl.AppendLn('  L, N, I : Integer;');
      Impl.AppendLn('begin');
      Impl.AppendLn('  P := @Buf;');
      Impl.AppendLn('  L := BufSize;');

      if PasVersion = cgsvAll then
        Impl.AppendLn('{$IFDEF VER_XE}');

      if PasVersion in [cgsvXE, cgsvAll] then
        Impl.AppendLn('  N := EncodeData(P^, 0);');

      if PasVersion = cgsvAll then
        Impl.AppendLn('{$ELSE}');

      if PasVersion in [cgsvLessXE, cgsvAll] then
        Impl.AppendLn('  N := ' + EncodeDataProcName + '(P^, 0, A);');

      if PasVersion = cgsvAll then
        Impl.AppendLn('{$ENDIF}');

      Impl.AppendLn('  I := pbEncodeValueInt32(P^, L, N);');
      Impl.AppendLn('  Inc(P, I);');
      Impl.AppendLn('  Dec(L, I);');

      if PasVersion = cgsvAll then
        Impl.AppendLn('{$IFDEF VER_XE}');

      if PasVersion in [cgsvXE, cgsvAll] then
        Impl.AppendLn('  I := EncodeData(P^, L);');

      if PasVersion = cgsvAll then
        Impl.AppendLn('{$ELSE}');

      if PasVersion in [cgsvLessXE, cgsvAll] then
        Impl.AppendLn('  I := ' + EncodeDataProcName + '(P^, L, A);');

      if PasVersion = cgsvAll then
        Impl.AppendLn('{$ENDIF}');


      Impl.AppendLn('  Assert(I = N);');
      Impl.AppendLn('  Dec(L, I);');
      Impl.AppendLn('  Result := BufSize - L;');
      Impl.AppendLn('end;');
      Impl.AppendLn;

      EncodeFieldProcName := 'pbEncodeField' + FPascalProtoName;
      if PasVersion in [cgsvLessXE, cgsvAll] then
      begin
        Proto := 'function  ' + EncodeFieldProcName + '(var Buf; const BufSize: Integer; const FieldNum: Integer; const A: ' + FPascalName + '): Integer;';
        Intf.AppendLn(Proto);
      end;

      if PasVersion = cgsvAll then
        Impl.AppendLn('{$IFDEF VER_XE}');

      if PasVersion in [cgsvXE, cgsvAll] then
        Impl.AppendLn('function T' + FPascalProtoName + '.EncodeField(var Buf; const BufSize: Integer; const FieldNum: Integer): Integer;');

      if PasVersion = cgsvAll then
        Impl.AppendLn('{$ELSE}');

      if PasVersion in [cgsvLessXE, cgsvAll] then
      begin
        Proto := 'function ' + EncodeFieldProcName + '(var Buf; const BufSize: Integer; const FieldNum: Integer; const A: ' + FPascalName + '): Integer;';
        Impl.AppendLn(Proto);
      end;

      if PasVersion = cgsvAll then
        Impl.AppendLn('{$ENDIF}');

      Impl.AppendLn('var');
      Impl.AppendLn('  P : PByte;');
      Impl.AppendLn('  L : Integer;');
      Impl.AppendLn('  I : Integer;');
      Impl.AppendLn('begin');
      Impl.AppendLn('  P := @Buf;');
      Impl.AppendLn('  L := BufSize;');
      Impl.AppendLn('  I := pbEncodeFieldKey(P^, L, FieldNum, pwtVarBytes);');
      Impl.AppendLn('  Dec(L, I);');
      Impl.AppendLn('  Inc(P, I);');

      if PasVersion = cgsvAll then
        Impl.AppendLn('{$IFDEF VER_XE}');

      if PasVersion in [cgsvXE, cgsvAll] then
        Impl.AppendLn('  I := EncodeValue(P^, L);');

      if PasVersion = cgsvAll then
        Impl.AppendLn('{$ELSE}');

      if PasVersion in [cgsvLessXE, cgsvAll] then
        Impl.AppendLn('  I := ' + EncodeValueProcName + '(P^, L, A);');

      if PasVersion = cgsvAll then
        Impl.AppendLn('{$ENDIF}');

      Impl.AppendLn('  Dec(L, I);');
      Impl.AppendLn('  Result := BufSize - L;');
      Impl.AppendLn('end;');
      Impl.AppendLn;
    end;
end;

procedure TpbProtoPascalMessage.GenerateRecordDecodeProc(const AUnit: TCodeGenPascalUnit;
  const PasVersion: TCodeGenSupportVersion);
var
  I, L : Integer;
  F : TpbProtoPascalField;
  CallbackName : RawByteString;
  Proto : RawByteString;
begin
  with AUnit do
    begin
      CallbackName := 'pbDecodeField' + FPascalProtoName + '_CallbackProc';
      Impl.AppendLn('procedure ' + CallbackName + '(const Field: TpbProtoBufDecodeField; const Data: Pointer);');
      Impl.AppendLn('var');
      Impl.AppendLn('  A : P' + FPascalProtoName + ';');
      Impl.AppendLn('begin');
      Impl.AppendLn('  A := Data;');
      Impl.AppendLn('  case Field.FieldNum of');
      L := GetFieldCount;
      for I := 0 to L - 1 do
        begin
          F := GetPascalField(I);

          if F.IsArray then
          begin
            case PasVersion of
              cgsvLessXE:  Impl.AppendLn('    ' + IntToStringB(F.FTagID) + ' : ' + F.GetPascalDecodeFieldTypeCall('Field', 'A^.' + F.FPascalName, 'A^.') + ';');
              cgsvXE:      Impl.AppendLn('    ' + IntToStringB(F.FTagID) + ' : ' + 'A^.' + F.FPascalName + '.DecodeField(Field)' + ';');
              cgsvAll:     Impl.AppendLn('    ' + IntToStringB(F.FTagID) + ' : {$IFDEF VER_XE}' + 'A^.' + F.FPascalName + '.DecodeField(Field)' + '{$ELSE}'  + F.GetPascalDecodeFieldTypeCall('Field', 'A^.' + F.FPascalName, 'A^.') + '{$ENDIF};');
            end;
          end
          else
          begin
            Impl.AppendLn('    ' + IntToStringB(F.FTagID) + ' : ' + F.GetPascalDecodeFieldTypeCall('Field', 'A^.' + F.FPascalName, 'A^.') + ';');
          end;
        end;
      Impl.AppendLn('  end;');
      Impl.AppendLn('end;');
      Impl.AppendLn;

      if PasVersion in [cgsvLessXE, cgsvAll] then
      begin
        Proto := 'function  pbDecodeValue' + FPascalProtoName + '(const Buf; const BufSize: Integer; var Value: ' + FPascalName + '): Integer;';
        Intf.AppendLn(Proto);
      end;

      if PasVersion = cgsvAll then
        Impl.AppendLn('{$IFDEF VER_XE}');

      if PasVersion in [cgsvXE, cgsvAll] then
        Impl.AppendLn('function T' + FPascalProtoName + '.DecodeValue(const Buf; const BufSize: Integer): Integer;');

      if PasVersion = cgsvAll then
        Impl.AppendLn('{$ELSE}');

      if PasVersion in [cgsvLessXE, cgsvAll] then
      begin
        Proto := 'function pbDecodeValue' + FPascalProtoName + '(const Buf; const BufSize: Integer; var Value: ' + FPascalName + '): Integer;';
        Impl.AppendLn(Proto);
      end;

      if PasVersion = cgsvAll then
        Impl.AppendLn('{$ENDIF}');

      Impl.AppendLn('var');
      Impl.AppendLn('  P : PByte;');
      Impl.AppendLn('  L, I, N : Integer;');
      Impl.AppendLn('begin');
      Impl.AppendLn('  P := @Buf;');
      Impl.AppendLn('  L := BufSize;');
      Impl.AppendLn('  I := pbDecodeValueInt32(P^, L, N);');
      Impl.AppendLn('  Dec(L, I);');
      Impl.AppendLn('  Inc(P, I);');

      case PasVersion of
        cgsvLessXE:  Impl.AppendLn('  pbDecodeProtoBuf(P^, N, ' + CallbackName + ', @Value);');
        cgsvXE:      Impl.AppendLn('  pbDecodeProtoBuf(P^, N, ' + CallbackName + ', @Self);');
        cgsvAll:     Impl.AppendLn('  pbDecodeProtoBuf(P^, N, ' + CallbackName + ', {$IFDEF VER_XE}@Self{$ELSE}@Value{$ENDIF});');
      end;



      Impl.AppendLn('  Dec(L, N);');
      Impl.AppendLn('  Result := BufSize - L;');
      Impl.AppendLn('end;');
      Impl.AppendLn;

      if PasVersion in [cgsvLessXE, cgsvAll] then
      begin
        Proto := 'procedure pbDecodeField' + FPascalProtoName + '(const Field: TpbProtoBufDecodeField; var Value: ' + FPascalName + ');';
        Intf.AppendLn(Proto);
      end;

      if PasVersion = cgsvAll then
        Impl.AppendLn('{$IFDEF VER_XE}');

      if PasVersion in [cgsvXE, cgsvAll] then
        Impl.AppendLn('procedure T' + FPascalProtoName + '.DecodeField(const Field: TpbProtoBufDecodeField);');

      if PasVersion = cgsvAll then
        Impl.AppendLn('{$ELSE}');

      if PasVersion in [cgsvLessXE, cgsvAll] then
      begin
        Proto := 'procedure pbDecodeField' + FPascalProtoName + '(const Field: TpbProtoBufDecodeField; var Value: ' + FPascalName + ');';
        Impl.AppendLn(Proto);
      end;

      if PasVersion = cgsvAll then
        Impl.AppendLn('{$ENDIF}');

      Impl.AppendLn('begin');
      case PasVersion of
        cgsvLessXE:  Impl.AppendLn('  pbDecodeProtoBuf(Field.ValueVarBytesPtr^, Field.ValueVarBytesLen, ' + CallbackName + ', @Value);');
        cgsvXE:      Impl.AppendLn('  pbDecodeProtoBuf(Field.ValueVarBytesPtr^, Field.ValueVarBytesLen, ' + CallbackName + ', @Self);');
        cgsvAll:     Impl.AppendLn('  pbDecodeProtoBuf(Field.ValueVarBytesPtr^, Field.ValueVarBytesLen, ' + CallbackName + ', {$IFDEF VER_XE}@Self{$ELSE}@Value{$ENDIF});');
      end;

      Impl.AppendLn('end;');
      Impl.AppendLn;
    end;
end;

procedure TpbProtoPascalMessage.GenerateMessageUnit(const AUnit: TCodeGenPascalUnit; const PasVersion: TCodeGenSupportVersion);
var
  I : Integer;
  CommentLine : RawByteString;
begin
  for I := 0 to GetEnumCount - 1 do
    GetPascalEnum(I).GenerateMessageUnit(AUnit);
  for I := 0 to GetFieldCount - 1 do
    GetPascalField(I).GenerateMessageUnit(AUnit, PasVersion);
  for I := 0 to GetMessageCount - 1 do
    GetPascalMessage(I).GenerateMessageUnit(AUnit, PasVersion);

  CommentLine := '{ ' + FPascalName + ' }';

  AUnit.Intf.AppendLn(CommentLine);
  AUnit.Intf.AppendLn;

  AUnit.Impl.AppendLn(CommentLine);
  AUnit.Impl.AppendLn;

  GenerateRecordDeclaration(AUnit, PasVersion);

  if PasVersion = cgsvAll then
    AUnit.Intf.AppendLn('{$IFNDEF VER_XE}');

  GenerateRecordInitProc(AUnit, PasVersion);
  GenerateRecordEncodeProc(AUnit, PasVersion);
  GenerateRecordDecodeProc(AUnit, PasVersion);

  if PasVersion = cgsvAll then
    AUnit.Intf.AppendLn('{$ENDIF}');

  AUnit.Intf.AppendLn;

  AUnit.Intf.AppendLn;
  AUnit.Intf.AppendLn;

  AUnit.Impl.AppendLn;
  AUnit.Impl.AppendLn;
end;



{ TpbProtoPascalPackage }

constructor TpbProtoPascalPackage.Create;
begin
  inherited Create;
  FMessageUnit := TCodeGenPascalUnit.Create;
end;

destructor TpbProtoPascalPackage.Destroy;
begin
  FreeAndNil(FMessageUnit);
  inherited Destroy;
end;

procedure TpbProtoPascalPackage.CodeGenInit;
var
  I : Integer;
begin
  FPascalProtoName := ProtoNameToPascalProtoName(FName);
  FPascalBaseName := 'pb' + FPascalProtoName;
  FMessageUnit.Name := FPascalBaseName + 'Messages';

  for I := 0 to GetImportedPackageCount - 1 do
    GetPascalImportedPackage(I).CodeGenInit;
  for I := 0 to GetEnumCount - 1 do
    GetPascalEnum(I).CodeGenInit;
  for I := 0 to GetMessageCount - 1 do
    GetPascalMessage(I).CodeGenInit;
end;

procedure TpbProtoPascalPackage.GenerateMessageUnit(const PasVersion: TCodeGenSupportVersion);
var I : Integer;
begin
   FMessageUnit.UnitComments := FMessageUnit.UnitComments +
      '{ Unit ' + FMessageUnit.FName + '.pas }' + CRLF;
  if FFileName <> '' then
    FMessageUnit.UnitComments := FMessageUnit.UnitComments +
        '{ Generated from ' + FFileName + ' }' + CRLF;
  FMessageUnit.UnitComments := FMessageUnit.UnitComments +
      '{ Package ' + FPascalProtoName + ' }' + CRLF;

  FMessageUnit.IntfUses.Add('flcUtils');
  FMessageUnit.IntfUses.Add('flcStrings');
  FMessageUnit.IntfUses.Add('flcProtoBufUtils');

  for I := 0 to GetImportedPackageCount - 1 do
    FMessageUnit.IntfUses.Add(GetPascalImportedPackage(I).FMessageUnit.FName);

  // add chongchong
  if PasVersion = cgsvAll then
  begin
    FMessageUnit.Intf.AppendLn('{$IF CompilerVersion >= 22}');
    FMessageUnit.Intf.AppendLn('  {$DEFINE VER_XE}');
    FMessageUnit.Intf.AppendLn('{$IFEND}');
    FMessageUnit.Intf.AppendCRLF;
    FMessageUnit.Intf.AppendCRLF;
  end;

  for I := 0 to GetEnumCount - 1 do
    GetPascalEnum(I).GenerateMessageUnit(FMessageUnit);
  for I := 0 to GetMessageCount - 1 do
    GetPascalMessage(I).GenerateMessageUnit(FMessageUnit, PasVersion);
end;

function TpbProtoPascalPackage.GetPascalMessage(const Idx: Integer): TpbProtoPascalMessage;
begin
  Result := GetMessage(Idx) as TpbProtoPascalMessage;
end;

function TpbProtoPascalPackage.GetPascalEnum(const Idx: Integer): TpbProtoPascalEnum;
begin
  Result := GetEnum(Idx) as TpbProtoPascalEnum;
end;

function TpbProtoPascalPackage.GetPascalImportedPackage(const Idx: Integer): TpbProtoPascalPackage;
begin
  Result := GetImportedPackage(Idx) as TpbProtoPascalPackage;
end;

procedure TpbProtoPascalPackage.Save(const OutputPath: String);
begin
  FMessageUnit.Save(OutputPath);
end;



{ TpbProtoCodeGenPascal }

constructor TpbProtoCodeGenPascal.Create;
begin
  inherited Create;
end;

destructor TpbProtoCodeGenPascal.Destroy;
begin
  inherited Destroy;
end;

procedure TpbProtoCodeGenPascal.GenerateCode(const APackage: TpbProtoPackage; const PasVersion: TCodeGenSupportVersion);
var P : TpbProtoPascalPackage;
begin
  Assert(Assigned(APackage));

  P := (APackage as TpbProtoPascalPackage);
  P.CodeGenInit;
  P.GenerateMessageUnit(PasVersion);
  P.Save(FOutputPath);
end;



{ TpbProtoPascalNodeFactory }

function TpbProtoPascalNodeFactory.CreatePackage: TpbProtoPackage;
begin
  Result := TpbProtoPascalPackage.Create;
end;

function TpbProtoPascalNodeFactory.CreateMessage(const AParentNode: TpbProtoNode): TpbProtoMessage;
begin
  Result := TpbProtoPascalMessage.Create(AParentNode);
end;

function TpbProtoPascalNodeFactory.CreateField(const AParentMessage: TpbProtoMessage): TpbProtoField;
begin
  Result := TpbProtoPascalField.Create(AParentMessage, self);
end;

function TpbProtoPascalNodeFactory.CreateFieldType(const AParentField: TpbProtoField): TpbProtoFieldType;
begin
  Result := TpbProtoPascalFieldType.Create(AParentField);
end;

function TpbProtoPascalNodeFactory.CreateLiteral(const AParentNode: TpbProtoNode): TpbProtoLiteral;
begin
  Result := TpbProtoPascalLiteral.Create(AParentNode);
end;

function TpbProtoPascalNodeFactory.CreateEnum(const AParentNode: TpbProtoNode): TpbProtoEnum;
begin
  Result := TpbProtoPascalEnum.Create(AParentNode);
end;

function TpbProtoPascalNodeFactory.CreateEnumValue(const AParentEnum: TpbProtoEnum): TpbProtoEnumValue;
begin
  Result := TpbProtoPascalEnumValue.Create(AParentEnum);
end;



{ GetPascalProtoNodeFactory }

var
  PascalProtoNodeFactory: TpbProtoPascalNodeFactory = nil;

function GetPascalProtoNodeFactory: TpbProtoPascalNodeFactory;
begin
  if not Assigned(PascalProtoNodeFactory) then
    PascalProtoNodeFactory := TpbProtoPascalNodeFactory.Create;
  Result := PascalProtoNodeFactory;
end;



end.

