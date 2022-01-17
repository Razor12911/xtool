{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals 5.00                                        }
{   File name:        flcJSON.pas                                              }
{   File version:     5.14                                                     }
{   Description:      JSON                                                     }
{                                                                              }
{   Copyright:        Copyright (c) 2011-2020, David J Butler                  }
{                     All rights reserved.                                     }
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
{   2011/05/28  0.01  Initial version with test cases.                         }
{   2011/06/04  0.02  Validation from schema and test case.                    }
{   2011/06/19  0.03  JSONValue improvements.                                  }
{   2011/07/02  0.04  Setters for JSONValues.                                  }
{   2011/07/24  0.05  Variant getters and setters.                             }
{   2011/08/27  0.06  Getters and setters for JSONArray.                       }
{   2013/03/22  4.07  UnicodeString changes.                                   }
{   2013/03/23  4.08  Improvements.                                            }
{   2015/03/31  4.09  Revision.                                                }
{   2015/05/05  4.10  JSONFloat type, dynamic array functions.                 }
{   2016/01/09  5.11  Revised for Fundamentals 5.                              }
{   2018/07/23  5.12  Delphi type changes.                                     }
{   2018/08/12  5.13  String type changes.                                     }
{   2020/06/02  5.14  Locale independent float conversion.                     }
{                                                                              }
{ Supported compilers:                                                         }
{                                                                              }
{   Delphi 7 Win32                      4.09  2015/03/31                       }
{   Delphi XE7 Win32                    5.11  2016/01/09                       }
{   Delphi XE7 Win64                    5.11  2016/01/09                       }
{                                                                              }
{ References:                                                                  }
{                                                                              }
{   JSON Schemas - http://tools.ietf.org/html/draft-zyp-json-schema-03         }
{                                                                              }
{******************************************************************************}

{$INCLUDE ..\flcInclude.inc}

{$IFDEF FREEPASCAL}
  {$WARNINGS OFF}
  {$HINTS OFF}
{$ENDIF}

unit flcJSON;

interface

uses
  { System }
  SysUtils,

  { Fundamentals }
  flcStdTypes,
  flcStrings,
  flcStringBuilder;



type
  { TJSON values }

  EJSONValue = class(Exception);

  EJSONSchema = class(EJSONValue);

  TJSONValueType = (
    jvtString,
    jvtInteger,
    jvtFloat,
    jvtBoolean,
    jvtNull,
    jvtArray,
    jvtObject
    );

  TJSONStringOptions = set of (
    jboIndent
    );

  {$IFDEF ExtendedIsDouble}
  JSONFloat = Double;
  {$ELSE}
  JSONFloat = Extended;
  {$ENDIF}
  JSONFloatArray = array of JSONFloat;

  TJSONArray = class;
  TJSONObject = class;

  TJSONValue = class
  protected
    procedure BuildJSONString(
              const A: TUnicodeStringBuilder;
              const AOptions: TJSONStringOptions;
              const ALevel: Integer); virtual;

    function  GetValueType: TJSONValueType; virtual;

    function  GetValueStr: UnicodeString; virtual;
    function  GetValueStrUTF8: RawByteString;
    function  GetValueInt: Int64; virtual;
    function  GetValueFloat: JSONFloat; virtual;
    function  GetValueBoolean: Boolean; virtual;
    function  GetValueArray: TJSONArray; virtual;
    function  GetValueObject: TJSONObject; virtual;
    function  GetValueVariant: Variant; virtual;

    procedure SetValueStr(const AValue: UnicodeString); virtual;
    procedure SetValueStrUTF8(const AValue: RawByteString); virtual;
    procedure SetValueInt(const AValue: Int64); virtual;
    procedure SetValueFloat(const AValue: JSONFloat); virtual;
    procedure SetValueBoolean(const AValue: Boolean); virtual;
    procedure SetValueVariant(const AValue: Variant); virtual;

    function  GetValueIsStr: Boolean; virtual;
    function  GetValueIsInt: Boolean; virtual;
    function  GetValueIsFloat: Boolean; virtual;
    function  GetValueIsBoolean: Boolean; virtual;
    function  GetValueIsNull: Boolean; virtual;
    function  GetValueIsArray: Boolean; virtual;
    function  GetValueIsObject: Boolean; virtual;

  public
    function  Clone: TJSONValue; virtual;

    function  GetJSONString(const AOptions: TJSONStringOptions = []): UnicodeString;
    function  GetJSONStringUTF8(const AOptions: TJSONStringOptions = []): RawByteString;

    property  ValueType: TJSONValueType read GetValueType;

    property  ValueStr: UnicodeString read GetValueStr write SetValueStr;
    property  ValueStrUTF8: RawByteString read GetValueStrUTF8 write SetValueStrUTF8;
    property  ValueInt: Int64 read GetValueInt write SetValueInt;
    property  ValueFloat: JSONFloat read GetValueFloat write SetValueFloat;
    property  ValueBoolean: Boolean read GetValueBoolean write SetValueBoolean;
    property  ValueArray: TJSONArray read GetValueArray;
    property  ValueObject: TJSONObject read GetValueObject;
    property  ValueVariant: Variant read GetValueVariant write SetValueVariant;

    property  ValueIsStr: Boolean read GetValueIsStr;
    property  ValueIsInt: Boolean read GetValueIsInt;
    property  ValueIsFloat: Boolean read GetValueIsFloat;
    property  ValueIsBoolean: Boolean read GetValueIsBoolean;
    property  ValueIsNull: Boolean read GetValueIsNull;
    property  ValueIsArray: Boolean read GetValueIsArray;
    property  ValueIsObject: Boolean read GetValueIsObject;

    function  Compare(const A: TJSONValue): Integer; virtual;
    procedure Validate(const Schema: TJSONObject); virtual;
  end;

  TJSONString = class(TJSONValue)
  private
    FValue : UnicodeString;

  protected
    procedure BuildJSONString(const A: TUnicodeStringBuilder; const AOptions: TJSONStringOptions; const ALevel: Integer); override;
    function  GetValueType: TJSONValueType; override;
    function  GetValueStr: UnicodeString; override;
    function  GetValueInt: Int64; override;
    function  GetValueFloat: JSONFloat; override;
    function  GetValueBoolean: Boolean; override;
    function  GetValueVariant: Variant; override;
    function  GetValueIsStr: Boolean; override;
    procedure SetValueStr(const AValue: UnicodeString); override;
    procedure SetValueInt(const AValue: Int64); override;
    procedure SetValueFloat(const AValue: JSONFloat); override;
    procedure SetValueBoolean(const AValue: Boolean); override;

  public
    constructor Create(const AValue: UnicodeString);
    constructor CreateUTF8(const AValue: RawByteString);

    function  Clone: TJSONValue; override;
    property  Value: UnicodeString read FValue;
    function  Compare(const A: TJSONValue): Integer; override;
    procedure Validate(const Schema: TJSONObject); override;
  end;

  TJSONInteger = class(TJSONValue)
  private
    FValue : Int64;

  protected
    procedure BuildJSONString(const A: TUnicodeStringBuilder; const AOptions: TJSONStringOptions; const ALevel: Integer); override;
    function  GetValueType: TJSONValueType; override;
    function  GetValueStr: UnicodeString; override;
    function  GetValueInt: Int64; override;
    function  GetValueFloat: JSONFloat; override;
    function  GetValueBoolean: Boolean; override;
    function  GetValueVariant: Variant; override;
    function  GetValueIsInt: Boolean; override;
    procedure SetValueStr(const AValue: UnicodeString); override;
    procedure SetValueInt(const AValue: Int64); override;
    procedure SetValueBoolean(const AValue: Boolean); override;

  public
    constructor Create(const AValue: Int64);

    function  Clone: TJSONValue; override;
    property  Value: Int64 read FValue;
    function  Compare(const A: TJSONValue): Integer; override;
    procedure Validate(const Schema: TJSONObject); override;
  end;

  TJSONFloat = class(TJSONValue)
  private
    FValue : JSONFloat;

  protected
    procedure BuildJSONString(const A: TUnicodeStringBuilder; const AOptions: TJSONStringOptions; const ALevel: Integer); override;
    function  GetValueType: TJSONValueType; override;
    function  GetValueStr: UnicodeString; override;
    function  GetValueFloat: JSONFloat; override;
    function  GetValueVariant: Variant; override;
    function  GetValueIsFloat: Boolean; override;
    procedure SetValueStr(const AValue: UnicodeString); override;
    procedure SetValueInt(const AValue: Int64); override;
    procedure SetValueFloat(const AValue: JSONFloat); override;

  public
    constructor Create(const AValue: JSONFloat);

    function  Clone: TJSONValue; override;
    property  Value: JSONFloat read FValue;
    function  Compare(const A: TJSONValue): Integer; override;
    procedure Validate(const Schema: TJSONObject); override;
  end;

  TJSONBoolean = class(TJSONValue)
  private
    FValue : Boolean;

  protected
    procedure BuildJSONString(const A: TUnicodeStringBuilder; const AOptions: TJSONStringOptions; const ALevel: Integer); override;
    function  GetValueType: TJSONValueType; override;
    function  GetValueStr: UnicodeString; override;
    function  GetValueInt: Int64; override;
    function  GetValueBoolean: Boolean; override;
    function  GetValueVariant: Variant; override;
    function  GetValueIsBoolean: Boolean; override;
    procedure SetValueStr(const AValue: UnicodeString); override;
    procedure SetValueBoolean(const AValue: Boolean); override;

  public
    constructor Create(const AValue: Boolean);

    function  Clone: TJSONValue; override;
    property  Value: Boolean read FValue;
    function  Compare(const A: TJSONValue): Integer; override;
  end;

  TJSONNull = class(TJSONValue)
  protected
    procedure BuildJSONString(const A: TUnicodeStringBuilder; const AOptions: TJSONStringOptions; const ALevel: Integer); override;
    function  GetValueType: TJSONValueType; override;
    function  GetValueVariant: Variant; override;
    function  GetValueIsNull: Boolean; override;

  public
    function  Clone: TJSONValue; override;
    function  Compare(const A: TJSONValue): Integer; override;
  end;

  TJSONArray = class(TJSONValue)
  private
    FList : array of TJSONValue;

    function  GetCount: Integer;
    function  GetItem(const Idx: Integer): TJSONValue;

    function  GetItemAsStr(const Idx: Integer): UnicodeString;
    function  GetItemAsStrUTF8(const Idx: Integer): RawByteString;
    function  GetItemAsInt(const Idx: Integer): Int64;
    function  GetItemAsFloat(const Idx: Integer): JSONFloat;
    function  GetItemAsBoolean(const Idx: Integer): Boolean;
    function  GetItemAsArray(const Idx: Integer): TJSONArray;
    function  GetItemAsObject(const Idx: Integer): TJSONObject;
    function  GetItemAsVariant(const Idx: Integer): Variant;

    procedure SetItemAsStr(const Idx: Integer; const Value: UnicodeString);
    procedure SetItemAsStrUTF8(const Idx: Integer; const Value: RawByteString);
    procedure SetItemAsInt(const Idx: Integer; const Value: Int64);
    procedure SetItemAsFloat(const Idx: Integer; const Value: JSONFloat);
    procedure SetItemAsBoolean(const Idx: Integer; const Value: Boolean);
    procedure SetItemAsVariant(const Idx: Integer; const Value: Variant);

  protected
    procedure BuildJSONString(const A: TUnicodeStringBuilder; const AOptions: TJSONStringOptions; const ALevel: Integer); override;
    function  GetValueType: TJSONValueType; override;
    function  GetValueArray: TJSONArray; override;
    function  GetValueIsArray: Boolean; override;

  public
    constructor Create;
    constructor CreateEx(const Values: array of const);
    destructor Destroy; override;

    function  Clone: TJSONValue; override;

    procedure Clear;
    property  Count: Integer read GetCount;
    property  Item[const Idx: Integer]: TJSONValue read GetItem; default;

    function  ItemIsNull(const Idx: Integer): Boolean;
    property  ItemAsStr[const Idx: Integer]: UnicodeString read GetItemAsStr write SetItemAsStr;
    property  ItemAsStrUTF8[const Idx: Integer]: RawByteString read GetItemAsStrUTF8 write SetItemAsStrUTF8;
    property  ItemAsInt[const Idx: Integer]: Int64 read GetItemAsInt write SetItemAsInt;
    property  ItemAsFloat[const Idx: Integer]: JSONFloat read GetItemAsFloat write SetItemAsFloat;
    property  ItemAsBoolean[const Idx: Integer]: Boolean read GetItemAsBoolean write SetItemAsBoolean;
    property  ItemAsArray[const Idx: Integer]: TJSONArray read GetItemAsArray;
    property  ItemAsObject[const Idx: Integer]: TJSONObject read GetItemAsObject;
    property  ItemAsVariant[const Idx: Integer]: Variant read GetItemAsVariant write SetItemAsVariant;

    procedure Append(const A: TJSONValue);
    procedure AppendStr(const A: UnicodeString);
    procedure AppendStrUTF8(const A: RawByteString);
    procedure AppendInt(const A: Int64);
    procedure AppendFloat(const A: JSONFloat);
    procedure AppendBoolean(const A: Boolean);
    procedure AppendNull;
    procedure AppendVariant(const A : Variant);

    function  Compare(const A: TJSONValue): Integer; override;
    procedure Validate(const Schema: TJSONObject); override;

    function  GetAsStrArray: UnicodeStringArray;
    function  GetAsStrArrayUTF8: RawByteStringArray;
    function  GetAsIntArray: Int64Array;
    function  GetAsFloatArray: JSONFloatArray;
  end;

  TJSONObjectItem = record
    Name  : UnicodeString;
    Value : TJSONValue;
  end;
  PJSONObjectItem = ^TJSONObjectItem;

  TJSONObjectIterator = record
    InternalIndex : Integer;
    Item          : TJSONObjectItem;
  end;

  TJSONObject = class(TJSONValue)
  private
    FList : array of TJSONObjectItem;

    function  GetCount: Integer;
    function  GetItemIndexByName(const Name: UnicodeString; out Item: PJSONObjectItem): Integer;
    function  GetItemByName(const Name: UnicodeString): PJSONObjectItem;
    function  GetItemValueByName(const Name: UnicodeString): TJSONValue;

  protected
    procedure BuildJSONString(const A: TUnicodeStringBuilder; const AOptions: TJSONStringOptions; const ALevel: Integer); override;
    function  GetValueType: TJSONValueType; override;
    function  GetValueObject: TJSONObject; override;
    function  GetValueIsObject: Boolean; override;

  public
    constructor Create;
    destructor Destroy; override;

    function  Clone: TJSONValue; override;

    procedure Clear;
    property  Count: Integer read GetCount;
    property  Item[const Name: UnicodeString]: TJSONValue read GetItemValueByName; default;
    procedure Add(const Name: UnicodeString; const Value: TJSONValue);
    function  Exists(const Name: UnicodeString): Boolean; overload;
    function  Exists(const Name: UnicodeString; out Value: TJSONValue): Boolean; overload;

    function  ItemIsNull(const Name: UnicodeString): Boolean;
    function  GetItemAsStr(const Name: UnicodeString; const Default: UnicodeString = ''): UnicodeString;
    function  GetItemAsStrUTF8(const Name: UnicodeString; const Default: RawByteString = ''): RawByteString;
    function  GetItemAsInt(const Name: UnicodeString; const Default: Int64 = 0): Int64;
    function  GetItemAsFloat(const Name: UnicodeString; const Default: JSONFloat = 0.0): JSONFloat;
    function  GetItemAsBoolean(const Name: UnicodeString; const Default: Boolean = False): Boolean;
    function  GetItemAsArray(const Name: UnicodeString): TJSONArray;
    function  GetItemAsObject(const Name: UnicodeString): TJSONObject;
    function  GetItemAsVariant(const Name: UnicodeString): Variant;

    function  RequireItemAsArray(const Name: UnicodeString): TJSONArray;
    function  RequireItemAsObject(const Name: UnicodeString): TJSONObject;

    procedure SetItemAsStr(const Name: UnicodeString; const Value: UnicodeString);
    procedure SetItemAsStrUTF8(const Name: UnicodeString; const Value: RawByteString);
    procedure SetItemAsInt(const Name: UnicodeString; const Value: Int64);
    procedure SetItemAsFloat(const Name: UnicodeString; const Value: JSONFloat);
    procedure SetItemAsBoolean(const Name: UnicodeString; const Value: Boolean);
    procedure SetItemAsVariant(const Name: UnicodeString; const Value: Variant);

    function  Iterate(var Iterator: TJSONObjectIterator): Boolean;
    function  IterateNext(var Iterator: TJSONObjectIterator): Boolean;
    procedure IterateClose(var Iterator: TJSONObjectIterator);

    function  Compare(const A: TJSONValue): Integer; override;
    procedure Validate(const Schema: TJSONObject); override;
  end;



  { TJSONParser }

  TJSONParserToken = (
    jptNone,
    jptEOF,
    jptWhiteSpace,
    jptLeftSquare,
    jptRightSquare,
    jptLeftCurly,
    jptRightCurly,
    jptColon,
    jptComma,
    jptTrue,
    jptFalse,
    jptNull,
    jptInteger,
    jptFloat,
    jptSciFloat,
    jptString
    );

  TJSONParser = class
  private
    FText            : UnicodeString;
    FTextLength      : Integer;
    FTextPos         : Integer;
    FTextRow         : Integer;
    FTextRowPos      : Integer;
    FTextChar        : WideChar;
    FTextEOF         : Boolean;

    FToken           : TJSONParserToken;
    FTokenPos        : Integer;
    FTokenLen        : Integer;
    FTokenInteger    : Int64;
    FTokenFloat      : JSONFloat;
    FTokenStrBuilder : TUnicodeStringBuilder;
    FTokenStr        : UnicodeString;

    procedure SetText(const AText: UnicodeString);
    procedure InitParser;

    function  GetNextChar: WideChar;
    function  ExpectNextChar: WideChar;

    function  GetTokenTextPtr: PWideChar;
    function  ParseToken_UnsignedInteger(const Ch: WideChar): Int64;
    function  ParseToken_ExpectUnsignedInteger(const Ch: WideChar): Int64;
    function  ParseToken_SignedInteger(const Ch: WideChar): Int64;
    function  ParseToken_ExpectSignedInteger(const Ch: WideChar): Int64;
    function  ParseToken_Float(const Ch: WideChar): JSONFloat;
    function  ParseToken_Number(const Ch: WideChar): TJSONParserToken;
    function  ParseToken_String_Escaped_Hex4(const Ch: WideChar): WideChar;
    function  ParseToken_String_Escaped(const Ch: WideChar): WideChar;
    function  ParseToken_String(const Ch: WideChar): TJSONParserToken;
    function  GetAnyNextToken: TJSONParserToken;
    function  GetNextToken: TJSONParserToken;
    function  RequireNextToken: TJSONParserToken;

    function  ParseNamedLiteral(const AToken: TJSONParserToken): TJSONValue;
    function  ParseNumber(const AToken: TJSONParserToken): TJSONValue;
    function  ParseString: TJSONValue;
    function  ParseRequiredStringValue: UnicodeString;
    function  ParseArray: TJSONArray;
    function  ParseObject: TJSONObject;
    function  ParseValue(const AToken: TJSONParserToken): TJSONValue;
    function  ParseRequiredValue(const AToken: TJSONParserToken): TJSONValue;

  public
    constructor Create;
    destructor Destroy; override;

    function  ParseText(const AText: UnicodeString): TJSONValue;
    function  ParseTextUTF8(const AText: RawByteString): TJSONValue;
  end;

  EJSONParser = class(Exception);



{                                                                              }
{ JSON helpers                                                                 }
{                                                                              }
function  ParseJSONText(const JSONText: UnicodeString): TJSONValue;
function  ParseJSONTextUTF8(const JSONText: RawByteString): TJSONValue;

function  GetSchemaSchemaObj: TJSONObject;
procedure ValidateSchema(const Schema: TJSONObject);



{                                                                              }
{ Self-testing code                                                            }
{                                                                              }
{$IFDEF DEBUG}
{$IFDEF TEST}
procedure Test;
{$ENDIF}
{$ENDIF}



implementation

uses
  { System }
  Math,
  Variants,

  { Fundamentals }
  flcUtils,
  flcFloats,
  flcASCII,
  flcUTF;



{ JSON value helpers }

const
  SJSONBoolean : array[Boolean] of UnicodeString = ('false', 'true');
  SJSONNull = 'null';
  SJSONIndent = '    ';

  SSchemaTypeAny = 'any';
  SSchemaTypeStr : array[TJSONValueType] of UnicodeString = (
    'string',
    'integer',
    'number',
    'boolean',
    'null',
    'array',
    'object');

  SSchemaField_Type             = 'type';
  SSchemaField_Enum             = 'enum';
  SSchemaField_MinLength        = 'minLength';
  SSchemaField_MaxLength        = 'maxLength';
  SSchemaField_Minimum          = 'minimum';
  SSchemaField_Maximum          = 'maximum';
  SSchemaField_ExclusiveMinimum = 'exclusiveMinimum';
  SSchemaField_ExclusiveMaximum = 'exclusiveMaximum';
  SSchemaField_MinItems         = 'minItems';
  SSchemaField_MaxItems         = 'maxItems';
  SSchemaField_Properties       = 'properties';
  SSchemaField_Required         = 'required';
  SSchemaField_Items            = 'items';

  SErr_InvalidSchema    = 'Invalid schema: %s';
  SErr_ValidationFailed = 'Validation failed: %s';
  SErr_InvalidValueType = 'Invalid value type';
  SErr_ValueOutOfRange  = 'Value out of range';
  SErr_InvalidLength    = 'Invalid length';
  SErr_InvalidItemCount = 'Invalid item count';
  SErr_InvalidValue     = 'Invalid value';

function EscapedJSONStringValue(const S: UnicodeString): UnicodeString;
var T : UnicodeString;
begin

  T := S;
  T := StrReplaceCharStrU('\', '\\', T);
  T := StrReplaceCharStrU('/', '\/', T);
  T := StrReplaceCharStrU('"', '\"', T);
  T := StrReplaceCharStrU(WideBS, '\b', T);
  T := StrReplaceCharStrU(WideFF, '\f', T);
  T := StrReplaceCharStrU(WideLF, '\n', T);
  T := StrReplaceCharStrU(WideCR, '\r', T);
  T := StrReplaceCharStrU(WideHT, '\t', T);
  T := StrReplaceCharStrU(WideNULL, '\u0000', T);
  T := StrReplaceCharStrU(WideEOF, '\u001A', T);
  Result := T;
end;

function QuotedJSONStringValue(const S: UnicodeString): UnicodeString;
begin
  Result := '"' + EscapedJSONStringValue(S) + '"';
end;



{ TJSONValue }

function TJSONValue.Clone: TJSONValue;
begin
  raise EJSONValue.CreateFmt('%s: Clone not implemented', [ClassName]);
end;

procedure TJSONValue.BuildJSONString(const A: TUnicodeStringBuilder; const AOptions: TJSONStringOptions; const ALevel: Integer);
begin
  raise EJSONValue.CreateFmt(' %s: Conversion to JSON string not implemented', [ClassName]);
end;

function TJSONValue.GetJSONString(const AOptions: TJSONStringOptions): UnicodeString;
var B : TUnicodeStringBuilder;
begin
  B := TUnicodeStringBuilder.Create;
  try
    BuildJSONString(B, AOptions, 0);
    Result := B.AsUnicodeString;
  finally
    B.Free;
  end;
end;

function TJSONValue.GetJSONStringUTF8(const AOptions: TJSONStringOptions): RawByteString;
begin
  Result := UnicodeStringToUTF8String(GetJSONString(AOptions));
end;

function TJSONValue.GetValueType: TJSONValueType;
begin
  raise EJSONValue.CreateFmt('%s: GetValueType not implemented', [ClassName]);
end;

function TJSONValue.GetValueStr: UnicodeString;
begin
  raise EJSONValue.CreateFmt('%s: Conversion of value to %s not supported', [ClassName, 'string']);
end;

function TJSONValue.GetValueStrUTF8: RawByteString;
begin
  Result := UnicodeStringToUTF8String(GetValueStr);
end;

function TJSONValue.GetValueInt: Int64;
begin
  raise EJSONValue.CreateFmt('%s: Conversion of value to %s not supported',
      [ClassName, 'integer']);
end;

function TJSONValue.GetValueFloat: JSONFloat;
begin
  raise EJSONValue.CreateFmt('%s: Conversion of value to %s not supported',
      [ClassName, 'float']);
end;

function TJSONValue.GetValueBoolean: Boolean;
begin
  raise EJSONValue.CreateFmt('%s: Conversion of value to %s not supported',
      [ClassName, 'boolean']);
end;

function TJSONValue.GetValueObject: TJSONObject;
begin
  raise EJSONValue.CreateFmt('%s: Conversion of value to %s not supported',
      [ClassName, 'object']);
end;

function TJSONValue.GetValueArray: TJSONArray;
begin
  raise EJSONValue.CreateFmt('%s: Conversion of value to %s not supported',
      [ClassName, 'array']);
end;

function TJSONValue.GetValueVariant: Variant;
begin
  case GetValueType of
    jvtString  : Result := GetValueStr;
    jvtInteger : Result := GetValueInt;
    jvtFloat   : Result := GetValueFloat;
    jvtBoolean : Result := GetValueBoolean;
    jvtNull    : Result := Null;
  else
    raise EJSONValue.CreateFmt('%s: Conversion of value to %s not supported',
        [ClassName, 'variant']);
  end;
end;

procedure TJSONValue.SetValueStr(const AValue: UnicodeString);
begin
  raise EJSONValue.CreateFmt('%s: Conversion from %s value not supported',
      [ClassName, 'string']);
end;

procedure TJSONValue.SetValueStrUTF8(const AValue: RawByteString);
begin
  SetValueStr(UTF8StringToUnicodeString(AValue));
end;

procedure TJSONValue.SetValueInt(const AValue: Int64);
begin
  raise EJSONValue.CreateFmt('%s: Conversion from %s value not supported',
      [ClassName, 'integer']);
end;

procedure TJSONValue.SetValueFloat(const AValue: JSONFloat);
begin
  raise EJSONValue.CreateFmt('%s: Conversion from %s value not supported',
      [ClassName, 'float']);
end;

procedure TJSONValue.SetValueBoolean(const AValue: Boolean);
begin
  raise EJSONValue.CreateFmt('%s: Conversion from %s value not supported',
      [ClassName, 'boolean']);
end;

procedure TJSONValue.SetValueVariant(const AValue: Variant);
begin
  case VarType(AValue) of
    varByte,
    varWord,
    varLongWord,
    varSmallint,
    varShortInt,
    varInteger,
    varInt64    : SetValueInt(AValue);
    varSingle,
    varDouble,
    varCurrency : SetValueFloat(AValue);
    varString   : SetValueStr(AValue);
    varBoolean  : SetValueBoolean(AValue);
  else
    raise EJSONValue.CreateFmt('%s: Conversion from %s value not supported',
        [ClassName, 'variant']);
  end;
end;

function TJSONValue.GetValueIsStr: Boolean;
begin
  Result := GetValueType = jvtString;
end;

function TJSONValue.GetValueIsInt: Boolean;
begin
  Result := GetValueType = jvtInteger;
end;

function TJSONValue.GetValueIsFloat: Boolean;
begin
  Result := GetValueType = jvtFloat;
end;

function TJSONValue.GetValueIsBoolean: Boolean;
begin
  Result := GetValueType = jvtBoolean;
end;

function TJSONValue.GetValueIsNull: Boolean;
begin
  Result := GetValueType = jvtNull;
end;

function TJSONValue.GetValueIsArray: Boolean;
begin
  Result := GetValueType = jvtArray;
end;

function TJSONValue.GetValueIsObject: Boolean;
begin
  Result := GetValueType = jvtObject;
end;

function TJSONValue.Compare(const A: TJSONValue): Integer; 
begin
  Assert(Assigned(A));
  raise EJSONValue.CreateFmt('%s: Compare with %s not implemented',
      [ClassName, A.ClassName]);
end;

procedure TJSONValue.Validate(const Schema: TJSONObject);
var A, P : TJSONValue;
    B : TJSONArray;
    I : Integer;
    T : UnicodeString;
    R : Boolean;
    S : UnicodeString;
begin
  if not Assigned(Schema) then
    exit;
  A := Schema[SSchemaField_Type];
  if Assigned(A) then
    begin
      if A is TJSONArray then
        begin
          B := TJSONArray(A);
          R := False;
          S := SSchemaTypeStr[GetValueType];
          for I := 0 to B.GetCount - 1 do
            begin
              P := B.GetItem(I);
              if P is TJSONString then
                begin
                  T := TJSONString(P).Value;
                  if T <> '' then
                    if (T = SSchemaTypeAny) or (T = S) then
                      begin
                        R := True;
                        break;
                      end;
                end;
            end;
          if not R then
            raise EJSONSchema.CreateFmt(SErr_ValidationFailed, [SErr_InvalidValueType])
        end else
      if A is TJSONString then
        begin
          T := A.ValueStr;
          if (T <> '') and (T <> SSchemaTypeAny) then
            begin
              S := SSchemaTypeStr[GetValueType];
              if T <> S then
                raise EJSONSchema.CreateFmt(SErr_ValidationFailed, [SErr_InvalidValueType])
            end;
        end
      else
        raise EJSONSchema.CreateFmt(SErr_InvalidSchema, ['Invalid value type for "type" field']);
    end;
  A := Schema[SSchemaField_Enum];
  if Assigned(A) then
    begin
      if not (A is TJSONArray) then
        raise EJSONSchema.CreateFmt(SErr_InvalidSchema, ['Invalid value type for "enum" field']);
      B := TJSONArray(A);
      R := False;
      for I := 0 to B.GetCount - 1 do
        if Compare(B.GetItem(I)) = 0 then
          begin
            R := True;
            break;
          end;
      if not R then
        raise EJSONSchema.CreateFmt(SErr_ValidationFailed, [SErr_InvalidValue]);
    end;
end;



{ TJSONString }

constructor TJSONString.Create(const AValue: UnicodeString);
begin
  inherited Create;
  FValue := AValue;
end;

constructor TJSONString.CreateUTF8(const AValue: RawByteString);
begin
  inherited Create;
  SetValueStrUTF8(AValue);
end;

function TJSONString.Clone: TJSONValue;
begin
  Result := TJSONString.Create(FValue);
end;

procedure TJSONString.BuildJSONString(const A: TUnicodeStringBuilder; const AOptions: TJSONStringOptions; const ALevel: Integer);
begin
  A.AppendCh(WideChar('"'));
  A.Append(EscapedJSONStringValue(FValue));
  A.AppendCh(WideChar('"'));
end;

function TJSONString.GetValueType: TJSONValueType;
begin
  Result := jvtString;
end;

function TJSONString.GetValueStr: UnicodeString;
begin
  Result := FValue;
end;

function TJSONString.GetValueInt: Int64;
begin
  Result := StrToInt64(FValue);
end;

function TJSONString.GetValueFloat: JSONFloat;
begin
  Result := StringToFloatU(FValue);
end;

function TJSONString.GetValueBoolean: Boolean;
begin
  if FValue = SJSONBoolean[False] then
    Result := False
  else
  if FValue = SJSONBoolean[True] then
    Result := True
  else
    raise EJSONValue.Create(SErr_InvalidValue);
end;

function TJSONString.GetValueVariant: Variant;
begin
  Result := GetValueBoolean;
end;

function TJSONString.GetValueIsStr: Boolean;
begin
  Result := True;
end;

procedure TJSONString.SetValueStr(const AValue: UnicodeString);
begin
  FValue := AValue;
end;

procedure TJSONString.SetValueInt(const AValue: Int64);
begin
  FValue := IntToStr(AValue);
end;

procedure TJSONString.SetValueFloat(const AValue: JSONFloat);
begin
  FValue := FloatToStringU(AValue);
end;

procedure TJSONString.SetValueBoolean(const AValue: Boolean);
begin
  FValue := SJSONBoolean[AValue];
end;

function TJSONString.Compare(const A: TJSONValue): Integer;
begin
  if A is TJSONString then
    Result := StrCompareU(FValue, TJSONString(A).FValue)
  else
    Result := inherited Compare(A);
end;

procedure TJSONString.Validate(const Schema: TJSONObject);
var A : TJSONValue;
begin
  inherited Validate(Schema);
  A := Schema[SSchemaField_MinLength];
  if Assigned(A) then
    if not A.ValueIsInt then
      raise EJSONValue.CreateFmt(SErr_InvalidSchema, ['Invalid value type for "minLength" field'])
    else
      if Length(FValue) < A.ValueInt then
        raise EJSONSchema.CreateFmt(SErr_ValidationFailed, [SErr_InvalidLength]);
  A := Schema[SSchemaField_MaxLength];
  if Assigned(A) then
    if not A.ValueIsInt then
      raise EJSONValue.CreateFmt(SErr_InvalidSchema, ['Invalid value type for "maxLength" field'])
    else
      if Length(FValue) > A.ValueInt then
        raise EJSONSchema.CreateFmt(SErr_ValidationFailed, [SErr_InvalidLength]);
end;



{ TJSONInteger }

constructor TJSONInteger.Create(const AValue: Int64);
begin
  inherited Create;
  FValue := AValue;
end;

function TJSONInteger.Clone: TJSONValue;
begin
  Result := TJSONInteger.Create(FValue);
end;

procedure TJSONInteger.BuildJSONString(const A: TUnicodeStringBuilder; const AOptions: TJSONStringOptions; const ALevel: Integer);
begin
  A.Append(IntToStr(FValue));
end;

function TJSONInteger.GetValueType: TJSONValueType;
begin
  Result := jvtInteger;
end;

function TJSONInteger.GetValueStr: UnicodeString;
begin
  Result := IntToStr(FValue);
end;

function TJSONInteger.GetValueInt: Int64;
begin
  Result := FValue;
end;

function TJSONInteger.GetValueFloat: JSONFloat;
begin
  Result := FValue;
end;

function TJSONInteger.GetValueBoolean: Boolean;
begin
  Result := FValue <> 0;
end;

function TJSONInteger.GetValueVariant: Variant;
begin
  Result := GetValueInt;
end;

function TJSONInteger.GetValueIsInt: Boolean;
begin
  Result := True;
end;

procedure TJSONInteger.SetValueStr(const AValue: UnicodeString);
begin
  FValue := StrToInt64(AValue);
end;

procedure TJSONInteger.SetValueInt(const AValue: Int64);
begin
  FValue := AValue;
end;

procedure TJSONInteger.SetValueBoolean(const AValue: Boolean);
begin
  FValue := Ord(AValue);
end;

function TJSONInteger.Compare(const A: TJSONValue): Integer;
begin
  if A is TJSONInteger then
    begin
      if FValue > TJSONInteger(A).FValue then
        Result := 1 else
      if FValue < TJSONInteger(A).FValue then
        Result := -1
      else
        Result := 0;
    end else
  if A is TJSONFloat then
    begin
      if FValue > TJSONFloat(A).FValue then
        Result := 1 else
      if FValue < TJSONFloat(A).FValue then
        Result := -1
      else
        Result := 0;
    end 
  else
    Result := inherited Compare(A);
end;

procedure TJSONInteger.Validate(const Schema: TJSONObject);
var A : TJSONValue;
    B : TJSONValue;
    R : Boolean;
begin
  inherited Validate(Schema);
  A := Schema[SSchemaField_Minimum];
  if Assigned(A) then
    begin
      B := Schema[SSchemaField_ExclusiveMinimum];
      if Assigned(B) then
        R := B.ValueBoolean
      else
        R := False;
      if (not R and (FValue <  A.ValueInt)) or
         (R     and (FValue <= A.ValueInt)) then
        raise EJSONSchema.CreateFmt(SErr_ValidationFailed, [SErr_ValueOutOfRange]);
    end;
  A := Schema[SSchemaField_Maximum];
  if Assigned(A) then
    begin
      B := Schema[SSchemaField_ExclusiveMaximum];
      if Assigned(B) then
        R := B.ValueBoolean
      else
        R := False;
      if (not R and (FValue >  A.ValueInt)) or
         (R     and (FValue >= A.ValueInt)) then
        raise EJSONSchema.CreateFmt(SErr_ValidationFailed, [SErr_ValueOutOfRange]);
    end;
end;



{ TJSONFloat }

constructor TJSONFloat.Create(const AValue: JSONFloat);
begin
  inherited Create;
  FValue := AValue;
end;

function TJSONFloat.Clone: TJSONValue;
begin
  Result := TJSONFloat.Create(FValue);
end;

procedure TJSONFloat.BuildJSONString(const A: TUnicodeStringBuilder; const AOptions: TJSONStringOptions; const ALevel: Integer);
begin
  A.Append(FloatToStringU(FValue));
end;

function TJSONFloat.GetValueType: TJSONValueType;
begin
  Result := jvtFloat;
end;

function TJSONFloat.GetValueStr: UnicodeString;
begin
  Result := FloatToStr(FValue);
end;

function TJSONFloat.GetValueFloat: JSONFloat;
begin
  Result := FValue;
end;

function TJSONFloat.GetValueVariant: Variant;
begin
  Result := GetValueFloat;
end;

function TJSONFloat.GetValueIsFloat: Boolean;
begin
  Result := True;
end;

procedure TJSONFloat.SetValueStr(const AValue: UnicodeString);
begin
  FValue := StringToFloatU(AValue);
end;

procedure TJSONFloat.SetValueInt(const AValue: Int64);
begin
  FValue := AValue;
end;

procedure TJSONFloat.SetValueFloat(const AValue: JSONFloat);
begin
  FValue := AValue;
end;

function TJSONFloat.Compare(const A: TJSONValue): Integer;
begin
  if A is TJSONFloat then
    begin
      if FValue > TJSONFloat(A).FValue then
        Result := 1 else
      if FValue < TJSONFloat(A).FValue then
        Result := -1
      else
        Result := 0;
    end else
  if A is TJSONInteger then
    begin
      if FValue > TJSONInteger(A).FValue then
        Result := 1 else
      if FValue < TJSONInteger(A).FValue then
        Result := -1
      else
        Result := 0;
    end
  else
    Result := inherited Compare(A);
end;

procedure TJSONFloat.Validate(const Schema: TJSONObject);
var A : TJSONValue;
    B : TJSONValue;
    R : Boolean;
begin
  inherited Validate(Schema);
  A := Schema[SSchemaField_Minimum];
  if Assigned(A) then
    begin
      B := Schema[SSchemaField_ExclusiveMinimum];
      if Assigned(B) then
        R := B.ValueBoolean
      else
        R := False;
      if (not R and (FValue <  A.ValueFloat)) or
         (R     and (FValue <= A.ValueFloat)) then
        raise EJSONSchema.CreateFmt(SErr_ValidationFailed, [SErr_ValueOutOfRange]);
    end;
  A := Schema[SSchemaField_Maximum];
  if Assigned(A) then
    begin
      B := Schema[SSchemaField_ExclusiveMaximum];
      if Assigned(B) then
        R := B.ValueBoolean
      else
        R := False;
      if (not R and (FValue >  A.ValueFloat)) or
         (R     and (FValue >= A.ValueFloat)) then
        raise EJSONSchema.CreateFmt(SErr_ValidationFailed, [SErr_ValueOutOfRange]);
    end;
end;



{ TJSONBoolean }

constructor TJSONBoolean.Create(const AValue: Boolean);
begin
  inherited Create;
  FValue := AValue;
end;

function TJSONBoolean.Clone: TJSONValue;
begin
  Result := TJSONBoolean.Create(FValue);
end;

procedure TJSONBoolean.BuildJSONString(const A: TUnicodeStringBuilder; const AOptions: TJSONStringOptions; const ALevel: Integer);
begin
  A.Append(SJSONBoolean[FValue]);
end;

function TJSONBoolean.GetValueType: TJSONValueType;
begin
  Result := jvtBoolean;
end;

function TJSONBoolean.GetValueStr: UnicodeString;
begin
  Result := SJSONBoolean[FValue];
end;

function TJSONBoolean.GetValueInt: Int64;
begin
  Result := Ord(FValue);
end;

function TJSONBoolean.GetValueBoolean: Boolean;
begin
  Result := FValue;
end;

function TJSONBoolean.GetValueVariant: Variant;
begin
  Result := GetValueBoolean;
end;

function TJSONBoolean.GetValueIsBoolean: Boolean;
begin
  Result := True;
end;

procedure TJSONBoolean.SetValueStr(const AValue: UnicodeString);
begin
  if AValue = SJSONBoolean[False] then
    FValue := False
  else
  if AValue = SJSONBoolean[True] then
    FValue := True
  else
    raise EJSONValue.Create(SErr_InvalidValue);
end;

procedure TJSONBoolean.SetValueBoolean(const AValue: Boolean);
begin
  FValue := AValue;
end;

function TJSONBoolean.Compare(const A: TJSONValue): Integer;
begin
  if A is TJSONBoolean then
    begin
      if FValue > TJSONBoolean(A).FValue then
        Result := 1 else
      if FValue < TJSONBoolean(A).FValue then
        Result := -1
      else
        Result := 0;
    end
  else
    Result := inherited Compare(A);
end;



{ TJSONNull }

function TJSONNull.Clone: TJSONValue;
begin
  Result := TJSONNull.Create;
end;

procedure TJSONNull.BuildJSONString(const A: TUnicodeStringBuilder; const AOptions: TJSONStringOptions; const ALevel: Integer);
begin
  A.Append(SJSONNull);
end;

function TJSONNull.GetValueType: TJSONValueType;
begin
  Result := jvtNull;
end;

function TJSONNull.GetValueVariant: Variant;
begin
  Result := Null;
end;

function TJSONNull.GetValueIsNull: Boolean;
begin
  Result := True;
end;

function TJSONNull.Compare(const A: TJSONValue): Integer;
begin
  if A is TJSONNull then
    Result := 0
  else
    Result := -1;
end;



{ TVarRec }

function VarRecToJSONValue(const Value: TVarRec): TJSONValue;
begin
  case Value.VType of
    System.vtInteger       : Result := TJSONInteger.Create(Value.VInteger);
    System.vtBoolean       : Result := TJSONBoolean.Create(Value.VBoolean);
    System.vtChar          : Result := TJSONString.Create(ToStringChB(Value.VChar));
    System.vtExtended      : Result := TJSONFloat.Create(Value.VExtended^);
    {$IFDEF SupportAnsiString}
    System.vtString        : Result := TJSONString.Create(String(Value.VString^));
    {$ENDIF}
    System.vtPointer       : if not Assigned(Value.VPointer) then
                               Result := TJSONNull.Create
                             else
                               raise EJSONValue.Create('VarRec value not supported');
    {$IFDEF SupportAnsiString}
    System.vtPChar         : Result := TJSONString.Create(ToStringA(Value.VPChar^));
    {$ENDIF}
    System.vtWideChar      : Result := TJSONString.Create(ToStringChW(Value.VWideChar));
    System.vtPWideChar     : Result := TJSONString.Create(Value.VPWideChar^);
    {$IFDEF SupportAnsiString}
    System.vtAnsiString    : Result := TJSONString.Create(ToStringA(AnsiString(Value.VAnsiString)));
    {$ENDIF}
    System.vtCurrency      : Result := TJSONFloat.Create(Value.VCurrency^);
    {$IFDEF SupportWideString}
    System.vtWideString    : Result := TJSONString.Create(UnicodeString(WideString(Value.VWideString)));
    {$ENDIF}
    System.vtInt64         : Result := TJSONInteger.Create(Value.VInt64^);
    {$IFDEF SupportUnicodeString}
    System.vtUnicodeString : Result := TJSONString.Create(UnicodeString(Value.VUnicodeString));
    {$ENDIF}
  else
    raise EJSONValue.Create('VarRec value type not supported');
  end;
end;



{ TJSONArray }

constructor TJSONArray.Create;
begin
  inherited Create;
end;

constructor TJSONArray.CreateEx(const Values: array of const);
var L, I : Integer;
begin
  inherited Create;
  L := Length(Values);
  SetLength(FList, L);
  for I := 0 to L - 1 do
    FList[I] := nil;
  for I := 0 to L - 1 do
    FList[I] := VarRecToJSONValue(Values[I]);
end;

destructor TJSONArray.Destroy;
var I : Integer;
begin
  for I := Length(FList) - 1 downto 0 do
    FreeAndNil(FList[I]);
  inherited Destroy;
end;

function TJSONArray.Clone: TJSONValue;
var A : TJSONArray;
    I : Integer;
begin
  A := TJSONArray.Create;
  try
    for I := 0 to Length(FList) - 1 do
      A.Append(FList[I].Clone);
  except
    A.Free;
    raise;
  end;
  Result := A;
end;

procedure TJSONArray.Clear;
var I : Integer;
begin
  for I := Length(FList) - 1 downto 0 do
    FreeAndNil(FList[I]);
  FList := nil;
end;

function TJSONArray.GetValueType: TJSONValueType;
begin
  Result := jvtArray;
end;

function TJSONArray.GetValueArray: TJSONArray;
begin
  Result := self;
end;

function TJSONArray.GetValueIsArray: Boolean;
begin
  Result := True;
end;

function TJSONArray.GetCount: Integer;
begin
  Result := Length(FList);
end;

function TJSONArray.GetItem(const Idx: Integer): TJSONValue;
begin
  Assert(Idx >= 0);
  Assert(Idx < Length(FList));

  Result := FList[Idx];
end;

function TJSONArray.ItemIsNull(const Idx: Integer): Boolean;
begin
  Result := GetItem(Idx).ValueIsNull;
end;

function TJSONArray.GetItemAsStr(const Idx: Integer): UnicodeString;
begin
  Result := GetItem(Idx).GetValueStr;
end;

function TJSONArray.GetItemAsStrUTF8(const Idx: Integer): RawByteString;
begin
  Result := GetItem(Idx).GetValueStrUTF8;
end;

function TJSONArray.GetItemAsInt(const Idx: Integer): Int64;
begin
  Result := GetItem(Idx).GetValueInt;
end;

function TJSONArray.GetItemAsFloat(const Idx: Integer): JSONFloat;
begin
  Result := GetItem(Idx).GetValueFloat;
end;

function TJSONArray.GetItemAsBoolean(const Idx: Integer): Boolean;
begin
  Result := GetItem(Idx).GetValueBoolean;
end;

function TJSONArray.GetItemAsArray(const Idx: Integer): TJSONArray;
begin
  Result := GetItem(Idx).GetValueArray;
end;

function TJSONArray.GetItemAsObject(const Idx: Integer): TJSONObject;
begin
  Result := GetItem(Idx).GetValueObject;
end;

function TJSONArray.GetItemAsVariant(const Idx: Integer): Variant;
begin
  Result := GetItem(Idx).GetValueVariant;
end;

procedure TJSONArray.SetItemAsStr(const Idx: Integer; const Value: UnicodeString);
begin
  GetItem(Idx).SetValueStr(Value);
end;

procedure TJSONArray.SetItemAsStrUTF8(const Idx: Integer; const Value: RawByteString);
begin
  GetItem(Idx).SetValueStrUTF8(Value);
end;

procedure TJSONArray.SetItemAsInt(const Idx: Integer; const Value: Int64);
begin
  GetItem(Idx).SetValueInt(Value);
end;

procedure TJSONArray.SetItemAsFloat(const Idx: Integer; const Value: JSONFloat);
begin
  GetItem(Idx).SetValueFloat(Value);
end;

procedure TJSONArray.SetItemAsBoolean(const Idx: Integer; const Value: Boolean);
begin
  GetItem(Idx).SetValueBoolean(Value);
end;

procedure TJSONArray.SetItemAsVariant(const Idx: Integer; const Value: Variant);
begin
  GetItem(Idx).SetValueVariant(Value);
end;

procedure TJSONArray.Append(const A: TJSONValue);
var L : Integer;
begin
  Assert(Assigned(A));

  L := Length(FList);
  SetLength(FList, L + 1);
  FList[L] := A;
end;

procedure TJSONArray.AppendStr(const A: UnicodeString);
begin
  Append(TJSONString.Create(A));
end;

procedure TJSONArray.AppendStrUTF8(const A: RawByteString);
begin
  Append(TJSONString.CreateUTF8(A));
end;

procedure TJSONArray.AppendInt(const A: Int64);
begin
  Append(TJSONInteger.Create(A));
end;

procedure TJSONArray.AppendFloat(const A: JSONFloat);
begin
  Append(TJSONFloat.Create(A));
end;

procedure TJSONArray.AppendBoolean(const A: Boolean);
begin
  Append(TJSONBoolean.Create(A));
end;

procedure TJSONArray.AppendNull;
begin
  Append(TJSONNull.Create);
end;

procedure TJSONArray.AppendVariant(const A : Variant);
begin
  case VarType(A) of
    varByte,
    varWord,
    varLongWord,
    varSmallint,
    varShortInt,
    varInteger,
    varInt64    : AppendInt(A);
    varSingle,
    varDouble,
    varCurrency : AppendFloat(A);
    varString   : AppendStr(A);
    varBoolean  : AppendBoolean(A);
    varNull     : AppendNull;
  else
    raise EJSONValue.Create('Variant type not supported');
  end;
end;

procedure TJSONArray.BuildJSONString(const A: TUnicodeStringBuilder; const AOptions: TJSONStringOptions; const ALevel: Integer);
var I, L, N : Integer;
    F : Boolean;
begin
  F := jboIndent in AOptions;
  A.AppendCh(WideChar('['));
  L := ALevel + 1;
  N := Length(FList);
  for I := 0 to N - 1 do
    begin
      if I > 0 then
        A.AppendCh(WideChar(','));
      if F then
        begin
          A.Append(WideCRLF);
          A.Append(SJSONIndent, L);
        end;
      FList[I].BuildJSONString(A, AOptions, L);
    end;
  if F and (N > 0) then
    begin
      A.Append(WideCRLF);
      A.Append(SJSONIndent, ALevel);
    end;
  A.AppendCh(WideChar(']'));
  if F and (N > 0) then
    begin
      A.Append(WideCRLF);
      A.Append(SJSONIndent, ALevel);
    end;
end;

function TJSONArray.Compare(const A: TJSONValue): Integer;
var B : TJSONArray;
    L, N, I, C : Integer;
begin
  if A is TJSONArray then
    begin
      B := TJSONArray(A);
      L := GetCount;
      N := B.GetCount;
      if L < N then
        Result := -1 else
      if L > N then
        Result := 1
      else
        begin
          for I := 0 to L - 1 do
            begin
              C := GetItem(I).Compare(B.GetItem(I));
              if C <> 0 then
                begin
                  Result := C;
                  exit;
                end;
            end;
          Result := 0;
        end;
    end
  else
    Result := inherited Compare(A);
end;

procedure TJSONArray.Validate(const Schema: TJSONObject);
var A, C : TJSONValue;
    B : TJSONArray;
    I, L, N : Integer;
begin
  inherited Validate(Schema);
  A := Schema[SSchemaField_MinItems];
  if Assigned(A) then
    if not A.ValueIsInt then
      raise EJSONValue.CreateFmt(SErr_InvalidSchema, ['Invalid value type for "minItems" field'])
    else
      if GetCount < A.ValueInt then
        raise EJSONSchema.CreateFmt(SErr_ValidationFailed, [SErr_InvalidItemCount]);
  A := Schema[SSchemaField_MaxItems];
  if Assigned(A) then
    if not A.ValueIsInt then
      raise EJSONValue.CreateFmt(SErr_InvalidSchema, ['Invalid value type for "maxItems" field'])
    else
      if GetCount > A.ValueInt then
        raise EJSONSchema.CreateFmt(SErr_ValidationFailed, [SErr_InvalidItemCount]);
  A := Schema[SSchemaField_Items];
  if Assigned(A) then
    if A is TJSONArray then
      begin
        B := TJSONArray(A);
        L := Length(FList);
        N := Length(B.FList);
        for I := 0 to Min(L, N) - 1 do
          begin
            C := B.FList[I];
            if not (C is TJSONObject) then
              raise EJSONSchema.CreateFmt(SErr_InvalidSchema, ['Invalid value type for item in "Items" field: Expected schema object'])
            else
              try
                FList[I].Validate(TJSONObject(C));
              except
                on E : Exception do
                  raise EJSONSchema.Create(E.Message + ': Item [' + IntToStr(I) + ']');
              end;
          end;
      end else
    if A is TJSONObject then
      begin
        for I := 0 to Length(FList) - 1 do
          try
            FList[I].Validate(TJSONObject(A));
          except
            on E : Exception do
              raise EJSONSchema.Create(E.Message + ': Item [' + IntToStr(I) + ']');
          end;
      end
    else
      raise EJSONSchema.CreateFmt(SErr_InvalidSchema, ['Invalid value type for "items" field']);
end;

function TJSONArray.GetAsStrArray: UnicodeStringArray;
var L, I : Integer;
begin
  L := Length(FList);
  SetLength(Result, L);
  for I := 0 to L - 1 do
    Result[I] := GetItemAsStr(I);
end;

function TJSONArray.GetAsStrArrayUTF8: RawByteStringArray;
var L, I : Integer;
begin
  L := Length(FList);
  SetLength(Result, L);
  for I := 0 to L - 1 do
    Result[I] := GetItemAsStrUTF8(I);
end;

function TJSONArray.GetAsIntArray: Int64Array;
var L, I : Integer;
begin
  L := Length(FList);
  SetLength(Result, L);
  for I := 0 to L - 1 do
    Result[I] := GetItemAsInt(I);
end;

function TJSONArray.GetAsFloatArray: JSONFloatArray;
var L, I : Integer;
begin
  L := Length(FList);
  SetLength(Result, L);
  for I := 0 to L - 1 do
    Result[I] := GetItemAsFloat(I);
end;



{ TJSONObject helpers }

procedure ClearJSONObjectItem(var Item: TJSONObjectItem);
begin
  Item.Name := '';
  Item.Value := nil;
end;



{ TJSONObject }

constructor TJSONObject.Create;
begin
  inherited Create;
end;

destructor TJSONObject.Destroy;
var I : Integer;
begin
  for I := 0 to Length(FList) - 1 do
    FreeAndNil(FList[I].Value);
  inherited Destroy;
end;

function TJSONObject.Clone: TJSONValue;
var A : TJSONObject;
    I : Integer;
    E : PJSONObjectItem;
begin
  A := TJSONObject.Create;
  try
    for I := 0 to Length(FList) - 1 do
      begin
        E := @FList[I];
        A.Add(E^.Name, E^.Value.Clone);
      end;
  except
    A.Free;
    raise;
  end;
  Result := A;
end;

procedure TJSONObject.Clear;
var I : Integer;
begin
  for I := 0 to Length(FList) - 1 do
    FreeAndNil(FList[I].Value);
  FList := nil;
end;

function TJSONObject.GetValueType: TJSONValueType;
begin
  Result := jvtObject;
end;

function TJSONObject.GetValueObject: TJSONObject;
begin
  Result := self;
end;

function TJSONObject.GetValueIsObject: Boolean;
begin
  Result := True;
end;

function TJSONObject.GetCount: Integer;
begin
  Result := Length(FList);
end;

function TJSONObject.GetItemIndexByName(const Name: UnicodeString; out Item: PJSONObjectItem): Integer;
var I : Integer;
    E : PJSONObjectItem;
begin
  for I := 0 to Length(FList) - 1 do
    begin
      E := @FList[I];
      if E^.Name = Name then
        begin
          Item := E;
          Result := I;
          exit;
        end;
    end;
  Item := nil;
  Result := -1;
end;

function TJSONObject.GetItemByName(const Name: UnicodeString): PJSONObjectItem;
begin
  GetItemIndexByName(Name, Result);
end;

function TJSONObject.GetItemValueByName(const Name: UnicodeString): TJSONValue;
var I : PJSONObjectItem;
begin
  I := GetItemByName(Name);
  if not Assigned(I) then
    Result := nil
  else
    Result := I^.Value;
end;

procedure TJSONObject.Add(const Name: UnicodeString; const Value: TJSONValue);
var L : Integer;
begin
  L := Length(FList);
  SetLength(FList, L + 1);
  FList[L].Name := Name;
  FList[L].Value := Value;
end;

function TJSONObject.Exists(const Name: UnicodeString): Boolean;
begin
  Result := Assigned(GetItemByName(Name));
end;

function TJSONObject.Exists(const Name: UnicodeString; out Value: TJSONValue): Boolean;
begin
  Value := GetItemValueByName(Name);
  Result := Assigned(Value);
end;

function TJSONObject.ItemIsNull(const Name: UnicodeString): Boolean;
var I : TJSONValue;
begin
  I := GetItemValueByName(Name);
  if not Assigned(I) then
    Result := False
  else
    Result := I.ValueIsNull;
end;

function TJSONObject.GetItemAsStr(const Name: UnicodeString; const Default: UnicodeString): UnicodeString;
var I : TJSONValue;
begin
  I := GetItemValueByName(Name);
  if not Assigned(I) then
    Result := Default
  else
    if I.ValueIsNull then
      Result := Default
    else
      try
        Result := I.ValueStr;
      except
        Result := Default;
      end;
end;

function TJSONObject.GetItemAsStrUTF8(const Name: UnicodeString; const Default: RawByteString): RawByteString;
var I : TJSONValue;
begin
  I := GetItemValueByName(Name);
  if not Assigned(I) then
    Result := Default
  else
    if I.ValueIsNull then
      Result := Default
    else
      try
        Result := I.ValueStrUTF8;
      except
        Result := Default;
      end;
end;

function TJSONObject.GetItemAsInt(const Name: UnicodeString; const Default: Int64): Int64;
var I : TJSONValue;
begin
  I := GetItemValueByName(Name);
  if not Assigned(I) then
    Result := Default
  else
    if I.ValueIsNull then
      Result := Default
    else
      try
        Result := I.ValueInt;
      except
        Result := Default;
      end;
end;

function TJSONObject.GetItemAsFloat(const Name: UnicodeString; const Default: JSONFloat): JSONFloat;
var I : TJSONValue;
begin
  I := GetItemValueByName(Name);
  if not Assigned(I) then
    Result := Default
  else
    if I.ValueIsNull then
      Result := Default
    else
      try
        Result := I.ValueFloat;
      except
        Result := Default;
      end;
end;

function TJSONObject.GetItemAsBoolean(const Name: UnicodeString; const Default: Boolean): Boolean;
var I : TJSONValue;
begin
  I := GetItemValueByName(Name);
  if not Assigned(I) then
    Result := Default
  else
    if I.ValueIsNull then
      Result := Default
    else
      try
        Result := I.ValueBoolean;
      except
        Result := Default;
      end;
end;

function TJSONObject.GetItemAsArray(const Name: UnicodeString): TJSONArray;
var I : TJSONValue;
begin
  I := GetItemValueByName(Name);
  if not Assigned(I) then
    Result := nil
  else
  if not (I is TJSONArray) then
    Result := nil
  else
    Result := TJSONArray(I);
end;

function TJSONObject.GetItemAsObject(const Name: UnicodeString): TJSONObject;
var I : TJSONValue;
begin
  I := GetItemValueByName(Name);
  if not Assigned(I) then
    Result := nil
  else
  if not (I is TJSONObject) then
    Result := nil
  else
    Result := TJSONObject(I);
end;

function TJSONObject.GetItemAsVariant(const Name: UnicodeString): Variant;
var I : TJSONValue;
begin
  I := GetItemValueByName(Name);
  if not Assigned(I) then
    Result := Unassigned
  else
    Result := I.ValueVariant;
end;

function TJSONObject.RequireItemAsArray(const Name: UnicodeString): TJSONArray;
var I : TJSONValue;
    V : TJSONArray;
begin
  I := GetItemValueByName(Name);
  if not Assigned(I) then
    begin
      V := TJSONArray.Create;
      Add(Name, V);
      Result := V;
    end
  else
  if not (I is TJSONArray) then
    raise EJSONValue.Create('Not an array value')
  else
    Result := TJSONArray(I);
end;

function TJSONObject.RequireItemAsObject(const Name: UnicodeString): TJSONObject;
var I : TJSONValue;
    V : TJSONObject;
begin
  I := GetItemValueByName(Name);
  if not Assigned(I) then
    begin
      V := TJSONObject.Create;
      Add(Name, V);
      Result := V;
    end
  else
  if not (I is TJSONObject) then
    raise EJSONValue.Create('Not an object value')
  else
    Result := TJSONObject(I);
end;

procedure TJSONObject.SetItemAsStr(const Name: UnicodeString; const Value: UnicodeString);
var I : TJSONValue;
begin
  I := GetItemValueByName(Name);
  if not Assigned(I) then
    Add(Name, TJSONString.Create(Value))
  else
    I.ValueStr := Value;
end;

procedure TJSONObject.SetItemAsStrUTF8(const Name: UnicodeString; const Value: RawByteString);
var I : TJSONValue;
begin
  I := GetItemValueByName(Name);
  if not Assigned(I) then
    Add(Name, TJSONString.CreateUTF8(Value))
  else
    I.ValueStrUTF8 := Value;
end;

procedure TJSONObject.SetItemAsInt(const Name: UnicodeString; const Value: Int64);
var I : TJSONValue;
begin
  I := GetItemValueByName(Name);
  if not Assigned(I) then
    Add(Name, TJSONInteger.Create(Value))
  else
    I.ValueInt := Value;
end;

procedure TJSONObject.SetItemAsFloat(const Name: UnicodeString; const Value: JSONFloat);
var I : TJSONValue;
begin
  I := GetItemValueByName(Name);
  if not Assigned(I) then
    Add(Name, TJSONFloat.Create(Value))
  else
    I.ValueFloat := Value;
end;

procedure TJSONObject.SetItemAsBoolean(const Name: UnicodeString; const Value: Boolean);
var I : TJSONValue;
begin
  I := GetItemValueByName(Name);
  if not Assigned(I) then
    Add(Name, TJSONBoolean.Create(Value))
  else
    I.ValueBoolean := Value;
end;

procedure TJSONObject.SetItemAsVariant(const Name: UnicodeString; const Value: Variant);
begin
  case VarType(Value) of
    varByte,
    varWord,
    varLongWord,
    varSmallint,
    varShortInt,
    varInteger,
    varInt64    : SetItemAsInt(Name, Value);
    varSingle,
    varDouble,
    varCurrency : SetItemAsFloat(Name, Value);
    varString   : SetItemAsStr(Name, Value);
    varBoolean  : SetItemAsBoolean(Name, Value);
  else
    raise EJSONValue.Create('Variant type not supported');
  end;
end;

function TJSONObject.Iterate(var Iterator: TJSONObjectIterator): Boolean;
var L : Integer;
begin
  L := Length(FList);
  Iterator.InternalIndex := 0;
  Result := L > 0;
  if Result then
    Iterator.Item := FList[0]
  else
    ClearJSONObjectItem(Iterator.Item);
end;

function TJSONObject.IterateNext(var Iterator: TJSONObjectIterator): Boolean;
var I, L : Integer;
    R : Boolean;
begin
  L := Length(FList);
  I := Iterator.InternalIndex;
  if I < 0 then
    raise EJSONValue.Create('Iterator closed');
  Inc(I);
  Iterator.InternalIndex := I;
  R := I < L;
  if R then
    Iterator.Item := FList[I]
  else
    ClearJSONObjectItem(Iterator.Item);
  Result := R;
end;

procedure TJSONObject.IterateClose(var Iterator: TJSONObjectIterator);
begin
  Iterator.InternalIndex := -1;
  ClearJSONObjectItem(Iterator.Item);
end;

procedure TJSONObject.BuildJSONString(const A: TUnicodeStringBuilder; const AOptions: TJSONStringOptions; const ALevel: Integer);
var I, L, N : Integer;
    F : Boolean;
    P : PJSONObjectItem;
begin
  F := jboIndent in AOptions;
  A.AppendCh(WideChar('{'));
  L := ALevel + 1;
  N := Length(FList);
  for I := 0 to N - 1 do
    begin
      if I > 0 then
        A.AppendCh(WideChar(','));
      if F then
        begin
          A.Append(WideCRLF);
          A.Append(SJSONIndent, L);
        end;
      P := @FList[I];
      A.AppendCh(WideChar('"'));
      A.Append(EscapedJSONStringValue(P^.Name));
      A.AppendCh(WideChar('"'));
      if F then
        A.AppendCh(WideSP);
      A.AppendCh(WideChar(':'));
      if F then
        A.AppendCh(WideSP);
      P^.Value.BuildJSONString(A, AOptions, L);
    end;
  if F and (N > 0) then
    begin
      A.Append(WideCRLF);
      A.Append(SJSONIndent, ALevel);
    end;
  A.AppendCh(WideChar('}'));
  if F and (N > 0) then
    begin
      A.Append(WideCRLF);
      A.Append(SJSONIndent, ALevel);
    end;
end;

function TJSONObject.Compare(const A: TJSONValue): Integer;
var B : TJSONObject;
    L, N, I, C : Integer;
    P, Q : PJSONObjectItem;
begin
  if A is TJSONObject then
    begin
      B := TJSONObject(A);
      L := GetCount;
      N := B.GetCount;
      if L < N then
        Result := -1 else
      if L > N then
        Result := 1
      else
        begin
          for I := 0 to L - 1 do
            begin
              P := @FList[I];
              Q := B.GetItemByName(P^.Name);
              if not Assigned(Q) then
                C := -1
              else
                C := P^.Value.Compare(Q^.Value);
              if C <> 0 then
                begin
                  Result := C;
                  exit;
                end;
            end;
          Result := 0;
        end;
    end
  else
    Result := inherited Compare(A);
end;

procedure TJSONObject.Validate(const Schema: TJSONObject);
var A : TJSONValue;
    P : TJSONObject;
    I : TJSONObjectIterator;
    V : TJSONValue;
    C : TJSONObject;
    B : TJSONValue;
    N : UnicodeString;
begin
  inherited Validate(Schema);
  A := Schema[SSchemaField_Properties];
  if Assigned(A) then
    begin
      if not A.ValueIsObject then
        raise EJSONValue.CreateFmt(SErr_InvalidSchema, ['Invalid value type for "properties" field']);
      P := A as TJSONObject;
      if P.Iterate(I) then
        try
          repeat
            C := I.Item.Value as TJSONObject;
            N := I.Item.Name;
            V := GetItemValueByName(N);
            if not Assigned(V) then
              begin
                B := C[SSchemaField_Required];
                if Assigned(B) then
                  if B.ValueBoolean then
                    raise EJSONSchema.CreateFmt(SErr_ValidationFailed, ['Required field missing: Field "' + N + '"']);
              end
            else
              try
                V.Validate(C);
              except
                on E: Exception do
                  raise EJSONSchema.Create(E.Message + ': Field "' + N + '"');
              end;
          until not P.IterateNext(I);
        finally
          P.IterateClose(I);
        end;
    end;
  A := Schema[SSchemaField_Items];
  if Assigned(A) then
    if A is TJSONObject then
      begin
        if Iterate(I) then
          try
            repeat
              V := I.Item.Value;
              try
                V.Validate(TJSONObject(A));
              except
                on E: Exception do
                  raise EJSONSchema.Create(E.Message + ': Field "' + I.Item.Name + '"');
              end;
            until not IterateNext(I);
          finally
            IterateClose(I);
          end;
      end
    else
      raise EJSONSchema.CreateFmt(SErr_InvalidSchema, ['Invalid value type for "items" field']);
end;



{ TJSONParser helpers }

function IsJSONWhiteSpaceChar(const C: WideChar): Boolean;
begin
  case C of
    WideHT,
    WideLF,
    WideCR,
    WideSP  : Result := True;
  else
    Result := False;
  end;
end;

function IsJSONReservedChar(const C: WideChar): Boolean;
begin
  case C of
    'a'..'z' : Result := True;
  else
    Result := False;
  end;
end;

function IsJSONUnsignedIntegerChar(const C: WideChar): Boolean;
begin
  case C of
    '0'..'9' : Result := True;
  else
    Result := False;
  end;
end;



{ TJSONParser }

const
  SErr_UnexpectedEndOfText = 'Unexpected end of text';
  SErr_NumberExpected      = 'Number expected';
  SErr_StringExpected      = 'String expected';
  SErr_InvalidString       = 'Invalid string';
  SErr_UnknownIdentifier   = 'Unknown identifier';
  SErr_IntegerOverflow     = 'Integer overflow';
  SErr_UnexpectedCharacter = 'Unexpected character';
  SErr_ValueExpected       = 'Value expected';
  SErr_UnexpectedToken     = 'Unexpected token';
  SErr_JSONParseError      = 'JSON parse error: %s';

constructor TJSONParser.Create;
begin
  inherited Create;
  FToken := jptNone;
  FTokenStrBuilder := TUnicodeStringBuilder.Create;
end;

destructor TJSONParser.Destroy;
begin
  FreeAndNil(FTokenStrBuilder);
  inherited Destroy;
end;

function TJSONParser.GetNextChar: WideChar;
var I : Integer;
    C : WideChar;
begin
  I := FTextPos;
  if I > FTextLength then
    begin
      FTextEOF := True;
      FTextChar := #0;
      Result := #0;
    end
  else
    begin
      C := FText[I];
      Inc(I);
      FTextPos := I;
      FTextChar := C;
      Result := C;
      Inc(FTokenLen);
    end;
end;

function TJSONParser.ExpectNextChar: WideChar;
begin
  Result := GetNextChar;
  if FTextEOF then
    raise EJSONParser.Create(SErr_UnexpectedEndOfText);
end;

function TJSONParser.GetTokenTextPtr: PWideChar;
begin
  Assert(FTextPos - FTokenLen - 1 >= 1);
  Result := @FText[FTextPos - FTokenLen - 1];
end;

function TJSONParser.ParseToken_UnsignedInteger(const Ch: WideChar): Int64;
const M1 = High(Int64) div 10;
      M2 = High(Int64) mod 10;
var C : WideChar;
    N : Int64;
    I : Byte;
    R : Boolean;
begin
  Assert(IsJSONUnsignedIntegerChar(Ch));
  C := Ch;
  N := 0;
  R := False;
  repeat
    if N >= M1 then
      begin
        if N > M1 then
          raise EJSONParser.Create(SErr_IntegerOverflow);
        R := True;
      end;
    N := N * 10;
    I := Ord(C) - Ord('0');
    if R then
      if I > M2 then
        raise EJSONParser.Create(SErr_IntegerOverflow);
    Inc(N, I);
    C := GetNextChar;
    if FTextEOF then
      break;
  until not IsJSONUnsignedIntegerChar(C);
  Result := N;
end;

function TJSONParser.ParseToken_ExpectUnsignedInteger(const Ch: WideChar): Int64;
begin
  if not IsJSONUnsignedIntegerChar(Ch) then
    raise EJSONParser.Create(SErr_NumberExpected);
  Result := ParseToken_UnsignedInteger(Ch);
end;

function TJSONParser.ParseToken_SignedInteger(const Ch: WideChar): Int64;
var C : WideChar;
    N : Boolean;
begin
  C := Ch;
  case C of
    '+', '-' :
      begin
        N := C = '-';
        C := ExpectNextChar;
        Result := ParseToken_ExpectUnsignedInteger(C);
        if N then
          Result := -Result;
      end;
  else
    Result := ParseToken_UnsignedInteger(C);
  end;
end;

function TJSONParser.ParseToken_ExpectSignedInteger(const Ch: WideChar): Int64;
begin
  case Ch of
    '+', '-', '0'..'9' : Result := ParseToken_SignedInteger(Ch);
  else
    raise EJSONParser.Create(SErr_NumberExpected);
  end;
end;

function TJSONParser.ParseToken_Float(const Ch: WideChar): JSONFloat;
var C : WideChar;
    F, E : JSONFloat;
    N : Integer;
begin
  C := Ch;
  Assert(C = '.');
  F := 0.0;
  E := 0.1;
  C := ExpectNextChar;
  if not IsJSONUnsignedIntegerChar(C) then
    raise EJSONParser.Create(SErr_NumberExpected);
  repeat
    N := Ord(C) - Ord('0');
    F := F + (E * N);
    C := GetNextChar;
    if FTextEOF then
      break;
    E := E / 10;
  until not IsJSONUnsignedIntegerChar(C);
  Result := F;
end;

function TJSONParser.ParseToken_Number(const Ch: WideChar): TJSONParserToken;
var C : WideChar;
    T : TJSONParserToken;
    N, E : Int64;
    F, G : JSONFloat;
begin
  C := Ch;
  T := jptInteger;
  case C of
    '-'      : N := ParseToken_SignedInteger(C);
    '0'..'9' : N := ParseToken_UnsignedInteger(C);
  else
    T := jptNone;
    N := 0;
  end;
  C := FTextChar;
  if C = '.' then
    begin
      if T = jptInteger then
        F := N
      else
        F := 0;
      G := ParseToken_Float(C);
      if F < 0 then
        F := F - G
      else
        F := F + G;
      T := jptFloat;
    end
  else
    F := 0.0;
  if T = jptNone then
    raise EJSONParser.Create(SErr_NumberExpected);
  C := FTextChar;
  if (C = 'E') or (C = 'e') then
    begin
      C := ExpectNextChar;
      E := ParseToken_ExpectSignedInteger(C);
      if T = jptInteger then
        F := N;
      F := F * Power(10, E);
      T := jptSciFloat;
    end;
  case T of
    jptInteger  : FTokenInteger := N;
    jptFloat,
    jptSciFloat : FTokenFloat := F;
  end;
  Result := T;
end;

function TJSONParser.ParseToken_String_Escaped_Hex4(const Ch: WideChar): WideChar;
var I : Integer;
    C, T : WideChar;
    D : Byte;
begin
  C := Ch;
  Assert(C = 'u');
  T := #0;
  for I := 1 to 4 do
    begin
      C := ExpectNextChar;
      case C of
        '0'..'9' : D := Ord(C) - Ord('0');
        'A'..'F' : D := Ord(C) - Ord('A') + 10;
        'a'..'f' : D := Ord(C) - Ord('a') + 10;
      else
        raise EJSONParser.Create(SErr_InvalidString);
      end;
      T := WideChar(Ord(T) * 16 + D);
    end;
  GetNextChar;
  Result := T;
end;

function TJSONParser.ParseToken_String_Escaped(const Ch: WideChar): WideChar;
var C, D : WideChar;
begin
  C := Ch;
  Assert(C = '\');
  C := ExpectNextChar;
  case C of
    '"',
    '\',
    '/' : D := C;
    'b' : D := #$8;
    'f' : D := #$C;
    'n' : D := #$A;
    'r' : D := #$D;
    't' : D := #$9;
  else
    D := #0;
  end;
  if D <> #0 then
    begin
      Result := D;
      GetNextChar;
      exit;
    end;
  if C = 'u' then
    begin
      Result := ParseToken_String_Escaped_Hex4(C);
      exit;
    end;
  // unknown escape code
  raise EJSONParser.Create(SErr_InvalidString);
end;

function TJSONParser.ParseToken_String(const Ch: WideChar): TJSONParserToken;
var C : WideChar;
    R : Boolean;
    D : WideChar;
begin
  C := Ch;
  Assert(C = '"');
  FTokenStrBuilder.Clear;
  R := False;
  C := ExpectNextChar;
  repeat
    case C of
      '"' :
        begin
          GetNextChar;
          R := True;
        end;
      '\' :
        begin
          D := ParseToken_String_Escaped(C);
          FTokenStrBuilder.AppendCh(D);
          if FTextEOF then
            raise EJSONParser.Create(SErr_InvalidString);
          C := FTextChar;
        end;
    else
      begin
        FTokenStrBuilder.AppendCh(C);
        C := ExpectNextChar;
      end;
    end;
  until R;
  FTokenStr := FTokenStrBuilder.AsUnicodeString;
  Result := jptString;
end;

function TJSONParser.GetAnyNextToken: TJSONParserToken;
var C : WideChar;
    T : TJSONParserToken;
    P : PWideChar;
    L : Integer;
begin
  FTokenPos := FTextPos;
  FTokenLen := 0;
  if FTextEOF then
    begin
      Result := jptEOF;
      exit;
    end;
  C := FTextChar;
  if IsJSONWhiteSpaceChar(C) then
    begin
      repeat
        C := GetNextChar;
        if FTextEOF then
          break;
        if C = WideLF then
          begin
            Inc(FTextRow);
            FTextRowPos := FTextPos;
          end;
      until not IsJSONWhiteSpaceChar(C);
      Result := jptWhiteSpace;
      exit;
    end;
  if IsJSONReservedChar(C) then
    begin
      repeat
        C := GetNextChar;
        if FTextEOF then
          break;
      until not IsJSONReservedChar(C);
      P := GetTokenTextPtr;
      L := FTokenLen;
      if (L = 4) and StrPMatchStrU(P, L, 'true') then
        T := jptTrue else
      if (L = 5) and StrPMatchStrU(P, L, 'false') then
        T := jptFalse else
      if (L = 4) and StrPMatchStrU(P, L, 'null') then
        T := jptNull
      else
        raise EJSONParser.Create(SErr_UnknownIdentifier);
      Result := T;
      exit;
    end;
  case C of
    '{' : T := jptLeftCurly;
    '}' : T := jptRightCurly;
    '[' : T := jptLeftSquare;
    ']' : T := jptRightSquare;
    ':' : T := jptColon;
    ',' : T := jptComma;
  else
    T := jptNone;
  end;
  if T <> jptNone then
    begin
      GetNextChar;
      Result := T;
      exit;
    end;
  case C of
    '-',
    '0'..'9',
    '.' : T := ParseToken_Number(C);
    '"' : T := ParseToken_String(C);
  else
    raise EJSONParser.Create(SErr_UnexpectedCharacter);
  end;
  Result := T;
end;

function TJSONParser.GetNextToken: TJSONParserToken;
var T : TJSONParserToken;
begin
  repeat
    T := GetAnyNextToken;
  until T <> jptWhiteSpace;
  FToken := T;
  Result := T;
end;

function TJSONParser.RequireNextToken: TJSONParserToken;
begin
  Result := GetNextToken;
  if Result = jptEOF then
    raise EJSONParser.Create(SErr_UnexpectedEndOfText);
end;

{ = false / null / true }
function TJSONParser.ParseNamedLiteral(const AToken: TJSONParserToken): TJSONValue;
begin
  Assert(AToken in [jptTrue, jptFalse, jptNull]);
  case AToken of
    jptTrue,
    jptFalse : Result := TJSONBoolean.Create(AToken = jptTrue);
    jptNull  : Result := TJSONNull.Create;
  else
    Result := nil;
  end;
  if not Assigned(Result) then
    exit;
  GetNextToken;
end;

{ number = [ minus ] int [ frac ] [ exp ] }
function TJSONParser.ParseNumber(const AToken: TJSONParserToken): TJSONValue;
begin
  Assert(AToken in [jptInteger, jptFloat, jptSciFloat]);
  case AToken of
    jptInteger  : Result := TJSONInteger.Create(FTokenInteger);
    jptFloat,
    jptSciFloat : Result := TJSONFloat.Create(FTokenFloat);
  else
    Result := nil;
  end;
  if not Assigned(Result) then
    exit;
  GetNextToken;
end;

{ string = quotation-mark *char quotation-mark }
function TJSONParser.ParseString: TJSONValue;
begin
  Assert(FToken = jptString);
  case FToken of
    jptString : Result := TJSONString.Create(FTokenStr);
  else
    Result := nil;
  end;
  if not Assigned(Result) then
    exit;
  GetNextToken;
end;

function TJSONParser.ParseRequiredStringValue: UnicodeString;
begin
  if FToken <> jptString then
    raise EJSONParser.Create(SErr_StringExpected);
  Result := FTokenStr;
  GetNextToken;
end;

{ array = begin-array [ value *( value-separator value ) ] end-array }
function TJSONParser.ParseArray: TJSONArray;
var A : TJSONArray;
    T : TJSONParserToken;
    R : Boolean;
begin
  Assert(FToken = jptLeftSquare);
  A := TJSONArray.Create;
  try
    T := RequireNextToken;
    R := T = jptRightSquare;
    while not R do
      begin
        A.Append(ParseRequiredValue(T));
        case FToken of
          jptRightSquare : R := True;
          jptComma       : T := RequireNextToken;
        else
          raise EJSONParser.Create(', or ] expected');
        end;
      end;
    Assert(FToken = jptRightSquare);
    GetNextToken;
  except
    A.Free;
    raise;
  end;
  Result := A;
end;

{ object = begin-object [ member *( value-separator member ) ] end-object }
{ member = string name-separator value }
function TJSONParser.ParseObject: TJSONObject;
var A : TJSONObject;
    T : TJSONParserToken;
    R : Boolean;
    N : UnicodeString;
    V : TJSONValue;
begin
  Assert(FToken = jptLeftCurly);
  A := TJSONObject.Create;
  try
    T := RequireNextToken;
    R := T = jptRightCurly;
    while not R do
      begin
        N := ParseRequiredStringValue;
        if FToken <> jptColon then
          raise EJSONParser.Create(': expected');
        T := RequireNextToken;
        V := ParseRequiredValue(T);
        A.Add(N, V);
        case FToken of
          jptRightCurly : R := True;
          jptComma      : RequireNextToken;
        else
          raise EJSONParser.Create(', or } expected');
        end;
      end;
    Assert(FToken = jptRightCurly);
    GetNextToken;
  except
    A.Free;
    raise;
  end;
  Result := A;
end;

{ value = false / null / true / object / array / number / string }
function TJSONParser.ParseValue(const AToken: TJSONParserToken): TJSONValue;
begin
  case AToken of
    jptFalse,
    jptNull,
    jptTrue       : Result := ParseNamedLiteral(AToken);
    jptLeftCurly  : Result := ParseObject;
    jptLeftSquare : Result := ParseArray;
    jptInteger,
    jptFloat,
    jptSciFloat   : Result := ParseNumber(AToken);
    jptString     : Result := ParseString;
  else
    Result := nil;
  end;
end;

function TJSONParser.ParseRequiredValue(const AToken: TJSONParserToken): TJSONValue;
begin
  Result := ParseValue(AToken);
  if not Assigned(Result) then
    raise EJSONParser.Create(SErr_ValueExpected);
end;

procedure TJSONParser.SetText(const AText: UnicodeString);
begin
  FText := AText;
  FTextLength := Length(FText);
end;

procedure TJSONParser.InitParser;
begin
  FTextPos := 1;
  FTextRow := 1;
  FTextRowPos := 1;
  FTextEOF := False;
  GetNextChar;
  FToken := GetNextToken;
end;

{ JSON-text = object / array }
function TJSONParser.ParseText(const AText: UnicodeString): TJSONValue;
begin
  SetText(AText);
  InitParser;
  try
    case FToken of
      jptEOF        : Result := nil;
      jptLeftSquare : Result := ParseArray;
      jptLeftCurly  : Result := ParseObject;
    else
      raise EJSONParser.Create(SErr_UnexpectedToken);
    end;
  except
    on E: Exception do
      raise EJSONParser.CreateFmt(SErr_JSONParseError, [E.Message]);
  end;
end;

function TJSONParser.ParseTextUTF8(const AText: RawByteString): TJSONValue;
begin
  Result := ParseText(UTF8StringToUnicodeString(AText));
end;



{                                                                              }
{ JSON helpers                                                                 }
{                                                                              }
function ParseJSONText(const JSONText: UnicodeString): TJSONValue;
var P : TJSONParser;
begin
  P := TJSONParser.Create;
  try
    Result := P.ParseText(JSONText);
  finally
    P.Free;
  end;
end;

function ParseJSONTextUTF8(const JSONText: RawByteString): TJSONValue;
begin
  Result := ParseJSONText(UTF8StringToUnicodeString(JSONText));
end;



{                                                                              }
{ JSON schema schema                                                           }
{                                                                              }
const
  SSchemaSchema =
    '{' +
    '"$schema" : "http://json-schema.org/draft-03/schema#",' +
    '"id" : "http://json-schema.org/draft-03/schema#",' +
    '"type" : "object",' +

    '"properties" : {' +
      '"type" : {' +
        '"type" : ["string", "array"],' +
        '"items" : {' +
          '"type" : ["string", {"$ref" : "#"}]' +
        '},' +
        '"uniqueItems" : true,' +
        '"default" : "any"' +
    '},' +

    '"properties" : {' +
        '"type" : "object",' +
        '"additionalProperties" : {"$ref" : "#"},' +
        '"default" : {}' +
    '},' +

      '"patternProperties" : {' +
        '"type" : "object",' +
        '"additionalProperties" : {"$ref" : "#"},' +
        '"default" : {}' +
    '},' +

      '"additionalProperties" : {' +
        '"type" : [{"$ref" : "#"}, "boolean"],' +
        '"default" : {}' +
    '},' +

      '"items" : {' +
        '"type" : [{"$ref" : "#"}, "array"],' +
        '"items" : {"$ref" : "#"},' +
        '"default" : {}' +
    '},' +
		
      '"additionalItems" : {' +
        '"type" : [{"$ref" : "#"}, "boolean"],' +
        '"default" : {}' +
    '},' +

      '"required" : {' +
        '"type" : "boolean",' +
        '"default" : false ' +
    '},' +
		
      '"dependencies" : {' +
        '"type" : "object",' +
        '"additionalProperties" : {' +
          '"type" : ["string", "array", {"$ref" : "#"}],' +
          '"items" : {' +
            '"type" : "string"' +
          '}' +
        '},' +
        '"default" : {}' +
    '},' +
		
      '"minimum" : {' +
        '"type" : "number"' +
    '},' +
		
    
      '"maximum" : {' +
        '"type" : "number"' +
    '},' +
		
      '"exclusiveMinimum" : {' +
        '"type" : "boolean",' +
        '"default" : false ' +
    '},' +
		
      '"exclusiveMaximum" : {' +
        '"type" : "boolean",' +
        '"default" : false ' +
    '},' +

      '"minItems" : {' +
        '"type" : "integer",' +
        '"minimum" : 0,' +
        '"default" : 0 ' +
    '},' +
		
      '"maxItems" : {' +
        '"type" : "integer",' +
        '"minimum" : 0 ' +
    '},' +
		
      '"uniqueItems" : {' +
        '"type" : "boolean",' +
        '"default" : false ' +
    '},' +

      '"pattern" : {' +
        '"type" : "string",' +
        '"format" : "regex"' +
    '},' +
		
      '"minLength" : {' +
        '"type" : "integer",' +
        '"minimum" : 0,' +
        '"default" : 0 ' +
    '},' +
		
      '"maxLength" : {' +
        '"type" : "integer"' +
    '},' +
    
      '"enum" : {' +
        '"type" : "array",' +
        '"minItems" : 1,' +
        '"uniqueItems" : true ' +
    '},' +
    
      '"default" : {' +
        '"type" : "any"' +
    '},' +
    
      '"title" : {' +
        '"type" : "string"' +
    '},' +
		
      '"description" : {' +
        '"type" : "string"' +
    '},' +
		
      '"format" : {' +
        '"type" : "string"' +
    '},' +
		
      '"divisibleBy" : {' +
        '"type" : "number",' +
        '"minimum" : 0,' +
        '"exclusiveMinimum" : true,' +
        '"default" : 1 ' +
    '},' +
		
      '"disallow" : {' +
        '"type" : ["string", "array"],' +
        '"items" : {' +
          '"type" : ["string", {"$ref" : "#"}] ' +
        '},' +
        '"uniqueItems" : true ' +
    '},' +
    
      '"extends" : {' +
        '"type" : [{"$ref" : "#"}, "array"],' +
        '"items" : {"$ref" : "#"},' +
        '"default" : {}' +
    '},' +
		
      '"id" : {' +
        '"type" : "string",' +
        '"format" : "uri"' +
    '},' +

      '"$ref" : {' +
        '"type" : "string",' +
        '"format" : "uri"' +
    '},' +

      '"$schema" : {' +
        '"type" : "string",' +
        '"format" : "uri"' +
      '}' +
    '},' +

    '"dependencies" : {' +
      '"exclusiveMinimum" : "minimum",' +
      '"exclusiveMaximum" : "maximum"' +
    '},' +

    '"default" : {}' +
    '}';

var
  SchemaSchemaObj : TJSONObject = nil;

function GetSchemaSchemaObj: TJSONObject;
var P : TJSONParser;
begin
  if not Assigned(SchemaSchemaObj) then
    begin
      P := TJSONParser.Create;
      try
        SchemaSchemaObj := P.ParseText(SSchemaSchema) as TJSONObject;
      finally
        P.Free;
      end;
    end;
  Result := SchemaSchemaObj;
end;

procedure ValidateSchema(const Schema: TJSONObject);
begin
  Schema.Validate(GetSchemaSchemaObj);
end;



{                                                                              }
{ Self-testing code                                                            }
{                                                                              }
{$IFDEF DEBUG}
{$IFDEF TEST}
{$ASSERTIONS ON}
procedure Test;
const
  TestStr1 =
      '{' +
          '"test1":"a\"b\"c",' +
          '"test2":123,' +
          '"test3":true,' +
          '"test4":null,' +
          '"test5":[1,"a",12.345,"",{},[]],' +
          '"test6":1.2345' +
      '}';
  TestStr2 = ' [  ' +
             '1,-1,1.25,-1.25,1.2325e+02,-1.2325e+02 ,  1E3 , 1E-3 ,  ' +
             '9223372036854775807,-9223372036854775807,0' +
             ' ]  '#13#10;
  SchemaStr1 =
      '{' +
          '"properties":{' +
              '"test0":{' +
                  '"type":"string",' +
                  '"required":false' +
              '},' +
              '"test1":{' +
                  '"type":"string",' +
                  '"required":true,' +
                  '"minLength":2,' +
                  '"maxLength":9' +
              '},' +
              '"test2":{' +
                  '"type":"integer",' +
                  '"required":true,' +
                  '"minimum":123,' +
                  '"maximum":123' +
              '},' +
              '"test3":{' +
                  '"type":"boolean"' +
              '},' +
              '"test4":{' +
                  '"type":"any",' +
                  '"required":true' +
              '},' +
              '"test5":{' +
                  '"type":"array",' +
                  '"required":true,' +
                  '"minItems":6,' +
                  '"maxItems":6,' +
                  '"items":[' +
                      '{ "type":"integer" },' +
                      '{ "type":"string",' +
                      '  "enum":["a","b"],' +
                      '  "maxLength":1 },' +
                      '{ "type":"number" },' +
                      '{ "type":"string" },' +
                      '{ "type":"object" },' +
                      '{ "type":"array",' +
                      '  "minItems":0,' +
                      '  "maxItems":0 }' +
                  ']' +
              '},' +
              '"test6":{' +
                  '"type":"number",' +
                  '"required":true,' +
                  '"minimum":1.2,' +
                  '"maximum":1.3' +
              '},' +
              '"test7":{' +
                  '"type":"any",' +
                  '"required":false' +
              '}' +
          '}' +
      '}';
var X : TJSONObject;
    Y : TJSONArray;
    P : TJSONParser;
    A, B : TJSONValue;
begin
  Assert(EscapedJSONStringValue('') = '');
  Assert(EscapedJSONStringValue('abc') = 'abc');
  Assert(EscapedJSONStringValue('a"c\/"') = 'a\"c\\\/\"');
  Assert(EscapedJSONStringValue('abc'#13#10) = 'abc\r\n');
  Assert(QuotedJSONStringValue('a"\"c') = '"a\"\\\"c"');

  // TestStr1 construct
  X := TJSONObject.Create;
  X.Add('test1', TJSONString.Create('a"b"c'));
  X.Add('test2', TJSONInteger.Create(123));
  X.Add('test3', TJSONBoolean.Create(True));
  X.Add('test4', TJSONNull.Create);
  Y := TJSONArray.Create;
  Assert(Y.ValueType = jvtArray); 
  Y.Append(TJSONInteger.Create(1));
  Y.Append(TJSONString.Create('a'));
  Y.Append(TJSONFloat.Create(12.345));
  Y.Append(TJSONString.Create(''));
  Y.Append(TJSONObject.Create);
  Y.Append(TJSONArray.Create);
  X.Add('test5', Y);
  X.Add('test6', TJSONFloat.Create(1.2345));
  Assert(X.GetJSONString = TestStr1);
  X.Free;

  // TestStr1 parse
  P := TJSONParser.Create;
  A := P.ParseText(TestStr1);
  Assert(A.GetJSONString = TestStr1);
  // Writeln(A.GetJSONString([jboIndent]));
  Assert(A is TJSONObject);
  Assert(A.ValueType = jvtObject);
  X := TJSONObject(A);
  Assert(not X.Exists('abc'));
  Assert(X.GetItemAsInt('abc', -1) = -1);
  Assert(X.Exists('test1'));
  Assert(not X.Item['test1'].ValueIsNull);
  Assert(X.Item['test1'].ValueType = jvtString);
  Assert(X.Item['test1'].ValueStr = 'a"b"c');
  Assert(X.GetItemAsStr('test1') = 'a"b"c');
  Assert(X.Item['test2'].ValueType = jvtInteger);
  Assert(X.Item['test2'].ValueInt = 123);
  Assert(X.GetItemAsInt('test2') = 123);
  Assert(X.Item['test3'].ValueType = jvtBoolean);
  Assert(X.Item['test3'].ValueBoolean);
  Assert(X.GetItemAsBoolean('test3'));
  Assert(X.Item['test4'].ValueType = jvtNull);
  Assert(X.Item['test4'].ValueIsNull);
  Assert(X.Item['test5'].ValueType = jvtArray);
  Assert(X.Item['test5'].ValueIsArray);
  B := X.Item['test5'];
  Assert(B is TJSONArray);
  Assert(B.ValueType = jvtArray);
  Y := TJSONArray(B);
  Assert(Y.Count = 6);
  Assert(Y.Item[0].ValueType = jvtInteger);
  Assert(Y.Item[0].ValueInt = 1);
  Assert(Y.ItemAsInt[0] = 1);
  Assert(Y.Item[1].ValueStr = 'a');
  Assert(Y.ItemAsStr[1] = 'a');
  Assert(Y.Item[2].ValueType = jvtFloat);
  Assert(Abs(Y.Item[2].ValueFloat - 12.345) < 1e-6);
  Assert(Abs(Y.ItemAsFloat[2] - 12.345) < 1e-6);
  Assert(Y.Item[3].ValueType = jvtString);
  Assert(Y.Item[3].ValueStr = '');
  Assert(Y.ItemAsStr[3] = '');
  Assert(Y.Item[4].GetJSONString = '{}');
  Assert(Y.Item[4].ValueIsObject);
  Assert(Y.Item[5].GetJSONString = '[]');
  Assert(Y.Item[5].ValueIsArray);
  Assert(Y.Item[5].ValueType = jvtArray);
  Assert(Abs(X.Item['test6'].ValueFloat - 1.2345) < 1e-6);
  A.Free;
  P.Free;

  // TestStr2 parse
  P := TJSONParser.Create;
  A := P.ParseText(TestStr2);
  Assert(A is TJSONArray);
  Y := TJSONArray(A);
  Assert(Y.Count = 11);
  Assert(Y.Item[0].ValueInt = 1);
  Assert(Y.Item[1].ValueInt = -1);
  Assert(Y.Item[2].ValueFloat = 1.25);
  Assert(Y.Item[3].ValueFloat = -1.25);
  Assert(Abs(Y.Item[4].ValueFloat - 1.2325e+02) < 1e-08);
  Assert(Abs(Y.Item[5].ValueFloat + 1.2325e+02) < 1e-08);
  Assert(Y.Item[6].ValueFloat = 1e3);
  Assert(Y.Item[7].ValueType = jvtFloat);
  Assert(Abs(Y.Item[7].ValueFloat - 1e-3) < 1e-08);
  Assert(Y.Item[8].ValueInt = 9223372036854775807);
  Assert(Y.Item[9].ValueInt = -9223372036854775807);
  Assert(Y.Item[10].ValueType = jvtInteger);
  Assert(Y.Item[10].ValueInt = 0);
  A.Free;
  P.Free;

  P := TJSONParser.Create;
  try
    // Parse empty text
    A := P.ParseText('');
    Assert(not Assigned(A));
    A := P.ParseText('  ');
    Assert(not Assigned(A));

    // Various tests on schema schema
    Assert(GetSchemaSchemaObj <> nil);
    A := P.ParseText(SSchemaSchema);
    B := P.ParseText(A.GetJSONString);
    Assert(B.GetJSONString = A.GetJSONString);
    Assert(B.Compare(A) = 0);
    A.Validate(B as TJSONObject);
    B.Free;
    A.Free;
    
    // Validate schema
    P := TJSONParser.Create;
    A := P.ParseText(SchemaStr1);
    ValidateSchema(A as TJSONObject);
    A.Free;
    
    // Validate TestStr1 object against schema
    A := P.ParseText(SchemaStr1);
    B := P.ParseText(TestStr1);
    B.Validate(A as TJSONObject);
    B.Free;
    A.Free;
  finally
    P.Free;
  end;
end;
{$ENDIF}
{$ENDIF}



initialization
finalization
  FreeAndNil(SchemaSchemaObj);
end.

