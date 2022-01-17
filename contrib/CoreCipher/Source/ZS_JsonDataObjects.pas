{ ****************************************************************************** }
{ * json object library,                                                       * }
{ * https://zpascal.net                                                        * }
{ * https://github.com/PassByYou888/zAI                                        * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/PascalString                               * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zChinese                                   * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/FFMPEG-Header                              * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/InfiniteIoT                                * }
{ * https://github.com/PassByYou888/FastMD5                                    * }
{ ****************************************************************************** }


(*****************************************************************************
The MIT License (MIT)

Copyright (c) 2015-2016 Andreas Hausladen

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*****************************************************************************)

{$A8,B-,C+,E-,F-,G+,H+,I+,J-,K-,M-,N-,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Z1}
{$WARN WIDECHAR_REDUCED OFF} // All sets only use ASCII chars (<=#127) and the compiler generates the >=#128 check itself
{$STRINGCHECKS OFF} // It only slows down Delphi strings, doesn't help C++Builder migration and is finally gone in XE+
{$WARN SYMBOL_DEPRECATED OFF} // for StrLen/StrLComp
{$POINTERMATH ON}

unit ZS_JsonDataObjects;

{$IFDEF VER200}
  // Delphi 2009's ErrorInsight parser uses the CompilerVersion's memory address instead of 20.0, failing all the
  // IF CompilerVersion compiler directives
  {$DEFINE CPUX86}
{$ELSE}
  {$IF CompilerVersion >= 24.0} // XE3 or newer
    {$LEGACYIFEND ON}
  {$IFEND}
  {$IF CompilerVersion >= 23.0}
    {$DEFINE HAS_UNIT_SCOPE}
    {$DEFINE HAS_RETURN_ADDRESS}
  {$IFEND}
  {$IF CompilerVersion <= 22.0} // XE or older
    {$DEFINE CPUX86}
  {$IFEND}
{$ENDIF VER200}

{$IFDEF NEXTGEN}
  {$IF CompilerVersion >= 31.0} // 10.1 Berlin or newer
    {$DEFINE SUPPORTS_UTF8STRING} // Delphi 10.1 Berlin supports UTF8String for mobile compilers
  {$IFEND}
{$ELSE}
  {$DEFINE SUPPORTS_UTF8STRING}
{$ENDIF}

{$IFDEF CPUX64}
  {$IFNDEF LINUX64} // Linux 64 compiler doesn't support ASM for x64 code => LLVM
    {$DEFINE ASMSUPPORT}
  {$ENDIF ~LINUX64}
{$ENDIF CPUX64}
{$IFDEF CPUX86}
  {$DEFINE ASMSUPPORT}
{$ENDIF CPUX86}
{$IFDEF EXTERNALLINKER} // implicates LLVM
  {$UNDEF ASMSUPPORT}
{$ENDIF EXTERNALLINKER}

// Enables the progress callback feature
//{$DEFINE SUPPORT_PROGRESS}

// Sanity checks all array index accesses and raise an EListError exception.
//{$DEFINE CHECK_ARRAY_INDEX}

// JSON allows the slash to be escaped. This is only necessary if you plan to put the JSON string
// into a <script>-Tag because then "</" can't be used and must be escaped to "<\/". This switch
// enables the special handling for "</" but makes the parser slightly slower.
//{$DEFINE ESCAPE_SLASH_AFTER_LESSTHAN}

// When parsing a JSON string the pair names are interned to reduce the memory foot print. This
// slightly slows down the parser but saves a lot of memory if the JSON string contains repeating
// pair names. The interning uses a hashset to store the strings.
{$DEFINE USE_STRINGINTERN_FOR_NAMES}

// Use an optimized NewInstance implementation. It skips the initialization of the interface table.
// and seals the TJsonArray and TJsonObject classes because it isn't safe to derive from them.
//{$DEFINE USE_FAST_NEWINSTANCE}

//{$IF CompilerVersion < 28.0} // XE6 or older
  // The XE7 compiler is broken. It doesn't collapse duplicate string literals anymore. (RSP-10015)
  // But if the string literals are used in loops this optimization still helps.

  // Optimizes the following pattern:
  //   O['Name'][MyPropStr]
  //   O['Name']['MyProp'].
  // where the second O['Name'] is handled very fast by caching the pointer to the 'Name' string literal.
  {$DEFINE USE_LAST_NAME_STRING_LITERAL_CACHE}
//{$IFEND}

// When parsing the JSON string, the UStrAsg calls are skipped for internal strings what eliminates
// the CPU locks for those string assignments.
{$DEFINE USE_FAST_STRASG_FOR_INTERNAL_STRINGS}

{$IFDEF AUTOREFCOUNT}
  // Delphi's ARC is slow (RSP-9712). This switch enables a faster ARC handling and even skips memory
  // barrier were possible.
  {$DEFINE USE_FAST_AUTOREFCOUNT}
{$ENDIF AUTOREFCOUNT}

{$IFDEF MSWINDOWS}
  // When adding JSON object properties with string literals, the string literals are stored directly
  // in the "Name" field instead of using UStrAsg that creates a new heap string. This improves the
  // performance as no string is copied and it slighly reduces the memory usage.
  // The string literals are only used if they are in the main instance or the DLL that contains the
  // JsonDataObjects unit. Other string literals are copied using UStrAsg because unloading the DLL
  // that holds them would cause access violations.
  // This has no effect when parsing JSON strings because then there are no string literals.
  {$DEFINE USE_NAME_STRING_LITERAL}

  // Reading a large file >64 MB from a network drive in Windows 2003 Server or older can lead to
  // an INSUFFICIENT RESOURCES error. By enabling this switch, large files are read in 20 MB blocks.
  {$DEFINE WORKAROUND_NETWORK_FILE_INSUFFICIENT_RESOURCES}

  // If defined, the TzSpecificLocalTimeToSystemTime is imported with GetProcAddress and if it is
  // not available (Windows 2000) an alternative implementation is used.
  {$DEFINE SUPPORT_WINDOWS2000}

{$ENDIF MSWINDOWS}

interface

uses
  {$IFDEF HAS_UNIT_SCOPE}
  System.SysUtils, System.Classes;
  {$ELSE}
  SysUtils, Classes;
  {$ENDIF HAS_UNIT_SCOPE}

type
  TJsonBaseObject = class;
  TJsonObject = class;
  TJsonArray = class;

  {$IFDEF NEXTGEN}
  // Mobile compilers have PAnsiChar but it is hidden and then published under a new name. This alias
  // allows us to remove some IFDEFs.
  PAnsiChar = MarshaledAString;
  {$ENDIF NEXTGEN}

  EJsonException = class(Exception);
  EJsonCastException = class(EJsonException);
  EJsonPathException = class(EJsonException);

  EJsonParserException = class(EJsonException)
  private
    FColumn: NativeInt;
    FPosition: NativeInt;
    FLineNum: NativeInt;
  public
    constructor CreateResFmt(ResStringRec: PResStringRec; const Args: array of const; ALineNum, aColumn, APosition: NativeInt);
    constructor CreateRes(ResStringRec: PResStringRec; ALineNum, aColumn, APosition: NativeInt);

    property LineNum: NativeInt read FLineNum;   // base 1
    property Column: NativeInt read FColumn;     // base 1
    property Position: NativeInt read FPosition; // base 0  Utf8Char/WideChar index
  end;

  {$IFDEF SUPPORT_PROGRESS}
  TJsonReaderProgressProc = procedure(Data: Pointer; Percentage: Integer; Position, Size: NativeInt);

  PJsonReaderProgressRec = ^TJsonReaderProgressRec;
  TJsonReaderProgressRec = record
    Data: Pointer;        // used for the first Progress() parameter
    Threshold: NativeInt; // 0: Call only if percentage changed; greater than 0: call after n processed bytes
    Progress: TJsonReaderProgressProc;

    function Init(AProgress: TJsonReaderProgressProc; aData: Pointer = nil; AThreshold: NativeInt = 0): PJsonReaderProgressRec;
  end;
  {$ENDIF SUPPORT_PROGRESS}

  // TJsonOutputWriter is used to write the JSON data to a string, stream or TStrings in a compact
  // or human readable format.
  TJsonOutputWriter = record
  private type
    TLastType = (ltInitial, ltIndent, ltUnindent, ltIntro, ltValue, ltSeparator);

    PJsonStringArray = ^TJsonStringArray;
    TJsonStringArray = array[0..MaxInt div SizeOf(string) - 1] of string;

    PJsonStringBuilder = ^TJsonStringBuilder;
    TJsonStringBuilder = record
    private
      FData: PChar;
      FCapacity: Integer;
      FLen: Integer;
      procedure Grow(MinLen: Integer);
    public
      procedure Init;
      procedure Done;
      procedure DoneConvertToString(var s: string);
      function FlushToBytes(var Bytes: PByte; var Size: NativeInt; Encoding: TEncoding): NativeInt;
      procedure FlushToMemoryStream(stream: TMemoryStream; Encoding: TEncoding);
      procedure FlushToStringBuffer(var buffer: TJsonStringBuilder);
      procedure FlushToString(var s: string);

      function Append(const s: string): PJsonStringBuilder; overload;
      procedure Append(p: PChar; Len: Integer); overload;
      function Append2(const s1: string; s2: PChar; S2Len: Integer): PJsonStringBuilder; overload;
      procedure Append2(Ch1: Char; Ch2: Char); overload;
      procedure Append3(Ch1: Char; const s2, s3: string); overload;
      procedure Append3(Ch1: Char; const s2: string; Ch3: Char); overload; inline;
      procedure Append3(Ch1: Char; const p2: PChar; P2Len: Integer; Ch3: Char); overload;

      property Len: Integer read FLen;
      property Data: PChar read FData;
    end;
  private
    FLastType: TLastType;
    FCompact: Boolean;
    FStringBuffer: TJsonStringBuilder;
    FLines: TStrings;
    FLastLine: TJsonStringBuilder;

    FStreamEncodingBuffer: PByte;
    FStreamEncodingBufferLen: NativeInt;
    FStream: TStream;                // used when writing to a stream
    FEncoding: TEncoding;            // used when writing to a stream

    FIndents: PJsonStringArray;      // buffer for line indention strings
    FIndentsLen: Integer;
    FIndent: Integer;                // current indention level

    procedure StreamFlushPossible; inline; // checks if StreamFlush must be called
    procedure StreamFlush;                 // writes the buffer to the stream
    procedure ExpandIndents;
    procedure AppendLine(AppendOn: TLastType; const s: string); overload; inline;
    procedure AppendLine(AppendOn: TLastType; p: PChar; Len: Integer); overload; inline;
    procedure FlushLastLine;
  private // unit private
    procedure Init(ACompact: Boolean; AStream: TStream; AEncoding: TEncoding; ALines: TStrings);
    function Done: string;
    procedure StreamDone;
    procedure LinesDone;

    procedure Indent(const s: string);
    procedure Unindent(const s: string);
    procedure AppendIntro(p: PChar; Len: Integer);
    procedure AppendValue(const s: string); overload;
    procedure AppendValue(p: PChar; Len: Integer); overload;
    procedure AppendStrValue(p: PChar; Len: Integer);
    procedure AppendSeparator(const s: string);
    procedure FreeIndents;
  end;

  TJsonDataType = (
    jdtNone, jdtString, jdtInt, jdtLong, jdtULong, jdtFloat, jdtDateTime, jdtBool, jdtArray, jdtObject
  );

  // TJsonDataValue holds the actual value
  PJsonDataValue = ^TJsonDataValue;
  TJsonDataValue = packed record
  private type
    TJsonDataValueRec = record
      case TJsonDataType of
        jdtNone: (p: PChar);     // helps when debugging
        jdtString: (s: Pointer); // We manage the string ourself. Delphi doesn't allow "string" in a
                                 // variant record and if we have no string, we don't need to clean
                                 // it up, anyway.
        jdtInt: (i: Integer);
        jdtLong: (L: Int64);
        jdtULong: (u: UInt64);
        jdtFloat: (f: Double);
        jdtDateTime: (d: TDateTime);
        jdtBool: (b: Boolean);
        jdtArray: (a: Pointer);  // owned by TJsonDataValue
        jdtObject: (O: Pointer); // owned by TJsonDataValue
    end;
  private
    FValue: TJsonDataValueRec;
    FTyp: TJsonDataType;
    function GetValue: string;
    function GetIntValue: Integer;
    function GetLongValue: Int64;
    function GetULongValue: UInt64;
    function GetFloatValue: Double;
    function GetDateTimeValue: TDateTime;
    function GetBoolValue: Boolean;
    function GetArrayValue: TJsonArray;
    function GetObjectValue: TJsonObject;
    function GetVariantValue: Variant;

    procedure SetValue(const AValue: string);
    procedure SetIntValue(const AValue: Integer);
    procedure SetLongValue(const AValue: Int64);
    procedure SetULongValue(const AValue: UInt64);
    procedure SetFloatValue(const AValue: Double);
    procedure SetDateTimeValue(const AValue: TDateTime);
    procedure SetBoolValue(const AValue: Boolean);
    procedure SetArrayValue(const AValue: TJsonArray);
    procedure SetObjectValue(const AValue: TJsonObject);
    procedure SetVariantValue(const AValue: Variant);

    procedure InternToJSON(var Writer: TJsonOutputWriter);
    procedure InternSetValue(const AValue: string); // skips the call to Clear()
    procedure InternSetValueTransfer(var AValue: string); // skips the call to Clear() and transfers the string without going through UStrAsg+UStrClr
    procedure InternSetArrayValue(const AValue: TJsonArray);
    procedure InternSetObjectValue(const AValue: TJsonObject);
    procedure Clear;
    procedure TypeCastError(ExpectedType: TJsonDataType);
  public
    function IsNull: Boolean;

    property Typ: TJsonDataType read FTyp;
    property Value: string read GetValue write SetValue;
    property IntValue: Integer read GetIntValue write SetIntValue;
    property LongValue: Int64 read GetLongValue write SetLongValue;
    property ULongValue: UInt64 read GetULongValue write SetULongValue;
    property FloatValue: Double read GetFloatValue write SetFloatValue;
    property DateTimeValue: TDateTime read GetDateTimeValue write SetDateTimeValue;
    property BoolValue: Boolean read GetBoolValue write SetBoolValue;
    property ArrayValue: TJsonArray read GetArrayValue write SetArrayValue;
    property ObjectValue: TJsonObject read GetObjectValue write SetObjectValue;
    property VariantValue: Variant read GetVariantValue write SetVariantValue;
  end;

  // TJsonDataValueHelper is used to implement the "easy access" functionality. It is
  // slightly slower than using the direct indexed properties.
  TJsonDataValueHelper = record
  private
    function GetValue: string; inline;
    function GetIntValue: Integer; inline;
    function GetLongValue: Int64; inline;
    function GetULongValue: UInt64; //inline;  no implicit operator due to conflict with Int64
    function GetFloatValue: Double; inline;
    function GetDateTimeValue: TDateTime; inline;
    function GetBoolValue: Boolean; inline;
    function GetArrayValue: TJsonArray; inline;
    function GetObjectValue: TJsonObject; inline;
    function GetVariantValue: Variant; inline;

    procedure SetValue(const Value: string);
    procedure SetIntValue(const Value: Integer);
    procedure SetLongValue(const Value: Int64);
    procedure SetULongValue(const Value: UInt64);
    procedure SetFloatValue(const Value: Double);
    procedure SetDateTimeValue(const Value: TDateTime);
    procedure SetBoolValue(const Value: Boolean);
    procedure SetArrayValue(const Value: TJsonArray);
    procedure SetObjectValue(const Value: TJsonObject);
    procedure SetVariantValue(const Value: Variant);

    function GetArrayItem(index: Integer): TJsonDataValueHelper; inline;
    function GetArrayCount: Integer; inline;

    function GetObjectString(const Name: string): string; inline;
    function GetObjectInt(const Name: string): Integer; inline;
    function GetObjectLong(const Name: string): Int64; inline;
    function GetObjectULong(const Name: string): UInt64; inline;
    function GetObjectFloat(const Name: string): Double; inline;
    function GetObjectDateTime(const Name: string): TDateTime; inline;
    function GetObjectBool(const Name: string): Boolean; inline;
    function GetArray(const Name: string): TJsonArray; inline;
    function GetObject(const Name: string): TJsonDataValueHelper; inline;
    function GetObjectVariant(const Name: string): Variant; inline;
    procedure SetObjectString(const Name, Value: string); inline;
    procedure SetObjectInt(const Name: string; const Value: Integer); inline;
    procedure SetObjectLong(const Name: string; const Value: Int64); inline;
    procedure SetObjectULong(const Name: string; const Value: UInt64); inline;
    procedure SetObjectFloat(const Name: string; const Value: Double); inline;
    procedure SetObjectDateTime(const Name: string; const Value: TDateTime); inline;
    procedure SetObjectBool(const Name: string; const Value: Boolean); inline;
    procedure SetArray(const Name: string; const Value: TJsonArray); inline;
    procedure SetObject(const Name: string; const Value: TJsonDataValueHelper); inline;
    procedure SetObjectVariant(const Name: string; const Value: Variant); inline;

    function GetObjectPath(const Name: string): TJsonDataValueHelper; inline;
    procedure SetObjectPath(const Name: string; const Value: TJsonDataValueHelper); inline;

    function GetTyp: TJsonDataType;
    procedure ResolveName;
    class procedure SetInternValue(Item: PJsonDataValue; const Value: TJsonDataValueHelper); static;
  public
    class operator Implicit(const Value: string): TJsonDataValueHelper; overload;
    class operator Implicit(const Value: TJsonDataValueHelper): string; overload;
    class operator Implicit(const Value: Integer): TJsonDataValueHelper; overload;
    class operator Implicit(const Value: TJsonDataValueHelper): Integer; overload;
    class operator Implicit(const Value: Int64): TJsonDataValueHelper; overload;
    class operator Implicit(const Value: TJsonDataValueHelper): Int64; overload;
    //class operator Implicit(const Value: UInt64): TJsonDataValueHelper; overload;  conflicts with Int64 operator
    //class operator Implicit(const Value: TJsonDataValueHelper): UInt64; overload;  conflicts with Int64 operator
    class operator Implicit(const Value: Double): TJsonDataValueHelper; overload;
    class operator Implicit(const Value: TJsonDataValueHelper): Double; overload;
    class operator Implicit(const Value: Extended): TJsonDataValueHelper; overload;
    class operator Implicit(const Value: TJsonDataValueHelper): Extended; overload;
    class operator Implicit(const Value: TDateTime): TJsonDataValueHelper; overload;
    class operator Implicit(const Value: TJsonDataValueHelper): TDateTime; overload;
    class operator Implicit(const Value: Boolean): TJsonDataValueHelper; overload;
    class operator Implicit(const Value: TJsonDataValueHelper): Boolean; overload;
    class operator Implicit(const Value: TJsonArray): TJsonDataValueHelper; overload;
    class operator Implicit(const Value: TJsonDataValueHelper): TJsonArray; overload;
    class operator Implicit(const Value: TJsonObject): TJsonDataValueHelper; overload;
    class operator Implicit(const Value: TJsonDataValueHelper): TJsonObject; overload;
    class operator Implicit(const Value: Pointer): TJsonDataValueHelper; overload;
    class operator Implicit(const Value: TJsonDataValueHelper): Variant; overload;
    class operator Implicit(const Value: Variant): TJsonDataValueHelper; overload;

    function IsNull: Boolean;
    
    property Typ: TJsonDataType read GetTyp;
    property Value: string read GetValue write SetValue;
    property IntValue: Integer read GetIntValue write SetIntValue;
    property LongValue: Int64 read GetLongValue write SetLongValue;
    property ULongValue: UInt64 read GetULongValue write SetULongValue;
    property FloatValue: Double read GetFloatValue write SetFloatValue;
    property DateTimeValue: TDateTime read GetDateTimeValue write SetDateTimeValue;
    property BoolValue: Boolean read GetBoolValue write SetBoolValue;
    property ArrayValue: TJsonArray read GetArrayValue write SetArrayValue;
    property ObjectValue: TJsonObject read GetObjectValue write SetObjectValue;
    property VariantValue: Variant read GetVariantValue write SetVariantValue;

    // Access to array item count
    property Count: Integer read GetArrayCount;
    // Access to array items
    property Items[index: Integer]: TJsonDataValueHelper read GetArrayItem;

    property s[const Name: string]: string read GetObjectString write SetObjectString;        // returns '' if property doesn't exist, auto type-cast except for array/object
    property i[const Name: string]: Integer read GetObjectInt write SetObjectInt;             // returns 0 if property doesn't exist, auto type-cast except for array/object
    property L[const Name: string]: Int64 read GetObjectLong write SetObjectLong;             // returns 0 if property doesn't exist, auto type-cast except for array/object
    property u[const Name: string]: UInt64 read GetObjectULong write SetObjectULong;          // returns 0 if property doesn't exist, auto type-cast except for array/object
    property f[const Name: string]: Double read GetObjectFloat write SetObjectFloat;          // returns 0 if property doesn't exist, auto type-cast except for array/object
    property d[const Name: string]: TDateTime read GetObjectDateTime write SetObjectDateTime; // returns 0 if property doesn't exist, auto type-cast except for array/object
    property b[const Name: string]: Boolean read GetObjectBool write SetObjectBool;           // returns false if property doesn't exist, auto type-cast with "<>'true'" and "<>0" except for array/object
    // Used to auto create arrays
    property a[const Name: string]: TJsonArray read GetArray write SetArray;
    // Used to auto create objects and as default property where no Implicit operator matches
    property O[const Name: string]: TJsonDataValueHelper read GetObject write SetObject; default;
    property v[const Name: string]: Variant read GetObjectVariant write SetObjectVariant;

    property Path[const Name: string]: TJsonDataValueHelper read GetObjectPath write SetObjectPath;
  private
    FData: record // hide the data from CodeInsight (bug in CodeInsight)
      FIntern: PJsonDataValue;
      FName: string;
      FNameResolver: TJsonObject;
      FValue: string; // must be managed by Delphi otherwise we have a memory leak
      {$IFDEF AUTOREFCOUNT}
      FObj: TJsonBaseObject;
      {$ENDIF AUTOREFCOUNT}
      case FTyp: TJsonDataType of
        jdtInt: (FIntValue: Integer);
        jdtLong: (FLongValue: Int64);
        jdtULong: (FULongValue: UInt64);
        jdtFloat: (FFloatValue: Double);
        jdtDateTime: (FDateTimeValue: TDateTime);
        jdtBool: (FBoolValue: Boolean);
        {$IFNDEF AUTOREFCOUNT}
        jdtObject: (FObj: TJsonBaseObject); // used for both Array and Object
        //jdtArray: (FArrayValue: TJsonArray);
        //jdtObject: (FObjectValue: TJsonObject);
        {$ENDIF AUTOREFCOUNT}
    end;
  end;

  // TJsonBaseObject is the base class for TJsonArray and TJsonObject
  TJsonBaseObject = class abstract(TObject)
  private type
    TWriterAppendMethod = procedure(p: PChar; Len: Integer) of object;
    TStreamInfo = record
      buffer: PByte;
      Size: NativeInt;
      AllocationBase: Pointer;
    end;
  private
    class procedure StrToJSONStr(const AppendMethod: TWriterAppendMethod; const s: string); static;
    class procedure EscapeStrToJSONStr(f, p, EndP: PChar; const AppendMethod: TWriterAppendMethod); static;
    class procedure DateTimeToJSONStr(const AppendMethod: TWriterAppendMethod;
      const Value: TDateTime); static;
    class procedure InternInitAndAssignItem(dest, Source: PJsonDataValue); static;
    class procedure GetStreamBytes(stream: TStream; var Encoding: TEncoding; Utf8WithoutBOM: Boolean;
      var StreamInfo: TStreamInfo); static;

    {$IFDEF USE_FAST_AUTOREFCOUNT}
    function ARCObjRelease: Integer; inline;
    function ARCObjAddRef: Integer; inline;
    {$ENDIF USE_FAST_AUTOREFCOUNT}
  protected
    procedure InternToJSON(var Writer: TJsonOutputWriter); virtual; abstract;
  public
    const DataTypeNames: array[TJsonDataType] of string = (
      'null', 'String', 'Integer', 'Long', 'ULong', 'Float', 'DateTime', 'Bool', 'Array', 'Object'
    );

    {$IFDEF USE_FAST_NEWINSTANCE}
    class function NewInstance: TObject {$IFDEF AUTOREFCOUNT} unsafe {$ENDIF}; override;
    {$ENDIF USE_FAST_NEWINSTANCE}

    // ParseXxx returns nil if the JSON string is empty or consists only of white chars.
    // If the JSON string starts with a "[" then the returned object is a TJsonArray otherwise
    // it is a TJsonObject.
    class function ParseUtf8(s: PAnsiChar; Len: Integer = -1{$IFDEF SUPPORT_PROGRESS}; AProgress: PJsonReaderProgressRec = nil{$ENDIF}): TJsonBaseObject; overload; static; inline;
    {$IFDEF SUPPORTS_UTF8STRING}
    class function ParseUtf8(const s: UTF8String{$IFDEF SUPPORT_PROGRESS}; AProgress: PJsonReaderProgressRec = nil{$ENDIF}): TJsonBaseObject; overload; static; inline;
    {$ENDIF SUPPORTS_UTF8STRING}
    class function ParseUtf8Bytes(s: PByte; Len: Integer = -1{$IFDEF SUPPORT_PROGRESS}; AProgress: PJsonReaderProgressRec = nil{$ENDIF}): TJsonBaseObject; static;
    class function Parse(s: PWideChar; Len: Integer = -1{$IFDEF SUPPORT_PROGRESS}; AProgress: PJsonReaderProgressRec = nil{$ENDIF}): TJsonBaseObject; overload; static;
    class function Parse(const s: UnicodeString{$IFDEF SUPPORT_PROGRESS}; AProgress: PJsonReaderProgressRec = nil{$ENDIF}): TJsonBaseObject; overload; static; inline;
    class function Parse(const Bytes: TBytes; Encoding: TEncoding = nil; ByteIndex: Integer = 0;
      ByteCount: Integer = -1{$IFDEF SUPPORT_PROGRESS}; AProgress: PJsonReaderProgressRec = nil{$ENDIF}): TJsonBaseObject; overload; static;
    class function ParseFromFile(const FileName: string; Utf8WithoutBOM: Boolean = True{$IFDEF SUPPORT_PROGRESS}; AProgress: PJsonReaderProgressRec = nil{$ENDIF}): TJsonBaseObject; static;
    class function ParseFromStream(stream: TStream; Encoding: TEncoding = nil; Utf8WithoutBOM: Boolean = True{$IFDEF SUPPORT_PROGRESS}; AProgress: PJsonReaderProgressRec = nil{$ENDIF}): TJsonBaseObject; static;

    procedure LoadFromFile(const FileName: string; Utf8WithoutBOM: Boolean = True{$IFDEF SUPPORT_PROGRESS}; AProgress: PJsonReaderProgressRec = nil{$ENDIF});
    procedure LoadFromStream(stream: TStream; Encoding: TEncoding = nil; Utf8WithoutBOM: Boolean = True{$IFDEF SUPPORT_PROGRESS}; AProgress: PJsonReaderProgressRec = nil{$ENDIF});
    procedure SaveToFile(const FileName: string; Compact: Boolean = True; Encoding: TEncoding = nil; Utf8WithoutBOM: Boolean = True);
    procedure SaveToStream(stream: TStream; Compact: Boolean = True; Encoding: TEncoding = nil; Utf8WithoutBOM: Boolean = True);
    procedure SaveToLines(Lines: TStrings);

    // FromXxxJSON() raises an EJsonParserException if you try to parse an array JSON string into a
    // TJsonObject or a object JSON string into a TJsonArray.
    {$IFDEF SUPPORTS_UTF8STRING}
    procedure FromUtf8JSON(const s: UTF8String{$IFDEF SUPPORT_PROGRESS}; AProgress: PJsonReaderProgressRec = nil{$ENDIF}); overload; inline;
    {$ENDIF SUPPORTS_UTF8STRING}
    procedure FromUtf8JSON(s: PAnsiChar; Len: Integer = -1{$IFDEF SUPPORT_PROGRESS}; AProgress: PJsonReaderProgressRec = nil{$ENDIF}); overload; inline;
    procedure FromUtf8JSON(s: PByte; Len: Integer = -1{$IFDEF SUPPORT_PROGRESS}; AProgress: PJsonReaderProgressRec = nil{$ENDIF}); overload;
    procedure FromJSON(const s: UnicodeString{$IFDEF SUPPORT_PROGRESS}; AProgress: PJsonReaderProgressRec = nil{$ENDIF}); overload;
    procedure FromJSON(s: PWideChar; Len: Integer = -1{$IFDEF SUPPORT_PROGRESS}; AProgress: PJsonReaderProgressRec = nil{$ENDIF}); overload;

    function ToJSON(Compact: Boolean = True): string;
    {$IFDEF SUPPORTS_UTF8STRING}
    function ToUtf8JSON(Compact: Boolean = True): UTF8String; overload;
    {$ENDIF SUPPORTS_UTF8STRING}
    procedure ToUtf8JSON(var Bytes: TBytes; Compact: Boolean = True); {$IFDEF SUPPORTS_UTF8STRING}overload;{$ENDIF}
    // ToString() returns a compact JSON string
    function ToString: string; override;

    function Clone: TJsonBaseObject; virtual; abstract;

    class function JSONToDateTime(const Value: string): TDateTime; static;
    class function DateTimeToJSON(const Value: TDateTime; UseUtcTime: Boolean): string; static;
  end;

  PJsonDataValueArray = ^TJsonDataValueArray;
  TJsonDataValueArray = array[0..MaxInt div SizeOf(TJsonDataValue) - 1] of TJsonDataValue;

  TJsonArrayEnumerator = class(TObject)
  private
    FIndex: Integer;
    FArray: TJsonArray;
  public
    constructor Create(AArray: TJsonArray);

    function GetCurrent: TJsonDataValueHelper; inline;
    function MoveNext: Boolean;
    property Current: TJsonDataValueHelper read GetCurrent;
  end;

  // TJsonArray hold a JSON array and manages the array elements.
  TJsonArray = class {$IFDEF USE_FAST_NEWINSTANCE}sealed{$ENDIF}(TJsonBaseObject)
  private
    FItems: PJsonDataValueArray;
    FCapacity: Integer;
    FCount: Integer;
    function GetString(index: Integer): string; inline;
    function GetInt(index: Integer): Integer; inline;
    function GetLong(index: Integer): Int64; inline;
    function GetULong(index: Integer): UInt64; inline;
    function GetFloat(index: Integer): Double; inline;
    function GetDateTime(index: Integer): TDateTime; inline;
    function GetBool(index: Integer): Boolean; inline;
    function GetArray(index: Integer): TJsonArray; inline;
    function GetObject(index: Integer): TJsonObject; inline;
    function GetVariant(index: Integer): Variant; inline;

    procedure SetString(index: Integer; const Value: string); inline;
    procedure SetInt(index: Integer; const Value: Integer); inline;
    procedure SetLong(index: Integer; const Value: Int64); inline;
    procedure SetULong(index: Integer; const Value: UInt64); inline;
    procedure SetFloat(index: Integer; const Value: Double); inline;
    procedure SetDateTime(index: Integer; const Value: TDateTime); inline;
    procedure SetBool(index: Integer; const Value: Boolean); inline;
    procedure SetArray(index: Integer; const Value: TJsonArray); inline;
    procedure SetObject(index: Integer; const Value: TJsonObject); inline;
    procedure SetVariant(index: Integer; const Value: Variant); inline;

    function GetItem(index: Integer): PJsonDataValue; inline;
    function GetType(index: Integer): TJsonDataType; inline;
    function GetValue(index: Integer): TJsonDataValueHelper;

    procedure SetValue(index: Integer; const Value: TJsonDataValueHelper);
    function AddItem: PJsonDataValue;
    function InsertItem(index: Integer): PJsonDataValue;

    procedure Grow;
    procedure InternApplyCapacity; inline;
    procedure SetCapacity(const Value: Integer);
    procedure SetCount(const Value: Integer);
  protected
    procedure InternToJSON(var Writer: TJsonOutputWriter); override;
    class procedure RaiseListError(index: Integer); static;
  public
    destructor Destroy; override;

    procedure Clear;
    procedure Delete(index: Integer);
    // Extract removes the object/array from the array and transfers the ownership to the caller.
    function Extract(index: Integer): TJsonBaseObject;
    function ExtractArray(index: Integer): TJsonArray;
    function ExtractObject(index: Integer): TJsonObject;
    procedure Assign(aSource: TJsonArray);
    function Clone: TJsonBaseObject; override;

    procedure Add(const AValue: string); overload;
    procedure Add(const AValue: Integer); overload;
    procedure Add(const AValue: Int64); overload;
    procedure Add(const AValue: UInt64); overload;
    procedure AddF(const AValue: Double); overload;
    procedure AddT(const AValue: TDateTime); overload;
    procedure Add(const AValue: Boolean); overload;
    procedure Add(const AValue: TJsonArray); overload;
    procedure Add(const AValue: TJsonObject); overload;
    procedure AddV(const AValue: Variant); overload;
    function AddArray: TJsonArray;
    function AddObject: TJsonObject; overload;
    procedure AddObject(const Value: TJsonObject); overload; inline; // makes it easier to add "null"

    procedure Insert(index: Integer; const AValue: string); overload;
    procedure Insert(index: Integer; const AValue: Integer); overload;
    procedure Insert(index: Integer; const AValue: Int64); overload;
    procedure Insert(index: Integer; const AValue: UInt64); overload;
    procedure InsertF(index: Integer; const AValue: Double); overload;
    procedure InsertT(index: Integer; const AValue: TDateTime); overload;
    procedure Insert(index: Integer; const AValue: Boolean); overload;
    procedure Insert(index: Integer; const AValue: TJsonArray); overload;
    procedure Insert(index: Integer; const AValue: TJsonObject); overload;
    procedure InsertV(index: Integer; const AValue: Variant); overload;
    function InsertArray(index: Integer): TJsonArray;
    function InsertObject(index: Integer): TJsonObject; overload;
    procedure InsertObject(index: Integer; const Value: TJsonObject); overload; inline; // makes it easier to insert "null"

    function GetEnumerator: TJsonArrayEnumerator;
    function IsNull(index: Integer): Boolean;

    property Types[index: Integer]: TJsonDataType read GetType;
    property values[index: Integer]: TJsonDataValueHelper read GetValue write SetValue; default;

    // Short names
    property s[index: Integer]: string read GetString write SetString;
    property i[index: Integer]: Integer read GetInt write SetInt;
    property L[index: Integer]: Int64 read GetLong write SetLong;
    property u[index: Integer]: UInt64 read GetULong write SetULong;
    property f[index: Integer]: Double read GetFloat write SetFloat;
    property d[index: Integer]: TDateTime read GetDateTime write SetDateTime;
    property b[index: Integer]: Boolean read GetBool write SetBool;
    property a[index: Integer]: TJsonArray read GetArray write SetArray;
    property O[index: Integer]: TJsonObject read GetObject write SetObject;
    property v[index: Integer]: Variant read GetVariant write SetVariant;

    property Items[index: Integer]: PJsonDataValue read GetItem;
    property Count: Integer read FCount write SetCount;
    property Capacity: Integer read FCapacity write SetCapacity;
  end;

  TJsonNameValuePair = record
    Name: string;
    Value: TJsonDataValueHelper;
  end;

  TJsonObjectEnumerator = class(TObject)
  protected
    FIndex: Integer;
    FObject: TJsonObject;
  public
    constructor Create(AObject: TJsonObject);

    function GetCurrent: TJsonNameValuePair; inline;
    function MoveNext: Boolean;
    property Current: TJsonNameValuePair read GetCurrent;
  end;

  // TJsonObject hold a JSON object and manages the JSON object properties
  TJsonObject = class {$IFDEF USE_FAST_NEWINSTANCE}sealed{$ENDIF}(TJsonBaseObject)
  private type
    PJsonStringArray = ^TJsonStringArray;
    TJsonStringArray = array[0..MaxInt div SizeOf(string) - 1] of string;
  private
    FItems: PJsonDataValueArray;
    FNames: PJsonStringArray;
    FCapacity: Integer;
    FCount: Integer;
    {$IFDEF USE_LAST_NAME_STRING_LITERAL_CACHE}
    FLastValueItem: PJsonDataValue;
    FLastValueItemNamePtr: Pointer;
    procedure UpdateLastValueItem(const Name: string; Item: PJsonDataValue);
    {$ENDIF USE_LAST_NAME_STRING_LITERAL_CACHE}
    function FindItem(const Name: string; var Item: PJsonDataValue): Boolean;
    function RequireItem(const Name: string): PJsonDataValue;

    function GetString(const Name: string): string;
    function GetBool(const Name: string): Boolean;
    function GetInt(const Name: string): Integer;
    function GetLong(const Name: string): Int64;
    function GetULong(const Name: string): UInt64;
    function GetFloat(const Name: string): Double;
    function GetDateTime(const Name: string): TDateTime;
    function GetObject(const Name: string): TJsonObject;
    function GetArray(const Name: string): TJsonArray;
    procedure SetString(const Name, Value: string);
    procedure SetBool(const Name: string; const Value: Boolean);
    procedure SetInt(const Name: string; const Value: Integer);
    procedure SetLong(const Name: string; const Value: Int64);
    procedure SetULong(const Name: string; const Value: UInt64);
    procedure SetFloat(const Name: string; const Value: Double);
    procedure SetDateTime(const Name: string; const Value: TDateTime);
    procedure SetObject(const Name: string; const Value: TJsonObject);
    procedure SetArray(const Name: string; const Value: TJsonArray);

    function GetType(const Name: string): TJsonDataType;
    function GetName(index: Integer): string; inline;
    function GetItem(index: Integer): PJsonDataValue; inline;
    procedure SetValue(const Name: string; const Value: TJsonDataValueHelper);
    function GetValue(const Name: string): TJsonDataValueHelper;

    { Used from the reader, never every use them outside the reader, they may crash your strings }
    procedure InternAdd(var AName: string; const AValue: string); overload;
    procedure InternAdd(var AName: string; const AValue: Integer); overload;
    procedure InternAdd(var AName: string; const AValue: Int64); overload;
    procedure InternAdd(var AName: string; const AValue: UInt64); overload;
    procedure InternAdd(var AName: string; const AValue: Double); overload;
    procedure InternAdd(var AName: string; const AValue: TDateTime); overload;
    procedure InternAdd(var AName: string; const AValue: Boolean); overload;
    procedure InternAdd(var AName: string; const AValue: TJsonArray); overload;
    procedure InternAdd(var AName: string; const AValue: TJsonObject); overload;
    function InternAddArray(var AName: string): TJsonArray;
    function InternAddObject(var AName: string): TJsonObject;

    function InternAddItem(var Name: string): PJsonDataValue;
    function AddItem(const Name: string): PJsonDataValue;

    procedure Grow;
    procedure InternApplyCapacity;
    procedure SetCapacity(const Value: Integer);
    function GetPath(const NamePath: string): TJsonDataValueHelper;
    procedure SetPath(const NamePath: string; const Value: TJsonDataValueHelper);
    function IndexOfPChar(s: PChar; Len: Integer): Integer;
    procedure PathError(p, EndP: PChar);
    procedure PathNullError(p, EndP: PChar);
    procedure PathIndexError(p, EndP: PChar; Count: Integer);
  protected
    procedure InternToJSON(var Writer: TJsonOutputWriter); override;
    function FindCaseInsensitiveItem(const ACaseInsensitiveName: string): PJsonDataValue;
  public
    destructor Destroy; override;
    procedure Assign(aSource: TJsonObject);
    function Clone: TJsonBaseObject; override;

    // ToSimpleObject() maps the JSON object properties to the Delphi object by using the object's
    // TypeInfo.
    // The object's class must be compiled with the $M+ compiler switch or derive from TPersistent.
    procedure ToSimpleObject(AObject: TObject; ACaseSensitive: Boolean = True);
    // FromSimpleObject() clears the JSON object and adds the Delphi object's properties.
    // The object's class must be compiled with the $M+ compiler switch or derive from TPersistent.
    procedure FromSimpleObject(AObject: TObject; ALowerCamelCase: Boolean = False);

    procedure Clear;
    procedure Remove(const Name: string);
    procedure Delete(index: Integer);
    function IndexOf(const Name: string): Integer;
    function Contains(const Name: string): Boolean;
    // Extract removes the object/array from the object and transfers the ownership to the caller.
    function Extract(const Name: string): TJsonBaseObject;
    function ExtractArray(const Name: string): TJsonArray;
    function ExtractObject(const Name: string): TJsonObject;

    function GetEnumerator: TJsonObjectEnumerator;
    function IsNull(const Name: string): Boolean;

    property Types[const Name: string]: TJsonDataType read GetType;
    property values[const Name: string]: TJsonDataValueHelper read GetValue write SetValue; default;

    // Short names
    property s[const Name: string]: string read GetString write SetString;        // returns '' if property doesn't exist, auto type-cast except for array/object
    property i[const Name: string]: Integer read GetInt write SetInt;             // returns 0 if property doesn't exist, auto type-cast except for array/object
    property L[const Name: string]: Int64 read GetLong write SetLong;             // returns 0 if property doesn't exist, auto type-cast except for array/object
    property u[const Name: string]: UInt64 read GetULong write SetULong;          // returns 0 if property doesn't exist, auto type-cast except for array/object
    property f[const Name: string]: Double read GetFloat write SetFloat;          // returns 0 if property doesn't exist, auto type-cast except for array/object
    property d[const Name: string]: TDateTime read GetDateTime write SetDateTime; // returns 0 if property doesn't exist, auto type-cast except for array/object
    property b[const Name: string]: Boolean read GetBool write SetBool;           // returns false if property doesn't exist, auto type-cast with "<>'true'" and "<>0" except for array/object
    property a[const Name: string]: TJsonArray read GetArray write SetArray;      // auto creates array on first access
    property O[const Name: string]: TJsonObject read GetObject write SetObject;   // auto creates object on first access

    property Path[const NamePath: string]: TJsonDataValueHelper read GetPath write SetPath;

    // Indexed access to the named properties
    property Names[index: Integer]: string read GetName;
    property Items[index: Integer]: PJsonDataValue read GetItem;
    property Count: Integer read FCount;
    property Capacity: Integer read FCapacity write SetCapacity;
  end;

  TJsonSerializationConfig = record
    LineBreak: string;
    IndentChar: string;
    UseUtcTime: Boolean;
    NullConvertsToValueTypes: Boolean;
  end;

  // Rename classes because RTL classes have the same name
  TJDOJsonBaseObject = TJsonBaseObject;
  TJDOJsonObject = TJsonObject;
  TJDOJsonArray = TJsonArray;

var
  JsonSerializationConfig: TJsonSerializationConfig = ( // not thread-safe
    LineBreak: #10;
    IndentChar: #9;
    UseUtcTime: True;
    NullConvertsToValueTypes: False;  // If True and an object is nil/null, a convertion to String, Int, Long, Float, DateTime, Boolean will return ''/0/False
  );

implementation

uses
  {$IFDEF HAS_UNIT_SCOPE}
    {$IFDEF MSWINDOWS}
  Winapi.Windows,
    {$ELSE}
  System.DateUtils,
    {$ENDIF MSWINDOWS}
  System.Variants, System.RTLConsts, System.TypInfo, System.Math, System.SysConst;

  {$ELSE}

    {$IFDEF MSWINDOWS}
  Windows,
    {$ELSE}
  DateUtils,
    {$ENDIF MSWINDOWS}
  Variants, RTLConsts, TypInfo, Math, SysConst;
  {$ENDIF HAS_UNIT_SCOPE}

{$IF SizeOf(LongWord) <> 4}
// Make LongWord on all platforms a UInt32.
type
  LongWord = UInt32;
  PLongWord = ^LongWord;
{$IFEND}

resourcestring
  RsUnsupportedFileEncoding = 'File encoding is not supported';
  RsUnexpectedEndOfFile = 'Unexpected end of file where %s was expected';
  RsUnexpectedToken = 'Expected %s but found %s';
  RsInvalidStringCharacter = 'Invalid character in string';
  RsStringNotClosed = 'String not closed';
  RsInvalidHexNumber = 'Invalid hex number "%s"';
  RsTypeCastError = 'Cannot cast %s into %s';
  RsMissingClassInfo = 'Class "%s" doesn''t have type information. {$M+} was not specified';
  RsInvalidJsonPath = 'Invalid JSON path "%s"';
  RsJsonPathContainsNullValue = 'JSON path contains null value ("%s")';
  RsJsonPathIndexError = 'JSON path index out of bounds (%d) "%s"';
  RsVarTypeNotSupported = 'VarType %d is not supported';
  {$IFDEF USE_FAST_STRASG_FOR_INTERNAL_STRINGS}
    {$IFDEF DEBUG}
  //RsInternAsgStringUsageError = 'InternAsgString was called on a string literal';
    {$ENDIF DEBUG}
  {$ENDIF USE_FAST_STRASG_FOR_INTERNAL_STRINGS}

type
  TJsonTokenKind = (
    jtkEof, jtkInvalidSymbol,
    jtkLBrace, jtkRBrace, jtkLBracket, jtkRBracket, jtkComma, jtkColon,
    jtkIdent,
    jtkValue, jtkString, jtkInt, jtkLong, jtkULong, jtkFloat, jtkTrue, jtkFalse, jtkNull
  );

const
  JsonTokenKindToStr: array[TJsonTokenKind] of string = (
    'end of file', 'invalid symbol',
    '"{"', '"}"', '"["', '"]"', '","', '":"',
    'identifier',
    'value', 'value', 'value', 'value', 'value', 'value', 'value', 'value', 'value'
  );

  Power10: array[0..18] of Double = (
    1E0, 1E1, 1E2, 1E3, 1E4, 1E5, 1E6, 1E7, 1E8, 1E9,
    1E10, 1E11, 1E12, 1E13, 1E14, 1E15, 1E16, 1E17, 1E18
  );

  // XE7 broke string literal collapsing
var
  sTrue: string = 'true';
  sFalse: string = 'false';
const
  sNull = 'null';
  sQuoteChar = '"';

  {$IF not declared(varObject)}
  varObject = $0049;
  {$IFEND}

type
  PStrRec = ^TStrRec;
  TStrRec = packed record
    {$IF defined(CPUX64) or defined(CPU64BITS)} // XE2-XE7 (CPUX64), XE8+ (CPU64BITS)
    _Padding: Integer;
    {$IFEND}
    CodePage: Word;
    ElemSize: Word;
    RefCnt: Integer;
    length: Integer;
  end;

  // TEncodingStrictAccess gives us access to the strict protected functions which are much easier
  // to use with TJsonStringBuilder than converting FData to a dynamic TCharArray.
  TEncodingStrictAccess = class(TEncoding)
  public
    function GetByteCountEx(Chars: PChar; CharCount: Integer): Integer; inline;
    function GetBytesEx(Chars: PChar; CharCount: Integer; Bytes: PByte; ByteCount: Integer): Integer; inline;
    function GetCharCountEx(Bytes: PByte; ByteCount: Integer): Integer; inline;
    function GetCharsEx(Bytes: PByte; ByteCount: Integer; Chars: PChar; CharCount: Integer): Integer; inline;
  end;

  {$IFDEF USE_STRINGINTERN_FOR_NAMES}
  TStringIntern = record
  private type
    PJsonStringEntry = ^TJsonStringEntry;
    TJsonStringEntry = record
      Next: Integer;
      hash: Integer;
      Name: string;
    end;

    PJsonStringEntryArray = ^TJsonStringEntryArray;
    TJsonStringEntryArray = array[0..MaxInt div SizeOf(TJsonStringEntry) - 1] of TJsonStringEntry;

    PJsonIntegerArray = ^TJsonIntegerArray;
    TJsonIntegerArray = array[0..MaxInt div SizeOf(Integer) - 1] of Integer;
  private
    FStrings: PJsonStringEntryArray;
    FBuckets: PJsonIntegerArray;
    FCapacity: Integer;
    FCount: Integer;
    class function GetHash(const Name: string): Integer; static;
    procedure Grow;
    function Find(hash: Integer; const s: string): Integer;
    procedure InternAdd(AHash: Integer; const s: string);
  public
    procedure Init;
    procedure Done;
    procedure Intern(var s: string; var PropName: string);
  end;
  {$ENDIF USE_STRINGINTERN_FOR_NAMES}

  TJsonToken = record
    Kind: TJsonTokenKind;
    s: string; // jtkIdent/jtkString
    case Integer of
      0: (i: Integer; Hi: Integer);
      1: (L: Int64);
      2: (u: UInt64);
      3: (f: Double);
  end;

  TJsonReader = class(TObject)
  private
    {$IFDEF USE_STRINGINTERN_FOR_NAMES}
    FIdents: TStringIntern;
    {$ENDIF USE_STRINGINTERN_FOR_NAMES}
    FPropName: string;
    procedure Accept(TokenKind: TJsonTokenKind);
    procedure ParseObjectBody(const Data: TJsonObject);
    procedure ParseObjectProperty(const Data: TJsonObject);
    procedure ParseObjectPropertyValue(const Data: TJsonObject);
    procedure ParseArrayBody(const Data: TJsonArray);
    procedure ParseArrayPropertyValue(const Data: TJsonArray);
    procedure AcceptFailed(TokenKind: TJsonTokenKind);
  protected
    FLook: TJsonToken;
    FLineNum: Integer;
    FStart: Pointer;
    FLineStart: Pointer;
    {$IFDEF SUPPORT_PROGRESS}
    FLastProgressValue: NativeInt;
    FSize: NativeInt;
    FProgress: PJsonReaderProgressRec;
    procedure CheckProgress(Position: Pointer);
    {$ENDIF SUPPORT_PROGRESS}
    function GetLineColumn: NativeInt;
    function GetPosition: NativeInt;
    function GetCharOffset(StartPos: Pointer): NativeInt; virtual; abstract;
    function Next: Boolean; virtual; abstract;

    class procedure InvalidStringCharacterError(const Reader: TJsonReader); static;
    class procedure StringNotClosedError(const Reader: TJsonReader); static;
    class procedure JSONStrToStr(p, EndP: PChar; FirstEscapeIndex: Integer; var s: string;
      const Reader: TJsonReader); static;
    class procedure JSONUtf8StrToStr(p, EndP: PByte; FirstEscapeIndex: Integer; var s: string;
      const Reader: TJsonReader); static;
  public
    {$IFDEF USE_FAST_NEWINSTANCE}
    class function NewInstance: TObject {$IFDEF AUTOREFCOUNT} unsafe {$ENDIF}; override;
    procedure FreeInstance; override;
    {$ENDIF USE_FAST_NEWINSTANCE}

    constructor Create(AStart: Pointer{$IFDEF SUPPORT_PROGRESS}; ASize: NativeInt; AProgress: PJsonReaderProgressRec{$ENDIF});
    destructor Destroy; override;
    procedure Parse(Data: TJsonBaseObject);
  end;

  TUtf8JsonReader = class sealed(TJsonReader)
  private
    FText: PByte;
    FTextEnd: PByte;
  protected
    function GetCharOffset(StartPos: Pointer): NativeInt; override; final;
    function Next: Boolean; override; final;
    // ARM optimization: Next() already has EndP in a local variable so don't use the slow indirect
    // access to FTextEnd.
    procedure LexString(p: PByte{$IFDEF CPUARM}; EndP: PByte{$ENDIF});
    procedure LexNumber(p: PByte{$IFDEF CPUARM}; EndP: PByte{$ENDIF});
    procedure LexIdent(p: PByte{$IFDEF CPUARM}; EndP: PByte{$ENDIF});
  public
    constructor Create(s: PByte; Len: NativeInt{$IFDEF SUPPORT_PROGRESS}; AProgress: PJsonReaderProgressRec{$ENDIF});
  end;

  TStringJsonReader = class sealed(TJsonReader)
  private
    FText: PChar;
    FTextEnd: PChar;
  protected
    function GetCharOffset(StartPos: Pointer): NativeInt; override; final;
    function Next: Boolean; override; final;
    // ARM optimization: Next() already has EndP in a local variable so don't use the slow indirect
    // access to FTextEnd.
    procedure LexString(p: PChar{$IFDEF CPUARM}; EndP: PChar{$ENDIF});
    procedure LexNumber(p: PChar{$IFDEF CPUARM}; EndP: PChar{$ENDIF});
    procedure LexIdent(p: PChar{$IFDEF CPUARM}; EndP: PChar{$ENDIF});
  public
    constructor Create(s: PChar; Len: Integer{$IFDEF SUPPORT_PROGRESS}; AProgress: PJsonReaderProgressRec{$ENDIF});
  end;

  TMemoryStreamAccess = class(TMemoryStream);

  {$IFDEF SUPPORTS_UTF8STRING}
  TJsonUTF8StringStream = class(TMemoryStream)
  private
    FDataString: UTF8String;
  protected
    function Realloc(var NewCapacity: longint): Pointer; override;
  public
    constructor Create;
    property DataString: UTF8String read FDataString;
  end;
  {$ENDIF SUPPORTS_UTF8STRING}

  TJsonBytesStream = class(TMemoryStream)
  private
    FBytes: TBytes;
  protected
    function Realloc(var NewCapacity: longint): Pointer; override;
  public
    constructor Create;
    property Bytes: TBytes read FBytes;
  end;

var
  JSONFormatSettings: TFormatSettings;
  {$IFDEF USE_NAME_STRING_LITERAL}
  JsonMemInfoInitialized: Boolean = False;
  JsonMemInfoBlockStart: PByte = nil;
  JsonMemInfoBlockEnd: PByte = nil;
  JsonMemInfoMainBlockStart: PByte = nil;
  JsonMemInfoMainBlockEnd: PByte = nil;
  {$ENDIF USE_NAME_STRING_LITERAL}

{$IFDEF MSWINDOWS}

  {$IFDEF SUPPORT_WINDOWS2000}
var
  TzSpecificLocalTimeToSystemTime: function(lpTimeZoneInformation: PTimeZoneInformation;
    var lpLocalTime, lpUniversalTime: TSystemTime): BOOL; stdcall;

function TzSpecificLocalTimeToSystemTimeWin2000(lpTimeZoneInformation: PTimeZoneInformation;
  var lpLocalTime, lpUniversalTime: TSystemTime): BOOL; stdcall;
var
  TimeZoneInfo: TTimeZoneInformation;
begin
  if lpTimeZoneInformation <> nil then
    TimeZoneInfo := lpTimeZoneInformation^
  else
    GetTimeZoneInformation(TimeZoneInfo);

  // Reverse the bias so that SystemTimeToTzSpecificLocalTime becomes TzSpecificLocalTimeToSystemTime
  TimeZoneInfo.Bias := -TimeZoneInfo.Bias;
  TimeZoneInfo.StandardBias := -TimeZoneInfo.StandardBias;
  TimeZoneInfo.DaylightBias := -TimeZoneInfo.DaylightBias;

  Result := SystemTimeToTzSpecificLocalTime(@TimeZoneInfo, lpLocalTime, lpUniversalTime);
end;
  {$ELSE}
function TzSpecificLocalTimeToSystemTime(lpTimeZoneInformation: PTimeZoneInformation;
  var lpLocalTime, lpUniversalTime: TSystemTime): BOOL; stdcall;
  external kernel32 Name 'TzSpecificLocalTimeToSystemTime';
  {$ENDIF SUPPORT_WINDOWS2000}

{$ENDIF MSWINDOWS}

{$IFDEF USE_NAME_STRING_LITERAL}
procedure InitializeJsonMemInfo;
var
  MemInfo: TMemoryBasicInformation;
begin
  JsonMemInfoInitialized := True;
  if VirtualQuery(PByte(HInstance + $1000), MemInfo, SizeOf(MemInfo)) = SizeOf(MemInfo) then
  begin
    JsonMemInfoBlockStart := MemInfo.AllocationBase;
    JsonMemInfoBlockEnd := JsonMemInfoBlockStart + MemInfo.RegionSize;
  end;
  if HInstance <> MainInstance then
  begin
    if VirtualQuery(PByte(MainInstance + $1000), MemInfo, SizeOf(MemInfo)) = SizeOf(MemInfo) then
    begin
      JsonMemInfoMainBlockStart := MemInfo.AllocationBase;
      JsonMemInfoMainBlockEnd := JsonMemInfoBlockStart + MemInfo.RegionSize;
    end;
  end;
end;
{$ENDIF USE_NAME_STRING_LITERAL}

{ EJsonParserSyntaxException }

constructor EJsonParserException.CreateResFmt(ResStringRec: PResStringRec; const Args: array of const;
  ALineNum, aColumn, APosition: NativeInt);
begin
  inherited CreateResFmt(ResStringRec, Args);
  FLineNum := ALineNum;
  FColumn := aColumn;
  FPosition := APosition;
  if FLineNum > 0 then
    Message := Format('%s (%d, %d)', [Message, FLineNum, FColumn]);
end;

constructor EJsonParserException.CreateRes(ResStringRec: PResStringRec; ALineNum, aColumn, APosition: NativeInt);
begin
  inherited CreateRes(ResStringRec);
  FLineNum := ALineNum;
  FColumn := aColumn;
  FPosition := APosition;
  if FLineNum > 0 then
    Message := Format('%s (%d, %d)', [Message, FLineNum, FColumn]);
end;

procedure ListError(Msg: PResStringRec; Data: Integer);
begin
  raise EStringListError.CreateFmt(LoadResString(Msg), [Data])
        {$IFDEF HAS_RETURN_ADDRESS} at ReturnAddress{$ENDIF};
end;

procedure ErrorNoMappingForUnicodeCharacter;
begin
  {$IF not declared(SNoMappingForUnicodeCharacter)}
  RaiseLastOSError;
  {$ELSE}
  raise EEncodingError.CreateRes(@SNoMappingForUnicodeCharacter)
        {$IFDEF HAS_RETURN_ADDRESS} at ReturnAddress{$ENDIF};
  {$IFEND}
end;

procedure ErrorUnsupportedVariantType(VarType: TVarType);
begin
  raise EJsonCastException.CreateResFmt(@RsVarTypeNotSupported, [VarType]);
end;

{$IFDEF USE_NAME_STRING_LITERAL}
procedure AsgString(var dest: string; const Source: string);
begin
  if (Pointer(Source) <> nil) and (PInteger(@PByte(Source)[-8])^ = -1) and // string literal
     (((PByte(Source) < JsonMemInfoBlockEnd) and (PByte(Source) >= JsonMemInfoBlockStart)) or
      ((PByte(Source) < JsonMemInfoMainBlockEnd) and (PByte(Source) >= JsonMemInfoMainBlockStart))) then
  begin
    // Save memory by just using the string literal but only if it is in the EXE's or this DLL's
    // code segment. Otherwise the memory could be released by a FreeLibrary call without us knowning.
    Pointer(dest) := Pointer(Source);
  end
  else
    dest := Source;
end;
{$ENDIF USE_NAME_STRING_LITERAL}

{$IFDEF USE_FAST_STRASG_FOR_INTERNAL_STRINGS}
  {$IFDEF DEBUG}
//procedure InternAsgStringUsageError;
//begin
//  raise EJsonException.CreateRes(@RsInternAsgStringUsageError);
//end;
  {$ENDIF DEBUG}
{$ENDIF USE_FAST_STRASG_FOR_INTERNAL_STRINGS}

procedure AnsiLowerCamelCaseString(var s: string);
begin
  s := AnsiLowerCase(PChar(s)^) + Copy(s, 2);
end;

{$IF not declared(TryStrToUInt64)}
function TryStrToUInt64(const s: string; out Value: UInt64): Boolean;
var
  p, EndP: PChar;
  v: UInt64;
  Digit: Integer;
begin
  // No support for hexadecimal strings

  p := PChar(s);
  EndP := p + length(s);
  // skip spaces
  while (p < EndP) and (p^ = ' ') do
    inc(p);
  if p^ = '-' then
    Result := False // UInt64 cannot be negative
  else
  begin
    v := 0;
    while p < EndP do
    begin
      Digit := Integer(Ord(p^)) - Ord('0');
      if (Cardinal(Digit) >= 10) or (v > high(UInt64) div 10) then
        Break;
      //V := V * 10 + Digit;
      v := (v shl 3) + (v shl 1) + Digit;
      inc(p);
    end;

    Result := p = EndP;
    if Result then
      Value := v;
  end;
end;
{$IFEND}

function GetHexDigitsUtf8(p: PByte; Count: Integer; const Reader: TJsonReader): LongWord;
var
  Ch: Byte;
begin
  Result := 0;
  while Count > 0 do
  begin
    Ch := p^;
    case p^ of
      Ord('0')..Ord('9'): Result := (Result shl 4) or LongWord(Ch - Ord('0'));
      Ord('A')..Ord('F'): Result := (Result shl 4) or LongWord(Ch - (Ord('A') - 10));
      Ord('a')..Ord('f'): Result := (Result shl 4) or LongWord(Ch - (Ord('a') - 10));
    else
      Break;
    end;
    inc(p);
    dec(Count);
  end;
  if Count > 0 then
    raise EJsonParserException.CreateResFmt(@RsInvalidHexNumber, [p^], Reader.FLineNum, Reader.GetLineColumn, Reader.GetPosition);
end;

function GetHexDigits(p: PChar; Count: Integer; const Reader: TJsonReader): LongWord;
var
  Ch: Char;
begin
  Result := 0;
  while Count > 0 do
  begin
    Ch := p^;
    case p^ of
      '0'..'9': Result := (Result shl 4) or LongWord(Ord(Ch) - Ord('0'));
      'A'..'F': Result := (Result shl 4) or LongWord(Ord(Ch) - (Ord('A') - 10));
      'a'..'f': Result := (Result shl 4) or LongWord(Ord(Ch) - (Ord('a') - 10));
    else
      Break;
    end;
    inc(p);
    dec(Count);
  end;
  if Count > 0 then
    raise EJsonParserException.CreateResFmt(@RsInvalidHexNumber, [p^], Reader.FLineNum, Reader.GetLineColumn, Reader.GetPosition);
end;

function UtcDateTimeToLocalDateTime(UtcDateTime: TDateTime): TDateTime;
{$IFDEF MSWINDOWS}
var
  UtcTime, LocalTime: TSystemTime;
begin
  DateTimeToSystemTime(UtcDateTime, UtcTime);
  if SystemTimeToTzSpecificLocalTime(nil, UtcTime, LocalTime) then
    Result := SystemTimeToDateTime(LocalTime)
  else
    Result := UtcDateTime;
end;
{$ELSE}
begin
  Result := TTimeZone.Local.ToLocalTime(UtcDateTime);
end;
{$ENDIF MSWINDOWS}

function DateTimeToISO8601(Value: TDateTime): string;
{$IFDEF MSWINDOWS}
var
  LocalTime, UtcTime: TSystemTime;
  Offset: TDateTime;
  Hour, Min, Sec, MSec: Word;
begin
  DateTimeToSystemTime(Value, LocalTime);
  Result := Format('%.4d-%.2d-%.2dT%.2d:%.2d:%.2d.%d',
    [LocalTime.wYear, LocalTime.wMonth, LocalTime.wDay,
     LocalTime.wHour, LocalTime.wMinute, LocalTime.wSecond, LocalTime.wMilliseconds]);
  if TzSpecificLocalTimeToSystemTime(nil, LocalTime, UtcTime) then
  begin
    Offset := Value - SystemTimeToDateTime(UtcTime);
    DecodeTime(Offset, Hour, Min, Sec, MSec);
    if Offset < 0 then
      Result := Format('%s-%.2d:%.2d', [Result, Hour, Min])
    else if Offset > 0 then
      Result := Format('%s+%.2d:%.2d', [Result, Hour, Min])
    else
      Result := Result + 'Z';
  end;
end;
{$ELSE}
var
  Offset: TDateTime;
  Year, Month, Day, Hour, Minute, Second, Milliseconds: Word;
begin
  DecodeDate(Value, Year, Month, Day);
  DecodeTime(Value, Hour, Minute, Second, Milliseconds);
  Result := Format('%.4d-%.2d-%.2dT%.2d:%.2d:%.2d.%d', [Year, Month, Day, Hour, Minute, Second, Milliseconds]);
  Offset := Value - TTimeZone.Local.ToUniversalTime(Value);
  DecodeTime(Offset, Hour, Minute, Second, Milliseconds);
  if Offset < 0 then
    Result := Format('%s-%.2d:%.2d', [Result, Hour, Minute])
  else if Offset > 0 then
    Result := Format('%s+%.2d:%.2d', [Result, Hour, Minute])
  else
    Result := Result + 'Z';
end;
{$ENDIF MSWINDOWS}

class function TJsonBaseObject.DateTimeToJSON(const Value: TDateTime; UseUtcTime: Boolean): string;
{$IFDEF MSWINDOWS}
var
  LocalTime, UtcTime: TSystemTime;
begin
  if UseUtcTime then
  begin
    DateTimeToSystemTime(Value, LocalTime);
    if not TzSpecificLocalTimeToSystemTime(nil, LocalTime, UtcTime) then
      UtcTime := LocalTime;
    Result := Format('%.4d-%.2d-%.2dT%.2d:%.2d:%.2d.%dZ',
      [UtcTime.wYear, UtcTime.wMonth, UtcTime.wDay,
       UtcTime.wHour, UtcTime.wMinute, UtcTime.wSecond, UtcTime.wMilliseconds]);
  end
  else
    Result := DateTimeToISO8601(Value);
end;
{$ELSE}
var
  UtcTime: TDateTime;
  Year, Month, Day, Hour, Minute, Second, Milliseconds: Word;
begin
  if UseUtcTime then
  begin
    UtcTime := TTimeZone.Local.ToUniversalTime(Value);
    DecodeDate(UtcTime, Year, Month, Day);
    DecodeTime(UtcTime, Hour, Minute, Second, Milliseconds);
    Result := Format('%.4d-%.2d-%.2dT%.2d:%.2d:%.2d.%dZ', [Year, Month, Day, Hour, Minute, Second, Milliseconds]);
  end
  else
    Result := DateTimeToISO8601(Value);
end;
{$ENDIF MSWINDOWS}

function ParseDateTimePart(p: PChar; var Value: Integer; MaxLen: Integer): PChar;
var
  v: Integer;
begin
  Result := p;
  v := 0;
  while (Result^ in ['0'..'9']) and (MaxLen > 0) do
  begin
    v := v * 10 + (Ord(Result^) - Ord('0'));
    inc(Result);
    dec(MaxLen);
  end;
  Value := v;
end;

function VarTypeToJsonDataType(AVarType: TVarType): TJsonDataType;
begin
  case AVarType of
    varNull:
      Result := jdtObject;
    varOleStr, varString, varUString:
      Result := jdtString;
    varSmallInt, varInteger, varShortInt, varByte, varWord, varLongWord:
      Result := jdtInt;
    varInt64:
      Result := jdtLong;
    varUInt64:
      Result := jdtULong;
    varSingle, varDouble, varCurrency:
      Result := jdtFloat;
    varDate:
      Result := jdtDateTime;
    varBoolean:
      Result := jdtBool;
  else
    ErrorUnsupportedVariantType(AVarType);
    Result := jdtNone;
  end;
end;

class function TJsonBaseObject.JSONToDateTime(const Value: string): TDateTime;
var
  p: PChar;
  MSecsSince1970: Int64;
  Year, Month, Day, Hour, Min, Sec, MSec: Integer;
  OffsetHour, OffsetMin: Integer;
  Sign: Double;
begin
  Result := 0;
  if Value = '' then
    Exit;

  p := PChar(Value);
  if (p^ = '/') and (StrLComp('Date(', p + 1, 5) = 0) then  // .NET: milliseconds since 1970-01-01
  begin
    inc(p, 6);
    MSecsSince1970 := 0;
    while (p^ <> #0) and (p^ in ['0'..'9']) do
    begin
      MSecsSince1970 := MSecsSince1970 * 10 + (Ord(p^) - Ord('0'));
      inc(p);
    end;
    if (p^ = '+') or (p^ = '-') then // timezone information
    begin
      inc(p);
      while (p^ <> #0) and (p^ in ['0'..'9']) do
        inc(p);
    end;
    if (p[0] = ')') and (p[1] = '/') and (p[2] = #0) then
      Result := UtcDateTimeToLocalDateTime(UnixDateDelta + (MSecsSince1970 / MSecsPerDay))
    else
      Result := 0; // invalid format
  end
  else
  begin
    // "2015-02-01T16:08:19.202Z"
    if p^ = '-' then // negative year
      inc(p);
    p := ParseDateTimePart(p, Year, 4);
    if p^ <> '-' then
      Exit; // invalid format
    p := ParseDateTimePart(p + 1, Month, 2);
    if p^ <> '-' then
      Exit; // invalid format
    p := ParseDateTimePart(p + 1, Day, 2);

    Hour := 0;
    Min := 0;
    Sec := 0;
    MSec := 0;
    Result := EncodeDate(Year, Month, Day);

    if p^ = 'T' then
    begin
      p := ParseDateTimePart(p + 1, Hour, 2);
      if p^ <> ':' then
        Exit; // invalid format
      p := ParseDateTimePart(p + 1, Min, 2);
      if p^ = ':' then
      begin
        p := ParseDateTimePart(p + 1, Sec, 2);
        if p^ = '.' then
          p := ParseDateTimePart(p + 1, MSec, 3);
      end;
      Result := Result + EncodeTime(Hour, Min, Sec, MSec);
      if p^ <> 'Z' then
      begin
        if (p^ = '+') or (p^ = '-') then
        begin
          if p^ = '+' then
            Sign := -1 //  +0100 means that the time is 1 hour later than UTC
          else
            Sign := 1;

          p := ParseDateTimePart(p + 1, OffsetHour, 2);
          if p^ = ':' then
            inc(p);
          ParseDateTimePart(p, OffsetMin, 2);

          Result := Result + (EncodeTime(OffsetHour, OffsetMin, 0, 0) * Sign);
        end
        else
        begin
          Result := 0; // invalid format
          Exit;
        end;
      end;
      Result := UtcDateTimeToLocalDateTime(Result);
    end;
  end;
end;

{$IFDEF NEXTGEN}
function Utf8StrLen(p: PByte): Integer;
begin
  Result := 0;
  if p <> nil then
    while p[Result] <> 0 do
      inc(Result);
end;
{$ENDIF NEXTGEN}

procedure SetStringUtf8(var s: string; p: PByte; Len: Integer);
var
  L: Integer;
begin
  if s <> '' then
    s := '';
  if (p = nil) or (Len = 0) then
    Exit;
  SetLength(s, Len);

  L := Utf8ToUnicode(PWideChar(Pointer(s)), Len + 1, PAnsiChar(p), Len);
  if L > 0 then
  begin
    if L - 1 <> Len then
      SetLength(s, L - 1);
  end
  else
    s := '';
end;

procedure AppendString(var s: string; p: PChar; Len: Integer);
var
  OldLen: Integer;
begin
  if (p = nil) or (Len = 0) then
    Exit;
  OldLen := length(s);
  SetLength(s, OldLen + Len);
  Move(p^, PChar(Pointer(s))[OldLen], Len * SizeOf(Char));
end;

procedure AppendStringUtf8(var s: string; p: PByte; Len: Integer);
var
  L, OldLen: Integer;
begin
  if (p = nil) or (Len = 0) then
    Exit;
  OldLen := length(s);
  SetLength(s, OldLen + Len);

  L := Utf8ToUnicode(PWideChar(Pointer(s)) + OldLen, Len + 1, PAnsiChar(p), Len);
  if L > 0 then
  begin
    if L - 1 <> Len then
      SetLength(s, OldLen + L - 1);
  end
  else
    SetLength(s, OldLen);
end;

{$IFDEF SUPPORT_PROGRESS}
{ TJsonReaderProgressRec }

function TJsonReaderProgressRec.Init(AProgress: TJsonReaderProgressProc; aData: Pointer = nil; AThreshold: NativeInt = 0): PJsonReaderProgressRec;
begin
  Self.Data := aData;
  Self.Threshold := AThreshold;
  Self.Progress := AProgress;
  Result := @Self;
end;
{$ENDIF SUPPORT_PROGRESS}

{ TJsonReader }

{$IFDEF USE_FAST_NEWINSTANCE}
class function TJsonReader.NewInstance: TObject;
begin
  GetMem(Pointer(Result), InstanceSize);
  PPointer(Result)^ := Self;
  {$IFDEF AUTOREFCOUNT}
  TJsonReader(Result).FRefCount := 1;
  {$ENDIF AUTOREFCOUNT}
end;

procedure TJsonReader.FreeInstance;
begin
  // We have no WeakRef => faster cleanup
  FreeMem(Pointer(Self));
end;
{$ENDIF ~USE_FAST_NEWINSTANCE}

constructor TJsonReader.Create(AStart: Pointer{$IFDEF SUPPORT_PROGRESS}; ASize: NativeInt; AProgress: PJsonReaderProgressRec{$ENDIF});
begin
  //inherited Create;
  {$IFDEF USE_FAST_NEWINSTANCE}
  Pointer(FPropName) := nil;
  Pointer(FLook.s) := nil;
  {$ENDIF USE_FAST_NEWINSTANCE}
  {$IFDEF USE_STRINGINTERN_FOR_NAMES}
  FIdents.Init;
  {$ENDIF USE_STRINGINTERN_FOR_NAMES}

  FStart := AStart;
  FLineNum := 1; // base 1
  FLineStart := nil;

  {$IFDEF SUPPORT_PROGRESS}
  FSize := ASize;
  FProgress := AProgress;
  FLastProgressValue := 0; // class is not zero-filled
  if (FProgress <> nil) and Assigned(FProgress.Progress) then
    FProgress.Progress(FProgress.Data, 0, 0, FSize);
  {$ENDIF SUPPORT_PROGRESS}
end;

destructor TJsonReader.Destroy;
begin
  {$IFDEF USE_FAST_NEWINSTANCE}
  FPropName := '';
  FLook.s := '';
  {$ENDIF USE_FAST_NEWINSTANCE}
  {$IFDEF USE_STRINGINTERN_FOR_NAMES}
  FIdents.Done;
  {$ENDIF USE_STRINGINTERN_FOR_NAMES}

  {$IFDEF SUPPORT_PROGRESS}
  if (FLook.Kind = jtkEof) and (FProgress <> nil) and Assigned(FProgress.Progress) then
    FProgress.Progress(FProgress.Data, 100, FSize, FSize);
  {$ENDIF SUPPORT_PROGRESS}
  //inherited Destroy;
end;

{$IFDEF SUPPORT_PROGRESS}
procedure TJsonReader.CheckProgress(Position: Pointer);
var
  NewPercentage: NativeInt;
  ps: NativeInt;
begin
  if {(FProgress <> nil) and} Assigned(FProgress.Progress) then
  begin
    ps := PByte(Position) - PByte(FStart);
    if FProgress.Threshold = 0 then
    begin
      NewPercentage := ps * 100 div FSize;
      if NewPercentage <> FLastProgressValue then
      begin
        FLastProgressValue := NewPercentage;
        FProgress.Progress(FProgress.Data, NewPercentage, ps, FSize);
      end;
    end
    else if FProgress.Threshold > 0 then
    begin
      if ps - FLastProgressValue >= FProgress.Threshold then
      begin
        FLastProgressValue := ps;
        NewPercentage := 0;
        if FSize > 0 then
          NewPercentage := ps * 100 div FSize;
        FProgress.Progress(FProgress.Data, NewPercentage, ps, FSize);
      end;
    end;
  end;
end;
{$ENDIF SUPPORT_PROGRESS}

function TJsonReader.GetLineColumn: NativeInt;
begin
  if FLineStart = nil then
    FLineStart := FStart;
  Result := GetCharOffset(FLineStart) + 1; // base 1
end;

function TJsonReader.GetPosition: NativeInt;
begin
  Result := GetCharOffset(FStart);
end;

class procedure TJsonReader.InvalidStringCharacterError(const Reader: TJsonReader);
begin
  raise EJsonParserException.CreateRes(@RsInvalidStringCharacter,
    Reader.FLineNum, Reader.GetLineColumn, Reader.GetPosition);
end;

class procedure TJsonReader.StringNotClosedError(const Reader: TJsonReader);
begin
  raise EJsonParserException.CreateRes(@RsStringNotClosed,
    Reader.FLineNum, Reader.GetLineColumn, Reader.GetPosition);
end;

class procedure TJsonReader.JSONStrToStr(p, EndP: PChar; FirstEscapeIndex: Integer; var s: string;
  const Reader: TJsonReader);
const
  MaxBufPos = 127;
var
  Buf: array[0..MaxBufPos] of Char;
  f: PChar;
  BufPos, Len: Integer;
begin
  dec(FirstEscapeIndex);

  if FirstEscapeIndex > 0 then
  begin
    SetString(s, p, FirstEscapeIndex);
    inc(p, FirstEscapeIndex);
  end
  else
    s := '';

  while True do
  begin
    BufPos := 0;
    while (p < EndP) and (p^ = '\') do
    begin
      inc(p);
      if p = EndP then // broken escaped character
        Break;
      case p^ of
        '"': Buf[BufPos] := '"';
        '\': Buf[BufPos] := '\';
        '/': Buf[BufPos] := '/';
        'b': Buf[BufPos] := #8;
        'f': Buf[BufPos] := #12;
        'n': Buf[BufPos] := #10;
        'r': Buf[BufPos] := #13;
        't': Buf[BufPos] := #9;
        'u':
          begin
            inc(p);
            if p + 3 >= EndP then
              Break;
            Buf[BufPos] := Char(GetHexDigits(p, 4, TJsonReader(Reader)));
            inc(p, 3);
          end;
      else
        Break;
      end;
      inc(p);

      inc(BufPos);
      if BufPos > MaxBufPos then
      begin
        Len := length(s);
        SetLength(s, Len + BufPos);
        Move(Buf[0], PChar(Pointer(s))[Len], BufPos * SizeOf(Char));
        BufPos := 0;
      end;
    end;
    // append remaining buffer
    if BufPos > 0 then
    begin
      Len := length(s);
      SetLength(s, Len + BufPos);
      Move(Buf[0], PChar(Pointer(s))[Len], BufPos * SizeOf(Char));
    end;

    // fast forward
    f := p;
    while (p < EndP) and (p^ <> '\') do
      inc(p);
    if p > f then
      AppendString(s, f, p - f);
    if p >= EndP then
      Break;
  end;
end;

class procedure TJsonReader.JSONUtf8StrToStr(p, EndP: PByte; FirstEscapeIndex: Integer; var s: string;
  const Reader: TJsonReader);
const
  MaxBufPos = 127;
var
  Buf: array[0..MaxBufPos] of Char;
  f: PByte;
  BufPos, Len: Integer;
begin
  dec(FirstEscapeIndex);

  if FirstEscapeIndex > 0 then
  begin
    SetStringUtf8(s, p, FirstEscapeIndex);
    inc(p, FirstEscapeIndex);
  end
  else
    s := '';

  while True do
  begin
    BufPos := 0;
    while (p < EndP) and (p^ = Byte(Ord('\'))) do
    begin
      inc(p);
      if p = EndP then // broken escaped character
        Break;
      case p^ of
        Ord('"'): Buf[BufPos] := '"';
        Ord('\'): Buf[BufPos] := '\';
        Ord('/'): Buf[BufPos] := '/';
        Ord('b'): Buf[BufPos] := #8;
        Ord('f'): Buf[BufPos] := #12;
        Ord('n'): Buf[BufPos] := #10;
        Ord('r'): Buf[BufPos] := #13;
        Ord('t'): Buf[BufPos] := #9;
        Ord('u'):
          begin
            inc(p);
            if p + 3 >= EndP then
              Break;
            Buf[BufPos] := Char(GetHexDigitsUtf8(p, 4, TJsonReader(Reader)));
            inc(p, 3);
          end;
      else
        Break;
      end;
      inc(p);

      inc(BufPos);
      if BufPos > MaxBufPos then
      begin
        Len := length(s);
        SetLength(s, Len + BufPos);
        Move(Buf[0], PChar(Pointer(s))[Len], BufPos * SizeOf(Char));
        BufPos := 0;
      end;
    end;
    // append remaining buffer
    if BufPos > 0 then
    begin
      Len := length(s);
      SetLength(s, Len + BufPos);
      Move(Buf[0], PChar(Pointer(s))[Len], BufPos * SizeOf(Char));
    end;

    // fast forward
    f := p;
    while (p < EndP) and (p^ <> Byte(Ord('\'))) do
      inc(p);
    if p > f then
      AppendStringUtf8(s, f, p - f);
    if p >= EndP then
      Break;
  end;
end;

procedure TJsonReader.Parse(Data: TJsonBaseObject);
begin
  if Data is TJsonObject then
  begin
    TJsonObject(Data).Clear;
    Next; // initialize Lexer
    Accept(jtkLBrace);
    ParseObjectBody(TJsonObject(Data));
    Accept(jtkRBrace);
  end
  else if Data is TJsonArray then
  begin
    TJsonArray(Data).Clear;
    Next; // initialize Lexer
    Accept(jtkLBracket);
    ParseArrayBody(TJsonArray(Data));
    Accept(jtkRBracket)
  end;
end;

procedure TJsonReader.ParseObjectBody(const Data: TJsonObject);
// ObjectBody ::= [ ObjectProperty [ "," ObjectProperty ]* ]
begin
  if FLook.Kind <> jtkRBrace then
  begin
    while FLook.Kind <> jtkEof do
    begin
      ParseObjectProperty(Data);
      if FLook.Kind = jtkRBrace then
        Break;
      Accept(jtkComma);
    end;
  end;
end;

procedure TJsonReader.ParseObjectProperty(const Data: TJsonObject);
// Property ::= IDENT ":" ObjectPropertyValue
begin
  if FLook.Kind >= jtkIdent then // correct JSON would be "tkString" only
  begin
    {$IFDEF USE_STRINGINTERN_FOR_NAMES}
    FIdents.Intern(FLook.s, FPropName);
    {$ELSE}
    FPropName := '';
    // transfer the string without going through UStrAsg and UStrClr
    Pointer(FPropName) := Pointer(FLook.s);
    Pointer(FLook.s) := nil;
    {$ENDIF USE_STRINGINTERN_FOR_NAMES}
    Next;
  end
  else
    Accept(jtkString);

  Accept(jtkColon);
  ParseObjectPropertyValue(Data);
end;

procedure TJsonReader.ParseObjectPropertyValue(const Data: TJsonObject);
// ObjectPropertyValue ::= Object | Array | Value
begin
  case FLook.Kind of
    jtkLBrace:
      begin
        Accept(jtkLBrace);
        ParseObjectBody(Data.InternAddObject(FPropName));
        Accept(jtkRBrace);
      end;

    jtkLBracket:
      begin
        Accept(jtkLBracket);
        ParseArrayBody(Data.InternAddArray(FPropName));
        Accept(jtkRBracket);
      end;

    jtkNull:
      begin
        Data.InternAdd(FPropName, TJsonObject(nil));
        Next;
      end;

    jtkIdent,
    jtkString:
      begin
        Data.InternAddItem(FPropName).InternSetValueTransfer(FLook.s);
        Next;
      end;

    jtkInt:
      begin
        Data.InternAdd(FPropName, FLook.i);
        Next;
      end;

    jtkLong:
      begin
        Data.InternAdd(FPropName, FLook.L);
        Next;
      end;

    jtkULong:
      begin
        Data.InternAdd(FPropName, FLook.u);
        Next;
      end;

    jtkFloat:
      begin
        Data.InternAdd(FPropName, FLook.f);
        Next;
      end;

    jtkTrue:
      begin
        Data.InternAdd(FPropName, True);
        Next;
      end;

    jtkFalse:
      begin
        Data.InternAdd(FPropName, False);
        Next;
      end
  else
    Accept(jtkValue);
  end;
end;

procedure TJsonReader.ParseArrayBody(const Data: TJsonArray);
// ArrayBody ::= [ ArrayPropertyValue [ "," ArrayPropertyValue ]* ]
begin
  if FLook.Kind <> jtkRBracket then
  begin
    while FLook.Kind <> jtkEof do
    begin
      ParseArrayPropertyValue(Data);
      if FLook.Kind = jtkRBracket then
        Break;
      Accept(jtkComma);
    end;
  end;
end;

procedure TJsonReader.ParseArrayPropertyValue(const Data: TJsonArray);
// ArrayPropertyValue ::= Object | Array | Value
begin
  case FLook.Kind of
    jtkLBrace:
      begin
        Accept(jtkLBrace);
        ParseObjectBody(Data.AddObject);
        Accept(jtkRBrace);
      end;

    jtkLBracket:
      begin
        Accept(jtkLBracket);
        ParseArrayBody(Data.AddArray);
        Accept(jtkRBracket);
      end;

    jtkNull:
      begin
        Data.Add(TJsonObject(nil));
        Next;
      end;

    jtkIdent,
    jtkString:
      begin
        Data.Add(FLook.s);
        Next;
      end;

    jtkInt:
      begin
        Data.Add(FLook.i);
        Next;
      end;

    jtkLong:
      begin
        Data.Add(FLook.L);
        Next;
      end;

    jtkULong:
      begin
        Data.Add(FLook.u);
        Next;
      end;

    jtkFloat:
      begin
        Data.AddF(FLook.f);
        Next;
      end;

    jtkTrue:
      begin
        Data.Add(True);
        Next;
      end;

    jtkFalse:
      begin
        Data.Add(False);
        Next;
      end;
  else
    Accept(jtkValue);
  end;
end;

procedure TJsonReader.AcceptFailed(TokenKind: TJsonTokenKind);
var
  Col, Position: NativeInt;
begin
  Col := GetLineColumn;
  Position := GetPosition;
  if FLook.Kind = jtkEof then
    raise EJsonParserException.CreateResFmt(@RsUnexpectedEndOfFile, [JsonTokenKindToStr[TokenKind]], FLineNum, Col, Position);
  raise EJsonParserException.CreateResFmt(@RsUnexpectedToken, [JsonTokenKindToStr[TokenKind], JsonTokenKindToStr[FLook.Kind]], FLineNum, Col, Position);
end;

procedure TJsonReader.Accept(TokenKind: TJsonTokenKind);
begin
  if FLook.Kind <> TokenKind then
    AcceptFailed(TokenKind);
  Next;
end;

{ TJsonDataValue }

procedure TJsonDataValue.Clear;
{$IFDEF USE_FAST_AUTOREFCOUNT}
var
  p: Pointer;
{$ENDIF USE_FAST_AUTOREFCOUNT}
begin
  // All types must clear their value because if a value changes the type we need a zero-ed value
  case FTyp of
    jdtString:
      string(FValue.s) := '';
    jdtInt:
      FValue.i := 0;
    jdtLong:
      FValue.L := 0;
    jdtULong:
      FValue.u := 0;
    jdtFloat:
      FValue.f := 0;
    jdtDateTime:
      FValue.d := 0;
    jdtBool:
      FValue.b := False;
    jdtArray,
    jdtObject:
      begin
        {$IFDEF USE_FAST_AUTOREFCOUNT}
        p := FValue.O;
        if p <> nil then
        begin
          FValue.O := nil;
          TJsonBaseObject(p).ARCObjRelease;
        end;
        {$ELSE}
          {$IFNDEF AUTOREFCOUNT}
        TJsonBaseObject(FValue.O).Free;
          {$ENDIF ~AUTOREFCOUNT}
        TJsonBaseObject(FValue.O) := nil;
        {$ENDIF USE_FAST_AUTOREFCOUNT}
      end;
  end;
  FTyp := jdtNone;
end;

function TJsonDataValue.IsNull: Boolean;
begin
  case FTyp of
    jdtObject:
      Result := FValue.O = nil;
    jdtNone:
      Result := True;
  else
    Result := False;
  end;
end;

function TJsonDataValue.GetArrayValue: TJsonArray;
begin
  if FTyp = jdtArray then
    Result := TJsonArray(FValue.a)
  else if FTyp = jdtNone then
    Result := nil
  else
  begin
    TypeCastError(jdtArray);
    Result := nil;
  end;
end;

procedure TJsonDataValue.SetArrayValue(const AValue: TJsonArray);
var
  LTyp: TJsonDataType;
begin
  LTyp := FTyp;
  if (LTyp <> jdtArray) or (AValue <> FValue.a) then
  begin
    if LTyp <> jdtNone then
      Clear;
    FTyp := jdtArray;
    {$IFDEF USE_FAST_AUTOREFCOUNT}
    // Assert(FValue.A = nil);
    if AValue <> nil then
      AValue.ARCObjAddRef;
    FValue.a := Pointer(AValue);
    {$ELSE}
    TJsonArray(FValue.a) := AValue;
    {$ENDIF USE_FAST_AUTOREFCOUNT}
  end;
end;

function TJsonDataValue.GetObjectValue: TJsonObject;
begin
  if FTyp = jdtObject then
    Result := TJsonObject(FValue.O)
  else if FTyp = jdtNone then
    Result := nil
  else
  begin
    TypeCastError(jdtObject);
    Result := nil;
  end;
end;

procedure TJsonDataValue.SetObjectValue(const AValue: TJsonObject);
var
  LTyp: TJsonDataType;
begin
  LTyp := FTyp;
  if (LTyp <> jdtObject) or (AValue <> FValue.O) then
  begin
    if LTyp <> jdtNone then
      Clear;
    FTyp := jdtObject;
    {$IFDEF USE_FAST_AUTOREFCOUNT}
    // Assert(FValue.O = nil);
    if AValue <> nil then
      AValue.ARCObjAddRef;
    FValue.O := Pointer(AValue);
    {$ELSE}
    TJsonObject(FValue.O) := AValue;
    {$ENDIF USE_FAST_AUTOREFCOUNT}
  end;
end;

function TJsonDataValue.GetVariantValue: Variant;
begin
  case FTyp of
    jdtNone:
      Result := Unassigned;
    jdtString:
      Result := string(FValue.s);
    jdtInt:
      Result := FValue.i;
    jdtLong:
      Result := FValue.L;
    jdtULong:
      Result := FValue.u;
    jdtFloat:
      Result := FValue.f;
    jdtDateTime:
      Result := FValue.d;
    jdtBool:
      Result := FValue.b;
    jdtArray:
      ErrorUnsupportedVariantType(varArray);
    jdtObject:
      if FValue.O = nil then
        Result := Null // special handling for "null"
      else
        ErrorUnsupportedVariantType(varObject);
  else
    ErrorUnsupportedVariantType(varAny);
  end;
end;

procedure TJsonDataValue.SetVariantValue(const AValue: Variant);
var
  LTyp: TJsonDataType;
begin
  if FTyp <> jdtNone then
    Clear;
  LTyp := VarTypeToJsonDataType(VarType(AValue));
  if LTyp <> jdtNone then
  begin
    FTyp := LTyp;
    case LTyp of
      jdtString:
        string(FValue.s) := AValue;
      jdtInt:
        FValue.i := AValue;
      jdtLong:
        FValue.L := AValue;
      jdtULong:
        FValue.u := AValue;
      jdtFloat:
        FValue.f := AValue;
      jdtDateTime:
        FValue.d := AValue;
      jdtBool:
        FValue.b := AValue;
//    else
//      ErrorUnsupportedVariantType; handled by VarTypeToJsonDataType
    end;
  end;
end;

procedure TJsonDataValue.InternSetArrayValue(const AValue: TJsonArray);
begin
  FTyp := jdtArray;
  {$IFDEF USE_FAST_AUTOREFCOUNT}
  // Assert(FValue.A = nil);
  if AValue <> nil then
    inc(AValue.FRefCount); // AValue.ARCObjAddRef;   no other thread knows about this object right now
  FValue.a := Pointer(AValue);
  {$ELSE}
  TJsonArray(FValue.a) := AValue;
  {$ENDIF USE_FAST_AUTOREFCOUNT}
end;

procedure TJsonDataValue.InternSetObjectValue(const AValue: TJsonObject);
begin
  FTyp := jdtObject;
  {$IFDEF USE_FAST_AUTOREFCOUNT}
  // Assert(FValue.O = nil);
  if AValue <> nil then
    inc(AValue.FRefCount); // AValue.ARCObjAddRef;   no other thread knows about this object right now
  FValue.O := Pointer(AValue);
  {$ELSE}
  TJsonObject(FValue.O) := AValue;
  {$ENDIF USE_FAST_AUTOREFCOUNT}
end;

function TJsonDataValue.GetValue: string;
begin
  case FTyp of
    jdtNone:
      Result := '';
    jdtString:
      Result := string(FValue.s);
    jdtInt:
      Result := IntToStr(FValue.i);
    jdtLong:
      Result := IntToStr(FValue.L);
    jdtULong:
      Result := UIntToStr(FValue.u);
    jdtFloat:
      Result := FloatToStr(FValue.f, JSONFormatSettings);
    jdtDateTime:
      Result := TJsonBaseObject.DateTimeToJSON(FValue.f, JsonSerializationConfig.UseUtcTime);
    jdtBool:
      if FValue.b then
        Result := sTrue
      else
        Result := sFalse;
    jdtObject:
      begin
        if not JsonSerializationConfig.NullConvertsToValueTypes or (FValue.O <> nil) then
          TypeCastError(jdtString);
        Result := '';
      end;
  else
    TypeCastError(jdtString);
    Result := '';
  end;
end;

procedure TJsonDataValue.SetValue(const AValue: string);
var
  LTyp: TJsonDataType;
begin
  LTyp := FTyp;
  if (LTyp <> jdtString) or (AValue <> string(FValue.s)) then
  begin
    if LTyp <> jdtNone then
      Clear;
    FTyp := jdtString;
    string(FValue.s) := AValue;
  end;
end;

procedure TJsonDataValue.InternSetValue(const AValue: string);
begin
  FTyp := jdtString;
  string(FValue.s) := AValue;
end;

procedure TJsonDataValue.InternSetValueTransfer(var AValue: string);
begin
  FTyp := jdtString;
  // transfer the string without going through UStrAsg and UStrClr
  FValue.s := Pointer(AValue);
  Pointer(AValue) := nil;
end;

function TJsonDataValue.GetIntValue: Integer;
begin
  case FTyp of
    jdtNone:
      Result := 0;
    jdtString:
      if not TryStrToInt(string(FValue.s), Result) then
        Result := Trunc(StrToFloat(string(FValue.s), JSONFormatSettings));
    jdtInt:
      Result := FValue.i;
    jdtLong:
      Result := FValue.L;
    jdtULong:
      Result := FValue.u;
    jdtFloat:
      Result := Trunc(FValue.f);
    jdtDateTime:
      Result := Trunc(FValue.d);
    jdtBool:
      Result := Ord(FValue.b);
    jdtObject:
      begin
        if not JsonSerializationConfig.NullConvertsToValueTypes or (FValue.O <> nil) then
          TypeCastError(jdtInt);
        Result := 0;
      end;
  else
    TypeCastError(jdtInt);
    Result := 0;
  end;
end;

procedure TJsonDataValue.SetIntValue(const AValue: Integer);
var
  LTyp: TJsonDataType;
begin
  LTyp := FTyp;
  if (LTyp <> jdtInt) or (AValue <> FValue.i) then
  begin
    if LTyp <> jdtNone then
      Clear;
    FTyp := jdtInt;
    FValue.i := AValue;
  end;
end;

function TJsonDataValue.GetLongValue: Int64;
begin
  case FTyp of
    jdtNone:
      Result := 0;
    jdtString:
      if not TryStrToInt64(string(FValue.s), Result) then
        Result := Trunc(StrToFloat(string(FValue.s), JSONFormatSettings));
    jdtInt:
      Result := FValue.i;
    jdtLong:
      Result := FValue.L;
    jdtULong:
      Result := FValue.u;
    jdtFloat:
      Result := Trunc(FValue.f);
    jdtDateTime:
      Result := Trunc(FValue.d);
    jdtBool:
      Result := Ord(FValue.b);
    jdtObject:
      begin
        if not JsonSerializationConfig.NullConvertsToValueTypes or (FValue.O <> nil) then
          TypeCastError(jdtLong);
        Result := 0;
      end;
  else
    TypeCastError(jdtLong);
    Result := 0;
  end;
end;

procedure TJsonDataValue.SetLongValue(const AValue: Int64);
var
  LTyp: TJsonDataType;
begin
  LTyp := FTyp;
  if (LTyp <> jdtLong) or (AValue <> FValue.L) then
  begin
    if LTyp <> jdtNone then
      Clear;
    FTyp := jdtLong;
    FValue.L := AValue;
  end;
end;

function TJsonDataValue.GetULongValue: UInt64;
begin
  case FTyp of
    jdtNone:
      Result := 0;
    jdtString:
      if not TryStrToUInt64(string(FValue.s), Result) then
        Result := Trunc(StrToFloat(string(FValue.s), JSONFormatSettings));
    jdtInt:
      Result := FValue.i;
    jdtLong:
      Result := FValue.L;
    jdtULong:
      Result := FValue.u;
    jdtFloat:
      Result := Trunc(FValue.f);
    jdtDateTime:
      Result := Trunc(FValue.d);
    jdtBool:
      Result := Ord(FValue.b);
    jdtObject:
      begin
        if not JsonSerializationConfig.NullConvertsToValueTypes or (FValue.O <> nil) then
          TypeCastError(jdtULong);
        Result := 0;
      end;
  else
    TypeCastError(jdtULong);
    Result := 0;
  end;
end;

procedure TJsonDataValue.SetULongValue(const AValue: UInt64);
var
  LTyp: TJsonDataType;
begin
  LTyp := FTyp;
  if (LTyp <> jdtULong) or (AValue <> FValue.u) then
  begin
    if LTyp <> jdtNone then
      Clear;
    FTyp := jdtULong;
    FValue.u := AValue;
  end;
end;

function TJsonDataValue.GetFloatValue: Double;
begin
  case FTyp of
    jdtNone:
      Result := 0;
    jdtString:
      Result := StrToFloat(string(FValue.s), JSONFormatSettings);
    jdtInt:
      Result := FValue.i;
    jdtLong:
      Result := FValue.L;
    jdtULong:
      Result := FValue.u;
    jdtFloat:
      Result := FValue.f;
    jdtDateTime:
      Result := FValue.d;
    jdtBool:
      Result := Ord(FValue.b);
    jdtObject:
      begin
        if not JsonSerializationConfig.NullConvertsToValueTypes or (FValue.O <> nil) then
          TypeCastError(jdtFloat);
        Result := 0;
      end;
  else
    TypeCastError(jdtFloat);
    Result := 0;
  end;
end;

procedure TJsonDataValue.SetFloatValue(const AValue: Double);
var
  LTyp: TJsonDataType;
begin
  LTyp := FTyp;
  if (LTyp <> jdtFloat) or (AValue <> FValue.f) then
  begin
    if LTyp <> jdtNone then
      Clear;
    FTyp := jdtFloat;
    FValue.f := AValue;
  end;
end;

function TJsonDataValue.GetDateTimeValue: TDateTime;
begin
  case FTyp of
    jdtNone:
      Result := 0;
    jdtString:
      Result := TJsonBaseObject.JSONToDateTime(string(FValue.s));
    jdtInt:
      Result := FValue.i;
    jdtLong:
      Result := FValue.L;
    jdtULong:
      Result := FValue.u;
    jdtFloat:
      Result := FValue.f;
    jdtDateTime:
      Result := FValue.d;
    jdtBool:
      Result := Ord(FValue.b);
    jdtObject:
      begin
        if not JsonSerializationConfig.NullConvertsToValueTypes or (FValue.O <> nil) then
          TypeCastError(jdtDateTime);
        Result := 0;
      end;
  else
    TypeCastError(jdtDateTime);
    Result := 0;
  end;
end;

procedure TJsonDataValue.SetDateTimeValue(const AValue: TDateTime);
var
  LTyp: TJsonDataType;
begin
  LTyp := FTyp;
  if (LTyp <> jdtDateTime) or (AValue <> FValue.d) then
  begin
    if LTyp <> jdtNone then
      Clear;
    FTyp := jdtDateTime;
    FValue.d := AValue;
  end;
end;

function TJsonDataValue.GetBoolValue: Boolean;
begin
  case FTyp of
    jdtNone:
      Result := False;
    jdtString:
      Result := string(FValue.s) = 'true';
    jdtInt:
      Result := FValue.i <> 0;
    jdtLong:
      Result := FValue.L <> 0;
    jdtULong:
      Result := FValue.u <> 0;
    jdtFloat:
      Result := FValue.f <> 0;
    jdtDateTime:
      Result := FValue.d <> 0;
    jdtBool:
      Result := FValue.b;
    jdtObject:
      begin
        if not JsonSerializationConfig.NullConvertsToValueTypes or (FValue.O <> nil) then
          TypeCastError(jdtBool);
        Result := False;
      end;
  else
    TypeCastError(jdtBool);
    Result := False;
  end;
end;

procedure TJsonDataValue.SetBoolValue(const AValue: Boolean);
var
  LTyp: TJsonDataType;
begin
  LTyp := FTyp;
  if (LTyp <> jdtBool) or (AValue <> FValue.b) then
  begin
    if LTyp <> jdtNone then
      Clear;
    FTyp := jdtBool;
    FValue.b := AValue;
  end;
end;

function DoubleToText(buffer: PChar; const Value: Extended): Integer; inline;
begin
  Result := FloatToText(buffer, Value, fvExtended, ffGeneral, 15, 0, JSONFormatSettings);
end;

const
  DoubleDigits: array[0..99] of array[0..1] of Char = (
    '00', '01', '02', '03', '04', '05', '06', '07', '08', '09',
    '10', '11', '12', '13', '14', '15', '16', '17', '18', '19',
    '20', '21', '22', '23', '24', '25', '26', '27', '28', '29',
    '30', '31', '32', '33', '34', '35', '36', '37', '38', '39',
    '40', '41', '42', '43', '44', '45', '46', '47', '48', '49',
    '50', '51', '52', '53', '54', '55', '56', '57', '58', '59',
    '60', '61', '62', '63', '64', '65', '66', '67', '68', '69',
    '70', '71', '72', '73', '74', '75', '76', '77', '78', '79',
    '80', '81', '82', '83', '84', '85', '86', '87', '88', '89',
    '90', '91', '92', '93', '94', '95', '96', '97', '98', '99'
  );

function InternIntToText(Value: Cardinal; Negative: Boolean; EndP: PChar): PChar;
var
  i, Quotient, k: Cardinal;
begin
  i := Value;
  Result := EndP;
  while i >= 100 do
  begin
    Quotient := i div 100;
    k := Quotient * 100;
    k := i - k;
    i := Quotient;
    dec(Result, 2);
    PLongWord(Result)^ := LongWord(DoubleDigits[k]);
  end;
  if i >= 10 then
  begin
    dec(Result, 2);
    PLongWord(Result)^ := LongWord(DoubleDigits[i]);
  end
  else
  begin
    dec(Result);
    Result^ := Char(i or Ord('0'));
  end;

  if Negative then
  begin
    dec(Result);
    Result^ := '-';
  end;
end;

function IntToText(Value: Integer; EndP: PChar): PChar; inline;
begin
  if Value < 0 then
    Result := InternIntToText(Cardinal(-Value), True, EndP)
  else
    Result := InternIntToText(Cardinal(Value), False, EndP);
end;

function UInt64ToText(Value: UInt64; EndP: PChar): PChar;
var
  Quotient: UInt64;
  Remainder: Cardinal;
begin
  Result := EndP;

  while Value > high(Integer) do
  begin
    Quotient := Value div 100;
    //Remainder := Value - (Quotient * 100);
    Remainder := Value - (Quotient shl 6 + Quotient shl 5 + Quotient shl 2);
    Value := Quotient;

    dec(Result, 2);
    PLongWord(Result)^ := LongWord(DoubleDigits[Remainder]);
  end;

  Result := InternIntToText(Cardinal(Value), False, Result);
end;

function Int64ToText(Value: Int64; EndP: PChar): PChar;
var
  Neg: Boolean;
begin
  Neg := Value < 0;
  if Neg then
    Value := -Value;

  Result := UInt64ToText(UInt64(Value), EndP);

  if Neg then
  begin
    dec(Result);
    Result^ := '-';
  end;
end;

procedure TJsonDataValue.InternToJSON(var Writer: TJsonOutputWriter);
var
  buffer: array[0..63] of Char;
  p, BufEnd: PChar;
begin
  case FTyp of
    jdtNone:
      Writer.AppendValue(sNull);
    jdtString:
      TJsonBaseObject.StrToJSONStr(Writer.AppendStrValue, string(FValue.s));
    jdtInt:
      begin
        BufEnd := @PChar(@buffer[0])[length(buffer)]; // extra typecast to work around a compiler bug (fixed in XE3)
        p := IntToText(FValue.i, BufEnd);
        Writer.AppendValue(p, BufEnd - p);
      end;
    jdtLong:
      begin
        BufEnd := @PChar(@buffer[0])[length(buffer)]; // extra typecast to work around a compiler bug (fixed in XE3)
        p := Int64ToText(FValue.L, BufEnd);
        Writer.AppendValue(p, BufEnd - p);
      end;
    jdtULong:
      begin
        BufEnd := @PChar(@buffer[0])[length(buffer)]; // extra typecast to work around a compiler bug (fixed in XE3)
        p := UInt64ToText(FValue.u, BufEnd);
        Writer.AppendValue(p, BufEnd - p);
      end;
    jdtFloat:
      Writer.AppendValue(buffer, DoubleToText(buffer, FValue.f));
    jdtDateTime:
      TJsonBaseObject.DateTimeToJSONStr(Writer.AppendStrValue, FValue.d); // do the conversion in a function to prevent the compiler from creating a string intermediate in this method
    jdtBool:
      if FValue.b then
        Writer.AppendValue(sTrue)
      else
        Writer.AppendValue(sFalse);
    jdtArray:
      if (FValue.a = nil) or (TJsonArray(FValue.a).Count = 0) then
        Writer.AppendValue('[]')
      else
        TJsonArray(FValue.a).InternToJSON(Writer);
    jdtObject:
      if FValue.O = nil then
        Writer.AppendValue(sNull)
      else
        TJsonObject(FValue.O).InternToJSON(Writer);
  end;
end;

{ TJsonBaseObject }

{$IFDEF USE_FAST_NEWINSTANCE}
class function TJsonBaseObject.NewInstance: TObject;
begin
  Result := AllocMem(InstanceSize); // zeroes the new memory
  PPointer(Result)^ := Self; // VMT
  {$IFDEF AUTOREFCOUNT}
  TJsonBaseObject(Result).FRefCount := 1;
  {$ENDIF AUTOREFCOUNT}
end;
{$ENDIF ~USE_FAST_NEWINSTANCE}

{$IFDEF USE_FAST_AUTOREFCOUNT}
function TJsonBaseObject.ARCObjRelease: Integer;
begin
  // Use a static call instead of the virtual method call
  Result := inherited __ObjRelease;
end;

function TJsonBaseObject.ARCObjAddRef: Integer;
begin
  // Inline __ObjAddRef to skip the virtual method call
  Result := AtomicIncrement(FRefCount);
  //Result := inherited __ObjAddRef;
end;
{$ENDIF USE_FAST_AUTOREFCOUNT}

class procedure TJsonBaseObject.StrToJSONStr(const AppendMethod: TWriterAppendMethod; const s: string);
var
  p, EndP, f: PChar;
begin
  p := PChar(Pointer(s));
  if p <> nil then
  begin
    //EndP := P + Length(S);  inlined Length introduces too much unnecessary code
    EndP := p + PInteger(@PByte(s)[-4])^;

    // find the first char that must be escaped
    f := p;
//    DCC64 generates "bt mem,reg" code
//    while (P < EndP) and not (P^ in [#0..#31, '\', '"' {$IFDEF ESCAPE_SLASH_AFTER_LESSTHAN}, '/'{$ENDIF}]) do
//      Inc(P);
    while p < EndP do
      case p^ of
        #0..#31, '\', '"' {$IFDEF ESCAPE_SLASH_AFTER_LESSTHAN}, '/'{$ENDIF}: Break;
      else
        inc(p);
      end;

    // nothing found, than it is easy
    if p = EndP then
      AppendMethod(PChar(s), length(s))
    else
      EscapeStrToJSONStr(f, p, EndP, AppendMethod);
  end
  else
    AppendMethod(nil, 0);
end;

class procedure TJsonBaseObject.DateTimeToJSONStr(const AppendMethod: TWriterAppendMethod; const Value: TDateTime);
var
  s: string;
begin
  s := TJsonBaseObject.DateTimeToJSON(Value, JsonSerializationConfig.UseUtcTime);
  // StrToJSONStr isn't necessary because the date-time string doesn't contain any char
  // that must be escaped.
  AppendMethod(PChar(s), length(s));
end;

class procedure TJsonBaseObject.EscapeStrToJSONStr(f, p, EndP: PChar; const AppendMethod: TWriterAppendMethod);
const
  HexChars: array[0..15] of Char = '0123456789abcdef';
var
  Buf: TJsonOutputWriter.TJsonStringBuilder;
  Ch: Char;
  {$IFDEF ESCAPE_SLASH_AFTER_LESSTHAN}
  StartP: PChar;
  {$ENDIF ESCAPE_SLASH_AFTER_LESSTHAN}
begin
  {$IFDEF ESCAPE_SLASH_AFTER_LESSTHAN}
  StartP := f;
  {$ENDIF ESCAPE_SLASH_AFTER_LESSTHAN}

  Buf.Init;
  try
    repeat
      if p <> f then
        Buf.Append(f, p - f); // append the string part that doesn't need an escape sequence
      if p < EndP then
      begin
        Ch := p^;
        case Ch of
          #0..#7, #11, #14..#31:
            begin
              Buf.Append('\u00', 4);
              Buf.Append2(HexChars[Word(Ch) shr 4], HexChars[Word(Ch) and $F]);
            end;
          #8: Buf.Append('\b', 2);
          #9: Buf.Append('\t', 2);
          #10: Buf.Append('\n', 2);
          #12: Buf.Append('\f', 2);
          #13: Buf.Append('\r', 2);
          '\': Buf.Append('\\', 2);
          '"': Buf.Append('\"', 2);
          {$IFDEF ESCAPE_SLASH_AFTER_LESSTHAN}
          '/':
            begin
              if (p > StartP) and (p[-1] = '<') then // escape '/' only if we have '</' to support HTML <script>-Tag
                Buf.Append('\/', 2)
              else
                Buf.Append('/', 1);
            end;
          {$ENDIF ESCAPE_SLASH_AFTER_LESSTHAN}
        end;
        inc(p);
        f := p;
//        DCC64 generates "bt mem,reg" code
//        while (P < EndP) and not (P^ in [#0..#31, '\', '"' {$IFDEF ESCAPE_SLASH_AFTER_LESSTHAN}, '/'{$ENDIF}]) do
//          Inc(P);
        while p < EndP do
          case p^ of
            #0..#31, '\', '"' {$IFDEF ESCAPE_SLASH_AFTER_LESSTHAN}, '/'{$ENDIF}: Break;
          else
            inc(p);
          end;
      end
      else
        Break;
    until False;
    AppendMethod(Buf.Data, Buf.Len);
  finally
    Buf.Done;
  end;
end;

class function TJsonBaseObject.ParseUtf8(s: PAnsiChar; Len: Integer{$IFDEF SUPPORT_PROGRESS}; AProgress: PJsonReaderProgressRec{$ENDIF}): TJsonBaseObject;
begin
  Result := ParseUtf8Bytes(PByte(s), Len{$IFDEF SUPPORT_PROGRESS}, AProgress{$ENDIF});
end;

{$IFDEF SUPPORTS_UTF8STRING}
class function TJsonBaseObject.ParseUtf8(const s: UTF8String{$IFDEF SUPPORT_PROGRESS}; AProgress: PJsonReaderProgressRec{$ENDIF}): TJsonBaseObject;
begin
  Result := ParseUtf8Bytes(PByte(s), length(s){$IFDEF SUPPORT_PROGRESS}, AProgress{$ENDIF});
end;
{$ENDIF SUPPORTS_UTF8STRING}

class function TJsonBaseObject.ParseUtf8Bytes(s: PByte; Len: Integer{$IFDEF SUPPORT_PROGRESS}; AProgress: PJsonReaderProgressRec{$ENDIF}): TJsonBaseObject;
var
  p: PByte;
  L: Integer;
begin
  if (s = nil) or (Len = 0) then
    Result := nil
  else
  begin
    if Len < 0 then
    begin
      {$IFDEF NEXTGEN}
      Len := Utf8StrLen(s);
      {$ELSE}
      Len := StrLen(PAnsiChar(s));
      {$ENDIF NEXTGEN}
    end;
    p := s;
    L := Len;
    while (L > 0) and (p^ <= 32) do
    begin
      inc(p);
      dec(L);
    end;
    if L = 0 then
      Result := nil
    else
    begin
      if (L > 0) and (p^ = Byte(Ord('['))) then
        Result := TJsonArray.Create
      else
        Result := TJsonObject.Create;

      {$IFDEF AUTOREFCOUNT}
      Result.FromUtf8JSON(s, Len{$IFDEF SUPPORT_PROGRESS}, AProgress{$ENDIF});
      {$ELSE}
      try
        Result.FromUtf8JSON(s, Len{$IFDEF SUPPORT_PROGRESS}, AProgress{$ENDIF});
      except
        Result.Free;
        raise;
      end;
      {$ENDIF AUTOREFCOUNT}
    end;
  end;
end;

class function TJsonBaseObject.Parse(const s: UnicodeString{$IFDEF SUPPORT_PROGRESS}; AProgress: PJsonReaderProgressRec{$ENDIF}): TJsonBaseObject;
begin
  Result := Parse(PWideChar(Pointer(s)), length(s){$IFDEF SUPPORT_PROGRESS}, AProgress{$ENDIF});
end;

class function TJsonBaseObject.Parse(s: PWideChar; Len: Integer{$IFDEF SUPPORT_PROGRESS}; AProgress: PJsonReaderProgressRec{$ENDIF}): TJsonBaseObject;
var
  p: PWideChar;
  L: Integer;
begin
  if (s = nil) or (Len = 0) then
    Result := nil
  else
  begin
    if Len < 0 then
      Len := StrLen(s);
    p := s;
    L := Len;
    while (L > 0) and (p^ <= #32) do
    begin
      inc(p);
      dec(L);
    end;
    if L = 0 then
      Result := nil
    else
    begin
      if (L > 0) and (p^ = '[') then
        Result := TJsonArray.Create
      else
        Result := TJsonObject.Create;

      {$IFDEF AUTOREFCOUNT}
      Result.FromJSON(s, Len{$IFDEF SUPPORT_PROGRESS}, AProgress{$ENDIF});
      {$ELSE}
      try
        Result.FromJSON(s, Len{$IFDEF SUPPORT_PROGRESS}, AProgress{$ENDIF});
      except
        Result.Free;
        raise;
      end;
      {$ENDIF AUTOREFCOUNT}
    end;
  end;
end;

class function TJsonBaseObject.Parse(const Bytes: TBytes; Encoding: TEncoding; ByteIndex: Integer;
  ByteCount: Integer{$IFDEF SUPPORT_PROGRESS}; AProgress: PJsonReaderProgressRec{$ENDIF}): TJsonBaseObject;
var
  L: Integer;
begin
  L := length(Bytes);
  if ByteCount = -1 then
    ByteCount := L - ByteIndex;
  if (ByteCount <= 0) or (ByteIndex + ByteCount > L) then
    Result := nil
  else
  begin
    if (Encoding = TEncoding.UTF8) or (Encoding = nil) then
      Result := ParseUtf8Bytes(PByte(@Bytes[ByteIndex]), ByteCount{$IFDEF SUPPORT_PROGRESS}, AProgress{$ENDIF})
    else if Encoding = TEncoding.Unicode then
      Result := Parse(PWideChar(@Bytes[ByteIndex]), ByteCount div SizeOf(WideChar){$IFDEF SUPPORT_PROGRESS}, AProgress{$ENDIF})
    else
      Result := Parse(Encoding.GetString(Bytes, ByteIndex, ByteCount){$IFDEF SUPPORT_PROGRESS}, AProgress{$ENDIF});
  end;
end;

class function TJsonBaseObject.ParseFromFile(const FileName: string; Utf8WithoutBOM: Boolean{$IFDEF SUPPORT_PROGRESS}; AProgress: PJsonReaderProgressRec{$ENDIF}): TJsonBaseObject;
var
  stream: TFileStream;
begin
  stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    Result := ParseFromStream(stream, nil, Utf8WithoutBOM{$IFDEF SUPPORT_PROGRESS}, AProgress{$ENDIF});
  finally
    stream.Free;
  end;
end;

class function TJsonBaseObject.ParseFromStream(stream: TStream; Encoding: TEncoding; Utf8WithoutBOM: Boolean{$IFDEF SUPPORT_PROGRESS}; AProgress: PJsonReaderProgressRec{$ENDIF}): TJsonBaseObject;
var
  StreamInfo: TStreamInfo;
  s: string;
  L: Integer;
begin
  GetStreamBytes(stream, Encoding, Utf8WithoutBOM, StreamInfo);
  try
    if Encoding = TEncoding.UTF8 then
      Result := ParseUtf8Bytes(StreamInfo.buffer, StreamInfo.Size{$IFDEF SUPPORT_PROGRESS}, AProgress{$ENDIF})
    else if Encoding = TEncoding.Unicode then
      Result := Parse(PWideChar(Pointer(StreamInfo.buffer)), StreamInfo.Size div SizeOf(WideChar){$IFDEF SUPPORT_PROGRESS}, AProgress{$ENDIF})
    else
    begin
      L := TEncodingStrictAccess(Encoding).GetCharCountEx(StreamInfo.buffer, StreamInfo.Size);
      SetLength(s, L);
      if L > 0 then
        TEncodingStrictAccess(Encoding).GetCharsEx(StreamInfo.buffer, StreamInfo.Size, PChar(Pointer(s)), L)
      else if StreamInfo.Size > 0 then
        ErrorNoMappingForUnicodeCharacter;

      // release memory
      FreeMem(StreamInfo.AllocationBase);
      StreamInfo.AllocationBase := nil;

      Result := Parse(s{$IFDEF SUPPORT_PROGRESS}, AProgress{$ENDIF});
    end;
  finally
    FreeMem(StreamInfo.AllocationBase);
  end;
end;

{$IFDEF SUPPORTS_UTF8STRING}
procedure TJsonBaseObject.FromUtf8JSON(const s: UTF8String{$IFDEF SUPPORT_PROGRESS}; AProgress: PJsonReaderProgressRec{$ENDIF});
begin
  FromUtf8JSON(PAnsiChar(Pointer(s)), length(s){$IFDEF SUPPORT_PROGRESS}, AProgress{$ENDIF});
end;
{$ENDIF SUPPORTS_UTF8STRING}

procedure TJsonBaseObject.FromUtf8JSON(s: PAnsiChar; Len: Integer{$IFDEF SUPPORT_PROGRESS}; AProgress: PJsonReaderProgressRec{$ENDIF});
begin
  FromUtf8JSON(PByte(s), Len{$IFDEF SUPPORT_PROGRESS}, AProgress{$ENDIF});
end;

procedure TJsonBaseObject.FromUtf8JSON(s: PByte; Len: Integer{$IFDEF SUPPORT_PROGRESS}; AProgress: PJsonReaderProgressRec{$ENDIF});
var
  Reader: TJsonReader;
begin
  if Len < 0 then
  begin
    {$IFDEF NEXTGEN}
    Len := Utf8StrLen(s);
    {$ELSE}
    Len := StrLen(PAnsiChar(s));
    {$ENDIF NEXTGEN}
  end;
  Reader := TUtf8JsonReader.Create(s, Len{$IFDEF SUPPORT_PROGRESS}, AProgress{$ENDIF});
  try
    Reader.Parse(Self);
  finally
    Reader.Free;
  end;
end;

procedure TJsonBaseObject.FromJSON(const s: UnicodeString{$IFDEF SUPPORT_PROGRESS}; AProgress: PJsonReaderProgressRec{$ENDIF});
begin
  FromJSON(PWideChar(s), length(s){$IFDEF SUPPORT_PROGRESS}, AProgress{$ENDIF});
end;

procedure TJsonBaseObject.FromJSON(s: PWideChar; Len: Integer{$IFDEF SUPPORT_PROGRESS}; AProgress: PJsonReaderProgressRec{$ENDIF});
var
  Reader: TJsonReader;
begin
  if Len < 0 then
    Len := StrLen(s);
  Reader := TStringJsonReader.Create(s, Len{$IFDEF SUPPORT_PROGRESS}, AProgress{$ENDIF});
  try
    Reader.Parse(Self);
  finally
    Reader.Free;
  end;
end;

procedure TJsonBaseObject.LoadFromFile(const FileName: string; Utf8WithoutBOM: Boolean{$IFDEF SUPPORT_PROGRESS}; AProgress: PJsonReaderProgressRec{$ENDIF});
var
  stream: TFileStream;
begin
  stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream(stream, nil, Utf8WithoutBOM{$IFDEF SUPPORT_PROGRESS}, AProgress{$ENDIF});
  finally
    stream.Free;
  end;
end;

class procedure TJsonBaseObject.GetStreamBytes(stream: TStream; var Encoding: TEncoding; Utf8WithoutBOM: Boolean;
  var StreamInfo: TStreamInfo);
{$IFDEF WORKAROUND_NETWORK_FILE_INSUFFICIENT_RESOURCES}
const
  MaxBufSize = 20 * 1024 * 1024;
var
  ReadCount, ReadBufSize: NativeInt;
{$ENDIF WORKAROUND_NETWORK_FILE_INSUFFICIENT_RESOURCES}
var
  Position: Int64;
  Size: NativeInt;
  Bytes: PByte;
  BufStart: Integer;
begin
  BufStart := 0;
  Position := stream.Position;
  Size := stream.Size - Position;

  StreamInfo.buffer := nil;
  StreamInfo.Size := 0;
  StreamInfo.AllocationBase := nil;
  try
    Bytes := nil;
    if Size > 0 then
    begin
      if stream is TCustomMemoryStream then
      begin
        Bytes := TCustomMemoryStream(stream).Memory;
        TCustomMemoryStream(stream).Position := Position + Size;
        inc(Bytes, Position);
      end
      else
      begin
        GetMem(StreamInfo.AllocationBase, Size);
        Bytes := StreamInfo.AllocationBase;
        {$IFDEF WORKAROUND_NETWORK_FILE_INSUFFICIENT_RESOURCES}
        if (stream is THandleStream) and (Size > MaxBufSize) then
        begin
          ReadCount := Size;
          // Read in 20 MB blocks to work around a network limitation in Windows 2003 or older (INSUFFICIENT RESOURCES)
          while ReadCount > 0 do
          begin
            ReadBufSize := ReadCount;
            if ReadBufSize > MaxBufSize then
              ReadBufSize := MaxBufSize;
            stream.ReadBuffer(Bytes[Size - ReadCount], ReadBufSize);
            dec(ReadCount, ReadBufSize);
          end;
        end
        else
        {$ENDIF WORKAROUND_NETWORK_FILE_INSUFFICIENT_RESOURCES}
          stream.ReadBuffer(StreamInfo.AllocationBase^, Size);
      end;
    end;

    if Encoding = nil then
    begin
      // Determine the encoding from the BOM
      if Utf8WithoutBOM then
        Encoding := TEncoding.UTF8
      else
        Encoding := TEncoding.default;

      if Size >= 2 then
      begin
        if (Bytes[0] = $EF) and (Bytes[1] = $BB) then
        begin
          if Bytes[2] = $BF then
          begin
            Encoding := TEncoding.UTF8;
            BufStart := 3;
          end;
        end
        else if (Bytes[0] = $FF) and (Bytes[1] = $FE) then
        begin
          if (Bytes[2] = 0) and (Bytes[3] = 0) then
          begin
            raise EJsonException.CreateRes(@RsUnsupportedFileEncoding);
            //Result := bomUtf32LE;
            //BufStart := 4;
          end
          else
          begin
            Encoding := TEncoding.Unicode;
            BufStart := 2;
          end;
        end
        else if (Bytes[0] = $FE) and (Bytes[1] = $FF) then
        begin
          Encoding := TEncoding.BigEndianUnicode;
          BufStart := 2;
        end
        else if (Bytes[0] = 0) and (Bytes[1] = 0) and (Size >= 4) then
        begin
          if (Bytes[2] = $FE) and (Bytes[3] = $FF) then
          begin
            raise EJsonException.CreateRes(@RsUnsupportedFileEncoding);
            //Result := bomUtf32BE;
            //BufStart := 4;
          end;
        end;
      end;
    end;
    inc(Bytes, BufStart);
    StreamInfo.buffer := Bytes;
    StreamInfo.Size := Size - BufStart;
  except
    FreeMem(StreamInfo.AllocationBase);
    raise;
  end;
end;

procedure TJsonBaseObject.LoadFromStream(stream: TStream; Encoding: TEncoding; Utf8WithoutBOM: Boolean{$IFDEF SUPPORT_PROGRESS}; AProgress: PJsonReaderProgressRec{$ENDIF});
var
  StreamInfo: TStreamInfo;
  s: string;
  L: Integer;
begin
  GetStreamBytes(stream, Encoding, Utf8WithoutBOM, StreamInfo);
  try
    if Encoding = TEncoding.UTF8 then
      FromUtf8JSON(StreamInfo.buffer, StreamInfo.Size{$IFDEF SUPPORT_PROGRESS}, AProgress{$ENDIF})
    else if Encoding = TEncoding.Unicode then
      FromJSON(PWideChar(Pointer(StreamInfo.buffer)), StreamInfo.Size div SizeOf(WideChar){$IFDEF SUPPORT_PROGRESS}, AProgress{$ENDIF})
    else
    begin
      L := TEncodingStrictAccess(Encoding).GetCharCountEx(StreamInfo.buffer, StreamInfo.Size);
      SetLength(s, L);
      if L > 0 then
        TEncodingStrictAccess(Encoding).GetCharsEx(StreamInfo.buffer, StreamInfo.Size, PChar(Pointer(s)), L)
      else if StreamInfo.Size > 0 then
        ErrorNoMappingForUnicodeCharacter;

      // release memory
      FreeMem(StreamInfo.AllocationBase);
      StreamInfo.AllocationBase := nil;

      FromJSON(s{$IFDEF SUPPORT_PROGRESS}, AProgress{$ENDIF});
    end;
  finally
    FreeMem(StreamInfo.AllocationBase);
  end;
end;

procedure TJsonBaseObject.SaveToFile(const FileName: string; Compact: Boolean; Encoding: TEncoding; Utf8WithoutBOM: Boolean);
var
  stream: TStream;
begin
  stream := TFileStream.Create(FileName, fmCreate or fmShareDenyWrite);
  try
    SaveToStream(stream, Compact, Encoding, Utf8WithoutBOM);
  finally
    stream.Free;
  end;
end;

procedure TJsonBaseObject.SaveToStream(stream: TStream; Compact: Boolean; Encoding: TEncoding; Utf8WithoutBOM: Boolean);
var
  Preamble: TBytes;
  Writer: TJsonOutputWriter;
begin
  if Utf8WithoutBOM and ((Encoding = TEncoding.UTF8) or (Encoding = nil)) then
    Encoding := TEncoding.UTF8
  else
  begin
    { if Encoding = nil then
      Encoding := TEncoding.Default;

      Preamble := Encoding.GetPreamble;
      if Preamble <> nil then
      Stream.Write(Preamble[0], Length(Preamble)); }
    if Encoding = nil then
    begin
      Encoding := TEncoding.UTF8; //thx qingfeng qq274838061

      Preamble := Encoding.GetPreamble;
      if Preamble <> nil then
        stream.write(Preamble[0], length(Preamble));
    end;
  end;

  Writer.Init(Compact, stream, Encoding, nil);
  try
    InternToJSON(Writer);
  finally
    Writer.StreamDone;
  end;
end;

procedure TJsonBaseObject.SaveToLines(Lines: TStrings);
var
  Writer: TJsonOutputWriter;
begin
  Writer.Init(False, nil, nil, Lines);
  try
    InternToJSON(Writer);
  finally
    Writer.LinesDone;
  end;
end;

function TJsonBaseObject.ToJSON(Compact: Boolean): string;
var
  Writer: TJsonOutputWriter;
begin
  Writer.Init(Compact, nil, nil, nil);
  try
    InternToJSON(Writer);
  finally
    Result := Writer.Done;
  end;
end;

{$IFDEF SUPPORTS_UTF8STRING}
function TJsonBaseObject.ToUtf8JSON(Compact: Boolean = True): UTF8String;
var
  stream: TJsonUTF8StringStream;
  Size: NativeInt;
begin
  stream := TJsonUTF8StringStream.Create;
  try
    SaveToStream(stream, Compact, nil, True);
    Result := stream.DataString;
    Size := stream.Size;
  finally
    stream.Free;
  end;
  if length(Result) <> Size then
    SetLength(Result, Size);
end;
{$ENDIF SUPPORTS_UTF8STRING}

procedure TJsonBaseObject.ToUtf8JSON(var Bytes: TBytes; Compact: Boolean = True);
var
  stream: TJsonBytesStream;
  Size: NativeInt;
begin
  stream := TJsonBytesStream.Create;
  try
    SaveToStream(stream, Compact, nil, True);
    Size := stream.Size;
    Bytes := stream.Bytes;
  finally
    stream.Free;
  end;
  if length(Bytes) <> Size then
    SetLength(Bytes, Size);
end;

function TJsonBaseObject.ToString: string;
begin
  Result := ToJSON;
end;

class procedure TJsonBaseObject.InternInitAndAssignItem(dest, Source: PJsonDataValue);
begin
  dest.FTyp := Source.FTyp;
  case Source.Typ of
    jdtString:
      begin
        dest.FValue.p := nil;
        string(dest.FValue.s) := string(Source.FValue.s);
      end;
    jdtInt:
      dest.FValue.i := Source.FValue.i;
    jdtLong:
      dest.FValue.L := Source.FValue.L;
    jdtULong:
      dest.FValue.u := Source.FValue.u;
    jdtFloat:
      dest.FValue.f := Source.FValue.f;
    jdtDateTime:
      dest.FValue.d := Source.FValue.d;
    jdtBool:
      dest.FValue.b := Source.FValue.b;
    jdtArray:
      begin
        {$IFDEF AUTOREFCOUNT}
        dest.FValue.a := nil;
        {$ENDIF AUTOREFCOUNT}
        if Source.FValue.a <> nil then
        begin
          {$IFDEF USE_FAST_AUTOREFCOUNT}
          dest.FValue.a := TJsonArray.Create;
          TJsonArray(dest.FValue.a).ARCObjAddRef;
          {$ELSE}
          TJsonArray(dest.FValue.a) := TJsonArray.Create;
          {$ENDIF USE_FAST_AUTOREFCOUNT}
          TJsonArray(dest.FValue.a).Assign(TJsonArray(Source.FValue.a));
        end
        {$IFNDEF AUTOREFCOUNT}
        else
          dest.FValue.a := nil;
        {$ENDIF ~AUTOREFCOUNT}
      end;
    jdtObject:
      begin
        {$IFDEF AUTOREFCOUNT}
        dest.FValue.O := nil;
        {$ENDIF AUTOREFCOUNT}
        if Source.FValue.O <> nil then
        begin
          {$IFDEF USE_FAST_AUTOREFCOUNT}
          dest.FValue.O := TJsonObject.Create;
          TJsonObject(dest.FValue.O).ARCObjAddRef;
          {$ELSE}
          TJsonObject(dest.FValue.O) := TJsonObject.Create;
          {$ENDIF USE_FAST_AUTOREFCOUNT}
          TJsonObject(dest.FValue.O).Assign(TJsonObject(Source.FValue.O));
        end
        {$IFNDEF AUTOREFCOUNT}
        else
          dest.FValue.O := nil;
        {$ENDIF ~AUTOREFCOUNT}
      end;
  end;
end;

procedure TJsonDataValue.TypeCastError(ExpectedType: TJsonDataType);
begin
  raise EJsonCastException.CreateResFmt(@RsTypeCastError,
    [TJsonBaseObject.DataTypeNames[FTyp], TJsonBaseObject.DataTypeNames[ExpectedType]])
    {$IFDEF HAS_RETURN_ADDRESS} at ReturnAddress{$ENDIF};
end;

{ TJsonArrayEnumerator }

constructor TJsonArrayEnumerator.Create(AArray: TJsonArray);
begin
  inherited Create;
  FIndex := -1;
  FArray := AArray;
end;

function TJsonArrayEnumerator.GetCurrent: TJsonDataValueHelper;
begin
  Result := FArray[FIndex];
end;

function TJsonArrayEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FArray.Count - 1;
  if Result then
    inc(FIndex);
end;

{ TJsonArray }

destructor TJsonArray.Destroy;
begin
  Clear;
  FreeMem(FItems);
  FItems := nil;
  //inherited Destroy;
end;

procedure TJsonArray.Clear;
var
  i: Integer;
begin
  for i := 0 to FCount - 1 do
    FItems[i].Clear;
  FCount := 0;
end;

procedure TJsonArray.Delete(index: Integer);
begin
  if (index < 0) or (index >= FCount) then
    ListError(@SListIndexError, index);
  FItems[index].Clear;
  dec(FCount);
  if index < FCount then
    Move(FItems[index + 1], FItems[index], (FCount - index) * SizeOf(TJsonDataValue));
end;

function TJsonArray.AddItem: PJsonDataValue;
begin
  if FCount = FCapacity then
    Grow;
  Result := @FItems[FCount];
  Result.FTyp := jdtNone;
  Result.FValue.p := nil;
  inc(FCount);
end;

function TJsonArray.InsertItem(index: Integer): PJsonDataValue;
begin
  if Cardinal(index) > Cardinal(FCount) then
    RaiseListError(index);

  if FCount = FCapacity then
    Grow;
  Result := @FItems[index];
  if index < FCount then
    Move(Result^, FItems[index + 1], (FCount - index) * SizeOf(TJsonDataValue));
  Result.FTyp := jdtNone;
  Result.FValue.p := nil;
  inc(FCount);
end;

procedure TJsonArray.Grow;
var
  c, Delta: Integer;
begin
  c := FCapacity;
  if c > 64 then
    Delta := c div 4
  else if c > 8 then
    Delta := 16
  else
    Delta := 4;
  FCapacity := c + Delta;
  InternApplyCapacity;
end;

procedure TJsonArray.InternApplyCapacity;
begin
  ReallocMem(Pointer(FItems), FCapacity * SizeOf(TJsonDataValue));
end;

procedure TJsonArray.SetCapacity(const Value: Integer);
var
  i: Integer;
begin
  if Value <> FCapacity then
  begin
    if FCapacity < FCount then
    begin
      // delete all overlapping items
      for i := FCapacity to FCount - 1 do
        FItems[i].Clear;
      FCount := FCapacity;
    end;
    FCapacity := Value;
    InternApplyCapacity;
  end;
end;

function TJsonArray.Extract(index: Integer): TJsonBaseObject;
begin
  if Items[index].FTyp in [jdtNone, jdtArray, jdtObject] then
  begin
    Result := TJsonBaseObject(FItems[index].FValue.O);
    TJsonBaseObject(FItems[index].FValue.O) := nil;
  end
  else
    Result := nil;
  Delete(index);
end;

function TJsonArray.ExtractArray(index: Integer): TJsonArray;
begin
  Result := Extract(index) as TJsonArray;
end;

function TJsonArray.ExtractObject(index: Integer): TJsonObject;
begin
  Result := Extract(index) as TJsonObject;
end;

function TJsonArray.GetArray(index: Integer): TJsonArray;
begin
  {$IFDEF CHECK_ARRAY_INDEX}
  if Cardinal(index) >= Cardinal(FCount) then
    RaiseListError(index);
  {$ENDIF CHECK_ARRAY_INDEX}
  Result := FItems[index].ArrayValue;
end;

function TJsonArray.GetBool(index: Integer): Boolean;
begin
  {$IFDEF CHECK_ARRAY_INDEX}
  if Cardinal(index) >= Cardinal(FCount) then
    RaiseListError(index);
  {$ENDIF CHECK_ARRAY_INDEX}
  Result := FItems[index].BoolValue;
end;

function TJsonArray.GetObject(index: Integer): TJsonObject;
begin
  {$IFDEF CHECK_ARRAY_INDEX}
  if Cardinal(index) >= Cardinal(FCount) then
    RaiseListError(index);
  {$ENDIF CHECK_ARRAY_INDEX}
  Result := FItems[index].ObjectValue;
end;

function TJsonArray.GetVariant(index: Integer): Variant;
begin
  {$IFDEF CHECK_ARRAY_INDEX}
  if Cardinal(index) >= Cardinal(FCount) then
    RaiseListError(index);
  {$ENDIF CHECK_ARRAY_INDEX}
  Result := FItems[index].VariantValue;
end;

function TJsonArray.GetInt(index: Integer): Integer;
begin
  {$IFDEF CHECK_ARRAY_INDEX}
  if Cardinal(index) >= Cardinal(FCount) then
    RaiseListError(index);
  {$ENDIF CHECK_ARRAY_INDEX}
  Result := FItems[index].IntValue;
end;

function TJsonArray.GetLong(index: Integer): Int64;
begin
  {$IFDEF CHECK_ARRAY_INDEX}
  if Cardinal(index) >= Cardinal(FCount) then
    RaiseListError(index);
  {$ENDIF CHECK_ARRAY_INDEX}
  Result := FItems[index].LongValue;
end;

function TJsonArray.GetULong(index: Integer): UInt64;
begin
  {$IFDEF CHECK_ARRAY_INDEX}
  if Cardinal(index) >= Cardinal(FCount) then
    RaiseListError(index);
  {$ENDIF CHECK_ARRAY_INDEX}
  Result := FItems[index].ULongValue;
end;

function TJsonArray.GetFloat(index: Integer): Double;
begin
  {$IFDEF CHECK_ARRAY_INDEX}
  if Cardinal(index) >= Cardinal(FCount) then
    RaiseListError(index);
  {$ENDIF CHECK_ARRAY_INDEX}
  Result := FItems[index].FloatValue;
end;

function TJsonArray.GetDateTime(index: Integer): TDateTime;
begin
  {$IFDEF CHECK_ARRAY_INDEX}
  if Cardinal(index) >= Cardinal(FCount) then
    RaiseListError(index);
  {$ENDIF CHECK_ARRAY_INDEX}
  Result := FItems[index].DateTimeValue;
end;

function TJsonArray.GetItem(index: Integer): PJsonDataValue;
begin
  {$IFDEF CHECK_ARRAY_INDEX}
  if Cardinal(index) >= Cardinal(FCount) then
    RaiseListError(index);
  {$ENDIF CHECK_ARRAY_INDEX}
  Result := @FItems[index];
end;

function TJsonArray.GetString(index: Integer): string;
begin
  {$IFDEF CHECK_ARRAY_INDEX}
  if Cardinal(index) >= Cardinal(FCount) then
    RaiseListError(index);
  {$ENDIF CHECK_ARRAY_INDEX}
  Result := FItems[index].Value;
end;

procedure TJsonArray.Add(const AValue: TJsonObject);
var
  Data: PJsonDataValue;
begin
  Data := AddItem;
  Data.ObjectValue := AValue;
end;

procedure TJsonArray.Add(const AValue: TJsonArray);
var
  Data: PJsonDataValue;
begin
  Data := AddItem;
  Data.ArrayValue := AValue;
end;

procedure TJsonArray.Add(const AValue: Boolean);
var
  Data: PJsonDataValue;
begin
  Data := AddItem;
  Data.BoolValue := AValue;
end;

procedure TJsonArray.Add(const AValue: Integer);
var
  Data: PJsonDataValue;
begin
  Data := AddItem;
  Data.IntValue := AValue;
end;

procedure TJsonArray.Add(const AValue: Int64);
var
  Data: PJsonDataValue;
begin
  Data := AddItem;
  Data.LongValue := AValue;
end;

procedure TJsonArray.Add(const AValue: UInt64);
var
  Data: PJsonDataValue;
begin
  Data := AddItem;
  Data.ULongValue := AValue;
end;

procedure TJsonArray.AddF(const AValue: Double);
var
  Data: PJsonDataValue;
begin
  Data := AddItem;
  Data.FloatValue := AValue;
end;

procedure TJsonArray.AddT(const AValue: TDateTime);
var
  Data: PJsonDataValue;
begin
  Data := AddItem;
  Data.DateTimeValue := AValue;
end;

procedure TJsonArray.Add(const AValue: string);
var
  Data: PJsonDataValue;
begin
  Data := AddItem;
  Data.Value := AValue;
end;

procedure TJsonArray.AddV(const AValue: Variant);
var
  Data: PJsonDataValue;
begin
  VarTypeToJsonDataType(VarType(AValue)); // Handle type-check exception before adding the item
  Data := AddItem;
  Data.VariantValue := AValue;
end;

function TJsonArray.AddArray: TJsonArray;
begin
  {$IFDEF USE_FAST_AUTOREFCOUNT}
  if Result <> nil then
    Result.ARCObjRelease;
  Pointer(Result) := TJsonArray.Create;
  Result.ARCObjAddRef;
  {$ELSE}
  Result := TJsonArray.Create;
  {$ENDIF USE_FAST_AUTOREFCOUNT}
  Add(Result);
end;

function TJsonArray.AddObject: TJsonObject;
begin
  {$IFDEF USE_FAST_AUTOREFCOUNT}
  if Result <> nil then
    Result.ARCObjRelease;
  Pointer(Result) := TJsonObject.Create;
  Result.ARCObjAddRef;
  {$ELSE}
  Result := TJsonObject.Create;
  {$ENDIF USE_FAST_AUTOREFCOUNT}
  Add(Result);
end;

procedure TJsonArray.AddObject(const Value: TJsonObject);
begin
  Add(Value);
end;

procedure TJsonArray.Insert(index: Integer; const AValue: TJsonObject);
var
  Data: PJsonDataValue;
begin
  Data := InsertItem(index);
  Data.ObjectValue := AValue;
end;

procedure TJsonArray.Insert(index: Integer; const AValue: TJsonArray);
var
  Data: PJsonDataValue;
begin
  Data := InsertItem(index);
  Data.ArrayValue := AValue;
end;

procedure TJsonArray.Insert(index: Integer; const AValue: Boolean);
var
  Data: PJsonDataValue;
begin
  Data := InsertItem(index);
  Data.BoolValue := AValue;
end;

procedure TJsonArray.Insert(index: Integer; const AValue: Integer);
var
  Data: PJsonDataValue;
begin
  Data := InsertItem(index);
  Data.IntValue := AValue;
end;

procedure TJsonArray.Insert(index: Integer; const AValue: Int64);
var
  Data: PJsonDataValue;
begin
  Data := InsertItem(index);
  Data.LongValue := AValue;
end;

procedure TJsonArray.Insert(index: Integer; const AValue: UInt64);
var
  Data: PJsonDataValue;
begin
  Data := InsertItem(index);
  Data.ULongValue := AValue;
end;

procedure TJsonArray.InsertF(index: Integer; const AValue: Double);
var
  Data: PJsonDataValue;
begin
  Data := InsertItem(index);
  Data.FloatValue := AValue;
end;

procedure TJsonArray.InsertT(index: Integer; const AValue: TDateTime);
var
  Data: PJsonDataValue;
begin
  Data := InsertItem(index);
  Data.DateTimeValue := AValue;
end;

procedure TJsonArray.Insert(index: Integer; const AValue: string);
var
  Data: PJsonDataValue;
begin
  Data := InsertItem(index);
  Data.Value := AValue;
end;

procedure TJsonArray.InsertV(index: Integer; const AValue: Variant);
var
  Data: PJsonDataValue;
begin
  VarTypeToJsonDataType(VarType(AValue)); // Handle type-check exception before inserting the item
  Data := InsertItem(index);
  Data.VariantValue := AValue;
end;

function TJsonArray.InsertArray(index: Integer): TJsonArray;
begin
  Result := TJsonArray.Create;
  {$IFDEF AUTOREFCOUNT}
  Insert(index, Result);
  {$ELSE}
  try
    Insert(index, Result);
  except
    Result.Free;
    raise;
  end;
  {$ENDIF AUTOREFCOUNT}
end;

function TJsonArray.InsertObject(index: Integer): TJsonObject;
begin
  Result := TJsonObject.Create;
  {$IFDEF AUTOREFCOUNT}
  Insert(index, Result);
  {$ELSE}
  try
    Insert(index, Result);
  except
    Result.Free;
    raise;
  end;
  {$ENDIF AUTOREFCOUNT}
end;

procedure TJsonArray.InsertObject(index: Integer; const Value: TJsonObject);
begin
  {$IFDEF CHECK_ARRAY_INDEX}
  if Cardinal(index) >= Cardinal(FCount) then
    RaiseListError(index);
  {$ENDIF CHECK_ARRAY_INDEX}
  Insert(index, Value);
end;

function TJsonArray.GetEnumerator: TJsonArrayEnumerator;
begin
  Result := TJsonArrayEnumerator.Create(Self);
end;

function TJsonArray.IsNull(index: Integer): Boolean;
begin
  {$IFDEF CHECK_ARRAY_INDEX}
  if Cardinal(index) >= Cardinal(FCount) then
    RaiseListError(index);
  {$ENDIF CHECK_ARRAY_INDEX}
  Result := FItems[index].IsNull
end;

procedure TJsonArray.SetString(index: Integer; const Value: string);
begin
  {$IFDEF CHECK_ARRAY_INDEX}
  if Cardinal(index) >= Cardinal(FCount) then
    RaiseListError(index);
  {$ENDIF CHECK_ARRAY_INDEX}
  FItems[index].Value := Value;
end;

procedure TJsonArray.SetInt(index: Integer; const Value: Integer);
begin
  {$IFDEF CHECK_ARRAY_INDEX}
  if Cardinal(index) >= Cardinal(FCount) then
    RaiseListError(index);
  {$ENDIF CHECK_ARRAY_INDEX}
  FItems[index].IntValue := Value;
end;

procedure TJsonArray.SetLong(index: Integer; const Value: Int64);
begin
  {$IFDEF CHECK_ARRAY_INDEX}
  if Cardinal(index) >= Cardinal(FCount) then
    RaiseListError(index);
  {$ENDIF CHECK_ARRAY_INDEX}
  FItems[index].LongValue := Value;
end;

procedure TJsonArray.SetULong(index: Integer; const Value: UInt64);
begin
  {$IFDEF CHECK_ARRAY_INDEX}
  if Cardinal(index) >= Cardinal(FCount) then
    RaiseListError(index);
  {$ENDIF CHECK_ARRAY_INDEX}
  FItems[index].ULongValue := Value;
end;

procedure TJsonArray.SetFloat(index: Integer; const Value: Double);
begin
  {$IFDEF CHECK_ARRAY_INDEX}
  if Cardinal(index) >= Cardinal(FCount) then
    RaiseListError(index);
  {$ENDIF CHECK_ARRAY_INDEX}
  FItems[index].FloatValue := Value;
end;

procedure TJsonArray.SetDateTime(index: Integer; const Value: TDateTime);
begin
  {$IFDEF CHECK_ARRAY_INDEX}
  if Cardinal(index) >= Cardinal(FCount) then
    RaiseListError(index);
  {$ENDIF CHECK_ARRAY_INDEX}
  FItems[index].DateTimeValue := Value;
end;

procedure TJsonArray.SetBool(index: Integer; const Value: Boolean);
begin
  {$IFDEF CHECK_ARRAY_INDEX}
  if Cardinal(index) >= Cardinal(FCount) then
    RaiseListError(index);
  {$ENDIF CHECK_ARRAY_INDEX}
  FItems[index].BoolValue := Value;
end;

procedure TJsonArray.SetArray(index: Integer; const Value: TJsonArray);
begin
  {$IFDEF CHECK_ARRAY_INDEX}
  if Cardinal(index) >= Cardinal(FCount) then
    RaiseListError(index);
  {$ENDIF CHECK_ARRAY_INDEX}
  FItems[index].ArrayValue := Value;
end;

procedure TJsonArray.SetObject(index: Integer; const Value: TJsonObject);
begin
  {$IFDEF CHECK_ARRAY_INDEX}
  if Cardinal(index) >= Cardinal(FCount) then
    RaiseListError(index);
  {$ENDIF CHECK_ARRAY_INDEX}
  FItems[index].ObjectValue := Value;
end;

procedure TJsonArray.SetVariant(index: Integer; const Value: Variant);
begin
  {$IFDEF CHECK_ARRAY_INDEX}
  if Cardinal(index) >= Cardinal(FCount) then
    RaiseListError(index);
  {$ENDIF CHECK_ARRAY_INDEX}
  FItems[index].VariantValue := Value;
end;

function TJsonArray.GetType(index: Integer): TJsonDataType;
begin
  {$IFDEF CHECK_ARRAY_INDEX}
  if Cardinal(index) >= Cardinal(FCount) then
    RaiseListError(index);
  {$ENDIF CHECK_ARRAY_INDEX}
  Result := FItems[index].Typ;
end;

function TJsonArray.GetValue(index: Integer): TJsonDataValueHelper;
begin
  {$IFDEF CHECK_ARRAY_INDEX}
  if Cardinal(index) >= Cardinal(FCount) then
    RaiseListError(index);
  {$ENDIF CHECK_ARRAY_INDEX}
  Result.FData.FIntern := @FItems[index];
  Result.FData.FTyp := jdtNone;
end;

procedure TJsonArray.SetValue(index: Integer; const Value: TJsonDataValueHelper);
begin
  {$IFDEF CHECK_ARRAY_INDEX}
  if Cardinal(index) >= Cardinal(FCount) then
    RaiseListError(index);
  {$ENDIF CHECK_ARRAY_INDEX}
  TJsonDataValueHelper.SetInternValue(@FItems[index], Value);
end;

procedure TJsonArray.InternToJSON(var Writer: TJsonOutputWriter);
var
  i: Integer;
begin
  if FCount = 0 then
    Writer.AppendValue('[]')
  else
  begin
    Writer.Indent('[');
    FItems[0].InternToJSON(Writer);
    for i := 1 to FCount - 1 do
    begin
      Writer.AppendSeparator(',');
      FItems[i].InternToJSON(Writer);
    end;
    Writer.Unindent(']');
  end;
end;

procedure TJsonArray.Assign(aSource: TJsonArray);
var
  i: Integer;
begin
  Clear;
  if aSource <> nil then
  begin
    if FCapacity < aSource.Count then
    begin
      FCapacity := aSource.Count;
      ReallocMem(FItems, aSource.Count * SizeOf(TJsonDataValue));
    end;
    FCount := aSource.Count;
    for i := 0 to aSource.Count - 1 do
      InternInitAndAssignItem(@FItems[i], @aSource.FItems[i]);
  end
  else
  begin
    FreeMem(FItems);
    FCapacity := 0;
  end;
end;

function TJsonArray.Clone: TJsonBaseObject;
begin
  Result := TJsonArray.Create;
  try
    TJsonArray(Result).Assign(Self);
  except
    Result.Free;
    raise;
  end;
end;

class procedure TJsonArray.RaiseListError(index: Integer);
begin
  ListError(@SListIndexError, index);
end;

procedure TJsonArray.SetCount(const Value: Integer);
var
  i: Integer;
begin
  if Value <> FCount then
  begin
    SetCapacity(Value);
    // Initialize new Items to "null"
    for i := FCount to Value - 1 do
    begin
      FItems[i].FTyp := jdtObject;
      FItems[i].FValue.p := nil;
    end;
    FCount := Value;
  end;
end;

{ TJsonObjectEnumerator }

constructor TJsonObjectEnumerator.Create(AObject: TJsonObject);
begin
  inherited Create;
  FIndex := -1;
  FObject := AObject;
end;

function TJsonObjectEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FObject.Count - 1;
  if Result then
    inc(FIndex);
end;

function TJsonObjectEnumerator.GetCurrent: TJsonNameValuePair;
begin
  Result.Name := FObject.Names[FIndex];
  Result.Value.FData.FIntern := FObject.Items[FIndex];
  Result.Value.FData.FTyp := jdtNone;
end;

{ TJsonObject }

destructor TJsonObject.Destroy;
begin
  Clear;
  FreeMem(FItems);
  FreeMem(FNames);
  //inherited Destroy;
end;

{$IFDEF USE_LAST_NAME_STRING_LITERAL_CACHE}
procedure TJsonObject.UpdateLastValueItem(const Name: string; Item: PJsonDataValue);
begin
  if (Pointer(Name) <> nil) and (PInteger(@PByte(Name)[-8])^ = -1) then // string literal
  begin
    FLastValueItem := Item;
    FLastValueItemNamePtr := Pointer(Name);
  end
  else
    FLastValueItem := nil;
end;
{$ENDIF USE_LAST_NAME_STRING_LITERAL_CACHE}

procedure TJsonObject.Grow;
var
  c, Delta: Integer;
begin
  c := FCapacity;
  if c > 64 then
    Delta := c div 4
  else if c > 8 then
    Delta := 16
  else
    Delta := 4;
  FCapacity := c + Delta;
  InternApplyCapacity;
end;

procedure TJsonObject.InternApplyCapacity;
begin
  {$IFDEF USE_LAST_NAME_STRING_LITERAL_CACHE}
  FLastValueItem := nil;
  {$ENDIF USE_LAST_NAME_STRING_LITERAL_CACHE}
  ReallocMem(Pointer(FItems), FCapacity * SizeOf(FItems[0]));
  ReallocMem(Pointer(FNames), FCapacity * SizeOf(FNames[0]));
end;

procedure TJsonObject.SetCapacity(const Value: Integer);
var
  i: Integer;
begin
  if Value <> FCapacity then
  begin
    if FCapacity < FCount then
    begin
      // delete all overlapping items
      for i := FCapacity to FCount - 1 do
      begin
        FNames[i] := '';
        FItems[i].Clear;
      end;
      FCount := FCapacity;
    end;
    FCapacity := Value;
    InternApplyCapacity;
  end;
end;

procedure TJsonObject.Clear;
var
  i: Integer;
begin
  {$IFDEF USE_LAST_NAME_STRING_LITERAL_CACHE}
  FLastValueItem := nil;
  {$ENDIF USE_LAST_NAME_STRING_LITERAL_CACHE}
  for i := 0 to FCount - 1 do
  begin
    FNames[i] := '';
    FItems[i].Clear;
  end;
  FCount := 0;
end;

procedure TJsonObject.Remove(const Name: string);
var
  idx: Integer;
begin
  idx := IndexOf(Name);
  if idx <> -1 then
    Delete(idx);
end;

function TJsonObject.Extract(const Name: string): TJsonBaseObject;
var
  index: Integer;
begin
  index := IndexOf(Name);
  if index <> -1 then
  begin
    if FItems[index].FTyp in [jdtNone, jdtArray, jdtObject] then
    begin
      Result := TJsonBaseObject(FItems[index].FValue.O);
      TJsonBaseObject(FItems[index].FValue.O) := nil;
    end
    else
      Result := nil;
    Delete(index);
  end
  else
    Result := nil;
end;

function TJsonObject.ExtractArray(const Name: string): TJsonArray;
begin
  Result := Extract(Name) as TJsonArray;
end;

function TJsonObject.ExtractObject(const Name: string): TJsonObject;
begin
  Result := Extract(Name) as TJsonObject;
end;

function TJsonObject.GetEnumerator: TJsonObjectEnumerator;
begin
  Result := TJsonObjectEnumerator.Create(Self);
end;

function TJsonObject.IsNull(const Name: string): Boolean;
var
  Item: PJsonDataValue;
begin
  if FindItem(Name, Item) then
    Result := Item.IsNull
  else
    Result := True;
end;

function TJsonObject.AddItem(const Name: string): PJsonDataValue;
var
  p: PString;
begin
  if FCount = FCapacity then
    Grow;
  Result := @FItems[FCount];
  p := @FNames[FCount];
  inc(FCount);
  Pointer(p^) := nil; // initialize the string
  {$IFDEF USE_NAME_STRING_LITERAL}
  AsgString(p^, Name);
  {$ELSE}
  p^ := Name;
  {$ENDIF USE_NAME_STRING_LITERAL}

  Result.FValue.p := nil;
  Result.FTyp := jdtNone;
end;

function TJsonObject.InternAddItem(var Name: string): PJsonDataValue;
var
  p: PString;
begin
  if FCount = FCapacity then
    Grow;
  Result := @FItems[FCount];
  p := @FNames[FCount];
  inc(FCount);
  // Transfer the string without going through UStrAsg and UStrClr
  Pointer(p^) := Pointer(Name);
  Pointer(Name) := nil;

  Result.FValue.p := nil;
  Result.FTyp := jdtNone;
end;

function TJsonObject.GetArray(const Name: string): TJsonArray;
var
  Item: PJsonDataValue;
begin
  if FindItem(Name, Item) then
    Result := Item.ArrayValue
  else
  begin
    Result := TJsonArray.Create;
    AddItem(Name).ArrayValue := Result;
    {$IFDEF USE_LAST_NAME_STRING_LITERAL_CACHE}
    UpdateLastValueItem(Name, Item);
    {$ENDIF USE_LAST_NAME_STRING_LITERAL_CACHE}
  end;
end;

function TJsonObject.GetBool(const Name: string): Boolean;
var
  Item: PJsonDataValue;
begin
  if FindItem(Name, Item) then
    Result := Item.BoolValue
  else
    Result := False;
end;

function TJsonObject.GetInt(const Name: string): Integer;
var
  Item: PJsonDataValue;
begin
  if FindItem(Name, Item) then
    Result := Item.IntValue
  else
    Result := 0;
end;

function TJsonObject.GetLong(const Name: string): Int64;
var
  Item: PJsonDataValue;
begin
  if FindItem(Name, Item) then
    Result := Item.LongValue
  else
    Result := 0;
end;

function TJsonObject.GetULong(const Name: string): UInt64;
var
  Item: PJsonDataValue;
begin
  if FindItem(Name, Item) then
    Result := Item.ULongValue
  else
    Result := 0;
end;

function TJsonObject.GetFloat(const Name: string): Double;
var
  Item: PJsonDataValue;
begin
  if FindItem(Name, Item) then
    Result := Item.FloatValue
  else
    Result := 0;
end;

function TJsonObject.GetDateTime(const Name: string): TDateTime;
var
  Item: PJsonDataValue;
begin
  if FindItem(Name, Item) then
    Result := Item.DateTimeValue
  else
    Result := 0;
end;

function TJsonObject.GetObject(const Name: string): TJsonObject;
var
  Item: PJsonDataValue;
begin
  if FindItem(Name, Item) then
    Result := Item.ObjectValue
  else
  begin
    Result := TJsonObject.Create;
    AddItem(Name).ObjectValue := Result;
    {$IFDEF USE_LAST_NAME_STRING_LITERAL_CACHE}
    UpdateLastValueItem(Name, Item);
    {$ENDIF USE_LAST_NAME_STRING_LITERAL_CACHE}
  end;
end;

function TJsonObject.GetString(const Name: string): string;
var
  Item: PJsonDataValue;
begin
  if FindItem(Name, Item) then
    Result := Item.Value
  else
    Result := '';
end;

procedure TJsonObject.SetArray(const Name: string; const Value: TJsonArray);
begin
  RequireItem(Name).ArrayValue := Value;
end;

procedure TJsonObject.SetBool(const Name: string; const Value: Boolean);
begin
  RequireItem(Name).BoolValue := Value;
end;

procedure TJsonObject.SetInt(const Name: string; const Value: Integer);
begin
  RequireItem(Name).IntValue := Value;
end;

procedure TJsonObject.SetLong(const Name: string; const Value: Int64);
begin
  RequireItem(Name).LongValue := Value;
end;

procedure TJsonObject.SetULong(const Name: string; const Value: UInt64);
begin
  RequireItem(Name).ULongValue := Value;
end;

procedure TJsonObject.SetFloat(const Name: string; const Value: Double);
begin
  RequireItem(Name).FloatValue := Value;
end;

procedure TJsonObject.SetDateTime(const Name: string; const Value: TDateTime);
begin
  RequireItem(Name).DateTimeValue := Value;
end;

procedure TJsonObject.SetObject(const Name: string; const Value: TJsonObject);
begin
  RequireItem(Name).ObjectValue := Value;
end;

procedure TJsonObject.SetString(const Name, Value: string);
begin
  RequireItem(Name).Value := Value;
end;

function TJsonObject.GetType(const Name: string): TJsonDataType;
var
  Item: PJsonDataValue;
begin
  if FindItem(Name, Item) then
    Result := Item.Typ
  else
    Result := jdtNone;
end;

function TJsonObject.Contains(const Name: string): Boolean;
{$IFDEF USE_LAST_NAME_STRING_LITERAL_CACHE}
var
  Item: PJsonDataValue;
{$ENDIF USE_LAST_NAME_STRING_LITERAL_CACHE}
begin
  {$IFDEF USE_LAST_NAME_STRING_LITERAL_CACHE}
  Result := FindItem(Name, Item);
  {$ELSE}
  Result := IndexOf(Name) <> -1;
  {$ENDIF USE_LAST_NAME_STRING_LITERAL_CACHE}
end;

function TJsonObject.IndexOfPChar(s: PChar; Len: Integer): Integer;
var
  p: PJsonStringArray;
begin
  p := FNames;
  if Len = 0 then
  begin
    for Result := 0 to FCount - 1 do
      if p[Result] = '' then
        Exit;
  end
  else
  begin
    for Result := 0 to FCount - 1 do
      if (length(p[Result]) = Len) and CompareMem(s, Pointer(p[Result]), Len * SizeOf(Char)) then
        Exit;
  end;
  Result := -1;
end;

function TJsonObject.IndexOf(const Name: string): Integer;
var
  p: PJsonStringArray;
begin
  p := FNames;
  for Result := 0 to FCount - 1 do
    if {(Pointer(Name) = Pointer(P[Result])) or} (Name = p[Result]) then
      Exit;
  Result := -1;
end;

function TJsonObject.FindItem(const Name: string; var Item: PJsonDataValue): Boolean;
var
  idx: Integer;
begin
  {$IFDEF USE_LAST_NAME_STRING_LITERAL_CACHE}
  { If "Name" is a string literal we can compare the pointer of the last stored value instead of
    searching the list. }
  if (FLastValueItem <> nil) and (Pointer(Name) = FLastValueItemNamePtr) then
  begin
    Item := FLastValueItem;
    Result := True;
  end
  else
  {$ENDIF USE_LAST_NAME_STRING_LITERAL_CACHE}
  begin
    idx := IndexOf(Name);
    Result := idx <> -1;
    if Result then
    begin
      Item := @FItems[idx];
      {$IFDEF USE_LAST_NAME_STRING_LITERAL_CACHE}
      UpdateLastValueItem(Name, Item);
      {$ENDIF USE_LAST_NAME_STRING_LITERAL_CACHE}
    end
    else
      Item := nil;
  end;
end;

function TJsonObject.RequireItem(const Name: string): PJsonDataValue;
begin
  if not FindItem(Name, Result) then
  begin
    Result := AddItem(Name);
    {$IFDEF USE_LAST_NAME_STRING_LITERAL_CACHE}
    UpdateLastValueItem(Name, Result);
    {$ENDIF USE_LAST_NAME_STRING_LITERAL_CACHE}
  end;
end;

procedure TJsonObject.InternToJSON(var Writer: TJsonOutputWriter);
var
  i: Integer;
begin
  if Count = 0 then
    Writer.AppendValue('{}')
  else
  begin
    Writer.Indent('{');
    TJsonBaseObject.StrToJSONStr(Writer.AppendIntro, FNames[0]);
    FItems[0].InternToJSON(Writer);
    for i := 1 to FCount - 1 do
    begin
      Writer.AppendSeparator(',');
      TJsonBaseObject.StrToJSONStr(Writer.AppendIntro, FNames[i]);
      FItems[i].InternToJSON(Writer);
    end;
    Writer.Unindent('}');
  end;
end;

function TJsonObject.GetName(index: Integer): string;
begin
  Result := FNames[index];
end;

function TJsonObject.GetItem(index: Integer): PJsonDataValue;
begin
  Result := @FItems[index];
end;

procedure TJsonObject.Delete(index: Integer);
begin
  if (index < 0) or (index >= FCount) then
    ListError(@SListIndexError, index);

  {$IFDEF USE_LAST_NAME_STRING_LITERAL_CACHE}
  if @FItems[index] = FLastValueItem then
  begin
    FLastValueItem := nil;
    //FLastValueItemNamePtr := nil;
  end;
  {$ENDIF USE_LAST_NAME_STRING_LITERAL_CACHE}
  FNames[index] := '';
  FItems[index].Clear;
  dec(FCount);
  if index < FCount then
  begin
    Move(FItems[index + 1], FItems[index], (FCount - index) * SizeOf(FItems[0]));
    Move(FNames[index + 1], FNames[index], (FCount - index) * SizeOf(FNames[0]));
  end;
end;

function TJsonObject.GetValue(const Name: string): TJsonDataValueHelper;
begin
  if not FindItem(Name, Result.FData.FIntern) then
  begin
    Result.FData.FIntern := nil;
    Result.FData.FNameResolver := Self;
    Result.FData.FName := Name;
  end;
  Result.FData.FTyp := jdtNone;
end;

procedure TJsonObject.SetValue(const Name: string; const Value: TJsonDataValueHelper);
var
  Item: PJsonDataValue;
begin
  Item := RequireItem(Name);
  TJsonDataValueHelper.SetInternValue(Item, Value);
end;

procedure TJsonObject.InternAdd(var AName: string; const AValue: TJsonArray);
var
  Data: PJsonDataValue;
begin
  Data := InternAddItem(AName);
  Data.InternSetArrayValue(AValue);
end;

procedure TJsonObject.InternAdd(var AName: string; const AValue: TJsonObject);
var
  Data: PJsonDataValue;
begin
  Data := InternAddItem(AName);
  Data.InternSetObjectValue(AValue);
end;

procedure TJsonObject.InternAdd(var AName: string; const AValue: Boolean);
var
  Data: PJsonDataValue;
begin
  Data := InternAddItem(AName);
  Data.BoolValue := AValue;
end;

procedure TJsonObject.InternAdd(var AName: string; const AValue: Integer);
var
  Data: PJsonDataValue;
begin
  Data := InternAddItem(AName);
  Data.IntValue := AValue;
end;

procedure TJsonObject.InternAdd(var AName: string; const AValue: Int64);
var
  Data: PJsonDataValue;
begin
  Data := InternAddItem(AName);
  Data.LongValue := AValue;
end;

procedure TJsonObject.InternAdd(var AName: string; const AValue: UInt64);
var
  Data: PJsonDataValue;
begin
  Data := InternAddItem(AName);
  Data.ULongValue := AValue;
end;

procedure TJsonObject.InternAdd(var AName: string; const AValue: Double);
var
  Data: PJsonDataValue;
begin
  Data := InternAddItem(AName);
  Data.FloatValue := AValue;
end;

procedure TJsonObject.InternAdd(var AName: string; const AValue: TDateTime);
var
  Data: PJsonDataValue;
begin
  Data := InternAddItem(AName);
  Data.DateTimeValue := AValue;
end;

procedure TJsonObject.InternAdd(var AName: string; const AValue: string);
var
  Data: PJsonDataValue;
begin
  Data := InternAddItem(AName);
  Data.InternSetValue(AValue);
end;

function TJsonObject.InternAddArray(var AName: string): TJsonArray;
begin
  {$IFDEF USE_FAST_AUTOREFCOUNT}
  if Result <> nil then
    Result.ARCObjRelease;
  Pointer(Result) := TJsonArray.Create;
  inc(Result.FRefCount); //Result.ARCObjAddRef;
  {$ELSE}
  Result := TJsonArray.Create;
  {$ENDIF USE_FAST_AUTOREFCOUNT}
  InternAdd(AName, Result);
end;

function TJsonObject.InternAddObject(var AName: string): TJsonObject;
begin
  {$IFDEF USE_FAST_AUTOREFCOUNT}
  if Result <> nil then
    Result.ARCObjRelease;
  Pointer(Result) := TJsonObject.Create;
  inc(Result.FRefCount); //Result.ARCObjAddRef;
  {$ELSE}
  Result := TJsonObject.Create;
  {$ENDIF USE_FAST_AUTOREFCOUNT}
  InternAdd(AName, Result);
end;

procedure TJsonObject.ToSimpleObject(AObject: TObject; ACaseSensitive: Boolean);
var
  index, Count: Integer;
  PropList: PPropList;
  PropType: PTypeInfo;
  PropName: string;
  Item: PJsonDataValue;
  v: Variant;
begin
  if AObject = nil then
    Exit;
  if AObject.ClassInfo = nil then
    raise EJsonException.CreateResFmt(@RsMissingClassInfo, [AObject.ClassName]);

  Count := GetPropList(AObject, PropList);
  if Count > 0 then
  begin
    try
      for index := 0 to Count - 1 do
      begin
        if (PropList[index].StoredProc = Pointer($1)) or IsStoredProp(AObject, PropList[index]) then
        begin
          PropName := UTF8ToString(PropList[index].Name);
          if not ACaseSensitive then
            Item := FindCaseInsensitiveItem(PropName)
          else if not FindItem(PropName, Item) then
            Item := nil;

          if Item <> nil then
          begin
            case PropList[index].PropType^.Kind of
              tkInteger, tkChar, tkWChar:
                SetOrdProp(AObject, PropList[index], Item.IntValue);

              tkEnumeration:
                SetOrdProp(AObject, PropList[index], Item.IntValue);

              tkFloat:
                begin
                  PropType := PropList[index].PropType^;
                  if (PropType = TypeInfo(TDateTime)) or (PropType = TypeInfo(TDate)) or (PropType = TypeInfo(TTime)) then
                    SetFloatProp(AObject, PropList[index], Item.DateTimeValue)
                  else
                    SetFloatProp(AObject, PropList[index], Item.FloatValue);
                end;

              tkInt64:
                SetInt64Prop(AObject, PropList[index], Item.LongValue);

              tkString, tkLString, tkWString, tkUString:
                SetStrProp(AObject, PropList[index], Item.Value);

              tkSet:
                SetSetProp(AObject, PropList[index], Item.Value);

              tkVariant:
                begin
                  case Types[PropName] of
                    jdtObject, jdtArray:
                      v := Null;
                    jdtInt:
                      v := Item.IntValue;
                    jdtLong:
                      v := Item.LongValue;
                    jdtULong:
                      v := Item.ULongValue;
                    jdtFloat:
                      v := Item.FloatValue;
                    jdtDateTime:
                      v := Item.DateTimeValue;
                    jdtBool:
                      v := Item.BoolValue;
                  else
                    v := Item.Value;
                  end;
                  SetVariantProp(AObject, PropList[index], v);
                end;
            end;
          end;
        end;
      end;
    finally
      FreeMem(PropList);
    end;
  end;
end;

procedure TJsonObject.FromSimpleObject(AObject: TObject; ALowerCamelCase: Boolean);
var
  index, Count: Integer;
  PropList: PPropList;
  PropType: PTypeInfo;
  PropName: string;
  v: Variant;
  d: Double;
  Ch: Char;
begin
  Clear;
  if AObject = nil then
    Exit;
  if AObject.ClassInfo = nil then
    raise EJsonException.CreateResFmt(@RsMissingClassInfo, [AObject.ClassName]);

  Count := GetPropList(AObject, PropList);
  if Count > 0 then
  begin
    try
      for index := 0 to Count - 1 do
      begin
        if (PropList[index].StoredProc = Pointer($1)) or IsStoredProp(AObject, PropList[index]) then
        begin
          PropName := UTF8ToString(PropList[index].Name);
          if ALowerCamelCase and (PropName <> '') then
          begin
            Ch := PChar(Pointer(PropName))^;
            if Ord(Ch) < 128 then
            begin
              case Ch of
                'A'..'Z':
                  PChar(Pointer(PropName))^ := Char(Ord(Ch) xor $20);
              end;
            end
            else // Delphi 2005+ compilers allow unicode identifiers, even if that is a very bad idea
              AnsiLowerCamelCaseString(PropName);
          end;

          case PropList[index].PropType^.Kind of
            tkInteger, tkChar, tkWChar:
              InternAdd(PropName, GetOrdProp(AObject, PropList[index]));

            tkEnumeration:
              begin
                PropType := PropList[index].PropType^;
                if (PropType = TypeInfo(Boolean)) or (PropType = TypeInfo(ByteBool)) or
                   (PropType = TypeInfo(WordBool)) or (PropType = TypeInfo(LongBool)) then
                  InternAdd(PropName, GetOrdProp(AObject, PropList[index]) <> 0)
                else
                  InternAdd(PropName, GetOrdProp(AObject, PropList[index]));
              end;

            tkFloat:
              begin
                PropType := PropList[index].PropType^;
                d := GetFloatProp(AObject, PropList[index]);
                if (PropType = TypeInfo(TDateTime)) or (PropType = TypeInfo(TDate)) or (PropType = TypeInfo(TTime)) then
                  InternAdd(PropName, TDateTime(d))
                else
                  InternAdd(PropName, d);
              end;

            tkInt64:
              InternAdd(PropName, GetInt64Prop(AObject, PropList[index]));

            tkString, tkLString, tkWString, tkUString:
              InternAdd(PropName, GetStrProp(AObject, PropList[index]));

            tkSet:
              InternAdd(PropName, GetSetProp(AObject, PropList[index]));

            tkVariant:
              begin
                v := GetVariantProp(AObject, PropList[index]);
                if VarIsNull(v) or VarIsEmpty(v) then
                  InternAdd(PropName, TJsonObject(nil))
                else
                begin
                  case VarType(v) and varTypeMask of
                    varSingle, varDouble, varCurrency:
                      InternAdd(PropName, Double(v));
                    varShortInt, varSmallInt, varInteger, varByte, varWord:
                      InternAdd(PropName, Integer(v));
                    varLongWord:
                      InternAdd(PropName, Int64(LongWord(v)));
                    {$IF CompilerVersion >= 23.0} // XE2+
                    varInt64:
                      InternAdd(PropName, Int64(v));
                    {$IFEND}
                    varBoolean:
                      InternAdd(PropName, Boolean(v));
                  else
                    InternAdd(PropName, VarToStr(v));
                  end;
                end;
              end;
          end;
        end;
      end;
    finally
      FreeMem(PropList);
    end;
  end;
end;

function TJsonObject.FindCaseInsensitiveItem(const ACaseInsensitiveName: string): PJsonDataValue;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    if AnsiSameText(FNames[i], ACaseInsensitiveName) then
    begin
      Result := @FItems[i];
      Exit;
    end;
  end;
  Result := nil;
end;

procedure TJsonObject.Assign(aSource: TJsonObject);
var
  i: Integer;
begin
  Clear;
  if aSource <> nil then
  begin
    FCapacity := aSource.Count;
    InternApplyCapacity;

    FCount := aSource.Count;
    for i := 0 to aSource.Count - 1 do
    begin
      Pointer(FNames[i]) := nil;
      {$IFDEF USE_NAME_STRING_LITERAL}
      AsgString(FNames[i], aSource.FNames[i]);
      {$ELSE}
      FNames[i] := aSource.FNames[i];
      {$ENDIF USE_NAME_STRING_LITERAL}
      InternInitAndAssignItem(@FItems[i], @aSource.FItems[i]);
    end;
  end
  else
  begin
    FreeMem(FItems);
    FreeMem(FNames);
    FCapacity := 0;
  end;
end;

function TJsonObject.Clone: TJsonBaseObject;
begin
  Result := TJsonObject.Create;
  try
    TJsonObject(Result).Assign(Self);
  except
    Result.Free;
    raise;
  end;
end;

procedure TJsonObject.PathError(p, EndP: PChar);
var
  s: string;
begin
  System.SetString(s, p, EndP - p);
  raise EJsonPathException.CreateResFmt(@RsInvalidJsonPath, [s]);
end;

procedure TJsonObject.PathNullError(p, EndP: PChar);
var
  s: string;
begin
  System.SetString(s, p, EndP - p);
  raise EJsonPathException.CreateResFmt(@RsJsonPathContainsNullValue, [s]);
end;

procedure TJsonObject.PathIndexError(p, EndP: PChar; Count: Integer);
var
  s: string;
begin
  System.SetString(s, p, EndP - p);
  raise EJsonPathException.CreateResFmt(@RsJsonPathIndexError, [Count, s]);
end;

function TJsonObject.GetPath(const NamePath: string): TJsonDataValueHelper;
var
  f, p, EndF, LastEndF: PChar;
  Ch: Char;
  idx: Integer;
  Obj: TJsonObject;
  Arr: TJsonArray;
  Item: PJsonDataValue;
  s: string;
begin
  p := PChar(NamePath);
  // empty string => Self
  if p^ = #0 then
  begin
    Result := Self;
    Exit;
  end;

  Result.FData.FIntern := nil;
  Result.FData.FTyp := jdtNone;

  Obj := Self;
  Item := nil;
  LastEndF := nil;
  while True do
  begin
    f := p;

    // fast forward
    Ch := p^;
//    DCC64 generates "bt mem,reg" code
//    while not (Ch in [#0, '[', '.']) do
//    begin
//      Inc(P);
//      Ch := P^;
//    end;
    while True do
      case Ch of
        #0, '[', '.': Break;
      else
        inc(p);
        Ch := p^;
      end;

    EndF := p;
    if f = EndF then
      PathError(PChar(Pointer(NamePath)), p + 1);

    inc(p);
    case Ch of
      #0:
        begin
          if Obj <> nil then
          begin
            idx := Obj.IndexOfPChar(f, EndF - f);
            if idx <> -1 then
              Result.FData.FIntern := @Obj.FItems[idx]
            else
            begin
              Result.FData.FNameResolver := Obj;
              System.SetString(Result.FData.FName, f, EndF - f);
            end;
          end
          else
            Result.FData.FIntern := Item;
          Break;
        end;

      '.': // object access
        begin
          if Obj = nil then
            PathNullError(PChar(Pointer(NamePath)), LastEndF);

          idx := Obj.IndexOfPChar(f, EndF - f);
          if idx <> -1 then
            Obj := Obj.FItems[idx].ObjectValue
          else
          begin
            // auto create object
            System.SetString(s, f, EndF - f);
            Obj := Obj.InternAddObject(s);
          end;
        end;

      '[': // array access
        begin
          if Obj = nil then
            PathNullError(PChar(Pointer(NamePath)), LastEndF);

          idx := Obj.IndexOfPChar(f, EndF - f);
          if idx <> -1 then
          begin
            Arr := Obj.FItems[idx].ArrayValue;
            if Arr = nil then
            begin
              // Shouldn't happen => auto create array
              Arr := TJsonArray.Create;
              Obj.FItems[idx].ArrayValue := Arr;
            end;
          end
          else
          begin
            // auto create array
            System.SetString(s, f, EndF - f);
            Arr := Obj.InternAddArray(s);
          end;
          Ch := p^;
          // parse array index
          idx := 0;
          while Ch in ['0'..'9'] do
          begin
            idx := idx * 10 + (Word(Ch) - Ord('0'));
            inc(p);
            Ch := p^;
          end;

          if p^ <> ']' then
            PathError(PChar(Pointer(NamePath)), p + 1);
          inc(p);

          if idx >= Arr.Count then
            PathIndexError(PChar(Pointer(NamePath)), p, Arr.Count); // P is already incremented
          Item := @Arr.FItems[idx];

          if p^ = '.' then
          begin
            inc(p);
            Obj := Item.ObjectValue;
            Item := nil;
          end
          else if p^ = #0 then
          begin
            // return array element
            Result.FData.FIntern := Item;
            Break;
          end;
        end;
    end;
    LastEndF := EndF;
  end;
end;

procedure TJsonObject.SetPath(const NamePath: string; const Value: TJsonDataValueHelper);
var
  PathValue: TJsonDataValueHelper;
begin
  PathValue := Path[NamePath];
  PathValue.ResolveName;
  TJsonDataValueHelper.SetInternValue(PathValue.FData.FIntern, Value);
end;

{ TStringIntern }

{$IFDEF USE_STRINGINTERN_FOR_NAMES}
procedure TStringIntern.Init;
begin
  FCount := 0;
  FCapacity := 17;
  GetMem(FStrings, FCapacity * SizeOf(FStrings[0]));
  GetMem(FBuckets, FCapacity * SizeOf(FBuckets[0]));
  FillChar(FBuckets[0], FCapacity * SizeOf(FBuckets[0]), $FF);
end;

procedure TStringIntern.Done;
var
  i: Integer;
begin
  for i := 0 to FCount - 1 do
    FStrings[i].Name := '';
  FreeMem(FStrings);
  FreeMem(FBuckets);
end;

procedure TStringIntern.Intern(var s: string; var PropName: string);
var
  index: Integer;
  hash: Integer;
  {$IFDEF USE_FAST_STRASG_FOR_INTERNAL_STRINGS}
  Source: Pointer;
  {$ENDIF USE_FAST_STRASG_FOR_INTERNAL_STRINGS}
begin
  if PropName <> '' then
    PropName := ''; // guarantee that Pointer(FPropName) = nil because InternTransfer steals FLook.S and overwrites FPropName
  if s <> '' then
  begin
    hash := GetHash(s);
    index := Find(hash, s);
    if index <> -1 then
    begin
      {$IFDEF USE_FAST_STRASG_FOR_INTERNAL_STRINGS}
      Source := Pointer(FStrings[index].Name);
      if Source <> nil then
      begin
        {$IFDEF DEBUG}
        //if PInteger(@PByte(Source)[-8])^ = -1 then
        //  InternAsgStringUsageError;
        {$ENDIF DEBUG}
        Pointer(PropName) := Source;
        // We are parsing JSON, no other thread knowns about the string => skip the CPU lock
        inc(PInteger(@PByte(Source)[-8])^);
      end;
      {$ELSE}
      PropName := FStrings[index].Name;
      {$ENDIF USE_FAST_STRASG_FOR_INTERNAL_STRINGS}
      s := '';
    end
    else
    begin
      // Transfer the string without going through UStrAsg and UStrClr
      Pointer(PropName) := Pointer(s);
      Pointer(s) := nil;
      InternAdd(hash, PropName);
    end;
  end;
end;

class function TStringIntern.GetHash(const Name: string): Integer;
var
  p: PChar;
  Ch: Word;
begin
  // Only used to reduce memory when parsing large JSON strings
  Result := 0;
  p := PChar(Pointer(Name));
  if p <> nil then
  begin
    Result := PInteger(@PByte(Name)[-4])^;
    while True do
    begin
      Ch := Word(p[0]);
      if Ch = 0 then
        Break;
      Result := Result + Ch;

      Ch := Word(p[1]);
      if Ch = 0 then
        Break;
      Result := Result + Ch;

      Ch := Word(p[2]);
      if Ch = 0 then
        Break;
      Result := Result + Ch;

      Ch := Word(p[3]);
      if Ch = 0 then
        Break;
      Result := Result + Ch;

      Result := (Result shl 6) or ((Result shr 26) and $3F);
      inc(p, 4);
    end;
  end;
end;

procedure TStringIntern.InternAdd(AHash: Integer; const s: string);
var
  index: Integer;
  Bucket: PInteger;
begin
  if FCount = FCapacity then
    Grow;
  index := FCount;
  inc(FCount);

  Bucket := @FBuckets[(AHash and $7FFFFFFF) mod FCapacity];
  with FStrings[index] do
  begin
    Next := Bucket^;
    hash := AHash;
    Pointer(Name) := Pointer(s);
    inc(PInteger(@PByte(Name)[-8])^);
  end;
  Bucket^ := index;
end;

procedure TStringIntern.Grow;
var
  i: Integer;
  index: Integer;
  Len: Integer;
begin
  Len := FCapacity;
  // Some prime numbers
  case Len of
      17: Len := 37;
      37: Len := 59;
      59: Len := 83;
      83: Len := 127;
     127: Len := 353;
     353: Len := 739;
     739: Len := 1597;
    1597: Len := 2221;
  else
    Len := Len * 2 + 1;
  end;
  FCapacity := Len;

  ReallocMem(FStrings, Len * SizeOf(FStrings[0]));
  ReallocMem(FBuckets, Len * SizeOf(FBuckets[0]));
  FillChar(FBuckets[0], Len * SizeOf(FBuckets[0]), $FF);

  // Rehash
  for i := 0 to FCount - 1 do
  begin
    index := (FStrings[i].hash and $7FFFFFFF) mod Len;
    FStrings[i].Next := FBuckets[index];
    FBuckets[index] := i;
  end;
end;

function TStringIntern.Find(hash: Integer; const s: string): Integer;
var
  Strs: PJsonStringEntryArray;
begin
  Result := -1;
  if FCount <> 0 then
  begin
    Result := FBuckets[(hash and $7FFFFFFF) mod FCapacity];
    if Result <> -1 then
    begin
      Strs := FStrings;
      while True do
      begin
        if (Strs[Result].hash = hash) and (Strs[Result].Name = s) then
          Break;
        Result := Strs[Result].Next;
        if Result = -1 then
          Break;
      end;
    end;
  end;
end;
{$ENDIF USE_STRINGINTERN_FOR_NAMES}

{ TJsonOutputWriter }

procedure TJsonOutputWriter.Init(ACompact: Boolean; AStream: TStream; AEncoding: TEncoding; ALines: TStrings);
begin
  FCompact := ACompact;
  FStream := AStream;
  FEncoding := AEncoding;

  if ALines <> nil then
  begin
    FCompact := False; // there is no compact version for TStrings
    FLines := ALines;
  end
  else
  begin
    FStreamEncodingBuffer := nil;
    FStreamEncodingBufferLen := 0;
    FLines := nil;
    FStringBuffer.Init;
  end;

  if not ACompact then
  begin
    FLastLine.Init;

    FIndent := 0;
    FLastType := ltInitial;

    // Set up some initial indention levels
    // TODO change to one buffer with #0 vs. IndentChar
    FIndents := AllocMem(5 * SizeOf(string));
    FIndentsLen := 5;
    //FIndents[0] := '';
    FIndents[1] := JsonSerializationConfig.IndentChar;
    FIndents[2] := FIndents[1] + JsonSerializationConfig.IndentChar;
    FIndents[3] := FIndents[2] + JsonSerializationConfig.IndentChar;
    FIndents[4] := FIndents[3] + JsonSerializationConfig.IndentChar;
  end;
end;

procedure TJsonOutputWriter.FreeIndents;
var
  i: Integer;
begin
  for i := 0 to FIndentsLen - 1 do
    FIndents[i] := '';
  FreeMem(FIndents);
end;

function TJsonOutputWriter.Done: string;
begin
  if not FCompact then
  begin
    FlushLastLine;
    FreeIndents;
    FLastLine.Done;
  end;

  if FLines = nil then
    FStringBuffer.DoneConvertToString(Result);
end;

procedure TJsonOutputWriter.LinesDone;
begin
  FreeIndents;
  FlushLastLine;
  FLastLine.Done;
end;

procedure TJsonOutputWriter.StreamDone;
begin
  if not FCompact then
  begin
    FlushLastLine;
    FreeIndents;
    FLastLine.Done;
  end;

  if FStream <> nil then
    StreamFlush;
  if FStreamEncodingBuffer <> nil then
    FreeMem(FStreamEncodingBuffer);
  FStringBuffer.Done;
end;

procedure TJsonOutputWriter.FlushLastLine;
var
  s: Pointer;
begin
  if FLastLine.Len > 0 then
  begin
    if FLines = nil then
    begin
      FLastLine.FlushToStringBuffer(FStringBuffer);
      FStringBuffer.Append(JsonSerializationConfig.LineBreak);
    end
    else
    begin
      s := nil;
      try
        FLastLine.FlushToString(string(s));
        FLines.Add(string(s));
      finally
        string(s) := '';
      end;
    end
  end;
end;

procedure TJsonOutputWriter.StreamFlush;
var
  Size: NativeInt;
begin
  if FStringBuffer.Len > 0 then
  begin
    if FEncoding = TEncoding.Unicode then
    begin
      FStream.write(FStringBuffer.Data[0], FStringBuffer.Len);
      FStringBuffer.FLen := 0;
    end
    else if FStream is TMemoryStream then
      FStringBuffer.FlushToMemoryStream(TMemoryStream(FStream), FEncoding)
    else
    begin
      Size := FStringBuffer.FlushToBytes(FStreamEncodingBuffer, FStreamEncodingBufferLen, FEncoding);
      if Size > 0 then
        FStream.write(FStreamEncodingBuffer[0], Size);
    end;
  end;
end;

procedure TJsonOutputWriter.StreamFlushPossible;
const
  MinFlushBufferLen = 1024 * 1024;
begin
  if (FStream <> nil) and (FStringBuffer.Len >= MinFlushBufferLen) then
    StreamFlush;
end;

procedure TJsonOutputWriter.ExpandIndents;
begin
  inc(FIndentsLen);
  ReallocMem(Pointer(FIndents), FIndentsLen * SizeOf(string));
  Pointer(FIndents[FIndent]) := nil;
  FIndents[FIndent] := FIndents[FIndent - 1] + JsonSerializationConfig.IndentChar;
end;

procedure TJsonOutputWriter.AppendLine(AppendOn: TLastType; const s: string);
begin
  if FLastType = AppendOn then
    FLastLine.Append(s)
  else
  begin
    FlushLastLine;
    StreamFlushPossible;
    FLastLine.Append2(FIndents[FIndent], PChar(Pointer(s)), length(s));
  end;
end;

procedure TJsonOutputWriter.AppendLine(AppendOn: TLastType; p: PChar; Len: Integer);
begin
  if FLastType = AppendOn then
    FLastLine.Append(p, Len)
  else
  begin
    FlushLastLine;
    StreamFlushPossible;
    FLastLine.Append2(FIndents[FIndent], p, Len);
  end;
end;

procedure TJsonOutputWriter.Indent(const s: string);
var
  This: ^TJsonOutputWriter;
begin
  This := @Self;
  if This.FCompact then
  begin
    This.FStringBuffer.Append(s);
    This.StreamFlushPossible; // inlined
  end
  else
  begin
    This.AppendLine(ltIntro, s); // inlined
    inc(This.FIndent);
    if This.FIndent >= This.FIndentsLen then // this is a new indention level
      ExpandIndents;
    This.FLastType := ltIndent;
  end;
end;

procedure TJsonOutputWriter.Unindent(const s: string);
var
  This: ^TJsonOutputWriter;
begin
  This := @Self;
  if This.FCompact then
  begin
    This.FStringBuffer.Append(s);
    This.StreamFlushPossible; // inlined
  end
  else
  begin
    dec(This.FIndent);
    //Assert(FIndent >= 0);
    This.AppendLine(ltIndent, s); // inlined
    This.FLastType := ltUnindent;
  end;
end;

procedure TJsonOutputWriter.AppendIntro(p: PChar; Len: Integer);
const
  sQuoteCharColon = '":';
var
  This: ^TJsonOutputWriter;
begin
  This := @Self;
  if This.FCompact then
  begin
    This.FStringBuffer.Append2(sQuoteChar, p, Len).Append(sQuoteCharColon, 2);
    This.StreamFlushPossible; // inlined
  end
  else
  begin
    FlushLastLine;
    This.StreamFlushPossible; // inlined
    This.FLastLine.Append(This.FIndents[This.FIndent]).Append2(sQuoteChar, p, Len).Append('": ', 3);
    This.FLastType := ltIntro;
  end;
end;

procedure TJsonOutputWriter.AppendValue(p: PChar; Len: Integer);
var
  This: ^TJsonOutputWriter;
begin
  This := @Self;
  if This.FCompact then
  begin
    This.FStringBuffer.Append(p, Len);
    This.StreamFlushPossible; // inlined
  end
  else
  begin
    This.AppendLine(ltIntro, p, Len); // inlined
    This.FLastType := ltValue;
  end;
end;

procedure TJsonOutputWriter.AppendValue(const s: string);
var
  This: ^TJsonOutputWriter;
begin
  This := @Self;
  if This.FCompact then
  begin
    This.FStringBuffer.Append(s);
    This.StreamFlushPossible; // inlined
  end
  else
  begin
    This.AppendLine(ltIntro, s); // inlined
    This.FLastType := ltValue;
  end;
end;

procedure TJsonOutputWriter.AppendStrValue(p: PChar; Len: Integer);
var
  This: ^TJsonOutputWriter;
begin
  This := @Self;
  if This.FCompact then
  begin
    This.FStringBuffer.Append3(sQuoteChar, p, Len, sQuoteChar);
    This.StreamFlushPossible; // inlined
  end
  else
  begin
    if This.FLastType = ltIntro then
      This.FLastLine.Append3(sQuoteChar, p, Len, sQuoteChar)
    else
    begin
      FlushLastLine;
      This.StreamFlushPossible; // inlined
      This.FLastLine.Append(This.FIndents[This.FIndent]).Append3(sQuoteChar, p, Len, sQuoteChar);
    end;
    This.FLastType := ltValue;
  end;
end;

procedure TJsonOutputWriter.AppendSeparator(const s: string);
var
  This: ^TJsonOutputWriter;
begin
  This := @Self;
  if This.FCompact then
  begin
    This.FStringBuffer.Append(s);
    This.StreamFlushPossible; // inlined
  end
  else
  begin
    if This.FLastType in [ltValue, ltUnindent] then
      This.FLastLine.Append(s)
    else
    begin
      FlushLastLine;
      This.StreamFlushPossible; // inlined
      This.FLastLine.Append2(This.FIndents[This.FIndent], PChar(Pointer(s)), length(s));
    end;
    This.FLastType := ltSeparator;
  end;
end;

{ TUtf8JsonReader }

constructor TUtf8JsonReader.Create(s: PByte; Len: NativeInt{$IFDEF SUPPORT_PROGRESS}; AProgress: PJsonReaderProgressRec{$ENDIF});
begin
  inherited Create(s{$IFDEF SUPPORT_PROGRESS}, Len * SizeOf(Byte), AProgress{$ENDIF});
  FText := s;
  FTextEnd := s + Len;
end;

function TUtf8JsonReader.GetCharOffset(StartPos: Pointer): NativeInt;
begin
  Result := FText - PByte(StartPos);
end;

function TUtf8JsonReader.Next: Boolean;
label
  EndReached;
var
  p, EndP: PByte;
  Ch: Byte;
begin
  p := FText;
  EndP := FTextEnd;
  {$IF CompilerVersion <= 30.0} // Delphi 10 Seattle or older
    {$IFNDEF CPUX64}
  Ch := 0; // silence compiler warning
    {$ENDIF ~CPUX64}
  {$IFEND}
  while True do
  begin
    while True do
    begin
      if p = EndP then
        goto EndReached; // use GOTO to eliminate doing the "P = EndP", "P < EndP" 3 times - wish there was a "Break loop-label;"
      Ch := p^;
      if Ch > 32 then
        Break;
      if not (Ch in [9, 32]) then
        Break;
      inc(p);
    end;

    case Ch of
      10:
        begin
          FLineStart := p + 1;
          inc(FLineNum);
        end;
      13:
        begin
          inc(FLineNum);
          if (p + 1 < EndP) and (p[1] = 10) then
            inc(p);
          FLineStart := p + 1;
        end;
    else
      Break;
    end;
    inc(p);
  end;

EndReached:
  if p < EndP then
  begin
    case p^ of
      Ord('{'):
        begin
          FLook.Kind := jtkLBrace;
          FText := p + 1;
        end;
      Ord('}'):
        begin
          FLook.Kind := jtkRBrace;
          FText := p + 1;
        end;
      Ord('['):
        begin
          FLook.Kind := jtkLBracket;
          FText := p + 1;
        end;
      Ord(']'):
        begin
          FLook.Kind := jtkRBracket;
          FText := p + 1;
        end;
      Ord(':'):
        begin
          FLook.Kind := jtkColon;
          FText := p + 1;
        end;
      Ord(','):
        begin
          FLook.Kind := jtkComma;
          FText := p + 1;
        end;
      Ord('"'): // String
        begin
          LexString(p{$IFDEF CPUARM}, EndP{$ENDIF});
          {$IFDEF SUPPORT_PROGRESS}
          if FProgress <> nil then
            CheckProgress(FText);
          {$ENDIF SUPPORT_PROGRESS}
        end;
      Ord('-'), Ord('0')..Ord('9'), Ord('.'): // Number
        begin
          LexNumber(p{$IFDEF CPUARM}, EndP{$ENDIF});
          {$IFDEF SUPPORT_PROGRESS}
          if FProgress <> nil then
            CheckProgress(FText);
          {$ENDIF SUPPORT_PROGRESS}
        end
    else
      LexIdent(p{$IFDEF CPUARM}, EndP{$ENDIF}); // Ident/Bool/NULL
      {$IFDEF SUPPORT_PROGRESS}
      if FProgress <> nil then
        CheckProgress(FText);
      {$ENDIF SUPPORT_PROGRESS}
    end;
    Result := True;
  end
  else
  begin
    FText := EndP;
    FLook.Kind := jtkEof;
    Result := False;
  end;
end;

procedure TUtf8JsonReader.LexString(p: PByte{$IFDEF CPUARM}; EndP: PByte{$ENDIF});
var
  {$IFNDEF CPUARM}
  EndP: PByte;
  {$ENDIF ~CPUARM}
  EscapeSequences: PByte;
  Ch: Byte;
  idx: Integer;
begin
  inc(p); // skip initiating '"'
  {$IFNDEF CPUARM}
  EndP := FTextEnd;
  {$ENDIF ~CPUARM}
  EscapeSequences := nil;
  Ch := 0;
  idx := p - EndP;

  // find the string end
  repeat
    if idx = 0 then
      Break;
    Ch := EndP[idx];
    if (Ch = Byte(Ord('"'))) or (Ch = 10) or (Ch = 13) then
      Break;
    inc(idx);
    if Ch <> Byte(Ord('\')) then
      Continue;
    if idx = 0 then // Eof reached in an escaped char => broken JSON string
      Break;
    if EscapeSequences = nil then
      EscapeSequences := @EndP[idx];
    inc(idx);
  until False;

  if idx = 0 then
  begin
    FText := p - 1;
    TJsonReader.StringNotClosedError(Self);
  end;

  EndP := @EndP[idx];
  if EscapeSequences = nil then
    SetStringUtf8(FLook.s, p, EndP - p)
  else
    TUtf8JsonReader.JSONUtf8StrToStr(p, EndP, EscapeSequences - p, FLook.s, Self);

  if Ch = Byte(Ord('"')) then
    inc(EndP);
  FLook.Kind := jtkString;
  FText := EndP;

  if Ch in [10, 13] then
    TJsonReader.InvalidStringCharacterError(Self);
end;

{$IFDEF ASMSUPPORT}
  {$IFDEF CPUX64}
function ParseUInt64Utf8(p, EndP: PByte): UInt64;
// RCX = P
// RDX = EndP
asm
  cmp rcx, rdx
  jge @@LeaveFail

  mov r8, rdx
  Sub rcx, r8
  // r8+rcx = EndP + NegOffset = P => NegOffset can be incremented and checked for zero

  movzx rax, Byte PTR [r8+rcx]
  Sub al, '0'
  Add rcx, 1
  jz @@Leave

@@Loop:
  Add rax, rax
  // rax = 2*Result
  lea rax, [rax+rax*4]
  // rax = (2*Result)*4 + (2*Result) = 10*Result

  movzx rdx, Byte PTR [r8+rcx]
  Sub dl, '0'
  Add rax, rdx

  Add rcx, 1
  jnz @@Loop

@@Leave:
  ret
@@LeaveFail:
  xor rax, rax
end;
  {$ENDIF CPUX64}
  {$IFDEF CPUX86}
function ParseUInt64Utf8(p, EndP: PByte): UInt64;
asm
  cmp eax, edx
  jge @@LeaveFail

  Push esi
  Push edi
  Push ebx

  mov esi, edx
  mov edi, eax
  Sub edi, edx
  // esi+edi = EndP + NegOffset = P => NegOffset can be incremented and checked for zero

  xor edx, edx
  movzx eax, Byte PTR [esi+edi]
  Sub al, '0'
  Add edi, 1
  jz @@PopLeave

@@Loop:
  Add eax, eax
  adc edx, edx
  // eax:edx = 2*Result
  mov ebx, eax
  mov ecx, edx
  // ebx:ecx = 2*Result
  shld edx, eax, 2
  shl eax, 2
  // eax:edx = (2*Result)*4
  Add eax, ebx
  adc edx, ecx
  // eax:edx = (2*Result)*4 + (2*Result) = 10*Result

  movzx ecx, Byte PTR [esi+edi]
  Sub cl, '0'
  Add eax, ecx
  adc edx, 0

  Add edi, 1
  jnz @@Loop

@@PopLeave:
  Pop ebx
  Pop edi
  Pop esi
@@Leave:
  ret
@@LeaveFail:
  xor eax, eax
  xor edx, edx
end;
  {$ENDIF CPUX86}
{$ELSE}
function ParseUInt64Utf8(p, EndP: PByte): UInt64;
begin
  if p = EndP then
    Result := 0
  else
  begin
    Result := p^ - Byte(Ord('0'));
    inc(p);
    while p < EndP do
    begin
      Result := Result * 10 + (p^ - Byte(Ord('0')));
      inc(p);
    end;
  end;
end;
{$ENDIF ASMSUPPORT}

function ParseAsDoubleUtf8(f, p: PByte): Double;
begin
  Result := 0.0;
  while f < p do
  begin
    Result := Result * 10 + (f^ - Byte(Ord('0')));
    inc(f);
  end;
end;

procedure TUtf8JsonReader.LexNumber(p: PByte{$IFDEF CPUARM}; EndP: PByte{$ENDIF});
var
  f: PByte;
  {$IFNDEF CPUARM}
  EndP: PByte;
  {$ENDIF ~CPUARM}
  EndInt64P: PByte;
  Ch: Byte;
  Value, Scale: Double;
  Exponent, IntValue: Integer;
  Neg, NegE: Boolean;
  DigitCount: Integer;
begin
  {$IFNDEF CPUARM}
  EndP := FTextEnd;
  {$ENDIF ~CPUARM}
  Neg := False;

  Ch := p^;
  if Ch = Byte(Ord('-')) then
  begin
    inc(p);
    if p >= EndP then
    begin
      FLook.Kind := jtkInvalidSymbol;
      FText := p;
      Exit;
    end;
    Neg := True;
    Ch := p^;
  end;
  f := p;

  inc(p);
  if Ch <> Byte(Ord('0')) then
  begin
    if Ch in [Ord('1')..Ord('9')] then
    begin
      while (p < EndP) and (p^ in [Ord('0')..Ord('9')]) do
        inc(p);
    end
    else
    begin
      FLook.Kind := jtkInvalidSymbol;
      FText := p;
      Exit;
    end;
  end;

  DigitCount := p - f;
  if DigitCount <= 9 then // Int32 fits 9 digits
  begin
    IntValue := 0;
    while f < p do
    begin
      IntValue := IntValue * 10 + (f^ - Byte(Ord('0')));
      inc(f);
    end;
    FLook.Hi := 0;
    FLook.i := IntValue;
    FLook.Kind := jtkInt;
    if not (p^ in [Ord('.'), Ord('E'), Ord('e')]) then
    begin
      // just an integer
      if Neg then
        FLook.i := -FLook.i;
      FText := p;
      Exit;
    end;
    Value := FLook.i;
  end
  else if DigitCount <= 20 then // UInt64 fits 20 digits (not all)
  begin
    FLook.u := ParseUInt64Utf8(f, p);
    if (DigitCount = 20) and (FLook.u mod 10 <> PByte(p - 1)^ - Byte(Ord('0'))) then // overflow => too large
      Value := ParseAsDoubleUtf8(f, p)
    else if Neg and ((DigitCount = 20) or ((DigitCount = 19) and (FLook.Hi and $80000000 <> 0))) then
      // "negative UInt64" doesn't fit into UInt64/Int64 => use Double
      Value := FLook.u
    else
    begin
      FLook.Kind := jtkLong;
      case DigitCount of
        19:
         if FLook.Hi and $80000000 <> 0 then // can't be negative because we cached that case already
           FLook.Kind := jtkULong;
        20:
          FLook.Kind := jtkULong;
      end;

      if not (p^ in [Ord('.'), Ord('E'), Ord('e')]) then
      begin
        // just an integer
        if Neg then
        begin
          if (FLook.Hi = 0) and (FLook.i >= 0) then // 32bit Integer
          begin
            FLook.i := -FLook.i;
            FLook.Kind := jtkInt;
          end
          else                 // 64bit Integer
            FLook.L := -FLook.L;
        end;
        FText := p;
        Exit;
      end;
      Value := FLook.u;
    end;
  end
  else
    Value := ParseAsDoubleUtf8(f, p);

  // decimal digits
  if (p + 1 < EndP) and (p^ = Byte(Ord('.'))) then
  begin
    inc(p);
    f := p;
    EndInt64P := f + 18;
    if EndInt64P > EndP then
      EndInt64P := EndP;
    while (p < EndInt64P) and (p^ in [Ord('0')..Ord('9')]) do
      inc(p);
    Value := Value + ParseUInt64Utf8(f, p) / Power10[p - f];

    // "Double" can't handle that many digits
    while (p < EndP) and (p^ in [Ord('0')..Ord('9')]) do
      inc(p);
  end;

  // exponent
  if (p < EndP) and (p^ in [Ord('e'), Ord('E')]) then
  begin
    inc(p);
    NegE := False;
    if (p < EndP) then
    begin
      case p^ of
        Ord('-'):
          begin
            NegE := True;
            inc(p);
          end;
        Ord('+'):
          inc(p);
      end;
      Exponent := 0;
      f := p;
      while (p < EndP) and (p^ in [Ord('0')..Ord('9')]) do
      begin
        Exponent := Exponent * 10 + (p^ - Byte(Ord('0')));
        inc(p);
      end;
      if p = f then
      begin
        // no exponent
        FLook.Kind := jtkInvalidSymbol;
        FText := p;
        Exit;
      end;

      if Exponent > 308 then
        Exponent := 308;

      Scale := 1.0;
      while Exponent >= 50 do
      begin
        Scale := Scale * 1E50;
        dec(Exponent, 50);
      end;
      while Exponent >= 18 do
      begin
        Scale := Scale * 1E18;
        dec(Exponent, 18);
      end;
      Scale := Scale * Power10[Exponent];

      if NegE then
        Value := Value / Scale
      else
        Value := Value * Scale;
    end
    else
    begin
      FLook.Kind := jtkInvalidSymbol;
      FText := p;
      Exit;
    end;
  end;

  if Neg then
    FLook.f := -Value
  else
    FLook.f := Value;
  FLook.Kind := jtkFloat;
  FText := p;
end;

procedure TUtf8JsonReader.LexIdent(p: PByte{$IFDEF CPUARM}; EndP: PByte{$ENDIF});
const
  {$IFDEF BIGENDIAN}
  // Big Endian
  NullStr = LongWord((Ord('n') shl 24) or (Ord('u') shl 16) or (Ord('l') shl 8) or Ord('l'));
  TrueStr = LongWord((Ord('t') shl 24) or (Ord('r') shl 16) or (Ord('u') shl 8) or Ord('e'));
  FalseStr = LongWord((Ord('a') shl 24) or (Ord('l') shl 16) or (Ord('s') shl 8) or Ord('e'));
  {$ELSE}
  // Little Endian
  NullStr = LongWord(Ord('n') or (Ord('u') shl 8) or (Ord('l') shl 16) or (Ord('l') shl 24));
  TrueStr = LongWord(Ord('t') or (Ord('r') shl 8) or (Ord('u') shl 16) or (Ord('e') shl 24));
  FalseStr = LongWord(Ord('a') or (Ord('l') shl 8) or (Ord('s') shl 16) or (Ord('e') shl 24));
  {$ENDIF BIGENDIAN}
var
  f: PByte;
  {$IFNDEF CPUARM}
  EndP: PByte;
  {$ENDIF ~CPUARM}
  L: LongWord;
begin
  f := p;
  {$IFNDEF CPUARM}
  EndP := FTextEnd;
  {$ENDIF ~CPUARM}
  case p^ of
    Ord('A')..Ord('Z'), Ord('a')..Ord('z'), Ord('_'), Ord('$'):
      begin
        inc(p);
//        DCC64 generates "bt mem,reg" code
//        while (P < EndP) and (P^ in [Ord('A')..Ord('Z'), Ord('a')..Ord('z'), Ord('_'), Ord('0')..Ord('9')]) do
//          Inc(P);
        while p < EndP do
          case p^ of
            Ord('A')..Ord('Z'), Ord('a')..Ord('z'), Ord('_'), Ord('0')..Ord('9'): inc(p);
          else
            Break;
          end;

        L := p - f;
        if L = 4 then
        begin
          L := PLongWord(f)^;
          if L = NullStr then
            FLook.Kind := jtkNull
          else if L = TrueStr then
            FLook.Kind := jtkTrue
          else
          begin
            SetStringUtf8(FLook.s, f, p - f);
            FLook.Kind := jtkIdent;
          end;
        end
        else if (L = 5) and (f^ = Ord('f')) and (PLongWord(f + 1)^ = FalseStr) then
          FLook.Kind := jtkFalse
        else
        begin
          SetStringUtf8(FLook.s, f, p - f);
          FLook.Kind := jtkIdent;
        end;
      end;
  else
    FLook.Kind := jtkInvalidSymbol;
    inc(p);
  end;
  FText := p;
end;

{ TStringJsonReader }

constructor TStringJsonReader.Create(s: PChar; Len: Integer{$IFDEF SUPPORT_PROGRESS}; AProgress: PJsonReaderProgressRec{$ENDIF});
begin
  inherited Create(s{$IFDEF SUPPORT_PROGRESS}, Len * SizeOf(WideChar), AProgress{$ENDIF});
  FText := s;
  FTextEnd := s + Len;
end;

function TStringJsonReader.GetCharOffset(StartPos: Pointer): NativeInt;
begin
  Result := FText - PChar(StartPos);
end;

function TStringJsonReader.Next: Boolean;
var
  p, EndP: PChar;
begin
  p := FText;
  EndP := FTextEnd;
  while (p < EndP) and (p^ <= #32) do
    inc(p);

  if p < EndP then
  begin
    case p^ of
      '{':
        begin
          FLook.Kind := jtkLBrace;
          FText := p + 1;
        end;
      '}':
        begin
          FLook.Kind := jtkRBrace;
          FText := p + 1;
        end;
      '[':
        begin
          FLook.Kind := jtkLBracket;
          FText := p + 1;
        end;
      ']':
        begin
          FLook.Kind := jtkRBracket;
          FText := p + 1;
        end;
      ':':
        begin
          FLook.Kind := jtkColon;
          FText := p + 1;
        end;
      ',':
        begin
          FLook.Kind := jtkComma;
          FText := p + 1;
        end;
      '"': // String
        begin
          LexString(p{$IFDEF CPUARM}, EndP{$ENDIF});
          {$IFDEF SUPPORT_PROGRESS}
          if FProgress <> nil then
            CheckProgress(FText);
          {$ENDIF SUPPORT_PROGRESS}
        end;
      '-', '0'..'9', '.': // Number
        begin
          LexNumber(p{$IFDEF CPUARM}, EndP{$ENDIF});
          {$IFDEF SUPPORT_PROGRESS}
          if FProgress <> nil then
            CheckProgress(FText);
          {$ENDIF SUPPORT_PROGRESS}
        end
    else
      LexIdent(p{$IFDEF CPUARM}, EndP{$ENDIF}); // Ident/Bool/NULL
      {$IFDEF SUPPORT_PROGRESS}
      if FProgress <> nil then
        CheckProgress(FText);
      {$ENDIF SUPPORT_PROGRESS}
    end;
    Result := True;
  end
  else
  begin
    FText := EndP;
    FLook.Kind := jtkEof;
    Result := False;
  end;
end;

procedure TStringJsonReader.LexString(p: PChar{$IFDEF CPUARM}; EndP: PChar{$ENDIF});
var
  {$IFNDEF CPUARM}
  EndP: PChar;
  {$ENDIF ~CPUARM}
  EscapeSequences: PChar;
  Ch: Char;
  idx: Integer;
begin
  inc(p); // skip initiating '"'
  {$IFNDEF CPUARM}
  EndP := FTextEnd;
  {$ENDIF ~CPUARM}
  EscapeSequences := nil;
  Ch := #0;
  idx := p - EndP;

  // find the string end
  repeat
    if idx = 0 then
      Break;
    Ch := EndP[idx];
    if (Ch = '"') or (Ch = #10) or (Ch = #13) then
      Break;
    inc(idx);
    if Ch <> '\' then
      Continue;
    if idx = 0 then // Eof reached in an escaped char => broken JSON string
      Break;
    if EscapeSequences = nil then
      EscapeSequences := @EndP[idx];
    inc(idx);
  until False;

  if idx = 0 then
  begin
    FText := p - 1;
    TJsonReader.StringNotClosedError(Self);
  end;

  EndP := @EndP[idx];
  if EscapeSequences = nil then
    SetString(FLook.s, p, EndP - p)
  else
    TJsonReader.JSONStrToStr(p, EndP, EscapeSequences - p, FLook.s, Self);

  if Ch = '"' then
    inc(EndP);
  FLook.Kind := jtkString;
  FText := EndP;

  if Ch in [#10, #13] then
    TJsonReader.InvalidStringCharacterError(Self);
end;

{$IFDEF ASMSUPPORT}
  {$IFDEF CPUX64}
function ParseUInt64(p, EndP: PWideChar): UInt64;
// RCX = P
// RDX = EndP
asm
  cmp rcx, rdx
  jge @@LeaveFail

  mov r8, rdx
  Sub rcx, r8
  // r8+rcx = EndP + NegOffset = P => NegOffset can be incremented and checked for zero

  movzx rax, Word PTR [r8+rcx]
  Sub Ax, '0'
  Add rcx, 2
  jz @@Leave

@@Loop:
  Add rax, rax
  // rax = 2*Result
  lea rax, [rax+rax*4]
  // rax = (2*Result)*4 + (2*Result) = 10*Result

  movzx rdx, Word PTR [r8+rcx]
  Sub dx, '0'
  Add rax, rdx

  Add rcx, 2
  jnz @@Loop

@@Leave:
  ret
@@LeaveFail:
  xor rax, rax
end;
  {$ENDIF CPUX64}
  {$IFDEF CPUX86}
function ParseUInt64(p, EndP: PWideChar): UInt64;
asm
  cmp eax, edx
  jge @@LeaveFail

  Push esi
  Push edi
  Push ebx

  mov esi, edx
  mov edi, eax
  Sub edi, edx
  // esi+edi = EndP + NegOffset = P => NegOffset can be incremented and checked for zero

  xor edx, edx
  movzx eax, Word PTR [esi+edi]
  Sub Ax, '0'
  Add edi, 2
  jz @@PopLeave

@@Loop:
  Add eax, eax
  adc edx, edx
  // eax:edx = 2*Result
  mov ebx, eax
  mov ecx, edx
  // ebx:ecx = 2*Result
  shld edx, eax, 2
  shl eax, 2
  // eax:edx = (2*Result)*4
  Add eax, ebx
  adc edx, ecx
  // eax:edx = (2*Result)*4 + (2*Result) = 10*Result

  movzx ecx, Word PTR [esi+edi]
  Sub Cx, '0'
  Add eax, ecx
  adc edx, 0

  Add edi, 2
  jnz @@Loop

@@PopLeave:
  Pop ebx
  Pop edi
  Pop esi
@@Leave:
  ret
@@LeaveFail:
  xor eax, eax
  xor edx, edx
end;
  {$ENDIF CPUX86}
{$ELSE}
function ParseUInt64(p, EndP: PWideChar): UInt64;
begin
  if p = EndP then
    Result := 0
  else
  begin
    Result := Ord(p^) - Ord('0');
    inc(p);
    while p < EndP do
    begin
      Result := Result * 10 + (Ord(p^) - Ord('0'));
      inc(p);
    end;
  end;
end;
{$ENDIF ASMSUPPORT}

function ParseAsDouble(f, p: PWideChar): Double;
begin
  Result := 0.0;
  while f < p do
  begin
    Result := Result * 10 + (Ord(f^) - Ord('0'));
    inc(f);
  end;
end;

procedure TStringJsonReader.LexNumber(p: PChar{$IFDEF CPUARM}; EndP: PChar{$ENDIF});
var
  f: PChar;
  {$IFNDEF CPUARM}
  EndP: PChar;
  {$ENDIF ~CPUARM}
  EndInt64P: PChar;
  Ch: Char;
  Value, Scale: Double;
  Exponent, IntValue: Integer;
  Neg, NegE: Boolean;
  DigitCount: Integer;
begin
  {$IFNDEF CPUARM}
  EndP := FTextEnd;
  {$ENDIF ~CPUARM}
  Neg := False;

  Ch := p^;
  if Ch = '-' then
  begin
    inc(p);
    if p >= EndP then
    begin
      FLook.Kind := jtkInvalidSymbol;
      FText := p;
      Exit;
    end;
    Neg := True;
    Ch := p^;
  end;
  f := p;

  inc(p);
  if Ch <> '0' then
  begin
    if Ch in ['1'..'9'] then
    begin
      while (p < EndP) and (p^ in ['0'..'9']) do
        inc(p);
    end
    else
    begin
      FLook.Kind := jtkInvalidSymbol;
      FText := p;
      Exit;
    end;
  end;

  DigitCount := p - f;
  if DigitCount <= 9 then // Int32 fits 9 digits
  begin
    IntValue := 0;
    while f < p do
    begin
      IntValue := IntValue * 10 + (Ord(f^) - Ord('0'));
      inc(f);
    end;
    FLook.Hi := 0;
    FLook.i := IntValue;
    FLook.Kind := jtkInt;
    if not (p^ in ['.', 'E', 'e']) then
    begin
      // just an integer
      if Neg then
        FLook.i := -FLook.i;
      FText := p;
      Exit;
    end;
    Value := FLook.i;
  end
  else if DigitCount <= 20 then // UInt64 fits 20 digits (not all)
  begin
    FLook.u := ParseUInt64(f, p);
    if (DigitCount = 20) and (FLook.u mod 10 <> Ord(PWideChar(p - 1)^) - Ord('0')) then // overflow => too large
      Value := ParseAsDouble(f, p)
    else if Neg and ((DigitCount = 20) or ((DigitCount = 19) and (FLook.Hi and $80000000 <> 0))) then
      // "negative UInt64" doesn't fit into UInt64/Int64 => use Double
      Value := FLook.u
    else
    begin
      FLook.Kind := jtkLong;
      case DigitCount of
        19:
         if FLook.Hi and $80000000 <> 0 then // can't be negative because we cached that case already
           FLook.Kind := jtkULong;
        20:
          FLook.Kind := jtkULong;
      end;

      if not (p^ in ['.', 'E', 'e']) then
      begin
        // just an integer
        if Neg then
        begin
          if (FLook.Hi = 0) and (FLook.i >= 0) then // 32bit Integer
          begin
            FLook.i := -FLook.i;
            FLook.Kind := jtkInt;
          end
          else                 // 64bit Integer
            FLook.L := -FLook.L;
        end;
        FText := p;
        Exit;
      end;
      Value := FLook.u;
    end;
  end
  else
    Value := ParseAsDouble(f, p);

  // decimal digits
  if (p + 1 < EndP) and (p^ = '.') then
  begin
    inc(p);
    f := p;
    EndInt64P := f + 18;
    if EndInt64P > EndP then
      EndInt64P := EndP;
    while (p < EndInt64P) and (p^ in ['0'..'9']) do
      inc(p);
    Value := Value + ParseUInt64(f, p) / Power10[p - f];

    // "Double" can't handle that many digits
    while (p < EndP) and (p^ in ['0'..'9']) do
      inc(p);
  end;

  // exponent
  if (p < EndP) and ((p^ = 'e') or (p^ = 'E')) then
  begin
    inc(p);
    NegE := False;
    if (p < EndP) then
    begin
      case p^ of
        '-':
          begin
            NegE := True;
            inc(p);
          end;
        '+':
          inc(p);
      end;
      Exponent := 0;
      f := p;
      while (p < EndP) and (p^ in ['0'..'9']) do
      begin
        Exponent := Exponent * 10 + (Ord(p^) - Ord('0'));
        inc(p);
      end;
      if p = f then
      begin
        // no exponent
        FLook.Kind := jtkInvalidSymbol;
        FText := p;
        Exit;
      end;

      if Exponent > 308 then
        Exponent := 308;

      Scale := 1.0;
      while Exponent >= 50 do
      begin
        Scale := Scale * 1E50;
        dec(Exponent, 50);
      end;
      while Exponent >= 18 do
      begin
        Scale := Scale * 1E18;
        dec(Exponent, 18);
      end;
      Scale := Scale * Power10[Exponent];

      if NegE then
        Value := Value / Scale
      else
        Value := Value * Scale;
    end
    else
    begin
      FLook.Kind := jtkInvalidSymbol;
      FText := p;
      Exit;
    end;
  end;

  if Neg then
    FLook.f := -Value
  else
    FLook.f := Value;
  FLook.Kind := jtkFloat;
  FText := p;
end;

procedure TStringJsonReader.LexIdent(p: PChar{$IFDEF CPUARM}; EndP: PChar{$ENDIF});
const
  {$IFDEF BIGENDIAN}
  // Big Endian
  NullStr1 = LongWord((Ord('n') shl 16) or Ord('u'));
  NullStr2 = LongWord((Ord('l') shl 16) or Ord('l'));
  TrueStr1 = LongWord((Ord('t') shl 16) or Ord('r'));
  TrueStr2 = LongWord((Ord('u') shl 16) or Ord('e'));
  FalseStr1 = LongWord((Ord('a') shl 16) or Ord('l'));
  FalseStr2 = LongWord((Ord('s') shl 16) or Ord('e'));
  {$ELSE}
  // Little Endian
  NullStr1 = LongWord(Ord('n') or (Ord('u') shl 16));
  NullStr2 = LongWord(Ord('l') or (Ord('l') shl 16));
  TrueStr1 = LongWord(Ord('t') or (Ord('r') shl 16));
  TrueStr2 = LongWord(Ord('u') or (Ord('e') shl 16));
  FalseStr1 = LongWord(Ord('a') or (Ord('l') shl 16));
  FalseStr2 = LongWord(Ord('s') or (Ord('e') shl 16));
  {$ENDIF BIGENDIAN}
var
  f: PChar;
  {$IFNDEF CPUARM}
  EndP: PChar;
  {$ENDIF ~CPUARM}
  L: LongWord;
begin
  f := p;
  {$IFNDEF CPUARM}
  EndP := FTextEnd;
  {$ENDIF ~CPUARM}
  case p^ of
    'A'..'Z', 'a'..'z', '_', '$':
      begin
        inc(p);
//        DCC64 generates "bt mem,reg" code
//        while (P < EndP) and (P^ in ['A'..'Z', 'a'..'z', '_', '0'..'9']) do
//          Inc(P);
        while p < EndP do
          case p^ of
            'A'..'Z', 'a'..'z', '_', '0'..'9': inc(p);
          else
            Break;
          end;

        L := p - f;
        if L = 4 then
        begin
          L := PLongWord(f)^;
          if (L = NullStr1) and (PLongWord(f + 2)^ = NullStr2) then
            FLook.Kind := jtkNull
          else if (L = TrueStr1) and (PLongWord(f + 2)^ = TrueStr2) then
            FLook.Kind := jtkTrue
          else
          begin
            SetString(FLook.s, f, p - f);
            FLook.Kind := jtkIdent;
          end;
        end
        else if (L = 5) and (f^ = 'f') and (PLongWord(f + 1)^ = FalseStr1) and (PLongWord(f + 3)^ = FalseStr2) then
          FLook.Kind := jtkFalse
        else
        begin
          SetString(FLook.s, f, p - f);
          FLook.Kind := jtkIdent;
        end;
      end;
  else
    FLook.Kind := jtkInvalidSymbol;
    inc(p);
  end;
  FText := p;
end;

{ TJsonDataValueHelper }

class operator TJsonDataValueHelper.Implicit(const Value: string): TJsonDataValueHelper;
begin
  Result.FData.FName := '';
  Result.FData.FNameResolver := nil;
  Result.FData.FIntern := nil;
  {$IFDEF AUTOREFCOUNT}
  if Result.FData.FObj <> nil then
    Result.FData.FObj := nil;
  {$ENDIF AUTOREFCOUNT}
  Result.FData.FTyp := jdtString;
  Result.FData.FValue := Value;
end;

class operator TJsonDataValueHelper.Implicit(const Value: TJsonDataValueHelper): string;
begin
  if Value.FData.FIntern <> nil then
    Result := Value.FData.FIntern.Value
  else
    case Value.FData.FTyp of
      jdtString:
        Result := Value.FData.FValue;
      jdtInt:
        Result := IntToStr(Value.FData.FIntValue);
      jdtLong:
        Result := IntToStr(Value.FData.FLongValue);
      jdtULong:
        Result := UIntToStr(Value.FData.FULongValue);
      jdtFloat:
        Result := FloatToStr(Value.FData.FFloatValue, JSONFormatSettings);
      jdtDateTime:
        Result := TJsonBaseObject.DateTimeToJSON(Value.FData.FDateTimeValue, JsonSerializationConfig.UseUtcTime);
      jdtBool:
        if Value.FData.FBoolValue then
          Result := sTrue
        else
          Result := sFalse;
    else
      Result := '';
    end;
end;

class operator TJsonDataValueHelper.Implicit(const Value: Integer): TJsonDataValueHelper;
begin
  Result.FData.FName := '';
  Result.FData.FNameResolver := nil;
  Result.FData.FIntern := nil;
  {$IFDEF AUTOREFCOUNT}
  if Result.FData.FObj <> nil then
    Result.FData.FObj := nil;
  {$ENDIF AUTOREFCOUNT}
  Result.FData.FTyp := jdtInt;
  Result.FData.FIntValue := Value;
end;

class operator TJsonDataValueHelper.Implicit(const Value: TJsonDataValueHelper): Integer;
begin
  if Value.FData.FIntern <> nil then
    Result := Value.FData.FIntern.IntValue
  else
    case Value.FData.FTyp of
      jdtString:
        Result := StrToIntDef(Value.FData.FValue, 0);
      jdtInt:
        Result := Value.FData.FIntValue;
      jdtLong:
        Result := Value.FData.FLongValue;
      jdtULong:
        Result := Value.FData.FULongValue;
      jdtFloat:
        Result := Trunc(Value.FData.FFloatValue);
      jdtDateTime:
        Result := Trunc(Value.FData.FDateTimeValue);
      jdtBool:
        Result := Ord(Value.FData.FBoolValue);
    else
      Result := 0;
    end;
end;

class operator TJsonDataValueHelper.Implicit(const Value: Int64): TJsonDataValueHelper;
begin
  Result.FData.FName := '';
  Result.FData.FNameResolver := nil;
  Result.FData.FIntern := nil;
  {$IFDEF AUTOREFCOUNT}
  if Result.FData.FObj <> nil then
    Result.FData.FObj := nil;
  {$ENDIF AUTOREFCOUNT}
  Result.FData.FTyp := jdtLong;
  Result.FData.FLongValue := Value;
end;

class operator TJsonDataValueHelper.Implicit(const Value: TJsonDataValueHelper): Int64;
begin
  if Value.FData.FIntern <> nil then
    Result := Value.FData.FIntern.LongValue
  else
    case Value.FData.FTyp of
      jdtString:
        Result := StrToInt64Def(Value.FData.FValue, 0);
      jdtInt:
        Result := Value.FData.FIntValue;
      jdtLong:
        Result := Value.FData.FLongValue;
      jdtULong:
        Result := Value.FData.FULongValue;
      jdtFloat:
        Result := Trunc(Value.FData.FFloatValue);
      jdtDateTime:
        Result := Trunc(Value.FData.FDateTimeValue);
      jdtBool:
        Result := Ord(Value.FData.FBoolValue);
    else
      Result := 0;
    end;
end;

//class operator TJsonDataValueHelper.Implicit(const Value: UInt64): TJsonDataValueHelper;
//begin
//  Result.FData.FName := '';
//  Result.FData.FNameResolver := nil;
//  Result.FData.FIntern := nil;
//  {$IFDEF AUTOREFCOUNT}
//  if Result.FData.FObj <> nil then
//    Result.FData.FObj := nil;
//  {$ENDIF AUTOREFCOUNT}
//  Result.FData.FTyp := jdtULong;
//  Result.FData.FULongValue := Value;
//end;
//
//class operator TJsonDataValueHelper.Implicit(const Value: TJsonDataValueHelper): UInt64;
//begin
//  if Value.FData.FIntern <> nil then
//    Result := Value.FData.FIntern.LongValue
//  else
//    case Value.FData.FTyp of
//      jdtString:
//        Result := StrToInt64Def(Value.FData.FValue, 0);
//      jdtInt:
//        Result := Value.FData.FIntValue;
//      jdtLong:
//        Result := Value.FData.FLongValue;
//      jdtULong:
//        Result := Value.FData.FULongValue;
//      jdtFloat:
//        Result := Trunc(Value.FData.FFloatValue);
//      jdtDateTime:
//        Result := Trunc(Value.FData.FDateTimeValue);
//      jdtBool:
//        Result := Ord(Value.FData.FBoolValue);
//    else
//      Result := 0;
//    end;
//end;

class operator TJsonDataValueHelper.Implicit(const Value: Double): TJsonDataValueHelper;
begin
  Result.FData.FName := '';
  Result.FData.FNameResolver := nil;
  Result.FData.FIntern := nil;
  {$IFDEF AUTOREFCOUNT}
  if Result.FData.FObj <> nil then
    Result.FData.FObj := nil;
  {$ENDIF AUTOREFCOUNT}
  Result.FData.FTyp := jdtFloat;
  Result.FData.FFloatValue := Value;
end;

class operator TJsonDataValueHelper.Implicit(const Value: TJsonDataValueHelper): Double;
begin
  if Value.FData.FIntern <> nil then
    Result := Value.FData.FIntern.FloatValue
  else
    case Value.FData.FTyp of
      jdtString:
        Result := StrToFloat(Value.FData.FValue, JSONFormatSettings);
      jdtInt:
        Result := Value.FData.FIntValue;
      jdtLong:
        Result := Value.FData.FLongValue;
      jdtULong:
        Result := Value.FData.FULongValue;
      jdtFloat:
        Result := Value.FData.FFloatValue;
      jdtDateTime:
        Result := Value.FData.FDateTimeValue;
      jdtBool:
        Result := Ord(Value.FData.FBoolValue);
    else
      Result := 0;
    end;
end;

class operator TJsonDataValueHelper.Implicit(const Value: Extended): TJsonDataValueHelper;  // same that double
begin
  Result.FData.FName := '';
  Result.FData.FNameResolver := nil;
  Result.FData.FIntern := nil;
  {$IFDEF AUTOREFCOUNT}
  if Result.FData.FObj <> nil then
    Result.FData.FObj := nil;
  {$ENDIF AUTOREFCOUNT}
  Result.FData.FTyp := jdtFloat;
  Result.FData.FFloatValue := Value;
end;

class operator TJsonDataValueHelper.Implicit(const Value: TJsonDataValueHelper): Extended;  // same that double
begin
  if Value.FData.FIntern <> nil then
    Result := Value.FData.FIntern.FloatValue
  else
    case Value.FData.FTyp of
      jdtString:
        Result := StrToFloat(Value.FData.FValue, JSONFormatSettings);
      jdtInt:
        Result := Value.FData.FIntValue;
      jdtLong:
        Result := Value.FData.FLongValue;
      jdtULong:
        Result := Value.FData.FULongValue;
      jdtFloat:
        Result := Value.FData.FFloatValue;
      jdtDateTime:
        Result := Value.FData.FDateTimeValue;
      jdtBool:
        Result := Ord(Value.FData.FBoolValue);
    else
      Result := 0;
    end;
end;

class operator TJsonDataValueHelper.Implicit(const Value: TDateTime): TJsonDataValueHelper;
begin
  Result.FData.FName := '';
  Result.FData.FNameResolver := nil;
  Result.FData.FIntern := nil;
  {$IFDEF AUTOREFCOUNT}
  if Result.FData.FObj <> nil then
    Result.FData.FObj := nil;
  {$ENDIF AUTOREFCOUNT}
  Result.FData.FTyp := jdtDateTime;
  Result.FData.FDateTimeValue := Value;
end;

class operator TJsonDataValueHelper.Implicit(const Value: TJsonDataValueHelper): TDateTime;
begin
  if Value.FData.FIntern <> nil then
    Result := Value.FData.FIntern.DateTimeValue
  else
    case Value.FData.FTyp of
      jdtString:
        Result := TJsonBaseObject.JSONToDateTime(Value.FData.FValue);
      jdtInt:
        Result := Value.FData.FIntValue;
      jdtLong:
        Result := Value.FData.FLongValue;
      jdtULong:
        Result := Value.FData.FULongValue;
      jdtFloat:
        Result := Value.FData.FFloatValue;
      jdtDateTime:
        Result := Value.FData.FDateTimeValue;
      jdtBool:
        Result := Ord(Value.FData.FBoolValue);
    else
      Result := 0;
    end;
end;

class operator TJsonDataValueHelper.Implicit(const Value: Boolean): TJsonDataValueHelper;
begin
  Result.FData.FName := '';
  Result.FData.FNameResolver := nil;
  Result.FData.FIntern := nil;
  {$IFDEF AUTOREFCOUNT}
  if Result.FData.FObj <> nil then
    Result.FData.FObj := nil;
  {$ENDIF AUTOREFCOUNT}
  Result.FData.FTyp := jdtBool;
  Result.FData.FBoolValue := Value;
end;

class operator TJsonDataValueHelper.Implicit(const Value: TJsonDataValueHelper): Boolean;
begin
  if Value.FData.FIntern <> nil then
    Result := Value.FData.FIntern.BoolValue
  else
    case Value.FData.FTyp of
      jdtString:
        Result := Value.FData.FValue = 'true';
      jdtInt:
        Result := Value.FData.FIntValue <> 0;
      jdtLong:
        Result := Value.FData.FLongValue <> 0;
      jdtULong:
        Result := Value.FData.FULongValue <> 0;
      jdtFloat:
        Result := Value.FData.FFloatValue <> 0;
      jdtDateTime:
        Result := Value.FData.FDateTimeValue <> 0;
      jdtBool:
        Result := Value.FData.FBoolValue;
    else
      Result := False;
    end;
end;

class operator TJsonDataValueHelper.Implicit(const Value: TJsonArray): TJsonDataValueHelper;
begin
  Result.FData.FName := '';
  Result.FData.FNameResolver := nil;
  Result.FData.FIntern := nil;
  {$IFDEF AUTOREFCOUNT}
  if Result.FData.FValue <> '' then
    Result.FData.FValue := '';
  {$ENDIF AUTOREFCOUNT}
  Result.FData.FTyp := jdtArray;
  Result.FData.FObj := Value;
end;

class operator TJsonDataValueHelper.Implicit(const Value: TJsonDataValueHelper): TJsonArray;
begin
  Value.ResolveName;
  if Value.FData.FIntern <> nil then
  begin
    if Value.FData.FIntern.FTyp = jdtNone then
      Value.FData.FIntern.ArrayValue := TJsonArray.Create;
    Result := Value.FData.FIntern.ArrayValue;
  end
  else if Value.FData.FTyp = jdtArray then
    Result := TJsonArray(Value.FData.FObj)
  else
    Result := nil;
end;

class operator TJsonDataValueHelper.Implicit(const Value: TJsonObject): TJsonDataValueHelper;
begin
  Result.FData.FName := '';
  Result.FData.FNameResolver := nil;
  Result.FData.FIntern := nil;
  {$IFDEF AUTOREFCOUNT}
  if Result.FData.FValue <> '' then
    Result.FData.FValue := '';
  {$ENDIF AUTOREFCOUNT}
  Result.FData.FTyp := jdtObject;
  Result.FData.FObj := Value;
end;

class operator TJsonDataValueHelper.Implicit(const Value: TJsonDataValueHelper): TJsonObject;
begin
  Value.ResolveName;
  if Value.FData.FIntern <> nil then
  begin
    if Value.FData.FIntern.FTyp = jdtNone then
      Value.FData.FIntern.ObjectValue := TJsonObject.Create;
    Result := Value.FData.FIntern.ObjectValue;
  end
  else if Value.FData.FTyp = jdtObject then
    Result := TJsonObject(Value.FData.FObj)
  else
    Result := nil;
end;

class operator TJsonDataValueHelper.Implicit(const Value: Pointer): TJsonDataValueHelper;
begin
  Result.FData.FName := '';
  Result.FData.FNameResolver := nil;
  Result.FData.FIntern := nil;
  {$IFDEF AUTOREFCOUNT}
  if Result.FData.FValue <> '' then
    Result.FData.FValue := '';
  {$ENDIF AUTOREFCOUNT}
  Result.FData.FTyp := jdtObject;
  Result.FData.FObj := nil;
end;

class operator TJsonDataValueHelper.Implicit(const Value: TJsonDataValueHelper): Variant;
begin
  if Value.FData.FIntern <> nil then
    Result := Value.FData.FIntern.VariantValue
  else
    case Value.FData.FTyp of
      jdtNone:
        Result := Unassigned;
      jdtString:
        Result := Value.FData.FValue;
      jdtInt:
        Result := Value.FData.FIntValue;
      jdtLong:
        Result := Value.FData.FLongValue;
      jdtULong:
        Result := Value.FData.FULongValue;
      jdtFloat:
        Result := Value.FData.FFloatValue;
      jdtDateTime:
        Result := Value.FData.FDateTimeValue;
      jdtBool:
        Result := Value.FData.FBoolValue;
      jdtArray:
        ErrorUnsupportedVariantType(varArray);
      jdtObject:
        if Value.FData.FObj = nil then
          Result := Null
        else
          ErrorUnsupportedVariantType(varObject);
    else
      ErrorUnsupportedVariantType(varAny);
    end;
end;

class operator TJsonDataValueHelper.Implicit(const Value: Variant): TJsonDataValueHelper;
var
  LTyp: TJsonDataType;
begin
  Result.FData.FName := '';
  Result.FData.FNameResolver := nil;
  Result.FData.FIntern := nil;
  {$IFDEF AUTOREFCOUNT}
  if Result.FData.FObj <> nil then
    Result.FData.FObj := nil;
  {$ENDIF AUTOREFCOUNT}

  LTyp := VarTypeToJsonDataType(VarType(Value));
  if LTyp <> jdtNone then
  begin
    Result.FData.FTyp := LTyp;
    case LTyp of
      jdtString:
        string(Result.FData.FValue) := Value;
      jdtInt:
        Result.FData.FIntValue := Value;
      jdtLong:
        Result.FData.FLongValue := Value;
      jdtULong:
        Result.FData.FULongValue := Value;
      jdtFloat:
        Result.FData.FFloatValue := Value;
      jdtDateTime:
        Result.FData.FDateTimeValue := Value;
      jdtBool:
        Result.FData.FBoolValue := Value;
    end;
  end;
end;

function TJsonDataValueHelper.GetValue: string;
begin
  Result := Self;
end;

procedure TJsonDataValueHelper.SetValue(const Value: string);
begin
  ResolveName;
  if FData.FIntern <> nil then
    FData.FIntern.Value := Value
  else
    Self := Value;
end;

function TJsonDataValueHelper.GetIntValue: Integer;
begin
  Result := Self;
end;

procedure TJsonDataValueHelper.SetIntValue(const Value: Integer);
begin
  ResolveName;
  if FData.FIntern <> nil then
    FData.FIntern.IntValue := Value
  else
    Self := Value;
end;

function TJsonDataValueHelper.GetLongValue: Int64;
begin
  Result := Self;
end;

procedure TJsonDataValueHelper.SetLongValue(const Value: Int64);
begin
  ResolveName;
  if FData.FIntern <> nil then
    FData.FIntern.LongValue := Value
  else
    Self := Value;
end;

function TJsonDataValueHelper.GetULongValue: UInt64;
begin
//  Result := Self;
  // copied from UInt64 implicit operator
  if FData.FIntern <> nil then
    Result := FData.FIntern.LongValue
  else
    case FData.FTyp of
      jdtString:
        Result := StrToInt64Def(FData.FValue, 0);
      jdtInt:
        Result := FData.FIntValue;
      jdtLong:
        Result := FData.FLongValue;
      jdtULong:
        Result := FData.FULongValue;
      jdtFloat:
        Result := Trunc(FData.FFloatValue);
      jdtDateTime:
        Result := Trunc(FData.FDateTimeValue);
      jdtBool:
        Result := Ord(FData.FBoolValue);
    else
      Result := 0;
    end;
end;

procedure TJsonDataValueHelper.SetULongValue(const Value: UInt64);
begin
  ResolveName;
  if FData.FIntern <> nil then
    FData.FIntern.ULongValue := Value
  else
  begin
    //Self := Value;
    // copied from UInt64 implicit operator
    FData.FName := '';
    FData.FNameResolver := nil;
    FData.FIntern := nil;
    {$IFDEF AUTOREFCOUNT}
    if FData.FObj <> nil then
      FData.FObj := nil;
    {$ENDIF AUTOREFCOUNT}
    FData.FTyp := jdtLong;
    FData.FLongValue := Value;
  end;
end;

function TJsonDataValueHelper.GetFloatValue: Double;
begin
  Result := Self;
end;

procedure TJsonDataValueHelper.SetFloatValue(const Value: Double);
begin
  ResolveName;
  if FData.FIntern <> nil then
    FData.FIntern.FloatValue := Value
  else
    Self := Value;
end;

function TJsonDataValueHelper.GetDateTimeValue: TDateTime;
begin
  Result := Self;
end;

procedure TJsonDataValueHelper.SetDateTimeValue(const Value: TDateTime);
begin
  ResolveName;
  if FData.FIntern <> nil then
    FData.FIntern.DateTimeValue := Value
  else
    Self := Value;
end;

function TJsonDataValueHelper.GetBoolValue: Boolean;
begin
  Result := Self;
end;

procedure TJsonDataValueHelper.SetBoolValue(const Value: Boolean);
begin
  ResolveName;
  if FData.FIntern <> nil then
    FData.FIntern.BoolValue := Value
  else
    Self := Value;
end;

function TJsonDataValueHelper.GetArrayValue: TJsonArray;
begin
  Result := Self;
end;

procedure TJsonDataValueHelper.SetArrayValue(const Value: TJsonArray);
begin
  ResolveName;
  if FData.FIntern <> nil then
    FData.FIntern.ArrayValue := Value
  else
    Self := Value;
end;

function TJsonDataValueHelper.GetObjectValue: TJsonObject;
begin
  Result := Self;
end;

procedure TJsonDataValueHelper.SetObjectValue(const Value: TJsonObject);
begin
  ResolveName;
  if FData.FIntern <> nil then
    FData.FIntern.ObjectValue := Value
  else
    Self := Value;
end;

function TJsonDataValueHelper.GetVariantValue: Variant;
begin
  Result := Self;
end;

procedure TJsonDataValueHelper.SetVariantValue(const Value: Variant);
begin
  ResolveName;
  if FData.FIntern <> nil then
    FData.FIntern.VariantValue := Value
  else
    Self := Value;
end;

function TJsonDataValueHelper.GetTyp: TJsonDataType;
begin
  if FData.FIntern <> nil then
    Result := FData.FIntern.Typ
  else
    Result := FData.FTyp;
end;

function TJsonDataValueHelper.IsNull: Boolean;
begin
  if FData.FIntern <> nil then
    Result := FData.FIntern.IsNull
  else
  begin
    case FData.FTyp of
      jdtNone:
        Result := True;
      jdtObject:
        Result := FData.FObj = nil;
    else
      Result := False;
    end;
  end;
end;

class procedure TJsonDataValueHelper.SetInternValue(Item: PJsonDataValue;
  const Value: TJsonDataValueHelper);
begin
  Value.ResolveName;
  if Value.FData.FIntern <> nil then
  begin
    Item.Clear;
    TJsonBaseObject.InternInitAndAssignItem(Item, Value.FData.FIntern); // clones arrays and objects
  end
  else
  begin
    case Value.FData.FTyp of
      jdtString:
        Item.Value := Value.FData.FValue;
      jdtInt:
        Item.IntValue := Value.FData.FIntValue;
      jdtLong:
        Item.LongValue := Value.FData.FLongValue;
      jdtULong:
        Item.ULongValue := Value.FData.FULongValue;
      jdtFloat:
        Item.FloatValue := Value.FData.FFloatValue;
      jdtDateTime:
        Item.DateTimeValue := Value.FData.FDateTimeValue;
      jdtBool:
        Item.BoolValue := Value.FData.FBoolValue;
      jdtArray:
        Item.ArrayValue := TJsonArray(Value.FData.FObj);
      jdtObject:
        Item.ObjectValue := TJsonObject(Value.FData.FObj);
    else
      Item.Clear;
    end;
  end;
end;

function TJsonDataValueHelper.GetArrayItem(index: Integer): TJsonDataValueHelper;
begin
  Result := ArrayValue.values[index];
end;

function TJsonDataValueHelper.GetArrayCount: Integer;
begin
  Result := ArrayValue.Count;
end;

procedure TJsonDataValueHelper.ResolveName;
begin
  if (FData.FIntern = nil) and (FData.FNameResolver <> nil) then
  begin
    FData.FIntern := FData.FNameResolver.RequireItem(FData.FName);
    FData.FNameResolver := nil;
    FData.FName := '';
  end;
end;

function TJsonDataValueHelper.GetObjectString(const Name: string): string;
begin
  Result := ObjectValue.s[Name];
end;

function TJsonDataValueHelper.GetObjectInt(const Name: string): Integer;
begin
  Result := ObjectValue.i[Name];
end;

function TJsonDataValueHelper.GetObjectLong(const Name: string): Int64;
begin
  Result := ObjectValue.L[Name];
end;

function TJsonDataValueHelper.GetObjectULong(const Name: string): UInt64;
begin
  Result := ObjectValue.u[Name];
end;

function TJsonDataValueHelper.GetObjectFloat(const Name: string): Double;
begin
  Result := ObjectValue.f[Name];
end;

function TJsonDataValueHelper.GetObjectDateTime(const Name: string): TDateTime;
begin
  Result := ObjectValue.d[Name];
end;

function TJsonDataValueHelper.GetObjectBool(const Name: string): Boolean;
begin
  Result := ObjectValue.b[Name];
end;

function TJsonDataValueHelper.GetArray(const Name: string): TJsonArray;
begin
  Result := ObjectValue.a[Name];
end;

function TJsonDataValueHelper.GetObject(const Name: string): TJsonDataValueHelper;
begin
  Result := ObjectValue.values[Name];
end;

function TJsonDataValueHelper.GetObjectVariant(const Name: string): Variant;
begin
  Result := ObjectValue.values[Name];
end;

procedure TJsonDataValueHelper.SetObjectString(const Name, Value: string);
begin
  ObjectValue.s[Name] := Value;
end;

procedure TJsonDataValueHelper.SetObjectInt(const Name: string; const Value: Integer);
begin
  ObjectValue.i[Name] := Value;
end;

procedure TJsonDataValueHelper.SetObjectLong(const Name: string; const Value: Int64);
begin
  ObjectValue.L[Name] := Value;
end;

procedure TJsonDataValueHelper.SetObjectULong(const Name: string; const Value: UInt64);
begin
  ObjectValue.u[Name] := Value;
end;

procedure TJsonDataValueHelper.SetObjectFloat(const Name: string; const Value: Double);
begin
  ObjectValue.f[Name] := Value;
end;

procedure TJsonDataValueHelper.SetObjectDateTime(const Name: string; const Value: TDateTime);
begin
  ObjectValue.d[Name] := Value;
end;

procedure TJsonDataValueHelper.SetObjectBool(const Name: string; const Value: Boolean);
begin
  ObjectValue.b[Name] := Value;
end;

procedure TJsonDataValueHelper.SetArray(const Name: string; const Value: TJsonArray);
begin
  ObjectValue.a[Name] := Value;
end;

procedure TJsonDataValueHelper.SetObject(const Name: string; const Value: TJsonDataValueHelper);
begin
  ObjectValue.values[Name] := Value;
end;

procedure TJsonDataValueHelper.SetObjectVariant(const Name: string; const Value: Variant);
begin
  ObjectValue.values[Name] := Value;
end;

function TJsonDataValueHelper.GetObjectPath(const Name: string): TJsonDataValueHelper;
begin
  Result := ObjectValue.Path[Name];
end;

procedure TJsonDataValueHelper.SetObjectPath(const Name: string; const Value: TJsonDataValueHelper);
begin
  ObjectValue.Path[Name] := Value;
end;

{ TEncodingStrictAccess }

function TEncodingStrictAccess.GetByteCountEx(Chars: PChar; CharCount: Integer): Integer;
begin
  Result := GetByteCount(Chars, CharCount);
end;

function TEncodingStrictAccess.GetBytesEx(Chars: PChar; CharCount: Integer; Bytes: PByte; ByteCount: Integer): Integer;
begin
  Result := GetBytes(Chars, CharCount, Bytes, ByteCount);
end;

function TEncodingStrictAccess.GetCharCountEx(Bytes: PByte; ByteCount: Integer): Integer;
begin
  Result := GetCharCount(Bytes, ByteCount);
end;

function TEncodingStrictAccess.GetCharsEx(Bytes: PByte; ByteCount: Integer; Chars: PChar; CharCount: Integer): Integer;
begin
  Result := GetChars(Bytes, ByteCount, Chars, CharCount);
end;

{ TJsonOutputWriter.TJsonStringBuilder }

procedure TJsonOutputWriter.TJsonStringBuilder.Init;
begin
  FLen := 0;
  FCapacity := 0;
  FData := nil;
end;

procedure TJsonOutputWriter.TJsonStringBuilder.Done;
var
  p: PStrRec;
begin
  if FData <> nil then
  begin
    p := PStrRec(PByte(FData) - SizeOf(TStrRec));
    FreeMem(p);
  end;
end;

procedure TJsonOutputWriter.TJsonStringBuilder.DoneConvertToString(var s: string);
var
  StrP: PStrRec;
  p: PChar;
begin
  s := '';
  if FData <> nil then
  begin
    // Release the unused memory and terminate the string with a #0. The result is that we have a
    // native string that is exactly the same as if it was allocated by System.@NewUnicodeString.
    StrP := PStrRec(PByte(FData) - SizeOf(TStrRec));
    if Len <> FCapacity then
      ReallocMem(Pointer(StrP), SizeOf(TStrRec) + (Len + 1) * SizeOf(Char)); // allocate +1 char for the #0
    // Set the string's length
    StrP.length := Len;
    p := PChar(PByte(StrP) + SizeOf(TStrRec));
    p[Len] := #0;
    Pointer(s) := p; // keep the RefCnt=1
  end;
end;

function TJsonOutputWriter.TJsonStringBuilder.FlushToBytes(var Bytes: PByte; var Size: NativeInt; Encoding: TEncoding): NativeInt;
begin
  if FLen > 0 then
  begin
    // Use the "strict protected" methods that use PChar instead of TCharArray what allows us to
    // use FData directly without converting it to a dynamic TCharArray (and skipping the sanity
    // checks)
    Result := TEncodingStrictAccess(Encoding).GetByteCountEx(FData, FLen);
    if Result > 0 then
    begin
      if Result > Size then
      begin
        Size := (Result + 4095) and not 4095;
        ReallocMem(Bytes, Size);
      end;
      TEncodingStrictAccess(Encoding).GetBytesEx(FData, FLen, Bytes, Result);
    end;
    FLen := 0; // "clear" the buffer but don't release the memory
  end
  else
    Result := 0;
end;

procedure TJsonOutputWriter.TJsonStringBuilder.FlushToMemoryStream(stream: TMemoryStream; Encoding: TEncoding);
var
  L: Integer;
  idx, NewSize: NativeInt;
begin
  if FLen > 0 then
  begin
    // Use the "strict protected" methods that use PChar instead of TCharArray what allows us to
    // use FData directly without converting it to a dynamic TCharArray (and skipping the sanity
    // checks)
    L := TEncodingStrictAccess(Encoding).GetByteCountEx(FData, FLen);
    if L > 0 then
    begin
      // Directly convert into the TMemoryStream.Memory buffer
      idx := stream.Position;
      NewSize := idx + L;
      if NewSize > TMemoryStreamAccess(stream).Capacity then
        TMemoryStreamAccess(stream).Capacity := NewSize;

      TEncodingStrictAccess(Encoding).GetBytesEx(FData, FLen, @PByte(stream.Memory)[idx], L);
      TMemoryStreamAccess(stream).SetPointer(stream.Memory, NewSize);
      stream.Position := NewSize;
    end;
  end;
  FLen := 0; // "clear" the buffer but don't release the memory
end;

procedure TJsonOutputWriter.TJsonStringBuilder.Grow(MinLen: Integer);
var
  c: Integer;
  StrP: PStrRec;
begin
  c := FCapacity;
  c := c * 2;
  if MinLen < 256 then // begin with a 256 char buffer
    MinLen := 256;
  {$IFNDEF CPUX64}
  if c > 256 * 1024 * 1024 then
  begin
    // Memory fragmentation can become a problem, so allocate only the amount of memory that
    // is needed
    c := FCapacity;
    c := c + (c div 3);
    if c < MinLen then
      c := MinLen;
  end
  else
  {$ENDIF ~CPUX64}
  if c < MinLen then
    c := MinLen;
  FCapacity := c;
  if FData <> nil then
  begin
    StrP := Pointer(PByte(FData) - SizeOf(TStrRec));
    ReallocMem(StrP, SizeOf(TStrRec) + (c + 1) * SizeOf(Char)); // allocate +1 char for the #0 that DoneToString() adds
  end
  else
  begin
    // Build the buffer with the StrRec header so it can be easily mapped to a "native string" in
    // DoneToString.
    GetMem(Pointer(StrP), SizeOf(TStrRec) + (c + 1) * SizeOf(Char)); // allocate +1 char for the #0 that DoneToString() adds
    StrP.CodePage := Word(DefaultUnicodeCodePage);
    StrP.ElemSize := SizeOf(Char);
    StrP.RefCnt := 1;
    StrP.length := 0; // DoneToString set the correct value
  end;
  FData := PChar(PByte(StrP) + SizeOf(TStrRec));
end;

function TJsonOutputWriter.TJsonStringBuilder.Append(const s: string): PJsonStringBuilder;
var
  L, LLen: Integer;
begin
  LLen := FLen;
  L := length(s);
  if L > 0 then
  begin
    if LLen + L >= FCapacity then
      Grow(LLen + L);
    case L of
      1: FData[LLen] := PChar(Pointer(s))^;
      2: PLongWord(@FData[LLen])^ := PLongWord(Pointer(s))^;
    else
      Move(PChar(Pointer(s))[0], FData[LLen], L * SizeOf(Char));
    end;
    FLen := LLen + L;
  end;
  Result := @Self;
end;

procedure TJsonOutputWriter.TJsonStringBuilder.Append(p: PChar; Len: Integer);
var
  LLen: Integer;
begin
  LLen := FLen;
  if Len > 0 then
  begin
    if LLen + Len >= FCapacity then
      Grow(LLen + Len);
    case Len of
      1: FData[LLen] := p^;
      2: PLongWord(@FData[LLen])^ := PLongWord(p)^;
    else
      Move(p[0], FData[LLen], Len * SizeOf(Char));
    end;
    FLen := LLen + Len;
  end;
end;

function TJsonOutputWriter.TJsonStringBuilder.Append2(const s1: string; s2: PChar; S2Len: Integer): PJsonStringBuilder;
var
  L, S1Len, LLen: Integer;
begin
  LLen := FLen;
  S1Len := length(s1);
  L := S1Len + S2Len;
  if LLen + L >= FCapacity then
    Grow(LLen + L);

  case S1Len of
    0: ;
    1: FData[LLen] := PChar(Pointer(s1))^;
    2: PLongWord(@FData[LLen])^ := PLongWord(Pointer(s1))^;
  else
    Move(PChar(Pointer(s1))[0], FData[LLen], S1Len * SizeOf(Char));
  end;
  inc(LLen, S1Len);

  case S2Len of
    0: ;
    1: FData[LLen] := s2^;
    2: PLongWord(@FData[LLen])^ := PLongWord(Pointer(s2))^;
  else
    Move(s2[0], FData[LLen], S2Len * SizeOf(Char));
  end;
  FLen := LLen + S2Len;
  Result := @Self;
end;

procedure TJsonOutputWriter.TJsonStringBuilder.Append2(Ch1: Char; Ch2: Char);
var
  LLen: Integer;
begin
  LLen := FLen;
  if LLen + 2 >= FCapacity then
    Grow(2);
  FData[LLen] := Ch1;
  FData[LLen + 1] := Ch2;
  FLen := LLen + 2;
end;


procedure TJsonOutputWriter.TJsonStringBuilder.Append3(Ch1: Char; const s2, s3: string);
var
  L, S2Len, S3Len, LLen: Integer;
begin
  LLen := FLen;
  S2Len := length(s2);
  S3Len := length(s3);
  L := 1 + S2Len + S3Len;
  if LLen + L >= FCapacity then
    Grow(LLen + L);

  FData[LLen] := Ch1;
  inc(LLen);

  case S2Len of
    0: ;
    1: FData[LLen] := PChar(Pointer(s2))^;
    2: PLongWord(@FData[LLen])^ := PLongWord(Pointer(s2))^;
  else
    Move(PChar(Pointer(s2))[0], FData[LLen], S2Len * SizeOf(Char));
  end;
  inc(LLen, S2Len);

  case S3Len of
    1: FData[LLen] := PChar(Pointer(s3))^;
    2: PLongWord(@FData[LLen])^ := PLongWord(Pointer(s3))^;
  else
    Move(PChar(Pointer(s3))[0], FData[LLen], S3Len * SizeOf(Char));
  end;
  FLen := LLen + S3Len;
end;

procedure TJsonOutputWriter.TJsonStringBuilder.Append3(Ch1: Char; const p2: PChar; P2Len: Integer; Ch3: Char);
var
  L, LLen: Integer;
begin
  LLen := FLen;
  L := 2 + P2Len;
  if LLen + L >= FCapacity then
    Grow(LLen + L);

  FData[LLen] := Ch1;
  inc(LLen);

  case P2Len of
    0: ;
    1: FData[LLen] := p2^;
    2: PLongWord(@FData[LLen])^ := PLongWord(p2)^;
  else
    Move(p2[0], FData[LLen], P2Len * SizeOf(Char));
  end;
  inc(LLen, P2Len);

  FData[LLen] := Ch1;
  FLen := LLen + 1;
end;

procedure TJsonOutputWriter.TJsonStringBuilder.Append3(Ch1: Char; const s2: string; Ch3: Char);
begin
  Append3(Ch1, PChar(Pointer(s2)), length(s2), Ch3);
end;

procedure TJsonOutputWriter.TJsonStringBuilder.FlushToStringBuffer(var buffer: TJsonStringBuilder);
begin
  buffer.Append(FData, FLen);
  FLen := 0;
end;

procedure TJsonOutputWriter.TJsonStringBuilder.FlushToString(var s: string);
begin
  SetString(s, FData, FLen);
  FLen := 0;
end;

{ TJsonUTF8StringStream }

{$IFDEF SUPPORTS_UTF8STRING}
constructor TJsonUTF8StringStream.Create;
begin
  inherited Create;
  SetPointer(nil, 0);
end;

function TJsonUTF8StringStream.Realloc(var NewCapacity: longint): Pointer;
var
  L: longint;
begin
  if NewCapacity <> Capacity then
  begin
    if NewCapacity = 0 then
      FDataString := ''
    else
    begin
      L := length(FDataString) * 2;
      {$IFNDEF CPUX64}
      if L > 256 * 1024 * 1024 then
      begin
        // Memory fragmentation can become a problem, so allocate only the amount of memory that
        // is needed
        L := NewCapacity;
      end
      else
      {$ENDIF ~CPUX64}
      if L < NewCapacity then
        L := NewCapacity;
      NewCapacity := L;
      SetLength(FDataString, L);
    end;
  end;
  Result := Pointer(FDataString);
end;
{$ENDIF SUPPORTS_UTF8STRING}

{ TJsonBytesStream }

constructor TJsonBytesStream.Create;
begin
  inherited Create;
  SetPointer(nil, 0);
end;

function TJsonBytesStream.Realloc(var NewCapacity: longint): Pointer;
var
  L: longint;
begin
  if NewCapacity <> Capacity then
  begin
    if NewCapacity = 0 then
      FBytes := nil
    else
    begin
      L := length(FBytes) * 2;
      {$IFNDEF CPUX64}
      if L > 256 * 1024 * 1024 then
      begin
        // Memory fragmentation can become a problem, so allocate only the amount of memory that
        // is needed
        L := NewCapacity;
      end
      else
      {$ENDIF ~CPUX64}
      if L < NewCapacity then
        L := NewCapacity;
      NewCapacity := L;
      SetLength(FBytes, L);
    end;
  end;
  Result := Pointer(FBytes);
end;

initialization
  {$IFDEF USE_NAME_STRING_LITERAL}
  InitializeJsonMemInfo;
  {$ENDIF USE_NAME_STRING_LITERAL}
  {$IFDEF MSWINDOWS}
    {$IFDEF SUPPORT_WINDOWS2000}
  TzSpecificLocalTimeToSystemTime := GetProcAddress(GetModuleHandle(kernel32), PAnsiChar('TzSpecificLocalTimeToSystemTime'));
  if not Assigned(TzSpecificLocalTimeToSystemTime) then
    TzSpecificLocalTimeToSystemTime := TzSpecificLocalTimeToSystemTimeWin2000;
    {$ENDIF SUPPORT_WINDOWS2000}
  {$ENDIF MSWINDOWS}
  // Make sTrue and sFalse a mutable string (RefCount<>-1) so that UStrAsg doesn't always
  // create a new string.
  UniqueString(sTrue);
  UniqueString(sFalse);
  JSONFormatSettings.DecimalSeparator := '.';

end.

