{ ****************************************************************************** }
{ * MixedLibrary,writen by QQ 600585@qq.com                                    * }
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

{
  *
  * Unit Name: MixedLibrary
  * Purpose  : mixed Low Level Function Library
  *
}

(*
  update history
  2017-11-26 fixed UmlMD5Stream and umlMD5 calculate x64 and x86,ARM platform more than 4G memory Support QQ600585
*)

unit UnicodeMixedLib;

{$INCLUDE zDefine.inc}

interface

uses
{$IFDEF FPC}
  Dynlibs,
{$IFDEF MSWINDOWS} Windows, {$ENDIF MSWINDOWS}
{$ELSE FPC}
{$IFDEF MSWINDOWS} Windows, {$ENDIF MSWINDOWS}
  System.IOUtils,
{$ENDIF FPC}
  SysUtils, Types, Math, Variants, CoreClasses, PascalStrings, ListEngine;

const
  C_Max_UInt32 = $FFFFFFFF;
  C_Address_Size = SizeOf(Pointer);
  C_Pointer_Size = C_Address_Size;
  C_Integer_Size = 4;
  C_Int64_Size = 8;
  C_UInt64_Size = 8;
  C_Single_Size = 4;
  C_Double_Size = 8;
  C_Small_Int_Size = 2;
  C_Byte_Size = 1;
  C_Short_Int_Size = 1;
  C_Word_Size = 2;
  C_DWord_Size = 4;
  C_Cardinal_Size = 4;
  C_Boolean_Size = 1;
  C_Bool_Size = 1;
  C_MD5_Size = 16;

  C_PrepareReadCacheSize = 512;
  C_MaxBufferFragmentSize = $F000;

  C_StringError = -911;
  C_SeekError = -910;
  C_FileWriteError = -909;
  C_FileReadError = -908;
  C_FileHandleError = -907;
  C_OpenFileError = -905;
  C_NotOpenFile = -904;
  C_CreateFileError = -903;
  C_FileIsActive = -902;
  C_NotFindFile = -901;
  C_NotError = -900;

type
  U_SystemString = SystemString;
  U_String = TPascalString;
  P_String = PPascalString;
  U_Char = SystemChar;
  U_StringArray = array of U_SystemString;

  U_Bytes = TBytes;

  TSR = TSearchRec;

  U_Stream = TCoreClassStream;

  TReliableFileStream = class(TCoreClassStream)
  protected
    SourceIO, BackupFileIO: TCoreClassFileStream;
    FActivted: Boolean;
    FFileName, FBackupFileName: SystemString;

    procedure InitIO;
    procedure FreeIO;

    procedure SetSize(const NewSize: Int64); overload; override;
    procedure SetSize(NewSize: longint); overload; override;
  public
    constructor Create(const FileName_: SystemString; IsNew_, IsWrite_: Boolean);
    destructor Destroy; override;

    function write(const buffer; Count: longint): longint; override;
    function read(var buffer; Count: longint): longint; override;
    function Seek(const Offset: Int64; origin: TSeekOrigin): Int64; override;

    property FileName: SystemString read FFileName;
    property BackupFileName: SystemString read FBackupFileName;
    property Activted: Boolean read FActivted;
  end;

  PIOHnd = ^TIOHnd;

  TIOHnd = record
  public
    IsOnlyRead: Boolean;
    IsOpen: Boolean;
    AutoFree: Boolean;
    Handle: U_Stream;
    Time: TDateTime;
    Size: Int64;
    Position: Int64;
    FileName: U_String;
    FlushBuff: U_Stream;
    FlushPosition: Int64;
    PrepareReadPosition: Int64;
    PrepareReadBuff: U_Stream;
    IORead, IOWrite: Int64;
    WriteStated: Boolean;
    FixedStringL: Byte;
    Data: Pointer;
    Return: Integer;

    function FixedString2Pascal(s: TBytes): TPascalString;
    procedure Pascal2FixedString(var n: TPascalString; var out_: TBytes);
    function CheckFixedStringLoss(s: TPascalString): Boolean;
  end;

  U_ByteArray = array [0 .. MaxInt div SizeOf(Byte) - 1] of Byte;
  P_ByteArray = ^U_ByteArray;

function umlBytesOf(const s: TPascalString): TBytes;
function umlStringOf(const s: TBytes): TPascalString; overload;

function umlNewString(const s: TPascalString): P_String;
procedure umlFreeString(const p: P_String);

function umlComparePosStr(const s: TPascalString; Offset: Integer; const t: TPascalString): Boolean;
function umlPos(const SubStr, Str: TPascalString; const Offset: Integer = 1): Integer;

function umlVarToStr(const v: Variant; const Base64Conver: Boolean): TPascalString; overload;
function umlVarToStr(const v: Variant): TPascalString; overload;
function umlStrToVar(const s: TPascalString): Variant;

function umlMax(const v1, v2: UInt64): UInt64; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function umlMax(const v1, v2: Cardinal): Cardinal; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function umlMax(const v1, v2: Word): Word; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function umlMax(const v1, v2: Byte): Byte; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function umlMax(const v1, v2: Int64): Int64; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function umlMax(const v1, v2: Integer): Integer; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function umlMax(const v1, v2: SmallInt): SmallInt; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function umlMax(const v1, v2: ShortInt): ShortInt; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function umlMax(const v1, v2: Double): Double; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function umlMax(const v1, v2: Single): Single; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;

function umlMin(const v1, v2: UInt64): UInt64; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function umlMin(const v1, v2: Cardinal): Cardinal; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function umlMin(const v1, v2: Word): Word; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function umlMin(const v1, v2: Byte): Byte; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function umlMin(const v1, v2: Int64): Int64; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function umlMin(const v1, v2: Integer): Integer; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function umlMin(const v1, v2: SmallInt): SmallInt; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function umlMin(const v1, v2: ShortInt): ShortInt; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function umlMin(const v1, v2: Double): Double; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function umlMin(const v1, v2: Single): Single; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;

function umlClamp(const v, min_, max_: UInt64): UInt64; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function umlClamp(const v, min_, max_: Cardinal): Cardinal; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function umlClamp(const v, min_, max_: Word): Word; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function umlClamp(const v, min_, max_: Byte): Byte; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function umlClamp(const v, min_, max_: Int64): Int64; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function umlClamp(const v, min_, max_: SmallInt): SmallInt; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function umlClamp(const v, min_, max_: ShortInt): ShortInt; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function umlClamp(const v, min_, max_: Double): Double; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function umlClamp(const v, min_, max_: Single): Single; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;

function umlInRange(const v, min_, max_: UInt64): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function umlInRange(const v, min_, max_: Cardinal): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function umlInRange(const v, min_, max_: Word): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function umlInRange(const v, min_, max_: Byte): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function umlInRange(const v, min_, max_: SmallInt): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function umlInRange(const v, min_, max_: ShortInt): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function umlInRange(const v, min_, max_: Double): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function umlInRange(const v, min_, max_: Single): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;

function umlGetResourceStream(const FileName: TPascalString): TCoreClassStream;

function umlSameVarValue(const v1, v2: Variant): Boolean;
function umlSameVariant(const v1, v2: Variant): Boolean;

function umlRandom(const rnd: TMT19937Random): Integer; overload;
function umlRandom: Integer; overload;

function umlRandomRange(const rnd: TMT19937Random; const min_, max_: Integer): Integer; overload;
function umlRandomRangeS(const rnd: TMT19937Random; const min_, max_: Single): Single; overload;
function umlRandomRangeD(const rnd: TMT19937Random; const min_, max_: Double): Double; overload;
function umlRandomRangeF(const rnd: TMT19937Random; const min_, max_: Double): Double; overload;

function umlRandomRange(const min_, max_: Integer): Integer; overload;
function umlRandomRangeS(const min_, max_: Single): Single; overload;
function umlRandomRangeD(const min_, max_: Double): Double; overload;
function umlRandomRangeF(const min_, max_: Double): Double; overload;

function umlDefaultTime: Double;
function umlNow: Double;
function umlDefaultAttrib: Integer;
function umlBoolToStr(const Value: Boolean): TPascalString;
function umlStrToBool(const Value: TPascalString): Boolean;

function umlFileExists(const FileName: TPascalString): Boolean;
function umlDirectoryExists(const DirectoryName: TPascalString): Boolean;
function umlCreateDirectory(const DirectoryName: TPascalString): Boolean;
function umlCurrentDirectory: TPascalString;
function umlCurrentPath: TPascalString;
function umlGetCurrentPath: TPascalString;
procedure umlSetCurrentPath(ph: TPascalString);

function umlFindFirstFile(const FileName: TPascalString; var SR: TSR): Boolean;
function umlFindNextFile(var SR: TSR): Boolean;
function umlFindFirstDir(const DirName: TPascalString; var SR: TSR): Boolean;
function umlFindNextDir(var SR: TSR): Boolean;
procedure umlFindClose(var SR: TSR);

function umlGetFileList(const FullPath: TPascalString; AsLst: TCoreClassStrings): Integer; overload;
function umlGetDirList(const FullPath: TPascalString; AsLst: TCoreClassStrings): Integer; overload;

function umlGetFileList(const FullPath: TPascalString; AsLst: TPascalStringList): Integer; overload;
function umlGetDirList(const FullPath: TPascalString; AsLst: TPascalStringList): Integer; overload;

function umlGetFileListWithFullPath(const FullPath: TPascalString): U_StringArray;
function umlGetDirListWithFullPath(const FullPath: TPascalString): U_StringArray;
function umlGetFileListPath(const FullPath: TPascalString): U_StringArray;
function umlGetDirListPath(const FullPath: TPascalString): U_StringArray;

function umlCombinePath(const s1, s2: TPascalString): TPascalString;
function umlCombineFileName(const pathName, FileName: TPascalString): TPascalString;
function umlCombineUnixPath(const s1, s2: TPascalString): TPascalString;
function umlCombineUnixFileName(const pathName, FileName: TPascalString): TPascalString;
function umlCombineWinPath(const s1, s2: TPascalString): TPascalString;
function umlCombineWinFileName(const pathName, FileName: TPascalString): TPascalString;
function umlGetFileName(const s: TPascalString): TPascalString;
function umlGetFilePath(const s: TPascalString): TPascalString;
function umlChangeFileExt(const s, ext: TPascalString): TPascalString;
function umlGetFileExt(const s: TPascalString): TPascalString;

procedure InitIOHnd(var IOHnd: TIOHnd);
function umlFileCreateAsStream(const FileName: TPascalString; stream: U_Stream; var IOHnd: TIOHnd): Boolean; overload;
function umlFileCreateAsStream(stream: U_Stream; var IOHnd: TIOHnd): Boolean; overload;
function umlFileOpenAsStream(const FileName: TPascalString; stream: U_Stream; var IOHnd: TIOHnd; OnlyRead_: Boolean): Boolean;
function umlFileCreateAsMemory(var IOHnd: TIOHnd): Boolean;
function umlFileCreate(const FileName: TPascalString; var IOHnd: TIOHnd): Boolean;
function umlFileOpen(const FileName: TPascalString; var IOHnd: TIOHnd; OnlyRead_: Boolean): Boolean;
function umlFileClose(var IOHnd: TIOHnd): Boolean;
function umlFileUpdate(var IOHnd: TIOHnd): Boolean;
function umlFileTest(var IOHnd: TIOHnd): Boolean;

procedure umlResetPrepareRead(var IOHnd: TIOHnd);
function umlFilePrepareRead(var IOHnd: TIOHnd; Size: Int64; var buff): Boolean;
function umlFileRead(var IOHnd: TIOHnd; const Size: Int64; var buff): Boolean;
function umlBlockRead(var IOHnd: TIOHnd; var buff; Size: Int64): Boolean;

function umlFilePrepareWrite(var IOHnd: TIOHnd): Boolean;
function umlFileFlushWrite(var IOHnd: TIOHnd): Boolean;
function umlFileWrite(var IOHnd: TIOHnd; const Size: Int64; var buff): Boolean;
function umlBlockWrite(var IOHnd: TIOHnd; var buff; const Size: Int64): Boolean;

function umlFileWriteFixedString(var IOHnd: TIOHnd; var Value: TPascalString): Boolean;
function umlFileReadFixedString(var IOHnd: TIOHnd; var Value: TPascalString): Boolean;

function umlFileSeek(var IOHnd: TIOHnd; APos: Int64): Boolean;
function umlFileGetPOS(var IOHnd: TIOHnd): Int64;
function umlFilePOS(var IOHnd: TIOHnd): Int64;
function umlFileGetSize(var IOHnd: TIOHnd): Int64;
function umlFileSize(var IOHnd: TIOHnd): Int64;

function umlGetFileTime(const FileName: TPascalString): TDateTime;
procedure umlSetFileTime(const FileName: TPascalString; newTime: TDateTime);

function umlGetFileSize(const FileName: TPascalString): Int64;

function umlGetFileCount(const FileName: TPascalString): Integer;
function umlGetFileDateTime(const FileName: TPascalString): TDateTime;
function umlDeleteFile(const FileName: TPascalString; const _VerifyCheck: Boolean): Boolean; overload;
function umlDeleteFile(const FileName: TPascalString): Boolean; overload;
function umlCopyFile(const SourFile, DestFile: TPascalString): Boolean;
function umlRenameFile(const OldName, NewName: TPascalString): Boolean;

procedure umlSetLength(var sVal: TPascalString; Len: Integer); overload;
procedure umlSetLength(var sVal: U_Bytes; Len: Integer); overload;
procedure umlSetLength(var sVal: TArrayPascalString; Len: Integer); overload;

function umlGetLength(const sVal: TPascalString): Integer; overload;
function umlGetLength(const sVal: U_Bytes): Integer; overload;
function umlGetLength(const sVal: TArrayPascalString): Integer; overload;

function umlUpperCase(const Str: TPascalString): TPascalString;
function umlLowerCase(const Str: TPascalString): TPascalString;
function umlCopyStr(const sVal: TPascalString; MainPosition, LastPosition: Integer): TPascalString;
function umlSameText(const s1, s2: TPascalString): Boolean;

function umlDeleteChar(const SText, Ch: TPascalString): TPascalString; overload;
function umlDeleteChar(const SText: TPascalString; const SomeChars: array of SystemChar): TPascalString; overload;
function umlDeleteChar(const SText: TPascalString; const SomeCharsets: TOrdChars): TPascalString; overload;
function umlGetNumberCharInText(const n: TPascalString): TPascalString;

function umlMatchChar(CharValue: U_Char; cVal: P_String): Boolean; overload;
function umlMatchChar(CharValue: U_Char; cVal: TPascalString): Boolean; overload;
function umlExistsChar(StrValue: TPascalString; cVal: TPascalString): Boolean;

function umlTrimChar(const s, trim_s: TPascalString): TPascalString;

function umlGetFirstStr(const sVal, trim_s: TPascalString): TPascalString;
function umlGetLastStr(const sVal, trim_s: TPascalString): TPascalString;
function umlDeleteFirstStr(const sVal, trim_s: TPascalString): TPascalString;
function umlDeleteLastStr(const sVal, trim_s: TPascalString): TPascalString;
function umlGetIndexStrCount(const sVal, trim_s: TPascalString): Integer;
function umlGetIndexStr(const sVal: TPascalString; trim_s: TPascalString; index: Integer): TPascalString;

procedure umlGetSplitArray(const sour: TPascalString; var dest: TArrayPascalString; const splitC: TPascalString); overload;
procedure umlGetSplitArray(const sour: TPascalString; var dest: U_StringArray; const splitC: TPascalString); overload;
function ArrayStringToText(var ary: TArrayPascalString; const splitC: TPascalString): TPascalString;
function umlStringsToText(lst: TCoreClassStrings; const splitC: TPascalString): TPascalString; overload;
function umlStringsToText(lst: TListPascalString; const splitC: TPascalString): TPascalString; overload;

function umlGetFirstStr_Discontinuity(const sVal, trim_s: TPascalString): TPascalString;
function umlDeleteFirstStr_Discontinuity(const sVal, trim_s: TPascalString): TPascalString;
function umlGetLastStr_Discontinuity(const sVal, trim_s: TPascalString): TPascalString;
function umlDeleteLastStr_Discontinuity(const sVal, trim_s: TPascalString): TPascalString;
function umlGetIndexStrCount_Discontinuity(const sVal, trim_s: TPascalString): Integer;
function umlGetIndexStr_Discontinuity(const sVal: TPascalString; trim_s: TPascalString; index: Integer): TPascalString;

function umlGetFirstTextPos(const s: TPascalString; const TextArry: TArrayPascalString; var OutText: TPascalString): Integer;
function umlDeleteText(const sour: TPascalString; const bToken, eToken: TArrayPascalString; ANeedBegin, ANeedEnd: Boolean): TPascalString;
function umlGetTextContent(const sour: TPascalString; const bToken, eToken: TArrayPascalString): TPascalString;

type
  TTextType = (ntBool, ntInt, ntInt64, ntUInt64, ntWord, ntByte, ntSmallInt, ntShortInt, ntUInt, ntSingle, ntDouble, ntCurrency, ntUnknow);
function umlGetNumTextType(const s: TPascalString): TTextType;

function umlIsHex(const sVal: TPascalString): Boolean;
function umlIsNumber(const sVal: TPascalString): Boolean;
function umlIsIntNumber(const sVal: TPascalString): Boolean;
function umlIsFloatNumber(const sVal: TPascalString): Boolean;
function umlIsBool(const sVal: TPascalString): Boolean;
function umlNumberCount(const sVal: TPascalString): Integer;

function umlPercentageToFloat(OriginMax, OriginMin, ProcressParameter: Double): Double;
function umlPercentageToInt(OriginParameter, ProcressParameter: Integer): Integer;
function umlPercentageToStr(OriginParameter, ProcressParameter: Integer): TPascalString;
function umlSmartSizeToStr(Size: Int64): TPascalString;

function umlIntToStr(Parameter: Single): TPascalString; overload;
function umlIntToStr(Parameter: Double): TPascalString; overload;
function umlIntToStr(Parameter: Int64): TPascalString; overload;

function umlPointerToStr(param: Pointer): TPascalString;

function umlSizeToStr(Parameter: Int64): TPascalString;
function umlDateTimeToStr(t: TDateTime): TPascalString;
function umlTimeTickToStr(const t: TTimeTick): TPascalString;
function umlTimeToStr(t: TDateTime): TPascalString;
function umlDateToStr(t: TDateTime): TPascalString;
function umlFloatToStr(const f: Extended): TPascalString;
function umlShortFloatToStr(const f: Extended): TPascalString;

function umlStrToInt(const _V: TPascalString): Integer; overload;
function umlStrToInt(const _V: TPascalString; _Def: Integer): Integer; overload;
function umlStrToInt64(const _V: TPascalString; _Def: Int64): Int64; overload;
function umlStrToFloat(const _V: TPascalString; _Def: Double): Double; overload;
function umlStrToFloat(const _V: TPascalString): Double; overload;

function umlMultipleMatch(IgnoreCase: Boolean; const SourceStr, TargetStr, umlMultipleString, umlMultipleCharacter: TPascalString): Boolean; overload;
function umlMultipleMatch(IgnoreCase: Boolean; const SourceStr, TargetStr: TPascalString): Boolean; overload;
function umlMultipleMatch(const SourceStr, TargetStr: TPascalString): Boolean; overload;
function umlMultipleMatch(const ValueCheck: array of TPascalString; const Value: TPascalString): Boolean; overload;
function umlSearchMatch(const SourceStr, TargetStr: TPascalString): Boolean; overload;
function umlSearchMatch(const ValueCheck: TArrayPascalString; Value: TPascalString): Boolean; overload;

// <prefix>.<postfix> formula, match sour -> dest
function umlMatchFileInfo(const exp_, sour_, dest_: TPascalString): Boolean;

function umlDecodeTimeToStr(NowDateTime: TDateTime): TPascalString;
function umlMakeRanName: TPascalString;

function umlStringReplace(const s, OldPattern, NewPattern: TPascalString; IgnoreCase: Boolean): TPascalString;
function umlReplaceString(const s, OldPattern, NewPattern: TPascalString; IgnoreCase: Boolean): TPascalString;
function umlCharReplace(const s: TPascalString; OldPattern, NewPattern: U_Char): TPascalString;
function umlReplaceChar(const s: TPascalString; OldPattern, NewPattern: U_Char): TPascalString;

function umlEncodeText2HTML(const psSrc: TPascalString): TPascalString;

function umlURLEncode(const Data: TPascalString): TPascalString;
function umlURLDecode(const Data: TPascalString; FormEncoded: Boolean): TPascalString;

type
  TBase64Context = record
    Tail: array [0 .. 3] of Byte;
    TailBytes: Integer;
    LineWritten: Integer;
    LineSize: Integer;
    TrailingEol: Boolean;
    PutFirstEol: Boolean;
    LiberalMode: Boolean;
    fEOL: array [0 .. 3] of Byte;
    EOLSize: Integer;
    OutBuf: array [0 .. 3] of Byte;
    EQUCount: Integer;
    UseUrlAlphabet: Boolean;
  end;

  TBase64EOLMarker = (emCRLF, emCR, emLF, emNone);

  TBase64ByteArray = array [0 .. MaxInt div SizeOf(Byte) - 1] of Byte;
  PBase64ByteArray = ^TBase64ByteArray;

const
  BASE64_DECODE_OK = 0;
  BASE64_DECODE_INVALID_CHARACTER = 1;
  BASE64_DECODE_WRONG_DATA_SIZE = 2;
  BASE64_DECODE_NOT_ENOUGH_SPACE = 3;

  Base64Symbols: array [0 .. 63] of Byte = (
    $41, $42, $43, $44, $45, $46, $47, $48, $49, $4A, $4B, $4C, $4D, $4E, $4F, $50,
    $51, $52, $53, $54, $55, $56, $57, $58, $59, $5A, $61, $62, $63, $64, $65, $66,
    $67, $68, $69, $6A, $6B, $6C, $6D, $6E, $6F, $70, $71, $72, $73, $74, $75, $76,
    $77, $78, $79, $7A, $30, $31, $32, $33, $34, $35, $36, $37, $38, $39, $2B, $2F);

  Base64Values: array [0 .. 255] of Byte = (
    $FE, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FE, $FE, $FF, $FF, $FE, $FF, $FF,
    $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $FE, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $3E, $FF, $FF, $FF, $3F,
    $34, $35, $36, $37, $38, $39, $3A, $3B, $3C, $3D, $FF, $FF, $FF, $FD, $FF, $FF,
    $FF, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E,
    $0F, $10, $11, $12, $13, $14, $15, $16, $17, $18, $19, $FF, $FF, $FF, $FF, $FF,
    $FF, $1A, $1B, $1C, $1D, $1E, $1F, $20, $21, $22, $23, $24, $25, $26, $27, $28,
    $29, $2A, $2B, $2C, $2D, $2E, $2F, $30, $31, $32, $33, $FF, $FF, $FF, $FF, $FF,
    $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);

function B64EstimateEncodedSize(Ctx: TBase64Context; InSize: Integer): Integer;
function B64InitializeDecoding(var Ctx: TBase64Context; LiberalMode: Boolean): Boolean;
function B64InitializeEncoding(var Ctx: TBase64Context; LineSize: Integer; fEOL: TBase64EOLMarker; TrailingEol: Boolean): Boolean;
function B64Encode(var Ctx: TBase64Context; buffer: PByte; Size: Integer; OutBuffer: PByte; var OutSize: Integer): Boolean;
function B64Decode(var Ctx: TBase64Context; buffer: PByte; Size: Integer; OutBuffer: PByte; var OutSize: Integer): Boolean;
function B64FinalizeEncoding(var Ctx: TBase64Context; OutBuffer: PByte; var OutSize: Integer): Boolean;
function B64FinalizeDecoding(var Ctx: TBase64Context; OutBuffer: PByte; var OutSize: Integer): Boolean;
function umlBase64Encode(InBuffer: PByte; InSize: Integer; OutBuffer: PByte; var OutSize: Integer; WrapLines: Boolean): Boolean;
function umlBase64Decode(InBuffer: PByte; InSize: Integer; OutBuffer: PByte; var OutSize: Integer; LiberalMode: Boolean): Integer;
procedure umlBase64EncodeBytes(var sour, dest: TBytes); overload;
procedure umlBase64DecodeBytes(var sour, dest: TBytes); overload;
procedure umlBase64EncodeBytes(var sour: TBytes; var dest: TPascalString); overload;
procedure umlBase64DecodeBytes(const sour: TPascalString; var dest: TBytes); overload;
procedure umlDecodeLineBASE64(const buffer: TPascalString; var output: TPascalString);
procedure umlEncodeLineBASE64(const buffer: TPascalString; var output: TPascalString);
procedure umlDecodeStreamBASE64(const buffer: TPascalString; output: TCoreClassStream);
procedure umlEncodeStreamBASE64(buffer: TCoreClassStream; var output: TPascalString);
function umlDivisionBase64Text(const buffer: TPascalString; width: Integer; DivisionAsPascalString: Boolean): TPascalString;
function umlTestBase64(const text: TPascalString): Boolean;

type
  PMD5 = ^TMD5;
  TMD5 = array [0 .. 15] of Byte;

const
  NullMD5: TMD5 = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  ZeroMD5: TMD5 = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);

function umlMD5(const buffPtr: PByte; bufSiz: nativeUInt): TMD5;
function umlMD5Char(const buffPtr: PByte; const BuffSize: nativeUInt): TPascalString;
function umlMD5String(const buffPtr: PByte; const BuffSize: nativeUInt): TPascalString;
function umlStreamMD5(stream: TCoreClassStream; StartPos, EndPos: Int64): TMD5; overload;
function umlStreamMD5(stream: TCoreClassStream): TMD5; overload;
function umlStreamMD5Char(stream: TCoreClassStream): TPascalString; overload;
function umlStreamMD5String(stream: TCoreClassStream): TPascalString; overload;
function umlStringMD5(const Value: TPascalString): TPascalString;
function umlFileMD5(FileName: TPascalString): TMD5; overload;
function umlCombineMD5(const m1: TMD5): TMD5; overload;
function umlCombineMD5(const m1, m2: TMD5): TMD5; overload;
function umlCombineMD5(const m1, m2, m3: TMD5): TMD5; overload;
function umlCombineMD5(const buff: array of TMD5): TMD5; overload;
function umlMD5ToStr(md5: TMD5): TPascalString; overload;
function umlMD5ToString(md5: TMD5): TPascalString; overload;
function umlMD52String(md5: TMD5): TPascalString; overload;
function umlMD5Compare(const m1, m2: TMD5): Boolean;
function umlCompareMD5(const m1, m2: TMD5): Boolean;
function umlIsNullMD5(m: TMD5): Boolean;
function umlWasNullMD5(m: TMD5): Boolean;

{$REGION 'crc16define'}


const
  CRC16Table: array [0 .. 255] of Word = (
    $0000, $C0C1, $C181, $0140, $C301, $03C0, $0280, $C241, $C601, $06C0, $0780,
    $C741, $0500, $C5C1, $C481, $0440, $CC01, $0CC0, $0D80, $CD41, $0F00, $CFC1,
    $CE81, $0E40, $0A00, $CAC1, $CB81, $0B40, $C901, $09C0, $0880, $C841, $D801,
    $18C0, $1980, $D941, $1B00, $DBC1, $DA81, $1A40, $1E00, $DEC1, $DF81, $1F40,
    $DD01, $1DC0, $1C80, $DC41, $1400, $D4C1, $D581, $1540, $D701, $17C0, $1680,
    $D641, $D201, $12C0, $1380, $D341, $1100, $D1C1, $D081, $1040, $F001, $30C0,
    $3180, $F141, $3300, $F3C1, $F281, $3240, $3600, $F6C1, $F781, $3740, $F501,
    $35C0, $3480, $F441, $3C00, $FCC1, $FD81, $3D40, $FF01, $3FC0, $3E80, $FE41,
    $FA01, $3AC0, $3B80, $FB41, $3900, $F9C1, $F881, $3840, $2800, $E8C1, $E981,
    $2940, $EB01, $2BC0, $2A80, $EA41, $EE01, $2EC0, $2F80, $EF41, $2D00, $EDC1,
    $EC81, $2C40, $E401, $24C0, $2580, $E541, $2700, $E7C1, $E681, $2640, $2200,
    $E2C1, $E381, $2340, $E101, $21C0, $2080, $E041, $A001, $60C0, $6180, $A141,
    $6300, $A3C1, $A281, $6240, $6600, $A6C1, $A781, $6740, $A501, $65C0, $6480,
    $A441, $6C00, $ACC1, $AD81, $6D40, $AF01, $6FC0, $6E80, $AE41, $AA01, $6AC0,
    $6B80, $AB41, $6900, $A9C1, $A881, $6840, $7800, $B8C1, $B981, $7940, $BB01,
    $7BC0, $7A80, $BA41, $BE01, $7EC0, $7F80, $BF41, $7D00, $BDC1, $BC81, $7C40,
    $B401, $74C0, $7580, $B541, $7700, $B7C1, $B681, $7640, $7200, $B2C1, $B381,
    $7340, $B101, $71C0, $7080, $B041, $5000, $90C1, $9181, $5140, $9301, $53C0,
    $5280, $9241, $9601, $56C0, $5780, $9741, $5500, $95C1, $9481, $5440, $9C01,
    $5CC0, $5D80, $9D41, $5F00, $9FC1, $9E81, $5E40, $5A00, $9AC1, $9B81, $5B40,
    $9901, $59C0, $5880, $9841, $8801, $48C0, $4980, $8941, $4B00, $8BC1, $8A81,
    $4A40, $4E00, $8EC1, $8F81, $4F40, $8D01, $4DC0, $4C80, $8C41, $4400, $84C1,
    $8581, $4540, $8701, $47C0, $4680, $8641, $8201, $42C0, $4380, $8341, $4100,
    $81C1, $8081, $4040
    );
{$ENDREGION 'crc16define'}

function umlCRC16(const Value: PByte; const Count: nativeUInt): Word;
function umlStringCRC16(const Value: TPascalString): Word;
function umlStreamCRC16(stream: U_Stream; StartPos, EndPos: Int64): Word; overload;
function umlStreamCRC16(stream: U_Stream): Word; overload;

{$REGION 'crc32define'}


const
  CRC32Table: array [0 .. 255] of Cardinal = (
    $00000000, $77073096, $EE0E612C, $990951BA, $076DC419, $706AF48F, $E963A535,
    $9E6495A3, $0EDB8832, $79DCB8A4, $E0D5E91E, $97D2D988, $09B64C2B, $7EB17CBD,
    $E7B82D07, $90BF1D91, $1DB71064, $6AB020F2, $F3B97148, $84BE41DE, $1ADAD47D,
    $6DDDE4EB, $F4D4B551, $83D385C7, $136C9856, $646BA8C0, $FD62F97A, $8A65C9EC,
    $14015C4F, $63066CD9, $FA0F3D63, $8D080DF5, $3B6E20C8, $4C69105E, $D56041E4,
    $A2677172, $3C03E4D1, $4B04D447, $D20D85FD, $A50AB56B, $35B5A8FA, $42B2986C,
    $DBBBC9D6, $ACBCF940, $32D86CE3, $45DF5C75, $DCD60DCF, $ABD13D59, $26D930AC,
    $51DE003A, $C8D75180, $BFD06116, $21B4F4B5, $56B3C423, $CFBA9599, $B8BDA50F,
    $2802B89E, $5F058808, $C60CD9B2, $B10BE924, $2F6F7C87, $58684C11, $C1611DAB,
    $B6662D3D, $76DC4190, $01DB7106, $98D220BC, $EFD5102A, $71B18589, $06B6B51F,
    $9FBFE4A5, $E8B8D433, $7807C9A2, $0F00F934, $9609A88E, $E10E9818, $7F6A0DBB,
    $086D3D2D, $91646C97, $E6635C01, $6B6B51F4, $1C6C6162, $856530D8, $F262004E,
    $6C0695ED, $1B01A57B, $8208F4C1, $F50FC457, $65B0D9C6, $12B7E950, $8BBEB8EA,
    $FCB9887C, $62DD1DDF, $15DA2D49, $8CD37CF3, $FBD44C65, $4DB26158, $3AB551CE,
    $A3BC0074, $D4BB30E2, $4ADFA541, $3DD895D7, $A4D1C46D, $D3D6F4FB, $4369E96A,
    $346ED9FC, $AD678846, $DA60B8D0, $44042D73, $33031DE5, $AA0A4C5F, $DD0D7CC9,
    $5005713C, $270241AA, $BE0B1010, $C90C2086, $5768B525, $206F85B3, $B966D409,
    $CE61E49F, $5EDEF90E, $29D9C998, $B0D09822, $C7D7A8B4, $59B33D17, $2EB40D81,
    $B7BD5C3B, $C0BA6CAD, $EDB88320, $9ABFB3B6, $03B6E20C, $74B1D29A, $EAD54739,
    $9DD277AF, $04DB2615, $73DC1683, $E3630B12, $94643B84, $0D6D6A3E, $7A6A5AA8,
    $E40ECF0B, $9309FF9D, $0A00AE27, $7D079EB1, $F00F9344, $8708A3D2, $1E01F268,
    $6906C2FE, $F762575D, $806567CB, $196C3671, $6E6B06E7, $FED41B76, $89D32BE0,
    $10DA7A5A, $67DD4ACC, $F9B9DF6F, $8EBEEFF9, $17B7BE43, $60B08ED5, $D6D6A3E8,
    $A1D1937E, $38D8C2C4, $4FDFF252, $D1BB67F1, $A6BC5767, $3FB506DD, $48B2364B,
    $D80D2BDA, $AF0A1B4C, $36034AF6, $41047A60, $DF60EFC3, $A867DF55, $316E8EEF,
    $4669BE79, $CB61B38C, $BC66831A, $256FD2A0, $5268E236, $CC0C7795, $BB0B4703,
    $220216B9, $5505262F, $C5BA3BBE, $B2BD0B28, $2BB45A92, $5CB36A04, $C2D7FFA7,
    $B5D0CF31, $2CD99E8B, $5BDEAE1D, $9B64C2B0, $EC63F226, $756AA39C, $026D930A,
    $9C0906A9, $EB0E363F, $72076785, $05005713, $95BF4A82, $E2B87A14, $7BB12BAE,
    $0CB61B38, $92D28E9B, $E5D5BE0D, $7CDCEFB7, $0BDBDF21, $86D3D2D4, $F1D4E242,
    $68DDB3F8, $1FDA836E, $81BE16CD, $F6B9265B, $6FB077E1, $18B74777, $88085AE6,
    $FF0F6A70, $66063BCA, $11010B5C, $8F659EFF, $F862AE69, $616BFFD3, $166CCF45,
    $A00AE278, $D70DD2EE, $4E048354, $3903B3C2, $A7672661, $D06016F7, $4969474D,
    $3E6E77DB, $AED16A4A, $D9D65ADC, $40DF0B66, $37D83BF0, $A9BCAE53, $DEBB9EC5,
    $47B2CF7F, $30B5FFE9, $BDBDF21C, $CABAC28A, $53B39330, $24B4A3A6, $BAD03605,
    $CDD70693, $54DE5729, $23D967BF, $B3667A2E, $C4614AB8, $5D681B02, $2A6F2B94,
    $B40BBE37, $C30C8EA1, $5A05DF1B, $2D02EF8D
    );
{$ENDREGION 'crc32define'}

function umlCRC32(const Value: PByte; const Count: nativeUInt): Cardinal;
function umlString2CRC32(const Value: TPascalString): Cardinal;
function umlStreamCRC32(stream: U_Stream; StartPos, EndPos: Int64): Cardinal; overload;
function umlStreamCRC32(stream: U_Stream): Cardinal; overload;

function umlTrimSpace(const s: TPascalString): TPascalString;

function umlSeparatorText(AText: TPascalString; dest: TCoreClassStrings; SeparatorChar: TPascalString): Integer; overload;
function umlSeparatorText(AText: TPascalString; dest: THashVariantList; SeparatorChar: TPascalString): Integer; overload;
function umlSeparatorText(AText: TPascalString; dest: TListPascalString; SeparatorChar: TPascalString): Integer; overload;

function umlStringsMatchText(OriginValue: TCoreClassStrings; DestValue: TPascalString; IgnoreCase: Boolean): Boolean;

function umlStringsInExists(dest: TListPascalString; SText: TPascalString; IgnoreCase: Boolean): Boolean; overload;
function umlStringsInExists(dest: TCoreClassStrings; SText: TPascalString; IgnoreCase: Boolean): Boolean; overload;
function umlStringsInExists(dest: TCoreClassStrings; SText: TPascalString): Boolean; overload;

function umlTextInStrings(const SText: TPascalString; dest: TListPascalString; IgnoreCase: Boolean): Boolean; overload;
function umlTextInStrings(const SText: TPascalString; dest: TCoreClassStrings; IgnoreCase: Boolean): Boolean; overload;
function umlTextInStrings(const SText: TPascalString; dest: TCoreClassStrings): Boolean; overload;

function umlAddNewStrTo(SourceStr: TPascalString; dest: TListPascalString; IgnoreCase: Boolean): Boolean; overload;
function umlAddNewStrTo(SourceStr: TPascalString; dest: TCoreClassStrings; IgnoreCase: Boolean): Boolean; overload;
function umlAddNewStrTo(SourceStr: TPascalString; dest: TCoreClassStrings): Boolean; overload;
function umlAddNewStrTo(SourceStr, dest: TCoreClassStrings): Integer; overload;
function umlDeleteStrings(const SText: TPascalString; dest: TCoreClassStrings; IgnoreCase: Boolean): Integer;
function umlDeleteStringsNot(const SText: TPascalString; dest: TCoreClassStrings; IgnoreCase: Boolean): Integer;
function umlMergeStrings(Source, dest: TCoreClassStrings; IgnoreCase: Boolean): Integer; overload;
function umlMergeStrings(Source, dest: TListPascalString; IgnoreCase: Boolean): Integer; overload;

function umlConverStrToFileName(const Value: TPascalString): TPascalString;

function umlSplitTextMatch(const SText, Limit, MatchText: TPascalString; IgnoreCase: Boolean): Boolean;
function umlSplitTextTrimSpaceMatch(const SText, Limit, MatchText: TPascalString; IgnoreCase: Boolean): Boolean;
function umlSplitDeleteText(const SText, Limit, MatchText: TPascalString; IgnoreCase: Boolean): TPascalString;
function umlSplitTextAsList(const SText, Limit: TPascalString; AsLst: TCoreClassStrings): Boolean;
function umlSplitTextAsListAndTrimSpace(const SText, Limit: TPascalString; AsLst: TCoreClassStrings): Boolean;
function umlListAsSplitText(const List: TCoreClassStrings; Limit: TPascalString): TPascalString; overload;
function umlListAsSplitText(const List: TListPascalString; Limit: TPascalString): TPascalString; overload;
function umlDivisionText(const buffer: TPascalString; width: Integer; DivisionAsPascalString: Boolean): TPascalString;

function umlUpdateComponentName(const Name: TPascalString): TPascalString;
function umlMakeComponentName(Owner: TCoreClassComponent; RefrenceName: TPascalString): TPascalString;

procedure umlReadComponent(stream: TCoreClassStream; comp: TCoreClassComponent);
procedure umlWriteComponent(stream: TCoreClassStream; comp: TCoreClassComponent);
procedure umlCopyComponentDataTo(comp, copyto: TCoreClassComponent);

function umlProcessCycleValue(CurrentVal, DeltaVal, StartVal, OverVal: Single; var EndFlag: Boolean): Single;

type
  TCSVGetLineCall = procedure(var L: TPascalString; var IsEnd: Boolean);
  TCSVSaveCall = procedure(const sour: TPascalString; const king, Data: TArrayPascalString);
  TCSVGetLineMethod = procedure(var L: TPascalString; var IsEnd: Boolean) of object;
  TCSVSaveMethod = procedure(const sour: TPascalString; const king, Data: TArrayPascalString) of object;
{$IFDEF FPC}
  TCSVGetLineProc = procedure(var L: TPascalString; var IsEnd: Boolean) is nested;
  TCSVSaveProc = procedure(const sour: TPascalString; const king, Data: TArrayPascalString) is nested;
{$ELSE FPC}
  TCSVGetLineProc = reference to procedure(var L: TPascalString; var IsEnd: Boolean);
  TCSVSaveProc = reference to procedure(const sour: TPascalString; const king, Data: TArrayPascalString);
{$ENDIF FPC}

procedure ImportCSV_C(const sour: TArrayPascalString; OnNotify: TCSVSaveCall);
procedure CustomImportCSV_C(const OnGetLine: TCSVGetLineCall; OnNotify: TCSVSaveCall);
procedure ImportCSV_M(const sour: TArrayPascalString; OnNotify: TCSVSaveMethod);
procedure CustomImportCSV_M(const OnGetLine: TCSVGetLineMethod; OnNotify: TCSVSaveMethod);
procedure ImportCSV_P(const sour: TArrayPascalString; OnNotify: TCSVSaveProc);
procedure CustomImportCSV_P(const OnGetLine: TCSVGetLineProc; OnNotify: TCSVSaveProc);

function GetExtLib(LibName: SystemString): HMODULE;
function FreeExtLib(LibName: SystemString): Boolean;
function GetExtProc(const LibName, ProcName: SystemString): Pointer;

type
  TArrayRawByte = array [0 .. MaxInt - 1] of Byte;
  PArrayRawByte = ^TArrayRawByte;

function umlCompareByteString(const s1: TPascalString; const s2: PArrayRawByte): Boolean; overload;
function umlCompareByteString(const s2: PArrayRawByte; const s1: TPascalString): Boolean; overload;
procedure umlSetByteString(const sour: TPascalString; const dest: PArrayRawByte); overload;
procedure umlSetByteString(const dest: PArrayRawByte; const sour: TPascalString); overload;
function umlGetByteString(const sour: PArrayRawByte; const L: Integer): TPascalString;

procedure SaveMemory(p: Pointer; siz: NativeInt; DestFile: TPascalString);

implementation

uses
{$IF Defined(WIN32) or Defined(WIN64)}
  Fast_MD5,
{$ENDIF}
  DoStatusIO, MemoryStream64;

procedure TReliableFileStream.InitIO;
begin
  if not FActivted then
      exit;

  DoStatus(PFormat('Reliable IO Open : %s', [umlGetFileName(FileName).text]));
  DoStatus(PFormat('Backup %s size: %s', [umlGetFileName(FileName).text, umlSizeToStr(SourceIO.Size).text]));

  BackupFileIO := TCoreClassFileStream.Create(FBackupFileName, fmCreate);
  BackupFileIO.Size := SourceIO.Size;
  SourceIO.Position := 0;
  BackupFileIO.Position := 0;
  BackupFileIO.CopyFrom(SourceIO, SourceIO.Size);
  BackupFileIO.Position := 0;
  DisposeObject(SourceIO);
  SourceIO := nil;
end;

procedure TReliableFileStream.FreeIO;
begin
  if not FActivted then
      exit;

  DisposeObject(BackupFileIO);
  BackupFileIO := nil;
  try
    umlDeleteFile(FFileName);
    umlRenameFile(FBackupFileName, FileName);
  except
  end;
  DoStatus(PFormat('Reliable IO Close : %s', [umlGetFileName(FileName).text]));
end;

procedure TReliableFileStream.SetSize(const NewSize: Int64);
begin
  SourceIO.Size := NewSize;
end;

procedure TReliableFileStream.SetSize(NewSize: longint);
begin
  SetSize(Int64(NewSize));
end;

constructor TReliableFileStream.Create(const FileName_: SystemString; IsNew_, IsWrite_: Boolean);
var
  m: Word;
begin
  inherited Create;
  if IsNew_ then
      m := fmCreate
  else if IsWrite_ then
      m := fmOpenReadWrite
  else
      m := fmOpenRead or fmShareDenyNone;
{$IFDEF ZDB_BACKUP}
  FActivted := IsNew_ or IsWrite_;
{$ELSE ZDB_BACKUP}
  FActivted := False;
{$ENDIF ZDB_BACKUP}
  SourceIO := TCoreClassFileStream.Create(FileName_, m);

  BackupFileIO := nil;
  FFileName := FileName_;
  FBackupFileName := FileName_ + '.save';
  umlDeleteFile(FBackupFileName);
  InitIO;
end;

destructor TReliableFileStream.Destroy;
begin
  DisposeObject(SourceIO);
  FreeIO;
  inherited Destroy;
end;

function TReliableFileStream.write(const buffer; Count: longint): longint;
begin
  if FActivted then
    begin
      Result := BackupFileIO.write(buffer, Count);
    end
  else
    begin
      Result := SourceIO.write(buffer, Count);
    end;
end;

function TReliableFileStream.read(var buffer; Count: longint): longint;
begin
  if FActivted then
    begin
      Result := BackupFileIO.read(buffer, Count);
    end
  else
    begin
      Result := SourceIO.read(buffer, Count);
    end;
end;

function TReliableFileStream.Seek(const Offset: Int64; origin: TSeekOrigin): Int64;
begin
  if FActivted then
    begin
      Result := BackupFileIO.Seek(Offset, origin);
    end
  else
    begin
      Result := SourceIO.Seek(Offset, origin);
    end;
end;

function TIOHnd.FixedString2Pascal(s: TBytes): TPascalString;
var
  buff: TBytes;
begin
  if (length(s) > 0) and (s[0] > 0) then
    begin
      SetLength(buff, s[0]);
      CopyPtr(@s[1], @buff[0], length(buff));
      Result.Bytes := buff;
      SetLength(buff, 0);
    end
  else
      Result := '';
end;

procedure TIOHnd.Pascal2FixedString(var n: TPascalString; var out_: TBytes);
var
  buff: TBytes;
begin
  while true do
    begin
      buff := n.Bytes;
      if length(buff) > FixedStringL - 1 then
          n.DeleteFirst
      else
          break;
    end;

  SetLength(out_, FixedStringL);
  out_[0] := length(buff);
  if out_[0] > 0 then
      CopyPtr(@buff[0], @out_[1], out_[0]);
  SetLength(buff, 0);
end;

function TIOHnd.CheckFixedStringLoss(s: TPascalString): Boolean;
var
  buff: TBytes;
begin
  buff := s.Bytes;
  Result := length(buff) > FixedStringL - 1;
  SetLength(buff, 0);
end;

function umlBytesOf(const s: TPascalString): TBytes;
begin
  Result := s.Bytes
end;

function umlStringOf(const s: TBytes): TPascalString;
begin
  Result.Bytes := s;
end;

function umlNewString(const s: TPascalString): P_String;
var
  p: P_String;
begin
  New(p);
  p^ := s;
  Result := p;
end;

procedure umlFreeString(const p: P_String);
begin
  if p <> nil then
    begin
      p^ := '';
      Dispose(p);
    end;
end;

function umlComparePosStr(const s: TPascalString; Offset: Integer; const t: TPascalString): Boolean;
begin
  Result := s.ComparePos(Offset, @t);
end;

function umlPos(const SubStr, Str: TPascalString; const Offset: Integer = 1): Integer;
begin
  Result := Str.GetPos(SubStr, Offset);
end;

function umlVarToStr(const v: Variant; const Base64Conver: Boolean): TPascalString; overload;
var
  n, b64: TPascalString;
begin
  try
    case VarType(v) of
      varSmallInt, varInteger, varShortInt, varByte, varWord, varLongWord: Result := IntToStr(v);
      varInt64: Result := IntToStr(Int64(v));
      varUInt64: {$IFDEF FPC} Result := IntToStr(UInt64(v)); {$ELSE} Result := UIntToStr(UInt64(v)); {$ENDIF}
      varSingle, varDouble, varCurrency, varDate: Result := FloatToStr(v);
      varOleStr, varString, varUString:
        begin
          n.text := VarToStr(v);

          if Base64Conver and umlExistsChar(n, #10#13#9#8#0) then
            begin
              umlEncodeLineBASE64(n, b64);
              Result := '___base64:' + b64.text;
            end
          else
              Result := n.text;
        end;
      varBoolean: Result := umlBoolToStr(v);
      else
        Result := VarToStr(v);
    end;
  except
    try
        Result := VarToStr(v);
    except
        Result := '';
    end;
  end;
end;

function umlVarToStr(const v: Variant): TPascalString;
begin
  Result := umlVarToStr(v, true);
end;

function umlStrToVar(const s: TPascalString): Variant;
var
  n, b64: TPascalString;
begin
  n := umlTrimSpace(s);
  try
    if n.ComparePos(1, '___base64:') then
      begin
        n := umlDeleteFirstStr(n, ':').text;
        umlDecodeLineBASE64(n, b64);
        Result := b64.text;
      end
    else
      begin
        case umlGetNumTextType(n) of
          ntBool: Result := umlStrToBool(n);
          ntInt: Result := StrToInt(n.text);
          ntInt64: Result := StrToInt64(n.text);
{$IFDEF FPC} ntUInt64: Result := StrToQWord(n.text); {$ELSE} ntUInt64: Result := StrToUInt64(n.text); {$ENDIF}
          ntWord: Result := StrToInt(n.text);
          ntByte: Result := StrToInt(n.text);
          ntSmallInt: Result := StrToInt(n.text);
          ntShortInt: Result := StrToInt(n.text);
          ntUInt: Result := StrToInt(n.text);
          ntSingle: Result := StrToFloat(n.text);
          ntDouble: Result := StrToFloat(n.text);
          ntCurrency: Result := StrToFloat(n.text);
          else Result := n.text;
        end;
      end;
  except
      Result := n.text;
  end;
end;

function umlMax(const v1, v2: UInt64): UInt64;
begin
  if v1 > v2 then
      Result := v1
  else
      Result := v2;
end;

function umlMax(const v1, v2: Cardinal): Cardinal;
begin
  if v1 > v2 then
      Result := v1
  else
      Result := v2;
end;

function umlMax(const v1, v2: Word): Word;
begin
  if v1 > v2 then
      Result := v1
  else
      Result := v2;
end;

function umlMax(const v1, v2: Byte): Byte;
begin
  if v1 > v2 then
      Result := v1
  else
      Result := v2;
end;

function umlMax(const v1, v2: Int64): Int64;
begin
  if v1 > v2 then
      Result := v1
  else
      Result := v2;
end;

function umlMax(const v1, v2: Integer): Integer;
begin
  if v1 > v2 then
      Result := v1
  else
      Result := v2;
end;

function umlMax(const v1, v2: SmallInt): SmallInt;
begin
  if v1 > v2 then
      Result := v1
  else
      Result := v2;
end;

function umlMax(const v1, v2: ShortInt): ShortInt;
begin
  if v1 > v2 then
      Result := v1
  else
      Result := v2;
end;

function umlMax(const v1, v2: Double): Double;
begin
  if v1 > v2 then
      Result := v1
  else
      Result := v2;
end;

function umlMax(const v1, v2: Single): Single;
begin
  if v1 > v2 then
      Result := v1
  else
      Result := v2;
end;

function umlMin(const v1, v2: UInt64): UInt64;
begin
  if v1 < v2 then
      Result := v1
  else
      Result := v2;
end;

function umlMin(const v1, v2: Cardinal): Cardinal;
begin
  if v1 < v2 then
      Result := v1
  else
      Result := v2;
end;

function umlMin(const v1, v2: Word): Word;
begin
  if v1 < v2 then
      Result := v1
  else
      Result := v2;
end;

function umlMin(const v1, v2: Byte): Byte;
begin
  if v1 < v2 then
      Result := v1
  else
      Result := v2;
end;

function umlMin(const v1, v2: Int64): Int64;
begin
  if v1 < v2 then
      Result := v1
  else
      Result := v2;
end;

function umlMin(const v1, v2: Integer): Integer;
begin
  if v1 < v2 then
      Result := v1
  else
      Result := v2;
end;

function umlMin(const v1, v2: SmallInt): SmallInt;
begin
  if v1 < v2 then
      Result := v1
  else
      Result := v2;
end;

function umlMin(const v1, v2: ShortInt): ShortInt;
begin
  if v1 < v2 then
      Result := v1
  else
      Result := v2;
end;

function umlMin(const v1, v2: Double): Double;
begin
  if v1 < v2 then
      Result := v1
  else
      Result := v2;
end;

function umlMin(const v1, v2: Single): Single;
begin
  if v1 < v2 then
      Result := v1
  else
      Result := v2;
end;

function umlClamp(const v, min_, max_: UInt64): UInt64;
begin
  if min_ > max_ then
      Result := umlClamp(v, max_, min_)
  else if v > max_ then
      Result := max_
  else if v < min_ then
      Result := min_
  else
      Result := v;
end;

function umlClamp(const v, min_, max_: Cardinal): Cardinal;
begin
  if min_ > max_ then
      Result := umlClamp(v, max_, min_)
  else if v > max_ then
      Result := max_
  else if v < min_ then
      Result := min_
  else
      Result := v;
end;

function umlClamp(const v, min_, max_: Word): Word;
begin
  if min_ > max_ then
      Result := umlClamp(v, max_, min_)
  else if v > max_ then
      Result := max_
  else if v < min_ then
      Result := min_
  else
      Result := v;
end;

function umlClamp(const v, min_, max_: Byte): Byte;
begin
  if min_ > max_ then
      Result := umlClamp(v, max_, min_)
  else if v > max_ then
      Result := max_
  else if v < min_ then
      Result := min_
  else
      Result := v;
end;

function umlClamp(const v, min_, max_: Int64): Int64;
begin
  if min_ > max_ then
      Result := umlClamp(v, max_, min_)
  else if v > max_ then
      Result := max_
  else if v < min_ then
      Result := min_
  else
      Result := v;
end;

function umlClamp(const v, min_, max_: SmallInt): SmallInt;
begin
  if min_ > max_ then
      Result := umlClamp(v, max_, min_)
  else if v > max_ then
      Result := max_
  else if v < min_ then
      Result := min_
  else
      Result := v;
end;

function umlClamp(const v, min_, max_: ShortInt): ShortInt;
begin
  if min_ > max_ then
      Result := umlClamp(v, max_, min_)
  else if v > max_ then
      Result := max_
  else if v < min_ then
      Result := min_
  else
      Result := v;
end;

function umlClamp(const v, min_, max_: Double): Double;
begin
  if min_ > max_ then
      Result := umlClamp(v, max_, min_)
  else if v > max_ then
      Result := max_
  else if v < min_ then
      Result := min_
  else
      Result := v;
end;

function umlClamp(const v, min_, max_: Single): Single;
begin
  if min_ > max_ then
      Result := umlClamp(v, max_, min_)
  else if v > max_ then
      Result := max_
  else if v < min_ then
      Result := min_
  else
      Result := v;
end;

function umlInRange(const v, min_, max_: UInt64): Boolean;
begin
  Result := (v >= umlMin(min_, max_)) and (v <= umlMax(min_, max_));
end;

function umlInRange(const v, min_, max_: Cardinal): Boolean;
begin
  Result := (v >= umlMin(min_, max_)) and (v <= umlMax(min_, max_));
end;

function umlInRange(const v, min_, max_: Word): Boolean;
begin
  Result := (v >= umlMin(min_, max_)) and (v <= umlMax(min_, max_));
end;

function umlInRange(const v, min_, max_: Byte): Boolean;
begin
  Result := (v >= umlMin(min_, max_)) and (v <= umlMax(min_, max_));
end;

function umlInRange(const v, min_, max_: SmallInt): Boolean;
begin
  Result := (v >= umlMin(min_, max_)) and (v <= umlMax(min_, max_));
end;

function umlInRange(const v, min_, max_: ShortInt): Boolean;
begin
  Result := (v >= umlMin(min_, max_)) and (v <= umlMax(min_, max_));
end;

function umlInRange(const v, min_, max_: Double): Boolean;
begin
  Result := (v >= umlMin(min_, max_)) and (v <= umlMax(min_, max_));
end;

function umlInRange(const v, min_, max_: Single): Boolean;
begin
  Result := (v >= umlMin(min_, max_)) and (v <= umlMax(min_, max_));
end;

function umlGetResourceStream(const FileName: TPascalString): TCoreClassStream;
var
  n: TPascalString;
begin
  if FileName.Exists('.') then
      n := umlDeleteLastStr(FileName, '.')
  else
      n := FileName;

  Result := TCoreClassResourceStream.Create(HInstance, n.text, RT_RCDATA);
end;

function umlSameVarValue(const v1, v2: Variant): Boolean;
begin
  try
      Result := VarSameValue(v1, v2);
  except
      Result := False;
  end;
end;

function umlSameVariant(const v1, v2: Variant): Boolean;
begin
  try
      Result := VarSameValue(v1, v2);
  except
      Result := False;
  end;
end;

function umlRandom(const rnd: TMT19937Random): Integer;
begin
  Result := rnd.Rand32(MaxInt);
end;

function umlRandom: Integer;
begin
  Result := MT19937Rand32(MaxInt);
end;

function umlRandomRange(const rnd: TMT19937Random; const min_, max_: Integer): Integer;
var
  mn, mx: Integer;
begin
  mn := min_;
  mx := max_;

  if mn > mx then
      inc(mn)
  else
      inc(mx);

  if mn > mx then
      Result := rnd.Rand32(mn - mx) + mx
  else
      Result := rnd.Rand32(mx - mn) + mn;
end;

function umlRandomRangeS(const rnd: TMT19937Random; const min_, max_: Single): Single;
begin
  Result := (umlRandomRange(rnd, Trunc(min_ * 1000), Trunc(max_ * 1000))) * 0.001;
end;

function umlRandomRangeD(const rnd: TMT19937Random; const min_, max_: Double): Double;
begin
  Result := (umlRandomRange(rnd, Trunc(min_ * 10000), Trunc(max_ * 10000))) * 0.0001;
end;

function umlRandomRangeF(const rnd: TMT19937Random; const min_, max_: Double): Double;
begin
  Result := (umlRandomRange(rnd, Trunc(min_ * 10000), Trunc(max_ * 10000))) * 0.0001;
end;

function umlRandomRange(const min_, max_: Integer): Integer;
var
  mn, mx: Integer;
begin
  mn := min_;
  mx := max_;

  if mn > mx then
      inc(mn)
  else
      inc(mx);

  if mn > mx then
      Result := MT19937Rand32(mn - mx) + mx
  else
      Result := MT19937Rand32(mx - mn) + mn;
end;

function umlRandomRangeS(const min_, max_: Single): Single;
begin
  Result := (umlRandomRange(Trunc(min_ * 1000), Trunc(max_ * 1000))) * 0.001;
end;

function umlRandomRangeD(const min_, max_: Double): Double;
begin
  Result := (umlRandomRange(Trunc(min_ * 10000), Trunc(max_ * 10000))) * 0.0001;
end;

function umlRandomRangeF(const min_, max_: Double): Double;
begin
  Result := (umlRandomRange(Trunc(min_ * 10000), Trunc(max_ * 10000))) * 0.0001;
end;

function umlDefaultTime: Double;
begin
  Result := Now;
end;

function umlNow: Double;
begin
  Result := Now;
end;

function umlDefaultAttrib: Integer;
begin
  Result := 0;
end;

function umlBoolToStr(const Value: Boolean): TPascalString;
begin
  if Value then
      Result := 'True'
  else
      Result := 'False';
end;

function umlStrToBool(const Value: TPascalString): Boolean;
var
  NewValue: TPascalString;
begin
  NewValue := umlTrimSpace(Value);
  if NewValue.Same('Yes') then
      Result := true
  else if NewValue.Same('No') then
      Result := False
  else if NewValue.Same('True') then
      Result := true
  else if NewValue.Same('False') then
      Result := False
  else if NewValue.Same('1') then
      Result := true
  else if NewValue.Same('0') then
      Result := False
  else
      Result := False;
end;

function umlFileExists(const FileName: TPascalString): Boolean;
begin
  if FileName.Len > 0 then
      Result := FileExists(FileName.text)
  else
      Result := False;
end;

function umlDirectoryExists(const DirectoryName: TPascalString): Boolean;
begin
  if DirectoryName.Len > 0 then
      Result := DirectoryExists(DirectoryName.text)
  else
      Result := False;
end;

function umlCreateDirectory(const DirectoryName: TPascalString): Boolean;
begin
  Result := umlDirectoryExists(DirectoryName);
  if Result then
      exit;

  try
      Result := ForceDirectories(DirectoryName.text);
  except
    try
        Result := CreateDir(DirectoryName.text);
    except
        Result := False;
    end;
  end;
end;

function umlCurrentDirectory: TPascalString;
begin
  Result.text := GetCurrentDir;
end;

function umlCurrentPath: TPascalString;
begin
  Result.text := GetCurrentDir;
  case CurrentPlatform of
    epWin32, epWin64: if (Result.Len = 0) or (Result.Last <> '\') then
          Result := Result.text + '\';
    else
      if (Result.Len = 0) or (Result.Last <> '/') then
          Result := Result.text + '/';
  end;
end;

function umlGetCurrentPath: TPascalString;
begin
  Result := umlCurrentPath();
end;

procedure umlSetCurrentPath(ph: TPascalString);
begin
  SetCurrentDir(ph.text);
end;

function umlFindFirstFile(const FileName: TPascalString; var SR: TSR): Boolean;
label SearchPoint;
begin
  if FindFirst(FileName.text, faAnyFile, SR) <> 0 then
    begin
      Result := False;
      exit;
    end;
  if ((SR.Attr and faDirectory) <> faDirectory) then
    begin
      Result := true;
      exit;
    end;
SearchPoint:
  if FindNext(SR) <> 0 then
    begin
      Result := False;
      exit;
    end;
  if ((SR.Attr and faDirectory) <> faDirectory) then
    begin
      Result := true;
      exit;
    end;
  goto SearchPoint;
end;

function umlFindNextFile(var SR: TSR): Boolean;
label SearchPoint;
begin
SearchPoint:
  if FindNext(SR) <> 0 then
    begin
      Result := False;
      exit;
    end;
  if ((SR.Attr and faDirectory) <> faDirectory) then
    begin
      Result := true;
      exit;
    end;
  goto SearchPoint;
end;

function umlFindFirstDir(const DirName: TPascalString; var SR: TSR): Boolean;
label SearchPoint;
begin
  if FindFirst(DirName.text, faAnyFile, SR) <> 0 then
    begin
      Result := False;
      exit;
    end;
  if ((SR.Attr and faDirectory) = faDirectory) and (SR.Name <> '.') and (SR.Name <> '..') then
    begin
      Result := true;
      exit;
    end;
SearchPoint:
  if FindNext(SR) <> 0 then
    begin
      Result := False;
      exit;
    end;
  if ((SR.Attr and faDirectory) = faDirectory) and (SR.Name <> '.') and (SR.Name <> '..') then
    begin
      Result := true;
      exit;
    end;
  goto SearchPoint;
end;

function umlFindNextDir(var SR: TSR): Boolean;
label SearchPoint;
begin
SearchPoint:
  if FindNext(SR) <> 0 then
    begin
      Result := False;
      exit;
    end;
  if ((SR.Attr and faDirectory) = faDirectory) and (SR.Name <> '.') and (SR.Name <> '..') then
    begin
      Result := true;
      exit;
    end;
  goto SearchPoint;
end;

procedure umlFindClose(var SR: TSR);
begin
  FindClose(SR);
end;

function umlGetFileList(const FullPath: TPascalString; AsLst: TCoreClassStrings): Integer;
var
  _SR: TSR;
begin
  Result := 0;
  if umlFindFirstFile(umlCombineFileName(FullPath, '*'), _SR) then
    begin
      repeat
        AsLst.Add(_SR.Name);
        inc(Result);
      until not umlFindNextFile(_SR);
    end;
  umlFindClose(_SR);
end;

function umlGetDirList(const FullPath: TPascalString; AsLst: TCoreClassStrings): Integer;
var
  _SR: TSR;
begin
  Result := 0;
  if umlFindFirstDir(umlCombineFileName(FullPath, '*'), _SR) then
    begin
      repeat
        AsLst.Add(_SR.Name);
        inc(Result);
      until not umlFindNextDir(_SR);
    end;
  umlFindClose(_SR);
end;

function umlGetFileList(const FullPath: TPascalString; AsLst: TPascalStringList): Integer;
var
  _SR: TSR;
begin
  Result := 0;
  if umlFindFirstFile(umlCombineFileName(FullPath, '*'), _SR) then
    begin
      repeat
        AsLst.Add(_SR.Name);
        inc(Result);
      until not umlFindNextFile(_SR);
    end;
  umlFindClose(_SR);
end;

function umlGetDirList(const FullPath: TPascalString; AsLst: TPascalStringList): Integer;
var
  _SR: TSR;
begin
  Result := 0;
  if umlFindFirstDir(umlCombineFileName(FullPath, '*'), _SR) then
    begin
      repeat
        AsLst.Add(_SR.Name);
        inc(Result);
      until not umlFindNextDir(_SR);
    end;
  umlFindClose(_SR);
end;

function umlGetFileListWithFullPath(const FullPath: TPascalString): U_StringArray;
var
  ph: TPascalString;
  ns: TPascalStringList;
  i: Integer;
begin
  ph := FullPath;
  ns := TPascalStringList.Create;
  umlGetFileList(FullPath, ns);
  SetLength(Result, ns.Count);
  for i := 0 to ns.Count - 1 do
      Result[i] := umlCombineFileName(ph, ns[i]).text;
  DisposeObject(ns);
end;

function umlGetDirListWithFullPath(const FullPath: TPascalString): U_StringArray;
var
  ph: TPascalString;
  ns: TPascalStringList;
  i: Integer;
begin
  ph := FullPath;
  ns := TPascalStringList.Create;
  umlGetDirList(FullPath, ns);
  SetLength(Result, ns.Count);
  for i := 0 to ns.Count - 1 do
      Result[i] := umlCombinePath(ph, ns[i]).text;
  DisposeObject(ns);
end;

function umlGetFileListPath(const FullPath: TPascalString): U_StringArray;
var
  ph: TPascalString;
  ns: TPascalStringList;
  i: Integer;
begin
  ph := FullPath;
  ns := TPascalStringList.Create;
  umlGetFileList(FullPath, ns);
  SetLength(Result, ns.Count);
  for i := 0 to ns.Count - 1 do
      Result[i] := ns[i];
  DisposeObject(ns);
end;

function umlGetDirListPath(const FullPath: TPascalString): U_StringArray;
var
  ph: TPascalString;
  ns: TPascalStringList;
  i: Integer;
begin
  ph := FullPath;
  ns := TPascalStringList.Create;
  umlGetDirList(FullPath, ns);
  SetLength(Result, ns.Count);
  for i := 0 to ns.Count - 1 do
      Result[i] := ns[i];
  DisposeObject(ns);
end;

function umlCombinePath(const s1, s2: TPascalString): TPascalString;
begin
  if CurrentPlatform in [epWin32, epWin64] then
      Result := umlCombineWinPath(s1, s2)
  else
      Result := umlCombineUnixPath(s1, s2);
end;

function umlCombineFileName(const pathName, FileName: TPascalString): TPascalString;
begin
  if CurrentPlatform in [epWin32, epWin64] then
      Result := umlCombineWinFileName(pathName, FileName)
  else
      Result := umlCombineUnixFileName(pathName, FileName);
end;

function umlCombineUnixPath(const s1, s2: TPascalString): TPascalString;
var
  n1, n2, n: TPascalString;
begin
  n1 := umlTrimSpace(s1);
  n2 := umlTrimSpace(s2);

  n1 := umlCharReplace(n1, '\', '/');
  n2 := umlCharReplace(n2, '\', '/');

  if (n2.Len > 0) and (n2.First = '/') then
      n2.DeleteFirst;

  if n1.Len > 0 then
    begin
      if n1.Last = '/' then
          Result := n1.text + n2.text
      else
          Result := n1.text + '/' + n2.text;
    end
  else
      Result := n2;

  repeat
    n := Result;
    Result := umlStringReplace(Result, '//', '/', true);
  until Result.Same(n);
  if (Result.Len > 0) and (Result.Last <> '/') then
      Result.Append('/');
end;

function umlCombineUnixFileName(const pathName, FileName: TPascalString): TPascalString;
var
  pn, fn, n: TPascalString;
begin
  pn := umlTrimSpace(pathName);
  fn := umlTrimSpace(FileName);

  pn := umlCharReplace(pn, '\', '/');
  fn := umlCharReplace(fn, '\', '/');

  if (fn.Len > 0) and (fn.First = '/') then
      fn.DeleteFirst;
  if (fn.Len > 0) and (fn.Last = '/') then
      fn.DeleteLast;

  if pn.Len > 0 then
    begin
      if pn.Last = '/' then
          Result := pn.text + fn.text
      else
          Result := pn.text + '/' + fn.text;
    end
  else
      Result := fn;

  repeat
    n := Result;
    Result := umlStringReplace(Result, '//', '/', true);
  until Result.Same(n);
end;

function umlCombineWinPath(const s1, s2: TPascalString): TPascalString;
var
  n1, n2, n: TPascalString;
begin
  n1 := umlTrimSpace(s1);
  n2 := umlTrimSpace(s2);

  n1 := umlCharReplace(n1, '/', '\');
  n2 := umlCharReplace(n2, '/', '\');

  if (n2.Len > 0) and (n2.First = '\') then
      n2.DeleteFirst;

  if n1.Len > 0 then
    begin
      if n1.Last = '\' then
          Result := n1.text + n2.text
      else
          Result := n1.text + '\' + n2.text;
    end
  else
      Result := n2;

  repeat
    n := Result;
    Result := umlStringReplace(Result, '\\', '\', true);
  until Result.Same(n);
  if (Result.Len > 0) and (Result.Last <> '\') then
      Result.Append('\');
end;

function umlCombineWinFileName(const pathName, FileName: TPascalString): TPascalString;
var
  pn, fn, n: TPascalString;
begin
  pn := umlTrimSpace(pathName);
  fn := umlTrimSpace(FileName);

  pn := umlCharReplace(pn, '/', '\');
  fn := umlCharReplace(fn, '/', '\');

  if (fn.Len > 0) and (fn.First = '\') then
      fn.DeleteFirst;
  if (fn.Len > 0) and (fn.Last = '\') then
      fn.DeleteLast;

  if pn.Len > 0 then
    begin
      if pn.Last = '\' then
          Result := pn.text + fn.text
      else
          Result := pn.text + '\' + fn.text;
    end
  else
      Result := fn;

  repeat
    n := Result;
    Result := umlStringReplace(Result, '\\', '\', true);
  until Result.Same(n);

  if Result.Last = '\' then
      Result.DeleteLast;
end;

function umlGetFileName(const s: TPascalString): TPascalString;
var
  n: TPascalString;
begin
  case CurrentPlatform of
    epWin32, epWin64:
      begin
        n := umlCharReplace(umlTrimSpace(s), '/', '\');
        if n.Len = 0 then
            Result := ''
        else if (n.Last = '\') then
            Result := ''
        else if n.Exists('\') then
            Result := umlGetLastStr(n, '\')
        else
            Result := n;
      end;
    else
      begin
        n := umlCharReplace(umlTrimSpace(s), '\', '/');
        if n.Len = 0 then
            Result := ''
        else if (n.Last = '/') then
            Result := ''
        else if n.Exists('/') then
            Result := umlGetLastStr(n, '/')
        else
            Result := n;
      end;
  end;
end;

function umlGetFilePath(const s: TPascalString): TPascalString;
var
  n: TPascalString;
begin
  case CurrentPlatform of
    epWin32, epWin64:
      begin
        n := umlCharReplace(umlTrimSpace(s), '/', '\');
        if n.Len = 0 then
            Result := ''
        else if not n.Exists('\') then
            Result := ''
        else if (n.Last <> '\') then
            Result := umlDeleteLastStr(n, '\')
        else
            Result := n;
        if umlMultipleMatch('?:', Result) then
            Result.Append('\');
      end;
    else
      begin
        n := umlCharReplace(umlTrimSpace(s), '\', '/');
        if n.Len = 0 then
            Result := ''
        else if not n.Exists('/') then
            Result := ''
        else if (n.Last <> '/') then
            Result := umlDeleteLastStr(n, '/')
        else
            Result := n;
      end;
  end;
end;

function umlChangeFileExt(const s, ext: TPascalString): TPascalString;
var
  ph, fn: TPascalString;
  n: TPascalString;
begin
  if s.Len = 0 then
    begin
      Result := ext;
      exit;
    end;

  ph := umlGetFilePath(s);
  fn := umlGetFileName(s);

  n := ext;
  if (n.Len > 0) and (n.First <> '.') then
      n.text := '.' + n.text;
  if umlExistsChar(fn, '.') then
      Result := umlDeleteLastStr(fn, '.') + n
  else
      Result := fn + n;

  if ph.Len > 0 then
      Result := umlCombineFileName(ph, Result);
end;

function umlGetFileExt(const s: TPascalString): TPascalString;
begin
  if (s.Len > 0) and (umlExistsChar(s, '.')) then
      Result := '.' + umlGetLastStr(s, '.')
  else
      Result := '';
end;

procedure InitIOHnd(var IOHnd: TIOHnd);
begin
  IOHnd.IsOnlyRead := true;
  IOHnd.IsOpen := False;
  IOHnd.AutoFree := False;
  IOHnd.Handle := nil;
  IOHnd.Time := 0;
  IOHnd.Size := 0;
  IOHnd.Position := 0;
  IOHnd.FileName := '';
  IOHnd.FlushBuff := nil;
  IOHnd.FlushPosition := -1;
  IOHnd.PrepareReadPosition := -1;
  IOHnd.PrepareReadBuff := nil;
  IOHnd.IORead := 0;
  IOHnd.IOWrite := 0;
  IOHnd.WriteStated := False;
  IOHnd.FixedStringL := 64 + 1;
  IOHnd.Data := nil;
  IOHnd.Return := C_NotError;
end;

function umlFileCreateAsStream(const FileName: TPascalString; stream: U_Stream; var IOHnd: TIOHnd): Boolean;
begin
  if IOHnd.IsOpen = true then
    begin
      IOHnd.Return := C_FileIsActive;
      Result := False;
      exit;
    end;
  stream.Position := 0;
  IOHnd.Handle := stream;
  IOHnd.Return := C_NotError;
  IOHnd.Size := stream.Size;
  IOHnd.Position := stream.Position;
  IOHnd.Time := umlDefaultTime;
  IOHnd.FileName := FileName;
  IOHnd.IsOpen := true;
  IOHnd.IsOnlyRead := False;
  IOHnd.AutoFree := False;
  Result := true;
end;

function umlFileCreateAsStream(stream: U_Stream; var IOHnd: TIOHnd): Boolean;
begin
  Result := umlFileCreateAsStream('', stream, IOHnd);
end;

function umlFileOpenAsStream(const FileName: TPascalString; stream: U_Stream; var IOHnd: TIOHnd; OnlyRead_: Boolean): Boolean;
begin
  if IOHnd.IsOpen = true then
    begin
      IOHnd.Return := C_FileIsActive;
      Result := False;
      exit;
    end;
  stream.Position := 0;
  IOHnd.Handle := stream;
  IOHnd.Return := C_NotError;
  IOHnd.Size := stream.Size;
  IOHnd.Position := stream.Position;
  IOHnd.Time := umlDefaultTime;
  IOHnd.FileName := FileName;
  IOHnd.IsOpen := true;
  IOHnd.IsOnlyRead := OnlyRead_;
  IOHnd.AutoFree := False;
  Result := true;
end;

function umlFileCreateAsMemory(var IOHnd: TIOHnd): Boolean;
begin
  if IOHnd.IsOpen = true then
    begin
      IOHnd.Return := C_FileIsActive;
      Result := False;
      exit;
    end;
  IOHnd.Handle := TMemoryStream64.CustomCreate(8192);
  IOHnd.Return := C_NotError;
  IOHnd.Size := IOHnd.Handle.Size;
  IOHnd.Position := IOHnd.Handle.Position;
  IOHnd.Time := umlDefaultTime;
  IOHnd.FileName := 'Memory';
  IOHnd.IsOpen := true;
  IOHnd.IsOnlyRead := False;
  IOHnd.AutoFree := true;
  Result := true;
end;

function umlFileCreate(const FileName: TPascalString; var IOHnd: TIOHnd): Boolean;
begin
  if IOHnd.IsOpen = true then
    begin
      IOHnd.Return := C_FileIsActive;
      Result := False;
      exit;
    end;
  try
      IOHnd.Handle := TReliableFileStream.Create(FileName.text, true, true);
  except
    IOHnd.Handle := nil;
    IOHnd.Return := C_CreateFileError;
    Result := False;
    exit;
  end;
  IOHnd.Return := C_NotError;
  IOHnd.Size := 0;
  IOHnd.Position := 0;
  IOHnd.Time := Now;
  IOHnd.FileName := FileName;
  IOHnd.IsOpen := true;
  IOHnd.IsOnlyRead := False;
  IOHnd.AutoFree := true;
  Result := true;
end;

function umlFileOpen(const FileName: TPascalString; var IOHnd: TIOHnd; OnlyRead_: Boolean): Boolean;
begin
  if IOHnd.IsOpen = true then
    begin
      IOHnd.Return := C_FileIsActive;
      Result := False;
      exit;
    end;
  if not umlFileExists(FileName) then
    begin
      IOHnd.Return := C_NotFindFile;
      Result := False;
      exit;
    end;
  try
      IOHnd.Handle := TReliableFileStream.Create(FileName.text, False, not OnlyRead_);
  except
    IOHnd.Handle := nil;
    IOHnd.Return := C_OpenFileError;
    Result := False;
    exit;
  end;
  IOHnd.IsOnlyRead := OnlyRead_;
  IOHnd.Return := C_NotError;
  IOHnd.Size := IOHnd.Handle.Size;
  IOHnd.Position := 0;
  IOHnd.Time := umlGetFileTime(FileName);
  IOHnd.FileName := FileName;
  IOHnd.IsOpen := true;
  IOHnd.AutoFree := true;
  Result := true;
end;

function umlFileClose(var IOHnd: TIOHnd): Boolean;
begin
  if IOHnd.IsOpen = False then
    begin
      IOHnd.Return := C_NotOpenFile;
      Result := False;
      exit;
    end;
  if IOHnd.Handle = nil then
    begin
      IOHnd.Return := C_FileHandleError;
      Result := False;
      exit;
    end;

  umlFileFlushWrite(IOHnd);

  if IOHnd.PrepareReadBuff <> nil then
      DisposeObject(IOHnd.PrepareReadBuff);
  IOHnd.PrepareReadBuff := nil;
  IOHnd.PrepareReadPosition := -1;

  try
    if IOHnd.AutoFree then
        DisposeObject(IOHnd.Handle)
    else
        IOHnd.Handle := nil;
  except
  end;
  IOHnd.Handle := nil;
  IOHnd.Return := C_NotError;
  IOHnd.Time := umlDefaultTime;
  IOHnd.FileName := '';
  IOHnd.IsOpen := False;
  IOHnd.WriteStated := False;
  Result := true;
end;

function umlFileUpdate(var IOHnd: TIOHnd): Boolean;
begin
  if (IOHnd.IsOpen = False) or (IOHnd.Handle = nil) then
    begin
      IOHnd.Return := C_FileHandleError;
      Result := False;
      exit;
    end;

  umlFileFlushWrite(IOHnd);
  umlResetPrepareRead(IOHnd);
  IOHnd.WriteStated := False;

  Result := true;
end;

function umlFileTest(var IOHnd: TIOHnd): Boolean;
begin
  if (IOHnd.IsOpen = False) or (IOHnd.Handle = nil) then
    begin
      IOHnd.Return := C_FileHandleError;
      Result := False;
      exit;
    end;
  IOHnd.Return := C_NotError;
  Result := true;
end;

procedure umlResetPrepareRead(var IOHnd: TIOHnd);
begin
  if IOHnd.PrepareReadBuff <> nil then
      DisposeObject(IOHnd.PrepareReadBuff);
  IOHnd.PrepareReadBuff := nil;
  IOHnd.PrepareReadPosition := -1;
end;

function umlFilePrepareRead(var IOHnd: TIOHnd; Size: Int64; var buff): Boolean;
var
  m64: TMemoryStream64;
  preRedSiz: Int64;
begin
  Result := False;

  if not IOHnd.Handle.InheritsFrom(TCoreClassFileStream) then
      exit;

  if Size > C_PrepareReadCacheSize then
    begin
      umlResetPrepareRead(IOHnd);
      IOHnd.Handle.Position := IOHnd.Position;
      exit;
    end;

  if IOHnd.PrepareReadBuff = nil then
      IOHnd.PrepareReadBuff := TMemoryStream64.Create;

  m64 := TMemoryStream64(IOHnd.PrepareReadBuff);

  if (IOHnd.Position < IOHnd.PrepareReadPosition) or (IOHnd.PrepareReadPosition + m64.Size < IOHnd.Position + Size) then
    begin
      // prepare read buffer
      IOHnd.Handle.Position := IOHnd.Position;
      IOHnd.PrepareReadPosition := IOHnd.Position;

      m64.Clear;
      IOHnd.PrepareReadPosition := IOHnd.Handle.Position;
      if IOHnd.Handle.Size - IOHnd.Handle.Position >= C_PrepareReadCacheSize then
        begin
          Result := m64.CopyFrom(IOHnd.Handle, C_PrepareReadCacheSize) = C_PrepareReadCacheSize;
          inc(IOHnd.IORead, C_PrepareReadCacheSize);
        end
      else
        begin
          preRedSiz := IOHnd.Handle.Size - IOHnd.Handle.Position;
          Result := m64.CopyFrom(IOHnd.Handle, preRedSiz) = preRedSiz;
          inc(IOHnd.IORead, preRedSiz);
        end;
    end;

  if (IOHnd.Position >= IOHnd.PrepareReadPosition) and (IOHnd.PrepareReadPosition + m64.Size >= IOHnd.Position + Size) then
    begin
      CopyPtr(Pointer(nativeUInt(m64.Memory) + (IOHnd.Position - IOHnd.PrepareReadPosition)), @buff, Size);
      inc(IOHnd.Position, Size);
      Result := true;
    end
  else
    begin
      // safe process
      umlResetPrepareRead(IOHnd);
      IOHnd.Handle.Position := IOHnd.Position;
      exit;
    end;
end;

function umlFileRead(var IOHnd: TIOHnd; const Size: Int64; var buff): Boolean;
var
  BuffPointer: Pointer;
  i: NativeInt;
  BuffInt: nativeUInt;
begin
  if not umlFileFlushWrite(IOHnd) then
    begin
      Result := False;
      exit;
    end;

  if Size = 0 then
    begin
      IOHnd.Return := C_NotError;
      Result := true;
      exit;
    end;

  if umlFilePrepareRead(IOHnd, Size, buff) then
    begin
      IOHnd.Return := C_NotError;
      Result := true;
      exit;
    end;

  try
    if Size > C_MaxBufferFragmentSize then
      begin
        // process Chunk buffer
        BuffInt := nativeUInt(@buff);
        BuffPointer := Pointer(BuffInt);
        for i := 1 to (Size div C_MaxBufferFragmentSize) do
          begin
            if IOHnd.Handle.read(BuffPointer^, C_MaxBufferFragmentSize) <> C_MaxBufferFragmentSize then
              begin
                IOHnd.Return := C_FileReadError;
                Result := False;
                exit;
              end;
            BuffInt := BuffInt + C_MaxBufferFragmentSize;
            BuffPointer := Pointer(BuffInt);
          end;
        // process buffer rest
        i := Size mod C_MaxBufferFragmentSize;
        if IOHnd.Handle.read(BuffPointer^, i) <> i then
          begin
            IOHnd.Return := C_FileReadError;
            Result := False;
            exit;
          end;
        inc(IOHnd.Position, Size);
        IOHnd.Return := C_NotError;
        Result := true;
        inc(IOHnd.IORead, Size);
        exit;
      end;
    if IOHnd.Handle.read(buff, Size) <> Size then
      begin
        IOHnd.Return := C_FileReadError;
        Result := False;
        exit;
      end;
    inc(IOHnd.Position, Size);
    IOHnd.Return := C_NotError;
    Result := true;
    inc(IOHnd.IORead, Size);
  except
    IOHnd.Return := C_FileReadError;
    Result := False;
  end;
end;

function umlBlockRead(var IOHnd: TIOHnd; var buff; Size: Int64): Boolean;
begin
  Result := umlFileRead(IOHnd, Size, buff);
end;

function umlFilePrepareWrite(var IOHnd: TIOHnd): Boolean;
begin
  Result := true;

  if not umlFileTest(IOHnd) then
      exit;

  if IOHnd.FlushBuff <> nil then
      exit;

  if IOHnd.Handle is TCoreClassFileStream then
    begin
      IOHnd.FlushBuff := TMemoryStream64.Create;
      IOHnd.FlushPosition := IOHnd.Handle.Position;
    end;
end;

function umlFileFlushWrite(var IOHnd: TIOHnd): Boolean;
var
  m64: TMemoryStream64;
begin
  if IOHnd.FlushBuff <> nil then
    begin
      m64 := TMemoryStream64(IOHnd.FlushBuff);
      IOHnd.FlushBuff := nil;

      if IOHnd.Handle.write(m64.Memory^, m64.Size) <> m64.Size then
        begin
          IOHnd.Return := C_FileWriteError;
          Result := False;
          exit;
        end;
      inc(IOHnd.IOWrite, m64.Size);
      DisposeObject(m64);
    end;
  Result := true;
end;

function umlFileWrite(var IOHnd: TIOHnd; const Size: Int64; var buff): Boolean;
var
  BuffPointer: Pointer;
  i: NativeInt;
  BuffInt: nativeUInt;
begin
  if (IOHnd.IsOnlyRead) or (not IOHnd.IsOpen) then
    begin
      IOHnd.Return := C_FileWriteError;
      Result := False;
      exit;
    end;
  if Size = 0 then
    begin
      IOHnd.Return := C_NotError;
      Result := true;
      exit;
    end;

  IOHnd.WriteStated := true;

  umlResetPrepareRead(IOHnd);

  if Size <= $F000 then
      umlFilePrepareWrite(IOHnd);

  if IOHnd.FlushBuff <> nil then
    begin
      if TMemoryStream64(IOHnd.FlushBuff).Write64(buff, Size) <> Size then
        begin
          IOHnd.Return := C_FileWriteError;
          Result := False;
          exit;
        end;

      inc(IOHnd.Position, Size);
      if IOHnd.Position > IOHnd.Size then
          IOHnd.Size := IOHnd.Position;
      IOHnd.Return := C_NotError;
      Result := true;

      // 8M flush buffer
      if IOHnd.FlushBuff.Size > 8 * 1024 * 1024 then
          umlFileFlushWrite(IOHnd);
      exit;
    end;

  try
    if Size > C_MaxBufferFragmentSize then
      begin
        // process buffer chunk
        BuffInt := nativeUInt(@buff);
        BuffPointer := Pointer(BuffInt);
        for i := 1 to (Size div C_MaxBufferFragmentSize) do
          begin
            if IOHnd.Handle.write(BuffPointer^, C_MaxBufferFragmentSize) <> C_MaxBufferFragmentSize then
              begin
                IOHnd.Return := C_FileWriteError;
                Result := False;
                exit;
              end;
            BuffInt := BuffInt + C_MaxBufferFragmentSize;
            BuffPointer := Pointer(BuffInt);
          end;
        // process buffer rest
        i := Size mod C_MaxBufferFragmentSize;
        if IOHnd.Handle.write(BuffPointer^, i) <> i then
          begin
            IOHnd.Return := C_FileWriteError;
            Result := False;
            exit;
          end;

        inc(IOHnd.Position, Size);
        if IOHnd.Position > IOHnd.Size then
            IOHnd.Size := IOHnd.Position;
        IOHnd.Return := C_NotError;
        Result := true;
        inc(IOHnd.IOWrite, Size);
        exit;
      end;
    if IOHnd.Handle.write(buff, Size) <> Size then
      begin
        IOHnd.Return := C_FileWriteError;
        Result := False;
        exit;
      end;

    inc(IOHnd.Position, Size);
    if IOHnd.Position > IOHnd.Size then
        IOHnd.Size := IOHnd.Position;
    IOHnd.Return := C_NotError;
    Result := true;
    inc(IOHnd.IOWrite, Size);
  except
    IOHnd.Return := C_FileWriteError;
    Result := False;
  end;
end;

function umlBlockWrite(var IOHnd: TIOHnd; var buff; const Size: Int64): Boolean;
begin
  Result := umlFileWrite(IOHnd, Size, buff);
end;

function umlFileWriteFixedString(var IOHnd: TIOHnd; var Value: TPascalString): Boolean;
var
  buff: TBytes;
begin
  IOHnd.Pascal2FixedString(Value, buff);
  if umlFileWrite(IOHnd, IOHnd.FixedStringL, buff[0]) = False then
    begin
      IOHnd.Return := C_FileWriteError;
      Result := False;
      exit;
    end;

  IOHnd.Return := C_NotError;
  Result := true;
end;

function umlFileReadFixedString(var IOHnd: TIOHnd; var Value: TPascalString): Boolean;
var
  buff: TBytes;
begin
  try
    SetLength(buff, IOHnd.FixedStringL);
    if umlFileRead(IOHnd, IOHnd.FixedStringL, buff[0]) = False then
      begin
        IOHnd.Return := C_FileReadError;
        Result := False;
        exit;
      end;
    Value := IOHnd.FixedString2Pascal(buff);
    SetLength(buff, 0);
    IOHnd.Return := C_NotError;
    Result := true;
  except
    Value.text := '';
    IOHnd.Return := C_StringError;
    Result := False;
  end;
end;

function umlFileSeek(var IOHnd: TIOHnd; APos: Int64): Boolean;
begin
  if (APos <> IOHnd.Position) or (APos <> IOHnd.Handle.Position) then
    if not umlFileFlushWrite(IOHnd) then
      begin
        Result := False;
        exit;
      end;

  IOHnd.Return := C_SeekError;
  Result := False;
  try
    IOHnd.Position := IOHnd.Handle.Seek(APos, TSeekOrigin.soBeginning);
    Result := IOHnd.Position <> -1;
    if Result then
        IOHnd.Return := C_NotError;
  except
  end;
end;

function umlFileGetPOS(var IOHnd: TIOHnd): Int64;
begin
  Result := IOHnd.Position;
end;

function umlFilePOS(var IOHnd: TIOHnd): Int64;
begin
  Result := umlFileGetPOS(IOHnd);
end;

function umlFileGetSize(var IOHnd: TIOHnd): Int64;
begin
  Result := IOHnd.Size;
end;

function umlFileSize(var IOHnd: TIOHnd): Int64;
begin
  Result := umlFileGetSize(IOHnd);
end;

function umlGetFileTime(const FileName: TPascalString): TDateTime;
{$IFDEF MSWINDOWS}
  function CovFileDate_(Fd: TFileTime): TDateTime;
  var
    Tct: _SystemTime;
    t: TFileTime;
  begin
    FileTimeToLocalFileTime(Fd, t);
    FileTimeToSystemTime(t, Tct);
    CovFileDate_ := SystemTimeToDateTime(Tct);
  end;

var
  SR: TSR;
begin
  if umlFindFirstFile(FileName, SR) then
      Result := CovFileDate_(SR.FindData.ftLastWriteTime)
  else
      Result := umlNow();
  umlFindClose(SR);
end;
{$ELSE MSWINDOWS}


var
  f: THandle;
begin
  f := FileOpen(FileName.text, fmOpenRead or fmShareDenyNone);
  if f <> THandle(-1) then
    begin
      Result := FileDateToDateTime(FileGetDate(f));
      FileClose(f);
    end
  else
      Result := Now;
end;
{$ENDIF MSWINDOWS}


procedure umlSetFileTime(const FileName: TPascalString; newTime: TDateTime);
begin
  FileSetDate(FileName.text, DateTimeToFileDate(newTime));
end;

function umlGetFileSize(const FileName: TPascalString): Int64;
var
  SR: TSR;
begin
  Result := 0;
  if umlFindFirstFile(FileName, SR) = true then
    begin
      Result := SR.Size;
      while umlFindNextFile(SR) do
          Result := Result + SR.Size;
    end;
  umlFindClose(SR);
end;

function umlGetFileCount(const FileName: TPascalString): Integer;
var
  SR: TSR;
begin
  Result := 0;
  if umlFindFirstFile(FileName, SR) = true then
    begin
      Result := Result + 1;
      while umlFindNextFile(SR) = true do
          Result := Result + 1;
    end;
  umlFindClose(SR);
end;

function umlGetFileDateTime(const FileName: TPascalString): TDateTime;
begin
  if not FileAge(FileName.text, Result, False) then
      Result := Now;
end;

function umlDeleteFile(const FileName: TPascalString; const _VerifyCheck: Boolean): Boolean;
var
  _SR: TSR;
  ph: TPascalString;
begin
  if umlExistsChar(FileName, '*?') then
    begin
      ph := umlGetFilePath(FileName);
      if umlFindFirstFile(FileName, _SR) then
        begin
          repeat
            try
                DeleteFile(umlCombineFileName(ph, _SR.Name).text);
            except
            end;
          until not umlFindNextFile(_SR);
        end;
      umlFindClose(_SR);
      Result := true;
    end
  else
    begin
      try
          Result := DeleteFile(FileName.text);
      except
          Result := False;
      end;
      if Result and _VerifyCheck then
          Result := not umlFileExists(FileName)
      else
          Result := true;
    end;
end;

function umlDeleteFile(const FileName: TPascalString): Boolean;
begin
  Result := umlDeleteFile(FileName, False);
end;

function umlCopyFile(const SourFile, DestFile: TPascalString): Boolean;
var
  _SH, _DH: TCoreClassFileStream;
begin
  Result := False;
  _SH := nil;
  _DH := nil;
  try
    if not umlFileExists(SourFile) then
        exit;
    if umlMultipleMatch(true, ExpandFileName(SourFile.text), ExpandFileName(DestFile.text)) then
        exit;
    _SH := TCoreClassFileStream.Create(SourFile.text, fmOpenRead or fmShareDenyNone);
    _DH := TCoreClassFileStream.Create(DestFile.text, fmCreate);
    Result := _DH.CopyFrom(_SH, _SH.Size) = _SH.Size;
    DisposeObject(_SH);
    DisposeObject(_DH);
    umlSetFileTime(DestFile, umlGetFileTime(SourFile));
  except
    if _SH <> nil then
        DisposeObject(_SH);
    if _DH <> nil then
        DisposeObject(_DH);
  end;
end;

function umlRenameFile(const OldName, NewName: TPascalString): Boolean;
begin
  Result := RenameFile(OldName.text, NewName.text);
end;

procedure umlSetLength(var sVal: TPascalString; Len: Integer);
begin
  sVal.Len := Len;
end;

procedure umlSetLength(var sVal: U_Bytes; Len: Integer);
begin
  SetLength(sVal, Len);
end;

procedure umlSetLength(var sVal: TArrayPascalString; Len: Integer);
begin
  SetLength(sVal, Len);
end;

function umlGetLength(const sVal: TPascalString): Integer;
begin
  Result := sVal.Len;
end;

function umlGetLength(const sVal: U_Bytes): Integer;
begin
  Result := length(sVal);
end;

function umlGetLength(const sVal: TArrayPascalString): Integer;
begin
  Result := length(sVal);
end;

function umlUpperCase(const Str: TPascalString): TPascalString;
begin
  Result := UpperCase(Str.text);
end;

function umlLowerCase(const Str: TPascalString): TPascalString;
begin
  Result := LowerCase(Str.text);
end;

function umlCopyStr(const sVal: TPascalString; MainPosition, LastPosition: Integer): TPascalString;
begin
  Result := sVal.GetString(MainPosition, LastPosition);
end;

function umlSameText(const s1, s2: TPascalString): Boolean;
begin
  Result := s1.Same(s2);
end;

function umlDeleteChar(const SText, Ch: TPascalString): TPascalString;
var
  i: Integer;
begin
  Result := '';
  if SText.Len > 0 then
    for i := 1 to SText.Len do
      if not CharIn(SText[i], Ch) then
          Result.Append(SText[i]);
end;

function umlDeleteChar(const SText: TPascalString; const SomeChars: array of SystemChar): TPascalString;
var
  i: Integer;
begin
  Result := '';
  if SText.Len > 0 then
    for i := 1 to SText.Len do
      if not CharIn(SText[i], SomeChars) then
          Result.Append(SText[i]);
end;

function umlDeleteChar(const SText: TPascalString; const SomeCharsets: TOrdChars): TPascalString; overload;
var
  i: Integer;
begin
  Result := '';
  if SText.Len > 0 then
    for i := 1 to SText.Len do
      if not CharIn(SText[i], SomeCharsets) then
          Result.Append(SText[i]);
end;

function umlGetNumberCharInText(const n: TPascalString): TPascalString;
var
  i: Integer;
begin
  Result := '';
  i := 0;
  if n.Len = 0 then
      exit;

  while i <= n.Len do
    begin
      if (not CharIn(n[i], c0to9)) then
        begin
          if (Result.Len = 0) then
              inc(i)
          else
              exit;
        end
      else
        begin
          Result.Append(n[i]);
          inc(i);
        end;
    end;
end;

function umlMatchChar(CharValue: U_Char; cVal: P_String): Boolean;
begin
  Result := CharIn(CharValue, cVal);
end;

function umlMatchChar(CharValue: U_Char; cVal: TPascalString): Boolean;
begin
  Result := CharIn(CharValue, @cVal);
end;

function umlExistsChar(StrValue: TPascalString; cVal: TPascalString): Boolean;
var
  c: SystemChar;
begin
  Result := true;
  for c in StrValue.buff do
    if CharIn(c, @cVal) then
        exit;
  Result := False;
end;

function umlTrimChar(const s, trim_s: TPascalString): TPascalString;
var
  L, bp, EP: Integer;
begin
  Result := '';
  L := s.Len;
  if L > 0 then
    begin
      bp := 1;
      while CharIn(s[bp], @trim_s) do
        begin
          inc(bp);
          if (bp > L) then
            begin
              Result := '';
              exit;
            end;
        end;
      if bp > L then
          Result := ''
      else
        begin
          EP := L;

          while CharIn(s[EP], @trim_s) do
            begin
              dec(EP);
              if (EP < 1) then
                begin
                  Result := '';
                  exit;
                end;
            end;
          Result := s.GetString(bp, EP + 1);
        end;
    end;
end;

function umlGetFirstStr(const sVal, trim_s: TPascalString): TPascalString;
var
  umlGetFirstName_PrevPos, umlGetFirstName_Pos: Integer;
begin
  Result := sVal;
  if Result.Len <= 1 then
    begin
      exit;
    end;
  umlGetFirstName_Pos := 1;
  while umlMatchChar(Result[umlGetFirstName_Pos], @trim_s) do
    begin
      if umlGetFirstName_Pos = Result.Len then
          exit;
      inc(umlGetFirstName_Pos);
    end;
  umlGetFirstName_PrevPos := umlGetFirstName_Pos;
  while not umlMatchChar(Result[umlGetFirstName_Pos], @trim_s) do
    begin
      if umlGetFirstName_Pos = Result.Len then
        begin
          Result := umlCopyStr(Result, umlGetFirstName_PrevPos, umlGetFirstName_Pos + 1);
          exit;
        end;
      inc(umlGetFirstName_Pos);
    end;
  Result := umlCopyStr(Result, umlGetFirstName_PrevPos, umlGetFirstName_Pos);
end;

function umlGetLastStr(const sVal, trim_s: TPascalString): TPascalString;
var
  umlGetLastName_PrevPos, umlGetLastName_Pos: Integer;
begin
  Result := sVal;
  umlGetLastName_Pos := Result.Len;
  if umlGetLastName_Pos <= 1 then
    begin
      exit;
    end;
  while umlMatchChar(Result[umlGetLastName_Pos], @trim_s) do
    begin
      if umlGetLastName_Pos = 1 then
          exit;
      dec(umlGetLastName_Pos);
    end;
  umlGetLastName_PrevPos := umlGetLastName_Pos;
  while not umlMatchChar(Result[umlGetLastName_Pos], @trim_s) do
    begin
      if umlGetLastName_Pos = 1 then
        begin
          Result := umlCopyStr(Result, umlGetLastName_Pos, umlGetLastName_PrevPos + 1);
          exit;
        end;
      dec(umlGetLastName_Pos);
    end;
  Result := umlCopyStr(Result, umlGetLastName_Pos + 1, umlGetLastName_PrevPos + 1);
end;

function umlDeleteFirstStr(const sVal, trim_s: TPascalString): TPascalString;
var
  umlMaskFirstName_Pos: Integer;
begin
  Result := sVal;
  if Result.Len <= 1 then
    begin
      Result := '';
      exit;
    end;
  umlMaskFirstName_Pos := 1;
  while umlMatchChar(Result[umlMaskFirstName_Pos], @trim_s) do
    begin
      if umlMaskFirstName_Pos = Result.Len then
        begin
          Result := '';
          exit;
        end;
      inc(umlMaskFirstName_Pos);
    end;
  while not umlMatchChar(Result[umlMaskFirstName_Pos], @trim_s) do
    begin
      if umlMaskFirstName_Pos = Result.Len then
        begin
          Result := '';
          exit;
        end;
      inc(umlMaskFirstName_Pos);
    end;
  while umlMatchChar(Result[umlMaskFirstName_Pos], @trim_s) do
    begin
      if umlMaskFirstName_Pos = Result.Len then
        begin
          Result := '';
          exit;
        end;
      inc(umlMaskFirstName_Pos);
    end;
  Result := umlCopyStr(Result, umlMaskFirstName_Pos, Result.Len + 1);
end;

function umlDeleteLastStr(const sVal, trim_s: TPascalString): TPascalString;
var
  umlMaskLastName_Pos: Integer;
begin
  Result := sVal;
  umlMaskLastName_Pos := Result.Len;
  if umlMaskLastName_Pos <= 1 then
    begin
      Result := '';
      exit;
    end;
  while umlMatchChar(Result[umlMaskLastName_Pos], @trim_s) do
    begin
      if umlMaskLastName_Pos = 1 then
        begin
          Result := '';
          exit;
        end;
      dec(umlMaskLastName_Pos);
    end;
  while not umlMatchChar(Result[umlMaskLastName_Pos], @trim_s) do
    begin
      if umlMaskLastName_Pos = 1 then
        begin
          Result := '';
          exit;
        end;
      dec(umlMaskLastName_Pos);
    end;
  while umlMatchChar(Result[umlMaskLastName_Pos], @trim_s) do
    begin
      if umlMaskLastName_Pos = 1 then
        begin
          Result := '';
          exit;
        end;
      dec(umlMaskLastName_Pos);
    end;
  umlSetLength(Result, umlMaskLastName_Pos);
end;

function umlGetIndexStrCount(const sVal, trim_s: TPascalString): Integer;
var
  Str: TPascalString;
  APos: Integer;
begin
  Str := sVal;
  Result := 0;
  if Str.Len = 0 then
      exit;
  APos := 1;
  while true do
    begin
      while umlMatchChar(Str[APos], @trim_s) do
        begin
          if APos >= Str.Len then
              exit;
          inc(APos);
        end;
      inc(Result);
      while not umlMatchChar(Str[APos], @trim_s) do
        begin
          if APos >= Str.Len then
              exit;
          inc(APos);
        end;
    end;
end;

function umlGetIndexStr(const sVal: TPascalString; trim_s: TPascalString; index: Integer): TPascalString;
var
  umlGetIndexName_Repeat: Integer;
begin
  case index of
    - 1:
      begin
        Result := '';
        exit;
      end;
    0, 1:
      begin
        Result := umlGetFirstStr(sVal, trim_s);
        exit;
      end;
  end;
  if index >= umlGetIndexStrCount(sVal, trim_s) then
    begin
      Result := umlGetLastStr(sVal, trim_s);
      exit;
    end;
  Result := sVal;
  for umlGetIndexName_Repeat := 2 to index do
    begin
      Result := umlDeleteFirstStr(Result, trim_s);
    end;
  Result := umlGetFirstStr(Result, trim_s);
end;

procedure umlGetSplitArray(const sour: TPascalString; var dest: TArrayPascalString; const splitC: TPascalString);
var
  i, idxCount: Integer;
  SText: TPascalString;
begin
  SText := sour;
  idxCount := umlGetIndexStrCount(SText, splitC);
  if (idxCount = 0) and (sour.Len > 0) then
    begin
      SetLength(dest, 1);
      dest[0] := sour;
    end
  else
    begin
      SetLength(dest, idxCount);
      i := low(dest);
      while i < idxCount do
        begin
          dest[i] := umlGetFirstStr(SText, splitC);
          SText := umlDeleteFirstStr(SText, splitC);
          inc(i);
        end;
    end;
end;

procedure umlGetSplitArray(const sour: TPascalString; var dest: U_StringArray; const splitC: TPascalString);
var
  i, idxCount: Integer;
  SText: TPascalString;
begin
  SText := sour;
  idxCount := umlGetIndexStrCount(SText, splitC);
  if (idxCount = 0) and (sour.Len > 0) then
    begin
      SetLength(dest, 1);
      dest[0] := sour;
    end
  else
    begin
      SetLength(dest, idxCount);
      i := low(dest);
      while i < idxCount do
        begin
          dest[i] := umlGetFirstStr(SText, splitC);
          SText := umlDeleteFirstStr(SText, splitC);
          inc(i);
        end;
    end;
end;

function ArrayStringToText(var ary: TArrayPascalString; const splitC: TPascalString): TPascalString;
var
  i: Integer;
begin
  Result := '';
  for i := low(ary) to high(ary) do
    if i < high(ary) then
        Result := Result + ary[i] + splitC
    else
        Result := Result + ary[i];
end;

function umlStringsToText(lst: TCoreClassStrings; const splitC: TPascalString): TPascalString;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to lst.Count - 1 do
    if i > 0 then
        Result.Append(splitC.text + lst[i])
    else
        Result := lst[i];
end;

function umlStringsToText(lst: TListPascalString; const splitC: TPascalString): TPascalString;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to lst.Count - 1 do
    if i > 0 then
        Result.Append(splitC.text + lst[i])
    else
        Result := lst[i];
end;

function umlGetFirstStr_Discontinuity(const sVal, trim_s: TPascalString): TPascalString;
var
  umlGetFirstName_PrevPos, umlGetFirstName_Pos: Integer;
begin
  Result := sVal;
  if Result.Len <= 1 then
      exit;
  umlGetFirstName_Pos := 1;
  if umlMatchChar(Result[umlGetFirstName_Pos], @trim_s) then
    begin
      inc(umlGetFirstName_Pos);
      umlGetFirstName_PrevPos := umlGetFirstName_Pos;
    end
  else
    begin
      umlGetFirstName_PrevPos := umlGetFirstName_Pos;
      while not umlMatchChar(Result[umlGetFirstName_Pos], @trim_s) do
        begin
          if umlGetFirstName_Pos = Result.Len then
            begin
              Result := umlCopyStr(Result, umlGetFirstName_PrevPos, umlGetFirstName_Pos + 1);
              exit;
            end;
          inc(umlGetFirstName_Pos);
        end;
    end;
  Result := umlCopyStr(Result, umlGetFirstName_PrevPos, umlGetFirstName_Pos);
end;

function umlDeleteFirstStr_Discontinuity(const sVal, trim_s: TPascalString): TPascalString;
var
  umlMaskFirstName_Pos: Integer;
begin
  Result := sVal;
  if Result.Len <= 1 then
    begin
      Result := '';
      exit;
    end;
  umlMaskFirstName_Pos := 1;
  while not umlMatchChar(Result[umlMaskFirstName_Pos], @trim_s) do
    begin
      if umlMaskFirstName_Pos = Result.Len then
        begin
          Result := '';
          exit;
        end;
      inc(umlMaskFirstName_Pos);
    end;
  if umlMatchChar(Result[umlMaskFirstName_Pos], @trim_s) then
      inc(umlMaskFirstName_Pos);
  Result := umlCopyStr(Result, umlMaskFirstName_Pos, Result.Len + 1);
end;

function umlGetLastStr_Discontinuity(const sVal, trim_s: TPascalString): TPascalString;
var
  umlGetLastName_PrevPos, umlGetLastName_Pos: Integer;
begin
  Result := sVal;
  umlGetLastName_Pos := Result.Len;
  if umlGetLastName_Pos <= 1 then
      exit;
  if Result[umlGetLastName_Pos] = trim_s then
      dec(umlGetLastName_Pos);
  umlGetLastName_PrevPos := umlGetLastName_Pos;
  while not umlMatchChar(Result[umlGetLastName_Pos], @trim_s) do
    begin
      if umlGetLastName_Pos = 1 then
        begin
          Result := umlCopyStr(Result, umlGetLastName_Pos, umlGetLastName_PrevPos + 1);
          exit;
        end;
      dec(umlGetLastName_Pos);
    end;
  Result := umlCopyStr(Result, umlGetLastName_Pos + 1, umlGetLastName_PrevPos + 1);
end;

function umlDeleteLastStr_Discontinuity(const sVal, trim_s: TPascalString): TPascalString;
var
  umlMaskLastName_Pos: Integer;
begin
  Result := sVal;
  umlMaskLastName_Pos := Result.Len;
  if umlMaskLastName_Pos <= 1 then
    begin
      Result := '';
      exit;
    end;
  if umlMatchChar(Result[umlMaskLastName_Pos], @trim_s) then
      dec(umlMaskLastName_Pos);
  while not umlMatchChar(Result[umlMaskLastName_Pos], @trim_s) do
    begin
      if umlMaskLastName_Pos = 1 then
        begin
          Result := '';
          exit;
        end;
      dec(umlMaskLastName_Pos);
    end;
  umlSetLength(Result, umlMaskLastName_Pos);
end;

function umlGetIndexStrCount_Discontinuity(const sVal, trim_s: TPascalString): Integer;
var
  Str: TPascalString;
  APos: Integer;
begin
  Str := sVal;
  Result := 0;
  if Str.Len = 0 then
      exit;
  APos := 1;
  Result := 1;
  while true do
    begin
      while not umlMatchChar(Str[APos], @trim_s) do
        begin
          if APos = Str.Len then
              exit;
          inc(APos);
        end;
      inc(Result);
      if APos = Str.Len then
          exit;
      inc(APos);
    end;
end;

function umlGetIndexStr_Discontinuity(const sVal: TPascalString; trim_s: TPascalString; index: Integer): TPascalString;
var
  umlGetIndexName_Repeat: Integer;
begin
  case index of
    - 1:
      begin
        Result := '';
        exit;
      end;
    0, 1:
      begin
        Result := umlGetFirstStr_Discontinuity(sVal, trim_s);
        exit;
      end;
  end;
  if index >= umlGetIndexStrCount_Discontinuity(sVal, trim_s) then
    begin
      Result := umlGetLastStr_Discontinuity(sVal, trim_s);
      exit;
    end;
  Result := sVal;
  for umlGetIndexName_Repeat := 2 to index do
      Result := umlDeleteFirstStr_Discontinuity(Result, trim_s);
  Result := umlGetFirstStr_Discontinuity(Result, trim_s);
end;

function umlGetFirstTextPos(const s: TPascalString; const TextArry: TArrayPascalString; var OutText: TPascalString): Integer;
var
  i, j: Integer;
begin
  Result := -1;
  for i := 1 to s.Len do
    begin
      for j := low(TextArry) to high(TextArry) do
        begin
          if s.ComparePos(i, @TextArry[j]) then
            begin
              OutText := TextArry[j];
              Result := i;
              exit;
            end;
        end;
    end;
end;

function umlDeleteText(const sour: TPascalString; const bToken, eToken: TArrayPascalString; ANeedBegin, ANeedEnd: Boolean): TPascalString;
var
  ABeginPos, AEndPos: Integer;
  ABeginText, AEndText, ANewStr: TPascalString;
begin
  Result := sour;
  if sour.Len > 0 then
    begin
      ABeginPos := umlGetFirstTextPos(sour, bToken, ABeginText);
      if ABeginPos > 0 then
          ANewStr := umlCopyStr(sour, ABeginPos + ABeginText.Len, sour.Len + 1)
      else if ANeedBegin then
          exit
      else
          ANewStr := sour;

      AEndPos := umlGetFirstTextPos(ANewStr, eToken, AEndText);
      if AEndPos > 0 then
          ANewStr := umlCopyStr(ANewStr, (AEndPos + AEndText.Len), ANewStr.Len + 1)
      else if ANeedEnd then
          exit
      else
          ANewStr := '';

      if ABeginPos > 0 then
        begin
          if AEndPos > 0 then
              Result := umlCopyStr(sour, 0, ABeginPos - 1) + umlDeleteText(ANewStr, bToken, eToken, ANeedBegin, ANeedEnd)
          else
              Result := umlCopyStr(sour, 0, ABeginPos - 1) + ANewStr;
        end
      else if AEndPos > 0 then
          Result := ANewStr;
    end;
end;

function umlGetTextContent(const sour: TPascalString; const bToken, eToken: TArrayPascalString): TPascalString;
var
  ABeginPos, AEndPos: Integer;
  ABeginText, AEndText, ANewStr: TPascalString;
begin
  Result := '';
  if sour.Len > 0 then
    begin
      ABeginPos := umlGetFirstTextPos(sour, bToken, ABeginText);
      if ABeginPos > 0 then
          ANewStr := umlCopyStr(sour, ABeginPos + ABeginText.Len, sour.Len + 1)
      else
          ANewStr := sour;

      AEndPos := umlGetFirstTextPos(ANewStr, eToken, AEndText);
      if AEndPos > 0 then
          Result := umlCopyStr(ANewStr, 0, AEndPos - 1)
      else
          Result := ANewStr;
    end;
end;

function umlGetNumTextType(const s: TPascalString): TTextType;
type
  TValSym = (vsSymSub, vsSymAdd, vsSymAddSub, vsSymDollar, vsDot, vsDotBeforNum, vsDotAfterNum, vsNum, vsAtoF, vsE, vsUnknow);
var
  cnt: array [TValSym] of Integer;
  n: TPascalString;
  v: TValSym;
  c: SystemChar;
  i: Integer;
begin
  n := umlTrimSpace(s);
  if n.Same('true') or n.Same('false') then
      exit(ntBool);

  for v := low(TValSym) to high(TValSym) do
      cnt[v] := 0;

  for i := 1 to n.Len do
    begin
      c := n[i];
      if CharIn(c, [c0to9]) then
        begin
          inc(cnt[vsNum]);
          if cnt[vsDot] > 0 then
              inc(cnt[vsDotAfterNum]);
        end
      else if CharIn(c, [cLoAtoF, cHiAtoF]) then
        begin
          inc(cnt[vsAtoF]);
          if CharIn(c, 'eE') then
              inc(cnt[vsE]);
        end
      else if c = '.' then
        begin
          inc(cnt[vsDot]);
          cnt[vsDotBeforNum] := cnt[vsNum];
        end
      else if CharIn(c, '-') then
        begin
          inc(cnt[vsSymSub]);
          inc(cnt[vsSymAddSub]);
        end
      else if CharIn(c, '+') then
        begin
          inc(cnt[vsSymAdd]);
          inc(cnt[vsSymAddSub]);
        end
      else if CharIn(c, '$') and (i = 1) then
        begin
          inc(cnt[vsSymDollar]);
          if i <> 1 then
              exit(ntUnknow);
        end
      else
          exit(ntUnknow);
    end;

  if cnt[vsDot] > 1 then
      exit(ntUnknow);
  if cnt[vsSymDollar] > 1 then
      exit(ntUnknow);
  if (cnt[vsSymDollar] = 0) and (cnt[vsNum] = 0) then
      exit(ntUnknow);
  if (cnt[vsSymAdd] > 1) and (cnt[vsE] = 0) and (cnt[vsSymDollar] = 0) then
      exit(ntUnknow);

  if (cnt[vsSymDollar] = 0) and
    ((cnt[vsDot] = 1) or ((cnt[vsE] = 1) and ((cnt[vsSymAddSub] >= 1) and (cnt[vsSymDollar] = 0)))) then
    begin
      if cnt[vsSymDollar] > 0 then
          exit(ntUnknow);
      if (cnt[vsAtoF] <> cnt[vsE]) then
          exit(ntUnknow);

      if cnt[vsE] = 1 then
        begin
          Result := ntDouble
        end
      else if ((cnt[vsDotBeforNum] > 0)) and (cnt[vsDotAfterNum] > 0) then
        begin
          if cnt[vsDotAfterNum] < 5 then
              Result := ntCurrency
          else if cnt[vsNum] > 7 then
              Result := ntDouble
          else
              Result := ntSingle;
        end
      else
          exit(ntUnknow);
    end
  else
    begin
      if cnt[vsSymDollar] = 1 then
        begin
          if cnt[vsSymSub] > 0 then
            begin
              if cnt[vsNum] + cnt[vsAtoF] = 0 then
                  Result := ntUnknow
              else if cnt[vsNum] + cnt[vsAtoF] < 2 then
                  Result := ntShortInt
              else if cnt[vsNum] + cnt[vsAtoF] < 4 then
                  Result := ntSmallInt
              else if cnt[vsNum] + cnt[vsAtoF] < 7 then
                  Result := ntInt
              else if cnt[vsNum] + cnt[vsAtoF] < 13 then
                  Result := ntInt64
              else
                  Result := ntUnknow;
            end
          else
            begin
              if cnt[vsNum] + cnt[vsAtoF] = 0 then
                  Result := ntUnknow
              else if cnt[vsNum] + cnt[vsAtoF] < 3 then
                  Result := ntByte
              else if cnt[vsNum] + cnt[vsAtoF] < 5 then
                  Result := ntWord
              else if cnt[vsNum] + cnt[vsAtoF] < 8 then
                  Result := ntUInt
              else if cnt[vsNum] + cnt[vsAtoF] < 14 then
                  Result := ntUInt64
              else
                  Result := ntUnknow;
            end;
        end
      else if cnt[vsAtoF] > 0 then
          exit(ntUnknow)
      else if cnt[vsSymSub] > 0 then
        begin
          if cnt[vsNum] = 0 then
              Result := ntUnknow
          else if cnt[vsNum] < 3 then
              Result := ntShortInt
          else if cnt[vsNum] < 5 then
              Result := ntSmallInt
          else if cnt[vsNum] < 8 then
              Result := ntInt
          else if cnt[vsNum] < 15 then
              Result := ntInt64
          else
              Result := ntUnknow;
        end
      else
        begin
          if cnt[vsNum] = 0 then
              Result := ntUnknow
          else if cnt[vsNum] < 3 then
              Result := ntByte
          else if cnt[vsNum] < 5 then
              Result := ntWord
          else if cnt[vsNum] < 8 then
              Result := ntUInt
          else if cnt[vsNum] < 16 then
              Result := ntUInt64
          else
              Result := ntUnknow;
        end;
    end;
end;

function umlIsHex(const sVal: TPascalString): Boolean;
begin
  Result := umlGetNumTextType(sVal) in
    [ntInt, ntInt64, ntUInt64, ntWord, ntByte, ntSmallInt, ntShortInt, ntUInt];
end;

function umlIsNumber(const sVal: TPascalString): Boolean;
begin
  Result := umlGetNumTextType(sVal) <> ntUnknow;
end;

function umlIsIntNumber(const sVal: TPascalString): Boolean;
begin
  Result := umlGetNumTextType(sVal) in
    [ntInt, ntInt64, ntUInt64, ntWord, ntByte, ntSmallInt, ntShortInt, ntUInt];
end;

function umlIsFloatNumber(const sVal: TPascalString): Boolean;
begin
  Result := umlGetNumTextType(sVal) in [ntSingle, ntDouble, ntCurrency];
end;

function umlIsBool(const sVal: TPascalString): Boolean;
begin
  Result := umlGetNumTextType(sVal) = ntBool;
end;

function umlNumberCount(const sVal: TPascalString): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to sVal.Len do
    if CharIn(sVal[i], [c0to9]) then
        inc(Result);
end;

function umlPercentageToFloat(OriginMax, OriginMin, ProcressParameter: Double): Double;
begin
  Result := (ProcressParameter - OriginMin) * 100.0 / (OriginMax - OriginMin);
end;

function umlPercentageToInt(OriginParameter, ProcressParameter: Integer): Integer;
begin
  if OriginParameter = 0 then
      Result := 0
  else
      Result := Round((ProcressParameter * 100.0) / OriginParameter);
end;

function umlPercentageToStr(OriginParameter, ProcressParameter: Integer): TPascalString;
begin
  Result := IntToStr(umlPercentageToInt(OriginParameter, ProcressParameter)) + '%';
end;

function umlSmartSizeToStr(Size: Int64): TPascalString;
begin
  if Size < 1 shl 10 then
      Result := Format('%d', [Size])
  else if Size < 1 shl 20 then
      Result := Format('%fKb', [Size / (1 shl 10)])
  else if Size < 1 shl 30 then
      Result := Format('%fM', [Size / (1 shl 20)])
  else
      Result := Format('%fG', [Size / (1 shl 30)])
end;

function umlIntToStr(Parameter: Single): TPascalString;
begin
  Result := IntToStr(Round(Parameter));
end;

function umlIntToStr(Parameter: Double): TPascalString;
begin
  Result := IntToStr(Round(Parameter));
end;

function umlIntToStr(Parameter: Int64): TPascalString;
begin
  Result := IntToStr(Parameter);
end;

function umlPointerToStr(param: Pointer): TPascalString;
begin
  Result := '0x' + IntToHex(nativeUInt(param), SizeOf(Pointer) * 2);
end;

function umlSizeToStr(Parameter: Int64): TPascalString;
begin
  try
      Result := umlSmartSizeToStr(Parameter);
  except
      Result := IntToStr(Parameter) + ' B';
  end;
end;

function umlDateTimeToStr(t: TDateTime): TPascalString;
begin
  Result := DateTimeToStr(t);
end;

function umlTimeTickToStr(const t: TTimeTick): TPascalString;
var
  tmp, d, h, m, s: TTimeTick;
begin
{$IFDEF FPC}
  d := t div C_Tick_Day;
  tmp := t mod C_Tick_Day;

  h := tmp div C_Tick_Hour;
  tmp := t mod C_Tick_Hour;

  m := tmp div C_Tick_Minute;
  tmp := t mod C_Tick_Minute;

  s := tmp div C_Tick_Second;
  tmp := t mod C_Tick_Second;
{$ELSE FPC}
  DivMod(t, C_Tick_Day, d, tmp);
  DivMod(tmp, C_Tick_Hour, h, tmp);
  DivMod(tmp, C_Tick_Minute, m, tmp);
  DivMod(tmp, C_Tick_Second, s, tmp);
{$ENDIF FPC}
  Result := '';
  if (d > 0) then
      Result.Append(IntToStr(d) + ' day ');
  if (Result.Len > 0) or (h > 0) then
      Result.Append(IntToStr(h) + ' hour ');
  if (Result.Len > 0) or (m > 0) then
      Result.Append(IntToStr(m) + ' minute ');

  if (Result.Len > 0) or (s > 0) then
      Result.Append(PFormat('%2.2f', [s + tmp / 1000]))
  else
      Result.Append('0');
end;

function umlTimeToStr(t: TDateTime): TPascalString;
begin
  Result := TimeToStr(t);
end;

function umlDateToStr(t: TDateTime): TPascalString;
begin
  Result := DateToStr(t);
end;

function umlFloatToStr(const f: Extended): TPascalString;
begin
  Result := FloatToStr(f);
end;

function umlShortFloatToStr(const f: Extended): TPascalString;
begin
  Result := Format('%f', [f]);
end;

function umlStrToInt(const _V: TPascalString): Integer;
begin
  Result := umlStrToInt(_V, 0);
end;

function umlStrToInt(const _V: TPascalString; _Def: Integer): Integer;
begin
  if umlIsNumber(_V) then
    begin
      try
          Result := StrToInt(_V.text);
      except
          Result := _Def;
      end;
    end
  else
      Result := _Def;
end;

function umlStrToInt64(const _V: TPascalString; _Def: Int64): Int64;
begin
  if umlIsNumber(_V) then
    begin
      try
          Result := StrToInt64(_V.text);
      except
          Result := _Def;
      end;
    end
  else
      Result := _Def;
end;

function umlStrToFloat(const _V: TPascalString; _Def: Double): Double;
begin
  if umlIsNumber(_V) then
    begin
      try
          Result := StrToFloat(_V.text);
      except
          Result := _Def;
      end;
    end
  else
      Result := _Def;
end;

function umlStrToFloat(const _V: TPascalString): Double;
begin
  Result := umlStrToFloat(_V, 0);
end;

function umlMultipleMatch(IgnoreCase: Boolean; const SourceStr, TargetStr, umlMultipleString, umlMultipleCharacter: TPascalString): Boolean;
label Character_Label, MChar_Label, MString_Label;
var
  UpperCaseSourceStr, UpperCaseTargetStr, SwapStr: TPascalString;
  SourceChar, TargetChar, SwapChar: U_Char;
  SourceIndex, TargetIndex, SwapIndex, SourceLength, TargetLength, SwapLength: Integer;
begin
  SourceLength := SourceStr.Len;
  if SourceLength = 0 then
    begin
      Result := true;
      exit;
    end;

  TargetLength := TargetStr.Len;
  if TargetLength = 0 then
    begin
      Result := False;
      exit;
    end;

  if IgnoreCase then
    begin
      UpperCaseSourceStr := umlUpperCase(SourceStr);
      UpperCaseTargetStr := umlUpperCase(TargetStr);
    end
  else
    begin
      UpperCaseSourceStr := SourceStr;
      UpperCaseTargetStr := TargetStr;
    end;

  if (not umlExistsChar(SourceStr, umlMultipleCharacter)) and (not umlExistsChar(SourceStr, umlMultipleString)) then
    begin
      Result := (SourceLength = TargetLength) and (UpperCaseSourceStr = UpperCaseTargetStr);
      exit;
    end;
  if SourceLength = 1 then
    begin
      if umlMatchChar(UpperCaseSourceStr[1], @umlMultipleString) then
          Result := true
      else
          Result := False;
      exit;
    end;
  SourceIndex := 1;
  TargetIndex := 1;
  SourceChar := UpperCaseSourceStr[SourceIndex];
  TargetChar := UpperCaseTargetStr[TargetIndex];

Character_Label:
  while (SourceChar = TargetChar) and (not umlMatchChar(SourceChar, @umlMultipleCharacter)) and (not umlMatchChar(SourceChar, @umlMultipleString)) do
    begin
      if SourceIndex = SourceLength then
        begin
          if TargetIndex = TargetLength then
            begin
              Result := true;
              exit;
            end;
          Result := False;
          exit;
        end;
      if TargetIndex = TargetLength then
        begin
          SourceIndex := SourceIndex + 1;
          if SourceIndex = SourceLength then
            begin
              SourceChar := UpperCaseSourceStr[SourceIndex];
              Result := umlMatchChar(SourceChar, @umlMultipleString) or umlMatchChar(SourceChar, @umlMultipleCharacter);
              exit;
            end;
          Result := False;
          exit;
        end;
      SourceIndex := SourceIndex + 1;
      TargetIndex := TargetIndex + 1;
      SourceChar := UpperCaseSourceStr[SourceIndex];
      TargetChar := UpperCaseTargetStr[TargetIndex];
    end;

MChar_Label:
  while umlMatchChar(SourceChar, @umlMultipleCharacter) do
    begin
      if SourceIndex = SourceLength then
        begin
          if TargetIndex = TargetLength then
            begin
              Result := true;
              exit;
            end;
          Result := False;
          exit;
        end;
      if TargetIndex = TargetLength then
        begin
          SourceIndex := SourceIndex + 1;
          SourceChar := UpperCaseSourceStr[SourceIndex];
          if (SourceIndex = SourceLength) and ((umlMatchChar(SourceChar, @umlMultipleString)) or (umlMatchChar(SourceChar, @umlMultipleCharacter))) then
            begin
              Result := true;
              exit;
            end;
          Result := False;
          exit;
        end;
      SourceIndex := SourceIndex + 1;
      TargetIndex := TargetIndex + 1;
      SourceChar := UpperCaseSourceStr[SourceIndex];
      TargetChar := UpperCaseTargetStr[TargetIndex];
    end;

MString_Label:
  if umlMatchChar(SourceChar, @umlMultipleString) then
    begin
      if SourceIndex = SourceLength then
        begin
          Result := true;
          exit;
        end;
      SourceIndex := SourceIndex + 1;
      SourceChar := UpperCaseSourceStr[SourceIndex];

      while (umlMatchChar(SourceChar, @umlMultipleString)) or (umlMatchChar(SourceChar, @umlMultipleCharacter)) do
        begin
          if SourceIndex = SourceLength then
            begin
              Result := true;
              exit;
            end;
          SourceIndex := SourceIndex + 1;
          SourceChar := UpperCaseSourceStr[SourceIndex];
          while umlMatchChar(SourceChar, @umlMultipleCharacter) do
            begin
              if SourceIndex = SourceLength then
                begin
                  Result := true;
                  exit;
                end;
              SourceIndex := SourceIndex + 1;
              SourceChar := UpperCaseSourceStr[SourceIndex];
            end;
        end;
      SwapStr := umlCopyStr(UpperCaseSourceStr, SourceIndex, SourceLength + 1);
      SwapLength := SwapStr.Len;
      if SwapLength = 0 then
        begin
          Result := (UpperCaseSourceStr[SourceIndex] = umlMultipleString);
          exit;
        end;
      SwapIndex := 1;
      SwapChar := SwapStr[SwapIndex];
      while (not umlMatchChar(SwapChar, @umlMultipleCharacter)) and (not umlMatchChar(SwapChar, @umlMultipleString)) and (SwapIndex < SwapLength) do
        begin
          SwapIndex := SwapIndex + 1;
          SwapChar := SwapStr[SwapIndex];
        end;
      if (umlMatchChar(SwapChar, @umlMultipleCharacter)) or (umlMatchChar(SwapChar, @umlMultipleString)) then
          SwapStr := umlCopyStr(SwapStr, 1, SwapIndex)
      else
        begin
          SwapStr := umlCopyStr(SwapStr, 1, SwapIndex + 1);
          if SwapStr = '' then
            begin
              Result := False;
              exit;
            end;
          SwapLength := SwapStr.Len;
          SwapIndex := 1;
          SwapChar := SwapStr[SwapLength];
          TargetChar := UpperCaseTargetStr[TargetLength];
          while SwapChar = TargetChar do
            begin
              if SwapIndex = SwapLength then
                begin
                  Result := true;
                  exit;
                end;
              if SwapIndex = TargetLength then
                begin
                  Result := False;
                  exit;
                end;
              SwapChar := SwapStr[(SwapLength) - SwapIndex];
              TargetChar := UpperCaseTargetStr[(TargetLength) - SwapIndex];
              SwapIndex := SwapIndex + 1;
            end;
          Result := False;
          exit;
        end;
      SwapChar := SwapStr[1];
      SwapIndex := 1;
      SwapLength := SwapStr.Len;
      while SwapIndex <= SwapLength do
        begin
          if (TargetIndex - 1) + SwapIndex > TargetLength then
            begin
              Result := False;
              exit;
            end;
          SwapChar := SwapStr[SwapIndex];
          TargetChar := UpperCaseTargetStr[(TargetIndex - 1) + SwapIndex];
          while SwapChar <> TargetChar do
            begin
              if (TargetIndex + SwapLength) > TargetLength then
                begin
                  Result := False;
                  exit;
                end;
              TargetIndex := TargetIndex + 1;
              SwapIndex := 1;
              SwapChar := SwapStr[SwapIndex];
              TargetChar := UpperCaseTargetStr[(TargetIndex - 1) + SwapIndex];
            end;
          SwapIndex := SwapIndex + 1;
        end;
      TargetIndex := (TargetIndex - 1) + SwapLength;
      SourceIndex := (SourceIndex - 1) + SwapLength;
      TargetChar := SwapChar;
      SourceChar := SwapChar;
    end;
  if SourceChar = TargetChar then
      goto Character_Label
  else if umlMatchChar(SourceChar, @umlMultipleCharacter) then
      goto MChar_Label
  else if umlMatchChar(SourceChar, @umlMultipleString) then
      goto MString_Label
  else
      Result := False;
end;

function umlMultipleMatch(IgnoreCase: Boolean; const SourceStr, TargetStr: TPascalString): Boolean;
begin
  if (SourceStr.Len > 0) and (SourceStr.text <> '*') then
      Result := umlMultipleMatch(IgnoreCase, SourceStr, TargetStr, '*', '?')
  else
      Result := true;
end;

function umlMultipleMatch(const SourceStr, TargetStr: TPascalString): Boolean;
var
  fi: TArrayPascalString;
begin
  if (SourceStr.Len > 0) and (SourceStr.text <> '*') then
    begin
      umlGetSplitArray(SourceStr, fi, ';');
      Result := umlMultipleMatch(fi, TargetStr);
    end
  else
      Result := true;
end;

function umlMultipleMatch(const ValueCheck: array of TPascalString; const Value: TPascalString): Boolean;
var
  i: Integer;
begin
  Result := False;
  if Value.Len > 0 then
    begin
      if high(ValueCheck) >= 0 then
        begin
          Result := False;
          for i := low(ValueCheck) to high(ValueCheck) do
            begin
              Result := umlMultipleMatch(true, ValueCheck[i], Value);
              if Result then
                  exit;
            end;
        end
      else
          Result := true;
    end;
end;

function umlSearchMatch(const SourceStr, TargetStr: TPascalString): Boolean;
var
  fi: TArrayPascalString;
begin
  if (SourceStr.Len > 0) and (SourceStr.text <> '*') then
    begin
      umlGetSplitArray(SourceStr, fi, ';,');
      Result := umlSearchMatch(fi, TargetStr);
    end
  else
      Result := true;
end;

function umlSearchMatch(const ValueCheck: TArrayPascalString; Value: TPascalString): Boolean;
var
  i: Integer;
begin
  Result := False;
  if umlGetLength(Value) > 0 then
    begin
      if high(ValueCheck) >= 0 then
        begin
          Result := False;
          for i := low(ValueCheck) to high(ValueCheck) do
            begin
              Result := (Value.GetPos(ValueCheck[i]) > 0) or (umlMultipleMatch(true, ValueCheck[i], Value));
              if Result then
                  exit;
            end;
        end
      else
          Result := true;
    end;
end;

function umlMatchFileInfo(const exp_, sour_, dest_: TPascalString): Boolean;
const
  prefix = '<prefix>';
  postfix = '<postfix>';
var
  sour, dest, dest_prefix, dest_postfix, n: TPascalString;
begin
  sour := umlGetFileName(sour_);
  dest := umlGetFileName(dest_);
  dest_prefix := umlChangeFileExt(dest, '');
  dest_postfix := umlGetFileExt(dest);
  n := umlStringReplace(exp_, prefix, dest_prefix, true);
  n := umlStringReplace(n, postfix, dest_postfix, true);
  Result := umlMultipleMatch(n, sour);
  sour := '';
  dest := '';
  dest_prefix := '';
  dest_postfix := '';
  n := '';
end;

function umlDecodeTimeToStr(NowDateTime: TDateTime): TPascalString;
var
  Year, Month, Day: Word;
  Hour, Min, Sec, MSec: Word;
begin
  DecodeDate(NowDateTime, Year, Month, Day);
  DecodeTime(NowDateTime, Hour, Min, Sec, MSec);
  Result := IntToHex(Year, 4) + IntToHex(Month, 2) +
    IntToHex(Day, 2) + IntToHex(Hour, 1) + IntToHex(Min, 2) +
    IntToHex(Sec, 2) + IntToHex(MSec, 3);
end;

function umlMakeRanName: TPascalString;
type
  TRanData = packed record
    Year, Month, Day: Word;
    Hour, Min, Sec, MSec: Word;
  end;
var
  d: TDateTime;
  r: TRanData;
begin
  d := umlNow();
  with r do
    begin
      DecodeDate(d, Year, Month, Day);
      DecodeTime(d, Hour, Min, Sec, MSec);
    end;
  Result := umlMD5String(@r, SizeOf(TRanData));
end;

function umlStringReplace(const s, OldPattern, NewPattern: TPascalString; IgnoreCase: Boolean): TPascalString;
var
  f: TReplaceFlags;
begin
  f := [rfReplaceAll];
  if IgnoreCase then
      f := f + [rfIgnoreCase];
  Result.text := StringReplace(s.text, OldPattern.text, NewPattern.text, f);
end;

function umlReplaceString(const s, OldPattern, NewPattern: TPascalString; IgnoreCase: Boolean): TPascalString;
begin
  Result := umlStringReplace(s, OldPattern, NewPattern, IgnoreCase);
end;

function umlCharReplace(const s: TPascalString; OldPattern, NewPattern: U_Char): TPascalString;
var
  i: Integer;
begin
  Result := s;
  if Result.Len > 0 then
    begin
      for i := 1 to umlGetLength(Result) do
        begin
          if Result[i] = OldPattern then
              Result[i] := NewPattern;
        end;
    end;
end;

function umlReplaceChar(const s: TPascalString; OldPattern, NewPattern: U_Char): TPascalString;
begin
  Result := umlCharReplace(s, OldPattern, NewPattern);
end;

function umlEncodeText2HTML(const psSrc: TPascalString): TPascalString;
var
  i: Integer;
begin
  Result := '';
  if psSrc.Len > 0 then
    begin
      i := 1;
      while i <= psSrc.Len do
        begin
          case psSrc[i] of
            ' ': Result.Append('&nbsp;');
            '<': Result.Append('&lt;');
            '>': Result.Append('&gt;');
            '&': Result.Append('&amp;');
            '"': Result.Append('&quot;');
            #9: Result.Append('&nbsp;&nbsp;&nbsp;&nbsp;');
            #13:
              begin
                if i + 1 <= psSrc.Len then
                  begin
                    if psSrc[i + 1] = #10 then
                        inc(i);
                    Result.Append('<br>');
                  end
                else
                  begin
                    Result.Append('<br>');
                  end;
              end;
            #10:
              begin
                if i + 1 <= psSrc.Len then
                  begin
                    if psSrc[i + 1] = #13 then
                        inc(i);
                    Result.Append('<br>');
                  end
                else
                  begin
                    Result.Append('<br>');
                  end;
              end;
            else
              Result.Append(psSrc[i]);
          end;
          inc(i);
        end;
    end;
end;

function umlURLEncode(const Data: TPascalString): TPascalString;
const
  EncodeSlash = False;
var
  UTF8Src: TBytes;
  i: Integer;
  b: Byte;
begin
  Result := '';
  try
    UTF8Src := Data.Bytes;
    for i := 0 to length(UTF8Src) - 1 do
      begin
        b := UTF8Src[i];
        if ((b >= $41) and (b <= $5A)) or ((b >= $61) and (b <= $7A)) or ((b >= $30) and (b <= $39)) or
          (b = $2D) or (b = $2E) or (b = $5F) or (b = $7E) or (b = $2F) or (b = $3A) then
            Result := Result + SystemChar(b)
        else
            Result := Result + '%' + IntToHex(b, 2);
      end;
  finally
      SetLength(UTF8Src, 0);
  end;
end;

function umlURLDecode(const Data: TPascalString; FormEncoded: Boolean): TPascalString;

  function CombineArry(const Buf1: TBytes; Buf2: Byte): TBytes;
  var
    L: Integer;
  begin
    L := length(Buf1);
    SetLength(Result, L + 1);

    if L > 0 then
        CopyPtr(@Buf1[0], @Result[0], L);

    Result[0 + L] := Buf2;
  end;

  procedure FreeArry(var a: TBytes);
  begin
    SetLength(a, 0);
  end;

var
  i: Integer;
  State: Byte;
  b, BV, B1: Byte;
  DataArry_, UTF8Str_: TBytes;
  tmpBytes: TBytes;
const
  STATE_READ_DATA = 0;
  STATE_READ_PERCENT_ENCODED_BYTE_1 = 1;
  STATE_READ_PERCENT_ENCODED_BYTE_2 = 2;
const
  HexCharsHigh: array [0 .. 15] of Byte = ($30, $31, $32, $33, $34, $35, $36, $37, $38, $39, 65, 66, 67, 68, 69, 70);
begin
  B1 := 0;
  State := STATE_READ_DATA;
  SetLength(UTF8Str_, 0);
  DataArry_ := Data.Bytes;
  for i := 0 to length(DataArry_) - 1 do
    begin
      b := DataArry_[i];
      if State = STATE_READ_DATA then
        begin
          if b = $25 then
              State := STATE_READ_PERCENT_ENCODED_BYTE_1
          else if FormEncoded and (b = $2B) then // + sign
            begin
              tmpBytes := UTF8Str_;
              UTF8Str_ := CombineArry(tmpBytes, Byte($20));
              FreeArry(tmpBytes);
            end
          else
            begin
              tmpBytes := UTF8Str_;
              UTF8Str_ := CombineArry(tmpBytes, Byte(Data[FirstCharPos + i]));
              FreeArry(tmpBytes);
            end;
        end
      else
        if (State = STATE_READ_PERCENT_ENCODED_BYTE_1) or (State = STATE_READ_PERCENT_ENCODED_BYTE_2) then
        begin
          if (b >= 65) and (b <= 70) then
              BV := b - 55
          else if (b >= 97) and (b <= 102) then
              BV := b - 87
          else if (b >= $30) and (b <= $39) then
              BV := b - $30
          else
              raiseInfo('Unexpected character: 0x' + IntToHex(b, 2));
          if State = STATE_READ_PERCENT_ENCODED_BYTE_1 then
            begin
              B1 := BV;
              State := STATE_READ_PERCENT_ENCODED_BYTE_2;
            end
          else
            begin
              b := (B1 shl 4) or BV;

              tmpBytes := UTF8Str_;
              UTF8Str_ := CombineArry(tmpBytes, b);
              FreeArry(tmpBytes);

              State := STATE_READ_DATA;
            end;
        end;
    end;
  Result.Bytes := UTF8Str_;
  FreeArry(UTF8Str_);
  FreeArry(DataArry_);
end;

function B64EstimateEncodedSize(Ctx: TBase64Context; InSize: Integer): Integer;
begin
  Result := ((InSize + 2) div 3) shl 2;

  if (Ctx.EOLSize > 0) and (Ctx.LineSize > 0) then
    begin
      Result := Result + ((Result + Ctx.LineSize - 1) div Ctx.LineSize) * Ctx.EOLSize;

      if not Ctx.TrailingEol then
          Result := Result - Ctx.EOLSize;
    end;
end;

function B64InitializeDecoding(var Ctx: TBase64Context; LiberalMode: Boolean): Boolean;
begin
  Ctx.TailBytes := 0;
  Ctx.EQUCount := 0;
  Ctx.LiberalMode := LiberalMode;

  Result := true;
end;

function B64InitializeEncoding(var Ctx: TBase64Context; LineSize: Integer; fEOL: TBase64EOLMarker; TrailingEol: Boolean): Boolean;
begin

  Result := False;
  Ctx.TailBytes := 0;
  Ctx.LineSize := LineSize;
  Ctx.LineWritten := 0;
  Ctx.EQUCount := 0;
  Ctx.TrailingEol := TrailingEol;
  Ctx.PutFirstEol := False;

  if LineSize < 4 then
      exit;

  case fEOL of
    emCRLF:
      begin
        Ctx.fEOL[0] := $0D;
        Ctx.fEOL[1] := $0A;
        Ctx.EOLSize := 2;
      end;
    emCR:
      begin
        Ctx.fEOL[0] := $0D;
        Ctx.EOLSize := 1;
      end;
    emLF:
      begin
        Ctx.fEOL[0] := $0A;
        Ctx.EOLSize := 1;
      end;
    else
      Ctx.EOLSize := 0;
  end;

  Result := true;
end;

function B64Encode(var Ctx: TBase64Context; buffer: PByte; Size: Integer; OutBuffer: PByte; var OutSize: Integer): Boolean;
var
  EstSize, i, Chunks: Integer;
  PreserveLastEol: Boolean;
begin
  PreserveLastEol := False;

  EstSize := ((Size + Ctx.TailBytes) div 3) shl 2;
  if (Ctx.LineSize > 0) and (Ctx.EOLSize > 0) then
    begin
      if (EstSize > 0) and ((Ctx.LineWritten + EstSize) mod Ctx.LineSize = 0) and
        ((Ctx.TailBytes + Size) mod 3 = 0) then
          PreserveLastEol := true;
      EstSize := EstSize + ((EstSize + Ctx.LineWritten) div Ctx.LineSize) * Ctx.EOLSize;
      if PreserveLastEol then
          EstSize := EstSize - Ctx.EOLSize;
    end;
  if Ctx.PutFirstEol then
      EstSize := EstSize + Ctx.EOLSize;

  if OutSize < EstSize then
    begin
      OutSize := EstSize;
      Result := False;
      exit;
    end;

  OutSize := EstSize;

  if Ctx.PutFirstEol then
    begin
      CopyPtr(@Ctx.fEOL[0], OutBuffer, Ctx.EOLSize);
      inc(OutBuffer, Ctx.EOLSize);
      Ctx.PutFirstEol := False;
    end;

  if Size + Ctx.TailBytes < 3 then
    begin
      for i := 0 to Size - 1 do
          Ctx.Tail[Ctx.TailBytes + i] := PBase64ByteArray(buffer)^[i];
      inc(Ctx.TailBytes, Size);
      Result := true;
      exit;
    end;

  if Ctx.TailBytes > 0 then
    begin
      for i := 0 to 2 - Ctx.TailBytes do
          Ctx.Tail[Ctx.TailBytes + i] := PBase64ByteArray(buffer)^[i];

      inc(buffer, 3 - Ctx.TailBytes);
      dec(Size, 3 - Ctx.TailBytes);

      Ctx.TailBytes := 0;

      Ctx.OutBuf[0] := Base64Symbols[Ctx.Tail[0] shr 2];
      Ctx.OutBuf[1] := Base64Symbols[((Ctx.Tail[0] and 3) shl 4) or (Ctx.Tail[1] shr 4)];
      Ctx.OutBuf[2] := Base64Symbols[((Ctx.Tail[1] and $F) shl 2) or (Ctx.Tail[2] shr 6)];
      Ctx.OutBuf[3] := Base64Symbols[Ctx.Tail[2] and $3F];

      if (Ctx.LineSize = 0) or (Ctx.LineWritten + 4 < Ctx.LineSize) then
        begin
          CopyPtr(@Ctx.OutBuf[0], OutBuffer, 4);
          inc(OutBuffer, 4);
          inc(Ctx.LineWritten, 4);
        end
      else
        begin
          i := Ctx.LineSize - Ctx.LineWritten;
          CopyPtr(@Ctx.OutBuf[0], OutBuffer, i);
          inc(OutBuffer, i);
          if (Size > 0) or (i < 4) or (not PreserveLastEol) then
            begin
              CopyPtr(@Ctx.fEOL[0], OutBuffer, Ctx.EOLSize);
              inc(OutBuffer, Ctx.EOLSize);
            end;
          CopyPtr(@Ctx.OutBuf[i], OutBuffer, 4 - i);
          inc(OutBuffer, 4 - i);
          Ctx.LineWritten := 4 - i;
        end;
    end;

  while Size >= 3 do
    begin
      if Ctx.LineSize > 0 then
        begin
          Chunks := (Ctx.LineSize - Ctx.LineWritten) shr 2;
          if Chunks > Size div 3 then
              Chunks := Size div 3;
        end
      else
          Chunks := Size div 3;

      for i := 0 to Chunks - 1 do
        begin
          OutBuffer^ := Base64Symbols[PBase64ByteArray(buffer)^[0] shr 2];
          inc(OutBuffer);
          PByte(OutBuffer)^ := Base64Symbols[((PBase64ByteArray(buffer)^[0] and 3) shl 4) or (PBase64ByteArray(buffer)^[1] shr 4)];
          inc(OutBuffer);
          PByte(OutBuffer)^ := Base64Symbols[((PBase64ByteArray(buffer)^[1] and $F) shl 2) or (PBase64ByteArray(buffer)^[2] shr 6)];
          inc(OutBuffer);
          PByte(OutBuffer)^ := Base64Symbols[PBase64ByteArray(buffer)^[2] and $3F];
          inc(OutBuffer);
          inc(buffer, 3);
        end;

      dec(Size, 3 * Chunks);

      if Ctx.LineSize > 0 then
        begin
          inc(Ctx.LineWritten, Chunks shl 2);

          if (Size >= 3) and (Ctx.LineSize - Ctx.LineWritten > 0) then
            begin
              Ctx.OutBuf[0] := Base64Symbols[PBase64ByteArray(buffer)^[0] shr 2];
              Ctx.OutBuf[1] := Base64Symbols[((PBase64ByteArray(buffer)^[0] and 3) shl 4) or (PBase64ByteArray(buffer)^[1] shr 4)];
              Ctx.OutBuf[2] := Base64Symbols[((PBase64ByteArray(buffer)^[1] and $F) shl 2) or (PBase64ByteArray(buffer)^[2] shr 6)];
              Ctx.OutBuf[3] := Base64Symbols[PBase64ByteArray(buffer)^[2] and $3F];
              inc(buffer, 3);

              dec(Size, 3);

              i := Ctx.LineSize - Ctx.LineWritten;

              CopyPtr(@Ctx.OutBuf[0], OutBuffer, i);
              inc(OutBuffer, i);
              if (Ctx.EOLSize > 0) and ((i < 4) or (Size > 0) or (not PreserveLastEol)) then
                begin
                  CopyPtr(@Ctx.fEOL[0], OutBuffer, Ctx.EOLSize);
                  inc(OutBuffer, Ctx.EOLSize);
                end;

              CopyPtr(@Ctx.OutBuf[i], OutBuffer, 4 - i);
              inc(OutBuffer, 4 - i);

              Ctx.LineWritten := 4 - i;
            end
          else
            if Ctx.LineWritten = Ctx.LineSize then
            begin
              Ctx.LineWritten := 0;
              if (Ctx.EOLSize > 0) and ((Size > 0) or (not PreserveLastEol)) then
                begin
                  CopyPtr(@Ctx.fEOL[0], OutBuffer, Ctx.EOLSize);
                  inc(OutBuffer, Ctx.EOLSize);
                end;
            end;
        end;
    end;

  if Size > 0 then
    begin
      CopyPtr(buffer, @Ctx.Tail[0], Size);
      Ctx.TailBytes := Size;
    end
  else
    if PreserveLastEol then
      Ctx.PutFirstEol := true;

  Result := true;
end;

function B64Decode(var Ctx: TBase64Context; buffer: PByte; Size: Integer; OutBuffer: PByte; var OutSize: Integer): Boolean;
var
  i, EstSize, EQUCount: Integer;
  BufPtr: PByte;
  c: Byte;
begin
  if Size = 0 then
    begin
      Result := true;
      OutSize := 0;
      exit;
    end;

  EQUCount := Ctx.EQUCount;
  EstSize := Ctx.TailBytes;
  BufPtr := buffer;

  for i := 0 to Size - 1 do
    begin
      c := Base64Values[PByte(BufPtr)^];
      if c < 64 then
          inc(EstSize)
      else
        if c = $FF then
        begin
          if not Ctx.LiberalMode then
            begin
              Result := False;
              OutSize := 0;
              exit;
            end;
        end
      else
        if c = $FD then
        begin
          if EQUCount > 1 then
            begin
              Result := False;
              OutSize := 0;
              exit;
            end;
          inc(EQUCount);
        end;
      inc(BufPtr);
    end;

  EstSize := (EstSize shr 2) * 3;
  if OutSize < EstSize then
    begin
      OutSize := EstSize;
      Result := False;
      exit;
    end;

  Ctx.EQUCount := EQUCount;
  OutSize := EstSize;

  while Size > 0 do
    begin
      c := Base64Values[PByte(buffer)^];
      if c < 64 then
        begin
          Ctx.Tail[Ctx.TailBytes] := c;
          inc(Ctx.TailBytes);

          if Ctx.TailBytes = 4 then
            begin
              PByte(OutBuffer)^ := (Ctx.Tail[0] shl 2) or (Ctx.Tail[1] shr 4);
              inc(OutBuffer);

              PByte(OutBuffer)^ := ((Ctx.Tail[1] and $F) shl 4) or (Ctx.Tail[2] shr 2);
              inc(OutBuffer);

              PByte(OutBuffer)^ := ((Ctx.Tail[2] and $3) shl 6) or Ctx.Tail[3];
              inc(OutBuffer);

              Ctx.TailBytes := 0;
            end;
        end;
      inc(buffer);
      dec(Size);
    end;
  Result := true;
end;

function B64FinalizeEncoding(var Ctx: TBase64Context; OutBuffer: PByte; var OutSize: Integer): Boolean;
var
  EstSize: Integer;
begin
  if Ctx.TailBytes > 0 then
      EstSize := 4
  else
      EstSize := 0;

  if Ctx.TrailingEol then
      EstSize := EstSize + Ctx.EOLSize;

  if OutSize < EstSize then
    begin
      OutSize := EstSize;
      Result := False;
      exit;
    end;

  OutSize := EstSize;

  if Ctx.TailBytes = 0 then
    begin
      { writing trailing EOL }
      Result := true;
      if (Ctx.EOLSize > 0) and Ctx.TrailingEol then
        begin
          OutSize := Ctx.EOLSize;
          CopyPtr(@Ctx.fEOL[0], OutBuffer, Ctx.EOLSize);
        end;
      exit;
    end;

  if Ctx.TailBytes = 1 then
    begin
      PBase64ByteArray(OutBuffer)^[0] := Base64Symbols[Ctx.Tail[0] shr 2];
      PBase64ByteArray(OutBuffer)^[1] := Base64Symbols[((Ctx.Tail[0] and 3) shl 4)];
      PBase64ByteArray(OutBuffer)^[2] := $3D; // '='
      PBase64ByteArray(OutBuffer)^[3] := $3D; // '='
    end
  else if Ctx.TailBytes = 2 then
    begin
      PBase64ByteArray(OutBuffer)^[0] := Base64Symbols[Ctx.Tail[0] shr 2];
      PBase64ByteArray(OutBuffer)^[1] := Base64Symbols[((Ctx.Tail[0] and 3) shl 4) or (Ctx.Tail[1] shr 4)];
      PBase64ByteArray(OutBuffer)^[2] := Base64Symbols[((Ctx.Tail[1] and $F) shl 2)];
      PBase64ByteArray(OutBuffer)^[3] := $3D; // '='
    end;

  if (Ctx.EOLSize > 0) and (Ctx.TrailingEol) then
      CopyPtr(@Ctx.fEOL[0], @PBase64ByteArray(OutBuffer)^[4], Ctx.EOLSize);

  Result := true;
end;

function B64FinalizeDecoding(var Ctx: TBase64Context; OutBuffer: PByte; var OutSize: Integer): Boolean;
begin
  if (Ctx.EQUCount = 0) then
    begin
      OutSize := 0;
      Result := Ctx.TailBytes = 0;
      exit;
    end
  else
    if (Ctx.EQUCount = 1) then
    begin
      if Ctx.TailBytes <> 3 then
        begin
          Result := False;
          OutSize := 0;
          exit;
        end;

      if OutSize < 2 then
        begin
          OutSize := 2;
          Result := False;
          exit;
        end;

      PByte(OutBuffer)^ := (Ctx.Tail[0] shl 2) or (Ctx.Tail[1] shr 4);
      inc(OutBuffer);
      PByte(OutBuffer)^ := ((Ctx.Tail[1] and $F) shl 4) or (Ctx.Tail[2] shr 2);
      OutSize := 2;
      Result := true;
    end
  else if (Ctx.EQUCount = 2) then
    begin
      if Ctx.TailBytes <> 2 then
        begin
          Result := False;
          OutSize := 0;
          exit;
        end;

      if OutSize < 1 then
        begin
          OutSize := 1;
          Result := False;
          exit;
        end;

      PByte(OutBuffer)^ := (Ctx.Tail[0] shl 2) or (Ctx.Tail[1] shr 4);

      OutSize := 1;
      Result := true;
    end
  else
    begin
      Result := False;
      OutSize := 0;
    end;
end;

function umlBase64Encode(InBuffer: PByte; InSize: Integer; OutBuffer: PByte; var OutSize: Integer; WrapLines: Boolean): Boolean;
var
  Ctx: TBase64Context;
  TmpSize: Integer;
begin
  if WrapLines then
      B64InitializeEncoding(Ctx, 64, emCRLF, False)
  else
      B64InitializeEncoding(Ctx, 0, emNone, False);

  TmpSize := B64EstimateEncodedSize(Ctx, InSize);

  if (OutSize < TmpSize) then
    begin
      OutSize := TmpSize;
      Result := False;
      exit;
    end;

  TmpSize := OutSize;
  B64Encode(Ctx, InBuffer, InSize, OutBuffer, TmpSize);
  OutSize := OutSize - TmpSize;
  B64FinalizeEncoding(Ctx, PByte(nativeUInt(OutBuffer) + UInt32(TmpSize)), OutSize);
  OutSize := OutSize + TmpSize;

  Result := true;
end;

function umlBase64Decode(InBuffer: PByte; InSize: Integer; OutBuffer: PByte; var OutSize: Integer; LiberalMode: Boolean): Integer;
var
  i, TmpSize: Integer;
  ExtraSyms: Integer;
  Ctx: TBase64Context;
begin
  ExtraSyms := 0;
  try
    for i := 0 to InSize - 1 do
      if (PBase64ByteArray(InBuffer)^[i] in [$0D, $0A, $0]) then // some buggy software products insert 0x00 characters to BASE64 they produce
          inc(ExtraSyms);
  except
  end;

  if not LiberalMode then
    begin
      if ((InSize - ExtraSyms) and $3) <> 0 then
        begin
          Result := BASE64_DECODE_WRONG_DATA_SIZE;
          OutSize := 0;
          exit;
        end;
    end;

  TmpSize := ((InSize - ExtraSyms) shr 2) * 3;
  if OutSize < TmpSize then
    begin
      Result := BASE64_DECODE_NOT_ENOUGH_SPACE;
      OutSize := TmpSize;
      exit;
    end;

  B64InitializeDecoding(Ctx, LiberalMode);
  TmpSize := OutSize;
  if not B64Decode(Ctx, InBuffer, InSize, OutBuffer, TmpSize) then
    begin
      Result := BASE64_DECODE_INVALID_CHARACTER;
      OutSize := 0;
      exit;
    end;
  OutSize := OutSize - TmpSize;
  if not B64FinalizeDecoding(Ctx, @PBase64ByteArray(OutBuffer)^[TmpSize], OutSize) then
    begin
      Result := BASE64_DECODE_INVALID_CHARACTER;
      OutSize := 0;
      exit;
    end;
  OutSize := OutSize + TmpSize;
  Result := BASE64_DECODE_OK;
end;

procedure umlBase64EncodeBytes(var sour, dest: TBytes);
var
  Size: Integer;
begin
  if length(sour) = 0 then
      exit;

  Size := 0;
  SetLength(dest, 0);
  umlBase64Encode(@sour[0], length(sour), nil, Size, False);
  SetLength(dest, Size);
  umlBase64Encode(@sour[0], length(sour), @dest[0], Size, False);
  SetLength(dest, Size);
end;

procedure umlBase64DecodeBytes(var sour, dest: TBytes);
var
  Size: Integer;
begin
  if length(sour) = 0 then
    begin
      SetLength(dest, 0);
      exit;
    end;

  Size := 0;
  umlBase64Decode(@sour[0], length(sour), nil, Size, true);
  SetLength(dest, Size);
  umlBase64Decode(@sour[0], length(sour), @dest[0], Size, true);
  SetLength(dest, Size);
end;

procedure umlBase64EncodeBytes(var sour: TBytes; var dest: TPascalString);
var
  buff: TBytes;
begin
  umlBase64EncodeBytes(sour, buff);
  dest.Bytes := buff;
end;

procedure umlBase64DecodeBytes(const sour: TPascalString; var dest: TBytes);
var
  buff: TBytes;
begin
  buff := sour.Bytes;
  umlBase64DecodeBytes(buff, dest);
end;

procedure umlDecodeLineBASE64(const buffer: TPascalString; var output: TPascalString);
var
  b, nb: TBytes;
begin
  b := umlBytesOf(buffer);
  umlBase64DecodeBytes(b, nb);
  output := umlStringOf(nb);
end;

procedure umlEncodeLineBASE64(const buffer: TPascalString; var output: TPascalString);
var
  b, nb: TBytes;
begin
  b := umlBytesOf(buffer);
  umlBase64EncodeBytes(b, nb);
  output := umlStringOf(nb);
end;

procedure umlDecodeStreamBASE64(const buffer: TPascalString; output: TCoreClassStream);
var
  b, nb: TBytes;
  bak: Int64;
begin
  b := umlBytesOf(buffer);
  umlBase64DecodeBytes(b, nb);
  bak := output.Position;
  output.WriteBuffer(nb[0], length(nb));
  output.Position := bak;
end;

procedure umlEncodeStreamBASE64(buffer: TCoreClassStream; var output: TPascalString);
var
  b, nb: TBytes;
  bak: Int64;
begin
  bak := buffer.Position;

  buffer.Position := 0;
  SetLength(b, buffer.Size);
  buffer.ReadBuffer(b[0], buffer.Size);
  umlBase64EncodeBytes(b, nb);
  output := umlStringOf(nb);

  buffer.Position := bak;
end;

function umlDivisionBase64Text(const buffer: TPascalString; width: Integer; DivisionAsPascalString: Boolean): TPascalString;
var
  i, n: Integer;
begin
  Result := '';
  n := 0;
  for i := 1 to buffer.Len do
    begin
      if (DivisionAsPascalString) and (n = 0) then
          Result.Append(#39);

      Result.Append(buffer[i]);
      inc(n);
      if n = width then
        begin
          if DivisionAsPascalString then
              Result.Append(#39 + '+' + #13#10)
          else
              Result.Append(#13#10);
          n := 0;
        end;
    end;
  if DivisionAsPascalString then
      Result.Append(#39);
end;

function umlTestBase64(const text: TPascalString): Boolean;
var
  sour, dest: TBytes;
begin
  sour := text.Bytes;
  SetLength(dest, 0);
  try
      umlBase64DecodeBytes(sour, dest);
  except
  end;
  Result := length(dest) > 0;
  if Result then
      SetLength(dest, 0);
end;

procedure umlTransformMD5(var Accu; var Buf); inline;
  function ROL(const x: Cardinal; const n: Byte): Cardinal; inline;
  begin
    Result := (x shl n) or (x shr (32 - n))
  end;

  function FF(const a, b, c, d, x: Cardinal; const s: Byte; const AC: Cardinal): Cardinal; inline;
  begin
    Result := ROL(a + x + AC + (b and c or not b and d), s) + b
  end;

  function GG(const a, b, c, d, x: Cardinal; const s: Byte; const AC: Cardinal): Cardinal; inline;
  begin
    Result := ROL(a + x + AC + (b and d or c and not d), s) + b
  end;

  function HH(const a, b, c, d, x: Cardinal; const s: Byte; const AC: Cardinal): Cardinal; inline;
  begin
    Result := ROL(a + x + AC + (b xor c xor d), s) + b
  end;

  function II(const a, b, c, d, x: Cardinal; const s: Byte; const AC: Cardinal): Cardinal; inline;
  begin
    Result := ROL(a + x + AC + (c xor (b or not d)), s) + b
  end;

type
  TDigestCardinal = array [0 .. 3] of Cardinal;
  TCardinalBuf = array [0 .. 15] of Cardinal;
var
  a, b, c, d: Cardinal;
begin
  a := TDigestCardinal(Accu)[0];
  b := TDigestCardinal(Accu)[1];
  c := TDigestCardinal(Accu)[2];
  d := TDigestCardinal(Accu)[3];

  a := FF(a, b, c, d, TCardinalBuf(Buf)[0], 7, $D76AA478);   { 1 }
  d := FF(d, a, b, c, TCardinalBuf(Buf)[1], 12, $E8C7B756);  { 2 }
  c := FF(c, d, a, b, TCardinalBuf(Buf)[2], 17, $242070DB);  { 3 }
  b := FF(b, c, d, a, TCardinalBuf(Buf)[3], 22, $C1BDCEEE);  { 4 }
  a := FF(a, b, c, d, TCardinalBuf(Buf)[4], 7, $F57C0FAF);   { 5 }
  d := FF(d, a, b, c, TCardinalBuf(Buf)[5], 12, $4787C62A);  { 6 }
  c := FF(c, d, a, b, TCardinalBuf(Buf)[6], 17, $A8304613);  { 7 }
  b := FF(b, c, d, a, TCardinalBuf(Buf)[7], 22, $FD469501);  { 8 }
  a := FF(a, b, c, d, TCardinalBuf(Buf)[8], 7, $698098D8);   { 9 }
  d := FF(d, a, b, c, TCardinalBuf(Buf)[9], 12, $8B44F7AF);  { 10 }
  c := FF(c, d, a, b, TCardinalBuf(Buf)[10], 17, $FFFF5BB1); { 11 }
  b := FF(b, c, d, a, TCardinalBuf(Buf)[11], 22, $895CD7BE); { 12 }
  a := FF(a, b, c, d, TCardinalBuf(Buf)[12], 7, $6B901122);  { 13 }
  d := FF(d, a, b, c, TCardinalBuf(Buf)[13], 12, $FD987193); { 14 }
  c := FF(c, d, a, b, TCardinalBuf(Buf)[14], 17, $A679438E); { 15 }
  b := FF(b, c, d, a, TCardinalBuf(Buf)[15], 22, $49B40821); { 16 }
  a := GG(a, b, c, d, TCardinalBuf(Buf)[1], 5, $F61E2562);   { 17 }
  d := GG(d, a, b, c, TCardinalBuf(Buf)[6], 9, $C040B340);   { 18 }
  c := GG(c, d, a, b, TCardinalBuf(Buf)[11], 14, $265E5A51); { 19 }
  b := GG(b, c, d, a, TCardinalBuf(Buf)[0], 20, $E9B6C7AA);  { 20 }
  a := GG(a, b, c, d, TCardinalBuf(Buf)[5], 5, $D62F105D);   { 21 }
  d := GG(d, a, b, c, TCardinalBuf(Buf)[10], 9, $02441453);  { 22 }
  c := GG(c, d, a, b, TCardinalBuf(Buf)[15], 14, $D8A1E681); { 23 }
  b := GG(b, c, d, a, TCardinalBuf(Buf)[4], 20, $E7D3FBC8);  { 24 }
  a := GG(a, b, c, d, TCardinalBuf(Buf)[9], 5, $21E1CDE6);   { 25 }
  d := GG(d, a, b, c, TCardinalBuf(Buf)[14], 9, $C33707D6);  { 26 }
  c := GG(c, d, a, b, TCardinalBuf(Buf)[3], 14, $F4D50D87);  { 27 }
  b := GG(b, c, d, a, TCardinalBuf(Buf)[8], 20, $455A14ED);  { 28 }
  a := GG(a, b, c, d, TCardinalBuf(Buf)[13], 5, $A9E3E905);  { 29 }
  d := GG(d, a, b, c, TCardinalBuf(Buf)[2], 9, $FCEFA3F8);   { 30 }
  c := GG(c, d, a, b, TCardinalBuf(Buf)[7], 14, $676F02D9);  { 31 }
  b := GG(b, c, d, a, TCardinalBuf(Buf)[12], 20, $8D2A4C8A); { 32 }
  a := HH(a, b, c, d, TCardinalBuf(Buf)[5], 4, $FFFA3942);   { 33 }
  d := HH(d, a, b, c, TCardinalBuf(Buf)[8], 11, $8771F681);  { 34 }
  c := HH(c, d, a, b, TCardinalBuf(Buf)[11], 16, $6D9D6122); { 35 }
  b := HH(b, c, d, a, TCardinalBuf(Buf)[14], 23, $FDE5380C); { 36 }
  a := HH(a, b, c, d, TCardinalBuf(Buf)[1], 4, $A4BEEA44);   { 37 }
  d := HH(d, a, b, c, TCardinalBuf(Buf)[4], 11, $4BDECFA9);  { 38 }
  c := HH(c, d, a, b, TCardinalBuf(Buf)[7], 16, $F6BB4B60);  { 39 }
  b := HH(b, c, d, a, TCardinalBuf(Buf)[10], 23, $BEBFBC70); { 40 }
  a := HH(a, b, c, d, TCardinalBuf(Buf)[13], 4, $289B7EC6);  { 41 }
  d := HH(d, a, b, c, TCardinalBuf(Buf)[0], 11, $EAA127FA);  { 42 }
  c := HH(c, d, a, b, TCardinalBuf(Buf)[3], 16, $D4EF3085);  { 43 }
  b := HH(b, c, d, a, TCardinalBuf(Buf)[6], 23, $04881D05);  { 44 }
  a := HH(a, b, c, d, TCardinalBuf(Buf)[9], 4, $D9D4D039);   { 45 }
  d := HH(d, a, b, c, TCardinalBuf(Buf)[12], 11, $E6DB99E5); { 46 }
  c := HH(c, d, a, b, TCardinalBuf(Buf)[15], 16, $1FA27CF8); { 47 }
  b := HH(b, c, d, a, TCardinalBuf(Buf)[2], 23, $C4AC5665);  { 48 }
  a := II(a, b, c, d, TCardinalBuf(Buf)[0], 6, $F4292244);   { 49 }
  d := II(d, a, b, c, TCardinalBuf(Buf)[7], 10, $432AFF97);  { 50 }
  c := II(c, d, a, b, TCardinalBuf(Buf)[14], 15, $AB9423A7); { 51 }
  b := II(b, c, d, a, TCardinalBuf(Buf)[5], 21, $FC93A039);  { 52 }
  a := II(a, b, c, d, TCardinalBuf(Buf)[12], 6, $655B59C3);  { 53 }
  d := II(d, a, b, c, TCardinalBuf(Buf)[3], 10, $8F0CCC92);  { 54 }
  c := II(c, d, a, b, TCardinalBuf(Buf)[10], 15, $FFEFF47D); { 55 }
  b := II(b, c, d, a, TCardinalBuf(Buf)[1], 21, $85845DD1);  { 56 }
  a := II(a, b, c, d, TCardinalBuf(Buf)[8], 6, $6FA87E4F);   { 57 }
  d := II(d, a, b, c, TCardinalBuf(Buf)[15], 10, $FE2CE6E0); { 58 }
  c := II(c, d, a, b, TCardinalBuf(Buf)[6], 15, $A3014314);  { 59 }
  b := II(b, c, d, a, TCardinalBuf(Buf)[13], 21, $4E0811A1); { 60 }
  a := II(a, b, c, d, TCardinalBuf(Buf)[4], 6, $F7537E82);   { 61 }
  d := II(d, a, b, c, TCardinalBuf(Buf)[11], 10, $BD3AF235); { 62 }
  c := II(c, d, a, b, TCardinalBuf(Buf)[2], 15, $2AD7D2BB);  { 63 }
  b := II(b, c, d, a, TCardinalBuf(Buf)[9], 21, $EB86D391);  { 64 }

  inc(TDigestCardinal(Accu)[0], a);
  inc(TDigestCardinal(Accu)[1], b);
  inc(TDigestCardinal(Accu)[2], c);
  inc(TDigestCardinal(Accu)[3], d)
end;

function umlMD5(const buffPtr: PByte; bufSiz: nativeUInt): TMD5;
{$IF Defined(FastMD5) and Defined(Delphi) and (Defined(WIN32) or Defined(WIN64))}
begin
  Result := FastMD5(buffPtr, bufSiz);
end;
{$ELSE}


var
  Digest: TMD5;
  Lo, Hi: Cardinal;
  p: PByte;
  ChunkIndex: Byte;
  ChunkBuff: array [0 .. 63] of Byte;
begin
  Lo := 0;
  Hi := 0;
  PCardinal(@Digest[0])^ := $67452301;
  PCardinal(@Digest[4])^ := $EFCDAB89;
  PCardinal(@Digest[8])^ := $98BADCFE;
  PCardinal(@Digest[12])^ := $10325476;

  inc(Lo, bufSiz shl 3);
  inc(Hi, bufSiz shr 29);

  p := buffPtr;

  while bufSiz >= $40 do
    begin
      umlTransformMD5(Digest, p^);
      inc(p, $40);
      dec(bufSiz, $40);
    end;
  if bufSiz > 0 then
      CopyPtr(p, @ChunkBuff[0], bufSiz);

  Result := PMD5(@Digest[0])^;
  ChunkBuff[bufSiz] := $80;
  ChunkIndex := bufSiz + 1;
  if ChunkIndex > $38 then
    begin
      if ChunkIndex < $40 then
          FillPtrByte(@ChunkBuff[ChunkIndex], $40 - ChunkIndex, 0);
      umlTransformMD5(Result, ChunkBuff);
      ChunkIndex := 0
    end;
  FillPtrByte(@ChunkBuff[ChunkIndex], $38 - ChunkIndex, 0);
  PCardinal(@ChunkBuff[$38])^ := Lo;
  PCardinal(@ChunkBuff[$3C])^ := Hi;
  umlTransformMD5(Result, ChunkBuff);
end;
{$IFEND}


function umlMD5Char(const buffPtr: PByte; const BuffSize: nativeUInt): TPascalString;
begin
  Result := umlMD5ToStr(umlMD5(buffPtr, BuffSize));
end;

function umlMD5String(const buffPtr: PByte; const BuffSize: nativeUInt): TPascalString;
begin
  Result := umlMD5ToStr(umlMD5(buffPtr, BuffSize));
end;

function umlStreamMD5(stream: TCoreClassStream; StartPos, EndPos: Int64): TMD5;
{$IF Defined(FastMD5) and Defined(Delphi) and (Defined(WIN32) or Defined(WIN64))}
begin
  Result := FastMD5(stream, StartPos, EndPos);
end;
{$ELSE}


const
  deltaSize: Cardinal = $40 * $FFFF;

var
  Digest: TMD5;
  Lo, Hi: Cardinal;
  DeltaBuf: Pointer;
  bufSiz: Int64;
  Rest: Cardinal;
  p: PByte;
  ChunkIndex: Byte;
  ChunkBuff: array [0 .. 63] of Byte;
begin
  if StartPos > EndPos then
      Swap(StartPos, EndPos);
  StartPos := umlClamp(StartPos, 0, stream.Size);
  EndPos := umlClamp(EndPos, 0, stream.Size);
  if EndPos - StartPos <= 0 then
    begin
      Result := umlMD5(nil, 0);
      exit;
    end;
{$IFDEF OptimizationMemoryStreamMD5}
  if stream is TCoreClassMemoryStream then
    begin
      Result := umlMD5(Pointer(nativeUInt(TCoreClassMemoryStream(stream).Memory) + StartPos), EndPos - StartPos);
      exit;
    end;
  if stream is TMemoryStream64 then
    begin
      Result := umlMD5(TMemoryStream64(stream).PositionAsPtr(StartPos), EndPos - StartPos);
      exit;
    end;
{$IFEND}
  //

  Lo := 0;
  Hi := 0;
  PCardinal(@Digest[0])^ := $67452301;
  PCardinal(@Digest[4])^ := $EFCDAB89;
  PCardinal(@Digest[8])^ := $98BADCFE;
  PCardinal(@Digest[12])^ := $10325476;

  bufSiz := EndPos - StartPos;
  Rest := 0;

  inc(Lo, bufSiz shl 3);
  inc(Hi, bufSiz shr 29);

  DeltaBuf := GetMemory(deltaSize);
  stream.Position := StartPos;

  if bufSiz < $40 then
    begin
      stream.read(DeltaBuf^, bufSiz);
      p := DeltaBuf;
    end
  else
    while bufSiz >= $40 do
      begin
        if Rest = 0 then
          begin
            if bufSiz >= deltaSize then
                Rest := deltaSize
            else
                Rest := bufSiz;
            stream.ReadBuffer(DeltaBuf^, Rest);

            p := DeltaBuf;
          end;
        umlTransformMD5(Digest, p^);
        inc(p, $40);
        dec(bufSiz, $40);
        dec(Rest, $40);
      end;

  if bufSiz > 0 then
      CopyPtr(p, @ChunkBuff[0], bufSiz);

  FreeMemory(DeltaBuf);

  Result := PMD5(@Digest[0])^;
  ChunkBuff[bufSiz] := $80;
  ChunkIndex := bufSiz + 1;
  if ChunkIndex > $38 then
    begin
      if ChunkIndex < $40 then
          FillPtrByte(@ChunkBuff[ChunkIndex], $40 - ChunkIndex, 0);
      umlTransformMD5(Result, ChunkBuff);
      ChunkIndex := 0
    end;
  FillPtrByte(@ChunkBuff[ChunkIndex], $38 - ChunkIndex, 0);
  PCardinal(@ChunkBuff[$38])^ := Lo;
  PCardinal(@ChunkBuff[$3C])^ := Hi;
  umlTransformMD5(Result, ChunkBuff);
end;
{$IFEND}


function umlStreamMD5(stream: TCoreClassStream): TMD5;
begin
  if stream.Size <= 0 then
    begin
      Result := NullMD5;
      exit;
    end;
  stream.Position := 0;
  Result := umlStreamMD5(stream, 0, stream.Size);
  stream.Position := 0;
end;

function umlStreamMD5Char(stream: TCoreClassStream): TPascalString;
begin
  Result := umlMD5ToStr(umlStreamMD5(stream));
end;

function umlStreamMD5String(stream: TCoreClassStream): TPascalString;
begin
  Result := umlMD5ToStr(umlStreamMD5(stream));
end;

function umlStringMD5(const Value: TPascalString): TPascalString;
var
  b: TBytes;
begin
  b := umlBytesOf(Value);
  Result := umlMD5ToStr(umlMD5(@b[0], length(b)));
end;

function umlFileMD5(FileName: TPascalString): TMD5;
var
  fs: TCoreClassFileStream;
begin
  try
      fs := TCoreClassFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  except
    Result := NullMD5;
    exit;
  end;
  try
      Result := umlStreamMD5(fs);
  finally
      DisposeObject(fs);
  end;
end;

function umlCombineMD5(const m1: TMD5): TMD5;
begin
  Result := umlMD5(@m1, SizeOf(TMD5));
end;

function umlCombineMD5(const m1, m2: TMD5): TMD5;
var
  buff: array [0 .. 1] of TMD5;
begin
  buff[0] := m1;
  buff[1] := m2;
  Result := umlMD5(@buff[0], SizeOf(TMD5) * 2);
end;

function umlCombineMD5(const m1, m2, m3: TMD5): TMD5;
var
  buff: array [0 .. 2] of TMD5;
begin
  buff[0] := m1;
  buff[1] := m2;
  buff[2] := m3;
  Result := umlMD5(@buff[0], SizeOf(TMD5) * 3);
end;

function umlCombineMD5(const buff: array of TMD5): TMD5;
begin
  Result := umlMD5(@buff[0], length(buff) * SizeOf(TMD5));
end;

function umlMD5ToStr(md5: TMD5): TPascalString;
const
  HexArr: array [0 .. 15] of U_Char = ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f');
var
  i: Integer;
begin
  Result.Len := 32;
  for i := 0 to 15 do
    begin
      Result.buff[i * 2] := HexArr[(md5[i] shr 4) and $0F];
      Result.buff[i * 2 + 1] := HexArr[md5[i] and $0F];
    end;
end;

function umlMD5ToString(md5: TMD5): TPascalString;
const
  HexArr: array [0 .. 15] of U_Char = ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f');
var
  i: Integer;
begin
  Result.Len := 32;
  for i := 0 to 15 do
    begin
      Result.buff[i * 2] := HexArr[(md5[i] shr 4) and $0F];
      Result.buff[i * 2 + 1] := HexArr[md5[i] and $0F];
    end;
end;

function umlMD52String(md5: TMD5): TPascalString;
const
  HexArr: array [0 .. 15] of U_Char = ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f');
var
  i: Integer;
begin
  Result.Len := 32;
  for i := 0 to 15 do
    begin
      Result.buff[i * 2] := HexArr[(md5[i] shr 4) and $0F];
      Result.buff[i * 2 + 1] := HexArr[md5[i] and $0F];
    end;
end;

function umlMD5Compare(const m1, m2: TMD5): Boolean;
begin
  Result := (PUInt64(@m1[0])^ = PUInt64(@m2[0])^) and (PUInt64(@m1[8])^ = PUInt64(@m2[8])^);
end;

function umlCompareMD5(const m1, m2: TMD5): Boolean;
begin
  Result := (PUInt64(@m1[0])^ = PUInt64(@m2[0])^) and (PUInt64(@m1[8])^ = PUInt64(@m2[8])^);
end;

function umlIsNullMD5(m: TMD5): Boolean;
begin
  Result := umlCompareMD5(m, NullMD5);
end;

function umlWasNullMD5(m: TMD5): Boolean;
begin
  Result := umlCompareMD5(m, NullMD5);
end;

function umlCRC16(const Value: PByte; const Count: nativeUInt): Word;
var
  i: nativeUInt;
  p: PByte;
begin
  p := Value;
  Result := 0;
  i := 0;
  while i < Count do
    begin
      Result := (Result shr 8) xor CRC16Table[p^ xor (Result and $00FF)];
      inc(i);
      inc(p);
    end;
end;

function umlStringCRC16(const Value: TPascalString): Word;
var
  b: TBytes;
begin
  b := umlBytesOf(Value);
  Result := umlCRC16(@b[0], length(b));
end;

function umlStreamCRC16(stream: U_Stream; StartPos, EndPos: Int64): Word;
const
  ChunkSize = 1024 * 1024;
  procedure CRC16BUpdate(var crc: Word; const Buf: Pointer; Len: nativeUInt);
  var
    p: PByte;
    i: Integer;
  begin
    p := Buf;
    i := 0;
    while i < Len do
      begin
        crc := (crc shr 8) xor CRC16Table[p^ xor (crc and $00FF)];
        inc(p);
        inc(i);
      end;
  end;

var
  j: nativeUInt;
  Num: nativeUInt;
  Rest: nativeUInt;
  Buf: Pointer;
  FSize: Int64;
begin
  if stream is TCoreClassMemoryStream then
    begin
      Result := umlCRC16(Pointer(nativeUInt(TCoreClassMemoryStream(stream).Memory) + StartPos), EndPos - StartPos);
      exit;
    end;
  if stream is TMemoryStream64 then
    begin
      Result := umlCRC16(TMemoryStream64(stream).PositionAsPtr(StartPos), EndPos - StartPos);
      exit;
    end;
  { Allocate buffer to read file }
  Buf := GetMemory(ChunkSize);
  { Initialize CRC }
  Result := 0;

  { V1.03 calculate how much of the file we are processing }
  FSize := stream.Size;
  if (StartPos >= FSize) then
      StartPos := 0;
  if (EndPos > FSize) or (EndPos = 0) then
      EndPos := FSize;

  { Calculate number of full chunks that will fit into the buffer }
  Num := EndPos div ChunkSize;
  { Calculate remaining bytes }
  Rest := EndPos mod ChunkSize;

  { Set the stream to the beginning of the file }
  stream.Position := StartPos;

  { Process full chunks }
  for j := 0 to Num - 1 do begin
      stream.read(Buf^, ChunkSize);
      CRC16BUpdate(Result, Buf, ChunkSize);
    end;

  { Process remaining bytes }
  if Rest > 0 then begin
      stream.read(Buf^, Rest);
      CRC16BUpdate(Result, Buf, Rest);
    end;

  FreeMem(Buf, ChunkSize);
end;

function umlStreamCRC16(stream: U_Stream): Word;
begin
  stream.Position := 0;
  Result := umlStreamCRC16(stream, 0, stream.Size);
  stream.Position := 0;
end;

function umlCRC32(const Value: PByte; const Count: nativeUInt): Cardinal;
var
  i: nativeUInt;
  p: PByte;
begin
  p := Value;
  Result := $FFFFFFFF;
  i := 0;
  while i < Count do
    begin
      Result := ((Result shr 8) and $00FFFFFF) xor CRC32Table[(Result xor p^) and $000000FF];
      inc(i);
      inc(p);
    end;
  Result := Result xor $FFFFFFFF;
end;

function umlString2CRC32(const Value: TPascalString): Cardinal;
var
  b: TBytes;
begin
  b := umlBytesOf(Value);
  Result := umlCRC32(@b[0], length(b));
end;

function umlStreamCRC32(stream: U_Stream; StartPos, EndPos: Int64): Cardinal;
const
  ChunkSize = 1024 * 1024;

  procedure CRC32BUpdate(var crc: Cardinal; const Buf: Pointer; Len: nativeUInt);
  var
    p: PByte;
    i: Integer;
  begin
    p := Buf;
    i := 0;
    while i < Len do
      begin
        crc := ((crc shr 8) and $00FFFFFF) xor CRC32Table[(crc xor p^) and $000000FF];
        inc(p);
        inc(i);
      end;
  end;

var
  j: nativeUInt;
  Num: nativeUInt;
  Rest: nativeUInt;
  Buf: Pointer;
  FSize: Int64;
begin
  if stream is TCoreClassMemoryStream then
    begin
      Result := umlCRC32(Pointer(nativeUInt(TCoreClassMemoryStream(stream).Memory) + StartPos), EndPos - StartPos);
      exit;
    end;
  if stream is TMemoryStream64 then
    begin
      Result := umlCRC32(TMemoryStream64(stream).PositionAsPtr(StartPos), EndPos - StartPos);
      exit;
    end;

  { Allocate buffer to read file }
  Buf := GetMemory(ChunkSize);

  { Initialize CRC }
  Result := $FFFFFFFF;

  { V1.03 calculate how much of the file we are processing }
  FSize := stream.Size;
  if (StartPos >= FSize) then
      StartPos := 0;
  if (EndPos > FSize) or (EndPos = 0) then
      EndPos := FSize;

  { Calculate number of full chunks that will fit into the buffer }
  Num := EndPos div ChunkSize;
  { Calculate remaining bytes }
  Rest := EndPos mod ChunkSize;

  { Set the stream to the beginning of the file }
  stream.Position := StartPos;

  { Process full chunks }
  for j := 0 to Num - 1 do begin
      stream.read(Buf^, ChunkSize);
      CRC32BUpdate(Result, Buf, ChunkSize);
    end;

  { Process remaining bytes }
  if Rest > 0 then begin
      stream.read(Buf^, Rest);
      CRC32BUpdate(Result, Buf, Rest);
    end;

  FreeMem(Buf, ChunkSize);

  Result := Result xor $FFFFFFFF;
end;

function umlStreamCRC32(stream: U_Stream): Cardinal;
begin
  stream.Position := 0;
  Result := umlStreamCRC32(stream, 0, stream.Size);
  stream.Position := 0;
end;

function umlTrimSpace(const s: TPascalString): TPascalString;
var
  L, bp, EP: Integer;
begin
  Result := '';
  L := s.Len;
  if L > 0 then
    begin
      bp := 1;
      while CharIn(s[bp], [#32, #0]) do
        begin
          inc(bp);
          if (bp > L) then
            begin
              Result := '';
              exit;
            end;
        end;
      if bp > L then
          Result := ''
      else
        begin
          EP := L;

          while CharIn(s[EP], [#32, #0]) do
            begin
              dec(EP);
              if (EP < 1) then
                begin
                  Result := '';
                  exit;
                end;
            end;
          Result := s.GetString(bp, EP + 1);
        end;
    end;
end;

function umlSeparatorText(AText: TPascalString; dest: TCoreClassStrings; SeparatorChar: TPascalString): Integer;
var
  ANewText, ASeparatorText: TPascalString;
begin
  Result := 0;
  if Assigned(dest) then
    begin
      ANewText := AText;
      ASeparatorText := umlGetFirstStr(ANewText, SeparatorChar);
      while (ASeparatorText.Len > 0) and (ANewText.Len > 0) do
        begin
          dest.Add(ASeparatorText.text);
          inc(Result);
          ANewText := umlDeleteFirstStr(ANewText, SeparatorChar);
          ASeparatorText := umlGetFirstStr(ANewText, SeparatorChar);
        end;
    end;
end;

function umlSeparatorText(AText: TPascalString; dest: THashVariantList; SeparatorChar: TPascalString): Integer;
var
  ANewText, ASeparatorText: TPascalString;
begin
  Result := 0;
  if Assigned(dest) then
    begin
      ANewText := AText;
      ASeparatorText := umlGetFirstStr(ANewText, SeparatorChar);
      while (ASeparatorText.Len > 0) and (ANewText.Len > 0) do
        begin
          dest.IncValue(ASeparatorText.text, 1);
          inc(Result);
          ANewText := umlDeleteFirstStr(ANewText, SeparatorChar);
          ASeparatorText := umlGetFirstStr(ANewText, SeparatorChar);
        end;
    end;
end;

function umlSeparatorText(AText: TPascalString; dest: TListPascalString; SeparatorChar: TPascalString): Integer;
var
  ANewText, ASeparatorText: TPascalString;
begin
  Result := 0;
  if Assigned(dest) then
    begin
      ANewText := AText;
      ASeparatorText := umlGetFirstStr(ANewText, SeparatorChar);
      while (ASeparatorText.Len > 0) and (ANewText.Len > 0) do
        begin
          dest.Add(ASeparatorText);
          inc(Result);
          ANewText := umlDeleteFirstStr(ANewText, SeparatorChar);
          ASeparatorText := umlGetFirstStr(ANewText, SeparatorChar);
        end;
    end;
end;

function umlStringsMatchText(OriginValue: TCoreClassStrings; DestValue: TPascalString; IgnoreCase: Boolean): Boolean;
var
  i: Integer;
begin
  Result := False;
  if not Assigned(OriginValue) then
      exit;
  if OriginValue.Count > 0 then
    begin
      for i := 0 to OriginValue.Count - 1 do
        begin
          if umlMultipleMatch(IgnoreCase, OriginValue[i], DestValue) then
            begin
              Result := true;
              exit;
            end;
        end;
    end;
end;

function umlStringsInExists(dest: TListPascalString; SText: TPascalString; IgnoreCase: Boolean): Boolean;
var
  i: Integer;
  ns: TPascalString;
begin
  Result := False;
  if IgnoreCase then
      ns := umlUpperCase(SText)
  else
      ns := SText;
  if Assigned(dest) then
    begin
      if dest.Count > 0 then
        begin
          for i := 0 to dest.Count - 1 do
            begin
              if ((not IgnoreCase) and (SText = dest[i])) or ((IgnoreCase) and (umlSameText(SText, dest[i]))) then
                begin
                  Result := true;
                  exit;
                end;
            end;
        end;
    end;
end;

function umlStringsInExists(dest: TCoreClassStrings; SText: TPascalString; IgnoreCase: Boolean): Boolean;
var
  i: Integer;
  ns: TPascalString;
begin
  Result := False;
  if IgnoreCase then
      ns := umlUpperCase(SText)
  else
      ns := SText;
  if Assigned(dest) then
    begin
      if dest.Count > 0 then
        begin
          for i := 0 to dest.Count - 1 do
            begin
              if ((not IgnoreCase) and (SText = dest[i])) or ((IgnoreCase) and (umlSameText(SText, dest[i]))) then
                begin
                  Result := true;
                  exit;
                end;
            end;
        end;
    end;
end;

function umlStringsInExists(dest: TCoreClassStrings; SText: TPascalString): Boolean;
begin
  Result := umlStringsInExists(dest, SText, true);
end;

function umlTextInStrings(const SText: TPascalString; dest: TListPascalString; IgnoreCase: Boolean): Boolean;
begin
  Result := umlStringsInExists(dest, SText, IgnoreCase);
end;

function umlTextInStrings(const SText: TPascalString; dest: TCoreClassStrings; IgnoreCase: Boolean): Boolean;
begin
  Result := umlStringsInExists(dest, SText, IgnoreCase);
end;

function umlTextInStrings(const SText: TPascalString; dest: TCoreClassStrings): Boolean;
begin
  Result := umlStringsInExists(dest, SText);
end;

function umlAddNewStrTo(SourceStr: TPascalString; dest: TListPascalString; IgnoreCase: Boolean): Boolean;
begin
  Result := not umlStringsInExists(dest, SourceStr, IgnoreCase);
  if Result then
      dest.Append(SourceStr.text);
end;

function umlAddNewStrTo(SourceStr: TPascalString; dest: TCoreClassStrings; IgnoreCase: Boolean): Boolean;
begin
  Result := not umlStringsInExists(dest, SourceStr, IgnoreCase);
  if Result then
      dest.Append(SourceStr.text);
end;

function umlAddNewStrTo(SourceStr: TPascalString; dest: TCoreClassStrings): Boolean;
begin
  Result := not umlStringsInExists(dest, SourceStr, true);
  if Result then
      dest.Append(SourceStr.text);
end;

function umlAddNewStrTo(SourceStr, dest: TCoreClassStrings): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to SourceStr.Count - 1 do
    if umlAddNewStrTo(SourceStr[i], dest) then
        inc(Result);
end;

function umlDeleteStrings(const SText: TPascalString; dest: TCoreClassStrings; IgnoreCase: Boolean): Integer;
var
  i: Integer;
begin
  Result := 0;
  if Assigned(dest) then
    begin
      if dest.Count > 0 then
        begin
          i := 0;
          while i < dest.Count do
            begin
              if ((not IgnoreCase) and (SText = dest[i])) or ((IgnoreCase) and (umlMultipleMatch(IgnoreCase, SText, dest[i]))) then
                begin
                  dest.Delete(i);
                  inc(Result);
                end
              else
                  inc(i);
            end;
        end;
    end;
end;

function umlDeleteStringsNot(const SText: TPascalString; dest: TCoreClassStrings; IgnoreCase: Boolean): Integer;
var
  i: Integer;
begin
  Result := 0;
  if Assigned(dest) then
    begin
      if dest.Count > 0 then
        begin
          i := 0;
          while i < dest.Count do
            begin
              if ((not IgnoreCase) and (SText <> dest[i])) or ((IgnoreCase) and (not umlMultipleMatch(IgnoreCase, SText, dest[i]))) then
                begin
                  dest.Delete(i);
                  inc(Result);
                end
              else
                  inc(i);
            end;
        end;
    end;
end;

function umlMergeStrings(Source, dest: TCoreClassStrings; IgnoreCase: Boolean): Integer;
var
  i: Integer;
begin
  Result := 0;
  if (Source = nil) or (dest = nil) then
      exit;
  if Source.Count > 0 then
    begin
      for i := 0 to Source.Count - 1 do
        begin
          umlAddNewStrTo(Source[i], dest, IgnoreCase);
          inc(Result);
        end;
    end;
end;

function umlMergeStrings(Source, dest: TListPascalString; IgnoreCase: Boolean): Integer;
var
  i: Integer;
begin
  Result := 0;
  if (Source = nil) or (dest = nil) then
      exit;
  if Source.Count > 0 then
    begin
      for i := 0 to Source.Count - 1 do
        begin
          umlAddNewStrTo(Source[i], dest, IgnoreCase);
          inc(Result);
        end;
    end;
end;

function umlConverStrToFileName(const Value: TPascalString): TPascalString;
var
  i: Integer;
begin
  Result := Value;
  for i := 1 to umlGetLength(Result) do
    begin
      if CharIn(Result[i], '":;/\|<>?*%') then
          Result[i] := ' ';
    end;
end;

function umlSplitTextMatch(const SText, Limit, MatchText: TPascalString; IgnoreCase: Boolean): Boolean;
var
  n, t: TPascalString;
begin
  Result := true;
  if MatchText = '' then
      exit;
  n := SText;
  //
  if umlExistsChar(n, Limit) then
    begin
      repeat
        t := umlGetFirstStr(n, Limit);
        if umlMultipleMatch(IgnoreCase, MatchText, t) then
            exit;
        n := umlDeleteFirstStr(n, Limit);
      until n = '';
    end
  else
    begin
      t := n;
      if umlMultipleMatch(IgnoreCase, MatchText, t) then
          exit;
    end;
  //
  Result := False;
end;

function umlSplitTextTrimSpaceMatch(const SText, Limit, MatchText: TPascalString; IgnoreCase: Boolean): Boolean;
var
  n, t: TPascalString;
begin
  Result := true;
  if MatchText = '' then
      exit;
  n := SText;

  if umlExistsChar(n, Limit) then
    begin
      repeat
        t := umlTrimSpace(umlGetFirstStr(n, Limit));
        if umlMultipleMatch(IgnoreCase, MatchText, t) then
            exit;
        n := umlDeleteFirstStr(n, Limit);
      until n = '';
    end
  else
    begin
      t := umlTrimSpace(n);
      if umlMultipleMatch(IgnoreCase, MatchText, t) then
          exit;
    end;

  Result := False;
end;

function umlSplitDeleteText(const SText, Limit, MatchText: TPascalString; IgnoreCase: Boolean): TPascalString;
var
  n, t: TPascalString;
begin
  if (MatchText = '') or (Limit = '') then
    begin
      Result := SText;
      exit;
    end;
  Result := '';
  n := SText;
  //
  if umlExistsChar(n, Limit) then
    begin
      repeat
        t := umlGetFirstStr(n, Limit);
        if not umlMultipleMatch(IgnoreCase, MatchText, t) then
          begin
            if Result <> '' then
                Result := Result + Limit[1] + t
            else
                Result := t;
          end;
        n := umlDeleteFirstStr(n, Limit);
      until n = '';
    end
  else
    begin
      t := n;
      if not umlMultipleMatch(IgnoreCase, MatchText, t) then
          Result := SText;
    end;
end;

function umlSplitTextAsList(const SText, Limit: TPascalString; AsLst: TCoreClassStrings): Boolean;
var
  n, t: TPascalString;
begin
  AsLst.Clear;
  n := SText;
  //
  if umlExistsChar(n, Limit) then
    begin
      repeat
        t := umlGetFirstStr(n, Limit);
        AsLst.Append(t.text);
        n := umlDeleteFirstStr(n, Limit);
      until n = '';
    end
  else
    begin
      t := n;
      if umlGetLength(t) > 0 then
          AsLst.Append(t.text);
    end;
  //
  Result := AsLst.Count > 0;
end;

function umlSplitTextAsListAndTrimSpace(const SText, Limit: TPascalString; AsLst: TCoreClassStrings): Boolean;
var
  n, t: TPascalString;
begin
  AsLst.Clear;
  n := SText;
  //
  if umlExistsChar(n, Limit) then
    begin
      repeat
        t := umlGetFirstStr(n, Limit);
        AsLst.Append(umlTrimSpace(t).text);
        n := umlDeleteFirstStr(n, Limit);
      until n = '';
    end
  else
    begin
      t := n;
      if umlGetLength(t) > 0 then
          AsLst.Append(umlTrimSpace(t).text);
    end;
  //
  Result := AsLst.Count > 0;
end;

function umlListAsSplitText(const List: TCoreClassStrings; Limit: TPascalString): TPascalString;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to List.Count - 1 do
    if Result = '' then
        Result := List[i]
    else
        Result := Result + Limit + List[i];
end;

function umlListAsSplitText(const List: TListPascalString; Limit: TPascalString): TPascalString;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to List.Count - 1 do
    if Result = '' then
        Result := List[i]
    else
        Result.Append(Limit + List[i]);
end;

function umlDivisionText(const buffer: TPascalString; width: Integer; DivisionAsPascalString: Boolean): TPascalString;
var
  i, n: Integer;
begin
  Result := '';
  n := 0;
  for i := 1 to buffer.Len do
    begin
      if (DivisionAsPascalString) and (n = 0) then
          Result.Append(#39);

      Result.Append(buffer[i]);
      inc(n);
      if n = width then
        begin
          if DivisionAsPascalString then
              Result.Append(#39 + '+' + #13#10)
          else
              Result.Append(#13#10);
          n := 0;
        end;
    end;
  if DivisionAsPascalString then
      Result.Append(#39);
end;

function umlUpdateComponentName(const Name: TPascalString): TPascalString;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to umlGetLength(name) do
    if umlGetLength(Result) > 0 then
      begin
        if CharIn(name[i], [c0to9, cLoAtoZ, cHiAtoZ], '-') then
            Result := Result + name[i];
      end
    else if CharIn(name[i], [cLoAtoZ, cHiAtoZ]) then
        Result := Result + name[i];
end;

function umlMakeComponentName(Owner: TCoreClassComponent; RefrenceName: TPascalString): TPascalString;
var
  c: Cardinal;
begin
  c := 1;
  RefrenceName := umlUpdateComponentName(RefrenceName);
  Result := RefrenceName;
  while Owner.FindComponent(Result.text) <> nil do
    begin
      Result := RefrenceName + IntToStr(c);
      inc(c);
    end;
end;

procedure umlReadComponent(stream: TCoreClassStream; comp: TCoreClassComponent);
var
  r: TCoreClassReader;
  needClearName: Boolean;
begin
  r := TCoreClassReader.Create(stream, 4096);
  r.IgnoreChildren := true;
  try
    needClearName := (comp.Name = '');
    r.ReadRootComponent(comp);
    if needClearName then
        comp.Name := '';
  except
  end;
  DisposeObject(r);
end;

procedure umlWriteComponent(stream: TCoreClassStream; comp: TCoreClassComponent);
var
  w: TCoreClassWriter;
begin
  w := TCoreClassWriter.Create(stream, 4096);
  w.IgnoreChildren := true;
  w.WriteDescendent(comp, nil);
  DisposeObject(w);
end;

procedure umlCopyComponentDataTo(comp, copyto: TCoreClassComponent);
var
  ms: TCoreClassMemoryStream;
begin
  if comp.ClassType <> copyto.ClassType then
      exit;
  ms := TCoreClassMemoryStream.Create;
  try
    umlWriteComponent(ms, comp);
    ms.Position := 0;
    umlReadComponent(ms, copyto);
  except
  end;
  DisposeObject(ms);
end;

function umlProcessCycleValue(CurrentVal, DeltaVal, StartVal, OverVal: Single; var EndFlag: Boolean): Single;
  function IfOut(Cur, Delta, dest: Single): Boolean;
  begin
    if Cur > dest then
        Result := Cur - Delta < dest
    else
        Result := Cur + Delta > dest;
  end;

  function GetOutValue(Cur, Delta, dest: Single): Single;
  begin
    if IfOut(Cur, Delta, dest) then
      begin
        if Cur > dest then
            Result := dest - (Cur - Delta)
        else
            Result := Cur + Delta - dest;
      end
    else
        Result := 0;
  end;

  function GetDeltaValue(Cur, Delta, dest: Single): Single;
  begin
    if Cur > dest then
        Result := Cur - Delta
    else
        Result := Cur + Delta;
  end;

begin
  if (DeltaVal > 0) and (StartVal <> OverVal) then
    begin
      if EndFlag then
        begin
          if IfOut(CurrentVal, DeltaVal, OverVal) then
            begin
              EndFlag := False;
              Result := umlProcessCycleValue(OverVal, GetOutValue(CurrentVal, DeltaVal, OverVal), StartVal, OverVal, EndFlag);
            end
          else
              Result := GetDeltaValue(CurrentVal, DeltaVal, OverVal);
        end
      else
        begin
          if IfOut(CurrentVal, DeltaVal, StartVal) then
            begin
              EndFlag := true;
              Result := umlProcessCycleValue(StartVal, GetOutValue(CurrentVal, DeltaVal, StartVal), StartVal, OverVal, EndFlag);
            end
          else
              Result := GetDeltaValue(CurrentVal, DeltaVal, StartVal);
        end
    end
  else
      Result := CurrentVal;
end;

procedure ImportCSV_C(const sour: TArrayPascalString; OnNotify: TCSVSaveCall);
var
  i, j, bp, hc: NativeInt;
  n: TPascalString;
  king, buff: TArrayPascalString;
begin
  // csv head
  bp := -1;
  for i := low(sour) to high(sour) do
    begin
      n := sour[i];
      if n.Len <> 0 then
        begin
          bp := i + 1;
          hc := n.GetCharCount(',') + 1;
          SetLength(buff, hc);
          SetLength(king, hc);

          for j := low(king) to high(king) do
              king[j] := '';
          j := 0;
          while (j < length(king)) and (n.Len > 0) do
            begin
              king[j] := umlGetFirstStr_Discontinuity(n, ',');
              n := umlDeleteFirstStr_Discontinuity(n, ',');
              inc(j);
            end;

          break;
        end;
    end;

  // csv body
  if bp > 0 then
    for i := bp to high(sour) do
      begin
        n := sour[i];
        if n.Len > 0 then
          begin
            for j := low(buff) to high(buff) do
                buff[j] := '';
            j := 0;
            while (j < length(buff)) and (n.Len > 0) do
              begin
                buff[j] := umlGetFirstStr_Discontinuity(n, ',');
                n := umlDeleteFirstStr_Discontinuity(n, ',');
                inc(j);
              end;
            OnNotify(sour[i], king, buff);
          end;
      end;

  SetLength(buff, 0);
  SetLength(king, 0);
  n := '';
end;

procedure CustomImportCSV_C(const OnGetLine: TCSVGetLineCall; OnNotify: TCSVSaveCall);
var
  IsEnd: Boolean;
  i, j, hc: NativeInt;
  n, s: TPascalString;
  king, buff: TArrayPascalString;
begin
  // csv head
  while true do
    begin
      IsEnd := False;
      n := '';
      OnGetLine(n, IsEnd);
      if IsEnd then
          exit;
      if n.L <> 0 then
        begin
          hc := n.GetCharCount(',') + 1;
          SetLength(buff, hc);
          SetLength(king, hc);

          for j := low(king) to high(king) do
              king[j] := '';
          j := 0;
          while (j < length(king)) and (n.Len > 0) do
            begin
              king[j] := umlGetFirstStr_Discontinuity(n, ',');
              n := umlDeleteFirstStr_Discontinuity(n, ',');
              inc(j);
            end;

          break;
        end;
    end;

  // csv body
  while true do
    begin
      IsEnd := False;
      n := '';
      OnGetLine(n, IsEnd);
      if IsEnd then
          exit;
      if n.Len > 0 then
        begin
          s := n;
          for j := low(buff) to high(buff) do
              buff[j] := '';
          j := 0;
          while (j < length(buff)) and (n.Len > 0) do
            begin
              buff[j] := umlGetFirstStr_Discontinuity(n, ',');
              n := umlDeleteFirstStr_Discontinuity(n, ',');
              inc(j);
            end;
          OnNotify(s, king, buff);
        end;
    end;

  SetLength(buff, 0);
  SetLength(king, 0);
  n := '';
end;

procedure ImportCSV_M(const sour: TArrayPascalString; OnNotify: TCSVSaveMethod);
var
  i, j, bp, hc: NativeInt;
  n: TPascalString;
  king, buff: TArrayPascalString;
begin
  // csv head
  bp := -1;
  for i := low(sour) to high(sour) do
    begin
      n := sour[i];
      if n.Len <> 0 then
        begin
          bp := i + 1;
          hc := n.GetCharCount(',') + 1;
          SetLength(buff, hc);
          SetLength(king, hc);

          for j := low(king) to high(king) do
              king[j] := '';
          j := 0;
          while (j < length(king)) and (n.Len > 0) do
            begin
              king[j] := umlGetFirstStr_Discontinuity(n, ',');
              n := umlDeleteFirstStr_Discontinuity(n, ',');
              inc(j);
            end;

          break;
        end;
    end;

  // csv body
  if bp > 0 then
    for i := bp to high(sour) do
      begin
        n := sour[i];
        if n.Len > 0 then
          begin
            for j := low(buff) to high(buff) do
                buff[j] := '';
            j := 0;
            while (j < length(buff)) and (n.Len > 0) do
              begin
                buff[j] := umlGetFirstStr_Discontinuity(n, ',');
                n := umlDeleteFirstStr_Discontinuity(n, ',');
                inc(j);
              end;
            OnNotify(sour[i], king, buff);
          end;
      end;

  SetLength(buff, 0);
  SetLength(king, 0);
  n := '';
end;

procedure CustomImportCSV_M(const OnGetLine: TCSVGetLineMethod; OnNotify: TCSVSaveMethod);
var
  IsEnd: Boolean;
  i, j, hc: NativeInt;
  n, s: TPascalString;
  king, buff: TArrayPascalString;
begin
  // csv head
  while true do
    begin
      IsEnd := False;
      n := '';
      OnGetLine(n, IsEnd);
      if IsEnd then
          exit;
      if n.L <> 0 then
        begin
          hc := n.GetCharCount(',') + 1;
          SetLength(buff, hc);
          SetLength(king, hc);

          for j := low(king) to high(king) do
              king[j] := '';
          j := 0;
          while (j < length(king)) and (n.Len > 0) do
            begin
              king[j] := umlGetFirstStr_Discontinuity(n, ',');
              n := umlDeleteFirstStr_Discontinuity(n, ',');
              inc(j);
            end;

          break;
        end;
    end;

  // csv body
  while true do
    begin
      IsEnd := False;
      n := '';
      OnGetLine(n, IsEnd);
      if IsEnd then
          exit;
      if n.Len > 0 then
        begin
          s := n;
          for j := low(buff) to high(buff) do
              buff[j] := '';
          j := 0;
          while (j < length(buff)) and (n.Len > 0) do
            begin
              buff[j] := umlGetFirstStr_Discontinuity(n, ',');
              n := umlDeleteFirstStr_Discontinuity(n, ',');
              inc(j);
            end;
          OnNotify(s, king, buff);
        end;
    end;

  SetLength(buff, 0);
  SetLength(king, 0);
  n := '';
end;

procedure ImportCSV_P(const sour: TArrayPascalString; OnNotify: TCSVSaveProc);
var
  i, j, bp, hc: NativeInt;
  n: TPascalString;
  king, buff: TArrayPascalString;
begin
  // csv head
  bp := -1;
  for i := low(sour) to high(sour) do
    begin
      n := sour[i];
      if n.Len <> 0 then
        begin
          bp := i + 1;
          hc := n.GetCharCount(',') + 1;
          SetLength(buff, hc);
          SetLength(king, hc);

          for j := low(king) to high(king) do
              king[j] := '';
          j := 0;
          while (j < length(king)) and (n.Len > 0) do
            begin
              king[j] := umlGetFirstStr_Discontinuity(n, ',');
              n := umlDeleteFirstStr_Discontinuity(n, ',');
              inc(j);
            end;

          break;
        end;
    end;

  // csv body
  if bp > 0 then
    for i := bp to high(sour) do
      begin
        n := sour[i];
        if n.Len > 0 then
          begin
            for j := low(buff) to high(buff) do
                buff[j] := '';
            j := 0;
            while (j < length(buff)) and (n.Len > 0) do
              begin
                buff[j] := umlGetFirstStr_Discontinuity(n, ',');
                n := umlDeleteFirstStr_Discontinuity(n, ',');
                inc(j);
              end;
            OnNotify(sour[i], king, buff);
          end;
      end;

  SetLength(buff, 0);
  SetLength(king, 0);
  n := '';
end;

procedure CustomImportCSV_P(const OnGetLine: TCSVGetLineProc; OnNotify: TCSVSaveProc);
var
  IsEnd: Boolean;
  i, j, hc: NativeInt;
  n, s: TPascalString;
  king, buff: TArrayPascalString;
begin
  // csv head
  while true do
    begin
      IsEnd := False;
      n := '';
      OnGetLine(n, IsEnd);
      if IsEnd then
          exit;
      if n.L <> 0 then
        begin
          hc := n.GetCharCount(',') + 1;
          SetLength(buff, hc);
          SetLength(king, hc);

          for j := low(king) to high(king) do
              king[j] := '';
          j := 0;
          while (j < length(king)) and (n.Len > 0) do
            begin
              king[j] := umlGetFirstStr_Discontinuity(n, ',');
              n := umlDeleteFirstStr_Discontinuity(n, ',');
              inc(j);
            end;

          break;
        end;
    end;

  // csv body
  while true do
    begin
      IsEnd := False;
      n := '';
      OnGetLine(n, IsEnd);
      if IsEnd then
          exit;
      if n.Len > 0 then
        begin
          s := n;
          for j := low(buff) to high(buff) do
              buff[j] := '';
          j := 0;
          while (j < length(buff)) and (n.Len > 0) do
            begin
              buff[j] := umlGetFirstStr_Discontinuity(n, ',');
              n := umlDeleteFirstStr_Discontinuity(n, ',');
              inc(j);
            end;
          OnNotify(s, king, buff);
        end;
    end;

  SetLength(buff, 0);
  SetLength(king, 0);
  n := '';
end;

var
  ExLibs: THashVariantList = nil;

function GetExtLib(LibName: SystemString): HMODULE;
begin
  Result := 0;
{$IF not(Defined(IOS) and Defined(CPUARM))}
  if ExLibs = nil then
      ExLibs := THashVariantList.Create;
  if not ExLibs.Exists(LibName) then
    begin
      try
{$IFNDEF FPC}
{$IFDEF ANDROID}
        Result := LoadLibrary(PChar(umlCombineFileName(System.IOUtils.TPath.GetLibraryPath, LibName).text));
{$ELSE ANDROID}
        Result := LoadLibrary(PChar(LibName));
{$ENDIF ANDROID}
{$ELSE FPC}
        Result := LoadLibrary(PChar(LibName));
{$ENDIF FPC}
        ExLibs.Add(LibName, Result);
      except
        FreeExtLib(LibName);
        Result := 0;
      end;
    end
  else
      Result := ExLibs[LibName];

  if Result = 0 then
      DoStatus('failed LoadLibrary %s', [LibName]);
{$IFEND}
end;

function FreeExtLib(LibName: SystemString): Boolean;
begin
  Result := False;
{$IF not(Defined(IOS) and Defined(CPUARM))}
  if ExLibs = nil then
      ExLibs := THashVariantList.Create;
  if ExLibs.Exists(LibName) then
    begin
      try
          FreeLibrary(HMODULE(ExLibs[LibName]));
      except
      end;
      ExLibs.Delete(LibName);
      Result := true;
    end;
{$IFEND}
end;

function GetExtProc(const LibName, ProcName: SystemString): Pointer;
{$IF not(Defined(IOS) and Defined(CPUARM))}
var
  h: HMODULE;
{$IFEND}
begin
  Result := nil;
{$IF not(Defined(IOS) and Defined(CPUARM))}
  h := GetExtLib(LibName);
  if h <> 0 then
    begin
      Result := GetProcAddress(h, PChar(ProcName));
      if Result = nil then
          DoStatus('error external libray: %s - %s', [LibName, ProcName]);
    end;
{$IFEND}
end;

{$IFDEF RangeCheck}{$R-}{$ENDIF}


function umlCompareByteString(const s1: TPascalString; const s2: PArrayRawByte): Boolean;
var
  tmp: TBytes;
  i: Integer;
begin
  SetLength(tmp, s1.L);
  for i := 0 to s1.L - 1 do
      tmp[i] := Byte(s1.buff[i]);

  Result := CompareMemory(@tmp[0], @s2^[0], s1.L);
end;

function umlCompareByteString(const s2: PArrayRawByte; const s1: TPascalString): Boolean;
var
  tmp: TBytes;
  i: Integer;
begin
  SetLength(tmp, s1.L);
  for i := 0 to s1.L - 1 do
      tmp[i] := Byte(s1.buff[i]);

  Result := CompareMemory(@tmp[0], @s2^[0], s1.L);
end;

procedure umlSetByteString(const sour: TPascalString; const dest: PArrayRawByte);
var
  i: Integer;
begin
  for i := 0 to sour.L - 1 do
      dest^[i] := Byte(sour.buff[i]);
end;

procedure umlSetByteString(const dest: PArrayRawByte; const sour: TPascalString);
var
  i: Integer;
begin
  for i := 0 to sour.L - 1 do
      dest^[i] := Byte(sour.buff[i]);
end;

function umlGetByteString(const sour: PArrayRawByte; const L: Integer): TPascalString;
var
  i: Integer;
begin
  Result.L := L;
  for i := 0 to L - 1 do
      Result.buff[i] := SystemChar(sour^[i]);
end;

{$IFDEF RangeCheck}{$R+}{$ENDIF}


procedure SaveMemory(p: Pointer; siz: NativeInt; DestFile: TPascalString);
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  m64.SetPointerWithProtectedMode(p, siz);
  m64.SaveToFile(DestFile);
  DisposeObject(m64);
end;

initialization

finalization

if ExLibs <> nil then
    DisposeObject(ExLibs);

end.
