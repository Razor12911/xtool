{ * Compressor                                                                 * }
{ ****************************************************************************** }
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
unit CoreCompress;

{$INCLUDE zDefine.inc}

interface

uses Math, Types, CoreClasses;

type
  PPCCInt8 = ^PCCInt8;
  PCCInt8 = ^TCCInt8;
  TCCInt8 = ShortInt;

  PPCCUInt8 = ^PCCUInt8;
  PCCUInt8 = ^TCCUInt8;
  TCCUInt8 = Byte;

  PCCInt16 = ^TCCInt16;
  TCCInt16 = SmallInt;

  PCCUInt16 = ^TCCUInt16;
  TCCUInt16 = Word;

  PCCInt32 = ^TCCInt32;
  TCCInt32 = Integer;

  PCCUInt32 = ^TCCUInt32;
  TCCUInt32 = Cardinal;

  PCCInt64 = ^TCCInt64;
  TCCInt64 = Int64;

  PCCUInt64 = ^TCCUInt64;
  TCCUInt64 = UInt64;

  PCCPtr = ^TCCPtr;
  TCCPtr = Pointer;

  TCCPtrUInt = nativeUInt;
  TCCPtrInt = NativeInt;

  PPCCPtrInt = ^PCCPtrInt;
  PCCPtrUInt = ^TCCPtrUInt;
  PCCPtrInt = ^TCCPtrInt;

  PCCSizeUInt = ^TCCSizeUInt;
  TCCSizeUInt = TCCPtrUInt;

  PCCSizeInt = ^TCCSizeInt;
  TCCSizeInt = TCCPtrInt;

  PCCNativeUInt = ^TCCNativeUInt;
  TCCNativeUInt = TCCPtrUInt;

  PCCNativeInt = ^TCCNativeInt;
  TCCNativeInt = TCCPtrInt;

  PCCSize = ^TCCSizeUInt;
  TCCSize = TCCPtrUInt;

  PCCUInt8Array = ^TCCUInt8Array;
  TCCUInt8Array = array [0 .. MaxInt div SizeOf(TCCUInt8) - 1] of TCCUInt8;

  PPCCUInt64Record = ^PCCUInt64Record;
  PCCUInt64Record = ^TCCUInt64Record;

  TCCUInt64Record = packed record
    case Boolean of
      False: ( {$IFDEF BIG_ENDIAN}Hi, Lo{$ELSE}Lo, Hi{$ENDIF}: TCCUInt32;);
      True: (Value: TCCUInt64;);
  end;

  TCompressor = class(TCoreClassObject)
  private const
    ChunkHeadSize = $3000;
    ChunkSize = $FFFF - ChunkHeadSize;
    PrepareBuffSize = $FFFF;
  public
    constructor Create; reintroduce; virtual;
    destructor Destroy; override;
    function Compress(const aInData: TCCPtr; const aInSize: TCCSizeUInt; const aOutData: TCCPtr; const aOutLimit: TCCSizeUInt): TCCSizeUInt; virtual;
    function Decompress(const aInData: TCCPtr; const aInSize: TCCSizeUInt; const aOutData: TCCPtr; const aOutLimit: TCCSizeUInt): TCCSizeUInt; virtual;

    procedure CompressStream(sour: TCoreClassStream; StartPos, EndPos: NativeInt; CompressTo: TCoreClassStream);
    procedure DecompressStream(sour, DecompressTo: TCoreClassStream);
  end;

  TCompressorClass = class of TCompressor;

  TCompressorDeflate = class(TCompressor)
  private const
    HashBits = 16;
    HashSize = 1 shl HashBits;
    HashMask = HashSize - 1;
    HashShift = 32 - HashBits;
    WindowSize = 32768;
    WindowMask = WindowSize - 1;
    MinMatch = 3;
    MaxMatch = 258;
    MaxOffset = 32768;
    HashRef_ENDIAN_B30 = {$IF defined(BIG_ENDIAN)}$FFFFFF00{$ELSE}$00FFFFFF{$IFEND};

{$IFNDEF BIG_ENDIAN}
    MultiplyDeBruijnBytePosition: array [0 .. 31] of TCCUInt8 = (0, 0, 3, 0, 3, 1, 3, 0, 3, 2, 2, 1, 3, 2, 0, 1, 3, 3, 1, 2, 2, 2, 2, 0, 3, 1, 2, 0, 1, 0, 1, 1);
{$ENDIF}
    //
    LengthCodes: array [0 .. 28, 0 .. 3] of TCCUInt32 =
      ( // Code, ExtraBits, Min, Max
      (257, 0, 3, 3), (258, 0, 4, 4), (259, 0, 5, 5),
      (260, 0, 6, 6), (261, 0, 7, 7), (262, 0, 8, 8),
      (263, 0, 9, 9), (264, 0, 10, 10), (265, 1, 11, 12),
      (266, 1, 13, 14), (267, 1, 15, 16), (268, 1, 17, 18),
      (269, 2, 19, 22), (270, 2, 23, 26), (271, 2, 27, 30),
      (272, 2, 31, 34), (273, 3, 35, 42), (274, 3, 43, 50),
      (275, 3, 51, 58), (276, 3, 59, 66), (277, 4, 67, 82),
      (278, 4, 83, 98), (279, 4, 99, 114), (280, 4, 115, 130),
      (281, 5, 131, 162), (282, 5, 163, 194), (283, 5, 195, 226),
      (284, 5, 227, 257), (285, 0, 258, 258)
      );
    DistanceCodes: array [0 .. 29, 0 .. 3] of TCCUInt32 =
      ( // Code, ExtraBits, Min, Max
      (0, 0, 1, 1), (1, 0, 2, 2), (2, 0, 3, 3),
      (3, 0, 4, 4), (4, 1, 5, 6), (5, 1, 7, 8),
      (6, 2, 9, 12), (7, 2, 13, 16), (8, 3, 17, 24),
      (9, 3, 25, 32), (10, 4, 33, 48), (11, 4, 49, 64),
      (12, 5, 65, 96), (13, 5, 97, 128), (14, 6, 129, 192),
      (15, 6, 193, 256), (16, 7, 257, 384), (17, 7, 385, 512),
      (18, 8, 513, 768), (19, 8, 769, 1024), (20, 9, 1025, 1536),
      (21, 9, 1537, 2048), (22, 10, 2049, 3072), (23, 10, 3073, 4096),
      (24, 11, 4097, 6144), (25, 11, 6145, 8192), (26, 12, 8193, 12288),
      (27, 12, 12289, 16384), (28, 13, 16385, 24576), (29, 13, 24577, 32768)
      );
    MirrorBytes: array [TCCUInt8] of TCCUInt8 =
      (
      $00, $80, $40, $C0, $20, $A0, $60, $E0,
      $10, $90, $50, $D0, $30, $B0, $70, $F0,
      $08, $88, $48, $C8, $28, $A8, $68, $E8,
      $18, $98, $58, $D8, $38, $B8, $78, $F8,
      $04, $84, $44, $C4, $24, $A4, $64, $E4,
      $14, $94, $54, $D4, $34, $B4, $74, $F4,
      $0C, $8C, $4C, $CC, $2C, $AC, $6C, $EC,
      $1C, $9C, $5C, $DC, $3C, $BC, $7C, $FC,
      $02, $82, $42, $C2, $22, $A2, $62, $E2,
      $12, $92, $52, $D2, $32, $B2, $72, $F2,
      $0A, $8A, $4A, $CA, $2A, $AA, $6A, $EA,
      $1A, $9A, $5A, $DA, $3A, $BA, $7A, $FA,
      $06, $86, $46, $C6, $26, $A6, $66, $E6,
      $16, $96, $56, $D6, $36, $B6, $76, $F6,
      $0E, $8E, $4E, $CE, $2E, $AE, $6E, $EE,
      $1E, $9E, $5E, $DE, $3E, $BE, $7E, $FE,
      $01, $81, $41, $C1, $21, $A1, $61, $E1,
      $11, $91, $51, $D1, $31, $B1, $71, $F1,
      $09, $89, $49, $C9, $29, $A9, $69, $E9,
      $19, $99, $59, $D9, $39, $B9, $79, $F9,
      $05, $85, $45, $C5, $25, $A5, $65, $E5,
      $15, $95, $55, $D5, $35, $B5, $75, $F5,
      $0D, $8D, $4D, $CD, $2D, $AD, $6D, $ED,
      $1D, $9D, $5D, $DD, $3D, $BD, $7D, $FD,
      $03, $83, $43, $C3, $23, $A3, $63, $E3,
      $13, $93, $53, $D3, $33, $B3, $73, $F3,
      $0B, $8B, $4B, $CB, $2B, $AB, $6B, $EB,
      $1B, $9B, $5B, $DB, $3B, $BB, $7B, $FB,
      $07, $87, $47, $C7, $27, $A7, $67, $E7,
      $17, $97, $57, $D7, $37, $B7, $77, $F7,
      $0F, $8F, $4F, $CF, $2F, $AF, $6F, $EF,
      $1F, $9F, $5F, $DF, $3F, $BF, $7F, $FF
      );
    CLCIndex: array [0 .. 18] of TCCUInt8 = (16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15);

  type
    PHashTable = ^THashTable;
    THashTable = array [0 .. HashSize - 1] of PCCUInt8;
    PChainTable = ^TChainTable;
    TChainTable = array [0 .. WindowSize - 1] of TCCPtr;
    PTree = ^TTree;

    TTree = packed record
      Table: array [0 .. 15] of TCCUInt16;
      Translation: array [0 .. 287] of TCCUInt16;
    end;

    PBuffer = ^TBuffer;
    TBuffer = array [0 .. 65535] of TCCUInt8;
    PLengths = ^TLengths;
    TLengths = array [0 .. 288 + 32 - 1] of TCCUInt8;
    POffsets = ^TOffsets;
    TOffsets = array [0 .. 15] of TCCUInt16;
    TBits = array [0 .. 29] of TCCUInt8;
    PBits = ^TBits;
    TBase = array [0 .. 29] of TCCUInt16;
    PBase = ^TBase;

  var
    fHashTable: THashTable;
    fChainTable: TChainTable;
    fLengthCodesLookUpTable: array [0 .. 258] of TCCInt32;
    fDistanceCodesLookUpTable: array [0 .. 32768] of TCCInt32;
    fSymbolLengthTree: TTree;
    fDistanceTree: TTree;
    fFixedSymbolLengthTree: TTree;
    fFixedDistanceTree: TTree;
    fLengthBits: TBits;
    fDistanceBits: TBits;
    fLengthBase: TBase;
    fDistanceBase: TBase;
    fCodeTree: TTree;
    fLengths: TLengths;
    fWithHeader: Boolean;
    fGreedy: Boolean;
    fSkipStrength: TCCUInt32;
    fMaxSteps: TCCUInt32;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Compress(const aInData: TCCPtr; const aInSize: TCCSizeUInt; const aOutData: TCCPtr; const aOutLimit: TCCSizeUInt): TCCSizeUInt; override;
    function Decompress(const aInData: TCCPtr; const aInSize: TCCSizeUInt; const aOutData: TCCPtr; const aOutLimit: TCCSizeUInt): TCCSizeUInt; override;

    property WithHeader: Boolean read fWithHeader write fWithHeader;
    property Greedy: Boolean read fGreedy write fGreedy;
    property SkipStrength: TCCUInt32 read fSkipStrength write fSkipStrength;
    property MaxSteps: TCCUInt32 read fMaxSteps write fMaxSteps;
  end;

  TCompressorBRRC = class(TCompressor)
  private const
    FlagModel = 0;
    LiteralModel = 2;
    SizeModels = 258;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Compress(const aInData: TCCPtr; const aInSize: TCCSizeUInt; const aOutData: TCCPtr; const aOutLimit: TCCSizeUInt): TCCSizeUInt; override;
    function Decompress(const aInData: TCCPtr; const aInSize: TCCSizeUInt; const aOutData: TCCPtr; const aOutLimit: TCCSizeUInt): TCCSizeUInt; override;
  end;

function CoreCompressStream(Compressor: TCompressor; sour: TCoreClassStream; ComTo: TCoreClassStream): Boolean;
function CoreDecompressStream(Compressor: TCompressor; sour: TCoreClassStream; DeTo: TCoreClassStream): Boolean;

function DeflateCompressStream(sour: TCoreClassStream; ComTo: TCoreClassStream): Boolean;
function DeflateDecompressStream(sour: TCoreClassStream; DeTo: TCoreClassStream): Boolean;

function BRRCCompressStream(sour: TCoreClassStream; ComTo: TCoreClassStream): Boolean;
function BRRCDecompressStream(sour: TCoreClassStream; DeTo: TCoreClassStream): Boolean;

implementation

uses MemoryStream64;

function CoreCompressStream(Compressor: TCompressor; sour: TCoreClassStream; ComTo: TCoreClassStream): Boolean;
begin
  try
    Compressor.CompressStream(sour, 0, sour.Size, ComTo);
    Result := True;
  except
      Result := False;
  end;
end;

function CoreDecompressStream(Compressor: TCompressor; sour: TCoreClassStream; DeTo: TCoreClassStream): Boolean;
begin
  try
    Compressor.DecompressStream(sour, DeTo);
    Result := True;
  except
      Result := False;
  end;
end;

function DeflateCompressStream(sour: TCoreClassStream; ComTo: TCoreClassStream): Boolean;
var
  c: TCompressorDeflate;
begin
  c := TCompressorDeflate.Create;
  Result := CoreCompressStream(c, sour, ComTo);
  DisposeObject(c);
end;

function DeflateDecompressStream(sour: TCoreClassStream; DeTo: TCoreClassStream): Boolean;
var
  c: TCompressorDeflate;
begin
  c := TCompressorDeflate.Create;
  Result := CoreDecompressStream(c, sour, DeTo);
  DisposeObject(c);
end;

function BRRCCompressStream(sour: TCoreClassStream; ComTo: TCoreClassStream): Boolean;
var
  c: TCompressorBRRC;
begin
  c := TCompressorBRRC.Create;
  Result := CoreCompressStream(c, sour, ComTo);
  DisposeObject(c);
end;

function BRRCDecompressStream(sour: TCoreClassStream; DeTo: TCoreClassStream): Boolean;
var
  c: TCompressorBRRC;
begin
  c := TCompressorBRRC.Create;
  Result := CoreDecompressStream(c, sour, DeTo);
  DisposeObject(c);
end;

procedure BytewiseMemoryMove(const aSource; var aDestination; const aLength: TCCSizeUInt);
var
  index: TCCSizeUInt;
  Source, Destination: PCCUInt8Array;
begin
  if aLength > 0 then
    begin
      Source := TCCPtr(@aSource);
      Destination := TCCPtr(@aDestination);
      for index := 0 to aLength - 1 do
        begin
          Destination^[index] := Source^[index];
        end;
    end;
end;

procedure RLELikeSideEffectAwareMemoryMove(const aSource; var aDestination; const aLength: TCCSizeUInt);
begin
  if aLength > 0 then
    begin
      if (TCCSizeUInt(TCCPtr(@aSource)) + aLength) <= TCCSizeUInt(TCCPtr(@aDestination)) then
        // Non-overlapping, so we an use an optimized memory move function
          CopyPtr(@aSource, @aDestination, aLength)
      else
        // Overlapping, so we must do copy byte-wise for to get the free RLE-like side-effect included
          BytewiseMemoryMove(aSource, aDestination, aLength);
    end;
end;

{$IFDEF RangeCheck}{$R-}{$ENDIF}

{$IFNDEF fpc}


function BSRDWord(Value: TCCUInt32): TCCUInt32;

const
  BSRDebruijn32Multiplicator = TCCUInt32($07C4ACDD);
  BSRDebruijn32Shift = 27;
  BSRDebruijn32Mask = 31;
  BSRDebruijn32Table: array [0 .. 31] of TCCInt32 = (0, 9, 1, 10, 13, 21, 2, 29, 11, 14, 16, 18, 22, 25, 3, 30, 8, 12, 20, 28, 15, 17, 24, 7, 19, 27, 23, 6, 26, 5, 4, 31);
begin
  if Value = 0 then
    begin
      Result := 255;
    end
  else
    begin
      Value := Value or (Value shr 1);
      Value := Value or (Value shr 2);
      Value := Value or (Value shr 4);
      Value := Value or (Value shr 8);
      Value := Value or (Value shr 16);
      Result := BSRDebruijn32Table[((Value * BSRDebruijn32Multiplicator) shr BSRDebruijn32Shift) and BSRDebruijn32Mask];
    end;
end;
{$IFEND}


function SARLongint(Value, Shift: TCCInt32): TCCInt32;
begin
  Shift := Shift and 31;
  Result := (TCCUInt32(Value) shr Shift) or (TCCUInt32(TCCInt32(TCCUInt32(-TCCUInt32(TCCUInt32(Value) shr 31)) and TCCUInt32(-TCCUInt32(Ord(Shift <> 0) and 1)))) shl (32 - Shift));
end;

function SARInt64(Value: TCCInt64; Shift: TCCInt32): TCCInt64;
begin
  Shift := Shift and 63;
  Result := (TCCInt64(Value) shr Shift) or (TCCInt64(TCCInt64(TCCInt64(-TCCInt64(TCCInt64(Value) shr 63)) and TCCInt64(-TCCInt64(Ord(Shift <> 0) and 1)))) shl (63 - Shift));
end;

constructor TCompressor.Create;
begin
  inherited Create;
end;

destructor TCompressor.Destroy;
begin
  inherited Destroy;
end;

function TCompressor.Compress(const aInData: TCCPtr; const aInSize: TCCSizeUInt; const aOutData: TCCPtr; const aOutLimit: TCCSizeUInt): TCCSizeUInt;
begin
  Result := 0;
end;

function TCompressor.Decompress(const aInData: TCCPtr; const aInSize: TCCSizeUInt; const aOutData: TCCPtr; const aOutLimit: TCCSizeUInt): TCCSizeUInt;
begin
  Result := 0;
end;

procedure TCompressor.CompressStream(sour: TCoreClassStream; StartPos, EndPos: NativeInt; CompressTo: TCoreClassStream);
type
  TPrepareBuff = array [0 .. PrepareBuffSize + 2] of Byte;
  PPrepareBuff = ^TPrepareBuff;

var
  buff: array [0 .. ChunkSize] of Byte;
  PrepareBuffPtr: PPrepareBuff;
  siz: Int64;
  j: NativeInt;
  Num: NativeInt;
  Rest: NativeInt;
begin
  siz := EndPos - StartPos;
  if siz > 0 then
    begin
      CompressTo.write(siz, 8);
      sour.Position := StartPos;

      new(PrepareBuffPtr);
      if siz > ChunkSize then
        begin
          Num := siz div ChunkSize;
          Rest := siz mod ChunkSize;

          for j := 0 to Num - 1 do
            begin
              sour.read(buff[0], ChunkSize);
              PWORD(@(PrepareBuffPtr^[0]))^ := Compress(@buff[0], ChunkSize, @PrepareBuffPtr^[2], PrepareBuffSize);
              CompressTo.write(PrepareBuffPtr^[0], PWORD(@(PrepareBuffPtr^[0]))^ + 2);
            end;

          if Rest > 0 then
            begin
              sour.read(buff[0], Rest);
              PWORD(@(PrepareBuffPtr^[0]))^ := Compress(@buff[0], Rest, @PrepareBuffPtr^[2], PrepareBuffSize);
              CompressTo.write(PrepareBuffPtr^[0], PWORD(@(PrepareBuffPtr^[0]))^ + 2);
            end;
        end
      else
        begin
          sour.read(buff[0], siz);
          PWORD(@(PrepareBuffPtr^[0]))^ := Compress(@buff[0], siz, @PrepareBuffPtr^[2], PrepareBuffSize);
          CompressTo.write(PrepareBuffPtr^[0], PWORD(@(PrepareBuffPtr^[0]))^ + 2);
        end;
      Dispose(PrepareBuffPtr);
    end;
end;

procedure TCompressor.DecompressStream(sour, DecompressTo: TCoreClassStream);
var
  siz, cSiz: Int64;
  bufSiz, deBufSiz: Word;
  buff, decryptBuff: Pointer;
begin
  if sour.Position + 10 < sour.Size then
    begin
      sour.read(siz, 8);
      cSiz := 0;

      buff := GetMemory(PrepareBuffSize);
      decryptBuff := GetMemory(PrepareBuffSize);
      while cSiz < siz do
        begin
          if sour.read(bufSiz, 2) <> 2 then
              Break;
          if sour.read(buff^, bufSiz) <> bufSiz then
              Break;

          deBufSiz := Decompress(buff, bufSiz, decryptBuff, PrepareBuffSize);
          DecompressTo.write(decryptBuff^, deBufSiz);
          inc(cSiz, deBufSiz);
        end;
      FreeMemory(buff);
      FreeMemory(decryptBuff);
    end;
end;

constructor TCompressorDeflate.Create;
  procedure BuildFixedTrees(var aLT, aDT: TTree);
  var
    i: TCCInt32;
  begin
    for i := 0 to 6 do
      begin
        aLT.Table[i] := 0;
      end;
    aLT.Table[7] := 24;
    aLT.Table[8] := 152;
    aLT.Table[9] := 112;
    for i := 0 to 23 do
        aLT.Translation[i] := 256 + i;
    for i := 0 to 143 do
        aLT.Translation[24 + i] := i;
    for i := 0 to 7 do
        aLT.Translation[168 + i] := 280 + i;
    for i := 0 to 111 do
        aLT.Translation[176 + i] := 144 + i;
    for i := 0 to 4 do
        aDT.Table[i] := 0;
    aDT.Table[5] := 32;
    for i := 0 to 31 do
        aDT.Translation[i] := i;
  end;
  procedure BuildBitsBase(aBits: PCCUInt8Array; aBase: PCCUInt16; aDelta, aFirst: TCCInt32);
  var
    i, Sum: TCCInt32;
  begin
    for i := 0 to aDelta - 1 do
        aBits^[i] := 0;
    for i := 0 to (30 - aDelta) - 1 do
        aBits^[i + aDelta] := i div aDelta;
    Sum := aFirst;
    for i := 0 to 29 do
      begin
        aBase^ := Sum;
        inc(aBase);
        inc(Sum, 1 shl aBits^[i]);
      end;
  end;

var
  index, ValueIndex: TCCInt32;
begin
  inherited Create;
  for index := 0 to length(LengthCodes) - 1 do
    for ValueIndex := IfThen(index = 0, 0, LengthCodes[index, 2]) to LengthCodes[index, 3] do
        fLengthCodesLookUpTable[ValueIndex] := index;
  for index := 0 to length(DistanceCodes) - 1 do
    for ValueIndex := IfThen(index = 0, 0, DistanceCodes[index, 2]) to DistanceCodes[index, 3] do
        fDistanceCodesLookUpTable[ValueIndex] := index;

  FillPtrByte(@fLengthBits, SizeOf(TBits), 0);
  FillPtrByte(@fDistanceBits, SizeOf(TBits), 0);
  FillPtrByte(@fLengthBase, SizeOf(TBase), 0);
  FillPtrByte(@fDistanceBase, SizeOf(TBase), 0);
  FillPtrByte(@fFixedSymbolLengthTree, SizeOf(TTree), 0);
  FillPtrByte(@fFixedDistanceTree, SizeOf(TTree), 0);
  BuildFixedTrees(fFixedSymbolLengthTree, fFixedDistanceTree);
  BuildBitsBase(TCCPtr(@fLengthBits[0]), PCCUInt16(TCCPtr(@fLengthBase[0])), 4, 3);
  BuildBitsBase(TCCPtr(@fDistanceBits[0]), PCCUInt16(TCCPtr(@fDistanceBase[0])), 2, 1);
  fLengthBits[28] := 0;
  fLengthBase[28] := 258;
  fWithHeader := False;
  fGreedy := False;
  fSkipStrength := 32;
  fMaxSteps := 128;
end;

destructor TCompressorDeflate.Destroy;
begin
  inherited Destroy;
end;

function TCompressorDeflate.Compress(const aInData: TCCPtr; const aInSize: TCCSizeUInt; const aOutData: TCCPtr; const aOutLimit: TCCSizeUInt): TCCSizeUInt;
var
  OutputBits, CountOutputBits: TCCUInt32;
  DestLen: TCCSizeUInt;
  OK: Boolean;
  procedure DoOutputBits(const aBits, aCountBits: TCCUInt32);
  begin
    Assert((CountOutputBits + aCountBits) <= 32);
    OutputBits := OutputBits or (aBits shl CountOutputBits);
    inc(CountOutputBits, aCountBits);
    while CountOutputBits >= 8 do
      begin
        if DestLen < aOutLimit then
          begin
            PCCUInt8Array(aOutData)^[DestLen] := OutputBits and $FF;
            inc(DestLen);
          end
        else
          begin
            OK := False;
          end;
        OutputBits := OutputBits shr 8;
        dec(CountOutputBits, 8);
      end;
  end;
  procedure DoOutputLiteral(const AValue: TCCUInt8);
  begin
    case AValue of
      0 .. 143: DoOutputBits(MirrorBytes[$30 + AValue], 8);
      else DoOutputBits((MirrorBytes[$90 + (AValue - 144)] shl 1) or 1, 9);
    end;
  end;
  procedure DoOutputCopy(const aDistance, aLength: TCCUInt32);
  var
    Remain, ToDo, index: TCCUInt32;
  begin
    Remain := aLength;
    while Remain > 0 do
      begin
        case Remain of
          0 .. 258: ToDo := Remain;
          259 .. 260: ToDo := Remain - 3;
          else ToDo := 258;
        end;
        dec(Remain, ToDo);
        index := fLengthCodesLookUpTable[Min(Max(ToDo, 0), 258)];
        if LengthCodes[index, 0] <= 279 then
            DoOutputBits(MirrorBytes[(LengthCodes[index, 0] - 256) shl 1], 7)
        else
            DoOutputBits(MirrorBytes[$C0 + (LengthCodes[index, 0] - 280)], 8);
        if LengthCodes[index, 1] <> 0 then
            DoOutputBits(ToDo - LengthCodes[index, 2], LengthCodes[index, 1]);
        index := fDistanceCodesLookUpTable[Min(Max(aDistance, 0), 32768)];
        DoOutputBits(MirrorBytes[DistanceCodes[index, 0] shl 3], 5);
        if DistanceCodes[index, 1] <> 0 then
            DoOutputBits(aDistance - DistanceCodes[index, 2], DistanceCodes[index, 1]);
      end;
  end;
  procedure OutputStartBlock;
  begin
    DoOutputBits(1, 1); // Final block
    DoOutputBits(1, 2); // Static huffman block
  end;
  procedure OutputEndBlock;
  begin
    DoOutputBits(0, 7); // Close block
    DoOutputBits(0, 7); // Make sure all bits are flushed
  end;
  function Adler32(const aData: TCCPtr; const aLength: TCCUInt32): TCCUInt32;
  const
    Base = 65521;
    MaximumCountAtOnce = 5552;
  var
    Buf: PCCUInt8;
    Remain, s1, s2, ToDo, index: TCCUInt32;
  begin
    s1 := 1;
    s2 := 0;
    Buf := aData;
    Remain := aLength;
    while Remain > 0 do
      begin
        if Remain < MaximumCountAtOnce then
            ToDo := Remain
        else
            ToDo := MaximumCountAtOnce;
        dec(Remain, ToDo);
        for index := 1 to ToDo do
          begin
            inc(s1, TCCUInt8(Buf^));
            inc(s2, s1);
            inc(Buf);
          end;
        s1 := s1 mod Base;
        s2 := s2 mod Base;
      end;
    Result := (s2 shl 16) or s1;
  end;

var
  CurrentPointer, EndPointer, EndSearchPointer, Head, CurrentPossibleMatch: PCCUInt8;
  BestMatchDistance, BestMatchLength, MatchLength, CheckSum, Step, Difference, Offset, UnsuccessfulFindMatchAttempts: TCCUInt32;
  HashTableItem: PPCCUInt8;
begin
  OK := True;
  DestLen := 0;
  OutputBits := 0;
  CountOutputBits := 0;
  if fWithHeader then
    begin
      DoOutputBits($78, 8); // CMF
      DoOutputBits($9C, 8); // FLG Default Compression
    end;
  OutputStartBlock;
  FillPtrByte(@fHashTable, SizeOf(THashTable), 0);
  FillPtrByte(@fChainTable, SizeOf(TChainTable), 0);
  CurrentPointer := aInData;
  EndPointer := TCCPtr(TCCPtrUInt(TCCPtrUInt(CurrentPointer) + TCCPtrUInt(aInSize)));
  EndSearchPointer := TCCPtr(TCCPtrUInt((TCCPtrUInt(CurrentPointer) + TCCPtrUInt(aInSize)) - TCCPtrUInt(TCCInt64(Max(TCCInt64(MinMatch), TCCInt64(SizeOf(TCCUInt32)))))));
  UnsuccessfulFindMatchAttempts := TCCUInt32(1) shl fSkipStrength;
  while TCCPtrUInt(CurrentPointer) < TCCPtrUInt(EndSearchPointer) do
    begin
      HashTableItem := @fHashTable[((((PCCUInt32(TCCPtr(CurrentPointer))^ and TCCUInt32(HashRef_ENDIAN_B30){$IF defined(BIG_ENDIAN)} shr 8{$IFEND})) * TCCUInt32($1E35A7BD)) shr HashShift) and HashMask];
      Head := HashTableItem^;
      CurrentPossibleMatch := Head;
      BestMatchDistance := 0;
      BestMatchLength := 1;
      Step := 0;
      while Assigned(CurrentPossibleMatch) and
        (TCCPtrUInt(CurrentPointer) > TCCPtrUInt(CurrentPossibleMatch)) and
        (TCCPtrInt(TCCPtrUInt(TCCPtrUInt(CurrentPointer) - TCCPtrUInt(CurrentPossibleMatch))) < TCCPtrInt(MaxOffset)) do
        begin
          Difference := PCCUInt32(TCCPtr(@PCCUInt8Array(CurrentPointer)^[0]))^ xor PCCUInt32(TCCPtr(@PCCUInt8Array(CurrentPossibleMatch)^[0]))^;
          if (Difference and TCCUInt32(HashRef_ENDIAN_B30)) = 0 then
            begin
              if (BestMatchLength <= (TCCPtrUInt(EndPointer) - TCCPtrUInt(CurrentPointer))) and
                (PCCUInt8Array(CurrentPointer)^[BestMatchLength - 1] = PCCUInt8Array(CurrentPossibleMatch)^[BestMatchLength - 1]) then
                begin
                  MatchLength := MinMatch;

                  while ((TCCPtrUInt(@PCCUInt8Array(CurrentPointer)^[MatchLength]) and (SizeOf(TCCUInt32) - 1)) <> 0) and
                    ((TCCPtrUInt(@PCCUInt8Array(CurrentPointer)^[MatchLength]) < TCCPtrUInt(EndPointer))) and
                    (PCCUInt8Array(CurrentPointer)^[MatchLength] = PCCUInt8Array(CurrentPossibleMatch)^[MatchLength]) do
                      inc(MatchLength);

                  while (TCCPtrUInt(@PCCUInt8Array(CurrentPointer)^[MatchLength + (SizeOf(TCCUInt32) - 1)]) < TCCPtrUInt(EndPointer)) do
                    begin
                      Difference := PCCUInt32(TCCPtr(@PCCUInt8Array(CurrentPointer)^[MatchLength]))^ xor PCCUInt32(TCCPtr(@PCCUInt8Array(CurrentPossibleMatch)^[MatchLength]))^;
                      if Difference = 0 then
                        begin
                          inc(MatchLength, SizeOf(TCCUInt32));
                        end
                      else
                        begin
{$IF defined(BIG_ENDIAN)}
                          if (Difference shr 16) <> 0 then
                              inc(MatchLength, not(Difference shr 24))
                          else
                              inc(MatchLength, 2 + (not(Difference shr 8)));
{$ELSE}
                          inc(MatchLength, MultiplyDeBruijnBytePosition[TCCUInt32(TCCUInt32(Difference and (-Difference)) * TCCUInt32($077CB531)) shr 27]);
{$IFEND}
                          Break;
                        end;
                    end;
                  if BestMatchLength < MatchLength then
                    begin
                      BestMatchDistance := TCCPtrUInt(TCCPtrUInt(CurrentPointer) - TCCPtrUInt(CurrentPossibleMatch));
                      BestMatchLength := MatchLength;
                    end;
                end;
            end;
          inc(Step);
          if Step < fMaxSteps then
              CurrentPossibleMatch := fChainTable[(TCCPtrUInt(CurrentPossibleMatch) - TCCPtrUInt(aInData)) and WindowMask]
          else
              Break;
        end;
      if (BestMatchDistance > 0) and (BestMatchLength > 1) then
        begin
          DoOutputCopy(BestMatchDistance, BestMatchLength);
          UnsuccessfulFindMatchAttempts := TCCUInt32(1) shl fSkipStrength;
        end
      else
        begin
          if fSkipStrength > 31 then
            begin
              DoOutputLiteral(CurrentPointer^);
            end
          else
            begin
              Step := UnsuccessfulFindMatchAttempts shr fSkipStrength;
              Offset := 0;
              while (Offset < Step) and ((TCCPtrUInt(CurrentPointer) + Offset) < TCCPtrUInt(EndSearchPointer)) do
                begin
                  DoOutputLiteral(PCCUInt8Array(CurrentPointer)^[Offset]);
                  inc(Offset);
                end;
              BestMatchLength := Offset;
              inc(UnsuccessfulFindMatchAttempts, Ord(UnsuccessfulFindMatchAttempts < TCCUInt32($FFFFFFFF)) and 1);
            end;
        end;

      if not OK then
          Break;

      HashTableItem^ := CurrentPointer;
      fChainTable[(TCCPtrUInt(CurrentPointer) - TCCPtrUInt(aInData)) and WindowMask] := Head;
      if fGreedy then
        begin
          inc(CurrentPointer);
          dec(BestMatchLength);
          while (BestMatchLength > 0) and (TCCPtrUInt(CurrentPointer) < TCCPtrUInt(EndSearchPointer)) do
            begin
              HashTableItem := @fHashTable[((((PCCUInt32(TCCPtr(CurrentPointer))^ and TCCUInt32(HashRef_ENDIAN_B30){$IF defined(BIG_ENDIAN)} shr 8{$IFEND})) * TCCUInt32($1E35A7BD)) shr HashShift) and HashMask];
              Head := HashTableItem^;
              HashTableItem^ := CurrentPointer;
              fChainTable[(TCCPtrUInt(CurrentPointer) - TCCPtrUInt(aInData)) and WindowMask] := Head;
              inc(CurrentPointer);
              dec(BestMatchLength);
            end;
        end;
      inc(CurrentPointer, BestMatchLength);
    end;
  while TCCPtrUInt(CurrentPointer) < TCCPtrUInt(EndPointer) do
    begin
      DoOutputLiteral(CurrentPointer^);

      if not OK then
          Break;

      inc(CurrentPointer);
    end;
  OutputEndBlock;
  if fWithHeader then
    begin
      CheckSum := Adler32(aInData, aInSize);
      if (DestLen + 4) < aOutLimit then
        begin
          PCCUInt8Array(aOutData)^[DestLen + 0] := (CheckSum shr 24) and $FF;
          PCCUInt8Array(aOutData)^[DestLen + 1] := (CheckSum shr 16) and $FF;
          PCCUInt8Array(aOutData)^[DestLen + 2] := (CheckSum shr 8) and $FF;
          PCCUInt8Array(aOutData)^[DestLen + 3] := (CheckSum shr 0) and $FF;
          inc(DestLen, 4);
        end;
    end;

  if OK then
      Result := DestLen
  else
      Result := 0;
end;

function TCompressorDeflate.Decompress(const aInData: TCCPtr; const aInSize: TCCSizeUInt; const aOutData: TCCPtr; const aOutLimit: TCCSizeUInt): TCCSizeUInt;
var
  Tag, BitCount: TCCUInt32;
  Source, SourceEnd: PCCUInt8;
  dest: PCCUInt8;
  DestLen: TCCSizeUInt;
  function Adler32(aData: TCCPtr; aLength: TCCUInt32): TCCUInt32;
  const
    Base = 65521;
    NMAX = 5552;
  var
    Buf: PCCUInt8;
    s1, s2, k, i: TCCUInt32;
  begin
    s1 := 1;
    s2 := 0;
    Buf := aData;
    while aLength > 0 do
      begin
        if aLength < NMAX then
            k := aLength
        else
            k := NMAX;
        dec(aLength, k);
        for i := 1 to k do
          begin
            inc(s1, TCCUInt8(Buf^));
            inc(s2, s1);
            inc(Buf);
          end;
        s1 := s1 mod Base;
        s2 := s2 mod Base;
      end;
    Result := (s2 shl 16) or s1;
  end;
  procedure BuildTree(var aTree: TTree; aLengths: PCCUInt8Array; aNum: TCCInt32);
  var
    Offsets: TOffsets;
    i: TCCInt32;
    Sum: TCCUInt32;
  begin
    for i := 0 to 15 do
        aTree.Table[i] := 0;
    for i := 0 to aNum - 1 do
        inc(aTree.Table[TCCUInt8(aLengths^[i])]);
    aTree.Table[0] := 0;
    Sum := 0;
    for i := 0 to 15 do
      begin
        Offsets[i] := Sum;
        inc(Sum, aTree.Table[i]);
      end;
    for i := 0 to aNum - 1 do
      if aLengths^[i] <> 0 then
        begin
          aTree.Translation[Offsets[TCCUInt8(aLengths^[i])]] := i;
          inc(Offsets[TCCUInt8(aLengths^[i])]);
        end;
  end;
  function GetBit: TCCUInt32;
  begin
    if BitCount = 0 then
      begin
        Tag := TCCUInt8(Source^);
        inc(Source);
        BitCount := 7;
      end
    else
        dec(BitCount);
    Result := Tag and 1;
    Tag := Tag shr 1;
  end;
  function ReadBits(aNum, aBase: TCCUInt32): TCCUInt32;
  var
    Limit, Mask: TCCUInt32;
  begin
    Result := 0;
    if aNum <> 0 then
      begin
        Limit := 1 shl aNum;
        Mask := 1;
        while Mask < Limit do
          begin
            if GetBit <> 0 then
                inc(Result, Mask);
            Mask := Mask shl 1;
          end;
      end;
    inc(Result, aBase);
  end;
  function DecodeSymbol(const aTree: TTree): TCCUInt32;
  var
    Sum, c, L: TCCInt32;
  begin
    Sum := 0;
    c := 0;
    L := 0;
    repeat
      c := (c * 2) + TCCInt32(GetBit);
      inc(L);
      inc(Sum, aTree.Table[L]);
      dec(c, aTree.Table[L]);
    until not(c >= 0);
    Result := aTree.Translation[Sum + c];
  end;
  procedure DecodeTrees(var aLT, aDT: TTree);
  var
    hlit, hdist, hclen, i, Num, Len, clen, Symbol, Prev: TCCUInt32;
  begin
    FillPtrByte(@fCodeTree, SizeOf(TTree), 0);
    FillPtrByte(@fLengths, SizeOf(TLengths), 0);
    hlit := ReadBits(5, 257);
    hdist := ReadBits(5, 1);
    hclen := ReadBits(4, 4);
    for i := 0 to 18 do
        fLengths[i] := 0;
    for i := 1 to hclen do
      begin
        clen := ReadBits(3, 0);
        fLengths[CLCIndex[i - 1]] := clen;
      end;
    BuildTree(fCodeTree, TCCPtr(@fLengths[0]), 19);
    Num := 0;
    while Num < (hlit + hdist) do
      begin
        Symbol := DecodeSymbol(fCodeTree);
        case Symbol of
          16:
            begin
              Prev := fLengths[Num - 1];
              Len := ReadBits(2, 3);
              while Len > 0 do
                begin
                  fLengths[Num] := Prev;
                  inc(Num);
                  dec(Len);
                end;
            end;
          17:
            begin
              Len := ReadBits(3, 3);
              while Len > 0 do
                begin
                  fLengths[Num] := 0;
                  inc(Num);
                  dec(Len);
                end;
            end;
          18:
            begin
              Len := ReadBits(7, 11);
              while Len > 0 do
                begin
                  fLengths[Num] := 0;
                  inc(Num);
                  dec(Len);
                end;
            end;
          else
            begin
              fLengths[Num] := Symbol;
              inc(Num);
            end;
        end;
      end;
    BuildTree(aLT, TCCPtr(@fLengths[0]), hlit);
    BuildTree(aDT, TCCPtr(@fLengths[hlit]), hdist);
  end;
  function InflateBlockData(const aLT, aDT: TTree): Boolean;
  var
    Symbol: TCCUInt32;
    Len, Distance, Offset: TCCInt32;
    t: PCCUInt8;
  begin
    Result := False;
    while (TCCPtrUInt(TCCPtr(Source)) < TCCPtrUInt(TCCPtr(SourceEnd))) or (BitCount > 0) do
      begin
        Symbol := DecodeSymbol(aLT);
        if Symbol = 256 then
          begin
            Result := True;
            Break;
          end;
        if Symbol < 256 then
          begin
            if (DestLen + 1) <= aOutLimit then
              begin
                dest^ := TCCUInt8(Symbol);
                inc(dest);
                inc(DestLen);
              end
            else
                Exit;
          end
        else
          begin
            dec(Symbol, 257);
            Len := ReadBits(fLengthBits[Symbol], fLengthBase[Symbol]);
            Distance := DecodeSymbol(aDT);
            Offset := ReadBits(fDistanceBits[Distance], fDistanceBase[Distance]);
            if (DestLen + TCCSizeUInt(Len)) <= aOutLimit then
              begin
                t := TCCPtr(dest);
                dec(t, Offset);
                RLELikeSideEffectAwareMemoryMove(t^, dest^, Len);
                inc(dest, Len);
                inc(DestLen, Len);
              end
            else
                Exit;
          end;
      end;
  end;
  function InflateUncompressedBlock: Boolean;
  var
    Len, InvLen: TCCUInt32;
  begin
    Result := False;
    Len := (TCCUInt8(PCCUInt8Array(Source)^[1]) shl 8) or TCCUInt8(PCCUInt8Array(Source)^[0]);
    InvLen := (TCCUInt8(PCCUInt8Array(Source)^[3]) shl 8) or TCCUInt8(PCCUInt8Array(Source)^[2]);
    if Len <> ((not InvLen) and $FFFF) then
        Exit;
    inc(Source, 4);
    if Len > 0 then
      begin
        if (DestLen + Len) < aOutLimit then
          begin
            CopyPtr(Source, dest, Len);
            inc(Source, Len);
            inc(dest, Len);
          end
        else
            Exit;
      end;
    BitCount := 0;
    inc(DestLen, Len);
    Result := True;
  end;
  function InflateFixedBlock: Boolean;
  begin
    Result := InflateBlockData(fFixedSymbolLengthTree, fFixedDistanceTree);
  end;
  function InflateDynamicBlock: Boolean;
  begin
    FillPtrByte(@fSymbolLengthTree, SizeOf(TTree), 0);
    FillPtrByte(@fDistanceTree, SizeOf(TTree), 0);
    DecodeTrees(fSymbolLengthTree, fDistanceTree);
    Result := InflateBlockData(fSymbolLengthTree, fDistanceTree);
  end;
  function Uncompress: Boolean;
  var
    FinalBlock: Boolean;
    BlockType: TCCUInt32;
  begin
    BitCount := 0;
    repeat
      FinalBlock := GetBit <> 0;
      BlockType := ReadBits(2, 0);
      case BlockType of
        0:
          begin
            Result := InflateUncompressedBlock;
          end;
        1:
          begin
            Result := InflateFixedBlock;
          end;
        2:
          begin
            Result := InflateDynamicBlock;
          end;
        else
          begin
            Result := False;
          end;
      end;
    until FinalBlock or not Result;
  end;
  function UncompressZLIB: Boolean;
  var
    cmf, flg: TCCUInt8;
    a32: TCCUInt32;
  begin
    Result := False;
    Source := aInData;
    cmf := TCCUInt8(PCCUInt8Array(Source)^[0]);
    flg := TCCUInt8(PCCUInt8Array(Source)^[1]);
    if ((((cmf shl 8) + flg) mod 31) <> 0) or ((cmf and $F) <> 8) or ((cmf shr 4) > 7) or ((flg and $20) <> 0) then
      begin
        Exit;
      end;
    a32 := (TCCUInt8(PCCUInt8Array(Source)^[aInSize - 4]) shl 24) or
      (TCCUInt8(PCCUInt8Array(Source)^[aInSize - 3]) shl 16) or
      (TCCUInt8(PCCUInt8Array(Source)^[aInSize - 2]) shl 8) or
      (TCCUInt8(PCCUInt8Array(Source)^[aInSize - 1]) shl 0);
    inc(Source, 2);
    SourceEnd := @PCCUInt8Array(Source)^[aInSize - 6];
    Result := Uncompress;
    if not Result then
      begin
        Exit;
      end;
    Result := Adler32(aOutData, DestLen) = a32;
  end;
  function UncompressDirect: Boolean;
  begin
    Source := aInData;
    SourceEnd := @PCCUInt8Array(Source)^[aInSize];
    Result := Uncompress;
  end;

begin
  dest := aOutData;
  DestLen := 0;
  Result := 0;
  if fWithHeader then
    begin
      if UncompressZLIB then
        begin
          Result := DestLen;
        end;
    end
  else
    begin
      if UncompressDirect then
        begin
          Result := DestLen;
        end;
    end;
end;

constructor TCompressorBRRC.Create;
begin
  inherited Create;
end;

destructor TCompressorBRRC.Destroy;
begin
  inherited Destroy;
end;

function TCompressorBRRC.Compress(const aInData: TCCPtr; const aInSize: TCCSizeUInt; const aOutData: TCCPtr; const aOutLimit: TCCSizeUInt): TCCSizeUInt;
var
{$IFNDEF CPU64}Code, {$ENDIF}Range, Cache, CountFFBytes: TCCUInt32;
{$IFDEF CPU64}Code: TCCUInt64; {$ENDIF}
  Model: array [0 .. SizeModels - 1] of TCCUInt32;
  OK, FirstByte{$IFNDEF CPU64}, Carry{$ENDIF} : Boolean;
  DestLen: TCCInt32;
  procedure EncoderShift;
{$IFDEF CPU64}
  var
    Carry: Boolean;
{$ENDIF}
  begin
{$IFDEF CPU64}
    Carry := PCCUInt64Record(TCCPtr(@Code))^.Hi <> 0; // or (Code shr 32)<>0; or also (Code and TCCUInt64($ffffffff00000000))<>0;
{$ENDIF}
    if (Code < $FF000000) or Carry then
      begin
        if FirstByte then
          begin
            FirstByte := False;
          end
        else
          begin
            if TCCSizeUInt(DestLen) < TCCSizeUInt(aOutLimit) then
              begin
                PCCUInt8Array(aOutData)^[DestLen] := TCCUInt8(Cache + TCCUInt8(Ord(Carry) and 1));
                inc(DestLen);
              end
            else
              begin
                OK := False;
                Exit;
              end;
          end;
        while CountFFBytes <> 0 do
          begin
            dec(CountFFBytes);
            if TCCSizeUInt(DestLen) < TCCSizeUInt(aOutLimit) then
              begin
                PCCUInt8Array(aOutData)^[DestLen] := TCCUInt8($FF + TCCUInt8(Ord(Carry) and 1));
                inc(DestLen);
              end
            else
              begin
                OK := False;
                Exit;
              end;
          end;
        Cache := (Code shr 24) and $FF;
      end
    else
      begin
        inc(CountFFBytes);
      end;
    Code := (Code shl 8){$IFDEF CPU64} and TCCUInt32($FFFFFFFF){$ENDIF};
    Carry := False;
  end;
  function EncodeBit(ModelIndex, Move, Bit: TCCInt32): TCCInt32;
  var
    Bound{$IFNDEF CPU64}, OldCode{$ENDIF}: TCCUInt32;
  begin
    Bound := (Range shr 12) * Model[ModelIndex];
    if Bit = 0 then
      begin
        Range := Bound;
        inc(Model[ModelIndex], (4096 - Model[ModelIndex]) shr Move);
      end
    else
      begin
{$IFNDEF CPU64}
        OldCode := Code;
{$ENDIF}
        inc(Code, Bound);
{$IFNDEF CPU64}
        Carry := Carry or (Code < OldCode);
{$ENDIF}
        dec(Range, Bound);
        dec(Model[ModelIndex], Model[ModelIndex] shr Move);
      end;
    while Range < $1000000 do
      begin
        Range := Range shl 8;
        EncoderShift;
      end;
    Result := Bit;
  end;
  procedure EncoderFlush;
  var
    Counter: TCCInt32;
  begin
    for Counter := 1 to 5 do
        EncoderShift;
  end;
  procedure EncodeTree(ModelIndex, Bits, Move, Value: TCCInt32);
  var
    Context: TCCInt32;
  begin
    Context := 1;
    while Bits > 0 do
      begin
        dec(Bits);
        Context := (Context shl 1) or EncodeBit(ModelIndex + Context, Move, (Value shr Bits) and 1);
      end;
  end;

var
  CurrentPointer, EndPointer: PCCUInt8;
  Len, MinDestLen: TCCInt32;
begin
  DestLen := 0;
  FirstByte := True;
  OK := True;
  CountFFBytes := 0;
  Range := $FFFFFFFF;
  Code := 0;
  for Len := 0 to SizeModels - 1 do
    begin
      Model[Len] := 2048;
    end;
  CurrentPointer := aInData;
  EndPointer := TCCPtr(TCCPtrUInt(TCCPtrUInt(CurrentPointer) + TCCPtrUInt(aInSize)));
  while TCCPtrUInt(CurrentPointer) < TCCPtrUInt(EndPointer) do
    begin
      EncodeBit(FlagModel, 1, 1);
      EncodeTree(LiteralModel, 8, 4, PCCUInt8(CurrentPointer)^);
      if not OK then
        begin
          Break;
        end;
      inc(CurrentPointer);
    end;
  EncodeBit(FlagModel, 1, 0);
  MinDestLen := Max(2, DestLen + 1);
  EncoderFlush;
  if OK then
    begin
      while (DestLen > MinDestLen) and (PCCUInt8Array(aOutData)^[DestLen - 1] = 0) do
          dec(DestLen);
      Result := DestLen;
    end
  else
      Result := 0;
end;

function TCompressorBRRC.Decompress(const aInData: TCCPtr; const aInSize: TCCSizeUInt; const aOutData: TCCPtr; const aOutLimit: TCCSizeUInt): TCCSizeUInt;
var
  Code, Range, Position: TCCUInt32;
  Model: array [0 .. SizeModels - 1] of TCCUInt32;
  OK: Boolean;
  function DecodeBit(ModelIndex, Move: TCCInt32): TCCInt32;
  var
    Bound: TCCUInt32;
  begin
    Bound := (Range shr 12) * Model[ModelIndex];
    if Code < Bound then
      begin
        Range := Bound;
        inc(Model[ModelIndex], (4096 - Model[ModelIndex]) shr Move);
        Result := 0;
      end
    else
      begin
        dec(Code, Bound);
        dec(Range, Bound);
        dec(Model[ModelIndex], Model[ModelIndex] shr Move);
        Result := 1;
      end;
    while Range < $1000000 do
      begin
        if Position < aInSize then
            Code := (Code shl 8) or PCCUInt8Array(aInData)^[Position]
        else
          begin
            if Position < (aInSize + 4 + 5) then
                Code := Code shl 8
            else
              begin
                OK := False;
                Break;
              end;
          end;
        inc(Position);
        Range := Range shl 8;
      end;
  end;
  function DecodeTree(ModelIndex, MaxValue, Move: TCCInt32): TCCInt32;
  begin
    Result := 1;
    while OK and (Result < MaxValue) do
        Result := (Result shl 1) or DecodeBit(ModelIndex + Result, Move);
    dec(Result, MaxValue);
  end;

var
  DestLen, Value: TCCInt32;
begin
  Result := 0;
  if aInSize >= 3 then
    begin
      OK := True;
      Code := (PCCUInt8Array(aInData)^[0] shl 24) or
        (PCCUInt8Array(aInData)^[1] shl 16) or
        (PCCUInt8Array(aInData)^[2] shl 8) or
        (PCCUInt8Array(aInData)^[3] shl 0);
      Position := 4;
      Range := $FFFFFFFF;
      for Value := 0 to SizeModels - 1 do
        begin
          Model[Value] := 2048;
        end;
      DestLen := 0;
      repeat
        Value := DecodeBit(FlagModel, 1);
        if OK then
          begin
            if Value <> 0 then
              begin
                Value := DecodeTree(LiteralModel, 256, 4);
                if OK and (TCCSizeUInt(DestLen) < TCCSizeUInt(aOutLimit)) then
                  begin
                    PCCUInt8Array(aOutData)^[DestLen] := Value;
                    inc(DestLen);
                  end
                else
                    Exit;
              end
            else
                Break;
          end
        else
            Exit;
      until False;

      Result := DestLen;
    end;
end;

{$IFDEF RangeCheck}{$R+}{$ENDIF}


end.
