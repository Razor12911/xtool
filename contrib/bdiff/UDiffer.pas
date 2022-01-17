{
  * Class that generates a diff for two files, logging progress as required.
  *
  * Based on bdiff.c and part of blksort.c by Stefan Reuther, copyright (c) 1999
  * Stefan Reuther <Streu@gmx.de>.
}

unit UDiffer;

interface

uses
  UBDiffTypes, UFileData, ULogger;

{ Structure for a matching block }
type
  TMatch = record
    OldOffset: Cardinal;
    NewOffset: Cardinal;
    BlockLength: Cardinal;
  end;

  PMatch = ^TMatch;

type
  TDiffer = class(TObject)
  private
    fMinMatchLength: Cardinal;
    fFormat: TFormat;
    function FindMaxMatch(OldFile: TFileData; SortedOldData: PBlock;
      SearchText: PSignedAnsiChar; SearchTextLength: Cardinal): TMatch;
    /// <summary>Finds maximum length "sub-string" of CompareData that is in
    /// Data.</summary>
    /// <param name="Data">PSignedAnsiCharArray [in] Data to be searched for
    /// "sub-string".</param>
    /// <param name="Block">PBlock [in] Block of indexes into Data that sort
    /// sub-strings of Data.</param>
    /// <param name="DataSize">Cardinal [in] Size of Data.</param>
    /// <param name="CompareData">PSignedAnsiChar [in] Pointer to data to be
    /// compared to Data.</param>
    /// <param name="CompareDataSize">Cardinal [in] Size of data pointed to by
    /// CompareData.</param>
    /// <param name="FoundPos">Cardinal [out] Position in Data where
    /// "sub-string" was found.</param>
    /// <returns>Cardinal. Length of found "sub-string".</returns>
    function FindString(Data: PSignedAnsiCharArray; Block: PBlock;
      DataSize: Cardinal; CompareData: PSignedAnsiChar;
      CompareDataSize: Cardinal; out FoundPos: Cardinal): Cardinal;
  public
    constructor Create;
    destructor Destroy; override;
    procedure MakeDiff(const OldFileName, NewFileName: string;
      const Logger: TLogger); overload;
    procedure MakeDiff(OldData, NewData, DiffData: Pointer;
      OldSize, NewSize: Integer; var DiffSize: Integer;
      const Logger: TLogger); overload;
    property MinMatchLength: Cardinal read fMinMatchLength write fMinMatchLength
      default 24;
    property Format: TFormat read fFormat write fFormat default FMT_QUOTED;
  end;

implementation

{$IOCHECKS OFF}

uses
  // Project
  UBlockSort, UErrors, UPatchWriters;

{ TDiffer }

constructor TDiffer.Create;
begin
  inherited Create;
  fMinMatchLength := 24; // default minimum match length
  fFormat := FMT_QUOTED; // default output format
end;

destructor TDiffer.Destroy;
begin
  inherited;
end;

function TDiffer.FindMaxMatch(OldFile: TFileData; SortedOldData: PBlock;
  SearchText: PSignedAnsiChar; SearchTextLength: Cardinal): TMatch;
var
  FoundPos: Cardinal;
  FoundLen: Cardinal;
begin
  Result.BlockLength := 0; { no match }
  Result.NewOffset := 0;
  while (SearchTextLength <> 0) do
  begin
    FoundLen := FindString(OldFile.Data, SortedOldData, OldFile.Size,
      SearchText, SearchTextLength, FoundPos);
    if FoundLen >= fMinMatchLength then
    begin
      Result.OldOffset := FoundPos;
      Result.BlockLength := FoundLen;
      Exit;
    end;
    Inc(SearchText);
    Inc(Result.NewOffset);
    Dec(SearchTextLength);
  end;
end;

function TDiffer.FindString(Data: PSignedAnsiCharArray; Block: PBlock;
  DataSize: Cardinal; CompareData: PSignedAnsiChar; CompareDataSize: Cardinal;
  out FoundPos: Cardinal): Cardinal;
var
  First: Cardinal; // first position in Data to search
  Last: Cardinal; // last position in Data to search
  Mid: Cardinal; // mid point of Data to search
  FoundSize: Cardinal; // size of matching "sub-string"
  FoundMax: Cardinal; // maximum size of matching "sub-string"
  PData: PSignedAnsiChar; // ptr to char in Data to be compared
  PCompareData: PSignedAnsiChar; // ptr to char in CompareData to be compared
begin
  First := 0;
  Last := DataSize - 1;
  Result := 0;
  FoundPos := 0;

  // Do binary search of Data
  while First <= Last do
  begin
    // Get mid point of (sorted) Data to search
    Mid := (First + Last) div 2;
    // Set pointer to start of Data search string
    PData := @Data[Block[Mid]];
    // Set pointer to start of CompareData
    PCompareData := CompareData;
    // Calculate maximum possible size of matching substring
    FoundMax := DataSize - Block[Mid];
    if FoundMax > CompareDataSize then
      FoundMax := CompareDataSize;
    // Find and count match chars from Data and CompareData
    FoundSize := 0;
    while (FoundSize < FoundMax) and (PData^ = PCompareData^) do
    begin
      Inc(FoundSize);
      Inc(PData);
      Inc(PCompareData);
    end;

    // We found a "match" of length FoundSize, position Block[Mid]
    if FoundSize > Result then
    begin
      Result := FoundSize;
      FoundPos := Block[Mid];
    end;

    // Determine next search area
    // Note: If FoundSize = FoundMatch then substrings match
    if (FoundSize = FoundMax) or (PData^ < PCompareData^) then
      // substring <= current data string: search above
      First := Mid + 1
    else
    // substring < current data string: search below
    begin
      Last := Mid;
      if Last <> 0 then
        Dec(Last)
      else
        Break;
    end;
  end;
end;

procedure TDiffer.MakeDiff(const OldFileName, NewFileName: string;
  const Logger: TLogger);
var
  OldFile: TFileData;
  NewFile: TFileData;
  NewOffset: Cardinal;
  ToDo: Cardinal;
  SortedOldData: PBlock;
  Match: TMatch;
  PatchWriter: TPatchWriter;
begin
  { initialize }
  OldFile := nil;
  NewFile := nil;
  SortedOldData := nil;
  PatchWriter := TPatchWriterFactory.Instance(fFormat);
  try
    Logger.Log('loading old file');
    OldFile := TFileData.Create(OldFileName);
    Logger.Log('loading new file');
    NewFile := TFileData.Create(NewFileName);
    Logger.Log('block sorting old file');
    SortedOldData := TBlockSort.Execute(OldFile.Data, OldFile.Size);
    if not Assigned(SortedOldData) then
      Error('virtual memory exhausted');
    Logger.Log('generating patch');
    PatchWriter.Header(OldFile.Name, NewFile.Name, OldFile.Size, NewFile.Size);
    { main loop }
    ToDo := NewFile.Size;
    NewOffset := 0;
    while (ToDo <> 0) do
    begin
      Match := FindMaxMatch(OldFile, SortedOldData,
        @NewFile.Data[NewOffset], ToDo);
      if Match.BlockLength <> 0 then
      begin
        { found a match }
        if Match.NewOffset <> 0 then
          { preceded by a "copy" block }
          PatchWriter.Add(@NewFile.Data[NewOffset], Match.NewOffset);
        Inc(NewOffset, Match.NewOffset);
        Dec(ToDo, Match.NewOffset);
        PatchWriter.Copy(NewFile.Data, NewOffset, Match.OldOffset,
          Match.BlockLength);
        Inc(NewOffset, Match.BlockLength);
        Dec(ToDo, Match.BlockLength);
      end
      else
      begin
        PatchWriter.Add(@NewFile.Data[NewOffset], ToDo);
        Break;
      end;
    end;
    Logger.Log('done');
  finally
    // finally section new to v1.1
    if Assigned(SortedOldData) then
      FreeMem(SortedOldData);
    OldFile.Free;
    NewFile.Free;
    PatchWriter.Free;
  end;
end;

procedure TDiffer.MakeDiff(OldData, NewData, DiffData: Pointer;
  OldSize, NewSize: Integer; var DiffSize: Integer; const Logger: TLogger);
var
  OldFile: TFileData;
  NewFile: TFileData;
  NewOffset: Cardinal;
  ToDo: Cardinal;
  SortedOldData: PBlock;
  Match: TMatch;
  PatchWriter: TPatchWriter;
begin
  { initialize }
  DiffSize := 0;
  OldFile := nil;
  NewFile := nil;
  SortedOldData := nil;
  PatchWriter := TPatchWriterFactory.Instance(fFormat);
  PatchWriter.Memory := DiffData;
  PatchWriter.Size := 0;
  PatchWriter.WriteToMemory := True;
  try
    Logger.Log('loading old file');
    OldFile := TFileData.Create(OldData, OldSize);
    Logger.Log('loading new file');
    NewFile := TFileData.Create(NewData, NewSize);
    Logger.Log('block sorting old file');
    SortedOldData := TBlockSort.Execute(OldFile.Data, OldFile.Size);
    if not Assigned(SortedOldData) then
      Error('virtual memory exhausted');
    Logger.Log('generating patch');
    PatchWriter.Header(OldFile.Name, NewFile.Name, OldFile.Size, NewFile.Size);
    { main loop }
    ToDo := NewFile.Size;
    NewOffset := 0;
    while (ToDo <> 0) do
    begin
      Match := FindMaxMatch(OldFile, SortedOldData,
        @NewFile.Data[NewOffset], ToDo);
      if Match.BlockLength <> 0 then
      begin
        { found a match }
        if Match.NewOffset <> 0 then
          { preceded by a "copy" block }
          PatchWriter.Add(@NewFile.Data[NewOffset], Match.NewOffset);
        Inc(NewOffset, Match.NewOffset);
        Dec(ToDo, Match.NewOffset);
        PatchWriter.Copy(NewFile.Data, NewOffset, Match.OldOffset,
          Match.BlockLength);
        Inc(NewOffset, Match.BlockLength);
        Dec(ToDo, Match.BlockLength);
      end
      else
      begin
        PatchWriter.Add(@NewFile.Data[NewOffset], ToDo);
        Break;
      end;
    end;
    Logger.Log('done');
    DiffSize := PatchWriter.Size;
  finally
    // finally section new to v1.1
    if Assigned(SortedOldData) then
      FreeMem(SortedOldData);
    OldFile.Free;
    NewFile.Free;
    PatchWriter.Free;
  end;
end;

end.
