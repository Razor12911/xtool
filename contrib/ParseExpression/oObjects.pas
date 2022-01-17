unit OObjects;

interface

uses Classes;

const

  { TOCollection interfaces between OWL TCollection and VCL TList }
  MaxCollectionSize = Maxint div (SizeOf(Integer) * 2);

type
  TOCollection = class(TList)
  public
    constructor Create(ACapacity: Integer);
    procedure AtFree(Index: Integer);
    procedure FreeAll;
    procedure DoFree(Item: Pointer);
    procedure FreeItem(Item: Pointer); virtual;
    destructor Destroy; override;
  end;

  TNoOwnerCollection = class(TOCollection)
  public
    procedure FreeItem(Item: Pointer); override;
  end;

  { TSortedCollection object }

  TSortedCollection = class(TOCollection)
  public
    Duplicates: Boolean;
    constructor Create(ACapacity: Integer);
    function Compare(Key1, Key2: Pointer): Integer; virtual; abstract;
    function IndexOf(Item: Pointer): Integer; virtual;
    procedure Add(Item: Pointer); virtual;
    procedure AddReplace(Item: Pointer); virtual;
    { if duplicate then replace the duplicate else add }
    function KeyOf(Item: Pointer): Pointer; virtual;
    function Search(Key: Pointer; var Index: Integer): Boolean; virtual;
  end;

  { TStrCollection object }

  TStrCollection = class(TSortedCollection)
  public
    function Compare(Key1, Key2: Pointer): Integer; override;
    procedure FreeItem(Item: Pointer); override;
  end;

implementation

uses SysUtils, AnsiStrings;

constructor TOCollection.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  { Delta is automatic in TList }
end;

destructor TOCollection.Destroy;
begin
  FreeAll;
  inherited Destroy;
end;

procedure TOCollection.AtFree(Index: Integer);
var
  Item: Pointer;
begin
  Item := Items[Index];
  Delete(Index);
  FreeItem(Item);
end;

procedure TOCollection.FreeAll;
var
  I: Integer;
begin
  try
    for I := 0 to Count - 1 do
      FreeItem(Items[I]);
  finally
    Count := 0;
  end;
end;

procedure TOCollection.DoFree(Item: Pointer);
begin
  AtFree(IndexOf(Item));
end;

procedure TOCollection.FreeItem(Item: Pointer);
begin
  if (Item <> nil) then
    with TObject(Item) as TObject do
      Free;
end;

{ ----------------------------------------------------------------virtual;
  Implementing TNoOwnerCollection
  ----------------------------------------------------------------- }

procedure TNoOwnerCollection.FreeItem(Item: Pointer);
begin
end;

{ TSortedCollection }

{$IFDEF maxComp}

constructor TSortedCollection.Create(ACapacity, ADelta: Integer);
begin
  inherited Create(ACapacity, ADelta);
  Duplicates := False;
end;
{$ELSE}

constructor TSortedCollection.Create(ACapacity: Integer);
begin
  inherited Create(ACapacity);
  Duplicates := False;
end;
{$ENDIF}

function TSortedCollection.IndexOf(Item: Pointer): Integer;
var
  I: Integer;
begin
  IndexOf := -1;
  if Search(KeyOf(Item), I) then
  begin
    if Duplicates then
      while (I < Count) and (Item <> Items[I]) do
        Inc(I);
    if I < Count then
      IndexOf := I;
  end;
end;

procedure TSortedCollection.AddReplace(Item: Pointer);
var
  Index: Integer;
begin
  if Search(KeyOf(Item), Index) then
    Delete(Index);
  Add(Item);
end;

procedure TSortedCollection.Add(Item: Pointer);
var
  I: Integer;
begin
  if not Search(KeyOf(Item), I) or Duplicates then
    Insert(I, Item);
end;

function TSortedCollection.KeyOf(Item: Pointer): Pointer;
begin
  KeyOf := Item;
end;

function TSortedCollection.Search(Key: Pointer; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Search := False;
  L := 0;
  H := Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := Compare(KeyOf(Items[I]), Key);
    if C < 0 then
      L := I + 1
    else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Search := True;
        if not Duplicates then
          L := I;
      end;
    end;
  end;
  Index := L;
end;

{ TStrCollection }

function TStrCollection.Compare(Key1, Key2: Pointer): Integer;
begin
  Compare := AnsiStrings.StrComp(PAnsiChar(Key1), PAnsiChar(Key2));
end;

procedure TStrCollection.FreeItem(Item: Pointer);
begin
  AnsiStrings.StrDispose(PAnsiChar(Item));
end;

end.
