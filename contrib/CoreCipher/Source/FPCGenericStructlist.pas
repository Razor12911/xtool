{ ****************************************************************************** }
{ * Generic list of any type (TGenericStructList).                             * }
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
{
  Based on FPC FGL unit, copyright by FPC team.
  License of FPC RTL is the same as our engine (modified LGPL,
  see COPYING.txt for details).
  Fixed to compile also under FPC 2.4.0 and 2.2.4.
  Some small comfortable methods added.
}

unit FPCGenericStructlist;

{$IFDEF FPC}
{$mode objfpc}{$H+}

{$IF defined(VER2_2)} {$DEFINE OldSyntax} {$IFEND}
{$IF defined(VER2_4)} {$DEFINE OldSyntax} {$IFEND}

{$define HAS_ENUMERATOR}
{$ifdef VER2_2} {$undef HAS_ENUMERATOR} {$endif}
{$ifdef VER2_4_0} {$undef HAS_ENUMERATOR} {$endif}
{ Just undef enumerator always, in FPC 2.7.1 it's either broken
  or I shouldn't overuse TFPGListEnumeratorSpec. }
{$undef HAS_ENUMERATOR}

{ FPC < 2.6.0 had buggy version of the Extract function,
  also with different interface, see http://bugs.freepascal.org/view.php?id=19960. }
{$define HAS_EXTRACT}
{$ifdef VER2_2} {$undef HAS_EXTRACT} {$endif}
{$ifdef VER2_4} {$undef HAS_EXTRACT} {$endif}
{$ENDIF FPC}

interface

{$IFDEF FPC}

uses fgl;

type
  { Generic list of types that are compared by CompareByte.

    This is equivalent to TFPGList, except it doesn't override IndexOf,
    so your type doesn't need to have a "=" operator built-in inside FPC.
    When calling IndexOf or Remove, it will simply compare values using
    CompareByte, this is what TFPSList.IndexOf uses.
    This way it works to create lists of records, vectors (constant size arrays),
    old-style TP objects, and also is suitable to create a list of methods
    (since for methods, the "=" is broken, for Delphi compatibility,
    see http://bugs.freepascal.org/view.php?id=9228).

    We also add some trivial helper methods like @link(Add) and @link(L). }
  generic TGenericsList<t> = class(TFPSList)
  private
    type
      TCompareFunc = function(const Item1, Item2: t): Integer;
      TTypeList = array[0..MaxGListSize] of t;
      PTypeList = ^TTypeList;
  {$ifdef HAS_ENUMERATOR} TFPGListEnumeratorSpec = specialize TFPGListEnumerator<t>; {$endif}

  {$ifndef OldSyntax}protected var{$else}
      {$ifdef PASDOC}protected var{$else} { PasDoc can't handle "var protected", and I don't know how/if they should be handled? }
                     var protected{$endif}{$endif} FOnCompare: TCompareFunc;

    procedure CopyItem(Src, dest: Pointer); override;
    procedure Deref(Item: Pointer); override;
    function  Get(index: Integer): t; {$ifdef CLASSESINLINE} inline; {$endif}
    function  GetList: PTypeList; {$ifdef CLASSESINLINE} inline; {$endif}
    function  ItemPtrCompare(Item1, Item2: Pointer): Integer;
    procedure Put(index: Integer; const Item: t); {$ifdef CLASSESINLINE} inline; {$endif}
  public
    constructor Create;
    function Add(const Item: t): Integer; {$ifdef CLASSESINLINE} inline; {$endif}
    {$ifdef HAS_EXTRACT} function Extract(const Item: t): t; {$ifdef CLASSESINLINE} inline; {$endif} {$endif}
    function First: t; {$ifdef CLASSESINLINE} inline; {$endif}
    {$ifdef HAS_ENUMERATOR} function GetEnumerator: TFPGListEnumeratorSpec; {$ifdef CLASSESINLINE} inline; {$endif} {$endif}
    function IndexOf(const Item: t): Integer;
    procedure Insert(index: Integer; const Item: t); {$ifdef CLASSESINLINE} inline; {$endif}
    function Last: t; {$ifdef CLASSESINLINE} inline; {$endif}
{$ifndef OldSyntax}
    procedure Assign(Source: TGenericsList);
{$endif OldSyntax}
    function Remove(const Item: t): Integer; {$ifdef CLASSESINLINE} inline; {$endif}
    procedure Sort(Compare: TCompareFunc);
    property Items[index: Integer]: t read Get write Put; default;
    property List: PTypeList read GetList;
    property ListData: PTypeList read GetList;
  end;

{$ENDIF FPC}

implementation

{$IFDEF FPC}
constructor TGenericsList.Create;
begin
  inherited Create(SizeOf(t));
end;

procedure TGenericsList.CopyItem(Src, dest: Pointer);
begin
  t(dest^) := t(Src^);
end;

procedure TGenericsList.Deref(Item: Pointer);
begin
  Finalize(t(Item^));
end;

function TGenericsList.Get(index: Integer): t;
begin
  Result := t(inherited Get(index)^);
end;

function TGenericsList.GetList: PTypeList;
begin
  Result := PTypeList(FList);
end;

function TGenericsList.ItemPtrCompare(Item1, Item2: Pointer): Integer;
begin
  Result := FOnCompare(t(Item1^), t(Item2^));
end;

procedure TGenericsList.Put(index: Integer; const Item: t);
begin
  inherited Put(index, @Item);
end;

function TGenericsList.Add(const Item: t): Integer;
begin
  Result := inherited Add(@Item);
end;

{$ifdef HAS_EXTRACT}
function TGenericsList.Extract(const Item: t): t;
begin
  inherited Extract(@Item, @Result);
end;
{$endif}

function TGenericsList.First: t;
begin
  Result := t(inherited First^);
end;

{$ifdef HAS_ENUMERATOR}
function TGenericsList.GetEnumerator: TFPGListEnumeratorSpec;
begin
  Result := TFPGListEnumeratorSpec.Create(Self);
end;
{$endif}

function TGenericsList.IndexOf(const Item: t): Integer;
begin
  Result := inherited IndexOf(@Item);
end;

procedure TGenericsList.Insert(index: Integer; const Item: t);
begin
  t(inherited Insert(index)^) := Item;
end;

function TGenericsList.Last: t;
begin
  Result := t(inherited Last^);
end;

{$ifndef OldSyntax}
procedure TGenericsList.Assign(Source: TGenericsList);
var
  i: Integer;
begin
  Clear;
  for i := 0 to Source.Count - 1 do
    Add(Source[i]);
end;
{$endif OldSyntax}

function TGenericsList.Remove(const Item: t): Integer;
begin
  Result := IndexOf(Item);
  if Result >= 0 then
    Delete(Result);
end;

procedure TGenericsList.Sort(Compare: TCompareFunc);
begin
  FOnCompare := Compare;
  inherited Sort(@ItemPtrCompare);
end;

{$ENDIF FPC}

end.



