{*****************************************************************************
  The DEC team (see file NOTICE.txt) licenses this file
  to you under the Apache License, Version 2.0 (the
  "License"); you may not use this file except in compliance
  with the License. A copy of this licence is found in the root directory of
  this project in the file LICENCE.txt or alternatively at

    http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing,
  software distributed under the License is distributed on an
  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied.  See the License for the
  specific language governing permissions and limitations
  under the License.
*****************************************************************************}
unit DECUtilRawByteStringHelper;

interface

{$INCLUDE DECOptions.inc}

/// <summary>
///   System.pas does not contain a RawByteString compatible version of this
///   routine so we created our own, copying and adapting code from system.pas
///   for the NextGen compiler and using a solution from Remy Lebeau for the
///   Win32/Win64 compiler.
/// </summary>
/// <param name="str">
///   String to be processed
/// </param>
procedure UniqueString(var Str: RawByteString);

implementation

uses
  {$IFDEF FPC}
  SysUtils;
  {$ELSE}
  System.SysUtils;
  {$ENDIF}

type
  // Duplicate of the System.pas internal declaration. Needs to be kept in sync.
  PStrRec = ^StrRec;
  StrRec = packed record
  {$IFDEF CPU64BITS}
    _Padding: Integer; // Make 16 byte align for payload..
  {$ENDIF}
    codePage: Word;
    elemSize: Word;
    refCnt: Integer;
    length: Integer;
  end;

function _NewAnsiString(CharLength: Integer; CodePage: Word): Pointer;
var
  P: PStrRec;
begin
  Result := nil;
  if CharLength > 0 then
  begin
    // Alloc an extra null for strings with even length.  This has no actual
    // cost since the allocator will round up the request to an even size
    // anyway. All _WideStr allocations have even length, and need a double
    // null terminator.
    if CharLength >= MaxInt - SizeOf(StrRec) then
      raise EIntOverflow.Create(
        'IntOverflow in _NewAnsiString. CharLength: ' + IntToStr(CharLength));

    GetMem(P, CharLength + SizeOf(StrRec) + 1 + ((CharLength + 1) and 1));
    Result := Pointer(PByte(P) + SizeOf(StrRec));
    P.length := CharLength;
    P.refcnt := 1;
    if CodePage = 0 then
{$IFDEF NEXTGEN}
      CodePage := Word(CP_UTF8);
{$ELSE  NEXTGEN}
      CodePage := Word(DefaultSystemCodePage);
{$ENDIF NEXTGEN}
    P.codePage := CodePage;
    P.elemSize := 1;
    PWideChar(Result)[CharLength div 2] := #0;  // length guaranteed >= 2
  end;
end;

function _LStrClr(var S): Pointer;
var
  P: PStrRec;
begin
  if Pointer(S) <> nil then
  begin
    P := Pointer(PByte(S) - SizeOf(StrRec));
    Pointer(S) := nil;
    if P.refCnt > 0 then
    begin
      {$IFDEF FPC}
      if InterlockedDecrement(P.refCnt) = 0 then
      {$ELSE}
        {$IF CompilerVersion >= 24.0}
        if AtomicDecrement(P.refCnt) = 0 then
        {$ELSE}
        Dec(P.refCnt);
        if (P.refCnt = 0) then
        {$IFEND}
      {$ENDIF}
        FreeMem(P);
    end;
  end;
  Result := @S;
end;

function InternalUniqueStringA(var Str: RawByteString): Pointer;
var
  P: PStrRec;
begin
  Result := Pointer(Str);
  if Result <> nil then
  begin
    Result := Pointer(Str);
    P := Pointer(PByte(Str) - sizeof(StrRec));
    if P.refCnt <> 1 then
    begin
      Result := _NewAnsiString(P.length, P.codePage);
      Move(Pointer(Str)^, Pointer(Result)^, P.length);
      _LStrClr(Str);
      Pointer(Str) := Result;
    end;
  end;
end;

procedure UniqueString(var Str: RawByteString);
begin
  InternalUniqueStringA(Str);
end;

end.
