{ Unit pbTestImport1Messages.pas }
{ Generated from TestImport1.proto }
{ Package TestImport1 }

unit pbTestImport1Messages;

interface

uses
  flcUtils,
  flcStrings,
  flcProtoBufUtils;



{ TEnumGlobal }

type
  TEnumGlobal = (
    enumglobalGVal1 = 1,
    enumglobalGVal2 = 2
  );

function  pbEncodeValueEnumGlobal(var Buf; const BufSize: Integer; const Value: TEnumGlobal): Integer;
function  pbEncodeFieldEnumGlobal(var Buf; const BufSize: Integer; const FieldNum: Integer; const Value: TEnumGlobal): Integer;
function  pbDecodeValueEnumGlobal(const Buf; const BufSize: Integer; var Value: TEnumGlobal): Integer;
procedure pbDecodeFieldEnumGlobal(const Field: TpbProtoBufDecodeField; var Value: TEnumGlobal);



implementation



{ TEnumGlobal }

function pbEncodeValueEnumGlobal(var Buf; const BufSize: Integer; const Value: TEnumGlobal): Integer;
begin
  Result := pbEncodeValueInt32(Buf, BufSize, Ord(Value));
end;

function pbEncodeFieldEnumGlobal(var Buf; const BufSize: Integer; const FieldNum: Integer; const Value: TEnumGlobal): Integer;
begin
  Result := pbEncodeFieldInt32(Buf, BufSize, FieldNum, Ord(Value));
end;

function pbDecodeValueEnumGlobal(const Buf; const BufSize: Integer; var Value: TEnumGlobal): Integer;
var I : LongInt;
begin
  Result := pbDecodeValueInt32(Buf, BufSize, I);
  Value := TEnumGlobal(I);
end;

procedure pbDecodeFieldEnumGlobal(const Field: TpbProtoBufDecodeField; var Value: TEnumGlobal);
var I : LongInt;
begin
  pbDecodeFieldInt32(Field, I);
  Value := TEnumGlobal(I);
end;



end.

