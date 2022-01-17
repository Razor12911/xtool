unit flcProtoBufProtoParserTests;

interface



{$INCLUDE flcProtoBuf.inc}

{$IFDEF PROTOBUF_TEST}
procedure Test;
{$ENDIF}



implementation

{$IFDEF PROTOBUF_TEST}
uses
  flcProtoBufProtoNodes,
  flcProtoBufProtoParser;



const
  CRLF = #$D#$A;

  ProtoText1 =
      'message test { ' +
      '  required int32 field1 = 1; ' +
      '} ';
  ProtoText1_ProtoString =
      'message test {' + CRLF +
      'required int32 field1 = 1;' +
      '}' + CRLF;

procedure Test;
var
  P : TpbProtoParser;
  A : TpbProtoPackage;
begin
  P := TpbProtoParser.Create;
  P.SetTextStr(ProtoText1);
  A := P.Parse(GetDefaultProtoNodeFactory);
  Assert(Assigned(A));
  Assert(A.GetAsProtoString = ProtoText1_ProtoString);
  P.Free;
end;
{$ENDIF}



end.

