{   2020/05/11  5.01  Move tests from unit flcTests into seperate units.       }

{$INCLUDE flcTCPTest.inc}

unit flcTCPTest_Buffer;

interface



{                                                                              }
{ Test                                                                         }
{                                                                              }
{$IFDEF TCP_TEST}
procedure Test;
{$ENDIF}



implementation

{$IFDEF TCP_TEST}
uses
  flcStdTypes,
  flcTCPBuffer;
{$ENDIF}



{                                                                              }
{ Test                                                                         }
{                                                                              }
{$IFDEF TCP_TEST}
{$ASSERTIONS ON}
procedure Test_Buffer;
var A : TTCPBuffer;
    B : Byte;
    S : RawByteString;
    I, L : Integer;
begin
  TCPBufferInitialise(A, 1500, 1000);
  Assert(TCPBufferUsed(A) = 0);
  Assert(TCPBufferAvailable(A) = 1500);
  TCPBufferSetMaxSize(A, 2000);
  Assert(TCPBufferAvailable(A) = 2000);
  S := 'Fundamentals';
  L := Length(S);
  TCPBufferAddBuf(A, S[1], L);
  Assert(TCPBufferUsed(A) = L);
  Assert(not TCPBufferEmpty(A));
  S := '';
  for I := 1 to L do
    S := S + 'X';
  Assert(S = 'XXXXXXXXXXXX');
  TCPBufferPeek(A, S[1], 3);
  Assert(S = 'FunXXXXXXXXX');
  Assert(TCPBufferPeekByte(A, B));
  Assert(B = Ord('F'));
  S := '';
  for I := 1 to L do
    S := S + #0;
  TCPBufferRemove(A, S[1], L);
  Assert(S = 'Fundamentals');
  Assert(TCPBufferUsed(A) = 0);
  S := 'X';
  for I := 1 to 2001 do
    begin
      S[1] := ByteChar(I mod 256);
      TCPBufferAddBuf(A, S[1], 1);
      Assert(TCPBufferUsed(A) = I);
      Assert(TCPBufferAvailable(A) = 2000 - I);
    end;
  for I := 1 to 2001 do
    begin
      S[1] := 'X';
      TCPBufferRemove(A, S[1], 1);
      Assert(S[1] = ByteChar(I mod 256));
      Assert(TCPBufferUsed(A) = 2001 - I);
    end;
  Assert(TCPBufferEmpty(A));
  TCPBufferShrink(A);
  Assert(TCPBufferEmpty(A));
  TCPBufferFinalise(A);
end;

procedure Test;
begin
  Test_Buffer;
end;
{$ENDIF}



end.
